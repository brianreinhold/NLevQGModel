module SetUpModule      ! Formerly NLEVSUB2.f90
    use ArrayDimensionsModule
    implicit none
    
    ! Declare the subroutines that will be used in this module
    public :: StartUp           ! Initializes the model when program starts reading data from NLEVQG.SAV (if the file exists) or defaults otherwise
    public :: newAllocation     ! Allocates the model arrays based upon gUserSettings and gModIntParams assuming nothing has been allocated yet.
                                ! This would be the case when starting with defaults or when reading data from a file at start up.
    public :: SaveModelState    ! Writes current model parameters to the file NLEVQG.SAV. Called at program close.
    public :: ReadSaveFile
    public :: ReadFileHeader
    public :: ReadFileData
    public :: WriteSensFileHeader
    public :: WriteFileData
    public :: CheckUserSettingsChanges  ! Function to check potentially new gUserSettings values and change allocations if needed
    public :: CheckUserSettingsData     ! Function to santity check the gUserSettings values
    public :: CheckModelResolutions     ! Function to sanity check and correct model resolution parameters in gModIntParams
    public :: CheckModelControlChanges
    public :: SetDimensions     ! SetDimensions which gets the dimensions for array sizes for allocation
    public :: DIMENSIONALIZE    ! Call this function to dimensionalize those fields of gModRealParams that need it for display to the user
    public :: NONDIMENSIONALIZE ! Call this function to undimensionalize those fields that were dimensionalized after the user is done
    public :: DEFAULTS  ! This creates the default values for all the fields in the ModelParamsModule
    public :: DFDIAB    ! Computes default diabatic vertical profiles
    public :: PERTB     ! Introduces a perturbation into the flow:
    public :: ComputeVerticalProfiles     ! Computes outside-loop Vertical arrays
    public :: RaisanenRadiationProfile    ! Radiation profile based upon ECMWF radiation model

    !  ReshuffleArrays which resdistributes the horizontal variables after K, L res. change
    !  PSAXPS exchanges PSA and PS arrays
    !  'BOMB' plants a wave#2 bomb with the ESC key
    !  FIRSTP plots first point for graphs
    !  CLIPP clipps the end of the vectors in the balance of forces diagram
    !  PROFIL updates profiles for sensitivity studies
    
    integer, parameter :: ZONAL_WAVES_CHANGED = Z'01', MERIDIONAL_WAVES_CHANGED = Z'02', LEVELS_CHANGED = Z'04'
    integer, parameter :: RN_BAD = Z'08', NUMBER_OF_MAPS_TOO_LARGE = Z'10', PRESSURE_LEVELS_BAD = Z'20'
    ! Recall the Z'10' is how to declare hex values, equivalent to C, C++ and Java 0x10

    contains

        ! The major type instances that are defined in the array and parameter modules are
        ! gUserSettings, gArrayDims, gVertArr, gHorzArr, gStatsArr, gCintArr

        !> **************************************************** StartUp
        function StartUp() result(errorCode)
            use ifwinty!, only ERROR_FILE_NOT_FOUND
            use ConstantsModule, only: MAP_OVERLAY_1, POTENTIAL_TEMP, OMEGA, INVALID
            use ModelParamsModule, only: gUserSettings, gModIntParams, gModRealParams, gModPlotParams, gModSensParams, &
                gNLevFile, gPlotControls, gSensFile, initConstants
            use NlevelQGModelGlobals, only: LogMessage    ! For logging
            use ArrayDimensionsModule, only: gArrayDims, gCurrentDims
            use ModelFieldsModule, only: createHorzArr
            use Fftw3HelpersModule, only: setup_fftw_plans

            implicit none

            CHARACTER(LEN=64) :: errorStr
            integer :: errorCode

            !INTEGER, PARAMETER :: ERROR_FILE_NOT_FOUND = 2 ! More efficient than using use 'ifwinty'

            call initConstants()

            errorCode = 0     ! Initialize err code to success

            !--------------------------- Read data from default file 'NLEVQG.SAV'. FileStruct 'gNLevFile' declared and defined in ModelParametersModule
            gNLevFile%fileName = 'NLEVQG.SAV'
            
            !---------------------------Set active variable to first
            gPlotControls%activePlotMap = MAP_OVERLAY_1   ! Three possible fields can be displayed on a map. The 'active' one is the field that can
                                            ! be changed by the user with the 'field' key. The other two are inactive and will not change
                                            ! These fields are given by gModIntParams%IPLT1, IPLT2, IPLT3
            !---------------------------Set toggle direction for graphics display selections to plus
            gPlotControls%togglePlotDir = 1
            !---------------------------Set temperature view to Potential temperature:
            gPlotControls%plotTempType = POTENTIAL_TEMP
            !---------------------------Set basic state temp profile to be excluded from output:
            gPlotControls%incBasicStateTemp = .false.
            !---------------------------Set vertical motion to view to omega (dp/dt):
            gPlotControls%vertMotionType = OMEGA
            !---------------------------Indicate no SENSIV file opened:
            gSensFile%fileHandle = INVALID
            !---------------------------Read in input parameters and allocate memory:
            ! If file does not exist or another error occurred we will set defaults
            errorCode = ReadSaveFile(gNLevFile)      ! function is 1ocated in this file
            if (errorCode .NE. 0) then
                ! We get here the file was not found or there was an error reading it
                if (errorCode == ERROR_FILE_NOT_FOUND) then
                    call LogMessage('File ' // trim(gNLevFile%fileName) // ' not found. Will initialize with defaults')
                else if (errorCode .GT. 0) then     ! System error
                    errorStr = REPEAT(' ', LEN(errorStr))       ! equivalent to errorStr = ' '; Sets the string to all blanks. Good for TRIM()
                    write(errorStr, '(I0)') errorCode
                    call LogMessage('Reading file ' // trim(gNLevFile%fileName) // ' gave error ' // TRIM(errorStr) // ' Trying with defaults' )
                end if

                ! Since data were not loaded from a save file for whatever reason,
                ! we initialize the model with default values. The DEFAULTS() function
                ! returns the value IXIYL which is the number of levels. It is used to
                ! allocate space for the IXL and IYL arrays which are used for storing
                ! a wavenumber (K and L, respectively) at all levels for plotting a time
                ! series of the vertical structure of a wave. It is equal to the number
                ! of current levels. We set it here just to handle the return value as
                ! required by FORTRAN.
                call DEFAULTS()   ! Subroutine 1ocated in this file
                !-----------------Get and set array dim, Allocate, SET default profiles:

                !---------------------------------Set array dimensions for allocation:
                !  This method loads the IXMAX, IYMAX, IZMAX, KX, LY, IBIG, IBOX, ITOT, IYH, IXMAX1
                !  values into the gArrayDims type.
                gArrayDims = SetDimensions(gUserSettings%maxZonalWaves, gUserSettings%maxMeridWaves, gUserSettings%maxLevels)  ! Function in module SetUpModule
                gCurrentDims = gArrayDims

                ! This function allocates all the arrays and creates the default vertical profiles and perturbs the fields
                ! The dimensions in the gUserSettings and gModIntParams types are the same as is the gArrayDims and gCurrentDims.
                errorCode = newAllocation()
                if (errorCode .NE. 0) then
                    call LogMessage('The model arrays could not be allocated. Cannot run model.')
                    return
                ENDIF
            end if
            ! At this time the user is presented with the 'main menu'. There the user has the chance to 
            ! the values in the gUserSettings fields (max zonal, meridional, levels, integration method
            ! and stats options
            call setup_fftw_plans(gCurrentDims%IXMAX, gCurrentDims%IYMAX)
        end function StartUp

        !> This method allocates all the model arrays assuming that nothing has been allocated yet BUT
        !> the gUserSettings and gModIntParams fields have been set to values read from a file or the defaults.
        function newAllocation() result(error)
            use ModelFieldsModule, only: AllocateHorizontalArrays, AllocateVerticalArrays, AllocateStatsArrays, createHorzArr, createVertArr
            use InteractionCoefficientsModule, only: AllocateCintArrays, computeInteractionCoefficients, createCintArr, gCintArr
            use ModelParamsModule, only: gUserSettings, gModIntParams, gModRealParams

            implicit none

            integer :: error, currentNoOfInteractions

            error = 0

            error = AllocateHorizontalArrays()
            if (error .NE. 0) return
            error = createHorzArr(gModIntParams%KMAX2, gModIntParams%LMAX2, gModIntParams%NLEV, gCurrentDims)
            if (error .NE. 0) return
            error = AllocateVerticalArrays()
            if (error .NE. 0) return
            error = createVertArr(gModIntParams%NLEV)
            if (error .NE. 0) return
            call DFDIAB(.true.)  ! Subroutine 1ocated in this file
            call PERTB() ! Subroutine 1ocated in this file
            ! TODO fix to be consistent with the others
            error = AllocateStatsArrays()
            if (error .NE. 0) return
            if (gUserSettings%integrationMethodTransform .EQV. .FALSE.) then
                ! Using interaction coefficients
                error = AllocateCintArrays()
                if (error .EQ. -1) then
                    error = 0       ! switched to transform method. Too many interaction coefficients
                    return
                else if (error .NE. 0) then
                    return
                end if
                !currentNoOfInteractions = computeInteractionCoefficients(gModRealParams%RN, &
                !                             gModIntParams%KMAX2, gModIntParams%LMAX2, 0)
                !error = createCintArr(gModIntParams%KMAX2, gModIntParams%LMAX2, currentNoOfInteractions)
                gCintArr%numOfInteractionCoeffs = computeInteractionCoefficients(gModRealParams%RN, &
                                             gModIntParams%KMAX2, gModIntParams%LMAX2, 1) ! flag = 1 means compute and fill arrays
            end if
        end function newAllocation

        !> **************************************************** SaveModelState
        function SaveModelState() result(errorCode)
            use NlevelQGModelGlobals, only: LogMessage    ! For logging
            use ModelParamsModule, only: gNLevFile
            use ModelFieldsModule, only: DeallocateHorizontalArrays, DeallocateVerticalArrays, DeallocateStatsArrays

            integer :: errorCode
            logical :: fileExists
            
            errorCode = 0

            INQUIRE(FILE = gNLevFile%fileName, EXIST = fileExists)
            if (.NOT. fileExists) then

                call LogMessage('The file ' // trim(gNLevFile%fileName) // ' is being created')
                OPEN(NEWUNIT = gNLevFile%fileHandle, FILE = trim(gNLevFile%fileName), FORM = 'UNFORMATTED', ACCESS = 'SEQUENTIAL', STATUS = 'NEW', IOSTAT = errorCode)
            elseif (gNLevFile%fileHandle == -1) then
                OPEN(NEWUNIT = gNLevFile%fileHandle, FILE = trim(gNLevFile%fileName), FORM = 'UNFORMATTED', ACCESS = 'SEQUENTIAL', STATUS = 'OLD', IOSTAT = errorCode)
                call LogMessage('The file ' // trim(gNLevFile%fileName) // ' exits. Overwriting with current model state.')
            end if
            if (errorCode .NE. 0) then
                call LogMessage('The file ' // trim(gNLevFile%fileName) // ' could not be opened')
                return
            end if
            errorCode = WriteFileData(gNLevFile%fileHandle)
            if (errorCode .NE. 0) then
                call LogMessage('Error writing data to file. Model state could not be saved.')
            else
                call LogMessage('Model state has been saved to file ' // trim(gNLevFile%fileName) //'.')
            end if
            close(gNLevFile%fileHandle)
            gNLevFile%fileHandle = -1
            fileExists = DeallocateHorizontalArrays()
            fileExists = DeallocateVerticalArrays()
            fileExists = DeallocateStatsArrays()
        end function SaveModelState

        !***************************************************************** ReadSaveFile:
        !>  Routine which reads in data from SAV file and allocates memory.
        !>
        !! @Param fileData: FileStruct type containing the information about the saved file
        function ReadSaveFile(fileData) result(errorCode)
            use ModelParamsModule, only: gUserSettings, gModIntParams, gModRealParams, gModPlotParams, gModSensParams, gUserSettingsPrev, FileStruct
            use ModelFieldsModule, only: gVertArr
            use NlevelQGModelGlobals, only: LogMessage
            use kernel32            ! For windows system calls like GetLastError
            use ISO_C_BINDING       ! Also for the 'GetLastError' windows system function

            type(FileStruct), intent(inout) :: fileData ! isOpened element can get changed

            ! local variables
            integer :: errorCode            ! F90 does not allow inline initialization of a return value. Don't know why
            integer :: i, m
            LOGICAL :: fileExists
            logical :: res

            errorCode = 0

            ! Does the file exist?
            INQUIRE(FILE = trim(fileData%fileName), EXIST = fileExists)
            if (.NOT. fileExists) then
                ! We dont treat this as an error
                errorCode = ERROR_FILE_NOT_FOUND
                call LogMessage('The file ' // trim(fileData%fileName) // ' was not found. Loading defaults.')
                return
            end if
            ! If we get here the file exists
            OPEN(NEWUNIT = fileData%fileHandle, FILE = trim(fileData%fileName), FORM = 'UNFORMATTED', ACCESS = 'SEQUENTIAL', STATUS = 'OLD', IOSTAT = errorCode)
            if (errorCode .NE. 0) then
                call LogMessage('The file ' // trim(fileData%fileName) // ' could not be opened.')
                return
            end if

        !---------------------------------------Read header for allocation:
            ! This loads all the ModelParamsModule fields
            errorCode = ReadFileHeader(fileData%fileHandle)   ! Function 1ocated in this file
        !---------------------------------------Incorrect identifier, or error reading ModelParamsModule data. Hop out:
            IF(errorCode .NE. 0) then           ! Log message in rhead routine
                close(fileData%fileHandle)
                fileData%fileHandle = -1
                return
            end if
        !---------------------------------------Check for bad data:
            if (.NOT. CheckUserSettingsData()) then    ! Function is 1ocated in this file
                call LogMessage('The data read from ' // trim(fileData%fileName) // ' had incorrect bounds on certain parameters. Need to invoke defaults.')
                ! Bad parameters loaded; load defaults
                errorCode = -1  ! Value -1 to separate from system error which tend to be > 0 (as far as I know)
                return
            end if
            errorCode = CheckModelResolutions()
            ! Since we are on the way to the model control variables here, we don't care about the return
            ! value which only says something was out of bounds and has been fixed.

            gArrayDims = SetDimensions(gUserSettings%maxZonalWaves, gUserSettings%maxMeridWaves, gUserSettings%maxLevels)  ! Function in module SetUpModule
            gCurrentDims = SetDimensions(gModIntParams%KMAX2, gModIntParams%LMAX2, gModIntParams%NLEV)
            !---------------------------------------------Allocate arrays based upon what we have read:

            errorCode = newAllocation()  ! Function 1ocated in this file
            ! A pressure profile will be created but they could very easily be overwritten when reading in the data arrays.
            if (errorCode .NE. 0) then
                call LogMessage('The model arrays could not be allocated based upon the header data read from file ' // fileData%fileName)
                CLOSE(fileData%fileHandle)
                fileData%fileHandle = -1
                return
            end if
        !------------------------------------------Read in data:
            errorCode = ReadFileData(fileData%fileHandle)   ! function 1ocated in this file
            IF(errorCode .NE. 0) then
                CLOSE(fileData%fileHandle)
                fileData%fileHandle = -1
                call LogMessage('Could not read the data arrays from file ' // trim(fileData%fileName))
                return
            end if
        !------------------------------------------Check PV profile:
        ! This makes sure that the pressure level decreases as one goes up.
            DO I = 1, gModIntParams%NLEV - 1
                IF(gVertArr%PV(I + 1) .GT. gVertArr%PV(I) .OR. gVertArr%PV(I + 1) .LT. 0.) THEN
                    errorCode = -3
                    CLOSE(fileData%fileHandle)
                    fileData%fileHandle = -1
                    call LogMessage('The pressure levels read for the layers did not decrease with height. Will need to invoke defaults ')
                    return
                ENDIF
            END DO

        !----------------------------------------------------Do not close SENSIV file (check this later)
!120        IF(IEXEC .NE. 5) CLOSE(fileHandle)
            CLOSE(fileData%fileHandle)
            fileData%fileHandle = -1
        END function ReadSaveFile

        !********************************************* Function ReadFileHeader:
        !> Reads just the header data from the file specified by fileHandle. This reads
        !> in the data that populates the data types specified in the ModelParamsModule.
        !> These values are needed to allocate the space needed for the read of the 
        !> remaining data arrays in the ModelFieldModule
        !> The OPEN and CLOSE statements are handled by the caller.
        !! @param fileHandle The file handle to read from.
        !! @return errorCode Returns 0 on success, or a Windows error code on failure.
        function ReadFileHeader(fileHandle) result(errorCode)
            use ModelParamsModule, only: gUserSettings, gModIntParams, gModRealParams, gModPlotParams, gModSensParams, gUserSettingsPrev
            use NlevelQGModelGlobals, only: LogMessage    ! For logging
            !use ifport              ! For the 'GetLastError' windows system function
            !use ISO_C_BINDING       ! Also for the 'GetLastError' windows system function

            INTEGER, intent(in) :: fileHandle
            
            integer :: errorCode, MIRI
            CHARACTER(LEN = 256) :: errorMsg
            
            errorCode = 0

            REWIND(fileHandle)

            ! Read header
            READ(fileHandle, IOSTAT = errorCode, IOMSG = errorMsg) MIRI, gUserSettings, gModRealParams, gModIntParams, gModPlotParams, gModSensParams
            if (errorCode .NE. 0) then
                call LogMessage('File Header could not be read. Error message: '// errorMsg // ' Will need to invoke defaults ')
                return
            end if
            if (MIRI /= 2147483647) then
                call LogMessage('File identifier incorrect. Will need to invoke defaults ')
                errorCode = -1  ! File identifier mismatch
                RETURN
            end if
            gUserSettingsPrev = gUserSettings

            !errorCode = GetLastError()  ! Get the last error code from Windows System
        END function ReadFileHeader

        !****************************************************************** ReadFileData:
        !> With the header data read and memory allocated for the ModelFieldsModule arrays
        !> This method reads the array data. The OPEN and CLOSE statements are handled by the caller.
        !! @param fileHandle The file handle to read from.
        !! @return errorCode Returns 0 on success, or a Windows error code on failure.
        FUNCTION ReadFileData(fileHandle) RESULT(errorCode)
            use ModelFieldsModule, only: gVertArr, gHorzArr, gStatsArr
            use InteractionCoefficientsModule, only: gCintArr
            !use NlevelQGModelGlobals, only: LogMessage    ! For logging
            !use ifport              ! For the 'GetLastError' windows system function
            !use ISO_C_BINDING       ! Also for the 'GetLastError' windows system function

            integer, intent(in) :: fileHandle

            ! Local variables
            INTEGER :: errorCode
            CHARACTER(LEN = 256) :: errorMsg

            ! Initialize error code
            errorCode = 0

            READ(fileHandle, IOSTAT = errorCode, IOMSG = errorMsg) &
                  gVertArr%allocated, &
                  gVertArr%PP, gVertArr%PV, gVertArr%DP, gVertArr%DPI, gVertArr%THS, gVertArr%FK, gVertArr%FJ, &
                  gVertArr%AU, gVertArr%A, gVertArr%H, gVertArr%G, gVertArr%SIG, &
                  gVertArr%X, gVertArr%Y, gVertArr%PRF

            READ(fileHandle, IOSTAT = errorCode, IOMSG = errorMsg) &
                  gHorzArr%allocated, &
                  gHorzArr%GQ, gHorzArr%BTA, gHorzArr%PSB, gHorzArr%PS, gHorzArr%TH, gHorzArr%WW, gHorzArr%TF, &
                  gHorzArr%FS, gHorzArr%ADS, gHorzArr%WS, gHorzArr%S, gHorzArr%FT, gHorzArr%ADT, gHorzArr%WT, &
                  gHorzArr%T, gHorzArr%SF, gHorzArr%ADO, gHorzArr%TOP, &
                  gHorzArr%G1, gHorzArr%G2, gHorzArr%G3, gHorzArr%WRK, gHorzArr%TMP, gHorzArr%DAT, &
                  gHorzArr%AKL, gHorzArr%AKLI, gHorzArr%ZS, gHorzArr%PSSAVE, gHorzArr%RML, &
                  gHorzArr%TRX, gHorzArr%TRY, gHorzArr%IXL, gHorzArr%IYL

            READ(fileHandle, IOSTAT = errorCode, IOMSG = errorMsg) &
                  gStatsArr%allocated, &
                  gStatsArr%PSA, gStatsArr%APE, gStatsArr%RKE, &
                  gStatsArr%PSS, gStatsArr%V1, gStatsArr%V2, &
                  gCintArr%allocated

            if (gCintArr%allocated) then
                READ(fileHandle, IOSTAT = errorCode, IOMSG = errorMsg) &
                    gCintArr%CINT, gCintArr%I1M, gCintArr%JIN, gCintArr%KIN
            endif

            ! Do not close the file if this is a sensitivity file, as it will be used later.

        END FUNCTION ReadFileData
        
        !****************************************************************** WriteSensFileHeader:
        !> Writes standard header and the vertical arrays. The others change.
        !! @param fileHandle The file handle to write to.
        !! @return errorCode Returns 0 on success, or a Windows error code on failure.
        function WriteSensFileHeader(fileHandle) RESULT(errorCode)
            use ModelParamsModule, only: gUserSettings, gModIntParams, gModRealParams, gModPlotParams, gModSensParams
            use ModelFieldsModule, only: gVertArr
            
            implicit none

            INTEGER, intent(in) :: fileHandle

            ! Local variables
            INTEGER :: errorCode
            INTEGER :: MIRI
            CHARACTER(LEN = 256) :: errorMsg

            ! Write data
            WRITE(fileHandle, IOSTAT = errorCode, IOMSG = errorMsg) MIRI, gUserSettings, gModRealParams, gModIntParams, gModPlotParams, gModSensParams
            WRITE(fileHandle, IOSTAT = errorCode, IOMSG = errorMsg) &
                  gVertArr%allocated, &
                  gVertArr%PP, gVertArr%PV, gVertArr%DP, gVertArr%DPI, gVertArr%THS, gVertArr%FK, gVertArr%FJ, &
                  gVertArr%AU, gVertArr%A, gVertArr%H, gVertArr%G, gVertArr%SIG, gVertArr%X, gVertArr%Y, gVertArr%PRF

        end function WriteSensFileHeader

        !****************************************************************** WriteFileData:
        !> Writes the data to the file specified by fileHandle. This write does both
        !> the header and the data arrays. The OPEN statement and CLOSE statements
        !> are done by the caller.
        !! @param fileHandle The file handle to write to.
        !! @return errorCode Returns 0 on success, or a Windows error code on failure.
        FUNCTION WriteFileData(fileHandle) RESULT(errorCode)
            use ModelParamsModule, only: gUserSettings, gModIntParams, gModRealParams, gModPlotParams, gModSensParams
            use ModelFieldsModule, only: gVertArr, gHorzArr, gStatsArr
            use InteractionCoefficientsModule, only: gCintArr
            !use NlevelQGModelGlobals, only: LogMessage    ! For logging
            !use ifport              ! For the 'GetLastError' windows system function
            !use ISO_C_BINDING       ! Also for the 'GetLastError' windows system function

            integer, intent(in) :: fileHandle

            ! Local variables
            INTEGER :: errorCode
            INTEGER :: MIRI
            CHARACTER(LEN = 256) :: errorMsg

            ! Initialize error code
            errorCode = 0

            ! Move to start of file
            REWIND(fileHandle)
            MIRI = 2147483647

            ! Write header data
            WRITE(fileHandle, IOSTAT = errorCode, IOMSG = errorMsg) MIRI, gUserSettings, gModRealParams, gModIntParams, gModPlotParams, gModSensParams
            ! Write model arrays (since they are allocatable, we need to write each array indivually and not just the type gVertArr, etc.)
            WRITE(fileHandle, IOSTAT = errorCode, IOMSG = errorMsg) &
                  gVertArr%allocated, &
                  gVertArr%PP, gVertArr%PV, gVertArr%DP, gVertArr%DPI, gVertArr%THS, gVertArr%FK, gVertArr%FJ, &
                  gVertArr%AU, gVertArr%A, gVertArr%H, gVertArr%G, gVertArr%SIG, &
                  gVertArr%X, gVertArr%Y, gVertArr%PRF

            WRITE(fileHandle, IOSTAT = errorCode, IOMSG = errorMsg) &
                  gHorzArr%allocated, &
                  gHorzArr%GQ, gHorzArr%BTA, gHorzArr%PSB, gHorzArr%PS, gHorzArr%TH, gHorzArr%WW, gHorzArr%TF, &
                  gHorzArr%FS, gHorzArr%ADS, gHorzArr%WS, gHorzArr%S, gHorzArr%FT, gHorzArr%ADT, gHorzArr%WT, &
                  gHorzArr%T, gHorzArr%SF, gHorzArr%ADO, gHorzArr%TOP, &
                  gHorzArr%G1, gHorzArr%G2, gHorzArr%G3, gHorzArr%WRK, gHorzArr%TMP, gHorzArr%DAT, &
                  gHorzArr%AKL, gHorzArr%AKLI, gHorzArr%ZS, gHorzArr%PSSAVE, gHorzArr%RML, &
                  gHorzArr%TRX, gHorzArr%TRY, gHorzArr%IXL, gHorzArr%IYL

            WRITE(fileHandle, IOSTAT = errorCode, IOMSG = errorMsg) &
                  gStatsArr%allocated, &
                  gStatsArr%PSA, gStatsArr%APE, gStatsArr%RKE, &
                  gStatsArr%PSS, gStatsArr%V1, gStatsArr%V2, &
                  gCintArr%allocated

            if (gCintArr%allocated) then
                WRITE(fileHandle, IOSTAT = errorCode, IOMSG = errorMsg) &
                    gCintArr%CINT, gCintArr%I1M, gCintArr%JIN, gCintArr%KIN
            endif

        END FUNCTION WriteFileData

        !> This function checks if the user has changed the base parameters
        !> (gUserSettings). If any of those settings have been changed, reallocation
        !> will be needed for the arrays that depend upon those settings. 
        !> A change in the number of levels effects gVertArr and thus new vertical profiles
        !> A change in the number of zonal or meridional waves effects gHorzArr and 
        !>   gCintArr (interaction coefficients) if they are being used.
        !> A change in the integration method will effect gCintArr
        !! @returns 0 on success. Any other value means the user should check.
        function CheckUserSettingsChanges() result(errorCode)
            use ModelParamsModule, only: gUserSettings, gUserSettingsPrev, gModIntParams, gModRealParams
            use ModelFieldsModule, only: DeallocateHorizontalArrays, DeallocateVerticalArrays, createHorzArr, createVertArr, &
                AllocateHorizontalArrays, AllocateVerticalArrays, gVertArr, gHorzArr
            use InteractionCoefficientsModule, only: DeallocateCintArrays, AllocateCintArrays, computeInteractionCoefficients, gCintArr

            logical :: boolHorzResult, boolVertResult, boolCoeffResult, boolStatsResult
            integer :: errorCode

            errorCode = 0   ! No error
            boolHorzResult = .false.
            boolVertResult = .false.
            boolCoeffResult = .false.
            boolStatsResult = .false.

            !--------------------------------------------Check for bad user parameters:
            if (CheckUserSettingsData() .EQV. .FALSE.) then  ! To me this is easier to read than 'if (.NOT. CheckUserSettingsData())'
                errorCode = 1
                return  ! Bad user data; Let user examine changes made by this function.
            endif
            ! Assures that gModIntParams resolutions are within bounds of gUserSettings values.
            boolHorzResult = CheckModelResolutions() ! borrow a boolean. We do not care about the result. It just means something was changed
            ! in case the old gModIntParams resolutions out of bounds with any changes, it will be reset. The error will be ignored in this
            ! case since we will eventually get to that dialog where the user can make changes if desired.
            boolHorzResult = .false.

            !===================================================Need to reallocate memory?
            IF((gUserSettings%maxZonalWaves .NE. gUserSettingsPrev%maxZonalWaves) .OR. &
               (gUserSettings%maxMeridWaves .NE. gUserSettingsPrev%maxMeridWaves)) boolHorzResult = .true.
            if(gUserSettings%maxLevels .NE. gUserSettingsPrev%maxLevels) boolVertResult = .true.
            if(gUserSettings%integrationMethodTransform .NEQV. gUserSettingsPrev%integrationMethodTransform) boolCoeffResult = .true.
            IF(gUserSettings%statsOption .NE. gUserSettingsPrev%statsOption) boolStatsResult = .true.
            
            if (boolHorzResult .OR. boolVertResult) then
                gArrayDims = setDimensions(gUserSettings%maxZonalWaves, gUserSettings%maxMeridWaves, gUserSettings%maxLevels)
                gCurrentDims = setDimensions(gModIntParams%KMAX2, gModIntParams%LMAX2, gModIntParams%NLEV)
                ! Handle horizontal array changes (which includes vertical dimensions):
                if (gHorzArr%allocated) errorCode = deallocateHorizontalArrays()  ! Deallocate horizontal arrays if allocated

                errorCode = allocateHorizontalArrays()    ! Reallocate horizontal arrays
                if(errorCode .NE. 0) return
                ! But use the model settings for KMAX2, LMAX2, NLEV for actual array dimensions.
                errorCode = createHorzArr(gModIntParams%KMAX2, gModIntParams%LMAX2, gModIntParams%NLEV, gCurrentDims)
                if(errorCode .NE. 0) return
            end if


            if ((gUserSettings%integrationMethodTransform .EQV. .FALSE.) .AND. & ! If we are using interaction coefficients and
                                                    (boolCoeffResult .OR.     & ! We have switched to interaction coefficients or
                                                     boolHorzResult)) then      ! the horizontal array dimensions have changed
                ! Handle interaction coefficient array changes:
                if (gCintArr%allocated) errorCode = DeallocateCintArrays()  ! Deallocate interaction coefficient arrays if allocated

                ! This method allocates the interaction coefficient arrays, and creates the gCintArr arrays using the gUserSettings
                ! dimensions (not the gModIntParams values) and computes the interaction coefficients. It also switches to the
                ! transform method if too many interaction coefficients are needed.
                errorCode = AllocateCintArrays()
                if (errorCode == 0) gCintArr%numOfInteractionCoeffs = computeInteractionCoefficients(gModRealParams%RN, &
                                                gModIntParams%KMAX2, gModIntParams%LMAX2, 1) ! flag = 1 means compute and fill arrays
            endif

            ! Handle vertical array changes:
            IF(boolVertResult) THEN
                if (gVertArr%allocated) errorCode = DeallocateVerticalArrays()  ! Deallocate vertical arrays if allocated
                errorCode = AllocateVerticalArrays()    ! Reallocate vertical arrays
                if (errorCode .NE. 0) return
                ! CheckModelResolutions has assured that gModIntParams%NLEV is within bounds of gUserSettings%maxLevels
                errorCode = createVertArr(gModIntParams%NLEV)
                if (errorCode .NE. 0) return
                CALL DFDIAB(.true.) ! recreate the vertical profiles. This uses gModIntParams%NLEV as the number of levels
            endif
            !-------------------------------------------Introduce perturbation if there have been reallocations:
            if (boolHorzResult .OR. boolVertResult) CALL PERTB()         ! Subroutine 1ocated in this file

            ! TODO Stats arrays
            !IF(boolStatsResult) isAlloced(STATS_ARRAYS) = NEED_REALLOC
            gUserSettingsPrev = gUserSettings

        end function CheckUserSettingsChanges

        !**********************************************Parameter check:
        !> Function sanity checks the UserSettings values and those modelIntParams
        !> that have to be consistent with the User Settings.
        !> If they are bad the function sets them to valid values and returns false.
        !> If they are valid and not changed, function returns true.
        !! @return  true if all fields are okay. false if one or more parameters had to
        !!          be reset because they were inconsistent
        function CheckUserSettingsData() result(mCode)
            use ConstantsModule, only: STATS_OPT_NONE, STATS_OPT_VAR
            use ModelParamsModule, only: gUserSettings
            
            implicit none

            ! Local variables
            logical :: mCode

            mCode = .true.   ! No error
            !-----------------------Check for bad user parameters in the gUserSettings Settings struct:

            IF(gUserSettings%maxZonalWaves .LE. 0) THEN
                gUserSettings%maxZonalWaves = 1
                mCode = mCode .AND. .false.
            ENDIF
            IF(gUserSettings%maxMeridWaves .LE. 0) THEN
                gUserSettings%maxMeridWaves = 1
                mCode = mCode .AND. .false.
            ENDIF
            IF(gUserSettings%maxLevels .LE. 0) THEN
                gUserSettings%maxLevels = 2
                mCode = mCode .AND. .false.
            ENDIF
            IF(gUserSettings%statsOption .LT. STATS_OPT_NONE .OR. &
                gUserSettings%statsOption .GT. STATS_OPT_VAR) THEN
                gUserSettings%statsOption = STATS_OPT_NONE
                mCode = mCode .AND. .false.
            ENDIF

        end function CheckUserSettingsData

        function CheckModelResolutions() result(mCode)
             use ModelParamsModule, only: gModIntParams, gUserSettings
             
             implicit none

            ! Local variables
            logical :: mCode

            mCode = .true.   ! No error
            IF(gModIntParams%KMAX2 .GT. gUserSettings%maxZonalWaves) THEN
                gModIntParams%KMAX2 = gUserSettings%maxZonalWaves
                mCode = mCode .AND. .false.
            ENDIF
            IF(gModIntParams%LMAX2 .GT. gUserSettings%maxMeridWaves) THEN
                gModIntParams%LMAX2 = gUserSettings%maxMeridWaves
                mCode = mCode .AND. .false.
            ENDIF
            IF(gModIntParams%NLEV .GT. gUserSettings%maxLevels) THEN
                gModIntParams%NLEV = gUserSettings%maxLevels
                mCode = mCode .AND. .false.
            ENDIF
            IF(gModIntParams%KMAX2 .LT. 1) THEN
                gModIntParams%KMAX2 = 1
                mCode = mCode .AND. .false.
            ENDIF
            IF(gModIntParams%LMAX2 .LT. 1) THEN
                gModIntParams%LMAX2 = 1
                mCode = mCode .AND. .false.
            ENDIF
            IF(gModIntParams%NLEV .LT. 2) THEN       ! Does the model support only level?
                gModIntParams%NLEV = 2
                mCode = mCode .AND. .false.
            ENDIF
        end function CheckModelResolutions

        function CheckModParams() result(errorCode)
            use ConstantsModule, only: WAVE_AMP_TIME_SERIES, PHASE_SPACE, MEAN_TEMP_TEND_GRAF, AMPLITUDE_RATIO_GRAF, VORTICITY, PLOT_OFF, OROGRAPHY
            use ModelParamsModule, only: gModRealParams, gModIntParams, gModPlotParams

            integer :: errorCode
            integer :: I2
            
            errorCode = 0
            !-------------------------------Check for bad user parameters
            ! Note the user can change these parameters
            ! integer, parameter :: ZONAL_WAVES_CHANGED = Z'01', MERIDIONAL_WAVES_CHANGED = Z'02', LEVELS_CHANGED = Z'04'
            ! integer, parameter :: RN_BAD = Z'08', NUMBER_OF_MAPS_TOO_LARGE = Z'10', PRESSURE_LEVELS_BAD = Z'20'
        
            IF(gModIntParams%KMAX2 .LT. 1) then
                gModIntParams%KMAX2 = 1
                errorCode = ZONAL_WAVES_CHANGED     ! Use if (IAND(errorCode, ZONAL_WAVES_CHANGED) == ZONAL_WAVES_CHANGED) to extract
            end if
            IF(gModIntParams%LMAX2 .LT. 1) then
                gModIntParams%LMAX2 = 1
                errorCode = IOR(errorCode, MERIDIONAL_WAVES_CHANGED)
            end if
            IF(gModIntParams%NLEV .LT. 2) then
                gModIntParams%NLEV = 2  ! Again can we support the barotropic one level case?
                errorCode = IOR(errorCode, LEVELS_CHANGED)
            end if

            !----------------------------------Checking lower bounds on JX0,JY0:
            IF(gModIntParams%JX0 .LT. 0) gModIntParams%JX0 = 0
            IF(gModIntParams%JY0 .LT. 1) gModIntParams%JY0 = 1

            !------------------------------------------Wavenumber, level bounds
            if(gModIntParams%JX .GT. gModIntParams%KMAX2) gModIntParams%JX = gModIntParams%KMAX2
            if(gModIntParams%JY .GT. gModIntParams%LMAX2) gModIntParams%JY = gModIntParams%LMAX2
            if(gModIntParams%LEVP .GT. gModIntParams%NLEV) gModIntParams%LEVP = gModIntParams%NLEV

            if(gModIntParams%JX .LT. -1) gModIntParams%JX = -1
            if(gModIntParams%JY .LT. 0) gModIntParams%JY = 0
            if(gModIntParams%LEVP .LT. 0) gModIntParams%LEVP = 0

            !-------------------------------------------Field or variable type ranges
            I2 = PLOT_OFF
            ! If the type of plot is PHASE_SPACE = 5, WAVE_AMP_TIME_SERIES = 6  WAVE_PHASE_TIME_SERIES = 7, WEATHER_STN = 8
            IF(gModIntParams%MPLOT .GE. PHASE_SPACE) I2 = MEAN_TEMP_TEND_GRAF     ! Not sure why we picked this field to assure the IPLT1
            IF(gModIntParams%MPLOT .EQ. WAVE_AMP_TIME_SERIES) I2 = AMPLITUDE_RATIO_GRAF
            IF(gModIntParams%IPLT1 .LE. 0) gModIntParams%IPLT1 = I2
            IF(gModIntParams%IPLT1 .GT. I2) gModIntParams%IPLT1 = VORTICITY

            !---------------------------------------------IPLT2,IPLT3:
            IF(gModIntParams%IPLT2 .GT. PLOT_OFF) gModIntParams%IPLT2 = VORTICITY
            IF(gModIntParams%IPLT3 .GT. PLOT_OFF) gModIntParams%IPLT3 = VORTICITY
            IF(gModIntParams%IPLT2 .LT. VORTICITY) gModIntParams%IPLT2 = PLOT_OFF
            IF(gModIntParams%IPLT3 .LT. VORTICITY) gModIntParams%IPLT3 = PLOT_OFF

            !---------------------------------------------NUMBX, NUMBY:
            IF(gModIntParams%NUMBX .LT. 1) gModIntParams%NUMBX = 1
            IF(gModIntParams%NUMBY .LT. 1) gModIntParams%NUMBY = 1

            !---------------------------------------------JX0,JX1:
            IF(gModIntParams%JX0 .GT. gModIntParams%KMAX2) gModIntParams%JX0 = gModIntParams%KMAX2
            IF(gModIntParams%JY0 .GT. gModIntParams%LMAX2) gModIntParams%JY0 = gModIntParams%LMAX2
            IF(gModIntParams%JX1 .GT. gModIntParams%KMAX2) gModIntParams%JX1 = gModIntParams%KMAX2
            IF(gModIntParams%JY1 .GT. gModIntParams%LMAX2) gModIntParams%JY1 = gModIntParams%LMAX2
            IF(gModIntParams%JX1 .LT. gModIntParams%JX0) gModIntParams%JX1 = gModIntParams%JX0
            IF(gModIntParams%JY1 .LT. gModIntParams%JY0) gModIntParams%JY1 = gModIntParams%JY0

            !---------------------------------------------IV1, IV2:
            IF(gModIntParams%IV1 .GT. OROGRAPHY) gModIntParams%IV1 = OROGRAPHY
            IF(gModIntParams%IV2 .GT. OROGRAPHY) gModIntParams%IV2 = OROGRAPHY
            IF(gModIntParams%IV1 .LT. VORTICITY) gModIntParams%IV1 = VORTICITY
            IF(gModIntParams%IV2 .LT. VORTICITY) gModIntParams%IV2 = VORTICITY

            !---------------------------------------------IPRM1, IPRM2:
            IF(gModIntParams%IPRM1 .LT. VORTICITY) gModIntParams%IPRM1 = VORTICITY    ! 1 was the original
            IF(gModIntParams%IPRM2 .LT. VORTICITY) gModIntParams%IPRM2 = VORTICITY
            IF(gModIntParams%IPRM1 .GT. 31) gModIntParams%IPRM1 = VORTICITY    ! 31 was the original
            IF(gModIntParams%IPRM2 .GT. 31) gModIntParams%IPRM2 = VORTICITY    ! Dont know what 31 is or if the VORTICITY - AMPLITUDE_RATIO_GRAF (25) is the right set.

            !---------------------------------------------ITER1, ITER2:
            ! The number of iterations one makes over a parameter %IPRM1 or %IPRM2 or both.
            ! Both have to be at least 1. If both are one, there is no iteration.
            ! The interval is determined by ?
            IF(gModIntParams%ITER1 .LT. 1) gModIntParams%ITER1 = 1
            IF(gModIntParams%ITER2 .LT. 1) gModIntParams%ITER2 = 1

            !---------------------------------------------KMODE,LMODE:
            IF(gModIntParams%KMODE .GT.gModIntParams%KMAX2) gModIntParams%KMODE = gModIntParams%KMAX2
            IF(gModIntParams%LMODE .GT.gModIntParams%LMAX2) gModIntParams%LMODE = gModIntParams%LMAX2

            !------------------------------------Turn off IPRM2 for KMODE>0, IPRM1>7:
            IF(gModIntParams%KMODE .GE. 1) THEN
                IF(gModIntParams%IPRM1 .GE. 8) gModIntParams%ITER2 = 1 !?
                IF(gModIntParams%IPRM2 .GE. 8) gModIntParams%IPRM2 = 1 !?
            ENDIF
            IF(gModIntParams%KMODE .LT. 0) gModIntParams%KMODE = 0
            IF(gModIntParams%LMODE .LT. 1) gModIntParams%LMODE = 1
        
            IF(gModPlotParams%ISCRN .GT. 1 .OR. gModPlotParams%ISCRN .LT. 0) gModPlotParams%ISCRN = 0
            !---------------------------------------------Some idiot tests:
            IF(gModRealParams%RN .LE. 0. .OR. gModRealParams%DELT .LE. 0.) errorCode = IOR(errorCode, RN_BAD)
            IF(gModRealParams%TPP .LT. 0) gModRealParams%TPP = 0.
            IF(gModRealParams%BOT .LE. gModRealParams%TPP) errorCode = IOR(errorCode, PRESSURE_LEVELS_BAD)

        end function CheckModParams 

        !> Method to allocate arrays
        !> This function allocates all the ModelFieldsModule arrays based upon the
        !> the user settings. It can also switch the integration method from interaction
        !> coefficients to spectral transform method if the gUserSettings are such that
        !> too many interaction coefficients are needed.
        !function ALLOC() result(errorCode)
        !    use ConstantsModule
        !    use ModelParamsModule, only: gUserSettings, gUserSettingsPrev, gModIntParams       ! Defines type Settings
        !    use ModelFieldsModule 
        !    use ArrayDimensionsModule
        !    use InteractionCoefficientsModule
        !
        !    !--------------- Local variables
        !    integer :: errorCode
        !    LOGICAL :: res
        !
        !    errorCode = 0
        !
        !!---------------------------------Set array dimensions for allocation:
        !    !  This method loads the IXMAX, IYMAX, IZMAX, KX, LY, IBIG, IBOX, ITOT, IYH, IXMAX1
        !    !  values into the gArrayDims type.
        !    !res = SetDimensions(gUserSettings%maxZonalWaves, gUserSettings%maxMeridWaves, gUserSettings%maxLevels)  ! Function in module SetUpModule
        !
        !    !=====================================Set aside space for all vertical arrays:
        !    IF (isAlloced(VERT_ARRAYS) .EQ. NEED_REALLOC) errorCode = DeallocateVerticalArrays()
        !    IF (isAlloced(VERT_ARRAYS) .LE. NEED_REALLOC) then 
        !        !---------- Allocates and zeros vertical arrays
        !        errorCode = AllocateVerticalArrays()
        !        !-------------------------------------Check for Error in allocation (not enough memory):
        !        IF(errorCode .EQ. 0) THEN
        !            isAlloced(VERT_ARRAYS) = IS_ALLOCATED
        !            errorCode = createVertArr(gModIntParams%NLEV)  ! Function in ModelFieldsModule to set up vertical arrays
        !        else
        !            errorCode = 11      ! Allocation error code
        !            isAlloced(VERT_ARRAYS) = NOT_ALLOCATED
        !            return
        !        END IF
        !    END IF
        !
        !    !=====================================Set aside space for horizontal arrays:
        !
        !    IF(isAlloced(HORZ_ARRAYS) .EQ. NEED_REALLOC) errorCode = deallocateHorizontalArrays()
        !    IF (isAlloced(HORZ_ARRAYS) .LE. NEED_REALLOC) then 
        !        !------------------------------------Allocate the Horizontal arrays
        !        errorCode = AllocateHorizontalArrays()
        !        !-------------------------------------Error in allocation (not enough memory):
        !        IF(errorCode .NE. 0)THEN
        !            isAlloced(HORZ_ARRAYS) = NOT_ALLOCATED
        !            errorCode = DeallocateVerticalArrays()
        !            return
        !        ELSE
        !            isAlloced(HORZ_ARRAYS) = IS_ALLOCATED
        !            errorCode = createHorzArr(gModIntParams%kmax2, gModIntParams%lmax2, gModIntParams%nlev, gCurrentDims)
        !        END IF
        !    END IF
        !
        !    !==================================================Allocate statistics arrays:
        !    IF(isAlloced(STATS_ARRAYS) .EQ. NEED_REALLOC) errorCode = DeallocateStatsArrays()
        !    if(isAlloced(STATS_ARRAYS) .LE. NEED_REALLOC) then
        !        errorCode = AllocateStatsArrays()
        !        IF(errorCode .NE. 0) THEN
        !            isAlloced(STATS_ARRAYS) = NOT_ALLOCATED
        !            errorCode = DeallocateVerticalArrays()
        !            errorCode = deallocateHorizontalArrays()
        !            return
        !        ELSE
        !            isAlloced(STATS_ARRAYS) = IS_ALLOCATED
        !        END IF
        !    END IF
        !
        !
        !    !==================================================For interaction coef model:
        !    IF(isAlloced(INTERACT_COEF_ARRAYS) .LE. NEED_REALLOC) THEN
        !        IF(isAlloced(INTERACT_COEF_ARRAYS) .EQ. NEED_REALLOC) errorCode = DeallocateCintArrays()
        !
        !        errorCode = AllocateCintArrays()
        !        !-------------------------------------Error in allocation (not enough memory or too many coefficients):
        !        IF(errorCode .NE. 0) THEN
        !            isAlloced(INTERACT_COEF_ARRAYS) = NOT_ALLOCATED    ! Method will have switched to transform method if it wasn't already
        !            errorCode = 0
        !        ELSE
        !            isAlloced(INTERACT_COEF_ARRAYS) = IS_ALLOCATED
        !        ENDIF
        !    ENDIF
        !    !------------------------Indicators for detecting new resolution change:
        !    gUserSettingsPrev%maxZonalWaves = gUserSettings%maxZonalWaves
        !    gUserSettingsPrev%maxMeridWaves = gUserSettings%maxMeridWaves
        !    gUserSettingsPrev%maxLevels = gUserSettings%maxLevels
        !    gUserSettingsPrev%integrationMethodTransform = gUserSettings%integrationMethodTransform
        !    gUserSettingsPrev%statsOption = gUserSettings%statsOption
        !END function ALLOC

        function CheckModelControlChanges(prevZonalWaves, prevMeridWaves, prevLevels, prevRN) result(errorCode)
            use ConstantsModule, only: USE_INTERACTION_COEFFICIENTS, STATS_OPT_NONE
            use ModelParamsModule, only: gModIntParams, gModRealParams, gUserSettings
            use InteractionCoefficientsModule, only: gCintArr, createCintArr, computeInteractionCoefficients
            use ModelFieldsModule, only: gHorzArr, PS_pool, PSSave_pool, TOP_pool, createHorzArr, createVertArr
            use ArrayDimensionsModule, only: gCurrentDims
            use Fftw3HelpersModule, only: setup_fftw_plans

            integer, intent(in) :: prevZonalWaves, prevMeridWaves, prevLevels
            real, intent(in) :: prevRN  ! nondimensional

            integer :: errorCode, I, N
            LOGICAL :: bret, boolUpdateCint, boolHorzChange, boolVertChange
            REAL, ALLOCATABLE :: DAT(:, :)
            integer :: itot_old, itot_new, total, currentNoOfInteractions

            errorCode = 0
            boolUpdateCint = .false.
            boolHorzChange = .false.
            boolVertChange = .false.

            ! Now we set dimensionals for all those arrays used for plotting (not model variables)
            ! based upon the user selection of zonal, meridional, and levels which may be the same
            ! as in the gUserSettings.
            ! The function always returns true
            ! TODO: WHAT? How does this make sense? Commenting out for now.

            ! Check the current gModIntParams values are okay and consistent with the gUserSettings values
            if (CheckModelResolutions() .EQV. .false.) then
                errorCode = -1
                return  ! There is a problem. So we return to the ModelControl Menu for the user to fix
            end if
            errorCode = CheckModParams()  ! Check all the other possible values the user may have changed.
            if (errorCode .NE. 0) return  ! Values changed by program because of bad values. Return to user for inspection.

            call NONDIMENSIONALIZE()    ! call DIMENSIONALIZE() was done in the WM_INITIDIALOG method in the ModelControl Dialog

            if (prevZonalWaves .NE. gModIntParams%KMAX2 .OR. prevMeridWaves .NE. gModIntParams%LMAX2) boolHorzChange = .true.
            if (prevLevels .NE. gModIntParams%NLEV) boolVertChange = .true.

            ! Check if the resolutions have changed and we need to reshuffle arrays
            if (boolHorzChange .OR. boolVertChange) then
                gCurrentDims = SetDimensions(gModIntParams%KMAX2, gModIntParams%LMAX2, gModIntParams%NLEV)
                total = gArrayDims%itot * gUserSettings%maxLevels ! This is for reshuffling arrays so we need to know their base size
                itot_old = prevMeridWaves + 2 * prevZonalWaves * prevMeridWaves
                itot_new = gModIntParams%LMAX2 + 2 * gModIntParams%KMAX2 * gModIntParams%LMAX2
                ALLOCATE(DAT(itot_new, gModIntParams%NLEV)) ! Temporary array for reshuffling

                ! Signal that we need to update the interaction coefficents if using interaction coefficients and the horizontal resolution has changed
                if (gUserSettings%integrationMethodTransform .EQV. .false. .AND. boolHorzChange) boolUpdateCint = .true.

                errorCode = createHorzArr(gModIntParams%KMAX2, gModIntParams%LMAX2, gModIntParams%NLEV, gCurrentDims)
                call setup_fftw_plans(gCurrentDims%IXMAX, gCurrentDims%IYMAX)
                call ReshuffleArrays(gHorzArr%PS, DAT, PS_pool, itot_old, prevLevels, itot_new, gModIntParams%NLEV, total)
                
                CALL ReshuffleArrays(gHorzArr%TF, DAT, PS_pool(4 * total + 1 : 5 * total), itot_old, prevLevels, itot_new, gModIntParams%NLEV, total)
                CALL ReshuffleArrays(gHorzArr%SF, DAT, PS_pool(5 * total + 1 : 6 * total), itot_old, prevLevels, itot_new, gModIntParams%NLEV, total)
                CALL ReshuffleArrays(gHorzArr%PSSAVE, DAT, PSSave_pool, itot_old, prevLevels, itot_new, gModIntParams%NLEV, total)
                CALL ReshuffleArrays(gHorzArr%TOP, DAT, TOP_pool, itot_old, 1, itot_new, 1, gArrayDims%itot)
            !  !===================================================Reshuffle stats arrays:
            !        IF(gUserSettings%statsOption .NE. STATS_OPT_NONE) THEN
            !            CALL ReshuffleArrays(DAT, gStatsArr%APE(:, N), prevZonalWaves, prevMeridWaves)
            !            CALL ReshuffleArrays(DAT, gStatsArr%RKE(:, N), prevZonalWaves, prevMeridWaves)
            !            DO I = 1,7
            !                CALL ReshuffleArrays(DAT, gStatsArr%PSS(:, N, I), prevZonalWaves, prevMeridWaves)
            !                CALL ReshuffleArrays(DAT, gStatsArr%PSA(:, N, I), prevZonalWaves, prevMeridWaves)
            !            END DO
            !        ENDIF
            !    END DO
                deallocate(DAT)
                !-----------------------------Change vertical:
                IF (boolVertChange) then
                    errorCode = createVertArr(gModIntParams%NLEV)
                    !----------------------------------------Compute default Diabatic terms:
                    CALL DFDIAB(.true.) ! True indicates that pressure levels have to be determined.
                    !----------------------------------------Zero Omega array for Barotropic flow:
                    IF(gModIntParams%NLEV .EQ. 1) gHorzArr%WW = 0.0   ! Fortran shortcut
                ENDIF
            endif
            ! If only the RN value has changed, all we need to do is recompute the interaction coefficients; all the 
            ! defined arrays will have the same dimensions. If the horizontal resolution has changed, we will need
            ! to recreate the Cint arrays as well and then compute the interaction coefficients.

            if (boolUpdateCint .OR. (prevRN .NE. gModRealParams%RN)) then
                ! Horizontal resolution has changed, need to recompute cint arrays.
                if (boolUpdateCint) then
                    ! Compute number of coefficients only. flag = 0 singals to do that. Need to create the gCintArr array shapes.
                    
                    currentNoOfInteractions = computeInteractionCoefficients(gModRealParams%RN, gModIntParams%KMAX2, gModIntParams%LMAX2, 0)
                    ! The way we have it now is that as long as KMAX2 and LMAX2 are within bounds, we are under the limit of interaction coefficients
                    ! so the method will never return -1 indicating a switch to a transform method.
                    errorCode = createCintArr(gModIntParams%KMAX2, gModIntParams%LMAX2, currentNoOfInteractions)
                endif
                ! Now compute the interaction coefficients. flag > 0 indicates to do that versus just count them
                gCintArr%numOfInteractionCoeffs = computeInteractionCoefficients(gModRealParams%RN, gModIntParams%KMAX2, gModIntParams%LMAX2, 1)
            endif

        end function CheckModelControlChanges

        !****************************************************************************
        !>  Routine copies the linear form of the array PS with old resolution itot_old,
        !>  levels_old to the new resolution itot_new, levels_new.  DAT is used as a buffer
        !>  for the transfer.  When one decreases the resolution, data values will, of course,
        !>  be lost if they are no longer within the realms of the new resolution.
        !>
        !>  NOTE that it is assumed that checks have already been made that itot_new * levels_new
        !>  does not exceed total.
        !>
        !! @param PS            The primary array to reshuffle (2D view)
        !! @param DAT           Temporary buffer array (2D)
        !! @param PS_pool       The pool array containing the data (1D)
        !! @param itot_old      Total number of spectral coefficients in old resolution
        !! @param levels_old    Number of vertical levels in old resolution
        !! @param itot_new      Total number of spectral coefficients in new resolution
        !! @param levels_new    Number of vertical levels in new resolution
        !! @param total         Total size of the PS_pool array 
        SUBROUTINE ReshuffleArrays(ps, dat, PS_pool, itot_old, levels_old, itot_new, levels_new, total)

            ! Dimensions are passed as arguments
            INTEGER, INTENT(IN) :: itot_old, levels_old, itot_new, levels_new, total
            REAL, intent(inout) :: ps(itot_new, levels_new)
            real, intent(inout) :: dat(itot_new, levels_new)
            REAL, intent(in) :: PS_pool(*)

            ! Local variables
            INTEGER :: i, j
            INTEGER :: old_linear_index

            ! --- Precondition Checks --- (This has already been done)
            !IF (itot_new * levels_new > total) THEN
            !    PRINT *, "Error: New dimensions exceed the capacity of PS_pool/Temp_pool."
            !    RETURN
            !END IF

            !IF (SIZE(PS_pool) < current_IBIG .OR. SIZE(Temp_pool) < current_IBIG) THEN
            !     PRINT *, "Error: Pools are not allocated correctly before calling reshuffle_ps."
            !     RETURN
            !END IF

            dat = 0.0  ! Zero the temporary array

            ! 3. Copy the relevant data from PS_pool into Dat
            ! Use the *old* dimensions (itot_old) to calculate the memory layout of the source data.
            DO j = 1, levels_new
                if (j .GT. levels_old) then
                    DAT(1 : itot_new, j) = 0.0
                    cycle
                endif
                DO i = 1, itot_new
                    IF (i > itot_old) THEN
                        DAT(i, j) = 0.0
                    ELSE
                        ! Calculate the linear index in PS_pool based on the OLD column-major shape
                        old_linear_index = (j - 1) * itot_old + i 
                        IF (old_linear_index <= total) THEN
                            DAT(i, j) = PS_pool(old_linear_index)
                        ENDIF
                        ! old_linear_index should never exceed total (that has already been checked). If it
                        ! does, we simply skip the assignment in order to avoid out-of-bounds write.
                    END IF
                END DO
            END DO

            ! 5. Copy the data from the temporary 2D view back to the main PS 2D view using a single assignment
            PS = DAT
        END SUBROUTINE ReshuffleArrays

        !SUBROUTINE ReshuffleArrays(DAT, PS, prevZonalWaves, prevMeridWaves)
        !    use ModelParamsModule, only: gModIntParams
        !    use UtilitiesModule, only: ZeroRealArray
        !
        !    REAL, intent(inout) :: PS(*)
        !    real, intent(inout) :: DAT(*)
        !    INTEGER, intent(in) :: prevZonalWaves, prevMeridWaves !KOLD, prevMeridWaves
        !
        !    INTEGER I, II, J, K, L, IOLD, INEW
        !
        !    ! To go from I to K, L we use K=(I-1-modIntParam%LMAX2)/(2*modIntParam%LMAX2)+1, L=(I-(2*K-1)*modIntParam%LMAX2+1)/2
        !    ! where MOD(L, 2) = 0 (before division by 2) means the component is real.
        !
        !    !------------------------------------------------ Compute old and new I-sizes:
        !    IOLD = prevMeridWaves + 2 * prevMeridWaves * prevZonalWaves
        !    INEW = gModIntParams%LMAX2 + 2 * gModIntParams%LMAX2 * gModIntParams%KMAX2
        !    !------------------------------------------------ Copy old data into DAT:
        !    DO I = 1, IOLD
        !        DAT(I) = PS(I)
        !    end DO
        !    !------------------------------------------------ Zero data in array:
        !    CALL ZeroRealArray(PS, INEW)
        !    !------------------------------------------------ Compute K,L,II for old data:
        !    DO I = 1, IOLD
        !        K = 1 + (I - 1 - prevMeridWaves) / (2 * prevMeridWaves)
        !        L = I - (2 * K - 1) * prevMeridWaves + 1
        !        !-------------------------------------------- See if we have cosknx or sinknx:
        !        II = MOD(L, 2)
        !        L = L / 2
        !        !-------------------------------------------- Compute position in new array:
        !        J = (2 * K - 1) * gModIntParams%LMAX2 + 2 * L - 1 + II
        !        !-------------------------------------------- Special case for I less than prevMeridWaves:
        !        IF(I .LE. prevMeridWaves) THEN
        !            K = 0
        !            J = I
        !            !---------------------------------------- Check that new array has the value:
        !            IF(J .GT. gModIntParams%LMAX2) CYCLE
        !        ENDIF
        !        IF(J .GT. INEW) CYCLE
        !        PS(J) = DAT(I)
        !    end DO
        !
        !END SUBROUTINE


        !>***************************************************** Get sizes of arrays:
        !> Method populates the Dims type defined in ArrayDimensionsModule
        !> given the input sizes. These are used for allocating the plotting-related arrays
        !! @Param zonalWaves: the number of zonal waves
        !! @Param meridWaves: the number of meridional waves
        !! @Param levels: the number of model levels
        !! @returns true always
        function SetDimensions(zonalWaves, meridWaves, levels) result(arraySizes)  ! Previously called SETDIM
            use ConstantsModule, only: MIN_ASXY_SIZE, STATS_OPT_NONE, STATS_OPT_VAR
            use ModelParamsModule, only: gUserSettings
            use ArrayDimensionsModule, only: Dims
    
            implicit none

            integer, intent(in) :: zonalWaves, meridWaves, levels

            !--------------------------------------------------Local variables:
            INTEGER :: I, J
            type (Dims) :: arraySizes
            
            !--------  The number of grid points that satisfy both 3*maxWavenumber + 1 that have factors 2,3,5,7 or 11.
            integer, parameter :: gridPoints(*) = [4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 18, 20, 21, 22, 24, 25, 27, 28, 30, &
                                      32, 33, 35, 36, 40, 42, 44, 45, 48, 49, 50, 54, 55, 56, 60, 63, 64, 66, 70, 72, 75, 77, &
                                      80, 81, 84, 88, 90, 96, 99, 100, 105, 108, 110, 112, 120, 121, 125, 126, 128, 132, 135, &
                                      140, 144, 147, 150, 154, 160, 162, 165, 168, 175, 176, 180, 189, 192, 198, 200, 210, 216, &
                                      220, 224, 225, 231, 240, 243, 245, 250, 252, 256, 264, 270, 275, 280, 288, 297, 300, 308, &
                                      315, 320, 324, 330, 336, 343, 350, 360, 363, 375, 378, 384, 385, 396, 400, 405, 420, 432, &
                                      440, 441, 448, 450, 462, 480, 486, 495, 500, 504, 512]

            call ZeroArrayDims(arraySizes)  ! This subroutine initializes all dimensions in gArrayDims to zero.
            !------------------------------------------------------------ Set up FFTs:
            J = 3 * zonalWaves + 1
            DO I = 1, SIZE(gridPoints)
                IF(J .LE. gridPoints(I)) THEN
                    arraySizes%IXMAX = gridPoints(I)
                    EXIT
                ENDIF
            END DO
            J = 3 * meridWaves + 1      ! We don't need to double the meridional length as the FFTW3 r2r handles our boundaries
            DO I = 1, SIZE(gridPoints)
                IF(J .LE. gridPoints(I)) THEN
                    arraySizes%IYMAX = gridPoints(I)
                    EXIT
                ENDIF
            END DO
            arraySizes%IXIYL = levels
            !------------------------------------For Graphics output of maps:
            arraySizes%KX = arraySizes%IXMAX + 2
            arraySizes%LY = arraySizes%IYMAX + 2
            !-------------------------------------Vertical dimension:
            arraySizes%IZMAX = 2 * levels
            arraySizes%IZMAX  = MAX(arraySizes%IZMAX, 16) ! Ensure IZMAX is at least 16

            !--------------------Put the larger of KX, LY into I and the other into J
            I = MAX(arraySizes%KX, arraySizes%LY)
            J = MIN(arraySizes%KX, arraySizes%LY)

            J = MAX(J, arraySizes%IZMAX) ! Ensure J is at least IZMAX (40)
            !---------------------------------Set KXLYD to largest of KX*LY, (KX,LY)*IZMAX
            arraySizes%KXLYD = I * J ! will be either (KX * LY) or (KX * IZMAX) or (LY * IZMAX)

            ! This is to compute the RML array size used ?
            ! Still have to see how this is used
            arraySizes%IRML = (2 * gUserSettings%maxMeridWaves + 1) * gUserSettings%maxMeridWaves

            !---------------------------------Set TMP array to largest of KX or LY
            arraySizes%LYD = I

            arraySizes%IBIG = I * J
            !-------------------------------------Make room for ASXY to plot
            arraySizes%IBIG = MAX(arraySizes%IBIG, MIN_ASXY_SIZE) ! Ensure IBIG is at least MIN_ASXY_SIZE (3721)

            arraySizes%IBOX = 4 * arraySizes%IBIG
            arraySizes%ITOT = meridWaves + 2 * zonalWaves * meridWaves
            arraySizes%IYH = arraySizes%IYMAX / 2 + 1
            arraySizes%IXMAX1 = arraySizes%IXMAX + 1
            
            arraySizes%ITOTND = arraySizes%ITOT * levels ! Forgot to add this
            arraySizes%ITRXD = 3 * arraySizes%IXMAX / 2 + 1
            arraySizes%ITRYD = 3 * arraySizes%IYMAX / 2 + 1
            
            !---------------------------------Statistical array sizes:
            arraySizes%ITOTA = 1
            arraySizes%NALA = 1
            arraySizes%KXV = 1
            arraySizes%LYV = 1
            arraySizes%KXLYA = 1

            !----------------------------------- Total sizes:
            !  If STATS_OPT_NONE give each array 1 element
            !  If STATS_OPT_MEAN allocate PSA,APE,RKE,PSS, and all others one element
            !  If STATS_OPT_VAR allocate all
            !-------------PSA,(WWA,FSA,ADSA,FTA,ADTA,ADOA) APE,RKE,PSS,V1,V2:  The (WWA,FSA,ADSA,FTA,ADTA,ADOA) are the other 6 fields in PSA
            
            IF(gUserSettings%statsOption .NE. STATS_OPT_NONE) THEN ! Both mean and variance use this
                arraySizes%ITOTA = arraySizes%ITOT
                arraySizes%NALA = levels
            end if
            IF(gUserSettings%statsOption .EQ. STATS_OPT_VAR) THEN
                arraySizes%KXLYA = 2 * arraySizes%KXLYD * levels
                arraySizes%NALA = levels
                arraySizes%KXV = arraySizes%KX
                arraySizes%LYV = arraySizes%IYH
                !I = arraySizes%NALA
            end if

        END function SetDimensions

        !************************************************ Default Diabatic profiles:
        !>  This subroutine sets up default diabatic profiles for the levels
        !>  This method is called in Startup when no save file is found
        !>  or from CheckUserSettingsChanges() when there are changes made by the check
        !>  of from CheckModelControlChanges() when levels are changed by the user
        !>  Eventually it will be called when the user directly changes some of the
        !>  changable diabatic parameters that require recalculation of the vertical profiles.
        !!  @Param needPressureLevels: logical indicating whether pressure levels need to be computed
        SUBROUTINE DFDIAB(needPressureLevels)
            use ConstantsModule, only: KAPPA
            use ModelFieldsModule, only: gVertArr, BV
            use ModelParamsModule, only: gModIntParams, gScaleFactors

            Logical, intent(in) :: needPressureLevels

            integer :: N
            REAL :: R, DELP, THS0, THS1, EX, TROP
        !-----------------------------------------------:
        ! TROP=Pressure level of tropopause
        ! THS0=Vorticity level 1 potential temperature
        ! THS1=Tropospheric static stability (dT/dp)in T/mb which is constant
        ! EX=Exponential decay rate of certain profiles
        
        ! Do we need pressure levels?
            IF(needPressureLevels) THEN
                BV = 0.0              ! Clear all the vertical arrays (in ModelFieldsModule). BV encapsulates all the vertical arrays.
        !----------------------------------------Set surface and upper level pressure:
                gVertArr%PP(1) = 1000.
                gVertArr%PP(gModIntParams%NLEV + 1) = 0.
        !-------------------------Compute pressure levels PV, diss FK and FJ:
                DELP = (gVertArr%PP(1) - gVertArr%PP(gModIntParams%NLEV + 1)) / REAL(gModIntParams%NLEV)
                gVertArr%PV(1) = gVertArr%PP(1) - DELP / 2.
                DO N = 1, gModIntParams%NLEV
                    gVertArr%PP(N) = gVertArr%PP(1) - (N - 1) * DELP
                    !--------------------------------Leaves PV(1) alone:
                    gVertArr%PV(N) = gVertArr%PV(1) - (N - 1) * DELP
                END DO
            ENDIF
        !========================================Compute Basic state temp profile:
            TROP = 250.         ! Height of tropopause
            THS0 = 285.         ! Potential temperature at surface (1000 mb)
            THS1 = .14          ! Increase in potential temperature per 100 mb
        !----------------------------------Potential temperature at tropopause
            EX = THS0 + (gVertArr%PV(1) - TROP) * THS1
            DO N = 1, gModIntParams%NLEV
                IF(gVertArr%PV(N) .GT. TROP) THEN
                    gVertArr%THS(N) = THS0 + THS1 * (gVertArr%PV(1) - gVertArr%PV(N))
                ELSE
        !-----------------------------------Stratospheric isothermal profile in Temp.
                    gVertArr%THS(N) = EX * (TROP / gVertArr%PV(N))**KAPPA
                ENDIF
            END DO
            gVertArr%FK(1 : gModIntParams%NLEV) = 0.     ! Zero ekman friction for every level
        !========================Thermal mixing exp decay (Convective mixing of temperature):
            EX = 6.
            DO N = 1, gModIntParams%NLEV - 1
                R = EX * (1. - gVertArr%PV(N) / gVertArr%PV(1))
        !------------------------------Stop convection at Tropopause
                IF(gVertArr%PV(N + 1) .GT. TROP) THEN
                    gVertArr%G(N) = gScaleFactors%dissipation / EXP(R / 2.)
                ELSE
                    gVertArr%G(N) = gScaleFactors%dissipation / 20.
                ENDIF
            END DO
            !  Vort mix=Constant=20 days:
            gVertArr%FJ(1:gModIntParams%NLEV - 1) = gScaleFactors%dissipation / 20.  ! This gives the same value for all N
            gVertArr%FJ(gModIntParams%NLEV) = 0.
        !====================================================Compute PP,DPI,A,AU,SIG
            CALL ComputeVerticalProfiles()
        !------------------------------------------Compute Petri TF and H profiles:
            CALL RaisanenRadiationProfile()
            IF(gModIntParams%NLEV .NE. 1) gVertArr%G(gModIntParams%NLEV - 1) = 0.
        !==========================================Set surface heating and Ekman fric:
            gVertArr%H(1) = gScaleFactors%dissipation / 8.  
            ! This was set in the RaisanenRadiationProfile method. The conversion between a dimensional dissipation value in 1/days
            ! is dimensional_FK = 1.0_wp / (secondsPerDay * nondimensional_FK * scaleCoriolis)
            ! gScaleFactors%dissipation = 1.0_wp / (secondsPerDay * scaleCoriolis)
            ! to increase the damping. Not sure why it is divided by 8. Probably because
            ! the scale factor gives 0.11 / day or about 2.7 hr damping.
            
            gVertArr%FK(1) = gScaleFactors%dissipation * 1000. / (gVertArr%PP(1) - gVertArr%PP(2))
        END subroutine DFDIAB

        !*****************************************Place perturbation in flow:
        SUBROUTINE PERTB()
            use ModelFieldsModule, only: gHorzArr
            use ModelParamsModule, only: gModIntParams
            use ArrayDimensionsModule, only: gCurrentDims

            implicit none

            INTEGER :: N
        !---------------------------------Place in zonal flow that starts at 0 and increases with height:
            DO N = 1, gModIntParams%NLEV
                gHorzArr%PS(1, N) = .1 * REAL(N - 1) / REAL(gModIntParams%NLEV)
            END DO
        !---------------------------------Perturb ALL surface components:
            gHorzArr%PS(1 : gCurrentDims%ITOT, 1) = .001
        END SUBROUTINE PERTB

        !***************************************************** DEFAULTS:
        !> THis function sets default values to gModRealParams, gModIntParams, gModScalingParams,
        !> gModPlotParams, and gModSensParams when there is no save file already containing these
        !> fields.
        SUBROUTINE DEFAULTS()
            use ConstantsModule, only: STATS_OPT_NONE, PI, VORTICITY, HEIGHT, TEMPERATURE, VERTICAL_WIND, TEMP_FORCING, &
                OROG_HGT_TEND, HGT_FORCING, PLOT_OFF, TOT_HGT_TEND_GRAF, TOT_TEMP_TEND_GRAF, MEAN_HGT_TEND_GRAF, MEAN_TEMP_TEND_GRAF, &
                HGT_WAVE_SPEED_GRAF, WEATHER_MAPS, STANDARD_NLEVEL, OROGRAPHY, HGT_TEMP_PHASE_DIFF, GROWTH_RATE_GRAF, &
                AMPLITUDE_RATIO_GRAF, ZLEVEL_1, ZLEVEL_2, ZLEVEL_3
            use ModelParamsModule, only: gModIntParams, gModRealParams, gUserSettings, gUserSettingsPrev, gScaleFactors, &
                gModScalingParams, gModPlotParams, gModSensParams, gPlotScales

            INTEGER I, I1, J, J1
            
            gUserSettings%maxZonalWaves = 10
            gUserSettings%maxMeridWaves = 5
            gUserSettings%maxLevels = 10
            gUserSettings%integrationMethodTransform = .false.
            gUserSettings%statsOption = STATS_OPT_NONE

            gUserSettingsPrev = gUserSettings

        !------------------------------------------------------------- Default Values:
        ! These fields are in type(ModelParams) gModRealParams in the ModelParamsModule
        !  BETA is the beta parameter
        !  RN is the horizontal aspect ratio (channel zonal length / channel width)
        !  DELT is the length of the time step
        !  ZHGT is the height of the model to be displayed
        !  RLAT is the latitude of the Hovmoller and vertical structure plot
        !  XPOS,YPOS is the position of the selected point in the model
        !  BOT,TPP is the pressure range to be displayed in X sections
        !  CONV is a convergence test for equilibrated solutions
        !  SPINUP is a spinup time toss amount for SENSIV runs
        !  TROP is the height of the tropopause in mb
        !  AMPT,AMPS,ANGS,EFOLD are prescribed "bomb" disturbance features
        !
        ! These parameters are dimensionalized and then non dimensionalized
            ! Dimensionalize for input
            ! IF(RN.GT.0.)RN=gScaleFactors%longestWave/RN
            ! DELT=DELT*gScaleFactors%timeStep
            ! BETA=4.*BETA
            ! XPOS=XPOS*RAD_TO_DEGREES
            ! YPOS=YPOS*RAD_TO_DEGREES
            ! Non dimensionalize
            ! RN=gScaleFactors%longestWave/RN
            !--------------------------------Recompute interactions if needed:
            ! IF(RNP.NE.RN)interactCoefsComputed = .false.
            ! DELT=DELT/gScaleFactors%timeStep
            ! BETA=BETA/4.
            ! XPOS=XPOS/RAD_TO_DEGREES
            ! YPOS=YPOS/RAD_TO_DEGREES
        !-------------------------------------------------Set user default parameters:
            gModRealParams%isDimensionalized = .FALSE.
            gModRealParams%RN = gScaleFactors%longestWave/28000.
            gModRealParams%BETA = .25
            gModRealParams%DELT = 90./gScaleFactors%timeStep
            gModRealParams%ZHGT = 900.
            gModRealParams%ZHGT2 = 950.
            gModRealParams%ZHGT3 = 500.
            gModRealParams%RLAT = 45.
            gModRealParams%RLON = 0.
            gModRealParams%XPOS = 0.
            gModRealParams%YPOS = PI/4.
            gModRealParams%BOT = 1000.
            gModRealParams%TPP = 0.
            gModRealParams%CONV = 0.
            gModRealParams%SPINUP = 160.
            gModRealParams%TROP = 250.
            gModRealParams%AMPT = 500.
            gModRealParams%AMPS = 50.
            gModRealParams%ANGS = 60.
            gModRealParams%EFOLD = 3.
        !----------------------------------------------VR variables
        !  ZLEV is the contour set to color level zero for the maps
        !  XLOW,XHIG is the upper,lower range of the x-axes for phase space/time series
        !  LLOW,LHIG is the upper,lower range of the y-axes for phase space/time series
        !  XINT is the interval to draw a grid in x for phase space plots
        !  YINT is the interval to draw a grid in y
        !  Plot order of 2-D: VOR,PS,TH,WW,TF,S,T,FS,ADS,ADT
        !  Plot order of 1-D: ADO,TOP,SF
        !---------------------------------------- (Gives 60m contours for L=5000km/PI)
            gModScalingParams%VR(VORTICITY) = 1.0 / gPlotScales(VORTICITY)
        !---------------------------------------- (Gives 60m contours for L=5000km/PI)
            gModScalingParams%VR(HEIGHT) = 60.0 / gPlotScales(HEIGHT)
            gModScalingParams%VR(TEMPERATURE) = 5.0 / gPlotScales(TEMPERATURE)
            gModScalingParams%VR(VERTICAL_WIND) = 5.0 / gPlotScales(VERTICAL_WIND)
            gModScalingParams%VR(TEMP_FORCING) = gModScalingParams%VR(TEMPERATURE)
            DO I = 1, 4
               gModScalingParams%VR(I + 5) = 60.0 / gPlotScales(I+5)
               gModScalingParams%VR(I + 9) = 5.0 / gPlotScales(I+9)
            END DO
            gModScalingParams%VR(OROG_HGT_TEND) = 60.0 / gPlotScales(OROG_HGT_TEND)
            gModScalingParams%VR(OROGRAPHY) = 100.0 / (90.0 * 11.0)
            gModScalingParams%VR(HGT_FORCING) = 60.0 / gPlotScales(HGT_FORCING)
            gModScalingParams%VR(PLOT_OFF) = 1.0 / gPlotScales(PLOT_OFF)
        !------------------------------Graphs only forces:
            gModScalingParams%VR(TOT_HGT_TEND_GRAF) = 60.0 / gPlotScales(TOT_HGT_TEND_GRAF)
            gModScalingParams%VR(TOT_TEMP_TEND_GRAF) = 5.0 / gPlotScales(TOT_TEMP_TEND_GRAF)
            gModScalingParams%VR(MEAN_HGT_TEND_GRAF) = 60.0 / gPlotScales(MEAN_HGT_TEND_GRAF)
            gModScalingParams%VR(MEAN_TEMP_TEND_GRAF) = 5.0 / gPlotScales(MEAN_TEMP_TEND_GRAF)
        !------------------------------Phase speed and phase difference
            gModScalingParams%VR(HGT_WAVE_SPEED_GRAF) = 50.0 / gPlotScales(HGT_WAVE_SPEED_GRAF)
            gModScalingParams%VR(HGT_TEMP_PHASE_DIFF) = 180.0
            gModScalingParams%VR(GROWTH_RATE_GRAF) = 1.0
            gModScalingParams%VR(AMPLITUDE_RATIO_GRAF) = 1.0
            gModScalingParams%VR(ZLEVEL_1) = 7.0     ! ZLEV of old
            gModScalingParams%VR(ZLEVEL_2) = 0.0     ! ZLV2 of old
            gModScalingParams%VR(ZLEVEL_3) = 0.0     ! ZLV3 of old
        
        !------------------------------------------------------------ The ICN values.
        !  KMAX2 the number of zonal waves
        !  LMAX2 the number of meridional waves (and zonal components)
        !  NLEV the number of vertical levels
        !  ICYC the number of cycles of time steps ISTEP
        !  ISTEP the number of time steps in a cycle
        !  MOUT is the number of time steps between outputs
        !  MPLOT is the type of plot
        !  IPLT1,IPLT2 the quantity to plot to the graphic screen
        !  JX and JY are the wave#s of quant to plot to phase space/time series diagram
        !  NUMBX,NUMBY are the number of maps in the x, y directions
        !  modelType: tells what type of model to be run. One of STANDARD_NLEVEL, BAROCLINIC_STRUCTS, ZONAL_MEAN_FLOWS
        !  JX1,JY1 are the number of waves to retain in the 'at a point' plots
        !-------------------------------------------------------------ICN(31) values:
        !  maxZonalWaves,maxMeridWaves: are the 'frame' maximum horizontal resolutions
        !  maxLevels: is the maximum vertical resolution
        !  statsOption: is the statistics option
        !  integrationMethodTransform: indicates interaction/transform model
        !  videoMode is the video resolution
        !----------------------------------------------------JCN values
        !  ISTRM is the type of streamline/contour plot to make
        !  JSTRM is the streamline color of the second map
        !  ICS is the screen clear color
        !  LP1 is the color of the first phase space/time series point
        !  LP2 
        !  LPL is the lead color of the phase space/time series cross
        !  ICA is the color of the phase space axes
        !  ICTXT is the color of any text
        !  ICFS,ICF,ICFH are the plot frames colors
        !  NUM is the color permutation option for weather maps
        !
            gModIntParams%KMAX2 = gUserSettings%maxZonalWaves     ! gUserSettings values set in ModelParamsModule
            gModIntParams%LMAX2 = gUserSettings%maxMeridWaves
            gModIntParams%NLEV = gUserSettings%maxLevels
            gModIntParams%ICYC = 1
            gModIntParams%ISTEP = 5840
            gModIntParams%MOUT = 4 
            gModIntParams%MPLOT = WEATHER_MAPS
            gModIntParams%IPLT1 = TEMPERATURE
            gModIntParams%IPLT2 = HEIGHT
            gModIntParams%IPLT3 = PLOT_OFF
            gModIntParams%JX = 1
            gModIntParams%JY = 1
            gModIntParams%LEVP = 0
            gModIntParams%NUMBX = 1
            gModIntParams%NUMBY = 2
            gModIntParams%modelType = STANDARD_NLEVEL
            gModIntParams%JX0 = 0
            gModIntParams%JY0 = 1
            gModIntParams%JX1 = gModIntParams%KMAX2
            gModIntParams%JY1 = gModIntParams%LMAX2
        !----------------------------------------------Stats variables:
            gModIntParams%IV1 = gModIntParams%IPLT1
            gModIntParams%IV2 = gModIntParams%IPLT2
            gModIntParams%IPRM1 = 1
            gModIntParams%IPRM2 = 8
            gModIntParams%ITER1 = 1
            gModIntParams%ITER2 = 1
            gModIntParams%KMODE = 0
            gModIntParams%LMODE = 1
            gModIntParams%IADD = 0

        !----------------------------------------------JCN variables
            gModPlotParams%ISTRM = -1
            gModPlotParams%JSTRM = 0
            gModPlotParams%KSTRM = 4
            gModPlotParams%LP1 = 9
            gModPlotParams%LP2 = 5
            gModPlotParams%LPL = 8
            gModPlotParams%ICS = 0
            gModPlotParams%ICA = 12
            gModPlotParams%ICTXT = 8
            gModPlotParams%ICFS = 13
            gModPlotParams%ICF = 11
            gModPlotParams%ICFH = 10
            gModPlotParams%ISCRN = 1

        !-------------------------------Defaults for SENSIV routine:
        !  MSEN(1-3)=Fields 1-3; PS,th,ww
        !  MSEN(4)=Value to vary along x-axis; K,L,N,Parameter#2
        !  MSEN(5)=Value to vary along y-axis;
        !  MSEN(6-8)=Wavenumber & Level of field to view
        !  MSEN(9)=IFLIP=Flip the display array option
        !  MSEN(10-11)=Experiment number
        !  MSEN(12-14)=Integral indicator
        !-------------------------------Defaults:
            DO I = 1,3
                gModScalingParams%VSEN(I) = 1.
            END DO
            gModSensParams%field1 = 2    ! field1 is the first parameter to vary in sensitivity runs (MSEN(1)) ?
            gModSensParams%field2 = 3    ! field2 is the 2nd parameter to vary in sensitivity runs (MSEN(2)) ?
            gModSensParams%field3 = 4    ! field3 is the 3rd parameter to vary in sensitivity runs (MSEN(3)) ?
            gModSensParams%zonalWave = 1 ! zonalWave is the zonal wavenumber of the field of view (MSEN(6))
            gModSensParams%meridWave = 1 ! zonalWave is the meridional wavenumber of the field of view (MSEN(7))
            gModSensParams%level = 1     ! level is the pressure level of the field of view (MSEN(8))
            gModSensParams%runNum1 = 1   ! run number1 (MSEN(10)) 
            gModSensParams%runNum2 = 1   ! run number2 (MSEN(11)) 
            gModSensParams%integralEnergyIndicator1 = 1. ! integral energy ? (MSEN(12)) Not sure what these do yet
            gModSensParams%integralEnergyIndicator2 = 1. ! integral energy ? (MSEN(13))
            gModSensParams%integralEnergyIndicator3 = 1. ! integral energy ? (MSEN(14))
            gModSensParams%IXAXIS = 3
            gModSensParams%IYAXIS = 5
            gModSensParams%IFLIP = 0

            !------------------------------Set 'previous' plot quantity:
            DO I = 1,5
                gModIntParams%MP(I) = gModIntParams%IPLT1
                !-----------------------------Set graphs (plus reset one map!)
                gModIntParams%MP(I + 4) = gModIntParams%IPLT1
                gModIntParams%MP(I + 9) = gModIntParams%IPLT2
                gModIntParams%MP(I + 14) = gModIntParams%IPLT3
            END DO
        END SUBROUTINE Defaults

        !> Subroutine to dimensionalize the gModRealParams before displaying to the user
        !> Only five of the parameters are affected.
        subroutine DIMENSIONALIZE()
            use ConstantsModule, only: RAD_TO_DEGREES
            use ModelParamsModule, only: gModRealParams, gScaleFactors
            
            ! If already dimensionalized dont do it again!
            if (gModRealParams%isDimensionalized) return

            IF(gModRealParams%RN .GT. 0.)gModRealParams%RN = gScaleFactors%longestWave/gModRealParams%RN
            gModRealParams%DELT = gModRealParams%DELT * gScaleFactors%timeStep
            gModRealParams%BETA = 4. * gModRealParams%BETA
            gModRealParams%XPOS = gModRealParams%XPOS * RAD_TO_DEGREES
            gModRealParams%YPOS = gModRealParams%YPOS * RAD_TO_DEGREES
            gModRealParams%isDimensionalized = .TRUE. ! Indicate dimensionalized
        end subroutine DIMENSIONALIZE

        !> Subroutine to nondimensionalize the gModRealParams for use in the model after the
        !> user has examined and or changed them. Only five of the parameters are affected.
        subroutine NONDIMENSIONALIZE()
            use ConstantsModule, only: RAD_TO_DEGREES
            use ModelParamsModule, only: gModRealParams, gScaleFactors

            ! If already non-dimensionalized dont do it again!
            if (.NOT. gModRealParams%isDimensionalized) return
            
            IF(gModRealParams%RN .GT. 0.) gModRealParams%RN = gScaleFactors%longestWave/gModRealParams%RN
            !--------------------------------Recompute interactions if needed:
            ! IF(RNP.NE.RN)interactCoefsComputed = .false.
            gModRealParams%DELT = gModRealParams%DELT / gScaleFactors%timeStep
            gModRealParams%BETA = gModRealParams%BETA / 4.
            gModRealParams%XPOS = gModRealParams%XPOS / RAD_TO_DEGREES
            gModRealParams%YPOS = gModRealParams%YPOS / RAD_TO_DEGREES
            gModRealParams%isDimensionalized = .FALSE. ! Indicate non-dimensionalized
        end subroutine NONDIMENSIONALIZE

        !***************************SUBROUTINE FOR PRECOMPUTABLE VERTICAL ARRAYS:
        !>  This routine computes all the vertical profiles given the user input
        !>  pressure levels PV and hydrostatic basic state potential temperature
        !>  profile THS.  From these two profiles all the remaining arrays SIG,PP,
        !>  DP,DPI,A, and AU are computed. These fields are located in gVertArr
        !>  which is declared and instantiated in the ModelFieldsModule.
        !>  Method is called after reading the default save file and from within
        !>  DFDIAB.
        !  
        SUBROUTINE ComputeVerticalProfiles()
            use ModelFieldsModule, only: gVertArr
            use ConstantsModule, only: OROGRAPHY, CP, CORIOLIS, KAPPA
            use ModelParamsModule, only: gModIntParams, gScaleFactors, gPlotScales

            !---------------------------------Order of storage:
            !  PP,PV,DP,DPI,THS,FK,FJ,AU,A,H,G,SIG

            !  Compute the PPs based upon the user specified PV's THEN:
            !  Compute the Delta P's where P is defined at the surfaces separating the
            !  layers.  Thus the PP has maxLevels+1 elements.  PP(1)=surface and PP(maxLevels+1)
            !  is the top. The delta-P's are needed at the interfaces where omega is
            !  defined (DPI) and for computing the strm at temperature level (DP).
            !  The gVertArr instance has been allocated such that all the field are
            !  one after the other in the order above.

            !--------------------------------------Local variables:
            INTEGER :: N
            REAL :: PU, PL, R, RL, RU, X

            !-----------------------------Compute PP by setting to the middle of PV's:
            DO N = 2, gModIntParams%NLEV
                gVertArr%PP(N) = (gVertArr%PV(N - 1) + gVertArr%PV(N)) / 2.
            END DO
            !-----------------------------Compute the delta P's and the ratio PP(1)/DP(n)
            DO N = 1, gModIntParams%NLEV
                gVertArr%DP(N) = gVertArr%PP(N) - gVertArr%PP(N + 1)
                gVertArr%DPI(N) = gVertArr%PP(1) / gVertArr%DP(N)
            END DO
            !--------------------------------Set scale height for orography as 11m/mb:
            gPlotScales(OROGRAPHY) = (gVertArr%PP(1) - gVertArr%PP(2)) * 11.
            !--------------------------------------------Compute ratio array AU:
            !  This ratio is needed in the turbulent transfer term. At each layer inter-
            !  face we need the ratios AU(N)=A(N)/A(N+1) and AL(N)=A(N)/A(N-1)=1./AU(N-1).
            !  The value A(N) is given by the pressures in the Centers of each layer and
            !  not the interface, so we have to approximate those values. Call PL the
            !  center of the level under PP(N) and PU the center of the level over PP(N).
            !  There is no AL at level 1 and no AU at level NLEV-1, so just NLEV-2 values.
            
            ! A(N) will be the scaling factor for the temperature.
            !
            PL = gVertArr%PV(1)**KAPPA      ! p(1)**kappa
            PU = gVertArr%PV(2)**KAPPA      ! p(2)**kappa
            RL = PL - PU                    ! p(1)**kappa - p(2)**kappa
            R = CP / (CORIOLIS * gVertArr%PP(1)**KAPPA)  ! Cp/ (2*Omega*p(sfc)**kappa) NO - removing divisor 2
            gVertArr%A(1) = R * RL          ! Cp(p(1)**kappa - p(2)**kappa)/ (2*Omega*p(sfc)**kappa)
                                            ! Cp has units of J/kg/K, the pressures have units of mb**kappa, 
                                            ! and the Coriolis parameter has units of 1/s.
                                            ! Joule = kg m**2 / s**2 giving A units of m**2/(s K) 
                                            ! General: A(N) = CP * (PV(N)**KAPPA - PV(N+1)**KAPPA) / (CORIOLIS * PP(1)**KAPPA)
            gVertArr%AU(1) = 0.
            DO N = 2, gModIntParams%NLEV - 1
            !----------------------------Get pressure at Strm level below temp level N:
                PL = PU
            !----------------------------Get pressure at strm level above temp level N:
                PU = gVertArr%PV(N + 1)**KAPPA
            !----------------------------------Compute the numerator A at level N:
                RU = PL - PU
            !----------------------------------Divide value into A at level N-1 for ratio:
                gVertArr%AU(N - 1) = RL / RU
            !----------------------------------Compute A at level N
                gVertArr%A(N) = R * RU
                RL = RU
            END DO
            !--------------------------------Set aside root scale factor:
                !AT = gVertArr%A(1)      ! We'll figure this out later. This is some kind of item for plotting
            !--------------------------------Compute sigma profile AND nondimensionalize:

            R = gVertArr%PP(1) / gScaleFactors%temperatureScale
            ! gScaleFactors%temperatureScale = CHANNEL_WIDTH * CHANNEL_WIDTH * CORIOLIS    ! 2.6121725E+08 m*m/s
            ! R = mb s / m**2
            ! gVertArr%A(N) has units of m**2/(s K) 
            ! X has units of mb*s*m**2/(s*K*m**2) = mb/K
            ! SIG has units K /mb
            DO N = 1, gModIntParams%NLEV - 1
                X = R * gVertArr%A(N)
                gVertArr%SIG(N) = (gVertArr%THS(N + 1) - gVertArr%THS(N)) / (gVertArr%PV(N) - gVertArr%PV(N + 1))
                gVertArr%SIG(N) = gVertArr%SIG(N) * X
            END DO

        END subroutine ComputeVerticalProfiles

        !********************************************"Radiation" Routine:
        SUBROUTINE RaisanenRadiationProfile()
            use ConstantsModule, only: KAPPA, SQRT2, TEMPERATURE
            use ModelFieldsModule, only: gVertArr, gHorzArr
            use ModelParamsModule, only: gModIntParams, gScaleFactors, gPlotScales
            !use ArrayDimensionsModule
            !use UtilitiesModule
        
            implicit none

        !---------------------------------------------------------------Model for TF:
        !  SW = short wave   LW = Long wave
        !  We have a simple system where dT/dt = LW + SW forcing.  The LW forcing is
        !  linearized about the basic state temperature
        !                 LW = -h0(z)-4*h0(z)*(TF(y,z)-T0(z))/T0(z)
        !  (The Long Wave cooling is linearized about the blackbody T**4 radiation)
        !  When TF=T0 there is only a steady cooling due to T0.
        !  The SW heating rate is given by S(y,z). We will assume that the LW cooling
        !  constant h0(z) and T0(z) are independent of y. In the QG model, T0(z) will be
        !  the basic state temperature lapse rate used to compute the static stability
        !  and thus it is independent of x and y.
        !  Equilibrium in the absence of dynamics will give TF(y,z) which we can use
        !  as the relaxation temperature in our model. The Newtonian cooling h(z) will
        !  be given by
        !                 h(z) =  .25*T0(z)/h0(z).
        !  Solving for TF
        !             SW(y,z) = h0(z)+4*h0(z)*(TF(y,z)-T0(z))/T0(z).
        !  In spectral form, there will be a mean heating
        !             SW0(z) = h0(z)+4*h0(z)*(TF0(z)-T0(z))/T0(z).
        !  All wave components will be given by
        !             SW(y,z) = 4*h0(z)*TF(y,z)/T0(z).
        !  or
        !             TF(y,z) = SW(y,z)*T0(z)/(4*h0(z)).
        !-----------------------------------------------------Data arrays:
        !  Data comes from Petri Raisanen using a real radiation package on a US
        !  standard atmosphere at 23 levels and 10 zenith angles.
        !  
            REAL AA, BB, R, X, Y
            INTEGER I, J, N
            
        ! Pressure levels where the values have been calculated.
            REAL, PARAMETER :: PRESSURE(23) = [950.0, 850.0, 750.0, 650.0, 550.0, 450.0, 375.0, &
                                               325.0, 275.0, 237.5, 212.5, 187.5, 162.5, 137.5, &
                                               112.5, 87.5, 62.5, 40.0, 25.0, 15.0, 7.5, 3.5, 1.0]
        !---------------------------------------------------------SW forcing:
        ! These values are heating rates in units of degrees K per day
            real, parameter :: SHORT_WAVE(23) = [1.06, 1.11, 1.18, 1.20, 1.12, 1.06, .93, .81, .68, .53,&
                                                .43, .36, .36, .40, .47, .62, .93,1.47,2.07,3.03,4.72, &
                                                9.40, 16.38]
        !---------------------------------------------------------------H0(N):
        ! These values are a cooling rate in units of degrees K per day.
            real, parameter :: H0(23) = [1.77, 1.73, 1.75, 1.74, 1.63, 1.64, 1.71, 1.48, 1.33, 1.03, .63, &
                                         .31, .21, .25, .30, .38, .55, .90, 1.32, 2.00, 3.10, 5.54, 8.96]

        !---------------------------------------------------------------Start:
        !  For now we will approximate the meridional profile of SHORT_WAVE based upon a simple
        !  Acosy profile. To get A we divide SHORT_WAVE(L,N)/(FLOAT(L)*.05).  This works
        !  very well in the troposphere.  The stratosphere decreases more rapidly.
        !  Here we have L=10 so we simply divide SHORT_WAVE by 2.
        !
        !  The work is interpolating NLEV to the 23 levels available:
        !  Everything is dimensional since THS is dimensional.
        !-------------------------------------------------------Start:
            DO N = 1, gModIntParams%NLEV - 1
                J = 1
                IF(gVertArr%PP(N + 1) .LE. PRESSURE(1)) THEN
                    DO I = 1, 22
                        IF(gVertArr%PP(N + 1) .LE. PRESSURE(I) .AND. gVertArr%PP(N + 1) .GT. PRESSURE(I + 1))THEN
                            J = I
                            EXIT
                        ENDIF
                    end DO
                ENDIF
        !------------------------------------------------------Get weights:
                AA = (gVertArr%PP(N + 1) - PRESSURE(J + 1)) / (PRESSURE(J) - PRESSURE(J + 1))
                BB = 1. - AA
        !--------------------------------------------------Get weighted cooling rate in deg K/day:
                Y = H0(J) * AA + H0(J + 1) * BB
        !--------------------------------------------------TH = R*T Potential temperature conv:
                R = (1000. / gVertArr%PP(N))**KAPPA
        !--------------------------------------------------Compute time scale (Y * R converts the Y temperature to potential temperation):
                gVertArr%H(N) = .25 * gVertArr%THS(N) / (Y * R) ! The .25 is the linearization of the T**4 boltzmann radiation I believe.
        !--------------------------------------------------Nondimensionalize:
                IF(gVertArr%H(N) .NE. 0.) gVertArr%H(N) = gScaleFactors%dissipation / gVertArr%H(N)
        !--------------------------------------------------Zero TF at level N
                gHorzArr%TF(1 : gArrayDims%ITOT, N) = 0.0
        !--------------------------------------------------Compute equilibrium TF:
                X = (SHORT_WAVE(J) * AA + SHORT_WAVE(J + 1) * BB) * gVertArr%THS(N)
                gHorzArr%TF(1, N) = X / (4. * Y * SQRT2)
                gHorzArr%TF(1, N) = gHorzArr%TF(1, N) * gVertArr%A(N) / gPlotScales(TEMPERATURE)
            END DO

        END subroutine RaisanenRadiationProfile

end module SetUpModule
