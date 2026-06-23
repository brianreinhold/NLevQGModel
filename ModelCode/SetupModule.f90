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
    public :: CheckUserSettingsBounds     ! Function to santity check the gUserSettings values
    public :: CheckModelResolutions     ! Function to sanity check and correct model resolution parameters in gModIntParams
    public :: CheckModelControlChanges
    public :: SetDimensions     ! SetDimensions which gets the dimensions for array sizes for allocation
    public :: SetModRealParamsDimensionalState    ! Call this function to dimensionalize those fields of gModRealParams that need it for display to the user
    public :: SetDefaults  ! This creates the default values for all the fields in the ModelParamsModule
    public :: DFDIAB    ! Computes default diabatic vertical profiles
    public :: CreatePerturbation     ! Introduces a perturbation into the flow:
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

        ! The user has two levels of control over the model parameters. The first level is set in the
        ! Settings dialog and that is the maximum number of zonal waves, meridional waves and levels that
        ! the model can have. These are stored in the gUserSettings type. The allocation of the model arrays
        ! based upon those values. The user then has a second level of control and this is done in the 
        ! Model Control dialog. The user can change the actual number of zonal waves, meridional waves and levels
        ! without reallocation as long as those values are less than or equal to those in gUserSettings. These
        ! values are stored in the gModIntParams type. 
        ! Since the user can change the values in gUserSettings we need to keep a second type gUserSettingsPrev to 
        ! compare to the current gUserSettings values to see if reallocation is required. 
    
        ! When the user first starts the following happens:
        ! StartUp is called. 
        !   It initializes some constants that will never change.
        !   It tries to read the default save file.
        !      If the file is found
        !         the values are read and checked to valid.
        !         the arrays are allocated based upon the read values
        !      If the file is not found or there is an error reading the file
        !         defaults are set
        !         the arrays are allocated based upon the default values
        !   The Settings Dialog appears

        !> **************************************************** StartUp
        function StartUp() result(errorCode)
            use ifwinty!, only ERROR_FILE_NOT_FOUND
            use ifwin

            use ConstantsModule, only: MAP_OVERLAY_1, POTENTIAL_TEMP, OMEGA, INVALID
            use ModelParamsModule, only: gUserSettings, gModIntParams, gModRealParams, gModSensParams, &
                gNLevFile, gPlotControls, gSensFile, initConstants
            use NLevQGModelGlobals, only: LogMessage, ghwndMain    ! For logging
            use ArrayDimensionsModule, only: gArrayDims, gCurrentDims
            use ModelFieldsModule, only: createHorzArr
            use Fftw3HelpersModule, only: setup_fftw_plans
            use UtilitiesModule, only: openModelFile

            implicit none

            CHARACTER(LEN=64) :: errorStr
            integer :: errorCode

            !INTEGER, PARAMETER :: ERROR_FILE_NOT_FOUND = 2 ! More efficient than using use 'ifwinty'
            ! This method does not need to be called again.
            call initConstants()

            errorCode = 0     ! Initialize err code to success

            !--------------------------- Read data from default file 'NLEVQG.SAV'. FileStruct 'gNLevFile' declared and defined in ModelParametersModule
            gNLevFile%fileName = 'NLEVQG.SAV'
            
            !---------------------------Set active variable to first
            gPlotControls%activePlotMap = MAP_OVERLAY_1   ! Three possible fields can be displayed on a map. The 'active' one is the field that can
                                            ! be changed by the user with the 'field' key. The other two are inactive and will not change
                                            ! These fields are given by gModIntParams%field1, field2, field3
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
            errorCode = openModelFile(gNLevFile, .true., ghwndMain)
            if (errorCode == 0) errorCode = ReadSaveFile(gNLevFile)   ! function is 1ocated in this file
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
                ! we initialize the model with default values. 
                call SetDefaults()   ! Subroutine 1ocated in this file
                !-----------------Get and set array dim, Allocate, SET default profiles:

                errorCode = SetAndAllocateArrays()
                if (errorCode .NE. 0) then
                    call LogMessage('The model arrays could not be allocated with default parameters. Cannot run model.')
                    errorCode = MessageBox(ghwndMain, "The model arrays could not be allocated with default parameters."//char(0), "Allocation failure"//char(0), MB_OK)
                    return
                endif
            end if
            call setup_fftw_plans(gCurrentDims%IXMAX, gCurrentDims%IYMAX)
        end function StartUp

        function SetAndAllocateArrays() result(errorCode)
            use ArrayDimensionsModule, only: gArrayDims, gCurrentDims
            use ModelParamsModule, only: gUserSettings, gModIntParams

            implicit none

            integer :: errorCode

            errorCode = 0
            
            !---------------------------------Set array dimensions for allocation:
            !  This method loads the IXMAX, IYMAX, IZMAX, KX, LY, IBIG, IBOX, ITOT, IYH, IXMAX1
            !  values into the gArrayDims type.
            gArrayDims = SetDimensions(gUserSettings%maxZonalWaves, gUserSettings%maxMeridWaves, gUserSettings%maxLevels)  ! Function in module SetUpModule
            gCurrentDims = SetDimensions(gModIntParams%KMAX2, gModIntParams%LMAX2, gModIntParams%NLEV)

            ! This function allocates all the arrays and creates the default vertical profiles and perturbs the fields
            ! The dimensions in the gUserSettings and gModIntParams types are the same as is the gArrayDims and gCurrentDims.
            errorCode = newAllocation()
        end function SetAndAllocateArrays

        !> This method allocates all the model arrays assuming that nothing has been allocated yet BUT
        !> the gUserSettings and gModIntParams fields have been set to values read from a file or the defaults.
        function newAllocation() result(error)
            use ConstantsModule, only: USE_INTERACTION_COEFFICIENTS
            use ModelFieldsModule, only: AllocateHorizontalArrays, AllocateTransformArrays, AllocateVerticalArrays, AllocateStatsArrays, &
                       createHorzArr, createTransArr, createVertArr, &
                       DeallocateHorizontalArrays, DeallocateTransformArrays, DeallocateVerticalArrays, DeallocateStatsArrays
            use InteractionCoefficientsModule, only: AllocateCintArrays, computeInteractionCoefficients, createCintArr, gCintArr, DeallocateCintArrays
            use ModelParamsModule, only: gUserSettings, gModIntParams, gModRealParams

            implicit none

            integer :: error, currentNoOfInteractions

            error = 0

            error = DeallocateHorizontalArrays()
            error = AllocateHorizontalArrays()
            if (error .NE. 0) return
            error = createHorzArr(gModIntParams%KMAX2, gModIntParams%LMAX2, gModIntParams%NLEV, gCurrentDims)
            if (error .NE. 0) return

            error = DeallocateTransformArrays()
            error = AllocateTransformArrays()
            if (error .NE. 0) return
            error = createTransArr(gCurrentDims)
            if (error .NE. 0) return
            error = DeallocateVerticalArrays()
            error = AllocateVerticalArrays()
            if (error .NE. 0) return
            error = createVertArr(gModIntParams%NLEV)
            if (error .NE. 0) return
            call DFDIAB(.true.)  ! Subroutine 1ocated in this file
            call CreatePerturbation() ! Subroutine 1ocated in this file
            ! TODO fix to be consistent with the others
            error = DeallocateStatsArrays()
            error = AllocateStatsArrays()
            if (error .NE. 0) return
            if (gUserSettings%integrationMethod == USE_INTERACTION_COEFFICIENTS) then
                ! Using interaction coefficients
                error = DeallocateCintArrays()
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
        function SaveModelState(fileData, deallocateFields) result(errorCode)
            use NLevQGModelGlobals, only: LogMessage    ! For logging
            use ModelParamsModule, only: FileStruct
            use ModelFieldsModule, only: DeallocateHorizontalArrays, DeallocateVerticalArrays, DeallocateStatsArrays
            
            implicit none
            
            type(FileStruct), intent(inout) :: fileData
            LOGICAL, intent(in) :: deallocateFields

            integer :: errorCode
            logical :: fileExists
            
            errorCode = 0

            errorCode = WriteFileData(fileData%fileHandle)
            if (errorCode .NE. 0) then
                call LogMessage('Error writing data to file. Model state could not be saved.')
            else
                call LogMessage('Model state has been saved to file ' // trim(fileData%fileName) //'.')
            end if
            close(fileData%fileHandle)
            fileData%fileHandle = -1

            if (deallocateFields) then
                ! Just borrowing the existing logical variable to call the deallocation functions
                fileExists = DeallocateHorizontalArrays()
                fileExists = DeallocateVerticalArrays()
                fileExists = DeallocateStatsArrays()
            endif
        end function SaveModelState

        !***************************************************************** ReadSaveFile:
        !>  Routine which reads in data from SAV file and allocates memory.
        !>
        !! @Param fileData: FileStruct type containing the information about the saved file
        function ReadSaveFile(fileData) result(errorCode)
            use ModelParamsModule, only: gUserSettings, gModIntParams, gModRealParams, gModSensParams, gUserSettingsPrev, FileStruct
            use ModelFieldsModule, only: gVertArr
            use NLevQGModelGlobals, only: LogMessage
            use kernel32            ! For windows system calls like GetLastError
            use ISO_C_BINDING       ! Also for the 'GetLastError' windows system function

            type(FileStruct), intent(inout) :: fileData ! isOpened element can get changed

            ! local variables
            integer :: errorCode            ! F90 does not allow inline initialization of a return value. Don't know why
            integer :: i, m
            LOGICAL :: fileExists
            logical :: res

            errorCode = 0

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
            if (.NOT. CheckUserSettingsBounds()) then    ! Function is 1ocated in this file
                call LogMessage('The data read from ' // trim(fileData%fileName) // ' had incorrect bounds on certain parameters. Need to invoke defaults.')
                ! Bad parameters loaded; load defaults
                errorCode = -1  ! Value -1 to separate from system error which tend to be > 0 (as far as I know)
                return
            end if
            errorCode = CheckModelResolutions()
            ! Since we are on the way to the model control variables here, we don't care about the return
            ! value which only says something was out of bounds and has been fixed.

            errorCode = SetAndAllocateArrays()
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
            use ModelParamsModule, only: gUserSettings, gModIntParams, gModRealParams, gModSensParams, gContourIntervals, gUserSettingsPrev
            use NLevQGModelGlobals, only: LogMessage    ! For logging
            !use ifport              ! For the 'GetLastError' windows system function
            !use ISO_C_BINDING       ! Also for the 'GetLastError' windows system function

            INTEGER, intent(in) :: fileHandle
            
            integer :: errorCode, MIRI
            CHARACTER(LEN = 256) :: errorMsg
            
            errorCode = 0

            REWIND(fileHandle)

            ! Read header
            READ(fileHandle, IOSTAT = errorCode, IOMSG = errorMsg) MIRI, gUserSettings, gModRealParams, gModIntParams, &
                gModSensParams, gContourIntervals
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
            !use NLevQGModelGlobals, only: LogMessage    ! For logging
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
                  gVertArr%X, gVertArr%Y, gVertArr%PRF, gVertArr%LVS

            READ(fileHandle, IOSTAT = errorCode, IOMSG = errorMsg) &
                  gHorzArr%allocated, &
                  gHorzArr%GQ, gHorzArr%BTA, gHorzArr%PSB, gHorzArr%PS, gHorzArr%TH, gHorzArr%WW, gHorzArr%TF, &
                  gHorzArr%FS, gHorzArr%ADS, gHorzArr%WS, gHorzArr%S, gHorzArr%FT, gHorzArr%ADT, gHorzArr%WT, &
                  gHorzArr%T, gHorzArr%SF, gHorzArr%ADO, gHorzArr%TOP, &
                  gHorzArr%DAT, &
                  gHorzArr%AKL, gHorzArr%AKLI, gHorzArr%ZS, gHorzArr%PSSAVE

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
            use ModelParamsModule, only: gUserSettings, gModIntParams, gModRealParams, gModSensParams, gContourIntervals
            use ModelFieldsModule, only: gVertArr
            
            implicit none

            INTEGER, intent(in) :: fileHandle

            ! Local variables
            INTEGER :: errorCode
            INTEGER :: MIRI
            CHARACTER(LEN = 256) :: errorMsg

            ! Write data
            WRITE(fileHandle, IOSTAT = errorCode, IOMSG = errorMsg) MIRI, gUserSettings, gModRealParams, gModIntParams, &
                gModSensParams, gContourIntervals
            WRITE(fileHandle, IOSTAT = errorCode, IOMSG = errorMsg) &
                  gVertArr%allocated, &
                  gVertArr%PP, gVertArr%PV, gVertArr%DP, gVertArr%DPI, gVertArr%THS, gVertArr%FK, gVertArr%FJ, &
                  gVertArr%AU, gVertArr%A, gVertArr%H, gVertArr%G, gVertArr%SIG, gVertArr%X, gVertArr%Y, gVertArr%PRF, gVertArr%LVS

        end function WriteSensFileHeader

        !****************************************************************** WriteFileData:
        !> Writes the data to the file specified by fileHandle. This write does both
        !> the header and the data arrays. The OPEN statement and CLOSE statements
        !> are done by the caller.
        !! @param fileHandle The file handle to write to.
        !! @return errorCode Returns 0 on success, or a Windows error code on failure.
        FUNCTION WriteFileData(fileHandle) RESULT(errorCode)
            use ModelParamsModule, only: gUserSettings, gModIntParams, gModRealParams, gModSensParams, gContourIntervals
            use ModelFieldsModule, only: gVertArr, gHorzArr, gStatsArr
            use InteractionCoefficientsModule, only: gCintArr
            !use NLevQGModelGlobals, only: LogMessage    ! For logging
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
            WRITE(fileHandle, IOSTAT = errorCode, IOMSG = errorMsg) MIRI, gUserSettings, gModRealParams, gModIntParams, &
                gModSensParams, gContourIntervals
            ! Write model arrays (since they are allocatable, we need to write each array indivually and not just the type gVertArr, etc.)
            WRITE(fileHandle, IOSTAT = errorCode, IOMSG = errorMsg) &
                  gVertArr%allocated, &
                  gVertArr%PP, gVertArr%PV, gVertArr%DP, gVertArr%DPI, gVertArr%THS, gVertArr%FK, gVertArr%FJ, &
                  gVertArr%AU, gVertArr%A, gVertArr%H, gVertArr%G, gVertArr%SIG, &
                  gVertArr%X, gVertArr%Y, gVertArr%PRF, gVertArr%LVS

            WRITE(fileHandle, IOSTAT = errorCode, IOMSG = errorMsg) &
                  gHorzArr%allocated, &
                  gHorzArr%GQ, gHorzArr%BTA, gHorzArr%PSB, gHorzArr%PS, gHorzArr%TH, gHorzArr%WW, gHorzArr%TF, &
                  gHorzArr%FS, gHorzArr%ADS, gHorzArr%WS, gHorzArr%S, gHorzArr%FT, gHorzArr%ADT, gHorzArr%WT, &
                  gHorzArr%T, gHorzArr%SF, gHorzArr%ADO, gHorzArr%TOP, &
                  gHorzArr%DAT, &
                  gHorzArr%AKL, gHorzArr%AKLI, gHorzArr%ZS, gHorzArr%PSSAVE

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
            use ConstantsModule, only: USE_INTERACTION_COEFFICIENTS
            use ModelParamsModule, only: gUserSettings, gUserSettingsPrev, gModIntParams, gModRealParams
            use ModelFieldsModule, only: DeallocateHorizontalArrays, DeallocateTransformArrays, DeallocateVerticalArrays, &
                createHorzArr, createTransArr, createVertArr, &
                AllocateHorizontalArrays, AllocateVerticalArrays, AllocateTransformArrays, gVertArr, gHorzArr, gTransArr
            use InteractionCoefficientsModule, only: DeallocateCintArrays, AllocateCintArrays, computeInteractionCoefficients, gCintArr

            logical :: boolHorzResult, boolVertResult, boolCoeffResult, boolStatsResult
            integer :: errorCode, index

            errorCode = 0   ! No error
            boolHorzResult = .false.
            boolVertResult = .false.
            boolCoeffResult = .false.
            boolStatsResult = .false.

            !--------------------------------------------Check for bad user parameters:
            if (CheckUserSettingsBounds() .EQV. .FALSE.) then  ! To me this is easier to read than 'if (.NOT. CheckUserSettingsBounds())'
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
            if(gUserSettings%integrationMethod .NE. gUserSettingsPrev%integrationMethod) boolCoeffResult = .true.
            IF(gUserSettings%statsOption .NE. gUserSettingsPrev%statsOption) boolStatsResult = .true.
            
            if (boolHorzResult .OR. boolVertResult) then
                gArrayDims = setDimensions(gUserSettings%maxZonalWaves, gUserSettings%maxMeridWaves, gUserSettings%maxLevels)
                gCurrentDims = setDimensions(gModIntParams%KMAX2, gModIntParams%LMAX2, gModIntParams%NLEV)

                ! Handle horizontal array changes (which includes vertical dimensions):
                if (gHorzArr%allocated) errorCode = DeallocateHorizontalArrays()  ! Deallocate horizontal arrays if allocated

                errorCode = allocateHorizontalArrays()    ! Reallocate horizontal arrays
                if(errorCode .NE. 0) then
                    return
                endif
                ! But use the model settings for KMAX2, LMAX2, NLEV for actual array dimensions.
                errorCode = createHorzArr(gModIntParams%KMAX2, gModIntParams%LMAX2, gModIntParams%NLEV, gCurrentDims)
                if(errorCode .NE. 0) then
                    return
                endif
                
                ! Handle transform array changes
                if (gTransArr%allocated) errorCode = DeallocateTransformArrays()  ! Deallocate transform arrays if allocated

                errorCode = allocateTransformArrays()    ! Reallocate transform arrays
                if(errorCode .NE. 0) return

                errorCode = createTransArr(gCurrentDims)
                if(errorCode .NE. 0) return
            end if
            if ((gUserSettings%integrationMethod == USE_INTERACTION_COEFFICIENTS) .AND. & ! If we are using interaction coefficients and
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
            if (boolHorzResult .OR. boolVertResult) then
                CALL CreatePerturbation()         ! Subroutine 1ocated in this file
                CALL DFDIAB(boolVertResult)
            endif

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
        function CheckUserSettingsBounds() result(mCode)
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
                gUserSettings%maxLevels = 1
                mCode = mCode .AND. .false.
            ENDIF
            IF(gUserSettings%statsOption .LT. STATS_OPT_NONE .OR. &
                gUserSettings%statsOption .GT. STATS_OPT_VAR) THEN
                gUserSettings%statsOption = STATS_OPT_NONE
                mCode = mCode .AND. .false.
            ENDIF

        end function CheckUserSettingsBounds

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
            ENDIF
            IF(gModIntParams%LMAX2 .LT. 1) THEN
                gModIntParams%LMAX2 = 1
            ENDIF
            IF(gModIntParams%NLEV .LT. 1) THEN
                gModIntParams%NLEV = 1
            ENDIF
        end function CheckModelResolutions

        function CheckModParams() result(errorCode)
            use ConstantsModule, only: PHASE_SPACE, VORTICITY, HEIGHT, TEMPERATURE, PLOTTING_OFF, OROGRAPHY
            use ModelParamsModule, only: gModRealParams, gModIntParams

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
            IF(gModIntParams%NLEV .LT. 1) then
                gModIntParams%NLEV = 1
                errorCode = IOR(errorCode, LEVELS_CHANGED)
            end if

            !----------------------------------Checking lower bounds on JX0,JY0:
            IF(gModIntParams%JX0 .LT. 0) gModIntParams%JX0 = 0
            IF(gModIntParams%JY0 .LT. 1) gModIntParams%JY0 = 1


            !-------------------------------------------Field or variable type ranges
            I2 = PLOTTING_OFF
            ! If the type of plot is PHASE_SPACE = 5, WAVE_AMP_TIME_SERIES = 6  WAVE_PHASE_TIME_SERIES = 7, WEATHER_STN = 8
            !IF(gModIntParams%plotType .GE. PHASE_SPACE) I2 = MEAN_TEMP_TEND_GRAF     ! Not sure why we picked this field to assure the field1
            !IF(gModIntParams%plotType .EQ. WAVE_AMP_TIME_SERIES) I2 = AMPLITUDE_RATIO_GRAF
            IF(gModIntParams%field1 .LE. 0) gModIntParams%field1 = TEMPERATURE
            IF(gModIntParams%field1 .GT. I2) gModIntParams%field1 = TEMPERATURE

            !---------------------------------------------field2,field3:
            IF(gModIntParams%field2 .GT. PLOTTING_OFF) gModIntParams%field2 = HEIGHT
            IF(gModIntParams%field3 .GT. PLOTTING_OFF) gModIntParams%field3 = HEIGHT
            IF(gModIntParams%field2 .LT. VORTICITY) gModIntParams%field2 = PLOTTING_OFF
            IF(gModIntParams%field3 .LT. VORTICITY) gModIntParams%field3 = PLOTTING_OFF

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
        
            !---------------------------------------------Some idiot tests:
            IF(gModRealParams%RN .LE. 0. .OR. gModRealParams%DELT .LT. 0.) errorCode = IOR(errorCode, RN_BAD)
            IF(gModRealParams%TPP .LT. 0) gModRealParams%TPP = 0.
            IF(gModRealParams%BOT .LE. gModRealParams%TPP) errorCode = IOR(errorCode, PRESSURE_LEVELS_BAD)

        end function CheckModParams 

        !! The parameters passed will be updated if needed. The parmeter prevRN should always be non dimensional
        function CheckModelControlChanges(prevZonalWaves, prevMeridWaves, prevLevels, prevRN) result(errorCode)
            use ifwinty
            use ifwin
            use NLevQGModelGlobals, only: ghModelControlDlg
            use ConstantsModule, only: USE_INTERACTION_COEFFICIENTS, STATS_OPT_NONE
            use ModelParamsModule, only: gModIntParams, gModRealParams, gUserSettings, gScaleFactors
            use InteractionCoefficientsModule, only: gCintArr, createCintArr, computeInteractionCoefficients
            use ModelFieldsModule, only: gHorzArr, PS_pool, PSSave_pool, TOP_pool, createHorzArr, createVertArr, createTransArr
            use ArrayDimensionsModule, only: gCurrentDims
            use Fftw3HelpersModule, only: setup_fftw_plans

            integer, intent(inout) :: prevZonalWaves, prevMeridWaves, prevLevels
            real, intent(inout) :: prevRN  ! nondimensional

            integer :: errorCode, I, N
            LOGICAL :: bret, boolUpdateCint, boolHorzChange, boolVertChange, saveDimensionalState
            REAL, ALLOCATABLE :: DAT(:, :), DAT2(:, :)
            integer :: totOld, itot_old, itot_new, currentNoOfInteractions

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
                bret = MessageBox(ghModelControlDlg, "Resolution parameters exceeded the size set in the Settings Dialog. Update settings."C, &
                     "Dimensions too large"C, MB_OK)
                errorCode = 1
                return
            end if
            errorCode = CheckModParams()  ! Check all the other possible values the user may have changed.
            if (errorCode .NE. 0) return  ! Values changed by program because of bad values. Return to user for inspection.
            
            saveDimensionalState = gModRealParams%isDimensionalized
            call SetModRealParamsDimensionalState(.false.)

            if (prevZonalWaves .NE. gModIntParams%KMAX2 .OR. prevMeridWaves .NE. gModIntParams%LMAX2) boolHorzChange = .true.

            if (prevLevels .NE. gModIntParams%NLEV) boolVertChange = .true.

            ! Check if the resolutions have changed and we need to reshuffle arrays
            if (boolHorzChange .OR. boolVertChange) then
                gCurrentDims = SetDimensions(gModIntParams%KMAX2, gModIntParams%LMAX2, gModIntParams%NLEV)
                itot_old = prevMeridWaves + 2 * prevZonalWaves * prevMeridWaves
                totOld = (prevMeridWaves + 2 * prevZonalWaves * prevMeridWaves) * prevLevels
                itot_new = gModIntParams%LMAX2 + 2 * gModIntParams%KMAX2 * gModIntParams%LMAX2
                ALLOCATE(DAT(itot_new, gModIntParams%NLEV)) ! Temporary array for reshuffling
                do N = 1, min(gModIntParams%NLEV, prevLevels)
                    DAT(1, N) = gHorzArr%TF(1, N)
                    DAT(2, N) = gHorzArr%SF(1, N)
                end do

                ! Signal that we need to update the interaction coefficents if using interaction coefficients and the horizontal resolution has changed
                if (gUserSettings%integrationMethod == USE_INTERACTION_COEFFICIENTS .AND. boolHorzChange) boolUpdateCint = .true.

                errorCode = createHorzArr(gModIntParams%KMAX2, gModIntParams%LMAX2, gModIntParams%NLEV, gCurrentDims)
                errorCode = createTransArr(gCurrentDims)
                gHorzArr%TF = 0.0
                gHorzArr%SF = 0.0
                do N = 1, min(gModIntParams%NLEV, prevLevels)
                    gHorzArr%TF(1, N) = DAT(1, N)
                    gHorzArr%SF(1, N) = DAT(2, N)
                end do
                call setup_fftw_plans(gCurrentDims%IXMAX, gCurrentDims%IYMAX)
                call ReshuffleArrays(gHorzArr%PS, DAT, PS_pool, prevZonalWaves, prevMeridWaves, prevLevels, gModIntParams%KMAX2, gModIntParams%LMAX2, gModIntParams%NLEV, totOld)
                
                ! Cant reshuffle TF and SF arrays as they will get overwritten by the first call when the resolution increases that much.
                CALL ReshuffleArrays(gHorzArr%PSSAVE, DAT, PSSave_pool, prevZonalWaves, prevMeridWaves, prevLevels, gModIntParams%KMAX2, gModIntParams%LMAX2, gModIntParams%NLEV, totOld)
                CALL ReshuffleArrays(gHorzArr%TOP, DAT, TOP_pool, prevZonalWaves, prevMeridWaves, 1, gModIntParams%KMAX2, gModIntParams%LMAX2, 1, itot_old)
                
                ! Zero out the dissipative terms in case we are going inviscid. All other field tendencies are calculated every time step
                ! and their initial values are inconsequential. These will not get reset since the dissipative routines are skipped.
                gHorzArr%S = 0.0
                gHorzArr%T = 0.0
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
                ! Now that we have checked prevRn for a change, update with the new value
                prevRN = gModRealParams%RN
                ! Horizontal resolution has changed, need to recompute cint arrays.
                if (boolUpdateCint) then
                    ! Compute number of coefficients only. flag = 0 signals to do that. Need to create the gCintArr array shapes.
                    
                    currentNoOfInteractions = computeInteractionCoefficients(gModRealParams%RN, gModIntParams%KMAX2, gModIntParams%LMAX2, 0)
                    ! The way we have it now is that as long as KMAX2 and LMAX2 are within bounds, we are under the limit of interaction coefficients
                    ! so the method will never return -1 indicating a switch to a transform method.
                    errorCode = createCintArr(gModIntParams%KMAX2, gModIntParams%LMAX2, currentNoOfInteractions)
                endif
                ! Now compute the interaction coefficients. flag > 0 indicates to do that versus just count them
                if (gUserSettings%integrationMethod == USE_INTERACTION_COEFFICIENTS) &
                    gCintArr%numOfInteractionCoeffs = computeInteractionCoefficients(gModRealParams%RN, gModIntParams%KMAX2, gModIntParams%LMAX2, 1)
            endif
            prevZonalWaves = gModIntParams%KMAX2
            prevMeridWaves = gModIntParams%LMAX2
            prevLevels = gModIntParams%NLEV

            call SetModRealParamsDimensionalState(saveDimensionalState) ! Restore the dimensional state to what it was before this function.

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
        !! @param kmax_old      Old maximum number of zonal waves
        !! @param lmax_old      Old maximum number of meridional waves
        !! @param levels_old    Number of vertical levels in old resolution
        !! @param kmax_new      New maximum number of zonal waves
        !! @param lmax_new      New maximum number of meridional waves
        !! @param levels_new    Number of vertical levels in new resolution
        !! @param total         Total size of the PS_pool array
        SUBROUTINE ReshuffleArrays(ps, dat, PS_pool, kmax_old, lmax_old, levels_old, &
                                   kmax_new, lmax_new, levels_new, totOld)
            IMPLICIT NONE
    
            ! Dimensions passed as arguments
            INTEGER, INTENT(IN) :: kmax_old, lmax_old, levels_old
            INTEGER, INTENT(IN) :: kmax_new, lmax_new, levels_new, totOld
    
            ! Flat 2D array declarations to match modern Fortran shape rules
            REAL, INTENT(INOUT) :: ps(lmax_new + 2*kmax_new*lmax_new, levels_new)
            REAL, INTENT(INOUT) :: dat(lmax_new + 2*kmax_new*lmax_new, levels_new)
            REAL, INTENT(IN)    :: PS_pool(totOld)
    
            ! Local variables
            INTEGER :: j, K, L
            INTEGER :: itot_old, itot_new
            INTEGER :: old_level_offset
            INTEGER :: index_old, index_new
            INTEGER :: max_j, max_k, max_l
    
            ! Calculate the exact horizontal dimension sizes
            itot_old = lmax_old + 2 * kmax_old * lmax_old
            itot_new = lmax_new + 2 * kmax_new * lmax_new
    
            ! 1. Initialize the entire target array to zero.
            ! This automatically handles zero-filling if the new resolution is larger.
            DAT = 0.0
    
            ! 2. Determine safe overlapping boundaries to avoid out-of-bounds reads
            max_j = MIN(levels_old, levels_new)
            max_k = MIN(kmax_old, kmax_new)
            max_l = MIN(lmax_old, lmax_new)
    
            ! 3. Loop through overlapping vertical levels
            DO j = 1, max_j
        
                ! Pointer to the start of the current level in the flat 1D pool
                old_level_offset = (j - 1) * itot_old
        
                ! ------------------------------------------------------------------
                ! BLOCK 1: Zonal Components (K=0)
                ! ------------------------------------------------------------------
                DO L = 1, max_l
                    DAT(L, j) = PS_pool(old_level_offset + L)
                END DO

                ! ------------------------------------------------------------------
                ! BLOCK 2: Zonal Waves (K > 0)
                ! ------------------------------------------------------------------
                DO K = 1, max_k
                    DO L = 1, max_l

                        ! Relative 0-based offset past the zonal block for the old layout
                        index_old = lmax_old &
                                    + (K - 1) * (lmax_old * 2) &
                                    + (L - 1) * 2
                
                        ! Relative 0-based offset past the zonal block for the new layout
                        index_new = lmax_new &
                                    + (K - 1) * (lmax_new * 2) &
                                    + (L - 1) * 2

                        ! Copy Cosine entry (+1 shifts to 1-based Fortran array indexing)
                        DAT(index_new + 1, j) = PS_pool(old_level_offset + index_old + 1)

                        ! Copy Sine entry
                        DAT(index_new + 2, j) = PS_pool(old_level_offset + index_old + 2)

                    END DO
                END DO

            END DO
            ! 4. Safe 2D-to-2D array assignment
            DO j = 1, max_j
                PS(:, j) = DAT(:, j)
            END DO

        END SUBROUTINE ReshuffleArrays
                           
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
            !integer, parameter :: gridPoints(*) = [4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 18, 20, 21, 22, 24, 25, 27, 28, 30, &
            !                          32, 33, 35, 36, 40, 42, 44, 45, 48, 49, 50, 54, 55, 56, 60, 63, 64, 66, 70, 72, 75, 77, &
            !                          80, 81, 84, 88, 90, 96, 99, 100, 105, 108, 110, 112, 120, 121, 125, 126, 128, 132, 135, &
            !                          140, 144, 147, 150, 154, 160, 162, 165, 168, 175, 176, 180, 189, 192, 198, 200, 210, 216, &
            !                          220, 224, 225, 231, 240, 243, 245, 250, 252, 256, 264, 270, 275, 280, 288, 297, 300, 308, &
            !                          315, 320, 324, 330, 336, 343, 350, 360, 363, 375, 378, 384, 385, 396, 400, 405, 420, 432, &
            !                          440, 441, 448, 450, 462, 480, 486, 495, 500, 504, 512]
            
            integer, parameter :: gridPoints(*) = [ &
                            32, 36, 40, 42, 44, 48, 50, 54, 56, 60, 64, 66, 70, 72, &
                            80, 84, 88, 90, 96, 100, 108, 110, 112, 120, 126, 128, 132, &
                            140, 144, 150, 154, 160, 162, 168, 176, 180, 192, 198, 200, 210, 216, &
                            220, 224, 240, 250, 252, 256, 264, 270, 280, 288, 300, 308, &
                            320, 324, 330, 336, 350, 360, 378, 384, 396, 400, 420, &
                            440, 448, 450, 462, 480, 486, 500, 504, 512]

            call ZeroArrayDims(arraySizes)  ! This subroutine initializes all dimensions in gArrayDims to zero.
            !------------------------------------------------------------ Set up FFTs:
            J = 3 * zonalWaves + 1
            DO I = 1, SIZE(gridPoints)
                IF(J .LE. gridPoints(I)) THEN
                    arraySizes%IXMAX = gridPoints(I)
                    EXIT
                ENDIF
            END DO
            J = 6 * meridWaves + 1
            DO I = 1, SIZE(gridPoints)
                IF(J .LE. gridPoints(I)) THEN
                    arraySizes%IYMAX = gridPoints(I)
                    EXIT
                ENDIF
            END DO
            
            ! For vertical arrays
            arraySizes%LVS = levels * 2 + 1
            !------------------------------------For Graphics output of maps:
            arraySizes%KX = arraySizes%IXMAX + 2
            arraySizes%LY = arraySizes%IYMAX + 2

            arraySizes%ITOT = meridWaves + 2 * zonalWaves * meridWaves
            arraySizes%IYH = arraySizes%IYMAX / 2 + 1

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
            REAL :: R, DELP, THS1, EX
        !-----------------------------------------------:
        ! gVertArr%tropopauseLevel=Pressure level of tropopause
        ! gVertArr%THS(1) = Fixed Vorticity level 1 potential temperature
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
            gVertArr%tropopauseLevel = 250.         ! Height of tropopause
            gVertArr%THS(1) = 285.0                 ! Potential temperature of vorticity level 1 (lowest layer). Note that it is NOT at an interface
                                                    ! The THS field is to compute static stabilty at the interfaces and to use in the Raisanen sheme
            THS1 = .14          ! Increase in potential temperature per 100 mb
        !----------------------------------Potential temperature at tropopause
            EX = gVertArr%THS(1) + (gVertArr%PV(1) - gVertArr%tropopauseLevel) * THS1
            DO N = 1, gModIntParams%NLEV
                IF(gVertArr%PV(N) .GT. gVertArr%tropopauseLevel) THEN
                    gVertArr%THS(N) = gVertArr%THS(1) + THS1 * (gVertArr%PV(1) - gVertArr%PV(N))
                    gVertArr%FK(1 : gModIntParams%NLEV) = 0.  ! Set Ekman friction to 0
                ELSE
        !-----------------------------------Stratospheric isothermal profile in Temp.
                    gVertArr%THS(N) = EX * (gVertArr%tropopauseLevel / gVertArr%PV(N))**KAPPA
                    gVertArr%FK(1 : gModIntParams%NLEV) = 50.     ! Set ekman friction to 50 days for stratosphere
                ENDIF
            END DO
            !gVertArr%FK(1 : gModIntParams%NLEV) = 20.     ! Set ekman friction to 20 days for every level
        !========================Thermal mixing exp decay (Convective mixing of temperature):
            EX = 6.
            DO N = 1, gModIntParams%NLEV - 1
                R = EXP(EX * 0.5 * (1. - gVertArr%PV(N) / gVertArr%PV(1)))
        !------------------------------Stop convection at Tropopause
                IF(gVertArr%PV(N) .GT. gVertArr%tropopauseLevel) THEN
                    gVertArr%G(N) = gScaleFactors%dissipation / R
                ELSE
                    gVertArr%G(N) = gScaleFactors%dissipation / 20. ! 20 day time scale
                ENDIF
            END DO
            !  Vort mix=Constant=20 days:
            gVertArr%FJ(1:gModIntParams%NLEV) = gScaleFactors%dissipation / 20.0  ! This gives the same value for all N of 20 days

        !====================================================Compute PP,DPI,A,AU,SIG
            CALL ComputeVerticalProfiles()
        !------------------------------------------Compute Petri TF and H profiles:
            CALL RaisanenRadiationProfile()
            !IF(gModIntParams%NLEV .NE. 1) gVertArr%G(gModIntParams%NLEV - 1) = 0.
        !==========================================Set surface heating and Ekman fric:
            gVertArr%H(1) = gScaleFactors%dissipation / 2.5     ! 2.5 day time scale  
            ! This was set in the RaisanenRadiationProfile method. The conversion between a dimensional dissipation value in 1/days
            ! is dimensional_FK = 1.0_wp / (secondsPerDay * nondimensional_FK * scaleCoriolis)
            ! gScaleFactors%dissipation = 1.0_wp / (secondsPerDay * scaleCoriolis)
            ! to increase the damping. Not sure why it is divided by 8. Probably because
            ! the scale factor gives 0.1157 / day or about 2.7 hr damping.
            
            gVertArr%FK(1) = gScaleFactors%dissipation / 1.0   ! Gives a 1 day dissipation 
        END subroutine DFDIAB

        !*****************************************Place perturbation in flow:
        SUBROUTINE CreatePerturbation()
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
        END SUBROUTINE CreatePerturbation

        !***************************************************** SetDefaults:
        !> THis function sets default values to gModRealParams, gModIntParams, gContourIntervals,
        !> gModPlotParams, and gModSensParams when there is no save file already containing these
        !> fields.
        SUBROUTINE SetDefaults()
            use ConstantsModule, only: STATS_OPT_NONE, PI, VORTICITY, HEIGHT, TEMPERATURE, VERTICAL_WIND, TEMP_FORCING, &
                OROG_HGT_TEND, HGT_FORCING, PLOTTING_OFF, TOT_HGT_TEND_GRAF, TOT_TEMP_TEND_GRAF, MEAN_HGT_TEND_GRAF, MEAN_TEMP_TEND_GRAF, &
                HGT_WAVE_SPEED_GRAF, WEATHER_MAP, STANDARD_NLEVEL, OROGRAPHY, HGT_TEMP_PHASE_DIFF, GROWTH_RATE_GRAF, &
                AMPLITUDE_RATIO_GRAF, ZLEVEL_1, ZLEVEL_2, ZLEVEL_3, USE_TRANSFORM
            use ModelParamsModule, only: gModIntParams, gModRealParams, gUserSettings, gUserSettingsPrev, gScaleFactors, &
                gContourIntervals, gModSensParams, gPlotScales
            use NLevQGModelGlobals, only: LogMessage

            implicit none
            
            character(64) :: valueStr

            INTEGER I, I1, J, J1
            
            gUserSettings%maxZonalWaves = 10
            gUserSettings%maxMeridWaves = 10
            gUserSettings%maxLevels = 32
            gUserSettings%integrationMethod = USE_TRANSFORM
            gUserSettings%statsOption = STATS_OPT_NONE

            gUserSettingsPrev = gUserSettings

        !------------------------------------------------------------- Default Values:
        ! These fields are in type(ModelParams) gModRealParams in the ModelParamsModule
        !  BETA is the beta parameter
        !  RN is the horizontal aspect ratio (channel zonal length / channel width)
        !  DELT is the length of the time step
        !  field1Level is the height of the model to be displayed
        !  hovLat is the latitude of the Hovmoller and vertical structure plot
        !  lonPoint,latPoint is the position of the selected point in the model
        !  BOT,TPP is the pressure range to be displayed in X sections
        !  CONV is a convergence test for equilibrated solutions
        !  SPINUP is a spinup time toss amount for SENSIV runs
        !  gVertArr%tropopauseLevel is the height of the tropopause in mb
        !  AMPT,AMPS,ANGS,EFOLD are prescribed "bomb" disturbance features
        !
        ! These parameters are dimensionalized and then non dimensionalized
            ! Dimensionalize for input
            ! IF(RN.GT.0.)RN=gScaleFactors%longestWave/RN
            ! DELT=DELT*gScaleFactors%timeScale
            ! BETA=4.*BETA
            ! lonPoint=lonPoint*RAD_TO_DEGREES
            ! latPoint=latPoint*RAD_TO_DEGREES
            ! Non dimensionalize
            ! RN=gScaleFactors%longestWave/RN
            !--------------------------------Recompute interactions if needed:
            ! IF(RNP.NE.RN)interactCoefsComputed = .false.
            ! DELT=DELT/gScaleFactors%timeScale
            ! BETA=BETA/4.
            ! lonPoint=lonPoint/RAD_TO_DEGREES
            ! latPoint=latPoint/RAD_TO_DEGREES
        !-------------------------------------------------Set user default parameters:
            gModRealParams%isDimensionalized = .FALSE.
            gModRealParams%RN = gScaleFactors%longestWave/24000.
            gModRealParams%BETA = .25
            gModRealParams%DELT = 60./gScaleFactors%timeScale
            gModRealParams%field1Level = 900.
            gModRealParams%field2Level = 950.
            gModRealParams%field3Level = 500.
            gModRealParams%hovLat = PI/4.
            gModRealParams%hovLon = 0.
            gModRealParams%lonPoint = 0.
            gModRealParams%latPoint = PI/4.
            gModRealParams%BOT = 1000.
            gModRealParams%TPP = 0.
            gModRealParams%CONV = 0.
            gModRealParams%SPINUP = 160.
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
            
 
            ! Vorticity in 10**-5/s
            ! Height in meters
            ! Temperature in Centigrade
            ! Vertical wind in mb/hr or mm/sec
            ! Temperature driving (forcing) - Radiative relaxation temperature in Centigrade
            ! Total Height tendency in meters/day
            ! height tendencies in m/day from advection plus beta
            ! height tendencies in m/day from omega (dp/dt)
            ! height tendencies in m/day from diabatic effects 
            ! total temperture tendencies in c/day
            ! temperature tendencies in c/day from advection
            ! temperature tendencies in c/day from omega
            ! temperature tendencies in c/day from diabatic effects
            ! mean streamfunction (height) forcing in m
            ! height tendencies from ororgraphy in m/day'/
            ! bottom orography in meters
            ! Plotting off

            ! These give the contours specified when the variables are dimensionalized by the corresponding gPlotScales(j) element
            ! Thus if I have PS(I, n) * gPlotScales(HEIGHT) this will give me th height in meters. Dividing by gContourIntervals%Contours(HEIGHT)
            ! will produce the value 0.0 to 1.0 for values between 0 and 60 meters and 1.0 to 2.0 for values between 60 and 120 meters.
            ! From this we can assign integer color values for those level ranges, for example we will use color 0 for 0.0 to < 1.0 and
            ! color 1 for 1.0 to < 2.0, etc. In the old days of 16-color VGA this was a direct assignment. But in todays 32-bit RGB colors
            ! we need to make a color map to map the value 0 to a specific color. We have such a map RAM_COLORS(18) in the PlotModule
            gContourIntervals%Contours(VORTICITY) = 1.0
            gContourIntervals%Contours(HEIGHT) = 60.0         ! Gives 60m contours for L=5000km/PI
            gContourIntervals%Contours(TEMPERATURE) = 5.0
            gContourIntervals%Contours(VERTICAL_WIND) = 5.0
            gContourIntervals%Contours(TEMP_FORCING) = 5.0
            DO I = 1, 4
               gContourIntervals%Contours(I + 5) = 60.0
               gContourIntervals%Contours(I + 9) = 5.0
            END DO
            gContourIntervals%Contours(OROG_HGT_TEND) = 60.0 
            gContourIntervals%Contours(OROGRAPHY) = 100.0
            gContourIntervals%Contours(HGT_FORCING) = 60.0
            gContourIntervals%Contours(PLOTTING_OFF) = 1.0
        !------------------------------Graphs only forces:
            gContourIntervals%Contours(TOT_HGT_TEND_GRAF) = 60.0 / gPlotScales(TOT_HGT_TEND_GRAF)
            gContourIntervals%Contours(TOT_TEMP_TEND_GRAF) = 5.0 / gPlotScales(TOT_TEMP_TEND_GRAF)
            gContourIntervals%Contours(MEAN_HGT_TEND_GRAF) = 60.0 / gPlotScales(MEAN_HGT_TEND_GRAF)
            gContourIntervals%Contours(MEAN_TEMP_TEND_GRAF) = 5.0 / gPlotScales(MEAN_TEMP_TEND_GRAF)
        !------------------------------Phase speed and phase difference
            gContourIntervals%Contours(HGT_WAVE_SPEED_GRAF) = 50.0 / gPlotScales(HGT_WAVE_SPEED_GRAF)
            gContourIntervals%Contours(HGT_TEMP_PHASE_DIFF) = 180.0
            gContourIntervals%Contours(GROWTH_RATE_GRAF) = 1.0
            gContourIntervals%Contours(AMPLITUDE_RATIO_GRAF) = 1.0
            gContourIntervals%Contours(ZLEVEL_1) = 7.0     ! ZLEV of old
            gContourIntervals%Contours(ZLEVEL_2) = 0.0     ! ZLV2 of old
            gContourIntervals%Contours(ZLEVEL_3) = 0.0     ! ZLV3 of old
            
            gContourIntervals%Ranges = 10.0 * gContourIntervals%Contours
        
        !------------------------------------------------------------ The ICN values.
        !  KMAX2 the number of zonal waves
        !  LMAX2 the number of meridional waves (and zonal components)
        !  NLEV the number of vertical levels
        !  ICYC the number of cycles of time steps ISTEP
        !  ISTEP the number of time steps in a cycle
        !  outputInterval is the number of time steps between outputs
        !  plotType is the type of plot
        !  field1,field2 the quantity to plot to the graphic screen
        !  zonalWavenumber and meridWavenumber are the wave#s of quant to plot to phase space/time series diagram
        !  sinZonal,sinMerid are the number of maps in the x, y directions
        !  modelType: tells what type of model to be run. One of STANDARD_NLEVEL, BAROCLINIC_STRUCTS, ZONAL_MEAN_FLOWS
        !  JX1,JY1 are the number of waves to retain in the 'at a point' plots
        !-------------------------------------------------------------ICN(31) values:
        !  maxZonalWaves,maxMeridWaves: are the 'frame' maximum horizontal resolutions
        !  maxLevels: is the maximum vertical resolution
        !  statsOption: is the statistics option
        !  integrationMethod: indicates interaction/transform model
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
            gModIntParams%ISTEP = 100000
            gModIntParams%outputInterval = 1
            gModIntParams%plotType = WEATHER_MAP
            gModIntParams%field1 = TEMPERATURE
            gModIntParams%field2 = HEIGHT
            gModIntParams%field3 = PLOTTING_OFF
            gModIntParams%phaseSpaceX = gModIntParams%LMAX2 + 1
            gModIntParams%phaseSpaceY = gModIntParams%LMAX2 + 2
            gModIntParams%LEVP = 2      ! Lowest streamfunction layer.
            gModIntParams%modelType = STANDARD_NLEVEL
            gModIntParams%JX0 = 0
            gModIntParams%JY0 = 1
            gModIntParams%JX1 = gModIntParams%KMAX2
            gModIntParams%JY1 = gModIntParams%LMAX2
        !----------------------------------------------Stats variables:
            gModIntParams%IV1 = gModIntParams%field1
            gModIntParams%IV2 = gModIntParams%field2
            gModIntParams%IPRM1 = 1
            gModIntParams%IPRM2 = 8
            gModIntParams%ITER1 = 1
            gModIntParams%ITER2 = 1
            gModIntParams%KMODE = 0
            gModIntParams%LMODE = 1
            gModIntParams%IADD = 0

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
                gContourIntervals%VSEN(I) = 1.
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
                gModIntParams%MP(I) = gModIntParams%field1
                !-----------------------------Set graphs (plus reset one map!)
                gModIntParams%MP(I + 4) = gModIntParams%field1
                gModIntParams%MP(I + 9) = gModIntParams%field2
                gModIntParams%MP(I + 14) = gModIntParams%field3
            END DO
        END SUBROUTINE SetDefaults

        !> Subroutine to dimensionalize or nondimensionalize the gModRealParams
        !> Only three of the parameters are affected. DELT, BETA, RN
        !> Two others lonPoint, latPoint go to degrees in 'dimensional' form, otherwise they are in radians.
        !! @Param setDimensional: logical indicating whether to set the parameters to dimensional or non-dimensional
        !!        form. If true, set to dimensional. If false, set to non-dimensional.
        subroutine SetModRealParamsDimensionalState(setDimensional)
            use ConstantsModule, only: RAD_TO_DEGREES
            use ModelParamsModule, only: gModRealParams, gScaleFactors

            logical, intent(in) :: setDimensional

            if (setDimensional) then
                ! If already dimensionalized dont do it again!
                if (gModRealParams%isDimensionalized) return
                IF(gModRealParams%RN .GT. 0.)gModRealParams%RN = gScaleFactors%longestWave/gModRealParams%RN
                gModRealParams%DELT = gModRealParams%DELT * gScaleFactors%timeScale
                gModRealParams%BETA = 4. * gModRealParams%BETA
                gModRealParams%lonPoint = gModRealParams%lonPoint * RAD_TO_DEGREES ! Convert to degrees for display
                gModRealParams%latPoint = gModRealParams%latPoint * RAD_TO_DEGREES ! Convert to degrees for display
                gModRealParams%hovLat = gModRealParams%hovLat * RAD_TO_DEGREES ! Convert to degrees for display
                gModRealParams%hovLon = gModRealParams%hovLon * RAD_TO_DEGREES ! Convert to degrees for display
                gModRealParams%isDimensionalized = .TRUE. ! Indicate dimensionalized
            else
                ! If already nondimensionalized dont do it again!
                if (.NOT. gModRealParams%isDimensionalized) return
                IF(gModRealParams%RN .GT. 0.) gModRealParams%RN = gScaleFactors%longestWave/gModRealParams%RN
                !--------------------------------Recompute interactions if needed:
                ! IF(RNP.NE.RN)interactCoefsComputed = .false.
                gModRealParams%DELT = gModRealParams%DELT / gScaleFactors%timeScale
                gModRealParams%BETA = gModRealParams%BETA / 4.
                gModRealParams%lonPoint = gModRealParams%lonPoint / RAD_TO_DEGREES ! Restore to radians
                gModRealParams%latPoint = gModRealParams%latPoint / RAD_TO_DEGREES ! Restore to radians
                gModRealParams%hovLat = gModRealParams%hovLat / RAD_TO_DEGREES ! Restore to radians
                gModRealParams%hovLon = gModRealParams%hovLon / RAD_TO_DEGREES ! Restore to radians
                gModRealParams%isDimensionalized = .FALSE. ! Indicate non-dimensionalized
            endif
        end subroutine SetModRealParamsDimensionalState

        subroutine DimensionalizeVerticalParams()
            use ModelFieldsModule, only: gVertArr
            use ModelParamsModule, only: gModIntParams, gScaleFactors

            implicit none

            integer :: N

            if (gVertArr%isDimensionalized) return
            ! Dimensionalize the vertical parameters in gVertArr
            do N = 1, gModIntParams%NLEV
                if (gVertArr%FK(N) > 0.0) gVertArr%FK(N) = gScaleFactors%dissipation / gVertArr%FK(N) ! Convert to dimensional FK 1/days
                if (gVertArr%FJ(N) > 0.0) gVertArr%FJ(N) = gScaleFactors%dissipation / gVertArr%FJ(N) ! Convert to dimensional FJ
                if (N .GT. gModIntParams%NLEV - 1) cycle
                ! Non dimensional form was computed from K/mb. For display we make it per 100 mb
                gVertArr%SIG(N) = 100. * gVertArr%SIG(N) * gScaleFactors%temperatureScale /(gVertArr%A(N) * gVertArr%PP(1)) ! Convert to dimensional SIG K/100 mb
                if (gVertArr%G(N) > 0.0) gVertArr%G(N) = gScaleFactors%dissipation / gVertArr%G(N) ! Convert to dimensional G
                if (gVertArr%H(N) > 0.0) gVertArr%H(N) = gScaleFactors%dissipation / gVertArr%H(N) ! Convert to dimensional H
            end do
            gVertArr%isDimensionalized = .true. ! Indicate dimensionalized

        end subroutine DimensionalizeVerticalParams
        
        subroutine NonDimensionalizeVerticalParams()
            use ModelFieldsModule, only: gVertArr
            use ModelParamsModule, only: gModIntParams, gScaleFactors

            implicit none

            integer :: N

            if (.NOT. gVertArr%isDimensionalized) return
            ! Dimensionalize the vertical parameters in gVertArr
            do N = 1, gModIntParams%NLEV
                if (gVertArr%FK(N) > 0.0) gVertArr%FK(N) = gScaleFactors%dissipation / gVertArr%FK(N) ! Convert to non dimensional FK
                if (gVertArr%FJ(N) > 0.0) gVertArr%FJ(N) = gScaleFactors%dissipation / gVertArr%FJ(N) ! Convert to non dimensional FJ
                if (N .GT. gModIntParams%NLEV - 1) cycle
                if (gVertArr%G(N) > 0.0) gVertArr%G(N) = gScaleFactors%dissipation / gVertArr%G(N) ! Convert to non dimensional G
                if (gVertArr%H(N) > 0.0) gVertArr%H(N) = gScaleFactors%dissipation / gVertArr%H(N) ! Convert to non dimensional H
                ! Originally calculated from K/mb and made non dimensional, but we display it as K /100mb so we need to divide by 100.
                gVertArr%SIG(N) = gVertArr%SIG(N) * gVertArr%A(N) * gVertArr%PP(1) / (100.0 * gScaleFactors%temperatureScale) ! Convert to non dimensional SIG
            end do
            gVertArr%isDimensionalized = .false. ! Indicate nondimensionalized

        end subroutine NonDimensionalizeVerticalParams
        
        subroutine DimensionalizeHorizontalParams()
            use ConstantsModule, only: GRAVITY, GAS_CONSTANT_R, HEIGHT, TEMPERATURE, TOT_HGT_TEND, ADV_BETA_HGT_TEND, HGT_FORCING
            use ModelFieldsModule, only: gHorzArr, gVertArr
            use ModelParamsModule, only: gModIntParams, gPlotScales
            use ArrayDimensionsModule, only: gCurrentDims

            implicit none

            integer :: Index, N
            real :: tempScale, topScale
            
            if (gHorzArr%isDimensionalized) return

            ! Do temperature variables first. There is one less interface than levels
            do N = 1, gModIntParams%NLEV - 1
                tempScale = gPlotScales(TEMPERATURE) / gVertArr%A(N) ! The scaling factor for the temperature at level N
                do Index = 1, gCurrentDims%ITOT
                    gHorzArr%TH(Index, N) = tempScale * gHorzArr%TH(Index, N)
                    gHorzArr%TF(Index, N) = tempScale * gHorzArr%TF(Index, N)
                end do
            end do
            gHorzArr%PS = gPlotScales(HEIGHT)  * gHorzArr%PS ! Convert to dimensional heights all at once
            gHorzArr%FS = gPlotScales(TOT_HGT_TEND) * gHorzArr%FS 
            gHorzArr%ADS = gPlotScales(ADV_BETA_HGT_TEND) * gHorzArr%ADS 
            gHorzArr%SF = gPlotScales(HGT_FORCING) * gHorzArr%SF ! Convert to dimensional heights

            ! The height of a 'standard' pressure level PP(n) is given by z = (R*T/g)ln(PP(1)/PP(N))
            ! T is the virtual temperature of the column in K which is given by (THS(1) + THS(2))/2
            if (gModIntParams%NLEV > 1) then
                topScale = GAS_CONSTANT_R * (gVertArr%THS(1) + gVertArr%THS(2)) * LOG(gVertArr%PP(1) / gVertArr%PP(2) )/ (2.0 * GRAVITY)
            else
                topScale = 500 * 11 ! 11 meters / mb with a 500 mb barotopic layer
            endif
            gHorzArr%TOP = topScale * gHorzArr%TOP ! Convert to dimensional heights
            gHorzArr%isDimensionalized = .true.
        end subroutine DimensionalizeHorizontalParams

        subroutine NonDimensionalizeHorizontalParams()
            use ConstantsModule, only: GRAVITY, GAS_CONSTANT_R, HEIGHT, TEMPERATURE, TOT_HGT_TEND, ADV_BETA_HGT_TEND, HGT_FORCING
            use ModelFieldsModule, only: gHorzArr, gVertArr
            use ModelParamsModule, only: gModIntParams, gPlotScales
            use ArrayDimensionsModule, only: gCurrentDims

            implicit none

            integer :: Index, N
            real :: tempScale, heightsScale, topScale

            if (.NOT. gHorzArr%isDimensionalized) return

            ! Do temperature variables first. There is one less interface than levels
            do N = 1, gModIntParams%NLEV - 1
                tempScale = gVertArr%A(N) / gPlotScales(TEMPERATURE) ! The scaling factor for the temperature at level N
                do Index = 1, gCurrentDims%ITOT
                    gHorzArr%TH(Index, N) = tempScale * gHorzArr%TH(Index, N)
                    gHorzArr%TF(Index, N) = tempScale * gHorzArr%TF(Index, N)
                end do
            end do
            gHorzArr%FS = gHorzArr%FS / gPlotScales(TOT_HGT_TEND)
            gHorzArr%ADS = gHorzArr%ADS / gPlotScales(ADV_BETA_HGT_TEND)
            gHorzArr%PS = gHorzArr%PS / gPlotScales(HEIGHT) ! Convert to nondimensional 
            gHorzArr%SF = gHorzArr%SF / gPlotScales(HGT_FORCING)! Convert to nondimensional 

            if (gModIntParams%NLEV > 1) then
                topScale = (2.0 * GRAVITY) /(GAS_CONSTANT_R * (gVertArr%THS(1) + gVertArr%THS(2)) * LOG(gVertArr%PP(1) / gVertArr%PP(2)))
            else 
                topScale = 1.0 / (500 * 11) ! 11 meters / mb with a 500 mb barotopic layer
            endif
            gHorzArr%TOP = topScale * gHorzArr%TOP ! Convert to nondimensional
            gHorzArr%isDimensionalized = .false.
        end subroutine NonDimensionalizeHorizontalParams

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
            use ConstantsModule, only: OROGRAPHY, CP, CORIOLIS, KAPPA, GAS_CONSTANT_R, GRAVITY
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
            
            if (gModIntParams%NLEV == 1) then
                gPlotScales(OROGRAPHY) = (500) * 11.    ! Set scale height for orography as 11m/mb 500 mb barotropic layer
                return
            endif
            !-----------------------------Compute PP by setting to the middle of PV's (and LVS which is just for plotting)
            gVertArr%LVS(1) = gVertArr%PP(1)
            DO N = 2, gModIntParams%NLEV
                gVertArr%PP(N) = (gVertArr%PV(N - 1) + gVertArr%PV(N)) / 2.
                ! Compute LVS which will be twice a long and have both interfaces and level centers including PP(1) and PP(NLEV+1)
                gVertArr%LVS((N - 1) * 2) = gVertArr%PV(N - 1)  ! Layer center
                gVertArr%LVS(N * 2 - 1) = gVertArr%PP(N)        ! Interface
            END DO
            gVertArr%LVS(gModIntParams%NLEV * 2) = gVertArr%PV(gModIntParams%NLEV)
            gVertArr%LVS(gModIntParams%NLEV * 2 + 1) = gVertArr%PP(gModIntParams%NLEV + 1)
            !-----------------------------Compute the ratio PP(1)/DP(n)
            DO N = 1, gModIntParams%NLEV
                gVertArr%DP(N) = gVertArr%PP(N) - gVertArr%PP(N + 1)
                gVertArr%DPI(N) = gVertArr%PP(1) / gVertArr%DP(N)
            END DO
            !--------------------------------Set scale height for orography
            gPlotScales(OROGRAPHY) = GAS_CONSTANT_R * (gVertArr%THS(1) + gVertArr%THS(2)) * LOG(gVertArr%PP(1) / gVertArr%PP(2) )/ (2.0 * GRAVITY)
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
            gVertArr%A(gModIntParams%NLEV) = CP * gVertArr%PV(gModIntParams%NLEV)**KAPPA / (CORIOLIS * gVertArr%PP(1)**KAPPA)

!--SIGMA:------------------------------Compute sigma profile AND nondimensionalize:

            R = gVertArr%PP(1) / gScaleFactors%temperatureScale
            ! gScaleFactors%temperatureScale = CHANNEL_WIDTH * CHANNEL_WIDTH * CORIOLIS = 2.6121725E+08 m*m/s
            ! R = mb s / m**2
            ! gVertArr%A(N) has units of m**2/(s K) 
            ! X = R * A(N) has units of mb*s*m**2/(s*K*m**2) = mb/K
            ! SIG has units K /mb
            DO N = 1, gModIntParams%NLEV - 1
                X = R * gVertArr%A(N)
                gVertArr%SIG(N) = (gVertArr%THS(N + 1) - gVertArr%THS(N)) / (gVertArr%PV(N) - gVertArr%PV(N + 1))
                gVertArr%SIG(N) = gVertArr%SIG(N) * X
            END DO

        END subroutine ComputeVerticalProfiles
        
        !subroutine computePlotTempScale()
        !    use ModelFieldsModule, only: gVertArr
        !    use ConstantsModule, only: OROGRAPHY, CP, CORIOLIS, KAPPA, GAS_CONSTANT_R, GRAVITY
        !    use ModelParamsModule, only: gModIntParams, gScaleFactors, gPlotScales
        !    
        !    implicit none
        !    ! scale factor at level gVertArr%LVS(n) =  CP*[LVS(n-1)**KAPPA - LVS(n+1)**KAPPA]/(CHANNEL_WIDTH * CHANNEL_WIDTH * CORIOLIS * LVS(1)**KAPPA)
        !    
        !    gVertArr%APlot(1) = 
        !
        !end subroutine computePlotTempScale

        !********************************************"Radiation" Routine:
        SUBROUTINE RaisanenRadiationProfile()
            use ConstantsModule, only: KAPPA, SQRT2, TEMPERATURE
            use ModelFieldsModule, only: gVertArr, gHorzArr
            use ModelParamsModule, only: gModIntParams, gScaleFactors, gPlotScales

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
                if (N .LT. gModIntParams%NLEV - 1) then
                    gVertArr%H(N) = .25 * (gVertArr%THS(N) +  gVertArr%THS(N + 1)) / (2.0 * Y * R) ! The .25 is the linearization of the T**4 boltzmann radiation I believe.
                else
                    gVertArr%H(N) = .25 * gVertArr%THS(N) / (Y * R)! The .25 is the linearization of the T**4 boltzmann radiation I believe.
                end if
        !--------------------------------------------------Nondimensionalize:
                IF(gVertArr%H(N) .NE. 0.) gVertArr%H(N) = gScaleFactors%dissipation / gVertArr%H(N)
        !--------------------------------------------------Zero TF at level N
                gHorzArr%TF(1 : gCurrentDims%ITOT, N) = 0.0
        !--------------------------------------------------Compute equilibrium TF:
                X = (SHORT_WAVE(J) * AA + SHORT_WAVE(J + 1) * BB) * gVertArr%THS(N)
                gHorzArr%TF(1, N) = X / (4. * Y * SQRT2)
                gHorzArr%TF(1, N) = gHorzArr%TF(1, N) * gVertArr%A(N) / gPlotScales(TEMPERATURE)
            END DO

        END subroutine RaisanenRadiationProfile

end module SetUpModule
