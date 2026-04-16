! This is the module that contains all the model integration time stepping code
module ModelModule
    implicit none

    public :: InitModelWithModelControlVals     ! Called after user enters the fields wanted in the ModelControlDialog and then has selected the option
                                                ! to start running the model (F7 or F8)
    public :: StartTimeLoop
    public :: GQSET
    public :: RMLAKL
    public :: EquationSystem
    public :: NONLIN
    public :: ComputeJacobianByTransform
    public :: DoDADX
    public :: DoDADY
    public :: STERMS
    public :: TTERMS
    public :: InverseTransform      ! Not used anymore
    public :: PROFIL
    public :: PICK
    public :: TRANS

    contains

        !> The user has configured the values exposed in the ModelControl dialog and has selected to run the model.
        !> After some basic idiot checks, this function creates those fields needed to run the time loop given the
        !> user values. It also does some further idiot checking like the scaling parameters not being zero.
        function InitModelWithModelControlVals() result(errorCode)
            use ConstantsModule, only: ZONAL_MEAN_FLOW, ERR_SCALING_PARAMETER_ZERO, ZLEVEL_1
            use ArrayDimensionsModule
            use ModelFieldsModule, only: gHorzArr, gVertArr
            use ModelParamsModule, only: gModIntParams, gModRealParams, gModPlotParams, gModScalingParams, gUserSettings
            use InteractionCoefficientsModule, only: computeInteractionCoefficients
            !use FFT991Module, only: SET99
            implicit none
            
            ! Local variables
            integer :: errorCode
            integer :: I, N, numberOfCoefs
            REAL :: VR24

            errorCode = 0
            CALL RMLAKL()   ! Computes RML array for the re-projection of the meridional components in the inverse transform routine
                            ! This precomputes some of the factors used in the inverse transform like wavenumber squared, etc. that
                            ! will reduce the muliplications and divisions during the time loop.
            !===========================================================Zonal model type setup:
            IF(gModIntParams%modelType .EQ. ZONAL_MEAN_FLOW .AND. gModPlotParams%isBarotropic .EQ. 0) THEN
                !----------------------------Create QcosG and QsinG and place in WW for waves:
                call GQSET()        ! In this file
                !----------------------------Initialize barotropic mean flow:       DO I=LMAX2+1,ITOT
                DO I = gModIntParams%LMAX2 + 1, gCurrentDims%ITOT
                    gHorzArr%PSB(I) = 0.
                    DO N = 1, gModIntParams%NLEV
                        gHorzArr%PSB(I) = gHorzArr%PSB(I) + gHorzArr%PS(I, N) / gVertArr%DPI(N)
                    END DO
                END DO
                !------------------------------------------------ Indicator for Zonal run:
                gModPlotParams%isBarotropic = .TRUE.
            ENDIF
            ! Check that none of the scaling parameters are zero
            ! These parameters are used only in plotting
            DO I = 1, 23
                IF(gModScalingParams%VR(I) .EQ. 0.) then
                    errorCode = ERR_SCALING_PARAMETER_ZERO
                    return
                end if
            END DO
            VR24 = gModScalingParams%VR(ZLEVEL_1) ! VR(28) So far not used and probably wont be.

        end function InitModelWithModelControlVals

        function StartTimeLoop() result(errorCode)
            use ConstantsModule
            use ArrayDimensionsModule
            use ModelParamsModule, only: gUserSettings, gModRealParams, gModIntParams, gScaleFactors, FileStruct, gPlotScales
            use ModelFieldsModule, only: gHorzArr, gVertArr, gStatsArr, G2_pool, G3_pool, ZeroHorzArrays, ZeroVerticalArrays
            use SetUpModule, only: WriteSensFileHeader, SetDimensions, ComputeVerticalProfiles, WriteFileData, PERTB
            use InteractionCoefficientsModule
            use NlevelQGModelGlobals, only: ghwndMain, LogMessage
            use iso_c_binding
            use Fftw3HelpersModule, only: test_transforms
            use UtilitiesModule, only: openModelFile
            use, intrinsic :: ieee_arithmetic

            implicit none

            ! Local variables
            type(FileStruct) :: newSensFile
            logical :: bufferIsAllocated
            integer :: errorCode
            integer :: ICONV, ISPIN, NR, ISHR, ITIME, IXP
            integer :: I, K, L, M, N, JXS, JYX, JYS
            integer :: ITOTB, ITOTS, ITEMP, ICOUNT
            integer :: KSTEP, KCYC, KSTEPT, NCYC4
            REAL :: RNP, R, AMPP, PAMPP, AMP, PAMP, RSS, PDIF, CONV
            REAL :: DDELT, RD, RDD, A1, B1, B1N, R1
            REAL, ALLOCATABLE :: buffer(:)
            character(len=256) :: msg
            
            ! Test transforms
            !call test_transforms()

            write(msg, '(*(I2, 1X))') (gCintArr%JIN(K), K = 1, gCintArr%numOfInteractionCoeffs)
            call LogMessage(msg)
            write(msg, '(*(I2, 1X))') (gCintArr%KIN(K), K = 1, gCintArr%numOfInteractionCoeffs)
            call LogMessage(msg)


            errorCode = 0
            ALLOCATE(buffer(gCurrentDims%ITOT))
            bufferIsAllocated = .TRUE.  ! Use this to make sure the buffer gets deallocated at the end of its use
            !call EquationSystem(buffer)

            !-------------------------------First iterative loop:
            RNP = gModRealParams%RN
            !-------------------------------Are we loading/storing consecutive runs?
            ! It appears this is set up in the F6 'view statistics' option of the model control menu.
            ! That is something that clearly needs its own option! It took me forever to find it.
            ICONV = 0
            ICONV = gModIntParams%ITER1 + gModIntParams%ITER2
            ! The only time there is no sensitivity set of experiments is when both ITER1 and ITER2 are one. In that
            ! case nothing is varied.
            IF(ICONV .GT. 2) ICONV = 3
            !-------------------------------Compute spin up time steps:
            ISPIN = INT(gModRealParams%SPINUP * 1440. / (gScaleFactors%timeStep * gModRealParams%DELT))
            !===============================================================Set up file
            ! If we are doing a series of runs where we vary the parameters we need to create a file
            ! to save the results of each run
            IF(ICONV .GT. 2) THEN
                block ! Do this just to locally declare strings for date-time
                    character(len=8)  :: date
                    character(len=10) :: time
                    character(len=5)  :: zone

                    call date_and_time(date, time, zone) ! date of format ccyymmdd  time of format hhmmss.sss
                    newSensFile%fileName = date // time //'.sens'
                    errorCode = openModelFile(newSensFile, .true.)

                    !------------------------------------Write header (This is different than the standard sav file):
                    errorCode = WriteSensFileHeader(newSensFile%fileHandle)
                    !------------------------------------Save current state if called for:
                    ! This is for the case where we begin each run from the same initial condition.
                    ! I believe this was probably looking for low resolution cases where there might
                    ! be multiple stable statistical equilibria. Otherwise, the same statistical equilibrium
                    ! should be reached regardless of initial conditions. In the original code values
                    ! of these parameters from 21-31 used the same initial condition at the start of
                    ! each run.
                    IF(gModIntParams%IPRM1 .GT. 20 .OR. gModIntParams%IPRM2 .GT. 20) then
                        DO N = 1, gModIntParams%NLEV
                            DO I = 1, gCurrentDims%ITOT
                                gHorzArr%PSSAVE(I, N) = gHorzArr%PS(I, N)
                            END DO
                        END DO
                    end if
                end block
            ENDIF
            !======================================================Start loop one:
            DO NR = 1, gModIntParams%ITER1
                IF(gModIntParams%ITER1 .GT. 1) THEN ! If there is only one iteration we have no fractional increment
                    !---------------------------------------- Alter the approp param:
                    R = REAL(NR - 1) / REAL(gModIntParams%ITER1 - 1)
                    !-----------------------------------------Reinitialize flow if IPRM1>20
                    IF(gModIntParams%IPRM1 .GT. 20) then
                        DO N = 1, gModIntParams%NLEV
                            DO I = 1, gCurrentDims%ITOT
                                gHorzArr%PS(I, N) = gHorzArr%PSSAVE(I, N)
                            END DO
                        END DO
                    end if
                    CALL PROFIL(gVertArr%PRF, R, gModIntParams%IPRM1) 
                            ! PRF is 4 arrays in one. PRF((Yindex-1) * maxLevels + 1) Passing like this is effectively a pointer
                            ! That pointer will avoid any copies. With PRF(1, 1) all we need to pass is PRF
                END IF
                !-----------------------------Second quantity:
                DO ISHR = 1, gModIntParams%ITER2
                    !----------------------------------Update second profile:
                    IF(gModIntParams%ITER2 .GT. 1) THEN
                        !------------------------------Evenly divide increments:
                        R = REAL(ISHR - 1) / REAL(gModIntParams%ITER2 - 1)
                        IF(gModIntParams%IPRM2 .GT. 20) then
                            DO N = 1, gModIntParams%NLEV
                                DO I = 1, gCurrentDims%ITOT
                                    gHorzArr%PS(I, N) = gHorzArr%PSSAVE(I, N)
                                END DO
                            END DO
                        end if
                        ! So now it seems I need to put in the : and change the subroutine to PRF(:) instead of PRF(*)
                        CALL PROFIL(gVertArr%PRF(2 * gUserSettings%maxLevels :), R, gModIntParams%IPRM2)  ! PRF(1, 3)
                    END IF
                    !---------------------------------------------Are we looping?
                    IF(ICONV .GT. 2) THEN
                        !-----------------------------------------No negative or 0 RN!
                        IF(gModRealParams%RN .LE. 0.) THEN
                            !M = 20
                            return  !GO TO 105
                        END IF
                        !-----------------------------------------Did we change RN?
                        ! RN can change if we are doing a sensitivity run varying the channel length.
                        IF(RNP .NE. gModRealParams%RN) THEN
                            ! This change will not effect the number of interaction coefficients
                            gCintArr%numOfInteractionCoeffs = &
                                            computeInteractionCoefficients(gModRealParams%RN, gModIntParams%KMAX2, gModIntParams%LMAX2, gCintArr%numOfInteractionCoeffs)
                            CALL RMLAKL()           ! Subroutine is in this file
                            RNP = gModRealParams%RN
                        END IF
                        !-----------------------------------------Static stability? Check if changed
                        IF(gModIntParams%IPRM1 .EQ. SENS_STATIC_LAPSE_RATE .OR. gModIntParams%IPRM2 .EQ. SENS_STATIC_LAPSE_RATE) CALL ComputeVerticalProfiles()
                        !---------------------------------------------Set convergence-test values:
                        AMPP = 0.
                        PAMPP = 0.
                    END IF
                    !--------------------------------------------- Start of original integrations:
                    !ITIME = INT(TIME) / gModIntParams%MOUT   ! I cannot figure out where this time value is set. Does not appear to be used.
                    !---------------------------------------------------- Initialize plot window:
200                 IXP = 1



                    !--------------------------------------------------- Stats setup:
                    IF(gUserSettings%statsOption .NE. STATS_OPT_NONE) THEN
                        gStatsArr%PSA = 0.0       ! F90 shortcut to zero an N-d array
                        gStatsArr%PSS = 0.0
                        gStatsArr%EK = 0.0
                        gStatsArr%APE = 0.0
                        gStatsArr%RKE = 0.0
                        !-----------------------------------Zero variance arrays:
                        IF(gUserSettings%statsOption .EQ. STATS_OPT_VAR) gStatsArr%V1 = 0.0       ! F90 shortcut to zero an N-d array
                        !-----------------------------------Do not zero GQ arrays in Zonal run:
                        IF(gModIntParams%modelType .NE. ZONAL_MEAN_FLOW) THEN
                            gHorzArr%GQ = 0.0
                            gHorzArr%BTA = 0.0
                        END IF
                    END IF
                    !------------------------------------------------ Time stepping variables:
                    KSTEPT = 0
                    DDELT = 4. * gModRealParams%DELT
                    RD = 1. / gModRealParams%DELT
                    RDD = 1. / DDELT
                    ITOTS = 1
                    !--------------------------------- Turn off integration of zonal fields:
                    IF(gModIntParams%modelType .EQ. BAROCLINIC_STRUCTS) ITOTS = gModIntParams%LMAX2 + 1
                    ITOTB = gCurrentDims%ITOT
                    IF(gModIntParams%modelType .EQ. ZONAL_MEAN_FLOW) ITOTB = gModIntParams%LMAX2
                    !--------------------------------------------------- Actual beginning of loop:
        900         DO KCYC = 1, gModIntParams%ICYC
                        DO KSTEP = 1, gModIntParams%ISTEP
                            !---------------------------------------KSTEPT=Total # of current time steps:
850                         KSTEPT = KSTEPT + 1
                            !write(*, '(A, I4, A, I4, A, I4)') 'KSTEPT=', KSTEPT, ' KSTEP=', KSTEP, ' KCYC=', KCYC
                            !write(msg, '(A, I4, A, I4, A, I4)') 'KSTEPT=', KSTEPT, ' KSTEP=', KSTEP, ' KCYC=', KCYC
                            !call LogMessage(msg)
                            !*************************************************************** N-Cycle loop:
                            A1 = 0.
                            B1 = RD
                            !--------------------------------------Normalize structures KE for modelType=BAROCLINIC_STRUCTS
                            IF(gModIntParams%modelType .EQ. BAROCLINIC_STRUCTS .AND. gModIntParams%NLEV .GT. 1)THEN
                                DO I = gModIntParams%LMAX2 + 1, gCurrentDims%ITOT, 2
                                    !-------------------------------------Find KE:
                                    R1 = 0.
                                    DO N = 1, gModIntParams%NLEV
                                        R1 = R1 + (gHorzArr%PS(I, N)**2 + gHorzArr%PS(I + 1, N)**2) / gVertArr%DPI(N)
                                    END DO
                                    IF(R1 .GT. 0.)THEN
                                        !-------------------------------------Set normalization factor to .05:
                                        R1 = .05 * SQRT(1. / R1)
                                        !-------------------------------------Normalize structure:
                                        DO N = 1, gModIntParams%NLEV
                                            gHorzArr%PS(I, N) = R1 * gHorzArr%PS(I, N)
                                            gHorzArr%PS(I + 1, N) = R1 * gHorzArr%PS(I + 1, N)
                                        END DO
                                    ENDIF
                                END DO
                            ENDIF
                            DO NCYC4 = 1, 4
                                call EquationSystem(buffer)
                                ! FS() has streamfunction tendencies after this call
                                ! ADS() has vorticity plus beta tendencies after this call
                                ! ADT() has temperature tendencies after this call
                                !--------------------------------------Computations after SYSTEM:
                                !  We have to do many computations when J=1, that is before the streamfunction
                                !  is time stepped, such that all the diagnostic fields are consistent with
                                !  the streamfunction.  These computation includes the statistics
                                !
                                IF(NCYC4 .EQ. 1) THEN
                                    !---------------------------------------------- Selecting when to output data:
                                    N = gModIntParams%MOUT
                                    ITEMP = MOD(KSTEP - 1, N)
                                    !IF(ITEMP .EQ. 0) EXECUTE PLTNOW
                                    !-----------------------------------------Compute mean fields:
                                    IF(gUserSettings%statsOption .NE. STATS_OPT_NONE) THEN
                                        CALL ComputeGQ()
                                        !-------------------------------Lorenz Energy cycle: Routine for correlations:
                                        !CALL LORENZ(gHorzArr%PS, gHorzArr%TH, gHorzArr%WW, gHorzArr%S, gHorzArr%T, gHorzArr%ADO, gVertArr%DPI, &
                                        !    gVertArr%SIG, gHorzArr%AKL, gStatsArr%EK, gStatsArr%APE, gStatsArr%RKE)
                                        !----------------------------------------------Variance computations:
                                        IF(gUserSettings%statsOption .EQ. STATS_OPT_VAR) THEN
                                            DO N = 1, gModIntParams%NLEV
                                                CALL PICK(gHorzArr%DAT, gHorzArr%PS, N, gModIntParams%IV1)
                                                CALL TRANS(G2_pool, gHorzArr%DAT, gHorzArr%WRK, gHorzArr%TRX, gHorzArr%TMP)
                                                CALL PICK(gHorzArr%DAT, gHorzArr%PS, N, gModIntParams%IV2)
                                                CALL TRANS(G3_pool, gHorzArr%DAT, gHorzArr%WRK, gHorzArr%TRX, gHorzArr%TMP)
                                                JXS = 0
                                                DO K = 1, gCurrentDims%IYH
                                                    DO I = 1, gCurrentDims%IXMAX1
                                                        JYS = JXS + I
                                                        gStatsArr%V1(JYS, N) = gStatsArr%V1(JYS, N) + G2_pool(JYS) * G2_pool(JYS)
                                                        gStatsArr%V2(JYS, N) = gStatsArr%V2(JYS, N) + G3_pool(JYS) * G3_pool(JYS)
                                                    END DO
                                                    JXS = JXS + gCurrentDims%KX
                                                END DO
                                            END DO
                                        ENDIF ! End variance:
                                    ENDIF ! End stats:
                                    !---------------------------------------------- Check for convergence:
                                    IF(ICONV .GT. 2) THEN
                                        !-----------------------------------------------------Toss spin up time:
                                        IF(ICONV .EQ. 3 .AND. KSTEPT .GT. ISPIN) THEN
                                            ICONV = 4
                                            GO TO 200
                                        ENDIF
                                        AMP = 0.
                                        !------------------------------------------Pick a middle level:
                                        N = gModIntParams%NLEV / 2 + 1
                                        L = gModIntParams%LMAX2 + 1
                                        !------------------------------------------Find amplitude tendency:
                                        IF(gModIntParams%modelType .NE. ZONAL_MEAN_FLOW) AMP = &
                                                ABS(gHorzArr%FS(L, N) * gHorzArr%PS(L,N) + gHorzArr%FS(L + 1, N) * gHorzArr%PS(L + 1, N))
                                        !-------------------------------------------------Compute amplitude:
                                        PAMP = SQRT(gHorzArr%PS(L, N)**2 + gHorzArr%PS(L + 1, N)**2)
                                        !-------------------------------------------------Compute tendency:
                                        IF(PAMP .GT. 0.) AMP = AMP / PAMP
                                        !-------------------------------------------------Add zonal flow tendency:
                                        IF(gModIntParams%modelType .NE. BAROCLINIC_STRUCTS) AMP = AMP + ABS(gHorzArr%FS(1, N))
                                        !-------------------------------------------------Scale to meters/day:
                                        AMP = AMP * gPlotScales(TOT_HGT_TEND)  ! gPlotScales is defined in ModelParametersModule
                                        !-------------------------------------------------Scale to meters:
                                        PAMP = PAMP * gPlotScales(HEIGHT)      ! gPlotScales is defined in ModelParametersModule
                                        !---------------------------------------------Find difference from previous:
                                        RSS = ABS(AMP - AMPP)
                                        PDIF = ABS(PAMP - PAMPP)
                                        !---------------------------------------------Set old to current:
                                        AMPP = AMP
                                        PAMPP = PAMP
                                        IF(RSS .LT. CONV .AND. PDIF .LT. CONV) THEN
                                        !------------------------------------------------------Indicate convergence:
                                            ICONV = 5
                                            GO TO 3131
                                        ENDIF 
                                    ENDIF !=====================================End of convergence tests:
                                ENDIF !=====================================End of NCYC=1 operations:
                                !============================================================ Integration:
                                B1N = 1. / B1
                                !--------------------------------------Do barotropic part for waves:
                                IF(gModIntParams%modelType .EQ. ZONAL_MEAN_FLOW)THEN
                                    DO I = gModIntParams%LMAX2 + 1, gCurrentDims%ITOT
                                        gHorzArr%ZS(I, 2) = 0.
                                        !--------------------------------------Take vertical average of forcing:
                                        !  Place vertical average FS in ZS(I,2)
                                        !  Use ZS(I,1) for the barotropic ZS array
                                        !  PSB is the barotropic flow
                                        !-----------------------------------------------------------------------
                                        DO N = 1, gModIntParams%NLEV
                                            gHorzArr%ZS(I, 2) = gHorzArr%ZS(I, 2) + gHorzArr%FS(I, N) / gVertArr%DPI(N)  ! Why not gVertArr%DP(N)
                                        END DO
                                        gHorzArr%ZS(I, 1) = A1 * gHorzArr%ZS(I, 1)
                                        gHorzArr%ZS(I, 1) = (gHorzArr%ZS(I, 1) + gHorzArr%ZS(I, 2)) * B1N
                                        gHorzArr%PSB(I) = gHorzArr%PSB(I) + gHorzArr%ZS(I, 1)
                                    END DO
                                ENDIF
                                !-------------------------------------------Time step all levels and waves:
                                DO N = 1, gModIntParams%NLEV
                                    DO I = ITOTS, ITOTB
                                        gHorzArr%ZS(I, N) = A1 * gHorzArr%ZS(I, N)
                                        gHorzArr%ZS(I, N) = (gHorzArr%ZS(I, N) + gHorzArr%FS(I, N)) * B1N
                                        gHorzArr%PS(I, N) = gHorzArr%PS(I, N) + gHorzArr%ZS(I, N)
                                    END DO
                                    !---------------------------------------------Numerical instability check:
                                    IF(ABS(gHorzArr%PS(1, N)) .GT. 100. .OR. ieee_is_nan(gHorzArr%PS(1, N))) THEN
                                        M = 28
                                        write(msg, '(A, I4, A, I4, A, I4)') 'Level=', N, ' KSTEP=', KSTEP, ' NCYC4=', NCYC4
                                        call LogMessage("Numerical instability. Exiting. " // msg)
                                        call ZeroHorzArrays(.true.) ! Don't zero settings like TF, SF, TOP, AKL, etc.
                                        call PERTB()
                                        return  !GO TO 105
                                    ENDIF
                                END DO
                                B1 = B1 - RDD
                                A1 = A1 - RDD
                            END DO !==============================================================End of N-cycle:
                            write(msg, '(A, I4, A, (F10.6, 1X), A, *(F10.6, 1X))') ' KSTEP=', KSTEP, ': ', gHorzArr%PS(1, (gModIntParams%NLEV/2 + 1)), ' ', (gHorzArr%PS(i, (gModIntParams%NLEV/2 + 1)), i = 1 + gModIntParams%LMAX2, gCurrentDims%ITOT, 2 * gModIntParams%LMAX2)
                            call LogMessage("Values: " // msg)
                        END DO ! Inner loop ISTEP done
                    END DO ! Outer loop ICYCL done   Total number of time steps = ISTEP * ICYCL 
                    call LogMessage("Time loop done")

                    !---------------------------------------------------------Write output
        3131        IF(ICONV .GT. 2) THEN
                        !-------------------------------------------Compute all averages:
                        !EXECUTE AVERAG
                        ICOUNT = ICOUNT + 1
                        !-------------------------------------------------------Save results to file:
                        WRITE(newSensFile%fileHandle) ICONV
                        M = WriteFileData(newSensFile%fileHandle)
                        !---------------------------------------------------Reset ICONV for SPINUP:
                        ICONV = 3
                    ENDIF
                END DO ! -------------------- Done ITER2 loop (Why was it called the 'shear' loop in quotes?)
            END DO !----------------------End ITER1 loop
            !--------------------------------------------------------End of statistics
        990 IF(ICONV .GT. 2) THEN
                !------------------------Write out the actual number of runs in JCN(30):
                !JCN(30) = ICOUNT
                if (newSensFile%fileHandle /= -1) CLOSE(newSensFile%fileHandle)
                newSensFile%fileHandle = -1
                
            ELSE !------------------------------------Dont do average computations twice:
                !====================================================Finalize Statistics:
                !EXECUTE AVERAG
            ENDIF
            ! will be return to model control menu          
        end function StartTimeLoop

        ! Resets the GQ field to Q(cosG,sinG)
        subroutine GQSET()
            use ArrayDimensionsModule
            use ModelParamsModule, only: gModIntParams
            use ModelFieldsModule, only: gHorzArr
            
            implicit none

            integer I, K, N
            real R
            !----------------------------------Reset GQ field to Q(cosG,sinG)
            DO I = gModIntParams%LMAX2 + 1, gCurrentDims%ITOT
                K = 2 * ((I - gModIntParams%LMAX2 - 1) / 2) + gModIntParams%LMAX2 + 1
                R = gHorzArr%BTA(K)
                IF(R .GT. 0.) R = 1. / (R * R)
                !--------------------------------------------We have PSB*TH in wave part:
                DO N = 1, gModIntParams%NLEV
                    gHorzArr%GQ(I, N) = gHorzArr%GQ(I, N) * R
                END DO
            END DO
        end subroutine GQSET

        !******************************************** Precompute ARRAYs RML,AKL for inverse transform:
        SUBROUTINE RMLAKL()
            use ModelFieldsModule, only: gHorzArr
            use ModelParamsModule, only: gModIntParams, gModRealParams, PI

            implicit none

            ! Local variables        
            INTEGER I, K, L, M
            REAL R, RL2, RM2, RTEM

            !--------------------------------------Reprojection array RML(2*gModIntParams%LMAX2+1,gModIntParams%LMAX2):
            !  Precalculation of the RML array for the re-projection of the meridional
            !  components in the inverse transform routine:
            DO L = 1, gModIntParams%LMAX2
                RL2 = REAL(L * L)
                DO M = 1, 2 * gModIntParams%LMAX2 + 1
                    RM2 = REAL((M - 1)**2)
                    RTEM = PI * (RM2 - RL2)
                    gHorzArr%RML(M, L) = 0.      ! RML(2 * maxMeridWaves+1, maxMeridWaves)
                    IF (RTEM .NE. 0.) gHorzArr%RML(M, L) = 4. / RTEM
                END DO
            END DO
        !----------------------------------------Compute AKL and AKLI:
            DO L = 1, gModIntParams%LMAX2
                gHorzArr%AKL(L) = REAL(L * L)
                gHorzArr%AKLI(L) = 1. / gHorzArr%AKL(L)
            END DO

            I = gModIntParams%LMAX2
            DO K = 1, gModIntParams%KMAX2
                R = gModRealParams%RN * gModRealParams%RN * REAL(K * K)
                DO L = 1, gModIntParams%LMAX2
                    I = I + 1
                    gHorzArr%AKL(I) = R + REAL(L * L)
                    gHorzArr%AKLI(I) = 1. / gHorzArr%AKL(I)
                    I = I + 1
                    gHorzArr%AKL(I) = gHorzArr%AKL(I - 1)
                    gHorzArr%AKLI(I) = gHorzArr%AKLI(I - 1)
                END DO
            END DO
        END subroutine RMLAKL

        !************************************************************ EQUATION SYSTEM:
        ! This routine computes the tendency equations for the Quasi-Geostrophic model.
        ! We have the vorticity equation for each level n. del2 is the horizontal laplacian, J is the Jacobian,
        ! f is the Coriolis parameter, w is the vertical velocity, p is pressure, and H is the scale height.
        ! d(del2 PS(n)) + J(PS(n), del2PS(n)) + f + f*top/H) – f(w(n-1) – w(n))/(p(n-1) – p(n)) =
        !   fk(n)del2(sf(n)-ps(n)) + fj(n)u(n)del2(ps(n+1) – ps(n)) + fj(n-1)v(n)del2(PS(n-1)-PS(n))
        ! u(n) = (p(n+1) – p(n+2)) / (p(n) - p(n+2))  pressures are at the boundaries, PP in this model
        ! v(n) = (p(n-1) – p(n)) / (p(n-1) - p(n+1))
        ! Temperature equation
        ! dTH(n)/dt +  J((PS(n+1) + PS(n))/2, TH(n)) -SIG(n)*W(n) = H(n)(TF(n)-TH(n)) + G(n)*u'(n)(TH(n+1) – TH(n)) + G(n-1)*v'(n)*(TH(n-1) – TH(n))
        ! u'(n) = (p'(n+1) – p'(n+2)) / (p'(n) - p(n + 2)) pressures are at the boundaries, PV in this model
        ! v'(n) = (p'(n-1) – p'(n)) / (p'(n-1) - p'(n+1))
        ! Thermal wind equation:
        ! A(n)del2(TH(n)) = del2(PS(n+1) - PS(n))) where A(n) = cp*[p(n)**kappa - p(n+1)**kappa]/(f * p(1)**kappa)
        ! del2(PS) = -a**2
        ! d(ps(n))/dt - [J(ps(n), del2(ps(n))) + f + f*top) - (w(n-1) - w(n))/(p(n-1) - p(n))]/a**2 = 
        !           fk(n)(sf(n)-ps(n)) + fj(n)u(n)(ps(n+1) – ps(n)) + fj(n-1)v(n)(PS(n-1) - PS(n))
        ! d(th(n))/dt + J((PS(n+1) + PS(n))/2, TH(n)) - SIG(n)*W(n) = H(n)(TF(n)-TH(n)) + G(n)*u'(n)(A(n)TH(n+1)/A(n+1) – TH(n)) + 
        !           G(n-1)*v'(n)*(A(n)TH(n-1)/A(n-1) – TH(n))
        ! th(n) = ps(n+1) - ps(n)   Thermal wind relation
        
        ! if we call S(n) = fk(n)(sf(n)-ps(n)) + fj(n)u(n)(ps(n+1) – ps(n)) + fj(n-1)v(n)(PS(n-1) - PS(n))
        !       and  T(n) = H(n)(TF(n)-TH(n)) + G(n)*u'(n)(A(n)TH(n+1)/A(n+1) – TH(n)) + G(n-1)*v'(n)*(A(n)TH(n-1)/A(n-1) – TH(n))
        !       and  W(n) = (w(n-1) - w(n))/(p(n-1) - p(n))
        
        ! Then 
        ! d(ps(n))/dt - [J(ps(n), del2(ps(n))) + f + f*top) - W(n)]/a**2 = S(n)
        ! d(th(n))/dt + J((PS(n+1) + PS(n))/2, TH(n)) - SIG(n)*w(n) = T(n)
        ! We can use the thermal wind relation to compute the omegas Top is only at the lowest layer.
        
        ! using th(n) = ps(n+1) - ps(n) 
        
        ! d(th(n))/dt = [J((PS, del2(ps) + f)(n+1) - J((PS, del2(ps) + f + f*top)(n) - W(n+1) + W(n))]/a**2 + S(n+1) - S(n)
        ! d(th(n))/dt = -J((PS(n+1) + PS(n))/2, TH(n)) + SIG(n)*w(n) + T(n)
        !
        ! J(PS)(n) = J(PS(n), del2(ps(n)) + f + f*top/H)
        ! J(TH)(n) = J((PS(n+1) + PS(n))/2, TH(n))
        ! Eliminate the time derivative
        ! -J(TH)(n) + SIG(n)*w(n) + T(n) = [J(PS)(n+1) - J(PS)(n) - W(n+1) + W(n))]/a**2 + S(n+1) - S(n)
        ! 0 = J(TH)(n) - SIG(n)*w(n) - T(n) + [J(PS)(n+1) - J(PS)(n) - W(n+1) + W(n))]/a**2 + S(n+1) - S(n)
        ! J(TH)(n) - T(n) + [J(PS)(n+1) - J(PS)(n)]/a**2 + S(n+1) - S(n) = [a**2*SIG(n)*w(n) + W(n+1) - W(n)]/a**2

        ! Define
        ! C(n) = J(TH)(n) - T(n) + [J(PS)(n+1) - J(PS)(n)]/a**2 + S(n+1) - S(n)
        ! C(n) = [a**2*SIG(n)*w(n) + (w(n) - w(n+1))/(p(n) - p(n+1)) - (w(n-1) - w(n))/(p(n-1) - p(n))]/a**2
        
        ! w(n)[1/(a**2(p(n)- p(n+1))) + 1/(a**2(p(n-1) - p(n))) + SIG(n)] -w(n+1)/(a**2(p(n) - p(n+1))) - w(n-1)/(a**2(p(n-1) - p(n))) = C(n)
        
        ! alpha(n)*w(n) - epsilon(n)*w(n+1) - gamma(n)*w(n-1) = C(n)
        ! where
        ! gamma(n) = 1/(a**2(p(n-1) - p(n)))
        ! alpha(n) = gamma(n+1) + gamma(n) + sig(n)
        ! epsilon(n) = gamma(n+1)

        !===========================================================================
        SUBROUTINE EquationSystem (buffer)
            use ModelParamsModule
            use ArrayDimensionsModule
            use ModelFieldsModule

            implicit none

            real, intent(inout) :: buffer(:)

            !------------------------------------------Local variables:
            INTEGER :: I, K, L, N, NP, NT
            REAL :: AX, BRN, R, R1
            REAL :: C, Clower, Cupper, gamma, gammaLower, alpha, alpha1 ! Terms for iterative omega loop

            IF(gModIntParams%modelType .EQ. ZONAL_MEAN_FLOW) THEN
                !-----------------Compute waves at level "N" based upon structures and PSB:
                DO N = 1, gModIntParams%NLEV
                    DO K = gModIntParams%LMAX2 + 1, gCurrentDims%ITOT, 2
                        L = K + 1
                        gHorzArr%PS(K, N) = gHorzArr%GQ(K, N) * gHorzArr%PSB(K) + gHorzArr%GQ(L, N) * gHorzArr%PSB(L) + gHorzArr%PSB(K)
                        gHorzArr%PS(L, N) = gHorzArr%GQ(K, N) * gHorzArr%PSB(L) - gHorzArr%GQ(L, N) * gHorzArr%PSB(K) + gHorzArr%PSB(L)
                    END DO
                END DO
            ENDIF

            !-------------------------------------------------Order of events:
            !  First we compute the nonlinear terms by either transforms or interactions,
            !  the orographic term has to be computed at level one.
            !  Then we compute the beta effect and add to vorticity advections.
            !  Next we compute the diabatic terms in STERMS and TTERMS
            !  Then we do the iterative proceedure to compute omega.
            !  Lastly we get the tendency for the streamfunction at each level.

            !----------------------------------------Compute Orographic influence:
            CALL NONLIN(gHorzArr%ADO, gHorzArr%PS(:, 1), gHorzArr%TOP)

            !---------------------------------------------Normalize by AKL:
            ! This is needed since the d(del**2(PS))/dt = -J(PS, TOP) + other terms including further jacobians. The del-squared operation gives the AKL factor
            ! dPS(k,l)/dt = -ADO(k,l)/AKL(k,l) where ADO = J(PS,TOP)
            ! ADO is the tendency due to orography 
            DO I = 1, gCurrentDims%ITOT
                gHorzArr%ADO(I) = -gHorzArr%AKLI(I) * gHorzArr%ADO(I)
            END DO

            !-----------------------------------------Compute vorticity advection:
            DO N = 1, gModIntParams%NLEV
                !----------------------------------------Compute vorticity:
                DO I = 1, gCurrentDims%ITOT
                    buffer(I) = -gHorzArr%AKL(I) * gHorzArr%PS(I, N)
                END DO
                CALL NONLIN(gHorzArr%ADS(:, N), gHorzArr%PS(:, N), buffer)

                !---------------------------------------------Normalize by AKL:
                DO I = 1, gCurrentDims%ITOT
                    gHorzArr%ADS(I, N) = gHorzArr%ADS(I, N) * gHorzArr%AKLI(I)
                END DO
            END DO

            !---------------------------------------Add Beta effect to ADS:
            IF(gModRealParams%BETA .NE. 0.) THEN
                I = gModIntParams%LMAX2 + 1
                BRN = gModRealParams%BETA * gModRealParams%RN
                DO K = 1, gModIntParams%KMAX2
                    R = BRN * REAL(K)
                    DO L = 1,  gModIntParams%LMAX2
                        R1 = R * gHorzArr%AKLI(I)
                        DO N = 1, gModIntParams%NLEV
                            gHorzArr%ADS(I, N) = gHorzArr%ADS(I, N) + R1 * gHorzArr%PS(I + 1, N)
                            gHorzArr%ADS(I + 1, N) = gHorzArr%ADS(I + 1, N) - R1 * gHorzArr%PS(I, N)
                        END DO
                    I = I + 2
                    END DO
                END DO
            ENDIF

            !----------------------------------------Compute diabatic strm terms:
            call STERMS()
            !-----------------------------------Barotropic computation:
            IF(gModIntParams%NLEV .EQ. 1) THEN
                DO I = 1, gCurrentDims%ITOT
                    gHorzArr%FS(I, 1) = gHorzArr%ADS(I, 1) + gHorzArr%ADO(I) + gHorzArr%S(I, 1)
                END DO
                RETURN
            ENDIF

            !---------------------------------------------Compute temperature advection:
            DO N = 1,  gModIntParams%NLEV - 1
                !-----------------------------------Compute strm and temp at temperature level:
                DO I = 1, gCurrentDims%ITOT
                    buffer(I) = (gHorzArr%PS(I, N) + gHorzArr%PS(I, N + 1)) / 2.
                    gHorzArr%TH(I, N) = (gHorzArr%PS(I, N + 1) - gHorzArr%PS(I, N)) ! No division by two here
                END DO
                CALL NONLIN(gHorzArr%ADT(:, N), buffer, gHorzArr%TH(:, N))
                !----------------------------------------Negate ADT to make warm adv pos:
                DO I = 1, gCurrentDims%ITOT
                    gHorzArr%ADT(I, N) = -gHorzArr%ADT(I, N)
                END DO
            END DO
            !----------------------------------------Compute diabatic temp terms:
            call TTERMS()

            !-----------------------------------------------Compute Omega for NLEV>1:
            K = gCurrentDims%ITOT

            !------------------------------If modelType=ZONAL_MEAN_FLOW set all I-terms>LMAX2 to zero:
            IF(gModIntParams%modelType .EQ. ZONAL_MEAN_FLOW)THEN
                K = gModIntParams%LMAX2
                DO N = 1, gModIntParams%NLEV
                    DO I = K + 1, gCurrentDims%ITOT
                        gHorzArr%WW(I, N) = 0.
                        gHorzArr%FS(I, N) = 0.
                        gHorzArr%WS(I, N) = 0.
                        gHorzArr%FT(I, N) = 0.
                        gHorzArr%WT(I, N) = 0.
                   END DO
                END DO
            ENDIF

            !============================================Omega iteration loop:
            !mu(n) = 2(p(n+1) - p(n+2))/(p(n) - p(n+2))	where the p's are the pressures of the boundary levels (PP array)
            !lambda(n) = 2(p(n-1) - p(n))/(p(n-1) - p(n+1))
            !mu'(n) = 2(p'(n+1) - p'(n+2))/(p'(n) - p'(n+2))	where these p's are the pressures of the boundaries (PV array)
            !lambda'(n) = 2(p'(n-1) - p'(n))/(p'(n-1) - p'(n+1))

            !S(n) =  k(n)*del2(PSIF(n) - PSI(n)) + j(n) * mu(n)*del2(PSI(n+1) - PSI(n)) + j(n)-lambda(n)*del2(psi(n-1)- psi(n))
            !T(n) = h(n)(thetaF(n) - theta(n)) + g(n)mu'(n)(theta(n+1) - theta(n)) + g(n-1)lambda'(n)(theta(n-1)- theta(n))
            !J(Theta) = J(PSI,Theta)
            !J(PSI) = J(Psi, del2(psi) + f + fh/H)

            !C(n) = J(Theta)(n) - T(n) + (S(n+1)- S(n))/2 + (1/a2)[J(Psi)(n+1) - J(Psi)(n) ]
            
            !alpha(n)*omega(n) – epsilon(n)*omega(n+1) – gamma(n-1)*omega(n-1) = C(n)
            !alpha(n) = gamma(n+1) + gamma(n) + sig(n)
            !gamma(n) = 1/(a**2 * P(n))
            !epsilon(n) = 1/(a**2 * P(n+1)) = gamma(n+1)
            
            !alpha(n)*omega(n) – gamma(n+1)*omega(n+1) – gamma(n-1)*omega(n-1) = C(n)
            ! Write
            ! WW(n) = X(n)*WW(n+1) + Y(n)
            ! Y(n) will have WW(n-1) in it, but the lower boundary condition of WW(0) = 0 will allow us to Y(n) to the top and there
            ! we can compute WW(NLEV-1)) at the top interface since WW(NLEV) = 0. It will be Y(NLEV-1). Then we can go back down to 
            ! WW(n) for n = NLEV-2 down to 1.


            DO I = 1, gCurrentDims%ITOT
                AX = gHorzArr%AKLI(I)   ! AKLI(i) is the inverse of a**2 and it depends only on the horizontal wavenumbers, not on the vertical level
                ! DPI(N) = PP(1)/(PP(N)-PP(N+1)) is the scaled inverse of the layer thickness. The PP array itself is NOT scaled.
                gammaLower = gVertArr%DPI(1) * AX  ! PP(1)/(a**2 * (PP(1) - PP(2)))
                gamma = gVertArr%DPI(2) * AX   ! PP(1)/(a**2 * (PP(2) - PP(3)))
                alpha = gamma + gammaLower + gVertArr%SIG(1)
                Cupper = gHorzArr%S(I, 2) + gHorzArr%ADS(I, 2)  ! Tendencies from streamfunction above omega level
                !-----------------------------------------------Add orography at bottom level:
                Clower = gHorzArr%S(I, 1) + gHorzArr%ADS(I, 1) + gHorzArr%ADO(I)   ! Tendencies from streamfunction below omega level
                C = gHorzArr%ADT(I, 1) + gHorzArr%T(I, 1) + (Clower - Cupper)   ! Temperature tendency plus difference in strm tendencies. This value
                                                                                ! of C(n) comes the omega eq. where the time derivative is eliminated
                gVertArr%X(1)  = gamma / alpha
                gVertArr%Y(1) = -C / alpha
                !-----------------------------------------------Iterative loop for NLEV>2
                DO N = 2, gModIntParams%NLEV - 1
                    gammaLower = gamma
                    gamma = gVertArr%DPI(N + 1) * AX    ! PP(1)/(a**2 * (PP(N) - PP(N+1)))
                    alpha = (gamma + gammaLower * (1. - gVertArr%X(N - 1)) + gVertArr%SIG(N))
                    Clower = Cupper
                    Cupper = gHorzArr%S(I, N + 1) + gHorzArr%ADS(I, N + 1)
                    C = gHorzArr%ADT(I, N) + gHorzArr%T(I, N) + (Clower - Cupper)
                    gVertArr%X(N) = gamma / alpha
                    gVertArr%Y(N) = (gammaLower * gVertArr%Y(N - 1) - C) / alpha
                END DO
                !------------------------------------Upper boundary to compute WW(NLEV-1):
                gHorzArr%WW(I, gModIntParams%NLEV - 1) = gVertArr%Y(gModIntParams%NLEV - 1)
                !-------------------------------------Strm tendency due to omega:
                gHorzArr%WS(I, gModIntParams%NLEV) = -gHorzArr%WW(I, gModIntParams%NLEV - 1) * gHorzArr%AKLI(I) * gVertArr%DPI(gModIntParams%NLEV)
                !------------------------------------Compute top level strm tendency:
                gHorzArr%FS(I, gModIntParams%NLEV) = gHorzArr%ADS(I, gModIntParams%NLEV) + gHorzArr%WS(I, gModIntParams%NLEV) + gHorzArr%S(I, gModIntParams%NLEV)
                !-----------------------------------------------Compute WW(N) for NLEV>2
                DO NT = 2, gModIntParams%NLEV - 1
                    N = gModIntParams%NLEV - NT
                    NP = N + 1
                    gHorzArr%WW(I, N) = gVertArr%X(N) * gHorzArr%WW(I, NP) + gVertArr%Y(N)
                    !-----------------------------------------Intermediate ws, strm tendencies:
                    gHorzArr%WS(I, NP) = -(gHorzArr%WW(I, N) - gHorzArr%WW(I, NP)) * gVertArr%DPI(NP) * gHorzArr%AKLI(I)
                    gHorzArr%FS(I, NP) = gHorzArr%ADS(I, NP) + gHorzArr%WS(I, NP) + gHorzArr%S(I, NP)
                END DO
                !----------------------------------Lower level strm tendency (with orography):
                gHorzArr%WS(I, 1) = gHorzArr%WW(I, 1) * gVertArr%DPI(1) * gHorzArr%AKLI(I)
                gHorzArr%FS(I, 1) = gHorzArr%ADS(I, 1) + gHorzArr%ADO(I) + gHorzArr%WS(I, 1) + gHorzArr%S(I,1)
                !-----------------------------------Load temperature arrays for diagnostics:
                DO N = 1, gModIntParams%NLEV - 1
                    gHorzArr%WT(I, N) = gVertArr%SIG(N) * gHorzArr%WW(I, N)
                    gHorzArr%FT(I, N) = gHorzArr%ADT(I, N) + gHorzArr%WT(I, N) + gHorzArr%T(I, N)
                END DO
            END DO

        !------------------------------------------------Noise filter:
        !    DO I=1,ITOT
        !       DO N=1,NLEV
        !        IF(ABS(FS(I,N)).LT.5.E-08)FS(I,N)=0.
        !        IF(ABS(PS(I,N,5)).LT.5.E-08)PS(I,N,5)=0.
        !        IF(ABS(FT(I,N)).LT.5.E-08)FT(I,N)=0.
        !        IF(ABS(PS(I,N,9)).LT.5.E-08)PS(I,N,9)=0.
        !        END DO
        !       END DO
        END subroutine EquationSystem

        !*******************************ROUTINE for NONLINEAR Computations:
        !  Contains both the interaction coefficient routine and the transform routine
        !  which are separated by the value of useTransformMethod.
        !


        !> Routine to compute the nonlinear Jacobian FS = J(A,B)
        !! @param FS The spectral form of the computed Jacobian
        !! @param A The first array in the Jacobian in spectral form J(A,B)
        !! @param B The second array in the Jacobian in spectral form J(A,B)
        SUBROUTINE NONLIN(FS, A, B)
            use ConstantsModule, only: USE_INTERACTION_COEFFICIENTS, FORWARD_TRANSFORM
            use ModelParamsModule, only: gUserSettings, gModIntParams, gModRealParams, SQRT2, SQRT22
            use ArrayDimensionsModule
            use ModelFieldsModule, only: gHorzArr, G1_pool
            use InteractionCoefficientsModule, only: gCintArr
            use NlevelQGModelGlobals, only: LogMessage
            !use FFT991Module, only: FFT991

            implicit none

            real, intent(inout) :: FS(:), A(:), B(:)
            character(len=256) :: msg

            !******************************************************** 'JTRN' part:
            !  Routine to compute the nonlinear Jacobian terms by a spectral transform
            !  method. J(A,B)=dA/dx * dB/dy - dA/dy * dB/dx
            !  The Jacobians are done for the mean streamfunction.
            !  The program cycles through a loop 4 times.
            !  The multiple GO TOs are arranged such that every odd pass, II=1,3
            !  the d/dx quantities are transformed and put in G1.  On the even
            !  passes (II=2,4), the d/dy quantities are transformed, put into G2,
            !  multiplied by G1 and added to G3.  After the 4th pass, G3 contains
            !  all the nonlinear terms.  It is then back-transformed.
            !  We do J(A,B) as below:
            !
            !  II=1  Zero G1 and G2, compute dA/dx, put in G1.
            !  II=2  Compute dB/dy of wave components, put in G2, compute the zonal
            !       contribution in TMP, add to G2, multiply dA/dx dB/dy, put in G3.
            !  II=3  Zero G1 and G2, compute dB/dx, put in G1.
            !  II=4  Compute dA/dy of wave components, put in G2, compute the zonal
            !       contribution in TMP, add to G2, multiply dA/dy dB/dx, subtract
            !       from G3, backtransform components, store in FS.
            !
            ! ---------------------------------------- INFORMATION  on FFT991
            !  The FFT991(G3,WRK,TRGi,IFX,INi,JMPi,N,M,ISIGN) has the following:
            !    G3 is array containing vectors to be transformed
            !    WRK is workspace array of dimension M*(N+2)
            !    TRGi is trigonometric array of size 3*N/2 precomputed
            !    IFX is an array holding factors 2,3, or 5 dimension 13 okay
            !    INi is the distance between successive data points
            !    JMPi is the distance between the start of successive vectors
            !    N is the dimension of the vector to be transformed
            !    M is the number of simultaneous vectors to be transformed
            !    ISIGN is the type of transform: -1=data to spec, 1=spec to data.
            !------------------------------------------------------------------------------
            !
            !  FS=The spectral form of the computed tendencies
            !  A,B are the arrays in the Jacobian in spectral form J(A,B)
            !  G1,G2,G3 are the three grid point arrays needed for the transform
            !  RML is an array used after the inverse transform for reprojection
            !  WRK,TRX,TRY are work and trig arrays used by FFT991
            !  TMP is a y_grid point array used for zonal only transforms

            !------------------------------------------In modIntParam of ModelParamsModule we find
            !  gModIntParams%KMAX2,gModIntParams%LMAX2=The number of x and y waves
            !  ----------------------------------------In ModelParamsModule
            !  SQRT2, SQRT22=The square root of two and SQRT2/2.
            !  modRealParam%RN = The wavenumber
            ! ----------------------------------------In gCurrentDims of ArrayDimensionsModule we find
            !  KX=The dimension of the grid point arrays in x:
            !  IXMAX=The number of grid points in x: IXMAX+2=KX
            !  IYMAX=The number of grid points in y: IYMAX+2=KY
            !  IFX(13),IFY(13)=The factors arrays for FFT991

            !-----------------------  Grid point arrays used in the transform integration
            !REAL :: G1(gCurrentDims%KX, gCurrentDims%LY), G2(gCurrentDims%KX, gCurrentDims%LY), G3(gCurrentDims%KX, gCurrentDims%LY), TMP(gCurrentDims%LY)

            !  Arrays containing the spectral strmfunction and topographic components
            !REAL:: A(gCurrentDims%ITOT), B(gCurrentDims%ITOT)

            !  Arrays for the time integration scheme used here to store nonlinear terms
            !REAL :: FS(gCurrentDims%ITOT)

            !  Arrays for the pre computation of the trig functions for FFT991
            !REAL :: WRK(gCurrentDims%KX * gCurrentDims%LY), TRX(1), TRY(1)

            !  Arrays for the analytical re-expression of the sinly to cosly components (needed in InverseTransform).
            !REAL :: RML(2 * maxMeridWaves + 1, maxMeridWaves)

            !-------------------------------------Interaction coefficient arrays:
            !INTEGER*2 gCintArr%I1M(1), gCintArr%JIN(1), gCintArr%KIN(1)

            !------------------------------------------------Other variables:
            INTEGER index
            INTEGER II, IODEV, IY, IX, I, KCOS, K, KSIN, LCOS, L, LSIN
            INTEGER II2, KY, M, MCOS, M1, MSIN, L2MAXR
            REAL AKN, SQ, AA, BA, RL, RM, AL2

            !------------------------------------------------------ Start for interaction coefficients:
            IF(gUserSettings%useTransformMethod .EQ. .FALSE.) then
                !****************************************Now for the interaction routine:
                !  Routine to compute the nonlinear terms of the interaction coefficient
                !  model.  The coefficients are computed in the routine computeInteractionCoefficients()
                !  in InteractionCoefficientsModule.
                !  FS is the spectral form of the advective terms
                !  A is the advecting streamfunction and B is advected: J(A,B)
                !  I1M is the number of interaction terms per equation
                !  JIN, KIN is the L, K index and C is the interaction coefficient
                !
                index = 1
                DO I = 1, gCurrentDims%ITOT
                    FS(I) = 0.
                    !----------------------------------- Advection terms:
                    DO II = 1, gCintArr%I1M(I)
                        L = gCintArr%JIN(index)
                        K = gCintArr%KIN(index)
                        FS(I) = FS(I) + gCintArr%CINT(index) * (B(K) * A(L) - B(L) * A(K))
                        write(msg, '(A, I4, A, F9.6, 1X, A, I4, A, I4, 1x, I4)') 'I: ', I, ' coef: ', gCintArr%CINT(INDEX), ' index: ', index, ' K, L: ', K, L
                        call LogMessage(msg)
                        index = index + 1
                    END DO
                END DO
                RETURN
            endif
            !------------------------------------------------------ Start for Transform method FFTW3 MKL:
            ! Computing J(A,B)=dA/dx * dB/dy - dA/dy * dB/dx where A(k,l) = (ac(k,l)cosknx + as(k,l)sinknx)sinly + az(l)cosly
            !                                                  and B(k,l) = (bc(k,l)cosknx + bs(k,l)sinknx)sinly + bz(l)cosly
            ! 'Returns' the results as the FS tendencies in spectral form.
            CALL ComputeJacobianByTransform(FS, A, B)

        END subroutine NONLIN
        
        !> Routine to compute the nonlinear Jacobian J(A,B) in gridpoint space
        !> and return the tendencies in spectral space in FS
        !! @param FS The spectral form of the computed Jacobian
        !! @param A The first term in the Jacobian
        !! @param B The second term in the Jacobian
        SUBROUTINE ComputeJacobianByTransform(FS, A, B)
            use ArrayDimensionsModule
            use ModelFieldsModule, only: gHorzArr
            use ModelParamsModule, only: gModIntParams
            use Fftw3HelpersModule

            implicit none
            include 'fftw3.f03'

            REAL, intent(in) :: A(:), B(:)
            REAL, intent(out) :: FS(:)

            integer I, K, L, i_temp
            REAL :: inverseValue
            ! In FFT991 the spectral coefficient for both cosknxsinly and sinknxsinly are loaded into the G1 arrays
            ! prior to invoking the transform.
            !   dA/dx is -2*k*n*ac(k,l)sinknxsinly and 2*k*n*as(k,l)cosknxsinly
            !   dB/dy is    2*l*bc(k,l)cosknxcosly and   2*l*bs(k,l)sinknxcosly and -sqrt(2)*l*bz(l)sinly

            ! dA/dx:  Done in subroutine DODADX result in G1(IXMAX, IYMAX)
            ! dB/dy:  Done in subroutine DODADY result in G2(IXMAX, IYMAX) this involves a transform of the zonal parts


            ! 1. dA/dx: 
            call DODADX(A)
            ! ================= dA/dx is now in G1

            ! 2. dB/dy: 
            call DODADY(B)
            ! ================ dB/dy is now in G2

            ! 3. Multiply G2 * G1 pointwise. Store in G4. G4 now has dA/dx * dB/dy in grid point space.
            gHorzArr%G4 = 0.0    ! Fortran short cut to zero an entire array of all dimensions
            do L = 1, gCurrentDims%IYMAX
                DO K = 1, gCurrentDims%IXMAX
                    gHorzArr%G4(K, L) = gHorzArr%G1(K, L) * gHorzArr%G2(K, L)
                end do
            end do
            ! ================================ G4 now has dA/dx * dB/dy in grid point space.
            ! G1, G2, and G3 can be reused now.
            !   dA/dy is    2*l*ac(k,l)cosknxcosly and   2*l*as(k,l)sinknxcosly and -sqrt(2)*l*az(l)sinly
            !   dB/dx is -2*k*n*bc(k,l)sinknxsinly and 2*k*n*bs(k,l)cosknxsinly

            ! 4. dB/dx:
            call DODADX(B)
            ! ================= dB/dx is now in G1

            ! 5. dA/dy:
            call DODADY(A)
            ! ================ dA/dy is now in G2

            ! 6. Multiply G1 * G2 pointwise. Store in G3. G3 now has dA/dy * dB/x in grid point space.
            do L = 1, gCurrentDims%IYMAX
                DO K = 1, gCurrentDims%IXMAX
                    gHorzArr%G3(K, L) = gHorzArr%G1(K, L) * gHorzArr%G2(K, L)
                end do
            end do

            ! 7. Subtract G3 from G4 pointwise. Store in G4. G4 now has J(A,B) in grid point space.
            do L = 1, gCurrentDims%IYMAX
                DO K = 1, gCurrentDims%IXMAX
                    gHorzArr%G4(K, L) = gHorzArr%G4(K, L) - gHorzArr%G3(K, L)
                end do
            end do


            ! ========================== G4 now has the tendencies due to J(A,B) in grid point space.
            ! Next step is to transform the tendencies back to spectral space.
            ! Get the cosly components. We do this by first computing the zonal average of G4 and
            ! placing it in gHorzArr%GZonal and taking the inverse transform with plan_y_FFTW_REDFT10
            ! We also need to remove the zonal average from the grid point data before we do the wave transforms.
            FS = 0.0    ! Zero the output tendency array
            ! Compute the zonal average:
            gHorzArr%GZonal = 0.0
            inverseValue = 1.0 / REAL(gCurrentDims%IXMAX)    ! Divisions are expensive so we create the inverse and muliply in the loop
            do L = 1, gCurrentDims%IYMAX
                DO K = 1, gCurrentDims%IXMAX
                    gHorzArr%GZonal(L) = gHorzArr%GZonal(L) + gHorzArr%G4(K, L)
                end do
                gHorzArr%GZonal(L) = gHorzArr%GZonal(L) * inverseValue
            end do
            ! Remove the zonal average from G4 while we still have it in GZonal
            do L = 1, gCurrentDims%IYMAX
                DO K = 1, gCurrentDims%IXMAX
                    gHorzArr%G4(K, L) = gHorzArr%G4(K, L) - gHorzArr%GZonal(L)
                end do
            end do
            ! Now do the inverse transform of the zonal profile
            call fftwf_execute_r2r(plan_y_FFTW_REDFT10, gHorzArr%GZonal, gHorzArr%GZonal)
            ! gHorzArr%GZonal[1] is the background average and we do not use it so the model spectral coeffs start at
            ! gHorzArr%GZonal[2]. Thus we load FS[1 : gCurrentDims%LMAX2] with gHorzArr%GZonal[2 : gCurrentDims%LMAX2 + 1]
            ! By our grid point size definition IYMAX = 3*LMAX2 + 1 we will not step out of bounds. We normalize by norm_inv_cosly
            ! as we make the copy
            FS(1 : gModIntParams%LMAX2) = gHorzArr%GZonal(2 : gModIntParams%LMAX2 + 1) * norm_inv_cosly

            ! Now to tranform the wave parts back to spectral space. Part one is to transform the sinly part
            DO K = 1, gCurrentDims%IXMAX
                ! Do the sinly transform 
                call fftwf_execute_r2r(plan_y_FFTW_RODFT10, gHorzArr%G4(K, :), gHorzArr%G4(K, :))
            END DO
            ! Now transform the periodic x part. Again we use a complex one-d array for each transform
            I = gModIntParams%LMAX2 + 1 ! Skip the cosly tendencies which we have already loaded into FS
            DO L = 1, gModIntParams%LMAX2
                ! Calculate loop-invariant values outside the inner loop
                gHorzArr%Gc = CMPLX(0.0, 0.0, kind=C_FLOAT_COMPLEX)  ! This is the input array for the transform
                call fftwf_execute_dft_r2C(plan_x_r2c_1d, gHorzArr%G4(:, L), gHorzArr%Gc)
                ! The output in gHorzArr%Gc now has the complex spectral coefficients coskx, isinkx for k=0 to k = KX/2 for the sinLy component
                ! Since we are only interested in the k wavenumber to KMAX2, (IXMAX + 2)/2 > KMAX2 we can just copy over what we need
                ! However, we need to normalize by 'norm_inv_wave'
                DO K = 2, gModIntParams%KMAX2 + 1
                    FS(I) = REAL(gHorzArr%Gc(K)) * norm_inv_wave    ! Load cosknxsinly components
                    I = I + 1
                    FS(I) = AIMAG(gHorzArr%Gc(K)) * norm_inv_wave    ! Load sinknxsinly components
                    I = I + 1
                END DO
            END DO

        end subroutine ComputeJacobianByTransform

        !> Does the transform of the d/dx part of the Jacobian calculation. The result is placed in gHorzArr%G1.
        !! @param A The spectral coefficients of the streamfunction A = (ac(k,l)cosknx + as(k,l)sinknx)sinly + az(l)cosly
        !!          Since this is the d/dx part of the Jacobian calculation the az(l) derivative is 0.
        subroutine DoDADX(A)
            use ArrayDimensionsModule
            use ModelFieldsModule, only: gHorzArr
            use ModelParamsModule, only: gModRealParams, gModIntParams
            use Fftw3HelpersModule
            use, intrinsic :: iso_c_binding, only : C_FLOAT_COMPLEX

            implicit none
            include 'fftw3.f03'
            
            REAL, intent(in) :: A(:)

            integer :: I, K, L
            REAL :: temp_calc

            !------------------------------------------------------ Start for Transform method FFTW3 MKL:
            ! Computing J(A,B)=dA/dx * dB/dy - dA/dy * dB/dx where A(k,l) = 2*(ac(k,l)*cosknx + as(k,l)*sinknx)sinly + sqrt(2)*az(l)*cosly
            !                                                  and B(k,l) = 2*(bc(k,l)*cosknx + bs(k,l)*sinknx)sinly + sqrt(2)*bz(l)*cosly

            ! 1. Load -2*k*n*ac(k,l)sinknxsinly and 2*k*n*as(k,l)cosknxsinly into the imaginary and real parts of Gc for each L
            gHorzArr%G1 = 0.0    ! Fortran short cut to zero an entire array of all dimensions. This is the output array for the transform
            temp_calc = 2. * gModRealParams%RN * norm_fwd ! Do the normalization first so we dont have to normalize after the transform
                                                         ! The FFT991 routine is such in two dimensions that to get the correct data
                                                         ! points for PS(K,L)*2*cos(kx)sin(ly) we have to divide by 2. So here we have
                                                         ! to multiply by 2.
            I = gModIntParams%LMAX2
            DO L = 1, gModIntParams%LMAX2
                gHorzArr%Gc = CMPLX(0.0, 0.0, kind=C_FLOAT_COMPLEX)  ! This is the input array for the transform
                DO K = 1, gModIntParams%KMAX2
                    ! Skip the first entry as that is the k=0 component which we do not have for either the cosknx or sinknx parts
                    gHorzArr%Gc(K + 1) = CMPLX( temp_calc * REAL(K) * A(I + 1), & ! as(k,l) cosknxsinly part
                                             -temp_calc * REAL(K) * A(I), &     ! ac(k,l) sinknxsinly part
                                             kind=C_FLOAT_COMPLEX)
                    I = I + 2
                END DO
                call fftwf_execute_dft_c2r(plan_x_c2r_1d, gHorzArr%Gc, gHorzArr%G1(:, L))
            END DO
            ! 2. Now transform the sinly part which is bounded and uses r2r transforms column by column
            DO I = 1, gCurrentDims%IYMAX
                ! Pass the column slice directly
                call fftwf_execute_r2r(plan_y_FFTW_RODFT01, gHorzArr%G1(I, :), gHorzArr%G1(I, :))
            END DO
            ! dA/dx is now in G1
        end subroutine DoDADX

        !> Does the transform of the d/dy part of the Jacobian calculation. The result is placed in gHorzArr%G2.
        !! @param A The spectral coefficients of the streamfunction A = (ac(k,l)cosknx + as(k,l)sinknx)sinly + az(l)cosly
        subroutine DODADY(A)
            use ArrayDimensionsModule
            use ModelFieldsModule, only: gHorzArr
            use ModelParamsModule, only: gModRealParams, gModIntParams, SQRT2
            use Fftw3HelpersModule
            use, intrinsic :: iso_c_binding, only : C_FLOAT_COMPLEX

            implicit none
            include 'fftw3.f03'

            REAL , intent(in) :: A(:)

            ! Local variables
            integer :: I, K, L
            REAL :: temp_calc

            !------------------------------------------------------ Start for Transform method FFTW3 MKL:
            ! Computing J(A,B)=dA/dx * dB/dy - dA/dy * dB/dx where A(k,l) = 2*(ac(k,l)*cosknx + as(k,l)*sinknx)sinly + sqrt(2)*az(l)*cosly
            !                                                  and B(k,l) = 2*(bc(k,l)*cosknx + bs(k,l)*sinknx)sinly + sqrt(2)*bz(l)*cosly

            ! 1. Load  2*l*ac(k,l)cosknxcosly and 2*l*as(k,l)sinknxcosly into the real and imaginary parts of Gc for each L
            gHorzArr%G2 = 0.0    ! Fortran short cut to zero an entire array of all dimensions
            I = gModIntParams%LMAX2
            DO L = 1, gModIntParams%LMAX2
                gHorzArr%Gc = CMPLX(0.0, 0.0, kind=C_FLOAT_COMPLEX)  ! This is the input array for the transform
                temp_calc = 2. * REAL(L) * norm_fwd ! Do the normalization first so we dont have to normalize after the transform
                DO K = 1, gModIntParams%KMAX2
                    ! Skip the first entry as that is the k=0 component which we do not have for either the cosknx or sinknx parts
                    gHorzArr%Gc(K + 1) = CMPLX( temp_calc * A(I), &     ! ac(k,l) cosknxcosly part
                                               temp_calc * A(I + 1), &  ! as(k,l) sinknxcosly part
                                             kind=C_FLOAT_COMPLEX)
                    I = I + 2
                END DO
                call fftwf_execute_dft_c2r(plan_x_c2r_1d, gHorzArr%Gc, gHorzArr%G2(:, L + 1)) ! Need to ignore the cos(0)y part. We do not have it.
            END DO
            ! 2. Now transform the cosly part which is bounded and uses r2r transforms column by column
            DO I = 1, gCurrentDims%IYMAX
                ! Pass the column slice directly
                call fftwf_execute_r2r(plan_y_FFTW_REDFT01, gHorzArr%G2(I, :), gHorzArr%G2(I, :))
            END DO

            ! 3. Zonal components: Load   -l*sqrt(2)*az(l)sinly into GZonal
            gHorzArr%GZonal = 0. ! Fortran shortcut to zero an entire array
            temp_calc = -SQRT2 * norm_sinly
            do L = 2, gModIntParams%LMAX2 + 1
                gHorzArr%GZonal(L) = -REAL(L - 1) * temp_calc * A(L - 1)  ! A(l) contains az(l) for cosly
            end do
            ! Do transform: 
            call fftwf_execute_r2r(plan_y_FFTW_RODFT01, gHorzArr%GZonal, gHorzArr%GZonal)

            ! 17. Add GZonal to G1 for every x. G1 now has dA/dy in gridpoint space.
            do L = 1, gCurrentDims%IYMAX
                DO K = 1, gCurrentDims%IXMAX
                    gHorzArr%G2(K, L) = gHorzArr%G2(K, L) + gHorzArr%GZonal(L)
                end do
            end do
            ! ================ dA/dy is now in G2
        end subroutine DODADY

        !*****************************Routines to compute the diabatic tendency terms:
        SUBROUTINE STERMS()
            use ModelParamsModule, only: gUserSettings, gModIntParams
            use ModelFieldsModule, only: gHorzArr, gVertArr
            use ArrayDimensionsModule, only: gCurrentDims
            
            implicit none

            REAL EkmanMixingLower, EkmanMixingUpper, EkmanFromAbove, EkmanFromBelow, DPS
            INTEGER N, NLEV, ITOT
            
            ! fk(n)*(sf(i, n) – ps(i,n)) + fj(n)*u(n)*(ps(i,n+1) – ps(i,n)) + fj(n-1)*v(n)*(ps(i, n-1) – ps(i, n))
            ! u(n) = 2(pp(n+1) – pp(n+2))/(pp(n) – pp(n+2))  weighting functions to account for different layer thicknesses for the ekman mixing
            ! v(n) = 2(pp(n-1) – pp(n))/(pp(n-1) – pp(n+1))  Pressures are at temperature levels or interfaces

            !----------------------------------------Compute diabatic strm terms:
            ! gHorzArr%S = tendencies of the streamfunction due to diabatic processes. This includes both the direct diabatic driving
            !              and the indirect effect of ekman dissipation and ekman mixing.
            NLEV = gModIntParams%NLEV
            ITOT = gCurrentDims%ITOT
            ! Barotropic case is a special case as there is no ekman mixing and we can skip the do loop over N and just do the calculation 
            ! for the single level.
            if (NLEV == 1) then
                ! Replaces a do-loop over I
                gHorzArr%S(1:ITOT, 1) = gVertArr%FK(1) * (gHorzArr%SF(1:ITOT, 1) - gHorzArr%PS(1:ITOT, 1))
                return
            endif

            ! Lower and upper boundaries first. Avoids a lot of if-then-elses in the loop
            ! If all layers are the same thickness, 2 * (gVertArr%PP(N + 1) - gVertArr%PP(N + 2)) / (gVertArr%PP(N) - gVertArr%PP(N + 2)) would be 1.0

            EkmanMixingLower = 2.0 * gVertArr%FJ(1) * (gVertArr%PP(2) - gVertArr%PP(3)) / (gVertArr%PP(1) + gVertArr%PP(3))
            EkmanMixingUpper = 2.0 * gVertArr%FJ(NLEV - 1) * (gVertArr%PP(NLEV - 1) - gVertArr%PP(NLEV)) / (gVertArr%PP(NLEV -1) - gVertArr%PP(NLEV + 1))
            ! Replace the do loop over ITOT
            gHorzArr%S(1:ITOT, 1) = gVertArr%FK(1) * (gHorzArr%SF(1:ITOT, 1) - gHorzArr%PS(1:ITOT, 1)) + &
                        EkmanMixingLower * (gHorzArr%PS(1:ITOT, 2) - gHorzArr%PS(1:ITOT, 1))
            gHorzArr%S(1:ITOT, NLEV) = gVertArr%FK(NLEV) * (gHorzArr%SF(1:ITOT, NLEV) - gHorzArr%PS(1:ITOT, NLEV)) + &
                        EkmanMixingUpper * (gHorzArr%PS(1:ITOT, NLEV - 1) - gHorzArr%PS(1:ITOT, NLEV))

            ! Handle levels between boundaries
            DO N = 2, gModIntParams%NLEV - 1
                EkmanFromAbove = 2.0 * gVertArr%FJ(N) * (gVertArr%PP(N + 1) - gVertArr%PP(N + 2)) / (gVertArr%PP(N) - gVertArr%PP(N + 2))
                EkmanFromBelow = 2.0 * gVertArr%FJ(N - 1) * (gVertArr%PP(N - 1) - gVertArr%PP(N)) / (gVertArr%PP(N - 1) - gVertArr%PP(N + 1))
                ! Replace the do loop over ITOT
                gHorzArr%S(1:ITOT, N) = gVertArr%FK(N) * (gHorzArr%SF(1:ITOT, N) - gHorzArr%PS(1:ITOT, N)) + &
                   EkmanFromAbove * (gHorzArr%PS(1:ITOT, N + 1) - gHorzArr%PS(1:ITOT, N)) + &
                   EkmanFromBelow * (gHorzArr%PS(1:ITOT, N - 1) - gHorzArr%PS(1:ITOT, N))
            END DO
        END subroutine STERMS

        !****************************************Compute diabatic temperature terms:
        SUBROUTINE TTERMS()
            use ModelParamsModule, only: gUserSettings, gModIntParams
            use ModelFieldsModule, only: gHorzArr, gVertArr
            use ArrayDimensionsModule
            
            REAL, DIMENSION(gModIntParams%NLEV) :: R_Upper, R_Lower

            INTEGER :: N, ITOT, NLEV

            NLEV = gModIntParams%NLEV
            ITOT = gCurrentDims%ITOT
            if (NLEV < 2) return ! SHould never be hit but just in case.

            ! 1. Pre-calculate scalar coefficients for each level (The "N" loop)
            R_Upper = 0.0
            R_Lower = 0.0

            IF (NLEV > 2) THEN
                ! N = 1 Case
                R_Upper(1) = 2.0 * gVertArr%G(1) * (gVertArr%PV(2) - gVertArr%PV(3)) / (gVertArr%PV(1) - gVertArr%PV(3))
        
                ! Intermediate Levels
                DO N = 2, NLEV - 2
                    R_Lower(N) = 2.0 * gVertArr%G(N - 1) * (gVertArr%PV(N - 1) - gVertArr%PV(N)) / (gVertArr%PV(N - 1) - gVertArr%PV(N + 1))
                    R_Upper(N) = 2.0 * gVertArr%G(N) * (gVertArr%PV(N + 1) - gVertArr%PV(N + 2)) / (gVertArr%PV(N) - gVertArr%PV(N + 2))
                END DO

                ! N = NLEV - 1 Case
                R_Lower(NLEV - 1) = 2.0 * gVertArr%G(NLEV - 2) * (gVertArr%PV(NLEV - 2) - gVertArr%PV(NLEV - 1)) / (gVertArr%PV(NLEV - 2) - gVertArr%PV(NLEV))
            END IF

            ! 2. The Heavy Lifting (Vectorized)
            DO N = 1, NLEV - 1
                ! Replace the do loop over ITOT
                gHorzArr%T(1:ITOT, N) = gVertArr%H(N) * (gHorzArr%TF(1:ITOT, N) - gHorzArr%TH(1:ITOT, N)) &
                    + R_Upper(N) * (gHorzArr%TH(1:ITOT, N + 1) * gVertArr%AU(N) - gHorzArr%TH(1:ITOT, N)) &
                    + R_Lower(N) * (gHorzArr%TH(1:ITOT, N - 1) / gVertArr%AU(N - 1) - gHorzArr%TH(1:ITOT, N))
            END DO
        END subroutine TTERMS

        !***************************************************************************
        !  Routine to convert grid point data to channel model spectral components.
        !  The input matrix is assumed to be in G3 and the output matrix
        !  is G3.  The format is the standard for the FFT991 routines where the
        !  cosnx and sinnx are stored in K and K+1 and the cosly and sinly are stored
        !  in L and L+1.  The first two columns contain the zonal components, but the
        !  second column is always zero since it is the sinn(0)x column.
        !
        !------------------------------------------We have
        !  KX=The dimension of the grid point arrays in x:
        !  gModIntParams%KMAX2,gModIntParams%LMAX2=The number of x and y waves
        !  IXMAX=The number of grid points in x IXMAX+2=KX
        !  IYMAX=The number of grid points in y (IYMAX+2=LY)
        !  IFX(13),IFY(13)=The factors arrays for FFT991
        ! SUBROUTINE InverseTransform(G3, WRK, TRY, TRX, IFX, IFY, KX, gModIntParams%KMAX2, gModIntParams%LMAX2, IXMAX, IYMAX, RML)
        SUBROUTINE InverseTransform()
            use ConstantsModule, only: INVERSE_TRANSFORM
            use ModelFieldsModule, only: gHorzArr, G3_pool
            use ModelParamsModule, only: gModIntParams
            use ArrayDimensionsModule
            !use FFT991Module, only: FFT991
            
            IMPLICIT NONE
            
            INTEGER :: L2MAXR
            INTEGER :: K, L, KCOS, KSIN, MCOS, M1, M, LSIN, MSIN, LCOS

            REAL :: AA, BA, RL, RM

            ! Zero the tails of G3
            ! Flatten the two d array. For G(K, L) with KX max x, flat = K + (L - 1) * KX
            ! K in this case is (gCurrentDims%IXMAX + 1) for cos part and (gCurrentDims%IXMAX + 2) for sin part
            DO L = 1, gCurrentDims%IYMAX + 2
                gHorzArr%G3(gCurrentDims%IXMAX + 1, L) = 0.
                gHorzArr%G3(gCurrentDims%IXMAX + 2, L) = 0.
            END DO

            DO K = 1, gCurrentDims%IXMAX + 2
                gHorzArr%G3(K, gCurrentDims%IYMAX + 1) = 0.
                gHorzArr%G3(K, gCurrentDims%IYMAX + 2) = 0.
            END DO

            ! Y-Transform for all x rows
            !CALL FFT991(gHorzArr%G3_1d, gCurrentDims%KXLYD, gHorzArr%WRK, gHorzArr%TRY, gCurrentDims%IFY, gCurrentDims%KX, 1, gCurrentDims%IYMAX, gCurrentDims%IXMAX, INVERSE_TRANSFORM)

            ! X-Transform for all y components
            L2MAXR = 4 * gModIntParams%LMAX2 + 2
            !CALL FFT991(gHorzArr%G3_1d, gCurrentDims%KXLYD, gHorzArr%WRK, gHorzArr%TRX, gCurrentDims%IFX, 1, gCurrentDims%KX, gCurrentDims%IXMAX, L2MAXR, INVERSE_TRANSFORM)
            
            !
            !  Currently, G3 has the following components after the FFT991 call: 
            ! 
            !  The cosnx siny components are in G3(2*K-1,2*L)     But one has to
            !  The sinnx siny components are in G3(2*K,2*L)       remember that the
            !  The cosnx cosy components are in G3(2*K-1,2*L-1)   0,0 components, etc
            !  The sinnx cosy components are in G3(2*K,2*L-1)     are also present.
            !
            !  The data from the tranforms has to be corrected to fit the form of the
            !  eigenfunctions for the channel model.  All the cos(0)x (cosly, sinly)
            !  terms need to be multiplied by SQRT(2).  In addition, all the sinly terms
            !  need to be negated.
            !  The (cosknx, sinknx) cos(0)y terms are okay except that the sinknx term
            !  needs to be negated.
            !  The (cosnkx, sinnkx)(cosly, sinly) terms need to be multiplied by 2.  In
            !  addition, all sin terms are negative, but then sinnkxsinly become positive.
            !  So only the cross terms need to be negated.
            !
            !  However, we do not make these corrections since some of them simply become
            !  undone in JTRN when recombined to mean strmf and mean potential temp.  The
            !  only "correction" done is to divide the (cosnKx, sinnKx)cos(0)y terms by 2.
            !  The reason we do this is because of the reprojection routine which would
            !  need to make a special check for the L=0 value.
            !
            !  Of course, by not doing these corrections here, we have to be careful in the
            !  reprojection routines.  When sinly is projected onto cosly, no factors are
            !  involved, but we have to remember that sinly is negative and cosly positive,
            !  thus we have to make the correction in the reprojection routine.
            !
            !  Here is the correction for the pure zonal wave components:
            
            KCOS = 1
            DO K = 1, gModIntParams%KMAX2
                KCOS = KCOS + 2
                KSIN = KCOS + 1
                gHorzArr%G3(KCOS, 1) = gHorzArr%G3(KCOS, 1) / 2.
                gHorzArr%G3(KSIN, 1) = gHorzArr%G3(KSIN, 1) / 2.
            END DO

            !--------------------- The reprojection routine:
            !
            !  This is the extra loop which takes each cosy structure and projects it onto
            !  sinly eigenfunctions.    COSMY=(4L/(L*L-M*M))PI SINLY
            !  We also speed up this routine by computing beforehand the 4/(L*L-M*M)PI
            !  values.  We had to have twice the range for the M values than the
            !  L values since we retain LMAXR values which must be transformed over to
            !  compatible eigenfunctions, but retain only gModIntParams%LMAX2 eigenfunctions.  For example
            !  cos2y is decomposed onto siny (and sin3y...) but not sin2y.  Thus we need
            !  M=2 but only L=1, since the cos2y comes from siny*siny and cosy*cosy.
            KCOS = 1
            DO K = 1, gModIntParams%KMAX2
                KCOS = KCOS + 2
                KSIN = KCOS + 1
                !  The extra range on M is needed to project the (cosknx,sinknx) cos(0)y terms.
                MCOS = -1
                DO M = 1, 2 * gModIntParams%LMAX2 + 1
                    MCOS = MCOS + 2
                    M1 = MOD(M - 1, 2) + 1
                    !-----------------------------------------  Get coskx cosy components:
                    AA = gHorzArr%G3(KCOS, MCOS)
                    !-----------------------------------------  Get sinkx cosy components:
                    BA = gHorzArr%G3(KSIN, MCOS)
                    DO L = M1, gModIntParams%LMAX2, 2
                        LSIN = 2 * L + 2
                        RL = REAL(L)
                        !  Place each cosly component in all the siny components.  (The ORIGINAL
                        !  expression has a minus sign in front of A and B, but to keep all cos sin
                        !  combinations negative, the positive "A" must be negated, giving plus.  With
                        !  the sin sin, which must be positive, "B" must be negated, making also plus.
                        !  The components we are interested in also are off by a factor of two, but the
                        !  pure zonal wave components are not (reached when MCOS=1) which is why we
                        !  divided these components by two in the loop above.  Are you confused?)
                        gHorzArr%G3(KCOS, LSIN) = gHorzArr%G3(KCOS, LSIN) + AA * RL * gHorzArr%RML(M, L)
                        gHorzArr%G3(KSIN, LSIN) = gHorzArr%G3(KSIN, LSIN) + BA * RL * gHorzArr%RML(M, L)
                    END DO
                END DO
            END DO

            !------------------------------ Zonal part ----------------------------------
            !
            !  Since all products in the Jacobian which give zonal structure come from the
            !  advection of one wave by another, the structure will always be zero at the
            !  boundaries.  Thus the DIRECT result is one of pure sinly structure, which
            !  then must be projected onto cosly.  No pure cosy structures result from the
            !  Jacobian advective process itself.  Nevertheless, we take the real part of
            !  the transform to get the cosy parts for a more general routine which can be
            !  used from things like local disturbance generators.
            !
            !  This is the extra loop which takes each sinly component and projects it onto
            !  cosy components.  SINMY=(4M/(M*M-L*L)) COSLY
            MSIN = 2
            DO M = 1, 2 * gModIntParams%LMAX2
                MSIN = MSIN + 2
                M1 = MOD(M, 2) + 1
                RM = M
                !---------------------------------------------------  Get the sinly component:
                BA = gHorzArr%G3(1, MSIN) * RM
                DO L = M1, gModIntParams%LMAX2, 2
                    LCOS = 2 * L + 1
                    !  Here again the sinly is negative to be projected onto cosly which is
                    !  positive.  Thus the B term in the expression below is now negative.
                    !  (Originally it was positive.)

                    gHorzArr%G3(1, LCOS) = gHorzArr%G3(1, LCOS) - BA * gHorzArr%RML(M + 1, L)
                END DO
            END DO

        END SUBROUTINE InverseTransform

        !********************************************Subroutine to compute increments:
        !  Routine increments either strm, vertical diabatic arrays or RN or BETA
        !  given R so the model can be rerun. 'I' gives which quantity (1-9).
        !  PRF is the start and end profile for the incrementing:
        !
        SUBROUTINE PROFIL(PRF, R, II)
            use ModelFieldsModule
            use ModelParamsModule

            implicit none
            !--------------------------------------------------------------
            !  PS(1,1,1)=PS      PV(1,1)=PV
            !  PS(1,1,2)=TH      PV(1,2)=DP
            !  PS(1,1,3)=WW      PV(1,3)=DPI
            !  PS(1,1,4)=TF      PV(1,4)=THS
            !  PS(1,1,5)=FS      PV(1,5)=FK
            !  PS(1,1,6)=ADS     PV(1,6)=FJ
            !  PS(1,1,7)=WS      PV(1,7)=AU
            !  PS(1,1,8)=S       PV(1,8)=A
            !  PS(1,1,9)=FT      PV(1,9)=H
            !  PS(1,1,10)=ADT    PV(1,10)=G
            !  PS(1,1,11)=WT     PV(1,11)=SIG
            !  PS(1,1,12)=T      PV(1,12)=X
            !  PS(1,1,13)=SF     PV(1,13)=Y
            !  PS(1,1,14)=ADO    PV(1,14)=PRF(1,1)
            !  PS(1,1,15)=TOP    PV(1,15)=PRF(1,2)
            !                    PV(1,16)=PRF(1,3)
            !                    PV(1,17)=PRF(1,4)
            real, intent(inout) :: PRF(:)
            real, intent(in) :: R
            INTEGER, intent(in) :: II
        !-----------------------------------------------Local variables:
            INTEGER I, J, K, N, M
            integer :: addressToPRF1_2, addressToPRF1_3, addressToPRF1_4
            REAL X, Y

            I = II
            addressToPRF1_2 = gUserSettings%maxLevels
            addressToPRF1_3 = gUserSettings%maxLevels + gUserSettings%maxLevels
            addressToPRF1_4 = gUserSettings%maxLevels + addressToPRF1_3


            !------------------------------------------New profile with PSSAVE restart:
            IF (II .GT. 20) I = II - 20     ! Brings options 21-31 down to 1 - 11 (SENS_CHANNEL_LENGTH to SENS_OROGRAPHIC_HEIGHT)
            !-----------------------------------------------Do RN and BETA options
            !IF(I .EQ. 1) VAR(I) = PRF(1, 1) * PRF(1, 2) / (PRF(1, 2) + R * (PRF(1, 1) - PRF(1, 2))) original 2-d addressing
            IF (I .EQ. 1) gModRealParams%RN = gVertArr%PRF(1) * gVertArr%PRF(addressToPRF1_2) / (gVertArr%PRF(addressToPRF1_2) + R * (gVertArr%PRF(1) - gVertArr%PRF(addressToPRF1_2)))
            IF (I .EQ. 2) gModRealParams%BETA = gVertArr%PRF(1) + R * (gVertArr%PRF(addressToPRF1_2) - gVertArr%PRF(1))
            !-----------------------------------------------Do  profiles:
            IF (I .GE. 3 .AND. I .LE. 11) THEN
                J = gModIntParams%LMODE
                IF (gModIntParams%KMODE .GT. 0) J = (2 * gModIntParams%KMODE - 1) * gModIntParams%LMAX2 + 2 * J
                M = gModIntParams%NLEV
                IF (I .EQ. 11) M = 1
                DO N = 1, M
                    !-----------------------------------------------Compute increment:
                    IF (I .GE. 4 .AND. I .LE. 7) THEN
                        X = (gVertArr%PRF(N + addressToPRF1_2) + R * (gVertArr%PRF(N) - gVertArr%PRF(N + addressToPRF1_2)))
                        IF (X .NE. 0.) X = gVertArr%PRF(N) * gVertArr%PRF(N + addressToPRF1_2) / X
                        !-----------------------------------------------Load it in FK,FJ,H,G profile:
                        IF (I .EQ. 4) gVertArr%FK(N) = X  !PV(N, I+1) = X 
                        IF (I .EQ. 5) gVertArr%FJ(N) = X
                        IF (I .EQ. 6) gVertArr%H(N) = X   !PV(N, I+3) = X
                        IF (I .EQ. 7) gVertArr%G(N) = X
                    ELSE
                        X = gVertArr%PRF(N) + R * (gVertArr%PRF(N + addressToPRF1_2) - PRF(N))
                        !-----------------------------------------------Load it in THS profile:
                        IF(I .EQ. 3) gVertArr%PV(N + addressToPRF1_4) = X
                        !-----------------------------------------------Load spectral profiles:
                        IF (I .GE. 8) THEN
                            !-----------------------------------------------Load it in PS
                            IF (I .EQ. 8) gHorzArr%PS(J, N) = X    !K=1
                            !-----------------------------------------------Load it in TF
                            IF (I .EQ. 9) gHorzArr%TF(J, N) = X    !K=4
                            !-----------------------------------------------Load it in SF
                            IF (I .EQ. 10) gHorzArr%SF(J, N) = X   !K = 13
                            !-----------------------------------------------Load it in TOP
                            IF (I .EQ. 11) gHorzArr%TOP(J) = X  !K = 15
                                                          !PS(J, N, K) = X
                            !-----------------------------------------------Get phase:
                            IF (gModIntParams%KMODE .GE. 1) THEN
                                Y = (gVertArr%PRF(N + addressToPRF1_3) + R * (gVertArr%PRF(N + addressToPRF1_4) - gVertArr%PRF(N + addressToPRF1_3))) / RAD_TO_DEGREES
                                IF (I .EQ. 8) Then
                                    gHorzArr%PS(J - 1, N) = X * COS(Y)
                                    gHorzArr%PS(J, N) = X * SIN(Y)
                                end IF
                                IF (I .EQ. 8) Then
                                    gHorzArr%TF(J - 1, N) = X * COS(Y)
                                    gHorzArr%TF(J, N) = X * SIN(Y)
                                end IF
                                IF (I .EQ. 8) Then
                                    gHorzArr%SF(J - 1, N) = X * COS(Y)
                                    gHorzArr%SF(J, N) = X * SIN(Y)
                                end IF
                                IF (I .EQ. 8) Then
                                    gHorzArr%TOP(J - 1) = X * COS(Y)
                                    gHorzArr%TOP(J) = X * SIN(Y)
                                end IF
                            ENDIF
                        ENDIF
                    ENDIF
                END DO
            ENDIF
        END SUBROUTINE PROFIL

        SUBROUTINE PICK(DAT, PS, N, IP)
            use ArrayDimensionsModule
            use ModelParamsModule, only: gUserSettings

            implicit none

            INTEGER, intent(in) :: IP
            INTEGER, intent(in) :: N
            REAL, intent(inout) :: DAT(:)
            real, intent(in) :: PS(:, :)

            ! local variables
            INTEGER :: I, K

            K = (IP - 2) * gUserSettings%maxLevels + N
            IF(IP .GE. 14) K = 12 * gUserSettings%maxLevels + IP - 13
            !----------------------------------Special case for vorticity:
            IF(IP .EQ. 1) THEN
                K = N
                DO I = 1, gCurrentDims%ITOT
                    DAT(I) = -PS(I, K) * DAT(I + gCurrentDims%ITOTA)
                END DO
            ELSE
                DO I = 1, gCurrentDims%ITOT
                   DAT(I) = PS(I, K)
                END DO
            ENDIF
        END subroutine PICK

        SUBROUTINE TRANS(G, DT, WK, TX, TM)
            use ConstantsModule, only: FORWARD_TRANSFORM
            use ArrayDimensionsModule
            use ModelParamsModule, only: gModIntParams, SQRT22
            
            !-------------------------------------------------Variables passed
            !  G,TM=Grid point array of output data
            !  WK space array for FFT991
            !  DT data array to transform
            !  TX,TY=Trig arrays
            !-------------------------------------------Data arrays:
            REAL*4 DT(1)
            !-------------------------------------------Transform data TY=TX(ITRXD+1):
            REAL, intent(inout) :: G(:), WK(:), TX(:), TM(:)
            
            ! local variables            
            INTEGER :: I, I1, K, K21, L, L21, L22, ITY

            !-------------------------------------Convert resolution to INTEGER*4:

            ITY = gCurrentDims%ITRXD + 1
            G = 0.0     ! CALL RZERO(G, gCurrentDims%KX * gCurrentDims%LY)
            TM = 0.0    ! CALL RZERO(TM, gCurrentDims%LY)
            !------------------------------------ Load the spectral components into GRID1:
            L21 = 1
            DO L = 1, gModIntParams%LMAX2
                L21 = L21 + 2
                TM(L21) = SQRT22 * DT(L)    ! SQRT22 defined in ModelParamsModule
            END DO
            I = gModIntParams%LMAX2
            !---------------------------------------Start data in 4th row:
            K21 = 3 * gCurrentDims%KX + 1
            DO K = 1, gModIntParams%KMAX2
                K21 = K21 + 2
                L22 = K21
                DO L = 1, gModIntParams%JY1
                    !--------------------------------------- I keeps count of the wave components:
                    I = I + 1
                    G(L22)= -DT(I) / 2.
                    I = I + 1
                    G(L22 + 1) = DT(I) / 2.
                    L22 = L22 + 2 * gCurrentDims%KX
                END DO
            !----------------------------------------Skip 1 row (the cosy row)
            END DO
            !----------------------------------------------------------------------------
            !  Transform the eigenfunctions into grid point space; The x-transforms first:
            !  knowing that all the cosy components are zero

            !CALL FFT991(G((3 * gCurrentDims%KX + 1) :), WK, TX, gCurrentDims%IFX, 1, 2 * gCurrentDims%KX, gCurrentDims%IXMAX, gModIntParams%LMAX2, FORWARD_TRANSFORM)
            !CALL FFT991(G, WK, TX(ITY :), gCurrentDims%IFY, gCurrentDims%KX, 1, gCurrentDims%IYMAX, gCurrentDims%IXMAX, FORWARD_TRANSFORM)
            !-----------------------------------------------  Now do the zonal components:
            !CALL FFT991(TM, WK, TX(ITY:), gCurrentDims%IFY, 1, gCurrentDims%LY, gCurrentDims%IYMAX, 1, FORWARD_TRANSFORM)
            !--------------------  Add the results to G: (Only half for plotting, however)
            I1 = 0
            DO L = 1, gCurrentDims%IYH
                DO K = 1, gCurrentDims%IXMAX
                    G(K + I1) = G(K + I1) + TM(L)
                END DO
                G(gCurrentDims%IXMAX1 + I1) = G(I1 + 1)
                !--------------------------------------------Move to next row:
                I1 = I1 + gCurrentDims%KX
            END DO
        END

        !******************************************** Subroutine QG for Amp-G values:
        SUBROUTINE ComputeGQ()      !(DAT,PS,DPI,GQ,GQA,PSA,PSS)
            use ArrayDimensionsModule
            use ModelFieldsModule, only : gHorzArr, gVertArr, gStatsArr
            use ModelParamsModule, only: gModIntParams, gModRealParams, gUserSettings
            
            implicit none
        !---------------------------------------------------Computation:
        !  The vertical mean flow is computed at each time step and the thickness
        !  to each vertical level is then computed.  G and Q are computed upon this
        !  basis.  The quantity stored is PS*TH(p)*cosG(p) and PS*TH(p)sinG(p).
        !  These quantities will be stored in GQ(ITOT,NLEV).  The barotropic flow
        !  will be stored in GQA(ITOT,1).  The depth or DPI values are inverse and
        !  normalized by the model Depth. So we don't need to do a division.

        !  For the structures of the remaining nonlinear fields, we can use a similar
        !  method.

            !REAL*4 DAT(1),PS(ITOTD,NALL,1),DPI(1),GQ(ITOTD,1),GQA(1)
            !REAL*4 PSA(ITOTA,NALA,7),PSS(ITOTA,NALA,7)
            
            ! local variables
            INTEGER J, K, M, N
            REAL THR, THI, THR1, THI1
            real, dimension(:,:), pointer :: field_ptr

            !-----------------------------------------Skip GQ if MODEL=2 Option on:
            IF(gModIntParams%modelType .NE. 2) THEN
                !-----------------------------------------Compute barotropic flow:
                gHorzArr%DAT = 0.0       ! Set entire DAT array to zero
                DO N = 1, gModIntParams%NLEV
                    ! Address an array linearly: J = K + (N - 1) * gCurrentDims%ITOTD for PS(K, N, 1) to PS(J, 1)
                    ! But we dont need to do this. Not sure why the original code did it as a 3-D array.
                    DO K = 1, gCurrentDims%ITOT
                        gHorzArr%DAT(K) = gHorzArr%DAT(K) + gHorzArr%PS(K, N) / gVertArr%DPI(N)
                    END DO
                END DO
                !----------------------Compute the Amp*Th*cosG quantities:
                DO N = 1, gModIntParams%NLEV
                    DO K = 1, gModIntParams%LMAX2
                        gHorzArr%GQ(K, N) = gHorzArr%GQ(K, N) + gHorzArr%PS(K, N) - gHorzArr%DAT(K)
                    END DO
                    DO K = gModIntParams%LMAX2 + 1, gCurrentDims%ITOT, 2
                        THR = gHorzArr%PS(K, N) - gHorzArr%DAT(K)
                        THI = gHorzArr%PS(K + 1, N) - gHorzArr%DAT(K + 1)
                        gHorzArr%GQ(K, N) = gHorzArr%GQ(K, N) + gHorzArr%DAT(K) * THR + gHorzArr%DAT(K + 1) * THI
                        gHorzArr%GQ(K + 1, N) = gHorzArr%GQ(K + 1, N) + gHorzArr%DAT(K + 1) * THR - gHorzArr%DAT(K) * THI
                    END DO
                END DO
                !-------------------------------------Compute amplitudes of barotropic flow:
                DO K = 1, gModIntParams%LMAX2
                    gHorzArr%BTA(K) = gHorzArr%BTA(K) + gHorzArr%DAT(K)
                END DO
                DO K = gModIntParams%LMAX2 + 1, gCurrentDims%ITOT, 2
                    gHorzArr%BTA(K) = gHorzArr%BTA(K) + SQRT(gHorzArr%DAT(K + 1) * gHorzArr%DAT(K + 1) + gHorzArr%DAT(K) * gHorzArr%DAT(K))
                END do
            end IF
            !=====================================Computing structures of all qnt:
            !  We compute the phase relative to the position of the surface streamfunction
            !  for all quantities. Thus G is defined as that phase difference.
            !  AMPSTRM(1)*AMP(N)sinG(N) = PS(1)r*PS(N)s - PS(1)s*PS(N)r
            !  AMPSTRM(1)*AMP(N)cosG(N) = PS(1)r*PS(N)r + PS(1)s*PS(N)r
            !  For level 1 we get the amplitude of level #1 squared.
            !-------------------------------------------------Locate PS data for means:
            ! Original chained location of variables:
            !PS variable 1
            !TH variable 2
            !WW variable 3
            !TF variable 4
            !FS variable 5
            !ADS variable 6
            !WS variable 7
            !S variable 8
            !FT variable 9
            !ADT variable 10
            !WT variable 11
            !T variable 12
            !SF variable 13
            !ADO variable 14
            !L(1) = 1
            !L(2) = 3
            !L(3) = 5
            !L(4) = 6
            !L(5) = 9
            !L(6) = 10
            !L(7) = 14
            DO M = 1, 7
                ! The => operator allows us to point to the PS array
                if (M .EQ. 1) call DoubleLoops(gHorzArr%PS, M)  ! 1 is PS
                if (M .EQ. 2) call DoubleLoops(gHorzArr%WW, M)  ! 2 is WW
                if (M .EQ. 3) call DoubleLoops(gHorzArr%FS, M)  ! 3 is FS
                if (M .EQ. 4) call DoubleLoops(gHorzArr%ADS, M) ! 4 is ADS
                if (M .EQ. 5) call DoubleLoops(gHorzArr%FT, M)  ! 5 is FT
                if (M .EQ. 6) call DoubleLoops(gHorzArr%ADT, M) ! 6 is ADT
                !if (M .EQ. 7) call DoubleLoops(gHorzArr%ADO) ! 7 is ADO
            END DO
        END subroutine ComputeGQ
        
        subroutine DoubleLoops(field, M)
            use ArrayDimensionsModule
            use ModelFieldsModule, only : gHorzArr, gStatsArr
            use ModelParamsModule, only: gModIntParams
            
            implicit none
            
            REAL, intent(in) :: field(:, :)
            INTEGER, intent(in) :: M
            
            ! local variables
            REAL :: THR1, THI1, THR, THI
            integer :: N, K
            DO N = 1, gModIntParams%NLEV
                DO K = 1, gModIntParams%LMAX2
                    gStatsArr%PSA(K, N, M) = gStatsArr%PSA(K, N, M) + field(K, N)
                    gStatsArr%PSS(K, N, M) = gStatsArr%PSS(K, N, M) + field(K, N)
                END DO
                DO K = gModIntParams%LMAX2 + 1, gCurrentDims%ITOT, 2
                    !--------------------------------Get the real & imag part of srfc strm:
                    THR1 = gHorzArr%PS(K, 1)
                    THI1 = gHorzArr%PS(K + 1, 1)
                    !--------------------------------Get the correct real & imag part for STRUC:
                    THR = field(K, N)
                    THI = field(K + 1, N)
                    gStatsArr%PSS(K, N, M) = gStatsArr%PSS(K, N, M) + THR * THR1 + THI * THI1
                    gStatsArr%PSS(K + 1, N, M) = gStatsArr%PSS(K + 1, N, M) + THI * THR1 - THR * THI1
                    !------------------------------------------------Normal averges:
                    gStatsArr%PSA(K, N, M) = gStatsArr%PSA(K, N, M) + field(K, N)
                    gStatsArr%PSA(K + 1, N, M) = gStatsArr%PSA(K + 1, N, M) + field(K + 1, N)
                END DO
            END DO
        end subroutine DoubleLoops


    end module ModelModule