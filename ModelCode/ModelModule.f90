! This is the module that contains all the model integration time stepping code
module ModelModule
    implicit none

    public :: InitModelWithModelControlVals     ! Called after user enters the fields wanted in the ModelControlDialog and then has selected the option
                                                ! to start running the model (F7 or F8)
    public :: StartTimeLoop
    public :: GQSET
    public :: ComputeAKLTerms
    public :: EquationSystem
    public :: NONLIN
    public :: ComputeJacobianByTransform
    public :: DoDADX
    public :: DoDADY
    public :: STERMS
    public :: TTERMS
    public :: PROFIL
    public :: PICK
    
    ! Supports the Sleep() subroutine
    interface
        subroutine Sleep(ms) bind(C, name='Sleep')
            use, intrinsic :: iso_c_binding
            integer(c_int), value :: ms
        end subroutine Sleep
    end interface

    contains

        !> The user has configured the values in the ModelControl dialog and has selected to run the model.
        !> After some basic idiot checks, this function creates those fields needed to run the time loop given the
        !> user values. It also does some further idiot checking like the scaling parameters not being zero.
        function InitModelWithModelControlVals() result(errorCode)
            use ConstantsModule, only: ZONAL_MEAN_FLOW, ERR_SCALING_PARAMETER_ZERO
            use ArrayDimensionsModule, only: gCurrentDims
            use ModelFieldsModule, only: gHorzArr, gVertArr
            use ModelParamsModule, only: gContourIntervals, gModIntParams, gModRealParams
            use InteractionCoefficientsModule, only: computeInteractionCoefficients
            implicit none
            
            ! Local variables
            integer :: errorCode
            integer :: I, N, numberOfCoefs

            errorCode = 0

            ! This precomputes the del2 operation on each component and its inverse and places
            ! them in the arrays gHorzArr%AKL(ITOT) and gHorzArr%AKLI(ITOT)
            CALL ComputeAKLTerms()

            !===========================================================Zonal model type setup:
            IF(gModIntParams%modelType .EQ. ZONAL_MEAN_FLOW) THEN
                !----------------------------Create QcosG and QsinG and place in WW for waves:
                call GQSET()        ! In this file
                !----------------------------Initialize barotropic mean flow:       DO I=LMAX2+1,ITOT
                DO I = gModIntParams%LMAX2 + 1, gCurrentDims%ITOT
                    gHorzArr%PSB(I) = 0.
                    DO N = 1, gModIntParams%NLEV
                        gHorzArr%PSB(I) = gHorzArr%PSB(I) + gHorzArr%PS(I, N) / gVertArr%DPI(N)
                    END DO
                END DO
            ENDIF
            ! Check that none of the scaling parameters are zero
            ! These parameters are used only in plotting
            DO I = 1, 23
                IF(gContourIntervals%Contours(I) .EQ. 0.) then
                    errorCode = ERR_SCALING_PARAMETER_ZERO
                    return
                end if
            END DO
        end function InitModelWithModelControlVals

        function StartTimeLoop() result(errorCode)
            use ConstantsModule, only: ADV_BETA_HGT_TEND, RUNNING, SENS_STATIC_LAPSE_RATE, STATS_OPT_NONE, STATS_OPT_VAR, &
                ZONAL_MEAN_FLOW, BAROCLINIC_STRUCTS, STOPPED, PAUSED, ERR_NUMERIC_INSTABILITY
            use ArrayDimensionsModule
            use ModelParamsModule, only: gUserSettings, gModRealParams, gModIntParams, gScaleFactors, FileStruct, gPlotScales, gModelState, &
                gContourIntervals
            use ModelFieldsModule, only: gHorzArr, gVertArr, gStatsArr, G2_pool, G3_pool, ZeroHorzArrays, ZeroVerticalArrays
            use SetUpModule, only: WriteSensFileHeader, SetDimensions, ComputeVerticalProfiles, WriteFileData, CreatePerturbation, CreateBomb, DFDIAB
            use InteractionCoefficientsModule
            use NLevQGModelGlobals, only: ghwndMain, LogMessage
            use iso_c_binding
            use UtilitiesModule, only: openModelFile
            use PlotModule, only: outputPlot, PumpMessages
            use WelfordModule, only: updateRunStats, gWelford
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

            ! This can only be used if the number of interaction coefficients is small
            !write(msg, '(*(I2, 1X))') (gCintArr%JIN(K), K = 1, gCintArr%numOfInteractionCoeffs)
            !call LogMessage(msg)
            !write(msg, '(*(I2, 1X))') (gCintArr%KIN(K), K = 1, gCintArr%numOfInteractionCoeffs)
            !call LogMessage(msg)


            errorCode = 0
            gModelState%runState = RUNNING
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
            ISPIN = INT(gModRealParams%SPINUP * 1440. / (gScaleFactors%timeScale * gModRealParams%DELT))
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
                            CALL ComputeAKLTerms()           ! Subroutine is in this file
                            RNP = gModRealParams%RN
                        END IF
                        !-----------------------------------------Static stability? Check if changed
                        IF(gModIntParams%IPRM1 .EQ. SENS_STATIC_LAPSE_RATE .OR. gModIntParams%IPRM2 .EQ. SENS_STATIC_LAPSE_RATE) CALL ComputeVerticalProfiles()
                        !---------------------------------------------Set convergence-test values:
                        AMPP = 0.
                        PAMPP = 0.
                    END IF

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
                    RD = 0.0
                    DDELT = 4. * gModRealParams%DELT
                    if (DDELT .GT. 0.0) then
                        RD = 1. / gModRealParams%DELT
                        RDD = 1. / DDELT
                    endif
                    ITOTS = 1
                    !--------------------------------- Turn off integration of zonal fields:
                    IF(gModIntParams%modelType .EQ. BAROCLINIC_STRUCTS) ITOTS = gModIntParams%LMAX2 + 1
                    ITOTB = gCurrentDims%ITOT
                    IF(gModIntParams%modelType .EQ. ZONAL_MEAN_FLOW) ITOTB = gModIntParams%LMAX2
                    !--------------------------------------------------- Actual beginning of loop:
                    DO KCYC = 1, gModIntParams%ICYC
                        DO KSTEP = 1, gModIntParams%ISTEP
                            ! timeStepsToDelay, if set to 0 will skip generating a bomb. 
                            ! If set to 1, it will get invoked on the first step. In that case there is no need to
                            !   to create the bomb, it has already been created and it is part of the PS variables.
                            !   In that manner, several different scaled bombs can be superimposed upon one another.
                            ! If greater than 1, only a single perturbation is added to the fields at the time the
                            !   the time step is reached. Here the CreateBomb is created.
                            ! In all cases, the timeStepsToDelay is set to its negative so it does not get called 
                            ! again until the user resets it.
                            if (gModIntParams%timeStepsToDelay == KSTEPT) then
                                if (KSTEPT > 1) call CreateBomb()
                                gModIntParams%timeStepsToDelay = -gModIntParams%timeStepsToDelay ! Have to reset to repeat
                            endif
                            !---------------------------------------KSTEPT=Total # of current time steps:
                            KSTEPT = KSTEPT + 1
                            !write(*, '(A, I4, A, I4, A, I4)') 'KSTEPT=', KSTEPT, ' KSTEP=', KSTEP, ' KCYC=', KCYC
                            !write(msg, '(A, I4, A, I4, A, I4)') 'KSTEPT=', KSTEPT, ' KSTEP=', KSTEP, ' KCYC=', KCYC
                            !call LogMessage(msg)

                            ! Checking if user stopped model
                            if (gModelState%runState == STOPPED) return
                            ! Handling user pausing the model
                            do while (gModelState%runState == PAUSED)
                                CALL PumpMessages()
                                call sleep(100) ! Suspends the thread for 100ms. 0% CPU usage.
                            end do
                            !---------------------------------------------- Selecting when to output data:
                            if (MOD(KSTEP - 1, gModIntParams%outputInterval) == 0) then
                                call ComputeZonalSpectrum()
                                call ComputeStructures()
                                !do N = 1, gModIntParams%NLEV
                                !    call updateRunStats(gEnergyArr%GQ(:, N), gWelford)
                                !end do
                                call outputPlot(.FALSE.)
                                gHorzArr%FS = 0.0
                            endif

                            !*************************************************************** N-Cycle loop:
                            A1 = 0.
                            B1 = RD
                            DO NCYC4 = 1, 4
                                call EquationSystem(buffer)
                                if (RD == 0.0) cycle        ! Case for a time step of zero
                                ! FS() has streamfunction tendencies after this call
                                ! ADS() has vorticity plus beta tendencies after this call
                                ! ADT() has temperature tendencies after this call
                                !============================================================ Integration:
                                B1N = 1. / B1
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
                                        !print *, "Streamfunction values: ", gHorzArr%PS(:, N)
                                        !print *, "Temperature values:    ", gHorzArr%TH(:, N)
                                        call ZeroHorzArrays(.true.) ! Don't zero settings like TF, SF, TOP, AKL, etc.
                                        !call DFDIAB(.false.)
                                        call CreatePerturbation()
                                        gModelState%runState = STOPPED
                                        errorCode = ERR_NUMERIC_INSTABILITY
                                        return
                                    ENDIF
                                END DO
                                B1 = B1 - RDD
                                A1 = A1 - RDD
                            END DO !==============================================================End of N-cycle:
                            !call plotDiagnostic(gHorzArr%G3, gContourIntervals%Contours(ADV_BETA_HGT_TEND), gPlotScales(ADV_BETA_HGT_TEND))
                            !write(msg, '(A, I4, A, (F10.6, 1X), A, *(F10.6, 1X))') ' KSTEP=', KSTEP, ': ', gHorzArr%PS(1, (gModIntParams%NLEV/2 + 1)), ' ', (gHorzArr%PS(i, (gModIntParams%NLEV/2 + 1)), i = 1 + gModIntParams%LMAX2, gCurrentDims%ITOT, 2 * gModIntParams%LMAX2)
                            !call LogMessage("Values: " // msg)
                        END DO ! Inner loop ISTEP done
                    END DO ! Outer loop ICYCL done   Total number of time steps = ISTEP * ICYCL 
                    call LogMessage("Time loop done")
                    gModelState%runState = STOPPED

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
            use ArrayDimensionsModule, only: gCurrentDims
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

        !****************************** Precompute ARRAYs AKL and AKLI for the del2 operator:
        ! The Array AKL and AKLI are for the del2 operator which gives a**2 = k*k*rn*rn + l*l.
        ! IN many places we divide by a**2. Since division is expensive, we precompute 1/a**2
        ! and AKLI(I) = 1/AKL(I). Recall we store the waves as all zonal only structs in the
        ! first LMAX elements and then cosnkx*sinly and sinnkx*sinly for all l for each k.
        SUBROUTINE ComputeAKLTerms()
            use ModelFieldsModule, only: gHorzArr
            use ModelParamsModule, only: gModIntParams, gModRealParams

            implicit none

            ! Local variables
            INTEGER :: I, K, L
            REAL :: R

            !----------------------------------------Compute AKL and AKLI:
            DO L = 1, gModIntParams%LMAX2
                gHorzArr%AKL(L) = REAL(L * L)
                gHorzArr%RL(L) = REAL(L)
            END DO

            I = gModIntParams%LMAX2
            DO K = 1, gModIntParams%KMAX2
                R = gModRealParams%RN * gModRealParams%RN * REAL(K * K)
                gHorzArr%RK(K) = REAL(K)
                DO L = 1, gModIntParams%LMAX2
                    I = I + 1
                    gHorzArr%AKL(I) = R + REAL(L * L)
                    I = I + 1
                    gHorzArr%AKL(I) = gHorzArr%AKL(I - 1)
                END DO
            END DO
            ! Compute the array of inverses
            gHorzArr%AKLI = 1. / gHorzArr%AKL
        END subroutine ComputeAKLTerms

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
            use ConstantsModule, only: ZONAL_MEAN_FLOW, RAD_TO_DEGREES
            use ModelParamsModule, only: gModIntParams, gModRealParams, gModelState
            use ArrayDimensionsModule, only: gCurrentDims
            use ModelFieldsModule, only: gHorzArr, gVertArr
            use, intrinsic :: ieee_arithmetic

            implicit none

            real, intent(inout) :: buffer(:)

            !------------------------------------------Local variables:
            INTEGER :: I, K, L, N, NP, NT, ITOT
            REAL :: AX, BRN, R, R1
            REAL :: C, Clower, Cupper, gamma, gammaLower, alpha, alpha1 ! Terms for iterative omega loop
            logical :: notHit

            !-------------------------------------------------Order of events:
            !  First we compute the nonlinear terms by either transforms or interactions,
            !  the orographic term has to be computed at level one.
            !  Then we compute the beta effect and add to vorticity advections.
            !  Next we compute the diabatic terms in STERMS and TTERMS
            !  Then we do the iterative proceedure to compute omega.
            !  Lastly we get the tendency for the streamfunction at each level.

            !----------------------------------------Compute Orographic influence:
            !! Doing Jacobian J(A, B) = dA/dx*dB/dy - dA/dy * dB/dx
            !! A = gHorzArr%PS(:, 1)
            !! B = gHorzArr%TOP
            gHorzArr%ADO = 0.0

            CALL NONLIN(gHorzArr%ADO, gHorzArr%PS(:, 1), gHorzArr%TOP)

            !---------------------------------------------Normalize by AKL:
            ! This is needed since the d(del**2(PS))/dt = -J(PS, TOP) + other terms including further jacobians. The del-squared operation gives the AKL factor
            ! dPS(k,l)/dt = ADO(k,l)/AKL(k,l) where ADO = J(PS,TOP)
            ! ADO is the tendency due to orography 
            !----------------------------------------Normalize by AKL:
            ITOT = gCurrentDims%ITOT
            gHorzArr%ADO(1:ITOT) = gHorzArr%ADO(1:ITOT) * gHorzArr%AKLI(1:ITOT) ! No minus sign here

            !-----------------------------------------Compute vorticity advection:
            DO N = 1, gModIntParams%NLEV
                !----------------------------------------Compute vorticity:
                buffer(1:ITOT) = -gHorzArr%AKL(1:ITOT) * gHorzArr%PS(1:ITOT, N)

                !dPS(k,l)/dt = ADS(k,l, n)/AKL(k,l) where ADS = J(PS,vorticity)
                CALL NONLIN(gHorzArr%ADS(:, N), gHorzArr%PS(:, N), buffer)

                !----------------------------------------Normalize by AKL:
                gHorzArr%ADS(1:ITOT, N) = gHorzArr%ADS(1:ITOT, N) * gHorzArr%AKLI(1:ITOT)
            END DO

            !---------------------------------------Add Beta effect to ADS:
            ! J(PS, beta*y) = (d/dx)(PS) * (d/dy)(beta*y)  = (d/dx)(PS) * beta  since d/dx of beta*y is 0.
            ! This means that (d/dx)cosknx gives -knsinkxn and (d/dx)sinknx = knxcosknx 
            ! so the ps(i+1) term gives a constribution R1 below on ads(i) term and the ps(i) term gives a -R1 contribution on the ads(i+1) term
            IF(gModRealParams%BETA .NE. 0.) THEN
                I = gModIntParams%LMAX2 + 1     ! Skip the zonal structs - beta has no effect on them
                BRN = gModRealParams%BETA * gModRealParams%RN
                DO K = 1, gModIntParams%KMAX2
                    R = BRN * gHorzArr%RK(K)
                    DO L = 1,  gModIntParams%LMAX2
                        R1 = R * gHorzArr%AKLI(I)
                        ! Array slicing to handle the loop over N
                        gHorzArr%ADS(I, 1:gModIntParams%NLEV) = gHorzArr%ADS(I, 1:gModIntParams%NLEV) + R1 * gHorzArr%PS(I + 1, 1:gModIntParams%NLEV)
                        gHorzArr%ADS(I + 1, 1:gModIntParams%NLEV) = gHorzArr%ADS(I + 1, 1:gModIntParams%NLEV) - R1 * gHorzArr%PS(I, 1:gModIntParams%NLEV)
                        I = I + 2
                    END DO
                END DO
            ENDIF

            !----------------------------------------Compute diabatic strm terms:
            if (.NOT. gModelState%runInviscid) call STERMS()
            !-----------------------------------Barotropic computation:
            IF(gModIntParams%NLEV .EQ. 1) THEN
                DO I = 1, gCurrentDims%ITOT
                    gHorzArr%FS(I, 1) = gHorzArr%ADS(I, 1) + gHorzArr%ADO(I) + gHorzArr%S(I, 1)
                END DO
                RETURN
            ENDIF

            !---------------------------------------------Compute temperature advection:
            BLOCK
                REAL :: del_p, del_pi, w_n, w_np1 !, w_shear
                INTEGER :: N
            
                DO N = 1, gModIntParams%NLEV - 1
                    ! 1. Calculate physical pressure distance between adjacent layer centers
                    del_p = gVertArr%PV(N) - gVertArr%PV(N+1)
                    del_pi = 1.0 / del_p
            
                    ! 2. Calculate linear interpolation weights for the mean streamfunction interface PP(N+1)
                    w_n   = (gVertArr%PP(N + 1) - gVertArr%PV(N + 1)) * del_pi
                    w_np1 = (gVertArr%PV(N)   - gVertArr%PP(N + 1)) * del_pi
            
                    ! 3. Correctly weighted mean streamfunction at the temperature interface
                    buffer(1:ITOT) = w_n   * gHorzArr%PS(1:ITOT, N) + &
                                     w_np1 * gHorzArr%PS(1:ITOT, N + 1)
            
                    ! 4. Calculate the normalized vertical shear weighting factor.
                    ! If layers are uniform, (DP(N)+DP(N+1))/2 equals del_p, making w_shear = 1.0
                    ! Using this caused numerical instability, so we have removed this weighting.
                    !w_shear = 0.5 * (gVertArr%DP(N) + gVertArr%DP(N+1)) * del_pi

                    ! 5. Nondimensional Temperature (Shear) weighted properly for variable spacing
                    gHorzArr%TH(1:ITOT, N) = (gHorzArr%PS(1:ITOT, N + 1) - gHorzArr%PS(1:ITOT, N)) !* w_shear
            
                    ! 6. Call the non-linear advection routine
                    CALL NONLIN(gHorzArr%ADT(:, N), buffer, gHorzArr%TH(:, N))
                END DO
            END BLOCK

            ! Completely unweighted version of the above code, which is not really correct for non-uniform layers:
            !DO N = 1,  gModIntParams%NLEV - 1
            !    !-----------------------------------Compute strm and temp at temperature level:
            !    buffer(1:ITOT) = (gHorzArr%PS(1:ITOT, N) + gHorzArr%PS(1:ITOT, N + 1)) / 2. ! Mean streamfunction at temperature interface
            !    gHorzArr%TH(1:ITOT, N) = (gHorzArr%PS(1:ITOT, N + 1) - gHorzArr%PS(1:ITOT, N)) ! Shear centered at temperature interface
            !    CALL NONLIN(gHorzArr%ADT(:, N), buffer, gHorzArr%TH(:, N))
            !END DO
            !----------------------------------------Compute diabatic temp terms:
            if (.NOT. gModelState%runInviscid) call TTERMS()

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
            !-----------------------------------------------Compute Omega for NLEV>1:
            K = gCurrentDims%ITOT

            !============================================Omega iteration loop:
            !mu(n) = 2(p(n+1) - p(n+2))/(p(n) - p(n+2))	where the p's are the pressures of the boundary levels (PP array)
            !lambda(n) = 2(p(n-1) - p(n))/(p(n-1) - p(n+1))
            !mu'(n) = 2(p'(n+1) - p'(n+2))/(p'(n) - p'(n+2))	where these p's are the pressures of the boundaries (PV array)
            !lambda'(n) = 2(p'(n-1) - p'(n))/(p'(n-1) - p'(n+1))

            !S(n) =  k(n)*del2(PSIF(n) - PSI(n)) + j(n) * mu(n)*del2(PSI(n+1) - PSI(n)) + j(n)-lambda(n)*del2(psi(n-1)- psi(n))
            !T(n) = h(n)(thetaF(n) - theta(n)) + g(n)mu'(n)(theta(n+1) - theta(n)) + g(n-1)lambda'(n)(theta(n-1)- theta(n))
            !J(Theta) = J(PSI,Theta)
            !J(PSI) = J(Psi, del2(psi) + f + fh/H)

            !C(n) = J(Theta)(n) - T(n) + S(n+1)- S(n) + (1/a2)[J(Psi)(n+1) - J(Psi)(n) ]
            
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

            notHit = .true.
            DO I = 1, gCurrentDims%ITOT
                AX = gHorzArr%AKLI(I)   ! AKLI(i) is the inverse of a**2 and it depends only on the horizontal wavenumbers, not on the vertical level
                ! DPI(N) = PP(1)/(PP(N)-PP(N+1)) is the scaled inverse of the layer thickness. The PP array itself is NOT scaled
                ! and the scaling is p = Pp' where P in this case is PP(1).
                gammaLower = gVertArr%DPI(1) * AX  ! PP(1)/(a**2 * (PP(1) - PP(2)))
                gamma = gVertArr%DPI(2) * AX   ! PP(1)/(a**2 * (PP(2) - PP(3)))
                alpha = gamma + gammaLower + gVertArr%SIG(1)
                Cupper = gHorzArr%S(I, 2) + gHorzArr%ADS(I, 2)  ! Tendencies from streamfunction above omega level
                !-----------------------------------------------Add orography at bottom level:
                Clower = gHorzArr%S(I, 1) + gHorzArr%ADS(I, 1) + gHorzArr%ADO(I)   ! Tendencies from streamfunction below omega level
                
                C = gHorzArr%ADT(I, 1) - gHorzArr%T(I, 1) + (Cupper - Clower)   ! Temperature tendency plus difference in strm tendencies. This value
                                                                                ! of C(n) comes the omega eq. where the time derivative is eliminated
                gVertArr%X(1)  = gamma / alpha
                gVertArr%Y(1) = C / alpha
                !-----------------------------------------------Iterative loop for NLEV>2
                DO N = 2, gModIntParams%NLEV - 1
                    gammaLower = gamma
                    gamma = gVertArr%DPI(N + 1) * AX    ! PP(1)/(a**2 * (PP(N) - PP(N+1)))
                    alpha = (gamma + gammaLower * (1. - gVertArr%X(N - 1)) + gVertArr%SIG(N))
                    Clower = Cupper
                    Cupper = gHorzArr%S(I, N + 1) + gHorzArr%ADS(I, N + 1)
                    C = gHorzArr%ADT(I, N) - gHorzArr%T(I, N) + (Cupper - Clower)
                    gVertArr%X(N) = gamma / alpha
                    gVertArr%Y(N) = (gammaLower * gVertArr%Y(N - 1) + C) / alpha
                END DO

                !!------------------------------------Upper boundary to compute WW(NLEV-1):
                !! This smoother version did not work so well but it did stop numerical instability
                !gHorzArr%WW(I, gModIntParams%NLEV - 1) = gVertArr%Y(gModIntParams%NLEV - 1)
                !
                !!------------------------------------Compute raw WW(N) via Back-Substitution
                !DO NT = 2, gModIntParams%NLEV - 1
                !    N = gModIntParams%NLEV - NT
                !    gHorzArr%WW(I, N) = gVertArr%X(N) * gHorzArr%WW(I, N + 1) + gVertArr%Y(N)
                !END DO
                !
                !!------------------------------------VERTICAL SMOOTHER FOR WW (Omega)
                !BLOCK
                !    REAL :: ww_prev, ww_curr, ww_next
                !    ww_prev = gHorzArr%WW(I, 1)
                !    DO N = 2, gModIntParams%NLEV - 2
                !        ww_curr = gHorzArr%WW(I, N)
                !        ww_next = gHorzArr%WW(I, N + 1)
                !
                !        ! 3-point vertical filter
                !        gHorzArr%WW(I, N) = ww_curr + 0.125 * (ww_prev + ww_next - 2.0 * ww_curr)
                !
                !        ww_prev = ww_curr 
                !    END DO
                !END BLOCK
                !
                !!-------------------------------------Strm tendency due to omega at top interface:
                !gHorzArr%WS(I, gModIntParams%NLEV) = -gHorzArr%WW(I, gModIntParams%NLEV - 1) * gHorzArr%AKLI(I) * gVertArr%DPI(gModIntParams%NLEV)
                !
                !!------------------------------------Compute top level strm tendency:
                !gHorzArr%FS(I, gModIntParams%NLEV) = gHorzArr%ADS(I, gModIntParams%NLEV) + gHorzArr%WS(I, gModIntParams%NLEV) + gHorzArr%S(I, gModIntParams%NLEV)
                !
                !!-------------------------------------Compute WS and FS using smoothed WW
                !DO NT = 2, gModIntParams%NLEV - 1
                !    N = gModIntParams%NLEV - NT
                !    NP = N + 1
                !
                !    gHorzArr%WS(I, NP) = -(gHorzArr%WW(I, N) - gHorzArr%WW(I, NP)) * gVertArr%DPI(NP) * gHorzArr%AKLI(I)
                !    gHorzArr%FS(I, NP) = gHorzArr%ADS(I, NP) + gHorzArr%WS(I, NP) + gHorzArr%S(I, NP)
                !
                !    IF(notHit .AND. (ABS(gHorzArr%FS(I, NP)) .GT. 1. .OR. ieee_is_nan(gHorzArr%FS(I, NP)))) THEN
                !        print *, 'Level=', NP, ' I=', I, ' FS(I,N)=', gHorzArr%FS(I, NP), ' WS(I, NP)= ', gHorzArr%WS(I, NP), ' PS(I, NP)= ', gHorzArr%PS(I, NP)
                !        notHit = .false.
                !    ENDIF
                !END DO
                !
                !!----------------------------------Lower level strm tendency (with orography):
                !gHorzArr%WS(I, 1) = gHorzArr%WW(I, 1) * gVertArr%DPI(1) * gHorzArr%AKLI(I)
                !gHorzArr%FS(I, 1) = gHorzArr%ADS(I, 1) + gHorzArr%ADO(I) + gHorzArr%WS(I, 1) + gHorzArr%S(I, 1)
                !
                !!-----------------------------------Load temperature arrays for diagnostics:
                !DO N = 1, gModIntParams%NLEV - 1 
                !    ! Calculated safely using the smoothed WW values from above
                !    gHorzArr%WT(I, N) = gVertArr%SIG(N) * gHorzArr%WW(I, N)
                !    gHorzArr%FT(I, N) = -gHorzArr%ADT(I, N) + gHorzArr%WT(I, N) + gHorzArr%T(I, N)
                !END DO

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
                    IF(notHit .AND. ABS(gHorzArr%FS(I, NP)) .GT. 1. .OR. ieee_is_nan(gHorzArr%FS(I, NP))) THEN
                        print *, 'Level=', NP, ' I=', I, ' FS(I,N)=', gHorzArr%FS(I, NP), ' WS(I, NP)= ', gHorzArr%WS(I, NP), ' PS(I, NP)= ', gHorzArr%PS(I, NP)
                        notHit = .false.
                    ENDIF
                END DO
                !----------------------------------Lower level strm tendency (with orography):
                gHorzArr%WS(I, 1) = gHorzArr%WW(I, 1) * gVertArr%DPI(1) * gHorzArr%AKLI(I)
                gHorzArr%FS(I, 1) = gHorzArr%ADS(I, 1) + gHorzArr%ADO(I) + gHorzArr%WS(I, 1) + gHorzArr%S(I,1)
                !-----------------------------------Load temperature arrays for diagnostics:
                DO N = 1, gModIntParams%NLEV - 1
                    ! The temperature equation is d(Theta)/dt = -J(theta) + sig*w + T
                    gHorzArr%WT(I, N) = gVertArr%SIG(N) * gHorzArr%WW(I, N)
                    gHorzArr%FT(I, N) = -gHorzArr%ADT(I, N) + gHorzArr%WT(I, N) + gHorzArr%T(I, N)
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
        !  which are separated by the value of integrationMethod.
        !


        !> Routine to compute the nonlinear Jacobian FS = J(A,B)
        !! @param FS The spectral form of the computed Jacobian
        !! @param A The first array in the Jacobian in spectral form J(A,B)
        !! @param B The second array in the Jacobian in spectral form J(A,B)
        SUBROUTINE NONLIN(FS, A, B)
            use ConstantsModule, only: USE_INTERACTION_COEFFICIENTS
            use ModelParamsModule, only: gUserSettings
            use ArrayDimensionsModule, only: gCurrentDims
            use InteractionCoefficientsModule, only: gCintArr
            use NLevQGModelGlobals, only: LogMessage

            implicit none

            real, intent(inout) :: FS(:), A(:), B(:)
            character(len=256) :: msg

            !-------------------------------------Interaction coefficient arrays:
            !INTEGER*2 gCintArr%I1M(1), gCintArr%JIN(1), gCintArr%KIN(1)

            !------------------------------------------------Other variables:
            INTEGER index
            INTEGER II, IY, I, K, L

            !------------------------------------------------------ Start for interaction coefficients:
            IF(gUserSettings%integrationMethod == USE_INTERACTION_COEFFICIENTS) then
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
                        !write(msg, '(A, I4, A, F9.6, 1X, A, I4, A, I4, 1x, I4)') 'I: ', I, ' coef: ', gCintArr%CINT(INDEX), ' index: ', index, ' K, L: ', K, L
                        !call LogMessage(msg)
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
            use ArrayDimensionsModule, only: gCurrentDims
            use ModelFieldsModule, only: gHorzArr, gTransArr
            use ModelParamsModule, only: gModIntParams, gContourIntervals, gPlotScales, gModelState
            use ConstantsModule, only: SQRT2, ADV_BETA_HGT_TEND
            use Fftw3HelpersModule   ! This just gives the location of the 'fftw3.f03' file which is in the directory of the module. Not sure how.
            use PlotModule, only: plotDiagnostic

            implicit none
            include 'fftw3.f03'

            REAL, intent(in) :: A(:), B(:)
            REAL, intent(out) :: FS(:)

            integer IXMAX, IYMAX

            !   dA/dx is -2*k*n*ac(k,l)sinknxsinly and 2*k*n*as(k,l)cosknxsinly
            !   dB/dy is    2*l*bc(k,l)cosknxcosly and   2*l*bs(k,l)sinknxcosly and -sqrt(2)*l*bz(l)sinly

            ! dA/dx:  Done in subroutine DODADX result in G1(IXMAX, IYMAX)
            ! dB/dy:  Done in subroutine DODADY result in G2(IXMAX, IYMAX) this involves a transform of the zonal parts

            ! 1. dA/dx: 
            call DODADX(A, gTransArr%G1)
            ! ================= dA/dx is now in G1
            call plotDiagnostic(gTransArr%G1, gContourIntervals%Contours(ADV_BETA_HGT_TEND), gPlotScales(ADV_BETA_HGT_TEND) / 60.)

            ! 2. dB/dy: 
            call DODADY(B, gTransArr%G2)
            call plotDiagnostic(gTransArr%G2, gContourIntervals%Contours(ADV_BETA_HGT_TEND), gPlotScales(ADV_BETA_HGT_TEND) / 240.)
            ! ================ dB/dy is now in G2
            
            IXMAX = gCurrentDims%IXMAX
            IYMAX = gCurrentDims%IYMAX

            ! 3. Multiply G2 * G1 pointwise. Store in G4. G4 now has dA/dx * dB/dy in grid point space.
            gTransArr%G4(1:IXMAX, 1:IYMAX) = gTransArr%G1(1:IXMAX, 1:IYMAX) * gTransArr%G2(1:IXMAX, 1:IYMAX)
            call plotDiagnostic(gTransArr%G4, gContourIntervals%Contours(ADV_BETA_HGT_TEND), gPlotScales(ADV_BETA_HGT_TEND))

            ! ================================ G4 now has dA/dx * dB/dy in grid point space.
            ! G1 and G2 can be reused now.
            !   dA/dy is    2*l*ac(k,l)cosknxcosly and   2*l*as(k,l)sinknxcosly and -sqrt(2)*l*az(l)sinly
            !   dB/dx is -2*k*n*bc(k,l)sinknxsinly and 2*k*n*bs(k,l)cosknxsinly

            ! 4. dB/dx:
            call DODADX(B, gTransArr%G1)
            call plotDiagnostic(gTransArr%G1, gContourIntervals%Contours(ADV_BETA_HGT_TEND), gPlotScales(ADV_BETA_HGT_TEND) / 60.)
            ! ================= dB/dx is now in G1

            ! 5. dA/dy:
            call DODADY(A, gTransArr%G2)
            call plotDiagnostic(gTransArr%G2, gContourIntervals%Contours(ADV_BETA_HGT_TEND), gPlotScales(ADV_BETA_HGT_TEND) / 240.)
            ! ================ dA/dy is now in G2

            ! 6. Multiply G1 * G2 pointwise. Store in G3. G3 now has dA/dy * dB/x in grid point space.

            gTransArr%G3(1:IXMAX, 1:IYMAX) = gTransArr%G1(1:IXMAX, 1:IYMAX) * gTransArr%G2(1:IXMAX, 1:IYMAX)
            call plotDiagnostic(gTransArr%G3, gContourIntervals%Contours(ADV_BETA_HGT_TEND), gPlotScales(ADV_BETA_HGT_TEND))

            ! 7. Subtract G3 from G4 pointwise. Store in G4. G4 now has J(A,B) in grid point space.
            gTransArr%G4(1:IXMAX, 1:IYMAX) = gTransArr%G4(1:IXMAX, 1:IYMAX) - gTransArr%G3(1:IXMAX, 1:IYMAX)
            call plotDiagnostic(gTransArr%G4, gContourIntervals%Contours(ADV_BETA_HGT_TEND), gPlotScales(ADV_BETA_HGT_TEND))


            ! ========================== G4 now has the tendencies due to J(A,B) in grid point space.
            ! Next step is to transform the tendencies back to spectral space.

            call DoGridToSpectral(FS)

        end subroutine ComputeJacobianByTransform

        !> Does the transform of the d/dx part of the Jacobian calculation. The result is placed in gTransArr%G1.
        !! @param A The spectral coefficients of the streamfunction A = (ac(k,l)cosknx + as(k,l)sinknx)sinly + az(l)cosly
        !!          Since this is the d/dx part of the Jacobian calculation the az(l) derivative is 0.
        subroutine DoDADX(A, out_grid)
            use ArrayDimensionsModule, only: gCurrentDims
            use ModelFieldsModule,     only: gTransArr, gHorzArr
            use ModelParamsModule,     only: gModRealParams, gModIntParams
            use Fftw3HelpersModule
            use, intrinsic :: iso_c_binding, only : C_FLOAT_COMPLEX, C_FLOAT
            implicit none
            include 'fftw3.f03'
    
            REAL, intent(in)   :: A(:)
            REAL, intent(out)  :: out_grid(gCurrentDims%IXMAX, gCurrentDims%IYMAX)
    
            integer :: K, L, IXMAX, IYMAX, KMAX2, LMAX2, index
            real(C_FLOAT) :: temp_calc, temp_calc_k, wave_cos, wave_sin
            real(C_FLOAT) :: deriv_cos, deriv_sin
    
            IXMAX = gCurrentDims%IXMAX
            IYMAX = gCurrentDims%IYMAX
            KMAX2 = gModIntParams%KMAX2
            LMAX2 = gModIntParams%LMAX2
    
            ! Clear the entire 2D spectral working array
            gTransArr%Gc(:, :) = CMPLX(0.0_C_FLOAT, 0.0_C_FLOAT, kind=C_FLOAT_COMPLEX)
            
            !------------------------------------------------------ Start for Transform method FFTW3 MKL:
            ! Computing J(A,B)=dA/dx * dB/dy - dA/dy * dB/dx where A(k,l) = 2*(ac(k,l)*cosknx + as(k,l)*sinknx)sinly + sqrt(2)*az(l)*cosly
            !                                                  and B(k,l) = 2*(bc(k,l)*cosknx + bs(k,l)*sinknx)sinly + sqrt(2)*bz(l)*cosly

            ! dA/dx where A = A(k,l) = 2*(ac(k,l)*cosknx + as(k,l)*sinknx)sinly + sqrt(2)*az(l)*cosly
            ! equals -2*k*n*ac(k,l)sinknxsinly and 2*k*n*as(k,l)cosknxsinly
            ! 1. Load -2*k*n*ac(k,l)sinknxsinly and 2*k*n*as(k,l)cosknxsinly into the imaginary and real parts of Gc for each L
            ! We zero out_grid. This is to assure no 'leaks' of garbage

            ! ====================================================================
            ! PART A: LOAD THE ZONAL MODES (d/dx of Zonal is zero)
            ! ====================================================================
            ! Column 1 (K=0) is left as 0.0 because zonal terms have no x-dependence.
    
            ! ====================================================================
            ! PART B: LOAD THE WAVE MODES (Apply d/dx)
            ! ====================================================================
            ! 0.5 comes from basis function wave multiplier split, scaled by FFTW norm factor
            temp_calc = 0.5_C_FLOAT 
    
            do K = 1, KMAX2
                ! Compute Kn factor for this wave wavenumber (RN is domain scale factor)
                temp_calc_k = temp_calc * gHorzArr%RK(K) * gModRealParams%RN
        
                do L = 1, LMAX2
                    ! Linear vector index calculations matching your layout
                    index = LMAX2 + (K - 1) * (LMAX2 * 2) + (L - 1) * 2 + 1
            
                    wave_cos = A(index) * temp_calc_k      ! A(index) is the cosknxsinly part of the streamfunction spectral coefficient
                    wave_sin = A(index + 1) * temp_calc_k  ! A(index + 1) is the sinknxsinly part of the streamfunction spectral coefficient
            
                    ! Apply d/dx analytical derivative rules:
                    deriv_cos =  wave_sin       !  2*k*n*as(k,l)cosknxsinly
                    deriv_sin = -wave_cos       ! -2*k*n*ac(k,l)sinknxsinly
                    ! Map to positive Y-frequency (+L slot)
                    gTransArr%Gc(K + 1, L + 1) = CMPLX(-deriv_sin, -deriv_cos, kind=C_FLOAT_COMPLEX)

                    ! Map to negative Y-frequency (-L slot) with sine sign inversion
                    gTransArr%Gc(K + 1, IYMAX - L + 1) = CMPLX(deriv_sin, deriv_cos, kind=C_FLOAT_COMPLEX)
                end do
            end do
    
            ! ====================================================================
            ! PART C: EXECUTE 2D TRANSFORM
            ! ====================================================================
            call fftwf_execute_dft_c2r(plan_c2r_2d, gTransArr%Gc, out_grid)
    
        end subroutine DoDADX
        

        !> Does the transform of the d/dy part of the Jacobian calculation. The result is placed in gTransArr%G2.
        !! @param A The spectral coefficients of the streamfunction A = (ac(k,l)cosknx + as(k,l)sinknx)sinly + az(l)cosly
        subroutine DoDADY(A, out_grid)
            use ArrayDimensionsModule, only: gCurrentDims
            use ConstantsModule, only: SQRT22
            use ModelFieldsModule,     only: gTransArr
            use ModelParamsModule,     only: gModIntParams
            use Fftw3HelpersModule
            use, intrinsic :: iso_c_binding, only : C_FLOAT_COMPLEX, C_FLOAT
            implicit none
            include 'fftw3.f03'
    
            REAL, intent(in)   :: A(:)
            REAL, intent(out)  :: out_grid(gCurrentDims%IXMAX, gCurrentDims%IYMAX)
    
            integer :: K, L, IXMAX, IYMAX, KMAX2, LMAX2, index
            real(C_FLOAT) :: temp_calc, temp_calc_l, wave_cos, wave_sin, zonal_val
            real(C_FLOAT) :: deriv_cos, deriv_sin, deriv_zonal
    
            IXMAX = gCurrentDims%IXMAX
            IYMAX = gCurrentDims%IYMAX
            KMAX2 = gModIntParams%KMAX2
            LMAX2 = gModIntParams%LMAX2
    
            ! Clear the entire 2D spectral working array
            gTransArr%Gc(:, :) = CMPLX(0.0_C_FLOAT, 0.0_C_FLOAT, kind=C_FLOAT_COMPLEX)

            ! ====================================================================
            ! PART A: LOAD THE ZONAL MODES (d/dy turns cos(Ly) into -L*sin(Ly))
            ! ====================================================================
            ! SQRT2 multiplier comes from original basis function
            temp_calc = SQRT22
    
            do L = 1, LMAX2
                ! Analytical y-wavenumber L scaling
                temp_calc_l = temp_calc * REAL(L, C_FLOAT) 
        
                zonal_val = A(L) * temp_calc_l
                deriv_zonal = -zonal_val ! d/dy[cos(Ly)] = -L*sin(Ly)
        
                ! Pure sine wave components in 2D FFT mapping:
                ! Positive frequency slot gets -deriv*i
                gTransArr%Gc(1, L + 1) = CMPLX(0.0_C_FLOAT, -deriv_zonal, kind=C_FLOAT_COMPLEX)
                ! Negative frequency slot gets +deriv*i (conjugate symmetry)
                gTransArr%Gc(1, IYMAX - L + 1) = CMPLX(0.0_C_FLOAT, deriv_zonal, kind=C_FLOAT_COMPLEX)
            end do

            !------------------------------------------------------ Start for Transform method FFTW3 MKL:
            ! Computing J(A,B)=dA/dx * dB/dy - dA/dy * dB/dx where A(k,l) = 2*(ac(k,l)*cosknx + as(k,l)*sinknx)sinly + sqrt(2)*az(l)*cosly
            !                                                  and B(k,l) = 2*(bc(k,l)*cosknx + bs(k,l)*sinknx)sinly + sqrt(2)*bz(l)*cosly

            ! 1. Load  2*l*ac(k,l)cosknxcosly and 2*l*as(k,l)sinknxcosly into the real and imaginary parts of Gc for each L
            !  We zero the out_grid. This is to assure no 'leaks' of garbage

            ! ====================================================================
            ! PART B: LOAD THE WAVE MODES (d/dy turns sin(Ly) into L*cos(Ly))
            ! ====================================================================
            ! 0.5 multiplier comes from original wave basis function split, scaled by FFTW norm factor
            temp_calc = 0.5_C_FLOAT
    
            do K = 1, KMAX2
                do L = 1, LMAX2
                    temp_calc_l = temp_calc * REAL(L, C_FLOAT)
            
                    index = LMAX2 + (K - 1) * (LMAX2 * 2) + (L - 1) * 2 + 1
            
                    wave_cos = A(index) * temp_calc_l
                    wave_sin = A(index + 1) * temp_calc_l
            
                    ! d/dy [ sin(Ly) ] = L * cos(Ly)
                    ! The wave basis was sin(Ly), now it behaves as cos(Ly)
                    deriv_cos = wave_cos ! 2*l*ac(k,l)cosknxcosly
                    deriv_sin = wave_sin ! 2*l*as(k,l)sinknxcosly
            
                    ! Map to positive Y-frequency (+L slot)
                    ! For cos(Kx)*cos(Ly) -> Real part, positive sign
                    ! For sin(Kx)*cos(Ly) -> Imaginary part, negative sign
                    gTransArr%Gc(K + 1, L + 1) = CMPLX(deriv_cos, -deriv_sin, kind=C_FLOAT_COMPLEX)
            
                    ! Map to negative Y-frequency (-L slot)
                    ! Because cos(-Ly) = cos(Ly), signs DO NOT invert across the boundary!
                    gTransArr%Gc(K + 1, IYMAX - L + 1) = CMPLX(deriv_cos, -deriv_sin, kind=C_FLOAT_COMPLEX)
                end do
            end do
    
            ! ====================================================================
            ! PART C: EXECUTE 2D TRANSFORM
            ! ====================================================================
            call fftwf_execute_dft_c2r(plan_c2r_2d, gTransArr%Gc, out_grid)
    
        end subroutine DoDADY

        subroutine DoGridToSpectral(FS)
            use ArrayDimensionsModule, only: gCurrentDims
            use ConstantsModule, only: SQRT2
            use ModelFieldsModule,     only: gTransArr
            use ModelParamsModule,     only: gModRealParams, gModIntParams
            use Fftw3HelpersModule
            use, intrinsic :: iso_c_binding, only : C_FLOAT_COMPLEX, C_FLOAT
            implicit none
            include 'fftw3.f03'
    
            REAL, intent(out)  :: FS(:)
    
            integer :: K, L, IXMAX, IYMAX, KMAX2, LMAX2, I, m_idx
            real(C_FLOAT) :: norm_factor_2d, wave_cos, wave_sin, zonal_val, invNorm
            
            ! This does not work as the double periodic grid transform does not know about the model 
            ! boundary conditions. It will take a sinnxcos2y structure and understand that it is a 
            ! valid component. In the rigid wall model, cos2y is not allowed and has to be reprojected
            ! onto sinLy components. This is what the interaction coefficients do at a great cost.
            !
            ! A possible workaround now that we have gotten this far.
            ! 1. Take the zonal average from Y = 1 to IYMAX/2. Keep that array for the cosLy zonal flows.
            ! 2. Subtract that from the original grid so that there is no zonal mean left (ignore y = IYMAX/2 to IYMAX)
            ! 3. Replace y = IYMAX/2 to IYMAX with - (y = 1 to IYMAX/2). Odd symmetry. Then transform. This will force
            !    only sinLy components.
    
            IXMAX = gCurrentDims%IXMAX
            IYMAX = gCurrentDims%IYMAX
            KMAX2 = gModIntParams%KMAX2
            LMAX2 = gModIntParams%LMAX2
    
            ! Initialize output vector to zero
            FS = 0.0_C_FLOAT
            
            ! Compute the zonal average to ge the cos(Ly) components.
            I = IYMAX / 2
            invNorm = 1.0_C_FLOAT / REAL(IXMAX, C_FLOAT)
            gTransArr%GZonal = 0.0
            ! =====================================================================
            ! 1. Handle the First Stationary Point (L = 1, y = 0)
            ! =====================================================================
            gTransArr%GZonal(1) = sum(gTransArr%G4(:, 1)) * invNorm ! Compute the zonal average of the y=0 row
            gTransArr%G4(:, 1)  = gTransArr%G4(:, 1) - gTransArr%GZonal(1)       ! Subtract that zonal average from the y=0 row

            ! =====================================================================
            ! 2. Loop over the internal paired rows (From L = 2 up to I)
            ! =====================================================================
            do L = 2, I
                m_idx = IYMAX - L + 2 

                ! Compute zonal average from the bottom half row
                gTransArr%GZonal(L) = sum(gTransArr%G4(:, L)) * invNorm
    
                ! Remove the zonal average from this bottom half row
                gTransArr%G4(:, L) = gTransArr%G4(:, L) - gTransArr%GZonal(L)
    
                ! Enforce anti-symmetry on the top half row
                gTransArr%G4(:, m_idx) = -gTransArr%G4(:, L)
    
                ! Mirror the zonal average to the top half symmetrically
                !gTransArr%GZonal(m_idx) = gTransArr%GZonal(L)
            end do

            ! =====================================================================
            ! 3. Handle the Second Stationary Point (L = I + 1, y = pi)
            ! =====================================================================
            ! This is the true Nyquist peak row (e.g., Row 5 if IYMAX=8)
            gTransArr%GZonal(I + 1) = sum(gTransArr%G4(:, I + 1)) * invNorm
            gTransArr%G4(:, I + 1)  = gTransArr%G4(:, I + 1) - gTransArr%GZonal(I + 1)

            ! Because this row is anti-symmetric with itself across the midpoint, 
            ! the 2D grid values at this specific row must mathematically be zeroed out.
            gTransArr%G4(:, I + 1) = 0.0_C_FLOAT 

            ! 6. Transform the zonal average to get the cos(Ly) components in spectral space.
            invNorm = 2./ REAL(IYMAX, C_FLOAT) ! Normalization factor for 1D FFTW transforms (see FFTW documentation)
            call fftwf_execute_r2r(plan_y_FFTW_REDFT00, gTransArr%GZonal, gTransArr%GZonal)

            ! The REDFT00 output is purely real and has the cos(Ly) components in the first LMAX2 + 1 slots. We do not use the cos(0) value
            FS(1:LMAX2) = gTransArr%GZonal(2 : LMAX2 + 1) * invNorm

            ! ====================================================================
            ! STEP 1: EXECUTE COMPREHENSIVE 2D REAL-TO-COMPLEX FFT
            ! ====================================================================
            ! Input:  gTransArr%G4 (Real array of size IXMAX, IYMAX)
            ! Output: gTransArr%Gc (Complex array of size IXMAX/2 + 1, IYMAX)
            call fftwf_execute_dft_r2c(plan_r2c_2d, gTransArr%G4, gTransArr%Gc)
    
            ! Global inverse scaling factor to normalize the raw FFTW accumulation
            ! norm_inv_wave is 2.0 / (IXMAX * IYMAX)
            norm_factor_2d = 0.5_C_FLOAT * norm_inv_wave

            ! ====================================================================
            ! STEP 3: EXTRACT WAVE COMPONENTS (sinLy boundaries at K > 0)
            ! ====================================================================
            !norm_factor_2d = norm_inv_wave
            do K = 1, KMAX2
                do L = 1, LMAX2
                    ! Structural address pointer inside the compacted linear array
                    I = LMAX2 + 2 * LMAX2 * (K - 1) + 2 * (L - 1) + 1

                    ! Decode the components by tracking the anti-symmetric behavior of sin(Ly)
                    ! Forward model set Gc(L+1) = (-sin, -cos) and Gc(-L) = (sin, cos)

                    ! 1. Extract Cos(Kx)*Sin(Ly) -> Mapped to Imaginary parts
                    wave_cos = AIMAG(gTransArr%Gc(K + 1, IYMAX - L + 1)) - AIMAG(gTransArr%Gc(K + 1, L + 1))
                    !wave_cos = wave_cos - AIMAG(gTransArr%Gc(K + 1, L + 1))

                    ! 2. Extract Sin(Kx)*Sin(Ly) -> Mapped to Real parts
                    wave_sin = REAL(gTransArr%Gc(K + 1, IYMAX - L + 1)) - REAL(gTransArr%Gc(K + 1, L + 1))
                    !wave_sin = wave_sin - REAL(gTransArr%Gc(K + 1, L + 1))

                    ! Store values normalized 
                    FS(I)     = wave_cos * norm_factor_2d
                    FS(I + 1) = wave_sin * norm_factor_2d
                end do
            end do
        end subroutine DoGridToSpectral

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
            ! Replace the do loop ove
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
            use ArrayDimensionsModule, only: gCurrentDims
            
            REAL, DIMENSION(gModIntParams%NLEV) :: R_Upper, R_Lower

            INTEGER :: N, ITOT, NLEV

            NLEV = gModIntParams%NLEV
            ITOT = gCurrentDims%ITOT
            if (NLEV < 2) return ! SHould never be hit but just in case.

            ! 1. Pre-calculate scalar coefficients for each level (The "N" loop)
            R_Upper = 0.0
            R_Lower = 0.0

            IF (NLEV > 2) THEN
                ! N = 1 Case. Here we have to skip the R_Lower
                R_Upper(1) = 2.0 * gVertArr%G(1) * (gVertArr%PV(2) - gVertArr%PV(3)) / (gVertArr%PV(1) - gVertArr%PV(3))
                gHorzArr%T(1:ITOT, 1) = gVertArr%H(1) * (gHorzArr%TF(1:ITOT, 1) - gHorzArr%TH(1:ITOT, 1)) &
                    + gVertArr%G(1) * R_Upper(1) * (gHorzArr%TH(1:ITOT, 2) * gVertArr%AU(1) - gHorzArr%TH(1:ITOT, 1))

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
                gHorzArr%T(1:ITOT, N) = gVertArr%H(N) * (gHorzArr%TF(1:ITOT, N) - gHorzArr%TH(1:ITOT, N))
                if (N > 1) gHorzArr%T(1:ITOT, N) = gHorzArr%T(1:ITOT, N) &
                    + R_Upper(N) * (gHorzArr%TH(1:ITOT, N + 1) * gVertArr%AU(N) - gHorzArr%TH(1:ITOT, N)) & ! Cut the upper transfer in half
                    + R_Lower(N) * ((gHorzArr%TH(1:ITOT, N - 1) / gVertArr%AU(N - 1)) - gHorzArr%TH(1:ITOT, N))
            END DO
        END subroutine TTERMS

        !********************************************Subroutine to compute increments:
        !  Routine increments either strm, vertical diabatic arrays or RN or BETA
        !  given R so the model can be rerun. 'I' gives which quantity (1-9).
        !  PRF is the start and end profile for the incrementing:
        !
        SUBROUTINE PROFIL(PRF, R, II)
            use ConstantsModule, only: RAD_TO_DEGREES
            use ModelFieldsModule, only: gVertArr, gHorzArr
            use ModelParamsModule, only: gUserSettings, gModRealParams, gModIntParams

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
                J = gModIntParams%bombMeridWave
                IF (gModIntParams%bombZonalWave .GT. 0) J = (2 * gModIntParams%bombZonalWave - 1) * gModIntParams%LMAX2 + 2 * J
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
                            IF (gModIntParams%bombZonalWave .GE. 1) THEN
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
            use ArrayDimensionsModule, only: gCurrentDims
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

        !******************************************** Subroutine QG for Amp-G values:
        SUBROUTINE ComputeGQ()      !(DAT,PS,DPI,GQ,GQA,PSA,PSS)
            use ArrayDimensionsModule, only: gCurrentDims
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
            use ArrayDimensionsModule, only: gCurrentDims
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

        SUBROUTINE ComputeEnergySpectrum(DELTA_K, ENERGY_1D, MAX_BIN)
            use ModelFieldsModule, only: gHorzArr, gVertArr
            use ModelParamsModule, only: gModRealParams, gModIntParams
            IMPLICIT NONE

            ! Inputs
            INTEGER, INTENT(IN) :: MAX_BIN
            REAL, INTENT(IN) :: DELTA_K                 ! Bin size for 1D spectrum
    
            ! Outputs
            REAL, DIMENSION(MAX_BIN), INTENT(OUT) :: ENERGY_1D
    
            ! Locals
            integer :: KMAX, LMAX, NMAX
            INTEGER :: I, N, K, L, BIN_IDX
            REAL :: K_PHYS_SQ, L_SQ, K_TOTAL, KE, APE, RN_SQ, RN, DELTA_K_INV
            REAL, DIMENSION(gModIntParams%NLEV - 1) :: APE_WEIGHT

            KMAX = gModIntParams%KMAX2
            LMAX = gModIntParams%LMAX2
            NMAX = gModIntParams%NLEV
    
            ENERGY_1D = 0.0
            RN = gModRealParams%RN
            RN_SQ = RN**2
            DELTA_K_INV = 1.0 / DELTA_K
    
            ! Precompute APE weighting factor: A(N)^2 / SIG(N)
            APE_WEIGHT(1 : NMAX - 1) = (gVertArr%A(1 : NMAX - 1) * gVertArr%A(1 : NMAX - 1)) / MAX(gVertArr%SIG(1 : NMAX - 1), 1.0E-8)

            ! ==========================================
            ! STEP 1: PROCESS ZONAL MEAN FLOW (K = 0)
            ! ==========================================
            K_PHYS_SQ = 0.0 ! Zonal mean has no x-dependence, so n*k = 0
            DO I = 1, LMAX
                L = I
                L_SQ = REAL(L * L)
                K_TOTAL = SQRT(K_PHYS_SQ + L_SQ) ! K_TOTAL = L
                BIN_IDX = INT(K_TOTAL * DELTA_K_INV) + 1
        
                IF (BIN_IDX <= MAX_BIN) THEN
                    ! Kinetic Energy for K=0 across all layers
                    KE = 0.0
                    DO N = 1, NMAX
                        KE = KE + 0.5 * L_SQ * (gHorzArr%PS(I, N) * gHorzArr%PS(I, N))
                    END DO
            
                    ! Available Potential Energy directly from temperature coefficients
                    APE = 0.0
                    DO N = 1, NMAX - 1
                        APE = APE + 0.5 * APE_WEIGHT(N) * (gHorzArr%TH(I, N) * gHorzArr%TH(I, N))
                    END DO

                    ENERGY_1D(BIN_IDX) = ENERGY_1D(BIN_IDX) + KE + APE
                END IF
            END DO

            ! ==========================================
            ! STEP 2: PROCESS WAVE COMPONENTS (K > 0)
            ! ==========================================
            I = LMAX + 1
            DO K = 1, KMAX
                ! Scale the zonal wavenumber by the aspect ratio 'n'
                K_PHYS_SQ = REAL(K * K) * RN_SQ

                DO L = 1, LMAX
                    L_SQ = REAL(L * L)
                    K_TOTAL = SQRT(K_PHYS_SQ + L_SQ)
                    BIN_IDX = INT(K_TOTAL * DELTA_K_INV) + 1

                    IF (BIN_IDX <= MAX_BIN) THEN
                        KE = 0.0
                        APE = 0.0

                        ! Sum KE over layers using scaled total wavenumber
                        DO N = 1, NMAX
                            KE = KE + 0.5 * (K_PHYS_SQ + L_SQ) * (gHorzArr%PS(I, N) * gHorzArr%PS(I, N) + gHorzArr%PS(I + 1, N) * gHorzArr%PS(I + 1, N))
                        END DO

                        ! Sum APE over interfaces directly from TH wave pairs
                        DO N = 1, NMAX - 1
                            APE = APE + 0.5 * APE_WEIGHT(N) * (gHorzArr%TH(I, N) * gHorzArr%TH(I, N) + gHorzArr%TH(I + 1, N) * gHorzArr%TH(I + 1, N))
                        END DO

                        ENERGY_1D(BIN_IDX) = ENERGY_1D(BIN_IDX) + KE + APE
                    END IF
                    ! Jump past the (cos, sin) pair
                    I = I + 2
                END DO
            END DO

        END SUBROUTINE ComputeEnergySpectrum
        
        !> Returns the total wave energy (kinetic + available potential) as a function of zonal wavenumber (for all L and N) 
        !> in gTransArr%G2Zonal (reuse of a transform array) and
        !> the total zonal energy as a function of meridional wavenumber for all N in gTransArr%GZonal.
        SUBROUTINE ComputeZonalSpectrum()
            use ConstantsModule, only: HEIGHT, TEMPERATURE
            use ModelFieldsModule, only: gHorzArr, gVertArr, gEnergyArr
            use ModelParamsModule, only: gModRealParams, gModIntParams, gPlotScales
            use WelfordModule, only: updateRunStats
            IMPLICIT NONE


            ! Locals
            integer :: KMAX, LMAX, NMAX
            INTEGER :: I, I_Next, N, K, L, BIN_IDX
            REAL :: K_PHYS_SQ, L_SQ, K_TOTAL, KE, APE, RN_SQ, RN, DELTA_K_INV, pMeanInv
            REAL, DIMENSION(gModIntParams%NLEV) :: local_KE_W
            REAL, DIMENSION(gModIntParams%NLEV - 1) :: local_APE_W
            REAL, DIMENSION(gModIntParams%KMAX2) :: totalKE

            KMAX = gModIntParams%KMAX2
            LMAX = gModIntParams%LMAX2
            NMAX = gModIntParams%NLEV
    
            gEnergyArr%ZonalWaveKE = 0.0
            gEnergyArr%ZonalWaveAPE = 0.0
            gEnergyArr%ZonalKE = 0.0
            gEnergyArr%ZonalAPE = 0.0
            RN = gModRealParams%RN
            RN_SQ = RN*RN
   
            ! Precompute APE weighting factor: A(N)^2 / SIG(N)  But sig is non-dimensional and is itself scaled by A(N)
            ! so A(N)/SIG
            !APE_WEIGHT(1 : NMAX - 1) = (gVertArr%A(1 : NMAX - 1) * gVertArr%A(1 : NMAX - 1)) / MAX(gVertArr%SIG(1 : NMAX - 1), 1.0E-8)

            ! ==========================================
            ! STEP 1: PROCESS ZONAL MEAN FLOW (K = 0)
            ! ==========================================
            !DO L = 1, LMAX
            !    L_SQ = REAL(L * L)
            !    ! Zonal Kinetic Energy across all layers
            !    KE = 0.0
            !    DO N = 1, NMAX
            !        KE = KE + 0.5 * L_SQ * (gHorzArr%PS(L, N) * gHorzArr%PS(L, N))
            !    END DO
            !    ! Zonal Available Potential Energy directly from temperature coefficients
            !    APE = 0.0
            !    DO N = 1, NMAX - 1
            !        APE = APE + 0.5 * (gHorzArr%TH(L, N) * gHorzArr%TH(L, N)) / gVertArr%SIG(N)
            !    END DO
            !
            !    gEnergyArr%ZonalKE(L) = gEnergyArr%ZonalKE(L) + KE
            !    gEnergyArr%ZonalAPE(L) = gEnergyArr%ZonalAPE(L) + APE
            !END DO

            ! ==========================================
            ! STEP 2: PROCESS WAVE COMPONENTS (K > 0)
            ! ==========================================
            I = LMAX + 1
            K_PHYS_SQ = 0.0
            pMeanInv = REAL(NMAX) / REAL(gVertArr%PP(1) - gVertArr%PP(NMAX + 1))
            ! Pre compute arrays for weighting
            DO N = 1, NMAX
                local_KE_W(N) = 0.5 * gVertArr%DP(N) * pMeanInv * gPlotScales(HEIGHT) * gPlotScales(HEIGHT)
            END DO

            DO N = 1, NMAX - 1
                local_APE_W(N) = 0.25 * (gVertArr%DP(N) + gVertArr%DP(N + 1)) * pMeanInv * gPlotScales(TEMPERATURE) * gPlotScales(TEMPERATURE) / &
                            (gVertArr%SIG(N) * gVertArr%A(N))
            END DO
            totalKE = 0.0
            DO K = 1, KMAX
                ! Scale the zonal wavenumber by the aspect ratio 'n'
                K_PHYS_SQ = REAL(K * K) * RN_SQ
                I_next = I + 1 ! Eliminates repetitive integer additions in the loops

                DO L = 1, LMAX
                    L_SQ = REAL(L * L)

                    KE = 0.0
                    APE = 0.0

                    ! Sum KE over layers using scaled total wavenumber
                    DO N = 1, NMAX
                        KE = KE + local_KE_W(N) * (gHorzArr%PS(I, N) * gHorzArr%PS(I, N) + gHorzArr%PS(I_next, N) * gHorzArr%PS(I_next, N))
                    END DO
                    KE = KE * (K_PHYS_SQ + L_SQ)

                    ! Sum APE over interfaces directly from TH wave pairs
                    DO N = 1, NMAX - 1
                        APE = APE + (gHorzArr%TH(I, N) * gHorzArr%TH(I, N) + gHorzArr%TH(I_next, N) * gHorzArr%TH(I_next, N)) * local_APE_W(N)
                    END DO
                    totalKE(K) = totalKE(K) + KE
                    gEnergyArr%ZonalWaveKE(K) = gEnergyArr%ZonalWaveKE(K) + KE
                    gEnergyArr%ZonalWaveAPE(K) = gEnergyArr%ZonalWaveAPE(K) + APE
                    ! Jump past the (cos, sin) pair
                    I = I + 2
                END DO
            END DO

            call updateRunStats(totalKE, gEnergyArr%WelfordKE)


        END SUBROUTINE ComputeZonalSpectrum
        
        !SUBROUTINE ComputeStructures()
        !    use ConstantsModule, only: RAD_TO_DEGREES
        !    use ArrayDimensionsModule, only: gCurrentDims
        !    use ModelFieldsModule, only: gHorzArr, gVertArr, gStatsArr, gEnergyArr
        !    use ModelParamsModule, only: gModIntParams, gModRealParams
        !    use WelfordModule, only: updateRunStats
        !    implicit none
        !    
        !    integer :: I, N
        !    integer :: KMAX, LMAX, NMAX, ITOT
        !    REAL :: phase, amplitude, phase_diff
        !    
        !    KMAX = gModIntParams%KMAX2
        !    LMAX = gModIntParams%LMAX2
        !    NMAX = gModIntParams%NLEV
        !    ITOT = gCurrentDims%ITOT
        !    
        !    ! To compute the structure we get the phase and amplitude of the streamfunction at layer
        !    ! n and compute the phase difference with respect to the phase of the streamfunction at
        !    ! the layer n-1.
        !    ! 1. First Pass: Compute absolute amplitudes and phases for ALL levels
        !    do N = 1, NMAX
        !        do I = LMAX + 1, ITOT - 1, 2
        !            ! Compute absolute phase and amplitude at level N
        !            phase = atan2(gHorzArr%PS(I + 1, N), gHorzArr%PS(I, N)) * RAD_TO_DEGREES
        !            amplitude = sqrt(gHorzArr%PS(I, N)**2 + gHorzArr%PS(I + 1, N)**2)
        !
        !            gEnergyArr%AMP_PHASE(I, N) = amplitude
        !            gEnergyArr%AMP_PHASE(I + 1, N) = phase  ! Temporarily store absolute phase
        !        end do
        !    end do
        !
        !    ! 2. Second Pass: Compute relative phase differences and update stats
        !    do N = 2, NMAX
        !        do I = LMAX + 1, ITOT - 1, 2
        !            ! Compute difference relative to level 1
        !            phase_diff = gEnergyArr%AMP_PHASE(I + 1, N) - gEnergyArr%AMP_PHASE(I + 1, 1)
        !    
        !            ! Crucial: Wrap phase difference to the range [-180, 180] 
        !            ! to prevent Welford statistics from blowing up due to 360-degree jumps
        !            phase_diff = phase_diff - 360.0 * nint(phase_diff / 360.0)
        !            gEnergyArr%AMP_PHASE(I + 1, N) = phase_diff
        !
        !        end do
        !    end do
        !    
        !    ! 3. Third Pass: Log statistics and cleanly zero out Level 1
        !    gEnergyArr%AMP_PHASE(LMAX + 2 : ITOT : 2, 1) = 0.0
        !    do N = 1, NMAX
        !        call updateRunStats(gEnergyArr%AMP_PHASE(:, N), gEnergyArr%WelfordAMP_PHASE(:, N))
        !    end do
        !    print '(*(F10.4))', (gEnergyArr%WelfordAMP_PHASE(3, N)%mean, N = 1, SIZE(gEnergyArr%WelfordAMP_PHASE, 2))
        !end subroutine ComputeStructures
        
        SUBROUTINE ComputeStructures()
            use ConstantsModule, only: RAD_TO_DEGREES
            use ArrayDimensionsModule, only: gCurrentDims
            use ModelFieldsModule, only: gHorzArr, gVertArr, gStatsArr, gEnergyArr
            use ModelParamsModule, only: gModIntParams, gModRealParams
            use WelfordModule, only: updateRunStats
            implicit none
            
            integer :: I, N
            integer :: KMAX, LMAX, NMAX, ITOT
            REAL :: phaseS, amplitudeS, phaseT, amplitudeT, phase_diff, phase_diff_TMin
            
            KMAX = gModIntParams%KMAX2
            LMAX = gModIntParams%LMAX2
            NMAX = gModIntParams%NLEV
            ITOT = gCurrentDims%ITOT
            
            ! To compute the structure we get the phase and amplitude of the streamfunction at layer
            ! n and compute the phase difference with respect to the phase of the streamfunction at
            ! the layer n-1.

            do N = 1, NMAX
                do I = LMAX + 1, ITOT - 1, 2
                    ! Compute absolute phase and amplitude at level N
                    phaseS = atan2(gHorzArr%PS(I + 1, N), gHorzArr%PS(I, N)) * RAD_TO_DEGREES
                    amplitudeS = sqrt(gHorzArr%PS(I, N)**2 + gHorzArr%PS(I + 1, N)**2)
                    phaseT = atan2(gHorzArr%TH(I + 1, N), gHorzArr%TH(I, N)) * RAD_TO_DEGREES
                    amplitudeT = sqrt(gHorzArr%TH(I, N)**2 + gHorzArr%TH(I + 1, N)**2)

                    gEnergyArr%AMP_PHASE(I, N) = amplitudeS

                    phase_diff = phaseS - phaseT
                    
                    if (phase_diff > 180.0) then
                        phase_diff = phase_diff - 360.0
                    else if (phase_diff <= -180.0) then
                        phase_diff = phase_diff + 360.0
                    end if
                    
                    phase_diff_TMin = phaseS - (phaseT + 180.)
                    
                    if (phase_diff_TMin > 180.0) then
                        phase_diff_TMin = phase_diff_TMin - 360.0
                    else if (phase_diff_TMin <= -180.0) then
                        phase_diff_TMin = phase_diff_TMin + 360.0
                    end if
                    
                    !if (N == 1) print '(F10.4, A, F10.4)', phase_diff, " ", phase_diff_TMin
                    
                    if (ABS(phase_diff_TMin) .LT. ABS(phase_diff)) phase_diff = phase_diff_TMin - 180.0

                    gEnergyArr%AMP_PHASE(I + 1, N) = phase_diff
                end do
                call updateRunStats(gEnergyArr%AMP_PHASE(:, N), gEnergyArr%WelfordAMP_PHASE(:, N))
            end do
            !print '(*(F10.4))', (gEnergyArr%WelfordAMP_PHASE(3, N)%mean, N = 1, SIZE(gEnergyArr%WelfordAMP_PHASE, 2))
        end subroutine ComputeStructures

    end module ModelModule