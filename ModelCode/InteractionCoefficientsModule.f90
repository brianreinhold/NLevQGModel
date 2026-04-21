module InteractionCoefficientsModule
    implicit none
    public :: computeInteractionCoefficients
    public :: AllocateCintArrays, DeallocateCintArrays
    public :: CintArrays, gCintArr, BI, IBI


    !  BI(*) contains all the real interaction coefficient arrays
    !  IBI(*) contains all the integer interaction coefficient arrays
    !  I1M(*) is the number of interactions per equation for interaction model (Number of Cijk in the Ith equation)
    !  JIN(*) is the j-index of Cijk (is the J-value of the products in the Ith equation)
    !  KIN(*) is the k-index of Cijk (Is the K-value of the products in the Ith equation)
    !  CINT(*) are the interaction coefficients Cijk

    type :: CintArrays
        LOGICAL :: allocated = .false. ! Flag to indicate if the arrays are allocated (direct setting only in fortran 2003+)
        LOGICAL :: defined = .false.   ! Flag to indicate if the arrays are defined (i.e., populated with values)
        integer :: numOfInteractionCoeffs ! Number of interaction coefficients
        REAL, POINTER :: CINT(:) => null()
        INTEGER, POINTER :: I1M(:) => NULL()
        INTEGER, POINTER :: JIN(:) => NULL()
        INTEGER, POINTER :: KIN(:) => NULL()
    end type CintArrays

    ! This will contain all the interaction coefficients and it shall be treated as a global variable.
    ! It is defined here and only here.
    type(CintArrays) :: gCintArr

    real, allocatable, target :: BI(:) ! Define one array that encompasses all real interaction coeffcient arrays
    integer, allocatable, target :: IBI(:) ! Define one array that encompasses all integer interaction coefficient arrays

    contains
        !****************************************************************************
        !>  Function computeInteractionCoefficients to compute interaction coefficients 
        !>  for a spectral channel model. It is messy and confusing. Sorry.
        !>  This method is intended to be called only by the AllocateCintArrays method.
        !>------------------------------- METHOD -------------------------------------
        !>  The interaction coefficients are given by
        !>            cijk = [1/(2*PI*PI)] Integral [Fi*J(Fj,Fk)].
        !>  where Fi = 2cosanx sindy  Fj = 2cosbnx siney  Fk = 2coscnx sinfy
        !>        Fi'= 2sinanx sindy  Fj'= 2sinbnx siney  Fk'= 2sincnx sinfy
        !>  and the zonal form Fio = SQRT(2)cosly.
        !>
        !>  After the application of the Jacobian one gets the following possible com-
        !>  binations of terms:
        !>
        !> i j k = 8n[-bfcosanx sinbnx coscnx Y1 + ce cosanx cosbnx sincnx Y2]
        !> i j k'= 8n[-bfcosanx sinbnx sincnx Y1 - ce cosanx cosbnx coscnx Y2]
        !> i j'k = 8n[ bfcosanx cosbnx coscnx Y1 + ce cosanx sinbnx sincnx Y2]
        !> i j'k'= 8n[ bfcosanx cosbnx sincnx Y1 - ce cosanx sinbnx coscnx Y2]
        !>  where Y1=sindy siney cosfy  and Y2=sindy cosey sinfy.
        !>
        !> i'j k = 8n[-bfsinanx sinbnx coscnx Y1 + ce sinanx cosbnx sincnx Y2]
        !> i'j k'= 8n[-bfsinanx sinbnx sincnx Y1 - ce sinanx cosbnx coscnx Y2]
        !> i'j'k = 8n[ bfsinanx cosbnx coscnx Y1 + ce sinanx sinbnx sincnx Y2]
        !> i'j'k'= 8n[ bfsinanx cosbnx sincnx Y1 - ce sinanx sinbnx coscnx Y2]
        !>
        !>  where Y1=sindy siney cosfy  and Y2=sindy cosey sinfy.
        !>
        !>  The zonal interaction coefficients have the following form:
        !>
        !> ioj k = 4nSQ2[-bfsinbnx coscnx Y3 + ce cosbnx sincnx Y4]
        !> ioj k'= 4nSQ2[-bfsinbnx sincnx Y3 - ce cosbnx coscnx Y4]
        !> ioj'k = 4nSQ2[ bfcosbnx coscnx Y3 + ce sinbnx sincnx Y4]
        !> ioj'k'= 4nSQ2[ bfcosbnx sincnx Y3 - ce sinbnx coscnx Y4]
        !>
        !>  where Y3=cosly siney cosfy  and  Y4=cosly cosey sinfy.
    
        !>  From the x integral, we see that the ioj k and ioj'k' interaction 
        !>  coefficients are zero.  The only non zero values come if b=c in the two
        !>  that remain.
        !>  In order to do the integration, we have to express the triple products of
        !>  the above funtions as sine and cosine of sums of angles.  After lots of
        !>  blood sweat and tears we get:
        !>
        !>  cosa cosb cosc = [ cosA + cosB + cosC + cosD]/4
        !>  cosa sinb sinc = [-cosA + cosB - cosC + cosD]/4
        !>  sina sinb cosc = [-cosA + cosB - cosC + cosD]/4
        !>  sina cosb sinc = [ cosA + cosB - cosC - cosD]/4
        !>
        !>  cosa cosb sinc = [ sinA - sinB - sinC - sinD]/4
        !>  cosa sinb cosc = [ sinA + sinB - sinC + sinD]/4
        !>  sina sinb sinc = [-sinA + sinB - sinC - sinD]/4
        !>  sina cosb cosc = [ sinA + sinB + sinC - sinD]/4
        !>
        !>  where A=a+b+c  B=a+b-c  C=a-b-c  and  D=-a+b-c.
    
        !>  From the form of the integrals over 2*PI we see that only the first group
        !>  of triple products which have cosine forms as the sum will be non zero. Of
        !>  these, the only way to get a nonzero result is if A, B, C, or D is zero.
        !>  This limits the above combinations from 8 to 4. 
        !>  Then the integral will be over a constant.  For the half channel integration
        !>  over PI we must also consider the sine forms.  It turns out that all the
        !>  y-dependent products to be integrated from 0 to PI have only the form in
        !>  the second set (sine forms).  For these integrals to be nonzero, the values
        !>  A, B, C, or D must be odd.  But the form of the solution is much messier
        !>  since the integral gives -cosA/A, etc.
        !>
        !>  The forms of the interaction coefficients then appear as follows:
        !>
        !> i j k'= n/2[-bf( K2-K3+K4) Y1 - ce( K2+K3+K4) Y2]
        !> i j'k = n/2[ bf( K2+K3+K4) Y1 + ce( K2-K3+K4) Y2]
        !> i'j k = n/2[-bf(-K2+K3+K4) Y1 + ce( K2+K3-K4) Y2]
        !> i'j'k'= n/2[ bf( K2+K3-K4) Y1 - ce(-K2+K3+K4) Y2]
        !>
        !>  where Y1=(-L2+L3+L4)  and Y2=(L2+L3-L4) and 
        !>  K1=cos( a+b+c)      L1=cos( d+e+f)
        !>  K2=cos( a+b-c)      L2=cos( d+e-f)
        !>  K3=cos( a-b-c)      L3=cos( d-e-f)
        !>  K4=cos(-a+b-c)      L4=cos(-d+e-f)
        !>  which is one if the sum is zero, otherwise it is zero.
    
        !>  The zonal interaction terms are as follows:
        !>
        !> ioj k' = -SQRT2 n[bf(1/Z1+1/Z2-1/Z3+1/Z4) + ce(1/Z1-1/Z2-1/Z3-1/Z4)]/PI
        !>  and ioj'k = -ioj k' and
        !>  Z1=l+e+f  Z3= l-e-f
        !>  Z2=l+e-f  Z4=-l+e-f.
        !>  We order the eigenfunctions as zonal first, and then the waves, with all
        !>  the y-modes first.  Recall that the k-waves have both a cos and sin part
        !>  and the cos part comes first
        !>  So if the number of waves is KMAX,LMAX, the total number
        !>  of eigenfunctions is LMAX+2*KMAX*LMAX.  In the single array numbering,
        !>  which goes from 1 to IMAX, the first LMAX are zonal only.  Thus the K,L
        !>  wavenumber of a given ith element is given by:
        !>      K=((I-1)-LMAX)/(2*LMAX)+1
        !>      L=((I-1)+LMAX*(1-2*K))/2   for I>LMAX.  Otherwise it is just L=I, K=0.

        !>------------------------------- Function behavior ---------------------
        !> One should not call this function directly! Instead, call AllocateCintArrays()
        !> That method will first call this function with flag = 0 to compute the number
        !> of interaction coefficients. It will then call it again to allocate the arrays and
        !> populate the arrays given the number of interaction coefficients.

        !! gCintArr: contains
        !!      I1M: The number of interactions per equation
        !!      JIN: The jth variable of the interaction
        !!      KIN: The kth variable of the interaction
        !!      C:   The array of interaction coefficients Cijk of size of numOfIntCoefs
        !! @param RN: Wavenumber aspect ratio for channel (part of the modelParams type in ModelParamsModule)
        !! @param KMX2: The number of zonal waves
        !! @param LMX2: The number of meridional waves
        !! @param flag: indicates whether to just compute the number of interaction coefficients
        !!          or compute the actual interaction coefficients as well.
        !!          If flag = 0 on INPUT,  the routine just computes the number of interactions and
        !!          returns numOfIntCoefs without filling the arrays. This option is for allocation.
        !!          ALLOCATION must be done before this method is called with flag not 0

        !!          If flag is not 0 the arrays are also filled. If the number exceeds 75834 or 11x11, 
        !!          then numOfIntCoefs = -1 and the routine stops. This means use the transform method

        !! @returns numOfIntCoeffs: which is the number of interaction coefficients. If the
        !!          value returned is -1, there are too many and one should use the transform method
        function computeInteractionCoefficients(RN, KMX2, LMX2, flag) result(numOfIntCoeffs)
            use ConstantsModule, only: PI, SQRT2

            implicit none

            real, intent(in) :: RN
            integer, intent(in) :: KMX2, LMX2, flag

            !---------------------------------------Local variables:
            integer :: numOfIntCoeffs
            INTEGER :: KMAX, LMAX, TwoLmax, INDX, ICOS, JCOS, KCOS, I, LI, KI, I1, J
            INTEGER :: LJ, JSTART, K, KK, KSTART, LK, KJ, LT, LX, KS, K2, K3, K4, L2
            INTEGER :: L3, L4, IJK, LS, ITOT
            REAL :: RN2, T1, T2, Z1, Z2, Z3, Z4, ZJK

            if (flag > 0 .AND. (.NOT. gCintArr%allocated)) then
                numOfIntCoeffs = -1
                return
            end if
        !********************************************************* START: RN
            KMAX = KMX2
            LMAX = LMX2
            ITOT = LMAX + 2 * LMAX * KMAX
            RN2 = RN * RN
            TwoLmax = 2 * LMAX
        !
        !  ICOS, JCOS, and KCOS are variables which are 3, 0 or 1 depending upon the
        !  type of wave.  A 3 means a zonal component, a 0 a cosnkx and a 1 a sinnkx.
        !  Actually, KCOS is never a zonal component.  The wave-wave coefficients
        !  are straight forward:  When I and J are greater than LMAX, K goes from J+1
        !  to ITOT while I and J goes from LMAX+1 to ITOT.  Four separate routines
        !  compute the interaction coefficients depending upon the I,J,K-COS values.
        !  To compute the zonal quantities, we use just the I and J indices.  When
        !  I and J are both less than LMAX+1, we hop to the next J.  When J is greater
        !  than LMAX, we compute Cio,j,k' or Cio,j',k.  K is always greater than J
        !  during this phase of the loop.  The value of JCOS (or KCOS) tell which of
        !  the two interactions are correct.  When I is greater than LMAX and J is less
        !  than LMAX, we have to be a little careful.  First we flip the I and J in 
        !  the calculation so it has the same form as the io,j,k.  Thus we must neg-
        !  ate the result.  If ICOS is also 1, the result is again negated since since
        !  we have a Cio,j',k form.  Thus if J is less tha LMAX+1 and ICOS is 0, we
        !  negate the coefficient.  Otherwise we leave it alone.
        !
        !  KI,LI KJ,LJ KK,LK are the wavenumbers of the respective waves:
        !
            INDX = 1
            ICOS = 3
            DO I = 1, ITOT
                !----------------------  Now we treat I as the zonal index (less than LMAX+1)
                LI = I
                KI = 0
                JSTART = LMAX + 1
                !
                !  If I is greater than LMAX we shall use I as the j element if J<LMAX+1 or
                !  as part of the wave-wave computation.  We treat it as a wave in any case.
                !
                IF(I .GT. LMAX) THEN
                    ICOS = MOD(ICOS + 1, 2)
                    KI = 1 + ((I - 1) - LMAX) / TwoLmax
                    LI = 1 + ((I - 1) + LMAX * (1 - 2 * KI)) / 2
                    JSTART = 1
                END IF
                JCOS = 3
                I1 = 1
                DO J = JSTART, ITOT
                    !--------------Start J as a zonal index:
                    LJ = J
                    KJ = 0
                    !------------- If both I and J less than LMAX+1, no computation:
                    IF(I .LE. LMAX .AND. J .LE. LMAX) cycle ! Back to the start of this do loop
                    
                    KSTART = LMAX + 1
                    KCOS = 1
                    !-------------------------------------------- Treat J like a wave for J>LMAX:
                    IF(J .GT. LMAX) THEN
                        JCOS = MOD(JCOS + 1, 2)
                        KJ = 1 + ((J - 1) - LMAX) /TwoLmax
                        LJ = 1 + ((J - 1) + LMAX * (1 - 2 * KJ)) / 2
                        KSTART = J + 1
                        KCOS = JCOS
                    END IF
                    !----------------------------------------------K is always treated as a wave:
                    DO K = KSTART, ITOT
                        KCOS = MOD(KCOS + 1, 2)
                        KK = 1 + ((K - 1) - LMAX) / TwoLmax
                        LK = 1 + ((K - 1) + LMAX * (1 - 2 * KK)) / 2
                        !-------------------------------------------------------- Zonal coefficients:
                        !
                        !  The zonal computation only applies when either I or J is less than LMAX+1.
                        !  When I is less than LMAX+1, this is the "normal" situation.
                        !
                        IF(I .LE. LMAX .OR. J .LE. LMAX) THEN
                            IF(I .LE. LMAX) THEN
                                !
                                !  The zonal wavenumber of the waves must be the same, and one component must
                                !  be a sinknx while the other is a cosknx.  If not, the value is zero.
                                !
                                IF(KK.NE.KJ) cycle ! Back to the start of this do loop
                                IF(JCOS.EQ.KCOS) cycle ! Back to the start of this do loop
                                
                                T1=REAL(KJ*LK)
                                T2=REAL(KK*LJ)
                                LS=LI
                                LT=LJ
                            END IF
                            !-------------------------------------------------Reverse I,J for J<LMAX+1:
                            IF(J .LE. LMAX) THEN
                                IF(KK.NE.KI) cycle ! Back to the start of this do loop
                                IF(ICOS.EQ.KCOS) cycle ! Back to the start of this do loop
                                T1=REAL(KI*LK)
                                T2=REAL(KK*LI)
                                LS=LJ
                                LT=LI
                            END IF
                            LX=LS+LT+LK
                            !
                            ! If KS is even, all sums are even, and we can only have a nonzero value if
                            ! KS is odd.
                            !
                            KS=MOD(LX,2)
                            IF(KS.EQ.0)  cycle ! Back to the start of this do loop
                            
                            !------------------------------------------------- Count option only:
                            IF(flag .EQ. 0)GO TO 1005
                            
                            Z1 = 1./REAL(LX)
                            Z2 = 1./REAL(LS+LT-LK)
                            Z3 = 1./REAL(LS-LT-LK)
                            Z4 = 1./REAL(-LS+LT-LK)
                            ZJK = -T1*(Z1+Z2-Z3+Z4)-T2*(Z1-Z2-Z3-Z4)
                            !------------------------------------------------- Check for Cijk=-Cjik=Ckij:
                            IF(J.LE.LMAX.AND.ICOS.EQ.0) ZJK=-ZJK
                            IF(J.GT.LMAX.AND.JCOS.EQ.1) ZJK=-ZJK
                            ZJK = RN * SQRT2 * ZJK / PI
                            GO TO 1005
                        END IF
                        !---------------------------------------- Now for the wave-wave interactions:
                        !  The only nonzero contributions come from these sums being odd.  If they are
                        !  odd, then they contribute a 1 to the sum, otherwise they are zero. First we
                        !  do the x-contribution, and then the y-contribution.
                        !
                        K2=0
                        KS=KI+KJ-KK
                        IF(KS.EQ.0)K2=1
                        K3=0
                        KS=KI-KJ-KK
                        IF(KS.EQ.0)K3=1
                        K4=0
                        KS=-KI+KJ-KK
                        IF(KS.EQ.0)K4=1

                        L2=0
                        KS=LI+LJ-LK
                        IF(KS.EQ.0)L2=1
                        L3=0
                        KS=LI-LJ-LK
                        IF(KS.EQ.0)L3=1
                        L4=0
                        KS=-LI+LJ-LK
                        IF(KS.EQ.0)L4=1
                        !-------------------I  Computation:
                        IF(ICOS.EQ.0) THEN
                            !----------------------- Here we must have i j k' or i j' k for nonzero value:

                            IF(JCOS.EQ.KCOS) cycle ! Back to the start of this do loop

                            IF(JCOS.EQ.0) IJK = -KJ*LK*(K2-K3+K4)*(-L2+L3+L4)-KK*LJ*(K2+K3+K4)*(L2+L3-L4)
                            IF(JCOS.EQ.1) IJK = KJ*LK*(K2+K3+K4)*(-L2+L3+L4)+KK*LJ*(K2-K3+K4)*(L2+L3-L4)
                        END IF
                        !-------------------I' Computation:
                        IF(ICOS .EQ. 1) THEN
                            IF(JCOS.NE.KCOS) cycle  ! Back to the start of this do loop

                            IF(JCOS.EQ.0) IJK = -KJ*LK*(-K2+K3+K4)*(-L2+L3+L4)+KK*LJ*(K2+K3-K4)*(L2+L3-L4)
                            IF(JCOS.EQ.1) IJK = KJ*LK*(K2+K3-K4)*(-L2+L3+L4)-KK*LJ*(-K2+K3+K4)*(L2+L3-L4)
                        END IF
                        IF(IJK .EQ. 0) cycle ! Back to the start of this do loop
                        ZJK=RN*REAL(IJK)/2.
        1005            CONTINUE
                        !
                        !  Store all the coefficients as one long string where the number of elements
                        !  for each equation (I) is given by I1MAX(I).  In the nonlinear part of the
                        !  model we repeat a similar loop simply incrementing INDX each pass through
                        !  the I1 loop (giving the sum over the I I1MAX elements).
                        !
                        IF(flag .NE. 0)THEN
                            gCintArr%CINT(INDX) = ZJK
                            gCintArr%JIN(INDX) = J
                            gCintArr%KIN(INDX) = K
                            gCintArr%I1M(I) = I1
                        END IF
                        !-------------------------------------------- Increment the counter by one:
                        INDX = INDX + 1
                        !-------------------------------------------- Maximum index value:
                        IF(INDX .GT. 75835) THEN
                            numOfIntCoeffs = -1
                            gCintArr%numOfInteractionCoeffs = -1
                            RETURN
                        END IF
                        I1 = I1 + 1
                    END DO
        1000    END DO
            END DO

            numOfIntCoeffs = INDX - 1
            gCintArr%numOfInteractionCoeffs = numOfIntCoeffs
        END FUNCTION computeInteractionCoefficients


        !> Function to allocate the interaction coefficients arrays and compute the interaction coefficients
        !> THis allocation method is a little different than the other ones since here we also create the
        !> gCintArr arrays based upon the gUserSettings base parameters and calculate the the interaction coefficients.
        !> That is because these will change for any change in the horizontal resolution requiring deallocation and 
        !> reallocation and then recomputation. On success the arrays in gCintArr will be populated.
        !! gArrayDims: The NINTD field is the only field used. You do not have to set it to any value.
        !!                   Upon return it will contain the number of interaction coefficients.
        !! gUserSettings: This type contains the number of zonal and meridional waves as well as a flag
        !!                      To indicate which integration method to use. The caller has to provide these
        !!                      The integration method must be set to USE_INTERACTION_COEFFCIENTS or this 
        !!                      method will return with a non-zero status. If there are too many interaction
        !!                      coefficients, the integration method will be switched to USE_TRANSFORM.
        !! @returns status: -1 if the interaction coefficients are not used and the transform method is being used
        !!                     0 if successful
        function AllocateCintArrays() result(status)
            use ModelParamsModule, only: gUserSettings, gModRealParams, gModIntParams

            ! Local variables
            INTEGER :: totalSize, status, ITOT, currentNoOfInteractions

            ITOT = gUserSettings%maxMeridWaves + 2 * gUserSettings%maxZonalWaves * gUserSettings%maxMeridWaves
            gCintArr%numOfInteractionCoeffs = -1
            status = 0
            !----------------------------------Compute ONLY number of interaction coefs:
            ! With a seting of the flag to 0 we will only count the number of interaction coefficeints
            ! given the number of zonal and meridional waves. If there are too many, (return value -1)
            ! then we switch to the transform method and do not allocate the interaction coefficients arrays.

            IF(.not. gUserSettings%useTransformMethod) then ! False means USE_INTERACTION_COEFFICIENTS
                !----------------------------------Switch to transform method when gArrayDims%NINTD = -1 (too many interaction coefficients)
                gCintArr%numOfInteractionCoeffs = computeInteractionCoefficients(gModRealParams%RN, &
                                                gUserSettings%maxZonalWaves, gUserSettings%maxMeridWaves, 0)
                IF(gCintArr%numOfInteractionCoeffs .EQ. -1) THEN
                    gUserSettings%useTransformMethod = .TRUE.
                    status = -1 ! Signal that interaction coefficients are not used and transform method is being used
                    return
                end if
            else
                status = -1 ! Signal that interaction coefficients are not used and transform method is being used
                return
            end if

            ALLOCATE(BI(gCintArr%numOfInteractionCoeffs), STAT = status) ! Single contiguous memory block for real arrays (there is only one now)
            if (status /= 0) THEN
                gUserSettings%useTransformMethod = .TRUE.
                gCintArr%numOfInteractionCoeffs = -1
                status = -1 ! Signal that interaction coefficients are not used and transform method is being used
                RETURN
            END IF
            !-------------------------------------Size of JIN, KIN, and I1M arrays
            totalSize = 2 * gCintArr%numOfInteractionCoeffs + ITOT
            ALLOCATE(IBI(totalSize), STAT = status) ! Single contiguous memory block for integer arrays
            if (status /= 0) THEN
                gUserSettings%useTransformMethod = .TRUE.
                deallocate(BI, STAT = status)
                gCintArr%numOfInteractionCoeffs = -1
                status = -1 ! Signal that interaction coefficients are not used and transform method is being used
                RETURN
            end if
    !! ===== DANGER!!  DO NOT f10 THESE TWO STEPS IN DEBUGGER !!!
            BI(:) = 0.0 ! Zero entire set of real arrays
            IBI(:) = 0 ! Zero entire set of integer arrays

            gCintArr%allocated = .true. ! Set the allocated flag to true

            currentNoOfInteractions = computeInteractionCoefficients(gModRealParams%RN, &
                                                gModIntParams%KMAX2, gModIntParams%LMAX2, 0)

            status = createCintArr(gModIntParams%KMAX2, gModIntParams%LMAX2, currentNoOfInteractions)
            if (status /= 0) THEN
                gUserSettings%useTransformMethod = .TRUE.
                status = DeallocateCintArrays()
                gCintArr%numOfInteractionCoeffs = -1
                status = -1 ! Signal that interaction coefficients are not used and transform method is being used
                RETURN
            END IF

            ! Now compute the interaction coefficients
            ! gArrayDims%NINTD will have a value > 0 if interaction coefficients are used
            !gCintArr%numOfInteractionCoeffs = computeInteractionCoefficients(gModRealParams%RN, &
            !                                    gModIntParams%KMAX2, gModIntParams%LMAX2, 1) ! flag = 1 means compute and fill arrays
        END FUNCTION AllocateCintArrays

        function createCintArr(zonal_waves, merid_waves, currentNoOfInteractions) RESULT(status)
            implicit none

            integer, intent(in) :: zonal_waves, merid_waves, currentNoOfInteractions

            integer :: status ! FORTRAN restriction, cant initialize a variable in result in the declaration statement.
                              ! Reason is that initializing a varaible in a declaration statement implicitly saves the
                              ! varaible like a C static varaible.

            ! local variables
            integer :: ITOT

            ITOT = merid_waves + 2 * zonal_waves * merid_waves

            if (.not. gCintArr%allocated .OR. gCintArr%numOfInteractionCoeffs < 1) then
                status = 1
                return
            end if

            if (gCintArr%numOfInteractionCoeffs > size(BI)) then
                status = 1
                return
            end if

            if ((2 * gCintArr%numOfInteractionCoeffs + ITOT) > size(IBI)) then
                status = 1
                return
            end if

            status = 0

            gCintArr%Cint(1 : currentNoOfInteractions)  => BI(1 : currentNoOfInteractions)
            gCintArr%JIN(1 :  currentNoOfInteractions)  => IBI(1 : currentNoOfInteractions)
            gCintArr%KIN(1 :  currentNoOfInteractions)  => IBI(gCintArr%numOfInteractionCoeffs + 1 : & 
                                gCintArr%numOfInteractionCoeffs + currentNoOfInteractions)
            gCintArr%I1M(1 :  ITOT)                     => IBI(2 * gCintArr%numOfInteractionCoeffs + 1 : &
                                2 * gCintArr%numOfInteractionCoeffs + ITOT)
            BI(:) = 0.0 ! Zero entire set of real arrays  - ===== DANGER!!  DO NOT F10 THESE TWO STEPS IN DEBUGGER !!!
            IBI(:) = 0 ! Zero entire set of integer arrays
            gCintArr%defined = .true.

        end function createCintArr

        !> Function to deallocate the interaction coefficients arrays and nullify the gCintArr type
        !> Be sure to call at termination or before reallocation for a different resolution.
        function DeallocateCintArrays() result(statusErr)
            integer statusErr

            statusErr = 0

            IF (ALLOCATED(BI)) DEALLOCATE(BI)
            nullify(gCintArr%CINT)
            IF (ALLOCATED(IBI)) DEALLOCATE(IBI)
            Nullify(gCintArr%JIN)
            Nullify(gCintArr%KIN)
            Nullify(gCintArr%I1M)
            gCintArr%allocated = .false. ! Set the allocated flag to false
            gCintArr%defined = .false.   ! Set the defined flag to false
            gCintArr%numOfInteractionCoeffs = 0

        end function DeallocateCintArrays

end module InteractionCoefficientsModule

!---------------------------------------------------------------------------
!  Table of the number of interaction coefficients for a given resolution:
!    K  1     2     3     4     5     6     7     8     9    10    11
!  L
!  1    3     6     9    12    15    18    21    24    27    30    33
!  2   12    36    84   144   228   324   444   576   732   900  1092
!  3   42   132   270   468   726  1020  1386  1800  2262  2784  3366
!  4   96   288   588   984  1512  2124  2868  3696  4644  5700  6876
!  5  189   546  1083  1788  2673  3738  5007  6432  8049  9822 11811
!  6  324   900  1740  2844  4224  5844  7788  9972 12432 15144 18168
!  7  516  1392  2640  4260  6264  8616 11364 14496 18012 21888 26184
!  8  768  2016  3768  6000  8760 11988 15744 19968 24744 30000 35808
!  9 1095  2814  5169  8160 11823 16086 21033 26592 32737 39678 47253
! 10 1500  3780  6864 10740 15432 20904 27228 34320 42228 50916 60516
! 11 1998  4956  8898 13812 19722 26592 34494 43344 53190 63996 75834
!---------------------------------------------------------------------------
