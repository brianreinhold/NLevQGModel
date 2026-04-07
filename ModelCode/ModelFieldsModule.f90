! The allocation and definition of all arrays used in the model that are resized based upon user parameters
! will use the following procedure in order to have the dimensions defined at all times in the code. This
! apporach is necessary in F90+. In F77 could change the shape and dimensions freely at the cost of having
! undetected overwrites and memory issues. Explicitely defining the actual array shapes and sizes in F90+ prevents
! These types of errors.

! We will be using the apprach shown below. The interaction coefficient arrays are used as an example since they 
! have so few members whcih makes the appoach easier to follow. The interaction coefficient arrays are defined in
    ! their own module InteractionCoeffsModule.f90
! Define memory array pool
!    real, allocatable, target :: BI(:)     ! Define one array that encompasses all real interaction coefficient arrays
!    integer, allocatable, target :: IBI(:) ! Define one array that encompasses all integer interaction coefficient arrays
! and pointer arrays that can be configured to have changeable dimensions based on user parameters
!    REAL, POINTER :: CINT(:) => null()
!    INTEGER, POINTER :: I1M(:) => NULL()
!    INTEGER, POINTER :: JIN(:) => NULL()
!    INTEGER, POINTER :: KIN(:) => NULL()
!
! Allocate the pool based upon some 'base' parameter determined from user settings
!    ALLOCATE(BI(arrayDims%NINTD), STAT = status)
!    ALLOCATE(IBI(totalSize), STAT = status)
!
! Use a create function to define the arrays in the pool. When the user sets new parameters this create function is
! is simply called again as long as the new parameters do not exceed the base value defining the base memory pool.
!    cintArr%Cint(1 : numberOfInteractionCoeffs)  => BI(1 : numberOfInteractionCoeffs)
!    cintArr%JIN(1 :  numberOfInteractionCoeffs)  => IBI(1 : numberOfInteractionCoeffs)
!    cintArr%KIN(1 :  numberOfInteractionCoeffs)  => IBI(numberOfInteractionCoeffs : 2 * numberOfInteractionCoeffs)
!    cintArr%I1M(1 :  ITOT)                       => IBI(2 * numberOfInteractionCoeffs : ITOT)
!
! Deallocate if the user parameter exceeds the current base value
!
!    IF (ALLOCATED(BI)) DEALLOCATE(BI)
!    nullify(cintArr%CINT)
!    IF (ALLOCATED(IBI)) DEALLOCATE(IBI)
!    Nullify(cintArr%JIN)
!    Nullify(cintArr%KIN)
!    Nullify(cintArr%I1M)
!
! Then the whole allocation and creation process has to be repeated if base value is exceeded with a new base value.

MODULE ModelFieldsModule
    IMPLICIT NONE

    ! Subroutines and functions
    public :: AllocateVerticalArrays, DeallocateVerticalArrays
    public :: AllocateHorizontalArrays, DeallocateHorizontalArrays
    public :: AllocateStatsArrays, DeallocateStatsArrays

    ! Variables
    public :: vertArr, horzArr, statsArr
    public :: BV

    !========================== Vertical arrays
    !  BV(*) contains all vertical profile arrays in one contiguous block.
    ! Consider a 11-layer model
    !  User specifies PV as follows:
    !  PV(11) =     975, 925, 850, 750, 650, 550, 450, 350, 250, 150, 50 mb
    !  Then the user must provide values for the mean potential temperature at each of these levels (THS)
    !  for the computation of the statis stability
    !  THS(11)
    !  PP(12) = 1000, 950, 900, 800, 700, 600, 500, 400, 300, 200, 100, 0 mb
    !  DP(11) =     50,   50, 100, 100, 100, 100, 100, 100, 100, 100, 100, mb
    ! 
    TYPE :: VertArrays
        LOGICAL :: allocated = .FALSE. ! Flag to indicate if arrays are allocated
        LOGICAL :: defined = .FALSE.   ! Flag to indicate if arrays are defined with proper sizes
        REAL, POINTER :: PV(:) => null()    !  PV(maxLevels) is the user specified levels of the streamfunction. IN a two-layer
                                            !                model this would be at 750 and 250 mb. The interfaces (PP)
                                            !                would be 1000, 500, and 0 (top) mb.
        REAL, POINTER :: DP(:) => null()    !  DP(maxLevels)=The layer thickness
        REAL, POINTER :: DPI(:) => null()   !  DPI(maxLevels)=1./(PP(N-1)-PP(N)) is the inverse of the layer thickness
        REAL, POINTER :: THS(:) => null()   !  THS(maxLevels) is the user specified mean temperature at the streamfunction level for static stability.
        REAL, POINTER :: FK(:) => null()    !  FK(maxLevels) is the ekman dissipation of the streamfunction at each level
        REAL, POINTER :: FJ(:) => null()    !  FJ(maxLevels) is the turbulent transfer of the streamfunction between levels
        REAL, POINTER :: AU(:) => null()    !  AU(maxLevels) is ratio of the thermal wind A between layers for turbulent transfer
        REAL, POINTER :: A(:) => null()     !  A(maxLevels) is the thermal wind constant
        REAL, POINTER :: H(:) => null()     !  H(maxLevels) is the Newtonian cooling at each level on the temperature
        REAL, POINTER :: G(:) => null()     !  G(maxLevels) is the turbulent transfer of temperature at each level
        REAL, POINTER :: SIG(:) => null()   !  SIG(maxLevels) is the static stability at the interface between each layer
        REAL, POINTER :: X(:) => null()     !  X(maxLevels), Y(maxLevels) is used in the iterative procedure for omega
        REAL, POINTER :: Y(:) => null()     !  X(maxLevels), Y(maxLevels) is used in the iterative procedure for omega
        REAL, POINTER :: PP(:) => null()    !  PP(maxLevels+1)=The pressures at each interface. 1=sfc and maxLevels+1=Top
        REAL, POINTER :: PRF(:) => null()   !  PRF(maxLevels,4) are profile arrays for functional behavior of model
    END TYPE VertArrays

    TYPE(VertArrays) :: vertArr
    REAL, ALLOCATABLE, TARGET :: BV(:)      ! Define one array that encompasses all vertical arrays

    !=============================== Horizontal arrays
        !TYPE :: ChainHorzArrays
        !    LOGICAL :: allocated = .FALSE. ! Flag to indicate if arrays are allocated
        !    REAL, POINTER :: GQ(:),BTA(:),PSB(:),PS(:),TH(:),WW(:),TF(:),FS(:),ADS(:),WS(:),S(:),FT(:),ADT(:),WT(:),T(:),SF(:),ADO(:),TOP(:)
        !    REAL, POINTER :: G1(:),G2(:),G3(:),WRK(:),TMP(:),DAT(:),AKL(:),AKLI(:),ZS(:),PSSAVE(:),RML(:),TRX(:),TRY(:)
        !    INTEGER, POINTER :: IXL(:),IYL(:)
        !END TYPE ChainHorzArrays
    ! Unfortunately Fortran does not allow allocatable target arrays in a type

    REAL, ALLOCATABLE, TARGET :: GQ_pool(:,:)     !  GQ(ITOT, maxLevels) is the cosG sinG (QG) values between the mean PS, TH of a layer
    REAL, ALLOCATABLE, TARGET :: BTA_pool(:)      !  BTA(ITOT) is the mean vertically (spatially) averaged flow 
    REAL, ALLOCATABLE, TARGET :: PSB_pool(:)      !  PSB(ITOT) is the vert. mean flow for the waves in the modelType = ZONAL_MEAN_FLOWS option
    REAL, ALLOCATABLE, TARGET :: PS_pool(:)       !  PS(ITOT,maxLevels) is the streamfunction spectral components
                                                  !  Here we effectively chain all 14 2-d arrays from PS to SF into one big 1-d PS array
                                                  !  The individual contained arrays are:
                                                    !  TH(ITOT,maxLevels) is the temperature spectral components
                                                    !  WW(ITOT,maxLevels) is the omega spectral components
                                                    !  TF(ITOT,maxLevels) is the thermal forcing field
                                                    !  FS(ITOT,maxLevels) is the streamfunction tendency
                                                    !  ADS(ITOT,maxLevels) is the spectral form of the vorticity advection
                                                    !  WS(ITOT,maxLevels) is the omega forcing on the streamfunction
                                                    !  S(ITOT,maxLevels) is the diabatic forcing on the streamfunction
                                                    !  FT(ITOT,maxLevels) is the temperature tendency
                                                    !  ADT(ITOT,maxLevels) is the spectral form of the temperature advection
                                                    !  WT(ITOT,maxLevels) is the omega forcing on the temperature
                                                    !  T(ITOT,maxLevels) is the diabatic forcing on the temperature
                                                    !  SF(ITOT,maxLevels) is the Barotropic momentum forcing field
    REAL, ALLOCATABLE, TARGET :: ADO_pool(:)      !  ADO(ITOT) is the spectral form of the forcing by srf flow over orography
    REAL, ALLOCATABLE, TARGET :: TOP_pool(:)      !  TOP(ITOT) is the surface orographic field
    REAL, ALLOCATABLE, TARGET :: G1_pool(:)       !  G1(KX*LY) grid point arrays for transform routine
    REAL, ALLOCATABLE, TARGET :: G2_pool(:)       !  G2(KX*LY) grid point arrays for transform routine
    REAL, ALLOCATABLE, TARGET :: G3_pool(:)       !  G3(KX*LY) grid point arrays for transform routine
    REAL, ALLOCATABLE, TARGET :: G4_pool(:)       !  G4(KX*LY) grid point arrays for transform routine
    COMPLEX, ALLOCATABLE, TARGET :: Gc_pool(:)    !  Gc(KX) complex grid point array for the periodic transform routine
    REAL, ALLOCATABLE, TARGET :: WRK_pool(:)      !  WRK(IBIG>KX*LY) is a work array for FFT991 routine
    real, allocatable, target :: GZonal_pool(:)   !  G1Zonal(LY) grid point array for zonal component of transform 
    REAL, ALLOCATABLE, TARGET :: TMP_pool(:)      !  TMP(arrayDims%LYD)
    REAL, ALLOCATABLE, TARGET :: DAT_pool(:)      !  DAT(ITOT) is an all purpose array for computing some variable at a level.
    REAL, ALLOCATABLE, TARGET :: AKL_pool(:)      !  AKL(ITOT) two dimensional wavenumber used for calculating things like the vorticity
    REAL, ALLOCATABLE, TARGET :: AKLI_pool(:)     !  AKLI(ITOT) are the two dimensional inverse wavenumber used for calculating things like the vorticity
    REAL, ALLOCATABLE, TARGET :: ZS_pool(:,:)     !  ZS(ITOTD,maxLevels)
    REAL, ALLOCATABLE, TARGET :: PSSAVE_pool(:,:) !  PSSAVE(ITOTD,maxLevels)
    REAL, ALLOCATABLE, TARGET :: RML_pool(:,:)    !  RML(2*maxMeridWaves+1,maxMeridWaves) is used in the projection routines in inverse transform.
    REAL, ALLOCATABLE, TARGET :: TRX_pool(:)      !  TRX(arrayDims%ITRXD)
    REAL, ALLOCATABLE, TARGET :: TRY_pool(:)      !  TRY(arrayDims%ITRYD)
    
    INTEGER, ALLOCATABLE, TARGET :: IXL_pool(:)   !  IXL() array for plotting graphs at all levels or wavenumbers (probably hovmoller horizontal) and cross sections
    INTEGER, ALLOCATABLE, TARGET :: IYL_pool(:)   !  IYL() array for plotting graphs at all levels or wavenumbers (probably hovmoller meridional) and cross sections

    TYPE :: HorzArrays
        LOGICAL :: allocated = .FALSE.          !  Flag to indicate if arrays are allocated and defined
        LOGICAL :: defined = .FALSE.            !  Flag to indicate if arrays are defined with proper sizes
        REAL, POINTER :: GQ(:,:) => null()      !  GQ(ITOT, maxLevels) is the cosG sinG (QG) values between the mean PS, TH of a layer
        REAL, POINTER :: BTA(:) => null()       !  BTA(ITOT) is the mean vertically (spatially) averaged flow 
        REAL, POINTER :: PSB(:) => null()       !  PSB(ITOT) is the vert. mean flow for the waves in the modelType = ZONAL_MEAN_FLOWS option
        REAL, POINTER :: PS(:,:) => null()      !  PS(ITOT,maxLevels) is the streamfunction spectral components
        REAL, POINTER :: TH(:,:) => null()      !  TH(ITOT,maxLevels) is the temperature spectral components
        REAL, POINTER :: WW(:,:) => null()      !  WW(ITOT,maxLevels) is the omega spectral components
        REAL, POINTER :: TF(:,:) => null()      !  TF(ITOT,maxLevels) is the thermal forcing field
        REAL, POINTER :: FS(:,:) => null()      !  FS(ITOT,maxLevels) is the streamfunction tendency
        REAL, POINTER :: ADS(:,:) => null()     !  ADS(ITOT,maxLevels) is the spectral form of the vorticity advection
        REAL, POINTER :: WS(:,:) => null()      !  WS(ITOT,maxLevels) is the omega forcing on the streamfunction
        REAL, POINTER :: S(:,:) => null()       !  S(ITOT,maxLevels) is the diabatic forcing on the streamfunction
        REAL, POINTER :: FT(:,:) => null()      !  FT(ITOT,maxLevels) is the temperature tendency
        REAL, POINTER :: ADT(:,:) => null()     !  ADT(ITOT,maxLevels) is the spectral form of the temperature advection
        REAL, POINTER :: WT(:,:) => null()      !  WT(ITOT,maxLevels) is the omega forcing on the temperature
        REAL, POINTER :: T(:,:) => null()       !  T(ITOT,maxLevels) is the diabatic forcing on the temperature
        REAL, POINTER :: SF(:,:) => null()      !  SF(ITOT,maxLevels) is the Barotropic momentum forcing field
        REAL, POINTER :: ADO(:) => null()       !  ADO(ITOT) is the spectral form of the forcing by srf flow over orography
        REAL, POINTER :: TOP(:) => null()       !  TOP(ITOT) is the surface orographic field
        COMPLEX, POINTER :: Gc(:) => null()     !  Gc(KX) complex grid point array for the periodic transform routine
        REAL, POINTER :: G1(:, :) => null()     !  G1(IXMAX, IYMAX) grid point arrays for transform routines in 2 d
        REAL, POINTER :: G1plot(:, :) => null() !  G1plot(KX, LY) grid point arrays for transform routines in 2 d
        REAL, POINTER :: G1plot_1D(:) => null() !  G1plot(KX*LY) grid point arrays for transform routines in 1 d
        REAL, POINTER :: G2(:, :) => null()     !  G2(IXMAX, IYMAX) grid point arrays for transform routines in 2 d
        REAL, POINTER :: G2plot(:, :) => null() !  G2plot(KX*LY) grid point arrays for transform routines (?)
        REAL, POINTER :: G2plot_1D(:) => null() !  G2plot(KX*LY) grid point arrays for transform routines in 1 d
        REAL, POINTER :: G3(:, :) => null()     !  G3(IXMAX, IYMAX) grid point arrays for transform routines in 2 d
        REAL, POINTER :: G3plot(:, :) => null() !  G3plot(KX*LY) grid point arrays for transform routines (?)
        REAL, POINTER :: G3plot_1D(:) => null() !  G3plot(KX*LY) grid point arrays for transform routines in 1 d
        REAL, POINTER :: G4(:, :) => null()     !  G4(IXMAX, IYMAX) grid point arrays for transform routines in 2 d
        REAL, POINTER :: G4plot(:, :) => null() !  G4plot(KX*LY) grid point arrays for transform routines (?)
        REAL, POINTER :: G4plot_1D(:) => null() !  G4plot(KX*LY) grid point arrays for transform routines in 1 d
        REAL, POINTER :: WRK(:) => null()       !  WRK(IBIG>KX*LY) is a work array for FFT991 routine
        REAL, POINTER :: GZonal(:) => null()    !  GZonal(IYMAX) grid point array for zonal (cosly) transforms
        REAL, POINTER :: GZonalplot(:) => null()!  GZonal(LY) grid point array for zonal (cosly) transforms
        REAL, POINTER :: TMP(:) => null()       !  TMP(arrayDims%LYD)
        REAL, POINTER :: DAT(:) => null()       !  DAT(ITOT) is an all purpose array for computing some variable at a level.
        REAL, POINTER :: AKL(:) => null()       !  AKL(ITOT) two dimensional wavenumber used for calculating things like the vorticity
        REAL, POINTER :: AKLI(:) => null()      !  AKLI(ITOT) are the two dimensional inverse wavenumber used for calculating things like the vorticity
        REAL, POINTER :: ZS(:,:) => null()      !  ZS(ITOTD,maxLevels)
        REAL, POINTER :: PSSAVE(:,:) => NULL()  !  PSSAVE(ITOTD,maxLevels)
        REAL, POINTER :: RML(:,:) => null()     !  RML(2*maxMeridWaves+1,maxMeridWaves) is used in the projection routines in inverse transform.
        REAL, POINTER :: TRX(:) => null()       !  TRX(arrayDims%ITRXD)
        REAL, POINTER :: TRY(:) => null()       !  TRY(arrayDims%ITRYD)

        INTEGER, POINTER :: IXL(:) => null()    !  IXL() array for plotting graphs at all levels or wavenumbers (probably hovmoller horizontal) and cross sections
        INTEGER, POINTER :: IYL(:) => null()    !  IYL() array for plotting graphs at all levels or wavenumbers (probably hovmoller meridional) and cross sections
    END TYPE HorzArrays

    !TYPE(HorzArraysPool) :: horzArr_pool
    TYPE(HorzArrays) :: horzArr
    !REAL, ALLOCATABLE, TARGET :: BH(:)      ! Define one array that encompasses all real horizontal arrays
    !INTEGER, ALLOCATABLE, TARGET :: BHI(:)  ! Define one array that encompasses all integer horizontal arrays


    ! PSA: is the time average of the streamfunction at each level PSA(ITOTA,NALA,7)
    ! APE: is the time average available potential energy at each level APE(ITOTA,NALA)
    ! RKE: is the time average of the kinetic energy at each level RKE(ITOTA,NALA)
    ! PSS: is the time average of 'PSS = AMP(n)*AMPSTRM(1)[cosG(n),sinG(n)]' PSS(ITOTA,NALA,7)
    ! V1: is the time average of variance of the (streamfunction?) V1(KXV*LYV,NALA)
    ! V2: is the time average of variance of the (temperature?) ,V2(KXV*LYV,NALA)
    TYPE :: StatsArrays
        LOGICAL :: allocated = .FALSE. ! Flag to indicate if arrays are allocated
        REAL, ALLOCATABLE :: PSA(:,:,:)
        REAL, ALLOCATABLE :: APE(:,:)
        REAL, ALLOCATABLE :: RKE(:,:)
        REAL, ALLOCATABLE :: PSS(:,:,:)
        REAL, ALLOCATABLE :: V1(:,:)
        REAL, ALLOCATABLE :: V2(:,:)
        REAL, ALLOCATABLE :: EK(:)
    END TYPE StatsArrays

    TYPE(StatsArrays) :: statsArr
    !REAL, ALLOCATABLE, TARGET :: BA(:)      ! Define one array that encompasses all real statistics arrays
    

    CONTAINS
    ! ================================== Allocate and deallocate Vertical arrays
        FUNCTION AllocateVerticalArrays() RESULT(status)
            use ArrayDimensionsModule
            use ModelParamsModule, only: gUserSettings

            INTEGER :: status

            ALLOCATE(BV(18 * gUserSettings%maxLevels + 4), STAT = status) ! Single contiguous memory block
            if (status /= 0) RETURN

            call ZeroVerticalArrays()  ! Zero entire set of array
            vertArr%allocated = .TRUE. ! Set allocated flag to true

        END FUNCTION AllocateVerticalArrays

        subroutine ZeroVerticalArrays()
            BV = 0.0
        end subroutine ZeroVerticalArrays

        function createVertArr(levels) RESULT(status)

            implicit none

            integer, intent(in) :: levels

            integer :: status

            status = 0
            if (.not. vertArr%allocated) then
                status = 1
                return
            end if

            vertArr%PV(1 : levels)  => BV(             1 : levels)
            vertArr%DP(1 : levels)  => BV(    levels + 1 : 2 * levels)
            vertArr%DPI(1 : levels) => BV(2 * levels + 1 : 3 * levels)
            vertArr%THS(1 : levels) => BV(3 * levels + 1 : 4 * levels)
            vertArr%FK(1 : levels)  => BV(4 * levels + 1 : 5 * levels)
            vertArr%FJ(1 : levels)  => BV(5 * levels + 1 : 6 * levels)
            vertArr%AU(1 : levels)  => BV(6 * levels + 1 : 7 * levels)
            vertArr%A(1 : levels)   => BV(7 * levels + 1 : 8 * levels)
            vertArr%H(1 : levels)   => BV(8 * levels + 1 : 9 * levels)
            vertArr%G(1 : levels)   => BV(9 * levels + 1 : 10 * levels)
            vertArr%SIG(1 : levels) => BV(10 * levels + 1 : 11 * levels)
            vertArr%X(1 : levels)   => BV(11 * levels + 1 : 12 * levels)
            vertArr%Y(1 : levels)   => BV(12 * levels + 1 : 13 * levels)
            vertArr%PP(1 : levels + 1)  => BV(13 * levels + 1 : 14 * levels + 1)  ! This array has one extra element; 1=sfc and maxLevels+1=Top. The size is levels + 1
            vertArr%PRF(1 : 4 * levels) => BV(14 * levels + 2 : 18 * levels + 1)  ! This array has size 4 * levels
            
            vertArr%defined = .TRUE. ! Set defined flag to true
        end function createVertArr

        function DeallocateVerticalArrays() result(statusErr)
            integer statusErr
            
            IF (ALLOCATED(BV)) DEALLOCATE(BV, STAT = statusErr)
            NULLIFY(vertArr%PP)
            NULLIFY(vertArr%PV)
            NULLIFY(vertArr%DP)
            NULLIFY(vertArr%DPI)
            NULLIFY(vertArr%THS)
            NULLIFY(vertArr%FK)
            NULLIFY(vertArr%FJ)
            NULLIFY(vertArr%AU)
            NULLIFY(vertArr%A)
            NULLIFY(vertArr%H)
            NULLIFY(vertArr%G)
            NULLIFY(vertArr%SIG)
            NULLIFY(vertArr%X)
            NULLIFY(vertArr%Y)
            NULLIFY(vertArr%PRF)
            vertArr%allocated = .FALSE. ! Set allocated flag to false
            vertArr%defined = .FALSE.   ! Set defined flag to false

        end function DeallocateVerticalArrays

    ! ================================== Allocate and deallocate Horizontal arrays
        !> This function allocates the horizontal arrays used in the model. 
        !> Upon success, the horzArr%allocated flag is set to .TRUE.
        !>
        !! arrayDims The Dims object that contains the dimensions of the model arrays.
        !!                  Before passing in this object, the caller should have called the SetDimensions() method
        !!                  which populates some of the fields. This function then populates more of the
        !!                  fields in the Dims object as shown above.
        !! gUserSettings The Settings object that contains the current settings of the model. The zonal and
        !!                     meridional wave numbers are used in this object. 
        !! @return status An integer status code. 0 on success, non-zero on failure.
        FUNCTION AllocateHorizontalArrays() RESULT(status)
            USE ISO_FORTRAN_ENV
            use ArrayDimensionsModule
            use ModelParamsModule, only: gUserSettings
            use, intrinsic :: iso_c_binding, only : C_FLOAT_COMPLEX

            IMPLICIT NONE

            INTEGER :: status

            status = 0
            
            !arrayDims%ITOTND = arrayDims%ITOT * gUserSettings%maxLevels ! Forgot to add this
            !arrayDims%ITRXD = 3 * arrayDims%IXMAX / 2 + 1
            !arrayDims%ITRYD = 3 * arrayDims%IYMAX / 2 + 1

            ! Sample allocation — continue as needed
            ALLOCATE(GQ_pool(arrayDims%itot , gUserSettings%maxLevels),&
                    BTA_pool(arrayDims%itot),                         &
                    PSB_pool(arrayDims%itot),                         &
                    PS_pool(14 * arrayDims%itot * gUserSettings%maxLevels),  &

                    ADO_pool(arrayDims%itot),                         &
                    TOP_pool(arrayDims%itot),                         &
                    Gc_pool(arrayDims%KX),                            &
                    G1_pool(arrayDims%KXLYD),                         &
                    G2_pool(arrayDims%KXLYD),                         &
                    G3_pool(arrayDims%KXLYD),                         &
                    G4_pool(arrayDims%KXLYD),                         &
                    WRK_pool(arrayDims%ibig),                         &
                    GZonal_pool(arrayDims%LY),                        &
                    TMP_pool(arrayDims%lyd),                          &
                    DAT_pool(arrayDims%itot),                         &
                    AKL_pool(arrayDims%itot),                         &
                    AKLI_pool(arrayDims%itot),                        &
                    ZS_pool(arrayDims%ITOT, gUserSettings%maxLevels),  &
                    PSSAVE_pool(arrayDims%itot, gUserSettings%maxLevels), &
                    RML_pool((2 * gUserSettings%maxLevels + 1) , gUserSettings%maxLevels), &
                    TRX_pool(arrayDims%ITRXD),              &
                    TRY_pool(arrayDims%ITRYD),              &
                    IXL_pool(arrayDims%lyd),                &
                    IYL_pool(arrayDims%lyd), STAT = status)

            IF (status /= 0) RETURN
            ! Zero everything
            CALL ZeroHorzArrays(.false.)
            horzArr%ALLOCATED = .TRUE.
        END FUNCTION AllocateHorizontalArrays

        !> This subroutine zeros all horizontal arrays in the horzArr_pool type
        SUBROUTINE ZeroHorzArrays(skipSettings)
            use, intrinsic :: iso_c_binding, only : C_FLOAT_COMPLEX
            use ModelParamsModule, only: gModIntParams
            
            implicit none

            LOGICAL, intent(in) :: skipSettings
            integer :: size2d

            GQ_pool = 0.0
            BTA_pool = 0.0
            PSB_pool = 0.0
            PS_pool = 0.0
            ! TF and SF are included in PS_pool We need to skip those
            if (skipSettings) then
                size2d = (gModIntParams%LMAX2 + 2 * (gModIntParams%LMAX2 * gModIntParams%KMAX2)) * gModIntParams%NLEV
                PS_pool(1 : 4 * size2d) = 0.0
                ! skip TF
                PS_pool(5 * size2d + 1 : 13 * size2d) = 0.0
                ! skip SF
            else
                PS_pool = 0.0
            end if
            ADO_pool = 0.0
            Gc_pool = cmplx(0.0, 0.0, kind=C_FLOAT_COMPLEX)
            G1_pool = 0.0
            G2_pool = 0.0
            G3_pool = 0.0
            G4_pool = 0.0
            WRK_pool = 0.0
            GZonal_pool = 0.0
            TMP_pool = 0.0
            DAT_pool = 0.0
            AKL_pool = 0.0
            AKLI_pool = 0.0
            ZS_pool = 0.0
            PSSAVE_pool = 0.0
            
            if (skipSettings) return
            AKL_pool = 0.0
            AKLI_pool = 0.0
            TOP_pool = 0.0
            RML_pool = 0.0
            TRX_pool = 0.0
            TRY_pool = 0.0
            IXL_pool = 0.0
            IYL_pool = 0.0
        end subroutine ZeroHorzArrays

        !> This function creates the horizontal arrays by associating the pointers in the horzArr type
        !> This function needs to be called after the AllocateHorizontalArrays() method when first allocating
        !> when setting the overall model size from the gUserSettings struct. It needs to be called again
        !> if the horizontal resolution and number of levels are changed in the gModIntParams struct.
        !! @param zonal_waves The new number of zonal waves in the model.
        !! @param merid_waves The new number of meridional waves in the model.
        !! @param levels The new number of levels in the model.
        function createHorzArr(zonal_waves, merid_waves, levels, arraySizes) RESULT(status)
            use ModelParamsModule, only: gUserSettings
            use ArrayDimensionsModule
            
            implicit none

            integer, intent(in) :: zonal_waves, merid_waves, levels
            type(Dims), intent(in) :: arraySizes
            
            integer :: status

            ! local variables
            integer :: itot, size2d

            status = 0
            if (.not. horzArr%allocated) then
                status = 1
                return
            end if

            itot = merid_waves + 2 * zonal_waves * merid_waves
            size2d = itot * levels

            ! Handle 2-d arrays that have size (itot, levels)
            if (size2d > size(GQ_pool)) then
                status = 1
                return
            end if
            horzArr%GQ(1 : itot, 1 : levels) => GQ_pool(1 : itot, 1 : levels)

            horzArr%PS(1 : itot, 1 : levels) =>  PS_pool(             1 :     size2d) ! PS_pool has all 14 fields
            horzArr%TH(1 : itot, 1 : levels) =>  PS_pool(    size2d + 1 : 2 * size2d)
            horzArr%TH(1 : itot, 1 : levels) =>  PS_pool(2 * size2d + 1 : 3 * size2d)
            horzArr%WW(1 : itot, 1 : levels) =>  PS_pool(3 * size2d + 1 : 4 * size2d)
            horzArr%TF(1 : itot, 1 : levels) =>  PS_pool(4 * size2d + 1 : 5 * size2d)
            horzArr%FS(1 : itot, 1 : levels) =>  PS_pool(5 * size2d + 1 : 6 * size2d)
            horzArr%ADS(1 : itot, 1 : levels) => PS_pool(6 * size2d + 1 : 7 * size2d)
            horzArr%WS(1 : itot, 1 : levels) =>  PS_pool(7 * size2d + 1 : 8 * size2d)
            horzArr%S(1 : itot, 1 : levels) =>   PS_pool(8 * size2d + 1 : 9 * size2d)
            horzArr%FT(1 : itot, 1 : levels) =>  PS_pool(9 * size2d + 1 : 10 * size2d)
            horzArr%ADT(1 : itot, 1 : levels) => PS_pool(10 * size2d + 1 : 11 * size2d)
            horzArr%WT(1 : itot, 1 : levels) =>  PS_pool(11 * size2d + 1 : 12 * size2d)
            horzArr%T(1 : itot, 1 : levels) =>   PS_pool(12 * size2d + 1 : 13 * size2d)
            horzArr%SF(1 : itot, 1 : levels) =>  PS_pool(13 * size2d + 1 : 14 * size2d)

            horzArr%ZS(1 : itot, 1 : levels) => ZS_pool(1 : itot, 1 : levels)
            horzArr%PSSAVE(1 : itot, 1 : levels) => PSSAVE_pool(1 : itot, 1 : levels)

            ! Handle 1-d arrays that have size itot
            if (itot > size(BTA_pool)) then
                status = 1
                return
            end if
            horzArr%BTA(1 : itot) => BTA_pool(1 : itot)
            horzArr%PSB(1 : itot) => PSB_pool(1 : itot)
            horzArr%ADO(1 : itot) => ADO_pool(1 : itot)
            horzArr%TOP(1 : itot) => TOP_pool(1 : itot)
            horzArr%DAT(1 : itot) => DAT_pool(1 : itot)
            horzArr%AKL(1 : itot) => AKL_pool(1 : itot)
            horzArr%AKLI(1 : itot) => AKLI_pool(1 : itot)

            ! RML is always the same size?
            horzArr%RML(1 : 2 * gUserSettings%maxMeridWaves + 1, 1 : gUserSettings%maxMeridWaves) => RML_pool
            
            ! Transform arrays
            horzArr%TRX(1 : arraySizes%ITRXD) => TRX_pool(1 : arraySizes%ITRXD)
            horzArr%TRY(1 : arraySizes%ITRYD) => TRY_pool(1 : arraySizes%ITRYD)
            horzArr%IXL(1 : arraySizes%lyd) => IXL_pool(1 : arraySizes%lyd)
            horzArr%IYL(1 : arraySizes%lyd) => IYL_pool(1 : arraySizes%lyd)
            horzArr%Gc(1 : arraySizes%KX) => Gc_pool(1 : arraySizes%KX)
            horzArr%G1(1 : arraySizes%IXMAX, 1 : arraySizes%IYMAX) => G1_pool(1 : arraySizes%IXMAX * arraySizes%IYMAX)
            horzArr%G1plot(1 : arraySizes%KX, 1 : arraySizes%LY) => G1_pool(1 : arraySizes%KX * arraySizes%LY)
            horzArr%G1plot_1D(1 : arraySizes%KX * arraySizes%LY) => G1_pool(1 : arraySizes%KX * arraySizes%LY)
            horzArr%G2(1 : arraySizes%IXMAX, 1 : arraySizes%IYMAX) => G2_pool(1 : arraySizes%IXMAX * arraySizes%IYMAX)
            horzArr%G2plot(1 : arraySizes%KX, 1 : arraySizes%LY) => G2_pool(1 : arraySizes%KX * arraySizes%LY)
            horzArr%G2plot_1D(1 : arraySizes%KX * arraySizes%LY) => G2_pool(1 : arraySizes%KX * arraySizes%LY)
            horzArr%G3(1 : arraySizes%IXMAX, 1 : arraySizes%IYMAX) => G3_pool(1 : arraySizes%IXMAX * arraySizes%IYMAX)
            horzArr%G3plot(1 : arraySizes%KX, 1 : arraySizes%LY) => G3_pool(1 : arraySizes%KX * arraySizes%LY)
            horzArr%G3plot_1D(1 : arraySizes%KX * arraySizes%LY) => G3_pool(1 : arraySizes%KX * arraySizes%LY)
            horzArr%G4(1 : arraySizes%IXMAX, 1 : arraySizes%IYMAX) => G4_pool(1 : arraySizes%IXMAX * arraySizes%IYMAX)
            horzArr%G4plot(1 : arraySizes%KX, 1 : arraySizes%LY) => G4_pool(1 : arraySizes%KX * arraySizes%LY)
            horzArr%G4plot_1D(1 : arraySizes%KX * arraySizes%LY) => G4_pool(1 : arraySizes%KX * arraySizes%LY)
            horzArr%WRK(1 : arrayDims%ibig) => WRK_pool(1 : arrayDims%ibig)
            horzArr%GZonal(1 : arrayDims%IYMAX) => GZonal_pool(1 : arraySizes%IYMAX)
            horzArr%GZonalplot(1 : arrayDims%LY) => GZonal_pool(1 : arraySizes%LY)
            horzArr%TMP(1 : arraySizes%lyd) => TMP_pool(1 : arraySizes%lyd)
            horzArr%defined = .TRUE. ! Set defined flag to true

        end function createHorzArr

        !> This function deallocates the horizontal arrays allocated by the AllocateHorizontalArrays method.
        !! @return statusErr An integer status code. 0 on success, non-zero on failure.
        function DeallocateHorizontalArrays() result(statusErr)

            IMPLICIT NONE

            integer :: statusErr

            statusErr = 0
            IF (horzArr%allocated .EQV. .FALSE.) RETURN

            ! Deallocate all allocated components
            IF (ALLOCATED(GQ_pool))      DEALLOCATE(GQ_pool)
            IF (ALLOCATED(BTA_pool))     DEALLOCATE(BTA_pool)
            IF (ALLOCATED(PSB_pool))     DEALLOCATE(PSB_pool)
            IF (ALLOCATED(PS_pool))      DEALLOCATE(PS_pool) ! Contains 14 2-d fields

            IF (ALLOCATED(ADO_pool))     DEALLOCATE(ADO_pool)
            IF (ALLOCATED(TOP_pool))     DEALLOCATE(TOP_pool)
            IF (ALLOCATED(Gc_pool))      DEALLOCATE(Gc_pool)
            IF (ALLOCATED(G1_pool))      DEALLOCATE(G1_pool)
            IF (ALLOCATED(G2_pool))      DEALLOCATE(G2_pool)
            IF (ALLOCATED(G3_pool))      DEALLOCATE(G3_pool)
            IF (ALLOCATED(G4_pool))      DEALLOCATE(G4_pool)
            IF (ALLOCATED(WRK_pool))     DEALLOCATE(WRK_pool)
            IF (ALLOCATED(GZonal_pool))  DEALLOCATE(GZonal_pool)
            IF (ALLOCATED(TMP_pool))     DEALLOCATE(TMP_pool)
            IF (ALLOCATED(DAT_pool))     DEALLOCATE(DAT_pool)
            IF (ALLOCATED(AKL_pool))     DEALLOCATE(AKL_pool)
            IF (ALLOCATED(AKLI_pool))    DEALLOCATE(AKLI_pool)
            IF (ALLOCATED(ZS_pool))      DEALLOCATE(ZS_pool)
            IF (ALLOCATED(PSSAVE_pool))  DEALLOCATE(PSSAVE_pool)
            IF (ALLOCATED(RML_pool))     DEALLOCATE(RML_pool)
            IF (ALLOCATED(TRX_pool))     DEALLOCATE(TRX_pool)
            IF (ALLOCATED(TRY_pool))     DEALLOCATE(TRY_pool)
            IF (ALLOCATED(IXL_pool))     DEALLOCATE(IXL_pool)
            IF (ALLOCATED(IYL_pool))     DEALLOCATE(IYL_pool)
            ! The following below is equivalent to nullifying all pointers in horzArr type
            horzArr%GQ  => null()      !  GQ(ITOT, maxLevels) is the cosG sinG (QG) values between the mean PS, TH of a layer
            horzArr%BTA => null()       !  BTA(ITOT) is the mean vertically (spatially) averaged flow 
            horzArr%PSB => null()       !  PSB(ITOT) is the vert. mean flow for the waves in the modelType = ZONAL_MEAN_FLOWS option
            horzArr%PS  => null()      !  PS(ITOT,maxLevels) is the streamfunction spectral components
            horzArr%TH  => null()      !  TH(ITOT,maxLevels) is the temperature spectral components
            horzArr%WW  => null()      !  WW(ITOT,maxLevels) is the omega spectral components
            horzArr%TF  => null()      !  TF(ITOT,maxLevels) is the thermal forcing field
            horzArr%FS  => null()      !  FS(ITOT,maxLevels) is the streamfunction tendency
            horzArr%ADS => null()     !  ADS(ITOT,maxLevels) is the spectral form of the vorticity advection
            horzArr%WS  => null()      !  WS(ITOT,maxLevels) is the omega forcing on the streamfunction
            horzArr%S   => null()       !  S(ITOT,maxLevels) is the diabatic forcing on the streamfunction
            horzArr%FT  => null()      !  FT(ITOT,maxLevels) is the temperature tendency
            horzArr%ADT => null()     !  ADT(ITOT,maxLevels) is the spectral form of the temperature advection
            horzArr%WT  => null()      !  WT(ITOT,maxLevels) is the omega forcing on the temperature
            horzArr%T   => null()       !  T(ITOT,maxLevels) is the diabatic forcing on the temperature
            horzArr%SF  => null()      !  SF(ITOT,maxLevels) is the Barotropic momentum forcing field
            horzArr%ADO => null()       !  ADO(ITOT) is the spectral form of the forcing by srf flow over orography
            horzArr%TOP => null()       !  TOP(ITOT) is the surface orographic field
            horzArr%Gc  => null()        !  Gc(KX) complex grid point array for the periodic transform routine
            horzArr%G1  => null()     !  G1(IXMAX, IYMAX) grid point arrays for transform routines in 2 d
            horzArr%G1plot => null() !  G1plot(KX, LY) grid point arrays for transform routines in 2 d
            horzArr%G1plot_1D  => null() !  G1plot(KX*LY) grid point arrays for transform routines in 1 d
            horzArr%G2  => null()     !  G2(IXMAX, IYMAX) grid point arrays for transform routines in 2 d
            horzArr%G2plot  => null() !  G2plot(KX*LY) grid point arrays for transform routines (?)
            horzArr%G2plot_1D  => null() !  G2plot(KX*LY) grid point arrays for transform routines in 1 d
            horzArr%G3  => null()     !  G3(IXMAX, IYMAX) grid point arrays for transform routines in 2 d
            horzArr%G3plot  => null() !  G3plot(KX*LY) grid point arrays for transform routines (?)
            horzArr%G3plot_1D  => null() !  G3plot(KX*LY) grid point arrays for transform routines in 1 d
            horzArr%G4  => null()     !  G4(IXMAX, IYMAX) grid point arrays for transform routines in 2 d
            horzArr%G4plot  => null() !  G4plot(KX*LY) grid point arrays for transform routines (?)
            horzArr%G4plot_1D  => null() !  G4plot(KX*LY) grid point arrays for transform routines in 1 d
            horzArr%WRK  => null()       !  WRK(IBIG>KX*LY) is a work array for FFT991 routine
            horzArr%GZonal  => null()    !  GZonal(IYMAX) grid point array for zonal (cosly) transforms
            horzArr%GZonalplot  => null()!  GZonal(LY) grid point array for zonal (cosly) transforms
            horzArr%TMP  => null()       !  TMP(arrayDims%LYD)
            horzArr%DAT  => null()       !  DAT(ITOT) is an all purpose array for computing some variable at a level.
            horzArr%AKL  => null()       !  AKL(ITOT) two dimensional wavenumber used for calculating things like the vorticity
            horzArr%AKLI => null()      !  AKLI(ITOT) are the two dimensional inverse wavenumber used for calculating things like the vorticity
            horzArr%ZS  => null()      !  ZS(ITOTD,maxLevels)
            horzArr%PSSAVE  => NULL()  !  PSSAVE(ITOTD,maxLevels)
            horzArr%RML  => null()     !  RML(2*maxMeridWaves+1,maxMeridWaves) is used in the projection routines in inverse transform.
            horzArr%TRX  => null()       !  TRX(arrayDims%ITRXD)
            horzArr%TRY  => null()       !  TRY(arrayDims%ITRYD)

            horzArr%IXL  => null()
            horzArr%IYL  => null()

            horzArr%allocated = .FALSE.
            horzArr%defined = .FALSE.
        END function DeallocateHorizontalArrays

    ! ================================== Allocate and deallocate Statistics arrays
        !> This function allocates the statistics arrays used in the model and sets sizes in the arrayDims.
        !> Before calling this function the SetDimensions() and the AllocateHorizontalArrays() must
        !> have been called. The AllocateStatsArrays() method sets the %ITOTA, %KXLYA, %NALA, %KXV, %LYV, KXLYA
        !> values. The %KXV and %LYV are used to allocate space for the variance arrays
        !! arrayDims is The Dims object that contains the dimensions of the model arrays.
        !!                  The fields in this object need to be set by calling the SetDimensions()
        !!                  and AllocateHorizontalArrays() methods. This function then populates 
        !!                  more of the arrayDims fields.
        !! gUserSettings is The Settings object which contains the current statistics option as
        !!                     well as the horizonal and vertical resolutions.
        FUNCTION AllocateStatsArrays() RESULT(status)
            use ConstantsModule, only: STATS_OPT_NONE, STATS_OPT_MEAN, STATS_OPT_VAR
            use ArrayDimensionsModule
            use ModelParamsModule, only: gUserSettings       ! If referencing a type, Dont use type(Settings), just 'Settings' in an 'only' statement

            integer :: status, levels
            
            status = 0
            levels = 1
            IF(gUserSettings%statsOption .EQ. STATS_OPT_VAR) levels = arrayDims%NALA
            !!---------------------------------Statistical array sizes:
            !arrayDims%ITOTA = 1
            !arrayDims%NALA = 1
            !arrayDims%KXV = 1
            !arrayDims%LYV = 1
            !arrayDims%KXLYA = 1
            !levels = 1
            !
            !!----------------------------------- Total sizes:
            !!  If STATS_OPT_NONE give each array 1 element
            !!  If STATS_OPT_MEAN allocate PSA,APE,RKE,PSS, and all others one element
            !!  If STATS_OPT_VAR allocate all
            !!-------------PSA,(WWA,FSA,ADSA,FTA,ADTA,ADOA) APE,RKE,PSS,V1,V2:  The (WWA,FSA,ADSA,FTA,ADTA,ADOA) are the other 6 fields in PSA
            !
            !IF(gUserSettings%statsOption .NE. STATS_OPT_NONE) THEN ! Both mean and variance use this
            !    arrayDims%ITOTA = arrayDims%ITOT
            !    arrayDims%NALA = gUserSettings%maxLevels
            !end if
            !IF(gUserSettings%statsOption .EQ. STATS_OPT_VAR) THEN
            !    arrayDims%KXLYA = 2 * arrayDims%KXLYD * gUserSettings%maxLevels
            !    arrayDims%NALA = gUserSettings%maxLevels
            !    arrayDims%KXV = arrayDims%KX
            !    arrayDims%LYV = arrayDims%IYH
            !    levels = arrayDims%NALA
            !end if
            
            !---------------------------------------------------------Try to allocate:
            ! PSA, APE, RKE, PSS, V1, V2
            ALLOCATE(statsArr%PSA(arrayDims%ITOTA, arrayDims%NALA, 7),  &
                     statsArr%APE(arrayDims%ITOTA, arrayDims%NALA),     &
                     statsArr%RKE(arrayDims%ITOTA, arrayDims%NALA),     &
                     statsArr%PSS(arrayDims%ITOTA, arrayDims%NALA, 7),  &
                     statsArr%V1(arrayDims%KXV*arrayDims%LYV, levels),      &   ! V1(KXV*LYV,NALA) when variance options otherwise V(1,1)
                     statsArr%V2(arrayDims%KXV*arrayDims%LYV, levels),      &   ! V2(KXV*LYV,NALA)
                     statsArr%EK(300))

            statsArr%allocated = .TRUE. ! Set allocated flag to true

        END FUNCTION AllocateStatsArrays

        function DeallocateStatsArrays() result(statusErr)
            integer statusErr
            
            statusErr = 0
            IF (.NOT. statsArr%allocated) RETURN

            IF (ALLOCATED(statsArr%PSA))      DEALLOCATE(statsArr%PSA)
            IF (ALLOCATED(statsArr%APE))     DEALLOCATE(statsArr%APE)
            IF (ALLOCATED(statsArr%RKE))     DEALLOCATE(statsArr%RKE)
            IF (ALLOCATED(statsArr%PSS))      DEALLOCATE(statsArr%PSS)
            IF (ALLOCATED(statsArr%V1))      DEALLOCATE(statsArr%V1)
            IF (ALLOCATED(statsArr%V2))      DEALLOCATE(statsArr%V2)
            IF (ALLOCATED(statsArr%EK))      DEALLOCATE(statsArr%EK)
            statsArr%allocated = .FALSE. ! Set allocated flag to false

        end function DeallocateStatsArrays


END MODULE ModelFieldsModule
