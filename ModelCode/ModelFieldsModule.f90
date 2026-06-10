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
!    ALLOCATE(BI(gArrayDims%NINTD), STAT = status)
!    ALLOCATE(IBI(totalSize), STAT = status)
!
! Use a create function to define the arrays in the pool. When the user sets new parameters this create function is
! is simply called again as long as the new parameters do not exceed the base value defining the base memory pool.
!    gCintArr%Cint(1 : numberOfInteractionCoeffs)  => BI(1 : numberOfInteractionCoeffs)
!    gCintArr%JIN(1 :  numberOfInteractionCoeffs)  => IBI(1 : numberOfInteractionCoeffs)
!    gCintArr%KIN(1 :  numberOfInteractionCoeffs)  => IBI(numberOfInteractionCoeffs : 2 * numberOfInteractionCoeffs)
!    gCintArr%I1M(1 :  ITOT)                       => IBI(2 * numberOfInteractionCoeffs : ITOT)
!
! Deallocate if the user parameter exceeds the current base value
!
!    IF (ALLOCATED(BI)) DEALLOCATE(BI)
!    nullify(gCintArr%CINT)
!    IF (ALLOCATED(IBI)) DEALLOCATE(IBI)
!    Nullify(gCintArr%JIN)
!    Nullify(gCintArr%KIN)
!    Nullify(gCintArr%I1M)
!
! Then the whole allocation and creation process has to be repeated if base value is exceeded with a new base value.

MODULE ModelFieldsModule
    IMPLICIT NONE
    
    !include 'fftw3.f03'

    ! Subroutines and functions
    public :: AllocateVerticalArrays, DeallocateVerticalArrays
    public :: AllocateHorizontalArrays, DeallocateHorizontalArrays
    public :: AllocateStatsArrays, DeallocateStatsArrays

    ! Variables
    public :: gVertArr, gHorzArr, gStatsArr
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
        LOGICAL :: allocated         = .FALSE. ! Flag to indicate if arrays are allocated
        LOGICAL :: defined           = .FALSE. ! Flag to indicate if arrays are defined with proper sizes
        LOGICAL :: isDimensionalized = .FALSE. ! Flag to indicate if arrays have been dimensionalized with physical units (e.g., mb, K, m/s)
        REAL ::    tropopauseLevel   = 250.0   !  Height of the tropopause

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
        REAL, POINTER :: PRF(:) => null()   !  PRF(maxLevels * 4) are profile arrays for functional behavior of model
        REAL, POINTER :: LVS(:) => null()   !  LVS(2 * maxLevels + 1) is an array of all the levels, just for plotting
    END TYPE VertArrays

    TYPE(VertArrays) :: gVertArr
    REAL, ALLOCATABLE, TARGET :: BV(:)      ! Define one array that encompasses all vertical arrays

    !=============================== Horizontal arrays
        !TYPE :: ChainHorzArrays
        !    LOGICAL :: allocated = .FALSE. ! Flag to indicate if arrays are allocated
        !    REAL, POINTER :: GQ(:),BTA(:),PSB(:),PS(:),TH(:),WW(:),TF(:),FS(:),ADS(:),WS(:),S(:),FT(:),ADT(:),WT(:),T(:),SF(:),ADO(:),TOP(:)
        !    REAL, POINTER :: G1(:),G2(:),G3(:),TMP(:),DAT(:),AKL(:),AKLI(:),ZS(:),PSSAVE(:)
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
    REAL, ALLOCATABLE, TARGET :: DAT_pool(:)      !  DAT(ITOT) is an all purpose array for computing some variable at a level.
    REAL, ALLOCATABLE, TARGET :: AKL_pool(:)      !  AKL(ITOT) two dimensional wavenumber used for calculating things like the vorticity
    REAL, ALLOCATABLE, TARGET :: AKLI_pool(:)     !  AKLI(ITOT) are the two dimensional inverse wavenumber used for calculating things like the vorticity
    REAL, ALLOCATABLE, TARGET :: Interp_Strm_pool(:)     !  An ITOT array to compute the strm variables at an interpolated level for plotting
    REAL, ALLOCATABLE, TARGET :: Interp_Temp_pool(:)     !  An ITOT array to compute the temp variables at an interpolated level for plotting
    REAL, ALLOCATABLE, TARGET :: RK_pool(:)       !  RK(maxZonalWaves) K wavenumber as a real value
    REAL, ALLOCATABLE, TARGET :: RL_pool(:)       !  RL(maxMeridWaves) L wavenumber as a real value
    REAL, ALLOCATABLE, TARGET :: ZS_pool(:,:)     !  ZS(ITOTD,maxLevels), for the N-cycle scheme
    REAL, ALLOCATABLE, TARGET :: PSSAVE_pool(:,:) !  PSSAVE(ITOTD,maxLevels)

    COMPLEX, ALLOCATABLE, TARGET :: Gc_pool(:)    !  Gc((IXMAX/2 + 1) * IYMAX) complex grid point array for the periodic transform routine
    REAL, ALLOCATABLE, TARGET :: G1_pool(:)       !  G1(IXMAX*IYMAX) grid point arrays for transform routine
    REAL, ALLOCATABLE, TARGET :: G2_pool(:)       !  G2(IXMAX*IYMAX) grid point arrays for transform routine
    REAL, ALLOCATABLE, TARGET :: G3_pool(:)       !  G3(IXMAX*IYMAX) grid point arrays for transform routine
    REAL, ALLOCATABLE, TARGET :: G4_pool(:)       !  G4(IXMAX*IYMAX) grid point arrays for transform routine
    real, allocatable, target :: plot_pool(:)     !  plot(IXMAX*IYMAX) grid point array for plotting with fftw3
    real, allocatable, target :: plotFull_pool(:) !  plotFull(KX*LY) grid point array for plotting to screen
    COMPLEX, ALLOCATABLE, TARGET :: GcZ_pool(:)   !  Gc(KX or LY) complex grid point array for the periodic transform routine
    real, allocatable, target :: GZonal_pool(:)   !  GZonal(KX or LY) grid point array for zonal component of transform 
    real, allocatable, target :: G2Zonal_pool(:)  !  G2Zonal(KX or LY) grid point array for zonal component of transform 

    TYPE :: HorzArrays
        LOGICAL :: allocated = .FALSE.          !  Flag to indicate if arrays are allocated and defined
        LOGICAL :: defined = .FALSE.            !  Flag to indicate if arrays are defined with proper sizes
        LOGICAL :: isDimensionalized = .FALSE.  !  Flag to indicate if arrays have been dimensionalized with physical units (for display to user and graphics)
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
        REAL, POINTER :: DAT(:) => null()       !  DAT(ITOT) is an all purpose array for computing some variable at a level.
        REAL, POINTER :: AKL(:) => null()       !  AKL(ITOT) two dimensional wavenumber used for calculating things like the vorticity
        REAL, POINTER :: AKLI(:) => null()      !  AKLI(ITOT) are the two dimensional inverse wavenumber used for calculating things like the vorticity
        real, pointer :: Interp_Strm(:) => null() !  An ITOT array to compute the strm variables at an interpolated level for plotting
        real, pointer :: Interp_Temp(:) => null() !  An ITOT array to compute the temp variables at an interpolated level for plotting
        real, pointer :: RK(:) => null()        !  RK(maxZonalWaves) is the zonal wavenumber as a real value
        real, pointer :: RL(:) => null()        !  RL(maxMeridWaves) is the zonal wavenumber as a real value
        REAL, POINTER :: ZS(:,:) => null()      !  ZS(ITOT,maxLevels)
        REAL, POINTER :: PSSAVE(:,:) => NULL()  !  PSSAVE(ITOTD,maxLevels)
    END TYPE HorzArrays
    
    type :: TransArrays
        LOGICAL :: allocated = .FALSE.          !  Flag to indicate if arrays are allocated and defined
        LOGICAL :: defined = .FALSE.            !  Flag to indicate if arrays are defined with proper sizes
        COMPLEX, POINTER :: Gc(:,:) => null()   !  Gc(IXMAX/2 +1, IYMAX) complex grid point array for the periodic transform routine
        REAL, POINTER :: G1(:, :) => null()     !  G1(IXMAX, IYMAX) grid point arrays for transform routines in 2 d
        REAL, POINTER :: G2(:, :) => null()     !  G2(IXMAX, IYMAX) grid point arrays for transform routines in 2 d
        REAL, POINTER :: G3(:, :) => null()     !  G3(IXMAX, IYMAX) grid point arrays for transform routines in 2 d
        REAL, POINTER :: G4(:, :) => null()     !  G4(IXMAX, IYMAX) grid point arrays for transform routines in 2 d
        REAL, POINTER :: plot(:, :) => null()   !  plot(IXMAX, IYMAX) grid point array for plotting in fftw3
        REAL, POINTER :: plotFull(:, :) => null()   !  plotFull(KX, LY) grid point array for plotting on the screen
        COMPLEX, POINTER :: GcZ(:) => null()    !  GcZ(KX or LY) complex grid point array for the periodic transform routine
        REAL, POINTER :: GZonal(:) => null()    !  GZonal(KX or LY) grid point array for zonal (cosly) transforms
        REAL, POINTER :: G2Zonal(:) => null()   !  G2Zonal(KX or LY) grid point array for zonal (cosly) transforms
    end type TransArrays

    !TYPE(HorzArraysPool) :: horzArr_pool
    TYPE(HorzArrays) :: gHorzArr
    type(TransArrays) :: gTransArr
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

    TYPE(StatsArrays) :: gStatsArr
    !REAL, ALLOCATABLE, TARGET :: BA(:)      ! Define one array that encompasses all real statistics arrays
    

    CONTAINS
    ! ================================== Allocate and deallocate Vertical arrays
        FUNCTION AllocateVerticalArrays() RESULT(status)
            use ArrayDimensionsModule
            use ModelParamsModule, only: gUserSettings

            INTEGER :: status

            ALLOCATE(BV(20 * gUserSettings%maxLevels + 5), STAT = status) ! Single contiguous memory block
            if (status /= 0) RETURN

            call ZeroVerticalArrays()  ! Zero entire set of array
            gVertArr%allocated = .TRUE. ! Set allocated flag to true

        END FUNCTION AllocateVerticalArrays

        subroutine ZeroVerticalArrays()
            implicit none
            BV = 0.0
        end subroutine ZeroVerticalArrays

        function createVertArr(levels) RESULT(status)

            implicit none

            integer, intent(in) :: levels

            integer :: status

            status = 0
            if (.not. gVertArr%allocated) then
                status = 1
                return
            end if

            gVertArr%PV(1 : levels)  => BV(             1 : levels)
            gVertArr%DP(1 : levels)  => BV(    levels + 1 : 2 * levels)
            gVertArr%DPI(1 : levels) => BV(2 * levels + 1 : 3 * levels)
            gVertArr%THS(1 : levels) => BV(3 * levels + 1 : 4 * levels)
            gVertArr%FK(1 : levels)  => BV(4 * levels + 1 : 5 * levels)
            gVertArr%FJ(1 : levels)  => BV(5 * levels + 1 : 6 * levels)
            gVertArr%AU(1 : levels)  => BV(6 * levels + 1 : 7 * levels)
            gVertArr%A(1 : levels)   => BV(7 * levels + 1 : 8 * levels)
            gVertArr%H(1 : levels)   => BV(8 * levels + 1 : 9 * levels)
            gVertArr%G(1 : levels)   => BV(9 * levels + 1 : 10 * levels)
            gVertArr%SIG(1 : levels) => BV(10 * levels + 1 : 11 * levels)
            gVertArr%X(1 : levels)   => BV(11 * levels + 1 : 12 * levels)
            gVertArr%Y(1 : levels)   => BV(12 * levels + 1 : 13 * levels)
            gVertArr%PP(1 : levels + 1)  => BV(13 * levels + 1 : 14 * levels + 1)  ! This array has one extra element; 1=sfc and maxLevels+1=Top. The size is levels + 1
            gVertArr%PRF(1 : 4 * levels) => BV(14 * levels + 2 : 18 * levels + 1)  ! This array has size 4 * levels
            gVertArr%LVS(1: 2 * levels + 1) => BV(18 * levels + 3 : 20 * levels + 1) ! This array has size 2 * levels + 1
            
            gVertArr%defined = .TRUE. ! Set defined flag to true
        end function createVertArr

        function DeallocateVerticalArrays() result(statusErr)
            integer statusErr
            
            IF (ALLOCATED(BV)) DEALLOCATE(BV, STAT = statusErr)
            NULLIFY(gVertArr%PP)
            NULLIFY(gVertArr%PV)
            NULLIFY(gVertArr%DP)
            NULLIFY(gVertArr%DPI)
            NULLIFY(gVertArr%THS)
            NULLIFY(gVertArr%FK)
            NULLIFY(gVertArr%FJ)
            NULLIFY(gVertArr%AU)
            NULLIFY(gVertArr%A)
            NULLIFY(gVertArr%H)
            NULLIFY(gVertArr%G)
            NULLIFY(gVertArr%SIG)
            NULLIFY(gVertArr%X)
            NULLIFY(gVertArr%Y)
            NULLIFY(gVertArr%PRF)
            NULLIFY(gVertArr%LVS)
            gVertArr%allocated = .FALSE. ! Set allocated flag to false
            gVertArr%defined = .FALSE.   ! Set defined flag to false

        end function DeallocateVerticalArrays

    ! ================================== Allocate and deallocate Horizontal arrays
        !> This function allocates the horizontal arrays used in the model. 
        !> Upon success, the gHorzArr%allocated flag is set to .TRUE.
        !>
        !! gArrayDims The Dims object that contains the dimensions of the model arrays.
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

            ALLOCATE(GQ_pool(gArrayDims%itot , gUserSettings%maxLevels),&
                    BTA_pool(gArrayDims%itot),                         &
                    PSB_pool(gArrayDims%itot),                         &
                    PS_pool(14 * gArrayDims%itot * gUserSettings%maxLevels),  &

                    ADO_pool(gArrayDims%itot),                         &
                    TOP_pool(gArrayDims%itot),                         &
                    DAT_pool(gArrayDims%itot),                         &
                    AKL_pool(gArrayDims%itot),                         &
                    AKLI_pool(gArrayDims%itot),                        &
                    Interp_Strm_pool(gArrayDims%itot),                 &
                    Interp_Temp_pool(gArrayDims%itot),                 &
                    RK_pool(gUserSettings%maxZonalWaves),              &
                    RL_pool(gUserSettings%maxMeridWaves),              &
                    ZS_pool(gArrayDims%ITOT, gUserSettings%maxLevels), &
                    PSSAVE_pool(gArrayDims%itot, gUserSettings%maxLevels),&
                    STAT = status)
            IF (status /= 0) RETURN

            ! Zero everything
            CALL ZeroHorzArrays(.false.)
            gHorzArr%ALLOCATED = .TRUE.
        END FUNCTION AllocateHorizontalArrays
        
        FUNCTION AllocateTransformArrays() RESULT(status)
            USE ISO_FORTRAN_ENV
            use ArrayDimensionsModule
            use ModelParamsModule, only: gUserSettings
            use, intrinsic :: iso_c_binding, only : C_FLOAT_COMPLEX

            IMPLICIT NONE

            INTEGER :: status
            complex(C_FLOAT_COMPLEX), pointer :: dummy_c_2d(:, :)
            type(C_PTR) :: p_dummy_r, p_dummy_c

            status = 0


            allocate(Gc_pool((gArrayDims%IXMAX/2 + 1) * gArrayDims%IYMAX),     &
                    G1_pool(gArrayDims%IXMAX * gArrayDims%IYMAX),              &
                    G2_pool(gArrayDims%IXMAX * gArrayDims%IYMAX),              &
                    G3_pool(gArrayDims%IXMAX * gArrayDims%IYMAX),              &
                    G4_pool(gArrayDims%IXMAX * gArrayDims%IYMAX),              &
                    plot_pool(gArrayDims%IXMAX * gArrayDims%IYMAX),            &
                    plotFull_pool(gArrayDims%KX * gArrayDims%LY),              &
                    GZonal_pool(max(gArrayDims%KX, gArrayDims%LY)),            &
                    G2Zonal_pool(max(gArrayDims%KX, gArrayDims%LY)),           &
                    GcZ_pool(max(gArrayDims%KX, gArrayDims%LY)),               &
                    STAT = status)

            IF (status /= 0) RETURN
            ! Zero everything
            CALL ZeroTransArrays()
            gTransArr%ALLOCATED = .TRUE.
        END FUNCTION AllocateTransformArrays


        !> This subroutine zeros all horizontal arrays in the horzArr_pool type
        SUBROUTINE ZeroHorzArrays(skipSettings)
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
            DAT_pool = 0.0
            AKL_pool = 0.0
            AKLI_pool = 0.0

            ZS_pool = 0.0
            PSSAVE_pool = 0.0
            Interp_Strm_pool = 0.0
            Interp_Temp_pool = 0.0

            if (skipSettings) return
            AKL_pool = 0.0
            AKLI_pool = 0.0
            RK_pool = 0.0
            RL_pool = 0.0
            TOP_pool = 0.0
        end subroutine ZeroHorzArrays
        
        subroutine ZeroTransArrays()
            use, intrinsic :: iso_c_binding, only : C_FLOAT_COMPLEX

            implicit none
            Gc_pool = (0.0, 0.0)
            G1_pool = 0.0
            G2_pool = 0.0
            G3_pool = 0.0
            G4_pool = 0.0
            plot_pool = 0.0
            plotFull_pool = 0.0
            GcZ_pool = (0.0, 0.0)
            GZonal_pool = 0.0
            G2Zonal_pool = 0.0
        end subroutine ZeroTransArrays

        !> This function creates the horizontal arrays by associating the pointers in the gHorzArr type
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
            integer :: itot, size2d, mx

            status = 0
            if (.not. gHorzArr%allocated) then
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
            gHorzArr%GQ(1 : itot, 1 : levels) => GQ_pool(1 : itot, 1 : levels)

            gHorzArr%PS(1 : itot, 1 : levels) =>  PS_pool(             1 :     size2d) ! PS_pool has all 14 fields
            gHorzArr%TH(1 : itot, 1 : levels) =>  PS_pool(    size2d + 1 : 2 * size2d)
            gHorzArr%TH(1 : itot, 1 : levels) =>  PS_pool(2 * size2d + 1 : 3 * size2d)
            gHorzArr%WW(1 : itot, 1 : levels) =>  PS_pool(3 * size2d + 1 : 4 * size2d)
            gHorzArr%TF(1 : itot, 1 : levels) =>  PS_pool(4 * size2d + 1 : 5 * size2d)
            gHorzArr%FS(1 : itot, 1 : levels) =>  PS_pool(5 * size2d + 1 : 6 * size2d)
            gHorzArr%ADS(1 : itot, 1 : levels) => PS_pool(6 * size2d + 1 : 7 * size2d)
            gHorzArr%WS(1 : itot, 1 : levels) =>  PS_pool(7 * size2d + 1 : 8 * size2d)
            gHorzArr%S(1 : itot, 1 : levels) =>   PS_pool(8 * size2d + 1 : 9 * size2d)
            gHorzArr%FT(1 : itot, 1 : levels) =>  PS_pool(9 * size2d + 1 : 10 * size2d)
            gHorzArr%ADT(1 : itot, 1 : levels) => PS_pool(10 * size2d + 1 : 11 * size2d)
            gHorzArr%WT(1 : itot, 1 : levels) =>  PS_pool(11 * size2d + 1 : 12 * size2d)
            gHorzArr%T(1 : itot, 1 : levels) =>   PS_pool(12 * size2d + 1 : 13 * size2d)
            gHorzArr%SF(1 : itot, 1 : levels) =>  PS_pool(13 * size2d + 1 : 14 * size2d)

            gHorzArr%ZS(1 : itot, 1 : levels) => ZS_pool(1 : itot, 1 : levels)
            gHorzArr%PSSAVE(1 : itot, 1 : levels) => PSSAVE_pool(1 : itot, 1 : levels)

            ! Handle 1-d arrays that have size itot
            if (itot > size(BTA_pool)) then
                status = 1
                return
            end if
            gHorzArr%BTA(1 : itot) => BTA_pool(1 : itot)
            gHorzArr%PSB(1 : itot) => PSB_pool(1 : itot)
            gHorzArr%ADO(1 : itot) => ADO_pool(1 : itot)
            gHorzArr%TOP(1 : itot) => TOP_pool(1 : itot)
            gHorzArr%DAT(1 : itot) => DAT_pool(1 : itot)
            gHorzArr%AKL(1 : itot) => AKL_pool(1 : itot)
            gHorzArr%AKLI(1 : itot) => AKLI_pool(1 : itot)
            gHorzArr%Interp_Strm(1 : itot) => Interp_Strm_pool(1 : itot)
            gHorzArr%Interp_Temp(1 : itot) => Interp_Temp_pool(1 : itot)
            gHorzArr%RK(1 : zonal_waves) => RK_pool(1 : zonal_waves)
            gHorzArr%RL(1 : merid_waves) => RL_pool(1 : merid_waves)
            gHorzArr%defined = .TRUE. ! Set defined flag to true

        end function createHorzArr
        
        function createTransArr(arraySizes) result(status)
            use ArrayDimensionsModule
            use ModelParamsModule, only: gUserSettings
            use, intrinsic :: iso_c_binding, only : C_FLOAT_COMPLEX
            
            implicit none
            type(Dims), intent(in) :: arraySizes
            
            integer :: status, mx

            status = 0
            if (.not. gTransArr%allocated) then
                status = 1
                return
            end if
            ! Transform arrays
            mx = max(arraySizes%KX, arraySizes%LY)
            gTransArr%Gc(1 : arraySizes%IXMAX/2 + 1, 1 : arraySizes%IYMAX) => Gc_pool(1 : (arraySizes%IXMAX/2 + 1) * arraySizes%IYMAX)
            gTransArr%G1(1 : arraySizes%IXMAX, 1 : arraySizes%IYMAX) => G1_pool(1 : arraySizes%IXMAX * arraySizes%IYMAX)
            gTransArr%G2(1 : arraySizes%IXMAX, 1 : arraySizes%IYMAX) => G2_pool(1 : arraySizes%IXMAX * arraySizes%IYMAX)
            gTransArr%G3(1 : arraySizes%IXMAX, 1 : arraySizes%IYMAX) => G3_pool(1 : arraySizes%IXMAX * arraySizes%IYMAX)
            gTransArr%G4(1 : arraySizes%IXMAX, 1 : arraySizes%IYMAX) => G4_pool(1 : arraySizes%IXMAX * arraySizes%IYMAX)
            ! Plot array
            gTransArr%plot(1: arraySizes%IXMAX, 1 : arraySizes%IYMAX) => plot_pool(1 : arraySizes%IXMAX * arraySizes%IYMAX)
            gTransArr%plotFull(1: arraySizes%KX, 1 : arraySizes%LY) => plotFull_pool(1 : arraySizes%KX * arraySizes%LY)
            gTransArr%GcZ(1 : mx) => GcZ_pool(1 : mx)
            gTransArr%GZonal(1 : mx) => GZonal_pool(1 : mx)
            gTransArr%G2Zonal(1 : mx) => G2Zonal_pool(1 : mx)

            call ZeroTransArrays()
        end function createTransArr

        !> This function deallocates the horizontal arrays allocated by the AllocateHorizontalArrays method.
        !! @return statusErr An integer status code. 0 on success, non-zero on failure.
        function DeallocateHorizontalArrays() result(statusErr)

            IMPLICIT NONE

            integer :: statusErr

            statusErr = 0
            IF (gHorzArr%allocated .EQV. .FALSE.) RETURN

            ! Deallocate all allocated components
            IF (ALLOCATED(GQ_pool))      DEALLOCATE(GQ_pool)
            IF (ALLOCATED(BTA_pool))     DEALLOCATE(BTA_pool)
            IF (ALLOCATED(PSB_pool))     DEALLOCATE(PSB_pool)
            IF (ALLOCATED(PS_pool))      DEALLOCATE(PS_pool) ! Contains 14 2-d fields

            IF (ALLOCATED(ADO_pool))     DEALLOCATE(ADO_pool)
            IF (ALLOCATED(TOP_pool))     DEALLOCATE(TOP_pool)
            IF (ALLOCATED(DAT_pool))     DEALLOCATE(DAT_pool)
            IF (ALLOCATED(AKL_pool))     DEALLOCATE(AKL_pool)
            IF (ALLOCATED(AKLI_pool))    DEALLOCATE(AKLI_pool)
            IF (ALLOCATED(Interp_Strm_pool)) DEALLOCATE(Interp_Strm_pool)
            IF (ALLOCATED(Interp_Temp_pool)) DEALLOCATE(Interp_Temp_pool)
            IF (ALLOCATED(RK_pool))      DEALLOCATE(RK_pool)
            IF (ALLOCATED(RL_pool))      DEALLOCATE(RL_pool)
            IF (ALLOCATED(ZS_pool))      DEALLOCATE(ZS_pool)
            IF (ALLOCATED(PSSAVE_pool))  DEALLOCATE(PSSAVE_pool)
            ! The following below is equivalent to nullifying all pointers in gHorzArr type
            gHorzArr%GQ  => null()      !  GQ(ITOT, maxLevels) is the cosG sinG (QG) values between the mean PS, TH of a layer
            gHorzArr%BTA => null()       !  BTA(ITOT) is the mean vertically (spatially) averaged flow 
            gHorzArr%PSB => null()       !  PSB(ITOT) is the vert. mean flow for the waves in the modelType = ZONAL_MEAN_FLOWS option
            gHorzArr%PS  => null()      !  PS(ITOT,maxLevels) is the streamfunction spectral components
            gHorzArr%TH  => null()      !  TH(ITOT,maxLevels) is the temperature spectral components
            gHorzArr%WW  => null()      !  WW(ITOT,maxLevels) is the omega spectral components
            gHorzArr%TF  => null()      !  TF(ITOT,maxLevels) is the thermal forcing field
            gHorzArr%FS  => null()      !  FS(ITOT,maxLevels) is the streamfunction tendency
            gHorzArr%ADS => null()     !  ADS(ITOT,maxLevels) is the spectral form of the vorticity advection
            gHorzArr%WS  => null()      !  WS(ITOT,maxLevels) is the omega forcing on the streamfunction
            gHorzArr%S   => null()       !  S(ITOT,maxLevels) is the diabatic forcing on the streamfunction
            gHorzArr%FT  => null()      !  FT(ITOT,maxLevels) is the temperature tendency
            gHorzArr%ADT => null()     !  ADT(ITOT,maxLevels) is the spectral form of the temperature advection
            gHorzArr%WT  => null()      !  WT(ITOT,maxLevels) is the omega forcing on the temperature
            gHorzArr%T   => null()       !  T(ITOT,maxLevels) is the diabatic forcing on the temperature
            gHorzArr%SF  => null()      !  SF(ITOT,maxLevels) is the Barotropic momentum forcing field
            gHorzArr%ADO => null()       !  ADO(ITOT) is the spectral form of the forcing by srf flow over orography
            gHorzArr%TOP => null()       !  TOP(ITOT) is the surface orographic field
            gHorzArr%DAT  => null()       !  DAT(ITOT) is an all purpose array for computing some variable at a level.
            gHorzArr%AKL  => null()       !  AKL(ITOT) two dimensional wavenumber used for calculating things like the vorticity
            gHorzArr%AKLI => null()      !  AKLI(ITOT) are the two dimensional inverse wavenumber used for calculating things like the vorticity
            gHorzArr%Interp_Strm => null() !  An ITOT array to compute the strm variables at an interpolated level for plotting
            gHorzArr%Interp_Temp => null() !  An ITOT array to compute the temp variables at an interpolated level for plotting
            gHorzArr%RK  => null()       !  RK(maxZonalWaves) K wavenumber as a real value
            gHorzArr%RL => null()        !  RL(maxMeridWaves) L wavenumber as a real value
            gHorzArr%ZS  => null()      !  ZS(ITOTD,maxLevels)
            gHorzArr%PSSAVE  => NULL()  !  PSSAVE(ITOTD,maxLevels)

            gHorzArr%allocated = .FALSE.
            gHorzArr%defined = .FALSE.
        END function DeallocateHorizontalArrays
        
        function DeallocateTransformArrays() result(statusErr)
            use, intrinsic :: iso_c_binding, only : C_FLOAT_COMPLEX
            implicit none
            integer :: statusErr
            statusErr = 0
            IF (gTransArr%allocated .EQV. .FALSE.) RETURN
            IF (ALLOCATED(Gc_pool))     DEALLOCATE(Gc_pool)
            IF (ALLOCATED(G1_pool))     DEALLOCATE(G1_pool)
            IF (ALLOCATED(G2_pool))     DEALLOCATE(G2_pool)
            IF (ALLOCATED(G3_pool))     DEALLOCATE(G3_pool)
            IF (ALLOCATED(G4_pool))     DEALLOCATE(G4_pool)
            IF (ALLOCATED(plot_pool))   DEALLOCATE(plot_pool)
            IF (ALLOCATED(plotFull_pool))   DEALLOCATE(plotFull_pool)
            IF (ALLOCATED(GZonal_pool)) DEALLOCATE(GZonal_pool)
            IF (ALLOCATED(G2Zonal_pool)) DEALLOCATE(G2Zonal_pool)
            IF (ALLOCATED(GcZ_pool))    DEALLOCATE(GcZ_pool)

            gTransArr%Gc  => null()     !  Gc(IXMAX/2 + 1, IYMAX) complex grid point array for the periodic transform routine
            gTransArr%G1  => null()     !  G1(IXMAX, IYMAX) grid point arrays for transform routines in 2 d
            gTransArr%G2  => null()     !  G2(IXMAX, IYMAX) grid point arrays for transform routines in 2 d
            gTransArr%G3  => null()     !  G3(IXMAX, IYMAX) grid point arrays for transform routines in 2 d
            gTransArr%G4  => null()     !  G4(IXMAX, IYMAX) grid point arrays for transform routines in 2 d
            gTransArr%plot  => null()   !  plot(IXMAX, IYMAX) grid point array for plot
            gTransArr%plotFull  => null()   !  plotFull(KX, LY) grid point array for full plot
            gTransArr%GZonal  => null() !  GZonal(KX or LY) grid point array for zonal (cosly) transforms
            gTransArr%G2Zonal  => null() !  G2Zonal(KX or LY) grid point array for zonal (cosly) transforms
            gTransArr%GcZ  => null()    !  GcZ(KX or LY) complex grid point array for the periodic transform routine
            gTransArr%allocated = .FALSE.
            gTransArr%defined = .FALSE.
        END function DeallocateTransformArrays

    ! ================================== Allocate and deallocate Statistics arrays
        !> This function allocates the statistics arrays used in the model and sets sizes in the gArrayDims.
        !> Before calling this function the SetDimensions() and the AllocateHorizontalArrays() must
        !> have been called. The AllocateStatsArrays() method sets the %ITOTA, %KXLYA, %NALA, %KXV, %LYV, KXLYA
        !> values. The %KXV and %LYV are used to allocate space for the variance arrays
        !! gArrayDims is The Dims object that contains the dimensions of the model arrays.
        !!                  The fields in this object need to be set by calling the SetDimensions()
        !!                  and AllocateHorizontalArrays() methods. This function then populates 
        !!                  more of the gArrayDims fields.
        !! gUserSettings is The Settings object which contains the current statistics option as
        !!                     well as the horizonal and vertical resolutions.
        FUNCTION AllocateStatsArrays() RESULT(status)
            use ConstantsModule, only: STATS_OPT_NONE, STATS_OPT_MEAN, STATS_OPT_VAR
            use ArrayDimensionsModule
            use ModelParamsModule, only: gUserSettings       ! If referencing a type, Dont use type(Settings), just 'Settings' in an 'only' statement

            integer :: status, levels
            
            status = 0
            levels = 1
            IF(gUserSettings%statsOption .EQ. STATS_OPT_VAR) levels = gArrayDims%NALA
            !!---------------------------------Statistical array sizes:
            !gArrayDims%ITOTA = 1
            !gArrayDims%NALA = 1
            !gArrayDims%KXV = 1
            !gArrayDims%LYV = 1
            !gArrayDims%KXLYA = 1
            !levels = 1
            !
            !!----------------------------------- Total sizes:
            !!  If STATS_OPT_NONE give each array 1 element
            !!  If STATS_OPT_MEAN allocate PSA,APE,RKE,PSS, and all others one element
            !!  If STATS_OPT_VAR allocate all
            !!-------------PSA,(WWA,FSA,ADSA,FTA,ADTA,ADOA) APE,RKE,PSS,V1,V2:  The (WWA,FSA,ADSA,FTA,ADTA,ADOA) are the other 6 fields in PSA
            !
            !IF(gUserSettings%statsOption .NE. STATS_OPT_NONE) THEN ! Both mean and variance use this
            !    gArrayDims%ITOTA = gArrayDims%ITOT
            !    gArrayDims%NALA = gUserSettings%maxLevels
            !end if
            !IF(gUserSettings%statsOption .EQ. STATS_OPT_VAR) THEN
            !    gArrayDims%KXLYA = 2 * gArrayDims%KXLYD * gUserSettings%maxLevels
            !    gArrayDims%NALA = gUserSettings%maxLevels
            !    gArrayDims%KXV = gArrayDims%KX
            !    gArrayDims%LYV = gArrayDims%IYH
            !    levels = gArrayDims%NALA
            !end if
            
            !---------------------------------------------------------Try to allocate:
            ! PSA, APE, RKE, PSS, V1, V2
            ALLOCATE(gStatsArr%PSA(gArrayDims%ITOTA, gArrayDims%NALA, 7),  &
                     gStatsArr%APE(gArrayDims%ITOTA, gArrayDims%NALA),     &
                     gStatsArr%RKE(gArrayDims%ITOTA, gArrayDims%NALA),     &
                     gStatsArr%PSS(gArrayDims%ITOTA, gArrayDims%NALA, 7),  &
                     gStatsArr%V1(gArrayDims%KXV*gArrayDims%LYV, levels),      &   ! V1(KXV*LYV,NALA) when variance options otherwise V(1,1)
                     gStatsArr%V2(gArrayDims%KXV*gArrayDims%LYV, levels),      &   ! V2(KXV*LYV,NALA)
                     gStatsArr%EK(300))

            gStatsArr%allocated = .TRUE. ! Set allocated flag to true

        END FUNCTION AllocateStatsArrays

        function DeallocateStatsArrays() result(statusErr)
            integer statusErr
            
            statusErr = 0
            IF (.NOT. gStatsArr%allocated) RETURN

            IF (ALLOCATED(gStatsArr%PSA))      DEALLOCATE(gStatsArr%PSA)
            IF (ALLOCATED(gStatsArr%APE))     DEALLOCATE(gStatsArr%APE)
            IF (ALLOCATED(gStatsArr%RKE))     DEALLOCATE(gStatsArr%RKE)
            IF (ALLOCATED(gStatsArr%PSS))      DEALLOCATE(gStatsArr%PSS)
            IF (ALLOCATED(gStatsArr%V1))      DEALLOCATE(gStatsArr%V1)
            IF (ALLOCATED(gStatsArr%V2))      DEALLOCATE(gStatsArr%V2)
            IF (ALLOCATED(gStatsArr%EK))      DEALLOCATE(gStatsArr%EK)
            gStatsArr%allocated = .FALSE. ! Set allocated flag to false

        end function DeallocateStatsArrays


END MODULE ModelFieldsModule
