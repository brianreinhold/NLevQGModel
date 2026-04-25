module ModelParamsModule
    use ConstantsModule, only: PLOT_SCALES_SIZE
    implicit none

    ! The original code compacted all the values into a single common block and single buffer
    ! It started with INTEGER*1 IPARAM(1432): A single buffer containing ICOLOR, IBUF, ICN, JCN, MSEN, MP, VAR, VR, VSEN
    ! IPARAM and ICOLOR shared a common address. This was done to simplify writing and reading to files and initializing all to zero.
    ! ICOLOR and IBUF/IBUF1 were used for 16 and 256 color registers, respectively. Defined so: INTEGER*1 ICOLOR(16),IBUF(816),IBUF1(48)
    ! where IBUF and IBUF1 share a common address. The first 16 values of IBUF were IBUF1 which was the 16 colors for the 16-color VGA mode.

    ! After the color values IPARAM(1-816) were the integer parameters ICN(40), JCN(30), MSEN(30), MP(20)
    ! followed by the real*4 VAR(30),VR(30),VSEN(30) for a total at the time of 1432 bytes for IPARAM.
    !--------------------------------------------------------- Parameter arrays:
    !  VAR is the array of real input parameters controlled by GLOAD.
    !  ICN is the arrays of integer input parameters controlled by GLOAD.
    !  JCN is the graphics customizable data
    !  MSEN is the array of parameters for sensitivity runs (see how the model statistics change as a function of a parameter) ?
    !  MP are previous plot parameters
    !  VR are scaling factors for plots. Used to convert nondimensional model data into scaled values for plotting. ?
    !  VSEN play a role in scaling for the sensitivity runs. Still need to figure this out. ?
    ! This structure will be broken into organized type structures for clarity and maintainability.
    ! In this day of 64-bit  processors INTEGER and REAL should be determined by the system/compiler.
    ! Eventually the names of the individual fields will be changed to be more descriptive.

    ! Type defintions
    public :: Settings, ModelRealParams, ModelIntParams, ModelPlotParams, ModelSensParams, ModelScalingParams, PlotControls
    public :: FileStruct
    ! Methods
    public :: initConstants
    ! Instances of the types
    public :: gUserSettings, gModRealParams, gModIntParams, gModPlotParams, gModSensParams, gModScalingParams, gPlotControls
    public :: gNLevFile, gSensFile
    ! Constants that are not expressed as parameters
    public :: gPlotScales

    ! Define the model parameters
    ! These settings determine the size of the arrays used and thus the memory allocations that need to be done.
    ! When these are changed the arrays may need to be reallocated.
    type :: Settings                    ! originally from ICN(31-36) equivalence in NPARAM.f90
                                        ! though ICN(36) was the now useless videomode
        integer :: maxZonalWaves        ! Maximum number of zonal waves
        integer :: maxMeridWaves        ! Maximum number of meridional waves
        integer :: maxLevels            ! Maximum number of levels
        logical :: useTransformMethod    ! Integration method One of:
                                        !    USE_INTERACTION_COEFFICIENTS = .false. 
                                        !    USE_TRANSFORM = .true. (Spectral transform method)
        integer :: statsOption         ! Statistics options One of:
                                        !   STATS_OPT_NONE = 0  Compute no statistics
                                        !   STATS_OPT_MEAN = 1  Compute averages and energy cycle
                                        !   STATS_OPT_VAR = 2   Compute variances in addition to STATS_OPT_MEAN
    end type Settings

    TYPE :: ModelRealParams ! originally from VAR(1-19) equivalence in NPARAM.f90
        LOGICAL :: isDimensionalized     ! True if the parameters that need to be dimensionalized for the user has been done
        
        REAL :: RN      !  Rn is the channel length/width ratio
        REAL :: BETA    !  BETA is the beta parameter
        REAL :: DELT    !  DELT is the length of the time step
        
        ! Graphics
        REAL :: ZHGT    !  ZHGT is the height of the model to be displayed in the first map
        REAL :: ZHGT2   !  ZHGT2 is the height of the model to be displayed in the second map overlay       
        REAL :: ZHGT3   !  ZHGT3 is the height of the model to be displayed in the third map overlay
        REAL :: RLAT    !  RLAT is the latitude of the Hovmoller and vertical structure cross section plot     
        REAL :: RLON    !  RLON is the longitude of the Hovmoller and vertical structure cross section plot
        REAL :: XPOS    !  XPOS,YPOS is the position of the selected point in the model
        REAL :: YPOS    !   for plots of data like weather station plots
        REAL :: BOT     !  BOT,TPP is the pressure range to be displayed in coross section sections
        REAL :: TPP

        ! model (not graphics) related
        REAL :: CONV    !  CONV is a convergence test for equilibrated solutions - nothing to do with plotting
        REAL :: SPINUP  !  SPINUP is a spinup time which we exclude from any SENSIV runs. Sensitivity runs are 
                        !     a sequence of intergrations where a parameter (like the surface friction) changes by
                        !     certain amount and a run is done and statistics taken.
        ! Parameters for a created 'bomb' disturbance like that done for the sudden stratospheric warming experiments.
        REAL :: AMPT    !  Amplitude of the streamfunction for the prescribed "bomb" disturbance feature 
        REAL :: AMPS    !  Amplitude of the temperature for the prescribed "bomb" disturbance feature  
        REAL :: ANGS
        REAL :: EFOLD
    END TYPE ModelRealParams

    type :: ModelIntParams  ! originally from ICN(1-29) equivalence in NPARAM.f90
        integer :: KMAX2    ! The current maximum number of zonal waves in the model (may be <= Settings%maxZonalWaves)
        integer :: LMAX2    ! The current maximum number of meridional waves in the model (may be <= Settings%maxMeridWaves)
        integer :: NLEV     ! NLEV is the current number of vertical levels in the model (may be <= Settings%maxLevels)
        integer :: ICYC     ! ICYC is the number of cycles to run the model for
        integer :: ISTEP    ! ISTEP is the number of steps to run the model for in each cycle
        integer :: outputInterval     ! outputInterval is the number of steps between model outputs
        integer :: plotType    ! plotType is the type of output to display (horizontal maps, hovmoller, vertical cross sections, etc)
        integer :: field1    ! field1 is the first plot field overlay to display (e.g. vorticity, height, temperature, etc)
        integer :: field2    ! field2 is the second plot field overlay to display (e.g. vorticity, height, temperature, etc)
        integer :: IPLT3    ! IPLT3 is the third plot field overlay to display (e.g. vorticity, height, temperature, etc)
        integer :: JX       ! JX is the zonal wavenumber to plot in graphs)
        integer :: JY       ! JY is the meridional wavenumber to plot in graphs
        integer :: LEVP     ! LEVP is the level to plot in graphs (JX, JY, and LEVP together determine the where the given field is being plotted))
        integer :: NUMBX    ! NUMBX is the number of (2-d) maps to plot in the x-direction on the screen
        integer :: NUMBY    ! NUMBY is the number of (2-d) maps to plot in the y-direction on the screen
        integer :: modelType    ! Model type (1 = standard  2 = GQ initial value model (zonal fields held constant))
        integer :: JX0      ! JX0 is the lower range of the zonal wavenumbers being displayed in the two-d plots
        integer :: JY0      ! JY0 is the lower range of the meridional wavenumbers being displayed in the two-d plots
        integer :: JX1      ! JX1 is the upper range of the zonal wavenumbers being displayed in the two-d plots
        integer :: JY1      ! JY1 is the upper range of the meridional wavenumbers being displayed in the two-d plots
        integer :: IV1      ! Parameter 1 to take the variance of 
        integer :: IV2      ! Parameter 2 to take the variance of
        integer :: IPRM1    ! Believe that this is a model parameter to increment in the sensitivity runs
        integer :: IPRM2    ! Believe that this is a model parameter to increment in the sensitivity runs
        integer :: ITER1    ! This is the number of iterations to make of parameter 1 in sensitivity runs
        integer :: ITER2    ! This is the number of iterations to make of parameter 2 in sensitivity runs
                            ! If both ITER1 and ITER2 are set to 1, then there is only one run.
        integer :: KMODE
        integer :: LMODE
        integer :: IADD
        integer :: MP(20) ! MP is the array of previous plot parameters
        integer :: JHOV     ! JHOV indicates whether a multiple-line time series graph shall be plotted as a 'sideways' hovmoller
                            ! For example, an n-level time series of a wave component amplitude can be plotted as N lines or
                            ! interpolated into a Hovmoller so all the values between each line are displayed.
                            !   JHOV = 1 means toggle to a sideways hovmoller plot. Not sure what other values mean beside no sideways hovmoller.
    end type ModelIntParams

    ! These are primarily colors
    type :: ModelPlotParams ! Originally from JCN(1-14) equivalence in NPARAM.f90. Not sure how many we will need to retain
        LOGICAL :: isBarotropic     ! True for barotopic model, false otherwise (not selected directly by user)
        integer :: ISTRM    !  ISTRM is an indicator for streamline or solid color plots with the following  (This looks very video card oriented):
                            !   options, that give the colors of the streamlines, etc.:
                            !   ISTRM =-1 plots solid color
                            !   ISTRM = NCOLOR plots streamlines with the color of the value one higher
                            !   ISTRM = 0 to NCOLOR-1 plots streamline with
                            !         0 contour = color ISTRM
                            !         - contours = color ISTRM-NCOLOR/4
                            !         + contours = color ISTRM+NCOLOR/4
        integer(kind=4) :: JSTRM    ! JSTRM is the streamline RGB color of the second overlay map
        integer(kind=4) :: KSTRM    ! KSTRM is the streamline color of the third overlay map
        integer(kind=4) :: LP1      ! LP1 is the color of the first phase space/time series point
        integer(kind=4) :: LP2      ! LP2 (?) is the color of the second phase space/time series point
        integer(kind=4) :: LPL      ! LPL is the lead color of the phase space/time series cross
        integer(kind=4) :: ICS      !
        integer(kind=4) :: ICA      ! ICA is the color of the phase space axes
        integer(kind=4) :: ICTXT    ! ICTXT is the color of any text
        integer(kind=4) :: ICFH     ! Plot frame hilight color
        integer(kind=4) :: ICF      ! Plot frame color
        integer(kind=4) :: ICFS     ! Plot frame shadow color
        integer(kind=4) :: IZERO    ! Option to set negative values to 0
        integer :: ISCRN    ! ISCRN = 0  means plot directly to screen 
                            !       = 1  means plot to a screen buffer array
    end type ModelPlotParams

    type ModelSensParams    ! Previously MSEN(1-14) [part of the IPARAM buffer] (Still need some work to figure out what these all do)
        integer :: field1   ! field1 is the first parameter to vary in sensitivity runs (MSEN(1))
        integer :: field2   ! field2 is the first parameter to vary in sensitivity runs (MSEN(2))
        integer :: field3   ! field3 is the first parameter to vary in sensitivity runs (MSEN(3))
        integer :: IXAXIS   ! IXAXIS parameter to vary along x-axis; K,L,N,Parameter#2 (MSEN(4))
        integer :: IYAXIS   ! IYAXIS parameter to vary along y-axis MSEN(5)
        integer :: zonalWave ! zonalWave is the zonal wavenumber of the field of view (MSEN(6))
        integer :: meridWave ! zonalWave is the meridional wavenumber of the field of view (MSEN(7))
        integer :: level    ! level is the pressure level of the field of view (MSEN(8))
        integer :: IFLIP    ! IFLIP flip the axes of the sensitivity plots MSEN(9)
        integer :: runNum1  ! run number1 (MSEN(10))  I do not understand the difference between runNum1 and runNum2 yet
        integer :: runNum2  ! run number2 (MSEN(11))
        integer :: integralEnergyIndicator1 ! integralEnergyIndicator1 is the indicator for the integral energy (MSEN(12)) Not sure what these do yet
        integer :: integralEnergyIndicator2 ! integralEnergyIndicator2 is the indicator for the integral energy (MSEN(13))
        integer :: integralEnergyIndicator3 ! integralEnergyIndicator3 is the indicator for the integral energy (MSEN(14))
    end type ModelSensParams
    
    type :: ModelScalingParams ! This is a new type to hold the scaling factors for the model
        real :: VR(30)    ! Scaling factors for converting nondimensional model data to dimensional for plotting
        real :: VSEN(30)  ! Scaling factors for the sensitivity runs
    end type ModelScalingParams
    
    type :: PlotControls
        integer :: activePlotMap    ! Three possible fields can be displayed on a map. The 'active' one is the field that can
                                    ! be changed by the user with the 'field' key. The other two are inactive and will not change
                                    ! These fields are given by field1, field2, IPLT3
                                    ! 1 = field1 is active, 2 = field2 is active, 3 = IPLT3 is active 
        integer :: togglePlotDir    ! Sets toggle direction for graphics display selections. There are toggles that change level,
                                    ! wavenumber, etc. and this determines if the toggle goes up (set to 1) or down (set to zero)
        integer :: plotTempType     ! Indicates what type of temperature is displayed, standard or potential. Takes on two values
                                    ! STANDARD_TEMP or POTENTIAL_TEMP
        logical :: incBasicStateTemp     ! Sets whether or not the  basic state temp profile is included (.true.) or excluded (.false.) from output:
        integer :: vertMotionType   ! Sets which type of vertical motion to view, pressure velocity (OMEGA) or vertical velocity (VERT_WIND)
    end type PlotControls


    type FileStruct
        integer :: fileHandle = -1                  ! Going to use the NEWUNIT feature
        CHARACTER(len=270) :: fileName              ! This is populated by the caller.
        integer(kind=8) :: welfordStart             ! Where in the file the WelfordType starts. This is populated by a write and not the caller
        integer(kind=8) :: dataStart                ! Where in the file the model data starts. This is populated by writes and not the caller.
        integer(kind=8) :: fileSize                 ! The size of the file in bytes. This is populated by writes and not the caller.
    end type FileStruct
    
    type ScaleFactors
        real :: dissipation
        real :: longestWave
        real :: heights
        real :: timeScale
        real :: temperatureScale
        real :: CORIOLIS
    end type ScaleFactors

    type(Settings) :: gUserSettings          !Settings(10, 5, 10, USE_INTERACTION_COEFFICIENTS, STATS_OPT_NONE) ! Initialize to defaults
    type(Settings) :: gUserSettingsPrev      !Settings(10, 5, 10, USE_INTERACTION_COEFFICIENTS, STATS_OPT_NONE) ! Initialize to defaults

    type(ModelRealParams) :: gModRealParams
    type(ModelIntParams) :: gModIntParams
    type(ModelPlotParams) :: gModPlotParams
    type(ModelSensParams) :: gModSensParams
    type(ModelScalingParams) :: gModScalingParams
    type(PlotControls) :: gPlotControls
    type(FileStruct) :: gNLevFile
    type(FileStruct) :: gSensFile
    type(ScaleFactors) :: gScaleFactors

    !-----------------------------------------------------------------  Constants:
    real :: gPlotScales(PLOT_SCALES_SIZE)      ! Scale factors and scaling ratios. Constants give size of the arrays

    contains

        subroutine initConstants()
            use ConstantsModule, only: CORIOLIS, CORIOLIS_INVERSE, SECONDS_PER_DAY, PI, CHANNEL_WIDTH, GRAVITY, &
                   VORTICITY, VORTICITY, HEIGHT, TEMPERATURE, VERTICAL_WIND, TEMP_FORCING, TOT_HGT_TEND, ADV_BETA_HGT_TEND, &
                   OMEGA_HGT_TEND, DIABATIC_HGT_TEND, TOT_TEMP_TEND, ADV_TEMP_TEND, OMEGA_TEMP_TEND, DIABATIC_TEMP_TEND, &
                   HGT_FORCING, OROG_HGT_TEND, OROGRAPHY, NO_PLOT, TOT_HGT_TEND_GRAF, TOT_TEMP_TEND_GRAF, MEAN_HGT_TEND_GRAF, &
                   MEAN_TEMP_TEND_GRAF, HGT_WAVE_SPEED_GRAF, HGT_TEMP_PHASE_DIFF, GROWTH_RATE_GRAF, AMPLITUDE_RATIO_GRAF

            implicit none

            !-------------------------------Scaling for dissipation (in days):
            !  Non dimensionalize: FK(nondimensional) = 1/[coriolis * secondsPerDay * FK(dimensional)]
            gScaleFactors%dissipation = CORIOLIS_INVERSE / SECONDS_PER_DAY    ! 0.1122340 = 2.7 hours
            !-------------------------------Scaling for longest wave (in kilometers):
            gScaleFactors%longestWave = PI * CHANNEL_WIDTH / 500.           ! 10000.00
            !-------------------------------Scaling for heights L*L*f*f/g in meters:
            gScaleFactors%heights = (CHANNEL_WIDTH * CHANNEL_WIDTH * CORIOLIS * CORIOLIS) / GRAVITY  ! 2748.764
            !-------------------------------Scaling for time step 4/(60*f) in minutes:
            gScaleFactors%timeScale = CORIOLIS_INVERSE / 15.                 ! 646.4681
            !-------------------------------Scaling for the temperature field in deg C:
            gScaleFactors%temperatureScale = CHANNEL_WIDTH * CHANNEL_WIDTH * CORIOLIS    ! 2.6121725E+08
            gScaleFactors%CORIOLIS = CORIOLIS

            !-------------------------------Scale factors for the plotted quantities:
            !  Plot order of 2-D: VOR, PS, TH, WW, TF, S, T, FS, ADS, ADT
            !  Plot order of 1-D: ADO, TOP, SF
            
            !  These factors, when multipled by their nondimensional counterpart, generate scaled versions
            !  of the said field.
            !---------------------------Vorticity in 1/s * E-05:
            gPlotScales(VORTICITY) = gScaleFactors%CORIOLIS * 100000.
            !---------------------------Heights in meters:
            gPlotScales(HEIGHT) = gScaleFactors%heights
            !---------------------------Temperature in deg C-Needs 'A(n)' factor:
            gPlotScales(TEMPERATURE) = gScaleFactors%temperatureScale
            !---------------------------Vertical velocity (omega) in mb/hr:
            gPlotScales(VERTICAL_WIND) = 1000. * gScaleFactors%CORIOLIS * 3600. ! 371.2480
            !---------------------------Temperature forcing in deg C-Needs 'A(n)' factor:
            gPlotScales(TEMP_FORCING) = gScaleFactors%temperatureScale
            !---------------------------Total height tendencies m/day:
            gPlotScales(TOT_HGT_TEND) = gPlotScales(HEIGHT) * CORIOLIS * SECONDS_PER_DAY
            !---------------------------Vorticity advection + beta effect:
            gPlotScales(ADV_BETA_HGT_TEND) = gPlotScales(TOT_HGT_TEND)
            !---------------------------Vorticity tendencies from omega:
            gPlotScales(OMEGA_HGT_TEND) = gPlotScales(TOT_HGT_TEND)
            !---------------------------Diabatic height tendencies m/day:
            gPlotScales(DIABATIC_HGT_TEND) = gPlotScales(TOT_HGT_TEND)
            !---------------------------Total temp tendencies C/day:
            gPlotScales(TOT_TEMP_TEND) = gPlotScales(TEMPERATURE) * CORIOLIS * SECONDS_PER_DAY
            !---------------------------Temperature advection:
            gPlotScales(ADV_TEMP_TEND) = gPlotScales(TOT_TEMP_TEND)
            !---------------------------Temperature tendencies from omega:
            gPlotScales(OMEGA_TEMP_TEND) = gPlotScales(TOT_TEMP_TEND)
            !---------------------------Diabatic temperature tendencies C/day:
            gPlotScales(DIABATIC_TEMP_TEND) = gPlotScales(TOT_TEMP_TEND)
            !---------------------------Heights in meters (for strm forcing):
            gPlotScales(HGT_FORCING) = gPlotScales(HEIGHT)
            !---------------------------Orographic advection:
            gPlotScales(OROG_HGT_TEND) = gPlotScales(TOT_HGT_TEND)
            !---------------------------Orographic heights in meters (depends upon DP(1)):
            gPlotScales(OROGRAPHY) = 1.
            !---------------------------No Plot:
            gPlotScales(NO_PLOT) = 1.
            !------------------------------------Height tendencies for balance of forces
            gPlotScales(TOT_HGT_TEND_GRAF) = gPlotScales(TOT_HGT_TEND)
            !------------------------------------Temp tendencies for balance of forces
            gPlotScales(TOT_TEMP_TEND_GRAF) = gPlotScales(TOT_TEMP_TEND)
            !---------------------------Mean height tendencies
            gPlotScales(MEAN_HGT_TEND_GRAF) = gPlotScales(TOT_HGT_TEND)
            !---------------------------Mean temp tendencies
            gPlotScales(MEAN_TEMP_TEND_GRAF) = gPlotScales(TOT_TEMP_TEND)
            !------------------------------------Ground phase speed in m/s
            gPlotScales(HGT_WAVE_SPEED_GRAF) = CHANNEL_WIDTH * CORIOLIS
            !-------------------------------------Phase differences
            gPlotScales(HGT_TEMP_PHASE_DIFF) = 1.
            !------------------------------Growth rate:
            gPlotScales(GROWTH_RATE_GRAF) = 1.
            !------------------------------Amplitude ratio
            gPlotScales(AMPLITUDE_RATIO_GRAF) = 1.
        end subroutine initConstants
    

end module ModelParamsModule