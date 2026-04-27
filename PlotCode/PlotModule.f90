module PlotModule
    USE IFWINTY
    USE, INTRINSIC :: ISO_C_BINDING
    use ConstantsModule, only: wp
    
    implicit none
    
    public  ! Makes everything in the module public (unless otherwise specified)
    
    ! Creating a parameter color ram for Hovmoller diagrams and weather maps.
    integer, parameter :: RGB32 = c_int32_t

    ! Base colors for the contours (little endian)
    integer(RGB32), parameter :: RAM_COLORS(18) = [ &
        INT(z'000000FF'), & ! 1: Deep Blue (Was z'00FF0000')
        INT(z'000066FF'), & ! 2: Blue
        INT(z'00008DFF'), & ! 3: Medium Blue
        INT(z'0000A6FF'), & ! 4: Lighter Blue
        ! Cyans/Greens
        INT(z'0000BBE7'), & ! 5: Cyan-Blue
        INT(z'0000CE9A'), & ! 6: Cyan    
        INT(z'00007861'), & ! 7: Light Cyan
        INT(z'00009655'), & ! 8: Green-Cyan
        ! Greens/Yellows
        INT(z'0000B444'), & ! 9: Green
        INT(z'004DD12D'), & ! 10: Bright Green
        INT(z'00ABEA0D'), & ! 11: Yellow-Green
        INT(z'00FFFF00'), & ! 12: Yellow
        ! Oranges/Reds
        INT(z'00FFC000'), & ! 13: Orange-Yellow
        INT(z'00FF8000'), & ! 14: Orange
        INT(z'00FF0000'), & ! 15: Red
        INT(z'00FF0077'), & ! 16: Magenta
        INT(z'00FF00AA'), & ! 17: Lite Magenta
        INT(z'00FF77AA')  & ! 18: Pink Magenta
    ]
    integer, parameter :: NUM_RAM_COLORS = size(RAM_COLORS)
    
    integer(kind=4), parameter :: WX_STN_HEIGHT_COLOR = Z'00FF6400'           ! RGB(0, 100, 255)
    integer(kind=4), parameter :: WX_STN_TEMP_COLOR = Z'000064FF'             ! RGB(255, 100,0)

    integer(kind=4), parameter :: PHASE_SPACE1_LEAD_COLOR = Z'0000FFFF'
    integer(kind=4), parameter :: PHASE_SPACE1_COLOR = Z'000064FF'
    integer(kind=4), parameter :: PHASE_SPACE2_LEAD_COLOR = Z'00FFFF00'
    integer(kind=4), parameter :: PHASE_SPACE2_COLOR = Z'00FF6400'
    
    integer(kind=4), parameter :: STD_DEV_HEIGHT_COLOR = Z'00FF0000'          ! RGB(0, 0, 255)
    integer(kind=4), parameter :: STD_DEV_TEMP_COLOR = Z'000088FF'            ! RGB(255, 136, 0)

    integer(kind=4), parameter :: KE_COLOR = Z'00FF0000'                      ! RGB(0, 0, 255)
    integer(kind=4), parameter :: APE_COLOR = Z'000055EE'                     ! RGB(238, 85, 0)
    integer(kind=4), parameter :: TOTAL_ENERGY_COLOR = Z'0000FF00'            ! RGB(0, FF, 0)
    
    integer(kind=4), parameter :: RMSE_HEIGHT_COLOR = Z'0077AA00'             ! RGB(0, 170, 119)
    integer(kind=4), parameter :: RMSEM_HEIGHT_COLOR = Z'0000FF00'            ! RGB(0, 255, 0)
    integer(kind=4), parameter :: SPRD_HEIGHT_COLOR = Z'00FF0000'             ! RGB(0, 0, 255)
    integer(kind=4), parameter :: BFIT_HEIGHT_COLOR = Z'00FFFFFF'             ! RGB(225, 225, 225)
    integer(kind=4), parameter :: RMSE_TEMP_COLOR = Z'007700FF'               ! RGB(255, 0, 170)
    integer(kind=4), parameter :: RMSEM_TEMP_COLOR = Z'000055FF'              ! RGB(255, 85, 0)
    integer(kind=4), parameter :: SPRD_TEMP_COLOR = Z'00775577'               ! RGB(119, 85, 119)
    integer(kind=4), parameter :: BFIT_TEMP_COLOR = Z'0000AAFF'               ! RGB(255, 170, 0)



    type :: PlotConfig
        integer :: stn_latitude     ! Latitude for weather station plots
        integer :: stn_longitude    ! Longitude for weather station plots
        integer :: row              ! Row for latitude Hovmoller plots
        integer :: col              ! Column for longitude Hovmoller plots
        integer :: contHghtsMaps    ! Contour interval index for Hovmoller and weather map in meters
        integer :: contHghtsSeries  ! Contour interval for Phase space and time series in meters
        integer :: logfloor         ! Orders of magnitude for log plots
        integer :: contKESeries     ! Contour interval for KE 
        integer :: contTempsMaps    ! Contour interval for Hovmoller and weather maps in deg C
        integer :: contTempsSeries  ! Contour interval  for Phase space and time series in deg C
        integer :: hov_latitude     ! Latitude for Hovmoller plots
        integer :: hov_longitude    ! Longitude for Hovmoller plots
        integer :: phaseSpaceXaxis  ! Which spectral coefficient to plot on X-axis in phase space plots
        integer :: phaseSpaceYaxis  ! Which spectral coefficient to plot on Y-axis in phase space plots
        logical :: weatherStnCom(16)! Which spectral components to inlcude in weather station time series (first 10)
                                    !   and ensemble stats display (11-16)
        LOGICAL :: drawLines        ! Whether to draw connecting lines in time series plots
        real :: rcos(32)            ! Cheap FFT for basis functions for x-direction
        real :: rsin(32)            ! Cheap FFT for basis functions for y-direction
    end type PlotConfig
    
    type :: PhaseSpace
        LOGICAL :: isInitialized = .FALSE.    ! Indicates that this structure has been initialized and is ready to use when true
        real, pointer :: x(:)       ! Points to the variable to be plotted on the x-axis at a certain level
        real, pointer :: y(:)       ! Points to the varibale to be plotted on the y-axis at a certain level
        real :: xScale              ! The scaling factor needed to make the quantity plotting along x dimensional. Includes the 2 or SQRT(2) factor
        real :: yScale              ! The scaling factor needed to make the quantity plotting along y dimensional. Includes the 2 or SQRT(2) factor
        ! In phase space we plot the cosknxsinly component along one axis and sinknxsinly component along the other axis.
        ! In our 1-d expression of the components all the zonal modes come first. THere are gModeIntParams%LMAX2 of these modes.
        ! Then comes the cos component of the first zonal wavenumber followed by the sin component of the first zonal wavenumber
        ! for the first meridional mode for all meridional modes. For example, cosnxsiny, sinnxsiny, cosnxsin2y, sinnxsin2y, 
        ! cosnxsin3y, sinnxsin3y, ... cosnxsinLMAX2y, sinnxsinLMAX2y, cos2nxsiny, sin2nxsiny, ..., cosn2sinLMAX2y, sin2nxsinLMAX2y
        integer :: xIndex       ! index in array (ITOT) for the field to be plotted along the x-axis
        integer :: yIndex       ! index in array (ITOT) for the field to be plotted along the y-axis
        integer :: field        ! The field being plotted. Field ranges from 1 (VORTICITY) to 15 (OROG_HGT_TEND)
        integer(4) :: colorLead ! Color of the point when first plotted
        integer(4) :: color     ! Color of the point next time
        integer(4) :: gDCcolor  ! Reverse byte order do use in GDC calls. DIB sections use reverse order
        integer :: xcenter      ! x-Coordinate of the origin on the screen
        integer :: ycenter      ! y-coordinate of the origin on the screen

        ! This will be set to the position plotted x_pix, y_pix. x_pix, y_pix will be computed from the current values. We do not
        ! need to keep those. x_pix, y_pix will be plotted with color 'colorLead'. In the routine, prev_x, prev_y will be plotted
        ! with color 'color'. Then it will be set to x_pix, y_pix. 
        integer :: prev_x = 0
        integer :: prev_y = 0 
    end type PhaseSpace

    type :: TimeSeriesBuffer
        REAL(4), ALLOCATABLE :: data_buffer(:)
        REAL(wp), ALLOCATABLE :: real8_buffer(:)        ! Use of circular buffer for other purposes than plotting time series
        TYPE(T_POINT), ALLOCATABLE :: plot_pts(:)
        INTEGER(4) :: head_idx
        INTEGER(4) :: count
        LOGICAL :: is_initialized
    end type TimeSeriesBuffer
    
    type :: ElapsedTime
        integer(8) :: timeSteps = 0_8
        real(wp) :: dimensionalTime
        real(wp) :: days
        integer :: years
    end type ElapsedTime
    
    type(ElapsedTime) gElapsedTime

    type(PlotConfig) :: gPlotConfig
    type(PhaseSpace) :: gPhaseSpace1
    type(PhaseSpace) :: gPhaseSpace2
    type(TimeSeriesBuffer) :: gHeightTimeSeriesBuffer
    type(TimeSeriesBuffer) :: gTempTimeSeriesBuffer
    type(TimeSeriesBuffer) :: gHeightStatsTimeSeriesBuffer
    type(TimeSeriesBuffer) :: gTempStatsTimeSeriesBuffer
    type(TimeSeriesBuffer) :: gKeTimeSeriesBuffer
    type(TimeSeriesBuffer) :: gApeTimeSeriesBuffer
    type(TimeSeriesBuffer) :: gTotTimeSeriesBuffer

 
    type :: CartesianData           ! Used only locally 
        integer(4) :: x_origin
        integer(4) :: y_origin
        integer(4) :: num_ticks_x
        integer(4) :: num_ticks_y
        real       :: tick_step_px
    end type CartesianData

    type :: InterpolationBounds
        integer :: IL       ! Lower range in x of the input array to be interpolated
        integer :: IU       ! Upper range in x of the input array to be interpolated
        integer :: JL       ! Lower range in y of the input array to be interpolated (not used in 1-d case)
        integer :: JU       ! Upper range in y of the input array to be interpolated (not used in 1-d case)
        integer :: KL       ! Lower range in x of the output interpolated array
        integer :: KU       ! Upper range in x of the output interpolated array
        integer :: LL       ! Lower range in y of the output interpolated array (not used in 1-d case)
        integer :: LU       ! Upper range in y of the output interpolated array (not used in 1-d case)
    end type InterpolationBounds
    
!=====================  SUBMODULE INTERFACES ============================
! These are subroutines implemented in submodules. In order to put subroutine
! belonging to this module in another file one has to create 'submodules' and
! then create interfaces in the main module for every subroutine in the submodules.
    INTERFACE
        ! Use 'MODULE' keyword here to link it to a submodule
        MODULE SUBROUTINE Interpolate1D(R, b, isPeriodic, X)
            !------------------------------------------------------Passed variables:
            Logical, intent(in) :: isPeriodic
            REAL, intent(in) :: R(:)
            REAL, intent(out) :: X(:)
            type(InterpolationBounds), intent(in) :: b
        END SUBROUTINE Interpolate1D
    END INTERFACE

    INTERFACE
        MODULE SUBROUTINE Interpolate2D(R, b, X)
                ! R(b%IL:b%IU, b%JL:b%JU) - Input Grid
                ! X(b%KL:b%KU, b%LL:b%LU) - Output Grid
                type(InterpolationBounds), intent(in) :: b
                REAL, INTENT(IN)  :: R(:, :)
                REAL, INTENT(OUT) :: X(:, :)
        END SUBROUTINE Interpolate2D
    END INTERFACE

    INTERFACE
        MODULE SUBROUTINE DrawCartesianAxes(invalidate)
                LOGICAL, INTENT(IN)  :: invalidate
        END SUBROUTINE DrawCartesianAxes
    END INTERFACE

    INTERFACE
        MODULE SUBROUTINE labelPlotsAndDisplayTime(invalidate)
                LOGICAL, INTENT(IN)  :: invalidate
        END SUBROUTINE labelPlotsAndDisplayTime
    END INTERFACE

    INTERFACE
        MODULE SUBROUTINE DrawLegendItem(hdc, x, y, color, label)
            USE IFWINTY, ONLY: HANDLE
            INTEGER(HANDLE), intent(in) :: hdc
            INTEGER, intent(in) :: x, y, color
            CHARACTER(len=*), intent(in) :: label
        END SUBROUTINE DrawLegendItem
    END INTERFACE

    contains

!=========================================== INITIALIZE PLOT
        ! Sets up the various plot structures to initialize fields that only need to be set once
        ! during a model run. The user might be able to change some parameters while the model is
        ! running that could require that this routine be called again.
        subroutine initializePlot()
            use ConstantsModule, only: PHASE_SPACE, WEATHER_STN, HOV_DIAG_AT_LAT, HOV_DIAG_AT_LONG, PHASE_AND_MAP, SQRT2, PLOTTING_OFF
            use ModelParamsModule, only: gModIntParams, gPlotScales
            IMPLICIT NONE


            if (gModIntParams%plotType == PHASE_SPACE) then
                if (gModIntParams%field1 < PLOTTING_OFF) then
                    gPhaseSpace1%colorLead = PHASE_SPACE1_LEAD_COLOR
                    gPhaseSpace1%color = PHASE_SPACE1_COLOR
                    call initializePhaseSpaceType(gModIntParams%field1, gPhaseSpace1)
                endif
                if (gModIntParams%field2 < PLOTTING_OFF) then
                    gPhaseSpace2%colorLead = PHASE_SPACE2_LEAD_COLOR
                    gPhaseSpace2%color = PHASE_SPACE2_COLOR
                    call initializePhaseSpaceType(gModIntParams%field2, gPhaseSpace2)
                endif
            endif
            if (gModIntParams%plotType == WEATHER_STN) then
            endif
            if (gModIntParams%plotType == HOV_DIAG_AT_LAT) then
            end if
            if (gModIntParams%plotType == HOV_DIAG_AT_LONG)then
            endif
            if (gModIntParams%plotType == PHASE_AND_MAP) then 
            endif
        
        end subroutine initializePlot
        
        subroutine initializePhaseSpaceType(field, phaseSpaceType)
            use NLevQGModelGlobals, only: gBitMapInfo
            use ConstantsModule, only: SQRT2
            use ModelParamsModule, only: gModIntParams, gPlotScales, gModScalingParams, gIsTemperature
            use ModelFieldsModule, only: gVertArr

            IMPLICIT NONE

            integer, intent(in) :: field
            type(PhaseSpace), intent(inout) :: phaseSpaceType
            
            phaseSpaceType%field = field
            
            ! In phase space we plot the cosknxsinly component along one axis and sinknxsinly component along the other axis.
            ! In our 1-d expression of the k,l components all the zonal modes come first. THere are gModeIntParams%LMAX2 of these modes.
            ! Then comes the cos component of the first zonal wavenumber followed by the sin component of the first zonal wavenumber
            ! for the first meridional mode for all meridional modes. For example, cosnxsiny, sinnxsiny, cosnxsin2y, sinnxsin2y, 
            ! cosnxsin3y, sinnxsin3y, ... cosnxsinLMAX2y, sinnxsinLMAX2y, cos2nxsiny, sin2nxsiny, ..., cosn2sinLMAX2y, sin2nxsinLMAX2y
            ! Get the indicies in the varaible arrays to be plotted:
            ! To get the index for the component along the x-axis we have gModIntParams%zonalWavenumber and gModIntParams%meridWavenumber
            ! provided by the user. For the index along the y-axis we have gModIntParams%sinZonal and gModIntParams%sinMerid.
                
            ! If the user specifies a zonal wavenumber for the cosine part greater than zero
            if (gModIntParams%zonalWavenumber .GT. 0) then
                ! This gets the index for cosknx sinly component
                phaseSpaceType%xIndex = gModIntParams%LMAX2 + 1 + & ! Puts us at the start of the waves
                                        2 * gModIntParams%LMAX2 * (gModIntParams%zonalWavenumber - 1) + & ! Puts us at the start of the Kth zonal wave
                                        2 * (gModIntParams%meridWavenumber - 1)
                !phaseSpaceType%xScale = REAL(min(gBitMapInfo%height, gBitMapInfo%width))
                !phaseSpaceType%xScale = 2.0 * gModScalingParams%VR(field)
                if (gIsTemperature(field)) then
                    phaseSpaceType%xScale = 2.0 * gPlotScales(field) / gVertArr%A(gModIntParams%LEVP)
                else
                    phaseSpaceType%xScale = 2.0 * gPlotScales(field)
                endif
            else
                ! Otherwise we just have the zonal mode cosLy if the user sets the zonal wavenumber to 0
                phaseSpaceType%xIndex = gModIntParams%meridWavenumber
                phaseSpaceType%xScale = SQRT2 * gPlotScales(field)
            endif
            ! If the user specifies a zonal wavenumber for the sine part greater than zero
            if (gModIntParams%sinZonal .GT. 0) then
                ! This gets the index for cosknx sinly component
                phaseSpaceType%yIndex = gModIntParams%LMAX2 + 1 + & ! Puts us at the start of the waves
                                        2 * gModIntParams%LMAX2 * (gModIntParams%sinZonal - 1) + & ! Puts us at the start of the Kth zonal wave
                                        2 * (gModIntParams%sinMerid - 1) + 1
                if (gIsTemperature(field)) then
                    phaseSpaceType%yScale = 2.0 * gPlotScales(field) / gVertArr%A(gModIntParams%LEVP)
                else
                    phaseSpaceType%yScale = 2.0 * gPlotScales(field)
                endif
            else
                ! Otherwise we just have the zonal mode cosLy if the user sets the zonal wavenumber to 0
                phaseSpaceType%yIndex = gModIntParams%meridWavenumber
                phaseSpaceType%yScale = SQRT2 * gPlotScales(field)
            endif
            ! Now get the variable to be plotted along the x-axis. That will be gModIntParams%field1. 
            call loadFields( phaseSpaceType%x, field, gModIntParams%LEVP)
                
            ! Now get the variable to be plotted along the y-axis. That will be gModIntParams%field1. 
            call loadFields( phaseSpaceType%y, field, gModIntParams%LEVP)
            phaseSpaceType%xcenter = gBitMapInfo%width / 2
            phaseSpaceType%ycenter = gBitMapInfo%height / 2
            phaseSpaceType%gDCcolor = IOR(ISHFT(IAND(phaseSpaceType%color, Z'000000FF'), 16), & ! R: bit 0 -> 16
                        IOR(IAND(phaseSpaceType%color, Z'0000FF00'), &            ! G: bit 8 -> 8 (stays)
                        ISHFT(IAND(phaseSpaceType%color, Z'00FF0000'), -16))) ! B: bit 16 -> 0
            phaseSpaceType%isInitialized = .TRUE.

        end subroutine initializePhaseSpaceType

!=========================================== OUTPUT PLOT
        !> This method determines what to plot depending upon the setting of gModIntParams%plotType
        !> and gDataType%dataSource (where the data to be plotted is coming from the model, a file
        !> or a set of ensemble files
        subroutine outputPlot()
            use ConstantsModule, only: WEATHER_MAPS, PHASE_AND_MAP, HOV_DIAG_CROSS_SEC_AT_LAT, HOV_DIAG_AT_LAT, HOV_DIAG_CROSS_SEC_AT_LONG, &
                HOV_DIAG_AT_LONG, PHASE_SPACE,  WAVE_AMP_TIME_SERIES, WAVE_PHASE_TIME_SERIES, WEATHER_STN, NO_PLOT, &
                HEIGHT, TEMPERATURE, SECONDS_PER_DAY, wp
            use ModelParamsModule, only: gModIntParams, gModRealParams, gScaleFactors

            IMPLICIT NONE

            logical :: usesHeight
            logical :: usesTemperature
            
            usesHeight = (gModIntParams%field1 == HEIGHT .OR. gModIntParams%field2 == HEIGHT)
            usesTemperature = (gModIntParams%field1 == TEMPERATURE .OR. gModIntParams%field2 == TEMPERATURE)
            
            ! Allows the user to interact with the program.
            CALL PumpMessages()

    ! NO PLOT SKIP -------------------------
            if (gModIntParams%plotType == NO_PLOT .AND. mod(gElapsedTime%timeSteps, 720) .NE. 0) then
                gElapsedTime%timeSteps = gElapsedTime%timeSteps + gModIntParams%outputInterval
                return
            endif

            ! timeScale * gModRealParams%delt gives the length of each time step in seconds. We divide by secondsPerDay to give days
            ! 'Days' may generate too big of a number - may have to go to years and days
            gElapsedTime%days = REAL(gElapsedTime%timeSteps) * gElapsedTime%dimensionalTime
            gElapsedTime%years = Floor(gElapsedTime%days / 365.0_wp)
            gElapsedTime%days = gElapsedTime%days - gElapsedTime%years * 365
            ! Use an internal write to format the string

    ! NO PLOT No SKIP on 720 -------------------------
            if (gModIntParams%plotType == NO_PLOT) then
                call labelPlotsAndDisplayTime(.true.)
                gElapsedTime%timeSteps = gElapsedTime%timeSteps + gModIntParams%outputInterval
                return
            endif

    ! PHASE SPACE ----------------------
            if (gModIntParams%plotType == PHASE_SPACE) then
                call labelPlotsAndDisplayTime(.true.)
                if(gModIntParams%field1 .NE. NO_PLOT) call plotPhaseSpace(gPhaseSpace1, gModIntParams%field2 == NO_PLOT)
                if(gModIntParams%field2 .NE. NO_PLOT) call plotPhaseSpace(gPhaseSpace2, .TRUE.)
                !if(usesTemperature) call plotPhaseSpace(Y, .true., TEMPERATURE)
            end if

    !! WEATHER STATION ----------------------
    !        if (gModIntParams%plotType == WEATHER_STN) call plotTimeSeries(Y)
    !
    !! ENERGY TIME SERIES ----------------------
    !        if (gModIntParams%plotType == ENERGY_TIME_SERIES) then
    !            if (gDataType%dataSource == MODEL) then
    !                call plotEnergyTimeSeries(KE)
    !            else
    !                call plotEnsembleStatsTimeSeries(KE)
    !            end if
    !        endif
    !
    !! HOVMOLLER LAT ----------------------
    !        if (gModIntParams%plotType == HOV_DIAG_AT_LAT) call plotLatHovmoller(Y)
    !
    !! HOVMOLLER LON ----------------------
    !        if (gModIntParams%plotType == HOV_DIAG_AT_LONG) call plotLonHovmoller(Y)
    !
    !! HOVMOLLER SPECTRAL RMSE ------------
    !        if (gModIntParams%plotType == HOV_DIAG_RMSE .AND. (gDataType%dataSource == FILE .OR. gDataType%dataSource == ENSEMBLES)) then
    !            call plotRmseHovmoller(gDataType)
    !        endif
    !
    !! WEATHER MAP ----------------------
    !        if (gModIntParams%plotType == WEATHER_MAPS) then
    !            ! We do not force the screen to update if gModIntParams%field2 is not equal
    !            ! to NO_PLOT. In that case gModIntParams%field1 gets plotted first and then
    !            ! a second call to plotWeatherMap fir gModIntParams%field2 is made.
    !            
    !            ! If gModIntParams%field1 OR gModIntParams%field2 is NO_PLOT, plotWeatherMap
    !            ! simply returns and does nothing
    !            call plotWeatherMap(Y, (gModIntParams%field2 == NO_PLOT), gModIntParams%field1, .true.) ! Plot solid color contours
    !            ! This will always plot unless gModIntParams%field2 = NO_PLOT.
    !            call plotWeatherMap(Y, .true., gModIntParams%field2, (gModIntParams%field1 == NO_PLOT)) ! If field1 is NO_PLOT this will be solid cover, otherwise lines
    !        endif
    !! WEATHER MAP and PHASE SPACE ----------------------
    !        if (gModIntParams%plotType == PHASE_AND_MAP) then
    !            call plotWeatherMap(Y, .false., gModIntParams%field1, .true.)
    !            call plotWeatherMap(Y, .false., gModIntParams%field2, (gModIntParams%field1 == NO_PLOT))
    !            if(usesHeight) call plotPhaseSpace(Y, (.NOT. usesTemperature), HEIGHT)
    !            if(usesTemperature) call plotPhaseSpace(Y, .true., TEMPERATURE)
    !        endif
    !
    !! RUNNING MEAN STATISTICS ----------------------
    !        if (gModIntParams%plotType == STATISTICS) then
    !            ! Expects the welford array to be in gCurrentWelford
    !            call plotStatisticsTimeSeries()
    !        end if

            gElapsedTime%timeSteps = gElapsedTime%timeSteps + gModIntParams%outputInterval

        end subroutine outputPlot
        
        !> This routine creates the one dimensional array representing the actual
        !> values of the height or temperature field at the latitude selected by
        !> gPlotConfig%hov_latitude. Those array points are calculated at only 33
        !> points along the east-west latitude line. Normally this would be done with an FFT
        !> but since only 10 spectral amplitudes are involved, doing the computations
        !> via brute force probably saves time. This is sped up as the cos and sin
        !> functions are precalculated for 32 points in the period. These pre-calculated
        !> functions are in gPlotConfig%rcos and gPlotConfig%rsin
        !> Of course this is FAR to few points to generate a plot on the screen which
        !> has more than a thousand points. Thus the array (hov(33) which is part of this
        !> module) created by this routine is interpolated using bipolar cubic splines to 
        !> create each pixel value.
        !subroutine createHovmollerAtLat(hov, Y)
        !    use ConstantsModule, only: HEIGHT, TEMPERATURE, wp
        !    use ModelTypesModule, only: PI, scaleHeights
        !
        !    implicit none
        !
        !    real, intent(inout) :: hov(33)
        !    real(wp), intent(in) :: Y(:)
        !
        !    integer :: k, I1, I2
        !    real :: waveLat, zonalLat, waveLat2, zonalLat2
        !    
        !    logical :: usesHeight
        !    logical :: usesTemperature
        !    
        !    usesHeight = (gModIntParams%field1 == HEIGHT .OR. gModIntParams%field2 == HEIGHT)
        !    usesTemperature = (gModIntParams%field1 == TEMPERATURE .OR. gModIntParams%field2 == TEMPERATURE)
        !
        !    if (usesHeight) then
        !        call setVariables(Y, HEIGHT) ! Sets module variables S1-S10 with correct scalings
        !    else
        !        call setVariables(Y, TEMPERATURE)
        !    endif
        !    waveLat = sin(REAL(gPlotConfig%hov_latitude) * PI / 90.0)
        !    zonalLat = S1 * cos(REAL(gPlotConfig%hov_latitude) * PI / 90.0)
        !    waveLat2 = sin(REAL(2 * gPlotConfig%hov_latitude) * PI / 90.0)
        !    zonalLat2 = S4 * cos(REAL(2 * gPlotConfig%hov_latitude) * PI / 90.0)
        !    do K = 1, 33
        !        I1 = IAND(K - 1, 31) + 1
        !        I2 = IAND(2 * (k - 1), 31) + 1
        !        hov(k) = &
        !            (S2 * gPlotConfig%rcos(i1) + S3 * gPlotConfig%rsin(I1) + S7 * gPlotConfig%rcos(i2) + S8 * gPlotConfig%rsin(i2) ) * waveLat &
        !          + (S5 * gPlotConfig%rcos(i1) + S6 * gPlotConfig%rsin(I1) + S9 * gPlotConfig%rcos(i2) + S10 * gPlotConfig%rsin(i2)) * waveLat2 &
        !          + zonalLat + zonalLat2 
        !    end do
        !end subroutine createHovmollerAtLat

        !> This routine creates the one dimensional array representing the actual
        !> values of the height or temperature field at the longitude selected by
        !> gPlotConfig%hov_longitude. Those array points are calculated at only 17
        !> points along the north-south longitude line. Normally this would be done with an FFT
        !> but since only 10 spectral amplitudes are involved, doing the computations
        !> via brute force probably saves time. This is sped up as the cos and sin
        !> functions are precalculated for 32 points in the period. These pre-calculated
        !> functions are in gPlotConfig%rcos and gPlotConfig%rsin. Though we only have
        !> a half period in the meridional direction, we can use our precalculated sin and cos.
        !> Of course this is FAR to few points to generate a plot on the screen which
        !> has more than a thousand points. Thus the array hov(33), of which only 17 are
        !> used in this case, created by this routine is interpolated using bipolar cubic 
        !> splines to create each pixel value.
        !subroutine createHovmollerAtLon(hov, Y)
        !    use ConstantsModule, only: HEIGHT, TEMPERATURE, wp
        !    use ModelTypesModule, only: PI, scaleHeights
        !
        !    implicit none
        !
        !    real, intent(inout) :: hov(17)
        !    real(wp), intent(in) :: Y(:)
        !
        !    integer :: L, L2
        !    real :: waveCosLon, waveSinLon, waveCosLon2, waveSinLon2
        !    logical :: usesHeight
        !    logical :: usesTemperature
        !    
        !    usesHeight = (gModIntParams%field1 == HEIGHT .OR. gModIntParams%field2 == HEIGHT)
        !    usesTemperature = (gModIntParams%field1 == TEMPERATURE .OR. gModIntParams%field2 == TEMPERATURE)
        !
        !    if (usesHeight) then
        !        call setVariables(Y, HEIGHT) ! Sets S1-S10 with correct scalings
        !    else
        !        call setVariables(Y, TEMPERATURE)
        !    endif
        !    waveCosLon = cos(REAL(gPlotConfig%hov_longitude) * PI / 180.0)
        !    waveSinLon = sin(REAL(gPlotConfig%hov_longitude) * PI / 180.0)
        !    waveCosLon2 = cos(REAL(2 * gPlotConfig%hov_longitude) * PI / 180.0)
        !    waveSinLon2 = sin(REAL(2 * gPlotConfig%hov_longitude) * PI / 180.0)
        !    hov = 0.0
        !    do L = 1, 17
        !        L2 = IAND(2 * (L - 1), 31) + 1
        !        hov(L) = &
        !            (S2 * waveCosLon + S3 * waveSinLon + S7 * waveCosLon2 + S8 * waveSinLon2 ) * gPlotConfig%rsin(L) +  S1 * gPlotConfig%rcos(L) &
        !          + (S5 * waveCosLon + S6 * waveSinLon + S9 * waveCosLon2 + S10 * waveSinLon2) * gPlotConfig%rsin(L2) + S4 * gPlotConfig%rcos(L2)
        !    end do
        !end subroutine createHovmollerAtLon

        !> This routine creates the two dimensional grid representing the actual
        !> values of the height or temperature field in the channel. Those array points
        !> are calculated at only GRID(33, 17) points. GRID(33, 17) is part of this
        !> module. Normally this would be done with an FFT but since only 10 spectral 
        !> amplitudes are involved, doing the computations via brute force probably saves
        !> time. This is sped up as the cos and sin functions are precalculated for 32 points in
        !> the period. These pre-calculated functions are in gPlotConfig%rcos and gPlotConfig%rsin. 
        !> Of course this is FAR too few points to generate a plot on the screen which
        !> has more than a thousand points in both directions. Thus the array GRID(33, 17))
        !> created by this routine is interpolated using two dimensional bipolar cubic 
        !> splines to create each pixel value.
        !subroutine createGrid(grid, Y, field)
        !    use ConstantsModule, only: HEIGHT, TEMPERATURE, NO_PLOT, wp
        !    use, intrinsic :: ieee_arithmetic
        !
        !    implicit none
        !
        !    real, intent(inout) :: grid(33, 17)
        !    real(wp), intent(in) :: Y(:)
        !    integer, intent(in) :: field
        !
        !    integer :: lon, lat, I1, I2, j
        !    real :: mode11, mode22, mode21, mode12
        !
        !    if (field == HEIGHT) then
        !        call setVariables(Y, HEIGHT) ! Sets S1-S10 with correct scalings
        !    else if (field == TEMPERATURE) then
        !        call setVariables(Y, TEMPERATURE)
        !    else if (field == NO_PLOT) then
        !        return
        !    endif
        !    ! S1-S10 can represent either the heights or the temperature
        !    ! depending upon a user setting.
        !    do lon = 1, 33
        !        I1 = IAND(lon - 1, 31) + 1
        !        I2 = IAND(2 * (lon - 1), 31) + 1
        !        mode11 = S2 * gPlotConfig%rcos(I1) + S3 * gPlotConfig%rsin(I1)
        !        mode21 = S5 * gPlotConfig%rcos(i1) + S6 * gPlotConfig%rsin(I1)
        !        mode12 = S7 * gPlotConfig%rcos(i2) + S8 * gPlotConfig%rsin(i2)
        !        mode22 = S9 * gPlotConfig%rcos(i2) + S10 * gPlotConfig%rsin(i2)
        !        do lat = 1, 17
        !            J = IAND(2 * (lat - 1), 31) + 1
        !            grid(lon, lat) = mode11 * gPlotConfig%rsin(lat) &
        !                        + mode21 * gPlotConfig%rsin(J) & 
        !                        + mode12 * gPlotConfig%rsin(lat) &
        !                        + mode22 * gPlotConfig%rsin(J) &
        !                        + S1 * gPlotConfig%rcos(lat) & 
        !                        + S4 * gPlotConfig%rcos(J)
        !        end do
        !    end do
        !end subroutine createGrid

        !> This function gets the value of the current location in a paused time series plot based on the y-coordinate
        !> position of the mouse.
        !! @param  mouse_y: the y-ccordinate value of the mouse cursor
        !! @Param  isHeight: when true we get the current height value the y-coordinate corresponds to. Otherwise it
        !!                   gets the temperature. The values are based upon plot type (log or linear) and the
        !!                   values of time series max values user-defined in gPlotConfig
        !function getYValues(mouse_y, isHeight) result(labelText)
        !    use ConstantsModule, only: HEIGHT, TEMPERATURE, WEATHER_STN, ENERGY_TIME_SERIES, MODEL
        !    use WeatherRegimesGlobals, only: gBitMapInfo
        !    use ModelTypesModule, only: gDataType
        !    
        !    implicit none
        !
        !    integer, intent(in) :: mouse_y  ! y-coordinate of mouse
        !    LOGICAL, intent(in) :: isHeight ! are we doing the isHeight option (create string for height values, otherwise temperature values)
        !
        !    real :: value, topValue
        !    character(len=32) :: labelText
        !    character(len=16) :: units
        !
        !    real :: logMin, logMax, logRange, logVal
        !    
        !    logical :: usesHeight
        !    logical :: usesTemperature
        !    
        !    usesHeight = (gModIntParams%field1 == HEIGHT .OR. gModIntParams%field2 == HEIGHT)
        !    usesTemperature = (gModIntParams%field1 == TEMPERATURE .OR. gModIntParams%field2 == TEMPERATURE)
        !
        !    if (gModIntParams%plotType == WEATHER_STN) then
        !        if (isHeight) then
        !            if (.NOT. usesHeight) then
        !                labelText = CHAR(0)
        !                return
        !            endif
        !            topValue = gPlotConfig%contHghtsSeries
        !            units = " m"
        !        else
        !            if (.NOT. usesTemperature) then
        !                labelText = CHAR(0)
        !                return
        !            endif
        !            topValue = gPlotConfig%contTempsSeries
        !            units = CHAR(176)//"C"
        !        endif
        !        ! This 'T12' forces the units to start at the 12th character of the string
        !        value = topValue * (1 - 2.0 * real(mouse_y) / REAL(gBitMapInfo%height))
        !        write(labelText, '(F10.3, T12, A)') value, trim(units)
        !    else if (gModIntParams%plotType == ENERGY_TIME_SERIES) then
        !        if (gDataType%dataSource == MODEL) then
        !            if (.NOT. isHeight) then
        !                labelText = CHAR(0)
        !                return
        !            end if
        !            topValue = gPlotConfig%contKESeries
        !            units = "J/kg"
        !            value = topValue * (1 - real(mouse_y) / REAL(gBitMapInfo%height))
        !            ! This 'T12' forces the units to start at the 12th character of the string
        !            write(labelText, '(F10.3, T12, A)') value, trim(units)
        !        else ! If not 'MODEL' source we are dealing with FILE and ENSEMBLES source and displaying RMSE and SPREADs
        !            if (isHeight) then
        !                ! ANY(myArray) returns .TRUE. if one is true.
        !                ! .NOT. ANY(myArray) returns .TRUE. only if all of them are false.
        !                ! Exit if NONE of 11, 13, or 15 are true
        !                if (.NOT. ANY(gPlotConfig%weatherStnCom(11:15:2))) then
        !                    labelText = CHAR(0)
        !                    return
        !                endif 
        !                topValue = REAL(gPlotConfig%contHghtsSeries)
        !                units = "m"
        !            else
        !                ! Exit if NONE of 12, 14, or 16 are true
        !                if (.NOT. ANY(gPlotConfig%weatherStnCom(12:16:2))) then
        !                    labelText = CHAR(0)
        !                    return
        !                endif 
        !                topValue = REAL(gPlotConfig%contTempsSeries)
        !                units = CHAR(176)//"C"
        !            end if
        !            logMax = log10(topValue)
        !            logMin = logMax - REAL(gPlotConfig%logfloor)  ! Displays gPlotConfig%logfloor orders of magnitude (e.g., 100 to 0.001)
        !            logRange = logMax - logMin
        !            logVal = logMax - logRange * (real(mouse_y)/REAL(gBitMapInfo%height))
        !            value = 10.0**logVal
        !            ! This 'T13' forces the units to start at the 13th character of the string
        !            if (value < .01) then
        !                write(labelText, '(E11.4, T13, A)') value, trim(units)
        !            else
        !                write(labelText, '(F10.4, T13, A)') value, trim(units)
        !            endif
        !        endif
        !    endif
        !    
        !end function getYValues

        subroutine ClearScreen()
            USE IFWINTY
            USE GDI32
            USE USER32
            use IFWBASE ! Contains RGB macro
            use NLevQGModelGlobals, only: gBitMapInfo, ghwndMain
            use ConstantsModule, only: WEATHER_MAPS, PHASE_AND_MAP, HOV_DIAG_CROSS_SEC_AT_LAT, HOV_DIAG_AT_LAT, HOV_DIAG_CROSS_SEC_AT_LONG, &
                HOV_DIAG_AT_LONG, PHASE_SPACE,  WAVE_AMP_TIME_SERIES, WAVE_PHASE_TIME_SERIES, WEATHER_STN, NO_PLOT, &
                HEIGHT, TEMPERATURE, wp
            use ModelParamsModule, only: gModIntParams
            
            IMPLICIT NONE
            
            INTEGER :: iret, i
            integer(HANDLE) :: hBrush, hOldBrush


            hBrush = CreateSolidBrush(RGB(0, 30, 10))       ! Create the color brush
            hOldBrush = SelectObject(gBitMapInfo%hMemDC, hBrush) ! Put it in the DC

            ! 6. Clear the canvas to dark green initially
            iret = PatBlt(gBitMapInfo%hMemDC, 0, 0, &
                    gBitMapInfo%width, gBitMapInfo%height, PATCOPY)

            ! Cleanup the brushes
            iret = SelectObject(gBitMapInfo%hMemDC, hOldBrush)
            iret = DeleteObject(hBrush)
                
            if (gModIntParams%plotType == WEATHER_STN) then
                ! Clear the time series buffers
                call clearBuffer(gHeightTimeSeriesBuffer)
                call clearBuffer(gTempTimeSeriesBuffer) 

                ! Do heights on right
                !call DrawHorizontalLines(.false., & ! No screen update
                !                        .true., & ! put labels on left
                !                        .TRUE., & ! making labels for height field
                !                        #0000DD00) ! color of labels
                !call DrawHorizontalLines(.true., .false., .FALSE., #000055EE) ! true means force screen update. false means draw labels on right side
            !else if (gModIntParams%plotType == ENERGY_TIME_SERIES .OR. gModIntParams%plotType == STATISTICS) then
            !    ! Clear the time series buffers
            !    call clearBuffer(gKETimeSeriesBuffer)
            !    call clearBuffer(gApeTimeSeriesBuffer)
            !    call clearBuffer(gTotTimeSeriesBuffer)
            !    call clearBuffer(gHeightStatsTimeSeriesBuffer)
            !    call clearBuffer(gTempStatsTimeSeriesBuffer)
            !    do i = 1, 8
            !        call clearBuffer(gEnsStatsTimeSeriesBuffer(i))
            !    end do
            !    call clearBuffer(gBestFitPsi)
            !    call clearBuffer(gBestFitTemp)
            !    call DrawHorizontalLines(.false., .true., .false., #00323232) ! The isHeight parameter is not used in this case

            else if (gModIntParams%plotType == PHASE_SPACE) then
                call DrawCartesianAxes(.false.)
            else if (gModIntParams%plotType == HOV_DIAG_AT_LAT) then
                gPlotConfig%row = 0
                !call DrawVerticalLines(.false.)
            else if (gModIntParams%plotType == HOV_DIAG_AT_LONG) then
                gPlotConfig%col = 0
                !call DrawHorizontalLines(.false., .true., .false., #00323232) ! 'isHeight' parameter is not used in this case
            else if (gModIntParams%plotType == WEATHER_MAPS) then
                !call DrawGridLines(.false.)
            else if (gModIntParams%plotType == PHASE_AND_MAP) then
                !call DrawGridLines(.false.)
                call DrawCartesianAxes(.false.)
            endif

            ! Invalidate the window rectangle to trigger a redraw
            iret = InvalidateRect(ghwndMain, LOC(gBitMapInfo%rect), 0) ! 0 means do not erase background
            iret = UpdateWindow(ghwndMain)

        end subroutine ClearScreen

        ! Clears the time series buffers
        subroutine clearBuffer(timeSeriesBuf)
            implicit none

            type(TimeSeriesBuffer), intent(inout) :: timeSeriesBuf

            if (.NOT. timeSeriesBuf%is_initialized) return
            timeSeriesBuf%data_buffer = 0.0
            timeSeriesBuf%head_idx = 0.
            timeSeriesBuf%count = 0
        end subroutine clearBuffer


        ! Helper to point to the arrays containing the user-selected field to  plot
        subroutine loadFields(ptr, field, level)
            use ConstantsModule, only: VORTICITY, HEIGHT, TEMPERATURE, VERTICAL_WIND, TEMP_FORCING, TOT_HGT_TEND, ADV_BETA_HGT_TEND, &
                OMEGA_HGT_TEND, DIABATIC_HGT_TEND, TOT_TEMP_TEND, ADV_TEMP_TEND, OMEGA_TEMP_TEND, DIABATIC_TEMP_TEND, HGT_FORCING, &
                OROG_HGT_TEND, OROGRAPHY

            use ModelFieldsModule, only: gHorzArr
            
            implicit none
            
            real, pointer, intent(out) :: ptr(:)
            integer, intent(in)        :: field
            integer, intent(in)        :: level

            select case (field)
                case(VORTICITY)             ! Vorticity in 10**-5/s
                    ptr => gHorzArr%PS(:, level)      ! PS is the streamfunction spectral components

                case(HEIGHT)                ! Height in meters
                    ptr => gHorzArr%PS(:, level)      ! PS is the streamfunction spectral components

                case(TEMPERATURE)           ! Temperature in Centigrade
                    ptr => gHorzArr%TH(:, level)      ! TH(ITOT,maxLevels) is the temperature spectral components

                case(VERTICAL_WIND)         ! Vertical wind in mb/hr or mm/sec
                    ptr => gHorzArr%WW(:, level)      ! WW(ITOT,maxLevels) is the omega spectral components
                    
                case(TEMP_FORCING)          ! Temperature driving (forcing) - Radiative relaxation temperature in Centigrade
                    ptr => gHorzArr%TF(:, level)      ! TF(ITOT,maxLevels) is the thermal forcing field

                case(TOT_HGT_TEND)          ! Total Height tendency in meters/day
                    ptr => gHorzArr%FS(:, level)      ! FS(ITOT,maxLevels) is the total streamfunction tendency

                case(ADV_BETA_HGT_TEND)     ! Height tendencies in m/day from Advection plus beta
                    ptr => gHorzArr%ADS(:, level)     ! ADS(ITOT,maxLevels) is the spectral form of the vorticity advection

                case(OMEGA_HGT_TEND)        ! Height tendencies in m/day from omega (dp/dt)
                    ptr => gHorzArr%WS(:, level)      ! WS(ITOT,maxLevels) is the omega forcing on the streamfunction

                case(DIABATIC_HGT_TEND)     ! Height tendencies in m/day from Diabatic effects (like dissipation)
                    ptr => gHorzArr%S(:, level)       ! S(ITOT,maxLevels) is the diabatic forcing on the streamfunction

                case(TOT_TEMP_TEND)         ! Total temperture tendencies in C/day
                    ptr => gHorzArr%FT(:, level)      ! FT(ITOT,maxLevels) is the temperature tendency

                case(ADV_TEMP_TEND)         ! Temperature tendencies in C/day from advection
                    ptr => gHorzArr%ADT(:, level)     ! ADT(ITOT,maxLevels) is the spectral form of the temperature advection

                case(OMEGA_TEMP_TEND)       ! Temperature tendencies in C/day from omega
                    ptr => gHorzArr%WT(:, level)      ! WT(ITOT,maxLevels) is the omega forcing on the temperature

                case(DIABATIC_TEMP_TEND)    ! Temperature tendencies in C/day from Diabatic effects
                    ptr => gHorzArr%T(:, level)       ! T(ITOT,maxLevels) is the diabatic forcing on the temperature

                case(HGT_FORCING)           ! Mean streamfunction (height) forcing in m
                    ptr => gHorzArr%SF(:, level)      ! SF(ITOT,maxLevels) is the Barotropic momentum forcing field
                    
                case(OROG_HGT_TEND)         ! Height tendencies from ororgraphy in m/day

            end select
        end subroutine loadFields


        subroutine plotPhaseSpace(phaseSpaceType, invalidate)
            use NLevQGModelGlobals, only: gBitMapInfo, ghwndMain

            USE IFWINTY
            USE GDI32
            USE USER32
            USE IFWBASE ! For RGB
            USE, INTRINSIC :: ISO_C_BINDING

            ! Windows (0,0) is the top-left corner and the range is 

            IMPLICIT NONE

            type(PhaseSpace), intent(inout) :: phaseSpaceType
            LOGICAL, intent(in) :: invalidate

            INTEGER ::      x_pix, y_pix, iret
            REAL ::         xc, yc
            TYPE(T_RECT) :: rect
            INTEGER(4), POINTER :: pixelArray(:, :)

            xc = phaseSpaceType%x(phaseSpaceType%xIndex) * phaseSpaceType%xScale
            yc = phaseSpaceType%y(phaseSpaceType%yIndex) * phaseSpaceType%yScale

            x_pix = phaseSpaceType%xcenter  + INT(xc)   ! Note screen coordinates are 0-based.
            y_pix = phaseSpaceType%ycenter - INT(yc)
            if (x_pix < 0) x_pix = 0
            if (y_pix < 0) y_pix = 0
            if (x_pix >= gBitMapInfo%width) x_pix = gBitMapInfo%width - 1
            if (y_pix >= gBitMapInfo%height) y_pix = gBitMapInfo%height - 1
            ! Maps pixelArray to the DIBSection
            CALL C_F_POINTER(gBitMapInfo%pBits, pixelArray, [gBitMapInfo%width, gBitMapInfo%height])
            ! 3. Draw the lead pixel
            pixelArray(x_pix + 1, y_pix + 1) = phaseSpaceType%colorLead
            ! 4. Draw the previous pixel
            pixelArray(phaseSpaceType%prev_x + 1, phaseSpaceType%prev_y + 1) = phaseSpaceType%color
            ! 5. Set the previous pixel to the current pixel
            phaseSpaceType%prev_x = x_pix
            phaseSpaceType%prev_y = y_pix
            if (invalidate) then
                iret = InvalidateRect(ghwndMain, LOC(gBitMapInfo%rect), 0) ! 0 means do not erase background
                iret = UpdateWindow(ghwndMain)
            endif

        end subroutine plotPhaseSpace

        !> This routine plots the time series of a 'weather station' at a selected lat/lon
        !> coordinate. It computes the value from the spectral coefficients times the associated
        !> basis function by brute force (no 2-d FFT) - there are only 10 modes so not so bad.
        !> Which components are to be included in the plot and which field can also be set.
        !! @param Y The spectral coefficients array. It is passed instead of imported from
        !!          the ModelModule to avoid circular dependencies.
        !subroutine plotTimeSeries(Y)
        !    use ConstantsModule, only: HEIGHT, TEMPERATURE, wp
        !    use ModelTypesModule, only: PI, scaleHeights, scaleTemps, scaleVelocity, gModRealParams, sqrt2
        !    use WeatherRegimesGlobals, only: gBitMapInfo
        !
        !    IMPLICIT NONE
        !
        !    real(wp), intent(in) :: Y(:)
        !
        !    ! --- Variable Declarations ---
        !    REAL :: VALUE, VALUET, hovLon, hovLat, scale, scalet, radx, rady, U, V, rn, wind_dir
        !
        !    real :: mask(10), s_y, s_2y, c_y, c_2y
        !    real :: s_x, c_x, s_2x, c_2x
        !    LOGICAL :: usesHeight, usesTemperature
        !
        !    hovLon = REAL(gPlotConfig%stn_longitude)
        !    hovLat = REAL(gPlotConfig%stn_latitude)
        !    !if (gPlotConfig%field == HEIGHT)
        !    scale = gBitMapInfo%Height / REAL(2 * gPlotConfig%contHghtsSeries)
        !    !if (gPlotConfig%field == TEMPERATURE)
        !    scalet = gBitMapInfo%Height / REAL(2 * gPlotConfig%contTempsSeries)
        !    rn = real(gModRealParams%RN)
        !    radx = hovLon * PI / 180.
        !    rady = hovLat * PI / 90.
        !
        !    ! 1. Pre-calculate trig ONCE
        !    s_x = SIN(radx)
        !    c_x = COS(radx)
        !    s_2x = SIN(2.0 * radx)
        !    c_2x = COS(2.0 * radx)
        !    s_y = SIN(rady)
        !    s_2y = SIN(2.0 * rady)
        !    c_y = COS(rady)
        !    c_2y = COS(2.0 * rady)
        !
        !    ! 2. Create a numeric mask (1.0 if true, 0.0 if false)
        !    ! This removes the need for 'IF' branches
        !    mask = MERGE(1.0, 0.0, gPlotConfig%weatherStnCom(1:10))
        !
        !    ! 3. Value calculation (Vectorized style)
        !    VALUE = mask(2) * 2.0 * Y(2) * c_x * s_y + &
        !            mask(3) * 2.0 * Y(3) * s_x * s_y + &
        !            mask(5) * 2.0 * Y(5) * c_x * s_2y + &
        !            mask(6) * 2.0 * Y(6) * s_x * s_2y + &
        !            mask(7) * 2.0 * Y(7) * c_2x * s_y + &
        !            mask(8) * 2.0 * Y(8) * s_2x * s_y + &
        !            mask(9) * 2.0 * Y(9) * c_2x * s_y + &
        !            mask(10)* 2.0 * Y(10)* s_2x * s_y + &
        !            mask(1) * sqrt2 * Y(1) * c_y + &
        !            mask(4) * sqrt2 * Y(4) * c_2y
        !    VALUE = VALUE * scaleHeights
        !
        !    ! 4. ValueT calculation
        !    VALUET = mask(2) * 2.0 * Y(12) * c_x * s_y + &
        !             mask(3) * 2.0 * Y(13) * s_x * s_y + &
        !             mask(5) * 2.0 * Y(15) * c_x * s_2y + &
        !             mask(6) * 2.0 * Y(16) * s_x * s_2y + &
        !             mask(7) * 4.0 * Y(17) * c_2x * s_y + &
        !             mask(8) * 4.0 * Y(18) * s_2x * s_y + &
        !             mask(9) * 4.0 * Y(19) * c_2x * s_y + &
        !             mask(10)* 4.0 * Y(20) * s_2x * s_y + &
        !             mask(1) * sqrt2 * Y(11) * c_y + &
        !             mask(4) * sqrt2 * Y(14) * c_2y
        !    VALUET = VALUET * scaleTemps
        !
        !    ! Right now we dont plot the winds but one day I will figure out how to introduce it into the UI.
        !    U = 0.0
        !    V = 0.0
        !    if (gPlotConfig%weatherStnCom(2)) then
        !        U = U - 2. * Y(2) * c_x * c_y     ! d/dy Y(2)*[2cosnx*siny]
        !        V = V - 2. * Y(2) * RN * s_x * s_y ! d/dx Y(2)*[2cosnx*siny]
        !    endif
        !    if (gPlotConfig%weatherStnCom(3)) then
        !        U = U - 2. * Y(3) * s_x * c_y      ! d/dy Y(3)*[2sinnx*siny]
        !        V = V + 2. * Y(3) * RN * c_x * s_y  ! d/dx Y(3)*[2sinnx*siny]
        !    endif
        !    if (gPlotConfig%weatherStnCom(5))then
        !        U = U - 2. * 2. * Y(5) * c_x * c_2y   ! d/dy Y(5)*[2cosnx*sin2y]
        !        V = V - 2. * Y(5) * RN * s_x * s_2y    ! d/dx Y(5)*[2cosnx*sin2y]
        !    endif
        !    if (gPlotConfig%weatherStnCom(6)) then
        !        U = U - 2. * 2. * Y(6) * s_x * c_2y   ! d/dy Y(6)*[2sinnx*sin2y]
        !        V = V + 2. * Y(6) * RN * c_x * s_2y    ! d/dx Y(6)*[2sinnx*sin2y]
        !    endif
        !    if (gPlotConfig%weatherStnCom(7)) then
        !        U = U - 2. * Y(7) * c_2x * c_y           ! d/dy Y(7)*[2cos2nx*siny]
        !        V = V - 2. * 2. * RN * Y(7) * s_2x * s_y  ! d/dx Y(7)*[2cos2nx*siny]
        !    endif
        !    if (gPlotConfig%weatherStnCom(8)) then
        !        U = U - 2. * Y(8) * s_2x * c_y            ! d/dy Y(8)*[2sin2nx*siny]
        !        V = V + 2. * 2. * RN * Y(8) * c_2x * s_y   ! d/dx Y(8)*[2sin2nx*siny]
        !    endif
        !    if (gPlotConfig%weatherStnCom(9)) then
        !        U = U - 2. * 2. * Y(9) * c_2x * c_2y       ! d/dy Y(9)*[2cos2nx*sin2y]
        !        V = V - 2. * 2. * RN * Y(9) * s_2x * s_2y   ! d/dx Y(9)*[2cos2nx*sin2y]
        !    end if
        !    if (gPlotConfig%weatherStnCom(10)) then
        !        U = U - 2. * 2. * Y(10) * s_2x * c_2y       ! d/dy Y(10)*[2sin2nx*sin2y]
        !        V = V + 2. * 2. * RN * Y(10) * c_2x * s_2y   ! d/dx Y(10)*[2sin2nx*sin2y]
        !    endif 
        !    if (gPlotConfig%weatherStnCom(1))then
        !        u = u + sqrt2 * Y(1) * s_y ! d/dy Y(1)*[cosy]  (d/dx is 0)
        !    endif
        !    if (gPlotConfig%weatherStnCom(4)) then
        !        u = u + 2. * sqrt2 * Y(4) * s_2y! d/dy Y(4)*[cos2y]  (d/dx is 0)
        !    endif
        !    u = u * scaleVelocity
        !    v = v * scaleVelocity
        !    ! Result is in degrees (0=North, 90=East, 180=South, 270=West)
        !    wind_dir = ATAN2(-u, -v) * 180.0 / PI
        !    ! Ensure the result is in the range [0, 360)
        !    if (wind_dir < 0.0) wind_dir = wind_dir + 360.0
        !    u = SQRT(u * u + v * v)
        !
        !    usesHeight = (gModIntParams%field1 == HEIGHT .OR. gModIntParams%field2 == HEIGHT)
        !    usesTemperature = (gModIntParams%field1 == TEMPERATURE .OR. gModIntParams%field2 == TEMPERATURE)
        !    ! #0000FF00 = alpha, blue, green, red
        !    if (usesHeight) then
        !        call loadTimeSeriesArray(gHeightTimeSeriesBuffer, value, scale, gBitMapInfo%height / 2, WX_STN_HEIGHT_COLOR, & ! gBitMapInfo%height is screen height
        !            .true., &   ! Always Clear screen since heights are first
        !            (.NOT. usesTemperature))    ! draw moving timelines if true, False for both, true if temperature is not done
        !        if (.NOT. usesTemperature) call labelPlotsAndDisplayTime(.false.) ! Do it here if no temperature plot
        !        call DrawHorizontalLines((.NOT. usesTemperature), .true., .true., WX_STN_HEIGHT_COLOR) ! Force screen update if no temperature plot
        !        ! If the temperature is NOT being plotted, we need to update the screen. true means Draw labels on left side, next .true. is to that these are heights
        !    end if
        !    if (usesTemperature) then
        !        call loadTimeSeriesArray(gTempTimeSeriesBuffer, valuet, scalet, gBitMapInfo%height / 2, WX_STN_TEMP_COLOR, &
        !            (.NOT. usesHeight), & ! Clear screen if no height plot
        !            .true.)     ! Always draw moving time lines since temperature is last
        !        call labelPlotsAndDisplayTime(.false.)
        !        call DrawHorizontalLines(.true., .false., .false., WX_STN_TEMP_COLOR) ! true means force screen update. false means draw labels on right side
        !    end if
        !
        !end subroutine plotTimeSeries

        !subroutine plotStatisticsTimeSeries()
        !    use ConstantsModule, only: HEIGHT, TEMPERATURE
        !    use ModelTypesModule, only: scaleHeights, scaleTemps, gCurrentWelford
        !    use WeatherRegimesGlobals, only: gBitMapInfo
        !    use StatisticsModule, only: getVariance
        !
        !    IMPLICIT NONE
        !
        !    ! --- Variable Declarations ---
        !    REAL :: VALUE, VALUET, scale, scalet
        !    INTEGER :: J, countStrm, countTemp
        !    
        !    logical :: usesHeight
        !    logical :: usesTemperature
        !    
        !    usesHeight = (gModIntParams%field1 == HEIGHT .OR. gModIntParams%field2 == HEIGHT)
        !    usesTemperature = (gModIntParams%field1 == TEMPERATURE .OR. gModIntParams%field2 == TEMPERATURE)
        !    scale = gBitMapInfo%Height / REAL(gPlotConfig%contHghtsSeries)
        !    scalet = gBitMapInfo%Height / REAL(gPlotConfig%contTempsSeries)
        !
        !    VALUE = 0.0
        !    VALUET = 0.0
        !
        !    do J = 1, 10
        !        if (gPlotConfig%weatherStnCom(J)) VALUE = VALUE + getVariance(gCurrentWelford(J), .false.)
        !        if (gPlotConfig%weatherStnCom(J)) VALUET = VALUET + getVariance(gCurrentWelford(J + 10), .false.)
        !    end do
        !
        !    VALUE = SQRT(VALUE) * scaleHeights
        !    VALUET = SQRT(VALUET) * scaleTemps
        !
        !    ! #0000FF00 = alpha, blue, green, red
        !    if (usesHeight) then
        !        call loadTimeSeriesArray(gHeightStatsTimeSeriesBuffer, value, scale, gBitMapInfo%height, STD_DEV_HEIGHT_COLOR, & ! gBitMapInfo%height is screen height
        !            .true., &   ! Always Clear screen since heights are first
        !            (.NOT. usesTemperature))    ! draw moving timelines if true, False for both, true if temperature is not done
        !        if (.NOT. usesTemperature) call labelPlotsAndDisplayTime(.false.) ! Do it here if no temperature plot
        !        call DrawHorizontalLines((.NOT. usesTemperature), .true., .true., #00DD0000) ! Force screen update if not temperature plot
        !        ! If the temperature is NOT being plotted, we need to update the screen. true means Draw labels on left side, next .true. is to that these are heights
        !    end if
        !    if (usesTemperature) then
        !        call loadTimeSeriesArray(gTempStatsTimeSeriesBuffer, valuet, scalet, gBitMapInfo%height, STD_DEV_TEMP_COLOR, &
        !            (.NOT. usesHeight), & ! Clear screen if no height plot
        !            .true.)     ! Always draw moving time lines since temperature is last
        !        call labelPlotsAndDisplayTime(.false.)
        !        call DrawHorizontalLines(.true., .false., .false., #000077FF) ! true means force screen update. false means draw labels on right side
        !    end if
        !
        !end subroutine plotStatisticsTimeSeries
        
        !> This routine plots the time series of the model kinetic and potential energies.
        !! @param KE The kinetic energy. It is passed instead of imported from
        !!          the ModelModule to avoid circular dependencies.
        !subroutine plotEnergyTimeSeries(KE)
        !    use IFWBASE ! For RGB
        !    use ConstantsModule, only: wp, MODEL, FILE, ENSEMBLES, HEIGHT, TEMPERATURE
        !    use ModelTypesModule, only: scaleKE, scaleHeights, scaleTemps, gDataType
        !    use WeatherRegimesGlobals, only: gBitMapInfo
        !
        !    IMPLICIT NONE
        !    
        !    real(wp), intent(in) :: KE(:) ! This can have length 2, 3, or 4
        !    
        !    real :: scale, valueAPE, valueKE, valueTot
        !
        !
        !    scale = real(scaleKE) * REAL(gBitMapInfo%Height) / REAL(gPlotConfig%contKESeries)
        !    valueKe = REAL(KE(1))
        !    valueApe = REAL(KE(2))
        !    valueTot = REAL(KE(3))
        !    call loadTimeSeriesArray(gKeTimeSeriesBuffer, valueKe, scale, gBitMapInfo%height, KE_COLOR, .true., .false.) ! KE blue
        !    call loadTimeSeriesArray(gApeTimeSeriesBuffer, valueApe, scale, gBitMapInfo%height, APE_COLOR, .false., .false.) ! APE orange
        !    call loadTimeSeriesArray(gTotTimeSeriesBuffer, valueTot, scale, gBitMapInfo%height, TOTAL_ENERGY_COLOR, .false., .true.) ! Total green
        !    call labelPlotsAndDisplayTime(.false.)
        !    call DrawHorizontalLines(.true., .true., .false., #00323232) ! All plots have the same units; 'isHeight' parameter is not used
        !end subroutine plotEnergyTimeSeries
        !
        !!> This routine plots the time series of the rmse and spread for single and multiple ensembles.
        !!> THe spread comes only from multiple ensembles
        !!! @param KE(1-4) rmse strm, rmse temp, spread strm, spread temp.
        !subroutine plotEnsembleStatsTimeSeries(KE)
        !    use IFWBASE ! For RGB
        !    use ConstantsModule, only: wp, HEIGHT, TEMPERATURE, ENSEMBLES
        !    use ModelTypesModule, only: scaleHeights, scaleTemps, gDataType
        !    use WeatherRegimesGlobals, only: gBitMapInfo
        !
        !    IMPLICIT NONE
        !
        !    integer, parameter :: MAX_LINES = 8
        !
        !    real(wp), intent(in) :: KE(:) ! This can have length 2 or 8
        !
        !    real :: logValues(MAX_LINES), scales(MAX_LINES)
        !    real :: logVal(MAX_LINES), logMax(MAX_LINES), logMin(MAX_LINES), logRange(MAX_LINES)
        !    real :: final_pixel_value(MAX_LINES)
        !
        !    integer :: i, firstIdx, lastIdx, maxNumSeries, saveField
        !    integer(kind=4) :: color(MAX_LINES) = &
        !        [RMSEM_HEIGHT_COLOR, RMSEM_TEMP_COLOR, SPRD_HEIGHT_COLOR, SPRD_TEMP_COLOR, BFIT_HEIGHT_COLOR, BFIT_TEMP_COLOR, RMSE_HEIGHT_COLOR, RMSE_TEMP_COLOR]
        !
        !    logical :: userSelections(MAX_LINES), invalidate, drawLeft, ensSrc, userSelectedStrm, userSelectedTemp
        !
        !    ensSrc = gDataType%dataSource == ENSEMBLES
        !    maxNumSeries = 2
        !    scales(1) = real(scaleHeights)
        !    scales(2) = real(scaleTemps)
        !    ! Prepare log scales
        !    logMax(1) = log10(REAL(gPlotConfig%contHghtsSeries))
        !    logMax(2) = log10(REAL(gPlotConfig%contTempsSeries))
        !    
        !    ! if we are coming from ENSEMBLES data source there will also be Spread time series and best fits
        !    if(ensSrc) then
        !        maxNumSeries = MAX_LINES
        !        scales(3) = scales(1)
        !        scales(4) = scales(2)
        !        scales(5) = scales(1)
        !        scales(6) = scales(2)
        !        scales(7) = scales(1)
        !        scales(8) = scales(2)
        !        logMax(3) = logMax(1)
        !        logMax(4) = logMax(2)
        !        logMax(5) = logMax(1)
        !        logMax(6) = logMax(2)
        !        logMax(7) = logMax(1)
        !        logMax(8) = logMax(2)
        !        
        !    endif
        !
        !    final_pixel_value = 0.0
        !    do I = 1, maxNumSeries
        !        ! 1. Define the range (must match DrawHorizontalLines)
        !        logMin(i) = logMax(i) - REAL(gPlotConfig%logfloor)  ! This is the 10^-xx floor (range of log)
        !        logRange(i) = logMax(i) - logMin(i)
        !        ! Set the values
        !        logValues(I) = REAL(KE(I)) * scales(I)
        !        ! Check against a log(0) which is undefined. We will treat anything less than our pre-determined min value as a 0 and set to the min
        !        if (logValues(I) > 10.0**logMin(I)) then
        !            logValues(I) = log10(logValues(I))
        !        else
        !            logValues(I) = logMin(I)
        !        end if
        !        final_pixel_value(I) = REAL(gBitMapInfo%height) - &
        !                                REAL(gBitMapInfo%height) * (logMax(I) - logValues(I)) / logRange(I)
        !    end do
        !
        !    !write(*, '(A, I0)') "maxNumSeries= ", maxNumSeries
        !
        !    ! Extract the MAX_SIZE user choices for the ensemble RMSE/SPREAD/BEST_FIT displays into a temporary logical array
        !    userSelections(1:6) = gPlotConfig%weatherStnCom(11:16)
        !    userSelections(7) = gPlotConfig%weatherStnCom(11)
        !    userSelections(8) = gPlotConfig%weatherStnCom(12)
        !    ! If the user has selected SPREADs but is doing a single ensemble comparison there is no spread/best fit so turn them off.
        !    if (.NOT. ensSrc) then
        !        userSelections(3:8) = .false.
        !    endif
        !    userSelectedStrm = ANY(userSelections(1:5:2)) ! The user picked at least one option that is heights
        !    userSelectedTemp = ANY(userSelections(2:6:2)) ! The user picked at least one option that is temperature
        !    ! Find the first and last .TRUE. indices (relative to this 1-6 array)
        !    firstIdx = findloc(userSelections, .TRUE., dim=1)
        !    lastIdx  = findloc(userSelections, .TRUE., dim=1, back=.TRUE.)
        !
        !    !write(*, '(A, I0, A, I0)') "firstIdx= ", firstIdx, " lastIdx= ", lastIdx
        !
        !    ! Loop through and call plotting routine
        !    do i = 1, maxNumSeries
        !        if (userSelections(i)) then
        !            ! Call routine with conditional booleans
        !            call loadTimeSeriesArray(gEnsStatsTimeSeriesBuffer(i), final_pixel_value(I), 1.0, gBitMapInfo%height, color(I), &
        !                (i == firstIdx), & ! clearScreen is TRUE only for the first plot
        !                (i == lastIdx) )   ! drawMovingTimeLines is TRUE only for the last plot
        !        end if
        !    end do
        !    call labelPlotsAndDisplayTime(.false.)  ! False means we do not invalidate (force a screen redraw)
        !    ! If the user has selected to display a heights time series 'userSelectedStrm' will be true
        !    ! If the user has selected to display a temperature time series 'userSelectedTemp' will be true
        !    ! Now we need to draw axes and need to know how to label the y-axes values
        !    ! If the user selects only to plot heights or temps but not both, the labels will appear on the left.
        !    ! If the user selects both, then heights will be to the left, and temperatures to the right
        !    ! We also need to know if to clear the screen. The first field to be plotted clears the screen; that is done in loadTimeSeriesArray
        !    ! We then need to know whether to update the screen. The last DrawHorizontalLines() method updates the screen
        !
        !    if (userSelectedStrm) then
        !        drawLeft = .true.
        !        invalidate = .false.
        !        if (.NOT. userSelectedTemp) invalidate = .true.
        !        call DrawHorizontalLines(invalidate, drawLeft, .true., #0000AA00) ! 'isHeight' set to TRUE
        !    endif
        !    if (userSelectedTemp) then
        !        drawLeft = .true.
        !        invalidate = .true.
        !        if (userSelectedStrm) drawLeft = .false.
        !        call DrawHorizontalLines(invalidate, drawLeft, .false., RGB(221, 68, 0)) 
        !    endif
        !end subroutine plotEnsembleStatsTimeSeries
        
        !subroutine loadTimeSeriesArray(timeSeriesBuf, value, scale, yzero_axis, color, clearScreen, drawMovingTimeLines)
        !    use WeatherRegimesGlobals, only: gBitMapInfo, ghwndMain
        !    use ModelTypesModule, only: timeScale, secondsPerDay, gModRealParams, gModIntParams, gDataType
        !
        !    USE IFWINTY
        !    USE IFWINA   ! Replaced GDI32 for standard Intel compatibility
        !    USE USER32
        !    USE, INTRINSIC :: ISO_C_BINDING
        !    implicit none
        !
        !    type(TimeSeriesBuffer), intent(inout) :: timeSeriesBuf
        !    real, intent(in) :: value, scale
        !    integer(4), intent(in) :: yzero_axis
        !    integer(4), intent(in) :: color
        !    logical, intent(in) :: clearScreen
        !    logical, intent(in) :: drawMovingTimeLines
        !    
        !    LOGICAL(BOOL)   :: bret
        !    INTEGER(4)      :: i, read_idx, iret, flippedColor
        !    INTEGER(HANDLE) :: hBrush, hOldBrush, hPen, hOldPen, hPenAxis, hOldFont, hFontAxis
        !
        !    INTEGER(4), POINTER :: pixelArray(:, :)
        !    integer :: ix, iy, first_mark , gridInterval, kInterval
        !    character(len=32) :: timeStr
        !    real :: days
        !    integer :: years
        !
        !
        !    ! 2. Initialization & Re-allocation (Check if width changed)
        !    IF (.NOT. timeSeriesBuf%is_initialized .OR. size(timeSeriesBuf%data_buffer) /= gBitMapInfo%width) THEN
        !        ! This auto allocates/reallocates to the correct size AND initializes to zero
        !        timeSeriesBuf%data_buffer = spread(0.0, 1, gBitMapInfo%width)
        !                                    ! implied do loop for auto allocation here since plot_pts is a derived type T_POINT
        !        timeSeriesBuf%plot_pts    = [(T_POINT(0, 0), i = 1, gBitMapInfo%width)]
        !        timeSeriesBuf%head_idx = 0
        !        timeSeriesBuf%count = 0
        !        timeSeriesBuf%is_initialized = .TRUE.
        !    END IF
        !
        !    ! 3. Update Circular Buffer (1-based)
        !    timeSeriesBuf%head_idx = MOD(timeSeriesBuf%head_idx, gBitMapInfo%width) + 1
        !    timeSeriesBuf%data_buffer(timeSeriesBuf%head_idx) = scale * value
        !    timeSeriesBuf%count = MIN(timeSeriesBuf%count + 1, gBitMapInfo%width) ! Keep a count of how many points are loaded. This is to stop the sprious 
        !                                                                          ! zero points at the start when the buffer has not been filled. So count will
        !                                                                          ! will go from 0 to screen width. ONce at the screen width it will stay there
        !                                                                          ! until reset.
        !
        !    ! 4. Prepare Point Array (Mapping circular buffer to 0-based screen X)
        !    DO i = 1, timeSeriesBuf%count
        !        ! Offset the starting point so the "newest" data is always on the right
        !        read_idx = MOD(timeSeriesBuf%head_idx - timeSeriesBuf%count + i, gBitMapInfo%width)
        !        if (read_idx < 0) read_idx = read_idx + gBitMapInfo%width
        !        read_idx = read_idx + 1 ! Back to 1-based
        !
        !        timeSeriesBuf%plot_pts(i)%x = (gBitMapInfo%width - timeSeriesBuf%count) + (i - 1)
        !        ! NOTE: Flip and Scale Y. Windows Y=0 is top. 
        !        ! This positions the y-axis = 0
        !        timeSeriesBuf%plot_pts(i)%y = yzero_axis - NINT(timeSeriesBuf%data_buffer(read_idx))
        !    END DO
        !    ! 5. Clear Background (Dark Green)
        !    hBrush = CreateSolidBrush(RGB(0, 30, 10))
        !    hOldBrush = SelectObject(gBitMapInfo%hMemDC, hBrush)
        !    if (clearScreen) iret = PatBlt(gBitMapInfo%hMemDC, 0, 0, gBitMapInfo%width, gBitMapInfo%height, PATCOPY)
        !
        !    ! 6. Draw lines (Optional: using your brush color or a pen)
        !    ! [Insert your MoveToEx/LineTo axes code here if desired]
        !    if (gPlotConfig%drawLines) then
        !        ! Create a Solid Pen: PS_SOLID, Width=2, Color=Yellow (RGB 255, 255, 0)
        !        hPen = CreatePen(PS_SOLID, 0, color) ! 0  = 1-pixel wide line regardless of GDI logical units
        !        hOldPen = SelectObject(gBitMapInfo%hMemDC, hPen)
        !
        !        ! 7. Draw the entire line at once
        !        ! Polyline uses the current Pen.
        !        bret = Polyline(gBitMapInfo%hMemDC, timeSeriesBuf%plot_pts(1), timeSeriesBuf%count - 1)
        !        ! Cleanup GDI Objects
        !        hOldPen = SelectObject(gBitMapInfo%hMemDC, hOldPen)
        !        bret = DeleteObject(hPen)
        !    else
        !        ! gdi approach: SetPixelV in a loop (Slower)
        !        !do i = 1, gBitMapInfo%width
        !        !    bret = SetPixelV(gBitMapInfo%hMemDC, timeSeriesBuf%plot_pts(i)%x, timeSeriesBuf%plot_pts(i)%y, RGB(0, 255, 0))
        !        !end do
        !        bret = GdiFlush()
        !        CALL C_F_POINTER(gBitMapInfo%pBits, pixelArray, [gBitMapInfo%width, gBitMapInfo%height])
        !
        !        ! 1. Pre-calculate flipped color once outside the loop
        !        ! Converts 0x00BBGGRR (Polyline) to 0x00RRGGBB (DIB)
        !        flippedColor = IOR(ISHFT(IAND(color, Z'000000FF'), 16), & ! R: bit 0 -> 16
        !                       IOR(IAND(color, Z'0000FF00'), &            ! G: bit 8 -> 8 (stays)
        !                           ISHFT(IAND(color, Z'00FF0000'), -16))) ! B: bit 16 -> 0
        !        do i = 1, timeSeriesBuf%count
        !            ! Get coordinates from your buffer
        !            ix = timeSeriesBuf%plot_pts(i)%x
        !            iy = timeSeriesBuf%plot_pts(i)%y
        !        
        !            ! Bounds checking is critical when using pointers!
        !            if (ix >= 0 .and. ix < gBitMapInfo%width .and. &
        !                iy >= 0 .and. iy < gBitMapInfo%height) then
        !        
        !                ! Direct memory write (Fast!)
        !                ! Color #0000FF00 is Green in ABGR (00 Alpha, 00 Red, FF Green, 00 Blue)
        !                pixelArray(ix + 1, iy + 1) = flippedColor 
        !            end if
        !        end do
        !    end if
        !
        !    hpenaxis = createpen(ps_solid, 2, rgb(50, 50, 50)) ! 0  = 1-pixel wide line regardless of gdi logical units
        !    hOldpen = selectobject(gbitmapinfo%hmemdc, hpenaxis)
        !
        !    ! 8. Draw X-Axis (Horizontal line through the middle)
        !    bret = movetoex(gbitmapinfo%hmemdc, 0, gbitmapinfo%height / 2, null)
        !    bret = lineto(gbitmapinfo%hmemdc, gbitmapinfo%width, gbitmapinfo%height / 2)
        !
        !    if (drawMovingTimeLines) then 
        !        hOldFont = SelectObject(gBitMapInfo%hMemDC, hFontAxis) ! Use a small font
        !        bret = SetTextColor(gBitMapInfo%hMemDC, RGB(100, 100, 100))
        !        ! Now draw the vertical lines which represent time. These axes will need to move to the left
        !        ! as new values come in.
        !        ! How many pixels represent the gap between lines
        !        gridInterval = gBitMapInfo%width / 20 
        !    
        !        ! The distance between lines in units of 'obsK'
        !        ! If you plot every 10 steps, a 50-pixel gap represents 500 steps
        !        kInterval = gridInterval * gModIntParams%outputInterval
        !    
        !        ! Align the first mark to the nearest kInterval
        !        first_mark = (gDataType%obsK / kInterval) * kInterval
        !
        !        do i = first_mark, first_mark - (gBitMapInfo%width * gModIntParams%outputInterval), -kInterval
        !            if (i < 0) exit 
        !
        !            ! NEW CALCULATION:
        !            ! Difference in timesteps divided by interval = difference in pixels
        !            ix = (gBitMapInfo%width - 1) - ((gDataType%obsK - i) / gModIntParams%outputInterval)
        !
        !            if (ix >= 0) then
        !                ! 1. Draw Vertical Line (using MoveToEx/LineTo)
        !                bret = MoveToEx(gBitMapInfo%hMemDC, ix, gBitMapInfo%height, NULL)
        !                bret = LineTo(gBitMapInfo%hMemDC, ix, 0)
        !
        !                ! 2. Draw Text Label which will be years:days.fraction
        !                days =  REAL(i) * REAL(gDimensionalTime)
        !                years = Floor(days / 365.0_wp)
        !                days = days - years * 365
        !                write(timeStr, '( I0, ":", F0.2)') years, days
        !                bret = SetTextAlign(gBitMapInfo%hMemDC, TA_CENTER)
        !                bret = TextOut(gBitMapInfo%hMemDC, ix, 2, trim(timeStr), len_trim(timeStr))
        !            end if
        !        end do
        !        hOldFont = SelectObject(gBitMapInfo%hMemDC, hOldFont)
        !        bret = DeleteObject(hFontAxis)
        !    endif
        !
        !    ! Cleanup GDI Objects
        !    hOldPen = SelectObject(gBitMapInfo%hMemDC, hOldPen)
        !    bret = DeleteObject(hPenAxis)
        !    hOldBrush = SelectObject(gBitMapInfo%hMemDC, hOldBrush)
        !    bret = DeleteObject(hBrush)
        !
        !end subroutine loadTimeSeriesArray

        !> Plot a hovmoller row at a specified latitude on the screen
        !! @Param: row The row on the bit map to plot (0 is the top row
        !subroutine plotLatHovmoller(Y)
        !    use ConstantsModule, only : HEIGHT, TEMPERATURE, wp
        !    use WeatherRegimesGlobals, only: gBitMapInfo, ghwndMain
        !    use Kernel32, only: RtlMoveMemory
        !    USE IFWIN
        !    USE GDI32
        !    USE USER32
        !    USE, INTRINSIC :: iso_c_binding, only: c_intptr_t, c_loc
        !    
        !    IMPLICIT NONE
        !    
        !    real(wp), intent(in) :: Y(:)
        !
        !    LOGICAL(BOOL)   :: bret
        !    REAL ::         hov(33), X(gBitMapInfo%width), cntInt
        !    
        !    integer(RGB32), target :: Pixels(gBitMapInfo%width)
        !    integer ::  iVal, i, colorIdx
        !    
        !    integer(c_intptr_t) :: baseAddr, targetAddr, srcAddr
        !    integer(c_intptr_t) :: bytesPerRow
        !    
        !    logical :: usesHeight
        !    logical :: usesTemperature
        !    type(InterpolationBounds) b
        !    
        !    usesHeight = (gModIntParams%field1 == HEIGHT .OR. gModIntParams%field2 == HEIGHT)
        !    usesTemperature = (gModIntParams%field1 == TEMPERATURE .OR. gModIntParams%field2 == TEMPERATURE)
        !
        !    call createHovmollerAtLat(hov, Y)
        !    b%il = 1
        !    b%iu = 32
        !    b%KL = 1
        !    b%KU = gBitMapInfo%width
        !    call Interpolate1D(hov, b, .true., X) ! is periodic
        !    
        !    ! Turn X into an integer array of 32-pixel values
        !    if (usesHeight) then
        !        cntInt = 1.0 / REAL(gPlotConfig%contHghtsMaps)
        !    else if (usesTemperature) then
        !        cntInt = 1.0 / REAL(gPlotConfig%contTempsMaps)
        !    end if
        !    do I = 1, gBitMapInfo%width
        !        ! Calculate value based on the contour interval
        !        iVal = FLOOR(X(I) * cntInt) + 9
        !        ! Cycle through the colorRam if iVal exceeds its size
        !        ! Use ABS to handle negative iVal if necessary
        !        ! Modulo keeps the recylcing postive versus mod which does not
        !        colorIdx = modulo(iVal, NUM_RAM_COLORS) + 1
        !
        !        Pixels(I) = RAM_COLORS(colorIdx)
        !    end do
        !            
        !    ! 1. Width * 4 bytes per pixel (32-bit DIBs have no padding)
        !    bytesPerRow = int(gBitMapInfo%width, c_intptr_t) * 4
        !    
        !    ! 2. Calculate memory address for the desired row 'targetY'
        !    ! y=0 is the top row because 'bmi%bmiHeader%biHeight' is negative
        !    baseAddr = transfer(gBitMapInfo%pBits, baseAddr)
        !    targetAddr = baseAddr + (int(gPlotConfig%row, c_intptr_t) * bytesPerRow)
        !    
        !    ! 3. Get the address of your source array
        !    ! Pixels must have the TARGET attribute
        !    srcAddr = transfer(c_loc(Pixels(1)), srcAddr)
        !
        !    ! 4. Move the interpolated array of Pixels into the DIBSection memory
        !    ! Call the Intel-defined interface
        !    ! Since the interface defines these as integers, pass them directly.
        !    call RtlMoveMemory(targetAddr, srcAddr, bytesPerRow)
        !    
        !    gPlotConfig%row = mod(gPlotConfig%row + 1, gBitMapInfo%height)
        !    call labelPlotsAndDisplayTime(.false.)
        !    call DrawVerticalLines(.true.) ! True invalidates window and forcesd redraw
        !
        !end subroutine plotLatHovmoller
        !
        !subroutine plotLonHovmoller(Y)
        !    use ConstantsModule, only: HEIGHT, TEMPERATURE, wp
        !    use WeatherRegimesGlobals, only: gBitMapInfo, ghwndMain
        !    USE, INTRINSIC :: iso_c_binding, only: C_F_POINTER
        !    
        !    IMPLICIT NONE
        !    
        !    real(wp), intent(in) :: Y(:)
        !
        !    LOGICAL(BOOL)   :: bret
        !    REAL ::         hov(17), X(gBitMapInfo%height), cntInt
        !    
        !    integer(RGB32), target :: Pixels(gBitMapInfo%height)
        !    integer ::  iVal, i, colorIdx
        !
        !    integer(RGB32), pointer :: dibArray(:,:)
        !    
        !    logical :: usesHeight
        !    logical :: usesTemperature
        !    type (InterpolationBounds) b
        !    
        !    usesHeight = (gModIntParams%field1 == HEIGHT .OR. gModIntParams%field2 == HEIGHT)
        !    usesTemperature = (gModIntParams%field1 == TEMPERATURE .OR. gModIntParams%field2 == TEMPERATURE)
        !
        !    call createHovmollerAtLon(hov, Y)
        !    b%il = 1
        !    b%iu = 16
        !    b%KL = 1
        !    b%KU = gBitMapInfo%height
        !    call Interpolate1D(hov, b, .false., X) ! not periodic
        !    
        !    ! Turn X into an integer array of 32-pixel values
        !    if (usesHeight) then
        !        cntInt = 1.0 / REAL(gPlotConfig%contHghtsMaps)
        !    elseif (usesTemperature) then
        !        cntInt = 1.0 / REAL(gPlotConfig%contTempsMaps)
        !    end if
        !    do I = 1, gBitMapInfo%height
        !        ! Calculate value based on the contour interval
        !        iVal = FLOOR(X(I) * cntInt) + NUM_RAM_COLORS / 2
        !        ! Cycle through the colorRam if iVal exceeds its size
        !        ! Use ABS to handle negative iVal if necessary
        !        ! Modulo keeps the recylcing postive versus mod which does not
        !        colorIdx = MODULO(iVal, NUM_RAM_COLORS) + 1
        !        Pixels(I) = RAM_COLORS(colorIdx)
        !    end do
        !    
        !    ! Get access to the DIBSection 
        !    call C_F_POINTER(gBitMapInfo%pBits, dibArray, [gBitMapInfo%width, gBitMapInfo%height])
        !
        !    ! 2. Loop through 'Pixels' array (height) 
        !    ! and place each pixel into the DIBSection row-by-row
        !    do i = gBitMapInfo%height, 1, -1
        !        dibArray(gPlotConfig%col + 1, gBitMapInfo%height - i + 1) = Pixels(i)
        !    end do
        !    
        !    gPlotConfig%col = mod(gPlotConfig%col + 1, gBitMapInfo%width)
        !    call labelPlotsAndDisplayTime(.false.)
        !    call DrawHorizontalLines(.true., .true., .false., #00646464) ! True invalidates window and forced redraw; 'isHeight is not used here
        !
        !end subroutine plotLonHovmoller
        
        !subroutine plotRmseHovmoller(datType)
        !    use ConstantsModule, only: HEIGHT, TEMPERATURE, wp
        !    use ModelTypesModule, only: DataType, scaleHeights, scaleTemps
        !    use WeatherRegimesGlobals, only: gBitMapInfo, ghwndMain
        !    USE, INTRINSIC :: iso_c_binding, only: C_F_POINTER
        !    
        !    IMPLICIT NONE
        !    
        !    type(DataType), intent(in) :: datType
        !    
        !    logical :: usesHeight
        !    logical :: usesTemperature
        !    type (InterpolationBounds) b
        !    
        !    REAL :: hov(10), X(gBitMapInfo%height), scale, logMin, cntInt
        !    integer(RGB32), target :: Pixels(gBitMapInfo%height)
        !    integer ::  iVal, i, colorIdx, iLogMin
        !    integer(RGB32), pointer :: dibArray(:,:)
        !
        !    usesHeight = (gModIntParams%field1 == HEIGHT .OR. gModIntParams%field2 == HEIGHT)
        !    usesTemperature = (gModIntParams%field1 == TEMPERATURE .OR. gModIntParams%field2 == TEMPERATURE)
        !    if (usesHeight) then
        !        hov(1 : 10) = ABS(datType%ensMeanY(1 : 10) - datType%obsY(1 : 10))
        !        scale = real(scaleHeights)
        !        cntInt = 1.0 / REAL(gPlotConfig%contHghtsMaps)
        !    else if (usesTemperature) then
        !        usesHeight = .false.
        !        hov(1 : 10) = ABS(datType%ensMeanY(11 : 20) - datType%obsY(11 : 20))
        !        scale = real(scaleTemps)
        !        cntInt = 1.0 / REAL(gPlotConfig%contTempsMaps)
        !    else
        !        return
        !    endif
        !
        !    logMin = -REAL(gPlotConfig%logfloor)
        !    iLogMin = INT(logMin)
        !
        !    do I = 1, 10
        !        ! Set the values
        !        hov(I) = hov(I) * scale  ! More efficient to do this in the loop instead of one liner initializing code outside the loop (if I have more loop stuff)
        !        ! Check against a log(0) which is undefined. We will treat anything less than our pre-determined min value as a 0 and set to the min
        !        if (hov(I) > 10.0**logMin) then
        !            hov(I) = log10(hov(I))
        !        else
        !            hov(I) = logMin
        !        end if
        !    end do
        !
        !    b%il = 1
        !    b%iu = 10
        !    b%KL = 1
        !    b%KU = gBitMapInfo%height
        !    call Interpolate1D(hov, b, .false., X) ! not periodic
        !    
        !    ! Turn X into an integer array of 32-pixel values
        !    do I = 1, gBitMapInfo%height
        !        ! Calculate value based on the contour interval
        !        iVal = FLOOR(X(I)) - iLogMin
        !        ! Cycle through the colorRam if iVal exceeds its size
        !        ! Use ABS to handle negative iVal if necessary
        !        ! Modulo keeps the recylcing postive versus mod which does not
        !        colorIdx = MODULO(iVal, NUM_RAM_COLORS) + 1
        !        Pixels(I) = RAM_COLORS(colorIdx)
        !    end do
        !    
        !    ! Get access to the DIBSection 
        !    call C_F_POINTER(gBitMapInfo%pBits, dibArray, [gBitMapInfo%width, gBitMapInfo%height])
        !
        !    ! 2. Loop through 'Pixels' array (height) 
        !    ! and place each pixel into the DIBSection row-by-row
        !    do i = gBitMapInfo%height, 1, -1
        !        dibArray(gPlotConfig%col + 1, gBitMapInfo%height - i + 1) = Pixels(i)
        !    end do
        !    
        !    gPlotConfig%col = mod(gPlotConfig%col + 1, gBitMapInfo%width)
        !    call labelPlotsAndDisplayTime(.false.)
        !    call DrawHorizontalLines(.true., .true., usesHeight, #00646464)
        !end subroutine PlotRmseHovmoller
        
        !! @param Y: the spectral components array
        !! @param invalidate: if true, invalidate rectangle and update to force screen redraw
        !! @param field: which field to plot, one of HEIGHT, TEMPERATURE, or NO_PLOT
        !! @param solidColor: if true, draw a solid color contour map, if false, draw line contours
        !subroutine plotWeatherMap(Y, invalidate, field, solidColor)
        !    use ConstantsModule, only: HEIGHT, TEMPERATURE, NO_PLOT, wp
        !    use WeatherRegimesGlobals, only: gBitMapInfo, ghwndMain
        !    use Kernel32, only: RtlMoveMemory
        !    USE IFWIN
        !    USE GDI32
        !    USE USER32
        !    USE, INTRINSIC :: iso_c_binding, only: c_intptr_t, c_loc, C_F_POINTER
        !    
        !    IMPLICIT NONE
        !    
        !    real(wp), intent(in) :: Y(:)
        !    LOGICAL, intent(in) :: invalidate
        !    integer, intent(in) :: field
        !    LOGICAL, intent(in) :: solidColor
        !
        !    LOGICAL(BOOL)   :: bret
        !    REAL :: grid(33, 17), cntInt
        !    type (InterpolationBounds) b
        !
        !    REAL, save, ALLOCATABLE, TARGET :: X(:,:), X_Overlay(:, :)  ! Created on the heap
        !    REAL, POINTER :: Xptr(:,:)
        !    integer(c_int), pointer :: pixelArray(:, :) ! Our map to the DIBSection
        !
        !    integer(RGB32), target :: Pixels(gBitMapInfo%width)
        !    integer ::  iVal, iValRight, iValDown, i, j, J1, colorIdx
        !    
        !    integer(c_intptr_t) :: baseAddr, targetAddr, srcAddr
        !    integer(c_intptr_t) :: bytesPerRow
        !    integer(4) :: colorToDraw
        !    
        !    if (field == NO_PLOT) return    ! Do nothing
        !
        !    ! Do the brute force calculation of the current model values on a grid (a poor
        !    ! man's FFT). This routine also dimensionalizes the values
        !    call createGrid(grid, Y, field)
        !    
        !    ! --- RESIZE LOGIC ---
        !    ! Check if X needs allocation or resizing based on the DIB size
        !    if (solidColor) then
        !        if (.not. allocated(X)) then
        !            allocate(X(gBitMapInfo%width, gBitMapInfo%height / 2))
        !        else if (size(X, 1) /= gBitMapInfo%width .or. size(X, 2) /= gBitMapInfo%height / 2) then
        !            deallocate(X)
        !            allocate(X(gBitMapInfo%width, gBitMapInfo%height / 2))
        !        end if
        !    end if
        !    ! Check if X_Overlay needs allocation or resizing based on the DIB size
        !    if (.NOT. solidColor) then
        !        if (.not. allocated(X_Overlay)) then
        !            allocate(X_Overlay(gBitMapInfo%width, gBitMapInfo%height / 2))
        !        else if (size(X_Overlay, 1) /= gBitMapInfo%width .or. size(X_Overlay, 2) /= gBitMapInfo%height / 2) then
        !            deallocate(X_Overlay)
        !            allocate(X_Overlay(gBitMapInfo%width, gBitMapInfo%height / 2))
        !        end if
        !    end if
        !
        !    if (solidColor) then
        !        Xptr => X        ! Note the pointer and not the equals! Not like C!
        !    else
        !        Xptr => X_Overlay
        !    endif
        !    ! Interpolate the grid array to a resolution that matches the size of the display of the
        !    ! weather map on the screen.
        !    b%il = 1
        !    b%iu = 33
        !    b%jl = 1
        !    b%ju = 17
        !    b%KL = 1
        !    b%KU = gBitMapInfo%width
        !    b%LL = 1
        !    b%LU = gBitMapInfo%height / 2
        !    call Interpolate2D(grid, b, Xptr)
        !    ! However, we need to map these interpolated values in X to colors for each pixel. We will
        !    ! fill a row (Pixels) with pixel color values and then copy it to the DIBSection. This is
        !    ! a lot more efficient than creating a 2d array of color values and then writing that to
        !    ! the DIBSection.
        !    
        !    ! In order to make contours instead of color that continuously transition for each value,
        !    ! We create a color ram of 18 values and let the user determine the contour interval. 
        !    
        !    ! 1. Width * 4 bytes per pixel (32-bit DIBs have no padding)
        !    bytesPerRow = int(gBitMapInfo%width, c_intptr_t) * 4
        !    ! 2. Calculate base address
        !    ! y=0 is the top row because 'bmi%bmiHeader%biHeight' is negative
        !    baseAddr = transfer(gBitMapInfo%pBits, baseAddr)
        !    
        !    ! Here we define the factor that will generate the contour intervals.
        !    if (field == HEIGHT) then
        !        cntInt = 1.0 / REAL(gPlotConfig%contHghtsMaps)
        !    else if (field == TEMPERATURE) then
        !        cntInt = 1.0 / REAL(gPlotConfig%contTempsMaps)
        !    end if
        !    if (solidColor) then
        !        ! Turn X into an integer array of 32-pixel values
        !        do J = gBitMapInfo%height / 2, 1, -1
        !            J1 = gBitMapInfo%height / 2 - J + 1
        !            do I = 1, gBitMapInfo%width
        !                ! Calculate value based on the contour interval
        !                iVal = FLOOR(Xptr(I, J1) * cntInt) + NUM_RAM_COLORS / 2
        !                ! Cycle through the colorRam if iVal exceeds its size
        !                ! Use ABS to handle negative iVal if necessary
        !                ! Modulo keeps the recycling positive versus mod which does not
        !                colorIdx = modulo(iVal, NUM_RAM_COLORS) + 1
        !                Pixels(I) = RAM_COLORS(colorIdx)
        !            end do
        !            ! 2. Calculate memory address for the desired row 'targetY'
        !            ! y=0 is the top row because 'bmi%bmiHeader%biHeight' is negative
        !            targetAddr = baseAddr + (int(J - 1, c_intptr_t) * bytesPerRow)
        !    
        !            ! 3. Get the address of your source array
        !            ! Pixels must have the TARGET attribute
        !            srcAddr = transfer(c_loc(Pixels(1)), srcAddr)
        !
        !            ! 4. Move the interpolated array of Pixels into the DIBSection memory
        !            ! Call the Intel-defined interface
        !            ! Since the interface defines these as integers, pass them directly.
        !            call RtlMoveMemory(targetAddr, srcAddr, bytesPerRow)
        !        end do
        !    else
        !        CALL C_F_POINTER(gBitMapInfo%pBits, pixelArray, [gBitMapInfo%width, gBitMapInfo%height])
        !        ! 2. Calculate borders by comparing current pixel to its neighbors.
        !        ! We stop at Width-1 and Height-1 to prevent "out of bounds" on the iVal checks.
        !        ! Mapping X_Overlay to the upper half of the screen
        !        do J = 1, (gBitMapInfo%height / 2) - 1
        !
        !            ! J_data maps Screen Top (J=1) to Array North (Height/2)
        !            J1 = (gBitMapInfo%height / 2) - J + 1
        !            ! J_next is the row physically below J on the screen (one step South in data)
        !            !J_next = J_data - 1
        !
        !            do I = 1, gBitMapInfo%width - 1
        !
        !                ! Calculate interval bins
        !                iVal      = floor(X_Overlay(I, J1) * cntInt)
        !                iValRight = floor(X_Overlay(I + 1, J1) * cntInt)
        !                iValDown  = floor(X_Overlay(I, J1 - 1) * cntInt)
        !
        !                ! Detect boundary
        !                if (iVal /= iValRight .or. iVal /= iValDown) then
        !                    colorIdx = max(iVal, iValRight, iValDown)    ! reuse colorIdx for the max level
        !                    if (colorIdx > 0) then
        !                        colorToDraw = z'00DD0077' ! Reddish-pink
        !                    else if (colorIdx < 0) then
        !                        colorToDraw = z'000000FF' ! Blue
        !                    else 
        !                        colorToDraw = z'00000000' ! White for the 0-contour
        !                    end if
        !                    
        !                    ! J=1 is the top row of the pixelArray
        !                    pixelArray(I, J) = colorToDraw
        !                end if
        !            end do
        !        end do
        !    endif
        !    call labelPlotsAndDisplayTime(.false.)
        !    call DrawGridLines(invalidate)
        !
        !end subroutine plotWeatherMap

        !! This method populates a grid X of all the interpolated values that are displayed on the
        !! screen in weather maps. Recall the screen values are just ARGB color values. The array
        !! X contains the actual values for each pixel on the screen. This allows us to display the
        !! actual value of weather map when we pause the integration and mouse over the map.
        !! This routine is called from the main window callback when the user presses F1 while the
        !! model is integrating. This array is populated only once for every pause.
        !subroutine populateInterpolatedGrid(spectralComponents, X, field)
        !    use ConstantsModule, only: NO_PLOT, wp
        !    use WeatherRegimesGlobals, only: gBitMapInfo
        !    implicit none
        !    
        !    real(wp), intent(in) :: spectralComponents(:)
        !    real, intent(out) :: X(:, :)
        !    integer, intent(in) :: field
        !
        !    REAL :: grid(33, 17)
        !    type(InterpolationBounds) b
        !    
        !    if (field == NO_PLOT) return
        !
        !    ! Creates a small grid of values directly from the spectral components. THis is a brute 
        !    ! force approach - a larger resolution model would use an FFT. But we only have 10 components
        !    ! and doing it brute force with sine and cosine functions is not too costly.
        !    call createGrid(grid, spectralComponents, field)
        !    ! Generate the interpolate 2D array X
        !    b%il = 1
        !    b%iu = 33
        !    b%jl = 1
        !    b%ju = 17
        !    b%KL = 1
        !    b%KU = gBitMapInfo%width
        !    b%LL = 1
        !    b%LU = gBitMapInfo%height / 2
        !    call Interpolate2D(grid, b, X)
        !
        !end subroutine populateInterpolatedGrid

        
        SUBROUTINE PumpMessages()
            USE IFWINTY
            USE USER32
            IMPLICIT NONE
    
            TYPE(T_MSG) :: msg
            LOGICAL(4)  :: lret ! Throwaway return variable
    
            ! 1. PeekMessage used as a FUNCTION to control the loop
            DO WHILE (PeekMessage(msg, NULL, 0, 0, PM_REMOVE))

                ! 2. These can be called with 'CALL' to ignore their return values
                lret = TranslateMessage(msg)
                lret = DispatchMessage(msg)
                
                ! 3. Check for the Quit message specifically
                IF (msg%message == WM_QUIT) STOP "User closed the application."

            END DO
        END SUBROUTINE


end module PlotModule