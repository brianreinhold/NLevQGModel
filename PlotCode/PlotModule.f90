module PlotModule
    USE IFWINTY
    USE, INTRINSIC :: ISO_C_BINDING
    use ConstantsModule, only: PLOTTING_OFF, wp
    
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

    integer(kind=4), parameter :: PHASE_SPACE_HEIGHT_LEAD_COLOR = Z'00FFFF00'     ! RGB(0, 255, 255)
    integer(kind=4), parameter :: PHASE_SPACE_HEIGHT_COLOR = Z'00FF6400'          ! RGB(0, 100, 255)
    integer(kind=4), parameter :: PHASE_SPACE_TEMP_LEAD_COLOR = Z'0000FFFF'       ! RGB(255, 255, 0)
    integer(kind=4), parameter :: PHASE_SPACE_TEMP_COLOR = Z'000064FF'            ! RGB(255, 100,0)
    
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



    !type :: PlotConfig
    !    integer :: stn_latitude     ! Latitude for weather station plots
    !    integer :: stn_longitude    ! Longitude for weather station plots
    !    integer :: row              ! Row for latitude Hovmoller plots
    !    integer :: col              ! Column for longitude Hovmoller plots
    !    integer :: contHghtsMaps    ! Contour interval index for Hovmoller and weather map in meters
    !    integer :: contHghtsSeries  ! Contour interval for Phase space and time series in meters
    !    integer :: logfloor         ! Orders of magnitude for log plots
    !    integer :: contKESeries     ! Contour interval for KE 
    !    integer :: contTempsMaps    ! Contour interval for Hovmoller and weather maps in deg C
    !    integer :: contTempsSeries  ! Contour interval  for Phase space and time series in deg C
    !    integer :: hov_latitude     ! Latitude for Hovmoller plots
    !    integer :: hov_longitude    ! Longitude for Hovmoller plots
    !    integer :: phaseSpaceXaxis  ! Which spectral coefficient to plot on X-axis in phase space plots
    !    integer :: phaseSpaceYaxis  ! Which spectral coefficient to plot on Y-axis in phase space plots
    !    logical :: weatherStnCom(16)! Which spectral components to inlcude in weather station time series (first 10)
    !                                !   and ensemble stats display (11-16)
    !    LOGICAL :: drawLines        ! Whether to draw connecting lines in time series plots
    !end type PlotConfig
    
    type :: TimeSeriesBuffer
        REAL(4), ALLOCATABLE :: data_buffer(:)
        REAL(wp), ALLOCATABLE :: real8_buffer(:)        ! Use of circular buffer for other purposes than plotting time series
        TYPE(T_POINT), ALLOCATABLE :: plot_pts(:)
        INTEGER(4) :: head_idx
        INTEGER(4) :: count
        LOGICAL :: is_initialized
    end type TimeSeriesBuffer
    
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
        integer :: field = PLOTTING_OFF       ! The field being plotted. Field ranges from 1 (VORTICITY) to 15 (OROG_HGT_TEND)
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
    
    type :: BaseMap
        LOGICAL :: isInitialized = .FALSE.  ! Indicates that this structure has been initialized and is ready to use when true
        LOGICAL :: useSolidColor    ! When true, the map is plotted using solid colors and not lines
        real, pointer :: value(:)   ! Points to the variable to be plotted at a certain level
        real :: contourIntervalInv  ! The contour interval for the map. Since the field has to be divided by the contour interval specified
        real :: scale               ! The scaling factor needed to make the quantity dimensional and contoured.
        integer :: field = PLOTTING_OFF  ! The field being plotted. Field ranges from 1 (VORTICITY) to 16 (OROGRAPHY) and PLOTTING_OFF (17)
    end type BaseMap

    type, extends(BaseMap) :: TrigTable
        REAL, ALLOCATABLE :: siny(:)    ! Array of precomputed sinLy values for constant y  Latitudinal Hovmoller
        REAL, ALLOCATABLE :: cosy(:)    ! Array of precomputed cosLy values for constant y
        REAL, ALLOCATABLE :: cosx(:)    ! Array of precomputed cosknxy values for constant x    Longitudinal Hovmoller
        REAL, ALLOCATABLE :: sinx(:)    ! Array of precomputed sinknxy values for constant x
    end type TrigTable

    type, extends(BaseMap) :: WeatherMap    ! Contains all the data not dependent upon input for creating 2d weather maps.
    end type WeatherMap

    type, extends(TrigTable) :: Hovmoller
        integer :: row = 0          ! row on the screen to draw the hovmoller. I think this can be used for both lat and lon
    end type Hovmoller

    type, extends(TrigTable) :: VertCrossSection
        real, allocatable :: tempScales(:)   ! The scaling factors needed to make temperature dimensional which depends upon level.
    end type VertCrossSection
       
    type :: WeatherStation
        LOGICAL :: isInitialized = .FALSE.  ! Indicates that this structure has been initialized and is ready to use when true
        integer(4) :: color         ! Color to use in GDC calls. This is the RGB color passed by the caller.
        integer(4) :: gDCcolor      ! Internal reversed byte color for DIB section plotting of points.
        integer :: yCenter          ! Location of the origin in the y-axis. The x-axis will be time in this plot.
                                    ! The y-axis will be the value of the variable being plotted scaled by the range and 
                                    ! multiplied by the height of the screen to fit in the screen. The range is from -R to R
        real, pointer :: value(:)   ! Points to the variable to be plotted at a certain level
        type(TimeSeriesBuffer) :: timeSeriesBuf ! Buffer to store the time series values to be plotted. This is a circular
                                    ! buffer that stores the most recent values up to the size of the screen width in pixels.
        real :: scale               ! The scaling factor needed to make the quantity dimensional.
        real :: range               ! The range of the graph in y. Since the origin is in the middle, the range will be from -range to +range.
                                    ! This is used to scale the plot to fit in the screen.
        character(:), allocatable :: units  ! Units of the variable being plotted to be displayed in the graph legend
        REAL, ALLOCATABLE :: siny(:)    ! Array of precomputed sinLy values for constant y
        REAL, ALLOCATABLE :: cosy(:)    ! Array of precomputed cosLy values for constant y
        REAL, ALLOCATABLE :: cosx(:)    ! Array of precomputed cosKnxy values for constant x
        REAL, ALLOCATABLE :: sinx(:)    ! Array of precomputed sinKnxy values for constant x
        integer :: field = PLOTTING_OFF ! The field being plotted. Field ranges from 1 (VORTICITY) to 16 (OROGRAPHY) and PLOTTING_OFF (17)
    end type WeatherStation

    type :: ElapsedTime
        integer(8) :: timeSteps = 0_8
        real(wp) :: dimensionalTime
        real(wp) :: days
        integer :: years
    end type ElapsedTime
    
    type(ElapsedTime) gElapsedTime

    !type(PlotConfig) :: gPlotConfig
    type(PhaseSpace) :: gPhaseSpace1
    type(PhaseSpace) :: gPhaseSpace2
    type(WeatherMap) :: gWeatherMap1
    type(WeatherMap) :: gWeatherMap2
    type(Hovmoller) :: gLatHovmoller1
    type(Hovmoller) :: gLatHovmoller2
    type(VertCrossSection) :: gLatVertCrossSec1
    type(VertCrossSection) :: gLatVertCrossSec2
    type(Hovmoller) :: gLonHovmoller1
    type(Hovmoller) :: gLonHovmoller2
    type(WeatherStation) :: gWeatherStation1
    type(WeatherStation) :: gWeatherStation2

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

    INTERFACE
        MODULE SUBROUTINE DrawGridLines(invalidate)
            LOGICAL, intent(in) :: invalidate
        END SUBROUTINE DrawGridLines
    END INTERFACE
    
    INTERFACE
        MODULE SUBROUTINE DrawVerticalLines(invalidate)
            LOGICAL, intent(in) :: invalidate
        END SUBROUTINE DrawVerticalLines
    END INTERFACE

    INTERFACE
        MODULE SUBROUTINE DrawHorizontalLines(invalidate, labelsOnLeft, color, weatherStn)
            LOGICAL, intent(in) :: invalidate
            LOGICAL, intent(in) :: labelsOnLeft
            INTEGER(4), intent(in) :: color
            type(WeatherStation), optional, intent(in) :: weatherStn
        END SUBROUTINE DrawHorizontalLines
    END INTERFACE
    
    ! Supports the Sleep() subroutine
    interface
        subroutine Sleep(ms) bind(C, name='Sleep')
            use, intrinsic :: iso_c_binding
            integer(c_int), value :: ms
        end subroutine Sleep
    end interface

    contains

!=========================================== INITIALIZE PLOT
        ! Sets up the various plot structures to initialize fields that only need to be set once
        ! during a model run. The user might be able to change some parameters while the model is
        ! running that could require that this routine be called again.
        subroutine initializePlot()
            use ConstantsModule, only: PHASE_SPACE, WEATHER_STN, HOV_DIAG_AT_LAT, HOV_DIAG_AT_LONG, PHASE_AND_MAP, WEATHER_MAP, &
                                       VERT_CROSS_SEC_AT_LAT, VERT_CROSS_SEC_AT_LONG, SQRT2, PLOTTING_OFF
            use ModelParamsModule, only: gModIntParams, gPlotScales, gIsTemperature
            IMPLICIT NONE
            
            integer(4) :: leadColor, color

! ------ PHASE SPACE -----------------------------
            if (gModIntParams%plotType == PHASE_SPACE .OR. gModIntParams%plotType == PHASE_AND_MAP) then
                gPhaseSpace1%field = gModIntParams%field1
                if (gModIntParams%field1 < PLOTTING_OFF) then
                    if (gIsTemperature(gModIntParams%field1)) then
                        leadColor = PHASE_SPACE_TEMP_LEAD_COLOR
                        color = PHASE_SPACE_TEMP_COLOR
                    else
                        leadColor = PHASE_SPACE_HEIGHT_LEAD_COLOR
                        color = PHASE_SPACE_HEIGHT_COLOR
                    endif
                    call initializePhaseSpaceType(gModIntParams%field1, gPhaseSpace1, leadColor, color)
                endif
                gPhaseSpace2%field = gModIntParams%field2
                if (gModIntParams%field2 < PLOTTING_OFF) then
                    ! Make sure the two colors are different in case two height-related or temperature-related fields are plotted
                    ! Otherwise we will plot a height related field with blue and temperature orange.
                    if (leadColor == PHASE_SPACE_TEMP_LEAD_COLOR) then
                        leadColor = PHASE_SPACE_HEIGHT_LEAD_COLOR
                        color = PHASE_SPACE_HEIGHT_COLOR
                    else
                        leadColor = PHASE_SPACE_TEMP_LEAD_COLOR
                        color = PHASE_SPACE_TEMP_COLOR
                    endif
                    call initializePhaseSpaceType(gModIntParams%field2, gPhaseSpace2, leadColor, color)
                endif
 
! ------ WEATHER STN -----------------------------
            else if (gModIntParams%plotType == WEATHER_STN) then
                gWeatherStation1%field = gModIntParams%field1
                if (gModIntParams%field1 < PLOTTING_OFF) then
                    if (gIsTemperature(gModIntParams%field1)) then
                        color = PHASE_SPACE_TEMP_COLOR
                    else
                        color = PHASE_SPACE_HEIGHT_COLOR
                    endif
                    call initializeWeatherStationType(gModIntParams%field1, gWeatherStation1, color)
                endif
                gWeatherStation2%field = gModIntParams%field2
                if (gModIntParams%field2 < PLOTTING_OFF) then
                    if (color == PHASE_SPACE_TEMP_COLOR) then
                        color = PHASE_SPACE_HEIGHT_COLOR
                    else
                        color = PHASE_SPACE_TEMP_COLOR
                    endif
                    call initializeWeatherStationType(gModIntParams%field2, gWeatherStation2, color)
                endif
                
! ------ VERTICAL CROSS SECTION AT LATITUDE -----------------------------
            else if (gModIntParams%plotType == VERT_CROSS_SEC_AT_LAT)  then
                gLatVertCrossSec1%field = gModIntParams%field1
                if (gModIntParams%field1 < PLOTTING_OFF) then
                    call initializeVerticalCrossSectionType(gModIntParams%field1, gLatVertCrossSec1, .true., .true.)
                endif
                gLatVertCrossSec2%field = gModIntParams%field2
                if (gModIntParams%field2 < PLOTTING_OFF) then
                    call initializeVerticalCrossSectionType(gModIntParams%field2, gLatVertCrossSec2, gModIntParams%field1 == PLOTTING_OFF, .true.)
                endif


! ------ HOVMOLLER LATITUDE -----------------------------
            else if (gModIntParams%plotType == HOV_DIAG_AT_LAT) then
                gLatHovmoller1%field = gModIntParams%field1
                if (gModIntParams%field1 < PLOTTING_OFF) then
                    call initializeHovmollerType(gModIntParams%field1, gLatHovmoller1, .true., .true.)
                endif
                gLatHovmoller2%field = gModIntParams%field2
                if (gModIntParams%field2 < PLOTTING_OFF) then
                    call initializeHovmollerType(gModIntParams%field2, gLatHovmoller2, gModIntParams%field1 == PLOTTING_OFF, .true.)
                endif

! ------ HOVMOLLER LONGITUDE -----------------------------
            else if (gModIntParams%plotType == HOV_DIAG_AT_LONG)then
                gLonHovmoller1%field = gModIntParams%field1
                if (gModIntParams%field1 < PLOTTING_OFF) then
                    call initializeHovmollerType(gModIntParams%field1, gLonHovmoller1, .true., .false.)
                endif
                gLonHovmoller2%field = gModIntParams%field2
                if (gModIntParams%field2 < PLOTTING_OFF) then
                    call initializeHovmollerType(gModIntParams%field2, gLonHovmoller2, gModIntParams%field1 == PLOTTING_OFF, .false.)
                endif
            endif 

! ------ WEATHER MAP & COMBO -----------------------------
            if (gModIntParams%plotType == WEATHER_MAP .OR. gModIntParams%plotType == PHASE_AND_MAP) then
                gWeatherMap1%field = gModIntParams%field1
                if (gModIntParams%field1 < PLOTTING_OFF) then
                    call initializeWeatherMapType(gModIntParams%field1, gWeatherMap1, .true.)
                endif
                gWeatherMap2%field = gModIntParams%field2
                if (gModIntParams%field2 < PLOTTING_OFF) then
                    call initializeWeatherMapType(gModIntParams%field2, gWeatherMap2, gModIntParams%field1 == PLOTTING_OFF)
                endif
            endif
        
        end subroutine initializePlot
        
        !! Initializes all the parameters for a phase space plot that do not depend upon the newly generated values
        !! from the integration of the model. It speeds up the calculation and makes the routine much simpler.
        !> @param field: This is the quantity being plotted, such as HEIGHT, TEMPERATURE, etc.
        !> @param phaseSpaceType: This is the PhaseSpace type that is being populated.
        !> @param colorLead: This is the color in ARGB of the current pixel being plotted. Since these pixels are
        !>                   drawn to a DIB section, we have to reverse the RGB value to little endian byte order
        !>                   which has the alpha, red, green, blue order in the integer(4) value
        !> @param color: This is the color in ARGB of the previous pixel that was plotted with color leadColor being
        !>               plotted. So it overwrites that pixel with this color. Since these pixels are drawn to a DIB
        !>               section, we have to reverse the RGB value to little endian byte order which has the alpha, 
        !>               red, green, blue order in the integer(4) value.
        !>               This value is also placed in the %gDCcolor for use in GDC win32 APIs needed in the plot, such
        !>               as the legend.
        subroutine initializePhaseSpaceType(field, phaseSpaceType, colorLead, color)
            use NLevQGModelGlobals, only: gBitMapInfo
            use ConstantsModule, only: SQRT2, PHASE_AND_MAP
            use ModelParamsModule, only: gModIntParams, gPlotScales, gContourIntervals, gIsTemperature
            use ModelFieldsModule, only: gVertArr

            IMPLICIT NONE

            integer, intent(in) :: field
            type(PhaseSpace), intent(inout) :: phaseSpaceType
            integer(4), intent(in) :: colorLead, color
            
            integer :: factor
            
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
                !phaseSpaceType%xScale = 2.0 * gContourIntervals%Contours(field)
                ! The spectral eigenfunction is 2 * (cosx, sinx)siny
                if (gIsTemperature(field)) then
                    phaseSpaceType%xScale = 2.0 * gPlotScales(field) / interpolateTemperatureScale(gModIntParams%LEVP)
                else
                    phaseSpaceType%xScale = 2.0 * gPlotScales(field)
                endif
            else
                ! Otherwise we just have the zonal mode cosLy if the user sets the zonal wavenumber to 0
                phaseSpaceType%xIndex = gModIntParams%meridWavenumber
                if (gIsTemperature(field)) then
                    phaseSpaceType%xScale = SQRT2 * gPlotScales(field) / interpolateTemperatureScale(gModIntParams%LEVP)
                else
                    phaseSpaceType%xScale = SQRT2 * gPlotScales(field)
                endif
            endif
            ! If the user specifies a zonal wavenumber for the sine part greater than zero
            if (gModIntParams%sinZonal .GT. 0) then
                ! This gets the index for cosknx sinly component
                phaseSpaceType%yIndex = gModIntParams%LMAX2 + 1 + & ! Puts us at the start of the waves
                                        2 * gModIntParams%LMAX2 * (gModIntParams%sinZonal - 1) + & ! Puts us at the start of the Kth zonal wave
                                        2 * (gModIntParams%sinMerid - 1) + 1
                if (gIsTemperature(field)) then
                    phaseSpaceType%yScale = 2.0 * gPlotScales(field) / interpolateTemperatureScale(gModIntParams%LEVP)
                else
                    phaseSpaceType%yScale = 2.0 * gPlotScales(field)
                endif
            else
                ! Otherwise we just have the zonal mode cosLy if the user sets the zonal wavenumber to 0
                phaseSpaceType%yIndex = gModIntParams%meridWavenumber
                if (gIsTemperature(field)) then
                    phaseSpaceType%yScale = SQRT2 * gPlotScales(field) / interpolateTemperatureScale(gModIntParams%LEVP)
                else
                    phaseSpaceType%yScale = SQRT2 * gPlotScales(field)
                endif
            endif
            factor = 2
            if (gModIntParams%plotType == PHASE_AND_MAP) factor = 4
            ! Now take into account the screen size and the range that it is supposed to represent.
            phaseSpaceType%xScale = phaseSpaceType%xScale * REAL(min(gBitMapInfo%height, gBitMapInfo%width)) / (REAL(factor) * gContourIntervals%Ranges(field))
            phaseSpaceType%yScale = phaseSpaceType%yScale * REAL(min(gBitMapInfo%height, gBitMapInfo%width)) / (REAL(factor) * gContourIntervals%Ranges(field))
            
            phaseSpaceType%xcenter = gBitMapInfo%width / factor
            phaseSpaceType%ycenter = gBitMapInfo%height / factor
            
            if (gModIntParams%plotType == PHASE_AND_MAP) then
                phaseSpaceType%xcenter = 3 * gBitMapInfo%width / factor
                phaseSpaceType%ycenter = 3 * gBitMapInfo%height / factor
            endif

            ! No need to call these anymore - They have to be called every time step because of interpolation
            ! Now get the variable to be plotted along the x-axis. That will be gModIntParams%field1. 
            !call loadFields( phaseSpaceType%x, field, gModIntParams%LEVP)
                
            ! Now get the variable to be plotted along the y-axis. That will be gModIntParams%field1. 
            !call loadFields( phaseSpaceType%y, field, gModIntParams%LEVP)

            phaseSpaceType%colorLead = IOR(ISHFT(IAND(colorLead, Z'000000FF'), 16), & ! R: bit 0 -> 16
                        IOR(IAND(colorLead, Z'0000FF00'), &            ! G: bit 8 -> 8 (stays)
                        ISHFT(IAND(colorLead, Z'00FF0000'), -16))) ! B: bit 16 -> 0
            phaseSpaceType%color = IOR(ISHFT(IAND(color, Z'000000FF'), 16), & ! R: bit 0 -> 16
                        IOR(IAND(color, Z'0000FF00'), &            ! G: bit 8 -> 8 (stays)
                        ISHFT(IAND(color, Z'00FF0000'), -16))) ! B: bit 16 -> 0
            phaseSpaceType%gDCcolor = color
            phaseSpaceType%isInitialized = .TRUE.

        end subroutine initializePhaseSpaceType

        !! Initializes the base fields common to the Map type plots (maps, homollers, and vertical cross sections)
        subroutine initializeBaseMapType(field, baseMapType, useSolidColor)
            use ModelParamsModule, only: gModIntParams, gPlotScales, gContourIntervals, gIsTemperature
            use ModelFieldsModule, only: gVertArr

            IMPLICIT NONE

            integer, intent(in) :: field
            class(BaseMap), intent(inout) :: baseMapType
            logical, intent(in) :: useSolidColor

            baseMapType%field = field
            baseMapType%useSolidColor = useSolidColor
            ! Scale factor
            if (gIsTemperature(field)) then
                baseMapType%scale = gPlotScales(field) / interpolateTemperatureScale(gModIntParams%LEVP)
            else
                baseMapType%scale = gPlotScales(field)
            endif
            ! We divide by the contour interval here to save a division
            baseMapType%contourIntervalInv = 1.0 / REAL(gContourIntervals%Contours(field))
        end subroutine initializeBaseMapType
        
        !! Initializes the trig tables needed for either plots along a latitude or longitude.
        subroutine initializeTrigTablesType(latOrLon, trigTableType)
            use ConstantsModule, only: SQRT2
            use ModelParamsModule, only: gModIntParams, gModRealParams

            IMPLICIT NONE
            
            logical, intent(in) :: latOrLon
            class(TrigTable), intent(inout) :: trigTableType
            
            integer :: L, K

            if (latOrLon) then       ! Precompute the sinLy and cosLy for constant y
                ! The latitude is given by gModRealParams%hovLat. The number of L waves is given by gModIntParams%LMAX2
                if (ALLOCATED(trigTableType%siny)) deallocate(trigTableType%siny)
                allocate(trigTableType%siny(gModIntParams%LMAX2))
                if (ALLOCATED(trigTableType%cosy)) deallocate(trigTableType%cosy)
                allocate(trigTableType%cosy(gModIntParams%LMAX2))
                ! gModRealParams%hovLat is in radians (in the UI in dimensional form it is in degrees). Here it will always be radians
                ! However, the user treats the channel as going from 0 to 90, but the actual range is 0 to 180 such that siny is 0 at the
                ! equatorial boundary and 0 at the polar boundary and max in the middle. Thus we need to multiply gModRealParams%hovLat
                ! by 2.
                do L = 1, gModIntParams%LMAX2
                    trigTableType%siny(L) = 2. * SIN(real(2 * L) * gModRealParams%hovLat)
                    trigTableType%cosy(L) = SQRT2 * COS(real(2 * L) * gModRealParams%hovLat)
                end do
            else
                ! The longitude is given by gModRealParams%hovLon. The number of K waves is given by gModIntParams%KMAX2
                if (ALLOCATED(trigTableType%cosx)) deallocate(trigTableType%cosx)
                allocate(trigTableType%cosx(gModIntParams%KMAX2))
                if (ALLOCATED(trigTableType%sinx)) deallocate(trigTableType%sinx)
                allocate(trigTableType%sinx(gModIntParams%KMAX2))
                ! gModRealParams%hovLat is in radians (in the UI in dimensional form it is in degrees). Here it will always be radians
                ! In this case the degrees entered is from 0 to 360 which is consistent with the actual eigenfunctions. No factor of
                ! 2 in this case.
                do K = 1, gModIntParams%KMAX2
                    trigTableType%sinx(K) = SIN(real(K) * gModRealParams%hovLon)
                    trigTableType%cosx(K) = COS(real(K) * gModRealParams%hovLon)
                end do
            endif
        end subroutine initializeTrigTablesType

        !! Initializes all the parameters for a weather map plot that do not depend upon the newly generated values
        !! from the integration of the model. It speeds up the calculation and makes the routine much simpler.
        !> @param field: This is the quantity being plotted, such as HEIGHT, TEMPERATURE, etc.
        !> @param weatherMapType: This is the WeatherMap type that is being populated.
        !> @param solidColor: When true the map is plotted using solid color and not line contours. If more than one
        !>                    field is being plotted, only the first field can be solid color
        subroutine initializeWeatherMapType(field, weatherMapType, useSolidColor)

            IMPLICIT NONE

            integer, intent(in) :: field
            type(WeatherMap), intent(inout) :: weatherMapType
            logical, intent(in) :: useSolidColor

            call initializeBaseMapType(field, weatherMapType, useSolidColor)
            weatherMapType%isInitialized = .true.

        end subroutine initializeWeatherMapType
    
        !> @param vertCrossSecType: This is the VerticalCrossSection type that is being populated.
        !> @param solidColor: When true the map is plotted using solid color and not line contours. If more than one
        !>                    field is being plotted, only the first field can be solid color
        !> @param latOrLon: if true the latitude is the fixed value, otherwise its the longitude. This will tell which of
        !>                  the trig functions to pre-compute
        subroutine initializeVerticalCrossSectionType(field, vertCrossSecType, useSolidColor, latOrLon)
            use ModelParamsModule, only: gPlotScales,  gIsTemperature, gModIntParams
            use ModelFieldsModule, only: gVertArr

            IMPLICIT NONE

            integer, intent(in) :: field
            type(VertCrossSection), intent(inout) :: vertCrossSecType
            logical, intent(in) :: useSolidColor
            logical, intent(in) :: latOrLon
            
            integer :: totalLevels, plotLevel

            
            call initializeBaseMapType(field, vertCrossSecType, useSolidColor)
            call initializeTrigTablesType(latOrLon, vertCrossSecType)
            
            ! Create the temperature scale factors for the different levels if a temperature field
            ! The scaling factor gVertArr%A is a function of level.
            totalLevels = SIZE(gVertArr%LVS)
            if (ALLOCATED(vertCrossSecType%tempScales)) deallocate(vertCrossSecType%tempScales)
            allocate(vertCrossSecType%tempScales(totalLevels))
            if (gIsTemperature(field)) then
                vertCrossSecType%scale = gPlotScales(field) ! This is needed to be reset since the initializeBaseMapType will already
                                                            ! have normalized by A(LEVP) which in the cross sections will cause a double normalization.
                do plotLevel = 1, totalLevels
                    vertCrossSecType%tempScales(plotLevel) = interpolateTemperatureScale(plotLevel)
                end do
            else
                vertCrossSecType%tempScales = 1.0
            end if
            
            vertCrossSecType%isInitialized = .true.
        end subroutine initializeVerticalCrossSectionType
        
        function interpolateTemperatureScale(plotLevel) result(tempScale)
            use ModelFieldsModule, only: gVertArr
            use ModelParamsModule, only: gModIntParams
            
            implicit none
            
            integer, intent(in) :: plotLevel
            
            real :: tempScale, weight
            LOGICAL :: isOdd
            integer :: level, levelt, totalLevels
            
            totalLevels = SIZE(gVertArr%LVS)
            
            isOdd = BTEST(plotLevel, 0) ! if Bit 0 is 1, the function returns true and the number is odd. This means we are in the middle of a layer 
            levelt = min(1 + (plotLevel - 2) / 2, gModIntParams%NLEV - 1)
            level = min(1 + (plotLevel - 1) / 2, gModIntParams%NLEV)
            if (isOdd .OR. (plotLevel .LE. 3) .OR. (plotLevel .GE. totalLevels - 2)) then
                tempScale = gVertArr%A(levelt) ! At the interface level. Use the actual temperature and do not interpolate
            else
                weight = (gVertArr%PP(level) - gVertArr%PV(level)) / (gVertArr%PP(level)- gVertArr%PP(level + 1))
                tempScale =  (1.0 - weight) * gVertArr%A(levelt - 1) + weight * gVertArr%A(levelt)
            end if

        end function interpolateTemperatureScale

        !! Initializes all the parameters for a hovmoller plot that do not depend upon the newly generated values
        !! from the integration of the model. It speeds up the calculation and makes the routine much simpler.
        !! Also good for the vertical cross sections because both have the same horizontal transforms
        !> @param field: This is the quantity being plotted, such as HEIGHT, TEMPERATURE, etc.
        !> @param hovmollerType: This is the HOVMOLLER type that is being populated.
        !> @param solidColor: When true the map is plotted using solid color and not line contours. If more than one
        !>                    field is being plotted, only the first field can be solid color
        !> @param latOrLon: if true the latitude is the fixed value, otherwise its the longitude. This will tell which of
        !>                  the trig functions to pre-compute
        subroutine initializeHovmollerType(field, hovmollerType, useSolidColor, latOrLon)

            IMPLICIT NONE

            integer, intent(in) :: field
            type(Hovmoller), intent(inout) :: hovmollerType
            logical, intent(in) :: useSolidColor
            logical, intent(in) :: latOrLon

            call initializeBaseMapType(field, hovmollerType, useSolidColor)
            call initializeTrigTablesType(latOrLon, hovmollerType)
            hovmollerType%row = 0
            hovmollerType%isInitialized = .true.
        end subroutine initializeHovmollerType

        subroutine initializeWeatherStationType(field, weatherStnType, color)
            USE IFWINTY

            use NLevQGModelGlobals, only: gBitMapInfo
            use ConstantsModule, only: SQRT2
            use ModelParamsModule, only: gModIntParams, gModRealParams, gPlotScales, gContourIntervals, gIsTemperature
            use ModelFieldsModule, only: gVertArr

            implicit none
            
            integer, intent(in) :: field
            type(WeatherStation), intent(inout) :: weatherStnType
            integer(4), intent(in) :: color
            
            integer :: I, K, L

            weatherStnType%field = field
            weatherStnType%yCenter = gBitMapInfo%height / 2

            ! Initialization & Re-allocation (Check if width changed)
            IF (.NOT. weatherStnType%timeSeriesBuf%is_initialized .OR. size(weatherStnType%timeSeriesBuf%data_buffer) .NE. gBitMapInfo%width) THEN
                ! This auto allocates/reallocates to the correct size AND initializes to zero
                weatherStnType%timeSeriesBuf%data_buffer = spread(0.0, 1, gBitMapInfo%width)
                                            ! implied do loop for auto allocation here since plot_pts is a derived type T_POINT
                weatherStnType%timeSeriesBuf%plot_pts    = [(T_POINT(0, 0), I = 1, gBitMapInfo%width)]
                weatherStnType%timeSeriesBuf%head_idx = 0
                weatherStnType%timeSeriesBuf%count = 0
                weatherStnType%timeSeriesBuf%is_initialized = .TRUE.
            END IF

            ! color for bit map point and GDC lines
            weatherStnType%color = IOR(ISHFT(IAND(color, Z'000000FF'), 16), & ! R: bit 0 -> 16
                        IOR(IAND(color, Z'0000FF00'), &            ! G: bit 8 -> 8 (stays)
                        ISHFT(IAND(color, Z'00FF0000'), -16))) ! B: bit 16 -> 0
            weatherStnType%gDCcolor = color

            ! Scale factor
            if (gIsTemperature(field)) then
                weatherStnType%scale = gPlotScales(field) / interpolateTemperatureScale(gModIntParams%LEVP)
            else
                weatherStnType%scale = gPlotScales(field)
            endif
            weatherStnType%range = gContourIntervals%Ranges(field)
            weatherStnType%scale = weatherStnType%scale * REAL(gBitMapInfo%height) / (2.0 * gContourIntervals%Ranges(field))

            ! Pre computed arrays
            if (ALLOCATED(weatherStnType%cosx)) deallocate(weatherStnType%cosx)
            allocate(weatherStnType%cosx(gModIntParams%KMAX2))
            if (ALLOCATED(weatherStnType%sinx)) deallocate(weatherStnType%sinx)
            allocate(weatherStnType%sinx(gModIntParams%KMAX2))
            if (ALLOCATED(weatherStnType%siny)) deallocate(weatherStnType%siny)
            allocate(weatherStnType%siny(gModIntParams%LMAX2))
            if (ALLOCATED(weatherStnType%cosy)) deallocate(weatherStnType%cosy)
            allocate(weatherStnType%cosy(gModIntParams%LMAX2))
            ! The location of the weather station is given by gModRealParams%lonPoint and gModRealParams%latPoint.
            ! This 'weather station' will be at the level gModIntParams%LEVP
            do L = 1, gModIntParams%LMAX2
                weatherStnType%siny(L) = 2.0 * SIN(real(2 * L) * gModRealParams%latPoint)     ! Put in the factor of 2 here for 2(cosnknx, sinknx)sinLy
                                                                                              ! The 2*L is because the user enters 0 to 90 and it really is 0 to 180
                weatherStnType%cosy(L) = SQRT2 * COS(real(2 * L) * gModRealParams%latPoint)   ! Put in the SQRT2 factor here for cosLy
            end do
            do K = 1, gModIntParams%KMAX2
                weatherStnType%sinx(K) = SIN(real(K) * gModRealParams%lonPoint)
                weatherStnType%cosx(K) = COS(real(K) * gModRealParams%lonPoint)
            end do
            ! No need to call these anymore - They have to be called every time step because of interpolation
            !call loadFields(weatherStnType%value, field, gModIntParams%LEVP)
            weatherStnType%units = getUnits(field)
            weatherStnType%isInitialized = .true.

        end subroutine initializeWeatherStationType

!=========================================== OUTPUT PLOT
        !> This method determines what to plot depending upon the setting of gModIntParams%plotType
        !> and gDataType%dataSource (where the data to be plotted is coming from the model, a file
        !> or a set of ensemble files
        subroutine outputPlot()
            use ConstantsModule, only: WEATHER_MAP, PHASE_AND_MAP, VERT_CROSS_SEC_AT_LAT, HOV_DIAG_AT_LAT, VERT_CROSS_SEC_AT_LONG, &
                HOV_DIAG_AT_LONG, PHASE_SPACE, WEATHER_STN, NO_PLOT, PLOTTING_OFF, SECONDS_PER_DAY, wp
            use ModelParamsModule, only: gModIntParams, gModRealParams, gScaleFactors

            IMPLICIT NONE

            ! Allows the user to interact with the program.
            CALL PumpMessages()

    ! NO PLOT SKIP -------------------------
            if (gModIntParams%plotType == NO_PLOT .AND. mod(gElapsedTime%timeSteps, 720) .NE. 0) then
                gElapsedTime%timeSteps = gElapsedTime%timeSteps + gModIntParams%outputInterval
                return
            endif

            ! timeScale * gModRealParams%delt gives the length of each time step in seconds. We divide by secondsPerDay to give days
            ! 'Days' may generate too big of a number - go to years and days
            gElapsedTime%days = REAL(gElapsedTime%timeSteps) * gElapsedTime%dimensionalTime
            gElapsedTime%years = Floor(gElapsedTime%days / 365.0_wp)
            gElapsedTime%days = gElapsedTime%days - gElapsedTime%years * 365
            ! Use an internal write to format the string

    ! NO PLOT No SKIP on 720 time steps -------------------------
            if (gModIntParams%plotType == NO_PLOT) then
                call labelPlotsAndDisplayTime(.true.)
                gElapsedTime%timeSteps = gElapsedTime%timeSteps + gModIntParams%outputInterval
                return
            endif

    ! PHASE SPACE ----------------------
            if (gModIntParams%plotType == PHASE_SPACE) then
                call labelPlotsAndDisplayTime(.true.)
                if(gModIntParams%field1 .NE. PLOTTING_OFF) call plotPhaseSpace(gPhaseSpace1, gModIntParams%field2 == PLOTTING_OFF)
                if(gModIntParams%field2 .NE. PLOTTING_OFF) call plotPhaseSpace(gPhaseSpace2, .TRUE.)
            end if

    ! WEATHER STATION ----------------------
            if (gModIntParams%plotType == WEATHER_STN) then
                call plotTimeSeries()
            endif

    ! HOVMOLLER LAT ----------------------
            if (gModIntParams%plotType == HOV_DIAG_AT_LAT) then
                if(gModIntParams%field1 .NE. PLOTTING_OFF) call plotLatHovmoller(gLatHovmoller1, gModIntParams%field2 == PLOTTING_OFF)
                if(gModIntParams%field2 .NE. PLOTTING_OFF) call plotLatHovmoller(gLatHovmoller2, .TRUE.)
            endif
    
    ! HOVMOLLER LON ----------------------
            if (gModIntParams%plotType == HOV_DIAG_AT_LONG) then
                if(gModIntParams%field1 .NE. PLOTTING_OFF) call plotLonHovmoller(gLonHovmoller1, gModIntParams%field2 == PLOTTING_OFF)
                if(gModIntParams%field2 .NE. PLOTTING_OFF) call plotLonHovmoller(gLonHovmoller2, .TRUE.)
            endif
            
    ! HOVMOLLER LAT CROSS SECTION----------------------
            if (gModIntParams%plotType == VERT_CROSS_SEC_AT_LAT) then
                if(gModIntParams%field1 .NE. PLOTTING_OFF) call plotLatVertCrossSec(gLatVertCrossSec1, gModIntParams%field2 == PLOTTING_OFF)
                if(gModIntParams%field2 .NE. PLOTTING_OFF) call plotLatVertCrossSec(gLatVertCrossSec2, .TRUE.)
            endif

    ! HOVMOLLER LON CROSS SECTION----------------------
            if (gModIntParams%plotType == VERT_CROSS_SEC_AT_LONG) then
                if(gModIntParams%field1 .NE. PLOTTING_OFF) call plotLonVertCrossSec(gLonHovmoller1, gModIntParams%field2 == PLOTTING_OFF)
                if(gModIntParams%field2 .NE. PLOTTING_OFF) call plotLonVertCrossSec(gLonHovmoller2, .TRUE.)
            endif

    ! WEATHER MAP ----------------------
            if (gModIntParams%plotType == WEATHER_MAP) then
                ! We do not force the screen to update if gModIntParams%field2 is a plot.
                ! In that case gModIntParams%field1 gets plotted first and then
                ! a second call to plotWeatherMap for gModIntParams%field2 is made.
                
                ! If gModIntParams%field1 OR gModIntParams%field2 is PLOTTING_OFF, plotWeatherMap
                ! simply returns and does nothing
                call plotWeatherMap((gModIntParams%field2 == PLOTTING_OFF), gWeatherMap1)
                ! This will always plot unless gModIntParams%field2 = PLOTTING_OFF.
                call plotWeatherMap(.true., gWeatherMap2)
            endif
    ! WEATHER MAP and PHASE SPACE ----------------------
            if (gModIntParams%plotType == PHASE_AND_MAP) then
                call plotWeatherMap((gModIntParams%field2 == PLOTTING_OFF), gWeatherMap1)
                call plotWeatherMap(.true., gWeatherMap2)
                if(gModIntParams%field1 .NE. PLOTTING_OFF) call plotPhaseSpace(gPhaseSpace1, gModIntParams%field2 == PLOTTING_OFF)
                if(gModIntParams%field2 .NE. PLOTTING_OFF) call plotPhaseSpace(gPhaseSpace2, .TRUE.)
            endif

            gElapsedTime%timeSteps = gElapsedTime%timeSteps + gModIntParams%outputInterval

        end subroutine outputPlot

        !! Helper funtion to get the correct weathermap type structure. This is
        !! for mousing over the map during an F1 pause operation.
        function getWeatherMapType(field) result(wxMapType)
            implicit none
            
            integer, intent(in) :: field
            
            type(WeatherMap) :: wxMapType

            if (gWeatherMap1%field == field) wxMapType = gWeatherMap1
            if (gWeatherMap2%field == field) wxMapType = gWeatherMap2
        end function getWeatherMapType
        
        function getLatCrossSectionType(field) result(xSecType)
            implicit none
            
            integer, intent(in) :: field
            
            type(VertCrossSection) :: xSecType

            if (gLatVertCrossSec1%field == field) xSecType = gLatVertCrossSec1
            if (gLatVertCrossSec2%field == field) xSecType = gLatVertCrossSec2
        end function getLatCrossSectionType

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
        subroutine createGrid(grid, weatherMapType)
            use ConstantsModule, only: PLOTTING_OFF, SQRT22
            use ArrayDimensionsModule, only: gCurrentDims
            use ModelParamsModule, only: gModIntParams
            use ModelFieldsModule, only: gTransArr
            use Fftw3HelpersModule, only: norm_fwd, norm_cosly, norm_inv_wave, plan_x_c2r_1d, plan_c2r_2d
            use, intrinsic :: ieee_arithmetic
        
            implicit none
            
            include 'fftw3.f03'

            real, intent(inout) :: grid(:, :)
            type(WeatherMap), intent(in) :: weatherMapType

            integer :: index, K, L, IXMAX, IYMAX, KMAX2, LMAX2
            REAL :: temp_calc, wave_cos, wave_sin, zonal_val

            if (weatherMapType%field == PLOTTING_OFF) then
                return
            endif
            
            IXMAX = gCurrentDims%IXMAX
            IYMAX = gCurrentDims%IYMAX
            KMAX2 = gModIntParams%KMAX2
            LMAX2 = gModIntParams%LMAX2
            
            gTransArr%plot = 0.0        !FFTw3 forces us to define two whole arrays JUST for plotting an extra row
            grid = 0.0                  ! Fortran short cut to zero an entire array in all dimensions. This is the output array for the grid
            gTransArr%Gc(:, :) = CMPLX(0.0_C_FLOAT, 0.0_C_FLOAT, kind=C_FLOAT)

            ! 1. Load ac(k,l)cosknxsinly and as(k,l)sinknxsinly into the imaginary and real parts of Gc for each L
            ! In two dimensions the spectral coeffcients A(k,l) would be
            ! A(k,l) = (ac(k,l)*2*cosknx + as(k,l)*2*sinknx)sinly + sqrt(2)*az(l)*cosly
            ! However, our arrays for the horizonal field are compacted into one dimension such that
            ! the first LMAX2 components are the zonal az(L) sqrt(2)cosLy part. 
            ! Then the remainder of the array is has cosx, sinx for all sinLy, then cos2x, sin2x for all sinLy, etc.
            temp_calc = SQRT22 * weatherMapType%scale ! Do the normalization first so we dont have to normalize after the transform
            
            ! ====================================================================
            ! PART A: LOAD THE ZONAL MODES (cosLy only)
            ! ====================================================================
            ! The first LMAX entries in your linear array are your pure zonal components.
            ! They occupy the K=0 frequency (Column 1 in Fortran's 1-based indexing).
            !DO L = 1, LMAX2
            !    ! Extract value from the linear array (1 to LMAX2 block) weatherMapType%value(L)
            !    ! Map to 2D complex array: K=0 implies column 1.
            !    ! Zonal-only modes are cosLy, so place entirely in the REAL part.
            !    ! Note: Add +1 to L if your physical grid index L=0 is reserved for mean flow
            !    gTransArr%Gc(1, L + 1) = CMPLX(temp_calc *weatherMapType%value(L), 0.0_C_FLOAT, kind=C_FLOAT_COMPLEX)
            !end do
            
            do L = 1, LMAX2
                zonal_val = temp_calc * weatherMapType%value(L)
    
                ! Positive frequency (+L)
                gTransArr%Gc(1, L + 1)         = CMPLX(zonal_val, 0.0_C_FLOAT, kind=C_FLOAT_COMPLEX)
                ! Negative frequency (-L) matches exactly because cosine is symmetric: cos(-Ly) = cos(Ly)
                gTransArr%Gc(1, IYMAX - L + 1) = CMPLX(zonal_val, 0.0_C_FLOAT, kind=C_FLOAT_COMPLEX)
            end do

            !! ====================================================================
            ! PART B: LOAD THE WAVE MODES (cosKx and sinKx for sinLy)
            ! ====================================================================
            ! These follow the LMAX2 block. They cycle through all L for each K.
            ! Each unique (K, L) combination contains a pair of entries: [Cos, Sin]
            temp_calc = 0.5 * weatherMapType%scale ! Do the normalization first so we dont have to normalize after the transform
            
            DO K = 1, KMAX2
                DO L = 1, LMAX2
        
                    ! 1. Calculate the exact index in your linear storage vector
                    index = LMAX2 &                         ! Step past Zonal block
                          + (K - 1) * (LMAX2 * 2) &         ! Step past completed K channels
                          + (L - 1) * 2 &                   ! Step past completed L pairs in current K
                          + 1                               ! Pointer to the Cos entry
        
                    ! Extract the unscaled wave amplitudes
                    wave_cos = weatherMapType%value(index) * temp_calc      ! The cosknxsinly part
                    wave_sin = weatherMapType%value(index + 1) * temp_calc  ! The sinknxsinly part
        
                    ! 2. --- Interaction with positive Y-frequency (+L slot) ---
                    ! For cos(Kx)*sin(Ly) -> Sits in Imaginary part, with a negative sign
                    ! For sin(Kx)*sin(Ly) -> Sits in Real part, with a negative sign
                    gTransArr%Gc(K + 1, L + 1) = CMPLX(-wave_sin, -wave_cos, kind=C_FLOAT)
        
                    ! 3. --- Interaction with negative Y-frequency (-L slot) ---
                    ! Because sin(-Ly) = -sin(Ly), the signs reverse across the wrap-around boundary.
                    ! This forces the unwanted cross-terms to cancel out during the 2D transform!
                    gTransArr%Gc(K + 1, IYMAX - L + 1) = CMPLX(wave_sin, wave_cos, kind=C_FLOAT)
        
                END DO
            END DO
            
            ! ====================================================================
            ! PART C: EXECUTE THE 2D COMPLEX-TO-REAL TRANSFORM
            ! ====================================================================
            ! Gc (Input):  Complex array of size (IXMAX/2 + 1, IYMAX)
            ! plot (Output): Real array of size (IXMAX, IYMAX)

            call fftwf_execute_dft_c2r(plan_c2r_2d, gTransArr%Gc, gTransArr%plot)


            ! Copy the first column to the column just after the FFT data
            ! where the FFT data occupies indices 1 to IXMAX
            ! Do loop is more efficient when copying non-contiguous data like this than array slicing
            ! 1. Copy the core un-padded grid from FFTW into your extended plotting grid
            grid(1:IXMAX, 1:IYMAX) = gTransArr%plot(1:IXMAX, 1:IYMAX)

            ! 2. Instantly replicate the periodic boundary column in one line (No DO loop!)
            grid(IXMAX + 1, 1:IYMAX) = grid(1, 1:IYMAX)
            !call InverseTransform(weatherMapType) ! This creates the 1D array of the latitudinal profile at the weather station latitude for the hovmoller diagram
        end subroutine createGrid
        
        subroutine InverseTransform(weatherMapType)
            use ArrayDimensionsModule, only: gCurrentDims
            use ConstantsModule, only: SQRT2
            use ModelFieldsModule,     only: gTransArr
            use ModelParamsModule,     only: gModRealParams, gModIntParams
            use Fftw3HelpersModule,    only: plan_y_r2c_1d, plan_r2c_2d, norm_inv_wave
            use, intrinsic :: iso_c_binding, only : C_FLOAT_COMPLEX, C_FLOAT
            implicit none
            include 'fftw3.f03'
    
            type(WeatherMap), intent(in)  :: weatherMapType
    
            integer :: K, L, IXMAX, IYMAX, KMAX2, LMAX2, I, m_idx
            real(C_FLOAT) :: norm_factor_2d, wave_cos, wave_sin, zonal_val, FS(gCurrentDims%ITOT)
    
            IXMAX = gCurrentDims%IXMAX
            IYMAX = gCurrentDims%IYMAX
            KMAX2 = gModIntParams%KMAX2
            LMAX2 = gModIntParams%LMAX2
    
            ! Initialize output vector to zero
            FS = 0.0_C_FLOAT
            
            ! Compute the zonal average to ge the cos(Ly) components.
            I = IYMAX / 2
            ! =====================================================================
            ! 1. Handle the First Stationary Point (L = 1, y = 0)
            ! =====================================================================
            gTransArr%GZonal(1) = sum(gTransArr%plot(:, 1)) / REAL(IXMAX, C_FLOAT)
            gTransArr%plot(:, 1)  = gTransArr%plot(:, 1) - gTransArr%GZonal(1)
            ! No reflection needed for G4(:,1) or GZonal(1) because it maps to itself 
            ! across the periodic boundary.

            ! =====================================================================
            ! 2. Loop over the internal paired rows (From L = 2 up to I)
            ! =====================================================================
            do L = 2, I
                ! For IYMAX=8: 
                ! L=2 -> m_idx=8. Pairs: (2,8)
                ! L=3 -> m_idx=7. Pairs: (3,7)
                ! L=4 -> m_idx=6. Pairs: (4,6)
                m_idx = iymax - L + 2 

                ! Compute zonal average from the bottom half row
                gTransArr%GZonal(L) = sum(gTransArr%plot(:, L)) / REAL(IXMAX, C_FLOAT)
    
                ! Remove the zonal average from this bottom half row
                gTransArr%plot(:, L) = gTransArr%plot(:, L) - gTransArr%GZonal(L)
    
                ! Enforce anti-symmetry on the top half row
                gTransArr%plot(:, m_idx) = -gTransArr%plot(:, L)
    
                ! Mirror the zonal average to the top half symmetrically
                gTransArr%GZonal(m_idx) = gTransArr%GZonal(L)
            end do

            ! =====================================================================
            ! 3. Handle the Second Stationary Point (L = I + 1, y = pi)
            ! =====================================================================
            ! This is your true Nyquist peak row (e.g., Row 5 if IYMAX=8)
            gTransArr%GZonal(I + 1) = sum(gTransArr%plot(:, I + 1)) / REAL(ixmax, C_FLOAT)
            gTransArr%plot(:, I + 1)  = gTransArr%plot(:, I + 1) - gTransArr%GZonal(I + 1)

            ! Because this row is anti-symmetric with itself across the midpoint, 
            ! the 2D grid values at this specific row must mathematically be zeroed out.
            gTransArr%plot(:, I + 1) = 0.0_C_FLOAT 

            ! 6. Transform the zonal average to get the cos(Ly) components in spectral space.
            ! Clear 1D spectral scratch array before running the plan
            gTransArr%GcZ = CMPLX(0.0_C_FLOAT, 0.0_C_FLOAT, kind=C_FLOAT_COMPLEX)
            call fftwf_execute_dft_r2c(plan_y_r2c_1d, gTransArr%GZonal, gTransArr%GcZ)
            norm_factor_2d = 2./ REAL(IYMAX, C_FLOAT) ! Normalization factor for 1D FFTW transforms (see FFTW documentation)
            do L = 1, LMAX2
                zonal_val = REAL(gTransArr%GcZ(L + 1)) + REAL(gTransArr%GcZ(IYMAX - L + 1))
                FS(L) = zonal_val * norm_factor_2d
            end do

            ! Clear 2D spectral scratch array before running the plan
            gTransArr%Gc(:, :) = CMPLX(0.0_C_FLOAT, 0.0_C_FLOAT, kind=C_FLOAT_COMPLEX)
    
            ! ====================================================================
            ! STEP 1: EXECUTE COMPREHENSIVE 2D REAL-TO-COMPLEX FFT
            ! ====================================================================
            ! Input:  gTransArr%plot (Real array of size IXMAX, IYMAX)
            ! Output: gTransArr%Gc (Complex array of size IXMAX/2 + 1, IYMAX)
            call fftwf_execute_dft_r2c(plan_r2c_2d, gTransArr%plot, gTransArr%Gc)
    
            ! Global inverse scaling factor to normalize the raw FFTW accumulation
            ! norm_inv_wave is 2.0 / (IXMAX * IYMAX)
            norm_factor_2d = 0.5_C_FLOAT * norm_inv_wave
    
            ! ====================================================================
            ! STEP 2: EXTRACT ZONAL PROFILE (cosLy boundaries at K=0)
            ! ====================================================================
            !do L = 1, LMAX2
            !    ! Extract and average between positive and negative Y frequencies 
            !    ! to enforce pure cos(Ly) projection symmetry.
            !    zonal_val = REAL(gTransArr%Gc(1, L + 1)) + REAL(gTransArr%Gc(1, IYMAX - L + 1))
            !    FS(L) = zonal_val * norm_factor_2d
            !end do
    
            ! ====================================================================
            ! STEP 3: EXTRACT WAVE COMPONENTS (sinLy boundaries at K > 0)
            ! ====================================================================
            norm_factor_2d = norm_inv_wave
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
        end subroutine InverseTransform


        subroutine plotDiagnostic(grid, contourInterval, scale)
            USE IFWIN
            USE GDI32
            USE USER32
            USE, INTRINSIC :: iso_c_binding, only: c_intptr_t, c_loc, C_F_POINTER
            use Kernel32, only: RtlMoveMemory
            
            use ArrayDimensionsModule, only: gCurrentDims
            use ConstantsModule, only: PLOTTING_OFF, DIAGNOSTIC, wp
            use NLevQGModelGlobals, only: gBitMapInfo, ghwndMain
            use ModelFieldsModule, only: gTransArr
            use ModelParamsModule, only: gModIntParams

            IMPLICIT NONE


            real, intent(in) :: grid(:,:)
            real, intent(in) :: contourInterval
            real, intent(in) :: scale

            LOGICAL(BOOL)   :: bret
            REAL :: cntInt
            type (InterpolationBounds) b
        
            REAL, save, ALLOCATABLE, TARGET :: X(:,:), X_Overlay(:, :)  ! Created on the heap
            REAL, POINTER :: Xptr(:,:)
            integer(c_int), pointer :: pixelArray(:, :) ! Our map to the DIBSection
        
            integer(RGB32), target :: Pixels(gBitMapInfo%width)
            integer ::  iVal, iValRight, iValDown, i, j, J1, colorIdx, saveField, saveField2
            
            integer(c_intptr_t) :: baseAddr, targetAddr, srcAddr
            integer(c_intptr_t) :: bytesPerRow
            integer(4) :: colorToDraw
            
            logical :: useSolidColor = true

    !! ===========  NOT USING THIS NOW
            return

            saveField = gModIntParams%field1
            gModIntParams%field1 = DIAGNOSTIC
            saveField2 = gModIntParams%field2
            gModIntParams%field2 = PLOTTING_OFF

            gTransArr%plotFull(1:gCurrentDims%IXMAX, 1:gCurrentDims%IYMAX) = grid(1:gCurrentDims%IXMAX, 1:gCurrentDims%IYMAX)

            ! 2. Instantly replicate the periodic boundary column in one line (No DO loop!)
            gTransArr%plotFull(gCurrentDims%IXMAX + 1, 1:gCurrentDims%IYMAX) = gTransArr%plotFull(1, 1:gCurrentDims%IYMAX)

            ! --- RESIZE LOGIC ---
            ! Check if X needs allocation or resizing based on the DIB size
            if (useSolidColor) then
                if (.not. allocated(X)) then
                    allocate(X(gBitMapInfo%width, gBitMapInfo%height / 2))
                else if (size(X, 1) /= gBitMapInfo%width .or. size(X, 2) /= gBitMapInfo%height / 2) then
                    deallocate(X)
                    allocate(X(gBitMapInfo%width, gBitMapInfo%height / 2))
                end if
            end if
            ! Check if X_Overlay needs allocation or resizing based on the DIB size
            if (.NOT. useSolidColor) then
                if (.not. allocated(X_Overlay)) then
                    allocate(X_Overlay(gBitMapInfo%width, gBitMapInfo%height / 2))
                else if (size(X_Overlay, 1) /= gBitMapInfo%width .or. size(X_Overlay, 2) /= gBitMapInfo%height / 2) then
                    deallocate(X_Overlay)
                    allocate(X_Overlay(gBitMapInfo%width, gBitMapInfo%height / 2))
                end if
            end if
        
            if (useSolidColor) then
                Xptr => X        ! Note the pointer and not the equals! Not like C!
            else
                Xptr => X_Overlay
            endif
            ! Interpolate the grid array to a resolution that matches the size of the display of the
            ! weather map on the screen.
            b%il = 1
            b%iu = gCurrentDims%IXMAX
            b%jl = 1
            b%ju = gCurrentDims%IYMAX/2
            b%KL = 1
            b%KU = gBitMapInfo%width
            b%LL = 1
            b%LU = gBitMapInfo%height / 2
            gTransArr%plotFull = gTransArr%plotFull * scale
            call Interpolate2D(gTransArr%plotFull, b, Xptr)

            ! However, we need to map these interpolated values in X to colors for each pixel. We will
            ! fill a row (Pixels) with pixel color values and then copy it to the DIBSection. This is
            ! a lot more efficient than creating a 2d array of color values and then writing that to
            ! the DIBSection.
            
            ! In order to make contours instead of color that continuously transition for each value,
            ! We create a color ram of 18 values and let the user determine the contour interval. 
            
            ! 1. Width * 4 bytes per pixel (32-bit DIBs have no padding)
            bytesPerRow = int(gBitMapInfo%width, c_intptr_t) * 4
            ! 2. Calculate base address
            ! y=0 is the top row because 'bmi%bmiHeader%biHeight' is negative
            baseAddr = transfer(gBitMapInfo%pBits, baseAddr)

            if (useSolidColor) then
                ! Turn X into an integer array of 32-pixel values
                do J = gBitMapInfo%height / 2, 1, -1
                    J1 = gBitMapInfo%height / 2 - J + 1
                    do I = 1, gBitMapInfo%width
                        ! Calculate value based on the contour interval
                        iVal = FLOOR(Xptr(I, J1) / contourInterval) + NUM_RAM_COLORS / 2
                        ! Cycle through the colorRam if iVal exceeds its size
                        ! Use ABS to handle negative iVal if necessary
                        ! Modulo keeps the recycling positive versus mod which does not
                        colorIdx = modulo(iVal, NUM_RAM_COLORS) + 1
                        Pixels(I) = RAM_COLORS(colorIdx)
                    end do
                    ! 2. Calculate memory address for the desired row 'targetY'
                    ! y=0 is the top row because 'bmi%bmiHeader%biHeight' is negative
                    targetAddr = baseAddr + (int(J - 1, c_intptr_t) * bytesPerRow)

                    ! 3. Get the address of your source array
                    ! Pixels must have the TARGET attribute
                    srcAddr = transfer(c_loc(Pixels(1)), srcAddr)
        
                    ! 4. Move the interpolated array of Pixels into the DIBSection memory
                    ! Call the Intel-defined interface
                    ! Since the interface defines these as integers, pass them directly.
                    call RtlMoveMemory(targetAddr, srcAddr, bytesPerRow)
                end do
            else
                CALL C_F_POINTER(gBitMapInfo%pBits, pixelArray, [gBitMapInfo%width, gBitMapInfo%height])
                ! 2. Calculate borders by comparing current pixel to its neighbors.
                ! We stop at Width-1 and Height-1 to prevent "out of bounds" on the iVal checks.
                ! Mapping X_Overlay to the upper half of the screen
                do J = 1, (gBitMapInfo%height / 2) - 1
        
                    ! J_data maps Screen Top (J=1) to Array North (Height/2)
                    J1 = (gBitMapInfo%height / 2) - J + 1
                    ! J_next is the row physically below J on the screen (one step South in data)
                    !J_next = J_data - 1
        
                    do I = 1, gBitMapInfo%width - 1
        
                        ! Calculate interval bins
                        iVal      = floor(X_Overlay(I, J1) / contourInterval)
                        iValRight = floor(X_Overlay(I + 1, J1) / contourInterval)
                        iValDown  = floor(X_Overlay(I, J1 - 1) * contourInterval)
        
                        ! Detect boundary
                        if (iVal /= iValRight .or. iVal /= iValDown) then
                            colorIdx = max(iVal, iValRight, iValDown)    ! reuse colorIdx for the max level
                            if (colorIdx > 0) then
                                colorToDraw = z'00DD0077' ! Reddish-pink
                            else if (colorIdx < 0) then
                                colorToDraw = z'000000FF' ! Blue
                            else 
                                colorToDraw = z'00000000' ! White for the 0-contour
                            end if
                            
                            ! J=1 is the top row of the pixelArray
                            pixelArray(I, J) = colorToDraw
                        end if
                    end do
                end do
            endif
            gModIntParams%field1 = saveField
            call labelPlotsAndDisplayTime(.false.)
            call DrawGridLines(.true.)
            gModIntParams%field1 = DIAGNOSTIC
            call pauseRun()
            gModIntParams%field1 = saveField
            gModIntParams%field2 = saveField2

        end subroutine plotDiagnostic
        
        subroutine pauseRun()
            use ConstantsModule, only: PAUSED
            use ModelParamsModule, only: gModelState
            gModelState%runState = PAUSED
            do while (gModelState%runState == PAUSED)
                CALL PumpMessages()
                call sleep(100) ! Suspends the thread for 100ms. 0% CPU usage.
            end do
        end subroutine pauseRun

        !> This function gets the values of the current location in a paused time series plot based on the y-coordinate
        !> position of the mouse.
        !! @param  mouse_y: the y-ccordinate value of the mouse cursor
        !! @Param  field: variable being displayed
        function getYValues(mouse_y, field) result(labelText)
            use ConstantsModule, only: PLOTTING_OFF, WEATHER_STN
            use NLevQGModelGlobals, only: gBitMapInfo
            use ModelParamsModule, only: gModelState, gModIntParams, gContourIntervals
            
            implicit none
        
            integer, intent(in) :: mouse_y  ! y-coordinate of mouse
            integer, intent(in) :: field
        
            real :: value, topValue
            character(len=32) :: labelText
            character(len=16) :: units
            
            if (field == PLOTTING_OFF) then
                labelText = CHAR(0)
                return
            endif

            if (gModIntParams%plotType == WEATHER_STN) then
                topValue = gContourIntervals%Ranges(field)
                units = getUnits(field)
                ! This 'T12' forces the units to start at the 12th character of the string
                value = topValue * (1 - 2.0 * real(mouse_y) / REAL(gBitMapInfo%height))
                write(labelText, '(F10.3, T12, A)') value, trim(units)
            endif

        end function getYValues

        subroutine ClearScreen()
            USE IFWINTY
            USE GDI32
            USE USER32
            use IFWBASE ! Contains RGB macro
            use NLevQGModelGlobals, only: gBitMapInfo, ghwndMain
            use ConstantsModule, only: WEATHER_MAP, PHASE_AND_MAP, VERT_CROSS_SEC_AT_LAT, HOV_DIAG_AT_LAT, VERT_CROSS_SEC_AT_LONG, &
                HOV_DIAG_AT_LONG, PHASE_SPACE, WEATHER_STN, PLOTTING_OFF, &
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
                call clearBuffer(gWeatherStation1%timeSeriesBuf)
                call clearBuffer(gWeatherStation2%timeSeriesBuf)

                ! Do heights on right
                call DrawHorizontalLines(gWeatherStation2%field == PLOTTING_OFF, &     ! No screen update if false
                                         .true., &       ! put labels on left
                                         Z'0000DD00', &  ! color of labels
                                         gWeatherStation1)     ! isHeight is true for heights so draw horizontal lines         
                call DrawHorizontalLines(.true., &      ! true means force screen update. 
                                         .false., &     ! false means draw labels on right side
                                         Z'000055EE', &
                                         gWeatherStation2)
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
                gLatHovmoller1%row = 0
                gLatHovmoller2%row = 0
                call DrawVerticalLines(.false.)
            else if (gModIntParams%plotType == HOV_DIAG_AT_LONG) then
                gLonHovmoller1%row = 0
                gLonHovmoller2%row = 0
                !call DrawHorizontalLines(.false., .true., .false., #00323232) ! 'isHeight' parameter is not used in this case
            else if (gModIntParams%plotType == WEATHER_MAP) then
                call DrawGridLines(.false.)
            else if (gModIntParams%plotType == PHASE_AND_MAP) then
                call DrawGridLines(.false.)
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
        ! The level is now trickier. Now we have all levels in LEVP which includes the interfaces just for plotting
        ! So to get the actual levels where the fields are define we need to set levels = 1 + (plotLevels - 1) / 2 

        subroutine loadFields(ptr, field, plotLevel)
            use ConstantsModule, only: VORTICITY, HEIGHT, TEMPERATURE, VERTICAL_WIND, TEMP_FORCING, TOT_HGT_TEND, ADV_BETA_HGT_TEND, &
                OMEGA_HGT_TEND, DIABATIC_HGT_TEND, TOT_TEMP_TEND, ADV_TEMP_TEND, OMEGA_TEMP_TEND, DIABATIC_TEMP_TEND, HGT_FORCING, &
                OROG_HGT_TEND, OROGRAPHY

            use ArrayDimensionsModule, only: gCurrentDims
            use ModelFieldsModule, only: gHorzArr, gVertArr
            use ModelParamsModule, only: gModIntParams
            
            implicit none
            
            real, pointer, intent(out) :: ptr(:)
            integer, intent(in)        :: field
            integer, intent(in)        :: plotLevel
            
            integer :: level, levelt
            logical :: isTempField, isOdd, isInterp
            REAL, pointer :: var(:, :)
            REAL :: weight
            
            level = min(1 + (plotLevel - 1) / 2, gModIntParams%NLEV)
            levelt = min(1 + (plotLevel - 2) / 2, gModIntParams%NLEV - 1)

            ! Example when NLEV = 10
            ! plotLevel level   levelt pressure   temperature          height
            !    1        1       1     1000         t(1)               h(1)
            !    2        1       1      950         t(1)               h(1)
            !    3        2       1      900         t(1)           [h(1) + h(2)]/2     [h(level-1) + h(level)]/2
            !    4        2       2      850    [t(1) + t(2)]/2         h(2)            [t(levelt-1) + t(levelt)]/2 
            !    5        3       2      800         t(2)           [h(2) + h(3)]/2
            !    6        3       3      750    [t(2) + t(3)]/2         h(3)
            !    7        4       3      700         t(3)           [h(3) + h(4)]/2
            !    8        4       4      650
            !    9        5       4      600
            !    10       5       5      550
            !    11       6       5      500
            !    12       6       6      450
            !    13       7       6      400
            !    14       7       7      350
            !    15       8       7      300
            !    16       8       8      250
            !    17       9       8      200
            !    18       9       9      150    [t(8) + t(9)]/2         h(9)
            !    19       10      9      100         t(9)           [h(9) + h(10)]/2
            !    20       10      9       50         t(9)               h(10)
            !    21       10      9        0         t(9)               h(10)
            
            ! This makes level equal to actual index level of the streamfunction and the temperature such that
            ! PS(:, level) is the streamfunction at that level and TH(:, level) is the temperature at that level.
            ! The levelt is for the interface. 

            select case (field)
                case(VORTICITY)             ! Vorticity in 10**-5/s
                    var => gHorzArr%PS(:, :)      ! PS is the streamfunction spectral components
                    isTempField = .FALSE.

                case(HEIGHT)                ! Height in meters
                    var => gHorzArr%PS(:, :)      ! PS is the streamfunction spectral components
                    isTempField = .FALSE.

                case(TEMPERATURE)           ! Temperature in Centigrade
                    var => gHorzArr%TH(:, :)      ! TH(ITOT,maxLevels) is the temperature spectral components
                    isTempField = .TRUE.

                case(VERTICAL_WIND)         ! Vertical wind in mb/hr or mm/sec
                    var => gHorzArr%WW(:, :)      ! WW(ITOT,maxLevels) is the omega spectral components
                    isTempField = .FALSE.

                case(TEMP_FORCING)          ! Temperature driving (forcing) - Radiative relaxation temperature in Centigrade
                    var => gHorzArr%TF(:, :)      ! TF(ITOT,maxLevels) is the thermal forcing field
                    isTempField = .TRUE.

                case(TOT_HGT_TEND)          ! Total Height tendency in meters/day
                    var => gHorzArr%FS(:, :)      ! FS(ITOT,maxLevels) is the total streamfunction tendency
                    isTempField = .FALSE.

                case(ADV_BETA_HGT_TEND)     ! Height tendencies in m/day from Advection plus beta
                    var => gHorzArr%ADS(:, :)     ! ADS(ITOT,maxLevels) is the spectral form of the vorticity advection
                    isTempField = .FALSE.

                case(OMEGA_HGT_TEND)        ! Height tendencies in m/day from omega (dp/dt)
                    var => gHorzArr%WS(:, :)      ! WS(ITOT,maxLevels) is the omega forcing on the streamfunction
                    isTempField = .FALSE.

                case(DIABATIC_HGT_TEND)     ! Height tendencies in m/day from Diabatic effects (like dissipation)
                    var => gHorzArr%S(:, :)       ! S(ITOT,maxLevels) is the diabatic forcing on the streamfunction
                    isTempField = .FALSE.

                case(TOT_TEMP_TEND)         ! Total temperture tendencies in C/day
                    var => gHorzArr%FT(:, :)      ! FT(ITOT,maxLevels) is the temperature tendency
                    isTempField = .TRUE.

                case(ADV_TEMP_TEND)         ! Temperature tendencies in C/day from advection
                    var => gHorzArr%ADT(:, :)     ! ADT(ITOT,maxLevels) is the spectral form of the temperature advection
                    isTempField = .TRUE.

                case(OMEGA_TEMP_TEND)       ! Temperature tendencies in C/day from omega
                    var => gHorzArr%WT(:, :)      ! WT(ITOT,maxLevels) is the omega forcing on the temperature
                    isTempField = .TRUE.

                case(DIABATIC_TEMP_TEND)    ! Temperature tendencies in C/day from Diabatic effects
                    var => gHorzArr%T(:, :)       ! T(ITOT,maxLevels) is the diabatic forcing on the temperature
                    isTempField = .TRUE.

                case(HGT_FORCING)           ! Mean streamfunction (height) forcing in m
                    var => gHorzArr%SF(:, :)      ! SF(ITOT,maxLevels) is the Barotropic momentum forcing field
                    isTempField = .FALSE.

                case(OROG_HGT_TEND)         ! Height tendencies from ororgraphy in m/day
                    isTempField = .FALSE.

                case(OROGRAPHY)             ! Height of ororgraphy in m
                    ptr => gHorzArr%TOP(:) 
                    return
            end select

            isOdd = BTEST(plotLevel, 0) ! if Bit 0 is 1, the function returns true and the number is odd. This means we are in the middle of a layer 

            if (isTempField) then
                if (isOdd .OR. (plotLevel .LE. 3) .OR. (plotLevel .GE. SIZE(gVertArr%LVS) - 2)) then
                    ptr => var(:, levelt) ! At the interface level. Use the actual temperature and do not interpolate
                    isInterp = .FALSE.
                else
                    weight = (gVertArr%PP(level) - gVertArr%PV(level)) / (gVertArr%PP(level)- gVertArr%PP(level + 1))
                    gHorzArr%Interp_Temp(:) =  (1.0 - weight) * var(:, levelt - 1) + weight * var(:, levelt)
                    ptr => gHorzArr%Interp_Temp
                    isInterp = .TRUE.
                end if
            else
                if (isOdd .AND. (plotLevel .GE. 3) .AND. (plotLevel .LT. SIZE(gVertArr%LVS) - 1)) then
                    weight = (gVertArr%PV(level - 1) - gVertArr%PP(level)) / gVertArr%DP(level)
                    gHorzArr%Interp_Strm(:) =  (1.0 - weight) * var(:, level - 1) + weight * var(:, level)
                    ptr => gHorzArr%Interp_Strm
                    isInterp = .TRUE.
                else
                    ptr => var(:, level)    ! At the middle of the layer. Use the actual streamfunction and do not interpolate
                    isInterp = .FALSE.
                endif
            end if
            ! Example when NLEV = 10
            !write(*,*) 'Debug: plotLevel=', plotLevel, ' -> level=', level, ' levelt=', levelt, ' isOdd=', isOdd, ' is Temp: ', isTempField, ' is Interpolated: ', isInterp
            ! Debug: plotLevel= 1  -> level= 1  levelt= 1  isOdd= T  is Temp:  T  is Interpolated:  F
            ! Debug: plotLevel= 1  -> level= 1  levelt= 1  isOdd= T  is Temp:  F  is Interpolated:  F
            !
            ! Debug: plotLevel= 2  -> level= 1  levelt= 1  isOdd= F  is Temp:  T  is Interpolated:  F
            ! Debug: plotLevel= 2  -> level= 1  levelt= 1  isOdd= F  is Temp:  F  is Interpolated:  F
            !
            ! Debug: plotLevel= 3  -> level= 2  levelt= 1  isOdd= T  is Temp:  T  is Interpolated:  F
            ! Debug: plotLevel= 3  -> level= 2  levelt= 1  isOdd= T  is Temp:  F  is Interpolated:  T
            !
            ! Debug: plotLevel= 4  -> level= 2  levelt= 2  isOdd= F  is Temp:  T  is Interpolated:  T
            ! Debug: plotLevel= 4  -> level= 2  levelt= 2  isOdd= F  is Temp:  F  is Interpolated:  F
            !
            ! Debug: plotLevel= 5  -> level= 3  levelt= 2  isOdd= T  is Temp:  T  is Interpolated:  F
            ! Debug: plotLevel= 5  -> level= 3  levelt= 2  isOdd= T  is Temp:  F  is Interpolated:  T
            !
            ! Debug: plotLevel= 6  -> level= 3  levelt= 3  isOdd= F  is Temp:  T  is Interpolated:  T
            ! Debug: plotLevel= 6  -> level= 3  levelt= 3  isOdd= F  is Temp:  F  is Interpolated:  F
            !
            ! Debug: plotLevel= 7  -> level= 4  levelt= 3  isOdd= T  is Temp:  T  is Interpolated:  F
            ! Debug: plotLevel= 7  -> level= 4  levelt= 3  isOdd= T  is Temp:  F  is Interpolated:  T
            !
            ! Debug: plotLevel= 8  -> level= 4  levelt= 4  isOdd= F  is Temp:  T  is Interpolated:  T
            ! Debug: plotLevel= 8  -> level= 4  levelt= 4  isOdd= F  is Temp:  F  is Interpolated:  F
            !
            ! Debug: plotLevel= 9  -> level= 5  levelt= 4  isOdd= T  is Temp:  T  is Interpolated:  F
            ! Debug: plotLevel= 9  -> level= 5  levelt= 4  isOdd= T  is Temp:  F  is Interpolated:  T
            !
            ! Debug: plotLevel= 10  -> level= 5  levelt= 5  isOdd= F  is Temp:  T  is Interpolated:  T
            ! Debug: plotLevel= 10  -> level= 5  levelt= 5  isOdd= F  is Temp:  F  is Interpolated:  F
            !
            ! Debug: plotLevel= 11  -> level= 6  levelt= 5  isOdd= T  is Temp:  T  is Interpolated:  F
            ! Debug: plotLevel= 11  -> level= 6  levelt= 5  isOdd= T  is Temp:  F  is Interpolated:  T
            !
            ! Debug: plotLevel= 12  -> level= 6  levelt= 6  isOdd= F  is Temp:  T  is Interpolated:  T
            ! Debug: plotLevel= 12  -> level= 6  levelt= 6  isOdd= F  is Temp:  F  is Interpolated:  F
            !
            ! Debug: plotLevel= 13  -> level= 7  levelt= 6  isOdd= T  is Temp:  T  is Interpolated:  F
            ! Debug: plotLevel= 13  -> level= 7  levelt= 6  isOdd= T  is Temp:  F  is Interpolated:  T
            !
            ! Debug: plotLevel= 14  -> level= 7  levelt= 7  isOdd= F  is Temp:  T  is Interpolated:  T
            ! Debug: plotLevel= 14  -> level= 7  levelt= 7  isOdd= F  is Temp:  F  is Interpolated:  F
            !
            ! Debug: plotLevel= 15  -> level= 8  levelt= 7  isOdd= T  is Temp:  T  is Interpolated:  F
            ! Debug: plotLevel= 15  -> level= 8  levelt= 7  isOdd= T  is Temp:  F  is Interpolated:  T
            !
            ! Debug: plotLevel= 16  -> level= 8  levelt= 8  isOdd= F  is Temp:  T  is Interpolated:  T
            ! Debug: plotLevel= 16  -> level= 8  levelt= 8  isOdd= F  is Temp:  F  is Interpolated:  F
            !
            ! Debug: plotLevel= 17  -> level= 9  levelt= 8  isOdd= T  is Temp:  T  is Interpolated:  F
            ! Debug: plotLevel= 17  -> level= 9  levelt= 8  isOdd= T  is Temp:  F  is Interpolated:  T
            !
            ! Debug: plotLevel= 18  -> level= 9  levelt= 9  isOdd= F  is Temp:  T  is Interpolated:  T
            ! Debug: plotLevel= 18  -> level= 9  levelt= 9  isOdd= F  is Temp:  F  is Interpolated:  F
            !
            ! Debug: plotLevel= 19  -> level= 10  levelt= 9  isOdd= T  is Temp:  T  is Interpolated:  F
            ! Debug: plotLevel= 19  -> level= 10  levelt= 9  isOdd= T  is Temp:  F  is Interpolated:  T
            !
            ! Debug: plotLevel= 20  -> level= 10  levelt= 9  isOdd= F  is Temp:  T  is Interpolated:  F
            ! Debug: plotLevel= 20  -> level= 10  levelt= 9  isOdd= F  is Temp:  F  is Interpolated:  F
            !
            ! Debug: plotLevel= 21  -> level= 10  levelt= 9  isOdd= T  is Temp:  T  is Interpolated:  F
            ! Debug: plotLevel= 21  -> level= 10  levelt= 9  isOdd= T  is Temp:  F  is Interpolated:  F

        end subroutine loadFields
        
        function getUnits(field) result(text)
            use ConstantsModule, only: VORTICITY, HEIGHT, TEMPERATURE, VERTICAL_WIND, TEMP_FORCING, TOT_HGT_TEND, ADV_BETA_HGT_TEND, &
                OMEGA_HGT_TEND, DIABATIC_HGT_TEND, TOT_TEMP_TEND, ADV_TEMP_TEND, OMEGA_TEMP_TEND, DIABATIC_TEMP_TEND, HGT_FORCING, &
                OROG_HGT_TEND, OROGRAPHY

            implicit none

            integer, intent(in)        :: field
            
            character(len=16) :: text

            select case (field)
                case(VORTICITY)             ! Vorticity in 10**-5/s
                    text =  " 10**-5/s"

                case(HEIGHT)                ! Height in meters
                    text =  " m"

                case(TEMPERATURE)           ! Temperature in Centigrade
                    text = CHAR(176)//"C"

                case(VERTICAL_WIND)         ! Vertical wind in mb/hr or mm/sec
                    text = " mb/hr"
                    
                case(TEMP_FORCING)          ! Temperature driving (forcing) - Radiative relaxation temperature in Centigrade
                    text = CHAR(176)//"C/day"

                case(TOT_HGT_TEND)          ! Total Height tendency in meters/day
                    text = " m/day"

                case(ADV_BETA_HGT_TEND)     ! Height tendencies in m/day from Advection plus beta
                    text = " m/day"

                case(OMEGA_HGT_TEND)        ! Height tendencies in m/day from omega (dp/dt)
                    text = " m/day"

                case(DIABATIC_HGT_TEND)     ! Height tendencies in m/day from Diabatic effects (like dissipation)
                    text = " m/day"

                case(TOT_TEMP_TEND)         ! Total temperture tendencies in C/day
                    text = CHAR(176)//"C/day"

                case(ADV_TEMP_TEND)         ! Temperature tendencies in C/day from advection
                    text = CHAR(176)//"C/day"

                case(OMEGA_TEMP_TEND)       ! Temperature tendencies in C/day from omega
                    text = CHAR(176)//"C/day"

                case(DIABATIC_TEMP_TEND)    ! Temperature tendencies in C/day from Diabatic effects
                    text = CHAR(176)//"C/day"

                case(HGT_FORCING)           ! Mean streamfunction (height) forcing in m
                    text = " m/day"
                    
                case(OROG_HGT_TEND)         ! Height tendencies from ororgraphy in m/day
                    text = " m/day"

                case(OROGRAPHY)             ! Height of ororgraphy in m
                    text = " m" 

            end select
        end function getUnits

! PLOT PHASE SPACE ----------------------------------------------------
        subroutine plotPhaseSpace(phaseSpaceType, invalidate)
            USE IFWINTY
            USE USER32
            !USE IFWBASE ! For RGB
            USE, INTRINSIC :: ISO_C_BINDING

            use NLevQGModelGlobals, only: gBitMapInfo, ghwndMain
            use ModelParamsModule, only: gModIntParams

            ! Windows (0,0) is the top-left corner and the range is 

            IMPLICIT NONE

            type(PhaseSpace), intent(inout) :: phaseSpaceType
            LOGICAL, intent(in) :: invalidate

            INTEGER ::      x_pix, y_pix, iret
            REAL ::         xc, yc
            TYPE(T_RECT) :: rect
            INTEGER(4), POINTER :: pixelArray(:, :)

            ! Need to do this to recompute interplated fields every plot time
            call loadFields( phaseSpaceType%x, phaseSpaceType%field, gModIntParams%LEVP)
            call loadFields( phaseSpaceType%y, phaseSpaceType%field, gModIntParams%LEVP)

            xc = phaseSpaceType%x(phaseSpaceType%xIndex) * phaseSpaceType%xScale
            yc = phaseSpaceType%y(phaseSpaceType%yIndex) * phaseSpaceType%yScale

            x_pix = phaseSpaceType%xcenter + INT(xc)   ! Note screen coordinates are 0-based.
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

! PLOT WEATHER STN TIME SERIES ----------------------------------------------------
        !> This routine plots the time series of a 'weather station' at a selected lat/lon
        !> coordinate. It computes the value from the spectral coefficients times the associated
        !> basis function by brute force (no FFT).
        subroutine plotTimeSeries()
            use ConstantsModule, only: PLOTTING_OFF
            use ArrayDimensionsModule, only: gCurrentDims
            use ModelParamsModule, only: gModIntParams

            IMPLICIT NONE
            
            real :: value
            integer :: I, K, L

            if (gWeatherStation1%field /= PLOTTING_OFF) then
                call loadFields(gWeatherStation1%value, gWeatherStation1%field, gModIntParams%LEVP)
                ! Compute the value
                value = computeValue(gWeatherStation1)
                call loadTimeSeriesArray(gWeatherStation1%timeSeriesBuf, value * gWeatherStation1%scale, &
                                         gWeatherStation1%yCenter, gWeatherStation1%gDCcolor, .true., gWeatherStation2%field == PLOTTING_OFF)
                call DrawHorizontalLines(.false., .true., WX_STN_TEMP_COLOR, gWeatherStation1) ! true means force screen update. false means draw labels on right side

            endif
            if (gWeatherStation2%field /= PLOTTING_OFF) then
                call loadFields(gWeatherStation2%value, gWeatherStation2%field, gModIntParams%LEVP)
                value = computeValue(gWeatherStation2)
                call loadTimeSeriesArray(gWeatherStation2%timeSeriesBuf, value * gWeatherStation2%scale, &
                                         gWeatherStation2%yCenter, gWeatherStation2%gDCcolor, gWeatherStation1%field == PLOTTING_OFF, .true.)
                call DrawHorizontalLines(.false., gWeatherStation1%field == PLOTTING_OFF, WX_STN_TEMP_COLOR, gWeatherStation2)
            endif

            call labelPlotsAndDisplayTime(.true.)

        end subroutine plotTimeSeries
        
        function computeValue(weatherStn) result (value)
                use ArrayDimensionsModule, only: gCurrentDims
                use ModelParamsModule, only: gModIntParams
                implicit none

                type(WeatherStation), intent(in) :: weatherStn
                
                real :: value
                integer :: I, K, L

                value = 0.0
                do I = 1, gModIntParams%LMAX2
                    value = value + weatherStn%value(I) * weatherStn%cosy(I)
                end do
                K = 1
                L = 1
                do I = gModIntParams%LMAX2 + 1, gCurrentDims%ITOT - 1
                    value = value + (weatherStn%value(I) * weatherStn%cosx(K) + &
                                     weatherStn%value(I + 1) * weatherStn%sinx(K)) * weatherStn%siny(L)
                    L = L + 1
                    if (L .GT. gModIntParams%LMAX2) then
                        K = 1
                        L = 1
                    endif
                end do
        end function computeValue

        subroutine loadTimeSeriesArray(timeSeriesBuf, value, yzero_axis, color, clearScreen, drawMovingTimeLines)
            use NLevQGModelGlobals, only: gBitMapInfo, ghwndMain
            use ModelParamsModule, only: gModRealParams, gModIntParams
        
            USE IFWINTY
            USE IFWINA   ! Replaced GDI32 for standard Intel compatibility
            USE USER32
            USE, INTRINSIC :: ISO_C_BINDING
            implicit none
        
            type(TimeSeriesBuffer), intent(inout) :: timeSeriesBuf
            real, intent(in) :: value
            integer(4), intent(in) :: yzero_axis
            integer(4), intent(in) :: color
            logical, intent(in) :: clearScreen
            logical, intent(in) :: drawMovingTimeLines
            
            LOGICAL(BOOL)   :: bret
            INTEGER(4)      :: i, read_idx, iret, flippedColor
            INTEGER(HANDLE) :: hBrush, hOldBrush, hPen, hOldPen, hPenAxis
        
            INTEGER(4), POINTER :: pixelArray(:, :)
            integer :: ix, iy, first_mark , gridInterval, kInterval
            character(len=32) :: timeStr
            real :: days
            integer :: years
            LOGICAL :: drawLines = .TRUE.
        
            ! 1. Update Circular Buffer (1-based)
            timeSeriesBuf%head_idx = MOD(timeSeriesBuf%head_idx, gBitMapInfo%width) + 1
            timeSeriesBuf%data_buffer(timeSeriesBuf%head_idx) = value
            timeSeriesBuf%count = MIN(timeSeriesBuf%count + 1, gBitMapInfo%width) ! Keep a count of how many points are loaded. This is to stop the sprious 
                                                                                  ! zero points at the start when the buffer has not been filled. So count will
                                                                                  ! will go from 0 to screen width. ONce at the screen width it will stay there
                                                                                  ! until reset.
        
            ! 2. Prepare Point Array (Mapping circular buffer to 0-based screen X)
            DO i = 1, timeSeriesBuf%count
                ! Offset the starting point so the "newest" data is always on the right
                read_idx = MOD(timeSeriesBuf%head_idx - timeSeriesBuf%count + i, gBitMapInfo%width)
                if (read_idx < 1) read_idx = read_idx + gBitMapInfo%width
                !read_idx = read_idx + 1 ! Back to 1-based
        
                timeSeriesBuf%plot_pts(i)%x = (gBitMapInfo%width - timeSeriesBuf%count) + (i - 1)
                ! NOTE: Flip and Scale Y. Windows Y=0 is top. 
                ! This positions the y-axis = 0
                timeSeriesBuf%plot_pts(i)%y = yzero_axis - NINT(timeSeriesBuf%data_buffer(read_idx))
            END DO
            ! 5. Clear Background (Dark Green)
            hBrush = CreateSolidBrush(RGB(0, 30, 10))
            hOldBrush = SelectObject(gBitMapInfo%hMemDC, hBrush)
            if (clearScreen) iret = PatBlt(gBitMapInfo%hMemDC, 0, 0, gBitMapInfo%width, gBitMapInfo%height, PATCOPY)
        
            ! 6. Draw lines (Optional: using your brush color or a pen)
            ! [Insert your MoveToEx/LineTo axes code here if desired]
            if (drawLines) then
                ! Create a Solid Pen: PS_SOLID, Width=2, Color=Yellow (RGB 255, 255, 0)
                hPen = CreatePen(PS_SOLID, 0, color) ! 0  = 1-pixel wide line regardless of GDI logical units
                hOldPen = SelectObject(gBitMapInfo%hMemDC, hPen)
        
                ! 7. Draw the entire line at once
                ! Polyline uses the current Pen.
                bret = Polyline(gBitMapInfo%hMemDC, timeSeriesBuf%plot_pts(1), timeSeriesBuf%count - 1)
                ! Cleanup GDI Objects
                hOldPen = SelectObject(gBitMapInfo%hMemDC, hOldPen)
                bret = DeleteObject(hPen)
            else
                bret = GdiFlush()
                CALL C_F_POINTER(gBitMapInfo%pBits, pixelArray, [gBitMapInfo%width, gBitMapInfo%height])
        
                ! 1. Pre-calculate flipped color once outside the loop
                ! Converts 0x00BBGGRR (Polyline) to 0x00RRGGBB (DIB)
                flippedColor = IOR(ISHFT(IAND(color, Z'000000FF'), 16), & ! R: bit 0 -> 16
                               IOR(IAND(color, Z'0000FF00'), &            ! G: bit 8 -> 8 (stays)
                                   ISHFT(IAND(color, Z'00FF0000'), -16))) ! B: bit 16 -> 0
                do i = 1, timeSeriesBuf%count
                    ! Get coordinates from your buffer
                    ix = timeSeriesBuf%plot_pts(i)%x
                    iy = timeSeriesBuf%plot_pts(i)%y
                
                    ! Bounds checking is critical when using pointers!
                    if (ix >= 0 .and. ix < gBitMapInfo%width .and. &
                        iy >= 0 .and. iy < gBitMapInfo%height) then
                
                        ! Direct memory write (Fast!)
                        ! Color #0000FF00 is Green in ABGR (00 Alpha, 00 Red, FF Green, 00 Blue)
                        pixelArray(ix + 1, iy + 1) = flippedColor 
                    end if
                end do
            end if
        
            hpenaxis = createpen(ps_solid, 2, rgb(50, 50, 50)) ! 0  = 1-pixel wide line regardless of gdi logical units
            hOldpen = selectobject(gbitmapinfo%hmemdc, hpenaxis)
        
            ! 8. Draw X-Axis (Horizontal line through the middle)
            bret = movetoex(gbitmapinfo%hmemdc, 0, gbitmapinfo%height / 2, null)
            bret = lineto(gbitmapinfo%hmemdc, gbitmapinfo%width, gbitmapinfo%height / 2)
        
            if (drawMovingTimeLines) then 
                bret = SetTextColor(gBitMapInfo%hMemDC, RGB(100, 100, 100))
                ! Now draw the vertical lines which represent time. These axes will need to move to the left
                ! as new values come in.
                ! How many pixels represent the gap between lines
                gridInterval = gBitMapInfo%width / 20 
            
                ! The distance between lines in units of 'obsK'
                ! If you plot every 10 steps, a 50-pixel gap represents 500 steps
                kInterval = gridInterval * gModIntParams%outputInterval

                ! Align the first mark to the nearest kInterval
                first_mark = (gElapsedTime%timeSteps / kInterval) * kInterval
        
                do i = first_mark, first_mark - (gBitMapInfo%width * gModIntParams%outputInterval), -kInterval
                    if (i < 0) exit 
        
                    ! NEW CALCULATION:
                    ! Difference in timesteps divided by interval = difference in pixels
                    ix = (gBitMapInfo%width - 1) - ((gElapsedTime%timeSteps - i) / gModIntParams%outputInterval)
        
                    if (ix >= 0) then
                        ! 1. Draw Vertical Line (using MoveToEx/LineTo)
                        bret = MoveToEx(gBitMapInfo%hMemDC, ix, gBitMapInfo%height, NULL)
                        bret = LineTo(gBitMapInfo%hMemDC, ix, 0)
        
                        ! 2. Draw Text Label which will be years:days.fraction
                        days =  REAL(i) * REAL(gElapsedTime%dimensionalTime)
                        years = Floor(days / 365.0_wp)
                        days = days - years * 365
                        write(timeStr, '( I0, ":", F0.2)') years, days
                        bret = SetTextAlign(gBitMapInfo%hMemDC, TA_CENTER)
                        bret = TextOut(gBitMapInfo%hMemDC, ix, 2, trim(timeStr), len_trim(timeStr))
                    end if
                end do
            endif

            ! Cleanup GDI Objects
            hOldPen = SelectObject(gBitMapInfo%hMemDC, hOldPen)
            bret = DeleteObject(hPenAxis)
            hOldBrush = SelectObject(gBitMapInfo%hMemDC, hOldBrush)
            bret = DeleteObject(hBrush)

        end subroutine loadTimeSeriesArray

! PLOT HOVMOLLER AT LATITUDE -------------------------------------------
        ! Plot a hovmoller row at a specified latitude on the screen
        ! @Param: row The row on the bit map to plot (0 is the top row
        subroutine plotLatHovmoller(latHovmoller, invalidate)
            use Kernel32, only: RtlMoveMemory
            USE IFWIN
            USE GDI32
            USE USER32
            USE, INTRINSIC :: iso_c_binding, only: c_intptr_t, c_loc
            use ArrayDimensionsModule, only: gCurrentDims
            use NLevQGModelGlobals, only: gBitMapInfo, ghwndMain
            use ModelFieldsModule, only: gHorzArr, gTransArr
            use ModelParamsModule, only: gModIntParams

            
            IMPLICIT NONE

            type(Hovmoller), intent(inout) :: latHovmoller  ! Need the out as the 'row' is kept track of here.
            LOGICAL, intent(in) :: invalidate

            LOGICAL(BOOL) :: bret
            REAL, save, ALLOCATABLE, TARGET :: X(:, :), X_Overlay(:, :)  ! Created on the heap
            REAL, POINTER :: Xptr(:, :)
            integer(RGB32), pointer :: dibArray(:,:)

            integer(RGB32), target :: Pixels(gBitMapInfo%width)
            integer :: K, L, iVal, iValLeft, iValUp, prevRow, index, offset, colorIdx, colorToDraw
            
            integer(c_intptr_t) :: baseAddr, targetAddr, srcAddr, currentRow
            integer(c_intptr_t) :: bytesPerRow

            type(InterpolationBounds) b
            
            ! Returns transformed latitude line at specified level in gTransArr%GZonal
            call loadLatValues(latHovmoller, gModIntParams%LEVP, 1.0)

            if (latHovmoller%useSolidColor) then
                if (.not. allocated(X)) allocate(X(gBitMapInfo%width, 3))
                Xptr => X        ! Note the pointer and not the equals! Not like C!
            else
                if (.not. allocated(X_Overlay)) allocate(X_Overlay(gBitMapInfo%width, 3))
                Xptr => X_Overlay
            endif
            
            b%il = 1
            b%iu = gCurrentDims%IXMAX + 1
            b%KL = 1
            b%KU = gBitMapInfo%width

            currentRow = mod(latHovmoller%row, 3) + 1 
            call Interpolate1D(gTransArr%GZonal, b, .true., Xptr(:, currentRow)) ! is periodic

            if (latHovmoller%useSolidColor) then
                ! Turn X into an integer array of 32-pixel values
                do K = 1, gBitMapInfo%width
                    ! Calculate value based on the contour interval
                    iVal = FLOOR(Xptr(K, currentRow) * latHovmoller%contourIntervalInv) + 9
                    ! Cycle through the colorRam if iVal exceeds its size
                    ! Use ABS to handle negative iVal if necessary
                    ! Modulo keeps the recylcing postive versus mod which does not
                    colorIdx = modulo(iVal, NUM_RAM_COLORS) + 1
        
                    Pixels(K) = RAM_COLORS(colorIdx)
                end do
                    
                ! 1. Width * 4 bytes per pixel (32-bit DIBs have no padding)
                bytesPerRow = int(gBitMapInfo%width, c_intptr_t) * 4
            
                ! 2. Calculate memory address for the desired row 'targetY'
                ! y=0 is the top row because 'bmi%bmiHeader%biHeight' is negative
                baseAddr = transfer(gBitMapInfo%pBits, baseAddr)
                targetAddr = baseAddr + (int(latHovmoller%row, c_intptr_t) * bytesPerRow)
            
                ! 3. Get the address of your source array
                ! Pixels must have the TARGET attribute
                srcAddr = transfer(c_loc(Pixels(1)), srcAddr)
        
                ! 4. Move the interpolated array of Pixels into the DIBSection memory
                ! Call the Intel-defined interface
                ! Since the interface defines these as integers, pass them directly.
                call RtlMoveMemory(targetAddr, srcAddr, bytesPerRow)
            else
                call C_F_POINTER(gBitMapInfo%pBits, dibArray, [gBitMapInfo%width, gBitMapInfo%height])
                ! 2. Calculate borders by comparing current pixel to its neighbors.
                !    Search through the row from 'left' to  'right'
                prevRow = currentRow - 1
                if (prevRow < 1) prevRow = 3
                do K = 1, gBitMapInfo%width - 1

                    L = gBitMapInfo%width - K + 1

                    iVal      = floor(X_Overlay(L, currentRow) * latHovmoller%contourIntervalInv)
                    iValLeft  = floor(X_Overlay(L - 1, currentRow) * latHovmoller%contourIntervalInv)
                    iValUp    = floor(X_Overlay(L, prevRow) * latHovmoller%contourIntervalInv)
                    ! Detect boundary
                    if (iVal /= iValLeft) then
                        colorIdx = max(iVal, iValLeft)    ! reuse colorIdx for the max level
                        if (colorIdx > 0) then
                            colorToDraw = z'00DD0077' ! Reddish-pink
                        else if (colorIdx < 0) then
                            colorToDraw = z'000000FF' ! Blue
                        else 
                            colorToDraw = z'00000000' ! White for the 0-contour
                        end if
                        ! J=1 is the top row of the pixelArray

                        dibArray(K, latHovmoller%row + 1) = colorToDraw
                    end if
                    if (iVal /= iValUp .AND. latHovmoller%row > 0) then
                        colorIdx = max(iVal, iValUp)    ! reuse colorIdx for the max level
                        if (colorIdx > 0) then
                            colorToDraw = z'00DD0077' ! Reddish-pink
                        else if (colorIdx < 0) then
                            colorToDraw = z'000000FF' ! Blue
                        else 
                            colorToDraw = z'00000000' ! White for the 0-contour
                        end if
                        ! J=1 is the top row of the pixelArray

                        dibArray(K, latHovmoller%row + 1) = colorToDraw
                    end if
                    
                end do
            endif

            latHovmoller%row = mod(latHovmoller%row + 1, gBitMapInfo%height)
            call labelPlotsAndDisplayTime(.false.)
            call DrawVerticalLines(invalidate) ! True invalidates window and forces redraw
        
        end subroutine plotLatHovmoller
        
! PLOT VERT CROSS SECTION AT LATITUDE -------------------------------------------
        ! Plot a vertical cross section at a specified latitude on the screen
        ! @Param: latVertCrossSec: The structure containing the info not dependent upon new values.
        ! @Param: invalidate: If true, a screen redraw is forced.
        subroutine plotLatVertCrossSec(latVertCrossSec, invalidate)
            USE IFWIN
            USE GDI32
            USE USER32
            USE, INTRINSIC :: iso_c_binding, only: c_intptr_t, c_loc, C_F_POINTER
            use Kernel32, only: RtlMoveMemory
            
            use ArrayDimensionsModule, only: gCurrentDims
            use ConstantsModule, only: PLOTTING_OFF
            use NLevQGModelGlobals, only: gBitMapInfo, ghwndMain
            use ModelFieldsModule, only: gTransArr, gTransArr, gVertArr
            use ModelParamsModule, only: gModIntParams

            IMPLICIT NONE


            LOGICAL, intent(in) :: invalidate
            type(VertCrossSection), intent(in) :: latVertCrossSec


            LOGICAL(BOOL) :: bret
            REAL :: cntInt
            type (InterpolationBounds) b
        
            REAL, save, ALLOCATABLE, TARGET :: X(:,:), X_Overlay(:, :)  ! Created on the heap
            REAL, POINTER :: Xptr(:,:)
            integer(c_int), pointer :: pixelArray(:, :) ! Our map to the DIBSection
        
            integer(RGB32), target :: Pixels(gBitMapInfo%width)
            integer ::  LEVS, N, iVal, iValRight, iValDown, i, j, J1, colorIdx
            
            integer(c_intptr_t) :: baseAddr, targetAddr, srcAddr
            integer(c_intptr_t) :: bytesPerRow
            integer(4) :: colorToDraw
            
            if (latVertCrossSec%field == PLOTTING_OFF) return    ! Do nothing
            
            LEVS = SIZE(gVertArr%LVS)
            gTransArr%plotFull = 0.0

            do N = 1, LEVS
                ! Populates gTransArr%GZonal with a row of transformed values at that level and latitude including the periodic IXMAX + 1
                call loadLatValues(latVertCrossSec, N, latVertCrossSec%tempScales(N))
                gTransArr%plotFull(1 : gCurrentDims%IXMAX + 1, N) = gTransArr%GZonal(1 : gCurrentDims%IXMAX + 1)
            end DO
            
            ! --- RESIZE LOGIC ---
            ! Check if X needs allocation or resizing based on the DIB size
            if (latVertCrossSec%useSolidColor) then
                if (.not. allocated(X)) then
                    allocate(X(gBitMapInfo%width, gBitMapInfo%height))
                else if (size(X, 1) .NE. gBitMapInfo%width .or. size(X, 2) .NE. gBitMapInfo%height) then ! size(X, 1) is size of first dimension
                    deallocate(X)
                    allocate(X(gBitMapInfo%width, gBitMapInfo%height))
                end if
            end if
            ! Check if X_Overlay needs allocation or resizing based on the DIB size
            if (.NOT. latVertCrossSec%useSolidColor) then
                if (.not. allocated(X_Overlay)) then
                    allocate(X_Overlay(gBitMapInfo%width, gBitMapInfo%height))
                else if (size(X_Overlay, 1) .NE. gBitMapInfo%width .or. size(X_Overlay, 2) .NE. gBitMapInfo%height) then
                    deallocate(X_Overlay)
                    allocate(X_Overlay(gBitMapInfo%width, gBitMapInfo%height))
                end if
            end if
        
            if (latVertCrossSec%useSolidColor) then
                Xptr => X        ! Note the pointer and not the equals! Not like C!
            else
                Xptr => X_Overlay
            endif
            ! Interpolate the grid array to a resolution that matches the size of the display of the
            ! weather map on the screen.
            b%il = 1
            b%iu = gCurrentDims%IXMAX + 1
            b%jl = 1
            b%ju = LEVS
            b%KL = 1
            b%KU = gBitMapInfo%width
            b%LL = 1
            b%LU = gBitMapInfo%height
            call Interpolate2D(gTransArr%plotFull, b, Xptr)
            ! However, we need to map these interpolated values in X to colors for each pixel. We will
            ! fill a row (Pixels) with pixel color values and then copy it to the DIBSection. This is
            ! a lot more efficient than creating a 2d array of color values and then writing that to
            ! the DIBSection.
            
            ! In order to make contours instead of color that continuously transition for each value,
            ! We create a color ram of 18 values and let the user determine the contour interval. 
            
            ! 1. Width * 4 bytes per pixel (32-bit DIBs have no padding)
            bytesPerRow = int(gBitMapInfo%width, c_intptr_t) * 4
            ! 2. Calculate base address
            ! y=0 is the top row because 'bmi%bmiHeader%biHeight' is negative
            baseAddr = transfer(gBitMapInfo%pBits, baseAddr)

            if (latVertCrossSec%useSolidColor) then
                ! Turn X into an integer array of 32-pixel values
                do J = gBitMapInfo%height, 1, -1
                    J1 = gBitMapInfo%height - J + 1
                    do I = 1, gBitMapInfo%width
                        ! Calculate value based on the contour interval
                        iVal = FLOOR(Xptr(I, J1) * latVertCrossSec%contourIntervalInv) + NUM_RAM_COLORS / 2
                        ! Cycle through the colorRam if iVal exceeds its size
                        ! Use ABS to handle negative iVal if necessary
                        ! Modulo keeps the recycling positive versus mod which does not
                        colorIdx = modulo(iVal, NUM_RAM_COLORS) + 1
                        Pixels(I) = RAM_COLORS(colorIdx)
                    end do
                    ! 2. Calculate memory address for the desired row 'targetY'
                    ! y=0 is the top row because 'bmi%bmiHeader%biHeight' is negative
                    targetAddr = baseAddr + (int(J - 1, c_intptr_t) * bytesPerRow)

                    ! 3. Get the address of your source array
                    ! Pixels must have the TARGET attribute
                    srcAddr = transfer(c_loc(Pixels(1)), srcAddr)
        
                    ! 4. Move the interpolated array of Pixels into the DIBSection memory
                    ! Call the Intel-defined interface
                    ! Since the interface defines these as integers, pass them directly.
                    call RtlMoveMemory(targetAddr, srcAddr, bytesPerRow)
                end do
            else
                CALL C_F_POINTER(gBitMapInfo%pBits, pixelArray, [gBitMapInfo%width, gBitMapInfo%height])
                ! 2. Calculate borders by comparing current pixel to its neighbors.
                ! We stop at Width-1 and Height-1 to prevent "out of bounds" on the iVal checks.
                ! Mapping X_Overlay to the upper half of the screen
                do J = 1, gBitMapInfo%height - 1
        
                    ! J_data maps Screen Top (J=1) to Array North (Height/2)
                    J1 = gBitMapInfo%height - J + 1
                    ! J_next is the row physically below J on the screen (one step South in data)
                    !J_next = J_data - 1
        
                    do I = 1, gBitMapInfo%width - 1
        
                        ! Calculate interval bins
                        iVal      = floor(X_Overlay(I, J1) * latVertCrossSec%contourIntervalInv)
                        iValRight = floor(X_Overlay(I + 1, J1) * latVertCrossSec%contourIntervalInv)
                        iValDown  = floor(X_Overlay(I, J1 - 1) * latVertCrossSec%contourIntervalInv)
        
                        ! Detect boundary
                        if (iVal /= iValRight .or. iVal /= iValDown) then
                            colorIdx = max(iVal, iValRight, iValDown)    ! reuse colorIdx for the max level
                            if (colorIdx > 0) then
                                colorToDraw = z'00DD0077' ! Reddish-pink
                            else if (colorIdx < 0) then
                                colorToDraw = z'000000FF' ! Blue
                            else 
                                colorToDraw = z'00000000' ! White for the 0-contour
                            end if
                            
                            ! J=1 is the top row of the pixelArray
                            pixelArray(I, J) = colorToDraw
                        end if
                    end do
                end do
            endif
            call labelPlotsAndDisplayTime(invalidate)
            !call DrawGridLines(invalidate)

        end subroutine plotLatVertCrossSec
        
        !! Helper to load transformed grid values along a latitude at a given level
        !! The result is loaded into gTransArr%GZonal including the periodic value at IXMAX + 1
        subroutine loadLatValues(trigTableType, level, tempScale)
            use ArrayDimensionsModule, only: gCurrentDims
            use ConstantsModule, only: SQRT2
            use ModelFieldsModule, only: gHorzArr, gTransArr
            use ModelParamsModule, only: gModIntParams, gIsTemperature
            use Fftw3HelpersModule, only: norm_fwd, norm_cosly, plan_x_c2r_1d
            
            implicit none

            include 'fftw3.f03'

            class(TrigTable), intent(in) :: trigTableType
            integer, intent(in) :: level
            REAL, intent(in) :: tempScale
            
            integer :: KMAX, LMAX, IXMAX, K, L, index
            REAL :: temp_calc, zonalMean, sin_ly, scale
            
            KMAX = gModIntParams%KMAX2
            LMAX = gModIntParams%LMAX2
            IXMAX = gCurrentDims%IXMAX

            call loadFields(trigTableType%value, trigTableType%field, level)
            
            if (gIsTemperature(trigTableType%field)) then
                scale = trigTableType%scale / tempScale
            else 
                scale = trigTableType%scale
            endif

            ! 1. Calculate the zonal mean (the first LMAX elements)
            !    This is the "background" field at latitude y0
            ! There is no transform here so we dont need to worry about normalization
            zonalMean = sum(trigTableType%value(1:LMAX) * trigTableType%cosy(1:LMAX)) * SQRT2 * scale

            ! 1. Initialize input with zeros (crucial since k=0 is empty)
            gTransArr%GcZ = cmplx(0.0, 0.0, kind=C_FLOAT_COMPLEX)
            temp_calc = 2. * norm_fwd * scale ! Do the normalization first so we dont have to normalize after the transform

            ! 2. Sum over L to collapse the Y-dimension
            do L = 1, LMAX
                sin_ly = trigTableType%siny(L) ! Precomputed sinly's
                do K = 1, KMAX
                    ! Indexing for the 1-d storage of the zonal and wave components:
                    index = LMAX + 2 * LMAX * (K - 1) + 2 * (L - 1) + 1
                    ! Accumulate the weighted spectral components
                    gTransArr%GcZ(K + 1) = gTransArr%GcZ(K + 1) + CMPLX( &
                        temp_calc * trigTableType%value(index) * sin_ly, &
                        -temp_calc * trigTableType%value(index + 1) * sin_ly, &
                        kind=C_FLOAT_COMPLEX)
                end do
            end do

            ! 3. Transform the accumulated 1D row
            call fftwf_execute_dft_c2r(plan_x_c2r_1d, gTransArr%GcZ, gTransArr%GZonal)
            gTransArr%GZonal(IXMAX + 1) = gTransArr%GZonal(1)
            gTransArr%GZonal(1 : IXMAX + 1) = gTransArr%GZonal(1 : IXMAX + 1) + zonalMean
        end subroutine loadLatValues


! PLOT HOVMOLLER AT LONGITUDE -------------------------------------------
        subroutine plotLonHovmoller(lonHovmoller, invalidate)
            USE IFWIN
            USE GDI32
            USE USER32
            USE, INTRINSIC :: iso_c_binding, only: c_intptr_t, c_loc
            use ArrayDimensionsModule, only: gCurrentDims
            use ConstantsModule, only: SQRT22
            use NLevQGModelGlobals, only: gBitMapInfo, ghwndMain
            use ModelFieldsModule, only: gTransArr, gTransArr
            use ModelParamsModule, only: gModIntParams
            use Fftw3HelpersModule, only: norm_cosly, plan_y_FFTW_REDFT01, plan_y_FFTW_RODFT01

            IMPLICIT NONE

            include 'fftw3.f03'

            type(Hovmoller), intent(inout) :: lonHovmoller  ! Need the out as the 'row' is kept track of here.
                                                            ! However, row is really the column
            LOGICAL, intent(in) :: invalidate

            LOGICAL(BOOL) :: bret
            REAL, save, ALLOCATABLE, TARGET :: X(:, :), X_Overlay(:, :)  ! Created on the heap
            REAL, POINTER :: Xptr(:, :)
            integer(RGB32), pointer :: dibArray(:,:)

            integer(RGB32), target :: Pixels(gBitMapInfo%height)
            integer ::  iVal, iValDown, iValLeft, index, K, L, offset, colorIdx, colorToDraw, KMAX, LMAX, currentColumn, prevColumn


            type(InterpolationBounds) b
            
            real :: temp_calc, cos_kx, sin_kx
            real :: wave_real, wave_imag

            call loadFields(lonHovmoller%value, lonHovmoller%field, gModIntParams%LEVP)
            KMAX = gModIntParams%KMAX2
            LMAX = gModIntParams%LMAX2
            
            ! 1. The cosLy components
            ! These are k=0, so they have no x-dependence. 
            ! Put them straight into the cosine buffer.
            gTransArr%GZonal = 0
            gTransArr%GZonal(2:LMAX+1) = lonHovmoller%value(1:LMAX) * lonHovmoller%scale * SQRT22


            temp_calc = 2. * norm_cosly * lonHovmoller%scale ! Do the normalization first so we dont have to normalize after the transform

            ! 2. The Waves (sinLy components)
            gTransArr%G2Zonal = 0 ! Zero this array
            
            ! 1. Precompute combined phase/normalization factor for each K
            ! This moves the K-loop's trigonometry out of the L-loop bottleneck
            do K = 1, KMAX
                wave_real = temp_calc * lonHovmoller%cosx(K)
                wave_imag = temp_calc * lonHovmoller%sinx(K)
    
                index = LMAX + 2 * LMAX * (K - 1) + 1
    
                ! 2. Vectorized slice operation
                ! Instead of looping L=1 to LMAX, use stride-2 slices to pick out Real and Imag parts
                gTransArr%G2Zonal(1:LMAX) = gTransArr%G2Zonal(1:LMAX) + &
                    (lonHovmoller%value(index : index + 2*LMAX - 2 : 2) * wave_real) - &
                    (lonHovmoller%value(index + 1 : index + 2*LMAX - 1 : 2) * wave_imag)
            end do

            ! 3. Transform to Grid
            ! Background flow (using REDFT01 for Cosine)
            call fftwf_execute_r2r(plan_y_FFTW_REDFT01, gTransArr%GZonal, gTransArr%GZonal) 

            ! Waves (using RODFT01 for Sine)
            ! We use a temp buffer so we can add them to the background
            call fftwf_execute_r2r(plan_y_FFTW_RODFT01, gTransArr%G2Zonal, gTransArr%G2Zonal)
            ! 4. Combine
            gTransArr%GZonal(1:gCurrentDims%IYMAX) = gTransArr%GZonal(1:gCurrentDims%IYMAX) + gTransArr%G2Zonal(1:gCurrentDims%IYMAX)
            
            if (lonHovmoller%useSolidColor) then
                if (.not. allocated(X)) allocate(X(gBitMapInfo%height, 3))
                Xptr => X        ! Note the pointer and not the equals! Not like C!
            else
                if (.not. allocated(X_Overlay)) allocate(X_Overlay(gBitMapInfo%height, 3))
                Xptr => X_Overlay
            endif

            b%il = 1
            b%iu = gCurrentDims%IYMAX
            b%KL = 1
            b%KU = gBitMapInfo%height
            currentColumn = MOD(lonHovmoller%row, 3) + 1
            call Interpolate1D(gTransArr%GZonal, b, .false., Xptr(:, currentColumn)) ! is NOT periodic
            
            ! Get access to the DIBSection 
            call C_F_POINTER(gBitMapInfo%pBits, dibArray, [gBitMapInfo%width, gBitMapInfo%height])
            if (lonHovmoller%useSolidColor) then
                ! Turn X into an integer array of 32-pixel values
                do K = 1, gBitMapInfo%height
                    ! Calculate value based on the contour interval
                    iVal = FLOOR(Xptr(K, currentColumn) * lonHovmoller%contourIntervalInv) + NUM_RAM_COLORS / 2
                    ! Cycle through the colorRam if iVal exceeds its size
                    ! Use ABS to handle negative iVal if necessary
                    ! Modulo keeps the recylcing postive versus mod which does not
                    colorIdx = modulo(iVal, NUM_RAM_COLORS) + 1

                    Pixels(K) = RAM_COLORS(colorIdx)
                end do


                ! 2. Loop through 'Pixels' array (height) 
                ! and place each pixel into the DIBSection row-by-row
                do K = gBitMapInfo%height, 1, -1
                    dibArray(lonHovmoller%row + 1, gBitMapInfo%height - K + 1) = Pixels(K)
                end do
            else
                ! 2. Calculate borders by comparing current pixel to its neighbors.
                ! We stop at Height-1 to prevent "out of bounds" on the iVal checks.
                prevColumn = currentColumn - 1
                if (prevColumn < 1) prevColumn = 3
                do K = 1, gBitMapInfo%height - 1
        
                    ! J_data maps Screen Top (J=1) to Array North (Height)
                    L = gBitMapInfo%height - K + 1
                    ! J_next is the row physically below J on the screen (one step South in data)

                    ! Calculate interval bins
                    iVal      = floor(X_Overlay(L, currentColumn) * lonHovmoller%contourIntervalInv)
                    iValDown  = floor(X_Overlay(L - 1, currentColumn) * lonHovmoller%contourIntervalInv)
                    iValLeft  = floor(X_Overlay(L, prevColumn) * lonHovmoller%contourIntervalInv)
        
                    ! Detect boundary
                    if (iVal /= iValDown) then
                        colorIdx = max(iVal, iValDown)    ! reuse colorIdx for the max level
                        if (colorIdx > 0) then
                            colorToDraw = z'00DD0077' ! Reddish-pink
                        else if (colorIdx < 0) then
                            colorToDraw = z'000000FF' ! Blue
                        else 
                            colorToDraw = z'00000000' ! White for the 0-contour
                        end if
                            
                        ! J=1 is the top row of the pixelArray
                        dibArray(lonHovmoller%row + 1, K) = colorToDraw
                    end if
                    if (iVal /= iValLeft .AND. lonHovmoller%row > 0) then
                        colorIdx = max(iVal, iValLeft)    ! reuse colorIdx for the max level
                        if (colorIdx > 0) then
                            colorToDraw = z'00DD0077' ! Reddish-pink
                        else if (colorIdx < 0) then
                            colorToDraw = z'000000FF' ! Blue
                        else 
                            colorToDraw = z'00000000' ! White for the 0-contour
                        end if
                        ! J=1 is the top row of the pixelArray

                        dibArray(lonHovmoller%row + 1, K) = colorToDraw
                    end if
                end do
            endif

            lonHovmoller%row = mod(lonHovmoller%row + 1, gBitMapInfo%width)
            call labelPlotsAndDisplayTime(.false.)
            call DrawHorizontalLines(.true., .true., #00646464) ! True invalidates window and forces redraw

        end subroutine plotLonHovmoller
        
! PLOT VERTICAL CROSS SECTION AT LONGITUDE -------------------------------------------
        subroutine plotLonVertCrossSec(lonHovmoller, invalidate)
            USE IFWIN
            USE GDI32
            USE USER32
            USE, INTRINSIC :: iso_c_binding, only: c_intptr_t, c_loc
            use ArrayDimensionsModule, only: gCurrentDims
            use ConstantsModule, only: SQRT22
            use NLevQGModelGlobals, only: gBitMapInfo, ghwndMain
            use ModelFieldsModule, only: gTransArr, gTransArr
            use ModelParamsModule, only: gModIntParams
            use Fftw3HelpersModule, only: norm_cosly, plan_y_FFTW_REDFT01, plan_y_FFTW_RODFT01

            IMPLICIT NONE

            include 'fftw3.f03'

            type(Hovmoller), intent(inout) :: lonHovmoller  ! Need the out as the 'row' is kept track of here.
                                                            ! However, row is really the column
            LOGICAL, intent(in) :: invalidate

            LOGICAL(BOOL) :: bret
            REAL, save, ALLOCATABLE, TARGET :: X(:, :), X_Overlay(:, :)  ! Created on the heap
            REAL, POINTER :: Xptr(:, :)
            integer(RGB32), pointer :: dibArray(:,:)

            integer(RGB32), target :: Pixels(gBitMapInfo%height)
            integer ::  iVal, iValDown, iValLeft, index, K, L, offset, colorIdx, colorToDraw, KMAX, LMAX, currentColumn, prevColumn


            type(InterpolationBounds) b
            
            real :: temp_calc, cos_kx, sin_kx
            real :: wave_real, wave_imag
            return

            call loadFields(lonHovmoller%value, lonHovmoller%field, gModIntParams%LEVP)
            KMAX = gModIntParams%KMAX2
            LMAX = gModIntParams%LMAX2
            
            ! 1. The cosLy components
            ! These are k=0, so they have no x-dependence. 
            ! Put them straight into the cosine buffer.
            gTransArr%GZonal = 0
            gTransArr%GZonal(2:LMAX+1) = lonHovmoller%value(1:LMAX) * lonHovmoller%scale * SQRT22


            temp_calc = 2. * norm_cosly * lonHovmoller%scale ! Do the normalization first so we dont have to normalize after the transform

            ! 2. The Waves (sinLy components)
            gTransArr%G2Zonal = 0 ! Zero this array
            
            ! 1. Precompute combined phase/normalization factor for each K
            ! This moves the K-loop's trigonometry out of the L-loop bottleneck
            do K = 1, KMAX
                wave_real = temp_calc * lonHovmoller%cosx(K)
                wave_imag = temp_calc * lonHovmoller%sinx(K)
    
                index = LMAX + 2 * LMAX * (K - 1) + 1
    
                ! 2. Vectorized slice operation
                ! Instead of looping L=1 to LMAX, use stride-2 slices to pick out Real and Imag parts
                gTransArr%G2Zonal(1:LMAX) = gTransArr%G2Zonal(1:LMAX) + &
                    (lonHovmoller%value(index : index + 2*LMAX - 2 : 2) * wave_real) - &
                    (lonHovmoller%value(index + 1 : index + 2*LMAX - 1 : 2) * wave_imag)
            end do

            ! 3. Transform to Grid
            ! Background flow (using REDFT01 for Cosine)
            call fftwf_execute_r2r(plan_y_FFTW_REDFT01, gTransArr%GZonal, gTransArr%GZonal) 

            ! Waves (using RODFT01 for Sine)
            ! We use a temp buffer so we can add them to the background
            call fftwf_execute_r2r(plan_y_FFTW_RODFT01, gTransArr%G2Zonal, gTransArr%G2Zonal)
            ! 4. Combine
            gTransArr%GZonal(1:gCurrentDims%IYMAX) = gTransArr%GZonal(1:gCurrentDims%IYMAX) + gTransArr%G2Zonal(1:gCurrentDims%IYMAX)
            
            if (lonHovmoller%useSolidColor) then
                if (.not. allocated(X)) allocate(X(gBitMapInfo%height, 3))
                Xptr => X        ! Note the pointer and not the equals! Not like C!
            else
                if (.not. allocated(X_Overlay)) allocate(X_Overlay(gBitMapInfo%height, 3))
                Xptr => X_Overlay
            endif

            b%il = 1
            b%iu = gCurrentDims%IYMAX
            b%KL = 1
            b%KU = gBitMapInfo%height
            currentColumn = MOD(lonHovmoller%row, 3) + 1
            call Interpolate1D(gTransArr%GZonal, b, .false., Xptr(:, currentColumn)) ! is NOT periodic
            
            ! Get access to the DIBSection 
            call C_F_POINTER(gBitMapInfo%pBits, dibArray, [gBitMapInfo%width, gBitMapInfo%height])
            if (lonHovmoller%useSolidColor) then
                ! Turn X into an integer array of 32-pixel values
                do K = 1, gBitMapInfo%height
                    ! Calculate value based on the contour interval
                    iVal = FLOOR(Xptr(K, currentColumn) * lonHovmoller%contourIntervalInv) + NUM_RAM_COLORS / 2
                    ! Cycle through the colorRam if iVal exceeds its size
                    ! Use ABS to handle negative iVal if necessary
                    ! Modulo keeps the recylcing postive versus mod which does not
                    colorIdx = modulo(iVal, NUM_RAM_COLORS) + 1

                    Pixels(K) = RAM_COLORS(colorIdx)
                end do


                ! 2. Loop through 'Pixels' array (height) 
                ! and place each pixel into the DIBSection row-by-row
                do K = gBitMapInfo%height, 1, -1
                    dibArray(lonHovmoller%row + 1, gBitMapInfo%height - K + 1) = Pixels(K)
                end do
            else
                ! 2. Calculate borders by comparing current pixel to its neighbors.
                ! We stop at Height-1 to prevent "out of bounds" on the iVal checks.
                prevColumn = currentColumn - 1
                if (prevColumn < 1) prevColumn = 3
                do K = 1, gBitMapInfo%height - 1
        
                    ! J_data maps Screen Top (J=1) to Array North (Height)
                    L = gBitMapInfo%height - K + 1
                    ! J_next is the row physically below J on the screen (one step South in data)

                    ! Calculate interval bins
                    iVal      = floor(X_Overlay(L, currentColumn) * lonHovmoller%contourIntervalInv)
                    iValDown  = floor(X_Overlay(L - 1, currentColumn) * lonHovmoller%contourIntervalInv)
                    iValLeft  = floor(X_Overlay(L, prevColumn) * lonHovmoller%contourIntervalInv)
        
                    ! Detect boundary
                    if (iVal /= iValDown) then
                        colorIdx = max(iVal, iValDown)    ! reuse colorIdx for the max level
                        if (colorIdx > 0) then
                            colorToDraw = z'00DD0077' ! Reddish-pink
                        else if (colorIdx < 0) then
                            colorToDraw = z'000000FF' ! Blue
                        else 
                            colorToDraw = z'00000000' ! White for the 0-contour
                        end if
                            
                        ! J=1 is the top row of the pixelArray
                        dibArray(lonHovmoller%row + 1, K) = colorToDraw
                    end if
                    if (iVal /= iValLeft .AND. lonHovmoller%row > 0) then
                        colorIdx = max(iVal, iValLeft)    ! reuse colorIdx for the max level
                        if (colorIdx > 0) then
                            colorToDraw = z'00DD0077' ! Reddish-pink
                        else if (colorIdx < 0) then
                            colorToDraw = z'000000FF' ! Blue
                        else 
                            colorToDraw = z'00000000' ! White for the 0-contour
                        end if
                        ! J=1 is the top row of the pixelArray

                        dibArray(lonHovmoller%row + 1, K) = colorToDraw
                    end if
                end do
            endif

            lonHovmoller%row = mod(lonHovmoller%row + 1, gBitMapInfo%width)
            call labelPlotsAndDisplayTime(.false.)
            call DrawHorizontalLines(.true., .true., #00646464) ! True invalidates window and forces redraw

        end subroutine plotLonVertCrossSec


! PLOT WEATHER MAP -------------------------------------------
        ! @param Y: the spectral components array
        ! @param invalidate: if true, invalidate rectangle and update to force screen redraw
        ! @param field: which field to plot, one of HEIGHT, TEMPERATURE, or NO_PLOT
        ! @param solidColor: if true, draw a solid color contour map, if false, draw line contours
        subroutine plotWeatherMap(invalidate, weatherMapType)
            USE IFWIN
            USE GDI32
            USE USER32
            USE, INTRINSIC :: iso_c_binding, only: c_intptr_t, c_loc, C_F_POINTER
            use Kernel32, only: RtlMoveMemory
            
            use ArrayDimensionsModule, only: gCurrentDims
            use ConstantsModule, only: PLOTTING_OFF, wp
            use NLevQGModelGlobals, only: gBitMapInfo, ghwndMain
            use ModelFieldsModule, only: gTransArr, gTransArr
            use ModelParamsModule, only: gModIntParams

            IMPLICIT NONE


            LOGICAL, intent(in) :: invalidate
            type(WeatherMap), intent(in) :: weatherMapType


            LOGICAL(BOOL)   :: bret
            REAL :: cntInt
            type (InterpolationBounds) b
        
            REAL, save, ALLOCATABLE, TARGET :: X(:,:), X_Overlay(:, :)  ! Created on the heap
            REAL, POINTER :: Xptr(:,:)
            integer(c_int), pointer :: pixelArray(:, :) ! Our map to the DIBSection
        
            integer(RGB32), target :: Pixels(gBitMapInfo%width)
            integer ::  iVal, iValRight, iValDown, i, j, J1, colorIdx
            
            integer(c_intptr_t) :: baseAddr, targetAddr, srcAddr
            integer(c_intptr_t) :: bytesPerRow
            integer(4) :: colorToDraw
            
            if (weatherMapType%field == PLOTTING_OFF) return    ! Do nothing
            
            call loadFields( weatherMapType%value, weatherMapType%field, gModIntParams%LEVP)
        
            ! This routine also dimensionalizes the values
            call createGrid(gTransArr%plotFull, weatherMapType)
            
            ! --- RESIZE LOGIC ---
            ! Check if X needs allocation or resizing based on the DIB size
            if (weatherMapType%useSolidColor) then
                if (.not. allocated(X)) then
                    allocate(X(gBitMapInfo%width, gBitMapInfo%height / 2))
                else if (size(X, 1) /= gBitMapInfo%width .or. size(X, 2) /= gBitMapInfo%height / 2) then ! size(X, 1) is size of first dimension
                    deallocate(X)
                    allocate(X(gBitMapInfo%width, gBitMapInfo%height / 2))
                end if
            end if
            ! Check if X_Overlay needs allocation or resizing based on the DIB size
            if (.NOT. weatherMapType%useSolidColor) then
                if (.not. allocated(X_Overlay)) then
                    allocate(X_Overlay(gBitMapInfo%width, gBitMapInfo%height / 2))
                else if (size(X_Overlay, 1) /= gBitMapInfo%width .or. size(X_Overlay, 2) /= gBitMapInfo%height / 2) then
                    deallocate(X_Overlay)
                    allocate(X_Overlay(gBitMapInfo%width, gBitMapInfo%height / 2))
                end if
            end if
        
            if (weatherMapType%useSolidColor) then
                Xptr => X        ! Note the pointer and not the equals! Not like C!
            else
                Xptr => X_Overlay
            endif
            ! Interpolate the grid array to a resolution that matches the size of the display of the
            ! weather map on the screen.
            b%il = 1
            b%iu = gCurrentDims%IXMAX + 1
            b%jl = 1
            b%ju = gCurrentDims%IYMAX/2 + 1
            b%KL = 1
            b%KU = gBitMapInfo%width
            b%LL = 1
            b%LU = gBitMapInfo%height / 2
            call Interpolate2D(gTransArr%plotFull, b, Xptr)
            ! However, we need to map these interpolated values in X to colors for each pixel. We will
            ! fill a row (Pixels) with pixel color values and then copy it to the DIBSection. This is
            ! a lot more efficient than creating a 2d array of color values and then writing that to
            ! the DIBSection.
            
            ! In order to make contours instead of color that continuously transition for each value,
            ! We create a color ram of 18 values and let the user determine the contour interval. 
            
            ! 1. Width * 4 bytes per pixel (32-bit DIBs have no padding)
            bytesPerRow = int(gBitMapInfo%width, c_intptr_t) * 4
            ! 2. Calculate base address
            ! y=0 is the top row because 'bmi%bmiHeader%biHeight' is negative
            baseAddr = transfer(gBitMapInfo%pBits, baseAddr)

            if (weatherMapType%useSolidColor) then
                ! Turn X into an integer array of 32-pixel values
                do J = gBitMapInfo%height / 2, 1, -1
                    J1 = gBitMapInfo%height / 2 - J + 1
                    do I = 1, gBitMapInfo%width
                        ! Calculate value based on the contour interval
                        iVal = FLOOR(Xptr(I, J1) * weatherMapType%contourIntervalInv) + NUM_RAM_COLORS / 2
                        ! Cycle through the colorRam if iVal exceeds its size
                        ! Use ABS to handle negative iVal if necessary
                        ! Modulo keeps the recycling positive versus mod which does not
                        colorIdx = modulo(iVal, NUM_RAM_COLORS) + 1
                        Pixels(I) = RAM_COLORS(colorIdx)
                    end do
                    ! 2. Calculate memory address for the desired row 'targetY'
                    ! y=0 is the top row because 'bmi%bmiHeader%biHeight' is negative
                    targetAddr = baseAddr + (int(J - 1, c_intptr_t) * bytesPerRow)

                    ! 3. Get the address of your source array
                    ! Pixels must have the TARGET attribute
                    srcAddr = transfer(c_loc(Pixels(1)), srcAddr)
        
                    ! 4. Move the interpolated array of Pixels into the DIBSection memory
                    ! Call the Intel-defined interface
                    ! Since the interface defines these as integers, pass them directly.
                    call RtlMoveMemory(targetAddr, srcAddr, bytesPerRow)
                end do
            else
                CALL C_F_POINTER(gBitMapInfo%pBits, pixelArray, [gBitMapInfo%width, gBitMapInfo%height])
                ! 2. Calculate borders by comparing current pixel to its neighbors.
                ! We stop at Width-1 and Height-1 to prevent "out of bounds" on the iVal checks.
                ! Mapping X_Overlay to the upper half of the screen
                do J = 1, (gBitMapInfo%height / 2) - 1
        
                    ! J_data maps Screen Top (J=1) to Array North (Height/2)
                    J1 = (gBitMapInfo%height / 2) - J + 1
                    ! J_next is the row physically below J on the screen (one step South in data)
                    !J_next = J_data - 1
        
                    do I = 1, gBitMapInfo%width - 1
        
                        ! Calculate interval bins
                        iVal      = floor(X_Overlay(I, J1) * weatherMapType%contourIntervalInv)
                        iValRight = floor(X_Overlay(I + 1, J1) * weatherMapType%contourIntervalInv)
                        iValDown  = floor(X_Overlay(I, J1 - 1) * weatherMapType%contourIntervalInv)
        
                        ! Detect boundary
                        if (iVal /= iValRight .or. iVal /= iValDown) then
                            colorIdx = max(iVal, iValRight, iValDown)    ! reuse colorIdx for the max level
                            if (colorIdx > 0) then
                                colorToDraw = z'00DD0077' ! Reddish-pink
                            else if (colorIdx < 0) then
                                colorToDraw = z'000000FF' ! Blue
                            else 
                                colorToDraw = z'00000000' ! White for the 0-contour
                            end if
                            
                            ! J=1 is the top row of the pixelArray
                            pixelArray(I, J) = colorToDraw
                        end if
                    end do
                end do
            endif
            call labelPlotsAndDisplayTime(.false.)
            call DrawGridLines(invalidate)
            !call inverseTransform(gTransArr%G1plot, weatherMapType)

        end subroutine plotWeatherMap

        ! This method populates a grid X of all the interpolated values that are displayed on the
        ! screen in weather maps. Recall the screen values are just ARGB color values. The array
        ! X contains the actual values for each pixel on the screen. This allows us to display the
        ! actual value of weather map when we pause the integration and mouse over the map.
        ! This routine is called from the main window callback when the user presses F1 while the
        ! model is integrating. This array is populated only once for every pause.
        subroutine populateInterpolatedGrid(X, field)
            use ConstantsModule, only: PLOTTING_OFF, DIAGNOSTIC, VERT_CROSS_SEC_AT_LAT
            use ArrayDimensionsModule, only: gCurrentDims
            use NLevQGModelGlobals, only: gBitMapInfo
            use ModelFieldsModule, only: gTransArr, gVertArr
            use ModelParamsModule, only: gModIntParams
            implicit none

            real, intent(out) :: X(:, :)
            integer, intent(in) :: field

            type(InterpolationBounds) b
            integer :: N
            type(VertCrossSection) :: xSecType
            
            if (field == PLOTTING_OFF) return

            if (gModIntParams%plotType == VERT_CROSS_SEC_AT_LAT) then
                xSecType = getLatCrossSectionType(field)
                gTransArr%plotFull = 0.0
                do N = 1, SIZE(gVertArr%LVS)
                    ! Populates gTransArr%GZonal with a row of transformed values at that level and latitude including the periodic IXMAX + 1
                    call loadLatValues(xSecType, N, xSecType%tempScales(N))
                    gTransArr%plotFull(1 : gCurrentDims%IXMAX + 1, N) = gTransArr%GZonal(1 : gCurrentDims%IXMAX + 1)
                end DO
            else
                if (field .NE. DIAGNOSTIC) call createGrid(gTransArr%plotFull, getWeatherMapType(field))
            endif
            ! Generate the interpolated 2D array X
            b%il = 1
            b%iu = gCurrentDims%IXMAX + 1
            b%jl = 1
            b%ju = gCurrentDims%IYMAX / 2 + 1
            b%KL = 1
            b%KU = gBitMapInfo%width
            b%LL = 1
            b%LU = gBitMapInfo%height / 2
            if (gModIntParams%plotType == VERT_CROSS_SEC_AT_LAT) then
                b%ju = SIZE(gVertArr%LVS)
                b%LU = gBitMapInfo%height
            endif
            call Interpolate2D(gTransArr%plotFull, b, X)   ! X is populated here
        
        end subroutine populateInterpolatedGrid

        
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