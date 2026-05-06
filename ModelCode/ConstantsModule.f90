
! These constants are used like C-enums to better describe what values certain parameters in this model can take.
! In the original f77 version of this code, the short names meant we often had expressions like
!    'ISTAT=1' which can now be expressed by 'statistisOptions = STATS_OPT_MEAN' which is much more descriptive
module ConstantsModule
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none

    public  ! Makes everything in the module public (unless otherwise specified)

    integer, parameter :: wp = real64 ! Double precision

    real, parameter :: PI = 4.*ATAN(1.)
    real, parameter :: PI_INVERSE = 1./PI
    real, parameter :: RAD_TO_DEGREES = 180./PI
    real, parameter :: SQRT2 = SQRT(2.)
    real, parameter :: SQRT22 = SQRT2/2.
   !--------------------------------Set scale factors:
    real, parameter :: CHANNEL_WIDTH = 5000000.*PI_INVERSE  ! 1591549.
    real, parameter :: CORIOLIS = 7.292 * SQRT2 / 100000   ! Omega is the Earth's angular velocity (= 7.292×10-5 radians per second)      
                                                ! f = 2*Omega*sin(45) = 1.03*10-4 rad/sec (note: 2 * SQRT22 = SQRT2) = .00010312445
    real, parameter :: CORIOLIS_INVERSE = 1./CORIOLIS       ! 9697.021
    real, parameter :: SECONDS_PER_DAY = 86400.
    real, parameter :: GAS_CONSTANT_R = 287.            ! Gas constant R in units of Joules/(kg·K)
    real, parameter :: GRAVITY = 9.8
    real, parameter :: CP = 1005.                  ! Heat capacity at constant pressure Joules/(kg·K)
    real, parameter :: KAPPA = GAS_CONSTANT_R / CP      ! 0.2855721

                                                     ! Options for 'useTransformMethod'
    LOGICAL, parameter :: USE_INTERACTION_COEFFICIENTS = .false. ! Used to set a boolean. When false, use interaction coefficients!
    LOGICAL, parameter :: USE_TRANSFORM = .true.    ! Used to set a boolean. When true, use spectral transform method
    integer, parameter :: FORWARD_TRANSFORM = 1     ! Do forward (spectral to grid) transform in FFT991 routine
    integer, parameter :: INVERSE_TRANSFORM = -1    ! Do inverse (grid to spectral) transform in FFT991 routine
    
    integer, parameter :: STOPPED = 1               ! Model is stopped. Waiting for the user to start it. Restarting will start from scratch.
    integer, parameter :: PAUSED  = 2               ! Model is paused. Waiting for the user to unpause.
    integer, parameter :: RUNNING = 3               ! Model is running
    
    integer, parameter :: ASCII_TO_REAL = 0         ! Flag to indicate convert a string to a real number in subroutine RealToAsciiToReal
    integer, parameter :: REAL_TO_ASCII = 1         ! Flag to indicate convert a real number to a string in subroutine RealToAsciiToReal
    
                                                    ! Options for 'statsOption'
    integer, parameter :: STATS_OPT_NONE = 0        ! Compute no statistics
    integer, parameter :: STATS_OPT_MEAN = 1        ! Compute averages and Lorenz energy cycle
    integer, parameter :: STATS_OPT_VAR = 2         ! Compute variances in addition to STATS_OPT_MEAN
    
    integer, parameter :: VERT_ARRAYS = 1           ! Vertical arrays allocated when isAlloced(VERT_ARRAYS]) is true
    integer, parameter :: HORZ_ARRAYS = 2           ! Horizontal arrays allocated when isAlloced(HORZ_ARRAYS] is true
    integer, parameter :: INTERACT_COEF_ARRAYS = 3  ! Interaction coefficient arrays allocated when isAlloced(INTERACT_COEF_ARRAYS] is true
    integer, parameter :: STATS_ARRAYS = 4          ! Statistics arrays allocated when isAlloced(STATS_ARRAYS] is true

                                                    ! Options for 'modelType'
    integer, parameter :: STANDARD_NLEVEL = 0       ! Equations are integrated as as a fully nonlinear Quasi-geostrophic standard N-Level model
    integer, parameter :: BAROCLINIC_STRUCTS = 1    ! Equations are linearized about a basic zonal state and integerated using the
                                                    !   amplitude-ratio (temperature/streamfunction), phase-difference approach. The use of the 
                                                    !   amplitude ratio means that growing disturbances do not blow up. This integration option
                                                    !   is good for studying for studing the nonmodal initial value problem.
    integer, parameter :: ZONAL_MEAN_FLOW = 2       ! Experimental mode where the waves are predicted barotropically and the baroclinicity is 
                                                    !   parameterized from vertical structures. Not fully thought out yet.
    
    !============================================================ PLOTTING =========================================================
                                                    !----- Is Temperature indicator:
                                                    ! This sequence is to 'match' the current variable to be displayed when
                                                    ! the user presses the 'field' key that gets entered into field1, field2, field3
                                                    ! for example, a value 1 is VORTICITY (see the Constants module). Value
                                                    ! 3 is temperature and thus the list below is set to .TRUE. for 'index' 3.
    logical, parameter :: isTemperature(28) = [.FALSE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE., .FALSE., .FALSE., .FALSE., .TRUE., .TRUE., &
                                               .TRUE., .TRUE., .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE., &
                                               .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., .FALSE.]
                                                    ! Map overlays for variable 'gPlotControls%activePlotMap'. The active map will be the map of three possible
                                                    ! maps that can be displayed one on top of the other whose features can be changed by the user 
    integer, parameter :: MAP_OVERLAY_1 = 1
    integer, parameter :: MAP_OVERLAY_2 = 2
    integer, parameter :: MAP_OVERLAY_3 = 3
    
                                                    ! Options for the types of displays for variable plotType
    integer, parameter :: WEATHER_MAP = 0           ! Classic weather maps at various levels
    integer, parameter :: PHASE_AND_MAP = 1         ! Both weather map and phase space diagram together
    integer, parameter :: HOV_DIAG_CROSS_SEC_AT_LAT = 2    ! Latitude-pressure cross section at a selected 'latitude'
    integer, parameter :: HOV_DIAG_AT_LAT = 3       ! Hovmoller diagram at specified 'latitude' and level
    integer, parameter :: HOV_DIAG_CROSS_SEC_AT_LONG = 4   ! Latitude-pressure (meridional) cross section at a slected 'longitude'
    integer, parameter :: HOV_DIAG_AT_LONG = 5      ! Hovmoller diagram at specified 'longitude' and level
    integer, parameter :: PHASE_SPACE = 6           ! Phase space diagram
    integer, parameter :: WAVE_AMP_TIME_SERIES = 7  ! Time series of a spectral wave ampltude at given levels
    integer, parameter :: WAVE_PHASE_TIME_SERIES = 8! Time series of a spectral wave phase at given levels
    integer, parameter :: WEATHER_STN = 9           ! Time series of a field at a particular point (lat-lon)
    integer, parameter :: NO_PLOT = 10              ! Skip plotting (good for speed)
    
                                                    ! Display options for variables field1, field2, field3
                                                    ! Also used for the scale factor array 'gPlotScales' below
                                                    ! Options 1-17 are for 2D contour maps
    integer, parameter :: FIELD_START = 1
    integer, parameter :: VORTICITY  = 1            ! Vorticity in 10**-5/s
    integer, parameter :: HEIGHT  = 2               ! Height in meters
    integer, parameter :: TEMPERATURE  = 3          ! Temperature in Centigrade
    integer, parameter :: VERTICAL_WIND  = 4        ! Vertical wind in mb/hr or mm/sec
    integer, parameter :: TEMP_FORCING  = 5         ! Temperature driving (forcing) - Radiative relaxation temperature in Centigrade
    integer, parameter :: TOT_HGT_TEND  = 6         ! Total Height tendency in meters/day
    integer, parameter :: ADV_BETA_HGT_TEND  = 7    ! Height tendencies in m/day from Advection plus beta
    integer, parameter :: OMEGA_HGT_TEND  = 8       ! Height tendencies in m/day from omega (dp/dt)
    integer, parameter :: DIABATIC_HGT_TEND  = 9    ! Height tendencies in m/day from Diabatic effects 
    integer, parameter :: TOT_TEMP_TEND  = 10       ! Total temperture tendencies in C/day
    integer, parameter :: ADV_TEMP_TEND  = 11       ! Temperature tendencies in C/day from advection
    integer, parameter :: OMEGA_TEMP_TEND  = 12     ! Temperature tendencies in C/day from omega
    integer, parameter :: DIABATIC_TEMP_TEND  = 13  ! Temperature tendencies in C/day from Diabatic effects
    integer, parameter :: HGT_FORCING = 14          ! Mean streamfunction (height) forcing in m
    integer, parameter :: OROG_HGT_TEND = 15        ! Height tendencies from ororgraphy in m/day'/
    integer, parameter :: OROGRAPHY = 16            ! Bottom orography in meters
    integer, parameter :: PLOTTING_OFF = 17         ! Plotting off

    ! Options 18-25 are for graphs (where points are plotted as a function of time) only
    integer, parameter :: FIELD_GRAPH_START = 18
    integer, parameter :: TOT_HGT_TEND_GRAF = 18    ! Total Height tendencies in m/day
    integer, parameter :: TOT_TEMP_TEND_GRAF = 19   ! Total temperature tendencies in C/day
    integer, parameter :: MEAN_HGT_TEND_GRAF = 20   ! Mean height tendencies in m/day
    integer, parameter :: MEAN_TEMP_TEND_GRAF = 21  ! Mean temperature tendencies in C/day
    integer, parameter :: HGT_WAVE_SPEED_GRAF = 22  ! Height wave ground speed in m/sec
    integer, parameter :: HGT_TEMP_PHASE_DIFF = 23  ! Phase difference in degrees between height and temperature wave
    integer, parameter :: GROWTH_RATE_GRAF = 24     ! Growth rate of a wave in e-folds per day
    integer, parameter :: AMPLITUDE_RATIO_GRAF = 25 ! Amplitude ratio of the temperature to Streamfunction component of a wave
    
    integer, parameter :: PLOT_SCALES_SIZE = 25     ! Size of the scaling factors array for plotting
    
    integer, parameter :: ZLEVEL_1          = 28    ! (ZLEV)Not exactly sure what these are, but appear to be levels of some plot parameter
    integer, parameter :: ZLEVEL_2          = 29    ! (ZLV2)
    integer, parameter :: ZLEVEL_3          = 30    ! (ZLV3)
    integer, parameter :: MIN_ASXY_SIZE = 3721      ! Minimum size for ASXY plotting arrays

    
    !============================================================ FIELD TERMS ========================================================= 
    ! These constants are to make indicies for arrays easier to understand and values of flags more descriptive.
    
                                                    ! Temperature type to plot for 'gPlotControls%plotTempType'
    integer, parameter :: POTENTIAL_TEMP = 0        ! Plot potential temperature (T) in deg C or K
    integer, parameter :: STANDARD_TEMP = 1         ! Plot standard temperature (T) in deg C
    
                                                    ! For variable 'gPlotControls%vertMotionType'
    integer, parameter :: OMEGA = 0                 ! Plot vertical motion as omega (dp/dt)
    integer, parameter :: VERT_WIND = 1             ! Plot vertical motion as vertical wind (mm/sec)
    
    integer, parameter :: INVALID = -1              ! Indicates and invalid value for a positive integer value, or file handles

    !============================= PARAMETERS to vary in Sensitivity experiments
    integer, parameter :: SENS_CHANNEL_LENGTH           = 1 ! Vary the channel length
    integer, parameter :: SENS_BETA                     = 2 ! Vary the beta effect
    integer, parameter :: SENS_STATIC_LAPSE_RATE        = 3 ! Vary the vertical static stability lapse rate (not sure how this is done in an N > 2-layer setup)
    integer, parameter :: SENS_EKMAN_DISSIPATION        = 4 ! Vary the ekman dissipation (not sure how this is done in an N > 2-layer setup)
    integer, parameter :: SENS_EKMAN_CONVECTIVE_MIXING  = 5 ! Vary the ekman between layer mixing. Again not sure how this is done in an N > 2-layer setup
    integer, parameter :: SENS_NEWTONIAN_COOLING        = 6 ! Vary the newtonian cooling (not sure how this is done in an N > 2-layer setup)
    integer, parameter :: SENS_THERM_CONVECTIVE_MIXING  = 7 ! Vary the newtonian between layer mixing (not sure how this is done in an N > 2-layer setup)
    integer, parameter :: SENS_HEIGHTS                  = 8 ! Not sure what this means
    integer, parameter :: SENS_TEMPERATURE_DRIVING      = 9 ! Vary the thermal driving (not sure how this is done in an N > 2-layer setup)
    integer, parameter :: SENS_HEIGHTS_DRIVING          = 10 ! Vary the streamfunction driving (not sure how this is done in an N > 2-layer setup)
    integer, parameter :: SENS_OROGRAPHIC_HEIGHT        = 11 ! Vary the orographic height
    
                        ! parameter values of 21-31 do the same as above but each experiment starts with the same initial conditions. I am guessing that
                        ! the the others begin with an initial condition that is that which is left from the previous run. I can't see how this would make 
                        ! any difference unless there are multiple stable quasi-equilibria.


    
    !============================= ERROR CODES
    integer, parameter :: ERR_SCALING_PARAMETER_ZERO = -24  ! One of the model scaling parameters is zero. Can lead to divide by zero.
      

end module ConstantsModule