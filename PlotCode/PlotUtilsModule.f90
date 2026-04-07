module PlotUtilsModule
    implicit none
    
    ! functions and subroutines for plotting utilities
    public :: GetFont, GetPlotWindowSize
    public :: SetupMapRegions

    contains

        !> ================== getFont ==================
        !> This function creates a font with the specified name and size.
        !! @param direction Direction in whole (not 10ths) degrees to rotate the text
        !! @param fontName Name of the font to create, e.g., "Times New Roman"
        !! @param fontSize Size of the font in logical units
        !! @return fontParams. Will have no handle if creation failed.
        function GetFont(direction, fontName, fontSize, hWnd) result(fontParams)
            use IFWINTY
            use IFWIN
            use gdi32
            use PlotParamsModule, only: FontParameters

            IMPLICIT NONE

            INTEGER, INTENT(IN) :: direction        ! Rotation angle in degrees
            character(*), intent(in) :: fontName    ! Font name, e.g., "Times New Roman"
            integer(LONG), INTENT(IN) :: fontSize   ! Size of the text in logical units
            integer(HANDLE), intent(IN) :: hWnd     ! Window handle for the device context

            TYPE(T_LOGFONT) :: lf
            
            type(FontParameters) :: fontParams
            integer(BOOL) :: bret  ! Boolean return value
            integer(HANDLE) :: hDC, hOldFont  ! Device context handle and font handle
            type(T_TEXTMETRIC) :: tm

            fontParams%direction = direction
            fontParams%fontName = fontName
            fontParams%fontSize = fontSize

            ! Define font properties
            lf%lfHeight = -fontParams%fontSize  ! Font height (negative for logical units)
            lf%lfWidth = 0
            lf%lfEscapement = fontParams%direction * 10 ! Rotation angle in tenths of a degree
            lf%lfOrientation = lf%lfEscapement
            lf%lfWeight = FW_NORMAL
            lf%lfItalic = 0
            lf%lfUnderline = 0
            lf%lfStrikeOut = 0
            lf%lfCharSet = ANSI_CHARSET
            lf%lfOutPrecision = OUT_TT_PRECIS
            lf%lfClipPrecision = CLIP_DEFAULT_PRECIS
            lf%lfQuality = DEFAULT_QUALITY
            lf%lfPitchAndFamily = DEFAULT_PITCH
            lf%lfFaceName = fontParams%fontName // CHAR(0)  ! Font name (up to 32 characters)

            ! Create the font (returns handle)
            fontParams%hFont = CreateFontIndirect(lf)
            ! Get metrics for the font
            hDC = GetDC(hWnd)
            IF (hDC == 0) return
            hOldFont = SelectObject(hDC, fontParams%hFont)
            bret = GetTextMetrics(hDC, tm)
            hOldFont = SelectObject(hDC, hOldFont)
            bret = ReleaseDC(hWnd, hDC)
            fontParams%tm = tm


        end function GetFont

        !> This function initializes the global variables for the plotting window.
        !! THe globals are stored in this module (PboxModule)
        !! @param hwnd Window handle
        function GetPlotWindowSize(hwnd) result(message)
            use PlotParamsModule
            use IFWINTY
            use IFWIN
            
            IMPLICIT NONE

            INTEGER(HANDLE), INTENT(IN) :: hwnd  ! Window handle

            TYPE(T_RECT) :: clientRect
            CHARACTER(128) :: message
            integer(BOOL) :: bret  ! Boolean return value
        
            ! Initialize global variables
            bret = GetClientRect(hWnd, clientRect)
            IF (bret) THEN
                drawParams%IXRES = clientRect%right - clientRect%left
                drawParams%IYRES = clientRect%bottom - clientRect%top
            ENDIF
        
            message = "OK" // CHAR(0)
        END function GetPlotWindowSize

        SUBROUTINE ScreenBuffer()
            use PlotParamsModule, only : drawParams
        
            implicit none
        
            integer :: K, N
            !-----------------------------------------Get screen buffer sizes:
            K = drawParams%KU - drawParams%KL + 1
            N = drawParams%LU - drawParams%LL + 3
            drawParams%KIAT = K
            IF(N .GT. K) drawParams%KIAT = N
            drawParams%KIA = K * N + 3
        END subroutine ScreenBuffer

        !  Subroutine OUTPUT which sets up the frame and labels for NLEVELQG
        !  output so one can page through the 5 types of output (given by MPLOT)
        !
        !SUBROUTINE OUTPUT(IFIRST, fontParams, hWnd)
        !    use ifwinty, only: HANDLE, SINT
        !    use ConstantsModule, only: TIME_STEP, PLOT_OFF, VORTICITY, LAT_PRES_CROSS_SEC, LONG_PRES_CROSS_SEC, HOV_DIAG_AT_LAT, HOV_DIAG_AT_LONG
        !    use PlotParamsModule
        !    use ModelParamsModule, only: modIntParams, modRealParams, scales
        !    use GSUBSModule
        !
        !    implicit none
        !
        !    INTEGER, intent(in) :: IFIRST
        !    type(FontParameters), intent(in) :: fontParams  ! Font parameters
        !    integer(HANDLE), intent(in) :: hWnd  ! Window handle
        !
        !    ! Local variables
        !    INTEGER IBDR, NN, N1, JJ
        !    INTEGER I, J, K, L, M, N
        !
        !    !--------------------------------Heres a common block for printing PNTEXT
        !    !  Here we make use of the fact that most calls to PNTEXT have the same
        !    !  IDIR,ISPACE,ICTXT, and ICHAR.  We have ID,IE in common to the subroutine
        !    !  PTEXT which just the text string is passed.
        !    !
        !    INTEGER IDIR, ID, IE, ICHAR
        !
        !    drawParams%MU = drawParams%KU
        !    drawParams%ML = drawParams%KL
        !    drawParams%NU = drawParams%LU
        !    drawParams%NL = drawParams%LL
        !    !
        !    !  IFIRST=Indicator
        !    !    0=
        !    !----------------------------------------------Message boxes:
        !    !  IB gives the (IX1,IY1,IX2,IY2) coordinates of the message boxes.
        !    !  MPLOT=Indicates what to plot:
        !    !  IPLT1,IPLT2,IPLT3=Indicates which quantity is to be plotted:
        !    !-------------------------------------------Go to 10 if just changing var.:
        !    IBDR = 10   ! Border width of the frame
        !    IDIR = 0    ! Text direction (0 is normal)
        !    IF(IFIRST .EQ. 0 .OR. IFIRST .GE. 3) GO TO 10
        !    IF(IFIRST .EQ. 2) GO TO 20
        !    !------------------------------------- (Re)draw screen:
        !    message = PCLEAR(drawParams%ICS, hWnd)
        !    !=================================================Write out help screen:
        !    IF(drawParams%KL .GT. 20 * fontParams%tm%tmAveCharWidth) THEN   ! Average character width
        !        !----------------------------------------------Action for MAP integrations:
        !        !IF(modIntParams%MPLOT .LE. HOV_DIAG_AT_LONG) CALL HLPSCN(MSGHI(1, 1))
        !        !----------------------------------------------Action for GRAPHS integrations:
        !        !IF(modIntParams%MPLOT .GE. PHASE_SPACE) CALL HLPSCN(MSGHI(1,26))
        !    ENDIF
        !    !----------------------------------------------Draw Plot window border:
        !    CALL FRAME(-fontParams%tm%tmHeight, drawParams%KL, drawParams%LL, drawParams%KU, drawParams%LU, hWnd)
        !    !=================================================Set up weather maps:
        !    IF(modIntParams%MPLOT .LE. HOV_DIAG_AT_LONG) THEN
        !        IF(modIntParams%IPLT1 .GT. PLOT_OFF) modIntParams%IPLT1 = VORTICITY
        !        CALL SETUP(modIntParams%NUMBX, modIntParams%NUMBY, modIntParams%MPLOT)
        !        !---------------------------------------Give axes values for 2-D maps:
        !        !  The map ranges from 0 to 360 degrees in x and 0 to 90 degrees in y:
        !        !
        !        axesParams%XL = 0.
        !        axesParams%XU = 360.
        !        axesParams%YL = 0.
        !        axesParams%YU = 90.
        !        IF(modIntParams%MPLOT .GE. LAT_PRES_CROSS_SEC) THEN
        !            axesParams%YL = modRealParams%BOT
        !            axesParams%YU = modRealParams%TPP
        !        ENDIF
        !        IF(modIntParams%MPLOT .EQ. LONG_PRES_CROSS_SEC) axesParams%XU = 90.
        !        IF(modIntParams%MPLOT .EQ. HOV_DIAG_AT_LAT .OR. modIntParams%MPLOT .EQ. HOV_DIAG_AT_LONG) THEN
        !            axesParams%YU = REAL((drawParams%NU - drawParams%NL + 1) * modIntParams%MOUT) * modRealParams%DELT * scales%timeStep / 1440.
        !            axesParams%YL = 0.
        !        ENDIF
        !        XINT = (axesParams%XU - axesParams%XL) / 10.
        !        YINT = (axesParams%YU - axesParams%YL) / 10.
        !    ENDIF
        !end subroutine OUTPUT

        !SUBROUTINE OUTPUT(IFIRST, fontParams, hWnd)
        !    use ifwinty, only: HANDLE, SINT
        !    use ConstantsModule, only: TIME_STEP, PLOT_OFF, VORTICITY, LAT_PRES_CROSS_SEC, LONG_PRES_CROSS_SEC, HOV_DIAG_AT_LAT, HOV_DIAG_AT_LONG, &
        !                               WAVE_PHASE_TIME_SERIES, WAVE_AMP_TIME_SERIES, OROG_HGT_TEND, OROGRAPHY, isTemperature
        !    use PlotParamsModule, only: drawParams, axesParams, FontParameters
        !    use ModelParamsModule, only: modIntParams, modRealParams, scales, modPlotParams, modScalingParams, pltControls
        !    use PboxModule, only: PCLEAR, PNTEXT, PBOX, PLINE
        !
        !    implicit none
        !
        !    INTEGER, intent(in) :: IFIRST
        !    type(FontParameters), intent(in) :: fontParams  ! Font parameters
        !    integer(HANDLE), intent(in) :: hWnd  ! Window handle
        !
        !    ! Local variables
        !    INTEGER(SINT) :: IBDR, NN, N1, JJ
        !    INTEGER :: I, J, K, L, M, N
        !    CHARACTER(128) :: errMsg
        !
        !    !--------------------------------Heres a common block for printing PNTEXT
        !    !  Here we make use of the fact that most calls to PNTEXT have the same
        !    !  IDIR,ISPACE,ICTXT, and ICHAR.  We have ID,IE in common to the subroutine
        !    !  PTEXT is now replaced by doing PNTEXT in-line.
        !
        !    INTEGER(SINT) IDIR, ID, IE, ICHAR
        !
        !    drawParams%MU = drawParams%KU
        !    drawParams%ML = drawParams%KL
        !    drawParams%NU = drawParams%LU
        !    drawParams%NL = drawParams%LL
        !
        !    !  IFIRST=Indicator
        !    !    0=
        !    !    1=
        !    !    2=
        !    !    3=
        !    !----------------------------------------------Message boxes:
        !    !  IB gives the (IX1,IY1,IX2,IY2) coordinates of the message boxes.
        !    !  MPLOT=Indicates what to plot:
        !    !  IPLT1,IPLT2,IPLT3=Indicates which quantity is to be plotted:
        !    !-------------------------------------------Go to 10 if just changing var.:
        !    IBDR = 10
        !    IDIR = 0
        !    IF(IFIRST .NE. 2) THEN
        !        IF(IFIRST .GT. 0 .AND. IFIRST .LT. 3) THEN
        !
        !            !------------------------------------- (Re)draw screen:
        !            errMsg = PCLEAR(drawParams%ICS, hWnd)
        !            !=================================================Write out help screen:
        !            IF(drawParams%KL .GT. 20 * fontParams%tm%tmAveCharWidth) THEN   ! Replaces ISPACE
        !                !----------------------------------------------Action for MAP integrations:
        !                !IF(MPLOT .LE. 4) CALL HLPSCN(MSGHI(1,1))
        !                !----------------------------------------------Action for GRAPHS integrations:
        !                !IF(MPLOT .GE. 5) CALL HLPSCN(MSGHI(1,26))
        !            END IF
        !            !----------------------------------------------Draw Plot window border:
        !            CALL FRAME(-fontParams%tm%tmHeight, drawParams%KL, drawParams%LL, drawParams%KU, drawParams%LU, hWnd) ! Replaces IHGHT
        !            !=================================================Set up weather maps:
        !            IF(modIntParams%MPLOT .LE. HOV_DIAG_AT_LONG) THEN
        !                IF(modIntParams%IPLT1 .GT. PLOT_OFF) modIntParams%IPLT1 = VORTICITY
        !                CALL SetupMapRegions()
        !                !---------------------------------------Give axes values for 2-D maps:
        !                !  The map ranges from 0 to 360 degrees in x and 0 to 90 degrees in y:
        !                axesParams%XL = 0.
        !                axesParams%XU = 360.
        !                axesParams%YL = 0.
        !                axesParams%YU = 90.
        !                IF(modIntParams%MPLOT .GE. LAT_PRES_CROSS_SEC) THEN
        !                    axesParams%YL = modRealParams%BOT
        !                    axesParams%YU = modRealParams%TPP
        !                END IF
        !                IF(modIntParams%MPLOT .EQ. LONG_PRES_CROSS_SEC) axesParams%XU = 90.
        !                IF(modIntParams%MPLOT .EQ. HOV_DIAG_AT_LAT .OR. modIntParams%MPLOT .EQ. HOV_DIAG_AT_LONG) THEN
        !                    axesParams%YU = REAL((drawParams%NU - drawParams%NL + 1) * modIntParams%MOUT) * modRealParams%DELT * scales%timeStep / 1440.
        !                    axesParams%YL = 0.
        !                END IF
        !                axesParams%XINT = (axesParams%XU - axesParams%XL) / 10.
        !                axesParams%YINT = (axesParams%YU - axesParams%YL) / 10.
        !            END IF
        !        end IF
        !        !==================================== Print data in left frame:
        !        CALL LEFTAX(fontParams, hWnd)
        !        IF(IFIRST .EQ. 4) RETURN
        !        !==================================== Print out info in upper frame:
        !        CALL TOPAX(fontParams, hWnd)
        !        !=============================================== Create Label & Stop box:
        !        CALL LABEL(fontParams, hWnd)
        !        IF(IFIRST .EQ. 3) RETURN
        !        !----------------------------------Special case for Hovmoller axis labeling:
        !        IF(modIntParams%MPLOT .EQ. HOV_DIAG_AT_LAT .OR. modIntParams%MPLOT .EQ. HOV_DIAG_AT_LONG) THEN
        !        !-------------------------------Label axes
        !           I = 0
        !           CALL DAXES(I, I, I, modPlotParams%ICTXT, modPlotParams%ICA)
        !           RETURN
        !        ENDIF
        !        !-------------------------------------------------Lable phase/time axes:
        !        IF(modIntParams%MPLOT .LE. HOV_DIAG_AT_LONG .OR. modIntParams%IPLT1 .EQ. PLOT_OFF) RETURN
        !        !--------------------------------------------- Set scale factors:
        !        axesParams%XU = 10. * modScalingParams%VR(modIntParams%IPLT1)
        !        IF(modIntParams%IPLT1 .GT. PLOT_OFF) axesParams%XU = modScalingParams%VR(modIntParams%IPLT1)
        !        !--------------------------------------------- If temp variable, scale by AT
        !        IF(isTemperature(modIntParams%IPLT1)) axesParams%XU = axesParams%AT * axesParams%XU
        !        axesParams%XL = -axesParams%XU
        !        axesParams%XINT = (axesParams%XU - axesParams%XL) / 10.
        !        axesParams%YU = axesParams%XU
        !        axesParams%YL = axesParams%XL
        !        axesParams%YINT = axesParams%XINT
        !        !--------------------------------------------Time series:
        !        IF(modIntParams%MPLOT .GE. WAVE_AMP_TIME_SERIES) THEN
        !            axesParams%XU = REAL(modIntParams%ISTEP)
        !            axesParams%XL = 0.
        !            axesParams%XINT = axesParams%XU / 10.
        !            !------------------------------------------Phase scalings:
        !            IF(modIntParams%MPLOT .EQ. WAVE_PHASE_TIME_SERIES) axesParams%YU = modScalingParams%VR(23)
        !            axesParams%YL = -axesParams%YU
        !            !------------------------------------------No negative no. for amplitudes:
        !            IF(modIntParams%MPLOT .EQ. WAVE_AMP_TIME_SERIES .AND. modIntParams%JX .NE. 0 .AND. modIntParams%IPLT1 .LE. 21) axesParams%YL = 0.
        !            axesParams%YINT = (axesParams%YU - axesParams%YL) / 10.
        !        ENDIF
        !    end IF
        !    axesParams%XSCAL = REAL(drawParams%KU - drawParams%KL + 1) / (axesParams%XU - axesParams%XL)
        !    axesParams%YSCAL = REAL(drawParams%LU - drawParams%LL + 1) / (axesParams%YU - axesParams%YL)
        !    axesParams%X0 = -axesParams%XL * axesParams%XSCAL
        !    axesParams%Y0 = -axesParams%YL * axesParams%YSCAL
        !    !-------------------------------------------Erase center of plot window only:
        !    NN = 0
        !    N1 = modPlotParams%ICF ! Save current color of the frame
        !    modPlotParams%ICF = modPlotParams%ICS
        !    CALL FRAME(NN, drawParams%KL, drawParams%LL, drawParams%KU, drawParams%LU, hWnd)
        !    modPlotParams%ICF = N1 ! Restore the original frame color
        !    !-------------------------------------------Erase frame only and redraw:
        !    CALL FRAME(-fontParams%tm%tmHeight, drawParams%KL, drawParams%LL, drawParams%KU, drawParams%LU, hWnd)
        !    call LEFTAX(fontParams, hWnd)
        !    CALL TOPAX(fontParams, hWnd)
        !    !---------------------------------JHOV=1 Case: (Believe it indicates a hovmoller diagram))
        !    IF(modIntParams%JHOV .EQ. 1 .AND. modIntParams%MPLOT .GE. WAVE_AMP_TIME_SERIES) THEN
        !        axesParams%YL = 1.
        !        IF(modIntParams%JX .EQ. -1) axesParams%YU = REAL(modIntParams%KMAX2)
        !        IF(modIntParams%JY .EQ. 0) axesParams%YU = REAL(modIntParams%LMAX2)
        !        IF(modIntParams%LEVP .EQ. 0) THEN
        !            axesParams%YU = REAL(modIntParams%NLEV)
        !            IF(isTemperature(modIntParams%IPLT1)) axesParams%YU = axesParams%YU - 1.
        !        ENDIF
        !        axesParams%YINT = 1.
        !        axesParams%XL = 0.
        !        axesParams%XU = REAL((drawParams%KU - drawParams%KL + 1) * modIntParams%MOUT) * modRealParams%DELT * scales%timeStep / 1440.
        !        axesParams%XINT = (axesParams%XU - axesParams%XL) / 10.
        !        I = 0
        !        CALL DAXES(I, I, I, modPlotParams%ICTXT, modPlotParams%ICA)
        !        RETURN
        !    ENDIF
        !    !-------------------------------------------Plot axes and labels:
        !    I = 0
        !    CALL PDIMEN(I)
        !    CALL DAXES(I, I, I, modPlotParams%ICTXT, modPlotParams%ICA)
        !    I = 1
        !    CALL PDIMEN(I)
        !    !--------------------------------------------- Only one quantity to show set active plot (the one the user can change on the fly):
        !    pltControls%activePlotMap = 1
        !end subroutine OUTPUT

        !******************************************* Conditions:
        !Subroutine CONDIT()
        !    use ifwinty, only: HANDLE, SINT
        !    use ConstantsModule, only: HOV_DIAG_AT_LAT, HOV_DIAG_AT_LONG, WAVE_AMP_TIME_SERIES, WAVE_PHASE_TIME_SERIES, &
        !                               LONG_PRES_CROSS_SEC, LAT_PRES_CROSS_SEC
        !    use ArrayDimensionsModule, only: arrayDims
        !    use ModelParamsModule, only: modPlotParams, modIntParams
        !    use PlotParamsModule, only: drawParams, FontParameters
        !
        !    implicit none
        !    
        !    integer :: mapChoice, IERASE        !TODO What is N1 and IRDIM and IERASE
        !    !---------------------------mapChoice condition:
        !    !  mapChoice=0 means 2D plot
        !    !  mapChoice=1 means Hovmoller
        !    !  mapChoice=2 means vert Hovmoller or contour graph
        !    !  mapChoice=3 means lines/points graph
        !    !---------------------------------------------------Hovmoller or 2d maps?
        !    mapChoice = 0
        !    IF(modIntParams%MPLOT .EQ. HOV_DIAG_AT_LAT .OR. modIntParams%MPLOT .EQ. HOV_DIAG_AT_LONG) mapChoice = 1
        !    !---------------------------------------------------Graph or contour graf?
        !    IF((modIntParams%MPLOT .GE. WAVE_AMP_TIME_SERIES .AND. JHOV .EQ. 1).AND. NPTS .GE. 2) mapChoice = 2
        !    IF(mapChoice .EQ. 0 .AND. modIntParams%MPLOT .GE. WAVE_PHASE_TIME_SERIES) mapChoice = 3
        !    !---------------------------------------------------Erasing conditions:
        !    !  Erase when the 1st map is streamlines before first call to ASXY
        !    !  Erase before 2nd call to ASXY when first map is not generated and is also
        !    !    streamlines and there are direct screen writes.
        !    !  Erase screen buffer after dumping to screen when either of above are yes.
        !    !
        !    IERASE = 0
        !    IF(modPlotParams%ISTRM .GE. 0) IERASE = 1
        !    IF(modPlotParams%JSTRM .GE. 0 .AND. modIntParams%IPLT1 .EQ. 17) IERASE = 2
        !    IF(IERASE .GT. 0 .AND. IOPT(4) .EQ. 1) IERASE = 3
        !    !----------------------------------------------IRDIM values for ASXY:
        !    IRDIM(1) = N1
        !    IRDIM(3) = N1
        !    !-----------------------------Valid for MPLOT=0,1 x-value for hov MPLOT=2
        !    IRDIM(2) = arrayDims%IXMAX1
        !    IRDIM(4) = arrayDims%IYH
        !    IF(modIntParams%MPLOT .EQ. LAT_PRES_CROSS_SEC) IRDIM(4) = arrayDims%IZMAX
        !    !-----------------------------Reverse of 0,1 for MPLOT=3
        !    IF(modIntParams%MPLOT .EQ. LONG_PRES_CROSS_SEC) THEN
        !        IRDIM(2) = arrayDims%IYH
        !        IRDIM(4) = arrayDims%IZMAX
        !    ENDIF
        !    !-----------------------------Reverse of 2 for hov MPLOT=4
        !    IF(modIntParams%MPLOT .EQ. HOV_DIAG_AT_LONG) IRDIM(2) = arrayDims%IYH
        !END Subroutine CONDIT

        !> ***************************************** Routine to initialize IB boxes:
        !> IB is an array of 12 integers that define the size of 3 framed boxes. Certain
        !> messages are placed in the these boxes. These are the boxes that have the
        !> Stop and help instrucions (top left of main plot screen), and then the
        !> the message boxes at the top of the screen that are of the same
        !> size. One has what is currently being displayed. The other box which is
        !> to the right of the current fields label box appears to always be empty.
        !> This does NOT appear to include the box to the left which has the keys the
        !> user can use to change the fields and types of maps being displayed.
        SUBROUTINE IBVALS(fontParams, frameBorderWidth, numOfCharacters)
            use ifwinty, only : SINT
            use PlotParamsModule, only : drawParams, IB, FontParameters
        
            implicit none
            !  frameBorderWidth = Border width of the frame
            !  numOfCharacters = Length of the message box in characters:
            !  The coordinates IB(1-4) are lower left (x,y) and upper right (x,y)
            !---------------------------------------------------------------------------
        
            type(FontParameters), intent(in) :: fontParams  ! Font parameters
            INTEGER(SINT), intent(in) ::  frameBorderWidth
            INTEGER, intent(in) :: numOfCharacters
            
            ! Local variable
            INTEGER :: I
            
            !---------------------------------------------- Stop button (8 spaces wide):
            IB(1) = drawParams%KL
            IB(2) = drawParams%LU + 3 * frameBorderWidth + 4
            IB(3) = IB(1) + fontParams%tm%tmAveCharWidth * 8  ! 8 characters wide
            IB(4) = IB(2) + 2 * fontParams%tm%tmHeight        ! Room for two rows of text
            
            !---------------------------------------------- Message box number one:
            !  Makes room for three rows of fontParams%tm%tmHeight characters
            IB(5) = (drawParams%KU + IB(1) - 2 * fontParams%tm%tmAveCharWidth * numOfCharacters) / 2
            I = IB(3) + 3 * frameBorderWidth
            IF(IB(5) .LT. IB(3) + I) IB(5) = I
            IB(6) = IB(2) - 8
            IB(7) = IB(5) + fontParams%tm%tmAveCharWidth * numOfCharacters
            IB(8) = IB(6) + 3 * fontParams%tm%tmHeight
            !---------------------------------------------- Message box number two:
            !  Makes room for three rows of fontParams%tm%tmHeight characters
            IB(9) = IB(7) + 3 * frameBorderWidth
            IB(10) = IB(6)
            IB(11) = IB(9) + fontParams%tm%tmAveCharWidth * numOfCharacters
            IB(12) = IB(10) + 3 * fontParams%tm%tmHeight    ! Room for three rows of text
        
        END subroutine IBVALS


        !=====================================Left and bottom axis stuff:
        SUBROUTINE LEFTAX(fontParams, hWnd)
            use ifwinty, only : HANDLE, SINT
            use ConstantsModule, only: HOV_DIAG_AT_LAT, HOV_DIAG_AT_LONG, isTemperature
            use ModelParamsModule, only: modIntParams, pltControls, modPlotParams
            use PlotParamsModule, only : drawParams, FontParameters
            use PboxModule
            use ScreenTextBuffers
        
            implicit none
            
            type(FontParameters), intent(in) :: fontParams  ! Font parameters
            INTEGER(HANDLE), intent(in) ::  hWnd
            
            integer(SINT) :: ID, IE, N, ICHAR
            CHARACTER(128) :: errMessage
        
            !----------------------------------------------Write + - info:
            CALL PLUSM(fontParams, hWnd)
            !----------------------------------------------Bottom axis:
            !----------------------------------------------Denote Pot, Reg, Tot temp.
            IF(modIntParams%MPLOT .LT. HOV_DIAG_AT_LONG .AND. modIntParams%MPLOT .NE. HOV_DIAG_AT_LAT) THEN
                !---------------------------------------------- Temp variable displayed?
                IF(isTemperature(modIntParams%IPLT1) .OR. isTemperature(modIntParams%IPLT2)) THEN
                    !-----------------------------------------------------Erase lower frame:
                    errMessage = PBOX(drawParams%KL, drawParams%LL - fontParams%tm%tmHeight, drawParams%KL + 56 * fontParams%tm%tmAveCharWidth, &
                            drawParams%LL - 2,  modPlotParams%ICF, hWnd)
                    ICHAR = 28
                    !---------------------------------Potential or standard temperature:
                    N = 17 + pltControls%plotTempType
                    !-----------------------------------------Text is in MSGM(13),(14):
                    ID = drawParams%KL
                    IE = drawParams%LL - fontParams%tm%tmHeight - 2
                    errMessage = PNTEXT(fontParams, ID, IE, modPlotParams%ICTXT, MSGM(N), hwnd)
                    !CALL PTEXT(MSGMI(1, N))
                    !-----------------------------------------Basic state included?
                    IF(pltControls%incBasicStateTemp) THEN
                        ID = ID + 28 * fontParams%tm%tmAveCharWidth
                        errMessage = PNTEXT(fontParams, ID, IE, modPlotParams%ICTXT, MSGM(19), hwnd)
                        !CALL PTEXT(MSGMI(1, 19))
                    END IF
                END IF
            END IF
        END subroutine LEFTAX

        !**********************************************Top Frame Data:
        SUBROUTINE TOPAX(fontParams, hWnd)
            use ifwinty, only : HANDLE, SINT
            use ConstantsModule, only: HOV_DIAG_AT_LAT, HOV_DIAG_AT_LONG, WEATHER_STN, WAVE_AMP_TIME_SERIES, OROG_HGT_TEND, OROGRAPHY, VORTICITY
            use ModelParamsModule, only: modIntParams, modRealParams, pltControls, modPlotParams
            use PlotParamsModule, only : drawParams, FontParameters
            use PboxModule
            use ScreenTextBuffers
            !use UtilitiesModule, only: IntegerToAscii, RealToAscii
        
            implicit none
        
            type(FontParameters), intent(in) :: fontParams  ! Font parameters
            INTEGER(HANDLE), INTENT(IN) :: hWnd
        
            ! Local variables
            integer :: I, N, K, J, L, M, ID, IE, JJ, IPLT, ICHAR
            real :: zhgt
            CHARACTER(128) :: errMessage
            type(FontParameters) :: font90Params  ! Font parameters for 90 degree text
        
            !====================================Print out Maps info in upper frame:
            IF(modIntParams%MPLOT .LE. HOV_DIAG_AT_LONG) THEN
            !------------------------------------Give heights if horizontal maps/hov
                N = 4
                I = 1
                !-------------------------------------There are three msgs to fill:
                DO M = 1, 3
                    K = 35
                    !-------------------------------------Each has three possible levels:
                    DO J = 1, 3
                        if (J .eq. 1) then
                            IPLT = modIntParams%IPLT1
                            zhgt = modRealParams%ZHGT
                        else if  (J .eq. 2) then 
                            IPLT = modIntParams%IPLT2
                            zhgt = modRealParams%ZHGT2
                        else if  (J .eq. 3) then 
                            IPLT = modIntParams%IPLT3
                            zhgt = modRealParams%ZHGT3
                        end if
                        !-------------------------------------Load with blanks:
                        DO L = 0, 4
                            MSGLI(K + L, I : I) = ' '
                        END DO
                        !--------------------------------------Load pressure values if appropriate:
                        ! (MPLOT,ICN(7)), (IPLT1,ICN(8)),(IPLT2,ICN(9)),(IPLT3,ICN(10)), old equivalence values
                        IF(IPLT .LT. 15) THEN
                            ! (RN,VAR(1)),(BETA,VAR(2)),(DELT,VAR(3)),(ZHGT,VAR(4)),(ZHGT2,VAR(5)),(ZHGT3,VAR(6)),(RLAT,VAR(7)) old equivalence values
                            L = INT(zhgt)
                            !CALL IntegerToAscii(N, MSGLI(K, I), L)
                        END IF
                        K = K + 7
                    END DO
                    I = I + 2
                END DO
                J = 3   ! Places before the decimal point
                K = 1   ! Places after the decimal point
                !CALL RealToAscii(J, K, MSGLI(44, 2), modRealParams%RLAT)
                !CALL RealToAscii(J, K, MSGLI(22, 3), modRealParams%RLAT)
                !CALL RealToAscii(J, K, MSGLI(44, 4), modRealParams%RLON)
                !CALL RealToAscii(J, K, MSGLI(22, 5), modRealParams%RLON)
            ENDIF
            !-------------------------------- Data at a point special case:
            IF(modIntParams%MPLOT .EQ. WEATHER_STN .OR. modIntParams%MPLOT .LE. HOV_DIAG_AT_LONG) THEN
                I = modIntParams%JX1
                J = modIntParams%JY1
                K = 9
                N = 2   ! Number of digits in the string
                !CALL IntegerToAscii(N, MSGLI(40, K), J)
                !CALL IntegerToAsci(N, MSGLI(26, K), I)
                I = modIntParams%JX0
                J = modIntParams%JY0
            ELSE
                !-------------------------------------------------Wavenumber K, L:
                I = modIntParams%JX
                J = modIntParams%JY
                K = modIntParams%MPLOT + 1
            ENDIF
            !-----------------------Set in word "ALL" in MSGL(x,MPLOT+1):
            JJ = 22
            DO N = 1, 3
                !CALL WORDS(MSGLI(JJ, K), 1)
                JJ = JJ + 14
            END DO
            L = modIntParams%LEVP
            !-----------------------------------------Set to VORTICITY (1) for IPLTx = 15, 16: OROG_HGT_TEND, OROGRAPHY
            ! The user is paging through the variables and this only affects the active variable which is
            ! either IPLT1, IPLT2, or IPLT3. When the user reaches OROG_HGT_TEND or OROGRAPHY, we cycle back to VORTICIY (1).
            ! old but effective way but damn hard to follow! IF(ICN(IVRB + 7) .EQ. 15 .OR. ICN(IVRB + 7) .EQ. 16) L = 1
            if (pltControls%activePlotMap == 1 .AND. (modIntParams%IPLT1 .EQ. OROG_HGT_TEND .OR. modIntParams%IPLT1 .EQ. OROGRAPHY)) then
                L = VORTICITY
            else if (pltControls%activePlotMap == 2 .AND. (modIntParams%IPLT2 .EQ. OROG_HGT_TEND .OR. modIntParams%IPLT2 .EQ. OROGRAPHY)) then
                L = VORTICITY
            else if  (pltControls%activePlotMap == 3 .AND. (modIntParams%IPLT1 .EQ. OROG_HGT_TEND .OR. modIntParams%IPLT1 .EQ. OROGRAPHY)) then
                L = VORTICITY
            end if
            N = 2
            !IF(L .GT. 0) CALL IntegerToAscii(N, MSGLI(50, K), L)
            !IF(J .GT. 0) CALL IntegerToAscii(N, MSGLI(36, K), J)
            !IF(I .GE. 0) CALL IntegerToAscii(N, MSGLI(22, K), I)
            !-------------------------------------------Center text string:
            ID = (drawParams%KU + drawParams%KL) / 2 - 30 * fontParams%tm%tmAveCharWidth
            !-------------------------------------------Make sure start is after KL:
            IF(ID .LT. drawParams%KL) ID = drawParams%KL
            !-------------------------------------------Erase whats in frame:
            errMessage = PBOX(ID, drawParams%LU + 3, ID + 56 * fontParams%tm%tmAveCharWidth, drawParams%LU + fontParams%tm%tmHeight - 1, modPlotParams%ICF, hWnd)
            !-------------------------------------------Now print out results in frame:
            !ICHAR = 56
            IE = drawParams%LU + 1
            !CALL PTEXT(MSGLI(1, modIntParams%MPLOT + 1))
            errMessage = PNTEXT(fontParams, ID, IE, modPlotParams%ICTXT, MSGL(modIntParams%MPLOT + 1), hwnd)
            IF(modIntParams%MPLOT .LE. HOV_DIAG_AT_LONG .OR. modIntParams%JHOV .EQ. 1) THEN
                errMessage = PBOX(drawParams%KU + 3, drawParams%LL, drawParams%KU + fontParams%tm%tmHeight, &
                                drawParams%LL + 42 * fontParams%tm%tmAveCharWidth, modPlotParams%ICF, hWnd)
                !-------------------------------------------Now print out results in frame:
                !IDIR = 1            ! This direction will require a new font. In the old code:
                                    !  0=Normal   now 0
                                    !  1=90 degrees  now 90
                                    !  2=-90 degrees  now -90
                font90Params = GetFont(90, "Times New Roman", 16, hWnd) ! New font for 90 degrees
                ID = drawParams%KU + fontParams%tm%tmHeight + 2
                IF(modIntParams%JHOV .EQ. 1 .AND. modIntParams%MPLOT .GE. WAVE_AMP_TIME_SERIES) THEN
                    IF(modIntParams%JX .EQ. -1) N = 12
                    IF(modIntParams%JY .EQ. 0) N = 13
                    IF(modIntParams%LEVP .EQ. 0) N = 14
                    ICHAR = 28
                    IE = (drawParams%LU + drawParams%LL - fontParams%tm%tmAveCharWidth * ICHAR) / 2
                    !CALL PTEXT(MSGSI(1, N))
                    errMessage = PNTEXT(fontParams, ID, IE, modPlotParams%ICTXT, MSGS(N), hwnd)
                ELSE
                    IE = fontParams%tm%tmAveCharWidth
                    ICHAR = 42
                    !CALL PTEXT(MSGLI(1, 9))
                    errMEssage = PNTEXT(font90Params, ID, IE, modPlotParams%ICTXT, MSGL(9), hwnd)
                ENDIF
                !IDIR = 0        ! Back to orignal font
            ENDIF
        END subroutine TOPAX

        !*************************************************Write out main label:
        SUBROUTINE LABEL(fontParams, hWnd)
            use ifwinty, only : HANDLE, SINT
            use ConstantsModule, only: HOV_DIAG_AT_LAT, HOV_DIAG_AT_LONG, WEATHER_STN, WAVE_AMP_TIME_SERIES, OROG_HGT_TEND, OROGRAPHY, VORTICITY
            use ModelParamsModule, only: modIntParams, modRealParams, pltControls
            use PlotParamsModule, only : drawParams, FontParameters, IB
            use ScreenTextBuffers
        
            implicit none
            
            type(FontParameters), intent(in) :: fontParams  ! Font parameters
            INTEGER(HANDLE), INTENT(IN) :: hWnd
            
            integer :: I, J, K, L, M, N, ID, IE, frameWidth
        
            frameWidth = 2   ! Frame thickness
            CALL FRAME(frameWidth, IB(5), IB(6), IB(7), IB(8), hWnd)
            CALL FRAME(frameWidth, IB(9), IB(10), IB(11), IB(12), hWnd)
            J = 1
            !------------------------------------Up to 3 variables for maps:
            IF((modIntParams%MPLOT .GE. 6 .AND. modIntParams%JHOV .EQ. 1) .OR. modIntParams%MPLOT .LE. 4) J = 3
            IE = IB(6) + 2 * fontParams%tm%tmHeight
            ID = IB(5)
            DO I = 1, J
                if (I .EQ. 1) K = modIntParams%IPLT1
                if (I .EQ. 2) K = modIntParams%IPLT2
                if (I .EQ. 3) K = modIntParams%IPLT3
                CALL PTEXT(fontParams, ID, IE, MSGR(K), hWnd)
                IE = IE - fontParams%tm%tmHeight
            END DO
            !-------------------------------------------------Plot type if graph:
            ID = IB(5)
            IE = IB(6)
            IF(modIntParams%JHOV .EQ. -1 .OR. modIntParams%MPLOT .EQ. 5) THEN
                IF(modIntParams%MPLOT .GT. 4) CALL PTEXT(fontParams, ID, IE, MSGM(modIntParams%MPLOT + 1), hWnd)
                !-----------------------------------------------Print out 'BALANCE of FORCES'
                IE = IE + fontParams%tm%tmHeight
                IF(modIntParams%IPLT1 .GE. 18 .AND. modIntParams%IPLT1 .LE. 21)CALL PTEXT(fontParams, ID, IE, MSGM(20), hWnd)
            ENDIF
            !--------------------------------------Hilight active variable:
            CALL HILIT(1, fontParams, hWnd)      !Dont know how to do this yet
            !-----------------------------------------------Print out stop/help button:
            ID = IB(1)
            IE = IB(2)
            CALL FRAME(frameWidth, ID, IE, IB(3), IB(4), hWnd)
            CALL PTEXT(fontParams, ID, IE, MSGM(25), hWnd) ! Should be only from 9-16 but too difficult to fix
            IE = IE + fontParams%tm%tmHeight
            CALL PTEXT(fontParams, ID, IE, MSGH(25), hWnd)
        END subroutine LABEL

        !*************************************Routine PLUSM to write - + on border:
        SUBROUTINE PLUSM(fontParams, hWnd)
            use ifwinty, only: HANDLE, SINT
            use PboxModule, only: PNTEXT
            use ModelParamsModule, only: modPlotParams
            use PlotParamsModule, only : drawParams, FontParameters
            
            implicit none
            
            type(FontParameters), intent(in) :: fontParams  ! Font parameters
            integer(HANDLE), intent(in) :: hWnd  ! Window handle
        
            INTEGER(SINT) IX, IY
            character(len=64) message
            !-------------------------------Here we have - + in ASCII code:
            !MSGSI(11, 16) = 45
            !MSGSI(12, 16) = 32
            !MSGSI(13, 16) = 43
            IX = drawParams%LU + 2
            IY = drawParams%KU - fontParams%tm%tmAveCharWidth
            message = PNTEXT(fontParams, IX, IY, modPlotParams%ICTXT, '- +', hWnd)
            !CALL HILIT(2, fontParams, hWnd)
        END subroutine PLUSM

        !============================================================Frame routine:
        !>  Draws a box on the screen with coordinates (IX1,IY1),(IX2,IY2)
        !>  with a frame about it of thickness KK.  The total frame thickness is about
        !>  KK+2 since there is a 2-line shadow drawn. Box color is ICF, shadow ICFS
        !>  and highlight ICFH. A negative KK hops over the fill done by calling PBOX.
        !! @param KK: Thickness of the frame. If negative, no fill of the frame is done.
        !! @param IX1, IY1, IX2, IY2: left, bottom, right, top Coordinates of the box.
        !! @param hWnd: Window handle for the drawing.
        SUBROUTINE FRAME(KK, IX1, IY1, IX2, IY2, hWnd)
            use ifwinty, only: HANDLE, SINT
            use ModelParamsModule, only: modPlotParams
            use PboxModule, only: PBOX, PLINE
        
            implicit none
            INTEGER(SINT), intent(in) :: KK, IX1, IY1, IX2, IY2
            integer(HANDLE), intent(in) :: hWnd  ! Window handle
        
            ! Local variables
            INTEGER :: L1, L2, M1, M2, K
            CHARACTER(128) :: message
            INTEGER :: I
            
            !----------------------------------------------------------------- Fill box
            K = KK
            IF(KK .GE. 0) THEN
                K = KK
                message = PBOX(IX1 - K, IY1 - K, IX2 + K, IY2 + K, modPlotParams%ICF, hWnd)
                IF(K .EQ. 0)RETURN
            ELSE
                K = -KK
                !-----------------------------------------------------Left box:
                message = PBOX(IX1 - K, IY1 - K, IX1, IY2 + K, modPlotParams%ICF, hWnd)
                !-----------------------------------------------------Bottom box:
                message = PBOX(IX1, IY1 - K, IX2 + K, IY1, modPlotParams%ICF, hWnd)
                !-----------------------------------------------------Top box:
                message = PBOX(IX1, IY2, IX2 + K, IY2 + K, modPlotParams%ICF, hWnd)
                !------------------------------------------------------Right box:
                message = PBOX(IX2, IY1, IX2 + K, IY2 + K, modPlotParams%ICF, hWnd)
            ENDIF
            DO I = 1, 2
        !------------------------------Draw frame; Lines nearest text block first:
               L1 = IX1 - I
               L2 = IY1 - I
               M1 = IX2 + I
               M2 = IY2 + I
               message = PLINE(L1, L2, L1, M2, modPlotParams%ICFS, hWnd)
               message = PLINE(L1, M2, M1, M2, modPlotParams%ICFS, hWnd)
               message = PLINE(M1, M2, M1, L2, modPlotParams%ICFH, hWnd)
               message = PLINE(M1, L2, L1, L2, modPlotParams%ICFH, hWnd)
        !-----------------------------Lines outside the frame body next:
               L1 = IX1 - I - K
               L2 = IY1 - I - K
               M1 = IX2 + I + K
               M2 = IY2 + I + K
               message = PLINE(L1, L2, L1, M2, modPlotParams%ICFH, hWnd)
               message = PLINE(L1, M2, M1, M2, modPlotParams%ICFH, hWnd)
               message = PLINE(M1, M2, M1, L2, modPlotParams%ICFS, hWnd)
               message = PLINE(M1, L2, L1, L2, modPlotParams%ICFS, hWnd)
            end DO
        END subroutine FRAME

        !************************************Subroutine PTEXT
        SUBROUTINE PTEXT(fontParams, IX, IY, MSG, hWnd)
            use ifwinty, only: HANDLE, SINT
            use ModelParamsModule, only: modPlotParams
            use PlotParamsModule, only : FontParameters
            use PboxModule, only: PNTEXT
        
            implicit none
        
            type(FontParameters), intent(in) :: fontParams  ! Font parameters
            character(*), intent(in) :: MSG
            integer(SINT), intent(in) :: IX, IY
            integer(HANDLE), intent(in) :: hWnd  ! Window handle
        
            character(len=64) message
        
            message = PNTEXT(fontParams, ix, iy, modPlotParams%ICTXT, MSG, hwnd)
        
        END subroutine PTEXT

        !***************************************************Hilite routine:
        SUBROUTINE HILIT(M, fontParams, hWnd)
            use ifwinty, only: HANDLE, SINT
            use PboxModule, only: PBOX
            use ModelParamsModule, only: modPlotParams, pltControls
            use PlotParamsModule, only : FontParameters, drawParams, IB
        
            implicit none
        
            type(FontParameters), intent(in) :: fontParams  ! Font parameters
            integer(HANDLE), intent(in) :: hWnd
            integer, intent(in) :: M  ! Hilite options: 1=active variable, 2=hilite +/-:
        
            integer(SINT) :: I6, J6
            CHARACTER(128) :: message
            !------------------------------If M=1 hilite active variable
            IF(M .EQ. 1)THEN
               I6 = IB(8) - pltControls%activePlotMap * fontParams%tm%tmHeight
               message = PBOX(IB(5), I6, IB(7), I6 + fontParams%tm%tmHeight, modPlotParams%LPL, hWnd)
               RETURN
            ENDIF
            !------------------------------If M=2 Hilite + -:
            I6 = drawParams%KU + pltControls%togglePlotDir * fontParams%tm%tmAveCharWidth
            J6 = drawParams%LU + 3
            message = PBOX(I6, J6, I6 + fontParams%tm%tmAveCharWidth, J6 + fontParams%tm%tmHeight - 3, modPlotParams%LPL, hWnd)
        END subroutine HILIT


end module PlotUtilsModule