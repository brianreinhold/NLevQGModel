!  NlevelQGModel.f90

! ============================ Compile Dependencies ============================
! If a module has an error, any module that uses that module will also have an error.
! We are only considering modules that we have written, not intel, fortran intrinsics, or win32
! So here are a list of the modules and their dependencies (if any) as of 2026/4/21
! NLevQGModelGlobals: None
! ArrayDimensionsModule: None
! ConstantsModule: None
! fftw3HelpersModule: None if test code is excluded. Otherwise: ArrayDimensionsModule, ModelParamsModule, NLevQGModelGlobals
! ModelParamsModule: ConstantsModule
! InteractionCoefficientsModule: ConstantsModule, ModelParamsModule
! UtilitiesModule: ConstantsModule, ModelParamsModule
! ModelFieldsModule: ConstantsModule, ArrayDimensionsModule, ModelParamsModule
! SetupModule: ConstantsModule, ArrayDimensionsModule, NLevQGModelGlobals, fftwsHelpersModule, ModelParamsModule, ModelFieldsModule,
!              InteractionCoefficientsModule, UtilitiesModule
! ModelModule: ConstantsModule, ArrayDimensionsModule, ModelFieldsModule, ModelParamsModule, InteractionCoefficientsModule, SetupModule,
!              NLevQGModelGlobals, fftw3HelpersModule, UtilitiesModule,
! DialogHelpers Module: None
! DialogProcInterfacesModule: None
! InitVariablesDialogModule: NLevQGModelGlobals, ArrayDimensionsModule, ConstantsModule, UtilitiesModule, DialogHelpersModule, 
!                            ModelParamsModule, KeyboardHooksModule, ModelFieldsModule, SetUpModule
! VerticalParamsDialogModule: NLevQGModelGlobals, ConstantsModule, UtilitiesModule, DialogHelpersModule, ModelParamsModule, 
!                             KeyboardHooksModule, ModelFieldsModule, SetUpModule
! KeyboardHooksModule: NLevQGModelGlobals, DlgProcInterfacesModule, SetUpModule, ModelModule, ModelParamsModule, DialogHelpersModule
! ModelControlDlgProc: NLevQGModelGlobals, ModelParamsModule, SetUpModule, ConstantsModule, KeyboardHooksModule, ModelModule, 
!                      DialogHelpersModule, VerticalParamsDialogModule, InitVariablesDialogModule, UtilitiesModule
! SettingsDialogModule: NLevQGModelGlobals, DialogHelpersModule, ModelParamsModule, ModelControlDialogModule, KeyboardHooksModule
! NLevQGModel: NLevQGModelGlobals, DlgProcInterfacesModule, ModelParamsModule, UtilitiesModule
!
! Model defines the streamfunction at the center of the N levels and the temperature at the interface between the levels
! Thus PSI(n + 1) - PSI(n) = TH(n + .5)
! However, we are not going to deal with half-levels. TH(n) located at the interface between PSI(n) and PSI(n + 1).
!
!  FUNCTIONS:
!   WinMain()      - Entry point for the application;
!                    displays the main window; processes the message loop
!   MainWndProc()  - processes messages for the main window
!   LogWndProc()   - Messages for the Logging Window
!

!****************************************************************************
!
!  FUNCTION: WinMain( hInstance, hPrevInstance, lpszCmdLine, nCmdShow )
!
!  PURPOSE:  Entry point for the application
!
!  COMMENTS: Displays the main window and processes the message loop
!
!****************************************************************************

function WinMain( hInstance, hPrevInstance, lpszCmdLine, nCmdShow )
!DEC$ IF DEFINED(_X86_)
!DEC$ ATTRIBUTES STDCALL, ALIAS : '_WinMain@16' :: WinMain
!DEC$ ELSE
!DEC$ ATTRIBUTES STDCALL, ALIAS : 'WinMain' :: WinMain
!DEC$ ENDIF

    use user32
    use ifwin
    use kernel32
    use gdi32
    
    use NLevQGModelGlobals, only: ghInstance, ghModule, ghWndMain, gBitMapInfo, ghLogMain, ghSettingsDlg, WriteMessageBox, SIZEOFAPPNAME, &
                                    LogMessage, StartTimer
    use WndProcInterfacesModule, only: MainWndProc, LogWndProc
    use DlgProcInterfacesModule, only: SettingsDlgProc

    use SetUpModule

    implicit none

    integer(SINT) :: WinMain
    integer(HANDLE) hInstance
    integer(HANDLE) hPrevInstance
    integer(LPWSTR) lpszCmdLine
    integer(SINT)   nCmdShow

    !include 'NlevelQGModel.fi'
    include 'resource.fd'

    ! Variables

    type (T_WNDCLASS)       wc
    type (T_MSG)            mesg
    TYPE (T_RECT) ::        rcWork
    integer                 ret
    integer(LRESULT)        lret
    integer(BOOL)           bret
    integer(SINT)           iret
    integer(HANDLE)         haccel

    ! Main window
    character(SIZEOFAPPNAME) lpszClassName
    character(SIZEOFAPPNAME) lpszIconName
    character(SIZEOFAPPNAME) lpszAppName
    character(SIZEOFAPPNAME) lpszMenuName
    character(SIZEOFAPPNAME) lpszAccelName
    
    ! Log Window
    character(SIZEOFAPPNAME) lpszLogClassName
    character(SIZEOFAPPNAME) lpszLogAppName
    character(SIZEOFAPPNAME) lpszLogMenuName
    
    LOGICAL :: success
    success = AllocConsole()


    ghInstance = hInstance
    ghModule = GetModuleHandle(NULL)
    ghwndMain = NULL

    lpszClassName ="NlevelQGModel"C
    lpszAppName ="NlevelQGModel"C
    lpszIconName ="NlevelQGModel"C  
    lpszMenuName ="NlevelQGModel"C
    lpszAccelName ="NlevelQGModel"C
    
    lpszLogClassName = "Log"C
    lpszLogMenuName = "Log"C
    lpszLogAppName = "Log"C

    !  If this is the first instance of the application, register the
    !  window class(es)
    if (hPrevInstance .eq. 0) then
        ! Main window registration
        wc%lpszClassName = LOC(lpszClassName)
        wc%lpfnWndProc = LOC(MainWndProc)
        wc%style = IOR(CS_VREDRAW , CS_HREDRAW)
        wc%hInstance = hInstance
        wc%hIcon = LoadIcon( hInstance, LOC(lpszIconName))
        wc%hCursor = LoadCursor( NULL, IDC_ARROW )
        wc%lpszMenuName = NULL
        wc%cbClsExtra = 0
        wc%cbWndExtra = 0
        if (RegisterClass(wc) == 0) goto 99999
        
        ! Log Window Registration
        wc%lpszClassName = LOC(lpszLogClassName)
        wc%hbrBackground = CreateSolidBrush(RGB(200, 250, 255))
        wc%lpfnWndProc = LOC(LogWndProc) 
        if (RegisterClass(wc) == 0) goto 99999
    end if

    ! Load the window's menu and accelerators and create the window
    !
    !ghMenu = LoadMenu(hInstance, LOC(lpszMenuName))
    !ret = GetLastError()
    !if (ghMenu == 0) goto 99999
    !haccel = LoadAccelerators(hInstance, LOC(lpszAccelName))
    !if (haccel == 0) goto 99999
    
    ! Get Screen data
    lret = SystemParametersInfo(SPI_GETWORKAREA, 0, LOC(rcWork), 0)

    gBitMapInfo%rect = rcWork
    gBitMapInfo%width = rcWork%right - rcWork%left
    gBitMapInfo%height = rcWork%bottom - rcWork%top
    gBitMapInfo%startX = rcWork%left
    gBitMapInfo%startY = rcWork%top
    
    ghLogMain = CreateWindowEx(  0, lpszLogClassName,     &
                                 lpszLogAppName,           &   ! Window name
                                 INT(WS_OVERLAPPEDWINDOW),  &   ! style
                                 CW_USEDEFAULT,             &   ! X position
                                 10,                        &   ! Y position
                                 CW_USEDEFAULT,             &   ! Width
                                 0,                         &   ! Height
                                 NULL,                      &   ! Parent window
                                 NULL,                      &   ! Menu
                                 hInstance,                 &   ! Instance
                                 NULL)                          ! Parameters

    lret = ShowWindow( ghLogMain, nCmdShow )

    !======================= Create main window =====================
    !HWND CreateWindowEx(               https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-createwindowexa
    !  [in]           DWORD     dwExStyle,
    !  [in, optional] LPCSTR    lpClassName,
    !  [in, optional] LPCSTR    lpWindowName,
    !  [in]           DWORD     dwStyle,
    !  [in]           int       X,
    !  [in]           int       Y,
    !  [in]           int       nWidth,
    !  [in]           int       nHeight,
    !  [in, optional] HWND      hWndParent,
    !  [in, optional] HMENU     hMenu,
    !  [in, optional] HINSTANCE hInstance,
    !  [in, optional] LPVOID    lpParam
    !);
    ghwndMain = CreateWindowEx(  0, lpszClassName,                 &
                                 lpszAppName,                      &
                                 IOR(WS_POPUP, IOR(WS_THICKFRAME, WS_VISIBLE)), & !INT(WS_OVERLAPPEDWINDOW),         &
                                 gBitMapInfo%startX,               &
                                 gBitMapInfo%startY,               &
                                 gBitMapInfo%width,                &
                                 gBitMapInfo%height,               &
                                 NULL,                             &
                                 NULL,                             &
                                 hInstance,                        &
                                 NULL                              &
                              )
    if (ghwndMain == 0) goto 99999

    lret = ShowWindow( ghwndMain, nCmdShow )
    ! Initialize screen buffers for text arrays addressable as bytes.
    !CALL InitScreenBuffers()
    call StartTimer()
    call LogMessage("Program Start: Main Window up and running")

    ! Initialize model
    ret = StartUp()
    if (ret .NE. 0) then
        iret = MessageBox(ghwndMain, "Error initializing Model"C, "Error"C, MB_OK)
        
    end if

    ! Programatically open a Modeless dialog box
    ghSettingsDlg = CreateDialogParam(ghInstance, INT(IDD_SETTINGS_DLG, LONG_PTR), &
                NULL, & !ghwndMain,  Make the dialog unowned so the main window can come in the foreground
                LOC(SettingsDlgProc), 0_LONG_PTR)
    if (ghSettingsDlg /= 0) then
        ret = ShowWindow(ghSettingsDlg, SW_SHOW)
        ret = UpdateWindow(ghSettingsDlg) ! Optional: forces an immediate paint
    end if

    ! Read and process messsages
    do while( GetMessage (mesg, NULL, 0, 0) ) 
        ! Check if message is for the Modeless dialog ModelControlDialog. If not, handle the message normally.
        ! If so, the dialog will handle the message.
        ! If this is not done, keypresses will be ignored by the modeless dialog. Mouse clicks will work however.
        lret = 0
        if (ghSettingsDlg /= 0) then
            lret = IsDialogMessage(ghSettingsDlg, mesg)
        endif 
        ! If not handled by a modeless dialog, process the message normally
        if (lret == 0) then
            ! Standard processing for all other messages
            bret = TranslateMessage( mesg )
            lret = DispatchMessage( mesg )
        endif
    end do

    ! Cannot put the save file here as the windows handles are no longer valid, and the overwrite file message will crash.
    WinMain = mesg.wParam
    return

99999 &

    iret = MessageBox(ghwndMain, "Error initializing application NlevelQGModel"C, &
                     "Error"C, MB_OK)
    WinMain = 0

end

!****************************************************************************
!
!  FUNCTION: LogWndProc ( hWnd, mesg, wParam, lParam )
!
!  PURPOSE:  Processes messages for the diagnostics window
!
!  COMMENTS:
!
!****************************************************************************

integer(LRESULT) function LogWndProc ( hWnd, mesg, wParam, lParam )
!DEC$ IF DEFINED(_X86_)
!DEC$ ATTRIBUTES STDCALL, ALIAS : '_LogWndProc@16' :: LogWndProc
!DEC$ ELSE
!DEC$ ATTRIBUTES STDCALL, ALIAS : 'LogWndProc' :: LogWndProc
!DEC$ ENDIF

    use user32
    use gdi32               ! For DeleteObject()
    use ifport              ! For GetLastError()
    use NLevQGModelGlobals, only: gFont, ghLogList, ghInstance, ghLogMain, WriteMessageBox, SIZEOFAPPNAME
    use iso_c_binding
    
    implicit none

    integer(HANDLE) hWnd
    integer(UINT) mesg
    integer(UINT_PTR) wParam
    integer(LONG_PTR) lParam 
    
    include 'resource.fd'
    
    ! Variables
    integer(BOOL)       bret
    integer(LRESULT)    lret
    integer(SINT)       iret
    character(SIZEOFAPPNAME)  lpszName, lpszHelpFileName, lpszContents, lpszMessage
    character(SIZEOFAPPNAME)  lpszHeader
    TYPE (T_PAINTSTRUCT) lpPaint
    TYPE(T_RECT) :: rect
    integer(HANDLE) hdc
    integer(4) err

    select case ( mesg )

    ! WM_DESTROY:
      case (WM_DESTROY)
        bret = DeleteObject(gFont)
        !call PostQuitMessage( 0 )
        LogWndProc = 0
        return
          
      case (WM_SETFONT)
        gFont = wParam  ! Store the font handle (not really necessary since it is already done)
        LogWndProc = 0  ! Indicate successful handling
        return
       
      ! Override and do not close - eventually remove the close button
      case (WM_CLOSE)
        LogWndProc = 0  ! Indicate successful handling
        return
        
      case (WM_CREATE)
        ! Create the EDIT control within the diagnostics window
        ! Should be done in the WM_CREATE message of the diagnostics window to assure the parent is in place.
        ! WE want to make the edit box fill the parent window client area
          
        iret = GetClientRect(hwnd, rect)  ! Get client area dimensions
        !if (iret == 0) then
        !    err = GetLastError()
        !    iret = WriteMessageBox(hwnd, err, "Error getting client area rectangle", "Log Window error")
        !end if
        !ghLogEdit = CreateWindowEx(WS_EX_CLIENTEDGE, &
        !        "EDIT" // C_NULL_CHAR, & ! As  usual, change a FORTRAN string to a C string
        !        NULL, & 
        !        IOR(IOR(IOR(IOR(IOR(IOR(WS_CHILD, WS_VISIBLE), WS_BORDER), WS_VSCROLL), ES_MULTILINE), ES_READONLY), ES_AUTOVSCROLL), &
        !        0, &
        !        0, &
        !        rect%right - rect%left, &
        !        rect%bottom - rect%top, &
        !        hwnd, &
        !        NULL, &
        !        ghInstance, &
        !        NULL)
        ghLogList = CreateWindowEx(WS_EX_CLIENTEDGE, &
            "LISTBOX" // C_NULL_CHAR, &
            NULL, &
            IOR(IOR(WS_CHILD, WS_VISIBLE), WS_VSCROLL), &
            0, &
            0, &
            rect%right - rect%left, &
            rect%bottom - rect%top, &
            hwnd, &
            NULL, &
            ghInstance, &
            NULL)
        if (ghLogList == 0) then
            err = GetLastError()
            iret = WriteMessageBox(hwnd, err, "Error creating List control", "Log List Box error")
        end if
            
        ! Make the Edit control use the Times New Roman font
        !HFONT CreateFont(          https://learn.microsoft.com/en-us/windows/win32/api/wingdi/nf-wingdi-createfonta
        !  [in] int    cHeight,
        !  [in] int    cWidth,
        !  [in] int    cEscapement,
        !  [in] int    cOrientation,
        !  [in] int    cWeight,         0 = FW_DONTCARE 100 = FW_THIN ... 400 = FW_NORMAL ... 900 = FW_BLACK
        !  [in] DWORD  bItalic,
        !  [in] DWORD  bUnderline,
        !  [in] DWORD  bStrikeOut,
        !  [in] DWORD  iCharSet,
        !  [in] DWORD  iOutPrecision,
        !  [in] DWORD  iClipPrecision,
        !  [in] DWORD  iQuality,
        !  [in] DWORD  iPitchAndFamily,
        !  [in] LPCSTR pszFaceName
        !);
        gFont = CreateFont(20, &
            0, &
            0, &
            0, &
            FW_NORMAL, &
            .FALSE., &
            .FALSE., &
            .FALSE., &
            ANSI_CHARSET, &
            OUT_DEFAULT_PRECIS, &
            CLIP_DEFAULT_PRECIS, &
            DEFAULT_QUALITY, &
            DEFAULT_PITCH, "Times New Roman" // C_NULL_CHAR)
        iret = SendMessage(ghLogList, WM_SETFONT, gFont, TRUE) ! Without iret = this will not build
       ! LogWndProc = 0  ! Indicate successful handling
       ! return
        
     case (WM_SIZE)

        iret = GetClientRect(ghLogMain, rect)  ! Get updated client area
        iret = MoveWindow(ghLogList, 0, 0, rect%right - rect%left, rect%bottom - rect%top, TRUE)
        LogWndProc = 0  ! Indicate successful handling
        return
        
     ! Let the default window proc handle all other messages
      case default
        LogWndProc = DefWindowProc( hWnd, mesg, wParam, lParam )

    end select
end   



!****************************************************************************
!
!  FUNCTION: MainWndProc ( hWnd, mesg, wParam, lParam )
!
!  PURPOSE:  Processes messages for the main window
!
!  COMMENTS:
!
!****************************************************************************

integer(LRESULT) function MainWndProc ( hWnd, mesg, wParam, lParam )
!DEC$ IF DEFINED(_X86_)
!DEC$ ATTRIBUTES STDCALL, ALIAS : '_MainWndProc@16' :: MainWndProc
!DEC$ ELSE
!DEC$ ATTRIBUTES STDCALL, ALIAS : 'MainWndProc' :: MainWndProc
!DEC$ ENDIF

    use ifwin       ! For GetCurrentThreadId
    use user32
    use NLevQGModelGlobals, only: ghInstance, ghWndMain, gBitMapInfo, ghCurrentDlg, WriteMessageBox, SIZEOFAPPNAME, LogMessage, CenterWindow
    use ConstantsModule, only: RUNNING, PAUSED, STOPPED, WEATHER_MAP, PHASE_AND_MAP, WEATHER_STN, VERT_CROSS_SEC_AT_LAT, VERT_CROSS_SEC_AT_LON, &
                               ZONAL_WAVE_ENERGY, PHASE_SPACE, VERT_HOV_AT_WEATHER_STN, HOV_DIAG_AT_LAT, HOV_DIAG_AT_LON, NO_PLOT, DEGREES_TO_RAD, &
                               VORTICITY, PLOTTING_OFF
    use UtilitiesModule, only: openModelFile
    use SetUpModule, only: SaveModelState, SetModRealParamsDimensionalState
    use ModelParamsModule, only: gNLevFile, gModelState, gModIntParams, gModRealParams
    use ModelFieldsModule, only: gVertArr
    use ModelModule, only: StartTimeLoop
    use PlotModule, only: ClearScreen, initializePlot, getYValues, gElapsedTime, gPressure

    implicit none

    integer(HANDLE) hWnd
    integer(UINT) mesg
    integer(UINT_PTR) wParam
    integer(LONG_PTR) lParam 
    
    integer(HANDLE) ::       hScreenDC
    integer(HANDLE) ::       hBrush
    integer(HANDLE) ::       hOldBrush

    include 'resource.fd'

    ! Variables
    type(T_RECT) :: rect

    ! For drawing
    INTEGER(HANDLE) :: hdc 
    TYPE(T_PAINTSTRUCT) :: ps
    type(T_BITMAPINFO) :: bmi
    INTEGER(INT_PTR_KIND()) :: pRawBits
    
    integer(BOOL)       bret
    integer(LRESULT)    lret
    integer:: ret
    character(SIZEOFAPPNAME)  lpszName, lpszHelpFileName, lpszContents, lpszMessage
    character(SIZEOFAPPNAME)  lpszHeader
    integer(HANDLE), save :: hHook

    ! Variables used for mousing over maps and graphs to display the values at that point
    integer, save :: mouse_x, mouse_y   ! Location of the mouse cursor
    logical :: maps, graphs             ! Mousing over a map or a graph? Need both since both can be present on the screen
    real, save :: current_val1, current_val2 ! Value at moused over location of field1 and field2
    character*96 :: textbuffer
    character*32, save :: valueStringField1, valueStringField2
    INTEGER(4) :: textLen
    TYPE(T_SIZE) :: sz
    integer :: draw_x, i, screenHeight
    real :: rlat, rlon
    integer :: years
    REAL, ALLOCATABLE, save :: X(:,:), X_Overlay(:, :)  ! These are the arrays containing interpolated values for weather maps. Created on heap.



    select case ( mesg )
        
        case (WM_DESTROY)
            ret = 0
            if (gNLevFile%fileHandle /= -1) close(gNLevFile%fileHandle)
            gNLevFile%fileName = 'NLEVQG.SAV'
            if (openModelFile(gNLevFile, .false., ghwndMain) == 0) ret = SaveModelState(gNLevFile, .true.)     ! Saves current model state to file and deallocates arrays
            if (ret .NE. 0) ret = WriteMessageBox(ghwndMain, ret, 'Unable to save model state', 'File IO error')
            call PostQuitMessage( 0 )
            ! Need to reverse the SelectObject call before deleting the objects to avoid memory leaks
            lret = SelectObject(gBitMapInfo%hMemDC, gBitMapInfo%hOldBitmap)
            lret = DeleteObject(gBitMapInfo%hBitmap)
            lret = DeleteDC(gBitMapInfo%hMemDC)
            MainWndProc = 0
            return

        ! WM_CREATE: 
        case (WM_CREATE)
            ! Set up a hook to trap key presses before the dialog message loop handles them
            ghwndMain = hWnd ! Identifier of the active window
                        ! Center the main window relative to the desktop
            call CenterWindow(hWnd, GetDesktopWindow())
            
            ! 1. Create a DC compatible with the screen
            hScreenDC = GetDC(hWnd)
            ! 2. Create the persistent Memory DC
            gBitMapInfo%hMemDC = CreateCompatibleDC(hScreenDC)
            
            lret =  GetClientRect(hWnd, rect)
            
            gBitMapInfo%rect = rect
            gBitMapInfo%width = rect%right - rect%left
            gBitMapInfo%height = rect%bottom - rect%top
            gBitMapInfo%startX = rect%left
            gBitMapInfo%startY = rect%top

            ! 3. Create the Bitmap using the pre-populated width/height
            bmi%bmiHeader%biSize        = SIZEOF(bmi%bmiHeader)
            bmi%bmiHeader%biWidth       = gBitMapInfo%width
            bmi%bmiHeader%biHeight      = -gBitMapInfo%height ! Negative for top-down (0,0 = top-left)
            bmi%bmiHeader%biPlanes      = 1
            bmi%bmiHeader%biBitCount    = 32                  ! 32-bit (BGRA)
            bmi%bmiHeader%biCompression = BI_RGB
            bmi%bmiHeader%biSizeImage   = 0

            gBitMapInfo%hBitmap = CreateDIBSection(hScreenDC, bmi, DIB_RGB_COLORS, pRawBits, 0, 0)
            ! 4. Store the pointer in your manager structure
            gBitMapInfo%pBits = TRANSFER(pRawBits, gBitMapInfo%pBits)
            
            ! 5. Select the bitmap into the DC and store the 'Old' bitmap for cleanup
            ! (Selection is a function that returns the replaced object handle)
            gBitMapInfo%hOldBitmap = SelectObject(gBitMapInfo%hMemDC, gBitMapInfo%hBitmap)
            
            ! To use a custom color (e.g., dark blue) instead of just BLACKNESS:
            hBrush = CreateSolidBrush(RGB(0, 30, 10))       ! Create the color brush
            hOldBrush = SelectObject(gBitMapInfo%hMemDC, hBrush) ! Put it in the DC
    
            ! 6. Clear the canvas to black initially
            lret = PatBlt(gBitMapInfo%hMemDC, 0, 0, &
                  gBitMapInfo%width, gBitMapInfo%height, PATCOPY)

            ! Cleanup the brushes
            lret = SelectObject(gBitMapInfo%hMemDC, hOldBrush)
            lret = DeleteObject(hBrush)


            ! 7. Release the Screen DC to the system pool (MANDATORY)
            lret = ReleaseDC(hWnd, hScreenDC)

            MainWndProc = 0
            return

        case (WM_PAINT)
            ! 1. Begin the paint process to get the window's DC
            hdc = BeginPaint(hWnd, ps)
 
            ! 2. Copy the entire persistent black canvas from memory to the screen
            lret = BitBlt(hdc, 0, 0, gBitMapInfo%width, gBitMapInfo%height, gBitMapInfo%hMemDC, 0, 0, SRCCOPY)
            
! =============================== MOUSE OVER CODE
            ! THis mess prints the moused-over value 'current_val' of the weather map to the screen in a separate 
            ! layer. Some extra variables are needed since the value will be printed to the left when
            ! the cursor is too close to the right side.
            maps = (gModIntParams%plotType == WEATHER_MAP .OR.  &
                    gModIntParams%plotType == PHASE_AND_MAP .OR. &
                    gModIntParams%plotType == VERT_CROSS_SEC_AT_LAT .OR. &
                    gModIntParams%plotType == VERT_CROSS_SEC_AT_LON)
            graphs = (gModIntParams%plotType == WEATHER_STN)

            ! Early exit 
            if ((gModelState%runState == RUNNING) .OR. &
               (.NOT. (maps .OR. graphs)) .OR. &
               (maps .AND. .NOT. (ALLOCATED(X) .OR. ALLOCATED(X_Overlay)))) then
                lret = EndPaint(hWnd, ps)
                MainWndProc = 0
                return
            endif
            
            ! --- 2. Calculate Content (The "What") This is the lat-lon for maps and the time for time series graphs
            !        The values of the two field are printed above and below this line
            textBuffer = ""
            if (maps) then
                screenHeight = gBitMapInfo%height / 2
                if (gModIntParams%plotType == VERT_CROSS_SEC_AT_LAT .OR. &
                        gModIntParams%plotType == VERT_CROSS_SEC_AT_LON) screenHeight = gBitMapInfo%height
                if (mouse_y >= screenHeight) then
                    lret = EndPaint(hWnd, ps)
                    MainWndProc = 0
                    return
                endif
                if (gModIntParams%plotType == VERT_CROSS_SEC_AT_LAT) then
                    ! Anyone of the vertical plot initializations types will have the same correct pressure field
                    rlat = gPressure(gBitMapInfo%height - REAL(mouse_y))
                    rLon = REAL(mouse_x * 360) / REAL(gBitMapInfo%width)
                    write(textBuffer, '(F5.1, A, F6.1, A)') rlon, " lon ", rlat, " mb"
                else if (gModIntParams%plotType == VERT_CROSS_SEC_AT_LON) then
                    rlon = gPressure(gBitMapInfo%height - REAL(mouse_y))
                    rlat = REAL(mouse_x * 90) / REAL(gBitMapInfo%width)
                    write(textBuffer, '(F5.1, A, F6.1, A)') rlat, " lat ", rlon, " mb"
                else
                    rlat = REAL(screenHeight - mouse_y) * 180.0 / REAL(gBitMapInfo%height)
                    rLon = REAL(mouse_x * 360) / REAL(gBitMapInfo%width)
                    write(textBuffer, '(F5.1, A, F5.1, A)') rlon, " lon ", rlat, " lat"
                endif
            else if (graphs) then
                if (current_val1 < 0.0) then
                    lret = EndPaint(hWnd, ps)
                    MainWndProc = 0
                    return
                end if
                years = Floor(current_val1 / 365.0)
                write(textBuffer, '(I0, ":", F0.2, " yrs:days")') years, current_val1 - (years * 365)
            end if

            ! --- 3. UI Positioning (The "Where") ---
            textLen = LEN_TRIM(textBuffer)
            ret = GetTextExtentPoint32(hDC, TRIM(textBuffer), textLen, sz)
            
            ! result = MERGE( value_if_true, value_if_false, condition )  Like C's ? operator.
            draw_x = merge(mouse_x - 15 - sz%cx, mouse_x + 15, mouse_x + 15 + sz%cx > gBitMapInfo%width)

            ! --- 4. Draw (The "How") ---
            ret = SetBkMode(hDC, TRANSPARENT)


            ! Center Row
            call QuickText(draw_x, mouse_y, textBuffer)

            if (graphs) then
                ret = MSFWIN$SetTextColor(hDC, RGB(10, 80, 10))
                call QuickText(draw_x, mouse_y, textBuffer) ! Center Row
                call QuickText(draw_x, mouse_y - 14, valueStringField1)
                call QuickText(draw_x, mouse_y + 14, valueStringField2)
            else if (maps) then
                ret = MSFWIN$SetTextColor(hDC, RGB(10, 10, 10))
                call QuickText(draw_x, mouse_y, textBuffer) ! Center Row
                call FormatField(textBuffer, gModIntParams%field1, current_val1)
                call QuickText(draw_x, mouse_y - 14, textBuffer)
                call FormatField(textBuffer, gModIntParams%field2, current_val2)
                call QuickText(draw_x, mouse_y + 14, textBuffer)
            end if

            ! 3. End the paint process
            lret = EndPaint(hWnd, ps)

            MainWndProc = 0
            return
            
            CASE (WM_MOUSEMOVE)
                if (GetForegroundWindow() .NE. hWnd .or. gModelState%runState /= PAUSED) return
                mouse_x = IAND(lParam, #FFFF)
                mouse_y = ISHFT(lParam, -16)
                maps = (gModIntParams%plotType == WEATHER_MAP .OR.  &
                        gModIntParams%plotType == PHASE_AND_MAP .OR. &
                        gModIntParams%plotType == VERT_CROSS_SEC_AT_LAT .OR. &
                        gModIntParams%plotType == VERT_CROSS_SEC_AT_LON)
                graphs = (gModIntParams%plotType == WEATHER_STN)
                if (maps) then
                    screenHeight = gBitMapInfo%height / 2
                    if (gModIntParams%plotType == VERT_CROSS_SEC_AT_LAT .OR. & 
                        gModIntParams%plotType == VERT_CROSS_SEC_AT_LON) screenHeight = gBitMapInfo%height
                    ! 1. Ensure data is ready (only happens once per plot change)
                    if (.NOT. ALLOCATED(X) .AND. .NOT. ALLOCATED(X_Overlay)) then
                         call SetModRealParamsDimensionalState(.false.)
                         call PrepareGrid(X, gModIntParams%field1, screenHeight)
                         call PrepareGrid(X_Overlay, gModIntParams%field2, screenHeight)
                    end if
            
                    ! 2. Sample data if within bounds
                    if (mouse_y >= 0 .and. mouse_y < screenHeight) then
                        if (ALLOCATED(X))         current_val1 = X(mouse_x + 1, screenHeight - mouse_y)
                        if (ALLOCATED(X_Overlay)) current_val2 = X_Overlay(mouse_x + 1, screenHeight - mouse_y)
                        ret = InvalidateRect(hWnd, NULL, .FALSE.)
                    end if
            
                else if (graphs) then
                    i = gModIntParams%outputInterval * (mouse_x - (gBitMapInfo%width - 1)) + gElapsedTime%timeSteps
                    current_val1 = REAL(i) * gElapsedTime%dimensionalTime
                    valueStringField1 = getYValues(mouse_y, gModIntParams%field1)
                    valueStringField2 = getYValues(mouse_y, gModIntParams%field2)
                    ret = InvalidateRect(hWnd, NULL, .FALSE.)
                end if


        case (WM_ERASEBKGND)
            ! Tell Windows: "Don't erase me, I've got it covered with the BitBlt."
            MainWndProc = 1  ! Return non-zero to indicate you handled it
            return
            
        CASE (WM_LBUTTONDOWN)
            ! Release mouse capture and trick the system into a "Non-Client" drag
            lret = ReleaseCapture()
            ! 161 is WM_NCLBUTTONDOWN; 2 is HTCAPTION
            lret = SendMessage(hWnd, 161, 2, 0) 
            MainWndProc = 0
            RETURN
            
        case (WM_SYSKEYUP)
            ! Will cause the model to quit on the next plot check.
            if (wParam == VK_F10) then
                if (gModelState%runState .NE. STOPPED) then
                    gModelState%runState = STOPPED
                    call LogMessage("User stopped run using F10")
                else
                    call LogMessage("User returning to ModelControl Dialog via F10")
                    ret = EnableWindow(ghCurrentDlg, .TRUE.)
                    ret = SetFocus(ghCurrentDlg)
                    ret = SetActiveWindow(ghCurrentDlg)

                    gElapsedTime%timeSteps = 0 ! Reset the elapsed time displayed in plots
                    if(ALLOCATED(X)) then
                        call LogMessage("DeAllocated interpolated grid X on F10")
                        deallocate(X)
                    endif
                    if(ALLOCATED(X_Overlay)) then
                        call LogMessage("DeAllocated interpolated grid X_overlay on F10")
                        deallocate(X_Overlay)
                    endif
                endif
            endif
            MainWndProc = 0
            return
            
        case (WM_KEYUP)
            ! wParam contains the Virtual Key code
            select case (wParam)
    ! F1: PAUSE/RESUME -----------------------
                case (VK_F1)
                    if (gModelState%runState .NE. RUNNING) then
                        if(ALLOCATED(X)) then
                            call LogMessage("DeAllocated interpolated X grid on F1")
                            deallocate(X)
                        endif
                        if(ALLOCATED(X_Overlay)) then
                            call LogMessage("DeAllocated interpolated x_Overlay grid on F1")
                            deallocate(X_Overlay)
                        endif
                        call SetModRealParamsDimensionalState(.false.)
                        call LogMessage("User resuming run")
                        if (gModelState%runState == PAUSED) then
                            gModelState%runState = RUNNING
                        else
                            ret = StartTimeLoop()
                        endif
                    else
                        gModelState%runState = PAUSED
                        call LogMessage("User pausing run")
                    endif
    ! F2: Change Plot type -----------------------
                case (VK_F2)
                    ! Check if the SHIFT key is pressed
                    ! GetKeyState returns a negative value if the high-order bit is set
                    if (GetKeyState(VK_SHIFT) < 0) then
                        gModIntParams%plotType = gModIntParams%plotType - 1
                        if (gModIntParams%plotType .EQ. ZONAL_WAVE_ENERGY .AND. gModIntParams%KMAX2 < 3) gModIntParams%plotType = PHASE_SPACE
                        if (gModIntParams%plotType .LT. WEATHER_MAP) gModIntParams%plotType = NO_PLOT
                    else
                        gModIntParams%plotType = gModIntParams%plotType + 1
                        if (gModIntParams%plotType .EQ. ZONAL_WAVE_ENERGY .AND. gModINtParams%KMAX2 < 3) gModIntParams%plotType = WEATHER_STN
                        if (gModIntParams%plotType .GT. NO_PLOT) gModIntParams%plotType = WEATHER_MAP
                    end if
                    if (ALLOCATED(X)) deallocate(X)     ! Otherwise we will have an old X and a new one wont be created
                                                        ! if we switch to weather maps
                    if (ALLOCATED(X_Overlay)) deallocate(X_Overlay)
                    current_val1 = -1.0
                    call initializePlot()
                    call ClearScreen()
                    if (gModelState%runState .NE. RUNNING) then
                        call LogMessage("Plot type changed while model was stopped or paused. Restarting model")
                        call SetModRealParamsDimensionalState(.false.)
                        ret = StartTimeLoop()
                    end if

    ! F3: Clear Screen -----------------------
                case (VK_F3)
                    call ClearScreen()
                    call LogMessage("User cleared screen")
    ! F4: Change Field 1 -----------------------
                case (VK_F4)
                    if (gModIntParams%plotType .NE. ZONAL_WAVE_ENERGY) then
                        if (GetKeyState(VK_SHIFT) < 0) then
                            gModIntParams%field1 = gModIntParams%field1 - 1
                            if (gModIntParams%field1 .LT. VORTICITY) gModIntParams%field1 = PLOTTING_OFF
                        else
                            gModIntParams%field1 = gModIntParams%field1 + 1
                            if (gModIntParams%field1 .GT. PLOTTING_OFF) gModIntParams%field1 = VORTICITY
                        endif
                        call initializePlot()
                        call ClearScreen()                        
                        call LogMessage("User changed field 1")
                    endif
    ! F5: Change Field 1 -----------------------
                case (VK_F5)
                    if (gModIntParams%plotType .NE. ZONAL_WAVE_ENERGY) then
                        if (GetKeyState(VK_SHIFT) < 0) then
                            gModIntParams%field2 = gModIntParams%field2 - 1
                            if (gModIntParams%field1 .LT. VORTICITY) gModIntParams%field1 = PLOTTING_OFF
                        else
                            gModIntParams%field2 = gModIntParams%field2 + 1
                            if (gModIntParams%field2 .GT. PLOTTING_OFF) gModIntParams%field2 = VORTICITY
                        endif
                        call initializePlot()
                        call ClearScreen()                        
                        call LogMessage("User changed field 1")
                    endif
    ! Up/Down: Change Levels or move lat/lon in vertical cross sections
                case (VK_UP)
                    if (gModIntParams%plotType == VERT_CROSS_SEC_AT_LAT) then
                        call LogMessage("User moving poleward 5 degrees Latitude")
                        gModRealParams%hovLat = gModRealParams%hovLat + 5.0 * DEGREES_TO_RAD
                        if (gModRealParams%hovLat .GT. 90.0 * DEGREES_TO_RAD) gModRealParams%hovLat = 0.

                    else if (gModIntParams%plotType == VERT_CROSS_SEC_AT_LON) then
                        call LogMessage("User moving eastward 5 degrees Longitude")
                        gModRealParams%hovLon = gModRealParams%hovLon + 5.0 * DEGREES_TO_RAD
                        if (gModRealParams%hovLon .GT. 360.0 * DEGREES_TO_RAD) gModRealParams%hovLon = 0.

                    else if (gModIntParams%plotType == VERT_HOV_AT_WEATHER_STN) then
                        call LogMessage("User moving poleward 5 degrees Latitude")
                        gModRealParams%latPoint= gModRealParams%latPoint + 5.0 * DEGREES_TO_RAD
                        if (gModRealParams%latPoint .GT. 90.0 * DEGREES_TO_RAD) gModRealParams%latPoint = 0.
                    else
                        call LogMessage("User moving up a level")
                        gModIntParams%LEVP = gModIntParams%LEVP + 1
                        if (gModIntParams%LEVP > SIZE(gVertArr%LVS)) gModIntParams%LEVP = 1
                    endif
                    ! Need to recompute the specific plot types struct values
                    call initializePlot()
                    call ClearScreen()
                case (VK_DOWN)
                    if (gModIntParams%plotType == VERT_CROSS_SEC_AT_LAT) then
                        call LogMessage("User moving equatorward 5 degrees Latitude")
                        gModRealParams%hovLat = gModRealParams%hovLat - 5.0 * DEGREES_TO_RAD
                        if (gModRealParams%hovLat .LT. 0.0) gModRealParams%hovLat = 90.0 * DEGREES_TO_RAD

                    else if (gModIntParams%plotType == VERT_CROSS_SEC_AT_LON) then
                        call LogMessage("User moving westward 5 degrees Longitude")
                        gModRealParams%hovLon = gModRealParams%hovLon - 5.0 * DEGREES_TO_RAD
                        if (gModRealParams%hovLon .LT. 0.0) gModRealParams%hovLon = 360. * DEGREES_TO_RAD

                    else if (gModIntParams%plotType == VERT_HOV_AT_WEATHER_STN) then
                        call LogMessage("User moving equatorward 5 degrees Latitude")
                        gModRealParams%latPoint = gModRealParams%latPoint - 5.0 * DEGREES_TO_RAD
                        if (gModRealParams%latPoint .LT. 0.0) gModRealParams%latPoint = 90.* DEGREES_TO_RAD
                    else 
                        call LogMessage("User moving down a level")
                        gModIntParams%LEVP = gModIntParams%LEVP - 1
                        if (gModIntParams%LEVP < 1) gModIntParams%LEVP = SIZE(gVertArr%LVS)
                    endif
                    ! Need to recompute the specific plot types struct values
                    call initializePlot()
                    call ClearScreen()
                ! Left arrow key
                ! Vertical Hovmoller at point moves longitude point WESTWARD (Poleward is done by up arrow)
                ! Hovmoller at latitude moves latitude EQUATORWARD
                ! Hovmoller at longitude moves longitude WESTWARD
                case (VK_LEFT)
                    if (GetKeyState(VK_SHIFT) < 0 .AND. gModIntParams%plotType == WEATHER_STN) then
                        call LogMessage("User moving equatorward 5 degrees Latitude")
                        gModRealParams%latPoint = gModRealParams%latPoint - 5.0 * DEGREES_TO_RAD
                        if (gModRealParams%latPoint .LT. 0.0) gModRealParams%latPoint = 90.0 * DEGREES_TO_RAD
                    else
                        if (gModIntParams%plotType == VERT_HOV_AT_WEATHER_STN .OR. gModIntParams%plotType == WEATHER_STN) then
                            call LogMessage("User moving westward 5 degrees Longitude")
                            gModRealParams%lonPoint= gModRealParams%lonPoint - 5.0 * DEGREES_TO_RAD
                            if (gModRealParams%lonPoint .LT. 0.0) gModRealParams%lonPoint = 360. * DEGREES_TO_RAD

                        else if (gModIntParams%plotType == HOV_DIAG_AT_LAT) then
                            call LogMessage("User moving equatorward 5 degrees Latitude")
                            gModRealParams%hovLat = gModRealParams%hovLat - 5.0 * DEGREES_TO_RAD
                            if (gModRealParams%hovLat .LT. 0.0) gModRealParams%hovLat = 90.0 * DEGREES_TO_RAD

                        else if (gModIntParams%plotType == HOV_DIAG_AT_LON) then
                            call LogMessage("User moving westward 5 degrees Longitude")
                            gModRealParams%hovLon = gModRealParams%hovLon - 5.0 * DEGREES_TO_RAD
                            if (gModRealParams%hovLon .LT. 0.0) gModRealParams%hovLon = 360.0 * DEGREES_TO_RAD
                        else
                            return
                        endif
                    endif
                    call initializePlot()
                    call ClearScreen()
                    
                ! Right arrow key shift handles the equatorward/poleward shift of the weather station
                ! Vertical Hovmoller at point moves latitude point EASTWARD (equatorward is done by down arrow)
                ! Hovmoller at latitude moves latitude POLEWARD
                ! Hovmoller at longitude moves longitude EASTWARD
                case (VK_RIGHT)
                    if (GetKeyState(VK_SHIFT) < 0 .AND. gModIntParams%plotType == WEATHER_STN) then
                        call LogMessage("User moving poleward 5 degrees Latitude")
                        gModRealParams%latPoint = gModRealParams%latPoint + 5.0 * DEGREES_TO_RAD
                        if (gModRealParams%latPoint .GT. 90.0 * DEGREES_TO_RAD) gModRealParams%latPoint = 0.0
                    else
                        if (gModIntParams%plotType == VERT_HOV_AT_WEATHER_STN .OR. gModIntParams%plotType == WEATHER_STN) then
                            call LogMessage("User moving eastward 5 degrees Longitude")
                            gModRealParams%lonPoint= gModRealParams%lonPoint + 5.0 * DEGREES_TO_RAD
                            if (gModRealParams%lonPoint .GT. 360.0 * DEGREES_TO_RAD) gModRealParams%lonPoint = 0.0

                        else if (gModIntParams%plotType == HOV_DIAG_AT_LAT) then
                            call LogMessage("User moving poleward 5 degrees Latitude")
                            gModRealParams%hovLat = gModRealParams%hovLat + 5.0 * DEGREES_TO_RAD
                            if (gModRealParams%hovLat .GT. 90.0 * DEGREES_TO_RAD) gModRealParams%hovLat = 0.0

                        else if (gModIntParams%plotType == HOV_DIAG_AT_LON) then
                            call LogMessage("User moving eastward 5 degrees Longitude")
                            gModRealParams%hovLon = gModRealParams%hovLon + 5.0 * DEGREES_TO_RAD
                            if (gModRealParams%hovLon .GT. 360.0 * DEGREES_TO_RAD) gModRealParams%hovLon = 0.0
                        else
                            return
                        endif
                    endif
                    call initializePlot()
                    call ClearScreen()
                case (VK_ESCAPE)
                    call PostQuitMessage(0)
            end select

            MainWndProc = 0
            return


        ! WM_COMMAND: user command
        case (WM_COMMAND)
            select case ( IAND(wParam, 16#ffff ) )

                ! no menu options 

                case DEFAULT
                    MainWndProc = DefWindowProc( hWnd, mesg, wParam, lParam )
                    return
            end select

        ! Let the default window proc handle all other messages
        case default
            MainWndProc = DefWindowProc( hWnd, mesg, wParam, lParam )

    end select


    contains
        ! The variables declared in function are visible in these 'internal' methods 
        ! Helpers for WM_PAINT
        subroutine QuickText(x, y, str)
            use IFWBASE ! Contains RGB macro
            use IFWIN, only: MSFWIN$SetTextColor
            use user32
            USE GDI32
    
            implicit none

            integer, intent(in) :: x, y
            character(len=*), intent(in) :: str

            integer :: tLen
            integer(bool) ret
    
            tLen = LEN_TRIM(str)
            if (tLen > 0 .AND. str(1:1) /= char(0)) ret = TextOut(hDC, x, y, TRIM(str), tLen)
        end subroutine

        subroutine FormatField(buf, field, val)
            use ConstantsModule, only: VORTICITY, HEIGHT, TEMPERATURE, VERTICAL_WIND, TEMP_FORCING, TOT_HGT_TEND, ADV_BETA_HGT_TEND, &
                OMEGA_HGT_TEND, DIABATIC_HGT_TEND, TOT_TEMP_TEND, ADV_TEMP_TEND, OMEGA_TEMP_TEND, DIABATIC_TEMP_TEND, HGT_FORCING, &
                OROG_HGT_TEND, OROGRAPHY

            implicit none

            character(len=*), intent(out) :: buf
            integer, intent(in) :: field
            real, intent(in) :: val

            select case (field)     ! semi colon allows more than one line of code on a single line like in C
                case (VORTICITY);   write(buf, '("Vorticity ", F10.2, " 10**-5/s")') val
                case (HEIGHT);      write(buf, '("Heights ", F10.2, " m")') val
                case (TEMPERATURE); write(buf, '("Temps ",F10.2, A, "C")') val, char(176)
                case (VERTICAL_WIND); write(buf, '("Omega ",F10.2, " mb/hr")') val
                case (HGT_FORCING); write(buf, '("Height driving ", F10.2, " m")') val
                case (TOT_HGT_TEND); write(buf, '("Tot height tend ", F10.2, " m/day")') val
                case (ADV_BETA_HGT_TEND); write(buf, '("Adv-beta height tend ", F10.2, " m/day")') val
                case (DIABATIC_HGT_TEND); write(buf, '("Diab height tend ", F10.2, " m/day")') val
                case (OMEGA_HGT_TEND); write(buf, '("Omega height tend ", F10.2, " m/day")') val
                case (TOT_TEMP_TEND); write(buf, '("Tot temp tend ", F10.2, A, "C/day")') val, char(176)
                case (TEMP_FORCING); write(buf, '("Temp driving ", F10.2, A, "C")') val, char(176)
                case (ADV_TEMP_TEND); write(buf, '("Adv temp tend ", F10.2, A, "C/day")') val, char(176)
                case (DIABATIC_TEMP_TEND); write(buf, '("Diab temp tend ", F10.2, A, "C/day")') val, char(176)
                case (OMEGA_TEMP_TEND); write(buf, '("Omega temp tend ", F10.2, A, "C/day")') val, char(176)
                case (OROGRAPHY); write(buf, '("Orog ", F10.2, " m")') val
                case (OROG_HGT_TEND); write(buf, '("Orog hgt tend ", F10.2, " m/day")') val
                case (PLOTTING_OFF); write(buf, '("plotting off")')
                case default;       buf = ""
            end select
        end subroutine

        ! Helper for MOUSE_MOVE
        subroutine PrepareGrid(X, field, screenHeight)
            use ConstantsModule, only: PLOTTING_OFF
            use PlotModule, only: populateInterpolatedGrid

            implicit none

            REAL, ALLOCATABLE, INTENT(INOUT) :: X(:,:)
            INTEGER, INTENT(IN) :: field, screenHeight
        
            if (field == PLOTTING_OFF) return

            allocate(X(gBitMapInfo%width, screenHeight))

            call populateInterpolatedGrid(X, field)
        end subroutine

end function MainWndProc



