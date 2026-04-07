!  NlevelQGModel.f90
!
! Model defines the streamfunction at the center of the N levels and the temperature at the interface between the levels
! Thus PSI(n + 1) - PSI(n) = TH(n + .5)
! However, we are not going to deal with half-levels. TH(n) located at the interface between PSI(n) and PSI(n + 1).
!
!  FUNCTIONS:
!   WinMain()      - Entry point for the application;
!                    displays the main window; processes the message loop
!   MainWndProc()  - processes messages for the main window
!   CenterWindow() - centers one window over another
!   SettingsDlgProc() - processes messages for the about box
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
    
    use NlevelQGModelGlobals
    use WndProcInterfacesModule
    use DlgProcInterfacesModule
    use KeyboardHooksModule

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

    ret = SaveModelState()     ! Saves current model state to file
    if (ret .NE. 0) ret = WriteMessageBox(ghwndMain, ret, 'Unable to save model state', 'File IO error')
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
    use NlevelQGModelGlobals
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
    use NlevelQGModelGlobals
    use DlgProcInterfacesModule
    use KeyboardHooksModule
    use ConstantsModule, only: INVALID

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
    character(SIZEOFAPPNAME)  lpszName, lpszHelpFileName, lpszContents, lpszMessage
    character(SIZEOFAPPNAME)  lpszHeader
    integer(HANDLE) :: hHook
    save hHook

    select case ( mesg )
        
        case (WM_DESTROY)
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
            ! 3. End the paint process
            lret = EndPaint(hWnd, ps)

            MainWndProc = 0
            return


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
end



