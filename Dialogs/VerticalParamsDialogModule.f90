module VerticalParamsDialogModule
    implicit none
    
    contains
        !/****************************************************************************
        !
        !  FUNCTION: VerticalParamsDlgProc(HWND, UINT, WPARAM, LPARAM)
        !
        !  PURPOSE:  Processes messages for "Settings" dialog box
        !
        !  COMMENTS: Display version information from the version section of the
        !            application resource.  Wait for user to click on "Ok" button,
        !            then close the dialog box.
        !
        !****************************************************************************/
        function VerticalParamsDlgProc( hDlg, dlgMsg, wParam, lParam )
        !DEC$ IF DEFINED(_X86_)
        !DEC$ ATTRIBUTES STDCALL, ALIAS : '_VerticalParamsDlgProc@16' :: VerticalParamsDlgProc
        !DEC$ ELSE
        !DEC$ ATTRIBUTES STDCALL, ALIAS : 'VerticalParamsDlgProc' :: VerticalParamsDlgProc
        !DEC$ ENDIF

            use ifwinty
            use ifwin
            use kernel32
            use user32
            use gdi32

            use NlevelQGModelGlobals, only: ghVerticalParamsDlg, CenterWindow, LogMessage
            use iso_c_binding

            use DialogHelpersModule, only: GetComboIndex, moveEditBox, initListView, AddListViewRow, ResizeListViewToFitItems
            use ModelParamsModule, only: gUserSettings
            use KeyboardHooksModule, only: KeyboardForDlgVerticalParamsProc
            use ModelFieldsModule, only: vertArr



            implicit none

            integer(LRESULT) ::  VerticalParamsDlgProc ! LRESULT
            integer(HANDLE) ::   hDlg        ! window handle of the dialog box
            integer(UINT) ::     dlgMsg
            integer(UINT_PTR) :: wParam
            integer(LONG_PTR) :: lParam

            include 'resource.fd'

            integer(KIND=4) :: lData
            integer(kind=4) :: iData
            integer(kind=4) :: iret
            integer(BOOL) ::   bret
            integer(HANDLE) :: hHook
            save hHook
    
            type(T_RECT) :: rc
            type(T_POINT) :: pt

            ! For the list view report rows
            integer(HANDLE) :: hList, hEdit, hStaticEdit, hCombo, hStaticCombo, hDoneButton
            integer :: rowClicked
            save rowClicked
            CHARACTER(LEN=16) :: buffer ! Use this to read the value in the edit box

            integer :: newValue      ! For editing the displayed value in the list box

            select case (dlgMsg)
                case (WM_INITDIALOG)   ! dlgMsg: initialize dialog box
                    rowClicked = -1
                    ! Set up a hook to trap key presses before the dialog message loop handles them
                    hHook = SetWindowsHookEx(WH_KEYBOARD, LOC(KeyboardForDlgVerticalParamsProc), NULL, GetCurrentThreadId())
                    ghVerticalParamsDlg = hDlg
                    !hEdit = GetDlgItem(hDlg, IDC_EDIT_USERSETTINGS)
                    !bret = SendMessage(hEdit, WM_SETFONT, gFont, .TRUE.)
                    !hStaticEdit = GetDlgItem(hDlg, IDC_STATIC_EDIT_SETTINGS)
                    !bret = SendMessage(hStaticEdit, WM_SETFONT, gFont, .TRUE.)

                    ! Combo boxes require manual editing of their height if you want the list of options
                    ! to be exposed on click only.
                    ! Integration Method
                    hCombo = GetDlgItem(hDlg, IDC_COMBO_USERSETTINGS1)
                    !bret = SendMessage(hCombo, WM_SETFONT, gFont, .TRUE.)
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Use interaction coefficients'//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Use spectral transforms'//char(0)))
                    ! Initialize the combo box
                    iret = 0
                    if (gUserSettings%integrationMethodTransform) iret = 1
                    bret = SendMessage(hCombo, CB_SETCURSEL, INT(iret, UINT_PTR), 0)    ! Cast avoids warning. Not necessary to run
                    hStaticCombo = GetDlgItem(hDlg, IDC_STATIC_COMBO_SETTINGS1)
                    !bret = SendMessage(hStaticCombo, WM_SETFONT, gFont, .TRUE.)

                    ! Statistics Options
                    hCombo = GetDlgItem(hDlg, IDC_COMBO_USERSETTINGS2)
                    !bret = SendMessage(hCombo, WM_SETFONT, gFont, .TRUE.)
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('None'//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Time averages'//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Time averages and variances'//char(0)))
                    ! Initialize the combo box
                    bret = SendMessage(hCombo, CB_SETCURSEL, INT(gUserSettings%statsOption, UINT_PTR), 0) ! Cast gets rid of warning. Not needed to run.
                    hStaticCombo = GetDlgItem(hDlg, IDC_STATIC_COMBO_SETTINGS2)
                    !bret = SendMessage(hStaticCombo, WM_SETFONT, gFont, .TRUE.)
                    
                    call handleIntListControl(hDlg)

                    ! Center the dialog over the application window
                    call CenterWindow (hDlg, GetWindow (hDlg, GW_OWNER))

                    VerticalParamsDlgProc = 1
                    return

                case (WM_NOTIFY)
                    BLOCK
                        type(c_ptr) :: p_raw
                        type(T_NM_LISTVIEW), pointer :: pnmv
                        integer(kind=4) :: i, j, prevRowClicked
                        integer(kind=8) :: lp
                        integer :: ios
                        character(len=16) :: str
                        ! Step 1: cast lParam to a C_PTR
                        lp = lParam
                        p_raw = transfer(lp, p_raw)
                        ! Step 2: cast to a Fortran structure pointer
                        call c_f_pointer(p_raw, pnmv)
                        if (pnmv%hdr%idFrom == IDC_LIST_LEVELS) then
                            select case (pnmv%hdr%code)
                                case (NM_CLICK)
                                    i = pnmv%isubItem
                                    j = pnmv%iitem
                                    WRITE(buffer, '(I0, 2X, I0)') i, j
                                    call LogMessage("Real: pnmv%isubItem (i) and pnmv%iitem (j) have values " //TRIM(buffer)//CHAR(0))
                                    if (j >= 0) then
                                        !intRowClicked = -1 ! Reset integer row clicked
                                        hList = GetDlgItem(hDlg, IDC_LIST_LEVELS)
                                        hEdit = GetDlgItem(hDlg, IDC_EDIT_VERTICAL)
                                        ios = rowClicked
                                        rowClicked = SendMessage(hList, LVM_GETNEXTITEM, -1, LVNI_SELECTED)
                                        if (ios .EQ. rowClicked) then
                                            bret = SetFocus(hEdit)
                                            bret = SetWindowPos(hEdit, 0, 0, 0, 0, 0, SWP_NOMOVE .OR. SWP_NOSIZE .OR. SWP_FRAMECHANGED .OR. SWP_NOZORDER)
                                            return
                                        end if
                                        ! Hides the integer edit box if it is visible
                                        !bret = ShowWindow(GetDlgItem(hDlg, IDC_EDIT_INTEGERVALUES), SW_HIDE)
                                        ! Moves the edit box over the value and focuses it
                                        call moveEditBox(hDlg, hList, hEdit, rowClicked)

                                        ! Method picks the correct real parameter to convert to string for edit box
                                        str = getStringFromRealForEditBox(j)
                                        bret = SetWindowPos(hEdit, 0, 0, 0, 0, 0, SWP_NOMOVE .OR. SWP_NOSIZE .OR. SWP_FRAMECHANGED .OR. SWP_NOZORDER)
                                    end if
                                case default
                                    VerticalParamsDlgProc = 1
                                    return
                            end select
                            bret = SetDlgItemText(hDlg, IDC_EDIT_VERTICAL, str)
                        end if
                    end BLOCK

                case (WM_COMMAND)                      ! dlgMsg: received a command
                    !loWord = IAND(iData, Z'FFFF')
                    !hiWord = ISHFT(iData, -16)
                    ! Combo box change
                    if (ISHFT(wParam, -16) == CBN_SELCHANGE) then
                        select case(IAND(wParam, Z'FFFF'))
                            case (IDC_COMBO_USERSETTINGS1)
                                gUserSettings%integrationMethodTransform = GetComboIndex(hDlg, IDC_COMBO_USERSETTINGS1) 
                            case (IDC_COMBO_USERSETTINGS2)
                                gUserSettings%statsOption = GetComboIndex(hDlg, IDC_COMBO_USERSETTINGS2) 
                        end select
                    end if

                    select case (IAND(wParam, 16#FFFF))

                        case (IDC_BUTTON_EXIT_VERTICAL) ! F10 Close
                            bret = EndDialog(hDlg, 0)
                            VerticalParamsDlgProc = 1

                        case (IDOK)
                            bret = EndDialog(hDlg, IDOK)
                            VerticalParamsDlgProc = 1

                        case (IDCANCEL)
                            bret = EndDialog(hDlg, IDCANCEL)
                            VerticalParamsDlgProc = 1

                        ! Get result of user editing real values
                        ! Recall that for the moved the edit control to work correctly the RC file needs to be manually edit
                        ! DO THE FOLLOWING two things:
                        ! 1. Add | WS_CLIPSIBLINGS to all list controls and corresponding edit boxes.
                        ! 2. Move the EDIT controls BEFORE the list controls.
                        case (IDC_EDIT_VERTICAL)
                            SELECT CASE (ISHFT(wParam, -16))
                                CASE (EN_CHANGE)
                                    EN: BLOCK
                                        character(LEN=16) :: strC ! C-string will have null terminator
                                        integer :: ios
                                        CHARACTER(:), ALLOCATABLE :: str ! fortran string with no null terminator
                                        call LogMessage("EN_CHANGE signaled from edit real control ")

                                        hEdit = GetDlgItem(hDlg, IDC_EDIT_VERTICAL)
                                        newValue = GetDlgItemText(hDlg, IDC_EDIT_VERTICAL, strC, LEN(strC))
                                        str = strC(1 : newValue) ! Clean out the junk
                                        if (ADJUSTL(TRIM(str)) == '') exit EN ! User typed enter
                                        hList = GetDlgItem(hDlg, IDC_LIST_LEVELS)
                                        ! This should be the row that was hilighted when we clicked on it. Of
                                        ! course the original value of 'realRowClicked' is lost when callback returned
                                        rowClicked = SendMessage(hList, LVM_GETNEXTITEM, -1, LVNI_SELECTED)
                                        ios = 0
                                        ! Load the new value into the correct modRealParam field
                                        ios = readRealDataFromEditBox(str, rowClicked)
                                        !write(*, '(A, A)') "Real value 3: ", str
                                        if (ios .NE. 0) then
                                            !ret = WriteMessageBox(hDlg, ios, "Input is not a valid real number", "Input Error")
                                            strC = getStringFromRealForEditBox(rowClicked)
                                            bret = SetDlgItemText(hDlg, IDC_EDIT_VERTICAL, strC)
                                            exit EN
                                        end if
                                        call updateListViewValue(hEdit, hList, rowClicked, strC)
                                    end BLOCK EN
                            end SELECT
                    end select

                case (WM_CLOSE)
                    bret = EndDialog(hDlg, 0)
                    VerticalParamsDlgProc = 1

                case (WM_DESTROY)
                    !bret = WriteCurrentStateToFile(hDlg, gModelSaveFile, gModelWelford, .true.)
                    bret = UnhookWindowsHookEx(hHook)
                    ghVerticalParamsDlg = 0
            end select
            VerticalParamsDlgProc = 0 ! Didn't process the dialog message dlgMsg
        end function VerticalParamsDlgProc
        

        subroutine handleIntListControl(hDlg)
            use ifwinty
            use ifwin
            use DialogHelpersModule, only: EnsureListViewColumns, ResizeListViewToFitItems
            implicit none
            
            include '../resource.fd'

            integer(HANDLE), intent(in) :: hDlg

            integer(HANDLE) :: hList            
            integer(bool) :: bret
            integer :: widths(2)
            type(T_RECT) :: rc
            type(T_POINT) :: pt
            
            hList = GetDlgItem(hDlg, IDC_LIST_LEVELS)
            bret = SendMessage(hList, LVM_DELETEALLITEMS, 0, 0) ! Clear first
            ! Load everything into the int list view
            bret = GetClientRect(hList, rc)
            widths(1) = 132  ! This is a hard coded width for the first column. It will only contain 'Level n'.
            widths(2) = rc%right - rc%left - widths(1)  ! buffer for margin/scrollbar is 16 but that is not to be there
            call EnsureListViewColumns(hList, ["Levels", "Pressure"], widths)
            call loadLevelsIntItemsIntoListView(hList)
            bret = UpdateWindow(hList)

            ! Getting the current position and width of the ListView control in the dialog
            bret = GetWindowRect(hList, rc) ! Gets the ListView rectangle coordinates (this is screen coordinates)
            pt%x = rc%left  ! A T_POINT struct is needed to convert the screen coordinates to dlg coordinates
            pt%y = rc%top
            bret = ScreenToClient(hDlg, pt) ! Converts the screen pt struct coordinates to dialog relative coordinates
            ! The makes the height sized to encompass the actual number of rows added
            call ResizeListViewToFitItems(hList, pt%x, pt%y, rc%right - rc%left)
        end subroutine handleIntListControl
        
        subroutine updateListViewValue(hEdit, hList, rowClicked, strC)
            use ifwinty
            use user32, only: SendMessage, SetWindowPos
            
            integer, intent(in) :: rowClicked
            integer(HANDLE), intent(in) :: hList, hEdit
            character(*), intent(in) :: strC

            type(T_LVITEM) :: lvItem
            integer(BOOL) ::  bret

            lvItem%mask = LVIF_TEXT
            lvItem%iItem = rowClicked
            lvItem%iSubItem = 1 ! Our list view has two columns, 0 and 1. The value is in column 1.
            lvItem%pszText = loc(strC)
            ! LVM_SETITEMTEXT should work here but it does not. It causes row 0 to get updated regardless
            ! LVM_SETITEM does work. Don't ask me why. Many days of frustration
            bret = SendMessage(hList, LVM_SETITEM, 0, loc(lvItem))

            ! This line redrawns the edit box borders which otherwise vanish.
            bret = SetWindowPos(hEdit, 0, 0, 0, 0, 0, SWP_NOMOVE .OR. SWP_NOSIZE .OR. SWP_FRAMECHANGED .OR. SWP_NOZORDER)
        end subroutine updateListViewValue
        
        subroutine loadLevelsIntItemsIntoListView(hList)
            use ModelParamsModule, only: gModIntParams
            use ModelFieldsModule, only: vertArr
            use DialogHelpersModule, only : AddListViewRowReal
            use ifwinty, only: HANDLE
        
            integer(HANDLE), intent(in) :: hList
            integer :: i
            character(len=16) :: levels
        
            do i = 1, gModIntParams%nlev
                write(levels, '(A, I0)') "Level ", i
                call AddListViewRowReal(hList, levels, [vertArr%PV(I)], '(F6.2)') ! The 'trim' is done in the called subroutine
            end do
        end subroutine loadLevelsIntItemsIntoListView
        
        ! Just to simplify code parsing in the dialog procedure. The user has clicked on
        ! one of the real parameters and this method gets the real value and converts it
        ! to a string to load into the edit box. The routine that actually does the work
        ! is RealToAsciiToReal. All this does is pick the correct real parameter to convert.
        function getStringFromRealForEditBox(caseSelect) result(str)
            use ModelParamsModule, only: gModRealParams
            use ConstantsModule, only: REAL_TO_ASCII
            use UtilitiesModule, only: LENGTH, RealToAsciiToReal
            use ModelFieldsModule, only: vertArr
            
            integer, intent(in) :: caseSelect

            character(len=LENGTH) :: str
            integer :: ios

            ios = RealToAsciiToReal(REAL_TO_ASCII, vertArr%PV(caseSelect + 1), str)

        end function getStringFromRealForEditBox
        
        ! Just to simplify code parsing in the dialog procedure. The user has edit
        ! a real parameter. This loads the edited value into the correct field
        function readRealDataFromEditBox(str, rowClicked) result(ios)
            use ModelFieldsModule, only: vertArr
            
            CHARACTER(:), ALLOCATABLE, intent(inout) :: str
            integer, intent(in) :: rowClicked

            integer :: ios

            ios = 0
            read(str, *, iostat = ios) vertArr%PV(rowClicked + 1)
        end function readRealDataFromEditBox

end module VerticalParamsDialogModule
