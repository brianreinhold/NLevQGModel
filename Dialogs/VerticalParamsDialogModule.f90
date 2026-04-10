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

            use DialogHelpersModule, only: GetComboIndex, moveEditBox, initListView, AddListViewRow, ResizeListViewToFitItems, &
                                           updateListViewValue, HideMyEdit
            use ModelParamsModule, only: gUserSettings
            use KeyboardHooksModule, only: KeyboardForDlgVerticalParamsProc
            use ModelFieldsModule, only: gVertArr



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
            integer(HANDLE) :: hList, hEdit, hStaticEdit, hCombo, hStaticCombo, hDoneButton, hFocus
            integer, save :: pvRowClicked, thsRowClicked, fkRowClicked
            integer :: id
            CHARACTER(LEN=16) :: buffer ! Use this to read the value in the edit box

            integer :: newValue      ! For editing the displayed value in the list box

            select case (dlgMsg)
            CASE (WM_APP + 100)
                    ! hide edit box
                    ID = getListControlId()
                    if (id /= -1) then
                        call HideMyEdit(GetDlgItem(hDlg, IDC_EDIT_VERTICAL))
                        hFocus = GetDlgItem(ghVerticalParamsDlg, id)
                        bret = SetFocus(hFocus)
                    endif
                    bret = SetFocus(hFocus)
                    RETURN
                case (WM_INITDIALOG)   ! dlgMsg: initialize dialog box
                    pvRowClicked = -1
                    thsRowClicked = -1
                    ! Set up a hook to trap key presses before the dialog message loop handles them
                    hHook = SetWindowsHookEx(WH_KEYBOARD, LOC(KeyboardForDlgVerticalParamsProc), NULL, GetCurrentThreadId())
                    ghVerticalParamsDlg = hDlg

                    !! ===> Combo boxes require manual editing of their height if you want the list of options <== !!
                    !       to be exposed on click only.
                    ! Integration Method
                    hCombo = GetDlgItem(hDlg, IDC_COMBO_USERSETTINGS1)
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Use interaction coefficients'//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Use spectral transforms'//char(0)))
                    ! Initialize the combo box
                    iret = 0
                    if (gUserSettings%integrationMethodTransform) iret = 1
                    bret = SendMessage(hCombo, CB_SETCURSEL, INT(iret, UINT_PTR), 0)    ! Cast avoids warning. Not necessary to run
                    hStaticCombo = GetDlgItem(hDlg, IDC_STATIC_COMBO_SETTINGS1)

                    ! Statistics Options
                    hCombo = GetDlgItem(hDlg, IDC_COMBO_USERSETTINGS2)
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('None'//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Time averages'//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Time averages and variances'//char(0)))
                    ! Initialize the combo box
                    bret = SendMessage(hCombo, CB_SETCURSEL, INT(gUserSettings%statsOption, UINT_PTR), 0) ! Cast gets rid of warning. Not needed to run.
                    hStaticCombo = GetDlgItem(hDlg, IDC_STATIC_COMBO_SETTINGS2)

                    call handleIntListControl(hDlg, IDC_LIST_LEVELS, ["Levels", "Pressure"], gVertArr%PV)
                    call handleIntListControl(hDlg, IDC_LIST_TEMPERATURES, ["Levels", "Temperature"], gVertArr%THS)
                    call handleIntListControl(hDlg, IDC_LIST_EKMAN_FRICTION, ["Levels", "Ekman Friction"], gVertArr%FK)

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
                        integer :: ios, rowClicked
                        character(len=16) :: str
                        real, pointer :: ptr(:)
                        ! Step 1: cast lParam to a C_PTR
                        lp = lParam
                        p_raw = transfer(lp, p_raw)
                        ! Step 2: cast to a Fortran structure pointer
                        call c_f_pointer(p_raw, pnmv)
                        if (pnmv%hdr%idFrom == IDC_LIST_LEVELS .OR. pnmv%hdr%idFrom == IDC_LIST_TEMPERATURES &
                            .OR. pnmv%hdr%idFrom == IDC_LIST_EKMAN_FRICTION) then
                            select case (pnmv%hdr%code)
                                case (NM_CLICK)
                                    i = pnmv%isubItem
                                    j = pnmv%iitem
                                    WRITE(buffer, '(I0, 2X, I0)') i, j
                                    call LogMessage("Real: pnmv%isubItem (i) and pnmv%iitem (j) have values " //TRIM(buffer)//CHAR(0))
                                    if (j >= 0) then
                                        ! Gets the xxRowClicked variable that corresponds to the list view that was clicked and resets the other two to -1.
                                        ! This is used to keep track of which list view was clicked when the callback returns to the EN_CHANGE event of the edit box
                                        ios = getRowClickedInCorrectListView(pnmv%hdr%idFrom)
                                        hList = GetDlgItem(hDlg, INT(pnmv%hdr%idFrom))
                                        hEdit = GetDlgItem(hDlg, IDC_EDIT_VERTICAL)
                                        rowClicked = SendMessage(hList, LVM_GETNEXTITEM, -1, LVNI_SELECTED)
                                        if (pnmv%hdr%idfrom == idc_list_levels) then
                                            pvrowclicked = rowclicked
                                        else if (pnmv%hdr%idfrom == idc_list_temperatures) then
                                            thsrowclicked = rowclicked
                                        else if (pnmv%hdr%idfrom == idc_list_ekman_friction) then
                                            fkrowclicked = rowclicked
                                        end if
                                        if (ios .EQ. rowClicked) then
                                            bret = SetFocus(hEdit)
                                            bret = SetWindowPos(hEdit, 0, 0, 0, 0, 0, SWP_NOMOVE .OR. SWP_NOSIZE .OR. SWP_FRAMECHANGED .OR. SWP_NOZORDER)
                                            return
                                        end if
                                        ! Hides the integer edit box if it is visible
                                        !bret = ShowWindow(GetDlgItem(hDlg, IDC_EDIT_INTEGERVALUES), SW_HIDE)
                                        ! Moves the edit box over the value and focuses it
                                        call moveEditBox(hDlg, hList, hEdit, rowClicked)

                                        ptr => gVertArr%PV
                                        if (pnmv%hdr%idFrom == IDC_LIST_TEMPERATURES) ptr => gVertArr%THS
                                        if (pnmv%hdr%idFrom == IDC_LIST_EKMAN_FRICTION) ptr => gVertArr%FK
                                        ! Method picks the correct real parameter to convert to string for edit box
                                        str = getStringFromRealForEditBox(j, ptr)
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
                                        integer :: ios, rowClicked
                                        real, pointer :: ptr(:)
                                        CHARACTER(:), ALLOCATABLE :: str ! fortran string with no null terminator
                                        call LogMessage("EN_CHANGE signaled from edit real control ")

                                        hEdit = GetDlgItem(hDlg, IDC_EDIT_VERTICAL)
                                        newValue = GetDlgItemText(hDlg, IDC_EDIT_VERTICAL, strC, LEN(strC))
                                        str = strC(1 : newValue) ! Clean out the junk
                                        if (ADJUSTL(TRIM(str)) == '') exit EN ! User typed enter
                                        if (pvRowClicked >= 0) then
                                            hList = GetDlgItem(hDlg, IDC_LIST_LEVELS)
                                            ptr => gVertArr%PV
                                        else if (thsRowClicked >= 0) then 
                                            hList = GetDlgItem(hDlg, IDC_LIST_TEMPERATURES)
                                            ptr => gVertArr%THS
                                        else if (fkRowClicked >= 0) then 
                                            hList = GetDlgItem(hDlg, IDC_LIST_EKMAN_FRICTION)
                                            ptr => gVertArr%FK
                                        endif

                                        ! This should be the row that was hilighted when we clicked on it. Of
                                        ! course the original value of 'realRowClicked' is lost when callback returned
                                        rowClicked = SendMessage(hList, LVM_GETNEXTITEM, -1, LVNI_SELECTED)
                                        ios = 0
                                        
                                        ! Load the new value into the correct array index field
                                        ios = readRealDataFromEditBox(str, rowClicked, ptr)
                                        !write(*, '(A, A)') "Real value 3: ", str
                                        if (ios .NE. 0) then
                                            !ret = WriteMessageBox(hDlg, ios, "Input is not a valid real number", "Input Error")
                                            strC = getStringFromRealForEditBox(rowClicked, ptr)
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
            
            contains

            function getListControlId() result(id)
                implicit none

                integer :: id

                id = -1
                if (pvRowClicked >= 0) then
                    id = IDC_LIST_LEVELS
                else if (thsRowClicked >= 0) then
                    id = IDC_LIST_TEMPERATURES
                else if (fkRowClicked >= 0) then
                    id = IDC_LIST_EKMAN_FRICTION
                endif
            end function getListControlId
            
            function getRowClickedInCorrectListView(id) result(rowClicked)
                implicit none

                integer, intent(in) :: id

                integer :: rowClicked

                if (id == IDC_LIST_LEVELS) then
                    rowClicked = pvRowClicked
                    thsRowClicked = -1
                    fkRowClicked = -1
                else if (id == IDC_LIST_TEMPERATURES) then
                    rowClicked = thsRowClicked
                    pvRowClicked = -1
                    fkRowClicked = -1
                else if (id == IDC_LIST_EKMAN_FRICTION) then
                    rowClicked = fkRowClicked
                    pvRowClicked = -1
                    thsRowClicked = -1
                end if
            end function getRowClickedInCorrectListView

        end function VerticalParamsDlgProc


        subroutine handleIntListControl(hDlg, id, labels, vertArr)
            use ifwinty
            use ifwin
            use DialogHelpersModule, only: EnsureListViewColumns, ResizeListViewToFitItems
            implicit none
            
            include '../resource.fd'

            integer(HANDLE), intent(in) :: hDlg
            integer, intent(in) :: id
            character(len=*), intent(in) :: labels(:)
            REAL, intent(in) :: vertArr(:)

            integer(HANDLE) :: hList 
            integer(bool) :: bret
            integer :: widths(2)
            type(T_RECT) :: rc
            type(T_POINT) :: pt
            
            hList = GetDlgItem(hDlg, id)
            bret = SendMessage(hList, LVM_DELETEALLITEMS, 0, 0) ! Clear first
            ! Load everything into the int list view
            bret = GetClientRect(hList, rc)
            widths(1) = 96  ! This is a hard coded width for the first column. It will only contain 'Level n'.
            widths(2) = rc%right - rc%left - widths(1)  ! buffer for margin/scrollbar is 16 but that is not to be there
            call EnsureListViewColumns(hList, labels, widths)
            call loadLevelsIntItemsIntoListView(hList, vertArr)
            bret = UpdateWindow(hList)

            ! Getting the current position and width of the ListView control in the dialog
            bret = GetWindowRect(hList, rc) ! Gets the ListView rectangle coordinates (this is screen coordinates)
            pt%x = rc%left  ! A T_POINT struct is needed to convert the screen coordinates to dlg coordinates
            pt%y = rc%top
            bret = ScreenToClient(hDlg, pt) ! Converts the screen pt struct coordinates to dialog relative coordinates
            ! The makes the height sized to encompass the actual number of rows added
            call ResizeListViewToFitItems(hList, pt%x, pt%y, rc%right - rc%left)
        end subroutine handleIntListControl

        subroutine loadLevelsIntItemsIntoListView(hList, vertArr)
            use ModelParamsModule, only: gModIntParams
            use ModelFieldsModule, only: gVertArr
            use DialogHelpersModule, only : AddListViewRowReal
            use ifwinty, only: HANDLE
            
            implicit none
        
            integer(HANDLE), intent(in) :: hList
            REAL, intent(in) :: vertArr(:)

            integer :: i
            character(len=16) :: levels
        
            do i = 1, SIZE(vertArr)
                write(levels, '(A, I0)') "Level ", i
                call AddListViewRowReal(hList, levels, [vertArr(i)], '(F6.2)') ! The 'trim' is done in the called subroutine
            end do
        end subroutine loadLevelsIntItemsIntoListView
        
        ! Just to simplify code parsing in the dialog procedure. The user has clicked on
        ! one of the real parameters and this method gets the real value and converts it
        ! to a string to load into the edit box. The routine that actually does the work
        ! is RealToAsciiToReal. All this does is pick the correct real parameter to convert.
        function getStringFromRealForEditBox(caseSelect, vertArr) result(str)
            use ModelParamsModule, only: gModRealParams
            use ConstantsModule, only: REAL_TO_ASCII
            use UtilitiesModule, only: LENGTH, RealToAsciiToReal
            
            implicit none

            integer, intent(in) :: caseSelect
            real, intent(inout) :: vertArr(:)

            character(len=LENGTH) :: str
            integer :: ios

            ios = RealToAsciiToReal(REAL_TO_ASCII, vertArr(caseSelect + 1), str)

        end function getStringFromRealForEditBox

        ! Just to simplify code parsing in the dialog procedure. The user has edit
        ! a real parameter. This loads the edited value into the correct field
        function readRealDataFromEditBox(str, rowClicked, vertArr) result(ios)
            use ModelFieldsModule, only: gVertArr
            
            implicit none
            
            CHARACTER(:), ALLOCATABLE, intent(inout) :: str
            integer, intent(in) :: rowClicked
            REAL, intent(inout) :: vertArr(:)

            integer :: ios

            ios = 0
            read(str, *, iostat = ios) vertArr(rowClicked + 1)
        end function readRealDataFromEditBox

end module VerticalParamsDialogModule
