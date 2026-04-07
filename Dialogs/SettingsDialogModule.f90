module SettingsDialogModule
    implicit none
    
    contains
        !/****************************************************************************
        !
        !  FUNCTION: SettingsDlgProc(HWND, UINT, WPARAM, LPARAM)
        !
        !  PURPOSE:  Processes messages for "Settings" dialog box
        !
        !  COMMENTS: Display version information from the version section of the
        !            application resource.  Wait for user to click on "Ok" button,
        !            then close the dialog box.
        !
        !****************************************************************************/
        function SettingsDlgProc( hDlg, dlgMsg, uParam, lParam )
        !DEC$ IF DEFINED(_X86_)
        !DEC$ ATTRIBUTES STDCALL, ALIAS : '_SettingsDlgProc@16' :: SettingsDlgProc
        !DEC$ ELSE
        !DEC$ ATTRIBUTES STDCALL, ALIAS : 'SettingsDlgProc' :: SettingsDlgProc
        !DEC$ ENDIF

            use ifwinty
            use ifwin
            use kernel32
            use user32
            use gdi32

            use NlevelQGModelGlobals, only: ghInstance, ghModelControlDlg, ghwndMain, ghSettingsDlg, LogMessage, CenterWindow
            use iso_c_binding

            use DialogHelpersModule, only: GetComboIndex, &
                        moveEditBox, initListView, AddListViewRow, ResizeListViewToFitItems, updateListViewValue ! List control helpers
            use ModelParamsModule, only: gUserSettings, gModIntParams
            use ModelControlDialogModule, only: ModelControlDlgProc
            use KeyboardHooksModule, only: KeyboardForDlgSettingsProc



            implicit none

            integer(LRESULT) ::  SettingsDlgProc ! LRESULT
            integer(HANDLE) ::   hDlg        ! window handle of the dialog box
            integer(UINT) ::     dlgMsg
            integer(UINT_PTR) :: uParam
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
                    hHook = SetWindowsHookEx(WH_KEYBOARD, LOC(KeyboardForDlgSettingsProc), NULL, GetCurrentThreadId())
                    ghSettingsDlg = hDlg
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

                    SettingsDlgProc = 1
                    return

                case (WM_NOTIFY)
                    BLOCK
                        type(c_ptr) :: p_raw
                        type(T_NM_LISTVIEW), pointer :: pnmv
                        integer(kind=4) :: i, j, prevRowClicked
                        integer(kind=8) :: lp
                        ! Step 1: cast lParam to a C_PTR
                        lp = lParam
                        p_raw = transfer(lp, p_raw)
                        ! Step 2: cast to a Fortran structure pointer
                        call c_f_pointer(p_raw, pnmv)
                        if (pnmv%hdr%idFrom == IDC_USERSETTINGS) then
                            select case (pnmv%hdr%code)
                                case (NM_CLICK)
                                    i = pnmv%isubItem
                                    j = pnmv%iitem
                                    WRITE(buffer, '(I0, 2X, I0)') i, j
                                    call LogMessage("User Settings: pnmv%isubItem (i) and pnmv%iitem (j) have values " //TRIM(buffer)//CHAR(0))

                                    if (j >= 0) then
                                        hEdit = GetDlgItem(hDlg, IDC_EDIT_USERSETTINGS)
                                        bret = ShowWindow(hEdit, SW_SHOW)
                                        hList = GetDlgItem(hDlg, IDC_USERSETTINGS)
                                        prevRowClicked = rowClicked
                                        rowClicked = SendMessage(hList, LVM_GETNEXTITEM, -1, LVNI_SELECTED)
                                        if (prevRowClicked .EQ. rowClicked) then
                                            bret = SetFocus(hEdit)
                                            bret = SetWindowPos(hEdit, 0, 0, 0, 0, 0, SWP_NOMOVE .OR. SWP_NOSIZE .OR. SWP_FRAMECHANGED .OR. SWP_NOZORDER)
                                            return
                                        end if
                                        ! Moves the edit box over the value and focuses it (method in DialogHelpersModule))
                                        call moveEditBox(hDlg, hList, hEdit, rowClicked)
                                        ! Method selects the integer in gModIntParams to load into the edit box
                                        select case (j)
                                            case (0)    ! gUserSettings%maxZonalWaves
                                                bret = SetDlgItemInt(hDlg, IDC_EDIT_USERSETTINGS, gUserSettings%maxZonalWaves, .TRUE.)
                                            case (1)    ! gUserSettings%maxMeridWaves
                                                bret = SetDlgItemInt(hDlg, IDC_EDIT_USERSETTINGS, gUserSettings%maxMeridWaves, .TRUE.)
                                            case (2)    !gUserSettings%maxLevels
                                                bret = SetDlgItemInt(hDlg, IDC_EDIT_USERSETTINGS, gUserSettings%maxLevels, .TRUE.)
                                        end select
                                        bret = SetWindowPos(hEdit, 0, 0, 0, 0, 0, SWP_NOMOVE .OR. SWP_NOSIZE .OR. SWP_FRAMECHANGED .OR. SWP_NOZORDER)
                                    end if
                                case default
                                    SettingsDlgProc = 0
                                    return
                            end select
                        end if
                    end BLOCK

                case (WM_COMMAND)                      ! dlgMsg: received a command
                    iData = uParam
                    lData = lParam
                    !loWord = IAND(iData, Z'FFFF')
                    !hiWord = ISHFT(iData, -16)
                    ! Combo box change
                    if (ISHFT(iData, -16) == CBN_SELCHANGE) then
                        select case(IAND(iData, Z'FFFF'))
                            case (IDC_COMBO_USERSETTINGS1)
                                gUserSettings%integrationMethodTransform = GetComboIndex(hDlg, IDC_COMBO_USERSETTINGS1) 
                            case (IDC_COMBO_USERSETTINGS2)
                                gUserSettings%statsOption = GetComboIndex(hDlg, IDC_COMBO_USERSETTINGS2) 
                        end select
                    end if

                    select case (IAND(iData, 16#FFFF))

!--- F4: Model control dialog
                        case (IDC_BUTTON_MODEL_CONTROL)
                            ! Going to create another dialog box here. Dialog procedure ModelControlDlgProc is in ModelControlDialogModule.f90
                            iret = EnableWindow(hDlg, .FALSE.)
                            bret = ShowWindow(ghSettingsDlg, SW_HIDE)
                            ghModelControlDlg = CreateDialogParam(ghInstance, INT(IDD_MODEL_CONTROL, LONG_PTR), &
                                        NULL, & !ghwndMain,  Make the dialog unowned so the main window can come in the foreground
                                        LOC(ModelControlDlgProc), 0_LONG_PTR)
                            if (ghModelControlDlg /= 0) then
                                iret = ShowWindow(ghModelControlDlg, SW_SHOW)
                                iret = UpdateWindow(ghModelControlDlg) ! Optional: forces an immediate paint
                            end if
                            !iret = DialogBoxParam(ghInstance, LOC("ModelControlDlg"C), hDlg, LOC(ModelControlDlgProc), 0_LONG_PTR)
                            !ghSettingsDlg = hDlg
                            SettingsDlgProc = 1
                            return
                        case (IDC_BUTTON_EXIT_SETTINGS) ! F10 Close
                            bret = UnhookWindowsHookEx(hHook)
                            bret = DestroyWindow(hDlg)
                            
                        ! Recall that for the moved the edit control to work correctly the RC file needs to be manually edit
                        ! DO THE FOLLOWING two things:
                        ! 1. Add | WS_CLIPSIBLINGS to all list controls and corresponding edit boxes.
                        ! 2. Move the EDIT controls BEFORE the list controls.
                        CASE (IDC_EDIT_USERSETTINGS) ! The ID of the edit box
                            SELECT CASE (ISHFT(uParam, -16))
                                CASE (EN_CHANGE)
                                    BLOCK
                                        character(LEN=16) :: strC ! C-string will have null terminator

                                        call LogMessage("EN_CHANGE signaled from edit control ")
                                        hEdit = GetDlgItem(hDlg, IDC_EDIT_USERSETTINGS)
                                        ! Get the updated text from the edit control
                                        newValue = GetDlgItemInt(hDlg, IDC_EDIT_USERSETTINGS, LOC(bret), .TRUE.)
                                        hList = GetDlgItem(hDlg, IDC_USERSETTINGS)

                                        ! This should be the row that we clicked. Of course the original value of 'rowClicked'
                                        ! is lost when callback returned so we need to save it
                                        rowClicked = SendMessage(hList, LVM_GETNEXTITEM, -1, LVNI_SELECTED)
                                        ! Load value into the gUserSettings struct
                                        select case (rowClicked)
                                            case (0)    ! gUserSettings%maxZonalWaves
                                                gUserSettings%maxZonalWaves = newValue
                                            case (1)    ! gUserSettings%maxMeridWaves
                                                gUserSettings%maxMeridWaves = newValue
                                            case (2)    !gUserSettings%maxLevels
                                                gUserSettings%maxLevels = newValue
                                        end select
                                        ! Now get the item as a string to update the parameter in the list
                                        bret = GetDlgItemText(hDlg, IDC_EDIT_USERSETTINGS, strC, LEN(strC))
                                        call updateListViewValue(hEdit, hList, rowClicked, strC)
                                    end BLOCK
                            END SELECT
                    end select

                    if ((IAND(iData, 16#ffff) .EQ. IDOK) & !OK Selected?
                            .OR. (IAND(iData, 16#ffff) .EQ. IDCANCEL)) then ! Close command?
                        ! TODO check that iret is 0
                        bret = UnhookWindowsHookEx(hHook)
                        bret = DestroyWindow(hDlg)      ! Exit the dialog
                        !iret = CheckUserSettingsChanges()   ! Make sure any user changes in the gUserSettings have been properly accounted for
                        SettingsDlgProc = 1
                        return
                    end if

                case (WM_CLOSE)
                    bret = DestroyWindow(hDlg)
                    SettingsDlgProc = 1

                case (WM_DESTROY)
                    !bret = WriteCurrentStateToFile(hDlg, gModelSaveFile, gModelWelford, .true.)
                    bret = UnhookWindowsHookEx(hHook)
                    bret = PostMessage(ghwndMain, WM_CLOSE, 0, 0)
                    ghSettingsDlg = 0
            end select
            SettingsDlgProc = 0 ! Didn't process the dialog message dlgMsg
        end function SettingsDlgProc

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
            
            hList = GetDlgItem(hDlg, IDC_USERSETTINGS)
            bret = SendMessage(hList, LVM_DELETEALLITEMS, 0, 0) ! Clear first
            ! Load everything into the int list view
            bret = GetClientRect(hList, rc)
            widths(1) = 400
            widths(2) = rc%right - rc%left - widths(1)  ! buffer for margin/scrollbar is 16 but that is not to be there
            call EnsureListViewColumns(hList, ["Base Model Settings", "Values"], widths)
            call loadSettingsIntItemsIntoListControl(hList)
            bret = UpdateWindow(hList)

            ! Getting the current position and width of the ListView control in the dialog
            bret = GetWindowRect(hList, rc) ! Gets the ListView rectangle coordinates (this is screen coordinates)
            pt%x = rc%left  ! A T_POINT struct is needed to convert the screen coordinates to dlg coordinates
            pt%y = rc%top
            bret = ScreenToClient(hDlg, pt) ! Converts the screen pt struct coordinates to dialog relative coordinates
            ! The makes the height sized to encompass the actual number of rows added
            call ResizeListViewToFitItems(hList, pt%x, pt%y, rc%right - rc%left)
        end subroutine handleIntListControl

        subroutine loadSettingsIntItemsIntoListControl(hList)
            use ModelParamsModule, only: gUserSettings
            use DialogHelpersModule, only : AddListViewRow
            use ifwinty, only: HANDLE

            integer(HANDLE), intent(in) :: hList

            call AddListViewRow(hList, "Maximum number of zonal waves", gUserSettings%maxZonalWaves)
            call AddListViewRow(hList, "Maximum number of meridional waves", gUserSettings%maxMeridWaves)
            call AddListViewRow(hList, "Maximum number of levels", gUserSettings%maxLevels)
        end subroutine loadSettingsIntItemsIntoListControl

end module SettingsDialogModule
