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

            use NLevQGModelGlobals, only: ghInstance, ghModelControlDlg, ghwndMain, ghSettingsDlg, LogMessage, CenterWindow
            use iso_c_binding

            use DialogHelpersModule, only: GetComboIndex, HideMyEdit, &
                        moveEditBox, initListView, AddListViewRow, ResizeListViewToFitItems, updateListViewValue, &
                        GetNameOfUserSelectedFile, GetUserSelectedSaveName  ! List control helpers
            use SetupModule, only: SaveModelState, ReadSaveFile, CheckUserSettingsChanges
            use ModelParamsModule, only: gUserSettings, gModIntParams, gNLevFile
            use ModelControlDialogModule, only: ModelControlDlgProc
            use KeyboardHooksModule, only: KeyboardForDlgSettingsProc
            use ContoursAndRangesDialogModule, only: ContoursAndRangesDlgProc
            use UtilitiesModule, only: openModelFile
            use ModelFieldsModule, only: gHorzArr
            use PlotModule, only: resetPhaseSpace

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
            integer(HANDLE), save :: hHook
    
            type(T_RECT) :: rc
            type(T_POINT) :: pt

            ! For the list view report rows
            integer(HANDLE) :: hList, hEdit, hStaticEdit, hCombo, hStaticCombo, hDoneButton
            integer, save :: rowClicked

            CHARACTER(LEN=16) :: buffer ! Use this to read the value in the edit box
            LOGICAL :: res              ! File loading and saving
            character(len=270) :: selected_file

            integer :: newValue         ! For editing the displayed value in the list box
            
            SettingsDlgProc = 0

            select case (dlgMsg)
                
                CASE (WM_APP + 104)
                    ! Model Control dialog - from F4 keypress
                    call runModelControlDialog()
                    SettingsDlgProc = 1
                
                case (WM_ACTIVATE)
                    ! Check if window is being activated (WA_ACTIVE or WA_CLICKACTIVE)
                    if (IAND(uParam, 16#FFFF) /= WA_INACTIVE) then
                        ! The dialog just got focus. Update the selection here.
                        call handleIntListControl(hDlg)
                        hCombo = GetDlgItem(hDlg, IDC_COMBO_USERSETTINGS1)
                        bret = SendMessage(hCombo, CB_SETCURSEL, INT(gUserSettings%integrationMethod, UINT_PTR), 0)

                        ! Force the visual refresh
                        iret = InvalidateRect(hDlg, NULL, .TRUE.) 
                        iret = UpdateWindow(hDlg) 
                    end if
                    SettingsDlgProc = 0
                    return

                case (WM_INITDIALOG)    ! dlgMsg: initialize dialog box
                    rowClicked = -1
                    ! Set up a hook to trap key presses before the dialog message loop handles them
                    hHook = SetWindowsHookEx(WH_KEYBOARD, LOC(KeyboardForDlgSettingsProc), NULL, GetCurrentThreadId())
                    ghSettingsDlg = hDlg
                    !hEdit = GetDlgItem(hDlg, IDC_EDIT_USERSETTINGS)
                    !bret = SendMessage(hEdit, WM_SETFONT, gFont, .TRUE.)
                    !hStaticEdit = GetDlgItem(hDlg, IDC_STATIC_EDIT_SETTINGS)
                    !bret = SendMessage(hStaticEdit, WM_SETFONT, gFont, .TRUE.)

                    call initializeDialog()

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
                                        hList = GetDlgItem(hDlg, IDC_USERSETTINGS)
                                        hEdit = GetDlgItem(hDlg, IDC_EDIT_USERSETTINGS)
                                        prevRowClicked = rowClicked
                                        rowClicked = SendMessage(hList, LVM_GETNEXTITEM, -1, LVNI_SELECTED)
                                        bret = ShowWindow(hEdit, SW_SHOW)
                                        !if (prevRowClicked .EQ. rowClicked) then
                                        !    bret = SetFocus(hEdit)
                                        !    bret = SetWindowPos(hEdit, 0, 0, 0, 0, 0, SWP_NOMOVE .OR. SWP_NOSIZE .OR. SWP_FRAMECHANGED .OR. SWP_NOZORDER)
                                        !    return
                                        !end if
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
                                        bret = SetFocus(hEdit)
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
                                gUserSettings%integrationMethod = GetComboIndex(hDlg, IDC_COMBO_USERSETTINGS1) 
                            case (IDC_COMBO_USERSETTINGS2)
                                gUserSettings%statsOption = GetComboIndex(hDlg, IDC_COMBO_USERSETTINGS2) 
                        end select
                    end if

                    select case (IAND(iData, 16#FFFF))
!--- F1: Set countours and Ranges
                        case (IDC_BUTTON_SET_SCALE_FACTORS)
                            bret = DialogBoxParam(ghInstance, INT(IDD_CONTOURS, LONG_PTR), hDlg, LOC(ContoursAndRangesDlgProc), 0_LONG_PTR)
                            SettingsDlgProc = 1

! ---- F2:  Load Saved Flow
                        case (IDC_BUTTON_LOAD_FROM_FILE)
                            res = GetNameOfUserSelectedFile(hDlg, gNLevFile)
                            if (res) then
                                if (len_trim(gNLevFile%fileName) == 0) return
                                call LogMessage("Save file loaded: "// selected_file)
                                iret = openModelFile(gNLevFile, .true., hDlg)
                                if (iret == 0) iret = ReadSaveFile(gNLevFile)
                                call resetPhaseSpace()
                                call initializeDialog()
                                gHorzArr%S = 0.0 ! Remove any dissipative or forcing terms that may be in the file since we are just loading a flow state
                                gHorzArr%T = 0.0 ! Remove any dissipative or forcing terms that may be in the file since we are just loading a flow state
                            endif
                            ghSettingsDlg = hDlg

! ---- F3:  Save Current Flow
                        case (IDC_BUTTON_SAVE_TO_FILE)
                            res = GetUserSelectedSaveName(hDlg, gNLevFile)
                            if (res) then
                                iret = openModelFile(gNLevFile, .false., hDlg)
                                if (iret == 0) iret = SaveModelState(gNLevFile, .false.)
                            endif
                            ghSettingsDlg = hDlg

!--- F4: Model control dialog
                        case (IDC_BUTTON_MODEL_CONTROL)
                            call runModelControlDialog()
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
                                case( EN_KILLFOCUS)
                                    call HideMyEdit(GetDlgItem(hDlg, IDC_EDIT_USERSETTINGS))
                                CASE (EN_CHANGE)
                                    STB: BLOCK
                                        character(LEN=16) :: strC ! C-string will have null terminator

                                        call LogMessage("EN_CHANGE signaled from edit control ")
                                        hEdit = GetDlgItem(hDlg, IDC_EDIT_USERSETTINGS)
                                        ! Get the updated text from the edit control
                                        newValue = GetDlgItemInt(hDlg, IDC_EDIT_USERSETTINGS, LOC(bret), .TRUE.)
                                        if (bret == 0) exit STB
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
                                    end BLOCK STB
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
            
            contains
                subroutine initializeDialog()
                    implicit none
                    ! Combo boxes require manual editing of their height if you want the list of options
                    ! to be exposed on click only.
                    ! Integration Method
                    hCombo = GetDlgItem(hDlg, IDC_COMBO_USERSETTINGS1)
                    !bret = SendMessage(hCombo, WM_SETFONT, gFont, .TRUE.)
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Use interaction coefficients'//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Use spectral transforms'//char(0)))
                    ! Initialize the combo box
                    !iret = 0
                    !if (gUserSettings%integrationMethod == USE_TRANSFORM) iret = 1
                    bret = SendMessage(hCombo, CB_SETCURSEL, INT(gUserSettings%integrationMethod, UINT_PTR), 0)    ! Cast avoids warning. Not necessary to run
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
                end subroutine initializeDialog
                
                subroutine runModelControlDialog()
                    if (CheckUserSettingsChanges() == 0) then
                        call cleanUpEditBoxe()
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
                    else
                        bret = MessageBox (hDlg, &
                            "Unable to handle the new user settings"C, &
                            "Error encountered"C, &
                            IOR(MB_SYSTEMMODAL, IOR(MB_OK, MB_ICONHAND)))
                    end if
                end subroutine runModelControlDialog
                
                subroutine cleanUpEditBoxe()
                    bret = SetDlgItemText(hDlg, IDC_EDIT_USERSETTINGS, ""//char(0))
                    hEdit = GetDlgItem(hDlg, IDC_EDIT_USERSETTINGS)
                    call HideMyEdit(hEdit)
                    bret = SetFocus(GetDlgItem(hDlg, IDC_USERSETTINGS))
                    !bret = MoveWindow(hEdit, pt%x, pt%y, pt2%x - pt%x, pt2%y - pt%y, 1)
                    ! Move off screen
                    bret = SetWindowPos(hEdit, 0, -500, -500, 0, 0, SWP_NOSIZE .OR. SWP_NOZORDER)

                end subroutine cleanUpEditBoxe

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
            call ResizeListViewToFitItems(hList, pt%x, pt%y, rc%right - rc%left, 14)
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
