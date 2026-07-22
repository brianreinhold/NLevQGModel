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

            use NLevQGModelGlobals, only: ghVerticalParamsDlg, CenterWindow, LogMessage
            use iso_c_binding

            use ConstantsModule, only: REAL_TO_ASCII, SQRT2, STRATOSPHERIC_WARMING_32, STRATOSPHERIC_WARMING_32_V2
            use UtilitiesModule, only: RealToAsciiToReal
            use DialogHelpersModule, only: moveEditBox, initListView, AddListViewRow, ResizeListViewToFitItems, &
                                           updateListViewValue, HideMyEdit, GetComboIndex
            use ModelParamsModule, only: gModIntParams
            use KeyboardHooksModule, only: KeyboardForDlgVerticalParamsProc
            use ModelFieldsModule, only: gVertArr, gHorzArr
            use SetUpModule, only: DimensionalizeVerticalParams, NonDimensionalizeVerticalParams, &
                                   DimensionalizeHorizontalParams, NonDimensionalizeHorizontalParams, ComputeVerticalProfiles



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
            integer(kind=4), save :: profile = 0
            integer(BOOL) ::   bret
            integer(HANDLE), save :: hHook


            type(T_RECT) :: rc
            type(T_POINT) :: pt
            
            real, pointer :: pTempDriving(:)  ! Local 1D pointer

            ! For the list view report rows
            integer(HANDLE) :: hList, hEdit, hCheckBox, hFocus, hCombo
            integer, save :: pvRowClicked, thsRowClicked, fkRowClicked, fjRowClicked
            integer, save :: hRowClicked, gRowClicked, tfRowClicked
            integer :: id
            CHARACTER(LEN=16) :: buffer ! Use this to read the value in the edit box

            integer :: newValue      ! For editing the displayed value in the list box

            select case (dlgMsg)
            CASE (WM_APP + 400)
                    ! hide edit box
                    ID = getListControlIdFromRowClicked()
                    if (id /= -1) then
                        call HideMyEdit(GetDlgItem(hDlg, IDC_EDIT_VERTICAL))
                        hFocus = GetDlgItem(ghVerticalParamsDlg, id)
                        bret = SetFocus(hFocus)
                    endif
                    bret = SetFocus(hFocus)
                    RETURN
                    
                ! Just for interest, the average height to 950 mb is about 540 meters.
                case (WM_INITDIALOG)   ! dlgMsg: initialize dialog box
                    pvRowClicked = -1
                    thsRowClicked = -1
                    fkRowClicked = -1
                    call DimensionalizeVerticalParams()
                    call DimensionalizeHorizontalParams()
                    ! Set up a hook to trap key presses before the dialog message loop handles them
                    hHook = SetWindowsHookEx(WH_KEYBOARD, LOC(KeyboardForDlgVerticalParamsProc), NULL, GetCurrentThreadId())
                    ghVerticalParamsDlg = hDlg

                    hCheckBox = GetDlgItem(hDlg, IDC_CHECK_VERT_DIMENSIONALIZE)
                    bret = SendMessage(hCheckBox, BM_SETCHECK, BST_CHECKED, 0)
                    
                    call initializeFields() ! "contains' subroutine as these lines need to be called in several places.

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
                        if (isListControlId(INT(pnmv%hdr%idFrom))) then
                            select case (pnmv%hdr%code)
                                case (NM_CLICK)
                                    i = pnmv%isubItem
                                    j = pnmv%iitem
                                    WRITE(buffer, '(I0, 2X, I0)') i, j
                                    !call LogMessage("Real: pnmv%isubItem (i) and pnmv%iitem (j) have values " //TRIM(buffer)//CHAR(0))
                                    if (j >= 0) then
                                        ! Gets the rowClicked variable that corresponds to the list view that was clicked and resets the others to -1.
                                        ! This is used to keep track of which list view was clicked when the callback returns to the EN_CHANGE event of the edit box
                                        ios = getRowClickedFromId(INT(pnmv%hdr%idFrom))
                                        hList = GetDlgItem(hDlg, INT(pnmv%hdr%idFrom))
                                        hEdit = GetDlgItem(hDlg, IDC_EDIT_VERTICAL)
                                        rowClicked = SendMessage(hList, LVM_GETNEXTITEM, -1, LVNI_SELECTED)
                                        call setRowClickedForCorrectListControl(rowclicked, INT(pnmv%hdr%idFrom))
                                        if (ios .EQ. rowClicked) then
                                            bret = SetFocus(hEdit)
                                            bret = SetWindowPos(hEdit, 0, 0, 0, 0, 0, SWP_NOMOVE .OR. SWP_NOSIZE .OR. SWP_FRAMECHANGED .OR. SWP_NOZORDER)
                                            return
                                        end if
                                        ! Moves the edit box over the value and focuses it
                                        call moveEditBox(hDlg, hList, hEdit, rowClicked)
                                        ! Function returns pointer to the correct array based on which list view was clicked
                                        ptr => getArrayFromId(INT(pnmv%hdr%idFrom))
                                        if (gHorzArr%isDimensionalized) then
                                            if (pnmv%hdr%idFrom .EQ. IDC_LIST_THERMAL_DRIVING) then
                                                ptr(J + 1) = SQRT2 * ptr(J + 1)
                                            endif
                                        endif
                                        ! Converts real value to string for edit box
                                        ios = RealToAsciiToReal(REAL_TO_ASCII, ptr(J + 1), str)
                                        !str = getStringFromRealForEditBox(ptr(J + 1))
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
                    !if (ISHFT(wParam, -16) == CBN_SELCHANGE) then
                    !    select case(IAND(wParam, Z'FFFF'))
                    !        case (IDC_COMBO_USERSETTINGS1)
                    !            gUserSettings%integrationMethod = GetComboIndex(hDlg, IDC_COMBO_USERSETTINGS1) 
                    !        case (IDC_COMBO_USERSETTINGS2)
                    !            gUserSettings%statsOption = GetComboIndex(hDlg, IDC_COMBO_USERSETTINGS2) 
                    !    end select
                    !end if

                    select case (IAND(wParam, Z'FFFF'))

                        case (IDC_COMBO_VERT_PROFILES)
                            bret = GetComboIndex(hDlg, IDC_COMBO_VERT_PROFILES)
                            if (bret == profile) return
                            profile = bret
                            if (profile == STRATOSPHERIC_WARMING_32) then
                                if (gModIntParams%NLEV .NE. 32) then
                                    bret = MessageBox (hDlg, &
                                        "This profile needs to have the model configured to 32 levels."C, &
                                        "Incorrect number of levels"C, &
                                        IOR(MB_SYSTEMMODAL, IOR(MB_OK, MB_ICONINFORMATION)))
                                    return
                                endif
                            endif
                            call generateVerticalProfile(profile)
                            call initializeFields()
                            bret = MessageBox (hDlg, &
                                "The selected profile has been set."C, &
                                "Vertical Profile set"C, &
                                IOR(MB_SYSTEMMODAL, IOR(MB_OK, MB_ICONINFORMATION)))

!  F10  Exit --------------------------------------------
                        case (IDC_BUTTON_EXIT_VERTICAL)
                            bret = EndDialog(hDlg, 0)
                            VerticalParamsDlgProc = 1

                        case (IDOK)
                            bret = EndDialog(hDlg, IDOK)
                            VerticalParamsDlgProc = 1

                        case (IDCANCEL)
                            bret = EndDialog(hDlg, IDCANCEL)
                            VerticalParamsDlgProc = 1
                            
                        case (IDC_CHECK_VERT_DIMENSIONALIZE)
                            if (ISHFT(wParam, -16) == BN_CLICKED) then
        
                                ! Now we ask Windows: "Is it checked or not?"
                                if (IsDlgButtonChecked(hDlg, IDC_CHECK_VERT_DIMENSIONALIZE) /= 0) then
                                    ! User checked it - update your variable/logic here
                                    call DimensionalizeHorizontalParams()
                                    call DimensionalizeVerticalParams()
                                else
                                    ! User unchecked it
                                    call NonDimensionalizeHorizontalParams()
                                    call NonDimensionalizeVerticalParams()
                                end if
                                bret = PostMessage(hDlg, WM_APP + 400, 0, 0)
                                call initializeFields()
                            endif
                            VerticalParamsDlgProc = 1

                        ! Get result of user editing real values
                        ! Recall that for the moved the edit control to work correctly the RC file needs to be manually edit
                        ! DO THE FOLLOWING two things:
                        ! 1. Add | WS_CLIPSIBLINGS to all list controls and corresponding edit boxes.
                        ! 2. Move the EDIT controls BEFORE the list controls.
                        case (IDC_EDIT_VERTICAL)
                            SELECT CASE (ISHFT(wParam, -16))
                                case( EN_KILLFOCUS)
                                    call HideMyEdit(GetDlgItem(hDlg, IDC_EDIT_VERTICAL))
                                CASE (EN_CHANGE)
                                    EN: BLOCK
                                        character(LEN=16) :: strC ! C-string will have null terminator
                                        integer :: ios, rowClicked
                                        real, pointer :: ptr(:)
                                        CHARACTER(:), ALLOCATABLE :: str ! fortran string with no null terminator
                                        logical, save :: isUpdating = .FALSE.
                                        
                                        if (isUpdating) exit EN
                                        isUpdating = .TRUE.
                                        call LogMessage("EN_CHANGE signaled from edit real control ")

                                        hEdit = GetDlgItem(hDlg, IDC_EDIT_VERTICAL)
                                        
                                        newValue = GetDlgItemText(hDlg, IDC_EDIT_VERTICAL, strC, LEN(strC))
                                        str = strC(1 : newValue) ! Clean out the junk
                                        str = ADJUSTL(TRIM(str))
                                        if (len_trim(str) == 0 .OR. str == "." .OR. str == "-" .OR. str == "-.") then
                                            isUpdating = .FALSE.
                                            exit EN
                                        endif

                                        hList = getListControlHandleFromRowClicked()
                                        ptr => getArrayFromRowClicked()

                                        ! This should be the row that was hilighted when we clicked on it. Of
                                        ! course the original value of 'realRowClicked' is lost when callback returned
                                        rowClicked = SendMessage(hList, LVM_GETNEXTITEM, -1, LVNI_SELECTED)
                                        ios = 0
                                        
                                        ! Load the new value into the correct array index field
                                        ios = readRealDataFromEditBox(str, rowClicked, ptr, GetDlgCtrlID(hList) == IDC_LIST_THERMAL_DRIVING)
                                        !write(*, '(A, A)') "Real value 3: ", str
                                        if (ios .NE. 0) then
                                            if (gHorzArr%isDimensionalized) then
                                                ptr(rowClicked + 1) = SQRT2 * ptr(rowClicked + 1)
                                            endif
                                            ios = RealToAsciiToReal(REAL_TO_ASCII, ptr(rowClicked + 1), strC)
                                            !strC = getStringFromRealForEditBox(ptr(rowClicked + 1))
                                            bret = SetDlgItemText(hDlg, IDC_EDIT_VERTICAL, strC)
                                            isUpdating = .FALSE.
                                            exit EN
                                        end if
                                        call updateListViewValue(hEdit, hList, rowClicked, strC)
                                        isUpdating = .FALSE.
                                    end BLOCK EN
                            end SELECT
                    end select

                case (WM_CLOSE)
                    bret = EndDialog(hDlg, 0)
                    VerticalParamsDlgProc = 1

                case (WM_DESTROY)
                    !bret = WriteCurrentStateToFile(hDlg, gModelSaveFile, gModelWelford, .true.)
                    bret = UnhookWindowsHookEx(hHook)
                    call NonDimensionalizeHorizontalParams()
                    call NonDimensionalizeVerticalParams()
                    CALL ComputeVerticalProfiles()
                    ghVerticalParamsDlg = 0
            end select
            VerticalParamsDlgProc = 0 ! Didn't process the dialog message dlgMsg

        contains
        
                subroutine initializeFields()
                    implicit none
                    
                    hCombo = GetDlgItem(hDlg, IDC_COMBO_VERT_PROFILES)
                    bret = SendMessage(hCombo, CB_RESETCONTENT, 0, 0)
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('None'//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Default profile and default levels'//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Default profile with user levels'//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('32-level Sudden Stratospheric Warming'//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('32-level Sudden Stratospheric Warming no SQRT2'//char(0)))
                    bret = SendMessage(hCombo, CB_SETCURSEL, INT(profile, UINT_PTR), 0)
                    
                    call handleIntListControl(hDlg, IDC_LIST_LEVELS, ["Layers", "mb"], gVertArr%PV, "Layer", gModIntParams%NLEV)
                    call handleIntListControl(hDlg, IDC_LIST_TEMPERATURES, ["Layer", ACHAR(176)//"K"], gVertArr%THS, "Layer", gModIntParams%NLEV)
                    call handleIntListControl(hDlg, IDC_LIST_EKMAN_FRICTION, ["Layers", "1/days"], gVertArr%FK, "Layer", gModIntParams%NLEV)
                    call handleIntListControl(hDlg, IDC_LIST_TURB_MIXING, ["Layers", "1/days"], gVertArr%FJ, "Layer", gModIntParams%NLEV)
                    hList = GetDlgItem(hDlg, IDC_LIST_TURB_MIXING)
                    bret = EnableWindow(hList, MERGE(1, 0, gModIntParams%NLEV >= 2))
                    ! Disabling does not 'gray out' the control. So one has to do it manually.
                    IF (gModIntParams%NLEV > 1) THEN
                        ! RESTORE to standard colors (Black text on White background)
                        bret = SendMessage(hList, LVM_SETTEXTCOLOR, 0, INT(RGB(0, 0, 0), kind=8))
                        bret = SendMessage(hList, LVM_SETBKCOLOR, 0, INT(RGB(255, 255, 255), kind=8))
                        bret = SendMessage(hList, LVM_SETTEXTBKCOLOR, 0, INT(RGB(255, 255, 255), kind=8))
                    ELSE
                        ! SET to "Grayed" colors
                        bret = SendMessage(hList, LVM_SETTEXTCOLOR, 0, INT(RGB(128, 128, 128), kind=8))
                        bret = SendMessage(hList, LVM_SETBKCOLOR, 0, INT(RGB(240, 240, 240), kind=8))
                        bret = SendMessage(hList, LVM_SETTEXTBKCOLOR, 0, INT(RGB(240, 240, 240), kind=8))
                    END IF
                    call handleIntListControl(hDlg, IDC_LIST_NEWT_COOLING, ["Interfaces", "1/days"], gVertArr%H, "Interface", gModIntParams%NLEV - 1)
                    call handleIntListControl(hDlg, IDC_LIST_TEMP_MIXING, ["Interfaces", "1/days"], gVertArr%G, "Interface", gModIntParams%NLEV - 2)
                    pTempDriving => ghorzArr%TF(1, 1:gModIntParams%NLEV)
                    call handleIntListControl(hDlg, IDC_LIST_THERMAL_DRIVING, ["Interfaces", ACHAR(176)//"C"], pTempDriving, "Interface", gModIntParams%NLEV - 1)
                end subroutine initializeFields

                ! Set of 'contained' subroutine and function to link list control rows to the corresponding arrays
                ! Contained methods have access to all declared variables in the parent function so one does not 
                ! need to pass in many variables and re-type 'use' statements.
                ! WM_CLICK handler functions:
                ! isListControlId: Function to check if we have clicked on one of the list controls.
                ! getRowClickedFromId: Gets the row clicked variable for the list control that was clicked and resets the others to -1.
                ! setRowClickedForCorrectListControl: Sets the value of the row clicked variable for the clicked list control.
                ! getArrayFromId: Gets the correct array to be editied based on which list control was clicked.
                ! EN_CHANGE handler for Edit box functions:
                ! getListControlHandleFromRowClicked(): Gets the handle of the corresponding List Control having the value being
                !                                       edited based on the specific row clicked variable that is >= 0.
                ! getArrayFromRowClicked(): Gets the correct array that is being edited based on the specific row clicked variable that is >= 0.
                ! ENTER key handler for Edit box functions:
                ! getListControlIdFromRowClicked(): Gets list control id based on the specific row clicked variable that is >= 0. 
                !                                   This is used to hide the edit box and return focus to the correct list control.

! ------ isListControlId()
                function isListControlId(id) result(isClicked)
                    implicit none

                    integer, intent(in) :: id

                    logical :: isClicked
                    isClicked = .false.

                    if (id == IDC_LIST_LEVELS .OR. &
                        id == IDC_LIST_TEMPERATURES .OR. &
                        id == IDC_LIST_EKMAN_FRICTION .OR. &
                        id == IDC_LIST_TURB_MIXING .OR. &
                        id == IDC_LIST_NEWT_COOLING .OR. &
                        id == IDC_LIST_TEMP_MIXING .OR. &
                        id == IDC_LIST_THERMAL_DRIVING) isClicked = .true.
                end function isListControlId
        
! ------ getListControlIdFromRowClicked()

                function getListControlIdFromRowClicked() result(id)
                    implicit none

                    integer :: id

                    id = -1
                    if (pvRowClicked >= 0) then
                        id = IDC_LIST_LEVELS
                    else if (thsRowClicked >= 0) then
                        id = IDC_LIST_TEMPERATURES
                    else if (fkRowClicked >= 0) then
                        id = IDC_LIST_EKMAN_FRICTION
                    else if (fjRowClicked >= 0) then
                        id = IDC_LIST_TURB_MIXING
                    else if (hRowClicked >= 0) then
                        id = IDC_LIST_NEWT_COOLING
                    else if (gRowClicked >= 0) then
                        id = IDC_LIST_TEMP_MIXING
                    else if (tfRowClicked >= 0) then
                        id = IDC_LIST_THERMAL_DRIVING
                    endif
                end function getListControlIdFromRowClicked

! ------ getListControlHandleFromRowClicked()

                function getListControlHandleFromRowClicked() result(hList)
                    implicit none

                    integer(HANDLE) :: hList

                    if (pvRowClicked >= 0) then
                        hList = GetDlgItem(hDlg, IDC_LIST_LEVELS)
                    else if (thsRowClicked >= 0) then 
                        hList = GetDlgItem(hDlg, IDC_LIST_TEMPERATURES)
                    else if (fkRowClicked >= 0) then 
                        hList = GetDlgItem(hDlg, IDC_LIST_EKMAN_FRICTION)
                    else if (fjRowClicked >= 0) then 
                        hList = GetDlgItem(hDlg, IDC_LIST_TURB_MIXING)
                    else if (hRowClicked >= 0) then 
                        hList = GetDlgItem(hDlg, IDC_LIST_NEWT_COOLING)
                    else if (gRowClicked >= 0) then 
                        hList = GetDlgItem(hDlg, IDC_LIST_TEMP_MIXING)
                    else if (tfRowClicked >= 0) then 
                        hList = GetDlgItem(hDlg, IDC_LIST_THERMAL_DRIVING)
                    endif
                end function getListControlHandleFromRowClicked

! ------ getArrayFromRowClicked()

                function getArrayFromRowClicked() result(ptr)
                    implicit none

                    REAL, pointer :: ptr(:)

                    if (pvRowClicked >= 0) then
                        ptr => gVertArr%PV
                    else if (thsRowClicked >= 0) then 
                        ptr => gVertArr%THS
                    else if (fkRowClicked >= 0) then 
                        ptr => gVertArr%FK
                    else if (fjRowClicked >= 0) then 
                        ptr => gVertArr%FJ
                    else if (hRowClicked >= 0) then 
                        ptr => gVertArr%H
                    else if (gRowClicked >= 0) then 
                        ptr => gVertArr%G
                    else if (tfRowClicked >= 0) then 
                        ptr => ghorzArr%TF(1, 1:gModIntParams%NLEV)
                    endif
                end function getArrayFromRowClicked

! ------ setRowClickedForCorrectListControl(rowclicked, id)

                subroutine setRowClickedForCorrectListControl(rowclicked, id)
                    implicit none

                    integer, intent(in) :: rowclicked
                    integer, intent(in) :: id

                    if (id == IDC_LIST_LEVELS) then
                        pvrowclicked = rowclicked
                    else if (id == IDC_LIST_TEMPERATURES) then
                        thsrowclicked = rowclicked
                    else if (id == IDC_LIST_EKMAN_FRICTION) then
                        fkrowclicked = rowclicked
                    else if (id == IDC_LIST_TURB_MIXING) then
                        fjrowclicked = rowclicked
                    else if (id == IDC_LIST_NEWT_COOLING) then
                        hrowclicked = rowclicked
                    else if (id == IDC_LIST_TEMP_MIXING) then
                        growclicked = rowclicked
                    else if (id == IDC_LIST_THERMAL_DRIVING) then
                        tfrowclicked = rowclicked
                    end if
                end subroutine setRowClickedForCorrectListControl

! ------ getArrayFromId()

                function getArrayFromId(id) result(ptr)
                    implicit none

                    integer, intent(in) :: id
                    REAL, pointer :: ptr(:)

                    ptr => gVertArr%PV
                    if  (id == IDC_LIST_TEMPERATURES) then
                        ptr => gVertArr%THS
                    elseif  (id == IDC_LIST_EKMAN_FRICTION) then
                        ptr => gVertArr%FK
                    else if (id == IDC_LIST_TURB_MIXING) then
                        ptr => gVertArr%FJ
                    else if (id == IDC_LIST_NEWT_COOLING) then
                        ptr => gVertArr%H
                    else if (id == IDC_LIST_TEMP_MIXING) then
                        ptr => gVertArr%G
                    else if (id == IDC_LIST_THERMAL_DRIVING) then
                        ptr => ghorzArr%TF(1, 1:gModIntParams%NLEV)
                    endif
                end function getArrayFromId

! ------ getRowClickedFromId(id)

                function getRowClickedFromId(id) result(rowClicked)
                    implicit none

                    integer, intent(in) :: id

                    integer :: rowClicked

                    pvRowClicked = -1
                    thsRowClicked = -1
                    fkRowClicked = -1
                    fjrowclicked = -1
                    hRowClicked = -1
                    gRowClicked = -1
                    tfRowClicked = -1

                    if (id == IDC_LIST_LEVELS) then
                        rowClicked = pvRowClicked
                    else if (id == IDC_LIST_TEMPERATURES) then
                        rowClicked = thsRowClicked
                    else if (id == IDC_LIST_EKMAN_FRICTION) then
                        rowClicked = fkRowClicked
                    else if (id == IDC_LIST_TURB_MIXING) then
                        rowClicked = fjRowClicked
                    else if (id == IDC_LIST_NEWT_COOLING) then
                        rowClicked = hRowClicked
                    else if (id == IDC_LIST_TEMP_MIXING) then
                        rowClicked = gRowClicked
                    else if (id == IDC_LIST_THERMAL_DRIVING) then
                        rowClicked = tfRowClicked
                    end if
                end function getRowClickedFromId

            subroutine handleIntListControl(hDlg, id, labels, vertArr, description, levels)
                use DialogHelpersModule, only: EnsureListViewColumns
                implicit none
            
                include '../resource.fd'

                integer(HANDLE), intent(in) :: hDlg
                integer, intent(in) :: id
                character(len=*), intent(in) :: labels(:)
                REAL, intent(in) :: vertArr(:)
                character(len=*), intent(in) :: description
                integer, intent(in) :: levels

                integer(HANDLE) :: hList 
                integer(bool) :: bret
                integer :: widths(2), dlgHeight
                type(T_RECT) :: rc
                type(T_POINT) :: pt
            
                hList = GetDlgItem(hDlg, id)
                bret = SendMessage(hList, LVM_DELETEALLITEMS, 0, 0) ! Clear first
                ! Load everything into the int list view
                bret = GetClientRect(hList, rc)
                widths(1) = 96  ! This is a hard coded width for the first column. It will only contain 'Level n'.
                widths(2) = rc%right - rc%left - widths(1)  ! buffer for margin/scrollbar is 16 but that is not to be there
                call EnsureListViewColumns(hList, labels, widths)
                call loadLevelsValsIntoListView(hList, vertArr, description, levels)
                bret = UpdateWindow(hList)

                ! Check that we have enough room to resize. If not we dont and we scroll.
                bret = GetClientRect(hDlg, rc)
                dlgHeight = rc%bottom   ! Note that rc%top and rc%left are both 0
                ! Getting the current position and width of the ListView control in the dialog
                bret = GetWindowRect(hList, rc) ! Gets the ListView rectangle coordinates (this is screen coordinates)
                pt%x = rc%left  ! A T_POINT struct is needed to convert the screen coordinates to dlg coordinates
                pt%y = rc%top
                bret = ScreenToClient(hDlg, pt) ! Converts the screen pt struct coordinates to dialog relative coordinates
                ! The makes the height sized to encompass the actual number of rows added
                call ResizeListViewToFitItems(hList, pt%x, pt%y, rc%right - rc%left, dlgHeight)
            end subroutine handleIntListControl

            subroutine loadLevelsValsIntoListView(hList, vertArr, description, nlevs) 
                use DialogHelpersModule, only : AddListViewRowReal
            
                implicit none

                integer(HANDLE), intent(in) :: hList
                REAL, intent(in) :: vertArr(:)
                character(len=*), intent(in) :: description
                integer, intent(in) :: nlevs

                integer :: i
                character(len=16) :: levels
                real :: zonalFactor

                zonalFactor = 1.0
                if ((GetDlgCtrlID(hList) == IDC_LIST_THERMAL_DRIVING) .AND. gHorzArr%isDimensionalized) zonalFactor = SQRT2
                !    if (profile .EQ. STRATOSPHERIC_WARMING_32_V2) then
                !        zonalFactor = 1.0 / SQRT2
                !    else
                !        zonalFactor = SQRT2
                !    endif
                !endif

                do i = 1, nlevs
                    write(levels, '(A, I0)') trim(description)//" ", i
                    call AddListViewRowReal(hList, levels, [zonalFactor * vertArr(i)], '(F8.3)') ! The 'trim' is done in the called subroutine
                end do
            end subroutine loadLevelsValsIntoListView
        
            ! Just to simplify code parsing in the dialog procedure. The user has clicked on
            ! one of the real parameters and this method gets the real value and converts it
            ! to a string to load into the edit box. The routine that actually does the work
            ! is RealToAsciiToReal. All this does is pick the correct real parameter to convert.
            !function getStringFromRealForEditBox(vertValue) result(str)
            !    use ModelParamsModule, only: gModRealParams
            !    use ConstantsModule, only: REAL_TO_ASCII
            !    use UtilitiesModule, only: LENGTH, RealToAsciiToReal
            !    
            !    implicit none
            !
            !    real, intent(inout) :: vertValue
            !
            !    character(len=LENGTH) :: str
            !    integer :: ios
            !
            !    ios = RealToAsciiToReal(REAL_TO_ASCII, vertValue, str)
            !
            !end function getStringFromRealForEditBox

            ! Just to simplify code parsing in the dialog procedure. The user has edit
            ! a real parameter. This loads the edited value into the correct field
            function readRealDataFromEditBox(str, rowClicked, vertArr, is_thermal) result(ios)
                !use ConstantsModule, only: SQRT2
                !use ModelFieldsModule, only: gHorzArr
            
                implicit none
            
                CHARACTER(:), ALLOCATABLE, intent(inout) :: str
                integer, intent(in) :: rowClicked
                REAL, intent(inout) :: vertArr(:)
                logical, intent(in) :: is_thermal

                integer :: ios
                real :: val

                ios = 0
                read(str, *, iostat = ios) val
                if (is_thermal .and. gHorzArr%isDimensionalized) then
                    val = val / SQRT2
                endif
                vertArr(rowClicked + 1) = val

            end function readRealDataFromEditBox
        end function VerticalParamsDlgProc
        
        subroutine generateVerticalProfile(profile)
            use ConstantsModule, only: DEFAULT_PROFILE, DEFAULT_WITH_USER_LEVELS, STRATOSPHERIC_WARMING_32, STRATOSPHERIC_WARMING_32_V2, SQRT2, TEMPERATURE
            use ModelFieldsModule, only: gVertArr, gHorzArr
            use ModelParamsModule, only: gModIntParams, gPlotScales
            use SetupModule, only: DFDIAB, DimensionalizeVerticalParams, DimensionalizeHorizontalParams

            implicit none

            integer(kind=4), intent(in) :: profile
            
            integer :: n

            if (profile == DEFAULT_PROFILE .OR. profile == DEFAULT_WITH_USER_LEVELS) then
                call DFDIAB(profile == DEFAULT_PROFILE)  
                ! This will create values in non-dimensional form.
                ! If the Dialog is viewing in dimensional form, we need to set these to their dimensional form
                if (gVertArr%isDimensionalized) then
                    gVertArr%isDimensionalized = .false.
                    call DimensionalizeVerticalParams()
                    
                    do n = 1, gModIntParams%NLEV - 1
                        gHorzArr%TF(1, N) = gHorzArr%TF(1, N) * gPlotScales(TEMPERATURE) / gVertArr%A(N)
                    end do
                    
                endif
            elseif (gModIntParams%NLEV == 32 .AND. &
                     (profile == STRATOSPHERIC_WARMING_32 .OR. profile == STRATOSPHERIC_WARMING_32_V2 )) then ! For this case we do not multiply by SQRT2 when loading into list view

                gVertArr%PV = [ &
                    960.0,  900.0,  840.0,  780.0,  720.0,  660.0,  600.0,  540.0, &
                    480.0,  420.0,  360.0,  300.0,  285.0,  270.0,  255.0,  240.0, &
                    225.0,  210.0,  195.0,  180.0,  165.0,  150.0,  135.0,  120.0, &
                    105.0,   90.0,   75.0,   60.0,   45.0,   30.0,   15.0,    5.0  ]
                gVertArr%THS = [ &
                    285.00, 287.96, 290.92, 293.87, 296.83, 299.79, 302.75, 305.70, &
                    308.66, 311.62, 314.58, 317.54, 318.27, 319.01, 319.75, 323.76, &
                    329.78, 336.35, 343.55, 351.51, 360.36, 370.31, 381.63, 394.70, &
                    410.06, 428.53, 451.46, 481.19, 522.44, 586.64, 715.19, 979.07  ]
                ! These arrays are of length 31
                gVertArr%FK = [ &
                    0.07,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  &
                    0.0,  0.0,  0.0,  0.0,  0.0,  50.0, 50.0, 50.0, &
                    50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, &
                    50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 40.0,  10.0  ]
                gVertArr%FJ = [ &
                    20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, &
                    20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, &
                    20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, &
                    20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0        ]
                gHorzArr%TF(1, :) = [ &
                    43.0,  46.0,  48.0,  50.0,  51.0,  52.0,  52.0,  51.0,  50.0,  44.0,  43.0,  42.0,  41.0,  41.0,  41.0,  43.0, &
                    52.0,  67.0,  100.0, 126.0, 152.0, 150.0, 151.0, 155.0, 165.0, 176.0, 190.0, 200.0, 211.0, 227.0, 271.0        ]
                gVertArr%H = [ &
                    4.0,    41.0,  40.0,  40.0,  39.0,  39.0,  40.0,  40.0,  39.0,  37.0,  40.0,  42.0,  42.0,  45.0,  49.0,  50.0, &
                    50.0,  50.0,  50.0,  50.0,  50.0,  50.0,  50.0,  50.0,  50.0,  50.0,  50.0,  50.0,  50.0,  25.0,  13.0         ]
                ! This array is only of size 30
                gVertArr%g = [ &
                    1.0, 1.2, 1.5, 1.8, 2.1, 2.6, 3.1, 3.7, &
                    4.5, 5.4, 6.5, 7.9, 8.2, 8.6, 20.0, 20.0, &
                    20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, &
                    20.0, 20.0, 20.0, 20.0, 20.0, 20.0               ]
                if (profile == STRATOSPHERIC_WARMING_32_V2) gHorzArr%TF(1, :) = gHorzArr%TF(1, :) / SQRT2
            endif
        end subroutine generateVerticalProfile


end module VerticalParamsDialogModule
