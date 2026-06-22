module InitVariablesDialogModule
    implicit none
    
    contains
        !/****************************************************************************
        !
        !  FUNCTION: InitVariablesDlgProc(HWND, UINT, WPARAM, LPARAM)
        !
        !  PURPOSE:  Processes messages for "Settings" dialog box
        !
        !  COMMENTS: Display version information from the version section of the
        !            application resource.  Wait for user to click on "Ok" button,
        !            then close the dialog box.
        !
        !****************************************************************************/
        function InitVariablesDlgProc( hDlg, dlgMsg, wParam, lParam )
        !DEC$ IF DEFINED(_X86_)
        !DEC$ ATTRIBUTES STDCALL, ALIAS : '_InitVariablesDlgProc@16' :: InitVariablesDlgProc
        !DEC$ ELSE
        !DEC$ ATTRIBUTES STDCALL, ALIAS : 'InitVariablesDlgProc' :: InitVariablesDlgProc
        !DEC$ ENDIF

            use ifwinty
            use ifwin
            use iso_c_binding

            use NLevQGModelGlobals, only: ghInstance, ghInitVariablesDlg, CenterWindow, LogMessage

            use ArrayDimensionsModule, only: gCurrentDims
            use ConstantsModule, only: REAL_TO_ASCII, SQRT2
            use UtilitiesModule, only: RealToAsciiToReal
            use DialogHelpersModule, only: moveEditBox, initListView, AddListViewRow, ResizeListViewToFitItems, &
                                           updateListViewValue, HideMyEdit, GetComboIndex
            use ModelParamsModule, only: gModIntParams
            use KeyboardHooksModule, only: KeyboardForDlgInitVariablesProc
            use ModelFieldsModule, only: gVertArr, gHorzArr
            use SetUpModule, only: DimensionalizeHorizontalParams, NonDimensionalizeHorizontalParams
            use ViewVariablesDialogModule, only: ViewVariablesDlgProc



            implicit none

            integer(LRESULT) ::  InitVariablesDlgProc ! LRESULT
            integer(HANDLE) ::   hDlg        ! window handle of the dialog box
            integer(UINT) ::     dlgMsg
            integer(UINT_PTR) :: wParam
            integer(LONG_PTR) :: lParam

            include 'resource.fd'

            ! Local variables
            integer(KIND=4) :: lData
            integer(kind=4) :: iData
            integer(kind=4) :: iret
            integer(BOOL) ::   bret
            integer(HANDLE), save :: hHook


            type(T_RECT) :: rc
            type(T_POINT) :: pt
            
            real, pointer :: pTempDriving(:)  ! Local 1D pointer
            integer, save :: levelIndex = 1 ! Current levelIndex being edited in the list view. 
            
            ! For the list view report rows
            integer(HANDLE) :: hList, hEdit, hCheckBox, hFocus, hCombo
            integer, save :: psRowClicked, tfRowClicked, topRowClicked, sfRowClicked
            integer :: id
            CHARACTER(LEN=16) :: buffer ! Use this to read the value in the edit box

            integer :: newValue      ! For editing the displayed value in the list box
            integer :: i

            select case (dlgMsg)
                CASE (WM_APP + 300)
                    ! hide edit box
                    ID = getListControlIdFromRowClicked()
                    if (id /= -1) then
                        call HideMyEdit(GetDlgItem(hDlg, IDC_EDIT_HORZ))
                        hFocus = GetDlgItem(ghInitVariablesDlg, id)
                        bret = SetFocus(hFocus)
                    endif
                    bret = SetFocus(hFocus)
                    RETURN
                CASE (WM_APP + 301)
                    ! Zero variables
                    call ZeroVariables()
                    RETURN
                CASE (WM_APP + 302)
                    gHorzArr%PS = 0.0
                    gHorzArr%TH = 0.0
                    call populateListControls(levelIndex)
                    RETURN
                CASE (WM_APP + 303)
                    gHorzArr%TF = 0.0
                    call populateListControls(levelIndex)
                    RETURN
                CASE (WM_APP + 304)
                    gHorzArr%TOP = 0.0
                    call populateListControls(levelIndex)
                    RETURN
                CASE (WM_APP + 305)
                    gHorzArr%SF = 0.0
                    call populateListControls(levelIndex)
                    RETURN
                CASE (WM_APP + 306)
                    RETURN

                case (WM_INITDIALOG)   ! dlgMsg: initialize dialog box
                    psRowClicked = -1
                    tfRowClicked = -1
                    topRowClicked = -1
                    call DimensionalizeHorizontalParams()
                    ! Set up a hook to trap key presses before the dialog message loop handles them
                    hHook = SetWindowsHookEx(WH_KEYBOARD, LOC(KeyboardForDlgInitVariablesProc), NULL, GetCurrentThreadId())
                    ghInitVariablesDlg = hDlg
                    
                    hCombo = GetDlgItem(hDlg, IDC_COMBO_LEVEL)
                    do i = 1, gModIntParams%NLEV
                        write(buffer, '(F6.1)') gVertArr%PV(i)
                        bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc(trim(buffer)//char(0)))
                    end do
                    ! Initialize the combo box with levelIndex
                    bret = SendMessage(hCombo, CB_SETCURSEL, INT(levelIndex - 1, UINT_PTR), 0)
                    ! How many items to display in dropdown
                    !bret = SendMessage(hCombo, Z'1701', INT(min(gModIntParams%NLEV, 20)), 0) ! CB_SETMINVISIBLE = 0x1701

                    hCheckBox = GetDlgItem(hDlg, IDC_CHECK_HORZ_DIMENSIONALIZE)
                    bret = SendMessage(hCheckBox, BM_SETCHECK, BST_CHECKED, 0)
                    call populateListControls(levelIndex)

                    ! Center the dialog over the application window
                    call CenterWindow (hDlg, GetWindow (hDlg, GW_OWNER))

                    InitVariablesDlgProc = 1
                    return

                case (WM_NOTIFY)
                    BLOCK
                        type(c_ptr) :: p_raw
                        type(T_NM_LISTVIEW), pointer :: pnmv
                        integer(kind=4) :: i, j, prevRowClicked
                        integer(kind=8) :: lp
                        integer :: ios, rowClicked
                        character(len=16) :: str
                        real, pointer :: ptr(:, :)
                        real :: val
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
                                        hEdit = GetDlgItem(hDlg, IDC_EDIT_HORZ)
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
                                        ! Converts real value to string for edit box
                                        if (pnmv%hdr%idFrom .NE. IDC_LIST_INIT_OROGRAPHY) then
                                            val = ptr(J + 1, levelIndex)
                                        else
                                            val = ptr(J + 1, 1) ! Orography case
                                        endif
                                        if (gHorzArr%isDimensionalized) then
                                            if (J + 1 .LE. gModIntParams%LMAX2) then
                                                val = SQRT2 * val
                                            else
                                                val = 2.0 * val
                                            endif
                                        endif
                                        ios = RealToAsciiToReal(REAL_TO_ASCII, val, str)
                                        bret = SetWindowPos(hEdit, 0, 0, 0, 0, 0, SWP_NOMOVE .OR. SWP_NOSIZE .OR. SWP_FRAMECHANGED .OR. SWP_NOZORDER)
                                    end if
                                case default
                                    InitVariablesDlgProc = 1
                                    return
                            end select
                            bret = SetDlgItemText(hDlg, IDC_EDIT_HORZ, str)
                        end if
                    end BLOCK

                case (WM_COMMAND)                      ! dlgMsg: received a command
                    !loWord = IAND(iData, Z'FFFF')
                    !hiWord = ISHFT(iData, -16)
                    ! Combo box change
                    if (ISHFT(wParam, -16) == CBN_SELCHANGE) then
                        select case(IAND(wParam, Z'FFFF'))
                            case (IDC_COMBO_LEVEL)
                                levelIndex = GetComboIndex(hDlg, IDC_COMBO_LEVEL) + 1
                                call populateListControls(levelIndex)
                        end select
                    end if

                    select case (IAND(wParam, Z'FFFF'))

                        case (IDOK)
                            bret = EndDialog(hDlg, IDOK)
                            InitVariablesDlgProc = 1

                        case (IDCANCEL)
                            bret = EndDialog(hDlg, IDCANCEL)
                            InitVariablesDlgProc = 1

!--F1: Zero all fields ====================================================
                        case (IDC_BUTTON_INIT_ZERO_ALL)
                            call ZeroVariables()
                            InitVariablesDlgProc = 1
                            
!--F2: Zero PS ====================================================
                        case (IDC_BUTTON_INIT_ZERO_PS)
                            gHorzArr%PS = 0.0
                            gHorzArr%TH = 0.0
                            call populateListControls(levelIndex)
                            InitVariablesDlgProc = 1
!--F3: Zero TF ====================================================
                        case (IDC_BUTTON_INIT_ZERO_TF)
                            gHorzArr%TF = 0.0
                            call populateListControls(levelIndex)
                            InitVariablesDlgProc = 1
!--F4: Zero OROGRAPHY ====================================================
                        case (IDC_BUTTON_INIT_ZERO_OROGRAPHY)
                            gHorzArr%TOP= 0.0
                            call populateListControls(levelIndex)
                            InitVariablesDlgProc = 1
!--F5: Zero SF ====================================================
                        case (IDC_BUTTON_INIT_ZERO_SF)
                            gHorzArr%SF = 0.0
                            call populateListControls(levelIndex)
                            InitVariablesDlgProc = 1
                            
!--F6: View all variables ====================================================
                        case (IDC_BUTTON_VIEW_VARIABLES)
                            iret = DialogBoxParam(ghInstance, INT(IDD_VIEW_VARIABLES, LONG_PTR), hDlg, LOC(ViewVariablesDlgProc), 0_LONG_PTR)
                            InitVariablesDlgProc = 1

!--F10: EXIT ==============================================================
                        case (IDC_BUTTON_EXIT_INIT_VARIABLES) ! F10 Close
                            bret = EndDialog(hDlg, 0)
                            InitVariablesDlgProc = 1

                        case (IDC_CHECK_HORZ_DIMENSIONALIZE)
                            if (ISHFT(wParam, -16) == BN_CLICKED) then

                                ! Now we ask Windows: "Is it checked or not?"
                                if (IsDlgButtonChecked(hDlg, IDC_CHECK_HORZ_DIMENSIONALIZE) /= 0) then
                                    ! User checked it - update your variable/logic here
                                    call DimensionalizeHorizontalParams()
                                else
                                    ! User unchecked it
                                    call NonDimensionalizeHorizontalParams()
                                end if
                                bret = PostMessage(hDlg, WM_APP + 300, 0, 0)
                                call populateListControls(levelIndex)

                            endif
                            InitVariablesDlgProc = 1

                        ! Get result of user editing real values
                        ! Recall that for the moved the edit control to work correctly the RC file needs to be manually edit
                        ! DO THE FOLLOWING two things:
                        ! 1. Add | WS_CLIPSIBLINGS to all list controls and corresponding edit boxes.
                        ! 2. Move the EDIT controls BEFORE the list controls.
                        case (IDC_EDIT_HORZ)
                            SELECT CASE (ISHFT(wParam, -16))
                                case( EN_KILLFOCUS)
                                    call HideMyEdit(GetDlgItem(hDlg, IDC_EDIT_HORZ))
                                CASE (EN_CHANGE)
                                    EN: BLOCK
                                        character(LEN=16) :: strC ! C-string will have null terminator
                                        integer :: ios, rowClicked
                                        real, pointer :: ptr(:, :)
                                        real :: val
                                        logical, save :: isUpdating = .FALSE.
                                        CHARACTER(:), ALLOCATABLE :: str ! fortran string with no null terminator
                                        
                                        call LogMessage("EN_CHANGE signaled from edit real control ")
                                        if (isUpdating) exit EN
                                        isUpdating = .TRUE.
                                        hEdit = GetDlgItem(hDlg, IDC_EDIT_HORZ)
                                        newValue = GetDlgItemText(hDlg, IDC_EDIT_HORZ, strC, LEN(strC))
                                        str = strC(1 : newValue) ! Clean out the junk
                                        str = ADJUSTL(TRIM(str))
                                        if (len_trim(str) == 0 .OR. str == "." .OR. str == "-" .OR. str == "-.") then
                                            isUpdating = .FALSE.
                                            exit EN
                                        endif

                                        ! Get the handle to the list control based upon the row clicked.
                                        hList = getListControlHandleFromRowClicked()
                                        ! Get the array that is being edited based upon the row clicked
                                        ptr => getArrayFromRowClicked()

                                        ! This should be the row that was hilighted when we clicked on it. Of
                                        ! course the original value of 'rowClicked' is lost when callback returned
                                        rowClicked = SendMessage(hList, LVM_GETNEXTITEM, -1, LVNI_SELECTED)
                                        ios = 0
                                        
                                        ! Load the new value into the correct array index field
                                        if (GetDlgCtrlID(hList) == IDC_LIST_INIT_OROGRAPHY) then
                                            ios = readRealDataFromEditBox(str, rowClicked, ptr, 1)
                                        else
                                            ios = readRealDataFromEditBox(str, rowClicked, ptr, levelIndex)
                                        endif
                                        !write(*, '(A, A)') "Real value 3: ", str
                                        if (ios .NE. 0) then
                                            !ret = WriteMessageBox(hDlg, ios, "Input is not a valid real number", "Input Error")
                                            if (GetDlgCtrlID(hList) == IDC_LIST_INIT_OROGRAPHY) then
                                                val = ptr(rowClicked + 1, 1)
                                            else
                                                val = ptr(rowClicked + 1, levelIndex)
                                            endif
                                            if (gHorzArr%isDimensionalized) then
                                                if (rowClicked + 1 .LE. gModIntParams%LMAX2) then
                                                    val = SQRT2 * val
                                                else
                                                    val = 2.0 * val
                                                endif
                                            endif
                                            ios = RealToAsciiToReal(REAL_TO_ASCII, val, strC)
                                            !strC = getStringFromRealForEditBox(ptr(rowClicked + 1))
                                            bret = SetDlgItemText(hDlg, IDC_EDIT_HORZ, strC)
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
                    InitVariablesDlgProc = 1

                case (WM_DESTROY)
                    !bret = WriteCurrentStateToFile(hDlg, gModelSaveFile, gModelWelford, .true.)
                    bret = UnhookWindowsHookEx(hHook)
                    call NonDimensionalizeHorizontalParams()
                    ghInitVariablesDlg = 0
            end select
            InitVariablesDlgProc = 0 ! Didn't process the dialog message dlgMsg

        contains
                subroutine ZeroVariables()
                    implicit none
                    gHorzArr%PS = 0.0
                    gHorzArr%TF = 0.0
                    gHorzArr%TH = 0.0
                    gHorzArr%TOP = 0.0
                    gHorzArr%SF = 0.0
                    call CreateEquivBarotropic()
                    call populateListControls(levelIndex)
                end subroutine ZeroVariables

                subroutine CreateEquivBarotropic()
                    use ConstantsModule, only: HEIGHT
                    use ModelParamsModule, only: gPlotScales, gModIntParams
                    implicit none
                    integer :: N, KL
                    REAL :: scale
                    
                    scale = 1.0
                    if (gHorzArr%isDimensionalized) scale = gPlotScales(HEIGHT)
                    KL = gModIntParams%LMAX2 + 2 * gModIntParams%LMAX2 * 1 + 1
                    DO N = 1, gModIntParams%NLEV
                        !gHorzArr%PS(2, N) = .1 * scale * REAL(N - 1) / REAL(gModIntParams%NLEV)
                        gHorzArr%PS(KL, N) = .1 * scale * REAL(N - 1) / REAL(gModIntParams%NLEV)
                    END DO
                end subroutine CreateEquivBarotropic

                subroutine populateListControls(N)
                    implicit none

                    integer, intent(in) :: N

                    call handleIntListControl(hDlg, IDC_LIST_INIT_STRM, ["Component", "meters"], gHorzArr%PS, N)
                    call handleIntListControl(hDlg, IDC_LIST_INIT_TEMP_DRIVING, ["Component", ACHAR(176)//"C"], gHorzArr%TF, N)
                    ! The topography is a one-d array. We need to fake a two-d array to reuse the same handleIntListControl subroutine. 
                    ! The second dimension is just 1 in this case and the subroutine will index it accordingly.
                    ! Apparently fortran has a 'reshape' function that can do just this kind of thing!
                    call handleIntListControl(hDlg, IDC_LIST_INIT_OROGRAPHY, ["Component", "meters"], reshape(gHorzArr%TOP, [gCurrentDims%ITOT, 1]), 1)
                    call handleIntListControl(hDlg, IDC_LIST_INIT_STRM_DRIVING, ["Component", "meters"], gHorzArr%SF, N)
                    ! Keep forgetting about MERGE
                    hList = GetDlgItem(hDlg, IDC_LIST_INIT_TEMP_DRIVING)
                    bret = EnableWindow(hList, MERGE(1, 0, N < gModIntParams%NLEV))
                    ! Disabling does not 'gray out' the control. So one has to do it manually.
                    IF (N < gModIntParams%NLEV) THEN
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

                end subroutine populateListControls

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

                    if (id == IDC_LIST_INIT_STRM .OR. &
                        id == IDC_LIST_INIT_TEMP_DRIVING .OR. &
                        id == IDC_LIST_INIT_OROGRAPHY .OR. &
                        id == IDC_LIST_INIT_STRM_DRIVING) isClicked = .true.
                end function isListControlId
        
! ------ getListControlIdFromRowClicked()

                function getListControlIdFromRowClicked() result(id)
                    implicit none

                    integer :: id

                    id = -1
                    if (psRowClicked >= 0) then
                        id = IDC_LIST_INIT_STRM
                    else if (tfRowClicked >= 0) then
                        id = IDC_LIST_INIT_TEMP_DRIVING
                    else if (topRowClicked >= 0) then
                        id = IDC_LIST_INIT_OROGRAPHY
                    else if (sfRowClicked >= 0) then
                        id = IDC_LIST_INIT_STRM_DRIVING
                    endif
                end function getListControlIdFromRowClicked

! ------ getListControlHandleFromRowClicked()

                function getListControlHandleFromRowClicked() result(hList)
                    implicit none

                    integer(HANDLE) :: hList

                    if (psRowClicked >= 0) then
                        hList = GetDlgItem(hDlg, IDC_LIST_INIT_STRM)
                    else if (tfRowClicked >= 0) then 
                        hList = GetDlgItem(hDlg, IDC_LIST_INIT_TEMP_DRIVING)
                    else if (topRowClicked >= 0) then 
                        hList = GetDlgItem(hDlg, IDC_LIST_INIT_OROGRAPHY)
                    else if (sfRowClicked >= 0) then 
                        hList = GetDlgItem(hDlg, IDC_LIST_INIT_STRM_DRIVING)
                    endif
                end function getListControlHandleFromRowClicked

! ------ getArrayFromRowClicked()

                function getArrayFromRowClicked() result(ptr)
                    implicit none

                    REAL, pointer :: ptr(:, :)

                    if (psRowClicked >= 0) then
                        ptr => gHorzArr%PS
                    else if (tfRowClicked >= 0) then 
                        ptr => gHorzArr%TF
                    else if (topRowClicked >= 0) then 
                        ! Remap 1-D TOP array as a 2-D (ITOT,1) array
                        ptr(1:size(gHorzArr%TOP), 1:1) => gHorzArr%TOP
                    else if (sfRowClicked >= 0) then 
                        ptr => gHorzArr%SF
                    endif
                end function getArrayFromRowClicked

! ------ setRowClickedForCorrectListControl(rowclicked, id)

                subroutine setRowClickedForCorrectListControl(rowclicked, id)
                    implicit none

                    integer, intent(in) :: rowclicked
                    integer, intent(in) :: id

                    if (id == IDC_LIST_INIT_STRM) then
                        psRowClicked = rowclicked
                    else if (id == IDC_LIST_INIT_TEMP_DRIVING) then
                        tfRowClicked = rowclicked
                    else if (id == IDC_LIST_INIT_OROGRAPHY) then
                        topRowClicked = rowclicked
                    else if (id == IDC_LIST_INIT_STRM_DRIVING) then
                        sfRowClicked = rowclicked
                    end if
                end subroutine setRowClickedForCorrectListControl

! ------ getArrayFromId()

                function getArrayFromId(id) result(ptr)
                    implicit none

                    integer, intent(in) :: id
                    REAL, pointer :: ptr(:, :)

                    ptr => gHorzArr%PS
                    if  (id == IDC_LIST_INIT_TEMP_DRIVING) then
                        ptr => gHorzArr%TF
                    elseif  (id == IDC_LIST_INIT_OROGRAPHY) then
                        ! Remap 1-D TOP array as a 2-D (ITOT,1) array
                        ptr(1:size(gHorzArr%TOP), 1:1) => gHorzArr%TOP
                    else if (id == IDC_LIST_INIT_STRM_DRIVING) then
                        ptr => gHorzArr%SF
                    endif
                end function getArrayFromId

! ------ getRowClickedFromId(id)

                function getRowClickedFromId(id) result(rowClicked)
                    implicit none

                    integer, intent(in) :: id

                    integer :: rowClicked

                    psRowClicked = -1
                    tfRowClicked = -1
                    topRowClicked = -1
                    sfRowClicked = -1


                    if (id == IDC_LIST_INIT_STRM) then
                        rowClicked = psRowClicked
                    else if (id == IDC_LIST_INIT_TEMP_DRIVING) then
                        rowClicked = tfRowClicked
                    else if (id == IDC_LIST_INIT_OROGRAPHY) then
                        rowClicked = topRowClicked
                    else if (id == IDC_LIST_INIT_STRM_DRIVING) then
                        rowClicked = sfRowClicked
                    end if
                end function getRowClickedFromId
        end function InitVariablesDlgProc

        subroutine handleIntListControl(hDlg, id, labels, horzArr, level)
            use ifwinty
            use ifwin
            use DialogHelpersModule, only: EnsureListViewColumns, ResizeListViewToFitItems
            implicit none
            
            include '../resource.fd'

            integer(HANDLE), intent(in) :: hDlg
            integer, intent(in) :: id
            character(len=*), intent(in) :: labels(:)
            REAL, intent(in) :: horzArr(:, :)
            integer, intent(in) :: level

            integer(HANDLE) :: hList 
            integer(bool) :: bret
            integer :: widths(2), dlgHeight
            type(T_RECT) :: rc
            type(T_POINT) :: pt
            
            hList = GetDlgItem(hDlg, id)
            bret = SendMessage(hList, LVM_DELETEALLITEMS, 0, 0) ! Clear first
            ! Load everything into the int list view
            bret = GetClientRect(hList, rc)
            widths(1) = 128  ! This is a hard coded width for the first column. It will only contain 'Level n'.
            widths(2) = rc%right - rc%left - widths(1)  ! buffer for margin/scrollbar is 16 but that is not to be there
            call EnsureListViewColumns(hList, labels, widths)
            call loadLevelsValuesIntoListView(hList, horzArr, level)
            bret = SendMessage(hList, LVM_ENSUREVISIBLE, 0, 0)
            bret = UpdateWindow(hList)

            !! Getting the current position and width of the ListView control in the dialog
            !! Skip the resize when the list is so long that it exceeds the size of the dialog frame.
            ! Get the dialog size
            ! 1. Get dialog client height in client coordinates
            bret = GetClientRect(hDlg, rc)
            dlgHeight = rc%bottom   ! Note that rc%top and rc%left are both 0

            bret = GetWindowRect(hList, rc) ! Gets the ListView rectangle coordinates (this is screen coordinates)
            pt%x = rc%left  ! A T_POINT struct is needed to convert the screen coordinates to dlg coordinates
            pt%y = rc%top
            bret = ScreenToClient(hDlg, pt) ! Converts the screen pt struct coordinates to dialog relative coordinates
            ! The makes the height sized to encompass the actual number of rows added
            ! THe resize will skipped if there is not enough room in the dialog to show all the rows without scrolling.
            call ResizeListViewToFitItems(hList, pt%x, pt%y, rc%right - rc%left, dlgHeight)
        end subroutine handleIntListControl

        subroutine loadLevelsValuesIntoListView(hList, horzArr, level)
            use ifwinty
            use ifwin
            use ConstantsModule, only: SQRT2
            use ArrayDimensionsModule, only: gCurrentDims
            use ModelParamsModule, only: gModIntParams
            use ModelFieldsModule, only: gHorzArr
            use DialogHelpersModule, only : AddListViewRowReal
             
            implicit none
        
            integer(HANDLE), intent(in) :: hList
            REAL, intent(in) :: horzArr(:, :)
            integer, intent(in) :: level

            integer :: i, K, L, waveStart
            character(len=32) :: description
            real :: zonalFactor, waveFactor

            zonalFactor = 1.0
            waveFactor = 1.0
            if (gHorzArr%isDimensionalized) then
                zonalFactor = SQRT2
                waveFactor = 2.0
            endif
            do i = 1, gCurrentDims%ITOT
                if (I <= gModIntParams%LMAX2) then
                        write(description, '(A, I0, A)') "cos", i, "y"
                        call AddListViewRowReal(hList, description, [zonalFactor * horzArr(i, level)], '(F8.3)') ! The 'trim' is done in the called subroutine
                else
                    waveStart = (i - gModIntParams%LMAX2)
                    ! pattern is cosKnxsinLy, sinKnxsinLy, for all L and then for all K
                    K = (waveStart - 1) / (2 * gModIntParams%LMAX2) + 1
                    L = mod(waveStart - 1, 2 * gModIntParams%LMAX2) / 2 + 1
                    if (mod(waveStart, 2) == 1) then
                        ! Print out the cosKxSinLy for every odd waveStart
                        write(description, '(A, I0, A, I0, A)') "cosn", k, "x sin", L, "y"
                    else
                        ! Print out the sinKxSinLy for every even waveStart
                        write(description, '(A, I0, A, I0, A)') "sinn", k, "x sin", L, "y"
                    endif
                    call AddListViewRowReal(hList, description, [waveFactor * horzArr(i, level)], '(F8.3)') ! The 'trim' is done in the called subroutine
                endif
            end do
            i = SendMessage(hList, LVM_ENSUREVISIBLE, INT(gCurrentDims%ITOT - 1, INT_PTR_KIND()), TRUE)
        end subroutine loadLevelsValuesIntoListView

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
        function readRealDataFromEditBox(str, rowClicked, horzArr, level) result(ios)
            use ConstantsModule, only: SQRT2
            use ModelFieldsModule, only: gHorzArr
            use ModelParamsModule, only: gModIntParams
            
            implicit none

            CHARACTER(:), ALLOCATABLE, intent(inout) :: str
            integer, intent(in) :: rowClicked
            REAL, intent(inout) :: horzArr(:, :)
            integer, intent(in) :: level
            
            REAL :: val

            integer :: ios

            ios = 0
            read(str, *, iostat = ios) val
            if (gHorzArr%isDimensionalized) then
                if (rowClicked + 1 .LE. gModIntParams%LMAX2) then
                    val = val / SQRT2
                else
                    val = val / 2.0
                endif
            endif
            horzArr(rowClicked + 1, level) = val
        end function readRealDataFromEditBox

end module InitVariablesDialogModule
