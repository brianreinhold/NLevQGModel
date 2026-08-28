module ViewVariablesDialogModule
    implicit none
    
    contains
        !/****************************************************************************
        !
        !  FUNCTION: ViewVariablesDlgProc(HWND, UINT, WPARAM, LPARAM)
        !
        !  PURPOSE:  Processes messages for "Settings" dialog box
        !
        !  COMMENTS: Display version information from the version section of the
        !            application resource.  Wait for user to click on "Ok" button,
        !            then close the dialog box.
        !
        !****************************************************************************/
        function ViewVariablesDlgProc( hDlg, dlgMsg, wParam, lParam )
        !DEC$ IF DEFINED(_X86_)
        !DEC$ ATTRIBUTES STDCALL, ALIAS : '_ViewVariablesDlgProc@16' :: ViewVariablesDlgProc
        !DEC$ ELSE
        !DEC$ ATTRIBUTES STDCALL, ALIAS : 'ViewVariablesDlgProc' :: ViewVariablesDlgProc
        !DEC$ ENDIF

            use ifwinty
            use ifwin
            use iso_c_binding

            use NLevQGModelGlobals, only: ghViewVariablesDlg, CenterWindow, LogMessage

            use ArrayDimensionsModule, only: gCurrentDims
            use ConstantsModule, only: REAL_TO_ASCII, SQRT2
            use DialogHelpersModule, only: moveEditBox, initListView, AddListViewRow, ResizeListViewToFitItems, &
                                           updateListViewValue, HideMyEdit, GetComboIndex
            use ModelParamsModule, only: gModIntParams
            use KeyboardHooksModule, only: KeyboardForDlgViewVariablesProc
            use ModelFieldsModule, only: gVertArr, gHorzArr
            use SetUpModule, only: DimensionalizeHorizontalParams, NonDimensionalizeHorizontalParams



            implicit none

            integer(LRESULT) ::  ViewVariablesDlgProc ! LRESULT
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
                case (WM_INITDIALOG)   ! dlgMsg: initialize dialog box
                    call DimensionalizeHorizontalParams()
                    ! Set up a hook to trap key presses before the dialog message loop handles them
                    hHook = SetWindowsHookEx(WH_KEYBOARD, LOC(KeyboardForDlgViewVariablesProc), NULL, GetCurrentThreadId())
                    ghViewVariablesDlg = hDlg
                    
                    hCombo = GetDlgItem(hDlg, IDC_COMBO_VIEW_LEVELS)
                    do i = 1, gModIntParams%NLEV
                        write(buffer, '(F6.1)') gVertArr%PV(i)
                        bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc(trim(buffer)//char(0)))
                    end do
                    ! Initialize the combo box with levelIndex
                    bret = SendMessage(hCombo, CB_SETCURSEL, INT(levelIndex - 1, UINT_PTR), 0)
                    ! How many items to display in dropdown
                    !bret = SendMessage(hCombo, Z'1701', INT(min(gModIntParams%NLEV, 20)), 0) ! CB_SETMINVISIBLE = 0x1701

                    hCheckBox = GetDlgItem(hDlg, IDC_CHECK_VIEW_DIMENSIONAL)
                    bret = SendMessage(hCheckBox, BM_SETCHECK, BST_CHECKED, 0)
                    call populateListControls(levelIndex)

                    ! Center the dialog over the application window
                    call CenterWindow (hDlg, GetWindow (hDlg, GW_OWNER))

                    ViewVariablesDlgProc = 1
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
                        if (pnmv%hdr%idFrom == IDC_LIST_VIEW_VARIABLES) then
                            select case (pnmv%hdr%code)
                                case (NM_CLICK)
                                    i = pnmv%isubItem
                                    j = pnmv%iitem
                                    WRITE(buffer, '(I0, 2X, I0)') i, j
                                    !call LogMessage("Real: pnmv%isubItem (i) and pnmv%iitem (j) have values " //TRIM(buffer)//CHAR(0))
                                    if (j >= 0) then
                                    end if
                                case default
                                    ViewVariablesDlgProc = 1
                                    return
                            end select
                        end if
                    end BLOCK

                case (WM_COMMAND)                      ! dlgMsg: received a command
                    !loWord = IAND(iData, Z'FFFF')
                    !hiWord = ISHFT(iData, -16)
                    ! Combo box change
                    if (ISHFT(wParam, -16) == CBN_SELCHANGE) then
                        select case(IAND(wParam, Z'FFFF'))
                            case (IDC_COMBO_VIEW_LEVELS)
                                levelIndex = GetComboIndex(hDlg, IDC_COMBO_VIEW_LEVELS) + 1
                                call populateListControls(levelIndex)
                        end select
                    end if

                    select case (IAND(wParam, 16#FFFF))

                        case (IDOK)
                            bret = EndDialog(hDlg, IDOK)
                            ViewVariablesDlgProc = 1

                        case (IDCANCEL)
                            bret = EndDialog(hDlg, IDCANCEL)
                            ViewVariablesDlgProc = 1

!--F10: EXIT ==============================================================
                        case (IDC_BUTTON_EXIT_VIEW_VARIABLES) ! F10 Close
                            bret = EndDialog(hDlg, 0)
                            ViewVariablesDlgProc = 1

                        case (IDC_CHECK_VIEW_DIMENSIONAL)
                            if (ISHFT(wParam, -16) == BN_CLICKED) then

                                ! Now we ask Windows: "Is it checked or not?"
                                if (IsDlgButtonChecked(hDlg, IDC_CHECK_VIEW_DIMENSIONAL) /= 0) then
                                    ! User checked it - update your variable/logic here
                                    call DimensionalizeHorizontalParams()
                                else
                                    ! User unchecked it
                                    call NonDimensionalizeHorizontalParams()
                                end if
                                call populateListControls(levelIndex)
                            endif
                            ViewVariablesDlgProc = 1

                        ! Get result of user editing real values
                        ! Recall that for the moved the edit control to work correctly the RC file needs to be manually edit
                        ! DO THE FOLLOWING two things:
                        ! 1. Add | WS_CLIPSIBLINGS to all list controls and corresponding edit boxes.
                        ! 2. Move the EDIT controls BEFORE the list controls.
                    end select

                case (WM_CLOSE)
                    bret = EndDialog(hDlg, 0)
                    ViewVariablesDlgProc = 1

                case (WM_DESTROY)
                    bret = UnhookWindowsHookEx(hHook)
                    call NonDimensionalizeHorizontalParams()
                    ghViewVariablesDlg = 0
            end select
            ViewVariablesDlgProc = 0 ! Didn't process the dialog message dlgMsg

        contains
                subroutine populateListControls(N)
                    implicit none

                    integer, intent(in) :: N
                    character(len=18) :: labels(14) = [character(len=18) :: &
                              "Component         ", &
                              "Heights meters    ", &
                              "Temperature "//ACHAR(176)//"C    ", &
                              "Omega mb/hr       ", &
                              "Temp Driving      ", &
                              "Total hgt tend    ", &
                              "Adv-beta hgt tend ", &
                              "Omega hgt tend    ", &
                              "Diabatic hgt tend ", &
                              "Total Temp tend   ", &
                              "Adv temp tend     ", &
                              "Omega temp tend   ", &
                              "Diabatic temp tend", &
                              "Height Driving    "]

                    call handleViewListControl(hDlg, IDC_LIST_VIEW_VARIABLES, labels, N)
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
        end function ViewVariablesDlgProc

        subroutine handleViewListControl(hDlg, id, labels, level)
            use ifwinty
            use ifwin
            use DialogHelpersModule, only: EnsureListViewColumns, ResizeListViewToFitItems
            implicit none
            
            include '../resource.fd'

            integer(HANDLE), intent(in) :: hDlg
            integer, intent(in) :: id
            character(len=*), intent(in) :: labels(:)
            integer, intent(in) :: level

            integer(HANDLE) :: hList 
            integer(bool) :: bret
            integer :: widths(18), dlgHeight
            type(T_RECT) :: rc
            type(T_POINT) :: pt
            
            hList = GetDlgItem(hDlg, id)
            bret = SendMessage(hList, LVM_DELETEALLITEMS, 0, 0) ! Clear first
            ! Load everything into the int list view
            bret = GetClientRect(hList, rc)
            dlgHeight = (rc%right - rc%left - 128) / 13 ! buffer for margin/scrollbar is 16 but that is not to be there
            widths = dlgHeight
            widths(1) = 128
            call EnsureListViewColumns(hList, labels, widths)
            call loadLevelsValuesIntoListView(hList, level)
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
        end subroutine handleViewListControl

        subroutine loadLevelsValuesIntoListView(hList, level)
            use ifwinty
            use ifwin
            use ConstantsModule, only: SQRT2
            use ModelFieldsModule, only: gHorzArr
            use ArrayDimensionsModule, only: gCurrentDims
            use ModelParamsModule, only: gModIntParams
            use ModelFieldsModule, only: gHorzArr
             
            implicit none
        
            integer(HANDLE), intent(in) :: hList
            integer, intent(in) :: level

            integer(BOOL) :: bret
            integer :: i, K, L, M, waveStart
            character(len=32) :: description
            real :: zonalFactor, waveFactor, factor
            type(T_LVITEMW) :: lvi

            zonalFactor = 1.0
            waveFactor = 1.0
            if (gHorzArr%isDimensionalized) then
                zonalFactor = SQRT2
                waveFactor = 2.0
            endif
            
            ! Populate the labels first (win32 column 0)
            do I = 1, gCurrentDims%ITOT
                lvi%mask = LVIF_TEXT
                lvi%iItem = I - 1     ! Win32 is 0-indexed
                lvi%iSubItem = 0      ! Column 0
                if (I <= gModIntParams%LMAX2) then
                    write(description, '(A, I0, A)') "cos", i, "y"
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
                endif
                lvi%pszText = LOC(TRIM(description)//CHAR(0))

                ! This creates the row in the Windows control
                bRet = SendMessage(hList, LVM_INSERTITEM, 0, loc(lvi))
            end do
            
            do M = 1, 13
                do I = 1, gCurrentDims%ITOT
                    if (I <= gModIntParams%LMAX2) then
                        factor = zonalFactor
                    else
                        factor = waveFactor
                    endif
                    ! 1. Select the correct 2D array data based on column 'c'
                    !          "Heights meters    "
                    !          "Temperature "//ACHAR(176)//"C    "
                    !          "Omega mb/hr       "
                    !          "Temp Driving      "
                    !          "Total hgt tend    "
                    !          "Adv-beta hgt tend "
                    !          "Omega hgt tend    "
                    !          "Diabatic hgt tend "
                    !          "Total Temp tend   "
                    !          "Adv temp tend     "
                    !          "Omega temp tend   "
                    !          "Diabatic temp tend"
                    !          "Height Driving    "
                    select case(M)
                        case(1)
                            write(description, '(F11.6)') factor * gHorzArr%PS(I, level)
                        case(2)
                            write(description, '(F11.6)') factor * gHorzArr%TH(I, level)
                        case(3)
                            write(description, '(F11.6)') factor * gHorzArr%WW(I, level)
                        case(4)
                            write(description, '(F11.6)') factor * gHorzArr%TF(I, level)
                        case(5)
                            write(description, '(F11.6)') factor * gHorzArr%FS(I, level)
                        case(6)
                            write(description, '(F11.6)') factor * gHorzArr%ADS(I, level)
                        case(7)
                            write(description, '(F11.6)') factor * gHorzArr%WS(I, level)
                        case(8)
                            write(description, '(F11.6)') factor * gHorzArr%S(I, level)
                        case(9)
                            write(description, '(F11.6)') factor * gHorzArr%FT(I, level)
                        case(10)
                            write(description, '(F11.6)') factor * gHorzArr%ADT(I, level)
                        case(11)
                            write(description, '(F11.6)') factor * gHorzArr%WT(I, level)
                        case(12)
                            write(description, '(F11.6)') factor * gHorzArr%T(I, level)
                        case(13)
                            write(description, '(F11.6)') factor * gHorzArr%SF(I, level)
                    end select

                    ! 2. Target the specific row and column cell
                    lvi%mask = LVIF_TEXT
                    lvi%iItem = I - 1     ! The row we created in Step 1
                    lvi%iSubItem = M      ! The specific column
                    lvi%pszText = LOC(TRIM(description)//CHAR(0)) ! Ensure null-terminated
                    ! This updates the existing cell without creating a new row
                    bRet = SendMessage(hList, LVM_SETITEMTEXT, INT(i - 1, INT_PTR), loc(lvi))
                end do
            end do
            i = SendMessage(hList, LVM_ENSUREVISIBLE, INT(gCurrentDims%ITOT - 1, INT_PTR_KIND()), TRUE)
        end subroutine loadLevelsValuesIntoListView

end module ViewVariablesDialogModule
