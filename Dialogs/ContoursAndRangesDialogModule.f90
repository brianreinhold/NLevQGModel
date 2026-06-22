module ContoursAndRangesDialogModule
    implicit none
    
    contains
        !/****************************************************************************
        !
        !  FUNCTION: ContoursAndRangesDlgProc(HWND, UINT, WPARAM, LPARAM)
        !
        !  PURPOSE:  Processes messages for "Settings" dialog box
        !
        !  COMMENTS: Display version information from the version section of the
        !            application resource.  Wait for user to click on "Ok" button,
        !            then close the dialog box.
        !
        !****************************************************************************/
        function ContoursAndRangesDlgProc( hDlg, dlgMsg, wParam, lParam )
        !DEC$ IF DEFINED(_X86_)
        !DEC$ ATTRIBUTES STDCALL, ALIAS : '_ContoursAndRangesDlgProc@16' :: ContoursAndRangesDlgProc
        !DEC$ ELSE
        !DEC$ ATTRIBUTES STDCALL, ALIAS : 'ContoursAndRangesDlgProc' :: ContoursAndRangesDlgProc
        !DEC$ ENDIF

            use ifwinty
            use ifwin
            use iso_c_binding

            use NLevQGModelGlobals, only: ghContoursAndRangesDlg, CenterWindow, LogMessage

            use ArrayDimensionsModule, only: gCurrentDims
            use ConstantsModule, only: REAL_TO_ASCII
            use UtilitiesModule, only: RealToAsciiToReal
            use DialogHelpersModule, only: moveEditBox, initListView, AddListViewRow, ResizeListViewToFitItems, &
                                           updateListViewValue, HideMyEdit, GetComboIndex
            use ModelParamsModule, only: gModIntParams, gContourIntervals
            use KeyboardHooksModule, only: KeyboardForDlgContoursAndRangesProc


            implicit none

            integer(LRESULT) ::  ContoursAndRangesDlgProc ! LRESULT
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

            ! For the list view report rows
            integer(HANDLE) :: hList, hEdit, hFocus
            integer, save :: cntRowClicked, grafRowClicked
            integer :: id
            CHARACTER(LEN=16) :: buffer ! Use this to read the value in the edit box

            integer :: newValue      ! For editing the displayed value in the list box
            integer :: i

            select case (dlgMsg)
                CASE (WM_APP + 300)
                    ! hide edit box
                    ID = getListControlIdFromRowClicked()
                    if (id /= -1) then
                        call HideMyEdit(GetDlgItem(hDlg, IDC_EDIT_CONTOURS))
                        hFocus = GetDlgItem(ghContoursAndRangesDlg, id)
                        bret = SetFocus(hFocus)
                    endif
                    bret = SetFocus(hFocus)
                    RETURN
                CASE (WM_APP + 301)
                    RETURN
                case (WM_INITDIALOG)   ! dlgMsg: initialize dialog box
                    cntRowClicked = -1
                    grafRowClicked = -1
                    ! Set up a hook to trap key presses before the dialog message loop handles them
                    hHook = SetWindowsHookEx(WH_KEYBOARD, LOC(KeyboardForDlgContoursAndRangesProc), NULL, GetCurrentThreadId())
                    ghContoursAndRangesDlg = hDlg
                    
                    call populateListControls()

                    ! Center the dialog over the application window
                    call CenterWindow (hDlg, GetWindow (hDlg, GW_OWNER))

                    ContoursAndRangesDlgProc = 1
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
                                        hEdit = GetDlgItem(hDlg, IDC_EDIT_CONTOURS)
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
                                        ios = RealToAsciiToReal(REAL_TO_ASCII, ptr(J + 1), str)
                                        !str = getStringFromRealForEditBox(ptr(J + 1))
                                        bret = SetWindowPos(hEdit, 0, 0, 0, 0, 0, SWP_NOMOVE .OR. SWP_NOSIZE .OR. SWP_FRAMECHANGED .OR. SWP_NOZORDER)
                                    end if
                                case default
                                    ContoursAndRangesDlgProc = 1
                                    return
                            end select
                            bret = SetDlgItemText(hDlg, IDC_EDIT_CONTOURS, str)
                        end if
                    end BLOCK

                case (WM_COMMAND)                      ! dlgMsg: received a command
                    !loWord = IAND(iData, Z'FFFF')
                    !hiWord = ISHFT(iData, -16)
                    select case (IAND(wParam, 16#FFFF))

                        case (IDOK)
                            bret = EndDialog(hDlg, IDOK)
                            ContoursAndRangesDlgProc = 1

                        case (IDCANCEL)
                            bret = EndDialog(hDlg, IDCANCEL)
                            ContoursAndRangesDlgProc = 1

!--F1:  ====================================================


!--F10: EXIT ==============================================================
                        case (IDC_BUTTON_EXIT_CONTOURS) ! F10 Close
                            bret = EndDialog(hDlg, 0)
                            ContoursAndRangesDlgProc = 1

                        ! Get result of user editing real values
                        ! Recall that for the moved the edit control to work correctly the RC file needs to be manually edit
                        ! DO THE FOLLOWING two things:
                        ! 1. Add | WS_CLIPSIBLINGS to all list controls and corresponding edit boxes.
                        ! 2. Move the EDIT controls BEFORE the list controls.
                        case (IDC_EDIT_CONTOURS)
                            SELECT CASE (ISHFT(wParam, -16))
                                case( EN_KILLFOCUS)
                                    call HideMyEdit(GetDlgItem(hDlg, IDC_EDIT_CONTOURS))
                                CASE (EN_CHANGE)
                                    EN: BLOCK
                                        character(LEN=16) :: strC ! C-string will have null terminator
                                        integer :: ios, rowClicked
                                        real, pointer :: ptr(:)
                                        real :: val
                                        logical, save :: isUpdating = .FALSE.
                                        CHARACTER(:), ALLOCATABLE :: str ! fortran string with no null terminator
                                        
                                        call LogMessage("EN_CHANGE signaled from edit real control ")
                                        if (isUpdating) exit EN
                                        isUpdating = .TRUE.
                                        hEdit = GetDlgItem(hDlg, IDC_EDIT_CONTOURS)
                                        newValue = GetDlgItemText(hDlg, IDC_EDIT_CONTOURS, strC, LEN(strC))
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
                                        ios = readRealDataFromEditBox(str, rowClicked, ptr)
                                        !write(*, '(A, A)') "Real value 3: ", str
                                        if (ios .NE. 0) then
                                            !ret = WriteMessageBox(hDlg, ios, "Input is not a valid real number", "Input Error")
                                            ios = RealToAsciiToReal(REAL_TO_ASCII, ptr(rowClicked + 1), strC)
                                            !strC = getStringFromRealForEditBox(ptr(rowClicked + 1))
                                            bret = SetDlgItemText(hDlg, IDC_EDIT_CONTOURS, strC)
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
                    ContoursAndRangesDlgProc = 1

                case (WM_DESTROY)
                    !bret = WriteCurrentStateToFile(hDlg, gModelSaveFile, gModelWelford, .true.)
                    bret = UnhookWindowsHookEx(hHook)
                    ghContoursAndRangesDlg = 0
            end select
            ContoursAndRangesDlgProc = 0 ! Didn't process the dialog message dlgMsg

        contains

                subroutine populateListControls()
                    implicit none

                    call handleIntListControl(hDlg, IDC_LIST_CONTOURS, ["Field", "interval"], gContourIntervals%Contours)
                    call handleIntListControl(hDlg, IDC_LIST_RANGES, ["Field", "range"], gContourIntervals%Ranges)

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

                    if (id == IDC_LIST_CONTOURS .OR. &
                        id == IDC_LIST_RANGES) isClicked = .true.
                end function isListControlId
        
! ------ getListControlIdFromRowClicked()

                function getListControlIdFromRowClicked() result(id)
                    implicit none

                    integer :: id

                    id = -1
                    if (cntRowClicked >= 0) then
                        id = IDC_LIST_CONTOURS
                    else if (grafRowClicked >= 0) then
                        id = IDC_LIST_RANGES
                    endif
                end function getListControlIdFromRowClicked

! ------ getListControlHandleFromRowClicked()

                function getListControlHandleFromRowClicked() result(hList)
                    implicit none

                    integer(HANDLE) :: hList

                    if (cntRowClicked >= 0) then
                        hList = GetDlgItem(hDlg, IDC_LIST_CONTOURS)
                    else if (grafRowClicked >= 0) then 
                        hList = GetDlgItem(hDlg, IDC_LIST_RANGES)
                    endif
                end function getListControlHandleFromRowClicked

! ------ getArrayFromRowClicked()

                function getArrayFromRowClicked() result(ptr)
                    use iso_c_binding
                    implicit none

                    real, dimension(:), pointer :: ptr

                    if (cntRowClicked >= 0) then
                        call get_ptr(gContourIntervals%Contours, ptr)
                    else if (grafRowClicked >= 0) then 
                        call get_ptr(gContourIntervals%Ranges, ptr)
                    endif

                end function getArrayFromRowClicked

                subroutine get_ptr(arr, p)
                    real, dimension(:), intent(in), target :: arr  ! Add TARGET here locally
                    real, dimension(:), pointer, intent(out) :: p
                    p => arr
                end subroutine get_ptr

! ------ setRowClickedForCorrectListControl(rowclicked, id)

                subroutine setRowClickedForCorrectListControl(rowclicked, id)
                    implicit none

                    integer, intent(in) :: rowclicked
                    integer, intent(in) :: id

                    if (id == IDC_LIST_CONTOURS) then
                        cntRowClicked = rowclicked
                    else if (id == IDC_LIST_RANGES) then
                        grafRowClicked = rowclicked
                    end if
                end subroutine setRowClickedForCorrectListControl

! ------ getArrayFromId()

                function getArrayFromId(id) result(ptr)
                    implicit none

                    integer, intent(in) :: id
                    REAL, pointer :: ptr(:)

                    call get_ptr(gContourIntervals%Contours, ptr)
                    if  (id == IDC_LIST_RANGES) then
                        call get_ptr(gContourIntervals%Ranges, ptr)
                    endif
                end function getArrayFromId

! ------ getRowClickedFromId(id)

                function getRowClickedFromId(id) result(rowClicked)
                    implicit none

                    integer, intent(in) :: id

                    integer :: rowClicked

                    cntRowClicked = -1
                    grafRowClicked = -1


                    if (id == IDC_LIST_CONTOURS) then
                        rowClicked = cntRowClicked
                    else if (id == IDC_LIST_RANGES) then
                        rowClicked = grafRowClicked
                    end if
                end function getRowClickedFromId

        end function ContoursAndRangesDlgProc


        subroutine handleIntListControl(hDlg, id, labels, vrArr)
            use ifwinty
            use ifwin
            use DialogHelpersModule, only: EnsureListViewColumns, ResizeListViewToFitItems
            implicit none
            
            include '../resource.fd'

            integer(HANDLE), intent(in) :: hDlg
            integer, intent(in) :: id
            character(len=*), intent(in) :: labels(:)
            REAL, intent(in) :: vrArr(:)

            integer(HANDLE) :: hList 
            integer(bool) :: bret
            integer :: widths(2), dlgHeight
            type(T_RECT) :: rc
            type(T_POINT) :: pt
            
            hList = GetDlgItem(hDlg, id)
            bret = SendMessage(hList, LVM_DELETEALLITEMS, 0, 0) ! Clear first
            ! Load everything into the int list view
            bret = GetClientRect(hList, rc)
            widths(1) = 404  ! This is a hard coded width for the first column.
            widths(2) = rc%right - rc%left - widths(1)  ! buffer for margin/scrollbar is 16 but that is not to be there
            call EnsureListViewColumns(hList, labels, widths)
            call loadLevelsValuesIntoListView(hList, vrArr)
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

        subroutine loadLevelsValuesIntoListView(hList, vrArr)
            use ifwinty
            use ifwin
            use ConstantsModule, only: VORTICITY, HEIGHT, TEMPERATURE, VERTICAL_WIND, TEMP_FORCING, TOT_HGT_TEND, ADV_BETA_HGT_TEND, &
                OMEGA_HGT_TEND, DIABATIC_HGT_TEND, TOT_TEMP_TEND, ADV_TEMP_TEND, OMEGA_TEMP_TEND, DIABATIC_TEMP_TEND, HGT_FORCING, &
                OROG_HGT_TEND, OROGRAPHY, PLOTTING_OFF
            use ArrayDimensionsModule, only: gCurrentDims
            use ModelParamsModule, only: gModIntParams
            use DialogHelpersModule, only : AddListViewRowReal
             
            implicit none
        
            integer(HANDLE), intent(in) :: hList
            REAL, intent(in) :: vrArr(:)
            
            integer :: i
            
            ! The 'trim' is done in the called subroutine
            call addlistviewrowreal(hlist, "Vorticity in 10**-5/second", [vrArr(VORTICITY)], '(f10.4)')
            call addlistviewrowreal(hlist, "Heights in meters", [vrArr(HEIGHT)], '(f10.4)')
            call addlistviewrowreal(hlist, "Potential temperature in "//char(176)//"C", [vrArr(TEMPERATURE)], '(f10.4)')
            call addlistviewrowreal(hlist, "Vertical wind in mb/hr or mm/sec", [vrArr(VERTICAL_WIND)], '(f10.4)')
            call addlistviewrowreal(hlist, "Radiative relaxation temperature in "//char(176)//"C", [vrArr(TEMP_FORCING)], '(f10.4)')
            call addlistviewrowreal(hlist, "Total Height tend. in meters/day", [vrArr(TOT_HGT_TEND)], '(f10.4)')
            call addlistviewrowreal(hlist, "Height tend in m/day from Advection plus beta", [vrArr(ADV_BETA_HGT_TEND)], '(f10.4)')
            call addlistviewrowreal(hlist, "Height tend in m/day from omega (dp/dt)", [vrArr(OMEGA_HGT_TEND)], '(f10.4)')
            call addlistviewrowreal(hlist, "Height tend in m/day from Diabatic effects", [vrArr(DIABATIC_HGT_TEND)], '(f10.4)') 
            call addlistviewrowreal(hlist, "Total temperture tendencies in C/day", [vrArr(TOT_TEMP_TEND)], '(f10.4)')
            call addlistviewrowreal(hlist, "Temperature tend in "//char(176)//"C/day from advection", [vrArr(ADV_TEMP_TEND)], '(f10.4)')
            call addlistviewrowreal(hlist, "Temperature tend in "//char(176)//"C/day from omega", [vrArr(OMEGA_TEMP_TEND)], '(f10.4)')
            call addlistviewrowreal(hlist, "Temperature tend in "//char(176)//"C/day from Diabatic effects", [vrArr(DIABATIC_TEMP_TEND)], '(f10.4)')
            call addlistviewrowreal(hlist, "Mean streamfunction (height) forcing in m/day", [vrArr(HGT_FORCING)], '(f10.4)')
            call addlistviewrowreal(hlist, "Height tend from ororgraphy in m/day'/", [vrArr(OROG_HGT_TEND)], '(f10.4)')
            call addlistviewrowreal(hlist, "Bottom orography in meters", [vrArr(OROGRAPHY)], '(f10.4)')

            i = SendMessage(hList, LVM_ENSUREVISIBLE, INT(29, INT_PTR_KIND()), TRUE)
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
        function readRealDataFromEditBox(str, rowClicked, vrArr) result(ios)
            implicit none
            
            CHARACTER(:), ALLOCATABLE, intent(inout) :: str
            integer, intent(in) :: rowClicked
            REAL, intent(inout) :: vrArr(:)


            integer :: ios

            ios = 0
            read(str, *, iostat = ios) vrArr(rowClicked + 1)
        end function readRealDataFromEditBox

end module ContoursAndRangesDialogModule
