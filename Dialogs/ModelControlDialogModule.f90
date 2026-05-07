module ModelControlDialogModule
    implicit none
    
    public ModelControlDlgProc
    !public AddListViewRowLoc, ResizeListViewToFitItems
    
    private reloadComboBoxFields
    private loadIntegerParameterIntoEditBox, getStringFromRealForEditBox, readRealDataFromEditBox, readIntDataFromEditBox

    contains

        function ModelControlDlgProc(hDlg, uMsg, wParam, lParam)
        !DEC$ IF DEFINED(_X86_)
        !DEC$ ATTRIBUTES STDCALL, ALIAS : '_ModelControlDlgProc@16' :: ModelControlDlgProc
        !DEC$ ELSE
        !DEC$ ATTRIBUTES STDCALL, ALIAS : 'ModelControlDlgProc' :: ModelControlDlgProc
        !DEC$ ENDIF
            use ifwinty
            use ifwin

            use NLevQGModelGlobals, only: ghInstance, ghSettingsDlg, ghModelControlDlg, gFont, ghwndMain, ghCurrentDlg, &
                                          LogMessage, WriteMessageBox, CenterWindow
            use iso_c_binding

            use ModelParamsModule, only: gModIntParams, gModRealParams, gScaleFactors, gNLevFile
            use SetUpModule, only: CheckUserSettingsChanges, CheckModelControlChanges, NonDimensionalizeModRealParams, DimensionalizeModRealParams, &
                                   ReadSaveFile, SaveModelState
            use ConstantsModule, only: WEATHER_MAP, HOV_DIAG_AT_LAT, HOV_DIAG_AT_LONG, PHASE_SPACE, NO_PLOT, TEMPERATURE, TOT_HGT_TEND_GRAF, &
                                       FIELD_START, FIELD_GRAPH_START, SECONDS_PER_DAY, CORIOLIS_INVERSE
            use KeyboardHooksModule, only: KeyboardForDlgModelControlProc, paramChange
            use ModelModule, only: InitModelWithModelControlVals, StartTimeLoop
            use DialogHelpersModule, only: GetComboIndex, moveEditBox, updateListViewValue, HideMyEdit, GetNameOfUserSelectedFile, GetUserSelectedSaveName
            use VerticalParamsDialogModule, only: VerticalParamsDlgProc
            use InitVariablesDialogModule, only: InitVariablesDlgProc
            use ContoursAndRangesDialogModule, only: ContoursAndRangesDlgProc
            use UtilitiesModule, only: openModelFile
            use PlotModule, only: ClearScreen, initializePlot, gElapsedTime

            implicit none
            
            integer(HANDLE), intent(in) :: hDlg
            integer(UINT), intent(in)   :: uMsg
            integer(UINT_PTR) :: wParam
            integer(LONG_PTR) :: lParam
            integer(LRESULT) :: ModelControlDlgProc

            character(len=270) :: selected_file
            integer(HANDLE) :: hHook
            save hHook

            include '../resource.fd'

            ! Local variables
            integer(kind=4) :: ret
            integer(BOOL) ::   bret

            ! If these resolution values get changed by the user one may need to completely reallocate the 
            ! data arrays because the new resolution exceeds that in gUserSettings. If the change in resolution
            ! is such that it less than or equal to that in gUserSettings, no reallocation is needed but the
            ! data in the current arrays need to be reshuffled to account for the new resolution. If the new
            ! sizes are less than than the original sizes, those larger sizes (higher wavenumbers) will be
            ! lost. If the size is larger, those new higher wavenumbers will be zero.
            ! We will set these values in the WM_INITDIALOG message. So we have access to them when some user
            ! action takes place (Fx-keys F1-F10) we will need to save them otherwise they will go out of scope.
            integer :: prevZonalWaves, prevMeridWaves, prevLevels
            save prevZonalWaves, prevMeridWaves, prevLevels
            ! This change will effect the interaction coefficients if the interaction coefficients integration
            ! method is being used. As above, we set it in the WM_INITDIALOG message and so it will not go
            ! out of scope we will need to save it
            real :: prevRN
            save prevRN

            type(T_RECT) :: rc
            type(T_POINT) :: pt

            ! For the list view report rows
            integer(HANDLE) :: hList, hEdit, hCombo, hCheckBox
            integer :: i
            INTEGER :: realRowClicked, intRowClicked
            save realRowClicked, intRowClicked
            CHARACTER(LEN=16) :: buffer ! Use this to read the value in the edit box

            integer :: newValue      ! For editing the displayed value in the list box
            LOGICAL :: res

            ModelControlDlgProc = 0

            select case (uMsg)

                CASE (WM_APP + 200)
                    ! Run model
                    call setupAndRunModel()
                    
                case (WM_ACTIVATE)
                    ! Check if window is being activated (WA_ACTIVE or WA_CLICKACTIVE)
                    if (IAND(wParam, 16#FFFF) /= WA_INACTIVE) then
                        ! The dialog just got focus. Update the selection here.
                        hCombo = GetDlgItem(hDlg, IDC_COMBO_MAP_TYPE)
                        bret = SendMessage(hCombo, CB_SETCURSEL, INT(gModIntParams%plotType, UINT_PTR), 0)

                        ! Force the visual refresh
                        ret = InvalidateRect(hCombo, NULL, .TRUE.)
                        ret = UpdateWindow(hCombo)
                    end if
                    ModelControlDlgProc = 0
                    return

                case (WM_INITDIALOG)
                    ghCurrentDlg = hDlg
                    realRowClicked = -1
                    intRowClicked = -1
                    ! When we get here we have come from the F4 option of the Main Menu. We need to
                    ! check if the user has changed the gUserSettings options that might require
                    ! reallocation of the ModelFieldsModule arrays
                    ret = CheckUserSettingsChanges()
                    if (ret .NE. 0) then
                        ret = MessageBox (hDlg, &
                            "Unable to handle the new user settings"C, &
                            "Error encountered"C, &
                            IOR(MB_SYSTEMMODAL, IOR(MB_OK, MB_ICONHAND)))
                        bret = DestroyWindow(hDlg)
                        ModelControlDlgProc = 1
                        return
                    end if
                    
                    ! If we get here we need to save some of the fields in modelIntParams and modelRealParams in case
                    ! the user changes them.
                    paramChange%zonalWaves = gModIntParams%KMAX2   ! Save the current number of zonal waves
                    paramChange%meridWaves = gModIntParams%LMAX2   ! Save the current number of meridional waves
                    paramChange%levels     = gModIntParams%NLEV    ! Save the current number of model levels
                    paramChange%RN         = gModRealParams%RN     ! Save the current channel length. A change will alter the interaction coefficients
                                                                   ! The saved RN value here is non dimensional

                    call DimensionalizeModRealParams()
                    hCheckBox = GetDlgItem(hDlg, IDC_CHECK_MODREAL_DIMENSIONALIZE)
                    bret = SendMessage(hCheckBox, BM_SETCHECK, BST_CHECKED, 0)

                    ! Set up a hook to trap key presses before the dialog message loop handles them
                    hHook = SetWindowsHookEx(WH_KEYBOARD, LOC(KeyboardForDlgModelControlProc), NULL, GetCurrentThreadId())
                    ghModelControlDlg = hDlg

                    !! Combo boxes require manual editing of their height if you want the list of options
                    !! to be exposed on click only. Do it in the rc file. You have to increase the height
                    !! by 25 * (number of rows - 1) for times new roman 14 font, in any case.

                    !! Map type
                    hCombo = GetDlgItem(hDlg, IDC_COMBO_MAP_TYPE)
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Weather Maps'//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Weather Maps and phase space combo'//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Vertical Cross section at latitude X '//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Hovmoller diagram at latitude X'//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Vertical Cross section at longitude Y'//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Hovmoller diagram at longitude Y'//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Phase space plot'//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Wave-amplitude time series'//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Wave-phase time series'//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Weather station time series'//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Skip plot for speed'//char(0)))

                    ! Initialize the plotType combo box
                    bret = SendMessage(hCombo, CB_SETCURSEL, INT(gModIntParams%plotType, UINT_PTR), 0)

                    !! Fields 1, 2, 3
                    ! These are made difficult as the quantities that can be displayed depend upon
                    ! the type of plot plotType. For map-type plots, there can be three fields. For time
                    ! series plots, field2 and field3 are ignored.
                    ! If plotType is a map type, we load field1 with map options, otherwise we load with 
                    ! time series options. Fields 2 and 3 are always loaded with map options.
                    ! When plotType changes, we have to check and see if we have switched between time series
                    ! and maps or vice versa, and if so we reload FIELD1 with the appropriate options.
                    ! This is done in 'reloadComboBoxFields'
                    bret = reloadComboBoxFields(hDlg, gModIntParams%plotType, IDC_COMBO_FIELD1)
                    bret = reloadComboBoxFields(hDlg, WEATHER_MAP, IDC_COMBO_FIELD2)   ! Always load with map options
                    bret = reloadComboBoxFields(hDlg, WEATHER_MAP, IDC_COMBO_FIELD3)   ! Always load with map options
                    ! initialize FIELD1
                    hCombo = GetDlgItem(hDlg, IDC_COMBO_FIELD1)
                    !if (gModIntParams%plotType .LE. HOV_DIAG_AT_LONG) then
                    bret = SendMessage(hCombo, CB_SETCURSEL, INT(gModIntParams%field1 - FIELD_START, UINT_PTR), 0) ! Silly cast avoids warning.Still runs without it
                    !else
                    !    bret = SendMessage(hCombo, CB_SETCURSEL, INT(gModIntParams%field1 - FIELD_GRAPH_START, UINT_PTR), 0)
                    !end if
                    ! initialize FIELD2
                    hCombo = GetDlgItem(hDlg, IDC_COMBO_FIELD2)
                    bret = SendMessage(hCombo, CB_SETCURSEL, INT(gModIntParams%field2 - FIELD_START, UINT_PTR), 0)
                    ! Do not enable this option unless the type of plot is one of the maps (hovmoller or weather map or phase space)
                    bret = EnableWindow(hCombo, MERGE(1_BOOL, 0_BOOL, gModIntParams%plotType .LE. PHASE_SPACE)) ! Cast for logical to INTEGER(BOOL) to avoid warning
                    ! initialize FIELD3
                    hCombo = GetDlgItem(hDlg, IDC_COMBO_FIELD3)
                    bret = SendMessage(hCombo, CB_SETCURSEL, INT(gModIntParams%field3 - FIELD_START, UINT_PTR), 0)
                    ! Do not enable this option unless the type of plot is one of the maps (hovmoller or weather map)
                    bret = EnableWindow(hCombo, MERGE(1_BOOL, 0_BOOL, gModIntParams%plotType .LE. HOV_DIAG_AT_LONG)) ! No three fields in phase space

                    !! Model type
                    hCombo = GetDlgItem(hDlg, IDC_COMBO_MODEL_TYPE)
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Fully nonlinear N-level QG model'//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('G-Q linearized initial-value model '//char(0)))
                    bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Not implemented yet - dont use'//char(0)))
                    ! initialize
                    bret = SendMessage(hCombo, CB_SETCURSEL, INT(gModIntParams%modelType, UINT_PTR), 0) ! These are zero based in our code

                    !---------- LIST CONTROLS
                    ! REAL:
                    call handleRealListControl(hDlg)

                    ! INTEGER:
                    call handleIntListControl(hDlg)

                    ! Center the dialog over the application window  !
                    call CenterWindow (hDlg, GetWindow (hDlg, GW_OWNER))

                    ModelControlDlgProc = 1

                case (WM_NOTIFY)
                    BLOCK
                        ! User has clicked on a row in one of the ListView controls to edit a value
                        type(c_ptr) :: p_raw
                        type(T_NM_LISTVIEW), pointer :: pnmv
                        !TYPE(T_RECT) :: rect_bounds
                        integer(kind=4) :: i, j
                        integer(kind=8) :: lp
                        character(len=16) :: str
                        integer :: ios
                        ! Step 1: cast lParam to a C_PTR
                        lp = lParam
                        p_raw = transfer(lp, p_raw)
                        ! Step 2: cast to a Fortran structure pointer
                        call c_f_pointer(p_raw, pnmv)
                        ! User click on the list view row for real values
                        if (pnmv%hdr%idFrom == IDC_REALVALUES) then
                            select case (pnmv%hdr%code)
                                case (NM_CLICK)
                                    i = pnmv%isubItem
                                    j = pnmv%iitem
                                    WRITE(buffer, '(I0, 2X, I0)') i, j
                                    call LogMessage("Real: pnmv%isubItem (i) and pnmv%iitem (j) have values " //TRIM(buffer)//CHAR(0))
                                    if (j >= 0) then
                                        intRowClicked = -1 ! Reset integer row clicked
                                        hList = GetDlgItem(hDlg, IDC_REALVALUES)
                                        hEdit = GetDlgItem(hDlg, IDC_EDIT_REALVALUES)
                                        ios = realRowClicked
                                        realRowClicked = SendMessage(hList, LVM_GETNEXTITEM, -1, LVNI_SELECTED)
                                        if (ios .EQ. realRowClicked) then
                                            bret = SetFocus(hEdit)
                                            bret = SetWindowPos(hEdit, 0, 0, 0, 0, 0, SWP_NOMOVE .OR. SWP_NOSIZE .OR. SWP_FRAMECHANGED .OR. SWP_NOZORDER)
                                            return
                                        end if
                                        ! Hides the integer edit box if it is visible
                                        bret = ShowWindow(GetDlgItem(hDlg, IDC_EDIT_INTEGERVALUES), SW_HIDE)
                                        ! Moves the edit box over the value and focuses it
                                        call moveEditBox(hDlg, hList, hEdit, realRowClicked) ! (method in NlevelQGModelglobals))

                                        ! Method picks the correct real parameter to convert to string for edit box
                                        str = getStringFromRealForEditBox(j)
                                        bret = SetWindowPos(hEdit, 0, 0, 0, 0, 0, SWP_NOMOVE .OR. SWP_NOSIZE .OR. SWP_FRAMECHANGED .OR. SWP_NOZORDER)
                                    end if
                                case default
                                    ModelControlDlgProc = 0
                                    return
                            end select
                            bret = SetDlgItemText(hDlg, IDC_EDIT_REALVALUES, str)
                        end if
                        if (pnmv%hdr%idFrom == IDC_INTEGERVALUES) then
                            select case (pnmv%hdr%code)
                                case (NM_CLICK)
                                    i = pnmv%isubItem
                                    j = pnmv%iitem
                                    WRITE(buffer, '(I0, 2X, I0)') i, j
                                    call LogMessage("Integer: pnmv%isubItem (i) and pnmv%iitem (j) have values " //TRIM(buffer)//CHAR(0))

                                    if (j >= 0) then
                                        realRowClicked = -1 ! Reset real row clicked
                                        hList = GetDlgItem(hDlg, IDC_INTEGERVALUES)
                                        hEdit = GetDlgItem(hDlg, IDC_EDIT_INTEGERVALUES)
                                        ios = intRowClicked
                                        intRowClicked = SendMessage(hList, LVM_GETNEXTITEM, -1, LVNI_SELECTED)
                                        if (ios .EQ. intRowClicked) then
                                            bret = SetFocus(hEdit)
                                            bret = SetWindowPos(hEdit, 0, 0, 0, 0, 0, SWP_NOMOVE .OR. SWP_NOSIZE .OR. SWP_FRAMECHANGED .OR. SWP_NOZORDER)
                                            return
                                        end if
                                        ! Hides the real edit box if it is visible
                                        bret = ShowWindow(GetDlgItem(hDlg, IDC_EDIT_REALVALUES), SW_HIDE)
                                        ! Moves the edit box over the value and focuses it
                                        call moveEditBox(hDlg, hList, hEdit, intRowClicked) ! (method in NlevelQGModelglobals))
                                        ! Method selects the integer in gModIntParams to load into the edit box
                                        bret = loadIntegerParameterIntoEditBox(hDlg, j, IDC_EDIT_INTEGERVALUES)
                                        bret = SetWindowPos(hEdit, 0, 0, 0, 0, 0, SWP_NOMOVE .OR. SWP_NOSIZE .OR. SWP_FRAMECHANGED .OR. SWP_NOZORDER)
                                    end if
                                case default
                                    ModelControlDlgProc = 1
                                    return
                            end select
                        end if
                    end BLOCK

                case (WM_COMMAND)
                    !loWord = IAND(wParam, Z'FFFF')
                    !hiWord = ISHFT(wParam, -16)
                    if (ISHFT(wParam, -16) == CBN_SELCHANGE) then ! Combo box select
                        BLOCK
                            integer :: previousMPLOT, addoffset
                            ! We need this offset to map the combo box indicies with our indicies. Combo
                            ! box indicies begin with 0, our begin with 1 for maps and 18 for time series
                            addOffset = 1
                            if (gModIntParams%plotType .GT. PHASE_SPACE) addOffset = FIELD_GRAPH_START
                            select case(IAND(wParam, Z'FFFF'))
                                ! The tricky thing here is if the user changes the plot type from map
                                ! to time series or vice versa. Then we have to load different options
                                ! into the field1 dropdown and disable or enable the other two combo boxes.
                                ! It gets messy.
                                case (IDC_COMBO_MAP_TYPE)
                                    previousMPLOT = gModIntParams%plotType
                                    gModIntParams%plotType = GetComboIndex(hDlg, IDC_COMBO_MAP_TYPE) ! These are zero based
                                    ! Did plotType change from maps to time series?
                                    ! plotType <= 4 is map plotType > 4 is time series
                                    !if ((gModIntParams%plotType .GT. HOV_DIAG_AT_LONG .AND. previousMPLOT .LE. HOV_DIAG_AT_LONG) .OR. &
                                    !    (gModIntParams%plotType .LE. HOV_DIAG_AT_LONG .AND. previousMPLOT .GT. HOV_DIAG_AT_LONG)) Then
                                    !    ! We need to change the contents of what is in the COMBO_FIELD1 dropdown
                                    !    ! Field2 and Field3 can only be used with maps and will be enabled/disabled appropriately
                                    !    hCombo = GetDlgItem(hDlg, IDC_COMBO_FIELD1)
                                    !    ! This loads the new set of options (clearing the old first)
                                    !    bret = reloadComboBoxFields(hDlg, gModIntParams%plotType, IDC_COMBO_FIELD1)
                                    !    ! Set a new default value for the field since the existing one is for time series
                                    !    !if (gModIntParams%plotType .LE. HOV_DIAG_AT_LONG) then
                                    !        gModIntParams%field1 = TEMPERATURE
                                    !        bret = SendMessage(hCombo, CB_SETCURSEL, INT(gModIntParams%field1 - FIELD_START, UINT_PTR), 0)
                                    !    !else
                                    !    !    gModIntParams%field1 = TOT_HGT_TEND_GRAF
                                    !    !    ! This is for getting by NO_PLOT which is the first field in the graphs options (check this)
                                    !    !    bret = SendMessage(hCombo, CB_SETCURSEL, INT(gModIntParams%field1 - NO_PLOT, UINT_PTR), 0)
                                    !    !end if
                                    !    ! Enable or disable as the case may be
                                    !    hCombo = GetDlgItem(hDlg, IDC_COMBO_FIELD2)
                                    !    bret = EnableWindow(hCombo, MERGE(1_BOOL, 0_BOOL, gModIntParams%plotType .LE. PHASE_SPACE))
                                    !    hCombo = GetDlgItem(hDlg, IDC_COMBO_FIELD3)
                                    !    bret = EnableWindow(hCombo, MERGE(1_BOOL, 0_BOOL, gModIntParams%plotType .LE. HOV_DIAG_AT_LONG))
                                    !end if
                                case (IDC_COMBO_FIELD1)
                                    gModIntParams%field1 = GetComboIndex(hDlg, IDC_COMBO_FIELD1) + addOffset
                                case (IDC_COMBO_FIELD2)
                                    gModIntParams%field2 = GetComboIndex(hDlg, IDC_COMBO_FIELD2) + addOffset
                                case (IDC_COMBO_FIELD3)
                                    gModIntParams%field3 = GetComboIndex(hDlg, IDC_COMBO_FIELD3) + addOffset
                                case (IDC_COMBO_MODEL_TYPE)
                                    gModIntParams%ModelType = GetComboIndex(hDlg, IDC_COMBO_MODEL_TYPE)
                            end select
                        end BLOCK
                    end if
                    select case (IAND(wParam, Z'FFFF'))
                        case (IDOK)
                            bret = UnhookWindowsHookEx(hHook)
                            bret = DestroyWindow(hDlg)
                            ModelControlDlgProc = 1
                        case (IDCANCEL)
                            bret = UnhookWindowsHookEx(hHook)
                            bret = DestroyWindow(hDlg)
                            ModelControlDlgProc = 1
                            
                        ! Get result of user editing real values
                        ! Recall that for the moved the edit control to work correctly the RC file needs to be manually edit
                        ! DO THE FOLLOWING two things:
                        ! 1. Add | WS_CLIPSIBLINGS to all list controls and corresponding edit boxes.
                        ! 2. Move the EDIT controls BEFORE the list controls.
                        case (IDC_EDIT_REALVALUES)
                            SELECT CASE (ISHFT(wParam, -16))
                                CASE (EN_CHANGE)
                                    EN: BLOCK
                                        character(LEN=16) :: strC ! C-string will have null terminator
                                        integer :: ios
                                        CHARACTER(:), ALLOCATABLE :: str ! fortran string with no null terminator
                                        logical, save :: isUpdating = .FALSE.

                                        call LogMessage("EN_CHANGE signaled from edit real control ")
                                        if (isUpdating) exit EN
                                        isUpdating = .TRUE.
                                        hEdit = GetDlgItem(hDlg, IDC_EDIT_REALVALUES)
                                        newValue = GetDlgItemText(hDlg, IDC_EDIT_REALVALUES, strC, LEN(strC))
                                        str = strC(1 : newValue) ! Clean out the junk
                                        str = ADJUSTL(TRIM(str))
                                        if (len_trim(str) == 0 .OR. str == "." .OR. str == "-" .OR. str == "-.") then
                                            isUpdating = .FALSE.
                                            exit EN
                                        endif
                                        ! This should be the row that was hilighted when we clicked on it. Of
                                        ! course the original value of 'realRowClicked' is lost when callback returned
                                        hList = GetDlgItem(hDlg, IDC_REALVALUES)
                                        realRowClicked = SendMessage(hList, LVM_GETNEXTITEM, -1, LVNI_SELECTED)
                                        ios = 0
                                        ! Load the new value into the correct modRealParam field
                                        ios = readRealDataFromEditBox(str, realRowClicked)
                                        if (ios .NE. 0) then
                                            !ret = WriteMessageBox(hDlg, ios, "Input is not a valid real number", "Input Error")
                                            strC = getStringFromRealForEditBox(realRowClicked)
                                            bret = SetDlgItemText(hDlg, IDC_EDIT_REALVALUES, strC)
                                        end if
                                        call updateListViewValue(hEdit, hList, realRowClicked, strC)
                                        isUpdating = .FALSE.
                                    end BLOCK EN
                            end SELECT

                        ! Recall that for the moved the edit control to work correctly the RC file needs to be manually edit
                        ! DO THE FOLLOWING two things:
                        ! 1. Add | WS_CLIPSIBLINGS to all list controls and corresponding edit boxes.
                        ! 2. Move the EDIT controls BEFORE the list controls.
                        CASE (IDC_EDIT_INTEGERVALUES) ! The ID of the integer edit box
                            SELECT CASE (ISHFT(wParam, -16))
                                CASE (EN_CHANGE)
                                    BLOCK
                                        character(LEN=16) :: strC ! C-string will have null terminator

                                        call LogMessage("EN_CHANGE signaled from edit integer control ")
                                        hEdit = GetDlgItem(hDlg, IDC_EDIT_INTEGERVALUES)
                                        ! Get the updated text from the edit control
                                        newValue = GetDlgItemInt(hDlg, IDC_EDIT_INTEGERVALUES, LOC(bret), .TRUE.)
                                        hList = GetDlgItem(hDlg, IDC_INTEGERVALUES)

                                        ! This should be the row that we clicked. Of course the original value of 'intRowClicked'
                                        ! is lost when callback returned so we need to save it
                                        intRowClicked = SendMessage(hList, LVM_GETNEXTITEM, -1, LVNI_SELECTED)
                                        ! Load value into the gModIntParams struct
                                        call readIntDataFromEditBox(intRowClicked, newValue)
                                        ! Now get the item as a string to update the parameter in the list
                                        bret = GetDlgItemText(hDlg, IDC_EDIT_INTEGERVALUES, strC, LEN(strC))
                                        call updateListViewValue(hEdit, hList, intRowClicked, strC)
                                    end BLOCK
                            END SELECT
                        case (IDC_CHECK_MODREAL_DIMENSIONALIZE)
                            if (ISHFT(wParam, -16) == BN_CLICKED) then
        
                                ! Now we ask Windows: "Is it checked or not?"
                                if (IsDlgButtonChecked(hDlg, IDC_CHECK_MODREAL_DIMENSIONALIZE) /= 0) then
                                    ! User checked it - update your variable/logic here
                                    call DimensionalizeModRealParams()
                                else
                                    ! User unchecked it
                                    call NonDimensionalizeModRealParams()
                                end if
                                bret = SetFocus(GetDlgItem(hDlg, IDC_REALVALUES))
                                call HideMyEdit(GetDlgItem(hDlg, IDC_EDIT_REALVALUES))
                                call handleRealListControl(hDlg)
                            endif

! ------------------ Now to handle the F1-F10 buttons
! ---- F1:  Run Model
                        case (IDC_BUTTON_RUN_MODEL) ! F1 Run model
                            call setupAndRunModel()

! ---- F2:  Edit Vertical Parameters
                        case (IDC_BUTTON_VERTICAL_PARAMS)
                            ret = CheckModelControlChanges( paramChange%zonalWaves, paramChange%meridWaves, paramChange%levels, paramChange%RN)
                            ret = InitModelWithModelControlVals()
                            ret = DialogBoxParam(ghInstance, INT(IDD_VERTICAL_PARAMS, LONG_PTR), hDlg, LOC(VerticalParamsDlgProc), 0_LONG_PTR)
                            ghModelControlDlg = hDlg

! ---- F3:  Initialize flow variables
                        case (IDC_BUTTON_INIT_FLOW_VARIABLES)
                            ret = CheckModelControlChanges( paramChange%zonalWaves, paramChange%meridWaves, paramChange%levels, paramChange%RN)
                            ret = InitModelWithModelControlVals()
                            ret = DialogBoxParam(ghInstance, INT(IDD_INIT_VARIABLES, LONG_PTR), hDlg, LOC(InitVariablesDlgProc), 0_LONG_PTR)
                            ghModelControlDlg = hDlg

! ---- F4:  Save Current Flow
                        case (IDC_BUTTON_SAVE_CURRENT_FLOW)
                            res = GetUserSelectedSaveName(hDlg, gNLevFile)
                            if (res) then
                                ret = openModelFile(gNLevFile, .false., hDlg)
                                if (ret == 0) ret = SaveModelState(gNLevFile, .false.)
                            endif
                            ghModelControlDlg = hDlg

! ---- F5:  Load Saved Flow
                        case (IDC_BUTTON_LOAD_SAVED_FLOW)
                            res = GetNameOfUserSelectedFile(hDlg, gNLevFile)
                            if (res) then
                                if (len_trim(gNLevFile%fileName) == 0) return
                                call LogMessage("Save file loaded: "// selected_file)
                                ret = openModelFile(gNLevFile, .true., hDlg)
                                if (ret == 0) ret = ReadSaveFile(gNLevFile)
                            endif
                            ghModelControlDlg = hDlg

! ---- F7:  EditContours
                        case (IDC_BUTTON_CONTOURS)
                            ret = DialogBoxParam(ghInstance, INT(IDD_CONTOURS, LONG_PTR), hDlg, LOC(ContoursAndRangesDlgProc), 0_LONG_PTR)
                            ghModelControlDlg = hDlg

! ---- F8:  Clear Screen
                        case (IDC_BUTTON_CLEAR_SCREEN)
                            call ClearScreen()
                            ghModelControlDlg = hDlg


! ---- F10:  Exit to main
                        case (IDC_BUTTON_RETURN_TO_MAIN) ! F10 Close
                            bret = DestroyWindow(hDlg)
                    end select

                case (WM_CLOSE)
                    bret = UnhookWindowsHookEx(hHook)
                    ModelControlDlgProc = 1
                    bret = DestroyWindow(hDlg)
                case (WM_DESTROY)
                    ! We have to call this here in case the user sets a parameter but then just returns to the main menu.
                    ret = CheckModelControlChanges( paramChange%zonalWaves, paramChange%meridWaves, paramChange%levels, paramChange%RN)
                    call NonDimensionalizeModRealParams() ! Method checks if already non-dimensionalized
                    bret = EnableWindow(ghSettingsDlg, .TRUE.)
                    bret = SetForegroundWindow(ghSettingsDlg)
                    bret = ShowWindow(ghSettingsDlg, SW_SHOW)
                    ghModelControlDlg = 0
            end select

            contains
                subroutine setupAndRunModel()
                    implicit none
                    ! paramChange is defined in KeyboardHooksModule
                    ! Non dimensionalized in the following function
                    ret = CheckModelControlChanges( paramChange%zonalWaves, paramChange%meridWaves, paramChange%levels, paramChange%RN)
                    ret = InitModelWithModelControlVals()
                    bret = EnableWindow(hDlg, .FALSE.)
                    gElapsedTime%dimensionalTime = 4.0 * CORIOLIS_INVERSE * gModRealParams%DELT / SECONDS_PER_DAY
                    gElapsedTime%timeSteps = 0
                    call ClearScreen()
                    bret = SetFocus(ghwndMain)
                    call initializePlot() ! Sets up all the plot type structure for computations that do not need to be repeated every time step plotted.
                    ret = StartTimeLoop()
                    paramChange%zonalWaves = gModIntParams%KMAX2   ! Save the current number of zonal waves
                    paramChange%meridWaves = gModIntParams%LMAX2   ! Save the current number of meridional waves
                    paramChange%levels     = gModIntParams%NLEV    ! Save the current number of model levels
                    paramChange%RN         = gModRealParams%RN     ! Save the current channel length. A change will alter the interaction coefficients
                    call DimensionalizeModRealParams() ! Method checks if already dimensionalized
                end subroutine setupAndRunModel

        end function ModelControlDlgProc

!===================================== PRIVATE HELPERS


        subroutine handleRealListControl(hDlg)
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
            
            hList = GetDlgItem(hDlg, IDC_REALVALUES)
            bret = SendMessage(hList, LVM_DELETEALLITEMS, 0, 0) ! Clear first
            ! Load everything into the real list view
            bret = GetClientRect(hList, rc)
            widths(1) = 404
            widths(2) = rc%right - rc%left - widths(1) ! buffer for margin/scrollbar is 16 but that is not to be there
            call EnsureListViewColumns(hList, ["Model Physical parameters", "Value"], widths)
            call loadRealItemsIntoListView(hList)
            bret = UpdateWindow(hList)

            ! Getting the current position and width of the ListView control in the dialog
            bret = GetWindowRect(hList, rc) ! Gets the ListView rectangle coordinates (this is screen coordinates)
            pt%x = rc%left  ! A T_POINT struct is needed to convert the screen coordinates to dlg coordinates
            pt%y = rc%top
            bret = ScreenToClient(hDlg, pt) ! Converts the screen pt struct coordinates to dialog relative coordinates
            ! The makes the height sized to encompass the actual number of rows added
            call ResizeListViewToFitItems(hList, pt%x, pt%y, rc%right - rc%left)
        end subroutine handleRealListControl
        
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
            
            hList = GetDlgItem(hDlg, IDC_INTEGERVALUES)
            bret = SendMessage(hList, LVM_DELETEALLITEMS, 0, 0) ! Clear first
            ! Load everything into the int list view
            bret = GetClientRect(hList, rc)
            widths(1) = 450
            widths(2) = rc%right - rc%left - 450  ! buffer for margin/scrollbar is 16 but that is not to be there
            call EnsureListViewColumns(hList, ["Model Configuration parameters", "Values"], widths)
            call loadIntItemsIntoListView(hList)
            bret = UpdateWindow(hList)

            ! Getting the current position and width of the ListView control in the dialog
            bret = GetWindowRect(hList, rc) ! Gets the ListView rectangle coordinates (this is screen coordinates)
            pt%x = rc%left  ! A T_POINT struct is needed to convert the screen coordinates to dlg coordinates
            pt%y = rc%top
            bret = ScreenToClient(hDlg, pt) ! Converts the screen pt struct coordinates to dialog relative coordinates
            ! The makes the height sized to encompass the actual number of rows added
            call ResizeListViewToFitItems(hList, pt%x, pt%y, rc%right - rc%left)
        end subroutine handleIntListControl

        ! Just to simplify code parsing in the dialog procedure. Loading the
        ! real list box with labels and values from gModRealParams so the user
        ! can edit them if so desired.
        subroutine loadrealitemsintolistview(hlist)
            use ModelParamsModule, only: gModRealParams
            use DialogHelpersModule, only: addlistviewrowreal
            use ifwinty, only: handle
        
            integer(handle), intent(in) :: hlist
        
            call addlistviewrowreal(hlist, "channel length in kilometers", [gModRealParams%RN], '(f10.4)')
            call addlistviewrowreal(hlist, "beta parameter relative to 45 deg. latitude", [gModRealParams%BETA], '(f10.4)')
            call addlistviewrowreal(hlist, "time step length in minutes", [gModRealParams%DELT], '(f10.4)')
            call addlistviewrowreal(hlist, "pressure level of the first quantity in mb", [gModRealParams%field1Level], '(f10.4)')
            call addlistviewrowreal(hlist, "pressure level of the second quantity in mb", [gModRealParams%field2Level], '(f10.4)')
            call addlistviewrowreal(hlist, "pressure level of the third quantity in mb", [gModRealParams%field3Level], '(f10.4)')
            call addlistviewrowreal(hlist, "latitude of x-hovmoller and cross-sections", [gModRealParams%hovLat], '(f10.4)')
            call addlistviewrowreal(hlist, "longitude of y-hovmoller and cross-sections", [gModRealParams%hovLon], '(f10.4)')
            call addlistviewrowreal(hlist, "longitude of selected point for a weather station", [gModRealParams%lonXPoint], '(f10.4)')
            call addlistviewrowreal(hlist, "latitude of selected point for a weather station", [gModRealParams%latYPoint], '(f10.4)')
            call addlistviewrowreal(hlist, "lowest level displayed in vertical cross sections", [gModRealParams%BOT], '(f10.4)')
            call addlistviewrowreal(hlist, "highest level displayed in vertical cross sections", [gModRealParams%TPP], '(f10.4)')
            call addlistviewrowreal(hlist, "convergence test threshold for sensitivity runs", [gModRealParams%CONV], '(f10.4)')
        end subroutine loadrealitemsintolistview

        ! Just to simplify code parsing in the dialog procedure. Loading the
        ! integer list box with labels and values from gModIntParams so the user
        ! can edit them if so desired.
        subroutine loadIntItemsIntoListView(hList)
            use ModelParamsModule, only: gModIntParams
            use DialogHelpersModule, only: AddListViewRow
            use ifwinty, only: HANDLE
        
            integer(HANDLE), intent(in) :: hList
        
            call AddListViewRow(hList, "Current number of zonal waves", gModIntParams%KMAX2)
            call AddListViewRow(hList, "Current number of meridional waves", gModIntParams%LMAX2)
            call AddListViewRow(hList, "Current number levels", gModIntParams%NLEV)
            call AddListViewRow(hList, "Number of integration cycles", gModIntParams%ICYC)
            call AddListViewRow(hList, "Number of time steps", gModIntParams%ISTEP)
            call AddListViewRow(hList, "Number of time steps between display output", gModIntParams%outputInterval) 
            call AddListViewRow(hList, "Zonal wave for time series x-axis phase space", gModIntParams%zonalWavenumber)
            call AddListViewRow(hList, "Meridional wave for time series x-axis phase space", gModIntParams%meridWavenumber)
            call AddListViewRow(hList, "Level of time series and phase space plots", gModIntParams%LEVP)
            call AddListViewRow(hList, "Zonal wave for y-axis phase space component", gModIntParams%sinZonal)
            call AddListViewRow(hList, "Meridional wave for y-axis phase space", gModIntParams%sinMerid)
            call AddListViewRow(hList, "Lower bound of zonal waves to retain in maps", gModIntParams%JX0)
            call AddListViewRow(hList, "Lower bound of meridional waves to retain in maps", gModIntParams%JY0)
            call AddListViewRow(hList, "Upper bound of zonal waves to retain in maps", gModIntParams%JX1)
            call AddListViewRow(hList, "Upper bound of meridional waves to retain in maps", gModIntParams%JY1)
        end subroutine loadIntItemsIntoListView

        ! Just to simplify code parsing in the dialog procedure. The user has clicked on
        ! one of the real parameters and this method gets the real value and converts it
        ! to a string to load into the edit box. The routine that actually does the work
        ! is RealToAsciiToReal. All this does is pick the correct real parameter to convert.
        function getStringFromRealForEditBox(caseSelect) result(str)
            use ModelParamsModule, only: gModRealParams
            use ConstantsModule, only: REAL_TO_ASCII
            use UtilitiesModule, only: LENGTH, RealToAsciiToReal
            
            integer, intent(in) :: caseSelect

            character(len=LENGTH) :: str
            integer :: ios

            select case (caseSelect)
                case (0)
                    ios = RealToAsciiToReal(REAL_TO_ASCII, gModRealParams%RN, str)
                case (1) 
                    ios = RealToAsciiToReal(REAL_TO_ASCII, gModRealParams%BETA, str)
                case (2)
                    ios = RealToAsciiToReal(REAL_TO_ASCII, gModRealParams%DELT, str)
                case (3)
                    ios = RealToAsciiToReal(REAL_TO_ASCII, gModRealParams%field1Level, str)
                case (4)
                    ios = RealToAsciiToReal(REAL_TO_ASCII, gModRealParams%field2Level, str)
                case (5)
                    ios = RealToAsciiToReal(REAL_TO_ASCII, gModRealParams%field3Level, str)
                case (6)
                    ios = RealToAsciiToReal(REAL_TO_ASCII, gModRealParams%hovLat, str)
                case (7)
                    ios = RealToAsciiToReal(REAL_TO_ASCII, gModRealParams%hovLon, str)
                case (8)
                    ios = RealToAsciiToReal(REAL_TO_ASCII, gModRealParams%lonXPoint, str)
                case (9)
                    ios = RealToAsciiToReal(REAL_TO_ASCII, gModRealParams%latYPoint, str)
                case (10)
                    ios = RealToAsciiToReal(REAL_TO_ASCII, gModRealParams%BOT, str)
                case (11)
                    ios = RealToAsciiToReal(REAL_TO_ASCII, gModRealParams%TPP, str)
                case (12)
                    ios = RealToAsciiToReal(REAL_TO_ASCII, gModRealParams%CONV, str)
            end select
        end function getStringFromRealForEditBox

        ! Just to simplify code parsing in the dialog procedure. The user has clicked on
        ! one of the integer parameters and this method uses the handy SetDlgItemInt to
        ! directly load an integer value into an edit box. It does the int to string 
        ! conversion. We pass the parameter editId which is the ID of the edit box to
        ! avoid including the file Resource.fd
        function loadIntegerParameterIntoEditBox(hDlg, caseSelect, editId) result(bret)
            use ModelParamsModule, only: gModIntParams
            use user32
            
            integer(HANDLE), intent(in) :: hDlg
            integer, intent(in) :: caseSelect
            integer, intent(in) :: editId
            
            integer(BOOL) :: bret
            
            bret = .TRUE.

            select case (caseSelect)
                case (0)
                    bret = SetDlgItemInt(hDlg, editId, gModIntParams%KMAX2, .TRUE.)
                case (1)
                    bret = SetDlgItemInt(hDlg, editId, gModIntParams%LMAX2, .TRUE.)
                case (2)
                    bret = SetDlgItemInt(hDlg, editId, gModIntParams%NLEV, .TRUE.)
                case (3)
                    bret = SetDlgItemInt(hDlg, editId, gModIntParams%ICYC, .TRUE.)
                case (4)
                    bret = SetDlgItemInt(hDlg, editId, gModIntParams%ISTEP, .TRUE.)
                case (5)
                    bret = SetDlgItemInt(hDlg, editId, gModIntParams%outputInterval, .TRUE.)
                case (6)
                    bret = SetDlgItemInt(hDlg, editId, gModIntParams%zonalWavenumber, .TRUE.)
                case (7)
                    bret = SetDlgItemInt(hDlg, editId, gModIntParams%meridWavenumber, .TRUE.)
                case (8)
                    bret = SetDlgItemInt(hDlg, editId, gModIntParams%LEVP, .TRUE.)
                case (9)
                    bret = SetDlgItemInt(hDlg, editId, gModIntParams%sinZonal, .TRUE.)
                case (10)
                    bret = SetDlgItemInt(hDlg, editId, gModIntParams%sinMerid, .TRUE.)
                case (11)
                    bret = SetDlgItemInt(hDlg, editId, gModIntParams%JX0, .TRUE.)
                case (12)
                    bret = SetDlgItemInt(hDlg, editId, gModIntParams%JY0, .TRUE.)
                case (13)
                    bret = SetDlgItemInt(hDlg, editId, gModIntParams%JX1, .TRUE.)
                case (14)
                    bret = SetDlgItemInt(hDlg, editId, gModIntParams%JY1, .TRUE.)
            end select
        end function loadIntegerParameterIntoEditBox

        ! Just to simplify code parsing in the dialog procedure. The user has edit
        ! a real parameter and clicked on the 'DONE' button. This loads the edited
        ! value into the correct field of the gModRealParams type
        function readRealDataFromEditBox(str, rowClicked) result(ios)
            use ModelParamsModule, only: gModRealParams
            
            CHARACTER(:), ALLOCATABLE, intent(inout) :: str
            integer, intent(in) :: rowClicked

            integer :: ios

            ios = 0
            select case (rowClicked)
                case (0)
                    read(str, *, iostat = ios) gModRealParams%RN
                case (1) 
                    read(str, *, iostat = ios) gModRealParams%BETA
                case (2)
                    read(str, *, iostat = ios) gModRealParams%DELT
                case (3)
                    read(str, *, iostat = ios) gModRealParams%field1Level
                case (4)
                    read(str, *, iostat = ios)  gModRealParams%field2Level
                case (5)
                    read(str, *, iostat = ios)  gModRealParams%field3Level
                case (6)
                    read(str, *, iostat = ios)  gModRealParams%hovLat
                case (7)
                    read(str, *, iostat = ios)  gModRealParams%hovLon
                case (8)
                    read(str, *, iostat = ios)  gModRealParams%lonXPoint
                case (9)
                    read(str, *, iostat = ios)  gModRealParams%latYPoint
                case (10)
                    read(str, *, iostat = ios)  gModRealParams%BOT
                case (11)
                    read(str, *, iostat = ios)  gModRealParams%TPP
                case (12)
                    read(str, *, iostat = ios)  gModRealParams%CONV
            end select
        end function readRealDataFromEditBox

        ! Just to simplify code parsing in the dialog procedure. The user has edit
        ! a integer parameter and clicked on the 'DONE' button. This loads the edited
        ! value into the correct field of the modIntlParams type
        subroutine readIntDataFromEditBox(rowClicked, newValue)
            use ModelParamsModule, only: gModIntParams

            integer, intent(in) :: rowClicked, newValue
            
            select case (rowClicked)
                case (0)
                    gModIntParams%KMAX2 = newValue
                case (1)    
                    gModIntParams%LMAX2 = newValue
                case (2)
                    gModIntParams%NLEV = newValue
                case (3)
                    gModIntParams%ICYC = newValue
                case (4)
                    gModIntParams%ISTEP = newValue
                case (5)
                    gModIntParams%outputInterval = newValue
                case (6)
                    gModIntParams%zonalWavenumber = newValue
                case (7)
                    gModIntParams%meridWavenumber = newValue
                case (8)
                    gModIntParams%LEVP = newValue
                case (9)
                    gModIntParams%sinZonal = newValue
                case (10)
                    gModIntParams%sinMerid = newValue
                case (11)
                    gModIntParams%JX0 = newValue
                case (12)
                    gModIntParams%JY0 = newValue
                case (13)
                    gModIntParams%JX1 = newValue
                case (14)
                    gModIntParams%JY1 = newValue
            end select
        end subroutine readIntDataFromEditBox

        function reloadComboBoxFields(hDlg, plotType, comboId) result(bret)
            use ifwinty
            use user32      ! For SendMessage
            use ConstantsModule, only: HOV_DIAG_AT_LONG

            implicit none

            integer(HANDLE), intent(in) :: hDlg
            integer, intent(in) :: plotType
            integer, intent(in) :: comboId

            integer(BOOL) :: bret
            integer(HANDLE) :: hCombo

            ! Clear what is in the combo box
            bret = SendDlgItemMessage(hDlg, comboId, CB_RESETCONTENT, 0, 0)
            ! Get handle to combo box
            hCombo = GetDlgItem(hDlg, comboID)
            !if (plotType .LE. HOV_DIAG_AT_LONG) then
                ! Load with map options
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Vorticity in 10**-5/s'//char(0)))
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Height in meters'//char(0)))
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Temperature in Centigrade'//char(0)))
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Vertical wind in mb/hr or mm/sec'//char(0)))
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Temperature driving (forcing)'//char(0)))
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Total Height tendency in meters/day'//char(0)))
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Height tendencies from Advection + beta'//char(0)))
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Height tendencies from omega (dp/dt)'//char(0)))
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Height tendencies from Diabatic effects'//char(0)))
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Total temperture tendencies in C/day'//char(0)))
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Temperature tendencies from advection'//char(0)))
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Temperature tendencies from omega'//char(0)))
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Temperature tendencies from diabatic effects'//char(0)))
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Mean streamfunction (height) forcing in m'//char(0)))
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Height tendencies from ororgraphy in m/day'//char(0)))
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Bottom orography in meters'//char(0)))
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Plotting off'//char(0)))
            !else
                ! Load with time series options
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Total Height tendencies in m/day'//char(0)))
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Total temperature tendencies in C/day'//char(0)))
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Mean height tendencies in m/day'//char(0)))
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Height wave ground speed in m/sec'//char(0)))
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Phase diff in deg between height and temperature wave'//char(0)))
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Growth rate of a wave in e-folds per day'//char(0)))
                bret = SendMessage(hCombo, CB_ADDSTRING, 0, loc('Amplitude ratio of temperature to Strmfnct of a wave'//char(0)))
            !end if
        end function reloadComboBoxFields

end module ModelControlDialogModule