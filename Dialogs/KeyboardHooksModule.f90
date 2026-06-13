module KeyboardHooksModule
    
    ! This module contains the keyboard hooks. Unfortunately dialogs take control of all keypresses
    ! and the application cannot respond to keyboard actions. We present to the user both buttons that
    ! can be clicked OR a keyboard press to do the same thing. So in order to get a keypress we have
    ! to create a keyboard hook as follows:
    !       hHook = SetWindowsHookEx(WH_KEYBOARD, LOC(KeyboardForDlgSettingsProc), NULL, GetCurrentThreadId())
    ! This function is called in the WM_CREATE or WM_INITDIALOG message. 
    !
    ! The SetWindowHookEx has a callback procedure which gets the keypress. These callbacks are defined in this
    ! module. There is one for each dialog which needs to handle keypresses.
    ! 
    ! Since these keyboard hooks are nested as the dialogs are nested we have to make sure that the callback ignores keys
    ! if the dialog for which it is handling is not the foreground window. This is done by checking
    !       IF (GetForegroundWindow() .EQ. dialogHandle) 
    ! is true, where the dialog handle is the handle of the dialog of interest. That means we need to define a global
    ! handle value for every dialog where we wish to handle keypresses. That means we set the global when the window
    ! or dialog is initialized and that we invalidate it when the dialog/window closes.
    ! When the procdure in which the hook is intiated closes, the hook is cleared using the 
    !       bret = UnhookWindowsHookEx(hHook)
    ! which would be wherever there is a dialog close type of message or wherever the EndDialog is called.
    !
    ! Thus in this application  we have something like the following:
    !    In the SettingsDlgProc WM_INITDIALOG handler we invoke the keyboard hook for the SettingsDialog with callback KeyboardForDlgSettingsProc
    !    In the ModelControlDlgProc we create the hook for the ModelControl dialog in the WM_INITDIALOG message
    ! For example:
    ! hHook = SetWindowsHookEx(WH_KEYBOARD, LOC(KeyboardForDlgSettingsProc), NULL, GetCurrentThreadId()) and set  ghSettingsDlg = hDlg

    ! Wish this wasnt so damn complicated.

    public :: paramChange, ParameterChange
    public :: KeyboardForDlgSettingsProc, KeyboardForDlgModelControlProc
    
    type :: ParameterChange       ! Structure used to detect user parameter changes that might require a model re-initialization
        integer :: zonalWaves
        integer :: meridWaves
        integer :: levels
        real :: RN
    end type ParameterChange

    type(ParameterChange) :: paramChange

    contains
        function KeyboardForDlgSettingsProc(nCode, wParam, lParam)
        !DEC$ IF DEFINED(_X86_)
        !DEC$ ATTRIBUTES STDCALL, ALIAS : '_KeyboardForDlgSettingsProc@16' :: KeyboardForDlgSettingsProc
        !DEC$ ELSE
        !DEC$ ATTRIBUTES STDCALL, ALIAS : 'KeyboardForDlgSettingsProc' :: KeyboardForDlgSettingsProc
        !DEC$ ENDIF
            USE IFWIN
            use user32
            use NLevQGModelGlobals, only: ghInstance, ghSettingsDlg, ghModelControlDlg, ghContoursAndRangesDlg, LogMessage
            use DlgProcInterfacesModule, only: ModelControlDlgProc, ContoursAndRangesDlgProc
            use DialogHelpersModule, only: GetNameOfUserSelectedFile, GetUserSelectedSaveName
            use UtilitiesModule, only: openModelFile
            use SetupModule, only: SaveModelState, ReadSaveFile
            use ModelParamsModule, only: gNLevFile
            
            implicit none

            include '../resource.fd'

            INTEGER(kind=4), INTENT(IN) :: nCode
            integer(UINT_PTR), intent(in) :: wParam
            integer(LONG_PTR), intent(in) :: lParam

            integer(LRESULT) ::  KeyboardForDlgSettingsProc ! LRESULT
            integer(kind=4) :: iret
            logical :: isDialogOpen = .false.
            logical :: res
            character(len=270) :: selected_file
            integer(BOOL) ::   bret
            integer(HANDLE) :: hFocus, hLB_User

            IF (GetForegroundWindow() .EQ. ghSettingsDlg) then

                IF (nCode >= 0) THEN
                    IF (wParam == VK_F1 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only F1 key up; lParam bit 31 set
                        ! F1: Configure colors and set map contour intervals
                        call LogMessage('User pressed F1 key in Settings Dialog')
                        bret = DialogBoxParam(ghInstance, INT(IDD_CONTOURS, LONG_PTR), ghContoursAndRangesDlg, LOC(ContoursAndRangesDlgProc), 0_LONG_PTR)
                        KeyboardForDlgSettingsProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F2 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F2: Load data from a saved file
                        call LogMessage('User pressed F2 key in Settings Dialog')
                        res = GetNameOfUserSelectedFile(ghSettingsDlg, gNLevFile)
                        if (res) then
                            if (len_trim(gNLevFile%fileName) == 0) return
                            call LogMessage("Save file loaded: "// selected_file)
                            iret = openModelFile(gNLevFile, .true., ghSettingsDlg)
                            if (iret == 0) iret = ReadSaveFile(gNLevFile)
                        endif
                        KeyboardForDlgSettingsProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F3 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F3: Save data to a file
                        call LogMessage('User pressed F3 key in Settings Dialog')
                        res = GetUserSelectedSaveName(ghSettingsDlg, gNLevFile)
                        if (res) then
                            iret = openModelFile(gNLevFile, .false., ghSettingsDlg)
                            if (iret == 0) iret = SaveModelState(gNLevFile, .false.)
                        endif
                        KeyboardForDlgSettingsProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F4 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        call LogMessage('User pressed F4 key in Settings Dialog')
                        bret = EnableWindow(ghSettingsDlg, .FALSE.)
                        bret = EnableWindow(ghSettingsDlg, .FALSE.)
                        bret = ShowWindow(ghSettingsDlg, SW_HIDE)
                        ghModelControlDlg = CreateDialogParam(ghInstance, INT(IDD_MODEL_CONTROL, LONG_PTR), &
                                    NULL, & !ghwndMain,  Make the dialog unowned so the main window can come in the foreground
                                    LOC(ModelControlDlgProc), 0_LONG_PTR)
                        if (ghModelControlDlg /= 0) then
                            bret = ShowWindow(ghModelControlDlg, SW_SHOW)
                            bret = UpdateWindow(ghModelControlDlg) ! Optional: forces an immediate paint
                        end if
                        KeyboardForDlgSettingsProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F5 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F5: Go to Sensitivity file menu
                        call LogMessage('User pressed F5 key in Settings Dialog')
                        KeyboardForDlgSettingsProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F6 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F6: Generic help
                        call LogMessage('User pressed F6 key in Settings Dialog')
                        KeyboardForDlgSettingsProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F10 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F10: Return to main menu
                        call LogMessage('User pressed F10 key in Settings Dialog')
                        bret = PostMessage(ghSettingsDlg, WM_CLOSE, 0, 0)
                        KeyboardForDlgSettingsProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_RETURN) THEN
                        ! Trap the Enter key: return non-zero to prevent default processing
                        call LogMessage('User pressed Enter key, trapped by hook')
                        ! Get the handle of the window that currently has focus
                        ! If it is one of the list boxes, we are going to programmatically
                        ! click on the top row.
                        hFocus = GetFocus() ! currently focused control
                        hLB_User = GetDlgItem(ghSettingsDlg, IDC_EDIT_USERSETTINGS)    ! User settings EDIT box handle
                        IF (hFocus == hLB_User) THEN
                            ! If the edit box is focused, set the focus on the correct list box row
                            hFocus = GetDlgItem(ghSettingsDlg, IDC_USERSETTINGS)
                            bret = SetFocus(hFocus)
                        END IF
                        KeyboardForDlgSettingsProc = 1  ! Swallow the key
                        RETURN
                    END IF
                END IF
            END IF
            ! Pass to next hook in chain
            KeyboardForDlgSettingsProc = CallNextHookEx(0, nCode, wParam, lParam)
        END FUNCTION KeyboardForDlgSettingsProc

        function KeyboardForDlgModelControlProc(nCode, wParam, lParam) ! This call signature is fixed by win32 API and cannot be changed.
        !DEC$ IF DEFINED(_X86_)
        !DEC$ ATTRIBUTES STDCALL, ALIAS : '_KeyboardForDlgModelControlProc@16' :: KeyboardForDlgModelControlProc
        !DEC$ ELSE
        !DEC$ ATTRIBUTES STDCALL, ALIAS : 'KeyboardForDlgModelControlProc' :: KeyboardForDlgModelControlProc
        !DEC$ ENDIF
            USE IFWIN
            use user32
            use NLevQGModelGlobals, only: ghInstance, ghModelControlDlg, ghVerticalParamsDlg, ghInitVariablesDlg, ghContoursAndRangesDlg, LogMessage
            use SetUpModule, only: CheckModelControlChanges, SaveModelState, ReadSaveFile, DimensionalizeModRealParams
            use ModelModule, only: InitModelWithModelControlVals, StartTimeLoop
            use ModelParamsModule, only: gModIntParams, gModRealParams, gNLevFile
            use DialogHelpersModule, only: HideMyEdit, GetNameOfUserSelectedFile, GetUserSelectedSaveName
            use PlotModule, only: ClearScreen
            use DlgProcInterfacesModule, only: VerticalParamsDlgProc, InitVariablesDlgProc, ContoursAndRangesDlgProc
            use UtilitiesModule, only: openModelFile

            implicit none

            include '../resource.fd'

            INTEGER(kind=4), INTENT(IN) :: nCode
            integer(UINT_PTR), intent(in) :: wParam
            integer(LONG_PTR), intent(in) :: lParam

            integer(LRESULT) ::  KeyboardForDlgModelControlProc ! LRESULT
            integer(kind=4) :: iret
            integer(LONG_PTR) :: longInteger
            !logical :: isDialogOpen = .false.
            integer(BOOL) ::   bret
            integer(HANDLE) :: hFocus, hLB_real, hLB_integer

            logical :: res
            character(len=270) :: selected_file

            IF (GetForegroundWindow() .EQ. ghModelControlDlg) then

                IF (nCode >= 0) THEN
                    IF (wParam == VK_F1 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F1: Run model
                        call LogMessage('User pressed F1 key in Model Control Dialog')
                        bret = PostMessage(ghModelControlDlg, WM_APP + 200, 0, 0) 
                        KeyboardForDlgModelControlProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F2 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F2: Set Vertical parameters
                        call LogMessage('User pressed F2 key in Model Control Dialog')
            !TODO: We need to move this to the main dialog to handle errors and update fields.
                        iret = CheckModelControlChanges( paramChange%zonalWaves, paramChange%meridWaves, paramChange%levels, paramChange%RN)
                        iret = InitModelWithModelControlVals()
                        bret = DialogBoxParam(ghInstance, INT(IDD_VERTICAL_PARAMS, LONG_PTR), ghVerticalParamsDlg, LOC(VerticalParamsDlgProc), 0_LONG_PTR)
                        paramChange%zonalWaves = gModIntParams%KMAX2   ! Save the current number of zonal waves
                        paramChange%meridWaves = gModIntParams%LMAX2   ! Save the current number of meridional waves
                        paramChange%levels     = gModIntParams%NLEV    ! Save the current number of model levels
                        paramChange%RN         = gModRealParams%RN     ! Save the current channel length. A change will alter the interaction coefficients
                        call DimensionalizeModRealParams() 
                        KeyboardForDlgModelControlProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F3 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F3: Initialize flow variables
                        call LogMessage('User pressed F3 key in Model Control Dialog')
            !TODO: We need to move this to the main dialog to handle errors and update fields.
                        iret = CheckModelControlChanges( paramChange%zonalWaves, paramChange%meridWaves, paramChange%levels, paramChange%RN)
                        iret = InitModelWithModelControlVals()
                        bret = DialogBoxParam(ghInstance, INT(IDD_INIT_VARIABLES, LONG_PTR), ghInitVariablesDlg, LOC(InitVariablesDlgProc), 0_LONG_PTR)
                        paramChange%zonalWaves = gModIntParams%KMAX2   ! Save the current number of zonal waves
                        paramChange%meridWaves = gModIntParams%LMAX2   ! Save the current number of meridional waves
                        paramChange%levels     = gModIntParams%NLEV    ! Save the current number of model levels
                        paramChange%RN         = gModRealParams%RN     ! Save the current channel length. A change will alter the interaction coefficients
                        call DimensionalizeModRealParams() 
                        KeyboardForDlgModelControlProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F4 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F4: Save the current flow
                        call LogMessage('User pressed F4 key in Model Control Dialog')
                        res = GetNameOfUserSelectedFile(ghModelControlDlg, gNLevFile)
                        if (res) then
                            if (len_trim(gNLevFile%fileName) == 0) return
                            call LogMessage("Save file loaded: "// selected_file)
                            iret = openModelFile(gNLevFile, .true., ghModelControlDlg)
                            if (iret == 0) iret = ReadSaveFile(gNLevFile)
                        endif
                        KeyboardForDlgModelControlProc = 1  ! Swallow the key
                        RETURN
                    END IF
                
                    IF (wParam == VK_F5 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F5: Load saved flow
                        call LogMessage('User pressed F5 key in Model Control Dialog')
                        res = GetUserSelectedSaveName(ghModelControlDlg, gNLevFile)
                        if (res) then
                            iret = openModelFile(gNLevFile, .false., ghModelControlDlg)
                            if (iret == 0) iret = SaveModelState(gNLevFile, .false.)
                        endif
                        KeyboardForDlgModelControlProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F6 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F6: View statistics
                        call LogMessage('User pressed F6 key in Model Control Dialog')
                        KeyboardForDlgModelControlProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F7 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F7: Set map contour intervals and graph ranges
                        call LogMessage('User pressed F7 key in Model Control Dialog')
                        bret = DialogBoxParam(ghInstance, INT(IDD_CONTOURS, LONG_PTR), ghContoursAndRangesDlg, LOC(ContoursAndRangesDlgProc), 0_LONG_PTR)
                        KeyboardForDlgModelControlProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F8 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F8: Clear Screen
                        call LogMessage('User pressed F8 key in Model Control Dialog')
                        call ClearScreen()
                        KeyboardForDlgModelControlProc = 1  ! Swallow the key
                        RETURN
                    END IF


                    IF (wParam == VK_F9 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F9: Set up 'bomb' disturbance
                        call LogMessage('User pressed F9 key in Model Control Dialog')
                        KeyboardForDlgModelControlProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F10 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F10: Return to main menu
                        call LogMessage('User pressed F10 key in Model Control Dialog')
                        bret = PostMessage(ghModelControlDlg, WM_CLOSE, 0, 0)
                        KeyboardForDlgModelControlProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_RETURN) THEN
                        ! Trap the Enter key: return non-zero to prevent default processing
                        call LogMessage('User pressed Enter key, trapped by hook')
                        ! Get the handle of the window that currently has focus
                        ! If it is one of the list boxes, we are going to programmatically
                        ! click on the top row.
                        hFocus = GetFocus() ! currently focused control
                        hLB_Real = GetDlgItem(ghModelControlDlg, IDC_EDIT_REALVALUES)      ! Real values EDIT box handle
                        hLB_Integer = GetDlgItem(ghModelControlDlg, IDC_EDIT_INTEGERVALUES)    ! Integer values EDIT box handle
                        IF (hFocus == hLB_Real .OR. hFocus == hLB_Integer) THEN
                            ! If one of the edit boxes is focused, set the focus on the correct list box
                            if (hFocus == hLB_Real) hFocus = GetDlgItem(ghModelControlDlg, IDC_REALVALUES)
                            if (hFocus == hLB_Integer) hFocus = GetDlgItem(ghModelControlDlg, IDC_INTEGERVALUES)
                            bret = SetFocus(hFocus)
                            call HideMyEdit(GetDlgItem(ghModelControlDlg, IDC_EDIT_REALVALUES))
                        END IF
                        KeyboardForDlgModelControlProc = 1  ! Swallow the key
                        RETURN
                    END IF

                END IF
            end IF
            ! Pass to next hook in chain
            KeyboardForDlgModelControlProc = CallNextHookEx(0, nCode, wParam, lParam)
        END FUNCTION KeyboardForDlgModelControlProc
        
        function KeyboardForDlgVerticalParamsProc(nCode, wParam, lParam)
            !DEC$ IF DEFINED(_X86_)
            !DEC$ ATTRIBUTES STDCALL, ALIAS : '_KeyboardForDlgVerticalParamsProc@16' :: KeyboardForDlgVerticalParamsProc
            !DEC$ ELSE
            !DEC$ ATTRIBUTES STDCALL, ALIAS : 'KeyboardForDlgVerticalParamsProc' :: KeyboardForDlgVerticalParamsProc
            !DEC$ ENDIF
            USE IFWIN
            use user32
            use NLevQGModelGlobals, only: ghVerticalParamsDlg, LogMessage

            implicit none

            include '../resource.fd'

            INTEGER(kind=4), INTENT(IN) :: nCode
            integer(UINT_PTR), intent(in) :: wParam
            integer(LONG_PTR), intent(in) :: lParam

            integer(LRESULT) ::  KeyboardForDlgVerticalParamsProc ! LRESULT
            integer(kind=4) :: iret
            integer(BOOL) ::   bret
            integer(HANDLE) :: hFocus, hLB_User

            IF (GetForegroundWindow() .EQ. ghVerticalParamsDlg) then

                IF (nCode >= 0) THEN
                    IF (wParam == VK_F1 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only F1 key up; lParam bit 31 set
                        ! F1: Configure colors and set map contour intervals
                        call LogMessage('User pressed F1 key in VerticalParams Dialog')
                        KeyboardForDlgVerticalParamsProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F2 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F2: Load data from a saved file
                        call LogMessage('User pressed F2 key in VerticalParams Dialog')
                        KeyboardForDlgVerticalParamsProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F3 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F3: Save data to a file
                        call LogMessage('User pressed F3 key in VerticalParams Dialog')
                        KeyboardForDlgVerticalParamsProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F4 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        call LogMessage('User pressed F4 key in VerticalParams Dialog')
                        KeyboardForDlgVerticalParamsProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F5 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F5: Go to Sensitivity file menu
                        call LogMessage('User pressed F5 key in VerticalParams Dialog')
                        KeyboardForDlgVerticalParamsProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F6 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F6: Generic help
                        call LogMessage('User pressed F6 key in VerticalParams Dialog')
                        KeyboardForDlgVerticalParamsProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F10 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F10: Return to main menu
                        call LogMessage('User pressed F10 key in VerticalParams Dialog')
                        bret = PostMessage(ghVerticalParamsDlg, WM_CLOSE, 0, 0)
                        KeyboardForDlgVerticalParamsProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_RETURN) THEN
                        ! Trap the Enter key: return non-zero to prevent default processing
                        call LogMessage('User pressed Enter key, trapped by hook')
                        ! Get the handle of the window that currently has focus
                        ! If it is one of the list boxes, we are going to programmatically
                        ! click on the top row.
                        hFocus = GetFocus() ! currently focused control
                        hLB_User = GetDlgItem(ghVerticalParamsDlg, IDC_EDIT_VERTICAL)    ! Vertical levels EDIT box handle
                        IF (hFocus == hLB_User) THEN
                            bret = PostMessage(ghVerticalParamsDlg, WM_APP + 400, 0, 0) ! Hides edit box if it is in focus
                        END IF
                        KeyboardForDlgVerticalParamsProc = 1  ! Swallow the key
                        RETURN
                    END IF
                END IF
            END IF
            ! Pass to next hook in chain
            KeyboardForDlgVerticalParamsProc = CallNextHookEx(0, nCode, wParam, lParam)
        END FUNCTION KeyboardForDlgVerticalParamsProc
        
        function KeyboardForDlgInitVariablesProc(nCode, wParam, lParam)
            !DEC$ IF DEFINED(_X86_)
            !DEC$ ATTRIBUTES STDCALL, ALIAS : '_KeyboardForDlgInitVariablesProc@16' :: KeyboardForDlgInitVariablesProc
            !DEC$ ELSE
            !DEC$ ATTRIBUTES STDCALL, ALIAS : 'KeyboardForDlgInitVariablesProc' :: KeyboardForDlgInitVariablesProc
            !DEC$ ENDIF
            USE IFWIN
            use user32
            use NLevQGModelGlobals, only: ghInitVariablesDlg, LogMessage
            use ModelFieldsModule, only: gHorzArr
            
            implicit none

            include '../resource.fd'

            INTEGER(kind=4), INTENT(IN) :: nCode
            integer(UINT_PTR), intent(in) :: wParam
            integer(LONG_PTR), intent(in) :: lParam

            integer(LRESULT) ::  KeyboardForDlgInitVariablesProc ! LRESULT
            integer(kind=4) :: iret
            integer(BOOL) ::   bret
            integer(HANDLE) :: hFocus, hLB_User

            IF (GetForegroundWindow() .EQ. ghInitVariablesDlg) then

                IF (nCode >= 0) THEN
                    IF (wParam == VK_F1 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only F1 key up; lParam bit 31 set
                        ! F1: Zero variables
                        call LogMessage('User pressed F1 key in InitVariables Dialog')
                        bret = PostMessage(ghInitVariablesDlg, WM_APP + 301, 0, 0) ! Calls ZeroVariables() and then repopulates the list controls
                        KeyboardForDlgInitVariablesProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F2 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F2: Load data from a saved file
                        call LogMessage('User pressed F2 key in InitVariables Dialog')
                        KeyboardForDlgInitVariablesProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F3 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F3: Save data to a file
                        call LogMessage('User pressed F3 key in InitVariables Dialog')
                        KeyboardForDlgInitVariablesProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F4 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        call LogMessage('User pressed F4 key in InitVariables Dialog')
                        KeyboardForDlgInitVariablesProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F5 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F5: Go to Sensitivity file menu
                        call LogMessage('User pressed F5 key in InitVariables Dialog')
                        KeyboardForDlgInitVariablesProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F6 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F6: Generic help
                        call LogMessage('User pressed F6 key in InitVariables Dialog')
                        KeyboardForDlgInitVariablesProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F10 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F10: Return to main menu
                        call LogMessage('User pressed F10 key in InitVariables Dialog')
                        bret = PostMessage(ghInitVariablesDlg, WM_CLOSE, 0, 0)
                        KeyboardForDlgInitVariablesProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_RETURN) THEN
                        ! Trap the Enter key: return non-zero to prevent default processing
                        call LogMessage('User pressed Enter key, trapped by hook')
                        ! Get the handle of the window that currently has focus
                        ! If it is one of the list boxes, we are going to programmatically
                        ! click on the top row.
                        hFocus = GetFocus() ! currently focused control
                        hLB_User = GetDlgItem(ghInitVariablesDlg, IDC_EDIT_VERTICAL)    ! Vertical levels EDIT box handle
                        IF (hFocus == hLB_User) THEN
                            bret = PostMessage(ghInitVariablesDlg, WM_APP + 300, 0, 0) ! Hides edit box if it is in focus
                        END IF
                        KeyboardForDlgInitVariablesProc = 1  ! Swallow the key
                        RETURN
                    END IF
                END IF
            END IF
            ! Pass to next hook in chain
            KeyboardForDlgInitVariablesProc = CallNextHookEx(0, nCode, wParam, lParam)
        END FUNCTION KeyboardForDlgInitVariablesProc
        
        function KeyboardForDlgContoursAndRangesProc(nCode, wParam, lParam)
            !DEC$ IF DEFINED(_X86_)
            !DEC$ ATTRIBUTES STDCALL, ALIAS : '_KeyboardForDlgContoursAndRangesProc@16' :: KeyboardForDlgContoursAndRangesProc
            !DEC$ ELSE
            !DEC$ ATTRIBUTES STDCALL, ALIAS : 'KeyboardForDlgContoursAndRangesProc' :: KeyboardForDlgContoursAndRangesProc
            !DEC$ ENDIF
            USE IFWIN
            use user32
            use NLevQGModelGlobals, only: ghContoursAndRangesDlg, LogMessage
            use ModelParamsModule, only: gContourIntervals
            
            implicit none

            include '../resource.fd'

            INTEGER(kind=4), INTENT(IN) :: nCode
            integer(UINT_PTR), intent(in) :: wParam
            integer(LONG_PTR), intent(in) :: lParam

            integer(LRESULT) ::  KeyboardForDlgContoursAndRangesProc ! LRESULT
            integer(kind=4) :: iret
            integer(BOOL) ::   bret
            integer(HANDLE) :: hFocus, hLB_User

            IF (GetForegroundWindow() .EQ. ghContoursAndRangesDlg) then

                IF (nCode >= 0) THEN
                    IF (wParam == VK_F1 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only F1 key up; lParam bit 31 set
                        ! F1: Zero variables
                        call LogMessage('User pressed F1 key in ContoursAndRanges Dialog')
                        bret = PostMessage(ghContoursAndRangesDlg, WM_APP + 301, 0, 0) ! Calls ZeroVariables() and then repopulates the list controls
                        KeyboardForDlgContoursAndRangesProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F2 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F2: Load data from a saved file
                        call LogMessage('User pressed F2 key in ContoursAndRanges Dialog')
                        KeyboardForDlgContoursAndRangesProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F3 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F3: Save data to a file
                        call LogMessage('User pressed F3 key in ContoursAndRanges Dialog')
                        KeyboardForDlgContoursAndRangesProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F4 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        call LogMessage('User pressed F4 key in ContoursAndRanges Dialog')
                        KeyboardForDlgContoursAndRangesProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F5 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F5: Go to Sensitivity file menu
                        call LogMessage('User pressed F5 key in ContoursAndRanges Dialog')
                        KeyboardForDlgContoursAndRangesProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F6 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F6: Generic help
                        call LogMessage('User pressed F6 key in ContoursAndRanges Dialog')
                        KeyboardForDlgContoursAndRangesProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_F10 .AND. IAND(lParam, Z'80000000') /= 0) THEN ! Respond to only key up; lParam bit 31 set
                        ! F10: Return to main menu
                        call LogMessage('User pressed F10 key in ContoursAndRanges Dialog')
                        bret = PostMessage(ghContoursAndRangesDlg, WM_CLOSE, 0, 0)
                        KeyboardForDlgContoursAndRangesProc = 1  ! Swallow the key
                        RETURN
                    END IF

                    IF (wParam == VK_RETURN) THEN
                        ! Trap the Enter key: return non-zero to prevent default processing
                        call LogMessage('User pressed Enter key, trapped by hook')
                        ! Get the handle of the window that currently has focus
                        ! If it is one of the list boxes, we are going to programmatically
                        ! click on the top row.
                        hFocus = GetFocus() ! currently focused control
                        hLB_User = GetDlgItem(ghContoursAndRangesDlg, IDC_EDIT_VERTICAL)    ! Vertical levels EDIT box handle
                        IF (hFocus == hLB_User) THEN
                            bret = PostMessage(ghContoursAndRangesDlg, WM_APP + 300, 0, 0) ! Hides edit box if it is in focus
                        END IF
                        KeyboardForDlgContoursAndRangesProc = 1  ! Swallow the key
                        RETURN
                    END IF
                END IF
            END IF
            ! Pass to next hook in chain
            KeyboardForDlgContoursAndRangesProc = CallNextHookEx(0, nCode, wParam, lParam)
        END FUNCTION KeyboardForDlgContoursAndRangesProc

end module KeyboardHooksModule