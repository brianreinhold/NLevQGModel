!DEC$ OBJCOMMENT LIB:'shell32.lib'
! Nessecary ugliness for the SHCreateDirectoryEx function used to create the ensemble directory. Somehow this
! tells the compiler where to find the object code for the function. Who could of ever figured that out?
module BombDialogModule
    implicit none
    
    public BombDlgProc
    
    contains

        function BombDlgProc(hDlg, uMsg, wParam, lParam)
            !! DO NOT FORGET TO UPDATE THESE CALL PROCEDURES !!
            !DEC$ IF DEFINED(_X86_)
            !DEC$ ATTRIBUTES STDCALL, ALIAS : '_BombDlgProc@16' :: BombDlgProc
            !DEC$ ELSE
            !DEC$ ATTRIBUTES STDCALL, ALIAS : 'BombDlgProc' :: BombDlgProc
            !DEC$ ENDIF
            use ifwinty
            use ifwin
            use kernel32
            use user32
            use gdi32

            use NLevQGModelGlobals, only: CenterWindow, ghBombDlg, ghCurrentDlg, ghInstance, LogMessage
            use iso_c_binding

            use ModelParamsModule, only: gModIntParams, gModRealParams
            use ModelFieldsModule, only: gHorzArr
            use SetupModule, only: CreateBomb, NonDimensionalizeHorizontalParams, DimensionalizeHorizontalParams

            use KeyboardHooksModule, only: KeyboardForDlgBombProc

            implicit none

            integer(HANDLE), intent(in) :: hDlg
            integer(UINT), intent(in)   :: uMsg
            integer(UINT_PTR) :: wParam
            integer(LONG_PTR) :: lParam
            integer(LRESULT) :: BombDlgProc

            integer(HANDLE) :: hHook
            save hHook

            include '../resource.fd'

            ! Local variables
            integer(kind=4) :: ret
            integer(BOOL) ::   bret
            logical, save :: isInitialized ! Saftey check for the dangerous KILL_FOCUS messages sent by SetFocus in WM_INITDIALOG
            logical :: boolResp
            character(len=16) :: textFromReal

            type(T_RECT) :: rc
            type(T_POINT) :: pt

            ! For the edit boxes
            integer(HANDLE) :: hEdit

            integer :: i, controlId, notifyCode, ios
            real :: angs
            LOGICAL :: saveDimensionalState

            BombDlgProc = 0

            select case (uMsg)

            case (WM_INITDIALOG)
                    isInitialized = .false.

                    hHook = SetWindowsHookEx(WH_KEYBOARD, LOC(KeyboardForDlgBombProc), NULL, GetCurrentThreadId())
                    ghBombDlg = hDlg
                    
                    if (gModIntParams%timeStepsToDelay < 0) then
                        gModIntParams%timeStepsToDelay = -gModIntParams%timeStepsToDelay
                        bret = MessageBox (hDlg, &
                            "Bomb (perturbation) option has been reset."C, &
                            "Reset Done"C, &
                            IOR(MB_SYSTEMMODAL, IOR(MB_OK, MB_ICONINFORMATION)))
                    endif
                    if (gModIntParams%timeStepsToDelay .NE. 1) then
                        bret = ShowWindow(GetDlgItem(hDlg, IDC_BUTTON_BOMB_CREATE), SW_HIDE)
                    else
                        bret = ShowWindow(GetDlgItem(hDlg, IDC_BUTTON_BOMB_CREATE), SW_SHOW)
                    endif

                    
                    block
                        character(len=*), parameter :: INSTRUCTIONS = &
                            "There are several options."// char(13) // char(10) // &
                            "1. If the 'Time steps to delay' is set to 0 no bomb will be created; a normal undisturbed run."// char(13) // char(10) // &
                            "   The F1 button will not work in this case." // char(13) // char(10) // &
                            "2. If set to 1 the perturbations will be run on the first time step. In this case the user can"// char(13) // char(10) // &
                            "   create several superposed disturbances of different scale by setting up one, clicking on the"// char(13) // char(10) // &
                            "   F1 button, create another, click in F1, etc. F1 BUTTON MUST BE CLICKED IN THIS CASE!"// char(13) // char(10) // &
                            "3. If greater than 1 a single bomb will be created at the delayed time. In this case the F1"// char(13) // char(10) // &
                            "   button will not work. The bomb will be created at the appropriate time."// char(13) // char(10) // &
                            "To assure that bombs are not created every restart, once created during a run, the user needs"// char(13) // char(10) // &
                            "to return to this menu to reset the bomb."//char(0)
                            bret = SetDlgItemText(hDlg, IDC_EDIT_BOMB_READ_ONLY, INSTRUCTIONS)
                    end block

                    ! Center the dialog over the application window
                    call CenterWindow (hDlg, GetWindow (hDlg, GW_OWNER))
                    ret = SetFocus(hDlg)


                    ret = SetDlgItemInt(hDlg, IDC_EDIT_BOMB_ZONAL_WAVE, gModIntParams%bombZonalWave, .false.)
                    ret = SetDlgItemInt(hDlg, IDC_EDIT_BOMB_MERID_WAVE, gModIntParams%bombMeridWave, .false.)
                    ret = SetDlgItemInt(hDlg, IDC_EDIT_BOMB_TIMESTEPS_DELAY, gModIntParams%timeStepsToDelay, .false.)  ! false = Unsigned
                    
                    write(textFromReal, '(F10.4)', iostat=ios) gModRealParams%AMPS
                    if (ios == 0) ret = SetDlgItemText(hDlg, IDC_EDIT_BOMB_STRM_AMP, adjustl(trim(textFromReal)) // char(0))
                    
                    write(textFromReal, '(F10.4)', iostat=ios) gModRealParams%AMPT
                    if (ios == 0) ret = SetDlgItemText(hDlg, IDC_EDIT_BOMB_TEMP_AMP, adjustl(trim(textFromReal)) // char(0))
                    
                    write(textFromReal, '(F10.4)', iostat=ios) gModRealParams%ANGS
                    if (ios == 0) ret = SetDlgItemText(hDlg, IDC_EDIT_BOMB_PHASE_DIFF, adjustl(trim(textFromReal)) // char(0))

                    write(textFromReal, '(F10.4)', iostat=ios) gModRealParams%EFOLD
                    if (ios == 0) ret = SetDlgItemText(hDlg, IDC_EDIT_BOMB_EXPONENT, adjustl(trim(textFromReal)) // char(0))

                    isInitialized = .true.
                    BombDlgProc = 0

                case (WM_COMMAND)
                    ! LOWORD(wParam) = Control ID
                    ! HIWORD(wParam) = Notification Code
                    notifyCode = ISHFT(wParam, -16)
                    controlId  = IAND(wParam, Z'FFFF')
                    select case (controlId)

                        ! This loads the edited values into their respective variables when the user hits enter or clicks
                        ! out of the edit box. We have to be careful to only handle the EN_KILLFOCUS notification for the edit boxes since
                        ! there are other notifications like EN_CHANGE that we want to ignore. We also want to ignore all notifications from
                        ! the edit boxes until the dialog is initialized since SetFocus in the init dialog will send killfocus notifications
                        ! for each control as it sets the focus to them.
                        case (IDC_EDIT_BOMB_ZONAL_WAVE, IDC_EDIT_BOMB_MERID_WAVE, IDC_EDIT_BOMB_TIMESTEPS_DELAY, IDC_EDIT_BOMB_STRM_AMP, &
                                    IDC_EDIT_BOMB_TEMP_AMP, IDC_EDIT_BOMB_PHASE_DIFF,  IDC_EDIT_BOMB_EXPONENT)
                            if (.not. isInitialized) return  ! Do not handle messages from SetFocus() called in init dialog
                                                             ! It will send a killfocus for each control as it goes through the various controls
                                                             ! Otherwise, since the edit box is empty it gets reallocated with no size.
                            ! If it was the edit box, only proceed if it's losing focus (EN_KILLFOCUS)
                            ! If it was IDOK, we proceed regardless.
                            ! If it's an edit box, we only care about it when the user is FINISHED (Kill Focus)
                            if  (IAND(wParam, Z'FFFF') == IDC_EDIT_BOMB_TIMESTEPS_DELAY) then
                                gModIntParams%timeStepsToDelay = GetDlgItemInt(hDlg, IDC_EDIT_BOMB_TIMESTEPS_DELAY, LOC(bret), .TRUE.)
                                if (gModIntParams%timeStepsToDelay .NE. 1) then
                                    bret = ShowWindow(GetDlgItem(hDlg, IDC_BUTTON_BOMB_CREATE), SW_HIDE)
                                else
                                    bret = ShowWindow(GetDlgItem(hDlg, IDC_BUTTON_BOMB_CREATE), SW_SHOW)
                                endif
                            endif
                            if (notifyCode /= en_killfocus) return ! ignore other edit box events (like typing)

                            ! When the user presses enter when in the edit box
                            ! this IDOK message is sent to the dialog. We will get the
                            ! the file names, number of ensembles, and seed value here.
                            ! Using the F10 button exits (instead of the windows system response to the Enter key) 
                            ! the dialog.
                            ! TODO: make subroutine
                            if (IAND(wParam, Z'FFFF') == IDC_EDIT_BOMB_ZONAL_WAVE) then
                                gModIntParams%bombZonalWave = GetDlgItemInt(hDlg, IDC_EDIT_BOMB_ZONAL_WAVE, LOC(bret), .TRUE.)
                            elseif  (IAND(wParam, Z'FFFF') == IDC_EDIT_BOMB_MERID_WAVE) then
                                gModIntParams%bombMeridWave = GetDlgItemInt(hDlg, IDC_EDIT_BOMB_MERID_WAVE, LOC(bret), .TRUE.)
                            else if (IAND(wParam, Z'FFFF') == IDC_EDIT_BOMB_STRM_AMP) then
                                bret = GetDlgItemText(hDlg, IDC_EDIT_BOMB_STRM_AMP, textFromReal, LEN(textFromReal))
                                read(textFromReal, *, iostat=ios) gModRealParams%AMPS
                            else if (IAND(wParam, Z'FFFF') == IDC_EDIT_BOMB_TEMP_AMP) then
                                bret = GetDlgItemText(hDlg, IDC_EDIT_BOMB_TEMP_AMP, textFromReal, LEN(textFromReal))
                                read(textFromReal, *, iostat=ios) gModRealParams%AMPT
                            else if (IAND(wParam, Z'FFFF') == IDC_EDIT_BOMB_PHASE_DIFF) then
                                bret = GetDlgItemText(hDlg, IDC_EDIT_BOMB_PHASE_DIFF, textFromReal, LEN(textFromReal))
                                if (bret) read(textFromReal, *, iostat=ios) gModRealParams%ANGS
                            else if (IAND(wParam, Z'FFFF') == IDC_EDIT_BOMB_EXPONENT) then
                                bret = GetDlgItemText(hDlg, IDC_EDIT_BOMB_EXPONENT, textFromReal, LEN(textFromReal))
                                read(textFromReal, *, iostat=ios) gModRealParams%EFOLD
                                ! The following brings the edit box out of focus otherwise it will wait for further edits
                                !ret = SetFocus(GetDlgItem(hDlg, IDC_BUTTON_ENS_CREATE))
                            endif
                            BombDlgProc = 1
                        case (IDCANCEL)
                            bret = UnhookWindowsHookEx(hHook)
                            bret = EndDialog(hDlg, IDCANCEL)
                            ghBombDlg = 0
                            BombDlgProc = 1

! ===============================  Now to handle the F1-F10 buttons
! F1 : Create OBS file
                        case (IDC_BUTTON_BOMB_CREATE)
                            if (gModIntParams%timeStepsToDelay /= 1) then
                                bret = MessageBox (hDlg, &
                                    "Time Steps to delay must be set to 1 to use this option."C, &
                                    "Incorrect Setting for this usage."C, &
                                    IOR(MB_SYSTEMMODAL, IOR(MB_OK, MB_ICONHAND)))
                            endif

                            saveDimensionalState = gHorzArr%isDimensionalized
                            call NonDimensionalizeHorizontalParams()
                            call CreateBomb()
                            if (saveDimensionalState) call DimensionalizeHorizontalParams() 
                            
! F10: Exit dialog
                        case (IDC_BUTTON_EXIT_BOMB) 
                            bret = UnhookWindowsHookEx(hHook)
                            bret = EndDialog(hDlg, 0)
                            ghBombDlg = 0

! F11: Load ensemble set file
!                        case (IDC_BUTTON_LOAD_ENS_SET)

! Clear Bomb Set File
                        !case (IDC_BUTTON_CLEAR_SET)
                        !
                        !case (IDC_BUTTON_ENS_DISTRIBUTION)

                    end select

                case (WM_CLOSE)
                    bret = EndDialog(hDlg, 0)
                    BombDlgProc = 1

                case (WM_DESTROY)

            end select
        end function BombDlgProc

end module BombDialogModule