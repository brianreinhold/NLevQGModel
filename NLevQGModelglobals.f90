!****************************************************************************
!  Global data, parameters, and structures 
!****************************************************************************

module NLevQGModelGlobals
    use ifwinty
    use iso_c_binding

    implicit none
    
    public LogMessage, WriteMessageBox, StartTimer, CenterWindow

    !  Parameters

    integer*4, parameter, public :: SIZEOFAPPNAME = 100

    !  Global data

    integer(HANDLE)		ghInstance
    integer(HANDLE)		ghModule
    integer(HANDLE)		ghwndMain
    integer(HANDLE)		ghMenu

    integer(kind=4)     gFrameX          ! Space around the plots
    integer(kind=4)     gFrameY          ! Space around the plots
    
    ! For drawing into main window
    TYPE BitMapInfo
        INTEGER(HANDLE) :: hMemDC       ! Persistent Drawing DC
        INTEGER(HANDLE) :: hBitmap      ! The actual "Canvas"
        INTEGER(HANDLE) :: hOldBitmap   ! For cleanup
        type(T_RECT)    :: rect         ! Canvas Rectangle
        INTEGER(4)      :: width        ! Canvas Width
        INTEGER(4)      :: height       ! Canvas Height
        INTEGER(4)      :: startX       ! X Position
        INTEGER(4)      :: startY       ! Y Position
        TYPE(C_PTR)     :: pBits        ! Pointer to raw pixel data for direct writes
    END TYPE BitMapInfo

    ! LOG Window
    integer(HANDLE) ::     gFont
    integer(HANDLE) ::     ghLogMain
    integer(HANDLE) ::     ghLogEdit
    integer(HANDLE) ::     ghLogList
    integer(HANDLE) ::     ghLogMenu
    
    ! Dialog handles
    integer(HANDLE) ::     ghSettingsDlg
    integer(HANDLE) ::     ghModelControlDlg
    integer(HANDLE) ::     ghVerticalParamsDlg
    integer(HANDLE) ::     ghInitVariablesDlg
    integer(HANDLE) ::     ghContoursAndRangesDlg
    integer(HANDLE) ::     ghViewVariablesDlg
    integer(HANDLE) ::     ghBombDlg
    integer(HANDLE) ::     ghHelpDlg = NULL
    
    integer(HANDLE) ::     ghCurrentDlg             ! For anytime we need to refer to the "current" dialog
                                                    ! for example in winmain procedure when returning to calling dialog

    integer(8) ::          startTime
    type(BitMapInfo) ::    gBitMapInfo


    contains ! Need this to declare a method in a module. Must come after any interfaces
        integer(4) function WriteMessageBox(hwnd, number, msg, header)
            use user32
            use kernel32
            use ifwbase
            use iso_c_binding

            implicit none

            integer(HANDLE), intent(in) :: hwnd
            integer(4), intent(in) :: number
            character(*), intent(in) :: msg
            character(*), intent(in) :: header

            character(len = 16) errStr
            character(len = 16, kind = c_char) errMsg

            integer (LRESULT) resp
            integer(4) ret

            ret = 0

            WRITE(errStr, '(I0)') number  ! Converts 'err' integer to string
                                        ! WRITE won�t work directly on a c_char character variable so
                                        ! need two variables, errStr and errMsg. The latter is declared
                                        ! as a c-type character, errStr is not.
                                        ! Note that Fortran requires a separate buffer for integer-to-string conversion.
            errMsg = trim(adjustl(errStr)) // C_NULL_CHAR  ! Trim off extra and Null-terminate
            resp = MessageBox(hwnd, trim(msg) // " " // errMsg, trim(header) // C_NULL_CHAR, MB_OK) ! In FORTRAN you MUST assign a return value for every function.
            WriteMessageBox = ret
        end function

        SUBROUTINE StartTimer()
            INTEGER :: count, rate
            CALL SYSTEM_CLOCK(count, rate)
            startTime = count
        END SUBROUTINE StartTimer

        FUNCTION GetElapsedTime() RESULT(elapsedMS)
            INTEGER :: count, rate, elapsedMS
            CALL SYSTEM_CLOCK(count, rate)
            elapsedMS = (count - startTime) * 1000 / rate
        END FUNCTION 
    
        SUBROUTINE LogMessage(msg)
            use iso_c_binding
            use user32
            
            CHARACTER(len=*), INTENT(IN) :: msg

            INTEGER :: elapsedMS
            character(len = 16) errStr
            character(len = 16, kind = c_char) errMsg
            integer(SINT)   iret
            CHARACTER(:), ALLOCATABLE :: logEntry
            INTEGER :: msgLen
        
        
            elapsedMS = GetElapsedTime()
            write(errStr, '(I0)') elapsedMS
            errMsg = trim(adjustl(errStr))
            
            msgLen = LEN_TRIM(msg) + LEN_TRIM(errMsg) + 15  ! Calculate required length
            ALLOCATE(CHARACTER(msgLen) :: logEntry)  ! Allocate dynamically
            logEntry = "Elapsed Time: " // errMsg // " " // msg // C_NULL_CHAR
            write(*, '(A, I6, A, A)') "Elapsed Time: ", elapsedMS, " ", trim(msg)  ! Also write to console for debugging (Second A forces output on one line)
            iret = SendMessage(ghLogList, LB_INSERTSTRING, 0, LOC(logEntry)) ! Insert log at TOP of list so latest is first

        END SUBROUTINE LogMessage

        !****************************************************************************
        !
        !  FUNCTION: CenterWindow (HWND, HWND)
        !
        !  PURPOSE:  Center one window over another
        !
        !  COMMENTS: Dialog boxes take on the screen position that they were designed
        !            at, which is not always appropriate. Centering the dialog over a
        !            particular window usually results in a better position.
        !
        !****************************************************************************

        subroutine CenterWindow (hwndChild, hwndParent)

            use user32
            use gdi32 

            implicit none

            integer(HANDLE)         hwndChild, hwndParent

            include 'resource.fd'

            ! Variables
            type (T_RECT)   rChild, rParent
            integer         wChild, hChild, wParent, hParent
            integer         wScreen, hScreen, xNew, yNew
            integer(HANDLE)         hdc
            integer*4       retval

            ! Get the Height and Width of the child window
                retval = GetWindowRect (hwndChild, rChild)
                wChild = rChild.right - rChild.left
                hChild = rChild.bottom - rChild.top

            ! Get the Height and Width of the parent window
                if (hwndParent == 0) then
                    ! Use the Desktop handle to get screen dimensions
                    hwndParent = GetDesktopWindow() 
                end if
                retval = GetWindowRect (hwndParent, rParent)
                wParent = rParent.right - rParent.left
                hParent = rParent.bottom - rParent.top

            ! Get the display limits
                hdc = GetDC (hwndChild)
                wScreen = GetDeviceCaps (hdc, HORZRES)
                hScreen = GetDeviceCaps (hdc, VERTRES)
                retval = ReleaseDC (hwndChild, hdc)

            ! Calculate new X position, then adjust for screen
                xNew = rParent.left + ((wParent - wChild) /2)
                if (xNew .LT. 0) then
                    xNew = 0
                else if ((xNew+wChild) .GT. wScreen) then
                    xNew = wScreen - wChild
                end if

            ! Calculate new Y position, then adjust for screen
                yNew = rParent.top  + ((hParent - hChild) /2)
                if (yNew .LT. 0) then
                    yNew = 0
                else if ((yNew+hChild) .GT. hScreen) then
                    yNew = hScreen - hChild
                end if

            ! Set it, and return
                retval = SetWindowPos (hwndChild, NULL, xNew, yNew, 0, 0,      &
                                IOR(SWP_NOSIZE , SWP_NOZORDER))
        end subroutine CenterWindow

end module
