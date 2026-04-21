module DialogHelpersModule
    implicit none

    contains

!================================== GENERIC LISTVIEW Methods used in multiple dialogs
        !! Making a list control is ugly and confusing. If it needs to change (as this one does)
        !! One needs to make 3 separate routines. The rows and columns are handled differently.
        !! You have to create the columns at least once. If you want to update the text in the
        !! column headers you must use a different routine.
        !! 1. Call subroutine EnsureListViewColumns(hList, ampPhase)
        !!    This routine checks to see if the columns have been created and if
        !!    so the subroutine 
        !1    1a. call UpdateListViewHeaders(hList, ampPhase) to update 
        !!        the existing column is called. 
        !!    If it has not been created then the subroutine 
        !!    1b. InitListView(hList, ampPhase) is called to create the header 
        !!        and populate it.
        !! 2. Then after the header crap is done we call the subroutine 
        !!    call loadRealItemsIntoListView(hList, ampPhase) is called to load one of two sets of rows


        !! This routine creates or updates the list control. It checks whether
        !! the list control has been created or not and calls different routines
        !! as appropriate. If it has not been created, the list control will
        !! be initialized otherwise the existing control will get updated with
        !! new data. This does not support a change in the number of columns created
        !! when initialized.
        subroutine EnsureListViewColumns(hList, labels, widths)
            use ifwinty
            use user32
            implicit none
            
            integer(HANDLE), intent(in) :: hList
            character(*), intent(in), dimension(:) :: labels
            integer, intent(in), dimension(:) :: widths

            integer :: numCols
            integer(HANDLE) :: hHeader

            hHeader = SendMessage(hList, LVM_GETHEADER, 0, 0)
            numCols = SendMessage(hHeader, HDM_GETITEMCOUNT, 0, 0)

            if (numCols == 0) then
                call InitListView(hList, labels, widths)
            else
                ! If switching modes changed the number of columns, 
                ! you might need to Delete/Insert, but usually, just update text:
                call  UpdateListViewHeaders(hList, labels)
            end if
        end subroutine

        !! Method initializes the columns in a list control and populates
        !! the headers. The number of columns generated is determined by the
        !! number of headers in 'labels'. The width of each column is given
        !! by 'widths'. Of course the size of 'labels' and 'widths' must be 
        !! the same.
        subroutine InitListView(hwndList, labels, widths)
            use ifwinty
            use user32
            implicit none

            integer(HANDLE), intent(in) :: hwndList
            character(*), dimension(:), intent(in) :: labels
            integer, dimension(:), intent(in) :: widths
            
            type(T_LV_COLUMN) :: lvc
            integer(4) :: i
            integer(4) :: iret            

            do i = 1, size(labels)
                lvc%mask = ior(LVCF_FMT, ior(LVCF_WIDTH, ior(LVCF_TEXT, LVCF_SUBITEM)))
                lvc%fmt  = merge(LVCFMT_LEFT, LVCFMT_LEFT, i == 1) ! Col 1 left, others right
                lvc%cx   = widths(i)
                lvc%pszText = loc(trim(labels(i)) // char(0))
                lvc%iSubItem = i - 1
                iret =  SendMessage(hwndList, LVM_INSERTCOLUMN, INT(i - 1, INT_PTR), loc(lvc))
            end do
            ! Set Full Row Select once
            iret = SendMessage(hwndList, LVM_SETEXTENDEDLISTVIEWSTYLE, 0, INT(LVS_EX_FULLROWSELECT, INT_PTR))
        end subroutine InitListView

        !! Method updates only the headers of the list control. The number of columns is determined 
        !! by the size of 'labels'.
        subroutine UpdateListViewHeaders(hList, labels)
            use ifwinty
            use user32
            implicit none
            integer(HANDLE), intent(in) :: hList
            character(*), dimension(:), intent(in) :: labels
            
            type(T_LV_COLUMN) :: lvc
            integer :: i
            integer(4) :: iret

            lvc%mask = LVCF_TEXT
            do i = 1, size(labels)
                lvc%pszText = loc(trim(labels(i)) // char(0))
                iret = SendMessage(hList, LVM_SETCOLUMN, INT(i - 1, INT_PTR), loc(lvc))
            end do
        end subroutine UpdateListViewHeaders

        !> This subroutine resizes the ListView control to encompass all the rows that have been
        !> added to it. It should be called after all the rows have been added of course.
        !! @hwndList: the ListView control handle. Obtained from hWndList = GetDlgItem(hDlg, IDC_OF_LISTVIEW)
        !!            assuming the control is in a dialog
        !! @left: The current left coordinate in the parent window of the ListView.
        !! @top: The current top coordinate in the parent window of the ListView.
        !! @width: The current width of the ListView control
        subroutine ResizeListViewToFitItems(hwndList, left, top, width, dlgHeight)
            use ifwinty
            use user32      ! For SendMessage

            implicit none
            integer(HANDLE), intent(in) :: hwndList
            integer, intent(in) :: left, top, width
            integer, optional, intent(in) :: dlgHeight

            integer(BOOL) :: bret
            integer :: itemCount, heightPixels
            integer(KIND=INT_PTR_KIND()) :: approxRect

            ! Get number of items
            itemCount = SendMessage(hwndList, LVM_GETITEMCOUNT, 0, 0)

            ! Ask List View for estimated size to fit all rows
            approxRect = SendMessage(hwndList, LVM_APPROXIMATEVIEWRECT, INT(itemCount, UINT_PTR), -1)

            ! Extract height from high word
            heightPixels = ISHFT(approxRect, -16)
            
            ! Do we have enough room in the dialog to expand the list view to fit all items? If not, we will skip and scroll
            if (present(dlgHeight)) then
                if (top + heightPixels > dlgHeight) return
            end if

            ! Resize the control
            bret = MoveWindow(hwndList, left, top, width, heightPixels - 14, .TRUE.) 
            ! The -14 is done because an extra SPACE of pretty close to 14 pt times new roman is left
            ! May need a different value for different font point sizes
        end subroutine ResizeListViewToFitItems
        
        subroutine InitListViewSingle(hwndList, label, labelWidth)
            use ifwinty
            use user32      ! For SendMessage
        
            implicit none
        
            integer(HANDLE), intent(in) :: hwndList
            !integer(HANDLE), intent(in) :: hFont
            character(*), intent(in) :: label
            integer, intent(in) :: labelWidth
            
            type(T_LV_COLUMN) :: lvc
            integer(BOOL) :: bret
            integer(DWORD) :: currentExStyle
            type(T_RECT) :: rc
            integer :: valueWidth
            
            bret = GetClientRect(hwndList, rc)
            valueWidth = rc%right - rc%left - labelWidth  ! buffer for margin/scrollbar is 16 but that is not to be there
            if (valueWidth < 0) valueWidth = 0  ! just in case
        
            ! Set font
            !bret = SendMessage(hwndList, WM_SETFONT, hFont, .TRUE.)
        
            ! Column 1: Parameter
            lvc%mask = LVCF_FMT + LVCF_WIDTH + LVCF_TEXT + LVCF_SUBITEM
            lvc%fmt = LVCFMT_LEFT
            lvc%cx = labelWidth
            lvc%pszText = LOC(label // CHAR(0))
            lvc%iSubItem = 0
            bret = SendMessage(hwndList, LVM_INSERTCOLUMN, 0, LOC(lvc))
        
            ! Column 2: Value
            lvc%cx = valueWidth
            lvc%pszText = LOC("Value"C)
            lvc%iSubItem = 1
            bret = SendMessage(hwndList, LVM_INSERTCOLUMN, 1, LOC(lvc))
            ! Need this so the selected hilite bar extends across both columns
            currentExStyle = SendMessage(hWndList, LVM_GETEXTENDEDLISTVIEWSTYLE, 0, 0)
            bret = SendMessage(hWndList, LVM_SETEXTENDEDLISTVIEWSTYLE, 0, INT(IOR(currentExStyle, LVS_EX_FULLROWSELECT), UINT_PTR))
        
        end subroutine InitListViewSingle

        !> This subroutine adds a row to a ListView report control with two columns
        !> The row will have text and a value, the text describing the value and the value an integer
        !! @hwndList: the ListView control handle. Obtained from hWndList = GetDlgItem(hDlg, IDC_OF_LISTVIEW)
        !!            assuming the control is in a dialog
        !! @label: The text describing the parameter value to be edited.
        !! @value: The value of the parameter that can be edited. This is for integers.
        subroutine AddListViewRow(hwndList, label, value)
            use ifwinty
            use user32      ! For SendMessage
    
            implicit none
            integer(HANDLE), intent(in) :: hwndList
            character(*), intent(in)    :: label
            integer, intent(in)         :: value

            type(T_LV_ITEM) :: lvi
            integer(BOOL) :: bret
            integer :: rowIndex
            character(16) :: valueStr

            ! Insert the label in column 0
            lvi%mask = LVIF_TEXT
            lvi%iItem = SendMessage(hwndList, LVM_GETITEMCOUNT, 0, 0)  ! Next available row
            lvi%iSubItem = 0
            lvi%pszText = LOC(TRIM(label)//CHAR(0))
            rowIndex = SendMessage(hwndList, LVM_INSERTITEM, 0, LOC(lvi))

            ! Convert integer to string for display
            WRITE(valueStr, '(I0)') value

            lvi%iItem = rowIndex
            ! Insert the value in column 1
            lvi%iSubItem = 1
            lvi%pszText = LOC(TRIM(ADJUSTL(valueStr))//CHAR(0)) ! LOC(TRIM(valueStr)//CHAR(0))  trim removes trailing spaces and ajustl removes leading spaces
            bret = SendMessage(hwndList, LVM_SETITEM, 0, LOC(lvi))
        end subroutine AddListViewRow

        !> This subroutine adds a row to a ListView report control with two columns
        !> The row will have text and a value, the text describing the value
        !! @hwndList: the ListView control handle. Obtained from hWndList = GetDlgItem(hDlg, IDC_OF_LISTVIEW)
        !!            assuming the control is in a dialog
        !! @label: The text describing the parameter value to be edited.
        !! @value: The value of the parameter that can be edited. This is for integers.
        !! @formatStr: The format string to use for converting the real value to a string.
        subroutine AddListViewRowReal(hwndList, label, values, formatStr)
            use ifwinty
            use user32      ! For SendMessage

            implicit none
            integer(HANDLE), intent(in) :: hwndList
            character(*), intent(in)    :: label, formatStr
            real, intent(in) :: values(:)  ! Assumed-shape array for N values

            type(T_LV_ITEM) :: lvi
            integer(BOOL)   :: bret
            character(64)   :: valueStr
            REAL        :: val

            integer         :: ret, i, row

            ! 1. Insert the label in column 0 to create the new row
            lvi%mask = LVIF_TEXT
            row = SendMessage(hwndList, LVM_GETITEMCOUNT, 0, 0)
            lvi%iItem = row
            lvi%iSubItem = 0
            lvi%pszText = LOC(TRIM(label)//CHAR(0))
            bret = SendMessage(hwndList, LVM_INSERTITEM, 0, LOC(lvi))

            ! 2. Loop through the array to fill the remaining columns
            do i = 1, SIZE(values)
                !val = values(I)
                ! Convert the current real value to string
                !ret = RealToAsciiToReal(REAL_TO_ASCII, val, valueStr)
                !lvi%pszText = LOC(TRIM(ADJUSTL(valueStr))//CHAR(0))
                ! Update lvi to target the specific subitem (column)
                lvi%iItem = row
                lvi%iSubItem = i  ! Column index (1, 2, 3...)

                write(valueStr, formatStr) values(i)  ! G format is "General" - switches between decimal and scientific
                !write(*, '(A, A)') "Real value 1: ", valueStr
                lvi%pszText = LOC(TRIM(ADJUSTL(valueStr))//CHAR(0))

                ! Use LVM_SETITEM to update the subitem text
                bret = SendMessage(hwndList, LVM_SETITEM, 0, LOC(lvi))
            end do
        end subroutine AddListViewRowReal

        !! Same as above but uses wide characters so one can type symbols
        subroutine AddListViewRowRealWide(hwndList, label, values)
            use, intrinsic :: iso_fortran_env, only: wp => real64
            use ifwinty
            use user32
            use kernel32 ! Needed for MultiByteToWideChar
            implicit none

            integer(HANDLE), intent(in) :: hwndList
            character(*), intent(in)    :: label  ! Pass standard string
            real(wp), intent(in)        :: values(:)

            type(T_LVITEMW) :: lvi
            integer(2), allocatable :: wLabel(:)
            integer :: i, row, bret, nWide
            character(64) :: valueStr
            integer(2) :: wVal(65)

            ! 1. Use the Win32 API to handle the conversion properly
            ! CP_UTF8 (65001) tells Windows the Fortran string is UTF-8
            nWide = MultiByteToWideChar(65001, 0, label, len_trim(label), NULL, 0)
            allocate(wLabel(nWide + 1))
            bret = MultiByteToWideChar(65001, 0, label, len_trim(label), wLabel, nWide)
            wLabel(nWide + 1) = 0

            ! 2. Insert row using Unicode version
            lvi%mask = LVIF_TEXT
            row = SendMessage(hwndList, LVM_GETITEMCOUNT, 0, 0)
            lvi%iItem = row
            lvi%iSubItem = 0
            lvi%pszText = loc(wLabel)
            bret = SendMessage(hwndList, LVM_INSERTITEMW, 0, loc(lvi))

            ! 4. Fill subitems len_trim(adjustl(valueStr))
            valueStr = adjustl(valueStr)
            do i = 1, size(values)
                write(valueStr, '(ES26.17)') values(i)
                block
                    integer :: k
                    wVal = 0
                    ! 1. TRANSFER(adjustl(valueStr), " "):
                    ! The TRANSFER function is Fortran’s "Swiss Army Knife" for memory. It doesn't convert data; it just takes the raw bits 
                    ! from one variable and "pours" them into another shape. By using it here, we ensure the string's bits are handled as a 
                    ! clean array of characters before being converted.
                    ! 2. iachar(...)
                    ! IACHAR is the specific function to get the ASCII code of a character. When you apply it to an array 
                    ! (which the TRANSFER created), it returns an array of integers.
                    ! 3. The "Array Assignment"
                    ! The line wVal(1:N) = iachar(...) is a vector operation. Instead of you writing a DO loop, the compiler handles the loop
                    ! internally. It takes the integer code for "-" and puts it in wVal(1), the code for "2" in wVal(2), and so on.
                    ! Without all this crap I could not get anything past the 'E' to display in the list control.
                    wVal(1:len_trim(adjustl(valueStr))) = iachar(transfer(adjustl(valueStr), " ", len_trim(adjustl(valueStr))))
                    lvi%iSubItem = i
                    lvi%pszText = loc(wVal)
                    bret = SendMessage(hwndList, LVM_SETITEMW, 0, loc(lvi))
                end block
            end do
        end subroutine AddListViewRowRealWide
        
        subroutine updateListViewValue(hEdit, hList, rowClicked, strC)
            use ifwinty
            use user32, only: SendMessage, SetWindowPos
            
            integer, intent(in) :: rowClicked
            integer(HANDLE), intent(in) :: hList, hEdit
            character(*), intent(in) :: strC

            type(T_LVITEM) :: lvItem
            integer(BOOL) ::  bret

            lvItem%mask = LVIF_TEXT
            lvItem%iItem = rowClicked
            lvItem%iSubItem = 1 ! Our list view has two columns, 0 and 1. The value is in column 1.
            lvItem%pszText = loc(strC)
            ! LVM_SETITEMTEXT should work here but it does not. It causes row 0 to get updated regardless
            ! LVM_SETITEM does work. Don't ask me why. Many days of frustration
            bret = SendMessage(hList, LVM_SETITEM, 0, loc(lvItem))

            ! This line redrawns the edit box borders which otherwise vanish.
            bret = SetWindowPos(hEdit, 0, 0, 0, 0, 0, SWP_NOMOVE .OR. SWP_NOSIZE .OR. SWP_FRAMECHANGED .OR. SWP_NOZORDER)
        end subroutine updateListViewValue

        !=================== HELPER to move an edit box over a list view value
        !> Moves the edit box over the selected list view item for editing
        !> Assumes the list view has two columns where the first column is a text description
        !> and the second column is the integer or real value to be edited.
        !>
        !> Absurdly complicated. To do this you have to get the rectangle 
        !> occupied by the column containing the value in the list view.
        !> Then you move the edit box there and resize it to fit the rectangle.
        !> The problem is that the various methods get the rectangle
        !> coordinates in list view client coordinates which has to be
        !> converted to dialog client coordinates.
        !! @param hDlg - handle to the dialog box
        !! @param hList - handle to the list view control
        !! @param hEdit - handle to the edit box control to move
        !! @param rowClicked - the row in the list view that was clicked
        subroutine moveEditBox(hdlg, hList, hEdit, rowClicked)
            use ifwinty
            use user32
            use iso_c_binding, only: C_INTPTR_T
            
            implicit none
            
            integer(HANDLE), intent(in) :: hdlg, hList, hEdit
            integer, intent(in) :: rowClicked
            
            integer(BOOL) ::  bret
            TYPE(T_RECT) :: rect_bounds
            TYPE(T_POINT) :: pt, pt2

            rect_bounds%top = 1 ! This gives the list view column to get the rectangle for
            rect_bounds%left = LVIR_BOUNDS ! This is a command that tells the message to get the bounding rectangle
            bret = SendMessage(hList, &                      ! Handle to list view
                               LVM_GETSUBITEMRECT, &         ! Instruction to get the subitem in rect_bounds%top
                               INT(rowClicked, UINT_PTR), &  ! The row index of the list view containing the desired subitem 
                               LOC(rect_bounds))            ! Returns the rectangle in list view client coordinates
                                        
            ! Convert the top-left point from list view client to screen coordinates
            pt%x = rect_bounds%left
            pt%y = rect_bounds%top
            bret = ClientToScreen(hList, pt)

            ! Convert the bottom-right point from list view client to screen coordinates
            pt2%x = rect_bounds%right
            pt2%y = rect_bounds%bottom
            bret = ClientToScreen(hList, pt2)

            ! Convert the two points from screen to dialog client coordinates
            bret = ScreenToClient(hDlg, pt)
            bret = ScreenToClient(hDlg, pt2)

            ! Now we can move the edit box to the correct position and size
            bret = MoveWindow(hEdit, pt%x, pt%y, &
                pt2%x - pt%x, &
                pt2%y - pt%y, 1)
            ! Make the edit box visible and set focus to it
            bret = ShowWindow(hEdit, SW_SHOW)
            ! The last thing is actually sending the value 1 but as a pointer to 1. Bizzare.
            bret = PostMessage(hDlg, WM_NEXTDLGCTL, hEdit, INT(1, C_INTPTR_T)) ! Delay set focus until the calling routine in callback completes
            !bret = SetFocus(hEdit)  ! This is immediate and will interrupt an ongoing handling such as a WM_CLICK
        end subroutine moveEditBox

        !> This routine hides the edit box that overlays a list control value. THis is called
        !> when the user clicks on something else besides a list control row.
        subroutine HideMyEdit(hEdit)
            use ifwin, only: BOOL, HANDLE, SW_HIDE, ShowWindow, SetWindowText

            implicit none

            integer(HANDLE), intent(in) :: hEdit
            
            integer(BOOL) :: bret

            ! 1. Hide it so it doesn't stay on screen
            bret =  ShowWindow(hEdit, SW_HIDE)
            ! 2. Clear the text so old data doesn't "ghost" next time
            bret = SetWindowText(hEdit, ""C) 
        end subroutine HideMyEdit

        function GetComboIndex(hDlg, ctrlID) result(idx)
            use ifwinty
            use ifwin

            INTEGER(HANDLE), INTENT(IN) :: hDlg
            INTEGER, INTENT(IN) :: ctrlID

            integer :: idx
            idx = SendMessage(GetDlgItem(hDlg, ctrlID), CB_GETCURSEL, 0, 0)
        END function GetComboIndex

! =============================== WINDOWS FILE ROUTINES
        function GetUserSelectedSaveName(hDlg, fileInfo) result(bres)
            use ifwinty    ! or equivalent win32 module
            use ifwin      ! for GetSaveFileName and constants
            use ModelParamsModule, only: FileStruct

            implicit none
            
            integer(HANDLE), intent(in) :: hDlg
            type(FileStruct), intent(inout) :: fileInfo
            
            type(T_OPENFILENAME) :: ofn
            character(270)       :: szFile = ""C  ! Uses the C-string extension to null the buffer
                                                  ! shown to user          for win32 api
            character(100)       :: szFilter    = "Model save Files (*.sav)"C// "*.sav"C // ""C

            LOGICAL :: bres

            bres = .true.

            ! Clear and set structure size
            ofn%lpstrCustomFilter = 0
            ofn%nMaxCustFilter    = 0
            ofn%lpstrFileTitle    = 0
            ofn%nMaxFileTitle     = 0
            ofn%lpstrInitialDir   = 0
            ofn%lpstrTitle        = 0
            ofn%lCustData         = 0
            ofn%lpfnHook          = 0
            ofn%lpTemplateName    = 0

            ofn%lStructSize = sizeof(ofn)
            ofn%hwndOwner   = hDlg  ! Parent dialog handle
            ofn%lpstrFile   = loc(szFile)
            ofn%nMaxFile    = len(szFile)   ! Get the number of characters
            ofn%lpstrFilter = loc(szFilter)
            ofn%nFilterIndex = 1
            ofn%lpstrFileTitle = 0
            ofn%nMaxFileTitle = 0
            ofn%lpstrInitialDir = 0
            ofn%lpstrDefExt = loc("sav"C)
            ofn%Flags = IANY([OFN_EXPLORER, OFN_OVERWRITEPROMPT, OFN_PATHMUSTEXIST, OFN_NOCHANGEDIR]) ! Modern way of ORing several parameters
            
            if (GetSaveFileName(ofn)) then
                ! Set file name
                fileInfo%filename = szFile(1:INDEX(szFile, CHAR(0)) - 1) ! Forces fortran to go until null char and not treat null char as okay
            else
                bres = .false.
            endif
        end function GetUserSelectedSaveName
    
        function GetNameOfUserSelectedFile(hDlg, fileInfo) result(bres)
            use ifwinty    ! or equivalent win32 module
            use ifwin      ! for GetSaveFileName and constants
            use ModelParamsModule, only: FileStruct

            implicit none
            
            integer(HANDLE), intent(in) :: hDlg
            type(FileStruct), intent(inout) :: fileInfo
            
            type(T_OPENFILENAME) :: ofn
            character(270)       :: szFile = ""C  ! Uses the C-string extension to null the buffer 
            character(100)       :: szFilter    = "Model Save Files (*.sav)"C// "*.sav"C // ""C

            LOGICAL :: bres
            integer :: ires

            bres = .true.

            ! Clear and set structure size
            ofn%lpstrCustomFilter = 0
            ofn%nMaxCustFilter    = 0
            ofn%lpstrFileTitle    = 0
            ofn%nMaxFileTitle     = 0
            ofn%lpstrInitialDir   = 0
            ofn%lpstrTitle        = 0
            ofn%lCustData         = 0
            ofn%lpfnHook          = 0
            ofn%lpTemplateName    = 0

            ofn%lStructSize = sizeof(ofn)
            ofn%hwndOwner   = hDlg  ! Parent dialog handle
            ofn%lpstrFile   = loc(szFile)
            ofn%nMaxFile    = len(szFile)   ! Get the number of characters
            ofn%lpstrFilter = loc(szFilter)
            ofn%nFilterIndex = 1
            ofn%lpstrFileTitle = 0
            ofn%nMaxFileTitle = 0
            ofn%lpstrInitialDir = 0
            ofn%lpstrDefExt = loc("sav"C)
            ofn%Flags = IANY([OFN_EXPLORER, OFN_OVERWRITEPROMPT, OFN_PATHMUSTEXIST, OFN_NOCHANGEDIR]) ! Modern way of ORing several parameters
            !ofn%Flags = IANY([OFN_FILEMUSTEXIST, OFN_HIDEREADONLY, OFN_NOCHANGEDIR]) ! Modern way of ORing several parameters
            if (GetOpenFileName(ofn)) then
                ! Set file name
                fileInfo%filename = trim(szFile(1:INDEX(szFile, CHAR(0)) - 1)) ! Forces fortran to go until null char and not treat null char as okay
            else
                bres = .false.
            endif
        end function GetNameOfUserSelectedFile

end module DialogHelpersModule