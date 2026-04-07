MODULE PboxModule
    use IFWINTY
    implicit none
    private
    public :: PBOX, PCLEAR, PLINE, LineBOX, PTROW, PLOT, PNTEXT, PlotTextArray
    public :: getTextStringLength

    ! These methods contain win32 API equivalents to the old plotting ASM methods that wrote
    ! directly to the video memory.
    CONTAINS

        !> ================== PBOX ==================
        !> This method draws a solid color box in a window.
        !> It uses the Win32 API to create a filled rectangle
        !> based on the provided coordinates and RGB color.
        !> The coordinates must be INTEGER(SINT) or INTEGER*2.
        !> Since the rectangle method does not include the bottom
        !> and right edges defined by IX2 and IY2, this function
        !> extends the call to include them. That makes the 
        !> the behavior consistent with the line method.
        !!
        !! @param IX1 Upper-left X-coordinate in window 
        !! @param IY1 Upper-left Y-coordinate in window 
        !! @param IX2 Lower-right X-coordinate in window inclusive
        !! @param IY2 Lower-right Y-coordinate in window inclusive
        !! @param color Packed RGB color value 
        !! @param hWnd Window handle
        function PBOX(IX1, IY1, IX2, IY2, color, hWnd) result(message)
            USE IFWIN
            use gdi32
    
            IMPLICIT NONE

            ! Declare variables
            INTEGER(SINT), INTENT(IN) :: IX1, IY1, IX2, IY2
            INTEGER(KIND = 4), intent(in) :: color  ! RGB color value. Must be 32-bit
            INTEGER(HANDLE), INTENT(IN) :: hWnd
            
            INTEGER(HANDLE) :: hDC, hBrush, hOldBrush, oldPen, newPen
            INTEGER(BOOL) :: bret
            integer :: iret
            CHARACTER(128) :: message
            
            ! Get the device context for the window
            hDC = GetDC(hWnd)
            IF (hDC == 0) THEN
                message = TRIM("Error getting device context in PBOX"C)
                return
            end if
               
            ! color already represents an RGB color
            hBrush = CreateSolidBrush(color)
                
            ! Select the brush into the DC
            hOldBrush = SelectObject(hDC, hBrush)
            
            newPen = GetStockObject(NULL_PEN) ! Use a null pen to avoid drawing the outline
            oldPen = SelectObject(hdc, newPen)
                           
            ! Draw the filled rectangle
            ! Why I have to use the prefixed version of the intel defined interface to Rectangle is a mystery
            bret = MSFWIN$Rectangle(hDC, IX1, IY1, IX2 + 1, IY2 + 1)
            
            ! Restore the old brush. We don't care about the return value but need to handle the return
            hOldBrush = SelectObject(hDC, hOldBrush)
            iret = DeleteObject(hBrush)
            oldPen = SelectObject(hdc, oldPen) ! Restore the old pen but we dont need to delete the new pen as it is a stock object
                
            ! Cleanup: release device context and delete brush
            iret = ReleaseDC(hWnd, hDC)
            message = TRIM("OK"C)
        END function PBOX
        
        !> ================== PCLEAR ==================
        !> This method clears the client area of a window .
        !> to a set color. Use GetSysColor(COLOR_WINDOW) 
        !> to get the window's system background color.
        !!
        !! @param color Packed RGB color value 
        !! @param hWnd Window handle
        function PCLEAR(color, hWnd) result(message)
            USE IFWIN
            use user32
    
            IMPLICIT NONE

            ! Declare variables
            INTEGER(KIND = 4), intent(in) :: color  ! RGB color value. Must be 32-bit
            INTEGER(HANDLE), INTENT(IN) :: hWnd
            
            TYPE(T_RECT) :: rect
            INTEGER(BOOL) :: bret
            CHARACTER(128) :: message
            
            bret = GetClientRect(hWnd, rect) ! Get the test window's client area
            ! Clear anything done in a previous test. Of course PBOX has to work for this to work!
            message = PBOX(rect%left, rect%top, rect%right, rect%bottom, color, hWnd)            
        END function PCLEAR
        
        !> ================== PLINE ==================
        !> This method draws a solid color line from
        !> (IX1, IY1) to (IX2, IY2) in the window specified by hWnd.
        !> The end point is included in the line.
        !> The coordinates must be INTEGER(SINT) or INTEGER*2.
        !!
        !! @param IX1 Start X-coordinate in window 
        !! @param IY1 Start Y-coordinate in window 
        !! @param IX2 End X-coordinate in window inclusive
        !! @param IY2 End Y-coordinate in window inclusive
        !! @param color Packed RGB color value 
        !! @param hWnd Window handle
        function PLINE(IX1, IY1, IX2, IY2, color, hWnd) result(message)
            USE IFWIN       ! Contains GetDC and ReleaseDC
            use gdi32       ! Contains CreatePen, SelectObject, MoveToEx, LineTo, DeleteObject
            
            IMPLICIT NONE

            INTEGER(HANDLE), INTENT(IN) :: hWnd
            INTEGER(SINT), INTENT(IN) :: IX1, IY1, IX2, IY2
            INTEGER, INTENT(IN) :: color
            CHARACTER(128) :: message
            
            INTEGER(HANDLE) :: hDC, hPen, hOldPen
            INTEGER(BOOL) :: bret
            integer :: iret

            ! Obtain device context
            hDC = GetDC(hWnd)
            if (hDC == 0) THEN
                message = TRIM("Error obtaining device context in PLINE.") // CHAR(0)
                RETURN
            END IF

            ! Create a pen with the specified RGB color
            hPen = CreatePen(PS_SOLID, 1, color)
            ! Returns the current pen, or NULL if it fails
            hOldPen = SelectObject(hDC, hPen)

            ! Set the starting point
            bret = MoveToEx(hDC, IX1, IY1, NULL)

            ! Draw the line
            bret = MSFWIN$LineTo(hDC, IX2, IY2)

            ! Restore old pen (we dont care about the return value) and clean up
            hOldPen = SelectObject(hDC, hOldPen)
            bret = DeleteObject(hPen)

            ! Release device context
            iret = ReleaseDC(hWnd, hDC)
            message = TRIM("OK") // CHAR(0)
        END function PLINE
        
        !!************************************************* Make LineBOX of lines:
        !> This method draws a rectangle with no fill by drawing four lines around the rectangle.
        !!
        !! @param IX1 Upper-left X-coordinate in window 
        !! @param IY1 Upper-left Y-coordinate in window 
        !! @param IX2 Lower-right X-coordinate in window inclusive 
        !! @param IY2 Lower-right Y-coordinate in window inclusive
        !! @param color RGB color value for the lines
        !! @param colorFrame RGB color value for the frame
        !! @param hWnd Window handle
        function LineBOX(IX1, IY1, IX2, IY2, color, hWnd) result(message)
            USE IFWIN       ! Contains GetDC and ReleaseDC
            use gdi32       ! Contains CreatePen, SelectObject, MoveToEx, LineTo, DeleteObject
            
            IMPLICIT NONE

            INTEGER(HANDLE), INTENT(IN) :: hWnd
            INTEGER(SINT), INTENT(IN) :: IX1, IY1, IX2, IY2
            INTEGER, INTENT(IN) :: color
            CHARACTER(128) :: message
            
            INTEGER(HANDLE) :: hDC, hPen, hOldPen
            INTEGER(BOOL) :: bret
            integer :: iret

            ! Obtain device context
            hDC = GetDC(hWnd)
            if (hDC == 0) THEN
                message = TRIM("Error obtaining device context in PLINE.") // CHAR(0)
                RETURN
            END IF

            ! Create a pen with the specified RGB color
            hPen = CreatePen(PS_SOLID, 1, color)
            ! Returns the current pen, or NULL if it fails
            hOldPen = SelectObject(hDC, hPen)

            ! Set the starting point
            bret = MoveToEx(hDC, IX1, IY1, NULL)

            ! Draw the outline
            bret = MSFWIN$LineTo(hDC, IX2, IY1) ! Top line
            bret = MSFWIN$LineTo(hDC, IX2, IY2) ! Right line
            bret = MSFWIN$LineTo(hDC, IX1, IY2) ! Bottom line
            bret = MSFWIN$LineTo(hDC, IX1, IY1) ! Left line
            
            ! Restore old pen (we dont care about the return value) and clean up
            hOldPen = SelectObject(hDC, hOldPen)
            bret = DeleteObject(hPen)

            ! Release device context
            iret = ReleaseDC(hWnd, hDC)
            message = TRIM("OK") // CHAR(0)
        END function LineBOX
        
        !> ================== PTROW ==================
        !> This method copies a row of precomputed color data
        !> from an array to the client area of a window from
        !> the start coordinate (IX, IY) to the end of the row.
        !> The row has a value for every pixel and in most cases
        !> the row values have been computed in advance from model
        !> data at much poorer resolution than the screen resolution.
        !> The coordinates must be INTEGER(SINT) or INTEGER*2.
        !!
        !! @param IX Start X-coordinate in window 
        !! @param IY Start Y-coordinate in window 
        !! @param row Array of pixel RGB color values for the row
        !! @param hWnd Window handle
        FUNCTION PTROW(IX, IY, row, hWnd) RESULT(message)
            use IFWINTY
            use IFWIN
            use gdi32
            
            IMPLICIT NONE

            INTEGER(SINT), INTENT(IN) :: IX, IY
            INTEGER(HANDLE), DIMENSION(:), INTENT(IN) :: row
            INTEGER(HANDLE), INTENT(IN) :: hWnd
            CHARACTER(128) :: message

            INTEGER(HANDLE) :: hDC, hMemDC, hBitmap, hOldBitmap
            TYPE(T_BITMAPINFO) :: bmi
            INTEGER(HANDLE) :: lpBits
            INTEGER :: width, height, i
            INTEGER(BOOL) :: bret
            INTEGER :: iret

            ! Define width (number of pixels in the row) and height (1 pixel tall)
            width = SIZE(row)
            height = 1  ! Single row

            ! Get the device context
            hDC = GetDC(hWnd)
            if (hDC == 0) THEN
                message = TRIM("Error obtaining device context in PTROW.") // CHAR(0)
                RETURN
            END IF
            hMemDC = CreateCompatibleDC(hDC)
            if (hMemDC == 0) THEN
                message = TRIM("Error obtaining memory device context in PTROW.") // CHAR(0)
                iret = ReleaseDC(hWnd, hDC)
                RETURN
            END IF

            message = TRIM("OK") // CHAR(0)
            ! Set up bitmap info
            bmi.bmiHeader.biSize = SIZEOF(bmi.bmiHeader)
            bmi.bmiHeader.biWidth = width
            bmi.bmiHeader.biHeight = -height  ! Negative for top-down order
            bmi.bmiHeader.biPlanes = 1
            bmi.bmiHeader.biBitCount = 32  ! 32-bit color
            bmi.bmiHeader.biCompression = BI_RGB

            ! Create DIB Section
            hBitmap = CreateDIBSection(hDC, bmi, DIB_RGB_COLORS, lpBits, NULL, 0)
            hOldBitmap = SelectObject(hMemDC, hBitmap)

            ! Copy row data directly into bitmap memory (fast pixel transfer)
            DO i = 0, width-1
                CALL CopyMemory(lpBits + i * 4, row(i + 1), 4)  ! Still a subroutine
            END DO

            ! BitBlt the row to the screen (function, captures return value)
            iret = BitBlt(hDC, IX, IY, width, height, hMemDC, 0, 0, SRCCOPY)

            ! Generate formatted success message we can display in win32
            IF (iret /= 0) THEN
                WRITE(message, '(A,I0,A,I0)') "Row plotted successfully at (", IX, ", ", IY, ")"C
            ELSE
                message = "Error: Failed to plot row"C
            END IF

            ! Cleanup (all returning values correctly)
            hOldBitmap = SelectObject(hMemDC, hOldBitmap)
            bret = DeleteObject(hBitmap)
            bret = DeleteDC(hMemDC)
            bret = ReleaseDC(hWnd, hDC)
        END FUNCTION PTROW
        
        !> ================== PLOT ==================
        !> This method Plots a single point in a window at the 
        !> specified coordinates with a given color.
        !> Dumb name but that is what it is cled in the original code.
        !> The coordinates must be INTEGER(SINT) or INTEGER*2.
        !!
        !! @param x X-coordinate in window 
        !! @param y Y-coordinate in window 
        !! @param color RGB color value of pixel
        !! @param hWnd Window handle
        function PLOT(x, y, color, hWnd) RESULT(message)
            use IFWINTY
            use IFWIN
            use gdi32
            
            IMPLICIT NONE

            INTEGER(SINT), INTENT(IN) :: x, y
            INTEGER(KIND = 4), INTENT(IN) :: color
            INTEGER(HANDLE), INTENT(IN) :: hWnd
            
            CHARACTER(128) :: message

            INTEGER(HANDLE) :: hDC
            integer :: iret
            INTEGER(BOOL) :: bret
            
            ! Get the device context
            hDC = GetDC(hWnd)
            if (hDC == 0) THEN
                message = TRIM("Error obtaining device context in PTROW.") // CHAR(0)
                RETURN
            END IF
            message = TRIM("OK") // CHAR(0)
            iret = MSFWIN$SetPixel(hDC, x, y, color)
            if (iret == CLR_INVALID) then
                message = TRIM("Error setting pixel color in PLOT.") // CHAR(0)
            end if
            bret = ReleaseDC(hWnd, hDC)
            
        end function PLOT
        
        !> ================== PNTEXT ==================
        !> This method prints text in a window given by hwnd at the
        !> specified coordinates with a given color.
        !> Dumb name but that is what it is called in the original code.
        !> The coordinates must be INTEGER(SINT) or INTEGER*2.
        !!
        !! @param fontParams: type with created font information 
        !! @param ix X-coordinate in window of start of text
        !! @param iy Y-coordinate in window of start of text
        !! @param color RGB color value of the text
        !! @param text Text string to display
        !! @param hWnd Window handle
        !! @return message Resulting message indicating success or failure
        !!         where success if given by the string "OK"C
        FUNCTION PNTEXT(fontParams, ix, iy, color, text, hwnd) RESULT(message)
            USE gdi32
            USE IFWIN
            USE IFWINTY
            use PlotParamsModule, only: FontParameters

            IMPLICIT NONE

            type(FontParameters), intent(in) :: fontParams
            INTEGER(SINT), INTENT(IN) :: ix, iy     ! Coordinates
            INTEGER*4, INTENT(IN) :: color          ! Text color (RGB)
            CHARACTER(*), INTENT(IN) :: text        ! Text to display
            INTEGER(HANDLE), INTENT(IN) :: hwnd     ! Window handle
    
            CHARACTER(128) :: message
            INTEGER(HANDLE) :: hdc, oldFont
            INTEGER :: textResult, colorResult
            INTEGER(BOOL) :: bret
            INTEGER(SINT) :: prevMode

            ! Get the device context (DC)
            hdc = GetDC(hwnd)
            IF (hdc == 0) THEN
                message = TRIM("Failed to get device context.") // CHAR(0)
                RETURN
            END IF
            message = TRIM("OK") // CHAR(0)

            ! Select the font into the DC (returns old font handle)
            oldFont = SelectObject(hdc, fontParams%hFont)
            IF (oldFont == 0) THEN
                message = "Failed to select font."C
                bret = ReleaseDC(hwnd, hdc)
                RETURN
            END IF

            ! Set text color (returns previous color)
            colorResult = MSFWIN$SetTextColor(hdc, color)
            IF (colorResult == CLR_INVALID) THEN
                message = "Failed to set text color."C
                oldFont = SelectObject(hdc, oldFont)
                bret = DeleteObject(fontParams%hFont)
                bret = ReleaseDC(hwnd, hdc)
                RETURN
            END IF

            ! Set background mode to transparent so just the text is printed leaving the background alone
            prevMode = SetBkMode(hdc, TRANSPARENT) 
            ! Display the text (returns success/failure)
            textResult = TextOut(hdc, ix, iy, text, LEN_TRIM(text))
            IF (textResult == 0) message = "Failed to display text."C

            ! Restore previous font and cleanup. Stock fonts are not to be deleted.
            IF (fontParams%hFont /= 0 .AND. fontParams%hFont /= oldFont) THEN
                IF (fontParams%hFont /= GetStockObject(SYSTEM_FONT)) THEN
                    oldFont = SelectObject(hdc, oldFont)
                    bret = DeleteObject(fontParams%hFont)
                END IF
            END IF
            bret = ReleaseDC(hwnd, hdc)

        END FUNCTION PNTEXT
        
        !> ================== PLotTextArray ==================
        !> This method prints a text array in a window given by hwnd at the
        !> specified coordinates with a given color and direction
        !> specified by 'direction'. Each row in the array is drawn one under
        !> the other. It is equiavlent to calling PNTEXT for each row which
        !> would be inefficient because of the creation of the font for each row
        !> and other setup and teardown operations.
        !> The coordinates must be INTEGER(SINT) or INTEGER*2.
        !> For now the 0 degrees is the only supported direction.
        !! @param fontParams: type with created font information 
        !! @param ix X-coordinate in window of start of text
        !! @param iy Y-coordinate in window of start of text
        !! @param color RGB color value of the text
        !! @param textArray Rows of text strings to display
        !! @param hWnd Window handle
        !! @return message Resulting message indicating success or failure
        !!         where success if given by the string "OK"C
        FUNCTION PlotTextArray(fontParams, ix, iy, color, rowLength, textArray, hwnd) RESULT(message)
            USE gdi32
            USE IFWIN
            USE IFWINTY
            use PlotParamsModule, only: FontParameters

            IMPLICIT NONE

            type(FontParameters), intent(in) :: fontParams
            INTEGER(SINT), INTENT(IN) :: ix, iy                 ! Coordinates
            INTEGER(KIND = 4), INTENT(IN) :: color                      ! Text color (RGB)
            INTEGER, INTENT(IN) :: rowLength                    ! Length of each row (determined by caller)
            CHARACTER(rowLength), ALLOCATABLE, DIMENSION(:), intent(in) :: textArray ! Text Array to display with deferred length
            INTEGER(HANDLE), INTENT(IN) :: hwnd                 ! Window handle

            ! Local variables
            CHARACTER(128) :: message
            INTEGER(HANDLE) :: hdc, oldFont
            INTEGER :: textResult, colorResult
            INTEGER(BOOL) :: bret
            INTEGER(SINT) :: prevMode, i, IY1
            TYPE(T_SIZE) :: textSize

            ! Get the device context (DC)
            hdc = GetDC(hwnd)
            IF (hdc == 0) THEN
                message = TRIM("Failed to get device context.") // CHAR(0)
                RETURN
            END IF
            message = TRIM("OK") // CHAR(0)

            ! Select the font into the DC (returns old font handle)
            oldFont = SelectObject(hdc, fontParams%hFont)
            IF (oldFont == 0) THEN
                message = "Failed to select font."C
                bret = ReleaseDC(hwnd, hdc)
                bret = DeleteObject(fontParams%hFont)
                RETURN
            END IF

            ! Get the text size for the first row. Only interested in the height here.
            bret = GetTextExtentPoint32(hdc, textArray(1), LEN(textArray(1)), textSize)

            IF (bret .NE. 0) THEN

                ! Set text color (returns previous color)
                colorResult = MSFWIN$SetTextColor(hdc, color)
                IF (colorResult .NE. CLR_INVALID) THEN
                    ! Set background mode to transparent so just the text is printed leaving the background alone
                    prevMode = SetBkMode(hdc, TRANSPARENT)
                    ! Now for the loop:
                    iy1 = iy  ! Start Y-coordinate for the first row (cant modify IY)
                    do i = 1, SIZE(textArray)  ! Loop through each row in the text array
                        ! Display the text (returns success/failure)
                        textResult = TextOut(hdc, ix, iy1, textArray(i), LEN_TRIM(textArray(i)))
                        IF (textResult == 0) THEN
                            message = "Failed to display text."C
                            exit
                        ENDIF
                        iy1 = iy1 + textSize%cy  ! Move down by the height of the text for the next row
                    end do 
                else
                    message = "Failed to set text color."C
                ENDIF
            ENDIF

            ! Restore previous font and cleanup. Stock fonts are not to be deleted.
            IF (fontParams%hFont /= 0 .AND. fontParams%hFont /= oldFont) THEN
                IF (fontParams%hFont /= GetStockObject(SYSTEM_FONT)) THEN
                    oldFont = SelectObject(hdc, oldFont)
                    bret = DeleteObject(fontParams%hFont)
                END IF
            END IF
            
            bret = ReleaseDC(hwnd, hdc)

        END FUNCTION PlotTextArray
        
        !> ================== getTextStringLength ==================
        !> This method gets the length and width of a text string
        !!
        !! @param fontParams: type with created font information
        !! @param textString Text string to diemnsions of
        !! @param hWnd Window handle
        !! @return the T_SIZE structure returning the width and height as textSize%cx and textSize%cy
        FUNCTION getTextStringLength(fontParams, textString, hWnd) RESULT(textSize)
            USE IFWINTY
            use IFWIN
            use gdi32
            use PlotParamsModule, only: FontParameters

            IMPLICIT NONE

            type(FontParameters), intent(in) :: fontParams
            character(*), INTENT(IN) :: textString
            
            CHARACTER(128) :: message
            INTEGER(HANDLE), intent(in) :: hWnd

            INTEGER(HANDLE) :: hdc, oldFont
            TYPE(T_SIZE) :: textSize
            integer(bool) :: bret

            ! Get the device context for the screen
            hdc = GetDC(hWnd)

            ! Create and select the font
            oldFont = SelectObject(hdc, fontParams%hFont)

            ! Measure the text
            bret = GetTextExtentPoint32(hdc, TRIM(textString), LEN(TRIM(textString)), textSize)

            IF (bret == 0) THEN
                textSize%cx = 0
                textSize%cy = 0
            END IF

            ! Restore the original font and clean up
            oldFont = SelectObject(hdc, oldFont)
            bret = DeleteObject(fontParams%hFont)
            bret = ReleaseDC(0, hdc)
        END FUNCTION getTextStringLength

        ! Indent due to 'contains' but there is no 'end contains' in Fortran

end module PboxModule

