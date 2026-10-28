SUBMODULE (PlotModule) AxesSubmodule

    contains

        ! This is for the axes in phase space plots. Unlike most of the axes drawn this is
        ! drawn once at the start of the plot as only points are drawn each time step on the same background.
        Module subroutine DrawCartesianAxes(invalidate, fasRymd)
            USE IFWINA
            use ConstantsModule, only: HEIGHT, TEMPERATURE, PHASE_SPACE, PHASE_AND_MAP, PLOTTING_OFF
            use NLevQGModelGlobals, only: gBitMapInfo, ghwndMain
            use ModelParamsModule, only: gModIntParams, gContourIntervals
            implicit none

            ! Arguments
            logical, intent(in) :: invalidate  ! Force redraw flag
            TYPE(PhaseSpace), intent(in) :: fasRymd

            ! Local variables
            type(CartesianData) cartData
            integer(4) :: x_len       ! Length of X-axis in pixels
            integer(4) :: y_len       ! Length of Y-axis in pixels
            INTEGER(BOOL) :: bret
            integer(4) :: i
        
            integer(HANDLE) :: hOldPen, hAxisPen
            type(T_POINT) :: oldPoint

            ! For labels
            character(len=16) :: label
            integer(4) :: x_anchor, y_anchor, y_pos, x_pos
            type(T_SIZE) :: sz

            cartData = fasRymd%cartData

            ! 1. Create 2nd pen 1 pixel thick and select it
            hAxisPen = CreatePen(PS_SOLID, 1, AXIS_COLOR)
            hOldPen = SelectObject(gBitMapInfo%hMemDC, hAxisPen)

            ! --- Draw Vertical Grid Lines (X-axis ticks) ---
            do i = -cartData%n_left, cartData%n_right
                if (i == 0) cycle  ! skip central axis
                ! Move to top and draw to bottom
                bret = MoveToEx(gBitMapInfo%hMemDC, int(cartData%x_origin + i * cartData%axis_dx), cartData%startY, oldPoint)
                bret = LineTo(gBitMapInfo%hMemDC, int(cartData%x_origin + i * cartData%axis_dx), cartData%endY)
            end do

            ! --- Draw Horizontal Grid Lines (Y-axis ticks) ---
            do i = cartData%n_down, -cartData%n_up, -1
                if (i == 0) cycle ! skip central axis
                ! Move to left side, draw to right side
                bret = MoveToEx(gBitMapInfo%hMemDC, cartData%startX, int(cartData%y_origin + i * cartData%axis_dx), oldPoint)
                bret = LineTo(gBitMapInfo%hMemDC, cartData%endX, int(cartData%y_origin + i * cartData%axis_dx))
            end do

            ! 2. Do labels
            if (fasRymd%field .NE. PLOTTING_OFF) then
                call labelCartesianAxes(cartData, (fasRymd%field == gModIntParams%field1), fasRymd%gDCcolor)
            endif

            ! 3. Cleanup: Restore old pen and delete the one created
            hOldPen = SelectObject(gBitMapInfo%hMemDC, hOldPen)
            bret = DeleteObject(hAxisPen)

            !  Now to draw the main origin axes.
            ! 4. Create and select a pen 2 pixels thick. Do main axes last so they are on top
            hAxisPen = CreatePen(PS_SOLID, 2, RGB(50, 50, 50))
            hOldPen = SelectObject(gBitMapInfo%hMemDC, hAxisPen)

            ! 5. Draw X-axis
            bret = MoveToEx(gBitMapInfo%hMemDC, cartData%startX, cartData%y_origin, oldPoint)
            bret = LineTo(gBitMapInfo%hMemDC, cartData%endX, cartData%y_origin)

            ! 6. Draw Y-axis
            bret = MoveToEx(gBitMapInfo%hMemDC, cartData%x_origin, cartData%startY, oldPoint)
            bret = LineTo(gBitMapInfo%hMemDC, cartData%x_origin, cartData%endY)

            ! 7. Cleanup: Restore old pen and delete the one created
            hOldPen = SelectObject(gBitMapInfo%hMemDC, hOldPen)
            bret = DeleteObject(hAxisPen)

            ! 8. Redraw if requested
            if(invalidate) then
                ! Invalidate the window rectangle to trigger a redraw
                bret = InvalidateRect(ghwndMain, LOC(gBitMapInfo%rect), 0) ! 0 means do not erase background
                bret = UpdateWindow(ghwndMain)
            endif

        end subroutine DrawCartesianAxes

        ! So far this is only called from this file and therefore the prefix 'module' is not needed, nor is an interface
        subroutine labelCartesianAxes(cartData, useBottomLeft, textColor)
            USE IFWINA
            use NLevQGModelGlobals, only: gBitMapInfo, ghwndMain

            implicit none

            type(CartesianData), intent(in) :: cartData
            logical, intent(in) :: useBottomLeft
            integer(4), intent(in) :: textColor

            integer(bool) :: bret
            ! For labels
            character(len=16) :: label
            integer(4) :: x_anchor, y_anchor, y_pos, x_pos, i
            type(T_SIZE) :: sz

            ! Now doing labels. Make the background transparent
            bret = SetBkMode(gBitMapInfo%hMemDC, TRANSPARENT)
            bret = SetTextColor(gBitMapInfo%hMemDC, textColor) ! dark text

            ! --- Label the Y-Axis values ---
            if (useBottomLeft) then
                ! Anchor: To the right of the first 'y axis' (left-most vertical grid line)
                x_anchor = cartData%x_origin - cartData%n_left * int(cartData%axis_dx)
            else
                ! Anchor: To the left of the last 'y axis' (right-most vertical grid line)
                x_anchor = cartData%x_origin + (cartData%n_right - 1) * int(cartData%axis_dx)
            endif

            do i = cartData%n_up, -cartData%n_down, -2
                if (cartData%interval * i .LE. 999 .AND. cartData%interval * i .GE. -999) then
                    write(label, '(F8.2)') (real(cartData%interval * i))
                else
                    write(label, '(F6.0)') (real(cartData%interval * i))
                endif
                y_pos = cartData%y_origin - int(i * cartData%axis_dx) - 2 ! Top-down: subtract to go "up"

                ! Get label dimensions to vertically center it on the tick
                bret = GetTextExtentPoint32(gBitMapInfo%hMemDC, trim(label), len_trim(label), sz)

                ! Draw 5 pixels to the right of the first vertical grid line
                bret = TextOut(gBitMapInfo%hMemDC, x_anchor + 5, y_pos - (sz%cy / 2), &
                              trim(label), len_trim(label))
            end do

            ! --- Label the X-Axis values ---
            if (useBottomLeft) then
                ! Anchor: Above the last 'x axis' (bottom-most horizontal grid line)
                y_anchor = cartData%y_origin + cartData%n_down * int(cartData%axis_dx)
            else
                ! Anchor: Below the first 'x axis' (top-most horizontal grid line)
                y_anchor = cartData%y_origin - cartData%n_up * int(cartData%axis_dx) + 5
            endif
            
            do i = -cartData%n_left, cartData%n_right, 2
                if (cartData%interval * i .LE. 999 .AND. cartData%interval * i .GE. -999) then
                    write(label, '(F8.2)') (real(cartData%interval * i))
                else
                    write(label, '(F6.0)') (real(cartData%interval * i))
                endif
                x_pos = cartData%x_origin + int(i * cartData%axis_dx)
            
                bret = GetTextExtentPoint32(gBitMapInfo%hMemDC, trim(label), len_trim(label), sz)
            
                ! Draw 5 pixels above the bottom-most horizontal grid line
                bret = TextOut(gBitMapInfo%hMemDC, x_pos - (sz%cx / 2), y_anchor - sz%cy + 5, &
                              trim(label), len_trim(label))
            end do
    end subroutine labelCartesianAxes

        !> This routine draws horizontal (east-west latitude) and vertical 
        !> (north-south longitude) lines in a grid for the weather maps
        !> In cross sections one has either latitude or longitude along x
        !> and level along y.
        !! @param: isFullScreen - if true, the plot is a cross section and occupies the full screen.
        !!                      If false, the plot is a weather map and occupies half the screen.
        !! @param: invalidate - if set to true the screen is redrawn.
        !!         The option is present as the caller may do the redraw
        Module subroutine DrawGridLines(isFullScreen, invalidate)
            use IFWBASE ! Contains RGB macro
            USE IFWINTY
            USE IFWINA
            USE USER32
            use NLevQGModelGlobals, only: gBitMapInfo, ghwndMain
            use ModelParamsModule, only: gModIntParams
            use ConstantsModule, only: VERT_CROSS_SEC_AT_LON
        
            IMPLICIT NONE
        
            LOGICAL, intent(in) :: isFullScreen, invalidate
        
            INTEGER(BOOL) :: bret            
            integer(HANDLE) :: hOldPen, hAxisPen
            integer(4) :: I, height, count, x_coord, y_coord, offset
            type(T_POINT) :: oldPoint
            
            real :: value, spacing
            character(len=32) :: labelText
            character(len=16) :: units
            
            ! 1. Create and select a pen
            hAxisPen = CreatePen(PS_SOLID, 1, RGB(50, 50, 50))
            hOldPen = SelectObject(gBitMapInfo%hMemDC, hAxisPen)
            
            ! Align text so the coordinate (x,y) is the top-left of the text
            bret = SetTextColor(gBitMapInfo%hMemDC, RGB(50, 50, 50))
            bret = SetTextAlign(gBitMapInfo%hMemDC, IOR(TA_LEFT, TA_TOP))
            bret = SetBkMode(gBitMapInfo%hMemDC, TRANSPARENT)
            
            if (isFullScreen) then
                height = gBitMapInfo%height
                count = 8
            else
                height = gBitMapInfo%height / 2
                count = 4
            endif

            ! 2. Draw horizonal lines. Will be latitude for WEATHER_MAP and height (pressure) for VERT_CROSS_SEC_AT_LON, latitude in VERT_CROSS_SEC_AT_LAT
            if (isFullScreen) then  ! This is a cross section
                units = " mb"
            else
                spacing = 22.5    ! 90.0 / 4.0
                units = CHAR(176)//"lat"
            endif
            offset = -15
            x_coord = 20
            do I = count, 1, -1
                ! Move to the start of the line: X=0, Y=calculated height
                Y_coord = (I * gBitMapInfo%height / 8) - 1
                bret = MoveToEx(gBitMapInfo%hMemDC, 0, Y_coord, oldPoint)
                ! Draw to the end of the line: X=width, Y=calculated height
                bret = LineTo(gBitMapInfo%hMemDC, gBitMapInfo%width, Y_coord)
                !Y_coord = (I - 1) * (gBitMapInfo%height - 1) / (count - 1)
                labelText = ""
                ! 2. Draw the label (assuming latitude is calculated based on I)
                if (isFullScreen) then
                    value = gPressure(gBitMapInfo%height - Y_coord)
                else
                    value = real((count - I)) * spacing
                endif
                ! This 'T12' forces the units to start at the 12th character of the string
                write(labelText, '(F10.3, T12, A)') value, trim(units)
                ! TextOut(hdc, x, y, string, length)
                ! Placing label at X=5 pixels from the left edge
                if (I .EQ. 0) then
                    offset = 0
                    x_coord = 40
                endif
                bret = TextOut(gBitMapInfo%hMemDC, x_coord, Y_coord + offset, labelText, len_trim(labelText))
            end do

            ! 3. Draw the vertical lines. They will be the longitude in WEATHER_MAP and VERT_CROSS_SEC_AT_LAT, longitude in VERT_CROSS_SEC_AT_LAT
            if (gModIntParams%plotType == VERT_CROSS_SEC_AT_LON) then
                spacing = 11.25     ! 90.0 / 8.0
                units = "lat"
            else  ! WEATHER_MAP and VERT_CROSS_SEC_AT_LAT with both have the longitude along the top in x
                spacing = 45.0      ! 360. / 8.0
                units = "lon"
            endif
            do I = 1, 8
                bret = MoveToEx(gBitMapInfo%hMemDC, I * gBitMapInfo%width / 8, 0, oldPoint)
                bret = LineTo(gBitMapInfo%hMemDC, I * gBitMapInfo%width / 8, height)
                x_coord = (I - 1) * gBitMapInfo%width / 8
                value = real((I - 1)) * spacing
                labelText = ""
                ! This 'T8' forces the units to start at the 8th character of the string
                write(labelText, '(F6.2, T8, A)') value, CHAR(176)//units
                ! TextOut(hdc, x, y, string, length)
                ! Placing label at X=5 pixels from the left edge
                if (I .EQ. 9) x_coord = x_coord - 25
                bret = TextOut(gBitMapInfo%hMemDC, x_coord, 5, labelText, len_trim(labelText))
            end do
        
            ! 4. Cleanup: Restore old pen and delete the one created
            hOldPen = SelectObject(gBitMapInfo%hMemDC, hOldPen)
            bret = DeleteObject(hAxisPen)
        
            if(invalidate) then
                ! Invalidate the window rectangle to trigger a redraw
                bret = InvalidateRect(ghwndMain, LOC(gBitMapInfo%rect), 0) ! 0 means do not erase background
                bret = UpdateWindow(ghwndMain)
            endif
        end subroutine DrawGridLines

        !> This routine draws vertical (longitudinal) lines for the
        !> latitudinal hovmoller plot. Remember that the latitudinal
        !> Hovmoller plots the values along a specified east-west
        !> latitude as a function of time where time is along the y-axis
        !> so the plot comes from the top. These lines show the longitude
        !> positions on the latitudinal line.
        !! @param: invalidate - if set to true the screen is redrawn.
        !!         The option is present as the caller may do the redraw
        module subroutine DrawVerticalLines(invalidate)
            use IFWBASE ! Contains RGB macro
            USE IFWINTY
            USE IFWINA
            USE USER32

            use NLevQGModelGlobals, only: gBitMapInfo, ghwndMain
            
            IMPLICIT NONE
            
            LOGICAL, intent(in) :: invalidate
            
            INTEGER(BOOL) :: bret            
            integer(HANDLE) :: hOldPen, hAxisPen
            integer(4) :: I, x_coord, offset
            type(T_POINT) :: oldPoint
            real :: value
            character(len=32) :: labelText
            character(len=16) :: units
        
            ! 2. Create and select a pen (Black, 2 pixels wide)
            hAxisPen = CreatePen(PS_SOLID, 1, RGB(50, 50, 50))
            hOldPen = SelectObject(gBitMapInfo%hMemDC, hAxisPen)
            
            ! Align text so the coordinate (x,y) is the top-left of the text
            bret = SetTextColor(gBitMapInfo%hMemDC, RGB(50, 50, 50))
            bret = SetTextAlign(gBitMapInfo%hMemDC, IOR(TA_LEFT, TA_TOP))
            bret = SetBkMode(gBitMapInfo%hMemDC, TRANSPARENT)
        
            ! 4. Draw Longitude lines
            do I = 1, 9
                bret = MoveToEx(gBitMapInfo%hMemDC, I * gBitMapInfo%width / 8, 0, oldPoint)
                bret = LineTo(gBitMapInfo%hMemDC, I * gBitMapInfo%width / 8, gBitMapInfo%height)
                
                x_coord = (I - 1) * gBitMapInfo%width / 8
                value = real((I - 1)) * 45.0 !360.0 / 8
                labelText = ""
                ! This 'T8' forces the units to start at the 8th character of the string
                write(labelText, '(F6.2, T8, A)') value, CHAR(176)//"lon"
                ! TextOut(hdc, x, y, string, length)
                ! Placing label at X=5 pixels from the left edge
                if (I .EQ. 9) x_coord = x_coord - 25
                bret = TextOut(gBitMapInfo%hMemDC, x_coord, 5, labelText, len_trim(labelText))
            end do
        
            ! 5. Cleanup: Restore old pen and delete the one created
            hOldPen = SelectObject(gBitMapInfo%hMemDC, hOldPen)
            bret = DeleteObject(hAxisPen)
        
            if (invalidate) then
                ! Invalidate the window rectangle to trigger a redraw
                bret = InvalidateRect(ghwndMain, LOC(gBitMapInfo%rect), 0) ! 0 means do not erase background
                bret = UpdateWindow(ghwndMain)
            endif
        
    end subroutine DrawVerticalLines

        !> This routine draws horizonal (latitudinal) lines for the longitudinal hovmoller plot. Remember that the longitudinal
        !> Hovmoller plots the values along a specified north-south longitude as a function of time where time is along the x-axis
        !> so the plot comes from the left. These lines show the latitude positions on the longitudinal line.
        !> THere is also an optional special case to draw the horizontal lines for a weather station time series plot.
        !> The special case is needed because the weather station time series plot y-values gives the values of a field
        !> and not longitude or latitude.
        !! @param: invalidate - if set to true the screen is redrawn.
        !!         The option is present as the caller may do the redraw somewhere else
        !! @param: labelsOnLeft - when true, the labels are drawn on the left side of the plot, otherwise they are drawn on the right
        !! @param: color - ABGR color value of the labels
        !! @param: weatherStn - optional weather station data structure. If present, the horizontal lines are drawn for the 
        !!                      weather station time series plot which involves fields with different values.
        module subroutine DrawHorizontalLines(invalidate, labelsOnLeft, color, weatherStn)
            use IFWBASE ! Contains RGB macro
            USE IFWINTY
            USE IFWINA
        
            use NLevQGModelGlobals, only: gBitMapInfo, ghwndMain
            use ConstantsModule, only: HOV_DIAG_AT_LON, WEATHER_STN, VERT_HOV_AT_WEATHER_STN, ZONAL_WAVE_ENERGY
            use ModelParamsModule, only: gModIntParams, gContourIntervals
            
            IMPLICIT NONE
            
            LOGICAL, intent(in) :: invalidate
            LOGICAL, intent(in) :: labelsOnLeft
            INTEGER(4), intent(in) :: color
            type(WeatherStation), optional, intent(in) :: weatherStn
        
            INTEGER(BOOL) :: bret
            integer(HANDLE) :: hOldPen, hAxisPen
            integer(4) :: I, Y_coord, offset, X_coord
            integer :: N    ! Number of horizonatal lines
            type(T_POINT) :: oldPoint
            real :: value, topValue
            character(len=32) :: labelText
            character(len=16) :: units
        
            ! 2. Create and select a pen (dark gray, 1 pixels wide)
            hAxisPen = CreatePen(PS_SOLID, 1, AXIS_COLOR)
            hOldPen = SelectObject(gBitMapInfo%hMemDC, hAxisPen)
            
            ! Align text so the coordinate (x,y) is the top-left of the text
            bret = SetTextColor(gBitMapInfo%hMemDC, color)
            bret = SetTextAlign(gBitMapInfo%hMemDC, IOR(TA_LEFT, TA_TOP))
            bret = SetBkMode(gBitMapInfo%hMemDC, TRANSPARENT)
        
            ! 4. Draw horizontal lines
            N = 9
            units = ""
            if (present(weatherStn) .AND. gModIntParams%plotType == WEATHER_STN) then
                bret = SetTextColor(gBitMapInfo%hMemDC, weatherStn%gDCcolor)
                if (weatherStn%field /= PLOTTING_OFF) then
                    topValue = weatherStn%range
                    units = weatherStn%units
                else
                    return
                endif
            else if (gModIntParams%plotType == HOV_DIAG_AT_LON) then
                units = CHAR(176)//"lat"
            else if (gModIntParams%plotType == VERT_HOV_AT_WEATHER_STN) then
                units = " mb"
            endif
            offset = 10
            if (labelsOnLeft) then
                X_coord = 5
            else
                X_coord = gBitMapInfo%width - 100
            endif
            
            if (gModIntParams%plotType == ZONAL_WAVE_ENERGY) then
                N = gModIntParams%KMAX2
                offset = 2
            endif

            ! Draw the horizontal lines for the y-axis values
            do I = 1, N
                Y_coord = (I - 1) * (gBitMapInfo%height - 1) / (N - 1)
                ! Move to the start of the line: X=0, Y=calculated height
                bret = MoveToEx(gBitMapInfo%hMemDC, 0, I * gBitMapInfo%height / (N - 1), oldPoint)
                ! Draw to the end of the line: X=width, Y=calculated height
                bret = LineTo(gBitMapInfo%hMemDC, gBitMapInfo%width, I * gBitMapInfo%height / (N - 1))
                labelText = ""
                    ! 2. Draw the label (assuming latitude is calculated based on I)
                    if (gModIntParams%plotType == HOV_DIAG_AT_LON) then
                        value = 90. - real(I - 1) * 90.0 / real(N - 1)
                    else if (gModIntParams%plotType == VERT_HOV_AT_WEATHER_STN) then
                        value = gPressure(gBitMapInfo%height - Y_coord)
                    else if (gModIntParams%plotType == ZONAL_WAVE_ENERGY) then
                        value = REAL(gModIntParams%KMAX2 - 1) * (1.0 - REAL(I - 1) / REAL(N - 1)) + 1.0
                    else if (gModIntParams%plotType == WEATHER_STN) then
                        value = topValue - real(I - 1) * topValue * .25   ! Here the assumption is that there are N lines and the screen height is cut up
                                                                            ! into N-1 sections, but the 0 value is at the center. So if the topValue is 600
                                                                            ! the lower value is -600. So 600 is cut up into (N-1)/2 positive sections. For
                                                                            ! N = 9 that is 4 positive and 4 negative sections. 1/4 = .25.
                    endif
                    ! This 'T12' forces the units to start at the 12th character of the string
                    write(labelText, '(F10.3, T12, A)') value, trim(units)
                ! TextOut(hdc, x, y, string, length)
                ! Placing label at X=5 pixels from the left edge
                if (I .EQ. N) offset = -15
                bret = TextOut(gBitMapInfo%hMemDC, X_coord, Y_coord + offset, labelText, len_trim(labelText))
            end do
        
            ! 5. Cleanup: Restore old pen and delete the one created
            hOldPen = SelectObject(gBitMapInfo%hMemDC, hOldPen)
            bret = DeleteObject(hAxisPen)
        
            if (invalidate) then
                ! Invalidate the window rectangle to trigger a redraw
                bret = InvalidateRect(ghwndMain, LOC(gBitMapInfo%rect), 0) ! 0 means do not erase background
                bret = UpdateWindow(ghwndMain)
            endif
        
        end subroutine DrawHorizontalLines

        module subroutine DrawLineOnScreen(rowColumn, direction)
            USE IFWINTY
            USE IFWINA
            use ConstantsModule, only: VERTICAL, HORIZONTAL
            use NLevQGModelGlobals, only: gBitMapInfo, ghwndMain

            implicit none
            
            integer, intent(in) :: rowColumn, direction

            integer(BOOL) :: bret
            integer(HANDLE) :: hOldPen, hAxisPen
            type(T_POINT) :: oldPoint
            
            ! 1. Create and select a pen (dark gray, 1 pixels wide)
            hAxisPen = CreatePen(PS_SOLID, 1, AXIS_COLOR)
            hOldPen = SelectObject(gBitMapInfo%hMemDC, hAxisPen)

            ! 2. Draw line based on the direction specified
            if (direction == VERTICAL) then
                ! Move to the start of the line: X=calculated width, Y=0
                bret = MoveToEx(gBitMapInfo%hMemDC, rowColumn, 0, oldPoint)
                ! Draw to the end of the line: X=calculated width, Y=height
                bret = LineTo(gBitMapInfo%hMemDC, rowColumn, gBitMapInfo%height)
            else if (direction == HORIZONTAL) then
                ! Move to the start of the line: X=0, Y=calculated height
                bret = MoveToEx(gBitMapInfo%hMemDC, 0, rowColumn, oldPoint)
                ! Draw to the end of the line: X=width, Y=calculated height
                bret = LineTo(gBitMapInfo%hMemDC, gBitMapInfo%width, rowColumn)
            endif
            ! 3. Cleanup: Restore old pen and delete the one created
            hOldPen = SelectObject(gBitMapInfo%hMemDC, hOldPen)
            bret = DeleteObject(hAxisPen)

        end subroutine DrawLineOnScreen


        !> This subroutine labels what the current plot is.
        !> TODO: Make a legend to show what field the colors are for the time series
        !! @param: invalidate - when true, the screen is forced to update.
        module subroutine labelPlotsAndDisplayTime(invalidate)
            USE IFWINTY
            USE IFWINA
            use ConstantsModule, only: WEATHER_MAP, PHASE_AND_MAP, VERT_CROSS_SEC_AT_LAT, HOV_DIAG_AT_LAT, VERT_CROSS_SEC_AT_LON, &
                HOV_DIAG_AT_LON, PHASE_SPACE, WEATHER_STN, VERT_HOV_AT_WEATHER_STN, ZONAL_WAVE_ENERGY, NO_PLOT, PLOTTING_OFF, RAD_TO_DEGREES
            use NLevQGModelGlobals, only: gBitMapInfo, ghwndMain
            use ModelParamsModule, only: gModIntParams, gFieldLabels, gModRealParams
            use ModelFieldsModule, only: gVertArr

            implicit none
            
            LOGICAL, intent(in) :: invalidate

            INTEGER(BOOL) :: bret
            integer(HANDLE) :: hOldPen, hAxisPen, hBrush
            character(len=64) :: labelText
            character(len=128) :: infoText
            character(len=128) :: elapsedTime
            integer(4) :: I, Y_coord, X_coord, actualBgColor, Y_temp, X_temp
            logical :: useTransparent
            type(T_SIZE) :: textSize


            ! 2. No pen needed for Text
            if (gModIntParams%plotType == HOV_DIAG_AT_LAT) then
                bret = SetTextColor(gBitMapInfo%hMemDC, DARKEST_TEXT)
                X_coord = 25
            else 
                bret = SetTextColor(gBitMapInfo%hMemDC, GRAY_TEXT)
                X_coord = 200
            endif
            ! Align text so the coordinate (x,y) is the top-left of the text
            bret = SetTextAlign(gBitMapInfo%hMemDC, IOR(TA_LEFT, TA_TOP))

            Y_coord = gBitMapInfo%height - 100
            !if (gModIntParams%plotType == STATISTICS) then
            !    labelText = "Running Mean Standard Deviation:"
            !    Y_temp = Y_coord + 20
            !    call DrawLegendItem(gBitMapInfo%hMemDC, X_coord, Y_temp, STD_DEV_HEIGHT_COLOR, "Heights standard deviation")
            !    Y_temp = Y_temp + 20 ! Step down for next item
            !    call DrawLegendItem(gBitMapInfo%hMemDC, X_coord, Y_temp, STD_DEV_TEMP_COLOR, "Temperature standard deviation")
            !end if
            if (gModIntParams%plotType == WEATHER_STN) then
                write(labelText, '(A, F0.2, A, F0.2)') "Weather Station time series at lon: ", &
                                gModRealParams%lonPoint * RAD_TO_DEGREES, " lat: ", gModRealParams%latPoint * RAD_TO_DEGREES
                write(infoText, '(A, F0.3)') "Level ", gVertArr%LVS(gModIntParams%LEVP)
                Y_temp = Y_coord + 20
                if (gWeatherStation1%field /= PLOTTING_OFF) then
                    call DrawLegendItem(gBitMapInfo%hMemDC, X_coord, Y_temp, gWeatherStation1%gDCcolor, gFieldLabels(gWeatherStation1%field)%s)
                    Y_temp = Y_temp + 20 ! Step down for next item
                endif
                if (gWeatherStation2%field /= PLOTTING_OFF) then
                    call DrawLegendItem(gBitMapInfo%hMemDC, X_coord, Y_temp, gWeatherStation2%gDCcolor, gFieldLabels(gWeatherStation2%field)%s)
                endif
                useTransparent = .true.
            endif
            if (gModIntParams%plotType == VERT_HOV_AT_WEATHER_STN) then
                write(labelText, '(A, F0.2, A, F0.2)') "Vertical hovmoller at Lon: ", gModRealParams%lonPoint * RAD_TO_DEGREES, &
                                                                            " Lat: ", gModRealParams%latPoint * RAD_TO_DEGREES
                write(infoText, '(A, A, A, A, A)') "All Levels ", &
                    " Field 1: ", gFieldLabels(gModIntParams%field1)%s, &
                    " Field 2: ", gFieldLabels(gModIntParams%field2)%s
                useTransparent = .false.
            endif
            if (gModIntParams%plotType == WEATHER_MAP) then
                labelText = "Weather map:"
                write(infoText, '(A, F0.3, A, A, A, A)') "Level ", gVertArr%LVS(gModIntParams%LEVP), &
                    " Field 1: ", gFieldLabels(gModIntParams%field1)%s, &
                    " Field 2: ", gFieldLabels(gModIntParams%field2)%s
                useTransparent = .false.
            endif
            if (gModIntParams%plotType == HOV_DIAG_AT_LAT) then
                write(labelText, '(A, F0.2)') "Hovmoller diagram at Latitude: ", gModRealParams%hovLat * RAD_TO_DEGREES
                write(infoText, '(A, F0.3, A, A, A, A)') "Level ", gVertArr%LVS(gModIntParams%LEVP), &
                    " Field 1: ", gFieldLabels(gModIntParams%field1)%s, &
                    " Field 2: ", gFieldLabels(gModIntParams%field2)%s
                useTransparent = .false.
            endif
            if (gModIntParams%plotType == VERT_CROSS_SEC_AT_LAT) then
                write(labelText, '(A, F0.2)') "Vertical Cross Section at Latitude: ", gModRealParams%hovLat * RAD_TO_DEGREES
                write(infoText, '(A, A, A, A, A)') "All Levels", &
                    " Field 1: ", gFieldLabels(gModIntParams%field1)%s, &
                    " Field 2: ", gFieldLabels(gModIntParams%field2)%s
                useTransparent = .true.
            endif
            if (gModIntParams%plotType == HOV_DIAG_AT_LON) then
                write(labelText, '(A, F0.2)') "Hovmoller diagram at Longitude: ", gModRealParams%hovLon * RAD_TO_DEGREES
                write(infoText, '(A, F0.3, A, A, A, A)') "Level ", gVertArr%LVS(gModIntParams%LEVP), &
                    " Field 1: ", gFieldLabels(gModIntParams%field1)%s, &
                    " Field 2: ", gFieldLabels(gModIntParams%field2)%s
                useTransparent = .false.
            endif
            if (gModIntParams%plotType == VERT_CROSS_SEC_AT_LON) then
                write(labelText, '(A, F0.2)') "Vertical Cross Section at Longitude: ", gModRealParams%hovLon * RAD_TO_DEGREES
                write(infoText, '(A, A, A, A, A)') "All Levels", &
                    " Field 1: ", gFieldLabels(gModIntParams%field1)%s, &
                    " Field 2: ", gFieldLabels(gModIntParams%field2)%s
                useTransparent = .true.
            endif
            if (gModIntParams%plotType == ZONAL_WAVE_ENERGY) then
                write(labelText, '(A)') "Log 10 of Zonal wave energy Spectrum"
                write(infoText, '(A)') "Integrated over All Levels"
                useTransparent = .false.
            endif
            if (gModIntParams%plotType == PHASE_AND_MAP .OR. gModIntParams%plotType == PHASE_SPACE) then
                if (gModIntParams%plotType == PHASE_AND_MAP) then
                    labelText = "Weather map and Phase Space:"
                else
                    labelText = "Phase Space:"
                endif
                write(infoText, '(A, F0.3)') "Level ", gVertArr%LVS(gModIntParams%LEVP)
                Y_temp = Y_coord + 20
                ! Converts 0x00BBGGRR (Polyline) to 0x00RRGGBB (DIB)

                if (gModIntParams%plotType == PHASE_SPACE) then
                    if (gModIntParams%field1 .NE. PLOTTING_OFF) then
                        call DrawLegendItem(gBitMapInfo%hMemDC, X_coord, Y_temp, gPhaseSpace1%gDCcolor, gFieldLabels(gPhaseSpace1%field)%s)
                        Y_temp = Y_temp + 20 ! Step down for next item
                    endif
                    if (gModIntParams%field2 .NE. PLOTTING_OFF) then
                        call DrawLegendItem(gBitMapInfo%hMemDC, X_coord, Y_temp, gPhaseSpace2%gDCcolor, gFieldLabels(gPhaseSpace2%field)%s)
                    endif
                else if (gModIntParams%plotType == PHASE_AND_MAP) then
                    if (gModIntParams%field1 .NE. PLOTTING_OFF) then
                        call DrawLegendItem(gBitMapInfo%hMemDC, X_coord, Y_temp, gPhaseSpace3%gDCcolor, gFieldLabels(gPhaseSpace3%field)%s)
                        Y_temp = Y_temp + 20 ! Step down for next item
                    endif
                    if (gModIntParams%field2 .NE. PLOTTING_OFF) then
                        call DrawLegendItem(gBitMapInfo%hMemDC, X_coord, Y_temp, gPhaseSpace4%gDCcolor, gFieldLabels(gPhaseSpace4%field)%s)
                    endif
                endif
                ! Use that sampled color to set the DC's background color
                useTransparent = .false.
            endif

            if (gModIntParams%plotType == NO_PLOT) then
                labelText = "Skipping Plot:"
                write(infoText, '(A, F0.3)') "Level ", gVertArr%LVS(gModIntParams%LEVP)
                useTransparent = .false.
            endif

            elapsedTime = ""
            write(elapsedTime, '(A, I0, A, F7.2, A)')"Time: ", gElapsedTime%years, " yrs  ", gElapsedTime%days, " days"
            ! Remove leading/trailing blanks if necessary
            elapsedTime = adjustl(elapsedTime)
            bret = GetTextExtentPoint32(gBitMapInfo%hMemDC, labelText, len_trim(labelText), textSize)

            if (useTransparent) then
                bret = SetBkMode(gBitMapInfo%hMemDC, Transparent)
            else
                actualBgColor = GetPixel(gBitMapInfo%hMemDC, X_coord + textSize%cx + 6, Y_coord)
                I = SetBkColor(gBitMapInfo%hMemDC, actualBgColor) ! I is the old background color
                bret = SetBkMode(gBitMapInfo%hMemDC, OPAQUE)
            endif
            bret = TextOut(gBitMapInfo%hMemDC, X_coord + textSize%cx + 6, Y_coord, elapsedTime, len_trim(elapsedTime))
            bret = SetBkMode(gBitMapInfo%hMemDC, Transparent) ! Now print everything left transparent
            bret = TextOut(gBitMapInfo%hMemDC, X_coord, Y_coord, labelText, len_trim(labelText))
            bret = TextOut(gBitMapInfo%hMemDC, X_coord, Y_coord - 20, infoText, len_trim(infoText))
            if (invalidate) then
                bret = InvalidateRect(ghwndMain, LOC(gBitMapInfo%rect), 0) ! 0 means do not erase background
                bret = UpdateWindow(ghwndMain)
            endif

        end subroutine labelPlotsAndDisplayTime

        module subroutine DrawLegendItem(hdc, x, y, color, label)
            USE IFWINTY
            USE IFWINA
            implicit none

            integer(HANDLE), intent(in) :: hdc
            integer, intent(in) :: x, y, color
            character(len=*), intent(in) :: label

            integer(HANDLE) :: hBrush, hOldBrush
            integer(BOOL) :: bret

            ! 1. Draw the colored box
            hBrush = CreateSolidBrush(color)
            hOldBrush = SelectObject(hdc, hBrush)
            ! Draws a 15x10 pixel color swatch
            bret = Rectangle(hdc, x, y + 2, x + 15, y + 12) 
    
            ! 2. Draw the text label next to the box
            bret = TextOut(hdc, x + 20, y, trim(label), len_trim(label))

            ! Clean up
            bret = SelectObject(hdc, hOldBrush)
            bret = DeleteObject(hBrush)
        end subroutine DrawLegendItem

end submodule AxesSubmodule