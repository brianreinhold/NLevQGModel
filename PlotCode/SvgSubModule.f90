
SUBMODULE (PlotModule) SvgSubmodule

    integer, parameter :: NO_BORDER = -9999999

    CONTAINS

        module subroutine GenerateSVG()
            use ConstantsModule, only: WEATHER_MAP, PHASE_AND_MAP, VERT_CROSS_SEC_AT_LAT, VERT_CROSS_SEC_AT_LON, &
                               ZONAL_WAVE_ENERGY, VERT_HOV_AT_WEATHER_STN, HOV_DIAG_AT_LAT, HOV_DIAG_AT_LON, PLOTTING_OFF
            use ModelParamsModule, only: gModIntParams
            use NLevQGModelGlobals, only: gBitMapInfo
            implicit None
            
            select case(gModIntParams%plotType)

                case (WEATHER_MAP)
                    if (associated(X1)) then
                        call createSVG(X1, size(X1, dim=1), size(X1, dim=2), gWeatherMap1%contourIntervalInv)
                    endif
                    
                case (VERT_HOV_AT_WEATHER_STN)
                    if (associated(X1)) then
                        call createSVG(X1, size(X1, dim=1), size(X1, dim=2), gVertHovmoller1%contourIntervalInv)
                    endif

            end select
            
        end subroutine GenerateSVG

    
        subroutine CreateSVG(X, width, height, contourIntervalInv)
            REAL, intent(in) :: X(:, :)
            integer, intent(in) :: width, height
            REAL, intent(in) :: contourIntervalInv

            integer, allocatable :: borders(:, :)
            integer :: fileUnit, ios
            character(len=32) :: filename
            
            allocate(borders(width, height))
    
            call CreateBorderPoints(X, width, height, contourIntervalInv, borders)
            
            call GenerateUniqueFilename(filename) ! Uses current time to create a unique filename
            
            open(newunit=fileUnit, file=trim(filename), status='REPLACE', form='FORMATTED', iostat=ios)
            if (ios .NE. 0) return

            ! 1. Write the opening SVG tag with the correct W3C namespace URL and viewbox integers
            ! We use the I0 format descriptor to cleanly embed the numbers.
            write(fileUnit, '(A, I0, A, I0, A)') '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ', width, ' ', height, '">'
            ! Paint the background white.
            write(fileUnit, '(A)') '<rect width="100%" height="100%" fill="#FFFFFF"/>'
            
            call ExportBordersToSVG(fileUnit, borders, width, height)
            write(fileUnit, '(A)') '</svg>'
            deallocate(borders)
            close(fileUnit)

        end subroutine CreateSVG
    
        subroutine GenerateUniqueFilename(filename)
            implicit none
            character(len=*), intent(out) :: filename
    
            ! date_time_str will hold "YYYYMMDDHHMMSS.FFF" (18 characters)
            character(len=18) :: date_time_str
            character(len=8)  :: date_part
            character(len=10) :: time_part
    
            ! Call the standard Fortran intrinsic routine
            ! date_part receives "YYYYMMDD", time_part receives "HHMMSS.FFF"
            call date_and_time(date=date_part, time=time_part)
    
            ! Combine them into a single string, stripping out the decimal point
            date_time_str = date_part // time_part(1:6) // time_part(8:10)
    
            ! Construct the final filename string
            filename = 'plot_' // date_time_str // '.svg'
        end subroutine GenerateUniqueFilename

        !! Takes the array of interpolated values in X that are the actual values of the
        !! variable being plotted. X has the array that allows the mouse over to display
        !! the real value at that given point. This routine takes the integer array 
        !! borders of the same dimensions as X and fills it with the integer value of the 
        !! contour interval that is user-specified. The borders array is then loaded with
        !! the value NO_BORDER to indicate no border. The array X is searched through and every
        !! border point is found and then set. They will have values ranging from -N to
        !! +M depending upon the contour interval picked by the user.
        !> @param: X the 2-D input array being mapped and to be made into an SVG in dimensional form.
        !> @param: width the width of the array X
        !> @param: height the height of the array X
        !> @param: contourIntInv the inverse of the contour interval to be used to find the borders.
        !>         This value is set when the plot type is intialized.
        !> @param: borders the 2-D output array of the same dimensions as X that will be filled with
        !>         the border values. This array will be used to trace out streamlines as an SVG.
        SUBROUTINE CreateBorderPoints(X, width, height, contourIntervalInv, borders)
            implicit none
            !------------------------------------------------------Passed variables:
            REAL, intent(in) :: X(:, :)
            integer, intent(in) :: width, height
            REAL, intent(in) :: contourIntervalInv
            integer, intent(out) :: borders(:, :)

            integer :: i, j, j1, iVal, iValRight, iValDown, integerScale

            borders = NO_BORDER         ! NO_BORDER indicates a non border point background
            integerScale = nint(100 / contourIntervalInv)  ! Scale factor to convert back to actual value
            
            ! 2. Calculate borders by comparing current pixel to its neighbors.
            ! We stop at Width-1 and Height-1 to prevent "out of bounds" on the iVal checks.
            ! Mapping X_Overlay to the upper half of the screen
            do J = 1, height - 1
                ! J1 maps Screen Top (J=1) to Array North (height)
                J1 = height - J + 1
                do I = 1, width - 1

                    ! Calculate interval bins
                    iVal      = floor(X(I, J1) * contourIntervalInv)
                    iValRight = floor(X(I + 1, J1) * contourIntervalInv)
                    iValDown  = floor(X(I, J1 - 1) * contourIntervalInv)

                    ! If boundary found, set the border value
                    if (iVal .NE. iValRight .OR. iVal .NE. iValDown) then
                        ! Preserve the actual value of the contour border point.
                        borders(I, J) = max(iVal, iValRight, iValDown) * integerScale
                    end if
                end do
            end do
        END subroutine CreateBorderPoints

        !subroutine ExportBordersToSVG(fileUnit, borders, width, height)
        !    integer, intent(in)    :: fileUnit
        !    integer, intent(inout) :: borders(:, :) ! Must be inout so we can clear points
        !    integer, intent(in)    :: width, height
        !
        !    integer :: x, y, cx, cy, nx, ny, i
        !    logical :: found_next
        !
        !    ! 8-way directional pixel offsets to find connected line segments
        !    integer :: dx(8) = [ 0, 1, 1, 1, 0,-1,-1,-1]
        !    integer :: dy(8) = [-1,-1, 0, 1, 1, 1, 0,-1]
        !
        !    ! Sweep the screen rows from top to bottom
        !    do y = 1, height
        !        do x = 1, width
        !            ! Found an untraced contour border element!
        !            if (borders(x, y) .NE. NO_BORDER) then
        !                ! Start a new SVG vector path
        !                cx = x
        !                cy = y
        !                ! You can optionally use borders(x, y) here to look up a unique color
        !                write(fileUnit, '(A, F0.2, A)', advance='no') '<polyline data-contour-val="', &
        !                    real(borders(cx, cy)) * 0.01, '" stroke="#000000" stroke-width="1.0" fill="none" points="'
        !                ! Follow the line until it hits an edge or terminates
        !                do
        !                    ! Stream out the current pixel coordinate to the SVG string
        !                    write(fileUnit, '(I0, A, I0, A)', advance='no') cx, ',', cy, ' '
        !
        !                    ! Clear the point to the background value so we don't look at it again
        !                    borders(cx, cy) = NO_BORDER
        !
        !                    found_next = .false.
        !
        !                    ! Search the immediate 8-way neighborhood for a continuation pixel
        !                    do i = 1, 8
        !                        nx = cx + dx(i)
        !                        ny = cy + dy(i)
        !
        !                        ! Protect against array boundary overflows
        !                        if (nx >= 1 .and. nx <= width .and. ny >= 1 .and. ny <= height) then
        !                            if (borders(nx, ny) .NE. NO_BORDER) then
        !                                cx = nx
        !                                cy = ny
        !                                found_next = .true.
        !                                exit ! Break inner neighborhood loop, step to next line point
        !                            end if
        !                        end if
        !                    end do
        !
        !                    ! If the line ends or runs off the screen edge, wrap up this polyline
        !                    if (.not. found_next) exit
        !                end do
        !
        !                ! Close the SVG XML element
        !                write(fileUnit, '(A)') '" />'
        !
        !            end if
        !        end do
        !    end do
        !end subroutine ExportBordersToSVG

        subroutine ExportBordersToSVG(fileUnit, borders, width, height)
            integer, intent(in)    :: fileUnit
            integer, intent(inout) :: borders(:, :) 
            integer, intent(in)    :: width, height
    
            integer :: x, y, cx, cy, nx, ny, i
            integer :: startX, startY, contour_val
            logical :: found_next
    
            ! 8-way directional pixel offsets
            integer :: dx(8) = [ 0, 1, 1, 1, 0,-1,-1,-1]
            integer :: dy(8) = [-1,-1, 0, 1, 1, 1, 0,-1]
    
            ! Sweep rows from top to bottom
            do y = 1, height
                do x = 1, width
                    if (borders(x, y) .NE. NO_BORDER) then
                
                        ! Save our anchor point details before tracing clears them
                        startX = x
                        startY = y
                        contour_val = borders(x, y)
                
                        ! ==========================================================
                        ! LINE 1: TRACE FORWARD FROM THE ANCHOR
                        ! ==========================================================
                        cx = startX
                        cy = startY
                
                        write(fileUnit, '(A, F0.2, A)', advance='no') '<polyline data-contour-val="', &
                            real(contour_val) * 0.01, '" stroke="#000000" stroke-width="1.0" fill="none" points="'
                
                        do
                            write(fileUnit, '(I0, A, I0, A)', advance='no') cx, ',', cy, ' '
                            borders(cx, cy) = NO_BORDER ! Wipes points as we go
                    
                            found_next = .false.
                            do i = 1, 8
                                nx = cx + dx(i)
                                ny = cy + dy(i)
                        
                                if (nx >= 1 .and. nx <= width .and. ny >= 1 .and. ny <= height) then
                                    if (borders(nx, ny) .NE. NO_BORDER) then
                                        cx = nx
                                        cy = ny
                                        found_next = .true.
                                        exit 
                                    end if
                                end if
                            end do
                            if (.not. found_next) exit
                        end do
                        write(fileUnit, '(A)') '" />'
                
                        ! ==========================================================
                        ! LINE 2: TRACE BACKWARD FROM THE SAME ANCHOR
                        ! ==========================================================
                        ! Put the anchor point back so the second loop can find its neighbor
                        borders(startX, startY) = contour_val
                
                        cx = startX
                        cy = startY
                
                        write(fileUnit, '(A, F0.2, A)', advance='no') '<polyline data-contour-val="', &
                            real(contour_val) * 0.01, '" stroke="#000000" stroke-width="1.0" fill="none" points="'
                
                        do
                            write(fileUnit, '(I0, A, I0, A)', advance='no') cx, ',', cy, ' '
                            borders(cx, cy) = NO_BORDER ! Wipes out the anchor and backward segments
                    
                            found_next = .false.
                            do i = 1, 8
                                nx = cx + dx(i)
                                ny = cy + dy(i)
                        
                                if (nx >= 1 .and. nx <= width .and. ny >= 1 .and. ny <= height) then
                                    if (borders(nx, ny) .NE. NO_BORDER) then
                                        cx = nx
                                        cy = ny
                                        found_next = .true.
                                        exit 
                                    end if
                                end if
                            end do
                            if (.not. found_next) exit
                        end do
                        write(fileUnit, '(A)') '" />'
                
                    end if
                end do
            end do
        end subroutine ExportBordersToSVG
    

    end submodule SvgSubmodule

