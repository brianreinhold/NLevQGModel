
SUBMODULE (PlotModule) SvgSubmodule

    integer, parameter :: NO_BORDER = -9999999

    CONTAINS
    
        module subroutine GenerateSVG(X, width, height, contourIntInv)
            REAL, intent(in) :: X(:, :)
            integer, intent(in) :: width, height
            REAL, intent(in) :: contourIntInv

            integer, allocatable :: borders(:, :)
            integer :: fileUnit, ios
            character(len=24) :: filename
            
            allocate(borders(width, height))
    
            call CreateBorderPoints(X, width, height, contourIntInv, borders)
            
            call GenerateUniqueFilename(filename) ! Uses current time to create a unique filename
            
            open(newunit=fileUnit, file=trim(filename), status='REPLACE', form='FORMATTED', iostat=ios)
            if (ios .NE. 0) return

            ! 1. Write the opening SVG tag with the correct W3C namespace URL and viewbox integers
            ! We use the I0 format descriptor to cleanly embed the numbers.
            write(fileUnit, '(A, I0, A, I0, A)') '<svg xmlns="http://w3.org" viewBox="0 0 ', width, ' ', height, '">'
            ! Paint the background white.
            write(fileUnit, '(A)') '<rect width="100%" height="100%" fill="#FFFFFF"/>'
            
            call ExportBordersToSVG(fileUnit, borders, width, height)
            deallocate(borders)
            close(fileUnit)

        end subroutine GenerateSVG
    
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
        !! the value -9999 to indicate no border. The array X is searched through and every
        !! border point is found and then set. They will have values ranging from -N to
        !! +M depending upon the contour interval picked by the user.
        !> @param: X the 2-D input array being mapped and to be made into an SVG in dimensional form.
        !> @param: width the width of the array X
        !> @param: height the height of the array X
        !> @param: contourIntInv the inverse of the contour interval to be used to find the borders.
        !>         This value is set when the plot type is intialized.
        !> @param: borders the 2-D output array of the same dimensions as X that will be filled with
        !>         the border values. This array will be used to trace out streamlines as an SVG.
        SUBROUTINE CreateBorderPoints(X, width, height, contourIntInv, borders)
            implicit none
            !------------------------------------------------------Passed variables:
            REAL, intent(in) :: X(:, :)
            integer, intent(in) :: width, height
            REAL, intent(in) :: contourIntInv
            integer, intent(out) :: borders(:, :)

            integer :: i, j, j1, iVal, iValRight, iValDown, integerScale

            borders = NO_BORDER         ! NO_BORDER indicates a non border point background
            integerScale = nint(100 / contourIntInv)  ! Scale factor to convert back to actual value
            
            ! 2. Calculate borders by comparing current pixel to its neighbors.
            ! We stop at Width-1 and Height-1 to prevent "out of bounds" on the iVal checks.
            ! Mapping X_Overlay to the upper half of the screen
            do J = 1, height - 1
                ! J1 maps Screen Top (J=1) to Array North (height)
                J1 = height - J + 1
                do I = 1, width - 1

                    ! Calculate interval bins
                    iVal      = floor(X(I, J1) * contourIntInv)
                    iValRight = floor(X(I + 1, J1) * contourIntInv)
                    iValDown  = floor(X(I, J1 - 1) * contourIntInv)

                    ! If boundary found, set the border value
                    if (iVal .NE. iValRight .OR. iVal .NE. iValDown) then
                        ! Preserve the actual value of the contour border point.
                        borders(I, J) = max(iVal, iValRight, iValDown) * integerScale
                    end if
                end do
            end do
        END subroutine CreateBorderPoints

        subroutine ExportBordersToSVG(fileUnit, borders, width, height)
            integer, intent(in)    :: fileUnit
            integer, intent(inout) :: borders(:, :) ! Must be inout so we can clear points
            integer, intent(in)    :: width, height
    
            integer :: x, y, cx, cy, nx, ny, i
            logical :: found_next

            ! 8-way directional pixel offsets to find connected line segments
            integer :: dx(8) = [ 0, 1, 1, 1, 0,-1,-1,-1]
            integer :: dy(8) = [-1,-1, 0, 1, 1, 1, 0,-1]
    
            ! Sweep the screen rows from top to bottom
            do y = 1, height
                do x = 1, width
                    ! Found an untraced contour border element!
                    if (borders(x, y) .NE. NO_BORDER) then
                        ! Start a new SVG vector path
                        ! You can optionally use borders(x, y) here to look up a unique color
                        write(fileUnit, '(A)', advance='no') '<polyline data-contour-val="', &
                            real(borders(cx,cy)) * 0.01, '" stroke="#000000" stroke-width="1.0" fill="none" points="'

                        cx = x
                        cy = y

                        ! Follow the line until it hits an edge or terminates
                        do
                            ! Stream out the current pixel coordinate to the SVG string
                            write(fileUnit, '(I0,A,I0,A)', advance='no') cx, ',', cy, ' '

                            ! Clear the point to the background value so we don't look at it again
                            borders(cx, cy) = NO_BORDER

                            found_next = .false.

                            ! Search the immediate 8-way neighborhood for a continuation pixel
                            do i = 1, 8
                                nx = cx + dx(i)
                                ny = cy + dy(i)

                                ! Protect against array boundary overflows
                                if (nx >= 1 .and. nx <= width .and. ny >= 1 .and. ny <= height) then
                                    if (borders(nx, ny) .NE. NO_BORDER) then
                                        cx = nx
                                        cy = ny
                                        found_next = .true.
                                        exit ! Break inner neighborhood loop, step to next line point
                                    end if
                                end if
                            end do

                            ! If the line ends or runs off the screen edge, wrap up this polyline
                            if (.not. found_next) exit
                        end do

                        ! Close the SVG XML element
                        write(fileUnit, '(A)') '" />'

                    end if
                end do
            end do
        end subroutine ExportBordersToSVG

    end submodule SvgSubmodule

