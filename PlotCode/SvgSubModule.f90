
SUBMODULE (PlotModule) SvgSubmodule

    integer, parameter :: NO_BORDER = -9999999
    
    ! Base colors converted cleanly from #BBGGRR to web #RRGGBB layout
    character(len=7), parameter :: SVG_RAM_COLORS(20) = [ &
        '#0000AA', & ! 0:  Was #AA0000 -> Now #0000AA (Dark Blue)
        '#0000FF', & ! 1:  Was #FF0000 -> Now #0000FF (Deep Blue)
        '#0066FF', & ! 2:  Was #FF6600 -> Now #0066FF (Blue)
        '#008DFF', & ! 3:  Was #FF8D00 -> Now #008DFF (Medium Blue)
        '#00A6FF', & ! 4:  Was #FFA600 -> Now #00A6FF (Lighter Blue)
        '#00BBE7', & ! 5:  Was #E7BB00 -> Now #00BBE7 (Cyan-Blue)
        '#00CE9A', & ! 6:  Was #9ACE00 -> Now #00CE9A (Cyan)
        '#007861', & ! 7:  Was #617800 -> Now #007861 (Light Cyan)
        '#009655', & ! 8:  Was #559600 -> Now #009655 (Green-Cyan)
        '#00B444', & ! 9:  Was #44B400 -> Now #00B444 (Green)
        '#4DD12D', & ! 10: Was #2DD14D -> Now #4DD12D (Bright Green)
        '#ABEA0D', & ! 11: Was #0DEAAB -> Now #ABEA0D (Yellow-Green)
        '#FFFF00', & ! 12: Was #00FFFF -> Now #FFFF00 (Yellow)
        '#FFC000', & ! 13: Was #00C0FF -> Now #FFC000 (Orange-Yellow)
        '#FF8000', & ! 14: Was #0080FF -> Now #FF8000 (Orange)
        '#FF0000', & ! 15: Was #0000FF -> Now #FF0000 (Red)
        '#FF0077', & ! 16: Was #7700FF -> Now #FF0077 (Magenta)
        '#FF00AA', & ! 17: Was #AA00FF -> Now #FF00AA (Lite Magenta)
        '#FF77AA', & ! 18: Was #AA77FF -> Now #FF77AA (Pink Magenta)
        '#FF77FF'  & ! 19: Was #FF77FF -> Now #FF77FF (Pink Lilac)
    ]

    ! 100% Academic Publication Compliant Grayscale Palette
    ! Strictly adheres to the 15% to 85% safe contrast boundaries
    character(len=7), parameter :: SVG_GRAY_RAM_COLORS(20) = [ &
        '#262626', & ! 0:  15.0% Gray (Strict Minimum Boundary)
        '#2F2F2F', & ! 1:  18.4% Gray
        '#383838', & ! 2:  22.0% Gray
        '#424242', & ! 3:  25.9% Gray
        '#4B4B4B', & ! 4:  29.4% Gray
        '#545454', & ! 5:  32.9% Gray
        '#5E5E5E', & ! 6:  36.9% Gray
        '#676767', & ! 7:  40.4% Gray
        '#707070', & ! 8:  43.9% Gray
        '#7A7A7A', & ! 9:  47.8% Gray
        '#838383', & ! 10: 51.4% Gray
        '#8D8D8D', & ! 11: 55.3% Gray
        '#969696', & ! 12: 58.8% Gray
        '#9F9F9F', & ! 13: 62.4% Gray
        '#A9A9A9', & ! 14: 66.3% Gray
        '#B2B2B2', & ! 15: 69.8% Gray
        '#BBBBBB', & ! 16: 73.3% Gray
        '#C5C5C5', & ! 17: 77.3% Gray
        '#CECECE', & ! 18: 80.8% Gray
        '#D8D8D8'  & ! 19: 84.7% Gray (Strict Maximum Boundary)
    ]

    CONTAINS

        module subroutine GenerateSVG()
            use ConstantsModule, only: WEATHER_MAP, PHASE_AND_MAP, VERT_CROSS_SEC_AT_LAT, VERT_CROSS_SEC_AT_LON, &
                               ZONAL_WAVE_ENERGY, VERT_HOV_AT_WEATHER_STN, HOV_DIAG_AT_LAT, HOV_DIAG_AT_LON, PLOTTING_OFF
            use ModelParamsModule, only: gModIntParams, FileStruct
            use NLevQGModelGlobals, only: gBitMapInfo
            implicit None
            
            REAL :: contourIntervalInv1, contourIntervalInv2
            type(FileStruct) :: svgFileInfo

            contourIntervalInv1 = 0.0
            contourIntervalInv2 = 0.0
            if (gModIntParams%plotType == WEATHER_MAP .OR. gModIntParams%plotType == PHASE_AND_MAP) then
                if (associated(X1)) contourIntervalInv1 = gWeatherMap1%contourIntervalInv
                if (associated(X2)) contourIntervalInv2 = gWeatherMap2%contourIntervalInv
            else if (gModIntParams%plotType == VERT_HOV_AT_WEATHER_STN) then
                if (associated(X1)) contourIntervalInv1 = gVertHovmoller1%contourIntervalInv
                if (associated(X2)) contourIntervalInv2 = gVertHovmoller2%contourIntervalInv
            else if (gModIntParams%plotType == VERT_CROSS_SEC_AT_LAT) then
                if (associated(X1)) contourIntervalInv1 = gLatVertCrossSec1%contourIntervalInv
                if (associated(X2)) contourIntervalInv2 = gLatVertCrossSec2%contourIntervalInv
            else if (gModIntParams%plotType == VERT_CROSS_SEC_AT_LON) then
                if (associated(X1)) contourIntervalInv1 = gLonVertCrossSec1%contourIntervalInv
                if (associated(X2)) contourIntervalInv2 = gLonVertCrossSec2%contourIntervalInv
            else if (gModIntParams%plotType == HOV_DIAG_AT_LAT) then
                if (associated(X1)) contourIntervalInv1 = gLatHovmoller1%contourIntervalInv
                if (associated(X2)) contourIntervalInv2 = gLatHovmoller2%contourIntervalInv
            else if (gModIntParams%plotType == HOV_DIAG_AT_LON) then
                if (associated(X1)) contourIntervalInv1 = gLonHovmoller1%contourIntervalInv
                if (associated(X2)) contourIntervalInv2 = gLonHovmoller2%contourIntervalInv
            else if (gModIntParams%plotType == ZONAL_WAVE_ENERGY) then
                if (associated(X1)) contourIntervalInv1 = gEnergyHovmoller1%contourIntervalInv
                ! No second field here
            endif
            if (associated(X1)) then
                call CreateSVG(X1, size(X1, dim=1), size(X1, dim=2), contourIntervalInv1, svgFileInfo, .NOT. ASSOCIATED(X2), .true.)
            endif
            if (associated(X2)) then
                call createSVG(X2, size(X2, dim=1), size(X2, dim=2), contourIntervalInv2, svgFileInfo, .TRUE. , .NOT. ASSOCIATED(X1) )
            endif

        end subroutine GenerateSVG

    
        subroutine CreateSVG(X, width, height, contourIntervalInv, svgFileInfo, closeFile, useSolidColor)
            use ModelParamsModule, only: FileStruct

            implicit none

            REAL, intent(in) :: X(:, :)
            integer, intent(in) :: width, height
            REAL, intent(in) :: contourIntervalInv
            type(FileStruct), intent(inout) :: svgFileInfo
            logical, intent(in) :: closeFile, useSolidColor

            integer, allocatable :: borders(:, :)
            integer :: ios, binMin,  binMax, i
            character(len=7) :: fillColor
            character(len=32) :: filename


            allocate(borders(width, height))

            call CreateBorderPoints(X, width, height, contourIntervalInv, borders)

            !call DebugPrintFirst20Rows("BordersFile.txt", borders, width, height)

            if (svgFileInfo%fileHandle .EQ. -1) then
                call GenerateUniqueFilename(svgFileInfo%fileName) ! Uses current time to create a unique filename
            
                open(newunit=svgFileInfo%fileHandle, file=trim(svgFileInfo%fileName), status='REPLACE', form='FORMATTED', iostat=ios)
                if (ios .NE. 0) return

                ! 1. Write the opening SVG tag with the correct W3C namespace URL and viewbox integers
                ! We use the I0 format descriptor to cleanly embed the numbers.
                write(svgFileInfo%fileHandle, '(A, I0, A, I0, A)') '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ', width, ' ', height, '">'
                ! Paint the background white.
                write(svgFileInfo%fileHandle, '(A)') '<rect width="100%" height="100%" fill="#FFFFFF"/>'
            endif
            if (useSolidColor) then
                do i = 1, 20
                    binMin = i - 11
                    binMax = i - 10
                    fillColor = SVG_GRAY_RAM_COLORS(i)
                    call FillContourBand(svgFileInfo%fileHandle, X, width, height, contourIntervalInv, binMin, binMax, trim(fillColor))
                end do
                call ExportBordersToSVG(svgFileInfo%fileHandle, borders, width, height, "#000000", '')
            else
                !call ExportBordersToSVG(svgFileInfo%fileHandle, borders, width, height, "#000000", 'stroke-dasharray="6,4"')
                call ExportBordersToSVG(svgFileInfo%fileHandle, borders, width, height, "#000000", '')
            endif
            deallocate(borders)
            
            if (closeFile) then
                write(svgFileInfo%fileHandle, '(A)') '</svg>'
                close(svgFileInfo%fileHandle)
            endif

    end subroutine CreateSVG
    
    
    
!subroutine DebugPrintFirst20Rows(filename, borders, width, height)
!    implicit none
!    character(len=*), intent(in) :: filename
!    integer, intent(in)          :: width, height
!    integer, intent(in)          :: borders(width, height)
!    
!    integer :: u, ios, x, y
!    
!    open(newunit=u, file=trim(filename), status='REPLACE', form='FORMATTED', iostat=ios)
!    if (ios /= 0) return
!    
!    write(u, '(A)') 'DISPLAYING BORDERS ARRAY: FIRST 40 ROWS (Y=1 to 40)'
!    write(u, '(A)') 'Focusing on X = 900 to 1400 where the diagonal error occurs'
!    write(u, '(A)') '----------------------------------------------------------------------'
!    
!    ! 1. Loop through exactly the first 20 rows of your screen array
!    do y = 1, 40
!        write(u, '(A,I3,A)', advance='no') 'Row ', y, ': '
!        
!        ! 2. Focus strictly on the X-width coordinates around your broken polylines
!        do x = 900, 1400
!            
!            if (borders(x, y) == NO_BORDER) then
!                ! Print a clean visual dot for background space
!                write(u, '(A)', advance='no') '    .'
!            else
!                ! Print the actual cell contents scaled down (e.g., 0)
!                ! Using I5 ensures everything stays perfectly aligned like a grid
!                write(u, '(I5)', advance='no') borders(x, y) / 100
!            end if
!            
!        end do
!        ! Line break at the end of every row
!        write(u, *)
!    end do
!    
!    close(u)
!end subroutine DebugPrintFirst20Rows
    
    
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
            !logical :: cross_right, cross_down

            borders = NO_BORDER         ! NO_BORDER indicates a non border point background
            integerScale = nint(100 / contourIntervalInv)  ! Scale factor to convert back to actual value
            
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

! TODO: Make that loop around dx a subroutine. It is repeated 4 times!!
        subroutine ExportBordersToSVG(fileUnit, borders, width, height, strokeColor, lineStyle)
            integer, intent(in)    :: fileUnit
            integer, intent(inout) :: borders(:, :) 
            integer, intent(in)    :: width, height
            character(len=*), intent(in) :: strokeColor ! Expects e.g., "#000000" or "#FF0000"
            character(len=*), intent(in) :: lineStyle   ! Expects e.g., "solid", "dashed", or "dotted"
    
            integer :: x, y, cx, cy, nx, ny, i, mx, my, j
            integer :: startX, startY, contour_val, count
            logical :: found_next
            character(len=32) :: lineType
    
            ! 8-way directional pixel offsets
            integer :: dx(8) = [ 1, 1, 0, -1, -1, -1,  0, 1]
            integer :: dy(8) = [ 0, 1, 1,  1,  0, -1, -1,-1]


            ! Sweep rows from top to bottom
            ! SVG coordinates are like screen coordinates; 0,0 is the top left.
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
                        if (.NOT. found_next) then
                            borders(cx, cy) = NO_BORDER
                            cycle
                        endif
                        cx = startX
                        cy = startY
                        !write(*, '(A, I0, A, I0, A, I0, A, I0)') 'Forward: x = ', x, ' y = ', y, ' StartX = ', startX, ' startY = ', startY

                        lineType = ''
                        if (contour_val < 0) lineType = 'stroke-dasharray="6,4"'
                        write(fileUnit, '(A, F0.2, A, A, A, A, A)', advance='no') '<polyline data-contour-val="', real(contour_val) * 0.01, &
                                        '" stroke="', trim(strokeColor), '" stroke-width="1.0" fill="none" ', trim(lineType), ' points="'

                        count = 0
                        do
                            write(fileUnit, '(I0, A, I0, A)', advance='no') cx, ',', cy, ' '
                            if (cx .EQ. startX .and. cy .EQ. startY) then
                                ! Have we completed a loop - kill if so or the loop will go indefinitely
                                if (COUNT .GE. 1) then
                                    borders(cx, cy) = NO_BORDER
                                    exit
                                endif
                                count = count + 1
                            else
                                borders(cx, cy) = NO_BORDER ! Wipes points as we go but not the start point (closed contour)
                            endif
 
                            found_next = .false.
                            do i = 1, 8
                                nx = cx + dx(i)
                                ny = cy + dy(i)
                        
                                if (nx >= 1 .and. nx <= width .and. ny >= 1 .and. ny <= height) then
                                    if (borders(nx, ny) .NE. NO_BORDER) then
                                        if (cx .NE. startX .AND. cy .NE. startY) then
                                            do j = 1, 8
                                                mx = cx + dx(j)
                                                my = cy + dy(j)
                                                if (mx >= 1 .and. mx <= width .and. my >= 1 .and. my <= height) then
                                                    borders(mx, my) = NO_BORDER
                                                endif
                                            end do
                                        endif
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
                        found_next = .false.
                        do i = 8, 1, -1
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
                        if (.NOT. found_next) then
                            borders(cx, cy) = NO_BORDER
                            cycle
                        endif
                        cx = startX
                        cy = startY

                        !write(*, '(A, I0, A, I0, A, I0, A, I0)') 'Reverse: x = ', x, ' y = ', y, ' StartX = ', startX, ' startY = ', startY

                        lineType = ''
                        if (contour_val < 0) lineType = 'stroke-dasharray="6,4"'
                        write(fileUnit, '(A, F0.2, A, A, A, A, A)', advance='no') '<polyline data-contour-val="', real(contour_val) * 0.01, &
                                        '" stroke="', trim(strokeColor), '" stroke-width="1.0" fill="none" ', trim(lineType), ' points="'

                        do
                            write(fileUnit, '(I0, A, I0, A)', advance='no') cx, ',', cy, ' '
                            borders(cx, cy) = NO_BORDER ! Wipes out the anchor and backward segments
                    
                            found_next = .false.
                            do i = 8, 1, -1
                                nx = cx + dx(i)
                                ny = cy + dy(i)
                        
                                if (nx >= 1 .and. nx <= width .and. ny >= 1 .and. ny <= height) then
                                    if (borders(nx, ny) .NE. NO_BORDER) then
                                        if (cx .NE. startX .AND. cy .NE. startY) then
                                            do j = 8, 1, -1
                                                mx = cx + dx(j)
                                                my = cy + dy(j)
                                                if (mx >= 1 .and. mx <= width .and. my >= 1 .and. my <= height) then
                                                    borders(mx, my) = NO_BORDER
                                                endif
                                            end do
                                        endif
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

        subroutine FillContourBand(fileUnit, X, width, height, contourIntervalInv, binMin, binMax, fillColor)
            implicit none
            integer, intent(in)          :: fileUnit
            real, intent(in)             :: X(:,:)
            integer, intent(in)          :: width, height
            real, intent(in)             :: contourIntervalInv
            integer, intent(in)          :: binMin, binMax      ! The target band range (e.g., bin 1 to 2)
            character(len=*), intent(in) :: fillColor          ! Hex color string (e.g., "#99CCFF")
    
            integer :: x_idx, y_idx, j1
            integer :: current_bin
            integer :: spanStart
            logical :: insideSpan
    
            ! Sweep rows from top to bottom
            write(fileUnit, '(A)'), '<g shape-rendering="crispEdges">'
            do y_idx = 1, height
                j1 = height - y_idx + 1
        
                insideSpan = .false.
                spanStart  = 0
        
                do x_idx = 1, width
                    ! 1. Identify which contour bin the current data coordinate belongs to
                    current_bin = floor(X(x_idx, j1) * contourIntervalInv)
            
                    ! 2. Check if this pixel sits inside our active target shading band
                    if (current_bin >= binMin .and. current_bin < binMax) then
                        if (.not. insideSpan) then
                            ! Start a new horizontal fill ribbon segment
                            spanStart = x_idx
                            insideSpan = .true.
                        end if
                    else
                        if (insideSpan) then
                            ! The band ended -> Stream out a solid 1-pixel-tall rectangle mapping the span
                            ! width = current_x - spanStart
                            write(fileUnit, '(A, I0, A, I0, A, I0, A, A, A)') &
                                '<rect x="', spanStart, '" y="', y_idx, &
                                '" width="', (x_idx - spanStart), '" height="1" fill="', trim(fillColor), '"/>'
                            insideSpan = .false.
                        end if
                    end if
                end do
        
                ! 3. Safety Catch: If the shaded band stretches all the way to the right frame edge
                if (insideSpan) then
                    write(fileUnit, '(A, I0, A, I0, A, I0, A, A, A)') &
                        '<rect x="', spanStart, '" y="', y_idx, &
                        '" width="', (width - spanStart + 1), '" height="1" fill="', trim(fillColor), '"/>'
                end if
        
                ! Inject a hard line break every row to keep Chrome's text file parsing buffer happy
                write(fileUnit, *)
            end do
            write(fileUnit, '(A)'), '</g>'
        end subroutine FillContourBand
    

    end submodule SvgSubmodule

