
SUBMODULE (PlotModule) InterpolateSubmodule

    CONTAINS

        MODULE SUBROUTINE Interpolate1D(R, b, isPeriodic, X)
            !------------------------------------------------------Passed variables:
            LOGICAL, intent(in) :: isPeriodic
            REAL, intent(in) :: R(:)
            REAL, intent(out) :: X(:)
            type(InterpolationBounds), intent(in) :: b
            !------------------------------------------------------INPUT VARIABLES:
            !
            !  R is input one-dimensional array
            !  b%IL is the lower bound in x in the input array
            !  b%IU is the upper bound in x in the input array
            !  b%KL is the lower x-position in the new array
            !  b%KU is the upper x-position in the new array
            !  X is the output array.

            !--------------------------- METHOD:
            !--------------------------------------------------------- Cubic Hermite Spline Interpolation Overview:
            ! Step 1: Calculate Slopes using Centered Finite Differences
            ! For each point i in your input array R, calculate the slope d(i) using your centered finite difference formula. For a uniform grid spacing
            ! d(i) = (R(i+1)-R(i-1))/2h  (Note: At the boundaries i=1 and i=IL, use one-sided differences.)
        
            ! Step 2: Define the Hermite Basis Functions
            ! For any point y(j)) falling between R(i) and R(i+1), define the relative distance t=(y(j)-x(i))/h
            ! where 0 < t < 1. The cubic polynomial is constructed using four basis functions: 

            ! h00(t) = 2t^3 - 3t^2 + 1
            ! h10(t) = t^3 - 2t^2 + t
            ! h01(t) = -2t^3 + 3t^2
            ! h11(t) = t^3 - t^2
        
            ! Step 3: Interpolate the Value
            ! The interpolated value X(j)) is the weighted sum of the values and slopes at the two surrounding points:

            !       X(j) = h00(t) * R(i) + h10(t) * h * d(i) + h01(t)*R(i+1) + h11(t) * h * d(i+1)
        
            ! The Cubic Hermite Spline value for a point at fractional distance t between R(i) and R(i+1) is:

            !       X(j) = (2t^3-3t^2+1)*R(i) + (t^3-2t^2+t)h*d(i) + (-2t^3+3t^2)*R(i+1) + (t^3-t^2)*h*d(i+1)
        
            integer :: i
            INTEGER :: IL, IU, KL, KU
            real, allocatable :: D(:) ! Array to hold derivatives
            real :: t, h, dx_ratio
            real :: h00, h10, h01, h11
            
            real :: u2, u3

            IL = MAX(1, b%IL)
            IU = MIN(SIZE(R), b%IU)
            KL = MAX(1, b%KL)
            KU = MIN(SIZE(X), b%KU)
            ! Insure there are enough points to do interpolation
            IF (IU <= IL .OR. KU <= KL) RETURN

            allocate(D(IL : IU)) ! This allocation is of size (IU-IL + 1) but D(IL) is the way to index the first point.
 
            ! Create the array of derivatives
            ! Simple centered difference for internal points
            do I = IL + 1, IU - 1
                D(I) = (R(I + 1) - R(I - 1)) / 2.0
            end do
            if (isPeriodic) then
                ! Periodic: Point IL-1 is the same as point IU
                ! Point IU+1 is the same as point IL
                D(IL) = (R(IL + 1) - R(IU))     / 2.0
                D(IU) = (R(IL)     - R(IU - 1)) / 2.0
            else
                ! Non-periodic: Use one-sided difference
                D(IL) = R(IL + 1) - R(IL)
                D(IU) = R(IU)     - R(IU - 1)
            endif
        
            !dx_target = real(KU - KL)
            !dx_input  = real(IU - IL)
            dx_ratio = real(IU - IL) / real(KU - KL)
        
            do K = KL, KU
                ! Find fractional position t in [IL, IU]
                t = IL + dx_ratio * real(k - KL)        !dx_input * (real(k - KL) / dx_target)
                
                ! 2. Using INT() is generally faster than FLOOR() for positive indices
                ! TODO: Check that KL will always be < KU. It is here, but it may not have been in the N-level program
                i = int(t)         ! i = floor(t)
                if (i .GE. IU) i = IU - 1 ! Handle boundary
                u = t - real(i)           ! Fractional distance [0, 1]
                u2 = u * u
                u3 = u2 * u
                ! Hermite Basis Functions
                h00 = 2.0 * u3 - 3.0 * u2 + 1.0
                h10 = u3 - 2.0 * u2 + u
                h01 = -2.0 * u3 + 3.0 * u2
                h11 = u3 - u2

                ! Interpolate
                X(k) = h00 * R(i) + h10 * D(i) + h01 * R(i + 1) + h11 * D(i + 1)
            end do
            deallocate(D)
        END subroutine Interpolate1D

        MODULE SUBROUTINE Interpolate2D(R, b, X)
            ! R(b%IL:b%IU, b%JL:b%JU) - Input Grid
            ! X(b%KL:b%KU, b%LL:b%LU) - Output Grid
            REAL, INTENT(IN)  :: R(:, :)
            REAL, INTENT(OUT) :: X(:, :)
            type(InterpolationBounds), intent(in) :: b


            ! Intermediate array: same width as output, but same height as input
            REAL, save,  ALLOCATABLE :: TempGrid(:,:)
            INTEGER :: i, j

            ! Only reallocate if dimensions have changed
            if (.not. allocated(TempGrid)) then
                allocate(TempGrid(b%KL : b%KU, b%JL : b%JU))
            else if (size(TempGrid, 1) .NE. (b%KU - b%KL + 1) .or. size(TempGrid, 2) .NE. (b%JU - b%JL + 1)) then
                deallocate(TempGrid)
                allocate(TempGrid(b%KL : b%KU, b%JL : b%JU))
            end if

            ! Step 1: Interpolate each ROW (Horizontal)
            ! We take a 1D slice of R (a row) and expand it into a row of TempGrid
            do j = b%JL, b%JU
                call Interpolate1D(R(b%IL : b%IU, j), b, b%isPeriodicX, TempGrid(b%KL : b%KU, j)) ! is periodic
            end do

            ! Step 2: Interpolate each COLUMN (Vertical)
            ! We take a 1D slice of TempGrid (a column) and expand it into a column of X
            do i = b%KL, b%KU
                call Interpolate1D(TempGrid(i, b%JL : b%JU), b, .false., X(i, b%LL : b%LU)) ! is not periodic
            end do

        END SUBROUTINE Interpolate2D

    end submodule InterpolateSubmodule

