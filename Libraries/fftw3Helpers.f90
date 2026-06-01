! There was a great deal of pain to find out which FFTW sets of plans could transform our a(k,l)coskxsinly + b(k,l)sinkxsinly + c(l)cosly
! spectral components to grid space and back again correctly. The challenge is that the x-direction is periodic and the only FFTW
! plans that will work require complex input arrays even though our components are all real. The y-direction is bounded and requires
! bounded real-only transforms sometimes called DST (Discrete Sine Transform) or DCT (Discrete Cosine Transform). There are several 
! r2r (real-to-real) plans in FFTW that can do this but each plan places the grid points in different locations on the y-axis. But the
! big problem is that some of the FFTW_REDFTxx and FFTW_RODFTxx plans do not place the y-points at the same location. So your cosly
! and sinly points are not the same. That really sucks. The only set of plans that IS the same is FFTW_REDFT01 and FFTW_RODFT01. These
! both place the grid points at y(j) = pi*(j + 1/2)/Ny for j = 0 to Ny-1. 
! In this case if there are Ny grid points, the boundaries at y=0 and y=pi are not included in the grid. All Ny points are
! interior points. To do that, the first point is shifted by 1/2 an index. So if I have Ny points, the first y value is at 
! y = (1 - 1/2) * 180 /Ny. So for fortran indexing starting at 1, y(j) = sin(pi*(j-0.5)/Ny) for j = 1 to Ny. 
! An example of Ny = Nx = 4 is given below for a spectral component a(k,l) = 1.0 (Note the normalization factor for a 2-d
! array is 4, so we need to multiply the result by 4.)
!       1.530734  0.000000 -1.530734  0.000000      sin(pi* .5) * 180 / 4) = sin(22.5) = 0.382683.      4 * 0.382683 = 1.530734
!       3.695518  0.000000 -3.695518  0.000000      sin(pi* 1.5) * 180 / 4) = sin(67.5) = 0.9238795.    4 * 0.9238795 = 3.695518
!       3.695518  0.000000 -3.695518  0.000000
!       1.530734 -0.000000 -1.530734 -0.000000
! Here is an example where Ny = Nx = 7
!       0.890084  0.554958 -0.198062 -0.801938 -0.801938 -0.198062  0.554958    sin(pi* .5) * 180 / 7) = sin(12.86) = 0.2225209.      4 * 0.2225209 = 0.890084
!       2.493959  1.554958 -0.554958 -2.246979 -2.246979 -0.554958  1.554958
!       3.603875  2.246979 -0.801938 -3.246979 -3.246979 -0.801938  2.246979
!       4.000000  2.493959 -0.890084 -3.603875 -3.603875 -0.890084  2.493959
!       3.603875  2.246979 -0.801938 -3.246979 -3.246979 -0.801938  2.246979
!       2.493959  1.554958 -0.554958 -2.246979 -2.246979 -0.554958  1.554958
!       0.890084  0.554958 -0.198062 -0.801938 -0.801938 -0.198062  0.554958
! The test code and the DODADX(a) and DODADY(a) subroutines have now been updated to use the normalized value of the spectral component. Thus
! the peak amplitude should be 1 in the test cases in test_transforms().

! For the inverse discrete transforms (grid to spectral), the plan type is FFTW_REDFT10. So FFTW_REDFT10 is the inverse of FFTW_REDFT10.
! For the sinly the inverse discrete transform is FFTW_RODFT10.
! The normalization factors are also different. If we normalize the spectral components when going from spectral to grid, we will get the 
! correct values in the grid. Going back we have to normalize by 1/(Ny), then the

module Fftw3HelpersModule
    use, intrinsic :: iso_c_binding
    implicit none

    ! Handles for the mixed-mode (Periodic X, Bounded Y - Sin LY) transforms
    type(c_ptr) :: plan_x_c2r_1d = c_null_ptr ! X-dir C2R (Spectral to Grid). This uses a complex input and real output
    type(c_ptr) :: plan_x_r2c_1d = c_null_ptr ! X-dir R2C (Grid to Spectral). This uses a real input and complex output
    
    type(c_ptr) :: plan_y_c2r_1d = c_null_ptr ! Y-dir C2R (Spectral to Grid). This uses a complex input and real output
    type(c_ptr) :: plan_y_r2c_1d = c_null_ptr ! Y-dir R2C (Grid to Spectral). This uses a real input and complex output
    
    type(c_ptr) :: plan_c2r_2d = c_null_ptr ! Two-d C2R (Spectral to Grid). This uses a complex input and real output
    type(c_ptr) :: plan_r2c_2d = c_null_ptr ! Two-d R2C (Grid to Spectral). This uses a real input and complex output
    
    type(c_ptr) :: plan_y_c2c_1d = c_null_ptr 


    type(c_ptr) :: plan_y_FFTW_REDFT10 = c_null_ptr     ! Needed for Grid to Spectral for FFTW_REDFT01 spectral to grid
    type(c_ptr) :: plan_y_FFTW_REDFT01 = c_null_ptr     ! The only cosly plan that will line up the sinly grid points correctly in y
    
    type(c_ptr) :: plan_y_FFTW_REDFT00 = c_null_ptr     ! The cosly plan that is good for the jacobian grid to spectral for cosLy


    type(c_ptr) :: plan_y_FFTW_RODFT10 = c_null_ptr     ! Needed for Grid to Spectral for FFTW_RODFT01 spectral to grid
    type(c_ptr) :: plan_y_FFTW_RODFT01 = c_null_ptr     ! The only sinly plan that will line up the cosly grid points correctly in y


    ! ... (Keep all your normalization factors) ...
    REAL :: norm_fwd = 1 / 4.0   ! Normalization for Spectral -> Grid for wave part
    REAL :: norm_cosly = 1 / 2.0 ! Normalization for a cosly zonal transform
    REAL :: norm_sinly = 1 / 2.0 ! Normalization for a sinly zonal transform
    REAL :: norm_inv_cosly = 1   ! Normalization for a cosly zonal inv transform
    REAL :: norm_inv_sinly = 1   ! Normalization for a sinly zonal inv transform
    REAL :: norm_inv_wave = 1    ! Normalization for grid -> spectral for wave part

    contains

        ! New setup routine specifically for the mixed-mode transform
        subroutine setup_fftw_plans(Nx, Ny)
            use, intrinsic :: iso_c_binding
            implicit none

            include 'fftw3.f03'


            integer(c_int), intent(in) :: Nx, Ny

            ! Dummy arrays for planning, allocated with standard FORTRAN ALLOCATE
            real(C_FLOAT), allocatable :: dummy_r_1d(:)
            complex(C_FLOAT_COMPLEX), allocatable :: dummy_c_1d(:)

            ! Dummy arrays for 2d array planning
            real(C_FLOAT), allocatable :: dummy_r_2d(:, :)
            complex(C_FLOAT_COMPLEX), allocatable :: dummy_c_2d(:, :)

            ! Clean up existing plans
            if (c_associated(plan_x_c2r_1d)) call fftw_destroy_plan(plan_x_c2r_1d)
            if (c_associated(plan_x_r2c_1d)) call fftw_destroy_plan(plan_x_r2c_1d)
            if (c_associated(plan_y_c2r_1d)) call fftw_destroy_plan(plan_y_c2r_1d)
            if (c_associated(plan_y_r2c_1d)) call fftw_destroy_plan(plan_y_r2c_1d)
            
            if (c_associated(plan_y_c2c_1d)) call fftw_destroy_plan(plan_y_c2c_1d)
            
            if (c_associated(plan_c2r_2d)) call fftw_destroy_plan(plan_c2r_2d)
            if (c_associated(plan_r2c_2d)) call fftw_destroy_plan(plan_r2c_2d)

            if (c_associated(plan_y_FFTW_RODFT01)) call fftw_destroy_plan(plan_y_FFTW_RODFT01)
            if (c_associated(plan_y_FFTW_REDFT01)) call fftw_destroy_plan(plan_y_FFTW_REDFT01)
            if (c_associated(plan_y_FFTW_RODFT10)) call fftw_destroy_plan(plan_y_FFTW_RODFT10)
            if (c_associated(plan_y_FFTW_REDFT10)) call fftw_destroy_plan(plan_y_FFTW_REDFT10)

            if (c_associated(plan_y_FFTW_REDFT00)) call fftw_destroy_plan(plan_y_FFTW_REDFT00)

            ! Set normalization factors: 1/(Nx*Ny) total normalization
            norm_inv_wave = 2.0 / (REAL(Nx) * REAL(Ny))
            norm_inv_cosly = 1.0 / real(Ny)
            norm_inv_sinly = 1.0 / real(Ny)


            ! Allocate dummy arrays matching the size of the 1D buffers used during execution
            allocate(dummy_r_1d(max(Nx, Ny))) ! Need size Nx for X output, size Ny for Y input/output
            allocate(dummy_c_1d(Nx + 1))    ! Need size Nx+1 for X input/output
            
            ! Allocate dummy 2D arrays for the 2D plans.
            allocate(dummy_r_2d(Nx, Ny))
            allocate(dummy_c_2d((Nx/2 + 1), Ny))
            ! --- Plan 2D C2R (Spectral to Grid) ---
            plan_c2r_2d = fftwf_plan_dft_c2r_2d(Ny, Nx, dummy_c_2d, dummy_r_2d, FFTW_MEASURE)

            ! --- Plan 2D R2C (Grid to Spectral) ---
            plan_r2c_2d = fftwf_plan_dft_r2c_2d(Ny, Nx, dummy_r_2d, dummy_c_2d, FFTW_MEASURE)


            
            
            ! --- Plan 1D C2C (Spectral to grid) ---
            plan_y_c2c_1d = fftwf_plan_dft_1d(Ny, dummy_c_1d, dummy_c_1d, FFTW_FORWARD, FFTW_MEASURE)

            ! --- Plan 1D C2R (Spectral to Grid, X-direction) ---
            plan_x_c2r_1d = fftwf_plan_dft_c2r_1d(Nx, dummy_c_1d, dummy_r_1d, FFTW_MEASURE)

            ! --- Plan 1D R2C (Grid to Spectral, X-direction) ---
            plan_x_r2c_1d = fftwf_plan_dft_r2c_1d(Nx, dummy_r_1d, dummy_c_1d, FFTW_MEASURE)
            
            ! --- Plan 1D C2R (Spectral to Grid, Y-direction) ---
            plan_y_c2r_1d = fftwf_plan_dft_c2r_1d(Ny, dummy_c_1d, dummy_r_1d, FFTW_MEASURE)

            ! --- Plan 1D R2C (Grid to Spectral, Y-direction) ---
            plan_y_r2c_1d = fftwf_plan_dft_r2c_1d(Ny, dummy_r_1d, dummy_c_1d, FFTW_MEASURE)


            plan_y_FFTW_REDFT10 = fftwf_plan_r2r_1d(Ny, dummy_r_1d, dummy_r_1d, FFTW_REDFT10, FFTW_MEASURE) ! Grid to spectral for FFTW_REDFT01 spectral to grid
            plan_y_FFTW_REDFT01 = fftwf_plan_r2r_1d(Ny, dummy_r_1d, dummy_r_1d, FFTW_REDFT01, FFTW_MEASURE)
            
            ! Grid to spectral r2r for Jacobian cosLy projections
            plan_y_FFTW_REDFT00 = fftwf_plan_r2r_1d(Ny/2 + 1, dummy_r_1d, dummy_r_1d, FFTW_REDFT00, FFTW_MEASURE)

            plan_y_fftw_rodft10 = fftwf_plan_r2r_1d(ny, dummy_r_1d, dummy_r_1d, fftw_rodft10, fftw_measure)
            plan_y_fftw_rodft01 = fftwf_plan_r2r_1d(ny, dummy_r_1d, dummy_r_1d, fftw_rodft01, fftw_measure)


            deallocate(dummy_r_1d)
            deallocate(dummy_c_1d)
            deallocate(dummy_r_2d)
            deallocate(dummy_c_2d)


        end subroutine setup_fftw_plans


        subroutine test_transforms()
            use ArrayDimensionsModule, only: gCurrentDims
            use ModelFieldsModule, only: gTransArr
            use ModelParamsModule, only: gModIntParams
            use, intrinsic :: iso_c_binding, only : C_FLOAT_COMPLEX
            use NLevQGModelGlobals, only: LogMessage

            implicit none

            include 'fftw3.f03'

            integer :: I, K, L
            real :: temp_var
            character(len=256) :: msg

            ! ========================= Print out the grid points for a single component coskxsinly for k=1,l=1
            call LogMessage("Test for spectral component coskxsinly + cosly for k,l = 1,1")
            gTransArr%G2 = 0.0    ! Fortran short cut to zero an entire array of all dimensions. This is the output array for the transform
            DO L = 1, gModIntParams%LMAX2
                ! Calculate loop-invariant values outside the inner loop
                gTransArr%GcZ = CMPLX(0.0, 0.0, kind=C_FLOAT_COMPLEX)  ! This is the input array for the transform
                if (L .EQ. 1) gTransArr%GcZ(2) = CMPLX( norm_fwd, norm_fwd, kind=C_FLOAT_COMPLEX)

                call fftwf_execute_dft_c2r(plan_x_c2r_1d, gTransArr%GcZ, gTransArr%G2(:, L))
            END DO
            ! Now transform the sinly part which is bounded and uses r2r transforms column by column
            DO I = 1, gCurrentDims%IYMAX
                ! Pass the column slice directly
                call fftwf_execute_r2r(plan_y_FFTW_RODFT01, gTransArr%G2(I, :), gTransArr%G2(I, :))
            END DO

            ! Print out the grid points
            call LogMessage("Wave part coskxsinly + sinkxsinly")
            do L = 1, gCurrentDims%IYMAX
                write(msg, '(*(F9.6, 1X))') (gTransArr%G2(K, L), K = 1, gCurrentDims%IXMAX)
                call LogMessage(msg)
            end do

            ! Zonal test part. These test the various r2r plans to see where the y grid points are placed.
            ! As is turns out, only the REDFT01 and RODFT01 place the grid points at the same y-values for the
            ! sinly and cosly transforms.

            ! --------------- FFTW_REDFT11 and FFTW_RODFT11 plans
            !gTransArr%GZonal = 0. ! Fortran shortcut to zero an entire array
            !gTransArr%GZonal(2) = 1
            !call fftwf_execute_r2r(plan_y_FFTW_REDFT11, gTransArr%GZonal, gTransArr%GZonal)
            !call logZonal("Zonal case FFTW_REDFT11")
            !
            !gTransArr%GZonal = 0. ! Fortran shortcut to zero an entire array
            !gTransArr%GZonal(1) = 1
            !call fftwf_execute_r2r(plan_y_FFTW_RODFT11, gTransArr%GZonal, gTransArr%GZonal)
            !call logZonal("Zonal case FFTW_RODFT11")

            ! --------------- FFTW_REDFT01 and FFTW_RODFT01 plans
            gTransArr%GZonal = 0. ! Fortran shortcut to zero an entire array
            gTransArr%GZonal(1) = norm_sinly                      ! Note that the sinly transform does not have the l=0 mode
            call fftwf_execute_r2r(plan_y_FFTW_RODFT01, gTransArr%GZonal, gTransArr%GZonal)
            call logZonal("Zonal case FFTW_RODFT01")

            gTransArr%GZonal = 0. ! Fortran shortcut to zero an entire array
            gTransArr%GZonal(2) = norm_cosly                      ! Note that the cosly transform has the l=0 mode
            call fftwf_execute_r2r(plan_y_FFTW_REDFT01, gTransArr%GZonal, gTransArr%GZonal)
            call logZonal("Zonal case FFTW_REDFT01")

            ! --------------- FFTW_REDFT10 and FFTW_RODFT10 plans
            !gTransArr%GZonal = 0. ! Fortran shortcut to zero an entire array
            !gTransArr%GZonal(2) = 1
            !call fftwf_execute_r2r(plan_y_FFTW_REDFT10, gTransArr%GZonal, gTransArr%GZonal)
            !call logZonal("Zonal case FFTW_REDFT10")
            !
            !gTransArr%GZonal = 0. ! Fortran shortcut to zero an entire array
            !gTransArr%GZonal(1) = 1
            !call fftwf_execute_r2r(plan_y_FFTW_RODFT10, gTransArr%GZonal, gTransArr%GZonal)
            !call logZonal("Zonal case FFTW_RODFT10")

            ! --------------- FFTW_REDFT11 and FFTW_RODFT11 plans
            !gTransArr%GZonal = 0. ! Fortran shortcut to zero an entire array
            !gTransArr%GZonal(2) = 1
            !call fftwf_execute_r2r(plan_y_FFTW_REDFT00, gTransArr%GZonal, gTransArr%GZonal)
            !call logZonal("Zonal case FFTW_REDFT00 cosly")
            !
            !gTransArr%GZonal = 0. ! Fortran shortcut to zero an entire array
            !gTransArr%GZonal(1) = 1
            !call fftwf_execute_r2r(plan_y_FFTW_RODFT00, gTransArr%GZonal, gTransArr%GZonal)
            !call logZonal("Zonal case FFTW_RODFT00 sinly")

            ! Add the zonal part to each column in G2
            do L = 1, gCurrentDims%IYMAX
                DO K = 1, gCurrentDims%IXMAX
                    gTransArr%G2(K, L) = gTransArr%G2(K, L) + gTransArr%GZonal(L)
                end do
            end do
            ! ---------- Print the grid with both wave and zonal parts
            call LogMessage("Wave plus zonal cosy")
            do L = 1, gCurrentDims%IYMAX
                write(msg, '(*(F9.6, 1X))') (gTransArr%G2(K, L), K = 1, gCurrentDims%IXMAX)
                call LogMessage(msg)
            end do
            
            ! Separate test for just cosxcosy (only comes up in Jacobian because of the derivatives)
            call LogMessage("Test for spectral component coskxcosly")
            gTransArr%G1 = 0.0    ! Fortran short cut to zero an entire array of all dimensions. This is the output array for the transform
            DO L = 1, gModIntParams%LMAX2
                ! Calculate loop-invariant values outside the inner loop
                gTransArr%GcZ = CMPLX(0.0, 0.0, kind=C_FLOAT_COMPLEX)  ! This is the input array for the transform
                if (L .EQ. 1) gTransArr%GcZ(2) = CMPLX( norm_fwd, 0, kind=C_FLOAT_COMPLEX)

                call fftwf_execute_dft_c2r(plan_x_c2r_1d, gTransArr%GcZ, gTransArr%G1(:, L + 1))
            END DO
            ! Now transform the cosly part which is bounded and uses r2r transforms column by column
            DO I = 1, gCurrentDims%IYMAX
                ! Pass the column slice directly
                call fftwf_execute_r2r(plan_y_FFTW_REDFT01, gTransArr%G1(I, :), gTransArr%G1(I, :))
            END DO
            do L = 1, gCurrentDims%IYMAX
                write(msg, '(*(F9.6, 1X))') (gTransArr%G1(K, L), K = 1, gCurrentDims%IXMAX)
                call LogMessage(msg)
            end do

            ! =========================== Now do the INVERSE transforms
            call LogMessage("Inverse transform")
            ! Get the zonal average
            gTransArr%GZonal = 0
            temp_var = 1. / REAL(gCurrentDims%IXMAX)
            do I = 1, gCurrentDims%IYMAX
                gTransArr%GZonal(I) = 0.0
                do K = 1, gCurrentDims%IXMAX
                    gTransArr%GZonal(I) = gTransArr%GZonal(I) + gTransArr%G2(K, I)
                end do
                gTransArr%GZonal(I) = gTransArr%GZonal(I) * temp_var
            end do
            ! Print out the zonal mean to check is has cosy structure
            call logZonal("Zonal mean")
            ! Remove the zonal mean from G2 to get the wave part only
            ! Do this while we still have the zonal mean and the unaltered grid
            do L = 1, gCurrentDims%IYMAX
                DO K = 1, gCurrentDims%IXMAX
                    gTransArr%G2(K, L) = gTransArr%G2(K, L) - gTransArr%GZonal(L)
                end do
            end do
            call fftwf_execute_r2r(plan_y_FFTW_REDFT10, gTransArr%GZonal, gTransArr%GZonal)
            ! Normalize the transformed spectral components. The l=0 component is ignored
            ! but the transform generates it. So the components that will have modes 1 to LMAX2 are
            ! in 2 to LMAX2 + 1. Modes 0 and those greater than LMAX2 are ignored in our model.
            ! But in this example the l=0 and l>(LMAX2 + 1) should all be zero
            do I = 2, gModIntParams%LMAX2 + 1
                gTransArr%GZonal(I) = gTransArr%GZonal(I) * norm_inv_cosly
            end do
            call logZonal("Inverse zonal transform")

            ! Now do the wave. First the discrete sinly inverse transform
            call LogMessage("Inverse wave transform")
            DO I = 1, gCurrentDims%IXMAX
                ! Do the sinly transform 
                call fftwf_execute_r2r(plan_y_FFTW_RODFT10, gTransArr%G2(I, :), gTransArr%G2(I, :))
            END DO
            ! Now the kx part
            DO L = 1, gModIntParams%LMAX2
                ! Calculate loop-invariant values outside the inner loop
                gTransArr%GcZ = CMPLX(0.0, 0.0, kind=C_FLOAT_COMPLEX)  ! This is the input array for the transform
                call fftwf_execute_dft_r2C(plan_x_r2c_1d, gTransArr%G2(:, L), gTransArr%GcZ)
                write(msg, '(*(F9.6, 1X))') (gTransArr%GcZ(K) * norm_inv_wave, K = 1, gModIntParams%KMAX2 + 1)
                call LogMessage(msg)
            END DO
            ! Note there is no need for a sign reversal on the sinkx terms like there was in FFT991


        end subroutine test_transforms

        subroutine logZonal(str)
            use ArrayDimensionsModule
            use ModelFieldsModule, only: gTransArr
            use NLevQGModelGlobals, only: LogMessage
            implicit none
            
            CHARacter(len=*), intent(in) :: str

            integer L
            character(len=256) :: msg

            call LogMessage(str)
            write(msg, '(*(F9.6, 1X))') (gTransArr%GZonal(L), L = 1, gCurrentDims%IYMAX)
            call LogMessage(msg)

        end subroutine LogZonal

end module Fftw3HelpersModule