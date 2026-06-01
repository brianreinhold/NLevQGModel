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
    !type(c_ptr) :: plan_x_r2c_1d = c_null_ptr ! X-dir R2C (Grid to Spectral). This uses a real input and complex output
    
    !type(c_ptr) :: plan_y_c2r_1d = c_null_ptr ! Y-dir C2R (Spectral to Grid). This uses a complex input and real output
    type(c_ptr) :: plan_y_r2c_1d = c_null_ptr ! Y-dir R2C (Grid to Spectral). This uses a real input and complex output
    
    type(c_ptr) :: plan_c2r_2d = c_null_ptr ! Two-d C2R (Spectral to Grid). This uses a complex input and real output
    type(c_ptr) :: plan_r2c_2d = c_null_ptr ! Two-d R2C (Grid to Spectral). This uses a real input and complex output
    
    !type(c_ptr) :: plan_y_c2c_1d = c_null_ptr 


    !type(c_ptr) :: plan_y_FFTW_REDFT10 = c_null_ptr     ! Needed for Grid to Spectral for FFTW_REDFT01 spectral to grid
    type(c_ptr) :: plan_y_FFTW_REDFT01 = c_null_ptr     ! The only cosly plan that will line up the sinly grid points correctly in y
    
    type(c_ptr) :: plan_y_FFTW_REDFT00 = c_null_ptr     ! The cosly plan that is good for the jacobian grid to spectral for cosLy


    !type(c_ptr) :: plan_y_FFTW_RODFT10 = c_null_ptr     ! Needed for Grid to Spectral for FFTW_RODFT01 spectral to grid
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
            !if (c_associated(plan_x_r2c_1d)) call fftw_destroy_plan(plan_x_r2c_1d)
            !if (c_associated(plan_y_c2r_1d)) call fftw_destroy_plan(plan_y_c2r_1d)
            if (c_associated(plan_y_r2c_1d)) call fftw_destroy_plan(plan_y_r2c_1d)
            
            !if (c_associated(plan_y_c2c_1d)) call fftw_destroy_plan(plan_y_c2c_1d)
            
            if (c_associated(plan_c2r_2d)) call fftw_destroy_plan(plan_c2r_2d)
            if (c_associated(plan_r2c_2d)) call fftw_destroy_plan(plan_r2c_2d)

            if (c_associated(plan_y_FFTW_RODFT01)) call fftw_destroy_plan(plan_y_FFTW_RODFT01)
            if (c_associated(plan_y_FFTW_REDFT01)) call fftw_destroy_plan(plan_y_FFTW_REDFT01)
            !if (c_associated(plan_y_FFTW_RODFT10)) call fftw_destroy_plan(plan_y_FFTW_RODFT10)
            !if (c_associated(plan_y_FFTW_REDFT10)) call fftw_destroy_plan(plan_y_FFTW_REDFT10)

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
            !plan_y_c2c_1d = fftwf_plan_dft_1d(Ny, dummy_c_1d, dummy_c_1d, FFTW_FORWARD, FFTW_MEASURE)

            ! --- Plan 1D C2R (Spectral to Grid, X-direction) ---
            plan_x_c2r_1d = fftwf_plan_dft_c2r_1d(Nx, dummy_c_1d, dummy_r_1d, FFTW_MEASURE)

            ! --- Plan 1D R2C (Grid to Spectral, X-direction) ---
            !plan_x_r2c_1d = fftwf_plan_dft_r2c_1d(Nx, dummy_r_1d, dummy_c_1d, FFTW_MEASURE)
            
            ! --- Plan 1D C2R (Spectral to Grid, Y-direction) ---
            !plan_y_c2r_1d = fftwf_plan_dft_c2r_1d(Ny, dummy_c_1d, dummy_r_1d, FFTW_MEASURE)

            ! --- Plan 1D R2C (Grid to Spectral, Y-direction) ---
            plan_y_r2c_1d = fftwf_plan_dft_r2c_1d(Ny, dummy_r_1d, dummy_c_1d, FFTW_MEASURE)


            !plan_y_FFTW_REDFT10 = fftwf_plan_r2r_1d(Ny, dummy_r_1d, dummy_r_1d, FFTW_REDFT10, FFTW_MEASURE) ! Grid to spectral for FFTW_REDFT01 spectral to grid
            plan_y_FFTW_REDFT01 = fftwf_plan_r2r_1d(Ny, dummy_r_1d, dummy_r_1d, FFTW_REDFT01, FFTW_MEASURE)
            
            ! Grid to spectral r2r for Jacobian cosLy projections
            plan_y_FFTW_REDFT00 = fftwf_plan_r2r_1d(Ny/2 + 1, dummy_r_1d, dummy_r_1d, FFTW_REDFT00, FFTW_MEASURE)

            !PLAN_Y_FFTW_RODFT10 = fftwf_plan_r2r_1d(ny, dummy_r_1d, dummy_r_1d, FFTW_RODFT10, FFTW_MEASURE)
            plan_y_FFTW_RODFT01 = fftwf_plan_r2r_1d(ny, dummy_r_1d, dummy_r_1d, FFTW_RODFT01, FFTW_MEASURE)


            deallocate(dummy_r_1d)
            deallocate(dummy_c_1d)
            deallocate(dummy_r_2d)
            deallocate(dummy_c_2d)


        end subroutine setup_fftw_plans

end module Fftw3HelpersModule