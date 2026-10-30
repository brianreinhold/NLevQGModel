module WelfordModule

    implicit none

    public  ! Everything public
    
    type WelfordType
        integer :: n = 0    ! Current number of items
        real :: mean = 0.0  ! Current running mean
        real :: m2 = 0.0    ! Sum of squares of differences
    end type WelfordType
    
    type(WelfordType) :: gWelford

    contains

        subroutine updateStats(welfordStatsType, x)
            implicit none
        
            type(WelfordType), intent(inout) :: welfordStatsType
            real, intent(in) :: x

            real :: delta, delta2

            welfordStatsType%n = welfordStatsType%n + 1
            delta = x - welfordStatsType%mean
            welfordStatsType%mean = welfordStatsType%mean + delta / welfordStatsType%n
        
            ! Second delta uses the updated mean
            delta2 = x - welfordStatsType%mean
            welfordStatsType%m2 = welfordStatsType%m2 + delta * delta2
        end subroutine updateStats

        !> This method is called from the time stepping or file record stepping
        !> loop and this updates the WelfordType for each field so one can (if needed) compute the
        !> mean and variance up to that point. This method calls the updateStats method.
        !! @param PS input array of spectral components from model or file data
        !! @param welfordStatsArray the WelfordType for each of the spectral components PS. There is
        !!         a one-to-one correspondence between PS(I) and welfordStatsArray(I)
        subroutine updateRunStats(PS, welfordStatsArray)
            implicit none

            real, intent(in) :: PS(:)
            type(WelfordType), intent(inout) :: welfordStatsArray(:)

            integer :: i
            ! Update the statistics for each spectral component 
            do I = 1 , size(PS)
                call updateStats(welfordStatsArray(i), PS(i))
            end do
        end subroutine updateRunStats

        !> This function returns the variance given a WelfordType.
        !! @ Param welfordStatsType the WelfordType of a given field or component
        !! @ Param getSampleVar is a logical which if true does the N-1 for the sample bias.
        !! @ returns: the variance for the component or field handled by the welfordStatsType
        function getVariance(welfordStatsType, getSampleVar) result(var)
            implicit none
            
            type(WelfordType), intent(inout) :: welfordStatsType
            logical, intent(in) :: getSampleVar
            real :: var

            if (welfordStatsType%n < 2) then
                var = 0.0
            else if (getSampleVar) then
                var = welfordStatsType%m2 / (welfordStatsType%n - 1)  ! Sample variance (Bessel's correction)
            else
                var = welfordStatsType%m2 / welfordStatsType%n        ! Population variance
            end if
        end function getVariance


    end module WelfordModule
