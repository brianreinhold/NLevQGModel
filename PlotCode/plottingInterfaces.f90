module plottingInterfaces
    implicit none
    private
    public :: PBOX, PCLEAR    
   
    !-------------------- PBOX
    INTERFACE
        SUBROUTINE PBOX(IX1, IY1, IX2, IY2, color, hWnd, hLogWnd)
            USE IFWINTY  ! Windows types (HANDLE, etc.)
            IMPLICIT NONE
            INTEGER(SINT), INTENT(IN) :: IX1, IY1, IX2, IY2
            INTEGER*4, INTENT(IN) :: color
            INTEGER(HANDLE), INTENT(IN) :: hWnd, hLogWnd
        END SUBROUTINE PBOX
    END INTERFACE
    
    !-------------------- PCLEAR
    INTERFACE
        SUBROUTINE PCLEAR(color, hWnd, hLogWnd)
            USE IFWINTY ! Windows types (HANDLE, etc.)
            IMPLICIT NONE
            INTEGER*4, INTENT(IN) :: color
            INTEGER(HANDLE), INTENT(IN) :: hWnd, hLogWnd
        END SUBROUTINE PCLEAR
    END INTERFACE
    
end module plottingInterfaces