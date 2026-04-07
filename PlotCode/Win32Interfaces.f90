module win32interfaces

    implicit none
    private
    public :: Rectangle

    INTERFACE
        INTEGER(C_INT) FUNCTION Rectangle(hDC, x1, y1, x2, y2) BIND(C, NAME="Rectangle")
            USE, INTRINSIC :: ISO_C_BINDING
            USE IFWIN  ! Windows API functions
            USE IFWINTY  ! Windows types (HANDLE, etc.)            
            IMPLICIT NONE            
            INTEGER(HANDLE), INTENT(IN) :: hDC  ! Device context handle
            INTEGER(C_INT), INTENT(IN) :: x1, y1, x2, y2  ! Coordinates of the rectangle
        END FUNCTION Rectangle
    END INTERFACE
    
end module win32interfaces