module UtilitiesModule
    implicit none

    ! How to remove lines containing only whitespace: ^[\t ]+\r?\n  (Alt-e to get regular expression option)
    public RealToAsciiToReal, bytesToChar

    !-------------------------------------------- Parameters:
    integer, parameter :: LENGTH = 16
    integer, parameter :: UPPERCASE_E = 69 
    integer, parameter :: PLUS_SIGN = 43 
    integer, parameter :: MINUS_SIGN = 45 
    integer, parameter :: DECIMAL_PT = 46 
    integer, parameter :: ASCII_NINE = 57 
    integer, parameter :: SPACE = 32 
    integer, parameter :: ASCII_ZERO = 48
    integer, parameter :: COLON = 58
    integer, parameter :: ASCII_FIVE = 53

    contains

        !---------------------------------------------------------------------
        !> Program to convert floating point number to ASCII or ASCII to floating point.
        !> Letting the fortran read statement do the ascii to real. That is easy.
        !>
        !> The hard work is creating a ascii string from a real with a wide range of values.
        !> This conversion is done by brute force using bytes.
        !>
        !>  If the value of realValue is less than E-03 or greater than E+07 then the real to
        !>  ASCII conversion will be made as an Exponential string, otherwise it
        !>  will be written in straight decimal form
        !>
        !! @param conversionType:Type of conversion:
        !!    0: ASCII to Floating Point; input is str, output is realValue
        !!    1: Floating Point to ASCII; input is realValue, output is str
        !! @param realValue: The floating point number to convert when conversionType is 1 (floating point to ascii)
        !! @param str: The ASCII string.
        function RealToAsciiToReal(conversionType, realValue, str) result(ios)
            use ConstantsModule, only: ASCII_TO_REAL

            real, intent(inout) :: realValue
            character(len=LENGTH), intent(inout) :: str
            integer, intent(in) :: conversionType

            integer :: ios

            !-------------------------------------------- Some handy ASCII codes
            !    E=69
            !    e=101
            !    +=43
            !    -=45
            !    .=46
            !-------------------------------------------- Other variables:
            integer(kind=1) string(LENGTH)
            INTEGER I, J, K, L, M, N, IB, IA, IE, ICAR, JDIF
            real R

            ios = 0

            !-------------------------- If ASCII string to REAL number
            IF( conversionType .EQ. ASCII_TO_REAL) THEN
                read(str, *, iostat = ios) realValue
                if (ios .NE. 0) return
            ENDIF

            !--------------------------- Real to ASCII
            IF(realValue .EQ. 0.) THEN
                DO I = 1, LENGTH
                    string(I) = SPACE
                end do
                string(2) = ASCII_ZERO
                string(3) = DECIMAL_PT
                string(4) = ASCII_ZERO
                str = bytesToChar(string)
                RETURN
            ENDIF
            !------------------------------------------------Set sign of mantissa
            IF(realValue .LT. 0.) string(1) = MINUS_SIGN
            IF(ABS(realValue) .GT. 1.E+07 .OR. ABS(realValue) .LT. 1.E-03) THEN
                !------------------------------------------------Conversion for E format
                string = ASCII_ZERO     ! Fortran shortcut to set an array of integers
                string(1) = SPACE

                string(10) = UPPERCASE_E
                string(11) = PLUS_SIGN
                string(1) = SPACE
                string(2) = DECIMAL_PT
                IF(realValue .LT. 0.) string(1) = MINUS_SIGN
                IF(realValue .EQ. 0.) then
                    str = bytesToChar(string)
                    RETURN
                endif
                !-------------------------------------------------Get power of 10:
                R = LOG10(ABS(realValue))
                !-------------------------------------------------Get sign of Exp:
                I = INT(R)
                IF(I .GT. 0) I = I + 1
                IF(I .LT. 0) string(11) = MINUS_SIGN
                !-------------------------------------------------Get the mantissa part *10**7
                R = R - REAL(I - 7)
                !-------------------------------------------------Convert to digits
                R = 10.**R
                M = INT(R)
                !----------------------------------Rounding here:
                IF(M .GT. 1000000)THEN
                    M = M + 5
                !--------------------------------Set the last digit to zero:
                    M = 10 * (M / 10)
                    IF(M .GE. 10000000)THEN
                        I = I + 1
                        M = M / 10
                    ENDIF
                ENDIF
                !-------------------------------------------------Convert Exp to ASCII
                string(12) = IABS(I) / 10 + string(12)
                string(13) = MOD(IABS(I), 10) + string(13)
                string(14) = 0
                !----------------Special case where mantissa is zero:
                L = 10
                K = 7
            else
                !------------------------------------------------Set string to blanks:

                string = SPACE      ! fortran shortcut to set an array of integers

                !------------------------------------------------Conversion for F format
                M = IABS(INT(realValue))
                N = M
                J = LENGTH
                string(J) = DECIMAL_PT
                DO
                    J = J - 1
                    IF(M .LE. 0) EXIT
                    string(J) = MOD(M, 10) + ASCII_ZERO
                    M = M / 10
                END DO
                !---------------------------Find out how many places were written:
                JDIF = LENGTH - J
                !-------------------------Move them to the beginning of string after sign place:
                DO I = 1, JDIF
                    string(I + 1) = string(J + I)
                    string(J + I) = SPACE
                end do
                !-------------------------------------Compute number of places to make 7 digits:
                K = 9 - JDIF
                IF(K .LE. 0) K = 3
                L = JDIF + K + 2
                M = INT((ABS(realValue) - REAL(N)) * 10.**K)
            end if

            DO I = 1, K
                string(L - I) = MOD(M, 10) + ASCII_ZERO
                M = M / 10
            end do
            !--------------------------Round string and remove trailing zeros:
            J = 1
            ICAR = 1
            IF(string(L - J) .LT. ASCII_FIVE) ICAR = 0
            string(L - J) = SPACE
            !------------------------------------------------Go to previous
            do
                J = J + 1
                !------------------------------------------------Decimal point?
                IF(string(L - J) .EQ. DECIMAL_PT) EXIT
                !------------------------------------------------Otherwise round up
                string(L - J) = string(L - J) + ICAR
                !------------------------------------------------Less than 10 return
                IF(string(L - J) .LT. COLON .AND. string(L - J) .GT. ASCII_ZERO) then
                    str = bytesToChar(string)
                    RETURN
                endif
                ICAR = 0
                IF(string(L - J) .EQ. COLON) ICAR = 1
                !------------------------------------------------Otherwise set to blanks
                string(L - J) = SPACE
            END DO
            !------------------------------------------------Round above decimal point
            do
                J = J + 1
                IF(L - J .EQ. 1) then
                    str = bytesToChar(string)
                    RETURN
                endif
                string(L - J) = string(L - J) + ICAR
                IF(string(L - J) .NE. COLON) then
                    str = bytesToChar(string)
                    RETURN
                endif
                string(L - J) = ASCII_ZERO
            end do
        END function RealToAsciiToReal

        function bytesToChar(string) result(str)
            implicit none
            integer(kind=1), intent(in) :: string(LENGTH)
            character(:), allocatable :: str
            character(len=LENGTH) :: str1
            integer :: i

            ! Convert ASCII codes to characters
            do i = 1, size(string)
                str1(i:i) = char(string(i))
            end do

            ! Allocate result string with trimmed length + 1 for CHAR(0)
            allocate(character(len=len_trim(str1) + 1) :: str)
            str = trim(str1) // char(0)
        end function bytesToChar

!============================= FILES
        !> Opens a file for reading or writing, with checks for existence and write permissions as needed. 
        !> If justOpen is true, it will open the file if it exists but will not prompt to overwrite if it does not.
        !> If justOpen is false, it will prompt to overwrite if the file exists and will check for write permissions.
        !> The open is done with 'NEWUNIT' which lets the OS pick the unit number or file handle, typcially a number
        !> less than -10. We initialize -1 to indicate that the file has not been opened. When closed, we set the 
        !> file handle back to -1.
        !> Files are opened as streamed and binary. The 'STREAM' is good for saving data and then rereading it.
        !! @param fileInfo: A FileStruct containing the filename and file handle to open.
        !! @param justOpen: If true, the function will only attempt to open the file if it exists and will not prompt
        !!                 for overwriting. If false, it will prompt to overwrite if the file exists and check for write permissions.
        !! @param hDlg: Handle to the dialog for displaying message boxes.
        function openModelFile(fileInfo, justOpen, hDlg) result(status)
            use IFWIN
            use ModelParamsModule, only: FileStruct
            implicit none

            type(FileStruct), intent(inout) :: fileInfo
            logical, intent(in)             :: justOpen
            integer(HANDLE), optional, intent(in)     :: hDlg

            integer :: status, ios, response
            logical :: fileExists, isOpened
            character(len=10) :: write_status

            status = 0

            ! 1. Clean up existing connection to this unit if it's already open
            inquire(unit=fileInfo%fileHandle, opened=isOpened)
            if (isOpened) close(fileInfo%fileHandle)

            ! 2. Check for existence only if we might need to prompt for overwrite
            if (.not. justOpen) then
                inquire(file=trim(fileInfo%filename), exist=fileExists)
                if (fileExists) then
                    if (PRESENT(hDlg)) then
                        response = MessageBox(hDlg, "File exists. Overwrite?"//char(0), &
                                              "Confirm"//char(0), IOR(MB_YESNO, MB_ICONQUESTION))
                        if (response /= IDYES) then
                            status = 1
                            return
                        end if
                        ! Check if file allows writing
                        INQUIRE(FILE=trim(fileInfo%filename), WRITE = write_status)     ! Prevent a runtime crash
                        if (write_status == 'NO') then
                            response = MessageBox(hDlg, "File is read only so we cannot write to it."//char(0), "Read-only file"//char(0), MB_OK)
                            status = 1
                            return
                        endif
                    endif 
                end if
            end if

            ! 3. Open the file
            ! STATUS='UNKNOWN' handles both "Open existing" and "Create new" automatically.
            open(newunit   = fileInfo%fileHandle, &
                 file   = trim(fileInfo%filename), &
                 status = 'UNKNOWN', &
                 access = 'STREAM', &
                 form   = 'UNFORMATTED', &
                 iostat = ios)

            if (ios /= 0 ) then
                if (PRESENT(hDlg)) response = MessageBox(hDlg, "Error opening file."//trim(fileInfo%filename) //char(0), "Error"//char(0), MB_OK)
                status = 1

            ! If we are overwriting an existing file, ensure it is truncated to zero. Otherwise move to the start
            else if (.not. justOpen) then
                !rewind(fileInfo%fileHandle)
                ! In STREAM access, use the POS specifier to go to the start instead of REWIND
                write(fileInfo%fileHandle, pos = 1)  ! This does not truncate
                endfile(fileInfo%fileHandle)         ! This truncates making the file open for a clean start
            end if

        end function openModelFile

end module UtilitiesModule