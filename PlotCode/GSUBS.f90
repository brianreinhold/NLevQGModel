module GSUBSModule
    use PboxModule

    implicit none
    private
    public :: GTEXT, GBOX, MCURSOR
    
    contains

        !*************************************************************SUBROUTINE HELP:
        !SUBROUTINE HELP(IBLK, HANDLE, DATASZ, HBLK, ICOOR, IJKPTR, MXH, LD)
        !    !-------------------------------------------------------------Method:
        !    !  IBLK = Position in HBLK which gives the help block # to be displayed
        !    !  DATASZ=Size of file before location of help text
        !    !  ICOOR=Coordinates on screen in TEXT format of upper left hand corner
        !    !  IJKPTR=Pointer to start of requested HELP text.
        !    !  MXH=Maximum number of help blocks
        !    !  HANDLE=File handle for *.FRM or HELP file text
        !    !  LD=Location of HBLK
        !    !  There will be one 2-d array which has the position of the start of the help
        !    !  text in the file *.FRM; real, integer, and F-key.
        !    !  There will be a second 2-d array which will have the help block size.
        !    INTEGER(KIND = 1), intent(in) :: IBLK, ICOOR(1), HBLK(1), IEDIT
        !    INTEGER(KIND = 2), intent(in) :: HANDLE, DATASZ
        !    INTEGER, intent(in) :: IJKPTR(1), MXH, LD
        !!------------------------------------------------------File operating variables:
        !    INTEGER WHERE4, IBYTE4, I4, IEOF4, INXT4, IDIFF4
        !!---------------------------------------------------ASCII/numb conv, buffers:
        !    INTEGER*1 MSGI(128), BUF(6144), HBUF(6144)
        !    INTEGER*2 DAT
        !    INTEGER*1 RSTRNG(13,30)
        !    INTEGER*1 ISTRNG(6,30)
        !    COMMON/$LOAD1/MSGI,BUF,HBUF,ISTRNG,RSTRNG,DAT
        !!------------------------------------------------------------Input variables:
        !!  KSTAT=Key status
        !!  ICODE=ASCII code of key pressed
        !!  ISCAN=Scan code of key pressed
        !!  MX,MY=Text character position on graphics screen of mouse cursor
        !!  MCX,MCY,MDY=Mouse cursor position on screen in pixels (MDY= 0 at bottom)
        !!  MPRESS=Number of presses of mouse button
        !!  MBSTAT=Status of mouse button
        !!  IBUT=Button pushed -1=none 0=Left 1=right
        !!  MOUSE=Mouse indicator
        !!
        !    INTEGER*1 ISCAN,ICODE,MOUSE
        !    INTEGER*2 KSTAT,MX,MY,MCX,MCY,MDY,MPRESS,MBSTAT,IBUT
        !    COMMON/$INPUT/KSTAT,ICODE,ISCAN,MX,MY,MCX,MCY,MDY,MPRESS,MBSTAT,IBUT,MOUSE
        !!-------------------------------------------------------Graph/text variables:
        !    INTEGER*2 ICT,ISPACE,IHGHT,NCOLOR,IXRES,IYRES,ICARD
        !    COMMON/$TEXTD/ICT,ISPACE,IHGHT,NCOLOR,IXRES,IYRES,ICARD
        !!-------------------------------------------------------Color variables:
        !    INTEGER*2 ICEXT,ICHLP,ICVAR,ICICN,ICFNC
        !    COMMON/$COLOR/ICEXT,ICHLP,ICVAR,ICICN,ICFNC
        !!---------------------------------------------------Other variables:
        !    INTEGER*2 IBFF,N1,IH,IHOW,IBYTES,IBYTER,IX,IY,IXH,IYH,IXYOLD,IFRM,IBUFSZ,I,IX1,IY1
        !    character(128) :: errMsg
        !
        !!---------------------------------------------------Program start:
        !    IEDIT=0
        !    IBFF=4096
        !    N1=1
        !!---------------------------------------------------Get help block number:
        !50    IH=HBLK(IBLK)
        !!-------------------------------------Move file pointer to start of help block:
        !    IHOW=0
        !    WHERE4=IJKPTR(IH)+DATASZ
        !    CALL FPOINT(HANDLE,WHERE4,IHOW,IBYTE4)
        !!----------------------------------------------Read in ICOOR bytes into buffer:
        !    IX=ICOOR(2*IH-1)
        !    IY=ICOOR(2*IH)
        !    IBYTES=IX*IY
        !!-------------------------------------------------------No help file yet?
        !    IF(IBYTES.NE.0)CALL FREAD(HANDLE,IBYTES,BUF,IBYTER)
        !!-------------------------------------------------------Draw background for text:
        !    IFRM=ISPACE
        !!-----------------------------------Center box:
        !    IXH=(IXRES-IX*ISPACE)/2
        !    IYH=(IYRES-IY*IHGHT)/2
        !!-----------------------------------Erase old cursor:
        !100    errMsg = MCURSOR(MCX, MCY)
        !!----------------------------------------------Draw help box
        !    errMsg = GBOX(IFRM,IXH,IYH,IX,IY,N1,ICHLP)
        !!----------------------------------------------Set new cursor
        !    CALL MCURSOR(MCX,MCY)
        !!-------------------------------------------------------Dump text to screen:
        !    IF(IBYTES.NE.0)CALL GTEXT(IXH,IYH,IX,IY,ICT,BUF)
        !!-------------------------------------------------------Have we come from edit?
        !    IF(IEDIT.EQ.1) GO TO 110
        !!****************************************************Wait for keyboard input:
        !    CALL INPUT(IXH,IYH)
        !    IEDIT=0
        !!**************************************************** Edit options
        !    IF(ISCAN.EQ.105)THEN
        !!---------------------------------------Option to change help block assignments:
        !       CALL GHLPAS
        !!---------------------------------------Write to file:
        !       GO TO 250
        !       ENDIF
        !!----------------------------------------------------Edit help block:
        !    IF(ISCAN.NE.104)RETURN
        !110    IXYOLD=IX*IY
        !!----------------------------------------------------Edit the help block:
        !    CALL GEDIT(IXH,IYH,IX,IY,BUF)
        !    IEDIT=0
        !    IF(IX.EQ.0.OR.IY.EQ.0)GO TO 100
        !!--------------------------------------------No save of edits if IX<0
        !    IF(IX.LT.0)THEN
        !        IX=-IX
        !        GO TO 100
        !        ENDIF
        !!--------------------------------------------Redraw screen and reedit if IXH=-2:
        !    IF(IXH.LT.0)THEN
        !        IXH=-IXH
        !        IEDIT=1
        !        ENDIF
        !!----------------------------------------------------Change of help buffer size:
        !    IDIFF4=IX*IY-IXYOLD
        !!-----------------------------------------------------Find current EOF in bytes:
        !    IEOF4=IJKPTR(MXH)+ICOOR(2*MXH)*ICOOR(2*MXH-1)+DATASZ
        !!---------------------------------------------------Find start of next help buf:
        !    INXT4=IJKPTR(IH)+IXYOLD+DATASZ
        !!--------------------------------------------------Size of file portion to move:
        !    IBYTE4=IEOF4-INXT4
        !!--------------------------------------------------Move from end or INXT4?
        !    IF(IDIFF4.NE.0)THEN
        !!------------------------------------File larger, move from end of file
        !       IF(IDIFF4.GT.0)THEN
        !        WHERE4=IEOF4
        !        IBUFSZ=IBFF
        !        ENDIF
        !!------------------------------------File smaller,move from next block
        !       IF(IDIFF4.LT.0)THEN
        !        WHERE4=INXT4
        !        IBUFSZ=-IBFF
        !        ENDIF
        !!------------------------------------------------------Take IBFF byte hunks:
        !200       IF(IBYTE4.GT.IBFF)THEN
        !        IBYTE4=IBYTE4-IBFF
        !        WHERE4=WHERE4-IBUFSZ
        !        I4=WHERE4
        !!---------------------------------------------File smaller 1st pass is at INXT4:
        !        IF(IDIFF4.LT.0)I4=I4+IBUFSZ
        !!-------------------------------------------------------Move FP relative to beg:
        !        CALL FPOINT(HANDLE,I4,IHOW,I4)
        !!--------------------------------------------------------Read IBFF bytes:
        !        CALL FREAD(HANDLE,IBFF,HBUF,IBYTER)
        !!------------------------------------------------------Move FP back
        !        CALL FPOINT(HANDLE,I4,IHOW,I4)
        !!-------------------------------------------------------Move up IDIFF4:
        !        I4=I4+IDIFF4
        !        CALL FPOINT(HANDLE,I4,IHOW,I4)
        !!------------------------------------------------------Write HBUF to file:
        !        CALL FWRITE(HANDLE,IBFF,HBUF,IBYTER)
        !        GO TO 200
        !        ENDIF
        !!---------------------------------------------------Move the last hunk:
        !       IF(IDIFF4.GT.0)WHERE4=WHERE4-IBYTE4
        !!---------------------------------------------------Move file pointer
        !       CALL FPOINT(HANDLE,WHERE4,IHOW,I4)
        !       I=IBYTE4
        !!----------------------------------------------------Read last hunk
        !       CALL FREAD(HANDLE,I,HBUF,IBYTER)
        !!----------------------------------------------------Return file pointer
        !       CALL FPOINT(HANDLE,WHERE4,IHOW,I4)
        !!-----------------------------------------------------Move up IDIFF4:
        !       I4=WHERE4+IDIFF4
        !       CALL FPOINT(HANDLE,I4,IHOW,I4)
        !!------------------------------------------------------Write data
        !       CALL FWRITE(HANDLE,I,HBUF,IBYTER)
        !       ENDIF
        !!-----------------------------------------------------Now write edited data
        !    I=IXYOLD+IDIFF4
        !    WHERE4=INXT4-IXYOLD
        !    CALL FPOINT(HANDLE,WHERE4,IHOW,I4)
        !    CALL FWRITE(HANDLE,I,BUF,IBYTER)
        !!-----------------------------------------------------Update new IX,IY values:
        !    ICOOR(2*IH-1)=IX
        !    ICOOR(2*IH)=IY
        !!--------------------------------------------Compute new offset pointers:
        !    WHERE4=0
        !    DO I=1,MXH
        !       IX1=ICOOR(2*I-1)
        !       IY1=ICOOR(2*I)
        !       IJKPTR(I)=WHERE4
        !       WHERE4=WHERE4+IX1*IY1
        !       END DO
        !!-----------------------------------------Write ICOOR, IJKPTR, and HBLK to file:
        !250    WHERE4=LD
        !    I=0
        !    CALL FPOINT(HANDLE,WHERE4,I,I4)
        !    I=7*MXH
        !    CALL FWRITE(HANDLE,I,ICOOR,IBYTER)
        !!---------------------------------------------If no size change, don't reread
        !    IF(IDIFF4.EQ.0)GO TO 100
        !!-------------------------------------Otherwise reload HBUF:
        !    GO TO 50
        !    RETURN
        !    END
    
        !!*****************************************************Graphic text conversion:
        !! This call can be replaced by a call to PlotTextArray in the PboxModule
        function GTEXT(fontParams, IX, IY, color, rowLength, msg, hWnd) result(message)
            use ifwinty
            use PboxModule
            use PlotParamsModule, only: FontParameters

            IMPLICIT NONE

            type(FontParameters), intent(in) :: fontParams
            INTEGER(SINT), intent(in) :: IX, IY
            integer*4, intent(in) :: color
            INTEGER, INTENT(IN) :: rowLength                    ! Length of each row (determined by caller)
            CHARACTER(rowLength), ALLOCATABLE, DIMENSION(:), intent(in) :: msg ! Text Array to display with deferred length
            integer(HANDLE), intent(in) :: hWnd
            
            character(128) :: message

            message = PlotTextArray(fontParams, ix, iy, color, rowLength, msg, hWnd)
        END function GTEXT
    
        !!***************************************************** Function to plot framed box:
        !> This method draws a box with an optional frame around it with a shadow and lit effect
        !> giving the frame a 3D appearance if the parameter `frameWidth` is greater than zero.
        !>
        !> If `frameWidth` is zero, this method is equivalent to calling PBOX(IX1, IY1, IX2, IY2, colorBox, hWnd)
        !>
        !> The purpose of this method is for framing text, for example help, instructions, and actions.
        !> It is also used for framing maps and graphs. 
        !>
        !> The final drawn rectangle with a non-zero frameWidth will be from:
        !> (IX1 - frameWidth - 4, IY1 - frameWidth - 4) to (IX2 + framewidth +4, IY2 + frameWidth + 4) inclusive.
        !!
        !! @param framewidth the width in pixels of the frame around the box. If 0, no frame is drawn.
        !! @param IX1 Upper-left X-coordinate in window of the box - any frame will extend left of this side
        !! @param IY1 Upper-left Y-coordinate in window of the box - any frame will extend above this side
        !! @param IX2 Lower-right X-coordinate in window inclusive of the box - any frame will extend right of this side
        !! @param IY2 Lower-right Y-coordinate in window inclusive of the box - any frame will extend below this side
        !! @param colorBox RGB color value for the box fill
        !! @param colorFrame RGB color value for the frame
        !! @param colorLit RGB color value for the 'lit' effect of the frame
        !! @param colorLit RGB color value for the 'shadow' effect of the frame
        !! @param hWnd Window handle
        function GBOX(frameWidth, IX1, IY1, IX2, IY2, colorBox, colorFrame, colorLit, colorShadow, hWnd) result(message)
            use ifwinty
            use PboxModule
        
            integer(SINT), intent(in) :: frameWidth, IX1, IY1, IX2, IY2
            integer*4, intent(in) :: colorBox, colorFrame, colorLit, colorShadow
            integer(HANDLE), intent(in) :: hWnd

            !-------------------------------------- Local variables:
            INTEGER(SINT) I, K, L1, L2, M1, M2, K1
            character(128) :: message

            K = frameWidth + 4

            !---------------------------------------------Draw frame box if frameWidth > 0
            IF(frameWidth > 0) message = PBOX(IX1 - K, IY1 - K, IX2 + K, IY2 + K, colorFrame, hWnd)

            !----------------------------------------------------Create just the box           
            message = PBOX(IX1, IY1, IX2, IY2, colorBox, hWnd) ! Overwrite the center of the frame box. Leaves a frame around the box.

            !---------------------------------------------Draw lit and shadows if frameWidth > 0
            IF(frameWidth > 0) THEN
                !------------------------------Create line frame two lines thick:
                K = frameWidth + 2
                DO I = 1, 2
                    !------------------------------Lines nearest text block first:
                    L1 = IX1 - I
                    L2 = IY1 - I
                    M1 = IX2 + I
                    M2 = IY2 + I
                    message = PLINE(L1, L2, L1, M2, colorLit, hWnd)
                    message = PLINE(L1, M2, M1, M2, colorLit, hWnd)
                    message = PLINE(M1, M2, M1, L2, colorShadow, hWnd)
                    message = PLINE(M1, L2, L1, L2, colorShadow, hWnd)
                    !-----------------------------Lines outside the frame body next:
                    L1 = IX1 - I - K
                    L2 = IY1 - I - K
                    M1 = IX2 + I + K
                    M2 = IY2 + I + K
                    message = PLINE(L1, L2, L1, M2, colorShadow, hWnd)
                    message = PLINE(L1, M2, M1, M2, colorShadow, hWnd)
                    message = PLINE(M1, M2, M1, L2, colorLit, hWnd)
                    message = PLINE(M1, L2, L1, L2, colorLit, hWnd)
                END DO
                !-----------------------------Create the shadow:
                K = frameWidth + 5
                K1 = K + frameWidth
            ENDIF
        END function GBOX

        !*********************************************Mouse cursor drawing routine:
        function MCURSOR(IX, IYY, color, hWnd) result(message)
            use ifwinty
            use PboxModule

            INTEGER(SINT), intent(in) :: IX, IYY
            INTEGER*4, intent(in) :: color
            INTEGER(HANDLE), INTENT(IN) :: hWnd

            INTEGER(SINT) :: I, IX2, IY2, IY
            character(128) :: message
    
            ! Removing the inverting of the y coordinate gWndHeight  IY = gWndHeight - IYY
            IY = IYY
            IX2 = IX - 8
            IY2 = IY - 8
            DO I = 1, 17
               message = PLOT(INT(IX2 + I, SINT), IY, color, hwnd)
               message = PLOT(IX2 + I, IY + 1, color, hwnd)
               message = PLOT(IX + 1, IY2 + I, color, hwnd)
               message = PLOT(IX, IY2 + I, color, hwnd)
            END DO
        END function MCURSOR

!!************************************General Input Routine
!    SUBROUTINE INPUT(IX,IY)
!    INTEGER*2 IX,IY
!!----------------------------------------Passed parameters:
!!  IX,IY=Coordinates of upper left hand corner of box in pixels to be used to
!!  convert cursor position into text block position
!!  IX<0 inverts sign, but exits after call without waiting for input
!!  IY<0 inverts sign, but does not print clock.
!!--------------------------------------------------Passed in COMMON
!!  KSTAT=Key status
!!  ICODE=ASCII code of key pressed
!!  ISCAN=Scan code of key pressed
!!  MX,MY=Text character position on graphics screen of mouse cursor
!!        relative to input box IX,IY
!!  MCX,MCY=Mouse cursor position on screen in pixels (y-coordinate 0 at top)
!!  MDY=y-cursor in pixels 0 at bottom
!!  MPRESS=Number of presses of mouse button
!!  MBSTAT=Status of mouse button
!!  IBUT=Button pushed -1=none 0=Left 1=right
!!
!    INTEGER*1 ICLK,MSG(8)
!    INTEGER*1 ISCAN,ICODE,MOUSE
!    INTEGER*2 MX,MY,MCX,MCY,MDY,MPRESS,MBSTAT,IBUT
!    COMMON/$INPUT/KSTAT,ICODE,ISCAN,MX,MY,MCX,MCY,MDY,MPRESS,MBSTAT,IBUT,MOUSE
!    INTEGER*2 ICT,ISPACE,IHGHT,NCOLOR,IXRES,IYRES,ICARD
!    COMMON/$TEXTD/ICT,ISPACE,IHGHT,NCOLOR,IXRES,IYRES,ICARD
!    INTEGER*2 IX1,IY1,N1,N4,N5,N8,N14,KSTAT,KX,KY
!    IY1=IY
!    IX1=IX
!    IF(IX.LT.0)IX1=-IX
!    IF(IY.LT.0)IY1=-IY
!!---------------------------------------------Read keyboard:
!100    CALL LKEY(KSTAT,ICODE,ISCAN,MSG)
!    IF(ISCAN.EQ.0.AND.ICODE.EQ.0)THEN
!!---------------------------------------------Print out clock in upper left:
!       IF(ICLK.NE.MSG(8))THEN
!          IF(IY.GE.0.AND.IX.GE.0)THEN
!           N1=1
!           N4=4
!           N5=5
!           N8=8
!           N14=14
!           CALL GBOX(N1,N4,N5,N8,N1,N1,N14)
!           CALL GTEXT(N4,N5,N8,N1,ICT,MSG)
!           ENDIF
!        ENDIF
!       ICLK=MSG(8)
!!------------------------------------------------------------Check mouse:
!       IF(MOUSE.EQ.0)THEN
!        IF(IX.GE.0)GO TO 100
!        RETURN
!        ENDIF
!!--------------------------------------------Poll mouse:
!       CALL MOUSEK(KX,KY,MPRESS,MBSTAT,IBUT)
!!-------------------------------------------No button wait; read data and go
!       IF(IX.LT.0)THEN
!          IF(KX.EQ.0.AND.KY.EQ.0.AND.IBUT.LT.0)RETURN
!        GO TO 10
!        ENDIF
!!--------------------------------------------Mouse moved or button pushed?
!       IF(KX.EQ.0.AND.KY.EQ.0.AND.IBUT.LT.0)GO TO 100
!!--------------------------------------------Erase current mouse cursor:
!10       CALL MCURSOR(MCX,MCY)
!       MCX=MCX+KX
!       MCY=MCY-KY
!       MDY=IYRES-MCY-1
!!--------------------------------------------Test limits:
!       IF(MCX.GE.IXRES)MCX=0
!       IF(MCX.LT.0)MCX=IXRES-1
!       IF(MCY.GE.IYRES)MCY=0
!       IF(MCY.LT.0)MCY=IYRES-1
!!--------------------------------------------Draw new cursor
!       CALL MCURSOR(MCX,MCY)
!!-----------------------------------Convert to text position relative to box
!       MX=(MCX-IX1)/ISPACE
!       MY=(MCY-IY1)/IHGHT
!       IF(IX.LT.0.OR.IBUT.GE.0)RETURN
!       GO TO 100
!       ENDIF
!    RETURN
!    END
    end module GSUBSModule
