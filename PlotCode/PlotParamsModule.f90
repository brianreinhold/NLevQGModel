module PlotParamsModule
    use IFWINTY, only: SINT, HANDLE, T_TEXTMETRIC
    
    implicit none
    
    ! Type defintions
    ! These parameters are not directly set by the user but determined by the window size and what needs to be displayed.
    public :: FontParameters, DrawingParams, GraphAxesParams
    ! Arrays
    public :: IB        ! Contains the sizes of three boxes that are displayed in the main plot window.
    public :: IA, IAT, GRF ! Arrays used for plotting maps and time series.
    
    INTEGER*4, ALLOCATABLE :: IA(:), IAT(:)
    REAL, ALLOCATABLE :: GRF(:)
    
    ! Instances of the types (Creating fonts on the fly is fast - using them is slow)
    public :: drawParams, axesParams

    type :: FontParameters
        integer(SINT) :: direction      ! Direction of the text in degrees (-180 to 180)
        integer(HANDLE) :: hFont        ! Handle for the font
        integer(SINT) :: fontSize       ! Specifies the logical unit size of the text.
                                        !     This depends on the mapping mode and can represent:
                                        !     - Pixels (default in MM_TEXT)
                                        !     - Device-independent pixels (DIPs)
                                        !     - Points (1 point = 1/72 of a logical inch)
        character(64) :: fontName       ! Name of the font to use, e.g., "Times New Roman"
        type(T_TEXTMETRIC) :: tm        ! Text metrics structure containing information about the font
                                        !TYPE T_TEXTMETRIC
                                        !SEQUENCE
                                        !   integer(LONG) tmHeight ! The height (ascent + descent) of characters. 
                                        !   integer(LONG) tmAscent ! The ascent (units above the base line) of characters. 
                                        !   integer(LONG) tmDescent ! The descent (units below the base line) of characters. 
                                        !   integer(LONG) tmInternalLeading ! The amount of leading (space) inside the bounds set by the tmHeight member
                                                                            ! Accent marks and other diacritical characters may occur in this area. 
                                                                            ! The designer may set this member to zero. 
                                        !   integer(LONG) tmExternalLeading ! The amount of extra leading (space) that the application adds between rows. 
                                                                            ! Since this area is outside the font, it contains no marks and is not altered
                                                                            ! by text output calls in either OPAQUE or TRANSPARENT mode. The designer may 
                                                                            ! set this member to zero. 
                                        !   integer(LONG) tmAveCharWidth ! The average width of characters in the font (generally defined as the width of the letter x ).
                                                                         ! This value does not include the overhang required for bold or italic characters. 
                                        !   integer(LONG) tmMaxCharWidth ! The width of the widest character in the font. 
                                        !   integer(LONG) tmWeight ! The weight of the font (such as FW_NORMAL or FW_BOLD).
                                        !   integer(LONG) tmOverhang  ! The extra width per string that may be added to some synthesized fonts. 
                                                                      ! When synthesizing some attributes, such as bold or italic, graphics device interface (GDI) 
                                                                      ! or a device may have to add width to a string on both a per-character and per-string basis.
                                                                      ! For example, GDI makes a string bold by expanding the spacing of each character and 
                                                                      ! overstriking by an offset value; it italicizes a font by shearing the string. In either case,
                                                                      ! there is an overhang past the basic string. For bold strings, the overhang is the 
                                                                      ! distance by which the overstrike is offset. For italic strings, the overhang is the 
                                                                      ! amount the top of the font is sheared past the bottom of the font.

                                                                      ! The tmOverhang member enables the application to determine how much of the 
                                                                      ! character width returned by a GetTextExtentPoint32 function call on a single character is 
                                                                      ! the actual character width and how much is the per-string extra width. The actual width is 
                                                                      ! the extent minus the overhang.
                                        !   integer(LONG) tmDigitizedAspectX ! The horizontal aspect of the device for which the font was designed
                                        !   integer(LONG) tmDigitizedAspectY ! The vertical aspect of the device for which the font was designed. The ratio of the 
                                                                             ! tmDigitizedAspectX and tmDigitizedAspectY members is the aspect ratio of the device 
                                                                             ! for which the font was designed 
                                        !   integer(BYTE) tmFirstChar ! The value of the first character defined in the font 
                                        !   integer(BYTE) tmLastChar ! The value of the last character defined in the font. 
                                        !   integer(BYTE) tmDefaultChar ! The value of the character to be substituted for characters not in the font. 
                                        !   integer(BYTE) tmBreakChar ! The value of the character that will be used to define word breaks for text justification. 
                                        !   integer(BYTE) tmItalic ! Specifies an italic font if it is nonzero. 
                                        !   integer(BYTE) tmUnderlined ! Specifies an underlined font if it is nonzero. 
                                        !   integer(BYTE) tmStruckOut ! A strikeout font if it is nonzero.
                                        !   integer(BYTE) tmPitchAndFamily ! Specifies information about the pitch, the technology, and the family of a physical font.
                                                                           ! The four low-order bits of this member specify information about the pitch and the technology
                                                                           ! of the font. A constant is defined for each of the four bits. (There is MUCH more information)
                                        !   integer(BYTE) tmCharSet ! The character set of the font. The character set can be one of the following values:
                                                                           !   ANSI_CHARSET, BALTIC_CHARSET, CHINESEBIG5_CHARSET, DEFAULT_CHARSET, EASTEUROPE_CHARSET,
                                                                           !   GB2312_CHARSET, GREEK_CHARSET, HANGEUL_CHARSET, HEBREW_CHARSET, JOHAB_CHARSET,
                                                                           !   MAC_CHARSET, OEM_CHARSET, RUSSIAN_CHARSET, SHIFTJIS_CHARSET, SYMBOL_CHARSET,
                                                                           !   TURKISH_CHARSET, VIETNAMESE_CHARSET
                                        !END TYPE
    end type FontParameters

    type :: DrawingParams
        integer(kind=4) :: ICS    !  ICS is the RGB background color used in the PCLEAR() method
        integer(SINT) :: KL       !  KL is the lower x-position on the screen: possible values are (0 to XRES-1) where XRES is window x-size
        integer(SINT) :: KU       !  KU is the upper x-position on the screen: possible values are (0 to XRES-1)
        integer(SINT) :: LL       !  LL is the lower y-position on the screen: possible values are (0 to YRES-1) where YRES is window y-size
        integer(SINT) :: LU       !  LU is the upper y-position on the screen: possible values are (0 to YRES-1)
        integer(SINT) :: KLH
        integer(SINT) :: ML
        integer(SINT) :: MU
        integer(SINT) :: NL
        integer(SINT) :: NU
        integer(SINT) :: KLI(8)   !  KLI(8) are the left most x-positions on the screen for each of the 8 subwindows
        integer(SINT) :: KUI(8)   !  KUI(8) are the right most x-positions on the screen for each of the 8 subwindows
        integer(SINT) :: LLI(8)   !  LLI(8) are the lower most y-positions on the screen for each of the 8 subwindows
        integer(SINT) :: LUI(8)   !  LUI(8) are the upper most y-positions on the screen for each of the 8 subwindows
        integer :: MAX            !  MAX is the current number of subwindows on the screen selected by the user
        integer(SINT) :: IXRES    !  Number of pixels in X in the plot window    gWndWidth, gWndHght come from get client rectangle
        integer(SINT) :: IYRES    !  Number of pixels in Y in the plot window

                                  ! Should these be in ArrayDimensionsModule?
        integer :: KIA            !  KIA is the size of the screen buffer (drawParams%KU - drawParams%KL + 1) * (drawParams%LU - drawParams%LL + 3) + 
        integer :: KIAT           !  KIAT is maximum of (drawParams%KU - drawParams%KL + 1), (drawParams%LU - drawParams%LL + 3)
    end type DrawingParams
    
    type :: GraphAxesParams
        REAL :: XL
        REAL :: XU
        REAL :: XINT
        REAL :: YL
        REAL :: YU
        REAL :: YINT
        REAL :: XSCAL
        REAL :: YSCAL
        REAL :: X0
        REAL :: Y0
        REAL :: AT
    end type GraphAxesParams

    type(DrawingParams) :: drawParams
    type(GraphAxesParams) :: axesParams
    integer(SINT) :: IB(12)     ! An array that contains the sizes of three boxes that are displayed in the main plot window.
                                ! This naming sucks and needs to be improved.

end module PlotParamsModule