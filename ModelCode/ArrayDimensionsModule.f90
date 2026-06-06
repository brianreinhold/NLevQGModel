module ArrayDimensionsModule
    implicit none
    
    public ZeroArrayDims
    public gArrayDims, gCurrentDims, Dims
    
    !  These dimensions are calulated in the method SetDimensions from the Settings type
    
    !  ITOT: is the horizontal size of the spectral arrays
    !        The spectral arrays are defined as PS(ITOT, maxLevels) instead of PS(maxZonal, maxMerid, maxLevels)
    !        So the storage in ITOT is in order with I = 1 to ITOT
    !           I = 1 to maxMeridWaves is the zonal mean components cosIy followed by
    !           I = maxMeridWaves+1 to ITOT are the wave components 
    !               K = 1 to KMAX
    !                   L = 1, LMAX cosKnxsinLy, sinsKnxsinLy, for 2*LMAX more entrants
    !   ITOT: cosly l=1, LMAX  sinknxsinly, cosknxsinly l = 1, LMAX for k=1,KMAX
    !         cosy, cos2y ... cosLMAXy  cosnxsiny,   sinnxsiny,   cosnxsin2y,   sinnxsin2y, ...   cosnxsinLMAXy,   sinnxsinLMAXy
    !                                   cos2nxsiny,  sin2nxsiny,  cos2nxsin2y,  sin2nxsin2y, ...  cos2nxsinLMAXy,  sin2nxsinLMAXy
    !                                   cos3nxsiny,  sin3nxsiny,  cos3nxsin2y,  sin3nxsin2y, ...  cos3nxsinLMAXy,  sin3nxsinLMAXyy
    !                                                            ...
    !                                   cosKMAXnxsiny, sinKMAXnxsiny, cosKMAXnxsin2y, sinKMAXnxsin2y, ... cosMAXnxsinLMAXy, sinKMAXnxsinLMAXy

    !  PS(k,l) = a(k,l)2*cos(knx)sinly + b(k,l)2*sin(knx)sinly + c(l)sqrt(2)*cosly basis. y=0 to pi
    !        So for each 2-D wave the cosnxsiny and sinnxsiny pairs go through all L (y-components) for all K (x-components)

    !  IXMAX: at least 3*currentZonalWaves+1 gives the number of x grid points for transform method
    !  IYMAX: at least 6*currentMeridWaves+2 gives the number of y grid points for transform method
    !  KX: IXMAX+2 for the FFT991 routine
    !  LY: IYMAX+2 for the FFT991 routine
    !      IXMAX,IYMAX, IZMAX, KX,LY are local sizes of the above arrays given the local or
    !      current resolution, which is smaller than or equal to the allocated space given in the Settings
    !      type gUserSettings%maxZonalWaves, gUserSettings%maxMeridWaves, gUserSettings%levels  
    !  IYH: Y dimension of arrays plotted to the screen. It is IYMAX/2 + 1
    
    !  KXLYD is the allocated size of the G1, G2, G3 arrays used in FFT991 and ASXY plot routine
    !       To compute the transform we come across two conditions in y.  When there is an
    !       x-derivative of a quantity (II odd or IODEV=1) the wave components will have
    !       a siny dependence, whereas when there is a y-derivative the wave components
    !       will have a cosy dependence.  The x-derivative transforms are stored in
    !       G1 and the y-derivative transforms are stored in G2.  But we also have
    !       in the y-transform the zonal components which must be added to the wave
    !       components.  When II=4 we multiply G1 by G2 and store in G3.
    !  ITRXD,ITRYD are the allocated sizes of the trig arrays for FFT991
    !  NINTD is the allocated size of the interaction coefficient arrays
    
    !  ITOTA,NALA,KXLYA are dimensions of average value statistics arrays which are
    !      dependent upon 'statsOption'. They can be 1 or ITOTD, maxLevels.

    !  KX,LY,IXMAX,IYMAX are local sizes of the above arrays given the local or
    !      current resolution, which is smaller than the allocated space.
    !  IYH are the sizes of the arrays to be plotted to the screen.
    
    ! These values were in the common block
    !   COMMON/DIM/ITOTD,NINTD,ITOTA,NALA,KXLYA,KXV,LYV


    type :: Dims
        !  - Computed in SetDimensions
        integer :: ITOT     ! maxMeridWaves + 2*maxMeridWaves*maxZonalWaves is the horizontal size of the spectral arrays
        integer :: IXMAX    ! IXMAX: at least 3*currentZonalWaves+1 gives the number of x grid points for transform method
        integer :: IYMAX    ! IYMAX: at least 6*currentMeridWaves+2 gives the number of y grid points for transform method
        integer :: KX       ! IXMAX+2 for the FFT991 routine
        integer :: LY       ! IYMAX+2 for the FFT991 routine
        integer :: IYH      ! IYH: Y dimension of arrays plotted to the screen. It is IYMAX/2 + 1
        integer :: KXLYD    ! KXLYD is the allocated size of the G1, G2, G3 arrays used in FFT991 and ASXY plot routine

        integer :: NINTD    ! NINTD is the allocated size of the interaction coefficient arrays
        integer :: ITOTA    ! ITOTA,KXLYA are dimensions of average value statistics arrays which are
                            !      dependent upon 'statsOption'. They can be 1 or ITOTD.
        integer :: NALA     !  Is either 1 or maxLevels
        integer :: KXLYA
        integer :: KXV
        integer :: LYV
    end type Dims

    type(Dims) :: gArrayDims
    type(Dims) :: gCurrentDims

    contains
        subroutine ZeroArrayDims(arraySizes)
        type(Dims), intent(inout) :: arraySizes
            ! This subroutine initializes all dimensions to zero.
            arraySizes%ITOT = 0
            arraySizes%IXMAX = 0
            arraySizes%IYMAX = 0
            arraySizes%KX = 0
            arraySizes%LY = 0
            arraySizes%IYH = 0
            arraySizes%KXLYD = 0
            arraySizes%NINTD = 0
            arraySizes%ITOTA = 0
            arraySizes%NALA = 0
            arraySizes%KXLYA = 0
            arraySizes%KXV = 0
            arraySizes%LYV = 0
        end subroutine ZeroArrayDims

end module ArrayDimensionsModule