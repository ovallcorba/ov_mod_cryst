!------------------------------------------------------------------------------
! mod_2Ddata - Encapsulation/operations to 2D-XRD data
!------------------------------------------------------------------------------
!
! MODULE: mod_2Ddata
!
!> @author
!> Oriol Vallcorba
!> @copyright
!> Copyright 2013, Oriol Vallcorba
!
! DESCRIPTION: 
!> Read/write and operations with 2D-XRD data files.
!> Defines the 2D data type and implements operation functions that have
!> always as a parameter to which data they will operate.
!
! CHANGELOG:
!> @changelog
!> + Current (OV)
!>  - write IMG/EDF
!> + 160710 (OV)
!>  - Afegides subrutines per llegir EDFs
!>  - public var filename moved inside the type2d 
!>  - 140918: Canvi correccio LP
!>  - Correccio subrutina Yarc de forma que funcioni bé als canvis d'hemisferi
!>  - Afegida subrtina calcul factors amplada, angle
!>  - Afegida subrutina Calc2Tdeg
!> + 140205 (OV)
!>  - Adaptation to gFortran
!> + 131122 (OV)
!>  - Afegida subrutina de calcul de la desviacio estandar de les intensitats
!>    sense tenir en compte els valors de I=0 (tal com fa el calcul per defecte 
!>    de iymean al llegir un BIN).
!> + 131004 (OV)
!>  - Afegit parametre ymean al tipus de dades 2ddata. S'omple a l'obrir un BIN
!> + 130920 (OV)
!>  - Les zones excloses passen a ser trapezoides, cal canviar la lectura de la zona i
!>    el tipus ja que ara estan definides per 4 vertexs. També cal canviar la funcio
!>    isExZone ja que considerava la zona Quadrada (la antiga la mantenim amb
!>    el sufix *Square). Fem tamb� opci� pixel I=0 passem a mask.
!>  - Canvi de format fitxer zones excloses ExZ. Ara també es llegeix el parametre
!>    Y0toMask
!> + 130613 (OV)
!>  - Afegit a les subrutines de lectura imatges que retorni un missatge amb les
!>    dades de la imatge (msg)
!>  - Subr polaritzacio -> reanomenat lpcorr, i ara fa correccions per lab/synch
!>    S'han canviat els par�metres d'entrada a la subrutina -> iLor, iPol
!> + 130611 (OV)
!>  - subrutines utils (calcul 2t, calcul mitjana al voltant d'un pixel zona quadrada,etc...)
!> + 130606 (OV)
!>  - Init2Ddata s'inicialitzen els valors a -1
!> + 130524 (OV)
!>  - Afegida subrutina que suma intensitats d'un arc al voltant d'un pixel
!> + 130523 (OV)
!>  - En cas IMG desallocatem el header en la mateixa rutina de lectura, aixi
!>    canviem la rutina deallocate i la fem universal
!>  - La correcci� de polaritzaci� agafa parametres de la imatge a no ser
!>    que siguin zero.
!>  - Canvi format fitxer BIN, cap�alera 60 bytes fixa. Introduccio dades
!>    PixSx, pixSy, distOD, wavelength.
!>  - Canvi tipus al centre, de enter a REAL
!>  - Lectura de tots els parametres instrumentals dels fitxers IMG
!>  - A l'escriure PNG corregida comanda sistema de 'rm' per 'del'
!> + 130430 (OV)
!>  - Tret el tipus (type) peak. L'he posat a un modul propi del prog d2dpeaksearch
!> + 130419 (OV)
!>  - Afegit el tipus (type) peak que utilitza el prog d2dpeaksearch
!> + 130325 (OV)
!>  - Afegida subrutina per llegir les zones excloses d'un fitxer codef.EXZ
!> + 130313 (OV)
!>  - CANVI varibles noms i rang (abans de 1..nx, ara de 0..nx-1) per unificar amb altres progs
!>  - Canvi a l'inicialitzacio de la imatge que posava marge a zero ignorant
!>    el valor que s'havia entrat abans
!> + 130307 (OV)
!>  - Noves subrutines scaleImage i meanI
!>  - Al llegir IMG nomes escalem si es supera la intensitat maxima i no al reves
!> + 130201_1615 (OV)
!>  - Al llegir format IMG NO es corregeixen els valors de Xcentre Ycentre posant-los
!>    referenciats al nou origen 0,0 (extrem superior esquerra) i no a l'antic
!>    (extrem inferior esquerra), �s a dir, al fitxer IMG cal que s'entrin correctament
!>    referenciats a dalt a l'esquerra.
!
!========= DEPENDENCIES =======================================================
! mod_Aux
!
!========= VARIABLES ==========================================================
! exZone (type)
! data2D (type)
! fname (char*128)
! mod_writeToRES (logical)
! maxXYpoints (integer)
!
!========= FUNCTIONS ==========================================================
! isExZone(patt2d,col,row)
! isExZoneSquare(patt2d,col,row)
!
!========= SUBROUTINES ========================================================
! initData2D(d2d)
! copyData2D(d2din,d2d)
! cloneData2D(d2din,d2d,alloc)
! readBIN(filename,patt2d,msg)
! writeBIN(filename,patt2d)
! readEDF(filename,patt2d,msg)
! readEDF_header(filename,patt2d)
! readEDF_data(filename,patt2d,msg)
! readIMG(filename,patt2d,msg)
! readIMG_header(filename,patt2d)
! readIMG_data(filename,patt2d,msg)
! writePPM(filename,patt2d,fcontrast)
! data2D_dealloc(patt2d)
! lpcorr(patt2d,sepOD,pixsize,iPol,iLor)
! Imean(patt2d,meanI,desvI)
! scaleImage(patt2d,fscale)
! setFname(filename)
! readExZones(filename,patt2d)
! Yarc(patt2d,px,py,amplada,angle,self,ysum,npix,desv)!,debug)
! intRad(patt2d,t2ini,t2fin,stepsize,ysum,npix,desv)
! writeXY(filename,t2ini,t2fin,stepsize,ysum,npix,desv)
! copyMaskPixels(patt2d,dest)
! calc2T(patt2d,px,py,t2p)
! calc2Tdeg(patt2d,px,py,t2p)
! calcIntSquare(patt2d,px,py,aresta,inten,self)
! calcDesvEst(patt2d)
! getFactAngleAmplada(px,py,fang,famp)
! calib(...)
! maskToZero(patt2d)
! zeroToMask(patt2d)
!==============================================================================

module mod_2Ddata
    use mod_Aux
    
    type exZone
      integer(kind=2) :: ULx
      integer(kind=2) :: ULy
      integer(kind=2) :: URx
      integer(kind=2) :: URy
      integer(kind=2) :: BRx
      integer(kind=2) :: BRy
      integer(kind=2) :: BLx
      integer(kind=2) :: BLy
    end type exZone
    
    type data2D
      integer(kind=2), allocatable :: iyPix(:,:) !< intensity points (column,row)==(x,y)
      integer :: nymx, nxmx  !< pattern size
      integer :: maxY, minY !< max and min intensity (counts) -> ESCALATS
      real :: centy, centx, pixLx, pixLy, sepOD, wave !< beam centre, pixel size (micres), distance sample-detector (mm) and wavelength (A)
      real(kind=4) :: scale !< scale for tranformation int*2 to int*4 (32000->65000)
      integer :: headerBytes !< number of bytes in the image header file
      character, allocatable :: header(:) !< mida = bytes header
      type(exZone), allocatable :: ex(:) !< zones excloses (mask -1)
      integer(kind=2) :: margin !< si la imatge te un marge als 4 costats
      integer :: iymean !< mean intensity
      real :: ydesvest !< esd of the mean intensity
      character(LEN=128) fname !< filename
    end type

    logical :: mod_writeToRES = .true. !< parametre que indica si s'han d'escriure en el fitxer fname.res les sortides en pantalla
    
    integer maxXYpoints,y0ToMask
    logical debug,debugMode
    
    DATA maxXYpoints/10000/y0ToMask/0/debug/.true./debugMode/.false./
    
contains
    
   !---------------------------------------------------------------------------  
   ! DESCRIPTION: 
   !> Initialization of the 2D data type variables to default values
   !
   !> @param[inout] d2d
   !---------------------------------------------------------------------------  
    subroutine initData2D(d2d)
       type(data2D),intent(inout) :: d2d
       d2d%nymx=-1   !2048 !valors per defecte
       d2d%nxmx=-1   !2048
       d2d%centy=-1  !1023
       d2d%centx=-1  !1023
       d2d%pixLx=-1  !102.4 !micres
       d2d%pixLy=-1  !102.4
       d2d%sepOD=-1  !150300 !micres
       d2d%wave=-1   !0.7379 !Ang
       d2d%maxY=0      !65536
       d2d%minY=99999999 !0
       d2d%scale=1.00
       d2d%headerBytes=-1 !512
       if(d2d%margin.LT.0.OR.d2d%margin.GT.50)d2d%margin=0
    end subroutine initData2D
    
    !inicialitza les variables del tipus copiant els valors d'un altre data2D
    !de referencia (d2din)
    subroutine copyData2D(d2din,d2d)
       type(data2D),intent(in) :: d2din
       type(data2D),intent(inout) :: d2d
       d2d%nymx=d2din%nymx
       d2d%nxmx=d2din%nxmx
       d2d%centy=d2din%centy
       d2d%centx=d2din%centx
       d2d%scale=d2din%scale
       d2d%pixLx=d2din%pixLx
       d2d%pixLy=d2din%pixLy
       d2d%sepOD=d2din%sepOD
       d2d%wave=d2din%wave
       d2d%maxY=0      !65536
       d2d%minY=99999999 !0
       d2d%headerBytes=0
       d2d%margin=d2din%margin
       !zones excloses
       if (allocated(d2din%ex)) then
           allocate(d2d%ex(size(d2din%ex)))
           d2d%ex=d2din%ex
       end if
    end subroutine copyData2D

    !CLONA 2 IMATGES
    subroutine cloneData2D(d2din,d2d,alloc)

       type(data2D),intent(in) :: d2din
       type(data2D),intent(inout) :: d2d
       logical, intent(in) :: alloc
       
       integer i, j
       
       call copyData2D(d2din,d2d)
       !aquests tres no es copien amb copyData
       d2d%maxY=d2din%maxY 
       d2d%minY=d2din%minY
       d2d%headerBytes=d2din%headerBytes

       if (alloc) then
           allocate(d2d%iyPix(0:d2d%nxmx-1,0:d2d%nymx-1))
       end if
       
       do i=0, d2d%nymx-1
          do j=0, d2d%nxmx-1
              d2d%iyPix(j,i) = d2din%iyPix(j,i)
          end do
       end do
       
    end subroutine cloneData2D
    
    !LECTURA FITXER DADES BIN (format propi)
    !FITXER BIN (23/05/2013):
    !- Cap�alera fixa de 60 bytes. De moment hi ha 9 valors (la resta �s "buida"):
    !    Int*4     NXMX(cols)
    !    Int*4     NYMX(rows)
    !    Real*4    SCALE
    !    Real*4    CENTX
    !    Real*4    CENTY
    !    Real*4    PIXLX
    !    Real*4    PIXLY
    !    Real*4    DISTOD
    !    Real*4    WAVEL
    !- Llista Int*2 (amb signe.. -32,768 to 32,767) amb ordre files-columnes, es a dir,
    !  l'index rapid es el de les columnes (files,columnes): (1,1) (2,1) (3,1) ...
    subroutine readBIN(filename,patt2d,msg)
        implicit none
        character(len=*),intent(in) :: filename
        type(data2D),intent(inout) :: patt2d
        character(len=*),intent(inout) :: msg

        integer y0ToMaskIn,npix
        integer(kind=8)iysum
        integer i,j,adv
        logical :: lexist

        inquire (file=trim(filename),exist=lexist)
        if(.NOT.lexist)then
          write(*,*) trim(filename),' not found'
          stop
        end if
        
        
        open(unit=1,file=trim(filename),status='old',form='UNFORMATTED',Access='STREAM')
        read(1) patt2d%nxmx,patt2d%nymx,patt2d%scale,patt2d%centx,patt2d%centy
        read(1) patt2d%pixLx,patt2d%pixLy,patt2d%sepOD,patt2d%wave
        allocate(patt2d%iyPix(0:patt2d%nxmx-1,0:patt2d%nymx-1))
        
        patt2d%maxY=0
        patt2d%minY=999999999
        adv=0
        
        !ens interessa que al llegir y0ToMask sigui zero per evitar problemes amb
        !imatges on s'ha tret el fons
        y0ToMaskIn=y0ToMask
        y0ToMask=0
        
        iysum=0
        npix=0
        
        !llegim les dades
        do i=0, patt2d%nymx-1
          do j=0, patt2d%nxmx-1
            read(1,rec=61+adv) patt2d%iyPix(j,i)
            adv=adv+2
            !si es zona exclosa doncs assignem valor mascara -1
            !segons com podriem posar directament:if(patt2d%iyPix(j,i).LT.0)cycle
            if(isExZone(patt2d,j,i))THEN
              patt2d%iyPix(j,i)=-1
              cycle
            end if
            !si el valor del diagrama es menor de 0 l'assignem a -1 (mascara) i saltem
            !en tot cas sera una zona no valida
            if(patt2d%iyPix(j,i).LT.0)then
              patt2d%iyPix(j,i)=-1
              cycle
            end if
            
            !incrementem npix i sumem a ysum
            !LA ZERO NO LA TENIM EN COMPTE AL PROMIG
            if(patt2d%iyPix(j,i).NE.0)then
              npix=npix+1
              iysum=iysum+patt2d%iyPix(j,i)
            end if
            
            !max i min I
            if(patt2d%iyPix(j,i).GT.patt2d%maxY)patt2d%maxY=patt2d%iyPix(j,i)
            if(patt2d%iyPix(j,i).LT.patt2d%minY)patt2d%minY=patt2d%iyPix(j,i)
          end do
        end do
        
        !recuperem el valor de y0ToMask
        y0ToMask=y0ToMaskIn
        
        !calculem la intensitat promig
        patt2d%iymean=nint(real(iysum)/real(npix))
        
        close(unit=1)
    
        write(msg,'(A,I0,A,F0.3,A,I0,A,F0.3)') "Image: MaxI= ",NINT(patt2d%maxY*patt2d%scale),"/",&
                          patt2d%scale,"  MinI= ",NINT(patt2d%minY*patt2d%scale),"/",patt2d%scale

        patt2d%fname=filename
        
    end subroutine readBIN

    !ESCRITURA FITXER DADES BIN
    !
    !Parametres - filename: fitxer a escriure (amb extensio)
    !           - patt2d: dades a guardar al fitxer
    subroutine writeBIN(filename,patt2d)
        implicit none
        CHARACTER(LEN=*),intent(in):: filename
        type(data2D),intent(in)::patt2d
        integer i,j,adv
        
        open(unit=2, status='unknown',file=trim(filename)//'.bin',form='UNFORMATTED',Access='STREAM')
        write(2)patt2d%nxmx,patt2d%nymx,patt2d%scale,patt2d%centx,patt2d%centy
        write(2)patt2d%pixLx,patt2d%pixLy,patt2d%sepOD,patt2d%wave
        adv=0
        do i=0, patt2d%nymx-1
          do j=0, patt2d%nxmx-1
            write(2,rec=61+adv)patt2d%iyPix(j,i)
            adv=adv+2
          end do
        end do
        
        close(unit=2)
    
    end subroutine

    subroutine readEDF(filename,patt2d,msg)
        implicit none
        type(data2D),intent(inout) :: patt2d
        character(len=*),intent(in) :: filename
        character(len=*),intent(inout) :: msg
        
        call readEDF_header(filename,patt2d)
        call readEDF_data(filename,patt2d,msg)
        call setFname(patt2d,filename)
    
    end subroutine readEDF

    

    !--LECTURA FITXER IMG
    subroutine readIMG(filename,patt2d,msg)
        implicit none
        type(data2D),intent(inout) :: patt2d
        character(len=*),intent(in) :: filename
        character(len=*),intent(inout) :: msg
        
        call readIMG_header(filename,patt2d)
        call readIMG_data(filename,patt2d,msg)
        call setFname(patt2d,filename)

    end subroutine readIMG
    
    subroutine readEDF_header(filename,patt2d)
        implicit none
        
        character(len=*),intent(in) :: filename
        type(data2D),intent(inout) :: patt2d
        CHARACTER(LEN=128)line,msg
        integer i,iigual,file_size,data_size
        real pixSize,beamCX,beamCY
        character(LEN=16)temp
        logical :: lexist
        
        inquire (file=trim(filename),exist=lexist)
        if(.NOT.lexist)then
          write(*,*) trim(filename),' not found'
          stop
        end if
        
        INQUIRE(FILE=trim(filename), SIZE=file_size)
        
        !obrim com a text sequencial i llegim primeres linies buscant el header
        open(unit=1,file=trim(filename),status='old') 
        do i=1,50 
          read(1,'(A)',END=101)line
          iigual = index(line,"=")+1
          
          if(index(line,"Size =").NE.0)read(line(iigual:(LEN_TRIM(line)-1)), *) data_size
          if(index(line,"Dim_1").NE.0)read(line(iigual:(LEN_TRIM(line)-1)), *) patt2d%nxmx
          if(index(line,"Dim_2").NE.0)read(line(iigual:(LEN_TRIM(line)-1)), *) patt2d%nymx
          !centerxy estan en pixels
          if(index(line,"beam_center_x").NE.0)read(line(iigual:(LEN_TRIM(line)-1)), *) patt2d%centx
          if(index(line,"beam_center_y").NE.0)read(line(iigual:(LEN_TRIM(line)-1)), *) patt2d%centy
          !pixel size estan en micres
          if(index(line,"pixel_size_x").NE.0) then
            read(line(iigual:(LEN_TRIM(line)-1)), *) patt2d%pixLx
            read(line(iigual:(LEN_TRIM(line)-1)), *) patt2d%pixLy
          end if
          if(index(line,"pixelsize_x").NE.0)then
            read(line(iigual:(LEN_TRIM(line)-1)), *) patt2d%pixLx
            read(line(iigual:(LEN_TRIM(line)-1)), *) patt2d%pixLy
          end if
          !sepOD en milimetres
          if(index(line,"ref_distance").NE.0)read(line(iigual:(LEN_TRIM(line)-1)), *) patt2d%sepOD
          if(index(line,"ref_wave").NE.0)read(line(iigual:(LEN_TRIM(line)-1)), *) patt2d%wave

        end do
101     close(unit=1)

        patt2d%headerBytes = file_size - data_size
          
    end subroutine readEDF_header
    
    subroutine readEDF_data(filename,patt2d,msg)
        implicit none
        character(len=*),intent(in) :: filename
        character(len=*),intent(inout) :: msg
        type(data2D),intent(inout) :: patt2d
        integer(kind=2) valueI2
        integer i,j,valueI4,indexRow
        
        patt2d%maxY=0
        patt2d%minY=999999999

        allocate(patt2d%header(patt2d%headerBytes),patt2d%iyPix(0:patt2d%nxmx-1,0:patt2d%nymx-1)) !guardo fila,columna

        !si volem treballar amb el tipus Integer*2 de fortran que té signe i
        !degut a que estem llegint dades de l'aparell que son Integer*2 sense signe,
        !caldrà escalar-ho al maxim de 32767 (al sense signe s'arriba a 65536)
        !
        !Per això caldrà fer dues passades, a la primera calcularem el factor
        !d'escala i a la segona l'aplicarem a totes les intensitats.
        
        open(unit=1,file=trim(filename),status='unknown',form='UNFORMATTED',Access='STREAM')
        read(1) patt2d%header

        indexRow=0 !assignacio files de dalt a baix
        !llegim les dades per calcular el factor d'escala
        do i=patt2d%nymx-1, 0, -1 !files, llegim de baix a dalt que es com esta guardat (en aquest pas no importaria)
          do j=0, patt2d%nxmx-1 !columnes
            read(1) valueI2
            if(isExZone(patt2d,j,indexRow))cycle
            if(valueI2.LT.0)then
              valueI4=valueI2+65536
            else
              valueI4=valueI2
            end if
            if(valueI4.GT.patt2d%maxY)patt2d%maxY=valueI4
            if(valueI4.LT.patt2d%minY)patt2d%minY=valueI4
          end do
          indexRow=indexRow+1
        end do
        
        !nomes escalem a menys I
        if(patt2d%maxY.GT.32766)then
          patt2d%scale=patt2d%maxY/32767.
        else
          patt2d%scale=1.0
        end if
        
        close(1)
        
        !ara ja podem emmagatzemar les dades correctament (tornem a llegir el fitxer)
        open(unit=1,file=trim(filename),status='unknown',form='UNFORMATTED',Access='STREAM')
        read(1) patt2d%header
        
        indexRow=0 !per assignar correctament les files de dalt a baix
        !llegim les dades, les corregim d'escala i les guardem
        do i=patt2d%nymx-1, 0, -1 !files, llegim de baix a dalt que es com esta guardat (en aquest pas no importaria)
          do j=0, patt2d%nxmx-1 !columnes
            read(1) valueI2
            !si es zona exclosa doncs assignem valor mascara -1
            if(isExZone(patt2d,j,indexRow))THEN
              patt2d%iyPix(j,indexRow)=-1
              cycle
            end if
            !si no es zona exclosa, normal:
            if(valueI2.LT.0)then
              valueI4=nint(real(valueI2+65536)/patt2d%scale)
              valueI2=valueI4
            else
              valueI2=nint(real(valueI2)/patt2d%scale)
            end if
              patt2d%iyPix(j,indexRow)=valueI2
          end do
          indexRow=indexRow+1 !seguent linia
        end do
        
        close(unit=1)
        
        !correccio maxY minY
        patt2d%maxY=patt2d%maxY/patt2d%scale
        patt2d%minY=patt2d%minY/patt2d%scale
        
        write(msg,'(A,I0,A,F0.3,A,I0,A,F0.3)') "Image: MaxI= ",NINT(patt2d%maxY*patt2d%scale),"/",&
                          patt2d%scale,"  MinI= ",NINT(patt2d%minY*patt2d%scale),"/",patt2d%scale

        !deallocatem el header
        deallocate(patt2d%header)
        
    end subroutine readEDF_data
    
    
    !--LECTURA HEADER FITXER IMG    
    !aquesta subrutina hauria de llegir el header i treure'n info que es guarda
    !a les variables del modul.
    subroutine readIMG_header(filename,patt2d)
        implicit none
        
        character(len=*),intent(in) :: filename
        type(data2D),intent(inout) :: patt2d
        CHARACTER(LEN=128)line,msg
        integer i
        real pixSize,beamCX,beamCY
        character(LEN=16)temp
        logical :: lexist
        
        inquire (file=trim(filename),exist=lexist)
        if(.NOT.lexist)then
          write(*,*) trim(filename),' not found'
          stop
        end if
        
        !obrim com a text sequencial i llegim primeres linies buscant el header
        open(unit=1,file=trim(filename),status='old') 
        do i=1,50 
          read(1,'(A)',END=101)line
          
          !123456789012345678N
          !HEADER_BYTES=  512;
          temp=line(14:(LEN_TRIM(line)-1))
          if(index(line,"HEADER").NE.0)read(line(14:(LEN_TRIM(line)-1)), *) patt2d%headerBytes
          !1234567890N
          !SIZE1=2048;   son les X (num columnes)
          if(index(line,"SIZE1").NE.0)read(line(7:(LEN_TRIM(line)-1)), *) patt2d%nxmx
          !SIZE2=2048;   son les Y (num files)
          if(index(line,"SIZE2").NE.0)read(line(7:(LEN_TRIM(line)-1)), *) patt2d%nymx
          !123456789012345678901N
          !BEAM_CENTER_X=106.877;
          if(index(line,"BEAM_CENTER_X").NE.0)read(line(15:(LEN_TRIM(line)-1)), *) beamCX
          !123456789012345678901N
          !BEAM_CENTER_Y=104.353;
          if(index(line,"BEAM_CENTER_Y").NE.0)read(line(15:(LEN_TRIM(line)-1)), *) beamCY
          !1234567890123456789N
          !PIXEL_SIZE=0.102400;
          if(index(line,"PIXEL_SIZE").NE.0)read(line(12:(LEN_TRIM(line)-1)), *) pixSize
          !WAVELENGTH=0.7379;
          if(index(line,"WAVELENGTH").NE.0)read(line(12:(LEN_TRIM(line)-1)), *) patt2d%wave
          !DISTANCE=150.300;
          if(index(line,"DISTANCE").NE.0)read(line(10:(LEN_TRIM(line)-1)), *) patt2d%sepOD !en mm
          
          !AQUI PODRIEM ANAR AFEGINT CONDICIONS i llegint altres parametres
          
        end do
101     close(unit=1)

        !calculem el pixel central
        patt2d%centx=(beamCX/pixSize)
        patt2d%centy=(beamCY/pixSize)
        
        patt2d%pixLx=pixSize*1000 !en aquest cas IMG esta en mm, passem a micres
        patt2d%pixLy=pixSize*1000
          
    end subroutine readIMG_header

    !--LECTURA INTENSITATS FITXER IMG
    !aquesta subrutina ha de llegir les intensitats del fitxer IMG correctament 
    !segons els parametres que s'han llegit del header
    subroutine readIMG_data(filename,patt2d,msg)
        implicit none
        character(len=*),intent(in) :: filename
        character(len=*),intent(inout) :: msg
        type(data2D),intent(inout) :: patt2d
        integer(kind=2) valueI2
        integer i,j,valueI4,indexRow
        
        patt2d%maxY=0
        patt2d%minY=999999999

        allocate(patt2d%header(patt2d%headerBytes),patt2d%iyPix(0:patt2d%nxmx-1,0:patt2d%nymx-1)) !guardo fila,columna

        !si volem treballar amb el tipus Integer*2 de fortran que té signe i
        !degut a que estem llegint dades de l'aparell que son Integer*2 sense signe,
        !caldrà escalar-ho al maxim de 32767 (al sense signe s'arriba a 65536)
        !
        !Per això caldrà fer dues passades, a la primera calcularem el factor
        !d'escala i a la segona l'aplicarem a totes les intensitats.
        
        open(unit=1,file=trim(filename),status='unknown',form='UNFORMATTED',Access='STREAM')
        read(1) patt2d%header

        indexRow=0 !assignacio files de dalt a baix
        !llegim les dades per calcular el factor d'escala
        do i=patt2d%nymx-1, 0, -1 !files, llegim de baix a dalt que es com esta guardat (en aquest pas no importaria)
          do j=0, patt2d%nxmx-1 !columnes
            read(1) valueI2
            if(isExZone(patt2d,j,indexRow))cycle
            if(valueI2.LT.0)then
              valueI4=valueI2+65536
            else
              valueI4=valueI2
            end if
            if(valueI4.GT.patt2d%maxY)patt2d%maxY=valueI4
            if(valueI4.LT.patt2d%minY)patt2d%minY=valueI4
            !if(valueI4.EQ.0)write(*,*)"I=0, pix= ",i,j
          end do
          indexRow=indexRow+1
        end do
        
        !nomes escalem a menys I
        if(patt2d%maxY.GT.32766)then
          patt2d%scale=patt2d%maxY/32767.
        else
          patt2d%scale=1.0
        end if
        
        close(1)
        
        !ara ja podem emmagatzemar les dades correctament (tornem a llegir el fitxer)
        open(unit=1,file=trim(filename),status='unknown',form='UNFORMATTED',Access='STREAM')
        read(1) patt2d%header
        
        indexRow=0 !per assignar correctament les files de dalt a baix
        !llegim les dades, les corregim d'escala i les guardem
        do i=patt2d%nymx-1, 0, -1 !files, llegim de baix a dalt que es com esta guardat (en aquest pas no importaria)
          do j=0, patt2d%nxmx-1 !columnes
            read(1) valueI2
            !si es zona exclosa doncs assignem valor mascara -1
            if(isExZone(patt2d,j,indexRow))THEN
              patt2d%iyPix(j,indexRow)=-1
              cycle
            end if
            !si no es zona exclosa, normal:
            if(valueI2.LT.0)then
              valueI4=nint(real(valueI2+65536)/patt2d%scale)
              valueI2=valueI4
            else
              valueI2=nint(real(valueI2)/patt2d%scale)
            end if
              patt2d%iyPix(j,indexRow)=valueI2
          end do
          indexRow=indexRow+1 !seguent linia
        end do
        
        close(unit=1)
        
        !correccio maxY minY
        patt2d%maxY=patt2d%maxY/patt2d%scale
        patt2d%minY=patt2d%minY/patt2d%scale
        
        write(msg,'(A,I0,A,F0.3,A,I0,A,F0.3)') "Image: MaxI= ",NINT(patt2d%maxY*patt2d%scale),"/",&
                          patt2d%scale,"  MinI= ",NINT(patt2d%minY*patt2d%scale),"/",patt2d%scale

        !deallocatem el header
        deallocate(patt2d%header)
        
    end subroutine readIMG_data

    subroutine writeIMG(filename,patt2d)
    
        implicit none
        CHARACTER(LEN=*),intent(in):: filename
        type(data2D),intent(in)::patt2d
        integer i,j,adv
        
        open(unit=2, status='unknown',file=trim(filename)//'.bin',form='UNFORMATTED',Access='STREAM')
        write(2)patt2d%nxmx,patt2d%nymx,patt2d%scale,patt2d%centx,patt2d%centy
        write(2)patt2d%pixLx,patt2d%pixLy,patt2d%sepOD,patt2d%wave
        adv=0
        do i=0, patt2d%nymx-1
          do j=0, patt2d%nxmx-1
            write(2,rec=61+adv)patt2d%iyPix(j,i)
            adv=adv+2
          end do
        end do
        
        close(unit=2)
    
    end subroutine writeIMG
    
    !ESCRITURA IMATGE PNG.. ATENCIO:CAL TENIR INSTAL·LAT IMAGEMAGICK
    !(de moment blanc i negre) control parametre factor contrast
    subroutine writePPM(filename,patt2d,fcontrast)
        implicit none
        CHARACTER(LEN=*),intent(in):: filename
        type(data2D),intent(in)::patt2d
        integer i,j,k,l,m
        integer ccomponent, maxYcorr, minYcorr
        character*1 b
        character*32 L2
        real, intent(in)::fcontrast
        
        !todo: treure l'extensio de filename
        open(unit=2,file=trim(filename)//'.ppm',status='unknown',form='UNFORMATTED',Access='STREAM')   !P6
            
        write(2) 'P6',char(10)
        write(L2,'(2(i4,1x))') patt2d%nxmx, patt2d%nymx
        write(2) trim(L2), char(10)
        write(2) '255',char(10)
        
        maxYcorr = NINT(patt2d%maxY*fcontrast)
        minYcorr = NINT(patt2d%minY/fcontrast)
        
        do i=0, patt2d%nymx-1
           do j=0, patt2d%nxmx-1
             if(patt2d%iyPix(j,i).LT.0)then
               write(2) char(255),char(0),char(255)
               cycle
             end if
             ccomponent = int(real(patt2d%iyPix(j,i)-minYcorr)/(real(maxYcorr-minYcorr)/255.))
             b=char(ccomponent)
             write(2) b,b,b
           end do
        end do
       
        close(unit=2)
       
        !cridem convert (Imagemagick) per passar a png
        call SYSTEM('convert '//trim(filename)//'.ppm '//trim(filename)//'.png')
        call SYSTEM('del '//trim(filename)//'.ppm ')
     
    end subroutine
    
    
    subroutine data2D_dealloc(patt2d)
      implicit none
      type(data2D),intent(inout)::patt2d
      deallocate(patt2d%iyPix)
    end subroutine data2D_dealloc


    function isExZone(patt2d,col,row)
      implicit none
      integer,intent(in)::row,col
      type(data2D),intent(in)::patt2d
      logical :: isExZone
      integer i,j,crossings
      integer difx,dify,halfx,halfy
      integer sx,sy !punt final del segment
      integer ax,ay,bx,by,cx,cy,dx,dy !temporals pel calcul
      
      isExZone = .false.

      !si Intensitat es zero i opcio activada (y0ToMask=1) el fem mascara
      if(y0ToMask.EQ.1.AND.patt2d%iyPix(col,row).EQ.0)then
        isExZone=.true.
        return
      end if
      
      !primer mirem els marges
      if(row.LT.patt2d%margin.OR.row.GE.(patt2d%nymx-patt2d%margin))then
        isExZone=.true.
        return
      end if
      if(col.LT.patt2d%margin.OR.col.GE.(patt2d%nxmx-patt2d%margin))then
        isExZone=.true.
        return
      end if
      
      !si no s'ha inicialitzat marxem
      if(.not.allocated(patt2d%ex))return
      
      !ara calculem el segment del punt en questio al costat mes proper de la imatge
       halfx=nint(real(patt2d%nxmx)/2.)
       halfy=nint(real(patt2d%nymx)/2.)
       difx=patt2d%nxmx-row
       dify=patt2d%nymx-col
       if(abs(halfx-difx).GE.abs(halfy-dify))then
         !guanyen les x, files
         if(difx.GT.halfx)then
           !final segment (0,y)
           sx=0
           sy=row
         else
           !final segment (nxmx,y)
           sx=patt2d%nxmx
           sy=row
         end if
       else
         !guanyen les y, columnes
         if(dify.GT.halfy)then
           !final segment (x,0)
           sx=col
           sy=0
         else
           !final segment (x,nymx)
           sx=col
           sy=patt2d%nymx
         end if
       end if

      !ara mirem si estan dins o no dels trapezoides
      !el segment AB sempre sera el del punt a analitzar
      ax=col
      ay=row
      bx=sx
      by=sy
      do i=1,size(patt2d%ex)  !per cada poligon
        !iniciem a zero els creuaments      
        crossings=0
        !per cadascun dels 4 segments del poligon mirem si creua al segment (col,row,sx,sy)
        !1r segment UL-UR
        cx=patt2d%ex(i)%ULx
        cy=patt2d%ex(i)%ULy
        dx=patt2d%ex(i)%URx
        dy=patt2d%ex(i)%URy
        !primer mirarem que no estigui sobre el segment en cas que sigui horitzontal o vertical
        if(inSegment(ax,ay,cx,cy,dx,dy))then
          isExZone=.true.
          return
        end if
        if(ccw(ax,ay,cx,cy,dx,dy).EQV.ccw(bx,by,cx,cy,dx,dy))goto 200
        if(ccw(ax,ay,bx,by,cx,cy).EQV.ccw(ax,ay,bx,by,dx,dy))goto 200
        crossings = crossings +1
        
        !2n segment UR-BR
200     cx=patt2d%ex(i)%URx
        cy=patt2d%ex(i)%URy
        dx=patt2d%ex(i)%BRx
        dy=patt2d%ex(i)%BRy
        if(inSegment(ax,ay,cx,cy,dx,dy))then
          isExZone=.true.
          return
        end if
        if(ccw(ax,ay,cx,cy,dx,dy).EQV.ccw(bx,by,cx,cy,dx,dy))goto 300
        if(ccw(ax,ay,bx,by,cx,cy).EQV.ccw(ax,ay,bx,by,dx,dy))goto 300
        crossings = crossings +1
        
        !3r segment BR-BL
300     cx=patt2d%ex(i)%BRx
        cy=patt2d%ex(i)%BRy
        dx=patt2d%ex(i)%BLx
        dy=patt2d%ex(i)%BLy
        if(inSegment(ax,ay,cx,cy,dx,dy))then
          isExZone=.true.
          return
        end if
        if(ccw(ax,ay,cx,cy,dx,dy).EQV.ccw(bx,by,cx,cy,dx,dy))goto 400
        if(ccw(ax,ay,bx,by,cx,cy).EQV.ccw(ax,ay,bx,by,dx,dy))goto 400
        crossings = crossings +1
        
        !4t segment BL-UL
400     cx=patt2d%ex(i)%BLx
        cy=patt2d%ex(i)%BLy
        dx=patt2d%ex(i)%ULx
        dy=patt2d%ex(i)%ULy
        if(inSegment(ax,ay,cx,cy,dx,dy))then
          isExZone=.true.
          return
        end if
        if(ccw(ax,ay,cx,cy,dx,dy).EQV.ccw(bx,by,cx,cy,dx,dy))goto 500
        if(ccw(ax,ay,bx,by,cx,cy).EQV.ccw(ax,ay,bx,by,dx,dy))goto 500
        crossings = crossings +1
500     if(mod(crossings,2).NE.0)then
          isExZone = .true.
          return
        end if
      end do
    
      return !si no ha tornat abans tornara false

    end function isExZone
    
    
    function isExZoneSquare(patt2d,col,row)
      implicit none
      integer,intent(in)::row,col
      type(data2D),intent(in)::patt2d
      logical :: isExZoneSquare
      integer i
      
      isExZoneSquare = .false.
      
      if(row.LT.patt2d%margin.OR.row.GE.(patt2d%nymx-patt2d%margin))then
        isExZoneSquare=.true.
        return
      end if
      
      if(col.LT.patt2d%margin.OR.col.GE.(patt2d%nxmx-patt2d%margin))then
        isExZoneSquare=.true.
        return
      end if
      
      do i=1,size(patt2d%ex)
        if(row.GE.patt2d%ex(i)%ULy.AND.row.LE.patt2d%ex(i)%BRy)THEN
          if(col.GE.patt2d%ex(i)%ULx.AND.col.LE.patt2d%ex(i)%BRy)THEN
            isExZoneSquare=.true.
            return
          end if
        end if
      end do
    
      return !si no ha tornat abans tornara false
    end function isExZoneSquare

    !Aplica les correccions de lorentz i polaritzacio a unes dades entrades
    !Parametres: - patt2d: dades entrades (ja contenen la sepOD i el pixsize suposadament)
    !            - sepOD: distancia mostra detector (mm)
    !            - pixsize: mida del pixel (mm)
    !            - iPol: 0-NoCorr,1-Synchr,2-Lab&average
    !            - iLor: 0-NoCorr,1-Oscil,2-Powder
    !            - Paint=1 implica convertir la imatge a intensitat constant 12500
    ! iosc s'assigna a l'iniciar (OSC=1,2 => eix d'oscillació X,Y respect)
    subroutine lpcorr(patt2d,sepOD,pixsize,iPol,iLor,paint)
      implicit none
      type(data2D),intent(inout)::patt2d
      real,intent(inout)::sepOD,pixsize
      integer,intent(in)::iPol,iLor,paint
      integer(kind=4),allocatable::pattTemp(:,:)
      integer i, j, maxVal, minVal
      real fscale
      real vecCPx, vecCPy, vecCPmod, t2
      real fact, xkug, ykug, zkug, dkug, phi, arg, rloren, xdist2, ydist2, sepOD2, pol
      character(LEN=256)::msg
      integer iosc
      
      allocate(pattTemp(0:patt2d%nxmx-1,0:patt2d%nymx-1))
      maxVal=0
      minVal=9999999
      
      !si valen zero utilitzem els de la imatge, i si son zero sortim
      if(sepOD.LE.0)sepOD=patt2d%sepOD
      if(pixsize.LE.0)pixsize=patt2d%pixLx/1000 !en mm
      if(sepOD.LE.0.OR.pixsize.LE.0)return
      
      fact = pixsize / (1 + patt2d%wave * sepOD)
      iosc = 2
      
      !per cada pixel s'ha de calcular l'angle azimutal (entre la normal
      !(al pla de polaritzacio *i* el vector del centre de la imatge (xc,yc)
      !al pixel (x,y) en questió). També s'ha de calcular l'angle 2T del pixel
      do i=0, patt2d%nymx-1
        do j=0, patt2d%nxmx-1
          if(patt2d%iyPix(j,i).LT.0)cycle !si es mascara el saltem
          if(i.EQ.patt2d%centy.AND.j.EQ.patt2d%centx)cycle !el centre tambe el saltem
          
          !** DEBUG: TOTS ELS PUNTS AMB INTENSITAT=12500 (per veure efectes)
          if(paint.EQ.1)then
            patt2d%iyPix(j,i)=12500
            patt2d%maxY=12500
          end if
          
          !calc 2T
          vecCPx=real(j-patt2d%centx)*pixsize
          vecCPy=real(patt2d%centy-i)*pixsize
          vecCPmod=sqrt(vecCPx*vecCPx+vecCPy*vecCPy)
          t2=atan(vecCPmod/sepOD)
          
          !lorentz
          rloren = 1.0
          if (iLor.EQ.1) then
            xkug = fact * (j-patt2d%centx) * cos(t2)
            ykug = fact * (i-patt2d%centy) * cos(t2)
            zkug = (2./patt2d%wave) * sin(t2/2.)**2
            dkug=sqrt(xkug*xkug+ykug*ykug+zkug*zkug)
          
            if (iosc.EQ.1) phi = acos(xkug/dkug)
            if (iosc.EQ.2) phi = acos(ykug/dkug)
            arg = sin(phi)**2 - sin(t2/2.)**2
            rloren = 2*sin(t2/2.) * sqrt(arg)
          end if
          if (iLor.EQ.2) then
            rloren=1/(cos(t2/2)*sin(t2/2)**2)
          end if
          
          !polaritzacio
          pol = 1.0
          if (iPol.EQ.1) then
            xdist2 = vecCPx**2
            ydist2 = vecCPy**2
            sepOD2 = sepOD**2
            if (iosc.EQ.1) pol = (sepOD2 + xdist2) / (sepOD2 + xdist2 + ydist2)
            if (iosc.EQ.2) pol = (sepOD2 + ydist2) / (sepOD2 + xdist2 + ydist2)
          end if
          if (iPol.EQ.2) then
            pol=0.5+0.5*(cos(t2)**2)
          end if
          
          pattTemp(j,i)=patt2d%iyPix(j,i)
          pattTemp(j,i)=nint(real((pattTemp(j,i)*rloren)/pol))
          
          !ara mirem si superem els limits (per escalar despres)
          if(pattTemp(j,i).GT.maxVal)maxVal=pattTemp(j,i)
          
        end do
      end do
      
      !si ens hem passat del maxim calculem el factor d'escala i escalem
      fscale=1.0
      if(maxVal.GT.patt2d%maxY)then
        fscale=real(patt2d%maxY-1)/real(maxVal) !-1 per assegurar-nos que entra
      end if
      do i=0, patt2d%nymx-1
        do j=0, patt2d%nxmx-1
          if(patt2d%iyPix(j,i).LT.0)cycle !si es mascara el saltem
          patt2d%iyPix(j,i)=nint(real(pattTemp(j,i))*fscale)
        end do
      end do
     
      deallocate(pattTemp)

    end subroutine lpcorr

    
    !subrutina que retorna la intensitat mitjana, max, min i la desviacio d'una imatge 
    subroutine Imean(patt2d,meanI,desvI,maxInt,minInt)
      implicit none
      type(data2D),intent(in)::patt2d
      real,intent(out)::meanI,desvI
      integer,intent(out)::maxInt,minInt
      integer i,j
      integer*8 Isuma
      real arg
      
      maxInt=0
      minInt=9999999
      
      Isuma=0
      do i=0, patt2d%nymx-1
        do j=0, patt2d%nxmx-1
          Isuma=Isuma+patt2d%iyPix(j,i)
          if(patt2d%iyPix(j,i).GT.maxInt)maxInt=patt2d%iyPix(j,i)
          if(patt2d%iyPix(j,i).LT.minInt)minInt=patt2d%iyPix(j,i)
        end do
      end do
      meanI=Isuma/(patt2d%nymx*patt2d%nxmx)
      
      Isuma=0
      do i=0, patt2d%nymx-1
        do j=0, patt2d%nxmx-1
          Isuma=Isuma+(patt2d%iyPix(j,i)-meanI)*(patt2d%iyPix(j,i)-meanI)
        end do
      end do
      arg=Isuma/(patt2d%nymx*patt2d%nxmx-1)
      desvI=sqrt(arg)
    end subroutine Imean
    
    !aplica un factor d'escala a una imatge (I/fscale)
    subroutine scaleImage(patt2d,fscale)
      implicit none
      type(data2D),intent(inout)::patt2d
      real,intent(in)::fscale
      integer i,j
      do i=0, patt2d%nymx-1
        do j=0, patt2d%nxmx-1
          patt2d%iyPix(j,i)=patt2d%iyPix(j,i)/fscale
        end do
      end do
      !correccio maxY minY scale
      patt2d%maxY=patt2d%maxY/fscale
      patt2d%minY=patt2d%minY/fscale
      patt2d%scale=fscale
    end subroutine
    

    !estableix el nom del fitxer
    subroutine setFname(patt2d,filename)
      type(data2D),intent(inout)::patt2d
      character(len=*),intent(in) :: filename
      patt2d%fname=filename
    end subroutine
    

    !Llegeix les zones excloses TRAPEZOIDES d'un fitxer filename.EXZ
    subroutine readExZones(filename,patt2d)
      character(len=*),intent(in) :: filename !sense extensio
      type(data2D),intent(out) :: patt2d
      character(len=1) caracter
      character(len=256)::line
      integer nExZ,i
      logical file_exists
      
      INQUIRE(FILE=trim(filename)//'.EXZ', EXIST=file_exists)
      if(.NOT.file_exists)return

      !Es llegeixen zones determinades per 4 vertexs
      !Tambe es contempla si la imatge te "marge".
      !   ATENCIO: ELS VALORS S'HAN D'ENTRAR CONSIDERANT IMATGE 0..N-1
      !            COM A MARGE EL NOMBRE DE PIXELS DEL MARGE
      open(unit=1,file=trim(filename)//'.EXZ',STATUS='OLD')
      
      do while (1>0)
        read(1,*,END=101)line
        if(line(1:1).EQ.'!')cycle
        if(line(2:2).EQ.'!')cycle
        
        !Margin
        if(index(line,"MARGIN").NE.0)then
          i = index(line,"=")
          read(line(i+1:LEN_TRIM(line)), *) patt2d%margin
        end if
        
        !Y0ToMask
        if(index(line,"TOMASK").NE.0)then
          i = index(line,"=")
          read(line(i+1:LEN_TRIM(line)), *) Y0ToMask
        end if
        
        !ExZones
        if(index(line,"NEXZONES").NE.0)then
          i = index(line,"=")
          read(line(i+1:LEN_TRIM(line)), *) nExZ
          allocate(patt2d%ex(nExZ))
          !comprovem que no hi hagi una linia de comentari pel mig
202       read(1,*)caracter
          if(caracter.EQ.'!')goto 202
          backspace 1
          !ara llegim les zones
          do i=1,nExZ
             read(1,*) patt2d%ex(i)%ULx,patt2d%ex(i)%ULy,patt2d%ex(i)%URx,patt2d%ex(i)%URy,&
                       patt2d%ex(i)%BRx,patt2d%ex(i)%BRy,patt2d%ex(i)%BLx,patt2d%ex(i)%BLy
          end do
        end if
      end do
101   close(unit=1)
    end subroutine

    
    !Aquesta subrutina sumar� les intensitats dels p�xels en una zona en forma d'arc al voltant
    !d'un pixel determinat.
    ! 1)calculem (utilitzant amplada i angle) la 2Tmin i 2Tmax de l'arc, aix� com els 4 vertexs 
    !   de l'arc
    ! 2)trobarem el quadrat minim que cont� l'arc
    ! 3)per cada pixel del cuadrat:
    !   -mirarem si est� entre 2Tmin i 2Tmax
    !   -mirarem si l'angle entre el vector reflexio-centre i el vector pixel-centre es menor a 
    !    "angle"
    !   -si es compleixen les dues condicions anteriors en sumarem la intensitat
    !parametres d'entrada:
    !  - patt2d: la imatge de treball (CONSIDEREM QUE S'HAN ASSIGNAT EL CENTRE, DISTOD i PIXSIZE)
    !  - px, py: Coordenades x,y del p�xel
    !  - amplada: amplada de l'arc (gruix)
    !  - angle: obertura de l'arc
    !  - self: si es considera o no el propi pixel
    !  - bkgpt: num de punts de menor intensitat a considerar per calcular el fons
    !parametres de sortida:
    !  - ysum: intensitat suma de la zona
    !  - npix: nombre de pixels que han contribuit a la suma
    !  - ymean: intensitat mitjana de la zona
    !  - ymeandesv: desviacio estandard de la mitjana (ysum/npix)
    !  - ymax: intensitat maxima del pixel
    !  - ybkg: intensitat del fons (mitjana dels 20 punts de menor intensitat)
    !  - ybkgdesv: desviacio de la intensitat del fons (entre els 20 punts)
    !  - paint: per "pintar" de -1 l'arc, si no es per comprovacions ha de valer false
    subroutine Yarc(patt2d,px,py,amplada,angle,self,bkgpt,ysum,npix,ymean,ymeandesv,ymax,ybkg,ybkgdesv,paint)
      implicit none
      
      !parametres
!      type(data2D),intent(in)::patt2d
      type(data2D),intent(inout)::patt2d  !debug inout
      integer, intent(in)::px,py
      real,intent(in)::amplada,angle
      logical,intent(in)::self,paint
      integer,intent(out)::ysum,npix,ymax,bkgpt
      real,intent(out)::ymean,ymeandesv,ybkg,ybkgdesv
      
      integer i,j,k
      
      !calculs extraccio (quadrat,etc..)
      real RcX,RcY,RcXmax,RcYmax,RcXmin,RcYmin
      real RcXcw,RcYcw,RcXcwMax,RcYcwMax,RcXcwMin,RcYcwMin
      real RcXacw,RcYacw,RcXacwMax,RcYacwMax,RcXacwMin,RcYacwMin
      real vSupX,vSupY,vInfX,vInfY
      real t2min, t2max,x2,y2,radi,t2p
      real vPCx,vPCy
      integer nbkgpt,bkgsum,ivSupY,ivInfY,ivSupX,ivInfX
      real pesc, modvPC, modvRc, angleB, angleRad, modR, cosAng, sinAng
      real RcUx, RcUy, RcUXcw, RcUYcw, RcUXacw, RcUYacw
      integer intensitats(10000) !vector on guardarem les intensitats per calcular desv.estd
      real xmean,sumdesv
      integer, allocatable :: minint(:) !vector amb les 20 intensitats menors

      
      !inicialitzacions
      ysum=0
      ymean=0
      ybkg=0
      npix=0
      ymeandesv=0
      ybkgdesv=0
      ymax=0
      nbkgpt = 200
      allocate(minint(nbkgpt))
      do i = 1, nbkgpt
        minint(i) = 0  !per si de cas iniciem a zero
      end do
      bkgpt = 0
      
      angleRad = angle*crad
      if(debugMode)print*, "angleRad=",angleRad
      
      !calcul vector centre-pixelReflexio (Rc) amb l'origen al centre de la imatge (per fer rotacions)
      RcX=px-patt2d%centx
      RcY=patt2d%centy-py
      if(debugMode)print*, "Rc=",RcX,",",RcY
      !Amplada de l'arc (es podria fer RcX-Amplada*nxmx i RcY-Amp*nymx si la imatge no fos quadrada)
      !l'amplada es en "pixels"
      modR = sqrt(RcX*RcX+RcY*RcY)
      if(debugMode)print*, "modR=",modR
      RcUx = RcX/modR
      RcUy = RcY/modR
      RcXmax=RcUx*(modR+amplada/2)
      RcYmax=RcUy*(modR+amplada/2)
      RcXmin=RcUx*(modR-amplada/2)
      RcYmin=RcUy*(modR-amplada/2)
      if(debugMode)print*, "RcMax=",RcXmax,",",RcYmax," RcMin=",RcXmin,",",RcYmin
      !ara fem rotar el vector Rc +angle/2 i -angle/2 per trobar els limits de l'arc, trobem els vectors
      !RcCWMax i Min (clockwise) i RcACWMax i min (anticlockwise) i tindrem els vertexs de l'arc
      !CLOCKWISE
      cosAng = cos(angleRad/2)
      sinAng = sin(angleRad/2)
      RcXcw=RcX*cosAng+RcY*sinAng
      RcYcw=-RcX*sinAng+RcY*cosAng
      if(debugMode)print*, "RcCW=",RcXcw,",",RcYcw
      !amplada
      RcUXcw = RcXcw/modR
      RcUYcw = RcYcw/modR
      RcXcwMax=RcUXcw*(modR+amplada/2)
      RcYcwMax=RcUYcw*(modR+amplada/2)
      RcXcwMin=RcUXcw*(modR-amplada/2)
      RcYcwMin=RcUYcw*(modR-amplada/2)
      !ANTICLOCKWISE
      RcXacw=RcX*cosAng-RcY*sinAng
      RcYacw=RcX*sinAng+RcY*cosAng
      if(debugMode)print*, "RcACW=",RcXacw,",",RcYacw
      !amplada
      RcUXacw = RcXacw/modR
      RcUYacw = RcYacw/modR
      RcXacwMax=RcUXacw*(modR+amplada/2)
      RcYacwMax=RcUYacw*(modR+amplada/2)
      RcXacwMin=RcUXacw*(modR-amplada/2)
      RcYacwMin=RcUYacw*(modR-amplada/2)
      
      !ara ja tenim els vertexs, per definir el quadrat mirem el superior esquerra i l'inferior dret,
      if(debugMode)print*, "RcCWmax=",RcXcwMax,",",RcYcwMax
      if(debugMode)print*, "RcCWmin=",RcXcwMin,",",RcYcwMin
      if(debugMode)print*, "RcACWmax=",RcXacwMax,",",RcYacwMax
      if(debugMode)print*, "RcACWmin=",RcXacwMin,",",RcYacwMin

      vSupX=min(RcXcwMax,RcXcwMin,RcXacwMax,RcXacwMin)
      vSupY=max(RcYcwMax,RcYcwMin,RcYacwMax,RcYacwMin)
      vInfX=max(RcXcwMax,RcXcwMin,RcXacwMax,RcXacwMin)
      vInfY=min(RcYcwMax,RcYcwMin,RcYacwMax,RcYacwMin)
      
      if(debugMode)print*, "vSup=",vSupX,",",VsupY,"  Vinf=",VinfX,",",VinfY
      
      !Aquests valors definiran el quadrat m�nim sempre i quant s'hagi creuat algun eix.
      !MIREM si hem creuat algun eix (vertical o horitzontal) per tal d'evitar definir un
      !quadrat insuficient (i en conseq��ncia actualitzem el vertex)
      if(RcXacw.LT.0.AND.RcXcw.GT.0)then
          !estem creuant la vertical per l'hemisferi superior
          !caldr� considerar com a vSupY el vector sobre aquesta vertical (0,y)
          !amb y=(modR+amplada/2)
          vSupY=(modR+amplada/2)
      end if
      if(RcXacw.GT.0.AND.RcXcw.LT.0)then
          !estem creuant la vertical per l'hemisferi inferior
          !caldr� considerar com a vInfY el vector sobre aquesta vertical (0,-y)
          !amb y=(modR+amplada/2)
          vInfY=-(modR+amplada/2)
      end if
      if(RcYacw.LT.0.AND.RcYcw.GT.0)then
          !estem creuant la horitzontal per l'hemisferi esquerra
          !caldr� considerar com a vSupX el vector sobre aquesta horitzontal (-x,0)
          !amb x=(modR+amplada/2)
          vSupX=-(modR+amplada/2)
      end if
      if(RcYacw.GT.0.AND.RcYcw.LT.0)then
          !estem creuant la horitzontal per l'hemisferi dret
          !caldr� considerar com a vInfX el vector sobre aquesta horitzontal (x,0)
          !amb x=(modR+amplada/2)
          vInfX=(modR+amplada/2)
      end if
      !si l'angle es mes gran de 180 pot ser que toqui 3 hemisferis, agafem tot el quadrat
      if(angle.GT.180)then
          vSupY=(modR+amplada/2)
          vInfY=-(modR+amplada/2)
          vSupX=-(modR+amplada/2)
          vInfX=(modR+amplada/2)
      end if
      
      if(debugMode)print*, "(corr) vSup=",vSupX,",",VsupY,"  Vinf=",VinfX,",",VinfY
      
      !i li sumem el centre per tenir els pixels en la imatge real
      !tornem a referenciar l'origen (0,0) al vertex inferior esquerra
      vSupX=vSupX+patt2d%centx
      vSupY=patt2d%centy-vSupY
      vInfX=vInfX+patt2d%centx
      vInfY=patt2d%centy-vInfY
      
      if(debugMode)print*, "(image) vSup=",vSupX,",",VsupY,"  Vinf=",VinfX,",",VinfY
      
      !calculem els angles t2min i t2max de l'arc (sepMD pixL en micres=> radi micres)
      x2 = (RcXmax*patt2d%pixLx)*(RcXmax*patt2d%pixLx)
      y2 = (RcYmax*patt2d%pixLy)*(RcYmax*patt2d%pixLy)
      radi = sqrt(x2+y2)
      t2max=atan(radi/(patt2d%sepOD*1000))
      
      x2 = (RcXmin*patt2d%pixLx)*(RcXmin*patt2d%pixLx)
      y2 = (RcYmin*patt2d%pixLy)*(RcYmin*patt2d%pixLy)
      radi = sqrt(x2+y2)
      t2min=atan(radi/(patt2d%sepOD*1000))
      
      if(debugMode)print*, "sepOD=",patt2d%sepOD,"pixLx=",patt2d%pixLx
      if(debugMode)print*, "t2max=",t2max,"t2min=",t2min
      
      !Ara ja toca la part:
      !3)per cada pixel del cuadrat:
      !   -mirarem si est� entre 2Tmin i 2Tmax
      !   -mirarem si l'angle entre el vector reflexio-centre i el vector pixel-centre es menor a "angle"
      !   -si es compleixen les dues condicions anteriors en sumarem la intensitat
      ivSupY = NINT(vSupY);
      ivInfY = NINT(vInfY);
      ivSupX = NINT(vSupX);
      ivInfX = NINT(vInfX);
      
      do j=ivSupY,ivInfY
        do i=ivSupX,ivInfX
            !si esta fora la imatge el saltem
            if(i.LT.0.OR.j.LT.0.OR.i.GE.patt2d%nxmx.OR.j.GE.patt2d%nymx)cycle
            !si es mascara el saltem
            if(patt2d%iyPix(i,j).LT.0)cycle
            !si �s ell mateix i no es vol considerar (self=false) el saltem
            if(i.EQ.px.AND.j.EQ.py.AND..NOT.self)cycle
            
            !calculem el vector pixel-centre (corregit d'origen, centre 0,0 i necessari pels calculs seguents)
            vPCx=i-patt2d%centx
            vPCy=patt2d%centy-j
            
            !mirem si esta entre 2tmin i 2tmax, sino saltem (cal calcular t2p)
            x2 = (vPCx*patt2d%pixLx)*(vPCx*patt2d%pixLx)
            y2 = (vPCy*patt2d%pixLy)*(vPCy*patt2d%pixLy)
            radi = sqrt(x2+y2)
            t2p=atan(radi/(patt2d%sepOD*1000))
            if(t2p.LT.t2min.OR.t2p.GT.t2max)cycle
            
            !angle entre vPC i Rc (prod. escalar)
            pesc=vPCx*RcX+vPCy*RcY
            modvPC=sqrt(vPCx*vPCx+vPCy*vPCy)
            if((pesc/(modR*modvPC)).GT.1)then
              angleB=acos(1.0)
            else
              angleB=acos(pesc/(modR*modvPC))
            end if
            !si angleB es major a angle -> saltem
            if((angleB/crad).GT.(angle/2))cycle
            
            !si hem arribat aqu� es que hem se sumar la intensitat del pixel
            ysum=ysum+patt2d%iyPix(i,j)
            if(ymax.LT.patt2d%iyPix(i,j))ymax=patt2d%iyPix(i,j)
            !write(*,*) patt2d%iyPix(i,j)
            !write(*,*)'minint ',minint
            
            !FONS agafarem els del perimetre de l'arc (S'HAURIA DE FER AMB EL RADI micres, independent de sepOD i wl)
            !0.015 deg == 0.0002618
            !write(*,*) t2p, t2min, t2max
            if (bkgpt.LE.nbkgpt) then
                if ((abs(t2p-t2min).LT.0.0002618).OR.(abs(t2p-t2max).LT.0.0002618)) then
                    minint(bkgpt) = patt2d%iyPix(i,j)
                    bkgpt = bkgpt + 1
                    !if(paint)patt2d%iyPix(i,j)=-1
                end if
            end if
            
            npix=npix+1
            if(npix.LE.10000)intensitats(npix)=patt2d%iyPix(i,j)
            
            !debug
            if(paint)patt2d%iyPix(i,j)=-1
        end do
      end do
      
      if(npix.GT.0)then
        !calcul desviacio estandar sqrt((sum(xi-xmean)^2)/N-1)
        ymean=real(ysum)/real(npix)
        sumdesv=0
        do i=1,min(npix,10000)
            sumdesv=sumdesv+(real(intensitats(i))-ymean)**2
        end do
        if(npix.LT.2)npix=2
        ymeandesv=sqrt(sumdesv/real(npix-1))
        !calcul del valor de fons i la desviacio
        bkgsum=0
        bkgpt = bkgpt - 1 !corregim index
        do i=1, bkgpt
          bkgsum=bkgsum+minint(i)
        end do
        ybkg=real(real(bkgsum)/real(bkgpt))
        !write(*,*) bkgsum, bkgpt, ybkg
        !write(*,*) minint
        sumdesv=0
        do i=1, bkgpt
          sumdesv=sumdesv+(real(minint(i))-ybkg)**2
        end do
        ybkgdesv=sqrt(sumdesv/real(bkgpt-1))
        if(debugMode)write(*,*)bkgsum, minint(3),ybkg,sumdesv,ybkgdesv
      end if
      
      deallocate(minint)
        
    end subroutine Yarc
    
    subroutine getFactAngleAmplada(patt2d,px,py,fang,famp)
        implicit none
        
        !parameters
        type(data2D),intent(in)::patt2d
        integer,intent(in)::px,py
        real,intent(out)::fang,famp
        real maxAngle,angle,x1,y1,x2,y2
        
        call calc2Tdeg(patt2d,0,0,maxAngle)
        call calc2Tdeg(patt2d,px,py,angle)
        !interpolem sobre les rectes
        !ANGLE
        x1 = 0.0
        y1 = 3.0
        x2 = maxAngle
        y2 = 0.8
        fang = ((y2 - y1) / (x2 - x1)) * angle + y1 - ((y2 - y1) / (x2 - x1)) * x1
        !AMPLADA
!         x1 = 0.0f;
!         y1 = 1.2f;
!         x2 = maxAngle;
!         y2 = 0.8f;
!         factors[1] = ((y2 - y1) / (x2 - x1)) * angle + y1 - ((y2 - y1) / (x2 - x1)) * x1;
        famp= 1.0
        
    end subroutine
    
    
    !Aquesta subrutina fa la integraci� radial de la imatge
    !parametres d'entrada:
    !  - patt2d: la imatge de treball (CONSIDEREM QUE S'HAN ASSIGNAT EL CENTRE, DISTOD i PIXSIZE)
    !  - t2ini, t2fin
    !  - stepsize: pas del "diagrama" a generar == gruix dels anells
    !parametres de sortida:
    !  - ysum: vector d'intensitats
    !  - npix: vector de numero de pixels que han contribuit a cada punt
    !  - desv: vector de desviacions estandard de ysum en cada punt
    ! ATENCIO: TREC LA MULTIPLICACIO PER L'ESCALA
    subroutine intRad(patt2d,t2ini,t2fin,stepsize,ysum,npix,desv)
      implicit none
      
      !parametres
      type(data2D),intent(in)::patt2d
      real,intent(in)::t2ini,t2fin,stepsize
!      integer,intent(out),allocatable::ysum(:),npix(:)
!      real,intent(out),allocatable::desv(:)
      integer,intent(out)::ysum(maxXYpoints),npix(maxXYpoints)
      real,intent(out)::desv(maxXYpoints)
      real xmean,sumdesv
      
      integer i,j,p
      integer npoints
      integer vPCx,vPCy
      real x2,y2,radi,t2p
      
      !t2ini,t2fin,step (en graus)
      npoints=nint((t2fin-t2ini)/stepsize)+1 !+1 perque volem incloure t2ini i t2fin
      
!      allocate(ysum(npoints),npix(npoints),desv(npoints))
      
      !inicialitzem vectors
      do i=1,npoints
        ysum(i)=0
        npix(i)=0
        desv(i)=0
      end do
      !degug:
 !     write(*,*) patt2d%centx,patt2d%centy,patt2d%pixLx,patt2d%pixLy,patt2d%sepOD
      do i=0, patt2d%nymx-1
        do j=0, patt2d%nxmx-1
          
            !si es mascara o zero el descartarem (suposem que hem aplicat zones excloses abans)
            if(patt2d%iyPix(j,i).LE.0)cycle
            !mirem la 2T del pixel en la imatge per determinar amplada i angle
            !1)vector centre pixel:
            vPCx=j-patt2d%centx
            vPCy=patt2d%centy-i
            !2)calcul angle 2teta en graus
            x2 = (vPCx*patt2d%pixLx)*(vPCx*patt2d%pixLx)
            y2 = (vPCy*patt2d%pixLy)*(vPCy*patt2d%pixLy)
            radi = sqrt(x2+y2)
            t2p=atan(radi/(patt2d%sepOD*1000))/crad
            if(t2p.LT.t2ini)cycle
            !mirem a quina posicio del vector (diagrama pols) ha d'anar
            p=nint(t2p/stepsize)-nint(t2ini/stepsize)+1
            ysum(p)=ysum(p)+patt2d%iyPix(j,i)  !*patt2d%scale
            npix(p)=npix(p)+1
        end do
      end do
      
      !ara ja hauriem de tenir al vector el diagrama de pols.
      
      !tornem a fer una passada per calcular la desviacio estandar sqrt((sum(xi-xmean)^2)/N-1)
      do i=0, patt2d%nymx-1
        do j=0, patt2d%nxmx-1
            !si es mascara o zero el descartarem (!!que passa amb el braç?) podriem mirar si es
            !exzone en la imatge original en comptes de la intensitat (TODO)
            if(patt2d%iyPix(j,i).LE.0)cycle
            !mirem la 2T del pixel en la imatge per determinar amplada i angle
            !1)vector centre pixel:
            vPCx=j-patt2d%centx
            vPCy=patt2d%centy-i
            !2)calcul angle 2teta en graus
            x2 = (vPCx*patt2d%pixLx)*(vPCx*patt2d%pixLx)
            y2 = (vPCy*patt2d%pixLy)*(vPCy*patt2d%pixLy)
            radi = sqrt(x2+y2)
            t2p=atan(radi/(patt2d%sepOD*1000))/crad
            if(t2p.LT.t2ini)cycle
            !mirem a quina posicio del vector (diagrama pols) ha d'anar
            p=nint(t2p/stepsize)-nint(t2ini/stepsize)+1
            !I ARA ACUMULEM LA DESV
            xmean=real(ysum(p))/real(npix(p))
            desv(p)=desv(p)+(patt2d%iyPix(j,i)-xmean)*(patt2d%iyPix(j,i)-xmean)
        end do
      end do
      !Calcul final desviacio
      do i=1,npoints+1
        if(npix(i).LT.2)then
          desv(i)=0
          cycle
        end if
        desv(i)=sqrt(desv(i)/(real(npix(i))-1))
      end do
      
    end subroutine intRad

    !Aquesta subrutina fa la integraci� radial de la imatge
    !parametres d'entrada:
    !  - patt2d: la imatge de treball (CONSIDEREM QUE S'HAN ASSIGNAT EL CENTRE, DISTOD i PIXSIZE)
    !  - t2ini, t2fin
    !  - stepsize: pas del "diagrama" a generar == gruix dels anells
    !parametres de sortida:
    !  - ysum: vector d'intensitats
    !  - npix: vector de numero de pixels que han contribuit a cada punt
    !  - desv: vector de desviacions estandard de ysum en cada punt
    !  - lowThold/HighThold: only consider intensities higher than the lowThreshold and lower than the HighThresghold (vector tamb� per 2 theta)
    ! ATENCIO: TREC LA MULTIPLICACIO PER L'ESCALA
    subroutine intRadThreshold(patt2d,t2ini,t2fin,stepsize,ysum,npix,desv,lowThold,HighThold)
      implicit none
      
      !parametres
      type(data2D),intent(in)::patt2d
      real,intent(in)::t2ini,t2fin,stepsize
!      integer,intent(out),allocatable::ysum(:),npix(:)
!      real,intent(out),allocatable::desv(:)
      integer,intent(out)::ysum(maxXYpoints),npix(maxXYpoints)
      integer,intent(in)::lowThold(maxXYpoints),HighThold(maxXYpoints)
      real,intent(out)::desv(maxXYpoints)
      real xmean,sumdesv
      
      integer i,j,p
      integer npoints
      integer vPCx,vPCy
      real x2,y2,radi,t2p
      
      !t2ini,t2fin,step (en graus)
      npoints=nint((t2fin-t2ini)/stepsize)+1 !+1 perque volem incloure t2ini i t2fin
      
!      allocate(ysum(npoints),npix(npoints),desv(npoints))
      
      !inicialitzem vectors
      do i=1,npoints
        ysum(i)=0
        npix(i)=0
        desv(i)=0
      end do

      do i=0, patt2d%nymx-1
        do j=0, patt2d%nxmx-1
            !si es mascara o zero el descartarem (suposem que hem aplicat zones excloses abans)
            if(patt2d%iyPix(j,i).LE.0)cycle
            
            !mirem la 2T del pixel en la imatge per determinar amplada i angle
            !1)vector centre pixel:
            vPCx=j-patt2d%centx
            vPCy=patt2d%centy-i
            !2)calcul angle 2teta en graus
            x2 = (vPCx*patt2d%pixLx)*(vPCx*patt2d%pixLx)
            y2 = (vPCy*patt2d%pixLy)*(vPCy*patt2d%pixLy)
            radi = sqrt(x2+y2)
            t2p=atan(radi/(patt2d%sepOD*1000))/crad
            if(t2p.LT.t2ini)cycle
            !mirem a quina posicio del vector (diagrama pols) ha d'anar
            p=nint(t2p/stepsize)-nint(t2ini/stepsize)+1
            
            !mirem si est� per sobre de lowThold i per sota de highThold
            if(patt2d%iyPix(j,i).LT.lowThold(p))cycle
            if(patt2d%iyPix(j,i).GT.HighThold(p))cycle
            
            ysum(p)=ysum(p)+patt2d%iyPix(j,i) !*patt2d%scale
            npix(p)=npix(p)+1
        end do
      end do
      
      !ara ja hauriem de tenir al vector el diagrama de pols.
      
      !tornem a fer una passada per calcular la desviacio estandar sqrt((sum(xi-xmean)^2)/N-1)
      do i=0, patt2d%nymx-1
        do j=0, patt2d%nxmx-1
          
            !si es mascara o zero el descartarem (!!que passa amb el braç?) podriem mirar si es
            !exzone en la imatge original en comptes de la intensitat (TODO)
            if(patt2d%iyPix(j,i).LE.0)cycle
            
            !mirem la 2T del pixel en la imatge per determinar amplada i angle
            !1)vector centre pixel:
            vPCx=j-patt2d%centx
            vPCy=patt2d%centy-i
            !2)calcul angle 2teta en graus
            x2 = (vPCx*patt2d%pixLx)*(vPCx*patt2d%pixLx)
            y2 = (vPCy*patt2d%pixLy)*(vPCy*patt2d%pixLy)
            radi = sqrt(x2+y2)
            t2p=atan(radi/(patt2d%sepOD*1000))/crad
            if(t2p.LT.t2ini)cycle
            !mirem a quina posicio del vector (diagrama pols) ha d'anar
            p=nint(t2p/stepsize)-nint(t2ini/stepsize)+1
            
            !mirem si est� per sobre de lowThold i per sota de highThold
            if(patt2d%iyPix(j,i).LT.lowThold(p))cycle
            if(patt2d%iyPix(j,i).GT.HighThold(p))cycle
            
            !I ARA ACUMULEM LA DESV
            xmean=real(ysum(p))/real(npix(p))
            desv(p)=desv(p)+(patt2d%iyPix(j,i)*patt2d%scale-xmean)*(patt2d%iyPix(j,i)*patt2d%scale-xmean)
        end do
      end do
      !Calcul final desviacio
      do i=1,npoints+1
        if(npix(i).LT.2)then
          desv(i)=0
          cycle
        end if
        desv(i)=sqrt(desv(i)/(real(npix(i))-1))
      end do
      
    end subroutine intRadThreshold
    
    !escriu un diagrama de pols format XY amb els parametres d'entrada seguents:
    !  - filename: nom del fitxer de sortida (sense extensiO)
    !  - t2ini,t2fin,stepsize
    !  - ysum: vector d'intensitats
    !  - npix: vector de numero de pixels que han contribuit a cada punt
    !  - desv: vector desviacions
    subroutine writeXY(filename,t2ini,t2fin,stepsize,ysum,npix,desv)
      implicit none
      
      !parametres
      character(len=*),intent(in)::filename
      real,intent(in)::t2ini,t2fin,stepsize
      integer, intent(in)::ysum(maxXYpoints),npix(maxXYpoints)
      real,intent(in)::desv(maxXYpoints)
      
      integer i,j,p
      integer npoints
      real t2
      npoints=nint((t2fin-t2ini)/stepsize)+1 !un extra incloure t2ini i t2fin
      open(unit=1,file=trim(filename)//'.xy') 
      do i=1,npoints+1
        t2=(i-1)*stepsize+t2ini
        if(debugMode)write(*,*)t2
        if(npix(i).GT.0)then
          write(1,*) t2,ysum(i)/npix(i),desv(i)
        else
          write(1,*) t2,ysum(i)
        end if
      end do
      close(1)
      
    end subroutine writeXY

    !passa els pixels mascara d'una imatge a una altra. �s una forma de
    !copiar les zones excloses. Tamb� recalcularem maxI i minI ja que es
    !poden veure afectades a la imatge dest�.
    subroutine copyMaskPixels(patt2d,dest)
      implicit none
      type(data2D),intent(in)::patt2d
      type(data2D),intent(inout)::dest
      integer i,j
      
      !reiniciem maxY i minY per la imatge dest�
      dest%maxY=0
      dest%minY=99999999
            
      do i=0, patt2d%nymx-1
        do j=0, patt2d%nxmx-1
           if(patt2d%iyPix(j,i).LT.0)dest%iyPix(j,i)=patt2d%iyPix(j,i)
           if(dest%iyPix(j,i).LT.0)cycle
           if(dest%iyPix(j,i).GT.dest%maxY)dest%maxY=dest%iyPix(j,i)
           if(dest%iyPix(j,i).LT.dest%minY)dest%minY=dest%iyPix(j,i)
        end do
      end do
    end subroutine copyMaskPixels
    
    !donat un pixel i una imatge en calcula la 2t (suposem que tenim totes les dades) en radians
    subroutine calc2T(patt2d,px,py,t2p)
      implicit none
      
      type(data2D),intent(in)::patt2d
      integer, intent(in)::px,py
      real vPCx,vPCy,x2,y2,radi
      real,intent(inout)::t2p

      !1)vector centre pixel:
      vPCx=real(px)-patt2d%centx
      vPCy=patt2d%centy-real(py)
      !2)calcul angle
      x2 = (vPCx*patt2d%pixLx)*(vPCx*patt2d%pixLx)
      y2 = (vPCy*patt2d%pixLy)*(vPCy*patt2d%pixLy)
      radi = sqrt(x2+y2)
      t2p=atan(radi/(patt2d%sepOD*1000))
    
    end subroutine calc2T
    
    !donat un pixel i una imatge en calcula la 2t (suposem que tenim totes les dades) en graus
    subroutine calc2Tdeg(patt2d,px,py,t2p)
        implicit none
        type(data2D),intent(in)::patt2d
        integer, intent(in)::px,py
        real,intent(inout)::t2p
        
        call calc2T(patt2d,px,py,t2p)
        
        t2p=t2p/crad
        
    end subroutine calc2Tdeg
    
    !donat un pixel i una aresta calcula la intensitat mitjana del quadrat centrat a aquest pixel
    subroutine calcIntSquare(patt2d,px,py,aresta,inten,self)
      implicit none
      
      type(data2D),intent(in)::patt2d
      integer, intent(in)::px,py,aresta
      integer,intent(inout)::inten
      logical, intent(in)::self
      integer hA,npix,i,j

      hA=nint(real(aresta)/2.)
      npix=0
      inten=-1
      
      do i=py-hA, py+hA
        do j=px-hA, px+hA
           !fora la imatge ciclem
           if(j.LT.0.OR.j.GE.patt2d%nxmx.OR.i.LT.0.OR.i.GE.patt2d%nymx)cycle
           !podem o no considerar el propi pixel (self)
           if(.NOT.self.AND.j.EQ.px.AND.i.EQ.py)cycle
           !comprovem si es mascara
           if(patt2d%iyPix(j,i).LT.0)cycle
           
           !si hem superat totes les comprovacions l'afegim
           inten=inten+patt2d%iyPix(j,i)
           npix=npix+1
        end do
      end do
      if(npix.GT.0)then
        inten=nint(real(inten+1)/real(npix)) !recuperem el -1 d'abans
      end if
    end subroutine calcIntSquare
    
    !subrutina que calcula la desviacio estandar de la intensitat de la imatge
    subroutine calcDesvEst(patt2d)
          type(data2D),intent(inout)::patt2d
          real desv
          integer npix
          npix=0
          desv=0
          do i=0, patt2d%nymx-1
            do j=0, patt2d%nxmx-1
              !ni zones excloses ni intensitat 0 es te en compte al promig o desvest
              if(patt2d%iyPix(j,i).GT.0)then
                npix=npix+1
                desv=desv+(patt2d%iyPix(i,j)-patt2d%iymean)**2
              end if
            end do
          end do
          patt2d%ydesvest=sqrt(desv/npix)
    end subroutine
    
    
    !subrutina que canvia els parametres de calibracio de la cap�alera d'un fitxer bin
    !si l'argument val -1 es deixa el valor original (excepte el centre que nomes es
    !canvia si setCentre val true)
    ! 60 bytes
    !    Int*4     NXMX(cols)
    !    Int*4     NYMX(rows)
    !    Real*4    SCALE
    !    Real*4    CENTX
    !    Real*4    CENTY
    !    Real*4    PIXLX
    !    Real*4    PIXLY
    !    Real*4    DISTOD
    !    Real*4    WAVEL
    !      real :: centy, centx, pixLx, pixLy, sepOD, wave
    subroutine calib(patt2d,setcentre,centX,centY,pixLx,pixLy,distOD,wavel)

        type(data2D),intent(inout)::patt2d
        logical, intent(in) :: setcentre
        real, intent(inout) :: centX,centY,pixLx,pixLy,distOD,wavel
        
        if(.not.setcentre)then
            centx = patt2d%centx
            centy = patt2d%centy
        end if
        
        if(pixLx.LT.0) pixLx = patt2d%pixLx
        if(pixLy.LT.0) pixLy = patt2d%pixLy
        if(sepOD.LT.0) sepOD = patt2d%sepOD
        if(wavel.LT.0) wavel = patt2d%wave

        !assignment
        patt2d%centx = centx
        patt2d%centy = centy
        patt2d%pixLx = pixLx
        patt2d%pixLy = pixLy
        patt2d%sepOD = sepOD
        patt2d%wave = wavel
        
    end subroutine calib
    
    
    !pixels mascara (-1) a 0
    subroutine maskToZero(patt2d)
        type(data2D),intent(inout)::patt2d
        integer i,j
          do i=0, patt2d%nymx-1
            do j=0, patt2d%nxmx-1
              if (patt2d%iyPix(i,j).LT.0)patt2d%iyPix(i,j) = 0
            end do
          end do
    
    end subroutine
    
    !pixels I=0 a mascara (I=-1)
    subroutine zeroToMask(patt2d)
        type(data2D),intent(inout)::patt2d
        integer i,j
        integer(kind=2) :: maskInt = -1

        do i=0, patt2d%nymx-1
            do j=0, patt2d%nxmx-1
              if (patt2d%iyPix(i,j).EQ.0)patt2d%iyPix(i,j) = maskInt
            end do
        end do

    end subroutine
    
end module mod_2Ddata


    

