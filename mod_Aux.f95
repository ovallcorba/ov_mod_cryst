module mod_Aux

! by OV (20-09-2013)

    real crad,pi !crad=pi/180
    character(len=75)::line75,line75eq
    DATA pi/3.1415926535897932/crad/0.0174532925199433/
    DATA line75/'---------------------------------------------------------------------------'/
    DATA line75eq/'==========================================================================='/
    
contains

    ! funcio que calcula si 3 punts estan en counterclockwise
    function ccw(x1,y1,x2,y2,x3,y3)
      integer,intent(in)::x1,y1,x2,y2,x3,y3
      logical :: ccw
      integer ccwval1
      ccw=.false.
      ccwval1 = (y3-y1)*(x2-x1)
      ccwval2 = (y2-y1)*(x3-x1)
      !debug
!      write(*,*) 'ccwV1= ',ccwval1
!      write(*,*) 'ccwV2= ',ccwval2
      if(ccwval1.GT.ccwval2)ccw=.true.
      return
    end function ccw
    
    !x,y es el punt
    !sxy1,2 son els punts que limiten el segment
    !ATENCIO el segment ha de ser vertical o horitzontal
    function inSegment(x,y,sx1,sy1,sx2,sy2)
      integer,intent(in)::x,y,sx1,sy1,sx2,sy2
      logical :: inSegment
      inSegment=.false.
      if(sx1.EQ.sx2)then
        !segment vertical, comprovarem la y
        if(x.EQ.sx1.AND.y.GE.min(sy1,sy2).AND.y.LE.max(sy1,sy2))inSegment=.true.
      end if
      if(sy1.EQ.sy2)then
        !segment horitzontal, comprovarem la x
        if(y.EQ.sy1.AND.x.GE.min(sx1,sx2).AND.x.LE.max(sx1,sx2))inSegment=.true.
      end if
      return
    end function inSegment
    
    
    !escriu missatge per pantalla i opcionalment a fitxer de text (si wToFile=true)
    subroutine sub_wMsg(timestamp,message,wToFile,filename)
        implicit none
        CHARACTER(LEN=*) message
        integer timestamp !0= no, 1+=si
        !farem un timestamp del tipus [00h00m00s], es a dir 11 carcters 
        CHARACTER(LEN=12) time
        INTEGER :: dt(8)
        LOGICAL wToFile,lexist
        CHARACTER(LEN=*) filename
        
        If (timestamp.NE.0) THEN
          CALL date_and_time(values=dt)
          WRITE(time,'(A1,I2,A1,I2,A1,I2,A2)') '[',dt(5),'h',dt(6),'m',dt(7),'s]'
          WRITE(*,'(1X,A11,1X,A)')time,trim(message)
        ELSE
          WRITE(*,'(A)')trim(message)
        END IF
        
        !buidem el buffer d'escritura (util per java)
        call flush(6)
        
        IF(wToFile)then
          inquire (file=trim(filename),exist=lexist)
          if(lexist)THEN
            OPEN(UNIT=4,FILE=trim(filename),STATUS='OLD',POSITION='APPEND')
          else
            OPEN(UNIT=4,FILE=trim(filename),STATUS='NEW')
          end if
          If (timestamp.NE.0) THEN
            WRITE(4,'(1X,A11,1X,A)')time,trim(message)
          ELSE
            WRITE(4,'(A)')trim(message)
          END IF
          CLOSE(4)
        end if

    end subroutine sub_wMsg

    !Escriu el text envoltat de guions (-) i entre dues linies de 75 guions (-)
    subroutine titleOld(msg,timestamp,wToFile,filename)
      implicit none
      CHARACTER(LEN=*),intent(in):: msg
      CHARACTER(LEN=*) filename
      logical wToFile
      integer timestamp
      integer lmsg,lguions,i
      CHARACTER(LEN=38)::guions
      CHARACTER(LEN=75)::linia

      lmsg=len_trim(msg)+2 !+ un espai per banda
      if(lmsg.GE.75)then
        call sub_wMsg(timestamp,line75,wToFile,filename)
        call sub_wMsg(timestamp,msg,wToFile,filename)
        call sub_wMsg(timestamp,line75,wToFile,filename)
        return
      end if
      lguions=nint(real(75-lmsg)/2.)
      do i=1,lguions
        guions(i:i) = '-'
      end do
      do i=lguions+1,38
        guions(i:i) = ' '
      end do
      call sub_wMsg(timestamp,line75,wToFile,filename)
      if(mod(lguions,2).EQ.0)then
        linia=trim(guions)//' '//trim(msg)//' '//trim(guions)
      else
        linia=trim(guions)//' '//trim(msg)//' '//trim(guions)
      end if
      call sub_wMsg(timestamp,linia,wToFile,filename)
      call sub_wMsg(timestamp,line75,wToFile,filename)
    end subroutine titleOld

    !Escriu el text envoltat d'espais i entre dues linies de 75 iguals (=) i guions (-)
    subroutine title(msg,timestamp,wToFile,filename)
      implicit none
      CHARACTER(LEN=*),intent(in):: msg
      CHARACTER(LEN=*) filename
      logical wToFile
      integer timestamp
      integer lmsg,lguions,i
      CHARACTER(LEN=38)::fmtEspais,nEspais
      CHARACTER(LEN=75)::linia

      lmsg=len_trim(msg)+2 !+ un espai per banda
      if(lmsg.GE.75)then
        call sub_wMsg(timestamp,line75eq,wToFile,filename)
        call sub_wMsg(timestamp,msg,wToFile,filename)
        call sub_wMsg(timestamp,line75,wToFile,filename)
        return
      end if
      lguions=nint(real(75-lmsg)/2.)
      !lguions conte el num d'espais que hem de posar al davant
      call sub_wMsg(timestamp,line75eq,wToFile,filename)
      write(nEspais,'(i0)')lguions
      fmtEspais = '('//trim(nEspais)//'x,a)'
      write(linia,fmtEspais)trim(msg)
      call sub_wMsg(timestamp,linia,wToFile,filename)
      call sub_wMsg(timestamp,line75,wToFile,filename)
    end subroutine title
    
    !funcio que retorna l'extensio d'un fitxer
    function getExt (filename)
        implicit none
        character(LEN=*), intent (in) :: filename
        character(LEN=5) :: getExt !donem marge per extensions de 5 caracters
        integer i
        
        !si no hi ha extensio torna cadena buida
        getExt='' 
        !busquem el . al nom del fitxer comen�ant per darrera
        i=index(filename,'.',.true.)
        if(i.GT.0)then
          !hi ha extensio
          getExt=filename(i+1:(LEN_TRIM(filename)))
        end if
        return
    end function
    
    !funcio que retorna el nom del fitxer SENSE extensio
    function noExt (filename)
        implicit none
        character(LEN=*), intent (in) :: filename
        character(LEN=128) :: noExt
        integer i
         !busquem el . al nom del fitxer comen�ant per darrera
        i=index(filename,'.',.true.)
        if(i.GT.0)then
          !hi ha extensio
          noExt=filename(1:i-1)
        else
          !no hi ha extensio, tornem el nom complert
          noExt=filename
        end if
        return
    end function
    
end module mod_Aux

