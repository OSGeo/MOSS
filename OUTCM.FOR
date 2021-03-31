C ********************************************************************
C
C             S U B R O U T I N E    O U T C M
C FUNCTION:
C           WRITE OUT RUNTIME COMMON BLOCKS
C
C PARAMETERS:
C            NONE.
C ROUTINES
C CALLED:
C
C ********************************************************************
C
      SUBROUTINE outcm
C
$Include:'DNAMES.inc'
$Include:'dbchan.inc'
$Include:'IO.inc'
$Include:'STRNG.inc'
$Include:'VWND.inc'
$Include:'WHMAP.inc'
      COMMON /pntfet/ npoint
      COMMON /attfet/ kat(86) , lbat
      COMMON /plact / macts
      COMMON /type  / idatyp(40) , nacts
      COMMON /tktrnx/ itstuf(66)
      COMMON /output/ buffer(132) , ipt , iwrd , string(2)
      COMMON /fls   / iunit , ifile(5)
      COMMON /pplacc/ ixp , iyp
      COMMON /table / itable(40)
      COMMON /clines/ icline , ibatch
      COMMON /whatterm/ kterm
      COMMON /dbmap / dbnmap(2)
      COMMON /user  / name(10)
      INTEGER buffer , string , dbnmap
      INTEGER intrfl(25) , jcom(25)
C  SET CHANNEL NUMBER
C
      ichan = 50
      intrfl(1) = 'IN'
      intrfl(2) = 'TR'
      intrfl(3) = 'PR'
      intrfl(4) = 'OC'
      intrfl(5) = 32
C
C  OPEN DATA CHANNEL
C
      CALL openf(ichan,intrfl,2,0,1,ier)
      IF ( ier.NE.0 ) THEN
C
C  ERROR RETURN
C
         WRITE (nprnt,99001) ier
99001    FORMAT (' *OUTCM* ERROR FROM OPEN OF FILE INTERPRO  ERROR',I5)
         STOP
      ELSE
C
C  REWIND FILE
C
         REWIND ichan
         WRITE (ichan) mast , lendir
         WRITE (ichan) iwork
         WRITE (ichan) idirc
         WRITE (ichan) idesfa
         WRITE (ichan) idesdt
C
         DO l = 1 , 12
            IF ( iwork(l).EQ.46 ) GOTO 50
            jcom(l) = iwork(l)
         ENDDO
C update project interprocess file
 50      CONTINUE
         jcom(l) = ichar('.')
         jcom(l+1) = ichar('I')
         jcom(l+2) = ichar('P')
c
         CALL outip(jcom)
C
         CLOSE (ichan,IOSTAT=ier)
         RETURN
      ENDIF
      END
