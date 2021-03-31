C ********************************************************************
C
C             S U B R O U T I N E    outip
C FUNCTION:
C          update <project> status infor file
C
C PARAMETERS:
C            NONE.
C ROUTINES
C CALLED:
C
C ********************************************************************
C
      SUBROUTINE outip(intrfl)
C
$Include:'DNAMES.inc'
$Include:'dbchan.inc'
$Include:'IO.inc'
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
      INTEGER intrfl(25)
C  SET CHANNEL NUMBER
C
      ichan = 55
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
         WRITE (ichan) npoint
         WRITE (ichan) nstrct , (istrct(i),i=1,nstrct)
         WRITE (ichan) macts , nprnt , ioin , icline , ibatch , kterm
         WRITE (ichan) xorgin , yorgin , xalen , yalen , xstart , 
     &                 ystart , xlen , ylen , ibw
         WRITE (ichan) nacts , (idatyp(i),i=1,nacts)
         WRITE (ichan) itstuf
         WRITE (ichan) buffer
         WRITE (ichan) iunit , (ifile(i),i=1,5) , ixp , iyp
         WRITE (ichan) mast , lendir
         WRITE (ichan) iwork
         WRITE (ichan) idirc
         WRITE (ichan) idesfa
         WRITE (ichan) idesdt
         WRITE (ichan) dbnmap
         WRITE (ichan) name
         WRITE (ichan) itable
         WRITE (ichan) kat
C
         CLOSE (ichan,IOSTAT=ier)
         RETURN
      ENDIF
      END
