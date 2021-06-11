      SUBROUTINE inip(jcom)
C ********************************************************************
C
C             S U B R O U T I N E    U P D T I P
C FUNCTION:
C           to read in <project> status info
C
C PARAMETERS:
C            NONE.
C
C ********************************************************************
C
$Include:'DNAMES.inc'
$Include:'dbchan.inc'
$Include:'IO.inc'
$Include:'WHMAP.inc'
$Include:'VWND.inc'
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
      INTEGER buffer , string , dbnmap , jcom(25)
C
C  SET CHANNEL NUMBER
      ichan = 55
C
C  OPEN DATA CHANNEL
C
      CALL openf(ichan,jcom,1,0,1,ier)
      IF ( ier.EQ.0 ) THEN
C
C  REWIND FILE
C
         REWIND ichan
         READ (ichan) npoint
         READ (ichan) nstrct , (istrct(i),i=1,nstrct)
         READ (ichan) macts , nprnt1 , ioin , icline , ibatch , kterm
         READ (ichan) xorgin , yorgin , xalen , yalen , xstart , 
     &                ystart , xlen , ylen , ibw
         READ (ichan) nacts , (idatyp(i),i=1,nacts)
         READ (ichan) itstuf
         READ (ichan) buffer
         READ (ichan) iunit , (ifile(i),i=1,5) , ixp , iyp
         READ (ichan) mast , lendir
         READ (ichan) iwork
         READ (ichan) idirc
         READ (ichan) idesfa
         READ (ichan) idesdt
         READ (ichan) dbnmap
         READ (ichan) name
         READ (ichan) itable
         READ (ichan) kat
C
         CLOSE (ichan,IOSTAT=ier)
      ENDIF
C
      RETURN
      END
