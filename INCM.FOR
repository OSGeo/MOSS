      SUBROUTINE incm
C ********************************************************************
C
C             S U B R O U T I N E    I N C M
C FUNCTION:
C           READ IN RUNTIME COMMON BLOCKS
C
C PARAMETERS:
C            NONE.
C ROUTINES
C CALLED:    RDSEQ.
C
C ********************************************************************
C
$Include:'DNAMES.inc'
$Include:'dbchan.inc'
$Include:'IO.inc'
$Include:'STRNG.inc'
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
      INTEGER buffer , string , dbnmap
      INTEGER intrproc(5) , jcom(25)
C
      DATA intrproc/'IN' , 'TR' , 'PR' , 'OC' , '  '/
C
C  SET CHANNEL NUMBER
      ichan = 50
C
C  OPEN DATA CHANNEL
C
      CALL openf(ichan,intrproc,1,0,1,ier)
      IF ( ier.EQ.0 ) THEN
C
C  REWIND FILE
C
         REWIND ichan
         READ (ichan) mast , lendir
         READ (ichan) iwork
         READ (ichan) idirc
         READ (ichan) idesfa
         READ (ichan) idesdt
C
         icardp = 0
         DO l = 1 , 80
            icard(l) = 0
         ENDDO
         CLOSE (ichan,IOSTAT=ier)
         DO l = 1 , 12
            IF ( iwork(l).EQ.46 ) GOTO 50
            jcom(l) = iwork(l)
         ENDDO
C
 50      CONTINUE
         jcom(l) = ichar('.')
         jcom(l+1) = ichar('I')
         jcom(l+2) = ichar('P')
c
         CALL inip(jcom)
C
         CLOSE (ichan)
      ENDIF
      RETURN
      END
