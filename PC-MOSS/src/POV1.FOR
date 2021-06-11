      SUBROUTINE pov1(ipromt,merlab,iud,map,jcom,mlpnt,nchars,irec)
$Include:'strng.inc'
      COMMON /osave / idum(19) , ictsub
$Include:'io.inc'
      DIMENSION map(10) , jcom(10) , icom(25)
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C                   S U B R O U T I N E    P O V 1
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C
C
C  SET CHANNEL FOR SUBJECT FILE
C
      ictsub = 57
C
C  GET ATTRIBUTE MERGE TYPE FROM USER
C
      CALL asksub(ipromt,merlab,mlpnt,nchars,irec,ier)
C
C  GET OVERLAY TYPE
C
      iud = 1
C      WRITE(NPRNT,1000)
C 1000 FORMAT(' Enter: 1 for Intersection, 2 for Union, 3 for Not'\)
C      CALL READIN(ICARD,lenicd,icardp,istop)
C      CALL INUM(ICARD,IUD,icardp)
C      IF(IUD.LT.0.OR.IUD.GT.3) GO TO 20
C
C  PACK FILE NAME
C
      CALL packc(map,jcom,10)
C
C  OPEN LINK TO TEMP & TEMP1
      icom(1) = 'B '
      CALL openf(ictsub,icom,3,256,2,ier)
      IF ( ier.NE.0 ) CALL perr(9)
      icom(1) = 'C '
      CALL openf(56,icom,3,0,1,ier)
      IF ( ier.NE.0 ) CALL perr(12)
C
C
C
      RETURN
      END
