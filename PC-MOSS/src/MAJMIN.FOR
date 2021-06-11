      SUBROUTINE majmin(jump,ier)
      COMMON /boprms/ iud , npts1 , npts2 , nxyi , nlobe , klue , 
     &                nlist , iflgg
      COMMON /dot   / ih , jh , ids(353) , jds(353)
$Include:'io.inc'
$Include:'xypnts.500'
$Include:'pqpnts.500'
$Include:'xyxy.500'
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C                S U B R O U T I N E   M A J M I N
C
C
C  FUNCTION:  RESOLVE CASE 2 (KLUE=2) FOR POLYGON OVERLAY.  THIS IS
C             THE CASE IN WHICH THE MAJOR POLYGON IS ENTIRELY IN
C             THE MINOR POLYGON
C
C  INPUT PARAMETERS:
C
C             JUMP  =  0 = ISLANDS TO PROCESS
C                      1 = ISLANDS TO PROCESS
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C  CHECK FOR NOT CASE (IF A NOT B, THEN MAJOR IS THROUGH PROCESSING
C  IF THERE ARE NO ISLANDS)
C
      ier = 0
      IF ( jh.LE.0 .AND. iud.EQ.3 ) RETURN
C
C
C  CHECK FOR ISLAND PROCESSING
C
      IF ( iud.NE.2 .OR. ih.GT.0 ) THEN
         IF ( jh.GT.0 ) THEN
C
C
            loop = npts1
            IF ( iud.EQ.2 ) loop = npts2
C
            DO i = 1 , npts1
C
               IF ( iud.EQ.2 ) THEN
C
C        CALL PUTXY(1,I,P(I))
C        CALL PUTXY(2,I,Q(I))
                  CALL sspoints('P ',p,pi,i,1)
                  CALL sspoints('Q ',q,qi,i,1)
c        xy1(i)=p(i)
c        xy2(i)=q(i)
                  CALL sspoints('X1',xy1,pi,i,2)
                  CALL sspoints('X2',xy2,qi,i,2)
               ELSE
C
C        CALL PUTXY(1,I,X(I))
C        CALL PUTXY(2,I,Y(I))
                  CALL sspoints('X ',x,xi,i,1)
                  CALL sspoints('Y ',y,yi,i,1)
c        xy1(i)=x(i)
c        xy2(i)=y(i)
                  CALL sspoints('X1',xy1,xi,i,2)
                  CALL sspoints('X2',xy2,yi,i,2)
               ENDIF
C
            ENDDO
C
C  SET VARIABLES FOR FURTHER PROCESSING
C
            nlobe = 1
            nxyi = npts1
            IF ( iud.EQ.2 ) nxyi = npts2
            CALL sspoints('X1',xy1,xt,nxyi,1)
            xt = -xt
c      XT=-XY1(NXYI)
C      CALL PUTXY(1,NXYI,XT)
c      xy1(nxyi)=xt
            CALL sspoints('X1',xy1,xt,nxyi,2)
C
            jump = 1
            ier = 0
            GOTO 100
         ENDIF
      ENDIF
C
C  NO ISLANDS.  LOAD PROPER FROM COORDINATE ARRAYS TO OUTPUT
C  ARRAY FOR CALL TO INOUT
C
      npts = npts1
      IF ( ih.GT.0 ) npts = ids(ih+1)
C iud is always 1
C
      CALL inout1(npts,ids,ih,1)
C      IF(IUD.EQ.1) CALL INOUT1(NPTS,IDS,IH,1)
C      IF(IUD.EQ.2) CALL INOUT1(NPTS,JDS,JH,2)
C
      jump = 0
C
      ier = 0
C
C
 100  CONTINUE
      RETURN
      END
