      SUBROUTINE fnum(icard,ndim,tnum,k)
      COMMON /io    / nprnt , ioin
      DIMENSION icard(ndim)
      LOGICAL mcomp
      LOGICAL lper , lend , nega
      DOUBLE PRECISION rnum , tkk
C
C *************************************************************
C
C           S U B R O U T I N E    F N U M
C
C  FUNCTION: CONVERT A CHARACTER STRING TO A FLOATING POINT
C             NUMBER.
C PARAMETERS:
C             ICARD     IN - CHARACTER STRING ARRAY TO BE CONVERTED.
C             NDIM      IN - ICARD ARRAY SIZE.
C             TNUM      OUT- FLOATING POINT NUMBER.
C             K         IN - STARTING POSITION OF NUMBER IN THE ARRAY.
C ROUTINES
C CALLED:     NONE.
C
C ************************************************************
C
      tkk = 1D0
      lend = .FALSE.
      lper = .FALSE.
      nega = .FALSE.
      rnum = 0.0
      num = 0
      IF ( k.EQ.0 ) k = 1
      IF ( k.NE.1 ) THEN
         k = k + 1
      ENDIF
C
C  CHECK FOR NEGATIVE NUMBER OR ASSUME POSITIVE
C
 
      IF ( mcomp(icard(k),'-',1,ier) ) nega = .TRUE.
      IF ( mcomp(icard(k),'+',1,ier) ) k = k + 1
      IF ( nega ) k = k + 1
C  LESS THAN ASCII PERIOD OR GREATER THAN ASCII 9
 100  CONTINUE
      IF ( icard(k).LT.46 .OR. icard(k).GT.57 ) THEN
C
C  FINISHED COMPILING THE NUMBER.
C
         IF ( tkk.GT.1D0 ) tkk = tkk/10D0
         rnum = rnum/tkk
         IF ( nega ) rnum = -rnum
C**      CONVERT TO FLOATING POINT
         tnum = sngl(rnum)
         RETURN
      ELSE
         IF ( mcomp(icard(k),'.',1,ier) ) lper = .TRUE.
         IF ( lper ) tkk = tkk*10D0
         IF ( tkk.NE.10D0 ) THEN
            rnum = rnum*dble(10.)
C  SUBTRACT ASCII ZERO
            rnum = rnum + dble(icard(k)-48)
         ENDIF
C
C  BEGIN COMPILING THE NUMBER
C
         k = k + 1
         GOTO 100
      ENDIF
      END
