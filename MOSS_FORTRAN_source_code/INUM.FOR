      SUBROUTINE inum(icard,num,k)
      COMMON /io    / nprnt , ioin
      DIMENSION icard(*)
      LOGICAL mcomp
C
C *******************************************************************
C
C           S U B R O U T I N E    I N U M
C
C FUNCTION:   CONVERT A CHARACTER STRING TO AN INTEGER NUMBER.
C
C PARAMETERS:
C             ICARD     IN  - THE CHARACTER ARRAY CONTAINING STRING.
C             NUM       OUT - THE INTEGER NUMBER.
C             K         IN  - THE POSITION AT START OF STRING IN ARRAY.
C ROUTINES
C CALLED:     READIN.
C
C *******************************************************************
C
C  SET RETURNED VARAIBLE TO ZERO
C
      num = 0
      nega = 0
      IF ( k.EQ.0 ) k = 1
      IF ( k.NE.1 ) THEN
         k = k + 1
      ENDIF
 100  CONTINUE
      IF ( k.GT.80 ) THEN
C
C  EXTEND TO NEXT LINE
C
         WRITE (nprnt,99001)
99001    FORMAT (' ENTER NEW LINE OF DATA ')
         CALL readin(icard,80,k,istop)
         k = k + 1
         GOTO 100
      ELSEIF ( .NOT.(mcomp(icard(k),',',1,ier)) ) THEN
         IF ( .NOT.(mcomp(icard(k),' ',1,ier)) ) THEN
            IF ( .NOT.(mcomp(icard(k),'.',1,ier)) ) THEN
               IF ( .NOT.(mcomp(icard(k),';',1,ier)) ) THEN
                  IF ( icard(k).NE.0 ) THEN
                     IF ( icard(k).LT.45 ) GOTO 200
                     IF ( icard(k).GT.57 ) GOTO 200
C CHECK FOR NEGATIVE SIGN
                     IF ( icard(k).EQ.45 ) nega = 1
                     IF ( icard(k).NE.45 ) THEN
                        num = num*10
                        num = num + (icard(k)-48)
                     ENDIF
                     k = k + 1
                     GOTO 100
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF ( nega.EQ.1 ) num = -num
      RETURN
C  error - non numeric found
 200  CONTINUE
      k = -1
      RETURN
C
      END
