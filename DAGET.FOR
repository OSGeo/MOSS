      SUBROUTINE daget(icard,k,jtable,ndim,macts)
C ***********************************************
C
C        S U B R O U T I N E     D A G E T
C
C FUNCTION: EXTRACT A STRING OF INTEGERS
C           CONTAINED IN ICARD AND STORE THEM IN JTABLE
C PARAMETERS:
C     ICARD   IN - ARRAY CONTAINING THE INTEGER NUMBERS
C                  IN CHARARCTER FORMAT
C     K       IN - POINTER TO STARTING LOCATION IN
C                  ICARD
C     JTABLE  OUT- ARRAY  POINTERS TO ISTRCT ARE STORED
C     NDIM    IN - LENGTH OF JTABLE
C     MACTS   IN - RETURNED NUMBER OF INTEGERS IN ICARD
C ROUTINES
C CALLED:    INUM.
C
C *************************************************
C
$Include:'IO.inc'
$Include:'WHMAP.inc'
      DIMENSION icard(80) , jtable(ndim)
      DIMENSION icom(10) , jcom(10)
      LOGICAL ino
C
c
c  if macts comes in neg. means only numbers can be checked
c  not a mapname
c
      ino = .FALSE.
      IF ( macts.EQ.-1 ) ino = .TRUE.
      macts = 0
C
C   If input is a character
      IF ( icard(k+1).GE.65 ) THEN
C
C
C  MAP NAME ENTRY
C
         IF ( ino ) RETURN
         CALL decip(icard,icom,k)
         CALL packc(icom,jcom,10)
         DO i = 1 , nstrct , 7
            DO j = 1 , 5
               IF ( iabs(istrct(i+1+j)).NE.jcom(j) ) GOTO 50
            ENDDO
            jtable(1) = i
            macts = 1
            WRITE (*,*)
            WRITE (*,99001) (jcom(l),l=1,5)
99001       FORMAT (1X,'Using FIRST occurrence of ',5A2,
     &              ' in ACTIVE table')
            GOTO 200
 50      ENDDO
         WRITE (*,99002) (jcom(l),l=1,5)
99002    FORMAT (/,1X,5A2,
     &           ' not found in ACTIVE table - Use SELECT first')
         STOP '   '
      ELSE
C
 100     CONTINUE
         CALL inum(icard,numa,k)
C
C  CHECK FOR VALID ID
C
         IF ( nstrct/7.EQ.0 ) THEN
            WRITE (*,*) 'NO ACTIVE DATA SETS - Use SELECT first'
            STOP '   '
         ELSEIF ( numa.NE.0 ) THEN
            IF ( numa.GT.(nstrct/7) .OR. numa.LT.0 ) THEN
C
C   Illegal id
               WRITE (nprnt,99003) numa
99003          FORMAT (' ILLEGAL ACTIVE DATA SET ID: ',I5)
               lines = lines + 2
            ELSE
               macts = macts + 1
               index = ((numa-1)*7) + 1
               jtable(macts) = index
               IF ( icard(k+1).LT.48 .OR. icard(k+1).GT.57 ) GOTO 200
               IF ( ndim.EQ.1 ) GOTO 200
            ENDIF
            GOTO 100
         ENDIF
      ENDIF
C
 200  CONTINUE
      RETURN
      END
