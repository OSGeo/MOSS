C
C*********************************************************************
C
C       NAME:           SUBROUTINE TYFIL
C
C       FUNCTION:       THIS ROUTINE TYPES OUT A LINE ORIENTED DISK FILE
C                       ONTO A CHANNEL WHICH IS NORMALLY THE CONSOLE SCREEN.
C
C       PARAMETERS:     NAME    USE    TYPE    DIMENSION    RANGE    UNITS
C                       --------------------------------------------------
C                       IIN      I     I*2         1
C                               THE CHANNEL TO OPEN FOR INPUT AND TO
C                               READ FROM.
C                               ------------------------------------------
C                       IOUT     I     I*2         1
C                               THE CHANNEL TO WRITE DATA TO WHICH IS
C                               NORMALLY @CONSOLE
C                               ------------------------------------------
C                       IFARR    I     I*2         1        USER
C                               THE NAME OF THE FILE TO OPEN AND READ FROM
C                               ------------------------------------------
C
C       FILES:          THE OPENNING OF MH+'S HELP COMMAND INFORMATION FILES.
C**********************************************************************
C
      SUBROUTINE tyfil(iin,iout,ifarr,k)
      COMMON /io    / nprnt , ioin
      DIMENSION ic(80)
      CHARACTER ifarr*18 , ig*1
      OPEN (iin,FILE=ifarr,ACCESS='SEQUENTIAL',FORM='FORMATTED',ERR=100,
     &      IOSTAT=ier)
 
 100  CONTINUE
      IF ( ier.NE.0 ) THEN
C
C        ERROR RETURN.
C
         WRITE (iout,99001) ifarr , ier
99001    FORMAT (' *TYFIL*  COULD NOT OPEN TEXT FILE ',A18,'  ERROR: ',
     &           I5)
         GOTO 500
      ELSE
C
         iend = 0
         lines = 0
C
C        FOR A PRINTING OF A TEXTUAL DISK FILE
C
         READ (iin,'(79a1)',END=400) (ic(l),l=1,79)
      ENDIF
cssk      go to 21
cssk   30 iend=1
 200  CONTINUE
      DO l = 79 , 1 , -1
         IF ( ic(l).NE.32 ) GOTO 300
      ENDDO
 300  CONTINUE
      WRITE (*,'(1x,79a1)') (ic(k),k=1,l)
      IF ( iend.NE.1 ) THEN
         lines = lines + 1
         IF ( lines.EQ.22 ) THEN
            WRITE (*,99002)
99002       FORMAT (' Continue Y/N (cr=Y)'\)
            READ (*,'(a)') ig
            IF ( ig.LT.ichar('32') .OR. ig.EQ.'Y' .OR. ig.EQ.'y' ) THEN
               lines = 0
               WRITE (*,*) char(27) , char(12)
               WRITE (*,'(1x,79a1)') (ic(k),k=1,l)
               lines = lines + 1
               READ (iin,'(79a1)',END=400) (ic(l),l=1,79)
               GOTO 200
            ENDIF
         ELSE
            READ (iin,'(79a1)',END=400) (ic(l),l=1,79)
            GOTO 200
         ENDIF
      ENDIF
C
C  STOP TYPING FILE
C
 400  CONTINUE
      CLOSE (iin,IOSTAT=ier)
C
C        RETURN.
C
 500  CONTINUE
      RETURN
      END
