      SUBROUTINE readin(icard,idim,istart,iend)
C
C ********************************************************
C
C            S U B R O U T I NE    R E A D I N
C FUNCTION:
C            READ A STRING OF CHARACTERS FROM THE INPUT
C            BUFFER.
C PARAMETERS:
C             ICARD     OUT - THE STRING OF CHARACTERS READ IN,
C                             THE OUTPUT ARRAY.
C             IDIM      IN  - DIMENSION OF ICARD
C             istart    OUT - THE POSITION WHERE THE FIRST NONBLANK
C                             CHARACTER OCCURS.
C             iend      out - last non-blank position
C
C ROUTINES
C CALLED:     FILLC,ISHFT
C
C
C *******************************************************
C
      COMMON /io    / nprnt , ioin
      DIMENSION icard(idim)
C
C  READ A LINE
C
      istart = 0
 
      IF ( ioin.LE.6 ) WRITE (nprnt,99001)
99001 FORMAT (/,1X,'? ',\)
      READ (ioin,99002,END=200,IOSTAT=ier) icard
99002 FORMAT (132A1)
C
C  NULL FILL LEFT BYTE (IBM F77 PROBLEM)
C
      DO l = 1 , idim
         icard(l) = ishft(icard(l),8)
         icard(l) = ishft(icard(l),-8)
C
C  CONVER TO UPPER CASE
C
         IF ( icard(l).GT.96 .AND. icard(l).LT.123 ) icard(l) = icard(l)
     &         - 32
      ENDDO
C
      DO l = idim , 1 , -1
         IF ( icard(l).NE.32 ) GOTO 100
C  NULL FILL UNUSED
         icard(l) = ishft(icard(l),-8)
      ENDDO
C
C  capture last non-blank position
C
 100  CONTINUE
      iposl = l
C
C
C  A TRUE STRING.  SET ISTART TO FIRST NON BLANK CHARACTER
C
      istart = 1
      iend = iposl
      RETURN
C
C
 200  CONTINUE
      istart = 0
      iend = 0
      WRITE (nprnt,99003)
99003 FORMAT (' *READIN*  END OF INPUT FILE ENCOUNTERED')
      RETURN
C
      END
