C*******************************************************************
C
C     PROGRAM    DEVICE
C
C   to set up the graphics output device
C
C       default  :   <crt>     (iunit=6)
C       (optional):  <filename> (iunit=60)
C                 :  <*> or <alt> (iunit=61) (screen other than crt)
C******************************************************************
      PROGRAM device
C
$Include:'STRNG.inc'
      COMMON /fls   / iunit , ifile(5)
C
      CALL initl(ier)
      IF ( ier.NE.0 ) STOP
      CALL devget(icard,icardp)
      CALL outcm
      END
