      SUBROUTINE chrjst(string,kode)
C-----------------------------------------------------------------------
C
C   CHRJST    CHARACTER-JUSTIFY
C
C  Purpose:
C
C      This routine justifies the character string either left,
C    center, and right.
C
C  Parameters:
C
C    STRING    (I/O) = (Character[*]) The character string to justify
C    KODE    (Input) = (Integer) The justify code
C                                  -1 = Left
C                                   0 = Center
C                                   1 = Right
C
C  MODIFIED: S.K. GOODMAN, AUTOMETRIC, MAY 88 ( PC - MOSS )
C-----------------------------------------------------------------------
C
      CHARACTER*(*) string
C
      INTEGER kode , bblank , tblank , length , i , j , k , l , m , n
C
C Determine length of string
C
      length = len(string)
      IF ( length.GT.1 ) THEN
C
C Fine number of beginning and trailing blanks
C
         bblank = 0
         DO i = 1 , length
            IF ( string(i:i).NE.' ' ) THEN
               bblank = i - 1
               GOTO 50
            ENDIF
         ENDDO
C
C Check for all blanks
C
 50      CONTINUE
         IF ( bblank.NE.length ) THEN
C
            tblank = 0
            DO i = 1 , length
               j = length + 1 - i
               IF ( string(j:j).NE.' ' ) THEN
                  tblank = i - 1
                  GOTO 60
               ENDIF
            ENDDO
C
C Now branck to correct KODE
C
 60         CONTINUE
            IF ( kode.LT.0 ) THEN
C
C Left justify
C
               IF ( bblank.GT.0 ) THEN
                  j = bblank + 1
                  string = string(j:)
               ENDIF
               RETURN
            ELSEIF ( kode.EQ.0 ) THEN
C
C Center justify
C
               IF ( iabs(bblank-tblank).GT.1 ) THEN
                  i = (bblank+tblank)/2
                  IF ( i.LT.bblank ) THEN
                     j = bblank + 1
                     string(i:) = string(j:)
                  ELSE
                     k = length - bblank - tblank
                     i = length - i
                     j = length - tblank
                     DO l = 1 , k
                        m = i - l + 1
                        n = j - l + 1
                        string(m:m) = string(n:n)
                     ENDDO
                     m = i - k
                     string(:m) = ' '
                     m = i + 1
                     string(m:) = ' '
                  ENDIF
               ENDIF
               RETURN
            ELSE
C
C Right justify
C
               IF ( tblank.GT.0 ) THEN
                  k = length - bblank - tblank
                  j = length - tblank
                  DO l = 1 , k
                     m = length - l + 1
                     n = j - l + 1
                     string(m:m) = string(n:n)
                  ENDDO
                  m = length - k
                  string(:m) = ' '
               ENDIF
               RETURN
            ENDIF
C
         ENDIF
      ENDIF
C
      END
