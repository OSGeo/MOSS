      SUBROUTINE jkpack(chia,m,iy,n)
C***
C***  THIS ROUTINE PACKS 5 QUALIFYING CODES INTO 1
C***  TWO-BYTE WORD, OCCUPYING AS MANY AS NECESSARY TO
C***  PACK M CODES.
C***     CHIA  - ARRAY OF UNPACKED CODES
C***     M     - NUMBER OF CODES CONTAINED IN ARRAY CHIA
C***     IY    - ARRAY OF PACKED CODES (N WORDS OF 5 CODES PER WORD)
C***     N     - NUMBER OF WORDS IN ARRAY IY CONTAINING PACKED CODES
C***
      DIMENSION iy(99)
      CHARACTER*1 chia(m) , ic(8) , icl(8)
      DATA ic/' ' , 'B' , 'L' , 'N' , 'G' , 'T' , 'H' , '*'/
      DATA icl/' ' , 'b' , 'l' , 'n' , 'g' , 't' , 'h' , '*'/
      n = (m+4)/5
      DO i = 1 , m
         DO j = 2 , 7
            IF ( chia(i).EQ.icl(j) ) THEN
               chia(i) = ic(j)
               GOTO 100
            ENDIF
         ENDDO
 100  ENDDO
      n1 = 5
      DO i = 1 , n
         iy(i) = 0
         IF ( i.EQ.n ) THEN
            n1 = 5*(1-n) + m
         ENDIF
         DO j = 1 , n1
            kx = 0
            DO k = 1 , 8
               IF ( chia(5*(i-1)+j).EQ.ic(k) ) THEN
                  kx = k
               ENDIF
            ENDDO
            iy(i) = 8*iy(i) + kx - 1
         ENDDO
      ENDDO
      RETURN
      END
