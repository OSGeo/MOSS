      SUBROUTINE boolog(maxfea,kitty,istak,nsymb)
      INTEGER kitty(1000) , istak(128) , lhits(1000,2) , aceses
      LOGICAL full1 , full2
C  AND HERE WE GO <>
      i91 = 91
      loc1 = 1
      loc2 = 2
      full1 = .FALSE.
      full2 = .FALSE.
      igo = 0
      iostck = 0
      aceses = 0
      ks = 0
 100  CONTINUE
      igo = igo + 1
      idown = istak(igo)
      IF ( idown.LT.0 ) THEN
         IF ( idown.NE.-5 ) THEN
         ENDIF
         IF ( .NOT.(full2) ) THEN
            READ (i91,REC=128+iostck) kitty
            DO kkl = 1 , maxfea
               lhits(kkl,loc2) = kitty(kkl)
            ENDDO
            iostck = iostck - 1
         ENDIF
         IF ( idown.NE.-4 ) THEN
            IF ( idown.NE.-3 ) GOTO 300
            DO j = 1 , maxfea
               lhits(j,loc1) = ior(lhits(j,loc1),lhits(j,loc2))
            ENDDO
         ELSE
            DO j = 1 , maxfea
               lhits(j,loc1) = iand(lhits(j,loc1),lhits(j,loc2))
            ENDDO
         ENDIF
      ELSE
         ks = ks + 1
         IF ( full1 ) THEN
            i = loc1
            loc1 = loc2
            loc2 = i
            IF ( full2 ) THEN
               iostck = iostck + 1
               aceses = aceses + 1
               DO kl = 1 , maxfea
                  kitty(kl) = lhits(kl,loc1)
               ENDDO
               WRITE (i91,REC=128+iostck) kitty
            ELSE
               full2 = .TRUE.
            ENDIF
         ELSE
            full1 = .TRUE.
         ENDIF
         READ (i91,REC=idown) kitty
         DO kkl = 1 , maxfea
            lhits(kkl,loc1) = kitty(kkl)
         ENDDO
         IF ( igo.GE.nsymb ) GOTO 200
         GOTO 100
      ENDIF
      full2 = .FALSE.
      IF ( igo.LT.nsymb ) GOTO 100
 200  CONTINUE
      IF ( .NOT.(iostck.NE.0 .OR. full2) ) THEN
         IF ( loc1.NE.1 ) THEN
            DO j = 1 , maxfea
               lhits(j,1) = lhits(j,2)
            ENDDO
         ENDIF
C
C  ACTIVATE POLYGONS
C
         DO i = 1 , maxfea
            kitty(i) = lhits(i,1)
         ENDDO
         RETURN
      ENDIF
 300  CONTINUE
      WRITE (*,*) ' internal consistency error - '
      RETURN
 
C
      END
