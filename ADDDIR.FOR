      SUBROUTINE adddir(icom,mapout)
C****************************************************************************
C   ROUTINE ADDS THE DIRECTORY IN FRONT OF A MAP NAME.
C
C   PARAMETER:  MAPIN  - IN  -  MAP NAME TO BE ADDED TO
C               MAPOUT - OUT -  MAP NAME WITH PATH ADDED
C****************************************************************************
C
$Include:'DNAMES.inc'
      INTEGER icom(25) , mapin(25) , mapout(25) , maphold(25)
C
      index = 1
      ipack = 0
      CALL clear(mapout,25)
      CALL clear(mapin,25)
      DO il = 1 , 10
         mapin(il) = icom(il)
      ENDDO
      IF ( mapin(1).LT.0 ) THEN
         IF ( iabs(mapin(1)).GT.256 ) THEN
            CALL unpack(mapin,24)
            ipack = 1
         ENDIF
C
         DO index = 1 , lendir
            mapout(index) = mast(index)
         ENDDO
      ENDIF
C
      DO i = 1 , 12
         IF ( mapin(i).EQ.0 ) GOTO 100
         mapout(index) = iabs(mapin(i))
         index = index + 1
      ENDDO
 100  CONTINUE
      IF ( ipack.GT.0 ) THEN
         CALL packc(mapout,maphold,25)
         CALL clear(mapout,25)
         DO il = 1 , 25
            mapout(il) = maphold(il)
         ENDDO
      ENDIF
C       write(*,'(1x,25a1)')(mapout(ll),ll=1,25)
      RETURN
      END
