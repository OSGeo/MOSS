      SUBROUTINE fndmap(nmaps,nam,ichan,iloc1,iloc2)
      DIMENSION nam(16) , name(16)
      DIMENSION name1(16) , name2(16)
C
      lbound = 1
      iupper = nmaps + 2
      iloc1 = 0
      iloc2 = 0
      indx = 0
C
C**     unpack incoming name if necessary
      IF ( nam(1).GT.256 ) THEN
         CALL unpack(nam,16)
      ENDIF
C
      DO indx = 2 , nmaps + 1
c 100  CONTINUE
c      IF((IUPPER-LBOUND).LT.2) GO TO 400
C
C           INDX=((IUPPER+LBOUND)/2)
c        IF(INDX.GT.NMAPS+1) GO TO 300
         CALL getmap(ichan,indx,name1,ier)
         DO lll = 1 , 16
            name(lll) = name1(lll)
         ENDDO
         CALL unpack(name1,16)
C
         DO ii = 1 , 8
C            write(*,'(1x,a1,1x,a1,1x,i2)')nam(ii),name1(ii),ii
C            write(*,*)nam(ii),name1(ii)
            IF ( nam(ii).NE.name1(ii) ) GOTO 100
c            IF(NAM(II).GT.NAME1(II)) GO TO 300
         ENDDO
C
         iloc1 = indx
         DO ii = 1 , 16
            nam(ii) = name(ii)
         ENDDO
C
         RETURN
C
c 200    ILOC2=INDX
c        IUPPER=INDX
c        GO TO 100
c 300    CONTINUE
c        ILOC2=0
c        LBOUND=INDX
c        GO TO 100
C
 100  ENDDO
      IF ( iloc2.EQ.0 ) iloc2 = indx + 1
      IF ( iloc2.GT.nmaps+2 ) iloc2 = nmaps + 2
      RETURN
C
      END
