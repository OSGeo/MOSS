      FUNCTION nextpt(idf,mask,inlast,num)
      COMMON /io    / nprnt , ioin
      COMMON /parms / ps , psa(5) , lastm , lastalt , nextm , nextalt , 
     &                lastin , lastout
      COMMON /rundata/ xmmin , altmax , dmachc , daltc , nmach , nalt , 
     &                 nosave , itemid , idisp , ictsub
      DIMENSION idf(2) , oc(4)
      LOGICAL nosave
      nextpt = 0
C          CHECK TO SEE IF WE ARE ON A BORDER GOING OUT
C               WRITE(22,1) IDF,MASK,LASTIN,LASTM,LASTALT
C1              FORMAT("NEXTPT=",3(1X,Y4),3I6)
      IF ( lastin.EQ.2 ) THEN
         IF ( lastm.GE.nmach ) GOTO 200
      ELSEIF ( lastin.EQ.3 ) THEN
         IF ( lastalt.EQ.0 ) GOTO 200
      ELSEIF ( lastin.EQ.4 ) THEN
         IF ( lastm.EQ.0 ) GOTO 200
         IF ( iand(idf(1),mask).EQ.0 .AND. iand(idf(2),mask).EQ.0 )
     &         WRITE (nprnt,99001)
99001    FORMAT ('FLAGS INDICATE NO CONTOUR CROSSING')
      ELSEIF ( lastalt.GE.nalt ) THEN
         GOTO 200
      ENDIF
      IF ( iand(idf(1),mask).EQ.0 .AND. iand(idf(2),mask).EQ.0 ) RETURN
C          IS THIS A SADDLE
C               WRITE(22,2) PS,PSA
C2              FORMAT("PS,PSA=",6F7.2)
      IF ( (ps.LT.psa(1) .AND. ps.LT.psa(3) .AND. ps.GE.psa(2) .AND. 
     &     ps.GE.psa(4)) .OR. 
     &     (ps.LT.psa(2) .AND. ps.LT.psa(4) .AND. ps.GE.psa(1) .AND. 
     &     ps.GE.psa(3)) ) THEN
C
C.......THIS IS A CONTOUR CELL WITH A SADDLE. ORIENT CELL VALUES IN SUCH
C       A WAY THAT OC(1) CONTAINS THE CELL VALUE TO THE LEFT OF
C       DIRECTION LASTIN. OC INCREASES COUNTERCLOCKWISE.
C
         oc(1) = psa(lastin)
         oc(2) = psa(mod((lastin+2),4)+1)
         oc(3) = psa(mod((lastin+5),4)+1)
         oc(4) = psa(mod((lastin+4),4)+1)
C
C.......COMPUTE CELL-LENGTHS X1 Y1, WHERE THE CONTOURLINE WOULD INTER
C       SECT THE LEFT SIDE OF THE CONTOURCELL; AND COORDINATES X4 Y4
C       WHERE CONTOURLINE WOULD INTERSECT RIGHT SIDE.
C
         x1 = abs((ps-oc(1))/(oc(2)-oc(1)))
         y1 = abs((ps-oc(1))/(oc(4)-oc(1)))
         x2 = abs((ps-oc(2))/(oc(2)-oc(1)))
         y2 = abs((ps-oc(2))/(oc(3)-oc(2)))
         x3 = abs((ps-oc(3))/(oc(4)-oc(3)))
         y3 = abs((ps-oc(3))/(oc(3)-oc(2)))
         x4 = abs((ps-oc(4))/(oc(3)-oc(4)))
         y4 = abs((ps-oc(4))/(oc(4)-oc(1)))
C
C.......FIND AREA A1, WHICH WOULD BE CUT OFF BY A LEFT TURNING CON-
C       TOURLINE, AND A3 FOR RIGHT TURNING CONTOURLINE. A2 WOULD BE
C       CUT OFF BY THE SECOND CONTOUR LINE ENTERING THE CELL, IF THE
C       PRESENT CONTOUR LINE EXITS RIGHT. A2 IS THE CORROSPONDING AREA
C       FOR LEFT.
C
         a1 = x1*y1
         a2 = x2*y2
         a3 = x3*y3
         a4 = x4*y4
C        CALL Qanmode
C        WRITE (10,345) PS
C345      FORMAT ('PS=',F12.4)
C        WRITE (10,123) OC,A1,A2,X1,Y1,X2,Y2
C123      FORMAT ('SADDLE
CASE'/'OC=',4(F12.3,1X)/'A1,A2=',F12.3,1X,F12.3/'X1,Y1,X2,Y2=',4(F12.3,1X))
C
C.......IF A1+A3 < A2+A4, CONTOUR TURNS LEFT, OTHERWISE RIGHT.
C
         IF ( a1+a3.LT.a2+a4 ) iwayout = (mod((lastin+2),4)+1)
         IF ( a1+a3.GE.a2+a4 ) iwayout = (mod((lastin+4),4)+1)
      ELSE
         DO iwayout = 1 , 4
C          DO NOT GO OUT THE WAY WE CAME IN
            IF ( iwayout.NE.lastin ) THEN
               pslow = amin1(psa(iwayout),psa(iwayout+1))
               pshi = amax1(psa(iwayout),psa(iwayout+1))
               IF ( pslow.LE.ps .AND. ps.LT.pshi ) GOTO 100
            ENDIF
         ENDDO
         WRITE (*,*) 'COULD NOT GET OUT OF BOX' , ps , lastm , lastalt , 
     &               lastin
         RETURN
      ENDIF
 100  CONTINUE
      nextpt = 1
C               WRITE(22,3) IWAYOUT,INLAST,NUM
C3              FORMAT("IWAYOUT=",3I6)
      IF ( inlast.NE.0 .OR. num.NE.1 ) THEN
         IF ( iand(idf(1),mask).NE.0 ) THEN
C          1st TIME IN AND OUT OF BOX
            idf(1) = ieor(idf(1),mask)
         ELSE
C          THIS IS 2nd TIME WE HAVE COME IN AND GONE OUT OF BOX
C      WRITE(NPRNT,169) IDF(1),IDF(2),MASK
C  169 FORMAT(' 2ND:',3Y5)
            idf(2) = ieor(idf(2),mask)
         ENDIF
      ENDIF
C      WRITE(NPRNT,171)IDF(1),IDF(2),MASK
C  171 FORMAT(' 1ST:',3Y5)
      nextm = lastm
      nextalt = lastalt
C               WRITE(22,4) NEXTM,NEXTALT,IWAYOUT,IDF
C4              FORMAT("NEXTM,NEXTALT=",3I6,2(1X,Y4))
C          GET LOWER LEFT CORNER OF NEXT BOX
      IF ( iwayout.EQ.2 ) THEN
         nextm = nextm - 1
         lastout = 4
         RETURN
      ELSEIF ( iwayout.EQ.3 ) THEN
         nextalt = nextalt + 1
         lastout = 1
         RETURN
      ELSEIF ( iwayout.EQ.4 ) THEN
         nextm = nextm + 1
         lastout = 2
         RETURN
      ELSE
         nextalt = nextalt - 1
         lastout = 3
         RETURN
      ENDIF
 200  CONTINUE
      nextpt = -1
      RETURN
      END
