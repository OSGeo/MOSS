      SUBROUTINE tupdir(first,last,nmaps,name1,ihead,jcom,ichan,ier)
C  ******************************************************************
C  ADD A NEW MAP TO DATABASE.DT
C
C     PARAMETERS:   FIRST
C                   LAST
C                   NMAPS
C                   IHEAD
C                   JCOM
C                   ICHAN - IN - Channel number to openned <project>.DT
C
C  ******************************************************************
      COMMON /io    / nprnt , ioin
      COMMON /dirsy / lastc , lastbk , iwtrn , kdir(256)
      INTEGER first
      DIMENSION name(16) , name1(10) , jcom(25) , nam(16) , ihead(256)
      INTEGER wrk(16)
C
C  MAKE SURE THE BLOCK IS READ IN GETMAP
C
      lastc = -1
      iwtrn = -1
      CALL clear(nam,16)
C
      CALL getmap(ichan,1,wrk,ier)
      last = wrk(16)
      nmaps = last - 1
      IF ( nmaps.LT.1 ) THEN
         nmaps = 0
         iloc2 = 2
         GOTO 100
      ENDIF
C
C  SAVE NAME ... FNDMAP REPLACES WITH PACKED NAME
C
      DO i = 1 , 10
         nam(i) = name1(i)
      ENDDO
C
      CALL fndmap(nmaps,nam,ichan,iloc1,iloc2)
      IF ( iloc1.NE.0 ) THEN
C
C  AN UPDATE (GETS A PACKED NAME)
C
         CALL getmap(ichan,iloc1,name,ier)
         iloc2 = iloc1
         icount = name(11)
         last = last - 1
         nmaps = nmaps - 1
         WRITE (nprnt,99001) (name(i),i=1,8)
99001    FORMAT (' - UPDATING MAP ',8A2)
         GOTO 200
      ELSE
         IF ( iloc2.GT.last+1 ) iloc2 = last + 1
         IF ( iloc2.LE.last ) THEN
            iup = last
            index = iup
            DO ii = iloc2 , iup
               CALL getmap(ichan,index,wrk,ier)
               CALL putmap(ichan,index+1,wrk,ier)
               index = index - 1
            ENDDO
         ENDIF
C
C
         IF ( nmaps.EQ.0 ) iloc2 = 2
      ENDIF
C
C  PACK NAME & INSERT
C
C   PACK NAME1 INTO NAME(1 TO 5)
 100  CONTINUE
      IF ( name1(1).GT.256 ) THEN
         DO ll = 1 , 5
            name(ll) = name1(ll)
         ENDDO
      ELSE
         CALL packc(name1,name,10)
      ENDIF
C
 200  CONTINUE
      name(9) = 1
      name(10) = ihead(62)
      name(11) = icount
      name(12) = ihead(67)
      name(13) = ihead(63)
      name(14) = 0
      name(15) = 0
      name(16) = 0
C
C  FOR ADDING RASTERIZED MAPS FROM DATABTEST
      IF ( name(10).GE.5 .AND. name(10).LE.9 ) THEN
         name(9) = ihead(185)
         name(13) = ihead(60)
         name(14) = ihead(61)
         name(15) = ihead(191)
      ENDIF
C
C  INSERT NAME INTO .DT
C
      CALL putmap(ichan,iloc2,name,ier)
      last = last + 1
      nmaps = nmaps + 1
      CALL getmap(ichan,1,wrk,ier)
      wrk(16) = last
      CALL putmap(ichan,1,wrk,ier)
      WRITE (ichan,REC=1) kdir
C
C
      RETURN
      END
