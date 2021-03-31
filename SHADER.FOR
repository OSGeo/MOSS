 
      SUBROUTINE shader
C
      COMMON /fls   / iunit , ifile(5)
      COMMON /plact / macts
      COMMON /io    / nprnt , ioin
      COMMON /table / itable(40)
      COMMON /work  / ibuff(128) , map(25) , icom(10) , jcom(10)
      COMMON /type  / idatyp(40) , nacts
      COMMON /dsbuf / mlen , idata(1024)
      COMMON /locla / xinc , rot , vx1 , vx2 , vy1 , vy2 , scale , 
     &                ichan , lpinch
      COMMON /mailb / mbx2do
C
      INTEGER jbuff(128) , island(353) , icolor(2) , isym(2) , 
     &        mapname(25)
      REAL wind(4) , rottt(2) , ddd(2)
C
C **************************************************************
C
C          S U B R O U T I N E    S H A D E R
C FUNCTION:
C          THIS ROUTINE SHADES A SET OF POLYGONS ACCORDING TO
C          USER SPECIFICATIONS.
C PARAMETERS:
C             IER       OUT - THE ERROR RETURN FOR WRITING TO, READING,
C                             OPENNING, OR CLOSING A FILE,
C                             IER = 1, IF AN ERROR,
C                             IER = 0, IF NO ERROR.
C **************************************************************
C
      mlen = 1024
C
C  LOOP THROUGH DATA SET BY DATA SET, EXTRACTING POLYS FROM
C  THE DATA BASE AND PLOT THEM ON THE CRT
C
      DO i = 1 , macts
C
         CALL apget(itable(i),istart,iend,ntotal)
         IF ( ntotal.NE.0 ) THEN
            d = xinc/float(lpinch)
            rot = rot + 5.0
C
C UPDATE STRUCTURE TABLE BY ADDING SPACING. THIS
C INFORMATION IS USED FOR LEGEND GENERATION
C
            lpinch = lpinch + 3
C
C  GET CORRECT F.I.T. FOR THIS ACTIVE MAP SET
C
            idd = itable(i)
            iftyp = idatyp((idd/7)+1)
            IF ( iftyp.EQ.1 .OR. iftyp.EQ.2 .OR. iftyp.EQ.9 .OR. 
     &           iftyp.EQ.10 ) THEN
C
               WRITE (nprnt,99001) iftyp
99001          FORMAT (' *SHADE*  THIS COMMAND WORKS WITH POLYGON AND',
     &                 ' CELL NOT TYPE',I5)
               GOTO 100
            ELSEIF ( iftyp.LT.6 .OR. iftyp.GT.8 ) THEN
C
C  OPEN CHANNEL TO MAP
C
               CALL gtnam(idd,0,'  ',map,ier)
               CALL adddir(map,mapname)
               CALL openf(ichan,mapname,1,256,2,ier)
               IF ( ier.NE.0 ) THEN
C
C  ERROR RETURNS
C
                  WRITE (nprnt,99002) (map(i),i=1,10) , ier
99002             FORMAT (' *SHADE*  ERROR OPENING MAP ',10A2,'  ERROR',
     &                    I5)
                  GOTO 100
               ELSE
C
C  READ HEADER AND CHECK FOR PRE-ASSIGNED PATTERNS
C
                  kfont = 0
                  CALL rdblk(ichan,1,ibuff,1,ier)
                  IF ( ibuff(120).GT.0 .AND. ibuff(120).LE.10 )
     &                  kfont = 1
C
C  LOOP THROUGH THIS ACTIVE DATA SET FROM ISTART TO IEND
C
                  DO j = istart , iend
C
C  GET DATA POINTER
C
                     CALL getfast(j,wind,index,isub,iex1,iex2,ier)
                     IF ( ier.EQ.0 ) THEN
C
C  IS THIS POLYGON IN THE VIEWING WINDOW
C
                        inw = invwnd(wind(1),wind(2),wind(3),wind(4),
     &                        vx1,vx2,vy1,vy2)
                        IF ( inw.NE.0 ) THEN
C
C  READ FEATURE AND GET ISLAND DATA
C
                           CALL rdblk(ichan,index,ibuff,1,ier)
                           island(1) = 0
                           CALL igetis(ibuff,island,nsl,ichan,index)
C
C  GET COORDINATE DATA
C
                           isave = 0
                           CALL cordxy(ibuff,npts,index,wind(1),wind(3),
     &                                 ichan)
C
C  CROSS SHADE THE POLYGON IF NOT FONT ASSIGNED
C
                           IF ( kfont.EQ.0 )
     &                           CALL croshtch(npts,rot,d,nsl,island)
                           IF ( kfont.NE.0 ) THEN
C
C  GET FONT ASSIGNMENT
C
                              CALL rdblk(ichan,isub,jbuff,1,ier)
                              CALL gethat(jbuff,ncross,icolor,rottt,
     &                              isym,ddd)
C
C  CROSS HATCH THE POLYGON
C
                              DO jj = 1 , ncross
c             IF (ICOLOR(JJ) .LE.0) ICOLOR(JJ) = 1
C             CALL SETCOL(IUNIT,ICOLOR(JJ),IER)
                                 ddd(jj) = ddd(jj)/scale
                                 IF ( isym(jj).NE.4 )
     &                                 CALL croshtch(npts,rottt(jj),
     &                                ddd(jj),nsl,island)
                              ENDDO
                           ENDIF
                        ENDIF
                     ENDIF
C
                  ENDDO
C         call flsh
                  CLOSE (ichan,IOSTAT=ier)
               ENDIF
            ENDIF
         ENDIF
C
C  CELL SHADING
C
C         CALL GTNAM(IDD,5,'  ',MAP,IER)
C         CALL OPENF(ICHAN,MAP,2,256,2,IER)
C         IF(IER.NE.1) GO TO 85020
C         CALL RDBLK(ICHAN,2,IBUFF,1,IER)
C         LEN = BYTE(IBUFF(57),1)
C         CALL RDBLK(ICHAN,1,IBUFF,1,IER)
C         CALL CLOSE(ICHAN,IER)
C         IB = 2
C         IF (LEN .GE. 16) IB = 1
C         IF (IB .EQ. 1)  LEN = LEN/16
C         IDDT = IBUFF(62)
C         CALL TIDK ( 3, IER )
C         CALL GTNAM(IDD,IDDT,'  ',MAP,IER)
C         CALL DSOPN(ICHAN,MAP,1,1,IB,LEN,512,IER)
C         IF (IER .NE. 1) GO TO 85020
C  SAVE SOME SPACE BY STUFFING FOR RSTPLT
C         IF (IB .EQ. 2) LEN = 0
C         IBUFF(1) = LEN
C         CALL RSTPL1(ICHAN,0,ROT,D,IER)
C
      ENDDO
      CALL toutpt(-1)
C
C  JUMP OUT
C
 100  CONTINUE
      CALL flsh
      RETURN
      END
