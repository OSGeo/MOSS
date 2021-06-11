C *********************************************************************
C
C                S U B R O U T I N E  P O V 0
C
C  Purpose:  DETERMINE IF MAPS HAVE MULTIPLE ATTRIBUTES AND IF SO ASK
C            THE USER WHICH ATTRIBUTES THEY WANT TO SAVE FROM THE MAPS
C
C  Parameters:
C     Name       Use        Type         Dimension       Description
C     ----       ---        ----         ---------   ------------------
C
C     ICHAN      I          INT                      CHANNEL TO USE FOR MULTIPLE
C                                                    ATTRIBUTE FILE OPEN
C
C     MA1        I          INT                      MAP ONE ATT FLAG 1=EXISTS
C
C     MA2        I          INT                      MAP TWO ATT FLAG 1=EXISTS
C
C     MAP1       I          INT          10          PACKED NAME OF MAP ONE
C
C     MAP2       I          INT          10          PACKED NAME OF MAP TWO
C
C     IBUFF      O          INT          42          OUTPUT ARRAY
C                                                    1=# OF SAVED ATTS FOR ONE
C                                                      0-NONE -1-ALL
C                                                    2=# OF SAVED ATTS FOR TWO
C                                                      0-NONE -1-ALL
C                                                    3-22 ATT ID'S SAVED FOR ONE
C                                                    23-42ATT ID'S SAVED FOR TWO
C
C
C  Files:    Logical Unit       File name or description
C            ------------       ------------------------
C            ICHAN              MULTIPLE ATTRIBUTES FILE
C
C **********************************************************************
C
      SUBROUTINE pov0(ichan,ma1,ma2,map1,map2,ibuff)
$Include:'io.inc'
$Include:'strng.inc'
      DIMENSION map1(10) , map2(10) , ibuff(42) , name(25) , iarr(128)
      INTEGER mapname(25)
C
C  SEE IF EITHER ONE HAS MULTIPLE ATTRIBUTES
C
      IF ( ma1.NE.1 .AND. ma2.NE.1 ) GOTO 400
C
C  SET COUNTERS OF ATTRIBUTES TO SAVE AND ATT ID'S TO SAVE
C
      CALL clear(ibuff,42)
C
C  OPEN ,READ, AND CLOSE ATTRIBUTE FILE OF MAP ONE IF REQUIRED
C
      IF ( ma1.EQ.1 ) THEN
         CALL gtnam(0,4,map1,name,ier)
C
         CALL adddir(name,mapname)
         CALL openf(ichan,mapname,1,256,2,ier)
         IF ( ier.NE.0 ) GOTO 200
C
         CALL rdblk(ichan,1,iarr,1,ier)
         IF ( ier.NE.0 ) GOTO 300
         CLOSE (ichan,IOSTAT=ier)
C
C  GET THE ID'S TO SAVE
C
 50      CONTINUE
         maxa = iarr(1)
         IF ( maxa.GT.20 ) maxa = 20
         WRITE (nprnt,99001) (mapname(i),i=1,15) , iarr(1) , maxa
99001    FORMAT (1X,15A2,' HAS',I4,' ATTRIBUTES',/,' ENTER UP TO',I3,
     &           ' ATTRIBUTE ID(S) TO RETAIN',/,
     &           ' ENTER -1 TO RETAIN ALL ATTRIBUTES',/,
     &           ' ENTER  0 TO RETAIN NO  ATTRIBUTES'\)
         CALL readin(icard,lenicd,icardp,istop)
C
         i = icardp
         CALL inum(icard,id,icardp)
         IF ( id.EQ.-1 ) ibuff(1) = id
         IF ( id.NE.0 .AND. id.NE.-1 ) THEN
C
            icardp = i
            kount = 0
            DO i = 1 , maxa
               CALL inum(icard,id,icardp)
               IF ( id.EQ.0 ) GOTO 60
               IF ( id.LT.0 .OR. id.GT.iarr(1) ) WRITE (nprnt,99002)
     &               iarr(1)
99002          FORMAT (' THE ATTRIBUTE ID(S) MUST BE BETWEEN 1 AND',I5,
     &                 ' RE-ENTER'\)
               IF ( id.LT.0 .OR. id.GT.iarr(1) ) GOTO 50
               kount = kount + 1
               ibuff(2+kount) = id
            ENDDO
 60         CONTINUE
            ibuff(1) = kount
         ENDIF
      ENDIF
C
C  OPEN ,READ, AND CLOSE ATTRIBUTE FILE OF MAP TWO IF REQUIRED
C
      IF ( ma2.NE.1 ) GOTO 400
      CALL gtnam(0,4,map2,name,ier)
C
      CALL adddir(name,mapname)
      CALL openf(ichan,mapname,1,256,2,ier)
      IF ( ier.EQ.0 ) THEN
C
         CALL rdblk(ichan,1,iarr,1,ier)
         IF ( ier.NE.0 ) GOTO 300
         CLOSE (ichan,IOSTAT=ier)
C
C  GET THE ID'S TO SAVE
C
 100     CONTINUE
         maxa = iarr(1)
         IF ( maxa.GT.20 ) maxa = 20
         WRITE (nprnt,99001) (mapname(i),i=1,15) , iarr(1) , maxa
         CALL readin(icard,lenicd,icardp,istop)
C
         i = icardp
         CALL inum(icard,id,icardp)
         IF ( id.EQ.-1 ) ibuff(2) = id
         IF ( id.NE.0 .AND. id.NE.-1 ) THEN
C
            icardp = i
            kount = 0
            DO i = 1 , maxa
               CALL inum(icard,id,icardp)
               IF ( id.EQ.0 ) GOTO 120
               IF ( id.LT.0 .OR. id.GT.iarr(1) ) WRITE (nprnt,99002)
     &               iarr(1)
               IF ( id.LT.0 .OR. id.GT.iarr(1) ) GOTO 100
               kount = kount + 1
               ibuff(22+kount) = id
            ENDDO
C
 120        CONTINUE
            ibuff(2) = kount
         ENDIF
         GOTO 400
      ENDIF
C
C  ERROR RETURNS
C
 200  CONTINUE
      WRITE (nprnt,99003) (mapname(i),i=1,15) , ier
99003 FORMAT (' *POV0*  ERROR OPENING FILE ',15A2,'  ERROR',I5)
      GOTO 400
C
 300  CONTINUE
      WRITE (nprnt,99004) ier
99004 FORMAT (' *POV0*  ERROR READING ATTRIBUTE FILE  ERROR',I5)
      CLOSE (ichan,IOSTAT=ier)
C
C  JUMP OUT
C
 400  CONTINUE
      RETURN
      END
