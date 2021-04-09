      SUBROUTINE freeact(map1)
C ************************************************************
C
C        P R O G R A M    FREEact
C
C FUNCTION:   PROGRAM ALLOWS THE USER TO DE-ACTIVATE ACTIVATED
C             DATA SETS that are being deleted.  Called from Delete.
C
C ************************************************************
$Include:'DBCHAN.inc'
$Include:'IO.inc'
$Include:'TYPE.inc'
$Include:'WHMAP.inc'
$Include:'STRNG.inc'
      COMMON /attfet/ kat(86) , lbat
      COMMON /pntfet/ npoint
C
      LOGICAL mcomp
C
      INTEGER irec(31) , iall(10) , listfre(40) , name(10) , nam1(2) , 
     &        nam2(2)
      INTEGER icom(10) , map1(10)
C
C  LOOP THROUGH MACTS TIMES, FREEING DATA SETS
C
C
C  open <projectname>.sa
C
      CALL openf(icdcdt,idesdt,1,62,2,ier)
      IF ( ier.NE.0 ) THEN
         WRITE (*,*) 'Unable to open project .SA file'
      ELSE
         CALL packc(map1,name,10)
         kstrct = nstrct
         mstrct = nstrct - 7 + 1
         DO i = mstrct , 1 , -7
            CALL gtmap(icom,i)
            DO il = 1 , 10
               CALL byteit(icom(il),nam1)
               CALL byteit(name(il),nam2)
               DO ik = 1 , 2
                  IF ( nam1(ik).NE.nam2(ik) ) GOTO 50
               ENDDO
            ENDDO
            CALL adsdel(ier,i,index,npnts)
            kmacts = kmacts + 1
            listfre(kmacts) = i
 50      ENDDO
C
C  UPDATE TYPE TABLE AND SELECTION CRITERIA FILE
C
         DO i = kmacts , 1 , -1
            id = listfre(i)
            istart = (id/7) + 1
            istop = (kstrct/7) - 1
            IF ( istart.LE.istop ) THEN
               DO j = istart , istop
                  idatyp(j) = idatyp(j+1)
                  kat((j*2)-1) = kat((j+1)*2-1)
                  kat((j*2)) = kat((j+1)*2)
                  READ (icdcdt,REC=j+1,IOSTAT=ier,ERR=60) irec
                  WRITE (icdcdt,REC=j,IOSTAT=ier,ERR=60) irec
 60            ENDDO
            ENDIF
            kstrct = kstrct - 7
         ENDDO
         CLOSE (icdcdt)
C
C  RESET NUMBER OF ACTIVE AND LAST POSITION USED IN POINT.DT AND DESCRIBE.FA
C
         nacts = nacts - kmacts
         IF ( nacts.GE.1 ) THEN
            id = ((nacts-1)*7) + 1
            i = istrct(id) + istrct(id+1) - 1
            npoint = i
            CALL outcm
            GOTO 99999
         ENDIF
C
C  FREE ENTIRE STRUCTURE TABLE
C
         CALL clear(istrct,300)
         CALL clear(idatyp,40)
         CALL clear(kat,86)
         npoint = 0
         nstrct = 0
         nacts = 0
C
C  ERROR RETURNS
C
cssk 8100  WRITE(NPRNT,98100)
cssk       LINES=LINES+2
99001    FORMAT (' ILLEGAL ACTIVE ID. ')
C
cssk 8200  WRITE(NPRNT,98200)
99002    FORMAT (' *ACTIVE*   ERROR FROM INITL, TROUBLE WITH INTERPROC')
C
cssk 950   WRITE(NPRNT,3050) IER
cssk       LINES=LINES+2
99003    FORMAT (' *FREE* COULD NOT OPEN DESCRIBE.DT  ERROR',I5)
      ENDIF
C
C  JUMP OUT
C
C
      CALL outcm
99999 CONTINUE
      END
