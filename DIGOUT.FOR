      SUBROUTINE digout(locate)
C****************************************************************************
C
C NAME : SUBROUTINE DIGOUT
C
C FUNCTION : TO OUTPUT THE POLYGON VALUES OF A SPECIFIED ATTRIBUTE.
C            THE ROUTINE IS USED TO OUT PUT POLYGON VALUES WHICH
C            ARE INTEGER.
C
C PARAMETERS : LOCATE : ( INPUT ) THIS IS THE DATA BASE LOCATION WHERE
C                       THE FIRST RECORD OF THE POLYGON VALUES ARE
C                       STORED.
C ROUTINES
C CALLED:    RDBLK,ADPNT.
C
C****************************************************************************
$Include:'IO.inc'
$Include:'STRNG.inc'
$Include:'WORK.inc'
      COMMON /pntfet/ npoint
      COMMON /serit / jserch , count , kall , hits , dpoint , ichanp
      COMMON /unit  / chanel , atrbte , polygon , adress
C
      INTEGER dpoint
      INTEGER chanel , atrbte , polygon , adress , count , times , 
     &        bufer(128) , action , upper , hits
C
      count = 1
      limit = 128
C
      WRITE (nprnt,99001)
99001 FORMAT (' ENTER 1 TO OUTPUT ALL FEATURES',
     &        /' ENTER 2 TO OUTPUT BOUNDED LIST'\)
      CALL readin(icard,lenicd,icardp,istop)
      CALL inum(icard,action,icardp)
C
      IF ( action.LE.1 ) THEN
C
C  List out the attribute values for all the polygons
         kall = 1
         DO index = 1 , polygon , 128
C
C LIMIT IS THE NUMBER OF POLYGONS PER RECORD
C
            IF ( index+127.GT.polygon ) limit = polygon - index + 1
            CALL rdblk(chanel,locate,bufer,1,ier)
C
            DO times = 1 , limit
               IF ( jserch.EQ.0 ) WRITE (nprnt,99002) count , 
     &              bufer(times)
99002          FORMAT ('  POLYGON ',I7,'  ATTRIBUTE VALUE IS ',I5)
               count = count + 1
C
            ENDDO
            locate = locate + 1
C
         ENDDO
         GOTO 100
      ENDIF
C
      WRITE (nprnt,99003)
99003 FORMAT (' ENTER IN LOWER BOUND OF RANGE'\)
      CALL readin(icard,lenicd,icardp,istop)
      CALL inum(icard,lower,icardp)
C
      WRITE (nprnt,99004)
99004 FORMAT (' ENTER IN UPPER BOUND OF RANGE'\)
      CALL readin(icard,lenicd,icardp,istop)
      CALL inum(icard,upper,icardp)
      hits = 0
      count = 0
C
C OUTPUTTING POLYGON VALUES WHICH FALL WITH IN A CERTAIN SPECIFIED RANGE.
C
      kall = 0
      DO index = 1 , polygon , 128
C
         IF ( index+127.GT.polygon ) limit = polygon - index + 1
         CALL rdblk(chanel,locate,bufer,1,ier)
C
         DO times = 1 , limit
C
            IF ( jserch.EQ.1 ) CALL rdblk(ichanp,dpoint,ibuff,1,ier)
            count = count + 1
            IF ( lower.LE.bufer(times) ) THEN
               IF ( upper.GE.bufer(times) ) THEN
C
C A POLYGON VALUE WITHIN THE RANGE WAS LOCATED
C
                  IF ( jserch.EQ.0 ) THEN
                     WRITE (nprnt,99002) count , bufer(times)
                  ENDIF
                  IF ( jserch.EQ.1 ) THEN
                     npoint = npoint + 1
                     IF ( npoint.GT.32500 ) THEN
                        WRITE (nprnt,99005) npoint
99005                   FORMAT (' Feature file almost full:')
                     ENDIF
                     CALL putfast(npoint,ibuff(15),dpoint,ibuff(24),
     &                            dpoint,dpoint,ier)
                  ENDIF
                  hits = hits + 1
C
                  IF ( jserch.EQ.1 ) dpoint = ibuff(1)
               ENDIF
            ENDIF
         ENDDO
C
         locate = locate + 1
C
      ENDDO
      WRITE (nprnt,99006) hits
99006 FORMAT (' THERE WERE ',I4,' ITEMS WITHIN THE SPECIFIED RANGE')
C
 100  CONTINUE
      RETURN
      END
