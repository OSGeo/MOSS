      SUBROUTINE rlsout(locate)
C************************************************************************
C
C NAME : SUBROUTINE RLSOUT
C
C FUNCTION : TO OUTPUT THE ENTIRE OR SPECIFIED POLYGON VALUES OF A
C            ALREADY SPECIFIED ATTRIBUTE. THE POLYGON VALUES MUST BE OF
C            TYPE REAL.
C
C PARAMETERS : LOCATE : ( INTEGER;INPUT ) THE LOCATION OF THE FIRST RECORD
C                       OF THE DATA BASE WHERE THE POLYGON VALUES ARE
C                       STORED.
C
C ROUTINES REQUIRED : SUBROUTINE RDBLK
C
C************************************************************************
C
$Include:'IO.inc'
$Include:'STRNG.inc'
      COMMON /pntfet/ npoint
      COMMON /serit / jserch , count , kall , hits , dpoint , ichanp
      COMMON /unit  / chanel , atrbte , polygon , adress
C
      DIMENSION ibuff(128)
      INTEGER chanel , atrbte , polygon , adress , count , times
      INTEGER bufer(128) , action , hits
      INTEGER dpoint
C
      REAL lower , record(64)
C
      EQUIVALENCE (record(1),bufer(1))
C
C LIMIT IS THE NUMBER OF POLYGON VALUES PER RECORD.
C
      limit = 64
      count = 1
C
      WRITE (nprnt,99001)
99001 FORMAT (' ENTER 1 FOR OBTAINING ALL ITEMS',
     &        /' ENTER 2 FOR BOUNDED SEARCH'\)
      CALL readin(icard,lenicd,icardp,istop)
      CALL inum(icard,action,k)
C
      IF ( action.GT.1 ) THEN
C
C LISTING OUT ONLY THOSE POLYGON VALUES THAT ARE WITH IN THE SPECIFIED
C RANGE.
C
C
         WRITE (nprnt,99002)
99002    FORMAT (' ENTER IN THE LOWER BOUND OF RANGE '\)
         CALL readin(icard,lenicd,icardp,istop)
         CALL fnum(icard,lenicd,lower,icardp)
         WRITE (nprnt,99003)
99003    FORMAT (' ENTER IN THE UPPER BOUND OF THE RANGE '\)
         CALL readin(icard,lenicd,icardp,istop)
         CALL fnum(icard,lenicd,upper,icardp)
C
         kall = 0
         hits = 0
         count = 0
C
C SEARCHING THE ENTIRE LIST FOR THE VALUES OF INTEREST
C
         DO index = 1 , polygon , 64
C
            IF ( index+63.GT.polygon ) limit = polygon - index + 1
            CALL rdblk(chanel,locate,bufer,1,ier)
C
            DO times = 1 , limit
C
               IF ( jserch.EQ.1 ) CALL rdblk(ichanp,dpoint,ibuff,1,ier)
               count = count + 1
               IF ( lower.LT.record(times) ) THEN
                  IF ( upper.GT.record(times) ) THEN
                     IF ( jserch.EQ.0 ) THEN
                        WRITE (nprnt,99006) count , record(times)
                     ENDIF
                     IF ( jserch.EQ.1 ) THEN
                        npoint = npoint + 1
                        IF ( npoint.GT.32500 ) THEN
                           WRITE (nprnt,99004) npoint
99004                      FORMAT (' Feature file almost full:')
                        ENDIF
                        CALL putfast(npoint,ibuff(15),dpoint,ibuff(24),
     &                               dpoint,dpoint,ier)
                     ENDIF
C
C A POLYGON VALUES WITHIN THE RANGE WAS LOCATED.
C
                     hits = hits + 1
                  ENDIF
               ENDIF
C
               IF ( jserch.EQ.1 ) dpoint = ibuff(1)
            ENDDO
C
            locate = locate + 1
C
         ENDDO
         WRITE (nprnt,99005) hits
99005    FORMAT (' THERE WERE ',I5,' ITEMS WITHIN THE RANGE')
      ELSE
C
C WRITTING OUT THE ENTIRE POLYGON LIST TO THE SPECIFIED ATTRIBUTE
C
         kall = 0
         IF ( jserch.EQ.1 ) kall = 1
         IF ( kall.NE.1 ) THEN
            DO index = 1 , polygon , 64
C
               IF ( index+63.GT.polygon ) limit = polygon - index + 1
               CALL rdblk(chanel,locate,bufer,1,ier)
C
               DO times = 1 , limit
C
                  IF ( jserch.EQ.0 ) WRITE (nprnt,99006) count , 
     &                 record(times) + .0000005
99006             FORMAT (' FEATURE I.D. ',I5,' HAS VALUE ',F14.6)
                  count = count + 1
C
               ENDDO
               locate = locate + 1
C
            ENDDO
         ENDIF
      ENDIF
C
      RETURN
      END
