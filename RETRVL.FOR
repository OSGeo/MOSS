      SUBROUTINE retrvl(found,record)
C***********************************************************************
C
C NAME : SUBROUTINE RETRVL
C
C FUNCTION : TO SEARCH FOR A PARTICULAR ATTRIBUTE WITH RESPECT TO THE
C            ID, KEY, OR DESCRIPTOR.
C
C PARAMETERS : FOUND : ( RETURNED ) A LOGICAL VALUE, TRUE IF THE
C                      ATTRIBUTE OF INTERREST FOUND, FALSE IF THE ATTRIBUTE
C                      NOT FOUND
C
C               RECORD : ( RETURNED, ARRAY DEMENSIONED 128 ) IF THE
C                           ATTRIBUTE IS FOUND THIS WILL CONTAIN THE
C                           ATTRIBUTE INFORMATION.
C
C ROUTINES REQUIRED : SUBROUTINE RDBLK, SUBROUTINE RECEIVE, SUBROUTINE READIN
C***********************************************************************
$Include:'IO.inc'
$Include:'STRNG.inc'
      COMMON /unit  / chanel , atrbte , polygon , adress
C
      INTEGER chanel , atrbte , polygon , adress , record(128) , key(80)
     &         , action , putout , putin
C
      LOGICAL found
C
      found = .TRUE.
C
C TYPE IN YOUR OPTION.
C
 100  CONTINUE
      WRITE (nprnt,99001)
99001 FORMAT (' Enter ATTRIBUTE Id or Key  <none=CR>'\)
      CALL readin(icard,lenicd,icardp,istop)
      IF ( icard(1).EQ.0 ) THEN
         found = .FALSE.
      ELSE
         istart = icardp
         CALL inum(icard,action,istart)
         IF ( istart.LT.0 ) THEN
C
C SEARCHING FOR AN ATTRIBUTE WITH A CERTAIN KEY
C
            DO l = 1 , 10
               key(l) = 0
            ENDDO
            CALL decip(icard,key,icardp)
            length = atrbte + 1
            DO locate = 2 , length
C
               CALL rdblk(chanel,locate,record,1,ier)
               index = 1
               DO limit = 2 , 11
                  IF ( key(index).NE.record(limit) ) GOTO 120
                  index = index + 1
C
               ENDDO
               GOTO 200
C
 120        ENDDO
C
            WRITE (nprnt,99002)
99002       FORMAT (' INVALID ATTRIBUTE KEY')
            GOTO 100
C
C IS THE ID A VALID ONE ?
C
         ELSEIF ( action.GT.0 .AND. action.LE.atrbte ) THEN
C
C VALID ID
C
            CALL rdblk(chanel,action+1,record,1,ier)
         ELSE
            WRITE (nprnt,99003)
99003       FORMAT (' INVALID ATTRIBUTE ID')
            GOTO 100
         ENDIF
      ENDIF
C
C
 200  CONTINUE
      RETURN
      END
