      SUBROUTINE delmap(first,last,nmaps,name,ichan,ier)
      COMMON /dirsy / lastc , lastbk , iwtrn , kdir(256)
      COMMON /io    / nprnt , ioin
$Include:'dnames.inc'
$Include:'strng.inc'
      COMMON /dbmap / dbnmap(2)
      INTEGER name(16) , first , wrk(2) , dbnmap
C
C  DELETE A MAP FROM A MOSS DT FILE
C
      ier = 0
      IF ( nmaps.LT.1 ) RETURN
C
      CALL fndmap(dbnmap(2),name,ichan,iloc1,iloc2)
      IF ( iloc1.EQ.0 ) THEN
C
         WRITE (nprnt,99001)
99001    FORMAT (' *DELMAP* THIS MAP DOES NOT EXIST')
         ier = 1
      ELSE
C
         move = last - iloc1
         IF ( move.GE.1 ) THEN
C
            DO i = 1 , move
               iloc1 = iloc1 + 1
               CALL getmap(ichan,iloc1,name,ier)
               CALL putmap(ichan,iloc1-1,name,ier)
            ENDDO
            WRITE (ichan,REC=lastbk) kdir
         ENDIF
C
         last = last - 1
         nmaps = nmaps - 1
         CALL getmap(ichan,1,name,ier)
         name(16) = last
         CALL putmap(ichan,1,name,ier)
         WRITE (ichan,REC=1) kdir
      ENDIF
      RETURN
      END
