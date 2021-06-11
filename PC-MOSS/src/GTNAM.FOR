C***********************************************************
C
C  NAME:      G  T  N  A  M
C
C  FUNCTION:  ADDS AN EXTENSION ONTO A MAP NAME DEPENDING ON THE OPTION
C
C  PARAMETERS:
C    INPUT      INDEX = POINTER TO THE MAP IN THE STRUCTURE TABLE
C                          -1 INDICATES AN UNPACKED NAME
C               IOPT =  OPTION FOR NAME EXTENSION
C               NAME =  ALPHA ARRAY OF MAP NAME CHARACTERS IF INDEX LT ZERO
C   OUTPUT      NAME =  ALPHA ARRAY OF PACKED CHARACTERS FOR MAP NAME
C               IER  =  ERROR FLAG 1 IF OK NOT 1 IF ERROR
C  FILES:  NONE
C
C  ROUTINES
C  REQUIRED:    GTMAP, PACKC , UNPACK, MCOMP
C
C  SYSTEM
C  DEPENDENCE:
C
C  LANGUAGE
C  DEPENDENCE:
C
C
C**************************************************************************
C
      SUBROUTINE gtnam(index,iopt,name,map,ier)
      COMMON /io    / nprnt , ioin
      DIMENSION map(25) , name(25) , isuf(36)
      INTEGER wrk(30)
C
      IF ( index.LT.-1 .OR. index/7.GT.42 ) GOTO 200
      IF ( iopt.LT.-26 .OR. iopt.GT.18 ) GOTO 200
C
      idot = 46
      isuf(1) = 'DT'
      isuf(2) = 'DH'
      isuf(4) = 'AT'
      isuf(5) = 'HD'
      isuf(6) = 'CB'
      isuf(7) = 'CD'
      isuf(8) = 'CC'
      isuf(9) = 'CM'
      isuf(11) = 'CV'
      isuf(12) = 'AD'
      isuf(16) = 'WR'
      isuf(17) = 'RD'
      isuf(18) = 'MS'
C
      IF ( index.GT.0 ) CALL gtmap(wrk,index)
      IF ( index.LE.0 ) THEN
         DO l = 1 , 25
            wrk(l) = name(l)
         ENDDO
      ENDIF
C
C
C  MOVE TO OUTPUT ARRAY AND FIND END...MOSS MAX IS 10 CHARS PER NAME
C
      IF ( index.GE.0 ) CALL unpack(wrk,30)
      CALL unpack(isuf,36)
C
      DO i = 1 , 25
         ic = iabs(wrk(i))
         IF ( ic.EQ.32 .OR. ic.EQ.0 ) GOTO 100
      ENDDO
      i = 11
C
C  PUT ON EXTENSION
C
 100  CONTINUE
      j = i
C  IN CASE OF NULL FILL
      IF ( iopt.NE.0 ) THEN
C  IN CASE OF USER TYPED IN .DT
         IF ( iopt.EQ.1 .OR. iopt.EQ.2 ) THEN
            IF ( wrk(j-3).EQ.idot ) j = j - 3
         ENDIF
         wrk(j) = idot
C
         IF ( iopt.GT.0 ) wrk(j+1) = isuf(iopt*2-1)
         IF ( iopt.GT.0 ) wrk(j+2) = isuf(iopt*2)
         IF ( iopt.GT.0 ) j = j + 3
C
         IF ( iopt.LT.0 ) wrk(j+1) = 64 + abs(iopt)
         IF ( iopt.LT.0 ) j = j + 2
      ENDIF
C
C  NULL FILL
C
      DO i = j , 30
         wrk(i) = 0
      ENDDO
C
      CALL packc(wrk,map,30)
 
      IF ( ideb.NE.0 ) WRITE (nprnt,99001) (map(i),i=1,15)
99001 FORMAT (' *GTNAM* ',15A2)
      GOTO 300
C
C  ERROR RETURN
C
 200  CONTINUE
      WRITE (nprnt,99002) index , iopt
99002 FORMAT (' *GTNAM*  ILLEGAL FIRST ',I5,' OR SECOND PARAMETER ',1I5)
      ier = 2
C
cssk       WRITE(NPRNT,98002) (NAME(I),I=1,10)
99003 FORMAT (' *GTNAM*  COULD NOT FIND END OF ',10A1)
cssk       IER = 3
C
C  JUMP OUT
C
 
 300  CONTINUE
      RETURN
      END
