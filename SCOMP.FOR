      LOGICAL FUNCTION scomp(nmaps,name,scale,nents,itype,ier,ichan)
C ******************************************************************
C
C             F U N C T I O N    S C O M P
C FUNCTION:
C             CHECK A MOSS MAP FILE DIRECTORY FOR THE OCCURENCE OF
C             A PARTICULAR MAP.
C PARAMETERS:
C             SCOMP     OUT - THE FLAG USED TO SHOW IF THE MAP WAS FOUND
C                             .FALSE.,IF NOT FOUND.
C             NMAPS    OUT  - THE NUMBER OF MAP IN A MOSS DATABASE.
C             NAME      IN  - THE MAP FILE NAME.
C             SCALE     OUT - THE MAP SCALE.
C             NENTS     OUT - THE NUMBER OF ENTRYS.
C             ITYPE     OUT - THE TYPE OF MAP.
C             IER       OUT - THE ERROR RETURN FOR WRITING TO, READING,
C                             OPENNING, OR CLOSING A FILE,
C                             IER = 0, IF NO ERROR.
C             ICHAN     IN  - CHANNEL TO MAP FILE DIRECTORY  1=MASTER
C ROUTINES
C
C ******************************************************************
$Include:'IO.inc'
$Include:'dnames.inc'
      COMMON /dirsy / lastc , lastbk , iwtrn , kdir(256)
      DIMENSION name(12) , irec(16) , ibuff(256)
      INTEGER map(25) , wrk(2)
C
C  SET LOGICAL VALUE AND ERROR FLAG
C
      scomp = .FALSE.
      scale = 0.
      nents = 0
      itype = 0
      ier = 0
C
C  INITIALIZE UNUSED PARAMETER SCALE
C
      scale = 0.
C
C  MAKE SURE THE BLOCK IS READ BY GETMAP THEN GET THE NUMBER OF MAPS
C
      lastc = -1
      iwtrn = -1
      CALL getmap(ichan,1,irec,ier)
      nmaps = irec(16) - 1
C
C  SAVE THE INPUT NAME ARRAY SINCE FNDMAP WRITES IN THE ARRAY
C
      DO i = 1 , 10
         irec(i) = name(i)
      ENDDO
C
C  DO A SEARCH OF THE MAP NAMES  FILE OPEN ON CHANNEL ICHAN
C
      IF ( nmaps.GT.0 ) THEN
         CALL fndmap(nmaps,irec,ichan,match,iloc2)
         IF ( match.GT.0 ) THEN
C
C  FOUND THE NAME IN THE MAP NAMES FILE...RETURN PARAMETERS
C  IF ARCHIVED THEN HAVE SCOMP RETURN NEG OF # OF ITEMS
C
            itype = irec(10)
            nents = irec(13)
            IF ( itype.GE.6 .AND. itype.LE.9 ) nents = 1
            wrk(1) = irec(9)
            CALL unpack(wrk,2)
            IF ( wrk(2).EQ.3 ) nents = -nents
            scomp = .TRUE.
            GOTO 100
         ENDIF
      ENDIF
C
C  THE MAP IS NOT IN THE NAMES FILE...SEE IF IT'S ON DISK AND RETURN PARAMTERS
C
      IF ( ichan.EQ.1 ) GOTO 100
      kchan = 100
      CALL gtnam(-1,0,name,map,ier)
C
      CALL openf(kchan,map,1,256,2,ier)
      IF ( ier.EQ.0 ) THEN
         WRITE (nprnt,99001)
99001    FORMAT (' Vector map not found in project catalog - have:',
     &           /'                    - accessed it from disk',/,
     &          '                    - added it to your project catalog'
     &          ,/)
         CALL rdblk(kchan,1,ibuff(1),1,ier)
         CALL rdblk(kchan,1,ibuff(129),1,ier)
         CALL tupdir(first,last,nmaps,name,ibuff,jcom,2,ier)
         CLOSE (kchan,IOSTAT=ier)
         IF ( ier.EQ.0 ) THEN
C
            itype = ibuff(62)
            nents = ibuff(63)
            IF ( itype.GT.0 .AND. itype.LT.6 .OR. itype.GT.10 .AND. 
     &           itype.LT.14 ) scomp = .TRUE.
            GOTO 100
         ENDIF
      ENDIF
C
C  MAYBE IT'S A CELL FILE...ATTACH .HD SUFFIX AND TRY TO OPEN
C
      CALL gtnam(-1,5,name,map,ier)
C
      CALL openf(kchan,map,1,256,2,ier)
      IF ( ier.EQ.0 ) THEN
         WRITE (nprnt,99002)
         CALL rdblk(kchan,1,ibuff,1,ier)
99002    FORMAT (' Cell map not found in project catalog - have:',
     &           /'                    - accessed it from disk',/,
     &          '                    - added it to your project catalog'
     &          ,/)
         CALL tupdir(first,last,nmaps,name,ibuff,jcom,2,ier)
         CLOSE (kchan,IOSTAT=iier)
         IF ( ier.EQ.0 ) THEN
C
            itype = ibuff(62)
            nents = 1
            IF ( itype.GE.6 .AND. itype.LE.9 ) scomp = .TRUE.
         ENDIF
      ENDIF
C
C  AND RETURN
C
 100  CONTINUE
      RETURN
      END
