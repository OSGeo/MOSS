      PROGRAM union
$Include:'dbchan.inc'
$Include:'dnames.inc'
c**
c  variables for file name and existence check
c
      INTEGER fname(25)
      LOGICAL fexist
c**
      COMMON /bufstf/ ichan , bufz
      COMMON /pntfet/ npoint
      COMMON /io    / nprnt , ioin
      COMMON /whmap / istrct(300) , nstrct
      COMMON /table / itable(40)
$Include:'strng.inc'
      COMMON /type  / idatyp(40) , nacts
$Include:'work.inc'
      COMMON /osave / isub(15) , npolys , ltype2 , idisp , nosave , 
     &                ictsub
      COMMON /offst / sxmin , symin
$Include:'xypnts.inc'
      DIMENSION ihead(256) , kcom(5) , ibuf(128)
      REAL mbr1(4)
      INTEGER ibufr(10) , idum(1) , ids(99) , mbr(8) , lcom(10)
      LOGICAL mcomp
      EQUIVALENCE (mbr1(1),mbr(1))
      idisp = 0
C
C     ************************************************************
C
C        P R O G R A M   U N I O N
C
C        PURPOSE:  Do a UNION of OVERLAPPING POLYGONS
C
c        operation:  Based on resolve overlap option in original version
c                    of BUFFER.  Original data input to Buffer
c                    are read to establish overlap regions.
c                    Resolved overlaps are then written
c                    to the user-specified
c                    output map.  Some limitations inherent in
c                    Data General MOSS still exist in this version.
c
c        DATE:   21 Oct. 1987
c
C     *************************************************************
C
C   READ IN COMMON
C
      CALL initl(ier)
      IF ( ier.NE.0 ) STOP
C
C   CHANNEL NO.S FOR FILES
C
      ictsub = 51
      ichan = 10
      ichanb = 32
      jchan = icwork
      kchan = 52
      iovchn = 53
C
C     SET SOME VARIABLES
C
      lines = 1
      noway = 0
C     SAVE STATUS OF WORKFILE DATABASE POINTERS
C
      ipromt = 0
C
C   OPEN POINTER FILE
C
      CALL openf(icdcfa,idesfa,1,512,2,ier)
      IF ( ier.NE.0 ) THEN
C
C   ERROR ON OPENING A FILE
C
         WRITE (*,*) ' Could not open <DESCRIBE.FA>'
      ELSE
C
C
C  PARSE USER INPUT
C
         CALL rslcom(kchan,macts,fname,ier)
         IF ( ier.EQ.0 ) THEN
C
C
C     GET TOTAL NUMBER OF NEW ITEMS FOR THIS SAVE FILE
C
            mndex = itable(1)
C
C     CHECK PARAMETER INPUT
C
            IF ( macts.EQ.0 ) THEN
C
C     ERROR RETURNS
C
               WRITE (nprnt,99001)
            ELSEIF ( idatyp(mndex/7+1).EQ.6 ) THEN
               WRITE (nprnt,99002)
C
99001          FORMAT (' NO ACTIVE DATA SETS SPECIFIED')
99002          FORMAT (' USE THE CBUFFER COMMAND TO BUFFER CELL DATA')
            ELSE
C
c  get new map name for resolved map.  Map name is maintained
c  as a character variable.
c
c
               CALL overlp(fname)
            ENDIF
         ENDIF
      ENDIF
C
      END
