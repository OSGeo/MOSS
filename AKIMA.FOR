      SUBROUTINE akima(maxr,maxc,npts,nncp,ixfil,ier)
C
C     Driver for the quintic-splines interpolation program written
C     by Hiroshi Akima.  See subroutine IDSFFT for an explanation of
C     the parameters.
C       This routine is swapped in from the GRID program of MOSS.  It
C     reads the Interprocess Communication files containing gridding
C     parameters and data and passes them to IDSFFT for interpolation.
C     Upon return from IDSFFT, it writes the interpolated data to
C     interprocess files and returns to MOSS.
C       The Akima routines were slightly modified to permit execution
C     on computers with limited memory capacity.  Coordinates of the
C     output grid points are calculated by functions XI and YI, rather
C     than being stored as arrays.  The array IGP, used in routines
C     IDGRID and IDPTIP, has been replaced by a scratch disk file.
C     Output z-values for grid points are also written directly to disk.
C     Parameters for the function calls and virtual arrays are stored
C     in common areas VARRAY and GRIDPTS.  The use of commons minimizes
C     the changes to Akima's code.
C       User can change the number of closest data points, NCP, used in
C     calculating the value of a grid cell.  The maximum value for NCP
C     is stored in MAXNCP; this number may be increased, but dimension
C     statements in the subroutines will also need to be changed.
C       System dependencies: the variables LUN and IOIN are the sytem
C     units for console output and input. '<BEL>' in statement 140 sends
C     an ASCII 07 to the output device, i.e., it rings the bell. Array
C     IWK is typed as "INTEGER*2", i.e., each element is stored in 2
C     bytes rather than 4, to reduce the storage space required.
C
C  *********************************************************************
C
      IMPLICIT INTEGER*4(i-n)
C     assign channels for files and virtual arrays..........
      COMMON /varray/ izfil , ichigp
      COMMON /gridpts/ xmin , xside , ymax , yside
      COMMON /idpi  / itpv , x0 , y0 , ap , bp , cp , dp , p00 , p10 , 
     &                p20 , p30 , p40 , p50 , p01 , p11 , p21 , p31 , 
     &                p41 , p02 , p12 , p22 , p32 , p03 , p13 , p23 , 
     &                p04 , p14 , p05
$large
      INTEGER*2 iwk(18500) , ibad , iok , maxr , maxc , npts , nncp , 
     &          ier , ixfil
      DIMENSION wk(2500) , xd(500) , yd(500) , zd(500)
      DATA ibad , iok/ - 1 , 0/
      DATA lun , ioin/6 , 5/
      DATA maxncp/10/
      iyfil = 22
      ichpar = 14
      ichigp = 33
C
      nyi = maxr
      nxi = maxc
      ndp = npts
      ncp = nncp
C
C.....open x,y,z data files and set up arrays............
C
C      OPEN(IXFIL,file='TEMPKRIG.STP',FORM='UNFORMATTED')
      REWIND (ixfil)
      CALL rdxyz(ixfil,xd,yd,zd,ndp,ier)
      IF ( ier.NE.0 ) THEN
         PRINT * , ' bad return from RDXYZ,ier = ' , ier
         STOP
      ENDIF
      CLOSE (ixfil)
C
C......initialize work file to store triangle numbers for grid points
C
      OPEN (ichigp,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=4,
     &      STATUS='SCRATCH')
C
C......initialize file for output of zdata......................
C
      OPEN (izfil,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=4,
     &      STATUS='SCRATCH')
C
C.....call in interpolation routine................................
C
      md = 1
      CALL idsfft(md,ncp,ndp,xd,yd,zd,nxi,nyi,iwk,wk,ier)
      IF ( ier.NE.0 ) THEN
         ier = 1
         RETURN
      ELSE
C
C.....close output file and work file
C
C      CLOSE (IZFIL)
         CLOSE (ichigp)
         RETURN
      ENDIF
C
C.....write 'normal completion' code to error file and return
C
      END
