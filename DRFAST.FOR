      SUBROUTINE drfast(ichan,ier)
      COMMON /fls   / iunit , ifile(5)
      COMMON /tkout / cbf
      COMMON /color / icolor , kcolor(40)
      CHARACTER*1 cbf(1024)
C
C *********************************************************************
C
C                S U B R O U T I N E   D R F A S T
C
C  Purpose: This routine plots graphics representations for a map.
C
C  Parameters:
C     Name       Use        Type         Dimension       Description
C     ----       ---        ----         ---------   ------------------
C
C     ICHAN       I         INT           2/4 BYTES  DATA CHANNEL TO MAP
C     IER         O         INT           2/4 BYTES  ERROR RETURN
C
C  Files:    Logical Unit       File name or description
C            ------------       ------------------------
C              IUNIT            UNIT CONNECTED TO GRAPHICS FILE
C
C **********************************************************************
C
C  ENTER GRAPHICS PLOT LOOP
C
 100  CONTINUE
      READ (ichan,END=200) iwrd , (cbf(l),l=1,iwrd)
C
C  COLOR DISPLAY
C
      IF ( icolor.EQ.1 ) THEN
         jcolor = 0
      ELSEIF ( icolor.EQ.2 ) THEN
         jcolor = 6
      ELSEIF ( icolor.EQ.3 ) THEN
         jcolor = 1
      ELSEIF ( icolor.EQ.4 ) THEN
         jcolor = 5
      ELSEIF ( icolor.EQ.5 ) THEN
         jcolor = 11
      ELSEIF ( icolor.EQ.6 ) THEN
         jcolor = 12
      ELSEIF ( icolor.EQ.7 ) THEN
         jcolor = 10
      ELSEIF ( icolor.EQ.8 ) THEN
         jcolor = 9
      ELSEIF ( icolor.EQ.9 ) THEN
         jcolor = 8
      ELSEIF ( icolor.EQ.10 ) THEN
         jcolor = 13
      ELSEIF ( icolor.EQ.11 ) THEN
         jcolor = 3
      ELSEIF ( icolor.EQ.12 ) THEN
         jcolor = 4
      ELSEIF ( icolor.EQ.13 ) THEN
         jcolor = 2
      ELSEIF ( icolor.EQ.14 ) THEN
         jcolor = 14
      ELSEIF ( icolor.EQ.15 ) THEN
         jcolor = 7
      ELSE
         jcolor = 0
      ENDIF
      WRITE (iunit,*) '!COL C' , jcolor
C
      IF ( iwrd.NE.0 ) THEN
         WRITE (iunit,*) '!COL C' , jcolor
         WRITE (iunit,99001) (cbf(l),l=1,iwrd)
      ENDIF
99001 FORMAT (1024A)
      GOTO 100
C
 200  CONTINUE
      ier = 0
C
      RETURN
      END
