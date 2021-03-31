C******************************************************************************
C
C  Name:     S B J S R T
C
C  Purpose:  SCAN SUBJECT TABLE AND BUILD ARRAY OF SORTED SUBJECTS WHICH
C            CONFORM TO THESE RULES.  TO BE CONSIDERED, THE SUBJECT MUST BE
C            GREATER THAN OR EQUAL TO SPECIFIED MINIMUM AND FALL ON THE
C            SPECIFIED INTERVAL, I.E. EVERY FIFTH SUBJECT.
C
C  Commons/Variables    In/Out        Attributes   Function/Size
C
C  -- Dummy Arguments --
C     MCHN              In            Integer - Input map channel number
C     MINSUB            In            Integer - Minimum subject value
C     INTRVL            In            Integer - Labeling interval
C     IER               Out           Integer - Returned error code
C
C  -- IO --
C     NPRNT                           Integer
C     IOIN                            Integer
C
C  -- DEBUG --
C     IDEB                            Integer
C
C  -- WORK --
C     IBUFF                           Integer Array        128
C     NAME                            Integer Array         15
C     LABEL                           Integer Array         10
C     JCOM                            Integer Array         10
C
C  -- LOCO4 --
C     ISUBJ                           Integer Array        128
C     NSUBS                           Integer
C
C  -- Stack Variables --
C     I                               Integer
C     IVAL                            Integer
C     J                               Integer
C     M                               Integer
C     K                               Integer
C     N                               Integer
C     INDX                            Integer
C
C  --   external Subprograms --
C     Qrdblk
C
C  Files:
C       Unit    File name or description
C       ------  -------------------------------------------------------
C       15      Input contour LINE map
C       10      "@CONSOLE" for output
C
C  Calling:          LBGENR
C
C******************************************************************************
C
      SUBROUTINE sbjsrt(mchn,minsub,intrvl,ier)
      COMMON /io    / nprnt , ioin
      COMMON /work  / ibuff(128) , name(25) , label(10) , jcom(10)
      COMMON /loco4 / isubj(128) , nsubs
      CHARACTER*30 chrbuf
C
      m = 0
      DO i = 3 , nsubs + 2
C.......READ SUBJECT RECORD
         CALL rdblk(mchn,i,ibuff,1,ier)
C.......DECODE ASCII SUBJECT INTO NUMERIC VALUE
C        DECODE(IBUFF,95,ERR=800) IVAL
         kount = 0
         DO jj = 1 , 7
            kount = kount + 1
            CALL gtb(inxx,ibuff(jj),1)
            chrbuf(kount:kount) = char(inxx)
            kount = kount + 1
            CALL gtb(inxx,ibuff(jj),2)
            chrbuf(kount:kount) = char(inxx)
         ENDDO
         READ (chrbuf(3:7),99001,IOSTAT=ier,ERR=300) ival
99001    FORMAT (I5)
C.......IF NUMERIC SUBJECT LESS THAN MINIMUM SKIP OUT
         IF ( ival.GE.minsub ) THEN
            m = m + 1
C.........INSERT NUMERIC SUBJECT INTO ARRAY
            DO j = 1 , m
C...........FIRST TIME ONLY
               IF ( m.NE.1 ) THEN
C...........CHECK FOR DUPLICATE SUBJECTS
                  IF ( ival.EQ.isubj(j) ) GOTO 100
C.............CURRENT GREATER THAN PREVIOUS
                  IF ( ival.LE.isubj(m) ) THEN
C...............THIS SUBJECT LESS THAN PREVIOUS SUBJECT POINTED TO BY M
C...............SHIFT REST DOWN AND INSERT CURRENT
                     DO k = m , j - 1 , -1
                        isubj(k) = isubj(k-1)
                     ENDDO
                     isubj(j) = ival
                     GOTO 100
C.............CHECK FOR LAST VALID TIME THRU LOOP
                  ELSEIF ( j.GE.m-1 ) THEN
C...............LAST TIME THRU, INSERT AFTER LAST
                     isubj(m) = ival
                     GOTO 100
                  ENDIF
               ELSE
                  isubj(m) = ival
               ENDIF
            ENDDO
         ENDIF
 100  ENDDO
C.....APPLY INTERVAL SPECIFIED TO NUMERIC SUBJECTS
      IF ( intrvl.EQ.1 ) GOTO 400
      n = 0
 200  CONTINUE
      n = n + 1
      indx = ((n-1)*intrvl) + 1
      IF ( indx.GT.m ) THEN
         DO i = n , m
            isubj(i) = 0
         ENDDO
         nsubs = n - 1
         GOTO 400
      ELSE
         isubj(n) = isubj(indx)
         GOTO 200
      ENDIF
C
C.....ERROR TRAP ON DECODE
 300  CONTINUE
      minsub = i
      WRITE (*,*) ' *SBJSRT* error on decode  error' , ier
      ier = -1
C
 400  CONTINUE
      RETURN
      END
