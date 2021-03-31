      SUBROUTINE plots(mmdev,ipsize,ldev)
C ****************************************************************
C *                                                              *
C * (C) COPYRIGHT HEWLETT-PACKARD COMPANY 1987.  ALL RIGHTS      *
C * RESERVED.  NO PART OF THIS PROGRAM MAY BE PHOTOCOPIED, RE-   *
C * PRODUCED, OR TRANSLATED TO ANOTHER PROGRAM LANGUAGE WITH-    *
C * OUT THE PRIOR WRITTEN CONSENT OF HEWLETT-PACKARD COMPANY.    *
C *                                                              *
C ****************************************************************
C
C TURN ON THE PLOTTER
C
C   PARAMETERS :
C
C   LDUM1    = DUMMY PARAMETER RETAINED FOR COMPATIBILITY
C
C   LDUM2    = DUMMY PARAMETER RETAINED FOR COMPATIBILITY
C
C   LDEV     = LOGICAL UNIT OF THE PLOTTER
C
C ******************************************************************
C
C *** COMMON BLOCK
C
      COMMON /hp    / xminpu , yminpu , xorg , yorg , xoff , yoff , 
     &                xfact , yfact , xcntrl , ycntrl , xyunit , mdev , 
     &                munit , mpens , mlu , mcntrl , minit , mshake , 
     &                mptr , mcode , mplot , mflush , msize , mspool , 
     &                mver , mout(72) , rsi1 , rsi2 , rdi1 , rdi2 , 
     &                res1 , res2
      COMMON /hpsize/ xmaxpu , ymaxpu
C
C     ***** MDEV   - CONTAINS DEVICE TYPE ( 7220,7440,7470,7475,
C                     7550,7570,7580,7585,7586,7595,7596, 9872 )
C
C     ***** MINIT  - CONTAINS THE CODE FOR SOFTWARE SYSTEM INITIALIZE
C                    ( 0 = NOT INITIALIZED / 1 = INITIALIZED )
C
C     ***** MSHAKE - CONTAINS THE HANDSHAKE METHOD CODE ( 0 = ENQ/ACK
C                    1 = XON/XOFF /  2 = HP-IB )
C                    3= NO HANDSHAKE
C
C     ***** MFLUSH - CONTAINS THE CODE USED TO SIGNAL THAT AN IMMEDIATE
C                    TRANSMIT OF THE OUTPUT BUFFER MUST BE MADE ( 0 =
C                    NO IMMEDIATE TRANSMIT / 999 = IMMEDIATE TRANSMIT )
C
C *******************************************************************
C
      DIMENSION ia(4) , xbuff(1)
      DATA ia(1)/27/ , ia(2)/46/ , ia(3)/89/ , ia(4)/40/
C                ESC        .         Y         (
      mdev = mmdev
C
C**** HAS THE SOFTWARE SYSTEM BEEN INITIALIZED ?
C
      IF ( minit.NE.1 ) THEN
C
C**** INITIALIZE THE SOFTWARE SYSTEM
C
C     THE DEFAULTS ARE : PRIMARY HANDSHAKE METHOD
C                        NO MONITOR MODE
C                        CHARACTER SET 0
C                        INCHES UNIT SYSTEM
C
         lio = 10
         lmon = 0
         lchars = 0
         lunit = 0
         idev = ldev
         CALL hpinit(lio,lmon,lchars,lunit,idev,ipsize)
C
C**** SOFTWARE HAS BEEN INITIALIZED, SO TURN PLOTTER BACK ON
C
C
C**** IS THE HP-IB HANDSHAKE ENABLED ?
C**** OR ANY AT ALL?
C
      ELSEIF ( mshake.NE.2 .AND. mshake.NE.3 ) THEN
C
C**** IS THE ENQUIRE / ACKNOWLEDGE METHOD INVOKED ?
C
         IF ( mshake.NE.1 ) THEN
C
C**** YES, WE MUST IMMEDIATELY TRANSMIT THE NEXT I/O SEQUENCE
C**** SET THE I/O IMMEDIATE TRANSMIT CODE TO 999 TO SIGNAL
C**** SUBROUTINE BOUT REGARDING THIS FACT
C
            mflush = 999
         ENDIF
C
C**** ATTEMPT TO TURN ON THE PLOTTER ( ESC.Y )
C**** THIS RESOLVES THE PROBLEM WITH TERMINALS THAT
C**** MUST SEE AN UPPER-CASE CHARACTER FOLLOWING AN
C**** ESCAPE CHARACTER SEQUENCE
C
         CALL buff(1,ia(1),xbuff(1),3)
C
C**** RESET THE I/O IMMEDIATE TRANSMIT CODE IF REQUIRED
C
         mflush = 0
      ENDIF
      RETURN
      END
