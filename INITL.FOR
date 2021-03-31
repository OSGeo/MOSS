            SUBROUTINE INITL(IER)
C *********************************************************************
C
C            S U B R O U T I N E    I N I T L
C
C  FUNCTION: GENERAL INITIALIZATION
C
C ********************************************************************
C
$Include:'DNAMES.inc'
$Include:'DBCHAN.inc'
$Include:'IO.inc'
$Include:'MFAST.inc'
$Include:'RESTRT.inc'
$Include:'STRNG.inc'
$Include:'TYPE.inc'
$Include:'VWND.inc'
$Include:'WHMAP.inc'
        COMMON /CLINES/ ICLINE,IBATCH
        COMMON /PLACT/  MACTS
        COMMON /PNTFET/ NPOINT
        COMMON /FLS/    IUNIT,IFILE(5)
        common/dbmap/dbnmap(2)
        INTEGER CMDLINE(5),jmast(6)
        INTEGER*2 system
        INTEGER MAP(25),IARC(16),JCARD(12),dbnmap,jwork(25),KCARD(10)
        LOGICAL IRESTL,fexist,projdr,mcomp,iopen,REXIST,WEXIST,RECRAT
        CHARACTER USRDIR*25, ROOT*80,WFILE*2 
C
        DATA CMDLINE /2HCM,2HDL,2HIN,2HE ,2H  /
C
        iopen=.false.
        jmast(1)=ichar('M')
        jmast(2)=ichar('A')
        jmast(3)=ichar('S')
        jmast(4)=ichar('T')
        jmast(5)=ichar('E')
        jmast(6)=ichar('R')
C  flag sent in by 'OPEN' command only
        if(ier.eq.99)then
          iopen=.true.
        endif
        IER=0
        IRESTL = .TRUE.
        IHOLDP = ICARDP
        ICHAN = 50
C
C  Pull in current status if one exists
        CALL INCM
C
C  Check for restart,  CMDLINE is maintained by GIS
        CALL OPENF(ICHAN,CMDLINE,1,0,0,IER)
        do 5050 ll=1,80
 5050   icard(ll)=0
        IF ( IER .EQ. 0 ) THEN
           READ(ICHAN,1111)IRESTL,KK,(ICARD(L),L=1,80)
1111       FORMAT(L1,I2,80I3)
           CLOSE(ICHAN,STATUS='DELETE')
           ICARDP = 0
        ENDIF
C
C TEST IF DIRECTORIES AND .dt'S ARE SET UP
C
      IF(IDIRC(1).EQ. 0) THEN
       if(iopen)then
         irestl=.false.
         WRITE(NPRNT,1113)
1113     FORMAT(1X,'PLEASE ENTER THE NAME OF THE MS-DOS DIRECTORY ',
     +             'YOU ARE IN'\)
         CALL READIN(IDIRC,8,ICD,ISTOPD)
         IF ( IDIRC(1) .EQ. 0 ) GO TO 80000
         LENDIR=ISTOPD+2
         go to 4
       else
         write(*,*)
         write(*,*)' *Error* - Use the OPEN command to create a '
         write(*,*)' project catalog.'
         ier=-1
         return
       endif
      ENDIF
C
C  TEST STATUS  OF RESTART
C
      IF(IRESTL) THEN
C
C  Test status of dir's
C
         IF( IWORK(1) .EQ. 0) THEN
            WRITE(NPRNT,1114)
1114        FORMAT(1X,'* ERROR * - RESTART NOT ALLOWED - NO PATH TO ',
     +             '<.DT> LAST USED')
            go to 4
         ENDIF
         if(icard(1).ne.0)then
           go to 4
         else
           go to 20
         endif
      ENDIF
C
C  No restart - get dir's
    4 If(icard(1).ne.0)then
         Call Decip(Icard,Jcard,ICARDP)
         ic=1
         istop=ICARDP-1
         go to 12
      Endif
5     WRITE(NPRNT,1115)
1115  FORMAT(//,1X,'ENTER PROJECT NAME :'\)
      CALL READIN(JCARD,12,IC,ISTOP)
C
C if no name given assume restart
C
 12   icardp=1
      if(jcard(1).eq.0) goto 80000
      if(.not.iopen)go to 20
C
C test for name 'MASTER'
C
      do 76 il=1,6
      if(jcard(il).ne.jmast(il)) go to 77
76    continue
      write(*,*)' MASTER as a project name is invalid - again please'
      go to 5
C
C TEST AGAINST CURRENT DIRECTORY NAME
C
77    IF(ISTOP-(ICARDP+1).GT.8) THEN
        WRITE(NPRNT,1117)
1117    FORMAT(/,1X,'* ERROR * - ONLY 8 CHAR. OR LESS ALLOWED')
          GO TO 5
      ENDIF
C
C      call clear(iwork,25)
      PROJDR=.TRUE.
      DO 100  I=1,8
        if(jcard(I).eq.32)jcard(I)=0
        if(idirc(I).eq.32)idirc(I)=0
        IF(JCARD(I).NE.IDIRC(I)) PROJDR=.FALSE.
100   CONTINUE
      IF (.NOT. PROJDR) THEN
        DO 3 L=IC,ISTOP
        i=l
          jWORK(L)=JCARD(L)
3       CONTINUE
        jWORK(I+1)=ICHAR('.')
        jWORK(I+2)=ICHAR('D')
        jWORK(I+3)=ICHAR('T')
        call openf(icwork,jwork,1,512,2,ier)
        if(ier.ne.0)then
C....  Assume no project - test for existence of MASTER.DT
          INQUIRE(FILE='MASTER.DT',EXIST=FEXIST)
          IF(FEXIST) then
            WRITE(NPRNT,1116)
1116        FORMAT(/,1X,'* ERROR * - NOT ALLOWED. THIS IS A PROJECT',
     +             ' MASTER DIRECTORY')
            GOTO 80000
          ENDIF
          if(iopen)go to 56
          if(iwork(1).ne.0)then
            icardp=1
            go to 20
          else
            write(*,*)
            write(*,*)' ** Error ** That project catalog does not'
            write(*,*)'  exist.  Use the OPEN command to create it.'
            ier=-1
            return
          endif
        else
C  get <project>.ip file for new project
          jwork(i+2)=ichar('I')
          jwork(i+3)=ichar('P')
          call inip(jwork)
          go to 20
        endif
      ENDIF
C
C.... CHECKING FOR A ROOT DIRECTORY
C
      OPEN(UNIT=69,FILE='ZZ000000.DT')
      KK=0
      READ(69,'(/)')
      READ(69,1137,END=181) ROOT
1137  FORMAT(A80)
      DO 179 KJ=1,30
        IF (ROOT(KJ:KJ) .EQ. '\') KK=KK+1
179   CONTINUE
181   IF(KK .GT. 1) THEN
        WRITE(NPRNT,1119) root(16:79)
1119    FORMAT(/,1X,'* ERROR * - CANNOT CREATE MASTER CATALOG AT',
     +          ' NON-ROOT LEVEL DOS-DIRECTORY:'/,1x,a64/)
        GOTO 80000
      ENDIF
      INQUIRE(FILE='MASTER.DT',EXIST=FEXIST)
      RECRAT=.FALSE.
      IF(FEXIST) THEN
        WRITE(NPRNT,1129) root(16:31)
1129    FORMAT (' Do you want to DELETE the entire existing MASTER',/
     +          ' project catalog in the current directory ',a16,/
     +          ' and recreate it without any map entries?  (Y/[N]) '\)
        CALL READIN(KCARD,8,JJJ,III)
        IF (KCARD(1).EQ.89 .OR. KCARD(1).EQ.121) THEN
           PRINT *,'Are you sure?  (Y/[N])'
           call readin(kcard,8,jjj,iii)
           if (kcard(1).eq.89 .or. kcard(1).eq.121) then
             RECRAT=.TRUE.
             GOTO 85
           endif
           goto 80000
        ELSE
           GOTO 80000
        ENDIF
      ENDIF
      REWIND (UNIT=69,IOSTAT=IER)
      READ(69,'(/)')
      IWFILE=0
      DO 279 JK=1,100
        READ(69,19111,END=281) WFILE
19111   FORMAT(T10,A2)
        IF (WFILE .EQ. 'DT') IWFILE=IWFILE+1
279   CONTINUE
281   CLOSE(UNIT=69,IOSTAT=IER)
      IF (IWFILE .GT. 1) THEN
        WRITE(NPRNT,1127) root(16:79)
1127    FORMAT(/,1x,'* ERROR * - ANOTHER CATALOG ALREADY EXISTS IN',
     +  ' CURRENT DOS DIRECTORY:'/,11x,a64,/
     +             '           - CANNOT CREATE MASTER.DT HERE')
        GOTO 80000
      ENDIF
C
C  IF PROJECT DIR. - LOAD 'MASTER' INTO IWORK
C
85         IF(PROJDR) THEN
            IWORK(1)=ICHAR('M')
            IWORK(2)=ICHAR('A')
            IWORK(3)=ICHAR('S')
            IWORK(4)=ICHAR('T')
            IWORK(5)=ICHAR('E')
            IWORK(6)=ICHAR('R')
            IWORK(7)=ICHAR('.')
            IWORK(8)=ICHAR('D')
            IWORK(9)=ICHAR('T')
C
C  SET UP DIR'S FOR USER DIRECTORY W/ PROJECT NAME
C  LOAD MASTER DIRECTORY NAME AND PATH
C
         mast(1)=ichar('\')
         i=1
         do 11 l=ic,istop
            i=i+1
   11    mast(i)=jcard(l)
         mast(i+1)=ichar('\')
         mast(i+2)=ichar('M')
         mast(i+3)=ichar('A')
         mast(i+4)=ichar('S')
         mast(i+5)=ichar('T')
         mast(i+6)=ichar('E')
         mast(i+7)=ichar('R')
         mast(i+8)=ichar('.')
         mast(i+9)=ichar('D')
         mast(i+10)=ichar('T')
c capture project name length
         mast(25)=i+1
C
C  OPEN IT OR INITIALIZE AND SET IT UP
C
            I=0
            DO 333 L=1,6
               I=I+1
               idesfa(i)=jmast(l)
               idesdt(i)=jmast(l)
  333       CONTINUE
            idesfa(i+1)=ichar('.')
            idesfa(i+2)=ichar('F')
            idesfa(i+3)=ichar('A')
            idesfa(i+4)=ichar(' ')
            idesdt(i+1)=ichar('.')
            idesdt(i+2)=ichar('S')
            idesdt(i+3)=ichar('L')
            idesdt(i+4)=ichar(' ')
            GOTO 35
         ENDIF
C
C  LOAD USER PROJECT PATH AND NAME
C
56       I=0
         DO 33 L=IC,ISTOP
            I=I+1
            IWORK(I)=JCARD(L)
            idesfa(i)=jcard(l)
            idesdt(i)=jcard(l)
  33     CONTINUE
         IWORK(I+1)=ICHAR('.')
         IWORK(I+2)=ICHAR('D')
         IWORK(I+3)=ICHAR('T')
         iwork(i+4)=ichar(' ')
         idesfa(i+1)=ichar('.')
         idesfa(i+2)=ichar('F')
         idesfa(i+3)=ichar('A')
         idesfa(i+4)=ichar(' ')
         idesdt(i+1)=ichar('.')
         idesdt(i+2)=ichar('S')
         idesdt(i+3)=ichar('L')
         idesdt(i+4)=ichar(' ')
C
C  DIRS' NOW LOADED
C OPEN THEM - 0=OVER WRITE IF EXIST  1=MUST EXIST  2=CREATE IF NOT EXIST  
C             & SET THEM UP
C
35      IF(PROJDR) THEN
          IF(RECRAT) THEN
            open (-123,file='master.dt')
            open (-124,file='master.fa')
            open (-125,file='master.ip')
            open (-126,file='master.sl')
            open (-127,file='intrproc')
            open (-128,file='lm')
            close (-123,status='delete')
            close (-124,status='delete')
            close (-125,status='delete')
            close (-126,status='delete')
            close (-127,status='delete')
            close (-128,status='delete')
            CALL OPENF(ICMAST,MAST,2,512,2,IER)
            if (ier.eq.0) then
              CALL TREDIR(IP1,IP2,ip3,ICMAST,MAST,IER)
              dbnmap(1)=ip3
            endif
          ELSE
            CALL OPENF(ICMAST,MAST,2,512,2,IER)
          ENDIF
        ENDIF 
C
C  OPEN USER/PROJ AND SET UP
C
55       CALL OPENF(ICWORK,IWORK,2,512,2,ier)
         CALL TREDIR(ITP1,ITP2,itp3,ICWORK,IWORK,IER)
         dbnmap(2)=itp3
C
         jj=0
         do 58 l=1,12
   58    if(iwork(l).eq.46)jj=l-1
         write(*,118)(iwork(l),l=1,jj)
 118  format(/,1x,'Current Project =',12a1,/)
C
C  NOT A RESTART - DO REINITIALIZE AND OPEN WHAT HAS TO BE OPEN
C
C  SET VARIABLES AND ARRAYS
C
 15   MLEN = 512
      IUNIT = 0
      IRESTL=.false.
      ICLINE=23
      NPOINT=0
      NSTRCT=0
      MACTS=0
      NACTS=0
      IBW=-1
      XORGIN=0.0
      YORGIN=0.0
      XALEN=0.0
      YALEN=0.0
      CALL CLEAR(IDATYP,40)
      CALL CLEAR(ISTRCT,300)
      IWTRN=0
C
      CALL OPENF(icdcfa,idesfa,2,512,2,IER)
      read(icdcfa,rec=1,err=24) ifbuff
      go to 6
24    WRITE(icdcfa,REC=1) IFBUFF
  6   CLOSE(icdcfa,IOSTAT=IER)
      call openf(icdcdt,idesdt,2,62,2,ier)
      close(icdcdt,iostat=ier)
      GO TO 99999
C
C  IS A RESTART - SO OPEN NECESSARY FILES AND SET UP
C  SET WINDOW AND OPEN FEATURE FILE
C
 20   IF(IBW.EQ.0) CALL VWINDO(XORGIN,XALEN,YORGIN,YALEN)
      IF(IBW.EQ.0) CALL SCRSET(XORGIN,XALEN,YORGIN,YALEN)
      IF(IBW.EQ.1) CALL VWINDO(XSTART,XLEN,YSTART,YLEN)
      IF(IBW.EQ.1) CALL SCRSET(XSTART,XLEN,YSTART,YLEN)
C
C  OPEN 'DESCRIBE.FA'
      CALL OPENF(ICDCFA,IDESFA,1,512,2,IER)
       if(ier.ne.0)then
        write(*,*)
        write(*,*)'Use OPEN Command to Open a project first'
        go to 80000
       endif
      read(icdcfa,rec=1,err=21)ifbuff
      go to 22
  21  write(icdcfa,rec=1)ifbuff
  22  INCORE=1
      IWTRN=0
      CLOSE(icdcfa,IOSTAT=IER)
      LINES=LINES+4
C
C GET MASTER DIRECTORY
C
 40   CONTINUE
      CALL OPENF(ICMAST,MAST,1,512,2,IER)
      IF (IER .NE. 0) GOTO 60 
C
C  READ MASTER DIRECTORY
C
      CALL TREDIR(IP1,IP2,ip3,ICMAST,MAP,IER)
      dbnmap(1)=ip3
C
C OPEN POLYGON WORKFILE
C
 60   IF(IWORK(1).NE.0) THEN
         CALL OPENF(ICWORK,IWORK,2,512,2,IER)
         IF(IER.NE.0) THEN
            WRITE(NPRNT,1120)(IWORK(L),L=1,8)
 1120       FORMAT(/,1X,'* ERROR * - UNABLE TO OPEN ',
     +             'PROJECT CATALOG.:',12A1)
            write(*,*)' Use the Open command to create a new project '
            write(*,*)' catalog'
            ier=-1
            return
         ENDIF
C
C  READ IN USER WORKFILE DIRECTORY
         CALL TREDIR(ITP1,ITP2,itp3,ICWORK,IWORK,IER)
         dbnmap(2)=itp3
         jj=0
         do 59 l=1,12
   59    if(iwork(l).eq.46)jj=l-1
         write(*,118)(iwork(l),l=1,jj)
      ENDIF
C
C  AND RETURN
C
       GO TO 99999
C
C      ** ERROR RETURNS ****
80000  IER = 1
C
C
99999  continue
       RETURN
       END
