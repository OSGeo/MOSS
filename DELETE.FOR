C
C           PROGRAM DELETE
C
C  FUNCTION - DELETE AN ENTRY FROM A MOSS DATABASE
C
C
        PROGRAM DELETE
$Include:'strng.inc'
$Include:'dnames.inc'
        common/dbmap/dbnmap(2)
        COMMON /IO/    NPRNT,IOIN
C       COMMON /STRNG/ ICARD(80),K
        INTEGER IBUFF(128),MAP(16),KMAP(16),NAME(16),wrk(2),dbnmap
        integer mapp(25)
        LOGICAL MCOMP
C
C  INITIALIZATION
C
      call initl(ier)
      if(ier.ne.0)stop
C
C  GET MAP NAME TO DELETE
C
        CALL DECIP(ICARD,NAME,K)
        LINES=1
C
C  DID USER SPECIFY ANYTHING ?
C
        IF(NAME(1).NE.0) GO TO 10
C
C  NO.  PROMPT FOR NAME
C
        LINES=LINES+2
156        WRITE(NPRNT,1000)
 1000   FORMAT(' ENTER NAME OF MAP TO DELETE : (CR=QUIT)'\)
C
C****   NEW WAY
C
        CALL READIN(ICARD,lenicd,icardp,istop)
        IF(ICARD(1) .EQ. 0) GOTO 90000
        CALL DECIP(ICARD,NAME,icardp)
C
C  TRY TO FIND MAP
C
  10  iflagall=0
        if(mcomp(name,'ALL',3,ier))then
          iflagall=1
          call getmap(2,1,map,ier)
          nummaps=map(16)-1
          go to 40 
        endif
        CALL MCHEK(NAME,IFIL,NENTS,KTYPE,IER)
        IF (IFIL .EQ. 0) WRITE(NPRNT,1004)(name(il),il=1,10)
 1004   FORMAT(1x,10a1,':   THIS MAP DOES NOT EXIST')
        IF (IFIL .EQ. 0) GO TO 90000
C
C  IF TYPE IS MASTER, GET PASSWORD
C
        IF(IFIL.NE.1) GO TO 40
        write(*,*)
        write(*,*)'MASTER maps can only be DELETED while working in ',
     + 'the master directory.'
        go to 156
c        WRITE(NPRNT,1002)
c 1002   FORMAT(' ENTER PASSWORD'\)
C
C****   NEW WAY
C
c        CALL READIN(ICARD,lenicd,icardp,istop)
c        LINES=LINES+2
c        IF(MCOMP(ICARD(1),'ZZ',2,IER)) GO TO 40
C
C  INVALID PASSWORD.  TERMINATE COMMAND
C
c        WRITE(NPRNT,1003)
c 1003   FORMAT(' SORRY.  YOU CANNOT DO THAT')
C        GO TO 90000
C
C  MAP FOUND.  SEE IF THEY REALLY WANT TO DELETE
C
  40    CONTINUE
        if(iflagall.eq.1)then
           write(*,10066)
10066      Format(' DO YOU WISH TO DELETE ALL MAPS FROM YOUR DOS ',
     +            'DIRECTORY (Y/(N)) '\)
           go to 158
        else
           WRITE(NPRNT,1006) KTYPE,NENTS
 1006      FORMAT(' DATA TYPE = ',I5,' NUMBER OF ITEMS = ',I5,/,
     1     ' DO YOU WISH TO DELETE THIS MAP FROM YOUR DOS DIRECTORY ',
     +     ' (Y/(N)) '\)
C
C****   NEW WAY
C 
        endif
158     CALL READIN(ICARD,lenicd,icardp,istop)
C
      If(iflagall.eq.1) go to 159
C
C  RETAIN MAP NAME
C
        DO 60 I=1,10
          KMAP(I)=NAME(I)
  60    CONTINUE
C
C  DELETE FROM .DT FILE
C
        CALL GETMAP(2,1,MAP,IER)
        IF (IER .NE. 0) GO TO 90000
        LAST = MAP(16)
        NMAPS = LAST - 1
        CALL DELMAP(I,LAST,dbnmap(2),NAME,2,IER)
C      DELETE FROM DISK REGARDLESS
         IF (IER .NE. 0) GO TO 901
C
C Delete from active table
C
        call freeact(kmap)
C
C  NOW DELETE THE DISK FILES
C
      IF(.NOT.MCOMP(icard(1),'Y',1,IER)) GO TO 90000
        WRITE(*,10100)
10100   FORMAT(' ARE YOU SURE YOU WANT TO DO THIS (Y/(N)) '\)
        CALL READIN(ICARD,LENICD,ICARDP,ISTOP)
      IF(.NOT.MCOMP(icard(1),'Y',1,IER)) GO TO 90000
        IF (KTYPE .GE. 6  .AND.  KTYPE .LE. 9) GO TO 200
        do 152 ik=1,10
152     mapp(ik)=kmap(ik)
        CALL OPENF(5,mapp,1,256,2,IER)
        IF (IER .EQ. 0) CALL RDBLK(5,1,IBUFF,1,IER)
        close(5,status='delete',iostat=ier)
        IF (IER .NE. 0) WRITE(NPRNT,91000) (MAP(I),I=1,10),IER
91000   FORMAT(' *DELETE*  COULD NOT DELETE ',10A2,' FROM DISK  ERROR',
     1         I5)
        IF (IAT .EQ. 0) GO TO 90000
        CALL GTNAM(-1,4,KMAP,MAP,IER)
        IF (IER .NE. 0) WRITE(NPRNT,91000) (MAP(I),I=1,10),IER
        GO TO 90000
C
C  CELL FILES
C
  200   CONTINUE
        CALL GTNAM(-1,5,KMAP,MAP,IER)
        IF (IER .NE. 0) WRITE(NPRNT,91000) (MAP(I),I=1,10),IER
        do 210 kk=1,10
210       mapp(kk)=MAP(kk)
        CALL OPENF(5,mapp,1,256,2,IER)
        IF (IER .EQ. 0) CALL RDBLK(5,1,IBUFF,1,IER)
        close(5,status='delete',iostat=ier)
        IF (IER .NE. 0) WRITE(NPRNT,91000) (MAP(I),I=1,10),IER
        CALL GTNAM(-1,KTYPE,KMAP,MAP,IER)
        IF (IER .NE. 0) WRITE(NPRNT,91000) (MAP(I),I=1,10),IER
        do 220 kk=1,10
220       mapp(kk)=MAP(kk)
        CALL OPENF(5,mapp,1,256,2,IER)
        IF (IER .EQ. 0) CALL RDBLK(5,1,IBUFF,1,IER)
        close(5,status='delete',iostat=ier)
        IF (IER .NE. 0) WRITE(NPRNT,91000) (MAP(I),I=1,10),IER
C
        IF (KTYPE .NE. 7) GO TO 90000
        CALL GTNAM(-1,11,KMAP,MAP,IER)
        IF (IER .NE. 0) WRITE(NPRNT,91000) (MAP(I),I=1,10),IER
        do 230 kk=1,10
230       mapp(kk)=MAP(kk)
        CALL OPENF(5,mapp,1,256,2,IER)
        IF (IER .EQ. 0) CALL RDBLK(5,1,IBUFF,1,IER)
        close(5,status='delete',iostat=ier)
        CALL GTNAM(-1,12,KMAP,MAP,IER)
        IF (IER .NE. 0) WRITE(NPRNT,91000) (MAP(I),I=1,10),IER
        do 240 jj=1,10
240       mapp(jj)=MAP(jj)
        CALL OPENF(5,mapp,1,256,2,IER)
        IF (IER .EQ. 0) CALL RDBLK(5,1,IBUFF,1,IER)
        close(5,status='delete',iostat=ier)
        GO TO 90000
C
C DELETE ALL FILES
C

159     CALL GETMAP(2,1,MAP,IER)
        IF (IER .NE. 0) GO TO 90000
        LAST = MAP(16)
        NMAPS = LAST - 1
157     if( nmaps.eq.0 ) go to 90000
        Call getmap(2,2,name,ier)
        call unpack(name,16)
C
C  RETAIN MAP NAME
C
        DO 600 I=1,10
          KMAP(I)=NAME(I)
 600    CONTINUE
        CALL MCHEK(NAME,IFIL,NENTS,KTYPE,IER)
C
C  DELETE FROM .DT FILE
C
        CALL DELMAP(I,LAST,nmaps,NAME,2,IER)
        dbnmap(2)=nmaps 
C      DELETE FROM DISK REGARDLESS
         IF (IER .NE. 0) GO TO 901
C
C Delete from active table
C
        call freeact(kmap)
C
C  NOW DELETE THE DISK FILES
C
        IF(.NOT.MCOMP(ICARD(1),'Y',1,IER)) GO TO 157
        WRITE(*,10110)(KMAP(I),I=1,12)
10110   FORMAT(1x,'DELETE ',12A1,' (Y(N))'\) 
        CALL READIN(ICARD,lenicd,icardp,istop)
        IF(.NOT.MCOMP(ICARD(1),'Y',1,IER)) GO TO 157
        IF (KTYPE .GE. 6  .AND.  KTYPE .LE. 9) GO TO 2000
        do 154 ik=1,10
154     mapp(ik)=kmap(ik)
        CALL OPENF(5,mapp,1,256,2,IER)
        IF (IER .EQ. 0) CALL RDBLK(5,1,IBUFF,1,IER)
        close(5,status='delete',iostat=ier)
        IF (IER .NE. 0) WRITE(NPRNT,91000) (MAP(I),I=1,10),IER
        IF (IAT .EQ. 0) GO TO 157
        CALL GTNAM(-1,4,KMAP,MAP,IER)
        IF (IER .NE. 0) WRITE(NPRNT,91000) (MAP(I),I=1,10),IER
        GO TO 157
C
C  CELL FILES
C
 2000   CONTINUE
        CALL GTNAM(-1,5,KMAP,MAP,IER)
        IF (IER .NE. 0) WRITE(NPRNT,91000) (MAP(I),I=1,10),IER
        do 2100 Jk=1,10
2100      mapp(Jk)=MAP(Jk)
        CALL OPENF(5,mapp,1,256,2,IER)
        IF (IER .EQ. 0) CALL RDBLK(5,1,IBUFF,1,IER)
        close(5,status='delete',iostat=ier)
        IF (IER .NE. 0) WRITE(NPRNT,91000) (MAP(I),I=1,10),IER
        CALL GTNAM(-1,KTYPE,KMAP,MAP,IER)
        IF (IER .NE. 0) WRITE(NPRNT,91000) (MAP(I),I=1,10),IER
        do 2200 ij=1,10
2200      mapp(ij)=MAP(ij)
        CALL OPENF(5,mapp,1,256,2,IER)
        IF (IER .EQ. 0) CALL RDBLK(5,1,IBUFF,1,IER)
        close(5,status='delete',iostat=ier)
        IF (IER .NE. 0) WRITE(NPRNT,91000) (MAP(I),I=1,10),IER
C
        IF (KTYPE .NE. 7) GO TO 157
        CALL GTNAM(-1,11,KMAP,MAP,IER)
        IF (IER .NE. 0) WRITE(NPRNT,91000) (MAP(I),I=1,10),IER
        do 2300 jk=1,10
2300      mapp(jk)=MAP(jk)
        CALL OPENF(5,mapp,1,256,2,IER)
        IF (IER .EQ. 0) CALL RDBLK(5,1,IBUFF,1,IER)
        close(5,status='delete',iostat=ier)
        IF (IER .NE. 0) WRITE(NPRNT,91000) (MAP(I),I=1,10),IER
        CALL GTNAM(-1,12,KMAP,MAP,IER)
        IF (IER .NE. 0) WRITE(NPRNT,91000) (MAP(I),I=1,10),IER
        do 2400 ij=1,10
2400      mapp(ij)=MAP(ij)
        CALL OPENF(5,mapp,1,256,2,IER)
        IF (IER .EQ. 0) CALL RDBLK(5,1,IBUFF,1,IER)
        close(5,status='delete',iostat=ier)
        IF (IER .NE. 0) WRITE(NPRNT,91000) (MAP(I),I=1,10),IER
        go to 157 
C
C  ERROR RETURNS
C
 901    WRITE(NPRNT,3010) IER
 3010   FORMAT(' *DELETE* ERROR FROM DELMAP  ERROR',I5)
        LINES=LINES+2
        GO TO 90000
C
C  ALL DONE
C
90000   CONTINUE
        END
