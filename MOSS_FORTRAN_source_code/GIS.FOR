C * GIS: A  Global generic menu caller for BLM/GIS system

$include:'SPAWN.INC'
$Include:'GLOBAL.inc'
      character*40 systring
      character*10 prg(128)
      character*1 chr
      character*12 hold
      character*8 prog,cmdstrh
      character*13 cmnd1
      character*30 cmnd2
      character*80 syscmd
      integer prgid(128),prgnum
      common /strng/icard(80),icardc,initl
      logical holdf,initl
      integer*2 spawn,system
C# Initialize global variables.
      Call InitGV
C# Initialize system file.
      whoIam='GIS'
      vrsion=1.0
      NulCmd=0
      IONum=0
      FileNm='Null'
      Call LdCmd
      Nulcmd=IONum
      initl=.true.
      Holdf=.false.
      write(*,*)char(27),char(12)
C# Initial level.
      LvlNum = 1
C# Driver name.
      Level(LvlNum) = WhoIAm
      istepp=0
c#
c#  load in table of active programs names
c#
      open(1,file='\gis\intrface\active.prg',access='SEQUENTIAL',
     +status='OLD',iostat=ier,err=210)
      do 130 l=1,128
 130  read(1,'(i3,1x,a10)',end=135)prgid(l),prg(l)
 135  close(1,iostat=ier)
      prgnum=l-1
C
C Get initial menu
C
  350 Call LodMnu
C
C  Display it < against the display switch>
C
  300 Call wrtmnu
C
C clear the process transfer area
C
      Call Filint(icard,80,0,1)
C
C  Ask and get user response
C
      Call Ask
C
      Call Readln (usrin,Cmdstr,80,error,.true.,' ',.true.)
      syscmd=cmdstr
C
C  Parse it
C
      Call Getwrd(Cmdstr,80,Cmnds,80,20,Icmdfd)
C
      If(Icmdfd.gt.0)then
        Index=0
C
C  Find out what was said
C
  302   index=index+1
        if(index.gt.icmdfd) then
          go to 300
        endif
        Cmdstr=cmnds(index)
        Call Getprg
C        write(*,*)' ',index, cmnds(index),icmdfd,prgrun
C
C  Was Anything said
C
        If(Prgrun.eq.0)then
C
C  Not understood - user has been told - ask all over again 
C
          icmdfd=0
C
C Jump to next command
C
          Go to 302
C
        Else
C
C   Was understood  <prgrun >> 0 >
C
C
C Null Command ????
C
        If(Prgrun.le.Nulcmd)then
C
C Yes! - Exit??
C
          If(Prgrun.eq.1)then
C
C  Back up a level
C
      Level(lvlnum)=' '
      Lvlnum=Lvlnum-1
C
C  Are we finished
C
      If(Lvlnum.gt.0)then
       whoIam=Level(lvlnum)
       go to 350
      else
C
C Yes!!!
C
        Go to 301
      endif
C
          Else
C
C  Yes! - Help???
C
          If(Prgrun.eq.2)then
            Help=.true.
C
C Is next command valid - or there at all
C
            Index=index+1
            If(Index.gt.Icmdfd)then
C
C  Need to ask user - Help What??
C
              Call Wrtmnu
C
424           Call Ask
C
              Call Readln(Usrin,Cmdstr,80,error,.true.,' ',.true.)
              If(Cmdstr.ne.' ')then
                Call Getwrd(Cmdstr,80,Cmnds,80,20,Icmdfd)
                  cmdstrh=cmdstr
                  call Getprg
                  if(prgRun.gt.0) then
                  cmdstrh=imenu(prgrun)
                else
                  call mhelp(ier,cmdstr,1)
                  if(ier.eq.0)then
                    help=.false.
                    go to 300
                  endif
                  write(*,*)'Help <name> not available - again please!'
                  go to 424
                endif
                call Mhelp(ier,cmdstrh,1)
                help=.false.
              Else
                help=.false.
                Go to 300
              Endif
C
            Else
C
C  There is a next command  - in Cmnds(indsx)
C
              cmdstr=cmnds(index)
                call Getprg
                if(prgRun.gt.0) then
                  cmdstr=imenu(prgrun)
                else
                  cmdstr=cmnds(index)
                  call mhelp(ier,cmdstr,1)
                  if(ier.eq.0) then
                   help=.false.
                   go to 300
                  endif
                  write(*,*)'Help <name> not available - again please!'
                  go to 424
                endif
              call Mhelp(ier,cmdstr,1)
              help=.false.
              Go to 300
            Endif
C
          Else
C
C  Yes! - Display switch (menu on/off)
C
          If(Prgrun.eq.3)then
C
C  Flip switch
C
            If(display)then
               Display=.false.
           Else
              Display=.true.
            Endif
          Go to 302
C
          Endif
          Endif
          Endif
C
        Else
C
C   Prgrun > Nulcmd - Must be a command or program
C
         If(Ctype(Prgrun).eq.'P')then
C
C  Increment a level - downward
C
C  Load a new menu for that level
C
            Call Clrmnu
            Call Lodmnu
C
C  Get next command for Cmnds if available
C
            Go to 302
C
         Else
C
C  Is it a Program to exec
C
         If(Ctype(Prgrun).eq.'A')then
C
C  Get actual command to use
C  Loop thru current list of righteous and good commnds
C
            Do 310 ll=1,Prgnum
C
C Now loop char by char comparing for a hit
C
              Do 315 k=1,10
C
                If(Imenu(Prgrun)(k:k).eq.' ')go to 320
                Chr=Imenu(Prgrun)(k:k)
C
C Since list is upper case - convert menu name to upper
C
                If(Ichar(Chr).gt.96)chr=char(ichar(chr)-32)
C
C  Does char match - if no - get next name in list
C
                If(Prg(ll)(k:k).ne.chr)go to 310
C
  315          Continue
             GO TO 320
  310        Continue
C
C This should not occur - bad trubs.
C
      Write(*,*)' Not able to invoke that command - check with'
      Write(*,*)' your GIS Representative'
C
              Go to 300
C
C
C  Prg found - activate it
C
C  load up cmd line response area
C
  320        If(prg(ll)(1:3).eq.'SYS')then
               cmdstr=' '
               if(index.lt.icmdfd)then
                 index=index+1
                 cmdstr=cmnds(index)
               endif
               call sysdo(syscmd,systring,ierr)
               if(ierr.ne.0) then
                 ierr=0
                 prgrun=0
                 go to 300
               endif
               i=system(systring//char(0))
               prgrun=0
               go to 300
             endif
             prog=prg(ll)
             Icardc=0
             initl=.true.
             do 321 il=1,80
  321        icard(il)=0
             if(prg(ll)(1:4).eq.'OPEN')initl=.false.
              if(prg(ll)(1:6).eq.'ATTDES') then
                  icard(80)=1
                  prog='ATTRIBUT'
              endif
C
C fill with remaining cmds
C
              Istart=Index
              If(.not.Holdf)Istart=Index+1
               Do 335 i=Istart,Icmdfd
                 Do 340 k=1,12
                  If(Cmnds(i)(k:k).eq.' ')go to 345
                  Icardc=Icardc+1
  340             Icard(icardc)=Ichar(Cmnds(i)(k:k))
C
C Put a space in between each item
C
  345            Icardc=Icardc+1
  335          Continue
C
C  Dump to Process storage area    < file = 'cmdline' >
C
             if(prog(1:6).ne.'ATTRIB')icard(80)=istepp
             Call Crdout
C
C**********************************************************************
C
C  Execute the active program
C
             I=0
C
C            write(*,*)' ',ll,prgid(ll),prg(ll),prog,imenu(prgrun)(1:10)
C
              if(prog(1:3).eq.'ERA')then
               write(*,*)char(27),char(12)
               systring='del lm'
               i=system(systring//char(0))
               go to 180
              endif
             cmnd1=prog
             do 111 lm=1,8
              if(prog(lm:lm).eq.' ')then
                cmnd1(lm:lm+5)='.exe'//char(0)
                go to 112
              endif
  111        continue
             cmnd1(9:13)='.exe'//char(0)
  112      cmnd2='\gis\exe\'//cmnd1
C          write(*,*)cmnd1,cmnd2 
             i=spawn(0,loc(cmnd2),loc(cmnd2),int4(0))
C*************************************************************************
C
C  Were we (share the blame) successful
C
 180       if(i.eq.-1)then
             write(*,*)
             write(*,*) prg(ll),' not currently available.'
             write(*,*) 'Possible reasons:'
             write(*,*) ' 1) File ',prg(ll)(1:8),'.EXE not found'
             write(*,*) '    in subdirectory \GIS\EXE.'
             write(*,*) ' 2) Not enough memory available'
             write(*,*) '    to load ',prg(ll)(1:8),'.EXE.'
             write(*,*)
             write(*,*)
             go to 300
            else
C
C  Yes!!!!
             istepp=istepp+1
             holdf=.false.
             initl=.true.
             go to 300
            endif
            Endif
            Endif
            Endif
            Endif
C
C Get next given command if available
C
         endif
        go to 300
  210 write(*,*)' unable to access active program list (file'
      write(*,*)' \GIS\INTRFACE\ACTIVE.PRG) - check with your'
      write(*,*)' GIS representative'
 301  continue
      End
