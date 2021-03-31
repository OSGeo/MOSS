      PROGRAM open
c
c  function = to open an alternative master project directory and
c             master database, and store the name and path in mast()
c
c
c get initial settings
c
$Include:'SPAWN.INC'
      INTEGER*2 system
      OPEN (UNIT=69,FILE='ZZ000000.DT',STATUS='UNKNOWN')
      WRITE (69,*) 'scratch'
      CLOSE (UNIT=69,IOSTAT=ier)
      i = system('DIR *.DT >ZZ000000.DT'//char(0))
      ier = 99
      CALL initl(ier)
      IF ( ier.NE.0 ) THEN
         j = system('DEL ZZ000000.DT'//char(0))
         STOP '  '
      ENDIF
c
      CALL outcm
      j = system('DEL ZZ000000.DT'//char(0))
      END
