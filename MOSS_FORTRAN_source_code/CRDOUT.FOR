      SUBROUTINE crdout
      COMMON /strng / icard(80) , icardc , initl
      LOGICAL initl
      OPEN (50,FILE='cmdline',STATUS='new',IOSTAT=ier)
      IF ( ier.NE.0 ) THEN
         OPEN (50,FILE='cmdline',STATUS='old')
         CLOSE (50,STATUS='delete')
         OPEN (50,FILE='cmdline',STATUS='new')
      ENDIF
      icardc = 80
      WRITE (50,'(L1,i2,80i3)') initl , icardc , (icard(l),l=1,icardc)
      CLOSE (50)
      END
