      SUBROUTINE devget(icard,icardc)
C
C  routine to get and change the graphic output device and status
C
      COMMON /fls   / iunit , ifile(5)
      DIMENSION icard(80) , icom(10) , iwrk(25)
      LOGICAL mcomp
C
C  see if anything remains in command line array
C
      CALL decip(icard,icom,icardc)
      IF ( icom(1).EQ.0 ) THEN
C
C nothing good given - tell current status
C
         WRITE (*,*)
         IF ( iunit.EQ.6 ) THEN
            WRITE (*,*) 'Screen is current device'
            RETURN
         ELSE
            WRITE (*,99001) (ifile(ll),ll=1,5)
99001       FORMAT (1X,'File : ',5A2,' is current device',//,
     &              ' Use   DEVICE *   to return output to the SCREEN')
            RETURN
         ENDIF
C
C  should be a device option
C
      ELSEIF ( mcomp(icom,'*',1,ier) .OR. mcomp(icom,'SCREEN',6,ier) )
     &          THEN
         iunit = 6
         ifile(1) = 0
         RETURN
      ELSE
         CALL gtnam(-1,16,icom,iwrk,ier)
         DO l = 1 , 5
            ifile(l) = iwrk(l)
         ENDDO
         iunit = 60
         RETURN
      ENDIF
C
C
      END
