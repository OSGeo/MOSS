      SUBROUTINE mx(iact,lun,jpos,ibuf,lbuf,ierr)
C-----------------------------------------------------------------------
CP
C     READ OR WRITE DATA FROM A DIRECT ACCESS FILE
C     (THE FILE MUST BE PREVIOUSLY OPENED WITH SUBROUTINE MF)
CE
CA
C     IACT --- ACTION (1 - WRITE, 2 - READ, 3 - FIND LENGTH)
C     LUN ---- UNIT NUMBER FOR FILE
C     JPOS --- ADDRESS OF DATA IN FILE
C     IBUF --- RECORD BUFFER
C     LBUF --- LENGTH OF RECORD
C     IERR --- ERROR RETURN
C              0 - NO ERROR
C             -1 - END OF FILE ON READ OR WRITE
C             -2 - CANNOT READ RECORD
CE
C----------------------------------------------------------------------
C
      INTEGER*4 jpos
      INTEGER*2 iact , lbuf , ibuf(lbuf) , ierr , lun
C
C
      INTEGER*2 ix , err , ic , lun2 , is0 , is128
      INTEGER*4 pos
C
C  Initialize
      is0 = 0
      is128 = 128
      ierr = is0
      ic = lbuf
C      LUN2=LUNPRM(LUN)
C
C .. DETERMINE OFFSET
      pos = (jpos)
C
      IF ( iact.EQ.1 ) THEN
C
C .. WRITE A RECORD
         WRITE (lun,REC=pos,IOSTAT=ierr) ibuf
         IF ( ierr.EQ.0 ) GOTO 200
C
C .. IERROR -- CANNOT WRITE RECORD
C
         WRITE (*,*) 'Cannot write record' , jpos , ' unit= ' , lun
         ierr = -2
         RETURN
C
      ELSEIF ( iact.NE.2 ) THEN
C
C .. FIND LENGTH
         pos = 1
 50      CONTINUE
         READ (lun,REC=pos,END=100,IOSTAT=ierr) ibuf
         IF ( ierr.NE.0 ) THEN
            WRITE (*,*) 'Cannot determine end of file' , lun
            ierr = -2
            RETURN
         ELSE
            pos = pos + 1
            GOTO 50
         ENDIF
      ELSE
C
C .. READ A RECORD
         READ (lun,REC=pos,END=300,IOSTAT=ierr) ibuf
         IF ( ierr.EQ.0 ) GOTO 200
C
C .. IERROR -- CANNOT READ RECORD
C
         WRITE (*,*) 'Cannot read record' , jpos , ' unit= ' , lun
         ierr = -2
         RETURN
      ENDIF
 100  CONTINUE
      BACKSPACE (lun)
      ibuf(1) = pos
C
C .. ALL DONE
 200  CONTINUE
      RETURN
C
C .. IERROR -- END OF FILE
C
 300  CONTINUE
      ierr = -1
      RETURN
C
      END
