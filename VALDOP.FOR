      SUBROUTINE valdop(iparam,iscrch,unit10,unit14,unit21,ext1,ext2)
C***  PROGRAM MODIFIED BY GEORGE HEINE, BLM DSC, MAY 1985
C***     UNIT10 (STATPAC INPUT FILE) IS ALWAYS 'TEMPKRIG.UPR'
C***     UNIT21 (FORMATTED OUTPUT FILE) IS 'TEMPKRIG.FGR'
C***     UNIT14 (STATPAC OUTPUT FILE) IS ALWAYS 'TEMPKRIG.UGR'
C***       THIS FILE IS OVERWRITTEN IF IT ALREADY EXISTS
$INCLUDE: 'MYST.INC'
      CHARACTER*32 unit10 , unit14 , unit21
      CHARACTER*4 ext0 , ext1 , ext2
      CHARACTER*8 iholid
C      CHARACTER*4 IVIB
      INTEGER*2 ex , ey , name(16)
C
      unit10 = 'TEMPKRIG.UPR'
      unit21 = 'TEMPKRIG.FGR'
      unit14 = 'TEMPKRIG.UGR'
C.....OPEN......
C      OPEN(ipt,FILE=UNIT10,STATUS='OLD',FORM='UNFORMATTED')
      OPEN (ipr,FILE=unit21,STATUS='OLD',IOSTAT=ierr)
      IF ( ierr.GT.0 ) THEN
         OPEN (ipr,FILE=unit21,STATUS='NEW',FORM='FORMATTED')
      ELSE
         READ (ipr,'(a)',END=50,IOSTAT=ierr1) iholid
 50      CONTINUE
         CLOSE (ipr,STATUS='DELETE')
         OPEN (ipr,FILE=unit21,STATUS='NEW',FORM='FORMATTED')
      ENDIF
      OPEN (iout,FILE=unit14,STATUS='OLD',IOSTAT=ierr)
      IF ( ierr.GT.0 ) THEN
         OPEN (iout,FILE=unit14,STATUS='NEW',FORM='UNFORMATTED')
      ELSE
         READ (iout,END=100,IOSTAT=ierr1) iholid
 100     CONTINUE
         CLOSE (iout,STATUS='DELETE')
         OPEN (iout,FILE=unit14,STATUS='NEW',FORM='UNFORMATTED')
      ENDIF
      OPEN (iscrch,STATUS='SCRATCH',FORM='UNFORMATTED')
      OPEN (iparam,FILE='TEMPKRIG.PRM',STATUS='OLD',FORM='UNFORMATTED')
      READ (iparam) unit21
      RETURN
      END
