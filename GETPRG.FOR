C * Subroutine GetNum gets the menu location of the command that is being
C   accessed, and moves the sub-command into the location of the main command,
C   if applicable.
 
      SUBROUTINE getprg
 
$Include:'GLOBAL.inc'
      LOGICAL dup
      INTEGER duphold(10)
C# Menu position.
      prgrun = 0
      dup = .FALSE.
      kk = 0
C * First, see if a menu number was given.
 
C# Up to 128 commands available.
      IF ( cmdstr(1:1).LE.'9' ) THEN
         DO k = 1 , 3
            IF ( cmdstr(k:k).LE.'9' ) THEN
               IF ( cmdstr(k:k).GE.'0' ) THEN
                  IF ( prgrun.GT.0 ) THEN
                     prgrun = prgrun*10
                  ENDIF
                  prgrun = prgrun + (ichar(cmdstr(k:k))-48)
               ENDIF
            ENDIF
         ENDDO
      ENDIF
 
C * If no match there, check the menu file itself.
 
      IF ( prgrun.EQ.0 ) THEN
         DO i = 1 , ionum
            IF ( prgrun.EQ.0 ) THEN
               DO l = 1 , 8
c  convert menu character to uppercase
                  chr = imenu(i)(l:l)
                  CALL cnvupr(chr,1)
                  IF ( imenu(i)(l:l).EQ.' ' ) GOTO 10
                  IF ( cmdstr(l:l).EQ.' ' ) GOTO 10
                  IF ( cmdstr(l:l).NE.chr ) GOTO 50
               ENDDO
 10            CONTINUE
               prgrun = i
c
c check for duplicate name based on number of characters given
c
               dup = .FALSE.
               kk = 1
               duphold(kk) = i
               DO ii = i + 1 , ionum
                  DO l = 1 , 8
c  convert menu character to uppercase
                     chr = imenu(ii)(l:l)
                     CALL cnvupr(chr,1)
                     IF ( imenu(ii)(l:l).EQ.' ' ) GOTO 15
                     IF ( cmdstr(l:l).EQ.' ' ) GOTO 15
                     IF ( cmdstr(l:l).NE.chr ) GOTO 20
C      write(*,'(2(a10))')chr,cmdstr(l:l)
                  ENDDO
 15               CONTINUE
                  dup = .TRUE.
                  kk = kk + 1
                  duphold(kk) = ii
 20            ENDDO
            ENDIF
 50      ENDDO
         IF ( dup ) THEN
            WRITE (*,*)
            WRITE (*,*) 'Cannot be certain of your command. '
            WRITE (*,*)
            DO ll = 1 , kk
               WRITE (*,*) '             ' , imenu(duphold(ll))(1:10)
            ENDDO
            WRITE (*,*)
            WRITE (*,*) 'Again please'
            prgrun = 0
         ENDIF
      ENDIF
 
C * If ProgRun still equals 0, there was no exact match, or there was no
C   number.
      IF ( prgrun.NE.0 ) THEN
         IF ( prgrun.GT.ionum ) THEN
            prgrun = 0
         ENDIF
      ENDIF
C * If ProgRun still equals 0, then reset string to ' ', since any input was
C   invalid.
 
      IF ( prgrun.EQ.0 ) THEN
         cmdstr = ' '
         IF ( .NOT.dup ) THEN
            WRITE (*,*) 'Command not understood - again please!'
         ENDIF
      ENDIF
      RETURN
      END
