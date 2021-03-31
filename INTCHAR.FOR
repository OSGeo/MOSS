      SUBROUTINE intchar(tval,ibuf,num)
      INTEGER jbuf(6) , ibuf(6)
      CHARACTER*6 cbuf
      WRITE (cbuf,'(f6.0)') tval
      k = 0
      DO l = 1 , 6
         IF ( cbuf(l:l).NE.' ' ) THEN
            k = k + 1
            jbuf(k) = ichar(cbuf(l:l))
         ENDIF
      ENDDO
      num = k
      CALL packc(jbuf,ibuf,6)
      RETURN
      END
