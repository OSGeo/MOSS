      SUBROUTINE ingrd(iparam,ner,widy,nec,widx,ymn,xmn)
C***  THIS SUBROUTINE INPUTS DESCRIPTORS OF THE GRID OF POINTS
C***  FROM TEMPORARY PARAMETER FILE CREATED BY MOSS ROUTINE KRIGIN1.
C***  WRITTEN BY GEORGE HEINE, BLM DSC TO REPLACE ORIGINAL
C***  STATPAC ROUTINE 'INGRID' BY W.D. GRUNDY
      INTEGER*2 name(16)
      REWIND (iparam)
      READ (iparam,IOSTAT=ier,ERR=100) (name(ii),ii=1,16) , nk , ner , 
     &                                 widy , nec , widx , ymn , xmn
      GOTO 200
 100  CONTINUE
      WRITE (*,99001) ier
99001 FORMAT ('**ERROR*** COULD NOT READ FROM PARAMETER FILE ',
     &        '   ERROR',I5)
      CLOSE (iparam)
 200  CONTINUE
      RETURN
      END
