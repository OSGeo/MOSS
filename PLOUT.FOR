      SUBROUTINE plout(n,icl,irec)
C
$include:'xypnts.inc'
      COMMON /rundata/ xmin , ymin , dx , dy , ncols , nrows , nosave , 
     &                 itemid , idisp , ictsub
      DIMENSION icat(5) , icoord(4) , ihold(6) , itext(6)
      COMMON /fls   / iunit , ifile(5)
      COMMON /io    / nprnt , ioin
      COMMON /cof   / cofs
      COMMON /lablr / isub(128) , xd(128) , yd(128) , ict
      LOGICAL nosave
      CHARACTER*30 chrsub
      EQUIVALENCE (icoord(1),xt) , (icoord(3),yt)
      DATA icat/5*'  '/
C
C ****************************************************************
C
C            S U B R O U T I N E   P L O U T
C
C  FUNCTION - OUTPUT A CONTOUR LINE (WITH LABEL INFO) ON THE
C             CRT.
C
C  INPUT PARAMETERS
C
C      (X,Y)  - ARRAY CONTAINING CONTOUR TO BE OUTPUT
C      N      - NUMBER OF POINTS IN CONTOUR LINE
C      ICL    - CONTOUR LABEL
C
C *************************************************************************
C
      IF ( irec.GT.0 ) THEN
         ict = ict + 1
         isub(ict) = icl
C
         IF ( idisp.NE.0 ) THEN
            CALL spoints('X ',x,x1,1,1)
            CALL spoints('Y ',y,y1,1,1)
            xd(ict) = x1
            yd(ict) = y1
C    OUTPUT CONTOUR LINES
            CALL movea(x1,y1)
            DO i = 2 , n
               CALL spoints('X ',x,xi,i,1)
               CALL spoints('Y ',y,yi,i,1)
               CALL drawa(xi,yi)
            ENDDO
         ENDIF
c      call spoints('X ',x,xn,n,1)
c      call spoints('Y ',y,yn,n,1)
c      call movea(xn,yn)
C      CALL flsh
C
C  SAVE RESULT ?
C
         IF ( nosave ) THEN
            WRITE (chrsub,'(i6)') icl
            WRITE (ictsub,REC=irec,IOSTAT=ier,ERR=100) chrsub
            ih = 0
            WRITE (18) chrsub , (icat(i),i=1,5) , n , ih
            DO i = 1 , n
               CALL spoints('X ',x,xt,i,1)
               CALL spoints('Y ',y,yt,i,1)
c        XT=X(I)
c        YT=Y(I)
               WRITE (18) (icoord(ii),ii=1,4)
            ENDDO
            itemid = itemid + 1
         ENDIF
         RETURN
      ELSE
c
c  plot out labels
c
         DO k = 1 , 128
            IF ( xd(k).GT.0.0 ) THEN
               CALL movea(xd(k),yd(k))
               CALL toutpt(31)
               WRITE (chrsub(1:6),'(i6)') isub(k)
               ijj = 0
               DO il = 1 , 6
                  IF ( chrsub(il:il).NE.' ' ) THEN
                     CALL toutpt(ichar(chrsub(il:il)))
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
         CALL flsh
         RETURN
      ENDIF
 100  CONTINUE
      WRITE (nprnt,99001) ictsub , ier
99001 FORMAT (' PLOUT: ICTSUB,IER',2I7)
      RETURN
      END
