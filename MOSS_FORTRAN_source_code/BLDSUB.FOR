      SUBROUTINE bldsub(nsub,iarr,macts,ictemp,ier)
C*****************************************************************************
C
C      NAME:     SUBROUTINE BLDSUB
C
C      FUNCTION: RUN THROUGH THE ACTIVE MAP TABLE BUILDING A FILE OF
C                SUBJECTS INTO 'TEMP'.
C
C
C      NSUB = NUMBER OF SORTED SUBJECTS IN FILE 'TEMPS'   -  OUTPUT
C      IARR  = ARRAY OF ACTIVE MAP ID'S                   -  INPUT
C      MACTS = DIMENSION OF IARR...NUMBER OF ACTIVE MAPS  -  INPUT
C      ICTEMP= CHANNEL FOR SORTED DATA TO GO TO           -  IN
C      TEMP  = FILE NAME OF TEMPORARY FILE                -  IN
C      IER   = 0 = NO ERROR
C
C
C
C*****************************************************************************
C
$Include:'PARAMS.inc'
$Include:'DBCHAN.inc'
$Include:'MFAST.inc'
      COMMON /io    / nprnt , ioin
      DIMENSION iarr(macts) , icom(10) , ihead(128)
      INTEGER temp(namelen) , subjp , subject(15) , mbr(8) , mapname(25)
C
      icmap = 21
      nsub = 0
C
C  Open 'DESCRIBE.FA' for GETFAST
      ifchan = icdcfa
      CALL openf(icdcfa,idesfa,2,512,2,ier)
      IF ( ier.NE.0 ) THEN
C
C  ERROR RETURNS
C
C
         WRITE (nprnt,99001) icdcfa , ier
99001    FORMAT (' *BLDSUB*  ERROR OPENING DESCRIBE.FA ON CHANNEL',I5,
     &           '  ERROR',I5)
      ELSE
C
         DO i = 1 , macts
            index = iarr(i)
C
C  GET MAP NAME FOR THE INPUT DATA
C
            CALL clear(icom(1),10)
            CALL gtmap(icom,index)
            CALL clear(mapname,25)
C
C  OPEN CHANNEL TO INPUT MAP
C
            CALL adddir(icom,mapname)
            CALL openf(icmap,mapname,1,256,2,ier)
            IF ( ier.NE.0 ) THEN
C
               WRITE (nprnt,99002) mapname , ier
99002          FORMAT (' *BLDSUB* ERROR OPENING MAP ',25A1,'  ERROR ',
     &                 I5)
               GOTO 100
            ELSE
C
               CALL apget(index,istart,istop,ntot)
               DO ii = istart , istop
                  CALL getfast(ii,mbr,nrec,subjp,iex1,iex2,ier)
                  CALL rdblk(icmap,subjp,ihead,1,ier)
                  IF ( ier.NE.0 ) THEN
                     WRITE (nprnt,99003) subjp , ier
99003                FORMAT (' *BLDSUB* ERROR READING SUBJECT POINTER ',
     &                       I5,' ERROR ',I5)
                     CLOSE (icmap)
                     GOTO 50
                  ENDIF
C
C  READ THE SUBJECT FROM THE MAP AND WRITE IT IN 'TEMP1'
                  nsub = nsub + 1
                  CALL wrblk(ictemp,nsub,ihead,1,ier)
                  IF ( ier.NE.0 ) THEN
C
                     WRITE (nprnt,99004) nsub , ier
99004                FORMAT (' *BLDSUB*  ERROR WRITING RECORD ',I5,
     &                       ' TO TEMP','  ERROR ',I5)
                     GOTO 100
                  ENDIF
               ENDDO
               CLOSE (icmap)
            ENDIF
 50      ENDDO
C
C  Close fast file
C
         CLOSE (icdcfa)
      ENDIF
C
 100  CONTINUE
      RETURN
      END
