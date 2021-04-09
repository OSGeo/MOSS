      SUBROUTINE linkup(ichan,index,nrecs,ibuff,ioff)
C
C ***************************************************************
C
C              S U B R O U T I N E    L I N K U P
C
C FUNCTION:   FOLLOW A LINKED LIST TO COMPLETION
C
C PARAMETERS:
C        ICHAN IN - DATA CHANNEL ON WHICH MAP FILE RESIDES
C        INDEX IN - HEAD OF LINKED LIST
C        NRECS IN - NUMBER OF ENTRIES IN LIST
C        IBUFF IN - WORK SPACE
C        IOFF  IN - LOCATION OF LINK FIELD IN RECORD
C
C ****************************************************************
C
      COMMON /pntfet/ npoint
      COMMON /io    / nprnt , ioin
      DIMENSION ibuff(128)
C
C  STORE POINTER
C
      ipoint = index
      DO i = 1 , nrecs
         CALL rdblk(ichan,ipoint,ibuff,1,ier)
         npoint = npoint + 1
         CALL putfast(npoint,ibuff(15),ipoint,ibuff(24),ipoint,ipoint,
     &                ier)
         IF ( ier.NE.0 ) WRITE (nprnt,99001) npoint , ier
         ipoint = ibuff(ioff)
      ENDDO
C
C  ERROR RETURN
C
99001 FORMAT (' *LINKUP*  ERROR FROM PUTFAST OF RECORD',I5,' ERROR',I5)
C 85040  FORMAT(' *LINKUP*  ERROR FROM OPEN OF POINT.DT  ERROR',I5)
      RETURN
      END
