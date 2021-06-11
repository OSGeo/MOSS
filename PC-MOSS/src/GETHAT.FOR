      SUBROUTINE gethat(jbuff,ncross,icolor,rotat,isym,dens)
      COMMON /io    / nprnt , ioin
      INTEGER jbuff(128)
      DIMENSION icolor(1) , rotat(1) , isym(1) , dens(1)
      INTEGER it1(2) , it2(2)
      EQUIVALENCE (it1(1),dd) , (it2(1),rr)
C
C *************************************************************
C
C            S U B R O U T I N E   G E T H A T
C
C  FUNCTION:  THIS ROUTINE EXTRACTS THE HATCH INFORMATION FROM
C             AN CARTOGRAPHIC ATTRIBUTE RECORD.
C
C  INPUT/OUTPUT
C
C      JBUFF   = INPUT CARTOGRAPHIC ATTRIBUTE RECORD
C      ICOLOR = OUTPUT COLORS
C      ISYM   = OUTPUT SYMBOLOGY
C      ROTAT  = OUTPUT ROTATIONS
C      DENS   = OUTPUT DENSITIES
C
C ************************************************************
C
      ncross = jbuff(19)
      IF ( ncross.LT.1 ) ncross = 1
C
C  LOOP AND EXTRACT INFORMATION
C
      ioff = 20
      DO i = 1 , ncross
C
         isym(i) = jbuff(ioff)
         it1(1) = jbuff(ioff+2)
         it1(2) = jbuff(ioff+3)
         dens(i) = dd
         it2(1) = jbuff(ioff+4)
         it2(2) = jbuff(ioff+5)
         rotat(i) = rr
         icolor(i) = jbuff(ioff+6)
C
         ioff = ioff + 7
C
C        IF(IDEB.NE.0) WRITE(NPRNT,2000) ISYM(I),DENS(I),ROTAT(I),ICOLOR(I)
99001    FORMAT (' SYM,DENS,ROT,COL ',I5,2F8.2,I5)
C
C
      ENDDO
C
      RETURN
 
      END
