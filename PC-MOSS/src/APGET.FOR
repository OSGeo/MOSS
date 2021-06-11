      SUBROUTINE apget(index,istart,istop,ntot)
C
C *******************************************************
C
C       S U B R O U T I N E     A P G E T
C
C   FUNCTION - GET THE POINTER TO THE ACTIVE MAP DATA
C              SET POINTER TABLE (MAPPTS) FROM THE
C              ACTIVE MAP STRUCTURE TABLE (ISTRCT)
C
C  INPUT PARAMETERS:
C
C      INDEX  = LOCATION IN ISTRCT OF ACTIVE DATA SET
C               OF INTEREST.
C      ISTART = RETURNED LOCATION IN POINTER TABLE OF
C               FIRST ITEM OF ACTIVE MAP OF INTEREST
C      ISTOP  = LOCATION OF LAST ITEM OF INTEREST (RETURNED)
C      NTOT   = RETURNED NUMBER OF ITEMS
C
C ROUTINES
C CALLED:    NONE.
C *********************************************************
C
      COMMON /whmap / istrct(300) , nstrct
C
      istart = istrct(index)
      ntot = istrct(index+1)
      istop = istart + ntot - 1
C
      RETURN
      END
