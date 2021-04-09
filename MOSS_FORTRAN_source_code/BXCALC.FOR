      SUBROUTINE bxcalc(xbox,ybox,ndim,slen)
      DIMENSION xbox(ndim) , ybox(ndim)
C
C ***************************************************
C
C       S U B R O U T I N E    B X C A L C
C
C  FUNCTION -  CLACULATE A BOX OF SIDE LENGTH SLEN
C
C  INPUT PARAMETERS -
C
C       (XBOX,YBOX) - RETURNED WITH COORDINATES OF BOX
C       NDIM        - DIMENSION OF (XBOX,YBOX)
C       SLEN        - LENGTH OF SIDE OF BOX
C
C ROUTINES
C CALLED:
C             NONE.
C
C ***************************************************
C
      hblen = slen/2.
      xbox(1) = hblen
      ybox(1) = hblen
      ybox(2) = -hblen
      xbox(2) = hblen
      xbox(3) = -hblen
      ybox(3) = -hblen
      xbox(4) = -hblen
      ybox(4) = hblen
      xbox(5) = xbox(1)
      ybox(5) = ybox(1)
C
      RETURN
      END
