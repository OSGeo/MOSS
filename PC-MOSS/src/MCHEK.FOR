      SUBROUTINE mchek(icom,ifil,nents,itype,ier)
C****************************************************************************
C  FUNCTION:   CHECK ALL OPEN .DT FILES FOR OCCURENCE OF A MAP NAME
C
C  PARAMETERS:   ICOM      IN - THE MAP FILE NAME
C                IFIL      OUT -  1 IF FILE FOUND IN MASTER.DT
C                                 2 IF FILE FOUND IN POLYGON.DT
C                NENTS     OUT - NUMBER OF ENTRIES
C                ITYPE     OUT - TYPE OF MAP
C                IER       OUT -
C****************************************************************************
C
C
$Include:'DBCHAN.inc'
$Include:'DNAMES.inc'
C
      DIMENSION icom(12)
      LOGICAL scomp
C
      ifil = 0
C
      IF ( scomp(nmaps,icom,scale,nents,itype,ier,icwork) ) ifil = 2
      IF ( ifil.NE.2 ) THEN
C
         IF ( mast(1).NE.0 ) THEN
            IF ( scomp(nmaps,icom,scale,nents,itype,ier,icmast) )
     &            ifil = 1
         ENDIF
      ENDIF
C
C   Zero out any error that SCOMP might have returned
      ier = 0
C
      RETURN
      END
