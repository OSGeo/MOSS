      SUBROUTINE quparm(attout,edit,alread,xt,yt,ier)
$Include:'io.inc'
$Include:'strng.inc'
      LOGICAL attout , edit , alread , mcomp
      DIMENSION icom(10) , ixt(2) , iyt(2)
      EQUIVALENCE (ixt(1),xxt) , (iyt(1),yyt)
C
C *********************************************************************
C
C                S U B R O U T I N E  Q U P A R M
C
C  Purpose: This routine checks for additional parameters in the
C           QUERY command.  The parameters checked are for attribute
C           dumps, attribute edit, and passing in the search coordinate
C           from another MOSS function
C
C  Parameters:
C     Name       Use        Type         Dimension       Description
C     ----       ---        ----         ---------   ------------------
C
C     ICARD       I         INT          2/4 BYTES   USER INPUT STRING
C     icardp           I         INT          2/4 BYTES   POINTER INTO ICARD
C     ATTOUT      O         LOG          2/4 BYTES   ATTRIBUTE DUMP FLAG
C     EDIT        O         LOG          2/4 BYTES   EDIT ATTR. FLAG
C     ALREAD      O         LOG          2/4 BYTES   X,Y SEARCH POINT
C                                                    PASSED IN.
C     X           O         REAL         4 POINTS    X SEARCH POINT
C     Y           O         REAL         4 BYTES     Y SEARCH POINT
C     IER         O         REAL         2/4 BYTES   ERROR RETURN
C
C  Files:    Logical Unit       File name or description
C            ------------       ------------------------
C **********************************************************************
C
C
      attout = .FALSE.
      edit = .FALSE.
      alread = .FALSE.
C
C  CHECK FOR HIDDEN ATTRIBUTE DUMP
C
      kk = icardp
      CALL decip(icard,icom,icardp)
      IF ( mcomp(icom,'AT',2,ier) ) attout = .TRUE.
      IF ( .NOT.attout ) icardp = kk
C
C  CHECK EDIT FLAG
C
      CALL decip(icard,icom,icardp)
      IF ( mcomp(icom,'ED',2,ier) ) edit = .TRUE.
      IF ( edit ) attout = .TRUE.
C
C  CHECK FOR  DATA SENT IN
C
      IF ( icard(80).EQ.2 ) THEN
C
         alread = .TRUE.
         ixt(1) = icard(76)
         ixt(2) = icard(77)
         iyt(1) = icard(78)
         iyt(2) = icard(79)
         xt = xxt
         yt = yyt
      ENDIF
C
      ier = 0
      icard(80) = 0
C
      RETURN
      END
