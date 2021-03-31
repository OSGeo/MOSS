      SUBROUTINE krgin1(nprnt,ichan,label,dsnam,maxr,maxc,ymin)
C***
C***  CREATES A TEMPORARY DISK FILE NAMED "TEMPKRIG.PRM"
C***  WHICH CONTAINS PARAMETERS TO BE PASSED TO THE
C***  KRIGING PROGRAMS
C***
C***  MOSS      STORAGE    STATPAC        MEANING
C***  NAME                  NAME
C***
C***  NPRNT  I  I*2           -       UNIT NO FOR TERMINAL MESSAGES
C***  ICHAN  I  I*2           -       UNIT NO FOR BINARY DISK FILE
C***  LABEL  I  I*2 ARRAY     -       NAME OF DATA SET (UNPACKED CHARS)
C***  DSNAM  O  I*2 ARRAY    DSNAME   NAME OF DATA SET (PACKED CHARS,
C***                                   LEFT JUSTIFIED,PADDED WITH
C***                                   BLANKS OR NULLS)
C***  NHOLES I  I*2          NK       MAX NO OF HOLES TO BE USED
C***                                   IN KRIGING A POINT.  THIS
C***                                   IS A DUMMY PARAMETER,INCLUDED
C***                                   FOR POSSIBLE USE IN
C***                                   FUTURE REVISIONS.
C***  MAXR   I  I*2          NER      NUMBER OF ROWS
C***  YSIDE  I  R*4          WIDY               HEIGHT OF CELL
C***  MAXC   I  I*2          NEC      NUMBER OF COLS IN CELL FILE
C***  XSIDE  I  R*4          WIDX               WIDTH OF CELL
C***  YMIN   I  R*4          YMN      MIN Y OF DATA SET
C***  XMIN   I  R*4          XMN      MIN X OF DATA SET
C***
      COMMON /gridpts/ xmin , xside , ymax , yside
      INTEGER label(16) , dsnam(16) , ftemp(10)
      DATA iblnks/8224/
      DATA nholes/0/
C
C...BLANK FILL AND PACK DATA SET NAME......
C
      DO i = 1 , 16
         dsnam(i) = iblnks
      ENDDO
      CALL packc(label,dsnam,16)
C
C...CREATE AND OPEN PARAMETER FILE, WRITE PARAMETERS
C
C      DELETE "TEMPKRIG.PRM"
      ftemp(1) = 'TE'
      ftemp(2) = 'MP'
      ftemp(3) = 'KR'
      ftemp(4) = 'IG'
      ftemp(5) = '.P'
      ftemp(6) = 'RM'
      ftemp(7) = 0
      CALL openf(ichan,ftemp,0,2,1,ier)
      WRITE (ichan) (dsnam(i),i=1,16) , nholes , maxr , yside , maxc , 
     &              xside , ymin , xmin
      CLOSE (ichan,IOSTAT=iier)
      END
