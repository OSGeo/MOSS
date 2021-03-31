C * Subroutine Banner:  This subroutine gives the program banner with any
C   identifying characteristics deemed appropriate by the programmer who
C   creates/edits/modifies this subroutine.  We take requests.
 
      SUBROUTINE banner
 
$Include:'GLOBAL.INC'
 
 
C# Get initial date/time.
C **** Currently Data General only
C	Call GetDte ( ITime, IDate, Error )
C **** With other systems
      error = .TRUE.
 
      WRITE (usrout,99001)
99001 FORMAT (' ')
 
      IF ( display ) THEN
C# Identify program.
         whoiam = level(lvlnum)
         WRITE (usrout,99002) whoiam
99002    FORMAT ('>>>>> ',A,)
      ENDIF
 
C# Identify current program version.
      IF ( display ) THEN
         IF ( vrsion.GT.0.0 ) THEN
            WRITE (usrout,99003) vrsion
         ENDIF
99003    FORMAT (' BLM/DSC/DOI - CGTL/CFNR/CSU  Version ',F5.2)
      ENDIF
 
C# Make sure date/time were available before printing.
      IF ( error ) THEN
         WRITE (usrout,99004)
         error = .FALSE.
      ELSE
         WRITE (usrout,99005) itime , idate(2) , idate(3) , idate(1)
      ENDIF
99004 FORMAT (' ')
99005 FORMAT (' - ',2(I2.2,':'),I2.2,' ',2(I2.2,'/'),I4.4,/)
 
 
      RETURN
      END
