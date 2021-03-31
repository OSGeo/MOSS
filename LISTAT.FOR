        SUBROUTINE LISTAT()
C
C************************************************************************
C
C NAME : SUBROUTINE LISTAT
C
C FUNCTION : TO OUTPUT INFORMATION CONCERNING THE ATTRIBUTE SET AND THEIR
C            POLYGON VALUES.
C
C
C ROUTINES REQUIRED :  RDBLK, HEADER, DIGOUT,RLSOUT, CHROUT, RECEIVE,
C                      READIN
C************************************************************************
C
$Include:'IO.inc'
$Include:'STRNG.inc'
C
          COMMON/ UNIT / CHANEL, ATRBTE, POLYGON, ADRESS
C
          INTEGER CHANEL, ATRBTE, POLYGON, ADRESS, ACTION, KEY( 80 ),
     +              RECORD( 128 )
C
C
C     CALL FLSH
  10  CONTINUE
      WRITE(NPRNT,1111)
1111  FORMAT(///,' PLEASE ENTER OPTION: ',
     1       /'  1 = LIST INDIVIDUAL ATTRIBUTE BASED ON KEY ',
     2       /'  2 = LIST INDIVIDUAL ATTRIBUTE BASED ON I.D.',
     3       /'  3 = LIST ATTRIBUTE HEADER INFORMATION ',
     4       /'  4 = TERMINATE ATTRIBUTE BROWZE'\)
       CALL READIN(ICARD,LENICD,ICARDP,istop)
       CALL INUM(ICARD,ACTION,ICARDP)
C
       NUMBER = ATRBTE + 1
C      CALL MERASE
       IF( ACTION .LE. 0   .OR.   ACTION .GT. 8 ) THEN
          WRITE(NPRNT,1112)
 1112     FORMAT(' YOU ENTERED AN INVALID OPTION.  TRY AGAIN')
          GO TO 10
       ENDIF
C
        GO TO(2000,4000,9000,9999),ACTION
C
C SEARCHING THE ATTRIBUTE LIST FOR A ATTRIBUTE WITH A SPECIFIED ATTRIBUTE
C KEY, IF FOUND WRITE OUT THE ATTRIBUTE SPECIFICATIONS AND IT'S
C POLYGON VALUES.
C
2000      CONTINUE
            WRITE(NPRNT,1007)
 1007       FORMAT(' PLEASE TYPE IN THE ATTRIBUTE KEY OF INTEREST'\)
            CALL RECEIVE( KEY, 10 )
C
            DO 2500 LOCATE = 2, NUMBER
C
                CALL RDBLK( CHANEL, LOCATE, RECORD, 1, IER )
                I = 1
                DO 2300 INDEX = 2, 11
C
                  IF(KEY(I) .NE. RECORD( INDEX ) ) GO TO 2500
                  I = I + 1
C
2300            CONTINUE
C
                GO TO 5000
C
2500      CONTINUE
C
          WRITE(NPRNT,1008)
 1008     FORMAT(' INVALID KEY.  PLEASE TRY AGAIN')
          GO TO 10
C
C
C ENTER THE ATTRIBUTE'S ID OF INTEREST. IF THE ID VALID LIST OUT THE
C ATTRIBUTE'S SPECIFICATION AND IT'S POLYGON VALUES.
C
4000        CONTINUE
                WRITE(NPRNT,1010)
 1010           FORMAT(' PLEASE TYPE IN THE ATTRIBUTE I.D.'\)
                CALL READIN(ICARD,LENICD,ICARDP,istop)
                CALL INUM(ICARD,ACTION,ICARDP)
C
             IF( ACTION .GT. 0  .AND.  ACTION .LE. ATRBTE )GO TO 4100
                  WRITE(NPRNT,1011)
 1011             FORMAT(' INVALID ATTRIBUTE I.D.  TRY AGAIN')
                  GO TO 10
C
4100            CONTINUE
                  ACTION = ACTION + 1
                  CALL RDBLK( CHANEL, ACTION, RECORD, 1, IER )
C
5000            CONTINUE
                  CALL HEADER( RECORD )
C
C CALL DIGOUT IF THE POLYGON DATA TYPE IS INTEGER.
C CALL RLSOUT IF THE POLYGON DATA TYPE IS REALS
C CALL CHROUT IF THE POLYGON DATA TYPE IS CHARACTER STRING.
C
                  GO TO( 5100, 5200, 5300 ) RECORD( 84 )
C
5100              CONTINUE
                        CALL DIGOUT( RECORD( 86 ) )
                        GO TO 10
C
5200              CONTINUE
                        CALL RLSOUT( RECORD( 86 ) )
                        GO TO 10
C
5300              CONTINUE
                        CALL CHROUT( RECORD( 86 ), RECORD( 85 ) )
                        GO TO 10
C
8900            FORMAT( 1H  , 71( 1H-  )  )
C
C
C
C WRITE OUT IN TABLE FORMAT THE ENTIRE LIST OF ATTRIBUTE IDS, THE DATA
C TYPE OF THEIR POLYGONS, THE ATTRIBUTE KEYS, AND THE ATTRIBUTE DESCRIPTORS.
C
9000        CONTINUE
                WRITE( NPRNT, 9100 )
9100            FORMAT(' ID TY LN   KEY',19X,'DESCRIPTOR'  )
                WRITE( NPRNT, 8900 )
                LOCATE = ATRBTE + 1
                DO 9500 LIMIT = 2, LOCATE
C
                    CALL RDBLK( CHANEL, LIMIT, RECORD, 1, IER )
                    DO 9250 INDEX = 2, 11
                        IF( RECORD( INDEX ) .EQ. 0 ) RECORD( INDEX )= 32
9250                CONTINUE
                    IF( RECORD( 84 ) .EQ. 1 )
     +                  WRITE( NPRNT, 9301 ) ( RECORD( I ), I = 1, 70 )
9301                    FORMAT(1X,I3,1X,'I',4X,10A1,1X,60A1)
                    IF( RECORD( 84 ) .EQ. 2 )
     +                  WRITE( NPRNT, 9302 ) ( RECORD( I ), I = 1, 70 )
9302                    FORMAT(1X,I3,1X,'R',4X,10A1,1X,60A1)
                    IF( RECORD( 84 ) .EQ. 3 )
     +                  WRITE( NPRNT, 9303 ) RECORD(1),RECORD(85),
     +                                       (RECORD(I),I=2,70)
9303                    FORMAT(1X,I3,1X,'C',1X,I2,1X,10A1,1X,60A1)
C
9500            CONTINUE
                WRITE( NPRNT, 8900 )
                GO TO 10
C
C        CALL MERASE
 9999   RETURN
        END
