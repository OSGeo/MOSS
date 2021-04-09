      SUBROUTINE sspoints(w,array,value,sube,mode)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     S U B R O U T I N E  S P O I N T S
c
c   gets and distributes map coordinates via the array (array) and via
c   disk in blocks of 500.
c
c   w     = character denoting which array to get/put (x,y,p,q,r,s,xr,yr)
c   array = array containing the most recent block of 500 points.
c   Value = the value to either get from or put into array.
c   sube  = the external subscript for array (1 - 32700)
c   mode  = 1 = get value , 2 = put value.
c
c   subi  = the internal subscript computed via the 500 point block
c           will be 1-500.
c   fopen = contains unit number for open disk arrays (121-126)
c   fswap = contains character of array varaible names (x,y,p,q,r,s,xr,yr)
c   Iblock = currently accessed block # per array. Each file has two arrays
c            (x,y),(p,q),(r,s),(xr,yr),(xy(1,xy(2))
c
c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      CHARACTER w*2 , fswap(10)*2 , swapf(5)*2 , idrv*2
c
      COMMON /swap  / iblock(5,2) , fopen(5) , iswap(10) , subi
      COMMON /swapc / idrv , fswap , swapf
      REAL array(500)
      INTEGER sube , subi , fopen , updown
c
c   Find block and internal subscript values for incoming external subscript
c
      inow = (sube/500) + 1
      IF ( mod(sube,500).EQ.0 ) THEN
         inow = inow - 1
      ENDIF
      subi = mod(sube,500)
      IF ( subi.EQ.0 ) subi = 500
c
      DO l = 1 , 10
         IF ( w.EQ.fswap(l) ) THEN
c
c  iwhich = iswap(l) = arrray set (1-5) being worked upon
c
            iwhich = iswap(l)
            updown = mod(l,2)
            IF ( updown.EQ.1 ) THEN
               updown = 0
            ELSE
               updown = 1
            ENDIF
c
            IF ( iblock(iwhich,(updown+1)).NE.inow ) THEN
c
c
c    Different Block
c
c    Be sure file is open
c
               IF ( fopen(iwhich).EQ.0 ) THEN
c
c  Compute unit number
c
                  fopen(iwhich) = 120 + iwhich
c
                  OPEN (fopen(iwhich),ACCESS='DIRECT',STATUS='SCRATCH',
     &                  FORM='UNFORMATTED',RECL=2000,BLOCKSIZE=2000,
     &                  IOSTAT=ier)
c
                  IF ( ier.NE.0 ) THEN
                     WRITE (*,*) 'Error opening disk array' , 
     &                           swapf(iwhich) , ier
                     STOP
                  ENDIF
               ENDIF
c
c  Get or put new Block
c
c  Compute needed record numbers
c
               irecold = (iblock(iwhich,(updown+1))-1)*2 + 1 + updown
               irecnew = (inow-1)*2 + 1 + updown
c
c  Figure out if top or bottom part of block
c
               IF ( mode.EQ.2 ) THEN
c
c  PUT array away
c
                  WRITE (fopen(iwhich),REC=irecold) array
c
                  iblock(iwhich,(updown+1)) = inow
c
               ELSE
c
c  GET a new block - put current one away first
c
                  WRITE (fopen(iwhich),REC=irecold) array
c
                  READ (fopen(iwhich),REC=irecnew,IOSTAT=ier) array
c
c
                  IF ( ier.NE.0 ) THEN
                     DO k = 1 , 500
                        array(k) = 0.0
                     ENDDO
                     WRITE (*,*) ' Error reading new block ' , irecnew , 
     &                           irecold , inow , sube , subi , ier , 
     &                           fopen(iwhich) , iwhich , 
     &                           iblock(iwhich,updown+1) , updown
                  ENDIF
c
                  iblock(iwhich,(updown+1)) = inow
               ENDIF
c
c  Now that we have changed blocks - update array or value
c
               IF ( mode.EQ.1 ) THEN
                  value = array(subi)
               ELSE
                  array(subi) = value
               ENDIF
c
c   Same Block
c
c    Do value swap
c
            ELSEIF ( mode.EQ.1 ) THEN
               value = array(subi)
            ELSE
               array(subi) = value
c
            ENDIF
c
            RETURN
         ENDIF
      ENDDO
      WRITE (*,*) 'Error finding array name ' , w
      STOP
      END
