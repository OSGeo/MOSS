      Subroutine Spoints(w,array,Value,sube,mode)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     S U B R O U T I N E  S P O I N T S
c
c   gets and distributes map coordinates via the array (array) and via
c   disk in blocks of 5000.
c
c   w     = character denoting which array to get/put (x,y,p,q,r,s,xr,yr)
c   array = array containing the most recent block of 5000 points.
c   Value = the value to either get from or put into array.
c   sube  = the external subscript for array (1 - 32700)
c   mode  = 1 = get value , 2 = put value.
c
c   subi  = the internal subscript computed via the 5000 point block
c           will be 1-5000.
c   fopen = contains unit number for open disk arrays (121-126)
c   fswap = contains character of array varaible names (x,y,p,q,r,s,xr,yr)
c   Iblock = currently accessed block # per array. Each file has two arrays
c            (x,y),(p,q),(r,s),(xr,yr),(xy(1,xy(2))
c
c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      Character w*2, fswap(10)*2, swapf(5)*2, idrv*2
c
      Common /swap/iblock(5,2), fopen(5), iswap(10), subi
      Common /swapc/idrv, fswap, swapf
      Real array(500)
      Integer sube,subi,fopen,updown
c
c   Find block and internal subscript values for incoming external subscript
c
      inow = (sube/500)+1
      if(mod(sube,500).eq.0) then
       inow=inow-1
      endif
      subi=  mod(sube,500)
      if (subi.eq.0) subi=500
c
      do 10 l = 1 , 10
        if (w.eq.fswap(l)) go to 11
10    continue  
      write(*,*)'Error finding array name ',w
      stop
c
c  iwhich = iswap(l) = arrray set (1-5) being worked upon
c
11    iwhich = iswap(l)
      updown = mod(l,2)
      if(updown.eq.1) then
         updown = 0
      else
         updown=1
      endif
c
        if (iblock(iwhich,(updown+1)).eq.inow) then
c
c   Same Block
c
c    Do value swap
c
         if (mode.eq.1) then
           value = array(subi)
         else
           array(subi) = value
         endif
c
      else
c
c    Different Block
c
c    Be sure file is open
c     
         if (fopen(iwhich).eq.0) then
c
c  Compute unit number
c
          fopen(iwhich) = 120 + iwhich
c
          Open(fopen(iwhich),access='DIRECT',
     *         status='SCRATCH',form='UNFORMATTED', recl=2000,
     *         blocksize=2000,iostat=ier)
c
            if (ier.ne.0) then
               write(*,*)'Error opening disk array',swapf(iwhich),ier
               stop
            endif
         endif
c
c  Get or put new Block
c
c  Compute needed record numbers
c
         irecold = (iblock(iwhich,(updown+1)) -1) *2 +1 + updown
         irecnew = (inow -1) *2 +1 + updown
c
c  Figure out if top or bottom part of block
c
          if (mode.eq.2) then
c
c  PUT array away
c
             write(fopen(iwhich),rec=irecold) array
c
             iblock(iwhich,(updown+1)) = inow
c
          else
c
c  GET a new block - put current one away first
c
              write(fopen(iwhich),rec=irecold) array
c 
              read(fopen(iwhich),rec=irecnew,iostat=ier) array
c
c
              if (ier.ne.0) then
                do 20 k = 1 , 500
20              array(k) = 0.0
                write(*,*)' Error reading new block ',irecnew,irecold,
     *          inow,sube,subi,ier,fopen(iwhich),iwhich,
     *          iblock(iwhich,updown+1),updown
              endif
c
              iblock(iwhich,(updown+1)) = inow
           endif
c
c  Now that we have changed blocks - update array or value
c
       if (mode.eq.1) then
            value = array(subi)
       else
            array(subi) = value
       endif
c
      endif
c
      return
      end

