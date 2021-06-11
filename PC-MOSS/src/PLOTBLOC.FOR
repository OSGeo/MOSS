      BLOCKDATA plotbloc
C
C   Common initialization required for the PLOT command
C
      COMMON /blmldsh/ xt , yt , ds , iseg , ipen , dlseg(4,4)
      COMMON /mdevice/ idev
      COMMON /ththtt/ iflag , inipen , initim , pdown , pi , pup , x1 , 
     &                y1
      COMMON /thick1/ dfltpn , ntim , pnwd
      COMMON /hcurr / ichan , ifnum , ircur , icbuf(256) , x , y
C
      INTEGER pdown , pup
      REAL inipen
      DATA dlseg/.04 , .05 , .04 , .05 , .04 , .10 , .25 , .10 , .10 , 
     &     .07 , .10 , .07 , .25 , .10 , .25 , .10/
      DATA iflag/0/ , pdown/2/ , pi/3.1415926/ , pup/3/ , x1/0/ , y1/0/
      DATA idev/1/
      DATA dfltpn/.005/ , ntim/1/
      DATA ifnum/ - 32767/
      END
