      BLOCKDATA fonts
C
      COMMON /pwindo/ xmin , ymin , xmax , ymax , xlast , ylast , lastfg
      COMMON /mdevice/ idev
      COMMON /thick1/ dfltpn , ntim , pnwd
      COMMON /ththht/ iflag , inipen , initim , pdown , pi , pup , x1 , 
     &                y1
C
      DATA xmin , ymin , xmax , ymax/ - 9.E10 , -9.E10 , 9.E10 , 9.E10/
      DATA idev/2/
      DATA iflag , dfltpn , ntim , pdown/0 , .005 , 1 , 2/
      DATA pi , pup , x1 , y1/3.14159 , 3 , 0.0 , 0.0/
      END
