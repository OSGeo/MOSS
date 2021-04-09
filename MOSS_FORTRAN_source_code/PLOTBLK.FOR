      block data plotbloc
C
C   Common initialization required for the PLOT command
C
      Common/blmldsh/xt,yt,ds,iseg,ipen,dlseg(4,4)
      common/mdevice/idev
      common/ththtt/iflag,inipen,initim,pdown,pi,pup,x1,y1
      common /thick1/dfltpn,ntim,pnwd
      common/hcurr/ichan,ifnum,ircur,icbuf(256),x,y
C
      integer pdown,pup
      real inipen
      data dlseg/.04,.05,.04,.05,
     +           .04,.10,.25,.10,
     +           .10,.07,.10,.07,
     +           .25,.10,.25,.10/
      data iflag/0/,pdown/2/,pi/3.1415926/,pup/3/,x1/0/,y1/0/
      data idev/1/
      data dfltpn/.005/,ntim/1/
      data ifnum/-32767/
      end
