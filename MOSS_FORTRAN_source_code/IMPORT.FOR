      BLOCKDATA import
$Include:'DBCHAN.inc'
$Include:'DNAMES.inc'
$Include:'STRNG.inc'
      COMMON /io    / nprnt , ioin
      COMMON /pntfet/ npoint
      COMMON /attfet/ kat(86) , lbat
      COMMON /whmap / istrct(300) , nstrct
      COMMON /plact / macts
      COMMON /vwnd  / idumm(17)
      COMMON /type  / idatyp(40) , nacts
      COMMON /tktrnx/ itstuf(66)
      COMMON /output/ buffer(132) , ipt , iwrd , string(2)
      COMMON /fls   / iunit , ifile(5)
      COMMON /pplacc/ ixp , iyp
      COMMON /table / itable(40)
      COMMON /clines/ icline , ibatch
      COMMON /whatterm/ kterm
      COMMON /user  / name(10)
      COMMON /ptxlst/ incore , iwxlst
      COMMON /pxhigx/ high
      COMMON /dirsy / lastc , lastbk , iwrtn , kdir(256)
      COMMON /ptxrdt/ nnrec
      COMMON /rbx   / irbgx , ichx
      COMMON /rby   / irgby , ichy
      COMMON /rbyr  / irbyr , ichyr
      COMMON /restart/ irestl
$Include:'vwnd.inc'
      COMMON /udcovrt/ rmet(8) , feet(8)
      COMMON /maxs  / mx(16) , rmax
C        COMMON /DSBUF/MLEN,IDATA(512)
      CHARACTER w*2 , fswap(10)*2 , swapf(5)*2 , idrv*2
c
      COMMON /swap  / iblock(5,2) , fopen(5) , iswap(10) , subi
      COMMON /swapc / idrv , fswap , swapf
      INTEGER subi , fopen
      LOGICAL irestl
      INTEGER high
      INTEGER buffer , string
      DATA nprnt/6/ , ioin/5/
      DATA npoint/0/ , kat/86*0/ , lbat/0/ , istrct/300*0/ , nstrct/0/
      DATA icard/80*0/ , icardp/0/ , lenicd/80/ , macts/0/ , idumm/17*0/
      DATA idatyp/40*0/ , nacts/0/ , itstuf/66*0/ , buffer/132*0/
      DATA ipt/0/ , iwrd/0/ , string/2*0/ , iunit/0/ , ifile/5*0/
      DATA ixp/0/ , iyp/0/ , itable/40*0/ , icline/0/ , ibatch/0/ , 
     &     kterm/0/
      DATA mast/25*0/ , iwork/25*0/ , name/10*0/
      DATA high/0/ , nnrec/0/ , incore/0/ , lastc/0/ , lastbk/0/ , 
     &     iwrtn/0/
      DATA ichx/51/ , ichy/52/ , ichyr/53/ , iwxlst/0/
      DATA irestl/.FALSE./
      DATA icline , ibatch/2*0/
      DATA xorgin , yorgin , xalen , yalen , ibw/4*0 , -1/
C        DATA MLEN,IDATA/512,512*0/
      DATA icmast/1/ , icwork/2/ , icdcfa/7/ , icdcdt/8/
C
      DATA rmet/1609.34 , 1000.0 , 1.0 , .3048006 , .6254 , 1853.25 , 
     &     .9144018 , 5.02921/
      DATA feet/5280.0 , 3280.0 , 3.28083 , 1.0 , .08333 , 6080.2 , 
     &     3.0 , 16.5/
C
      DATA mx/2 , 16 , 8 , 18 , 10 , 16 , 32767 , 64 , 0 , 1024 , 0 , 
     &     0 , 0 , 0 , 0 , 256/
      DATA rmax/9999999.0/
      DATA iblock/10*1/
      DATA fswap/'X ' , 'Y ' , 'P ' , 'Q ' , 'R ' , 'S ' , 'XR' , 'YR' , 
     &     'X1' , 'X2'/
      DATA iswap/1 , 1 , 2 , 2 , 3 , 3 , 4 , 4 , 5 , 5/
      DATA swapf/'XY' , 'PQ' , 'RS' , 'XR' , 'YX'/
      DATA idrv/'D:'/
      END
