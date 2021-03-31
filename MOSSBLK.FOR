        BLOCK DATA IMPORT
$Include:'DBCHAN.inc'
$Include:'DNAMES.inc'
$Include:'STRNG.inc'
        COMMON /IO/     NPRNT,IOIN
        COMMON /PNTFET/ NPOINT
        COMMON /ATTFET/ KAT(86),LBAT
        COMMON /WHMAP/  ISTRCT(300),NSTRCT
        COMMON /PLACT/  MACTS
        COMMON /VWND/   IDUMM(17)
        COMMON /TYPE/   IDATYP(40),NACTS
        COMMON /TKTRNX/ ITSTUF(66)
        COMMON /OUTPUT/ BUFFER(132),IPT,IWRD,STRING(2)
        COMMON /FLS/    IUNIT,IFILE(5)
        COMMON /PPLACC/ IXP,IYP
        COMMON /TABLE/  ITABLE(40)
        COMMON /CLINES/ ICLINE,IBATCH
        COMMON /WHATTERM/KTERM
        COMMON /USER/   NAME(10)
        COMMON /PTXLST/ INCORE,IWXLST
        COMMON /PXHIGX/ HIGH
        COMMON /DIRSY/ LASTC,LASTBK,IWRTN,KDIR(256)
        COMMON /PTXRDT/ NNREC
        COMMON /RBX/IRBGX,ICHX
        COMMON /RBY/IRGBY,ICHY
        COMMON /RBYR/IRBYR,ICHYR
        COMMON /RESTART/IRESTL
$Include:'vwnd.inc'
        common/udcovrt/rmet(8),feet(8)
        common/maxs/ mx(16),rmax
C        COMMON /DSBUF/MLEN,IDATA(512)
      Character w*2, fswap(10)*2, swapf(5)*2, idrv*2
c
      Common /swap/iblock(5,2), fopen(5), iswap(10), subi
      Common /swapc/idrv, fswap, swapf
      Integer subi, fopen
        LOGICAL IRESTL
        INTEGER HIGH
        INTEGER BUFFER,STRING
        DATA NPRNT/6/,IOIN/5/
        DATA NPOINT/0/,KAT/86*0/,LBAT/0/,ISTRCT/300*0/,NSTRCT/0/
        DATA ICARD/80*0/,ICARDP/0/,LENICD/80/,MACTS/0/,IDUMM/17*0/
        DATA IDATYP/40*0/,NACTS/0/,ITSTUF/66*0/,BUFFER/132*0/
        DATA IPT/0/,IWRD/0/,STRING/2*0/,IUNIT/0/,IFILE/5*0/
        DATA IXP/0/,IYP/0/,ITABLE/40*0/,ICLINE/0/,IBATCH/0/,KTERM/0/
        DATA MAST/25*0/,IWORK/25*0/,NAME/10*0/
        DATA HIGH/0/,NNREC/0/,INCORE/0/,LASTC/0/,LASTBK/0/,IWRTN/0/
        DATA ICHX/51/,ICHY/52/,ICHYR/53/,IWXLST/0/
        DATA IRESTL/.FALSE./
        DATA ICLINE,IBATCH/2*0/
        DATA XORGIN,YORGIN,XALEN,YALEN,IBW/4*0,-1/
C        DATA MLEN,IDATA/512,512*0/
        DATA ICMAST/1/,ICWORK/2/,ICDCFA/7/,ICDCDT/8/
C
        data rmet/1609.34,1000.0,1.0,.3048006,.6254,1853.25,.9144018,
     +  5.02921/
        data feet/5280.0,3280.0,3.28083,1.0,.08333,6080.2,3.0,16.5/
C
        data mx/2,16,8,18,10,16,32767,64,0,1024,0,0,0,0,0,256/
        data rmax/9999999.0/
        data iblock/10*1/
        data fswap/'X ','Y ','P ','Q ','R ','S ','XR','YR','X1','X2'/
        data iswap/ 1,   1,   2,   2,   3,   3,   4,   4,   5,   5/
        data swapf/'XY','PQ','RS','XR','YX'/
        data idrv/'D:'/
        END
