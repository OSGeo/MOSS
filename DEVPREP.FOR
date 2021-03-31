       Subroutine Devprep(mdev,ipsize,ibplot)
*
*  ask user about plotter and paper and scale and border
*
       character*17 ipp(5)
       Dimension ip(11),pinch(5,2),icard(20),ipps(11,2)
       common /dimen/x1,x2,y1,y2,xln,yln,scale,fact,x0,y0
*
       data ip/7220,7470,7475,7550,7570,7580,7585,7586,7595,7596,9872/
       data ipp/'A ( 8.5" x 11.0")',
     1          'B (17.0" x 11.0")',
     2          'C (17.0" x 22.0")',
     3          'D (34.0" x 22.0")',
     4          'E (34.0" x 44.0")'/
c       data pinch/7.68,9.97,14.73,20.59,32.63,9.97,14.76,20.67,31.71,
c     1 41.83/
       data pinch/9.97,14.76,20.67,31.71,41.83,7.68,9.97,14.73,20.59,
     1 32.63/
       data ipps/2,1,1,1,3,1,1,1,1,1,2,2,1,2,2,4,4,5,5,5,5,2/
*
* ask plotter type
*
8      write(*,9)
9      format(/,1x,'Select H.P Plotter Type:')
       do 10 l=1,10
10     write(*,11)l,ip(l)
11     format(5x,i2,1x,i4,':')
       write(*,12)l,ip(l)
12     format(5x,i2,1x,i4,':',\)
       call readin(icard,5,is,ie)
       call fnum(icard,5,rdev,is)
       mdev=rdev
       if(mdev.ge.1.and.mdev.le.11)then
           l=mdev
           go to 25
       endif
*
       if(mdev.ge.7220.and.mdev.le.9872)then
        do 20 l=1,11
        if(mdev.eq.ip(l))go to 25
20      continue
       endif
*
*  Not found
*
       write(*,*)' Not able to understand plotter type - again please'
       go to 8
*
25     mdev=ip(l)
       il=l
*
*  ask paper size
*
24     write(*,26)
26     format(/,1x,'Select Paper Size:')
       do 27 l=ipps(il,1),ipps(il,2)-1
27     write(*,28)l,ipp(l)
       write(*,29)l,ipp(l)
28     format(5x,i1,1x,a17,':')
29     format(5x,i1,1x,a17,':',\)
       call readin(ipaper,1,is,ie)
       if(ipaper.ge.49.and.ipaper.le.53)then
        ipaper=ipaper-48
        go to 30
       endif
*
       if(ipaper.ge.65.and.ipaper.le.69)then
        ipaper=ipaper-64
        go to 30
       endif
*
* Not found
*
        write(*,*)' Paper size not understood - again please'
        go to 24
*
*  Subtract 15 to make paper size e=10 a=14 for hpinit.
*
30      ipsize=iabs(ipaper-15)
*
* Compute scale
*
       sc1=(x2-x1)*39.37/pinch(ipaper,1)
       sc2=(y2-y1)*39.37/pinch(ipaper,2)
*
* pick largest and round
*
       scale=sc1
       if(sc2.gt.sc1)then
        sc1=(x2-x1)*39.37/pinch(ipaper,2)
        scale=sc1
       endif
       scale=(aint(scale/1000.0)+1.0)*1000.0
*
* ask scale original plot digitized at:
*
       write(*,31)
31     format(1x,'At what scale was the map digitized ? [CR=24000]'\)
       call readin(icard,20,is,ie)
       call fnum(icard,20,oscale,is)
       if(oscale.eq.0.0)oscale=24000
*
*  compute a plot compression scale factor
*
       fact=oscale/scale
*
* ask if border is desired 
*
        write(*,32)
32      format(1x,/,
     1 ' Without a BORDER a Bar Scale and other annotation will not be',
     2 ' available. ',/,
     3 ' Without a BORDER the resultant map will be as large as',
     4 ' your plotter paper ',/,
     5 ' will allow.',/,
     5 ' Is a Border desired ? [CR]=Yes'\)
       call readin(ibplot,1,is,ie)
       if(ibplot.EQ.ichar('N'))go to 50
*
*  adjust current scale to fit within border with bar scales
*
      scx=(4.0*fact)/2.0
      scy=(3.0*fact)
*
      sc1=(x2-x1)*39.37/(pinch(ipaper,1)-scx)
      sc2=(y2-y1)*39.37/(pinch(ipaper,2)-scy)
*
*  again pick largest and round
*
      scale=sc1
      if(sc2.gt.sc1)then
       sc1=(x2-x1)*39.37/(pinch(ipaper,2)-scy)
       scale=sc1
      endif
      scale=(aint(scale/1000.0)+1.0)*1000.0
*
* compute new compression factor
*
      fact=oscale/scale
*
*  compute map size and tell user
*
50    xln=(x2-x1)*39.97/scale
      yln=(y2-y1)*39.37/scale
*
      write(*,35)xln,yln,scale
35    format(/1x,'Resultant map will be ',f5.2,' inches by ',f5.2,
     1 ' inches at a SCALE of 1:',f7.0,'.'/)
*
* done
*
      return
      end



