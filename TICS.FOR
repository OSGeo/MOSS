      SUBROUTINE tics(xt1,yt1,xt2,yt2)
C
      COMMON /dimen / x1 , x2 , y1 , y2 , xln , yln , scale , fact , 
     &                x0 , y0
C
C  DRAW CORNER TICS ON PLOT
C
C       MODIFIED BY MIKE FIEBACH, TGS/BLM/DSC FEB/86
C       (FIX BUG TO ALLOW ROTATION OF TIC MARKS)
C
      CALL cptrns(xt1,xc,xt1,scale)
      CALL cptrns(yt1,yc,yt1,scale)
      CALL cptrns(xt2,xcc,xt1,scale)
      CALL cptrns(yt2,ycc,yt1,scale)
C.....DRAW FIRST CORNER
      idev = 2
      xca = xc
      yca = yc
      off2 = .2*fact
      size = .07*fact
      off56 = .56*fact
      CALL hpnumber(xca,yca-off2,size,xt1,0.0,-1)
      CALL hpnumber(xca-off2,yca,size,yt1,90.0,-1)
      CALL pendv(xc,yc,3,idev)
      CALL pendv(xc+off2,yc,2,idev)
      CALL pendv(xc,yc,3,idev)
      CALL pendv(xc,yc+off2,2,idev)
C.....DRAW SECOND CORNER
      xca = xc
      ycca = ycc
      CALL hpnumber(xca,ycca+off2,size,xt1,0.0,-1)
      CALL hpnumber(xca-off2,ycca-off56,size,yt2,90.0,-1)
      CALL pendv(xc,ycc,3,idev)
      CALL pendv(xc+off2,ycc,2,idev)
      CALL pendv(xc,ycc,3,idev)
      CALL pendv(xc,ycc-off2,2,idev)
C.....DRAW THIRD CORNER
      xcca = xcc
      ycca = ycc
      CALL hpnumber(xcca-off56,ycca+off2,size,xt2,0.0,-1)
      CALL hpnumber(xcca+off2,ycca,size,yt2,-90.0,-1)
      CALL pendv(xcc,ycc,3,idev)
      CALL pendv(xcc-off2,ycc,2,idev)
      CALL pendv(xcc,ycc,3,idev)
      CALL pendv(xcc,ycc-off2,2,idev)
C.....DRAW FORTH CORNER
      xcca = xcc
      yca = yc
      CALL hpnumber(xcca-off56,yca-off2,size,xt2,0.0,-1)
      CALL hpnumber(xcca+off2,yca+off56,size,yt1,-90.0,-1)
      CALL pendv(xcc,yc,3,idev)
      CALL pendv(xcc-off2,yc,2,idev)
      CALL pendv(xcc,yc,3,idev)
      CALL pendv(xcc,yc+off2,2,idev)
      RETURN
      END
