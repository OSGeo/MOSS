      BLOCKDATA pen
      COMMON /select/ imaps , isub(31) , iact(5,11)
      COMMON /pthkpp/ xnew1 , ynew1 , xnew2 , ynew2 , kpmap
      COMMON /dimen / x1 , y1 , x2 , y2 , xln , yln , scale , fact , 
     &                x0 , y0
      DATA iact/'AL' , 'L ' , '  ' , '  ' , '  ' , 'SU' , 'BJ' , 'EC' , 
     &     'TS' , '  ' , 'IT' , 'EM' , 'S ' , '  ' , '  ' , 'AT' , 
     &     'TR' , 'IB' , 'UT' , 'E ' , 'EL' , 'EV' , 'AT' , 'IO' , 
     &     'N ' , 'FR' , 'OM' , '  ' , '  ' , '  ' , 'SI' , 'ZE' , 
     &     '  ' , '  ' , '  ' , 'CO' , 'NT' , 'IG' , 'UI' , 'TY' , 
     &     'SA' , 'MP' , 'LE' , '  ' , '  ' , 'PI' , 'P ' , '  ' , 
     &     '  ' , '  ' , '??' , '??' , '??' , '??' , '? '/
C
 
C
      DATA kpmap/0/
      DATA fact/1.0/
      END
