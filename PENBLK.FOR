        Block data pen
        COMMON /SELECT/ IMAPS,ISUB(31),IACT(5,11)
        common /pthkpp/xnew1,ynew1,xnew2,ynew2,kpmap
        common /dimen/x1,y1,x2,y2,xln,yln,scale,fact,x0,y0
        DATA IACT/2HAL,2HL ,2H  ,2H  ,2H  ,
     2            2HSU,2HBJ,2HEC,2HTS,2H  ,
     3            2HIT,2HEM,2HS ,2H  ,2H  ,
     4            2HAT,2HTR,2HIB,2HUT,2HE ,
     5            2HEL,2HEV,2HAT,2HIO,2HN ,
     6            2HFR,2HOM,2H  ,2H  ,2H  ,
     7            2HSI,2HZE,2H  ,2H  ,2H  ,
     8            2HCO,2HNT,2HIG,2HUI,2HTY,
     +            2HSA,2HMP,2HLE,2H  ,2H  ,
     +            2HPI,2HP ,2H  ,2H  ,2H  ,
     +            2H??,2H??,2H??,2H??,2H? /
C

C
       data kpmap/0/
       data fact/1.0/
      end
