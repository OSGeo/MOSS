      SUBROUTINE whttrm
c
c
c    to handle screen clearing, scrolling, and other minipulations
c     for the different terminals moss may run on.
c
c   kterm = type of terminal = 0 = 4010/4014 (non scrolling)
c                              1 = other  (scrolling)
c
c
      COMMON /termnl/ kterm
      RETURN
      END
