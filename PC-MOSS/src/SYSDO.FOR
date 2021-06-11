       Subroutine SYSDO (Syscmd,systring,ierr)
C**************************************************************
C
C      S U B R O U T I N E   S Y S D O
C
C      To gather system (MS-DOS) information from the user for 
C      use in a C language SYSTEM Function.
C
C**************************************************************
$Include:'Global.inc'
       Character*6 ans
       Character*40 systring
       Character*8 FN
       Character*30 CopyStr
       character*80 syscmd
       integer fnlth,kk,jj,i,index,accum,j
       do 70 i=1,80
         if (syscmd(i:i) .ne. ' ') then
            do 60 j=i,80
              if (syscmd(j:j) .eq. ' ') then
                 index=j
                 goto 75
              endif
60          continue
            goto 79
         endif
70     continue
       goto 79
75     continue
       accum=0
77     accum=accum+1
       if (index+accum .gt. 80) goto 82
       if (accum .eq. 1 .and. syscmd(index+accum:index+accum) .eq. ' ')
     +     goto 79
       if (syscmd(index+accum:index+accum) .eq. ' ') goto 82
       ans(accum:accum)=syscmd(index+accum:index+accum)
       goto 77
C
C   Present system menu
C
79     Do 80 il=1,7
80     write(*,81)il,systuff(il)
81     format(1x,i1,1x,'-',1x,a6)
C
       write(*,'(1x,2h? ,\)')
       read(*,'(a6)')ans
82     continue
       call cnvupr(ans,6)
       if(ans(1:1).eq.' '.or.ans(1:1).eq.'1'.or.ans(1:2).eq.'AB')then
        ierr=1
        return
       elseif(ans(1:1).eq.'2'.or.ans(1:2).eq.'DI')then
         systring='DIR|SORT/+10|SORT|MORE'
         print *, ' ...One moment please...'
         return
       elseif(ans(1:1).eq.'3'.or. ans(1:2).eq.'CO') then
         if(syscmd(index+accum+1:index+accum+1) .ne. ' ') then
           copystr=syscmd(index+accum+1:index+accum+30)
           goto 100
         endif
         write(*,*)'Enter COPY string - no checking is done'
         read(*,'(a30)')copystr
 100      if(copystr(1:1).eq.' ')then
            ierr=1
            return
          else
            systring='COPY '//copystr
            return
          endif
        elseif(ans(1:1).eq.'4'.or.ans(1:2).eq.'PR')then
          if(syscmd(index+accum+1:index+accum+1) .ne. ' ') then
            fn=syscmd(index+accum+1:index+accum+8)
            goto 160
          endif
          write(*,*)'Enter Filename to PRINT'
          read(*,'(a8)')fn
 160        if(fn(1:1).eq.' ')then
               ierr=1
               return
            else
               systring='PRINT '//fn
               return
            endif
        elseif(ans(1:1).eq.'5'.or.ans(1:2).eq.'PL')then
          if(syscmd(index+accum+1:index+accum+1) .ne. ' ') then
            do 200 kk=1,9
              if (syscmd(index+accum+kk:index+accum+kk).eq.' ')
     +            goto 205
200         continue
            write(*,*)'Filename must be Less than 8 Char!!'
            goto 206
205         fnlth=kk-1
            fn=syscmd(index+accum+1:index+accum+fnlth+1)
            goto 210
          endif
          fnlth=-1
206       write(*,*)'Enter Filename to PLOT'
          read(*,'(a8)')fn
210         if(fn(1:1).eq.' ')then
                ierr=1
                return
            else
                if(syscmd(index+accum+fnlth+3:index+accum+fnlth+3)
     +             .ne.' ') then
                  ans=syscmd(index+accum+fnlth+3:index+accum+fnlth+8)
                  goto 230
                endif
220             write(*,*)' Indicate Output Port: 1, 2, or 3'
                write(*,*)'(1)  COM1:'
                write(*,*)'(2)  COM2:'
                write(*,*)'(3)  COM3:'
                write(*,'(1x,2h? ,\)')
                read(*,'(a6)')ans
230             if (ans(1:1).eq.' ') then
                  ierr=1
                  return
                endif
                if(ans(1:1).gt.'0'.and.ans(1:1).lt.'4')then
                  systring='\gis\intrface\plotit '//fn//ans(1:1)
                else
                  write(*,*)'1, 2, or 3 please'
                  go to 220
                endif
               return
            endif
          else if(ans(1:1).eq.'7'.or.ans(1:2).eq.'CD')then
            if(syscmd(index+accum+1:index+accum+1).ne. ' ') then
               copystr=syscmd(index+accum+1:index+accum+30)
               goto 300
            endif
            write(*,*)'Enter Directory Name (must exist)'
            read(*,'(a30)')copystr
300          if(copystr(1:1).eq.' ')then
               ierr=1
               return
             else
               systring='CD \'//copystr
             endif
           else if(ans(1:1).eq.'6'.or.ans(1:2).eq.'CH')then
              systring='CHKDSK'
              return
           endif
        return
        end
