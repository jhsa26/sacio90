!
!  Simple test program for sacio90 library
!
   program test_sacio90
   use sacio90  ! use the sacio90 module
      implicit none
      type (SAC1) :: t1,t2,t3,tc
      character (len=80) :: fn
       

      fn = 'TEST.SAC' ;
      call sacio90_new(101,0.05,tc)
      
      tc % y(51) = 1.0
      tc % evla = 30.0
      call sacio90_write(fn,tc)

      
!
!     ** now check that we can edit the header
!      
!      tc % evla = 60.
!      call sacio90_writeheader(fn,tc)
      
!  ** delete the trace
!      call sacio90_deletetrace(tc)
      
            
   end program test_sacio90
