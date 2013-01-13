!
!  Simple test program for sacio90 library.
!
   program test_sacio90
   use sacio90  ! use the sacio90 module
      implicit none
      type (SAC1) :: t1,t2,t3,tc
      character (len=80) :: fn

      ! create a new trace, set some headers.

      fn = 'TEST.SAC' ;
      call sacio90_new(101,0.05,tc)
      
      ! put a spike in the middle position.
      tc % y(51) = 1.0
      tc % evla = 30.0
      call sacio90_write(fn,tc)

      ! check we can read it. 
      call sacio90_read(fn,t1)
      print*,'evla',t1%evla
            
      
      print*,tc % y(51),tc % npts
            
   end program test_sacio90
