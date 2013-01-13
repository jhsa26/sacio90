!===============================================================================
!-------------------------------------------------------------------------------
!
!  Fortran 90/95 Source Code File
!
!-------------------------------------------------------------------------------
!===============================================================================
!
!  PROGRAM : sacio90
!  VERSION : 1.0
!
!  (C) James Wookey
!  Department of Earth Sciences, University of Bristol
!  Wills Memorial Building, Queen's Road, Bristol, BR8 1RJ, UK
!  j.wookey@bristol.ac.uk
!
!-------------------------------------------------------------------------------
!
!   The module provides data structures and functions for reading,
!   writing and SAC files in Fortran 90/95. It consists of a wrapper around 
!   calls to the distributed sacio library. 
!
!   Please report bugs/problems to email address above
!
!   NOTE: This version of the code assumes IO filestream 99 is available 
!         for reading and writing. 
!
!-------------------------------------------------------------------------------
!
!  This software is distributed under the term of the BSD free software license.
!
!  Copyright:
!     (c) 2003-2011, James Wookey
!
!  All rights reserved.
!
!   * Redistribution and use in source and binary forms, with or without
!     modification, are permitted provided that the following conditions are
!     met:
!        
!   * Redistributions of source code must retain the above copyright notice,
!     this list of conditions and the following disclaimer.
!        
!   * Redistributions in binary form must reproduce the above copyright
!     notice, this list of conditions and the following disclaimer in the
!     documentation and/or other materials provided with the distribution.
!     
!   * Neither the name of the copyright holder nor the names of its
!     contributors may be used to endorse or promote products derived from
!     this software without specific prior written permission.
!
!
!   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
!   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
!   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
!   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
!   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
!   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
!   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
!   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
!   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
!   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
!   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
!-------------------------------------------------------------------------------
!     Changes log
!-------------------------------------------------------------------------------
!
!     2013-01-11  v0.1   * Incept date
!
!===============================================================================

   module sacio90 ! Utility module for F90/95 for SAC files

!===============================================================================
   implicit none

!  ** DECLARE CONTAINED FUNCTIONS

      public :: sacio90_new
      public :: sacio90_clone

      public :: sacio90_delete

      public :: sacio90_read
      public :: sacio90_write

      public :: sacio90_copyhdr

                
!  ** define a long (32 bit) integer and 32 bit real      
      integer, parameter, private :: int4 = selected_int_kind(9) ;
      integer, parameter, private :: real4 = selected_real_kind(6,37) ;
      integer, parameter, private :: real8 = selected_real_kind(15,307) ; 

      
!  ** define the unit number to use for reading and writing (opened and closed
!  ** within each call)
      integer, parameter :: sacio90_iounit = 99 ;

!  ** OPTIONAL suppression of warnings, set to 1 to supress      
#ifdef SUPPRESS_WARNINGS
      integer, parameter :: sacio90_suppress_warnings = 1
#else      
      integer, parameter :: sacio90_suppress_warnings = 0 
#endif      

!  ** noise generator seed value
      integer, private :: sacio90_random_seed ;      

!  ** standard filename length
      integer, parameter :: sacio90_fnlength = 256 ;      

!=============================================================================== 
!  ** Define a specialised data structure for containing SAC files
!=============================================================================== 
      type SAC1
!     ** Header floating point part
         real(real4) :: delta,depmin,depmax,scale,odelta,b,e,o,a
         real(real4) :: t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,f
         real(real4) :: resp0,resp1,resp2,resp3,resp4,resp5
         real(real4) :: resp6,resp7,resp8,resp9
         real(real4) :: stla,stlo,stel,stdp,evla,evlo,evel,evdp,mag
         real(real4) :: user0,user1,user2,user3,user4
         real(real4) :: user5,user6,user7,user8,user9
         real(real4) :: dist,az,baz,gcarc,depmen
         real(real4) :: cmpaz,cmpinc
         real(real4) :: xminimum,xmaximum,yminimum,ymaximum
!     ** Header integer part
         integer(int4) :: nzyear,nzjday,nzhour,nzmin,nzsec,nzmsec
         integer(int4) :: nvhdr,norid,nevid,npts
         integer(int4) :: internal3,nwfid,nxsize,nysize,unused8
         integer(int4) :: iftype,idep,iztype,unused9
         integer(int4) :: iinst,istreg,ievreg,ievtyp
         integer(int4) :: iqual,isynth,imagtyp,imagsrc    
!     ** Header logical part (stored as integers)
         integer(int4) ::  leven,lpspol,lovrok,lcalda
!     ** Header character part
         character (len = 16) :: kevnm
         character (len = 8) :: kstnm,khole,ko,ka
         character (len = 8) :: kt0,kt1,kt2,kt3,kt4
         character (len = 8) :: kt5,kt6,kt7,kt8,kt9
         character (len = 8) :: kf,kuser0,kuser1,kuser2
         character (len = 8) :: kcmpnm,knetwk,kdatrd,kinst
!     ** the trace
         real(real4), allocatable :: y(:)
                  
      end type SAC1

!=============================================================================== 
!  ** Define a data structure for containing SAC xy files
!=============================================================================== 
      type SAC2
!     ** Header floating point part
         real(real4) :: delta,depmin,depmax,scale,odelta,b,e,o,a
         real(real4) :: t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,f
         real(real4) :: resp0,resp1,resp2,resp3,resp4,resp5
         real(real4) :: resp6,resp7,resp8,resp9
         real(real4) :: stla,stlo,stel,stdp,evla,evlo,evel,evdp,mag
         real(real4) :: user0,user1,user2,user3,user4
         real(real4) :: user5,user6,user7,user8,user9
         real(real4) :: dist,az,baz,gcarc,depmen
         real(real4) :: cmpaz,cmpinc
         real(real4) :: xminimum,xmaximum,yminimum,ymaximum
!     ** Header integer part
         integer(int4) :: nzyear,nzjday,nzhour,nzmin,nzsec,nzmsec
         integer(int4) :: nvhdr,norid,nevid,npts
         integer(int4) :: internal3,nwfid,nxsize,nysize,unused8
         integer(int4) :: iftype,idep,iztype,unused9
         integer(int4) :: iinst,istreg,ievreg,ievtyp
         integer(int4) :: iqual,isynth,imagtyp,imagsrc 
!     ** Header logical part (stored as integers)
         integer(int4) ::  leven,lpspol,lovrok,lcalda
!     ** Header character part
         character (len = 16) :: kevnm
         character (len = 8) :: kstnm,khole,ko,ka
         character (len = 8) :: kt0,kt1,kt2,kt3,kt4
         character (len = 8) :: kt5,kt6,kt7,kt8,kt9
         character (len = 8) :: kf,kuser0,kuser1,kuser2
         character (len = 8) :: kcmpnm,knetwk,kdatrd,kinst

         real(real4), allocatable :: x(:)
         real(real4), allocatable :: y(:)
      end type SAC2    

!  ** NULL values set in SAC objects      
      real, parameter :: SAC_rnull = -12345.0
      integer, parameter :: SAC_inull = -12345
      integer, parameter :: SAC_lnull = -12345
      character (len = 8) :: SAC_cnull = '-12345'

      
!===============================================================================
!
!  ** MODULE SUBROUTINES
!
!===============================================================================

   CONTAINS

!===============================================================================
   subroutine sacio90_delete(tr)
!===============================================================================
!
!     Delete a trace: null out headers and deallocate the memory
!
      implicit none
      type (SAC1) :: tr

tr%delta     = 0.0       ; tr%resp3     = SAC_rnull ; tr%user8     = SAC_rnull
tr%depmin    = SAC_rnull ; tr%resp4     = SAC_rnull ; tr%user9     = SAC_rnull
tr%depmax    = SAC_rnull ; tr%resp5     = SAC_rnull ; tr%dist      = SAC_rnull
tr%scale     = SAC_rnull ; tr%resp6     = SAC_rnull ; tr%az        = SAC_rnull
tr%odelta    = SAC_rnull ; tr%resp7     = SAC_rnull ; tr%baz       = SAC_rnull
tr%b         = 0.0       ; tr%resp8     = SAC_rnull ; tr%gcarc     = SAC_rnull
tr%e         = 0.0       ; tr%resp9     = SAC_rnull ; 
tr%o         = SAC_rnull ; tr%stla      = SAC_rnull ; 
tr%a         = SAC_rnull ; tr%stlo      = SAC_rnull ; tr%depmen    = SAC_rnull
                           tr%stel      = SAC_rnull ; tr%cmpaz     = SAC_rnull
tr%t0        = SAC_rnull ; tr%stdp      = SAC_rnull ; tr%cmpinc    = SAC_rnull
tr%t1        = SAC_rnull ; tr%evla      = SAC_rnull ; tr%xminimum  = SAC_rnull
tr%t2        = SAC_rnull ; tr%evlo      = SAC_rnull ; tr%xmaximum  = SAC_rnull
tr%t3        = SAC_rnull ; tr%evel      = SAC_rnull ; tr%yminimum  = SAC_rnull
tr%t4        = SAC_rnull ; tr%evdp      = SAC_rnull ; tr%ymaximum  = SAC_rnull
tr%t5        = SAC_rnull ; tr%mag       = SAC_rnull ; 
tr%t6        = SAC_rnull ; tr%user0     = SAC_rnull ; 
tr%t7        = SAC_rnull ; tr%user1     = SAC_rnull ; 
tr%t8        = SAC_rnull ; tr%user2     = SAC_rnull ; 
tr%t9        = SAC_rnull ; tr%user3     = SAC_rnull ; 
tr%f         = SAC_rnull ; tr%user4     = SAC_rnull ; 
tr%resp0     = SAC_rnull ; tr%user5     = SAC_rnull ; 
tr%resp1     = SAC_rnull ; tr%user6     = SAC_rnull ; 
tr%resp2     = SAC_rnull ; tr%user7     = SAC_rnull ; 
                           
tr%nzyear    = SAC_inull ; tr%unused8   = SAC_inull ; 
tr%nzjday    = SAC_inull ; tr%iftype    = 1         ; 
tr%nzhour    = SAC_inull ; tr%idep      = 5         ; 
tr%nzmin     = SAC_inull ; tr%iztype    = 9         ; 
tr%nzsec     = SAC_inull ; tr%unused9   = SAC_inull ; 
tr%nzmsec    = SAC_inull ; tr%iinst     = SAC_inull ; 
tr%nvhdr     = 6         ; tr%istreg    = SAC_inull ; 
tr%norid     = SAC_inull ; tr%ievreg    = SAC_inull ; tr%leven     = 1   
tr%nevid     = SAC_inull ; tr%ievtyp    = 5         ; tr%lpspol    = 0
tr%npts      = 0         ; tr%iqual     = SAC_inull ; tr%lovrok    = 1
                           tr%isynth    = SAC_inull ; tr%lcalda    = 1
tr%nwfid     = SAC_inull ; tr%imagtyp   = SAC_inull ; 
tr%nxsize    = SAC_inull ; tr%imagsrc   = SAC_inull
tr%nysize    = SAC_inull ; 

tr%kstnm = SAC_cnull ; tr%kt3 = SAC_cnull ; tr%kuser0  = SAC_cnull
tr%kevnm = SAC_cnull ; tr%kt4 = SAC_cnull ; tr%kuser1  = SAC_cnull
tr%khole = SAC_cnull ; tr%kt5 = SAC_cnull ; tr%kuser2  = SAC_cnull
tr%ko    = SAC_cnull ; tr%kt6 = SAC_cnull ; tr%kcmpnm  = SAC_cnull
tr%ka    = SAC_cnull ; tr%kt7 = SAC_cnull ; tr%knetwk  = SAC_cnull
tr%kt0   = SAC_cnull ; tr%kt8 = SAC_cnull ; tr%kdatrd  = SAC_cnull
tr%kt1   = SAC_cnull ; tr%kt9 = SAC_cnull ; tr%kinst   = SAC_cnull
tr%kt2   = SAC_cnull ; tr%kf  = SAC_cnull ; 
      
      if (allocated(tr%y)) then
         deallocate(tr%y)
      endif 
      
      return
      end subroutine sacio90_delete
!===============================================================================

!===============================================================================
   subroutine sacio90_new(nsamp,delta,tr)
!===============================================================================
      implicit none
      type (SAC1) :: tr
      integer :: nsamp ! number of samples for trace 
      real :: delta
      
      tr%delta     = delta
      tr%depmin    = SAC_rnull
      tr%depmax    = SAC_rnull
      tr%scale     = SAC_rnull
      tr%odelta    = SAC_rnull
      tr%b         = 0.0
      tr%e         = real(nsamp-1)*delta
      tr%o         = SAC_rnull
      tr%a         = SAC_rnull
      tr%t0        = SAC_rnull
      tr%t1        = SAC_rnull
      tr%t2        = SAC_rnull
      tr%t3        = SAC_rnull
      tr%t4        = SAC_rnull
      tr%t5        = SAC_rnull
      tr%t6        = SAC_rnull
      tr%t7        = SAC_rnull
      tr%t8        = SAC_rnull
      tr%t9        = SAC_rnull
      tr%f         = SAC_rnull
      tr%resp0     = SAC_rnull
      tr%resp1     = SAC_rnull
      tr%resp2     = SAC_rnull
      tr%resp3     = SAC_rnull
      tr%resp4     = SAC_rnull
      tr%resp5     = SAC_rnull
      tr%resp6     = SAC_rnull
      tr%resp7     = SAC_rnull
      tr%resp8     = SAC_rnull
      tr%resp9     = SAC_rnull
      tr%stla      = SAC_rnull
      tr%stlo      = SAC_rnull
      tr%stel      = SAC_rnull
      tr%stdp      = SAC_rnull
      tr%evla      = SAC_rnull
      tr%evlo      = SAC_rnull
      tr%evel      = SAC_rnull
      tr%evdp      = SAC_rnull
      tr%mag       = SAC_rnull
      tr%user0     = SAC_rnull
      tr%user1     = SAC_rnull
      tr%user2     = SAC_rnull
      tr%user3     = SAC_rnull
      tr%user4     = SAC_rnull
      tr%user5     = SAC_rnull
      tr%user6     = SAC_rnull
      tr%user7     = SAC_rnull
      tr%user8     = SAC_rnull
      tr%user9     = SAC_rnull
      tr%dist      = SAC_rnull
      tr%az        = SAC_rnull
      tr%baz       = SAC_rnull
      tr%gcarc     = SAC_rnull
      tr%depmen    = SAC_rnull
      tr%cmpaz     = SAC_rnull
      tr%cmpinc    = SAC_rnull
      tr%xminimum  = SAC_rnull
      tr%xmaximum  = SAC_rnull
      tr%yminimum  = SAC_rnull
      tr%ymaximum  = SAC_rnull

      tr%nzyear    = SAC_inull
      tr%nzjday    = SAC_inull
      tr%nzhour    = SAC_inull
      tr%nzmin     = SAC_inull
      tr%nzsec     = SAC_inull
      tr%nzmsec    = SAC_inull
      tr%nvhdr     = 6 ! default
      tr%norid     = SAC_inull
      tr%nevid     = SAC_inull
      tr%npts      = nsamp ! number of samples
      tr%nwfid     = SAC_inull
      tr%nxsize    = SAC_inull
      tr%nysize    = SAC_inull
      tr%unused8   = SAC_inull
      tr%iftype    = 1 ! default (time series file)
      tr%idep      = 5 ! default
      tr%iztype    = 9 ! default
      tr%iinst     = SAC_inull
      tr%istreg    = SAC_inull
      tr%ievreg    = SAC_inull
      tr%ievtyp    = 5 ! default
      tr%iqual     = SAC_inull
      tr%isynth    = SAC_inull
      tr%imagtyp   = SAC_inull
      tr%imagsrc   = SAC_inull
      tr%leven     = 1 ! default
      tr%lpspol    = 0
      tr%lovrok    = 1
      tr%lcalda    = 1

      tr%kstnm   = SAC_cnull
      tr%kevnm   = SAC_cnull
      tr%khole   = SAC_cnull
      tr%ko      = SAC_cnull
      tr%ka      = SAC_cnull
      tr%kt0     = SAC_cnull
      tr%kt1     = SAC_cnull
      tr%kt2     = SAC_cnull
      tr%kt3     = SAC_cnull
      tr%kt4     = SAC_cnull
      tr%kt5     = SAC_cnull
      tr%kt6     = SAC_cnull
      tr%kt7     = SAC_cnull
      tr%kt8     = SAC_cnull
      tr%kt9     = SAC_cnull
      tr%kf      = SAC_cnull
      tr%kuser0  = SAC_cnull
      tr%kuser1  = SAC_cnull
      tr%kuser2  = SAC_cnull
      tr%kcmpnm  = SAC_cnull
      tr%knetwk  = SAC_cnull
      tr%kdatrd  = SAC_cnull
      tr%kinst   = SAC_cnull
 
!  ** allocate memory for the trace
      call sacio90_malloc(tr%y,tr%npts)
   
      tr % y(1:tr % npts) = 0.0
      

   end subroutine sacio90_new
!===============================================================================
   
!===============================================================================
   subroutine sacio90_malloc(x,n)
!===============================================================================
!
!     Allocate memory to an array, if required
!
      implicit none
      real(real4),allocatable :: x(:)
      integer :: n

      if (allocated(x)) then
         if (size(x)<n) then
!        ** reallocation required
!            print*,'reallocating memory'
            deallocate(x)
            allocate(x(n))
         endif   
      else
            allocate(x(n))
      endif   
      
   end subroutine sacio90_malloc
!===============================================================================
      
!===============================================================================
   subroutine sacio90_clone(target,clone)
!===============================================================================
!
!     Create a new, identical trace to the target
!
! input: target   SACtrace    target SAC trace object
! output:clone      SACTrace    clone SAC trace object
!
      implicit none
      type (SAC1) :: target,clone

!  ** make a new trace object     
      call sacio90_new(target % npts, target % delta, clone)
!  ** duplicate target header
      call sacio90_copyhdr(target,clone) 
!  ** copy data   
      clone % y(1:clone % npts) = target % y(1:target % npts)
      
   end subroutine sacio90_clone
!===============================================================================

!===============================================================================
   subroutine sacio90_copyhdr(source,dest)
!===============================================================================
! input: source   SACtrace    source SAC trace object
! output:dest      SACTrace    destination SAC trace object
!-------------------------------------------------------------------------------
!  modifications:
!     24-03-03    J. Wookey      Modified to use F90 constructs
!-------------------------------------------------------------------------------
      implicit none
      type (SAC1) :: source,dest

      dest%delta     =  source%delta     
      dest%depmin    =  source%depmin    
      dest%depmax    =  source%depmax    
      dest%scale     =  source%scale     
      dest%odelta    =  source%odelta    
      dest%b         =  source%b         
      dest%e         =  source%e         
      dest%o         =  source%o         
      dest%a         =  source%a         
      dest%t0        =  source%t0        
      dest%t1        =  source%t1        
      dest%t2        =  source%t2        
      dest%t3        =  source%t3        
      dest%t4        =  source%t4        
      dest%t5        =  source%t5        
      dest%t6        =  source%t6        
      dest%t7        =  source%t7        
      dest%t8        =  source%t8        
      dest%t9        =  source%t9        
      dest%f         =  source%f         
      dest%resp0     =  source%resp0     
      dest%resp1     =  source%resp1     
      dest%resp2     =  source%resp2     
      dest%resp3     =  source%resp3     
      dest%resp4     =  source%resp4     
      dest%resp5     =  source%resp5     
      dest%resp6     =  source%resp6     
      dest%resp7     =  source%resp7     
      dest%resp8     =  source%resp8     
      dest%resp9     =  source%resp9     
      dest%stla      =  source%stla      
      dest%stlo      =  source%stlo      
      dest%stel      =  source%stel      
      dest%stdp      =  source%stdp      
      dest%evla      =  source%evla      
      dest%evlo      =  source%evlo      
      dest%evel      =  source%evel      
      dest%evdp      =  source%evdp      
      dest%mag       =  source%mag       
      dest%user0     =  source%user0     
      dest%user1     =  source%user1     
      dest%user2     =  source%user2     
      dest%user3     =  source%user3     
      dest%user4     =  source%user4     
      dest%user5     =  source%user5     
      dest%user6     =  source%user6     
      dest%user7     =  source%user7     
      dest%user8     =  source%user8     
      dest%user9     =  source%user9     
      dest%dist      =  source%dist      
      dest%az        =  source%az        
      dest%baz       =  source%baz       
      dest%gcarc     =  source%gcarc     
      dest%depmen    =  source%depmen    
      dest%cmpaz     =  source%cmpaz     
      dest%cmpinc    =  source%cmpinc    
      dest%xminimum  =  source%xminimum  
      dest%xmaximum  =  source%xmaximum  
      dest%yminimum  =  source%yminimum  
      dest%ymaximum  =  source%ymaximum  

      dest%nzyear    =  source%nzyear    
      dest%nzjday    =  source%nzjday    
      dest%nzhour    =  source%nzhour    
      dest%nzmin     =  source%nzmin     
      dest%nzsec     =  source%nzsec     
      dest%nzmsec    =  source%nzmsec    
      dest%nvhdr     =  source%nvhdr     
      dest%norid     =  source%norid     
      dest%nevid     =  source%nevid     
      dest%npts      =  source%npts      
      dest%nwfid     =  source%nwfid     
      dest%nxsize    =  source%nxsize    
      dest%nysize    =  source%nysize    
      dest%iftype    =  source%iftype    
      dest%idep      =  source%idep      
      dest%iztype    =  source%iztype    
      dest%unused9   =  source%unused9   
      dest%iinst     =  source%iinst     
      dest%istreg    =  source%istreg    
      dest%ievreg    =  source%ievreg    
      dest%ievtyp    =  source%ievtyp    
      dest%iqual     =  source%iqual     
      dest%isynth    =  source%isynth    
      dest%imagtyp   =  source%imagtyp   
      dest%imagsrc   =  source%imagsrc   
      dest%leven     =  source%leven     
      dest%lpspol    =  source%lpspol    
      dest%lovrok    =  source%lovrok    
      dest%lcalda    =  source%lcalda    

      dest%kstnm     =  source%kstnm     
      dest%kevnm     =  source%kevnm     
      dest%khole     =  source%khole     
      dest%ko        =  source%ko        
      dest%ka        =  source%ka        
      dest%kt0       =  source%kt0       
      dest%kt1       =  source%kt1       
      dest%kt2       =  source%kt2       
      dest%kt3       =  source%kt3       
      dest%kt4       =  source%kt4       
      dest%kt5       =  source%kt5       
      dest%kt6       =  source%kt6       
      dest%kt7       =  source%kt7       
      dest%kt8       =  source%kt8       
      dest%kt9       =  source%kt9       
      dest%kf        =  source%kf        
      dest%kuser0    =  source%kuser0    
      dest%kuser1    =  source%kuser1    
      dest%kuser2    =  source%kuser2    
      dest%kcmpnm    =  source%kcmpnm    
      dest%knetwk    =  source%knetwk    
      dest%kdatrd    =  source%kdatrd    
      dest%kinst     =  source%kinst     

   end subroutine sacio90_copyhdr
!===============================================================================

!===============================================================================
   subroutine sacio90_write(fname,tr)
!===============================================================================
!
!  write a SAC time-series object to a file   
!
      implicit none
      character (len=*):: fname
      type (SAC1) :: tr
      integer :: nerr
      
!  ** create a new SAC file      
      call newhdr()
      
!!  ** upload the SAC header
      call setfhv('delta',tr%delta,nerr)
      call setfhv('depmin',tr%depmin,nerr)
      call setfhv('depmax',tr%depmax,nerr)
      call setfhv('scale',tr%scale,nerr)
      call setfhv('odelta',tr%odelta,nerr)
      call setfhv('b',tr%b,nerr)
      call setfhv('e',tr%e,nerr)
      call setfhv('o',tr%o,nerr)
      call setfhv('a',tr%a,nerr)
      call setfhv('t0',tr%t0,nerr)
      call setfhv('t1',tr%t1,nerr)
      call setfhv('t2',tr%t2,nerr)
      call setfhv('t3',tr%t3,nerr)
      call setfhv('t4',tr%t4,nerr)
      call setfhv('t5',tr%t5,nerr)
      call setfhv('t6',tr%t6,nerr)
      call setfhv('t7',tr%t7,nerr)
      call setfhv('t8',tr%t8,nerr)
      call setfhv('t9',tr%t9,nerr)
      call setfhv('f',tr%f,nerr)
      call setfhv('resp0',tr%resp0,nerr)
      call setfhv('resp1',tr%resp1,nerr)
      call setfhv('resp2',tr%resp2,nerr)
      call setfhv('resp3',tr%resp3,nerr)
      call setfhv('resp4',tr%resp4,nerr)
      call setfhv('resp5',tr%resp5,nerr)
      call setfhv('resp6',tr%resp6,nerr)
      call setfhv('resp7',tr%resp7,nerr)
      call setfhv('resp8',tr%resp8,nerr)
      call setfhv('resp9',tr%resp9,nerr)
      call setfhv('stla',tr%stla,nerr)
      call setfhv('stlo',tr%stlo,nerr)
      call setfhv('stel',tr%stel,nerr)
      call setfhv('stdp',tr%stdp,nerr)
      call setfhv('evla',tr%evla,nerr)
      call setfhv('evlo',tr%evlo,nerr)
      call setfhv('evel',tr%evel,nerr)
      call setfhv('evdp',tr%evdp,nerr)
      call setfhv('mag',tr%mag,nerr)
      call setfhv('user0',tr%user0,nerr)
      call setfhv('user1',tr%user1,nerr)
      call setfhv('user2',tr%user2,nerr)
      call setfhv('user3',tr%user3,nerr)
      call setfhv('user4',tr%user4,nerr)
      call setfhv('user5',tr%user5,nerr)
      call setfhv('user6',tr%user6,nerr)
      call setfhv('user7',tr%user7,nerr)
      call setfhv('user8',tr%user8,nerr)
      call setfhv('user9',tr%user9,nerr)
      call setfhv('dist',tr%dist,nerr)
      call setfhv('az',tr%az,nerr)
      call setfhv('baz',tr%baz,nerr)
      call setfhv('gcarc',tr%gcarc,nerr)
      call setfhv('depmen',tr%depmen,nerr)
      call setfhv('cmpaz',tr%cmpaz,nerr)
      call setfhv('cmpinc',tr%cmpinc,nerr)
      call setfhv('xminimum',tr%xminimum,nerr)
      call setfhv('xmaximum',tr%xmaximum,nerr)
      call setfhv('yminimum',tr%yminimum,nerr)
      call setfhv('ymaximum',tr%ymaximum,nerr)
      call setnhv('nzyear',tr%nzyear,nerr)
      call setnhv('nzjday',tr%nzjday,nerr)
      call setnhv('nzhour',tr%nzhour,nerr)
      call setnhv('nzmin',tr%nzmin,nerr)
      call setnhv('nzsec',tr%nzsec,nerr)
      call setnhv('nzmsec',tr%nzmsec,nerr)
      call setnhv('nvhdr',tr%nvhdr,nerr)
      call setnhv('norid',tr%norid,nerr)
      call setnhv('nevid',tr%nevid,nerr)
      call setnhv('npts',tr%npts,nerr)
      call setnhv('nwfid',tr%nwfid,nerr)
      call setnhv('nxsize',tr%nxsize,nerr)
      call setnhv('nysize',tr%nysize,nerr)
      call setnhv('unused8',tr%unused8,nerr)
      call setnhv('iftype',tr%iftype,nerr)
      call setnhv('idep',tr%idep,nerr)
      call setnhv('iztype',tr%iztype,nerr)
      call setnhv('unused9',tr%unused9,nerr)
      call setnhv('iinst',tr%iinst,nerr)
      call setnhv('istreg',tr%istreg,nerr)
      call setnhv('ievreg',tr%ievreg,nerr)
      call setnhv('ievtyp',tr%ievtyp,nerr)
      call setnhv('iqual',tr%iqual,nerr)
      call setnhv('isynth',tr%isynth,nerr)
      call setnhv('imagtyp',tr%imagtyp,nerr)
      call setnhv('imagsrc',tr%imagsrc,nerr)
      call setlhv('even',tr%leven,nerr)
      call setlhv('lpspol',tr%lpspol,nerr)
      call setlhv('lovrok',tr%lovrok,nerr)
      call setlhv('lcalda',tr%lcalda,nerr)
      call setkhv('kstnm',tr%kstnm(1:8),nerr)
      call setkhv('kevnm',tr%kevnm(1:16),nerr)
      call setkhv('khole',tr%khole(1:8),nerr)
      call setkhv('ko',tr%ko(1:8),nerr)
      call setkhv('ka',tr%ka(1:8),nerr)
      call setkhv('kt0',tr%kt0(1:8),nerr)
      call setkhv('kt1',tr%kt1(1:8),nerr)
      call setkhv('kt2',tr%kt2(1:8),nerr)
      call setkhv('kt3',tr%kt3(1:8),nerr)
      call setkhv('kt4',tr%kt4(1:8),nerr)
      call setkhv('kt5',tr%kt5(1:8),nerr)
      call setkhv('kt6',tr%kt6(1:8),nerr)
      call setkhv('kt7',tr%kt7(1:8),nerr)
      call setkhv('kt8',tr%kt8(1:8),nerr)
      call setkhv('kt9',tr%kt9(1:8),nerr)
      call setkhv('kf',tr%kf(1:8),nerr)
      call setkhv('kuser0',tr%kuser0(1:8),nerr)
      call setkhv('kuser1',tr%kuser1(1:8),nerr)
      call setkhv('kuser2',tr%kuser2(1:8),nerr)
      call setkhv('kcmpnm',tr%kcmpnm(1:8),nerr)
      call setkhv('knetwk',tr%knetwk(1:8),nerr)
      call setkhv('kdatrd',tr%kdatrd(1:8),nerr)
      call setkhv('kinst',tr%kinst(1:8),nerr)

!  ** Output the trace   
      call wsac0(fname,tr%y,tr%y,nerr)
 
   end subroutine sacio90_write
!===============================================================================

!===============================================================================
   subroutine sacio90_read(fname, tr)
!===============================================================================
!
!  read a SAC time-series object from a file, using the SACIO library.
!
   implicit none
      character (len=*) :: fname
      integer :: i, istatus, ndummy, npts, nerr
      real :: rdummy,beg,delta
      
      type (SAC1) :: tr
      
!  ** read in the file header, and get the number of points
      call rsac1(fname,rdummy,ndummy,beg,delta,0,nerr)
      call getnhv('NPTS',npts,nerr)

!  ** build a new trace
      call sacio90_new(npts,delta,tr)

!  ** load the data
      call rsac1(fname, tr % y, npts, beg, delta, npts, nerr)
      
!  ** populate the structure
      call getfhv('delta',tr%delta,nerr)
      call getfhv('depmin',tr%depmin,nerr)
      call getfhv('depmax',tr%depmax,nerr)
      call getfhv('scale',tr%scale,nerr)
      call getfhv('odelta',tr%odelta,nerr)
      call getfhv('b',tr%b,nerr)
      call getfhv('e',tr%e,nerr)
      call getfhv('o',tr%o,nerr)
      call getfhv('a',tr%a,nerr)
      call getfhv('t0',tr%t0,nerr)
      call getfhv('t1',tr%t1,nerr)
      call getfhv('t2',tr%t2,nerr)
      call getfhv('t3',tr%t3,nerr)
      call getfhv('t4',tr%t4,nerr)
      call getfhv('t5',tr%t5,nerr)
      call getfhv('t6',tr%t6,nerr)
      call getfhv('t7',tr%t7,nerr)
      call getfhv('t8',tr%t8,nerr)
      call getfhv('t9',tr%t9,nerr)
      call getfhv('f',tr%f,nerr)
      call getfhv('resp0',tr%resp0,nerr)
      call getfhv('resp1',tr%resp1,nerr)
      call getfhv('resp2',tr%resp2,nerr)
      call getfhv('resp3',tr%resp3,nerr)
      call getfhv('resp4',tr%resp4,nerr)
      call getfhv('resp5',tr%resp5,nerr)
      call getfhv('resp6',tr%resp6,nerr)
      call getfhv('resp7',tr%resp7,nerr)
      call getfhv('resp8',tr%resp8,nerr)
      call getfhv('resp9',tr%resp9,nerr)
      call getfhv('stla',tr%stla,nerr)
      call getfhv('stlo',tr%stlo,nerr)
      call getfhv('stel',tr%stel,nerr)
      call getfhv('stdp',tr%stdp,nerr)
      call getfhv('evla',tr%evla,nerr)
      call getfhv('evlo',tr%evlo,nerr)
      call getfhv('evel',tr%evel,nerr)
      call getfhv('evdp',tr%evdp,nerr)
      call getfhv('mag',tr%mag,nerr)
      call getfhv('user0',tr%user0,nerr)
      call getfhv('user1',tr%user1,nerr)
      call getfhv('user2',tr%user2,nerr)
      call getfhv('user3',tr%user3,nerr)
      call getfhv('user4',tr%user4,nerr)
      call getfhv('user5',tr%user5,nerr)
      call getfhv('user6',tr%user6,nerr)
      call getfhv('user7',tr%user7,nerr)
      call getfhv('user8',tr%user8,nerr)
      call getfhv('user9',tr%user9,nerr)
      call getfhv('dist',tr%dist,nerr)
      call getfhv('az',tr%az,nerr)
      call getfhv('baz',tr%baz,nerr)
      call getfhv('gcarc',tr%gcarc,nerr)
      call getfhv('depmen',tr%depmen,nerr)
      call getfhv('cmpaz',tr%cmpaz,nerr)
      call getfhv('cmpinc',tr%cmpinc,nerr)
      call getfhv('xminimum',tr%xminimum,nerr)
      call getfhv('xmaximum',tr%xmaximum,nerr)
      call getfhv('yminimum',tr%yminimum,nerr)
      call getfhv('ymaximum',tr%ymaximum,nerr)
      call getnhv('nzyear',tr%nzyear,nerr)
      call getnhv('nzjday',tr%nzjday,nerr)
      call getnhv('nzhour',tr%nzhour,nerr)
      call getnhv('nzmin',tr%nzmin,nerr)
      call getnhv('nzsec',tr%nzsec,nerr)
      call getnhv('nzmsec',tr%nzmsec,nerr)
      call getnhv('nvhdr',tr%nvhdr,nerr)
      call getnhv('norid',tr%norid,nerr)
      call getnhv('nevid',tr%nevid,nerr)
      call getnhv('npts',tr%npts,nerr)
      call getnhv('nwfid',tr%nwfid,nerr)
      call getnhv('nxsize',tr%nxsize,nerr)
      call getnhv('nysize',tr%nysize,nerr)
      call getnhv('unused8',tr%unused8,nerr)
      call getnhv('iftype',tr%iftype,nerr)
      call getnhv('idep',tr%idep,nerr)
      call getnhv('iztype',tr%iztype,nerr)
      call getnhv('unused9',tr%unused9,nerr)
      call getnhv('iinst',tr%iinst,nerr)
      call getnhv('istreg',tr%istreg,nerr)
      call getnhv('ievreg',tr%ievreg,nerr)
      call getnhv('ievtyp',tr%ievtyp,nerr)
      call getnhv('iqual',tr%iqual,nerr)
      call getnhv('isynth',tr%isynth,nerr)
      call getnhv('imagtyp',tr%imagtyp,nerr)
      call getnhv('imagsrc',tr%imagsrc,nerr)
      call getlhv('even',tr%leven,nerr)
      call getlhv('lpspol',tr%lpspol,nerr)
      call getlhv('lovrok',tr%lovrok,nerr)
      call getlhv('lcalda',tr%lcalda,nerr)
      call getkhv('kstnm',tr%kstnm(1:8),nerr)
      call getkhv('kevnm',tr%kevnm(1:16),nerr)
      call getkhv('khole',tr%khole(1:8),nerr)
      call getkhv('ko',tr%ko(1:8),nerr)
      call getkhv('ka',tr%ka(1:8),nerr)
      call getkhv('kt0',tr%kt0(1:8),nerr)
      call getkhv('kt1',tr%kt1(1:8),nerr)
      call getkhv('kt2',tr%kt2(1:8),nerr)
      call getkhv('kt3',tr%kt3(1:8),nerr)
      call getkhv('kt4',tr%kt4(1:8),nerr)
      call getkhv('kt5',tr%kt5(1:8),nerr)
      call getkhv('kt6',tr%kt6(1:8),nerr)
      call getkhv('kt7',tr%kt7(1:8),nerr)
      call getkhv('kt8',tr%kt8(1:8),nerr)
      call getkhv('kt9',tr%kt9(1:8),nerr)
      call getkhv('kf',tr%kf(1:8),nerr)
      call getkhv('kuser0',tr%kuser0(1:8),nerr)
      call getkhv('kuser1',tr%kuser1(1:8),nerr)
      call getkhv('kuser2',tr%kuser2(1:8),nerr)
      call getkhv('kcmpnm',tr%kcmpnm(1:8),nerr)
      call getkhv('knetwk',tr%knetwk(1:8),nerr)
      call getkhv('kdatrd',tr%kdatrd(1:8),nerr)
      call getkhv('kinst',tr%kinst(1:8),nerr)            
      
!  ** handle error

   end subroutine sacio90_read
!===============================================================================

!===============================================================================
   end module sacio90
!===============================================================================
!  END OF F90SAC module
!===============================================================================

