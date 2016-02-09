module mmisqcdloop
!AC!   use precision, only: ki, ki_ql
!AC!   use constants
!AC!   use options
!AC!   use mfunctions
!AC!   use notfirst
!AC!   use mmishighrank
!AC!   implicit none
!AC!
!AC!   private
!AC!
!AC!   complex(ki), dimension(:,:), allocatable, public :: s_mat
!AC!
!AC!	interface
!AC!		function qlI4(p1,p2,p3,p4,s12,s23,m1,m2,m3,m4,mu2,ep)
!AC!			use precision, only: ki_ql
!AC!			implicit none
!AC!			real(ki_ql), intent(in) :: p1,p2,p3,p4,s12,s23
!AC!			real(ki_ql), intent(in) :: m1,m2,m3,m4,mu2
!AC!			integer, intent(in) :: ep
!AC!			complex(ki_ql) :: qlI4
!AC!		end function qlI4
!AC!	end interface
!AC!	interface
!AC!		function qlI3(p1,p2,p3,m1,m2,m3,mu2,ep)
!AC!			use precision, only: ki_ql
!AC!			implicit none
!AC!			real(ki_ql), intent(in) :: p1,p2,p3
!AC!			real(ki_ql), intent(in) :: m1,m2,m3,mu2
!AC!			integer, intent(in) :: ep
!AC!			complex(ki_ql) :: qlI3
!AC!		end function qlI3
!AC!	end interface
!AC!	interface
!AC!		function qlI2(p1,m1,m2,mu2,ep)
!AC!			use precision, only: ki_ql
!AC!			implicit none
!AC!			real(ki_ql), intent(in) :: p1
!AC!			real(ki_ql), intent(in) :: m1,m2,mu2
!AC!			integer, intent(in) :: ep
!AC!			complex(ki_ql) :: qlI2
!AC!		end function qlI2
!AC!	end interface
!AC!	interface
!AC!		function qlI1(m1,mu2,ep)
!AC!			use precision, only: ki_ql
!AC!			implicit none
!AC!			real(ki_ql), intent(in) :: m1,mu2
!AC!			integer, intent(in) :: ep
!AC!			complex(ki_ql) :: qlI1
!AC!		end function qlI1
!AC!	end interface
!AC!
!AC!	public :: qcdloop4, qcdloop3, qcdloop2,qcdloop2hr, qcdloop1
!AC!
!AC!contains
!AC!
!AC!subroutine qcdloop4(V,m,scale2,MI4,cache_flag, cache_offset, scalar_cache)
!AC!	implicit none
!AC!	real(ki),    dimension(1:6),  intent(in ) :: V
!AC!	complex(ki), dimension(0:3),  intent(in ) :: m
!AC!	real(ki), 		      intent(in ) :: scale2
!AC!	complex(ki), dimension(-2:0), intent(out) :: MI4
!AC!	
!AC!	logical,     intent(in   ), optional 			:: cache_flag
!AC!	integer,     intent(inout), optional 			:: cache_offset
!AC!	complex(ki), intent(inout), optional, dimension(-2:0,*) :: scalar_cache
!AC!	
!AC!	complex(ki) :: m0, m1, m2, m3
!AC!	real(ki)    :: V1, V2, V3, V21, V31, V32
!AC!	integer     :: ep, cache_index
!AC!	complex(ki) :: MI4tmp
!AC!	
!AC!	notfirsti=.true.
!AC!	
!AC!	m0  = m(0)
!AC!	m1  = m(1)
!AC!	m2  = m(2)
!AC!	m3  = m(3)
!AC!	V1  = V(1)
!AC!	V2  = V(2)
!AC!	V3  = V(3)
!AC!	V21 = V(4)
!AC!	V31 = V(5)
!AC!	V32 = V(6)
!AC!	
!AC!  1   Format(A3,I4,A1,I2,A5,D24.15,A1,D24.15,A3)
!AC!
!AC!	if (present(cache_flag)) cache_index = lbound(scalar_cache,2)+cache_offset
!AC!	if ( (aimag(m0).ne.0) .or. (aimag(m1).ne.0) .or. &
!AC!	&    (aimag(m2).ne.0) .or. (aimag(m3).ne.0) ) then
!AC!		print *,'isca=1: QCDLoop does not support complex masses'
!AC!		stop
!AC!	endif
!AC!	do ep=-2,0
!AC!		if (present(cache_flag)) then
!AC!			if (cache_flag) then
!AC!				MI4tmp = scalar_cache(ep,cache_index)
!AC!			else
!AC!				MI4tmp=qlI4(&
!AC!				& real(V1,ki_ql),real(V21,ki_ql),real(V32,ki_ql),&
!AC!				& real(V3,ki_ql),real(V2,ki_ql),real(V31,ki_ql),&
!AC!				& real(m0,ki_ql),real(m1,ki_ql),real(m2,ki_ql),&
!AC!				& real(m3,ki_ql),real(scale2,ki_ql),ep)
!AC!				scalar_cache(ep,cache_index) = MI4tmp
!AC!			end if
!AC!		else
!AC!			MI4tmp=qlI4(&
!AC!			& real(V1,ki_ql),real(V21,ki_ql),real(V32,ki_ql),&
!AC!			& real(V3,ki_ql),real(V2,ki_ql),real(V31,ki_ql),&
!AC!			& real(m0,ki_ql),real(m1,ki_ql),real(m2,ki_ql),&
!AC!			& real(m3,ki_ql),real(scale2,ki_ql),ep)
!AC!		end if
!AC!		MI4(ep)=MI4tmp
!AC!	enddo
!AC!	if (present(cache_flag)) cache_offset = cache_offset + 1
!AC!end subroutine qcdloop4
!AC!
!AC!subroutine qcdloop3(V,m,scale2,MI3,cache_flag, cache_offset, scalar_cache)
!AC!	implicit none
!AC!	
!AC!	real(ki),    dimension(1:3),  intent(in ) :: V
!AC!	complex(ki), dimension(0:2),  intent(in ) :: m
!AC!	real(ki), 		      intent(in ) :: scale2
!AC!	complex(ki), dimension(-2:0), intent(out) :: MI3
!AC!	
!AC!	logical,     intent(in   ), optional 			:: cache_flag
!AC!	integer,     intent(inout), optional 			:: cache_offset
!AC!	complex(ki), intent(inout), optional, dimension(-2:0,*) :: scalar_cache
!AC!	
!AC!	complex(ki) :: m0, m1, m2
!AC!	real(ki)    :: V1, V2, V3
!AC!	complex(ki) :: MI3tmp
!AC!	integer     :: ep, cache_index
!AC!	
!AC!	notfirsti=.true.
!AC!
!AC!	m0 = m(0)
!AC!	m1 = m(1)
!AC!	m2 = m(2)
!AC!	V1 = V(1)
!AC!	V2 = V(2)
!AC!	V3 = V(3)
!AC!	
!AC!  1  Format(A3,I3,A1,I2,A5,D24.15,A1,D24.15,A3)
!AC!
!AC!	if (present(cache_flag)) cache_index = lbound(scalar_cache,2)+cache_offset
!AC!	do ep=-2,0
!AC!		if (present(cache_flag)) then
!AC!			if (cache_flag) then
!AC!				MI3tmp = scalar_cache(ep,cache_index)
!AC!			else
!AC!				MI3tmp=qlI3(&
!AC!				& real(V1,ki_ql),real(V2,ki_ql),real(V3,ki_ql),&
!AC!				& real(m0,ki_ql),real(m1,ki_ql),real(m2,ki_ql),&
!AC!				& real(scale2,ki_ql),ep)
!AC!				scalar_cache(ep,cache_index) = MI3tmp 
!AC!			end if
!AC!		else
!AC!			MI3tmp=qlI3(&
!AC!			& real(V1,ki_ql),real(V2,ki_ql),real(V3,ki_ql),&
!AC!			& real(m0,ki_ql),real(m1,ki_ql),real(m2,ki_ql),&
!AC!			& real(scale2,ki_ql),ep)
!AC!		end if
!AC!		MI3(ep)=MI3tmp
!AC!	enddo
!AC!	if (present(cache_flag)) cache_offset = cache_offset + 1
!AC!
!AC!end subroutine qcdloop3
!AC!
!AC!subroutine qcdloop2(K11,m,scale2,MI2J0,MI2J1,MI2J00,MI2J01,MI2J11,&
!AC!			& cache_flag, cache_offset, scalar_cache)
!AC!	implicit none
!AC!	
!AC!	real(ki),    		     intent(in ) :: K11 
!AC!	complex(ki), dimension(0:1), intent(in ) :: m
!AC!	real(ki), 		     intent(in ) :: scale2
!AC!	complex(ki), dimension(-2:0),intent(out) :: MI2J0, MI2J1, MI2J00, MI2J01, MI2J11
!AC!	
!AC!	logical,     intent(in   ), optional 			:: cache_flag
!AC!	integer,     intent(inout), optional 			:: cache_offset
!AC!	complex(ki), intent(inout), optional, dimension(-2:0,*) :: scalar_cache
!AC!	
!AC!	complex(ki) :: m0, m1
!AC!	integer     :: ep, cache_index
!AC!
!AC!	notfirsti=.true.
!AC!	
!AC!	m0=m(0)
!AC!	m1=m(1)
!AC!	
!AC!	if (present(cache_flag)) cache_index = lbound(scalar_cache,2)+cache_offset
!AC!	if (present(cache_flag)) then
!AC!		if (cache_flag) then
!AC!			MI2J0(:)  = scalar_cache(:,cache_index+0)
!AC!			MI2J1(:)  = scalar_cache(:,cache_index+1)
!AC!			MI2J01(:) = scalar_cache(:,cache_index+2)
!AC!			MI2J11(:) = scalar_cache(:,cache_index+3)
!AC!			MI2J00(:) = scalar_cache(:,cache_index+4)
!AC!		else
!AC!			 do ep=-2,0
!AC!				MI2J00(ep) = qlI2(&
!AC!				& real(K11,ki_ql),real(m0,ki_ql),real(m0,ki_ql),&
!AC!				& real(scale2,ki_ql),ep)
!AC!				MI2J11(ep) = qlI2(&
!AC!				& real(K11,ki_ql),real(m1,ki_ql),real(m1,ki_ql),&
!AC!				& real(scale2,ki_ql),ep)
!AC!				MI2J01(ep)=qlI2(&
!AC!				& real(K11,ki_ql),real(m0,ki_ql),real(m1,ki_ql),&
!AC!				& real(scale2,ki_ql),ep)
!AC!				MI2J0(ep) = qlI1(real(m0,ki_ql),real(scale2,ki_ql),ep)
!AC!				MI2J1(ep) = qlI1(real(m1,ki_ql),real(scale2,ki_ql),ep)
!AC!			 end do
!AC!			 scalar_cache(:,cache_index+0) = MI2J0(:)
!AC!			 scalar_cache(:,cache_index+1) = MI2J1(:)
!AC!			 scalar_cache(:,cache_index+2) = MI2J01(:)
!AC!			 scalar_cache(:,cache_index+3) = MI2J11(:)
!AC!			 scalar_cache(:,cache_index+4) = MI2J00(:)
!AC!		end if
!AC!	else
!AC!		do ep=-2,0
!AC!			MI2J00(ep) = qlI2(&
!AC!			& real(K11,ki_ql),real(m0,ki_ql),real(m0,ki_ql),&
!AC!			& real(scale2,ki_ql),ep)
!AC!			MI2J11(ep) = qlI2(&
!AC!			& real(K11,ki_ql),real(m1,ki_ql),real(m1,ki_ql),&
!AC!			& real(scale2,ki_ql),ep)
!AC!			MI2J01(ep)=qlI2(&
!AC!			& real(K11,ki_ql),real(m0,ki_ql),real(m1,ki_ql),&
!AC!			& real(scale2,ki_ql),ep)
!AC!			MI2J0(ep) = qlI1(real(m0,ki_ql),real(scale2,ki_ql),ep)
!AC!			MI2J1(ep) = qlI1(real(m1,ki_ql),real(scale2,ki_ql),ep)
!AC!		end do
!AC!	end if
!AC!	if (present(cache_flag)) cache_offset = cache_offset + 5
!AC!
!AC!end subroutine qcdloop2
!AC!
!AC!
!AC!subroutine qcdloop2hr(K11,m,scale2,J111)
!AC!	implicit none
!AC!	real(ki),    intent(in) :: K11
!AC!	complex(ki), dimension(0:1), intent(in) :: m
!AC!	real(ki),    intent(in) :: scale2
!AC!	complex(ki), dimension(-2:0), intent(out) :: J111
!AC!	integer     :: ep
!AC!	complex(ki) :: m0, m1
!AC! 	complex(ki), dimension(-2:0) :: B0p12, B0z11, B0z22
!AC!
!AC!	m0 = m(0)
!AC!	m1 = m(1)
!AC!
!AC!	do ep=-2,0
!AC!		B0p12(ep) = qlI2(&
!AC!		& real(K11,ki_ql),real(m0,ki_ql),real(m1,ki_ql),&
!AC!		& real(scale2,ki_ql),ep)
!AC!		B0z11(ep) = qlI2(&
!AC!		& real(zip,ki_ql),real(m0,ki_ql),real(m0,ki_ql),&
!AC!		& real(scale2,ki_ql),ep)
!AC!		B0z22(ep) = qlI2(&
!AC!		& real(zip,ki_ql),real(m1,ki_ql),real(m1,ki_ql),&
!AC!		& real(scale2,ki_ql),ep)
!AC!	enddo
!AC!
!AC!	call HJ111(J111, K11,m0,m1,B0p12,B0z11,B0z22)
!AC!
!AC!end subroutine qcdloop2hr
!AC!
!AC!subroutine qcdloop1(m,scale2,MI1, cache_flag, cache_offset, scalar_cache)
!AC!	implicit none 
!AC!	complex(ki), 			  intent(in ) :: m
!AC!	real(ki), 			  intent(in ) :: scale2
!AC!	complex(ki), dimension(-2:0), 	  intent(out) :: MI1
!AC!	
!AC!	logical,     intent(in   ), optional 			:: cache_flag
!AC!	integer,     intent(inout), optional 			:: cache_offset
!AC!	complex(ki), intent(inout), optional, dimension(-2:0,*) :: scalar_cache
!AC!	
!AC!	integer     :: j1
!AC!	complex(ki) :: m0, MI1tmp
!AC!	integer     :: ep, cache_index
!AC!	
!AC!	notfirsti=.true.
!AC!	
!AC!	m0=m
!AC!	
!AC!  1   Format(A3,I2,A1,I2,A5,D24.15,A1,D24.15,A3)
!AC!
!AC!	if (present(cache_flag)) cache_index = lbound(scalar_cache,2)+cache_offset
!AC!	do ep=-2,0
!AC!		if (present(cache_flag)) then
!AC!			if (cache_flag) then
!AC!				MI1tmp = scalar_cache(ep,cache_index)
!AC!			else
!AC!				MI1tmp = qlI1(real(m0,ki_ql),real(scale2,ki_ql),ep)
!AC!				scalar_cache(ep,cache_index) = MI1tmp
!AC!			end if
!AC!		else
!AC!			MI1tmp = qlI1(real(m0,ki_ql),real(scale2,ki_ql),ep)
!AC!		end if
!AC!		MI1(ep)=MI1tmp
!AC!	enddo
!AC!	if (present(cache_flag)) cache_offset = cache_offset + 1
!AC!
!AC!end subroutine qcdloop1
!AC!
end module mmisqcdloop

