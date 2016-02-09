module mmisgolem 
!AC!	use precision, only: ki
!AC!	use precision_golem, only: ki_gol => ki
!AC!	use constants
!AC!	use options
!AC!	use notfirst
!AC!	use mmishighrank
!AC!	implicit none
!AC!	
!AC!	private
!AC!	
!AC!	interface
!AC!		function gD0(p1,p2,p3,p4,s12,s23,m1,m2,m3,m4,mu2,ep)
!AC!			use precision_golem, only: ki
!AC!			implicit none
!AC!			real(ki), intent(in) :: p1,p2,p3,p4,s12,s23
!AC!			real(ki), intent(in) :: m1,m2,m3,m4
!AC!			real(ki), intent(in) :: mu2
!AC!			integer, intent(in) :: ep
!AC!			complex(ki) :: gD0
!AC!		end function gD0
!AC!	end interface
!AC!	interface
!AC!		function gC0(p1,p2,p3,m1,m2,m3,mu2,ep)
!AC!			use precision_golem, only: ki
!AC!			implicit none
!AC!			real(ki), intent(in) :: p1,p2,p3
!AC!			real(ki), intent(in) :: m1,m2,m3
!AC!			real(ki), intent(in) :: mu2
!AC!			integer, intent(in) :: ep
!AC!			complex(ki) :: gC0
!AC!		end function gC0
!AC!	end interface
!AC!	interface
!AC!		function gB0(p1,m1,m2,mu2,ep)
!AC!			use precision_golem, only: ki
!AC!			implicit none
!AC!			real(ki), intent(in) :: p1
!AC!			real(ki), intent(in) :: m1,m2
!AC!			real(ki), intent(in) :: mu2
!AC!			integer, intent(in) :: ep
!AC!			complex(ki) :: gB0
!AC!		end function gB0
!AC!	end interface
!AC!	interface
!AC!		function gD0C(p1,p2,p3,p4,s12,s23,m1,m2,m3,m4,mu2,ep)
!AC!			use precision_golem, only: ki
!AC!			implicit none
!AC!			complex(ki), intent(in) :: p1,p2,p3,p4,s12,s23
!AC!			complex(ki), intent(in) :: m1,m2,m3,m4
!AC!			real(ki), intent(in) :: mu2
!AC!			integer, intent(in) :: ep
!AC!			complex(ki) :: gD0C
!AC!		end function gD0C
!AC!	end interface
!AC!	interface
!AC!		function gC0C(p1,p2,p3,m1,m2,m3,mu2,ep)
!AC!			use precision_golem, only: ki
!AC!			implicit none
!AC!			complex(ki), intent(in) :: p1,p2,p3
!AC!			complex(ki), intent(in) :: m1,m2,m3
!AC!			real(ki), intent(in) :: mu2
!AC!			integer, intent(in) :: ep
!AC!			complex(ki) :: gC0C
!AC!		end function gC0C
!AC!	end interface
!AC!	interface
!AC!		function gC0i(idt,p1,p2,p3,m1,m2,m3,mu2,ep)
!AC!			use precision_golem, only: ki
!AC!			implicit none
!AC!			character (len=*), intent (in) :: idt
!AC!			real(ki), intent(in) :: p1,p2,p3
!AC!			real(ki), intent(in) :: m1,m2,m3
!AC!			real(ki), intent(in) :: mu2
!AC!			integer, intent(in) :: ep
!AC!			complex(ki) :: gC0i
!AC!		end function gC0i
!AC!	end interface
!AC!	interface
!AC!		function gC0iC(idt,p1,p2,p3,m1,m2,m3,mu2,ep)
!AC!			use precision_golem, only: ki
!AC!			implicit none
!AC!			character (len=*), intent (in) :: idt
!AC!			complex(ki), intent(in) :: p1,p2,p3
!AC!			complex(ki), intent(in) :: m1,m2,m3
!AC!			real(ki), intent(in) :: mu2
!AC!			integer, intent(in) :: ep
!AC!			complex(ki) :: gC0iC
!AC!		end function gC0iC
!AC!	end interface
!AC!	interface
!AC!		function gB0C(p1,m1,m2,mu2,ep)
!AC!			use precision_golem, only: ki
!AC!			implicit none
!AC!			complex(ki), intent(in) :: p1
!AC!			complex(ki), intent(in) :: m1,m2
!AC!			real(ki), intent(in) :: mu2
!AC!			integer, intent(in) :: ep
!AC!			complex(ki) :: gB0C
!AC!		end function gB0C
!AC!	end interface
!AC!	interface
!AC!		function gB0i(idt,p1,m1,m2,mu2,ep)
!AC!			use precision_golem, only: ki
!AC!			implicit none
!AC!			character (len=*), intent (in) :: idt
!AC!			real(ki), intent(in) :: p1
!AC!			real(ki), intent(in) :: m1,m2
!AC!			real(ki), intent(in) :: mu2
!AC!			integer, intent(in) :: ep
!AC!			complex(ki) :: gB0i
!AC!		end function gB0i
!AC!	end interface
!AC!	interface
!AC!		function gB0iC(idt,p1,m1,m2,mu2,ep)
!AC!			use precision_golem, only: ki
!AC!			implicit none
!AC!			character (len=*), intent (in) :: idt
!AC!			complex(ki), intent(in) :: p1
!AC!			complex(ki), intent(in) :: m1,m2
!AC!			real(ki), intent(in) :: mu2
!AC!			integer, intent(in) :: ep
!AC!			complex(ki) :: gB0iC
!AC!		end function gB0iC
!AC!	end interface
!AC!	!interface
!AC!	!	function gA0(m1,mu2,ep)
!AC!	!		use precision_golem, only: ki
!AC!	!		implicit none
!AC!	!		real(ki), intent(in) :: m1
!AC!	!		real(ki), intent(in) :: mu2
!AC!	!		integer, intent(in) :: ep
!AC!	!		complex(ki) :: gA0
!AC!	!	end function gA0
!AC!	!end interface
!AC!	interface
!AC!		function gA0C(m1,mu2,ep)
!AC!			use precision_golem, only: ki
!AC!			implicit none
!AC!			complex(ki), intent(in) :: m1
!AC!			real(ki), intent(in) :: mu2
!AC!			integer, intent(in) :: ep
!AC!			complex(ki) :: gA0C
!AC!		end function gA0C
!AC!	end interface
!AC!	interface
!AC!		function gA0i(idt,m1,mu2,ep)
!AC!			use precision_golem, only: ki
!AC!			implicit none
!AC!			character (len=*), intent (in) :: idt
!AC!			real(ki), intent(in) :: m1
!AC!			real(ki), intent(in) :: mu2
!AC!			integer, intent(in) :: ep
!AC!			complex(ki) :: gA0i
!AC!		end function gA0i
!AC!	end interface
!AC!	interface
!AC!		function gA0iC(idt,m1,mu2,ep)
!AC!			use precision_golem, only: ki
!AC!			implicit none
!AC!			character (len=*), intent (in) :: idt
!AC!			complex(ki), intent(in) :: m1
!AC!			real(ki), intent(in) :: mu2
!AC!			integer, intent(in) :: ep
!AC!			complex(ki) :: gA0iC
!AC!		end function gA0iC
!AC!	end interface
!AC!
!AC!
!AC!	public :: golemMI4, golemMI3, golemMI2, golemMI2hr1,golemMI2hr2, golemMI1
!AC!
!AC!
!AC!contains
!AC!
!AC!subroutine golemMI4(V,m,scale2,MI4,cache_flag, cache_offset, scalar_cache)
!AC!	implicit none
!AC!	real(ki),    dimension(1:6), intent(in ) :: V
!AC!	complex(ki), dimension(0:3), intent(in ) :: m
!AC!	real(ki),		     intent(in ) :: scale2
!AC!	complex(ki), dimension(-2:0),       intent(out) :: MI4
!AC!	
!AC!	logical,     intent(in   ), optional 			:: cache_flag
!AC!	integer,     intent(inout), optional 			:: cache_offset
!AC!	complex(ki), intent(inout), optional, dimension(-2:0,*) :: scalar_cache
!AC!	
!AC!	complex(ki) :: m0, m1, m2, m3
!AC!	complex(ki) :: V1, V2, V3, V21, V31, V32
!AC!	integer     :: ep, cache_index
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
!AC!	if (present(cache_flag)) cache_index = lbound(scalar_cache,2)+cache_offset
!AC!
!AC!	call gtrunc(abs(V32)+abs(V31),V1,V2,V3,V21,V32,V31,m0,m1,m2,m3)
!AC!	do ep=-2,0
!AC!		if (present(cache_flag)) then
!AC!			if (cache_flag) then
!AC!				MI4(ep) = scalar_cache(ep,cache_index)
!AC!			else
!AC!				MI4(ep)=gD0(real(V1,ki_gol),real(V21,ki_gol),&
!AC!				& real(V32,ki_gol),real(V3,ki_gol),&
!AC!				& real(V2,ki_gol),real(V31,ki_gol),&
!AC!				& real(m0,ki_gol),real(m1,ki_gol),&
!AC!				& real(m2,ki_gol),real(m3,ki_gol),&
!AC!				& real(scale2,ki_gol),ep)
!AC!				scalar_cache(ep,cache_index) = MI4(ep)
!AC!			end if
!AC!		else
!AC!			MI4(ep)=gD0(real(V1,ki_gol),real(V21,ki_gol),&
!AC!			& real(V32,ki_gol),real(V3,ki_gol),&
!AC!			& real(V2,ki_gol),real(V31,ki_gol),&
!AC!			& real(m0,ki_gol),real(m1,ki_gol),&
!AC!			& real(m2,ki_gol),real(m3,ki_gol),&
!AC!			& real(scale2,ki_gol),ep)
!AC!		end if
!AC!	end do
!AC!	if (present(cache_flag)) cache_offset = cache_offset + 1
!AC!end subroutine golemMI4
!AC!
!AC!subroutine golemMI3(V,m,scale2,MI3,cache_flag, cache_offset, scalar_cache)
!AC!	implicit none
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
!AC!	complex(ki) :: V1, V2, V3
!AC!	integer :: ep, cache_index
!AC!	complex(ki), dimension(-2:0) :: c0t
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
!AC!
!AC!	if (present(cache_flag)) cache_index = lbound(scalar_cache,2)+cache_offset
!AC!	call gtrunc(abs(V1)+abs(V2)+abs(V3),V1,V2,V3,m0,m1,m2)
!AC!	do ep=-2,0
!AC!		if (present(cache_flag)) then
!AC!			if (cache_flag) then
!AC!				c0t(ep) = scalar_cache(ep,cache_index)
!AC!			else
!AC!				c0t(ep)=gC0(real(V1,ki_gol),real(V2,ki_gol),&
!AC!				& real(V3,ki_gol),real(m0,ki_gol),&
!AC!				& real(m1,ki_gol),real(m2,ki_gol),&
!AC!				& real(scale2,ki_gol),ep)
!AC!				scalar_cache(ep,cache_index) = c0t(ep)
!AC!			end if
!AC!		else
!AC!			c0t(ep)=gC0(real(V1,ki_gol),real(V2,ki_gol),&
!AC!			& real(V3,ki_gol),real(m0,ki_gol),&
!AC!			& real(m1,ki_gol),real(m2,ki_gol),&
!AC!			& real(scale2,ki_gol),ep)
!AC!		end if
!AC!	end do
!AC!	MI3(:) = c0t(:) 
!AC!	if (present(cache_flag)) cache_offset = cache_offset + 1
!AC!end subroutine golemMI3
!AC!
!AC!subroutine golemMI2(K11in,m,scale2,J0,J1,J00,J01,J11, cache_flag, cache_offset, scalar_cache)
!AC!	implicit none
!AC!	real(ki),     		      intent(in ) :: K11in
!AC!	complex(ki), dimension(0:1),  intent(in ) :: m
!AC!	real(ki), 		      intent(in ) :: scale2
!AC!        complex(ki), dimension(-2:0),intent(out) :: J0, J1, J00, J01, J11
!AC!	
!AC!	logical,     intent(in   ), optional 			:: cache_flag
!AC!	integer,     intent(inout), optional 			:: cache_offset
!AC!	complex(ki), intent(inout), optional, dimension(-2:0,*) :: scalar_cache
!AC!	
!AC!	complex(ki) :: m0, m1
!AC!	complex(ki) :: K11
!AC!	integer     :: ep, cache_index
!AC!
!AC!	notfirsti=.true.
!AC!	
!AC!	m0=m(0)
!AC!	m1=m(1)
!AC!	K11=K11in
!AC!	
!AC!	if (present(cache_flag)) cache_index = lbound(scalar_cache,2)+cache_offset
!AC!	if (present(cache_flag)) then
!AC!		if (cache_flag) then
!AC!			J0(:)  = scalar_cache(:,cache_index+0)
!AC!			J1(:)  = scalar_cache(:,cache_index+1)
!AC!			J01(:) = scalar_cache(:,cache_index+2)
!AC!			J11(:) = scalar_cache(:,cache_index+3)
!AC!			J00(:) = scalar_cache(:,cache_index+4)
!AC!		else
!AC!			call gtrunc(abs(K11)+1.0_ki,K11,m0,m1)
!AC!			do ep=-2,0
!AC!				J00(ep)= gB0(real(K11,ki_gol),&
!AC!				& real(m0,ki_gol),real(m0,ki_gol),&
!AC!				& real(scale2,ki_gol),ep)
!AC!				J11(ep)= gB0(real(K11,ki_gol),&
!AC!				& real(m1,ki_gol),real(m1,ki_gol),&
!AC!				& real(scale2,ki_gol),ep)
!AC!				J01(ep)= gB0(real(K11,ki_gol),&
!AC!				& real(m0,ki_gol),real(m1,ki_gol),&
!AC!				& real(scale2,ki_gol),ep)
!AC!				J0(ep) = gA0(real(m0,ki_gol),&
!AC!				& real(scale2,ki_gol),ep)
!AC!				J1(ep) = gA0(real(m1,ki_gol),&
!AC!				& real(scale2,ki_gol),ep)
!AC!			end do
!AC!			scalar_cache(:,cache_index+0) = J0(:)
!AC!			scalar_cache(:,cache_index+1) = J1(:)
!AC!			scalar_cache(:,cache_index+2) = J01(:)
!AC!			scalar_cache(:,cache_index+3) = J11(:)
!AC!			scalar_cache(:,cache_index+4) = J00(:)
!AC!		end if
!AC!	else
!AC!		call gtrunc(abs(K11)+1.0_ki, K11,m0,m1)
!AC!		do ep=-2,0
!AC!			J00(ep)= gB0(real(K11,ki_gol),&
!AC!			& real(m0,ki_gol),real(m0,ki_gol),&
!AC!			& real(scale2,ki_gol),ep)
!AC!			J11(ep)= gB0(real(K11,ki_gol),&
!AC!			& real(m1,ki_gol),real(m1,ki_gol),&
!AC!			& real(scale2,ki_gol),ep)
!AC!			J01(ep)= gB0(real(K11,ki_gol),&
!AC!			& real(m0,ki_gol),real(m1,ki_gol),&
!AC!			& real(scale2,ki_gol),ep)
!AC!			J0(ep) = gA0(real(m0,ki_gol),&
!AC!			& real(scale2,ki_gol),ep)
!AC!			J1(ep) = gA0(real(m1,ki_gol),&
!AC!			& real(scale2,ki_gol),ep)
!AC!		end do
!AC!	end if
!AC!	if (present(cache_flag)) cache_offset = cache_offset + 5
!AC!end subroutine golemMI2
!AC!
!AC!subroutine golemMI2hr1(K11,m,scale2,J111,J001)
!AC!	implicit none
!AC!	real(ki),     		      intent(in )           :: K11
!AC!	complex(ki), dimension(0:1),  intent(in )           :: m
!AC!	real(ki), 		      intent(in )           :: scale2
!AC!        complex(ki), dimension(-2:0),intent(out)           :: J111
!AC!        complex(ki), dimension(-2:0),intent(out), optional :: J001
!AC!	
!AC!	complex(ki) :: m0, m1
!AC!	integer     :: ep
!AC!
!AC!	m0=m(0)
!AC!	m1=m(1)
!AC!
!AC!		do ep=-2,0
!AC!			J001(ep) = gB0i("001",real(K11,ki_gol),&
!AC!			& real(m0,ki_gol),real(m1,ki_gol),&
!AC!			& real(scale2,ki_gol),ep)
!AC!			J111(ep) = gB0i("111",real(K11,ki_gol),&
!AC!			& real(m0,ki_gol),real(m1,ki_gol),&
!AC!			& real(scale2,ki_gol),ep)
!AC!		end do
!AC!end subroutine golemMI2hr1
!AC!
!AC!subroutine golemMI2hr2(K11,m,scale2,J111)
!AC!	implicit none
!AC!	real(ki),   		      intent(in ) :: K11
!AC!	complex(ki), dimension(0:1),  intent(in ) :: m
!AC!	real(ki),   		      intent(in ) :: scale2
!AC!	complex(ki), dimension(-2:0), intent(out) :: J111
!AC!
!AC!	integer     :: ep
!AC!	complex(ki) :: m0,m1
!AC! 	complex(ki), dimension(-2:0) :: B0p12, B0z11, B0z22
!AC!
!AC!	m0=m(0)
!AC!	m1=m(1)
!AC!
!AC!	do ep=-2,0
!AC!		B0p12(ep)= gB0(real(K11,ki_gol),&
!AC!		& real(m0,ki_gol),real(m1,ki_gol),&
!AC!		& real(scale2,ki_gol),ep)
!AC!		B0z11(ep)= gB0(real(zip,ki_gol),&
!AC!		& real(m0,ki_gol),real(m0,ki_gol),&
!AC!		& real(scale2,ki_gol),ep)
!AC!		B0z22(ep)= gB0(real(zip,ki_gol),&
!AC!		& real(m1,ki_gol),real(m1,ki_gol),&
!AC!		& real(scale2,ki_gol),ep)
!AC!	enddo
!AC!
!AC!	call HJ111(J111, K11,m0,m1,B0p12,B0z11,B0z22)
!AC!
!AC!end subroutine golemMI2hr2
!AC!
!AC!
!AC!
!AC!subroutine golemMI1(m,scale2,MI1,cache_flag, cache_offset, scalar_cache)
!AC!
!AC!	implicit none 
!AC!	complex(ki), 			  intent(in ) :: m
!AC!	real(ki), 			  intent(in ) :: scale2
!AC!	complex(ki), dimension(-2:0),     intent(out) :: MI1
!AC!	
!AC!	logical,     intent(in   ), optional 			:: cache_flag
!AC!	integer,     intent(inout), optional 			:: cache_offset
!AC!	complex(ki), intent(inout), optional, dimension(-2:0,*) :: scalar_cache
!AC!	
!AC!	integer :: ep, cache_index
!AC!	complex(ki) :: m0
!AC!
!AC!	m0=m
!AC!	
!AC!	notfirsti=.true.
!AC!	
!AC!	MI1(-2)=czip
!AC!	if (present(cache_flag)) cache_index = lbound(scalar_cache,2)+cache_offset
!AC!	if (present(cache_flag)) then
!AC!		if (cache_flag) then
!AC!			do ep=-1,0
!AC!				MI1(ep) = scalar_cache(ep,cache_index)
!AC!			enddo
!AC!		else
!AC!			scalar_cache(-2,cache_index) = czip
!AC!			call gtrunc(1.0_ki, m0)
!AC!			do ep=-1,0
!AC!				MI1(ep) = gA0(real(m0,ki_gol),real(scale2,ki_gol),ep)
!AC!				scalar_cache(ep,cache_index) = MI1(ep)
!AC!			enddo
!AC!		end if
!AC!	else
!AC!		call gtrunc(1.0_ki, m0)
!AC!		do ep=-1,0
!AC!			MI1(ep) = gA0(real(m0,ki_gol),real(scale2,ki_gol),ep)
!AC!		enddo
!AC!	end if
!AC!	if (present(cache_flag)) cache_offset = cache_offset + 1
!AC!end subroutine golemMI1
!AC!
!AC!
!AC!pure subroutine gtrunc(ref,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10)
!AC!	implicit none
!AC!	real(ki), intent(in) :: ref
!AC!	complex(ki), intent(inout), optional :: s1,s2,s3,s4,s5,s6,s7,s8,s9,s10
!AC!	real(ki), parameter :: small = 1.0E-08_ki
!AC!	if(present(s1)) then
!AC!		if(abs(s1/ref) .lt. small) s1 = 0.0_ki
!AC!	end if
!AC!	if(present(s2)) then
!AC!		if(abs(s2/ref) .lt. small) s2 = 0.0_ki
!AC!	end if
!AC!	if(present(s3)) then
!AC!		if(abs(s3/ref) .lt. small) s3 = 0.0_ki
!AC!	end if
!AC!	if(present(s4)) then
!AC!		if(abs(s4/ref) .lt. small) s4 = 0.0_ki
!AC!	end if
!AC!	if(present(s5)) then
!AC!		if(abs(s5/ref) .lt. small) s5 = 0.0_ki
!AC!	end if
!AC!	if(present(s6)) then
!AC!		if(abs(s6/ref) .lt. small) s6 = 0.0_ki
!AC!	end if
!AC!	if(present(s7)) then
!AC!		if(abs(s7/ref) .lt. small) s7 = 0.0_ki
!AC!	end if
!AC!	if(present(s8)) then
!AC!		if(abs(s8/ref) .lt. small) s8 = 0.0_ki
!AC!	end if
!AC!	if(present(s9)) then
!AC!		if(abs(s9/ref) .lt. small) s9 = 0.0_ki
!AC!	end if
!AC!	if(present(s10)) then
!AC!		if(abs(s10/ref) .lt. small) s10 = 0.0_ki
!AC!	end if
!AC!end  subroutine gtrunc
!AC!
!AC!function gA0(m0,mu2,ep)
!AC!   implicit none
!AC!   real(ki_gol), intent(in) :: m0, mu2
!AC!   integer, intent(in) :: ep
!AC!   complex(ki_gol) :: gA0
!AC!   if(ep.eq.(-2) .or. m0.eq.0.0_ki_gol) then
!AC!      gA0 = (0.0_ki_gol, 0.0_ki_gol)
!AC!   elseif(ep.eq.(-1)) then
!AC!      gA0 = m0 * gB0(0.0_ki_gol,m0,m0,mu2,-1)
!AC!   else
!AC!      gA0 = m0 * (gB0(0.0_ki_gol,m0,m0,mu2,0) &
!AC!          &    +  gB0(0.0_ki_gol,m0,m0,mu2,-1))
!AC!   end if
!AC!end function gA0


end module mmisgolem

