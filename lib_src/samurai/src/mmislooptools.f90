module mmislooptools
!AC!   use precision, only: ki, ki_lt
!AC!
!AC!   use constants
!AC!   use options
!AC!   use mfunctions
!AC!   use notfirst
!AC!   implicit none
!AC!
!AC!   private
!AC!
!AC!
!AC!	interface
!AC!		function D0(p1,p2,p3,p4,s12,s23,m1,m2,m3,m4)
!AC!			use precision, only: ki_lt
!AC!			implicit none
!AC!			real(ki_lt), intent(in) :: p1,p2,p3,p4,s12,s23
!AC!			real(ki_lt), intent(in) :: m1,m2,m3,m4
!AC!			complex(ki_lt) :: D0
!AC!		end function D0
!AC!	end interface
!AC!	interface
!AC!		function C0(p1,p2,p3,m1,m2,m3)
!AC!			use precision, only: ki_lt
!AC!			implicit none
!AC!			real(ki_lt), intent(in) :: p1,p2,p3
!AC!			real(ki_lt), intent(in) :: m1,m2,m3
!AC!			complex(ki_lt) :: C0
!AC!		end function C0
!AC!	end interface
!AC!	interface
!AC!		function B0(p1,m1,m2)
!AC!			use precision, only: ki_lt
!AC!			implicit none
!AC!			real(ki_lt), intent(in) :: p1
!AC!			real(ki_lt), intent(in) :: m1,m2
!AC!			complex(ki_lt) :: B0
!AC!		end function B0
!AC!	end interface
!AC!	interface
!AC!		function B1(p1,m1,m2)
!AC!			use precision, only: ki_lt
!AC!			implicit none
!AC!			real(ki_lt), intent(in) :: p1
!AC!			real(ki_lt), intent(in) :: m1,m2
!AC!			complex(ki_lt) :: B1
!AC!		end function B1
!AC!	end interface
!AC!	interface
!AC!		function B00(p1,m1,m2)
!AC!			use precision, only: ki_lt
!AC!			implicit none
!AC!			real(ki_lt), intent(in) :: p1
!AC!			real(ki_lt), intent(in) :: m1,m2
!AC!			complex(ki_lt) :: B00
!AC!		end function B00
!AC!	end interface
!AC!	interface
!AC!		function B11(p1,m1,m2)
!AC!			use precision, only: ki_lt
!AC!			implicit none
!AC!			real(ki_lt), intent(in) :: p1
!AC!			real(ki_lt), intent(in) :: m1,m2
!AC!			complex(ki_lt) :: B11
!AC!		end function B11
!AC!	end interface
!AC!	interface
!AC!		function A0(m1)
!AC!			use precision, only: ki_lt
!AC!			implicit none
!AC!			real(ki_lt), intent(in) :: m1
!AC!			complex(ki_lt) :: A0
!AC!		end function A0
!AC!	end interface
!AC!	interface
!AC!		function D0C(p1,p2,p3,p4,s12,s23,m1,m2,m3,m4)
!AC!			use precision, only: ki_lt
!AC!			implicit none
!AC!			complex(ki_lt), intent(in) :: p1,p2,p3,p4,s12,s23
!AC!			complex(ki_lt), intent(in) :: m1,m2,m3,m4
!AC!			complex(ki_lt) :: D0C
!AC!		end function D0C
!AC!	end interface
!AC!	interface
!AC!		function C0C(p1,p2,p3,m1,m2,m3)
!AC!			use precision, only: ki_lt
!AC!			implicit none
!AC!			complex(ki_lt), intent(in) :: p1,p2,p3
!AC!			complex(ki_lt), intent(in) :: m1,m2,m3
!AC!			complex(ki_lt) :: C0C
!AC!		end function C0C
!AC!	end interface
!AC!	interface
!AC!		function B0C(p1,m1,m2)
!AC!			use precision, only: ki_lt
!AC!			implicit none
!AC!			complex(ki_lt), intent(in) :: p1
!AC!			complex(ki_lt), intent(in) :: m1,m2
!AC!			complex(ki_lt) :: B0C
!AC!		end function B0C
!AC!	end interface
!AC!	interface
!AC!		function B1C(p1,m1,m2)
!AC!			use precision, only: ki_lt
!AC!			implicit none
!AC!			complex(ki_lt), intent(in) :: p1
!AC!			complex(ki_lt), intent(in) :: m1,m2
!AC!			complex(ki_lt) :: B1C
!AC!		end function B1C
!AC!	end interface
!AC!	interface
!AC!		function B00C(p1,m1,m2)
!AC!			use precision, only: ki_lt
!AC!			implicit none
!AC!			complex(ki_lt), intent(in) :: p1
!AC!			complex(ki_lt), intent(in) :: m1,m2
!AC!			complex(ki_lt) :: B00C
!AC!		end function B00C
!AC!	end interface
!AC!	interface
!AC!		function B11C(p1,m1,m2)
!AC!			use precision, only: ki_lt
!AC!			implicit none
!AC!			complex(ki_lt), intent(in) :: p1
!AC!			complex(ki_lt), intent(in) :: m1,m2
!AC!			complex(ki_lt) :: B11C
!AC!		end function B11C
!AC!	end interface
!AC!	interface
!AC!		function A0C(m1)
!AC!			use precision, only: ki_lt
!AC!			implicit none
!AC!			complex(ki_lt), intent(in) :: m1
!AC!			complex(ki_lt) :: A0C
!AC!		end function A0C
!AC!	end interface
!AC!	interface
!AC!		function getlambda()
!AC!			use precision, only: ki_lt
!AC!			implicit none
!AC!			real(ki_lt) :: getlambda
!AC!		end function getlambda
!AC!	end interface
!AC!
!AC!
!AC!
!AC!	public :: looptools4, looptools3, looptools2, looptools2hr, looptools1
!AC!
!AC!contains
!AC!
!AC!
!AC!  subroutine looptools4(V,m,scale2,MI4, cache_flag, cache_offset, scalar_cache)
!AC!	implicit none
!AC!	real(ki),    dimension(1:6),  intent(in ) :: Vi
!AC!	complex(ki), dimension(0:3),  intent(in ) :: m
!AC!	real(ki), 		      intent(in ) :: scale2
!AC!	complex(ki), dimension(-2:0), intent(out) :: MI4
!AC!	
!AC!	logical,     intent(in   ), optional 			:: cache_flag
!AC!	integer,     intent(inout), optional 			:: cache_offset
!AC!	complex(ki), intent(inout), optional, dimension(-2:0,*) :: scalar_cache
!AC!	
!AC!	complex(ki) 		:: m0, m1, m2, m3
!AC!	real(ki) 		:: V1, V2, V3, V21, V31, V32
!AC!	integer 		:: ep, cache_index
!AC!	complex(ki) :: MI4tmp
!AC!
!AC!	if (notfirsti.eqv.(.false.)) then
!AC!		call setmudim(real(scale2, ki_lt))
!AC!		notfirsti=.true.
!AC!	endif
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
!AC!	MI4(:)=czip
!AC!	ep = -dim(0, int(getlambda()))
!AC!	if (present(cache_flag)) then
!AC!		if (cache_flag) then
!AC!			MI4tmp = scalar_cache(ep,cache_index)
!AC!		else
!AC!			call gtrunc(abs(V32)+abs(V31), &
!AC!			&   V1,V2,V3,V21,V32,V31,m0,m1,m2,m3)
!AC!			MI4tmp=D0(&
!AC!			& real(V1,ki_lt),real(V21,ki_lt),real(V32,ki_lt),&
!AC!			& real(V3,ki_lt),real(V2,ki_lt),real(V31,ki_lt),&
!AC!			& real(m0,ki_lt),real(m1,ki_lt),real(m2,ki_lt),&
!AC!			& real(m3,ki_lt))
!AC!			scalar_cache(ep,cache_index) = MI4tmp
!AC!		end if
!AC!	else
!AC!		call gtrunc(abs(V32)+abs(V31), &
!AC!		&   V1,V2,V3,V21,V32,V31,m0,m1,m2,m3)
!AC!		MI4tmp=D0(&
!AC!		& real(V1,ki_lt),real(V21,ki_lt),real(V32,ki_lt),&
!AC!		& real(V3,ki_lt),real(V2,ki_lt),real(V31,ki_lt),&
!AC!		& real(m0,ki_lt),real(m1,ki_lt),real(m2,ki_lt),&
!AC!		& real(m3,ki_lt))
!AC!	end if
!AC!	MI4(ep)=MI4tmp
!AC!	if (present(cache_flag)) cache_offset = cache_offset + 1
!AC!end subroutine add4
!AC!
!AC!subroutine add3(V,m,scale2,MI3,cache_flag, cache_offset, scalar_cache)
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
!AC!	real(ki) :: V1, V2, V3
!AC!	integer :: ep, cache_index
!AC!	
!AC!	if (notfirsti.eqv.(.false.)) then
!AC!		call setmudim(real(scale2, ki_lt))
!AC!		notfirsti=.true.
!AC!	endif
!AC!
!AC!	m0 = m(0)
!AC!	m1 = m(1)
!AC!	m2 = m(2)
!AC!	V1 = V(1)
!AC!	V2 = V(2)
!AC!	V3 = V(3)
!AC!
!AC!	if (present(cache_flag)) cache_index = lbound(scalar_cache,2)+cache_offset
!AC!	MI3(:) = 0
!AC!	call gtrunc(abs(V1)+abs(V2)+abs(V3),V1,V2,V3,m0,m1,m2)
!AC!	ep = -dim(0, int(getlambda()))
!AC!	if (present(cache_flag)) then
!AC!		if (cache_flag) then
!AC!			MI3tmp = scalar_cache(ep,cache_index)
!AC!		else
!AC!			MI3tmp=C0(&
!AC!			&  real(V1,ki_lt),real(V2,ki_lt),real(V3,ki_lt),&
!AC!			&  real(m0,ki_lt),real(m1,ki_lt),real(m2,ki_lt))
!AC!			scalar_cache(ep,cache_index) = MI3tmp
!AC!		end if
!AC!	else
!AC!		MI3tmp=C0(&
!AC!		&  real(V1,ki_lt),real(V2,ki_lt),real(V3,ki_lt),&
!AC!		&  real(m0,ki_lt),real(m1,ki_lt),real(m2,ki_lt))
!AC!	end if
!AC!	MI3(ep)=MI3tmp 
!AC!	if (present(cache_flag)) cache_offset = cache_offset + 1
!AC!end subroutine add3
!AC!
!AC!subroutine add2(K11,m,scale2,MI2J0,MI2J1,MI2J00,MI2J01,MI2J11,&
!AC!                        & cache_flag, cache_offset, scalar_cache)
!AC!	implicit none
!AC!	
!AC!	real(ki), 		      intent(in ) :: K11
!AC!	complex(ki), dimension(0:1),  intent(in ) :: m
!AC!	real(ki), 		      intent(in ) :: scale2
!AC!        complex(ki), dimension(-2:0), intent(out) :: MI2J0, MI2J1, MI2J00, MI2J01, MI2J11
!AC!
!AC!	
!AC!	logical,     intent(in   ), optional 			:: cache_flag
!AC!	integer,     intent(inout), optional 			:: cache_offset
!AC!	complex(ki), intent(inout), optional, dimension(-2:0,*) :: scalar_cache
!AC!	
!AC!	complex(ki) :: m0, m1
!AC!	integer     :: ep, cache_index
!AC!
!AC!	if (notfirsti.eqv.(.false.)) then
!AC!		call setmudim(real(scale2, ki_lt))
!AC!		notfirsti=.true.
!AC!	endif
!AC!
!AC!	
!AC!	m0=m(0)
!AC!	m1=m(1)
!AC!	
!AC!	if (present(cache_flag)) cache_index = lbound(scalar_cache,2)+cache_offset
!AC!	ep = -dim(0, int(getlambda()))
!AC!	MI2J00(:) = 0.0_ki_lt
!AC!	MI2J11(:) = 0.0_ki_lt
!AC!	MI2J0(:) = 0.0_ki_lt
!AC!	MI2J1(:) = 0.0_ki_lt
!AC!	if (present(cache_flag)) then
!AC!		if (cache_flag) then
!AC!			MI2J0(:)  = scalar_cache(:,cache_index+0)
!AC!			MI2J1(:)  = scalar_cache(:,cache_index+1)
!AC!			MI2J01(:) = scalar_cache(:,cache_index+2)
!AC!			MI2J11(:) = scalar_cache(:,cache_index+3)
!AC!			MI2J00(:) = scalar_cache(:,cache_index+4)
!AC!		else
!AC!			call gtrunc(abs(K11)+1.0_ki, K11,m0,m1)
!AC!			MI2J00(ep) = B0(real(K11,ki_lt),&
!AC!			& real(m0,ki_lt),real(m0,ki_lt))
!AC!			MI2J11(ep) = B0(real(K11,ki_lt),&
!AC!			& real(m1,ki_lt),real(m1,ki_lt))
!AC!			MI2J01(ep) = B0(real(K11,ki_lt),&
!AC!			& real(m0,ki_lt),real(m1,ki_lt))
!AC!			MI2J0(ep) = A0(real(m0,ki_lt))
!AC!			MI2J1(ep) = A0(real(m1,ki_lt))
!AC!			scalar_cache(:,cache_index+0) = J0(:)
!AC!			scalar_cache(:,cache_index+1) = J1(:)
!AC!			scalar_cache(:,cache_index+2) = J01(:)
!AC!			scalar_cache(:,cache_index+3) = J11(:)
!AC!			scalar_cache(:,cache_index+4) = J00(:)
!AC!		end if
!AC!	else
!AC!		call gtrunc(abs(K11)+1.0_ki, K11,m0,m1)
!AC!		MI2J00(ep) = B0(real(K11,ki_lt),&
!AC!		& real(m0,ki_lt),real(m0,ki_lt))
!AC!		 MI2J11(ep) = B0(real(K11,ki_lt),&
!AC!		& real(m1,ki_lt),real(m1,ki_lt))
!AC!		 MI2J01(ep) = B0(real(K11,ki_lt),&
!AC!		& real(m0,ki_lt),real(m1,ki_lt))
!AC!		 MI2J0(ep)  = A0(real(m0,ki_lt))
!AC!		 MI2J1(ep)  = A0(real(m1,ki_lt))
!AC!	end if
!AC!	if (present(cache_flag)) cache_offset = cache_offset + 5
!AC!end subroutine looptools2
!AC!
!AC!subroutine looptools2hr(K11,m,scale2,J111)
!AC!	implicit none
!AC!	real(ki),   		      intent(in ) :: K11
!AC!	complex(ki),dimension(0:1)    intent(in ) :: m
!AC!	real(ki),   		      intent(in ) :: scale2
!AC!	complex(ki), dimension(-2:0), intent(out) :: J111 
!AC!
!AC!	integer :: ep
!AC!	complex(ki) :: m0,m1
!AC!
!AC!	m0=m(0)
!AC!	m1=m(1)
!AC!
!AC!	print 'error looptools2hr, higher rank not yet supported!'
!AC!	stop
!AC!
!AC!end subroutine looptools2hr
!AC!
!AC!
!AC!
!AC!subroutine add1(m0,scale2,MI1,cache_flag, cache_offset, scalar_cache)
!AC!	implicit none 
!AC!	complex(ki),			  intent(in ) :: m0
!AC!	real(ki), 			  intent(in ) :: scale2
!AC!	complex(ki), dimension(-2:0),     intent(out) :: MI1
!AC!	
!AC!	logical,     intent(in   ), optional 			:: cache_flag
!AC!	integer,     intent(inout), optional 			:: cache_offset
!AC!	complex(ki), intent(inout), optional, dimension(-2:0,*) :: scalar_cache
!AC!	
!AC!	integer     :: ep, cache_index
!AC!	complex(ki), dimension(-2:0) :: MI1tmp
!AC!	
!AC!	if (notfirsti.eqv.(.false.)) then
!AC!		call setmudim(real(scale2, ki_lt))
!AC!		notfirsti=.true.
!AC!	endif
!AC!
!AC!	if (present(cache_flag)) cache_index = lbound(scalar_cache,2)+cache_offset
!AC!	MI1(:)=czip
!AC!	ep = -dim(0, int(getlambda()))
!AC!	if (present(cache_flag)) then
!AC!		if (cache_flag) then
!AC!			MI1tmp = scalar_cache(ep,cache_index)
!AC!		else
!AC!			MI1tmp = A0(real(m0,ki_lt))
!AC!			scalar_cache(ep,cache_index) = ctmp
!AC!		end if
!AC!	else
!AC!		MI1tmp = A0(real(m0,ki_lt))
!AC!	end if
!AC!	MI1(ep)=MItmp
!AC!	
!AC!	if (present(cache_flag)) cache_offset = cache_offset + 1
!AC!end subroutine add1
!AC!
!AC!
!AC!
end module mmislooptools

