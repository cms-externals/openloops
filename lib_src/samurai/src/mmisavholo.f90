module mmisavholo 
    	use precision, only: ki
    	use avh_olo_dp_kinds, only: ki_avh => kindr2
    	use constants
    	use options
    	use mfunctions
    	use notfirst
    	use mmishighrank
    	implicit none
    
    	public :: avholo4, avholo3, avholo2, avholo2hr, avholo1
    
    contains
    
    
    subroutine avholo4(V,m,scale2,MI4,cache_flag,cache_offset,scalar_cache)
    	use avh_olo, only: olo_d0, olo_scale
    	implicit none
    	real(ki),    dimension(1:6),  intent(in ) :: V
    	complex(ki), dimension(0:3),  intent(in ) :: m
    	real(ki), 		      intent(in ) :: scale2
    	complex(ki), dimension(-2:0), intent(out) :: MI4
    	
    	logical,     intent(in   ), optional 			:: cache_flag
    	integer,     intent(inout), optional 			:: cache_offset
    	complex(ki), intent(inout), optional, dimension(-2:0,*) :: scalar_cache
    	
    	complex(ki) :: m0, m1, m2, m3
    	real(ki)    :: V1, V2, V3, V21, V31, V32
    	integer     :: ep, cache_index
    	complex(ki_avh), dimension(0:2) :: vald0
    
    	if (notfirsti.eqv.(.false.)) then
    		call olo_scale(real(sqrt(scale2),ki_avh))
    		notfirsti=.true.
    	endif
    
    	m0  = m(0)
    	m1  = m(1)
    	m2  = m(2)
    	m3  = m(3)
    	V1  = V(1)
    	V2  = V(2)
    	V3  = V(3)
    	V21 = V(4)
    	V31 = V(5)
    	V32 = V(6)
    
    	if (present(cache_flag)) cache_index = lbound(scalar_cache,2)+cache_offset
    
    	if (present(cache_flag)) then
    		if (cache_flag) then
    			vald0(0) = scalar_cache( 0,cache_index)
    			vald0(1) = scalar_cache(-1,cache_index)
    			vald0(2) = scalar_cache(-2,cache_index)
    		else
    			call olo_d0(vald0,&
    			& cmplx(V1,0.0_ki_avh,ki_avh), &
    			& cmplx(V21,0.0_ki_avh,ki_avh), &
    			& cmplx(V32,0.0_ki_avh,ki_avh), &
    			& cmplx(V3,0.0_ki_avh,ki_avh), &
    			& cmplx(V2,0.0_ki_avh,ki_avh), &
    			& cmplx(V31,0.0_ki_avh,ki_avh), &
    			& cmplx(real(m0,ki_avh),aimag(m0),ki_avh),&
    			& cmplx(real(m1,ki_avh),aimag(m1),ki_avh),&
    			& cmplx(real(m2,ki_avh),aimag(m2),ki_avh),&
    			& cmplx(real(m3,ki_avh),aimag(m3),ki_avh))
    			scalar_cache( 0,cache_index) = vald0(0)
    			scalar_cache(-1,cache_index) = vald0(1)
    			scalar_cache(-2,cache_index) = vald0(2)
    		end if
    	else
    		call olo_d0(vald0,&
    		& cmplx(V1,0.0_ki_avh,ki_avh), &
    		& cmplx(V21,0.0_ki_avh,ki_avh), &
    		& cmplx(V32,0.0_ki_avh,ki_avh), &
    		& cmplx(V3,0.0_ki_avh,ki_avh), &
    		& cmplx(V2,0.0_ki_avh,ki_avh), &
    		& cmplx(V31,0.0_ki_avh,ki_avh), &
    		& cmplx(real(m0,ki_avh),aimag(m0),ki_avh),&
    		& cmplx(real(m1,ki_avh),aimag(m1),ki_avh),&
    		& cmplx(real(m2,ki_avh),aimag(m2),ki_avh),&
    		& cmplx(real(m3,ki_avh),aimag(m3),ki_avh))
    	end if
    	do ep=-2,0
    		MI4(ep)=vald0(-ep) 
    	enddo
    	if (present(cache_flag)) cache_offset = cache_offset + 1
    end subroutine avholo4
    
    
    subroutine avholo3(V,m,scale2,MI3,cache_flag, cache_offset, scalar_cache)
    	use avh_olo, only: olo_c0, olo_scale
    	implicit none
    	real(ki),    dimension(3),    intent(in ) :: V
    	complex(ki), dimension(0:2),  intent(in ) :: m
    	real(ki), 		      intent(in ) :: scale2
    	complex(ki), dimension(-2:0), intent(out) :: MI3
    	
    	logical,     intent(in   ), optional 			:: cache_flag
    	integer,     intent(inout), optional 			:: cache_offset
    	complex(ki), intent(inout), optional, dimension(-2:0,*) :: scalar_cache
    	
    	complex(ki) :: m0, m1, m2
    	real(ki)    :: V1, V2, V3
    	complex(ki_avh), dimension(0:2) :: valc0
    	integer     :: ep, cache_index
    	
    	if (notfirsti.eqv.(.false.)) then
    		call olo_scale(real(sqrt(scale2),ki_avh))
    		notfirsti=.true.
    	endif
    
    	m0 = m(0)
    	m1 = m(1)
    	m2 = m(2)
    	V1 = V(1)
    	V2 = V(2)
    	V3 = V(3)
    
    	if (present(cache_flag)) cache_index = lbound(scalar_cache,2)+cache_offset
    
    	if (present(cache_flag)) then
    		if (cache_flag) then
    			valc0(0) = scalar_cache( 0,cache_index)
    			valc0(1) = scalar_cache(-1,cache_index)
    			valc0(2) = scalar_cache(-2,cache_index)
    		else
    			call olo_c0(valc0,&
    			& cmplx(V1,0.0_ki_avh,ki_avh), &
    			& cmplx(V2,0.0_ki_avh,ki_avh), &
    			& cmplx(V3,0.0_ki_avh,ki_avh), &
    			& cmplx(real(m0,ki_avh),aimag(m0),ki_avh),&
    			& cmplx(real(m1,ki_avh),aimag(m1),ki_avh),&
    			& cmplx(real(m2,ki_avh),aimag(m2),ki_avh))
    			scalar_cache( 0,cache_index) = valc0(0)
    			scalar_cache(-1,cache_index) = valc0(1)
    			scalar_cache(-2,cache_index) = valc0(2)
    		end if
    	else
    		call olo_c0(valc0,&
    		& cmplx(V1,0.0_ki_avh,ki_avh), &
    		& cmplx(V2,0.0_ki_avh,ki_avh), &
    		& cmplx(V3,0.0_ki_avh,ki_avh), &
    		& cmplx(real(m0,ki_avh),aimag(m0),ki_avh),&
    		& cmplx(real(m1,ki_avh),aimag(m1),ki_avh),&
    		& cmplx(real(m2,ki_avh),aimag(m2),ki_avh))
    	end if
    	do ep=-2,0
    	   MI3(ep) = valc0(-ep) 
    	enddo
    	if (present(cache_flag)) cache_offset = cache_offset + 1
    end subroutine avholo3
    
    subroutine avholo2(K11,m,scale2,MI2a,MI2b,MI2c,MI2d,MI2e,cache_flag, cache_offset, scalar_cache)
    
    	use avh_olo, only: olo_b11, olo_scale
    	implicit none
    	
    	real(ki),    		      intent(in) :: K11
    	complex(ki), dimension(0:1),  intent(in) :: m
    	real(ki),    		      intent(in) :: scale2
    	complex(ki), dimension(-2:0), intent(out) :: MI2a, MI2b, MI2c, MI2d, MI2e
    
    	
    	logical,     intent(in   ), optional 			:: cache_flag
    	integer,     intent(inout), optional 			:: cache_offset
    	complex(ki), intent(inout), optional, dimension(-2:0,*) :: scalar_cache
    	
    	complex(ki) :: m0, m1
    	integer     :: ep, cache_index
    	complex(ki_avh), dimension(0:2) :: scf2, scf1, scf0, scf 
    
    	if (notfirsti.eqv.(.false.)) then
    		call olo_scale(real(sqrt(scale2),ki_avh))
    		notfirsti=.true.
    	endif
    
    	m0=m(0)
    	m1=m(1)
    	
    	if (present(cache_flag)) cache_index = lbound(scalar_cache,2)+cache_offset
    
    	if (present(cache_flag)) then
    		if (cache_flag) then
    			scf(:)  = scalar_cache(:,cache_index+0)
    			scf0(:) = scalar_cache(:,cache_index+1)
    			scf1(:) = scalar_cache(:,cache_index+2)
    			scf2(:) = scalar_cache(:,cache_index+3)
    		else
    			call olo_b11(scf2,scf0,scf1,scf,&
    			& cmplx(K11,0.0_ki_avh,ki_avh), &
    			& cmplx(real(m0,ki_avh),aimag(m0),ki_avh),&
    			& cmplx(real(m1,ki_avh),aimag(m1),ki_avh))
    			scalar_cache(:,cache_index+0) = scf(:)
    			scalar_cache(:,cache_index+1) = scf0(:)
    			scalar_cache(:,cache_index+2) = scf1(:)
    			scalar_cache(:,cache_index+3) = scf2(:)
    		end if
    	else
    		call olo_b11(scf2,scf0,scf1,scf,&
    		& cmplx(K11,0.0_ki_avh,ki_avh), &
    		& cmplx(real(m0,ki_avh),aimag(m0),ki_avh),&
    		& cmplx(real(m1,ki_avh),aimag(m1),ki_avh))
    	end if
    		MI2a(0) = scf (0)
    		MI2b(0) = scf1(0)
    		MI2c(0) = scf2(0)
    	do ep=1,2
    		MI2a(-ep) = scf (ep)
    		MI2b(-ep) = scf1(ep)
    		MI2c(-ep) = scf2(ep)
    	enddo
    	MI2d(:) = czip
    	MI2e(:) = czip
    	if (present(cache_flag)) cache_offset = cache_offset + 5
    end subroutine avholo2
    
    subroutine avholo2hr(K11,m,scale2,J111)
    	use avh_olo, only: olo_b11
    	implicit none
    	real(ki),                     intent(in ) :: K11
    	complex(ki), dimension( 0:1), intent(in ) :: m
    	real(ki),                     intent(in ) :: scale2
    	complex(ki), dimension(-2:0), intent(out) :: J111
    
    	integer :: ep
    	complex(ki),    dimension(-2:0) :: B0p12, B0z11, B0z22
    	complex(ki_avh),dimension( 0:2) :: dscf2, dscf1, dscf0, dscf 
    	complex(ki) :: m0,m1
    
    	m0=m(0)
    	m1=m(1)
    
    	call olo_b11(dscf2,dscf0,dscf1,dscf,&
    	& real(K11,ki_avh),real(m0,ki_avh),real(m1,ki_avh))
    	B0p12(-2) = dscf(2)
    	B0p12(-1) = dscf(1)
    	B0p12(0)  = dscf(0)
    	call olo_b11(dscf2,dscf0,dscf1,dscf,&
    	& real(zip,ki_avh),real(m0,ki_avh),real(m0,ki_avh))
    	B0z11(-2) = dscf(2)
    	B0z11(-1) = dscf(1)
    	B0z11(0)  = dscf(0)
    	call olo_b11(dscf2,dscf0,dscf1,dscf,&
    	& real(zip,ki_avh),real(m1,ki_avh),real(m1,ki_avh))
    	B0z22(-2) = dscf(2)
    	B0z22(-1) = dscf(1)
    	B0z22(0)  = dscf(0)
    
    	call HJ111(J111, K11,m0,m1,B0p12,B0z11,B0z22)
    end subroutine avholo2hr
    
    
    subroutine avholo1(m,scale2,MI1,cache_flag, cache_offset, scalar_cache)
    	use avh_olo, only: olo_a0, olo_scale
    	implicit none 
    	complex(ki),			  intent(in ) :: m
    	complex(ki), dimension(-2:0),     intent(out) :: MI1
    	real(ki), 			  intent(in ) :: scale2
    	
    	logical,     intent(in   ), optional 			:: cache_flag
    	integer,     intent(inout), optional 			:: cache_offset
    	complex(ki), intent(inout), optional, dimension(-2:0,*) :: scalar_cache
    	
    	integer     :: j1
    	complex(ki) :: m0
    	complex(ki_avh), dimension(0:2) :: vala0
    	integer     :: ep, cache_index
    	
    	if (notfirsti.eqv.(.false.)) then
    		call olo_scale(real(sqrt(scale2),ki_avh))
    		notfirsti=.true.
    	endif
    	
    	m0=m
    	
      1   Format(A3,I2,A1,I2,A5,D24.15,A1,D24.15,A3)
    
    	if (present(cache_flag)) cache_index = lbound(scalar_cache,2)+cache_offset
    
    	if (present(cache_flag)) then
    		if (cache_flag) then
    			vala0(0) = scalar_cache( 0,cache_index)
    			vala0(1) = scalar_cache(-1,cache_index)
    			vala0(2) = scalar_cache(-2,cache_index)
    		else
    			call olo_a0(vala0,cmplx(real(m0,ki_avh),aimag(m0),ki_avh))
    			scalar_cache( 0,cache_index) = vala0(0)
    			scalar_cache(-1,cache_index) = vala0(1)
    			scalar_cache(-2,cache_index) = vala0(2)
    		end if
    	else
    		call olo_a0(vala0,cmplx(real(m0,ki_avh),aimag(m0),ki_avh))
    	end if
    	do ep=-2,0
    		MI1(ep)= vala0(-ep) 
    	enddo
    	
    	if (present(cache_flag)) cache_offset = cache_offset + 1
    end subroutine avholo1
    
    
    
end module mmisavholo 

