**********************************************************************
        subroutine F_dd(F,p1,p2,p3,p4,p5,p6,p12,p23,p34,p45,p56,p16,
     &   p123,p234,p345,m02,m12,m22,m32,m42,m52,r2,id)
**********************************************************************
*       6-point coefficients  
*	F(i,j,k,l,m,n) = F_{0...01...12...23...34...45...5}(p1,...,m52)
*                           \___/\___/\___/\___/\___/\___/
*                             i    j    k    l    m    n  indices
*	of rank r=i+j+k+l+m+n with r <= r2
*---------------------------------------------------------------------
*       15.9.2006 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
	complex*16 F(0:rmax6,0:rmax6,0:rmax6,0:rmax6,0:rmax6,0:rmax6)
	complex*16 m02,m12,m22,m32,m42,m52,xdet_dd,ff(5),detx5
	complex*16 x5(0:5,0:5),tx5(0:5,0:5),ttx5(0:5,0:5,0:5,0:5)
	complex*16 dety5,eta5(0:5),mat(6,6),mati(6,6),vec(6)
	complex*16 detm,mat5(5,5),mati5(5,5)
	real*8 z5(5,5)
        integer r2,i,j,k,n,id,nid(0:nmax-1)

c store DD debug info
        if (id.eq.0) then
          s_DDin  = 'F_dd'
          nc_DDin = 6
          nr_DDin = 15
          ni_DDin = 2
          r_DDin(1) = p1
          r_DDin(2) = p2
          r_DDin(3) = p3
          r_DDin(4) = p4
          r_DDin(5) = p5
          r_DDin(6) = p6
          r_DDin(7) = p12
          r_DDin(8) = p23
          r_DDin(9) = p34
          r_DDin(10)= p45
          r_DDin(11)= p56
          r_DDin(12)= p16
          r_DDin(13)= p123
          r_DDin(14)= p234
          r_DDin(15)= p345
          c_DDin(1) = m02
          c_DDin(2) = m12
          c_DDin(3) = m22
          c_DDin(4) = m32
          c_DDin(5) = m42
          c_DDin(6) = m52
          i_DDin(1) = r2
          i_DDin(2) = id
        endif

        if (id.eq.0) then
          do i=0,63
            r2_aux(i)     = -1
            r20_aux(i)    = -1
            r2_new_aux(i) = -1
            dr2exp_aux(i) = 0
          enddo
        endif

c algebraic quantities
c---------------------
        q1  = p1
        q2  = p12
        q3  = p123
        q4  = p56
 	q5  = p6
        q12 = (p1  +p12 -p2  )/2d0
        q23 = (p12 +p123-p3  )/2d0
        q34 = (p123+p56 -p4  )/2d0
        q45 = (p56 +p6  -p5  )/2d0
        q13 = (p1  +p123-p23 )/2d0
        q24 = (p12 +p56 -p34 )/2d0
        q35 = (p123+p6  -p45 )/2d0
        q14 = (p1  +p56 -p234)/2d0
        q25 = (p12 +p6  -p345)/2d0
        q15 = (p1  +p6  -p16 )/2d0

c Gram and related matrices
        z5(1,1) = 2d0*q1
        z5(1,2) = 2d0*q12
        z5(1,3) = 2d0*q13
        z5(1,4) = 2d0*q14
        z5(1,5) = 2d0*q15
        z5(2,1) = z5(1,2)
        z5(2,2) = 2d0*q2
        z5(2,3) = 2d0*q23
        z5(2,4) = 2d0*q24
        z5(2,5) = 2d0*q25
        z5(3,1) = z5(1,3)
        z5(3,2) = z5(2,3)
        z5(3,3) = 2d0*q3
        z5(3,4) = 2d0*q34
        z5(3,5) = 2d0*q35
        z5(4,1) = z5(1,4)
        z5(4,2) = z5(2,4)
        z5(4,3) = z5(3,4)
        z5(4,4) = 2d0*q4
        z5(4,5) = 2d0*q45
        z5(5,1) = z5(1,5)
        z5(5,2) = z5(2,5)
        z5(5,3) = z5(3,5)
        z5(5,4) = z5(4,5)
        z5(5,5) = 2d0*q5

c Caley and related matrices
        ff(1) = q1-m12+m02
        ff(2) = q2-m22+m02
        ff(3) = q3-m32+m02
        ff(4) = q4-m42+m02
        ff(5) = q5-m52+m02

        x5(0,0) = 2d0*m02
	do 200 i=1,5
           x5(0,i) = ff(i)
           x5(i,0) = ff(i)
 	do 200 j=1,5
           x5(i,j) = z5(i,j)
200	continue
 
	do 201 i=1,6
 	do 201 j=1,6
           mat(i,j) = x5(i-1,j-1)
201	continue
         call xinverse_dd(mat,mati,detx5,6)
 	do 202 i=0,5
 	do 202 j=i,5
           tx5(i,j) = mati(j+1,i+1)*detx5
           tx5(j,i) = tx5(i,j) 
202	continue

C old version
c        do 203 i=1,5
c        do 203 k=1,5
c          ttx5(0,i,k,k) = 0d0
c          ttx5(k,k,0,i) = 0d0
c        do 203 j=1,k-1
c          ttx5(0,i,j,k) = (tx5(0,k)*tx5(i,j)-tx5(i,k)*tx5(0,j))/detx5
c          ttx5(0,i,k,j) = -ttx5(0,i,j,k)
c          ttx5(j,k,0,i) =  ttx5(0,i,j,k)
c          ttx5(k,j,0,i) =  ttx5(0,i,k,j)
203     continue

	do 210 i=1,5
 	do 210 j=1,5
	  mat5(i,j) = z5(i,j)
210	continue
	do 211 k=1,5
 	do 212 j=1,5
	  mat5(k,j) = ff(j)
212	continue
        call xinverse_dd(mat5,mati5,detm,5)
 	do 211 j=1,5
	  mat5(k,j) = z5(k,j)
          ttx5(k,k,0,j) = 0d0
          ttx5(0,j,k,k) = 0d0
 	do 211 i=k+1,5
          ttx5(k,i,0,j) = mati5(j,i)*detm
          ttx5(i,k,0,j) = -ttx5(k,i,0,j)
          ttx5(0,j,k,i) = ttx5(k,i,0,j)  
          ttx5(0,j,i,k) = ttx5(i,k,0,j)  
211	continue

c Y matrix and related quantities
        mat(1,1) = 2d0*m02
        mat(2,2) = 2d0*m12
        mat(3,3) = 2d0*m22
        mat(4,4) = 2d0*m32
        mat(5,5) = 2d0*m42
        mat(6,6) = 2d0*m52
        mat(1,2) = m02+m12-p1
        mat(2,3) = m12+m22-p2
        mat(3,4) = m22+m32-p3
        mat(4,5) = m32+m42-p4
        mat(5,6) = m42+m52-p5
        mat(1,6) = m52+m02-p6
        mat(1,3) = m02+m22-p12
        mat(2,4) = m12+m32-p23
        mat(3,5) = m22+m42-p34
        mat(4,6) = m32+m52-p45
        mat(1,5) = m42+m02-p56
        mat(2,6) = m52+m12-p16
        mat(1,4) = m02+m32-p123
        mat(2,5) = m12+m42-p234
        mat(3,6) = m22+m52-p345
	do 300 i=1,6
	do 300 j=i+1,6
          mat(j,i) = mat(i,j)
300	continue
	dety5 = xdet_dd(mat,6)

	do 301 i=0,5
	  do j=1,6
	    vec(j)     = mat(j,i+1)
	    mat(j,i+1) = 1d0
	  enddo
	  eta5(i) = xdet_dd(mat,6)/dety5
	  do j=1,6
	  mat(j,i+1) = vec(j)
	  enddo
301	continue

c set identifiers for lower-point integrals
        n = 0
        do k=0,nmax-1
          if (mod(id,2**(k+1))/2**k.eq.0) then
            nid(n) = id + 2**k
            n=n+1
          endif
          if (n.eq.6) goto 205
        enddo
205     continue

c*** Tensor reduction
c====================
	if (r2.eq.0) goto 900

	if (mode6.eq.0) then
        call Fx_dd(F,p1,p2,p3,p4,p5,p6,p12,p23,p34,p45,p56,p16,p123,
     &         p234,p345,m02,m12,m22,m32,m42,m52,eta5,tx5,ttx5,detx5,
     &	       r2,id,nid)
	elseif (mode6.eq.1) then
        call Fy_dd(F,p1,p2,p3,p4,p5,p6,p12,p23,p34,p45,p56,p16,
     &         p123,p234,p345,m02,m12,m22,m32,m42,m52,eta5,r2,id,nid)
	endif

900	continue

	end

**********************************************************************
        subroutine Fx_dd(F,p1,p2,p3,p4,p5,p6,p12,p23,p34,p45,p56,p16,
     &          p123,p234,p345,m02,m12,m22,m32,m42,m52,eta5,tx5,ttx5,
     &		detx5,r2,id,nid)
**********************************************************************
*       6-point coefficients  
*	F(i,j,k,l,m,n) = F_{0...01...12...23...34...45...5}(p1,...,m52)
*                           \___/\___/\___/\___/\___/\___/
*                             i    j    k    l    m    n  indices
*	of rank r=i+j+k+l+m+n with r <= r2
*
*	Method of A.Denner, S.Dittmaier, 
*		     NPB734 (2006) 62 [hep-ph/0509141], Section 7 
*---------------------------------------------------------------------
*       15.9.2006 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        integer r,r2,i0,i1,i2,i3,i4,i5,j,k,n,id,nid(0:nmax-1)
	complex*16 F(0:rmax6,0:rmax6,0:rmax6,0:rmax6,0:rmax6,0:rmax6)
	complex*16 Fb(0:5,0:rmax6,0:rmax6,0:rmax6,0:rmax6,0:rmax6,
     &		      0:rmax6)
	complex*16 E0(0:rmax5,0:rmax5,0:rmax5,0:rmax5,0:rmax5,0:rmax5)
	complex*16 E_1(0:rmax5,0:rmax5,0:rmax5,0:rmax5,0:rmax5)
	complex*16 E_2(0:rmax5,0:rmax5,0:rmax5,0:rmax5,0:rmax5)
	complex*16 E_3(0:rmax5,0:rmax5,0:rmax5,0:rmax5,0:rmax5)
	complex*16 E_4(0:rmax5,0:rmax5,0:rmax5,0:rmax5,0:rmax5)
	complex*16 E_5(0:rmax5,0:rmax5,0:rmax5,0:rmax5,0:rmax5)
	complex*16 m02,m12,m22,m32,m42,m52,eta5(0:5),c(5,5)
	complex*16 tx5(0:5,0:5),ttx5(0:5,0:5,0:5,0:5),detx5

        if (r2.gt.7) then
          write(*,*) 'Fx_dd not working up to rank ',r2
          call dd_set_error_code(1)
        endif

        call E0_dd(E0,p2,p3,p4,p5,p16,p23,p34,p45,p234,p345, 
     &             m12,m22,m32,m42,m52,r2-1,nid(0))
        call E_dd(E_1,p12,p3,p4,p5,p6,p123,p34,p45,p56,p345, 
     &             m02,m22,m32,m42,m52,r2-1,nid(1))
        call E_dd(E_2,p1,p23,p4,p5,p6,p123,p234,p45,p56,p16, 
     &             m02,m12,m32,m42,m52,r2-1,nid(2))
        call E_dd(E_3,p1,p2,p34,p5,p6,p12,p234,p345,p56,p16, 
     &             m02,m12,m22,m42,m52,r2-1,nid(3))
        call E_dd(E_4,p1,p2,p3,p45,p6,p12,p23,p345,p123,p16, 
     &             m02,m12,m22,m32,m52,r2-1,nid(4))
        call E_dd(E_5,p1,p2,p3,p4,p56,p12,p23,p34,p123,p234, 
     &             m02,m12,m22,m32,m42,r2-1,nid(5))

	F(0,0,0,0,0,0) = -eta5(0)*E0(0,0,0,0,0,0)-eta5(1)*E_1(0,0,0,0,0)
     &		         -eta5(2)*E_2(0,0,0,0,0) -eta5(3)*E_3(0,0,0,0,0)
     &		         -eta5(4)*E_4(0,0,0,0,0) -eta5(5)*E_5(0,0,0,0,0)

c find appropriate k for variant of Eq.(7.14)
	aux = 0d0
	do j=1,5
	  if (abs(tx5(j,0)).ge.aux) then
	    aux = abs(tx5(j,0))
	    k = j
	  endif
	enddo

	do 105 j=1,5
	do 105 n=1,5
c variant of Eq.(7.14)
	  if (k.ne.j) then
	    c(j,n) = -ttx5(k,j,0,n)/tx5(k,0)
	  else
	    c(j,n) = 0d0
	  endif
c variant of Eq.(7.15)
c	  c(j,n) = tx5(n,j)/detx5
105	continue
	
        do 100 r=1,r2

c Fbar(j>0,i1,...) from Eq.(7.13)
        do 110 i0=0,r-1,2
        do 110 i1=0,r-1-i0
        do 110 i2=0,r-1-i0-i1
        do 110 i3=0,r-1-i0-i1-i2
        do 110 i4=0,r-1-i0-i1-i2-i3
        i5 = r-1-i0-i1-i2-i3-i4
          Fb(0,i0,i1,i2,i3,i4,i5) = 0d0
        do 110 j=1,5
          Fb(j,i0,i1,i2,i3,i4,i5) = E0(i0,i1,i2,i3,i4,i5)
     &      *(-c(j,1)-c(j,2)-c(j,3)-c(j,4)-c(j,5))
          if (i1.eq.0) Fb(j,i0,i1,i2,i3,i4,i5)
     &      = Fb(j,i0,i1,i2,i3,i4,i5) + c(j,1)*E_1(i0,i2,i3,i4,i5)
          if (i2.eq.0) Fb(j,i0,i1,i2,i3,i4,i5)
     &      = Fb(j,i0,i1,i2,i3,i4,i5) + c(j,2)*E_2(i0,i1,i3,i4,i5)
          if (i3.eq.0) Fb(j,i0,i1,i2,i3,i4,i5)
     &      = Fb(j,i0,i1,i2,i3,i4,i5) + c(j,3)*E_3(i0,i1,i2,i4,i5)
          if (i4.eq.0) Fb(j,i0,i1,i2,i3,i4,i5)
     &      = Fb(j,i0,i1,i2,i3,i4,i5) + c(j,4)*E_4(i0,i1,i2,i3,i5)
          if (i5.eq.0) Fb(j,i0,i1,i2,i3,i4,i5)
     &      = Fb(j,i0,i1,i2,i3,i4,i5) + c(j,5)*E_5(i0,i1,i2,i3,i4)
110	continue

c F(i0,i1,...) from symmetrization as in Eq.(6.14)
        do 120 i0=0,r,2
        do 120 i1=0,r-i0
        do 120 i2=0,r-i0-i1
        do 120 i3=0,r-i0-i1-i2
        do 120 i4=0,r-i0-i1-i2-i3
        i5 = r-i0-i1-i2-i3-i4
          F(i0,i1,i2,i3,i4,i5) = 0d0
          if (i1.gt.0) F(i0,i1,i2,i3,i4,i5) = F(i0,i1,i2,i3,i4,i5)
     &                                   + i1*Fb(1,i0,i1-1,i2,i3,i4,i5)
          if (i2.gt.0) F(i0,i1,i2,i3,i4,i5) = F(i0,i1,i2,i3,i4,i5)
     &                                   + i2*Fb(2,i0,i1,i2-1,i3,i4,i5)
          if (i3.gt.0) F(i0,i1,i2,i3,i4,i5) = F(i0,i1,i2,i3,i4,i5)
     &                                   + i3*Fb(3,i0,i1,i2,i3-1,i4,i5)
          if (i4.gt.0) F(i0,i1,i2,i3,i4,i5) = F(i0,i1,i2,i3,i4,i5)
     &                                   + i4*Fb(4,i0,i1,i2,i3,i4-1,i5)
          if (i5.gt.0) F(i0,i1,i2,i3,i4,i5) = F(i0,i1,i2,i3,i4,i5)
     &                                   + i5*Fb(5,i0,i1,i2,i3,i4,i5-1)
          F(i0,i1,i2,i3,i4,i5) = F(i0,i1,i2,i3,i4,i5) / dfloat(r)
120     continue

100     continue

	end

**********************************************************************
        subroutine Fy_dd(F,p1,p2,p3,p4,p5,p6,p12,p23,p34,p45,p56,p16,
     &          p123,p234,p345,m02,m12,m22,m32,m42,m52,eta5,r2,id,nid)
**********************************************************************
*       6-point coefficients  
*	F(i,j,k,l,m,n) = F_{0...01...12...23...34...45...5}(p1,...,m52)
*                           \___/\___/\___/\___/\___/\___/
*                             i    j    k    l    m    n  indices
*
*	Method of A.Denner, S.Dittmaier, 
*		     NPB734 (2006) 62 [hep-ph/0509141], Appendix D
*---------------------------------------------------------------------
*       15.9.2006 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        integer r,r2,i0,i1,i2,i3,i4,i5,id,nid(0:nmax-1)
	complex*16 F(0:rmax6,0:rmax6,0:rmax6,0:rmax6,0:rmax6,0:rmax6)
	complex*16 E0(0:rmax5,0:rmax5,0:rmax5,0:rmax5,0:rmax5,0:rmax5)
	complex*16 E_1(0:rmax5,0:rmax5,0:rmax5,0:rmax5,0:rmax5)
	complex*16 E_2(0:rmax5,0:rmax5,0:rmax5,0:rmax5,0:rmax5)
	complex*16 E_3(0:rmax5,0:rmax5,0:rmax5,0:rmax5,0:rmax5)
	complex*16 E_4(0:rmax5,0:rmax5,0:rmax5,0:rmax5,0:rmax5)
	complex*16 E_5(0:rmax5,0:rmax5,0:rmax5,0:rmax5,0:rmax5)
	complex*16 m02,m12,m22,m32,m42,m52,eta5(0:5)

        call E0_dd(E0,p2,p3,p4,p5,p16,p23,p34,p45,p234,p345, 
     &             m12,m22,m32,m42,m52,r2,nid(0))
        call E_dd(E_1,p12,p3,p4,p5,p6,p123,p34,p45,p56,p345, 
     &             m02,m22,m32,m42,m52,r2,nid(1))
        call E_dd(E_2,p1,p23,p4,p5,p6,p123,p234,p45,p56,p16, 
     &             m02,m12,m32,m42,m52,r2,nid(2))
        call E_dd(E_3,p1,p2,p34,p5,p6,p12,p234,p345,p56,p16, 
     &             m02,m12,m22,m42,m52,r2,nid(3))
        call E_dd(E_4,p1,p2,p3,p45,p6,p12,p23,p345,p123,p16, 
     &             m02,m12,m22,m32,m52,r2,nid(4))
        call E_dd(E_5,p1,p2,p3,p4,p56,p12,p23,p34,p123,p234, 
     &             m02,m12,m22,m32,m42,r2,nid(5))

	do 100 r=0,r2
	do 100 i0=0,r,2
	do 100 i1=0,r-i0
 	do 100 i2=0,r-i0-i1
 	do 100 i3=0,r-i0-i1-i2
 	do 100 i4=0,r-i0-i1-i2-i3
 	i5 = r-i0-i1-i2-i3-i4
 	  F(i0,i1,i2,i3,i4,i5) = -eta5(0)*E0(i0,i1,i2,i3,i4,i5)
 	  if (i1.eq.0) F(i0,i1,i2,i3,i4,i5) 
     &	         = F(i0,i1,i2,i3,i4,i5) - eta5(1)*E_1(i0,i2,i3,i4,i5)
 	  if (i2.eq.0) F(i0,i1,i2,i3,i4,i5) 
     &	       	 = F(i0,i1,i2,i3,i4,i5) - eta5(2)*E_2(i0,i1,i3,i4,i5)
 	  if (i3.eq.0) F(i0,i1,i2,i3,i4,i5) 
     &	       	 = F(i0,i1,i2,i3,i4,i5) - eta5(3)*E_3(i0,i1,i2,i4,i5)
 	  if (i4.eq.0) F(i0,i1,i2,i3,i4,i5) 
     &	       	 = F(i0,i1,i2,i3,i4,i5) - eta5(4)*E_4(i0,i1,i2,i3,i5)
 	  if (i5.eq.0) F(i0,i1,i2,i3,i4,i5) 
     &	       	 = F(i0,i1,i2,i3,i4,i5) - eta5(5)*E_5(i0,i1,i2,i3,i4)
100	continue

	end

**********************************************************************
	subroutine Ftransmin(F,p1,p2,p3,p4,p5,p6,p12,p23,p34,p45,p56,
     &   	             p16,p123,p234,p345,rank)
**********************************************************************
*       transform 6-point coefficients to minimal momentum basis
*---------------------------------------------------------------------
*       19.3.2007 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        complex*16 F(0:rmax6,0:rmax6,0:rmax6,0:rmax6,0:rmax6,0:rmax6)
        real*8 z4(4,4),z4i(4,4),z5(4),a(4)
        integer rank,k,l,m,n
	integer i(4),j(4),h(4),ij(4),ih(4),jh(4),ijh(4)

	if (rank.le.1) return

c algebraic quantities
c---------------------
        q1  = p1
        q2  = p12
        q3  = p123
        q4  = p56
        q5  = p6
        q12 = (p1  +p12 -p2  )/2d0
        q23 = (p12 +p123-p3  )/2d0
        q34 = (p123+p56 -p4  )/2d0
        q45 = (p56 +p6  -p5  )/2d0
        q13 = (p1  +p123-p23 )/2d0
        q24 = (p12 +p56 -p34 )/2d0
        q35 = (p123+p6  -p45 )/2d0
        q14 = (p1  +p56 -p234)/2d0
        q25 = (p12 +p6  -p345)/2d0
        q15 = (p1  +p6  -p16 )/2d0

c Gram and related matrices
        z4(1,1) = 2d0*q1
        z4(1,2) = 2d0*q12
        z4(1,3) = 2d0*q13
        z4(1,4) = 2d0*q14
        z4(2,1) = z4(1,2)
        z4(2,2) = 2d0*q2
        z4(2,3) = 2d0*q23
        z4(2,4) = 2d0*q24
        z4(3,1) = z4(1,3)
        z4(3,2) = z4(2,3)
        z4(3,3) = 2d0*q3
        z4(3,4) = 2d0*q34
        z4(4,1) = z4(1,4)
        z4(4,2) = z4(2,4)
        z4(4,3) = z4(3,4)
        z4(4,4) = 2d0*q4
        call inverse_dd(z4,z4i,detz4,4)
	
        z5(1) = 2d0*q15
        z5(2) = 2d0*q25
        z5(3) = 2d0*q35
        z5(4) = 2d0*q45

	do 50 k=1,4
	  a(k) = 0d0
	do 50 l=1,4
	  a(k) = a(k) + z5(l)*z4i(l,k)
50	continue

c rank 1
	do 100 k=1,4
	  do m=1,4
          i(m) = 0
          enddo
	  i(k) = i(k) + 1
	  F(0,i(1),i(2),i(3),i(4),0) = F(0,i(1),i(2),i(3),i(4),0) 
     &			      	     + a(k)*F(0,0,0,0,0,1)
100	continue
	F(0,0,0,0,0,1) = 0d0

c rank 2
	do 200 k=1,4
	do 200 l=k,4
	  do m=1,4
          i(m) = 0
          enddo
	  i(k) = i(k) + 1
	  i(l) = i(l) + 1
	  F(0,i(1),i(2),i(3),i(4),0) = F(0,i(1),i(2),i(3),i(4),0) 
     &	  	     + 2d0*z4i(k,l)*F(2,0,0,0,0,0)
     &	      	     + a(k)*a(l)*F(0,0,0,0,0,2)
200	continue
     	F(2,0,0,0,0,0) = 0d0
     	F(0,0,0,0,0,2) = 0d0

	do 201 k=1,4
	do 201 l=k,4
	  do m=1,4
          i(m) = 0
          j(m) = 0
          enddo
	  i(k) = i(k) + 1
	  j(l) = j(l) + 1
	  do m=1,4
	  ij(m) = i(m) + j(m)
          enddo
	  F(0,ij(1),ij(2),ij(3),ij(4),0)=F(0,ij(1),ij(2),ij(3),ij(4),0) 
     &		      + a(l)*F(0,i(1),i(2),i(3),i(4),1)
     &		      + a(k)*F(0,j(1),j(2),j(3),j(4),1)
201	continue
	do 202 k=1,4
	  do m=1,4
          i(m) = 0
          enddo
	  i(k) = i(k) + 1
	  F(0,i(1),i(2),i(3),i(4),1) = 0d0
202	continue

	if (rank.le.2) return

c rank 3
	do 300 k=1,4
	do 300 l=k,4
	do 300 m=l,4
	  do n=1,4
          i(n) = 0
          enddo
	  i(k) = i(k) + 1
	  i(l) = i(l) + 1
	  i(m) = i(m) + 1
	  F(0,i(1),i(2),i(3),i(4),0) = F(0,i(1),i(2),i(3),i(4),0) 
     &	     			+a(k)*a(l)*a(m)*F(0,0,0,0,0,3)
     &	      			+2d0*a(k)*z4i(l,m)*F(2,0,0,0,0,1)
     &	      			+2d0*a(l)*z4i(k,m)*F(2,0,0,0,0,1)
     &	      			+2d0*a(m)*z4i(l,k)*F(2,0,0,0,0,1)
300	continue
     	F(0,0,0,0,0,3) = 0d0
     	F(2,0,0,0,0,1) = 0d0

	do 301 k=1,4
	do 301 l=k,4
	do 301 m=l,4
	  do n=1,4
          i(n) = 0
          j(n) = 0
          h(n) = 0
          enddo
	  i(k) = i(k) + 1
	  j(l) = j(l) + 1
	  h(m) = h(m) + 1
	  do n=1,4
	  ijh(n) = i(n) + j(n) + h(n)
          enddo
	  F(0,ijh(1),ijh(2),ijh(3),ijh(4),0) = 
     &		F(0,ijh(1),ijh(2),ijh(3),ijh(4),0) 
     &	      + a(l)*a(m)*F(0,i(1),i(2),i(3),i(4),2)
     &	      + a(k)*a(m)*F(0,j(1),j(2),j(3),j(4),2)
     &	      + a(l)*a(k)*F(0,h(1),h(2),h(3),h(4),2)
     &	      + 2d0*z4i(l,m)*F(2,i(1),i(2),i(3),i(4),0)
     &	      + 2d0*z4i(k,m)*F(2,j(1),j(2),j(3),j(4),0)
     &	      + 2d0*z4i(l,k)*F(2,h(1),h(2),h(3),h(4),0)
301	continue
	do 302 k=1,4
	  do m=1,4
          i(m) = 0
          enddo
	  i(k) = i(k) + 1
	  F(0,i(1),i(2),i(3),i(4),2) = 0d0
	  F(2,i(1),i(2),i(3),i(4),0) = 0d0
302	continue

	do 303 k=1,4
	do 303 l=k,4
	do 303 m=l,4
	  do n=1,4
          i(n) = 0
          j(n) = 0
          h(n) = 0
          enddo
	  i(k) = i(k) + 1
	  j(l) = j(l) + 1
	  h(m) = h(m) + 1
	  do n=1,4
	  ij(n)  = i(n) + j(n)
	  ih(n)  = i(n) + h(n)
	  jh(n)  = j(n) + h(n)
	  ijh(n) = i(n) + j(n) + h(n)
          enddo
	  F(0,ijh(1),ijh(2),ijh(3),ijh(4),0) = 
     &		F(0,ijh(1),ijh(2),ijh(3),ijh(4),0) 
     &	      + a(m)*F(0,ij(1),ij(2),ij(3),ij(4),1)
     &	      + a(l)*F(0,ih(1),ih(2),ih(3),ih(4),1)
     &	      + a(k)*F(0,jh(1),jh(2),jh(3),jh(4),1)
303	continue
	do 304 k=1,4
	do 304 l=1,4
	  do m=1,4
          ij(m) = 0
          enddo
	  ij(k) = ij(k) + 1
	  ij(l) = ij(l) + 1
	  F(0,ij(1),ij(2),ij(3),ij(4),1) = 0d0
304	continue

	if (rank.le.3) return

	write(*,*) 'subroutine Ftransmin not yet working for rank = ',
     &			rank
	call dd_set_error_code(1)
	
	end

