**********************************************************************
        subroutine E_dd(E,p1,p2,p3,p4,p5,p12,p23,p34,p45,p15,
     &			m02,m12,m22,m32,m42,r2,id)
**********************************************************************
*       5-point coefficients  
*	E(i,j,k,l,m) = E_{0...01...12...23...34...4}(p1,...,m42)
*                         \___/\___/\___/\___/\___/
*                           i    j    k    l    m  indices
*	of rank r=i+j+k+l+m with r <= r2
*---------------------------------------------------------------------
*       10.9.2006 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
	complex*16 E(0:rmax5,0:rmax5,0:rmax5,0:rmax5,0:rmax5)
	complex*16 m02,m12,m22,m32,m42,xdet_dd,f(4),detx4
	complex*16 x4(0:4,0:4),tx4(0:4,0:4),ttx4(0:4,0:4,0:4,0:4)
	complex*16 dety4,eta4(0:4),mat(5,5),mati(5,5),vec(5)
        complex*16 detm,mat4(4,4),mati4(4,4)
	real*8 z4(4,4),tz4(4,4),z4i(4,4)
        integer r2,i,j,k,n,id,nid(0:nmax-1)

c store DD debug info
        if (id.eq.0) then
          s_DDin  = 'E_dd'
          nc_DDin = 5
          nr_DDin = 10
          ni_DDin = 2
          r_DDin(1) = p1
          r_DDin(2) = p2
          r_DDin(3) = p3
          r_DDin(4) = p4
          r_DDin(5) = p5
          r_DDin(6) = p12
          r_DDin(7) = p23
          r_DDin(8) = p34
          r_DDin(9) = p45
          r_DDin(10)= p15
          c_DDin(1) = m02
          c_DDin(2) = m12
          c_DDin(3) = m22
          c_DDin(4) = m32
          c_DDin(5) = m42
          i_DDin(1) = r2
          i_DDin(2) = id
        endif

        if (id.eq.0) then
          do i=0,31
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
        q3  = p45
	q4  = p5
        q12 = (p1+p12-p2)/2d0
        q13 = (p1+p45-p23)/2d0
        q14 = (p1+p5-p15)/2d0
        q23 = (p45+p12-p3)/2d0
        q24 = (p5+p12-p34)/2d0
        q34 = (p5+p45-p4)/2d0

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
 	do 100 i=1,4
 	do 100 j=1,4
          tz4(i,j) = z4i(j,i)*detz4
100	continue

c Caley and related matrices
        f(1) = q1-m12+m02
        f(2) = q2-m22+m02
        f(3) = q3-m32+m02
        f(4) = q4-m42+m02

        x4(0,0) = 2d0*m02
	do 200 i=1,4
          x4(0,i) = f(i)
          x4(i,0) = f(i)
	do 200 j=1,4
          x4(i,j) = z4(i,j)
200	continue

	do 201 i=1,5
	do 201 j=1,5
          mat(i,j) = x4(i-1,j-1)
201	continue
        call xinverse_dd(mat,mati,detx4,5)
	do 202 i=0,4
	do 202 j=i,4
          tx4(i,j) = mati(j+1,i+1)*detx4
          tx4(j,i) = tx4(i,j) 
202	continue
	
C old version
c	do 203 i=1,4
c	do 203 k=1,4
c	  ttx4(0,i,k,k) = 0d0
c	  ttx4(k,k,0,i) = 0d0
c	do 203 j=1,k-1
c	  ttx4(0,i,j,k) = (tx4(0,k)*tx4(i,j)-tx4(i,k)*tx4(0,j))/detx4
c	  ttx4(0,i,k,j) = -ttx4(0,i,j,k)
c	  ttx4(j,k,0,i) =  ttx4(0,i,j,k)
c	  ttx4(k,j,0,i) =  ttx4(0,i,k,j)
203	continue

        do 210 i=1,4
        do 210 j=1,4
          mat4(i,j) = z4(i,j)
210	continue
        do 211 k=1,4
        do 212 j=1,4
          mat4(k,j) = f(j)
212	continue
        call xinverse_dd(mat4,mati4,detm,4)
        do 211 j=1,4
          mat4(k,j) = z4(k,j)
          ttx4(k,k,0,j) = 0d0
          ttx4(0,j,k,k) = 0d0
        do 211 i=k+1,4
          ttx4(k,i,0,j) = mati4(j,i)*detm
          ttx4(i,k,0,j) = -ttx4(k,i,0,j)
          ttx4(0,j,k,i) = ttx4(k,i,0,j)
          ttx4(0,j,i,k) = ttx4(i,k,0,j)
211	continue

c Y matrix and related quantities
        mat(1,1) = 2d0*m02
        mat(2,2) = 2d0*m12
        mat(3,3) = 2d0*m22
        mat(4,4) = 2d0*m32
        mat(5,5) = 2d0*m42
        mat(1,2) = m02+m12-p1
        mat(2,3) = m12+m22-p2
        mat(3,4) = m22+m32-p3
        mat(4,5) = m32+m42-p4
        mat(1,5) = m42+m02-p5
        mat(1,3) = m02+m22-p12
        mat(2,4) = m12+m32-p23
        mat(3,5) = m22+m42-p34
        mat(1,4) = m32+m02-p45
        mat(2,5) = m42+m12-p15
	do 300 i=1,5
	do 300 j=i+1,5
          mat(j,i) = mat(i,j)
300	continue
	dety4 = xdet_dd(mat,5)

	do 301 i=0,4
	  do j=1,5
	    vec(j)     = mat(j,i+1)
	    mat(j,i+1) = 1d0
	  enddo
	  eta4(i) = xdet_dd(mat,5)/dety4
	  do j=1,5
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
          if (n.eq.5) goto 205
        enddo
205     continue

c*** Tensor reduction
c====================
	if (r2.eq.0) goto 900

	if (mode5.eq.0) then
	  call Ex_dd(E,p1,p2,p3,p4,p5,p12,p23,p34,p45,p15,
     &		 m02,m12,m22,m32,m42,eta4,detx4,tx4,ttx4,r2,id,nid)
	elseif (mode5.eq.1) then
	  call Ey_dd(E,p1,p2,p3,p4,p5,p12,p23,p34,p45,p15,
     &		 m02,m12,m22,m32,m42,eta4,detz4,tz4,dety4,r2,id,nid)
	endif

900	continue

c test output
c============
        if ((outlevel.gt.1).and.(id.eq.0)) then
        do i=0,4
          write(*,*)
          write(*,*) '4pt tensor integral id = ',nid(i)
          write(*,*) '  Dacc_pave = ',acc_pave(nid(i))
          if (qmethod_new(nid(i)).ne.0)
     &    write(*,*) '  Dacc_new  = ',acc_new(nid(i)),
     &               '  method ',qmethod_new(nid(i))
        enddo
        endif

	end

**********************************************************************
        subroutine Ex_dd(E,p1,p2,p3,p4,p5,p12,p23,p34,p45,p15,
     &		     m02,m12,m22,m32,m42,eta4,detx4,tx4,ttx4,r2,id,nid)
**********************************************************************
*       5-point coefficients  
*	E(i,j,k,l,m) = E_{0...01...12...23...34...4}(p1,...,m32)
*                         \___/\___/\___/\___/\___/
*                           i    j    k    l    m  indices
*	of rank r=i+j+k+l+m with r <= r2 <= 5
*
*	Method of A.Denner, S.Dittmaier, 
*			NPB734 (2006) 62 [hep-ph/0509141], Section 6
*---------------------------------------------------------------------
*       15.9.2006 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        integer del(4,4),r2,i0,i1,i2,i3,i4,i(4),j,k,l(4),r
	integer id,nid(0:nmax-1)
        data del/1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1/
	complex*16 E(0:rmax5,0:rmax5,0:rmax5,0:rmax5,0:rmax5)
	complex*16 Eb(0:4,0:rmax5,0:rmax5,0:rmax5,0:rmax5,0:rmax5)
	complex*16 D0(0:rmax4,0:rmax4,0:rmax4,0:rmax4,0:rmax4)
	complex*16 Duv0(0:rmax4,0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 D_1(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 Duv_1(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 D_2(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 Duv_2(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 D_3(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 Duv_3(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 D_4(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 Duv_4(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
	complex*16 m02,m12,m22,m32,m42,eta4(0:4)
	complex*16 detx4,tx4(0:4,0:4),ttx4(0:4,0:4,0:4,0:4)

	if (r2.gt.5) then
	  write(*,*) 'Ex_dd not working up to rank ',r2
	  call dd_set_error_code(1)
	endif

        call D0_dd(D0,Duv0,   p2, p3, p4,p15,p23,p34,m12,m22,m32,m42,
     &		  r2-1,1,nid(0))
        call D_dd(D_1,Duv_1,p12, p3, p4, p5,p45,p34,m02,m22,m32,m42,
     &		  r2-1,1,nid(1))
        call D_dd(D_2,Duv_2, p1,p23, p4, p5,p45,p15,m02,m12,m32,m42,
     &		  r2-1,1,nid(2))
        call D_dd(D_3,Duv_3, p1, p2,p34, p5,p12,p15,m02,m12,m22,m42,
     &		  r2-1,1,nid(3))
        call D_dd(D_4,Duv_4, p1, p2, p3,p45,p12,p23,m02,m12,m22,m32,
     &		  r2-1,1,nid(4))

	E(0,0,0,0,0) = -eta4(0)*D0(0,0,0,0,0)
     &		       -eta4(1)*D_1(0,0,0,0)-eta4(2)*D_2(0,0,0,0)
     &		       -eta4(3)*D_3(0,0,0,0)-eta4(4)*D_4(0,0,0,0)

	do 100 r=1,r2

c Ebar(k>0,i1,...) from Eq.(6.12)
	do 110 i0=0,r-1,2
	do 110 i1=0,r-1-i0
	do 110 i2=0,r-1-i0-i1
	do 110 i3=0,r-1-i0-i1-i2
	i4 = r-1-i0-i1-i2-i3
	i(1) = i1
	i(2) = i2
	i(3) = i3
	i(4) = i4
	do 120 k=1,4
	  Eb(k,i0,i1,i2,i3,i4) = D0(i0,i1,i2,i3,i4)
     &	    *(-tx4(k,0)-tx4(k,1)-tx4(k,2)-tx4(k,3)-tx4(k,4))
	  if (i1.eq.0) Eb(k,i0,i1,i2,i3,i4)  
     &	    = Eb(k,i0,i1,i2,i3,i4) + tx4(k,1)*D_1(i0,i2,i3,i4)
	  if (i2.eq.0) Eb(k,i0,i1,i2,i3,i4)  
     &	    = Eb(k,i0,i1,i2,i3,i4) + tx4(k,2)*D_2(i0,i1,i3,i4)
	  if (i3.eq.0) Eb(k,i0,i1,i2,i3,i4)  
     &	    = Eb(k,i0,i1,i2,i3,i4) + tx4(k,3)*D_3(i0,i1,i2,i4)
	  if (i4.eq.0) Eb(k,i0,i1,i2,i3,i4) 
     &	    = Eb(k,i0,i1,i2,i3,i4) + tx4(k,4)*D_4(i0,i1,i2,i3)
	  do 120 j=1,4
	    if (i(j).ne.0) then
	      l(1) = i1-del(j,1)
	      l(2) = i2-del(j,2)
	      l(3) = i3-del(j,3)
	      l(4) = i4-del(j,4)
 	      Eb(k,i0,i1,i2,i3,i4) = Eb(k,i0,i1,i2,i3,i4) 
     &		+ 2d0*i(j)*( ttx4(k,1,0,j)+ttx4(k,2,0,j)
     &			    +ttx4(k,3,0,j)+ttx4(k,4,0,j) )
     &	             *D0(i0+2,l(1),l(2),l(3),l(4)) 
 	      if (l(1).eq.0) Eb(k,i0,i1,i2,i3,i4) = Eb(k,i0,i1,i2,i3,i4) 
     &		- 2d0*i(j)*ttx4(k,1,0,j)*D_1(i0+2,l(2),l(3),l(4))
 	      if (l(2).eq.0) Eb(k,i0,i1,i2,i3,i4) = Eb(k,i0,i1,i2,i3,i4) 
     &	        - 2d0*i(j)*ttx4(k,2,0,j)*D_2(i0+2,l(1),l(3),l(4))
 	      if (l(3).eq.0) Eb(k,i0,i1,i2,i3,i4) = Eb(k,i0,i1,i2,i3,i4) 
     &	        - 2d0*i(j)*ttx4(k,3,0,j)*D_3(i0+2,l(1),l(2),l(4))
 	      if (l(4).eq.0) Eb(k,i0,i1,i2,i3,i4) = Eb(k,i0,i1,i2,i3,i4) 
     &	        - 2d0*i(j)*ttx4(k,4,0,j)*D_4(i0+2,l(1),l(2),l(3))
	    endif
120	  continue
110	continue

c Ebar(i0>0,i1,...) from Eq.(6.13)
	do 130 i0=0,r-2,2
	do 130 i1=0,r-2-i0
	do 130 i2=0,r-2-i0-i1
	do 130 i3=0,r-2-i0-i1-i2
	i4 = r-2-i0-i1-i2-i3
	  Eb(0,i0,i1,i2,i3,i4) = D0(i0+2,i1,i2,i3,i4)
     &	    *(-tx4(0,1)-tx4(0,2)-tx4(0,3)-tx4(0,4))
	  if (i1.eq.0) Eb(0,i0,i1,i2,i3,i4)  
     &	    = Eb(0,i0,i1,i2,i3,i4) + tx4(0,1)*D_1(i0+2,i2,i3,i4)
	  if (i2.eq.0) Eb(0,i0,i1,i2,i3,i4)  
     &	    = Eb(0,i0,i1,i2,i3,i4) + tx4(0,2)*D_2(i0+2,i1,i3,i4)
	  if (i3.eq.0) Eb(0,i0,i1,i2,i3,i4)  
     &	    = Eb(0,i0,i1,i2,i3,i4) + tx4(0,3)*D_3(i0+2,i1,i2,i4)
	  if (i4.eq.0) Eb(0,i0,i1,i2,i3,i4) 
     &	    = Eb(0,i0,i1,i2,i3,i4) + tx4(0,4)*D_4(i0+2,i1,i2,i3)
130	continue

c E(i0,i1,...) from symmetrization as in Eqs.(6.14) and (6.15)
	do 140 i0=0,r,2
	do 140 i1=0,r-i0
	do 140 i2=0,r-i0-i1
	do 140 i3=0,r-i0-i1-i2
	i4 = r-i0-i1-i2-i3
	  E(i0,i1,i2,i3,i4) = 0d0
	  if (i0.gt.0) E(i0,i1,i2,i3,i4) = E(i0,i1,i2,i3,i4) 
     &				           + i0*Eb(0,i0-2,i1,i2,i3,i4)
	  if (i1.gt.0) E(i0,i1,i2,i3,i4) = E(i0,i1,i2,i3,i4) 
     &				           + i1*Eb(1,i0,i1-1,i2,i3,i4)
	  if (i2.gt.0) E(i0,i1,i2,i3,i4) = E(i0,i1,i2,i3,i4) 
     &				           + i2*Eb(2,i0,i1,i2-1,i3,i4)
	  if (i3.gt.0) E(i0,i1,i2,i3,i4) = E(i0,i1,i2,i3,i4) 
     &				           + i3*Eb(3,i0,i1,i2,i3-1,i4)
	  if (i4.gt.0) E(i0,i1,i2,i3,i4) = E(i0,i1,i2,i3,i4) 
     &				           + i4*Eb(4,i0,i1,i2,i3,i4-1)
	  E(i0,i1,i2,i3,i4) = E(i0,i1,i2,i3,i4) / (r*detx4)
140	continue

c algebraic extra terms according to Eqs.(6.16) and (6.21)
	if (r.eq.5) then
	  E(4,1,0,0,0) = E(4,1,0,0,0) - tx4(0,1)/(240d0*detx4)
	  E(4,0,1,0,0) = E(4,0,1,0,0) - tx4(0,2)/(240d0*detx4)
	  E(4,0,0,1,0) = E(4,0,0,1,0) - tx4(0,3)/(240d0*detx4)
	  E(4,0,0,0,1) = E(4,0,0,0,1) - tx4(0,4)/(240d0*detx4)
	endif

100	continue

	end

**********************************************************************
        subroutine Ey_dd(E,p1,p2,p3,p4,p5,p12,p23,p34,p45,p15,
     &		     m02,m12,m22,m32,m42,eta4,detz4,tz4,dety4,r2,id,nid)
**********************************************************************
*       5-point coefficients  
*	E(i,j,k,l,m) = E_{0...01...12...23...34...4}(p1,...,m32)
*                         \___/\___/\___/\___/\___/
*                           i    j    k    l    m  indices
*	of rank r=i+j+k+l+m with r <= r2 <= 4
*
*	Method of A.Denner, S.Dittmaier, NPB658 (2003) 175 [hep-ph/0212259]
*---------------------------------------------------------------------
*       10.9.2006 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        integer del(4,4),r2,i0,i1,i2,i3,i4,i(4),j,l(4),r
	integer id,nid(0:nmax-1)
        data del/1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1/
	complex*16 E(0:rmax5,0:rmax5,0:rmax5,0:rmax5,0:rmax5)
	complex*16 D0(0:rmax4,0:rmax4,0:rmax4,0:rmax4,0:rmax4)
	complex*16 Duv0(0:rmax4,0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 D_1(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 Duv_1(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 D_2(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 Duv_2(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 D_3(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 Duv_3(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 D_4(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 Duv_4(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
	complex*16 m02,m12,m22,m32,m42,eta4(0:4),dety4
	real*8 tz4(4,4)

	if (r2.gt.4) then
	  write(*,*) 'Ey_dd not working up to rank ',r2
	  call dd_set_error_code(1)
	endif

        call D0_dd(D0,Duv0,   p2, p3, p4,p15,p23,p34,m12,m22,m32,m42,
     &		  r2,1,nid(0))
        call D_dd(D_1,Duv_1,p12, p3, p4, p5,p45,p34,m02,m22,m32,m42,
     &		  r2,1,nid(1))
        call D_dd(D_2,Duv_2, p1,p23, p4, p5,p45,p15,m02,m12,m32,m42,
     &		  r2,1,nid(2))
        call D_dd(D_3,Duv_3, p1, p2,p34, p5,p12,p15,m02,m12,m22,m42,
     &		  r2,1,nid(3))
        call D_dd(D_4,Duv_4, p1, p2, p3,p45,p12,p23,m02,m12,m22,m32,
     &		  r2,1,nid(4))

	do 100 r=0,r2
	do 110 i0=0,r,2
	do 110 i1=0,r-i0
	do 110 i2=0,r-i0-i1
	do 110 i3=0,r-i0-i1-i2
	i4 = r-i0-i1-i2-i3
	i(1) = i1
	i(2) = i2
	i(3) = i3
	i(4) = i4
	  E(i0,i1,i2,i3,i4) = -eta4(0)*D0(i0,i1,i2,i3,i4)
	  if (i1.eq.0) E(i0,i1,i2,i3,i4) 
     &	       	         = E(i0,i1,i2,i3,i4) - eta4(1)*D_1(i0,i2,i3,i4)
	  if (i2.eq.0) E(i0,i1,i2,i3,i4) 
     &	       	         = E(i0,i1,i2,i3,i4) - eta4(2)*D_2(i0,i1,i3,i4)
	  if (i3.eq.0) E(i0,i1,i2,i3,i4) 
     &	       	         = E(i0,i1,i2,i3,i4) - eta4(3)*D_3(i0,i1,i2,i4)
	  if (i4.eq.0) E(i0,i1,i2,i3,i4) 
     &	       	         = E(i0,i1,i2,i3,i4) - eta4(4)*D_4(i0,i1,i2,i3)
	  do 120 j=1,4
	  l(1) = i1-del(j,1)
	  l(2) = i2-del(j,2)
	  l(3) = i3-del(j,3)
	  l(4) = i4-del(j,4)
	    if (i(j).ne.0) then
 	      E(i0,i1,i2,i3,i4) = E(i0,i1,i2,i3,i4) - 2d0*i(j)/dety4
     &	        *(tz4(j,1)+tz4(j,2)+tz4(j,3)+tz4(j,4))
     &	        *D0(i0+2,l(1),l(2),l(3),l(4)) 
 	      if (l(1).eq.0) E(i0,i1,i2,i3,i4) = E(i0,i1,i2,i3,i4) 
     &		+ 2d0*i(j)/dety4*tz4(j,1)*D_1(i0+2,l(2),l(3),l(4))
 	      if (l(2).eq.0) E(i0,i1,i2,i3,i4) = E(i0,i1,i2,i3,i4) 
     &	        + 2d0*i(j)/dety4*tz4(j,2)*D_2(i0+2,l(1),l(3),l(4))
 	      if (l(3).eq.0) E(i0,i1,i2,i3,i4) = E(i0,i1,i2,i3,i4) 
     &	        + 2d0*i(j)/dety4*tz4(j,3)*D_3(i0+2,l(1),l(2),l(4))
 	      if (l(4).eq.0) E(i0,i1,i2,i3,i4) = E(i0,i1,i2,i3,i4) 
     &	        + 2d0*i(j)/dety4*tz4(j,4)*D_4(i0+2,l(1),l(2),l(3))
	    endif
120	  continue
110	continue

	if (r.eq.4) E(4,0,0,0,0) = E(4,0,0,0,0) - detz4/dety4/48d0
100	continue

	end

**********************************************************************
        subroutine E0_dd(E0,p1,p2,p3,p4,p5,p12,p23,p34,p45,p15,
     &			 m02,m12,m22,m32,m42,r2,id)
**********************************************************************
*       5-point coefficients E(0)_{...} with unshifted momentum
*---------------------------------------------------------------------
*       15.9.2006 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
	complex*16 E(0:rmax5,0:rmax5,0:rmax5,0:rmax5,0:rmax5)
	complex*16 E0(0:rmax5,0:rmax5,0:rmax5,0:rmax5,0:rmax5,0:rmax5)
        complex*16 m02,m12,m22,m32,m42
        integer r2,r,i,i0,i1,i2,i3,i4,i5,i12345,id

        if (id.eq.0) then
          do i=0,31
            r2_aux(i)     = -1
            r20_aux(i)    = -1
            r2_new_aux(i) = -1
            dr2exp_aux(i) = 0
          enddo
        endif

        call E_dd(E,p1,p2,p3,p4,p5,p12,p23,p34,p45,p15,
     &		  m02,m12,m22,m32,m42,r2,id)

        do 101 r=0,r2
          do 101 i0=0,r,2
          i12345 = r-i0
          i1=0
          do 102 i2=0,i12345-i1
          do 102 i3=0,i12345-i1-i2
          do 102 i4=0,i12345-i1-i2-i3
            i5 = i12345-i1-i2-i3-i4
            E0(i0,0,i2,i3,i4,i5) = E(i0,i2,i3,i4,i5)
102       continue
          do 101 i1=1,i12345
          do 101 i2=0,i12345-i1
          do 101 i3=0,i12345-i1-i2
          do 101 i4=0,i12345-i1-i2-i3
            i5 = i12345-i1-i2-i3-i4
            E0(i0,i1,i2,i3,i4,i5) = -E0(i0,i1-1,i2,i3,i4,i5)
     &         - E0(i0,i1-1,i2+1,i3,i4,i5) - E0(i0,i1-1,i2,i3+1,i4,i5)
     &         - E0(i0,i1-1,i2,i3,i4+1,i5) - E0(i0,i1-1,i2,i3,i4,i5+1)
101     continue

	end

**********************************************************************
	subroutine Etransmin(E,p1,p2,p3,p4,p5,p12,p23,p34,p45,p15,rank)
**********************************************************************
*       transform 5-point coefficients to minimal momentum basis
*---------------------------------------------------------------------
*       19.3.2007 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        complex*16 E(0:rmax5,0:rmax5,0:rmax5,0:rmax5,0:rmax5)
        real*8 z4(4,4),z4i(4,4)
        integer rank,i(4),j(4),h(4),ijh(4),k,l,m,n

	if (rank.le.1) return

c algebraic quantities
c---------------------
        q1  = p1
        q2  = p12
        q3  = p45
        q4  = p5
        q12 = (p1+p12-p2)/2d0
        q13 = (p1+p45-p23)/2d0
        q14 = (p1+p5-p15)/2d0
        q23 = (p45+p12-p3)/2d0
        q24 = (p5+p12-p34)/2d0
        q34 = (p5+p45-p4)/2d0

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

c rank 2
	do 200 k=1,4
	do 200 l=k,4
	  do m=1,4
          i(m) = 0
          enddo
	  i(k) = i(k) + 1
	  i(l) = i(l) + 1
	  E(0,i(1),i(2),i(3),i(4)) = E(0,i(1),i(2),i(3),i(4)) 
     &	  	     + 2d0*z4i(k,l)*E(2,0,0,0,0)
200	continue
     	E(2,0,0,0,0) = 0d0

	if (rank.le.2) return

c rank 3
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
	  E(0,ijh(1),ijh(2),ijh(3),ijh(4)) = 
     &		E(0,ijh(1),ijh(2),ijh(3),ijh(4)) 
     &	      + 2d0*z4i(l,m)*E(2,i(1),i(2),i(3),i(4))
     &	      + 2d0*z4i(k,m)*E(2,j(1),j(2),j(3),j(4))
     &	      + 2d0*z4i(l,k)*E(2,h(1),h(2),h(3),h(4))
301	continue
	do 302 k=1,4
	  do m=1,4
          i(m) = 0
          enddo
	  i(k) = i(k) + 1
	  E(2,i(1),i(2),i(3),i(4)) = 0d0
302	continue

	if (rank.le.3) return

	write(*,*) 'subroutine Etransmin not yet working for rank = ',
     &			rank
	call dd_set_error_code(1)
	
	end

