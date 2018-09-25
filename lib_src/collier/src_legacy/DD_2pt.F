**********************************************************************
        subroutine A_dd(A,Auv,xm02,r,id)
**********************************************************************
*       1-point coefficients  
*
*	A(l)  =  A_{0...0}(m02)  of rank l with l <= r
*                   \___/
*                     l indices
*
*	Auv(l) = coefficient of 1/eps in A(l),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       11.4.2006 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
	complex*16 A(0:rmax1),Auv(0:rmax1),xm02,m02
	real*8 sumn(0:rmax1)
        integer r,n,id,factn1(0:rmax1)

c scalar and vector integrals
	if (abs(xm02).gt.1d-17) then
	  m02    = xm02
	  A(0)   = m02*(deltauv+log(muv2/m02)+1d0)
	  Auv(0) = m02
	else
	  m02    = 0d0
	  A(0)   = 0d0
	  Auv(0) = 0d0
	endif
	factn1(0) = 1
	sumn(0)   = 0d0
	A(1)   = 0d0
	Auv(1) = 0d0
	
c tensor coefficients for rank l > 1
	do n=1,r/2
	  factn1(n)  = (n+1)*factn1(n-1)
	  sumn(n)    = sumn(n-1) + 1d0/(n+1)
	  A(2*n)     = (m02/2d0)**n/factn1(n)*( A(0)+m02*sumn(n) )
	  Auv(2*n)   = (m02/2d0)**n/factn1(n)* Auv(0)
	  A(2*n+1)   = 0d0
	  Auv(2*n+1) = 0d0
	enddo

	end

**********************************************************************
        subroutine B_dd(B,Buv,xp2,xm02,xm12,r1,r2,dr2,id)
**********************************************************************
*       2-point coefficients  
*	B(i,j) = B_{0...01...1}(xp2,xm02,xm12) 
*                   \___/\___/
*                     i    j  indices
*	of rank i+j = r <= min{ r2+i/2, r2+dr2 }
*	coefficients with rank r < r1 assumed to be known
*
*	Buv(i,j) = coefficient of 1/eps in B(i,j),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       11.4.2006 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        integer r1,r2,dr2,i,i0,i1,k,l,n,r,qm0,id,id0
	complex*16 ieps,ieps2,y(2),ye(2),f1,f(0:rmax2,2),sqe_dd,luv
	complex*16 A(0:rmax1),Auv(0:rmax1)
	complex*16 B(0:rmax2,0:rmax2),Buv(0:rmax2,0:rmax2)
	complex*16 m02,m12,xm02,xm12,cm2,cmp2,caux
	complex*16 inteps,z,ze
        logical regp,regm02,regm12

        rmp2(rm2) = mx2(nint(rm2*1d20))
        cmp2(cm2) = mx2(nint(dreal(cm2*1d20)))
        inteps(z,ze) = dcmplx( dreal(z),(abs(dimag(z))+abs(z)*eps2)
     &                                  *sign(1d0,dimag(ze)) )

        if (id.eq.0) then
          do i=0,3
            r2_aux(i)     = -1
            r20_aux(i)    = -1
            r2_new_aux(i) = -1
            dr2exp_aux(i) = 0
          enddo
        endif

        crit  = 1d-8
        eps   = 1d-13
        ieps  = dcmplx(0d0,eps)
        eps2  = 1d-20
        ieps2 = dcmplx(0d0,eps2)

c initialization for first call
c==============================

	if (r2_aux(id).eq.-1) then

c convert input parameters 
c-------------------------
          regp   = (abs(xp2) .gt.1d-15)
          regm02 = (abs(xm02).gt.1d-15)
          regm12 = (abs(xm12).gt.1d-15)
          p2  = xp2
          m02 = xm02
          m12 = xm12
          if (regp.or.regm02.or.regm12) then
c regular case
            if (.not.regp)   p2  = 0d0
            if (.not.regm02) m02 = 0d0
            if (.not.regm12) m12 = 0d0
	    qm0 = 0
          else
c singular case
            if ((.not.regp).and.(p2.ne.0d0))          p2  = rmp2(p2)
            if ((.not.regm02).and.(m02.ne.(0d0,0d0))) m02 = cmp2(m02)
            if ((.not.regm12).and.(m12.ne.(0d0,0d0))) m12 = cmp2(m12)
	    qm0 = 1
          endif

c auxiliary parameters
c---------------------
	  f1    = p2-m12+m02
c m0 =/= 0
	  if (m02.ne.(0d0,0d0)) then
            luv = deltauv+log(muv2/m02)
            if (p2.eq.0d0) then
              y(1) = 1d20
              if (m02.eq.m12) then
                y(2) = 1d20
              else
                y(2) = m02/(m02-m12)
                if (abs(dimag(y(2)))/abs(y(2)).lt.crit) 
     &	          y(2) = y(2) - ieps2*abs(y(2))*sign(1d0,dreal(m02-m12))
              endif
            else
              y(1)  = sqe_dd(dcmplx(p2),-p2-m02+m12,m02)
              y(2)  = m02/p2/y(1)
              ye(1) = sqe_dd(dcmplx(p2),-p2-m02+m12,m02-abs(m02)*ieps)
              ye(2) = (m02-abs(m02)*ieps)/p2/y(1)
              if (abs(y(1)-ye(1)).gt.abs(y(1)-ye(2))) then
                 caux  = ye(1)
                 ye(1) = ye(2)
                 ye(2) = caux
               endif
               if (abs(dimag(y(1)))/abs(y(1)).lt.crit) 
     &	         y(1) = inteps(y(1),ye(1))
               if (abs(dimag(y(2)))/abs(y(2)).lt.crit) 
     &	         y(2) = inteps(y(2),ye(2))
            endif
c m0 = 0, p2 =/= 0
	  elseif (p2.ne.0d0) then
            luv  = deltauv
            y(1) = 1d0-m12/p2+ieps2*sign(1d0,p2)*abs(1d0-m12/p2)
c m0 = 0, m1 =/= 0
	  elseif (m12.ne.(0d0,0d0)) then
            luv  = deltauv
            y(1) = 1d20
	  else
c m02 =  m12 = p2 = 0
            luv  = deltauv-delta1ir+log(muv2/mir2)
          endif

	else
c read cached information for repeated calls
c-------------------------------------------
	  if (r1.gt.r2_aux(id)+1) then
	    write(*,*) 'Inconsistent call of B_dd for r1 = ',r1
	    write(*,*) 'B_aux only known up to r2 = ',r2_aux(id)
	    write(*,*) 'id = ',id
	    call dd_set_error_code(1)
	  endif
	  do 50 r=0,r2_aux(id)
          do 50 i0=0,r+min(r,dr2),2
	  i1 = r-max(i0/2,i0-dr2)
	    B(i0,i1)   = B_aux(id,i0,i1) 
	    Buv(i0,i1) = Buv_aux(id,i0,i1)
50 	  continue
	  if (r2.le.r2_aux(id)) return
	  p2     = auxr(id,1)
	  qm0    = auxr(id,2)
	  y(1)   = auxc(id,1) 
	  y(2)   = auxc(id,2) 
	  m02    = auxc(id,3)
	  m12    = auxc(id,4)
	  f1     = auxc(id,5)
	  luv    = auxc(id,6)
	endif

c calculation of B(0,j) = B_{1...1}
c==================================
c m0 =/= 0
	if (m02.ne.(0d0,0d0)) then
          do 100 k=1,2
          do 100 n=r1,r2
            if (abs(y(k)).lt.10d0) then
              if (1d0-1d0/y(k).ne.(0d0,0d0)) then
                f(n,k) = (1d0-y(k)**(n+1))*log(1d0-1d0/y(k))
              else
                f(n,k) = 0d0
              endif
              do l=0,n
                f(n,k) = f(n,k) - y(k)**(n-l)/(l+1d0)
              enddo
            else
              f(n,k) = log(1d0-1d0/y(k))
              if (abs(y(k)).lt.1d20) then
                do l=n+1,n+20
                  f(n,k) = f(n,k) + y(k)**(n-l)/(l+1d0)
                enddo
              endif
            endif
100       continue
c m0 = 0 and ( p2 =/= 0 or m1 =/= 0 )
	elseif ((p2.ne.0d0).or.(m12.ne.(0d0,0d0))) then
          do 200 n=r1,r2
            if (dcmplx(p2).eq.m12) then
              f(n,1) = -log(muv2/m12) - 1d0/(n+1d0)
              f(n,2) = - 1d0/(n+1d0)
            else
              if (abs(y(1)).lt.10d0) then
                if (1d0-1d0/y(1).ne.(0d0,0d0)) then
                  f(n,1) = (1d0-y(1)**(n+1))*log(1d0-1d0/y(1))
                else
                  f(n,1) = 0d0
                endif
                do l=0,n
                  f(n,1) = f(n,1) - y(1)**(n-l)/(l+1d0)
                enddo
              else
                f(n,1) = log(1d0-1d0/y(1))
                if (abs(y(1)).lt.1d20) then
                  do l=n+1,n+20
                    f(n,1) = f(n,1) + y(1)**(n-l)/(l+1d0)
                  enddo
                endif
              endif
              f(n,2) = log((m12-p2)/muv2 - ieps2*abs((m12-p2)/muv2)) 
     &	               - 1d0/(n+1d0)
            endif
200       continue
c m02 =  m12 = p2 = 0
	else
          do 300 n=r1,r2
            f(n,1) = 0d0
            f(n,2) = 0d0
300	  continue
	endif

        do 400 n=r1,r2
	  B(0,n)   = (-1)**n/(n+1d0)*( luv - f(n,1) - f(n,2) )
	  Buv(0,n) = (-1)**n/(n+1d0)
400	continue

c calculation of B(i,j) = B_{0...01...1}
c=======================================

c set identifier for 1-point integrals
        do k=0,nmax-1
          if (mod(id,2**(k+1))/2**k.eq.0) then
            id0 = id + 2**k
	    goto 450
          endif
        enddo
450     continue
	call A_dd(A,Auv,xm12,r2-1+dr2,id0)

        do 500 r=max(1,r1),r2
        do 500 i0=2,r+min(r,dr2),2
	i1 = r-max(i0/2,i0-dr2)
	  if (qm0.eq.1) then
	    B(i0,i1)   = 0d0
	    Buv(i0,i1) = 0d0
	  else
	    Buv(i0,i1) = 1d0/2d0/(i0+i1+1)*(
     &		(-1)**i1*Auv(i0-2)+2d0*m02*Buv(i0-2,i1)
     &		+f1*Buv(i0-2,i1+1)				)
	    B(i0,i1) = 1d0/2d0/(i0+i1+1)*(
     &		(-1)**i1*A(i0-2)+2d0*m02*B(i0-2,i1)
     &		+f1*B(i0-2,i1+1)				
     &		+4d0*Buv(i0,i1)				)
	  endif
500	continue

c cache information 
	r2_aux(id) = max(r2_aux(id),r2)
	do 600 r=r1,r2
        do 600 i0=0,r+min(r,dr2),2
	i1 = r-max(i0/2,i0-dr2)
	  B_aux(id,i0,i1)   = B(i0,i1) 
	  Buv_aux(id,i0,i1) = Buv(i0,i1) 
600 	continue
	if (r1.eq.0) then
	  auxr(id,1) = p2
	  auxr(id,2) = qm0
	  auxc(id,1) = y(1) 
	  auxc(id,2) = y(2) 
	  auxc(id,3) = m02
	  auxc(id,4) = m12
	  auxc(id,5) = f1
	  auxc(id,6) = luv
	endif

	end

**********************************************************************
        subroutine B0_dd(B0,Buv0,p2,m02,m12,r1,r2,dr2,id)
**********************************************************************
*       2-point coefficients B(0)_{...} with unshifted momentum 
*---------------------------------------------------------------------
*       21.7.2006 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        integer r,r1,r2,dr2,i,i0,i1,i2,i12,id
	complex*16 B(0:rmax2,0:rmax2),B0(0:rmax2,0:rmax2,0:rmax2)
	complex*16 Buv(0:rmax2,0:rmax2),Buv0(0:rmax2,0:rmax2,0:rmax2)
	complex*16 m02,m12

        if (id.eq.0) then
          do i=0,3
            r2_aux(i)     = -1
            r20_aux(i)    = -1
            r2_new_aux(i) = -1
            dr2exp_aux(i) = 0
          enddo
        endif

c read cached information for r1>0
c---------------------------------
        if (r20_aux(id).ne.-1) then
	  if (r1.gt.r20_aux(id)+1) then
	    write(*,*) 'Inconsistent call of B0_dd for r1 = ',r1
	    write(*,*) 'B0_aux only known up to r2 = ',r20_aux(id),
     &                 ' for id =',id
	    call dd_set_error_code(1)
	  endif
	  do 50 r=0,r20_aux(id)
          do 50 i0=0,r+min(r,dr2),2
	  i12 = r-max(i0/2,i0-dr2)
	  do 50 i1=0,i12
	    i2 = i12-i1
	    B0(i0,i1,i2)   = B0_aux(id,i0,i1,i2) 
	    Buv0(i0,i1,i2) = Buv0_aux(id,i0,i1,i2)
50 	  continue
	  if (r2.le.r20_aux(id)) return
	endif

	call B_dd(B,Buv,p2,m02,m12,r1,r2,dr2,id)

        do 101 r=r1,r2
        do 101 i0=0,r+min(r,dr2),2
	  i12 = r-max(i0/2,i0-dr2)
	  i2  = i12
          B0(i0,0,i2)   = B(i0,i2)
          Buv0(i0,0,i2) = Buv(i0,i2)
	  do 101 i1=1,i12
            i2 = i12-i1
            B0(i0,i1,i2)   = -B0(i0,i1-1,i2) - B0(i0,i1-1,i2+1)
            Buv0(i0,i1,i2) = -Buv0(i0,i1-1,i2) - Buv0(i0,i1-1,i2+1)
101     continue

c cache information 
	r20_aux(id) = max(r20_aux(id),r2)
	do 600 r=r1,r2
        do 600 i0=0,r+min(r,dr2),2
	i12 = r-max(i0/2,i0-dr2)
	do 600 i1=0,i12
	  i2 = i12-i1
	  B0_aux(id,i0,i1,i2)   = B0(i0,i1,i2) 
	  Buv0_aux(id,i0,i1,i2) = Buv0(i0,i1,i2) 
600 	continue

	end

**********************************************************************
        subroutine DB_dd(DB0,DB1,xp2,xm02,xm12,id)
**********************************************************************
*       momentum derivatives of 2-point coefficients B0 and B1 
*---------------------------------------------------------------------
*       7.5.2008 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
	complex*16 DB0,DB1,m02,m12,xm02,xm12,cm2,cmp2,x1,x2,sqe_dd
	complex*16 ieps,ieps2,logp,x1e,x2e,caux,inteps,z,ze
        integer qm0,id
        logical regp,regm02,regm12

        rmp2(rm2) = mx2(nint(rm2*1d20))
        cmp2(cm2) = mx2(nint(dreal(cm2*1d20)))

        inteps(z,ze) = dcmplx( dreal(z),(abs(dimag(z))+abs(z)*eps2)
     &                                  *sign(1d0,dimag(ze)) )

	crit  = 1d-8
	eps   = 1d-13
	ieps  = dcmplx(0d0,eps)
	eps2  = 1d-20
	ieps2 = dcmplx(0d0,eps2)

c convert input parameters 
c-------------------------
        regp   = (abs(xp2) .gt.1d-15)
        regm02 = (abs(xm02).gt.1d-15)
        regm12 = (abs(xm12).gt.1d-15)
        p2  = xp2
        m02 = xm02
        m12 = xm12
        if (regp.or.regm02.or.regm12) then
c regular case
          if (.not.regp)   p2  = 0d0
          if (.not.regm02) m02 = 0d0
          if (.not.regm12) m12 = 0d0
	  qm0 = 0
        else
c singular case
          if ((.not.regp).and.(p2.ne.0d0))          p2  = rmp2(p2)
          if ((.not.regm02).and.(m02.ne.(0d0,0d0))) m02 = cmp2(m02)
          if ((.not.regm12).and.(m12.ne.(0d0,0d0))) m12 = cmp2(m12)
	  qm0 = 1
        endif

c DB0 and DB1
c------------
c*** cases with p2=0
        if (p2.eq.0d0) then
          if (m02.eq.(0d0,0d0)) then
            if(m12.eq.(0d0,0d0)) then
              DB0 = 0d0
              DB1 = 0d0
            else	
              DB0 = 1d0/2d0/m12
              DB1 = -1d0/6d0/m12
            endif
          elseif (m12.eq.(0d0,0d0)) then
            DB0 = 1d0/2d0/m02
            DB1 = -1d0/3d0/m02
          elseif (m02.eq.m12) then
            DB0 = 1d0/6d0/m02
            DB1 = -1d0/12d0/m02
          else
            DB0 = (m02**2-m12**2-2d0*m02*m12*log(m02/m12))
     &            /2d0/(m02-m12)**3
	    DB1 = -(2d0*m02**3+3d0*m02**2*m12-6d0*m02*m12**2+m12**3
     &	            +6d0*m02**2*m12*log(m12/m02))/6d0/(m02-m12)**4
          endif

c*** cases with p2=/=0 and mi=0
        elseif (m02*m12.eq.(0d0,0d0)) then
          if ((m02.eq.(0d0,0d0)).and.(m12.eq.(0d0,0d0))) then
            DB0 = -1d0/p2
            DB1 = 1d0/(2d0*p2)
          elseif (dcmplx(p2).eq.m02) then
            DB0 = -(2d0+delta1ir+log(mir2/m02))/(2d0*m02)
            DB1 = (3d0+delta1ir+log(mir2/m02))/(2d0*m02)
          elseif (dcmplx(p2).eq.m12) then
            DB0 = -(2d0+delta1ir+log(mir2/m12))/(2d0*m12)
	    DB1 = -1d0/(2d0*m12)
          elseif (m02.eq.(0d0,0d0)) then
	    logp = log(1d0-p2/m12-ieps2*abs(1d0-p2/m12))
            DB0  = -(p2+m12*logp)/p2**2
	    DB1  = (p2*(p2-2d0*m12)-2d0*m12*(m12-p2)*logp )/(2d0*p2**3)
          elseif (m12.eq.(0d0,0d0)) then
	    logp = log(1d0-p2/m02-ieps2*abs(1d0-p2/m02))
            DB0  = -(p2+m02*logp)/p2**2
	    DB1  = (2d0*m02*p2+p2**2+2d0*m02**2*logp )/(2d0*p2**3)
          endif
c*** general case
        else
	  scale2 = abs(p2)+abs(m02)+abs(m12)
          x1  = sqe_dd(dcmplx(p2),-p2+m12-m02,m02)
          x2  = m02/p2/x1
          x1e = sqe_dd(dcmplx(p2),-p2+m12-m02,m02-scale2*ieps)
          x2e = (m02-scale2*ieps)/p2/x1
          if (abs(x1-x1e).gt.abs(x1-x2e)) then
             caux = x1e
             x1e  = x2e
             x2e  = caux
          endif
          if (abs(dimag(x1))/abs(x1).lt.crit) x1 = inteps(x1,x1e)
          if (abs(dimag(x2))/abs(x2).lt.crit) x2 = inteps(x2,x2e)
          DB0 = (-1d0 + x1*(1d0-x1)/(x1-x2)*log(1d0-1d0/x1)
     &                - x2*(1d0-x2)/(x1-x2)*log(1d0-1d0/x2) )/p2
	  DB1 = -( 1d0-2d0*x1-2d0*x2 
     &	          +(2d0*x1**2*(1d0-x1)*log(1d0-1d0/x1))/(x1-x2)
     &            -(2d0*x2**2*(1d0-x2)*log(1d0-1d0/x2))/(x1-x2) 
     &	         )/(2d0*p2)
        endif

	end
