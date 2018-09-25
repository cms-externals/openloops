












!!
!!  File DD_2pt.F is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!





        module dd_2pt_qp
        use dd_global_qp
        use dd_aux_qp
        complex(rk), allocatable, dimension(:,:,:) :: B_cache, Buv_cache
        contains

**********************************************************************
        subroutine A_dd(A,Auv,xm02,r,id)
**********************************************************************
*       1-point coefficients  
*
*       A(l)  =  A_{0...0}(m02)  of rank 2l with 2l <= r
*                   \___/
*                    2l indices
*
*       Auv(l) = coefficient of 1/eps in A(l),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       11.4.2006 Stefan Dittmaier
**********************************************************************
        implicit real(rk) (a-z)

c local variables
        integer r,n,id,factn1(0:r/2)
        complex(rk) A(0:r/2),Auv(0:r/2),xm02,m02
        real(rk) sumn(0:r/2)

        if (id.eq.0) then
          nmaster   = 1
          r2master  = r
          accflag   = 0
          errflag   = 0
          stopflag  = 0
        endif

c scalar and vector integrals
        if (abs(xm02).gt.1.e-17_rk) then
          m02    = xm02
          A(0)   = m02*(deltauv+log(muv2/m02)+1._rk)
          Auv(0) = m02
        else
          m02    = 0._rk
          A(0)   = 0._rk
          Auv(0) = 0._rk
        endif
        factn1(0) = 1
        sumn(0)   = 0._rk
        
c tensor coefficients for rank 2l > 0
        do n=1,r/2
          factn1(n)  = (n+1)*factn1(n-1)
          sumn(n)    = sumn(n-1) + 1._rk/(n+1)
          A(n)     = (m02/2._rk)**n/factn1(n)*( A(0)+m02*sumn(n) )
          Auv(n)   = (m02/2._rk)**n/factn1(n)* Auv(0)
        enddo

c accuracy estimate
c==================
        if (id.lt.2**nmaster) then
          do n=0,r/2
            resaccrel(id,2*n)    = dprec_dd
            resaccrel(id,2*n+1)  = 0._rk
            resaccabs(id,2*n)    = resaccrel(id,2*n)*abs(A(n))
            resaccabs(id,2*n+1)  = 0._rk
            resaccrel2(id,2*n)   = resaccrel(id,2*n)   
            resaccrel2(id,2*n+1) = resaccrel(id,2*n+1) 
            resaccabs2(id,2*n)   = resaccabs(id,2*n)   
            resaccabs2(id,2*n+1) = resaccabs(id,2*n+1) 
          enddo
        endif

        end

**********************************************************************
        subroutine B_dd(B,Buv,xp2,xm02,xm12,r2,id)
**********************************************************************
*       2-point coefficients  
*       B(i,j) = B_{0...01...1}(xp2,xm02,xm12) 
*                   \___/\___/
*                    2i    j  indices
*       rank r=i+j
*       
*                (i,j)
*       r2=0:    (0,0)
*       r2=1:    (2,0), (0,1)
*       r2=2:    (4,0), (2,1), (0,2)
*       ...
*       r2:      (2*r2,0), (2*r2-2,1), ... (0,r2)
*
*       Buv(i,j) = coefficient of 1/eps in B(i,j),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       11.4.2006 Stefan Dittmaier
**********************************************************************


        implicit real(rk) (a-z)

c local variables
        integer r2,i0,i1,k,l,n,r,qm0,id,id0
        complex(rk) ieps,ieps2,y(2),ye(2),f1,f(0:r2,2),luv
        complex(rk) A(0:max(r2,1)),Auv(0:max(r2,1))
        complex(rk) B(0:r2,0:r2),Buv(0:r2,0:r2)
        complex(rk) m02,m12,xm02,xm12,cm2,cmp2,caux
        complex(rk) inteps,z,ze
        logical regp,regm02,regm12


        rmp2(rm2) = mx2(nint(rm2*1.e20_rk))
        cmp2(cm2) = mx2(nint(real(cm2*1.e20_rk)))
        inteps(z,ze) = acmplx( real(z),(abs(aimag(z))+abs(z)*eps2)
     &                                  *sign(1._rk,aimag(ze)) )

        if(r2.lt.0) then
          write(*,*) 'B_dd called with r2<0. Fix it! r2 = ',r2
        endif

        if (id.eq.0) then
          nmaster   = 2
          r2master  = r2
          accflag   = 0
          errflag   = 0
          stopflag  = 0
          r2_aux(0)     = -1
          r2_new_aux(0) = -1
          do r=0,r2  
            resaccrel(0,r)  = 0._rk
            resaccabs(0,r)  = 0._rk
            resaccrel2(0,r) = 0._rk
            resaccabs2(0,r) = 0._rk
          enddo
        endif

        if (r2.gt.rmax2) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'B_dd called for rank r2 =',r2
            write(outchannel,*) 'rmax2 =',rmax2,' too small'
            if (cout.eq.coutmax) call DDlastmessage()
            cout = cout+1
          endif
          stopflag = min(-9,stopflag)
        endif

        crit  = 1.e-8_rk
        eps   = 1.e-13_rk
        ieps  = acmplx(0._rk,eps)
        eps2  = 1.e-20_rk
        ieps2 = acmplx(0._rk,eps2)

c initialization for first call
c==============================

        if ((r2_aux(id).eq.-1).or.(id.ge.2**nmaster)) then


c convert input parameters 
c-------------------------
          regp   = (abs(xp2) .gt.1.e-15_rk)
          regm02 = (abs(xm02).gt.1.e-15_rk)
          regm12 = (abs(xm12).gt.1.e-15_rk)
          p2  = xp2
          m02 = xm02
          m12 = xm12
          if (regp.or.regm02.or.regm12) then
c regular case
            if (.not.regp)   p2  = 0._rk
            if (.not.regm02) m02 = 0._rk
            if (.not.regm12) m12 = 0._rk
            qm0 = 0
          else
c singular case
            if ((.not.regp).and.(p2.ne.0._rk))          p2  = rmp2(p2)
            if ((.not.regm02).and.(m02.ne.(0._rk,0._rk))) m02 = cmp2(m02)
            if ((.not.regm12).and.(m12.ne.(0._rk,0._rk))) m12 = cmp2(m12)
            qm0 = 1
          endif

c auxiliary parameters
c---------------------
          f1    = p2-m12+m02
c m0 =/= 0
          if (m02.ne.(0._rk,0._rk)) then
            luv = deltauv+log(muv2/m02)
            if (p2.eq.0._rk) then
              y(1) = 1.e20_rk
              if (m02.eq.m12) then
                y(2) = 1.e20_rk
              else
                y(2) = m02/(m02-m12)
                if (abs(aimag(y(2)))/abs(y(2)).lt.crit)
     &            y(2) = y(2) - ieps2*abs(y(2))*sign(1._rk,real(m02-m12))
              endif
            else
              y(1)  = sqe_dd(acmplx(p2),-p2-m02+m12,m02)
              y(2)  = m02/p2/y(1)
              ye(1) = sqe_dd(acmplx(p2),-p2-m02+m12,m02-abs(m02)*ieps)
              ye(2) = (m02-abs(m02)*ieps)/p2/y(1)
              if (abs(y(1)-ye(1)).gt.abs(y(1)-ye(2))) then
                 caux  = ye(1)
                 ye(1) = ye(2)
                 ye(2) = caux
               endif
               if (abs(aimag(y(1)))/abs(y(1)).lt.crit)
     &           y(1) = inteps(y(1),ye(1))
               if (abs(aimag(y(2)))/abs(y(2)).lt.crit)
     &           y(2) = inteps(y(2),ye(2))
            endif
c m0 = 0, p2 =/= 0
          elseif (p2.ne.0._rk) then
            luv  = deltauv
            y(1) = 1._rk-m12/p2+ieps2*sign(1._rk,p2)*abs(1._rk-m12/p2)
c m0 = 0, m1 =/= 0
          elseif (m12.ne.(0._rk,0._rk)) then
            luv  = deltauv
            y(1) = 1.e20_rk
          else
c m02 =  m12 = p2 = 0
            luv  = deltauv-delta1ir+log(muv2/mir2)
          endif

        else
c read cached information for repeated calls
c-------------------------------------------
          do 50 r=0,min(r2,r2_aux(id))
          do 50 i0=0,2*r,2
          i1 = r-i0/2
            B(i0/2,i1)   = B_cache(tid(id),i0/2,i1)
            Buv(i0/2,i1) = Buv_cache(tid(id),i0/2,i1)
50        continue
          if (r2.le.r2_aux(id)) return
          p2     = auxr(id,1)
          qm0    = auxi(id,1)
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
        if (m02.ne.(0._rk,0._rk)) then
          do 100 k=1,2
          do 100 n=r2_aux(id)+1,r2
            if (abs(y(k)).lt.10._rk) then
              if (1._rk-1._rk/y(k).ne.(0._rk,0._rk)) then
                f(n,k) = (1._rk-y(k)**(n+1))*log(1._rk-1._rk/y(k))
              else
                f(n,k) = 0._rk
              endif
              do l=0,n
                f(n,k) = f(n,k) - y(k)**(n-l)/(l+1._rk)
              enddo
            else
              f(n,k) = log(1._rk-1._rk/y(k))
              if (abs(y(k)).lt.1.e20_rk) then
                do l=n+1,n+20
                  f(n,k) = f(n,k) + y(k)**(n-l)/(l+1._rk)
                enddo
              endif
            endif
100       continue
c m0 = 0 and ( p2 =/= 0 or m1 =/= 0 )
        elseif ((p2.ne.0._rk).or.(m12.ne.(0._rk,0._rk))) then
          do 200 n=r2_aux(id)+1,r2
            if (acmplx(p2).eq.m12) then
              f(n,1) = -log(muv2/m12) - 1._rk/(n+1._rk)
              f(n,2) = - 1._rk/(n+1._rk)
            else
              if (abs(y(1)).lt.10._rk) then
                if (1._rk-1._rk/y(1).ne.(0._rk,0._rk)) then
                  f(n,1) = (1._rk-y(1)**(n+1))*log(1._rk-1._rk/y(1))
                else
                  f(n,1) = 0._rk
                endif
                do l=0,n
                  f(n,1) = f(n,1) - y(1)**(n-l)/(l+1._rk)
                enddo
              else
                f(n,1) = log(1._rk-1._rk/y(1))
                if (abs(y(1)).lt.1.e20_rk) then
                  do l=n+1,n+20
                    f(n,1) = f(n,1) + y(1)**(n-l)/(l+1._rk)
                  enddo
                endif
              endif
              f(n,2) = log((m12-p2)/muv2 - ieps2*abs((m12-p2)/muv2)) 
     &                 - 1._rk/(n+1._rk)
            endif
200       continue
c m02 =  m12 = p2 = 0
        else
          do 300 n=r2_aux(id)+1,r2
            f(n,1) = 0._rk
            f(n,2) = 0._rk
300       continue
        endif

        do 400 n=r2_aux(id)+1,r2
          B(0,n)   = (-1)**n/(n+1._rk)*( luv - f(n,1) - f(n,2) )
          Buv(0,n) = (-1)**n/(n+1._rk)
400     continue

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
        if (id.ge.2**nmaster) id0 = 2**nmaster
        if (r2.gt.0) call A_dd(A(0:r2),Auv(0:r2),xm12,2*r2,id0)

        do 500 r=r2_aux(id)+1,r2
        do 500 i0=2,2*r,2
        i1 = r-i0/2
          if (qm0.eq.1) then
            B(i0/2,i1)   = 0._rk
            Buv(i0/2,i1) = 0._rk
          else
            Buv(i0/2,i1) = 1._rk/2._rk/(i0+i1+1)*(
     &          (-1)**i1*Auv(i0/2-1)+2._rk*m02*Buv(i0/2-1,i1)
     &          +f1*Buv(i0/2-1,i1+1)                              )
            B(i0/2,i1) = 1._rk/2._rk/(i0+i1+1)*(
     &          (-1)**i1*A(i0/2-1)+2._rk*m02*B(i0/2-1,i1)
     &          +f1*B(i0/2-1,i1+1)                                
     &          +4._rk*Buv(i0/2,i1)                         )
          endif
500     continue

c cache information 
        if (id.lt.2**nmaster) then
          do 600 r=r2_aux(id)+1,r2
          do 600 i0=0,2*r,2
          i1 = r-i0/2
            B_cache(tid(id),i0/2,i1)   = B(i0/2,i1) 
            Buv_cache(tid(id),i0/2,i1) = Buv(i0/2,i1) 
600       continue
          r2_aux(id) = r2
          auxr(id,1) = p2
          auxi(id,1) = qm0
          auxc(id,1) = y(1) 
          auxc(id,2) = y(2) 
          auxc(id,3) = m02
          auxc(id,4) = m12
          auxc(id,5) = f1
          auxc(id,6) = luv
        endif

c accuracy estimate
c==================
          Bmax = abs(B(0,0))
          do r=1,r2
            Bmax = max(Bmax,abs(B(0,r)))
          enddo
          do r=0,r2
            resaccrel(id,r)  = dprec_dd
            resaccabs(id,r)  = resaccrel(id,r)*Bmax
            resaccrel2(id,r) = resaccrel(id,r) 
            resaccabs2(id,r) = resaccabs(id,r) 
            if (resaccrel(id,r).gt.aimacc(2)) accflag = 1
            if (resaccrel(id,r).gt.erracc(2)) errflag = 1
          enddo


        end

**********************************************************************
        subroutine B0_dd(B0,Buv0,p2,m02,m12,r2,id)
**********************************************************************
*       2-point coefficients B(0)_{...} with unshifted momentum 
*---------------------------------------------------------------------
*       21.7.2006 Stefan Dittmaier
**********************************************************************
        implicit real(rk) (a-z)

c local variables
        integer r,r2,i0,i1,i2,i12,id
        complex(rk) B(0:r2,0:r2),B0(0:r2,0:r2,0:r2)
        complex(rk) Buv(0:r2,0:r2)
        complex(rk) Buv0(0:r2,0:r2,0:r2)
        complex(rk) m02,m12

        call B_dd(B,Buv,p2,m02,m12,r2,id)

        do 101 r=0,r2
        do 101 i0=0,2*r,2
          i12 = r-i0/2
          i2  = i12
          B0(i0/2,0,i2)   = B(i0/2,i2)
          Buv0(i0/2,0,i2) = Buv(i0/2,i2)
          do 101 i1=1,i12
            i2 = i12-i1
            B0(i0/2,i1,i2)   = -B0(i0/2,i1-1,i2) - B0(i0/2,i1-1,i2+1)
            Buv0(i0/2,i1,i2) = -Buv0(i0/2,i1-1,i2) - Buv0(i0/2,i1-1,i2+1)
101     continue

        end

**********************************************************************
        subroutine DB_dd(DB,DBuv,xp2,xm02,xm12,r2)
**********************************************************************
*       momentum derivative of 2-point coefficients  
*       DB(i,j) = DB_{0...01...1}(xp2,xm02,xm12) 
*                     \___/\___/
*                      2i    j  indices
*       rank r=i+j
*       
*                (i,j)
*       r2=0:    (0,0)
*       r2=1:    (2,0), (0,1)
*       r2=2:    (4,0), (2,1), (0,2)
*       ...
*       r2:      (2*r2,0), (2*r2-2,1), ... (0,r2)
*
*       DBuv(i,j) = coefficient of 1/eps_UV in DB(i,j),  Duv = 4-2*eps
*
*       NOTE: No cache system implemented for these functions !
*---------------------------------------------------------------------
*       21.6.2015 Stefan Dittmaier
**********************************************************************
        implicit real(rk) (a-z)

c local variables
        integer r2,i0,i1,k,l,n,qm0
        complex(rk) ieps,ieps2,y(2),ye(2),f1,f(-1:r2,2),g(0:r2,2)
        complex(rk) B(0:r2+3,0:r2+3),Buv(0:r2+3,0:r2+3)
        complex(rk) DB(0:r2,0:r2),DBuv(0:r2,0:r2)
        complex(rk) DB_aux(0:r2+1,0:r2+1),DBuv_aux(0:r2+1,0:r2+1)
        complex(rk) m02,m12,xm02,xm12,cm2,cmp2,caux,ma,ma2,root
        complex(rk) inteps,z,ze,rlam
        real(rk) Berr(0:r2+2),DBerr(0:r2)
        logical regp,regm02,regm12

        rmp2(rm2) = mx2(nint(rm2*1.e20_rk))
        cmp2(cm2) = mx2(nint(real(cm2*1.e20_rk)))
        inteps(z,ze) = acmplx( real(z),(abs(aimag(z))+abs(z)*eps2)
     &                                  *sign(1._rk,aimag(ze)) )

        accflag   = 0
        errflag   = 0
        stopflag  = 0

        crit  = 1.e-8_rk
        eps   = dprec_dd
        ieps  = acmplx(0._rk,eps)
        eps2  = 1.e-20_rk
        ieps2 = acmplx(0._rk,eps2)
        r2master = r2
        nmaster  = 2

c convert input parameters 
c-------------------------
          regp   = (abs(xp2) .gt.1.e-15_rk)
          regm02 = (abs(xm02).gt.1.e-15_rk)
          regm12 = (abs(xm12).gt.1.e-15_rk)
          p2  = xp2
          m02 = xm02
          m12 = xm12

          if (((m02.eq.(0._rk,0._rk)).and.(xp2.eq.xm12)).or.
     &        ((m12.eq.(0._rk,0._rk)).and.(xp2.eq.xm02)).or.
     &        ((.not.regm02).and.(xp2.eq.xm12).and.regp).or. 
     &        ((.not.regm12).and.(xp2.eq.xm02).and.regp)) then
c soft-singular case (dim. reg. or mass reg.)
            if ((.not.regp).and.(p2.ne.0._rk))          p2  = rmp2(p2)
            if ((.not.regm02).and.(m02.ne.(0._rk,0._rk))) m02 = cmp2(m02)
            if ((.not.regm12).and.(m12.ne.(0._rk,0._rk))) m12 = cmp2(m12)
            qm0 = -1
          elseif ((xp2.eq.xm12).or.(xp2.eq.xm02)) then
c regular on-shell case
            if ((.not.regp).and.(p2.ne.0._rk))          p2  = rmp2(p2)
            if ((.not.regm02).and.(m02.ne.(0._rk,0._rk))) m02 = cmp2(m02)
            if ((.not.regm12).and.(m12.ne.(0._rk,0._rk))) m12 = cmp2(m12)
            qm0 = 2
            if ((.not.regp).and.regm02) then
              p2  = 0._rk
              m12 = 0._rk
              qm0 = 3
            elseif ((.not.regp).and.regm12) then
              p2  = 0._rk
              m02 = 0._rk
              qm0 = 3
            endif
          elseif (regp.or.regm02.or.regm12) then
c regular case
            if (.not.regp)   p2  = 0._rk
            if (.not.regm02) m02 = 0._rk
            if (.not.regm12) m12 = 0._rk
            qm0 = 0
          else
c singular case -- all scales small
            if ((.not.regp).and.(p2.ne.0._rk))          p2  = rmp2(p2)
            if ((.not.regm02).and.(m02.ne.(0._rk,0._rk))) m02 = cmp2(m02)
            if ((.not.regm12).and.(m12.ne.(0._rk,0._rk))) m12 = cmp2(m12)
            qm0 = 1
          endif

c auxiliary parameters
c---------------------
          scale2  = max(abs(p2),abs(m02),abs(m12))
          f1      = p2-m12+m02
          f(-1,1) = 0._rk
          f(-1,2) = 0._rk
c m0 =/= 0
          if (m02.ne.(0._rk,0._rk)) then
            if (p2.eq.0._rk) then
              y(1) = 1.e20_rk
              if (m02.eq.m12) then
                y(2) = 1.e20_rk
                rlam = 0._rk
              else
                y(2) = m02/(m02-m12)
                if (abs(aimag(y(2)))/abs(y(2)).lt.crit)
     &            y(2) = y(2) - ieps2*abs(y(2))*sign(1._rk,real(m02-m12))
                  rlam = m02-m12
              endif
            else
              y(1)  = sqe_dd(acmplx(p2),-p2-m02+m12,m02)
              y(2)  = m02/p2/y(1)
              ye(1) = sqe_dd(acmplx(p2),-p2-m02+m12,m02-abs(m02)*ieps)
              ye(2) = (m02-abs(m02)*ieps)/p2/y(1)
              if (abs(y(1)-ye(1)).gt.abs(y(1)-ye(2))) then
                 caux  = ye(1)
                 ye(1) = ye(2)
                 ye(2) = caux
              endif
              if (abs(aimag(y(1)))/abs(y(1)).lt.crit)
     &          y(1) = inteps(y(1),ye(1))
              if (abs(aimag(y(2)))/abs(y(2)).lt.crit)
     &          y(2) = inteps(y(2),ye(2))
              rlam = p2*(y(1)-y(2))
            endif
c m02 = 0, p2 =/= 0
          elseif (p2.ne.0._rk) then
            y(1) = 1._rk-m12/p2+ieps2*sign(1._rk,p2)*abs(1._rk-m12/p2)
            y(2) = 0._rk
            rlam = p2*(y(1)-y(2))
c m02 = p2 = 0, m12 =/= 0
          elseif ((p2.eq.0._rk).and.(m12.ne.(0._rk,0._rk))) then
            y(1) = 0._rk
            y(2) = 1.e20_rk
            rlam = m12
          else
c m02 =  m12 = p2 = 0
            rlam = 0._rk
          endif

c optimize branching to different methods
c----------------------------------------
        if (qm0.eq.2) then
          if (abs(p2).lt.abs(rlam)) then
            qm0 = 0
          endif
        endif

c DB integrals
c-------------

c p2=m02=m12=rlam=0
        if ((p2.eq.0._rk).and.(m02.eq.(0._rk,0._rk))
     &                 .and.(m12.eq.(0._rk,0._rk))) then
          do n=0,r2
          do i0=0,2*n,2
            i1 = n-i0/2
            DBuv(i0/2,i1) = 0._rk
            DB(i0/2,i1)   = 0._rk
          enddo
          enddo
          do n=0,r2
            DBerr(n) = 0._rk
          enddo

c regular case with ( m02=0 or m12=0 ) and p2 not too small
        elseif (((qm0.eq.0).or.(qm0.eq.1)).and.
     &          (abs(p2).gt.abs(rlam)).and.
     &          ((m02.eq.(0._rk,0._rk)).or.(m12.eq.(0._rk,0._rk)))) then

          if (qm0.eq.0) then
            call B_dd(B(0:r2,0:r2),Buv(0:r2,0:r2),xp2,xm02,xm12,r2,0)
          elseif (qm0.eq.1) then
            call B_dd(B(0:r2,0:r2),Buv(0:r2,0:r2),p2,m02,m12,r2,0)
          endif

          DBuv(0,0) = 0._rk
          DB(0,0)   = -1._rk/p2
          if (m02.ne.(0._rk,0._rk)) then
            DB(0,0) = DB(0,0)
     &                -m02/p2**2*log(1._rk-p2/m02-ieps2*abs(1._rk-p2/m02))
          elseif (m12.ne.(0._rk,0._rk)) then
            DB(0,0) = DB(0,0)
     &                -m12/p2**2*log(1._rk-p2/m12-ieps2*abs(1._rk-p2/m12))
          endif

          do 700 n=1,r2
            DBuv(0,n) = 0._rk
            DB(0,n)   = -1._rk/p2*( B(0,n)
     &                           + n/(n+1._rk)*(B(0,n-1)+f1*DB(0,n-1)) )
            if (n.gt.1) DB(0,n) = DB(0,n) 
     &                   - 1._rk/p2*(n-1._rk)/(n+1._rk)
     &                           *( m02*DB(0,n-2)+2._rk*DBuv(1,n-2) )
          do 700 i0=2,2*n,2
            i1 = n-i0/2
            DBuv(i0/2,i1) = 1._rk/2._rk/(i0+i1+1)*( 2._rk*m02*DBuv(i0/2-1,i1)
     &                        +Buv(i0/2-1,i1+1) + f1*DBuv(i0/2-1,i1+1) )
            DB(i0/2,i1)   = 1._rk/2._rk/(i0+i1+1)*( 2._rk*m02*DB(i0/2-1,i1)
     &                        +B(i0/2-1,i1+1) + f1*DB(i0/2-1,i1+1) 
     &                        + 4._rk*DBuv(i0/2,i1)                    )
700       continue

c error estimate
          Berr(0)  = resaccabs(0,0)
          DBerr(0) = dprec_dd*abs(DB(0,0))
          do n=1,r2
            Berr(n)  = resaccabs(0,n)
            DBerr(n) = max(Berr(n),Berr(n-1),abs(f1)*DBerr(n-1))/abs(p2) 
            if (n.gt.1) DBerr(n) = max(DBerr(n),abs(m02/p2)*DBerr(n-2))
          enddo

c regular case 
        elseif (((qm0.eq.0).or.(qm0.eq.1).or.(qm0.eq.3))
     &          .and.(rlam.ne.(0._rk,0._rk))) then

          if ((qm0.eq.0).or.(qm0.eq.3)) then
            call B_dd(B(0:r2,0:r2),Buv(0:r2,0:r2),xp2,xm02,xm12,r2,0)
          elseif (qm0.eq.1) then
            call B_dd(B(0:r2,0:r2),Buv(0:r2,0:r2),p2,m02,m12,r2,0)
          endif

          do 100 k=1,2
          do 100 n=0,r2
            if (abs(y(k)).lt.10._rk) then
              if (y(k).ne.(0._rk,0._rk)) then
                caux = 1._rk-1._rk/y(k)
              else
                caux = 1.e25_rk
              endif
              if ((caux.eq.(0._rk,0._rk)).or.(y(k).eq.(0._rk,0._rk))) then
                f(n,k) = 0._rk
              else
                f(n,k) = (1._rk-y(k)**(n+1))*log(1._rk-1._rk/y(k))
              endif
              do l=0,n
                f(n,k) = f(n,k) - y(k)**(n-l)/(l+1._rk)
              enddo
              if (caux.eq.(0._rk,0._rk)) then
                g(n,k) = 0._rk
              elseif (y(k).eq.(0._rk,0._rk)) then
                g(n,k) = -1._rk/(1._rk+n)
              else
                g(n,k) = (1._rk-y(k))*( y(k)*f(n-1,k)
     &                     -y(k)*log(1._rk-1._rk/y(k))-1._rk/(1._rk+n) )
              endif
            else
              f(n,k) = log(1._rk-1._rk/y(k))
              g(n,k) = -1._rk/(n+2._rk)
              if (abs(y(k)).lt.1.e20_rk) then
                do l=n+1,n+20
                  f(n,k) = f(n,k) + y(k)**(n-l)/(l+1._rk)
                  g(n,k) = g(n,k) + y(k)**(n-l)/(l+1._rk)/(l+2._rk)
                enddo
              endif
            endif
100       continue

          do 400 n=0,r2
            DBuv(0,n) = 0._rk
            DB(0,n)   = (-1)**n/rlam*( g(n,2) - g(n,1) )
          do 400 i0=2,2*n,2
            i1 = n-i0/2
            DBuv(i0/2,i1) = 1._rk/2._rk/(i0+i1+1)*( 2._rk*m02*DBuv(i0/2-1,i1)
     &                        +Buv(i0/2-1,i1+1) + f1*DBuv(i0/2-1,i1+1) )
            DB(i0/2,i1)   = 1._rk/2._rk/(i0+i1+1)*( 2._rk*m02*DB(i0/2-1,i1)
     &                        +B(i0/2-1,i1+1) + f1*DB(i0/2-1,i1+1) 
     &                        + 4._rk*DBuv(i0/2,i1)                     )
400       continue

c error estimate
          do n=0,r2
            DBerr(n) = dprec_dd*abs(DB(0,n))
          enddo

c p2=0, m02=m12=/=0, rlam=0
        elseif ((p2.eq.0._rk).and.(m02.eq.m12)) then

          call B_dd(B,Buv,xp2,xm02,xm12,r2+3,0)

          do 403 n=0,r2+1
            DBuv_aux(0,n) = 0._rk
            do i0=2,2*n,2
              i1 = n-i0/2
              DBuv_aux(i0/2,i1) = 1._rk/2._rk/(i0+i1+1)*(2._rk*m02*DBuv_aux(i0/2-1,i1)
     &                        +Buv(i0/2-1,i1+1) + f1*DBuv_aux(i0/2-1,i1+1) )
            enddo
403       continue

          do 404 n=0,r2+1
            DB_aux(0,n)   = ( -(n+2._rk)/(n+1._rk)*B(0,n+1)
     &                      -(n+3._rk)/(n+1._rk)*B(0,n+2)
     &                      -2._rk*DBuv_aux(1,n) )/m02
            do 404 i0=2,2*n,2
              i1 = n-i0/2
              DB_aux(i0/2,i1)   = 1._rk/2._rk/(i0+i1+1)*( 2._rk*m02*DB_aux(i0/2-1,i1)
     &                          +B(i0/2-1,i1+1) + f1*DB_aux(i0/2-1,i1+1) 
     &                          + 4._rk*DBuv_aux(i0/2,i1)                  )
404       continue

          do 405 n=0,r2
            do 405 i0=0,2*n,2
              i1 = n-i0/2
              DBuv(i0/2,i1) = DBuv_aux(i0/2,i1) 
              DB(i0/2,i1)   = DB_aux(i0/2,i1) 
405       continue

c error estimate
          Berr(1) = resaccabs(0,1)
          do n=0,r2
            Berr(n+2)= resaccabs(0,n+2)
            DBerr(n) = max(Berr(n+1),Berr(n+2))/abs(m02)
          enddo

c soft-singular case (rlam=0)
c p2=m02=/=0, m12=small  or  p2=m12=/=0, m02=small
        elseif (qm0.eq.-1) then
          call B_dd(B(0:r2,0:r2),Buv(0:r2,0:r2),xp2,xm02,xm12,r2,0)

          DBuv(0,0) = 0._rk
          if ((m02.eq.(0._rk,0._rk)).or.(m12.eq.(0._rk,0._rk))) then
            DB(0,0)   = -(2._rk+delta1ir+log(mir2/p2))/(2._rk*p2)
          elseif(xp2.eq.xm12) then
            DB(0,0)   = -(2._rk+log(m02/p2))/(2._rk*p2)
          elseif(xp2.eq.xm02) then
            DB(0,0)   = -(2._rk+log(m12/p2))/(2._rk*p2)
          endif

          if (regp.and.(xp2.eq.xm12)) then
            m02 = 0._rk
            f1  = 0._rk
          elseif (regp.and.(xp2.eq.xm02)) then
            m12 = 0._rk
            f1  = 2._rk*p2
          endif

          do 600 n=1,r2
            DBuv(0,n) = 0._rk
            DB(0,n)   = -1._rk/p2*( B(0,n)
     &                           + n/(n+1._rk)*(B(0,n-1)+f1*DB(0,n-1)) )
            if (n.gt.1) DB(0,n) = DB(0,n) 
     &                   - 1._rk/p2*(n-1._rk)/(n+1._rk)
     &                           *( m02*DB(0,n-2)+2._rk*DBuv(1,n-2) )
          do 600 i0=2,2*n,2
            i1 = n-i0/2
            DBuv(i0/2,i1) = 1._rk/2._rk/(i0+i1+1)*( 2._rk*m02*DBuv(i0/2-1,i1)
     &                        +Buv(i0/2-1,i1+1) + f1*DBuv(i0/2-1,i1+1) )
            DB(i0/2,i1)   = 1._rk/2._rk/(i0+i1+1)*( 2._rk*m02*DB(i0/2-1,i1)
     &                        +B(i0/2-1,i1+1) + f1*DB(i0/2-1,i1+1) 
     &                        + 4._rk*DBuv(i0/2,i1)                    )
600       continue

c error estimate
          Berr(0)  = resaccabs(0,0)
          DBerr(0) = dprec_dd*abs(DB(0,0))
          do n=1,r2
            Berr(n)  = resaccabs(0,n)
            DBerr(n) = max(Berr(n),Berr(n-1),abs(f1)*DBerr(n-1))/abs(p2) 
            if (n.gt.1) DBerr(n) = max(DBerr(n),abs(m02/p2)*DBerr(n-2))
          enddo

c regular on-shell case
c p2=m02=/=0, m12=/=small  or  p2=m12=/=0, m02=/=small
        elseif (qm0.eq.2) then

          call B_dd(B(0:r2,0:r2),Buv(0:r2,0:r2),xp2,xm02,xm12,r2,0)

          if (xp2.eq.xm12) then
            ma2 = m02
          else
            ma2 = m12
          endif
          ma   = sqrt(ma2)
          root = sqrt( ma2-4._rk*p2-ieps2*abs(ma2-4._rk*p2) )

          DBuv(0,0) = 0._rk
          if (root.ne.0._rk) then
            DB(0,0)   = ma*(3._rk*p2-ma2)/root/p2**2
     &                    *log((ma+root)/2._rk/sqrt(p2))
     &                  +(ma2-p2)/2._rk/p2**2*log(ma2/p2)-1._rk/p2
          else
            DB(0,0)   = (-4._rk+3._rk*log(4._rk))/(2._rk*p2)
          endif

          do 800 n=1,r2
            DBuv(0,n) = 0._rk
            DB(0,n)   = -1._rk/p2*( B(0,n)
     &                           + n/(n+1._rk)*(B(0,n-1)+f1*DB(0,n-1)) )
            if (n.gt.1) DB(0,n) = DB(0,n) 
     &                   - 1._rk/p2*(n-1._rk)/(n+1._rk)
     &                           *( m02*DB(0,n-2)+2._rk*DBuv(1,n-2) )
          do 800 i0=2,2*n,2
            i1 = n-i0/2
            DBuv(i0/2,i1) = 1._rk/2._rk/(i0+i1+1)*( 2._rk*m02*DBuv(i0/2-1,i1)
     &                        +Buv(i0/2-1,i1+1) + f1*DBuv(i0/2-1,i1+1) )
            DB(i0/2,i1)   = 1._rk/2._rk/(i0+i1+1)*( 2._rk*m02*DB(i0/2-1,i1)
     &                        +B(i0/2-1,i1+1) + f1*DB(i0/2-1,i1+1) 
     &                        + 4._rk*DBuv(i0/2,i1)                    )
800       continue

c error estimate
          Berr(0)  = resaccabs(0,0)
          DBerr(0) = dprec_dd*abs(DB(0,0))
          do n=1,r2
            Berr(n)  = resaccabs(0,n)
            DBerr(n) = max(Berr(n),Berr(n-1),abs(f1)*DBerr(n-1))/abs(p2) 
            if (n.gt.1) DBerr(n) = max(DBerr(n),abs(m02/p2)*DBerr(n-2))
          enddo

        else
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'DB_dd: case not implemented'
            write(outchannel,*) 'p2  = ',p2
            write(outchannel,*) 'm02 = ',m02
            write(outchannel,*) 'm12 = ',m12
            if (cout.eq.coutmax) call DDlastmessage()
            cout = cout+1
          endif
          stopflag = min(-10,stopflag)
        endif

c error estimate
        DBmax = 0._rk
        do n=0,r2
          DBmax = max(DBmax,abs(DB(0,n)))
          resaccabs(0,n)  = DBerr(n)
          resaccrel(0,n)  = resaccabs(0,0)/DBmax
          resaccabs2(0,n) = resaccabs(0,n) 
          resaccrel2(0,n) = resaccrel(0,n) 
        enddo

        end

        end module dd_2pt_qp
