












!!
!!  File DD_3pt.F is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!





        module dd_3pt_qp
        use dd_global_qp
        use dd_aux_qp
        use dd_2pt_qp
        complex(rk), allocatable, dimension(:,:)       :: C_cache, Cuv_cache, C_new_cache, Cuv_new_cache
        complex(rk), allocatable, dimension(:,:,:,:,:) :: ttx2_aux
        complex(rk), allocatable, dimension(:,:,:)     :: x2_aux, tx2_aux
        real(rk)   , allocatable, dimension(:,:)       :: Cij_err, C00_err, Cij_err2, Cij_err_newprelim
        real(rk)   , allocatable, dimension(:,:)       :: C00_err_newprelim, Cij_err_new, C00_err_new
        real(rk)   , allocatable, dimension(:,:,:)     :: z2_aux, tz2_aux, z2i_aux
        real(rk)   , allocatable, dimension(:,:,:,:,:) :: ttz2_aux
        contains

*********************************************************************r
        subroutine C_dd(C,Cuv,p01,p12,p20,m02,m12,m22,r2,id)
**********************************************************************
*       3-point coefficients  
*       C(i,j,k) = C_{0...01...12...2}(p01,p12,p20,m02,m12,m22)
*                     \___/\___/\___/
*                      2i    j    k   indices
*       rank r=i+j+k 
*
*                (i,j+k)
*       r2=0:    (0,0)
*       r2=1:    (2,0), (0,1)
*       r2=2:    (4,0), (2,1), (0,2)
*       ...
*       r2:      (2*r2,0), (2*r2-2,1), ... (0,r2)
*
*       Cuv(i,j,k) = coefficient of 1/eps in C(i,j,k),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       12.4.2006 Stefan Dittmaier
**********************************************************************


        implicit real(rk) (a-z)

c local variables
        integer r2,r,i,igc,j,jg,jgc,k,ksm,l,n,sgn(2)
        complex(rk) C(0:r2,0:r2,0:r2)
        complex(rk) Cuv(0:r2,0:r2,0:r2)
        complex(rk) C_newprelim(0:rmax3,0:rmax3,0:rmax3)
        complex(rk) Cuv_newprelim(0:rmax3,0:rmax3,0:rmax3)
        complex(rk) C_new(0:rmax3,0:rmax3,0:rmax3)
        complex(rk) Cuv_new(0:rmax3,0:rmax3,0:rmax3)
        complex(rk) m02,m12,m22,f(2),detx2
        complex(rk) x2(0:2,0:2),tx2(0:2,0:2),ttx2(0:2,0:2,0:2,0:2)
        complex(rk) mat(3,3),mati(3,3)
        real(rk) z2(2,2),tz2(2,2),z2i(2,2),ttz2(2,2,2,2)
        integer i0,i1,i2,i12,id,cnt
        integer nid(0:nmax-1),qmethod_newprelim
        logical gp_ok,g2p_ok,gcp_ok,sm1_ok,sm2_ok,apv_ok,pvir_ok
        logical qnewmethodfixed

c       if (id.eq.18) then
c       write(*,*) 'C call ',r2,id
c       write(*,*) ' p01  = ',p01
c       write(*,*) ' p12  = ',p12
c       write(*,*) ' p20  = ',p20
c       write(*,*) ' m02 = ',m02
c       write(*,*) ' m12 = ',m12
c       write(*,*) ' m22 = ',m22
c       endif

c dummy declarations
        gp_ok  = .false.
        g2p_ok = .false.
        gcp_ok = .false.
        sm1_ok = .false.
        sm2_ok = .false.
        qnewmethodfixed = .false.
        gerr   = 0._rk
        g2err  = 0._rk
        gcerr  = 0._rk
        sm1err = 0._rk
        sm2err = 0._rk
        besterr= 0._rk
        accnew = 1.e30_rk

c store DD debug info
        if (id.eq.0) then
          s_DDin  = 'C_dd'
          nc_DDin = 3
          nr_DDin = 3
          ni_DDin = 2
          r_DDin(1) = p01
          r_DDin(2) = p12
          r_DDin(3) = p20
          c_DDin(1) = m02
          c_DDin(2) = m12
          c_DDin(3) = m22
          i_DDin(1) = r2
          i_DDin(2) = id
        endif

c set identifiers for lower-point integrals
        n = 0
        do k=0,nmax-1
          if (mod(id,2**(k+1))/2**k.eq.0) then
            nid(n) = id + 2**k
            n=n+1
          endif
          if (n.eq.3) goto 700
        enddo
700     continue

c initialization for master call
        if (id.eq.0) then
          do i=0,7
            r2_aux(i)     = -1
            r2_new_aux(i) = -1
            do r=0,r2
              resaccrel(i,r)  = 0._rk
              resaccabs(i,r)  = 0._rk
              resaccrel2(i,r) = 0._rk
              resaccabs2(i,r) = 0._rk
            enddo
          enddo
          nmaster   = 3
          r2master  = r2
          accflag   = 0
          errflag   = 0
          stopflag  = 0
        endif

        if (r2.gt.rmax3-3) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'C_dd called for rank r2 =',r2
            write(outchannel,*) 'rmax3 =',rmax3,' too small'
            call DD_debugoutput()
          endif
          stopflag = min(-9,stopflag)
        endif

c initilizations
        cutacc    = 1.e20_rk
        gcrit     = .2_rk
        g2crit    = .2_rk
        gccrit    = .2_rk
        sm1crit   = .2_rk
        sm2crit   = .2_rk
        reqacc    = 1._rk

c        expcrit = .05._rk
c        gcrit   = .05._rk
c        g2crit  = .05._rk
c        gccrit  = .05._rk

c initialization for first call
c==============================
        if (r2_aux(id).eq.-1) then

        qmethod(id)       = 0
        qmethod_new(id)   = 0
        qmethod_newprelim = 0

c algebraic quantities
c---------------------
        q1  = p01
        q2  = p20
        q12 = (p01+p20-p12)/2._rk

        sgn(1) = +1
        sgn(2) = -1

c Gram and related matrices
        z2(1,1)  = 2._rk*q1
        z2(1,2)  = 2._rk*q12
        z2(2,1)  = z2(1,2)
        z2(2,2)  = 2._rk*q2

        detz2    = 4._rk*(q1*q2-q12**2)

        if (abs(detz2).gt.1.e-10_rk*max(abs(p01),abs(p12),abs(p20))**2) then
          call inverse_dd(z2,z2i,detz2,2)
          do i=1,2
          do j=1,2
            tz2(i,j) = z2i(j,i)*detz2
          enddo
          enddo
        else
          tz2(1,1) = z2(2,2)
          tz2(1,2) = -z2(2,1)
          tz2(2,1) = tz2(1,2)
          tz2(2,2) = z2(1,1)
          do i=1,2
          do j=1,2
            z2i(i,j) = tz2(j,i)/detz2
          enddo
          enddo
        endif

        do 101 i=1,2
        do 101 j=1,2
          ttz2(i,  i,j,3-j) = 0._rk
          ttz2(i,3-i,j,  j) = 0._rk
          ttz2(i,  i,j,  j) = 0._rk
          ttz2(i,3-i,j,3-j) = -sgn(i)*sgn(j)
101     continue

c Cayley and related matrices
        f(1) = q1-m12+m02
        f(2) = q2-m22+m02
        x2(0,0) = 2._rk*m02
        do 200 i=1,2
          x2(0,i) = f(i)
          x2(i,0) = f(i)
        do 200 j=1,2
          x2(i,j) = z2(i,j)
200     continue

        detx2 = 2._rk*m02*detz2 + 2._rk*f(1)*f(2)*z2(1,2)
     &          - f(1)**2*z2(2,2) - f(2)**2*z2(1,1)

c detx2=0 for soft-singular C0
        if (((m02.eq.acmplx(0._rk,0._rk)).and.
     &       (acmplx(p01).eq.m12).and.(acmplx(p20).eq.m22)) .or.
     &      ((m12.eq.acmplx(0._rk,0._rk)).and.
     &       (acmplx(p12).eq.m22).and.(acmplx(p01).eq.m02)) .or.
     &      ((m22.eq.acmplx(0._rk,0._rk)).and.
     &       (acmplx(p20).eq.m02).and.(acmplx(p12).eq.m12)) ) detx2=0._rk

        if (abs(detx2).gt.1.e-10_rk*max(abs(p01),abs(p12),abs(p20)
     &                           ,abs(m02),abs(m12),abs(m22))**3) then
          do i=1,3
          do j=1,3
            mat(i,j) = x2(i-1,j-1)
          enddo
          enddo
          call xinverse_dd(mat,mati,detx2,3)
          do i=0,2
          do j=0,2
            tx2(i,j) = mati(j+1,i+1)*detx2
          enddo
          enddo
        else
          tx2(0,0) = detz2
          do i=1,2
            tx2(0,i)   = -tz2(i,1)*f(1)-tz2(i,2)*f(2)
            tx2(i,0)   = tx2(0,i)
            tx2(i,i)   = 2._rk*m02*tz2(i,i)  -f(3-i)*f(3-i)
            tx2(i,3-i) = 2._rk*m02*tz2(i,3-i)+f(i)*f(3-i)
          enddo
        endif

        do 202 i=1,2
        do 202 j=1,2
          ttx2(0,  i,0,  j) = -tz2(i,j)
          ttx2(i,  0,j,  0) = -tz2(i,j)
          ttx2(0,  i,j,  0) =  tz2(i,j)
          ttx2(i,  0,0,  j) =  tz2(i,j)
          ttx2(0,  i,j,  j) =  0._rk
          ttx2(i,  0,j,  j) =  0._rk
          ttx2(j,  j,0,  i) =  0._rk
          ttx2(j,  j,i,  0) =  0._rk
          ttx2(0,  i,j,3-j) = -f(3-i)*ttz2(3-i,i,j,3-j) 
          ttx2(i,  0,j,3-j) = -ttx2(0,i,j,3-j)
          ttx2(j,3-j,0,  i) =  ttx2(0,i,j,3-j) 
          ttx2(j,3-j,i,  0) = -ttx2(0,i,j,3-j) 
202     continue

c internal cache for later calls
          do 601 i=1,2
          do 601 j=1,2
            z2_aux(tid(id),i,j)  = z2(i,j)
            tz2_aux(tid(id),i,j) = tz2(i,j)
            z2i_aux(tid(id),i,j) = z2i(i,j)
          do 601 k=1,2
          do 601 l=1,2
            ttz2_aux(tid(id),i,j,k,l) = ttz2(i,j,k,l)
601       continue
          do 602 i=0,2
          do 602 j=0,2
            x2_aux(tid(id),i,j)  = x2(i,j)
            tx2_aux(tid(id),i,j) = tx2(i,j)
          do 602 k=0,2
          do 602 l=0,2
            ttx2_aux(tid(id),i,j,k,l) = ttx2(i,j,k,l)
602       continue
          auxr(id,1) = detz2
          auxc(id,1) = detx2
          auxc(id,2) = f(1)
          auxc(id,3) = f(2)

c initialize error propagation 
        do i=0,2*rmax3
          Cij_err(tid(id),i)       = 0._rk
          Cij_err2(tid(id),i)      = 0._rk
          C00_err(tid(id),i)       = 0._rk
          accr2_aux(tid(id),i)     = 0._rk
          Cij_err_newprelim(tid(id),i) = 0._rk
          C00_err_newprelim(tid(id),i) = 0._rk
          accr2_newprelim(tid(id),i)   = 0._rk
          Cij_err_new(tid(id),i)   = 0._rk
          C00_err_new(tid(id),i)   = 0._rk
          accr2_new_aux(tid(id),i) = 0._rk
        enddo

c scalar 3pt integral
c--------------------

        C(0,0,0)    = C0dd(p01,p12,p20,m02,m12,m22,0)
        Cuv(0,0,0)  = 0._rk
        scalint(id) = C(0,0,0)
        scalintnew(id) = 0._rk

        scalint_err(id) = 10*dprec_dd*abs(scalint(id))
     &            *max( 1._rk/sqrt(abs(detz2))/abs(scalint(id)), 1._rk )
        Cij_err(tid(id),0)   = scalint_err(id)
        Cij_err2(tid(id),0)  = scalint_err(id)
        C00_err(tid(id),0)   = 0._rk
        accr2_aux(tid(id),0) = abs(scalint_err(id))/abs(scalint(id))


        else
c read cached information for repeated calls
c-------------------------------------------

        cnt = 0
        do 500 r=0,min(r2,r2_aux(id))
        do 500 i0=0,2*r,2
        i12 = r-i0/2
        do 500 i1=0,i12
          i2 = i12-i1
          cnt = cnt + 1
          C(i0/2,i1,i2)   = C_cache(tid(id),cnt)
          Cuv(i0/2,i1,i2) = Cuv_cache(tid(id),cnt)
500     continue

        if (r2_aux(id).ge.r2) return

        if (qmethod_new(id).ge.200) then
          cnt = 0
          do 503 r=0,r2_new_aux(id)
          do 503 i0=0,2*r,2
          i12 = r-i0/2
          do 503 i1=0,i12
            i2 = i12-i1
            cnt = cnt + 1
            C_newprelim(i0/2,i1,i2)   = C_new_cache(tid(id),cnt)
            Cuv_newprelim(i0/2,i1,i2) = Cuv_new_cache(tid(id),cnt)
            C_new(i0/2,i1,i2)         = C_new_cache(tid(id),cnt)
            Cuv_new(i0/2,i1,i2)       = Cuv_new_cache(tid(id),cnt)
503       continue
        endif

        do 501 i=1,2
        do 501 j=1,2
          z2(i,j)  = z2_aux(tid(id),i,j) 
          tz2(i,j) = tz2_aux(tid(id),i,j)
          z2i(i,j) = z2i_aux(tid(id),i,j)
        do 501 k=1,2
        do 501 l=1,2
          ttz2(i,j,k,l) = ttz2_aux(tid(id),i,j,k,l)
501     continue
        do 502 i=0,2
        do 502 j=0,2
          x2(i,j)  = x2_aux(tid(id),i,j) 
          tx2(i,j) = tx2_aux(tid(id),i,j)
        do 502 k=0,2
        do 502 l=0,2
          ttx2(i,j,k,l) = ttx2_aux(tid(id),i,j,k,l)
502     continue
        detz2 = auxr(id,1)
        detx2 = auxc(id,1)
        f(1)  = auxc(id,2)
        f(2)  = auxc(id,3)

        endif

c quit if no tensors are needed
        if (r2.le.0) then
          if (r2_aux(id).eq.-1) then
            acc = 0._rk
            accnew = 1.e30_rk
          else
            acc = acc_pave(id)
            accnew = acc_new(id)
          endif
          if (qmethod(id).gt.0) acc=1.e30_rk
          goto 599
        endif

c Tensor reduction 
c=================

c*** PaVe reduction available for any tensor rank
c       accumulating tensor rank supported

        if (qmethod(id).gt.0) then
          acc=1.e30_rk
        else
          call Cpave_dd(C,Cuv,p01,p12,p20,m02,m12,m22,acc,
     &                  z2,z2i,r2,id,nid)
          acc_pave(id) = acc

          if (acc.gt.cutacc) acc = cutacc

          if (qmethod_new(id).gt.0) then
            accnew = accr2_new_aux(tid(id),r2)
          else
            accnew = 1.e30_rk
          endif
        endif

        if ((acc.lt.cacc).or.(mode34.eq.0)) goto 599

c mode34=2: improvement by expansion in small Gram/kinematical determinants
c--------------------------------------------------------------------------
        if (mode34.eq.2) then

c previous calculation sufficient
          if (r2_new_aux(id).ge.r2) then
            if (acc_new(id).lt.1.e31_rk)
     &        accnew = accr2_new_aux(tid(id),r2)
            goto 599
          else
c start selection of new method again (changed)
            r2_new_aux(id) = -1
            qmethod_new(id)   = 0
            qmethod_newprelim = 0
            qnewmethodfixed = .false.
          endif

c*** some settings if called first time, otherwise remember setup
          if (qmethod_new(id).ne.0) then
            jg  = auxi(id,1)
            igc = auxi(id,2)
            jgc = auxi(id,3)
            k   = auxi(id,4)
            l   = auxi(id,5) 
            ksm = auxi(id,6) 
          else

c some criteria / parameter for expansions
            scale2c  = 1._rk/abs(scalint(id))
            scale2in = ( abs(p01)+abs(p12)+abs(p20)
     &                  +abs(m02)+abs(m12)+abs(m22) )/6._rk
            scale2hi = max(scale2c,scale2in)
            scale2lo = min(scale2c,scale2in)

c determine indices k,l for expansions
            maxtz_kl = 0._rk
            k = 1
            l = 1
            do i1=1,2
            do i2=i1,2  
              if (abs(tz2(i1,i2)).ge.maxtz_kl) then 
                maxtz_kl = max(maxtz_kl,abs(tz2(i1,i2))) 
                k = i1
                l = i2
              endif
            enddo
            enddo
            if (abs(tx2(l,0)).lt.abs(tx2(k,0))) then
              i1 = k
              k  = l
              l  = i1
            endif
            maxttx0klm(id)  = max(abs(ttx2(0,k,l,3-l)),
     &                            abs(ttx2(0,l,k,3-k)))
            maxttz_knlm(id) = abs(ttz2(k,3-k,l,3-l))
            ttzff_kl(id)    = abs(tx2(k,l)-2._rk*m02*tz2(k,l))

c determine indices i=igc,j=jgc for Gram/Cayley
            maxtx_0j = 0._rk
            do i1=1,2
              if (abs(tx2(0,i1)).ge.maxtx_0j) then
                maxtx_0j = abs(tx2(0,i1))
                jg = i1
              endif
            enddo
            maxtz_nj(id) = 0._rk
            do n=1,2
              maxtz_nj(id) = max(maxtz_nj(id),abs(tz2(n,jg)))
            enddo

c determine indices i=igc,j=jgc for Gram/Cayley
            maxtx2ij = 0._rk
            igc = 1
            jgc = 1
            do i1=1,2
            do i2=i1,2
              if (abs(tx2(i1,i2)).ge.maxtx2ij) then
                maxtx2ij = abs(tx2(i1,i2))
                igc = i1
                jgc = i2
              endif
            enddo
            enddo
            if (abs(tx2(0,igc)).lt.abs(tx2(0,jgc))) then
              i1  = igc
              igc = jgc
              jgc = i1
            endif
            maxttx0ijm(id) =
     &        max(abs(ttx2(0,igc,jgc,3-jgc)),abs(ttx2(0,jgc,igc,3-igc)))

c determine index k=ksm for small-momentum expansion
            maxf = 0._rk
            ksm = 1
            do i1=1,2
              if (abs(f(i1)).ge.maxf) then
                maxf = abs(f(i1))
                ksm = i1
              endif
            enddo

            auxi(id,1) = jg
            auxi(id,2) = igc
            auxi(id,3) = jgc
            auxi(id,4) = k
            auxi(id,5) = l
            auxi(id,6) = ksm

c check if expansions are appropriate
            b0err = dprec_dd
            cmiss = 1._rk/scale2lo
            aa = max( 1._rk, maxtx2ij/maxtx_0j, maxttx0klm(id)/maxtz_kl )
            bb = max( 1._rk, maxtx2ij/maxtx_0j )
            maxz = max(abs(z2(1,1)),abs(z2(1,2)),abs(z2(2,2)))

c expansion parameter for Gram
            if ((maxtz_kl.ne.0._rk).and.(maxtx_0j.ne.0._rk)) then
cold              gparam = max(abs(detz2)/maxtx_0j,
cold     &                     abs(detz2)/scale2lo/maxtz_kl)
              gparam = abs(detz2)/maxtx_0j
            else
              gparam = 1.e10_rk
            endif
c error propagation for Gram to rank=r2+2
            gp_ok = (gparam.lt.gcrit)
            if (gp_ok) then
            gerr1 = b0err*maxtz_kl/maxtx_0j * max(
     &                gparam**(2*r2-2)
     &                  *(maxtx2ij*maxttz_knlm(id)/maxtz_kl**2)**(r2-2)
     &                  *aa*max(aa, scale2hi*maxttz_knlm(id)/maxtz_kl),
     &                (scale2hi*maxttz_knlm(id)/maxtz_kl)**(r2/2)*aa,
     &                max( aa, scale2hi*maxttz_knlm(id)/maxtz_kl )
     &                  *aa**(r2-3)
     &                  *max(aa**2,
     &                       maxttz_knlm(id)*maxtx_0j/maxtz_kl**2*bb) )
            gerr2 = cmiss * gparam**(r2+1) * aa**(r2-2) * bb 
     &                 *max( aa, maxttz_knlm(id)*maxtx_0j/maxtz_kl**2 ) 
            gerr  = max(gerr1,gerr2) / cmiss   
c            if (gerr.gt.reqacc) gp_ok=.false.
            else
              gerr  = 1.e10_rk
            endif

c expansion parameter for Gram - version 2
            g2param = abs(detz2)/abs(detx2)*scale2hi 
c error propagation for Gram - version 2 up to rank=r2+2
            g2p_ok = (g2param.lt.g2crit)
            if (g2p_ok) then
              g2err1 = b0err * max( 
     &                 maxtx_0j/abs(detx2)  
     &                  * max( 1._rk,(scale2hi*maxtx_0j/abs(detx2))**r2 ),
     &                 g2param**(r2+1)/scale2lo )
              g2err2 = cmiss * g2param**(r2+1)
              g2err  = max( g2err1, g2err2 ) / cmiss   
c            if (g2err.gt.reqacc) g2p_ok=.false.
            else
              g2err  = 1.e10_rk
            endif

c expansion parameters for Gram/Cayley
            if ((maxtz_kl.ne.0._rk).and.(tx2(igc,jgc).ne.0._rk)) then
              gcparam = max( abs(detz2/tx2(igc,jgc)),
     &                       abs(maxtx_0j/tx2(igc,jgc)) )
            else
              gcparam = 1.e10_rk
            endif
c error propagation for Gram/Cayley up to rank=r2+2
            gcp_ok = (gcparam.lt.gccrit)
            if (gcp_ok) then
              gcerr1 = b0err*max(maxtz_kl,maxttx0ijm(id))/maxtx2ij
              gcerr2 = cmiss * gcparam**(r2+1)  
              gcerr  = max( gcerr1, gcerr2 ) / cmiss   
c            if (gcerr.gt.reqacc) gcp_ok=.false.
            else
              gcerr  = 1.e10_rk
            endif

c expansion parameters for small momentum - variant 1 (f(ksm) not small)
            if (maxf.ne.0._rk) then
              sm1param = max(maxz/maxf,maxz*abs(m02)/maxf**2)
            else
              sm1param = 1.e10_rk
            endif
c error propagation for small momentum up to rank=r2+2
            sm1_ok  = (sm1param.lt.sm1crit)
            if (sm1_ok) then
              sm1err1 = b0err/maxf * max( 1._rk, abs(m02/maxf) )**r2
              sm1err2 = cmiss * abs(detz2/maxf)**(r2+1) 
     &                        * max( 1._rk, abs(m02/maxf) )**r2
              sm1err  = max( sm1err1, sm1err2 ) / cmiss   
c            if (sm1err.gt.reqacc) sm1_ok=.false.
            else
              sm1err  = 1.e10_rk
            endif
        
c expansion parameters for small momentum - variant 2 (all f(k) small)
            if (abs(m02).lt.1.e-5_rk) then
              sm2param = 1.e10_rk
            else
              sm2param = max(maxf/scale2lo,maxz/abs(m02))
            endif
c error propagation for small momentum
            sm2_ok = (sm2param.lt.sm2crit)
            if (sm2_ok) then
              sm2err = sm2param**r2
            else
              sm2err  = 1.e10_rk
            endif
c not yet implemented
            sm2_ok = .false.

          endif

c find optimal method
          Cij_err_new_max = 1.e33_rk
          accnew = 1.e30_rk
          if (qmethod_new(id).eq.0) then
            qnewmethodfixed = .false.
          elseif (qmethod_new(id).gt.0) then
            qnewmethodfixed = .true.
          elseif (qmethod_new(id).eq.-1) then
            goto 599
          endif

          if (.not.qnewmethodfixed) then
            apv_ok = .true.
          else
            apv_ok = .false.
          endif

          if (((m12.eq.acmplx(0._rk,0._rk)).and.
     &         (acmplx(p12).eq.m22).and.(acmplx(p01).eq.m02)).or.
     &        ((m22.eq.acmplx(0._rk,0._rk)).and.
     &         (acmplx(p20).eq.m02).and.(acmplx(p12).eq.m12))) then
            pvir_ok = .true.
          else
            pvir_ok = .false.
          endif

777       continue

          besterr = 1.e10_rk
          if (.not.qnewmethodfixed) then
            r2_newprelim(id) = -1
            do r=0,2*rmax3
              accr2_newprelim(tid(id),r) = 0._rk
            enddo
            if (gp_ok)  besterr = min(besterr,gerr)
            if (g2p_ok) besterr = min(besterr,g2err)
            if (gcp_ok) besterr = min(besterr,gcerr)
            if (sm1_ok) besterr = min(besterr,sm1err)
            if (sm2_ok) besterr = min(besterr,sm2err)
            if ((.not.apv_ok).and.(besterr.eq.1.e10_rk)) goto 599
          elseif (qnewmethodfixed) then
            r2_newprelim(id) = r2_new_aux(id) 
            do r=0,r2_new_aux(id)
              Cij_err_newprelim(tid(id),r) = Cij_err_new(tid(id),r)
              C00_err_newprelim(tid(id),r) = C00_err_new(tid(id),r)
              accr2_newprelim(tid(id),r) = accr2_new_aux(tid(id),r)
            enddo
          endif

c         if (id.eq.8) then
c         write(*,*) 'C id ',id
c         write(*,*) 'acc_pave ',acc_pave(id)
c         write(*,*) 'besterr  ',besterr
c         write(*,*) 'sm1param ',sm1_ok,sm1param,sm1err
c         write(*,*) 'sm2param ',sm2_ok,sm2param,sm2err
c         write(*,*) 'gparam   ',gp_ok,gparam,gerr
c         write(*,*) 'g2param  ',g2p_ok,g2param,g2err
c         write(*,*) 'gcparam  ',gcp_ok,gcparam,gcerr
c         write(*,*) 'method   ',qmethod_new(id)
c         write(*,*) 'scale2hi', scale2hi
c         write(*,*) 'scale2lo', scale2lo
c         write(*,*) 'igc,jgc ', igc,jgc 
c         write(*,*) tx2(igc,jgc)
c         write(*,*) 'ccc ', p01,p12,p20,m02,m12,m22
c         endif

c decide on expansion
          if (((qnewmethodfixed).and.(qmethod_new(id).eq.206)).or.
     &            (pvir_ok)) then
            C_newprelim(0,0,0) = C(0,0,0)
            call Cpaveir_dd(C_newprelim,Cuv_newprelim,
     &             p01,p12,p20,m02,m12,m22,accnewprelim,r2,id)
            qmethod_newprelim = 206
            r2_newprelim(id) = r2
            pvir_ok = .false.
          elseif (((qnewmethodfixed).and.(qmethod_new(id).eq.202)).or.
     &            (apv_ok)) then
            if (abs(detx2*C(0,0,0)**2).gt.1.e-8_rk) then
              C_newprelim(0,0,0) = C(0,0,0)
              qmethod_newprelim = 202
              call Calpave_dd(C_newprelim,Cuv_newprelim,
     &          p01,p12,p20,m02,m12,m22,
     &          accnewprelim,f,detx2,tx2,r2,id,nid)
              r2_newprelim(id) = r2
            else
              accnewprelim = 1.e30_rk
              qmethod_newprelim = 0
            endif
            apv_ok = .false.
          elseif (((qnewmethodfixed).and.(qmethod_new(id).eq.200)).or.
     &        (gp_ok.and.(besterr.eq.gerr))) then
            qmethod_newprelim = 200
            call Cgram_dd(C_newprelim,Cuv_newprelim,
     &          p01,p12,p20,m02,m12,m22,jg,k,l,
     &          accnewprelim,detz2,tz2,ttz2,tx2,f,
     &          r2,r2_newprelim(id),id,nid)
            gp_ok = .false.
          elseif (((qnewmethodfixed).and.(qmethod_new(id).eq.201)).or.
     &            (gcp_ok.and.(besterr.eq.gcerr))) then
            qmethod_newprelim = 201
            call Cgramcayley_dd(C_newprelim,Cuv_newprelim,
     &          p01,p12,p20,m02,m12,m22,
     &          igc,jgc,k,l,accnewprelim,detz2,tz2,ttz2,tx2,f,
     &          r2,r2_newprelim(id),id,nid)
            gcp_ok = .false.
          elseif (((qnewmethodfixed).and.(qmethod_new(id).eq.203)).or.
     &            (sm1_ok.and.(besterr.eq.sm1err))) then
            qmethod_newprelim = 203
            call Csm1_dd(C_newprelim,Cuv_newprelim,
     &          p01,p12,p20,m02,m12,m22,ksm,
     &          accnewprelim,z2,f,r2,r2_newprelim(id),id,nid)
            sm1_ok = .false.
          elseif (((qnewmethodfixed).and.(qmethod_new(id).eq.205)).or.
     &            (g2p_ok.and.(besterr.eq.g2err))) then
            qmethod_newprelim = 205
            call Cgram2_dd(C_newprelim,Cuv_newprelim,
     &          p01,p12,p20,m02,m12,m22,
     &          accnewprelim,detz2,detx2,tx2,f,
     &          r2,r2_newprelim(id),id,nid)
            g2p_ok = .false.
          endif

c*** result acceptable
            if ((qnewmethodfixed).or.(accnewprelim.lt.reqacc)) then
              Cij_err_newprelim_max = 0._rk
              do r=0,r2
                 Cij_err_newprelim_max = max( Cij_err_newprelim_max,
     &                                        Cij_err_newprelim(tid(id),r) )
              enddo
              if (Cij_err_newprelim_max.lt.Cij_err_new_max) then
                qmethod_new(id) = qmethod_newprelim
                r2_new_aux(id)  = r2_newprelim(id)
                accnew          = accnewprelim
                Cij_err_new_max = Cij_err_newprelim_max
                do 900 r=0,r2_new_aux(id)
                  Cij_err_new(tid(id),r) = Cij_err_newprelim(tid(id),r)
                  C00_err_new(tid(id),r) = C00_err_newprelim(tid(id),r)
                  accr2_new_aux(tid(id),r) = accr2_newprelim(tid(id),r)
                  do 900 i0=0,2*r,2
                  i12 = r-i0/2
                  do 900 i1=0,i12
                    i2 = i12-i1
                    C_new(i0/2,i1,i2)   = C_newprelim(i0/2,i1,i2)
                    Cuv_new(i0/2,i1,i2) = Cuv_newprelim(i0/2,i1,i2)
900             continue
              endif
            endif

c*** try other expansions if previous expansion was inappropriate

            if ((accnewprelim.gt.cacc).and.(.not.qnewmethodfixed)) then
c             write(*,*) 'new method failed ',qmethod_newprelim,accnewprelim,id
              goto 777
            endif

        endif

599     continue


c*** Final result 
c================

        if (accnew.gt.cutacc) accnew = cutacc

        if ((qmethod(id).eq.0).and.(qmethod_new(id).gt.0).and.
     &      (accnew.gt.reqacc)) then
          qmethod_new(id) = -1
          accnew =1.e31_rk
        endif

        acc_new(id)  = accnew
        qmethod(id)  = 0
        do r=0,max(0,r2)
          resaccrel(id,r) = accr2_aux(tid(id),r)
        enddo

        Cij_err_max     = 0._rk
        do r=0,r2
          Cij_err_max   = max(Cij_err_max,Cij_err(tid(id),r))
        enddo
        Cij_err_new_max = 0._rk
        if (qmethod_new(id).gt.0) then
          do r=0,r2
            Cij_err_new_max = max(Cij_err_new_max,Cij_err_new(tid(id),r))
          enddo
        endif

        if ((qmethod_new(id).gt.0).and.
     &      ((acc.eq.1.e30_rk).or.(Cij_err_new_max.lt.Cij_err_max))) then
          r2_aux(id)  = r2_new_aux(id)
          qmethod(id) = qmethod_new(id)

          do 550 r=0,r2
            if ((accr2_aux(tid(id),r).ne.0._rk).and.(Cij_err(tid(id),r).ne.0._rk).and.
     &          (Cij_err(tid(id),r).lt.Cij_err_new(tid(id),r))) goto 550
            Cij_err(tid(id),r)  = Cij_err_new(tid(id),r)
            Cij_err2(tid(id),r) = Cij_err(tid(id),r)
            C00_err(tid(id),r)  = C00_err_new(tid(id),r)
            accr2_aux(tid(id),r) = accr2_new_aux(tid(id),r)
            resaccrel(id,r) = accr2_new_aux(tid(id),r)
            do 551 i0=0,2*r,2
            i12 = r-i0/2
            do 551 i1=0,i12
              i2 = i12-i1
              C(i0/2,i1,i2)   = C_new(i0/2,i1,i2)
              Cuv(i0/2,i1,i2) = Cuv_new(i0/2,i1,i2)
551         continue
550       continue
        endif

c accuracy estimate
c==================
          Cmax = abs(C(0,0,0))
          do r=1,r2
            i0=0
            i12 = r-i0
            do i1=0,i12
              i2=i12-i1
              Cmax = max(Cmax,abs(C(i0/2,i1,i2)))
            enddo
          enddo
          do r=0,max(0,r2)
            resaccabs(id,r)  = Cmax*resaccrel(id,r)
            resaccabs2(id,r) = Cij_err2(tid(id),r) 
            resaccrel2(id,r) = Cij_err2(tid(id),r)/Cmax
            if (resaccrel(id,r).gt.aimacc(3)) accflag = 1
            if (resaccrel(id,r).gt.erracc(3)) errflag = 1
          enddo

c store for checking purposes
c============================
        scalintnew(id) = C_new(0,0,0)

c cache information
c==================

        r2_aux(id) = r2

        cnt = 0
        do 600 r=0,r2_aux(id)
        do 600 i0=0,2*r,2
        i12 = r-i0/2
        do 600 i1=0,i12
          i2 = i12-i1
          cnt = cnt + 1
          C_cache(tid(id),cnt)   = C(i0/2,i1,i2)
          Cuv_cache(tid(id),cnt) = Cuv(i0/2,i1,i2)
600     continue
        if (cnt.gt.Ncoefmax3_int) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'Ncoefmax3_int too small!'
            if (cout.eq.coutmax) call DDlastmessage()
            cout = cout+1
          endif
          stopflag = min(-9,stopflag)
        endif

        if (qmethod_new(id).ge.200) then
          cnt = 0
          do 610 r=0,r2_new_aux(id)
          do 610 i0=0,2*r,2
          i12 = r-i0/2
          do 610 i1=0,i12
            i2 = i12-i1
            cnt = cnt + 1
            C_new_cache(tid(id),cnt)   = C_new(i0/2,i1,i2)
            Cuv_new_cache(tid(id),cnt) = Cuv_new(i0/2,i1,i2)
610       continue
          if (cnt.gt.Ncoefmax3_int) then
            if (cout_on.and.(cout.le.coutmax)) then
              write(outchannel,*) 'Ncoefmax3_int too small!'
              if (cout.eq.coutmax) call DDlastmessage()
              cout = cout+1
            endif
            stopflag = min(-9,stopflag)
          endif
        endif


c test output
c============
        acc_new(id)  = Cij_err_new_max/Cmax

        if ((outlevel.gt.0).and.
     &      (acc_pave(id).ge.1._rk).and.(acc_new(id).ge.1._rk))
     &    call DD_debugoutput()

        if ((outlevel.gt.0).and.(id.eq.0)) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*)
            write(outchannel,*) '3pt tensor integral id = ',id
            write(outchannel,*) '  ranks up to ',r2
            write(outchannel,*) '  Cacc_pave = ',acc_pave(id)
            if (qmethod_new(id).ne.0)
     &      write(outchannel,*) '  Cacc_new  = ',acc_new(id),
     &               '  method ',qmethod_new(id)
            if (cout.eq.coutmax) call DDlastmessage()
            cout = cout+1
          endif
        endif

        end

**********************************************************************
        subroutine Cpave_dd(C,Cuv,p01,p12,p20,m02,m12,m22,acc,
     &                      z2,z2i,r2,id,nid)
**********************************************************************
*       Passarino-Veltman reduction
*
*       3-point coefficients  
*       C(i,j,k) = C_{0...01...12...2}(p01,p12,p20,m02,m12,m22)
*                     \___/\___/\___/
*                      2i    j    k   indices
*       of rank r=i+j+k with 1 <= r <= r2
*
*       Cuv(i,j,k) = coefficient of 1/eps in C(i,j,k),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       12.4.2006 Stefan Dittmaier
**********************************************************************
        implicit real(rk) (a-z)

c local variables
        integer r2,r,id,nid(0:nmax-1)
        complex(rk) C(0:r2,0:r2,0:r2)
        complex(rk) Cuv(0:r2,0:r2,0:r2)
        complex(rk) m02,m12,m22
        real(rk) Cij_err_prelim(0:r2),Cij_err2_prelim(0:r2)
        real(rk) C00_err_prelim(0:r2),accr2_prelim(0:r2)
        real(rk) z2(2,2),z2i(2,2)

        if (r2.le.0) then
          write(*,*) 'Cpave_dd called with r2<1. Fix that! r2 = ',r2
          stop
        endif

        Cij_err_prelim(0)  = scalint_err(id)
        Cij_err2_prelim(0) = scalint_err(id)
        C00_err_prelim(0)  = 0._rk
        accr2_prelim(0)    = scalint_err(id)/abs(scalint(id))

        call Cpave_kernel_dd(C,Cuv,Cij_err_prelim,Cij_err2_prelim,C00_err_prelim,
     &                      p01,p12,p20,m02,m12,m22,acc,accr2_prelim,
     &                      z2,z2i,r2,nid)

        do r=1,r2
          Cij_err(tid(id),r)   = Cij_err_prelim(r) 
          Cij_err2(tid(id),r)  = Cij_err2_prelim(r)
          C00_err(tid(id),r)   = C00_err_prelim(r)
          accr2_aux(tid(id),r) = accr2_prelim(r)
        enddo

        end

**********************************************************************
        subroutine Cpaveir_dd(C,Cuv,p01,p12,p20,m02,m12,m22,acc,r2,id)
**********************************************************************
*       Passarino-Veltman reduction
*       Soft IR-divergent case with shifted momentum
*
*       3-point coefficients  
*       C(i,j,k) = C_{0...01...12...2}(p01,p12,p20,m02,m12,m22)
*                     \___/\___/\___/
*                      2i    j    k   indices
*       of rank r=i+j+k with 1 <= r <= r2
*
*       Cuv(i,j,k) = coefficient of 1/eps in C(i,j,k),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       23.3.2016 Stefan Dittmaier
**********************************************************************
        implicit real(rk) (a-z)

c local variables
        integer r2,r,id,xnid(0:nmax-1),sgn,i,j,k,l,i0,i1,i2
        integer*8 maxbin(0:r2)
        complex(rk) C(0:rmax3,0:rmax3,0:rmax3)
        complex(rk) Cuv(0:rmax3,0:rmax3,0:rmax3)
        complex(rk) Cshift(0:r2,0:r2,0:r2)
        complex(rk) Cuvshift(0:r2,0:r2,0:r2)
        complex(rk) m02,m12,m22
        real(rk) Cij_err_prelim(0:r2),Cij_err2_prelim(0:r2)
        real(rk) C00_err_prelim(0:r2),accr2_prelim(0:r2)
        real(rk) z2(2,2),z2i(2,2),tz2(2,2)
        logical case1,case2

        if (r2.le.0) then
          write(*,*) 'Cpave_dd called with r2<1. Fix that! r2 = ',r2
          stop
        endif

c dummy identifier switches off internal and external cache
        xnid(0) = 2**nmaster
        xnid(1) = 2**nmaster
        xnid(2) = 2**nmaster
        r2_aux(2**nmaster) = -1

        Cshift(0,0,0)   = C(0,0,0)
        Cuvshift(0,0,0) = 0._rk

        Cij_err_prelim(0)  = scalint_err(id)
        Cij_err2_prelim(0) = scalint_err(id)
        C00_err_prelim(0)  = 0._rk
        accr2_prelim(0)    = scalint_err(id)/abs(scalint(id))

        case1 = (m12.eq.acmplx(0._rk,0._rk)).and.
     &          (acmplx(p12).eq.m22).and.(acmplx(p01).eq.m02)
        case2 = (m22.eq.acmplx(0._rk,0._rk)).and.
     &          (acmplx(p20).eq.m02).and.(acmplx(p12).eq.m12)

c Gram and related matrices
        if (case1) then 
          q1  = p01
          q2  = p12
          q12 = (p01+p12-p20)/2._rk
        elseif (case2) then
          q1  = p12
          q2  = p20
          q12 = (p12+p20-p01)/2._rk
        else
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*)
     &        'Cpaveir_dd: inconsistent call !'  
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
          q1  = p01
          q2  = p12
          q12 = (p01+p12-p20)/2._rk
        endif

        z2(1,1)  = 2._rk*q1
        z2(1,2)  = 2._rk*q12
        z2(2,1)  = z2(1,2)
        z2(2,2)  = 2._rk*q2

        detz2    = 4._rk*(q1*q2-q12**2)

        if (abs(detz2).gt.1.e-10_rk*max(abs(p01),abs(p12),abs(p20))**2) then
          call inverse_dd(z2,z2i,detz2,2)
          do i=1,2
          do j=1,2
            tz2(i,j) = z2i(j,i)*detz2
          enddo
          enddo
        else
          tz2(1,1) = z2(2,2)
          tz2(1,2) = -z2(2,1)
          tz2(2,1) = tz2(1,2)
          tz2(2,2) = z2(1,1)
          do i=1,2
          do j=1,2
            z2i(i,j) = tz2(j,i)/detz2
          enddo
          enddo
        endif

        if (case1) then 
          call Cpave_kernel_dd(Cshift,Cuvshift,Cij_err_prelim,Cij_err2_prelim,C00_err_prelim,
     &                      p01,p20,p12,m12,m02,m22,acc,accr2_prelim,
     &                      z2,z2i,r2,xnid)
          do 101 r=0,r2
            maxbin(r) = 0
            do 101 i0=0,2*r,2
            do 101 i1=0,r-i0/2
              i2 = r-i0/2-i1
              sgn = (-1)**i1
              C(i0/2,i1,i2)   = 0._rk
              Cuv(i0/2,i1,i2) = 0._rk
              do l=0,i1
              do k=l,i1
                C(i0/2,i1,i2)   = C(i0/2,i1,i2)
     &            + sgn*BinC(i1,k)*BinC(k,l)*Cshift(i0/2,i1-k,i2+l)
                Cuv(i0/2,i1,i2) = Cuv(i0/2,i1,i2)
     &            + sgn*BinC(i1,k)*BinC(k,l)*Cuvshift(i0/2,i1-k,i2+l)
                maxbin(r) = max(maxbin(r),BinC(i1,k)*BinC(k,l))
              enddo
              enddo
101       continue

        elseif (case2) then
          call Cpave_kernel_dd(Cshift,Cuvshift,Cij_err_prelim,Cij_err2_prelim,C00_err_prelim,
     &                      p12,p01,p20,m22,m12,m02,acc,accr2_prelim,
     &                      z2,z2i,r2,xnid)
          do 102 r=0,r2
            maxbin(r) = 0
            do 102 i0=0,2*r,2
            do 102 i2=0,r-i0/2
              i1 = r-i0/2-i2
              sgn = (-1)**i2
              C(i0/2,i1,i2)   = 0._rk
              Cuv(i0/2,i1,i2) = 0._rk
              do l=0,i2
              do k=l,i2
                C(i0/2,i1,i2)   = C(i0/2,i1,i2)
     &            + sgn*BinC(i2,k)*BinC(k,l)*Cshift(i0/2,i1+l,i2-k)
                Cuv(i0/2,i1,i2) = Cuv(i0/2,i1,i2)
     &            + sgn*BinC(i2,k)*BinC(k,l)*Cuvshift(i0/2,i1+l,i2-k)
                maxbin(r) = max(maxbin(r),BinC(i1,k)*BinC(k,l))
              enddo
              enddo
102       continue

        else
          acc = 1.e35_rk
          return
        endif

        do r=0,r2
          Cij_err_newprelim(tid(id),r) = Cij_err_prelim(r) * maxbin(r)
          C00_err_newprelim(tid(id),r) = C00_err_prelim(r) * maxbin(r)
          accr2_aux(tid(id),r) = accr2_prelim(r) * maxbin(r)
        enddo

        end

**********************************************************************
        subroutine Cpave_kernel_dd(C,Cuv,
     &               Cij_err_prelim,Cij_err2_prelim,C00_err_prelim,
     &               p01,p12,p20,m02,m12,m22,acc,accr2_prelim,
     &               z2,z2i,r2,nid)
**********************************************************************
*       kernel for Passarino-Veltman reduction
*
*       3-point coefficients  
*       C(i,j,k) = C_{0...01...12...2}(p01,p12,p20,m02,m12,m22)
*                     \___/\___/\___/
*                      2i    j    k   indices
*       of rank r=i+j+k with 1 <= r <= r2
*
*       Cuv(i,j,k) = coefficient of 1/eps in C(i,j,k),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       23.2.2016 Stefan Dittmaier
**********************************************************************
        implicit real(rk) (a-z)

c local variables
        integer r2,i(2),j(2),k,l(2),n,r,i0,i1,i2,i12
        integer del(2,2),nid(0:nmax-1)
        data del/1,0,0,1/
        complex(rk) B0(0:r2-1,0:r2-1,0:r2-1)
        complex(rk) Buv0(0:r2-1,0:r2-1,0:r2-1)
        complex(rk) B_n(0:r2-1,0:r2-1),Buv_n(0:r2-1,0:r2-1)
        complex(rk) S(2,0:r2-1,0:r2-1)
        complex(rk) C(0:r2,0:r2,0:r2)
        complex(rk) Cuv(0:r2,0:r2,0:r2)
        complex(rk) m02,m12,m22,f(2),vers(2)
        real(rk) Cij_err_prelim(0:r2),Cij_err2_prelim(0:r2)
        real(rk) C00_err_prelim(0:r2),accr2_prelim(0:r2)
        real(rk) z2(2,2),z2i(2,2)

        f(1) = p01-m12+m02
        f(2) = p20-m22+m02

        maxz  = max(abs(z2(1,1)),abs(z2(1,2)),abs(z2(2,2)))
        maxzi = max(abs(z2i(1,1)),abs(z2i(1,2)),abs(z2i(2,2)))
        ziff  = max( abs(z2i(1,1)*f(1)**2),abs(z2i(1,2)*f(1)*f(2)),
     &                   abs(z2i(2,2)*f(2)**2) )
        maxzif = max( abs(z2i(1,1)*f(1)),abs(z2i(1,2)*f(2)),
     &                   abs(z2i(2,1)*f(1)),abs(z2i(2,2)*f(2)) )

        call B0_dd(B0,Buv0 ,p12,m12,m22,r2-1,nid(0))
        maxB0 = abs(B0(0,0,0))
        do n=1,2
          if (n.eq.1) then
            call B_dd(B_n,Buv_n,p20,m02,m22,r2-1,nid(1))
            maxB0 = max(maxB0,abs(B_n(0,0)))
          else
            call B_dd(B_n,Buv_n,p01,m02,m12,r2-1,nid(2))
            maxB0 = max(maxB0,abs(B_n(0,0)))
          endif
          do r=0,r2-1
          do i1=0,r
            i2 = r-i1
            S(n,i1,i2) = -B0(0,i1,i2)
            if ((n.eq.1).and.(i1.eq.0)) S(1,i1,i2) = S(1,i1,i2) + B_n(0,i2)
            if ((n.eq.2).and.(i2.eq.0)) S(2,i1,i2) = S(2,i1,i2) + B_n(0,i1)
          enddo
          enddo
        enddo

c initialization of error propagation for PaVe
        Bij_err = dprec_dd * max(1._rk,maxB0)
        acc = accr2_prelim(0)

        Cmax = abs(C(0,0,0))
   
        do 100 r=1,r2

        do n=2,1,-1
          do i1=0,r-1
            i2 = r-1-i1
              S(n,i1,i2) = S(n,i1,i2) - f(n)*C(0,i1,i2)
          enddo
        enddo

c PaVe reduction of C_{00...} of rank r -- Eq.(5.10)
          do 102 i0=2,r,2
          i12 = r-i0
          do 102 i1=0,i12
            i2 = i12-i1
            Cuv(i0/2,i1,i2) = 1._rk/2._rk/(i0+i12)*(
     &              Buv0(i0/2-1,i1,i2) + 2._rk*m02*Cuv(i0/2-1,i1,i2)
     &            + f(1)*Cuv(i0/2-1,i1+1,i2) + f(2)*Cuv(i0/2-1,i1,i2+1) )
            C(i0/2,i1,i2)   = 1._rk/2._rk/(i0+i12)*( 4._rk*Cuv(i0/2,i1,i2)
     &            + B0(i0/2-1,i1,i2) + 2._rk*m02*C(i0/2-1,i1,i2)
     &            + f(1)*C(i0/2-1,i1+1,i2) + f(2)*C(i0/2-1,i1,i2+1) )
102       continue

c PaVe reduction of C(0,...) -- Eq.(5.11)
c NOTE: C(0,...) are UV finite !
        Csym_err = 0._rk
        do 104 i1=0,r-1
          i(1) = i1
          i(2) = r-i(1)-1
          do 104 k=2,1,-1
            j(1) = i(1)+del(k,1)
            j(2) = i(2)+del(k,2)
            C(0,j(1),j(2))   = 0._rk
            Cuv(0,j(1),j(2)) = 0._rk
            do n=1,2
              l(1) = i(1)-del(n,1)
              l(2) = i(2)-del(n,2)
              C(0,j(1),j(2))   = C(0,j(1),j(2)) 
     &                           + z2i(k,n)*S(n,i(1),i(2))
              if (i(n).ne.0) then
                C(0,j(1),j(2))   = C(0,j(1),j(2)) 
     &                             -2._rk*z2i(k,n)*i(n)*C(1,l(1),l(2))
              endif
            enddo    
          if ((j(1).ne.0).and.(j(2).ne.0)) then
            vers(k) = C(0,j(1),j(2))
            if (k.eq.2) then
              Csym_err = max(Csym_err,abs(vers(1)-vers(2)))
            endif
          endif
104       continue

c error propagation for PaVe
        if (r.ge.2) then
          C00_err_prelim(r) = max(Bij_err,abs(m02)*Cij_err_prelim(r-2),
     &      maxzif*Bij_err,ziff*Cij_err_prelim(r-2),
     &      maxzif*C00_err_prelim(r-1))
        else
          C00_err_prelim(1) = 0._rk
        endif
        Cij_err_prelim(r)  = max(Csym_err,maxzi*Bij_err,
     &    maxzif*Cij_err_prelim(r-1),maxzi*C00_err_prelim(r))
        Cij_err2_prelim(r) = max(maxzi*Bij_err,
     &    maxzif*Cij_err2_prelim(r-1),maxzi*C00_err_prelim(r))
     &    / sqrt(maxz*maxzi)

c find maximal value for |C(0,...)| of rank r
        i0=0
        i12 = r-i0
        do i1=0,i12
          i2=i12-i1
          Cmax = max(Cmax,abs(C(i0/2,i1,i2)))
        enddo
        acc = max(acc,Cij_err_prelim(r)/Cmax)
c        Cij_err_prelim(r) = acc*Cmax

        accr2_prelim(r) = acc

100     continue

c PaVe reduction of C_{00...} of rank r=r2+1, ... ,r2+r2 -- Eq.(5.10)
          do 105 r=r2+1,2*r2
          do 105 i0=2*(r-r2),r,2
          i12 = r-i0
          do 105 i1=0,i12
            i2 = i12-i1
            Cuv(i0/2,i1,i2) = 1._rk/2._rk/(i0+i12)*(
     &              Buv0(i0/2-1,i1,i2) + 2._rk*m02*Cuv(i0/2-1,i1,i2)
     &            + f(1)*Cuv(i0/2-1,i1+1,i2) + f(2)*Cuv(i0/2-1,i1,i2+1) )
            C(i0/2,i1,i2)   = 1._rk/2._rk/(i0+i12)*( 4._rk*Cuv(i0/2,i1,i2)
     &            + B0(i0/2-1,i1,i2) + 2._rk*m02*C(i0/2-1,i1,i2)
     &            + f(1)*C(i0/2-1,i1+1,i2) + f(2)*C(i0/2-1,i1,i2+1) )
105       continue

        end

**********************************************************************
        subroutine Calpave_dd(C,Cuv,
     &              p01,p12,p20,m02,m12,m22,
     &              acc,f,detx2,tx2,r2,id,nid)
**********************************************************************
*       Alternative Passarino-Veltman reduction
*
*       3-point coefficients  
*       C(i,j,k) = C_{0...01...12...2}(p01,p12,p20,m02,m12,m22)
*                     \___/\___/\___/
*                      2i    j    k   indices
*       of rank r=i+j+k with 1 <= r <= r2
*
*       Cuv(i,j,k) = coefficient of 1/eps in C(i,j,k),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       10.2.2009 Stefan Dittmaier
**********************************************************************
        implicit real(rk) (a-z)

c local variables
        integer r,r2,id,nid(0:nmax-1)
        integer del(2,2),i(2),j(2),k,l(2),n,i0,i1,i2,i12
        data del/1,0,0,1/
        complex(rk) B0(0:r2-1,0:r2-1,0:r2-1)
        complex(rk) Buv0(0:r2-1,0:r2-1,0:r2-1)
        complex(rk) B_n(0:r2-1,0:r2-1),Buv_n(0:r2-1,0:r2-1)
        complex(rk) Sh(2,0:r2-1,0:r2-1,0:r2-1)
        complex(rk) C(0:rmax3,0:rmax3,0:rmax3)
        complex(rk) Cuv(0:rmax3,0:rmax3,0:rmax3)
        complex(rk) m02,m12,m22,f(2),vers(2)
        complex(rk) detx2,tx2(0:2,0:2)

        if (r2.le.0) then
          write(*,*) 'Calpave_dd called with r2<1. Fix that! r2 = ',r2
          stop
        endif

c initialization of error propagation for alternative PaVe
        do r=0,rmax3
          Cij_err_newprelim(tid(id),r) = 0._rk
          C00_err_newprelim(tid(id),r) = 0._rk
        enddo

        call B0_dd(B0,Buv0 ,p12,m12,m22,r2-1,nid(0))
        maxB0 = abs(B0(0,0,0))
        
        do n=1,2
          if (n.eq.1) then
            call B_dd(B_n,Buv_n,p20,m02,m22,r2-1,nid(1))
            maxB0 = max(maxB0,abs(B_n(0,0)))
          else
            call B_dd(B_n,Buv_n,p01,m02,m12,r2-1,nid(2))
            maxB0 = max(maxB0,abs(B_n(0,0)))
          endif
          do r=0,r2-1
          do i0=0,r,2
          do i1=0,r-i0
            i2 = r-i0-i1
            Sh(n,i0/2,i1,i2) = -B0(i0/2,i1,i2) 
            if ((n.eq.1).and.(i1.eq.0)) Sh(1,i0/2,i1,i2) = Sh(1,i0/2,i1,i2) + B_n(i0/2,i2)
            if ((n.eq.2).and.(i2.eq.0)) Sh(2,i0/2,i1,i2) = Sh(2,i0/2,i1,i2) + B_n(i0/2,i1)
          enddo
          enddo
          enddo
        enddo

        Cuv(0,0,0) = 0._rk

        Cmax = abs(C(0,0,0))
        Bij_err = dprec_dd * max(1._rk,maxB0)
        Cij_err_newprelim(tid(id),0) = scalint_err(id)
        acc = scalint_err(id)/Cmax
        accr2_newprelim(tid(id),0) = acc

        maxtx20n = max(abs(tx2(0,1)),abs(tx2(0,2)))
        maxtx2kn = max(abs(tx2(1,1)),abs(tx2(1,2)),abs(tx2(2,2)))

c alternative PaVe reduction
        do 100 r=1,r2

c reduction of C_{00...}
        do 105 i0=0,r-1,2
        do 105 i1=0,r-1-i0
        i2   = r-1-i0-i1
        i(1) = i1
        i(2) = i2
c Cuv_{00...} from conventional PaVe reduction -- Eq.(5.10)
          if (i0.eq.0) then
            Cuv(0,i1+1,i2) = 0._rk
            Cuv(0,i1,i2+1) = 0._rk
          endif
          Cuv(i0/2+1,i1,i2) = 1._rk/dble(2*r+2)*(
     &            Buv0(i0/2,i1,i2) + 2._rk*m02*Cuv(i0/2,i1,i2)
     &          + f(1)*Cuv(i0/2,i1+1,i2) + f(2)*Cuv(i0/2,i1,i2+1) )

c alternative PaVe reduction of C_{00...} -- Eq.(5.16)
          C(i0/2+1,i1,i2) = 1._rk/dble(2*r+2)*(
     &            4._rk*Cuv(i0/2+1,i1,i2) + B0(i0/2,i1,i2)
     &          + detx2/tx2(0,0)*C(i0/2,i1,i2) )
          do n=1,2
            l(1) = i1-del(n,1)
            l(2) = i2-del(n,2)
            C(i0/2+1,i1,i2) = C(i0/2+1,i1,i2) 
     &           - tx2(0,n)/tx2(0,0)/dble(2*r+2)*Sh(n,i0/2,i1,i2)
            if (i(n).gt.0) then
              C(i0/2+1,i1,i2) = C(i0/2+1,i1,i2)
     &           + tx2(0,n)/tx2(0,0)*i(n)/dble(r+1)*C(i0/2+1,l(1),l(2))
            endif
          enddo
105     continue

c*** Cayley reduction of all C(0,i1,i2) with i1+i2 > 0 -- Eq.(5.15)
c       compare two versions of C(0,i1,i2):  
c                       C(0,i1+1,i2) vs. C(0,j1,j2+1) 
c       and deduce error estimate from difference
        Csym_err = 0._rk
        do 120 i1=0,r-1
        i(1) = i1
        i(2) = r-1-i1
        do 120 k=2,1,-1
          j(1) = i(1)+del(k,1)
          j(2) = i(2)+del(k,2)
          Cuv(0,j(1),j(2)) = 0._rk
          C(0,j(1),j(2))   = tx2(0,k)/detx2*( 
     &          2._rk*(r+1)*C(1,i(1),i(2)) -4._rk*Cuv(1,i(1),i(2))
     &        - B0(0,i(1),i(2)) )

          do n=1,2
            l(1) = i(1)-del(n,1)
            l(2) = i(2)-del(n,2)
            C(0,j(1),j(2)) = C(0,j(1),j(2)) 
     &                  +tx2(k,n)/detx2*Sh(n,0,i(1),i(2))
            if (i(n).ne.0) C(0,j(1),j(2)) = C(0,j(1),j(2)) 
     &                  -2._rk*tx2(k,n)/detx2*i(n)*C(1,l(1),l(2))
          enddo
          if ((j(1).ne.0).and.(j(2).ne.0)) then
            vers(k) = C(0,j(1),j(2))
            if (k.eq.2) then
              Csym_err = max(Csym_err,abs(vers(1)-vers(2)))
            endif
          endif

120     continue

c error propagation for alternative PaVe
        C00_err_newprelim(tid(id),r+1) = max( Bij_err,
     &    abs(detx2/tx2(0,0))*Cij_err_newprelim(tid(id),r-1),
     &    maxtx20n/abs(tx2(0,0))*max(Bij_err,C00_err_newprelim(tid(id),r)) )
        Cij_err_newprelim(tid(id),r) = max(Csym_err, 
     &    maxtx20n/abs(detx2)*max(C00_err_newprelim(tid(id),r+1),Bij_err),
     &    maxtx2kn/abs(detx2)*max(C00_err_newprelim(tid(id),r),Bij_err) )

c find maximal value for |C(0,...)| of rank r
        i0=0
        i12 = r-i0
        do i1=0,i12
          i2=i12-i1
          Cmax = max(Cmax,abs(C(i0/2,i1,i2)))
        enddo
        acc = max(acc,Cij_err_newprelim(tid(id),r)/Cmax)
        Cij_err_newprelim(tid(id),r) = acc*Cmax

        accr2_newprelim(tid(id),r) = acc

100     continue

c PaVe reduction of C_{00...} of rank r=r2+1, ... ,r2+r2 -- Eq.(5.10)
          do 400 r=r2+1,r2+r2
          do 400 i0=2*(r-r2),r,2
          i12 = r-i0
          do 400 i1=0,i12
            i2 = i12-i1
            Cuv(i0/2,i1,i2) = 1._rk/2._rk/(i0+i12)*(
     &              Buv0(i0/2-1,i1,i2) + 2._rk*m02*Cuv(i0/2-1,i1,i2)
     &            + f(1)*Cuv(i0/2-1,i1+1,i2) + f(2)*Cuv(i0/2-1,i1,i2+1) )
            C(i0/2,i1,i2)   = 1._rk/2._rk/(i0+i12)*( 4._rk*Cuv(i0/2,i1,i2)
     &            + B0(i0/2-1,i1,i2) + 2._rk*m02*C(i0/2-1,i1,i2)
     &            + f(1)*C(i0/2-1,i1+1,i2) + f(2)*C(i0/2-1,i1,i2+1) )
400       continue

        end

**********************************************************************
        subroutine Cgram_dd(C,Cuv,
     &              p01,p12,p20,m02,m12,m22,j,k,l,acc,
     &              detz2,tz2,ttz2,tx2,f,r2,r2eff,id,nid)
**********************************************************************
*       Expansion for small Gram determinant
*
*       3-point coefficients  
*       C(i,j,k) = C_{0...01...12...2}(p01,p12,p20,m02,m12,m22)
*                     \___/\___/\___/
*                      2i    j    k   indices
*       of rank r=i+j+k with 0 <= r <= r2
*
*       Cuv(i,j,k) = coefficient of 1/eps in C(i,j,k),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       17.7.2006 Stefan Dittmaier
**********************************************************************
        implicit real(rk) (a-z)

c local variables
        integer r,r2,i(2),j,k,l,m,n,rup,rupmax,r2eff
        integer del(2,2),i0,i1,i2,i12,id,nid(0:nmax-1)
        data del/1,0,0,1/
        complex(rk) B0(0:ritmax+2,0:ritmax+2,0:ritmax+2)
        complex(rk) Buv0(0:ritmax+2,0:ritmax+2,0:ritmax+2)
        complex(rk) B_n(0:ritmax+2,0:ritmax+2),Buv_n(0:ritmax+2,0:ritmax+2)
        complex(rk) Cstore(2,0:rmax3)
        complex(rk) C(0:rmax3,0:rmax3,0:rmax3)
        complex(rk) Cuv(0:rmax3,0:rmax3,0:rmax3)
        complex(rk) m02,m12,m22,f(2),tx2(0:2,0:2)
        complex(rk) Sh3(2,0:ritmax+2,0:ritmax+2,0:ritmax+2)
        real(rk) tz2(2,2),ttz2(2,2,2,2)
        real(rk) Cmax(0:rmax3)

        rupmax = ritmax+2
        if (rupmax.gt.rmax3-1) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*)
     &        'Cgram_dd called with rmax3-1 < ritmax = ',ritmax
            write(outchannel,*) 'This is fatal. Fix it!'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        endif

c initializations
c----------------
        do r=0,rmax3
          Cmax(r) = abs(scalint(id))
          Cij_err_newprelim(tid(id),r) = 0._rk
          C00_err_newprelim(tid(id),r) = 0._rk
        enddo

        rup=-1

        do i1=0,rup+1
        i2 = rup+1-i1
          Cuv(0,i1,i2) = 0._rk
        enddo

        accprev1 = 1.e30_rk
        accprev2 = 1.e30_rk

        call B0_dd(B0,Buv0,p12,m12,m22,rupmax,nid(0))
        maxB0 = abs(B0(0,0,0))

c S functions
        do n=1,2
          if (n.eq.1) then
            call B_dd(B_n,Buv_n,p20,m02,m22,rupmax,nid(1))
            maxB0 = max(maxB0,abs(B_n(0,0)))
          else
            call B_dd(B_n,Buv_n,p01,m02,m12,rupmax,nid(2))
            maxB0 = max(maxB0,abs(B_n(0,0)))
          endif
          do r=0,rupmax
          do i0=0,r,2
          do i1=0,r-i0
          i2 = r-i0-i1
              Sh3(n,i0/2,i1,i2) = -B0(i0/2,i1,i2)
            if ((n.eq.1).and.(i1.eq.0)) Sh3(1,i0/2,i1,i2)
     &                    = Sh3(1,i0/2,i1,i2) + B_n(i0/2,i2)
            if ((n.eq.2).and.(i2.eq.0)) Sh3(2,i0/2,i1,i2)
     &                    = Sh3(2,i0/2,i1,i2) + B_n(i0/2,i1)
          enddo
          enddo
          enddo
        enddo

c Cuv_{00...} from PaVe reduction -- Eq.(5.10)
        do r=0,rupmax+1
        do i0=0,r,2
        do i1=0,r-i0
        i2 = r-i0-i1
          if (i0.eq.0) then
            Cuv(i0/2,i1,i2) = 0
          else
            Cuv(i0/2,i1,i2) = 1._rk/2._rk/dble(r)*(
     &              Buv0(i0/2-1,i1,i2) + 2._rk*m02*Cuv(i0/2-1,i1,i2)
     &            + f(1)*Cuv(i0/2-1,i1+1,i2) + f(2)*Cuv(i0/2-1,i1,i2+1) )
          endif
        enddo
        enddo
        enddo

500     continue
        rup = rup+1

c initialize higher-rank tensors
        do i0=0,rup+1,2
        do i1=0,rup+1-i0
        i2 = rup+1-i0-i1
          C(i0/2,i1,i2) = 0._rk
        enddo
        enddo

c store previous approximation for error estimate
        if ((rup.ge.r2).or.(rup.ge.rupmax-2)) then
          do n=0,rup-1
            Cstore(1,n) = C(0,n,0) 
            Cstore(2,n) = C(0,0,n) 
          enddo
        endif

c iteration: rup,  step: r
        do 400 r=rup,0,-1

c C_{00...} from iteration Eq.(5.40)
        do 102 i0=2*((r+1)/2),2,-2
        do 102 i1=0,r+1-i0
        i2 = r+1-i0-i1
        i(1) = i1
        i(2) = i2
          n = 3-k
          m = 3-l
          C(i0/2,i1,i2) = 1._rk/2._rk/(1._rk+i0+2*i1+2*i2)*(
     &      4._rk*Cuv(i0/2,i1,i2)
     &      - detz2/tz2(k,l)
     &          *C(i0/2-1,i1+del(k,1)+del(l,1),i2+del(k,2)+del(l,2))
     &      + 2._rk*B0(i0/2-1,i1,i2)
crrr terms rearranged - start (see also below under crrr)
crrr     &      + 2._rk*m02*C(i0/2-1,i1,i2)
     &      + tx2(k,l)/tz2(k,l)*C(i0/2-1,i1,i2)
crrr terms rearranged - end
     &      + tz2(1,l)/tz2(k,l)
     &          *Sh3(1,i0/2-1,i1+del(k,1),i2+del(k,2))
     &      + tz2(2,l)/tz2(k,l)
     &          *Sh3(2,i0/2-1,i1+del(k,1),i2+del(k,2))
     &      - Sh3(1,i0/2-1,i1+1,i2) 
     &      - Sh3(2,i0/2-1,i1,i2+1)
     &      - ttz2(k,n,l,m)/tz2(k,l)*f(n)*Sh3(m,i0/2-1,i1,i2) 
crrr     &      + ttz2(k,n,l,m)/tz2(k,l)*f(n)*f(m)*C(i0/2-1,i1,i2)       
     &      ) 
          if (i(n).ne.0) C(i0/2,i1,i2) = C(i0/2,i1,i2) 
     &      - i(n)/(1._rk+i0+2*i1+2*i2)*ttz2(k,n,l,m)/tz2(k,l)
     &          *(Sh3(m,i0/2,i1-del(1,n),i2-del(2,n))
     &            -f(m)*C(i0/2,i1-del(1,n),i2-del(2,n)))
          if (i(m).ne.0) C(i0/2,i1,i2) = C(i0/2,i1,i2) 
     &      + i(m)/(1._rk+i0+2*i1+2*i2)*ttz2(k,n,l,m)/tz2(k,l)
     &          *f(n)*C(i0/2,i1-del(1,m),i2-del(2,m))
          if (i(n)*i(m).ne.0) then
            if (n.ne.m) then
              C(i0/2,i1,i2) = C(i0/2,i1,i2) 
     &          +2._rk*i(n)*i(m)/(1._rk+i0+2*i1+2*i2)*ttz2(k,n,l,m)/tz2(k,l)
     &            *C(i0/2+1,i1-del(1,m)-del(1,n),i2-del(2,m)-del(2,n))
            elseif (i(n).gt.1) then
              C(i0/2,i1,i2) = C(i0/2,i1,i2) 
     &          + 2._rk*i(n)*(i(n)-1)/(1._rk+i0+2*i1+2*i2)
     &            *ttz2(k,n,l,m)/tz2(k,l)
     &            *C(i0/2+1,i1-2*del(1,n),i2-2*del(2,n))
            endif
          endif
102     continue

c coefficients C(0,i1,i2) from Eq.(5.38)
        Cmax(r) = 0._rk
        i0 = 0
        do 103 i1=0,r
        i2 = r-i0-i1
        i(1) = i1
        i(2) = i2
          C(i0/2,i1,i2) = 0._rk
          do 104 n=1,2
            C(i0/2,i1,i2) = C(i0/2,i1,i2)-tz2(j,n)*Sh3(n,i0/2,i1,i2) 
            if (i(n).ne.0) C(i0/2,i1,i2) = C(i0/2,i1,i2)
     &        + 2._rk*i(n)*tz2(j,n)*C(i0/2+1,i1-del(1,n),i2-del(2,n))
104       continue
          C(i0/2,i1,i2) = ( C(i0/2,i1,i2)
     &        + detz2*C(i0/2,i1+del(1,j),i2+del(2,j)) )/tx2(0,j) 
        Cmax(r) = max(Cmax(r),abs(C(i0/2,i1,i2)))
103     continue

400     continue

c estimate precision from last improvements
        if ((rup.le.r2).and.(rup.lt.rupmax-2)) then
          acc = 1.e30_rk
        else
c         acc1 = abs(C(0,0,0)/scalint(id)-1._rk)
          acc1 = 0._rk
          do n=0,rup-1
            acc1 = max(acc1,abs(Cstore(1,n)-C(0,n,0))/Cmax(n))
            acc1 = max(acc1,abs(Cstore(2,n)-C(0,0,n))/Cmax(n))
            accr2_newprelim(tid(id),n) = acc1
          enddo
          acc = accr2_newprelim(tid(id),r2) 
        endif

c error propagation for Gram expansion
          Bij_err = dprec_dd * max(1._rk,maxB0)
          do 800 r=rup,0,-1
          if (r.ge.1) then
            C00_err_newprelim(tid(id),r+1) = max( 
     &        abs(detz2)/abs(tz2(k,l))*Cij_err_newprelim(tid(id),r+1),
     &        Bij_err, 
crrr     &        abs(m02)*Cij_err_newprelim(tid(id),r-1),
     &        max( maxttx0klm(id)*Bij_err,
crrr     &             ttzff_kl(id)*Cij_err_newprelim(tid(id),r-1),
     &             abs(tx2(k,l))*Cij_err_newprelim(tid(id),r-1),
     &             maxttx0klm(id)*C00_err_newprelim(tid(id),r) )
     &         /abs(tz2(k,l)) )
          endif
          Cij_err_newprelim(tid(id),r) = 
     &          max( maxtz_nj(id)*max(Bij_err,C00_err_newprelim(tid(id),r+1))
     &            ,abs(detz2)*Cij_err_newprelim(tid(id),r+1) )/abs(tx2(0,j)) 
          accr2_newprelim(tid(id),r) = max(accr2_newprelim(tid(id),r),
     &            Cij_err_newprelim(tid(id),r)/Cmax(r))
800       continue
          do n=0,r2
            acc = max(acc,accr2_newprelim(tid(id),n))
          enddo

        r2eff = rup

c stop if accuracy becomes worse
        if ((rup.ge.r2+3).and.(acc.gt.accprev1).and.
     &      (accprev1.gt.accprev2)) goto 600
        accprev2 = accprev1
        accprev1 = acc

c repeat iteration if necessary
        if ((acc.gt.cacc).and.(rup.le.rupmax-1)) goto 500

600     continue

c PaVe reduction of C_{00...} of rank r=rup+1, ... ,2*rup -- Eq.(5.10)
          do 601 r=rup+1,2*rup
          do 601 i0=2*(r-rup),r,2
          i12 = r-i0
          do 601 i1=0,i12
            i2 = i12-i1
            Cuv(i0/2,i1,i2) = 1._rk/2._rk/(i0+i12)*(
     &              Buv0(i0/2-1,i1,i2) + 2._rk*m02*Cuv(i0/2-1,i1,i2)
     &            + f(1)*Cuv(i0/2-1,i1+1,i2) + f(2)*Cuv(i0/2-1,i1,i2+1) )
            C(i0/2,i1,i2)   = 1._rk/2._rk/(i0+i12)*( 4._rk*Cuv(i0/2,i1,i2)
     &            + B0(i0/2-1,i1,i2) + 2._rk*m02*C(i0/2-1,i1,i2)
     &            + f(1)*C(i0/2-1,i1+1,i2) + f(2)*C(i0/2-1,i1,i2+1) )
601       continue

c final absolute error
        i0=0
        do r=0,r2eff
          Cmax(r) = 0._rk
        i12 = r-i0
        do i1=0,i12
          i2=i12-i1
          Cmax(r) = max(Cmax(r),abs(C(i0/2,i1,i2)))
        enddo
        Cij_err_newprelim(tid(id),r) = accr2_newprelim(tid(id),r)*Cmax(r)
        enddo

        end

**********************************************************************
        subroutine Cgram2_dd(C,Cuv,
     &              p01,p12,p20,m02,m12,m22,acc,
     &              detz2,detx2,tx2,f,r2,r2eff,id,nid)
**********************************************************************
*       Expansion for small Gram determinant - version 2
*       Limit: |detZ| << |detX|/M^2,  max{|tX_0j|}    (M = typ. scale)
*
*       3-point coefficients  
*       C(i,j,k) = C_{0...01...12...2}(p01,p12,p20,m02,m12,m22)
*                     \___/\___/\___/
*                      2i    j    k   indices
*       of rank r=i+j+k with 0 <= r <= r2
*
*       Cuv(i,j,k) = coefficient of 1/eps in C(i,j,k),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       30.7.2014 Stefan Dittmaier
**********************************************************************
        implicit real(rk) (a-z)

c local variables
        integer r,r2,i(2),ii(2),n,rup,rupmax,r2eff
        integer del(2,2),i0,i1,i12,i2,id,nid(0:nmax-1)
        data del/1,0,0,1/
        complex(rk) B0(0:ritmax+2,0:ritmax+2,0:ritmax+2)
        complex(rk) Buv0(0:ritmax+2,0:ritmax+2,0:ritmax+2)
        complex(rk) B_1(0:ritmax+2,0:ritmax+2),Buv_1(0:ritmax+2,0:ritmax+2)
        complex(rk) B_2(0:ritmax+2,0:ritmax+2),Buv_2(0:ritmax+2,0:ritmax+2)
        complex(rk) Cstore(2,0:rmax3)
        complex(rk) C(0:rmax3,0:rmax3,0:rmax3)
        complex(rk) Cuv(0:rmax3,0:rmax3,0:rmax3)
        complex(rk) m02,m12,m22,f(2),tx2(0:2,0:2),detx2,Sh3
        real(rk) Cmax(0:rmax3)

        rupmax = ritmax+2
        if (rupmax.gt.rmax3-1) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*)
     &        'Cgram2_dd called with rmax3-1 < ritmax = ',ritmax
            write(outchannel,*) 'This is fatal. Fix it!'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        endif

c start iteration
c----------------
        do r=0,rmax3
          Cmax(r) = abs(scalint(id))
          Cij_err_newprelim(tid(id),r) = 0._rk
          C00_err_newprelim(tid(id),r) = 0._rk
        enddo
        rup = -1
        Cuv(0,0,0) = 0._rk
        accprev1 = 1.e30_rk
        accprev2 = 1.e30_rk
        maxtx20n = max(abs(tx2(0,1)),abs(tx2(0,2)))

        call B0_dd(B0,Buv0,p12,m12,m22,rupmax,nid(0))
        call B_dd(B_1,Buv_1,p20,m02,m22,rupmax,nid(1))
        call B_dd(B_2,Buv_2,p01,m02,m12,rupmax,nid(2))

c Cuv_{00...} from PaVe reduction -- Eq.(5.10)
        do r=0,rupmax+1
        do i0=0,2*r,2
        do i1=0,r-i0/2
        i2 = r-i0/2-i1
          if (i0.lt.2) then
            Cuv(i0/2,i1,i2) = 0._rk
          else
            Cuv(i0/2,i1,i2) = 1._rk/2._rk/dble(i0+i1+i2)*(
     &              Buv0(i0/2-1,i1,i2) + 2._rk*m02*Cuv(i0/2-1,i1,i2)
     &            + f(1)*Cuv(i0/2-1,i1+1,i2) + f(2)*Cuv(i0/2-1,i1,i2+1) )
          endif
        enddo
        enddo
        enddo

500     continue
        rup = rup+1

c initialize higher-rank tensors
        do i0=0,2*rup+2,2
        do i1=0,rup+1-i0/2
        i2   = rup+1-i0/2-i1
          C(i0/2,i1,i2) = 0._rk
        enddo
        enddo

c store previous approximation for error estimate
        if ((rup.ge.r2).or.(rup.ge.rupmax-2)) then
          do n=0,rup-1
            Cstore(1,n) = C(0,n,0) 
            Cstore(2,n) = C(0,0,n) 
          enddo
        endif

c iteration: rup,  step: r
        do 400 r=rup,0,-1

c reduction -- Eq.(5.14)
        do 105 i0=2*r,0,-2
        do 105 i1=0,r-i0/2
        i2   = r-i0/2-i1
        i(1) = i1
        i(2) = i2
          C(i0/2,i1,i2) = detz2/detx2*(
     &                      2*(i0+i1+i2+2)*C(i0/2+1,i1,i2)
     &                      - 4._rk*Cuv(i0/2+1,i1,i2) - B0(i0/2,i1,i2) )
          do n=1,2
            ii(1) = i1-del(n,1)
            ii(2) = i2-del(n,2)
            Sh3 = -B0(i0/2,i1,i2) 
            if ((n.eq.1).and.(i1.eq.0)) Sh3 = Sh3 + B_1(i0/2,i2)
            if ((n.eq.2).and.(i2.eq.0)) Sh3 = Sh3 + B_2(i0/2,i1)
            C(i0/2,i1,i2) = C(i0/2,i1,i2)
     &           + tx2(0,n)/detx2*Sh3
            if (i(n).gt.0) then
              C(i0/2,i1,i2) = C(i0/2,i1,i2)
     &           - 2._rk*tx2(0,n)/detx2*i(n)*C(i0/2+1,ii(1),ii(2))
            endif
          enddo
        if (i0.eq.0) Cmax(r) = max(Cmax(r),abs(C(i0/2,i1,i2)))
105     continue

400     continue

c estimate precision from last improvements
        if ((rup.le.r2).and.(rup.lt.rupmax-2)) then
          acc = 1.e30_rk
        else
c         acc1 = abs(C(0,0,0)/scalint(id)-1._rk)
          acc1 = 0._rk
          do n=0,rup-1
            acc1 = max(acc1,abs(Cstore(1,n)-C(0,n,0))/Cmax(n))
            acc1 = max(acc1,abs(Cstore(2,n)-C(0,0,n))/Cmax(n))
            accr2_newprelim(tid(id),n) = acc1
          enddo
          acc = accr2_newprelim(tid(id),r2)
        endif

c error propagation for Gram expansion
        scale2 = ( abs(p01)+abs(p12)+abs(p20)
     &            +abs(m02)+abs(m12)+abs(m22) )/6._rk
        Bij_err = dprec_dd * max(1._rk,abs(B0(0,0,0)),abs(B_1(0,0)),abs(B_2(0,0)))
        B00_err = Bij_err * scale2

        do 800 r=rup,0,-1
          C00_err_newprelim(tid(id),r) =
     &             abs(detz2/detx2)*B00_err
     &           + maxtx20n/abs(detx2)*B00_err
          Cij_err_newprelim(tid(id),r) =
     &             abs(detz2/detx2)*(C00_err_newprelim(tid(id),r+2)+Bij_err )
     &           + maxtx20n/abs(detx2)
     &               *(Bij_err+C00_err_newprelim(tid(id),r+1))
          accr2_newprelim(tid(id),r) = max(accr2_newprelim(tid(id),r),
     &               Cij_err_newprelim(tid(id),r)/Cmax(r))
800       continue

          do n=0,r2
            acc = max(acc,accr2_newprelim(tid(id),n))
          enddo
        r2eff = rup

c stop if accuracy becomes worse
        if ((rup.ge.r2+3).and.(acc.gt.accprev1).and.
     &      (accprev1.gt.accprev2)) goto 999
        accprev2 = accprev1
        accprev1 = acc

c repeat iteration if necessary
        if ((acc.gt.cacc).and.(rup.le.rupmax-1)) goto 500

999     continue
c final absolute error
        i0=0
        do r=0,r2eff
          Cmax(r) = 0._rk
        i12 = r-i0
        do i1=0,i12
          i2=i12-i1
          Cmax(r) = max(Cmax(r),abs(C(i0/2,i1,i2)))
        enddo
        Cij_err_newprelim(tid(id),r) = accr2_newprelim(tid(id),r)*Cmax(r)
        enddo

        end

**********************************************************************
        subroutine Cgramcayley_dd(C,Cuv,
     &              p01,p12,p20,m02,m12,m22,i,j,k,l,
     &              acc,detz2,tz2,ttz2,tx2,f,r2,r2eff,id,nid)
**********************************************************************
*       Expansion for small Gram and Cayley determinants
*
*       3-point coefficients  
*       C(i,j,k) = C_{0...01...12...2}(p01,p12,p20,m02,m12,m22)
*                     \___/\___/\___/
*                      2i    j    k   indices
*       of rank r=i+j+k with 0 <= r <= r2
*
*       Cuv(i,j,k) = coefficient of 1/eps in C(i,j,k),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       19.7.2006 Stefan Dittmaier
**********************************************************************
        implicit real(rk) (a-z)

c local variables
        integer r,rr,r2,h(2),i,j,k,l,m,n,rup,rupmax,r2eff
        integer del(2,2),i0,i1,i2,i12,il,im
        integer id,nid(0:nmax-1)
        data del/1,0,0,1/
        complex(rk) B0(0:ritmax+4,0:ritmax+4,0:ritmax+4)
        complex(rk) Buv0(0:ritmax+4,0:ritmax+4,0:ritmax+4)
        complex(rk) B_n(0:ritmax+4,0:ritmax+4),Buv_n(0:ritmax+4,0:ritmax+4)
        complex(rk) Cstore(2,0:rmax3)
        complex(rk) C(0:rmax3,0:rmax3,0:rmax3)
        complex(rk) Cuv(0:rmax3,0:rmax3,0:rmax3)
        complex(rk) m02,m12,m22,f(2),tx2(0:2,0:2)
        complex(rk) Sh3(2,0:ritmax+4,0:ritmax+4,0:ritmax+4)
        real(rk) tz2(2,2),ttz2(2,2,2,2)
        real(rk) Cmax(0:rmax3)

        rupmax = ritmax+2
        if (rupmax-1.gt.rmax3-3) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*)
     &        'Cgramcayley_dd called with rmax3-3 < ritmax = ',ritmax
            write(outchannel,*) 'This is fatal. Fix it!'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        endif

c initializations
c----------------
        do r=0,rmax3
          Cmax(r) = abs(scalint(id))
          Cij_err_newprelim(tid(id),r) = 0._rk
          C00_err_newprelim(tid(id),r) = 0._rk
        enddo

        call B0_dd(B0,Buv0,p12,m12,m22,rupmax+2,nid(0))
        maxB0 = abs(B0(0,0,0))

c S functions
        do n=1,2
          if (n.eq.1) then
            call B_dd(B_n,Buv_n,p20,m02,m22,rupmax+2,nid(1))
            maxB0 = max(maxB0,abs(B_n(0,0)))
          else
            call B_dd(B_n,Buv_n,p01,m02,m12,rupmax+2,nid(2))
            maxB0 = max(maxB0,abs(B_n(0,0)))
          endif
          do r=0,rupmax+2
          do i0=0,r,2
          do i1=0,r-i0
          i2 = r-i0-i1
            Sh3(n,i0/2,i1,i2) = -B0(i0/2,i1,i2) 
            if ((n.eq.1).and.(i1.eq.0)) Sh3(1,i0/2,i1,i2) 
     &                    = Sh3(1,i0/2,i1,i2) + B_n(i0/2,i2)
            if ((n.eq.2).and.(i2.eq.0)) Sh3(2,i0/2,i1,i2) 
     &                    = Sh3(2,i0/2,i1,i2) + B_n(i0/2,i1)
          enddo
          enddo
          enddo
        enddo

c Cuv_{00...} from PaVe reduction -- Eq.(5.10)
        do r=0,rupmax+3
        do i0=0,r,2
        do i1=0,r-i0
        i2 = r-i0-i1
          if (i0.eq.0) then
            Cuv(i0/2,i1,i2) = 0._rk
          else
            Cuv(i0/2,i1,i2) = 1._rk/2._rk/dble(r)*(
     &              Buv0(i0/2-1,i1,i2) + 2._rk*m02*Cuv(i0/2-1,i1,i2)
     &            + f(1)*Cuv(i0/2-1,i1+1,i2) + f(2)*Cuv(i0/2-1,i1,i2+1) )
          endif
        enddo
        enddo
        enddo

        rup = -2
        C(0,0,0)   = 0._rk
        C(0,1,0)   = 0._rk
        C(0,0,1)   = 0._rk

        accprev1 = 1.e30_rk
        accprev2 = 1.e30_rk

500     continue
        rup = rup+2

c initialize higher-rank tensors
        do r=rup+2,rup+3
        do i0=0,r,2
        do i1=0,r-i0
        i2 = r-i0-i1
          C(i0/2,i1,i2) = 0._rk
        enddo
        enddo
        enddo

c store previous approximation for error estimate
        if ((rup.ge.r2).or.(rup.ge.rupmax-2)) then
          do n=0,rup-1
            Cstore(1,n) = C(0,n,0) 
            Cstore(2,n) = C(0,0,n) 
          enddo
        endif

c iteration: rup/2,  step: rr/2
        do 400 rr=rup,0,-2
        do 400 r=rr,min(rr+1,rupmax)

c C_{00...} from iteration Eq.(5.40)
        m = 3-l
        do 102 i0=2,r+2,2
        do 102 il=r+2-i0,0,-1
        im = r+2-il-i0
        h(l) = il
        h(m) = im

c coefficients C_{00...} from Eq.(5.49)
          C(i0/2,h(1),h(2)) = 1._rk/2._rk/dble(il+1)/tz2(k,l)*(
     &        tz2(k,1)*Sh3(1,i0/2-1,h(1)+del(1,l),h(2)+del(2,l))
     &      + tz2(k,2)*Sh3(2,i0/2-1,h(1)+del(1,l),h(2)+del(2,l))
     &      + tx2(k,0)*C(i0/2-1,h(1)+del(1,l),h(2)+del(2,l))
     &      - detz2*C(i0/2-1,h(1)+del(1,l)+del(1,k),
     &                     h(2)+del(2,l)+del(2,k))              )
          if (im.gt.0) C(i0/2,h(1),h(2)) = C(i0/2,h(1),h(2)) 
     &      + 1._rk/2._rk/dble(il+1)/tz2(k,l)*(
     &          - 2._rk*im*tz2(k,m)*C(i0/2,h(1)-del(1,m)+del(1,l),
     &                              h(2)-del(2,m)+del(2,l))     )

102     continue

c coefficients C(0,i1,i2) from Eq.(5.53)
        i0 = 0
        n = 3-i
        m = 3-j
        do 103 i1=0,r
        i2 = r-i0-i1
        h(1) = i1
        h(2) = i2
          C(i0/2,i1,i2) = 1._rk/tx2(i,j)*(
     &        tz2(i,j)*( 2._rk*(2+r)*C(i0/2+1,i1,i2)
     &                  - 4._rk*Cuv(i0/2+1,i1,i2) - B0(i0/2,i1,i2) )
     &      + ttz2(i,n,j,m)*f(n)*Sh3(m,i0/2,i1,i2)
     &      + tx2(0,j)*C(i0/2,i1+del(1,i),i2+del(2,i))    )
          if (h(m).gt.0) C(i0/2,i1,i2) = C(i0/2,i1,i2) 
     &          - 2._rk*h(m)*ttz2(i,n,j,m)*f(n)/tx2(i,j)
     &            *C(i0/2+1,i1-del(1,m),i2-del(2,m))
        Cmax(r) = max(Cmax(r),abs(C(i0/2,i1,i2)))
103     continue

400     continue

c estimate precision from last improvements
        if ((rup.le.r2).and.(rup.lt.rupmax-2)) then
          acc = 1.e30_rk
        else
c         acc1 = abs(C(0,0,0)/scalint(id)-1._rk)
          acc1 = 0._rk
          do n=0,rup-1
            acc1 = max(acc1,abs(Cstore(1,n)-C(0,n,0))/Cmax(n))
            acc1 = max(acc1,abs(Cstore(2,n)-C(0,0,n))/Cmax(n))
            accr2_newprelim(tid(id),n) = acc1
          enddo
          accr2_newprelim(tid(id),rup)   = accr2_newprelim(tid(id),rup-1)
          accr2_newprelim(tid(id),rup+1) = accr2_newprelim(tid(id),rup-1)
          acc = accr2_newprelim(tid(id),r2) 
        endif

c error propagation for Gram/Cayley expansion
          Bij_err = dprec_dd * max(1._rk,maxB0)
          do 800 r=rup,0,-1
            C00_err_newprelim(tid(id),r+2) = max( Bij_err,
     &        abs(tx2(k,0))/abs(tz2(k,l))*Cij_err_newprelim(tid(id),r+1),
     &        abs(detz2)/abs(tz2(k,l))*Cij_err_newprelim(tid(id),r+2) )
            Cij_err_newprelim(tid(id),r) = max( 
     &        abs(tx2(0,j))*Cij_err_newprelim(tid(id),r+1),        
     &        abs(tz2(i,j))*max(C00_err_newprelim(tid(id),r+2),Bij_err),
     &        maxttx0ijm(id)*max(Bij_err,C00_err_newprelim(tid(id),r+1)) 
     &                             )/abs(tx2(i,j))
          accr2_newprelim(tid(id),r) = max(accr2_newprelim(tid(id),r),
     &                    Cij_err_newprelim(tid(id),r)/Cmax(r))

800       continue
          do n=0,r2
            acc = max(acc,accr2_newprelim(tid(id),n))
          enddo

        r2eff = min(rup+1,rupmax)

c stop if accuracy becomes worse
        if ((rup.ge.r2+3).and.(acc.gt.accprev1).and.
     &      (accprev1.gt.accprev2)) goto 600
        accprev2 = accprev1
        accprev1 = acc

c repeat iteration if necessary 
        if ((acc.gt.cacc).and.(rup.le.rupmax-2)) goto 500

600     continue

c PaVe reduction of C_{00...} of rank r=rup+1, ... ,2*rup -- Eq.(5.10)
          do 601 r=rup+2,2*rup+2
          do 601 i0=2*(r-rup-1),r,2
          i12 = r-i0
          do 601 i1=0,i12
            i2 = i12-i1
            Cuv(i0/2,i1,i2) = 1._rk/2._rk/(i0+i12)*(
     &              Buv0(i0/2-1,i1,i2) + 2._rk*m02*Cuv(i0/2-1,i1,i2)
     &            + f(1)*Cuv(i0/2-1,i1+1,i2) + f(2)*Cuv(i0/2-1,i1,i2+1) )
            C(i0/2,i1,i2)   = 1._rk/2._rk/(i0+i12)*( 4._rk*Cuv(i0/2,i1,i2)
     &            + B0(i0/2-1,i1,i2) + 2._rk*m02*C(i0/2-1,i1,i2)
     &            + f(1)*C(i0/2-1,i1+1,i2) + f(2)*C(i0/2-1,i1,i2+1) )
601       continue

c final absolute error
        i0=0
        do r=0,r2eff
          Cmax(r) = 0._rk
        i12 = r-i0
        do i1=0,i12
          i2=i12-i1
          Cmax(r) = max(Cmax(r),abs(C(i0/2,i1,i2)))
        enddo
        Cij_err_newprelim(tid(id),r) = accr2_newprelim(tid(id),r)*Cmax(r)
        enddo

        end

**********************************************************************
        subroutine Csm1_dd(C,Cuv,
     &              p01,p12,p20,m02,m12,m22,k,acc,z2,f,
     &              r2,r2eff,id,nid)
**********************************************************************
*       Expansion for small momenta - variant 1 (f(k) not small)
*
*       3-point coefficients  
*       C(i,j,k) = C_{0...01...12...2}(p01,p12,p20,m02,m12,m22)
*                     \___/\___/\___/
*                      2i    j    k   indices
*       of rank r=i+j+k with 0 <= r <= r2
*
*       Cuv(i,j,k) = coefficient of 1/eps in C(i,j,k),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       28.5.2013 Stefan Dittmaier
**********************************************************************
        implicit real(rk) (a-z)

c local variables
        integer r,r2,i(2),k,m,n,rup,rupmax,r2eff
        integer del(2,2),i0,i1,i2,i12,id,nid(0:nmax-1)
        data del/1,0,0,1/
        complex(rk) B0(0:ritmax+4,0:ritmax+4,0:ritmax+4)
        complex(rk) Buv0(0:ritmax+4,0:ritmax+4,0:ritmax+4)
        complex(rk) B_1(0:ritmax+4,0:ritmax+4),Buv_1(0:ritmax+4,0:ritmax+4)
        complex(rk) B_2(0:ritmax+4,0:ritmax+4),Buv_2(0:ritmax+4,0:ritmax+4)
        complex(rk) Cstore(2,0:rmax3)
        complex(rk) C(0:rmax3,0:rmax3,0:rmax3)
        complex(rk) Cuv(0:rmax3,0:rmax3,0:rmax3)
        complex(rk) m02,m12,m22,f(2),Sh3
        real(rk) z2(2,2)
        real(rk) Cmax(0:rmax3)

        rupmax = ritmax+2
        if (rupmax.gt.rmax3-1) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*)
     &        'Csm1_dd called with rmax3-1 < ritmax = ',ritmax
            write(outchannel,*) 'This is fatal. Fix it!'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        endif

c initializations 
c----------------
        maxz  = max(abs(z2(1,1)),abs(z2(1,2)),abs(z2(2,2)))

        do r=0,rmax3
          Cmax(r) = abs(scalint(id))
          Cij_err_newprelim(tid(id),r) = 0._rk
          C00_err_newprelim(tid(id),r) = 0._rk
        enddo

        rup = -1

        accprev1 = 1.e30_rk
        accprev2 = 1.e30_rk

        call B0_dd(B0,Buv0,p12,m12,m22,rupmax+2,nid(0))
        call B_dd(B_1,Buv_1,p20,m02,m22,rupmax+2,nid(1))
        call B_dd(B_2,Buv_2,p01,m02,m12,rupmax+2,nid(2))

c Cuv_{00...} from PaVe reduction -- Eq.(5.10)
        do r=0,rupmax+1
        do i0=0,r,2
        do i1=0,r-i0
        i2 = r-i0-i1
          if (i0.eq.0) then
            Cuv(i0/2,i1,i2) = 0._rk
          else
            Cuv(i0/2,i1,i2) = 1._rk/2._rk/dble(r)*(
     &              Buv0(i0/2-1,i1,i2) + 2._rk*m02*Cuv(i0/2-1,i1,i2)
     &            + f(1)*Cuv(i0/2-1,i1+1,i2) + f(2)*Cuv(i0/2-1,i1,i2+1) )
          endif
        enddo
        enddo
        enddo

500     continue
        rup = rup+1

c store previous approximation for error estimate
        if ((rup.ge.r2).or.(rup.ge.rupmax-2)) then
          do n=0,rup-1
            Cstore(1,n) = C(0,n,0) 
            Cstore(2,n) = C(0,0,n) 
          enddo
        endif

        do i0=0,rup+1,2
        do i1=0,rup+1-i0
        i2 = rup+1-i0-i1
          C(i0/2,i1,i2) = 0._rk
        enddo
        enddo

c iteration: rup,  step: r
        do 400 r=rup,0,-1

c C_{00...} from iteration Eq.(5.63)
        do 102 i0=2*((r+1)/2),2,-2
        do 102 i1=0,r+1-i0
        i2 = r+1-i0-i1
        i(1) = i1
        i(2) = i2
          C(i0/2,i1,i2) = 1._rk/2._rk/(2._rk+i0+2*i1+2*i2)*(
     &      4._rk*Cuv(i0/2,i1,i2)
     &      + 2._rk*B0(i0/2-1,i1,i2)+2._rk*m02*C(i0/2-1,i1,i2) )
          do 102 m=1,2
          do 102 n=1,2
          C(i0/2,i1,i2) = C(i0/2,i1,i2) - 1._rk/2._rk/(2._rk+i0+2*i1+2*i2)
     &      * z2(m,n)*C(i0/2-1,i1+del(m,1)+del(n,1),i2+del(m,2)+del(n,2))
102     continue

c coefficients C(0,i1,i2) from Eq.(5.62)
        i0 = 0
        do 103 i1=0,r
        i2 = r-i0-i1
        i(1) = i1
        i(2) = i2
          Sh3 = -B0(i0/2,i1,i2) 
          if ((k.eq.1).and.(i1.eq.0)) Sh3 = Sh3 + B_1(i0/2,i2)
          if ((k.eq.2).and.(i2.eq.0)) Sh3 = Sh3 + B_2(i0/2,i1)
          C(i0/2,i1,i2) = ( Sh3
     &      - z2(k,1)*C(i0/2,i1+del(1,1),i2+del(2,1)) 
     &      - z2(k,2)*C(i0/2,i1+del(1,2),i2+del(2,2)) )/f(k)
          if (i(k).ne.0) C(i0/2,i1,i2) = C(i0/2,i1,i2)
     &      - 2._rk*i(k)*C(i0/2+1,i1-del(1,k),i2-del(2,k))/f(k)
        Cmax(r) = max(Cmax(r),abs(C(i0/2,i1,i2)))
103     continue

400     continue

c estimate precision from last improvements
        if ((rup.le.r2).and.(rup.lt.rupmax-2)) then
          acc = 1.e30_rk
        else
c         acc1 = abs(C(0,0,0)/scalint(id)-1._rk)
          acc1 = 0._rk
          do n=0,rup-1
            acc1 = max(acc1,abs(Cstore(1,n)-C(0,n,0))/Cmax(n))
            acc1 = max(acc1,abs(Cstore(2,n)-C(0,0,n))/Cmax(n))
            accr2_newprelim(tid(id),n) = acc1
          enddo
          acc = accr2_newprelim(tid(id),r2) 
        endif

c error propagation for small momentum expansion - variant 1
          Bij_err = dprec_dd * max(1._rk,abs(B0(0,0,0)),abs(B_1(0,0)),abs(B_2(0,0)))
          do 800 r=rup,0,-1
          if (r.ge.1) then
            C00_err_newprelim(tid(id),r+1) = max( Bij_err,
     &                     abs(m02)*Cij_err_newprelim(tid(id),r-1),
     &                     maxz*Cij_err_newprelim(tid(id),r+1) )
          endif
          Cij_err_newprelim(tid(id),r) = 
     &          max( Bij_err,C00_err_newprelim(tid(id),r+1)
     &               ,maxz*Cij_err_newprelim(tid(id),r+1) )/abs(f(k)) 
          accr2_newprelim(tid(id),r) = max(accr2_newprelim(tid(id),r),
     &                     Cij_err_newprelim(tid(id),r)/Cmax(r))

800       continue
          do n=0,r2
            acc = max(acc,accr2_newprelim(tid(id),n))
          enddo

        r2eff = rup

c stop if accuracy becomes worse
        if ((rup.ge.r2+3).and.(acc.gt.accprev1).and.
     &      (accprev1.gt.accprev2)) goto 600
        accprev2 = accprev1
        accprev1 = acc

c repeat iteration if necessary
        if ((acc.gt.cacc).and.(rup.le.rupmax-1)) goto 500

600     continue

c PaVe reduction of C_{00...} of rank r=rup+1, ... ,2*rup -- Eq.(5.10)
          do 601 r=rup+1,2*rup
          do 601 i0=2*(r-rup),r,2
          i12 = r-i0
          do 601 i1=0,i12
            i2 = i12-i1
            Cuv(i0/2,i1,i2) = 1._rk/2._rk/(i0+i12)*(
     &              Buv0(i0/2-1,i1,i2) + 2._rk*m02*Cuv(i0/2-1,i1,i2)
     &            + f(1)*Cuv(i0/2-1,i1+1,i2) + f(2)*Cuv(i0/2-1,i1,i2+1) )
            C(i0/2,i1,i2)   = 1._rk/2._rk/(i0+i12)*( 4._rk*Cuv(i0/2,i1,i2)
     &            + B0(i0/2-1,i1,i2) + 2._rk*m02*C(i0/2-1,i1,i2)
     &            + f(1)*C(i0/2-1,i1+1,i2) + f(2)*C(i0/2-1,i1,i2+1) )
601       continue

c final absolute error
        i0=0
        do r=0,r2eff
          Cmax(r) = 0._rk
        i12 = r-i0
        do i1=0,i12
          i2=i12-i1
          Cmax(r) = max(Cmax(r),abs(C(i0/2,i1,i2)))
        enddo
        Cij_err_newprelim(tid(id),r) = accr2_newprelim(tid(id),r)*Cmax(r)
        enddo

        end

**********************************************************************
        subroutine C0_dd(C0,Cuv0,p01,p12,p20,m02,m12,m22,r2,id)
**********************************************************************
*       3-point coefficients C(0)_{...} with unshifted momentum 
*---------------------------------------------------------------------
*       21.7.2006 Stefan Dittmaier
**********************************************************************
        implicit real(rk) (a-z)

c local variables
        integer r2,r,i0,i1,i2,i3,i123,id
        complex(rk) C(0:r2,0:r2,0:r2)
        complex(rk) Cuv(0:r2,0:r2,0:r2)
        complex(rk) C0(0:r2,0:r2,0:r2,0:r2)
        complex(rk) Cuv0(0:r2,0:r2,0:r2,0:r2)
        complex(rk) m02,m12,m22

        call C_dd(C,Cuv,p01,p12,p20,m02,m12,m22,r2,id)

        do 101 r=0,r2
          do 101 i0=0,2*r,2
          i123 = r-i0/2
          i1=0
          do 102 i2=0,i123-i1
            i3 = i123-i1-i2
            C0(i0/2,0,i2,i3)   = C(i0/2,i2,i3)
            Cuv0(i0/2,0,i2,i3) = Cuv(i0/2,i2,i3)
102       continue
          do 101 i1=1,i123
          do 101 i2=0,i123-i1
            i3 = i123-i1-i2
            C0(i0/2,i1,i2,i3)   = -C0(i0/2,i1-1,i2,i3)
     &          - C0(i0/2,i1-1,i2+1,i3)   - C0(i0/2,i1-1,i2,i3+1)
            Cuv0(i0/2,i1,i2,i3) = -Cuv0(i0/2,i1-1,i2,i3)
     &          - Cuv0(i0/2,i1-1,i2+1,i3) - Cuv0(i0/2,i1-1,i2,i3+1)
101     continue

        end

**********************************************************************
        function C0dd(p01,p12,p20,m02,m12,m22,ext)
**********************************************************************
*       scalar 3-point function
*---------------------------------------------------------------------
*       20.2.2008 Stefan Dittmaier
**********************************************************************
        implicit real(rk) (a-z)

        complex(rk) C0dd
        complex(rk) m02,m12,m22,m2(0:2),m2e(0:2)
        complex(rk) x(0:2),xe(0:2),dx(0:2),dxe(0:2),ieps
        real(rk) p2(0:2)
        integer ext,i,ip,im
        logical smallp2(0:2),smallm2(0:2),coll(0:2),soft(0:2)

        p2(0) = p01
        p2(1) = p12
        p2(2) = p20
        m2(0) = m02
        m2(1) = m12
        m2(2) = m22

        do i=0,2
          smallp2(i) = (abs(p2(i)).lt.1.e-15_rk)
          smallm2(i) = (abs(m2(i)).lt.1.e-15_rk)
        enddo

c soft and collinear singularities
        do i=0,2
          ip = mod(i+1,3)
          soft(i) = (abs(m2(i)).lt.1.e-15_rk).and.
     &              (acmplx(p2(i)).eq.m2(ip)).and.
     &              (acmplx(p2(mod(i+2,3))).eq.m2(mod(i+2,3)))
          coll(i) = smallp2(i).and.smallm2(i).and.smallm2(ip)
          if (coll(i).and.(.not.
     &    (((p2(i).eq.0._rk).and.(m2(i).eq.m2(ip))).or.
     &    ((acmplx(p2(i)).eq.m2(i)).and.(m2(ip).eq.(0._rk,0._rk))).or.
     &    ((acmplx(p2(i)).eq.m2(ip)).and.(m2(i).eq.(0._rk,0._rk))) ))) then
            if (cout_on.and.(cout.le.coutmax)) then
              write(outchannel,*) 
     &         'C0dd: structure of collinear singularity not supported:'
              call DD_debugoutput()
            endif
            stopflag = min(-10,stopflag)
          endif
        enddo

c soft or collinear singular cases
c---------------------------------

        if (smallp2(0).and.smallp2(1).and.smallp2(2)) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 
     &         'C0dd with only small momenta not supported!'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
c soft singularity
        elseif (soft(0)) then
          C0dd = C0ir_dd(p01,p12,p20,m02,ext)
          return
        elseif (soft(1)) then
          C0dd = C0ir_dd(p12,p20,p01,m12,ext)
          return
        elseif (soft(2)) then
          C0dd = C0ir_dd(p20,p01,p12,m22,ext)
          return
c double collinear singularity
        elseif (coll(0).and.coll(2)) then
          C0dd = C0coll02_dd(p01,p12,p20,m02,m12,m22,ext)
          return
        elseif (coll(1).and.coll(0)) then
          C0dd = C0coll02_dd(p12,p20,p01,m12,m22,m02,ext)
          return
        elseif (coll(2).and.coll(1)) then
          C0dd = C0coll02_dd(p20,p01,p12,m22,m02,m12,ext)
          return
c single collinear singularity
        elseif (coll(0)) then
          C0dd = C0coll0_dd(p01,p12,p20,m02,m12,m22,ext)
          return
        elseif (coll(1)) then
          C0dd = C0coll0_dd(p12,p20,p01,m12,m22,m02,ext)
          return
        elseif (coll(2)) then
          C0dd = C0coll0_dd(p20,p01,p12,m22,m02,m12,ext)
          return
        endif

c regular case
c-------------
        do i=0,2
          if (smallp2(i)) p2(i) = 0._rk
          if (smallm2(i)) m2(i) = 0._rk
        enddo

c regular case with zero masses
        if (smallm2(2)) then
          C0dd = C0m2zero_dd(p2(0),p2(1),p2(2),m2(0),m2(1),ext)
          return
        elseif (smallm2(0)) then
          C0dd = C0m2zero_dd(p2(1),p2(2),p2(0),m2(1),m2(2),ext)
          return
        elseif (smallm2(1)) then
          C0dd = C0m2zero_dd(p2(2),p2(0),p2(1),m2(2),m2(0),ext)
          return
        endif

c regular case with all masses non-zero
        if ((p2(0)+p2(1)-p2(2))**2-4._rk*p2(0)*p2(1).le.0._rk ) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'C0: unphysical momentum flow (alpha<0)'
            call DD_debugoutput()
          endif
          stopflag = min(-7,stopflag)
        endif

        rlam   = sqrt( (p2(0)+p2(1)-p2(2))**2-4._rk*p2(0)*p2(1) )
        scale2 = abs(p01)+abs(p12)+abs(p20)+abs(m02)+abs(m12)+abs(m22)

        if (rlam/scale2.lt.1.e-14_rk) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 
     &         'C0dd for too small Gram det not supported!'
            call DD_debugoutput()
          endif
          stopflag = min(-7,stopflag)
        endif

        eps  = 1.e-10_rk
        ieps = acmplx(0._rk,eps)

        do i=0,2
          if (smallm2(i)) then
            m2e(i) = ((i+.7_rk)*eps-(2*i+1)*ieps)*scale2
          elseif (aimag(m2(i)).eq.0._rk) then
            m2e(i) = m2(i)*(1+((i+.7_rk)*eps-(2*i+1)*ieps))
          else
            m2e(i) = m2(i)+abs(m2(i))*((i+.7_rk)*eps-(2*i+1)*ieps)
          endif
        enddo

        C0dd = 0._rk

        do i=0,2
          ip = mod(i+1,3)
          im = mod(i+2,3)

          if (smallp2(i)) then
            if (p2(im)-p2(ip).lt.0._rk) then
              x(i)  = ( (m2(i)*p2(ip)-m2(ip)*p2(im))/(p2(ip)-p2(im))
     &                +p2(ip)-m2(im) )/(p2(ip)-p2(im))
              xe(i) = ( (m2e(i)*p2(ip)-m2e(ip)*p2(im))/(p2(ip)-p2(im))
     &                +p2(ip)-m2e(im) )/(p2(ip)-p2(im))
              dx(i) = 1._rk-x(i)
              dxe(i)= 1._rk-xe(i)
              C0dd = C0dd + S3( x(i),0._rk, m2(i)- m2(ip), m2(ip),
     &                         xe(i),0._rk,m2e(i)-m2e(ip),m2e(ip))/rlam
            elseif (p2(im)-p2(ip).gt.0._rk) then
c             contribution is zero in this case
            endif

          else

            if (p2(im)-p2(ip)-p2(i).gt.0._rk) then
              alpha = (p2(im)-p2(ip)-p2(i)+rlam)/2._rk/p2(i)
            else
              alpha = 2._rk*p2(ip)/(p2(im)-p2(ip)-p2(i)-rlam)
            endif
c           alphap  = (p2(ip)+p2(i)*alpha)/rlam
c           alphap1 = alphap-1._rk
            if (p2(im)+p2(ip)-p2(i).gt.0._rk) then
              alphap = (p2(im)+p2(ip)-p2(i)+rlam)/2._rk/rlam
              alphap1= 2._rk*p2(im)*p2(ip)/rlam/(p2(im)+p2(ip)-p2(i)+rlam)
            else
              alphap = 2._rk*p2(im)*p2(ip)/rlam/(p2(im)+p2(ip)-p2(i)-rlam)
              alphap1= (p2(im)+p2(ip)-p2(i)-rlam)/2._rk/rlam
            endif
c           alpha1 = 1._rk+alpha
            if (p2(im)-p2(ip)+p2(i).gt.0._rk) then
              alpha1 = (p2(im)-p2(ip)+p2(i)+rlam)/2._rk/p2(i)
            else
              alpha1 = 2._rk*p2(im)/(p2(im)-p2(ip)+p2(i)-rlam)
            endif

            x(i)  = alphap -( m2(im)+alpha* m2(i)-alpha1* m2(ip))/rlam
            xe(i) = alphap -(m2e(im)+alpha*m2e(i)-alpha1*m2e(ip))/rlam
            dx(i) =-alphap1+( m2(im)+alpha* m2(i)-alpha1* m2(ip))/rlam
            dxe(i)=-alphap1+(m2e(im)+alpha*m2e(i)-alpha1*m2e(ip))/rlam

            if (abs(x(i)).lt.abs(dx(i))) then   
            C0dd = C0dd 
     &        +S3( x(i),p2(i),-p2(i)+m2(i)-m2(ip),m2(ip),
     &            xe(i),p2(i),-p2(i)+m2e(i)-m2e(ip),m2e(ip))/rlam
            else        
            C0dd = C0dd 
     &       -S3( dx(i),p2(i),-p2(i)+m2(ip)-m2(i),m2(i),
     &           dxe(i),p2(i),-p2(i)+m2e(ip)-m2e(i),m2e(i))/rlam
            endif

          endif

        enddo

        end

**********************************************************************
        function S3(y0,a,b,c,y0e,ae,be,ce)
**********************************************************************
*       Auxiliary integral:
*       S3 = int_0^1 dy / (y-y0) * ( ln(ay^2+by+c) - ln(ay0^2+by0+c) )
*           a = real,  y0,b,c = complex,  
*           but Im(ay^2+by+c) < 0 for y in (0,1)
*
*       See also 't Hooft, Veltman NPB 153 (1979) 365, (B.1)
*---------------------------------------------------------------------
*       20.2.2008 Stefan Dittmaier
**********************************************************************
        implicit real(rk) (a-z)

        complex(rk) S3,b,c,be,ce,eta3,a1,a2,a3
        complex(rk) y0,y0e,y(2),ye(2),caux,ieps,etay
        complex(rk) x1(2),x2(2),x3(2),xe1(2),xe2(2),xe3(2),x0,x0e
        integer k

        eta3(a1,a2,a3) = eta_dd(a1*a2,a3) + eta_dd(a1,a2)

        S3 = 0._rk

        eps  = 1.e-20_rk
        ieps = acmplx(0._rk,eps)
        crit = 1.e-12_rk

        if ((a.eq.0._rk).and.(b.eq.(0._rk,0._rk)).and.(c.eq.(0._rk,0._rk))) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'S3: a=b=c=0 not allowed.'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        elseif ((a.eq.0._rk).and.(b.eq.(0._rk,0._rk))) then
          y(1)  = 1.e30_rk
          y(2)  = 1.e30_rk
          if ((ae.eq.0._rk).and.(be.eq.0._rk)) then
            S3 = 0._rk
            return
          elseif (ae.eq.0._rk) then
            ye(1) = sqe_dd(acmplx(ae),be,ce)
            ye(2) = 1.e30_rk
          else
            ye(1) = sqe_dd(acmplx(ae),be,ce)
            ye(2) = ce/ae/ye(1)
          endif
        elseif (a.eq.0._rk) then
          y(1)  = -c/b
          y(2)  = 1.e30_rk
          ye(1) = -ce/be
          ye(2) = y(2)
        elseif (c.eq.0._rk) then
          y(1)  = 0._rk
          y(2)  = -b/a
          ye(1) = sqe_dd(acmplx(ae),be,ce)
          ye(2) = ce/ae/ye(1)
        else
          y(1)  = sqe_dd(acmplx(a),b,c)
          y(2)  = c/a/y(1)
          ye(1) = sqe_dd(acmplx(ae),be,ce)
          ye(2) = ce/ae/ye(1)
        endif
        if (abs(y(1)-ye(1)).gt.abs(y(1)-ye(2))) then
          caux  = ye(1)
          ye(1) = ye(2)
          ye(2) = caux
        endif
        if ((abs(y0-y(1)).lt.crit*(abs(y0)+abs(y(1)))).or.
     &      (abs(y0-y(2)).lt.crit*(abs(y0)+abs(y(2))))) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'S3: y0=y(k) not allowed.'
            call DD_debugoutput()
          endif
          stopflag = min(-7,stopflag)
        endif

        if (abs(y(1)).lt.crit) y(1) = 0._rk
        if (abs(y(2)).lt.crit) y(2) = 0._rk
        if (abs(y(1)-1._rk).lt.crit) y(1) = 1._rk
        if (abs(y(2)-1._rk).lt.crit) y(2) = 1._rk

        do k=1,2

        if (real(y(k)).eq.1.e30_rk) then
c             contribution is zero in this case
        elseif ((abs(y(k)-y0).gt.abs(y(k)-1._rk)).or.
     &          (y(k).eq.(0._rk,0._rk))) then
          x1(k)  = y0/(y0-y(k))
          xe1(k) = y0e/(y0e-ye(k))
          x2(k)  = (y0 -1._rk)/(y0 -y(k))
          xe2(k) = (y0e-1._rk)/(y0e-ye(k))
          if (abs(aimag(x1(k))).lt.crit*abs(x1(k)))
     &      x1(k) = acmplx(real(x1(k)),
     &                     (abs(aimag(x1(k)))+abs(x1(k))*eps)
     &                     *sign(1._rk,aimag(xe1(k))))
          if (abs(aimag(x2(k))).lt.crit*abs(x2(k)))
     &      x2(k) = acmplx(real(x2(k)),
     &                     (abs(aimag(x2(k)))+abs(x2(k))*eps)
     &                     *sign(1._rk,aimag(xe2(k))))
          S3 = S3 + cspen_dd(x1(k)) - cspen_dd(x2(k))
          etay = eta_dd((ye(k)-1._rk)/ye(k),ye(k)/(ye(k)-y0e))
          if (etay.ne.(0._rk,0._rk)) S3 = S3 - etay*log(x2(k))
        else
          x1(k)  = y0 /y(k)
          xe1(k) = y0e/ye(k)
          x2(k)  = (y0 -1._rk)/(y(k) -1._rk)
          xe2(k) = (y0e-1._rk)/(ye(k)-1._rk)
          x3(k)  = (y(k) -1._rk)/y(k)
          xe3(k) = (ye(k)-1._rk)/ye(k)
          if (abs(aimag(x1(k))).lt.crit*abs(x1(k)))
     &      x1(k) = acmplx(real(x1(k)),
     &                     (abs(aimag(x1(k)))+abs(x1(k))*eps)
     &                     *sign(1._rk,aimag(xe1(k))))
          if (abs(aimag(x2(k))).lt.crit*abs(x2(k)))
     &      x2(k) = acmplx(real(x2(k)),
     &                     (abs(aimag(x2(k)))+abs(x2(k))*eps)
     &                     *sign(1._rk,aimag(xe2(k))))
          if (abs(aimag(x3(k))).lt.crit*abs(x3(k)))
     &      x3(k) = acmplx(real(x3(k)),
     &                     (abs(aimag(x3(k)))+abs(x3(k))*eps)
     &                     *sign(1._rk,aimag(xe3(k))))
          S3 = S3 - cspen_dd(x1(k)) + cspen_dd(x2(k)) 
     &         + log(x3(k))**2/2._rk - log(x3(k))*log(1._rk-x1(k))
          etay = eta_dd(xe3(k),ye(k)/(ye(k)-y0e))
          if (etay.ne.(0._rk,0._rk))
     &      S3 = S3 + etay*(-log(x2(k))
     &           -eta_dd((1._rk-ye(k))/(ye(k)-y0e),(ye(k)-y0e)/(1._rk-y0e)))
        endif

        enddo

        if ((y0.ne.(0._rk,0._rk)).and.(y0.ne.(1._rk,0._rk))) then
          etay  = eta3(ce,1._rk-y0e/ye(1),1._rk-y0e/ye(2))
          if (etay.eq.(0._rk,0._rk)) return
          x0  = (y0 -1._rk)/y0
          x0e = (y0e-1._rk)/y0e
          if (abs(aimag(x0))/abs(x0).lt.crit)
     &      x0 = acmplx(real(x0),(abs(aimag(x0))+abs(x0)*eps)
     &                            *sign(1._rk,aimag(x0e)))
          if ((aimag(x0).ne.0._rk).or.(real(x0).gt.0._rk)) then
            S3 = S3 - log(x0)*etay
          else
            if (cout_on.and.(cout.le.coutmax)) then
              write(outchannel,*) 'S3: log(x0)*etay is problematic.'
              call DD_debugoutput()
            endif
            stopflag = min(-7,stopflag)
          endif
        endif

        end

**********************************************************************
        function C0ir_dd(p01,p12,p20,m02,ext)
**********************************************************************
*       soft divergent scalar 3-point function
*       m02 = small,  m12 = p01,  m22 = p20
*       m02 can only be non-zero if p01, p20 not small
*       Result of Beenakker/Denner NPB338 (1990) 349
*---------------------------------------------------------------------
*       20.2.2008 Stefan Dittmaier
**********************************************************************
        implicit real(rk) (a-z)

c local variables
        complex(rk) C0ir_dd,ieps,s,xs,root,m02,cmp2,cm2,null
        complex(rk) div
        integer ext,ext0
        logical smallm12,smallm22

        rmp2(rm2) = mx2(nint(rm2*1.e20_rk))
        cmp2(cm2) = mx2(nint(real(cm2*1.e20_rk)))

        C0ir_dd = 0._rk

        ext0 = ext
        null = acmplx(0._rk,0._rk)
        eps  = 1.e-20_rk
        ieps = acmplx(0._rk,eps)
        pi   = 4._rk*atan(1._rk)

        if (((m02.ne.(0._rk,0._rk)).and.((p01.lt.1.e-18_rk).or.(p20.lt.1.e-18_rk)))
     &      .or.(abs(m02).gt.1.e-18_rk)) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'C0ir_dd: inconsistent choice of m02:'
            call DD_debugoutput()
          endif
          stopflag = min(-9,stopflag)
        endif

c convert input parameters
        smallm12 = (abs(p01).lt.1.e-15_rk)
        smallm22 = (abs(p20).lt.1.e-15_rk)
        if (smallm12) then
          if (p01.ne.0._rk) then
            m12 = rmp2(p01)
          else
            m12 = 0._rk
          endif
        else
          m12 = p01
        endif
        if (smallm22) then
          if (p20.ne.0._rk) then
            m22 = rmp2(p20)
          else
            m22 = 0._rk
          endif
        else
          m22 = p20
        endif
        m1 = sqrt(m12)
        m2 = sqrt(m22)
        if (abs(p12).gt.1.e-15_rk) then
          s = p12+abs(p12)*ieps
        else
          s = null
        endif

        if ((m12.eq.0._rk).and.(m22.eq.0._rk)) then
          if (s.eq.null) goto 100
          C0ir_dd = ( delta2ir+delta1ir*log(-mir2/s)
     &               +log(-mir2/s)**2/2._rk-pi**2/6._rk )/s
        elseif ((m12.eq.0._rk).and.smallm22) then
          if (s.eq.null) goto 100
          C0ir_dd = ( delta2ir/2._rk+delta1ir*log(-mir2/s)
     &               -delta1ir*log(mir2/m22)/2._rk
     &               +log(-mir2/s)**2/2._rk-log(mir2/m22)**2/4._rk
     &               -pi**2/6._rk )/s
        elseif ((m22.eq.0._rk).and.smallm12) then
          if (s.eq.null) goto 100
          C0ir_dd = ( delta2ir/2._rk+delta1ir*log(-mir2/s)
     &               -delta1ir*log(mir2/m12)/2._rk
     &               +log(-mir2/s)**2/2._rk-log(mir2/m12)**2/4._rk
     &               -pi**2/6._rk )/s
        elseif (m12.eq.0._rk) then
          C0ir_dd = ( delta2ir/2._rk+delta1ir*log(mir2/(m22-s))
     &               -delta1ir*log(mir2/m22)/2._rk
     &               +log(mir2/(m22-s))**2/2._rk-log(mir2/m22)**2/4._rk
     &               -cspen_dd(s/(s-m22)) )/(s-m22)
        elseif (m22.eq.0._rk) then
          C0ir_dd = ( delta2ir/2._rk+delta1ir*log(mir2/(m12-s))
     &               -delta1ir*log(mir2/m12)/2._rk
     &               +log(mir2/(m12-s))**2/2._rk-log(mir2/m12)**2/4._rk
     &               -cspen_dd(s/(s-m12)) )/(s-m12)
        elseif (smallm12.and.smallm22) then
          if (s.eq.null) goto 100
          C0ir_dd = ( (delta1ir+log(-mir2/s))*log(-m1*m2/s)
     &      -log(-m12/s)**2/4._rk-log(-m22/s)**2/4._rk-pi**2/6._rk ) / s
        elseif (smallm12) then
          C0ir_dd = ( (-delta1ir+log(m1*(m22-s)/mir2/m2))
     &                *log((m22-s)/m1/m2)+cspen_dd(s/m22) ) / (s-m22)
        elseif (smallm22) then
          C0ir_dd = ( (-delta1ir+log(m2*(m12-s)/mir2/m1))
     &                *log((m12-s)/m2/m1)+cspen_dd(s/m12) ) / (s-m12)
        else
          if (s.eq.null) then
            if (m12.eq.m22) then
              C0ir_dd = (delta1ir+log(mir2/m12))/2._rk/m12
            else
              C0ir_dd = (delta1ir+log(mir2/m1/m2))*log(m1/m2)/(m12-m22)
            endif
            return
          endif
          root = sqrt(1._rk-4._rk*m1*m2/(s-(m1-m2)**2))
          xs   = (root-1._rk)/(root+1._rk)
          if (m02.eq.(0._rk,0._rk)) then
            div  = delta1ir+log(mir2)
          else
            div = log(cmp2(m02))
          endif
          C0ir_dd = xs/m1/m2/(1._rk-xs**2)*(
     &      (-div+log(m1*m2))*log(xs)-log(xs)**2/2._rk
     &      +2._rk*log(xs)*log(1._rk-xs**2)+log(m1/m2)**2/2._rk-pi**2/6._rk
     &      +cspen_dd(xs**2)+cspen_dd(1._rk-xs*m1/m2)
     &      +cspen_dd(1._rk-xs*m2/m1) )
        endif

        return

100     continue
        if (cout_on.and.(cout.le.coutmax)) then
          write(outchannel,*) 
     &       'C0ir_dd: not implemented if all scales are small'
          call DD_debugoutput()
        endif
        stopflag = min(-10,stopflag)

        end

**********************************************************************
        function C0coll02_dd(p01,p12,p20,cm02,cm12,cm22,ext)
**********************************************************************
*       doubly collinear singular scalar 3-point function
*       p01, p20 and all masses small
*       Results of S.D. NPB675 (2003) 447
*---------------------------------------------------------------------
*       20.2.2008 Stefan Dittmaier
**********************************************************************
        implicit real(rk) (a-z)

c local variables
        complex(rk) C0coll02_dd,ieps,cm02,cm12,cm22,s
        integer ext,ext0

        rmp2(rm2) = mx2(nint(rm2*1.e20_rk))

        C0coll02_dd = 0._rk

        ext0 = ext
        eps  = 1.e-20_rk
        ieps = acmplx(0._rk,eps)
        pi   = 4._rk*atan(1._rk)

c convert input parameters
        if (abs(p12).lt.1.e-15_rk) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 
     &         'C0coll02_dd: not implemented for small p12'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        endif
        q01 = 0._rk
        q20 = 0._rk
        m02 = 0._rk
        m12 = 0._rk
        m22 = 0._rk
        if (p01.ne.0._rk) q01 = rmp2(p01)
        if (p20.ne.0._rk) q20 = rmp2(p20)
        if (cm02.ne.(0._rk,0._rk)) m02 = rmp2(real(cm02))
        if (cm12.ne.(0._rk,0._rk)) m12 = rmp2(real(cm12))
        if (cm22.ne.(0._rk,0._rk)) m22 = rmp2(real(cm22))
        s = p12+abs(p12)*ieps

c C0(m^2,s,m^2,m,0,0)     (B.13)
        if ((m02.ne.0._rk).and.(m12.eq.0._rk).and.(m22.eq.0._rk).and.
     &      (q01.eq.m02).and.(q20.eq.m02)) then
          C0coll02_dd = ( log(-m02/s)**2/2._rk+2._rk*pi**2/3._rk )/s
          return
c C0(m^2,s,0,m,0,m)       (B.14)
        elseif ((m02.ne.0._rk).and.(m12.eq.0._rk).and.(m22.eq.m02).and.
     &          (q01.eq.m02).and.(q20.eq.0._rk)) then
          C0coll02_dd = ( log(-m02/s)**2/2._rk+pi**2/3._rk )/s
          return
c C0(0,s,m^2,m,m,0)       (B.14)
        elseif ((m02.ne.0._rk).and.(m12.eq.m02).and.(m22.eq.0._rk).and.
     &          (q01.eq.0._rk).and.(q20.eq.m02)) then
          C0coll02_dd = ( log(-m02/s)**2/2._rk+pi**2/3._rk )/s
          return
c C0(0,s,0,m,m,m)         (B.15)
        elseif ((m02.ne.0._rk).and.(m12.eq.m02).and.(m22.eq.m02).and.
     &          (q01.eq.0._rk).and.(q20.eq.0._rk)) then
          C0coll02_dd = log(-m02/s)**2/2._rk/s
          return
        endif

        if (cout_on.and.(cout.le.coutmax)) then
          write(outchannel,*) 'C0coll02_dd: case not yet implemented:'
          call DD_debugoutput()
        endif
        stopflag = min(-10,stopflag)

        end

**********************************************************************
        function C0coll0_dd(p01,p12,p20,cm02,cm12,cm22,ext)
**********************************************************************
*       singly collinear singular scalar 3-point function
*       p01, m02, m12 small
*       Results of S.D. NPB675 (2003) 447
*---------------------------------------------------------------------
*       20.2.2008 Stefan Dittmaier
**********************************************************************
        implicit real(rk) (a-z)

c local variables
        complex(rk) C0coll0_dd,ieps,cm02,cm12,cm22,s1,s2,m22
        integer ext,ext0

        rmp2(rm2) = mx2(nint(rm2*1.e20_rk))

        C0coll0_dd = 0._rk

        ext0 = ext
        eps  = 1.e-20_rk
        ieps = acmplx(0._rk,eps)
        pi   = 4._rk*atan(1._rk)

c convert input parameters
        q01 = 0._rk
        m02 = 0._rk
        m12 = 0._rk
        if (p01.ne.0._rk) q01 = rmp2(p01)
        if (cm02.ne.(0._rk,0._rk)) m02 = rmp2(real(cm02))
        if (cm12.ne.(0._rk,0._rk)) m12 = rmp2(real(cm12))
        if (abs(p12).lt.1.e-15_rk) then
          s1 = 0._rk
        else
          s1 = p12+abs(p12)*ieps
        endif
        if (abs(p20).lt.1.e-15_rk) then
          s2 = 0._rk
        else
          s2 = p20+abs(p20)*ieps*1.3_rk
        endif
        if (abs(cm22).lt.1.e-15_rk) then
          m22 = 0._rk
        else
          m22 = cm22
        endif

c C0(m^2,s,M^2,m,0,M)   (B.10)
        if ((q01.ne.0._rk).and.(q01.eq.m02).and.(m12.eq.0._rk).and.
     &      (acmplx(p20).eq.cm22)) then
          C0coll0_dd = ( log((m22-s1)/sqrt(m02*m22))**2
     &                  +5._rk*pi**2/12._rk+cspen_dd(s1/m22) )/(s1-m22)
          return
c C0(m^2,M^2,s,0,m,M)   (B.10)
        elseif ((q01.ne.0._rk).and.(q01.eq.m12).and.(m02.eq.0._rk).and.
     &          (acmplx(p12).eq.cm22)) then
          C0coll0_dd = ( log((m22-s2)/sqrt(m12*m22))**2
     &                  +5._rk*pi**2/12._rk+cspen_dd(s2/m22) )/(s2-m22)
          return
c C0(0,s,M^2,m,m,M)     (B.11)
        elseif ((q01.eq.0._rk).and.(m02.eq.m12).and.(m02.ne.0._rk).and.
     &          (acmplx(p20).eq.cm22)) then
          C0coll0_dd = ( log((m22-s1)/sqrt(m02*m22))**2
     &                  +pi**2/12._rk+cspen_dd(s1/m22) )/(s1-m22)
          return
c C0(0,M^2,s,m,m,M)     (B.11)
        elseif ((q01.eq.0._rk).and.(m02.eq.m12).and.(m02.ne.0._rk).and.
     &          (acmplx(p12).eq.cm22)) then
          C0coll0_dd = ( log((m22-s2)/sqrt(m12*m22))**2
     &                  +pi**2/12._rk+cspen_dd(s2/m22) )/(s2-m22)
          return
c C0(m^2,s1,s2,m,0,M)   (B.2)
        elseif ((q01.ne.0._rk).and.(q01.eq.m02).and.(m12.eq.0._rk)) then
          if (m22.eq.(0._rk,0._rk)) then
            C0coll0_dd = ( log(s2/s1)/2._rk*(log(-s2/q01)+log(-s1/q01))
     &                    -2._rk*cspen_dd((s1-s2)/s1) )/(s2-s1)
          else
            C0coll0_dd = ( log((m22-s2)/q01)*log((m22-s2)/m22)
     &                    -log((m22-s1)/q01)*log((m22-s1)/m22)
     &                    -2._rk*cspen_dd((s2-s1)/(m22-s1))
     &                    +cspen_dd(s2/m22)-cspen_dd(s1/m22) )/(s2-s1)
          endif
          return
c C0(m^2,s1,s2,0,m,M)   (B.2)
        elseif ((q01.ne.0._rk).and.(q01.eq.m12).and.(m02.eq.0._rk)) then
          if (m22.eq.(0._rk,0._rk)) then
            C0coll0_dd = ( log(s1/s2)/2._rk*(log(-s1/q01)+log(-s2/q01))
     &                    -2._rk*cspen_dd((s2-s1)/s2) )/(s1-s2)
          else
            C0coll0_dd = ( log((m22-s1)/q01)*log((m22-s1)/m22)
     &                    -log((m22-s2)/q01)*log((m22-s2)/m22)
     &                    -2._rk*cspen_dd((s1-s2)/(m22-s2))
     &                    +cspen_dd(s1/m22)-cspen_dd(s2/m22) )/(s1-s2)
          endif
          return
c C0(0,s1,s2,m,m,M)     (B.3)
        elseif ((q01.eq.0._rk).and.(m02.eq.m12).and.(m02.ne.0._rk)) then
          if (m22.eq.(0._rk,0._rk)) then
            C0coll0_dd = log(s1/s2)/2._rk*(log(-s1/m02)+log(-s2/m02))
     &                     /(s1-s2)
          else
            C0coll0_dd = ( log((m22-s1)/sqrt(m02*m22))**2
     &                    -log((m22-s2)/sqrt(m02*m22))**2
     &                    +cspen_dd(s1/m22)-cspen_dd(s2/m22) )/(s1-s2)
          endif
          return
c C0(0,s1,s2,0,0,M)     (B.4)
        elseif ((q01.eq.0._rk).and.(m02.eq.0._rk).and.(m12.eq.0._rk)) then
          if (m22.eq.(0._rk,0._rk)) then
            C0coll0_dd = log(s2/s1)/2._rk/(s1-s2)
     &                     *(2._rk*delta1ir+log(-mir2/s1)+log(-mir2/s2))
          else
            C0coll0_dd = ( (delta1ir+log(mir2/m22))
     &                     *log((m22-s2)/(m22-s1))
     &                    +log((m22-s1)/m22)**2-log((m22-s2)/m22)**2
     &                    +cspen_dd(s1/m22)-cspen_dd(s2/m22) )/(s1-s2)
          endif
          return
        endif

        if (cout_on.and.(cout.le.coutmax)) then
          write(outchannel,*) 'C0coll0_dd: case not yet implemented:'
          call DD_debugoutput()
        endif
        stopflag = min(-10,stopflag)

        end

**********************************************************************
        function nC0m2zero_dd(p01,p12,p20,m02,m12,ext)
**********************************************************************
*       regular scalar 3-point function with m2 = 0
*---------------------------------------------------------------------
*       28.2.2008 Stefan Dittmaier
**********************************************************************
        implicit real(rk) (a-z)

        complex(rk) nC0m2zero_dd,C0m2zero_dd
        complex(rk) m02,m12,m02e,m12e,ieps
        complex(rk) b,c,be,ce,y0(2),dy0(2)
        complex(rk) db,dc,dbe,dce,arg,arge
        integer i,sgn,ext,ext0

        ext0  = ext
        rlam2 = (p01+p12-p20)**2-4._rk*p01*p12
        if (rlam2.le.0._rk) then
          rlam = 0._rk
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'C0m2zero_dd: lambda<=0 not supported'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        else
          rlam = sqrt(rlam2)
        endif

        scale2 = abs(p01)+abs(p12)+abs(p20)+abs(m02)+abs(m12)

        if (rlam/scale2.lt.1.e-14_rk) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 
     &         'C0m2zero_dd for too small Gram det not supported!'
            call DD_debugoutput()
          endif
          stopflag = min(-7,stopflag)
        endif

        eps  = 1.e-13_rk
        ieps = acmplx(0._rk,eps)
        eps2 = 1.e-20_rk
        crit = 1.e-10_rk

        if (m02.eq.(0._rk,0._rk)) then
          m02e = (eps-ieps)*scale2
        elseif (aimag(m02).eq.0._rk) then
          m02e = m02*(1._rk+eps-ieps)
        else
          m02e = m02+abs(m02)*(eps-ieps)*1.3_rk
        endif
        if (m12.eq.(0._rk,0._rk)) then
          m12e = 1.3_rk*(eps-1.3_rk*ieps)*scale2
        elseif (aimag(m12).eq.0._rk) then
          m12e = m12*(1._rk+1.4*eps-1.8_rk*ieps)
        else
          m12e = m12+abs(m12)*(2.1_rk*eps-1.3_rk*ieps)*2.1_rk
        endif

        if (p01.eq.0._rk) then
          if (p12-p20.gt.0._rk) then
            y0(1)  = 1.e30_rk
            dy0(1) = 1.e30_rk
            y0(2)  = p12/(p12-p20)
            dy0(2) = p20/(p20-p12)
          else
            y0(1)  = p12/(p12-p20)
            dy0(1) = p20/(p20-p12)
            y0(2)  = 1.e30_rk
            dy0(2) = 1.e30_rk
          endif
        else
          if (p12+p01-p20.gt.0._rk) then
            y0(1) = (p12+p01-p20+rlam)/2._rk/p01
            y0(2) = 2._rk*p12/(p12+p01-p20+rlam)
          else
            y0(1) = 2._rk*p12/(p12+p01-p20-rlam)
            y0(2) = (p12+p01-p20-rlam)/2._rk/p01
          endif
          if (p12-p01-p20.gt.0._rk) then
            dy0(1) = -(p12-p01-p20+rlam)/2._rk/p01
            dy0(2) = -2._rk*p20/(p12-p01-p20+rlam)
          else
            dy0(1) = -2._rk*p20/(p12-p01-p20-rlam)
            dy0(2) = -(p12-p01-p20-rlam)/2._rk/p01
          endif
        endif

        a   = p01
        b   = m02-m12-p01
        c   = m12
        be  = m02e-m12e-p01
        ce  = m12e
        db  = m12-m02-p01
        dc  = m02
        dbe = m12e-m02e-p01
        dce = m02e

        C0m2zero_dd = 0._rk
        do i=1,2
        if (real(y0(i)).ne.1.e30_rk) then
          sgn = 3-2*i
c special case a*y0^2+b*y0+c=0
          if (abs(a*y0(i)**2+b*y0(i)+c).lt.
     &        crit*(abs(a)+abs(b)+abs(c))) then
            C0m2zero_dd = C0m2zero_dd 
     &       -sgn*S3(y0(i),0._rk,acmplx(a),b+a*y0(i),
     &               y0(i),0._rk,acmplx(a),be+a*y0(i))/rlam
             arg  = (2._rk*a*y0(i)+m02-m12-p01)/(m02-m12-p20+p12)
             arge = (2._rk*a*y0(i)+m02e-m12e-p01)/(m02e-m12e-p20+p12)
             if (abs(aimag(arg))/abs(arg).lt.crit)
     &         arg = acmplx(real(arg),(abs(aimag(arg))+abs(arg)*eps2)
     &                                 *sign(1._rk,aimag(arge)))
            if ((real(y0(i)).lt.0._rk).or.(real(y0(i)).gt.1._rk)) then
              C0m2zero_dd = C0m2zero_dd 
     &                      -sgn/rlam*log(arg)*log(1._rk-1._rk/y0(i))
            else
              if (cout_on.and.(cout.le.coutmax)) then
                write(outchannel,*) 
     &             'C0m2zero_dd: 0<y0<1 for a*y0^2+b*y0+c=0 !'
                call DD_debugoutput()
              endif
              stopflag = min(-7,stopflag)
            endif
            y0(i) = 1.e30_rk
c regular case a*y0^2+b*y0+c=/=0
          elseif (abs(y0(i)).lt.abs(dy0(i))) then
            C0m2zero_dd = C0m2zero_dd 
     &       -sgn*S3(y0(i),a,b,c,y0(i),a,be,ce)/rlam
          else
            C0m2zero_dd = C0m2zero_dd 
     &       +sgn*S3(dy0(i),a,db,dc,dy0(i),a,dbe,dce)/rlam
          endif
        endif
        enddo

        a   = 0._rk
        b   = m02-m12-p20+p12
        c   = m12-p12
        be  = m02e-m12e-p20+p12
        ce  = m12e-p12
        db  = -b
        dc  = m02-p20
        dbe = -be
        dce = m02e-p20

        do i=1,2
        if (real(y0(i)).ne.1.e30_rk) then
          sgn = 3-2*i
          if (abs(y0(i)).lt.abs(dy0(i))) then
            C0m2zero_dd = C0m2zero_dd 
     &       +sgn*S3(y0(i),a,b,c,y0(i),a,be,ce)/rlam
          else
            C0m2zero_dd = C0m2zero_dd 
     &       -sgn*S3(dy0(i),a,db,dc,dy0(i),a,dbe,dce)/rlam
          endif
        endif
        enddo

        nC0m2zero_dd = C0m2zero_dd

        end

**********************************************************************
        function C0m2zero_dd(q01,q12,q20,xm02,xm12,ext)
**********************************************************************
*       regular scalar 3-point function with m2 = 0
*---------------------------------------------------------------------
*       28.2.2008 Stefan Dittmaier
**********************************************************************
        implicit real(rk) (a-z)

        complex(rk) xm02,xm12,m02,m12,m02e,m12e,p01e,p12e,p20e
        complex(rk) C0m2zero_dd,inteps,z,ze
        complex(rk) x(5),xe(5),xx(2),arg(5),arge(5),ieps,ieps2,caux
        complex(rk) etax
        integer i,j,sgn,ext,ext0

        inteps(z,ze) = acmplx( real(z),(abs(aimag(z))+abs(z)*eps2)
     &                                  *sign(1._rk,aimag(ze)) )

        ext0  = ext
        eps   = 1.e-13_rk
        ieps  = acmplx(0._rk,eps)
        eps2  = 1.e-20_rk
        ieps2 = acmplx(0._rk,eps2)
        crit  = 1.e-10_rk

        if (q20.ne.0._rk) then
          m02 = xm02
          m12 = xm12
          p01 = q01
          p12 = q12
          p20 = q20
        else
          m02 = xm12
          m12 = xm02
          p01 = q01
          p12 = q20
          p20 = q12
        endif

        scale2 = abs(p01)+abs(p12)+abs(p20)+abs(m02)+abs(m12)

        if (m02.eq.(0._rk,0._rk)) then
          m02e = (eps-ieps)*scale2
        elseif (aimag(m02).eq.0._rk) then
          m02e = m02*(1._rk+eps-ieps)
        else
          m02e = m02+abs(m02)*(eps-ieps)*1.3_rk
        endif
        if (m12.eq.(0._rk,0._rk)) then
          m12e = 1.3_rk*(eps-1.3_rk*ieps)*scale2
        elseif (aimag(m12).eq.0._rk) then
          m12e = m12*(1._rk+1.4*eps-1.8_rk*ieps)
        else
          m12e = m12+abs(m12)*(2.1_rk*eps-1.3_rk*ieps)
        endif

        if ((p12.eq.0._rk).and.(p20.eq.0._rk)) then
          x(3)  = sqe_dd(m02,m02+m12-p01,m12)
          x(4)  = m12/m02/x(3)
          xe(3) = sqe_dd(m02e,m02e+m12e-p01,m12e)
          xe(4) = m12e/m02e/xe(3)
          if (abs(x(3)-xe(3)).gt.abs(x(3)-xe(4))) then
            caux  = xe(3)
            xe(3) = xe(4)
            xe(4) = caux
          endif
          if (abs(aimag(x(3)))/abs(x(3)).lt.crit)
     &      x(3) = inteps(x(3),xe(3))
          if (abs(aimag(x(4)))/abs(x(4)).lt.crit)
     &      x(4) = inteps(x(4),xe(4))
          C0m2zero_dd = ( log(-x(3))**2+log(-x(4))**2
     &                   -log(m02/m12)**2 )/2._rk/p01
          goto 999
        endif

        rlam2 = (p01+p12-p20)**2-4._rk*p01*p12
        if (rlam2.le.0._rk) then
          rlam = 0._rk
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'C0m2zero_dd: lambda<=0 not supported'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        else
          rlam = sqrt(rlam2)
        endif

        if (rlam/scale2.lt.1.e-14_rk) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 
     &         'C0m2zero_dd for too small Gram det not supported!'
            call DD_debugoutput()
          endif
          stopflag = min(-7,stopflag)
        endif

        if (p01.eq.0._rk) then
          p01e = 2.1_rk*(eps+0.9_rk*ieps)*scale2
        else
          p01e = p01+ieps*abs(p01)*1.7_rk
        endif
        if (p12.eq.0._rk) then
          p12e = 2.6_rk*(eps+0.7_rk*ieps)*scale2
        else
          p12e = p12+ieps*abs(p12)*3.7_rk
        endif
        if (p20.eq.0._rk) then
          p20e = 1.1_rk*(eps+1.2_rk*ieps)*scale2
        else
          p20e = p20+ieps*abs(p20)*2.7_rk
        endif

        x(1)  = sqe_dd(acmplx(p20),acmplx(p20-p01+p12),acmplx(p12))
        x(2)  = p12/p20/x(1)
        xe(1) = sqe_dd(p20e,p20e-p01e+p12e,acmplx(p12e))
        xe(2) = p12e/p20e/xe(1)
        if (abs(x(1)-xe(1)).gt.abs(x(1)-xe(2))) then
          caux  = xe(1)
          xe(1) = xe(2)
          xe(2) = caux
        endif
        do i=1,2
          xx(i) = x(i)
          if (abs(aimag(xx(i))).lt.crit*abs(xx(i)))
     &      xx(i) = inteps(xx(i),xe(i))
        enddo
        if (real(x(1)-x(2))*p20.lt.0._rk) rlam = -rlam

        if (m02.eq.(0._rk,0._rk)) then
          if (acmplx(p01).eq.m12) then
            x(3) = 1.e30_rk
            x(4) = 1.e30_rk
          else
            x(3) = m12/(p01-m12)
            x(4) = 1.e30_rk
          endif
        elseif (m12.eq.(0._rk,0._rk)) then
            x(3) = 0._rk
            x(4) = (p01-m02)/m02
        else
          x(3) = sqe_dd(m02,m02+m12-p01,m12)
          x(4) = m12/m02/x(3)
        endif
        xe(3) = sqe_dd(m02e,m02e+m12e-p01e,m12e)
        xe(4) = m12e/m02e/xe(3)
        if (abs(x(3)-xe(3)).gt.abs(x(3)-xe(4))) then
          caux  = xe(3)
          xe(3) = xe(4)
          xe(4) = caux
        endif
        if (acmplx(p20).eq.m02) then
          x(5) = 1.e30_rk
        else
          x(5) = -(m12-p12)/(m02-p20)
        endif
        xe(5) = -(m12e-p12e)/(m02e-p20e)

        C0m2zero_dd = 0._rk
        do 100 i=1,2
          sgn = 3-2*i

          if (m12.eq.(0._rk,0._rk)) then
            if (acmplx(p01).eq.m02) then
              C0m2zero_dd = C0m2zero_dd - sgn/rlam*(
     &          -log(m02-abs(m02)*ieps2)*log(-xx(i))-log(-xx(i))**2 )
            else
              C0m2zero_dd = C0m2zero_dd - sgn/rlam*(
     &          -log(m02-p01-abs(m02-p01)*ieps2)*log(-xx(i))
     &          -log(-xx(i))**2/2._rk )
              arg(4)  = 1._rk-x(i)/x(4)
              arge(4) = 1._rk-xe(i)/xe(4)
              if (abs(aimag(arg(4))).lt.crit*abs(arg(4)))
     &          arg(4) = inteps(arg(4),arge(4))
              C0m2zero_dd = C0m2zero_dd - sgn/rlam*cspen_dd(arg(4))
              etax = eta_dd(-xe(i),-1._rk/xe(4))
              if (etax.ne.(0._rk,0._rk)) C0m2zero_dd = C0m2zero_dd
     &                               - sgn/rlam*etax*log(arg(4)) 
            endif
          else
            if (p12.ne.0._rk) C0m2zero_dd = C0m2zero_dd
     &                        + sgn/rlam*log(m12)*log(-xx(i))
            do 101 j=3,4
            if (x(j).ne.acmplx(1.e30_rk)) then
              arg(j)  = 1._rk-x(i)/x(j)
              arge(j) = 1._rk-xe(i)/xe(j)
              if (abs(aimag(arg(j))).lt.crit*abs(arg(j)))
     &          arg(j) = inteps(arg(j),arge(j))
              C0m2zero_dd = C0m2zero_dd - sgn/rlam*cspen_dd(arg(j))
              etax = eta_dd(-xe(i),-1._rk/xe(j))
              if (etax.ne.(0._rk,0._rk)) C0m2zero_dd = C0m2zero_dd
     &                               - sgn/rlam*etax*log(arg(j)) 
            endif
101         continue
          endif

          if (acmplx(p12).eq.m12) then
            C0m2zero_dd = C0m2zero_dd - sgn/rlam*(
     &         log(m02-p20-abs(m02-p20)*ieps2)*log(-xx(i))
     &        +log(-xx(i))**2/2._rk )
          else
            if (p12.ne.0._rk) C0m2zero_dd = C0m2zero_dd
     &         - sgn/rlam*log(m12-p12-abs(m12-p12)*ieps2)*log(-xx(i))
            if (x(5).ne.acmplx(1.e30_rk)) then
              arg(5)  = 1._rk-x(i)/x(5)
              arge(5) = 1._rk-xe(i)/xe(5)
              if (abs(aimag(arg(5))).lt.crit*abs(arg(5)))
     &           arg(5) = inteps(arg(5),arge(5))
              C0m2zero_dd = C0m2zero_dd + sgn/rlam*cspen_dd(arg(5))
              etax = eta_dd(-xe(i),-1._rk/xe(5))
              if (etax.ne.(0._rk,0._rk)) C0m2zero_dd = C0m2zero_dd
     &                               + sgn/rlam*etax*log(arg(5))
            endif
          endif

          C0m2zero_dd = C0m2zero_dd + sgn/rlam*cspen_dd(1._rk+xx(i))

100     continue

999     continue

        end

        end module dd_3pt_qp
