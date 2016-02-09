*********************************************************************r
        subroutine C_dd(C,Cuv,p01,p12,p20,m02,m12,m22,r2,id)
**********************************************************************
*       3-point coefficients  
*       C(i,j,k) = C_{0...01...12...2}(p01,p12,p20,m02,m12,m22)
*                     \___/\___/\___/
*                       i    j    k   indices
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

#ifdef collierdd
c LH  global cache system
        use cache
#endif

        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        complex*16 C(0:2*rmax3,0:rmax3,0:rmax3),C0dd
        complex*16 Cuv(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 C_newprelim(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 Cuv_newprelim(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 C_new(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 Cuv_new(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 m02,m12,m22,f(2),detx2
        complex*16 x2(0:2,0:2),tx2(0:2,0:2),ttx2(0:2,0:2,0:2,0:2)
        real*8 z2(2,2),tz2(2,2),z2i(2,2),ttz2(2,2,2,2)
        integer r2,r,i,igc,j,jg,jgc,k,ksm,l,n,sgn(2),count
        integer i0,i1,i2,i12,id
        integer r1eff,r2eff,nid(0:nmax-1),qmethod_newprelim
        logical qsoft0,qsoft1,qsoft2,qcoll01,qcoll12,qcoll20
        logical qcoll01_12,qcoll12_20,qcoll20_01
        logical gp_ok,g2p_ok,gcp_ok,sm1_ok,sm2_ok
        logical qnewmethodfixed
#ifdef collierdd
c LH: variables for cache system
        complex*16 xcache(6), fct(NCoefmax3)
        integer n0,n1,n2,cnt,cntmax
        logical nocalc/.false./, wrica/.false./
        save wrica
#endif

c       if (id.eq.8) then
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
        gerr   = 0d0
        g2err  = 0d0
        gcerr  = 0d0
        sm1err = 0d0
        sm2err = 0d0
        besterr= 0d0

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

c initialization for master call
        if (id.eq.0) then
          r2eff = 0
          do i=0,7
            r2_aux(i)     = -1
            r20_aux(i)    = -1
            r2_new_aux(i) = -1
            do r=0,r2
              resaccrel(i,r)  = 0d0
              resaccabs(i,r)  = 0d0
              resaccrel2(i,r) = 0d0
              resaccabs2(i,r) = 0d0
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
        cutacc    = 1d20
        gcrit     = .2d0
        g2crit    = .2d0
        gccrit    = .2d0
        sm1crit   = .2d0
        sm2crit   = .2d0
        reqacc    = 10d0

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
        q12 = (p01+p20-p12)/2d0

        sgn(1) = +1
        sgn(2) = -1

c Gram and related matrices
        z2(1,1)  = 2d0*q1
        z2(1,2)  = 2d0*q12
        z2(2,1)  = z2(1,2)
        z2(2,2)  = 2d0*q2
        tz2(1,1) = z2(2,2)
        tz2(1,2) = -z2(2,1)
        tz2(2,1) = tz2(1,2)
        tz2(2,2) = z2(1,1)
        detz2    = 4d0*(q1*q2-q12**2)
        do 100 i=1,2
        do 100 j=1,2
          z2i(i,j) = tz2(j,i)/detz2
100     continue
        do 101 i=1,2
        do 101 j=1,2
          ttz2(i,  i,j,3-j) = 0d0
          ttz2(i,3-i,j,  j) = 0d0
          ttz2(i,  i,j,  j) = 0d0
          ttz2(i,3-i,j,3-j) = -sgn(i)*sgn(j)
101     continue

c Cayley and related matrices
        f(1) = q1-m12+m02
        f(2) = q2-m22+m02
        x2(0,0) = 2d0*m02
        do 200 i=1,2
          x2(0,i) = f(i)
          x2(i,0) = f(i)
        do 200 j=1,2
          x2(i,j) = z2(i,j)
200     continue
        detx2 = 2d0*m02*detz2 + 2d0*f(1)*f(2)*z2(1,2)
     &          - f(1)**2*z2(2,2) - f(2)**2*z2(1,1)
c detx2=0 for soft-singular C0
        if (((m02.eq.dcmplx(0d0,0d0)).and.
     &       (dcmplx(p01).eq.m12).and.(dcmplx(p20).eq.m22)) .or.
     &      ((m12.eq.dcmplx(0d0,0d0)).and.
     &       (dcmplx(p12).eq.m22).and.(dcmplx(p01).eq.m02)) .or.
     &      ((m22.eq.dcmplx(0d0,0d0)).and.
     &       (dcmplx(p20).eq.m02).and.(dcmplx(p12).eq.m12)) ) detx2=0d0
        tx2(0,0) = detz2
        do 201 i=1,2
          tx2(0,i)   = -tz2(i,1)*f(1)-tz2(i,2)*f(2)
          tx2(i,0)   = tx2(0,i)
          tx2(i,i)   = 2d0*m02*tz2(i,i)  -f(3-i)*f(3-i)
          tx2(i,3-i) = 2d0*m02*tz2(i,3-i)+f(i)*f(3-i)
201     continue
        do 202 i=1,2
        do 202 j=1,2
          ttx2(0,  i,0,  j) = -tz2(i,j)
          ttx2(i,  0,j,  0) = -tz2(i,j)
          ttx2(0,  i,j,  0) =  tz2(i,j)
          ttx2(i,  0,0,  j) =  tz2(i,j)
          ttx2(0,  i,j,  j) =  0d0
          ttx2(i,  0,j,  j) =  0d0
          ttx2(j,  j,0,  i) =  0d0
          ttx2(j,  j,i,  0) =  0d0
          ttx2(0,  i,j,3-j) = -f(3-i)*ttz2(3-i,i,j,3-j) 
          ttx2(i,  0,j,3-j) = -ttx2(0,i,j,3-j)
          ttx2(j,3-j,0,  i) =  ttx2(0,i,j,3-j) 
          ttx2(j,3-j,i,  0) = -ttx2(0,i,j,3-j) 
202     continue

c initialize error propagation 
        maxz(id)  = max(abs(z2(1,1)),abs(z2(1,2)),abs(z2(2,2)))
        maxzi(id) = max(abs(z2i(1,1)),abs(z2i(1,2)),abs(z2i(2,2)))
        am02(id)  = abs(m02)
        maxzif(id)= max( abs(z2i(1,1)*f(1)+z2i(1,2)*f(2)),
     &                   abs(z2i(2,1)*f(1)+z2i(2,2)*f(2)) )
c       ziff(id)  = abs( z2i(1,1)*f(1)**2+2d0*z2i(1,2)*f(1)*f(2)
c    &                  +z2i(2,2)*f(2)**2 )
        ziff(id)  = abs(2d0*m02-detx2/detz2)
        do i=0,2*rmax3
          Cij_err(id,i)       = 0d0
          Cij_err2(id,i)      = 0d0
          C00_err(id,i)       = 0d0
          accr2_aux(id,i)     = 0d0
          Cij_err_newprelim(id,i) = 0d0
          C00_err_newprelim(id,i) = 0d0
          accr2_newprelim(id,i)   = 0d0
          Cij_err_new(id,i)   = 0d0
          C00_err_new(id,i)   = 0d0
          accr2_new_aux(id,i) = 0d0
        enddo

c scalar 3pt integral
c--------------------

        C(0,0,0)    = C0dd(p01,p12,p20,m02,m12,m22,0)
        Cuv(0,0,0)  = 0d0
        scalint(id) = C(0,0,0)
        scalintnew(id) = 0d0

        Cij_err(id,0)  = 1d-14*max( 1d0/sqrt(abs(detz2)),
     &                              abs(scalint(id)) )
        Cij_err2(id,0)  = Cij_err(id,0)
        C00_err(id,0)   = 0d0
        accr2_aux(id,0) = abs(Cij_err(id,0))/abs(scalint(id))

#ifdef collierdd
c LH: read global cache
c =====================
        if (use_cache_system) then
          if ((ncache.gt.0).and.(ncache.le.ncache_max)) then
              xcache(1) = p01
              xcache(2) = p12
              xcache(3) = p20
              xcache(4) = m02
              xcache(5) = m12
              xcache(6) = m22

              cntmax = 2*NCoefsG(r2,3)+8*r2+11 

              if (cntmax.gt.NCoefmax3) then
                if (cout_on.and.(cout.le.coutmax)) then
                  write(outchannel,*) 'NCoefmax3 too small!'
                  if (cout.eq.coutmax) call DDlastmessage()
                  cout = cout+1
                endif
                stopflag = min(-9,stopflag)
              endif

              call ReadCache(fct(1:cntmax),cntmax,xcache,6,2,id,3,r2,nocalc,wrica)

              if (nocalc) then
                accflag   = fct(1)
                errflag   = fct(2)
                stopflag  = min(int(fct(3)),stopflag)
                cnt = 3

                do r=0,r2
                  cnt = cnt+1
                  resaccabs(id,r)  = fct(cnt)
                  cnt = cnt+1
                  resaccrel(id,r)  = fct(cnt)
                  cnt = cnt+1
                  Cij_err(id,r)    = fct(cnt)
                  cnt = cnt+1
                  resaccabs2(id,r) = fct(cnt)
                  cnt = cnt+1
                  resaccrel2(id,r) = fct(cnt)
                  cnt = cnt+1
                  Cij_err2(id,r)   = fct(cnt)
                  cnt = cnt+1
                  C00_err(id,r)    = fct(cnt)
                  cnt = cnt+1
                  accr2_aux(id,r)  = fct(cnt)
                  do n0=0,2*r,2
                    do n1=0,r-n0/2
                      n2 = r-n0/2-n1

                      cnt = cnt+1
                      C(n0,n1,n2) = fct(cnt)
                      cnt = cnt+1
                      Cuv(n0,n1,n2) = fct(cnt)

                    end do
                  end do
                end do
                do n=0,2
                  r2_aux(nid(n)) = -1
                enddo
                r2_aux(id)     = -1
                r2_new_aux(id) = -1

                return
              endif
          end if
        end if
c =====================
#endif

        else
c read cached information for repeated calls
c-------------------------------------------

        do 500 r=0,r2_aux(id)
        do 500 i0=0,2*r,2
        i12 = r-i0/2
        do 500 i1=0,i12
          i2 = i12-i1
          C(i0,i1,i2)   = C_aux(id,i0,i1,i2)  
          Cuv(i0,i1,i2) = Cuv_aux(id,i0,i1,i2)
500     continue

        if (qmethod_new(id).ge.200) then
          do 503 r=0,r2_new_aux(id)
          do 503 i0=0,2*r,2
          i12 = r-i0/2
          do 503 i1=0,i12
            i2 = i12-i1
            C_newprelim(i0,i1,i2)   = C_new_aux(id,i0,i1,i2)
            Cuv_newprelim(i0,i1,i2) = Cuv_new_aux(id,i0,i1,i2)
            C_new(i0,i1,i2)         = C_new_aux(id,i0,i1,i2)
            Cuv_new(i0,i1,i2)       = Cuv_new_aux(id,i0,i1,i2)
503       continue
        endif

        do 501 i=1,2
        do 501 j=1,2
          z2(i,j)  = z2_aux(id,i,j) 
          tz2(i,j) = tz2_aux(id,i,j)
          z2i(i,j) = z2i_aux(id,i,j)
        do 501 k=1,2
        do 501 l=1,2
          ttz2(i,j,k,l) = ttz2_aux(id,i,j,k,l)
501     continue
        do 502 i=0,2
        do 502 j=0,2
          x2(i,j)  = x2_aux(id,i,j) 
          tx2(i,j) = tx2_aux(id,i,j)
        do 502 k=0,2
        do 502 l=0,2
          ttx2(i,j,k,l) = ttx2_aux(id,i,j,k,l)
502     continue
        detz2 = auxr(id,1)
        detx2 = auxc(id,1)
        f(1)  = auxc(id,2)
        f(2)  = auxc(id,3)

        endif

c quit if no tensors are needed
        if (r2.le.0) then
          r1eff = 0
          r2eff = r2
          if (r2_aux(id).eq.-1) then
            acc = 0d0
            accnew = 1d30
          else
            acc = acc_pave(id)
            accnew = acc_new(id)
          endif
          if (qmethod(id).gt.0) acc=1d30
          goto 599
        endif

c Tensor reduction 
c=================

c*** PaVe reduction available for any tensor rank
c       accumulating tensor rank supported

        if (qmethod(id).gt.0) then
          acc=1d30
        else
          if (r2_aux(id).le.0) then
            r1eff = 0
          else
            r1eff = r2_aux(id)+1
          endif
          if (r1eff.le.r2) then
            call Cpave_dd(C,Cuv,p01,p12,p20,m02,m12,m22,acc,
     &                  z2,z2i,f,r1eff,r2,id,nid)
          else
            acc = accr2_aux(id,r2)
          endif

          if (acc.gt.cutacc) acc = cutacc
          r2eff  = r2

          if (qmethod_new(id).gt.0) then
            accnew = accr2_new_aux(id,r2)
          else
            accnew = 1d30
          endif
        endif

        if ((acc.lt.cacc).or.(mode34.eq.0)) goto 599

c set start rank for improvement method
        if (r2_new_aux(id).le.0) then
          r1eff = 0
        else
          r1eff = r2_new_aux(id)+1
        endif

        if (r1eff.gt.r2) then
          if (acc_new(id).lt.1d31) 
     &      accnew = accr2_new_aux(id,r2)
          goto 599
        endif

c mode34=1: improvement by Cayley reduction + soft/collinear special cases
c-------------------------------------------------------------------------
        if (mode34.eq.1) then

          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'mode34=1: not fully supported.'
            write(outchannel,*) 
     &         '          Use only for debugging puproses.'
            if (cout.eq.coutmax) call DDlastmessage()
            cout = cout+1
          endif
          stopflag = min(-10,stopflag)

c*** soft/collinear singular integrals
          qsoft0  = (m02.eq.(0d0,0d0)).and.
     &              (dcmplx(p01).eq.m12).and.(dcmplx(p20).eq.m22)
          qsoft1  = (m12.eq.(0d0,0d0)).and.
     &              (dcmplx(p12).eq.m22).and.(dcmplx(p01).eq.m02)
          qsoft2  = (m22.eq.(0d0,0d0)).and.
     &              (dcmplx(p20).eq.m02).and.(dcmplx(p12).eq.m12)
          qcoll01 = (abs(m02+m12)+abs(p01).lt.1d-15)
          qcoll12 = (abs(m12+m22)+abs(p12).lt.1d-15)
          qcoll20 = (abs(m22+m02)+abs(p20).lt.1d-15)
          qcoll01_12 = qcoll01.and.qcoll12
          qcoll12_20 = qcoll12.and.qcoll20
          qcoll20_01 = qcoll20.and.qcoll01

          if (qsoft0.or.qsoft1.or.qsoft2.or.
     &          qcoll01.or.qcoll12.or.qcoll20) detx2 = 0d0

          if (qsoft0.or.qsoft1.or.qsoft2) then
c soft-singular case (always detx2=0)
c improvement not yet implemented
            accnew = 1d30
            r2_new_aux(id) = 0
            qmethod_new(id) = 100

          elseif (qcoll01_12.or.qcoll12_20.or.qcoll20_01) then
c double-collinear-singular case (always detx2=0)
c improvement not yet implemented
            accnew = 1d30
            r2_new_aux(id) = 0
            qmethod_new(id) = 101

          elseif (qcoll01.or.qcoll12.or.qcoll20) then
c single-collinear-singular case (always detx2=0)
c returns always tensor rank 4
            call Ccoll_dd(C_new,Cuv_new,p01,p12,p20,m02,m12,m22,accnew,
     &                    r2+r2,id)
            r2_new_aux(id) = r2
            qmethod_new(id) = 102
          else

c*** Cayley reduction available for any tensor rank 
c       accumulating tensor rank NOT supported

          call Ccayley_dd(C_new,Cuv_new,
     &          p01,p12,p20,m02,m12,m22,accnew,
     &          detx2,tx2,f,r2,id,nid)
            r2_new_aux(id) = r2
            qmethod_new(id) = 103
          endif

c mode34=2: improvement by expansion in small Gram/kinematical determinants
c--------------------------------------------------------------------------
        elseif (mode34.eq.2) then

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
            scale2c  = 1d0/abs(scalint(id))
            scale2in = ( abs(p01)+abs(p12)+abs(p20)
     &                  +abs(m02)+abs(m12)+abs(m22) )/6d0
            scale2hi = max(scale2c,scale2in)
            scale2lo = min(scale2c,scale2in)

c determine indices k,l for expansions
            maxtz_kl = 0d0
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
            ttzff_kl(id)    = abs(tx2(k,l)-2d0*m02*tz2(k,l))

c determine indices i=igc,j=jgc for Gram/Cayley
            maxtx_0j = 0d0
            do i1=1,2
              if (abs(tx2(0,i1)).ge.maxtx_0j) then
                maxtx_0j = abs(tx2(0,i1))
                jg = i1
              endif
            enddo
            maxtz_nj(id) = 0d0
            do n=1,2
              maxtz_nj(id) = max(maxtz_nj(id),abs(tz2(n,jg)))
            enddo

c determine indices i=igc,j=jgc for Gram/Cayley
            maxtx2ij = 0d0
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
            maxf = 0d0
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
            b0err = 1d-14
            cmiss = 1d0/scale2lo
            aa = max( 1d0, maxtx2ij/maxtx_0j, maxttx0klm(id)/maxtz_kl )
            bb = max( 1d0, maxtx2ij/maxtx_0j )

c expansion parameter for Gram
            if ((maxtz_kl.ne.0d0).and.(maxtx_0j.ne.0d0)) then
cold              gparam = max(abs(detz2)/maxtx_0j,
cold     &                     abs(detz2)/scale2lo/maxtz_kl)
              gparam = abs(detz2)/maxtx_0j
            else
              gparam = 1d10
            endif
c error propagation for Gram to rank=r2+2
            gp_ok = (gparam.lt.gcrit)
            if (gp_ok) then
cold              gerr1 = maxttx0klm(id)/maxtz_kl
cold              gerr2 = maxttz_knlm(id)/maxtz_kl*scale2hi
cold              gerr3 = ttzff_kl(id)/maxtz_kl   /scale2lo
cold              gerr4 = maxtz_nj(id)/maxtx_0j   *scale2hi
cold              gerr  = max(gerr1,gerr2,gerr3,gerr4)
cold              gerr  = gerr*(gerr*gparam)**r2
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
            gerr  = max(gerr1,gerr2)
            else
              gerr  = 1d10
            endif

c expansion parameter for Gram - version 2
            g2param = abs(detz2)/abs(detx2)*scale2hi 
c error propagation for Gram - version 2 up to rank=r2+2
            g2p_ok = (g2param.lt.g2crit)
            if (g2p_ok) then
cold            g2err  = maxtx_0j/abs(detx2)*scale2hi
cold            g2err  = g2err*(g2err*g2param)**r2
              g2err1 = b0err * max( 
     &                 maxtx_0j/abs(detx2)  
     &                  * max( 1d0,(scale2hi*maxtx_0j/abs(detx2))**r2 ),
     &                 g2param**(r2+1)/scale2lo )
              g2err2 = cmiss * g2param**(r2+1)
              g2err  = max( g2err1, g2err2 )
            else
              g2err  = 1d10
            endif

c expansion parameters for Gram/Cayley
            if ((maxtz_kl.ne.0d0).and.(tx2(igc,jgc).ne.0d0)) then
cold              gcparam = max( abs(tx2(k,0))/maxtz_kl/scale2lo,
cold     &                       abs(tx2(0,jgc)/tx2(igc,jgc)),
cold     &                       abs(detz2)/maxtz_kl/scale2lo )
              gcparam = max( abs(detz2/tx2(igc,jgc)),
     &                       abs(maxtx_0j/tx2(igc,jgc)) )
            else
              gcparam = 1d10
            endif
c error propagation for Gram/Cayley up to rank=r2+2
            gcp_ok = (gcparam.lt.gccrit)
            if (gcp_ok) then
cold              gcerr1 = abs(tz2(igc,jgc)/tx2(igc,jgc))*scale2hi
cold              gcerr2 = maxttx0ijm(id)/abs(tx2(igc,jgc))*scale2hi
cold              gcerr  = max(gcerr1,gcerr2) 
cold              gcerr  = gcerr*(gcerr*gcparam)**r2
              gcerr1 = b0err*max(maxtz_kl,maxttx0ijm(id))/maxtx2ij
              gcerr2 = cmiss * gcparam**(r2+1)  
              gcerr  = max( gcerr1, gcerr2 )
            else
              gcerr  = 1d10
            endif

c expansion parameters for small momentum - variant 1 (f(ksm) not small)
            if (maxf.ne.0d0) then
cold              sm1param = max(maxz(id)/maxf,maxz(id)/scale2lo)
              sm1param = max(maxz(id)/maxf,maxz(id)*abs(m02)/maxf**2)
            else
              sm1param = 1d10
            endif
c error propagation for small momentum up to rank=r2+2
            sm1_ok  = (sm1param.lt.sm1crit)
            if (sm1_ok) then
cold              sm1err1 = scale2hi/maxf
cold              sm1err2 = abs(m02)/scale2lo
cold              sm1err  = max(sm1err1,sm1err2)
cold              sm1err  = sm1err*(sm1err*sm1param)**r2
              sm1err1 = b0err/maxf * max( 1d0, abs(m02/maxf) )**r2 
              sm1err2 = cmiss * abs(detz2/maxf)**(r2+1) 
     &                        * max( 1d0, abs(m02/maxf) )**r2 
              sm1err  = max( sm1err1, sm1err2 )
            else
              sm1err  = 1d10
            endif
        
c expansion parameters for small momentum - variant 2 (all f(k) small)
            if (abs(m02).lt.1d-5) then
              sm2param = 1d10
            else
              sm2param = max(maxf/scale2lo,maxz(id)/abs(m02))
            endif
c error propagation for small momentum
            sm2_ok = (sm2param.lt.sm2crit)
            if (sm2_ok) then
              sm2err = sm2param**r2
            else
              sm2err  = 1d10
            endif
c not yet implemented
            sm2_ok = .false.

          endif

c find optimal method
          Cij_err_new_max = 1d33
          if (qmethod_new(id).eq.0) then
            qnewmethodfixed = .false.
          elseif (qmethod_new(id).gt.0) then
            qnewmethodfixed = .true.
          elseif (qmethod_new(id).eq.-1) then
            accnew = 1d30
            goto 599
          endif
777       continue
          besterr = 1d10
          if (.not.qnewmethodfixed) then
            if (gp_ok)  besterr = min(besterr,gerr)
            if (g2p_ok) besterr = min(besterr,g2err)
            if (gcp_ok) besterr = min(besterr,gcerr)
            if (sm1_ok) besterr = min(besterr,sm1err)
            if (sm2_ok) besterr = min(besterr,sm2err)
          elseif (qnewmethodfixed) then
            r2_newprelim(id) = r2_new_aux(id) 
            do r=0,r2_new_aux(id)
              Cij_err_newprelim(id,r) = Cij_err_new(id,r)
              C00_err_newprelim(id,r) = C00_err_new(id,r)
              accr2_newprelim(id,r) = accr2_new_aux(id,r)
            enddo
          endif

c       if (id.eq.8) then
c       write(*,*) 'C id ',id
c       write(*,*) 'besterr  ',besterr
c       write(*,*) 'sm1param ',sm1_ok,sm1param,sm1err
c       write(*,*) 'sm2param ',sm2_ok,sm2param,sm2err
c       write(*,*) 'gparam   ',gp_ok,gparam,gerr
c       write(*,*) 'g2param  ',g2p_ok,g2param,g2err
c       write(*,*) 'gcparam  ',gcp_ok,gcparam,gcerr
c       write(*,*) 'method   ',qmethod_new(id)
c       write(*,*) 'scale2hi', scale2hi
c       write(*,*) 'scale2lo', scale2lo
c       write(*,*) 'igc,jgc ', igc,jgc 
c       write(*,*) tx2(igc,jgc)
c       write(*,*) 'ccc ', p01,p12,p20,m02,m12,m22
c        endif

c decide on expansion
          if (((qnewmethodfixed).and.(qmethod_new(id).eq.200)).or.
     &        (gp_ok.and.(besterr.eq.gerr))) then
            qmethod_newprelim = 200
            call Cgram_dd(C_newprelim,Cuv_newprelim,
     &          p01,p12,p20,m02,m12,m22,jg,k,l,
     &          accnewprelim,detz2,tz2,ttz2,tx2,f,
     &          r1eff,r2,r2_newprelim(id),id,nid)
            gp_ok = .false.
          elseif (((qnewmethodfixed).and.(qmethod_new(id).eq.201)).or.
     &            (gcp_ok.and.(besterr.eq.gcerr))) then
            qmethod_newprelim = 201
            call Cgramcayley_dd(C_newprelim,Cuv_newprelim,
     &          p01,p12,p20,m02,m12,m22,
     &          igc,jgc,k,l,accnewprelim,detz2,tz2,ttz2,tx2,f,
     &          r1eff,r2,r2_newprelim(id),id,nid)
            gcp_ok = .false.
          elseif (((qnewmethodfixed).and.(qmethod_new(id).eq.203)).or.
     &            (sm1_ok.and.(besterr.eq.sm1err))) then
            qmethod_newprelim = 203
            call Csm1_dd(C_newprelim,Cuv_newprelim,
     &          p01,p12,p20,m02,m12,m22,ksm,
     &          accnewprelim,z2,f,r1eff,r2,r2_newprelim(id),id,nid)
            sm1_ok = .false.
          elseif (((qnewmethodfixed).and.(qmethod_new(id).eq.205)).or.
     &            (g2p_ok.and.(besterr.eq.g2err))) then
            qmethod_newprelim = 205
            call Cgram2_dd(C_newprelim,Cuv_newprelim,
     &          p01,p12,p20,m02,m12,m22,
     &          accnewprelim,detz2,detx2,tx2,f,
     &          r1eff,r2,r2_newprelim(id),id,nid)
            g2p_ok = .false.
          elseif (((qnewmethodfixed).and.(qmethod_new(id).eq.202)).or.
     &            (besterr.eq.1d10)) then
            if (abs(detx2*C(0,0,0)**2).gt.1d-8) then
              C_newprelim(0,0,0) = C(0,0,0)
              qmethod_newprelim = 202
              call Calpave_dd(C_newprelim,Cuv_newprelim,
     &          p01,p12,p20,m02,m12,m22,
     &          accnewprelim,detz2,f,detx2,tx2,r1eff,r2,id,nid)
              r2_newprelim(id) = r2
            else
              accnewprelim = 1d30
              qmethod_newprelim = 0
            endif
          endif

c*** result acceptable
            if ((qnewmethodfixed).or.(accnewprelim.lt.reqacc)) then
              Cij_err_newprelim_max = 0d0
              do r=0,r2
                 Cij_err_newprelim_max = max( Cij_err_newprelim_max,
     &                                        Cij_err_newprelim(id,r) )
              enddo
              if (Cij_err_newprelim_max.lt.Cij_err_new_max) then
                qmethod_new(id) = qmethod_newprelim
                r2_new_aux(id)  = r2_newprelim(id)
                accnew          = accnewprelim
                Cij_err_new_max = Cij_err_newprelim_max
                do 900 r=0,r2_new_aux(id)
                  Cij_err_new(id,r) = Cij_err_newprelim(id,r)
                  C00_err_new(id,r) = C00_err_newprelim(id,r)
                  accr2_new_aux(id,r) = accr2_newprelim(id,r)
                  do 900 i0=0,2*r,2
                  i12 = r-i0/2
                  do 900 i1=0,i12
                    i2 = i12-i1
                    C_new(i0,i1,i2)   = C_newprelim(i0,i1,i2)
                    Cuv_new(i0,i1,i2) = Cuv_newprelim(i0,i1,i2)
900             continue
              endif
            endif

c*** try other expansions if previous expansion was inappropriate
          if ((qmethod_newprelim.eq.200).or.
     &        (qmethod_newprelim.eq.201).or.
     &        (qmethod_newprelim.eq.203).or.
     &        (qmethod_newprelim.eq.205)) then
            if ((accnewprelim.gt.cacc).and.(.not.qnewmethodfixed)) then
c	if (id.eq.1) then
c             write(*,*) 'C: ',qmethod_newprelim,' failed',accnewprelim
c             write(*,*) 'besterr    ',besterr 
c             write(*,*) 'gram       ',gp_ok,gerr
c             write(*,*) 'gram2      ',g2p_ok,g2err
c             write(*,*) 'gramcayley ',gcp_ok,gcerr
c             write(*,*) 'sm1        ',sm1_ok,sm1err
c             write(*,*) 'sm2        ',sm2_ok,sm2err
c	endif
              goto 777
            endif
          endif

        endif

599     continue

c*** Final result 
c================

        if (accnew.gt.cutacc) accnew = cutacc

        if ((qmethod(id).eq.0).and.(qmethod_new(id).gt.0).and.
     &      (accnew.gt.reqacc)) then
          qmethod_new(id) = -1
          accnew =1d31
        endif

        acc_pave(id) = acc
        acc_new(id)  = accnew
        qmethod(id)  = 0
        do r=0,r2
          resaccrel(id,r) = accr2_aux(id,r)
        enddo

        Cij_err_max     = 0d0
        Cij_err_new_max = 0d0
        if (qmethod_new(id).gt.0) then
          do r=0,r2
            Cij_err_max     = max(Cij_err_max,Cij_err(id,r))
            Cij_err_new_max = max(Cij_err_new_max,Cij_err_new(id,r))
          enddo
        endif

c        if ((qmethod_new(id).gt.0).and.(accnew.lt.acc)) then
        if ((qmethod_new(id).gt.0).and.
     &      ((acc.eq.1d30).or.(Cij_err_new_max.lt.Cij_err_max))) then
          r2eff = r2_new_aux(id)
          qmethod(id) = qmethod_new(id)

          do 550 r=0,r2eff
c            if ((accr2_aux(id,r).ne.0d0).and.
c     &          (accr2_aux(id,r).lt.accr2_new_aux(id,r))) goto 550
            if ((accr2_aux(id,r).ne.0d0).and.
     &          (Cij_err(id,r).lt.Cij_err_new(id,r))) goto 550
            Cij_err(id,r)  = Cij_err_new(id,r)
            Cij_err2(id,r) = Cij_err(id,r)
            C00_err(id,r)  = C00_err_new(id,r)
            resaccrel(id,r) = accr2_new_aux(id,r)
            do 551 i0=0,2*r,2
            i12 = r-i0/2
            do 551 i1=0,i12
              i2 = i12-i1
              C(i0,i1,i2)   = C_new(i0,i1,i2)
              Cuv(i0,i1,i2) = Cuv_new(i0,i1,i2)
551         continue
550       continue
        endif

c accuracy estimate
c==================
          count = 1
c normalization to typical coefficient
c          Ctyp  = log(abs(C(0,0,0)))
c          do r=1,r2
c            count = count+2
c            Ctyp = Ctyp + log(abs(C(0,r,0))) + log(abs(C(0,0,r)))
c          enddo
c          Ctyp = exp(Ctyp/count)
c          do r=0,r2
c            resaccabs(r) = Ctyp*resaccrel(r)
c            if (resaccrel(r).gt.aimacc(3)) accflag = 1
c            if (resaccrel(r).gt.erracc(3)) errflag = 1
c          enddo    
          Cmax = abs(C(0,0,0))
          do r=1,r2
            i0=0
            i12 = r-i0
            do i1=0,i12
              i2=i12-i1
              Cmax = max(Cmax,abs(C(i0,i1,i2)))
            enddo
          enddo
          do r=0,r2
            resaccabs(id,r)  = Cmax*resaccrel(id,r)
            resaccabs2(id,r) = Cij_err2(id,r) 
            resaccrel2(id,r) = Cij_err2(id,r)/Cmax
            if (resaccrel(id,r).gt.aimacc(3)) accflag = 1
            if (resaccrel(id,r).gt.erracc(3)) errflag = 1
          enddo

c store for checking purposes
c============================
        scalintnew(id) = C_new(0,0,0)

c cache information
c==================

        r2_aux(id) = max(r2_aux(id),r2eff)

        do 600 r=0,r2eff
        do 600 i0=0,2*r,2
        i12 = r-i0/2
        do 600 i1=0,i12
          i2 = i12-i1
          C_aux(id,i0,i1,i2)   = C(i0,i1,i2)
          Cuv_aux(id,i0,i1,i2) = Cuv(i0,i1,i2)
600     continue

        if (qmethod_new(id).ge.200) then
          do 610 r=0,r2_new_aux(id)
          do 610 i0=0,2*r,2
          i12 = r-i0/2
          do 610 i1=0,i12
            i2 = i12-i1
            C_new_aux(id,i0,i1,i2)   = C_new(i0,i1,i2)
            Cuv_new_aux(id,i0,i1,i2) = Cuv_new(i0,i1,i2)
610       continue
        endif

        if (r1eff.eq.0) then
          do 601 i=1,2
          do 601 j=1,2
            z2_aux(id,i,j)  = z2(i,j)
            tz2_aux(id,i,j) = tz2(i,j)
            z2i_aux(id,i,j) = z2i(i,j)
          do 601 k=1,2
          do 601 l=1,2
            ttz2_aux(id,i,j,k,l) = ttz2(i,j,k,l)
601       continue
          do 602 i=0,2
          do 602 j=0,2
            x2_aux(id,i,j)  = x2(i,j)
            tx2_aux(id,i,j) = tx2(i,j)
          do 602 k=0,2
          do 602 l=0,2
            ttx2_aux(id,i,j,k,l) = ttx2(i,j,k,l)
602       continue
          auxr(id,1) = detz2
          auxc(id,1) = detx2
          auxc(id,2) = f(1)
          auxc(id,3) = f(2)
        endif

#ifdef collierdd
c LH: write global cache
c== ====================
        if (use_cache_system) then
          if ((ncache.gt.0).and.(ncache.le.ncache_max)) then
              if (wrica) then
                fct(1) = accflag   
                fct(2) = errflag   
                fct(3) = stopflag   
                cnt = 3

                do r=0,r2
                  cnt = cnt+1
                  fct(cnt) = resaccabs(id,r) 
                  cnt = cnt+1
                  fct(cnt) = resaccrel(id,r) 
                  cnt = cnt+1
                  fct(cnt) = Cij_err(id,r)   
                  cnt = cnt+1
                  fct(cnt) = resaccabs2(id,r) 
                  cnt = cnt+1
                  fct(cnt) = resaccrel2(id,r) 
                  cnt = cnt+1
                  fct(cnt) = Cij_err2(id,r)   
                  cnt = cnt+1
                  fct(cnt) = C00_err(id,r)   
                  cnt = cnt+1
                  fct(cnt) = accr2_aux(id,r) 
                  do n0=0,2*r,2
                    do n1=0,r-n0/2
                      n2=r-n0/2-n1

                      cnt = cnt+1
                      fct(cnt) = C(n0,n1,n2)
                      cnt = cnt+1
                      fct(cnt) = Cuv(n0,n1,n2)

                    end do
                  end do
                end do

                call WriteCache(fct(1:cnt),cnt,id,3,r2)

              end if
          end if
        end if
c== ====================
#endif

c test output
c============
        if ((outlevel.gt.0).and.
     &      (acc_pave(id).ge.1d0).and.(acc_new(id).ge.1d0))
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

c       write(*,*) 'C0   ', C(8,0,0)
c       write(*,*) 'C0   ', C(0,0,0)
c       write(*,*) 'C1   ', C(0,1,0)
c       write(*,*) 'C2   ', C(0,0,1)
c       write(*,*) 'C11  ', C(0,2,0)
c       write(*,*) 'C22  ', C(0,0,2)
c       write(*,*) 'C111 ', C(0,3,0)
c       write(*,*) 'c222 ', C(0,0,3)
c       write(*,*) 'C001 ', C(2,1,0)
c       write(*,*) 'C002 ', C(2,0,1)

        end

**********************************************************************
        subroutine Cpave_dd(C,Cuv,p01,p12,p20,m02,m12,m22,acc,
     &                      z2,z2i,f,r1,r2,id,nid)
**********************************************************************
*       Passarino-Veltman reduction
*
*       3-point coefficients  
*       C(i,j,k) = C_{0...01...12...2}(p01,p12,p20,m02,m12,m22)
*                     \___/\___/\___/
*                       i    j    k   indices
*       of rank r=i+j+k with r1 <= r <= r2
*       coefficients with rank r < r1 assumed to be known
*
*       Cuv(i,j,k) = coefficient of 1/eps in C(i,j,k),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       12.4.2006 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        integer r1,r2,i(2),j(2),k,l(2),m,n,r,i0,i1,i2,i3,i12
        integer del(2,2),id,nid(0:nmax-1),count
        data del/1,0,0,1/
        complex*16 B0(0:2*rmax2,0:rmax2,0:rmax2)
        complex*16 Buv0(0:2*rmax2,0:rmax2,0:rmax2)
        complex*16 B_1(0:2*rmax2,0:rmax2),Buv_1(0:2*rmax2,0:rmax2)
        complex*16 B_2(0:2*rmax2,0:rmax2),Buv_2(0:2*rmax2,0:rmax2)
        complex*16 S(2,0:2*rmax3,0:rmax3)
        complex*16 C(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 Cuv(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 m02,m12,m22,f(2),vers(2),Stest
        real*8 z2(2,2),z2i(2,2)

        call B0_dd(B0,Buv0 ,p12,m12,m22,r2-1,nid(0))
        call B_dd(B_1,Buv_1,p20,m02,m22,r2-1,nid(1))
        call B_dd(B_2,Buv_2,p01,m02,m12,r2-1,nid(2))

c initialization of error propagation for PaVe
        Bij_err = 1d-14
        acc = accr2_aux(id,r1)

        do 100 r=max(1,r1),r2

c PaVe reduction of C_{00...} of rank r -- Eq.(5.10)
          do 102 i0=2,r,2
          i12 = r-i0
          do 102 i1=0,i12
            i2 = i12-i1
            Cuv(i0,i1,i2) = 1d0/2d0/(i0+i12)*( 
     &              Buv0(i0-2,i1,i2) + 2d0*m02*Cuv(i0-2,i1,i2) 
     &            + f(1)*Cuv(i0-2,i1+1,i2) + f(2)*Cuv(i0-2,i1,i2+1) )
            C(i0,i1,i2)   = 1d0/2d0/(i0+i12)*( 4d0*Cuv(i0,i1,i2)
     &            + B0(i0-2,i1,i2) + 2d0*m02*C(i0-2,i1,i2) 
     &            + f(1)*C(i0-2,i1+1,i2) + f(2)*C(i0-2,i1,i2+1) )
102       continue

c PaVe reduction of C(0,...) -- Eq.(5.11)
c NOTE: C(0,...) are UV finite !
        do 103 i1=0,r-1
          i2 = r-1-i1
          do k=1,2
            S(k,i1,i2) = -B0(0,i1,i2) - f(k)*C(0,i1,i2)
          enddo
          if (i1.eq.0) S(1,i1,i2) = S(1,i1,i2) + B_1(0,i2)
          if (i2.eq.0) S(2,i1,i2) = S(2,i1,i2) + B_2(0,i1)
103     continue

        do 104 i1=0,r-1
          i(1) = i1
          i(2) = r-i(1)-1
          do 104 k=2,1,-1
            j(1) = i(1)+del(k,1)
            j(2) = i(2)+del(k,2)
            C(0,j(1),j(2))   = 0d0
            Cuv(0,j(1),j(2)) = 0d0
            do n=1,2
              l(1) = i(1)-del(n,1)
              l(2) = i(2)-del(n,2)
              C(0,j(1),j(2))   = C(0,j(1),j(2)) 
     &                           + z2i(k,n)*S(n,i(1),i(2))
              if (i(n).ne.0) then
                C(0,j(1),j(2))   = C(0,j(1),j(2)) 
     &                             -2d0*z2i(k,n)*i(n)*C(2,l(1),l(2))
              endif
            enddo    
          if ((j(1).ne.0).and.(j(2).ne.0)) then
            vers(k) = C(0,j(1),j(2))
            if (k.eq.2) then
              acc1 = abs((vers(1)-vers(2))/(vers(1)+vers(2))*2d0)
              acc = max(acc,acc1)
            endif
          endif
104       continue

c consistency check via Eq.(5.8)
        do 200 k=1,2
        do 200 i2=1,2
          Stest = 0d0
          do 201 m=1,2
            i(1) = (r-1)*del(i2,1)+del(m,1)
            i(2) = (r-1)*del(i2,2)+del(m,2)
            Stest = Stest + z2(k,m)*C(0,i(1),i(2))
201       continue
            i(1) = (r-2)*del(i2,1)
            i(2) = (r-2)*del(i2,2)
            if (r.gt.1) 
     &         Stest = Stest + del(k,i2)*2d0*(r-1)*C(2,i(1),i(2))
            if ((Stest.ne.(0d0,0d0)).and.
     &          (S(k,i(1)+del(i2,1),i(2)+del(i2,2)).ne.(0d0,0d0))) 
     &        acc = max(acc, abs(S(k,i(1)+del(i2,1),i(2)+del(i2,2))
     &                         /Stest-1d0) )
200     continue

c consistency check via Eq.(5.9)
        if (r.ge.2) then
        do 210 i3=1,2
          Stest = 0d0
          do 211 m=1,2
          do 211 n=1,2
            j(1) = (r-2)*del(i3,1)
            j(2) = (r-2)*del(i3,2)
            i(1) = j(1)+del(m,1)+del(n,1)
            i(2) = j(2)+del(m,2)+del(n,2)
            Stest = Stest + z2(m,n)*C(0,i(1),i(2))
211       continue
          Stest = Stest + 4d0*r*C(2,j(1),j(2)) - 4d0*Cuv(2,j(1),j(2))
          if ((Stest.ne.(0d0,0d0)).and.
     &        (B0(0,j(1),j(2))+m02*C(0,j(1),j(2)).ne.(0d0,0d0))) 
     &      acc = max(acc, abs(2d0*(B0(0,j(1),j(2))+m02*C(0,j(1),j(2)))
     &                       /Stest-1d0) )
210     continue
        endif

c error propagation for PaVe
        if (r.ge.2) then
          C00_err(id,r) = max(Bij_err,am02(id)*Cij_err(id,r-2),
     &      maxzif(id)*Bij_err,ziff(id)*Cij_err(id,r-2),
     &      maxzif(id)*C00_err(id,r-1))
          acc = max(acc,C00_err(id,r)/abs(C(2,0,0)))
        else
          C00_err(id,1) = 0d0
        endif
        Cij_err(id,r)  = max(maxzi(id)*Bij_err,
     &    maxzif(id)*Cij_err(id,r-1),maxzi(id)*C00_err(id,r))
        Cij_err2(id,r) = max(maxzi(id)*Bij_err,
     &    maxzif(id)*Cij_err2(id,r-1),maxzi(id)*C00_err(id,r))
     &    / sqrt(maxz(id)*maxzi(id))

c find typical value for |C(0,...)| of rank r
c        Ctyp = log(abs(C(0,0,0)))
c        count = 1
c        do 300 i1=1,r
c          count = count+2
c          Ctyp = Ctyp + log(abs(C(0,i1,0)))
c     &                + log(abs(C(0,0,i1)))
c
c300     continue
c        Ctyp = exp(Ctyp/count)
c        acc = max(acc,Cij_err(id,r)/Ctyp)

c find maximal value for |C(0,...)| of rank r
        count = 1
        Cmax = 0d0
        i0=0
        i12 = r-i0
        do i1=0,i12
          i2=i12-i1
          Cmax = max(Cmax,abs(C(i0,i1,i2)))
        enddo
        acc = max(acc,Cij_err(id,r)/Cmax)
        Cij_err(id,r) = acc*Cmax

        accr2_aux(id,r) = acc

100     continue

c PaVe reduction of C_{00...} of rank r=r2+1, ... ,r2+r2 -- Eq.(5.10)
          do 105 r=r2+1,2*r2
          do 105 i0=2*(r-r2),r,2
          i12 = r-i0
          do 105 i1=0,i12
            i2 = i12-i1
            Cuv(i0,i1,i2) = 1d0/2d0/(i0+i12)*( 
     &              Buv0(i0-2,i1,i2) + 2d0*m02*Cuv(i0-2,i1,i2) 
     &            + f(1)*Cuv(i0-2,i1+1,i2) + f(2)*Cuv(i0-2,i1,i2+1) )
            C(i0,i1,i2)   = 1d0/2d0/(i0+i12)*( 4d0*Cuv(i0,i1,i2)
     &            + B0(i0-2,i1,i2) + 2d0*m02*C(i0-2,i1,i2) 
     &            + f(1)*C(i0-2,i1+1,i2) + f(2)*C(i0-2,i1,i2+1) )
105       continue

        end

**********************************************************************
        subroutine Calpave_dd(C,Cuv,
     &              p01,p12,p20,m02,m12,m22,
     &              acc,detz2,f,detx2,tx2,r1,r2,id,nid)
**********************************************************************
*       Alternative Passarino-Veltman reduction
*
*       3-point coefficients  
*       C(i,j,k) = C_{0...01...12...2}(p01,p12,p20,m02,m12,m22)
*                     \___/\___/\___/
*                       i    j    k   indices
*       of rank r=i+j+k with r1 <= r <= r2
*       coefficients with rank r < r1 assumed to be known
*
*       Cuv(i,j,k) = coefficient of 1/eps in C(i,j,k),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       10.2.2009 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        integer r,r1,r2,id,nid(0:nmax-1),count
        integer del(2,2),i(2),j(2),k,l(2),n,i0,i1,i2,i12
        data del/1,0,0,1/
        complex*16 B0(0:2*rmax2,0:rmax2,0:rmax2)
        complex*16 Buv0(0:2*rmax2,0:rmax2,0:rmax2)
        complex*16 B_1(0:2*rmax2,0:rmax2),Buv_1(0:2*rmax2,0:rmax2)
        complex*16 B_2(0:2*rmax2,0:rmax2),Buv_2(0:2*rmax2,0:rmax2)
        complex*16 Sh(2,0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 C(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 Cuv(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 m02,m12,m22,f(2),vers(2)
        complex*16 detx2,tx2(0:2,0:2)

c initialization of error propagation for alternative PaVe
        do r=r1,rmax3
          Cij_err_newprelim(id,r) = 0d0
          C00_err_newprelim(id,r) = 0d0
        enddo
        acc = 0d0
        accr2_newprelim(id,0) = 1d-14
        Bij_err = 1d-14
        Cij_err_newprelim(id,0)   = 1d-14*max( 1d0/sqrt(abs(detz2)),
     &                              abs(scalint(id)) )
        maxtx20n = max(abs(tx2(0,1)),abs(tx2(0,2)))
        maxtx2kn = max(abs(tx2(1,1)),abs(tx2(1,2)),abs(tx2(2,2)))

        call B0_dd(B0,Buv0 ,p12,m12,m22,r2-1,nid(0))
        call B_dd(B_1,Buv_1,p20,m02,m22,r2-1,nid(1))
        call B_dd(B_2,Buv_2,p01,m02,m12,r2-1,nid(2))

c alternative PaVe reduction
        do 100 r=max(1,r1),r2

        do 104 i0=0,r-1,2
        do 104 i1=0,r-1-i0
          i2 = r-1-i0-i1
          do k=1,2
            Sh(k,i0,i1,i2) = -B0(i0,i1,i2) 
          enddo
          if (i1.eq.0) Sh(1,i0,i1,i2) = Sh(1,i0,i1,i2) + B_1(i0,i2)
          if (i2.eq.0) Sh(2,i0,i1,i2) = Sh(2,i0,i1,i2) + B_2(i0,i1)
104     continue

c reduction of C_{00...}
        do 105 i0=0,r-1,2
        do 105 i1=0,r-1-i0
        i2   = r-1-i0-i1
        i(1) = i1
        i(2) = i2
c Cuv_{00...} from conventional PaVe reduction -- Eq.(5.10)
          Cuv(i0+2,i1,i2) = 1d0/dfloat(2*r+2)*( 
     &            Buv0(i0,i1,i2) + 2d0*m02*Cuv(i0,i1,i2) 
     &          + f(1)*Cuv(i0,i1+1,i2) + f(2)*Cuv(i0,i1,i2+1) )

c alternative PaVe reduction of C_{00...} -- Eq.(5.16)
          C(i0+2,i1,i2) = 1d0/dfloat(2*r+2)*( 
     &            4d0*Cuv(i0+2,i1,i2) + B0(i0,i1,i2) 
     &          + detx2/detz2*C(i0,i1,i2) )
          do n=1,2
            l(1) = i1-del(n,1)
            l(2) = i2-del(n,2)
            C(i0+2,i1,i2) = C(i0+2,i1,i2) 
     &           - tx2(0,n)/detz2/dfloat(2*r+2)*Sh(n,i0,i1,i2)
            if (i(n).gt.0) then
              C(i0+2,i1,i2) = C(i0+2,i1,i2)
     &           + tx2(0,n)/detz2*i(n)/dfloat(r+1)*C(i0+2,l(1),l(2))
            endif
          enddo
105     continue

c*** Cayley reduction of all C(0,i1,i2) with i1+i2 > 0 -- Eq.(5.15)
c       compare two versions of C(0,i1,i2):  
c                       C(0,i1+1,i2) vs. C(0,j1,j2+1) 
c       and deduce error estimate from difference
        do 120 i1=0,r-1
        i(1) = i1
        i(2) = r-1-i1
        do 120 k=2,1,-1
          j(1) = i(1)+del(k,1)
          j(2) = i(2)+del(k,2)
          Cuv(0,j(1),j(2)) = 0d0
          C(0,j(1),j(2))   = tx2(0,k)/detx2*( 
     &          2d0*(r+1)*C(2,i(1),i(2)) -4d0*Cuv(2,i(1),i(2)) 
     &        - B0(0,i(1),i(2)) )

          do n=1,2
            l(1) = i(1)-del(n,1)
            l(2) = i(2)-del(n,2)
            C(0,j(1),j(2)) = C(0,j(1),j(2)) 
     &                  +tx2(k,n)/detx2*Sh(n,0,i(1),i(2))
            if (i(n).ne.0) C(0,j(1),j(2)) = C(0,j(1),j(2)) 
     &                  -2d0*tx2(k,n)/detx2*i(n)*C(2,l(1),l(2))
          enddo
          if ((j(1).ne.0).and.(j(2).ne.0)) then
            vers(k) = C(0,j(1),j(2))
            if (k.eq.2) then
              acc1 = abs((vers(1)-vers(2))/(vers(1)+vers(2))*2d0)
              acc = max(acc,acc1)
            endif
          endif

120     continue

c error propagation for alternative PaVe
        C00_err_newprelim(id,r+1) = max( Bij_err,
     &    abs(detx2/detz2)*Cij_err_newprelim(id,r-1),
     &    maxtx20n/abs(detz2)*max(Bij_err,C00_err_newprelim(id,r)) )
        Cij_err_newprelim(id,r) = max( 
     &    maxtx20n/abs(detx2)*max(C00_err_newprelim(id,r+1),Bij_err),
     &    maxtx2kn/abs(detx2)*max(C00_err_newprelim(id,r),Bij_err) )

c find typical value for |C(0,...)| of rank r
c        Ctyp = 0d0
c        count = 0
c        do 300 i1=0,r
c        i2=r-i1
c          count = count+1
c          Ctyp = Ctyp + log(abs(C(0,i1,i2)))
c300     continue        
c        Ctyp = exp(Ctyp/count)
c        acc = max(acc,Cij_err_newprelim(id,r)/Ctyp)

c find maximal value for |C(0,...)| of rank r
        count = 0
        Cmax = 0d0
        i0=0
        i12 = r-i0
        do i1=0,i12
          i2=i12-i1
          Cmax = max(Cmax,abs(C(i0,i1,i2)))
        enddo
        acc = max(acc,Cij_err_newprelim(id,r)/Cmax)
        Cij_err_newprelim(id,r) = acc*Cmax

        accr2_newprelim(id,r) = acc

100     continue

c PaVe reduction of C_{00...} of rank r=r2+1, ... ,r2+r2 -- Eq.(5.10)
          do 400 r=r2+1,r2+r2
          do 400 i0=2*(r-r2),r,2
          i12 = r-i0
          do 400 i1=0,i12
            i2 = i12-i1
            Cuv(i0,i1,i2) = 1d0/2d0/(i0+i12)*( 
     &              Buv0(i0-2,i1,i2) + 2d0*m02*Cuv(i0-2,i1,i2) 
     &            + f(1)*Cuv(i0-2,i1+1,i2) + f(2)*Cuv(i0-2,i1,i2+1) )
            C(i0,i1,i2)   = 1d0/2d0/(i0+i12)*( 4d0*Cuv(i0,i1,i2)
     &            + B0(i0-2,i1,i2) + 2d0*m02*C(i0-2,i1,i2) 
     &            + f(1)*C(i0-2,i1+1,i2) + f(2)*C(i0-2,i1,i2+1) )
400       continue

        end

**********************************************************************
        subroutine Ccayley_dd(C,Cuv,
     &              p01,p12,p20,m02,m12,m22,acc,
     &              detx2,tx2,f,r2,id,nid)
**********************************************************************
*       Cayley reduction 
*       with numerical evaluation of C_{0...0} for r2 = n/2
*                                       --n--
*
*       3-point coefficients  
*       C(i,j,k) = C_{0...01...12...2}(p01,p12,p20,m02,m12,m22)
*                     \___/\___/\___/
*                       i    j    k   indices
*       of rank r=i+j+k with r <= r2
*
*       Cuv(i,j,k) = coefficient of 1/eps in C(i,j,k),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       17.6.2006 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        integer r,r2,i(2),j(2),k,l(2),n,i0,i1,i2,del(2,2),i12,numfun
        integer id,nid(0:nmax-1),count,pre
        data del/1,0,0,1/
        complex*16 B0(0:2*rmax2,0:rmax2,0:rmax2)
        complex*16 Buv0(0:2*rmax2,0:rmax2,0:rmax2)
        complex*16 B_1(0:2*rmax2,0:rmax2),Buv_1(0:2*rmax2,0:rmax2)
        complex*16 B_2(0:2*rmax2,0:rmax2),Buv_2(0:2*rmax2,0:rmax2)
        complex*16 S(2,0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 Cnum(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 Cnum2(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 C(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 Cuv(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 m02,m12,m22,f(2),detx2
        complex*16 tx2(0:2,0:2),vers(2)
        real*8 Cn0err(0:rmax3),Cn0err2(0:rmax3)

        do r=0,rmax3
          Cij_err_newprelim(id,r) = 0d0
          C00_err_newprelim(id,r) = 0d0
        enddo

c*** subintegrals
        call B0_dd(B0,Buv0, p12,m12,m22,r2-1,nid(0))
        call B_dd(B_1,Buv_1,p20,m02,m22,r2-1,nid(1))
        call B_dd(B_2,Buv_2,p01,m02,m12,r2-1,nid(2))

c PaVe reduction of C_{00...} -- Eq.(5.10)
        do 50 r=0,r2
          accr2_newprelim(id,r) = 0d0
        do 50 i0=0,2*r,2
        do 50 i1=0,r-i0/2
        i2 = r-i0/2-i1
          if (i0.eq.0) then
            Cuv(i0,i1,i2) = 0d0
          else
            Cuv(i0,i1,i2) = 1d0/2d0/(i0+i1+i2)*( 
     &              Buv0(i0-2,i1,i2) + 2d0*m02*Cuv(i0-2,i1,i2) 
     &            + f(1)*Cuv(i0-2,i1+1,i2) + f(2)*Cuv(i0-2,i1,i2+1) )
          endif
50      continue

c*** numerical evaluation of C(*,0,0)
        numfun = max(2,2*r2)
c    Note: Cuv_{00...ij} not calculated numerically, but stable !
        call yCn0_dd(Cnum,Cuv,Cn0err,p01,p12,p20,m02,m12,m22,numfun)

        do 100 r=0,2*(r2-1)

        do 103 i0=0,r,2
        do 103 i1=0,r-i0
          i2 = r-i0-i1
          do k=1,2
            S(k,i0,i1,i2)  = -B0(i0,i1,i2)  
          enddo
          if (i1.eq.0) S(1,i0,i1,i2) = S(1,i0,i1,i2)+B_1(i0,i2)
          if (i2.eq.0) S(2,i0,i1,i2) = S(2,i0,i1,i2)+B_2(i0,i1)
103     continue

100     continue

c*** Cayley reduction of all C(i0,0,0) -- Eq.(5.14)
        Cnum2(2*r2,0,0)    = Cnum(2*r2,0,0)
        Cn0err2(2*r2) = Cn0err(2*r2)
        do 104 i0=2*(r2-1),0,-2
            Cnum2(i0,0,0) = tx2(0,0)/detx2*( 
     &         2d0*(i0+2)*Cnum2(2+i0,0,0)-4d0*Cuv(2+i0,0,0)-B0(i0,0,0) )
     &       + tx2(0,1)/detx2*S(1,i0,0,0)+ tx2(0,2)/detx2*S(2,i0,0,0)
            Cn0err2(i0) = abs(tx2(0,0)/detx2)*Cn0err2(2+i0)
104       continue

c*** C0 from numerically calculated C_00
          Cnum(0,0,0) = tx2(0,0)/detx2*( 
     &             4d0*Cnum(2,0,0)-4d0*Cuv(2,0,0)-B0(0,0,0) )
     &       + tx2(0,1)/detx2*S(1,0,0,0)+ tx2(0,2)/detx2*S(2,0,0,0)

c*** compare C0 of the to versions with direct calculation and take
c    C(i0,0,0) from better version
        acc1 = abs(Cnum (0,0,0)/scalint(id)-1d0)
        acc2 = abs(Cnum2(0,0,0)/scalint(id)-1d0)
        if (acc1.lt.acc2) then
          do 110 i0=0,2*r2,2
            C(i0,0,0) = Cnum(i0,0,0) 
110       continue
          acc = acc1
        else
          do 111 i0=0,2*r2,2
            C(i0,0,0)  = Cnum2(i0,0,0) 
            Cn0err(i0) = Cn0err2(i0)
111       continue
          acc = acc2
        endif
        Cij_err_newprelim(id,0) = acc*abs(C(0,0,0))
        C00_err_newprelim(id,2) = Cn0err(2) 

c*** Cayley reduction of all C(i0,i1,i2) with i1+i2 > 0 -- Eq.(5.15)
c       compare two versions of C(i0,i1,i2):  C(i0,i1+1,i2) vs. C(i0,j1,j2+1) 
c       and deduce error estimate from difference
        do 120 i0=2*r2-2,0,-2
        do 120 i12=0,r2-i0/2-1
        do 120 i1=0,i12
        i(1) = i1
        i(2) = i12-i1
        do 120 k=2,1,-1
          j(1) = i(1)+del(k,1)
          j(2) = i(2)+del(k,2)
          if (i0.eq.0) Cuv(i0,j(1),j(2)) = 0d0
          C(i0,j(1),j(2)) = tx2(0,k)/detx2*( 
     &          2d0*(2+i0+i12)*C(i0+2,i(1),i(2))-4d0*Cuv(i0+2,i(1),i(2))
     &          -B0(i0,i(1),i(2)) )
          do n=1,2
            l(1) = i(1)-del(n,1)
            l(2) = i(2)-del(n,2)
            C(i0,j(1),j(2)) = C(i0,j(1),j(2)) 
     &                           + tx2(k,n)/detx2*S(n,i0,i(1),i(2))
            if (i(n).ne.0) C(i0,j(1),j(2)) = C(i0,j(1),j(2)) 
     &                        -2d0*tx2(k,n)/detx2*i(n)*C(i0+2,l(1),l(2))
          enddo
          if ((j(1).ne.0).and.(j(2).ne.0)) then
            vers(k) = C(i0,j(1),j(2))
            if (k.eq.2) then
              acc1 = abs((vers(1)-vers(2))/(vers(1)+vers(2))*2d0)
              acc = max(acc,acc1)
              if (i0.eq.0) then
                Cij_err_newprelim(id,i0+i12+1)=acc1*abs(C(i0,j(1),j(2)))
              elseif (i0.eq.2) then
                C00_err_newprelim(id,i0+i12+1)=acc1*abs(C(i0,j(1),j(2)))
              endif
            endif
          endif
120     continue

c estimated propagation of numerical error
        maxtx2 = 0d0
        do n=0,2
        do k=max(1,n),2
          maxtx2 = max(maxtx2,abs(tx2(k,n)))
        enddo
        enddo
        Bij_err = 1d-14 
        Cij_err_newprelim(id,1) = 0d0
        do 200 r=1,r2
          pre = 2**r
          do n=1,r
            pre = pre*(1+n+r)
          enddo
          Cij_err_newprelim(id,r) = max(Cij_err_newprelim(id,r),
     &                         maxtx2/abs(detx2)*Bij_err,
     &                         pre*(maxtx2/abs(detx2))**r*Cn0err(2*r))

c find typical value for |C(0,...)| of rank r
c          Ctyp = 0d0
c          count = 0
c          do 201 i1=0,r
c          i2=r-i1
c            count = count+1
c            Ctyp = Ctyp + log(abs(C(0,i1,i2)))
c201       continue
c          Ctyp = exp(Ctyp/count)
c          acc = max(acc,Cij_err_newprelim(id,r)/Ctyp)

c find maximal value for |C(0,...)| of rank r
        count = 0
        Cmax = 0d0
        i0=0
        i12 = r-i0
        do i1=0,i12
          i2=i12-i1
          Cmax = max(Cmax,abs(C(i0,i1,i2)))
        enddo
        acc = max(acc,Cij_err_newprelim(id,r)/Cmax)

200     continue

        end

************************************************************************
        subroutine yCn0_dd(C,Cuv,Cn0err,P01,P12,P20,M02,M12,M22,
     &                     numfun)
************************************************************************
*       C0...0 with complex mass -- numerical evaluation with dcuhre
*        --n--
*-----------------------------------------------------------------------
*       17.12.04 Stefan Dittmaier
************************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        complex*16 M02,M12,M22,m2(0:2)
        complex*16 C(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 Cuv(0:2*rmax3,0:rmax3,0:rmax3)
        real*8 k(0:2,0:2),Cn0err(0:rmax3)
        integer i,j,n

c dcuhre input parameters:
        integer NDIM             ! integration dimension
        parameter (ndim = 2)
        integer NUMFUN           ! # of components in integrand
        real*8  A(NDIM), B(NDIM) ! lower/upper limits of integration
        integer MINPTS,MAXPTS    ! Minimum/maximum # of function evaluations
        real*8  EPSABS,EPSREL    ! Requested absolute/relative error
        integer KEY    ! Key to selected local integration rule.
                       ! KEY = 0 is the default value.
                       ! For NDIM = 2 the degree 13 rule is selected.
                       ! For NDIM = 3 the degree 11 rule is selected.
                       ! For NDIM > 3 the degree  9 rule is selected.
                       ! KEY = 1 gives the user the 2 dimensional degree 13
                       ! integration rule that uses 65 evaluation points.
                       ! KEY = 2 gives the user the 3 dimensional degree 11
                       ! integration rule that uses 127 evaluation points.
                       ! KEY = 3 gives the user the degree 9 integration rule.
                       ! KEY = 4 gives the user the degree 7 integration rule.
                       ! This is the recommended rule for problems that
                       ! require great adaptivity.
        integer NUM    ! # of evaluation points = 65(127) for ndim=2(3)
        integer NW     ! length of the working array WORK, set to
                       ! ((MAXPTS-NUM)/(2*NUM)+1)
                       ! *(2*NDIM+2*NUMFUN+2)+17*NUMFUN+1
        integer RESTAR ! If RESTAR = 0, first attempt to compute the integral
                       ! If RESTAR = 1, restart a previous attempt
c dcuhre output variables:
        real*8 RESULT(NUMFUN) ! Approximations to all components of the integral
        real*8 ABSERR(NUMFUN) ! Estimates of absolute errors
        integer NEVAL         ! # of function evaluations used by DCUHRE
        integer IFAIL         ! exit flag                 -> see file dcuhre.f
c       real*8 WORK(NW)       ! information on subregions -> see file dcuhre.f
c                             ! output suppressed in this version

c dcuhre integrand
        external Cn0_dd   ! Externally declared subroutine for computing
                          ! all components of the integrand in the given
                          ! evaluation point.
                          ! It must have parameters (NDIM,X,NUMFUN,FUNVLS)
                          ! Input parameters:
                          !   NDIM   Integer: dimension of the integral
                          !   X      Real array of dimension NDIM
                          !          that defines the evaluation point
                          !   NUMFUN Integer that defines the # of components
                          !   Output parameter:
                          !   FUNVLS Real array of dimension NUMFUN
                          !   that defines NUMFUN components of the integrand

        common/DD_Cargs/m2,k,scale

        m2(0)  = m02
        m2(1)  = m12
        m2(2)  = m22
        k(0,1) = p01
        k(1,2) = p12
        k(0,2) = p20
        scale = 0d0
        do i=0,2
          scale  = scale + abs(m2(i))
        do j=0,i-1
          scale  = scale + abs(k(j,i))
        enddo
        enddo
        scale=scale/6d0

c input for dcuhre
        do i=1,ndim
          A(i) = 0d0
          B(i) = 1d0
        enddo
        MINPTS = 50000    ! 5000 was ok for ee->4f
        MAXPTS = MINPTS
        EPSREL = 1d-50
        EPSABS = 0d0
        KEY    = 0
        NUM    = 65
        NW     = ((MAXPTS-NUM)/(2*NUM) + 1)
     &           *(2*NDIM+2*NUMFUN+2) + 17*NUMFUN + 1
        RESTAR = 0

c call integration with dcuhre
        CALL DCUHRE(NDIM,NUMFUN,A,B,MINPTS,MAXPTS,Cn0_dd,EPSABS,
     &              EPSREL,KEY,NW,RESTAR,RESULT,ABSERR,NEVAL,IFAIL)

        const = 0d0
        do n=1,numfun/2
        C(2*n,0,0) = dcmplx( result(2*n-1), result(2*n) )
     &       + Cuv(2*n,0,0)*(const+deltauv-log(scale/muv2))
        const = const + 1d0/n
        Cn0err(2*n+2) = max( abs(abserr(2*n-1)), abs(abserr(2*n)) )
        enddo

        end

************************************************************************
        subroutine Cn0_dd(ndim,x,numfun,f)
************************************************************************
*       interface for integration with dcuhre
************************************************************************
        implicit real*8 (a-z)
        complex*16 m2(0:2),ieps,fc(50),aux
        parameter(ieps=(0d0,1d-20))
        real*8 y(0:2),k(0:2,0:2)
        integer ndim,numfun,n
        real*8 x(ndim),f(numfun)

        common/DD_Cargs/m2,k,scale

        y(1) = x(1)
        y(2) = (1d0-y(1))*x(2)
        y(0) = 1d0-y(1)-y(2)
        jac  = 1d0-y(1)

        aux = y(0)*m2(0)+y(1)*m2(1)+y(2)*m2(2)
     &         -k(1,2)*y(1)*y(2)-k(0,1)*y(0)*y(1)-k(0,2)*y(0)*y(2)

c fc(n) = C(2n,0,0,0)
        fc(1) = -log(aux/scale-ieps)*jac/2d0
        do n=2,numfun/2
          fc(n) = fc(n-1)/2d0/(n-1)*aux 
        enddo
        do n=1,numfun/2
          f(2*n-1) = dreal(fc(n))
          f(2*n)   = dimag(fc(n))
        enddo

        end

**********************************************************************
        subroutine Cgram_dd(C,Cuv,
     &              p01,p12,p20,m02,m12,m22,j,k,l,acc,
     &              detz2,tz2,ttz2,tx2,f,r1,r2,r2eff,id,nid)
**********************************************************************
*       Expansion for small Gram determinant
*
*       3-point coefficients  
*       C(i,j,k) = C_{0...01...12...2}(p01,p12,p20,m02,m12,m22)
*                     \___/\___/\___/
*                       i    j    k   indices
*       of rank r=i+j+k with r1 <= r <= r2
*       coefficients with rank r < r1 assumed to be known
*
*       Cuv(i,j,k) = coefficient of 1/eps in C(i,j,k),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       17.7.2006 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        integer r,r1,r2,i(2),j,k,l,m,n,rup,rupmax,r2eff
        integer del(2,2),i0,i1,i2,i12,id,nid(0:nmax-1)
        data del/1,0,0,1/
        complex*16 B0(0:2*rmax2,0:rmax2,0:rmax2)
        complex*16 Buv0(0:2*rmax2,0:rmax2,0:rmax2)
        complex*16 B_1(0:2*rmax2,0:rmax2),Buv_1(0:2*rmax2,0:rmax2)
        complex*16 B_2(0:2*rmax2,0:rmax2),Buv_2(0:2*rmax2,0:rmax2)
        complex*16 Cstore(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 C(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 Cuv(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 m02,m12,m22,f(2),tx2(0:2,0:2)
        real*8 tz2(2,2),ttz2(2,2,2,2)

        rupmax = rmax3-1

c maximal iteration already reached in previous call
        if ((r1.gt.0).and.(r2_new_aux(id).ge.rupmax)) then
          r2eff = r2_new_aux(id)
          acc = accr2_newprelim(id,r2eff) 
          return
        endif

c start iteration
c----------------
        do r=0,rmax3
          Cij_err_newprelim(id,r) = 0d0
          C00_err_newprelim(id,r) = 0d0
        enddo
c       rup = maximal rank calculated for C(0,i1,i2)
        if (r1.eq.0) then
          rup = -1
          Cuv(0,0,0) = 0d0
        else
          rup = r2_new_aux(id)
        endif
        accprev1 = 1d30
        accprev2 = 1d30
500     continue
        rup = rup+1

c calculate new B integrals
        call B0_dd(B0,Buv0, p12,m12,m22,rup,nid(0))
        call B_dd(B_1,Buv_1,p20,m02,m22,rup,nid(1))
        call B_dd(B_2,Buv_2,p01,m02,m12,rup,nid(2))

c initialize higher-rank tensors
        do i0=0,rup+1
        do i1=0,rup+1-i0
        i2 = rup+1-i0-i1
          C(i0,i1,i2) = 0d0
        enddo
        enddo

c S functions
        do i0=0,rup
        do i1=0,rup-i0
        i2 = rup-i0-i1
          do n=1,2
            Sh3_aux(id,n,i0,i1,i2) = -B0(i0,i1,i2) 
          enddo
          if (i1.eq.0) Sh3_aux(id,1,i0,i1,i2) = Sh3_aux(id,1,i0,i1,i2) 
     &                                          + B_1(i0,i2)
          if (i2.eq.0) Sh3_aux(id,2,i0,i1,i2) = Sh3_aux(id,2,i0,i1,i2) 
     &                                          + B_2(i0,i1)
        enddo
        enddo

c Cuv_{00...} from PaVe reduction -- Eq.(5.10)
        do i0=2*((rup+1)/2),2,-2
        do i1=0,rup+1-i0
        i2 = rup+1-i0-i1
          Cuv(i0,i1,i2) = 1d0/2d0/(rup+1d0)*( 
     &              Buv0(i0-2,i1,i2) + 2d0*m02*Cuv(i0-2,i1,i2) 
     &            + f(1)*Cuv(i0-2,i1+1,i2) + f(2)*Cuv(i0-2,i1,i2+1) )
        enddo
        enddo

c store previous approximation for error estimate
        if (rup.ge.r2) then
          do n=0,rup-1
            Cstore(0,n,0) = C(0,n,0) 
            Cstore(0,0,n) = C(0,0,n) 
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
          C(i0,i1,i2) = 1d0/2d0/(1d0+i0+2*i1+2*i2)*(
     &      4d0*Cuv(i0,i1,i2) 
     &      - detz2/tz2(k,l)
     &          *C(i0-2,i1+del(k,1)+del(l,1),i2+del(k,2)+del(l,2))
     &      + 2d0*B0(i0-2,i1,i2)+2d0*m02*C(i0-2,i1,i2)
     &      + tz2(1,l)/tz2(k,l)
     &          *Sh3_aux(id,1,i0-2,i1+del(k,1),i2+del(k,2))
     &      + tz2(2,l)/tz2(k,l)
     &          *Sh3_aux(id,2,i0-2,i1+del(k,1),i2+del(k,2))
     &      - Sh3_aux(id,1,i0-2,i1+1,i2) - Sh3_aux(id,2,i0-2,i1,i2+1)
     &      - ttz2(k,n,l,m)/tz2(k,l)*f(n)
     &          *(Sh3_aux(id,m,i0-2,i1,i2)-f(m)*C(i0-2,i1,i2))       ) 
          if (i(n).ne.0) C(i0,i1,i2) = C(i0,i1,i2) 
     &      - i(n)/(1d0+i0+2*i1+2*i2)*ttz2(k,n,l,m)/tz2(k,l)
     &          *(Sh3_aux(id,m,i0,i1-del(1,n),i2-del(2,n))
     &            -f(m)*C(i0,i1-del(1,n),i2-del(2,n)))
          if (i(m).ne.0) C(i0,i1,i2) = C(i0,i1,i2) 
     &      + i(m)/(1d0+i0+2*i1+2*i2)*ttz2(k,n,l,m)/tz2(k,l)
     &          *f(n)*C(i0,i1-del(1,m),i2-del(2,m))
          if (i(n)*i(m).ne.0) then
            if (n.ne.m) then
              C(i0,i1,i2) = C(i0,i1,i2) 
     &          +2d0*i(n)*i(m)/(1d0+i0+2*i1+2*i2)*ttz2(k,n,l,m)/tz2(k,l)
     &            *C(i0+2,i1-del(1,m)-del(1,n),i2-del(2,m)-del(2,n))
            elseif (i(n).gt.1) then
              C(i0,i1,i2) = C(i0,i1,i2) 
     &          + 2d0*i(n)*(i(n)-1)/(1d0+i0+2*i1+2*i2)
     &            *ttz2(k,n,l,m)/tz2(k,l)
     &            *C(i0+2,i1-2*del(1,n),i2-2*del(2,n))
            endif
          endif
102     continue

c coefficients C(0,i1,i2) from Eq.(5.38)
        i0 = 0
        do 103 i1=0,r
        i2 = r-i0-i1
        i(1) = i1
        i(2) = i2
          C(i0,i1,i2) = 0d0
          do 104 n=1,2
            C(i0,i1,i2) = C(i0,i1,i2) - tz2(j,n)*Sh3_aux(id,n,i0,i1,i2) 
            if (i(n).ne.0) C(i0,i1,i2) = C(i0,i1,i2)
     &        + 2d0*i(n)*tz2(j,n)*C(i0+2,i1-del(1,n),i2-del(2,n))
104       continue
          C(i0,i1,i2) = ( C(i0,i1,i2)
     &        + detz2*C(i0,i1+del(1,j),i2+del(2,j)) )/tx2(0,j) 
103     continue

400     continue

c estimate precision from last improvements
        if ((rup.le.r2).and.(rup.lt.rupmax)) then
          acc = 1d30
        else
c         acc1 = abs(C(0,0,0)/scalint(id)-1d0)
          acc1 = 0d0
          do n=0,rup-1
            acc1 = max(acc1,abs(Cstore(0,n,0)/C(0,n,0)-1d0))
            acc1 = max(acc1,abs(Cstore(0,0,n)/C(0,0,n)-1d0))
            accr2_newprelim(id,n) = acc1
          enddo
          acc = accr2_newprelim(id,r2) 
        endif

c error propagation for Gram expansion
          Bij_err = 1d-14 
          do 800 r=rup,0,-1
          if (r.ge.1) then
            C00_err_newprelim(id,r+1) = max( 
     &        abs(detz2)/abs(tz2(k,l))*Cij_err_newprelim(id,r+1),
     &        Bij_err, am02(id)*Cij_err_newprelim(id,r-1),
     &        max( maxttx0klm(id)*Bij_err,
     &             ttzff_kl(id)*Cij_err_newprelim(id,r-1),
     &             maxttx0klm(id)*C00_err_newprelim(id,r) )
     &         /abs(tz2(k,l)) )
          endif
          Cij_err_newprelim(id,r) = 
     &          max( maxtz_nj(id)*max(Bij_err,C00_err_newprelim(id,r+1))
     &            ,abs(detz2)*Cij_err_newprelim(id,r+1) )/abs(tx2(0,j)) 
          accr2_newprelim(id,r) = max(accr2_newprelim(id,r),
     &            Cij_err_newprelim(id,r)/abs(scalint(id)))
800       continue
          do n=0,r2
            acc = max(acc,accr2_newprelim(id,n))
          enddo

        r2eff = rup

c stop if accuracy becomes worse
        if ((rup.ge.r2+3).and.(acc.gt.accprev1).and.
     &      (accprev1.gt.accprev2)) goto 600
        accprev2 = accprev1
        accprev1 = acc

c repeat iteration if necessary
        if ((acc.gt.cacc).and.(rup.lt.rupmax)) goto 500

600     continue

c PaVe reduction of C_{00...} of rank r=rup+1, ... ,2*rup -- Eq.(5.10)
          do 601 r=rup+1,2*rup
          do 601 i0=2*(r-rup),r,2
          i12 = r-i0
          do 601 i1=0,i12
            i2 = i12-i1
            Cuv(i0,i1,i2) = 1d0/2d0/(i0+i12)*( 
     &              Buv0(i0-2,i1,i2) + 2d0*m02*Cuv(i0-2,i1,i2) 
     &            + f(1)*Cuv(i0-2,i1+1,i2) + f(2)*Cuv(i0-2,i1,i2+1) )
            C(i0,i1,i2)   = 1d0/2d0/(i0+i12)*( 4d0*Cuv(i0,i1,i2)
     &            + B0(i0-2,i1,i2) + 2d0*m02*C(i0-2,i1,i2) 
     &            + f(1)*C(i0-2,i1+1,i2) + f(2)*C(i0-2,i1,i2+1) )
601       continue

c final absolute error
        Cmax = 0d0
        i0=0
        do r=0,r2
        i12 = r-i0
        do i1=0,i12
          i2=i12-i1
          Cmax = max(Cmax,abs(C(i0,i1,i2)))
        enddo
        Cij_err_newprelim(id,r) = accr2_newprelim(id,r)*Cmax
        enddo

        end

**********************************************************************
        subroutine Cgram2_dd(C,Cuv,
     &              p01,p12,p20,m02,m12,m22,acc,
     &              detz2,detx2,tx2,f,r1,r2,r2eff,id,nid)
**********************************************************************
*       Expansion for small Gram determinant - version 2
*       Limit: |detZ| << |detX|/M^2,  max{|tX_0j|}    (M = typ. scale)
*
*       3-point coefficients  
*       C(i,j,k) = C_{0...01...12...2}(p01,p12,p20,m02,m12,m22)
*                     \___/\___/\___/
*                       i    j    k   indices
*       of rank r=i+j+k with r1 <= r <= r2
*       coefficients with rank r < r1 assumed to be known
*
*       Cuv(i,j,k) = coefficient of 1/eps in C(i,j,k),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       30.7.2014 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        integer r,r1,r2,i(2),ii(2),n,rup,rupmax,r2eff
        integer del(2,2),i0,i1,i12,i2,id,nid(0:nmax-1)
        data del/1,0,0,1/
        complex*16 B0(0:2*rmax2,0:rmax2,0:rmax2)
        complex*16 Buv0(0:2*rmax2,0:rmax2,0:rmax2)
        complex*16 B_1(0:2*rmax2,0:rmax2),Buv_1(0:2*rmax2,0:rmax2)
        complex*16 B_2(0:2*rmax2,0:rmax2),Buv_2(0:2*rmax2,0:rmax2)
        complex*16 Cstore(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 C(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 Cuv(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 m02,m12,m22,f(2),tx2(0:2,0:2),detx2

        rupmax = rmax3-2

c maximal iteration already reached in previous call
        if ((r1.gt.0).and.(r2_new_aux(id).ge.rupmax)) then
          r2eff = r2_new_aux(id)
          acc = accr2_newprelim(id,r2eff)
          return
        endif

c start iteration
c----------------
        do r=0,rmax3
          Cij_err_newprelim(id,r) = 0d0
          C00_err_newprelim(id,r) = 0d0
        enddo
c       rup = maximal rank calculated for C(0,i1,i2)
        rup = -1
        Cuv(0,0,0) = 0d0
        accprev1 = 1d30
        accprev2 = 1d30
        maxtx20n = max(abs(tx2(0,1)),abs(tx2(0,2)))

500     continue
        rup = rup+1

c calculate new B integrals
        call B0_dd(B0,Buv0, p12,m12,m22,rup,nid(0))
        call B_dd(B_1,Buv_1,p20,m02,m22,rup,nid(1))
        call B_dd(B_2,Buv_2,p01,m02,m12,rup,nid(2))

c initialize higher-rank tensors
        do i0=0,2*rup+2,2
        do i1=0,rup+1-i0/2
        i2   = rup+1-i0/2-i1
          C(i0,i1,i2) = 0d0
        enddo
        enddo

c S functions
        do i0=0,2*rup,2
        do i1=0,rup-i0/2
        i2   = rup-i0/2-i1
          do n=1,2
            Sh3_aux(id,n,i0,i1,i2) = -B0(i0,i1,i2) 
          enddo
          if (i1.eq.0) Sh3_aux(id,1,i0,i1,i2) = Sh3_aux(id,1,i0,i1,i2) 
     &                                          + B_1(i0,i2)
          if (i2.eq.0) Sh3_aux(id,2,i0,i1,i2) = Sh3_aux(id,2,i0,i1,i2) 
     &                                          + B_2(i0,i1)
        enddo
        enddo

c Cuv_{00...} from PaVe reduction -- Eq.(5.10)
        do i0=0,2*rup+2,2
        do i1=0,rup+1-i0/2
        i2 = rup+1-i0/2-i1
          if (i0.lt.2) then
            Cuv(i0,i1,i2) = 0d0
          else
            Cuv(i0,i1,i2) = 1d0/2d0/(i0+i1+i2)*( 
     &              Buv0(i0-2,i1,i2) + 2d0*m02*Cuv(i0-2,i1,i2) 
     &            + f(1)*Cuv(i0-2,i1+1,i2) + f(2)*Cuv(i0-2,i1,i2+1) )
          endif
        enddo
        enddo

c store previous approximation for error estimate
        if (rup.ge.r2) then
          do n=0,rup-1
            Cstore(0,n,0) = C(0,n,0) 
            Cstore(0,0,n) = C(0,0,n) 
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
          C(i0,i1,i2) = detz2/detx2*(
     &                      2*(i0+i1+i2+2)*C(i0+2,i1,i2)
     &                      - 4d0*Cuv(i0+2,i1,i2) - B0(i0,i1,i2) )
          do n=1,2
            ii(1) = i1-del(n,1)
            ii(2) = i2-del(n,2)
            C(i0,i1,i2) = C(i0,i1,i2)
     &           + tx2(0,n)/detx2*Sh3_aux(id,n,i0,i1,i2)
            if (i(n).gt.0) then
              C(i0,i1,i2) = C(i0,i1,i2)
     &           - 2d0*tx2(0,n)/detx2*i(n)*C(i0+2,ii(1),ii(2))
            endif
          enddo
105     continue

400     continue

c estimate precision from last improvements
        if ((rup.le.r2).and.(rup.lt.rupmax)) then
          acc = 1d30
        else
c         acc1 = abs(C(0,0,0)/scalint(id)-1d0)
          acc1 = 0d0
          do n=0,rup-1
            acc1 = max(acc1,abs(Cstore(0,n,0)/C(0,n,0)-1d0))
            acc1 = max(acc1,abs(Cstore(0,0,n)/C(0,0,n)-1d0))
            accr2_newprelim(id,n) = acc1
          enddo
          acc = accr2_newprelim(id,r2)
        endif

c error propagation for Gram expansion
        scale2 = ( abs(p01)+abs(p12)+abs(p20)
     &            +abs(m02)+abs(m12)+abs(m22) )/6d0
        Bij_err = 1d-14 
        B00_err = 1d-14 * scale2

        do 800 r=rup,0,-1
          C00_err_newprelim(id,r) =
     &             abs(detz2/detx2)*B00_err
     &           + maxtx20n/abs(detx2)*B00_err
          Cij_err_newprelim(id,r) =
     &             abs(detz2/detx2)*(C00_err_newprelim(id,r+2)+Bij_err )
     &           + maxtx20n/abs(detx2)
     &               *(Bij_err+C00_err_newprelim(id,r+1))
          accr2_newprelim(id,r) = max(accr2_newprelim(id,r),
     &               Cij_err_newprelim(id,r)/abs(scalint(id)))
800       continue
          do n=0,r2
            acc = max(acc,accr2_newprelim(id,n))
          enddo

        r2eff = rup

c stop if accuracy becomes worse
        if ((rup.ge.r2+3).and.(acc.gt.accprev1).and.
     &      (accprev1.gt.accprev2)) goto 999
        accprev2 = accprev1
        accprev1 = acc

c repeat iteration if necessary
        if ((acc.gt.cacc).and.(rup.lt.rupmax)) goto 500

999     continue
c final absolute error
        Cmax = 0d0
        i0=0
        do r=0,r2
        i12 = r-i0
        do i1=0,i12
          i2=i12-i1
          Cmax = max(Cmax,abs(C(i0,i1,i2)))
        enddo
        Cij_err_newprelim(id,r) = accr2_newprelim(id,r)*Cmax
        enddo

        end

**********************************************************************
        subroutine Cgramcayley_dd(C,Cuv,
     &              p01,p12,p20,m02,m12,m22,i,j,k,l,
     &              acc,detz2,tz2,ttz2,tx2,f,r1,r2,r2eff,id,nid)
**********************************************************************
*       Expansion for small Gram and Cayley determinants
*
*       3-point coefficients  
*       C(i,j,k) = C_{0...01...12...2}(p01,p12,p20,m02,m12,m22)
*                     \___/\___/\___/
*                       i    j    k   indices
*       of rank r=i+j+k with r1 <= r <= r2
*       coefficients with rank r < r1 assumed to be known
*
*       Cuv(i,j,k) = coefficient of 1/eps in C(i,j,k),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       19.7.2006 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        integer r,rr,r1,r2,h(2),i,j,k,l,m,n,rup,rupmax,r2eff
        integer del(2,2),i0,i1,i2,i12,il,im
        integer id,nid(0:nmax-1)
        data del/1,0,0,1/
        complex*16 B0(0:2*rmax2,0:rmax2,0:rmax2)
        complex*16 Buv0(0:2*rmax2,0:rmax2,0:rmax2)
        complex*16 B_1(0:2*rmax2,0:rmax2),Buv_1(0:2*rmax2,0:rmax2)
        complex*16 B_2(0:2*rmax2,0:rmax2),Buv_2(0:2*rmax2,0:rmax2)
        complex*16 Cstore(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 C(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 Cuv(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 m02,m12,m22,f(2),tx2(0:2,0:2)
        real*8 tz2(2,2),ttz2(2,2,2,2)

        rupmax = rmax3-4

c maximal iteration already reached in previous call
        if ((r1.gt.0).and.(r2_new_aux(id).ge.rupmax)) then
          r2eff = r2_new_aux(id)
          acc = accr2_newprelim(id,r2eff) 
          return
        endif

c start iteration
c----------------
c       rup+1 = maximal rank calculated for C(0,i1,i2)
        do r=0,rmax3
          Cij_err_newprelim(id,r) = 0d0
          C00_err_newprelim(id,r) = 0d0
        enddo
        if (r1.eq.0) then
          rup = -2
          Cuv(0,0,0) = 0d0
          Cuv(0,1,0) = 0d0
          Cuv(0,0,1) = 0d0
          call B0_dd(B0,Buv0, p12,m12,m22,0,nid(0))
          call B_dd(B_1,Buv_1,p20,m02,m22,0,nid(1))
          call B_dd(B_2,Buv_2,p01,m02,m12,0,nid(2))
          Sh3_aux(id,1,0,0,0) = -B0(0,0,0) + B_1(0,0)
          Sh3_aux(id,2,0,0,0) = -B0(0,0,0) + B_2(0,0)
        else
          rup = r2_new_aux(id)-1
        endif
        accprev1 = 1d30
        accprev2 = 1d30
500     continue
        rup = rup+2

c calculate new B integrals
        call B0_dd(B0,Buv0, p12,m12,m22,rup+2,nid(0))
        call B_dd(B_1,Buv_1,p20,m02,m22,rup+2,nid(1))
        call B_dd(B_2,Buv_2,p01,m02,m12,rup+2,nid(2))

c initialize higher-rank tensors
        do r=rup+2,rup+3
        do i0=0,r
        do i1=0,r-i0
        i2 = r-i0-i1
          C(i0,i1,i2)   = 0d0
          Cuv(i0,i1,i2) = 0d0
        enddo
        enddo
        enddo

c S functions
        do r=rup+1,rup+2
        do i0=0,r
        do i1=0,r-i0
        i2 = r-i0-i1
          do n=1,2
            Sh3_aux(id,n,i0,i1,i2) = -B0(i0,i1,i2) 
          enddo
          if (i1.eq.0) Sh3_aux(id,1,i0,i1,i2) = Sh3_aux(id,1,i0,i1,i2) 
     &                                          + B_1(i0,i2)
          if (i2.eq.0) Sh3_aux(id,2,i0,i1,i2) = Sh3_aux(id,2,i0,i1,i2) 
     &                                          + B_2(i0,i1)
        enddo
        enddo
        enddo

c Cuv_{00...} from PaVe reduction -- Eq.(5.10)
        do r=rup+2,rup+3
        do i0=2,r,2
        do i1=0,r-i0
        i2 = r-i0-i1
          Cuv(i0,i1,i2) = 1d0/2d0/dfloat(r)*( 
     &              Buv0(i0-2,i1,i2) + 2d0*m02*Cuv(i0-2,i1,i2) 
     &            + f(1)*Cuv(i0-2,i1+1,i2) + f(2)*Cuv(i0-2,i1,i2+1) )
        enddo
        enddo
        enddo

c store previous approximation for error estimate
        if (rup.ge.r2) then
          do n=0,rup-1
            Cstore(0,n,0) = C(0,n,0) 
            Cstore(0,0,n) = C(0,0,n) 
          enddo
        endif

c iteration: rup/2,  step: rr/2
        do 400 rr=rup,0,-2
        do 400 r=rr,rr+1

c C_{00...} from iteration Eq.(5.40)
        m = 3-l
        do 102 i0=2,r+2,2
        do 102 il=r+2-i0,0,-1
        im = r+2-il-i0
        h(l) = il
        h(m) = im

c coefficients C_{00...} from Eq.(5.49)
          C(i0,h(1),h(2)) = 1d0/2d0/dfloat(il+1)/tz2(k,l)*(
     &        tz2(k,1)*Sh3_aux(id,1,i0-2,h(1)+del(1,l),h(2)+del(2,l))
     &      + tz2(k,2)*Sh3_aux(id,2,i0-2,h(1)+del(1,l),h(2)+del(2,l))
     &      + tx2(k,0)*C(i0-2,h(1)+del(1,l),h(2)+del(2,l))
     &      - detz2*C(i0-2,h(1)+del(1,l)+del(1,k),
     &                     h(2)+del(2,l)+del(2,k))              )
          if (im.gt.0) C(i0,h(1),h(2)) = C(i0,h(1),h(2)) 
     &      + 1d0/2d0/dfloat(il+1)/tz2(k,l)*(
     &          - 2d0*im*tz2(k,m)*C(i0,h(1)-del(1,m)+del(1,l),
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
          C(i0,i1,i2) = 1d0/tx2(i,j)*(
     &        tz2(i,j)*( 2d0*(2+r)*C(i0+2,i1,i2) 
     &                  - 4d0*Cuv(i0+2,i1,i2) - B0(i0,i1,i2) )
     &      + ttz2(i,n,j,m)*f(n)*Sh3_aux(id,m,i0,i1,i2)
     &      + tx2(0,j)*C(i0,i1+del(1,i),i2+del(2,i))    )
          if (h(m).gt.0) C(i0,i1,i2) = C(i0,i1,i2) 
     &          - 2d0*h(m)*ttz2(i,n,j,m)*f(n)/tx2(i,j)
     &            *C(i0+2,i1-del(1,m),i2-del(2,m))
103     continue

400     continue

c estimate precision from last improvements
        if ((rup.le.r2).and.(rup.lt.rupmax)) then
          acc = 1d30
        else
c         acc1 = abs(C(0,0,0)/scalint(id)-1d0)
          acc1 = 0d0
          do n=0,rup-1
            acc1 = max(acc1,abs(Cstore(0,n,0)/C(0,n,0)-1d0))
            acc1 = max(acc1,abs(Cstore(0,0,n)/C(0,0,n)-1d0))
            accr2_newprelim(id,n) = acc1
          enddo
          accr2_newprelim(id,rup)   = accr2_newprelim(id,rup-1)
          accr2_newprelim(id,rup+1) = accr2_newprelim(id,rup-1)
          acc = accr2_newprelim(id,r2) 
        endif

c error propagation for Gram/Cayley expansion
          Bij_err = 1d-14 
          do 800 r=rup,0,-1
            C00_err_newprelim(id,r+2) = max( Bij_err,
     &        abs(tx2(k,0))/abs(tz2(k,l))*Cij_err_newprelim(id,r+1),
     &        abs(detz2)/abs(tz2(k,l))*Cij_err_newprelim(id,r+2) )
            Cij_err_newprelim(id,r) = max( 
     &        abs(tx2(0,j))*Cij_err_newprelim(id,r+1),        
     &        abs(tz2(i,j))*max(C00_err_newprelim(id,r+2),Bij_err),
     &        maxttx0ijm(id)*max(Bij_err,C00_err_newprelim(id,r+1)) 
     &                             )/abs(tx2(i,j))
          accr2_newprelim(id,r) = max(accr2_newprelim(id,r),
     &                    Cij_err_newprelim(id,r)/abs(scalint(id)))

800       continue
          do n=0,r2
            acc = max(acc,accr2_newprelim(id,n))
          enddo

        r2eff = rup+1

c stop if accuracy becomes worse
        if ((rup.ge.r2+3).and.(acc.gt.accprev1).and.
     &      (accprev1.gt.accprev2)) goto 600
        accprev2 = accprev1
        accprev1 = acc

c repeat iteration if necessary 
        if ((acc.gt.cacc).and.(rup.lt.rupmax)) goto 500

600     continue

c PaVe reduction of C_{00...} of rank r=rup+1, ... ,2*rup -- Eq.(5.10)
          do 601 r=rup+1,2*rup
          do 601 i0=2*(r-rup),r,2
          i12 = r-i0
          do 601 i1=0,i12
            i2 = i12-i1
            Cuv(i0,i1,i2) = 1d0/2d0/(i0+i12)*( 
     &              Buv0(i0-2,i1,i2) + 2d0*m02*Cuv(i0-2,i1,i2) 
     &            + f(1)*Cuv(i0-2,i1+1,i2) + f(2)*Cuv(i0-2,i1,i2+1) )
            C(i0,i1,i2)   = 1d0/2d0/(i0+i12)*( 4d0*Cuv(i0,i1,i2)
     &            + B0(i0-2,i1,i2) + 2d0*m02*C(i0-2,i1,i2) 
     &            + f(1)*C(i0-2,i1+1,i2) + f(2)*C(i0-2,i1,i2+1) )
601       continue

c final absolute error
        Cmax = 0d0
        i0=0
        do r=0,r2
        i12 = r-i0
        do i1=0,i12
          i2=i12-i1
          Cmax = max(Cmax,abs(C(i0,i1,i2)))
        enddo
        Cij_err_newprelim(id,r) = accr2_newprelim(id,r)*Cmax
        enddo

        end

**********************************************************************
        subroutine Csm1_dd(C,Cuv,
     &              p01,p12,p20,m02,m12,m22,k,acc,z2,f,
     &              r1,r2,r2eff,id,nid)
**********************************************************************
*       Expansion for small momenta - variant 1 (f(k) not small)
*
*       3-point coefficients  
*       C(i,j,k) = C_{0...01...12...2}(p01,p12,p20,m02,m12,m22)
*                     \___/\___/\___/
*                       i    j    k   indices
*       of rank r=i+j+k with r1 <= r <= r2
*       coefficients with rank r < r1 assumed to be known
*
*       Cuv(i,j,k) = coefficient of 1/eps in C(i,j,k),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       28.5.2013 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        integer r,r1,r2,i(2),k,m,n,rup,rupmax,r2eff
        integer del(2,2),i0,i1,i2,i12,id,nid(0:nmax-1)
        data del/1,0,0,1/
        complex*16 B0(0:2*rmax2,0:rmax2,0:rmax2)
        complex*16 Buv0(0:2*rmax2,0:rmax2,0:rmax2)
        complex*16 B_1(0:2*rmax2,0:rmax2),Buv_1(0:2*rmax2,0:rmax2)
        complex*16 B_2(0:2*rmax2,0:rmax2),Buv_2(0:2*rmax2,0:rmax2)
        complex*16 Cstore(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 C(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 Cuv(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 m02,m12,m22,f(2)
        real*8 z2(2,2)

        rupmax = rmax3-1

c maximal iteration already reached in previous call
        if ((r1.gt.0).and.(r2_new_aux(id).ge.rupmax)) then
          r2eff = r2_new_aux(id)
          acc = accr2_newprelim(id,r2eff) 
          return
        endif

c start iteration
c----------------
c       rup = maximal rank calculated for C(0,i1,i2)
        do r=0,rmax3
          Cij_err_newprelim(id,r) = 0d0
          C00_err_newprelim(id,r) = 0d0
        enddo
        if (r1.eq.0) then
          rup = -1
          Cuv(0,0,0) = 0d0
        else
          rup = r2_new_aux(id)
        endif
        accprev1 = 1d30
        accprev2 = 1d30
500     continue
        rup = rup+1

c calculate new B integrals
        call B0_dd(B0,Buv0, p12,m12,m22,rup,nid(0))
        call B_dd(B_1,Buv_1,p20,m02,m22,rup,nid(1))
        call B_dd(B_2,Buv_2,p01,m02,m12,rup,nid(2))

c initialize higher-rank tensors
        do i0=0,rup+1
        do i1=0,rup+1-i0
        i2 = rup+1-i0-i1
          C(i0,i1,i2) = 0d0
        enddo
        enddo

c S functions
        do i0=0,rup
        do i1=0,rup-i0
        i2 = rup-i0-i1
          do n=1,2
            Sh3_aux(id,n,i0,i1,i2) = -B0(i0,i1,i2) 
          enddo
          if (i1.eq.0) Sh3_aux(id,1,i0,i1,i2) = Sh3_aux(id,1,i0,i1,i2) 
     &                                          + B_1(i0,i2)
          if (i2.eq.0) Sh3_aux(id,2,i0,i1,i2) = Sh3_aux(id,2,i0,i1,i2) 
     &                                          + B_2(i0,i1)
        enddo
        enddo

c Cuv_{00...} from PaVe reduction -- Eq.(5.10)
        do i0=2*((rup+1)/2),2,-2
        do i1=0,rup+1-i0
        i2 = rup+1-i0-i1
          Cuv(i0,i1,i2) = 1d0/2d0/(rup+1d0)*( 
     &              Buv0(i0-2,i1,i2) + 2d0*m02*Cuv(i0-2,i1,i2) 
     &            + f(1)*Cuv(i0-2,i1+1,i2) + f(2)*Cuv(i0-2,i1,i2+1) )
        enddo
        enddo

c store previous approximation for error estimate
        if (rup.ge.r2) then
          do n=0,rup-1
            Cstore(0,n,0) = C(0,n,0) 
            Cstore(0,0,n) = C(0,0,n) 
          enddo
        endif

c iteration: rup,  step: r
        do 400 r=rup,0,-1

c C_{00...} from iteration Eq.(5.63)
        do 102 i0=2*((r+1)/2),2,-2
        do 102 i1=0,r+1-i0
        i2 = r+1-i0-i1
        i(1) = i1
        i(2) = i2
          C(i0,i1,i2) = 1d0/2d0/(2d0+i0+2*i1+2*i2)*(
     &      4d0*Cuv(i0,i1,i2) 
     &      + 2d0*B0(i0-2,i1,i2)+2d0*m02*C(i0-2,i1,i2) )
          do 102 m=1,2
          do 102 n=1,2
          C(i0,i1,i2) = C(i0,i1,i2) - 1d0/2d0/(2d0+i0+2*i1+2*i2)
     &      * z2(m,n)*C(i0-2,i1+del(m,1)+del(n,1),i2+del(m,2)+del(n,2))
102     continue

c coefficients C(0,i1,i2) from Eq.(5.62)
        i0 = 0
        do 103 i1=0,r
        i2 = r-i0-i1
        i(1) = i1
        i(2) = i2
          C(i0,i1,i2) = ( Sh3_aux(id,k,i0,i1,i2) 
     &      - z2(k,1)*C(i0,i1+del(1,1),i2+del(2,1)) 
     &      - z2(k,2)*C(i0,i1+del(1,2),i2+del(2,2)) )/f(k)
          if (i(k).ne.0) C(i0,i1,i2) = C(i0,i1,i2)
     &      - 2d0*i(k)*C(i0+2,i1-del(1,k),i2-del(2,k))/f(k)
103     continue

400     continue

c estimate precision from last improvements
        if ((rup.le.r2).and.(rup.lt.rupmax)) then
          acc = 1d30
        else
c         acc1 = abs(C(0,0,0)/scalint(id)-1d0)
          acc1 = 0d0
          do n=0,rup-1
            acc1 = max(acc1,abs(Cstore(0,n,0)/C(0,n,0)-1d0))
            acc1 = max(acc1,abs(Cstore(0,0,n)/C(0,0,n)-1d0))
            accr2_newprelim(id,n) = acc1
          enddo
          acc = accr2_newprelim(id,r2) 
        endif

c error propagation for small momentum expansion - variant 1
          Bij_err = 1d-14 
          do 800 r=rup,0,-1
          if (r.ge.1) then
            C00_err_newprelim(id,r+1) = max( Bij_err,
     &                     am02(id)*Cij_err_newprelim(id,r-1),
     &                     maxz(id)*Cij_err_newprelim(id,r+1) )
          endif
          Cij_err_newprelim(id,r) = 
     &          max( Bij_err,C00_err_newprelim(id,r+1)
     &               ,maxz(id)*Cij_err_newprelim(id,r+1) )/abs(f(k)) 
          accr2_newprelim(id,r) = max(accr2_newprelim(id,r),
     &                     Cij_err_newprelim(id,r)/abs(scalint(id)))

800       continue
          do n=0,r2
            acc = max(acc,accr2_newprelim(id,n))
          enddo

        r2eff = rup

c stop if accuracy becomes worse
        if ((rup.ge.r2+3).and.(acc.gt.accprev1).and.
     &      (accprev1.gt.accprev2)) goto 600
        accprev2 = accprev1
        accprev1 = acc

c repeat iteration if necessary
        if ((acc.gt.cacc).and.(rup.lt.rupmax)) goto 500

600     continue

c PaVe reduction of C_{00...} of rank r=rup+1, ... ,2*rup -- Eq.(5.10)
          do 601 r=rup+1,2*rup
          do 601 i0=2*(r-rup),r,2
          i12 = r-i0
          do 601 i1=0,i12
            i2 = i12-i1
            Cuv(i0,i1,i2) = 1d0/2d0/(i0+i12)*( 
     &              Buv0(i0-2,i1,i2) + 2d0*m02*Cuv(i0-2,i1,i2) 
     &            + f(1)*Cuv(i0-2,i1+1,i2) + f(2)*Cuv(i0-2,i1,i2+1) )
            C(i0,i1,i2)   = 1d0/2d0/(i0+i12)*( 4d0*Cuv(i0,i1,i2)
     &            + B0(i0-2,i1,i2) + 2d0*m02*C(i0-2,i1,i2) 
     &            + f(1)*C(i0-2,i1+1,i2) + f(2)*C(i0-2,i1,i2+1) )
601       continue

c final absolute error
        Cmax = 0d0
        i0=0
        do r=0,r2
        i12 = r-i0
        do i1=0,i12
          i2=i12-i1
          Cmax = max(Cmax,abs(C(i0,i1,i2)))
        enddo
        Cij_err_newprelim(id,r) = accr2_newprelim(id,r)*Cmax
        enddo

        end

**********************************************************************
        subroutine C0_dd(C0,Cuv0,p01,p12,p20,m02,m12,m22,r2,id)
**********************************************************************
*       3-point coefficients C(0)_{...} with unshifted momentum 
*---------------------------------------------------------------------
*       21.7.2006 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        complex*16 C(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 Cuv(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 C0(0:2*rmax3,0:rmax3,0:rmax3,0:rmax3)
        complex*16 Cuv0(0:2*rmax3,0:rmax3,0:rmax3,0:rmax3)
        complex*16 m02,m12,m22
        integer r2,r,i,i0,i1,i2,i3,i123,id

        if (id.eq.0) then
          do i=0,7
            r2_aux(i)     = -1
            r20_aux(i)    = -1
            r2_new_aux(i) = -1
          enddo
        endif

c read cached information
c------------------------
        if (r20_aux(id).ne.-1) then
           do 500 r=0,r20_aux(id)
           do 500 i0=0,2*r,2
           i123 = r-i0/2
           do 500 i1=0,i123
           do 500 i2=0,i123-i1
            i3 = i123-i1-i2
             C0(i0,i1,i2,i3)   = C0_aux(id,i0,i1,i2,i3)
             Cuv0(i0,i1,i2,i3) = Cuv0_aux(id,i0,i1,i2,i3)
500       continue
        endif

        call C_dd(C,Cuv,p01,p12,p20,m02,m12,m22,r2,id)

        do 101 r=0,r2
          do 101 i0=0,2*r,2
          i123 = r-i0/2
          i1=0
          do 102 i2=0,i123-i1
            i3 = i123-i1-i2
            C0(i0,0,i2,i3)   = C(i0,i2,i3)
            Cuv0(i0,0,i2,i3) = Cuv(i0,i2,i3)
102       continue
          do 101 i1=1,i123
          do 101 i2=0,i123-i1
            i3 = i123-i1-i2
            C0(i0,i1,i2,i3)   = -C0(i0,i1-1,i2,i3)
     &                  - C0(i0,i1-1,i2+1,i3)   - C0(i0,i1-1,i2,i3+1)
            Cuv0(i0,i1,i2,i3) = -Cuv0(i0,i1-1,i2,i3)
     &                  - Cuv0(i0,i1-1,i2+1,i3) - Cuv0(i0,i1-1,i2,i3+1)
101     continue

c cache information
        r20_aux(id) = max(r20_aux(id),r2)
        do 600 r=0,r2
        do 600 i0=0,2*r,2
        i123 = r-i0/2
        do 600 i1=0,i123
        do 600 i2=0,i123-i1
          i3 = i123-i1-i2
          C0_aux(id,i0,i1,i2,i3)   = C0(i0,i1,i2,i3)
          Cuv0_aux(id,i0,i1,i2,i3) = Cuv0(i0,i1,i2,i3)
600     continue

        end

**********************************************************************
        function C0dd(p01,p12,p20,m02,m12,m22,ext)
**********************************************************************
*       scalar 3-point function
*---------------------------------------------------------------------
*       20.2.2008 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

        complex*16 C0dd,C0ir_dd,C0coll02_dd,C0coll0_dd,C0m2zero_dd,S3
        complex*16 m02,m12,m22,m2(0:2),m2e(0:2)
        complex*16 x(0:2),xe(0:2),dx(0:2),dxe(0:2),ieps
        real*8 p2(0:2)
        integer ext,i,ip,im
        logical smallp2(0:2),smallm2(0:2),coll(0:2),soft(0:2)

        p2(0) = p01
        p2(1) = p12
        p2(2) = p20
        m2(0) = m02
        m2(1) = m12
        m2(2) = m22

        do i=0,2
          smallp2(i) = (abs(p2(i)).lt.1d-15)
          smallm2(i) = (abs(m2(i)).lt.1d-15)
        enddo

c soft and collinear singularities
        do i=0,2
          ip = mod(i+1,3)
          soft(i) = (abs(m2(i)).lt.1d-15).and.
     &              (dcmplx(p2(i)).eq.m2(ip)).and.
     &              (dcmplx(p2(mod(i+2,3))).eq.m2(mod(i+2,3)))
          coll(i) = smallp2(i).and.smallm2(i).and.smallm2(ip)
          if (coll(i).and.(.not.
     &    (((p2(i).eq.0d0).and.(m2(i).eq.m2(ip))).or.
     &    ((dcmplx(p2(i)).eq.m2(i)).and.(m2(ip).eq.(0d0,0d0))).or.
     &    ((dcmplx(p2(i)).eq.m2(ip)).and.(m2(i).eq.(0d0,0d0))) ))) then
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
          if (smallp2(i)) p2(i) = 0d0
          if (smallm2(i)) m2(i) = 0d0
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

        rlam   = sqrt( (p2(0)+p2(1)-p2(2))**2-4d0*p2(0)*p2(1) )
        scale2 = abs(p01)+abs(p12)+abs(p20)+abs(m02)+abs(m12)+abs(m22)

        if (rlam/scale2.lt.1d-14) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 
     &         'C0dd for too small Gram det not supported!'
            call DD_debugoutput()
          endif
          stopflag = min(-7,stopflag)
        endif

        eps  = 1d-10
        ieps = dcmplx(0d0,eps)

        do i=0,2
          if (smallm2(i)) then
            m2e(i) = ((i+.7d0)*eps-(2*i+1)*ieps)*scale2
          elseif (dimag(m2(i)).eq.0d0) then
            m2e(i) = m2(i)*(1+((i+.7d0)*eps-(2*i+1)*ieps))
          else
            m2e(i) = m2(i)+abs(m2(i))*((i+.7d0)*eps-(2*i+1)*ieps)
          endif
        enddo

        C0dd = 0d0

        do i=0,2
          ip = mod(i+1,3)
          im = mod(i+2,3)

          if (smallp2(i)) then
            if (p2(im)-p2(ip).lt.0d0) then
              x(i)  = ( (m2(i)*p2(ip)-m2(ip)*p2(im))/(p2(ip)-p2(im))
     &                +p2(ip)-m2(im) )/(p2(ip)-p2(im))
              xe(i) = ( (m2e(i)*p2(ip)-m2e(ip)*p2(im))/(p2(ip)-p2(im))
     &                +p2(ip)-m2e(im) )/(p2(ip)-p2(im))
              dx(i) = 1d0-x(i)
              dxe(i)= 1d0-xe(i)
              C0dd = C0dd + S3( x(i),0d0, m2(i)- m2(ip), m2(ip),
     &                         xe(i),0d0,m2e(i)-m2e(ip),m2e(ip))/rlam
            elseif (p2(im)-p2(ip).gt.0d0) then
c             contribution is zero in this case
            endif

          else

            if (p2(im)-p2(ip)-p2(i).gt.0d0) then
              alpha = (p2(im)-p2(ip)-p2(i)+rlam)/2d0/p2(i)
            else
              alpha = 2d0*p2(ip)/(p2(im)-p2(ip)-p2(i)-rlam)
            endif
c           alphap  = (p2(ip)+p2(i)*alpha)/rlam
c           alphap1 = alphap-1d0
            if (p2(im)+p2(ip)-p2(i).gt.0d0) then
              alphap = (p2(im)+p2(ip)-p2(i)+rlam)/2d0/rlam
              alphap1= 2d0*p2(im)*p2(ip)/rlam/(p2(im)+p2(ip)-p2(i)+rlam)
            else
              alphap = 2d0*p2(im)*p2(ip)/rlam/(p2(im)+p2(ip)-p2(i)-rlam)
              alphap1= (p2(im)+p2(ip)-p2(i)-rlam)/2d0/rlam
            endif
c           alpha1 = 1d0+alpha
            if (p2(im)-p2(ip)+p2(i).gt.0d0) then
              alpha1 = (p2(im)-p2(ip)+p2(i)+rlam)/2d0/p2(i)
            else
              alpha1 = 2d0*p2(im)/(p2(im)-p2(ip)+p2(i)-rlam)
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
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

        complex*16 S3,b,c,be,ce,cspen_dd,eta_dd,sqe_dd,eta3,a1,a2,a3
        complex*16 y0,y0e,y(2),ye(2),caux,ieps,etay
        complex*16 x1(2),x2(2),x3(2),xe1(2),xe2(2),xe3(2),x0,x0e
        integer k

        eta3(a1,a2,a3) = eta_dd(a1*a2,a3) + eta_dd(a1,a2)

        S3 = 0d0

        eps  = 1d-20
        ieps = dcmplx(0d0,eps)
        crit = 1d-12

        if ((a.eq.0d0).and.(b.eq.(0d0,0d0)).and.(c.eq.(0d0,0d0))) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'S3: a=b=c=0 not allowed.'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        elseif ((a.eq.0d0).and.(b.eq.(0d0,0d0))) then
          y(1)  = 1d30
          y(2)  = 1d30
          if ((ae.eq.0d0).and.(be.eq.0d0)) then
            S3 = 0d0
            return
          elseif (ae.eq.0d0) then
            ye(1) = sqe_dd(dcmplx(ae),be,ce)
            ye(2) = 1d30
          else
            ye(1) = sqe_dd(dcmplx(ae),be,ce)
            ye(2) = ce/ae/ye(1)
          endif
        elseif (a.eq.0d0) then
          y(1)  = -c/b
          y(2)  = 1d30
          ye(1) = -ce/be
          ye(2) = y(2)
        elseif (c.eq.0d0) then
          y(1)  = 0d0
          y(2)  = -b/a
          ye(1) = sqe_dd(dcmplx(ae),be,ce)
          ye(2) = ce/ae/ye(1)
        else
          y(1)  = sqe_dd(dcmplx(a),b,c)
          y(2)  = c/a/y(1)
          ye(1) = sqe_dd(dcmplx(ae),be,ce)
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

        if (abs(y(1)).lt.crit) y(1) = 0d0 
        if (abs(y(2)).lt.crit) y(2) = 0d0 
        if (abs(y(1)-1d0).lt.crit) y(1) = 1d0 
        if (abs(y(2)-1d0).lt.crit) y(2) = 1d0 

        do k=1,2

        if (dreal(y(k)).eq.1d30) then
c             contribution is zero in this case
        elseif ((abs(y(k)-y0).gt.abs(y(k)-1d0)).or.
     &          (y(k).eq.(0d0,0d0))) then
          x1(k)  = y0/(y0-y(k))
          xe1(k) = y0e/(y0e-ye(k))
          x2(k)  = (y0 -1d0)/(y0 -y(k))
          xe2(k) = (y0e-1d0)/(y0e-ye(k))
          if (abs(dimag(x1(k))).lt.crit*abs(x1(k))) 
     &      x1(k) = dcmplx(dreal(x1(k)),
     &                     (abs(dimag(x1(k)))+abs(x1(k))*eps)
     &                     *sign(1d0,dimag(xe1(k))))
          if (abs(dimag(x2(k))).lt.crit*abs(x2(k))) 
     &      x2(k) = dcmplx(dreal(x2(k)),
     &                     (abs(dimag(x2(k)))+abs(x2(k))*eps)
     &                     *sign(1d0,dimag(xe2(k))))
          S3 = S3 + cspen_dd(x1(k)) - cspen_dd(x2(k))
          etay = eta_dd((ye(k)-1d0)/ye(k),ye(k)/(ye(k)-y0e))
          if (etay.ne.(0d0,0d0)) S3 = S3 - etay*log(x2(k))
        else
          x1(k)  = y0 /y(k)
          xe1(k) = y0e/ye(k)
          x2(k)  = (y0 -1d0)/(y(k) -1d0)
          xe2(k) = (y0e-1d0)/(ye(k)-1d0)
          x3(k)  = (y(k) -1d0)/y(k)
          xe3(k) = (ye(k)-1d0)/ye(k)
          if (abs(dimag(x1(k))).lt.crit*abs(x1(k))) 
     &      x1(k) = dcmplx(dreal(x1(k)),
     &                     (abs(dimag(x1(k)))+abs(x1(k))*eps)
     &                     *sign(1d0,dimag(xe1(k))))
          if (abs(dimag(x2(k))).lt.crit*abs(x2(k))) 
     &      x2(k) = dcmplx(dreal(x2(k)),
     &                     (abs(dimag(x2(k)))+abs(x2(k))*eps)
     &                     *sign(1d0,dimag(xe2(k))))
          if (abs(dimag(x3(k))).lt.crit*abs(x3(k))) 
     &      x3(k) = dcmplx(dreal(x3(k)),
     &                     (abs(dimag(x3(k)))+abs(x3(k))*eps)
     &                     *sign(1d0,dimag(xe3(k))))
          S3 = S3 - cspen_dd(x1(k)) + cspen_dd(x2(k)) 
     &         + log(x3(k))**2/2d0 - log(x3(k))*log(1d0-x1(k))
          etay = eta_dd(xe3(k),ye(k)/(ye(k)-y0e))
          if (etay.ne.(0d0,0d0)) 
     &      S3 = S3 + etay*(-log(x2(k))
     &           -eta_dd((1d0-ye(k))/(ye(k)-y0e),(ye(k)-y0e)/(1d0-y0e)))
        endif

        enddo

        if ((y0.ne.(0d0,0d0)).and.(y0.ne.(1d0,0d0))) then
          etay  = eta3(ce,1d0-y0e/ye(1),1d0-y0e/ye(2))
          if (etay.eq.(0d0,0d0)) return
          x0  = (y0 -1d0)/y0
          x0e = (y0e-1d0)/y0e
          if (abs(dimag(x0))/abs(x0).lt.crit) 
     &      x0 = dcmplx(dreal(x0),(abs(dimag(x0))+abs(x0)*eps)
     &                            *sign(1d0,dimag(x0e)))
          if ((dimag(x0).ne.0d0).or.(dreal(x0).gt.0d0)) then
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
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        complex*16 C0ir_dd,ieps,cspen_dd,s,xs,root,m02,cmp2,cm2,null
        complex*16 div
        integer ext,ext0
        logical smallm12,smallm22

        rmp2(rm2) = mx2(nint(rm2*1d20))
        cmp2(cm2) = mx2(nint(dreal(cm2*1d20)))

        C0ir_dd = 0d0

        ext0 = ext
        null = dcmplx(0d0,0d0)
        eps  = 1d-20
        ieps = dcmplx(0d0,eps)
        pi   = 4d0*datan(1d0)

        if (((m02.ne.(0d0,0d0)).and.((p01.lt.1d-18).or.(p20.lt.1d-18)))
     &      .or.(abs(m02).gt.1d-18)) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'C0ir_dd: inconsistent choice of m02:'
            call DD_debugoutput()
          endif
          stopflag = min(-9,stopflag)
        endif

c convert input parameters
        smallm12 = (abs(p01).lt.1d-15)
        smallm22 = (abs(p20).lt.1d-15)
        if (smallm12) then
          if (p01.ne.0d0) then
            m12 = rmp2(p01)
          else
            m12 = 0d0
          endif
        else
          m12 = p01
        endif
        if (smallm22) then
          if (p20.ne.0d0) then
            m22 = rmp2(p20)
          else
            m22 = 0d0
          endif
        else
          m22 = p20
        endif
        m1 = sqrt(m12)
        m2 = sqrt(m22)
        if (abs(p12).gt.1d-15) then
          s = p12+abs(p12)*ieps
        else
          s = null
        endif

        if ((m12.eq.0d0).and.(m22.eq.0d0)) then
          if (s.eq.null) goto 100
          C0ir_dd = ( delta2ir+delta1ir*log(-mir2/s)
     &               +log(-mir2/s)**2/2d0-pi**2/6d0 )/s
        elseif ((m12.eq.0d0).and.smallm22) then
          if (s.eq.null) goto 100
          C0ir_dd = ( delta2ir/2d0+delta1ir*log(-mir2/s)
     &               -delta1ir*log(mir2/m22)/2d0
     &               +log(-mir2/s)**2/2d0-log(mir2/m22)**2/4d0
     &               -pi**2/6d0 )/s
        elseif ((m22.eq.0d0).and.smallm12) then
          if (s.eq.null) goto 100
          C0ir_dd = ( delta2ir/2d0+delta1ir*log(-mir2/s)
     &               -delta1ir*log(mir2/m12)/2d0
     &               +log(-mir2/s)**2/2d0-log(mir2/m12)**2/4d0
     &               -pi**2/6d0 )/s
        elseif (m12.eq.0d0) then
          C0ir_dd = ( delta2ir/2d0+delta1ir*log(mir2/(m22-s))
     &               -delta1ir*log(mir2/m22)/2d0
     &               +log(mir2/(m22-s))**2/2d0-log(mir2/m22)**2/4d0
     &               -cspen_dd(s/(s-m22)) )/(s-m22)
        elseif (m22.eq.0d0) then
          C0ir_dd = ( delta2ir/2d0+delta1ir*log(mir2/(m12-s))
     &               -delta1ir*log(mir2/m12)/2d0
     &               +log(mir2/(m12-s))**2/2d0-log(mir2/m12)**2/4d0
     &               -cspen_dd(s/(s-m12)) )/(s-m12)
        elseif (smallm12.and.smallm22) then
          if (s.eq.null) goto 100
          C0ir_dd = ( (delta1ir+log(-mir2/s))*log(-m1*m2/s)
     &      -log(-m12/s)**2/4d0-log(-m22/s)**2/4d0-pi**2/6d0 ) / s
        elseif (smallm12) then
          C0ir_dd = ( (-delta1ir+log(m1*(m22-s)/mir2/m2))
     &                *log((m22-s)/m1/m2)+cspen_dd(s/m22) ) / (s-m22)
        elseif (smallm22) then
          C0ir_dd = ( (-delta1ir+log(m2*(m12-s)/mir2/m1))
     &                *log((m12-s)/m2/m1)+cspen_dd(s/m12) ) / (s-m12)
        else
          if (s.eq.null) then
            if (m12.eq.m22) then
              C0ir_dd = (delta1ir+log(mir2/m12))/2d0/m12
            else
              C0ir_dd = (delta1ir+log(mir2/m1/m2))*log(m1/m2)/(m12-m22)
            endif
            return
          endif
          root = sqrt(1d0-4d0*m1*m2/(s-(m1-m2)**2))
          xs   = (root-1d0)/(root+1d0)
          if (m02.eq.(0d0,0d0)) then
            div  = delta1ir+log(mir2)
          else
            div = log(cmp2(m02))
          endif
          C0ir_dd = xs/m1/m2/(1d0-xs**2)*( 
     &      (-div+log(m1*m2))*log(xs)-log(xs)**2/2d0
     &      +2d0*log(xs)*log(1d0-xs**2)+log(m1/m2)**2/2d0-pi**2/6d0
     &      +cspen_dd(xs**2)+cspen_dd(1d0-xs*m1/m2)
     &      +cspen_dd(1d0-xs*m2/m1) )
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
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        complex*16 C0coll02_dd,ieps,cm02,cm12,cm22,s
        integer ext,ext0

        rmp2(rm2) = mx2(nint(rm2*1d20))

        C0coll02_dd = 0d0

        ext0 = ext
        eps  = 1d-20
        ieps = dcmplx(0d0,eps)
        pi   = 4d0*datan(1d0)

c convert input parameters
        if (abs(p12).lt.1d-15) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 
     &         'C0coll02_dd: not implemented for small p12'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        endif
        q01 = 0d0
        q20 = 0d0
        m02 = 0d0
        m12 = 0d0
        m22 = 0d0
        if (p01.ne.0d0) q01 = rmp2(p01)
        if (p20.ne.0d0) q20 = rmp2(p20)
        if (cm02.ne.(0d0,0d0)) m02 = rmp2(dreal(cm02))
        if (cm12.ne.(0d0,0d0)) m12 = rmp2(dreal(cm12))
        if (cm22.ne.(0d0,0d0)) m22 = rmp2(dreal(cm22))
        s = p12+abs(p12)*ieps

c C0(m^2,s,m^2,m,0,0)     (B.13)
        if ((m02.ne.0d0).and.(m12.eq.0d0).and.(m22.eq.0d0).and.
     &      (q01.eq.m02).and.(q20.eq.m02)) then
          C0coll02_dd = ( log(-m02/s)**2/2d0+2d0*pi**2/3d0 )/s
          return
c C0(m^2,s,0,m,0,m)       (B.14)
        elseif ((m02.ne.0d0).and.(m12.eq.0d0).and.(m22.eq.m02).and.
     &          (q01.eq.m02).and.(q20.eq.0d0)) then
          C0coll02_dd = ( log(-m02/s)**2/2d0+pi**2/3d0 )/s
          return
c C0(0,s,m^2,m,m,0)       (B.14)
        elseif ((m02.ne.0d0).and.(m12.eq.m02).and.(m22.eq.0d0).and.
     &          (q01.eq.0d0).and.(q20.eq.m02)) then
          C0coll02_dd = ( log(-m02/s)**2/2d0+pi**2/3d0 )/s
          return
c C0(0,s,0,m,m,m)         (B.15)
        elseif ((m02.ne.0d0).and.(m12.eq.m02).and.(m22.eq.m02).and.
     &          (q01.eq.0d0).and.(q20.eq.0d0)) then
          C0coll02_dd = log(-m02/s)**2/2d0/s
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
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        complex*16 C0coll0_dd,ieps,cm02,cm12,cm22,s1,s2,m22,cspen_dd
        integer ext,ext0

        rmp2(rm2) = mx2(nint(rm2*1d20))

        C0coll0_dd = 0d0

        ext0 = ext
        eps  = 1d-20
        ieps = dcmplx(0d0,eps)
        pi   = 4d0*datan(1d0)

c convert input parameters
        q01 = 0d0
        m02 = 0d0
        m12 = 0d0
        if (p01.ne.0d0) q01 = rmp2(p01)
        if (cm02.ne.(0d0,0d0)) m02 = rmp2(dreal(cm02))
        if (cm12.ne.(0d0,0d0)) m12 = rmp2(dreal(cm12))
        if (abs(p12).lt.1d-15) then
          s1 = 0d0
        else
          s1 = p12+abs(p12)*ieps
        endif
        if (abs(p20).lt.1d-15) then
          s2 = 0d0
        else
          s2 = p20+abs(p20)*ieps*1.3d0
        endif
        if (abs(cm22).lt.1d-15) then
          m22 = 0d0
        else
          m22 = cm22
        endif

c C0(m^2,s,M^2,m,0,M)   (B.10)
        if ((q01.ne.0d0).and.(q01.eq.m02).and.(m12.eq.0d0).and.
     &      (dcmplx(p20).eq.cm22)) then
          C0coll0_dd = ( log((m22-s1)/sqrt(m02*m22))**2
     &                  +5d0*pi**2/12d0+cspen_dd(s1/m22) )/(s1-m22)
          return
c C0(m^2,M^2,s,0,m,M)   (B.10)
        elseif ((q01.ne.0d0).and.(q01.eq.m12).and.(m02.eq.0d0).and.
     &          (dcmplx(p12).eq.cm22)) then
          C0coll0_dd = ( log((m22-s2)/sqrt(m12*m22))**2
     &                  +5d0*pi**2/12d0+cspen_dd(s2/m22) )/(s2-m22)
          return
c C0(0,s,M^2,m,m,M)     (B.11)
        elseif ((q01.eq.0d0).and.(m02.eq.m12).and.(m02.ne.0d0).and.
     &          (dcmplx(p20).eq.cm22)) then
          C0coll0_dd = ( log((m22-s1)/sqrt(m02*m22))**2
     &                  +pi**2/12d0+cspen_dd(s1/m22) )/(s1-m22)
          return
c C0(0,M^2,s,m,m,M)     (B.11)
        elseif ((q01.eq.0d0).and.(m02.eq.m12).and.(m02.ne.0d0).and.
     &          (dcmplx(p12).eq.cm22)) then
          C0coll0_dd = ( log((m22-s2)/sqrt(m12*m22))**2
     &                  +pi**2/12d0+cspen_dd(s2/m22) )/(s2-m22)
          return
c C0(m^2,s1,s2,m,0,M)   (B.2)
        elseif ((q01.ne.0d0).and.(q01.eq.m02).and.(m12.eq.0d0)) then
          if (m22.eq.(0d0,0d0)) then
            C0coll0_dd = ( log(s2/s1)/2d0*(log(-s2/q01)+log(-s1/q01))
     &                    -2d0*cspen_dd((s1-s2)/s1) )/(s2-s1)
          else
            C0coll0_dd = ( log((m22-s2)/q01)*log((m22-s2)/m22)
     &                    -log((m22-s1)/q01)*log((m22-s1)/m22)
     &                    -2d0*cspen_dd((s2-s1)/(m22-s1))
     &                    +cspen_dd(s2/m22)-cspen_dd(s1/m22) )/(s2-s1)
          endif
          return
c C0(m^2,s1,s2,0,m,M)   (B.2)
        elseif ((q01.ne.0d0).and.(q01.eq.m12).and.(m02.eq.0d0)) then
          if (m22.eq.(0d0,0d0)) then
            C0coll0_dd = ( log(s1/s2)/2d0*(log(-s1/q01)+log(-s2/q01))
     &                    -2d0*cspen_dd((s2-s1)/s2) )/(s1-s2)
          else
            C0coll0_dd = ( log((m22-s1)/q01)*log((m22-s1)/m22)
     &                    -log((m22-s2)/q01)*log((m22-s2)/m22)
     &                    -2d0*cspen_dd((s1-s2)/(m22-s2))
     &                    +cspen_dd(s1/m22)-cspen_dd(s2/m22) )/(s1-s2)
          endif
          return
c C0(0,s1,s2,m,m,M)     (B.3)
        elseif ((q01.eq.0d0).and.(m02.eq.m12).and.(m02.ne.0d0)) then
          if (m22.eq.(0d0,0d0)) then
            C0coll0_dd = log(s1/s2)/2d0*(log(-s1/m02)+log(-s2/m02))
     &                     /(s1-s2)
          else
            C0coll0_dd = ( log((m22-s1)/sqrt(m02*m22))**2
     &                    -log((m22-s2)/sqrt(m02*m22))**2
     &                    +cspen_dd(s1/m22)-cspen_dd(s2/m22) )/(s1-s2)
          endif
          return
c C0(0,s1,s2,0,0,M)     (B.4)
        elseif ((q01.eq.0d0).and.(m02.eq.0d0).and.(m12.eq.0d0)) then
          if (m22.eq.(0d0,0d0)) then
            C0coll0_dd = log(s2/s1)/2d0/(s1-s2)
     &                     *(2d0*delta1ir+log(-mir2/s1)+log(-mir2/s2))
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
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

        complex*16 nC0m2zero_dd
        complex*16 C0m2zero_dd,m02,m12,m02e,m12e,ieps
        complex*16 b,c,be,ce,y0(2),dy0(2),S3
        complex*16 db,dc,dbe,dce,arg,arge
        integer i,sgn,ext,ext0

        ext0  = ext
        rlam2 = (p01+p12-p20)**2-4d0*p01*p12
        if (rlam2.le.0d0) then
          rlam = 0d0
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'C0m2zero_dd: lambda<=0 not supported'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        else
          rlam = sqrt(rlam2)
        endif

        scale2 = abs(p01)+abs(p12)+abs(p20)+abs(m02)+abs(m12)

        if (rlam/scale2.lt.1d-14) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 
     &         'C0m2zero_dd for too small Gram det not supported!'
            call DD_debugoutput()
          endif
          stopflag = min(-7,stopflag)
        endif

        eps  = 1d-13
        ieps = dcmplx(0d0,eps)
        eps2 = 1d-20
        crit = 1d-10

        if (m02.eq.(0d0,0d0)) then
          m02e = (eps-ieps)*scale2
        elseif (dimag(m02).eq.0d0) then
          m02e = m02*(1d0+eps-ieps)
        else
          m02e = m02+abs(m02)*(eps-ieps)*1.3d0
        endif
        if (m12.eq.(0d0,0d0)) then
          m12e = 1.3d0*(eps-1.3d0*ieps)*scale2
        elseif (dimag(m12).eq.0d0) then
          m12e = m12*(1d0+1.4*eps-1.8d0*ieps)
        else
          m12e = m12+abs(m12)*(2.1d0*eps-1.3d0*ieps)*2.1d0
        endif

        if (p01.eq.0d0) then
          if (p12-p20.gt.0d0) then
            y0(1)  = 1d30
            dy0(1) = 1d30
            y0(2)  = p12/(p12-p20)
            dy0(2) = p20/(p20-p12)
          else
            y0(1)  = p12/(p12-p20)
            dy0(1) = p20/(p20-p12)
            y0(2)  = 1d30
            dy0(2) = 1d30
          endif
        else
          if (p12+p01-p20.gt.0d0) then
            y0(1) = (p12+p01-p20+rlam)/2d0/p01
            y0(2) = 2d0*p12/(p12+p01-p20+rlam)
          else
            y0(1) = 2d0*p12/(p12+p01-p20-rlam)
            y0(2) = (p12+p01-p20-rlam)/2d0/p01
          endif
          if (p12-p01-p20.gt.0d0) then
            dy0(1) = -(p12-p01-p20+rlam)/2d0/p01
            dy0(2) = -2d0*p20/(p12-p01-p20+rlam)
          else
            dy0(1) = -2d0*p20/(p12-p01-p20-rlam)
            dy0(2) = -(p12-p01-p20-rlam)/2d0/p01
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

        C0m2zero_dd = 0d0
        do i=1,2
        if (dreal(y0(i)).ne.1d30) then
          sgn = 3-2*i
c special case a*y0^2+b*y0+c=0
          if (abs(a*y0(i)**2+b*y0(i)+c).lt.
     &        crit*(abs(a)+abs(b)+abs(c))) then
            C0m2zero_dd = C0m2zero_dd 
     &       -sgn*S3(y0(i),0d0,dcmplx(a),b+a*y0(i),
     &               y0(i),0d0,dcmplx(a),be+a*y0(i))/rlam
             arg  = (2d0*a*y0(i)+m02-m12-p01)/(m02-m12-p20+p12)
             arge = (2d0*a*y0(i)+m02e-m12e-p01)/(m02e-m12e-p20+p12)
             if (abs(dimag(arg))/abs(arg).lt.crit) 
     &         arg = dcmplx(dreal(arg),(abs(dimag(arg))+abs(arg)*eps2)
     &                                 *sign(1d0,dimag(arge)))
            if ((dreal(y0(i)).lt.0d0).or.(dreal(y0(i)).gt.1d0)) then
              C0m2zero_dd = C0m2zero_dd 
     &                      -sgn/rlam*log(arg)*log(1d0-1d0/y0(i))
            else
              if (cout_on.and.(cout.le.coutmax)) then
                write(outchannel,*) 
     &             'C0m2zero_dd: 0<y0<1 for a*y0^2+b*y0+c=0 !'
                call DD_debugoutput()
              endif
              stopflag = min(-7,stopflag)
            endif
            y0(i) = 1d30
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

        a   = 0d0
        b   = m02-m12-p20+p12
        c   = m12-p12
        be  = m02e-m12e-p20+p12
        ce  = m12e-p12
        db  = -b
        dc  = m02-p20
        dbe = -be
        dce = m02e-p20

        do i=1,2
        if (dreal(y0(i)).ne.1d30) then
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
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

        complex*16 xm02,xm12,m02,m12,m02e,m12e,p01e,p12e,p20e
        complex*16 C0m2zero_dd,sqe_dd,eta_dd,cspen_dd,inteps,z,ze
        complex*16 x(5),xe(5),xx(2),arg(5),arge(5),ieps,ieps2,caux
        complex*16 etax
        integer i,j,sgn,ext,ext0

        inteps(z,ze) = dcmplx( dreal(z),(abs(dimag(z))+abs(z)*eps2)
     &                                  *sign(1d0,dimag(ze)) )

        ext0  = ext
        eps   = 1d-13
        ieps  = dcmplx(0d0,eps)
        eps2  = 1d-20
        ieps2 = dcmplx(0d0,eps2)
        crit  = 1d-10

        if (q20.ne.0d0) then
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

        if (m02.eq.(0d0,0d0)) then
          m02e = (eps-ieps)*scale2
        elseif (dimag(m02).eq.0d0) then
          m02e = m02*(1d0+eps-ieps)
        else
          m02e = m02+abs(m02)*(eps-ieps)*1.3d0
        endif
        if (m12.eq.(0d0,0d0)) then
          m12e = 1.3d0*(eps-1.3d0*ieps)*scale2
        elseif (dimag(m12).eq.0d0) then
          m12e = m12*(1d0+1.4*eps-1.8d0*ieps)
        else
          m12e = m12+abs(m12)*(2.1d0*eps-1.3d0*ieps)
        endif

        if ((p12.eq.0d0).and.(p20.eq.0d0)) then
          x(3)  = sqe_dd(m02,m02+m12-p01,m12)
          x(4)  = m12/m02/x(3)
          xe(3) = sqe_dd(m02e,m02e+m12e-p01,m12e)
          xe(4) = m12e/m02e/xe(3)
          if (abs(x(3)-xe(3)).gt.abs(x(3)-xe(4))) then
            caux  = xe(3)
            xe(3) = xe(4)
            xe(4) = caux
          endif
          if (abs(dimag(x(3)))/abs(x(3)).lt.crit) 
     &      x(3) = inteps(x(3),xe(3))
          if (abs(dimag(x(4)))/abs(x(4)).lt.crit) 
     &      x(4) = inteps(x(4),xe(4))
          C0m2zero_dd = ( log(-x(3))**2+log(-x(4))**2
     &                   -log(m02/m12)**2 )/2d0/p01
          goto 999
        endif

        rlam2 = (p01+p12-p20)**2-4d0*p01*p12
        if (rlam2.le.0d0) then
          rlam = 0d0
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'C0m2zero_dd: lambda<=0 not supported'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        else
          rlam = sqrt(rlam2)
        endif

        if (rlam/scale2.lt.1d-14) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 
     &         'C0m2zero_dd for too small Gram det not supported!'
            call DD_debugoutput()
          endif
          stopflag = min(-7,stopflag)
        endif

        if (p01.eq.0d0) then
          p01e = 2.1d0*(eps+0.9d0*ieps)*scale2
        else
          p01e = p01+ieps*abs(p01)*1.7d0
        endif
        if (p12.eq.0d0) then
          p12e = 2.6d0*(eps+0.7d0*ieps)*scale2
        else
          p12e = p12+ieps*abs(p12)*3.7d0
        endif
        if (p20.eq.0d0) then
          p20e = 1.1d0*(eps+1.2d0*ieps)*scale2
        else
          p20e = p20+ieps*abs(p20)*2.7d0
        endif

        x(1)  = sqe_dd(dcmplx(p20),dcmplx(p20-p01+p12),dcmplx(p12))
        x(2)  = p12/p20/x(1)
        xe(1) = sqe_dd(p20e,p20e-p01e+p12e,dcmplx(p12e))
        xe(2) = p12e/p20e/xe(1)
        if (abs(x(1)-xe(1)).gt.abs(x(1)-xe(2))) then
          caux  = xe(1)
          xe(1) = xe(2)
          xe(2) = caux
        endif
        do i=1,2
          xx(i) = x(i)
          if (abs(dimag(xx(i))).lt.crit*abs(xx(i))) 
     &      xx(i) = inteps(xx(i),xe(i))
        enddo
        if (dreal(x(1)-x(2))*p20.lt.0d0) rlam = -rlam

        if (m02.eq.(0d0,0d0)) then
          if (dcmplx(p01).eq.m12) then
            x(3) = 1d30
            x(4) = 1d30
          else
            x(3) = m12/(p01-m12)
            x(4) = 1d30
          endif
        elseif (m12.eq.(0d0,0d0)) then
            x(3) = 0d0
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
        if (dcmplx(p20).eq.m02) then
          x(5) = 1d30
        else
          x(5) = -(m12-p12)/(m02-p20)
        endif
        xe(5) = -(m12e-p12e)/(m02e-p20e)

        C0m2zero_dd = 0d0
        do 100 i=1,2
          sgn = 3-2*i

          if (m12.eq.(0d0,0d0)) then
            if (dcmplx(p01).eq.m02) then
              C0m2zero_dd = C0m2zero_dd - sgn/rlam*(
     &          -log(m02-abs(m02)*ieps2)*log(-xx(i))-log(-xx(i))**2 )
            else
              C0m2zero_dd = C0m2zero_dd - sgn/rlam*(
     &          -log(m02-p01-abs(m02-p01)*ieps2)*log(-xx(i))
     &          -log(-xx(i))**2/2d0 )
              arg(4)  = 1d0-x(i)/x(4)
              arge(4) = 1d0-xe(i)/xe(4)
              if (abs(dimag(arg(4))).lt.crit*abs(arg(4))) 
     &          arg(4) = inteps(arg(4),arge(4))
              C0m2zero_dd = C0m2zero_dd - sgn/rlam*cspen_dd(arg(4))
              etax = eta_dd(-xe(i),-1d0/xe(4))
              if (etax.ne.(0d0,0d0)) C0m2zero_dd = C0m2zero_dd 
     &                               - sgn/rlam*etax*log(arg(4)) 
            endif
          else
            if (p12.ne.0d0) C0m2zero_dd = C0m2zero_dd 
     &                        + sgn/rlam*log(m12)*log(-xx(i))
            do 101 j=3,4
            if (x(j).ne.dcmplx(1d30)) then
              arg(j)  = 1d0-x(i)/x(j)
              arge(j) = 1d0-xe(i)/xe(j)
              if (abs(dimag(arg(j))).lt.crit*abs(arg(j))) 
     &          arg(j) = inteps(arg(j),arge(j))
              C0m2zero_dd = C0m2zero_dd - sgn/rlam*cspen_dd(arg(j))
              etax = eta_dd(-xe(i),-1d0/xe(j))
              if (etax.ne.(0d0,0d0)) C0m2zero_dd = C0m2zero_dd 
     &                               - sgn/rlam*etax*log(arg(j)) 
            endif
101         continue
          endif

          if (dcmplx(p12).eq.m12) then
            C0m2zero_dd = C0m2zero_dd - sgn/rlam*(
     &         log(m02-p20-abs(m02-p20)*ieps2)*log(-xx(i))
     &        +log(-xx(i))**2/2d0 )
          else
            if (p12.ne.0d0) C0m2zero_dd = C0m2zero_dd 
     &         - sgn/rlam*log(m12-p12-abs(m12-p12)*ieps2)*log(-xx(i))
            if (x(5).ne.dcmplx(1d30)) then
              arg(5)  = 1d0-x(i)/x(5)
              arge(5) = 1d0-xe(i)/xe(5)
              if (abs(dimag(arg(5))).lt.crit*abs(arg(5))) 
     &           arg(5) = inteps(arg(5),arge(5))
              C0m2zero_dd = C0m2zero_dd + sgn/rlam*cspen_dd(arg(5))
              etax = eta_dd(-xe(i),-1d0/xe(5)) 
              if (etax.ne.(0d0,0d0)) C0m2zero_dd = C0m2zero_dd 
     &                               + sgn/rlam*etax*log(arg(5))
            endif
          endif

          C0m2zero_dd = C0m2zero_dd + sgn/rlam*cspen_dd(1d0+xx(i)) 

100     continue

999     continue

        end

