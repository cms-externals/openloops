**********************************************************************
        subroutine Ccoll_dd(C,Cuv,p01,p12,p20,m02,m12,m22,crelacc,r2,id)
**********************************************************************
*       collinear singular C function
*       exact calculation, numerically stable for detG->0
*---------------------------------------------------------------------
*       6.4.05 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        integer r2,id,i1,i2,i12
        complex*16 m02,m12,m22,m2
        complex*16 C(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 Cuv(0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 C0,C1(2),C2(0:2,0:2),C3(0:2,0:2,2)
        complex*16 C4(0:2,0:2,0:2,0:2)
        complex*16 cm2,cmp2

        rmp2(rm2) = mx2(nint(rm2*1d20))
        cmp2(cm2) = mx2(nint(dreal(cm2*1d20)))

        if (r2.gt.4) then
          write(*,*) 'Ccoll_dd not yet supported for rank = ',r2
          stop
        endif

        C0 = scalint(id)

c*** C0(mxx2,s1,s2,0,mxx2,M2) for mxx2 -> 0
        if ((m02.eq.(0d0,0d0)).and.(dreal(m12).eq.p01)) then
          mxx2 = rmp2(p01)
          s1  = p12
          s2  = p20
          m2  = m22

          call Ccoll1_dd(mxx2,s1,s2,M2,C0,C1,C2,C3,C4,crelacc)
c*** C0(s2,s1,mxx2,0,M2,mxx2) for mxx2 -> 0
        elseif ((m02.eq.(0d0,0d0)).and.(dreal(m22).eq.p20)) then
          mxx2 = rmp2(p20)
          s1  = p12
          s2  = p01
          m2  = m12
          call Ccoll1_dd(mxx2,s1,s2,M2,C0,C1,C2,C3,C4,crelacc)
          call Cswap_dd(C0,C1,C2,C3,C4,1,2,4)   
c*** C0(mxx2,s2,s1,mxx2,0,M2) for mxx2 -> 0
        elseif ((m12.eq.(0d0,0d0)).and.(dreal(m02).eq.p01)) then
          mxx2 = rmp2(p01)
          s2  = p12
          s1  = p20
          m2  = m22
          call Ccoll2_dd(mxx2,s1,s2,M2,C0,C1,C2,C3,C4,crelacc)
c*** C0(s1,s2,mxx2,mxx2,M2,0) for mxx2 -> 0
        elseif ((m22.eq.(0d0,0d0)).and.(dreal(m02).eq.p20)) then
          mxx2 = rmp2(p20)
          s2  = p12
          s1  = p01
          m2  = m12
          call Ccoll2_dd(mxx2,s1,s2,M2,C0,C1,C2,C3,C4,crelacc)
          call Cswap_dd(C0,C1,C2,C3,C4,1,2,4)   
c*** C0(s1,mxx2,s2,M2,mxx2,0) for mxx2 -> 0
        elseif ((m22.eq.(0d0,0d0)).and.(dreal(m12).eq.p12)) then
          mxx2 = rmp2(p12)
          s2  = p20
          s1  = p01
          m2  = m02
          call Ccoll3_dd(mxx2,s1,s2,M2,C0,C1,C2,C3,C4,crelacc)
c*** C0(s2,mxx2,s1,M2,0,mxx2) for mxx2 -> 0
        elseif ((m12.eq.(0d0,0d0)).and.(dreal(m22).eq.p12)) then
          mxx2 = rmp2(p12)
          s1  = p20
          s2  = p01
          m2  = m02
          call Ccoll3_dd(mxx2,s1,s2,M2,C0,C1,C2,C3,C4,crelacc)
          call Cswap_dd(C0,C1,C2,C3,C4,1,2,4)   
        endif

c translation to new conventions
c-------------------------------
        C(0,0,0) = C0
        C(0,1,0) = C1(1)
        C(0,0,1) = C1(2)
        C(2,0,0) = C2(0,0)
        C(0,2,0) = C2(1,1)
        C(0,1,1) = C2(1,2)
        C(0,0,2) = C2(2,2)
        C(2,1,0) = C3(0,0,1)
        C(2,0,1) = C3(0,0,2)
        C(0,3,0) = C3(1,1,1)
        C(0,2,1) = C3(1,1,2)
        C(0,1,2) = C3(1,2,2)
        C(0,0,3) = C3(2,2,2)
        C(4,0,0) = C4(0,0,0,0)
        C(2,2,0) = C4(0,0,1,1)
        C(2,1,1) = C4(0,0,1,2)
        C(2,0,2) = C4(0,0,2,2)
        C(0,4,0) = C4(1,1,1,1)
        C(0,3,1) = C4(1,1,1,2)
        C(0,2,2) = C4(1,1,2,2)
        C(0,1,3) = C4(1,2,2,2)
        C(0,0,4) = C4(2,2,2,2)

        do i12=0,4
        do i1=0,i12
          i2 = i12-i1
          Cuv(0,i1,i2) = 0d0
        enddo
        enddo

        Cuv(2,0,0) =  1d0/4d0
        Cuv(2,1,0) = -1d0/12d0
        Cuv(2,0,1) = -1d0/12d0
        Cuv(2,2,0) =  1d0/24d0
        Cuv(2,1,1) =  1d0/48d0
        Cuv(2,0,2) =  1d0/24d0
        Cuv(4,0,0) = (-p01-p20-p12+4d0*(m02+m12+m22))/96d0

        end

**********************************************************************
        subroutine BCfin_dd(s,ds,m12,M2,Logm12,xLogM2,Logmu2,C0,
     &                   fb0,fc0,dfb0,dfc0)
**********************************************************************
*       Scalar integrals
*         B0(s+ds,0,M2)
*         C0(m12,s,s+ds,0,m12,M2) for m12 -> 0
*       with first n parts of Taylor expansion for ds->0 subtracted,
*       exact calculation, numerically stable for s1 -> s2
*---------------------------------------------------------------------
*       6.4.05 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        integer nbcmax
        parameter(nbcmax=20)
        complex*16 ieps,M2,dxsm,logm12,xlogm2,logmu2,C0,B0
        complex*16 kb(0:nbcmax),kc(0:nbcmax),fb0(0:5),fc0(0:5)
        complex*16 dfb0(0:5),dfc0(0:5)
        integer i,j

        dxs  = 0d0
        dxsm = 0d0

        ieps   = dcmplx(0d0,1d-20)

        if ((s.ne.0d0).and.(ds.ne.-s)) then

        dxs   = ds/s 
        dxsm  = ds/(s-M2)
        Logm12 = Log(m12/(M2-s)+abs(m12/(M2-s))*ieps) 
        if (m2.eq.(0d0,0d0)) then
          xLogM2 = 0d0
        else
          xLogM2 = M2/(M2-s)*Log(M2/(M2-s)+abs(M2/(M2-s))*ieps)
        endif
        Logmu2 = Log(muv2/(M2-s)+abs(muv2/(M2-s))*ieps) 

        KB(0) = 2D0+LOGMU2-((M2-S)*XLOGM2)/S
        KB(1) = -(DXS)+(DXS*(M2-S)*XLOGM2)/S
        KB(2) = (DXS*(2D0*DXS-DXSM))/2D0-(DXS**2*(M2-S)*XLOGM2)/S
        KB(3) = -((DXS*(6D0*DXS**2-3D0*DXS*DXSM-DXSM**2))/6D0)+
     &  (DXS**3*(M2-S)*XLOGM2)/S
        KB(4) = (DXS*(12D0*DXS**3-6D0*DXS**2*DXSM-2D0*DXS*DXSM**2-
     &  DXSM**3))/12D0-(DXS**4*(M2-S)*XLOGM2)/S
        KB(5) = -((DXS*(60D0*DXS**4-30D0*DXS**3*DXSM-10D0*DXS**2*
     &  DXSM**2-5D0*DXS*DXSM**3-3D0*DXSM**4))/60D0)+(DXS**5*(M2-S)*
     &  XLOGM2)/S

        KC(0) = -(2D0/(M2-S))+LOGM12/(M2-S)+XLOGM2/S
        KC(1) = (DXS)/(2D0*(M2-S))-(DXS*S*(M2-S)**(-2))/2D0+(DXS*
     &  LOGM12*S*(M2-S)**(-2))/2D0-(DXS*XLOGM2)/(2D0*S)+(DXS*
     &  XLOGM2)/(2D0*(M2-S))
        KC(2) = (DXSM*(2D0*DXS+DXSM))/(6D0*S)-(2D0*DXSM**2)/(9D0*
     &  (M2-S))+(DXSM**2*LOGM12)/(3D0*(M2-S))+((DXS**2+DXS*DXSM+
     &  DXSM**2)*XLOGM2)/(3D0*S)
        KC(3) = -((DXSM*(6D0*DXS**2+3D0*DXS*DXSM+2D0*DXSM**2))/
     &  (24D0*S))+(DXSM**3)/(8D0*(M2-S))-(DXSM**3*LOGM12)/(4D0*(M2-
     &  S))-((DXS+DXSM)*(DXS**2+DXSM**2)*XLOGM2)/(4D0*S)
        KC(4) = (DXSM*(12D0*DXS**3+6D0*DXS**2*DXSM+4D0*DXS*DXSM**2+
     &  3D0*DXSM**3))/(60D0*S)-(2D0*DXSM**4)/(25D0*(M2-S))+(DXSM**4*
     &  LOGM12)/(5D0*(M2-S))+((DXS**4+DXS**3*DXSM+DXS**2*DXSM**2+
     &  DXS*DXSM**3+DXSM**4)*XLOGM2)/(5D0*S)
        KC(5) = -((DXSM*(60D0*DXS**4+30D0*DXS**3*DXSM+20D0*DXS**2*
     &  DXSM**2+15D0*DXS*DXSM**3+12D0*DXSM**4))/(360D0*S))+(DXSM**
     &  5)/(18D0*(M2-S))-(DXSM**5*LOGM12)/(6D0*(M2-S))-((DXS+DXSM)*
     &  (DXS**2+DXS*DXSM+DXSM**2)*(DXS**2-DXS*DXSM+DXSM**2)*XLOGM2)/
     &  (6D0*S)

        elseif (s.eq.0d0) then

          dxs    = 0d0
          dxsm   = ds/(-M2)
          Logm12 = Log(m12/M2) 
          xLogM2 = 0d0
          Logmu2 = Log(muv2/M2) 

          KB(0) = 1D0+LOGMU2
          KB(1) = -(DXSM)/2D0
          KB(2) = (DXSM**2)/6D0
          KB(3) = -((DXSM**3)/12D0)
          KB(4) = (DXSM**4)/20D0
          KB(5) = -((DXSM**5)/30D0)

          KC(0) = -(1D0/M2)+LOGM12/M2
          KC(1) = (DXSM)/(4D0*M2)-(DXSM*LOGM12)/(2D0*M2)
          KC(2) = -((DXSM**2)/(9D0*M2))+(DXSM**2*LOGM12)/(3D0*M2)
          KC(3) = (DXSM**3)/(16D0*M2)-(DXSM**3*LOGM12)/(4D0*M2)
          KC(4) = -((DXSM**4)/(25D0*M2))+(DXSM**4*LOGM12)/(5D0*M2)
          KC(5) = (DXSM**5)/(36D0*M2)-(DXSM**5*LOGM12)/(6D0*M2)

        elseif (ds.eq.-s) then

          dxs    = 0d0
          dxsm   = ds/(-M2)
          Logm12 = Log(m12/M2) 
          xLogM2 = 0d0
          Logmu2 = Log(muv2/M2) 

          KB(0) = 1D0+LOGMU2
          KB(1) = (DXSM)/2D0
          KB(2) = (DXSM**2)/6D0
          KB(3) = (DXSM**3)/12D0
          KB(4) = (DXSM**4)/20D0
          KB(5) = (DXSM**5)/30D0

          KC(0) = -(1D0/M2)+LOGM12/M2
          KC(1) = (3D0*DXSM)/(4D0*M2)+(DXSM*LOGM12)/(2D0*M2)
          KC(2) = (8D0*DXSM**2)/(9D0*M2)+(DXSM**2*LOGM12)/(3D0*M2)
          KC(3) = (41D0*DXSM**3)/(48D0*M2)+(DXSM**3*LOGM12)/(4D0*M2)
          KC(4) = (119D0*DXSM**4)/(150D0*M2)+(DXSM**4*LOGM12)/(5D0*M2)
          KC(5) = (11D0*DXSM**5)/(15D0*M2)+(DXSM**5*LOGM12)/(6D0*M2)

        endif

        if ((abs(dxs).gt.0.1d0).or.(abs(dxsm).gt.0.1d0)) then
c*** calculation of fb0(i) and fc0(i) from full B0 or C0 functions
c*** used only for checking purposes
c       if (1d0.lt.0d0) then

          B0 = 2D0+Log(muv2/(M2-s-ds)+abs(muv2/(M2-s-ds))*ieps)
          if (M2.ne.(0d0,0d0)) 
     &      B0 = B0-M2/(S+ds)*Log(M2/(M2-s-ds)+abs(M2/(M2-s-ds))*ieps)
          do i=0,5
            fb0(i) = B0 
            fc0(i) = C0 
            do j=0,i
              fb0(i) = (fb0(i) - kb(j))
              fc0(i) = (fc0(i) - kc(j))
            enddo
            fb0(i) = fb0(i)/ds**i
            fc0(i) = fc0(i)/ds**i
          enddo

        else

        if ((s.ne.0d0).and.(ds.ne.-s)) then
        
        KB(6) = (DXS*(60D0*DXS**5-30D0*DXS**4*DXSM-10D0*DXS**3*
     &  DXSM**2-5D0*DXS**2*DXSM**3-3D0*DXS*DXSM**4-2D0*DXSM**5))/
     &  60D0-(DXS**6*(M2-S)*XLOGM2)/S
        KB(7) = -((DXS*(420D0*DXS**6-210D0*DXS**5*DXSM-70D0*DXS**4*
     &  DXSM**2-35D0*DXS**3*DXSM**3-21D0*DXS**2*DXSM**4-14D0*DXS*
     &  DXSM**5-10D0*DXSM**6))/420D0)+(DXS**7*(M2-S)*XLOGM2)/S
        KB(8) = (DXS*(840D0*DXS**7-420D0*DXS**6*DXSM-140D0*DXS**5*
     &  DXSM**2-70D0*DXS**4*DXSM**3-42D0*DXS**3*DXSM**4-28D0*DXS**2*
     &  DXSM**5-20D0*DXS*DXSM**6-15D0*DXSM**7))/840D0-(DXS**8*(M2-
     &  S)*XLOGM2)/S
        KB(9) = -((DXS*(2520D0*DXS**8-1260D0*DXS**7*DXSM-420D0*DXS**
     &  6*DXSM**2-210D0*DXS**5*DXSM**3-126D0*DXS**4*DXSM**4-84D0*
     &  DXS**3*DXSM**5-60D0*DXS**2*DXSM**6-45D0*DXS*DXSM**7-35D0*
     &  DXSM**8))/2520D0)+(DXS**9*(M2-S)*XLOGM2)/S
        KB(10) = (DXS*(2520D0*DXS**9-1260D0*DXS**8*DXSM-420D0*DXS**
     &  7*DXSM**2-210D0*DXS**6*DXSM**3-126D0*DXS**5*DXSM**4-84D0*
     &  DXS**4*DXSM**5-60D0*DXS**3*DXSM**6-45D0*DXS**2*DXSM**7-35D0*
     &  DXS*DXSM**8-28D0*DXSM**9))/2520D0-(DXS**10*(M2-S)*XLOGM2)/S
        KB(11) = -((DXS*(27720D0*DXS**10-13860D0*DXS**9*DXSM-252D0*
     &  DXSM**10-4620D0*DXS**8*DXSM**2-2310D0*DXS**7*DXSM**3-1386D0*
     &  DXS**6*DXSM**4-924D0*DXS**5*DXSM**5-660D0*DXS**4*DXSM**6-
     &  495D0*DXS**3*DXSM**7-385D0*DXS**2*DXSM**8-308D0*DXS*DXSM**
     &  9))/27720D0)+(DXS**11*(M2-S)*XLOGM2)/S
        KB(12) = (DXS*(27720D0*DXS**11-13860D0*DXS**10*DXSM-252D0*
     &  DXS*DXSM**10-210D0*DXSM**11-4620D0*DXS**9*DXSM**2-2310D0*
     &  DXS**8*DXSM**3-1386D0*DXS**7*DXSM**4-924D0*DXS**6*DXSM**5-
     &  660D0*DXS**5*DXSM**6-495D0*DXS**4*DXSM**7-385D0*DXS**3*
     &  DXSM**8-308D0*DXS**2*DXSM**9))/27720D0-(DXS**12*(M2-S)*
     &  XLOGM2)/S
        KB(13) = -((DXS*(360360D0*DXS**12-180180D0*DXS**11*DXSM-
     &  3276D0*DXS**2*DXSM**10-2730D0*DXS*DXSM**11-2310D0*DXSM**12-
     &  60060D0*DXS**10*DXSM**2-30030D0*DXS**9*DXSM**3-18018D0*DXS**
     &  8*DXSM**4-12012D0*DXS**7*DXSM**5-8580D0*DXS**6*DXSM**6-
     &  6435D0*DXS**5*DXSM**7-5005D0*DXS**4*DXSM**8-4004D0*DXS**3*
     &  DXSM**9))/360360D0)+(DXS**13*(M2-S)*XLOGM2)/S
        KB(14) = (DXS*(360360D0*DXS**13-180180D0*DXS**12*DXSM-
     &  3276D0*DXS**3*DXSM**10-2730D0*DXS**2*DXSM**11-2310D0*DXS*
     &  DXSM**12-1980D0*DXSM**13-60060D0*DXS**11*DXSM**2-30030D0*
     &  DXS**10*DXSM**3-18018D0*DXS**9*DXSM**4-12012D0*DXS**8*DXSM**
     &  5-8580D0*DXS**7*DXSM**6-6435D0*DXS**6*DXSM**7-5005D0*DXS**5*
     &  DXSM**8-4004D0*DXS**4*DXSM**9))/360360D0-(DXS**14*(M2-S)*
     &  XLOGM2)/S
        KB(15) = -((DXS*(360360D0*DXS**14-180180D0*DXS**13*DXSM-
     &  3276D0*DXS**4*DXSM**10-2730D0*DXS**3*DXSM**11-2310D0*DXS**2*
     &  DXSM**12-1980D0*DXS*DXSM**13-1716D0*DXSM**14-60060D0*DXS**
     &  12*DXSM**2-30030D0*DXS**11*DXSM**3-18018D0*DXS**10*DXSM**4-
     &  12012D0*DXS**9*DXSM**5-8580D0*DXS**8*DXSM**6-6435D0*DXS**7*
     &  DXSM**7-5005D0*DXS**6*DXSM**8-4004D0*DXS**5*DXSM**9))/
     &  360360D0)+(DXS**15*(M2-S)*XLOGM2)/S
        KB(16) = (DXS*(720720D0*DXS**15-360360D0*DXS**14*DXSM-
     &  6552D0*DXS**5*DXSM**10-5460D0*DXS**4*DXSM**11-4620D0*DXS**3*
     &  DXSM**12-3960D0*DXS**2*DXSM**13-3432D0*DXS*DXSM**14-3003D0*
     &  DXSM**15-120120D0*DXS**13*DXSM**2-60060D0*DXS**12*DXSM**3-
     &  36036D0*DXS**11*DXSM**4-24024D0*DXS**10*DXSM**5-17160D0*
     &  DXS**9*DXSM**6-12870D0*DXS**8*DXSM**7-10010D0*DXS**7*DXSM**
     &  8-8008D0*DXS**6*DXSM**9))/720720D0-(DXS**16*(M2-S)*XLOGM2)/S
        KB(17) = -((DXS*(12252240D0*DXS**16-6126120D0*DXS**15*DXSM-
     &  111384D0*DXS**6*DXSM**10-92820D0*DXS**5*DXSM**11-78540D0*
     &  DXS**4*DXSM**12-67320D0*DXS**3*DXSM**13-58344D0*DXS**2*
     &  DXSM**14-51051D0*DXS*DXSM**15-45045D0*DXSM**16-2042040D0*
     &  DXS**14*DXSM**2-1021020D0*DXS**13*DXSM**3-612612D0*DXS**12*
     &  DXSM**4-408408D0*DXS**11*DXSM**5-291720D0*DXS**10*DXSM**6-
     &  218790D0*DXS**9*DXSM**7-170170D0*DXS**8*DXSM**8-136136D0*
     &  DXS**7*DXSM**9))/12252240D0)+(DXS**17*(M2-S)*XLOGM2)/S
        KB(18) = (DXS*(12252240D0*DXS**17-6126120D0*DXS**16*DXSM-
     &  111384D0*DXS**7*DXSM**10-92820D0*DXS**6*DXSM**11-78540D0*
     &  DXS**5*DXSM**12-67320D0*DXS**4*DXSM**13-58344D0*DXS**3*
     &  DXSM**14-51051D0*DXS**2*DXSM**15-45045D0*DXS*DXSM**16-
     &  40040D0*DXSM**17-2042040D0*DXS**15*DXSM**2-1021020D0*DXS**
     &  14*DXSM**3-612612D0*DXS**13*DXSM**4-408408D0*DXS**12*DXSM**
     &  5-291720D0*DXS**11*DXSM**6-218790D0*DXS**10*DXSM**7-
     &  170170D0*DXS**9*DXSM**8-136136D0*DXS**8*DXSM**9))/
     &  12252240D0-(DXS**18*(M2-S)*XLOGM2)/S
        KB(19) = -((DXS*(232792560D0*DXS**18-116396280D0*DXS**17*
     &  DXSM-2116296D0*DXS**8*DXSM**10-1763580D0*DXS**7*DXSM**11-
     &  1492260D0*DXS**6*DXSM**12-1279080D0*DXS**5*DXSM**13-
     &  1108536D0*DXS**4*DXSM**14-969969D0*DXS**3*DXSM**15-855855D0*
     &  DXS**2*DXSM**16-760760D0*DXS*DXSM**17-680680D0*DXSM**18-
     &  38798760D0*DXS**16*DXSM**2-19399380D0*DXS**15*DXSM**3-
     &  11639628D0*DXS**14*DXSM**4-7759752D0*DXS**13*DXSM**5-
     &  5542680D0*DXS**12*DXSM**6-4157010D0*DXS**11*DXSM**7-
     &  3233230D0*DXS**10*DXSM**8-2586584D0*DXS**9*DXSM**9))/
     &  232792560D0)+(DXS**19*(M2-S)*XLOGM2)/S
        KB(20) = (DXS*(232792560D0*DXS**19-116396280D0*DXS**18*DXSM-
     &  2116296D0*DXS**9*DXSM**10-1763580D0*DXS**8*DXSM**11-
     &  1492260D0*DXS**7*DXSM**12-1279080D0*DXS**6*DXSM**13-
     &  1108536D0*DXS**5*DXSM**14-969969D0*DXS**4*DXSM**15-855855D0*
     &  DXS**3*DXSM**16-760760D0*DXS**2*DXSM**17-680680D0*DXS*DXSM**
     &  18-612612D0*DXSM**19-38798760D0*DXS**17*DXSM**2-19399380D0*
     &  DXS**16*DXSM**3-11639628D0*DXS**15*DXSM**4-7759752D0*DXS**
     &  14*DXSM**5-5542680D0*DXS**13*DXSM**6-4157010D0*DXS**12*
     &  DXSM**7-3233230D0*DXS**11*DXSM**8-2586584D0*DXS**10*DXSM**
     &  9))/232792560D0-(DXS**20*(M2-S)*XLOGM2)/S

        KC(6) = (DXSM*(60D0*DXS**5+30D0*DXS**4*DXSM+20D0*DXS**3*
     &  DXSM**2+15D0*DXS**2*DXSM**3+12D0*DXS*DXSM**4+10D0*DXSM**5))/
     &  (420D0*S)-(2D0*DXSM**6)/(49D0*(M2-S))+(DXSM**6*LOGM12)/(7D0*
     &  (M2-S))+((DXS**6+DXS**5*DXSM+DXS**4*DXSM**2+DXS**3*DXSM**3+
     &  DXS**2*DXSM**4+DXS*DXSM**5+DXSM**6)*XLOGM2)/(7D0*S)
        KC(7) = -((DXSM*(420D0*DXS**6+210D0*DXS**5*DXSM+140D0*DXS**
     &  4*DXSM**2+105D0*DXS**3*DXSM**3+84D0*DXS**2*DXSM**4+70D0*DXS*
     &  DXSM**5+60D0*DXSM**6))/(3360D0*S))+(DXSM**7)/(32D0*(M2-S))-
     &  (DXSM**7*LOGM12)/(8D0*(M2-S))-((DXS+DXSM)*(DXS**2+DXSM**2)*
     &  (DXS**4+DXSM**4)*XLOGM2)/(8D0*S)
        KC(8) = (DXSM*(840D0*DXS**7+420D0*DXS**6*DXSM+280D0*DXS**5*
     &  DXSM**2+210D0*DXS**4*DXSM**3+168D0*DXS**3*DXSM**4+140D0*
     &  DXS**2*DXSM**5+120D0*DXS*DXSM**6+105D0*DXSM**7))/(7560D0*S)-
     &  (2D0*DXSM**8)/(81D0*(M2-S))+(DXSM**8*LOGM12)/(9D0*(M2-S))+
     &  ((DXS**2+DXS*DXSM+DXSM**2)*(DXS**6+DXS**3*DXSM**3+DXSM**6)*
     &  XLOGM2)/(9D0*S)
        KC(9) = -((DXSM*(2520D0*DXS**8+1260D0*DXS**7*DXSM+840D0*
     &  DXS**6*DXSM**2+630D0*DXS**5*DXSM**3+504D0*DXS**4*DXSM**4+
     &  420D0*DXS**3*DXSM**5+360D0*DXS**2*DXSM**6+315D0*DXS*DXSM**7+
     &  280D0*DXSM**8))/(25200D0*S))+(DXSM**9)/(50D0*(M2-S))-(DXSM**
     &  9*LOGM12)/(10D0*(M2-S))-((DXS+DXSM)*(DXS**4+DXS**3*DXSM+
     &  DXS**2*DXSM**2+DXS*DXSM**3+DXSM**4)*(DXS**4-DXS**3*DXSM+
     &  DXS**2*DXSM**2-DXS*DXSM**3+DXSM**4)*XLOGM2)/(10D0*S)
        KC(10) = (DXSM*(2520D0*DXS**9+1260D0*DXS**8*DXSM+840D0*DXS**
     &  7*DXSM**2+630D0*DXS**6*DXSM**3+504D0*DXS**5*DXSM**4+420D0*
     &  DXS**4*DXSM**5+360D0*DXS**3*DXSM**6+315D0*DXS**2*DXSM**7+
     &  280D0*DXS*DXSM**8+252D0*DXSM**9))/(27720D0*S)-(2D0*DXSM**
     &  10)/(121D0*(M2-S))+(DXSM**10*LOGM12)/(11D0*(M2-S))+((DXS**
     &  10+DXS**9*DXSM+DXSM**10+DXS**8*DXSM**2+DXS**7*DXSM**3+DXS**
     &  6*DXSM**4+DXS**5*DXSM**5+DXS**4*DXSM**6+DXS**3*DXSM**7+DXS**
     &  2*DXSM**8+DXS*DXSM**9)*XLOGM2)/(11D0*S)
        KC(11) = -((DXSM*(27720D0*DXS**10+13860D0*DXS**9*DXSM+
     &  2520D0*DXSM**10+9240D0*DXS**8*DXSM**2+6930D0*DXS**7*DXSM**3+
     &  5544D0*DXS**6*DXSM**4+4620D0*DXS**5*DXSM**5+3960D0*DXS**4*
     &  DXSM**6+3465D0*DXS**3*DXSM**7+3080D0*DXS**2*DXSM**8+2772D0*
     &  DXS*DXSM**9))/(332640D0*S))+(DXSM**11)/(72D0*(M2-S))-(DXSM**
     &  11*LOGM12)/(12D0*(M2-S))-((DXS+DXSM)*(DXS**2+DXSM**2)*(DXS**
     &  2+DXS*DXSM+DXSM**2)*(DXS**2-DXS*DXSM+DXSM**2)*(DXS**4-DXS**
     &  2*DXSM**2+DXSM**4)*XLOGM2)/(12D0*S)
        KC(12) = (DXSM*(27720D0*DXS**11+13860D0*DXS**10*DXSM+2520D0*
     &  DXS*DXSM**10+2310D0*DXSM**11+9240D0*DXS**9*DXSM**2+6930D0*
     &  DXS**8*DXSM**3+5544D0*DXS**7*DXSM**4+4620D0*DXS**6*DXSM**5+
     &  3960D0*DXS**5*DXSM**6+3465D0*DXS**4*DXSM**7+3080D0*DXS**3*
     &  DXSM**8+2772D0*DXS**2*DXSM**9))/(360360D0*S)-(2D0*DXSM**12)/
     &  (169D0*(M2-S))+(DXSM**12*LOGM12)/(13D0*(M2-S))+((DXS**12+
     &  DXS**11*DXSM+DXS**2*DXSM**10+DXS*DXSM**11+DXSM**12+DXS**10*
     &  DXSM**2+DXS**9*DXSM**3+DXS**8*DXSM**4+DXS**7*DXSM**5+DXS**6*
     &  DXSM**6+DXS**5*DXSM**7+DXS**4*DXSM**8+DXS**3*DXSM**9)*
     &  XLOGM2)/(13D0*S)
        KC(13) = -((DXSM*(360360D0*DXS**12+180180D0*DXS**11*DXSM+
     &  32760D0*DXS**2*DXSM**10+30030D0*DXS*DXSM**11+27720D0*DXSM**
     &  12+120120D0*DXS**10*DXSM**2+90090D0*DXS**9*DXSM**3+72072D0*
     &  DXS**8*DXSM**4+60060D0*DXS**7*DXSM**5+51480D0*DXS**6*DXSM**
     &  6+45045D0*DXS**5*DXSM**7+40040D0*DXS**4*DXSM**8+36036D0*
     &  DXS**3*DXSM**9))/(5045040D0*S))+(DXSM**13)/(98D0*(M2-S))-
     &  (DXSM**13*LOGM12)/(14D0*(M2-S))-((DXS+DXSM)*(DXS**6+DXS**5*
     &  DXSM+DXS**4*DXSM**2+DXS**3*DXSM**3+DXS**2*DXSM**4+DXS*DXSM**
     &  5+DXSM**6)*(DXS**6-DXS**5*DXSM+DXS**4*DXSM**2-DXS**3*DXSM**
     &  3+DXS**2*DXSM**4-DXS*DXSM**5+DXSM**6)*XLOGM2)/(14D0*S)
        KC(14) = (DXSM*(360360D0*DXS**13+180180D0*DXS**12*DXSM+
     &  32760D0*DXS**3*DXSM**10+30030D0*DXS**2*DXSM**11+27720D0*DXS*
     &  DXSM**12+25740D0*DXSM**13+120120D0*DXS**11*DXSM**2+90090D0*
     &  DXS**10*DXSM**3+72072D0*DXS**9*DXSM**4+60060D0*DXS**8*DXSM**
     &  5+51480D0*DXS**7*DXSM**6+45045D0*DXS**6*DXSM**7+40040D0*
     &  DXS**5*DXSM**8+36036D0*DXS**4*DXSM**9))/(5405400D0*S)-(2D0*
     &  DXSM**14)/(225D0*(M2-S))+(DXSM**14*LOGM12)/(15D0*(M2-S))+
     &  ((DXS**2+DXS*DXSM+DXSM**2)*(DXS**4+DXS**3*DXSM+DXS**2*DXSM**
     &  2+DXS*DXSM**3+DXSM**4)*(DXS**8-DXS**7*DXSM+DXS**5*DXSM**3-
     &  DXS**4*DXSM**4+DXS**3*DXSM**5-DXS*DXSM**7+DXSM**8)*XLOGM2)/
     &  (15D0*S)
        KC(15) = -((DXSM*(360360D0*DXS**14+180180D0*DXS**13*DXSM+
     &  32760D0*DXS**4*DXSM**10+30030D0*DXS**3*DXSM**11+27720D0*
     &  DXS**2*DXSM**12+25740D0*DXS*DXSM**13+24024D0*DXSM**14+
     &  120120D0*DXS**12*DXSM**2+90090D0*DXS**11*DXSM**3+72072D0*
     &  DXS**10*DXSM**4+60060D0*DXS**9*DXSM**5+51480D0*DXS**8*DXSM**
     &  6+45045D0*DXS**7*DXSM**7+40040D0*DXS**6*DXSM**8+36036D0*
     &  DXS**5*DXSM**9))/(5765760D0*S))+(DXSM**15)/(128D0*(M2-S))-
     &  (DXSM**15*LOGM12)/(16D0*(M2-S))-((DXS+DXSM)*(DXS**2+DXSM**
     &  2)*(DXS**4+DXSM**4)*(DXS**8+DXSM**8)*XLOGM2)/(16D0*S)
        KC(16) = (DXSM*(720720D0*DXS**15+360360D0*DXS**14*DXSM+
     &  65520D0*DXS**5*DXSM**10+60060D0*DXS**4*DXSM**11+55440D0*
     &  DXS**3*DXSM**12+51480D0*DXS**2*DXSM**13+48048D0*DXS*DXSM**
     &  14+45045D0*DXSM**15+240240D0*DXS**13*DXSM**2+180180D0*DXS**
     &  12*DXSM**3+144144D0*DXS**11*DXSM**4+120120D0*DXS**10*DXSM**
     &  5+102960D0*DXS**9*DXSM**6+90090D0*DXS**8*DXSM**7+80080D0*
     &  DXS**7*DXSM**8+72072D0*DXS**6*DXSM**9))/(12252240D0*S)-(2D0*
     &  DXSM**16)/(289D0*(M2-S))+(DXSM**16*LOGM12)/(17D0*(M2-S))+
     &  ((DXS**16+DXS**15*DXSM+DXS**6*DXSM**10+DXS**5*DXSM**11+DXS**
     &  4*DXSM**12+DXS**3*DXSM**13+DXS**2*DXSM**14+DXS*DXSM**15+
     &  DXSM**16+DXS**14*DXSM**2+DXS**13*DXSM**3+DXS**12*DXSM**4+
     &  DXS**11*DXSM**5+DXS**10*DXSM**6+DXS**9*DXSM**7+DXS**8*DXSM**
     &  8+DXS**7*DXSM**9)*XLOGM2)/(17D0*S)
        KC(17) = -((DXSM*(12252240D0*DXS**16+6126120D0*DXS**15*DXSM+
     &  1113840D0*DXS**6*DXSM**10+1021020D0*DXS**5*DXSM**11+
     &  942480D0*DXS**4*DXSM**12+875160D0*DXS**3*DXSM**13+816816D0*
     &  DXS**2*DXSM**14+765765D0*DXS*DXSM**15+720720D0*DXSM**16+
     &  4084080D0*DXS**14*DXSM**2+3063060D0*DXS**13*DXSM**3+
     &  2450448D0*DXS**12*DXSM**4+2042040D0*DXS**11*DXSM**5+
     &  1750320D0*DXS**10*DXSM**6+1531530D0*DXS**9*DXSM**7+
     &  1361360D0*DXS**8*DXSM**8+1225224D0*DXS**7*DXSM**9))/
     &  (220540320D0*S))+(DXSM**17)/(162D0*(M2-S))-(DXSM**17*
     &  LOGM12)/(18D0*(M2-S))-((DXS+DXSM)*(DXS**2+DXS*DXSM+DXSM**2)*
     &  (DXS**2-DXS*DXSM+DXSM**2)*(DXS**6+DXS**3*DXSM**3+DXSM**6)*
     &  (DXS**6-DXS**3*DXSM**3+DXSM**6)*XLOGM2)/(18D0*S)
        KC(18) = (DXSM*(12252240D0*DXS**17+6126120D0*DXS**16*DXSM+
     &  1113840D0*DXS**7*DXSM**10+1021020D0*DXS**6*DXSM**11+
     &  942480D0*DXS**5*DXSM**12+875160D0*DXS**4*DXSM**13+816816D0*
     &  DXS**3*DXSM**14+765765D0*DXS**2*DXSM**15+720720D0*DXS*DXSM**
     &  16+680680D0*DXSM**17+4084080D0*DXS**15*DXSM**2+3063060D0*
     &  DXS**14*DXSM**3+2450448D0*DXS**13*DXSM**4+2042040D0*DXS**12*
     &  DXSM**5+1750320D0*DXS**11*DXSM**6+1531530D0*DXS**10*DXSM**7+
     &  1361360D0*DXS**9*DXSM**8+1225224D0*DXS**8*DXSM**9))/
     &  (232792560D0*S)-(2D0*DXSM**18)/(361D0*(M2-S))+(DXSM**18*
     &  LOGM12)/(19D0*(M2-S))+((DXS**18+DXS**17*DXSM+DXS**8*DXSM**
     &  10+DXS**7*DXSM**11+DXS**6*DXSM**12+DXS**5*DXSM**13+DXS**4*
     &  DXSM**14+DXS**3*DXSM**15+DXS**2*DXSM**16+DXS*DXSM**17+DXSM**
     &  18+DXS**16*DXSM**2+DXS**15*DXSM**3+DXS**14*DXSM**4+DXS**13*
     &  DXSM**5+DXS**12*DXSM**6+DXS**11*DXSM**7+DXS**10*DXSM**8+
     &  DXS**9*DXSM**9)*XLOGM2)/(19D0*S)
        KC(19) = -((DXSM*(232792560D0*DXS**18+116396280D0*DXS**17*
     &  DXSM+21162960D0*DXS**8*DXSM**10+19399380D0*DXS**7*DXSM**11+
     &  17907120D0*DXS**6*DXSM**12+16628040D0*DXS**5*DXSM**13+
     &  15519504D0*DXS**4*DXSM**14+14549535D0*DXS**3*DXSM**15+
     &  13693680D0*DXS**2*DXSM**16+12932920D0*DXS*DXSM**17+
     &  12252240D0*DXSM**18+77597520D0*DXS**16*DXSM**2+58198140D0*
     &  DXS**15*DXSM**3+46558512D0*DXS**14*DXSM**4+38798760D0*DXS**
     &  13*DXSM**5+33256080D0*DXS**12*DXSM**6+29099070D0*DXS**11*
     &  DXSM**7+25865840D0*DXS**10*DXSM**8+23279256D0*DXS**9*DXSM**
     &  9))/(4655851200D0*S))+(DXSM**19)/(200D0*(M2-S))-(DXSM**19*
     &  LOGM12)/(20D0*(M2-S))-((DXS+DXSM)*(DXS**2+DXSM**2)*(DXS**4+
     &  DXS**3*DXSM+DXS**2*DXSM**2+DXS*DXSM**3+DXSM**4)*(DXS**4-
     &  DXS**3*DXSM+DXS**2*DXSM**2-DXS*DXSM**3+DXSM**4)*(DXS**8-
     &  DXS**6*DXSM**2+DXS**4*DXSM**4-DXS**2*DXSM**6+DXSM**8)*
     &  XLOGM2)/(20D0*S)
        KC(20) = (DXSM*(232792560D0*DXS**19+116396280D0*DXS**18*
     &  DXSM+21162960D0*DXS**9*DXSM**10+19399380D0*DXS**8*DXSM**11+
     &  17907120D0*DXS**7*DXSM**12+16628040D0*DXS**6*DXSM**13+
     &  15519504D0*DXS**5*DXSM**14+14549535D0*DXS**4*DXSM**15+
     &  13693680D0*DXS**3*DXSM**16+12932920D0*DXS**2*DXSM**17+
     &  12252240D0*DXS*DXSM**18+11639628D0*DXSM**19+77597520D0*DXS**
     &  17*DXSM**2+58198140D0*DXS**16*DXSM**3+46558512D0*DXS**15*
     &  DXSM**4+38798760D0*DXS**14*DXSM**5+33256080D0*DXS**13*DXSM**
     &  6+29099070D0*DXS**12*DXSM**7+25865840D0*DXS**11*DXSM**8+
     &  23279256D0*DXS**10*DXSM**9))/(4888643760D0*S)-(2D0*DXSM**
     &  20)/(441D0*(M2-S))+(DXSM**20*LOGM12)/(21D0*(M2-S))+((DXS**2+
     &  DXS*DXSM+DXSM**2)*(DXS**6+DXS**5*DXSM+DXS**4*DXSM**2+DXS**3*
     &  DXSM**3+DXS**2*DXSM**4+DXS*DXSM**5+DXSM**6)*(DXS**12-DXS**
     &  11*DXSM-DXS*DXSM**11+DXSM**12+DXS**9*DXSM**3-DXS**8*DXSM**4+
     &  DXS**6*DXSM**6-DXS**4*DXSM**8+DXS**3*DXSM**9)*XLOGM2)/(21D0*
     &  S)

        elseif (s.eq.0d0) then

          KB(6) = (DXSM**6)/42D0
          KB(7) = -((DXSM**7)/56D0)
          KB(8) = (DXSM**8)/72D0
          KB(9) = -((DXSM**9)/90D0)
          KB(10) = (DXSM**10)/110D0
          KB(11) = -((DXSM**11)/132D0)
          KB(12) = (DXSM**12)/156D0
          KB(13) = -((DXSM**13)/182D0)
          KB(14) = (DXSM**14)/210D0
          KB(15) = -((DXSM**15)/240D0)
          KB(16) = (DXSM**16)/272D0
          KB(17) = -((DXSM**17)/306D0)
          KB(18) = (DXSM**18)/342D0
          KB(19) = -((DXSM**19)/380D0)
          KB(20) = (DXSM**20)/420D0
        
          KC(6) = -((DXSM**6)/(49D0*M2))+(DXSM**6*LOGM12)/(7D0*M2)
          KC(7) = (DXSM**7)/(64D0*M2)-(DXSM**7*LOGM12)/(8D0*M2)
          KC(8) = -((DXSM**8)/(81D0*M2))+(DXSM**8*LOGM12)/(9D0*M2)
          KC(9) = (DXSM**9)/(100D0*M2)-(DXSM**9*LOGM12)/(10D0*M2)
          KC(10) = -((DXSM**10)/(121D0*M2))+(DXSM**10*LOGM12)/(11D0*M2)
          KC(11) = (DXSM**11)/(144D0*M2)-(DXSM**11*LOGM12)/(12D0*M2)
          KC(12) = -((DXSM**12)/(169D0*M2))+(DXSM**12*LOGM12)/(13D0*M2)
          KC(13) = (DXSM**13)/(196D0*M2)-(DXSM**13*LOGM12)/(14D0*M2)
          KC(14) = -((DXSM**14)/(225D0*M2))+(DXSM**14*LOGM12)/(15D0*M2)
          KC(15) = (DXSM**15)/(256D0*M2)-(DXSM**15*LOGM12)/(16D0*M2)
          KC(16) = -((DXSM**16)/(289D0*M2))+(DXSM**16*LOGM12)/(17D0*M2)
          KC(17) = (DXSM**17)/(324D0*M2)-(DXSM**17*LOGM12)/(18D0*M2)
          KC(18) = -((DXSM**18)/(361D0*M2))+(DXSM**18*LOGM12)/(19D0*M2)
          KC(19) = (DXSM**19)/(400D0*M2)-(DXSM**19*LOGM12)/(20D0*M2)
          KC(20) = -((DXSM**20)/(441D0*M2))+(DXSM**20*LOGM12)/(21D0*M2)

        elseif (ds.eq.-s) then
        
          KB(6) = (DXSM**6)/42D0
          KB(7) = (DXSM**7)/56D0
          KB(8) = (DXSM**8)/72D0
          KB(9) = (DXSM**9)/90D0
          KB(10) = (DXSM**10)/110D0
          KB(11) = (DXSM**11)/132D0
          KB(12) = (DXSM**12)/156D0
          KB(13) = (DXSM**13)/182D0
          KB(14) = (DXSM**14)/210D0
          KB(15) = (DXSM**15)/240D0
          KB(16) = (DXSM**16)/272D0
          KB(17) = (DXSM**17)/306D0
          KB(18) = (DXSM**18)/342D0
          KB(19) = (DXSM**19)/380D0
          KB(20) = (DXSM**20)/420D0

          KC(6) = (333D0*DXSM**6)/(490D0*M2)+(DXSM**6*LOGM12)/(7D0*M2)
          KC(7) = (1417D0*DXSM**7)/(2240D0*M2)+DXSM**7*LOGM12/(8D0*M2)
          KC(8) = (6709D0*DXSM**8)/(11340D0*M2)+DXSM**8*LOGM12/(9D0*M2)
          KC(9) = (7003D0*DXSM**9)/(12600D0*M2)+(DXSM**9*LOGM12)/
     &     (10D0*M2)
          KC(10) = (79931D0*DXSM**10)/(152460D0*M2)+(DXSM**10*LOGM12)/
     &     (11D0*M2)
          KC(11) = (20639D0*DXSM**11)/(41580D0*M2)+(DXSM**11*LOGM12)/
     &     (12D0*M2)
          KC(12) = (1104413D0*DXSM**12)/(2342340D0*M2)+(DXSM**12*
     &     LOGM12)/(13D0*M2)
          KC(13) = (1133123D0*DXSM**13)/(2522520D0*M2)+(DXSM**13*
     &     LOGM12)/(14D0*M2)
          KC(14) = (1159721D0*DXSM**14)/(2702700D0*M2)+(DXSM**14*
     &     LOGM12)/(15D0*M2)
          KC(15) = (4737983D0*DXSM**15)/(11531520D0*M2)+(DXSM**15*
     &     LOGM12)/(16D0*M2)
          KC(16) = (41061143D0*DXSM**16)/(104144040D0*M2)+(DXSM**16*
     &     LOGM12)/(17D0*M2)
          KC(17) = (13933961D0*DXSM**17)/(36756720D0*M2)+(DXSM**17*
     &     LOGM12)/(18D0*M2)
          KC(18) = (269169679D0*DXSM**18)/(737176440D0*M2)+(DXSM**18*
     &     LOGM12)/(19D0*M2)
          KC(19) = (273355861D0*DXSM**19)/(775975200D0*M2)+(DXSM**19*
     &     LOGM12)/(20D0*M2)
          KC(20) = (6162847D0*DXSM**20)/(18106088D0*M2)+(DXSM**20*
     &     LOGM12)/(21D0*M2)

        endif

        do i=0,5
          FB0(i) = 0d0
          FC0(i) = 0d0
          do j=i+1,nbcmax
            FB0(i) = FB0(i) + KB(j)
            FC0(i) = FC0(i) + KC(j)
          enddo
          fb0(i)  = fb0(i)/ds**i
          fc0(i)  = fc0(i)/ds**i
          dfb0(i) = kb(nbcmax)/ds**i
          dfc0(i) = kc(nbcmax)/ds**i
        enddo

        C0 = FC0(0)+KC(0)

        endif

        end

**********************************************************************
        subroutine Ccoll1_dd(m12,s1,s2,M2,C0,C1,C2,C3,C4,crelacc)
**********************************************************************
*       C0(m12,s1,s2,0,m12,M2) for m12 -> 0
*       exact calculation, numerically stable for s1 -> s2
*---------------------------------------------------------------------
*       22.3.05 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)
        complex*16 M2,logm12,xlogm2,logmu2
        complex*16 fb0(0:5),fc0(0:5),dfb0(0:5),dfc0(0:5)
        complex*16 C0,C1(2),C2(0:2,0:2),C3(0:2,0:2,2)
        complex*16 C4(0:2,0:2,0:2,0:2)

        dc111 = 0d0
        dc222 = 0d0

        if (abs(s1).gt.1d-15) then
          s = s1
        else
          s = 0d0
        endif
        if (abs(s2).gt.1d-15) then
          ds = s2-s1
        else
          ds = -s1
        endif

        call BCfin_dd(s,ds,m12,M2,Logm12,xLogM2,Logmu2,C0,
     &                   fb0,fc0,dfb0,dfc0)

        if ((s.ne.0d0).and.(ds.ne.-s1)) then

        C1(1) = ((-(4D0*FB0(2)*(DS+S))-2D0*FC0(1)*(DS-M2+S)-LOGM12*
     &  (M2-S)**(-2)*(DS+M2-S)-M2*S**(-2)*(M2-S)**(-2)*(4D0*DS*M2-
     &  5D0*DS*S-M2*S+S**2)+(S**(-3)*(4D0*DS*M2**2-7D0*DS*M2*S-M2**
     &  2*S+2D0*DS*S**2+M2*S**2)*XLOGM2)/(M2-S)))/2D0
        C1(2) = FB0(1)-1D0/S+S**(-2)*(M2-S)*XLOGM2
        C2(0,0) = ((LOGMU2+FB0(1)*(DS-M2+S)-((DS-M2-2D0*S))/S+(DS-
     &  M2)*S**(-2)*(M2-S)*XLOGM2))/4D0
        C2(1,1) = ((54D0*FB0(3)*(DS+S)*(DS-M2+S)+18D0*FC0(2)*(DS-M2+
     &  S)**2+3D0*LOGM12*(M2-S)**(-3)*(2D0*DS**2-DS*M2+2D0*M2**2+DS*
     &  S-4D0*M2*S+2D0*S**2)-S**(-3)*(M2-S)**(-3)*(54D0*DS**2*M2**3-
     &  54D0*DS*M2**4-129D0*DS**2*M2**2*S+177D0*DS*M2**3*S+6D0*M2**
     &  4*S+84D0*DS**2*M2*S**2-201D0*DS*M2**2*S**2-9D0*M2**3*S**2-
     &  5D0*DS**2*S**3+79D0*DS*M2*S**3-5D0*M2**2*S**3-DS*S**4+13D0*
     &  M2*S**4-5D0*S**5)+3D0*S**(-4)*(M2-S)**(-2)*(18D0*DS**2*M2**
     &  3-18D0*DS*M2**4-52D0*DS**2*M2**2*S+68D0*DS*M2**3*S+2D0*M2**
     &  4*S+48D0*DS**2*M2*S**2-95D0*DS*M2**2*S**2-4D0*M2**3*S**2-
     &  12D0*DS**2*S**3+57D0*DS*M2*S**3+2D0*M2**2*S**3-12D0*DS*S**
     &  4)*XLOGM2))/18D0
        C2(1,2) = ((-(2D0*FB0(2)*(DS-M2+S))-(S**(-2)*(2D0*DS*M2-2D0*
     &  M2**2-DS*S+M2*S+S**2))/(M2-S)+2D0*(DS-M2)*S**(-3)*(M2-S)*
     &  XLOGM2))/4D0
        C2(2,2) = -(((FB0(1)*(DS-M2+S)-((DS-2D0*M2+S))/S+S**(-2)*
     &  (M2-S)*(DS-2D0*M2+2D0*S)*XLOGM2))/(2D0*(DS+S)))
        C3(1,1,1) = ((-(264D0*FB0(4)*(DS+S)*(DS-M2+S)**2)-72D0*
     &  FC0(3)*(DS-M2+S)**3-6D0*LOGM12*(M2-S)**(-4)*(3D0*DS**3-5D0*
     &  DS**2*M2+3D0*DS*M2**2+3D0*M2**3+5D0*DS**2*S-6D0*DS*M2*S-9D0*
     &  M2**2*S+3D0*DS*S**2+9D0*M2*S**2-3D0*S**3)-S**(-4)*(M2-S)**(-
     &  4)*(264D0*DS**3*M2**4-528D0*DS**2*M2**5+264D0*DS*M2**6-
     &  906D0*DS**3*M2**3*S+2322D0*DS**2*M2**4*S-1398D0*DS*M2**5*S-
     &  18D0*M2**6*S+1081D0*DS**3*M2**2*S**2-3917D0*DS**2*M2**3*S**
     &  2+3031D0*DS*M2**4*S**2+45D0*M2**5*S**2-472D0*DS**3*M2*S**3+
     &  3049D0*DS**2*M2**2*S**3-3384D0*DS*M2**3*S**3-33D0*M2**4*S**
     &  3+24D0*DS**3*S**4-962D0*DS**2*M2*S**4+1971D0*DS*M2**2*S**4+
     &  33D0*M2**3*S**4+36D0*DS**2*S**5-508D0*DS*M2*S**5-81D0*M2**2*
     &  S**5+24D0*DS*S**6+78D0*M2*S**6-24D0*S**7)+6D0*S**(-5)*(M2-
     &  S)**(-3)*(44D0*DS**3*M2**4-88D0*DS**2*M2**5+44D0*DS*M2**6-
     &  173D0*DS**3*M2**3*S+431D0*DS**2*M2**4*S-255D0*DS*M2**5*S-
     &  3D0*M2**6*S+252D0*DS**3*M2**2*S**2-839D0*DS**2*M2**3*S**2+
     &  618D0*DS*M2**4*S**2+9D0*M2**5*S**2-158D0*DS**3*M2*S**3+
     &  806D0*DS**2*M2**2*S**3-799D0*DS*M2**3*S**3-9D0*M2**4*S**3+
     &  32D0*DS**3*S**4-374D0*DS**2*M2*S**4+576D0*DS*M2**2*S**4+3D0*
     &  M2**3*S**4+64D0*DS**2*S**5-216D0*DS*M2*S**5+32D0*DS*S**6)*
     &  XLOGM2))/72D0
        C3(1,1,2) = ((6D0*FB0(3)*(DS-M2+S)**2-S**(-3)*(M2-S)**(-2)*
     &  (6D0*DS**2*M2**2-12D0*DS*M2**3+6D0*M2**4-9D0*DS**2*M2*S+
     &  24D0*DS*M2**2*S-9D0*M2**3*S+2D0*DS**2*S**2-13D0*DS*M2*S**2+
     &  2D0*M2**2*S**2+DS*S**3-M2*S**3+2D0*S**4)+6D0*S**(-4)*(M2-S)*
     &  (DS**2-2D0*DS*M2+M2**2+DS*S)*XLOGM2))/18D0
        C3(1,2,2) = -(((-(2D0*FB0(2)*(DS-M2+S)**2)-(S**(-2)*(2D0*
     &  DS**2*M2-4D0*DS*M2**2+6D0*M2**3-DS**2*S+4D0*DS*M2*S-9D0*M2**
     &  2*S+2D0*M2*S**2+S**3))/(M2-S)+2D0*S**(-3)*(M2-S)*(DS**2-2D0*
     &  DS*M2+3D0*M2**2+DS*S-3D0*M2*S)*XLOGM2))/(12D0*(DS+S)))
        C3(2,2,2) = ((DS+S)**(-2)*(2D0*FB0(1)*(DS-M2+S)**2-S**(-2)*
     &  (2D0*DS*M2**2+2D0*DS**2*S-9D0*DS*M2*S+6D0*M2**2*S+4D0*DS*S**
     &  2-9D0*M2*S**2+2D0*S**3)+2D0*S**(-3)*(M2-S)*(DS*M2**2+DS**2*
     &  S-5D0*DS*M2*S+3D0*M2**2*S+4D0*DS*S**2-6D0*M2*S**2+3D0*S**3)*
     &  XLOGM2))/6D0
        C3(0,0,1) = ((-(6D0*LOGMU2)-6D0*FB0(2)*(DS-M2+S)**2-(S**(-
     &  2)*(6D0*DS**2*M2-12D0*DS*M2**2+6D0*M2**3-3D0*DS**2*S+12D0*
     &  DS*M2*S-3D0*M2**2*S+10D0*M2*S**2-13D0*S**3))/(M2-S)+6D0*S**
     &  (-3)*(M2-S)*(DS**2-2D0*DS*M2+M2**2+DS*S)*XLOGM2))/72D0
        C3(0,0,2) = ((-(3D0*LOGMU2)-(3D0*FB0(1)*(DS-M2+S)**2)/(DS+
     &  S)+(3D0*DS**2-6D0*DS*M2+6D0*M2**2-2D0*DS*S-6D0*M2*S-5D0*S**
     &  2)/(S*(DS+S))-(3D0*S**(-2)*(M2-S)*(DS**2-2D0*DS*M2+2D0*M2**
     &  2+DS*S-3D0*M2*S)*XLOGM2)/(DS+S)))/36D0
        C4(0,0,0,0) = ((-((6D0*FB0(1)*(DS-M2+S)**3)/(DS+S))-6D0*
     &  LOGMU2*(DS-4D0*M2+2D0*S)+(6D0*DS**3-18D0*DS**2*M2+18D0*DS*
     &  M2**2-12D0*M2**3-DS**2*S+34D0*DS*M2*S+18D0*M2**2*S-39D0*DS*
     &  S**2+52D0*M2*S**2-32D0*S**3)/(S*(DS+S))-(6D0*S**(-2)*(M2-S)*
     &  (DS**3-3D0*DS**2*M2+3D0*DS*M2**2-2D0*M2**3+2D0*DS**2*S-3D0*
     &  DS*M2*S+4D0*M2**2*S+DS*S**2)*XLOGM2)/(DS+S)))/576D0
        C4(0,0,1,1) = ((6D0*LOGMU2+6D0*FB0(3)*(DS-M2+S)**3-S**(-3)*
     &  (M2-S)**(-2)*(6D0*DS**3*M2**2-18D0*DS**2*M2**3+18D0*DS*M2**
     &  4-6D0*M2**5-9D0*DS**3*M2*S+39D0*DS**2*M2**2*S-45D0*DS*M2**3*
     &  S+9D0*M2**4*S+2D0*DS**3*S**2-24D0*DS**2*M2*S**2+39D0*DS*M2**
     &  2*S**2-2D0*M2**3*S**2+3D0*DS**2*S**3-15D0*DS*M2*S**3-13D0*
     &  M2**2*S**3+3D0*DS*S**4+26D0*M2*S**4-14D0*S**5)+6D0*S**(-4)*
     &  (M2-S)*(DS**3-3D0*DS**2*M2+3D0*DS*M2**2-M2**3+2D0*DS**2*S-
     &  3D0*DS*M2*S+DS*S**2)*XLOGM2))/144D0
        C4(0,0,1,2) = ((6D0*LOGMU2+(6D0*FB0(2)*(DS-M2+S)**3)/(DS+S)+
     &  (S**(-2)*(6D0*DS**3*M2-18D0*DS**2*M2**2+18D0*DS*M2**3-18D0*
     &  M2**4-3D0*DS**3*S+21D0*DS**2*M2*S-27D0*DS*M2**2*S+33D0*M2**
     &  3*S-3D0*DS**2*S**2+19D0*DS*M2*S**2-9D0*M2**2*S**2-10D0*DS*
     &  S**3+4D0*M2*S**3-10D0*S**4))/((DS+S)*(M2-S))-(6D0*S**(-3)*
     &  (M2-S)*(DS**3-3D0*DS**2*M2+3D0*DS*M2**2-3D0*M2**3+2D0*DS**2*
     &  S-3D0*DS*M2*S+4D0*M2**2*S+DS*S**2)*XLOGM2)/(DS+S)))/288D0
        C4(0,0,2,2) = ((6D0*LOGMU2+6D0*FB0(1)*(DS+S)**(-2)*(DS-M2+
     &  S)**3-S**(-2)*(DS+S)**(-2)*(-(6D0*DS*M2**3)+6D0*DS**3*S-
     &  18D0*DS**2*M2*S+39D0*DS*M2**2*S-18D0*M2**3*S+2D0*DS**2*S**2-
     &  36D0*DS*M2*S**2+39D0*M2**2*S**2-14D0*DS*S**3-18D0*M2*S**3-
     &  10D0*S**4)-6D0*S**(-3)*(DS+S)**(-2)*(M2-S)*(DS*M2**3-DS**3*
     &  S+3D0*DS**2*M2*S-7D0*DS*M2**2*S+3D0*M2**3*S-2D0*DS**2*S**2+
     &  9D0*DS*M2*S**2-8D0*M2**2*S**2-DS*S**3+6D0*M2*S**3)*XLOGM2))/
     &  144D0
        C4(1,1,1,1) = ((7500D0*FB0(5)*(DS+S)*(DS-M2+S)**3+1800D0*
     &  FC0(4)*(DS-M2+S)**4+30D0*LOGM12*(M2-S)**(-5)*(12D0*DS**4-
     &  33D0*DS**3*M2+32D0*DS**2*M2**2-8D0*DS*M2**3+12D0*M2**4+33D0*
     &  DS**3*S-64D0*DS**2*M2*S+24D0*DS*M2**2*S-48D0*M2**3*S+32D0*
     &  DS**2*S**2-24D0*DS*M2*S**2+72D0*M2**2*S**2+8D0*DS*S**3-48D0*
     &  M2*S**3+12D0*S**4)-S**(-5)*(M2-S)**(-5)*(7500D0*DS**4*M2**5-
     &  22500D0*DS**3*M2**6+22500D0*DS**2*M2**7-7500D0*DS*M2**8-
     &  33390D0*DS**4*M2**4*S+122310D0*DS**3*M2**5*S-144090D0*DS**2*
     &  M2**6*S+54810D0*DS*M2**7*S+360D0*M2**8*S+57130D0*DS**4*M2**
     &  3*S**2-270030D0*DS**3*M2**4*S**2+389010D0*DS**2*M2**5*S**2-
     &  174400D0*DS*M2**6*S**2-1260D0*M2**7*S**2-45305D0*DS**4*M2**
     &  2*S**3+304890D0*DS**3*M2**3*S**3-568905D0*DS**2*M2**4*S**3+
     &  313235D0*DS*M2**5*S**3+1560D0*M2**6*S**3+14815D0*DS**4*M2*
     &  S**4-178755D0*DS**3*M2**2*S**4+478455D0*DS**2*M2**3*S**4-
     &  343390D0*DS*M2**4*S**4-750D0*M2**5*S**4-606D0*DS**4*S**5+
     &  45684D0*DS**3*M2*S**5-222321D0*DS**2*M2**2*S**5+230434D0*DS*
     &  M2**3*S**5-606D0*M2**4*S**5-1599D0*DS**3*S**6+46887D0*DS**2*
     &  M2*S**6-88322D0*DS*M2**2*S**6+2664D0*M2**3*S**6-1536D0*DS**
     &  2*S**7+15457D0*DS*M2*S**7-3876D0*M2**2*S**7-324D0*DS*S**8+
     &  2514D0*M2*S**8-606D0*S**9)+30D0*S**(-6)*(M2-S)**(-4)*(250D0*
     &  DS**4*M2**5-750D0*DS**3*M2**6+750D0*DS**2*M2**7-250D0*DS*
     &  M2**8-1238D0*DS**4*M2**4*S+4452D0*DS**3*M2**5*S-5178D0*DS**
     &  2*M2**6*S+1952D0*DS*M2**7*S+12D0*M2**8*S+2440D0*DS**4*M2**3*
     &  S**2-10977D0*DS**3*M2**4*S**2+15306D0*DS**2*M2**5*S**2-
     &  6706D0*DS*M2**6*S**2-48D0*M2**7*S**2-2380D0*DS**4*M2**2*S**
     &  3+14355D0*DS**3*M2**3*S**3-25078D0*DS**2*M2**4*S**3+13206D0*
     &  DS*M2**5*S**3+72D0*M2**6*S**3+1130D0*DS**4*M2*S**4-10440D0*
     &  DS**3*M2**2*S**4+24530D0*DS**2*M2**3*S**4-16252D0*DS*M2**4*
     &  S**4-48D0*M2**5*S**4-190D0*DS**4*S**5+3930D0*DS**3*M2*S**5-
     &  14230D0*DS**2*M2**2*S**5+12740D0*DS*M2**3*S**5+12D0*M2**4*
     &  S**5-570D0*DS**3*S**6+4470D0*DS**2*M2*S**6-6170D0*DS*M2**2*
     &  S**6-570D0*DS**2*S**7+1670D0*DS*M2*S**7-190D0*DS*S**8)*
     &  XLOGM2))/1800D0
        C4(1,1,1,2) = ((-(12D0*FB0(4)*(DS-M2+S)**3)-S**(-4)*(M2-S)**
     &  (-3)*(12D0*DS**3*M2**3-36D0*DS**2*M2**4+36D0*DS*M2**5-12D0*
     &  M2**6-30D0*DS**3*M2**2*S+114D0*DS**2*M2**3*S-126D0*DS*M2**4*
     &  S+30D0*M2**5*S+22D0*DS**3*M2*S**2-126D0*DS**2*M2**2*S**2+
     &  168D0*DS*M2**3*S**2-22D0*M2**4*S**2-3D0*DS**3*S**3+53D0*DS**
     &  2*M2*S**3-105D0*DS*M2**2*S**3+3D0*M2**3*S**3-5D0*DS**2*S**4+
     &  30D0*DS*M2*S**4+3D0*M2**2*S**4-3D0*DS*S**5-5D0*M2*S**5+3D0*
     &  S**6)+12D0*S**(-5)*(M2-S)*(DS**3-3D0*DS**2*M2+3D0*DS*M2**2-
     &  M2**3+2D0*DS**2*S-3D0*DS*M2*S+DS*S**2)*XLOGM2))/48D0
        C4(1,1,2,2) = -(((6D0*FB0(3)*(DS-M2+S)**3-S**(-3)*(M2-S)**(-
     &  2)*(6D0*DS**3*M2**2-18D0*DS**2*M2**3+18D0*DS*M2**4-24D0*M2**
     &  5-9D0*DS**3*M2*S+39D0*DS**2*M2**2*S-45D0*DS*M2**3*S+60D0*
     &  M2**4*S+2D0*DS**3*S**2-24D0*DS**2*M2*S**2+39D0*DS*M2**2*S**
     &  2-44D0*M2**3*S**2+3D0*DS**2*S**3-15D0*DS*M2*S**3+6D0*M2**2*
     &  S**3+3D0*DS*S**4+2D0*S**5)+6D0*S**(-4)*(M2-S)*(DS**3-3D0*
     &  DS**2*M2+3D0*DS*M2**2-4D0*M2**3+2D0*DS**2*S-3D0*DS*M2*S+4D0*
     &  M2**2*S+DS*S**2)*XLOGM2))/(72D0*(DS+S)))
        C4(1,2,2,2) = ((DS+S)**(-2)*(-(2D0*FB0(2)*(DS-M2+S)**3)-(S**
     &  (-3)*(-(6D0*DS*M2**4)+2D0*DS**3*M2*S-6D0*DS**2*M2**2*S+25D0*
     &  DS*M2**3*S-12D0*M2**4*S-DS**3*S**2+7D0*DS**2*M2*S**2-28D0*
     &  DS*M2**2*S**2+30D0*M2**3*S**2-DS**2*S**3+8D0*DS*M2*S**3-
     &  22D0*M2**2*S**3+DS*S**4+3D0*M2*S**4+S**5))/(M2-S)+2D0*S**(-
     &  4)*(M2-S)*(-(3D0*DS*M2**3)+DS**3*S-3D0*DS**2*M2*S+11D0*DS*
     &  M2**2*S-6D0*M2**3*S+2D0*DS**2*S**2-9D0*DS*M2*S**2+12D0*M2**
     &  2*S**2+DS*S**3-6D0*M2*S**3)*XLOGM2))/24D0
        C4(2,2,2,2) = ((DS+S)**(-3)*(-(6D0*FB0(1)*(DS-M2+S)**3)+S**
     &  (-3)*(-(6D0*DS**2*M2**3)+21D0*DS**2*M2**2*S-18D0*DS*M2**3*S+
     &  6D0*DS**3*S**2-44D0*DS**2*M2*S**2+81D0*DS*M2**2*S**2-24D0*
     &  M2**3*S**2+18D0*DS**2*S**3-88D0*DS*M2*S**3+60D0*M2**2*S**3+
     &  18D0*DS*S**4-44D0*M2*S**4+6D0*S**5)+6D0*S**(-4)*(M2-S)*(DS**
     &  2*M2**3-4D0*DS**2*M2**2*S+3D0*DS*M2**3*S-DS**3*S**2+9D0*DS**
     &  2*M2*S**2-15D0*DS*M2**2*S**2+4D0*M2**3*S**2-6D0*DS**2*S**3+
     &  21D0*DS*M2*S**3-12D0*M2**2*S**3-9D0*DS*S**4+12D0*M2*S**4-
     &  4D0*S**5)*XLOGM2))/24D0

c        dC1111 = ( abs(25D0/6d0*dFB0(5)*(DS+S)*(DS-M2+S)**3)
c     &           +abs(dFC0(4)*(DS-M2+S)**4) )/abs(C4(1,1,1,1))
        dC111 = ( abs(11D0/3d0*dFB0(4)*(DS+S)*(DS-M2+S)**2)
     &           +abs(dFC0(3)*(DS-M2+S)**3) )/abs(C3(1,1,1))
        dC222 = abs((DS+S)**(-2)*dFB0(1)*(DS-M2+S)**2/3D0)
     &          /abs(C3(2,2,2))

        elseif (s.eq.0d0) then


        C0 = FC0(0)-1D0/M2+LOGM12/M2
        C1(1) = -(2D0*FB0(2)*DS)-(LOGM12*M2**(-2)*(DS+M2))/2D0-
     &  FC0(1)*(DS-M2)-(M2**(-2)*(DS+3D0*M2))/12D0

        C1(2) = FB0(1)+1D0/(2D0*M2)
        C2(0,0) = (LOGMU2)/4D0+(FB0(1)*(DS-M2))/4D0+((DS+3D0*M2))/
     &  (8D0*M2)
        C2(1,1) = 3D0*FB0(3)*DS*(DS-M2)+FC0(2)*(DS-M2)**2+(M2**(-3)*
     &  (5D0*DS**2+8D0*DS*M2+14D0*M2**2))/36D0+(LOGM12*M2**(-3)*
     &  (2D0*DS**2-DS*M2+2D0*M2**2))/6D0
        C2(1,2) = (FB0(2)*(-(DS)+M2))/2D0-(M2**(-2)*(DS+2D0*M2))/
     &  12D0
        C2(2,2) = (FB0(2)*(-(DS)+M2))/2D0-(M2**(-2)*(DS+2D0*M2))/
     &  12D0
        C3(1,1,1) = -((11D0*FB0(4)*DS*(DS-M2)**2)/3D0)-FC0(3)*(DS-
     &  M2)**3-(M2**(-4)*(87D0*DS**3+11D0*DS**2*M2+57D0*DS*M2**2+
     &  285D0*M2**3))/720D0-(LOGM12*M2**(-4)*(3D0*DS**3-5D0*DS**2*
     &  M2+3D0*DS*M2**2+3D0*M2**3))/12D0
        C3(1,1,2) = (FB0(3)*(DS-M2)**2)/3D0+(M2**(-3)*(DS**2+3D0*
     &  M2**2))/36D0
        C3(1,2,2) = (FB0(3)*(DS-M2)**2)/6D0+(M2**(-3)*(DS**2+3D0*
     &  M2**2))/72D0
        C3(2,2,2) = (FB0(3)*(DS-M2)**2)/3D0+(M2**(-3)*(DS**2+3D0*
     &  M2**2))/36D0
        C3(0,0,1) = -((LOGMU2)/12D0)-(FB0(2)*(DS-M2)**2)/12D0-(M2**
     &  (-2)*(DS**2+DS*M2+11D0*M2**2))/72D0
        C3(0,0,2) = -((LOGMU2)/12D0)-(FB0(2)*(DS-M2)**2)/12D0-(M2**
     &  (-2)*(DS**2+DS*M2+5D0*M2**2))/72D0
        C4(0,0,0,0) = -((FB0(2)*(DS-M2)**3)/96D0)-(LOGMU2*(DS-4D0*
     &  M2))/96D0-(M2**(-2)*(DS**3+7D0*DS*M2**2-44D0*M2**3))/576D0
        C4(0,0,1,1) = (LOGMU2)/24D0+(FB0(3)*(DS-M2)**3)/24D0+(M2**(-
     &  3)*(DS**3-DS**2*M2+3D0*DS*M2**2+25D0*M2**3))/288D0
        C4(0,0,1,2) = (LOGMU2)/48D0+(FB0(3)*(DS-M2)**3)/48D0+(M2**(-
     &  3)*(DS**3-DS**2*M2+3D0*DS*M2**2+13D0*M2**3))/576D0
        C4(0,0,2,2) = (LOGMU2)/24D0+(FB0(3)*(DS-M2)**3)/24D0+(M2**(-
     &  3)*(DS**3-DS**2*M2+3D0*DS*M2**2+7D0*M2**3))/288D0
        C4(1,1,1,1) = (25D0*FB0(5)*DS*(DS-M2)**3)/6D0+FC0(4)*(DS-
     &  M2)**4+(LOGM12*M2**(-5)*(12D0*DS**4-33D0*DS**3*M2+32D0*DS**
     &  2*M2**2-8D0*DS*M2**3+12D0*M2**4))/60D0+(M2**(-5)*(356D0*DS**
     &  4-399D0*DS**3*M2+136D0*DS**2*M2**2+426D0*DS*M2**3+1356D0*
     &  M2**4))/3600D0
        C4(1,1,1,2) = -((FB0(4)*(DS-M2)**3)/4D0)-(M2**(-4)*(3D0*DS**
     &  3-4D0*DS**2*M2+4D0*DS*M2**2+12D0*M2**3))/240D0
        C4(1,1,2,2) = -((FB0(4)*(DS-M2)**3)/12D0)-(M2**(-4)*(3D0*
     &  DS**3-4D0*DS**2*M2+4D0*DS*M2**2+12D0*M2**3))/720D0
        C4(1,2,2,2) = -((FB0(4)*(DS-M2)**3)/12D0)-(M2**(-4)*(3D0*
     &  DS**3-4D0*DS**2*M2+4D0*DS*M2**2+12D0*M2**3))/720D0
        C4(2,2,2,2) = -((FB0(4)*(DS-M2)**3)/4D0)-(M2**(-4)*(3D0*DS**
     &  3-4D0*DS**2*M2+4D0*DS*M2**2+12D0*M2**3))/240D0

c        dC1111 = ( abs((25D0*dFB0(5)*DS*(DS-M2)**3)/6D0)
c     &           +abs(dFC0(4)*(DS-M2)**4))/abs(C4(1,1,1,1))
        dC111 = ( abs(-((11D0*dFB0(4)*DS*(DS-M2)**2)/3D0))
     &           +abs(-dFC0(3)*(DS-M2)**3))/abs(C3(1,1,1))
        dC222 = abs((dFB0(3)*(DS-M2)**2)/3D0)/abs(C3(2,2,2))

        elseif (ds.eq.-s1) then

        C0 = FC0(0)-1D0/M2+LOGM12/M2
        C1(1) = -(FB0(2)*DS)+FC0(1)*M2-(LOGM12)/(2D0*M2)-(M2**(-2)*
     &  (2D0*DS+3D0*M2))/12D0
        C1(2) = -(FB0(1))+1D0/(2D0*M2)
        C2(0,0) = (LOGMU2)/4D0+(FB0(1)*(DS+M2))/4D0-((DS-3D0*M2))/
     &  (8D0*M2)
        C2(1,1) = (LOGM12)/(3D0*M2)+FC0(2)*M2**2+(FB0(3)*DS*(DS-3D0*
     &  M2))/2D0-(M2**(-3)*(3D0*DS**2-15D0*DS*M2-28D0*M2**2))/72D0
        C2(1,2) = -(FB0(2)*M2)-1D0/(6D0*M2)
        C2(2,2) = (FB0(2)*(DS+M2))/2D0+(M2**(-2)*(DS-2D0*M2))/12D0
        C3(1,1,1) = -((LOGM12)/(4D0*M2))+FC0(3)*M2**3-(FB0(4)*DS*
     &  (2D0*DS**2-5D0*DS*M2+11D0*M2**2))/6D0-(M2**(-4)*(12D0*DS**3-
     &  50D0*DS**2*M2+156D0*DS*M2**2+285D0*M2**3))/720D0
        C3(1,1,2) = 1D0/(12D0*M2)-FB0(3)*M2**2
        C3(1,2,2) = (FB0(3)*M2*(DS+M2))/2D0-(M2**(-2)*(DS-M2))/24D0
        C3(2,2,2) = -((FB0(3)*(DS+M2)**2)/3D0)+(M2**(-3)*(DS**2+3D0*
     &  M2**2))/36D0
        C3(0,0,1) = -((LOGMU2)/12D0)-(FB0(2)*(DS+M2)*(DS-2D0*M2))/
     &  12D0-(M2**(-2)*(DS**2-4D0*DS*M2+11D0*M2**2))/72D0
        C3(0,0,2) = -((LOGMU2)/12D0)-(FB0(2)*(DS+M2)**2)/12D0-(M2**
     &  (-2)*(DS**2-DS*M2+5D0*M2**2))/72D0
        C4(0,0,0,0) = (FB0(2)*(DS+M2)**3)/96D0+(LOGMU2*(DS+4D0*M2))/
     &  96D0+(M2**(-2)*(DS**3+7D0*DS*M2**2+44D0*M2**3))/576D0
        C4(0,0,1,1) = (LOGMU2)/24D0+(FB0(3)*(DS+M2)*(DS**2-2D0*DS*
     &  M2+3D0*M2**2))/24D0-(M2**(-3)*(DS**3-3D0*DS**2*M2+9D0*DS*
     &  M2**2-25D0*M2**3))/288D0
        C4(0,0,1,2) = (LOGMU2)/48D0+(FB0(3)*(DS+M2)**2*(DS-3D0*M2))/
     &  48D0-(M2**(-3)*(DS**3-3D0*DS**2*M2+3D0*DS*M2**2-13D0*M2**
     &  3))/576D0
        C4(0,0,2,2) = (LOGMU2)/24D0+(FB0(3)*(DS+M2)**3)/24D0-(M2**(-
     &  3)*(DS**3+DS**2*M2+3D0*DS*M2**2-7D0*M2**3))/288D0
        C4(1,1,1,1) = (LOGM12)/(5D0*M2)+FC0(4)*M2**4+(FB0(5)*DS*
     &  (3D0*DS**3-7D0*DS**2*M2+13D0*DS*M2**2-25D0*M2**3))/12D0-
     &  (M2**(-5)*(30D0*DS**4-115D0*DS**3*M2+310D0*DS**2*M2**2-
     &  770D0*DS*M2**3-1356D0*M2**4))/3600D0
        C4(1,1,1,2) = -(1D0/(20D0*M2))-FB0(4)*M2**3
        C4(1,1,2,2) = (FB0(4)*M2**2*(DS+M2))/2D0+(M2**(-2)*(3D0*DS-
     &  2D0*M2))/120D0
        C4(1,2,2,2) = -((FB0(4)*M2*(DS+M2)**2)/3D0)-(M2**(-3)*(3D0*
     &  DS**2+DS*M2+3D0*M2**2))/180D0
        C4(2,2,2,2) = (FB0(4)*(DS+M2)**3)/4D0+(M2**(-4)*(3D0*DS**3+
     &  4D0*DS**2*M2+4D0*DS*M2**2-12D0*M2**3))/240D0

c        dC1111 = ( abs(dFC0(4)*M2**4)
c     &    +abs(dFB0(5)*DS*(3D0*DS**3-7D0*DS**2*M2
c     &         +13D0*DS*M2**2-25D0*M2**3))/12D0 )/abs(C4(1,1,1,1))
        dC111 = ( abs(dFC0(3)*M2**3)
     &           +abs(dFB0(4)*DS*(2D0*DS**2-5D0*DS*M2+11D0*M2**2))/6D0 )
     &          /abs(C3(1,1,1))
        dC222 = abs((dFB0(3)*(DS+M2)**2)/3D0)/abs(C3(2,2,2))

        endif

c redundant coefficients
          C2(2,1) = C2(1,2)
          C3(1,2,1) = C3(1,1,2)
          C3(2,1,1) = C3(1,1,2)
          C3(2,1,2) = C3(1,2,2)
          C3(2,2,1) = C3(1,2,2)
          C4(0,0,2,1) = C4(0,0,1,2)
          C4(1,1,2,1) = C4(1,1,1,2)
          C4(1,2,1,1) = C4(1,1,1,2)
          C4(2,1,1,1) = C4(1,1,1,2)
          C4(1,2,1,2) = C4(1,1,2,2)
          C4(2,1,1,2) = C4(1,1,2,2)
          C4(2,1,2,1) = C4(1,1,2,2)
          C4(2,2,1,1) = C4(1,1,2,2)
          C4(1,2,2,1) = C4(1,1,2,2)
          C4(2,1,2,2) = C4(1,2,2,2)
          C4(2,2,1,2) = C4(1,2,2,2)
          C4(2,2,2,1) = C4(1,2,2,2)

        crelacc = max(dC111,dC222)

        end

**********************************************************************
        subroutine Ccoll2_dd(m12,s1,s2,M2,C0,C1,C2,C3,C4,crelacc)
**********************************************************************
*       C0(m12,s2,s1,m12,0,M2) for m12 -> 0
*       exact calculation, numerically stable for s1 -> s2
*---------------------------------------------------------------------
*       6.4.05 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)
        complex*16 M2,logm12,xlogm2,logmu2
        complex*16 fb0(0:5),fc0(0:5),dfb0(0:5),dfc0(0:5)
        complex*16 C0,C1(2),C2(0:2,0:2),C3(0:2,0:2,2)
        complex*16 C4(0:2,0:2,0:2,0:2)

        dc111 = 0d0
        dc222 = 0d0

        if (abs(s1).gt.1d-15) then
          s = s1
        else
          s = 0d0
        endif
        if (abs(s2).gt.1d-15) then
          ds = s2-s1
        else
          ds = -s1
        endif

        call BCfin_dd(s,ds,m12,M2,Logm12,xLogM2,Logmu2,C0,
     &                   fb0,fc0,dfb0,dfc0)

        if ((s.ne.0d0).and.(ds.ne.-s1)) then

        C1(1) = ((-(2D0*FC0(1)*(M2-S))-(LOGM12)/(M2-S)+2D0*FB0(2)*
     &  (DS+2D0*S)+(S**(-2)*(2D0*DS*M2-DS*S+M2*S+2D0*S**2))/(M2-S)-
     &  S**(-3)*(2D0*DS*M2-2D0*DS*S+M2*S)*XLOGM2))/2D0
        C1(2) = FB0(1)-1D0/S+S**(-2)*(M2-S)*XLOGM2
        C2(0,0) = ((LOGMU2+FB0(1)*(DS-M2+S)-((DS-M2-2D0*S))/S+(DS-
     &  M2)*S**(-2)*(M2-S)*XLOGM2))/4D0
        C2(1,1) = (((12D0*LOGM12)/(M2-S)+36D0*FC0(2)*(M2-S)**2-
     &  (18D0*FB0(3)*(DS**3+3D0*DS**2*M2-DS**2*S+10D0*DS*M2*S-8D0*
     &  DS*S**2+6D0*M2*S**2-6D0*S**3))/(DS+S)+(S**(-3)*(M2-S)**(-2)*
     &  (18D0*DS**3*M2**2+54D0*DS**2*M2**3-27D0*DS**3*M2*S-117D0*
     &  DS**2*M2**2*S+114D0*DS*M2**3*S+6D0*DS**3*S**2+72D0*DS**2*M2*
     &  S**2-255D0*DS*M2**2*S**2-12D0*M2**3*S**2-15D0*DS**2*S**3+
     &  130D0*DS*M2*S**3+6D0*M2**2*S**3+5D0*DS*S**4-20D0*M2*S**4+
     &  26D0*S**5))/(DS+S)-(6D0*S**(-4)*(3D0*DS**3*M2+9D0*DS**2*M2**
     &  2-3D0*DS**3*S-15D0*DS**2*M2*S+19D0*DS*M2**2*S+6D0*DS**2*S**
     &  2-33D0*DS*M2*S**2-2D0*M2**2*S**2+12D0*DS*S**3)*XLOGM2)/(DS+
     &  S)))/36D0
        C2(1,2) = ((-(2D0*FB0(2)*(2D0*DS*M2-DS*S+M2*S-S**2))-(S**(-
     &  2)*(4D0*DS*M2**2-4D0*DS*M2*S-2D0*M2**2*S+DS*S**2+M2*S**2+S**
     &  3))/(M2-S)+2D0*S**(-3)*(M2-S)*(2D0*DS*M2-DS*S-M2*S)*
     &  XLOGM2))/(4D0*(DS+S))
        C2(2,2) = -(((FB0(1)*(DS-M2+S)-((DS-2D0*M2+S))/S+S**(-2)*
     &  (M2-S)*(DS-2D0*M2+2D0*S)*XLOGM2))/(2D0*(DS+S)))
        C3(1,1,1) = ((-((18D0*LOGM12)/(M2-S))-72D0*FC0(3)*(M2-S)**3+
     &  12D0*FB0(4)*(DS+S)**(-2)*(2D0*DS**5+5D0*DS**4*M2+11D0*DS**3*
     &  M2**2+DS**4*S-7D0*DS**3*M2*S+51D0*DS**2*M2**2*S+2D0*DS**3*
     &  S**2-78D0*DS**2*M2*S**2+60D0*DS*M2**2*S**2+31D0*DS**2*S**3-
     &  110D0*DS*M2*S**3+22D0*M2**2*S**3+50D0*DS*S**4-44D0*M2*S**4+
     &  22D0*S**5)+S**(-4)*(DS+S)**(-2)*(M2-S)**(-3)*(24D0*DS**5*
     &  M2**3+60D0*DS**4*M2**4+132D0*DS**3*M2**5-60D0*DS**5*M2**2*S-
     &  162D0*DS**4*M2**3*S-474D0*DS**3*M2**4*S+462D0*DS**2*M2**5*S+
     &  44D0*DS**5*M2*S**2+140D0*DS**4*M2**2*S**2+638D0*DS**3*M2**3*
     &  S**2-1893D0*DS**2*M2**4*S**2+228D0*DS*M2**5*S**2-6D0*DS**5*
     &  S**3-37D0*DS**4*M2*S**3-387D0*DS**3*M2**2*S**3+2974D0*DS**2*
     &  M2**3*S**3-1062D0*DS*M2**4*S**3+18D0*M2**5*S**3+5D0*DS**4*
     &  S**4+111D0*DS**3*M2*S**4-2118D0*DS**2*M2**2*S**4+1876D0*DS*
     &  M2**3*S**4-27D0*M2**4*S**4-14D0*DS**3*S**5+604D0*DS**2*M2*
     &  S**5-1418D0*DS*M2**2*S**5+6D0*M2**3*S**5-25D0*DS**2*S**6+
     &  334D0*DS*M2*S**6+39D0*M2**2*S**6+42D0*DS*S**7-78D0*M2*S**7+
     &  42D0*S**8)-6D0*S**(-5)*(DS+S)**(-2)*(4D0*DS**5*M2+10D0*DS**
     &  4*M2**2+22D0*DS**3*M2**3-4D0*DS**5*S-12D0*DS**4*M2*S-46D0*
     &  DS**3*M2**2*S+77D0*DS**2*M2**3*S+2D0*DS**4*S**2+30D0*DS**3*
     &  M2*S**2-200D0*DS**2*M2**2*S**2+38D0*DS*M2**3*S**2-6D0*DS**3*
     &  S**3+170D0*DS**2*M2*S**3-120D0*DS*M2**2*S**3+3D0*M2**3*S**3-
     &  44D0*DS**2*S**4+120D0*DS*M2*S**4-32D0*DS*S**5)*XLOGM2))/72D0
        C3(1,1,2) = ((DS+S)**(-2)*(6D0*FB0(3)*(3D0*DS**2*M2**2-3D0*
     &  DS**2*M2*S+3D0*DS*M2**2*S+DS**2*S**2-5D0*DS*M2*S**2+M2**2*
     &  S**2+2D0*DS*S**3-2D0*M2*S**3+S**4)-S**(-3)*(M2-S)**(-2)*
     &  (18D0*DS**2*M2**4-45D0*DS**2*M2**3*S+39D0*DS**2*M2**2*S**2-
     &  12D0*DS*M2**3*S**2+6D0*M2**4*S**2-15D0*DS**2*M2*S**3+24D0*
     &  DS*M2**2*S**3-9D0*M2**3*S**3+2D0*DS**2*S**4-16D0*DS*M2*S**4+
     &  2D0*M2**2*S**4+4D0*DS*S**5-M2*S**5+2D0*S**6)+6D0*S**(-4)*
     &  (M2-S)*(3D0*DS**2*M2**2-3D0*DS**2*M2*S+DS**2*S**2-2D0*DS*M2*
     &  S**2+M2**2*S**2+DS*S**3)*XLOGM2))/18D0
        C3(1,2,2) = -(((DS+S)**(-2)*(-(2D0*FB0(2)*(DS-M2+S)*(3D0*DS*
     &  M2-DS*S+M2*S-S**2))-(S**(-2)*(6D0*DS**2*M2**2-6D0*DS*M2**3-
     &  5D0*DS**2*M2*S+7D0*DS*M2**2*S+6D0*M2**3*S+DS**2*S**2-3D0*DS*
     &  M2*S**2-9D0*M2**2*S**2+2D0*DS*S**3+2D0*M2*S**3+S**4))/(M2-
     &  S)+2D0*S**(-3)*(M2-S)*(3D0*DS**2*M2-3D0*DS*M2**2-DS**2*S+
     &  2D0*DS*M2*S+3D0*M2**2*S-DS*S**2-3D0*M2*S**2)*XLOGM2))/12D0)
        C3(2,2,2) = ((DS+S)**(-2)*(2D0*FB0(1)*(DS-M2+S)**2-S**(-2)*
     &  (2D0*DS*M2**2+2D0*DS**2*S-9D0*DS*M2*S+6D0*M2**2*S+4D0*DS*S**
     &  2-9D0*M2*S**2+2D0*S**3)+2D0*S**(-3)*(M2-S)*(DS*M2**2+DS**2*
     &  S-5D0*DS*M2*S+3D0*M2**2*S+4D0*DS*S**2-6D0*M2*S**2+3D0*S**3)*
     &  XLOGM2))/6D0
        C3(0,0,1) = ((-(6D0*LOGMU2)-(6D0*FB0(2)*(DS-M2+S)*(DS**2+
     &  2D0*DS*M2+M2*S-S**2))/(DS+S)-(S**(-2)*(6D0*DS**3*M2+6D0*DS**
     &  2*M2**2-12D0*DS*M2**3-3D0*DS**3*S-3D0*DS**2*M2*S+18D0*DS*
     &  M2**2*S+6D0*M2**3*S+3D0*DS**2*S**2+DS*M2*S**2-3D0*M2**2*S**
     &  2-7D0*DS*S**3+10D0*M2*S**3-13D0*S**4))/((DS+S)*(M2-S))+(6D0*
     &  S**(-3)*(M2-S)*(DS**3+DS**2*M2-2D0*DS*M2**2+2D0*DS*M2*S+M2**
     &  2*S-DS*S**2)*XLOGM2)/(DS+S)))/72D0
        C3(0,0,2) = ((-(3D0*LOGMU2)-(3D0*FB0(1)*(DS-M2+S)**2)/(DS+
     &  S)+(3D0*DS**2-6D0*DS*M2+6D0*M2**2-2D0*DS*S-6D0*M2*S-5D0*S**
     &  2)/(S*(DS+S))-(3D0*S**(-2)*(M2-S)*(DS**2-2D0*DS*M2+2D0*M2**
     &  2+DS*S-3D0*M2*S)*XLOGM2)/(DS+S)))/36D0
        C4(0,0,0,0) = ((-((6D0*FB0(1)*(DS-M2+S)**3)/(DS+S))-6D0*
     &  LOGMU2*(DS-4D0*M2+2D0*S)+(6D0*DS**3-18D0*DS**2*M2+18D0*DS*
     &  M2**2-12D0*M2**3-DS**2*S+34D0*DS*M2*S+18D0*M2**2*S-39D0*DS*
     &  S**2+52D0*M2*S**2-32D0*S**3)/(S*(DS+S))-(6D0*S**(-2)*(M2-S)*
     &  (DS**3-3D0*DS**2*M2+3D0*DS*M2**2-2D0*M2**3+2D0*DS**2*S-3D0*
     &  DS*M2*S+4D0*M2**2*S+DS*S**2)*XLOGM2)/(DS+S)))/576D0
        C4(0,0,1,1) = ((6D0*LOGMU2+6D0*FB0(3)*(DS+S)**(-2)*(DS-M2+
     &  S)*(DS**4+2D0*DS**3*M2+3D0*DS**2*M2**2+DS**3*S+3D0*DS*M2**2*
     &  S-4D0*DS*M2*S**2+M2**2*S**2+DS*S**3-2D0*M2*S**3+S**4)-S**(-
     &  3)*(DS+S)**(-2)*(M2-S)**(-2)*(6D0*DS**5*M2**2+6D0*DS**4*M2**
     &  3+6D0*DS**3*M2**4-18D0*DS**2*M2**5-9D0*DS**5*M2*S-3D0*DS**4*
     &  M2**2*S-9D0*DS**3*M2**3*S+57D0*DS**2*M2**4*S+2D0*DS**5*S**2-
     &  7D0*DS**4*M2*S**2+2D0*DS**3*M2**2*S**2-75D0*DS**2*M2**3*S**
     &  2+18D0*DS*M2**4*S**2-6D0*M2**5*S**2+DS**4*S**3-4D0*DS**3*M2*
     &  S**3+38D0*DS**2*M2**2*S**3-45D0*DS*M2**3*S**3+9D0*M2**4*S**
     &  3+2D0*DS**3*S**4+3D0*DS**2*M2*S**4+14D0*DS*M2**2*S**4-2D0*
     &  M2**3*S**4-5D0*DS**2*S**5+35D0*DS*M2*S**5-13D0*M2**2*S**5-
     &  22D0*DS*S**6+26D0*M2*S**6-14D0*S**7)+6D0*S**(-4)*(DS+S)**(-
     &  2)*(M2-S)*(DS**5+DS**4*M2+DS**3*M2**2-3D0*DS**2*M2**3+DS**4*
     &  S+5D0*DS**2*M2**2*S-4D0*DS**2*M2*S**2+3D0*DS*M2**2*S**2-M2**
     &  3*S**2+DS**2*S**3-3D0*DS*M2*S**3+DS*S**4)*XLOGM2))/144D0
        C4(0,0,1,2) = ((6D0*LOGMU2+6D0*FB0(2)*(DS+S)**(-2)*(DS-M2+
     &  S)**2*(DS**2+3D0*DS*M2+M2*S-S**2)+(S**(-2)*(DS+S)**(-2)*
     &  (6D0*DS**4*M2+6D0*DS**3*M2**2-30D0*DS**2*M2**3+18D0*DS*M2**
     &  4-3D0*DS**4*S+3D0*DS**3*M2*S+39D0*DS**2*M2**2*S-27D0*DS*M2**
     &  3*S-18D0*M2**4*S-8D0*DS**2*M2*S**2+24D0*DS*M2**2*S**2+33D0*
     &  M2**3*S**2-DS**2*S**3-DS*M2*S**3-9D0*M2**2*S**3-14D0*DS*S**
     &  4+4D0*M2*S**4-10D0*S**5))/(M2-S)-6D0*S**(-3)*(DS+S)**(-2)*
     &  (M2-S)*(DS**4+DS**3*M2-5D0*DS**2*M2**2+3D0*DS*M2**3+DS**3*S+
     &  4D0*DS**2*M2*S-3D0*DS*M2**2*S-3D0*M2**3*S-DS**2*S**2+3D0*DS*
     &  M2*S**2+4D0*M2**2*S**2-DS*S**3)*XLOGM2))/288D0
        C4(0,0,2,2) = ((6D0*LOGMU2+6D0*FB0(1)*(DS+S)**(-2)*(DS-M2+
     &  S)**3-S**(-2)*(DS+S)**(-2)*(-(6D0*DS*M2**3)+6D0*DS**3*S-
     &  18D0*DS**2*M2*S+39D0*DS*M2**2*S-18D0*M2**3*S+2D0*DS**2*S**2-
     &  36D0*DS*M2*S**2+39D0*M2**2*S**2-14D0*DS*S**3-18D0*M2*S**3-
     &  10D0*S**4)-6D0*S**(-3)*(DS+S)**(-2)*(M2-S)*(DS*M2**3-DS**3*
     &  S+3D0*DS**2*M2*S-7D0*DS*M2**2*S+3D0*M2**3*S-2D0*DS**2*S**2+
     &  9D0*DS*M2*S**2-8D0*M2**2*S**2-DS*S**3+6D0*M2*S**3)*XLOGM2))/
     &  144D0
        C4(1,1,1,1) = (((720D0*LOGM12)/(M2-S)+3600D0*FC0(4)*(M2-S)**
     &  4-300D0*FB0(5)*(DS+S)**(-3)*(3D0*DS**7+7D0*DS**6*M2+13D0*
     &  DS**5*M2**2+25D0*DS**4*M2**3+5D0*DS**6*S+2D0*DS**5*M2*S-
     &  23D0*DS**4*M2**2*S+148D0*DS**3*M2**3*S+3D0*DS**5*S**2+13D0*
     &  DS**4*M2*S**2-330D0*DS**3*M2**2*S**2+258D0*DS**2*M2**3*S**2-
     &  3D0*DS**4*S**3+260D0*DS**3*M2*S**3-682D0*DS**2*M2**2*S**3+
     &  188D0*DS*M2**3*S**3-72D0*DS**3*S**4+604D0*DS**2*M2*S**4-
     &  538D0*DS*M2**2*S**4+50D0*M2**3*S**4-180D0*DS**2*S**5+512D0*
     &  DS*M2*S**5-150D0*M2**2*S**5-162D0*DS*S**6+150D0*M2*S**6-
     &  50D0*S**7)+S**(-5)*(DS+S)**(-3)*(M2-S)**(-4)*(900D0*DS**7*
     &  M2**4+2100D0*DS**6*M2**5+3900D0*DS**5*M2**6+7500D0*DS**4*
     &  M2**7-3150D0*DS**7*M2**3*S-6750D0*DS**6*M2**4*S-15150D0*DS**
     &  5*M2**5*S-37050D0*DS**4*M2**6*S+36180D0*DS**3*M2**7*S+
     &  3344D0*DS*S**10-5184D0*M2*S**10+1788D0*S**11+3900D0*DS**7*
     &  M2**2*S**2+7000D0*DS**6*M2**3*S**2+22450D0*DS**5*M2**4*S**2+
     &  75700D0*DS**4*M2**5*S**2-211950D0*DS**3*M2**6*S**2+39240D0*
     &  DS**2*M2**7*S**2-1875D0*DS**7*M2*S**3-1775D0*DS**6*M2**2*S**
     &  3-15675D0*DS**5*M2**3*S**3-82525D0*DS**4*M2**4*S**3+
     &  523680D0*DS**3*M2**5*S**3-248700D0*DS**2*M2**6*S**3+15240D0*
     &  DS*M2**7*S**3+180D0*DS**7*S**4-830D0*DS**6*M2*S**4+5205D0*
     &  DS**5*M2**2*S**4+51600D0*DS**4*M2**3*S**4-701595D0*DS**3*
     &  M2**4*S**4+660840D0*DS**2*M2**5*S**4-95700D0*DS*M2**6*S**4-
     &  720D0*M2**7*S**4+75D0*DS**6*S**5-1105D0*DS**5*M2*S**5-
     &  19130D0*DS**4*M2**2*S**5+539822D0*DS**3*M2**3*S**5-946510D0*
     &  DS**2*M2**4*S**5+255340D0*DS*M2**5*S**5+1800D0*M2**6*S**5+
     &  105D0*DS**5*S**6+4080D0*DS**4*M2*S**6-229486D0*DS**3*M2**2*
     &  S**6+770556D0*DS**2*M2**3*S**6-372510D0*DS*M2**4*S**6-
     &  1320D0*M2**5*S**6-355D0*DS**4*S**7+45116D0*DS**3*M2*S**7-
     &  336978D0*DS**2*M2**2*S**7+309506D0*DS*M2**3*S**7+180D0*M2**
     &  4*S**7-1857D0*DS**3*S**8+61288D0*DS**2*M2*S**8-132438D0*DS*
     &  M2**2*S**8-1608D0*M2**3*S**8+264D0*DS**2*S**9+17218D0*DS*M2*
     &  S**9+5064D0*M2**2*S**9)-60D0*S**(-6)*(DS+S)**(-3)*(15D0*DS**
     &  7*M2+35D0*DS**6*M2**2+65D0*DS**5*M2**3+125D0*DS**4*M2**4-
     &  15D0*DS**7*S-25D0*DS**6*M2*S-90D0*DS**5*M2**2*S-305D0*DS**4*
     &  M2**3*S+603D0*DS**3*M2**4*S-10D0*DS**6*S**2+30D0*DS**5*M2*
     &  S**2+270D0*DS**4*M2**2*S**2-2025D0*DS**3*M2**3*S**2+654D0*
     &  DS**2*M2**4*S**2-5D0*DS**5*S**3-110D0*DS**4*M2*S**3+2560D0*
     &  DS**3*M2**2*S**3-2510D0*DS**2*M2**3*S**3+254D0*DS*M2**4*S**
     &  3+20D0*DS**4*S**4-1430D0*DS**3*M2*S**4+3540D0*DS**2*M2**2*
     &  S**4-960D0*DS*M2**3*S**4-12D0*M2**4*S**4+280D0*DS**3*S**5-
     &  2160D0*DS**2*M2*S**5+1390D0*DS*M2**2*S**5+440D0*DS**2*S**6-
     &  910D0*DS*M2*S**6+190D0*DS*S**7)*XLOGM2))/3600D0
        C4(1,1,1,2) = ((DS+S)**(-3)*(-(12D0*FB0(4)*(2D0*DS*M2-DS*S+
     &  M2*S-S**2)*(2D0*DS**2*M2**2-2D0*DS**2*M2*S+2D0*DS*M2**2*S+
     &  DS**2*S**2-4D0*DS*M2*S**2+M2**2*S**2+2D0*DS*S**3-2D0*M2*S**
     &  3+S**4))-S**(-4)*(M2-S)**(-3)*(48D0*DS**3*M2**6-192D0*DS**3*
     &  M2**5*S+24D0*DS**2*M2**6*S+316D0*DS**3*M2**4*S**2-156D0*DS**
     &  2*M2**5*S**2+24D0*DS*M2**6*S**2-276D0*DS**3*M2**3*S**3+
     &  368D0*DS**2*M2**4*S**3-96D0*DS*M2**5*S**3-12D0*M2**6*S**3+
     &  136D0*DS**3*M2**2*S**4-416D0*DS**2*M2**3*S**4+170D0*DS*M2**
     &  4*S**4+30D0*M2**5*S**4-34D0*DS**3*M2*S**5+244D0*DS**2*M2**2*
     &  S**5-174D0*DS*M2**3*S**5-22D0*M2**4*S**5+3D0*DS**3*S**6-
     &  73D0*DS**2*M2*S**6+111D0*DS*M2**2*S**6+3D0*M2**3*S**6+9D0*
     &  DS**2*S**7-44D0*DS*M2*S**7+3D0*M2**2*S**7+9D0*DS*S**8-5D0*
     &  M2*S**8+3D0*S**9)+12D0*S**(-5)*(M2-S)*(4D0*DS**3*M2**3-6D0*
     &  DS**3*M2**2*S+2D0*DS**2*M2**3*S+4D0*DS**3*M2*S**2-8D0*DS**2*
     &  M2**2*S**2+2D0*DS*M2**3*S**2-DS**3*S**3+7D0*DS**2*M2*S**3-
     &  3D0*DS*M2**2*S**3-M2**3*S**3-2D0*DS**2*S**4+3D0*DS*M2*S**4-
     &  DS*S**5)*XLOGM2))/48D0
        C4(1,1,2,2) = -(((DS+S)**(-3)*(6D0*FB0(3)*(DS-M2+S)*(6D0*
     &  DS**2*M2**2-4D0*DS**2*M2*S+4D0*DS*M2**2*S+DS**2*S**2-6D0*DS*
     &  M2*S**2+M2**2*S**2+2D0*DS*S**3-2D0*M2*S**3+S**4)-S**(-3)*
     &  (M2-S)**(-2)*(36D0*DS**3*M2**4-36D0*DS**2*M2**5-78D0*DS**3*
     &  M2**3*S+102D0*DS**2*M2**4*S+12D0*DS*M2**5*S+54D0*DS**3*M2**
     &  2*S**2-126D0*DS**2*M2**3*S**2-24D0*M2**5*S**2-17D0*DS**3*M2*
     &  S**3+88D0*DS**2*M2**2*S**3-41D0*DS*M2**3*S**3+60D0*M2**4*S**
     &  3+2D0*DS**3*S**4-34D0*DS**2*M2*S**4+40D0*DS*M2**2*S**4-44D0*
     &  M2**3*S**4+6D0*DS**2*S**5-17D0*DS*M2*S**5+6D0*M2**2*S**5+
     &  6D0*DS*S**6+2D0*S**7)+6D0*S**(-4)*(M2-S)*(6D0*DS**3*M2**2-
     &  6D0*DS**2*M2**3-4D0*DS**3*M2*S+8D0*DS**2*M2**2*S+2D0*DS*M2**
     &  3*S+DS**3*S**2-7D0*DS**2*M2*S**2+3D0*DS*M2**2*S**2-4D0*M2**
     &  3*S**2+2D0*DS**2*S**3-3D0*DS*M2*S**3+4D0*M2**2*S**3+DS*S**
     &  4)*XLOGM2))/72D0)
        C4(1,2,2,2) = ((DS+S)**(-3)*(-(2D0*FB0(2)*(DS-M2+S)**2*(4D0*
     &  DS*M2-DS*S+M2*S-S**2))-(S**(-2)*(8D0*DS**3*M2**2-16D0*DS**2*
     &  M2**3+6D0*DS*M2**4-6D0*DS**3*M2*S+22D0*DS**2*M2**2*S-DS*M2**
     &  3*S-12D0*M2**4*S+DS**3*S**2-9D0*DS**2*M2*S**2-8D0*DS*M2**2*
     &  S**2+30D0*M2**3*S**2+3D0*DS**2*S**3-22D0*M2**2*S**3+3D0*DS*
     &  S**4+3D0*M2*S**4+S**5))/(M2-S)+2D0*S**(-3)*(M2-S)*(4D0*DS**
     &  3*M2-8D0*DS**2*M2**2+3D0*DS*M2**3-DS**3*S+7D0*DS**2*M2*S+DS*
     &  M2**2*S-6D0*M2**3*S-2D0*DS**2*S**2-3D0*DS*M2*S**2+12D0*M2**
     &  2*S**2-DS*S**3-6D0*M2*S**3)*XLOGM2))/24D0
        C4(2,2,2,2) = ((DS+S)**(-3)*(-(6D0*FB0(1)*(DS-M2+S)**3)+S**
     &  (-3)*(-(6D0*DS**2*M2**3)+21D0*DS**2*M2**2*S-18D0*DS*M2**3*S+
     &  6D0*DS**3*S**2-44D0*DS**2*M2*S**2+81D0*DS*M2**2*S**2-24D0*
     &  M2**3*S**2+18D0*DS**2*S**3-88D0*DS*M2*S**3+60D0*M2**2*S**3+
     &  18D0*DS*S**4-44D0*M2*S**4+6D0*S**5)+6D0*S**(-4)*(M2-S)*(DS**
     &  2*M2**3-4D0*DS**2*M2**2*S+3D0*DS*M2**3*S-DS**3*S**2+9D0*DS**
     &  2*M2*S**2-15D0*DS*M2**2*S**2+4D0*M2**3*S**2-6D0*DS**2*S**3+
     &  21D0*DS*M2*S**3-12D0*M2**2*S**3-9D0*DS*S**4+12D0*M2*S**4-
     &  4D0*S**5)*XLOGM2))/24D0

        dC111 = ( abs(dFC0(3)*(M2-S)**3) +abs(12D0*dFB0(4)
     &  *(DS+S)**(-2)*(2D0*DS**5+5D0*DS**4*M2+11D0*DS**3*
     &  M2**2+DS**4*S-7D0*DS**3*M2*S+51D0*DS**2*M2**2*S+2D0*DS**3*
     &  S**2-78D0*DS**2*M2*S**2+60D0*DS*M2**2*S**2+31D0*DS**2*S**3-
     &  110D0*DS*M2*S**3+22D0*M2**2*S**3+50D0*DS*S**4-44D0*M2*S**4+
     &  22D0*S**5)/72D0) )/abs(C3(1,1,1))
        dC222 = abs((DS+S)**(-2)*dFB0(1)*(DS-M2+S)**2/3D0)
     &          /abs(C3(2,2,2))

        elseif (s.eq.0d0) then

        C0 = FC0(0)-1D0/M2+LOGM12/M2
        C1(1) = FB0(2)*DS-FC0(1)*M2-(LOGM12)/(2D0*M2)+(M2**(-2)*
     &  (2D0*DS+9D0*M2))/12D0
        C1(2) = FB0(1)+1D0/(2D0*M2)
        C2(0,0) = (LOGMU2)/4D0+(FB0(1)*(DS-M2))/4D0+((DS+3D0*M2))/
     &  (8D0*M2)
        C2(1,1) = (LOGM12)/(3D0*M2)+FC0(2)*M2**2-(FB0(3)*DS*(DS+3D0*
     &  M2))/2D0-(M2**(-3)*(3D0*DS**2+15D0*DS*M2+44D0*M2**2))/72D0
        C2(1,2) = -(FB0(2)*M2)-1D0/(6D0*M2)
        C2(2,2) = (FB0(2)*(-(DS)+M2))/2D0-(M2**(-2)*(DS+2D0*M2))/
     &  12D0
        C3(1,1,1) = -((LOGM12)/(4D0*M2))-FC0(3)*M2**3+(FB0(4)*DS*
     &  (2D0*DS**2+5D0*DS*M2+11D0*M2**2))/6D0+(M2**(-4)*(12D0*DS**3+
     &  50D0*DS**2*M2+156D0*DS*M2**2+375D0*M2**3))/720D0
        C3(1,1,2) = 1D0/(12D0*M2)+FB0(3)*M2**2
        C3(1,2,2) = (M2**(-2)*(DS+M2))/24D0+(FB0(3)*M2*(DS-M2))/2D0
        C3(2,2,2) = (FB0(3)*(DS-M2)**2)/3D0+(M2**(-3)*(DS**2+3D0*
     &  M2**2))/36D0
        C3(0,0,1) = -((LOGMU2)/12D0)-(FB0(2)*(DS-M2)*(DS+2D0*M2))/
     &  12D0-(M2**(-2)*(DS**2+4D0*DS*M2+11D0*M2**2))/72D0
        C3(0,0,2) = -((LOGMU2)/12D0)-(FB0(2)*(DS-M2)**2)/12D0-(M2**
     &  (-2)*(DS**2+DS*M2+5D0*M2**2))/72D0
        C4(0,0,0,0) = -((FB0(2)*(DS-M2)**3)/96D0)-(LOGMU2*(DS-4D0*
     &  M2))/96D0-(M2**(-2)*(DS**3+7D0*DS*M2**2-44D0*M2**3))/576D0
        C4(0,0,1,1) = (LOGMU2)/24D0+(FB0(3)*(DS-M2)*(DS**2+2D0*DS*
     &  M2+3D0*M2**2))/24D0+(M2**(-3)*(DS**3+3D0*DS**2*M2+9D0*DS*
     &  M2**2+25D0*M2**3))/288D0
        C4(0,0,1,2) = (LOGMU2)/48D0+(FB0(3)*(DS-M2)**2*(DS+3D0*M2))/
     &  48D0+(M2**(-3)*(DS**3+3D0*DS**2*M2+3D0*DS*M2**2+13D0*M2**
     &  3))/576D0
        C4(0,0,2,2) = (LOGMU2)/24D0+(FB0(3)*(DS-M2)**3)/24D0+(M2**(-
     &  3)*(DS**3-DS**2*M2+3D0*DS*M2**2+7D0*M2**3))/288D0
        C4(1,1,1,1) = (LOGM12)/(5D0*M2)+FC0(4)*M2**4-(FB0(5)*DS*
     &  (3D0*DS**3+7D0*DS**2*M2+13D0*DS*M2**2+25D0*M2**3))/12D0-
     &  (M2**(-5)*(30D0*DS**4+115D0*DS**3*M2+310D0*DS**2*M2**2+
     &  770D0*DS*M2**3+1644D0*M2**4))/3600D0
        C4(1,1,1,2) = -(1D0/(20D0*M2))-FB0(4)*M2**3
        C4(1,1,2,2) = -((FB0(4)*M2**2*(DS-M2))/2D0)-(M2**(-2)*(3D0*
     &  DS+2D0*M2))/120D0
        C4(1,2,2,2) = -((FB0(4)*M2*(DS-M2)**2)/3D0)-(M2**(-3)*(3D0*
     &  DS**2-DS*M2+3D0*M2**2))/180D0
        C4(2,2,2,2) = -((FB0(4)*(DS-M2)**3)/4D0)-(M2**(-4)*(3D0*DS**
     &  3-4D0*DS**2*M2+4D0*DS*M2**2+12D0*M2**3))/240D0

        dC111 = ( abs(dFC0(3)*M2**3)+abs(dFB0(4)*DS*
     &  (2D0*DS**2+5D0*DS*M2+11D0*M2**2))/6D0 )/abs(C3(1,1,1))
        dC222 = abs(dFB0(3)*(DS-M2)**2)/3D0 /abs(C3(2,2,2))

        elseif (ds.eq.-s1) then

        C0 = FC0(0)-1D0/M2+LOGM12/M2
        C1(1) = 2D0*FB0(2)*DS-FC0(1)*(DS+M2)+(LOGM12*M2**(-2)*(DS-
     &  M2))/2D0+(M2**(-2)*(13D0*DS+9D0*M2))/12D0
        C1(2) = -(FB0(1))+1D0/(2D0*M2)
        C2(0,0) = (LOGMU2)/4D0+(FB0(1)*(DS+M2))/4D0-((DS-3D0*M2))/
     &  (8D0*M2)
        C2(1,1) = -(3D0*FB0(3)*DS*(DS+M2))+FC0(2)*(DS+M2)**2+(M2**(-
     &  3)*(41D0*DS**2+28D0*DS*M2-22D0*M2**2))/36D0+(LOGM12*M2**(-
     &  3)*(2D0*DS**2+DS*M2+2D0*M2**2))/6D0
        C2(1,2) = (FB0(2)*(DS+M2))/2D0+(M2**(-2)*(DS-2D0*M2))/12D0
        C2(2,2) = (FB0(2)*(DS+M2))/2D0+(M2**(-2)*(DS-2D0*M2))/12D0
        C3(1,1,1) = (11D0*FB0(4)*DS*(DS+M2)**2)/3D0-FC0(3)*(DS+M2)**
     &  3+(M2**(-4)*(747D0*DS**3+1249D0*DS**2*M2+597D0*DS*M2**2+
     &  375D0*M2**3))/720D0+(LOGM12*M2**(-4)*(3D0*DS**3+5D0*DS**2*
     &  M2+3D0*DS*M2**2-3D0*M2**3))/12D0
        C3(1,1,2) = -((FB0(3)*(DS+M2)**2)/3D0)+(M2**(-3)*(DS**2+3D0*
     &  M2**2))/36D0
        C3(1,2,2) = -((FB0(3)*(DS+M2)**2)/6D0)+(M2**(-3)*(DS**2+3D0*
     &  M2**2))/72D0
        C3(2,2,2) = -((FB0(3)*(DS+M2)**2)/3D0)+(M2**(-3)*(DS**2+3D0*
     &  M2**2))/36D0
        C3(0,0,1) = -((LOGMU2)/12D0)-(FB0(2)*(DS+M2)**2)/12D0-(M2**
     &  (-2)*(DS**2-DS*M2+11D0*M2**2))/72D0
        C3(0,0,2) = -((LOGMU2)/12D0)-(FB0(2)*(DS+M2)**2)/12D0-(M2**
     &  (-2)*(DS**2-DS*M2+5D0*M2**2))/72D0
        C4(0,0,0,0) = (FB0(2)*(DS+M2)**3)/96D0+(LOGMU2*(DS+4D0*M2))/
     &  96D0+(M2**(-2)*(DS**3+7D0*DS*M2**2+44D0*M2**3))/576D0
        C4(0,0,1,1) = (LOGMU2)/24D0+(FB0(3)*(DS+M2)**3)/24D0-(M2**(-
     &  3)*(DS**3+DS**2*M2+3D0*DS*M2**2-25D0*M2**3))/288D0
        C4(0,0,1,2) = (LOGMU2)/48D0+(FB0(3)*(DS+M2)**3)/48D0-(M2**(-
     &  3)*(DS**3+DS**2*M2+3D0*DS*M2**2-13D0*M2**3))/576D0
        C4(0,0,2,2) = (LOGMU2)/24D0+(FB0(3)*(DS+M2)**3)/24D0-(M2**(-
     &  3)*(DS**3+DS**2*M2+3D0*DS*M2**2-7D0*M2**3))/288D0
        C4(1,1,1,1) = -((25D0*FB0(5)*DS*(DS+M2)**3)/6D0)+FC0(4)*(DS+
     &  M2)**4+(LOGM12*M2**(-5)*(12D0*DS**4+33D0*DS**3*M2+32D0*DS**
     &  2*M2**2+8D0*DS*M2**3+12D0*M2**4))/60D0+(M2**(-5)*(3356D0*
     &  DS**4+9099D0*DS**3*M2+8536D0*DS**2*M2**2+2574D0*DS*M2**3-
     &  1644D0*M2**4))/3600D0
        C4(1,1,1,2) = (FB0(4)*(DS+M2)**3)/4D0+(M2**(-4)*(3D0*DS**3+
     &  4D0*DS**2*M2+4D0*DS*M2**2-12D0*M2**3))/240D0
        C4(1,1,2,2) = (FB0(4)*(DS+M2)**3)/12D0+(M2**(-4)*(3D0*DS**3+
     &  4D0*DS**2*M2+4D0*DS*M2**2-12D0*M2**3))/720D0
        C4(1,2,2,2) = (FB0(4)*(DS+M2)**3)/12D0+(M2**(-4)*(3D0*DS**3+
     &  4D0*DS**2*M2+4D0*DS*M2**2-12D0*M2**3))/720D0
        C4(2,2,2,2) = (FB0(4)*(DS+M2)**3)/4D0+(M2**(-4)*(3D0*DS**3+
     &  4D0*DS**2*M2+4D0*DS*M2**2-12D0*M2**3))/240D0

        dC111 = ( abs(11D0*dFB0(4)*DS*(DS+M2)**2)/3D0
     &           +abs(dFC0(3)*(DS+M2)**3) )/abs(C3(1,1,1))
        dC222 = abs((dFB0(3)*(DS+M2)**2)/3D0)/abs(C3(2,2,2))

        endif

c redundant coefficients
          C2(2,1) = C2(1,2)
          C3(1,2,1) = C3(1,1,2)
          C3(2,1,1) = C3(1,1,2)
          C3(2,1,2) = C3(1,2,2)
          C3(2,2,1) = C3(1,2,2)
          C4(0,0,2,1) = C4(0,0,1,2)
          C4(1,1,2,1) = C4(1,1,1,2)
          C4(1,2,1,1) = C4(1,1,1,2)
          C4(2,1,1,1) = C4(1,1,1,2)
          C4(1,2,1,2) = C4(1,1,2,2)
          C4(2,1,1,2) = C4(1,1,2,2)
          C4(2,1,2,1) = C4(1,1,2,2)
          C4(2,2,1,1) = C4(1,1,2,2)
          C4(1,2,2,1) = C4(1,1,2,2)
          C4(2,1,2,2) = C4(1,2,2,2)
          C4(2,2,1,2) = C4(1,2,2,2)
          C4(2,2,2,1) = C4(1,2,2,2)

        crelacc = max(dC111,dC222)

        end

**********************************************************************
        subroutine Ccoll3_dd(m12,s1,s2,M2,C0,C1,C2,C3,C4,crelacc)
**********************************************************************
*       C0(s1,m12,s2,M2,m12,0) for m12 -> 0
*       exact calculation, numerically stable for s1 -> s2
*---------------------------------------------------------------------
*       6.4.05 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)
        complex*16 M2,logm12,xlogm2,logmu2
        complex*16 fb0(0:5),fc0(0:5),dfb0(0:5),dfc0(0:5)
        complex*16 C0,C1(2),C2(0:2,0:2),C3(0:2,0:2,2)
        complex*16 C4(0:2,0:2,0:2,0:2)

        dc111 = 0d0
        dc222 = 0d0

        if (abs(s1).gt.1d-15) then
          s = s1
        else
          s = 0d0
        endif
        if (abs(s2).gt.1d-15) then
          ds = s2-s1
        else
          ds = -s1
        endif

        call BCfin_dd(s,ds,m12,M2,Logm12,xLogM2,Logmu2,C0,
     &                   fb0,fc0,dfb0,dfc0)

        if ((s.ne.0d0).and.(ds.ne.-s1)) then

        C1(1) = ((-(4D0*FB0(2)*(DS+S))-2D0*FC0(1)*(DS-M2+S)-LOGM12*
     &  (M2-S)**(-2)*(DS+M2-S)-M2*S**(-2)*(M2-S)**(-2)*(4D0*DS*M2-
     &  5D0*DS*S-M2*S+S**2)+(S**(-3)*(4D0*DS*M2**2-7D0*DS*M2*S-M2**
     &  2*S+2D0*DS*S**2+M2*S**2)*XLOGM2)/(M2-S)))/2D0
        C1(2) = ((-(2D0*FC0(1)*(M2-S))-(LOGM12)/(M2-S)+2D0*FB0(2)*
     &  (DS+2D0*S)+(S**(-2)*(2D0*DS*M2-DS*S+M2*S+2D0*S**2))/(M2-S)-
     &  S**(-3)*(2D0*DS*M2-2D0*DS*S+M2*S)*XLOGM2))/2D0
        C2(0,0) = ((LOGMU2+FB0(1)*(DS-M2+S)-((DS-M2-2D0*S))/S+(DS-
     &  M2)*S**(-2)*(M2-S)*XLOGM2))/4D0
        C2(1,1) = ((54D0*FB0(3)*(DS+S)*(DS-M2+S)+18D0*FC0(2)*(DS-M2+
     &  S)**2+3D0*LOGM12*(M2-S)**(-3)*(2D0*DS**2-DS*M2+2D0*M2**2+DS*
     &  S-4D0*M2*S+2D0*S**2)-S**(-3)*(M2-S)**(-3)*(54D0*DS**2*M2**3-
     &  54D0*DS*M2**4-129D0*DS**2*M2**2*S+177D0*DS*M2**3*S+6D0*M2**
     &  4*S+84D0*DS**2*M2*S**2-201D0*DS*M2**2*S**2-9D0*M2**3*S**2-
     &  5D0*DS**2*S**3+79D0*DS*M2*S**3-5D0*M2**2*S**3-DS*S**4+13D0*
     &  M2*S**4-5D0*S**5)+3D0*S**(-4)*(M2-S)**(-2)*(18D0*DS**2*M2**
     &  3-18D0*DS*M2**4-52D0*DS**2*M2**2*S+68D0*DS*M2**3*S+2D0*M2**
     &  4*S+48D0*DS**2*M2*S**2-95D0*DS*M2**2*S**2-4D0*M2**3*S**2-
     &  12D0*DS**2*S**3+57D0*DS*M2*S**3+2D0*M2**2*S**3-12D0*DS*S**
     &  4)*XLOGM2))/18D0
        C2(1,2) = ((36D0*FC0(2)*(DS-M2+S)*(M2-S)+6D0*LOGM12*(M2-S)**
     &  (-2)*(2D0*DS+M2-S)-18D0*FB0(3)*(DS**2-5D0*DS*M2+7D0*DS*S-
     &  6D0*M2*S+6D0*S**2)+S**(-3)*(M2-S)**(-2)*(18D0*DS**2*M2**2-
     &  90D0*DS*M2**3-27D0*DS**2*M2*S+231D0*DS*M2**2*S-6D0*M2**3*S+
     &  6D0*DS**2*S**2-162D0*DS*M2*S**2+3D0*M2**2*S**2+7D0*DS*S**3+
     &  2D0*M2*S**3+S**4)-(6D0*S**(-4)*(3D0*DS**2*M2**2-15D0*DS*M2**
     &  3-6D0*DS**2*M2*S+46D0*DS*M2**2*S-M2**3*S+3D0*DS**2*S**2-
     &  45D0*DS*M2*S**2+M2**2*S**2+12D0*DS*S**3)*XLOGM2)/(M2-S)))/
     &  36D0
        C2(2,2) = (((12D0*LOGM12)/(M2-S)+36D0*FC0(2)*(M2-S)**2-
     &  (18D0*FB0(3)*(DS**3+3D0*DS**2*M2-DS**2*S+10D0*DS*M2*S-8D0*
     &  DS*S**2+6D0*M2*S**2-6D0*S**3))/(DS+S)+(S**(-3)*(M2-S)**(-2)*
     &  (18D0*DS**3*M2**2+54D0*DS**2*M2**3-27D0*DS**3*M2*S-117D0*
     &  DS**2*M2**2*S+114D0*DS*M2**3*S+6D0*DS**3*S**2+72D0*DS**2*M2*
     &  S**2-255D0*DS*M2**2*S**2-12D0*M2**3*S**2-15D0*DS**2*S**3+
     &  130D0*DS*M2*S**3+6D0*M2**2*S**3+5D0*DS*S**4-20D0*M2*S**4+
     &  26D0*S**5))/(DS+S)-(6D0*S**(-4)*(3D0*DS**3*M2+9D0*DS**2*M2**
     &  2-3D0*DS**3*S-15D0*DS**2*M2*S+19D0*DS*M2**2*S+6D0*DS**2*S**
     &  2-33D0*DS*M2*S**2-2D0*M2**2*S**2+12D0*DS*S**3)*XLOGM2)/(DS+
     &  S)))/36D0
        C3(1,1,1) = ((-(264D0*FB0(4)*(DS+S)*(DS-M2+S)**2)-72D0*
     &  FC0(3)*(DS-M2+S)**3-6D0*LOGM12*(M2-S)**(-4)*(3D0*DS**3-5D0*
     &  DS**2*M2+3D0*DS*M2**2+3D0*M2**3+5D0*DS**2*S-6D0*DS*M2*S-9D0*
     &  M2**2*S+3D0*DS*S**2+9D0*M2*S**2-3D0*S**3)-S**(-4)*(M2-S)**(-
     &  4)*(264D0*DS**3*M2**4-528D0*DS**2*M2**5+264D0*DS*M2**6-
     &  906D0*DS**3*M2**3*S+2322D0*DS**2*M2**4*S-1398D0*DS*M2**5*S-
     &  18D0*M2**6*S+1081D0*DS**3*M2**2*S**2-3917D0*DS**2*M2**3*S**
     &  2+3031D0*DS*M2**4*S**2+45D0*M2**5*S**2-472D0*DS**3*M2*S**3+
     &  3049D0*DS**2*M2**2*S**3-3384D0*DS*M2**3*S**3-33D0*M2**4*S**
     &  3+24D0*DS**3*S**4-962D0*DS**2*M2*S**4+1971D0*DS*M2**2*S**4+
     &  33D0*M2**3*S**4+36D0*DS**2*S**5-508D0*DS*M2*S**5-81D0*M2**2*
     &  S**5+24D0*DS*S**6+78D0*M2*S**6-24D0*S**7)+6D0*S**(-5)*(M2-
     &  S)**(-3)*(44D0*DS**3*M2**4-88D0*DS**2*M2**5+44D0*DS*M2**6-
     &  173D0*DS**3*M2**3*S+431D0*DS**2*M2**4*S-255D0*DS*M2**5*S-
     &  3D0*M2**6*S+252D0*DS**3*M2**2*S**2-839D0*DS**2*M2**3*S**2+
     &  618D0*DS*M2**4*S**2+9D0*M2**5*S**2-158D0*DS**3*M2*S**3+
     &  806D0*DS**2*M2**2*S**3-799D0*DS*M2**3*S**3-9D0*M2**4*S**3+
     &  32D0*DS**3*S**4-374D0*DS**2*M2*S**4+576D0*DS*M2**2*S**4+3D0*
     &  M2**3*S**4+64D0*DS**2*S**5-216D0*DS*M2*S**5+32D0*DS*S**6)*
     &  XLOGM2))/72D0
        C3(1,1,2) = ((-(72D0*FC0(3)*(DS-M2+S)**2*(M2-S))-6D0*LOGM12*
     &  (M2-S)**(-3)*(3D0*DS**2-2D0*DS*M2+M2**2+2D0*DS*S-2D0*M2*S+
     &  S**2)+24D0*FB0(4)*(DS-M2+S)*(DS**2-10D0*DS*M2+12D0*DS*S-
     &  11D0*M2*S+11D0*S**2)+S**(-4)*(M2-S)**(-3)*(24D0*DS**3*M2**3-
     &  264D0*DS**2*M2**4+240D0*DS*M2**5-60D0*DS**3*M2**2*S+930D0*
     &  DS**2*M2**3*S-1092D0*DS*M2**4*S+6D0*M2**5*S+44D0*DS**3*M2*
     &  S**2-1141D0*DS**2*M2**2*S**2+1886D0*DS*M2**3*S**2-9D0*M2**4*
     &  S**2-6D0*DS**3*S**3+516D0*DS**2*M2*S**3-1490D0*DS*M2**2*S**
     &  3+2D0*M2**3*S**3-28D0*DS**2*S**4+470D0*DS*M2*S**4-5D0*M2**2*
     &  S**4-14D0*DS*S**5+10D0*M2*S**5-4D0*S**6)-6D0*S**(-5)*(M2-
     &  S)**(-2)*(4D0*DS**3*M2**3-44D0*DS**2*M2**4+40D0*DS*M2**5-
     &  12D0*DS**3*M2**2*S+177D0*DS**2*M2**3*S-202D0*DS*M2**4*S+M2**
     &  5*S+12D0*DS**3*M2*S**2-264D0*DS**2*M2**2*S**2+402D0*DS*M2**
     &  3*S**2-2D0*M2**4*S**2-4D0*DS**3*S**3+170D0*DS**2*M2*S**3-
     &  392D0*DS*M2**2*S**3+M2**3*S**3-36D0*DS**2*S**4+184D0*DS*M2*
     &  S**4-32D0*DS*S**5)*XLOGM2))/72D0
        C3(1,2,2) = ((-(72D0*FC0(3)*(DS-M2+S)*(M2-S)**2)-6D0*LOGM12*
     &  (M2-S)**(-2)*(3D0*DS+M2-S)+(12D0*FB0(4)*(DS**4+4D0*DS**3*M2-
     &  17D0*DS**2*M2**2-2D0*DS**3*S+48D0*DS**2*M2*S-40D0*DS*M2**2*
     &  S-29D0*DS**2*S**2+88D0*DS*M2*S**2-22D0*M2**2*S**2-48D0*DS*
     &  S**3+44D0*M2*S**3-22D0*S**4))/(DS+S)+(S**(-4)*(M2-S)**(-3)*
     &  (12D0*DS**4*M2**3+48D0*DS**3*M2**4-204D0*DS**2*M2**5-30D0*
     &  DS**4*M2**2*S-156D0*DS**3*M2**3*S+1020D0*DS**2*M2**4*S-
     &  258D0*DS*M2**5*S+22D0*DS**4*M2*S**2+178D0*DS**3*M2**2*S**2-
     &  1925D0*DS**2*M2**3*S**2+1125D0*DS*M2**4*S**2+6D0*M2**5*S**2-
     &  3D0*DS**4*S**3-78D0*DS**3*M2*S**3+1658D0*DS**2*M2**2*S**3-
     &  1895D0*DS*M2**3*S**3-9D0*M2**4*S**3+10D0*DS**3*S**4-578D0*
     &  DS**2*M2*S**4+1488D0*DS*M2**2*S**4+2D0*M2**3*S**4+31D0*DS**
     &  2*S**5-480D0*DS*M2*S**5+M2**2*S**5+20D0*DS*S**6-2D0*M2*S**6+
     &  2D0*S**7))/(DS+S)-(6D0*S**(-5)*(2D0*DS**4*M2**2+8D0*DS**3*
     &  M2**3-34D0*DS**2*M2**4-4D0*DS**4*M2*S-22D0*DS**3*M2**2*S+
     &  153D0*DS**2*M2**3*S-43D0*DS*M2**4*S+2D0*DS**4*S**2+20D0*DS**
     &  3*M2*S**2-250D0*DS**2*M2**2*S**2+166D0*DS*M2**3*S**2+M2**4*
     &  S**2-6D0*DS**3*S**3+174D0*DS**2*M2*S**3-240D0*DS*M2**2*S**3-
     &  M2**3*S**3-40D0*DS**2*S**4+152D0*DS*M2*S**4-32D0*DS*S**5)*
     &  XLOGM2)/((DS+S)*(M2-S))))/72D0
        C3(2,2,2) = ((-((18D0*LOGM12)/(M2-S))-72D0*FC0(3)*(M2-S)**3+
     &  12D0*FB0(4)*(DS+S)**(-2)*(2D0*DS**5+5D0*DS**4*M2+11D0*DS**3*
     &  M2**2+DS**4*S-7D0*DS**3*M2*S+51D0*DS**2*M2**2*S+2D0*DS**3*
     &  S**2-78D0*DS**2*M2*S**2+60D0*DS*M2**2*S**2+31D0*DS**2*S**3-
     &  110D0*DS*M2*S**3+22D0*M2**2*S**3+50D0*DS*S**4-44D0*M2*S**4+
     &  22D0*S**5)+S**(-4)*(DS+S)**(-2)*(M2-S)**(-3)*(24D0*DS**5*
     &  M2**3+60D0*DS**4*M2**4+132D0*DS**3*M2**5-60D0*DS**5*M2**2*S-
     &  162D0*DS**4*M2**3*S-474D0*DS**3*M2**4*S+462D0*DS**2*M2**5*S+
     &  44D0*DS**5*M2*S**2+140D0*DS**4*M2**2*S**2+638D0*DS**3*M2**3*
     &  S**2-1893D0*DS**2*M2**4*S**2+228D0*DS*M2**5*S**2-6D0*DS**5*
     &  S**3-37D0*DS**4*M2*S**3-387D0*DS**3*M2**2*S**3+2974D0*DS**2*
     &  M2**3*S**3-1062D0*DS*M2**4*S**3+18D0*M2**5*S**3+5D0*DS**4*
     &  S**4+111D0*DS**3*M2*S**4-2118D0*DS**2*M2**2*S**4+1876D0*DS*
     &  M2**3*S**4-27D0*M2**4*S**4-14D0*DS**3*S**5+604D0*DS**2*M2*
     &  S**5-1418D0*DS*M2**2*S**5+6D0*M2**3*S**5-25D0*DS**2*S**6+
     &  334D0*DS*M2*S**6+39D0*M2**2*S**6+42D0*DS*S**7-78D0*M2*S**7+
     &  42D0*S**8)-6D0*S**(-5)*(DS+S)**(-2)*(4D0*DS**5*M2+10D0*DS**
     &  4*M2**2+22D0*DS**3*M2**3-4D0*DS**5*S-12D0*DS**4*M2*S-46D0*
     &  DS**3*M2**2*S+77D0*DS**2*M2**3*S+2D0*DS**4*S**2+30D0*DS**3*
     &  M2*S**2-200D0*DS**2*M2**2*S**2+38D0*DS*M2**3*S**2-6D0*DS**3*
     &  S**3+170D0*DS**2*M2*S**3-120D0*DS*M2**2*S**3+3D0*M2**3*S**3-
     &  44D0*DS**2*S**4+120D0*DS*M2*S**4-32D0*DS*S**5)*XLOGM2))/72D0
        C3(0,0,1) = ((-(6D0*LOGMU2)-6D0*FB0(2)*(DS-M2+S)**2-(S**(-
     &  2)*(6D0*DS**2*M2-12D0*DS*M2**2+6D0*M2**3-3D0*DS**2*S+12D0*
     &  DS*M2*S-3D0*M2**2*S+10D0*M2*S**2-13D0*S**3))/(M2-S)+6D0*S**
     &  (-3)*(M2-S)*(DS**2-2D0*DS*M2+M2**2+DS*S)*XLOGM2))/72D0
        C3(0,0,2) = ((-(6D0*LOGMU2)-(6D0*FB0(2)*(DS-M2+S)*(DS**2+
     &  2D0*DS*M2+M2*S-S**2))/(DS+S)-(S**(-2)*(6D0*DS**3*M2+6D0*DS**
     &  2*M2**2-12D0*DS*M2**3-3D0*DS**3*S-3D0*DS**2*M2*S+18D0*DS*
     &  M2**2*S+6D0*M2**3*S+3D0*DS**2*S**2+DS*M2*S**2-3D0*M2**2*S**
     &  2-7D0*DS*S**3+10D0*M2*S**3-13D0*S**4))/((DS+S)*(M2-S))+(6D0*
     &  S**(-3)*(M2-S)*(DS**3+DS**2*M2-2D0*DS*M2**2+2D0*DS*M2*S+M2**
     &  2*S-DS*S**2)*XLOGM2)/(DS+S)))/72D0
        C4(0,0,0,0) = ((-((6D0*FB0(1)*(DS-M2+S)**3)/(DS+S))-6D0*
     &  LOGMU2*(DS-4D0*M2+2D0*S)+(6D0*DS**3-18D0*DS**2*M2+18D0*DS*
     &  M2**2-12D0*M2**3-DS**2*S+34D0*DS*M2*S+18D0*M2**2*S-39D0*DS*
     &  S**2+52D0*M2*S**2-32D0*S**3)/(S*(DS+S))-(6D0*S**(-2)*(M2-S)*
     &  (DS**3-3D0*DS**2*M2+3D0*DS*M2**2-2D0*M2**3+2D0*DS**2*S-3D0*
     &  DS*M2*S+4D0*M2**2*S+DS*S**2)*XLOGM2)/(DS+S)))/576D0
        C4(0,0,1,1) = ((6D0*LOGMU2+6D0*FB0(3)*(DS-M2+S)**3-S**(-3)*
     &  (M2-S)**(-2)*(6D0*DS**3*M2**2-18D0*DS**2*M2**3+18D0*DS*M2**
     &  4-6D0*M2**5-9D0*DS**3*M2*S+39D0*DS**2*M2**2*S-45D0*DS*M2**3*
     &  S+9D0*M2**4*S+2D0*DS**3*S**2-24D0*DS**2*M2*S**2+39D0*DS*M2**
     &  2*S**2-2D0*M2**3*S**2+3D0*DS**2*S**3-15D0*DS*M2*S**3-13D0*
     &  M2**2*S**3+3D0*DS*S**4+26D0*M2*S**4-14D0*S**5)+6D0*S**(-4)*
     &  (M2-S)*(DS**3-3D0*DS**2*M2+3D0*DS*M2**2-M2**3+2D0*DS**2*S-
     &  3D0*DS*M2*S+DS*S**2)*XLOGM2))/144D0
        C4(0,0,1,2) = ((6D0*LOGMU2+(6D0*FB0(3)*(DS-M2+S)**2*(DS**2+
     &  3D0*DS*M2-DS*S+2D0*M2*S-2D0*S**2))/(DS+S)-(S**(-3)*(M2-S)**
     &  (-2)*(6D0*DS**4*M2**2+6D0*DS**3*M2**3-30D0*DS**2*M2**4+18D0*
     &  DS*M2**5-9D0*DS**4*M2*S-9D0*DS**3*M2**2*S+87D0*DS**2*M2**3*
     &  S-63D0*DS*M2**4*S-6D0*M2**5*S+2D0*DS**4*S**2+2D0*DS**3*M2*
     &  S**2-91D0*DS**2*M2**2*S**2+96D0*DS*M2**3*S**2+9D0*M2**4*S**
     &  2-DS**3*S**3+37D0*DS**2*M2*S**3-89D0*DS*M2**2*S**3-2D0*M2**
     &  3*S**3-3D0*DS**2*S**4+52D0*DS*M2*S**4-13D0*M2**2*S**4-14D0*
     &  DS*S**5+26D0*M2*S**5-14D0*S**6))/(DS+S)+(6D0*S**(-4)*(M2-S)*
     &  (DS**4+DS**3*M2-5D0*DS**2*M2**2+3D0*DS*M2**3+7D0*DS**2*M2*S-
     &  6D0*DS*M2**2*S-M2**3*S-3D0*DS**2*S**2+6D0*DS*M2*S**2-2D0*DS*
     &  S**3)*XLOGM2)/(DS+S)))/288D0
        C4(0,0,2,2) = ((6D0*LOGMU2+6D0*FB0(3)*(DS+S)**(-2)*(DS-M2+
     &  S)*(DS**4+2D0*DS**3*M2+3D0*DS**2*M2**2+DS**3*S+3D0*DS*M2**2*
     &  S-4D0*DS*M2*S**2+M2**2*S**2+DS*S**3-2D0*M2*S**3+S**4)-S**(-
     &  3)*(DS+S)**(-2)*(M2-S)**(-2)*(6D0*DS**5*M2**2+6D0*DS**4*M2**
     &  3+6D0*DS**3*M2**4-18D0*DS**2*M2**5-9D0*DS**5*M2*S-3D0*DS**4*
     &  M2**2*S-9D0*DS**3*M2**3*S+57D0*DS**2*M2**4*S+2D0*DS**5*S**2-
     &  7D0*DS**4*M2*S**2+2D0*DS**3*M2**2*S**2-75D0*DS**2*M2**3*S**
     &  2+18D0*DS*M2**4*S**2-6D0*M2**5*S**2+DS**4*S**3-4D0*DS**3*M2*
     &  S**3+38D0*DS**2*M2**2*S**3-45D0*DS*M2**3*S**3+9D0*M2**4*S**
     &  3+2D0*DS**3*S**4+3D0*DS**2*M2*S**4+14D0*DS*M2**2*S**4-2D0*
     &  M2**3*S**4-5D0*DS**2*S**5+35D0*DS*M2*S**5-13D0*M2**2*S**5-
     &  22D0*DS*S**6+26D0*M2*S**6-14D0*S**7)+6D0*S**(-4)*(DS+S)**(-
     &  2)*(M2-S)*(DS**5+DS**4*M2+DS**3*M2**2-3D0*DS**2*M2**3+DS**4*
     &  S+5D0*DS**2*M2**2*S-4D0*DS**2*M2*S**2+3D0*DS*M2**2*S**2-M2**
     &  3*S**2+DS**2*S**3-3D0*DS*M2*S**3+DS*S**4)*XLOGM2))/144D0
        C4(1,1,1,1) = ((7500D0*FB0(5)*(DS+S)*(DS-M2+S)**3+1800D0*
     &  FC0(4)*(DS-M2+S)**4+30D0*LOGM12*(M2-S)**(-5)*(12D0*DS**4-
     &  33D0*DS**3*M2+32D0*DS**2*M2**2-8D0*DS*M2**3+12D0*M2**4+33D0*
     &  DS**3*S-64D0*DS**2*M2*S+24D0*DS*M2**2*S-48D0*M2**3*S+32D0*
     &  DS**2*S**2-24D0*DS*M2*S**2+72D0*M2**2*S**2+8D0*DS*S**3-48D0*
     &  M2*S**3+12D0*S**4)-S**(-5)*(M2-S)**(-5)*(7500D0*DS**4*M2**5-
     &  22500D0*DS**3*M2**6+22500D0*DS**2*M2**7-7500D0*DS*M2**8-
     &  33390D0*DS**4*M2**4*S+122310D0*DS**3*M2**5*S-144090D0*DS**2*
     &  M2**6*S+54810D0*DS*M2**7*S+360D0*M2**8*S+57130D0*DS**4*M2**
     &  3*S**2-270030D0*DS**3*M2**4*S**2+389010D0*DS**2*M2**5*S**2-
     &  174400D0*DS*M2**6*S**2-1260D0*M2**7*S**2-45305D0*DS**4*M2**
     &  2*S**3+304890D0*DS**3*M2**3*S**3-568905D0*DS**2*M2**4*S**3+
     &  313235D0*DS*M2**5*S**3+1560D0*M2**6*S**3+14815D0*DS**4*M2*
     &  S**4-178755D0*DS**3*M2**2*S**4+478455D0*DS**2*M2**3*S**4-
     &  343390D0*DS*M2**4*S**4-750D0*M2**5*S**4-606D0*DS**4*S**5+
     &  45684D0*DS**3*M2*S**5-222321D0*DS**2*M2**2*S**5+230434D0*DS*
     &  M2**3*S**5-606D0*M2**4*S**5-1599D0*DS**3*S**6+46887D0*DS**2*
     &  M2*S**6-88322D0*DS*M2**2*S**6+2664D0*M2**3*S**6-1536D0*DS**
     &  2*S**7+15457D0*DS*M2*S**7-3876D0*M2**2*S**7-324D0*DS*S**8+
     &  2514D0*M2*S**8-606D0*S**9)+30D0*S**(-6)*(M2-S)**(-4)*(250D0*
     &  DS**4*M2**5-750D0*DS**3*M2**6+750D0*DS**2*M2**7-250D0*DS*
     &  M2**8-1238D0*DS**4*M2**4*S+4452D0*DS**3*M2**5*S-5178D0*DS**
     &  2*M2**6*S+1952D0*DS*M2**7*S+12D0*M2**8*S+2440D0*DS**4*M2**3*
     &  S**2-10977D0*DS**3*M2**4*S**2+15306D0*DS**2*M2**5*S**2-
     &  6706D0*DS*M2**6*S**2-48D0*M2**7*S**2-2380D0*DS**4*M2**2*S**
     &  3+14355D0*DS**3*M2**3*S**3-25078D0*DS**2*M2**4*S**3+13206D0*
     &  DS*M2**5*S**3+72D0*M2**6*S**3+1130D0*DS**4*M2*S**4-10440D0*
     &  DS**3*M2**2*S**4+24530D0*DS**2*M2**3*S**4-16252D0*DS*M2**4*
     &  S**4-48D0*M2**5*S**4-190D0*DS**4*S**5+3930D0*DS**3*M2*S**5-
     &  14230D0*DS**2*M2**2*S**5+12740D0*DS*M2**3*S**5+12D0*M2**4*
     &  S**5-570D0*DS**3*S**6+4470D0*DS**2*M2*S**6-6170D0*DS*M2**2*
     &  S**6-570D0*DS**2*S**7+1670D0*DS*M2*S**7-190D0*DS*S**8)*
     &  XLOGM2))/1800D0
        C4(1,1,1,2) = ((3600D0*FC0(4)*(DS-M2+S)**3*(M2-S)-300D0*
     &  FB0(5)*(DS-M2+S)**2*(3D0*DS**2-47D0*DS*M2+53D0*DS*S-50D0*M2*
     &  S+50D0*S**2)+60D0*LOGM12*(M2-S)**(-4)*(12D0*DS**3-21D0*DS**
     &  2*M2+11D0*DS*M2**2+3D0*M2**3+21D0*DS**2*S-22D0*DS*M2*S-9D0*
     &  M2**2*S+11D0*DS*S**2+9D0*M2*S**2-3D0*S**3)+S**(-5)*(M2-S)**
     &  (-4)*(900D0*DS**4*M2**4-15900D0*DS**3*M2**5+29100D0*DS**2*
     &  M2**6-14100D0*DS*M2**7-3150D0*DS**4*M2**3*S+71730D0*DS**3*
     &  M2**4*S-160590D0*DS**2*M2**5*S+92190D0*DS*M2**6*S-180D0*M2**
     &  7*S+3900D0*DS**4*M2**2*S**2-124460D0*DS**3*M2**3*S**2+
     &  359170D0*DS**2*M2**4*S**2-252260D0*DS*M2**5*S**2+450D0*M2**
     &  6*S**2-1875D0*DS**4*M2*S**3+100285D0*DS**3*M2**2*S**3-
     &  410085D0*DS**2*M2**3*S**3+372005D0*DS*M2**4*S**3-330D0*M2**
     &  5*S**3+180D0*DS**4*S**4-33560D0*DS**3*M2*S**4+242865D0*DS**
     &  2*M2**2*S**4-314550D0*DS*M2**3*S**4+45D0*M2**4*S**4+1527D0*
     &  DS**3*S**5-62611D0*DS**2*M2*S**5+146816D0*DS*M2**2*S**5+
     &  258D0*M2**3*S**5+2151D0*DS**2*S**6-31142D0*DS*M2*S**6-714D0*
     &  M2**2*S**6+1041D0*DS*S**7+684D0*M2*S**7-213D0*S**8)-60D0*S**
     &  (-6)*(M2-S)**(-3)*(15D0*DS**4*M2**4-265D0*DS**3*M2**5+485D0*
     &  DS**2*M2**6-235D0*DS*M2**7-60D0*DS**4*M2**3*S+1328D0*DS**3*
     &  M2**4*S-2919D0*DS**2*M2**5*S+1654D0*DS*M2**6*S-3D0*M2**7*S+
     &  90D0*DS**4*M2**2*S**2-2650D0*DS**3*M2**3*S**2+7284D0*DS**2*
     &  M2**4*S**2-4953D0*DS*M2**5*S**2+9D0*M2**6*S**2-60D0*DS**4*
     &  M2*S**3+2620D0*DS**3*M2**2*S**3-9625D0*DS**2*M2**3*S**3+
     &  8184D0*DS*M2**4*S**3-9D0*M2**5*S**3+15D0*DS**4*S**4-1265D0*
     &  DS**3*M2*S**4+7065D0*DS**2*M2**2*S**4-8050D0*DS*M2**3*S**4+
     &  3D0*M2**4*S**4+220D0*DS**3*S**5-2685D0*DS**2*M2*S**5+4690D0*
     &  DS*M2**2*S**5+395D0*DS**2*S**6-1480D0*DS*M2*S**6+190D0*DS*
     &  S**7)*XLOGM2))/3600D0
        C4(1,1,2,2) = ((3600D0*FC0(4)*(DS-M2+S)**2*(M2-S)**2+60D0*
     &  LOGM12*(M2-S)**(-3)*(12D0*DS**2-9D0*DS*M2+2D0*M2**2+9D0*DS*
     &  S-4D0*M2*S+2D0*S**2)-(300D0*FB0(5)*(DS-M2+S)*(DS**4+6D0*DS**
     &  3*M2-43D0*DS**2*M2**2-4D0*DS**3*S+106D0*DS**2*M2*S-94D0*DS*
     &  M2**2*S-61D0*DS**2*S**2+200D0*DS*M2*S**2-50D0*M2**2*S**2-
     &  106D0*DS*S**3+100D0*M2*S**3-50D0*S**4))/(DS+S)+(S**(-5)*(M2-
     &  S)**(-4)*(300D0*DS**5*M2**4+1500D0*DS**4*M2**5-14700D0*DS**
     &  3*M2**6+12900D0*DS**2*M2**7-1050D0*DS**5*M2**3*S-6450D0*DS**
     &  4*M2**4*S+84030D0*DS**3*M2**5*S-101910D0*DS**2*M2**6*S+
     &  14580D0*DS*M2**7*S+1300D0*DS**5*M2**2*S**2+10700D0*DS**4*
     &  M2**3*S**2-194590D0*DS**3*M2**4*S**2+327220D0*DS**2*M2**5*
     &  S**2-93510D0*DS*M2**6*S**2-120D0*M2**7*S**2-625D0*DS**5*M2*
     &  S**3-8325D0*DS**4*M2**2*S**3+230095D0*DS**3*M2**3*S**3-
     &  554985D0*DS**2*M2**4*S**3+253320D0*DS*M2**5*S**3+300D0*M2**
     &  6*S**3+60D0*DS**5*S**4+2800D0*DS**4*M2*S**4-141355D0*DS**3*
     &  M2**2*S**4+531870D0*DS**2*M2**3*S**4-372165D0*DS*M2**4*S**4-
     &  220D0*M2**5*S**4-255D0*DS**4*S**5+38277D0*DS**3*M2*S**5-
     &  278334D0*DS**2*M2**2*S**5+314602D0*DS*M2**3*S**5+30D0*M2**4*
     &  S**5-1787D0*DS**3*S**6+65570D0*DS**2*M2*S**6-146840D0*DS*
     &  M2**2*S**6+92D0*M2**3*S**6-2331D0*DS**2*S**7+30934D0*DS*M2*
     &  S**7-236D0*M2**2*S**7-921D0*DS*S**8+216D0*M2*S**8-62D0*S**
     &  9))/(DS+S)-(60D0*S**(-6)*(M2-S)**(-2)*(5D0*DS**5*M2**3+25D0*
     &  DS**4*M2**4-245D0*DS**3*M2**5+215D0*DS**2*M2**6-15D0*DS**5*
     &  M2**2*S-95D0*DS**4*M2**3*S+1278D0*DS**3*M2**4*S-1591D0*DS**
     &  2*M2**5*S+243D0*DS*M2**6*S+15D0*DS**5*M2*S**2+135D0*DS**4*
     &  M2**2*S**2-2645D0*DS**3*M2**3*S**2+4694D0*DS**2*M2**4*S**2-
     &  1437D0*DS*M2**5*S**2-2D0*M2**6*S**2-5D0*DS**5*S**3-85D0*DS**
     &  4*M2*S**3+2705D0*DS**3*M2**2*S**3-7150D0*DS**2*M2**3*S**3+
     &  3544D0*DS*M2**4*S**3+4D0*M2**5*S**3+20D0*DS**4*S**4-1350D0*
     &  DS**3*M2*S**4+5950D0*DS**2*M2**2*S**4-4650D0*DS*M2**3*S**4-
     &  2D0*M2**4*S**4+245D0*DS**3*S**5-2540D0*DS**2*M2*S**5+3400D0*
     &  DS*M2**2*S**5+410D0*DS**2*S**6-1290D0*DS*M2*S**6+190D0*DS*
     &  S**7)*XLOGM2)/(DS+S)))/3600D0
        C4(1,2,2,2) = ((3600D0*FC0(4)*(DS-M2+S)*(M2-S)**3+180D0*
     &  LOGM12*(M2-S)**(-2)*(4D0*DS+M2-S)-300D0*FB0(5)*(DS+S)**(-2)*
     &  (DS**6+3D0*DS**5*M2+9D0*DS**4*M2**2-37D0*DS**3*M2**3-9D0*
     &  DS**4*M2*S+156D0*DS**3*M2**2*S-129D0*DS**2*M2**3*S+3D0*DS**
     &  4*S**2-186D0*DS**3*M2*S**2+438D0*DS**2*M2**2*S**2-141D0*DS*
     &  M2**3*S**2+69D0*DS**3*S**3-483D0*DS**2*M2*S**3+441D0*DS*M2**
     &  2*S**3-50D0*M2**3*S**3+174D0*DS**2*S**4-459D0*DS*M2*S**4+
     &  150D0*M2**2*S**4+159D0*DS*S**5-150D0*M2*S**5+50D0*S**6)+S**
     &  (-5)*(DS+S)**(-2)*(M2-S)**(-4)*(300D0*DS**6*M2**4+900D0*DS**
     &  5*M2**5+2700D0*DS**4*M2**6-11100D0*DS**3*M2**7-1050D0*DS**6*
     &  M2**3*S-3450D0*DS**5*M2**4*S-13050D0*DS**4*M2**5*S+82230D0*
     &  DS**3*M2**6*S-26880D0*DS**2*M2**7*S+87D0*S**10+1300D0*DS**6*
     &  M2**2*S**2+4950D0*DS**5*M2**3*S**2+25500D0*DS**4*M2**4*S**2-
     &  249970D0*DS**3*M2**5*S**2+177960D0*DS**2*M2**6*S**2-14160D0*
     &  DS*M2**7*S**2-625D0*DS**6*M2*S**3-3175D0*DS**5*M2**2*S**3-
     &  25425D0*DS**4*M2**3*S**3+403585D0*DS**3*M2**4*S**3-496820D0*
     &  DS**2*M2**5*S**3+92280D0*DS*M2**6*S**3-180D0*M2**7*S**3+
     &  60D0*DS**6*S**4+805D0*DS**5*M2*S**4+13240D0*DS**4*M2**2*S**
     &  4-369405D0*DS**3*M2**3*S**4+751340D0*DS**2*M2**4*S**4-
     &  252280D0*DS*M2**5*S**4+450D0*M2**6*S**4-75D0*DS**5*S**5-
     &  3290D0*DS**4*M2*S**5+185142D0*DS**3*M2**2*S**5-653577D0*DS**
     &  2*M2**3*S**5+372000D0*DS*M2**4*S**5-330D0*M2**5*S**5+280D0*
     &  DS**4*S**6-42489D0*DS**3*M2*S**6+314355D0*DS**2*M2**2*S**6-
     &  314654D0*DS*M2**3*S**6+45D0*M2**4*S**6+1977D0*DS**3*S**7-
     &  68979D0*DS**2*M2*S**7+147114D0*DS*M2**2*S**7-42D0*M2**3*S**
     &  7+2601D0*DS**2*S**8-31426D0*DS*M2*S**8+186D0*M2**2*S**8+
     &  1126D0*DS*S**9-216D0*M2*S**9)-(60D0*S**(-6)*(DS+S)**(-2)*
     &  (5D0*DS**6*M2**2+15D0*DS**5*M2**3+45D0*DS**4*M2**4-185D0*
     &  DS**3*M2**5-10D0*DS**6*M2*S-35D0*DS**5*M2**2*S-150D0*DS**4*
     &  M2**3*S+1093D0*DS**3*M2**4*S-448D0*DS**2*M2**5*S+5D0*DS**6*
     &  S**2+25D0*DS**5*M2*S**2+185D0*DS**4*M2**2*S**2-2465D0*DS**3*
     &  M2**3*S**2+2294D0*DS**2*M2**4*S**2-236D0*DS*M2**5*S**2-5D0*
     &  DS**5*S**3-100D0*DS**4*M2*S**3+2680D0*DS**3*M2**2*S**3-
     &  4690D0*DS**2*M2**3*S**3+1184D0*DS*M2**4*S**3-3D0*M2**5*S**3+
     &  20D0*DS**4*S**4-1400D0*DS**3*M2*S**4+4760D0*DS**2*M2**2*S**
     &  4-2350D0*DS*M2**3*S**4+3D0*M2**4*S**4+265D0*DS**3*S**5-
     &  2365D0*DS**2*M2*S**5+2300D0*DS*M2**2*S**5+425D0*DS**2*S**6-
     &  1100D0*DS*M2*S**6+190D0*DS*S**7)*XLOGM2)/(M2-S)))/3600D0
        C4(2,2,2,2) = (((720D0*LOGM12)/(M2-S)+3600D0*FC0(4)*(M2-S)**
     &  4-300D0*FB0(5)*(DS+S)**(-3)*(3D0*DS**7+7D0*DS**6*M2+13D0*
     &  DS**5*M2**2+25D0*DS**4*M2**3+5D0*DS**6*S+2D0*DS**5*M2*S-
     &  23D0*DS**4*M2**2*S+148D0*DS**3*M2**3*S+3D0*DS**5*S**2+13D0*
     &  DS**4*M2*S**2-330D0*DS**3*M2**2*S**2+258D0*DS**2*M2**3*S**2-
     &  3D0*DS**4*S**3+260D0*DS**3*M2*S**3-682D0*DS**2*M2**2*S**3+
     &  188D0*DS*M2**3*S**3-72D0*DS**3*S**4+604D0*DS**2*M2*S**4-
     &  538D0*DS*M2**2*S**4+50D0*M2**3*S**4-180D0*DS**2*S**5+512D0*
     &  DS*M2*S**5-150D0*M2**2*S**5-162D0*DS*S**6+150D0*M2*S**6-
     &  50D0*S**7)+S**(-5)*(DS+S)**(-3)*(M2-S)**(-4)*(900D0*DS**7*
     &  M2**4+2100D0*DS**6*M2**5+3900D0*DS**5*M2**6+7500D0*DS**4*
     &  M2**7-3150D0*DS**7*M2**3*S-6750D0*DS**6*M2**4*S-15150D0*DS**
     &  5*M2**5*S-37050D0*DS**4*M2**6*S+36180D0*DS**3*M2**7*S+
     &  3344D0*DS*S**10-5184D0*M2*S**10+1788D0*S**11+3900D0*DS**7*
     &  M2**2*S**2+7000D0*DS**6*M2**3*S**2+22450D0*DS**5*M2**4*S**2+
     &  75700D0*DS**4*M2**5*S**2-211950D0*DS**3*M2**6*S**2+39240D0*
     &  DS**2*M2**7*S**2-1875D0*DS**7*M2*S**3-1775D0*DS**6*M2**2*S**
     &  3-15675D0*DS**5*M2**3*S**3-82525D0*DS**4*M2**4*S**3+
     &  523680D0*DS**3*M2**5*S**3-248700D0*DS**2*M2**6*S**3+15240D0*
     &  DS*M2**7*S**3+180D0*DS**7*S**4-830D0*DS**6*M2*S**4+5205D0*
     &  DS**5*M2**2*S**4+51600D0*DS**4*M2**3*S**4-701595D0*DS**3*
     &  M2**4*S**4+660840D0*DS**2*M2**5*S**4-95700D0*DS*M2**6*S**4-
     &  720D0*M2**7*S**4+75D0*DS**6*S**5-1105D0*DS**5*M2*S**5-
     &  19130D0*DS**4*M2**2*S**5+539822D0*DS**3*M2**3*S**5-946510D0*
     &  DS**2*M2**4*S**5+255340D0*DS*M2**5*S**5+1800D0*M2**6*S**5+
     &  105D0*DS**5*S**6+4080D0*DS**4*M2*S**6-229486D0*DS**3*M2**2*
     &  S**6+770556D0*DS**2*M2**3*S**6-372510D0*DS*M2**4*S**6-
     &  1320D0*M2**5*S**6-355D0*DS**4*S**7+45116D0*DS**3*M2*S**7-
     &  336978D0*DS**2*M2**2*S**7+309506D0*DS*M2**3*S**7+180D0*M2**
     &  4*S**7-1857D0*DS**3*S**8+61288D0*DS**2*M2*S**8-132438D0*DS*
     &  M2**2*S**8-1608D0*M2**3*S**8+264D0*DS**2*S**9+17218D0*DS*M2*
     &  S**9+5064D0*M2**2*S**9)-60D0*S**(-6)*(DS+S)**(-3)*(15D0*DS**
     &  7*M2+35D0*DS**6*M2**2+65D0*DS**5*M2**3+125D0*DS**4*M2**4-
     &  15D0*DS**7*S-25D0*DS**6*M2*S-90D0*DS**5*M2**2*S-305D0*DS**4*
     &  M2**3*S+603D0*DS**3*M2**4*S-10D0*DS**6*S**2+30D0*DS**5*M2*
     &  S**2+270D0*DS**4*M2**2*S**2-2025D0*DS**3*M2**3*S**2+654D0*
     &  DS**2*M2**4*S**2-5D0*DS**5*S**3-110D0*DS**4*M2*S**3+2560D0*
     &  DS**3*M2**2*S**3-2510D0*DS**2*M2**3*S**3+254D0*DS*M2**4*S**
     &  3+20D0*DS**4*S**4-1430D0*DS**3*M2*S**4+3540D0*DS**2*M2**2*
     &  S**4-960D0*DS*M2**3*S**4-12D0*M2**4*S**4+280D0*DS**3*S**5-
     &  2160D0*DS**2*M2*S**5+1390D0*DS*M2**2*S**5+440D0*DS**2*S**6-
     &  910D0*DS*M2*S**6+190D0*DS*S**7)*XLOGM2))/3600D0

        dC111 = ( abs(11D0/3d0*dFB0(4)*(DS+S)*(DS-M2+S)**2)
     &           +abs(dFC0(3)*(DS-M2+S)**3) ) / abs(C3(1,1,1))
        dC222 = ( abs(dFC0(3)*(M2-S)**3) +abs(
     &  dFB0(4)*(DS+S)**(-2)*(2D0*DS**5+5D0*DS**4*M2+11D0*DS**3*
     &  M2**2+DS**4*S-7D0*DS**3*M2*S+51D0*DS**2*M2**2*S+2D0*DS**3*
     &  S**2-78D0*DS**2*M2*S**2+60D0*DS*M2**2*S**2+31D0*DS**2*S**3-
     &  110D0*DS*M2*S**3+22D0*M2**2*S**3+50D0*DS*S**4-44D0*M2*S**4+
     &  22D0*S**5)/6D0) ) / abs(C3(2,2,2))

        elseif (s.eq.0d0) then

        C0 = FC0(0)-1D0/M2+LOGM12/M2
        C1(1) = -(2D0*FB0(2)*DS)-(LOGM12*M2**(-2)*(DS+M2))/2D0-
     &  FC0(1)*(DS-M2)-(M2**(-2)*(DS+3D0*M2))/12D0
        C1(2) = FB0(2)*DS-FC0(1)*M2-(LOGM12)/(2D0*M2)+(M2**(-2)*
     &  (2D0*DS+9D0*M2))/12D0
        C2(0,0) = (LOGMU2)/4D0+(FB0(1)*(DS-M2))/4D0+((DS+3D0*M2))/
     &  (8D0*M2)
        C2(1,1) = 3D0*FB0(3)*DS*(DS-M2)+FC0(2)*(DS-M2)**2+(M2**(-3)*
     &  (5D0*DS**2+8D0*DS*M2+14D0*M2**2))/36D0+(LOGM12*M2**(-3)*
     &  (2D0*DS**2-DS*M2+2D0*M2**2))/6D0
        C2(1,2) = (LOGM12*M2**(-2)*(2D0*DS+M2))/6D0+FC0(2)*M2*(DS-
     &  M2)-(M2**(-3)*(DS-M2)*(3D0*DS+2D0*M2))/72D0-(FB0(3)*DS*(DS-
     &  5D0*M2))/2D0
        C2(2,2) = (LOGM12)/(3D0*M2)+FC0(2)*M2**2-(FB0(3)*DS*(DS+3D0*
     &  M2))/2D0-(M2**(-3)*(3D0*DS**2+15D0*DS*M2+44D0*M2**2))/72D0
        C3(1,1,1) = -((11D0*FB0(4)*DS*(DS-M2)**2)/3D0)-FC0(3)*(DS-
     &  M2)**3-(M2**(-4)*(87D0*DS**3+11D0*DS**2*M2+57D0*DS*M2**2+
     &  285D0*M2**3))/720D0-(LOGM12*M2**(-4)*(3D0*DS**3-5D0*DS**2*
     &  M2+3D0*DS*M2**2+3D0*M2**3))/12D0
        C3(1,1,2) = (FB0(4)*DS*(DS-10D0*M2)*(DS-M2))/3D0-FC0(3)*M2*
     &  (DS-M2)**2-(LOGM12*M2**(-3)*(3D0*DS**2-2D0*DS*M2+M2**2))/
     &  12D0+(M2**(-4)*(12D0*DS**3-67D0*DS**2*M2-70D0*DS*M2**2-55D0*
     &  M2**3))/720D0
        C3(1,2,2) = -((LOGM12*M2**(-2)*(3D0*DS+M2))/12D0)-FC0(3)*
     &  M2**2*(DS-M2)+(FB0(4)*DS*(DS**2+4D0*DS*M2-17D0*M2**2))/6D0+
     &  (M2**(-4)*(6D0*DS**3+34D0*DS**2*M2+3D0*DS*M2**2+5D0*M2**3))/
     &  720D0
        C3(2,2,2) = -((LOGM12)/(4D0*M2))-FC0(3)*M2**3+(FB0(4)*DS*
     &  (2D0*DS**2+5D0*DS*M2+11D0*M2**2))/6D0+(M2**(-4)*(12D0*DS**3+
     &  50D0*DS**2*M2+156D0*DS*M2**2+375D0*M2**3))/720D0
        C3(0,0,1) = -((LOGMU2)/12D0)-(FB0(2)*(DS-M2)**2)/12D0-(M2**
     &  (-2)*(DS**2+DS*M2+11D0*M2**2))/72D0
        C3(0,0,2) = -((LOGMU2)/12D0)-(FB0(2)*(DS-M2)*(DS+2D0*M2))/
     &  12D0-(M2**(-2)*(DS**2+4D0*DS*M2+11D0*M2**2))/72D0
        C4(0,0,0,0) = -((FB0(2)*(DS-M2)**3)/96D0)-(LOGMU2*(DS-4D0*
     &  M2))/96D0-(M2**(-2)*(DS**3+7D0*DS*M2**2-44D0*M2**3))/576D0
        C4(0,0,1,1) = (LOGMU2)/24D0+(FB0(3)*(DS-M2)**3)/24D0+(M2**(-
     &  3)*(DS**3-DS**2*M2+3D0*DS*M2**2+25D0*M2**3))/288D0
        C4(0,0,1,2) = (LOGMU2)/48D0+(FB0(3)*(DS-M2)**2*(DS+3D0*M2))/
     &  48D0+(M2**(-3)*(DS**3+3D0*DS**2*M2+3D0*DS*M2**2+25D0*M2**
     &  3))/576D0
        C4(0,0,2,2) = (LOGMU2)/24D0+(FB0(3)*(DS-M2)*(DS**2+2D0*DS*
     &  M2+3D0*M2**2))/24D0+(M2**(-3)*(DS**3+3D0*DS**2*M2+9D0*DS*
     &  M2**2+25D0*M2**3))/288D0
        C4(1,1,1,1) = (25D0*FB0(5)*DS*(DS-M2)**3)/6D0+FC0(4)*(DS-
     &  M2)**4+(LOGM12*M2**(-5)*(12D0*DS**4-33D0*DS**3*M2+32D0*DS**
     &  2*M2**2-8D0*DS*M2**3+12D0*M2**4))/60D0+(M2**(-5)*(356D0*DS**
     &  4-399D0*DS**3*M2+136D0*DS**2*M2**2+426D0*DS*M2**3+1356D0*
     &  M2**4))/3600D0
        C4(1,1,1,2) = FC0(4)*M2*(DS-M2)**3-(FB0(5)*DS*(DS-M2)**2*
     &  (3D0*DS-47D0*M2))/12D0+(LOGM12*M2**(-4)*(12D0*DS**3-21D0*
     &  DS**2*M2+11D0*DS*M2**2+3D0*M2**3))/60D0-(M2**(-5)*(30D0*DS**
     &  4-341D0*DS**3*M2+43D0*DS**2*M2**2-33D0*DS*M2**3-249D0*M2**
     &  4))/3600D0
        C4(1,1,2,2) = FC0(4)*M2**2*(DS-M2)**2+(LOGM12*M2**(-3)*
     &  (12D0*DS**2-9D0*DS*M2+2D0*M2**2))/60D0-(FB0(5)*DS*(DS-M2)*
     &  (DS**2+6D0*DS*M2-43D0*M2**2))/12D0-(M2**(-5)*(10D0*DS**4+
     &  65D0*DS**3*M2-246D0*DS**2*M2**2-193D0*DS*M2**3-86D0*M2**4))/
     &  3600D0
        C4(1,2,2,2) = (LOGM12*M2**(-2)*(4D0*DS+M2))/20D0+FC0(4)*M2**
     &  3*(DS-M2)-(FB0(5)*DS*(DS**3+3D0*DS**2*M2+9D0*DS*M2**2-37D0*
     &  M2**3))/12D0-(M2**(-5)*(10D0*DS**4+45D0*DS**3*M2+160D0*DS**
     &  2*M2**2+34D0*DS*M2**3+51D0*M2**4))/3600D0
        C4(2,2,2,2) = (LOGM12)/(5D0*M2)+FC0(4)*M2**4-(FB0(5)*DS*
     &  (3D0*DS**3+7D0*DS**2*M2+13D0*DS*M2**2+25D0*M2**3))/12D0-
     &  (M2**(-5)*(30D0*DS**4+115D0*DS**3*M2+310D0*DS**2*M2**2+
     &  770D0*DS*M2**3+1644D0*M2**4))/3600D0

        dC111 = ( abs((11D0*dFB0(4)*DS*(DS-M2)**2)/3D0)
     &               +abs(dFC0(3)*(DS-M2)**3) )/abs(C3(1,1,1))
        dC222 = ( abs(dFC0(3)*M2**3)+abs(dFB0(4)*DS*
     &           (2D0*DS**2+5D0*DS*M2+11D0*M2**2))/6D0 )/abs(C3(2,2,2))

        elseif (ds.eq.-s1) then

        C0 = FC0(0)-1D0/M2+LOGM12/M2
        C1(1) = -(FB0(2)*DS)+FC0(1)*M2-(LOGM12)/(2D0*M2)-(M2**(-2)*
     &  (2D0*DS+3D0*M2))/12D0
        C1(2) = 2D0*FB0(2)*DS-FC0(1)*(DS+M2)+(LOGM12*M2**(-2)*(DS-
     &  M2))/2D0+(M2**(-2)*(13D0*DS+9D0*M2))/12D0
        C2(0,0) = (LOGMU2)/4D0+(FB0(1)*(DS+M2))/4D0-((DS-3D0*M2))/
     &  (8D0*M2)
        C2(1,1) = (LOGM12)/(3D0*M2)+FC0(2)*M2**2+(FB0(3)*DS*(DS-3D0*
     &  M2))/2D0-(M2**(-3)*(3D0*DS**2-15D0*DS*M2-28D0*M2**2))/72D0
        C2(1,2) = -(FC0(2)*M2*(DS+M2))-(LOGM12*M2**(-2)*(2D0*DS-
     &  M2))/6D0+(FB0(3)*DS*(DS+5D0*M2))/2D0-(M2**(-3)*(3D0*DS**2+
     &  73D0*DS*M2-2D0*M2**2))/72D0
        C2(2,2) = -(3D0*FB0(3)*DS*(DS+M2))+FC0(2)*(DS+M2)**2+(M2**(-
     &  3)*(41D0*DS**2+28D0*DS*M2-22D0*M2**2))/36D0+(LOGM12*M2**(-
     &  3)*(2D0*DS**2+DS*M2+2D0*M2**2))/6D0
        C3(1,1,1) = -((LOGM12)/(4D0*M2))+FC0(3)*M2**3-(FB0(4)*DS*
     &  (2D0*DS**2-5D0*DS*M2+11D0*M2**2))/6D0-(M2**(-4)*(12D0*DS**3-
     &  50D0*DS**2*M2+156D0*DS*M2**2+285D0*M2**3))/720D0
        C3(1,1,2) = -(FC0(3)*M2**2*(DS+M2))+(LOGM12*M2**(-2)*(3D0*
     &  DS-M2))/12D0-(FB0(4)*DS*(DS**2-4D0*DS*M2-17D0*M2**2))/6D0-
     &  (M2**(-4)*(6D0*DS**3-34D0*DS**2*M2-657D0*DS*M2**2+55D0*M2**
     &  3))/720D0
        C3(1,2,2) = FC0(3)*M2*(DS+M2)**2-(FB0(4)*DS*(DS+M2)*(DS+
     &  10D0*M2))/3D0-(LOGM12*M2**(-3)*(3D0*DS**2+2D0*DS*M2+M2**2))/
     &  12D0-(M2**(-4)*(12D0*DS**3+727D0*DS**2*M2+530D0*DS*M2**2-
     &  5D0*M2**3))/720D0
        C3(2,2,2) = (11D0*FB0(4)*DS*(DS+M2)**2)/3D0-FC0(3)*(DS+M2)**
     &  3+(M2**(-4)*(747D0*DS**3+1249D0*DS**2*M2+597D0*DS*M2**2+
     &  375D0*M2**3))/720D0+(LOGM12*M2**(-4)*(3D0*DS**3+5D0*DS**2*
     &  M2+3D0*DS*M2**2-3D0*M2**3))/12D0
        C3(0,0,1) = -((LOGMU2)/12D0)-(FB0(2)*(DS+M2)*(DS-2D0*M2))/
     &  12D0-(M2**(-2)*(DS**2-4D0*DS*M2+11D0*M2**2))/72D0
        C3(0,0,2) = -((LOGMU2)/12D0)-(FB0(2)*(DS+M2)**2)/12D0-(M2**
     &  (-2)*(DS**2-DS*M2+11D0*M2**2))/72D0
        C4(0,0,0,0) = (FB0(2)*(DS+M2)**3)/96D0+(LOGMU2*(DS+4D0*M2))/
     &  96D0+(M2**(-2)*(DS**3+7D0*DS*M2**2+44D0*M2**3))/576D0
        C4(0,0,1,1) = (LOGMU2)/24D0+(FB0(3)*(DS+M2)*(DS**2-2D0*DS*
     &  M2+3D0*M2**2))/24D0-(M2**(-3)*(DS**3-3D0*DS**2*M2+9D0*DS*
     &  M2**2-25D0*M2**3))/288D0
        C4(0,0,1,2) = (LOGMU2)/48D0+(FB0(3)*(DS+M2)**2*(DS-3D0*M2))/
     &  48D0-(M2**(-3)*(DS**3-3D0*DS**2*M2+3D0*DS*M2**2-25D0*M2**
     &  3))/576D0
        C4(0,0,2,2) = (LOGMU2)/24D0+(FB0(3)*(DS+M2)**3)/24D0-(M2**(-
     &  3)*(DS**3+DS**2*M2+3D0*DS*M2**2-25D0*M2**3))/288D0
        C4(1,1,1,1) = (LOGM12)/(5D0*M2)+FC0(4)*M2**4+(FB0(5)*DS*
     &  (3D0*DS**3-7D0*DS**2*M2+13D0*DS*M2**2-25D0*M2**3))/12D0-
     &  (M2**(-5)*(30D0*DS**4-115D0*DS**3*M2+310D0*DS**2*M2**2-
     &  770D0*DS*M2**3-1356D0*M2**4))/3600D0
        C4(1,1,1,2) = -(FC0(4)*M2**3*(DS+M2))-(LOGM12*M2**(-2)*(4D0*
     &  DS-M2))/20D0+(FB0(5)*DS*(DS**3-3D0*DS**2*M2+9D0*DS*M2**2+
     &  37D0*M2**3))/12D0-(M2**(-5)*(10D0*DS**4-45D0*DS**3*M2+160D0*
     &  DS**2*M2**2+2966D0*DS*M2**3-249D0*M2**4))/3600D0
        C4(1,1,2,2) = FC0(4)*M2**2*(DS+M2)**2+(LOGM12*M2**(-3)*
     &  (12D0*DS**2+9D0*DS*M2+2D0*M2**2))/60D0+(FB0(5)*DS*(DS+M2)*
     &  (DS**2-6D0*DS*M2-43D0*M2**2))/12D0-(M2**(-5)*(10D0*DS**4-
     &  65D0*DS**3*M2-3246D0*DS**2*M2**2-2507D0*DS*M2**3-86D0*M2**
     &  4))/3600D0
        C4(1,2,2,2) = -(FC0(4)*M2*(DS+M2)**3)+(FB0(5)*DS*(DS+M2)**2*
     &  (3D0*DS+47D0*M2))/12D0-(LOGM12*M2**(-4)*(12D0*DS**3+21D0*
     &  DS**2*M2+11D0*DS*M2**2-3D0*M2**3))/60D0-(M2**(-5)*(30D0*DS**
     &  4+3341D0*DS**3*M2+5743D0*DS**2*M2**2+2733D0*DS*M2**3+51D0*
     &  M2**4))/3600D0
        C4(2,2,2,2) = -((25D0*FB0(5)*DS*(DS+M2)**3)/6D0)+FC0(4)*(DS+
     &  M2)**4+(LOGM12*M2**(-5)*(12D0*DS**4+33D0*DS**3*M2+32D0*DS**
     &  2*M2**2+8D0*DS*M2**3+12D0*M2**4))/60D0+(M2**(-5)*(3356D0*
     &  DS**4+9099D0*DS**3*M2+8536D0*DS**2*M2**2+2574D0*DS*M2**3-
     &  1644D0*M2**4))/3600D0

        dC111 = ( abs(dFC0(3)*M2**3)+abs(dFB0(4)*DS*
     &            (2D0*DS**2-5D0*DS*M2+11D0*M2**2))/6D0 )/abs(C3(1,1,1))
        dC222 = ( abs(11D0*dFB0(4)*DS*(DS+M2)**2)/3D0
     &           +abs(dFC0(3)*(DS+M2)**3) )/abs(C3(2,2,2))
        endif

c redundant coefficients
          C2(2,1) = C2(1,2)
          C3(1,2,1) = C3(1,1,2)
          C3(2,1,1) = C3(1,1,2)
          C3(2,1,2) = C3(1,2,2)
          C3(2,2,1) = C3(1,2,2)
          C4(0,0,2,1) = C4(0,0,1,2)
          C4(1,1,2,1) = C4(1,1,1,2)
          C4(1,2,1,1) = C4(1,1,1,2)
          C4(2,1,1,1) = C4(1,1,1,2)
          C4(1,2,1,2) = C4(1,1,2,2)
          C4(2,1,1,2) = C4(1,1,2,2)
          C4(2,1,2,1) = C4(1,1,2,2)
          C4(2,2,1,1) = C4(1,1,2,2)
          C4(1,2,2,1) = C4(1,1,2,2)
          C4(2,1,2,2) = C4(1,2,2,2)
          C4(2,2,1,2) = C4(1,2,2,2)
          C4(2,2,2,1) = C4(1,2,2,2)

        crelacc = max(dC111,dC222)

        end

***********************************************************************
        subroutine Cswap_dd(C0,C1,C2,C3,C4,i,j,rank)
***********************************************************************
*       swap indices i <-> j in 3-point tensor coefficients 
*-----------------------------------------------------------------------
*       26.11.04 Stefan Dittmaier
***********************************************************************
        implicit real*8 (a-z)                                         
        complex*16 C0,C1(2),C2(0:2,0:2),C3(0:2,0:2,2)
        complex*16 C4(0:2,0:2,0:2,0:2)
        complex*16 aux
        integer i,j,rank

        if (rank.gt.4) then
          write(*,*) 'Cswap_dd implemented only for rank < 5!',rank
          stop
        endif

        C0 = C0

        if (rank.eq.0) return  
        aux   = C1(i)
        C1(i) = C1(j)
        C1(j) = aux
        if (rank.eq.1) return  
        aux     = C2(i,i)
        C2(i,i) = C2(j,j)
        C2(j,j) = aux
        if (rank.eq.2) return  
        aux       = C3(0,0,i)
        C3(0,0,i) = C3(0,0,j)
        C3(0,0,j) = aux
        aux       = C3(i,i,i)
        C3(i,i,i) = C3(j,j,j)
        C3(j,j,j) = aux
        aux       = C3(i,i,j)
        C3(i,i,j) = C3(j,j,i)
        C3(j,j,i) = aux
        aux       = C3(i,j,j)
        C3(i,j,j) = C3(j,i,i)
        C3(j,i,i) = aux
        aux       = C3(j,i,j)
        C3(j,i,j) = C3(i,j,i)
        C3(i,j,i) = aux
        if (rank.eq.3) return  
        aux         = C4(0,0,i,i)
        C4(0,0,i,i) = C4(0,0,j,j)
        C4(0,0,j,j) = aux
        aux         = C4(i,i,i,i)
        C4(i,i,i,i) = C4(j,j,j,j)
        C4(j,j,j,j) = aux
        aux         = C4(i,i,i,j)
        C4(i,i,i,j) = C4(j,j,j,i)
        C4(j,j,j,i) = aux
        aux         = C4(i,i,j,i)
        C4(i,i,j,i) = C4(j,j,i,j)
        C4(j,j,i,j) = aux
        aux         = C4(i,j,i,i)
        C4(i,j,i,i) = C4(j,i,j,j)
        C4(j,i,j,j) = aux
        aux         = C4(j,i,i,i)
        C4(j,i,i,i) = C4(i,j,j,j)
        C4(i,j,j,j) = aux

        end

