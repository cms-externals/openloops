












!!
!!  File DD_aux.F is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!





        module dd_aux_qp
        use dd_global_qp, only: rk, acmplx
        use dd_dilog_qp, only: cspen_dd => Li2
        contains

**********************************************************************
        subroutine DDsetparam(xdeltauv,xmuv2,xdelta2ir,xdelta1ir,
     &                        xmir2,xmx2)
**********************************************************************
*       parameter initalization of DD library
*---------------------------------------------------------------------
*       9.4.2008 Stefan Dittmaier
**********************************************************************
        use dd_global_qp
        implicit real(rk) (a-z)

c local variables
        real(rk) xmx2(100)
        integer i

c UV parameters
        deltauv  = xdeltauv 
        muv2     = xmuv2   

c IR parameters
        delta2ir = xdelta2ir
        delta1ir = xdelta1ir
        mir2     = xmir2    

c small mass parameters
        do i=1,100
          mx2(i) = xmx2(i) 
        enddo

c determine CPU precision
        dprec = 1._rk
        dres_old = 2._rk
        do i=1,1000
          dprec = dprec/2._rk
          dres = exp(log(1._rk+dprec))
          if (abs(dres).ge.abs(dres_old)) exit
          dres_old = dres
        enddo
        dprec_dd = dprec*8._rk

        end

**********************************************************************
        subroutine DDgetparam(xdeltauv,xmuv2,xdelta2ir,xdelta1ir,
     &                        xmir2,xmx2)
**********************************************************************
*       read parameters of DD library
*---------------------------------------------------------------------
*       9.4.2008 Stefan Dittmaier
**********************************************************************
        use dd_global_qp
        implicit real(rk) (a-z)

c local variables
        real(rk) xmx2(100)
        integer i

c UV parameters
        xdeltauv  = deltauv 
        xmuv2     = muv2   

c IR parameters
        xdelta2ir = delta2ir
        xdelta1ir = delta1ir
        xmir2     = mir2    

c small mass parameters
        do i=1,100
          xmx2(i) = mx2(i) 
        enddo

        end

**********************************************************************
        subroutine DDsetmode(xcacc,xdacc,xmode34,xmode5,xmode6,
     &                       xoutlevel,xoutchannel)
**********************************************************************
*       set mode of DD library
*---------------------------------------------------------------------
*       9.4.2008 Stefan Dittmaier
**********************************************************************
        use dd_global_qp
        implicit real(rk) (a-z)

c local variables
        integer xmode34,xmode5,xmode6,xoutlevel,xoutchannel

        cacc       = xcacc
        dacc       = xdacc
        mode34     = xmode34
        mode5      = xmode5
        mode6      = xmode6
        outlevel   = xoutlevel
        outchannel = xoutchannel

        end

**********************************************************************
        subroutine DDgetmode(xcacc,xdacc,xmode34,xmode5,xmode6,
     &                       xoutlevel,xoutchannel)
**********************************************************************
*       read mode of DD library
*---------------------------------------------------------------------
*       9.4.2008 Stefan Dittmaier
**********************************************************************
        use dd_global_qp
        implicit real(rk) (a-z)

c local variables
        integer xmode34,xmode5,xmode6,xoutlevel,xoutchannel

        xcacc       = cacc
        xdacc       = dacc
        xmode34     = mode34
        xmode5      = mode5
        xmode6      = mode6
        xoutlevel   = outlevel
        xoutchannel = outchannel

        end

**********************************************************************
        subroutine DDsetaccthr(accthr)
**********************************************************************
*       set threshold for accuracy flags in DD library
*       acc > accthr: accuracy flag accflag raised 
*                     (use accthr = 10^-4 - 10^-8 or so)
*---------------------------------------------------------------------
*       28.3.2015 Stefan Dittmaier
**********************************************************************
        use dd_global_qp
        implicit real(rk) (a-z)
        integer n

        do n=1,nmax
          aimacc(n) = accthr
        enddo

        end

**********************************************************************
        subroutine DDseterrthr(errthr)
**********************************************************************
*       set threshold for error flags in DD library
*       err > errthr: error flag errflag raised
*                     (use errthr = 0.1  - 1 or so)
*---------------------------------------------------------------------
*       28.3.2015 Stefan Dittmaier
**********************************************************************
        use dd_global_qp
        implicit real(rk) (a-z)
        integer n

        do n=1,nmax
          erracc(n) = errthr
        enddo

        end

**********************************************************************
        subroutine DDgetacc(accrel,accabs,accrel2,accabs2,
     &                      n,rank,xaccflag,xerrflag,id)
**********************************************************************
*       get accuracy estimate for last master call of DD library
*
*       rank       = highest rank calculated by DD for n-point function
*       accrel(n)  = relative accuracy of rank <= rmax in n-point function
*       accabs(n)  = absolute accuracy for coefficients T_ij... (i,j,...=/=0)
*       accrel2(n) = as accrel(n), but relevant for whole tensor
*       accabs2(n) = as accabs(n), but relevant for whole tensor
*
*       accflag = 0,1: accuracy better than aimacc(n) (yes/no=0/1)
*       errflag = 0,1: internal problems in DD       (no/yes=0/1)
*---------------------------------------------------------------------
*       28.3.2015 Stefan Dittmaier
**********************************************************************
        use dd_global_qp
        implicit real(rk) (a-z)

        real(rk) accrel(0:rmax), accabs(0:rmax)
        real(rk) accrel2(0:rmax),accabs2(0:rmax)
        integer r,n,rank,xaccflag,xerrflag,id

        n        = nmaster
        rank     = r2master
        xaccflag = accflag
        xerrflag = errflag
        do r=0,rank
          accrel(r)  = resaccrel(id,r)
          accabs(r)  = resaccabs(id,r)
          accrel2(r) = resaccrel2(id,r)
          accabs2(r) = resaccabs2(id,r)
        enddo

        end

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!         FUNCTION cspen_dd(Z)
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! C       SPENCE-FUNKTION KOMPLEX, FREI NACH HOLLIK                     C
! C---------------------------------------------------------------------C
! C       20.07.83    LAST CHANGED 10.05.89        ANSGAR DENNER        C
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!         complex(rk) cspen_dd,W,SUM,Z,U
!         real(rk) RZ,AZ,A1
! c        real(rk) B(9)/
!         real(rk) :: B(9) = (/
!      1   0.1666666666666666666666666667_rk,
!      2  -0.0333333333333333333333333333_rk,
!      3   0.0238095238095238095238095238_rk,
!      4  -0.0333333333333333333333333333_rk,
!      5   0.0757575757575757575757575758_rk,
!      6  -0.2531135531135531135531135531_rk,
!      7   1.1666666666666666666666666667_rk,
!      8  -7.09215686274509804_rk         ,
!      9  54.97117794486215539_rk         /)
! c     9  54.97117794486215539_rk         /
! C     BEACHTE:                 B(N)=B2N
! C     B(1)=1./6.
! C     B(2)=-1./30.
! C     B(3)=1./42.
! C     B(4)=-1./30.
! C     B(5)=5./66.
! C     B(6)=-691./2730.
! C     B(7)=7./6.
! C     B(8)=-3617./510.
! C     B(9)=43867./798.
! C     B(10)=-174611./330.
! C     B(11)=854513./138.
! C     PI=3.1415926535897932384
! C     PI*PI/6.=1.6449..., PI*PI/3=3.28986...
! C
!       Z =Z*acmplx(1._rk)
!       RZ=real(Z)
!       AZ=abs(Z)
!       A1=abs(1._rk-Z)
! C     IF((SNGL(RZ) .EQ. 0.0) .AND. (SNGL(aimag(Z)) .EQ. 0.0)) THEN
! C ---> CHANGED  10.5.89
!       IF(AZ .LT. 1.e-20_rk) THEN
!         cspen_dd=-log(1._rk-Z)
!         RETURN
!       END IF
! c      IF((SNGL(RZ) .EQ. 1.0) .AND. (SNGL(aimag(Z)) .EQ. 0.0)) THEN
! c ---> changed 5.7.94
!        IF( (ABS(RZ-1._rk).LT.1.e-18_rk) .AND. (ABS(aimag(Z)).LT.1.e-18_rk) ) THEN
!         cspen_dd=1.64493406684822643_rk
!         RETURN
!       END IF
!       IF(RZ.GT.5.e-1_rk) GOTO 20
!       IF(AZ.GT.1._rk) GOTO 10
!       W=-log(1._rk-Z)
!       SUM=W-0.25_rk*W*W
!       U=W
!       IF(abs(U).LT.1.e-10_rk) GOTO 2
!       DO 1 K=1,9
!       U=U*W*W/dble(2*K*(2*K+1))
!       IF(abs(U*B(K)/SUM).LT.1.e-20_rk) GOTO 2
!       SUM=SUM+U*B(K)
!  1    CONTINUE
!  2    cspen_dd=SUM
!       RETURN
! 10    W=-log(1._rk-1._rk/Z)
!       SUM=W-0.25_rk*W*W
!       U=W
!       IF(abs(U).LT. 1.e-10_rk) GOTO 12
!
!       DO 11 K=1,9
!       U=U*W*W/dble(2*K*(2*K+1))
!       IF(abs(B(K)*U/SUM).LT. 1.e-20_rk) GOTO 12
!       SUM=SUM+U*B(K)
! 11    CONTINUE
! 12    cspen_dd=-SUM-1.64493406684822643_rk-.5_rk*log(-Z)**2
!       RETURN
! 20    IF(A1.GT.1._rk) GOTO 30
!       W=-log(Z)
!       SUM=W-0.25_rk*W*W
!       U=W
!       IF(abs(U).LT.1.e-10_rk) GOTO 22
!       DO 21 K=1,9
!       U=U*W*W/dble(2*K*(2*K+1))
!       IF(abs(U*B(K)/SUM).LT.1.e-20_rk) GOTO 22
!       SUM=SUM+U*B(K)
! 21    CONTINUE
! 22    cspen_dd=-SUM+1.64493406684822643_rk-log(Z)*log(1._rk-Z)
!       RETURN
! 30    W=log(1._rk-1._rk/Z)
!       SUM=W-0.25_rk*W*W
!       U=W
!       IF(abs(U).LT. 1.e-10_rk) GOTO 32
!       DO 31 K=1,9
!       U=U*W*W/dble(2*K*(2*K+1))
!       IF(abs(U*B(K)/SUM).LT. 1.e-20_rk) GOTO 32
!       SUM=SUM+U*B(K)
! 31    CONTINUE
! 32    cspen_dd=SUM+3.28986813369645287_rk
!      *               +.5_rk*log(Z-1._rk)**2-log(Z)*log(1._rk-Z)
!         END

***********************************************************************
        FUNCTION eta_dd(C1,C2)                                            
***********************************************************************
*       COMPLEX ETA-FUNKTION                                           
*---------------------------------------------------------------------*
*       8.06.90    ANSGAR DENNER                                       
***********************************************************************
        use dd_global_qp
        IMPLICIT   real(rk)(A-Z)

        complex(rk) eta_dd,C1,C2
        real(rk)     PI,IM1,IM2,IM12

        PI     = 4._rk*atan(1._rk)
        IM1    = aimag(C1)
        IM2    = aimag(C2)
        IM12   = aimag(C1*C2)

        if (((IM1.eq.0._rk).and.(real(C1).lt.0._rk)).or.
     &      ((IM2.eq.0._rk).and.(real(C2).lt.0._rk)).or.
     &      ((IM12.eq.0._rk).and.(real(C1*C2).lt.0._rk))) then

          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'eta function on cut !!!'
            write(outchannel,*) 'C1    = ',C1
            write(outchannel,*) 'C2    = ',C2
            write(outchannel,*) 'C1*C2 = ',C1*C2
            call DD_debugoutput()
          endif
          stopflag = min(-4,stopflag)
        endif
                                                                       
        IF(IM1.LT.0._rk.AND.IM2.LT.0._rk.AND.IM12.GT.0._rk) THEN
            eta_dd = acmplx(0._rk,2._rk*PI)
        ELSE IF (IM1.GT.0._rk.AND.IM2.GT.0._rk.AND.IM12.LT.0._rk) THEN
            eta_dd = acmplx(0._rk,-2._rk*PI)
        ELSE                                                           
            eta_dd = acmplx(0._rk)
        END IF                                                         
        END                                                            

***********************************************************************
        function sqe_dd(a,b,c)                                            
***********************************************************************
*       Solution of quadratic equation a*x^2 + b*x + c = 0
*----------------------------------------------------------------------
*       9.4.08  Stefan Dittmaier                                       
***********************************************************************
        use dd_global_qp
        implicit real(rk) (a-z)

        complex(rk) a,b,c,sqe_dd,x1,x2

        if ((a.eq.(0._rk,0._rk)).and.(b.eq.(0._rk,0._rk))) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'sqe_dd: a=b=0 not allowed'
            if (cout.eq.coutmax) call DDlastmessage()
            cout = cout+1
          endif
          sqe_dd = 0._rk
          stopflag = min(-7,stopflag)
        elseif ((a.eq.(0._rk,0._rk)).and.(b.ne.(0._rk,0._rk))) then
          sqe_dd = -c/b
        else
          x1=(-b+sqrt(b**2-4._rk*a*c))/2._rk/a
          x2=(-b-sqrt(b**2-4._rk*a*c))/2._rk/a
          if (abs(x1).gt.abs(x2)) then
             sqe_dd=x1
          else
             sqe_dd=x2
          endif
        endif

        end                                                            

***********************************************************************
        function xdet_dd(A,n)
************************************************************************
*       determinant of complex nxn-matrix A
*-----------------------------------------------------------------------
*       10.6.04 Stefan Dittmaier
************************************************************************
        implicit real(rk) (a-z)
        integer n
        complex(rk) A(n,n),Q(n,n),R(n,n),detQ,xdet_dd
        integer i

        call xQRdecomp_dd(A,Q,R,detQ,n)

        xdet_dd = detQ*r(n,n)
        do i=1,n-1
          xdet_dd = xdet_dd*r(i,i)
        enddo

        end

***********************************************************************
        subroutine xinverse_dd(A,iA,detA,n)
************************************************************************
*       inverse of complex nxn-matrix A
*-----------------------------------------------------------------------
*       8.1.04 Stefan Dittmaier
************************************************************************
        implicit real(rk) (a-z)
        integer n
        complex(rk) A(n,n),Q(n,n),R(n,n),iA(n,n),iR(n,n),detQ,w,detA
        integer i,j,k

        call xQRdecomp_dd(A,Q,R,detQ,n)

        detA = detQ*R(n,n)
        do i=1,n-1
          detA = detA*R(i,i)
        enddo

c invert R
        do i=n,1,-1
          do j=1,n
            if (j.lt.i) then
              iR(i,j) = 0._rk
            else
              w = 0._rk
              do k=i+1,n
                w = w + R(i,k)*iR(k,j)
              enddo
              if (i.eq.j) then
                iR(i,j) = (1._rk-w)/R(i,i)
              else
                iR(i,j) = -w/R(i,i)
              endif
            endif
          enddo
        enddo

c inverse of A
        do i=1,n
          do j=1,n
            iA(i,j) = 0._rk
            do k=1,n
              iA(i,j) = iA(i,j) + iR(i,k)*conjg(Q(j,k))
            enddo
          enddo
        enddo

        end

***********************************************************************
        subroutine xQRdecomp_dd(A,Q,R,detQ,n)
************************************************************************
*       QR-decomposition of complex nxn-matrix A using Householder matrices
*       Q = orthogonal matrix, R = upper triangle matrix
*-----------------------------------------------------------------------
*       8.1.04 Stefan Dittmaier
************************************************************************
        implicit real(rk) (a-z)
        integer n
        complex(rk) A(n,n),Q(n,n),R(n,n),w(n),wr(n),qw(n),detQ,aux,kap
        complex(rk) rmax
        integer i,j,k,imax

        detQ = 1._rk

c*** start with R=A and Q=unit matrix 
        do i=1,n
          do j=1,n
            r(i,j) = a(i,j)
            if (i.eq.j) then
              q(i,j) = 1._rk
            else
              q(i,j) = 0._rk
            endif
          enddo
        enddo

c*** n-1 steps of decomposition
        do 200 i=1,n-1

c exchange rows such that |r(i,i)| is maximal
          imax = i
          rmax = r(i,i)
          do j=i+1,n
            if (abs(r(j,i)).gt.abs(rmax)) then
              imax = j
              rmax = r(j,i)
            endif
          enddo
          if (imax.ne.i) then
            do j=1,n
              aux       = r(i,j)
              r(i,j)    = r(imax,j)
              r(imax,j) = aux
              aux       = q(j,i)
              q(j,i)    = q(j,imax)
              q(j,imax) = aux
            enddo
            detQ = -detQ
          endif
c calculate R and Q if r(i,i) is non-zero
          if (r(i,i).eq.(0._rk,0._rk)) goto 200
          sig2 = 0._rk
          do j=i,n
            sig2 = sig2 + abs(r(j,i))**2
          enddo
          sig = sqrt(sig2)
          kap   = -r(i,i)/abs(r(i,i))*sig
          wnorm = sqrt(2._rk*sig*(sig+abs(r(i,i))))
          do j=i,n
            w(j) = r(j,i)/wnorm
          enddo
          w(i) = w(i) - kap/wnorm

          do j=i,n
            wr(j) = 0._rk
            do k=i,n
              wr(j) = wr(j) + conjg(w(k))*r(k,j)
            enddo
          enddo
          do j=i,n
            if (j.eq.i) then
              r(j,i) = r(j,i) - 2._rk*w(j)*wr(i)
            else
              r(j,i) = 0._rk
            endif
            do k=i+1,n
              r(j,k) = r(j,k) - 2._rk*w(j)*wr(k)
            enddo
          enddo

c calculate matrix Q
          do j=1,n
            qw(j) = 0._rk
            do k=i,n
              qw(j) = qw(j) + q(j,k)*w(k)
            enddo
          enddo
          do j=1,n
            do k=i,n
              q(j,k) = q(j,k) - 2._rk*conjg(w(k))*qw(j)
            enddo
          enddo
          detQ = -detQ

200     continue

        end

***********************************************************************
        function det_dd(A,n)
************************************************************************
*       determinant of real nxn-matrix A
*-----------------------------------------------------------------------
*       8.1.04 Stefan Dittmaier
************************************************************************
        implicit real(rk) (a-z)
        integer n
        real(rk) A(n,n),Q(n,n),R(n,n)
        integer i

        call QRdecomp_dd(A,Q,R,detQ,n)

        det_dd = detQ*R(n,n)
        do i=1,n-1
          det_dd = det_dd*R(i,i)
        enddo

        end

***********************************************************************
        subroutine inverse_dd(A,iA,detA,n)
************************************************************************
*       inverse of real nxn-matrix A
*-----------------------------------------------------------------------
*       8.1.04 Stefan Dittmaier
************************************************************************
        implicit real(rk) (a-z)
        integer n
        real(rk) A(n,n),Q(n,n),R(n,n),iA(n,n),iR(n,n)
        integer i,j,k

        call QRdecomp_dd(A,Q,R,detQ,n)

        detA = detQ*R(n,n)
        do i=1,n-1
          detA = detA*R(i,i)
        enddo

c invert R
        do i=n,1,-1
          do j=1,n
            if (j.lt.i) then
              iR(i,j) = 0._rk
            else
              w = 0._rk
              do k=i+1,n
                w = w + R(i,k)*iR(k,j)
              enddo
              if (i.eq.j) then
                iR(i,j) = (1._rk-w)/R(i,i)
              else
                iR(i,j) = -w/R(i,i)
              endif
            endif
          enddo
        enddo

c inverse of A
        do i=1,n
          do j=1,n
            iA(i,j) = 0._rk
            do k=1,n
              iA(i,j) = iA(i,j) + iR(i,k)*Q(j,k)
            enddo
          enddo
        enddo

        end

***********************************************************************
        subroutine QRdecomp_dd(A,Q,R,detQ,n)
************************************************************************
*       QR-decomposition of real nxn-matrix A using Householder matrices
*       Q = orthogonal matrix, R = upper triangle matrix
*-----------------------------------------------------------------------
*       8.1.04 Stefan Dittmaier
************************************************************************
        implicit real(rk) (a-z)
        integer n
        real(rk) A(n,n),Q(n,n),R(n,n)
        real(rk) w(n),wr(n),qw(n)
        integer i,j,k,imax

        detQ = 1._rk

c*** start with R=A and Q=unit matrix 
        do i=1,n
          do j=1,n
            r(i,j) = a(i,j)
            if (i.eq.j) then
              q(i,j) = 1._rk
            else
              q(i,j) = 0._rk
            endif
          enddo
        enddo

c*** n-1 steps of decomposition
        do 200 i=1,n-1

c exchange rows such that |r(imax,i)| is maximal
          imax = i
          rmax = r(i,i)
          do j=i+1,n
            if (abs(r(j,i)).gt.abs(rmax)) then
              imax = j
              rmax = r(j,i)
            endif
          enddo
          if (imax.ne.i) then
            do j=1,n
              aux       = r(i,j)
              r(i,j)    = r(imax,j)
              r(imax,j) = aux
              aux       = q(j,i)
              q(j,i)    = q(j,imax)
              q(j,imax) = aux
            enddo
            detQ = -detQ
          endif
c calculate R and Q if r(i,i) is non-zero
          if (r(i,i).eq.0._rk) goto 200
          sig2 = 0._rk
          do j=i,n
            sig2 = sig2 + r(j,i)**2
          enddo
          sig = sqrt(sig2)
          kap   = -sign(1._rk,r(i,i))*sig
          wnorm = sqrt(2._rk*sig*(sig+abs(r(i,i))))
          do j=i,n
            w(j) = r(j,i)/wnorm
          enddo
          w(i) = w(i) - kap/wnorm

          do j=i,n
            wr(j) = 0._rk
            do k=i,n
              wr(j) = wr(j) + w(k)*r(k,j)
            enddo
          enddo
          do j=i,n
            if (j.eq.i) then
              r(j,i) = r(j,i) - 2._rk*w(j)*wr(i)
            else
              r(j,i) = 0._rk
            endif
            do k=i+1,n
              r(j,k) = r(j,k) - 2._rk*w(j)*wr(k)
            enddo
          enddo

c calculate matrix Q
          do j=1,n
            qw(j) = 0._rk
            do k=i,n
              qw(j) = qw(j) + q(j,k)*w(k)
            enddo
          enddo
          do j=1,n
            do k=i,n
              q(j,k) = q(j,k) - 2._rk*w(k)*qw(j)
            enddo
          enddo
          detQ = -detQ

200     continue

        end

***********************************************************************
        subroutine DD_debugoutput()
************************************************************************
*       write out debug info
*-----------------------------------------------------------------------
*       8.3.13 Stefan Dittmaier
************************************************************************
        use dd_global_qp
        implicit real(rk) (a-z)
        integer i

        if (cout_on.and.(cout.le.coutmax)) then
          write(outchannel,*) 'DD debug info:'
          write(outchannel,*) 'Last call by ',s_DDin
          write(outchannel,*) 'Real input parameters:   ',nr_DDin
          do i=1,nr_DDin
            write(outchannel,'(g26.16)') r_DDin(i)  
          enddo
          write(outchannel,*) 'Complex input parameters:',nc_DDin
          do i=1,nc_DDin
            write(outchannel,'(g26.16,3x,g26.16)') c_DDin(i)  
          enddo
          write(outchannel,*) 'Integer input parameters:',ni_DDin
          do i=1,ni_DDin
            write(outchannel,*) i_DDin(i)  
          enddo
          if (cout.eq.coutmax) call DDlastmessage()
          cout = cout+1
        endif

        end

**********************************************************************
        subroutine TN_dd_dummy(N,r2)
**********************************************************************
*       N-point coefficients of rank r <= r2
*
*       DUMMY CALL for initialization
*---------------------------------------------------------------------
*       10.9.2015 Stefan Dittmaier
**********************************************************************
        use dd_global_qp
        implicit real(rk) (a-z)

c local variables
        integer r,r2,i,k,N,m,id,nid(0:nmax-1)

          id = 0

          if (id.eq.0) then
            do i=0,2**N-1
              r2_aux(i)     = -1
              r2_new_aux(i) = -1
              do r=0,r2
                resaccrel(i,r)  = 0._rk
                resaccabs(i,r)  = 0._rk
                resaccrel2(i,r) = 0._rk
                resaccabs2(i,r) = 0._rk
              enddo
            enddo
            nmaster   = N
            r2master  = r2
            accflag   = 0
            errflag   = 0
            stopflag  = 0
          endif

        if (N.gt.nmax) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'TN_dd_dummy: N > nmax'
            if (cout.eq.coutmax) call DDlastmessage()
            cout = cout+1
          endif
          stopflag = min(-10,stopflag)
        else

c set identifiers for lower-point integrals
          m = 0
          do k=0,nmax-1
            if (mod(id,2**(k+1))/2**k.eq.0) then
              nid(m) = id + 2**k
              m=m+1
            endif
            if (m.eq.N) goto 205
          enddo
205       continue

        endif

        end

**********************************************************************
        subroutine DDsetcoutmax(ccoutmax)
**********************************************************************
*       set maximal number of DD messages
*---------------------------------------------------------------------
*       23.10.2015 Stefan Dittmaier
**********************************************************************
        use dd_global_qp
        implicit real(rk) (a-z)

        integer ccoutmax

        coutmax = ccoutmax

        end

**********************************************************************
        subroutine DDresetcout()
**********************************************************************
*       reset counter for DD messages
*---------------------------------------------------------------------
*       23.10.2015 Stefan Dittmaier
**********************************************************************
        use dd_global_qp
        implicit real(rk) (a-z)

        cout = 0

        end

**********************************************************************
        subroutine DDsetcout_on(ccout_on)
**********************************************************************
*       reset counter for DD messages
*---------------------------------------------------------------------
*       23.10.2015 Stefan Dittmaier
**********************************************************************
        use dd_global_qp
        implicit real(rk) (a-z)

        logical ccout_on

        cout_on = ccout_on

        end

**********************************************************************
        subroutine DDlastmessage()
**********************************************************************
*       last DD message
*---------------------------------------------------------------------
*       23.10.2015 Stefan Dittmaier
**********************************************************************
        use dd_global_qp
        implicit real(rk) (a-z)

        write(outchannel,*) 
        write(outchannel,*) 'cout = ',cout
        write(outchannel,*) '==================================' 
        write(outchannel,*) '== further DD output suppressed ==' 
        write(outchannel,*) '==================================' 

        end

        end module dd_aux_qp
