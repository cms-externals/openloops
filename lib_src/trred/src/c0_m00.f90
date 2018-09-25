!******************************************************************************!
!                                                                              !
!    c0_m00.f90                                                                !
!    is part of trred & OpenLoops2                                             !
!    Copyright (C) 2017-2018 Federico Buccioni, Jean-Nicolas Lang,             !
!                            Stefano Pozzorini, Hantian Zhang and Max Zoller   !
!                                                                              !
!    trred has been developed by Jean-Nicolas Lang and Hantian Zhang           !
!    trred is licenced under the GNU GPL version 3,                            !
!    see COPYING for details.                                                  !
!                                                                              !
!******************************************************************************!

!Last Modified: February 07, 2018

! Implements the n-th derivative of C0(-p^2,-p^2(1+\delta),m^2,0,0)
! (C0_n_m00_small_z). For large values of z = m^2/p^2 the formula is
! numerically unstable and a different representation in 1/z is chosen
! (C0_n_exp_m00). 

module c0_m00_DP
  use triangle_aux_DP, only: target_precision,dp,cone,cnul,Lphi,HarmNum,Sv1,LphiLog
  implicit none
  ! for values z=m2/p2 > THRESHOLD expansion formulas are used
  real(dp), parameter :: C_m00_exp_thr = 0.2_dp

  contains

  function C0_n_m00(p2,m2,muUV2,muIR2,n) result(C0)
    complex(dp), intent(in) :: p2,m2,muUV2,muIR2
    integer,       intent(in) :: n
    complex(dp)             :: z,C0

    z = m2/p2
    if (abs(z) .gt. C_m00_exp_thr) then
      C0 = C0_n_m00_large_z(z,p2,muIR2,n)
    else
      C0 = C0_n_m00_small_z(z,p2,muIR2,n)
    end if
    
  end function C0_n_m00

  function C0_n_m00_small_z(z,p2,muIR2,n) result(Cn)
    complex(dp), intent(in) :: z,p2,muIR2
    integer,       intent(in) :: n
    integer       :: k
    complex(dp) :: v,Cn,sum

    if (abs(z) .gt. 1) then
      write (*,*) 'ERROR: C0_n_m00_small_z not convergent for |z|>1'
      stop
    end if

    v = 1/(1+z)

    sum = cnul
    if (n .gt. 0) then
      do k = 0, n
        sum = sum + ((cone + z)**(n - k)-2)/cmplx(1+ k,kind=dp)
      end do
      sum = sum + 1/cmplx(1+ n,kind=dp)
    end if

    Cn = (-v)**(n+1)*(sum + log(z*v)*Sv1(z,n+1) - log(v) - log(muIR2/p2) &
                     )/ cmplx(n+1,kind=dp)/p2

  end function C0_n_m00_small_z

  function C0_n_m00_large_z(z,p2,muIR2,n) result(Cn)
    complex(dp), intent(in) :: z,p2,muIR2
    integer,       intent(in) :: n
    complex(dp) :: w,v,Cn

    if (n .lt. 0) then
      write (*,*) 'ERROR: called C0_n_m00_large_z with n<0'
      stop
    end if

    w = 1/z
    v = w/(cone+w)

    Cn = -(-v)**(n+1)*(v*Lphi(v,n+2) + 2*HarmNum(n)+cone/cmplx(n+1,kind=dp) &
                       + log(w) - 2*log(1+w)+log(muIR2/p2)                 &
                      )/cmplx(n+1,kind=dp)/p2

  end function C0_n_m00_large_z
    
end module c0_m00_DP

module c0_m00_QP
  use triangle_aux_QP, only: target_precision,qp,cone,cnul,Lphi,HarmNum,Sv1,LphiLog
  implicit none
  ! for values z=m2/p2 > THRESHOLD expansion formulas are used
  real(qp), parameter :: C_m00_exp_thr = 0.2_qp

  contains

  function C0_n_m00(p2,m2,muUV2,muIR2,n) result(C0)
    complex(qp), intent(in) :: p2,m2,muUV2,muIR2
    integer,       intent(in) :: n
    complex(qp)             :: z,C0

    z = m2/p2
    if (abs(z) .gt. C_m00_exp_thr) then
      C0 = C0_n_m00_large_z(z,p2,muIR2,n)
    else
      C0 = C0_n_m00_small_z(z,p2,muIR2,n)
    end if
    
  end function C0_n_m00

  function C0_n_m00_small_z(z,p2,muIR2,n) result(Cn)
    complex(qp), intent(in) :: z,p2,muIR2
    integer,       intent(in) :: n
    integer       :: k
    complex(qp) :: v,Cn,sum

    if (abs(z) .gt. 1) then
      write (*,*) 'ERROR: C0_n_m00_small_z not convergent for |z|>1'
      stop
    end if

    v = 1/(1+z)

    sum = cnul
    if (n .gt. 0) then
      do k = 0, n
        sum = sum + ((cone + z)**(n - k)-2)/cmplx(1+ k,kind=qp)
      end do
      sum = sum + 1/cmplx(1+ n,kind=qp)
    end if

    Cn = (-v)**(n+1)*(sum + log(z*v)*Sv1(z,n+1) - log(v) - log(muIR2/p2) &
                     )/ cmplx(n+1,kind=qp)/p2

  end function C0_n_m00_small_z

  function C0_n_m00_large_z(z,p2,muIR2,n) result(Cn)
    complex(qp), intent(in) :: z,p2,muIR2
    integer,       intent(in) :: n
    complex(qp) :: w,v,Cn

    if (n .lt. 0) then
      write (*,*) 'ERROR: called C0_n_m00_large_z with n<0'
      stop
    end if

    w = 1/z
    v = w/(cone+w)

    Cn = -(-v)**(n+1)*(v*Lphi(v,n+2) + 2*HarmNum(n)+cone/cmplx(n+1,kind=qp) &
                       + log(w) - 2*log(1+w)+log(muIR2/p2)                 &
                      )/cmplx(n+1,kind=qp)/p2

  end function C0_n_m00_large_z
    
end module c0_m00_QP
