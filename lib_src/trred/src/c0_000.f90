!******************************************************************************!
!                                                                              !
!    c0_000.f90                                                                !
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

! Implements the n-th derivative of C0(-p^2,-p^2(1+\delta),0,0,0)

module c0_000_DP
  use triangle_aux_DP, only: dp,cone,cnul
  implicit none

  contains

  function C0_n_000(p2,m2,muUV2,muIR2,n) result(Cn)
    complex(dp), intent(in) :: p2,m2,muUV2,muIR2
    integer,       intent(in) :: n
    integer       :: k
    complex(dp) :: Cn,sum

    sum = cnul
    if (n .ge. 0) then
      do k = 0, n-1
        sum = sum + ( (-1)**k )/(cone+k)*(-1)**(n-k+1)/cmplx(n-k,kind=dp)
      end do
      sum = sum + 2*(-1)**n/(cone+n)*log(p2/muIR2)
    end if

    Cn = - sum/(2*p2)

  end function C0_n_000
    
end module c0_000_DP

module c0_000_QP
  use triangle_aux_QP, only: qp,cone,cnul
  implicit none

  contains

  function C0_n_000(p2,m2,muUV2,muIR2,n) result(Cn)
    complex(qp), intent(in) :: p2,m2,muUV2,muIR2
    integer,       intent(in) :: n
    integer       :: k
    complex(qp) :: Cn,sum

    sum = cnul
    if (n .ge. 0) then
      do k = 0, n-1
        sum = sum + ( (-1)**k )/(cone+k)*(-1)**(n-k+1)/cmplx(n-k,kind=qp)
      end do
      sum = sum + 2*(-1)**n/(cone+n)*log(p2/muIR2)
    end if

    Cn = - sum/(2*p2)

  end function C0_n_000
    
end module c0_000_QP
