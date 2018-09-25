!******************************************************************************!
! Copyright (C) 2014-2018 OpenLoops Collaboration. For authors see authors.txt !
!                                                                              !
! This file is part of OpenLoops.                                              !
!                                                                              !
! OpenLoops is free software: you can redistribute it and/or modify            !
! it under the terms of the GNU General Public License as published by         !
! the Free Software Foundation, either version 3 of the License, or            !
! (at your option) any later version.                                          !
!                                                                              !
! OpenLoops is distributed in the hope that it will be useful,                 !
! but WITHOUT ANY WARRANTY; without even the implied warranty of               !
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                !
! GNU General Public License for more details.                                 !
!                                                                              !
! You should have received a copy of the GNU General Public License            !
! along with OpenLoops.  If not, see <http://www.gnu.org/licenses/>.           !
!******************************************************************************!


module ol_h_last_step_/**/REALKIND
  implicit none
  contains

!******************************************************************************
subroutine Hcheck_last_AQ_V(ntry, switch, G_A, J_Q, Gtensor, n, t)
!------------------------------------------------------------------------------
! bare AQ -> V gluon-like interaction
!******************************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_last_vert3
  use ol_last_step_/**/REALKIND, only: check_last_AQ_V
  implicit none

  integer,           intent(in)    :: switch
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(:,:)
  type(wfun),  intent(in)          :: J_Q(:)
  type(hol),   intent(in)          :: G_A
  complex(REALKIND), intent(out)   :: Gtensor(:)
  complex(REALKIND) :: G_add(size(Gtensor))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_last_vert3(ntry, J_Q, G_A, n, t)

  Gtensor = 0._/**/REALKIND
  G_add   = 0._/**/REALKIND

  do h = 1, n(3)  ! helicity summation
    call check_last_AQ_V(switch, G_A%j(:,:,:,h), J_Q(t(1,h))%j, G_add)
    Gtensor = Gtensor + G_add
  end do

end subroutine Hcheck_last_AQ_V


!******************************************************************************
subroutine Hcheck_last_QA_V(ntry, switch, G_Q, J_A, Gtensor, n, t)
!------------------------------------------------------------------------------
! bare QA -> V gluon-like interaction
!******************************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_last_vert3
  use ol_last_step_/**/REALKIND, only: check_last_QA_V
  implicit none

  integer,           intent(in)    :: switch
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(:,:)
  type(wfun),  intent(in)          :: J_A(:)
  type(hol),   intent(in)          :: G_Q
  complex(REALKIND), intent(out)   :: Gtensor(:)
  complex(REALKIND) :: G_add(size(Gtensor))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_last_vert3(ntry, J_A, G_Q, n, t)

  Gtensor = 0._/**/REALKIND
  G_add   = 0._/**/REALKIND

  do h = 1, n(3)  ! helicity summation
    call check_last_QA_V(switch, G_Q%j(:,:,:,h), J_A(t(1,h))%j, G_add)
    Gtensor = Gtensor + G_add
  end do

end subroutine Hcheck_last_QA_V


!******************************************************************************
subroutine Hcheck_last_AQ_Z(ntry, switch, G_A, J_Q, Gtensor, g_RL, n, t)
!------------------------------------------------------------------------------
! bare AQ -> Z Z-like interaction
!******************************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_last_vert3
  use ol_last_step_/**/REALKIND, only: check_last_AQ_Z
  implicit none

  integer,           intent(in)    :: switch
  complex(REALKIND), intent(in) :: g_RL(2)
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(:,:)
  type(wfun),  intent(in)          :: J_Q(:)
  type(hol),   intent(in)          :: G_A
  complex(REALKIND), intent(out)   :: Gtensor(:)
  complex(REALKIND) :: G_add(size(Gtensor))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_last_vert3(ntry, J_Q, G_A, n, t)

  Gtensor = 0._/**/REALKIND
  G_add   = 0._/**/REALKIND

  do h = 1, n(3)  ! helicity summation
    call check_last_AQ_Z(switch, G_A%j(:,:,:,h), J_Q(t(1,h))%j, G_add, g_RL)
    Gtensor = Gtensor + G_add
  end do

end subroutine Hcheck_last_AQ_Z


!******************************************************************************
subroutine Hcheck_last_QA_Z(ntry, switch, G_Q, J_A, Gtensor, g_RL, n, t)
!------------------------------------------------------------------------------
! bare QA -> Z Z-like interaction
!******************************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_last_vert3
  use ol_last_step_/**/REALKIND, only: check_last_QA_Z
  implicit none

  integer,           intent(in)    :: switch
  complex(REALKIND), intent(in) :: g_RL(2)
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(:,:)
  type(wfun),  intent(in)          :: J_A(:)
  type(hol),   intent(in)          :: G_Q
  complex(REALKIND), intent(out)   :: Gtensor(:)
  complex(REALKIND) :: G_add(size(Gtensor))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_last_vert3(ntry, J_A, G_Q, n, t)

  Gtensor = 0._/**/REALKIND
  G_add   = 0._/**/REALKIND

  do h = 1, n(3)  ! helicity summation
    call check_last_QA_Z(switch, G_Q%j(:,:,:,h), J_A(t(1,h))%j, G_add, g_RL)
    Gtensor = Gtensor + G_add
  end do

end subroutine Hcheck_last_QA_Z


!******************************************************************************
subroutine Hcheck_last_AQ_W(ntry, switch, G_A, J_Q, Gtensor, n, t)
!------------------------------------------------------------------------------
! bare AQ -> W W-like interaction
!******************************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_last_vert3
  use ol_last_step_/**/REALKIND, only: check_last_AQ_W
  implicit none

  integer,           intent(in)    :: switch
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(:,:)
  type(wfun),  intent(in)          :: J_Q(:)
  type(hol),   intent(in)          :: G_A
  complex(REALKIND), intent(out)   :: Gtensor(:)
  complex(REALKIND) :: G_add(size(Gtensor))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_last_vert3(ntry, J_Q, G_A, n, t)

  Gtensor = 0._/**/REALKIND
  G_add   = 0._/**/REALKIND

  do h = 1, n(3)  ! helicity summation
    call check_last_AQ_W(switch, G_A%j(:,:,:,h), J_Q(t(1,h))%j, G_add)
    Gtensor = Gtensor + G_add
  end do

end subroutine Hcheck_last_AQ_W


!******************************************************************************
subroutine Hcheck_last_QA_W(ntry, switch, G_Q, J_A, Gtensor, n, t)
!------------------------------------------------------------------------------
! bare QA -> W gluon-like interaction
!******************************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_last_vert3
  use ol_last_step_/**/REALKIND, only: check_last_QA_W
  implicit none

  integer,           intent(in)    :: switch
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(:,:)
  type(wfun),  intent(in)          :: J_A(:)
  type(hol),   intent(in)          :: G_Q
  complex(REALKIND), intent(out)   :: Gtensor(:)
  complex(REALKIND) :: G_add(size(Gtensor))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_last_vert3(ntry, J_A, G_Q, n, t)

  Gtensor = 0._/**/REALKIND
  G_add   = 0._/**/REALKIND

  do h = 1, n(3)  ! helicity summation
    call check_last_QA_W(switch, G_Q%j(:,:,:,h), J_A(t(1,h))%j, G_add)
    Gtensor = Gtensor + G_add
  end do

end subroutine Hcheck_last_QA_W

!******************************************************************************
subroutine Hcheck_last_A_Q(ntry, switch, G_A, K, M, Gtensor, n)
!------------------------------------------------------------------------------
! dressing anti-quark current with propagator
!******************************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_last_prop
  use ol_last_step_/**/REALKIND, only: check_last_A_Q
  implicit none

  integer,           intent(in)    :: switch
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n
  type(hol),   intent(in)          :: G_A
  complex(REALKIND), intent(in)    :: K(5), M
  complex(REALKIND), intent(out)   :: Gtensor(:)
  complex(REALKIND) :: G_add(size(Gtensor))
  integer :: h

  if (ntry == 1) call helbookkeeping_last_prop(ntry, G_A, n)

  Gtensor = 0._/**/REALKIND
  G_add   = 0._/**/REALKIND

  do h = 1, n  ! helicity summation
    call check_last_A_Q(switch, G_A%j(:,:,:,h), K, M, G_add)
    Gtensor = Gtensor + G_add
  end do

end subroutine Hcheck_last_A_Q


!******************************************************************************
subroutine Hcheck_last_Q_A(ntry, switch, G_Q, K, M, Gtensor, n)
!------------------------------------------------------------------------------
! dressing quark current with propagator
!******************************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_last_prop
  use ol_last_step_/**/REALKIND, only: check_last_Q_A
  implicit none

  integer,           intent(in)    :: switch
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n
  type(hol),   intent(in)          :: G_Q
  complex(REALKIND), intent(in)    :: K(5), M
  complex(REALKIND), intent(out)   :: Gtensor(:)
  complex(REALKIND) :: G_add(size(Gtensor))
  integer :: h

  if (ntry == 1) call helbookkeeping_last_prop(ntry, G_Q, n)

  Gtensor = 0._/**/REALKIND
  G_add   = 0._/**/REALKIND

  do h = 1, n  ! helicity summation
    call check_last_Q_A(switch, G_Q%j(:,:,:,h), K, M, G_add)
    Gtensor = Gtensor + G_add
  end do

end subroutine Hcheck_last_Q_A


!******************************************************************************
subroutine Hcheck_last_UV_W(ntry, switch, Gin_V, Ploop, J_V, Ptree,Gtensor,n,t)
!------------------------------------------------------------------------------
! bare VV -> V vertex
!******************************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_last_vert3
  use ol_last_step_/**/REALKIND, only: check_last_UV_W
  implicit none

  integer,           intent(in)    :: switch
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(:,:)
  complex(REALKIND), intent(in)    :: Ploop(4), Ptree(4)
  type(wfun),  intent(in)          :: J_V(:)
  type(hol),   intent(in)          :: Gin_V
  complex(REALKIND), intent(out)   :: Gtensor(:)
  complex(REALKIND) :: G_add(size(Gtensor))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_last_vert3(ntry, J_V, Gin_V, n, t)

  Gtensor = 0._/**/REALKIND
  G_add   = 0._/**/REALKIND

  do h = 1, n(3)  ! helicity summation
    call check_last_UV_W(switch, Gin_V%j(:,:,:,h),Ploop,J_V(t(1,h))%j,&
    Ptree, G_add)
    Gtensor = Gtensor + G_add
  end do

end subroutine Hcheck_last_UV_W


!******************************************************************************
subroutine Hcheck_last_GGG_G_23(ntry, switch, Gin, J1, J2, Gtensor, n, t)
!------------------------------------------------------------------------------
! bare 4-gluon vertex when the sigma particle enters the
! loop as a tree wavefunction
!******************************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_last_vert4
  use ol_last_step_/**/REALKIND, only: check_last_GGG_G_23
  implicit none

  integer,           intent(in)    :: switch
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(4), t(:,:)
  type(wfun),  intent(in)          :: J1(:), J2(:)
  type(hol),   intent(in)          :: Gin
  complex(REALKIND), intent(out)   :: Gtensor(:)
  complex(REALKIND) :: G_add(size(Gtensor))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_last_vert4(ntry, J1, J2, Gin, n, t)

  Gtensor = 0._/**/REALKIND
  G_add   = 0._/**/REALKIND

  do h = 1, n(4)  ! helicity summation
    call check_last_GGG_G_23(switch, Gin%j(:,:,:,h), J1(t(1,h))%j, &
    J2(t(2,h))%j, G_add)
    Gtensor = Gtensor + G_add
  end do

end subroutine Hcheck_last_GGG_G_23


!******************************************************************************
subroutine Hcheck_last_GGG_G_12(ntry, switch, Gin, J1, J2, Gtensor, n, t)
!------------------------------------------------------------------------------
! bare 4-gluon vertex when the sigma particle is in the loop
!******************************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_last_vert4
  use ol_last_step_/**/REALKIND, only: check_last_GGG_G_12
  implicit none

  integer,           intent(in)    :: switch
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(4), t(:,:)
  type(wfun),  intent(in)          :: J1(:), J2(:)
  type(hol),   intent(in)          :: Gin
  complex(REALKIND), intent(out)   :: Gtensor(:)
  complex(REALKIND) :: G_add(size(Gtensor))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_last_vert4(ntry, J1, J2, Gin, n, t)

  Gtensor = 0._/**/REALKIND
  G_add   = 0._/**/REALKIND

  do h = 1, n(4)  ! helicity summation
    call check_last_GGG_G_12(switch, Gin%j(:,:,:,h), J1(t(1,h))%j, &
    J2(t(2,h))%j, G_add)
    Gtensor = Gtensor + G_add
  end do

end subroutine Hcheck_last_GGG_G_12


!******************************************************************************
subroutine Hcheck_last_CV_D(ntry, switch, Gin, Ploop, J_V, Ptree, Gtensor,n,t)
!------------------------------------------------------------------------------
! bare ghost-gluon -> ghost vertex
!******************************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_last_vert3
  use ol_last_step_/**/REALKIND, only: check_last_CV_D
  implicit none

  integer,           intent(in)    :: switch
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(:,:)
  complex(REALKIND), intent(in)    :: Ploop(4), Ptree(4)
  type(wfun),  intent(in)          :: J_V(:)
  type(hol),   intent(in)          :: Gin
  complex(REALKIND), intent(out)   :: Gtensor(:)
  complex(REALKIND) :: G_add(size(Gtensor))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_last_vert3(ntry, J_V, Gin, n, t)

  Gtensor = 0._/**/REALKIND
  G_add   = 0._/**/REALKIND

  do h = 1, n(3)  ! helicity summation
    call check_last_CV_D(switch, Gin%j(:,:,:,h), Ploop, J_V(t(1,h))%j, &
    Ptree, G_add)
    Gtensor = Gtensor + G_add
  end do

end subroutine Hcheck_last_CV_D


!******************************************************************************
subroutine Hcheck_last_DV_C(ntry, switch, Gin, Ploop, J_V, Gtensor, n, t)
!------------------------------------------------------------------------------
! bare antighost-gluon -> antighost vertex
!******************************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_last_vert3
  use ol_last_step_/**/REALKIND, only: check_last_DV_C
  implicit none

  integer,           intent(in)    :: switch
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(:,:)
  complex(REALKIND), intent(in)    :: Ploop(4)
  type(wfun),  intent(in)          :: J_V(:)
  type(hol),   intent(in)          :: Gin
  complex(REALKIND), intent(out)   :: Gtensor(:)
  complex(REALKIND) :: G_add(size(Gtensor))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_last_vert3(ntry, J_V, Gin, n, t)

  Gtensor = 0._/**/REALKIND
  G_add   = 0._/**/REALKIND

  do h = 1, n(3)  ! helicity summation
    call check_last_DV_C(switch, Gin%j(:,:,:,h), Ploop, J_V(t(1,h))%j, G_add)
    Gtensor = Gtensor + G_add
  end do

end subroutine Hcheck_last_DV_C


end module ol_h_last_step_/**/REALKIND
