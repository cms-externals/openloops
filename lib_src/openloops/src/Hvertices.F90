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


module ol_hel_vertices_/**/REALKIND
  implicit none
  contains

!************************************************************************
subroutine vert_ZQ_A(g_RL, ntry, Z, Q, Q_out, n, t)
! ----------------------------------------------------------------------
! bare ZQ -> Q Z-like interaction
! ----------------------------------------------------------------------
! ntry          = 1 (2) for 1st (subsequent) PS points
! Q(1:n(1))     = incoming quark
! Z(1:n(2)      = incoming Z (light-cone representation)
! g_RL(1)       = right-handed coupling gR
! g_RL(2)       = left-handed coupling gL
! Q_out(1:n(3)) = outgoing quark
! Q_out(h)%j(i) = Z(t(1,h))%j(A)*[gamma_A*(gR*w_R+gL*w_L)](i,j)*Q(t(2,h))%j(j)
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun
  use ol_h_helicity_bookkeeping_/**/REALKIND, only: helbookkeeping_vert3
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(2,n(3))
  type(wfun),        intent(in)    :: Z(n(1)), Q(n(2))
  complex(REALKIND), intent(in)    :: g_RL(2)
  type(wfun),        intent(out)   :: Q_out(n(3))
  integer :: h

  do h = 1, n(3)
    select case (Q(t(2,h))%h)

    case (B"01")
      Q_out(h)%j(1) = g_RL(2) * ( - Z(t(1,h))%j(2)*Q(t(2,h))%j(3) + Z(t(1,h))%j(4)*Q(t(2,h))%j(4))
      Q_out(h)%j(2) = g_RL(2) * ( - Z(t(1,h))%j(1)*Q(t(2,h))%j(4) + Z(t(1,h))%j(3)*Q(t(2,h))%j(3))
      Q_out(h)%j(3:4) = 0
      Q_out(h)%h    = B"10"

    case (B"10")
      Q_out(h)%j(1:2) = 0
      Q_out(h)%j(3) = g_RL(1) * ( - Z(t(1,h))%j(1)*Q(t(2,h))%j(1) - Z(t(1,h))%j(4)*Q(t(2,h))%j(2))
      Q_out(h)%j(4) = g_RL(1) * ( - Z(t(1,h))%j(2)*Q(t(2,h))%j(2) - Z(t(1,h))%j(3)*Q(t(2,h))%j(1))
      Q_out(h)%h    = B"01"

    case (B"00")
      Q_out(h)%j = 0 ! needed to detect vanishing helicity configurations
      Q_out(h)%h = B"00"

    case default
      Q_out(h)%j(1) = g_RL(2) * ( - Z(t(1,h))%j(2)*Q(t(2,h))%j(3) + Z(t(1,h))%j(4)*Q(t(2,h))%j(4))
      Q_out(h)%j(2) = g_RL(2) * ( - Z(t(1,h))%j(1)*Q(t(2,h))%j(4) + Z(t(1,h))%j(3)*Q(t(2,h))%j(3))
      Q_out(h)%j(3) = g_RL(1) * ( - Z(t(1,h))%j(1)*Q(t(2,h))%j(1) - Z(t(1,h))%j(4)*Q(t(2,h))%j(2))
      Q_out(h)%j(4) = g_RL(1) * ( - Z(t(1,h))%j(2)*Q(t(2,h))%j(2) - Z(t(1,h))%j(3)*Q(t(2,h))%j(1))
      Q_out(h)%h    = B"11"

    end select
  end do

  if (ntry == 1) then
    ! n_part = number of external particles in the subtree
    Q_out(:)%n_part = Z(1)%n_part + Q(1)%n_part

    ! index of subtree: 2^(i-1) + 2^(j-1) where i,j are indices for Z and Q
    Q_out(:)%t = Z(1)%t + Q(1)%t

    ! global helicity label of the subtree
    Q_out(:)%hf = Z(t(1,:))%hf + Q(t(2,:))%hf

    call helbookkeeping_vert3(ntry, Z, Q, Q_out, n, t)
  end if

end subroutine vert_ZQ_A

! **********************************************************************
subroutine vert_AZ_Q(g_RL, ntry, A, Z, A_out, n, t)
! bare AZ -> A Z-like interaction
! ----------------------------------------------------------------------
! ntry          = 1 (2) for 1st (subsequent) PS points
! A(1:n(1))     = incoming anti-quark
! Z(1:n(2))     = incoming Z (light-cone representation)
! g_RL(1)       = right-handed coupling gR
! g_RL(2)       = left-handed coupling gL
! A_out(1:n(3)) = outgoing anti-quark
! A_out(h)%j(i) = A(t(1,h))%j(j) * [gamma_A*(gR*w_R+gL*w_L)](j,i) * Z(t(2,h))%j(A)
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun
  use ol_h_helicity_bookkeeping_/**/REALKIND, only: helbookkeeping_vert3
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(2,n(3))
  type(wfun),        intent(in)    :: A(n(1)), Z(n(2))
  complex(REALKIND), intent(in)    :: g_RL(2)
  type(wfun),        intent(out)   :: A_out(n(3))
  integer :: h

  do h = 1, n(3)
    select case (A(t(1,h))%h)

    case (B"01")
      A_out(h)%j(1) = g_RL(1) * ( - Z(t(2,h))%j(1)*A(t(1,h))%j(3) - Z(t(2,h))%j(3)*A(t(1,h))%j(4))
      A_out(h)%j(2) = g_RL(1) * ( - Z(t(2,h))%j(2)*A(t(1,h))%j(4) - Z(t(2,h))%j(4)*A(t(1,h))%j(3))
      A_out(h)%j(3:4) = 0
      A_out(h)%h    = B"10"

    case (B"10")
      A_out(h)%j(1:2) = 0
      A_out(h)%j(3) = g_RL(2) * ( - Z(t(2,h))%j(2)*A(t(1,h))%j(1) + Z(t(2,h))%j(3)*A(t(1,h))%j(2))
      A_out(h)%j(4) = g_RL(2) * ( - Z(t(2,h))%j(1)*A(t(1,h))%j(2) + Z(t(2,h))%j(4)*A(t(1,h))%j(1))
      A_out(h)%h    = B"01"

    case (B"00")
      A_out(h)%j = 0 ! needed to detect vanishing helicity configurations
      A_out(h)%h = B"00"

    case default
      A_out(h)%j(1) = g_RL(1) * ( - Z(t(2,h))%j(1)*A(t(1,h))%j(3) - Z(t(2,h))%j(3)*A(t(1,h))%j(4))
      A_out(h)%j(2) = g_RL(1) * ( - Z(t(2,h))%j(2)*A(t(1,h))%j(4) - Z(t(2,h))%j(4)*A(t(1,h))%j(3))
      A_out(h)%j(3) = g_RL(2) * ( - Z(t(2,h))%j(2)*A(t(1,h))%j(1) + Z(t(2,h))%j(3)*A(t(1,h))%j(2))
      A_out(h)%j(4) = g_RL(2) * ( - Z(t(2,h))%j(1)*A(t(1,h))%j(2) + Z(t(2,h))%j(4)*A(t(1,h))%j(1))
      A_out(h)%h    = B"11"

    end select
  end do

  if (ntry == 1) then
    ! n_part = number of external particles in the subtree
    A_out(:)%n_part = A(1)%n_part + Z(1)%n_part

    ! index of subtree: 2^(i-1) + 2^(j-1) where i,j are indices for A and V
    A_out(:)%t = A(1)%t + Z(1)%t

    ! global helicity label of the subtree
    A_out(:)%hf = A(t(1,:))%hf + Z(t(2,:))%hf

    call helbookkeeping_vert3(ntry, A, Z, A_out, n, t)
  end if

end subroutine vert_AZ_Q


! **********************************************************************
subroutine vert_QA_Z(g_RL, ntry, Q, A, Z_out, n, t)
! bare QA -> Z Z-like interaction
! ----------------------------------------------------------------------
! ntry          = 1 (2) for 1st (subsequent) PS points
! Q(1:n(1))     = incoming quark
! A(1:n(2))     = incoming anti-quark
! g_RL(1)       = right-handed coupling gR
! g_RL(2)       = left-handed coupling gL
! Z_out(1:n(3)) = outgoing Z (light-cone representation)
! Z_out(h)%j(A) = A(t(2,h))%j(i) * [gamma^A*(gR*w_R+gL*w_L)](i,j) * Q(t(1,h))%j(j)
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun
  use ol_h_helicity_bookkeeping_/**/REALKIND, only: helbookkeeping_vert3
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(2,n(3))
  type(wfun),        intent(in)    :: Q(n(1)), A(n(2))
  complex(REALKIND), intent(in)    :: g_RL(2)
  type(wfun),        intent(out)   :: Z_out(n(3))
  complex(REALKIND) :: A_aux(4)
  integer :: h

  do h = 1, n(3)
    select case (ishft(Q(t(1,h))%h,2) + A(t(2,h))%h)

    case (B"1111")
      A_aux(1:2)    = g_RL(2)*A(t(2,h))%j(1:2)
      A_aux(3:4)    = g_RL(1)*A(t(2,h))%j(3:4)
      Z_out(h)%j(1) = - A_aux(1)*Q(t(1,h))%j(3) - A_aux(4)*Q(t(1,h))%j(2)
      Z_out(h)%j(2) = - A_aux(2)*Q(t(1,h))%j(4) - A_aux(3)*Q(t(1,h))%j(1)
      Z_out(h)%j(3) = - A_aux(1)*Q(t(1,h))%j(4) + A_aux(3)*Q(t(1,h))%j(2)
      Z_out(h)%j(4) = - A_aux(2)*Q(t(1,h))%j(3) + A_aux(4)*Q(t(1,h))%j(1)
      Z_out(h)%j    =  Z_out(h)%j + Z_out(h)%j

    case (B"0110", B"0111", B"1110")
      A_aux(1:2)    = g_RL(2)*A(t(2,h))%j(1:2)
      Z_out(h)%j(1) = - A_aux(1)*Q(t(1,h))%j(3)
      Z_out(h)%j(2) = - A_aux(2)*Q(t(1,h))%j(4)
      Z_out(h)%j(3) = - A_aux(1)*Q(t(1,h))%j(4)
      Z_out(h)%j(4) = - A_aux(2)*Q(t(1,h))%j(3)
      Z_out(h)%j    =  Z_out(h)%j + Z_out(h)%j

    case (B"1001", B"1101", B"1011")
      A_aux(3:4)    = g_RL(1)*A(t(2,h))%j(3:4)
      Z_out(h)%j(1) = - A_aux(4)*Q(t(1,h))%j(2)
      Z_out(h)%j(2) = - A_aux(3)*Q(t(1,h))%j(1)
      Z_out(h)%j(3) = + A_aux(3)*Q(t(1,h))%j(2)
      Z_out(h)%j(4) = + A_aux(4)*Q(t(1,h))%j(1)
      Z_out(h)%j    =  Z_out(h)%j + Z_out(h)%j

    case default
      Z_out(h)%j = 0 ! needed to detect vanishing helicity configurations

    end select
  end do

  if (ntry == 1) then
    ! n_part = number of external particles in the subtree
    Z_out(:)%n_part = Q(1)%n_part + A(1)%n_part

    ! index of subtree: 2^(i-1) + 2^(j-1) where i,j are indices for Q and A
    Z_out(:)%t = Q(1)%t + A(1)%t

    ! global helicity label of the subtree
    Z_out(:)%hf = Q(t(1,:))%hf + A(t(2,:))%hf

    call helbookkeeping_vert3(ntry, Q, A, Z_out, n, t)
  end if

end subroutine vert_QA_Z


! **********************************************************************
subroutine vert_WQ_A(ntry, W, Q, Q_out, n, t)
! bare WQ -> Q W-like (i.e. left-handed) interaction
! ----------------------------------------------------------------------
! ntry          = 1 (2) for 1st (subsequent) PS points
! Q(1:n(1))     = incoming quark
! W(1:n(2))     = incoming W  (light-cone representation)
! Q_out(1:n(3)) = outgoing quark
! Q_out(h)%j(i) = W(t(2,h))%j(A) * [gamma_A*w_L](i,j) * Q(t(1,h))%j(j)
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun
  use ol_h_helicity_bookkeeping_/**/REALKIND, only: helbookkeeping_vert3
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(2,n(3))
  type(wfun),        intent(in)    :: W(n(1)), Q(n(2))
  type(wfun),        intent(out)   :: Q_out(n(3))
  integer :: h

  do h = 1, n(3)
    select case (Q(t(2,h))%h)

    case (B"01", B"11")
    Q_out(h)%j(1) = - W(t(1,h))%j(2)*Q(t(2,h))%j(3) + W(t(1,h))%j(4)*Q(t(2,h))%j(4)
    Q_out(h)%j(2) = - W(t(1,h))%j(1)*Q(t(2,h))%j(4) + W(t(1,h))%j(3)*Q(t(2,h))%j(3)
    Q_out(h)%j(3:4) = 0
    Q_out(h)%h    = B"10"

    case default
    Q_out(h)%j = 0 ! needed to detect vanishing helicity configurations
    Q_out(h)%h = B"00"

    end select
  end do

  if (ntry == 1) then
    ! n_part = number of external particles in the subtree
    Q_out(:)%n_part = W(1)%n_part + Q(1)%n_part

    ! index of subtree: 2^(i-1) + 2^(j-1) where i,j are indices for W and Q
    Q_out(:)%t = W(1)%t + Q(1)%t

    ! global helicity label of the subtree
    Q_out(:)%hf = W(t(1,:))%hf + Q(t(2,:))%hf

    call helbookkeeping_vert3(ntry, W, Q, Q_out, n, t)
  end if

end subroutine vert_WQ_A


! **********************************************************************
subroutine vert_AW_Q(ntry, A, W, A_out, n, t)
! bare AW -> A W-like (i.e. left-handed) interaction
! ----------------------------------------------------------------------
! ntry          = 1 (2) for 1st (subsequent) PS points
! A(1:n(1))     = incoming anti-quark
! W(1:n(2))     = incoming W  (light-cone representation)
! A_out(1:n(3)) = outgoing anti-quark
! A_out(h)%j(i) = A(t(1,h))%j(j) * [gamma_A*w_L](j,i) * W(t(2,h))%j(A)
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun
  use ol_h_helicity_bookkeeping_/**/REALKIND, only: helbookkeeping_vert3
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(2,n(3))
  type(wfun),        intent(in)    :: A(n(1)), W(n(2))
  type(wfun),        intent(out)   :: A_out(n(3))
  integer :: h

  do h = 1, n(3)
    select case (A(t(1,h))%h)

    case (B"10", B"11")
      A_out(h)%j(1:2) = 0
      A_out(h)%j(3) = - W(t(2,h))%j(2)*A(t(1,h))%j(1) + W(t(2,h))%j(3)*A(t(1,h))%j(2)
      A_out(h)%j(4) = - W(t(2,h))%j(1)*A(t(1,h))%j(2) + W(t(2,h))%j(4)*A(t(1,h))%j(1)
      A_out(h)%h    = B"01"

    case default
      A_out(h)%j = 0  ! needed to detect vanishing helicity configurations
      A_out(h)%h = B"00"

    end select
  end do

  if (ntry == 1) then
    ! n_part = number of external particles in the subtree
    A_out(:)%n_part = A(1)%n_part + W(1)%n_part

    ! index of subtree: 2^(i-1) + 2^(j-1) where i,j are indices for A and W
    A_out(:)%t = A(1)%t + W(1)%t

    ! global helicity label of the subtree
    A_out(:)%hf = A(t(1,:))%hf + W(t(2,:))%hf

    call helbookkeeping_vert3(ntry, A, W, A_out, n, t)
  end if

end subroutine vert_AW_Q


! **********************************************************************
subroutine vert_QA_W(ntry, Q, A, W_out, n, t)
! bare QA -> W W-like (i.e. left-handed) interaction
! ----------------------------------------------------------------------
! ntry          = 1 (2) for 1st (subsequent) PS points
! Q(1:n(1))     = incoming quark
! A(1:n(2))     = incoming anti-quark
! W_out(1:n(3)) = outgoing W  (light-cone representation)
! W_out(h)%j(A) = A(t(2,h))%j(i) * [gamma^A*w_L](i,j) * Q(t(1,h))%j(j)
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun
  use ol_h_helicity_bookkeeping_/**/REALKIND, only: helbookkeeping_vert3
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(2,n(3))
  type(wfun),        intent(in)    :: Q(n(1)), A(n(2))
  type(wfun),        intent(out)   :: W_out(n(3))
  integer :: h

  do h = 1, n(3)
    select case (ishft(Q(t(1,h))%h,2) + A(t(2,h))%h)

    case (B"1111", B"1110", B"0111", B"0110")
      W_out(h)%j(1) = - A(t(2,h))%j(1)*Q(t(1,h))%j(3)
      W_out(h)%j(2) = - A(t(2,h))%j(2)*Q(t(1,h))%j(4)
      W_out(h)%j(3) = - A(t(2,h))%j(1)*Q(t(1,h))%j(4)
      W_out(h)%j(4) = - A(t(2,h))%j(2)*Q(t(1,h))%j(3)
      W_out(h)%j    = W_out(h)%j + W_out(h)%j

    case default
      W_out(h)%j = 0 ! needed to detect vanishing helicity configurations

    end select
  end do

  if (ntry == 1) then
    ! n_part = number of external particles in the subtree
    W_out(:)%n_part = Q(1)%n_part + A(1)%n_part

    ! index of subtree: 2^(i-1) + 2^(j-1) where i,j are indices for Q and A
    W_out(:)%t = Q(1)%t + A(1)%t

    ! global helicity label of the subtree
    W_out(:)%hf = Q(t(1,:))%hf + A(t(2,:))%hf

    call helbookkeeping_vert3(ntry, Q, A, W_out, n, t)
  end if

end subroutine vert_QA_W


! **********************************************************************
subroutine vert_VQ_A(ntry, V, Q, Q_out, n, t)
! bare VQ -> Q gluon-like (i.e. vector-like) interaction
! ----------------------------------------------------------------------
! ntry          = 1 (2) for 1st (subsequent) PS points
! V(1:n(1))     = incoming gluon (light-cone representation)
! Q(1:n(2))     = incoming quark
! Q_out(1:n(3)) = outgoing quark
! Q_out(h)%j(i) = V(t(1,h))%j(A) * gamma_A(i,j) * Q(t(2,h))%j(j)
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun
  use ol_h_helicity_bookkeeping_/**/REALKIND, only: helbookkeeping_vert3
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(2,n(3))
  type(wfun),        intent(in)    :: V(n(1)), Q(n(2))
  type(wfun),        intent(out)   :: Q_out(n(3))
  integer :: h

   do h = 1, n(3)

    select case (Q(t(2,h))%h)

    case (B"01")
      Q_out(h)%j(1) = - V(t(1,h))%j(2)*Q(t(2,h))%j(3)+V(t(1,h))%j(4)*Q(t(2,h))%j(4)
      Q_out(h)%j(2) = - V(t(1,h))%j(1)*Q(t(2,h))%j(4)+V(t(1,h))%j(3)*Q(t(2,h))%j(3)
      Q_out(h)%j(3:4) = 0
      Q_out(h)%h    = B"10"

    case (B"10")
      Q_out(h)%j(1:2) = 0
      Q_out(h)%j(3) = - V(t(1,h))%j(1)*Q(t(2,h))%j(1)-V(t(1,h))%j(4)*Q(t(2,h))%j(2)
      Q_out(h)%j(4) = - V(t(1,h))%j(2)*Q(t(2,h))%j(2)-V(t(1,h))%j(3)*Q(t(2,h))%j(1)
      Q_out(h)%h    = B"01"

    case (B"00")
      Q_out(h)%j = 0 ! needed to detect vanishing helicity configurations
      Q_out(h)%h = B"00"

    case default
      Q_out(h)%j(1) = - V(t(1,h))%j(2)*Q(t(2,h))%j(3)+V(t(1,h))%j(4)*Q(t(2,h))%j(4)
      Q_out(h)%j(2) = - V(t(1,h))%j(1)*Q(t(2,h))%j(4)+V(t(1,h))%j(3)*Q(t(2,h))%j(3)
      Q_out(h)%j(3) = - V(t(1,h))%j(1)*Q(t(2,h))%j(1)-V(t(1,h))%j(4)*Q(t(2,h))%j(2)
      Q_out(h)%j(4) = - V(t(1,h))%j(2)*Q(t(2,h))%j(2)-V(t(1,h))%j(3)*Q(t(2,h))%j(1)
      Q_out(h)%h    = B"11"

    end select
  end do

  if (ntry == 1) then
    ! n_part = number of external particles in the subtree
    Q_out(:)%n_part = V(1)%n_part + Q(1)%n_part

    ! index of subtree: 2^(i-1) + 2^(j-1) where i,j are indices for V and Q
    Q_out(:)%t = V(1)%t + Q(1)%t

    ! global helicity label of the subtree
    Q_out(:)%hf = V(t(1,:))%hf + Q(t(2,:))%hf

    call helbookkeeping_vert3(ntry, V, Q, Q_out, n, t)
  end if

end subroutine vert_VQ_A


! **********************************************************************
subroutine vert_AV_Q(ntry, A, V, A_out, n, t)
! bare AV -> A gluon-like (i.e. vector-like) interaction
! ----------------------------------------------------------------------
! ntry          = 1 (2) for 1st (subsequent) PS points
! A(1:n(1))     = incoming anti-quark
! V(1:n(2))     = incoming gluon (light-cone representation)
! A_out(1:n(3)) = outgoing anti-quark
! A_out(h)%j(i) = A(t(1,h))%j(j) * gamma_A(j,i) * V(t(2,h))%j(A)
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun
  use ol_h_helicity_bookkeeping_/**/REALKIND, only: helbookkeeping_vert3
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(2,n(3))
  type(wfun),        intent(in)    :: A(n(1)), V(n(2))
  type(wfun),        intent(out)   :: A_out(n(3))
  integer :: h

  do h = 1, n(3)

    select case (A(t(1,h))%h)

    case (B"01")
      A_out(h)%j(1) = - V(t(2,h))%j(1)*A(t(1,h))%j(3) - V(t(2,h))%j(3)*A(t(1,h))%j(4)
      A_out(h)%j(2) = - V(t(2,h))%j(2)*A(t(1,h))%j(4) - V(t(2,h))%j(4)*A(t(1,h))%j(3)
      A_out(h)%j(3:4) = 0
      A_out(h)%h    = B"10"

    case (B"10")
      A_out(h)%j(1:2) = 0
      A_out(h)%j(3) = - V(t(2,h))%j(2)*A(t(1,h))%j(1) + V(t(2,h))%j(3)*A(t(1,h))%j(2)
      A_out(h)%j(4) = - V(t(2,h))%j(1)*A(t(1,h))%j(2) + V(t(2,h))%j(4)*A(t(1,h))%j(1)
      A_out(h)%h    = B"01"

    case (B"00")
      A_out(h)%j = 0 ! needed to detect vanishing helicity configurations
      A_out(h)%h = B"00"

    case default
      A_out(h)%j(1) = - V(t(2,h))%j(1)*A(t(1,h))%j(3) - V(t(2,h))%j(3)*A(t(1,h))%j(4)
      A_out(h)%j(2) = - V(t(2,h))%j(2)*A(t(1,h))%j(4) - V(t(2,h))%j(4)*A(t(1,h))%j(3)
      A_out(h)%j(3) = - V(t(2,h))%j(2)*A(t(1,h))%j(1) + V(t(2,h))%j(3)*A(t(1,h))%j(2)
      A_out(h)%j(4) = - V(t(2,h))%j(1)*A(t(1,h))%j(2) + V(t(2,h))%j(4)*A(t(1,h))%j(1)
      A_out(h)%h    = B"11"

    end select
  end do

  if (ntry == 1) then
    ! n_part = number of external particles in the subtree
    A_out(:)%n_part = A(1)%n_part + V(1)%n_part

    ! index of subtree: 2^(i-1) + 2^(j-1) where i,j are indices for A and V
    A_out(:)%t = A(1)%t + V(1)%t

    ! global helicity label of the subtree
    A_out(:)%hf = A(t(1,:))%hf + V(t(2,:))%hf

    call helbookkeeping_vert3(ntry, A, V, A_out, n, t)
  end if

end subroutine vert_AV_Q


! **********************************************************************
subroutine vert_QA_V(ntry, Q, A, V_out, n, t)
! bare QA -> V gluon-like (i.e. vector-like) interaction
! ----------------------------------------------------------------------
! ntry          = 1 (2) for 1st (subsequent) PS points
! Q(1:n(1))     = incoming quark
! A(1:n(2))     = incoming anti-quark
! V_out(1:n(3)) = outgoing gluon
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun
  use ol_h_helicity_bookkeeping_/**/REALKIND, only: helbookkeeping_vert3
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(2,n(3))
  type(wfun),        intent(in)    :: Q(n(1)), A(n(2))
  type(wfun),        intent(out)   :: V_out(n(3))
  integer :: h

  do h = 1, n(3)
    select case (ishft(Q(t(1,h))%h,2) + A(t(2,h))%h)

    case (B"1111")
      V_out(h)%j(1) = - A(t(2,h))%j(1)*Q(t(1,h))%j(3) - A(t(2,h))%j(4)*Q(t(1,h))%j(2)
      V_out(h)%j(2) = - A(t(2,h))%j(2)*Q(t(1,h))%j(4) - A(t(2,h))%j(3)*Q(t(1,h))%j(1)
      V_out(h)%j(3) = - A(t(2,h))%j(1)*Q(t(1,h))%j(4) + A(t(2,h))%j(3)*Q(t(1,h))%j(2)
      V_out(h)%j(4) = - A(t(2,h))%j(2)*Q(t(1,h))%j(3) + A(t(2,h))%j(4)*Q(t(1,h))%j(1)
      V_out(h)%j = V_out(h)%j + V_out(h)%j

    case (B"0110", B"0111", B"1110")
      V_out(h)%j(1) = - A(t(2,h))%j(1)*Q(t(1,h))%j(3)
      V_out(h)%j(2) = - A(t(2,h))%j(2)*Q(t(1,h))%j(4)
      V_out(h)%j(3) = - A(t(2,h))%j(1)*Q(t(1,h))%j(4)
      V_out(h)%j(4) = - A(t(2,h))%j(2)*Q(t(1,h))%j(3)
      V_out(h)%j = V_out(h)%j + V_out(h)%j

    case (B"1001", B"1101", B"1011")
      V_out(h)%j(1) = - A(t(2,h))%j(4)*Q(t(1,h))%j(2)
      V_out(h)%j(2) = - A(t(2,h))%j(3)*Q(t(1,h))%j(1)
      V_out(h)%j(3) =   A(t(2,h))%j(3)*Q(t(1,h))%j(2)
      V_out(h)%j(4) =   A(t(2,h))%j(4)*Q(t(1,h))%j(1)
      V_out(h)%j = V_out(h)%j + V_out(h)%j

    case default
      V_out(h)%j = 0 ! needed to detect vanishing helicity configurations

    end select
  end do

  if (ntry == 1) then
    ! n_part = number of external particles in the subtree
    V_out(:)%n_part = Q(1)%n_part + A(1)%n_part

    ! index of subtree: 2^(i-1) + 2^(j-1) where i,j are indices for Q and A
    V_out(:)%t = Q(1)%t + A(1)%t

    ! global helicity label of the subtree
    V_out(:)%hf = Q(t(1,:))%hf + A(t(2,:))%hf

    call helbookkeeping_vert3(ntry, Q, A, V_out, n, t)
  end if

end subroutine vert_QA_V


! **********************************************************************
subroutine vert_UV_W(ntry, V1, P1, V2, P2, V_out, n, t)
! bare VV -> V vertex
! ----------------------------------------------------------------------
! ntry           = 1 (2) for 1st (subsequent) PS points
! Vi(1:n(i))     = incoming gluon (light-cone representation)
! Pi(4)          = incoming Vi momentum  (light-cone representation)
! V_out(1:n(3))  = outgoing gluon (light-cone representation)
! V_out(h)%j(a3) = {  g(a1,a2)*[P1-P2](a3) + g(a2,a3)*[P2+Pout](a1)
!               + g(a3,a1)*[-Pout-P1](a2)} * V1(t(1,h))%j(a1) * V2(t(2,h))%j(a2)
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun
  use ol_h_helicity_bookkeeping_/**/REALKIND, only: helbookkeeping_vert3
!   use ol_h_contractions_/**/REALKIND, only: cont_PP
  implicit none
  integer(intkind1), intent(in)   :: ntry
  integer(intkind2), intent(inout):: n(3), t(2,n(3))
  type(wfun),        intent(in)   :: V1(n(1)), V2(n(2))
  complex(REALKIND), intent(in)   :: P1(4), P2(4)
  type(wfun),        intent(out)  :: V_out(n(3))
  complex(REALKIND) :: J1J2, P1J2(n(2)), P2J1(n(1))
  complex(REALKIND) :: P1half(4), P2half(4), P12(4), P112(4), P122(4)
  integer :: h

  P1half = 0.5_/**/REALKIND * P1
  P2half = 0.5_/**/REALKIND * P2
  P12  = P1half - P2half
  P112 = P1half + P1half + P2half
  P122 = P1half + P2half + P2half
  do h = 1, n(1)
    P2J1(h) = P122(1) * V1(h)%j(2) + P122(2) * V1(h)%j(1) &
            - P122(3) * V1(h)%j(4) - P122(4) * V1(h)%j(3)
  end do
  do h = 1, n(2)
    P1J2(h) = P112(1) * V2(h)%j(2) + P112(2) * V2(h)%j(1) &
             -P112(3) * V2(h)%j(4) - P112(4) * V2(h)%j(3)
  end do
  do h = 1, n(3)
    J1J2 = V1(t(1,h))%j(1) * V2(t(2,h))%j(2) + V1(t(1,h))%j(2) * V2(t(2,h))%j(1) &
          -V1(t(1,h))%j(3) * V2(t(2,h))%j(4) - V1(t(1,h))%j(4) * V2(t(2,h))%j(3)

    V_out(h)%j = J1J2 * P12 + P2J1(t(1,h)) * V2(t(2,h))%j - P1J2(t(2,h)) * V1(t(1,h))%j

  end do

  if (ntry == 1) then
    ! n_part = number of external particles in the subtree
    V_out(:)%n_part = V1(1)%n_part + V2(1)%n_part

    ! index of subtree: 2^(i-1) + 2^(j-1) where i,j are indices for V1 and V2
    V_out(:)%t = V1(1)%t + V2(1)%t

    ! global helicity label of the subtree
    V_out(:)%hf = V1(t(1,:))%hf + V2(t(2,:))%hf

    call helbookkeeping_vert3(ntry, V1, V2, V_out, n, t)
  end if

end subroutine vert_UV_W


! **********************************************************************
subroutine vert_GGG_G(ntry, G1, G2, G3, G_out, n, t)
! Four-gluon vertex: factorised colour monomials f(a,b,x)*f(c,d,x) (same as EV_V)
! ----------------------------------------------------------------------
! ntry          = 1 (2) for 1st (subsequent) PS points
! Gi(1:n(i))    = incoming gluons
! G_out(1:n(4)) = outgoing gluon
! G_out(h)%j(d) = (g(a,c)*g(b,d) + g(1,4)*g(2,3)) * G1(t(1,h))%j(a)
!                               * G2(t(2,h))%j(b) * G3(t(3,h))%j(c)
!               = G1(t(1,h))%j.G3(t(3,h))%j * G2(t(2,h))%j(d)
!               + G2(t(2,h))%j.G3(t(3,h))%j * G1(t(1,h))%j(d)
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun
  use ol_h_helicity_bookkeeping_/**/REALKIND, only: helbookkeeping_vert4
  use ol_h_contractions_/**/REALKIND, only: cont_PP
  implicit none
  integer(intkind1),  intent(in)    :: ntry
  integer(intkind2),  intent(inout) :: n(4), t(3,n(4))
  type(wfun), intent(in)   :: G1(n(1)), G2(n(2)), G3(n(3))
  type(wfun), intent(out)  :: G_out(n(4))
  integer :: h

  do h = 1, n(4)
    G_out(h)%j = cont_PP(G1(t(1,h))%j,G3(t(3,h))%j)*G2(t(2,h))%j &
               - cont_PP(G2(t(2,h))%j,G3(t(3,h))%j)*G1(t(1,h))%j
  end do

  if (ntry == 1) then

    ! index of subtree: 2^(i-1) + 2^(j-1) where i,j are indices for V and Q
    G_out(:)%t = G1(1)%t + G2(1)%t + G3(1)%t

    ! n_part = number of external particles in the subtree
    G_out(:)%n_part = G1(1)%n_part + G2(1)%n_part + G3(1)%n_part

    ! global helicity label of the subtree
    G_out(:)%hf = G1(t(1,:))%hf + G2(t(2,:))%hf + G3(t(3,:))%hf

    call helbookkeeping_vert4(ntry, G1, G2, G3, G_out, n, t)
  end if

end subroutine vert_GGG_G

! **********************************************************************
subroutine vert_VV_S(ntry, V1, V2, S_out, n, t)
! Two vector boson + scalar vertex
! ----------------------------------------------------------------------
! ntry            : 1 (2) for 1st (subsequent) PS points
! Incoming vectors: Vi(1:n(i))  (light-cone representation)
! Outgoing scalar : S_out(1:n(3))
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun
  use ol_h_helicity_bookkeeping_/**/REALKIND, only: helbookkeeping_vert3, checkzero_scalar
  use ol_h_contractions_/**/REALKIND, only: cont_PP
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(2,n(3))
  type(wfun),        intent(in)    :: V1(n(1)), V2(n(2))
  type(wfun),        intent(out)   :: S_out(n(3))
  integer :: h

  do h = 1, n(3)
    S_out(h)%j(1) = cont_PP(V1(t(1,h))%j,V2(t(2,h))%j)
  end do

  if (ntry == 1) then
    ! index of subtree: 2^(i-1) + 2^(j-1) where i,j are indices for V1 and V2
    S_out(:)%t = V1(1)%t + V2(1)%t

    ! n_part = number of external particles in the subtree
    S_out(:)%n_part = V1(1)%n_part + V2(1)%n_part

    ! global helicity label of the subtree
    S_out(:)%hf = V1(t(1,:))%hf + V2(t(2,:))%hf

    call checkzero_scalar(S_out)
    call helbookkeeping_vert3(ntry, V1, V2, S_out, n, t)
  end if

end subroutine vert_VV_S


! **********************************************************************
subroutine vert_QS_A(g_RL, ntry, Q, S, Q_out, n, t)
! Fermion-scalar-vertex
! ----------------------------------------------------------------------
! ntry                  : 1 (2) for 1st (subsequent) PS points
! right-handed coupling : g_RL(1)
! left-handed coupling  : g_RL(2)
! Incoming fermion      : Q(1:n(1))
! Incoming scalar       : S(1:n(2))
! Outgoing fermion      : Q_out(1:n(3))
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun
  use ol_h_helicity_bookkeeping_/**/REALKIND, only: helbookkeeping_vert3
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(2,n(3))
  type(wfun),        intent(in)    :: Q(n(1)), S(n(2))
  complex(REALKIND), intent(in)    :: g_RL(2)
  type(wfun),        intent(out)   :: Q_out(n(3))
  complex(REALKIND) :: g_aux(2)
  integer :: h

  do h = 1, n(3)
    select case (Q(t(1,h))%h)

    case (B"01")
      g_aux(2)   = g_RL(2) * S(t(2,h))%j(1)
      Q_out(h)%j(1:2) = 0
      Q_out(h)%j(3:4) = g_aux(2) * Q(t(1,h))%j(3:4)
      Q_out(h)%h    = B"01"

    case (B"10")
      g_aux(1)   = g_RL(1) * S(t(2,h))%j(1)
      Q_out(h)%j(1:2) = g_aux(1) * Q(t(1,h))%j(1:2)
      Q_out(h)%j(3:4) = 0
      Q_out(h)%h    = B"10"

    case (B"00")
      Q_out(h)%j = 0 ! needed to detect vanishing helicity configurations
      Q_out(h)%h = B"00"

    case default
      g_aux        = g_RL * S(t(2,h))%j(1)
      Q_out(h)%j(1:2) = g_aux(1) * Q(t(1,h))%j(1:2)
      Q_out(h)%j(3:4) = g_aux(2) * Q(t(1,h))%j(3:4)
      Q_out(h)%h      = B"11"

    end select
  end do

  if (ntry == 1) then
    ! n_part = number of external particles in the subtree
    Q_out(:)%n_part = Q(1)%n_part + S(1)%n_part

    ! index of subtree: 2^(i-1) + 2^(j-1) where i,j are indices for Q and S
    Q_out(:)%t = Q(1)%t + S(1)%t

    ! global helicity label of the subtree
    Q_out(:)%hf = Q(t(1,:))%hf + S(t(2,:))%hf

    call helbookkeeping_vert3(ntry, Q, S, Q_out, n, t)
  end if

end subroutine vert_QS_A


!**********************************************************************
subroutine vert_SA_Q(g_RL, ntry, S, A, A_out, n, t)
! Fermion-scalar-vertex
! ----------------------------------------------------------------------
! ntry                  : 1 (2) for 1st (subsequent) PS points
! right-handed coupling : g_RL(1)
! left-handed coupling  : g_RL(2)
! Incoming scalar       : S(1:n(1))
! Incoming anti-fermion : A(1:n(2))
! Outgoing anti-fermion : A_out(1:n(3))
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun
  use ol_h_helicity_bookkeeping_/**/REALKIND, only: helbookkeeping_vert3
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(2,n(3))
  type(wfun),        intent(in)    :: S(n(1)), A(n(2))
  complex(REALKIND), intent(in)    :: g_RL(2)
  type(wfun),        intent(out)   :: A_out(n(3))
  complex(REALKIND) :: g_aux(2)
  integer :: h

  do h = 1, n(3)
    select case (A(t(2,h))%h)

    case (B"01")
      g_aux(2)   = g_RL(2) * S(t(1,h))%j(1)
      A_out(h)%j(1:2) = 0
      A_out(h)%j(3:4) = g_aux(2) * A(t(2,h))%j(3:4)
      A_out(h)%h    = B"01"

    case (B"10")
      g_aux(1)   = g_RL(1) * S(t(1,h))%j(1)
      A_out(h)%j(1:2) = g_aux(1) * A(t(2,h))%j(1:2)
      A_out(h)%j(3:4) = 0
      A_out(h)%h    = B"10"

    case (B"00")
      A_out(h)%j = 0 ! needed to detect vanishing helicity configurations
      A_out(h)%h = B"00"

    case default
      g_aux = g_RL * S(t(1,h))%j(1)
      A_out(h)%j(1:2) = g_aux(1) * A(t(2,h))%j(1:2)
      A_out(h)%j(3:4) = g_aux(2) * A(t(2,h))%j(3:4)
      A_out(h)%h      = B"11"

    end select
  end do

  if (ntry == 1) then
    ! n_part = number of external particles in the subtree
    A_out(:)%n_part = S(1)%n_part + A(1)%n_part

    ! index of subtree: 2^(i-1) + 2^(j-1) where i,j are indices for S and A
    A_out(:)%t = S(1)%t + A(1)%t

    ! global helicity label of the subtree
    A_out(:)%hf = S(t(1,:))%hf + A(t(2,:))%hf

    call helbookkeeping_vert3(ntry, S, A, A_out, n, t)
  end if

end subroutine vert_SA_Q


! **********************************************************************
subroutine vert_AQ_S(g_RL, ntry, A, Q, S_out, n, t)
! Fermion-scalar-vertex
! ----------------------------------------------------------------------
! ntry                  : 1 (2) for 1st (subsequent) PS points
! right-handed coupling : g_RL(1)
! left-handed coupling  : g_RL(2)
! Incoming anti-fermion : A(1:n(1))
! Incoming fermion      : Q(1:n(2))
! Outgoing scalar       : S_out(1:n(3))
!                         S_out(h)%j(1) = gR*A(t(1,h))%j.P_R.Q(t(2,h))%j
!                                       + gL*A(t(1,h))%j.P_L.Q(t(2,h))%j
! with the chiral projectors P_R = (1+y5)/2 and P_L = (1-y5)/2
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun
  use ol_h_helicity_bookkeeping_/**/REALKIND, only: helbookkeeping_vert3, checkzero_scalar
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(2,n(3))
  type(wfun),        intent(in)    :: A(n(1)), Q(n(2))
  complex(REALKIND), intent(in)    :: g_RL(2)
  type(wfun),        intent(out)   :: S_out(n(3))
  integer :: h

  do h = 1, n(3)
    select case (ishft(Q(t(2,h))%h,2) + A(t(1,h))%h)

    case (B"1111")
      S_out(h)%j(1) = g_RL(1) * (A(t(1,h))%j(1)*Q(t(2,h))%j(1) + A(t(1,h))%j(2)*Q(t(2,h))%j(2)) &
                    + g_RL(2) * (A(t(1,h))%j(3)*Q(t(2,h))%j(3) + A(t(1,h))%j(4)*Q(t(2,h))%j(4))

    case (B"1010", B"1110", B"1011")
      S_out(h)%j(1) = g_RL(1) * (A(t(1,h))%j(1)*Q(t(2,h))%j(1) + A(t(1,h))%j(2)*Q(t(2,h))%j(2))

    case (B"0101", B"1101", B"0111")
      S_out(h)%j(1) = g_RL(2) * (A(t(1,h))%j(3)*Q(t(2,h))%j(3) + A(t(1,h))%j(4)*Q(t(2,h))%j(4))

    case default
      S_out(h)%j(1) = 0 ! needed to detect vanishing helicity configurations

    end select
  end do

  if (ntry == 1) then
    ! n_part = number of external particles in the subtree
    S_out(:)%n_part = A(1)%n_part + Q(1)%n_part

    ! index of subtree: 2^(i-1) + 2^(j-1) where i,j are indices for A and Q
    S_out(:)%t = A(1)%t + Q(1)%t

    ! global helicity label of the subtree
    S_out(:)%hf = A(t(1,:))%hf + Q(t(2,:))%hf

    call checkzero_scalar(S_out)
    call helbookkeeping_vert3(ntry, A, Q, S_out, n, t)
  end if

end subroutine vert_AQ_S

! **********************************************************************
subroutine vert_SV_V(ntry, S, V, V_out, n, t)
! ----------------------------------------------------------------------
! Two vector boson + scalar vertex
! ----------------------------------------------------------------------
! ntry            : 1 (2) for 1st (subsequent) PS points
! Incoming scalar : S(1:n(1))
! Incoming vector : V(1:n(2)) (light-cone representation)
! Outgoing vector : V_out(1:n(3))
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun
  use ol_h_helicity_bookkeeping_/**/REALKIND, only: helbookkeeping_vert3
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(2,n(3))
  type(wfun),        intent(in)    :: S(n(1)), V(n(2))
  type(wfun),        intent(out)   :: V_out(n(3))
  integer :: h

  do h = 1, n(3)
    V_out(h)%j = S(t(1,h))%j(1) * V(t(2,h))%j
  end do

  if (ntry == 1) then
    V_out(:)%n_part = S(1)%n_part + V(1)%n_part
    V_out(:)%t = S(1)%t + V(1)%t
    V_out(:)%hf = S(t(1,:))%hf + V(t(2,:))%hf
    call helbookkeeping_vert3(ntry, S, V, V_out, n, t)
  end if

end subroutine vert_SV_V


end module ol_hel_vertices_/**/REALKIND
