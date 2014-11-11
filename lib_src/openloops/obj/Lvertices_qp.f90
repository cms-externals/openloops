
! Copyright 2014 Fabio Cascioli, Jonas Lindert, Philipp Maierhoefer, Stefano Pozzorini
!
! This file is part of OpenLoops.
!
! OpenLoops is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! OpenLoops is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with OpenLoops.  If not, see <http://www.gnu.org/licenses/>.


module ol_loop_vertices_qp
  implicit none
  contains

! ******************************************************************************
subroutine vert_loop_AZ_Q(rank_in, rank_out, g_RL, G_A, J_Z, Gout_A)
! bare AZ -> A Z-like interaction
! ------------------------------------------------------------------------------
! G_A(sigma,l)   = incoming anti-quark loop-current
! sigma          = open index of loop line
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_Z(4)         = incoming tree level Z current (contravariant light-cone rep.)
! g_RL(1)        = right-handed coupling gR
! g_RL(2)        = left-handed coupling gL
! Gout_A(beta,l) = outgoing anti-quark loop current with same rank
! Gout_A(beta,l) = G_A(sigma,l) * [gamma_A*(gR*w_R+gL*w_L)](sigma,beta) * J_Z(A)
! ******************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_A(4,rank_in), J_Z(4), g_RL(2)
  complex(qp), intent(out) :: Gout_A(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_A(1,l) = g_RL(1) * ( - J_Z(1)*G_A(3,l) - J_Z(3)*G_A(4,l))
    Gout_A(2,l) = g_RL(1) * ( - J_Z(2)*G_A(4,l) - J_Z(4)*G_A(3,l))
    Gout_A(3,l) = g_RL(2) * ( - J_Z(2)*G_A(1,l) + J_Z(3)*G_A(2,l))
    Gout_A(4,l) = g_RL(2) * ( - J_Z(1)*G_A(2,l) + J_Z(4)*G_A(1,l))
  end do
end subroutine vert_loop_AZ_Q


! **********************************************************************************
subroutine vert_loop_AQ_Z(rank_in, rank_out, g_RL, G_A, J_Q, Gout_Z)
! bare AQ -> Z Z-like interaction
! ----------------------------------------------------------------------------------
! G_A(sigma,l)   = incoming anti-quark loop-current
! sigma          = open index of loop line
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_Q(4)         = incoming tree level quark current
! g_RL(1)        = right-handed coupling gR
! g_RL(2)        = left-handed coupling gL
! Gout_Z         = outgoing Z loop current (contravariant light-cone rep.)
! Gout_Z(beta,l) = G_A(sigma,l) * [gamma^beta*(gR*w_R+gL*w_L)](sigma,rho) * J_Q(rho)
! **********************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_A(4,rank_in), J_Q(4), g_RL(2)
  complex(qp), intent(out) :: Gout_Z(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_Z(1,l) = - g_RL(2)*G_A(1,l)*J_Q(3) - g_RL(1)*G_A(4,l)*J_Q(2)
    Gout_Z(2,l) = - g_RL(2)*G_A(2,l)*J_Q(4) - g_RL(1)*G_A(3,l)*J_Q(1)
    Gout_Z(3,l) = - g_RL(2)*G_A(1,l)*J_Q(4) + g_RL(1)*G_A(3,l)*J_Q(2)
    Gout_Z(4,l) = - g_RL(2)*G_A(2,l)*J_Q(3) + g_RL(1)*G_A(4,l)*J_Q(1)
    Gout_Z(:,l) = Gout_Z(:,l) + Gout_Z(:,l)
  end do
end subroutine vert_loop_AQ_Z


! ***********************************************************************************
subroutine vert_loop_ZA_Q(rank_in, rank_out, g_RL, G_Z, J_A, Gout_A)
! bare ZA -> A Z-like interaction
! -----------------------------------------------------------------------------------
! G_Z(sigma,l)   = incoming Z-boson loop-current
! sigma          = open index of loop line (contravariant light-cone)
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_A(4)         = incoming antiquark current
! g_RL(1)        = right-handed coupling gR
! g_RL(2)        = left-handed coupling gL
! Gout_A(beta,l) = outgoing anti-quark loop current
! Gout_A(beta,l) = G_Z(sigma,l) * Jbar_A(rho)*[gamma_sigma*(gR*w_R+gL*w_L)](rho,beta)
! ***********************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_Z(4,rank_in), J_A(4), g_RL(2)
  complex(qp), intent(out) :: Gout_A(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_A(1,l) = g_RL(1) * ( - J_A(3)*G_Z(1,l) - J_A(4)*G_Z(3,l))
    Gout_A(2,l) = g_RL(1) * ( - J_A(4)*G_Z(2,l) - J_A(3)*G_Z(4,l))
    Gout_A(3,l) = g_RL(2) * ( - J_A(1)*G_Z(2,l) + J_A(2)*G_Z(3,l))
    Gout_A(4,l) = g_RL(2) * ( - J_A(2)*G_Z(1,l) + J_A(1)*G_Z(4,l))
  end do
end subroutine vert_loop_ZA_Q


! ***************************************************************************
subroutine vert_loop_QZ_A(rank_in, rank_out, g_RL, G_Q, J_Z, Gout_Q)
! bare QZ -> Q Z-like interaction
! ---------------------------------------------------------------------------
! G_Q(sigma,l)   = incoming quark loop-current
! sigma          = open index of loop line
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_Z(4)         = incoming Z current (contravariant light-cone rep.)
! g_RL(1)        = right-handed coupling gR
! g_RL(2)        = left-handed coupling gL
! Gout_Q(beta,l) = outgoing quark loop current
! Gout_Q(beta,l) = J_Z(A)*[gamma_A*(gR*w_R+gL*w_L)](beta, sigma)*G_Q(sigma,l)
! ***************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_Q(4,rank_in), J_Z(4), g_RL(2)
  complex(qp), intent(out) :: Gout_Q(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_Q(1,l) = g_RL(2) * ( - J_Z(2)*G_Q(3,l) + J_Z(4)*G_Q(4,l))
    Gout_Q(2,l) = g_RL(2) * ( - J_Z(1)*G_Q(4,l) + J_Z(3)*G_Q(3,l))
    Gout_Q(3,l) = g_RL(1) * ( - J_Z(1)*G_Q(1,l) - J_Z(4)*G_Q(2,l))
    Gout_Q(4,l) = g_RL(1) * ( - J_Z(2)*G_Q(2,l) - J_Z(3)*G_Q(1,l))
  end do
end subroutine vert_loop_QZ_A


! ************************************************************************************
subroutine vert_loop_QA_Z(rank_in, rank_out, g_RL, G_Q, J_A, Gout_Z)
! bare QA -> Z Z-like interaction
! ------------------------------------------------------------------------------------
! G_Q(sigma,l)   = incoming quark loop current
! sigma          = open index of loop line
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_A(4)         = incoming tree level anti-quark current
! g_RL(1)        = right-handed coupling gR
! g_RL(2)        = left-handed coupling gL
! Gout_Z         = outgoing Z loop current (contravariant light-cone)
! Gout_Z(beta,l) = Jbar_A(rho)*[gamma^beta*(gR*w_R+gL*w_L)](rho,sigma) * G_Q(sigma, l)
! ************************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_Q(4,rank_in), J_A(4), g_RL(2)
  complex(qp), intent(out) :: Gout_Z(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_Z(1,l) = - g_RL(2)*G_Q(3,l)*J_A(1) - g_RL(1)*G_Q(2,l)*J_A(4)
    Gout_Z(2,l) = - g_RL(2)*G_Q(4,l)*J_A(2) - g_RL(1)*G_Q(1,l)*J_A(3)
    Gout_Z(3,l) = - g_RL(2)*G_Q(4,l)*J_A(1) + g_RL(1)*G_Q(2,l)*J_A(3)
    Gout_Z(4,l) = - g_RL(2)*G_Q(3,l)*J_A(2) + g_RL(1)*G_Q(1,l)*J_A(4)
    Gout_Z(:,l) = Gout_Z(:,l) + Gout_Z(:,l)
  end do
end subroutine vert_loop_QA_Z


! **********************************************************************************
subroutine vert_loop_ZQ_A(rank_in, rank_out, g_RL, G_Z, J_Q, Gout_Q)
! bare ZQ -> Q Z-like interaction
! ----------------------------------------------------------------------------------
! G_Z(sigma,l)   = incoming Z-boson loop-current
! sigma          = open index of loop line (contravariant light-cone)
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_Q(4)         = incoming quark current
! g_RL(1)        = right-handed coupling gR
! g_RL(2)        = left-handed coupling gL
! Gout_Q(beta,l) = outgoing quark loop current
! Gout_Q(beta,l) = G_Z(sigma,l) * [gamma_sigma*(gR*w_R+gL*w_L)](beta,rho) * J_Q(rho)
! **********************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_Z(4,rank_in), J_Q(4), g_RL(2)
  complex(qp), intent(out) :: Gout_Q(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_Q(1,l) = g_RL(2) * ( - J_Q(3)*G_Z(2,l) + J_Q(4)*G_Z(4,l))
    Gout_Q(2,l) = g_RL(2) * ( - J_Q(4)*G_Z(1,l) + J_Q(3)*G_Z(3,l))
    Gout_Q(3,l) = g_RL(1) * ( - J_Q(1)*G_Z(1,l) - J_Q(2)*G_Z(4,l))
    Gout_Q(4,l) = g_RL(1) * ( - J_Q(2)*G_Z(2,l) - J_Q(1)*G_Z(3,l))
  end do
end subroutine vert_loop_ZQ_A


! ******************************************************************************
subroutine vert_loop_AW_Q(rank_in, rank_out, G_A, J_W, Gout_A)
! bare AW -> A W-like interaction
! ------------------------------------------------------------------------------
! G_A(sigma,l)   = incoming anti-quark loop-current
! sigma          = open index of loop line
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_W(4)         = incoming tree level W current (contravariant light-cone rep.)
! Gout_A(beta,l) = outgoing anti-quark loop current with same rank
! Gout_A(beta,l) = G_A(sigma,l) * [gamma_A*(gL*w_L)](sigma,beta) * J_W(A)
! ******************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_A(4,rank_in), J_W(4)
  complex(qp), intent(out) :: Gout_A(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_A(1:2,l) = 0
    Gout_A(3,l)   = - J_W(2)*G_A(1,l) + J_W(3)*G_A(2,l)
    Gout_A(4,l)   = - J_W(1)*G_A(2,l) + J_W(4)*G_A(1,l)
  end do
end subroutine vert_loop_AW_Q


! ***************************************************************************
subroutine vert_loop_AQ_W(rank_in, rank_out, G_A, J_Q, Gout_W)
! bare AQ -> W W-like interaction
! ---------------------------------------------------------------------------
! G_A(sigma,l)   = incoming anti-quark loop-current
! sigma          = open index of loop line
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_Q(4)         = incoming tree level quark current
! Gout_W         = outgoing W loop current (contravariant light-cone rep.)
! Gout_W(beta,l) = G_A(sigma,l) * [gamma^beta*(gL*w_L)](sigma,rho) * J_Q(rho)
! ***************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_A(4,rank_in), J_Q(4)
  complex(qp), intent(out) :: Gout_W(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_W(1,l) = - G_A(1,l)*J_Q(3)
    Gout_W(2,l) = - G_A(2,l)*J_Q(4)
    Gout_W(3,l) = - G_A(1,l)*J_Q(4)
    Gout_W(4,l) = - G_A(2,l)*J_Q(3)
    Gout_W(:,l) = Gout_W(:,l) + Gout_W(:,l)
  end do
end subroutine vert_loop_AQ_W


! ****************************************************************************
subroutine vert_loop_WA_Q(rank_in, rank_out, G_W, J_A, Gout_A)
! bare WA -> A W-like interaction
! ----------------------------------------------------------------------------
! G_W(sigma,l)   = incoming W-boson loop-current
! sigma          = open index of loop line (contravariant light-cone)
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_A(4)         = incoming antiquark current
! Gout_A(beta,l) = outgoing anti-quark loop current
! Gout_A(beta,l) = G_W(sigma,l) * Jbar_A(rho)*[gamma_sigma*(gL*w_L)](rho,beta)
! ****************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_W(4,rank_in), J_A(4)
  complex(qp), intent(out) :: Gout_A(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_A(1:2,l) = 0
    Gout_A(3,l)   = - J_A(1)*G_W(2,l) + J_A(2)*G_W(3,l)
    Gout_A(4,l)   = - J_A(2)*G_W(1,l) + J_A(1)*G_W(4,l)
  end do
end subroutine vert_loop_WA_Q


! *********************************************************************
subroutine vert_loop_QW_A(rank_in, rank_out, G_Q, J_W, Gout_Q)
! bare QW -> Q W-like interaction
! ---------------------------------------------------------------------
! G_Q(sigma,l)   = incoming quark loop-current
! sigma          = open index of loop line
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_W(4)         = incoming W current (contravariant light-cone rep.)
! Gout_Q(beta,l) = outgoing quark loop current
! Gout_Q(beta,l) = J_W(A)*[gamma_A*(gL*w_L)](beta, sigma)*G_Q(sigma,l)
! *********************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_Q(4,rank_in), J_W(4)
  complex(qp), intent(out) :: Gout_Q(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_Q(1,l)   = - J_W(2)*G_Q(3,l) + J_W(4)*G_Q(4,l)
    Gout_Q(2,l)   = - J_W(1)*G_Q(4,l) + J_W(3)*G_Q(3,l)
    Gout_Q(3:4,l) = 0
  end do
end subroutine vert_loop_QW_A


! *****************************************************************************
subroutine vert_loop_QA_W(rank_in, rank_out, G_Q, J_A, Gout_W)
! bare QA -> W W-like interaction
! -----------------------------------------------------------------------------
! G_Q(sigma,l)   = incoming quark loop current
! sigma          = open index of loop line
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_A(4)         = incoming tree level anti-quark current
! Gout_W         = outgoing W loop current (contravariant light-cone)
! Gout_W(beta,l) = Jbar_A(rho)*[gamma^beta*(gL*w_L)](rho,sigma) * G_Q(sigma, l)
! *****************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_Q(4,rank_in), J_A(4)
  complex(qp), intent(out) :: Gout_W(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_W(1,l) = - G_Q(3,l)*J_A(1)
    Gout_W(2,l) = - G_Q(4,l)*J_A(2)
    Gout_W(3,l) = - G_Q(4,l)*J_A(1)
    Gout_W(4,l) = - G_Q(3,l)*J_A(2)
    Gout_W(:,l) = Gout_W(:,l) + Gout_W(:,l)
  end do
end subroutine vert_loop_QA_W


! ***************************************************************************
subroutine vert_loop_WQ_A(rank_in, rank_out, G_W, J_Q, Gout_Q)
! bare WQ -> Q W-like interaction
! ---------------------------------------------------------------------------
! G_W(sigma,l)   = incoming W-boson loop-current
! sigma          = open index of loop line (contravariant light-cone)
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_Q(4)         = incoming quark current
! Gout_Q(beta,l) = outgoing quark loop current
! Gout_Q(beta,l) = G_W(sigma,l) * [gamma_sigma*(gL*w_L)](beta,rho) * J_Q(rho)
! ***************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_W(4,rank_in), J_Q(4)
  complex(qp), intent(out) :: Gout_Q(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_Q(1,l)   = - J_Q(3)*G_W(2,l) + J_Q(4)*G_W(4,l)
    Gout_Q(2,l)   = - J_Q(4)*G_W(1,l) + J_Q(3)*G_W(3,l)
    Gout_Q(3:4,l) = 0
  end do
end subroutine vert_loop_WQ_A


! ***********************************************************************************
subroutine vert_loop_AV_Q(rank_in, rank_out, G_A, J_V, Gout_A)
! bare AV -> A gluon-like interaction
! -----------------------------------------------------------------------------------
! G_A(sigma,l)   = incoming anti-quark loop-current
! sigma          = open index of loop line
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_V(4)         = incoming tree level vector current (contravariant light-cone rep.)
! Gout_A(beta,l) = outgoing anti-quark loop current with same rank
! Gout_A(beta,l) = G_A(sigma,l) * gamma_A(sigma,beta) * J_V(A)
! ***********************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_A(4,rank_in), J_V(4)
  complex(qp), intent(out) :: Gout_A(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_A(1,l) = - J_V(1)*G_A(3,l) - J_V(3)*G_A(4,l)
    Gout_A(2,l) = - J_V(2)*G_A(4,l) - J_V(4)*G_A(3,l)
    Gout_A(3,l) = - J_V(2)*G_A(1,l) + J_V(3)*G_A(2,l)
    Gout_A(4,l) = - J_V(1)*G_A(2,l) + J_V(4)*G_A(1,l)
  end do
end subroutine vert_loop_AV_Q


! *****************************************************************************
subroutine vert_loop_AQ_V(rank_in, rank_out, G_A, J_Q, Gout_V)
! bare AQ -> V gluon-like interaction
! -----------------------------------------------------------------------------
! G_A(sigma,l)   = incoming anti-quark loop-current
! sigma          = open index of loop line
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_Q(4)         = incoming tree level quark current
! Gout_V         = outgoing vector loop current (contravariant light-cone rep.)
! Gout_V(beta,l) = G_A(sigma,l) * gamma^beta(sigma,rho) * J_Q(rho)
! *****************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_A(4,rank_in), J_Q(4)
  complex(qp), intent(out) :: Gout_V(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_V(1,l) = - G_A(1,l)*J_Q(3) - G_A(4,l)*J_Q(2)
    Gout_V(2,l) = - G_A(2,l)*J_Q(4) - G_A(3,l)*J_Q(1)
    Gout_V(3,l) = - G_A(1,l)*J_Q(4) + G_A(3,l)*J_Q(2)
    Gout_V(4,l) = - G_A(2,l)*J_Q(3) + G_A(4,l)*J_Q(1)
    Gout_V(:,l) = Gout_V(:,l) + Gout_V(:,l)
  end do
end subroutine vert_loop_AQ_V


! *********************************************************************
subroutine vert_loop_VA_Q(rank_in, rank_out, G_V, J_A, Gout_A)
! bare VA -> A gluon-like interaction
! ---------------------------------------------------------------------
! G_V(sigma,l)   = incoming vector loop-current
! sigma          = open index of loop line (contravariant light-cone)
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_A(4)         = incoming antiquark current
! Gout_A(beta,l) = outgoing anti-quark loop current
! Gout_A(beta,l) = G_V(sigma,l) * Jbar_A(rho)*gamma_sigma(rho,beta)
! *********************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_V(4,rank_in), J_A(4)
  complex(qp), intent(out) :: Gout_A(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_A(1,l) = - J_A(3)*G_V(1,l) - J_A(4)*G_V(3,l)
    Gout_A(2,l) = - J_A(4)*G_V(2,l) - J_A(3)*G_V(4,l)
    Gout_A(3,l) = - J_A(1)*G_V(2,l) + J_A(2)*G_V(3,l)
    Gout_A(4,l) = - J_A(2)*G_V(1,l) + J_A(1)*G_V(4,l)
  end do
end subroutine vert_loop_VA_Q


! ************************************************************************
subroutine vert_loop_QV_A(rank_in, rank_out, G_Q, J_V, Gout_Q)
! bare QV -> Q gluon-like interaction
! ------------------------------------------------------------------------
! G_Q(sigma,l)   = incoming quark loop-current
! sigma          = open index of loop line
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_V(4)         = incoming vector current (contravariant light-cone rep.)
! Gout_Q(beta,l) = outgoing quark loop current
! Gout_Q(beta,l) = J_V(A)*gamma_A(beta, sigma)*G_Q(sigma,l)
! ************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_Q(4,rank_in), J_V(4)
  complex(qp), intent(out) :: Gout_Q(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_Q(1,l) = - J_V(2)*G_Q(3,l) + J_V(4)*G_Q(4,l)
    Gout_Q(2,l) = - J_V(1)*G_Q(4,l) + J_V(3)*G_Q(3,l)
    Gout_Q(3,l) = - J_V(1)*G_Q(1,l) - J_V(4)*G_Q(2,l)
    Gout_Q(4,l) = - J_V(2)*G_Q(2,l) - J_V(3)*G_Q(1,l)
  end do
end subroutine vert_loop_QV_A


! ************************************************************************
subroutine vert_loop_QA_V(rank_in, rank_out, G_Q, J_A, Gout_V)
! bare QA -> V gluon-like interaction
! ------------------------------------------------------------------------
! G_Q(sigma,l)   = incoming quark loop current
! sigma          = open index of loop line
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_A(4)         = incoming tree level anti-quark current
! Gout_V         = outgoing vector loop current (contravariant light-cone)
! Gout_V(beta,l) = Jbar_A(rho)*gamma^beta(rho,sigma) * G_Q(sigma, l)
! ************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_Q(4,rank_in), J_A(4)
  complex(qp), intent(out) :: Gout_V(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_V(1,l) = - G_Q(3,l)*J_A(1) - G_Q(2,l)*J_A(4)
    Gout_V(2,l) = - G_Q(4,l)*J_A(2) - G_Q(1,l)*J_A(3)
    Gout_V(3,l) = - G_Q(4,l)*J_A(1) + G_Q(2,l)*J_A(3)
    Gout_V(4,l) = - G_Q(3,l)*J_A(2) + G_Q(1,l)*J_A(4)
    Gout_V(:,l) = Gout_V(:,l) + Gout_V(:,l)
  end do
end subroutine vert_loop_QA_V


! *********************************************************************
subroutine vert_loop_VQ_A(rank_in, rank_out, G_V, J_Q, Gout_Q)
! bare VQ -> Q gluon-like interaction
! ---------------------------------------------------------------------
! G_V(sigma,l)   = incoming vector loop-current
! sigma          = open index of loop line (contravariant light-cone)
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_Q(4)         = incoming quark current
! Gout_Q(beta,l) = outgoing quark loop current
! Gout_Q(beta,l) = G_V(sigma,l) * gamma_sigma(beta,rho) * J_Q(rho)
! *********************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_V(4,rank_in), J_Q(4)
  complex(qp), intent(out) :: Gout_Q(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_Q(1,l) = - J_Q(3)*G_V(2,l) + J_Q(4)*G_V(4,l)
    Gout_Q(2,l) = - J_Q(4)*G_V(1,l) + J_Q(3)*G_V(3,l)
    Gout_Q(3,l) = - J_Q(1)*G_V(1,l) - J_Q(2)*G_V(4,l)
    Gout_Q(4,l) = - J_Q(2)*G_V(2,l) - J_Q(1)*G_V(3,l)
  end do
end subroutine vert_loop_VQ_A


!******************************************************************************
subroutine vert_loop_UV_W(rank_in, rank_out, Gin_V, Ploop, J_V, Ptree, Gout_V)
! bare VV -> V vertex
!------------------------------------------------------------------------------
! Gin_V(beta,l)  = incoming gluon loop current (light-cone repr)
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! beta           = open index of loop line (contravariant light-cone)
! J_V(4)         = incoming gluon current (contravariant light-cone repr)
! Pi(4)          = incoming gluon-i momentum   (contravariant light-cone repr)
! Gout_V         = outgoing gluon loop current (contravariant light-cone repr)
! Gout_V(beta,l) = Gin_V(sigma,l)*V(sigma,beta) +
!                  + sum_{i=1}^4 Gin_V(sigma,LR(i,l))*V(sigma,beta,i)
!******************************************************************************
  use kind_types, only: qp
  use ol_contractions_qp, only: cont_VV
  use ol_tensor_bookkeeping, only: HR
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: Gin_V(4,rank_in), J_V(4), Ploop(4), Ptree(4)
  complex(qp), intent(out) :: Gout_V(4,rank_out)
  complex(qp) :: Ac, Bc, C, Jhalf(4), Jtwo(4), Ptmp(4,3)
  integer           :: beta, l

  Gout_V = 0
  Ptmp(:,1) = Ploop + 2*Ptree
  Ptmp(:,2) = Ptree + 2*Ploop
  Ptmp(:,3) = Ploop - Ptree
  C = cont_VV(Ptmp(1,2),J_V)
  Jhalf = 0.5_qp * J_V
  ! covariant components of 2*J_V_mu = 2 * g_(mu,nu)*J_V^nu, factor 2 simplifies in metric tensor
  Jtwo(1) =  J_V(2)
  Jtwo(2) =  J_V(1)
  Jtwo(3) = -J_V(4)
  Jtwo(4) = -J_V(3)

  do l = 1, rank_in
    Ac = cont_VV(Gin_V(1,l), J_V)
    Bc = cont_VV(Gin_V(1,l), Ptmp(1,1))

    Gout_V(1, HR(1,l)) = Gout_V(1, HR(1,l)) + Ac + Gin_V(2,l)*Jhalf(1) - Gin_V(1,l)*Jtwo(1)
    Gout_V(2, HR(1,l)) = Gout_V(2, HR(1,l))      + Gin_V(2,l)*Jhalf(2) - Gin_V(2,l)*Jtwo(1)
    Gout_V(3, HR(1,l)) = Gout_V(3, HR(1,l))      + Gin_V(2,l)*Jhalf(3) - Gin_V(3,l)*Jtwo(1)
    Gout_V(4, HR(1,l)) = Gout_V(4, HR(1,l))      + Gin_V(2,l)*Jhalf(4) - Gin_V(4,l)*Jtwo(1)

    Gout_V(1, HR(2,l)) = Gout_V(1, HR(2,l))      + Gin_V(1,l)*Jhalf(1) - Gin_V(1,l)*Jtwo(2)
    Gout_V(2, HR(2,l)) = Gout_V(2, HR(2,l)) + Ac + Gin_V(1,l)*Jhalf(2) - Gin_V(2,l)*Jtwo(2)
    Gout_V(3, HR(2,l)) = Gout_V(3, HR(2,l))      + Gin_V(1,l)*Jhalf(3) - Gin_V(3,l)*Jtwo(2)
    Gout_V(4, HR(2,l)) = Gout_V(4, HR(2,l))      + Gin_V(1,l)*Jhalf(4) - Gin_V(4,l)*Jtwo(2)

    Gout_V(1, HR(3,l)) = Gout_V(1, HR(3,l))      - Gin_V(4,l)*Jhalf(1) - Gin_V(1,l)*Jtwo(3)
    Gout_V(2, HR(3,l)) = Gout_V(2, HR(3,l))      - Gin_V(4,l)*Jhalf(2) - Gin_V(2,l)*Jtwo(3)
    Gout_V(3, HR(3,l)) = Gout_V(3, HR(3,l)) + Ac - Gin_V(4,l)*Jhalf(3) - Gin_V(3,l)*Jtwo(3)
    Gout_V(4, HR(3,l)) = Gout_V(4, HR(3,l))      - Gin_V(4,l)*Jhalf(4) - Gin_V(4,l)*Jtwo(3)

    Gout_V(1, HR(4,l)) = Gout_V(1, HR(4,l))      - Gin_V(3,l)*Jhalf(1) - Gin_V(1,l)*Jtwo(4)
    Gout_V(2, HR(4,l)) = Gout_V(2, HR(4,l))      - Gin_V(3,l)*Jhalf(2) - Gin_V(2,l)*Jtwo(4)
    Gout_V(3, HR(4,l)) = Gout_V(3, HR(4,l))      - Gin_V(3,l)*Jhalf(3) - Gin_V(3,l)*Jtwo(4)
    Gout_V(4, HR(4,l)) = Gout_V(4, HR(4,l)) + Ac - Gin_V(3,l)*Jhalf(4) - Gin_V(4,l)*Jtwo(4)

    do beta = 1, 4
      Gout_V(beta,l) = Gout_V(beta,l) + Ac*Ptmp(beta,3) + Bc*J_V(beta) - C*Gin_V(beta,l)
    end do
  end do
end subroutine vert_loop_UV_W


!****************************************************************************************
subroutine vert_loop_VE_V(rank_in, rank_out, Gin_V, J1, J2, Gout_V)
! bare 4-gluon vertex when the sigma particle enters the loop as a tree wf
!----------------------------------------------------------------------------------------
! Gin_V(beta,l)  = incoming gluon loop current (contravariant light-cone repr)
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! beta           = open index of loop line (contravariant light-cone)
! Ji (4)         = incoming gluon current (contravariant light-cone repr)
! Gout_V         = outgoing gluon loop current (contravariant light-cone repr)
! Gout_V(beta,l) = Gin_V(sigma,l)*J1(sigma)*J2(beta) - Gin_V(sigma,l)*J2(sigma)*J1(beta)
!****************************************************************************************
  use kind_types, only: qp
  use ol_contractions_qp, only: cont_VV
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: Gin_V(4,rank_in), J1(4), J2(4)
  complex(qp), intent(out) :: Gout_V(4,rank_out)
  complex(qp) :: GJ1, GJ2
  integer           :: l, beta

  do l = 1, rank_in
    GJ1 = cont_VV(Gin_V(1,l),J1)
    GJ2 = cont_VV(Gin_V(1,l),J2)
    do beta = 1, 4
      Gout_V(beta,l) = GJ1*J2(beta) - GJ2*J1(beta)
    end do
  end do
end subroutine vert_loop_VE_V


!****************************************************************************************
subroutine vert_loop_GGG_G_23(rank_in, rank_out, Gin_V, J1, J2, Gout_V)
!****************************************************************************************
  use kind_types, only: qp
  use ol_contractions_qp, only: cont_VV
  implicit none
  integer,     intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: Gin_V(4,rank_in), J1(4), J2(4)
  complex(qp), intent(out) :: Gout_V(4,rank_out)
  complex(qp) :: GJ1, GJ2
  integer     :: l, beta

  do l = 1, rank_in
    GJ1 = cont_VV(Gin_V(1,l),J1)
    GJ2 = cont_VV(Gin_V(1,l),J2)
    do beta = 1, 4
      Gout_V(beta,l) = GJ1*J2(beta) - GJ2*J1(beta)
    end do
  end do
end subroutine vert_loop_GGG_G_23


!****************************************************************************************
subroutine vert_loop_EV_V(rank_in, rank_out, Gin_V, J1, J2, Gout_V)
! bare 4-gluon vertex when the sigma particle is in the loop
!----------------------------------------------------------------------------------------
! Gin_V(beta,l)  = incoming gluon loop current (contravariant light-cone repr)
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! beta           = open index of loop line (contravariant light-cone)
! Ji (4)         = incoming gluon current (contravariant light-cone repr)
! Gout_V         = outgoing gluon loop current (contravariant light-cone repr)
! Gout_V(beta,l) = Gin_V(sigma,l)*J1(sigma)*J2(beta) - J1(sigma)*J2(sigma)*Gin_V(beta,l)
!****************************************************************************************
  use kind_types, only: qp
  use ol_contractions_qp, only: cont_VV
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: Gin_V(4,rank_in), J1(4), J2(4)
  complex(qp), intent(out) :: Gout_V(4,rank_out)
  complex(qp) :: GJ2, J1J2
  integer           :: l

  J1J2 = cont_VV(J1,J2)
  do l = 1, rank_in
    GJ2 = cont_VV(Gin_V(1,l),J2)
    Gout_V(:,l) = GJ2*J1 - J1J2*Gin_V(:,l)
  end do
end subroutine vert_loop_EV_V


!****************************************************************************************
subroutine vert_loop_GGG_G_12(rank_in, rank_out, Gin_V, J1, J2, Gout_V)
!****************************************************************************************
  use kind_types, only: qp
  use ol_contractions_qp, only: cont_VV
  implicit none
  integer,           intent(in) :: rank_in, rank_out
  complex(qp), intent(in) :: Gin_V(4,rank_in), J1(4), J2(4)
  complex(qp), intent(out):: Gout_V(4,rank_out)
  complex(qp) :: GJ2, J1J2
  integer           :: l

  J1J2 = cont_VV(J1,J2)
  do l = 1, rank_in
    GJ2 = cont_VV(Gin_V(1,l),J2)
    Gout_V(:,l) = GJ2*J1 - J1J2*Gin_V(:,l)
  end do
end subroutine vert_loop_GGG_G_12


!*******************************************************************************************
subroutine vert_loop_CV_D(rank_in, rank_out, Gin_C, Ploop, J_V, Ptree, Gout_C)
! bare ghost gluon -> ghost interaction
! always comes in a closed ghost loop
!-------------------------------------------------------------------------------------------
! Gin       = scalar ghost loop current
! rank_i/o  = length of tensor array up to highest incoming/outgoing rank
! Pi        = incoming momenta (contravariant light-cone)
! J_V       = gluon tree level current
! Gout      = outgoing ghost loop current
! Gout(l)   = Gin(l)*J_V(mu)*(Ploop+Ptree)(mu) + sum_{i=1}^4 Gin(LR(i,l))*J_i (!! covariant)
!*******************************************************************************************
  use kind_types, only: qp
  use ol_contractions_qp, only: cont_VV
  use ol_tensor_bookkeeping, only: HR
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: Gin_C(rank_in), Ploop(4), J_V(4), Ptree(4)
  complex(qp), intent(out) :: Gout_C(rank_out)
  complex(qp) :: Ptot(4), JPtot
  integer           :: l

  Ptot = Ploop + Ptree
  JPtot = cont_VV(Ptot,J_V)
  Gout_C = 0

  do l = 1, rank_in
    Gout_C(HR(1,l)) = Gout_C(HR(1,l)) + 0.5_qp*Gin_C(l)*J_V(2) ! J_V covariant components
    Gout_C(HR(2,l)) = Gout_C(HR(2,l)) + 0.5_qp*Gin_C(l)*J_V(1) ! J_V covariant components
    Gout_C(HR(3,l)) = Gout_C(HR(3,l)) - 0.5_qp*Gin_C(l)*J_V(4) ! J_V covariant components
    Gout_C(HR(4,l)) = Gout_C(HR(4,l)) - 0.5_qp*Gin_C(l)*J_V(3) ! J_V covariant components

    Gout_C(l) = Gout_C(l) + Gin_C(l)*JPtot
  end do
end subroutine vert_loop_CV_D


!*****************************************************************************************
subroutine vert_loop_DV_C(rank_in, rank_out, Gin_D, Ploop, J_V, Gout_D)
! bare anti-ghost gluon -> anti-ghost interaction
! always comes in a closed ghost loop
!-----------------------------------------------------------------------------------------
! Gin       = scalar anti-ghost loop current
! rank_i/o  = length of tensor array up to highest incoming/outgoing rank
! Ploop     = incoming momentum of Gin_D (contravariant light-cone)
! J_V       = gluon tree level current
! Gout      = outgoing anti-ghost loop current
! Gout(l)   = Gin(l)*J_V(mu)*(-Ploop)(mu) + sum_{i=1}^4 Gin(LR(i,l))*(-J_i) (!! covariant)
!*****************************************************************************************
  use kind_types, only: qp
  use ol_contractions_qp, only: cont_VV
  use ol_tensor_bookkeeping, only: HR
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: Gin_D(rank_in), Ploop(4), J_V(4)
  complex(qp), intent(out) :: Gout_D(rank_out)
  complex(qp) :: JPloop
  integer           :: l

  JPloop = - cont_VV(Ploop,J_V)
  Gout_D = 0

  do l = 1, rank_in
    Gout_D(HR(1,l)) = Gout_D(HR(1,l)) - 0.5_qp*Gin_D(l)*J_V(2) ! -J_V covariant components
    Gout_D(HR(2,l)) = Gout_D(HR(2,l)) - 0.5_qp*Gin_D(l)*J_V(1) ! -J_V covariant components
    Gout_D(HR(3,l)) = Gout_D(HR(3,l)) + 0.5_qp*Gin_D(l)*J_V(4) ! -J_V covariant components
    Gout_D(HR(4,l)) = Gout_D(HR(4,l)) + 0.5_qp*Gin_D(l)*J_V(3) ! -J_V covariant components

    Gout_D(l) = Gout_D(l) + Gin_D(l)*JPloop
  end do
end subroutine vert_loop_DV_C


!*****************************************************************************************
subroutine vert_loop_QS_A(rank_in, rank_out, g_RL, G_Q, J_S, Gout_Q)
! bare quark scalar -> anti-quark interaction
! copy of the tree level routine
!-----------------------------------------------------------------------------------------
! G_Q(sigma,l)   = incoming quark loop-current
! sigma          = open index of loop line
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_S            = incoming scalar current
! g_RL(1)        = right-handed coupling gR
! g_RL(2)        = left-handed coupling gL
! Gout_Q(beta,l) = outgoing quark loop current
! ***************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_Q(4,rank_in), J_S, g_RL(2)
  complex(qp), intent(out) :: Gout_Q(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_Q(1,l) = g_RL(1) * G_Q(1,l)*J_S
    Gout_Q(2,l) = g_RL(1) * G_Q(2,l)*J_S
    Gout_Q(3,l) = g_RL(2) * G_Q(3,l)*J_S
    Gout_Q(4,l) = g_RL(2) * G_Q(4,l)*J_S
  end do
end subroutine vert_loop_QS_A


!*****************************************************************************************
subroutine vert_loop_SQ_A(rank_in, rank_out, g_RL, G_S, J_Q, Gout_Q)
! bare scalar quark -> anti-quark interaction
!-----------------------------------------------------------------------------------------
! G_S(l)         = incoming scalar loop-current
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_Q            = incoming quark current
! g_RL(1)        = right-handed coupling gR
! g_RL(2)        = left-handed coupling gL
! Gout_Q(beta,l) = outgoing quark loop current
! ***************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_S(rank_in), J_Q(4), g_RL(2)
  complex(qp), intent(out) :: Gout_Q(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_Q(1,l) = g_RL(1) * G_S(l)*J_Q(1)
    Gout_Q(2,l) = g_RL(1) * G_S(l)*J_Q(2)
    Gout_Q(3,l) = g_RL(2) * G_S(l)*J_Q(3)
    Gout_Q(4,l) = g_RL(2) * G_S(l)*J_Q(4)
  end do
end subroutine vert_loop_SQ_A


!*****************************************************************************************
subroutine vert_loop_AS_Q(rank_in, rank_out, g_RL, G_A, J_S, Gout_A)
! bare anti-quark scalar -> quark interaction
! copy of the tree level routine
!-----------------------------------------------------------------------------------------
! G_A(sigma,l)   = incoming anti-quark loop-current
! sigma          = open index of loop line
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_S            = incoming scalar current
! g_RL(1)        = right-handed coupling gR
! g_RL(2)        = left-handed coupling gL
! Gout_A(beta,l) = outgoing anti-quark loop current
! ***************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_A(4,rank_in), J_S, g_RL(2)
  complex(qp), intent(out) :: Gout_A(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_A(1,l) = g_RL(1) * G_A(1,l)*J_S
    Gout_A(2,l) = g_RL(1) * G_A(2,l)*J_S
    Gout_A(3,l) = g_RL(2) * G_A(3,l)*J_S
    Gout_A(4,l) = g_RL(2) * G_A(4,l)*J_S
  end do
end subroutine vert_loop_AS_Q


!*****************************************************************************************
subroutine vert_loop_SA_Q(rank_in, rank_out, g_RL, G_S, J_A, Gout_A)
! bare scalar anti-quark -> quark interaction
!-----------------------------------------------------------------------------------------
! G_S(l)         = incoming scalar loop-current
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_A            = incoming anti-quark current
! g_RL(1)        = right-handed coupling gR
! g_RL(2)        = left-handed coupling gL
! Gout_A(beta,l) = outgoing anti-quark loop current
! ***************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_S(rank_in), J_A(4), g_RL(2)
  complex(qp), intent(out) :: Gout_A(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_A(1,l) = g_RL(1) * G_S(l)* J_A(1)
    Gout_A(2,l) = g_RL(1) * G_S(l)* J_A(2)
    Gout_A(3,l) = g_RL(2) * G_S(l)* J_A(3)
    Gout_A(4,l) = g_RL(2) * G_S(l)* J_A(4)
  end do
end subroutine vert_loop_SA_Q


!*****************************************************************************************
subroutine vert_loop_QA_S(rank_in, rank_out, g_RL, G_Q, J_A, Gout_S)
! bare quark anti-quark -> scalar interaction
!-----------------------------------------------------------------------------------------
! G_Q(sigma,l)   = incoming quark loop-current
! sigma          = open index of loop line
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_A            = incoming anti-quark current
! g_RL(1)        = right-handed coupling gR
! g_RL(2)        = left-handed coupling gL
! Gout_S(l)      = outgoing scalar loop current
! ***************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_Q(4,rank_in), J_A(4), g_RL(2)
  complex(qp), intent(out) :: Gout_S(rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_S(l) = g_RL(1) * (G_Q(1,l)*J_A(1) + G_Q(2,l)*J_A(2)) + g_RL(2) * (G_Q(3,l)*J_A(3) + G_Q(4,l)*J_A(4))
  end do
end subroutine vert_loop_QA_S


!*****************************************************************************************
subroutine vert_loop_AQ_S(rank_in, rank_out, g_RL, G_A, J_Q, Gout_S)
! bare anti-quark quark -> scalar interaction
!-----------------------------------------------------------------------------------------
! G_A(sigma,l)   = incoming anti-quark loop-current
! sigma          = open index of loop line
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_Q            = incoming quark current
! g_RL(1)        = right-handed coupling gR
! g_RL(2)        = left-handed coupling gL
! Gout_S(l)      = outgoing scalar loop current
! ***************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_A(4,rank_in), J_Q(4), g_RL(2)
  complex(qp), intent(out) :: Gout_S(rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_S(l) = g_RL(1) * (G_A(1,l)*J_Q(1) + G_A(2,l)*J_Q(2)) + g_RL(2) * (G_A(3,l)*J_Q(3) + G_A(4,l)*J_Q(4))
  end do
end subroutine vert_loop_AQ_S


!*****************************************************************************************
subroutine vert_loop_VV_S(rank_in, rank_out, G_V, J_V, Gout_S)
! bare vector vector -> scalar interaction
!-----------------------------------------------------------------------------------------
! G_V(sigma,l)   = incoming vector loop-current
! sigma          = open index of loop line
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_V            = incoming vector current
! Gout_S(l)      = outgoing scalar loop current
! ***************************************************************************
  use kind_types, only: qp
  use ol_contractions_qp, only: cont_VV
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_V(4,rank_in), J_V(4)
  complex(qp), intent(out) :: Gout_S(rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_S(l) = cont_VV(G_V(:,l),J_V)
  end do
end subroutine vert_loop_VV_S


!*****************************************************************************************
subroutine vert_loop_VS_V(rank_in, rank_out, G_V, J_S, Gout_V)
! bare vector scalar -> vector interaction
!-----------------------------------------------------------------------------------------
! G_V(sigma,l)   = incoming vector loop-current
! sigma          = open index of loop line
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_S            = incoming scalar current
! Gout_V(beta,l) = outgoing vector loop current
! ***************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_V(4,rank_in), J_S
  complex(qp), intent(out) :: Gout_V(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_V(:,l) = G_V(:,l)*J_S
  end do
end subroutine vert_loop_VS_V


!*****************************************************************************************
subroutine vert_loop_SV_V(rank_in, rank_out, G_S, J_V, Gout_V)
! bare scalar vector -> vector interaction
!-----------------------------------------------------------------------------------------
! G_S(l)         = incoming scalar loop-current
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_V            = incoming vector current
! Gout_V(beta,l) = outgoing vector loop current
! ***************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_S(rank_in), J_V(4)
  complex(qp), intent(out) :: Gout_V(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_V(:,l) = G_S(l)*J_V
  end do
end subroutine vert_loop_SV_V


!*****************************************************************************************
subroutine vert_loop_SV_T(rank_in, rank_out, Gin_S, Ploop, J_V, Ptree, Gout_S)
! bare scalar-vector -> scalar interaction
!-----------------------------------------------------------------------------------------
! Gin_S     = scalar loop current
! rank_i/o  = length of tensor array up to highest incoming/outgoing rank
! Pi        = incoming momenta (contravariant light-cone)
! J_V       = vector tree level current
! Gout_S    = outgoing scalar loop current
! Gout(l)   = Gin(l)*J_V(mu)*(2Ploop+Ptree)(mu) + sum_{i=1}^4 Gin(LR(i,l))*2(J_i) (covariant)
!*****************************************************************************************
  use kind_types, only: qp
  use ol_contractions_qp, only: cont_VV
  use ol_tensor_bookkeeping, only: HR
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: Gin_S(rank_in), Ploop(4), J_V(4), Ptree(4)
  complex(qp), intent(out) :: Gout_S(rank_out)
  complex(qp) :: JP
  integer           :: l

  JP = cont_VV(2*Ploop+Ptree,J_V)
  Gout_S = 0

  do l = 1, rank_in
    Gout_S(HR(1,l)) = Gout_S(HR(1,l)) + Gin_S(l)*J_V(2) ! 2*J_V covariant components (simplifies factor 1/2 in covariant components)
    Gout_S(HR(2,l)) = Gout_S(HR(2,l)) + Gin_S(l)*J_V(1) ! 2*J_V covariant components
    Gout_S(HR(3,l)) = Gout_S(HR(3,l)) - Gin_S(l)*J_V(4) ! 2*J_V covariant components
    Gout_S(HR(4,l)) = Gout_S(HR(4,l)) - Gin_S(l)*J_V(3) ! 2*J_V covariant components

    Gout_S(l) = Gout_S(l) + Gin_S(l)*JP
  end do
end subroutine vert_loop_SV_T


!*****************************************************************************************
subroutine vert_loop_TV_S(rank_in, rank_out, Gin_S, Ploop, J_V, Ptree, Gout_S)
! bare scalar-vector -> scalar interaction
!-----------------------------------------------------------------------------------------
! Gin_S     = scalar loop current
! rank_i/o  = length of tensor array up to highest incoming/outgoing rank
! Pi        = incoming momenta (contravariant light-cone)
! J_V       = vector tree level current
! Gout_S    = outgoing scalar loop current
! Gout(l)   = Gin(l)*J_V(mu)*(-2Ploop-Ptree)(mu) + sum_{i=1}^4 Gin(LR(i,l))*2(-J_i) (covariant)
!*****************************************************************************************
  use kind_types, only: qp
  use ol_contractions_qp, only: cont_VV
  use ol_tensor_bookkeeping, only: HR
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: Gin_S(rank_in), Ploop(4), J_V(4), Ptree(4)
  complex(qp), intent(out) :: Gout_S(rank_out)
  complex(qp)  :: JP
  integer            :: l

!   JP = cont_VV(2*Ploop+Ptree,J_V)
!   Gout_S = 0
! 
!   do l = 1, rank_in
!     Gout_S(HR(1,l)) = Gout_S(HR(1,l)) - Gin_S(l)*J_V(2) ! -2*J_V covariant components (simplifies factor 1/2 in covariant components)
!     Gout_S(HR(2,l)) = Gout_S(HR(2,l)) - Gin_S(l)*J_V(1) ! -2*J_V covariant components
!     Gout_S(HR(3,l)) = Gout_S(HR(3,l)) + Gin_S(l)*J_V(4) ! -2*J_V covariant components
!     Gout_S(HR(4,l)) = Gout_S(HR(4,l)) + Gin_S(l)*J_V(3) ! -2*J_V covariant components
! 
!     Gout_S(l) = Gout_S(l) - Gin_S(l)*JP
!   end do

  call vert_loop_SV_T(rank_in, rank_out, Gin_S, Ploop, J_V, Ptree, Gout_S)
  Gout_S = - Gout_S

end subroutine vert_loop_TV_S


!*****************************************************************************************
subroutine vert_loop_VS_T(rank_in, rank_out, Gin_V, Ploop, J_S, Ptree, Gout_S)
! bare vector-scalar -> scalar interaction
!-----------------------------------------------------------------------------------------
! Gin_V(sigma,l) = vector loop current
! sigma          = open index of loop line
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! Pi             = incoming momenta (contravariant light-cone)
! J_S            = scalar tree level current
! Gout_S         = outgoing scalar loop current
! Gout_S(l)      = Gin_V(sigma,l)*(Ploop+2Ptree)(sigma)*J_S + sum_{i=1}^4 Gin_V(sigma,LR(i,l))*g_(i,sigma)*J_S
!*****************************************************************************************
  use kind_types, only: qp
  use ol_contractions_qp, only: cont_VV
  use ol_tensor_bookkeeping, only: HR
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: Gin_V(4,rank_in), Ploop(4), J_S, Ptree(4)
  complex(qp), intent(out) :: Gout_S(rank_out)
  integer :: l

  Gout_S = 0

  do l = 1, rank_in
    Gout_S(HR(1,l)) = Gout_S(HR(1,l)) + 0.5_qp*Gin_V(2,l)*J_S
    Gout_S(HR(2,l)) = Gout_S(HR(2,l)) + 0.5_qp*Gin_V(1,l)*J_S
    Gout_S(HR(3,l)) = Gout_S(HR(3,l)) - 0.5_qp*Gin_V(4,l)*J_S
    Gout_S(HR(4,l)) = Gout_S(HR(4,l)) - 0.5_qp*Gin_V(3,l)*J_S

    Gout_S(l) = Gout_S(l) + cont_VV(Gin_V(:,l),Ploop+2*Ptree)*J_S
  end do
end subroutine vert_loop_VS_T


!*****************************************************************************************
subroutine vert_loop_VT_S(rank_in, rank_out, Gin_V, Ploop, J_S, Ptree, Gout_S)
! bare vector-scalar -> scalar interaction
!-----------------------------------------------------------------------------------------
! Gin_V(sigma,l) = vector loop current
! sigma          = open index of loop line
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! Pi             = incoming momenta (contravariant light-cone)
! J_S            = scalar tree level current
! Gout_S         = outgoing scalar loop current
! Gout_S(l)      = Gin_V(sigma,l)*(-Ploop-2Ptree)(sigma)*J_S + sum_{i=1}^4 Gin_V(sigma,LR(i,l))*(-g_(i,sigma))*J_S
!*****************************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: Gin_V(4,rank_in), Ploop(4), J_S, Ptree(4)
  complex(qp), intent(out) :: Gout_S(rank_out)

  call vert_loop_VS_T(rank_in, rank_out, Gin_V, Ploop, J_S, Ptree, Gout_S)
  Gout_S = - Gout_S

end subroutine vert_loop_VT_S


!*****************************************************************************************
subroutine vert_loop_ST_V(rank_in, rank_out, Gin_S, Ploop, J_S, Ptree, Gout_V)
! bare scalar-scalar -> vector interaction
!-----------------------------------------------------------------------------------------
! Gin_S(l)       = scalar loop current
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! Pi             = incoming momenta (contravariant light-cone)
! J_S            = scalar tree level current
! Gout_V(beta,l) = outgoing vector loop current
! Gout_V(beta,l) = Gin_S(l)*(Ploop-Ptree)^beta*J_S + sum_{i=1}^4 Gin_S(LR(i,l))*J_S*(g_i^beta) (kronecker delta)
!*****************************************************************************************
  use kind_types, only: qp
  use ol_tensor_bookkeeping, only: HR
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: Gin_S(rank_in), Ploop(4), J_S, Ptree(4)
  complex(qp), intent(out) :: Gout_V(4,rank_out)
  integer :: l

  Gout_V = 0

  do l = 1, rank_in
    Gout_V(1, HR(1,l)) = Gout_V(1, HR(1,l)) + Gin_S(l)*J_S
    Gout_V(2, HR(2,l)) = Gout_V(2, HR(2,l)) + Gin_S(l)*J_S
    Gout_V(3, HR(3,l)) = Gout_V(3, HR(3,l)) + Gin_S(l)*J_S
    Gout_V(4, HR(4,l)) = Gout_V(4, HR(4,l)) + Gin_S(l)*J_S

    Gout_V(:,l) = Gout_V(:,l) + Gin_S(l)*J_S*(Ploop-Ptree)
  end do
end subroutine vert_loop_ST_V


!*****************************************************************************************
subroutine vert_loop_TS_V(rank_in, rank_out, Gin_S, Ploop, J_S, Ptree, Gout_V)
! bare scalar-scalar -> vector interaction
!-----------------------------------------------------------------------------------------
! Gin_S(l)       = scalar loop current
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! Pi             = incoming momenta (contravariant light-cone)
! J_S            = scalar tree level current
! Gout_V(beta,l) = outgoing vector loop current
! Gout_V(beta,l) = Gin_S(l)*(-Ploop+Ptree)^beta*J_S + sum_{i=1}^4 Gin_S(LR(i,l))*J_S*(-g_i^beta) (kronecker delta)
!*****************************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: Gin_S(rank_in), Ploop(4), J_S, Ptree(4)
  complex(qp), intent(out) :: Gout_V(4,rank_out)

  call vert_loop_ST_V(rank_in, rank_out, Gin_S, Ploop, J_S, Ptree, Gout_V)
  Gout_V = - Gout_V
end subroutine vert_loop_TS_V


!*****************************************************************************************
subroutine vert_loop_SS_S(rank_in, rank_out, G_S, J_S, Gout_S)
! bare scalar scalar -> scalar interaction
!-----------------------------------------------------------------------------------------
! G_S(l)         = incoming scalar loop-current
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_S            = incoming scalar current
! Gout_S(l)      = outgoing scalar loop current
! ***************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_S(rank_in), J_S
  complex(qp), intent(out) :: Gout_S(rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_S(l) = G_S(l)*J_S
  end do
end subroutine vert_loop_SS_S


!*****************************************************************************************
subroutine vert_loop_SSS_S(rank_in, rank_out, G_S, J_S1, J_S2, Gout_S)
! bare scalar scalar scalar -> scalar interaction
!-----------------------------------------------------------------------------------------
! G_S(l)         = incoming scalar loop-current
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_S1,2         = incoming scalar currents
! Gout_S(l)      = outgoing scalar loop current
! ***************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_S(rank_in), J_S1, J_S2
  complex(qp), intent(out) :: Gout_S(rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_S(l) = G_S(l)*J_S1*J_S2
  end do
end subroutine vert_loop_SSS_S


!*****************************************************************************************
subroutine vert_loop_VVS_S(rank_in, rank_out, G_V, J_V, J_S, Gout_S)
! bare vector vector scalar -> scalar interaction
!-----------------------------------------------------------------------------------------
! G_V(sigma,l)   = incoming vector loop-current
! sigma          = open index of loop line
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_V            = incoming vector current
! J_S            = incoming scalar current
! Gout_S(l)      = outgoing scalar loop current
! ***************************************************************************
  use kind_types, only: qp
  use ol_contractions_qp, only: cont_VV
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_V(4,rank_in), J_V(4), J_S
  complex(qp), intent(out) :: Gout_S(rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_S(l) = cont_VV(G_V(:,l),J_V) * J_S
  end do
end subroutine vert_loop_VVS_S


!*****************************************************************************************
subroutine vert_loop_SSV_V(rank_in, rank_out, G_S, J_S, J_V, Gout_V)
! bare scalar scalar vector -> vector interaction
!-----------------------------------------------------------------------------------------
! G_S(l)         = incoming scalar loop-current
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_V            = incoming vector current
! J_S            = incoming scalar current
! Gout_V(beta,l) = outgoing vector loop current
! ***************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_S(rank_in), J_V(4), J_S
  complex(qp), intent(out) :: Gout_V(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_V(:,l) = G_S(l) * J_S * J_V
  end do
end subroutine vert_loop_SSV_V


!*****************************************************************************************
subroutine vert_loop_VSS_V(rank_in, rank_out, G_V, J_S1, J_S2, Gout_V)
! bare vector scalar scalar -> vector interaction
!-----------------------------------------------------------------------------------------
! G_V(sigma,l)   = incoming vector loop-current
! sigma          = open index of loop line
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_S1,2         = incoming scalar currents
! Gout_V(beta,l) = outgoing vector loop current
! ***************************************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_V(4,rank_in), J_S1, J_S2
  complex(qp), intent(out) :: Gout_V(4,rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_V(:,l) = J_S1 * J_S2 * G_V(:,l)
  end do
end subroutine vert_loop_VSS_V


!*****************************************************************************************
subroutine vert_loop_SVV_S(rank_in, rank_out, G_S, J_V1, J_V2, Gout_S)
! bare scalar vector vector -> scalar interaction
!-----------------------------------------------------------------------------------------
! G_S(l)         = incoming scalar loop-current
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_V1,2         = incoming vector currents
! Gout_S(l)      = outgoing scalar loop current
! ***************************************************************************
  use kind_types, only: qp
  use ol_contractions_qp, only: cont_VV
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_S(rank_in), J_V1(4), J_V2(4)
  complex(qp), intent(out) :: Gout_S(rank_out)
  integer :: l
  do l = 1, rank_in
    Gout_S(l) = G_S(l) * cont_VV(J_V1,J_V2)
  end do
end subroutine vert_loop_SVV_S


!*****************************************************************************************
subroutine vert_loop_WWV_V(rank_in, rank_out, G_V, J_V1, J_V2, Gout_V)
! bare vector vector vector -> vector interaction
!-----------------------------------------------------------------------------------------
! G_V(sigma,l)   = incoming vector loop-current
! sigma          = open index of loop line
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_V1,2         = incoming vector currents
! Gout_V(beta,l) = outgoing vector loop current
! ***************************************************************************
  use kind_types, only: qp
  use ol_contractions_qp, only: cont_VV
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_V(4,rank_in), J_V1(4), J_V2(4)
  complex(qp), intent(out) :: Gout_V(4,rank_out)
  complex(qp) :: GJ1(rank_in), GJ2(rank_in), J1J2
  integer           :: l

  J1J2 = cont_VV(J_V1, J_V2)
  do l = 1, rank_in
    GJ1(l) = cont_VV(G_V(:,l),J_V1)
    GJ1(l) = GJ1(l) + GJ1(l)
    GJ2(l) = cont_VV(G_V(:,l),J_V2)

    Gout_V(:,l) = GJ1(l) * J_V2 - J1J2 * G_V(:,l) - GJ2(l) * J_V1
  end do
end subroutine vert_loop_WWV_V


!*****************************************************************************************
subroutine vert_loop_VWW_V(rank_in, rank_out, G_V, J_V1, J_V2, Gout_V)
! bare vector vector vector -> vector interaction
!-----------------------------------------------------------------------------------------
! G_V(sigma,l)   = incoming vector loop-current
! sigma          = open index of loop line
! l              = array component of the symmetrized tensor G
! rank_i/o       = length of tensor array up to highest incoming/outgoing rank
! J_V1,2         = incoming vector currents
! Gout_V(beta,l) = outgoing vector loop current
! ***************************************************************************
  use kind_types, only: qp
  use ol_contractions_qp, only: cont_VV
  implicit none
  integer,           intent(in)  :: rank_in, rank_out
  complex(qp), intent(in)  :: G_V(4,rank_in), J_V1(4), J_V2(4)
  complex(qp), intent(out) :: Gout_V(4,rank_out)
  complex(qp) :: GJ1(rank_in), GJ2(rank_in), J1J2
  integer           :: l

  J1J2 = cont_VV(J_V1, J_V2)
  J1J2 = J1J2 + J1J2
  do l = 1, rank_in
    GJ1(l) = cont_VV(G_V(:,l),J_V1)
    GJ2(l) = cont_VV(G_V(:,l),J_V2)

    Gout_V(:,l) = J1J2*G_V(:,l) - GJ2(l) * J_V1 - GJ1(l) * J_V2
  end do
end subroutine vert_loop_VWW_V


end module ol_loop_vertices_qp



! *****************************************************************************
!                            ALPHA INDEX INTERFACE
! -----------------------------------------------------------------------------
! When is needed (i.e. vector and fermion currents) it calls the vertex routine
! four times for the four components of the cut-open loop line
! *****************************************************************************

module ol_vert_interface_qp
  use ol_loop_vertices_qp
  implicit none
  contains

! *****************************************
subroutine loop_AZ_Q(G_A, J_Z, Gout_A, g_RL)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: g_RL(2), G_A(:,:,:), J_Z(4)
  complex(qp), intent(out) :: Gout_A(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_A,2)
  rank_out = size(Gout_A,2)

  call vert_loop_AZ_Q(rank_in, rank_out, g_RL, G_A(:,:,1), J_Z, Gout_A(:,:,1))
  call vert_loop_AZ_Q(rank_in, rank_out, g_RL, G_A(:,:,2), J_Z, Gout_A(:,:,2))
  call vert_loop_AZ_Q(rank_in, rank_out, g_RL, G_A(:,:,3), J_Z, Gout_A(:,:,3))
  call vert_loop_AZ_Q(rank_in, rank_out, g_RL, G_A(:,:,4), J_Z, Gout_A(:,:,4))
end subroutine loop_AZ_Q


! *****************************************
subroutine loop_AQ_Z(G_A, J_Q, Gout_Z, g_RL)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: g_RL(2), G_A(:,:,:), J_Q(4)
  complex(qp), intent(out) :: Gout_Z(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_A,2)
  rank_out = size(Gout_Z,2)

  call vert_loop_AQ_Z(rank_in, rank_out, g_RL, G_A(:,:,1), J_Q, Gout_Z(:,:,1))
  call vert_loop_AQ_Z(rank_in, rank_out, g_RL, G_A(:,:,2), J_Q, Gout_Z(:,:,2))
  call vert_loop_AQ_Z(rank_in, rank_out, g_RL, G_A(:,:,3), J_Q, Gout_Z(:,:,3))
  call vert_loop_AQ_Z(rank_in, rank_out, g_RL, G_A(:,:,4), J_Q, Gout_Z(:,:,4))
end subroutine loop_AQ_Z


! *****************************************
subroutine loop_ZA_Q(G_Z, J_A, Gout_A, g_RL)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: g_RL(2), G_Z(:,:,:), J_A(4)
  complex(qp), intent(out) :: Gout_A(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_Z,2)
  rank_out = size(Gout_A,2)

  call vert_loop_ZA_Q(rank_in, rank_out, g_RL, G_Z(:,:,1), J_A, Gout_A(:,:,1))
  call vert_loop_ZA_Q(rank_in, rank_out, g_RL, G_Z(:,:,2), J_A, Gout_A(:,:,2))
  call vert_loop_ZA_Q(rank_in, rank_out, g_RL, G_Z(:,:,3), J_A, Gout_A(:,:,3))
  call vert_loop_ZA_Q(rank_in, rank_out, g_RL, G_Z(:,:,4), J_A, Gout_A(:,:,4))
end subroutine loop_ZA_Q


! *****************************************
subroutine loop_QZ_A(G_Q, J_Z, Gout_Q, g_RL)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: g_RL(2), G_Q(:,:,:), J_Z(4)
  complex(qp), intent(out) :: Gout_Q(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_Q,2)
  rank_out = size(Gout_Q,2)

  call vert_loop_QZ_A(rank_in, rank_out, g_RL, G_Q(:,:,1), J_Z, Gout_Q(:,:,1))
  call vert_loop_QZ_A(rank_in, rank_out, g_RL, G_Q(:,:,2), J_Z, Gout_Q(:,:,2))
  call vert_loop_QZ_A(rank_in, rank_out, g_RL, G_Q(:,:,3), J_Z, Gout_Q(:,:,3))
  call vert_loop_QZ_A(rank_in, rank_out, g_RL, G_Q(:,:,4), J_Z, Gout_Q(:,:,4))
end subroutine loop_QZ_A


! *****************************************
subroutine loop_QA_Z(G_Q, J_A, Gout_Z, g_RL)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: g_RL(2), G_Q(:,:,:), J_A(4)
  complex(qp), intent(out) :: Gout_Z(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_Q,2)
  rank_out = size(Gout_Z,2)

  call vert_loop_QA_Z(rank_in, rank_out, g_RL, G_Q(:,:,1), J_A, Gout_Z(:,:,1))
  call vert_loop_QA_Z(rank_in, rank_out, g_RL, G_Q(:,:,2), J_A, Gout_Z(:,:,2))
  call vert_loop_QA_Z(rank_in, rank_out, g_RL, G_Q(:,:,3), J_A, Gout_Z(:,:,3))
  call vert_loop_QA_Z(rank_in, rank_out, g_RL, G_Q(:,:,4), J_A, Gout_Z(:,:,4))
end subroutine loop_QA_Z


! *****************************************
subroutine loop_ZQ_A(G_Z, J_Q, Gout_Q, g_RL)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: g_RL(2), G_Z(:,:,:), J_Q(4)
  complex(qp), intent(out) :: Gout_Q(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_Z,2)
  rank_out = size(Gout_Q,2)

  call vert_loop_ZQ_A(rank_in, rank_out, g_RL, G_Z(:,:,1), J_Q, Gout_Q(:,:,1))
  call vert_loop_ZQ_A(rank_in, rank_out, g_RL, G_Z(:,:,2), J_Q, Gout_Q(:,:,2))
  call vert_loop_ZQ_A(rank_in, rank_out, g_RL, G_Z(:,:,3), J_Q, Gout_Q(:,:,3))
  call vert_loop_ZQ_A(rank_in, rank_out, g_RL, G_Z(:,:,4), J_Q, Gout_Q(:,:,4))
end subroutine loop_ZQ_A


! *****************************************
subroutine loop_AW_Q(G_A, J_W, Gout_A)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_A(:,:,:), J_W(4)
  complex(qp), intent(out) :: Gout_A(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_A,2)
  rank_out = size(Gout_A,2)

  call vert_loop_AW_Q(rank_in, rank_out, G_A(:,:,1), J_W, Gout_A(:,:,1))
  call vert_loop_AW_Q(rank_in, rank_out, G_A(:,:,2), J_W, Gout_A(:,:,2))
  call vert_loop_AW_Q(rank_in, rank_out, G_A(:,:,3), J_W, Gout_A(:,:,3))
  call vert_loop_AW_Q(rank_in, rank_out, G_A(:,:,4), J_W, Gout_A(:,:,4))
end subroutine loop_AW_Q


! *****************************************
subroutine loop_AQ_W(G_A, J_Q, Gout_W)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_A(:,:,:), J_Q(4)
  complex(qp), intent(out) :: Gout_W(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_A,2)
  rank_out = size(Gout_W,2)

  call vert_loop_AQ_W(rank_in, rank_out, G_A(:,:,1), J_Q, Gout_W(:,:,1))
  call vert_loop_AQ_W(rank_in, rank_out, G_A(:,:,2), J_Q, Gout_W(:,:,2))
  call vert_loop_AQ_W(rank_in, rank_out, G_A(:,:,3), J_Q, Gout_W(:,:,3))
  call vert_loop_AQ_W(rank_in, rank_out, G_A(:,:,4), J_Q, Gout_W(:,:,4))
end subroutine loop_AQ_W


! *****************************************
subroutine loop_WA_Q(G_W, J_A, Gout_A)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_W(:,:,:), J_A(4)
  complex(qp), intent(out) :: Gout_A(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_W,2)
  rank_out = size(Gout_A,2)

  call vert_loop_WA_Q(rank_in, rank_out, G_W(:,:,1), J_A, Gout_A(:,:,1))
  call vert_loop_WA_Q(rank_in, rank_out, G_W(:,:,2), J_A, Gout_A(:,:,2))
  call vert_loop_WA_Q(rank_in, rank_out, G_W(:,:,3), J_A, Gout_A(:,:,3))
  call vert_loop_WA_Q(rank_in, rank_out, G_W(:,:,4), J_A, Gout_A(:,:,4))
end subroutine loop_WA_Q


! *****************************************
subroutine loop_QW_A(G_Q, J_W, Gout_Q)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_Q(:,:,:), J_W(4)
  complex(qp), intent(out) :: Gout_Q(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_Q,2)
  rank_out = size(Gout_Q,2)

  call vert_loop_QW_A(rank_in, rank_out, G_Q(:,:,1), J_W, Gout_Q(:,:,1))
  call vert_loop_QW_A(rank_in, rank_out, G_Q(:,:,2), J_W, Gout_Q(:,:,2))
  call vert_loop_QW_A(rank_in, rank_out, G_Q(:,:,3), J_W, Gout_Q(:,:,3))
  call vert_loop_QW_A(rank_in, rank_out, G_Q(:,:,4), J_W, Gout_Q(:,:,4))
end subroutine loop_QW_A


! *****************************************
subroutine loop_QA_W(G_Q, J_A, Gout_W)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_Q(:,:,:), J_A(4)
  complex(qp), intent(out) :: Gout_W(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_Q,2)
  rank_out = size(Gout_W,2)

  call vert_loop_QA_W(rank_in, rank_out, G_Q(:,:,1), J_A, Gout_W(:,:,1))
  call vert_loop_QA_W(rank_in, rank_out, G_Q(:,:,2), J_A, Gout_W(:,:,2))
  call vert_loop_QA_W(rank_in, rank_out, G_Q(:,:,3), J_A, Gout_W(:,:,3))
  call vert_loop_QA_W(rank_in, rank_out, G_Q(:,:,4), J_A, Gout_W(:,:,4))
end subroutine loop_QA_W


! *****************************************
subroutine loop_WQ_A(G_W, J_Q, Gout_Q)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_W(:,:,:), J_Q(4)
  complex(qp), intent(out) :: Gout_Q(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_W,2)
  rank_out = size(Gout_Q,2)

  call vert_loop_WQ_A(rank_in, rank_out, G_W(:,:,1), J_Q, Gout_Q(:,:,1))
  call vert_loop_WQ_A(rank_in, rank_out, G_W(:,:,2), J_Q, Gout_Q(:,:,2))
  call vert_loop_WQ_A(rank_in, rank_out, G_W(:,:,3), J_Q, Gout_Q(:,:,3))
  call vert_loop_WQ_A(rank_in, rank_out, G_W(:,:,4), J_Q, Gout_Q(:,:,4))
end subroutine loop_WQ_A


! *****************************************
subroutine loop_AV_Q(G_A, J_V, Gout_A)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_A(:,:,:), J_V(4)
  complex(qp), intent(out) :: Gout_A(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_A,2)
  rank_out = size(Gout_A,2)

  call vert_loop_AV_Q(rank_in, rank_out, G_A(:,:,1), J_V, Gout_A(:,:,1))
  call vert_loop_AV_Q(rank_in, rank_out, G_A(:,:,2), J_V, Gout_A(:,:,2))
  call vert_loop_AV_Q(rank_in, rank_out, G_A(:,:,3), J_V, Gout_A(:,:,3))
  call vert_loop_AV_Q(rank_in, rank_out, G_A(:,:,4), J_V, Gout_A(:,:,4))
end subroutine loop_AV_Q


! *****************************************
subroutine loop_AQ_V(G_A, J_Q, Gout_V)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_A(:,:,:), J_Q(4)
  complex(qp), intent(out) :: Gout_V(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_A,2)
  rank_out = size(Gout_V,2)

  call vert_loop_AQ_V(rank_in, rank_out, G_A(:,:,1), J_Q, Gout_V(:,:,1))
  call vert_loop_AQ_V(rank_in, rank_out, G_A(:,:,2), J_Q, Gout_V(:,:,2))
  call vert_loop_AQ_V(rank_in, rank_out, G_A(:,:,3), J_Q, Gout_V(:,:,3))
  call vert_loop_AQ_V(rank_in, rank_out, G_A(:,:,4), J_Q, Gout_V(:,:,4))
end subroutine loop_AQ_V


! *****************************************
subroutine loop_VA_Q(G_V, J_A, Gout_A)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_V(:,:,:), J_A(4)
  complex(qp), intent(out) :: Gout_A(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_V,2)
  rank_out = size(Gout_A,2)

  call vert_loop_VA_Q(rank_in, rank_out, G_V(:,:,1), J_A, Gout_A(:,:,1))
  call vert_loop_VA_Q(rank_in, rank_out, G_V(:,:,2), J_A, Gout_A(:,:,2))
  call vert_loop_VA_Q(rank_in, rank_out, G_V(:,:,3), J_A, Gout_A(:,:,3))
  call vert_loop_VA_Q(rank_in, rank_out, G_V(:,:,4), J_A, Gout_A(:,:,4))
end subroutine loop_VA_Q


! *****************************************
subroutine loop_QV_A(G_Q, J_V, Gout_Q)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_Q(:,:,:), J_V(4)
  complex(qp), intent(out) :: Gout_Q(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_Q,2)
  rank_out = size(Gout_Q,2)

  call vert_loop_QV_A(rank_in, rank_out, G_Q(:,:,1), J_V, Gout_Q(:,:,1))
  call vert_loop_QV_A(rank_in, rank_out, G_Q(:,:,2), J_V, Gout_Q(:,:,2))
  call vert_loop_QV_A(rank_in, rank_out, G_Q(:,:,3), J_V, Gout_Q(:,:,3))
  call vert_loop_QV_A(rank_in, rank_out, G_Q(:,:,4), J_V, Gout_Q(:,:,4))
end subroutine loop_QV_A


! *****************************************
subroutine loop_QA_V(G_Q, J_A, Gout_V)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_Q(:,:,:), J_A(4)
  complex(qp), intent(out) :: Gout_V(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_Q,2)
  rank_out = size(Gout_V,2)

  call vert_loop_QA_V(rank_in, rank_out, G_Q(:,:,1), J_A, Gout_V(:,:,1))
  call vert_loop_QA_V(rank_in, rank_out, G_Q(:,:,2), J_A, Gout_V(:,:,2))
  call vert_loop_QA_V(rank_in, rank_out, G_Q(:,:,3), J_A, Gout_V(:,:,3))
  call vert_loop_QA_V(rank_in, rank_out, G_Q(:,:,4), J_A, Gout_V(:,:,4))
end subroutine loop_QA_V


! *****************************************
subroutine loop_VQ_A(G_V, J_Q, Gout_Q)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_V(:,:,:), J_Q(4)
  complex(qp), intent(out) :: Gout_Q(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_V,2)
  rank_out = size(Gout_Q,2)

  call vert_loop_VQ_A(rank_in, rank_out, G_V(:,:,1), J_Q, Gout_Q(:,:,1))
  call vert_loop_VQ_A(rank_in, rank_out, G_V(:,:,2), J_Q, Gout_Q(:,:,2))
  call vert_loop_VQ_A(rank_in, rank_out, G_V(:,:,3), J_Q, Gout_Q(:,:,3))
  call vert_loop_VQ_A(rank_in, rank_out, G_V(:,:,4), J_Q, Gout_Q(:,:,4))
end subroutine loop_VQ_A


! *****************************************
subroutine loop_UV_W(Gin_V, Ploop, J_V, Ptree, Gout_V)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: Gin_V(:,:,:), J_V(4), Ploop(4), Ptree(4)
  complex(qp), intent(out) :: Gout_V(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(Gin_V,2)
  rank_out = size(Gout_V,2)

  call vert_loop_UV_W(rank_in, rank_out, Gin_V(:,:,1), Ploop, J_V, Ptree, Gout_V(:,:,1))
  call vert_loop_UV_W(rank_in, rank_out, Gin_V(:,:,2), Ploop, J_V, Ptree, Gout_V(:,:,2))
  call vert_loop_UV_W(rank_in, rank_out, Gin_V(:,:,3), Ploop, J_V, Ptree, Gout_V(:,:,3))
  call vert_loop_UV_W(rank_in, rank_out, Gin_V(:,:,4), Ploop, J_V, Ptree, Gout_V(:,:,4))
end subroutine loop_UV_W


! *****************************************
subroutine loop_UW_V(Gin_V, Ploop, J_V, Ptree, Gout_V)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: Gin_V(:,:,:), J_V(4), Ploop(4), Ptree(4)
  complex(qp), intent(out) :: Gout_V(:,:,:)
  call loop_UV_W(Gin_V, Ploop, J_V, Ptree, Gout_V)
  Gout_V = -Gout_V
end subroutine loop_UW_V


! *****************************************
subroutine loop_VE_V(Gin_V, J1, J2, Gout_V)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: Gin_V(:,:,:), J1(4), J2(4)
  complex(qp), intent(out) :: Gout_V(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(Gin_V,2)
  rank_out = size(Gout_V,2)

  call vert_loop_VE_V(rank_in, rank_out, Gin_V(:,:,1), J1, J2, Gout_V(:,:,1))
  call vert_loop_VE_V(rank_in, rank_out, Gin_V(:,:,2), J1, J2, Gout_V(:,:,2))
  call vert_loop_VE_V(rank_in, rank_out, Gin_V(:,:,3), J1, J2, Gout_V(:,:,3))
  call vert_loop_VE_V(rank_in, rank_out, Gin_V(:,:,4), J1, J2, Gout_V(:,:,4))
end subroutine loop_VE_V


! *****************************************
subroutine loop_GGG_G_23(Gin_V, J1, J2, Gout_V)
! the same as VE_V
! *****************************************
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: Gin_V(:,:,:), J1(4), J2(4)
  complex(qp), intent(out) :: Gout_V(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(Gin_V,2)
  rank_out = size(Gout_V,2)

  call vert_loop_GGG_G_23(rank_in, rank_out, Gin_V(:,:,1), J1, J2, Gout_V(:,:,1))
  call vert_loop_GGG_G_23(rank_in, rank_out, Gin_V(:,:,2), J1, J2, Gout_V(:,:,2))
  call vert_loop_GGG_G_23(rank_in, rank_out, Gin_V(:,:,3), J1, J2, Gout_V(:,:,3))
  call vert_loop_GGG_G_23(rank_in, rank_out, Gin_V(:,:,4), J1, J2, Gout_V(:,:,4))
end subroutine loop_GGG_G_23


! *****************************************
subroutine loop_EV_V(Gin_V, J1, J2, Gout_V)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: Gin_V(:,:,:), J1(4), J2(4)
  complex(qp), intent(out) :: Gout_V(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(Gin_V,2)
  rank_out = size(Gout_V,2)

  call vert_loop_EV_V(rank_in, rank_out, Gin_V(:,:,1), J1, J2, Gout_V(:,:,1))
  call vert_loop_EV_V(rank_in, rank_out, Gin_V(:,:,2), J1, J2, Gout_V(:,:,2))
  call vert_loop_EV_V(rank_in, rank_out, Gin_V(:,:,3), J1, J2, Gout_V(:,:,3))
  call vert_loop_EV_V(rank_in, rank_out, Gin_V(:,:,4), J1, J2, Gout_V(:,:,4))
end subroutine loop_EV_V


! *****************************************
subroutine loop_GGG_G_12(Gin_V, J1, J2, Gout_V)
! the same as EV_V
! *****************************************
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: Gin_V(:,:,:), J1(4), J2(4)
  complex(qp), intent(out) :: Gout_V(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(Gin_V,2)
  rank_out = size(Gout_V,2)

  call vert_loop_GGG_G_12(rank_in, rank_out, Gin_V(:,:,1), J1, J2, Gout_V(:,:,1))
  call vert_loop_GGG_G_12(rank_in, rank_out, Gin_V(:,:,2), J1, J2, Gout_V(:,:,2))
  call vert_loop_GGG_G_12(rank_in, rank_out, Gin_V(:,:,3), J1, J2, Gout_V(:,:,3))
  call vert_loop_GGG_G_12(rank_in, rank_out, Gin_V(:,:,4), J1, J2, Gout_V(:,:,4))
end subroutine loop_GGG_G_12


! *****************************************
subroutine loop_CV_D(Gin_C, Ploop, J_V, Ptree, Gout_C)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: Gin_C(:,:,:), J_V(4), Ploop(4), Ptree(4)
  complex(qp), intent(out) :: Gout_C(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(Gin_C,2)
  rank_out = size(Gout_C,2)

  call vert_loop_CV_D(rank_in,rank_out,Gin_C(1,:,1),Ploop,J_V,Ptree,Gout_C(1,:,1))
end subroutine loop_CV_D


! *****************************************
subroutine loop_DV_C(Gin_D, Ploop, J_V,Gout_D)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: Gin_D(:,:,:), J_V(4), Ploop(4)
  complex(qp), intent(out) :: Gout_D(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(Gin_D,2)
  rank_out = size(Gout_D,2)

  call vert_loop_DV_C(rank_in,rank_out,Gin_D(1,:,1),Ploop,J_V,Gout_D(1,:,1))
end subroutine loop_DV_C


! *****************************************
subroutine loop_AS_Q(G_A, J_S, Gout_A, g_RL)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: g_RL(2), G_A(:,:,:), J_S(4)
  complex(qp), intent(out) :: Gout_A(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_A,2)
  rank_out = size(Gout_A,2)

  call vert_loop_AS_Q(rank_in, rank_out, g_RL, G_A(:,:,1), J_S(1), Gout_A(:,:,1))
  call vert_loop_AS_Q(rank_in, rank_out, g_RL, G_A(:,:,2), J_S(1), Gout_A(:,:,2))
  call vert_loop_AS_Q(rank_in, rank_out, g_RL, G_A(:,:,3), J_S(1), Gout_A(:,:,3))
  call vert_loop_AS_Q(rank_in, rank_out, g_RL, G_A(:,:,4), J_S(1), Gout_A(:,:,4))
end subroutine loop_AS_Q


! *****************************************
subroutine loop_SA_Q(G_S, J_A, Gout_A, g_RL)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: g_RL(2), G_S(:,:,:), J_A(4)
  complex(qp), intent(out) :: Gout_A(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_S,2)
  rank_out = size(Gout_A,2)

  call vert_loop_SA_Q(rank_in, rank_out, g_RL, G_S(1,:,1), J_A, Gout_A(:,:,1))
  call vert_loop_SA_Q(rank_in, rank_out, g_RL, G_S(1,:,2), J_A, Gout_A(:,:,2))
  call vert_loop_SA_Q(rank_in, rank_out, g_RL, G_S(1,:,3), J_A, Gout_A(:,:,3))
  call vert_loop_SA_Q(rank_in, rank_out, g_RL, G_S(1,:,4), J_A, Gout_A(:,:,4))
end subroutine loop_SA_Q


! *****************************************
subroutine loop_QS_A(G_Q, J_S, Gout_Q, g_RL)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: g_RL(2), G_Q(:,:,:), J_S(4)
  complex(qp), intent(out) :: Gout_Q(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_Q,2)
  rank_out = size(Gout_Q,2)

  call vert_loop_QS_A(rank_in, rank_out, g_RL, G_Q(:,:,1), J_S(1), Gout_Q(:,:,1))
  call vert_loop_QS_A(rank_in, rank_out, g_RL, G_Q(:,:,2), J_S(1), Gout_Q(:,:,2))
  call vert_loop_QS_A(rank_in, rank_out, g_RL, G_Q(:,:,3), J_S(1), Gout_Q(:,:,3))
  call vert_loop_QS_A(rank_in, rank_out, g_RL, G_Q(:,:,4), J_S(1), Gout_Q(:,:,4))
end subroutine loop_QS_A


! *****************************************
subroutine loop_SQ_A(G_S, J_Q, Gout_Q, g_RL)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: g_RL(2), G_S(:,:,:), J_Q(4)
  complex(qp), intent(out) :: Gout_Q(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_S,2)
  rank_out = size(Gout_Q,2)

  call vert_loop_SQ_A(rank_in, rank_out, g_RL, G_S(1,:,1), J_Q, Gout_Q(:,:,1))
  call vert_loop_SQ_A(rank_in, rank_out, g_RL, G_S(1,:,2), J_Q, Gout_Q(:,:,2))
  call vert_loop_SQ_A(rank_in, rank_out, g_RL, G_S(1,:,3), J_Q, Gout_Q(:,:,3))
  call vert_loop_SQ_A(rank_in, rank_out, g_RL, G_S(1,:,4), J_Q, Gout_Q(:,:,4))
end subroutine loop_SQ_A


! *****************************************
subroutine loop_QA_S(G_Q, J_A, Gout_S, g_RL)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: g_RL(2), G_Q(:,:,:), J_A(4)
  complex(qp), intent(out) :: Gout_S(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_Q,2)
  rank_out = size(Gout_S,2)

  call vert_loop_QA_S(rank_in, rank_out, g_RL, G_Q(:,:,1), J_A, Gout_S(1,:,1))
  call vert_loop_QA_S(rank_in, rank_out, g_RL, G_Q(:,:,2), J_A, Gout_S(1,:,2))
  call vert_loop_QA_S(rank_in, rank_out, g_RL, G_Q(:,:,3), J_A, Gout_S(1,:,3))
  call vert_loop_QA_S(rank_in, rank_out, g_RL, G_Q(:,:,4), J_A, Gout_S(1,:,4))
end subroutine loop_QA_S


! *****************************************
subroutine loop_AQ_S(G_A, J_Q, Gout_S, g_RL)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: g_RL(2), G_A(:,:,:), J_Q(4)
  complex(qp), intent(out) :: Gout_S(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_A,2)
  rank_out = size(Gout_S,2)

  call vert_loop_AQ_S(rank_in, rank_out, g_RL, G_A(:,:,1), J_Q, Gout_S(1,:,1))
  call vert_loop_AQ_S(rank_in, rank_out, g_RL, G_A(:,:,2), J_Q, Gout_S(1,:,2))
  call vert_loop_AQ_S(rank_in, rank_out, g_RL, G_A(:,:,3), J_Q, Gout_S(1,:,3))
  call vert_loop_AQ_S(rank_in, rank_out, g_RL, G_A(:,:,4), J_Q, Gout_S(1,:,4))
end subroutine loop_AQ_S


! *****************************************
subroutine loop_VV_S(G_V, J_V, Gout_S)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_V(:,:,:), J_V(4)
  complex(qp), intent(out) :: Gout_S(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_V,2)
  rank_out = size(Gout_S,2)

  call vert_loop_VV_S(rank_in, rank_out, G_V(:,:,1), J_V, Gout_S(1,:,1))
  call vert_loop_VV_S(rank_in, rank_out, G_V(:,:,2), J_V, Gout_S(1,:,2))
  call vert_loop_VV_S(rank_in, rank_out, G_V(:,:,3), J_V, Gout_S(1,:,3))
  call vert_loop_VV_S(rank_in, rank_out, G_V(:,:,4), J_V, Gout_S(1,:,4))
end subroutine loop_VV_S


! *****************************************
subroutine loop_VS_V(G_V, J_S, Gout_V)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_V(:,:,:), J_S(4)
  complex(qp), intent(out) :: Gout_V(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_V,2)
  rank_out = size(Gout_V,2)

  call vert_loop_VS_V(rank_in, rank_out, G_V(:,:,1), J_S(1), Gout_V(:,:,1))
  call vert_loop_VS_V(rank_in, rank_out, G_V(:,:,2), J_S(1), Gout_V(:,:,2))
  call vert_loop_VS_V(rank_in, rank_out, G_V(:,:,3), J_S(1), Gout_V(:,:,3))
  call vert_loop_VS_V(rank_in, rank_out, G_V(:,:,4), J_S(1), Gout_V(:,:,4))
end subroutine loop_VS_V


! *****************************************
subroutine loop_SV_V(G_S, J_V, Gout_V)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_S(:,:,:), J_V(4)
  complex(qp), intent(out) :: Gout_V(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_S,2)
  rank_out = size(Gout_V,2)

  call vert_loop_SV_V(rank_in, rank_out, G_S(1,:,1), J_V, Gout_V(:,:,1))
  call vert_loop_SV_V(rank_in, rank_out, G_S(1,:,2), J_V, Gout_V(:,:,2))
  call vert_loop_SV_V(rank_in, rank_out, G_S(1,:,3), J_V, Gout_V(:,:,3))
  call vert_loop_SV_V(rank_in, rank_out, G_S(1,:,4), J_V, Gout_V(:,:,4))
end subroutine loop_SV_V


! *****************************************
subroutine loop_SV_T(G_S, Ploop, J_V, Ptree, Gout_S)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_S(:,:,:), Ploop(4), J_V(4), Ptree(4)
  complex(qp), intent(out) :: Gout_S(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_S,2)
  rank_out = size(Gout_S,2)

  call vert_loop_SV_T(rank_in, rank_out, G_S(1,:,1), Ploop, J_V, Ptree, Gout_S(1,:,1))
  call vert_loop_SV_T(rank_in, rank_out, G_S(1,:,2), Ploop, J_V, Ptree, Gout_S(1,:,2))
  call vert_loop_SV_T(rank_in, rank_out, G_S(1,:,3), Ploop, J_V, Ptree, Gout_S(1,:,3))
  call vert_loop_SV_T(rank_in, rank_out, G_S(1,:,4), Ploop, J_V, Ptree, Gout_S(1,:,4))
end subroutine loop_SV_T


! *****************************************
subroutine loop_TV_S(G_S, Ploop, J_V, Ptree, Gout_S)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_S(:,:,:), Ploop(4), J_V(4), Ptree(4)
  complex(qp), intent(out) :: Gout_S(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_S,2)
  rank_out = size(Gout_S,2)

  call vert_loop_TV_S(rank_in, rank_out, G_S(1,:,1), Ploop, J_V, Ptree, Gout_S(1,:,1))
  call vert_loop_TV_S(rank_in, rank_out, G_S(1,:,2), Ploop, J_V, Ptree, Gout_S(1,:,2))
  call vert_loop_TV_S(rank_in, rank_out, G_S(1,:,3), Ploop, J_V, Ptree, Gout_S(1,:,3))
  call vert_loop_TV_S(rank_in, rank_out, G_S(1,:,4), Ploop, J_V, Ptree, Gout_S(1,:,4))
end subroutine loop_TV_S


! *****************************************
subroutine loop_VS_T(G_V, Ploop, J_S, Ptree, Gout_S)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_V(:,:,:), Ploop(4), J_S(4), Ptree(4)
  complex(qp), intent(out) :: Gout_S(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_V,2)
  rank_out = size(Gout_S,2)

  call vert_loop_VS_T(rank_in, rank_out, G_V(:,:,1), Ploop, J_S(1), Ptree, Gout_S(1,:,1))
  call vert_loop_VS_T(rank_in, rank_out, G_V(:,:,2), Ploop, J_S(1), Ptree, Gout_S(1,:,2))
  call vert_loop_VS_T(rank_in, rank_out, G_V(:,:,3), Ploop, J_S(1), Ptree, Gout_S(1,:,3))
  call vert_loop_VS_T(rank_in, rank_out, G_V(:,:,4), Ploop, J_S(1), Ptree, Gout_S(1,:,4))
end subroutine loop_VS_T


! *****************************************
subroutine loop_VT_S(G_V, Ploop, J_S, Ptree, Gout_S)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_V(:,:,:), Ploop(4), J_S(4), Ptree(4)
  complex(qp), intent(out) :: Gout_S(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_V,2)
  rank_out = size(Gout_S,2)

  call vert_loop_VT_S(rank_in, rank_out, G_V(:,:,1), Ploop, J_S(1), Ptree, Gout_S(1,:,1))
  call vert_loop_VT_S(rank_in, rank_out, G_V(:,:,2), Ploop, J_S(1), Ptree, Gout_S(1,:,2))
  call vert_loop_VT_S(rank_in, rank_out, G_V(:,:,3), Ploop, J_S(1), Ptree, Gout_S(1,:,3))
  call vert_loop_VT_S(rank_in, rank_out, G_V(:,:,4), Ploop, J_S(1), Ptree, Gout_S(1,:,4))
end subroutine loop_VT_S


! *****************************************
subroutine loop_ST_V(G_S, Ploop, J_S, Ptree, Gout_V)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_S(:,:,:), Ploop(4), J_S(4), Ptree(4)
  complex(qp), intent(out) :: Gout_V(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_S,2)
  rank_out = size(Gout_V,2)

  call vert_loop_ST_V(rank_in, rank_out, G_S(1,:,1), Ploop, J_S(1), Ptree, Gout_V(:,:,1))
  call vert_loop_ST_V(rank_in, rank_out, G_S(1,:,2), Ploop, J_S(1), Ptree, Gout_V(:,:,2))
  call vert_loop_ST_V(rank_in, rank_out, G_S(1,:,3), Ploop, J_S(1), Ptree, Gout_V(:,:,3))
  call vert_loop_ST_V(rank_in, rank_out, G_S(1,:,4), Ploop, J_S(1), Ptree, Gout_V(:,:,4))
end subroutine loop_ST_V


! *****************************************
subroutine loop_TS_V(G_S, Ploop, J_S, Ptree, Gout_V)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_S(:,:,:), Ploop(4), J_S(4), Ptree(4)
  complex(qp), intent(out) :: Gout_V(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_S,2)
  rank_out = size(Gout_V,2)

  call vert_loop_TS_V(rank_in, rank_out, G_S(1,:,1), Ploop, J_S(1), Ptree, Gout_V(:,:,1))
  call vert_loop_TS_V(rank_in, rank_out, G_S(1,:,2), Ploop, J_S(1), Ptree, Gout_V(:,:,2))
  call vert_loop_TS_V(rank_in, rank_out, G_S(1,:,3), Ploop, J_S(1), Ptree, Gout_V(:,:,3))
  call vert_loop_TS_V(rank_in, rank_out, G_S(1,:,4), Ploop, J_S(1), Ptree, Gout_V(:,:,4))
end subroutine loop_TS_V


! *****************************************
subroutine loop_SS_S(G_S, J_S, Gout_S)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_S(:,:,:), J_S(4)
  complex(qp), intent(out) :: Gout_S(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_S,2)
  rank_out = size(Gout_S,2)

  call vert_loop_SS_S(rank_in, rank_out, G_S(1,:,1), J_S(1), Gout_S(1,:,1))
  call vert_loop_SS_S(rank_in, rank_out, G_S(1,:,2), J_S(1), Gout_S(1,:,2))
  call vert_loop_SS_S(rank_in, rank_out, G_S(1,:,3), J_S(1), Gout_S(1,:,3))
  call vert_loop_SS_S(rank_in, rank_out, G_S(1,:,4), J_S(1), Gout_S(1,:,4))
end subroutine loop_SS_S


! *****************************************
subroutine loop_SSS_S(G_S, J_S1, J_S2, Gout_S)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_S(:,:,:), J_S1(4), J_S2(4)
  complex(qp), intent(out) :: Gout_S(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_S,2)
  rank_out = size(Gout_S,2)

  call vert_loop_SSS_S(rank_in, rank_out, G_S(1,:,1), J_S1(1), J_S2(1), Gout_S(1,:,1))
  call vert_loop_SSS_S(rank_in, rank_out, G_S(1,:,2), J_S1(1), J_S2(1), Gout_S(1,:,2))
  call vert_loop_SSS_S(rank_in, rank_out, G_S(1,:,3), J_S1(1), J_S2(1), Gout_S(1,:,3))
  call vert_loop_SSS_S(rank_in, rank_out, G_S(1,:,4), J_S1(1), J_S2(1), Gout_S(1,:,4))
end subroutine loop_SSS_S


! *****************************************
subroutine loop_VVS_S(G_V, J_V, J_S, Gout_S)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_V(:,:,:), J_V(4), J_S(4)
  complex(qp), intent(out) :: Gout_S(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_V,2)
  rank_out = size(Gout_S,2)

  call vert_loop_VVS_S(rank_in, rank_out, G_V(:,:,1), J_V, J_S(1), Gout_S(1,:,1))
  call vert_loop_VVS_S(rank_in, rank_out, G_V(:,:,2), J_V, J_S(1), Gout_S(1,:,2))
  call vert_loop_VVS_S(rank_in, rank_out, G_V(:,:,3), J_V, J_S(1), Gout_S(1,:,3))
  call vert_loop_VVS_S(rank_in, rank_out, G_V(:,:,4), J_V, J_S(1), Gout_S(1,:,4))
end subroutine loop_VVS_S


! *****************************************
subroutine loop_SSV_V(G_S, J_S, J_V, Gout_V)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_S(:,:,:), J_S(4), J_V(4)
  complex(qp), intent(out) :: Gout_V(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_S,2)
  rank_out = size(Gout_V,2)

  call vert_loop_SSV_V(rank_in, rank_out, G_S(1,:,1), J_S(1), J_V, Gout_V(:,:,1))
  call vert_loop_SSV_V(rank_in, rank_out, G_S(1,:,2), J_S(1), J_V, Gout_V(:,:,2))
  call vert_loop_SSV_V(rank_in, rank_out, G_S(1,:,3), J_S(1), J_V, Gout_V(:,:,3))
  call vert_loop_SSV_V(rank_in, rank_out, G_S(1,:,4), J_S(1), J_V, Gout_V(:,:,4))
end subroutine loop_SSV_V


! *****************************************
subroutine loop_VSS_V(G_V, J_S1, J_S2, Gout_V)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_V(:,:,:), J_S1(4), J_S2(4)
  complex(qp), intent(out) :: Gout_V(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_V,2)
  rank_out = size(Gout_V,2)

  call vert_loop_VSS_V(rank_in, rank_out, G_V(:,:,1), J_S1(1), J_S2(1), Gout_V(:,:,1))
  call vert_loop_VSS_V(rank_in, rank_out, G_V(:,:,2), J_S1(1), J_S2(1), Gout_V(:,:,2))
  call vert_loop_VSS_V(rank_in, rank_out, G_V(:,:,3), J_S1(1), J_S2(1), Gout_V(:,:,3))
  call vert_loop_VSS_V(rank_in, rank_out, G_V(:,:,4), J_S1(1), J_S2(1), Gout_V(:,:,4))
end subroutine loop_VSS_V


! *****************************************
subroutine loop_SVV_S(G_S, J_V1, J_V2, Gout_S)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_S(:,:,:), J_V1(4), J_V2(4)
  complex(qp), intent(out) :: Gout_S(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_S,2)
  rank_out = size(Gout_S,2)

  call vert_loop_SVV_S(rank_in, rank_out, G_S(1,:,1), J_V1, J_V2, Gout_S(1,:,1))
  call vert_loop_SVV_S(rank_in, rank_out, G_S(1,:,2), J_V1, J_V2, Gout_S(1,:,2))
  call vert_loop_SVV_S(rank_in, rank_out, G_S(1,:,3), J_V1, J_V2, Gout_S(1,:,3))
  call vert_loop_SVV_S(rank_in, rank_out, G_S(1,:,4), J_V1, J_V2, Gout_S(1,:,4))
end subroutine loop_SVV_S


! *****************************************
subroutine loop_WWV_V(G_V, J_V1, J_V2, Gout_V)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_V(:,:,:), J_V1(4), J_V2(4)
  complex(qp), intent(out) :: Gout_V(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_V,2)
  rank_out = size(Gout_V,2)

  call vert_loop_WWV_V(rank_in, rank_out, G_V(:,:,1), J_V1, J_V2, Gout_V(:,:,1))
  call vert_loop_WWV_V(rank_in, rank_out, G_V(:,:,2), J_V1, J_V2, Gout_V(:,:,2))
  call vert_loop_WWV_V(rank_in, rank_out, G_V(:,:,3), J_V1, J_V2, Gout_V(:,:,3))
  call vert_loop_WWV_V(rank_in, rank_out, G_V(:,:,4), J_V1, J_V2, Gout_V(:,:,4))
end subroutine loop_WWV_V


! *****************************************
subroutine loop_VWW_V(G_V, J_V1, J_V2, Gout_V)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_V(:,:,:), J_V1(4), J_V2(4)
  complex(qp), intent(out) :: Gout_V(:,:,:)
  integer :: rank_in, rank_out
  rank_in  = size(G_V,2)
  rank_out = size(Gout_V,2)

  call vert_loop_VWW_V(rank_in, rank_out, G_V(:,:,1), J_V1, J_V2, Gout_V(:,:,1))
  call vert_loop_VWW_V(rank_in, rank_out, G_V(:,:,2), J_V1, J_V2, Gout_V(:,:,2))
  call vert_loop_VWW_V(rank_in, rank_out, G_V(:,:,3), J_V1, J_V2, Gout_V(:,:,3))
  call vert_loop_VWW_V(rank_in, rank_out, G_V(:,:,4), J_V1, J_V2, Gout_V(:,:,4))
end subroutine loop_VWW_V

end module ol_vert_interface_qp

