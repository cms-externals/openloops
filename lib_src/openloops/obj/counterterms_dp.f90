
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


module ol_counterterms_dp

contains

! **********************************************************************
subroutine counter_Q_A(ctQA, J_Q, K, Jout_Q)
! Q -> Q counter term without left/right splitting
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: ctQA(2), J_Q(4), K(4)
  complex(dp), intent(out) :: Jout_Q(4)
  Jout_Q(1) = ctQA(1) * ( - K(2)*J_Q(3) + K(4)*J_Q(4)) - ctQA(2) * J_Q(1)
  Jout_Q(2) = ctQA(1) * ( - K(1)*J_Q(4) + K(3)*J_Q(3)) - ctQA(2) * J_Q(2)
  Jout_Q(3) = ctQA(1) * ( - K(1)*J_Q(1) - K(4)*J_Q(2)) - ctQA(2) * J_Q(3)
  Jout_Q(4) = ctQA(1) * ( - K(2)*J_Q(2) - K(3)*J_Q(1)) - ctQA(2) * J_Q(4)
end subroutine counter_Q_A


! **********************************************************************
subroutine counter_A_Q(ctQA, J_A, K, Jout_A)
! A -> A counter term without left/right splitting
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: ctQA(2), J_A(4), K(4)
  complex(dp), intent(out) :: Jout_A(4)
  Jout_A(1) = ctQA(1) * ( + K(1)*J_A(3) + K(3)*J_A(4)) - ctQA(2) * J_A(1)
  Jout_A(2) = ctQA(1) * ( + K(2)*J_A(4) + K(4)*J_A(3)) - ctQA(2) * J_A(2)
  Jout_A(3) = ctQA(1) * ( + K(2)*J_A(1) - K(3)*J_A(2)) - ctQA(2) * J_A(3)
  Jout_A(4) = ctQA(1) * ( + K(1)*J_A(2) - K(4)*J_A(1)) - ctQA(2) * J_A(4)
end subroutine counter_A_Q


! **********************************************************************
subroutine counter_V_V(ctVV, J_V, K, Jout_V)
! V -> V counter term
! **********************************************************************
  use kind_types, only: dp
  use ol_contractions_dp, only: cont_VV
  implicit none
  complex(dp), intent(in)  :: ctVV(3), J_V(4), K(4)
  complex(dp), intent(out) :: Jout_V(4)
  Jout_V = (ctVV(1)*cont_VV(K,K) + ctVV(2)) * J_V + ctVV(3) * (cont_VV(J_V,K)) * K
end subroutine counter_V_V


! **********************************************************************
subroutine counter_S_S(ctSS, J_S, K, Jout_S)
! S -> S counter term
! **********************************************************************
  use kind_types, only: dp
  use ol_contractions_dp, only: cont_V
  implicit none
  complex(dp), intent(in)  :: ctSS(2), J_S(4), K(4)
  complex(dp), intent(out) :: Jout_S(4)
  Jout_S(1) = (ctSS(1)*cont_V(K) - ctSS(2)) * J_S(1)
end subroutine counter_S_S


! **********************************************************************
subroutine counter_Q_A_LR(ctQA, J_Q, K, Jout_Q)
! Q -> Q counter term with left/right splitting
! ctQA(1)*slash(k)*P_R+ctQA(2)*slash(k)*P_L - ctQA(3) P_R - ctQA(4) P_L
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: ctQA(4), J_Q(4), K(4)
  complex(dp), intent(out) :: Jout_Q(4)
  Jout_Q(1) = ctQA(2) * ( - K(2)*J_Q(3) + K(4)*J_Q(4)) - ctQA(3) * J_Q(1)
  Jout_Q(2) = ctQA(2) * ( - K(1)*J_Q(4) + K(3)*J_Q(3)) - ctQA(3) * J_Q(2)
  Jout_Q(3) = ctQA(1) * ( - K(1)*J_Q(1) - K(4)*J_Q(2)) - ctQA(4) * J_Q(3)
  Jout_Q(4) = ctQA(1) * ( - K(2)*J_Q(2) - K(3)*J_Q(1)) - ctQA(4) * J_Q(4)
end subroutine counter_Q_A_LR



! **********************************************************************
subroutine counter_A_Q_LR(ctQA, J_A, K, Jout_A)
! A -> A counter term with left/right splitting
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: ctQA(4), J_A(4), K(4)
  complex(dp), intent(out) :: Jout_A(4)
  Jout_A(1) = ctQA(1) * ( + K(1)*J_A(3) + K(3)*J_A(4)) - ctQA(3) * J_A(1)
  Jout_A(2) = ctQA(1) * ( + K(2)*J_A(4) + K(4)*J_A(3)) - ctQA(3) * J_A(2)
  Jout_A(3) = ctQA(2) * ( + K(2)*J_A(1) - K(3)*J_A(2)) - ctQA(4) * J_A(3)
  Jout_A(4) = ctQA(2) * ( + K(1)*J_A(2) - K(4)*J_A(1)) - ctQA(4) * J_A(4)
end subroutine counter_A_Q_LR



! ======================================================================
! Vertex counter terms.
! For QCD corrections, these are just copies of the vertex routines.
! ======================================================================


! **********************************************************************
subroutine counter_ZQ_A(g_RL, J_Z, J_Q, Jout_Q)
! bare ZQ -> Q Z-like interaction
! ----------------------------------------------------------------------
! J_Q(4)     = incoming quark current
! J_Z(4)     = incoming Z current ("light-cone" rep.)
! g_RL(1)    = right-handed coupling gR
! g_RL(2)    = left-handed coupling gL
! Jout_Q(4)  = outgoing quark current
! Jout_Q(i)  = J_Z(A)*[gamma_A*(gR*w_R+gL*w_L)](i,j)*J_Q(j)
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp) :: J_Q(4), J_Z(4), Jout_Q(4)
  complex(dp) :: g_RL(2)
  Jout_Q(1) = g_RL(2) * ( - J_Z(2)*J_Q(3) + J_Z(4)*J_Q(4))
  Jout_Q(2) = g_RL(2) * ( - J_Z(1)*J_Q(4) + J_Z(3)*J_Q(3))
  Jout_Q(3) = g_RL(1) * ( - J_Z(1)*J_Q(1) - J_Z(4)*J_Q(2))
  Jout_Q(4) = g_RL(1) * ( - J_Z(2)*J_Q(2) - J_Z(3)*J_Q(1))
end subroutine counter_ZQ_A


! **********************************************************************
subroutine counter_AZ_Q(g_RL, J_A, J_Z, Jout_A)
! bare AZ -> A Z-like interaction
! ----------------------------------------------------------------------
! J_A(4)     = incoming anti-quark current
! J_Z(4)     = incoming Z current (light-cone rep.)
! g_RL(1)    = right-handed coupling gR
! g_RL(2)    = left-handed coupling gL
! Jout_A(4)  = outgoing anti-quark current
! Jout_A(i)  = J_A(j) * [gamma_A*(gR*w_R+gL*w_L)](j,i) * J_Z(A)
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp) :: J_A(4), J_Z(4), Jout_A(4)
  complex(dp) :: g_RL(2)
  Jout_A(1) = g_RL(1) * ( - J_Z(1)*J_A(3) - J_Z(3)*J_A(4))
  Jout_A(2) = g_RL(1) * ( - J_Z(2)*J_A(4) - J_Z(4)*J_A(3))
  Jout_A(3) = g_RL(2) * ( - J_Z(2)*J_A(1) + J_Z(3)*J_A(2))
  Jout_A(4) = g_RL(2) * ( - J_Z(1)*J_A(2) + J_Z(4)*J_A(1))
end subroutine counter_AZ_Q


! **********************************************************************
subroutine counter_QA_Z(g_RL, J_Q, J_A, Jout_Z)
! bare QA -> Z Z-like interaction
! ----------------------------------------------------------------------
! J_Q(4)    = quark current
! J_A(4)    = anti-quark current
! g_RL(1)   = right-handed coupling gR
! g_RL(2)   = left-handed coupling gL
! Jout_Z(4) = outgoing Z current (light-cone rep.)
! Jout_Z(A) = J_A(i) * [gamma^A*(gR*w_R+gL*w_L)](i,j) * J_Q(j)
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp) :: J_Q(4), J_A(4), Jout_Z(4)
  complex(dp) :: g_RL(2)
  Jout_Z(1) = - g_RL(2)*J_A(1)*J_Q(3) - g_RL(1)*J_A(4)*J_Q(2)
  Jout_Z(2) = - g_RL(2)*J_A(2)*J_Q(4) - g_RL(1)*J_A(3)*J_Q(1)
  Jout_Z(3) = - g_RL(2)*J_A(1)*J_Q(4) + g_RL(1)*J_A(3)*J_Q(2)
  Jout_Z(4) = - g_RL(2)*J_A(2)*J_Q(3) + g_RL(1)*J_A(4)*J_Q(1)
  Jout_Z = Jout_Z + Jout_Z
end subroutine counter_QA_Z


! **********************************************************************
subroutine counter_WQ_A(J_W, J_Q, Jout_Q)
! bare WQ -> Q W-like (i.e. left-handed) interaction
! ----------------------------------------------------------------------
! J_Q(4)    = incoming quark current
! J_W(4)    = incoming W current ("light-cone" rep.)
! Jout_Q(4) = outgoing quark current
! Jout_Q(i) = J_W(A) * [gamma_A*w_L](i,j) * J_Q(j)
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp) :: J_Q(4), J_W(4), Jout_Q(4)
  Jout_Q(1)   = - J_W(2)*J_Q(3) + J_W(4)*J_Q(4)
  Jout_Q(2)   = - J_W(1)*J_Q(4) + J_W(3)*J_Q(3)
  Jout_Q(3:4) = 0
end subroutine counter_WQ_A


! **********************************************************************
subroutine counter_AW_Q(J_A, J_W, Jout_A)
! bare AW -> A W-like (i.e. left-handed) interaction
! ----------------------------------------------------------------------
! J_A(4)    = incoming anti-quark current
! J_W(4)    = incoming W current (light-cone rep.)
! Jout_A(4) = outgoing anti-quark current
! Jout_A(i) = J_A(j) * [gamma_A*w_L](j,i) * J_W(A)
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp) :: J_A(4), J_W(4), Jout_A(4)
  Jout_A(1:2) = 0
  Jout_A(3)   = - J_W(2)*J_A(1) + J_W(3)*J_A(2)
  Jout_A(4)   = - J_W(1)*J_A(2) + J_W(4)*J_A(1)
end subroutine counter_AW_Q


! **********************************************************************
subroutine counter_QA_W(J_Q, J_A, Jout_W)
! bare QA -> W W-like (i.e. left-handed) interaction
! ----------------------------------------------------------------------
! J_Q(4)    = quark current
! J_A(4)    = anti-quark current
! Jout_W(4) = outgoing W current (light-cone rep.)
! Jout_W(A) = J_A(i) * [gamma^A*w_L](i,j) * J_Q(j)
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp) :: J_Q(4), J_A(4), Jout_W(4)
  Jout_W(1) = - J_A(1)*J_Q(3)
  Jout_W(2) = - J_A(2)*J_Q(4)
  Jout_W(3) = - J_A(1)*J_Q(4)
  Jout_W(4) = - J_A(2)*J_Q(3)
  Jout_W = Jout_W + Jout_W
end subroutine counter_QA_W


! **********************************************************************
subroutine counter_VQ_A(J_V, J_Q, Jout_Q)
! VQ -> Q counter term; without left/right splitting
! Factorised wrt. vert_VQ_A: ctQAV = gQCD*dlnG + dZf + 1/2 * dZg
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: J_Q(4), J_V(4)
  complex(dp), intent(out) :: Jout_Q(4)
  Jout_Q(1) = - J_V(2)*J_Q(3)+J_V(4)*J_Q(4)
  Jout_Q(2) = - J_V(1)*J_Q(4)+J_V(3)*J_Q(3)
  Jout_Q(3) = - J_V(1)*J_Q(1)-J_V(4)*J_Q(2)
  Jout_Q(4) = - J_V(2)*J_Q(2)-J_V(3)*J_Q(1)
end subroutine counter_VQ_A


! **********************************************************************
subroutine counter_AV_Q(J_A, J_V, Jout_A)
! AV -> A counter term; without left/right splitting
! Factorised wrt. vert_AV_Q: ctQAV = gQCD*dlnG + dZf + 1/2 * dZg
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: J_A(4), J_V(4)
  complex(dp), intent(out) :: Jout_A(4)
  Jout_A(1) = - J_V(1)*J_A(3) - J_V(3)*J_A(4)
  Jout_A(2) = - J_V(2)*J_A(4) - J_V(4)*J_A(3)
  Jout_A(3) = - J_V(2)*J_A(1) + J_V(3)*J_A(2)
  Jout_A(4) = - J_V(1)*J_A(2) + J_V(4)*J_A(1)
end subroutine counter_AV_Q


! **********************************************************************
subroutine counter_QA_V(J_Q, J_A, Jout_V)
! QA -> V counter term; without left/right splitting
! Factorised wrt. vert_QA_V: ctQAV = gQCD*dlnG + dZf + 1/2 * dZg
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: J_Q(4), J_A(4)
  complex(dp), intent(out) :: Jout_V(4)
  Jout_V(1) = - J_A(1)*J_Q(3) - J_A(4)*J_Q(2)
  Jout_V(2) = - J_A(2)*J_Q(4) - J_A(3)*J_Q(1)
  Jout_V(3) = - J_A(1)*J_Q(4) + J_A(3)*J_Q(2)
  Jout_V(4) = - J_A(2)*J_Q(3) + J_A(4)*J_Q(1)
  Jout_V = Jout_V + Jout_V
end subroutine counter_QA_V


! **********************************************************************
subroutine counter_VQ_A_LR(ctVFF,J_V, J_Q, Jout_Q)
! VQ -> Q counter term; with left/right splitting -> ZQ_A
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: J_Q(4), J_V(4), ctVFF(2)
  complex(dp), intent(out) :: Jout_Q(4)
  call counter_ZQ_A(ctVFF,J_V, J_Q, Jout_Q)
end subroutine counter_VQ_A_LR

! **********************************************************************
subroutine counter_AV_Q_LR(ctVFF,J_A, J_V, Jout_A)
! AV -> A counter term; with left/right splitting -> AZ_Q
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: J_A(4), J_V(4),ctVFF(2)
  complex(dp), intent(out) :: Jout_A(4)
  call counter_AZ_Q(ctVFF,J_A, J_V, Jout_A)
end subroutine counter_AV_Q_LR

! **********************************************************************
subroutine counter_QA_V_LR(ctVFF,J_Q, J_A, Jout_V)
! QA -> V counter term; with left/right splitting -> QA_Z
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: J_Q(4), J_A(4),ctVFF(2)
  complex(dp), intent(out) :: Jout_V(4)
  call counter_QA_Z(ctVFF,J_Q, J_A, Jout_V)
end subroutine counter_QA_V_LR



! ! **********************************************************************
! subroutine counter_UV_W(J_V1, P1, J_V2, P2, Jout_V)
! ! VV -> V counter term
! ! Factorised wrt. vert_UV_W: ctVVV = dlnG*gQCD + 3/2 * dZg
! ! **********************************************************************
!   use kind_types, only: dp
!   use ol_contractions_dp, only: cont_VV
!   implicit none
!   complex(dp), intent(in)  :: J_V1(4), P1(4), J_V2(4), P2(4)
!   complex(dp), intent(out) :: Jout_V(4)
!   Jout_V = cont_VV(J_V1,J_V2) * (P1 - P2) + cont_VV(P1+P2+P2,J_V1) * J_V2 - cont_VV(P1+P1+P2,J_V2) * J_V1
! end subroutine counter_UV_W
subroutine counter_UV_W(J_V1, P1, J_V2, P2, Jout_V)
  use kind_types, only: dp
  use ol_contractions_dp, only: cont_VV
  implicit none
  complex(dp) :: P1(4), P2(4)
  complex(dp) :: J_V1(4), J_V2(4), Jout_V(4)
  complex(dp) :: J1J2, P1J2, P2J1
  J1J2 = cont_VV(J_V1,J_V2)
  P1J2 = cont_VV(P1+P1+P2,J_V2)
  P2J1 = cont_VV(P1+P2+P2,J_V1)
  Jout_V = J1J2 * (P1 - P2) + P2J1 * J_V2 - P1J2 * J_V1
end subroutine counter_UV_W


! **********************************************************************
subroutine counter_EV_V(J_V1, J_V2, J_V3, Jout_V)
! sigma vertex counter term, where the sigma wave function is replaced by two gluon wave functions J_V1 and J_V2
! Jout_V(d) = (g(a,c)*g(b,d) + g(1,4)*g(2,3)) * J_V1(a) * J_V1(b) * J_V1(c)
!           = J_V1.J_V3 * J_V2(d) + J_V2.J_V3 * J_V1(d)
! Factorised wrt. vert_EV_V: ctVVVV = 1/2*dlnG*gQCD + dZg
! **********************************************************************
  use kind_types, only: dp
  use ol_contractions_dp, only: cont_VV
  implicit none
  complex(dp), intent(in)  :: J_V1(4), J_V2(4), J_V3(4)
  complex(dp), intent(out) :: Jout_V(4)
  Jout_V = cont_VV(J_V1,J_V3) * J_V2 - cont_VV(J_V2,J_V3) * J_V1
end subroutine counter_EV_V


! **********************************************************************
subroutine counter_AQ_S(g_RL, J_A, J_Q, Jout_S)
! Fermion-scalar-vertex
! g_RL(1) = right-handed coupling
! g_RL(2) = left-handed coupling
! Incoming anti-fermion current: J_A(4)
! Incoming fermion current:      J_Q(4)
! Outgoing scalar current:       Jout_S = gR*J_A.P_R.J_Q + gL*J_A.P_L.J_Q
!   with the right- and left-handed projectors P_R = (1+y5)/2 and P_L = (1-y5)/2
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: g_RL(2), J_A(4), J_Q(4)
  complex(dp), intent(out) :: Jout_S(4)
  Jout_S(1) = g_RL(1) * (J_A(1)*J_Q(1) + J_A(2)*J_Q(2)) + g_RL(2) * (J_A(3)*J_Q(3) + J_A(4)*J_Q(4))
end subroutine counter_AQ_S


! **********************************************************************
subroutine counter_QS_A(g_RL, J_Q, J_S, Jout_A)
! Fermion-scalar-vertex
! g_RL(1) = right-handed coupling
! g_RL(2) = left-handed coupling
! Incoming fermion current:      J_Q(4)
! Incoming scalar current:       J_S
! Outgoing anti-fermion current: Jout_A(4)
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: g_RL(2), J_Q(4), J_S(4)
  complex(dp), intent(out) :: Jout_A(4)
  Jout_A(1) = g_RL(1) * J_Q(1) * J_S(1)
  Jout_A(2) = g_RL(1) * J_Q(2) * J_S(1)
  Jout_A(3) = g_RL(2) * J_Q(3) * J_S(1)
  Jout_A(4) = g_RL(2) * J_Q(4) * J_S(1)
end subroutine counter_QS_A


! **********************************************************************
subroutine counter_SA_Q(g_RL, J_S, J_A, Jout_Q)
! Fermion-scalar-vertex
! g_RL(1) = right-handed coupling
! g_RL(2) = left-handed coupling
! Incoming scalar current:       J_S
! Incoming anti-fermion current: J_A(4)
! Outgoing fermion current:      Jout_Q(4)
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: g_RL(2), J_S(4), J_A(4)
  complex(dp), intent(out) :: Jout_Q(4)
  Jout_Q(1) = g_RL(1) * J_A(1) * J_S(1)
  Jout_Q(2) = g_RL(1) * J_A(2) * J_S(1)
  Jout_Q(3) = g_RL(2) * J_A(3) * J_S(1)
  Jout_Q(4) = g_RL(2) * J_A(4) * J_S(1)
end subroutine counter_SA_Q


! **********************************************************************
subroutine counter_VG_G(J_V, Jin_G, p2, Jout_G, p3)
! Z-gluon-gluon vertex for R2
! Jout_G(c) = ep(a,b,c,d) * J_V(a) * Jin_G(b) * (p2-p3)(d)
! **********************************************************************
  use kind_types, only: dp
  use ol_contractions_dp, only: cont_EpVVV
  implicit none
  complex(dp), intent(in)  :: J_V(4), Jin_G(4), p2(4), p3(4)
  complex(dp), intent(out) :: Jout_G(4)
  call cont_EpVVV(J_V, Jin_G, p2-p3, Jout_G)
end subroutine counter_VG_G


! **********************************************************************
subroutine counter_GG_V(J_G1, p1, J_G2, p2, Jout_V)
! Z-gluon-gluon vertex for R2
! Jout_V(a) = ep(a,b,c,d) * J_G1(b) * J_G2(c) * (p1-p2)(d)
! **********************************************************************
  use kind_types, only: dp
  use ol_contractions_dp, only: cont_EpVVV
  implicit none
  complex(dp), intent(in)  :: J_G1(4), p1(4), J_G2(4), p2(4)
  complex(dp), intent(out) :: Jout_V(4)
  call cont_EpVVV(J_G1, J_G2, p1-p2, Jout_V)
end subroutine counter_GG_V


! **********************************************************************
subroutine counter_SG_G(J_S, Jin_G, Jout_G)
! Higgs-gluon-gluon vertex for R2
! Jout_G = J_S * Jin_G
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: J_S(4), Jin_G(4)
  complex(dp), intent(out) :: Jout_G(4)
  Jout_G = J_S(1) * Jin_G
end subroutine counter_SG_G


! **********************************************************************
subroutine counter_GG_S(J_G1, J_G2, Jout_S)
! Higgs-gluon-gluon vertex for R2
! Jout_S = J_G1.J_G2
! **********************************************************************
  use kind_types, only: dp
  use ol_contractions_dp, only: cont_VV
  implicit none
  complex(dp), intent(in)  :: J_G1(4), J_G2(4)
  complex(dp), intent(out) :: Jout_S(4)
  Jout_S(1) = cont_VV(J_G1,J_G2)
end subroutine counter_GG_S


! **********************************************************************
subroutine counter_VVG_G(J_V1, J_V2, Jin_G, Jout_G)
! Vector-vector-gluon-gluon vertex for R2
! Jout_G = J_V1.J_V2 * Jin_G + J_V1.Jin_G * J_V2 + J_V2.Jin_G * J_V1
! **********************************************************************
  use kind_types, only: dp
  use ol_contractions_dp, only: cont_VV
  implicit none
  complex(dp), intent(in)  :: J_V1(4), J_V2(4), Jin_G(4)
  complex(dp), intent(out) :: Jout_G(4)
  Jout_G = cont_VV(J_V1,J_V2) * Jin_G + cont_VV(J_V1,Jin_G) * J_V2 + cont_VV(J_V2,Jin_G) * J_V1
  ! Jout_G = Jout_G + 4*(1-g5s)*sum_ij(a_V1QiQj*aV2QiQj) * cont_VV(J_V1,J_V2) * Jin_G
end subroutine counter_VVG_G


! **********************************************************************
subroutine counter_SSG_G(J_S1, J_S2, Jin_G, Jout_G)
! Scalar-scalar-gluon-gluon vertex for R2
! Jout_G = J_S1 * J_S2 * Jin_G
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: J_S1(4), J_S2(4), Jin_G(4)
  complex(dp), intent(out) :: Jout_G(4)
  Jout_G = J_S1(1) * J_S2(1) * Jin_G
end subroutine counter_SSG_G


! **********************************************************************
subroutine counter_GGS_S(J_G1, J_G2, Jin_S, Jout_S)
! Scalar-scalar-gluon-gluon vertex for R2
! Jout_S = Jin_S * J_G1.J_G2
! **********************************************************************
  use kind_types, only: dp
  use ol_contractions_dp, only: cont_VV
  implicit none
  complex(dp), intent(in)  :: J_G1(4), J_G2(4), Jin_S(4)
  complex(dp), intent(out) :: Jout_S(4)
  Jout_S(1) = Jin_S(1) * cont_VV(J_G1,J_G2)
end subroutine counter_GGS_S


! **********************************************************************
subroutine counter_GGG_V(gVA, J_G1, J_G2, J_G3, Jout_V)
! Vector-gluon-gluon-gluon vertex for R2
! Jout_V(a) = [v * (g(a,b)*g(c,d) + g(a,c)*g(b,d) + g(a,d)*g(b,c)) + a * ep(a,b,c,d)] * J_G1(b) * J_G2(c) * J_G3(d)
! v = 2/3*vector_coupling; a = -6*i*axial_coupling
! **********************************************************************
  use kind_types, only: dp
  use ol_contractions_dp, only: cont_VV, cont_EpVVV
  implicit none
  complex(dp), intent(in)  :: gVA(2), J_G1(4), J_G2(4), J_G3(4)
  complex(dp), intent(out) :: Jout_V(4)
  if (gVA(2) /= 0) then
    call cont_EpVVV(J_G1, J_G2, J_G3, Jout_V)
    Jout_V = gVA(2) * Jout_V
  else
    Jout_V = 0
  end if
  Jout_V = Jout_V + gVA(1) * (cont_VV(J_G1,J_G2)*J_G3 + cont_VV(J_G2,J_G3)*J_G1 + cont_VV(J_G3,J_G1)*J_G2)
end subroutine counter_GGG_V


! **********************************************************************
subroutine counter_VGG_G(gVA, J_V, J_G1, J_G2, Jout_G)
! Vector-gluon-gluon-gluon vertex for R2
! Jout_G(d) = [v * (g(a,b)*g(c,d) + g(a,c)*g(b,d) + g(a,d)*g(b,c)) + a * ep(a,b,c,d)] * J_V(a) * J_G1(b) * J_G2(c)
! v = 2/3*vector_coupling; a = -6*i*axial_coupling
! **********************************************************************
  use kind_types, only: dp
  use ol_contractions_dp, only: cont_VV, cont_EpVVV
  implicit none
  complex(dp), intent(in)  :: gVA(2), J_V(4), J_G1(4), J_G2(4)
  complex(dp), intent(out) :: Jout_G(4)
  if (gVA(2) /= 0) then
    call cont_EpVVV(J_V, J_G1, J_G2, Jout_G)
    Jout_G = - gVA(2) * Jout_G
  else
    Jout_G = 0
  end if
  Jout_G = Jout_G + gVA(1) * (cont_VV(J_V,J_G1)*J_G2 + cont_VV(J_G1,J_G2)*J_V + cont_VV(J_G2,J_V)*J_G1)
end subroutine counter_VGG_G


! **********************************************************************
subroutine counter_GGG_G(J_G1, J_G2, J_G3, Jout_G)
! gluon-gluon-gluon-gluon vertex for R2, factorised Lorentz monomials g(a,b)*g(c,d)
! Jout_G(d) = g(a,b)*g(c,d) * J_G1(a) * J_G2(b) * J_G3(c) = J_G1.J_G2 * J_G3
! **********************************************************************
  use kind_types, only: dp
  use ol_contractions_dp, only: cont_VV
  implicit none
  complex(dp), intent(in)  :: J_G1(4), J_G2(4), J_G3(4)
  complex(dp), intent(out) :: Jout_G(4)
  Jout_G = cont_VV(J_G1,J_G2) * J_G3
end subroutine counter_GGG_G





! ======================================================================
! Additional vertex counter terms.
! for EW corrections, these are just copies of the vertex routines.
! ======================================================================

! **********************************************************************
subroutine counter_SS_S(J_S1, J_S2, Jout_S)
! Three scalar vertex
! Incoming scalar currents: J_S1, J_S2
! Outgoing scalar current:  Jout_S = J_S1 * J_S2
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: J_S1(4), J_S2(4)
  complex(dp), intent(out) :: Jout_S(4)
  Jout_S(1) = J_S1(1) * J_S2(1)
end subroutine counter_SS_S

! **********************************************************************
! subroutine vert_VS_S(J_V, J_S, P1, Jout_S)
subroutine counter_VS_T(J_V, P1, J_S, P2, Jout_S)
! Vector boson + two scalars vertex
! Incoming vector current: J_V(4), incoming momentum P1(4) (light-cone rep.)
! Incoming scalar current: J_S,    incoming momentum P2(4) (light-cone rep.)
! Outgoing scalar current: Jout_S = J_V.(2*P2+P1) * J_S
! **********************************************************************
  use kind_types, only: dp
  use ol_contractions_dp, only: cont_VV
  implicit none
  complex(dp), intent(in)  :: J_V(4), P1(4), J_S(4), P2(4)
  complex(dp), intent(out) :: Jout_S(4)
  Jout_S(1) = cont_VV(P1+P2+P2, J_V) * J_S(1)
end subroutine counter_VS_T


! **********************************************************************
! subroutine vert_SV_S(J_S, P2, J_V, Jout_S)
subroutine counter_TV_S(J_S, P1, J_V, P2, Jout_S)
! Vector boson + two scalars vertex
! Incoming scalar current: J_S, incoming momentum P2(4) (light-cone rep.)
! Incoming vector current: J_V(4) (light-cone rep.)
! Outgoing scalar current: Jout_S = J_V.(-2*P1-P2) * J_S
! **********************************************************************
  use kind_types, only: dp
  use ol_contractions_dp, only: cont_VV
  implicit none
  complex(dp), intent(in)  :: J_S(4), P1(4), J_V(4), P2(4)
  complex(dp), intent(out) :: Jout_S(4)
  Jout_S(1) = - cont_VV(P1+P1+P2, J_V) * J_S(1)
end subroutine counter_TV_S


! **********************************************************************
! subroutine vert_SS_V(J_S1, P1, J_S2, P2, Jout_V)
subroutine counter_ST_V(J_S1, P1, J_S2, P2, Jout_V)
! Vector boson + two scalars vertex
! Incoming scalar currents: J_S1, J_S2, incoming momenta P1(4), P2(4) (light-cone rep.)
! Outgoing vector current: Jout_V(4) = J_S1 * J_S2 * (P1 - P2)
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: J_S1(4), P1(4), J_S2(4), P2(4)
  complex(dp), intent(out) :: Jout_V(4)
  Jout_V = (J_S1(1) * J_S2(1)) * (P1 - P2)
! end subroutine vert_SS_V
end subroutine counter_ST_V


! **********************************************************************
subroutine counter_VV_S(J_V1, J_V2, Jout_S)
! Two vector boson + scalar vertex
! Incoming vector currents: J_V1(4), J_V2(4) (light-cone rep.)
! Outgoing scalar current:  Jout_S
! **********************************************************************
  use kind_types, only: dp
  use ol_contractions_dp, only: cont_VV
  implicit none
  complex(dp), intent(in)  :: J_V1(4), J_V2(4)
  complex(dp), intent(out) :: Jout_S(4)
  Jout_S(1) = cont_VV(J_V1,J_V2)
end subroutine counter_VV_S


! **********************************************************************
subroutine counter_VS_V(J_V, J_S, Jout_V)
! Two vector boson + scalar vertex
! Incoming vector current: J_V(4) (light-cone rep.)
! Incoming scalar current: J_S
! Outgoing vector current: Jout_V
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: J_V(4), J_S(4)
  complex(dp), intent(out) :: Jout_V(4)
  Jout_V = J_V * J_S(1)
end subroutine counter_VS_V


! **********************************************************************
subroutine counter_SV_V(J_S, J_V, Jout_V)
! Two vector boson + scalar vertex
! Incoming scalar current: J_S
! Incoming vector current: J_V(4) (light-cone rep.)
! Outgoing vector current: Jout_V
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: J_S(4), J_V(4)
  complex(dp), intent(out) :: Jout_V(4)
  Jout_V = J_S(1) * J_V
end subroutine counter_SV_V




! **********************************************************************
subroutine counter_SSS_S(J_S1, J_S2, J_S3, Jout_S)
! Four scalar vertex
! Incoming scalar currents: J_S1, J_S2, J_S3
! Outgoing scalar current:  Jout_S = J_S1 * J_S2 * J_S3
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: J_S1(4), J_S2(4), J_S3(4)
  complex(dp), intent(out) :: Jout_S(4)
  Jout_S(1) = J_S1(1) * J_S2(1) * J_S3(1)
end subroutine counter_SSS_S



! **********************************************************************
subroutine counter_VVV_V(J_V1, J_V2, J_V3, Jout_V)
! neutral Vector-vector-vector-vector pure R2
! ----------------------------------------------------------------------
! J_Vi(4)    = incoming vector boson currents (light-cone rep.)
! Jout_V(4)  = outgoing vector boson current  (light-cone rep.)
! Jout_V(a4) = [g(a1,a2)*g(a3,a4) + g(a2,a3)*g(a1,a4)
!               + g(a1,a3)*g(a2,a4)] * J_V1(a1) * J_V2(a2) * J_V3(a3)
! **********************************************************************
  use kind_types, only: dp
  use ol_contractions_dp, only: cont_VV
  implicit none

  complex(dp), intent(in)  :: J_V1(4), J_V2(4), J_V3(4)
  complex(dp), intent(out) :: Jout_V(4)
  complex(dp) :: J1J2, J1J3, J2J3

  J1J2 = cont_VV(J_V1, J_V2)
  J1J3 = cont_VV(J_V1, J_V3)
  J2J3 = cont_VV(J_V2, J_V3)
  Jout_V = J1J2 * J_V3 + J2J3 * J_V1 + J1J3 * J_V2

end subroutine counter_VVV_V


! **********************************************************************
subroutine counter_WWV_V(ctWWVV, J_V1, J_V2, J_V3, Jout_V)
! W-W-vector-vector UV & pure R2
! ----------------------------------------------------------------------
! J_Vi(4)    = incoming vector boson currents (light-cone rep.)
! Jout_V(4)  = outgoing vector boson current  (light-cone rep.)
! Jout_V(a4) = [ctVVVV(1) * g(a1,a2)*g(a3,a4) + ctVVVV(2) * g(a2,a3)*g(a1,a4)
!               + ctVVVV(2) * g(a1,a3)*g(a2,a4)] * J_V1(a1) * J_V2(a2) * J_V3(a3)
! **********************************************************************
  use kind_types, only: dp
  use ol_contractions_dp, only: cont_VV
  implicit none

  complex(dp), intent(in)  :: J_V1(4), J_V2(4), J_V3(4), ctWWVV(2)
  complex(dp), intent(out) :: Jout_V(4)
  complex(dp) :: J1J2, J1J3, J2J3

  J1J2 = cont_VV(J_V1, J_V2)
  J1J3 = cont_VV(J_V1, J_V3)
  J2J3 = cont_VV(J_V2, J_V3)
  Jout_V = J1J2 * J_V3 * ctWWVV(1) + J2J3 * J_V1 * ctWWVV(2) + J1J3 * J_V2 * ctWWVV(2)

end subroutine counter_WWV_V

! **********************************************************************
subroutine counter_VWW_V(ctWWVV,J_V1,J_V2,J_V3,Jout_V)
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: J_V1(4), J_V2(4), J_V3(4), ctWWVV(2)
  complex(dp), intent(out) :: Jout_V(4)
  call counter_WWV_V(ctWWVV,J_V2,J_V3,J_V1,Jout_V)
end subroutine counter_VWW_V


! **********************************************************************
subroutine counter_WVV_W(ctWWVV, J_V1, J_V2, J_V3, Jout_V)
! **********************************************************************
  use kind_types, only: dp
  use ol_contractions_dp, only: cont_VV
  implicit none

  complex(dp), intent(in)  :: J_V1(4), J_V2(4), J_V3(4), ctWWVV(2)
  complex(dp), intent(out) :: Jout_V(4)
  complex(dp) :: J1J2, J1J3, J2J3

  J1J2 = cont_VV(J_V1, J_V2)
  J1J3 = cont_VV(J_V1, J_V3)
  J2J3 = cont_VV(J_V2, J_V3)
  Jout_V = J1J2 * J_V3 * ctWWVV(2) + J2J3 * J_V1 * ctWWVV(1) + J1J3 * J_V2 * ctWWVV(2)

end subroutine counter_WVV_W


! **********************************************************************
subroutine counter_WVW_V(ctWWVV, J_V1, J_V2, J_V3, Jout_V)
! **********************************************************************
  use kind_types, only: dp
  use ol_contractions_dp, only: cont_VV
  implicit none

  complex(dp), intent(in)  :: J_V1(4), J_V2(4), J_V3(4), ctWWVV(2)
  complex(dp), intent(out) :: Jout_V(4)
  complex(dp) :: J1J2, J1J3, J2J3

  J1J2 = cont_VV(J_V1, J_V2)
  J1J3 = cont_VV(J_V1, J_V3)
  J2J3 = cont_VV(J_V2, J_V3)
  Jout_V = J1J2 * J_V3 * ctWWVV(2) + J2J3 * J_V1 * ctWWVV(1) + J1J3 * J_V2 * ctWWVV(1)

end subroutine counter_WVW_V


! **********************************************************************
subroutine counter_VVS_S(J_V1, J_V2, J_S, Jout_S)
! Two vector boson + two scalars vertex
! Incoming vector currents: J_V1(4), J_V2(4) (light-cone rep.)
! Incoming scalar current:  J_S
! Outgoing scalar current:  Jout_S
! **********************************************************************
  use kind_types, only: dp
  use ol_contractions_dp, only: cont_VV
  implicit none
  complex(dp), intent(in)  :: J_V1(4), J_V2(4), J_S(4)
  complex(dp), intent(out) :: Jout_S(4)
  Jout_S(1) = cont_VV(J_V1,J_V2) * J_S(1)
end subroutine counter_VVS_S


! **********************************************************************
subroutine counter_SSV_V(J_S1, J_S2, J_V, Jout_V)
! Two vector boson + two scalars vertex
! Incoming scalar currents: J_S1, J_S2
! Incoming vector current:  J_V(4) (light-cone rep.)
! Outgoing vector current:  Jout_V (light-cone rep.)
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: J_S1(4), J_S2(4), J_V(4)
  complex(dp), intent(out) :: Jout_V(4)
  Jout_V = (J_S1(1) * J_S2(1)) * J_V
end subroutine counter_SSV_V


! **********************************************************************
subroutine counter_VSS_V(J_V, J_S1, J_S2, Jout_V)
! Two vector boson + two scalars vertex
! Incoming vector current:  J_V(4) (light-cone rep.)
! Incoming scalar currents: J_S1, J_S2
! Outgoing vector current:  Jout_V (light-cone rep.)
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: J_V(4), J_S1(4), J_S2(4)
  complex(dp), intent(out) :: Jout_V(4)
  Jout_V = (J_S1(1) * J_S2(1)) * J_V
end subroutine counter_VSS_V


! **********************************************************************
subroutine counter_SVV_S(J_S, J_V1, J_V2, Jout_S)
! Two vector boson + two scalars vertex
! Incoming scalar current:  J_S
! Incoming vector currents: J_V1(4), J_V2(4) (light-cone rep.)
! Outgoing scalar current:  Jout_S
! **********************************************************************
  use kind_types, only: dp
  use ol_contractions_dp, only: cont_VV
  implicit none
  complex(dp), intent(in)  :: J_S(4), J_V1(4), J_V2(4)
  complex(dp), intent(out) :: Jout_S(4)
  Jout_S(1) = J_S(1) * cont_VV(J_V1,J_V2)
end subroutine counter_SVV_S



end module ol_counterterms_dp

