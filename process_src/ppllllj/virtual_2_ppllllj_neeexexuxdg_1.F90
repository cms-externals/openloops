
module ol_vamp_2_ppllllj_neeexexuxdg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_2(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppllllj_neeexexuxdg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppllllj_neeexexuxdg_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppllllj_neeexexuxdg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppllllj_neeexexuxdg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,15) :: G0
  complex(REALKIND), dimension(4,5,4,28) :: G1
  complex(REALKIND), dimension(4,15,4,14) :: G2
  complex(REALKIND), dimension(5,3) :: G1tensor
  complex(REALKIND), dimension(15,14) :: G2tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_VA_Q(G0(:,:,:,1),wf(:,231),G0(:,:,:,2))
  call loop_A_Q(G0(:,:,:,2),Q(:,86),ZERO,G1(:,:,:,1))
  call loop_AW_Q(G1(:,:,:,1),wf(:,42),G1(:,:,:,2))
  call loop_A_Q(G1(:,:,:,2),Q(:,95),ZERO,G2(:,:,:,1))
  call check_last_AQ_V(l_switch,G2(:,:,:,1),wf(:,-5),G2tensor(:,1))
  call check_last_AQ_V(l_switch,G1(:,:,:,1),wf(:,45),G1tensor(:,1))
  call loop_VQ_A(G0(:,:,:,1),wf(:,224),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,102),ZERO,G1(:,:,:,3))
  call loop_QW_A(G1(:,:,:,3),wf(:,42),G1(:,:,:,4))
  call loop_Q_A(G1(:,:,:,4),Q(:,111),ZERO,G2(:,:,:,2))
  call check_last_QA_V(l_switch,G2(:,:,:,2),wf(:,-4),G2tensor(:,2))
  call check_last_QA_V(l_switch,G1(:,:,:,3),wf(:,58),G1tensor(:,2))
  call loop_VQ_A(G0(:,:,:,1),wf(:,226),G0(:,:,:,4))
  call loop_Q_A(G0(:,:,:,4),Q(:,102),ZERO,G1(:,:,:,5))
  call loop_QW_A(G1(:,:,:,5),wf(:,42),G1(:,:,:,6))
  call loop_Q_A(G1(:,:,:,6),Q(:,111),ZERO,G2(:,:,:,3))
  call check_last_QA_V(l_switch,G2(:,:,:,3),wf(:,-4),G2tensor(:,3))
  call check_last_QA_V(l_switch,G1(:,:,:,5),wf(:,58),G1tensor(:,3))
  call loop_VQ_A(G0(:,:,:,1),wf(:,280),G0(:,:,:,5))
  call loop_Q_A(G0(:,:,:,5),Q(:,47),ZERO,G1(:,:,:,7))
  call loop_QV_A(G1(:,:,:,7),wf(:,-6),G1(:,:,:,8))
  call loop_Q_A(G1(:,:,:,8),Q(:,111),ZERO,G2(:,:,:,4))
  call check_last_QA_V(l_switch,G2(:,:,:,4),wf(:,-4),G2tensor(:,4))
  call loop_VQ_A(G0(:,:,:,1),wf(:,282),G0(:,:,:,6))
  call loop_Q_A(G0(:,:,:,6),Q(:,47),ZERO,G1(:,:,:,9))
  call loop_QV_A(G1(:,:,:,9),wf(:,-6),G1(:,:,:,10))
  call loop_Q_A(G1(:,:,:,10),Q(:,111),ZERO,G2(:,:,:,5))
  call check_last_QA_V(l_switch,G2(:,:,:,5),wf(:,-4),G2tensor(:,5))
  call loop_VQ_A(G0(:,:,:,1),wf(:,288),G0(:,:,:,7))
  call loop_Q_A(G0(:,:,:,7),Q(:,47),ZERO,G1(:,:,:,11))
  call loop_QV_A(G1(:,:,:,11),wf(:,-6),G1(:,:,:,12))
  call loop_Q_A(G1(:,:,:,12),Q(:,111),ZERO,G2(:,:,:,6))
  call check_last_QA_V(l_switch,G2(:,:,:,6),wf(:,-4),G2tensor(:,6))
  call loop_VQ_A(G0(:,:,:,1),wf(:,290),G0(:,:,:,8))
  call loop_Q_A(G0(:,:,:,8),Q(:,47),ZERO,G1(:,:,:,13))
  call loop_QV_A(G1(:,:,:,13),wf(:,-6),G1(:,:,:,14))
  call loop_Q_A(G1(:,:,:,14),Q(:,111),ZERO,G2(:,:,:,7))
  call check_last_QA_V(l_switch,G2(:,:,:,7),wf(:,-4),G2tensor(:,7))
  call loop_VQ_A(G0(:,:,:,1),wf(:,294),G0(:,:,:,9))
  call loop_Q_A(G0(:,:,:,9),Q(:,47),ZERO,G1(:,:,:,15))
  call loop_QV_A(G1(:,:,:,15),wf(:,-6),G1(:,:,:,16))
  call loop_Q_A(G1(:,:,:,16),Q(:,111),ZERO,G2(:,:,:,8))
  call check_last_QA_V(l_switch,G2(:,:,:,8),wf(:,-4),G2tensor(:,8))
  call loop_VQ_A(G0(:,:,:,1),wf(:,300),G0(:,:,:,10))
  call loop_Q_A(G0(:,:,:,10),Q(:,47),ZERO,G1(:,:,:,17))
  call loop_QV_A(G1(:,:,:,17),wf(:,-6),G1(:,:,:,18))
  call loop_Q_A(G1(:,:,:,18),Q(:,111),ZERO,G2(:,:,:,9))
  call check_last_QA_V(l_switch,G2(:,:,:,9),wf(:,-4),G2tensor(:,9))
  call loop_VQ_A(G0(:,:,:,1),wf(:,302),G0(:,:,:,11))
  call loop_Q_A(G0(:,:,:,11),Q(:,47),ZERO,G1(:,:,:,19))
  call loop_QV_A(G1(:,:,:,19),wf(:,-6),G1(:,:,:,20))
  call loop_Q_A(G1(:,:,:,20),Q(:,111),ZERO,G2(:,:,:,10))
  call check_last_QA_V(l_switch,G2(:,:,:,10),wf(:,-4),G2tensor(:,10))
  call loop_VQ_A(G0(:,:,:,1),wf(:,306),G0(:,:,:,12))
  call loop_Q_A(G0(:,:,:,12),Q(:,47),ZERO,G1(:,:,:,21))
  call loop_QV_A(G1(:,:,:,21),wf(:,-6),G1(:,:,:,22))
  call loop_Q_A(G1(:,:,:,22),Q(:,111),ZERO,G2(:,:,:,11))
  call check_last_QA_V(l_switch,G2(:,:,:,11),wf(:,-4),G2tensor(:,11))
  call loop_VQ_A(G0(:,:,:,1),wf(:,310),G0(:,:,:,13))
  call loop_Q_A(G0(:,:,:,13),Q(:,47),ZERO,G1(:,:,:,23))
  call loop_QV_A(G1(:,:,:,23),wf(:,-6),G1(:,:,:,24))
  call loop_Q_A(G1(:,:,:,24),Q(:,111),ZERO,G2(:,:,:,12))
  call check_last_QA_V(l_switch,G2(:,:,:,12),wf(:,-4),G2tensor(:,12))
  call loop_VQ_A(G0(:,:,:,1),wf(:,316),G0(:,:,:,14))
  call loop_Q_A(G0(:,:,:,14),Q(:,47),ZERO,G1(:,:,:,25))
  call loop_QV_A(G1(:,:,:,25),wf(:,-6),G1(:,:,:,26))
  call loop_Q_A(G1(:,:,:,26),Q(:,111),ZERO,G2(:,:,:,13))
  call check_last_QA_V(l_switch,G2(:,:,:,13),wf(:,-4),G2tensor(:,13))
  call loop_VQ_A(G0(:,:,:,1),wf(:,318),G0(:,:,:,15))
  call loop_Q_A(G0(:,:,:,15),Q(:,47),ZERO,G1(:,:,:,27))
  call loop_QV_A(G1(:,:,:,27),wf(:,-6),G1(:,:,:,28))
  call loop_Q_A(G1(:,:,:,28),Q(:,111),ZERO,G2(:,:,:,14))
  call check_last_QA_V(l_switch,G2(:,:,:,14),wf(:,-4),G2tensor(:,14))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (-(c(15)*M(1))) * den(381)
  T2sum(1:15,22) = T2sum(1:15,22) + Gcoeff * G2tensor(:,1)
  Gcoeff = (-(c(11)*M(1))) * den(382)
  T2sum(1:15,26) = T2sum(1:15,26) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(15)*M(1))) * den(383)
  T2sum(1:15,26) = T2sum(1:15,26) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(10)*M(1)) * den(313)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(14)*M(1)) * den(315)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,5)
  Gcoeff = (-(c(14)*M(1))) * den(318)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,6)
  Gcoeff = (c(5)*M(1)) * den(319)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(14)*M(1)) * den(321)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(14)*M(1)) * den(324)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(14)*M(1)) * den(325)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(2)*M(1)) * den(327)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,11)
  Gcoeff = (-(c(14)*M(1))) * den(329)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,12)
  Gcoeff = (-(c(14)*M(1))) * den(332)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,13)
  Gcoeff = (-(c(14)*M(1))) * den(333)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,14)
  Gcoeff = (-(c(11)*M(1))) * den(236)
  T1sum(1:5,9) = T1sum(1:5,9) + Gcoeff * G1tensor(:,2)
  Gcoeff = (-(c(15)*M(1))) * den(238)
  T1sum(1:5,9) = T1sum(1:5,9) + Gcoeff * G1tensor(:,3)
  Gcoeff = (-(c(15)*M(1))) * den(248)
  T1sum(1:5,7) = T1sum(1:5,7) + Gcoeff * G1tensor(:,1)

end subroutine vamp_2

end module ol_vamp_2_ppllllj_neeexexuxdg_1_/**/REALKIND
