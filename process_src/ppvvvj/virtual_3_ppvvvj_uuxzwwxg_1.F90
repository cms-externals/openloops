
module ol_vamp_3_ppvvvj_uuxzwwxg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_3(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppvvvj_uuxzwwxg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppvvvj_uuxzwwxg_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppvvvj_uuxzwwxg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppvvvj_uuxzwwxg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,14) :: G0
  complex(REALKIND), dimension(4,5,4,26) :: G1
  complex(REALKIND), dimension(4,15,4,13) :: G2
  complex(REALKIND), dimension(5,10) :: G1tensor
  complex(REALKIND), dimension(15,13) :: G2tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_VQ_A(G0(:,:,:,1),wf(:,333),G0(:,:,:,2))
  call loop_Q_A(G0(:,:,:,2),Q(:,57),ZERO,G1(:,:,:,1))
  call loop_QZ_A(G1(:,:,:,1),wf(:,-2),G1(:,:,:,2),gZu)
  call loop_Q_A(G1(:,:,:,2),Q(:,61),ZERO,G2(:,:,:,1))
  call check_last_QA_V(l_switch,G2(:,:,:,1),wf(:,-1),G2tensor(:,1))
  call check_last_QA_V(l_switch,G1(:,:,:,1),wf(:,33),G1tensor(:,1))
  call loop_VQ_A(G0(:,:,:,1),wf(:,335),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,57),ZERO,G1(:,:,:,3))
  call loop_QZ_A(G1(:,:,:,3),wf(:,-2),G1(:,:,:,4),gZu)
  call loop_Q_A(G1(:,:,:,4),Q(:,61),ZERO,G2(:,:,:,2))
  call check_last_QA_V(l_switch,G2(:,:,:,2),wf(:,-1),G2tensor(:,2))
  call check_last_QA_V(l_switch,G1(:,:,:,3),wf(:,33),G1tensor(:,2))
  call loop_VA_Q(G0(:,:,:,1),wf(:,337),G0(:,:,:,4))
  call loop_A_Q(G0(:,:,:,4),Q(:,30),ZERO,G1(:,:,:,5))
  call loop_AV_Q(G1(:,:,:,5),wf(:,-5),G1(:,:,:,6))
  call loop_A_Q(G1(:,:,:,6),Q(:,62),ZERO,G2(:,:,:,3))
  call check_last_AQ_V(l_switch,G2(:,:,:,3),wf(:,0),G2tensor(:,3))
  call loop_VA_Q(G0(:,:,:,1),wf(:,339),G0(:,:,:,5))
  call loop_A_Q(G0(:,:,:,5),Q(:,30),ZERO,G1(:,:,:,7))
  call loop_AV_Q(G1(:,:,:,7),wf(:,-5),G1(:,:,:,8))
  call loop_A_Q(G1(:,:,:,8),Q(:,62),ZERO,G2(:,:,:,4))
  call check_last_AQ_V(l_switch,G2(:,:,:,4),wf(:,0),G2tensor(:,4))
  call loop_VA_Q(G0(:,:,:,1),wf(:,341),G0(:,:,:,6))
  call loop_A_Q(G0(:,:,:,6),Q(:,30),ZERO,G1(:,:,:,9))
  call loop_AV_Q(G1(:,:,:,9),wf(:,-5),G1(:,:,:,10))
  call loop_A_Q(G1(:,:,:,10),Q(:,62),ZERO,G2(:,:,:,5))
  call check_last_AQ_V(l_switch,G2(:,:,:,5),wf(:,0),G2tensor(:,5))
  call loop_VA_Q(G0(:,:,:,1),wf(:,343),G0(:,:,:,7))
  call loop_A_Q(G0(:,:,:,7),Q(:,58),ZERO,G1(:,:,:,11))
  call loop_AZ_Q(G1(:,:,:,11),wf(:,-2),G1(:,:,:,12),gZu)
  call loop_A_Q(G1(:,:,:,12),Q(:,62),ZERO,G2(:,:,:,6))
  call check_last_AQ_V(l_switch,G2(:,:,:,6),wf(:,0),G2tensor(:,6))
  call check_last_AQ_V(l_switch,G1(:,:,:,11),wf(:,13),G1tensor(:,3))
  call loop_VA_Q(G0(:,:,:,1),wf(:,345),G0(:,:,:,8))
  call loop_A_Q(G0(:,:,:,8),Q(:,58),ZERO,G1(:,:,:,13))
  call loop_AZ_Q(G1(:,:,:,13),wf(:,-2),G1(:,:,:,14),gZu)
  call loop_A_Q(G1(:,:,:,14),Q(:,62),ZERO,G2(:,:,:,7))
  call check_last_AQ_V(l_switch,G2(:,:,:,7),wf(:,0),G2tensor(:,7))
  call check_last_AQ_V(l_switch,G1(:,:,:,13),wf(:,13),G1tensor(:,4))
  call loop_VQ_A(G0(:,:,:,1),wf(:,347),G0(:,:,:,9))
  call loop_Q_A(G0(:,:,:,9),Q(:,45),ZERO,G1(:,:,:,15))
  call loop_QW_A(G1(:,:,:,15),wf(:,-4),G1(:,:,:,16))
  call loop_Q_A(G1(:,:,:,16),Q(:,61),ZERO,G2(:,:,:,8))
  call check_last_QA_V(l_switch,G2(:,:,:,8),wf(:,-1),G2tensor(:,8))
  call check_last_QA_V(l_switch,G1(:,:,:,15),wf(:,14),G1tensor(:,5))
  call loop_VQ_A(G0(:,:,:,1),wf(:,349),G0(:,:,:,10))
  call loop_Q_A(G0(:,:,:,10),Q(:,45),ZERO,G1(:,:,:,17))
  call loop_QW_A(G1(:,:,:,17),wf(:,-4),G1(:,:,:,18))
  call loop_Q_A(G1(:,:,:,18),Q(:,61),ZERO,G2(:,:,:,9))
  call check_last_QA_V(l_switch,G2(:,:,:,9),wf(:,-1),G2tensor(:,9))
  call check_last_QA_V(l_switch,G1(:,:,:,17),wf(:,14),G1tensor(:,6))
  call loop_VQ_A(G0(:,:,:,1),wf(:,351),G0(:,:,:,11))
  call loop_Q_A(G0(:,:,:,11),Q(:,57),ZERO,G1(:,:,:,19))
  call loop_QZ_A(G1(:,:,:,19),wf(:,-2),G1(:,:,:,20),gZu)
  call loop_Q_A(G1(:,:,:,20),Q(:,61),ZERO,G2(:,:,:,10))
  call check_last_QA_V(l_switch,G2(:,:,:,10),wf(:,-1),G2tensor(:,10))
  call check_last_QA_V(l_switch,G1(:,:,:,19),wf(:,33),G1tensor(:,7))
  call loop_VA_Q(G0(:,:,:,1),wf(:,353),G0(:,:,:,12))
  call loop_A_Q(G0(:,:,:,12),Q(:,54),ZERO,G1(:,:,:,21))
  call loop_AW_Q(G1(:,:,:,21),wf(:,-3),G1(:,:,:,22))
  call loop_A_Q(G1(:,:,:,22),Q(:,62),ZERO,G2(:,:,:,11))
  call check_last_AQ_V(l_switch,G2(:,:,:,11),wf(:,0),G2tensor(:,11))
  call check_last_AQ_V(l_switch,G1(:,:,:,21),wf(:,32),G1tensor(:,8))
  call loop_VA_Q(G0(:,:,:,1),wf(:,355),G0(:,:,:,13))
  call loop_A_Q(G0(:,:,:,13),Q(:,54),ZERO,G1(:,:,:,23))
  call loop_AW_Q(G1(:,:,:,23),wf(:,-3),G1(:,:,:,24))
  call loop_A_Q(G1(:,:,:,24),Q(:,62),ZERO,G2(:,:,:,12))
  call check_last_AQ_V(l_switch,G2(:,:,:,12),wf(:,0),G2tensor(:,12))
  call check_last_AQ_V(l_switch,G1(:,:,:,23),wf(:,32),G1tensor(:,9))
  call loop_VA_Q(G0(:,:,:,1),wf(:,357),G0(:,:,:,14))
  call loop_A_Q(G0(:,:,:,14),Q(:,58),ZERO,G1(:,:,:,25))
  call loop_AZ_Q(G1(:,:,:,25),wf(:,-2),G1(:,:,:,26),gZu)
  call loop_A_Q(G1(:,:,:,26),Q(:,62),ZERO,G2(:,:,:,13))
  call check_last_AQ_V(l_switch,G2(:,:,:,13),wf(:,0),G2tensor(:,13))
  call check_last_AQ_V(l_switch,G1(:,:,:,25),wf(:,13),G1tensor(:,10))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (-(c(3)*M(1))) * den(339)
  T2sum(1:15,32) = T2sum(1:15,32) + Gcoeff * G2tensor(:,1)
  Gcoeff = (-(c(21)*M(1))) * den(340)
  T2sum(1:15,32) = T2sum(1:15,32) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(2)*M(1)) * den(341)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(20)*M(1)) * den(342)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,4)
  Gcoeff = (-(c(14)*M(1))) * den(343)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,5)
  Gcoeff = (-(c(3)*M(1))) * den(344)
  T2sum(1:15,36) = T2sum(1:15,36) + Gcoeff * G2tensor(:,6)
  Gcoeff = (-(c(21)*M(1))) * den(345)
  T2sum(1:15,36) = T2sum(1:15,36) + Gcoeff * G2tensor(:,7)
  Gcoeff = (-(c(9)*M(1))) * den(346)
  T2sum(1:15,16) = T2sum(1:15,16) + Gcoeff * G2tensor(:,8)
  Gcoeff = (-(c(9)*M(1))) * den(347)
  T2sum(1:15,16) = T2sum(1:15,16) + Gcoeff * G2tensor(:,9)
  Gcoeff = (-(c(9)*M(1))) * den(348)
  T2sum(1:15,32) = T2sum(1:15,32) + Gcoeff * G2tensor(:,10)
  Gcoeff = (-(c(9)*M(1))) * den(349)
  T2sum(1:15,26) = T2sum(1:15,26) + Gcoeff * G2tensor(:,11)
  Gcoeff = (-(c(9)*M(1))) * den(350)
  T2sum(1:15,26) = T2sum(1:15,26) + Gcoeff * G2tensor(:,12)
  Gcoeff = (-(c(9)*M(1))) * den(351)
  T2sum(1:15,36) = T2sum(1:15,36) + Gcoeff * G2tensor(:,13)
  Gcoeff = (-(c(3)*M(1))) * den(372)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,3)
  Gcoeff = (-(c(21)*M(1))) * den(373)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,4)
  Gcoeff = (-(c(9)*M(1))) * den(375)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,10)
  Gcoeff = (-(c(3)*M(1))) * den(380)
  T1sum(1:5,8) = T1sum(1:5,8) + Gcoeff * G1tensor(:,1)
  Gcoeff = (-(c(21)*M(1))) * den(381)
  T1sum(1:5,8) = T1sum(1:5,8) + Gcoeff * G1tensor(:,2)
  Gcoeff = (-(c(9)*M(1))) * den(382)
  T1sum(1:5,8) = T1sum(1:5,8) + Gcoeff * G1tensor(:,7)
  Gcoeff = (-(c(9)*M(1))) * den(391)
  T1sum(1:5,10) = T1sum(1:5,10) + Gcoeff * G1tensor(:,8)
  Gcoeff = (-(c(9)*M(1))) * den(392)
  T1sum(1:5,10) = T1sum(1:5,10) + Gcoeff * G1tensor(:,9)
  Gcoeff = (-(c(9)*M(1))) * den(399)
  T1sum(1:5,4) = T1sum(1:5,4) + Gcoeff * G1tensor(:,5)
  Gcoeff = (-(c(9)*M(1))) * den(400)
  T1sum(1:5,4) = T1sum(1:5,4) + Gcoeff * G1tensor(:,6)

end subroutine vamp_3

end module ol_vamp_3_ppvvvj_uuxzwwxg_1_/**/REALKIND
