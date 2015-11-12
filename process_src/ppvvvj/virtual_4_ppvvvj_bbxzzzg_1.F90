
module ol_vamp_4_ppvvvj_bbxzzzg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_4(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppvvvj_bbxzzzg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppvvvj_bbxzzzg_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppvvvj_bbxzzzg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppvvvj_bbxzzzg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,17) :: G0
  complex(REALKIND), dimension(4,5,4,32) :: G1
  complex(REALKIND), dimension(4,15,4,16) :: G2
  complex(REALKIND), dimension(5,14) :: G1tensor
  complex(REALKIND), dimension(15,16) :: G2tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_VQ_A(G0(:,:,:,1),wf(:,448),G0(:,:,:,2))
  call loop_Q_A(G0(:,:,:,2),Q(:,57),MB,G1(:,:,:,1))
  call loop_QZ_A(G1(:,:,:,1),wf(:,-2),G1(:,:,:,2),gZd)
  call loop_Q_A(G1(:,:,:,2),Q(:,61),MB,G2(:,:,:,1))
  call check_last_QA_V(l_switch,G2(:,:,:,1),wf(:,-1),G2tensor(:,1))
  call check_last_QA_V(l_switch,G1(:,:,:,1),wf(:,30),G1tensor(:,1))
  call loop_VA_Q(G0(:,:,:,1),wf(:,450),G0(:,:,:,3))
  call loop_A_Q(G0(:,:,:,3),Q(:,30),MB,G1(:,:,:,3))
  call loop_AV_Q(G1(:,:,:,3),wf(:,-5),G1(:,:,:,4))
  call loop_A_Q(G1(:,:,:,4),Q(:,62),MB,G2(:,:,:,2))
  call check_last_AQ_V(l_switch,G2(:,:,:,2),wf(:,0),G2tensor(:,2))
  call loop_VA_Q(G0(:,:,:,1),wf(:,452),G0(:,:,:,4))
  call loop_A_Q(G0(:,:,:,4),Q(:,30),MB,G1(:,:,:,5))
  call loop_AV_Q(G1(:,:,:,5),wf(:,-5),G1(:,:,:,6))
  call loop_A_Q(G1(:,:,:,6),Q(:,62),MB,G2(:,:,:,3))
  call check_last_AQ_V(l_switch,G2(:,:,:,3),wf(:,0),G2tensor(:,3))
  call loop_VA_Q(G0(:,:,:,1),wf(:,454),G0(:,:,:,5))
  call loop_A_Q(G0(:,:,:,5),Q(:,58),MB,G1(:,:,:,7))
  call loop_AZ_Q(G1(:,:,:,7),wf(:,-2),G1(:,:,:,8),gZd)
  call loop_A_Q(G1(:,:,:,8),Q(:,62),MB,G2(:,:,:,4))
  call check_last_AQ_V(l_switch,G2(:,:,:,4),wf(:,0),G2tensor(:,4))
  call check_last_AQ_V(l_switch,G1(:,:,:,7),wf(:,3),G1tensor(:,2))
  call loop_VQ_A(G0(:,:,:,1),wf(:,456),G0(:,:,:,6))
  call loop_Q_A(G0(:,:,:,6),Q(:,45),MB,G1(:,:,:,9))
  call loop_QZ_A(G1(:,:,:,9),wf(:,-4),G1(:,:,:,10),gZd)
  call loop_Q_A(G1(:,:,:,10),Q(:,61),MB,G2(:,:,:,5))
  call check_last_QA_V(l_switch,G2(:,:,:,5),wf(:,-1),G2tensor(:,5))
  call check_last_QA_V(l_switch,G1(:,:,:,9),wf(:,12),G1tensor(:,3))
  call loop_VQ_A(G0(:,:,:,1),wf(:,458),G0(:,:,:,7))
  call loop_Q_A(G0(:,:,:,7),Q(:,53),MB,G1(:,:,:,11))
  call loop_QZ_A(G1(:,:,:,11),wf(:,-3),G1(:,:,:,12),gZd)
  call loop_Q_A(G1(:,:,:,12),Q(:,61),MB,G2(:,:,:,6))
  call check_last_QA_V(l_switch,G2(:,:,:,6),wf(:,-1),G2tensor(:,6))
  call check_last_QA_V(l_switch,G1(:,:,:,11),wf(:,4),G1tensor(:,4))
  call loop_VQ_A(G0(:,:,:,1),wf(:,460),G0(:,:,:,8))
  call loop_Q_A(G0(:,:,:,8),Q(:,45),MB,G1(:,:,:,13))
  call loop_QZ_A(G1(:,:,:,13),wf(:,-4),G1(:,:,:,14),gZd)
  call loop_Q_A(G1(:,:,:,14),Q(:,61),MB,G2(:,:,:,7))
  call check_last_QA_V(l_switch,G2(:,:,:,7),wf(:,-1),G2tensor(:,7))
  call check_last_QA_V(l_switch,G1(:,:,:,13),wf(:,12),G1tensor(:,5))
  call loop_VQ_A(G0(:,:,:,1),wf(:,462),G0(:,:,:,9))
  call loop_Q_A(G0(:,:,:,9),Q(:,57),MB,G1(:,:,:,15))
  call loop_QZ_A(G1(:,:,:,15),wf(:,-2),G1(:,:,:,16),gZd)
  call loop_Q_A(G1(:,:,:,16),Q(:,61),MB,G2(:,:,:,8))
  call check_last_QA_V(l_switch,G2(:,:,:,8),wf(:,-1),G2tensor(:,8))
  call check_last_QA_V(l_switch,G1(:,:,:,15),wf(:,30),G1tensor(:,6))
  call loop_VQ_A(G0(:,:,:,1),wf(:,464),G0(:,:,:,10))
  call loop_Q_A(G0(:,:,:,10),Q(:,53),MB,G1(:,:,:,17))
  call loop_QZ_A(G1(:,:,:,17),wf(:,-3),G1(:,:,:,18),gZd)
  call loop_Q_A(G1(:,:,:,18),Q(:,61),MB,G2(:,:,:,9))
  call check_last_QA_V(l_switch,G2(:,:,:,9),wf(:,-1),G2tensor(:,9))
  call check_last_QA_V(l_switch,G1(:,:,:,17),wf(:,4),G1tensor(:,7))
  call loop_VQ_A(G0(:,:,:,1),wf(:,466),G0(:,:,:,11))
  call loop_Q_A(G0(:,:,:,11),Q(:,57),MB,G1(:,:,:,19))
  call loop_QZ_A(G1(:,:,:,19),wf(:,-2),G1(:,:,:,20),gZd)
  call loop_Q_A(G1(:,:,:,20),Q(:,61),MB,G2(:,:,:,10))
  call check_last_QA_V(l_switch,G2(:,:,:,10),wf(:,-1),G2tensor(:,10))
  call check_last_QA_V(l_switch,G1(:,:,:,19),wf(:,30),G1tensor(:,8))
  call loop_VA_Q(G0(:,:,:,1),wf(:,468),G0(:,:,:,12))
  call loop_A_Q(G0(:,:,:,12),Q(:,46),MB,G1(:,:,:,21))
  call loop_AZ_Q(G1(:,:,:,21),wf(:,-4),G1(:,:,:,22),gZd)
  call loop_A_Q(G1(:,:,:,22),Q(:,62),MB,G2(:,:,:,11))
  call check_last_AQ_V(l_switch,G2(:,:,:,11),wf(:,0),G2tensor(:,11))
  call check_last_AQ_V(l_switch,G1(:,:,:,21),wf(:,38),G1tensor(:,9))
  call loop_VA_Q(G0(:,:,:,1),wf(:,470),G0(:,:,:,13))
  call loop_A_Q(G0(:,:,:,13),Q(:,54),MB,G1(:,:,:,23))
  call loop_AZ_Q(G1(:,:,:,23),wf(:,-3),G1(:,:,:,24),gZd)
  call loop_A_Q(G1(:,:,:,24),Q(:,62),MB,G2(:,:,:,12))
  call check_last_AQ_V(l_switch,G2(:,:,:,12),wf(:,0),G2tensor(:,12))
  call check_last_AQ_V(l_switch,G1(:,:,:,23),wf(:,29),G1tensor(:,10))
  call loop_VA_Q(G0(:,:,:,1),wf(:,472),G0(:,:,:,14))
  call loop_A_Q(G0(:,:,:,14),Q(:,46),MB,G1(:,:,:,25))
  call loop_AZ_Q(G1(:,:,:,25),wf(:,-4),G1(:,:,:,26),gZd)
  call loop_A_Q(G1(:,:,:,26),Q(:,62),MB,G2(:,:,:,13))
  call check_last_AQ_V(l_switch,G2(:,:,:,13),wf(:,0),G2tensor(:,13))
  call check_last_AQ_V(l_switch,G1(:,:,:,25),wf(:,38),G1tensor(:,11))
  call loop_VA_Q(G0(:,:,:,1),wf(:,474),G0(:,:,:,15))
  call loop_A_Q(G0(:,:,:,15),Q(:,58),MB,G1(:,:,:,27))
  call loop_AZ_Q(G1(:,:,:,27),wf(:,-2),G1(:,:,:,28),gZd)
  call loop_A_Q(G1(:,:,:,28),Q(:,62),MB,G2(:,:,:,14))
  call check_last_AQ_V(l_switch,G2(:,:,:,14),wf(:,0),G2tensor(:,14))
  call check_last_AQ_V(l_switch,G1(:,:,:,27),wf(:,3),G1tensor(:,12))
  call loop_VA_Q(G0(:,:,:,1),wf(:,476),G0(:,:,:,16))
  call loop_A_Q(G0(:,:,:,16),Q(:,54),MB,G1(:,:,:,29))
  call loop_AZ_Q(G1(:,:,:,29),wf(:,-3),G1(:,:,:,30),gZd)
  call loop_A_Q(G1(:,:,:,30),Q(:,62),MB,G2(:,:,:,15))
  call check_last_AQ_V(l_switch,G2(:,:,:,15),wf(:,0),G2tensor(:,15))
  call check_last_AQ_V(l_switch,G1(:,:,:,29),wf(:,29),G1tensor(:,13))
  call loop_VA_Q(G0(:,:,:,1),wf(:,478),G0(:,:,:,17))
  call loop_A_Q(G0(:,:,:,17),Q(:,58),MB,G1(:,:,:,31))
  call loop_AZ_Q(G1(:,:,:,31),wf(:,-2),G1(:,:,:,32),gZd)
  call loop_A_Q(G1(:,:,:,32),Q(:,62),MB,G2(:,:,:,16))
  call check_last_AQ_V(l_switch,G2(:,:,:,16),wf(:,0),G2tensor(:,16))
  call check_last_AQ_V(l_switch,G1(:,:,:,31),wf(:,3),G1tensor(:,14))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(6)*M(1)) * den(455)
  T2sum(1:15,42) = T2sum(1:15,42) + Gcoeff * G2tensor(:,1)
  Gcoeff = (-(c(5)*M(1))) * den(456)
  T2sum(1:15,31) = T2sum(1:15,31) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(8)*M(1))) * den(457)
  T2sum(1:15,31) = T2sum(1:15,31) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(6)*M(1)) * den(458)
  T2sum(1:15,48) = T2sum(1:15,48) + Gcoeff * G2tensor(:,4)
  Gcoeff = (-(c(3)*M(1))) * den(459)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,5)
  Gcoeff = (-(c(3)*M(1))) * den(460)
  T2sum(1:15,18) = T2sum(1:15,18) + Gcoeff * G2tensor(:,6)
  Gcoeff = (-(c(3)*M(1))) * den(461)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,7)
  Gcoeff = (-(c(3)*M(1))) * den(462)
  T2sum(1:15,42) = T2sum(1:15,42) + Gcoeff * G2tensor(:,8)
  Gcoeff = (-(c(3)*M(1))) * den(463)
  T2sum(1:15,18) = T2sum(1:15,18) + Gcoeff * G2tensor(:,9)
  Gcoeff = (-(c(3)*M(1))) * den(464)
  T2sum(1:15,42) = T2sum(1:15,42) + Gcoeff * G2tensor(:,10)
  Gcoeff = (-(c(3)*M(1))) * den(465)
  T2sum(1:15,35) = T2sum(1:15,35) + Gcoeff * G2tensor(:,11)
  Gcoeff = (-(c(3)*M(1))) * den(466)
  T2sum(1:15,36) = T2sum(1:15,36) + Gcoeff * G2tensor(:,12)
  Gcoeff = (-(c(3)*M(1))) * den(467)
  T2sum(1:15,35) = T2sum(1:15,35) + Gcoeff * G2tensor(:,13)
  Gcoeff = (-(c(3)*M(1))) * den(468)
  T2sum(1:15,48) = T2sum(1:15,48) + Gcoeff * G2tensor(:,14)
  Gcoeff = (-(c(3)*M(1))) * den(469)
  T2sum(1:15,36) = T2sum(1:15,36) + Gcoeff * G2tensor(:,15)
  Gcoeff = (-(c(3)*M(1))) * den(470)
  T2sum(1:15,48) = T2sum(1:15,48) + Gcoeff * G2tensor(:,16)
  Gcoeff = (c(6)*M(1)) * den(488)
  T1sum(1:5,4) = T1sum(1:5,4) + Gcoeff * G1tensor(:,2)
  Gcoeff = (-(c(3)*M(1))) * den(491)
  T1sum(1:5,4) = T1sum(1:5,4) + Gcoeff * G1tensor(:,12)
  Gcoeff = (-(c(3)*M(1))) * den(492)
  T1sum(1:5,4) = T1sum(1:5,4) + Gcoeff * G1tensor(:,14)
  Gcoeff = (c(6)*M(1)) * den(501)
  T1sum(1:5,9) = T1sum(1:5,9) + Gcoeff * G1tensor(:,1)
  Gcoeff = (-(c(3)*M(1))) * den(502)
  T1sum(1:5,9) = T1sum(1:5,9) + Gcoeff * G1tensor(:,6)
  Gcoeff = (-(c(3)*M(1))) * den(503)
  T1sum(1:5,9) = T1sum(1:5,9) + Gcoeff * G1tensor(:,8)
  Gcoeff = (-(c(3)*M(1))) * den(513)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,10)
  Gcoeff = (-(c(3)*M(1))) * den(514)
  T1sum(1:5,11) = T1sum(1:5,11) + Gcoeff * G1tensor(:,13)
  Gcoeff = (-(c(3)*M(1))) * den(520)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,4)
  Gcoeff = (-(c(3)*M(1))) * den(521)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,7)
  Gcoeff = (-(c(3)*M(1))) * den(532)
  T1sum(1:5,13) = T1sum(1:5,13) + Gcoeff * G1tensor(:,9)
  Gcoeff = (-(c(3)*M(1))) * den(533)
  T1sum(1:5,13) = T1sum(1:5,13) + Gcoeff * G1tensor(:,11)
  Gcoeff = (-(c(3)*M(1))) * den(534)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,3)
  Gcoeff = (-(c(3)*M(1))) * den(535)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,5)

end subroutine vamp_4

end module ol_vamp_4_ppvvvj_bbxzzzg_1_/**/REALKIND
