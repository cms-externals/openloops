
module ol_vamp_1_heftpphhj_ddxhhg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_1(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_heftpphhj_ddxhhg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_heftpphhj_ddxhhg_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_heftpphhj_ddxhhg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_heftpphhj_ddxhhg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,7) :: G0
  complex(REALKIND), dimension(4,5,4,18) :: G1
  complex(REALKIND), dimension(4,15,4,14) :: G2
  complex(REALKIND), dimension(4,35,4,16) :: G3
  complex(REALKIND), dimension(4,70,4,6) :: G4
  complex(REALKIND), dimension(15,22) :: G2tensor
  complex(REALKIND), dimension(35,4) :: G3tensor
  complex(REALKIND), dimension(70,8) :: G4tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_GHHG_G(G0(:,:,:,1),Q(:,0),wf(:,-3),wf(:,-2),wf(:,-4),Q(:,16),G1(:,:,:,1),Q(:,28))
  call loop_VA_Q(G1(:,:,:,1),wf(:,-1),G1(:,:,:,2))
  call loop_A_Q(G1(:,:,:,2),Q(:,30),ZERO,G2(:,:,:,1))
  call check_last_AQ_V(l_switch,G2(:,:,:,1),wf(:,0),G2tensor(:,1))
  call check_last_UV_W(l_switch,G1(:,:,:,1),Q(:,28),wf(:,1),Q(:,3),G2tensor(:,2))
  call loop_GHH_G(G0(:,:,:,1),Q(:,0),wf(:,-3),wf(:,-2),G2(:,:,:,2),Q(:,12))
  call loop_UV_W(G2(:,:,:,2),Q(:,12),wf(:,-4),Q(:,16),G3(:,:,:,1))
  call check_last_UV_W(l_switch,G3(:,:,:,1),Q(:,28),wf(:,1),Q(:,3),G4tensor(:,1))
  call loop_VA_Q(G3(:,:,:,1),wf(:,-1),G3(:,:,:,2))
  call loop_A_Q(G3(:,:,:,2),Q(:,30),ZERO,G4(:,:,:,1))
  call check_last_AQ_V(l_switch,G4(:,:,:,1),wf(:,0),G4tensor(:,2))
  call loop_VQ_A(G2(:,:,:,2),wf(:,9),G2(:,:,:,3))
  call loop_Q_A(G2(:,:,:,3),Q(:,29),ZERO,G3(:,:,:,3))
  call check_last_QA_V(l_switch,G3(:,:,:,3),wf(:,-1),G3tensor(:,1))
  call loop_VA_Q(G2(:,:,:,2),wf(:,13),G2(:,:,:,4))
  call loop_A_Q(G2(:,:,:,4),Q(:,30),ZERO,G3(:,:,:,4))
  call check_last_AQ_V(l_switch,G3(:,:,:,4),wf(:,0),G3tensor(:,2))
  call loop_VA_Q(G2(:,:,:,2),wf(:,-1),G2(:,:,:,5))
  call loop_A_Q(G2(:,:,:,5),Q(:,14),ZERO,G3(:,:,:,5))
  call loop_AV_Q(G3(:,:,:,5),wf(:,-4),G3(:,:,:,6))
  call loop_A_Q(G3(:,:,:,6),Q(:,30),ZERO,G4(:,:,:,2))
  call check_last_AQ_V(l_switch,G4(:,:,:,2),wf(:,0),G4tensor(:,3))
  call loop_GHG_G(G0(:,:,:,1),Q(:,0),wf(:,3),wf(:,-4),Q(:,16),G1(:,:,:,3),Q(:,28))
  call loop_VA_Q(G1(:,:,:,3),wf(:,-1),G1(:,:,:,4))
  call loop_A_Q(G1(:,:,:,4),Q(:,30),ZERO,G2(:,:,:,6))
  call check_last_AQ_V(l_switch,G2(:,:,:,6),wf(:,0),G2tensor(:,3))
  call check_last_UV_W(l_switch,G1(:,:,:,3),Q(:,28),wf(:,1),Q(:,3),G2tensor(:,4))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,-4),Q(:,16),G1(:,:,:,5))
  call loop_GHH_G(G1(:,:,:,5),Q(:,16),wf(:,-3),wf(:,-2),G3(:,:,:,7),Q(:,28))
  call loop_VA_Q(G3(:,:,:,7),wf(:,-1),G3(:,:,:,8))
  call loop_A_Q(G3(:,:,:,8),Q(:,30),ZERO,G4(:,:,:,3))
  call check_last_AQ_V(l_switch,G4(:,:,:,3),wf(:,0),G4tensor(:,4))
  call loop_GH_G(G1(:,:,:,5),Q(:,16),wf(:,3),G3(:,:,:,9),Q(:,28))
  call loop_VA_Q(G3(:,:,:,9),wf(:,-1),G3(:,:,:,10))
  call loop_A_Q(G3(:,:,:,10),Q(:,30),ZERO,G4(:,:,:,4))
  call check_last_AQ_V(l_switch,G4(:,:,:,4),wf(:,0),G4tensor(:,5))
  call check_last_GHHG_G(l_switch,G1(:,:,:,5),Q(:,16),wf(:,-3),wf(:,-2),wf(:,1),Q(:,3),G2tensor(:,5),Q(:,31))
  call check_last_GHG_G(l_switch,G1(:,:,:,5),Q(:,16),wf(:,3),wf(:,1),Q(:,3),G2tensor(:,6),Q(:,31))
  call loop_VA_Q(G0(:,:,:,1),wf(:,-1),G0(:,:,:,2))
  call loop_A_Q(G0(:,:,:,2),Q(:,2),ZERO,G1(:,:,:,6))
  call loop_AV_Q(G1(:,:,:,6),wf(:,2),G1(:,:,:,7))
  call loop_A_Q(G1(:,:,:,7),Q(:,30),ZERO,G2(:,:,:,7))
  call check_last_AQ_V(l_switch,G2(:,:,:,7),wf(:,0),G2tensor(:,7))
  call loop_AV_Q(G1(:,:,:,6),wf(:,28),G1(:,:,:,8))
  call loop_A_Q(G1(:,:,:,8),Q(:,30),ZERO,G2(:,:,:,8))
  call check_last_AQ_V(l_switch,G2(:,:,:,8),wf(:,0),G2tensor(:,8))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,2),Q(:,28),G1(:,:,:,9))
  call loop_VA_Q(G1(:,:,:,9),wf(:,-1),G1(:,:,:,10))
  call loop_A_Q(G1(:,:,:,10),Q(:,30),ZERO,G2(:,:,:,9))
  call check_last_AQ_V(l_switch,G2(:,:,:,9),wf(:,0),G2tensor(:,9))
  call check_last_UV_W(l_switch,G1(:,:,:,9),Q(:,28),wf(:,1),Q(:,3),G2tensor(:,10))
  call loop_GH_G(G0(:,:,:,1),Q(:,0),wf(:,3),G2(:,:,:,10),Q(:,12))
  call loop_UV_W(G2(:,:,:,10),Q(:,12),wf(:,-4),Q(:,16),G3(:,:,:,11))
  call check_last_UV_W(l_switch,G3(:,:,:,11),Q(:,28),wf(:,1),Q(:,3),G4tensor(:,6))
  call loop_VA_Q(G3(:,:,:,11),wf(:,-1),G3(:,:,:,12))
  call loop_A_Q(G3(:,:,:,12),Q(:,30),ZERO,G4(:,:,:,5))
  call check_last_AQ_V(l_switch,G4(:,:,:,5),wf(:,0),G4tensor(:,7))
  call loop_VQ_A(G2(:,:,:,10),wf(:,9),G2(:,:,:,11))
  call loop_Q_A(G2(:,:,:,11),Q(:,29),ZERO,G3(:,:,:,13))
  call check_last_QA_V(l_switch,G3(:,:,:,13),wf(:,-1),G3tensor(:,3))
  call loop_VA_Q(G2(:,:,:,10),wf(:,13),G2(:,:,:,12))
  call loop_A_Q(G2(:,:,:,12),Q(:,30),ZERO,G3(:,:,:,14))
  call check_last_AQ_V(l_switch,G3(:,:,:,14),wf(:,0),G3tensor(:,4))
  call loop_VA_Q(G2(:,:,:,10),wf(:,-1),G2(:,:,:,13))
  call loop_A_Q(G2(:,:,:,13),Q(:,14),ZERO,G3(:,:,:,15))
  call loop_AV_Q(G3(:,:,:,15),wf(:,-4),G3(:,:,:,16))
  call loop_A_Q(G3(:,:,:,16),Q(:,30),ZERO,G4(:,:,:,6))
  call check_last_AQ_V(l_switch,G4(:,:,:,6),wf(:,0),G4tensor(:,8))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,28),Q(:,28),G1(:,:,:,11))
  call loop_VA_Q(G1(:,:,:,11),wf(:,-1),G1(:,:,:,12))
  call loop_A_Q(G1(:,:,:,12),Q(:,30),ZERO,G2(:,:,:,14))
  call check_last_AQ_V(l_switch,G2(:,:,:,14),wf(:,0),G2tensor(:,11))
  call check_last_UV_W(l_switch,G1(:,:,:,11),Q(:,28),wf(:,1),Q(:,3),G2tensor(:,12))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,1),G0(:,:,:,3))
  call check_last_GHH_G(l_switch,G0(:,:,:,3),Q(:,19),wf(:,-3),wf(:,-2),G2tensor(:,13),Q(:,31))
  call check_last_GH_G(l_switch,G0(:,:,:,3),Q(:,19),wf(:,3),G2tensor(:,14),Q(:,31))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,1),wf(:,-4),G0(:,:,:,4))
  call check_last_GHH_G(l_switch,G0(:,:,:,4),Q(:,19),wf(:,-3),wf(:,-2),G2tensor(:,15),Q(:,31))
  call check_last_GH_G(l_switch,G0(:,:,:,4),Q(:,19),wf(:,3),G2tensor(:,16),Q(:,31))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,1),G0(:,:,:,5))
  call check_last_GHH_G(l_switch,G0(:,:,:,5),Q(:,19),wf(:,-3),wf(:,-2),G2tensor(:,17),Q(:,31))
  call check_last_GH_G(l_switch,G0(:,:,:,5),Q(:,19),wf(:,3),G2tensor(:,18),Q(:,31))
  call loop_QV_A(G0(:,:,:,1),wf(:,2),G0(:,:,:,6))
  call loop_Q_A(G0(:,:,:,6),Q(:,28),ZERO,G1(:,:,:,13))
  call loop_QV_A(G1(:,:,:,13),wf(:,1),G1(:,:,:,14))
  call check_last_Q_A(l_switch,G1(:,:,:,14),Q(:,31),ZERO,G2tensor(:,19))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,2),Q(:,28),G1(:,:,:,15))
  call check_last_CV_D(l_switch,G1(:,:,:,15),Q(:,28),wf(:,1),Q(:,3),G2tensor(:,20))
  call loop_QV_A(G0(:,:,:,1),wf(:,28),G0(:,:,:,7))
  call loop_Q_A(G0(:,:,:,7),Q(:,28),ZERO,G1(:,:,:,16))
  call loop_QV_A(G1(:,:,:,16),wf(:,1),G1(:,:,:,17))
  call check_last_Q_A(l_switch,G1(:,:,:,17),Q(:,31),ZERO,G2tensor(:,21))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,28),Q(:,28),G1(:,:,:,18))
  call check_last_CV_D(l_switch,G1(:,:,:,18),Q(:,28),wf(:,1),Q(:,3),G2tensor(:,22))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(1)*M(1))
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(5)*M(1)) * den(1)
  T4sum(1:70,1) = T4sum(1:70,1) + Gcoeff * G4tensor(:,1)
  Gcoeff = (-(c(6)*M(1))) * den(2)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(4)*M(1)) * den(4)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,1)
  Gcoeff = (c(4)*M(1)) * den(6)
  T3sum(1:35,2) = T3sum(1:35,2) + Gcoeff * G3tensor(:,2)
  Gcoeff = (c(1)*M(1))
  T4sum(1:70,2) = T4sum(1:70,2) + Gcoeff * G4tensor(:,4)
  Gcoeff = (c(1)*M(1))
  T4sum(1:70,3) = T4sum(1:70,3) + Gcoeff * G4tensor(:,2)
  Gcoeff = (-(c(3)*M(1)))
  T4sum(1:70,4) = T4sum(1:70,4) + Gcoeff * G4tensor(:,3)
  Gcoeff = (-(c(3)*M(1))) * den(5)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(1)*M(1)) * den(5)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,9)
  Gcoeff = (-(c(10)*M(1))) * den(3)
  T4sum(1:70,1) = T4sum(1:70,1) + Gcoeff * G4tensor(:,6)
  Gcoeff = (-(c(9)*M(1))) * den(12)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,3)
  Gcoeff = (-(c(9)*M(1))) * den(13)
  T3sum(1:35,2) = T3sum(1:35,2) + Gcoeff * G3tensor(:,4)
  Gcoeff = (c(8)*M(1)) * den(14)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,8)
  Gcoeff = (-(c(6)*M(1))) * den(14)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,11)
  Gcoeff = (-(c(6)*M(1))) * den(2)
  T4sum(1:70,2) = T4sum(1:70,2) + Gcoeff * G4tensor(:,5)
  Gcoeff = (-(c(6)*M(1))) * den(2)
  T4sum(1:70,3) = T4sum(1:70,3) + Gcoeff * G4tensor(:,7)
  Gcoeff = (c(8)*M(1)) * den(2)
  T4sum(1:70,4) = T4sum(1:70,4) + Gcoeff * G4tensor(:,8)
  Gcoeff = (c(2)*M(1)) * den(1)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,13)
  Gcoeff = (c(2)*M(1)) * den(1)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,15)
  Gcoeff = (0) * den(1)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,17)
  Gcoeff = (c(2)*M(1)) * den(1)
  T2sum(1:15,4) = T2sum(1:15,4) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(2)*M(1)) * den(1)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(7)*M(1))) * den(3)
  T2sum(1:15,4) = T2sum(1:15,4) + Gcoeff * G2tensor(:,6)
  Gcoeff = (-(c(7)*M(1))) * den(3)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,4)
  Gcoeff = (-(c(7)*M(1))) * den(3)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,14)
  Gcoeff = (-(c(7)*M(1))) * den(3)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,16)
  Gcoeff = (0) * den(3)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,18)
  Gcoeff = (-(c(11)*M(1))) * den(11)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,19)
  Gcoeff = (-(c(12)*M(1))) * den(11)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,19)
  Gcoeff = (-(c(5)*M(1))) * den(11)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,20)
  Gcoeff = (c(2)*M(1)) * den(11)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(13)*M(1)) * den(15)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,21)
  Gcoeff = (c(14)*M(1)) * den(15)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,21)
  Gcoeff = (c(10)*M(1)) * den(15)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,22)
  Gcoeff = (-(c(7)*M(1))) * den(15)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,12)

end subroutine vamp_1

end module ol_vamp_1_heftpphhj_ddxhhg_1_/**/REALKIND
