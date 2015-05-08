
module ol_vamp_1_ppjj_dddxdx_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_1(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppjj_dddxdx_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppjj_dddxdx_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppjj_dddxdx_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppjj_dddxdx_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(2)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,10) :: G0
  complex(REALKIND), dimension(4,5,4,37) :: G1
  complex(REALKIND), dimension(4,15,4,12) :: G2
  complex(REALKIND), dimension(15,22) :: G2tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_VA_Q(G0(:,:,:,1),wf(:,-3),G0(:,:,:,2))
  call loop_A_Q(G0(:,:,:,2),Q(:,8),ZERO,G1(:,:,:,1))
  call loop_AV_Q(G1(:,:,:,1),wf(:,1),G1(:,:,:,2))
  call loop_A_Q(G1(:,:,:,2),Q(:,13),ZERO,G2(:,:,:,1))
  call check_last_AQ_V(l_switch,G2(:,:,:,1),wf(:,-1),G2tensor(:,1))
  call loop_AV_Q(G1(:,:,:,1),wf(:,4),G1(:,:,:,3))
  call loop_A_Q(G1(:,:,:,3),Q(:,14),ZERO,G2(:,:,:,2))
  call check_last_AQ_V(l_switch,G2(:,:,:,2),wf(:,0),G2tensor(:,2))
  call loop_AQ_V(G1(:,:,:,1),wf(:,-1),G1(:,:,:,4))
  call loop_VA_Q(G1(:,:,:,4),wf(:,-2),G1(:,:,:,5))
  call loop_A_Q(G1(:,:,:,5),Q(:,14),ZERO,G2(:,:,:,3))
  call check_last_AQ_V(l_switch,G2(:,:,:,3),wf(:,0),G2tensor(:,3))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1),Q(:,5),G1(:,:,:,6))
  call loop_VA_Q(G1(:,:,:,6),wf(:,-3),G1(:,:,:,7))
  call loop_A_Q(G1(:,:,:,7),Q(:,13),ZERO,G2(:,:,:,4))
  call check_last_AQ_V(l_switch,G2(:,:,:,4),wf(:,-1),G2tensor(:,4))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,4),Q(:,6),G1(:,:,:,8))
  call loop_VA_Q(G1(:,:,:,8),wf(:,-3),G1(:,:,:,9))
  call loop_A_Q(G1(:,:,:,9),Q(:,14),ZERO,G2(:,:,:,5))
  call check_last_AQ_V(l_switch,G2(:,:,:,5),wf(:,0),G2tensor(:,5))
  call loop_VA_Q(G0(:,:,:,1),wf(:,-2),G0(:,:,:,3))
  call loop_A_Q(G0(:,:,:,3),Q(:,4),ZERO,G1(:,:,:,10))
  call loop_AV_Q(G1(:,:,:,10),wf(:,3),G1(:,:,:,11))
  call loop_A_Q(G1(:,:,:,11),Q(:,13),ZERO,G2(:,:,:,6))
  call check_last_AQ_V(l_switch,G2(:,:,:,6),wf(:,-1),G2tensor(:,6))
  call loop_AV_Q(G1(:,:,:,10),wf(:,2),G1(:,:,:,12))
  call loop_A_Q(G1(:,:,:,12),Q(:,14),ZERO,G2(:,:,:,7))
  call check_last_AQ_V(l_switch,G2(:,:,:,7),wf(:,0),G2tensor(:,7))
  call loop_AQ_V(G1(:,:,:,10),wf(:,-1),G1(:,:,:,13))
  call loop_VA_Q(G1(:,:,:,13),wf(:,-3),G1(:,:,:,14))
  call loop_A_Q(G1(:,:,:,14),Q(:,14),ZERO,G2(:,:,:,8))
  call check_last_AQ_V(l_switch,G2(:,:,:,8),wf(:,0),G2tensor(:,8))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,3),Q(:,9),G1(:,:,:,15))
  call loop_VA_Q(G1(:,:,:,15),wf(:,-2),G1(:,:,:,16))
  call loop_A_Q(G1(:,:,:,16),Q(:,13),ZERO,G2(:,:,:,9))
  call check_last_AQ_V(l_switch,G2(:,:,:,9),wf(:,-1),G2tensor(:,9))
  call check_last_UV_W(l_switch,G1(:,:,:,15),Q(:,9),wf(:,4),Q(:,6),G2tensor(:,10))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,2),Q(:,10),G1(:,:,:,17))
  call loop_VA_Q(G1(:,:,:,17),wf(:,-2),G1(:,:,:,18))
  call loop_A_Q(G1(:,:,:,18),Q(:,14),ZERO,G2(:,:,:,10))
  call check_last_AQ_V(l_switch,G2(:,:,:,10),wf(:,0),G2tensor(:,11))
  call check_last_UV_W(l_switch,G1(:,:,:,17),Q(:,10),wf(:,1),Q(:,5),G2tensor(:,12))
  call loop_VQ_A(G0(:,:,:,1),wf(:,-1),G0(:,:,:,4))
  call loop_Q_A(G0(:,:,:,4),Q(:,2),ZERO,G1(:,:,:,19))
  call loop_QA_V(G1(:,:,:,19),wf(:,-3),G1(:,:,:,20))
  call loop_VA_Q(G1(:,:,:,20),wf(:,-2),G1(:,:,:,21))
  call loop_A_Q(G1(:,:,:,21),Q(:,14),ZERO,G2(:,:,:,11))
  call check_last_AQ_V(l_switch,G2(:,:,:,11),wf(:,0),G2tensor(:,13))
  call loop_QA_V(G1(:,:,:,19),wf(:,-2),G1(:,:,:,22))
  call loop_VA_Q(G1(:,:,:,22),wf(:,-3),G1(:,:,:,23))
  call loop_A_Q(G1(:,:,:,23),Q(:,14),ZERO,G2(:,:,:,12))
  call check_last_AQ_V(l_switch,G2(:,:,:,12),wf(:,0),G2tensor(:,14))
  call loop_QV_A(G0(:,:,:,1),wf(:,2),G0(:,:,:,5))
  call loop_Q_A(G0(:,:,:,5),Q(:,10),ZERO,G1(:,:,:,24))
  call loop_QV_A(G1(:,:,:,24),wf(:,1),G1(:,:,:,25))
  call check_last_Q_A(l_switch,G1(:,:,:,25),Q(:,15),ZERO,G2tensor(:,15))
  call loop_QV_A(G0(:,:,:,1),wf(:,2),G0(:,:,:,6))
  call loop_Q_A(G0(:,:,:,6),Q(:,10),MT,G1(:,:,:,26))
  call loop_QV_A(G1(:,:,:,26),wf(:,1),G1(:,:,:,27))
  call check_last_Q_A(l_switch,G1(:,:,:,27),Q(:,15),MT,G2tensor(:,16))
  call loop_QV_A(G0(:,:,:,1),wf(:,2),G0(:,:,:,7))
  call loop_Q_A(G0(:,:,:,7),Q(:,10),MB,G1(:,:,:,28))
  call loop_QV_A(G1(:,:,:,28),wf(:,1),G1(:,:,:,29))
  call check_last_Q_A(l_switch,G1(:,:,:,29),Q(:,15),MB,G2tensor(:,17))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,2),Q(:,10),G1(:,:,:,30))
  call check_last_CV_D(l_switch,G1(:,:,:,30),Q(:,10),wf(:,1),Q(:,5),G2tensor(:,18))
  call loop_QV_A(G0(:,:,:,1),wf(:,3),G0(:,:,:,8))
  call loop_Q_A(G0(:,:,:,8),Q(:,9),ZERO,G1(:,:,:,31))
  call loop_QV_A(G1(:,:,:,31),wf(:,4),G1(:,:,:,32))
  call check_last_Q_A(l_switch,G1(:,:,:,32),Q(:,15),ZERO,G2tensor(:,19))
  call loop_QV_A(G0(:,:,:,1),wf(:,3),G0(:,:,:,9))
  call loop_Q_A(G0(:,:,:,9),Q(:,9),MT,G1(:,:,:,33))
  call loop_QV_A(G1(:,:,:,33),wf(:,4),G1(:,:,:,34))
  call check_last_Q_A(l_switch,G1(:,:,:,34),Q(:,15),MT,G2tensor(:,20))
  call loop_QV_A(G0(:,:,:,1),wf(:,3),G0(:,:,:,10))
  call loop_Q_A(G0(:,:,:,10),Q(:,9),MB,G1(:,:,:,35))
  call loop_QV_A(G1(:,:,:,35),wf(:,4),G1(:,:,:,36))
  call check_last_Q_A(l_switch,G1(:,:,:,36),Q(:,15),MB,G2tensor(:,21))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,3),Q(:,9),G1(:,:,:,37))
  call check_last_CV_D(l_switch,G1(:,:,:,37),Q(:,9),wf(:,4),Q(:,6),G2tensor(:,22))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(6)*M(1)-c(5)*M(2)) * den(1)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,1)
  Gcoeff = (-(c(2)*M(1))+c(1)*M(2)) * den(1)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(5)*M(1)-c(6)*M(2)) * den(3)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(1)*M(1))+c(2)*M(2)) * den(3)
  T2sum(1:15,4) = T2sum(1:15,4) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(5)*M(1)-c(6)*M(2)) * den(2)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,6)
  Gcoeff = (-(c(1)*M(1))+c(2)*M(2)) * den(2)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(6)*M(1)-c(5)*M(2)) * den(4)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,7)
  Gcoeff = (-(c(2)*M(1))+c(1)*M(2)) * den(4)
  T2sum(1:15,8) = T2sum(1:15,8) + Gcoeff * G2tensor(:,11)
  Gcoeff = (c(7)*M(1)-c(8)*M(2))
  T2sum(1:15,9) = T2sum(1:15,9) + Gcoeff * G2tensor(:,13)
  Gcoeff = (c(8)*M(1)-c(7)*M(2))
  T2sum(1:15,10) = T2sum(1:15,10) + Gcoeff * G2tensor(:,14)
  Gcoeff = (-(c(10)*M(1))-c(5)*M(2))
  T2sum(1:15,11) = T2sum(1:15,11) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(5)*M(1)+c(10)*M(2))
  T2sum(1:15,12) = T2sum(1:15,12) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(15)*M(1)-c(14)*M(2)) * den(5)
  T2sum(1:15,13) = T2sum(1:15,13) + Gcoeff * G2tensor(:,15)
  Gcoeff = (c(13)*M(1)-c(12)*M(2)) * den(5)
  T2sum(1:15,14) = T2sum(1:15,14) + Gcoeff * G2tensor(:,16)
  Gcoeff = (c(15)*M(1)-c(14)*M(2)) * den(5)
  T2sum(1:15,13) = T2sum(1:15,13) + Gcoeff * G2tensor(:,15)
  Gcoeff = (c(13)*M(1)-c(12)*M(2)) * den(5)
  T2sum(1:15,15) = T2sum(1:15,15) + Gcoeff * G2tensor(:,17)
  Gcoeff = (c(11)*M(1)-c(9)*M(2)) * den(5)
  T2sum(1:15,13) = T2sum(1:15,13) + Gcoeff * G2tensor(:,18)
  Gcoeff = (-(c(4)*M(1))+c(3)*M(2)) * den(5)
  T2sum(1:15,13) = T2sum(1:15,13) + Gcoeff * G2tensor(:,12)
  Gcoeff = (c(14)*M(1)-c(15)*M(2)) * den(6)
  T2sum(1:15,16) = T2sum(1:15,16) + Gcoeff * G2tensor(:,19)
  Gcoeff = (c(12)*M(1)-c(13)*M(2)) * den(6)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,20)
  Gcoeff = (c(14)*M(1)-c(15)*M(2)) * den(6)
  T2sum(1:15,16) = T2sum(1:15,16) + Gcoeff * G2tensor(:,19)
  Gcoeff = (c(12)*M(1)-c(13)*M(2)) * den(6)
  T2sum(1:15,18) = T2sum(1:15,18) + Gcoeff * G2tensor(:,21)
  Gcoeff = (c(9)*M(1)-c(11)*M(2)) * den(6)
  T2sum(1:15,16) = T2sum(1:15,16) + Gcoeff * G2tensor(:,22)
  Gcoeff = (-(c(3)*M(1))+c(4)*M(2)) * den(6)
  T2sum(1:15,16) = T2sum(1:15,16) + Gcoeff * G2tensor(:,10)

end subroutine vamp_1

end module ol_vamp_1_ppjj_dddxdx_1_/**/REALKIND
