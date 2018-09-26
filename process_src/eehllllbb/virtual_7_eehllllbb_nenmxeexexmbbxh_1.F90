
module ol_vamp_7_eehllllbb_nenmxeexexmbbxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none

  contains

! **********************************************************************
subroutine vamp_7(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_eehllllbb_nenmxeexexmbbxh_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_eehllllbb_nenmxeexexmbbxh_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_eehllllbb_nenmxeexexmbbxh_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_eehllllbb_nenmxeexexmbbxh_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,13) :: G0
  complex(REALKIND), dimension(4,5,4,33) :: G1
  complex(REALKIND), dimension(4,15,4,21) :: G2
  complex(REALKIND), dimension(5,21) :: G1tensor
  complex(REALKIND), dimension(15,21) :: G2tensor
if (mode == -1) return

  if (mode == 1 .or. stability_mode < 20 .or. stability_mode >= 30) then

  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_VQ_A(G0(:,:,:,1),wf(:,5208),G0(:,:,:,2))
  call loop_Q_A(G0(:,:,:,2),Q(:,363),MB,G1(:,:,:,1))
  call check_last_QA_V(l_switch,G1(:,:,:,1),wf(:,66),G1tensor(:,1))
  call check_last_QA_V(l_switch,G1(:,:,:,1),wf(:,69),G1tensor(:,2))
  call loop_QV_A(G1(:,:,:,1),wf(:,3),G1(:,:,:,2))
  call loop_Q_A(G1(:,:,:,2),Q(:,383),MB,G2(:,:,:,1))
  call check_last_QA_V(l_switch,G2(:,:,:,1),wf(:,-7),G2tensor(:,1))
  call loop_QZ_A(G1(:,:,:,1),wf(:,14),G1(:,:,:,3),gZd)
  call loop_Q_A(G1(:,:,:,3),Q(:,383),MB,G2(:,:,:,2))
  call check_last_QA_V(l_switch,G2(:,:,:,2),wf(:,-7),G2tensor(:,2))
  call loop_VA_Q(G0(:,:,:,1),wf(:,5210),G0(:,:,:,3))
  call loop_A_Q(G0(:,:,:,3),Q(:,427),MB,G1(:,:,:,4))
  call check_last_AQ_V(l_switch,G1(:,:,:,4),wf(:,60),G1tensor(:,3))
  call check_last_AQ_V(l_switch,G1(:,:,:,4),wf(:,63),G1tensor(:,4))
  call loop_AV_Q(G1(:,:,:,4),wf(:,3),G1(:,:,:,5))
  call loop_A_Q(G1(:,:,:,5),Q(:,447),MB,G2(:,:,:,3))
  call check_last_AQ_V(l_switch,G2(:,:,:,3),wf(:,-6),G2tensor(:,3))
  call loop_AZ_Q(G1(:,:,:,4),wf(:,14),G1(:,:,:,6),gZd)
  call loop_A_Q(G1(:,:,:,6),Q(:,447),MB,G2(:,:,:,4))
  call check_last_AQ_V(l_switch,G2(:,:,:,4),wf(:,-6),G2tensor(:,4))
  call loop_VA_Q(G0(:,:,:,1),wf(:,5212),G0(:,:,:,4))
  call loop_A_Q(G0(:,:,:,4),Q(:,427),MB,G1(:,:,:,7))
  call check_last_AQ_V(l_switch,G1(:,:,:,7),wf(:,60),G1tensor(:,5))
  call check_last_AQ_V(l_switch,G1(:,:,:,7),wf(:,63),G1tensor(:,6))
  call loop_AV_Q(G1(:,:,:,7),wf(:,3),G1(:,:,:,8))
  call loop_A_Q(G1(:,:,:,8),Q(:,447),MB,G2(:,:,:,5))
  call check_last_AQ_V(l_switch,G2(:,:,:,5),wf(:,-6),G2tensor(:,5))
  call loop_AZ_Q(G1(:,:,:,7),wf(:,14),G1(:,:,:,9),gZd)
  call loop_A_Q(G1(:,:,:,9),Q(:,447),MB,G2(:,:,:,6))
  call check_last_AQ_V(l_switch,G2(:,:,:,6),wf(:,-6),G2tensor(:,6))
  call loop_VA_Q(G0(:,:,:,1),wf(:,5215),G0(:,:,:,5))
  call loop_A_Q(G0(:,:,:,5),Q(:,427),MB,G1(:,:,:,10))
  call check_last_AQ_V(l_switch,G1(:,:,:,10),wf(:,60),G1tensor(:,7))
  call check_last_AQ_V(l_switch,G1(:,:,:,10),wf(:,63),G1tensor(:,8))
  call loop_AV_Q(G1(:,:,:,10),wf(:,3),G1(:,:,:,11))
  call loop_A_Q(G1(:,:,:,11),Q(:,447),MB,G2(:,:,:,7))
  call check_last_AQ_V(l_switch,G2(:,:,:,7),wf(:,-6),G2tensor(:,7))
  call loop_AZ_Q(G1(:,:,:,10),wf(:,14),G1(:,:,:,12),gZd)
  call loop_A_Q(G1(:,:,:,12),Q(:,447),MB,G2(:,:,:,8))
  call check_last_AQ_V(l_switch,G2(:,:,:,8),wf(:,-6),G2tensor(:,8))
  call loop_VQ_A(G0(:,:,:,1),wf(:,5217),G0(:,:,:,6))
  call loop_Q_A(G0(:,:,:,6),Q(:,363),MB,G1(:,:,:,13))
  call check_last_QA_V(l_switch,G1(:,:,:,13),wf(:,66),G1tensor(:,9))
  call check_last_QA_V(l_switch,G1(:,:,:,13),wf(:,69),G1tensor(:,10))
  call loop_QV_A(G1(:,:,:,13),wf(:,3),G1(:,:,:,14))
  call loop_Q_A(G1(:,:,:,14),Q(:,383),MB,G2(:,:,:,9))
  call check_last_QA_V(l_switch,G2(:,:,:,9),wf(:,-7),G2tensor(:,9))
  call loop_QZ_A(G1(:,:,:,13),wf(:,14),G1(:,:,:,15),gZd)
  call loop_Q_A(G1(:,:,:,15),Q(:,383),MB,G2(:,:,:,10))
  call check_last_QA_V(l_switch,G2(:,:,:,10),wf(:,-7),G2tensor(:,10))
  call loop_VQ_A(G0(:,:,:,1),wf(:,5220),G0(:,:,:,7))
  call loop_Q_A(G0(:,:,:,7),Q(:,349),MT,G1(:,:,:,16))
  call check_last_QA_V(l_switch,G1(:,:,:,16),wf(:,238),G1tensor(:,11))
  call loop_QW_A(G1(:,:,:,16),wf(:,6),G1(:,:,:,17))
  call loop_Q_A(G1(:,:,:,17),Q(:,383),MB,G2(:,:,:,11))
  call check_last_QA_V(l_switch,G2(:,:,:,11),wf(:,-7),G2tensor(:,11))
  call loop_VQ_A(G0(:,:,:,1),wf(:,5223),G0(:,:,:,8))
  call loop_Q_A(G0(:,:,:,8),Q(:,349),MT,G1(:,:,:,18))
  call check_last_QA_V(l_switch,G1(:,:,:,18),wf(:,238),G1tensor(:,12))
  call loop_QW_A(G1(:,:,:,18),wf(:,6),G1(:,:,:,19))
  call loop_Q_A(G1(:,:,:,19),Q(:,383),MB,G2(:,:,:,12))
  call check_last_QA_V(l_switch,G2(:,:,:,12),wf(:,-7),G2tensor(:,12))
  call loop_VQ_A(G0(:,:,:,1),wf(:,5225),G0(:,:,:,9))
  call loop_Q_A(G0(:,:,:,9),Q(:,349),MT,G1(:,:,:,20))
  call check_last_QA_V(l_switch,G1(:,:,:,20),wf(:,238),G1tensor(:,13))
  call loop_QW_A(G1(:,:,:,20),wf(:,6),G1(:,:,:,21))
  call loop_Q_A(G1(:,:,:,21),Q(:,383),MB,G2(:,:,:,13))
  call check_last_QA_V(l_switch,G2(:,:,:,13),wf(:,-7),G2tensor(:,13))
  call loop_VA_Q(G0(:,:,:,1),wf(:,5227),G0(:,:,:,10))
  call loop_A_Q(G0(:,:,:,10),Q(:,427),MB,G1(:,:,:,22))
  call check_last_AQ_V(l_switch,G1(:,:,:,22),wf(:,60),G1tensor(:,14))
  call check_last_AQ_V(l_switch,G1(:,:,:,22),wf(:,63),G1tensor(:,15))
  call loop_AV_Q(G1(:,:,:,22),wf(:,3),G1(:,:,:,23))
  call loop_A_Q(G1(:,:,:,23),Q(:,447),MB,G2(:,:,:,14))
  call check_last_AQ_V(l_switch,G2(:,:,:,14),wf(:,-6),G2tensor(:,14))
  call loop_AZ_Q(G1(:,:,:,22),wf(:,14),G1(:,:,:,24),gZd)
  call loop_A_Q(G1(:,:,:,24),Q(:,447),MB,G2(:,:,:,15))
  call check_last_AQ_V(l_switch,G2(:,:,:,15),wf(:,-6),G2tensor(:,15))
  call loop_VA_Q(G0(:,:,:,1),wf(:,5229),G0(:,:,:,11))
  call loop_A_Q(G0(:,:,:,11),Q(:,427),MB,G1(:,:,:,25))
  call check_last_AQ_V(l_switch,G1(:,:,:,25),wf(:,60),G1tensor(:,16))
  call check_last_AQ_V(l_switch,G1(:,:,:,25),wf(:,63),G1tensor(:,17))
  call loop_AV_Q(G1(:,:,:,25),wf(:,3),G1(:,:,:,26))
  call loop_A_Q(G1(:,:,:,26),Q(:,447),MB,G2(:,:,:,16))
  call check_last_AQ_V(l_switch,G2(:,:,:,16),wf(:,-6),G2tensor(:,16))
  call loop_AZ_Q(G1(:,:,:,25),wf(:,14),G1(:,:,:,27),gZd)
  call loop_A_Q(G1(:,:,:,27),Q(:,447),MB,G2(:,:,:,17))
  call check_last_AQ_V(l_switch,G2(:,:,:,17),wf(:,-6),G2tensor(:,17))
  call loop_VQ_A(G0(:,:,:,1),wf(:,5231),G0(:,:,:,12))
  call loop_Q_A(G0(:,:,:,12),Q(:,363),MB,G1(:,:,:,28))
  call check_last_QA_V(l_switch,G1(:,:,:,28),wf(:,66),G1tensor(:,18))
  call check_last_QA_V(l_switch,G1(:,:,:,28),wf(:,69),G1tensor(:,19))
  call loop_QV_A(G1(:,:,:,28),wf(:,3),G1(:,:,:,29))
  call loop_Q_A(G1(:,:,:,29),Q(:,383),MB,G2(:,:,:,18))
  call check_last_QA_V(l_switch,G2(:,:,:,18),wf(:,-7),G2tensor(:,18))
  call loop_QZ_A(G1(:,:,:,28),wf(:,14),G1(:,:,:,30),gZd)
  call loop_Q_A(G1(:,:,:,30),Q(:,383),MB,G2(:,:,:,19))
  call check_last_QA_V(l_switch,G2(:,:,:,19),wf(:,-7),G2tensor(:,19))
  call loop_VQ_A(G0(:,:,:,1),wf(:,5233),G0(:,:,:,13))
  call loop_Q_A(G0(:,:,:,13),Q(:,363),MB,G1(:,:,:,31))
  call check_last_QA_V(l_switch,G1(:,:,:,31),wf(:,66),G1tensor(:,20))
  call check_last_QA_V(l_switch,G1(:,:,:,31),wf(:,69),G1tensor(:,21))
  call loop_QV_A(G1(:,:,:,31),wf(:,3),G1(:,:,:,32))
  call loop_Q_A(G1(:,:,:,32),Q(:,383),MB,G2(:,:,:,20))
  call check_last_QA_V(l_switch,G2(:,:,:,20),wf(:,-7),G2tensor(:,20))
  call loop_QZ_A(G1(:,:,:,31),wf(:,14),G1(:,:,:,33),gZd)
  call loop_Q_A(G1(:,:,:,33),Q(:,383),MB,G2(:,:,:,21))
  call check_last_QA_V(l_switch,G2(:,:,:,21),wf(:,-7),G2tensor(:,21))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (-(c(56)*M(1))) * den(9334)
  T1sum(1:5,4) = T1sum(1:5,4) + Gcoeff * G1tensor(:,1)
  Gcoeff = (-(c(57)*M(1))) * den(9335)
  T1sum(1:5,4) = T1sum(1:5,4) + Gcoeff * G1tensor(:,2)
  Gcoeff = (-(c(53)*M(1))) * den(9336)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,3)
  Gcoeff = (-(c(54)*M(1))) * den(9337)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,4)
  Gcoeff = (-(c(56)*M(1))) * den(9338)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,5)
  Gcoeff = (-(c(57)*M(1))) * den(9339)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,6)
  Gcoeff = (-(c(58)*M(1))) * den(9340)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,7)
  Gcoeff = (-(c(59)*M(1))) * den(9341)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,8)
  Gcoeff = (-(c(58)*M(1))) * den(9342)
  T1sum(1:5,4) = T1sum(1:5,4) + Gcoeff * G1tensor(:,9)
  Gcoeff = (-(c(59)*M(1))) * den(9343)
  T1sum(1:5,4) = T1sum(1:5,4) + Gcoeff * G1tensor(:,10)
  Gcoeff = (-(c(35)*M(1))) * den(9344)
  T1sum(1:5,7) = T1sum(1:5,7) + Gcoeff * G1tensor(:,11)
  Gcoeff = (-(c(35)*M(1))) * den(9345)
  T1sum(1:5,7) = T1sum(1:5,7) + Gcoeff * G1tensor(:,12)
  Gcoeff = (-(c(37)*M(1))) * den(9346)
  T1sum(1:5,7) = T1sum(1:5,7) + Gcoeff * G1tensor(:,13)
  Gcoeff = (-(c(55)*M(1))) * den(9347)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,14)
  Gcoeff = (-(c(56)*M(1))) * den(9348)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,16)
  Gcoeff = (-(c(56)*M(1))) * den(9349)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,15)
  Gcoeff = (-(c(57)*M(1))) * den(9350)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,17)
  Gcoeff = (-(c(55)*M(1))) * den(9351)
  T1sum(1:5,4) = T1sum(1:5,4) + Gcoeff * G1tensor(:,18)
  Gcoeff = (-(c(56)*M(1))) * den(9352)
  T1sum(1:5,4) = T1sum(1:5,4) + Gcoeff * G1tensor(:,20)
  Gcoeff = (-(c(56)*M(1))) * den(9353)
  T1sum(1:5,4) = T1sum(1:5,4) + Gcoeff * G1tensor(:,19)
  Gcoeff = (-(c(57)*M(1))) * den(9354)
  T1sum(1:5,4) = T1sum(1:5,4) + Gcoeff * G1tensor(:,21)
  Gcoeff = (-(c(56)*M(1))) * den(10890)
  T2sum(1:15,8) = T2sum(1:15,8) + Gcoeff * G2tensor(:,1)
  Gcoeff = (-(c(57)*M(1))) * den(10891)
  T2sum(1:15,8) = T2sum(1:15,8) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(53)*M(1))) * den(10898)
  T2sum(1:15,9) = T2sum(1:15,9) + Gcoeff * G2tensor(:,3)
  Gcoeff = (-(c(54)*M(1))) * den(10899)
  T2sum(1:15,9) = T2sum(1:15,9) + Gcoeff * G2tensor(:,4)
  Gcoeff = (-(c(56)*M(1))) * den(10900)
  T2sum(1:15,9) = T2sum(1:15,9) + Gcoeff * G2tensor(:,5)
  Gcoeff = (-(c(57)*M(1))) * den(10901)
  T2sum(1:15,9) = T2sum(1:15,9) + Gcoeff * G2tensor(:,6)
  Gcoeff = (-(c(58)*M(1))) * den(10906)
  T2sum(1:15,8) = T2sum(1:15,8) + Gcoeff * G2tensor(:,9)
  Gcoeff = (-(c(59)*M(1))) * den(10907)
  T2sum(1:15,8) = T2sum(1:15,8) + Gcoeff * G2tensor(:,10)
  Gcoeff = (-(c(58)*M(1))) * den(10908)
  T2sum(1:15,9) = T2sum(1:15,9) + Gcoeff * G2tensor(:,7)
  Gcoeff = (-(c(59)*M(1))) * den(10909)
  T2sum(1:15,9) = T2sum(1:15,9) + Gcoeff * G2tensor(:,8)
  Gcoeff = (-(c(55)*M(1))) * den(10921)
  T2sum(1:15,8) = T2sum(1:15,8) + Gcoeff * G2tensor(:,18)
  Gcoeff = (-(c(56)*M(1))) * den(10922)
  T2sum(1:15,8) = T2sum(1:15,8) + Gcoeff * G2tensor(:,20)
  Gcoeff = (-(c(56)*M(1))) * den(10923)
  T2sum(1:15,8) = T2sum(1:15,8) + Gcoeff * G2tensor(:,19)
  Gcoeff = (-(c(57)*M(1))) * den(10924)
  T2sum(1:15,8) = T2sum(1:15,8) + Gcoeff * G2tensor(:,21)
  Gcoeff = (-(c(55)*M(1))) * den(10925)
  T2sum(1:15,9) = T2sum(1:15,9) + Gcoeff * G2tensor(:,14)
  Gcoeff = (-(c(56)*M(1))) * den(10926)
  T2sum(1:15,9) = T2sum(1:15,9) + Gcoeff * G2tensor(:,16)
  Gcoeff = (-(c(56)*M(1))) * den(10927)
  T2sum(1:15,9) = T2sum(1:15,9) + Gcoeff * G2tensor(:,15)
  Gcoeff = (-(c(57)*M(1))) * den(10928)
  T2sum(1:15,9) = T2sum(1:15,9) + Gcoeff * G2tensor(:,17)
  Gcoeff = (-(c(35)*M(1))) * den(10929)
  T2sum(1:15,22) = T2sum(1:15,22) + Gcoeff * G2tensor(:,11)
  Gcoeff = (-(c(35)*M(1))) * den(10930)
  T2sum(1:15,22) = T2sum(1:15,22) + Gcoeff * G2tensor(:,12)
  Gcoeff = (-(c(37)*M(1))) * den(10931)
  T2sum(1:15,22) = T2sum(1:15,22) + Gcoeff * G2tensor(:,13)

#ifdef LOOPSQUARED
#ifndef PRECISION_dp
  call gtdealloc()
#endif
#endif

end subroutine vamp_7

#ifdef LOOPSQUARED
subroutine gtdealloc()
  implicit none

end subroutine gtdealloc
#endif

end module ol_vamp_7_eehllllbb_nenmxeexexmbbxh_1_/**/REALKIND
