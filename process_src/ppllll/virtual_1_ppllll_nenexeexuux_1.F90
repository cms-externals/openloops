
module ol_vamp_1_ppllll_nenexeexuux_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_1(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppllll_nenexeexuux_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppllll_nenexeexuux_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppllll_nenexeexuux_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppllll_nenexeexuux_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,6) :: G0
  complex(REALKIND), dimension(4,5,4,28) :: G1
  complex(REALKIND), dimension(4,15,4,28) :: G2
  complex(REALKIND), dimension(4,35,4,5) :: G3
  complex(REALKIND), dimension(5,5) :: G1tensor
  complex(REALKIND), dimension(15,24) :: G2tensor
  complex(REALKIND), dimension(35,5) :: G3tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_VA_Q(G0(:,:,:,1),wf(:,-5),G0(:,:,:,2))
  call loop_A_Q(G0(:,:,:,2),Q(:,32),ZERO,G1(:,:,:,1))
  call loop_AV_Q(G1(:,:,:,1),wf(:,2),G1(:,:,:,2))
  call loop_A_Q(G1(:,:,:,2),Q(:,44),ZERO,G2(:,:,:,1))
  call check_last_AQ_V(l_switch,G2(:,:,:,1),wf(:,6),G2tensor(:,1))
  call loop_AZ_Q(G2(:,:,:,1),wf(:,3),G2(:,:,:,2),gZu)
  call loop_A_Q(G2(:,:,:,2),Q(:,47),ZERO,G3(:,:,:,1))
  call check_last_AQ_V(l_switch,G3(:,:,:,1),wf(:,-4),G3tensor(:,1))
  call loop_AZ_Q(G1(:,:,:,1),wf(:,8),G1(:,:,:,3),gZu)
  call loop_A_Q(G1(:,:,:,3),Q(:,44),ZERO,G2(:,:,:,3))
  call check_last_AQ_V(l_switch,G2(:,:,:,3),wf(:,6),G2tensor(:,2))
  call loop_AZ_Q(G2(:,:,:,3),wf(:,3),G2(:,:,:,4),gZu)
  call loop_A_Q(G2(:,:,:,4),Q(:,47),ZERO,G3(:,:,:,2))
  call check_last_AQ_V(l_switch,G3(:,:,:,2),wf(:,-4),G3tensor(:,2))
  call loop_AZ_Q(G1(:,:,:,1),wf(:,3),G1(:,:,:,4),gZu)
  call loop_A_Q(G1(:,:,:,4),Q(:,35),ZERO,G2(:,:,:,5))
  call check_last_AQ_V(l_switch,G2(:,:,:,5),wf(:,12),G2tensor(:,3))
  call check_last_AQ_V(l_switch,G2(:,:,:,5),wf(:,14),G2tensor(:,4))
  call loop_AV_Q(G2(:,:,:,5),wf(:,2),G2(:,:,:,6))
  call loop_A_Q(G2(:,:,:,6),Q(:,47),ZERO,G3(:,:,:,3))
  call check_last_AQ_V(l_switch,G3(:,:,:,3),wf(:,-4),G3tensor(:,3))
  call loop_AZ_Q(G2(:,:,:,5),wf(:,8),G2(:,:,:,7),gZu)
  call loop_A_Q(G2(:,:,:,7),Q(:,47),ZERO,G3(:,:,:,4))
  call check_last_AQ_V(l_switch,G3(:,:,:,4),wf(:,-4),G3tensor(:,4))
  call loop_AV_Q(G1(:,:,:,1),wf(:,77),G1(:,:,:,5))
  call loop_A_Q(G1(:,:,:,5),Q(:,47),ZERO,G2(:,:,:,8))
  call check_last_AQ_V(l_switch,G2(:,:,:,8),wf(:,-4),G2tensor(:,5))
  call loop_AZ_Q(G1(:,:,:,1),wf(:,79),G1(:,:,:,6),gZu)
  call loop_A_Q(G1(:,:,:,6),Q(:,47),ZERO,G2(:,:,:,9))
  call check_last_AQ_V(l_switch,G2(:,:,:,9),wf(:,-4),G2tensor(:,6))
  call loop_AV_Q(G1(:,:,:,1),wf(:,81),G1(:,:,:,7))
  call loop_A_Q(G1(:,:,:,7),Q(:,47),ZERO,G2(:,:,:,10))
  call check_last_AQ_V(l_switch,G2(:,:,:,10),wf(:,-4),G2tensor(:,7))
  call loop_AZ_Q(G1(:,:,:,1),wf(:,83),G1(:,:,:,8),gZu)
  call loop_A_Q(G1(:,:,:,8),Q(:,47),ZERO,G2(:,:,:,11))
  call check_last_AQ_V(l_switch,G2(:,:,:,11),wf(:,-4),G2tensor(:,8))
  call loop_AW_Q(G1(:,:,:,1),wf(:,29),G1(:,:,:,9))
  call loop_A_Q(G1(:,:,:,9),Q(:,41),ZERO,G2(:,:,:,12))
  call check_last_AQ_V(l_switch,G2(:,:,:,12),wf(:,34),G2tensor(:,9))
  call loop_AW_Q(G2(:,:,:,12),wf(:,30),G2(:,:,:,13))
  call loop_A_Q(G2(:,:,:,13),Q(:,47),ZERO,G3(:,:,:,5))
  call check_last_AQ_V(l_switch,G3(:,:,:,5),wf(:,-4),G3tensor(:,5))
  call loop_AV_Q(G1(:,:,:,1),wf(:,31),G1(:,:,:,10))
  call loop_A_Q(G1(:,:,:,10),Q(:,47),ZERO,G2(:,:,:,14))
  call check_last_AQ_V(l_switch,G2(:,:,:,14),wf(:,-4),G2tensor(:,10))
  call loop_AZ_Q(G1(:,:,:,1),wf(:,84),G1(:,:,:,11),gZu)
  call loop_A_Q(G1(:,:,:,11),Q(:,47),ZERO,G2(:,:,:,15))
  call check_last_AQ_V(l_switch,G2(:,:,:,15),wf(:,-4),G2tensor(:,11))
  call loop_AV_Q(G1(:,:,:,1),wf(:,85),G1(:,:,:,12))
  call loop_A_Q(G1(:,:,:,12),Q(:,47),ZERO,G2(:,:,:,16))
  call check_last_AQ_V(l_switch,G2(:,:,:,16),wf(:,-4),G2tensor(:,12))
  call loop_AZ_Q(G1(:,:,:,1),wf(:,87),G1(:,:,:,13),gZu)
  call loop_A_Q(G1(:,:,:,13),Q(:,47),ZERO,G2(:,:,:,17))
  call check_last_AQ_V(l_switch,G2(:,:,:,17),wf(:,-4),G2tensor(:,13))
  call loop_AZ_Q(G1(:,:,:,1),wf(:,90),G1(:,:,:,14),gZu)
  call loop_A_Q(G1(:,:,:,14),Q(:,47),ZERO,G2(:,:,:,18))
  call check_last_AQ_V(l_switch,G2(:,:,:,18),wf(:,-4),G2tensor(:,14))
  call loop_AV_Q(G1(:,:,:,1),wf(:,91),G1(:,:,:,15))
  call loop_A_Q(G1(:,:,:,15),Q(:,47),ZERO,G2(:,:,:,19))
  call check_last_AQ_V(l_switch,G2(:,:,:,19),wf(:,-4),G2tensor(:,15))
  call loop_AZ_Q(G1(:,:,:,1),wf(:,93),G1(:,:,:,16),gZu)
  call loop_A_Q(G1(:,:,:,16),Q(:,47),ZERO,G2(:,:,:,20))
  call check_last_AQ_V(l_switch,G2(:,:,:,20),wf(:,-4),G2tensor(:,16))
  call loop_AZ_Q(G1(:,:,:,1),wf(:,96),G1(:,:,:,17),gZu)
  call loop_A_Q(G1(:,:,:,17),Q(:,47),ZERO,G2(:,:,:,21))
  call check_last_AQ_V(l_switch,G2(:,:,:,21),wf(:,-4),G2tensor(:,17))
  call loop_AZ_Q(G1(:,:,:,1),wf(:,98),G1(:,:,:,18),gZu)
  call loop_A_Q(G1(:,:,:,18),Q(:,47),ZERO,G2(:,:,:,22))
  call check_last_AQ_V(l_switch,G2(:,:,:,22),wf(:,-4),G2tensor(:,18))
  call loop_AZ_Q(G1(:,:,:,1),wf(:,101),G1(:,:,:,19),gZu)
  call loop_A_Q(G1(:,:,:,19),Q(:,47),ZERO,G2(:,:,:,23))
  call check_last_AQ_V(l_switch,G2(:,:,:,23),wf(:,-4),G2tensor(:,19))
  call loop_VA_Q(G0(:,:,:,1),wf(:,52),G0(:,:,:,3))
  call loop_A_Q(G0(:,:,:,3),Q(:,35),ZERO,G1(:,:,:,20))
  call loop_AV_Q(G1(:,:,:,20),wf(:,2),G1(:,:,:,21))
  call loop_A_Q(G1(:,:,:,21),Q(:,47),ZERO,G2(:,:,:,24))
  call check_last_AQ_V(l_switch,G2(:,:,:,24),wf(:,-4),G2tensor(:,20))
  call loop_AZ_Q(G1(:,:,:,20),wf(:,8),G1(:,:,:,22),gZu)
  call loop_A_Q(G1(:,:,:,22),Q(:,47),ZERO,G2(:,:,:,25))
  call check_last_AQ_V(l_switch,G2(:,:,:,25),wf(:,-4),G2tensor(:,21))
  call check_last_AQ_V(l_switch,G1(:,:,:,20),wf(:,12),G1tensor(:,1))
  call check_last_AQ_V(l_switch,G1(:,:,:,20),wf(:,14),G1tensor(:,2))
  call loop_VA_Q(G0(:,:,:,1),wf(:,55),G0(:,:,:,4))
  call loop_A_Q(G0(:,:,:,4),Q(:,44),ZERO,G1(:,:,:,23))
  call loop_AZ_Q(G1(:,:,:,23),wf(:,3),G1(:,:,:,24),gZu)
  call loop_A_Q(G1(:,:,:,24),Q(:,47),ZERO,G2(:,:,:,26))
  call check_last_AQ_V(l_switch,G2(:,:,:,26),wf(:,-4),G2tensor(:,22))
  call check_last_AQ_V(l_switch,G1(:,:,:,23),wf(:,6),G1tensor(:,3))
  call loop_VA_Q(G0(:,:,:,1),wf(:,56),G0(:,:,:,5))
  call loop_A_Q(G0(:,:,:,5),Q(:,44),ZERO,G1(:,:,:,25))
  call loop_AZ_Q(G1(:,:,:,25),wf(:,3),G1(:,:,:,26),gZu)
  call loop_A_Q(G1(:,:,:,26),Q(:,47),ZERO,G2(:,:,:,27))
  call check_last_AQ_V(l_switch,G2(:,:,:,27),wf(:,-4),G2tensor(:,23))
  call check_last_AQ_V(l_switch,G1(:,:,:,25),wf(:,6),G1tensor(:,4))
  call loop_VA_Q(G0(:,:,:,1),wf(:,68),G0(:,:,:,6))
  call loop_A_Q(G0(:,:,:,6),Q(:,41),ZERO,G1(:,:,:,27))
  call loop_AW_Q(G1(:,:,:,27),wf(:,30),G1(:,:,:,28))
  call loop_A_Q(G1(:,:,:,28),Q(:,47),ZERO,G2(:,:,:,28))
  call check_last_AQ_V(l_switch,G2(:,:,:,28),wf(:,-4),G2tensor(:,24))
  call check_last_AQ_V(l_switch,G1(:,:,:,27),wf(:,34),G1tensor(:,5))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (-(c(1)*M(1))) * den(5)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(2)*M(1)) * den(7)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(1)*M(1))) * den(51)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,20)
  Gcoeff = (c(2)*M(1)) * den(52)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,21)
  Gcoeff = (-(c(1)*M(1))) * den(10)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(2)*M(1)) * den(12)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,4)
  Gcoeff = (-(c(1)*M(1))) * den(55)
  T2sum(1:15,4) = T2sum(1:15,4) + Gcoeff * G2tensor(:,22)
  Gcoeff = (c(2)*M(1)) * den(57)
  T2sum(1:15,4) = T2sum(1:15,4) + Gcoeff * G2tensor(:,23)
  Gcoeff = (-(c(1)*M(1))) * den(87)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,1)
  Gcoeff = (c(2)*M(1)) * den(88)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,2)
  Gcoeff = (-(c(1)*M(1))) * den(87)
  T3sum(1:35,2) = T3sum(1:35,2) + Gcoeff * G3tensor(:,3)
  Gcoeff = (c(2)*M(1)) * den(88)
  T3sum(1:35,2) = T3sum(1:35,2) + Gcoeff * G3tensor(:,4)
  Gcoeff = (-(c(1)*M(1))) * den(67)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(2)*M(1)) * den(69)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,6)
  Gcoeff = (-(c(1)*M(1))) * den(71)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(2)*M(1)) * den(72)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,8)
  Gcoeff = (-(c(3)*M(1))) * den(60)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,24)
  Gcoeff = (-(c(3)*M(1))) * den(31)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,9)
  Gcoeff = (-(c(5)*M(1))) * den(73)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,10)
  Gcoeff = (-(c(4)*M(1))) * den(74)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,11)
  Gcoeff = (-(c(3)*M(1))) * den(26)
  T3sum(1:35,3) = T3sum(1:35,3) + Gcoeff * G3tensor(:,5)
  Gcoeff = (c(5)*M(1)) * den(75)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,12)
  Gcoeff = (-(c(6)*M(1))) * den(76)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,13)
  Gcoeff = (-(c(6)*M(1))) * den(79)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,14)
  Gcoeff = (c(5)*M(1)) * den(80)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,15)
  Gcoeff = (-(c(6)*M(1))) * den(81)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,16)
  Gcoeff = (-(c(6)*M(1))) * den(83)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,17)
  Gcoeff = (c(2)*M(1)) * den(84)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,18)
  Gcoeff = (c(2)*M(1)) * den(86)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,19)
  Gcoeff = (-(c(1)*M(1))) * den(61)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,3)
  Gcoeff = (c(2)*M(1)) * den(62)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,4)
  Gcoeff = (-(c(1)*M(1))) * den(63)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(2)*M(1)) * den(64)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,2)
  Gcoeff = (-(c(3)*M(1))) * den(65)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,5)

end subroutine vamp_1

end module ol_vamp_1_ppllll_nenexeexuux_1_/**/REALKIND
