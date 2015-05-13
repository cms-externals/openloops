
module ol_vamp_1_ppllll_nenexnexeudx_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_1(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppllll_nenexnexeudx_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppllll_nenexnexeudx_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppllll_nenexnexeudx_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppllll_nenexnexeudx_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,6) :: G0
  complex(REALKIND), dimension(4,5,4,21) :: G1
  complex(REALKIND), dimension(4,15,4,20) :: G2
  complex(REALKIND), dimension(4,35,4,4) :: G3
  complex(REALKIND), dimension(5,4) :: G1tensor
  complex(REALKIND), dimension(15,16) :: G2tensor
  complex(REALKIND), dimension(35,4) :: G3tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_VA_Q(G0(:,:,:,1),wf(:,-5),G0(:,:,:,2))
  call loop_A_Q(G0(:,:,:,2),Q(:,32),ZERO,G1(:,:,:,1))
  call loop_AW_Q(G1(:,:,:,1),wf(:,5),G1(:,:,:,2))
  call loop_A_Q(G1(:,:,:,2),Q(:,44),ZERO,G2(:,:,:,1))
  call check_last_AQ_V(l_switch,G2(:,:,:,1),wf(:,10),G2tensor(:,1))
  call loop_AZ_Q(G2(:,:,:,1),wf(:,4),G2(:,:,:,2),gZu)
  call loop_A_Q(G2(:,:,:,2),Q(:,47),ZERO,G3(:,:,:,1))
  call check_last_AQ_V(l_switch,G3(:,:,:,1),wf(:,-4),G3tensor(:,1))
  call loop_AZ_Q(G1(:,:,:,1),wf(:,4),G1(:,:,:,3),gZd)
  call loop_A_Q(G1(:,:,:,3),Q(:,35),ZERO,G2(:,:,:,3))
  call check_last_AQ_V(l_switch,G2(:,:,:,3),wf(:,13),G2tensor(:,2))
  call loop_AW_Q(G2(:,:,:,3),wf(:,5),G2(:,:,:,4))
  call loop_A_Q(G2(:,:,:,4),Q(:,47),ZERO,G3(:,:,:,2))
  call check_last_AQ_V(l_switch,G3(:,:,:,2),wf(:,-4),G3tensor(:,2))
  call loop_AW_Q(G1(:,:,:,1),wf(:,63),G1(:,:,:,4))
  call loop_A_Q(G1(:,:,:,4),Q(:,47),ZERO,G2(:,:,:,5))
  call check_last_AQ_V(l_switch,G2(:,:,:,5),wf(:,-4),G2tensor(:,3))
  call loop_AW_Q(G1(:,:,:,1),wf(:,65),G1(:,:,:,5))
  call loop_A_Q(G1(:,:,:,5),Q(:,47),ZERO,G2(:,:,:,6))
  call check_last_AQ_V(l_switch,G2(:,:,:,6),wf(:,-4),G2tensor(:,4))
  call loop_AW_Q(G1(:,:,:,1),wf(:,68),G1(:,:,:,6))
  call loop_A_Q(G1(:,:,:,6),Q(:,47),ZERO,G2(:,:,:,7))
  call check_last_AQ_V(l_switch,G2(:,:,:,7),wf(:,-4),G2tensor(:,5))
  call loop_AW_Q(G1(:,:,:,1),wf(:,23),G1(:,:,:,7))
  call loop_A_Q(G1(:,:,:,7),Q(:,42),ZERO,G2(:,:,:,8))
  call check_last_AQ_V(l_switch,G2(:,:,:,8),wf(:,27),G2tensor(:,6))
  call loop_AZ_Q(G2(:,:,:,8),wf(:,22),G2(:,:,:,9),gZu)
  call loop_A_Q(G2(:,:,:,9),Q(:,47),ZERO,G3(:,:,:,3))
  call check_last_AQ_V(l_switch,G3(:,:,:,3),wf(:,-4),G3tensor(:,3))
  call loop_AZ_Q(G1(:,:,:,1),wf(:,22),G1(:,:,:,8),gZd)
  call loop_A_Q(G1(:,:,:,8),Q(:,37),ZERO,G2(:,:,:,10))
  call check_last_AQ_V(l_switch,G2(:,:,:,10),wf(:,30),G2tensor(:,7))
  call loop_AW_Q(G2(:,:,:,10),wf(:,23),G2(:,:,:,11))
  call loop_A_Q(G2(:,:,:,11),Q(:,47),ZERO,G3(:,:,:,4))
  call check_last_AQ_V(l_switch,G3(:,:,:,4),wf(:,-4),G3tensor(:,4))
  call loop_AW_Q(G1(:,:,:,1),wf(:,69),G1(:,:,:,9))
  call loop_A_Q(G1(:,:,:,9),Q(:,47),ZERO,G2(:,:,:,12))
  call check_last_AQ_V(l_switch,G2(:,:,:,12),wf(:,-4),G2tensor(:,8))
  call loop_AW_Q(G1(:,:,:,1),wf(:,71),G1(:,:,:,10))
  call loop_A_Q(G1(:,:,:,10),Q(:,47),ZERO,G2(:,:,:,13))
  call check_last_AQ_V(l_switch,G2(:,:,:,13),wf(:,-4),G2tensor(:,9))
  call loop_AW_Q(G1(:,:,:,1),wf(:,74),G1(:,:,:,11))
  call loop_A_Q(G1(:,:,:,11),Q(:,47),ZERO,G2(:,:,:,14))
  call check_last_AQ_V(l_switch,G2(:,:,:,14),wf(:,-4),G2tensor(:,10))
  call loop_AW_Q(G1(:,:,:,1),wf(:,76),G1(:,:,:,12))
  call loop_A_Q(G1(:,:,:,12),Q(:,47),ZERO,G2(:,:,:,15))
  call check_last_AQ_V(l_switch,G2(:,:,:,15),wf(:,-4),G2tensor(:,11))
  call loop_AW_Q(G1(:,:,:,1),wf(:,78),G1(:,:,:,13))
  call loop_A_Q(G1(:,:,:,13),Q(:,47),ZERO,G2(:,:,:,16))
  call check_last_AQ_V(l_switch,G2(:,:,:,16),wf(:,-4),G2tensor(:,12))
  call loop_VA_Q(G0(:,:,:,1),wf(:,43),G0(:,:,:,3))
  call loop_A_Q(G0(:,:,:,3),Q(:,35),ZERO,G1(:,:,:,14))
  call loop_AW_Q(G1(:,:,:,14),wf(:,5),G1(:,:,:,15))
  call loop_A_Q(G1(:,:,:,15),Q(:,47),ZERO,G2(:,:,:,17))
  call check_last_AQ_V(l_switch,G2(:,:,:,17),wf(:,-4),G2tensor(:,13))
  call check_last_AQ_V(l_switch,G1(:,:,:,14),wf(:,13),G1tensor(:,1))
  call loop_VA_Q(G0(:,:,:,1),wf(:,45),G0(:,:,:,4))
  call loop_A_Q(G0(:,:,:,4),Q(:,44),ZERO,G1(:,:,:,16))
  call loop_AZ_Q(G1(:,:,:,16),wf(:,4),G1(:,:,:,17),gZu)
  call loop_A_Q(G1(:,:,:,17),Q(:,47),ZERO,G2(:,:,:,18))
  call check_last_AQ_V(l_switch,G2(:,:,:,18),wf(:,-4),G2tensor(:,14))
  call check_last_AQ_V(l_switch,G1(:,:,:,16),wf(:,10),G1tensor(:,2))
  call loop_VA_Q(G0(:,:,:,1),wf(:,54),G0(:,:,:,5))
  call loop_A_Q(G0(:,:,:,5),Q(:,37),ZERO,G1(:,:,:,18))
  call loop_AW_Q(G1(:,:,:,18),wf(:,23),G1(:,:,:,19))
  call loop_A_Q(G1(:,:,:,19),Q(:,47),ZERO,G2(:,:,:,19))
  call check_last_AQ_V(l_switch,G2(:,:,:,19),wf(:,-4),G2tensor(:,15))
  call check_last_AQ_V(l_switch,G1(:,:,:,18),wf(:,30),G1tensor(:,3))
  call loop_VA_Q(G0(:,:,:,1),wf(:,56),G0(:,:,:,6))
  call loop_A_Q(G0(:,:,:,6),Q(:,42),ZERO,G1(:,:,:,20))
  call loop_AZ_Q(G1(:,:,:,20),wf(:,22),G1(:,:,:,21),gZu)
  call loop_A_Q(G1(:,:,:,21),Q(:,47),ZERO,G2(:,:,:,20))
  call check_last_AQ_V(l_switch,G2(:,:,:,20),wf(:,-4),G2tensor(:,16))
  call check_last_AQ_V(l_switch,G1(:,:,:,20),wf(:,27),G1tensor(:,4))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (-(c(3)*M(1))) * den(8)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,1)
  Gcoeff = (-(c(3)*M(1))) * den(41)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,13)
  Gcoeff = (-(c(3)*M(1))) * den(11)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(3)*M(1))) * den(44)
  T2sum(1:15,4) = T2sum(1:15,4) + Gcoeff * G2tensor(:,14)
  Gcoeff = (-(c(2)*M(1))) * den(56)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,3)
  Gcoeff = (-(c(3)*M(1))) * den(4)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,1)
  Gcoeff = (-(c(3)*M(1))) * den(4)
  T3sum(1:35,2) = T3sum(1:35,2) + Gcoeff * G3tensor(:,2)
  Gcoeff = (-(c(3)*M(1))) * den(57)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,4)
  Gcoeff = (-(c(3)*M(1))) * den(59)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(3)*M(1)) * den(24)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,6)
  Gcoeff = (c(3)*M(1)) * den(47)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,15)
  Gcoeff = (c(3)*M(1)) * den(27)
  T2sum(1:15,8) = T2sum(1:15,8) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(3)*M(1)) * den(50)
  T2sum(1:15,9) = T2sum(1:15,9) + Gcoeff * G2tensor(:,16)
  Gcoeff = (c(2)*M(1)) * den(60)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(3)*M(1)) * den(20)
  T3sum(1:35,3) = T3sum(1:35,3) + Gcoeff * G3tensor(:,3)
  Gcoeff = (c(3)*M(1)) * den(20)
  T3sum(1:35,4) = T3sum(1:35,4) + Gcoeff * G3tensor(:,4)
  Gcoeff = (c(3)*M(1)) * den(61)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(3)*M(1)) * den(63)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(1)*M(1)) * den(64)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,11)
  Gcoeff = (-(c(1)*M(1))) * den(65)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,12)
  Gcoeff = (-(c(3)*M(1))) * den(51)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,2)
  Gcoeff = (-(c(3)*M(1))) * den(52)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(3)*M(1)) * den(53)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,4)
  Gcoeff = (c(3)*M(1)) * den(54)
  T1sum(1:5,4) = T1sum(1:5,4) + Gcoeff * G1tensor(:,3)

end subroutine vamp_1

end module ol_vamp_1_ppllll_nenexnexeudx_1_/**/REALKIND
