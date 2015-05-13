
module ol_vamp_1_ppllll_nenexnmmxuxd_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_1(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppllll_nenexnmmxuxd_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppllll_nenexnmmxuxd_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppllll_nenexnmmxuxd_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppllll_nenexnmmxuxd_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,4) :: G0
  complex(REALKIND), dimension(4,5,4,11) :: G1
  complex(REALKIND), dimension(4,15,4,10) :: G2
  complex(REALKIND), dimension(4,35,4,2) :: G3
  complex(REALKIND), dimension(5,2) :: G1tensor
  complex(REALKIND), dimension(15,8) :: G2tensor
  complex(REALKIND), dimension(35,2) :: G3tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_VQ_A(G0(:,:,:,1),wf(:,-5),G0(:,:,:,2))
  call loop_Q_A(G0(:,:,:,2),Q(:,32),ZERO,G1(:,:,:,1))
  call loop_QW_A(G1(:,:,:,1),wf(:,5),G1(:,:,:,2))
  call loop_Q_A(G1(:,:,:,2),Q(:,44),ZERO,G2(:,:,:,1))
  call check_last_QA_V(l_switch,G2(:,:,:,1),wf(:,10),G2tensor(:,1))
  call loop_QZ_A(G2(:,:,:,1),wf(:,4),G2(:,:,:,2),gZu)
  call loop_Q_A(G2(:,:,:,2),Q(:,47),ZERO,G3(:,:,:,1))
  call check_last_QA_V(l_switch,G3(:,:,:,1),wf(:,-4),G3tensor(:,1))
  call loop_QZ_A(G1(:,:,:,1),wf(:,4),G1(:,:,:,3),gZd)
  call loop_Q_A(G1(:,:,:,3),Q(:,35),ZERO,G2(:,:,:,3))
  call check_last_QA_V(l_switch,G2(:,:,:,3),wf(:,13),G2tensor(:,2))
  call loop_QW_A(G2(:,:,:,3),wf(:,5),G2(:,:,:,4))
  call loop_Q_A(G2(:,:,:,4),Q(:,47),ZERO,G3(:,:,:,2))
  call check_last_QA_V(l_switch,G3(:,:,:,2),wf(:,-4),G3tensor(:,2))
  call loop_QW_A(G1(:,:,:,1),wf(:,38),G1(:,:,:,4))
  call loop_Q_A(G1(:,:,:,4),Q(:,47),ZERO,G2(:,:,:,5))
  call check_last_QA_V(l_switch,G2(:,:,:,5),wf(:,-4),G2tensor(:,3))
  call loop_QW_A(G1(:,:,:,1),wf(:,40),G1(:,:,:,5))
  call loop_Q_A(G1(:,:,:,5),Q(:,47),ZERO,G2(:,:,:,6))
  call check_last_QA_V(l_switch,G2(:,:,:,6),wf(:,-4),G2tensor(:,4))
  call loop_QW_A(G1(:,:,:,1),wf(:,43),G1(:,:,:,6))
  call loop_Q_A(G1(:,:,:,6),Q(:,47),ZERO,G2(:,:,:,7))
  call check_last_QA_V(l_switch,G2(:,:,:,7),wf(:,-4),G2tensor(:,5))
  call loop_QW_A(G1(:,:,:,1),wf(:,46),G1(:,:,:,7))
  call loop_Q_A(G1(:,:,:,7),Q(:,47),ZERO,G2(:,:,:,8))
  call check_last_QA_V(l_switch,G2(:,:,:,8),wf(:,-4),G2tensor(:,6))
  call loop_VQ_A(G0(:,:,:,1),wf(:,26),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,35),ZERO,G1(:,:,:,8))
  call loop_QW_A(G1(:,:,:,8),wf(:,5),G1(:,:,:,9))
  call loop_Q_A(G1(:,:,:,9),Q(:,47),ZERO,G2(:,:,:,9))
  call check_last_QA_V(l_switch,G2(:,:,:,9),wf(:,-4),G2tensor(:,7))
  call check_last_QA_V(l_switch,G1(:,:,:,8),wf(:,13),G1tensor(:,1))
  call loop_VQ_A(G0(:,:,:,1),wf(:,28),G0(:,:,:,4))
  call loop_Q_A(G0(:,:,:,4),Q(:,44),ZERO,G1(:,:,:,10))
  call loop_QZ_A(G1(:,:,:,10),wf(:,4),G1(:,:,:,11),gZu)
  call loop_Q_A(G1(:,:,:,11),Q(:,47),ZERO,G2(:,:,:,10))
  call check_last_QA_V(l_switch,G2(:,:,:,10),wf(:,-4),G2tensor(:,8))
  call check_last_QA_V(l_switch,G1(:,:,:,10),wf(:,10),G1tensor(:,2))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (-(c(3)*M(1))) * den(8)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,1)
  Gcoeff = (-(c(3)*M(1))) * den(23)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,7)
  Gcoeff = (-(c(3)*M(1))) * den(11)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(3)*M(1))) * den(26)
  T2sum(1:15,4) = T2sum(1:15,4) + Gcoeff * G2tensor(:,8)
  Gcoeff = (-(c(2)*M(1))) * den(30)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,3)
  Gcoeff = (-(c(3)*M(1))) * den(4)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,1)
  Gcoeff = (-(c(3)*M(1))) * den(4)
  T3sum(1:35,2) = T3sum(1:35,2) + Gcoeff * G3tensor(:,2)
  Gcoeff = (-(c(3)*M(1))) * den(31)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,4)
  Gcoeff = (-(c(3)*M(1))) * den(34)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,5)
  Gcoeff = (-(c(1)*M(1))) * den(37)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,6)
  Gcoeff = (-(c(3)*M(1))) * den(27)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,2)
  Gcoeff = (-(c(3)*M(1))) * den(28)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,1)

end subroutine vamp_1

end module ol_vamp_1_ppllll_nenexnmmxuxd_1_/**/REALKIND
