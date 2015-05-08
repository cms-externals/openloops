
module ol_vamp_1_ppllll_nexnmemxddx_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_1(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppllll_nexnmemxddx_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppllll_nexnmemxddx_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppllll_nexnmemxddx_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppllll_nexnmemxddx_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,3) :: G0
  complex(REALKIND), dimension(4,5,4,12) :: G1
  complex(REALKIND), dimension(4,15,4,11) :: G2
  complex(REALKIND), dimension(4,35,4,1) :: G3
  complex(REALKIND), dimension(5,1) :: G1tensor
  complex(REALKIND), dimension(15,10) :: G2tensor
  complex(REALKIND), dimension(35,1) :: G3tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_VA_Q(G0(:,:,:,1),wf(:,28),G0(:,:,:,2))
  call loop_A_Q(G0(:,:,:,2),Q(:,37),ZERO,G1(:,:,:,1))
  call loop_AW_Q(G1(:,:,:,1),wf(:,5),G1(:,:,:,2))
  call loop_A_Q(G1(:,:,:,2),Q(:,47),ZERO,G2(:,:,:,1))
  call check_last_AQ_V(l_switch,G2(:,:,:,1),wf(:,-4),G2tensor(:,1))
  call check_last_AQ_V(l_switch,G1(:,:,:,1),wf(:,11),G1tensor(:,1))
  call loop_VA_Q(G0(:,:,:,1),wf(:,-5),G0(:,:,:,3))
  call loop_A_Q(G0(:,:,:,3),Q(:,32),ZERO,G1(:,:,:,3))
  call loop_AW_Q(G1(:,:,:,3),wf(:,4),G1(:,:,:,4))
  call loop_A_Q(G1(:,:,:,4),Q(:,37),ZERO,G2(:,:,:,2))
  call check_last_AQ_V(l_switch,G2(:,:,:,2),wf(:,11),G2tensor(:,2))
  call loop_AW_Q(G2(:,:,:,2),wf(:,5),G2(:,:,:,3))
  call loop_A_Q(G2(:,:,:,3),Q(:,47),ZERO,G3(:,:,:,1))
  call check_last_AQ_V(l_switch,G3(:,:,:,1),wf(:,-4),G3tensor(:,1))
  call loop_AV_Q(G1(:,:,:,3),wf(:,6),G1(:,:,:,5))
  call loop_A_Q(G1(:,:,:,5),Q(:,47),ZERO,G2(:,:,:,4))
  call check_last_AQ_V(l_switch,G2(:,:,:,4),wf(:,-4),G2tensor(:,3))
  call loop_AZ_Q(G1(:,:,:,3),wf(:,41),G1(:,:,:,6),gZd)
  call loop_A_Q(G1(:,:,:,6),Q(:,47),ZERO,G2(:,:,:,5))
  call check_last_AQ_V(l_switch,G2(:,:,:,5),wf(:,-4),G2tensor(:,4))
  call loop_AV_Q(G1(:,:,:,3),wf(:,42),G1(:,:,:,7))
  call loop_A_Q(G1(:,:,:,7),Q(:,47),ZERO,G2(:,:,:,6))
  call check_last_AQ_V(l_switch,G2(:,:,:,6),wf(:,-4),G2tensor(:,5))
  call loop_AZ_Q(G1(:,:,:,3),wf(:,44),G1(:,:,:,8),gZd)
  call loop_A_Q(G1(:,:,:,8),Q(:,47),ZERO,G2(:,:,:,7))
  call check_last_AQ_V(l_switch,G2(:,:,:,7),wf(:,-4),G2tensor(:,6))
  call loop_AZ_Q(G1(:,:,:,3),wf(:,47),G1(:,:,:,9),gZd)
  call loop_A_Q(G1(:,:,:,9),Q(:,47),ZERO,G2(:,:,:,8))
  call check_last_AQ_V(l_switch,G2(:,:,:,8),wf(:,-4),G2tensor(:,7))
  call loop_AV_Q(G1(:,:,:,3),wf(:,48),G1(:,:,:,10))
  call loop_A_Q(G1(:,:,:,10),Q(:,47),ZERO,G2(:,:,:,9))
  call check_last_AQ_V(l_switch,G2(:,:,:,9),wf(:,-4),G2tensor(:,8))
  call loop_AZ_Q(G1(:,:,:,3),wf(:,50),G1(:,:,:,11),gZd)
  call loop_A_Q(G1(:,:,:,11),Q(:,47),ZERO,G2(:,:,:,10))
  call check_last_AQ_V(l_switch,G2(:,:,:,10),wf(:,-4),G2tensor(:,9))
  call loop_AZ_Q(G1(:,:,:,3),wf(:,53),G1(:,:,:,12),gZd)
  call loop_A_Q(G1(:,:,:,12),Q(:,47),ZERO,G2(:,:,:,11))
  call check_last_AQ_V(l_switch,G2(:,:,:,11),wf(:,-4),G2tensor(:,10))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(1)*M(1)) * den(27)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(1)*M(1)) * den(10)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(3)*M(1))) * den(30)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(2)*M(1)) * den(32)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(1)*M(1)) * den(4)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,1)
  Gcoeff = (c(3)*M(1)) * den(33)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(4)*M(1)) * den(34)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,6)
  Gcoeff = (c(4)*M(1)) * den(37)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(3)*M(1)) * den(38)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(4)*M(1)) * den(39)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(4)*M(1)) * den(42)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(1)*M(1)) * den(28)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,1)

end subroutine vamp_1

end module ol_vamp_1_ppllll_nexnmemxddx_1_/**/REALKIND
