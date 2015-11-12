
module ol_vamp_1_ppllll_nenexnmnmxbbx_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_1(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppllll_nenexnmnmxbbx_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppllll_nenexnmnmxbbx_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppllll_nenexnmnmxbbx_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppllll_nenexnmnmxbbx_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,4) :: G0
  complex(REALKIND), dimension(4,5,4,12) :: G1
  complex(REALKIND), dimension(4,15,4,11) :: G2
  complex(REALKIND), dimension(4,35,4,2) :: G3
  complex(REALKIND), dimension(5,2) :: G1tensor
  complex(REALKIND), dimension(15,9) :: G2tensor
  complex(REALKIND), dimension(35,2) :: G3tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_VA_Q(G0(:,:,:,1),wf(:,-5),G0(:,:,:,2))
  call loop_A_Q(G0(:,:,:,2),Q(:,32),MB,G1(:,:,:,1))
  call loop_AZ_Q(G1(:,:,:,1),wf(:,5),G1(:,:,:,2),gZd)
  call loop_A_Q(G1(:,:,:,2),Q(:,44),MB,G2(:,:,:,1))
  call check_last_AQ_V(l_switch,G2(:,:,:,1),wf(:,9),G2tensor(:,1))
  call loop_AZ_Q(G2(:,:,:,1),wf(:,4),G2(:,:,:,2),gZd)
  call loop_A_Q(G2(:,:,:,2),Q(:,47),MB,G3(:,:,:,1))
  call check_last_AQ_V(l_switch,G3(:,:,:,1),wf(:,-4),G3tensor(:,1))
  call loop_AZ_Q(G1(:,:,:,1),wf(:,4),G1(:,:,:,3),gZd)
  call loop_A_Q(G1(:,:,:,3),Q(:,35),MB,G2(:,:,:,3))
  call check_last_AQ_V(l_switch,G2(:,:,:,3),wf(:,12),G2tensor(:,2))
  call loop_AZ_Q(G2(:,:,:,3),wf(:,5),G2(:,:,:,4),gZd)
  call loop_A_Q(G2(:,:,:,4),Q(:,47),MB,G3(:,:,:,2))
  call check_last_AQ_V(l_switch,G3(:,:,:,2),wf(:,-4),G3tensor(:,2))
  call loop_AS_Q(G1(:,:,:,1),wf(:,6),G1(:,:,:,4),gH)
  call loop_A_Q(G1(:,:,:,4),Q(:,47),MB,G2(:,:,:,5))
  call check_last_AQ_V(l_switch,G2(:,:,:,5),wf(:,-4),G2tensor(:,3))
  call loop_AZ_Q(G1(:,:,:,1),wf(:,45),G1(:,:,:,5),gZd)
  call loop_A_Q(G1(:,:,:,5),Q(:,47),MB,G2(:,:,:,6))
  call check_last_AQ_V(l_switch,G2(:,:,:,6),wf(:,-4),G2tensor(:,4))
  call loop_AZ_Q(G1(:,:,:,1),wf(:,48),G1(:,:,:,6),gZd)
  call loop_A_Q(G1(:,:,:,6),Q(:,47),MB,G2(:,:,:,7))
  call check_last_AQ_V(l_switch,G2(:,:,:,7),wf(:,-4),G2tensor(:,5))
  call loop_AZ_Q(G1(:,:,:,1),wf(:,50),G1(:,:,:,7),gZd)
  call loop_A_Q(G1(:,:,:,7),Q(:,47),MB,G2(:,:,:,8))
  call check_last_AQ_V(l_switch,G2(:,:,:,8),wf(:,-4),G2tensor(:,6))
  call loop_AZ_Q(G1(:,:,:,1),wf(:,53),G1(:,:,:,8),gZd)
  call loop_A_Q(G1(:,:,:,8),Q(:,47),MB,G2(:,:,:,9))
  call check_last_AQ_V(l_switch,G2(:,:,:,9),wf(:,-4),G2tensor(:,7))
  call loop_VA_Q(G0(:,:,:,1),wf(:,30),G0(:,:,:,3))
  call loop_A_Q(G0(:,:,:,3),Q(:,35),MB,G1(:,:,:,9))
  call loop_AZ_Q(G1(:,:,:,9),wf(:,5),G1(:,:,:,10),gZd)
  call loop_A_Q(G1(:,:,:,10),Q(:,47),MB,G2(:,:,:,10))
  call check_last_AQ_V(l_switch,G2(:,:,:,10),wf(:,-4),G2tensor(:,8))
  call check_last_AQ_V(l_switch,G1(:,:,:,9),wf(:,12),G1tensor(:,1))
  call loop_VA_Q(G0(:,:,:,1),wf(:,32),G0(:,:,:,4))
  call loop_A_Q(G0(:,:,:,4),Q(:,44),MB,G1(:,:,:,11))
  call loop_AZ_Q(G1(:,:,:,11),wf(:,4),G1(:,:,:,12),gZd)
  call loop_A_Q(G1(:,:,:,12),Q(:,47),MB,G2(:,:,:,11))
  call check_last_AQ_V(l_switch,G2(:,:,:,11),wf(:,-4),G2tensor(:,9))
  call check_last_AQ_V(l_switch,G1(:,:,:,11),wf(:,9),G1tensor(:,2))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(1)*M(1)) * den(8)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(1)*M(1)) * den(27)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(1)*M(1)) * den(11)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(1)*M(1)) * den(30)
  T2sum(1:15,4) = T2sum(1:15,4) + Gcoeff * G2tensor(:,9)
  Gcoeff = (-(c(2)*M(1))) * den(34)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(1)*M(1)) * den(4)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,1)
  Gcoeff = (c(1)*M(1)) * den(4)
  T3sum(1:35,2) = T3sum(1:35,2) + Gcoeff * G3tensor(:,2)
  Gcoeff = (c(1)*M(1)) * den(36)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(1)*M(1)) * den(39)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(1)*M(1)) * den(40)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,6)
  Gcoeff = (c(1)*M(1)) * den(43)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(1)*M(1)) * den(31)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,2)
  Gcoeff = (c(1)*M(1)) * den(32)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,1)

end subroutine vamp_1

end module ol_vamp_1_ppllll_nenexnmnmxbbx_1_/**/REALKIND
