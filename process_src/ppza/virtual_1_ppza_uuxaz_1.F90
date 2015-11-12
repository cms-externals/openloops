
module ol_vamp_1_ppza_uuxaz_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_1(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppza_uuxaz_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppza_uuxaz_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppza_uuxaz_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppza_uuxaz_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,6) :: G0
  complex(REALKIND), dimension(4,5,4,11) :: G1
  complex(REALKIND), dimension(4,15,4,8) :: G2
  complex(REALKIND), dimension(4,35,4,2) :: G3
  complex(REALKIND), dimension(5,2) :: G1tensor
  complex(REALKIND), dimension(15,4) :: G2tensor
  complex(REALKIND), dimension(35,2) :: G3tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_VQ_A(G0(:,:,:,1),wf(:,3),G0(:,:,:,2))
  call loop_Q_A(G0(:,:,:,2),Q(:,5),ZERO,G1(:,:,:,1))
  call loop_QZ_A(G1(:,:,:,1),wf(:,-3),G1(:,:,:,2),gZu)
  call loop_Q_A(G1(:,:,:,2),Q(:,13),ZERO,G2(:,:,:,1))
  call check_last_QA_V(l_switch,G2(:,:,:,1),wf(:,-1),G2tensor(:,1))
  call loop_VA_Q(G0(:,:,:,1),wf(:,10),G0(:,:,:,3))
  call loop_A_Q(G0(:,:,:,3),Q(:,6),ZERO,G1(:,:,:,3))
  call loop_AZ_Q(G1(:,:,:,3),wf(:,-3),G1(:,:,:,4),gZu)
  call loop_A_Q(G1(:,:,:,4),Q(:,14),ZERO,G2(:,:,:,2))
  call check_last_AQ_V(l_switch,G2(:,:,:,2),wf(:,0),G2tensor(:,2))
  call loop_VQ_A(G0(:,:,:,1),wf(:,6),G0(:,:,:,4))
  call loop_Q_A(G0(:,:,:,4),Q(:,9),ZERO,G1(:,:,:,5))
  call loop_QV_A(G1(:,:,:,5),wf(:,-2),G1(:,:,:,6))
  call loop_Q_A(G1(:,:,:,6),Q(:,13),ZERO,G2(:,:,:,3))
  call check_last_QA_V(l_switch,G2(:,:,:,3),wf(:,-1),G2tensor(:,3))
  call check_last_QA_V(l_switch,G1(:,:,:,5),wf(:,10),G1tensor(:,1))
  call loop_VA_Q(G0(:,:,:,1),wf(:,12),G0(:,:,:,5))
  call loop_A_Q(G0(:,:,:,5),Q(:,10),ZERO,G1(:,:,:,7))
  call loop_AV_Q(G1(:,:,:,7),wf(:,-2),G1(:,:,:,8))
  call loop_A_Q(G1(:,:,:,8),Q(:,14),ZERO,G2(:,:,:,4))
  call check_last_AQ_V(l_switch,G2(:,:,:,4),wf(:,0),G2tensor(:,4))
  call check_last_AQ_V(l_switch,G1(:,:,:,7),wf(:,3),G1tensor(:,2))
  call loop_VA_Q(G0(:,:,:,1),wf(:,-1),G0(:,:,:,6))
  call loop_A_Q(G0(:,:,:,6),Q(:,2),ZERO,G1(:,:,:,9))
  call loop_AZ_Q(G1(:,:,:,9),wf(:,-3),G1(:,:,:,10),gZu)
  call loop_A_Q(G1(:,:,:,10),Q(:,10),ZERO,G2(:,:,:,5))
  call loop_AV_Q(G2(:,:,:,5),wf(:,-2),G2(:,:,:,6))
  call loop_A_Q(G2(:,:,:,6),Q(:,14),ZERO,G3(:,:,:,1))
  call check_last_AQ_V(l_switch,G3(:,:,:,1),wf(:,0),G3tensor(:,1))
  call loop_AV_Q(G1(:,:,:,9),wf(:,-2),G1(:,:,:,11))
  call loop_A_Q(G1(:,:,:,11),Q(:,6),ZERO,G2(:,:,:,7))
  call loop_AZ_Q(G2(:,:,:,7),wf(:,-3),G2(:,:,:,8),gZu)
  call loop_A_Q(G2(:,:,:,8),Q(:,14),ZERO,G3(:,:,:,2))
  call check_last_AQ_V(l_switch,G3(:,:,:,2),wf(:,0),G3tensor(:,2))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (-(c(1)*M(1))) * den(1)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,1)
  Gcoeff = (-(c(1)*M(1))) * den(3)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(1)*M(1))) * den(2)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,3)
  Gcoeff = (-(c(1)*M(1))) * den(4)
  T2sum(1:15,4) = T2sum(1:15,4) + Gcoeff * G2tensor(:,4)
  Gcoeff = (-(c(1)*M(1)))
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,1)
  Gcoeff = (-(c(1)*M(1)))
  T3sum(1:35,2) = T3sum(1:35,2) + Gcoeff * G3tensor(:,2)
  Gcoeff = (-(c(1)*M(1))) * den(5)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,2)
  Gcoeff = (-(c(1)*M(1))) * den(6)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,1)

end subroutine vamp_1

end module ol_vamp_1_ppza_uuxaz_1_/**/REALKIND
