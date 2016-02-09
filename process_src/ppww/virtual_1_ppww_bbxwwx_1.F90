
module ol_vamp_1_ppww_bbxwwx_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_1(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppww_bbxwwx_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppww_bbxwwx_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppww_bbxwwx_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppww_bbxwwx_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,4) :: G0
  complex(REALKIND), dimension(4,5,4,9) :: G1
  complex(REALKIND), dimension(4,15,4,7) :: G2
  complex(REALKIND), dimension(4,35,4,1) :: G3
  complex(REALKIND), dimension(5,1) :: G1tensor
  complex(REALKIND), dimension(15,5) :: G2tensor
  complex(REALKIND), dimension(35,1) :: G3tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_VA_Q(G0(:,:,:,1),wf(:,12),G0(:,:,:,2))
  call loop_A_Q(G0(:,:,:,2),Q(:,6),MT,G1(:,:,:,1))
  call loop_AW_Q(G1(:,:,:,1),wf(:,-3),G1(:,:,:,2))
  call loop_A_Q(G1(:,:,:,2),Q(:,14),MB,G2(:,:,:,1))
  call check_last_AQ_V(l_switch,G2(:,:,:,1),wf(:,0),G2tensor(:,1))
  call loop_VQ_A(G0(:,:,:,1),wf(:,9),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,9),MT,G1(:,:,:,3))
  call loop_QW_A(G1(:,:,:,3),wf(:,-2),G1(:,:,:,4))
  call loop_Q_A(G1(:,:,:,4),Q(:,13),MB,G2(:,:,:,2))
  call check_last_QA_V(l_switch,G2(:,:,:,2),wf(:,-1),G2tensor(:,2))
  call check_last_QA_V(l_switch,G1(:,:,:,3),wf(:,12),G1tensor(:,1))
  call loop_VA_Q(G0(:,:,:,1),wf(:,-1),G0(:,:,:,4))
  call loop_A_Q(G0(:,:,:,4),Q(:,2),MB,G1(:,:,:,5))
  call loop_AS_Q(G1(:,:,:,5),wf(:,2),G1(:,:,:,6),gH)
  call loop_A_Q(G1(:,:,:,6),Q(:,14),MB,G2(:,:,:,3))
  call check_last_AQ_V(l_switch,G2(:,:,:,3),wf(:,0),G2tensor(:,3))
  call loop_AV_Q(G1(:,:,:,5),wf(:,4),G1(:,:,:,7))
  call loop_A_Q(G1(:,:,:,7),Q(:,14),MB,G2(:,:,:,4))
  call check_last_AQ_V(l_switch,G2(:,:,:,4),wf(:,0),G2tensor(:,4))
  call loop_AZ_Q(G1(:,:,:,5),wf(:,16),G1(:,:,:,8),gZd)
  call loop_A_Q(G1(:,:,:,8),Q(:,14),MB,G2(:,:,:,5))
  call check_last_AQ_V(l_switch,G2(:,:,:,5),wf(:,0),G2tensor(:,5))
  call loop_AW_Q(G1(:,:,:,5),wf(:,-2),G1(:,:,:,9))
  call loop_A_Q(G1(:,:,:,9),Q(:,6),MT,G2(:,:,:,6))
  call loop_AW_Q(G2(:,:,:,6),wf(:,-3),G2(:,:,:,7))
  call loop_A_Q(G2(:,:,:,7),Q(:,14),MB,G3(:,:,:,1))
  call check_last_AQ_V(l_switch,G3(:,:,:,1),wf(:,0),G3tensor(:,1))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(2)*M(1)) * den(5)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(2)*M(1)) * den(4)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(3)*M(1))) * den(6)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,3)
  Gcoeff = (-(c(1)*M(1))) * den(7)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(4)*M(1)) * den(8)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(2)*M(1))
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,1)
  Gcoeff = (c(2)*M(1)) * den(9)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,1)

end subroutine vamp_1

end module ol_vamp_1_ppww_bbxwwx_1_/**/REALKIND
