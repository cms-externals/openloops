
module ol_vamp_1_ppjj_ddxbbx_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_1(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppjj_ddxbbx_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppjj_ddxbbx_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppjj_ddxbbx_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppjj_ddxbbx_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(2)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,7) :: G0
  complex(REALKIND), dimension(4,5,4,18) :: G1
  complex(REALKIND), dimension(4,15,4,5) :: G2
  complex(REALKIND), dimension(15,11) :: G2tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_VA_Q(G0(:,:,:,1),wf(:,-3),G0(:,:,:,2))
  call loop_A_Q(G0(:,:,:,2),Q(:,8),MB,G1(:,:,:,1))
  call loop_AV_Q(G1(:,:,:,1),wf(:,1),G1(:,:,:,2))
  call loop_A_Q(G1(:,:,:,2),Q(:,11),MB,G2(:,:,:,1))
  call check_last_AQ_V(l_switch,G2(:,:,:,1),wf(:,-2),G2tensor(:,1))
  call loop_AQ_V(G1(:,:,:,1),wf(:,-2),G1(:,:,:,3))
  call loop_VA_Q(G1(:,:,:,3),wf(:,-1),G1(:,:,:,4))
  call loop_A_Q(G1(:,:,:,4),Q(:,14),ZERO,G2(:,:,:,2))
  call check_last_AQ_V(l_switch,G2(:,:,:,2),wf(:,0),G2tensor(:,2))
  call loop_VQ_A(G0(:,:,:,1),wf(:,-2),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,4),MB,G1(:,:,:,5))
  call loop_QA_V(G1(:,:,:,5),wf(:,-3),G1(:,:,:,6))
  call check_last_UV_W(l_switch,G1(:,:,:,6),Q(:,12),wf(:,1),Q(:,3),G2tensor(:,3))
  call loop_VA_Q(G1(:,:,:,6),wf(:,-1),G1(:,:,:,7))
  call loop_A_Q(G1(:,:,:,7),Q(:,14),ZERO,G2(:,:,:,3))
  call check_last_AQ_V(l_switch,G2(:,:,:,3),wf(:,0),G2tensor(:,4))
  call loop_VA_Q(G0(:,:,:,1),wf(:,-1),G0(:,:,:,4))
  call loop_A_Q(G0(:,:,:,4),Q(:,2),ZERO,G1(:,:,:,8))
  call loop_AV_Q(G1(:,:,:,8),wf(:,2),G1(:,:,:,9))
  call loop_A_Q(G1(:,:,:,9),Q(:,14),ZERO,G2(:,:,:,4))
  call check_last_AQ_V(l_switch,G2(:,:,:,4),wf(:,0),G2tensor(:,5))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,2),Q(:,12),G1(:,:,:,10))
  call loop_VA_Q(G1(:,:,:,10),wf(:,-1),G1(:,:,:,11))
  call loop_A_Q(G1(:,:,:,11),Q(:,14),ZERO,G2(:,:,:,5))
  call check_last_AQ_V(l_switch,G2(:,:,:,5),wf(:,0),G2tensor(:,6))
  call check_last_UV_W(l_switch,G1(:,:,:,10),Q(:,12),wf(:,1),Q(:,3),G2tensor(:,7))
  call loop_QV_A(G0(:,:,:,1),wf(:,2),G0(:,:,:,5))
  call loop_Q_A(G0(:,:,:,5),Q(:,12),ZERO,G1(:,:,:,12))
  call loop_QV_A(G1(:,:,:,12),wf(:,1),G1(:,:,:,13))
  call check_last_Q_A(l_switch,G1(:,:,:,13),Q(:,15),ZERO,G2tensor(:,8))
  call loop_QV_A(G0(:,:,:,1),wf(:,2),G0(:,:,:,6))
  call loop_Q_A(G0(:,:,:,6),Q(:,12),MT,G1(:,:,:,14))
  call loop_QV_A(G1(:,:,:,14),wf(:,1),G1(:,:,:,15))
  call check_last_Q_A(l_switch,G1(:,:,:,15),Q(:,15),MT,G2tensor(:,9))
  call loop_QV_A(G0(:,:,:,1),wf(:,2),G0(:,:,:,7))
  call loop_Q_A(G0(:,:,:,7),Q(:,12),MB,G1(:,:,:,16))
  call loop_QV_A(G1(:,:,:,16),wf(:,1),G1(:,:,:,17))
  call check_last_Q_A(l_switch,G1(:,:,:,17),Q(:,15),MB,G2tensor(:,10))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,2),Q(:,12),G1(:,:,:,18))
  call check_last_CV_D(l_switch,G1(:,:,:,18),Q(:,12),wf(:,1),Q(:,3),G2tensor(:,11))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (-(c(6)*M(1))+c(5)*M(2)) * den(1)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,1)
  Gcoeff = (-(c(2)*M(1))+c(1)*M(2)) * den(1)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,3)
  Gcoeff = (-(c(6)*M(1))+c(5)*M(2)) * den(2)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(2)*M(1)-c(1)*M(2)) * den(2)
  T2sum(1:15,4) = T2sum(1:15,4) + Gcoeff * G2tensor(:,6)
  Gcoeff = (-(c(7)*M(1))+c(8)*M(2))
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(10)*M(1)+c(5)*M(2))
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(15)*M(1))+c(14)*M(2)) * den(3)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,8)
  Gcoeff = (-(c(13)*M(1))+c(12)*M(2)) * den(3)
  T2sum(1:15,8) = T2sum(1:15,8) + Gcoeff * G2tensor(:,9)
  Gcoeff = (-(c(15)*M(1))+c(14)*M(2)) * den(3)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,8)
  Gcoeff = (-(c(13)*M(1))+c(12)*M(2)) * den(3)
  T2sum(1:15,9) = T2sum(1:15,9) + Gcoeff * G2tensor(:,10)
  Gcoeff = (-(c(11)*M(1))+c(9)*M(2)) * den(3)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,11)
  Gcoeff = (c(4)*M(1)-c(3)*M(2)) * den(3)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,7)

end subroutine vamp_1

end module ol_vamp_1_ppjj_ddxbbx_1_/**/REALKIND
