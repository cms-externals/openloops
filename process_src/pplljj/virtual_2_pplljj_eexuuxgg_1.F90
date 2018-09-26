
module ol_vamp_2_pplljj_eexuuxgg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_2(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_pplljj_eexuuxgg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_pplljj_eexuuxgg_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_pplljj_eexuuxgg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_pplljj_eexuuxgg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(3)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,4) :: G0
  complex(REALKIND), dimension(4,5,4,16) :: G1
  complex(REALKIND), dimension(15,16) :: G2tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_QV_A(G0(:,:,:,1),wf(:,28),G0(:,:,:,2))
  call loop_Q_A(G0(:,:,:,2),Q(:,48),ZERO,G1(:,:,:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,117),G1(:,:,:,2))
  call check_last_Q_A(l_switch,G1(:,:,:,2),Q(:,63),ZERO,G2tensor(:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,118),G1(:,:,:,3))
  call check_last_Q_A(l_switch,G1(:,:,:,3),Q(:,63),ZERO,G2tensor(:,2))
  call loop_QV_A(G1(:,:,:,1),wf(:,127),G1(:,:,:,4))
  call check_last_Q_A(l_switch,G1(:,:,:,4),Q(:,63),ZERO,G2tensor(:,3))
  call loop_QV_A(G1(:,:,:,1),wf(:,128),G1(:,:,:,5))
  call check_last_Q_A(l_switch,G1(:,:,:,5),Q(:,63),ZERO,G2tensor(:,4))
  call loop_QV_A(G0(:,:,:,1),wf(:,28),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,48),MT,G1(:,:,:,6))
  call loop_QV_A(G1(:,:,:,6),wf(:,117),G1(:,:,:,7))
  call check_last_Q_A(l_switch,G1(:,:,:,7),Q(:,63),MT,G2tensor(:,5))
  call loop_QV_A(G1(:,:,:,6),wf(:,118),G1(:,:,:,8))
  call check_last_Q_A(l_switch,G1(:,:,:,8),Q(:,63),MT,G2tensor(:,6))
  call loop_QV_A(G1(:,:,:,6),wf(:,127),G1(:,:,:,9))
  call check_last_Q_A(l_switch,G1(:,:,:,9),Q(:,63),MT,G2tensor(:,7))
  call loop_QV_A(G1(:,:,:,6),wf(:,128),G1(:,:,:,10))
  call check_last_Q_A(l_switch,G1(:,:,:,10),Q(:,63),MT,G2tensor(:,8))
  call loop_QV_A(G0(:,:,:,1),wf(:,28),G0(:,:,:,4))
  call loop_Q_A(G0(:,:,:,4),Q(:,48),MB,G1(:,:,:,11))
  call loop_QV_A(G1(:,:,:,11),wf(:,117),G1(:,:,:,12))
  call check_last_Q_A(l_switch,G1(:,:,:,12),Q(:,63),MB,G2tensor(:,9))
  call loop_QV_A(G1(:,:,:,11),wf(:,118),G1(:,:,:,13))
  call check_last_Q_A(l_switch,G1(:,:,:,13),Q(:,63),MB,G2tensor(:,10))
  call loop_QV_A(G1(:,:,:,11),wf(:,127),G1(:,:,:,14))
  call check_last_Q_A(l_switch,G1(:,:,:,14),Q(:,63),MB,G2tensor(:,11))
  call loop_QV_A(G1(:,:,:,11),wf(:,128),G1(:,:,:,15))
  call check_last_Q_A(l_switch,G1(:,:,:,15),Q(:,63),MB,G2tensor(:,12))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,28),Q(:,48),G1(:,:,:,16))
  call check_last_CV_D(l_switch,G1(:,:,:,16),Q(:,48),wf(:,117),Q(:,15),G2tensor(:,13))
  call check_last_CV_D(l_switch,G1(:,:,:,16),Q(:,48),wf(:,118),Q(:,15),G2tensor(:,14))
  call check_last_CV_D(l_switch,G1(:,:,:,16),Q(:,48),wf(:,127),Q(:,15),G2tensor(:,15))
  call check_last_CV_D(l_switch,G1(:,:,:,16),Q(:,48),wf(:,128),Q(:,15),G2tensor(:,16))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(26)*(-M(1)+M(2))) * den(160)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(24)*(-M(1)+M(2))) * den(160)
  T2sum(1:15,26) = T2sum(1:15,26) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(26)*(-M(1)+M(2))) * den(160)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(24)*(-M(1)+M(2))) * den(160)
  T2sum(1:15,27) = T2sum(1:15,27) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(27)*(M(1)-M(2))) * den(161)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(25)*(M(1)-M(2))) * den(161)
  T2sum(1:15,26) = T2sum(1:15,26) + Gcoeff * G2tensor(:,6)
  Gcoeff = (c(27)*(M(1)-M(2))) * den(161)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(25)*(M(1)-M(2))) * den(161)
  T2sum(1:15,27) = T2sum(1:15,27) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(9)*(-M(1)+M(2))) * den(160)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,13)
  Gcoeff = (c(14)*(M(1)-M(2))) * den(161)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,14)
  Gcoeff = (c(26)*(-M(1)+M(2))) * den(162)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(24)*(-M(1)+M(2))) * den(162)
  T2sum(1:15,26) = T2sum(1:15,26) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(26)*(-M(1)+M(2))) * den(162)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(24)*(-M(1)+M(2))) * den(162)
  T2sum(1:15,27) = T2sum(1:15,27) + Gcoeff * G2tensor(:,11)
  Gcoeff = (c(27)*(M(1)-M(2))) * den(163)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(25)*(M(1)-M(2))) * den(163)
  T2sum(1:15,26) = T2sum(1:15,26) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(27)*(M(1)-M(2))) * den(163)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(25)*(M(1)-M(2))) * den(163)
  T2sum(1:15,27) = T2sum(1:15,27) + Gcoeff * G2tensor(:,12)
  Gcoeff = (c(9)*(-M(1)+M(2))) * den(162)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,15)
  Gcoeff = (c(14)*(M(1)-M(2))) * den(163)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,16)

end subroutine vamp_2

end module ol_vamp_2_pplljj_eexuuxgg_1_/**/REALKIND
