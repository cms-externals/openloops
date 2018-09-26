
module ol_vamp_2_pphttj_ttxddxhg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_2(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_pphttj_ttxddxhg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_pphttj_ttxddxhg_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_pphttj_ttxddxhg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_pphttj_ttxddxhg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(4)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,5) :: G0
  complex(REALKIND), dimension(4,5,4,14) :: G1
  complex(REALKIND), dimension(15,12) :: G2tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_QV_A(G0(:,:,:,1),wf(:,25),G0(:,:,:,2))
  call loop_Q_A(G0(:,:,:,2),Q(:,44),MB,G1(:,:,:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,7),G1(:,:,:,2))
  call check_last_Q_A(l_switch,G1(:,:,:,2),Q(:,63),MB,G2tensor(:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,17),G1(:,:,:,3))
  call check_last_Q_A(l_switch,G1(:,:,:,3),Q(:,63),MB,G2tensor(:,2))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,25),Q(:,44),G1(:,:,:,4))
  call check_last_CV_D(l_switch,G1(:,:,:,4),Q(:,44),wf(:,7),Q(:,19),G2tensor(:,3))
  call check_last_CV_D(l_switch,G1(:,:,:,4),Q(:,44),wf(:,17),Q(:,19),G2tensor(:,4))
  call loop_QV_A(G0(:,:,:,1),wf(:,28),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,44),ZERO,G1(:,:,:,5))
  call loop_QV_A(G1(:,:,:,5),wf(:,7),G1(:,:,:,6))
  call check_last_Q_A(l_switch,G1(:,:,:,6),Q(:,63),ZERO,G2tensor(:,5))
  call loop_QV_A(G1(:,:,:,5),wf(:,17),G1(:,:,:,7))
  call check_last_Q_A(l_switch,G1(:,:,:,7),Q(:,63),ZERO,G2tensor(:,6))
  call loop_QV_A(G0(:,:,:,1),wf(:,28),G0(:,:,:,4))
  call loop_Q_A(G0(:,:,:,4),Q(:,44),MT,G1(:,:,:,8))
  call loop_QV_A(G1(:,:,:,8),wf(:,7),G1(:,:,:,9))
  call check_last_Q_A(l_switch,G1(:,:,:,9),Q(:,63),MT,G2tensor(:,7))
  call loop_QV_A(G1(:,:,:,8),wf(:,17),G1(:,:,:,10))
  call check_last_Q_A(l_switch,G1(:,:,:,10),Q(:,63),MT,G2tensor(:,8))
  call loop_QV_A(G0(:,:,:,1),wf(:,28),G0(:,:,:,5))
  call loop_Q_A(G0(:,:,:,5),Q(:,44),MB,G1(:,:,:,11))
  call loop_QV_A(G1(:,:,:,11),wf(:,7),G1(:,:,:,12))
  call check_last_Q_A(l_switch,G1(:,:,:,12),Q(:,63),MB,G2tensor(:,9))
  call loop_QV_A(G1(:,:,:,11),wf(:,17),G1(:,:,:,13))
  call check_last_Q_A(l_switch,G1(:,:,:,13),Q(:,63),MB,G2tensor(:,10))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,28),Q(:,44),G1(:,:,:,14))
  call check_last_CV_D(l_switch,G1(:,:,:,14),Q(:,44),wf(:,7),Q(:,19),G2tensor(:,11))
  call check_last_CV_D(l_switch,G1(:,:,:,14),Q(:,44),wf(:,17),Q(:,19),G2tensor(:,12))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (-(c(27)*M(2))+c(26)*M(4)) * den(117)
  T2sum(1:15,56) = T2sum(1:15,56) + Gcoeff * G2tensor(:,1)
  Gcoeff = (-(c(23)*M(2))+c(19)*M(4)) * den(117)
  T2sum(1:15,51) = T2sum(1:15,51) + Gcoeff * G2tensor(:,3)
  Gcoeff = (-(c(29)*M(3))+c(28)*M(4)) * den(121)
  T2sum(1:15,51) = T2sum(1:15,51) + Gcoeff * G2tensor(:,5)
  Gcoeff = (-(c(27)*M(3))+c(26)*M(4)) * den(121)
  T2sum(1:15,55) = T2sum(1:15,55) + Gcoeff * G2tensor(:,7)
  Gcoeff = (-(c(29)*M(3))+c(28)*M(4)) * den(121)
  T2sum(1:15,51) = T2sum(1:15,51) + Gcoeff * G2tensor(:,5)
  Gcoeff = (-(c(27)*M(3))+c(26)*M(4)) * den(121)
  T2sum(1:15,56) = T2sum(1:15,56) + Gcoeff * G2tensor(:,9)
  Gcoeff = (-(c(23)*M(3))+c(19)*M(4)) * den(121)
  T2sum(1:15,51) = T2sum(1:15,51) + Gcoeff * G2tensor(:,11)
  Gcoeff = (-(c(27)*M(2))+c(26)*M(4)) * den(125)
  T2sum(1:15,56) = T2sum(1:15,56) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(23)*M(2))+c(19)*M(4)) * den(125)
  T2sum(1:15,51) = T2sum(1:15,51) + Gcoeff * G2tensor(:,4)
  Gcoeff = (-(c(29)*M(3))+c(28)*M(4)) * den(128)
  T2sum(1:15,51) = T2sum(1:15,51) + Gcoeff * G2tensor(:,6)
  Gcoeff = (-(c(27)*M(3))+c(26)*M(4)) * den(128)
  T2sum(1:15,55) = T2sum(1:15,55) + Gcoeff * G2tensor(:,8)
  Gcoeff = (-(c(29)*M(3))+c(28)*M(4)) * den(128)
  T2sum(1:15,51) = T2sum(1:15,51) + Gcoeff * G2tensor(:,6)
  Gcoeff = (-(c(27)*M(3))+c(26)*M(4)) * den(128)
  T2sum(1:15,56) = T2sum(1:15,56) + Gcoeff * G2tensor(:,10)
  Gcoeff = (-(c(23)*M(3))+c(19)*M(4)) * den(128)
  T2sum(1:15,51) = T2sum(1:15,51) + Gcoeff * G2tensor(:,12)

end subroutine vamp_2

end module ol_vamp_2_pphttj_ttxddxhg_1_/**/REALKIND
