
module ol_vamp_3_ppzjjj_uuxddxzg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_3(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppzjjj_uuxddxzg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppzjjj_uuxddxzg_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppzjjj_uuxddxzg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppzjjj_uuxddxzg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(4)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,6) :: G0
  complex(REALKIND), dimension(4,5,4,17) :: G1
  complex(REALKIND), dimension(15,14) :: G2tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_QV_A(G0(:,:,:,1),wf(:,47),G0(:,:,:,2))
  call loop_Q_A(G0(:,:,:,2),Q(:,35),MT,G1(:,:,:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,10),G1(:,:,:,2))
  call check_last_Q_A(l_switch,G1(:,:,:,2),Q(:,63),MT,G2tensor(:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,20),G1(:,:,:,3))
  call check_last_Q_A(l_switch,G1(:,:,:,3),Q(:,63),MT,G2tensor(:,2))
  call loop_QV_A(G0(:,:,:,1),wf(:,47),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,35),MB,G1(:,:,:,4))
  call loop_QV_A(G1(:,:,:,4),wf(:,10),G1(:,:,:,5))
  call check_last_Q_A(l_switch,G1(:,:,:,5),Q(:,63),MB,G2tensor(:,3))
  call loop_QV_A(G1(:,:,:,4),wf(:,20),G1(:,:,:,6))
  call check_last_Q_A(l_switch,G1(:,:,:,6),Q(:,63),MB,G2tensor(:,4))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,47),Q(:,35),G1(:,:,:,7))
  call check_last_CV_D(l_switch,G1(:,:,:,7),Q(:,35),wf(:,10),Q(:,28),G2tensor(:,5))
  call check_last_CV_D(l_switch,G1(:,:,:,7),Q(:,35),wf(:,20),Q(:,28),G2tensor(:,6))
  call loop_QV_A(G0(:,:,:,1),wf(:,48),G0(:,:,:,4))
  call loop_Q_A(G0(:,:,:,4),Q(:,35),ZERO,G1(:,:,:,8))
  call loop_QV_A(G1(:,:,:,8),wf(:,10),G1(:,:,:,9))
  call check_last_Q_A(l_switch,G1(:,:,:,9),Q(:,63),ZERO,G2tensor(:,7))
  call loop_QV_A(G1(:,:,:,8),wf(:,20),G1(:,:,:,10))
  call check_last_Q_A(l_switch,G1(:,:,:,10),Q(:,63),ZERO,G2tensor(:,8))
  call loop_QV_A(G0(:,:,:,1),wf(:,48),G0(:,:,:,5))
  call loop_Q_A(G0(:,:,:,5),Q(:,35),MT,G1(:,:,:,11))
  call loop_QV_A(G1(:,:,:,11),wf(:,10),G1(:,:,:,12))
  call check_last_Q_A(l_switch,G1(:,:,:,12),Q(:,63),MT,G2tensor(:,9))
  call loop_QV_A(G1(:,:,:,11),wf(:,20),G1(:,:,:,13))
  call check_last_Q_A(l_switch,G1(:,:,:,13),Q(:,63),MT,G2tensor(:,10))
  call loop_QV_A(G0(:,:,:,1),wf(:,48),G0(:,:,:,6))
  call loop_Q_A(G0(:,:,:,6),Q(:,35),MB,G1(:,:,:,14))
  call loop_QV_A(G1(:,:,:,14),wf(:,10),G1(:,:,:,15))
  call check_last_Q_A(l_switch,G1(:,:,:,15),Q(:,63),MB,G2tensor(:,11))
  call loop_QV_A(G1(:,:,:,14),wf(:,20),G1(:,:,:,16))
  call check_last_Q_A(l_switch,G1(:,:,:,16),Q(:,63),MB,G2tensor(:,12))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,48),Q(:,35),G1(:,:,:,17))
  call check_last_CV_D(l_switch,G1(:,:,:,17),Q(:,35),wf(:,10),Q(:,28),G2tensor(:,13))
  call check_last_CV_D(l_switch,G1(:,:,:,17),Q(:,35),wf(:,20),Q(:,28),G2tensor(:,14))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (-(c(23)*M(1))+c(24)*M(3)) * den(209)
  T2sum(1:15,99) = T2sum(1:15,99) + Gcoeff * G2tensor(:,1)
  Gcoeff = (-(c(23)*M(1))+c(24)*M(3)) * den(209)
  T2sum(1:15,100) = T2sum(1:15,100) + Gcoeff * G2tensor(:,3)
  Gcoeff = (-(c(16)*M(1))+c(20)*M(3)) * den(209)
  T2sum(1:15,94) = T2sum(1:15,94) + Gcoeff * G2tensor(:,5)
  Gcoeff = (-(c(25)*M(1))+c(26)*M(2)) * den(212)
  T2sum(1:15,94) = T2sum(1:15,94) + Gcoeff * G2tensor(:,7)
  Gcoeff = (-(c(23)*M(1))+c(24)*M(2)) * den(212)
  T2sum(1:15,99) = T2sum(1:15,99) + Gcoeff * G2tensor(:,9)
  Gcoeff = (-(c(25)*M(1))+c(26)*M(2)) * den(212)
  T2sum(1:15,94) = T2sum(1:15,94) + Gcoeff * G2tensor(:,7)
  Gcoeff = (-(c(23)*M(1))+c(24)*M(2)) * den(212)
  T2sum(1:15,100) = T2sum(1:15,100) + Gcoeff * G2tensor(:,11)
  Gcoeff = (-(c(16)*M(1))+c(20)*M(2)) * den(212)
  T2sum(1:15,94) = T2sum(1:15,94) + Gcoeff * G2tensor(:,13)
  Gcoeff = (-(c(23)*M(1))+c(24)*M(3)) * den(215)
  T2sum(1:15,99) = T2sum(1:15,99) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(23)*M(1))+c(24)*M(3)) * den(215)
  T2sum(1:15,100) = T2sum(1:15,100) + Gcoeff * G2tensor(:,4)
  Gcoeff = (-(c(16)*M(1))+c(20)*M(3)) * den(215)
  T2sum(1:15,94) = T2sum(1:15,94) + Gcoeff * G2tensor(:,6)
  Gcoeff = (-(c(25)*M(1))+c(26)*M(2)) * den(218)
  T2sum(1:15,94) = T2sum(1:15,94) + Gcoeff * G2tensor(:,8)
  Gcoeff = (-(c(23)*M(1))+c(24)*M(2)) * den(218)
  T2sum(1:15,99) = T2sum(1:15,99) + Gcoeff * G2tensor(:,10)
  Gcoeff = (-(c(25)*M(1))+c(26)*M(2)) * den(218)
  T2sum(1:15,94) = T2sum(1:15,94) + Gcoeff * G2tensor(:,8)
  Gcoeff = (-(c(23)*M(1))+c(24)*M(2)) * den(218)
  T2sum(1:15,100) = T2sum(1:15,100) + Gcoeff * G2tensor(:,12)
  Gcoeff = (-(c(16)*M(1))+c(20)*M(2)) * den(218)
  T2sum(1:15,94) = T2sum(1:15,94) + Gcoeff * G2tensor(:,14)

end subroutine vamp_3

end module ol_vamp_3_ppzjjj_uuxddxzg_1_/**/REALKIND
