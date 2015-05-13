
module ol_vamp_2_pphbbj_ddxbbxhg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_2(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_pphbbj_ddxbbxhg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_pphbbj_ddxbbxhg_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_pphbbj_ddxbbxhg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_pphbbj_ddxbbxhg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(4)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,3) :: G0
  complex(REALKIND), dimension(4,5,4,7) :: G1
  complex(REALKIND), dimension(15,6) :: G2tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_QV_A(G0(:,:,:,1),wf(:,28),G0(:,:,:,2))
  call loop_Q_A(G0(:,:,:,2),Q(:,35),MT,G1(:,:,:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,10),G1(:,:,:,2))
  call check_last_Q_A(l_switch,G1(:,:,:,2),Q(:,63),MT,G2tensor(:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,20),G1(:,:,:,3))
  call check_last_Q_A(l_switch,G1(:,:,:,3),Q(:,63),MT,G2tensor(:,2))
  call loop_QV_A(G0(:,:,:,1),wf(:,28),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,35),MB,G1(:,:,:,4))
  call loop_QV_A(G1(:,:,:,4),wf(:,10),G1(:,:,:,5))
  call check_last_Q_A(l_switch,G1(:,:,:,5),Q(:,63),MB,G2tensor(:,3))
  call loop_QV_A(G1(:,:,:,4),wf(:,20),G1(:,:,:,6))
  call check_last_Q_A(l_switch,G1(:,:,:,6),Q(:,63),MB,G2tensor(:,4))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,28),Q(:,35),G1(:,:,:,7))
  call check_last_CV_D(l_switch,G1(:,:,:,7),Q(:,35),wf(:,10),Q(:,28),G2tensor(:,5))
  call check_last_CV_D(l_switch,G1(:,:,:,7),Q(:,35),wf(:,20),Q(:,28),G2tensor(:,6))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(23)*M(1)-c(24)*M(2)) * den(122)
  T2sum(1:15,55) = T2sum(1:15,55) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(23)*M(1)-c(24)*M(2)) * den(122)
  T2sum(1:15,56) = T2sum(1:15,56) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(16)*M(1)-c(20)*M(2)) * den(122)
  T2sum(1:15,52) = T2sum(1:15,52) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(23)*M(1)-c(24)*M(2)) * den(128)
  T2sum(1:15,55) = T2sum(1:15,55) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(23)*M(1)-c(24)*M(2)) * den(128)
  T2sum(1:15,56) = T2sum(1:15,56) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(16)*M(1)-c(20)*M(2)) * den(128)
  T2sum(1:15,52) = T2sum(1:15,52) + Gcoeff * G2tensor(:,6)

end subroutine vamp_2

end module ol_vamp_2_pphbbj_ddxbbxhg_1_/**/REALKIND
