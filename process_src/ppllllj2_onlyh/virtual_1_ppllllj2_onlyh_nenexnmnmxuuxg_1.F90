
module ol_vamp_1_ppllllj2_onlyh_nenexnmnmxuuxg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_1(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppllllj2_onlyh_nenexnmnmxuuxg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppllllj2_onlyh_nenexnmnmxuuxg_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppllllj2_onlyh_nenexnmnmxuuxg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppllllj2_onlyh_nenexnmnmxuuxg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(inout) :: M(1)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,5) :: G0
  complex(REALKIND), dimension(4,5,4,8) :: G1
  complex(REALKIND), dimension(4,15,4,8) :: G2
  complex(REALKIND), dimension(35,4) :: G3tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_QV_A(G0(:,:,:,1),wf(:,3),G0(:,:,:,2))
  call loop_Q_A(G0(:,:,:,2),Q(:,48),MT,G1(:,:,:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,-6),G1(:,:,:,2))
  call loop_Q_A(G1(:,:,:,2),Q(:,112),MT,G2(:,:,:,1))
  call loop_QS_A(G2(:,:,:,1),wf(:,6),G2(:,:,:,2),gH)
  call check_last_Q_A(l_switch,G2(:,:,:,2),Q(:,127),MT,G3tensor(:,1))
  call loop_AV_Q(G0(:,:,:,1),wf(:,3),G0(:,:,:,3))
  call loop_A_Q(G0(:,:,:,3),Q(:,48),MT,G1(:,:,:,3))
  call loop_AV_Q(G1(:,:,:,3),wf(:,-6),G1(:,:,:,4))
  call loop_A_Q(G1(:,:,:,4),Q(:,112),MT,G2(:,:,:,3))
  call loop_AS_Q(G2(:,:,:,3),wf(:,6),G2(:,:,:,4),gH)
  call check_last_A_Q(l_switch,G2(:,:,:,4),Q(:,127),MT,G3tensor(:,2))
  call loop_QV_A(G0(:,:,:,1),wf(:,3),G0(:,:,:,4))
  call loop_Q_A(G0(:,:,:,4),Q(:,48),MB,G1(:,:,:,5))
  call loop_QV_A(G1(:,:,:,5),wf(:,-6),G1(:,:,:,6))
  call loop_Q_A(G1(:,:,:,6),Q(:,112),MB,G2(:,:,:,5))
  call loop_QS_A(G2(:,:,:,5),wf(:,6),G2(:,:,:,6),gH)
  call check_last_Q_A(l_switch,G2(:,:,:,6),Q(:,127),MB,G3tensor(:,3))
  call loop_AV_Q(G0(:,:,:,1),wf(:,3),G0(:,:,:,5))
  call loop_A_Q(G0(:,:,:,5),Q(:,48),MB,G1(:,:,:,7))
  call loop_AV_Q(G1(:,:,:,7),wf(:,-6),G1(:,:,:,8))
  call loop_A_Q(G1(:,:,:,8),Q(:,112),MB,G2(:,:,:,7))
  call loop_AS_Q(G2(:,:,:,7),wf(:,6),G2(:,:,:,8),gH)
  call check_last_A_Q(l_switch,G2(:,:,:,8),Q(:,127),MB,G3tensor(:,4))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  M(1) = M(1) + (f(4)/2._/**/REALKIND * den(7)) * TI2_call(3,momenta_1,masses2_2,G3tensor(:,1),T3sum(1:35,1))
  M(1) = M(1) + (f(4)/2._/**/REALKIND * den(7)) * TI2_call(3,momenta_1,masses2_2,G3tensor(:,2),T3sum(1:35,1))
  M(1) = M(1) + (f(2)/2._/**/REALKIND * den(7)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,3),T3sum(1:35,2))
  M(1) = M(1) + (f(2)/2._/**/REALKIND * den(7)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,4),T3sum(1:35,2))

end subroutine vamp_1

end module ol_vamp_1_ppllllj2_onlyh_nenexnmnmxuuxg_1_/**/REALKIND
