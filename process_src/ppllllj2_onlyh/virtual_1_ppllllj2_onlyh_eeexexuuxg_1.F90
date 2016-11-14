
module ol_vamp_1_ppllllj2_onlyh_eeexexuuxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), save, target, allocatable :: G3tensorhel(:,:,:)

  contains

! **********************************************************************
subroutine vamp_1(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppllllj2_onlyh_eeexexuuxg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppllllj2_onlyh_eeexexuuxg_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppllllj2_onlyh_eeexexuuxg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppllllj2_onlyh_eeexexuuxg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(inout) :: M(1)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,5) :: G0
  complex(REALKIND), dimension(4,5,4,8) :: G1
  complex(REALKIND), dimension(4,15,4,12) :: G2
  complex(REALKIND), pointer :: G3tensor(:,:)
#ifdef PRECISION_dp
  logical, save :: first = .true.
  if (first) then
#endif
    allocate(G3tensorhel(35,8,128))
#ifdef PRECISION_dp
    first = .false.
  end if
#endif
  if (mode == -1) then
    call gtdealloc()
    return
  end if
  G3tensor => G3tensorhel(:,:,hel)

  if (mode == 1 .or. stability_mode < 20 .or. stability_mode >= 30) then

  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_QV_A(G0(:,:,:,1),wf(:,3),G0(:,:,:,2))
  call loop_Q_A(G0(:,:,:,2),Q(:,48),MT,G1(:,:,:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,-6),G1(:,:,:,2))
  call loop_Q_A(G1(:,:,:,2),Q(:,112),MT,G2(:,:,:,1))
  call loop_QS_A(G2(:,:,:,1),wf(:,6),G2(:,:,:,2),gH)
  call check_last_Q_A(l_switch,G2(:,:,:,2),Q(:,127),MT,G3tensor(:,1))
  call loop_QS_A(G2(:,:,:,1),wf(:,12),G2(:,:,:,3),gH)
  call check_last_Q_A(l_switch,G2(:,:,:,3),Q(:,127),MT,G3tensor(:,2))
  call loop_AV_Q(G0(:,:,:,1),wf(:,3),G0(:,:,:,3))
  call loop_A_Q(G0(:,:,:,3),Q(:,48),MT,G1(:,:,:,3))
  call loop_AV_Q(G1(:,:,:,3),wf(:,-6),G1(:,:,:,4))
  call loop_A_Q(G1(:,:,:,4),Q(:,112),MT,G2(:,:,:,4))
  call loop_AS_Q(G2(:,:,:,4),wf(:,6),G2(:,:,:,5),gH)
  call check_last_A_Q(l_switch,G2(:,:,:,5),Q(:,127),MT,G3tensor(:,3))
  call loop_AS_Q(G2(:,:,:,4),wf(:,12),G2(:,:,:,6),gH)
  call check_last_A_Q(l_switch,G2(:,:,:,6),Q(:,127),MT,G3tensor(:,4))
  call loop_QV_A(G0(:,:,:,1),wf(:,3),G0(:,:,:,4))
  call loop_Q_A(G0(:,:,:,4),Q(:,48),MB,G1(:,:,:,5))
  call loop_QV_A(G1(:,:,:,5),wf(:,-6),G1(:,:,:,6))
  call loop_Q_A(G1(:,:,:,6),Q(:,112),MB,G2(:,:,:,7))
  call loop_QS_A(G2(:,:,:,7),wf(:,6),G2(:,:,:,8),gH)
  call check_last_Q_A(l_switch,G2(:,:,:,8),Q(:,127),MB,G3tensor(:,5))
  call loop_QS_A(G2(:,:,:,7),wf(:,12),G2(:,:,:,9),gH)
  call check_last_Q_A(l_switch,G2(:,:,:,9),Q(:,127),MB,G3tensor(:,6))
  call loop_AV_Q(G0(:,:,:,1),wf(:,3),G0(:,:,:,5))
  call loop_A_Q(G0(:,:,:,5),Q(:,48),MB,G1(:,:,:,7))
  call loop_AV_Q(G1(:,:,:,7),wf(:,-6),G1(:,:,:,8))
  call loop_A_Q(G1(:,:,:,8),Q(:,112),MB,G2(:,:,:,10))
  call loop_AS_Q(G2(:,:,:,10),wf(:,6),G2(:,:,:,11),gH)
  call check_last_A_Q(l_switch,G2(:,:,:,11),Q(:,127),MB,G3tensor(:,7))
  call loop_AS_Q(G2(:,:,:,10),wf(:,12),G2(:,:,:,12),gH)
  call check_last_A_Q(l_switch,G2(:,:,:,12),Q(:,127),MB,G3tensor(:,8))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  M(1) = M(1) + (-(f(4)/2._/**/REALKIND) * den(7)) * TI2_call(3,momenta_1,masses2_2,G3tensor(:,1),T3sum(1:35,1))
  M(1) = M(1) + (-(f(4)/2._/**/REALKIND) * den(7)) * TI2_call(3,momenta_1,masses2_2,G3tensor(:,3),T3sum(1:35,1))
  M(1) = M(1) + (-(f(2)/2._/**/REALKIND) * den(7)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,5),T3sum(1:35,2))
  M(1) = M(1) + (-(f(2)/2._/**/REALKIND) * den(7)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,7),T3sum(1:35,2))
  M(1) = M(1) + (f(4)/2._/**/REALKIND * den(12)) * TI2_call(3,momenta_1,masses2_2,G3tensor(:,2),T3sum(1:35,1))
  M(1) = M(1) + (f(4)/2._/**/REALKIND * den(12)) * TI2_call(3,momenta_1,masses2_2,G3tensor(:,4),T3sum(1:35,1))
  M(1) = M(1) + (f(2)/2._/**/REALKIND * den(12)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,6),T3sum(1:35,2))
  M(1) = M(1) + (f(2)/2._/**/REALKIND * den(12)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,8),T3sum(1:35,2))

#ifdef LOOPSQUARED
#ifndef PRECISION_dp
  call gtdealloc()
#endif
#endif

end subroutine vamp_1

#ifdef LOOPSQUARED
subroutine gtdealloc()
  implicit none
  deallocate(G3tensorhel)

end subroutine gtdealloc
#endif

end module ol_vamp_1_ppllllj2_onlyh_eeexexuuxg_1_/**/REALKIND
