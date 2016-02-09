
module ol_vamp_1_ppll2_eexgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), save, target, allocatable :: G3tensorhel(:,:,:)

  contains

! **********************************************************************
subroutine vamp_1(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppll2_eexgg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppll2_eexgg_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppll2_eexgg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppll2_eexgg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(inout) :: M(1)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,7) :: G0
  complex(REALKIND), dimension(4,5,4,12) :: G1
  complex(REALKIND), dimension(4,15,4,20) :: G2
  complex(REALKIND), pointer :: G3tensor(:,:)
#ifdef PRECISION_dp
  logical, save :: first = .true.
  if (first) then
#endif
    allocate(G3tensorhel(35,14,16))
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
  call loop_AV_Q(G0(:,:,:,1),wf(:,-2),G0(:,:,:,2))
  call loop_A_Q(G0(:,:,:,2),Q(:,4),ZERO,G1(:,:,:,1))
  call loop_AV_Q(G1(:,:,:,1),wf(:,-3),G1(:,:,:,2))
  call loop_A_Q(G1(:,:,:,2),Q(:,12),ZERO,G2(:,:,:,1))
  call loop_AV_Q(G2(:,:,:,1),wf(:,4),G2(:,:,:,2))
  call check_last_A_Q(l_switch,G2(:,:,:,2),Q(:,15),ZERO,G3tensor(:,1))
  call loop_AZ_Q(G2(:,:,:,1),wf(:,3),G2(:,:,:,3),gZu)
  call check_last_A_Q(l_switch,G2(:,:,:,3),Q(:,15),ZERO,G3tensor(:,2))
  call loop_AZ_Q(G2(:,:,:,1),wf(:,3),G2(:,:,:,4),gZd)
  call check_last_A_Q(l_switch,G2(:,:,:,4),Q(:,15),ZERO,G3tensor(:,3))
  call loop_AV_Q(G0(:,:,:,1),wf(:,-2),G0(:,:,:,3))
  call loop_A_Q(G0(:,:,:,3),Q(:,4),MT,G1(:,:,:,3))
  call loop_AV_Q(G1(:,:,:,3),wf(:,-3),G1(:,:,:,4))
  call loop_A_Q(G1(:,:,:,4),Q(:,12),MT,G2(:,:,:,5))
  call loop_AV_Q(G2(:,:,:,5),wf(:,4),G2(:,:,:,6))
  call check_last_A_Q(l_switch,G2(:,:,:,6),Q(:,15),MT,G3tensor(:,4))
  call loop_AZ_Q(G2(:,:,:,5),wf(:,3),G2(:,:,:,7),gZu)
  call check_last_A_Q(l_switch,G2(:,:,:,7),Q(:,15),MT,G3tensor(:,5))
  call loop_QV_A(G0(:,:,:,1),wf(:,-2),G0(:,:,:,4))
  call loop_Q_A(G0(:,:,:,4),Q(:,4),ZERO,G1(:,:,:,5))
  call loop_QV_A(G1(:,:,:,5),wf(:,-3),G1(:,:,:,6))
  call loop_Q_A(G1(:,:,:,6),Q(:,12),ZERO,G2(:,:,:,8))
  call loop_QV_A(G2(:,:,:,8),wf(:,4),G2(:,:,:,9))
  call check_last_Q_A(l_switch,G2(:,:,:,9),Q(:,15),ZERO,G3tensor(:,6))
  call loop_QZ_A(G2(:,:,:,8),wf(:,3),G2(:,:,:,10),gZu)
  call check_last_Q_A(l_switch,G2(:,:,:,10),Q(:,15),ZERO,G3tensor(:,7))
  call loop_QZ_A(G2(:,:,:,8),wf(:,3),G2(:,:,:,11),gZd)
  call check_last_Q_A(l_switch,G2(:,:,:,11),Q(:,15),ZERO,G3tensor(:,8))
  call loop_QV_A(G0(:,:,:,1),wf(:,-2),G0(:,:,:,5))
  call loop_Q_A(G0(:,:,:,5),Q(:,4),MT,G1(:,:,:,7))
  call loop_QV_A(G1(:,:,:,7),wf(:,-3),G1(:,:,:,8))
  call loop_Q_A(G1(:,:,:,8),Q(:,12),MT,G2(:,:,:,12))
  call loop_QV_A(G2(:,:,:,12),wf(:,4),G2(:,:,:,13))
  call check_last_Q_A(l_switch,G2(:,:,:,13),Q(:,15),MT,G3tensor(:,9))
  call loop_QZ_A(G2(:,:,:,12),wf(:,3),G2(:,:,:,14),gZu)
  call check_last_Q_A(l_switch,G2(:,:,:,14),Q(:,15),MT,G3tensor(:,10))
  call loop_AV_Q(G0(:,:,:,1),wf(:,-2),G0(:,:,:,6))
  call loop_A_Q(G0(:,:,:,6),Q(:,4),MB,G1(:,:,:,9))
  call loop_AV_Q(G1(:,:,:,9),wf(:,-3),G1(:,:,:,10))
  call loop_A_Q(G1(:,:,:,10),Q(:,12),MB,G2(:,:,:,15))
  call loop_AV_Q(G2(:,:,:,15),wf(:,4),G2(:,:,:,16))
  call check_last_A_Q(l_switch,G2(:,:,:,16),Q(:,15),MB,G3tensor(:,11))
  call loop_AZ_Q(G2(:,:,:,15),wf(:,3),G2(:,:,:,17),gZd)
  call check_last_A_Q(l_switch,G2(:,:,:,17),Q(:,15),MB,G3tensor(:,12))
  call loop_QV_A(G0(:,:,:,1),wf(:,-2),G0(:,:,:,7))
  call loop_Q_A(G0(:,:,:,7),Q(:,4),MB,G1(:,:,:,11))
  call loop_QV_A(G1(:,:,:,11),wf(:,-3),G1(:,:,:,12))
  call loop_Q_A(G1(:,:,:,12),Q(:,12),MB,G2(:,:,:,18))
  call loop_QV_A(G2(:,:,:,18),wf(:,4),G2(:,:,:,19))
  call check_last_Q_A(l_switch,G2(:,:,:,19),Q(:,15),MB,G3tensor(:,13))
  call loop_QZ_A(G2(:,:,:,18),wf(:,3),G2(:,:,:,20),gZd)
  call check_last_Q_A(l_switch,G2(:,:,:,20),Q(:,15),MB,G3tensor(:,14))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  M(1) = M(1) + (f(5) * den(2)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,1),T3sum(1:35,1))
  M(1) = M(1) + (f(3) * den(2)) * TI2_call(3,momenta_1,masses2_2,G3tensor(:,4),T3sum(1:35,2))
  M(1) = M(1) + (f(5) * den(2)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,6),T3sum(1:35,1))
  M(1) = M(1) + (f(3) * den(2)) * TI2_call(3,momenta_1,masses2_2,G3tensor(:,9),T3sum(1:35,2))
  M(1) = M(1) + (-f(3) * den(2)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,1),T3sum(1:35,1))
  M(1) = M(1) + (-f(2) * den(2)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,11),T3sum(1:35,3))
  M(1) = M(1) + (-f(3) * den(2)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,6),T3sum(1:35,1))
  M(1) = M(1) + (-f(2) * den(2)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,13),T3sum(1:35,3))
  M(1) = M(1) + (-f(6) * den(1)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,2),T3sum(1:35,1))
  M(1) = M(1) + (-f(4) * den(1)) * TI2_call(3,momenta_1,masses2_2,G3tensor(:,5),T3sum(1:35,2))
  M(1) = M(1) + (-f(6) * den(1)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,7),T3sum(1:35,1))
  M(1) = M(1) + (-f(4) * den(1)) * TI2_call(3,momenta_1,masses2_2,G3tensor(:,10),T3sum(1:35,2))
  M(1) = M(1) + (-f(6) * den(1)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,3),T3sum(1:35,1))
  M(1) = M(1) + (-f(4) * den(1)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,12),T3sum(1:35,3))
  M(1) = M(1) + (-f(6) * den(1)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,8),T3sum(1:35,1))
  M(1) = M(1) + (-f(4) * den(1)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,14),T3sum(1:35,3))

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

end module ol_vamp_1_ppll2_eexgg_1_/**/REALKIND
