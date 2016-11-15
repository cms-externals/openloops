
module ol_vamp_1_pphh2_hhgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), save, target, allocatable :: G3tensorhel(:,:,:)
  complex(REALKIND), save, target, allocatable :: G4tensorhel(:,:,:)

  contains

! **********************************************************************
subroutine vamp_1(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_pphh2_hhgg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_pphh2_hhgg_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_pphh2_hhgg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_pphh2_hhgg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(inout) :: M(1)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,9) :: G0
  complex(REALKIND), dimension(4,5,4,24) :: G1
  complex(REALKIND), dimension(4,15,4,32) :: G2
  complex(REALKIND), dimension(4,35,4,24) :: G3
  complex(REALKIND), pointer :: G3tensor(:,:)
  complex(REALKIND), pointer :: G4tensor(:,:)
#ifdef PRECISION_dp
  logical, save :: first = .true.
  if (first) then
#endif
    allocate(G3tensorhel(35,4,4))
    allocate(G4tensorhel(70,12,4))
#ifdef PRECISION_dp
    first = .false.
  end if
#endif
  if (mode == -1) then
    call gtdealloc()
    return
  end if
  G3tensor => G3tensorhel(:,:,hel)
  G4tensor => G4tensorhel(:,:,hel)

  if (mode == 1 .or. stability_mode < 20 .or. stability_mode >= 30) then

  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_AV_Q(G0(:,:,:,1),wf(:,-2),G0(:,:,:,2))
  call loop_A_Q(G0(:,:,:,2),Q(:,4),MT,G1(:,:,:,1))
  call loop_AV_Q(G1(:,:,:,1),wf(:,-3),G1(:,:,:,2))
  call loop_A_Q(G1(:,:,:,2),Q(:,12),MT,G2(:,:,:,1))
  call loop_AS_Q(G2(:,:,:,1),wf(:,2),G2(:,:,:,2),gH)
  call check_last_A_Q(l_switch,G2(:,:,:,2),Q(:,15),MT,G3tensor(:,1))
  call loop_AS_Q(G1(:,:,:,1),wf(:,-1),G1(:,:,:,3),gH)
  call loop_A_Q(G1(:,:,:,3),Q(:,6),MT,G2(:,:,:,3))
  call loop_AV_Q(G2(:,:,:,3),wf(:,-3),G2(:,:,:,4))
  call loop_A_Q(G2(:,:,:,4),Q(:,14),MT,G3(:,:,:,1))
  call loop_AS_Q(G3(:,:,:,1),wf(:,0),G3(:,:,:,2),gH)
  call check_last_A_Q(l_switch,G3(:,:,:,2),Q(:,15),MT,G4tensor(:,1))
  call loop_QV_A(G0(:,:,:,1),wf(:,-2),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,4),MT,G1(:,:,:,4))
  call loop_QV_A(G1(:,:,:,4),wf(:,-3),G1(:,:,:,5))
  call loop_Q_A(G1(:,:,:,5),Q(:,12),MT,G2(:,:,:,5))
  call loop_QS_A(G2(:,:,:,5),wf(:,2),G2(:,:,:,6),gH)
  call check_last_Q_A(l_switch,G2(:,:,:,6),Q(:,15),MT,G3tensor(:,2))
  call loop_QS_A(G1(:,:,:,4),wf(:,-1),G1(:,:,:,6),gH)
  call loop_Q_A(G1(:,:,:,6),Q(:,6),MT,G2(:,:,:,7))
  call loop_QV_A(G2(:,:,:,7),wf(:,-3),G2(:,:,:,8))
  call loop_Q_A(G2(:,:,:,8),Q(:,14),MT,G3(:,:,:,3))
  call loop_QS_A(G3(:,:,:,3),wf(:,0),G3(:,:,:,4),gH)
  call check_last_Q_A(l_switch,G3(:,:,:,4),Q(:,15),MT,G4tensor(:,2))
  call loop_AV_Q(G0(:,:,:,1),wf(:,-2),G0(:,:,:,4))
  call loop_A_Q(G0(:,:,:,4),Q(:,4),MB,G1(:,:,:,7))
  call loop_AV_Q(G1(:,:,:,7),wf(:,-3),G1(:,:,:,8))
  call loop_A_Q(G1(:,:,:,8),Q(:,12),MB,G2(:,:,:,9))
  call loop_AS_Q(G2(:,:,:,9),wf(:,2),G2(:,:,:,10),gH)
  call check_last_A_Q(l_switch,G2(:,:,:,10),Q(:,15),MB,G3tensor(:,3))
  call loop_AS_Q(G1(:,:,:,7),wf(:,-1),G1(:,:,:,9),gH)
  call loop_A_Q(G1(:,:,:,9),Q(:,6),MB,G2(:,:,:,11))
  call loop_AV_Q(G2(:,:,:,11),wf(:,-3),G2(:,:,:,12))
  call loop_A_Q(G2(:,:,:,12),Q(:,14),MB,G3(:,:,:,5))
  call loop_AS_Q(G3(:,:,:,5),wf(:,0),G3(:,:,:,6),gH)
  call check_last_A_Q(l_switch,G3(:,:,:,6),Q(:,15),MB,G4tensor(:,3))
  call loop_QV_A(G0(:,:,:,1),wf(:,-2),G0(:,:,:,5))
  call loop_Q_A(G0(:,:,:,5),Q(:,4),MB,G1(:,:,:,10))
  call loop_QV_A(G1(:,:,:,10),wf(:,-3),G1(:,:,:,11))
  call loop_Q_A(G1(:,:,:,11),Q(:,12),MB,G2(:,:,:,13))
  call loop_QS_A(G2(:,:,:,13),wf(:,2),G2(:,:,:,14),gH)
  call check_last_Q_A(l_switch,G2(:,:,:,14),Q(:,15),MB,G3tensor(:,4))
  call loop_QS_A(G1(:,:,:,10),wf(:,-1),G1(:,:,:,12),gH)
  call loop_Q_A(G1(:,:,:,12),Q(:,6),MB,G2(:,:,:,15))
  call loop_QV_A(G2(:,:,:,15),wf(:,-3),G2(:,:,:,16))
  call loop_Q_A(G2(:,:,:,16),Q(:,14),MB,G3(:,:,:,7))
  call loop_QS_A(G3(:,:,:,7),wf(:,0),G3(:,:,:,8),gH)
  call check_last_Q_A(l_switch,G3(:,:,:,8),Q(:,15),MB,G4tensor(:,4))
  call loop_AS_Q(G0(:,:,:,1),wf(:,-1),G0(:,:,:,6),gH)
  call loop_A_Q(G0(:,:,:,6),Q(:,2),MT,G1(:,:,:,13))
  call loop_AV_Q(G1(:,:,:,13),wf(:,-3),G1(:,:,:,14))
  call loop_A_Q(G1(:,:,:,14),Q(:,10),MT,G2(:,:,:,17))
  call loop_AV_Q(G2(:,:,:,17),wf(:,-2),G2(:,:,:,18))
  call loop_A_Q(G2(:,:,:,18),Q(:,14),MT,G3(:,:,:,9))
  call loop_AS_Q(G3(:,:,:,9),wf(:,0),G3(:,:,:,10),gH)
  call check_last_A_Q(l_switch,G3(:,:,:,10),Q(:,15),MT,G4tensor(:,5))
  call loop_AV_Q(G1(:,:,:,13),wf(:,-2),G1(:,:,:,15))
  call loop_A_Q(G1(:,:,:,15),Q(:,6),MT,G2(:,:,:,19))
  call loop_AV_Q(G2(:,:,:,19),wf(:,-3),G2(:,:,:,20))
  call loop_A_Q(G2(:,:,:,20),Q(:,14),MT,G3(:,:,:,11))
  call loop_AS_Q(G3(:,:,:,11),wf(:,0),G3(:,:,:,12),gH)
  call check_last_A_Q(l_switch,G3(:,:,:,12),Q(:,15),MT,G4tensor(:,6))
  call loop_QS_A(G0(:,:,:,1),wf(:,-1),G0(:,:,:,7),gH)
  call loop_Q_A(G0(:,:,:,7),Q(:,2),MT,G1(:,:,:,16))
  call loop_QV_A(G1(:,:,:,16),wf(:,-3),G1(:,:,:,17))
  call loop_Q_A(G1(:,:,:,17),Q(:,10),MT,G2(:,:,:,21))
  call loop_QV_A(G2(:,:,:,21),wf(:,-2),G2(:,:,:,22))
  call loop_Q_A(G2(:,:,:,22),Q(:,14),MT,G3(:,:,:,13))
  call loop_QS_A(G3(:,:,:,13),wf(:,0),G3(:,:,:,14),gH)
  call check_last_Q_A(l_switch,G3(:,:,:,14),Q(:,15),MT,G4tensor(:,7))
  call loop_QV_A(G1(:,:,:,16),wf(:,-2),G1(:,:,:,18))
  call loop_Q_A(G1(:,:,:,18),Q(:,6),MT,G2(:,:,:,23))
  call loop_QV_A(G2(:,:,:,23),wf(:,-3),G2(:,:,:,24))
  call loop_Q_A(G2(:,:,:,24),Q(:,14),MT,G3(:,:,:,15))
  call loop_QS_A(G3(:,:,:,15),wf(:,0),G3(:,:,:,16),gH)
  call check_last_Q_A(l_switch,G3(:,:,:,16),Q(:,15),MT,G4tensor(:,8))
  call loop_AS_Q(G0(:,:,:,1),wf(:,-1),G0(:,:,:,8),gH)
  call loop_A_Q(G0(:,:,:,8),Q(:,2),MB,G1(:,:,:,19))
  call loop_AV_Q(G1(:,:,:,19),wf(:,-3),G1(:,:,:,20))
  call loop_A_Q(G1(:,:,:,20),Q(:,10),MB,G2(:,:,:,25))
  call loop_AV_Q(G2(:,:,:,25),wf(:,-2),G2(:,:,:,26))
  call loop_A_Q(G2(:,:,:,26),Q(:,14),MB,G3(:,:,:,17))
  call loop_AS_Q(G3(:,:,:,17),wf(:,0),G3(:,:,:,18),gH)
  call check_last_A_Q(l_switch,G3(:,:,:,18),Q(:,15),MB,G4tensor(:,9))
  call loop_AV_Q(G1(:,:,:,19),wf(:,-2),G1(:,:,:,21))
  call loop_A_Q(G1(:,:,:,21),Q(:,6),MB,G2(:,:,:,27))
  call loop_AV_Q(G2(:,:,:,27),wf(:,-3),G2(:,:,:,28))
  call loop_A_Q(G2(:,:,:,28),Q(:,14),MB,G3(:,:,:,19))
  call loop_AS_Q(G3(:,:,:,19),wf(:,0),G3(:,:,:,20),gH)
  call check_last_A_Q(l_switch,G3(:,:,:,20),Q(:,15),MB,G4tensor(:,10))
  call loop_QS_A(G0(:,:,:,1),wf(:,-1),G0(:,:,:,9),gH)
  call loop_Q_A(G0(:,:,:,9),Q(:,2),MB,G1(:,:,:,22))
  call loop_QV_A(G1(:,:,:,22),wf(:,-3),G1(:,:,:,23))
  call loop_Q_A(G1(:,:,:,23),Q(:,10),MB,G2(:,:,:,29))
  call loop_QV_A(G2(:,:,:,29),wf(:,-2),G2(:,:,:,30))
  call loop_Q_A(G2(:,:,:,30),Q(:,14),MB,G3(:,:,:,21))
  call loop_QS_A(G3(:,:,:,21),wf(:,0),G3(:,:,:,22),gH)
  call check_last_Q_A(l_switch,G3(:,:,:,22),Q(:,15),MB,G4tensor(:,11))
  call loop_QV_A(G1(:,:,:,22),wf(:,-2),G1(:,:,:,24))
  call loop_Q_A(G1(:,:,:,24),Q(:,6),MB,G2(:,:,:,31))
  call loop_QV_A(G2(:,:,:,31),wf(:,-3),G2(:,:,:,32))
  call loop_Q_A(G2(:,:,:,32),Q(:,14),MB,G3(:,:,:,23))
  call loop_QS_A(G3(:,:,:,23),wf(:,0),G3(:,:,:,24),gH)
  call check_last_Q_A(l_switch,G3(:,:,:,24),Q(:,15),MB,G4tensor(:,12))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  M(1) = M(1) + (-f(7) * den(1)) * TI2_call(3,momenta_1,masses2_2,G3tensor(:,1),T3sum(1:35,1))
  M(1) = M(1) + (-f(7) * den(1)) * TI2_call(3,momenta_1,masses2_2,G3tensor(:,2),T3sum(1:35,1))
  M(1) = M(1) + (-f(2) * den(1)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,3),T3sum(1:35,2))
  M(1) = M(1) + (-f(2) * den(1)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,4),T3sum(1:35,2))
  M(1) = M(1) + (-f(8)) * TI2_call(4,momenta_3,masses2_4,G4tensor(:,5),T4sum(1:70,1))
  M(1) = M(1) + (-f(8)) * TI2_call(4,momenta_3,masses2_4,G4tensor(:,7),T4sum(1:70,1))
  M(1) = M(1) + (-f(3)) * TI2_call(4,momenta_3,masses2_3,G4tensor(:,9),T4sum(1:70,2))
  M(1) = M(1) + (-f(3)) * TI2_call(4,momenta_3,masses2_3,G4tensor(:,11),T4sum(1:70,2))
  M(1) = M(1) + (-f(8)) * TI2_call(4,momenta_2,masses2_4,G4tensor(:,6),T4sum(1:70,3))
  M(1) = M(1) + (-f(8)) * TI2_call(4,momenta_2,masses2_4,G4tensor(:,8),T4sum(1:70,3))
  M(1) = M(1) + (-f(3)) * TI2_call(4,momenta_2,masses2_3,G4tensor(:,10),T4sum(1:70,4))
  M(1) = M(1) + (-f(3)) * TI2_call(4,momenta_2,masses2_3,G4tensor(:,12),T4sum(1:70,4))
  M(1) = M(1) + (-f(8)) * TI2_call(4,momenta_4,masses2_4,G4tensor(:,1),T4sum(1:70,5))
  M(1) = M(1) + (-f(8)) * TI2_call(4,momenta_4,masses2_4,G4tensor(:,2),T4sum(1:70,5))
  M(1) = M(1) + (-f(3)) * TI2_call(4,momenta_4,masses2_3,G4tensor(:,3),T4sum(1:70,6))
  M(1) = M(1) + (-f(3)) * TI2_call(4,momenta_4,masses2_3,G4tensor(:,4),T4sum(1:70,6))

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
  deallocate(G4tensorhel)

end subroutine gtdealloc
#endif

end module ol_vamp_1_pphh2_hhgg_1_/**/REALKIND
