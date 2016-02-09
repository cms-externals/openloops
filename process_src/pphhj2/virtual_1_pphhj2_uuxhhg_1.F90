
module ol_vamp_1_pphhj2_uuxhhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), save, target, allocatable :: G3tensorhel(:,:,:)
  complex(REALKIND), save, target, allocatable :: G4tensorhel(:,:,:)

  contains

! **********************************************************************
subroutine vamp_1(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_pphhj2_uuxhhg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_pphhj2_uuxhhg_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_pphhj2_uuxhhg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_pphhj2_uuxhhg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(inout) :: M(1)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,13) :: G0
  complex(REALKIND), dimension(4,5,4,28) :: G1
  complex(REALKIND), dimension(4,15,4,32) :: G2
  complex(REALKIND), dimension(4,35,4,24) :: G3
  complex(REALKIND), pointer :: G3tensor(:,:)
  complex(REALKIND), pointer :: G4tensor(:,:)
#ifdef PRECISION_dp
  logical, save :: first = .true.
  if (first) then
#endif
    allocate(G3tensorhel(35,4,8))
    allocate(G4tensorhel(70,12,8))
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
  call loop_AS_Q(G0(:,:,:,1),wf(:,3),G0(:,:,:,2),gH)
  call loop_A_Q(G0(:,:,:,2),Q(:,12),MT,G1(:,:,:,1))
  call loop_AV_Q(G1(:,:,:,1),wf(:,-4),G1(:,:,:,2))
  call loop_A_Q(G1(:,:,:,2),Q(:,28),MT,G2(:,:,:,1))
  call loop_AV_Q(G2(:,:,:,1),wf(:,1),G2(:,:,:,2))
  call check_last_A_Q(l_switch,G2(:,:,:,2),Q(:,31),MT,G3tensor(:,1))
  call loop_QS_A(G0(:,:,:,1),wf(:,3),G0(:,:,:,3),gH)
  call loop_Q_A(G0(:,:,:,3),Q(:,12),MT,G1(:,:,:,3))
  call loop_QV_A(G1(:,:,:,3),wf(:,-4),G1(:,:,:,4))
  call loop_Q_A(G1(:,:,:,4),Q(:,28),MT,G2(:,:,:,3))
  call loop_QV_A(G2(:,:,:,3),wf(:,1),G2(:,:,:,4))
  call check_last_Q_A(l_switch,G2(:,:,:,4),Q(:,31),MT,G3tensor(:,2))
  call loop_AS_Q(G0(:,:,:,1),wf(:,3),G0(:,:,:,4),gH)
  call loop_A_Q(G0(:,:,:,4),Q(:,12),MB,G1(:,:,:,5))
  call loop_AV_Q(G1(:,:,:,5),wf(:,-4),G1(:,:,:,6))
  call loop_A_Q(G1(:,:,:,6),Q(:,28),MB,G2(:,:,:,5))
  call loop_AV_Q(G2(:,:,:,5),wf(:,1),G2(:,:,:,6))
  call check_last_A_Q(l_switch,G2(:,:,:,6),Q(:,31),MB,G3tensor(:,3))
  call loop_QS_A(G0(:,:,:,1),wf(:,3),G0(:,:,:,5),gH)
  call loop_Q_A(G0(:,:,:,5),Q(:,12),MB,G1(:,:,:,7))
  call loop_QV_A(G1(:,:,:,7),wf(:,-4),G1(:,:,:,8))
  call loop_Q_A(G1(:,:,:,8),Q(:,28),MB,G2(:,:,:,7))
  call loop_QV_A(G2(:,:,:,7),wf(:,1),G2(:,:,:,8))
  call check_last_Q_A(l_switch,G2(:,:,:,8),Q(:,31),MB,G3tensor(:,4))
  call loop_QS_A(G0(:,:,:,1),wf(:,-3),G0(:,:,:,6),gH)
  call loop_Q_A(G0(:,:,:,6),Q(:,8),MT,G1(:,:,:,9))
  call loop_QS_A(G1(:,:,:,9),wf(:,-2),G1(:,:,:,10),gH)
  call loop_Q_A(G1(:,:,:,10),Q(:,12),MT,G2(:,:,:,9))
  call loop_QV_A(G2(:,:,:,9),wf(:,-4),G2(:,:,:,10))
  call loop_Q_A(G2(:,:,:,10),Q(:,28),MT,G3(:,:,:,1))
  call loop_QV_A(G3(:,:,:,1),wf(:,1),G3(:,:,:,2))
  call check_last_Q_A(l_switch,G3(:,:,:,2),Q(:,31),MT,G4tensor(:,1))
  call loop_AS_Q(G0(:,:,:,1),wf(:,-3),G0(:,:,:,7),gH)
  call loop_A_Q(G0(:,:,:,7),Q(:,8),MT,G1(:,:,:,11))
  call loop_AS_Q(G1(:,:,:,11),wf(:,-2),G1(:,:,:,12),gH)
  call loop_A_Q(G1(:,:,:,12),Q(:,12),MT,G2(:,:,:,11))
  call loop_AV_Q(G2(:,:,:,11),wf(:,-4),G2(:,:,:,12))
  call loop_A_Q(G2(:,:,:,12),Q(:,28),MT,G3(:,:,:,3))
  call loop_AV_Q(G3(:,:,:,3),wf(:,1),G3(:,:,:,4))
  call check_last_A_Q(l_switch,G3(:,:,:,4),Q(:,31),MT,G4tensor(:,2))
  call loop_QS_A(G0(:,:,:,1),wf(:,-3),G0(:,:,:,8),gH)
  call loop_Q_A(G0(:,:,:,8),Q(:,8),MB,G1(:,:,:,13))
  call loop_QS_A(G1(:,:,:,13),wf(:,-2),G1(:,:,:,14),gH)
  call loop_Q_A(G1(:,:,:,14),Q(:,12),MB,G2(:,:,:,13))
  call loop_QV_A(G2(:,:,:,13),wf(:,-4),G2(:,:,:,14))
  call loop_Q_A(G2(:,:,:,14),Q(:,28),MB,G3(:,:,:,5))
  call loop_QV_A(G3(:,:,:,5),wf(:,1),G3(:,:,:,6))
  call check_last_Q_A(l_switch,G3(:,:,:,6),Q(:,31),MB,G4tensor(:,3))
  call loop_AS_Q(G0(:,:,:,1),wf(:,-3),G0(:,:,:,9),gH)
  call loop_A_Q(G0(:,:,:,9),Q(:,8),MB,G1(:,:,:,15))
  call loop_AS_Q(G1(:,:,:,15),wf(:,-2),G1(:,:,:,16),gH)
  call loop_A_Q(G1(:,:,:,16),Q(:,12),MB,G2(:,:,:,15))
  call loop_AV_Q(G2(:,:,:,15),wf(:,-4),G2(:,:,:,16))
  call loop_A_Q(G2(:,:,:,16),Q(:,28),MB,G3(:,:,:,7))
  call loop_AV_Q(G3(:,:,:,7),wf(:,1),G3(:,:,:,8))
  call check_last_A_Q(l_switch,G3(:,:,:,8),Q(:,31),MB,G4tensor(:,4))
  call loop_AS_Q(G0(:,:,:,1),wf(:,-2),G0(:,:,:,10),gH)
  call loop_A_Q(G0(:,:,:,10),Q(:,4),MT,G1(:,:,:,17))
  call loop_AS_Q(G1(:,:,:,17),wf(:,-3),G1(:,:,:,18),gH)
  call loop_A_Q(G1(:,:,:,18),Q(:,12),MT,G2(:,:,:,17))
  call loop_AV_Q(G2(:,:,:,17),wf(:,-4),G2(:,:,:,18))
  call loop_A_Q(G2(:,:,:,18),Q(:,28),MT,G3(:,:,:,9))
  call loop_AV_Q(G3(:,:,:,9),wf(:,1),G3(:,:,:,10))
  call check_last_A_Q(l_switch,G3(:,:,:,10),Q(:,31),MT,G4tensor(:,5))
  call loop_AV_Q(G1(:,:,:,17),wf(:,-4),G1(:,:,:,19))
  call loop_A_Q(G1(:,:,:,19),Q(:,20),MT,G2(:,:,:,19))
  call loop_AS_Q(G2(:,:,:,19),wf(:,-3),G2(:,:,:,20),gH)
  call loop_A_Q(G2(:,:,:,20),Q(:,28),MT,G3(:,:,:,11))
  call loop_AV_Q(G3(:,:,:,11),wf(:,1),G3(:,:,:,12))
  call check_last_A_Q(l_switch,G3(:,:,:,12),Q(:,31),MT,G4tensor(:,6))
  call loop_QS_A(G0(:,:,:,1),wf(:,-2),G0(:,:,:,11),gH)
  call loop_Q_A(G0(:,:,:,11),Q(:,4),MT,G1(:,:,:,20))
  call loop_QS_A(G1(:,:,:,20),wf(:,-3),G1(:,:,:,21),gH)
  call loop_Q_A(G1(:,:,:,21),Q(:,12),MT,G2(:,:,:,21))
  call loop_QV_A(G2(:,:,:,21),wf(:,-4),G2(:,:,:,22))
  call loop_Q_A(G2(:,:,:,22),Q(:,28),MT,G3(:,:,:,13))
  call loop_QV_A(G3(:,:,:,13),wf(:,1),G3(:,:,:,14))
  call check_last_Q_A(l_switch,G3(:,:,:,14),Q(:,31),MT,G4tensor(:,7))
  call loop_QV_A(G1(:,:,:,20),wf(:,-4),G1(:,:,:,22))
  call loop_Q_A(G1(:,:,:,22),Q(:,20),MT,G2(:,:,:,23))
  call loop_QS_A(G2(:,:,:,23),wf(:,-3),G2(:,:,:,24),gH)
  call loop_Q_A(G2(:,:,:,24),Q(:,28),MT,G3(:,:,:,15))
  call loop_QV_A(G3(:,:,:,15),wf(:,1),G3(:,:,:,16))
  call check_last_Q_A(l_switch,G3(:,:,:,16),Q(:,31),MT,G4tensor(:,8))
  call loop_AS_Q(G0(:,:,:,1),wf(:,-2),G0(:,:,:,12),gH)
  call loop_A_Q(G0(:,:,:,12),Q(:,4),MB,G1(:,:,:,23))
  call loop_AS_Q(G1(:,:,:,23),wf(:,-3),G1(:,:,:,24),gH)
  call loop_A_Q(G1(:,:,:,24),Q(:,12),MB,G2(:,:,:,25))
  call loop_AV_Q(G2(:,:,:,25),wf(:,-4),G2(:,:,:,26))
  call loop_A_Q(G2(:,:,:,26),Q(:,28),MB,G3(:,:,:,17))
  call loop_AV_Q(G3(:,:,:,17),wf(:,1),G3(:,:,:,18))
  call check_last_A_Q(l_switch,G3(:,:,:,18),Q(:,31),MB,G4tensor(:,9))
  call loop_AV_Q(G1(:,:,:,23),wf(:,-4),G1(:,:,:,25))
  call loop_A_Q(G1(:,:,:,25),Q(:,20),MB,G2(:,:,:,27))
  call loop_AS_Q(G2(:,:,:,27),wf(:,-3),G2(:,:,:,28),gH)
  call loop_A_Q(G2(:,:,:,28),Q(:,28),MB,G3(:,:,:,19))
  call loop_AV_Q(G3(:,:,:,19),wf(:,1),G3(:,:,:,20))
  call check_last_A_Q(l_switch,G3(:,:,:,20),Q(:,31),MB,G4tensor(:,10))
  call loop_QS_A(G0(:,:,:,1),wf(:,-2),G0(:,:,:,13),gH)
  call loop_Q_A(G0(:,:,:,13),Q(:,4),MB,G1(:,:,:,26))
  call loop_QS_A(G1(:,:,:,26),wf(:,-3),G1(:,:,:,27),gH)
  call loop_Q_A(G1(:,:,:,27),Q(:,12),MB,G2(:,:,:,29))
  call loop_QV_A(G2(:,:,:,29),wf(:,-4),G2(:,:,:,30))
  call loop_Q_A(G2(:,:,:,30),Q(:,28),MB,G3(:,:,:,21))
  call loop_QV_A(G3(:,:,:,21),wf(:,1),G3(:,:,:,22))
  call check_last_Q_A(l_switch,G3(:,:,:,22),Q(:,31),MB,G4tensor(:,11))
  call loop_QV_A(G1(:,:,:,26),wf(:,-4),G1(:,:,:,28))
  call loop_Q_A(G1(:,:,:,28),Q(:,20),MB,G2(:,:,:,31))
  call loop_QS_A(G2(:,:,:,31),wf(:,-3),G2(:,:,:,32),gH)
  call loop_Q_A(G2(:,:,:,32),Q(:,28),MB,G3(:,:,:,23))
  call loop_QV_A(G3(:,:,:,23),wf(:,1),G3(:,:,:,24))
  call check_last_Q_A(l_switch,G3(:,:,:,24),Q(:,31),MB,G4tensor(:,12))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  M(1) = M(1) + (-(f(7)/2._/**/REALKIND) * den(3)) * TI2_call(3,momenta_1,masses2_2,G3tensor(:,1),T3sum(1:35,1))
  M(1) = M(1) + (-(f(7)/2._/**/REALKIND) * den(3)) * TI2_call(3,momenta_1,masses2_2,G3tensor(:,2),T3sum(1:35,1))
  M(1) = M(1) + (-(f(2)/2._/**/REALKIND) * den(3)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,3),T3sum(1:35,2))
  M(1) = M(1) + (-(f(2)/2._/**/REALKIND) * den(3)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,4),T3sum(1:35,2))
  M(1) = M(1) + (-(f(8)/2._/**/REALKIND) * den(1)) * TI2_call(4,momenta_4,masses2_4,G4tensor(:,1),T4sum(1:70,1))
  M(1) = M(1) + (-(f(8)/2._/**/REALKIND) * den(1)) * TI2_call(4,momenta_4,masses2_4,G4tensor(:,2),T4sum(1:70,1))
  M(1) = M(1) + (-(f(3)/2._/**/REALKIND) * den(1)) * TI2_call(4,momenta_4,masses2_3,G4tensor(:,3),T4sum(1:70,2))
  M(1) = M(1) + (-(f(3)/2._/**/REALKIND) * den(1)) * TI2_call(4,momenta_4,masses2_3,G4tensor(:,4),T4sum(1:70,2))
  M(1) = M(1) + (-(f(8)/2._/**/REALKIND) * den(1)) * TI2_call(4,momenta_2,masses2_4,G4tensor(:,5),T4sum(1:70,3))
  M(1) = M(1) + (-(f(8)/2._/**/REALKIND) * den(1)) * TI2_call(4,momenta_2,masses2_4,G4tensor(:,7),T4sum(1:70,3))
  M(1) = M(1) + (-(f(3)/2._/**/REALKIND) * den(1)) * TI2_call(4,momenta_2,masses2_3,G4tensor(:,9),T4sum(1:70,4))
  M(1) = M(1) + (-(f(3)/2._/**/REALKIND) * den(1)) * TI2_call(4,momenta_2,masses2_3,G4tensor(:,11),T4sum(1:70,4))
  M(1) = M(1) + (-(f(8)/2._/**/REALKIND) * den(1)) * TI2_call(4,momenta_3,masses2_4,G4tensor(:,6),T4sum(1:70,5))
  M(1) = M(1) + (-(f(8)/2._/**/REALKIND) * den(1)) * TI2_call(4,momenta_3,masses2_4,G4tensor(:,8),T4sum(1:70,5))
  M(1) = M(1) + (-(f(3)/2._/**/REALKIND) * den(1)) * TI2_call(4,momenta_3,masses2_3,G4tensor(:,10),T4sum(1:70,6))
  M(1) = M(1) + (-(f(3)/2._/**/REALKIND) * den(1)) * TI2_call(4,momenta_3,masses2_3,G4tensor(:,12),T4sum(1:70,6))

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

end module ol_vamp_1_pphhj2_uuxhhg_1_/**/REALKIND
