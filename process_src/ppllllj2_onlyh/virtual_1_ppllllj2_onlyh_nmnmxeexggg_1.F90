
module ol_vamp_1_ppllllj2_onlyh_nmnmxeexggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), save, target, allocatable :: G3tensorhel(:,:,:)
  complex(REALKIND), save, target, allocatable :: G4tensorhel(:,:,:)

  contains

! **********************************************************************
subroutine vamp_1(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppllllj2_onlyh_nmnmxeexggg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppllllj2_onlyh_nmnmxeexggg_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppllllj2_onlyh_nmnmxeexggg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppllllj2_onlyh_nmnmxeexggg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(inout) :: M(2)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,13) :: G0
  complex(REALKIND), dimension(4,5,4,36) :: G1
  complex(REALKIND), dimension(4,15,4,48) :: G2
  complex(REALKIND), dimension(4,35,4,24) :: G3
  complex(REALKIND), pointer :: G3tensor(:,:)
  complex(REALKIND), pointer :: G4tensor(:,:)
#ifdef PRECISION_dp
  logical, save :: first = .true.
  if (first) then
#endif
    allocate(G3tensorhel(35,12,128))
    allocate(G4tensorhel(70,12,128))
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
  call loop_AV_Q(G0(:,:,:,1),wf(:,-5),G0(:,:,:,6))
  call loop_A_Q(G0(:,:,:,6),Q(:,32),MT,G1(:,:,:,9))
  call loop_AV_Q(G1(:,:,:,9),wf(:,8),G1(:,:,:,10))
  call loop_A_Q(G1(:,:,:,10),Q(:,112),MT,G2(:,:,:,9))
  call loop_AS_Q(G2(:,:,:,9),wf(:,6),G2(:,:,:,10),gH)
  call check_last_A_Q(l_switch,G2(:,:,:,10),Q(:,127),MT,G3tensor(:,5))
  call loop_AV_Q(G1(:,:,:,9),wf(:,-4),G1(:,:,:,11))
  call loop_A_Q(G1(:,:,:,11),Q(:,48),MT,G2(:,:,:,11))
  call loop_AV_Q(G2(:,:,:,11),wf(:,-6),G2(:,:,:,12))
  call loop_A_Q(G2(:,:,:,12),Q(:,112),MT,G3(:,:,:,1))
  call loop_AS_Q(G3(:,:,:,1),wf(:,6),G3(:,:,:,2),gH)
  call check_last_A_Q(l_switch,G3(:,:,:,2),Q(:,127),MT,G4tensor(:,1))
  call loop_QV_A(G0(:,:,:,1),wf(:,-5),G0(:,:,:,7))
  call loop_Q_A(G0(:,:,:,7),Q(:,32),MT,G1(:,:,:,12))
  call loop_QV_A(G1(:,:,:,12),wf(:,8),G1(:,:,:,13))
  call loop_Q_A(G1(:,:,:,13),Q(:,112),MT,G2(:,:,:,13))
  call loop_QS_A(G2(:,:,:,13),wf(:,6),G2(:,:,:,14),gH)
  call check_last_Q_A(l_switch,G2(:,:,:,14),Q(:,127),MT,G3tensor(:,6))
  call loop_QV_A(G1(:,:,:,12),wf(:,-4),G1(:,:,:,14))
  call loop_Q_A(G1(:,:,:,14),Q(:,48),MT,G2(:,:,:,15))
  call loop_QV_A(G2(:,:,:,15),wf(:,-6),G2(:,:,:,16))
  call loop_Q_A(G2(:,:,:,16),Q(:,112),MT,G3(:,:,:,3))
  call loop_QS_A(G3(:,:,:,3),wf(:,6),G3(:,:,:,4),gH)
  call check_last_Q_A(l_switch,G3(:,:,:,4),Q(:,127),MT,G4tensor(:,2))
  call loop_AV_Q(G0(:,:,:,1),wf(:,-5),G0(:,:,:,8))
  call loop_A_Q(G0(:,:,:,8),Q(:,32),MB,G1(:,:,:,15))
  call loop_AV_Q(G1(:,:,:,15),wf(:,8),G1(:,:,:,16))
  call loop_A_Q(G1(:,:,:,16),Q(:,112),MB,G2(:,:,:,17))
  call loop_AS_Q(G2(:,:,:,17),wf(:,6),G2(:,:,:,18),gH)
  call check_last_A_Q(l_switch,G2(:,:,:,18),Q(:,127),MB,G3tensor(:,7))
  call loop_AV_Q(G1(:,:,:,15),wf(:,-4),G1(:,:,:,17))
  call loop_A_Q(G1(:,:,:,17),Q(:,48),MB,G2(:,:,:,19))
  call loop_AV_Q(G2(:,:,:,19),wf(:,-6),G2(:,:,:,20))
  call loop_A_Q(G2(:,:,:,20),Q(:,112),MB,G3(:,:,:,5))
  call loop_AS_Q(G3(:,:,:,5),wf(:,6),G3(:,:,:,6),gH)
  call check_last_A_Q(l_switch,G3(:,:,:,6),Q(:,127),MB,G4tensor(:,3))
  call loop_QV_A(G0(:,:,:,1),wf(:,-5),G0(:,:,:,9))
  call loop_Q_A(G0(:,:,:,9),Q(:,32),MB,G1(:,:,:,18))
  call loop_QV_A(G1(:,:,:,18),wf(:,8),G1(:,:,:,19))
  call loop_Q_A(G1(:,:,:,19),Q(:,112),MB,G2(:,:,:,21))
  call loop_QS_A(G2(:,:,:,21),wf(:,6),G2(:,:,:,22),gH)
  call check_last_Q_A(l_switch,G2(:,:,:,22),Q(:,127),MB,G3tensor(:,8))
  call loop_QV_A(G1(:,:,:,18),wf(:,-4),G1(:,:,:,20))
  call loop_Q_A(G1(:,:,:,20),Q(:,48),MB,G2(:,:,:,23))
  call loop_QV_A(G2(:,:,:,23),wf(:,-6),G2(:,:,:,24))
  call loop_Q_A(G2(:,:,:,24),Q(:,112),MB,G3(:,:,:,7))
  call loop_QS_A(G3(:,:,:,7),wf(:,6),G3(:,:,:,8),gH)
  call check_last_Q_A(l_switch,G3(:,:,:,8),Q(:,127),MB,G4tensor(:,4))
  call loop_AV_Q(G0(:,:,:,1),wf(:,-4),G0(:,:,:,10))
  call loop_A_Q(G0(:,:,:,10),Q(:,16),MT,G1(:,:,:,21))
  call loop_AV_Q(G1(:,:,:,21),wf(:,10),G1(:,:,:,22))
  call loop_A_Q(G1(:,:,:,22),Q(:,112),MT,G2(:,:,:,25))
  call loop_AS_Q(G2(:,:,:,25),wf(:,6),G2(:,:,:,26),gH)
  call check_last_A_Q(l_switch,G2(:,:,:,26),Q(:,127),MT,G3tensor(:,9))
  call loop_AV_Q(G1(:,:,:,21),wf(:,-5),G1(:,:,:,23))
  call loop_A_Q(G1(:,:,:,23),Q(:,48),MT,G2(:,:,:,27))
  call loop_AV_Q(G2(:,:,:,27),wf(:,-6),G2(:,:,:,28))
  call loop_A_Q(G2(:,:,:,28),Q(:,112),MT,G3(:,:,:,9))
  call loop_AS_Q(G3(:,:,:,9),wf(:,6),G3(:,:,:,10),gH)
  call check_last_A_Q(l_switch,G3(:,:,:,10),Q(:,127),MT,G4tensor(:,5))
  call loop_AV_Q(G1(:,:,:,21),wf(:,-6),G1(:,:,:,24))
  call loop_A_Q(G1(:,:,:,24),Q(:,80),MT,G2(:,:,:,29))
  call loop_AV_Q(G2(:,:,:,29),wf(:,-5),G2(:,:,:,30))
  call loop_A_Q(G2(:,:,:,30),Q(:,112),MT,G3(:,:,:,11))
  call loop_AS_Q(G3(:,:,:,11),wf(:,6),G3(:,:,:,12),gH)
  call check_last_A_Q(l_switch,G3(:,:,:,12),Q(:,127),MT,G4tensor(:,6))
  call loop_QV_A(G0(:,:,:,1),wf(:,-4),G0(:,:,:,11))
  call loop_Q_A(G0(:,:,:,11),Q(:,16),MT,G1(:,:,:,25))
  call loop_QV_A(G1(:,:,:,25),wf(:,10),G1(:,:,:,26))
  call loop_Q_A(G1(:,:,:,26),Q(:,112),MT,G2(:,:,:,31))
  call loop_QS_A(G2(:,:,:,31),wf(:,6),G2(:,:,:,32),gH)
  call check_last_Q_A(l_switch,G2(:,:,:,32),Q(:,127),MT,G3tensor(:,10))
  call loop_QV_A(G1(:,:,:,25),wf(:,-5),G1(:,:,:,27))
  call loop_Q_A(G1(:,:,:,27),Q(:,48),MT,G2(:,:,:,33))
  call loop_QV_A(G2(:,:,:,33),wf(:,-6),G2(:,:,:,34))
  call loop_Q_A(G2(:,:,:,34),Q(:,112),MT,G3(:,:,:,13))
  call loop_QS_A(G3(:,:,:,13),wf(:,6),G3(:,:,:,14),gH)
  call check_last_Q_A(l_switch,G3(:,:,:,14),Q(:,127),MT,G4tensor(:,7))
  call loop_QV_A(G1(:,:,:,25),wf(:,-6),G1(:,:,:,28))
  call loop_Q_A(G1(:,:,:,28),Q(:,80),MT,G2(:,:,:,35))
  call loop_QV_A(G2(:,:,:,35),wf(:,-5),G2(:,:,:,36))
  call loop_Q_A(G2(:,:,:,36),Q(:,112),MT,G3(:,:,:,15))
  call loop_QS_A(G3(:,:,:,15),wf(:,6),G3(:,:,:,16),gH)
  call check_last_Q_A(l_switch,G3(:,:,:,16),Q(:,127),MT,G4tensor(:,8))
  call loop_AV_Q(G0(:,:,:,1),wf(:,-4),G0(:,:,:,12))
  call loop_A_Q(G0(:,:,:,12),Q(:,16),MB,G1(:,:,:,29))
  call loop_AV_Q(G1(:,:,:,29),wf(:,10),G1(:,:,:,30))
  call loop_A_Q(G1(:,:,:,30),Q(:,112),MB,G2(:,:,:,37))
  call loop_AS_Q(G2(:,:,:,37),wf(:,6),G2(:,:,:,38),gH)
  call check_last_A_Q(l_switch,G2(:,:,:,38),Q(:,127),MB,G3tensor(:,11))
  call loop_AV_Q(G1(:,:,:,29),wf(:,-5),G1(:,:,:,31))
  call loop_A_Q(G1(:,:,:,31),Q(:,48),MB,G2(:,:,:,39))
  call loop_AV_Q(G2(:,:,:,39),wf(:,-6),G2(:,:,:,40))
  call loop_A_Q(G2(:,:,:,40),Q(:,112),MB,G3(:,:,:,17))
  call loop_AS_Q(G3(:,:,:,17),wf(:,6),G3(:,:,:,18),gH)
  call check_last_A_Q(l_switch,G3(:,:,:,18),Q(:,127),MB,G4tensor(:,9))
  call loop_AV_Q(G1(:,:,:,29),wf(:,-6),G1(:,:,:,32))
  call loop_A_Q(G1(:,:,:,32),Q(:,80),MB,G2(:,:,:,41))
  call loop_AV_Q(G2(:,:,:,41),wf(:,-5),G2(:,:,:,42))
  call loop_A_Q(G2(:,:,:,42),Q(:,112),MB,G3(:,:,:,19))
  call loop_AS_Q(G3(:,:,:,19),wf(:,6),G3(:,:,:,20),gH)
  call check_last_A_Q(l_switch,G3(:,:,:,20),Q(:,127),MB,G4tensor(:,10))
  call loop_QV_A(G0(:,:,:,1),wf(:,-4),G0(:,:,:,13))
  call loop_Q_A(G0(:,:,:,13),Q(:,16),MB,G1(:,:,:,33))
  call loop_QV_A(G1(:,:,:,33),wf(:,10),G1(:,:,:,34))
  call loop_Q_A(G1(:,:,:,34),Q(:,112),MB,G2(:,:,:,43))
  call loop_QS_A(G2(:,:,:,43),wf(:,6),G2(:,:,:,44),gH)
  call check_last_Q_A(l_switch,G2(:,:,:,44),Q(:,127),MB,G3tensor(:,12))
  call loop_QV_A(G1(:,:,:,33),wf(:,-5),G1(:,:,:,35))
  call loop_Q_A(G1(:,:,:,35),Q(:,48),MB,G2(:,:,:,45))
  call loop_QV_A(G2(:,:,:,45),wf(:,-6),G2(:,:,:,46))
  call loop_Q_A(G2(:,:,:,46),Q(:,112),MB,G3(:,:,:,21))
  call loop_QS_A(G3(:,:,:,21),wf(:,6),G3(:,:,:,22),gH)
  call check_last_Q_A(l_switch,G3(:,:,:,22),Q(:,127),MB,G4tensor(:,11))
  call loop_QV_A(G1(:,:,:,33),wf(:,-6),G1(:,:,:,36))
  call loop_Q_A(G1(:,:,:,36),Q(:,80),MB,G2(:,:,:,47))
  call loop_QV_A(G2(:,:,:,47),wf(:,-5),G2(:,:,:,48))
  call loop_Q_A(G2(:,:,:,48),Q(:,112),MB,G3(:,:,:,23))
  call loop_QS_A(G3(:,:,:,23),wf(:,6),G3(:,:,:,24),gH)
  call check_last_Q_A(l_switch,G3(:,:,:,24),Q(:,127),MB,G4tensor(:,12))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  M(1) = M(1) + (-(CI*f(5)) * den(7)) * TI2_call(3,momenta_3,masses2_2,G3tensor(:,1),T3sum(1:35,1))
  M(2) = M(2) + (CI*f(5) * den(7)) * TI2_call(3,momenta_3,masses2_2,G3tensor(:,1),T3sum(1:35,1))
  M(1) = M(1) + (-(CI*f(5)) * den(7)) * TI2_call(3,momenta_3,masses2_2,G3tensor(:,2),T3sum(1:35,1))
  M(2) = M(2) + (CI*f(5) * den(7)) * TI2_call(3,momenta_3,masses2_2,G3tensor(:,2),T3sum(1:35,1))
  M(1) = M(1) + (-(CI*f(2)) * den(7)) * TI2_call(3,momenta_3,masses2_1,G3tensor(:,3),T3sum(1:35,2))
  M(2) = M(2) + (CI*f(2) * den(7)) * TI2_call(3,momenta_3,masses2_1,G3tensor(:,3),T3sum(1:35,2))
  M(1) = M(1) + (-(CI*f(2)) * den(7)) * TI2_call(3,momenta_3,masses2_1,G3tensor(:,4),T3sum(1:35,2))
  M(2) = M(2) + (CI*f(2) * den(7)) * TI2_call(3,momenta_3,masses2_1,G3tensor(:,4),T3sum(1:35,2))
  M(1) = M(1) + (CI*f(5) * den(9)) * TI2_call(3,momenta_2,masses2_2,G3tensor(:,5),T3sum(1:35,3))
  M(2) = M(2) + (-(CI*f(5)) * den(9)) * TI2_call(3,momenta_2,masses2_2,G3tensor(:,5),T3sum(1:35,3))
  M(1) = M(1) + (CI*f(5) * den(9)) * TI2_call(3,momenta_2,masses2_2,G3tensor(:,6),T3sum(1:35,3))
  M(2) = M(2) + (-(CI*f(5)) * den(9)) * TI2_call(3,momenta_2,masses2_2,G3tensor(:,6),T3sum(1:35,3))
  M(1) = M(1) + (CI*f(2) * den(9)) * TI2_call(3,momenta_2,masses2_1,G3tensor(:,7),T3sum(1:35,4))
  M(2) = M(2) + (-(CI*f(2)) * den(9)) * TI2_call(3,momenta_2,masses2_1,G3tensor(:,7),T3sum(1:35,4))
  M(1) = M(1) + (CI*f(2) * den(9)) * TI2_call(3,momenta_2,masses2_1,G3tensor(:,8),T3sum(1:35,4))
  M(2) = M(2) + (-(CI*f(2)) * den(9)) * TI2_call(3,momenta_2,masses2_1,G3tensor(:,8),T3sum(1:35,4))
  M(1) = M(1) + (-(CI*f(5)) * den(11)) * TI2_call(3,momenta_1,masses2_2,G3tensor(:,9),T3sum(1:35,5))
  M(2) = M(2) + (CI*f(5) * den(11)) * TI2_call(3,momenta_1,masses2_2,G3tensor(:,9),T3sum(1:35,5))
  M(1) = M(1) + (-(CI*f(5)) * den(11)) * TI2_call(3,momenta_1,masses2_2,G3tensor(:,10),T3sum(1:35,5))
  M(2) = M(2) + (CI*f(5) * den(11)) * TI2_call(3,momenta_1,masses2_2,G3tensor(:,10),T3sum(1:35,5))
  M(1) = M(1) + (-(CI*f(2)) * den(11)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,11),T3sum(1:35,6))
  M(2) = M(2) + (CI*f(2) * den(11)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,11),T3sum(1:35,6))
  M(1) = M(1) + (-(CI*f(2)) * den(11)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,12),T3sum(1:35,6))
  M(2) = M(2) + (CI*f(2) * den(11)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,12),T3sum(1:35,6))
  M(1) = M(1) + (f(6) * den(6)) * TI2_call(4,momenta_6,masses2_4,G4tensor(:,2),T4sum(1:70,1))
  M(2) = M(2) + (f(6) * den(6)) * TI2_call(4,momenta_6,masses2_4,G4tensor(:,1),T4sum(1:70,1))
  M(1) = M(1) + (f(3) * den(6)) * TI2_call(4,momenta_6,masses2_3,G4tensor(:,4),T4sum(1:70,2))
  M(2) = M(2) + (f(3) * den(6)) * TI2_call(4,momenta_6,masses2_3,G4tensor(:,3),T4sum(1:70,2))
  M(1) = M(1) + (f(6) * den(6)) * TI2_call(4,momenta_4,masses2_4,G4tensor(:,5),T4sum(1:70,3))
  M(2) = M(2) + (f(6) * den(6)) * TI2_call(4,momenta_4,masses2_4,G4tensor(:,7),T4sum(1:70,3))
  M(1) = M(1) + (f(3) * den(6)) * TI2_call(4,momenta_4,masses2_3,G4tensor(:,9),T4sum(1:70,4))
  M(2) = M(2) + (f(3) * den(6)) * TI2_call(4,momenta_4,masses2_3,G4tensor(:,11),T4sum(1:70,4))
  M(2) = M(2) + (f(6) * den(6)) * TI2_call(4,momenta_5,masses2_4,G4tensor(:,6),T4sum(1:70,5))
  M(1) = M(1) + (f(6) * den(6)) * TI2_call(4,momenta_5,masses2_4,G4tensor(:,8),T4sum(1:70,5))
  M(2) = M(2) + (f(3) * den(6)) * TI2_call(4,momenta_5,masses2_3,G4tensor(:,10),T4sum(1:70,6))
  M(1) = M(1) + (f(3) * den(6)) * TI2_call(4,momenta_5,masses2_3,G4tensor(:,12),T4sum(1:70,6))

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

end module ol_vamp_1_ppllllj2_onlyh_nmnmxeexggg_1_/**/REALKIND
