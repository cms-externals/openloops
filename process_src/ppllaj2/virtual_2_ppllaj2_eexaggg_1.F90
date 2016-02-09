
module ol_vamp_2_ppllaj2_eexaggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), save, target, allocatable :: G3tensorhel(:,:,:)
  complex(REALKIND), save, target, allocatable :: G4tensorhel(:,:,:)
  complex(REALKIND), save, target, allocatable :: G5tensorhel(:,:,:)

  contains

! **********************************************************************
subroutine vamp_2(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppllaj2_eexaggg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppllaj2_eexaggg_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppllaj2_eexaggg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppllaj2_eexaggg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(inout) :: M(2)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,3) :: G0
  complex(REALKIND), dimension(4,5,4,10) :: G1
  complex(REALKIND), dimension(4,15,4,30) :: G2
  complex(REALKIND), dimension(4,35,4,42) :: G3
  complex(REALKIND), dimension(4,70,4,24) :: G4
  complex(REALKIND), pointer :: G3tensor(:,:)
  complex(REALKIND), pointer :: G4tensor(:,:)
  complex(REALKIND), pointer :: G5tensor(:,:)
#ifdef PRECISION_dp
  logical, save :: first = .true.
  if (first) then
#endif
    allocate(G3tensorhel(35,8,64))
    allocate(G4tensorhel(70,20,64))
    allocate(G5tensorhel(126,16,64))
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
  G5tensor => G5tensorhel(:,:,hel)

  if (mode == 1 .or. stability_mode < 20 .or. stability_mode >= 30) then

  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_QV_A(G0(:,:,:,1),wf(:,-3),G0(:,:,:,2))
  call loop_Q_A(G0(:,:,:,2),Q(:,8),MB,G1(:,:,:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,-2),G1(:,:,:,2))
  call loop_Q_A(G1(:,:,:,2),Q(:,12),MB,G2(:,:,:,1))
  call loop_QV_A(G2(:,:,:,1),wf(:,10),G2(:,:,:,2))
  call loop_Q_A(G2(:,:,:,2),Q(:,60),MB,G3(:,:,:,1))
  call loop_QV_A(G3(:,:,:,1),wf(:,1),G3(:,:,:,2))
  call check_last_Q_A(l_switch,G3(:,:,:,2),Q(:,63),MB,G4tensor(:,1))
  call loop_QZ_A(G3(:,:,:,1),wf(:,5),G3(:,:,:,3),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,3),Q(:,63),MB,G4tensor(:,2))
  call loop_QV_A(G2(:,:,:,1),wf(:,-4),G2(:,:,:,3))
  call loop_Q_A(G2(:,:,:,3),Q(:,28),MB,G3(:,:,:,4))
  call loop_QV_A(G3(:,:,:,4),wf(:,-5),G3(:,:,:,5))
  call loop_Q_A(G3(:,:,:,5),Q(:,60),MB,G4(:,:,:,1))
  call loop_QV_A(G4(:,:,:,1),wf(:,1),G4(:,:,:,2))
  call check_last_Q_A(l_switch,G4(:,:,:,2),Q(:,63),MB,G5tensor(:,1))
  call loop_QZ_A(G4(:,:,:,1),wf(:,5),G4(:,:,:,3),gZd)
  call check_last_Q_A(l_switch,G4(:,:,:,3),Q(:,63),MB,G5tensor(:,2))
  call loop_QV_A(G2(:,:,:,1),wf(:,-5),G2(:,:,:,4))
  call loop_Q_A(G2(:,:,:,4),Q(:,44),MB,G3(:,:,:,6))
  call loop_QV_A(G3(:,:,:,6),wf(:,-4),G3(:,:,:,7))
  call loop_Q_A(G3(:,:,:,7),Q(:,60),MB,G4(:,:,:,4))
  call loop_QV_A(G4(:,:,:,4),wf(:,1),G4(:,:,:,5))
  call check_last_Q_A(l_switch,G4(:,:,:,5),Q(:,63),MB,G5tensor(:,3))
  call loop_QZ_A(G4(:,:,:,4),wf(:,5),G4(:,:,:,6),gZd)
  call check_last_Q_A(l_switch,G4(:,:,:,6),Q(:,63),MB,G5tensor(:,4))
  call loop_QV_A(G1(:,:,:,1),wf(:,-4),G1(:,:,:,3))
  call loop_Q_A(G1(:,:,:,3),Q(:,24),MB,G2(:,:,:,5))
  call loop_QV_A(G2(:,:,:,5),wf(:,-2),G2(:,:,:,6))
  call loop_Q_A(G2(:,:,:,6),Q(:,28),MB,G3(:,:,:,8))
  call loop_QV_A(G3(:,:,:,8),wf(:,-5),G3(:,:,:,9))
  call loop_Q_A(G3(:,:,:,9),Q(:,60),MB,G4(:,:,:,7))
  call loop_QV_A(G4(:,:,:,7),wf(:,1),G4(:,:,:,8))
  call check_last_Q_A(l_switch,G4(:,:,:,8),Q(:,63),MB,G5tensor(:,5))
  call loop_QZ_A(G4(:,:,:,7),wf(:,5),G4(:,:,:,9),gZd)
  call check_last_Q_A(l_switch,G4(:,:,:,9),Q(:,63),MB,G5tensor(:,6))
  call loop_QV_A(G2(:,:,:,5),wf(:,-5),G2(:,:,:,7))
  call loop_Q_A(G2(:,:,:,7),Q(:,56),MB,G3(:,:,:,10))
  call loop_QV_A(G3(:,:,:,10),wf(:,16),G3(:,:,:,11))
  call check_last_Q_A(l_switch,G3(:,:,:,11),Q(:,63),MB,G4tensor(:,3))
  call loop_QZ_A(G3(:,:,:,10),wf(:,28),G3(:,:,:,12),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,12),Q(:,63),MB,G4tensor(:,4))
  call loop_QV_A(G3(:,:,:,10),wf(:,25),G3(:,:,:,13))
  call check_last_Q_A(l_switch,G3(:,:,:,13),Q(:,63),MB,G4tensor(:,5))
  call loop_QZ_A(G3(:,:,:,10),wf(:,31),G3(:,:,:,14),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,14),Q(:,63),MB,G4tensor(:,6))
  call loop_QV_A(G1(:,:,:,1),wf(:,-5),G1(:,:,:,4))
  call loop_Q_A(G1(:,:,:,4),Q(:,40),MB,G2(:,:,:,8))
  call loop_QV_A(G2(:,:,:,8),wf(:,-2),G2(:,:,:,9))
  call loop_Q_A(G2(:,:,:,9),Q(:,44),MB,G3(:,:,:,15))
  call loop_QV_A(G3(:,:,:,15),wf(:,-4),G3(:,:,:,16))
  call loop_Q_A(G3(:,:,:,16),Q(:,60),MB,G4(:,:,:,10))
  call loop_QV_A(G4(:,:,:,10),wf(:,1),G4(:,:,:,11))
  call check_last_Q_A(l_switch,G4(:,:,:,11),Q(:,63),MB,G5tensor(:,7))
  call loop_QZ_A(G4(:,:,:,10),wf(:,5),G4(:,:,:,12),gZd)
  call check_last_Q_A(l_switch,G4(:,:,:,12),Q(:,63),MB,G5tensor(:,8))
  call loop_QV_A(G2(:,:,:,8),wf(:,-4),G2(:,:,:,10))
  call loop_Q_A(G2(:,:,:,10),Q(:,56),MB,G3(:,:,:,17))
  call loop_QV_A(G3(:,:,:,17),wf(:,16),G3(:,:,:,18))
  call check_last_Q_A(l_switch,G3(:,:,:,18),Q(:,63),MB,G4tensor(:,7))
  call loop_QZ_A(G3(:,:,:,17),wf(:,28),G3(:,:,:,19),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,19),Q(:,63),MB,G4tensor(:,8))
  call loop_QV_A(G3(:,:,:,17),wf(:,25),G3(:,:,:,20))
  call check_last_Q_A(l_switch,G3(:,:,:,20),Q(:,63),MB,G4tensor(:,9))
  call loop_QZ_A(G3(:,:,:,17),wf(:,31),G3(:,:,:,21),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,21),Q(:,63),MB,G4tensor(:,10))
  call loop_QV_A(G1(:,:,:,1),wf(:,10),G1(:,:,:,5))
  call loop_Q_A(G1(:,:,:,5),Q(:,56),MB,G2(:,:,:,11))
  call loop_QV_A(G2(:,:,:,11),wf(:,16),G2(:,:,:,12))
  call check_last_Q_A(l_switch,G2(:,:,:,12),Q(:,63),MB,G3tensor(:,1))
  call loop_QZ_A(G2(:,:,:,11),wf(:,28),G2(:,:,:,13),gZd)
  call check_last_Q_A(l_switch,G2(:,:,:,13),Q(:,63),MB,G3tensor(:,2))
  call loop_QV_A(G2(:,:,:,11),wf(:,25),G2(:,:,:,14))
  call check_last_Q_A(l_switch,G2(:,:,:,14),Q(:,63),MB,G3tensor(:,3))
  call loop_QZ_A(G2(:,:,:,11),wf(:,31),G2(:,:,:,15),gZd)
  call check_last_Q_A(l_switch,G2(:,:,:,15),Q(:,63),MB,G3tensor(:,4))
  call loop_AV_Q(G0(:,:,:,1),wf(:,-3),G0(:,:,:,3))
  call loop_A_Q(G0(:,:,:,3),Q(:,8),MB,G1(:,:,:,6))
  call loop_AV_Q(G1(:,:,:,6),wf(:,-2),G1(:,:,:,7))
  call loop_A_Q(G1(:,:,:,7),Q(:,12),MB,G2(:,:,:,16))
  call loop_AV_Q(G2(:,:,:,16),wf(:,10),G2(:,:,:,17))
  call loop_A_Q(G2(:,:,:,17),Q(:,60),MB,G3(:,:,:,22))
  call loop_AV_Q(G3(:,:,:,22),wf(:,1),G3(:,:,:,23))
  call check_last_A_Q(l_switch,G3(:,:,:,23),Q(:,63),MB,G4tensor(:,11))
  call loop_AZ_Q(G3(:,:,:,22),wf(:,5),G3(:,:,:,24),gZd)
  call check_last_A_Q(l_switch,G3(:,:,:,24),Q(:,63),MB,G4tensor(:,12))
  call loop_AV_Q(G2(:,:,:,16),wf(:,-4),G2(:,:,:,18))
  call loop_A_Q(G2(:,:,:,18),Q(:,28),MB,G3(:,:,:,25))
  call loop_AV_Q(G3(:,:,:,25),wf(:,-5),G3(:,:,:,26))
  call loop_A_Q(G3(:,:,:,26),Q(:,60),MB,G4(:,:,:,13))
  call loop_AV_Q(G4(:,:,:,13),wf(:,1),G4(:,:,:,14))
  call check_last_A_Q(l_switch,G4(:,:,:,14),Q(:,63),MB,G5tensor(:,9))
  call loop_AZ_Q(G4(:,:,:,13),wf(:,5),G4(:,:,:,15),gZd)
  call check_last_A_Q(l_switch,G4(:,:,:,15),Q(:,63),MB,G5tensor(:,10))
  call loop_AV_Q(G2(:,:,:,16),wf(:,-5),G2(:,:,:,19))
  call loop_A_Q(G2(:,:,:,19),Q(:,44),MB,G3(:,:,:,27))
  call loop_AV_Q(G3(:,:,:,27),wf(:,-4),G3(:,:,:,28))
  call loop_A_Q(G3(:,:,:,28),Q(:,60),MB,G4(:,:,:,16))
  call loop_AV_Q(G4(:,:,:,16),wf(:,1),G4(:,:,:,17))
  call check_last_A_Q(l_switch,G4(:,:,:,17),Q(:,63),MB,G5tensor(:,11))
  call loop_AZ_Q(G4(:,:,:,16),wf(:,5),G4(:,:,:,18),gZd)
  call check_last_A_Q(l_switch,G4(:,:,:,18),Q(:,63),MB,G5tensor(:,12))
  call loop_AV_Q(G1(:,:,:,6),wf(:,-4),G1(:,:,:,8))
  call loop_A_Q(G1(:,:,:,8),Q(:,24),MB,G2(:,:,:,20))
  call loop_AV_Q(G2(:,:,:,20),wf(:,-2),G2(:,:,:,21))
  call loop_A_Q(G2(:,:,:,21),Q(:,28),MB,G3(:,:,:,29))
  call loop_AV_Q(G3(:,:,:,29),wf(:,-5),G3(:,:,:,30))
  call loop_A_Q(G3(:,:,:,30),Q(:,60),MB,G4(:,:,:,19))
  call loop_AV_Q(G4(:,:,:,19),wf(:,1),G4(:,:,:,20))
  call check_last_A_Q(l_switch,G4(:,:,:,20),Q(:,63),MB,G5tensor(:,13))
  call loop_AZ_Q(G4(:,:,:,19),wf(:,5),G4(:,:,:,21),gZd)
  call check_last_A_Q(l_switch,G4(:,:,:,21),Q(:,63),MB,G5tensor(:,14))
  call loop_AV_Q(G2(:,:,:,20),wf(:,-5),G2(:,:,:,22))
  call loop_A_Q(G2(:,:,:,22),Q(:,56),MB,G3(:,:,:,31))
  call loop_AV_Q(G3(:,:,:,31),wf(:,16),G3(:,:,:,32))
  call check_last_A_Q(l_switch,G3(:,:,:,32),Q(:,63),MB,G4tensor(:,13))
  call loop_AZ_Q(G3(:,:,:,31),wf(:,28),G3(:,:,:,33),gZd)
  call check_last_A_Q(l_switch,G3(:,:,:,33),Q(:,63),MB,G4tensor(:,14))
  call loop_AV_Q(G3(:,:,:,31),wf(:,25),G3(:,:,:,34))
  call check_last_A_Q(l_switch,G3(:,:,:,34),Q(:,63),MB,G4tensor(:,15))
  call loop_AZ_Q(G3(:,:,:,31),wf(:,31),G3(:,:,:,35),gZd)
  call check_last_A_Q(l_switch,G3(:,:,:,35),Q(:,63),MB,G4tensor(:,16))
  call loop_AV_Q(G1(:,:,:,6),wf(:,-5),G1(:,:,:,9))
  call loop_A_Q(G1(:,:,:,9),Q(:,40),MB,G2(:,:,:,23))
  call loop_AV_Q(G2(:,:,:,23),wf(:,-2),G2(:,:,:,24))
  call loop_A_Q(G2(:,:,:,24),Q(:,44),MB,G3(:,:,:,36))
  call loop_AV_Q(G3(:,:,:,36),wf(:,-4),G3(:,:,:,37))
  call loop_A_Q(G3(:,:,:,37),Q(:,60),MB,G4(:,:,:,22))
  call loop_AV_Q(G4(:,:,:,22),wf(:,1),G4(:,:,:,23))
  call check_last_A_Q(l_switch,G4(:,:,:,23),Q(:,63),MB,G5tensor(:,15))
  call loop_AZ_Q(G4(:,:,:,22),wf(:,5),G4(:,:,:,24),gZd)
  call check_last_A_Q(l_switch,G4(:,:,:,24),Q(:,63),MB,G5tensor(:,16))
  call loop_AV_Q(G2(:,:,:,23),wf(:,-4),G2(:,:,:,25))
  call loop_A_Q(G2(:,:,:,25),Q(:,56),MB,G3(:,:,:,38))
  call loop_AV_Q(G3(:,:,:,38),wf(:,16),G3(:,:,:,39))
  call check_last_A_Q(l_switch,G3(:,:,:,39),Q(:,63),MB,G4tensor(:,17))
  call loop_AZ_Q(G3(:,:,:,38),wf(:,28),G3(:,:,:,40),gZd)
  call check_last_A_Q(l_switch,G3(:,:,:,40),Q(:,63),MB,G4tensor(:,18))
  call loop_AV_Q(G3(:,:,:,38),wf(:,25),G3(:,:,:,41))
  call check_last_A_Q(l_switch,G3(:,:,:,41),Q(:,63),MB,G4tensor(:,19))
  call loop_AZ_Q(G3(:,:,:,38),wf(:,31),G3(:,:,:,42),gZd)
  call check_last_A_Q(l_switch,G3(:,:,:,42),Q(:,63),MB,G4tensor(:,20))
  call loop_AV_Q(G1(:,:,:,6),wf(:,10),G1(:,:,:,10))
  call loop_A_Q(G1(:,:,:,10),Q(:,56),MB,G2(:,:,:,26))
  call loop_AV_Q(G2(:,:,:,26),wf(:,16),G2(:,:,:,27))
  call check_last_A_Q(l_switch,G2(:,:,:,27),Q(:,63),MB,G3tensor(:,5))
  call loop_AZ_Q(G2(:,:,:,26),wf(:,28),G2(:,:,:,28),gZd)
  call check_last_A_Q(l_switch,G2(:,:,:,28),Q(:,63),MB,G3tensor(:,6))
  call loop_AV_Q(G2(:,:,:,26),wf(:,25),G2(:,:,:,29))
  call check_last_A_Q(l_switch,G2(:,:,:,29),Q(:,63),MB,G3tensor(:,7))
  call loop_AZ_Q(G2(:,:,:,26),wf(:,31),G2(:,:,:,30),gZd)
  call check_last_A_Q(l_switch,G2(:,:,:,30),Q(:,63),MB,G3tensor(:,8))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  M(1) = M(1) + (-(CI*f(5)) * den(10)) * TI2_call(4,momenta_10,masses2_4,G4tensor(:,1),T4sum(1:70,24))
  M(2) = M(2) + (CI*f(5) * den(10)) * TI2_call(4,momenta_10,masses2_4,G4tensor(:,1),T4sum(1:70,24))
  M(1) = M(1) + (-(CI*f(5)) * den(10)) * TI2_call(4,momenta_10,masses2_4,G4tensor(:,11),T4sum(1:70,24))
  M(2) = M(2) + (CI*f(5) * den(10)) * TI2_call(4,momenta_10,masses2_4,G4tensor(:,11),T4sum(1:70,24))
  M(1) = M(1) + (-(CI*f(7)) * den(11)) * TI2_call(4,momenta_10,masses2_4,G4tensor(:,2),T4sum(1:70,24))
  M(2) = M(2) + (CI*f(7) * den(11)) * TI2_call(4,momenta_10,masses2_4,G4tensor(:,2),T4sum(1:70,24))
  M(1) = M(1) + (-(CI*f(7)) * den(11)) * TI2_call(4,momenta_10,masses2_4,G4tensor(:,12),T4sum(1:70,24))
  M(2) = M(2) + (CI*f(7) * den(11)) * TI2_call(4,momenta_10,masses2_4,G4tensor(:,12),T4sum(1:70,24))
  M(2) = M(2) + (f(14) * den(1)) * TI2_call(5,momenta_22,masses2_7,G5tensor(:,1),T5sum(1:126,6))
  M(1) = M(1) + (f(14) * den(1)) * TI2_call(5,momenta_22,masses2_7,G5tensor(:,9),T5sum(1:126,6))
  M(2) = M(2) + (f(16) * den(4)) * TI2_call(5,momenta_22,masses2_7,G5tensor(:,2),T5sum(1:126,6))
  M(1) = M(1) + (f(16) * den(4)) * TI2_call(5,momenta_22,masses2_7,G5tensor(:,10),T5sum(1:126,6))
  M(1) = M(1) + (f(14) * den(1)) * TI2_call(5,momenta_23,masses2_7,G5tensor(:,3),T5sum(1:126,12))
  M(2) = M(2) + (f(14) * den(1)) * TI2_call(5,momenta_23,masses2_7,G5tensor(:,11),T5sum(1:126,12))
  M(1) = M(1) + (f(16) * den(4)) * TI2_call(5,momenta_23,masses2_7,G5tensor(:,4),T5sum(1:126,12))
  M(2) = M(2) + (f(16) * den(4)) * TI2_call(5,momenta_23,masses2_7,G5tensor(:,12),T5sum(1:126,12))
  M(2) = M(2) + (f(14) * den(1)) * TI2_call(5,momenta_24,masses2_7,G5tensor(:,5),T5sum(1:126,21))
  M(1) = M(1) + (f(14) * den(1)) * TI2_call(5,momenta_24,masses2_7,G5tensor(:,13),T5sum(1:126,21))
  M(2) = M(2) + (f(16) * den(4)) * TI2_call(5,momenta_24,masses2_7,G5tensor(:,6),T5sum(1:126,21))
  M(1) = M(1) + (f(16) * den(4)) * TI2_call(5,momenta_24,masses2_7,G5tensor(:,14),T5sum(1:126,21))
  M(2) = M(2) + (f(14) * den(1)) * TI2_call(5,momenta_25,masses2_7,G5tensor(:,15),T5sum(1:126,24))
  M(1) = M(1) + (f(14) * den(1)) * TI2_call(5,momenta_25,masses2_7,G5tensor(:,7),T5sum(1:126,24))
  M(2) = M(2) + (f(16) * den(4)) * TI2_call(5,momenta_25,masses2_7,G5tensor(:,16),T5sum(1:126,24))
  M(1) = M(1) + (f(16) * den(4)) * TI2_call(5,momenta_25,masses2_7,G5tensor(:,8),T5sum(1:126,24))
  M(1) = M(1) + (-(CI*f(7)) * den(34)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,1),T3sum(1:35,9))
  M(2) = M(2) + (CI*f(7) * den(34)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,1),T3sum(1:35,9))
  M(1) = M(1) + (-(CI*f(7)) * den(34)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,5),T3sum(1:35,9))
  M(2) = M(2) + (CI*f(7) * den(34)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,5),T3sum(1:35,9))
  M(1) = M(1) + (-(CI*f(11)) * den(24)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,2),T3sum(1:35,9))
  M(2) = M(2) + (CI*f(11) * den(24)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,2),T3sum(1:35,9))
  M(1) = M(1) + (-(CI*f(11)) * den(24)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,6),T3sum(1:35,9))
  M(2) = M(2) + (CI*f(11) * den(24)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,6),T3sum(1:35,9))
  M(1) = M(1) + (f(16) * den(30)) * TI2_call(4,momenta_11,masses2_4,G4tensor(:,13),T4sum(1:70,33))
  M(2) = M(2) + (f(16) * den(30)) * TI2_call(4,momenta_11,masses2_4,G4tensor(:,3),T4sum(1:70,33))
  M(1) = M(1) + (f(20) * den(21)) * TI2_call(4,momenta_11,masses2_4,G4tensor(:,14),T4sum(1:70,33))
  M(2) = M(2) + (f(20) * den(21)) * TI2_call(4,momenta_11,masses2_4,G4tensor(:,4),T4sum(1:70,33))
  M(2) = M(2) + (f(16) * den(30)) * TI2_call(4,momenta_12,masses2_4,G4tensor(:,17),T4sum(1:70,36))
  M(1) = M(1) + (f(16) * den(30)) * TI2_call(4,momenta_12,masses2_4,G4tensor(:,7),T4sum(1:70,36))
  M(2) = M(2) + (f(20) * den(21)) * TI2_call(4,momenta_12,masses2_4,G4tensor(:,18),T4sum(1:70,36))
  M(1) = M(1) + (f(20) * den(21)) * TI2_call(4,momenta_12,masses2_4,G4tensor(:,8),T4sum(1:70,36))
  M(1) = M(1) + (-(CI*f(7)) * den(37)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,3),T3sum(1:35,9))
  M(2) = M(2) + (CI*f(7) * den(37)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,3),T3sum(1:35,9))
  M(1) = M(1) + (-(CI*f(7)) * den(37)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,7),T3sum(1:35,9))
  M(2) = M(2) + (CI*f(7) * den(37)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,7),T3sum(1:35,9))
  M(1) = M(1) + (-(CI*f(11)) * den(28)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,4),T3sum(1:35,9))
  M(2) = M(2) + (CI*f(11) * den(28)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,4),T3sum(1:35,9))
  M(1) = M(1) + (-(CI*f(11)) * den(28)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,8),T3sum(1:35,9))
  M(2) = M(2) + (CI*f(11) * den(28)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,8),T3sum(1:35,9))
  M(1) = M(1) + (f(16) * den(31)) * TI2_call(4,momenta_11,masses2_4,G4tensor(:,15),T4sum(1:70,33))
  M(2) = M(2) + (f(16) * den(31)) * TI2_call(4,momenta_11,masses2_4,G4tensor(:,5),T4sum(1:70,33))
  M(1) = M(1) + (f(20) * den(25)) * TI2_call(4,momenta_11,masses2_4,G4tensor(:,16),T4sum(1:70,33))
  M(2) = M(2) + (f(20) * den(25)) * TI2_call(4,momenta_11,masses2_4,G4tensor(:,6),T4sum(1:70,33))
  M(2) = M(2) + (f(16) * den(31)) * TI2_call(4,momenta_12,masses2_4,G4tensor(:,19),T4sum(1:70,36))
  M(1) = M(1) + (f(16) * den(31)) * TI2_call(4,momenta_12,masses2_4,G4tensor(:,9),T4sum(1:70,36))
  M(2) = M(2) + (f(20) * den(25)) * TI2_call(4,momenta_12,masses2_4,G4tensor(:,20),T4sum(1:70,36))
  M(1) = M(1) + (f(20) * den(25)) * TI2_call(4,momenta_12,masses2_4,G4tensor(:,10),T4sum(1:70,36))

#ifdef LOOPSQUARED
#ifndef PRECISION_dp
  call gtdealloc()
#endif
#endif

end subroutine vamp_2

#ifdef LOOPSQUARED
subroutine gtdealloc()
  implicit none
  deallocate(G3tensorhel)
  deallocate(G4tensorhel)
  deallocate(G5tensorhel)

end subroutine gtdealloc
#endif

end module ol_vamp_2_ppllaj2_eexaggg_1_/**/REALKIND
