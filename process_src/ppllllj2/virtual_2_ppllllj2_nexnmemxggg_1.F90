
module ol_vamp_2_ppllllj2_nexnmemxggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), save, target, allocatable :: G3tensorhel(:,:,:)
  complex(REALKIND), save, target, allocatable :: G4tensorhel(:,:,:)
  complex(REALKIND), save, target, allocatable :: G5tensorhel(:,:,:)

  contains

! **********************************************************************
subroutine vamp_2(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppllllj2_nexnmemxggg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppllllj2_nexnmemxggg_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppllllj2_nexnmemxggg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppllllj2_nexnmemxggg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(inout) :: M(2)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,2) :: G0
  complex(REALKIND), dimension(4,5,4,5) :: G1
  complex(REALKIND), dimension(4,15,4,24) :: G2
  complex(REALKIND), dimension(4,35,4,38) :: G3
  complex(REALKIND), dimension(4,70,4,8) :: G4
  complex(REALKIND), pointer :: G3tensor(:,:)
  complex(REALKIND), pointer :: G4tensor(:,:)
  complex(REALKIND), pointer :: G5tensor(:,:)
#ifdef PRECISION_dp
  logical, save :: first = .true.
  if (first) then
#endif
    allocate(G3tensorhel(35,13,128))
    allocate(G4tensorhel(70,27,128))
    allocate(G5tensorhel(126,4,128))
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
  call loop_QV_A(G0(:,:,:,1),wf(:,-4),G0(:,:,:,2))
  call loop_Q_A(G0(:,:,:,2),Q(:,16),ZERO,G1(:,:,:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,9),G1(:,:,:,2))
  call loop_Q_A(G1(:,:,:,2),Q(:,112),ZERO,G2(:,:,:,1))
  call loop_QV_A(G2(:,:,:,1),wf(:,12),G2(:,:,:,2))
  call check_last_Q_A(l_switch,G2(:,:,:,2),Q(:,127),ZERO,G3tensor(:,1))
  call loop_QZ_A(G2(:,:,:,1),wf(:,43),G2(:,:,:,3),gZu)
  call check_last_Q_A(l_switch,G2(:,:,:,3),Q(:,127),ZERO,G3tensor(:,2))
  call loop_QZ_A(G2(:,:,:,1),wf(:,43),G2(:,:,:,4),gZd)
  call check_last_Q_A(l_switch,G2(:,:,:,4),Q(:,127),ZERO,G3tensor(:,3))
  call loop_QV_A(G2(:,:,:,1),wf(:,57),G2(:,:,:,5))
  call check_last_Q_A(l_switch,G2(:,:,:,5),Q(:,127),ZERO,G3tensor(:,4))
  call loop_QZ_A(G2(:,:,:,1),wf(:,58),G2(:,:,:,6),gZu)
  call check_last_Q_A(l_switch,G2(:,:,:,6),Q(:,127),ZERO,G3tensor(:,5))
  call loop_QZ_A(G2(:,:,:,1),wf(:,58),G2(:,:,:,7),gZd)
  call check_last_Q_A(l_switch,G2(:,:,:,7),Q(:,127),ZERO,G3tensor(:,6))
  call loop_QZ_A(G2(:,:,:,1),wf(:,59),G2(:,:,:,8),gZu)
  call check_last_Q_A(l_switch,G2(:,:,:,8),Q(:,127),ZERO,G3tensor(:,7))
  call loop_QZ_A(G2(:,:,:,1),wf(:,59),G2(:,:,:,9),gZd)
  call check_last_Q_A(l_switch,G2(:,:,:,9),Q(:,127),ZERO,G3tensor(:,8))
  call loop_QV_A(G2(:,:,:,1),wf(:,60),G2(:,:,:,10))
  call check_last_Q_A(l_switch,G2(:,:,:,10),Q(:,127),ZERO,G3tensor(:,9))
  call loop_QZ_A(G2(:,:,:,1),wf(:,61),G2(:,:,:,11),gZu)
  call check_last_Q_A(l_switch,G2(:,:,:,11),Q(:,127),ZERO,G3tensor(:,10))
  call loop_QZ_A(G2(:,:,:,1),wf(:,61),G2(:,:,:,12),gZd)
  call check_last_Q_A(l_switch,G2(:,:,:,12),Q(:,127),ZERO,G3tensor(:,11))
  call loop_QZ_A(G2(:,:,:,1),wf(:,62),G2(:,:,:,13),gZu)
  call check_last_Q_A(l_switch,G2(:,:,:,13),Q(:,127),ZERO,G3tensor(:,12))
  call loop_QZ_A(G2(:,:,:,1),wf(:,62),G2(:,:,:,14),gZd)
  call check_last_Q_A(l_switch,G2(:,:,:,14),Q(:,127),ZERO,G3tensor(:,13))
  call loop_QW_A(G1(:,:,:,1),wf(:,5),G1(:,:,:,3))
  call loop_Q_A(G1(:,:,:,3),Q(:,26),ZERO,G2(:,:,:,15))
  call loop_QV_A(G2(:,:,:,15),wf(:,9),G2(:,:,:,16))
  call loop_Q_A(G2(:,:,:,16),Q(:,122),ZERO,G3(:,:,:,1))
  call loop_QW_A(G3(:,:,:,1),wf(:,4),G3(:,:,:,2))
  call check_last_Q_A(l_switch,G3(:,:,:,2),Q(:,127),ZERO,G4tensor(:,1))
  call loop_QV_A(G2(:,:,:,15),wf(:,-6),G2(:,:,:,17))
  call loop_Q_A(G2(:,:,:,17),Q(:,90),ZERO,G3(:,:,:,3))
  call loop_QV_A(G3(:,:,:,3),wf(:,-5),G3(:,:,:,4))
  call loop_Q_A(G3(:,:,:,4),Q(:,122),ZERO,G4(:,:,:,1))
  call loop_QW_A(G4(:,:,:,1),wf(:,4),G4(:,:,:,2))
  call check_last_Q_A(l_switch,G4(:,:,:,2),Q(:,127),ZERO,G5tensor(:,1))
  call loop_QV_A(G2(:,:,:,15),wf(:,-5),G2(:,:,:,18))
  call loop_Q_A(G2(:,:,:,18),Q(:,58),ZERO,G3(:,:,:,5))
  call loop_QV_A(G3(:,:,:,5),wf(:,-6),G3(:,:,:,6))
  call loop_Q_A(G3(:,:,:,6),Q(:,122),ZERO,G4(:,:,:,3))
  call loop_QW_A(G4(:,:,:,3),wf(:,4),G4(:,:,:,4))
  call check_last_Q_A(l_switch,G4(:,:,:,4),Q(:,127),ZERO,G5tensor(:,2))
  call loop_QV_A(G1(:,:,:,1),wf(:,-5),G1(:,:,:,4))
  call loop_Q_A(G1(:,:,:,4),Q(:,48),ZERO,G2(:,:,:,19))
  call loop_QV_A(G2(:,:,:,19),wf(:,-6),G2(:,:,:,20))
  call loop_Q_A(G2(:,:,:,20),Q(:,112),ZERO,G3(:,:,:,7))
  call loop_QV_A(G3(:,:,:,7),wf(:,12),G3(:,:,:,8))
  call check_last_Q_A(l_switch,G3(:,:,:,8),Q(:,127),ZERO,G4tensor(:,2))
  call loop_QZ_A(G3(:,:,:,7),wf(:,43),G3(:,:,:,9),gZu)
  call check_last_Q_A(l_switch,G3(:,:,:,9),Q(:,127),ZERO,G4tensor(:,3))
  call loop_QZ_A(G3(:,:,:,7),wf(:,43),G3(:,:,:,10),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,10),Q(:,127),ZERO,G4tensor(:,4))
  call loop_QV_A(G3(:,:,:,7),wf(:,57),G3(:,:,:,11))
  call check_last_Q_A(l_switch,G3(:,:,:,11),Q(:,127),ZERO,G4tensor(:,5))
  call loop_QZ_A(G3(:,:,:,7),wf(:,58),G3(:,:,:,12),gZu)
  call check_last_Q_A(l_switch,G3(:,:,:,12),Q(:,127),ZERO,G4tensor(:,6))
  call loop_QZ_A(G3(:,:,:,7),wf(:,58),G3(:,:,:,13),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,13),Q(:,127),ZERO,G4tensor(:,7))
  call loop_QZ_A(G3(:,:,:,7),wf(:,59),G3(:,:,:,14),gZu)
  call check_last_Q_A(l_switch,G3(:,:,:,14),Q(:,127),ZERO,G4tensor(:,8))
  call loop_QZ_A(G3(:,:,:,7),wf(:,59),G3(:,:,:,15),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,15),Q(:,127),ZERO,G4tensor(:,9))
  call loop_QV_A(G3(:,:,:,7),wf(:,60),G3(:,:,:,16))
  call check_last_Q_A(l_switch,G3(:,:,:,16),Q(:,127),ZERO,G4tensor(:,10))
  call loop_QZ_A(G3(:,:,:,7),wf(:,61),G3(:,:,:,17),gZu)
  call check_last_Q_A(l_switch,G3(:,:,:,17),Q(:,127),ZERO,G4tensor(:,11))
  call loop_QZ_A(G3(:,:,:,7),wf(:,61),G3(:,:,:,18),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,18),Q(:,127),ZERO,G4tensor(:,12))
  call loop_QZ_A(G3(:,:,:,7),wf(:,62),G3(:,:,:,19),gZu)
  call check_last_Q_A(l_switch,G3(:,:,:,19),Q(:,127),ZERO,G4tensor(:,13))
  call loop_QZ_A(G3(:,:,:,7),wf(:,62),G3(:,:,:,20),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,20),Q(:,127),ZERO,G4tensor(:,14))
  call loop_QW_A(G2(:,:,:,19),wf(:,5),G2(:,:,:,21))
  call loop_Q_A(G2(:,:,:,21),Q(:,58),ZERO,G3(:,:,:,21))
  call loop_QV_A(G3(:,:,:,21),wf(:,-6),G3(:,:,:,22))
  call loop_Q_A(G3(:,:,:,22),Q(:,122),ZERO,G4(:,:,:,5))
  call loop_QW_A(G4(:,:,:,5),wf(:,4),G4(:,:,:,6))
  call check_last_Q_A(l_switch,G4(:,:,:,6),Q(:,127),ZERO,G5tensor(:,3))
  call loop_QV_A(G1(:,:,:,1),wf(:,-6),G1(:,:,:,5))
  call loop_Q_A(G1(:,:,:,5),Q(:,80),ZERO,G2(:,:,:,22))
  call loop_QV_A(G2(:,:,:,22),wf(:,-5),G2(:,:,:,23))
  call loop_Q_A(G2(:,:,:,23),Q(:,112),ZERO,G3(:,:,:,23))
  call loop_QV_A(G3(:,:,:,23),wf(:,12),G3(:,:,:,24))
  call check_last_Q_A(l_switch,G3(:,:,:,24),Q(:,127),ZERO,G4tensor(:,15))
  call loop_QZ_A(G3(:,:,:,23),wf(:,43),G3(:,:,:,25),gZu)
  call check_last_Q_A(l_switch,G3(:,:,:,25),Q(:,127),ZERO,G4tensor(:,16))
  call loop_QZ_A(G3(:,:,:,23),wf(:,43),G3(:,:,:,26),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,26),Q(:,127),ZERO,G4tensor(:,17))
  call loop_QV_A(G3(:,:,:,23),wf(:,57),G3(:,:,:,27))
  call check_last_Q_A(l_switch,G3(:,:,:,27),Q(:,127),ZERO,G4tensor(:,18))
  call loop_QZ_A(G3(:,:,:,23),wf(:,58),G3(:,:,:,28),gZu)
  call check_last_Q_A(l_switch,G3(:,:,:,28),Q(:,127),ZERO,G4tensor(:,19))
  call loop_QZ_A(G3(:,:,:,23),wf(:,58),G3(:,:,:,29),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,29),Q(:,127),ZERO,G4tensor(:,20))
  call loop_QZ_A(G3(:,:,:,23),wf(:,59),G3(:,:,:,30),gZu)
  call check_last_Q_A(l_switch,G3(:,:,:,30),Q(:,127),ZERO,G4tensor(:,21))
  call loop_QZ_A(G3(:,:,:,23),wf(:,59),G3(:,:,:,31),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,31),Q(:,127),ZERO,G4tensor(:,22))
  call loop_QV_A(G3(:,:,:,23),wf(:,60),G3(:,:,:,32))
  call check_last_Q_A(l_switch,G3(:,:,:,32),Q(:,127),ZERO,G4tensor(:,23))
  call loop_QZ_A(G3(:,:,:,23),wf(:,61),G3(:,:,:,33),gZu)
  call check_last_Q_A(l_switch,G3(:,:,:,33),Q(:,127),ZERO,G4tensor(:,24))
  call loop_QZ_A(G3(:,:,:,23),wf(:,61),G3(:,:,:,34),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,34),Q(:,127),ZERO,G4tensor(:,25))
  call loop_QZ_A(G3(:,:,:,23),wf(:,62),G3(:,:,:,35),gZu)
  call check_last_Q_A(l_switch,G3(:,:,:,35),Q(:,127),ZERO,G4tensor(:,26))
  call loop_QZ_A(G3(:,:,:,23),wf(:,62),G3(:,:,:,36),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,36),Q(:,127),ZERO,G4tensor(:,27))
  call loop_QW_A(G2(:,:,:,22),wf(:,5),G2(:,:,:,24))
  call loop_Q_A(G2(:,:,:,24),Q(:,90),ZERO,G3(:,:,:,37))
  call loop_QV_A(G3(:,:,:,37),wf(:,-5),G3(:,:,:,38))
  call loop_Q_A(G3(:,:,:,38),Q(:,122),ZERO,G4(:,:,:,7))
  call loop_QW_A(G4(:,:,:,7),wf(:,4),G4(:,:,:,8))
  call check_last_Q_A(l_switch,G4(:,:,:,8),Q(:,127),ZERO,G5tensor(:,4))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  M(1) = M(1) + (CI*f(22) * den(69)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,1),T3sum(1:35,9))
  M(2) = M(2) + (-(CI*f(22)) * den(69)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,1),T3sum(1:35,9))
  M(1) = M(1) + (-(CI*f(20)) * den(69)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,1),T3sum(1:35,9))
  M(2) = M(2) + (CI*f(20) * den(69)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,1),T3sum(1:35,9))
  M(1) = M(1) + (CI*f(16) * den(37)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,2),T3sum(1:35,9))
  M(2) = M(2) + (-(CI*f(16)) * den(37)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,2),T3sum(1:35,9))
  M(1) = M(1) + (CI*f(16) * den(37)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,3),T3sum(1:35,9))
  M(2) = M(2) + (-(CI*f(16)) * den(37)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,3),T3sum(1:35,9))
  M(1) = M(1) + (CI*f(8) * den(70)) * TI2_call(4,momenta_10,masses2_10,G4tensor(:,1),T4sum(1:70,19))
  M(2) = M(2) + (-(CI*f(8)) * den(70)) * TI2_call(4,momenta_10,masses2_10,G4tensor(:,1),T4sum(1:70,19))
  M(2) = M(2) + (-f(27) * den(58)) * TI2_call(4,momenta_11,masses2_10,G4tensor(:,2),T4sum(1:70,33))
  M(2) = M(2) + (f(25) * den(58)) * TI2_call(4,momenta_11,masses2_10,G4tensor(:,2),T4sum(1:70,33))
  M(2) = M(2) + (-f(18) * den(32)) * TI2_call(4,momenta_11,masses2_10,G4tensor(:,3),T4sum(1:70,33))
  M(2) = M(2) + (-f(18) * den(32)) * TI2_call(4,momenta_11,masses2_10,G4tensor(:,4),T4sum(1:70,33))
  M(1) = M(1) + (-f(27) * den(58)) * TI2_call(4,momenta_12,masses2_10,G4tensor(:,15),T4sum(1:70,36))
  M(1) = M(1) + (f(25) * den(58)) * TI2_call(4,momenta_12,masses2_10,G4tensor(:,15),T4sum(1:70,36))
  M(1) = M(1) + (-f(18) * den(32)) * TI2_call(4,momenta_12,masses2_10,G4tensor(:,16),T4sum(1:70,36))
  M(1) = M(1) + (-f(18) * den(32)) * TI2_call(4,momenta_12,masses2_10,G4tensor(:,17),T4sum(1:70,36))
  M(2) = M(2) + (-f(10) * den(4)) * TI2_call(5,momenta_24,masses2_17,G5tensor(:,3),T5sum(1:126,10))
  M(1) = M(1) + (-f(10) * den(4)) * TI2_call(5,momenta_25,masses2_17,G5tensor(:,4),T5sum(1:126,22))
  M(1) = M(1) + (-f(10) * den(4)) * TI2_call(5,momenta_23,masses2_17,G5tensor(:,1),T5sum(1:126,31))
  M(2) = M(2) + (-f(10) * den(4)) * TI2_call(5,momenta_22,masses2_17,G5tensor(:,2),T5sum(1:126,34))
  M(1) = M(1) + (-(CI*f(22)) * den(77)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,4),T3sum(1:35,9))
  M(2) = M(2) + (CI*f(22) * den(77)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,4),T3sum(1:35,9))
  M(1) = M(1) + (CI*f(20) * den(77)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,4),T3sum(1:35,9))
  M(2) = M(2) + (-(CI*f(20)) * den(77)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,4),T3sum(1:35,9))
  M(1) = M(1) + (CI*f(23) * den(78)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,5),T3sum(1:35,9))
  M(2) = M(2) + (-(CI*f(23)) * den(78)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,5),T3sum(1:35,9))
  M(1) = M(1) + (CI*f(23) * den(78)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,6),T3sum(1:35,9))
  M(2) = M(2) + (-(CI*f(23)) * den(78)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,6),T3sum(1:35,9))
  M(1) = M(1) + (CI*f(23) * den(79)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,7),T3sum(1:35,9))
  M(2) = M(2) + (-(CI*f(23)) * den(79)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,7),T3sum(1:35,9))
  M(1) = M(1) + (CI*f(23) * den(79)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,8),T3sum(1:35,9))
  M(2) = M(2) + (-(CI*f(23)) * den(79)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,8),T3sum(1:35,9))
  M(2) = M(2) + (f(27) * den(59)) * TI2_call(4,momenta_11,masses2_10,G4tensor(:,5),T4sum(1:70,33))
  M(2) = M(2) + (-f(25) * den(59)) * TI2_call(4,momenta_11,masses2_10,G4tensor(:,5),T4sum(1:70,33))
  M(2) = M(2) + (-f(28) * den(60)) * TI2_call(4,momenta_11,masses2_10,G4tensor(:,6),T4sum(1:70,33))
  M(2) = M(2) + (-f(28) * den(60)) * TI2_call(4,momenta_11,masses2_10,G4tensor(:,7),T4sum(1:70,33))
  M(1) = M(1) + (f(27) * den(59)) * TI2_call(4,momenta_12,masses2_10,G4tensor(:,18),T4sum(1:70,36))
  M(1) = M(1) + (-f(25) * den(59)) * TI2_call(4,momenta_12,masses2_10,G4tensor(:,18),T4sum(1:70,36))
  M(1) = M(1) + (-f(28) * den(60)) * TI2_call(4,momenta_12,masses2_10,G4tensor(:,19),T4sum(1:70,36))
  M(1) = M(1) + (-f(28) * den(60)) * TI2_call(4,momenta_12,masses2_10,G4tensor(:,20),T4sum(1:70,36))
  M(2) = M(2) + (-f(28) * den(61)) * TI2_call(4,momenta_11,masses2_10,G4tensor(:,8),T4sum(1:70,33))
  M(2) = M(2) + (-f(28) * den(61)) * TI2_call(4,momenta_11,masses2_10,G4tensor(:,9),T4sum(1:70,33))
  M(1) = M(1) + (-f(28) * den(61)) * TI2_call(4,momenta_12,masses2_10,G4tensor(:,21),T4sum(1:70,36))
  M(1) = M(1) + (-f(28) * den(61)) * TI2_call(4,momenta_12,masses2_10,G4tensor(:,22),T4sum(1:70,36))
  M(1) = M(1) + (-(CI*f(22)) * den(86)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,9),T3sum(1:35,9))
  M(2) = M(2) + (CI*f(22) * den(86)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,9),T3sum(1:35,9))
  M(1) = M(1) + (CI*f(20) * den(86)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,9),T3sum(1:35,9))
  M(2) = M(2) + (-(CI*f(20)) * den(86)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,9),T3sum(1:35,9))
  M(1) = M(1) + (CI*f(23) * den(87)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,10),T3sum(1:35,9))
  M(2) = M(2) + (-(CI*f(23)) * den(87)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,10),T3sum(1:35,9))
  M(1) = M(1) + (CI*f(23) * den(87)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,11),T3sum(1:35,9))
  M(2) = M(2) + (-(CI*f(23)) * den(87)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,11),T3sum(1:35,9))
  M(1) = M(1) + (CI*f(23) * den(88)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,12),T3sum(1:35,9))
  M(2) = M(2) + (-(CI*f(23)) * den(88)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,12),T3sum(1:35,9))
  M(1) = M(1) + (CI*f(23) * den(88)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,13),T3sum(1:35,9))
  M(2) = M(2) + (-(CI*f(23)) * den(88)) * TI2_call(3,momenta_1,masses2_3,G3tensor(:,13),T3sum(1:35,9))
  M(2) = M(2) + (f(27) * den(62)) * TI2_call(4,momenta_11,masses2_10,G4tensor(:,10),T4sum(1:70,33))
  M(2) = M(2) + (-f(25) * den(62)) * TI2_call(4,momenta_11,masses2_10,G4tensor(:,10),T4sum(1:70,33))
  M(2) = M(2) + (-f(28) * den(63)) * TI2_call(4,momenta_11,masses2_10,G4tensor(:,11),T4sum(1:70,33))
  M(2) = M(2) + (-f(28) * den(63)) * TI2_call(4,momenta_11,masses2_10,G4tensor(:,12),T4sum(1:70,33))
  M(1) = M(1) + (f(27) * den(62)) * TI2_call(4,momenta_12,masses2_10,G4tensor(:,23),T4sum(1:70,36))
  M(1) = M(1) + (-f(25) * den(62)) * TI2_call(4,momenta_12,masses2_10,G4tensor(:,23),T4sum(1:70,36))
  M(1) = M(1) + (-f(28) * den(63)) * TI2_call(4,momenta_12,masses2_10,G4tensor(:,24),T4sum(1:70,36))
  M(1) = M(1) + (-f(28) * den(63)) * TI2_call(4,momenta_12,masses2_10,G4tensor(:,25),T4sum(1:70,36))
  M(2) = M(2) + (-f(28) * den(64)) * TI2_call(4,momenta_11,masses2_10,G4tensor(:,13),T4sum(1:70,33))
  M(2) = M(2) + (-f(28) * den(64)) * TI2_call(4,momenta_11,masses2_10,G4tensor(:,14),T4sum(1:70,33))
  M(1) = M(1) + (-f(28) * den(64)) * TI2_call(4,momenta_12,masses2_10,G4tensor(:,26),T4sum(1:70,36))
  M(1) = M(1) + (-f(28) * den(64)) * TI2_call(4,momenta_12,masses2_10,G4tensor(:,27),T4sum(1:70,36))

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

end module ol_vamp_2_ppllllj2_nexnmemxggg_1_/**/REALKIND
