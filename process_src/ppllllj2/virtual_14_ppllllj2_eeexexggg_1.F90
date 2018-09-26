
module ol_vamp_14_ppllllj2_eeexexggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), save, target, allocatable :: G4tensorhel(:,:,:)
  complex(REALKIND), save, target, allocatable :: G5tensorhel(:,:,:)

  contains

! **********************************************************************
subroutine vamp_14(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppllllj2_eeexexggg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppllllj2_eeexexggg_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppllllj2_eeexexggg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppllllj2_eeexexggg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(inout) :: M(2)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,3) :: G0
  complex(REALKIND), dimension(4,5,4,14) :: G1
  complex(REALKIND), dimension(4,15,4,36) :: G2
  complex(REALKIND), dimension(4,35,4,60) :: G3
  complex(REALKIND), dimension(4,70,4,36) :: G4
  complex(REALKIND), pointer :: G4tensor(:,:)
  complex(REALKIND), pointer :: G5tensor(:,:)
#ifdef PRECISION_dp
  logical, save :: first = .true.
  if (first) then
#endif
    allocate(G4tensorhel(70,24,128))
    allocate(G5tensorhel(126,24,128))
#ifdef PRECISION_dp
    first = .false.
  end if
#endif
  if (mode == -1) then
    call gtdealloc()
    return
  end if
  G4tensor => G4tensorhel(:,:,hel)
  G5tensor => G5tensorhel(:,:,hel)

  if (mode == 1 .or. stability_mode < 20 .or. stability_mode >= 30) then

  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_QZ_A(G0(:,:,:,1),wf(:,66),G0(:,:,:,2),gZd)
  call loop_Q_A(G0(:,:,:,2),Q(:,9),ZERO,G1(:,:,:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,-6),G1(:,:,:,2))
  call loop_Q_A(G1(:,:,:,2),Q(:,73),ZERO,G2(:,:,:,1))
  call loop_QV_A(G2(:,:,:,1),wf(:,3),G2(:,:,:,2))
  call loop_Q_A(G2(:,:,:,2),Q(:,121),ZERO,G3(:,:,:,1))
  call loop_QV_A(G3(:,:,:,1),wf(:,60),G3(:,:,:,2))
  call check_last_Q_A(l_switch,G3(:,:,:,2),Q(:,127),ZERO,G4tensor(:,1))
  call loop_QZ_A(G3(:,:,:,1),wf(:,63),G3(:,:,:,3),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,3),Q(:,127),ZERO,G4tensor(:,2))
  call loop_QV_A(G2(:,:,:,1),wf(:,-4),G2(:,:,:,3))
  call loop_Q_A(G2(:,:,:,3),Q(:,89),ZERO,G3(:,:,:,4))
  call loop_QV_A(G3(:,:,:,4),wf(:,-5),G3(:,:,:,5))
  call loop_Q_A(G3(:,:,:,5),Q(:,121),ZERO,G4(:,:,:,1))
  call loop_QV_A(G4(:,:,:,1),wf(:,60),G4(:,:,:,2))
  call check_last_Q_A(l_switch,G4(:,:,:,2),Q(:,127),ZERO,G5tensor(:,1))
  call loop_QZ_A(G4(:,:,:,1),wf(:,63),G4(:,:,:,3),gZd)
  call check_last_Q_A(l_switch,G4(:,:,:,3),Q(:,127),ZERO,G5tensor(:,2))
  call loop_QV_A(G2(:,:,:,1),wf(:,-5),G2(:,:,:,4))
  call loop_Q_A(G2(:,:,:,4),Q(:,105),ZERO,G3(:,:,:,6))
  call loop_QV_A(G3(:,:,:,6),wf(:,-4),G3(:,:,:,7))
  call loop_Q_A(G3(:,:,:,7),Q(:,121),ZERO,G4(:,:,:,4))
  call loop_QV_A(G4(:,:,:,4),wf(:,60),G4(:,:,:,5))
  call check_last_Q_A(l_switch,G4(:,:,:,5),Q(:,127),ZERO,G5tensor(:,3))
  call loop_QZ_A(G4(:,:,:,4),wf(:,63),G4(:,:,:,6),gZd)
  call check_last_Q_A(l_switch,G4(:,:,:,6),Q(:,127),ZERO,G5tensor(:,4))
  call loop_QV_A(G1(:,:,:,1),wf(:,3),G1(:,:,:,3))
  call loop_Q_A(G1(:,:,:,3),Q(:,57),ZERO,G2(:,:,:,5))
  call loop_QV_A(G2(:,:,:,5),wf(:,-6),G2(:,:,:,6))
  call loop_Q_A(G2(:,:,:,6),Q(:,121),ZERO,G3(:,:,:,8))
  call loop_QV_A(G3(:,:,:,8),wf(:,60),G3(:,:,:,9))
  call check_last_Q_A(l_switch,G3(:,:,:,9),Q(:,127),ZERO,G4tensor(:,3))
  call loop_QZ_A(G3(:,:,:,8),wf(:,63),G3(:,:,:,10),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,10),Q(:,127),ZERO,G4tensor(:,4))
  call loop_QV_A(G1(:,:,:,1),wf(:,-5),G1(:,:,:,4))
  call loop_Q_A(G1(:,:,:,4),Q(:,41),ZERO,G2(:,:,:,7))
  call loop_QV_A(G2(:,:,:,7),wf(:,15),G2(:,:,:,8))
  call loop_Q_A(G2(:,:,:,8),Q(:,121),ZERO,G3(:,:,:,11))
  call loop_QV_A(G3(:,:,:,11),wf(:,60),G3(:,:,:,12))
  call check_last_Q_A(l_switch,G3(:,:,:,12),Q(:,127),ZERO,G4tensor(:,5))
  call loop_QZ_A(G3(:,:,:,11),wf(:,63),G3(:,:,:,13),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,13),Q(:,127),ZERO,G4tensor(:,6))
  call loop_QV_A(G2(:,:,:,7),wf(:,-4),G2(:,:,:,9))
  call loop_Q_A(G2(:,:,:,9),Q(:,57),ZERO,G3(:,:,:,14))
  call loop_QV_A(G3(:,:,:,14),wf(:,-6),G3(:,:,:,15))
  call loop_Q_A(G3(:,:,:,15),Q(:,121),ZERO,G4(:,:,:,7))
  call loop_QV_A(G4(:,:,:,7),wf(:,60),G4(:,:,:,8))
  call check_last_Q_A(l_switch,G4(:,:,:,8),Q(:,127),ZERO,G5tensor(:,5))
  call loop_QZ_A(G4(:,:,:,7),wf(:,63),G4(:,:,:,9),gZd)
  call check_last_Q_A(l_switch,G4(:,:,:,9),Q(:,127),ZERO,G5tensor(:,6))
  call loop_QV_A(G2(:,:,:,7),wf(:,-6),G2(:,:,:,10))
  call loop_Q_A(G2(:,:,:,10),Q(:,105),ZERO,G3(:,:,:,16))
  call loop_QV_A(G3(:,:,:,16),wf(:,-4),G3(:,:,:,17))
  call loop_Q_A(G3(:,:,:,17),Q(:,121),ZERO,G4(:,:,:,10))
  call loop_QV_A(G4(:,:,:,10),wf(:,60),G4(:,:,:,11))
  call check_last_Q_A(l_switch,G4(:,:,:,11),Q(:,127),ZERO,G5tensor(:,7))
  call loop_QZ_A(G4(:,:,:,10),wf(:,63),G4(:,:,:,12),gZd)
  call check_last_Q_A(l_switch,G4(:,:,:,12),Q(:,127),ZERO,G5tensor(:,8))
  call loop_QV_A(G1(:,:,:,1),wf(:,15),G1(:,:,:,5))
  call loop_Q_A(G1(:,:,:,5),Q(:,89),ZERO,G2(:,:,:,11))
  call loop_QV_A(G2(:,:,:,11),wf(:,-5),G2(:,:,:,12))
  call loop_Q_A(G2(:,:,:,12),Q(:,121),ZERO,G3(:,:,:,18))
  call loop_QV_A(G3(:,:,:,18),wf(:,60),G3(:,:,:,19))
  call check_last_Q_A(l_switch,G3(:,:,:,19),Q(:,127),ZERO,G4tensor(:,7))
  call loop_QZ_A(G3(:,:,:,18),wf(:,63),G3(:,:,:,20),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,20),Q(:,127),ZERO,G4tensor(:,8))
  call loop_QV_A(G1(:,:,:,1),wf(:,-4),G1(:,:,:,6))
  call loop_Q_A(G1(:,:,:,6),Q(:,25),ZERO,G2(:,:,:,13))
  call loop_QV_A(G2(:,:,:,13),wf(:,21),G2(:,:,:,14))
  call loop_Q_A(G2(:,:,:,14),Q(:,121),ZERO,G3(:,:,:,21))
  call loop_QV_A(G3(:,:,:,21),wf(:,60),G3(:,:,:,22))
  call check_last_Q_A(l_switch,G3(:,:,:,22),Q(:,127),ZERO,G4tensor(:,9))
  call loop_QZ_A(G3(:,:,:,21),wf(:,63),G3(:,:,:,23),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,23),Q(:,127),ZERO,G4tensor(:,10))
  call loop_QV_A(G2(:,:,:,13),wf(:,-5),G2(:,:,:,15))
  call loop_Q_A(G2(:,:,:,15),Q(:,57),ZERO,G3(:,:,:,24))
  call loop_QV_A(G3(:,:,:,24),wf(:,-6),G3(:,:,:,25))
  call loop_Q_A(G3(:,:,:,25),Q(:,121),ZERO,G4(:,:,:,13))
  call loop_QV_A(G4(:,:,:,13),wf(:,60),G4(:,:,:,14))
  call check_last_Q_A(l_switch,G4(:,:,:,14),Q(:,127),ZERO,G5tensor(:,9))
  call loop_QZ_A(G4(:,:,:,13),wf(:,63),G4(:,:,:,15),gZd)
  call check_last_Q_A(l_switch,G4(:,:,:,15),Q(:,127),ZERO,G5tensor(:,10))
  call loop_QV_A(G2(:,:,:,13),wf(:,-6),G2(:,:,:,16))
  call loop_Q_A(G2(:,:,:,16),Q(:,89),ZERO,G3(:,:,:,26))
  call loop_QV_A(G3(:,:,:,26),wf(:,-5),G3(:,:,:,27))
  call loop_Q_A(G3(:,:,:,27),Q(:,121),ZERO,G4(:,:,:,16))
  call loop_QV_A(G4(:,:,:,16),wf(:,60),G4(:,:,:,17))
  call check_last_Q_A(l_switch,G4(:,:,:,17),Q(:,127),ZERO,G5tensor(:,11))
  call loop_QZ_A(G4(:,:,:,16),wf(:,63),G4(:,:,:,18),gZd)
  call check_last_Q_A(l_switch,G4(:,:,:,18),Q(:,127),ZERO,G5tensor(:,12))
  call loop_QV_A(G1(:,:,:,1),wf(:,21),G1(:,:,:,7))
  call loop_Q_A(G1(:,:,:,7),Q(:,105),ZERO,G2(:,:,:,17))
  call loop_QV_A(G2(:,:,:,17),wf(:,-4),G2(:,:,:,18))
  call loop_Q_A(G2(:,:,:,18),Q(:,121),ZERO,G3(:,:,:,28))
  call loop_QV_A(G3(:,:,:,28),wf(:,60),G3(:,:,:,29))
  call check_last_Q_A(l_switch,G3(:,:,:,29),Q(:,127),ZERO,G4tensor(:,11))
  call loop_QZ_A(G3(:,:,:,28),wf(:,63),G3(:,:,:,30),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,30),Q(:,127),ZERO,G4tensor(:,12))
  call loop_AZ_Q(G0(:,:,:,1),wf(:,66),G0(:,:,:,3),gZd)
  call loop_A_Q(G0(:,:,:,3),Q(:,9),ZERO,G1(:,:,:,8))
  call loop_AV_Q(G1(:,:,:,8),wf(:,-6),G1(:,:,:,9))
  call loop_A_Q(G1(:,:,:,9),Q(:,73),ZERO,G2(:,:,:,19))
  call loop_AV_Q(G2(:,:,:,19),wf(:,3),G2(:,:,:,20))
  call loop_A_Q(G2(:,:,:,20),Q(:,121),ZERO,G3(:,:,:,31))
  call loop_AV_Q(G3(:,:,:,31),wf(:,60),G3(:,:,:,32))
  call check_last_A_Q(l_switch,G3(:,:,:,32),Q(:,127),ZERO,G4tensor(:,13))
  call loop_AZ_Q(G3(:,:,:,31),wf(:,63),G3(:,:,:,33),gZd)
  call check_last_A_Q(l_switch,G3(:,:,:,33),Q(:,127),ZERO,G4tensor(:,14))
  call loop_AV_Q(G2(:,:,:,19),wf(:,-4),G2(:,:,:,21))
  call loop_A_Q(G2(:,:,:,21),Q(:,89),ZERO,G3(:,:,:,34))
  call loop_AV_Q(G3(:,:,:,34),wf(:,-5),G3(:,:,:,35))
  call loop_A_Q(G3(:,:,:,35),Q(:,121),ZERO,G4(:,:,:,19))
  call loop_AV_Q(G4(:,:,:,19),wf(:,60),G4(:,:,:,20))
  call check_last_A_Q(l_switch,G4(:,:,:,20),Q(:,127),ZERO,G5tensor(:,13))
  call loop_AZ_Q(G4(:,:,:,19),wf(:,63),G4(:,:,:,21),gZd)
  call check_last_A_Q(l_switch,G4(:,:,:,21),Q(:,127),ZERO,G5tensor(:,14))
  call loop_AV_Q(G2(:,:,:,19),wf(:,-5),G2(:,:,:,22))
  call loop_A_Q(G2(:,:,:,22),Q(:,105),ZERO,G3(:,:,:,36))
  call loop_AV_Q(G3(:,:,:,36),wf(:,-4),G3(:,:,:,37))
  call loop_A_Q(G3(:,:,:,37),Q(:,121),ZERO,G4(:,:,:,22))
  call loop_AV_Q(G4(:,:,:,22),wf(:,60),G4(:,:,:,23))
  call check_last_A_Q(l_switch,G4(:,:,:,23),Q(:,127),ZERO,G5tensor(:,15))
  call loop_AZ_Q(G4(:,:,:,22),wf(:,63),G4(:,:,:,24),gZd)
  call check_last_A_Q(l_switch,G4(:,:,:,24),Q(:,127),ZERO,G5tensor(:,16))
  call loop_AV_Q(G1(:,:,:,8),wf(:,3),G1(:,:,:,10))
  call loop_A_Q(G1(:,:,:,10),Q(:,57),ZERO,G2(:,:,:,23))
  call loop_AV_Q(G2(:,:,:,23),wf(:,-6),G2(:,:,:,24))
  call loop_A_Q(G2(:,:,:,24),Q(:,121),ZERO,G3(:,:,:,38))
  call loop_AV_Q(G3(:,:,:,38),wf(:,60),G3(:,:,:,39))
  call check_last_A_Q(l_switch,G3(:,:,:,39),Q(:,127),ZERO,G4tensor(:,15))
  call loop_AZ_Q(G3(:,:,:,38),wf(:,63),G3(:,:,:,40),gZd)
  call check_last_A_Q(l_switch,G3(:,:,:,40),Q(:,127),ZERO,G4tensor(:,16))
  call loop_AV_Q(G1(:,:,:,8),wf(:,-5),G1(:,:,:,11))
  call loop_A_Q(G1(:,:,:,11),Q(:,41),ZERO,G2(:,:,:,25))
  call loop_AV_Q(G2(:,:,:,25),wf(:,15),G2(:,:,:,26))
  call loop_A_Q(G2(:,:,:,26),Q(:,121),ZERO,G3(:,:,:,41))
  call loop_AV_Q(G3(:,:,:,41),wf(:,60),G3(:,:,:,42))
  call check_last_A_Q(l_switch,G3(:,:,:,42),Q(:,127),ZERO,G4tensor(:,17))
  call loop_AZ_Q(G3(:,:,:,41),wf(:,63),G3(:,:,:,43),gZd)
  call check_last_A_Q(l_switch,G3(:,:,:,43),Q(:,127),ZERO,G4tensor(:,18))
  call loop_AV_Q(G2(:,:,:,25),wf(:,-4),G2(:,:,:,27))
  call loop_A_Q(G2(:,:,:,27),Q(:,57),ZERO,G3(:,:,:,44))
  call loop_AV_Q(G3(:,:,:,44),wf(:,-6),G3(:,:,:,45))
  call loop_A_Q(G3(:,:,:,45),Q(:,121),ZERO,G4(:,:,:,25))
  call loop_AV_Q(G4(:,:,:,25),wf(:,60),G4(:,:,:,26))
  call check_last_A_Q(l_switch,G4(:,:,:,26),Q(:,127),ZERO,G5tensor(:,17))
  call loop_AZ_Q(G4(:,:,:,25),wf(:,63),G4(:,:,:,27),gZd)
  call check_last_A_Q(l_switch,G4(:,:,:,27),Q(:,127),ZERO,G5tensor(:,18))
  call loop_AV_Q(G2(:,:,:,25),wf(:,-6),G2(:,:,:,28))
  call loop_A_Q(G2(:,:,:,28),Q(:,105),ZERO,G3(:,:,:,46))
  call loop_AV_Q(G3(:,:,:,46),wf(:,-4),G3(:,:,:,47))
  call loop_A_Q(G3(:,:,:,47),Q(:,121),ZERO,G4(:,:,:,28))
  call loop_AV_Q(G4(:,:,:,28),wf(:,60),G4(:,:,:,29))
  call check_last_A_Q(l_switch,G4(:,:,:,29),Q(:,127),ZERO,G5tensor(:,19))
  call loop_AZ_Q(G4(:,:,:,28),wf(:,63),G4(:,:,:,30),gZd)
  call check_last_A_Q(l_switch,G4(:,:,:,30),Q(:,127),ZERO,G5tensor(:,20))
  call loop_AV_Q(G1(:,:,:,8),wf(:,15),G1(:,:,:,12))
  call loop_A_Q(G1(:,:,:,12),Q(:,89),ZERO,G2(:,:,:,29))
  call loop_AV_Q(G2(:,:,:,29),wf(:,-5),G2(:,:,:,30))
  call loop_A_Q(G2(:,:,:,30),Q(:,121),ZERO,G3(:,:,:,48))
  call loop_AV_Q(G3(:,:,:,48),wf(:,60),G3(:,:,:,49))
  call check_last_A_Q(l_switch,G3(:,:,:,49),Q(:,127),ZERO,G4tensor(:,19))
  call loop_AZ_Q(G3(:,:,:,48),wf(:,63),G3(:,:,:,50),gZd)
  call check_last_A_Q(l_switch,G3(:,:,:,50),Q(:,127),ZERO,G4tensor(:,20))
  call loop_AV_Q(G1(:,:,:,8),wf(:,-4),G1(:,:,:,13))
  call loop_A_Q(G1(:,:,:,13),Q(:,25),ZERO,G2(:,:,:,31))
  call loop_AV_Q(G2(:,:,:,31),wf(:,21),G2(:,:,:,32))
  call loop_A_Q(G2(:,:,:,32),Q(:,121),ZERO,G3(:,:,:,51))
  call loop_AV_Q(G3(:,:,:,51),wf(:,60),G3(:,:,:,52))
  call check_last_A_Q(l_switch,G3(:,:,:,52),Q(:,127),ZERO,G4tensor(:,21))
  call loop_AZ_Q(G3(:,:,:,51),wf(:,63),G3(:,:,:,53),gZd)
  call check_last_A_Q(l_switch,G3(:,:,:,53),Q(:,127),ZERO,G4tensor(:,22))
  call loop_AV_Q(G2(:,:,:,31),wf(:,-5),G2(:,:,:,33))
  call loop_A_Q(G2(:,:,:,33),Q(:,57),ZERO,G3(:,:,:,54))
  call loop_AV_Q(G3(:,:,:,54),wf(:,-6),G3(:,:,:,55))
  call loop_A_Q(G3(:,:,:,55),Q(:,121),ZERO,G4(:,:,:,31))
  call loop_AV_Q(G4(:,:,:,31),wf(:,60),G4(:,:,:,32))
  call check_last_A_Q(l_switch,G4(:,:,:,32),Q(:,127),ZERO,G5tensor(:,21))
  call loop_AZ_Q(G4(:,:,:,31),wf(:,63),G4(:,:,:,33),gZd)
  call check_last_A_Q(l_switch,G4(:,:,:,33),Q(:,127),ZERO,G5tensor(:,22))
  call loop_AV_Q(G2(:,:,:,31),wf(:,-6),G2(:,:,:,34))
  call loop_A_Q(G2(:,:,:,34),Q(:,89),ZERO,G3(:,:,:,56))
  call loop_AV_Q(G3(:,:,:,56),wf(:,-5),G3(:,:,:,57))
  call loop_A_Q(G3(:,:,:,57),Q(:,121),ZERO,G4(:,:,:,34))
  call loop_AV_Q(G4(:,:,:,34),wf(:,60),G4(:,:,:,35))
  call check_last_A_Q(l_switch,G4(:,:,:,35),Q(:,127),ZERO,G5tensor(:,23))
  call loop_AZ_Q(G4(:,:,:,34),wf(:,63),G4(:,:,:,36),gZd)
  call check_last_A_Q(l_switch,G4(:,:,:,36),Q(:,127),ZERO,G5tensor(:,24))
  call loop_AV_Q(G1(:,:,:,8),wf(:,21),G1(:,:,:,14))
  call loop_A_Q(G1(:,:,:,14),Q(:,105),ZERO,G2(:,:,:,35))
  call loop_AV_Q(G2(:,:,:,35),wf(:,-4),G2(:,:,:,36))
  call loop_A_Q(G2(:,:,:,36),Q(:,121),ZERO,G3(:,:,:,58))
  call loop_AV_Q(G3(:,:,:,58),wf(:,60),G3(:,:,:,59))
  call check_last_A_Q(l_switch,G3(:,:,:,59),Q(:,127),ZERO,G4tensor(:,23))
  call loop_AZ_Q(G3(:,:,:,58),wf(:,63),G3(:,:,:,60),gZd)
  call check_last_A_Q(l_switch,G3(:,:,:,60),Q(:,127),ZERO,G4tensor(:,24))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  M(1) = M(1) + (CI*f(10) * den(496)) * TI2_call(4,momenta_7,masses2_6,G4tensor(:,1),T4sum(1:70,42))
  M(2) = M(2) + (-(CI*f(10)) * den(496)) * TI2_call(4,momenta_7,masses2_6,G4tensor(:,1),T4sum(1:70,42))
  M(1) = M(1) + (CI*f(10) * den(496)) * TI2_call(4,momenta_7,masses2_6,G4tensor(:,13),T4sum(1:70,42))
  M(2) = M(2) + (-(CI*f(10)) * den(496)) * TI2_call(4,momenta_7,masses2_6,G4tensor(:,13),T4sum(1:70,42))
  M(1) = M(1) + (CI*f(14) * den(497)) * TI2_call(4,momenta_7,masses2_6,G4tensor(:,2),T4sum(1:70,42))
  M(2) = M(2) + (-(CI*f(14)) * den(497)) * TI2_call(4,momenta_7,masses2_6,G4tensor(:,2),T4sum(1:70,42))
  M(1) = M(1) + (CI*f(14) * den(497)) * TI2_call(4,momenta_7,masses2_6,G4tensor(:,14),T4sum(1:70,42))
  M(2) = M(2) + (-(CI*f(14)) * den(497)) * TI2_call(4,momenta_7,masses2_6,G4tensor(:,14),T4sum(1:70,42))
  M(1) = M(1) + (CI*f(10) * den(496)) * TI2_call(4,momenta_6,masses2_6,G4tensor(:,15),T4sum(1:70,45))
  M(2) = M(2) + (-(CI*f(10)) * den(496)) * TI2_call(4,momenta_6,masses2_6,G4tensor(:,15),T4sum(1:70,45))
  M(1) = M(1) + (CI*f(10) * den(496)) * TI2_call(4,momenta_6,masses2_6,G4tensor(:,3),T4sum(1:70,45))
  M(2) = M(2) + (-(CI*f(10)) * den(496)) * TI2_call(4,momenta_6,masses2_6,G4tensor(:,3),T4sum(1:70,45))
  M(1) = M(1) + (CI*f(14) * den(497)) * TI2_call(4,momenta_6,masses2_6,G4tensor(:,16),T4sum(1:70,45))
  M(2) = M(2) + (-(CI*f(14)) * den(497)) * TI2_call(4,momenta_6,masses2_6,G4tensor(:,16),T4sum(1:70,45))
  M(1) = M(1) + (CI*f(14) * den(497)) * TI2_call(4,momenta_6,masses2_6,G4tensor(:,4),T4sum(1:70,45))
  M(2) = M(2) + (-(CI*f(14)) * den(497)) * TI2_call(4,momenta_6,masses2_6,G4tensor(:,4),T4sum(1:70,45))
  M(1) = M(1) + (-(CI*f(10)) * den(505)) * TI2_call(4,momenta_5,masses2_6,G4tensor(:,5),T4sum(1:70,51))
  M(2) = M(2) + (CI*f(10) * den(505)) * TI2_call(4,momenta_5,masses2_6,G4tensor(:,5),T4sum(1:70,51))
  M(1) = M(1) + (-(CI*f(10)) * den(505)) * TI2_call(4,momenta_5,masses2_6,G4tensor(:,17),T4sum(1:70,51))
  M(2) = M(2) + (CI*f(10) * den(505)) * TI2_call(4,momenta_5,masses2_6,G4tensor(:,17),T4sum(1:70,51))
  M(1) = M(1) + (-(CI*f(14)) * den(506)) * TI2_call(4,momenta_5,masses2_6,G4tensor(:,6),T4sum(1:70,51))
  M(2) = M(2) + (CI*f(14) * den(506)) * TI2_call(4,momenta_5,masses2_6,G4tensor(:,6),T4sum(1:70,51))
  M(1) = M(1) + (-(CI*f(14)) * den(506)) * TI2_call(4,momenta_5,masses2_6,G4tensor(:,18),T4sum(1:70,51))
  M(2) = M(2) + (CI*f(14) * den(506)) * TI2_call(4,momenta_5,masses2_6,G4tensor(:,18),T4sum(1:70,51))
  M(1) = M(1) + (-(CI*f(10)) * den(505)) * TI2_call(4,momenta_8,masses2_6,G4tensor(:,19),T4sum(1:70,54))
  M(2) = M(2) + (CI*f(10) * den(505)) * TI2_call(4,momenta_8,masses2_6,G4tensor(:,19),T4sum(1:70,54))
  M(1) = M(1) + (-(CI*f(10)) * den(505)) * TI2_call(4,momenta_8,masses2_6,G4tensor(:,7),T4sum(1:70,54))
  M(2) = M(2) + (CI*f(10) * den(505)) * TI2_call(4,momenta_8,masses2_6,G4tensor(:,7),T4sum(1:70,54))
  M(1) = M(1) + (-(CI*f(14)) * den(506)) * TI2_call(4,momenta_8,masses2_6,G4tensor(:,20),T4sum(1:70,54))
  M(2) = M(2) + (CI*f(14) * den(506)) * TI2_call(4,momenta_8,masses2_6,G4tensor(:,20),T4sum(1:70,54))
  M(1) = M(1) + (-(CI*f(14)) * den(506)) * TI2_call(4,momenta_8,masses2_6,G4tensor(:,8),T4sum(1:70,54))
  M(2) = M(2) + (CI*f(14) * den(506)) * TI2_call(4,momenta_8,masses2_6,G4tensor(:,8),T4sum(1:70,54))
  M(1) = M(1) + (CI*f(10) * den(514)) * TI2_call(4,momenta_4,masses2_6,G4tensor(:,9),T4sum(1:70,60))
  M(2) = M(2) + (-(CI*f(10)) * den(514)) * TI2_call(4,momenta_4,masses2_6,G4tensor(:,9),T4sum(1:70,60))
  M(1) = M(1) + (CI*f(10) * den(514)) * TI2_call(4,momenta_4,masses2_6,G4tensor(:,21),T4sum(1:70,60))
  M(2) = M(2) + (-(CI*f(10)) * den(514)) * TI2_call(4,momenta_4,masses2_6,G4tensor(:,21),T4sum(1:70,60))
  M(1) = M(1) + (CI*f(14) * den(515)) * TI2_call(4,momenta_4,masses2_6,G4tensor(:,10),T4sum(1:70,60))
  M(2) = M(2) + (-(CI*f(14)) * den(515)) * TI2_call(4,momenta_4,masses2_6,G4tensor(:,10),T4sum(1:70,60))
  M(1) = M(1) + (CI*f(14) * den(515)) * TI2_call(4,momenta_4,masses2_6,G4tensor(:,22),T4sum(1:70,60))
  M(2) = M(2) + (-(CI*f(14)) * den(515)) * TI2_call(4,momenta_4,masses2_6,G4tensor(:,22),T4sum(1:70,60))
  M(1) = M(1) + (CI*f(10) * den(514)) * TI2_call(4,momenta_9,masses2_6,G4tensor(:,23),T4sum(1:70,63))
  M(2) = M(2) + (-(CI*f(10)) * den(514)) * TI2_call(4,momenta_9,masses2_6,G4tensor(:,23),T4sum(1:70,63))
  M(1) = M(1) + (CI*f(10) * den(514)) * TI2_call(4,momenta_9,masses2_6,G4tensor(:,11),T4sum(1:70,63))
  M(2) = M(2) + (-(CI*f(10)) * den(514)) * TI2_call(4,momenta_9,masses2_6,G4tensor(:,11),T4sum(1:70,63))
  M(1) = M(1) + (CI*f(14) * den(515)) * TI2_call(4,momenta_9,masses2_6,G4tensor(:,24),T4sum(1:70,63))
  M(2) = M(2) + (-(CI*f(14)) * den(515)) * TI2_call(4,momenta_9,masses2_6,G4tensor(:,24),T4sum(1:70,63))
  M(1) = M(1) + (CI*f(14) * den(515)) * TI2_call(4,momenta_9,masses2_6,G4tensor(:,12),T4sum(1:70,63))
  M(2) = M(2) + (-(CI*f(14)) * den(515)) * TI2_call(4,momenta_9,masses2_6,G4tensor(:,12),T4sum(1:70,63))
  M(1) = M(1) + (-f(19) * den(67)) * TI2_call(5,momenta_27,masses2_9,G5tensor(:,5),T5sum(1:126,39))
  M(2) = M(2) + (-f(19) * den(67)) * TI2_call(5,momenta_27,masses2_9,G5tensor(:,17),T5sum(1:126,39))
  M(1) = M(1) + (-f(23) * den(69)) * TI2_call(5,momenta_27,masses2_9,G5tensor(:,6),T5sum(1:126,39))
  M(2) = M(2) + (-f(23) * den(69)) * TI2_call(5,momenta_27,masses2_9,G5tensor(:,18),T5sum(1:126,39))
  M(1) = M(1) + (-f(19) * den(67)) * TI2_call(5,momenta_29,masses2_9,G5tensor(:,13),T5sum(1:126,42))
  M(2) = M(2) + (-f(19) * den(67)) * TI2_call(5,momenta_29,masses2_9,G5tensor(:,1),T5sum(1:126,42))
  M(1) = M(1) + (-f(23) * den(69)) * TI2_call(5,momenta_29,masses2_9,G5tensor(:,14),T5sum(1:126,42))
  M(2) = M(2) + (-f(23) * den(69)) * TI2_call(5,momenta_29,masses2_9,G5tensor(:,2),T5sum(1:126,42))
  M(1) = M(1) + (-f(19) * den(67)) * TI2_call(5,momenta_25,masses2_9,G5tensor(:,21),T5sum(1:126,45))
  M(2) = M(2) + (-f(19) * den(67)) * TI2_call(5,momenta_25,masses2_9,G5tensor(:,9),T5sum(1:126,45))
  M(1) = M(1) + (-f(23) * den(69)) * TI2_call(5,momenta_25,masses2_9,G5tensor(:,22),T5sum(1:126,45))
  M(2) = M(2) + (-f(23) * den(69)) * TI2_call(5,momenta_25,masses2_9,G5tensor(:,10),T5sum(1:126,45))
  M(1) = M(1) + (-f(19) * den(67)) * TI2_call(5,momenta_30,masses2_9,G5tensor(:,3),T5sum(1:126,51))
  M(2) = M(2) + (-f(19) * den(67)) * TI2_call(5,momenta_30,masses2_9,G5tensor(:,15),T5sum(1:126,51))
  M(1) = M(1) + (-f(23) * den(69)) * TI2_call(5,momenta_30,masses2_9,G5tensor(:,4),T5sum(1:126,51))
  M(2) = M(2) + (-f(23) * den(69)) * TI2_call(5,momenta_30,masses2_9,G5tensor(:,16),T5sum(1:126,51))
  M(2) = M(2) + (-f(19) * den(67)) * TI2_call(5,momenta_26,masses2_9,G5tensor(:,23),T5sum(1:126,57))
  M(1) = M(1) + (-f(19) * den(67)) * TI2_call(5,momenta_26,masses2_9,G5tensor(:,11),T5sum(1:126,57))
  M(2) = M(2) + (-f(23) * den(69)) * TI2_call(5,momenta_26,masses2_9,G5tensor(:,24),T5sum(1:126,57))
  M(1) = M(1) + (-f(23) * den(69)) * TI2_call(5,momenta_26,masses2_9,G5tensor(:,12),T5sum(1:126,57))
  M(2) = M(2) + (-f(19) * den(67)) * TI2_call(5,momenta_28,masses2_9,G5tensor(:,7),T5sum(1:126,63))
  M(1) = M(1) + (-f(19) * den(67)) * TI2_call(5,momenta_28,masses2_9,G5tensor(:,19),T5sum(1:126,63))
  M(2) = M(2) + (-f(23) * den(69)) * TI2_call(5,momenta_28,masses2_9,G5tensor(:,8),T5sum(1:126,63))
  M(1) = M(1) + (-f(23) * den(69)) * TI2_call(5,momenta_28,masses2_9,G5tensor(:,20),T5sum(1:126,63))

#ifdef LOOPSQUARED
#ifndef PRECISION_dp
  call gtdealloc()
#endif
#endif

end subroutine vamp_14

#ifdef LOOPSQUARED
subroutine gtdealloc()
  implicit none
  deallocate(G4tensorhel)
  deallocate(G5tensorhel)

end subroutine gtdealloc
#endif

end module ol_vamp_14_ppllllj2_eeexexggg_1_/**/REALKIND
