
module ol_vamp_3_ppllllj2_nmnmxeexggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), save, target, allocatable :: G3tensorhel(:,:,:)
  complex(REALKIND), save, target, allocatable :: G4tensorhel(:,:,:)
  complex(REALKIND), save, target, allocatable :: G5tensorhel(:,:,:)

  contains

! **********************************************************************
subroutine vamp_3(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppllllj2_nmnmxeexggg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppllllj2_nmnmxeexggg_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppllllj2_nmnmxeexggg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppllllj2_nmnmxeexggg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(inout) :: M(2)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,2) :: G0
  complex(REALKIND), dimension(4,5,4,7) :: G1
  complex(REALKIND), dimension(4,15,4,33) :: G2
  complex(REALKIND), dimension(4,35,4,52) :: G3
  complex(REALKIND), dimension(4,70,4,24) :: G4
  complex(REALKIND), pointer :: G3tensor(:,:)
  complex(REALKIND), pointer :: G4tensor(:,:)
  complex(REALKIND), pointer :: G5tensor(:,:)
#ifdef PRECISION_dp
  logical, save :: first = .true.
  if (first) then
#endif
    allocate(G3tensorhel(35,10,128))
    allocate(G4tensorhel(70,23,128))
    allocate(G5tensorhel(126,12,128))
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
  call loop_Q_A(G0(:,:,:,2),Q(:,16),MB,G1(:,:,:,1))
  call loop_QV_A(G1(:,:,:,1),wf(:,12),G1(:,:,:,2))
  call loop_Q_A(G1(:,:,:,2),Q(:,112),MB,G2(:,:,:,1))
  call loop_QZ_A(G2(:,:,:,1),wf(:,48),G2(:,:,:,2),gZd)
  call check_last_Q_A(l_switch,G2(:,:,:,2),Q(:,127),MB,G3tensor(:,1))
  call loop_QS_A(G2(:,:,:,1),wf(:,49),G2(:,:,:,3),gH)
  call check_last_Q_A(l_switch,G2(:,:,:,3),Q(:,127),MB,G3tensor(:,2))
  call loop_QS_A(G2(:,:,:,1),wf(:,55),G2(:,:,:,4),gH)
  call check_last_Q_A(l_switch,G2(:,:,:,4),Q(:,127),MB,G3tensor(:,3))
  call loop_QV_A(G2(:,:,:,1),wf(:,66),G2(:,:,:,5))
  call check_last_Q_A(l_switch,G2(:,:,:,5),Q(:,127),MB,G3tensor(:,4))
  call loop_QZ_A(G2(:,:,:,1),wf(:,67),G2(:,:,:,6),gZd)
  call check_last_Q_A(l_switch,G2(:,:,:,6),Q(:,127),MB,G3tensor(:,5))
  call loop_QS_A(G2(:,:,:,1),wf(:,59),G2(:,:,:,7),gH)
  call check_last_Q_A(l_switch,G2(:,:,:,7),Q(:,127),MB,G3tensor(:,6))
  call loop_QV_A(G2(:,:,:,1),wf(:,68),G2(:,:,:,8))
  call check_last_Q_A(l_switch,G2(:,:,:,8),Q(:,127),MB,G3tensor(:,7))
  call loop_QZ_A(G2(:,:,:,1),wf(:,69),G2(:,:,:,9),gZd)
  call check_last_Q_A(l_switch,G2(:,:,:,9),Q(:,127),MB,G3tensor(:,8))
  call loop_QZ_A(G2(:,:,:,1),wf(:,70),G2(:,:,:,10),gZd)
  call check_last_Q_A(l_switch,G2(:,:,:,10),Q(:,127),MB,G3tensor(:,9))
  call loop_QZ_A(G2(:,:,:,1),wf(:,71),G2(:,:,:,11),gZd)
  call check_last_Q_A(l_switch,G2(:,:,:,11),Q(:,127),MB,G3tensor(:,10))
  call loop_QS_A(G1(:,:,:,1),wf(:,15),G1(:,:,:,3),gH)
  call loop_Q_A(G1(:,:,:,3),Q(:,28),MB,G2(:,:,:,12))
  call loop_QV_A(G2(:,:,:,12),wf(:,12),G2(:,:,:,13))
  call loop_Q_A(G2(:,:,:,13),Q(:,124),MB,G3(:,:,:,1))
  call loop_QZ_A(G3(:,:,:,1),wf(:,4),G3(:,:,:,2),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,2),Q(:,127),MB,G4tensor(:,1))
  call loop_QV_A(G2(:,:,:,12),wf(:,-6),G2(:,:,:,14))
  call loop_Q_A(G2(:,:,:,14),Q(:,92),MB,G3(:,:,:,3))
  call loop_QV_A(G3(:,:,:,3),wf(:,-5),G3(:,:,:,4))
  call loop_Q_A(G3(:,:,:,4),Q(:,124),MB,G4(:,:,:,1))
  call loop_QZ_A(G4(:,:,:,1),wf(:,4),G4(:,:,:,2),gZd)
  call check_last_Q_A(l_switch,G4(:,:,:,2),Q(:,127),MB,G5tensor(:,1))
  call loop_QV_A(G2(:,:,:,12),wf(:,-5),G2(:,:,:,15))
  call loop_Q_A(G2(:,:,:,15),Q(:,60),MB,G3(:,:,:,5))
  call loop_QV_A(G3(:,:,:,5),wf(:,-6),G3(:,:,:,6))
  call loop_Q_A(G3(:,:,:,6),Q(:,124),MB,G4(:,:,:,3))
  call loop_QZ_A(G4(:,:,:,3),wf(:,4),G4(:,:,:,4),gZd)
  call check_last_Q_A(l_switch,G4(:,:,:,4),Q(:,127),MB,G5tensor(:,2))
  call loop_QV_A(G1(:,:,:,1),wf(:,2),G1(:,:,:,4))
  call loop_Q_A(G1(:,:,:,4),Q(:,28),MB,G2(:,:,:,16))
  call loop_QV_A(G2(:,:,:,16),wf(:,12),G2(:,:,:,17))
  call loop_Q_A(G2(:,:,:,17),Q(:,124),MB,G3(:,:,:,7))
  call loop_QZ_A(G3(:,:,:,7),wf(:,4),G3(:,:,:,8),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,8),Q(:,127),MB,G4tensor(:,2))
  call loop_QV_A(G2(:,:,:,16),wf(:,-6),G2(:,:,:,18))
  call loop_Q_A(G2(:,:,:,18),Q(:,92),MB,G3(:,:,:,9))
  call loop_QV_A(G3(:,:,:,9),wf(:,-5),G3(:,:,:,10))
  call loop_Q_A(G3(:,:,:,10),Q(:,124),MB,G4(:,:,:,5))
  call loop_QZ_A(G4(:,:,:,5),wf(:,4),G4(:,:,:,6),gZd)
  call check_last_Q_A(l_switch,G4(:,:,:,6),Q(:,127),MB,G5tensor(:,3))
  call loop_QV_A(G2(:,:,:,16),wf(:,-5),G2(:,:,:,19))
  call loop_Q_A(G2(:,:,:,19),Q(:,60),MB,G3(:,:,:,11))
  call loop_QV_A(G3(:,:,:,11),wf(:,-6),G3(:,:,:,12))
  call loop_Q_A(G3(:,:,:,12),Q(:,124),MB,G4(:,:,:,7))
  call loop_QZ_A(G4(:,:,:,7),wf(:,4),G4(:,:,:,8),gZd)
  call check_last_Q_A(l_switch,G4(:,:,:,8),Q(:,127),MB,G5tensor(:,4))
  call loop_QZ_A(G1(:,:,:,1),wf(:,7),G1(:,:,:,5),gZd)
  call loop_Q_A(G1(:,:,:,5),Q(:,28),MB,G2(:,:,:,20))
  call loop_QV_A(G2(:,:,:,20),wf(:,12),G2(:,:,:,21))
  call loop_Q_A(G2(:,:,:,21),Q(:,124),MB,G3(:,:,:,13))
  call loop_QZ_A(G3(:,:,:,13),wf(:,4),G3(:,:,:,14),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,14),Q(:,127),MB,G4tensor(:,3))
  call loop_QV_A(G2(:,:,:,20),wf(:,-6),G2(:,:,:,22))
  call loop_Q_A(G2(:,:,:,22),Q(:,92),MB,G3(:,:,:,15))
  call loop_QV_A(G3(:,:,:,15),wf(:,-5),G3(:,:,:,16))
  call loop_Q_A(G3(:,:,:,16),Q(:,124),MB,G4(:,:,:,9))
  call loop_QZ_A(G4(:,:,:,9),wf(:,4),G4(:,:,:,10),gZd)
  call check_last_Q_A(l_switch,G4(:,:,:,10),Q(:,127),MB,G5tensor(:,5))
  call loop_QV_A(G2(:,:,:,20),wf(:,-5),G2(:,:,:,23))
  call loop_Q_A(G2(:,:,:,23),Q(:,60),MB,G3(:,:,:,17))
  call loop_QV_A(G3(:,:,:,17),wf(:,-6),G3(:,:,:,18))
  call loop_Q_A(G3(:,:,:,18),Q(:,124),MB,G4(:,:,:,11))
  call loop_QZ_A(G4(:,:,:,11),wf(:,4),G4(:,:,:,12),gZd)
  call check_last_Q_A(l_switch,G4(:,:,:,12),Q(:,127),MB,G5tensor(:,6))
  call loop_QV_A(G1(:,:,:,1),wf(:,-5),G1(:,:,:,6))
  call loop_Q_A(G1(:,:,:,6),Q(:,48),MB,G2(:,:,:,24))
  call loop_QV_A(G2(:,:,:,24),wf(:,-6),G2(:,:,:,25))
  call loop_Q_A(G2(:,:,:,25),Q(:,112),MB,G3(:,:,:,19))
  call loop_QZ_A(G3(:,:,:,19),wf(:,48),G3(:,:,:,20),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,20),Q(:,127),MB,G4tensor(:,4))
  call loop_QS_A(G3(:,:,:,19),wf(:,49),G3(:,:,:,21),gH)
  call check_last_Q_A(l_switch,G3(:,:,:,21),Q(:,127),MB,G4tensor(:,5))
  call loop_QS_A(G3(:,:,:,19),wf(:,55),G3(:,:,:,22),gH)
  call check_last_Q_A(l_switch,G3(:,:,:,22),Q(:,127),MB,G4tensor(:,6))
  call loop_QV_A(G3(:,:,:,19),wf(:,66),G3(:,:,:,23))
  call check_last_Q_A(l_switch,G3(:,:,:,23),Q(:,127),MB,G4tensor(:,7))
  call loop_QZ_A(G3(:,:,:,19),wf(:,67),G3(:,:,:,24),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,24),Q(:,127),MB,G4tensor(:,8))
  call loop_QS_A(G3(:,:,:,19),wf(:,59),G3(:,:,:,25),gH)
  call check_last_Q_A(l_switch,G3(:,:,:,25),Q(:,127),MB,G4tensor(:,9))
  call loop_QV_A(G3(:,:,:,19),wf(:,68),G3(:,:,:,26))
  call check_last_Q_A(l_switch,G3(:,:,:,26),Q(:,127),MB,G4tensor(:,10))
  call loop_QZ_A(G3(:,:,:,19),wf(:,69),G3(:,:,:,27),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,27),Q(:,127),MB,G4tensor(:,11))
  call loop_QZ_A(G3(:,:,:,19),wf(:,70),G3(:,:,:,28),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,28),Q(:,127),MB,G4tensor(:,12))
  call loop_QZ_A(G3(:,:,:,19),wf(:,71),G3(:,:,:,29),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,29),Q(:,127),MB,G4tensor(:,13))
  call loop_QS_A(G2(:,:,:,24),wf(:,15),G2(:,:,:,26),gH)
  call loop_Q_A(G2(:,:,:,26),Q(:,60),MB,G3(:,:,:,30))
  call loop_QV_A(G3(:,:,:,30),wf(:,-6),G3(:,:,:,31))
  call loop_Q_A(G3(:,:,:,31),Q(:,124),MB,G4(:,:,:,13))
  call loop_QZ_A(G4(:,:,:,13),wf(:,4),G4(:,:,:,14),gZd)
  call check_last_Q_A(l_switch,G4(:,:,:,14),Q(:,127),MB,G5tensor(:,7))
  call loop_QV_A(G2(:,:,:,24),wf(:,2),G2(:,:,:,27))
  call loop_Q_A(G2(:,:,:,27),Q(:,60),MB,G3(:,:,:,32))
  call loop_QV_A(G3(:,:,:,32),wf(:,-6),G3(:,:,:,33))
  call loop_Q_A(G3(:,:,:,33),Q(:,124),MB,G4(:,:,:,15))
  call loop_QZ_A(G4(:,:,:,15),wf(:,4),G4(:,:,:,16),gZd)
  call check_last_Q_A(l_switch,G4(:,:,:,16),Q(:,127),MB,G5tensor(:,8))
  call loop_QZ_A(G2(:,:,:,24),wf(:,7),G2(:,:,:,28),gZd)
  call loop_Q_A(G2(:,:,:,28),Q(:,60),MB,G3(:,:,:,34))
  call loop_QV_A(G3(:,:,:,34),wf(:,-6),G3(:,:,:,35))
  call loop_Q_A(G3(:,:,:,35),Q(:,124),MB,G4(:,:,:,17))
  call loop_QZ_A(G4(:,:,:,17),wf(:,4),G4(:,:,:,18),gZd)
  call check_last_Q_A(l_switch,G4(:,:,:,18),Q(:,127),MB,G5tensor(:,9))
  call loop_QV_A(G1(:,:,:,1),wf(:,-6),G1(:,:,:,7))
  call loop_Q_A(G1(:,:,:,7),Q(:,80),MB,G2(:,:,:,29))
  call loop_QV_A(G2(:,:,:,29),wf(:,-5),G2(:,:,:,30))
  call loop_Q_A(G2(:,:,:,30),Q(:,112),MB,G3(:,:,:,36))
  call loop_QZ_A(G3(:,:,:,36),wf(:,48),G3(:,:,:,37),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,37),Q(:,127),MB,G4tensor(:,14))
  call loop_QS_A(G3(:,:,:,36),wf(:,49),G3(:,:,:,38),gH)
  call check_last_Q_A(l_switch,G3(:,:,:,38),Q(:,127),MB,G4tensor(:,15))
  call loop_QS_A(G3(:,:,:,36),wf(:,55),G3(:,:,:,39),gH)
  call check_last_Q_A(l_switch,G3(:,:,:,39),Q(:,127),MB,G4tensor(:,16))
  call loop_QV_A(G3(:,:,:,36),wf(:,66),G3(:,:,:,40))
  call check_last_Q_A(l_switch,G3(:,:,:,40),Q(:,127),MB,G4tensor(:,17))
  call loop_QZ_A(G3(:,:,:,36),wf(:,67),G3(:,:,:,41),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,41),Q(:,127),MB,G4tensor(:,18))
  call loop_QS_A(G3(:,:,:,36),wf(:,59),G3(:,:,:,42),gH)
  call check_last_Q_A(l_switch,G3(:,:,:,42),Q(:,127),MB,G4tensor(:,19))
  call loop_QV_A(G3(:,:,:,36),wf(:,68),G3(:,:,:,43))
  call check_last_Q_A(l_switch,G3(:,:,:,43),Q(:,127),MB,G4tensor(:,20))
  call loop_QZ_A(G3(:,:,:,36),wf(:,69),G3(:,:,:,44),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,44),Q(:,127),MB,G4tensor(:,21))
  call loop_QZ_A(G3(:,:,:,36),wf(:,70),G3(:,:,:,45),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,45),Q(:,127),MB,G4tensor(:,22))
  call loop_QZ_A(G3(:,:,:,36),wf(:,71),G3(:,:,:,46),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,46),Q(:,127),MB,G4tensor(:,23))
  call loop_QS_A(G2(:,:,:,29),wf(:,15),G2(:,:,:,31),gH)
  call loop_Q_A(G2(:,:,:,31),Q(:,92),MB,G3(:,:,:,47))
  call loop_QV_A(G3(:,:,:,47),wf(:,-5),G3(:,:,:,48))
  call loop_Q_A(G3(:,:,:,48),Q(:,124),MB,G4(:,:,:,19))
  call loop_QZ_A(G4(:,:,:,19),wf(:,4),G4(:,:,:,20),gZd)
  call check_last_Q_A(l_switch,G4(:,:,:,20),Q(:,127),MB,G5tensor(:,10))
  call loop_QV_A(G2(:,:,:,29),wf(:,2),G2(:,:,:,32))
  call loop_Q_A(G2(:,:,:,32),Q(:,92),MB,G3(:,:,:,49))
  call loop_QV_A(G3(:,:,:,49),wf(:,-5),G3(:,:,:,50))
  call loop_Q_A(G3(:,:,:,50),Q(:,124),MB,G4(:,:,:,21))
  call loop_QZ_A(G4(:,:,:,21),wf(:,4),G4(:,:,:,22),gZd)
  call check_last_Q_A(l_switch,G4(:,:,:,22),Q(:,127),MB,G5tensor(:,11))
  call loop_QZ_A(G2(:,:,:,29),wf(:,7),G2(:,:,:,33),gZd)
  call loop_Q_A(G2(:,:,:,33),Q(:,92),MB,G3(:,:,:,51))
  call loop_QV_A(G3(:,:,:,51),wf(:,-5),G3(:,:,:,52))
  call loop_Q_A(G3(:,:,:,52),Q(:,124),MB,G4(:,:,:,23))
  call loop_QZ_A(G4(:,:,:,23),wf(:,4),G4(:,:,:,24),gZd)
  call check_last_Q_A(l_switch,G4(:,:,:,24),Q(:,127),MB,G5tensor(:,12))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  M(1) = M(1) + (-(CI*f(20)) * den(43)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,1),T3sum(1:35,9))
  M(2) = M(2) + (CI*f(20) * den(43)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,1),T3sum(1:35,9))
  M(1) = M(1) + (-(CI*f(16)) * den(44)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,2),T3sum(1:35,9))
  M(2) = M(2) + (CI*f(16) * den(44)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,2),T3sum(1:35,9))
  M(1) = M(1) + (-(CI*f(25)) * den(89)) * TI2_call(4,momenta_10,masses2_4,G4tensor(:,1),T4sum(1:70,20))
  M(2) = M(2) + (CI*f(25) * den(89)) * TI2_call(4,momenta_10,masses2_4,G4tensor(:,1),T4sum(1:70,20))
  M(1) = M(1) + (CI*f(5) * den(90)) * TI2_call(4,momenta_10,masses2_4,G4tensor(:,2),T4sum(1:70,20))
  M(2) = M(2) + (-(CI*f(5)) * den(90)) * TI2_call(4,momenta_10,masses2_4,G4tensor(:,2),T4sum(1:70,20))
  M(1) = M(1) + (CI*f(7) * den(91)) * TI2_call(4,momenta_10,masses2_4,G4tensor(:,3),T4sum(1:70,20))
  M(2) = M(2) + (-(CI*f(7)) * den(91)) * TI2_call(4,momenta_10,masses2_4,G4tensor(:,3),T4sum(1:70,20))
  M(2) = M(2) + (f(22) * den(36)) * TI2_call(4,momenta_11,masses2_4,G4tensor(:,4),T4sum(1:70,33))
  M(2) = M(2) + (f(17) * den(39)) * TI2_call(4,momenta_11,masses2_4,G4tensor(:,5),T4sum(1:70,33))
  M(1) = M(1) + (f(22) * den(36)) * TI2_call(4,momenta_12,masses2_4,G4tensor(:,14),T4sum(1:70,36))
  M(1) = M(1) + (f(17) * den(39)) * TI2_call(4,momenta_12,masses2_4,G4tensor(:,15),T4sum(1:70,36))
  M(2) = M(2) + (f(26) * den(17)) * TI2_call(5,momenta_24,masses2_7,G5tensor(:,7),T5sum(1:126,11))
  M(2) = M(2) + (-f(10) * den(4)) * TI2_call(5,momenta_24,masses2_7,G5tensor(:,8),T5sum(1:126,11))
  M(2) = M(2) + (-f(12) * den(7)) * TI2_call(5,momenta_24,masses2_7,G5tensor(:,9),T5sum(1:126,11))
  M(1) = M(1) + (f(26) * den(17)) * TI2_call(5,momenta_25,masses2_7,G5tensor(:,10),T5sum(1:126,23))
  M(1) = M(1) + (-f(10) * den(4)) * TI2_call(5,momenta_25,masses2_7,G5tensor(:,11),T5sum(1:126,23))
  M(1) = M(1) + (-f(12) * den(7)) * TI2_call(5,momenta_25,masses2_7,G5tensor(:,12),T5sum(1:126,23))
  M(1) = M(1) + (f(26) * den(17)) * TI2_call(5,momenta_23,masses2_7,G5tensor(:,1),T5sum(1:126,32))
  M(1) = M(1) + (-f(10) * den(4)) * TI2_call(5,momenta_23,masses2_7,G5tensor(:,3),T5sum(1:126,32))
  M(1) = M(1) + (-f(12) * den(7)) * TI2_call(5,momenta_23,masses2_7,G5tensor(:,5),T5sum(1:126,32))
  M(2) = M(2) + (f(26) * den(17)) * TI2_call(5,momenta_22,masses2_7,G5tensor(:,2),T5sum(1:126,35))
  M(2) = M(2) + (-f(10) * den(4)) * TI2_call(5,momenta_22,masses2_7,G5tensor(:,4),T5sum(1:126,35))
  M(2) = M(2) + (-f(12) * den(7)) * TI2_call(5,momenta_22,masses2_7,G5tensor(:,6),T5sum(1:126,35))
  M(1) = M(1) + (-(CI*f(25)) * den(104)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,3),T3sum(1:35,9))
  M(2) = M(2) + (CI*f(25) * den(104)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,3),T3sum(1:35,9))
  M(1) = M(1) + (CI*f(5) * den(105)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,4),T3sum(1:35,9))
  M(2) = M(2) + (-(CI*f(5)) * den(105)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,4),T3sum(1:35,9))
  M(1) = M(1) + (CI*f(7) * den(106)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,5),T3sum(1:35,9))
  M(2) = M(2) + (-(CI*f(7)) * den(106)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,5),T3sum(1:35,9))
  M(1) = M(1) + (-(CI*f(25)) * den(107)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,6),T3sum(1:35,9))
  M(2) = M(2) + (CI*f(25) * den(107)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,6),T3sum(1:35,9))
  M(1) = M(1) + (CI*f(5) * den(108)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,7),T3sum(1:35,9))
  M(2) = M(2) + (-(CI*f(5)) * den(108)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,7),T3sum(1:35,9))
  M(1) = M(1) + (CI*f(7) * den(109)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,8),T3sum(1:35,9))
  M(2) = M(2) + (-(CI*f(7)) * den(109)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,8),T3sum(1:35,9))
  M(2) = M(2) + (f(26) * den(74)) * TI2_call(4,momenta_11,masses2_4,G4tensor(:,6),T4sum(1:70,33))
  M(2) = M(2) + (-f(10) * den(76)) * TI2_call(4,momenta_11,masses2_4,G4tensor(:,7),T4sum(1:70,33))
  M(2) = M(2) + (-f(12) * den(77)) * TI2_call(4,momenta_11,masses2_4,G4tensor(:,8),T4sum(1:70,33))
  M(1) = M(1) + (f(26) * den(74)) * TI2_call(4,momenta_12,masses2_4,G4tensor(:,16),T4sum(1:70,36))
  M(1) = M(1) + (-f(10) * den(76)) * TI2_call(4,momenta_12,masses2_4,G4tensor(:,17),T4sum(1:70,36))
  M(1) = M(1) + (-f(12) * den(77)) * TI2_call(4,momenta_12,masses2_4,G4tensor(:,18),T4sum(1:70,36))
  M(2) = M(2) + (f(26) * den(78)) * TI2_call(4,momenta_11,masses2_4,G4tensor(:,9),T4sum(1:70,33))
  M(2) = M(2) + (-f(10) * den(79)) * TI2_call(4,momenta_11,masses2_4,G4tensor(:,10),T4sum(1:70,33))
  M(2) = M(2) + (-f(12) * den(80)) * TI2_call(4,momenta_11,masses2_4,G4tensor(:,11),T4sum(1:70,33))
  M(1) = M(1) + (f(26) * den(78)) * TI2_call(4,momenta_12,masses2_4,G4tensor(:,19),T4sum(1:70,36))
  M(1) = M(1) + (-f(10) * den(79)) * TI2_call(4,momenta_12,masses2_4,G4tensor(:,20),T4sum(1:70,36))
  M(1) = M(1) + (-f(12) * den(80)) * TI2_call(4,momenta_12,masses2_4,G4tensor(:,21),T4sum(1:70,36))
  M(1) = M(1) + (CI*f(7) * den(114)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,9),T3sum(1:35,9))
  M(2) = M(2) + (-(CI*f(7)) * den(114)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,9),T3sum(1:35,9))
  M(1) = M(1) + (CI*f(7) * den(115)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,10),T3sum(1:35,9))
  M(2) = M(2) + (-(CI*f(7)) * den(115)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,10),T3sum(1:35,9))
  M(2) = M(2) + (-f(12) * den(81)) * TI2_call(4,momenta_11,masses2_4,G4tensor(:,12),T4sum(1:70,33))
  M(1) = M(1) + (-f(12) * den(81)) * TI2_call(4,momenta_12,masses2_4,G4tensor(:,22),T4sum(1:70,36))
  M(2) = M(2) + (-f(12) * den(82)) * TI2_call(4,momenta_11,masses2_4,G4tensor(:,13),T4sum(1:70,33))
  M(1) = M(1) + (-f(12) * den(82)) * TI2_call(4,momenta_12,masses2_4,G4tensor(:,23),T4sum(1:70,36))

#ifdef LOOPSQUARED
#ifndef PRECISION_dp
  call gtdealloc()
#endif
#endif

end subroutine vamp_3

#ifdef LOOPSQUARED
subroutine gtdealloc()
  implicit none
  deallocate(G3tensorhel)
  deallocate(G4tensorhel)
  deallocate(G5tensorhel)

end subroutine gtdealloc
#endif

end module ol_vamp_3_ppllllj2_nmnmxeexggg_1_/**/REALKIND
