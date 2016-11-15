
module ol_vamp_1_pphllj2_nenexuuxhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), save, target, allocatable :: G3tensorhel(:,:,:)
  complex(REALKIND), save, target, allocatable :: G4tensorhel(:,:,:)

  contains

! **********************************************************************
subroutine vamp_1(M, mode, hel)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_pphllj2_nenexuuxhg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_pphllj2_nenexuuxhg_1.
! **********************************************************************
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/DREALKIND, only: stability_mode
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_pphllj2_nenexuuxhg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_pphllj2_nenexuuxhg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(inout) :: M(1)
  integer, intent(in) :: mode, hel ! recycle mode & hel conf number

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,17) :: G0
  complex(REALKIND), dimension(4,5,4,40) :: G1
  complex(REALKIND), dimension(4,15,4,52) :: G2
  complex(REALKIND), dimension(4,35,4,24) :: G3
  complex(REALKIND), pointer :: G3tensor(:,:)
  complex(REALKIND), pointer :: G4tensor(:,:)
#ifdef PRECISION_dp
  logical, save :: first = .true.
  if (first) then
#endif
    allocate(G3tensorhel(35,16,32))
    allocate(G4tensorhel(70,12,32))
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
  call loop_AZ_Q(G0(:,:,:,1),wf(:,6),G0(:,:,:,2),gZu)
  call loop_A_Q(G0(:,:,:,2),Q(:,19),ZERO,G1(:,:,:,1))
  call loop_AV_Q(G1(:,:,:,1),wf(:,-5),G1(:,:,:,2))
  call loop_A_Q(G1(:,:,:,2),Q(:,51),ZERO,G2(:,:,:,1))
  call loop_AV_Q(G2(:,:,:,1),wf(:,2),G2(:,:,:,2))
  call check_last_A_Q(l_switch,G2(:,:,:,2),Q(:,63),ZERO,G3tensor(:,1))
  call loop_AZ_Q(G0(:,:,:,1),wf(:,6),G0(:,:,:,3),gZu)
  call loop_A_Q(G0(:,:,:,3),Q(:,19),MT,G1(:,:,:,3))
  call loop_AV_Q(G1(:,:,:,3),wf(:,-5),G1(:,:,:,4))
  call loop_A_Q(G1(:,:,:,4),Q(:,51),MT,G2(:,:,:,3))
  call loop_AV_Q(G2(:,:,:,3),wf(:,2),G2(:,:,:,4))
  call check_last_A_Q(l_switch,G2(:,:,:,4),Q(:,63),MT,G3tensor(:,2))
  call loop_QZ_A(G0(:,:,:,1),wf(:,6),G0(:,:,:,4),gZu)
  call loop_Q_A(G0(:,:,:,4),Q(:,19),ZERO,G1(:,:,:,5))
  call loop_QV_A(G1(:,:,:,5),wf(:,-5),G1(:,:,:,6))
  call loop_Q_A(G1(:,:,:,6),Q(:,51),ZERO,G2(:,:,:,5))
  call loop_QV_A(G2(:,:,:,5),wf(:,2),G2(:,:,:,6))
  call check_last_Q_A(l_switch,G2(:,:,:,6),Q(:,63),ZERO,G3tensor(:,3))
  call loop_QZ_A(G0(:,:,:,1),wf(:,6),G0(:,:,:,5),gZu)
  call loop_Q_A(G0(:,:,:,5),Q(:,19),MT,G1(:,:,:,7))
  call loop_QV_A(G1(:,:,:,7),wf(:,-5),G1(:,:,:,8))
  call loop_Q_A(G1(:,:,:,8),Q(:,51),MT,G2(:,:,:,7))
  call loop_QV_A(G2(:,:,:,7),wf(:,2),G2(:,:,:,8))
  call check_last_Q_A(l_switch,G2(:,:,:,8),Q(:,63),MT,G3tensor(:,4))
  call loop_AZ_Q(G0(:,:,:,1),wf(:,6),G0(:,:,:,6),gZd)
  call loop_A_Q(G0(:,:,:,6),Q(:,19),ZERO,G1(:,:,:,9))
  call loop_AV_Q(G1(:,:,:,9),wf(:,-5),G1(:,:,:,10))
  call loop_A_Q(G1(:,:,:,10),Q(:,51),ZERO,G2(:,:,:,9))
  call loop_AV_Q(G2(:,:,:,9),wf(:,2),G2(:,:,:,10))
  call check_last_A_Q(l_switch,G2(:,:,:,10),Q(:,63),ZERO,G3tensor(:,5))
  call loop_AZ_Q(G0(:,:,:,1),wf(:,6),G0(:,:,:,7),gZd)
  call loop_A_Q(G0(:,:,:,7),Q(:,19),MB,G1(:,:,:,11))
  call loop_AV_Q(G1(:,:,:,11),wf(:,-5),G1(:,:,:,12))
  call loop_A_Q(G1(:,:,:,12),Q(:,51),MB,G2(:,:,:,11))
  call loop_AV_Q(G2(:,:,:,11),wf(:,2),G2(:,:,:,12))
  call check_last_A_Q(l_switch,G2(:,:,:,12),Q(:,63),MB,G3tensor(:,6))
  call loop_QZ_A(G0(:,:,:,1),wf(:,6),G0(:,:,:,8),gZd)
  call loop_Q_A(G0(:,:,:,8),Q(:,19),ZERO,G1(:,:,:,13))
  call loop_QV_A(G1(:,:,:,13),wf(:,-5),G1(:,:,:,14))
  call loop_Q_A(G1(:,:,:,14),Q(:,51),ZERO,G2(:,:,:,13))
  call loop_QV_A(G2(:,:,:,13),wf(:,2),G2(:,:,:,14))
  call check_last_Q_A(l_switch,G2(:,:,:,14),Q(:,63),ZERO,G3tensor(:,7))
  call loop_QZ_A(G0(:,:,:,1),wf(:,6),G0(:,:,:,9),gZd)
  call loop_Q_A(G0(:,:,:,9),Q(:,19),MB,G1(:,:,:,15))
  call loop_QV_A(G1(:,:,:,15),wf(:,-5),G1(:,:,:,16))
  call loop_Q_A(G1(:,:,:,16),Q(:,51),MB,G2(:,:,:,15))
  call loop_QV_A(G2(:,:,:,15),wf(:,2),G2(:,:,:,16))
  call check_last_Q_A(l_switch,G2(:,:,:,16),Q(:,63),MB,G3tensor(:,8))
  call loop_QV_A(G0(:,:,:,1),wf(:,2),G0(:,:,:,10))
  call loop_Q_A(G0(:,:,:,10),Q(:,12),MT,G1(:,:,:,17))
  call loop_QV_A(G1(:,:,:,17),wf(:,-5),G1(:,:,:,18))
  call loop_Q_A(G1(:,:,:,18),Q(:,44),MT,G2(:,:,:,17))
  call loop_QS_A(G2(:,:,:,17),wf(:,-4),G2(:,:,:,18),gH)
  call loop_Q_A(G2(:,:,:,18),Q(:,60),MT,G3(:,:,:,1))
  call loop_QZ_A(G3(:,:,:,1),wf(:,3),G3(:,:,:,2),gZu)
  call check_last_Q_A(l_switch,G3(:,:,:,2),Q(:,63),MT,G4tensor(:,1))
  call loop_QS_A(G1(:,:,:,17),wf(:,-4),G1(:,:,:,19),gH)
  call loop_Q_A(G1(:,:,:,19),Q(:,28),MT,G2(:,:,:,19))
  call loop_QV_A(G2(:,:,:,19),wf(:,-5),G2(:,:,:,20))
  call loop_Q_A(G2(:,:,:,20),Q(:,60),MT,G3(:,:,:,3))
  call loop_QZ_A(G3(:,:,:,3),wf(:,3),G3(:,:,:,4),gZu)
  call check_last_Q_A(l_switch,G3(:,:,:,4),Q(:,63),MT,G4tensor(:,2))
  call loop_AV_Q(G0(:,:,:,1),wf(:,2),G0(:,:,:,11))
  call loop_A_Q(G0(:,:,:,11),Q(:,12),MT,G1(:,:,:,20))
  call loop_AV_Q(G1(:,:,:,20),wf(:,-5),G1(:,:,:,21))
  call loop_A_Q(G1(:,:,:,21),Q(:,44),MT,G2(:,:,:,21))
  call loop_AS_Q(G2(:,:,:,21),wf(:,-4),G2(:,:,:,22),gH)
  call loop_A_Q(G2(:,:,:,22),Q(:,60),MT,G3(:,:,:,5))
  call loop_AZ_Q(G3(:,:,:,5),wf(:,3),G3(:,:,:,6),gZu)
  call check_last_A_Q(l_switch,G3(:,:,:,6),Q(:,63),MT,G4tensor(:,3))
  call loop_AS_Q(G1(:,:,:,20),wf(:,-4),G1(:,:,:,22),gH)
  call loop_A_Q(G1(:,:,:,22),Q(:,28),MT,G2(:,:,:,23))
  call loop_AV_Q(G2(:,:,:,23),wf(:,-5),G2(:,:,:,24))
  call loop_A_Q(G2(:,:,:,24),Q(:,60),MT,G3(:,:,:,7))
  call loop_AZ_Q(G3(:,:,:,7),wf(:,3),G3(:,:,:,8),gZu)
  call check_last_A_Q(l_switch,G3(:,:,:,8),Q(:,63),MT,G4tensor(:,4))
  call loop_QV_A(G0(:,:,:,1),wf(:,2),G0(:,:,:,12))
  call loop_Q_A(G0(:,:,:,12),Q(:,12),MB,G1(:,:,:,23))
  call loop_QV_A(G1(:,:,:,23),wf(:,-5),G1(:,:,:,24))
  call loop_Q_A(G1(:,:,:,24),Q(:,44),MB,G2(:,:,:,25))
  call loop_QS_A(G2(:,:,:,25),wf(:,-4),G2(:,:,:,26),gH)
  call loop_Q_A(G2(:,:,:,26),Q(:,60),MB,G3(:,:,:,9))
  call loop_QZ_A(G3(:,:,:,9),wf(:,3),G3(:,:,:,10),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,10),Q(:,63),MB,G4tensor(:,5))
  call loop_QS_A(G1(:,:,:,23),wf(:,-4),G1(:,:,:,25),gH)
  call loop_Q_A(G1(:,:,:,25),Q(:,28),MB,G2(:,:,:,27))
  call loop_QV_A(G2(:,:,:,27),wf(:,-5),G2(:,:,:,28))
  call loop_Q_A(G2(:,:,:,28),Q(:,60),MB,G3(:,:,:,11))
  call loop_QZ_A(G3(:,:,:,11),wf(:,3),G3(:,:,:,12),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,12),Q(:,63),MB,G4tensor(:,6))
  call loop_AV_Q(G0(:,:,:,1),wf(:,2),G0(:,:,:,13))
  call loop_A_Q(G0(:,:,:,13),Q(:,12),MB,G1(:,:,:,26))
  call loop_AV_Q(G1(:,:,:,26),wf(:,-5),G1(:,:,:,27))
  call loop_A_Q(G1(:,:,:,27),Q(:,44),MB,G2(:,:,:,29))
  call loop_AS_Q(G2(:,:,:,29),wf(:,-4),G2(:,:,:,30),gH)
  call loop_A_Q(G2(:,:,:,30),Q(:,60),MB,G3(:,:,:,13))
  call loop_AZ_Q(G3(:,:,:,13),wf(:,3),G3(:,:,:,14),gZd)
  call check_last_A_Q(l_switch,G3(:,:,:,14),Q(:,63),MB,G4tensor(:,7))
  call loop_AS_Q(G1(:,:,:,26),wf(:,-4),G1(:,:,:,28),gH)
  call loop_A_Q(G1(:,:,:,28),Q(:,28),MB,G2(:,:,:,31))
  call loop_AV_Q(G2(:,:,:,31),wf(:,-5),G2(:,:,:,32))
  call loop_A_Q(G2(:,:,:,32),Q(:,60),MB,G3(:,:,:,15))
  call loop_AZ_Q(G3(:,:,:,15),wf(:,3),G3(:,:,:,16),gZd)
  call check_last_A_Q(l_switch,G3(:,:,:,16),Q(:,63),MB,G4tensor(:,8))
  call loop_QS_A(G0(:,:,:,1),wf(:,-4),G0(:,:,:,14),gH)
  call loop_Q_A(G0(:,:,:,14),Q(:,16),MT,G1(:,:,:,29))
  call loop_QV_A(G1(:,:,:,29),wf(:,2),G1(:,:,:,30))
  call loop_Q_A(G1(:,:,:,30),Q(:,28),MT,G2(:,:,:,33))
  call loop_QV_A(G2(:,:,:,33),wf(:,-5),G2(:,:,:,34))
  call loop_Q_A(G2(:,:,:,34),Q(:,60),MT,G3(:,:,:,17))
  call loop_QZ_A(G3(:,:,:,17),wf(:,3),G3(:,:,:,18),gZu)
  call check_last_Q_A(l_switch,G3(:,:,:,18),Q(:,63),MT,G4tensor(:,9))
  call loop_QV_A(G1(:,:,:,29),wf(:,-5),G1(:,:,:,31))
  call loop_Q_A(G1(:,:,:,31),Q(:,48),MT,G2(:,:,:,35))
  call loop_QV_A(G2(:,:,:,35),wf(:,14),G2(:,:,:,36))
  call check_last_Q_A(l_switch,G2(:,:,:,36),Q(:,63),MT,G3tensor(:,9))
  call loop_QV_A(G2(:,:,:,35),wf(:,16),G2(:,:,:,37))
  call check_last_Q_A(l_switch,G2(:,:,:,37),Q(:,63),MT,G3tensor(:,10))
  call loop_AS_Q(G0(:,:,:,1),wf(:,-4),G0(:,:,:,15),gH)
  call loop_A_Q(G0(:,:,:,15),Q(:,16),MT,G1(:,:,:,32))
  call loop_AV_Q(G1(:,:,:,32),wf(:,2),G1(:,:,:,33))
  call loop_A_Q(G1(:,:,:,33),Q(:,28),MT,G2(:,:,:,38))
  call loop_AV_Q(G2(:,:,:,38),wf(:,-5),G2(:,:,:,39))
  call loop_A_Q(G2(:,:,:,39),Q(:,60),MT,G3(:,:,:,19))
  call loop_AZ_Q(G3(:,:,:,19),wf(:,3),G3(:,:,:,20),gZu)
  call check_last_A_Q(l_switch,G3(:,:,:,20),Q(:,63),MT,G4tensor(:,10))
  call loop_AV_Q(G1(:,:,:,32),wf(:,-5),G1(:,:,:,34))
  call loop_A_Q(G1(:,:,:,34),Q(:,48),MT,G2(:,:,:,40))
  call loop_AV_Q(G2(:,:,:,40),wf(:,14),G2(:,:,:,41))
  call check_last_A_Q(l_switch,G2(:,:,:,41),Q(:,63),MT,G3tensor(:,11))
  call loop_AV_Q(G2(:,:,:,40),wf(:,16),G2(:,:,:,42))
  call check_last_A_Q(l_switch,G2(:,:,:,42),Q(:,63),MT,G3tensor(:,12))
  call loop_QS_A(G0(:,:,:,1),wf(:,-4),G0(:,:,:,16),gH)
  call loop_Q_A(G0(:,:,:,16),Q(:,16),MB,G1(:,:,:,35))
  call loop_QV_A(G1(:,:,:,35),wf(:,2),G1(:,:,:,36))
  call loop_Q_A(G1(:,:,:,36),Q(:,28),MB,G2(:,:,:,43))
  call loop_QV_A(G2(:,:,:,43),wf(:,-5),G2(:,:,:,44))
  call loop_Q_A(G2(:,:,:,44),Q(:,60),MB,G3(:,:,:,21))
  call loop_QZ_A(G3(:,:,:,21),wf(:,3),G3(:,:,:,22),gZd)
  call check_last_Q_A(l_switch,G3(:,:,:,22),Q(:,63),MB,G4tensor(:,11))
  call loop_QV_A(G1(:,:,:,35),wf(:,-5),G1(:,:,:,37))
  call loop_Q_A(G1(:,:,:,37),Q(:,48),MB,G2(:,:,:,45))
  call loop_QV_A(G2(:,:,:,45),wf(:,14),G2(:,:,:,46))
  call check_last_Q_A(l_switch,G2(:,:,:,46),Q(:,63),MB,G3tensor(:,13))
  call loop_QV_A(G2(:,:,:,45),wf(:,16),G2(:,:,:,47))
  call check_last_Q_A(l_switch,G2(:,:,:,47),Q(:,63),MB,G3tensor(:,14))
  call loop_AS_Q(G0(:,:,:,1),wf(:,-4),G0(:,:,:,17),gH)
  call loop_A_Q(G0(:,:,:,17),Q(:,16),MB,G1(:,:,:,38))
  call loop_AV_Q(G1(:,:,:,38),wf(:,2),G1(:,:,:,39))
  call loop_A_Q(G1(:,:,:,39),Q(:,28),MB,G2(:,:,:,48))
  call loop_AV_Q(G2(:,:,:,48),wf(:,-5),G2(:,:,:,49))
  call loop_A_Q(G2(:,:,:,49),Q(:,60),MB,G3(:,:,:,23))
  call loop_AZ_Q(G3(:,:,:,23),wf(:,3),G3(:,:,:,24),gZd)
  call check_last_A_Q(l_switch,G3(:,:,:,24),Q(:,63),MB,G4tensor(:,12))
  call loop_AV_Q(G1(:,:,:,38),wf(:,-5),G1(:,:,:,40))
  call loop_A_Q(G1(:,:,:,40),Q(:,48),MB,G2(:,:,:,50))
  call loop_AV_Q(G2(:,:,:,50),wf(:,14),G2(:,:,:,51))
  call check_last_A_Q(l_switch,G2(:,:,:,51),Q(:,63),MB,G3tensor(:,15))
  call loop_AV_Q(G2(:,:,:,50),wf(:,16),G2(:,:,:,52))
  call check_last_A_Q(l_switch,G2(:,:,:,52),Q(:,63),MB,G3tensor(:,16))

  end if

  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  M(1) = M(1) + (-(f(3)/2._/**/REALKIND) * den(5)) * TI2_call(3,momenta_2,masses2_3,G3tensor(:,1),T3sum(1:35,1))
  M(1) = M(1) + (-(f(2)/2._/**/REALKIND) * den(5)) * TI2_call(3,momenta_2,masses2_2,G3tensor(:,2),T3sum(1:35,2))
  M(1) = M(1) + (-(f(3)/2._/**/REALKIND) * den(5)) * TI2_call(3,momenta_2,masses2_3,G3tensor(:,3),T3sum(1:35,1))
  M(1) = M(1) + (-(f(2)/2._/**/REALKIND) * den(5)) * TI2_call(3,momenta_2,masses2_2,G3tensor(:,4),T3sum(1:35,2))
  M(1) = M(1) + (-(f(3)/2._/**/REALKIND) * den(5)) * TI2_call(3,momenta_2,masses2_3,G3tensor(:,5),T3sum(1:35,1))
  M(1) = M(1) + (-(f(2)/2._/**/REALKIND) * den(5)) * TI2_call(3,momenta_2,masses2_1,G3tensor(:,6),T3sum(1:35,3))
  M(1) = M(1) + (-(f(3)/2._/**/REALKIND) * den(5)) * TI2_call(3,momenta_2,masses2_3,G3tensor(:,7),T3sum(1:35,1))
  M(1) = M(1) + (-(f(2)/2._/**/REALKIND) * den(5)) * TI2_call(3,momenta_2,masses2_1,G3tensor(:,8),T3sum(1:35,3))
  M(1) = M(1) + (-(f(7)/2._/**/REALKIND) * den(18)) * TI2_call(4,momenta_4,masses2_5,G4tensor(:,1),T4sum(1:70,1))
  M(1) = M(1) + (-(f(7)/2._/**/REALKIND) * den(18)) * TI2_call(4,momenta_4,masses2_5,G4tensor(:,3),T4sum(1:70,1))
  M(1) = M(1) + (-(f(5)/2._/**/REALKIND) * den(18)) * TI2_call(4,momenta_4,masses2_4,G4tensor(:,5),T4sum(1:70,2))
  M(1) = M(1) + (-(f(5)/2._/**/REALKIND) * den(18)) * TI2_call(4,momenta_4,masses2_4,G4tensor(:,7),T4sum(1:70,2))
  M(1) = M(1) + (-(f(7)/2._/**/REALKIND) * den(18)) * TI2_call(4,momenta_3,masses2_5,G4tensor(:,4),T4sum(1:70,3))
  M(1) = M(1) + (-(f(7)/2._/**/REALKIND) * den(18)) * TI2_call(4,momenta_3,masses2_5,G4tensor(:,2),T4sum(1:70,3))
  M(1) = M(1) + (-(f(5)/2._/**/REALKIND) * den(18)) * TI2_call(4,momenta_3,masses2_4,G4tensor(:,8),T4sum(1:70,4))
  M(1) = M(1) + (-(f(5)/2._/**/REALKIND) * den(18)) * TI2_call(4,momenta_3,masses2_4,G4tensor(:,6),T4sum(1:70,4))
  M(1) = M(1) + (-(f(7)/2._/**/REALKIND) * den(18)) * TI2_call(4,momenta_5,masses2_5,G4tensor(:,9),T4sum(1:70,5))
  M(1) = M(1) + (-(f(7)/2._/**/REALKIND) * den(18)) * TI2_call(4,momenta_5,masses2_5,G4tensor(:,10),T4sum(1:70,5))
  M(1) = M(1) + (-(f(5)/2._/**/REALKIND) * den(18)) * TI2_call(4,momenta_5,masses2_4,G4tensor(:,11),T4sum(1:70,6))
  M(1) = M(1) + (-(f(5)/2._/**/REALKIND) * den(18)) * TI2_call(4,momenta_5,masses2_4,G4tensor(:,12),T4sum(1:70,6))
  M(1) = M(1) + (-(f(7)/2._/**/REALKIND) * den(14)) * TI2_call(3,momenta_1,masses2_2,G3tensor(:,11),T3sum(1:35,4))
  M(1) = M(1) + (-(f(7)/2._/**/REALKIND) * den(14)) * TI2_call(3,momenta_1,masses2_2,G3tensor(:,9),T3sum(1:35,4))
  M(1) = M(1) + (-(f(5)/2._/**/REALKIND) * den(14)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,15),T3sum(1:35,5))
  M(1) = M(1) + (-(f(5)/2._/**/REALKIND) * den(14)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,13),T3sum(1:35,5))
  M(1) = M(1) + (-(f(7)/2._/**/REALKIND) * den(17)) * TI2_call(3,momenta_1,masses2_2,G3tensor(:,12),T3sum(1:35,4))
  M(1) = M(1) + (-(f(7)/2._/**/REALKIND) * den(17)) * TI2_call(3,momenta_1,masses2_2,G3tensor(:,10),T3sum(1:35,4))
  M(1) = M(1) + (-(f(5)/2._/**/REALKIND) * den(17)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,16),T3sum(1:35,5))
  M(1) = M(1) + (-(f(5)/2._/**/REALKIND) * den(17)) * TI2_call(3,momenta_1,masses2_1,G3tensor(:,14),T3sum(1:35,5))

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

end module ol_vamp_1_pphllj2_nenexuuxhg_1_/**/REALKIND
