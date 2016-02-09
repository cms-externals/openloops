
module ol_vamp_42_ppjjjj_gggggg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_42(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppjjjj_gggggg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppjjjj_gggggg_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppjjjj_gggggg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppjjjj_gggggg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(250)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,1) :: G0
  complex(REALKIND), dimension(4,5,4,1) :: G1
  complex(REALKIND), dimension(4,15,4,145) :: G2
  complex(REALKIND), dimension(4,35,4,138) :: G3
  complex(REALKIND), dimension(4,70,4,72) :: G4
  complex(REALKIND), dimension(4,126,4,18) :: G5
  complex(REALKIND), dimension(35,96) :: G3tensor
  complex(REALKIND), dimension(70,96) :: G4tensor
  complex(REALKIND), dimension(126,54) :: G5tensor
  complex(REALKIND), dimension(210,18) :: G6tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,-2),Q(:,4),G1(:,:,:,1))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,2),Q(:,56),G2(:,:,:,1))
  call check_last_CV_D(l_switch,G2(:,:,:,1),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,1))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,5),Q(:,56),G2(:,:,:,2))
  call check_last_CV_D(l_switch,G2(:,:,:,2),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,2))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,6),Q(:,56),G2(:,:,:,3))
  call check_last_CV_D(l_switch,G2(:,:,:,3),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,3))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,106),Q(:,57),G2(:,:,:,4))
  call check_last_CV_D(l_switch,G2(:,:,:,4),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,4))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,107),Q(:,57),G2(:,:,:,5))
  call check_last_CV_D(l_switch,G2(:,:,:,5),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,5))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,108),Q(:,57),G2(:,:,:,6))
  call check_last_CV_D(l_switch,G2(:,:,:,6),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,6))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,837),Q(:,58),G2(:,:,:,7))
  call check_last_CV_D(l_switch,G2(:,:,:,7),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,7))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,839),Q(:,58),G2(:,:,:,8))
  call check_last_CV_D(l_switch,G2(:,:,:,8),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,8))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,840),Q(:,58),G2(:,:,:,9))
  call check_last_CV_D(l_switch,G2(:,:,:,9),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,9))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,110),Q(:,57),G2(:,:,:,10))
  call check_last_CV_D(l_switch,G2(:,:,:,10),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,10))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,111),Q(:,57),G2(:,:,:,11))
  call check_last_CV_D(l_switch,G2(:,:,:,11),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,11))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,112),Q(:,57),G2(:,:,:,12))
  call check_last_CV_D(l_switch,G2(:,:,:,12),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,12))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,844),Q(:,58),G2(:,:,:,13))
  call check_last_CV_D(l_switch,G2(:,:,:,13),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,13))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,845),Q(:,58),G2(:,:,:,14))
  call check_last_CV_D(l_switch,G2(:,:,:,14),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,14))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,846),Q(:,58),G2(:,:,:,15))
  call check_last_CV_D(l_switch,G2(:,:,:,15),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,15))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,114),Q(:,57),G2(:,:,:,16))
  call check_last_CV_D(l_switch,G2(:,:,:,16),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,16))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,115),Q(:,57),G2(:,:,:,17))
  call check_last_CV_D(l_switch,G2(:,:,:,17),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,17))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,116),Q(:,57),G2(:,:,:,18))
  call check_last_CV_D(l_switch,G2(:,:,:,18),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,18))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,848),Q(:,58),G2(:,:,:,19))
  call check_last_CV_D(l_switch,G2(:,:,:,19),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,19))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,849),Q(:,58),G2(:,:,:,20))
  call check_last_CV_D(l_switch,G2(:,:,:,20),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,20))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,850),Q(:,58),G2(:,:,:,21))
  call check_last_CV_D(l_switch,G2(:,:,:,21),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,21))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,871),Q(:,57),G2(:,:,:,22))
  call check_last_CV_D(l_switch,G2(:,:,:,22),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,22))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,872),Q(:,57),G2(:,:,:,23))
  call check_last_CV_D(l_switch,G2(:,:,:,23),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,23))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,873),Q(:,57),G2(:,:,:,24))
  call check_last_CV_D(l_switch,G2(:,:,:,24),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,24))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,852),Q(:,58),G2(:,:,:,25))
  call check_last_CV_D(l_switch,G2(:,:,:,25),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,25))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,853),Q(:,58),G2(:,:,:,26))
  call check_last_CV_D(l_switch,G2(:,:,:,26),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,26))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,854),Q(:,58),G2(:,:,:,27))
  call check_last_CV_D(l_switch,G2(:,:,:,27),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,27))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,-1),Q(:,2),G2(:,:,:,28))
  call loop_CV_D(G2(:,:,:,28),Q(:,6),wf(:,2),Q(:,56),G3(:,:,:,1))
  call check_last_CV_D(l_switch,G3(:,:,:,1),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,1))
  call loop_CV_D(G2(:,:,:,28),Q(:,6),wf(:,5),Q(:,56),G3(:,:,:,2))
  call check_last_CV_D(l_switch,G3(:,:,:,2),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,2))
  call loop_CV_D(G2(:,:,:,28),Q(:,6),wf(:,6),Q(:,56),G3(:,:,:,3))
  call check_last_CV_D(l_switch,G3(:,:,:,3),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,3))
  call loop_CV_D(G2(:,:,:,28),Q(:,6),wf(:,268),Q(:,56),G3(:,:,:,4))
  call check_last_CV_D(l_switch,G3(:,:,:,4),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,4))
  call loop_CV_D(G2(:,:,:,28),Q(:,6),wf(:,75),Q(:,24),G3(:,:,:,5))
  call loop_CV_D(G3(:,:,:,5),Q(:,30),wf(:,-5),Q(:,32),G4(:,:,:,1))
  call check_last_CV_D(l_switch,G4(:,:,:,1),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,1))
  call loop_CV_D(G2(:,:,:,28),Q(:,6),wf(:,-5),Q(:,32),G3(:,:,:,6))
  call loop_CV_D(G3(:,:,:,6),Q(:,38),wf(:,75),Q(:,24),G4(:,:,:,2))
  call check_last_CV_D(l_switch,G4(:,:,:,2),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,2))
  call loop_CV_D(G3(:,:,:,6),Q(:,38),wf(:,-4),Q(:,16),G4(:,:,:,3))
  call loop_CV_D(G4(:,:,:,3),Q(:,54),wf(:,-3),Q(:,8),G5(:,:,:,1))
  call check_last_CV_D(l_switch,G5(:,:,:,1),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,1))
  call loop_CV_D(G3(:,:,:,6),Q(:,38),wf(:,-3),Q(:,8),G4(:,:,:,4))
  call loop_CV_D(G4(:,:,:,4),Q(:,46),wf(:,-4),Q(:,16),G5(:,:,:,2))
  call check_last_CV_D(l_switch,G5(:,:,:,2),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,2))
  call loop_CV_D(G2(:,:,:,28),Q(:,6),wf(:,269),Q(:,56),G3(:,:,:,7))
  call check_last_CV_D(l_switch,G3(:,:,:,7),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,5))
  call loop_CV_D(G2(:,:,:,28),Q(:,6),wf(:,79),Q(:,40),G3(:,:,:,8))
  call loop_CV_D(G3(:,:,:,8),Q(:,46),wf(:,-4),Q(:,16),G4(:,:,:,5))
  call check_last_CV_D(l_switch,G4(:,:,:,5),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,3))
  call loop_CV_D(G2(:,:,:,28),Q(:,6),wf(:,-4),Q(:,16),G3(:,:,:,9))
  call loop_CV_D(G3(:,:,:,9),Q(:,22),wf(:,79),Q(:,40),G4(:,:,:,6))
  call check_last_CV_D(l_switch,G4(:,:,:,6),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,4))
  call loop_CV_D(G3(:,:,:,9),Q(:,22),wf(:,-5),Q(:,32),G4(:,:,:,7))
  call loop_CV_D(G4(:,:,:,7),Q(:,54),wf(:,-3),Q(:,8),G5(:,:,:,3))
  call check_last_CV_D(l_switch,G5(:,:,:,3),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,3))
  call loop_CV_D(G3(:,:,:,9),Q(:,22),wf(:,-3),Q(:,8),G4(:,:,:,8))
  call loop_CV_D(G4(:,:,:,8),Q(:,30),wf(:,-5),Q(:,32),G5(:,:,:,4))
  call check_last_CV_D(l_switch,G5(:,:,:,4),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,4))
  call loop_CV_D(G2(:,:,:,28),Q(:,6),wf(:,270),Q(:,56),G3(:,:,:,10))
  call check_last_CV_D(l_switch,G3(:,:,:,10),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,6))
  call loop_CV_D(G2(:,:,:,28),Q(:,6),wf(:,84),Q(:,48),G3(:,:,:,11))
  call loop_CV_D(G3(:,:,:,11),Q(:,54),wf(:,-3),Q(:,8),G4(:,:,:,9))
  call check_last_CV_D(l_switch,G4(:,:,:,9),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,5))
  call loop_CV_D(G2(:,:,:,28),Q(:,6),wf(:,-3),Q(:,8),G3(:,:,:,12))
  call loop_CV_D(G3(:,:,:,12),Q(:,14),wf(:,84),Q(:,48),G4(:,:,:,10))
  call check_last_CV_D(l_switch,G4(:,:,:,10),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,6))
  call loop_CV_D(G3(:,:,:,12),Q(:,14),wf(:,-5),Q(:,32),G4(:,:,:,11))
  call loop_CV_D(G4(:,:,:,11),Q(:,46),wf(:,-4),Q(:,16),G5(:,:,:,5))
  call check_last_CV_D(l_switch,G5(:,:,:,5),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,5))
  call loop_CV_D(G3(:,:,:,12),Q(:,14),wf(:,-4),Q(:,16),G4(:,:,:,12))
  call loop_CV_D(G4(:,:,:,12),Q(:,30),wf(:,-5),Q(:,32),G5(:,:,:,6))
  call check_last_CV_D(l_switch,G5(:,:,:,6),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,6))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,916),Q(:,58),G2(:,:,:,29))
  call check_last_CV_D(l_switch,G2(:,:,:,29),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,28))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,917),Q(:,58),G2(:,:,:,30))
  call check_last_CV_D(l_switch,G2(:,:,:,30),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,29))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,918),Q(:,58),G2(:,:,:,31))
  call check_last_CV_D(l_switch,G2(:,:,:,31),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,30))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,919),Q(:,58),G2(:,:,:,32))
  call check_last_CV_D(l_switch,G2(:,:,:,32),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,31))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,920),Q(:,58),G2(:,:,:,33))
  call check_last_CV_D(l_switch,G2(:,:,:,33),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,32))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,921),Q(:,58),G2(:,:,:,34))
  call check_last_CV_D(l_switch,G2(:,:,:,34),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,33))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,922),Q(:,58),G2(:,:,:,35))
  call check_last_CV_D(l_switch,G2(:,:,:,35),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,34))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,923),Q(:,58),G2(:,:,:,36))
  call check_last_CV_D(l_switch,G2(:,:,:,36),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,35))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,924),Q(:,58),G2(:,:,:,37))
  call check_last_CV_D(l_switch,G2(:,:,:,37),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,36))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,26),Q(:,50),G2(:,:,:,38))
  call loop_CV_D(G2(:,:,:,38),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,13))
  call check_last_CV_D(l_switch,G3(:,:,:,13),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,7))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,29),Q(:,50),G2(:,:,:,39))
  call loop_CV_D(G2(:,:,:,39),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,14))
  call check_last_CV_D(l_switch,G3(:,:,:,14),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,8))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,30),Q(:,50),G2(:,:,:,40))
  call loop_CV_D(G2(:,:,:,40),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,15))
  call check_last_CV_D(l_switch,G3(:,:,:,15),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,9))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,-3),Q(:,8),G2(:,:,:,41))
  call loop_CV_D(G2(:,:,:,41),Q(:,12),wf(:,26),Q(:,50),G3(:,:,:,16))
  call check_last_CV_D(l_switch,G3(:,:,:,16),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,10))
  call loop_CV_D(G2(:,:,:,41),Q(:,12),wf(:,29),Q(:,50),G3(:,:,:,17))
  call check_last_CV_D(l_switch,G3(:,:,:,17),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,11))
  call loop_CV_D(G2(:,:,:,41),Q(:,12),wf(:,30),Q(:,50),G3(:,:,:,18))
  call check_last_CV_D(l_switch,G3(:,:,:,18),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,12))
  call loop_CV_D(G2(:,:,:,41),Q(:,12),wf(:,55),Q(:,49),G3(:,:,:,19))
  call check_last_CV_D(l_switch,G3(:,:,:,19),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,13))
  call loop_CV_D(G2(:,:,:,41),Q(:,12),wf(:,57),Q(:,49),G3(:,:,:,20))
  call check_last_CV_D(l_switch,G3(:,:,:,20),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,14))
  call loop_CV_D(G2(:,:,:,41),Q(:,12),wf(:,58),Q(:,49),G3(:,:,:,21))
  call check_last_CV_D(l_switch,G3(:,:,:,21),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,15))
  call loop_CV_D(G2(:,:,:,41),Q(:,12),wf(:,84),Q(:,48),G3(:,:,:,22))
  call check_last_CV_D(l_switch,G3(:,:,:,22),Q(:,60),wf(:,61),Q(:,3),G4tensor(:,16))
  call loop_CV_D(G2(:,:,:,41),Q(:,12),wf(:,-4),Q(:,16),G3(:,:,:,23))
  call loop_CV_D(G3(:,:,:,23),Q(:,28),wf(:,-5),Q(:,32),G4(:,:,:,13))
  call check_last_CV_D(l_switch,G4(:,:,:,13),Q(:,60),wf(:,61),Q(:,3),G5tensor(:,7))
  call loop_CV_D(G3(:,:,:,23),Q(:,28),wf(:,113),Q(:,33),G4(:,:,:,14))
  call check_last_CV_D(l_switch,G4(:,:,:,14),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,8))
  call loop_CV_D(G3(:,:,:,23),Q(:,28),wf(:,99),Q(:,34),G4(:,:,:,15))
  call check_last_CV_D(l_switch,G4(:,:,:,15),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,9))
  call loop_CV_D(G3(:,:,:,23),Q(:,28),wf(:,-1),Q(:,2),G4(:,:,:,16))
  call loop_CV_D(G4(:,:,:,16),Q(:,30),wf(:,-5),Q(:,32),G5(:,:,:,7))
  call check_last_CV_D(l_switch,G5(:,:,:,7),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,7))
  call loop_CV_D(G2(:,:,:,41),Q(:,12),wf(:,-5),Q(:,32),G3(:,:,:,24))
  call loop_CV_D(G3(:,:,:,24),Q(:,44),wf(:,-4),Q(:,16),G4(:,:,:,17))
  call check_last_CV_D(l_switch,G4(:,:,:,17),Q(:,60),wf(:,61),Q(:,3),G5tensor(:,10))
  call loop_CV_D(G3(:,:,:,24),Q(:,44),wf(:,109),Q(:,17),G4(:,:,:,18))
  call check_last_CV_D(l_switch,G4(:,:,:,18),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,11))
  call loop_CV_D(G3(:,:,:,24),Q(:,44),wf(:,95),Q(:,18),G4(:,:,:,19))
  call check_last_CV_D(l_switch,G4(:,:,:,19),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,12))
  call loop_CV_D(G3(:,:,:,24),Q(:,44),wf(:,-1),Q(:,2),G4(:,:,:,20))
  call loop_CV_D(G4(:,:,:,20),Q(:,46),wf(:,-4),Q(:,16),G5(:,:,:,8))
  call check_last_CV_D(l_switch,G5(:,:,:,8),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,8))
  call loop_CV_D(G2(:,:,:,41),Q(:,12),wf(:,193),Q(:,49),G3(:,:,:,25))
  call check_last_CV_D(l_switch,G3(:,:,:,25),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,17))
  call loop_CV_D(G2(:,:,:,41),Q(:,12),wf(:,109),Q(:,17),G3(:,:,:,26))
  call loop_CV_D(G3(:,:,:,26),Q(:,29),wf(:,-5),Q(:,32),G4(:,:,:,21))
  call check_last_CV_D(l_switch,G4(:,:,:,21),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,13))
  call loop_CV_D(G2(:,:,:,41),Q(:,12),wf(:,247),Q(:,50),G3(:,:,:,27))
  call check_last_CV_D(l_switch,G3(:,:,:,27),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,18))
  call loop_CV_D(G2(:,:,:,41),Q(:,12),wf(:,95),Q(:,18),G3(:,:,:,28))
  call loop_CV_D(G3(:,:,:,28),Q(:,30),wf(:,-5),Q(:,32),G4(:,:,:,22))
  call check_last_CV_D(l_switch,G4(:,:,:,22),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,14))
  call loop_CV_D(G2(:,:,:,41),Q(:,12),wf(:,194),Q(:,49),G3(:,:,:,29))
  call check_last_CV_D(l_switch,G3(:,:,:,29),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,19))
  call loop_CV_D(G2(:,:,:,41),Q(:,12),wf(:,113),Q(:,33),G3(:,:,:,30))
  call loop_CV_D(G3(:,:,:,30),Q(:,45),wf(:,-4),Q(:,16),G4(:,:,:,23))
  call check_last_CV_D(l_switch,G4(:,:,:,23),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,15))
  call loop_CV_D(G2(:,:,:,41),Q(:,12),wf(:,248),Q(:,50),G3(:,:,:,31))
  call check_last_CV_D(l_switch,G3(:,:,:,31),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,20))
  call loop_CV_D(G2(:,:,:,41),Q(:,12),wf(:,99),Q(:,34),G3(:,:,:,32))
  call loop_CV_D(G3(:,:,:,32),Q(:,46),wf(:,-4),Q(:,16),G4(:,:,:,24))
  call check_last_CV_D(l_switch,G4(:,:,:,24),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,16))
  call loop_CV_D(G2(:,:,:,41),Q(:,12),wf(:,195),Q(:,49),G3(:,:,:,33))
  call check_last_CV_D(l_switch,G3(:,:,:,33),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,21))
  call loop_CV_D(G2(:,:,:,41),Q(:,12),wf(:,249),Q(:,50),G3(:,:,:,34))
  call check_last_CV_D(l_switch,G3(:,:,:,34),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,22))
  call loop_CV_D(G2(:,:,:,41),Q(:,12),wf(:,-1),Q(:,2),G3(:,:,:,35))
  call loop_CV_D(G3(:,:,:,35),Q(:,14),wf(:,84),Q(:,48),G4(:,:,:,25))
  call check_last_CV_D(l_switch,G4(:,:,:,25),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,17))
  call loop_CV_D(G3(:,:,:,35),Q(:,14),wf(:,-5),Q(:,32),G4(:,:,:,26))
  call loop_CV_D(G4(:,:,:,26),Q(:,46),wf(:,-4),Q(:,16),G5(:,:,:,9))
  call check_last_CV_D(l_switch,G5(:,:,:,9),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,9))
  call loop_CV_D(G3(:,:,:,35),Q(:,14),wf(:,-4),Q(:,16),G4(:,:,:,27))
  call loop_CV_D(G4(:,:,:,27),Q(:,30),wf(:,-5),Q(:,32),G5(:,:,:,10))
  call check_last_CV_D(l_switch,G5(:,:,:,10),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,10))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,928),Q(:,58),G2(:,:,:,42))
  call check_last_CV_D(l_switch,G2(:,:,:,42),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,37))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,929),Q(:,58),G2(:,:,:,43))
  call check_last_CV_D(l_switch,G2(:,:,:,43),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,38))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,930),Q(:,58),G2(:,:,:,44))
  call check_last_CV_D(l_switch,G2(:,:,:,44),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,39))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,931),Q(:,58),G2(:,:,:,45))
  call check_last_CV_D(l_switch,G2(:,:,:,45),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,40))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,932),Q(:,58),G2(:,:,:,46))
  call check_last_CV_D(l_switch,G2(:,:,:,46),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,41))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,933),Q(:,58),G2(:,:,:,47))
  call check_last_CV_D(l_switch,G2(:,:,:,47),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,42))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,32),Q(:,42),G2(:,:,:,48))
  call loop_CV_D(G2(:,:,:,48),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,36))
  call check_last_CV_D(l_switch,G3(:,:,:,36),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,23))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,35),Q(:,42),G2(:,:,:,49))
  call loop_CV_D(G2(:,:,:,49),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,37))
  call check_last_CV_D(l_switch,G3(:,:,:,37),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,24))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,36),Q(:,42),G2(:,:,:,50))
  call loop_CV_D(G2(:,:,:,50),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,38))
  call check_last_CV_D(l_switch,G3(:,:,:,38),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,25))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,-4),Q(:,16),G2(:,:,:,51))
  call loop_CV_D(G2(:,:,:,51),Q(:,20),wf(:,32),Q(:,42),G3(:,:,:,39))
  call check_last_CV_D(l_switch,G3(:,:,:,39),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,26))
  call loop_CV_D(G2(:,:,:,51),Q(:,20),wf(:,35),Q(:,42),G3(:,:,:,40))
  call check_last_CV_D(l_switch,G3(:,:,:,40),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,27))
  call loop_CV_D(G2(:,:,:,51),Q(:,20),wf(:,36),Q(:,42),G3(:,:,:,41))
  call check_last_CV_D(l_switch,G3(:,:,:,41),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,28))
  call loop_CV_D(G2(:,:,:,51),Q(:,20),wf(:,49),Q(:,41),G3(:,:,:,42))
  call check_last_CV_D(l_switch,G3(:,:,:,42),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,29))
  call loop_CV_D(G2(:,:,:,51),Q(:,20),wf(:,51),Q(:,41),G3(:,:,:,43))
  call check_last_CV_D(l_switch,G3(:,:,:,43),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,30))
  call loop_CV_D(G2(:,:,:,51),Q(:,20),wf(:,52),Q(:,41),G3(:,:,:,44))
  call check_last_CV_D(l_switch,G3(:,:,:,44),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,31))
  call loop_CV_D(G2(:,:,:,51),Q(:,20),wf(:,79),Q(:,40),G3(:,:,:,45))
  call check_last_CV_D(l_switch,G3(:,:,:,45),Q(:,60),wf(:,61),Q(:,3),G4tensor(:,32))
  call loop_CV_D(G2(:,:,:,51),Q(:,20),wf(:,-3),Q(:,8),G3(:,:,:,46))
  call loop_CV_D(G3(:,:,:,46),Q(:,28),wf(:,-5),Q(:,32),G4(:,:,:,28))
  call check_last_CV_D(l_switch,G4(:,:,:,28),Q(:,60),wf(:,61),Q(:,3),G5tensor(:,18))
  call loop_CV_D(G3(:,:,:,46),Q(:,28),wf(:,113),Q(:,33),G4(:,:,:,29))
  call check_last_CV_D(l_switch,G4(:,:,:,29),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,19))
  call loop_CV_D(G3(:,:,:,46),Q(:,28),wf(:,99),Q(:,34),G4(:,:,:,30))
  call check_last_CV_D(l_switch,G4(:,:,:,30),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,20))
  call loop_CV_D(G3(:,:,:,46),Q(:,28),wf(:,-1),Q(:,2),G4(:,:,:,31))
  call loop_CV_D(G4(:,:,:,31),Q(:,30),wf(:,-5),Q(:,32),G5(:,:,:,11))
  call check_last_CV_D(l_switch,G5(:,:,:,11),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,11))
  call loop_CV_D(G2(:,:,:,51),Q(:,20),wf(:,-5),Q(:,32),G3(:,:,:,47))
  call loop_CV_D(G3(:,:,:,47),Q(:,52),wf(:,-3),Q(:,8),G4(:,:,:,32))
  call check_last_CV_D(l_switch,G4(:,:,:,32),Q(:,60),wf(:,61),Q(:,3),G5tensor(:,21))
  call loop_CV_D(G3(:,:,:,47),Q(:,52),wf(:,104),Q(:,9),G4(:,:,:,33))
  call check_last_CV_D(l_switch,G4(:,:,:,33),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,22))
  call loop_CV_D(G3(:,:,:,47),Q(:,52),wf(:,91),Q(:,10),G4(:,:,:,34))
  call check_last_CV_D(l_switch,G4(:,:,:,34),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,23))
  call loop_CV_D(G3(:,:,:,47),Q(:,52),wf(:,-1),Q(:,2),G4(:,:,:,35))
  call loop_CV_D(G4(:,:,:,35),Q(:,54),wf(:,-3),Q(:,8),G5(:,:,:,12))
  call check_last_CV_D(l_switch,G5(:,:,:,12),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,12))
  call loop_CV_D(G2(:,:,:,51),Q(:,20),wf(:,182),Q(:,41),G3(:,:,:,48))
  call check_last_CV_D(l_switch,G3(:,:,:,48),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,33))
  call loop_CV_D(G2(:,:,:,51),Q(:,20),wf(:,104),Q(:,9),G3(:,:,:,49))
  call loop_CV_D(G3(:,:,:,49),Q(:,29),wf(:,-5),Q(:,32),G4(:,:,:,36))
  call check_last_CV_D(l_switch,G4(:,:,:,36),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,24))
  call loop_CV_D(G2(:,:,:,51),Q(:,20),wf(:,236),Q(:,42),G3(:,:,:,50))
  call check_last_CV_D(l_switch,G3(:,:,:,50),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,34))
  call loop_CV_D(G2(:,:,:,51),Q(:,20),wf(:,91),Q(:,10),G3(:,:,:,51))
  call loop_CV_D(G3(:,:,:,51),Q(:,30),wf(:,-5),Q(:,32),G4(:,:,:,37))
  call check_last_CV_D(l_switch,G4(:,:,:,37),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,25))
  call loop_CV_D(G2(:,:,:,51),Q(:,20),wf(:,191),Q(:,41),G3(:,:,:,52))
  call check_last_CV_D(l_switch,G3(:,:,:,52),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,35))
  call loop_CV_D(G2(:,:,:,51),Q(:,20),wf(:,113),Q(:,33),G3(:,:,:,53))
  call loop_CV_D(G3(:,:,:,53),Q(:,53),wf(:,-3),Q(:,8),G4(:,:,:,38))
  call check_last_CV_D(l_switch,G4(:,:,:,38),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,26))
  call loop_CV_D(G2(:,:,:,51),Q(:,20),wf(:,245),Q(:,42),G3(:,:,:,54))
  call check_last_CV_D(l_switch,G3(:,:,:,54),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,36))
  call loop_CV_D(G2(:,:,:,51),Q(:,20),wf(:,99),Q(:,34),G3(:,:,:,55))
  call loop_CV_D(G3(:,:,:,55),Q(:,54),wf(:,-3),Q(:,8),G4(:,:,:,39))
  call check_last_CV_D(l_switch,G4(:,:,:,39),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,27))
  call loop_CV_D(G2(:,:,:,51),Q(:,20),wf(:,192),Q(:,41),G3(:,:,:,56))
  call check_last_CV_D(l_switch,G3(:,:,:,56),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,37))
  call loop_CV_D(G2(:,:,:,51),Q(:,20),wf(:,246),Q(:,42),G3(:,:,:,57))
  call check_last_CV_D(l_switch,G3(:,:,:,57),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,38))
  call loop_CV_D(G2(:,:,:,51),Q(:,20),wf(:,-1),Q(:,2),G3(:,:,:,58))
  call loop_CV_D(G3(:,:,:,58),Q(:,22),wf(:,79),Q(:,40),G4(:,:,:,40))
  call check_last_CV_D(l_switch,G4(:,:,:,40),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,28))
  call loop_CV_D(G3(:,:,:,58),Q(:,22),wf(:,-5),Q(:,32),G4(:,:,:,41))
  call loop_CV_D(G4(:,:,:,41),Q(:,54),wf(:,-3),Q(:,8),G5(:,:,:,13))
  call check_last_CV_D(l_switch,G5(:,:,:,13),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,13))
  call loop_CV_D(G3(:,:,:,58),Q(:,22),wf(:,-3),Q(:,8),G4(:,:,:,42))
  call loop_CV_D(G4(:,:,:,42),Q(:,30),wf(:,-5),Q(:,32),G5(:,:,:,14))
  call check_last_CV_D(l_switch,G5(:,:,:,14),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,14))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,937),Q(:,58),G2(:,:,:,52))
  call check_last_CV_D(l_switch,G2(:,:,:,52),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,43))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,938),Q(:,58),G2(:,:,:,53))
  call check_last_CV_D(l_switch,G2(:,:,:,53),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,44))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,939),Q(:,58),G2(:,:,:,54))
  call check_last_CV_D(l_switch,G2(:,:,:,54),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,45))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,38),Q(:,26),G2(:,:,:,55))
  call loop_CV_D(G2(:,:,:,55),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,59))
  call check_last_CV_D(l_switch,G3(:,:,:,59),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,39))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,41),Q(:,26),G2(:,:,:,56))
  call loop_CV_D(G2(:,:,:,56),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,60))
  call check_last_CV_D(l_switch,G3(:,:,:,60),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,40))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,42),Q(:,26),G2(:,:,:,57))
  call loop_CV_D(G2(:,:,:,57),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,61))
  call check_last_CV_D(l_switch,G3(:,:,:,61),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,41))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,-5),Q(:,32),G2(:,:,:,58))
  call loop_CV_D(G2(:,:,:,58),Q(:,36),wf(:,38),Q(:,26),G3(:,:,:,62))
  call check_last_CV_D(l_switch,G3(:,:,:,62),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,42))
  call loop_CV_D(G2(:,:,:,58),Q(:,36),wf(:,41),Q(:,26),G3(:,:,:,63))
  call check_last_CV_D(l_switch,G3(:,:,:,63),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,43))
  call loop_CV_D(G2(:,:,:,58),Q(:,36),wf(:,42),Q(:,26),G3(:,:,:,64))
  call check_last_CV_D(l_switch,G3(:,:,:,64),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,44))
  call loop_CV_D(G2(:,:,:,58),Q(:,36),wf(:,43),Q(:,25),G3(:,:,:,65))
  call check_last_CV_D(l_switch,G3(:,:,:,65),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,45))
  call loop_CV_D(G2(:,:,:,58),Q(:,36),wf(:,45),Q(:,25),G3(:,:,:,66))
  call check_last_CV_D(l_switch,G3(:,:,:,66),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,46))
  call loop_CV_D(G2(:,:,:,58),Q(:,36),wf(:,46),Q(:,25),G3(:,:,:,67))
  call check_last_CV_D(l_switch,G3(:,:,:,67),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,47))
  call loop_CV_D(G2(:,:,:,58),Q(:,36),wf(:,75),Q(:,24),G3(:,:,:,68))
  call check_last_CV_D(l_switch,G3(:,:,:,68),Q(:,60),wf(:,61),Q(:,3),G4tensor(:,48))
  call loop_CV_D(G2(:,:,:,58),Q(:,36),wf(:,-3),Q(:,8),G3(:,:,:,69))
  call loop_CV_D(G3(:,:,:,69),Q(:,44),wf(:,-4),Q(:,16),G4(:,:,:,43))
  call check_last_CV_D(l_switch,G4(:,:,:,43),Q(:,60),wf(:,61),Q(:,3),G5tensor(:,29))
  call loop_CV_D(G3(:,:,:,69),Q(:,44),wf(:,109),Q(:,17),G4(:,:,:,44))
  call check_last_CV_D(l_switch,G4(:,:,:,44),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,30))
  call loop_CV_D(G3(:,:,:,69),Q(:,44),wf(:,95),Q(:,18),G4(:,:,:,45))
  call check_last_CV_D(l_switch,G4(:,:,:,45),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,31))
  call loop_CV_D(G3(:,:,:,69),Q(:,44),wf(:,-1),Q(:,2),G4(:,:,:,46))
  call loop_CV_D(G4(:,:,:,46),Q(:,46),wf(:,-4),Q(:,16),G5(:,:,:,15))
  call check_last_CV_D(l_switch,G5(:,:,:,15),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,15))
  call loop_CV_D(G2(:,:,:,58),Q(:,36),wf(:,-4),Q(:,16),G3(:,:,:,70))
  call loop_CV_D(G3(:,:,:,70),Q(:,52),wf(:,-3),Q(:,8),G4(:,:,:,47))
  call check_last_CV_D(l_switch,G4(:,:,:,47),Q(:,60),wf(:,61),Q(:,3),G5tensor(:,32))
  call loop_CV_D(G3(:,:,:,70),Q(:,52),wf(:,104),Q(:,9),G4(:,:,:,48))
  call check_last_CV_D(l_switch,G4(:,:,:,48),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,33))
  call loop_CV_D(G3(:,:,:,70),Q(:,52),wf(:,91),Q(:,10),G4(:,:,:,49))
  call check_last_CV_D(l_switch,G4(:,:,:,49),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,34))
  call loop_CV_D(G3(:,:,:,70),Q(:,52),wf(:,-1),Q(:,2),G4(:,:,:,50))
  call loop_CV_D(G4(:,:,:,50),Q(:,54),wf(:,-3),Q(:,8),G5(:,:,:,16))
  call check_last_CV_D(l_switch,G5(:,:,:,16),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,16))
  call loop_CV_D(G2(:,:,:,58),Q(:,36),wf(:,181),Q(:,25),G3(:,:,:,71))
  call check_last_CV_D(l_switch,G3(:,:,:,71),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,49))
  call loop_CV_D(G2(:,:,:,58),Q(:,36),wf(:,104),Q(:,9),G3(:,:,:,72))
  call loop_CV_D(G3(:,:,:,72),Q(:,45),wf(:,-4),Q(:,16),G4(:,:,:,51))
  call check_last_CV_D(l_switch,G4(:,:,:,51),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,35))
  call loop_CV_D(G2(:,:,:,58),Q(:,36),wf(:,235),Q(:,26),G3(:,:,:,73))
  call check_last_CV_D(l_switch,G3(:,:,:,73),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,50))
  call loop_CV_D(G2(:,:,:,58),Q(:,36),wf(:,91),Q(:,10),G3(:,:,:,74))
  call loop_CV_D(G3(:,:,:,74),Q(:,46),wf(:,-4),Q(:,16),G4(:,:,:,52))
  call check_last_CV_D(l_switch,G4(:,:,:,52),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,36))
  call loop_CV_D(G2(:,:,:,58),Q(:,36),wf(:,186),Q(:,25),G3(:,:,:,75))
  call check_last_CV_D(l_switch,G3(:,:,:,75),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,51))
  call loop_CV_D(G2(:,:,:,58),Q(:,36),wf(:,109),Q(:,17),G3(:,:,:,76))
  call loop_CV_D(G3(:,:,:,76),Q(:,53),wf(:,-3),Q(:,8),G4(:,:,:,53))
  call check_last_CV_D(l_switch,G4(:,:,:,53),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,37))
  call loop_CV_D(G2(:,:,:,58),Q(:,36),wf(:,240),Q(:,26),G3(:,:,:,77))
  call check_last_CV_D(l_switch,G3(:,:,:,77),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,52))
  call loop_CV_D(G2(:,:,:,58),Q(:,36),wf(:,95),Q(:,18),G3(:,:,:,78))
  call loop_CV_D(G3(:,:,:,78),Q(:,54),wf(:,-3),Q(:,8),G4(:,:,:,54))
  call check_last_CV_D(l_switch,G4(:,:,:,54),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,38))
  call loop_CV_D(G2(:,:,:,58),Q(:,36),wf(:,190),Q(:,25),G3(:,:,:,79))
  call check_last_CV_D(l_switch,G3(:,:,:,79),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,53))
  call loop_CV_D(G2(:,:,:,58),Q(:,36),wf(:,244),Q(:,26),G3(:,:,:,80))
  call check_last_CV_D(l_switch,G3(:,:,:,80),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,54))
  call loop_CV_D(G2(:,:,:,58),Q(:,36),wf(:,-1),Q(:,2),G3(:,:,:,81))
  call loop_CV_D(G3(:,:,:,81),Q(:,38),wf(:,75),Q(:,24),G4(:,:,:,55))
  call check_last_CV_D(l_switch,G4(:,:,:,55),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,39))
  call loop_CV_D(G3(:,:,:,81),Q(:,38),wf(:,-4),Q(:,16),G4(:,:,:,56))
  call loop_CV_D(G4(:,:,:,56),Q(:,54),wf(:,-3),Q(:,8),G5(:,:,:,17))
  call check_last_CV_D(l_switch,G5(:,:,:,17),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,17))
  call loop_CV_D(G3(:,:,:,81),Q(:,38),wf(:,-3),Q(:,8),G4(:,:,:,57))
  call loop_CV_D(G4(:,:,:,57),Q(:,46),wf(:,-4),Q(:,16),G5(:,:,:,18))
  call check_last_CV_D(l_switch,G5(:,:,:,18),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,18))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1036),Q(:,57),G2(:,:,:,59))
  call check_last_CV_D(l_switch,G2(:,:,:,59),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,46))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1037),Q(:,57),G2(:,:,:,60))
  call check_last_CV_D(l_switch,G2(:,:,:,60),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,47))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1038),Q(:,57),G2(:,:,:,61))
  call check_last_CV_D(l_switch,G2(:,:,:,61),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,48))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1039),Q(:,57),G2(:,:,:,62))
  call check_last_CV_D(l_switch,G2(:,:,:,62),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,49))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1040),Q(:,57),G2(:,:,:,63))
  call check_last_CV_D(l_switch,G2(:,:,:,63),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,50))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1041),Q(:,57),G2(:,:,:,64))
  call check_last_CV_D(l_switch,G2(:,:,:,64),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,51))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1042),Q(:,57),G2(:,:,:,65))
  call check_last_CV_D(l_switch,G2(:,:,:,65),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,52))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1043),Q(:,57),G2(:,:,:,66))
  call check_last_CV_D(l_switch,G2(:,:,:,66),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,53))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1044),Q(:,57),G2(:,:,:,67))
  call check_last_CV_D(l_switch,G2(:,:,:,67),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,54))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,55),Q(:,49),G2(:,:,:,68))
  call loop_CV_D(G2(:,:,:,68),Q(:,53),wf(:,-3),Q(:,8),G3(:,:,:,82))
  call check_last_CV_D(l_switch,G3(:,:,:,82),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,55))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,57),Q(:,49),G2(:,:,:,69))
  call loop_CV_D(G2(:,:,:,69),Q(:,53),wf(:,-3),Q(:,8),G3(:,:,:,83))
  call check_last_CV_D(l_switch,G3(:,:,:,83),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,56))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,58),Q(:,49),G2(:,:,:,70))
  call loop_CV_D(G2(:,:,:,70),Q(:,53),wf(:,-3),Q(:,8),G3(:,:,:,84))
  call check_last_CV_D(l_switch,G3(:,:,:,84),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,57))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1045),Q(:,57),G2(:,:,:,71))
  call check_last_CV_D(l_switch,G2(:,:,:,71),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,55))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1046),Q(:,57),G2(:,:,:,72))
  call check_last_CV_D(l_switch,G2(:,:,:,72),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,56))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1047),Q(:,57),G2(:,:,:,73))
  call check_last_CV_D(l_switch,G2(:,:,:,73),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,57))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1048),Q(:,57),G2(:,:,:,74))
  call check_last_CV_D(l_switch,G2(:,:,:,74),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,58))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1049),Q(:,57),G2(:,:,:,75))
  call check_last_CV_D(l_switch,G2(:,:,:,75),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,59))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1050),Q(:,57),G2(:,:,:,76))
  call check_last_CV_D(l_switch,G2(:,:,:,76),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,60))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,49),Q(:,41),G2(:,:,:,77))
  call loop_CV_D(G2(:,:,:,77),Q(:,45),wf(:,-4),Q(:,16),G3(:,:,:,85))
  call check_last_CV_D(l_switch,G3(:,:,:,85),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,58))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,51),Q(:,41),G2(:,:,:,78))
  call loop_CV_D(G2(:,:,:,78),Q(:,45),wf(:,-4),Q(:,16),G3(:,:,:,86))
  call check_last_CV_D(l_switch,G3(:,:,:,86),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,59))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,52),Q(:,41),G2(:,:,:,79))
  call loop_CV_D(G2(:,:,:,79),Q(:,45),wf(:,-4),Q(:,16),G3(:,:,:,87))
  call check_last_CV_D(l_switch,G3(:,:,:,87),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,60))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1051),Q(:,57),G2(:,:,:,80))
  call check_last_CV_D(l_switch,G2(:,:,:,80),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,61))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1052),Q(:,57),G2(:,:,:,81))
  call check_last_CV_D(l_switch,G2(:,:,:,81),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,62))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1053),Q(:,57),G2(:,:,:,82))
  call check_last_CV_D(l_switch,G2(:,:,:,82),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,63))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,43),Q(:,25),G2(:,:,:,83))
  call loop_CV_D(G2(:,:,:,83),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,88))
  call check_last_CV_D(l_switch,G3(:,:,:,88),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,61))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,45),Q(:,25),G2(:,:,:,84))
  call loop_CV_D(G2(:,:,:,84),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,89))
  call check_last_CV_D(l_switch,G3(:,:,:,89),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,62))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,46),Q(:,25),G2(:,:,:,85))
  call loop_CV_D(G2(:,:,:,85),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,90))
  call check_last_CV_D(l_switch,G3(:,:,:,90),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,63))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,268),Q(:,56),G2(:,:,:,86))
  call check_last_CV_D(l_switch,G2(:,:,:,86),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,64))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,75),Q(:,24),G2(:,:,:,87))
  call loop_CV_D(G2(:,:,:,87),Q(:,28),wf(:,-5),Q(:,32),G3(:,:,:,91))
  call check_last_CV_D(l_switch,G3(:,:,:,91),Q(:,60),wf(:,61),Q(:,3),G4tensor(:,64))
  call loop_CV_D(G2(:,:,:,87),Q(:,28),wf(:,113),Q(:,33),G3(:,:,:,92))
  call check_last_CV_D(l_switch,G3(:,:,:,92),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,65))
  call loop_CV_D(G2(:,:,:,87),Q(:,28),wf(:,99),Q(:,34),G3(:,:,:,93))
  call check_last_CV_D(l_switch,G3(:,:,:,93),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,66))
  call loop_CV_D(G2(:,:,:,87),Q(:,28),wf(:,-1),Q(:,2),G3(:,:,:,94))
  call loop_CV_D(G3(:,:,:,94),Q(:,30),wf(:,-5),Q(:,32),G4(:,:,:,58))
  call check_last_CV_D(l_switch,G4(:,:,:,58),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,40))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,269),Q(:,56),G2(:,:,:,88))
  call check_last_CV_D(l_switch,G2(:,:,:,88),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,65))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,79),Q(:,40),G2(:,:,:,89))
  call loop_CV_D(G2(:,:,:,89),Q(:,44),wf(:,-4),Q(:,16),G3(:,:,:,95))
  call check_last_CV_D(l_switch,G3(:,:,:,95),Q(:,60),wf(:,61),Q(:,3),G4tensor(:,67))
  call loop_CV_D(G2(:,:,:,89),Q(:,44),wf(:,109),Q(:,17),G3(:,:,:,96))
  call check_last_CV_D(l_switch,G3(:,:,:,96),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,68))
  call loop_CV_D(G2(:,:,:,89),Q(:,44),wf(:,95),Q(:,18),G3(:,:,:,97))
  call check_last_CV_D(l_switch,G3(:,:,:,97),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,69))
  call loop_CV_D(G2(:,:,:,89),Q(:,44),wf(:,-1),Q(:,2),G3(:,:,:,98))
  call loop_CV_D(G3(:,:,:,98),Q(:,46),wf(:,-4),Q(:,16),G4(:,:,:,59))
  call check_last_CV_D(l_switch,G4(:,:,:,59),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,41))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,270),Q(:,56),G2(:,:,:,90))
  call check_last_CV_D(l_switch,G2(:,:,:,90),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,66))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,84),Q(:,48),G2(:,:,:,91))
  call loop_CV_D(G2(:,:,:,91),Q(:,52),wf(:,-3),Q(:,8),G3(:,:,:,99))
  call check_last_CV_D(l_switch,G3(:,:,:,99),Q(:,60),wf(:,61),Q(:,3),G4tensor(:,70))
  call loop_CV_D(G2(:,:,:,91),Q(:,52),wf(:,104),Q(:,9),G3(:,:,:,100))
  call check_last_CV_D(l_switch,G3(:,:,:,100),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,71))
  call loop_CV_D(G2(:,:,:,91),Q(:,52),wf(:,91),Q(:,10),G3(:,:,:,101))
  call check_last_CV_D(l_switch,G3(:,:,:,101),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,72))
  call loop_CV_D(G2(:,:,:,91),Q(:,52),wf(:,-1),Q(:,2),G3(:,:,:,102))
  call loop_CV_D(G3(:,:,:,102),Q(:,54),wf(:,-3),Q(:,8),G4(:,:,:,60))
  call check_last_CV_D(l_switch,G4(:,:,:,60),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,42))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1246),Q(:,57),G2(:,:,:,92))
  call check_last_CV_D(l_switch,G2(:,:,:,92),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,67))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,104),Q(:,9),G2(:,:,:,93))
  call loop_CV_D(G2(:,:,:,93),Q(:,13),wf(:,84),Q(:,48),G3(:,:,:,103))
  call check_last_CV_D(l_switch,G3(:,:,:,103),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,73))
  call loop_CV_D(G2(:,:,:,93),Q(:,13),wf(:,-5),Q(:,32),G3(:,:,:,104))
  call loop_CV_D(G3(:,:,:,104),Q(:,45),wf(:,-4),Q(:,16),G4(:,:,:,61))
  call check_last_CV_D(l_switch,G4(:,:,:,61),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,43))
  call loop_CV_D(G2(:,:,:,93),Q(:,13),wf(:,-4),Q(:,16),G3(:,:,:,105))
  call loop_CV_D(G3(:,:,:,105),Q(:,29),wf(:,-5),Q(:,32),G4(:,:,:,62))
  call check_last_CV_D(l_switch,G4(:,:,:,62),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,44))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1365),Q(:,57),G2(:,:,:,94))
  call check_last_CV_D(l_switch,G2(:,:,:,94),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,68))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,181),Q(:,25),G2(:,:,:,95))
  call loop_CV_D(G2(:,:,:,95),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,106))
  call check_last_CV_D(l_switch,G3(:,:,:,106),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,74))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1368),Q(:,57),G2(:,:,:,96))
  call check_last_CV_D(l_switch,G2(:,:,:,96),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,69))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,182),Q(:,41),G2(:,:,:,97))
  call loop_CV_D(G2(:,:,:,97),Q(:,45),wf(:,-4),Q(:,16),G3(:,:,:,107))
  call check_last_CV_D(l_switch,G3(:,:,:,107),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,75))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1217),Q(:,58),G2(:,:,:,98))
  call check_last_CV_D(l_switch,G2(:,:,:,98),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,70))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,91),Q(:,10),G2(:,:,:,99))
  call loop_CV_D(G2(:,:,:,99),Q(:,14),wf(:,84),Q(:,48),G3(:,:,:,108))
  call check_last_CV_D(l_switch,G3(:,:,:,108),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,76))
  call loop_CV_D(G2(:,:,:,99),Q(:,14),wf(:,-5),Q(:,32),G3(:,:,:,109))
  call loop_CV_D(G3(:,:,:,109),Q(:,46),wf(:,-4),Q(:,16),G4(:,:,:,63))
  call check_last_CV_D(l_switch,G4(:,:,:,63),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,45))
  call loop_CV_D(G2(:,:,:,99),Q(:,14),wf(:,-4),Q(:,16),G3(:,:,:,110))
  call loop_CV_D(G3(:,:,:,110),Q(:,30),wf(:,-5),Q(:,32),G4(:,:,:,64))
  call check_last_CV_D(l_switch,G4(:,:,:,64),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,46))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1378),Q(:,58),G2(:,:,:,100))
  call check_last_CV_D(l_switch,G2(:,:,:,100),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,71))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,235),Q(:,26),G2(:,:,:,101))
  call loop_CV_D(G2(:,:,:,101),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,111))
  call check_last_CV_D(l_switch,G3(:,:,:,111),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,77))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1380),Q(:,58),G2(:,:,:,102))
  call check_last_CV_D(l_switch,G2(:,:,:,102),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,72))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,236),Q(:,42),G2(:,:,:,103))
  call loop_CV_D(G2(:,:,:,103),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,112))
  call check_last_CV_D(l_switch,G3(:,:,:,112),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,78))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1254),Q(:,57),G2(:,:,:,104))
  call check_last_CV_D(l_switch,G2(:,:,:,104),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,73))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,109),Q(:,17),G2(:,:,:,105))
  call loop_CV_D(G2(:,:,:,105),Q(:,21),wf(:,79),Q(:,40),G3(:,:,:,113))
  call check_last_CV_D(l_switch,G3(:,:,:,113),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,79))
  call loop_CV_D(G2(:,:,:,105),Q(:,21),wf(:,-5),Q(:,32),G3(:,:,:,114))
  call loop_CV_D(G3(:,:,:,114),Q(:,53),wf(:,-3),Q(:,8),G4(:,:,:,65))
  call check_last_CV_D(l_switch,G4(:,:,:,65),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,47))
  call loop_CV_D(G2(:,:,:,105),Q(:,21),wf(:,-3),Q(:,8),G3(:,:,:,115))
  call loop_CV_D(G3(:,:,:,115),Q(:,29),wf(:,-5),Q(:,32),G4(:,:,:,66))
  call check_last_CV_D(l_switch,G4(:,:,:,66),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,48))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1401),Q(:,57),G2(:,:,:,106))
  call check_last_CV_D(l_switch,G2(:,:,:,106),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,74))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,186),Q(:,25),G2(:,:,:,107))
  call loop_CV_D(G2(:,:,:,107),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,116))
  call check_last_CV_D(l_switch,G3(:,:,:,116),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,80))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1404),Q(:,57),G2(:,:,:,108))
  call check_last_CV_D(l_switch,G2(:,:,:,108),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,75))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,193),Q(:,49),G2(:,:,:,109))
  call loop_CV_D(G2(:,:,:,109),Q(:,53),wf(:,-3),Q(:,8),G3(:,:,:,117))
  call check_last_CV_D(l_switch,G3(:,:,:,117),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,81))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1225),Q(:,58),G2(:,:,:,110))
  call check_last_CV_D(l_switch,G2(:,:,:,110),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,76))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,95),Q(:,18),G2(:,:,:,111))
  call loop_CV_D(G2(:,:,:,111),Q(:,22),wf(:,79),Q(:,40),G3(:,:,:,118))
  call check_last_CV_D(l_switch,G3(:,:,:,118),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,82))
  call loop_CV_D(G2(:,:,:,111),Q(:,22),wf(:,-5),Q(:,32),G3(:,:,:,119))
  call loop_CV_D(G3(:,:,:,119),Q(:,54),wf(:,-3),Q(:,8),G4(:,:,:,67))
  call check_last_CV_D(l_switch,G4(:,:,:,67),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,49))
  call loop_CV_D(G2(:,:,:,111),Q(:,22),wf(:,-3),Q(:,8),G3(:,:,:,120))
  call loop_CV_D(G3(:,:,:,120),Q(:,30),wf(:,-5),Q(:,32),G4(:,:,:,68))
  call check_last_CV_D(l_switch,G4(:,:,:,68),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,50))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1414),Q(:,58),G2(:,:,:,112))
  call check_last_CV_D(l_switch,G2(:,:,:,112),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,77))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,240),Q(:,26),G2(:,:,:,113))
  call loop_CV_D(G2(:,:,:,113),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,121))
  call check_last_CV_D(l_switch,G3(:,:,:,121),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,83))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1416),Q(:,58),G2(:,:,:,114))
  call check_last_CV_D(l_switch,G2(:,:,:,114),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,78))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,247),Q(:,50),G2(:,:,:,115))
  call loop_CV_D(G2(:,:,:,115),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,122))
  call check_last_CV_D(l_switch,G3(:,:,:,122),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,84))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1261),Q(:,57),G2(:,:,:,116))
  call check_last_CV_D(l_switch,G2(:,:,:,116),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,79))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,113),Q(:,33),G2(:,:,:,117))
  call loop_CV_D(G2(:,:,:,117),Q(:,37),wf(:,75),Q(:,24),G3(:,:,:,123))
  call check_last_CV_D(l_switch,G3(:,:,:,123),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,85))
  call loop_CV_D(G2(:,:,:,117),Q(:,37),wf(:,-4),Q(:,16),G3(:,:,:,124))
  call loop_CV_D(G3(:,:,:,124),Q(:,53),wf(:,-3),Q(:,8),G4(:,:,:,69))
  call check_last_CV_D(l_switch,G4(:,:,:,69),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,51))
  call loop_CV_D(G2(:,:,:,117),Q(:,37),wf(:,-3),Q(:,8),G3(:,:,:,125))
  call loop_CV_D(G3(:,:,:,125),Q(:,45),wf(:,-4),Q(:,16),G4(:,:,:,70))
  call check_last_CV_D(l_switch,G4(:,:,:,70),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,52))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1232),Q(:,58),G2(:,:,:,118))
  call check_last_CV_D(l_switch,G2(:,:,:,118),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,80))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,99),Q(:,34),G2(:,:,:,119))
  call loop_CV_D(G2(:,:,:,119),Q(:,38),wf(:,75),Q(:,24),G3(:,:,:,126))
  call check_last_CV_D(l_switch,G3(:,:,:,126),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,86))
  call loop_CV_D(G2(:,:,:,119),Q(:,38),wf(:,-4),Q(:,16),G3(:,:,:,127))
  call loop_CV_D(G3(:,:,:,127),Q(:,54),wf(:,-3),Q(:,8),G4(:,:,:,71))
  call check_last_CV_D(l_switch,G4(:,:,:,71),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,53))
  call loop_CV_D(G2(:,:,:,119),Q(:,38),wf(:,-3),Q(:,8),G3(:,:,:,128))
  call loop_CV_D(G3(:,:,:,128),Q(:,46),wf(:,-4),Q(:,16),G4(:,:,:,72))
  call check_last_CV_D(l_switch,G4(:,:,:,72),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,54))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1433),Q(:,57),G2(:,:,:,120))
  call check_last_CV_D(l_switch,G2(:,:,:,120),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,81))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,190),Q(:,25),G2(:,:,:,121))
  call loop_CV_D(G2(:,:,:,121),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,129))
  call check_last_CV_D(l_switch,G3(:,:,:,129),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,87))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1434),Q(:,57),G2(:,:,:,122))
  call check_last_CV_D(l_switch,G2(:,:,:,122),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,82))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1437),Q(:,58),G2(:,:,:,123))
  call check_last_CV_D(l_switch,G2(:,:,:,123),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,83))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,244),Q(:,26),G2(:,:,:,124))
  call loop_CV_D(G2(:,:,:,124),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,130))
  call check_last_CV_D(l_switch,G3(:,:,:,130),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,88))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1438),Q(:,58),G2(:,:,:,125))
  call check_last_CV_D(l_switch,G2(:,:,:,125),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,84))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1449),Q(:,57),G2(:,:,:,126))
  call check_last_CV_D(l_switch,G2(:,:,:,126),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,85))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,191),Q(:,41),G2(:,:,:,127))
  call loop_CV_D(G2(:,:,:,127),Q(:,45),wf(:,-4),Q(:,16),G3(:,:,:,131))
  call check_last_CV_D(l_switch,G3(:,:,:,131),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,89))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1452),Q(:,57),G2(:,:,:,128))
  call check_last_CV_D(l_switch,G2(:,:,:,128),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,86))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,194),Q(:,49),G2(:,:,:,129))
  call loop_CV_D(G2(:,:,:,129),Q(:,53),wf(:,-3),Q(:,8),G3(:,:,:,132))
  call check_last_CV_D(l_switch,G3(:,:,:,132),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,90))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1462),Q(:,58),G2(:,:,:,130))
  call check_last_CV_D(l_switch,G2(:,:,:,130),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,87))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,245),Q(:,42),G2(:,:,:,131))
  call loop_CV_D(G2(:,:,:,131),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,133))
  call check_last_CV_D(l_switch,G3(:,:,:,133),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,91))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1464),Q(:,58),G2(:,:,:,132))
  call check_last_CV_D(l_switch,G2(:,:,:,132),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,88))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,248),Q(:,50),G2(:,:,:,133))
  call loop_CV_D(G2(:,:,:,133),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,134))
  call check_last_CV_D(l_switch,G3(:,:,:,134),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,92))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1481),Q(:,57),G2(:,:,:,134))
  call check_last_CV_D(l_switch,G2(:,:,:,134),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,89))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,192),Q(:,41),G2(:,:,:,135))
  call loop_CV_D(G2(:,:,:,135),Q(:,45),wf(:,-4),Q(:,16),G3(:,:,:,135))
  call check_last_CV_D(l_switch,G3(:,:,:,135),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,93))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1482),Q(:,57),G2(:,:,:,136))
  call check_last_CV_D(l_switch,G2(:,:,:,136),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,90))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1485),Q(:,58),G2(:,:,:,137))
  call check_last_CV_D(l_switch,G2(:,:,:,137),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,91))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,246),Q(:,42),G2(:,:,:,138))
  call loop_CV_D(G2(:,:,:,138),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,136))
  call check_last_CV_D(l_switch,G3(:,:,:,136),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,94))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1486),Q(:,58),G2(:,:,:,139))
  call check_last_CV_D(l_switch,G2(:,:,:,139),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,92))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1493),Q(:,57),G2(:,:,:,140))
  call check_last_CV_D(l_switch,G2(:,:,:,140),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,93))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1494),Q(:,57),G2(:,:,:,141))
  call check_last_CV_D(l_switch,G2(:,:,:,141),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,94))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,195),Q(:,49),G2(:,:,:,142))
  call loop_CV_D(G2(:,:,:,142),Q(:,53),wf(:,-3),Q(:,8),G3(:,:,:,137))
  call check_last_CV_D(l_switch,G3(:,:,:,137),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,95))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1497),Q(:,58),G2(:,:,:,143))
  call check_last_CV_D(l_switch,G2(:,:,:,143),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,95))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,1498),Q(:,58),G2(:,:,:,144))
  call check_last_CV_D(l_switch,G2(:,:,:,144),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,96))
  call loop_CV_D(G1(:,:,:,1),Q(:,4),wf(:,249),Q(:,50),G2(:,:,:,145))
  call loop_CV_D(G2(:,:,:,145),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,138))
  call check_last_CV_D(l_switch,G3(:,:,:,138),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,96))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(6)*(M(131)-M(133)-M(135)+M(136)-M(140)+M(146)+M(152)-M(154)-M(164)+M(170)+M(176)-M(178)+M(196)-M(220)-M(244) &
    +M(250))) * den(19)
  T3sum(1:35,84) = T3sum(1:35,84) + Gcoeff * G3tensor(:,1)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(134)-M(136)+M(140)-M(142)-M(148)+M(154)+M(164)-M(166)-M(172)+M(178)-M(196)+M(202)+M(226) &
    -M(250))) * den(19)
  T3sum(1:35,84) = T3sum(1:35,84) + Gcoeff * G3tensor(:,2)
  Gcoeff = (c(6)*(-M(132)+M(133)-M(134)+M(135)+M(142)-M(146)+M(148)-M(152)+M(166)-M(170)+M(172)-M(176)-M(202)+M(220)-M(226) &
    +M(244))) * den(19)
  T3sum(1:35,84) = T3sum(1:35,84) + Gcoeff * G3tensor(:,3)
  Gcoeff = (c(6)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(396)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,4)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(158)-M(160)+M(179)-M(180)-M(185)+M(186)-M(195)+M(196)+M(201)-M(202)+M(221)-M(223)-M(245) &
    +M(247))) * den(396)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,5)
  Gcoeff = (c(6)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(396)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,6)
  Gcoeff = (c(6)*(-M(140)+M(155)-M(161)-M(167)+M(169)-M(173)+M(175)+M(177)-M(178)+M(182)+M(206)-M(216)+M(230)-M(240)-M(246) &
    +M(249))) * den(364)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,7)
  Gcoeff = (c(6)*(M(140)-M(142)-M(155)+M(156)+M(161)-M(162)+M(171)-M(172)-M(177)+M(178)-M(182)+M(184)-M(222)+M(225)+M(246) &
    -M(249))) * den(364)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,8)
  Gcoeff = (c(6)*(M(142)-M(156)+M(162)+M(167)-M(169)-M(171)+M(172)+M(173)-M(175)-M(184)-M(206)+M(216)+M(222)-M(225)-M(230) &
    +M(240))) * den(364)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,9)
  Gcoeff = (c(6)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(405)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,10)
  Gcoeff = (c(6)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(405)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,11)
  Gcoeff = (c(6)*(M(132)-M(135)-M(156)+M(159)-M(197)+M(199)-M(203)+M(204)+M(209)-M(210)+M(219)-M(220)-M(225)+M(226)+M(239) &
    -M(241))) * den(405)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,12)
  Gcoeff = (c(6)*(M(146)-M(157)+M(161)-M(163)+M(167)+M(174)-M(175)+M(176)-M(177)-M(182)+M(192)-M(206)-M(232)+M(240)-M(243) &
    +M(246))) * den(373)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,13)
  Gcoeff = (c(6)*(-M(148)+M(158)-M(161)+M(163)+M(165)-M(166)-M(168)-M(174)+M(177)+M(182)-M(192)-M(198)+M(201)+M(208)+M(232) &
    -M(246))) * den(373)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,14)
  Gcoeff = (c(6)*(-M(146)+M(148)+M(157)-M(158)-M(165)+M(166)-M(167)+M(168)+M(175)-M(176)+M(198)-M(201)+M(206)-M(208)-M(240) &
    +M(243))) * den(373)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,15)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(155)-M(157)+M(191)-M(193)-M(215)+M(217)+M(227)-M(228)-M(233)+M(234)-M(243)+M(244)+M(249) &
    -M(250))) * den(410)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,16)
  Gcoeff = (c(6)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(410)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,17)
  Gcoeff = (c(6)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(410)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,18)
  Gcoeff = (c(6)*(M(152)-M(154)-M(159)+M(160)+M(163)-M(164)-M(169)+M(170)+M(173)-M(174)-M(192)+M(195)+M(216)-M(219)-M(230) &
    +M(232))) * den(378)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,19)
  Gcoeff = (c(6)*(M(154)-M(160)+M(162)-M(163)+M(164)-M(165)+M(168)-M(171)+M(174)-M(184)+M(192)-M(195)+M(198)-M(208)+M(222) &
    -M(232))) * den(378)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,20)
  Gcoeff = (c(6)*(-M(152)+M(159)-M(162)+M(165)-M(168)+M(169)-M(170)+M(171)-M(173)+M(184)-M(198)+M(208)-M(216)+M(219)-M(222) &
    +M(230))) * den(378)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,21)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(135)-M(136)+M(155)-M(157)-M(159)+M(160)+M(195)-M(196)-M(219)+M(220)-M(243)+M(244)+M(249) &
    -M(250))) * den(412)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,22)
  Gcoeff = (c(6)*(M(131)-M(132)-M(134)+M(136)-M(155)+M(156)+M(158)-M(160)-M(195)+M(196)+M(201)-M(202)+M(225)-M(226)-M(249) &
    +M(250))) * den(412)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,23)
  Gcoeff = (c(6)*(M(132)-M(133)+M(134)-M(135)-M(156)+M(157)-M(158)+M(159)-M(201)+M(202)+M(219)-M(220)-M(225)+M(226)+M(243) &
    -M(244))) * den(412)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,24)
  Gcoeff = (c(6)*(-M(140)+M(146)+M(152)-M(154)+M(155)-M(157)-M(159)+M(160)-M(164)+M(170)+M(176)-M(178)+M(195)-M(219)-M(243) &
    +M(249))) * den(383)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,25)
  Gcoeff = (c(6)*(M(140)-M(142)-M(148)+M(154)-M(155)+M(156)+M(158)-M(160)+M(164)-M(166)-M(172)+M(178)-M(195)+M(201)+M(225) &
    -M(249))) * den(383)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,26)
  Gcoeff = (c(6)*(M(142)-M(146)+M(148)-M(152)-M(156)+M(157)-M(158)+M(159)+M(166)-M(170)+M(172)-M(176)-M(201)+M(219)-M(225) &
    +M(243))) * den(383)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,27)
  Gcoeff = (c(5)*(-M(73)+M(85)+M(97)-M(100)-M(110)+M(116)+M(122)-M(124)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(155)+M(157)+M(159) &
    -M(160)-M(195)+M(219)+M(243)-M(249))) * den(18)
  T4sum(1:70,195) = T4sum(1:70,195) + Gcoeff * G4tensor(:,1)
  Gcoeff = (c(5)*(M(73)-M(76)-M(88)+M(100)+M(110)-M(112)-M(118)+M(124)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(155)-M(156)-M(158) &
    +M(160)+M(195)-M(201)-M(225)+M(249))) * den(18)
  T4sum(1:70,195) = T4sum(1:70,195) + Gcoeff * G4tensor(:,2)
  Gcoeff = (c(5)*(M(76)-M(85)+M(88)-M(97)+M(112)-M(116)+M(118)-M(122)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(156)-M(157)+M(158) &
    -M(159)+M(201)-M(219)+M(225)-M(243))) * den(18)
  T4sum(1:70,195) = T4sum(1:70,195) + Gcoeff * G4tensor(:,3)
  Gcoeff = (c(6)*(-M(140)+M(146)+M(155)-M(157)-M(163)+M(169)-M(173)+M(174)+M(176)-M(178)+M(192)-M(216)+M(230)-M(232)-M(243) &
    +M(249))) * den(475)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,28)
  Gcoeff = (c(6)*(M(140)-M(146)-M(152)+M(154)-M(155)+M(157)+M(159)-M(160)+M(164)-M(170)-M(176)+M(178)-M(195)+M(219)+M(243) &
    -M(249))) * den(475)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,29)
  Gcoeff = (c(6)*(M(152)-M(154)-M(159)+M(160)+M(163)-M(164)-M(169)+M(170)+M(173)-M(174)-M(192)+M(195)+M(216)-M(219)-M(230) &
    +M(232))) * den(475)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,30)
  Gcoeff = (c(6)*(-M(146)+M(148)+M(157)-M(158)-M(165)+M(166)-M(167)+M(168)+M(175)-M(176)+M(198)-M(201)+M(206)-M(208)-M(240) &
    +M(243))) * den(479)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,31)
  Gcoeff = (c(6)*(-M(142)+M(146)-M(148)+M(152)+M(156)-M(157)+M(158)-M(159)-M(166)+M(170)-M(172)+M(176)+M(201)-M(219)+M(225) &
    -M(243))) * den(479)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,32)
  Gcoeff = (c(6)*(M(142)-M(152)-M(156)+M(159)+M(165)+M(167)-M(168)-M(170)+M(172)-M(175)-M(198)-M(206)+M(208)+M(219)-M(225) &
    +M(240))) * den(479)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,33)
  Gcoeff = (c(6)*(M(148)-M(158)+M(161)-M(163)-M(165)+M(166)+M(168)+M(174)-M(177)-M(182)+M(192)+M(198)-M(201)-M(208)-M(232) &
    +M(246))) * den(483)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,34)
  Gcoeff = (c(6)*(-M(148)+M(154)+M(158)-M(160)-M(161)+M(162)+M(164)-M(166)-M(171)+M(177)+M(182)-M(184)-M(195)+M(201)+M(222) &
    -M(246))) * den(483)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,35)
  Gcoeff = (c(6)*(-M(154)+M(160)-M(162)+M(163)-M(164)+M(165)-M(168)+M(171)-M(174)+M(184)-M(192)+M(195)-M(198)+M(208)-M(222) &
    +M(232))) * den(483)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,36)
  Gcoeff = (c(5)*(-M(49)+M(79)+M(91)-M(98)-M(106)-M(112)+M(114)+M(116)-M(117)-M(118)+M(120)+M(122))+c(6)*(-M(158)+M(168)+M(174) &
    -M(177)-M(182)+M(192)+M(198)-M(201))) * den(62)
  T4sum(1:70,187) = T4sum(1:70,187) + Gcoeff * G4tensor(:,7)
  Gcoeff = (c(5)*(M(49)-M(52)-M(86)+M(98)-M(104)+M(106)-M(110)+M(112)+M(117)+M(118)-M(123)-M(124))+c(6)*(M(158)-M(160)-M(171) &
    +M(177)+M(182)-M(184)-M(195)+M(201))) * den(62)
  T4sum(1:70,187) = T4sum(1:70,187) + Gcoeff * G4tensor(:,8)
  Gcoeff = (c(5)*(M(52)-M(79)+M(86)-M(91)+M(104)+M(110)-M(114)-M(116)-M(120)-M(122)+M(123)+M(124))+c(6)*(M(160)-M(168)+M(171) &
    -M(174)+M(184)-M(192)+M(195)-M(198))) * den(62)
  T4sum(1:70,187) = T4sum(1:70,187) + Gcoeff * G4tensor(:,9)
  Gcoeff = (c(5)*(-M(49)+M(79)+M(91)-M(98)-M(106)-M(112)+M(114)+M(116)-M(117)-M(118)+M(120)+M(122))+c(6)*(-M(148)-M(161)+M(163) &
    +M(165)-M(166)+M(208)+M(232)-M(246))) * den(62)
  T4sum(1:70,188) = T4sum(1:70,188) + Gcoeff * G4tensor(:,10)
  Gcoeff = (c(5)*(M(49)-M(52)-M(86)+M(98)-M(104)+M(106)-M(110)+M(112)+M(117)+M(118)-M(123)-M(124))+c(6)*(M(148)-M(154)+M(161) &
    -M(162)-M(164)+M(166)-M(222)+M(246))) * den(62)
  T4sum(1:70,188) = T4sum(1:70,188) + Gcoeff * G4tensor(:,11)
  Gcoeff = (c(5)*(M(52)-M(79)+M(86)-M(91)+M(104)+M(110)-M(114)-M(116)-M(120)-M(122)+M(123)+M(124))+c(6)*(M(154)+M(162)-M(163) &
    +M(164)-M(165)-M(208)+M(222)-M(232))) * den(62)
  T4sum(1:70,188) = T4sum(1:70,188) + Gcoeff * G4tensor(:,12)
  Gcoeff = (c(6)*(-M(140)+M(142)+M(155)-M(156)-M(161)+M(162)-M(171)+M(172)+M(177)-M(178)+M(182)-M(184)+M(222)-M(225)-M(246) &
    +M(249))) * den(486)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,37)
  Gcoeff = (c(6)*(M(140)-M(142)-M(148)+M(154)-M(155)+M(156)+M(158)-M(160)+M(164)-M(166)-M(172)+M(178)-M(195)+M(201)+M(225) &
    -M(249))) * den(486)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,38)
  Gcoeff = (c(6)*(M(148)-M(154)-M(158)+M(160)+M(161)-M(162)-M(164)+M(166)+M(171)-M(177)-M(182)+M(184)+M(195)-M(201)-M(222) &
    +M(246))) * den(486)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,39)
  Gcoeff = (c(6)*(M(142)-M(156)+M(162)+M(167)-M(169)-M(171)+M(172)+M(173)-M(175)-M(184)-M(206)+M(216)+M(222)-M(225)-M(230) &
    +M(240))) * den(490)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,40)
  Gcoeff = (c(6)*(-M(142)+M(152)+M(156)-M(159)-M(165)-M(167)+M(168)+M(170)-M(172)+M(175)+M(198)+M(206)-M(208)-M(219)+M(225) &
    -M(240))) * den(490)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,41)
  Gcoeff = (c(6)*(-M(152)+M(159)-M(162)+M(165)-M(168)+M(169)-M(170)+M(171)-M(173)+M(184)-M(198)+M(208)-M(216)+M(219)-M(222) &
    +M(230))) * den(490)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,42)
  Gcoeff = (c(5)*(-M(46)+M(67)+M(90)-M(95)-M(105)+M(108)+M(110)-M(111)-M(112)-M(118)+M(119)+M(124))+c(6)*(-M(156)+M(162)+M(173) &
    -M(175)-M(206)+M(216)+M(222)-M(225))) * den(65)
  T4sum(1:70,172) = T4sum(1:70,172) + Gcoeff * G4tensor(:,23)
  Gcoeff = (c(5)*(M(46)-M(51)-M(74)+M(95)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122))+c(6)*(M(156)-M(159)-M(165) &
    +M(175)+M(206)-M(208)-M(219)+M(225))) * den(65)
  T4sum(1:70,172) = T4sum(1:70,172) + Gcoeff * G4tensor(:,24)
  Gcoeff = (c(5)*(M(51)-M(67)+M(74)-M(90)+M(102)-M(108)-M(110)+M(116)-M(119)+M(121)+M(122)-M(124))+c(6)*(M(159)-M(162)+M(165) &
    -M(173)+M(208)-M(216)+M(219)-M(222))) * den(65)
  T4sum(1:70,172) = T4sum(1:70,172) + Gcoeff * G4tensor(:,25)
  Gcoeff = (c(5)*(-M(46)+M(67)+M(90)-M(95)-M(105)+M(108)+M(110)-M(111)-M(112)-M(118)+M(119)+M(124))+c(6)*(-M(142)-M(167)+M(169) &
    +M(171)-M(172)+M(184)+M(230)-M(240))) * den(65)
  T4sum(1:70,173) = T4sum(1:70,173) + Gcoeff * G4tensor(:,26)
  Gcoeff = (c(5)*(M(46)-M(51)-M(74)+M(95)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122))+c(6)*(M(142)-M(152)+M(167) &
    -M(168)-M(170)+M(172)-M(198)+M(240))) * den(65)
  T4sum(1:70,173) = T4sum(1:70,173) + Gcoeff * G4tensor(:,27)
  Gcoeff = (c(5)*(M(51)-M(67)+M(74)-M(90)+M(102)-M(108)-M(110)+M(116)-M(119)+M(121)+M(122)-M(124))+c(6)*(M(152)+M(168)-M(169) &
    +M(170)-M(171)-M(184)+M(198)-M(230))) * den(65)
  T4sum(1:70,173) = T4sum(1:70,173) + Gcoeff * G4tensor(:,28)
  Gcoeff = (c(6)*(-M(140)+M(155)-M(161)-M(167)+M(169)-M(173)+M(175)+M(177)-M(178)+M(182)+M(206)-M(216)+M(230)-M(240)-M(246) &
    +M(249))) * den(493)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,43)
  Gcoeff = (c(6)*(M(140)-M(146)-M(155)+M(157)+M(163)-M(169)+M(173)-M(174)-M(176)+M(178)-M(192)+M(216)-M(230)+M(232)+M(243) &
    -M(249))) * den(493)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,44)
  Gcoeff = (c(6)*(M(146)-M(157)+M(161)-M(163)+M(167)+M(174)-M(175)+M(176)-M(177)-M(182)+M(192)-M(206)-M(232)+M(240)-M(243) &
    +M(246))) * den(493)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,45)
  Gcoeff = (c(5)*(-M(45)+M(66)+M(78)-M(83)-M(103)+M(107)-M(109)-M(110)+M(112)+M(113)+M(118)-M(124))+c(6)*(-M(155)+M(161)+M(167) &
    -M(169)-M(230)+M(240)+M(246)-M(249))) * den(67)
  T4sum(1:70,175) = T4sum(1:70,175) + Gcoeff * G4tensor(:,39)
  Gcoeff = (c(5)*(M(45)-M(48)-M(71)+M(83)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124))+c(6)*(M(155)-M(157)-M(163) &
    +M(169)+M(230)-M(232)-M(243)+M(249))) * den(67)
  T4sum(1:70,175) = T4sum(1:70,175) + Gcoeff * G4tensor(:,40)
  Gcoeff = (c(5)*(M(48)-M(66)+M(71)-M(78)+M(101)-M(107)-M(112)-M(113)+M(115)+M(116)-M(118)+M(122))+c(6)*(M(157)-M(161)+M(163) &
    -M(167)+M(232)-M(240)+M(243)-M(246))) * den(67)
  T4sum(1:70,175) = T4sum(1:70,175) + Gcoeff * G4tensor(:,41)
  Gcoeff = (c(5)*(-M(45)+M(66)+M(78)-M(83)-M(103)+M(107)-M(109)-M(110)+M(112)+M(113)+M(118)-M(124))+c(6)*(-M(140)-M(173)+M(175) &
    +M(177)-M(178)+M(182)+M(206)-M(216))) * den(67)
  T4sum(1:70,176) = T4sum(1:70,176) + Gcoeff * G4tensor(:,42)
  Gcoeff = (c(5)*(M(45)-M(48)-M(71)+M(83)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124))+c(6)*(M(140)-M(146)+M(173) &
    -M(174)-M(176)+M(178)-M(192)+M(216))) * den(67)
  T4sum(1:70,176) = T4sum(1:70,176) + Gcoeff * G4tensor(:,43)
  Gcoeff = (c(5)*(M(48)-M(66)+M(71)-M(78)+M(101)-M(107)-M(112)-M(113)+M(115)+M(116)-M(118)+M(122))+c(6)*(M(146)+M(174)-M(175) &
    +M(176)-M(177)-M(182)+M(192)-M(206))) * den(67)
  T4sum(1:70,176) = T4sum(1:70,176) + Gcoeff * G4tensor(:,44)
  Gcoeff = (c(6)*(M(135)-M(136)-M(159)+M(160)-M(191)+M(193)+M(195)-M(196)+M(215)-M(217)-M(219)+M(220)-M(227)+M(228)+M(233) &
    -M(234))) * den(601)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,46)
  Gcoeff = (c(6)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(601)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,47)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(155)-M(157)+M(191)-M(193)-M(215)+M(217)+M(227)-M(228)-M(233)+M(234)-M(243)+M(244)+M(249) &
    -M(250))) * den(601)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,48)
  Gcoeff = (c(6)*(M(132)-M(135)-M(156)+M(159)-M(197)+M(199)-M(203)+M(204)+M(209)-M(210)+M(219)-M(220)-M(225)+M(226)+M(239) &
    -M(241))) * den(605)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,49)
  Gcoeff = (c(6)*(-M(132)+M(133)-M(134)+M(135)+M(156)-M(157)+M(158)-M(159)+M(201)-M(202)-M(219)+M(220)+M(225)-M(226)-M(243) &
    +M(244))) * den(605)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,50)
  Gcoeff = (c(6)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(605)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,51)
  Gcoeff = (c(6)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(609)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,52)
  Gcoeff = (c(6)*(M(131)-M(132)-M(155)+M(156)-M(179)+M(180)+M(185)-M(186)-M(221)+M(223)+M(225)-M(226)+M(245)-M(247)-M(249) &
    +M(250))) * den(609)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,53)
  Gcoeff = (c(6)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(609)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,54)
  Gcoeff = (c(5)*(-M(52)-M(64)-M(76)+M(79)+M(82)+M(85)-M(86)-M(87)-M(88)+M(91)+M(94)+M(97))+c(6)*(-M(156)-M(180)+M(209)+M(215) &
    -M(223)-M(225)+M(233)+M(239))) * den(10)
  T4sum(1:70,184) = T4sum(1:70,184) + Gcoeff * G4tensor(:,55)
  Gcoeff = (c(5)*(-M(49)+M(52)-M(61)+M(64)-M(73)+M(76)+M(86)+M(87)+M(88)-M(98)-M(99)-M(100))+c(6)*(-M(155)+M(156)-M(179)+M(180) &
    +M(223)+M(225)-M(247)-M(249))) * den(10)
  T4sum(1:70,184) = T4sum(1:70,184) + Gcoeff * G4tensor(:,56)
  Gcoeff = (c(5)*(M(49)+M(61)+M(73)-M(79)-M(82)-M(85)-M(91)-M(94)-M(97)+M(98)+M(99)+M(100))+c(6)*(M(155)+M(179)-M(209)-M(215) &
    -M(233)-M(239)+M(247)+M(249))) * den(10)
  T4sum(1:70,184) = T4sum(1:70,184) + Gcoeff * G4tensor(:,57)
  Gcoeff = (c(5)*(-M(52)-M(64)-M(76)+M(79)+M(82)+M(85)-M(86)-M(87)-M(88)+M(91)+M(94)+M(97))+c(6)*(-M(132)-M(186)+M(203)+M(217) &
    -M(221)-M(226)+M(227)+M(241))) * den(10)
  T4sum(1:70,185) = T4sum(1:70,185) + Gcoeff * G4tensor(:,13)
  Gcoeff = (c(5)*(-M(49)+M(52)-M(61)+M(64)-M(73)+M(76)+M(86)+M(87)+M(88)-M(98)-M(99)-M(100))+c(6)*(-M(131)+M(132)-M(185)+M(186) &
    +M(221)+M(226)-M(245)-M(250))) * den(10)
  T4sum(1:70,185) = T4sum(1:70,185) + Gcoeff * G4tensor(:,14)
  Gcoeff = (c(5)*(M(49)+M(61)+M(73)-M(79)-M(82)-M(85)-M(91)-M(94)-M(97)+M(98)+M(99)+M(100))+c(6)*(M(131)+M(185)-M(203)-M(217) &
    -M(227)-M(241)+M(245)+M(250))) * den(10)
  T4sum(1:70,185) = T4sum(1:70,185) + Gcoeff * G4tensor(:,15)
  Gcoeff = (c(6)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(612)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,55)
  Gcoeff = (c(6)*(M(131)-M(132)-M(134)+M(136)-M(155)+M(156)+M(158)-M(160)-M(195)+M(196)+M(201)-M(202)+M(225)-M(226)-M(249) &
    +M(250))) * den(612)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,56)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(612)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,57)
  Gcoeff = (c(6)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(616)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,58)
  Gcoeff = (c(6)*(M(133)-M(134)-M(157)+M(158)-M(197)+M(199)+M(201)-M(202)-M(203)+M(204)+M(209)-M(210)+M(239)-M(241)-M(243) &
    +M(244))) * den(616)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,59)
  Gcoeff = (c(6)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(616)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,60)
  Gcoeff = (c(5)*(-M(51)-M(63)+M(67)+M(70)+M(73)-M(74)-M(75)-M(76)-M(88)+M(90)+M(93)+M(100))+c(6)*(-M(158)+M(185)+M(191)-M(199) &
    -M(201)-M(204)+M(234)+M(245))) * den(9)
  T4sum(1:70,166) = T4sum(1:70,166) + Gcoeff * G4tensor(:,58)
  Gcoeff = (c(5)*(-M(46)+M(51)-M(58)+M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97))+c(6)*(-M(157)+M(158)+M(199)+M(201) &
    -M(203)+M(204)-M(241)-M(243))) * den(9)
  T4sum(1:70,166) = T4sum(1:70,166) + Gcoeff * G4tensor(:,59)
  Gcoeff = (c(5)*(M(46)+M(58)-M(67)-M(70)-M(73)+M(85)-M(90)-M(93)+M(95)+M(96)+M(97)-M(100))+c(6)*(M(157)-M(185)-M(191)+M(203) &
    -M(234)+M(241)+M(243)-M(245))) * den(9)
  T4sum(1:70,166) = T4sum(1:70,166) + Gcoeff * G4tensor(:,60)
  Gcoeff = (c(5)*(-M(51)-M(63)+M(67)+M(70)+M(73)-M(74)-M(75)-M(76)-M(88)+M(90)+M(93)+M(100))+c(6)*(-M(134)+M(179)+M(193)-M(197) &
    -M(202)-M(210)+M(228)+M(247))) * den(9)
  T4sum(1:70,167) = T4sum(1:70,167) + Gcoeff * G4tensor(:,29)
  Gcoeff = (c(5)*(-M(46)+M(51)-M(58)+M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97))+c(6)*(-M(133)+M(134)+M(197)+M(202) &
    -M(209)+M(210)-M(239)-M(244))) * den(9)
  T4sum(1:70,167) = T4sum(1:70,167) + Gcoeff * G4tensor(:,30)
  Gcoeff = (c(5)*(M(46)+M(58)-M(67)-M(70)-M(73)+M(85)-M(90)-M(93)+M(95)+M(96)+M(97)-M(100))+c(6)*(M(133)-M(179)-M(193)+M(209) &
    -M(228)+M(239)+M(244)-M(247))) * den(9)
  T4sum(1:70,167) = T4sum(1:70,167) + Gcoeff * G4tensor(:,31)
  Gcoeff = (c(6)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(619)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,61)
  Gcoeff = (c(6)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(619)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,62)
  Gcoeff = (c(6)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(619)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,63)
  Gcoeff = (c(5)*(-M(48)-M(60)+M(66)+M(69)-M(71)-M(72)-M(73)+M(76)+M(78)+M(81)+M(88)-M(100))+c(6)*(-M(160)+M(186)-M(193)-M(195) &
    +M(197)+M(210)+M(221)-M(228))) * den(8)
  T4sum(1:70,169) = T4sum(1:70,169) + Gcoeff * G4tensor(:,61)
  Gcoeff = (c(5)*(-M(45)+M(48)-M(57)+M(60)+M(71)+M(72)+M(73)-M(83)-M(84)-M(85)-M(97)+M(100))+c(6)*(-M(159)+M(160)+M(193)+M(195) &
    -M(217)-M(219)-M(227)+M(228))) * den(8)
  T4sum(1:70,169) = T4sum(1:70,169) + Gcoeff * G4tensor(:,62)
  Gcoeff = (c(5)*(M(45)+M(57)-M(66)-M(69)-M(76)-M(78)-M(81)+M(83)+M(84)+M(85)-M(88)+M(97))+c(6)*(M(159)-M(186)-M(197)-M(210) &
    +M(217)+M(219)-M(221)+M(227))) * den(8)
  T4sum(1:70,169) = T4sum(1:70,169) + Gcoeff * G4tensor(:,63)
  Gcoeff = (c(5)*(-M(48)-M(60)+M(66)+M(69)-M(71)-M(72)-M(73)+M(76)+M(78)+M(81)+M(88)-M(100))+c(6)*(-M(136)+M(180)-M(191)-M(196) &
    +M(199)+M(204)+M(223)-M(234))) * den(8)
  T4sum(1:70,170) = T4sum(1:70,170) + Gcoeff * G4tensor(:,45)
  Gcoeff = (c(5)*(-M(45)+M(48)-M(57)+M(60)+M(71)+M(72)+M(73)-M(83)-M(84)-M(85)-M(97)+M(100))+c(6)*(-M(135)+M(136)+M(191)+M(196) &
    -M(215)-M(220)-M(233)+M(234))) * den(8)
  T4sum(1:70,170) = T4sum(1:70,170) + Gcoeff * G4tensor(:,46)
  Gcoeff = (c(5)*(M(45)+M(57)-M(66)-M(69)-M(76)-M(78)-M(81)+M(83)+M(84)+M(85)-M(88)+M(97))+c(6)*(M(135)-M(180)-M(199)-M(204) &
    +M(215)+M(220)-M(223)+M(233))) * den(8)
  T4sum(1:70,170) = T4sum(1:70,170) + Gcoeff * G4tensor(:,47)
  Gcoeff = (c(6)*(M(131)-M(133)-M(135)+M(136)-M(140)+M(146)+M(152)-M(154)-M(164)+M(170)+M(176)-M(178)+M(196)-M(220)-M(244) &
    +M(250))) * den(356)
  T3sum(1:35,84) = T3sum(1:35,84) + Gcoeff * G3tensor(:,64)
  Gcoeff = (c(5)*(-M(25)+M(26)+M(29)-M(30)-M(37)+M(38)+M(39)-M(40)-M(45)+M(48)+M(71)-M(83))+c(6)*(-M(135)+M(136)-M(140)+M(146) &
    +M(176)-M(178)+M(196)-M(220))) * den(21)
  T4sum(1:70,88) = T4sum(1:70,88) + Gcoeff * G4tensor(:,48)
  Gcoeff = (c(5)*(-M(25)+M(26)+M(29)-M(30)-M(37)+M(38)+M(39)-M(40)-M(45)+M(48)+M(71)-M(83))+c(6)*(-M(131)+M(133)-M(152)+M(154) &
    +M(164)-M(170)+M(244)-M(250))) * den(21)
  T4sum(1:70,90) = T4sum(1:70,90) + Gcoeff * G4tensor(:,64)
  Gcoeff = (c(6)*(-M(132)+M(133)-M(134)+M(135)+M(142)-M(146)+M(148)-M(152)+M(166)-M(170)+M(172)-M(176)-M(202)+M(220)-M(226) &
    +M(244))) * den(359)
  T3sum(1:35,84) = T3sum(1:35,84) + Gcoeff * G3tensor(:,65)
  Gcoeff = (c(5)*(-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(74)-M(95))+c(6)*(-M(133)+M(134)-M(142)+M(152) &
    +M(170)-M(172)+M(202)-M(244))) * den(23)
  T4sum(1:70,85) = T4sum(1:70,85) + Gcoeff * G4tensor(:,32)
  Gcoeff = (c(5)*(-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(74)-M(95))+c(6)*(-M(132)+M(135)-M(146)+M(148) &
    +M(166)-M(176)+M(220)-M(226))) * den(23)
  T4sum(1:70,87) = T4sum(1:70,87) + Gcoeff * G4tensor(:,67)
  Gcoeff = (c(6)*(M(131)-M(132)-M(134)+M(136)-M(140)+M(142)+M(148)-M(154)-M(164)+M(166)+M(172)-M(178)+M(196)-M(202)-M(226) &
    +M(250))) * den(362)
  T3sum(1:35,84) = T3sum(1:35,84) + Gcoeff * G3tensor(:,66)
  Gcoeff = (c(5)*(-M(31)+M(32)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)+M(86)-M(98))+c(6)*(-M(131)+M(132)-M(148)+M(154) &
    +M(164)-M(166)+M(226)-M(250))) * den(27)
  T4sum(1:70,82) = T4sum(1:70,82) + Gcoeff * G4tensor(:,16)
  Gcoeff = (c(5)*(-M(31)+M(32)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)+M(86)-M(98))+c(6)*(-M(134)+M(136)-M(140)+M(142) &
    +M(172)-M(178)+M(196)-M(202))) * den(27)
  T4sum(1:70,84) = T4sum(1:70,84) + Gcoeff * G4tensor(:,70)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42) &
    +M(43)-M(44)+M(45)+M(46)-M(47)-M(48)+M(49)-M(50)-M(51)-M(52)-M(56)-M(59)-M(62)+M(68)-M(71)-M(74)+M(80)+M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(131)-M(154)-M(164)+M(250))) * den(11)
  T5sum(1:126,173) = T5sum(1:126,173) + Gcoeff * G5tensor(:,7)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42) &
    +M(43)-M(44)+M(45)+M(46)-M(47)-M(48)-M(49)-M(50)-M(51)+M(52)-M(56)-M(59)-M(62)+M(68)-M(71)-M(74)+M(80)+M(83)+M(86)+M(92)+M(95) &
    -M(98))+c(6)*(M(132)-M(148)-M(166)+M(226))) * den(11)
  T5sum(1:126,174) = T5sum(1:126,174) + Gcoeff * G5tensor(:,10)
  Gcoeff = (c(4)*(-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42) &
    +M(43)-M(44)-M(45)+M(46)-M(47)+M(48)+M(49)-M(50)-M(51)-M(52)-M(56)-M(59)-M(62)+M(68)+M(71)-M(74)+M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(133)-M(152)-M(170)+M(244))) * den(11)
  T5sum(1:126,177) = T5sum(1:126,177) + Gcoeff * G5tensor(:,18)
  Gcoeff = (c(4)*(-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(31)-M(32)+M(33)-M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42) &
    +M(43)-M(44)-M(45)-M(46)-M(47)+M(48)+M(49)-M(50)+M(51)-M(52)-M(56)-M(59)-M(62)+M(68)+M(71)+M(74)+M(80)-M(83)-M(86)+M(92)-M(95) &
    +M(98))+c(6)*(M(134)-M(142)-M(172)+M(202))) * den(11)
  T5sum(1:126,178) = T5sum(1:126,178) + Gcoeff * G5tensor(:,21)
  Gcoeff = (c(4)*(M(25)-M(26)-M(27)+M(28)-M(29)+M(30)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42) &
    +M(43)-M(44)+M(45)-M(46)-M(47)-M(48)-M(49)-M(50)+M(51)+M(52)-M(56)-M(59)-M(62)+M(68)-M(71)+M(74)+M(80)+M(83)+M(86)+M(92)-M(95) &
    -M(98))+c(6)*(M(135)-M(146)-M(176)+M(220))) * den(11)
  T5sum(1:126,179) = T5sum(1:126,179) + Gcoeff * G5tensor(:,29)
  Gcoeff = (c(4)*(-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42) &
    +M(43)-M(44)-M(45)-M(46)-M(47)+M(48)-M(49)-M(50)+M(51)+M(52)-M(56)-M(59)-M(62)+M(68)+M(71)+M(74)+M(80)-M(83)+M(86)+M(92)-M(95) &
    -M(98))+c(6)*(M(136)-M(140)-M(178)+M(196))) * den(11)
  T5sum(1:126,180) = T5sum(1:126,180) + Gcoeff * G5tensor(:,32)
  Gcoeff = (c(6)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(902)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,67)
  Gcoeff = (c(5)*(M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(73)+M(76)+M(88)-M(100))+c(6)*(M(134)-M(136)-M(179)+M(180) &
    -M(196)+M(202)+M(223)-M(247))) * den(81)
  T4sum(1:70,46) = T4sum(1:70,46) + Gcoeff * G4tensor(:,71)
  Gcoeff = (c(5)*(M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(73)+M(76)+M(88)-M(100))+c(6)*(M(158)-M(160)-M(185)+M(186) &
    -M(195)+M(201)+M(221)-M(245))) * den(81)
  T4sum(1:70,47) = T4sum(1:70,47) + Gcoeff * G4tensor(:,73)
  Gcoeff = (c(6)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(1200)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,68)
  Gcoeff = (c(5)*(-M(48)-M(60)+M(66)+M(69)-M(71)-M(72)-M(73)+M(76)+M(78)+M(81)+M(88)-M(100))+c(6)*(-M(160)+M(186)-M(193)-M(195) &
    +M(197)+M(210)+M(221)-M(228))) * den(190)
  T4sum(1:70,169) = T4sum(1:70,169) + Gcoeff * G4tensor(:,74)
  Gcoeff = (c(5)*(-M(48)-M(60)+M(66)+M(69)-M(71)-M(72)-M(73)+M(76)+M(78)+M(81)+M(88)-M(100))+c(6)*(-M(136)+M(180)-M(191)-M(196) &
    +M(199)+M(204)+M(223)-M(234))) * den(190)
  T4sum(1:70,170) = T4sum(1:70,170) + Gcoeff * G4tensor(:,49)
  Gcoeff = (c(6)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(1203)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,69)
  Gcoeff = (c(5)*(-M(51)-M(63)+M(67)+M(70)+M(73)-M(74)-M(75)-M(76)-M(88)+M(90)+M(93)+M(100))+c(6)*(-M(158)+M(185)+M(191)-M(199) &
    -M(201)-M(204)+M(234)+M(245))) * den(398)
  T4sum(1:70,166) = T4sum(1:70,166) + Gcoeff * G4tensor(:,75)
  Gcoeff = (c(5)*(-M(51)-M(63)+M(67)+M(70)+M(73)-M(74)-M(75)-M(76)-M(88)+M(90)+M(93)+M(100))+c(6)*(-M(134)+M(179)+M(193)-M(197) &
    -M(202)-M(210)+M(228)+M(247))) * den(398)
  T4sum(1:70,167) = T4sum(1:70,167) + Gcoeff * G4tensor(:,33)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)+M(17)-M(18)-M(19)+M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(41)+M(48)-M(51) &
    +M(53)+M(60)-M(63)+M(65)-M(66)+M(67)-M(68)-M(69)+M(70)+M(71)+M(72)+M(73)-M(74)-M(75)-M(76)-M(78)-M(81)-M(88)+M(90)+M(93) &
    +M(100))+c(6)*(M(191)-M(199)-M(204)+M(234))) * den(40)
  T5sum(1:126,145) = T5sum(1:126,145) + Gcoeff * G5tensor(:,35)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)+M(17)-M(18)-M(19)+M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(41)+M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)-M(66)-M(67)-M(68)-M(69)-M(70)+M(71)+M(72)-M(73)+M(74)+M(75)+M(76)-M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(158)-M(185)+M(201)-M(245))) * den(40)
  T5sum(1:126,146) = T5sum(1:126,146) + Gcoeff * G5tensor(:,43)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)-M(48)+M(51) &
    +M(53)-M(60)+M(63)+M(65)+M(66)-M(67)-M(68)+M(69)-M(70)-M(71)-M(72)-M(73)+M(74)+M(75)+M(76)+M(78)+M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(-M(193)+M(197)+M(210)-M(228))) * den(40)
  T5sum(1:126,147) = T5sum(1:126,147) + Gcoeff * G5tensor(:,24)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)+M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)-M(66)-M(67)-M(68)-M(69)-M(70)+M(71)+M(72)+M(73)+M(74)+M(75)-M(76)-M(78)-M(81)-M(88)-M(90)-M(93) &
    +M(100))+c(6)*(M(160)-M(186)+M(195)-M(221))) * den(40)
  T5sum(1:126,148) = T5sum(1:126,148) + Gcoeff * G5tensor(:,44)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)-M(48)-M(51) &
    +M(53)-M(60)-M(63)+M(65)+M(66)+M(67)-M(68)+M(69)+M(70)-M(71)-M(72)+M(73)-M(74)-M(75)-M(76)+M(78)+M(81)-M(88)+M(90)+M(93) &
    +M(100))+c(6)*(-M(134)+M(179)-M(202)+M(247))) * den(40)
  T5sum(1:126,149) = T5sum(1:126,149) + Gcoeff * G5tensor(:,22)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)+M(17)-M(18)-M(19)+M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(41)-M(48)-M(51) &
    +M(53)-M(60)-M(63)+M(65)+M(66)+M(67)-M(68)+M(69)+M(70)-M(71)-M(72)-M(73)-M(74)-M(75)+M(76)+M(78)+M(81)+M(88)+M(90)+M(93) &
    -M(100))+c(6)*(-M(136)+M(180)-M(196)+M(223))) * den(40)
  T5sum(1:126,150) = T5sum(1:126,150) + Gcoeff * G5tensor(:,33)
  Gcoeff = (c(6)*(-M(140)+M(142)+M(155)-M(156)-M(161)+M(162)-M(171)+M(172)+M(177)-M(178)+M(182)-M(184)+M(222)-M(225)-M(246) &
    +M(249))) * den(842)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,70)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(31)+M(32)+M(35)-M(36)-M(110)+M(112)+M(118)-M(124))+c(6)*(-M(140)+M(142)-M(171)+M(172) &
    +M(177)-M(178)+M(182)-M(184))) * den(114)
  T4sum(1:70,10) = T4sum(1:70,10) + Gcoeff * G4tensor(:,72)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(31)+M(32)+M(35)-M(36)-M(110)+M(112)+M(118)-M(124))+c(6)*(-M(155)+M(156)+M(161)-M(162) &
    -M(222)+M(225)+M(246)-M(249))) * den(114)
  T4sum(1:70,11) = T4sum(1:70,11) + Gcoeff * G4tensor(:,76)
  Gcoeff = (c(6)*(-M(140)+M(155)-M(161)-M(167)+M(169)-M(173)+M(175)+M(177)-M(178)+M(182)+M(206)-M(216)+M(230)-M(240)-M(246) &
    +M(249))) * den(1213)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,71)
  Gcoeff = (c(5)*(-M(45)+M(66)+M(78)-M(83)-M(103)+M(107)-M(109)-M(110)+M(112)+M(113)+M(118)-M(124))+c(6)*(-M(155)+M(161)+M(167) &
    -M(169)-M(230)+M(240)+M(246)-M(249))) * den(168)
  T4sum(1:70,175) = T4sum(1:70,175) + Gcoeff * G4tensor(:,77)
  Gcoeff = (c(5)*(-M(45)+M(66)+M(78)-M(83)-M(103)+M(107)-M(109)-M(110)+M(112)+M(113)+M(118)-M(124))+c(6)*(-M(140)-M(173)+M(175) &
    +M(177)-M(178)+M(182)+M(206)-M(216))) * den(168)
  T4sum(1:70,176) = T4sum(1:70,176) + Gcoeff * G4tensor(:,50)
  Gcoeff = (c(6)*(M(142)-M(156)+M(162)+M(167)-M(169)-M(171)+M(172)+M(173)-M(175)-M(184)-M(206)+M(216)+M(222)-M(225)-M(230) &
    +M(240))) * den(1215)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,72)
  Gcoeff = (c(5)*(-M(46)+M(67)+M(90)-M(95)-M(105)+M(108)+M(110)-M(111)-M(112)-M(118)+M(119)+M(124))+c(6)*(-M(156)+M(162)+M(173) &
    -M(175)-M(206)+M(216)+M(222)-M(225))) * den(371)
  T4sum(1:70,172) = T4sum(1:70,172) + Gcoeff * G4tensor(:,78)
  Gcoeff = (c(5)*(-M(46)+M(67)+M(90)-M(95)-M(105)+M(108)+M(110)-M(111)-M(112)-M(118)+M(119)+M(124))+c(6)*(-M(142)-M(167)+M(169) &
    +M(171)-M(172)+M(184)+M(230)-M(240))) * den(371)
  T4sum(1:70,173) = T4sum(1:70,173) + Gcoeff * G4tensor(:,34)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(31)+M(32)+M(35)-M(36)-M(44)+M(45)-M(46) &
    +M(53)-M(56)+M(65)-M(66)+M(67)-M(78)+M(83)+M(90)-M(95)+M(103)-M(105)-M(107)+M(108)+M(109)+M(110)-M(111)-M(112)-M(113)-M(118) &
    +M(119)+M(124))+c(6)*(M(173)-M(175)-M(206)+M(216))) * den(33)
  T5sum(1:126,97) = T5sum(1:126,97) + Gcoeff * G5tensor(:,36)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(31)+M(32)+M(35)-M(36)-M(44)+M(45)+M(46) &
    +M(53)-M(56)+M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)-M(110)+M(111)+M(112)-M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(156)-M(162)-M(222)+M(225))) * den(33)
  T5sum(1:126,98) = T5sum(1:126,98) + Gcoeff * G5tensor(:,45)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)-M(45)+M(46) &
    +M(53)-M(56)+M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)-M(103)+M(105)+M(107)-M(108)-M(109)-M(110)+M(111)+M(112)+M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(167)-M(169)-M(230)+M(240))) * den(33)
  T5sum(1:126,99) = T5sum(1:126,99) + Gcoeff * G5tensor(:,25)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)+M(45)+M(46) &
    +M(53)-M(56)+M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)+M(110)+M(111)-M(112)-M(113)-M(118) &
    -M(119)+M(124))+c(6)*(M(155)-M(161)-M(246)+M(249))) * den(33)
  T5sum(1:126,100) = T5sum(1:126,100) + Gcoeff * G5tensor(:,46)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)-M(45)-M(46) &
    +M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95)-M(103)-M(105)+M(107)+M(108)-M(109)+M(110)-M(111)-M(112)+M(113)-M(118) &
    +M(119)+M(124))+c(6)*(-M(142)+M(171)-M(172)+M(184))) * den(33)
  T5sum(1:126,101) = T5sum(1:126,101) + Gcoeff * G5tensor(:,23)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(31)+M(32)+M(35)-M(36)-M(44)-M(45)-M(46) &
    +M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95)-M(103)-M(105)+M(107)+M(108)-M(109)-M(110)-M(111)+M(112)+M(113)+M(118) &
    +M(119)-M(124))+c(6)*(-M(140)+M(177)-M(178)+M(182))) * den(33)
  T5sum(1:126,102) = T5sum(1:126,102) + Gcoeff * G5tensor(:,34)
  Gcoeff = (c(6)*(M(132)-M(135)-M(156)+M(159)-M(197)+M(199)-M(203)+M(204)+M(209)-M(210)+M(219)-M(220)-M(225)+M(226)+M(239) &
    -M(241))) * den(916)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,73)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(76)-M(85)+M(88)-M(97))+c(6)*(M(132)-M(135)+M(199)-M(203) &
    +M(204)-M(220)+M(226)-M(241))) * den(86)
  T4sum(1:70,49) = T4sum(1:70,49) + Gcoeff * G4tensor(:,68)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(76)-M(85)+M(88)-M(97))+c(6)*(M(156)-M(159)+M(197)-M(209) &
    +M(210)-M(219)+M(225)-M(239))) * den(86)
  T4sum(1:70,50) = T4sum(1:70,50) + Gcoeff * G4tensor(:,79)
  Gcoeff = (c(6)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(1236)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,74)
  Gcoeff = (c(5)*(M(45)+M(57)-M(66)-M(69)-M(76)-M(78)-M(81)+M(83)+M(84)+M(85)-M(88)+M(97))+c(6)*(M(159)-M(186)-M(197)-M(210) &
    +M(217)+M(219)-M(221)+M(227))) * den(195)
  T4sum(1:70,169) = T4sum(1:70,169) + Gcoeff * G4tensor(:,80)
  Gcoeff = (c(5)*(M(45)+M(57)-M(66)-M(69)-M(76)-M(78)-M(81)+M(83)+M(84)+M(85)-M(88)+M(97))+c(6)*(M(135)-M(180)-M(199)-M(204) &
    +M(215)+M(220)-M(223)+M(233))) * den(195)
  T4sum(1:70,170) = T4sum(1:70,170) + Gcoeff * G4tensor(:,51)
  Gcoeff = (c(6)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(1239)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,75)
  Gcoeff = (c(5)*(-M(52)-M(64)-M(76)+M(79)+M(82)+M(85)-M(86)-M(87)-M(88)+M(91)+M(94)+M(97))+c(6)*(-M(156)-M(180)+M(209)+M(215) &
    -M(223)-M(225)+M(233)+M(239))) * den(415)
  T4sum(1:70,184) = T4sum(1:70,184) + Gcoeff * G4tensor(:,81)
  Gcoeff = (c(5)*(-M(52)-M(64)-M(76)+M(79)+M(82)+M(85)-M(86)-M(87)-M(88)+M(91)+M(94)+M(97))+c(6)*(-M(132)-M(186)+M(203)+M(217) &
    -M(221)-M(226)+M(227)+M(241))) * den(415)
  T4sum(1:70,185) = T4sum(1:70,185) + Gcoeff * G4tensor(:,17)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(11)-M(12)+M(19)-M(20)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(42)+M(45)-M(52) &
    +M(54)+M(57)-M(64)-M(66)-M(69)-M(76)+M(77)-M(78)+M(79)-M(80)-M(81)+M(82)+M(83)+M(84)+M(85)-M(86)-M(87)-M(88)+M(91)+M(94) &
    +M(97))+c(6)*(-M(180)+M(215)-M(223)+M(233))) * den(43)
  T5sum(1:126,133) = T5sum(1:126,133) + Gcoeff * G5tensor(:,37)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(11)-M(12)+M(19)-M(20)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(42)+M(45)+M(52) &
    +M(54)+M(57)+M(64)-M(66)-M(69)+M(76)+M(77)-M(78)-M(79)-M(80)-M(81)-M(82)+M(83)+M(84)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94) &
    -M(97))+c(6)*(M(156)-M(209)+M(225)-M(239))) * den(43)
  T5sum(1:126,134) = T5sum(1:126,134) + Gcoeff * G5tensor(:,47)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(42)-M(45)+M(52) &
    +M(54)-M(57)+M(64)+M(66)+M(69)+M(76)+M(77)+M(78)-M(79)-M(80)+M(81)-M(82)-M(83)-M(84)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94) &
    -M(97))+c(6)*(M(186)-M(217)+M(221)-M(227))) * den(43)
  T5sum(1:126,135) = T5sum(1:126,135) + Gcoeff * G5tensor(:,13)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(42)+M(45)+M(52) &
    +M(54)+M(57)+M(64)-M(66)-M(69)-M(76)+M(77)-M(78)-M(79)-M(80)-M(81)-M(82)+M(83)+M(84)+M(85)+M(86)+M(87)-M(88)-M(91)-M(94) &
    +M(97))+c(6)*(M(159)-M(197)-M(210)+M(219))) * den(43)
  T5sum(1:126,136) = T5sum(1:126,136) + Gcoeff * G5tensor(:,48)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(42)-M(45)-M(52) &
    +M(54)-M(57)-M(64)+M(66)+M(69)-M(76)+M(77)+M(78)+M(79)-M(80)+M(81)+M(82)-M(83)-M(84)+M(85)-M(86)-M(87)-M(88)+M(91)+M(94) &
    +M(97))+c(6)*(-M(132)+M(203)-M(226)+M(241))) * den(43)
  T5sum(1:126,137) = T5sum(1:126,137) + Gcoeff * G5tensor(:,11)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(11)-M(12)+M(19)-M(20)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(42)-M(45)-M(52) &
    +M(54)-M(57)-M(64)+M(66)+M(69)+M(76)+M(77)+M(78)+M(79)-M(80)+M(81)+M(82)-M(83)-M(84)-M(85)-M(86)-M(87)+M(88)+M(91)+M(94) &
    -M(97))+c(6)*(-M(135)+M(199)+M(204)-M(220))) * den(43)
  T5sum(1:126,138) = T5sum(1:126,138) + Gcoeff * G5tensor(:,30)
  Gcoeff = (c(6)*(-M(146)+M(148)+M(157)-M(158)-M(165)+M(166)-M(167)+M(168)+M(175)-M(176)+M(198)-M(201)+M(206)-M(208)-M(240) &
    +M(243))) * den(856)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,76)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(27)+M(28)+M(33)-M(34)+M(112)-M(116)+M(118)-M(122))+c(6)*(-M(146)+M(148)-M(165)+M(166) &
    +M(175)-M(176)+M(206)-M(208))) * den(117)
  T4sum(1:70,13) = T4sum(1:70,13) + Gcoeff * G4tensor(:,69)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(27)+M(28)+M(33)-M(34)+M(112)-M(116)+M(118)-M(122))+c(6)*(-M(157)+M(158)+M(167)-M(168) &
    -M(198)+M(201)+M(240)-M(243))) * den(117)
  T4sum(1:70,14) = T4sum(1:70,14) + Gcoeff * G4tensor(:,82)
  Gcoeff = (c(6)*(M(146)-M(157)+M(161)-M(163)+M(167)+M(174)-M(175)+M(176)-M(177)-M(182)+M(192)-M(206)-M(232)+M(240)-M(243) &
    +M(246))) * den(1249)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,77)
  Gcoeff = (c(5)*(M(48)-M(66)+M(71)-M(78)+M(101)-M(107)-M(112)-M(113)+M(115)+M(116)-M(118)+M(122))+c(6)*(M(157)-M(161)+M(163) &
    -M(167)+M(232)-M(240)+M(243)-M(246))) * den(173)
  T4sum(1:70,175) = T4sum(1:70,175) + Gcoeff * G4tensor(:,83)
  Gcoeff = (c(5)*(M(48)-M(66)+M(71)-M(78)+M(101)-M(107)-M(112)-M(113)+M(115)+M(116)-M(118)+M(122))+c(6)*(M(146)+M(174)-M(175) &
    +M(176)-M(177)-M(182)+M(192)-M(206))) * den(173)
  T4sum(1:70,176) = T4sum(1:70,176) + Gcoeff * G4tensor(:,52)
  Gcoeff = (c(6)*(M(148)-M(158)+M(161)-M(163)-M(165)+M(166)+M(168)+M(174)-M(177)-M(182)+M(192)+M(198)-M(201)-M(208)-M(232) &
    +M(246))) * den(1251)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,78)
  Gcoeff = (c(5)*(-M(49)+M(79)+M(91)-M(98)-M(106)-M(112)+M(114)+M(116)-M(117)-M(118)+M(120)+M(122))+c(6)*(-M(158)+M(168)+M(174) &
    -M(177)-M(182)+M(192)+M(198)-M(201))) * den(387)
  T4sum(1:70,187) = T4sum(1:70,187) + Gcoeff * G4tensor(:,84)
  Gcoeff = (c(5)*(-M(49)+M(79)+M(91)-M(98)-M(106)-M(112)+M(114)+M(116)-M(117)-M(118)+M(120)+M(122))+c(6)*(-M(148)-M(161)+M(163) &
    +M(165)-M(166)+M(208)+M(232)-M(246))) * den(387)
  T4sum(1:70,188) = T4sum(1:70,188) + Gcoeff * G4tensor(:,18)
  Gcoeff = (c(4)*(M(3)-M(4)-M(9)+M(10)-M(13)+M(14)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)-M(27)+M(28)+M(33)-M(34)-M(47)+M(48)-M(49) &
    +M(54)-M(59)-M(66)+M(71)+M(77)-M(78)+M(79)+M(91)-M(98)+M(101)-M(106)-M(107)-M(112)-M(113)+M(114)+M(115)+M(116)-M(117)-M(118) &
    +M(120)+M(122))+c(6)*(M(174)-M(177)-M(182)+M(192))) * den(35)
  T5sum(1:126,85) = T5sum(1:126,85) + Gcoeff * G5tensor(:,38)
  Gcoeff = (c(4)*(M(3)-M(4)-M(9)+M(10)-M(13)+M(14)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)-M(27)+M(28)+M(33)-M(34)-M(47)+M(48)+M(49) &
    +M(54)-M(59)-M(66)+M(71)+M(77)-M(78)-M(79)-M(91)+M(98)+M(101)+M(106)-M(107)+M(112)-M(113)-M(114)+M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(M(158)-M(168)-M(198)+M(201))) * den(35)
  T5sum(1:126,86) = T5sum(1:126,86) + Gcoeff * G5tensor(:,49)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)-M(47)-M(48)+M(49) &
    +M(54)-M(59)+M(66)-M(71)+M(77)+M(78)-M(79)-M(91)+M(98)-M(101)+M(106)+M(107)+M(112)+M(113)-M(114)-M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(M(161)-M(163)-M(232)+M(246))) * den(35)
  T5sum(1:126,87) = T5sum(1:126,87) + Gcoeff * G5tensor(:,14)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)-M(47)+M(48)+M(49) &
    +M(54)-M(59)-M(66)+M(71)+M(77)-M(78)-M(79)-M(91)+M(98)+M(101)+M(106)-M(107)-M(112)-M(113)-M(114)+M(115)+M(116)+M(117)-M(118) &
    -M(120)+M(122))+c(6)*(M(157)-M(167)-M(240)+M(243))) * den(35)
  T5sum(1:126,88) = T5sum(1:126,88) + Gcoeff * G5tensor(:,50)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)-M(47)-M(48)-M(49) &
    +M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98)-M(101)-M(106)+M(107)-M(112)+M(113)+M(114)-M(115)+M(116)-M(117)-M(118) &
    +M(120)+M(122))+c(6)*(-M(148)+M(165)-M(166)+M(208))) * den(35)
  T5sum(1:126,89) = T5sum(1:126,89) + Gcoeff * G5tensor(:,12)
  Gcoeff = (c(4)*(M(3)-M(4)-M(9)+M(10)-M(13)+M(14)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)-M(27)+M(28)+M(33)-M(34)-M(47)-M(48)-M(49) &
    +M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98)-M(101)-M(106)+M(107)+M(112)+M(113)+M(114)-M(115)-M(116)-M(117)+M(118) &
    +M(120)-M(122))+c(6)*(-M(146)+M(175)-M(176)+M(206))) * den(35)
  T5sum(1:126,90) = T5sum(1:126,90) + Gcoeff * G5tensor(:,31)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(155)-M(157)+M(191)-M(193)-M(215)+M(217)+M(227)-M(228)-M(233)+M(234)-M(243)+M(244)+M(249) &
    -M(250))) * den(929)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,79)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)+M(73)-M(85)-M(97)+M(100))+c(6)*(M(131)-M(133)+M(193)-M(217) &
    -M(227)+M(228)-M(244)+M(250))) * den(88)
  T4sum(1:70,52) = T4sum(1:70,52) + Gcoeff * G4tensor(:,65)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)+M(73)-M(85)-M(97)+M(100))+c(6)*(M(155)-M(157)+M(191)-M(215) &
    -M(233)+M(234)-M(243)+M(249))) * den(88)
  T4sum(1:70,53) = T4sum(1:70,53) + Gcoeff * G4tensor(:,85)
  Gcoeff = (c(6)*(M(152)-M(154)-M(159)+M(160)+M(163)-M(164)-M(169)+M(170)+M(173)-M(174)-M(192)+M(195)+M(216)-M(219)-M(230) &
    +M(232))) * den(869)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,80)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)-M(25)+M(26)+M(29)-M(30)+M(110)-M(116)-M(122)+M(124))+c(6)*(-M(152)+M(154)-M(163)+M(164) &
    +M(169)-M(170)+M(230)-M(232))) * den(119)
  T4sum(1:70,16) = T4sum(1:70,16) + Gcoeff * G4tensor(:,66)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)-M(25)+M(26)+M(29)-M(30)+M(110)-M(116)-M(122)+M(124))+c(6)*(-M(159)+M(160)+M(173)-M(174) &
    -M(192)+M(195)+M(216)-M(219))) * den(119)
  T4sum(1:70,17) = T4sum(1:70,17) + Gcoeff * G4tensor(:,86)
  Gcoeff = (c(6)*(M(135)-M(136)-M(159)+M(160)-M(191)+M(193)+M(195)-M(196)+M(215)-M(217)-M(219)+M(220)-M(227)+M(228)+M(233) &
    -M(234))) * den(1268)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,81)
  Gcoeff = (c(5)*(M(45)-M(48)+M(57)-M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100))+c(6)*(M(159)-M(160)-M(193)-M(195) &
    +M(217)+M(219)+M(227)-M(228))) * den(202)
  T4sum(1:70,169) = T4sum(1:70,169) + Gcoeff * G4tensor(:,87)
  Gcoeff = (c(5)*(M(45)-M(48)+M(57)-M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100))+c(6)*(M(135)-M(136)-M(191)-M(196) &
    +M(215)+M(220)+M(233)-M(234))) * den(202)
  T4sum(1:70,170) = T4sum(1:70,170) + Gcoeff * G4tensor(:,53)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(135)-M(136)+M(155)-M(157)-M(159)+M(160)+M(195)-M(196)-M(219)+M(220)-M(243)+M(244)+M(249) &
    -M(250))) * den(1269)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,82)
  Gcoeff = (c(6)*(-M(140)+M(146)+M(155)-M(157)-M(163)+M(169)-M(173)+M(174)+M(176)-M(178)+M(192)-M(216)+M(230)-M(232)-M(243) &
    +M(249))) * den(1272)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,83)
  Gcoeff = (c(5)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(6)*(-M(155)+M(157)+M(163) &
    -M(169)-M(230)+M(232)+M(243)-M(249))) * den(178)
  T4sum(1:70,175) = T4sum(1:70,175) + Gcoeff * G4tensor(:,88)
  Gcoeff = (c(5)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(6)*(-M(140)+M(146)-M(173) &
    +M(174)+M(176)-M(178)+M(192)-M(216))) * den(178)
  T4sum(1:70,176) = T4sum(1:70,176) + Gcoeff * G4tensor(:,54)
  Gcoeff = (c(6)*(-M(140)+M(146)+M(152)-M(154)+M(155)-M(157)-M(159)+M(160)-M(164)+M(170)+M(176)-M(178)+M(195)-M(219)-M(243) &
    +M(249))) * den(1273)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,84)
  Gcoeff = (c(5)*(-M(73)+M(85)+M(97)-M(100)-M(110)+M(116)+M(122)-M(124)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(155)+M(157)+M(159) &
    -M(160)-M(195)+M(219)+M(243)-M(249))) * den(355)
  T4sum(1:70,195) = T4sum(1:70,195) + Gcoeff * G4tensor(:,4)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(37)+M(38)+M(39)-M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)+M(73)+M(83)+M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(155)-M(157)-M(243)+M(249))) * den(20)
  T5sum(1:126,31) = T5sum(1:126,31) + Gcoeff * G5tensor(:,1)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(37)+M(38)+M(39)-M(40)-M(45)+M(48)+M(57) &
    -M(60)+M(71)-M(72)+M(73)-M(83)+M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(163)-M(169)-M(230)+M(232))) * den(20)
  T5sum(1:126,32) = T5sum(1:126,32) + Gcoeff * G5tensor(:,40)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(37)+M(38)+M(39)-M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100)-M(101)+M(103)+M(109)-M(110)-M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(159)-M(160)-M(195)+M(219))) * den(20)
  T5sum(1:126,33) = T5sum(1:126,33) + Gcoeff * G5tensor(:,2)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(25)+M(26)+M(29)-M(30)-M(37)+M(38)+M(39)-M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(173)-M(174)-M(192)+M(216))) * den(20)
  T5sum(1:126,34) = T5sum(1:126,34) + Gcoeff * G5tensor(:,39)
  Gcoeff = (c(6)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(1284)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,85)
  Gcoeff = (c(5)*(M(46)+M(58)-M(67)-M(70)-M(73)+M(85)-M(90)-M(93)+M(95)+M(96)+M(97)-M(100))+c(6)*(M(157)-M(185)-M(191)+M(203) &
    -M(234)+M(241)+M(243)-M(245))) * den(204)
  T4sum(1:70,166) = T4sum(1:70,166) + Gcoeff * G4tensor(:,89)
  Gcoeff = (c(5)*(M(46)+M(58)-M(67)-M(70)-M(73)+M(85)-M(90)-M(93)+M(95)+M(96)+M(97)-M(100))+c(6)*(M(133)-M(179)-M(193)+M(209) &
    -M(228)+M(239)+M(244)-M(247))) * den(204)
  T4sum(1:70,167) = T4sum(1:70,167) + Gcoeff * G4tensor(:,35)
  Gcoeff = (c(6)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(1287)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,86)
  Gcoeff = (c(5)*(M(49)+M(61)+M(73)-M(79)-M(82)-M(85)-M(91)-M(94)-M(97)+M(98)+M(99)+M(100))+c(6)*(M(155)+M(179)-M(209)-M(215) &
    -M(233)-M(239)+M(247)+M(249))) * den(418)
  T4sum(1:70,184) = T4sum(1:70,184) + Gcoeff * G4tensor(:,90)
  Gcoeff = (c(5)*(M(49)+M(61)+M(73)-M(79)-M(82)-M(85)-M(91)-M(94)-M(97)+M(98)+M(99)+M(100))+c(6)*(M(131)+M(185)-M(203)-M(217) &
    -M(227)-M(241)+M(245)+M(250))) * den(418)
  T4sum(1:70,185) = T4sum(1:70,185) + Gcoeff * G4tensor(:,19)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)-M(43)+M(46)-M(49) &
    +M(55)+M(58)-M(61)-M(67)-M(70)-M(73)+M(79)+M(82)+M(85)+M(89)-M(90)+M(91)-M(92)-M(93)+M(94)+M(95)+M(96)+M(97)-M(98)-M(99) &
    -M(100))+c(6)*(-M(179)+M(209)+M(239)-M(247))) * den(45)
  T5sum(1:126,121) = T5sum(1:126,121) + Gcoeff * G5tensor(:,26)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)-M(43)+M(46)+M(49) &
    +M(55)+M(58)+M(61)-M(67)-M(70)+M(73)-M(79)-M(82)-M(85)+M(89)-M(90)-M(91)-M(92)-M(93)-M(94)+M(95)+M(96)-M(97)+M(98)+M(99) &
    +M(100))+c(6)*(M(155)-M(215)-M(233)+M(249))) * den(45)
  T5sum(1:126,122) = T5sum(1:126,122) + Gcoeff * G5tensor(:,51)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)-M(43)-M(46)+M(49) &
    +M(55)-M(58)+M(61)+M(67)+M(70)+M(73)-M(79)-M(82)-M(85)+M(89)+M(90)-M(91)-M(92)+M(93)-M(94)-M(95)-M(96)-M(97)+M(98)+M(99) &
    +M(100))+c(6)*(M(185)-M(203)-M(241)+M(245))) * den(45)
  T5sum(1:126,123) = T5sum(1:126,123) + Gcoeff * G5tensor(:,15)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)-M(43)+M(46)+M(49) &
    +M(55)+M(58)+M(61)-M(67)-M(70)-M(73)-M(79)-M(82)+M(85)+M(89)-M(90)-M(91)-M(92)-M(93)-M(94)+M(95)+M(96)+M(97)+M(98)+M(99) &
    -M(100))+c(6)*(M(157)-M(191)-M(234)+M(243))) * den(45)
  T5sum(1:126,124) = T5sum(1:126,124) + Gcoeff * G5tensor(:,52)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)-M(43)-M(46)-M(49) &
    +M(55)-M(58)-M(61)+M(67)+M(70)-M(73)+M(79)+M(82)+M(85)+M(89)+M(90)+M(91)-M(92)+M(93)+M(94)-M(95)-M(96)+M(97)-M(98)-M(99) &
    -M(100))+c(6)*(-M(131)+M(217)+M(227)-M(250))) * den(45)
  T5sum(1:126,125) = T5sum(1:126,125) + Gcoeff * G5tensor(:,8)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)-M(43)-M(46)-M(49) &
    +M(55)-M(58)-M(61)+M(67)+M(70)+M(73)+M(79)+M(82)-M(85)+M(89)+M(90)+M(91)-M(92)+M(93)+M(94)-M(95)-M(96)-M(97)-M(98)-M(99) &
    +M(100))+c(6)*(-M(133)+M(193)+M(228)-M(244))) * den(45)
  T5sum(1:126,126) = T5sum(1:126,126) + Gcoeff * G5tensor(:,19)
  Gcoeff = (c(6)*(-M(152)+M(159)-M(162)+M(165)-M(168)+M(169)-M(170)+M(171)-M(173)+M(184)-M(198)+M(208)-M(216)+M(219)-M(222) &
    +M(230))) * den(1297)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,87)
  Gcoeff = (c(5)*(M(51)-M(67)+M(74)-M(90)+M(102)-M(108)-M(110)+M(116)-M(119)+M(121)+M(122)-M(124))+c(6)*(M(159)-M(162)+M(165) &
    -M(173)+M(208)-M(216)+M(219)-M(222))) * den(181)
  T4sum(1:70,172) = T4sum(1:70,172) + Gcoeff * G4tensor(:,91)
  Gcoeff = (c(5)*(M(51)-M(67)+M(74)-M(90)+M(102)-M(108)-M(110)+M(116)-M(119)+M(121)+M(122)-M(124))+c(6)*(M(152)+M(168)-M(169) &
    +M(170)-M(171)-M(184)+M(198)-M(230))) * den(181)
  T4sum(1:70,173) = T4sum(1:70,173) + Gcoeff * G4tensor(:,36)
  Gcoeff = (c(6)*(-M(154)+M(160)-M(162)+M(163)-M(164)+M(165)-M(168)+M(171)-M(174)+M(184)-M(192)+M(195)-M(198)+M(208)-M(222) &
    +M(232))) * den(1299)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,88)
  Gcoeff = (c(5)*(M(52)-M(79)+M(86)-M(91)+M(104)+M(110)-M(114)-M(116)-M(120)-M(122)+M(123)+M(124))+c(6)*(M(160)-M(168)+M(171) &
    -M(174)+M(184)-M(192)+M(195)-M(198))) * den(390)
  T4sum(1:70,187) = T4sum(1:70,187) + Gcoeff * G4tensor(:,92)
  Gcoeff = (c(5)*(M(52)-M(79)+M(86)-M(91)+M(104)+M(110)-M(114)-M(116)-M(120)-M(122)+M(123)+M(124))+c(6)*(M(154)+M(162)-M(163) &
    +M(164)-M(165)-M(208)+M(222)-M(232))) * den(390)
  T4sum(1:70,188) = T4sum(1:70,188) + Gcoeff * G4tensor(:,20)
  Gcoeff = (c(4)*(M(5)-M(6)-M(11)+M(12)-M(17)+M(18)-M(19)+M(20)+M(21)-M(22)+M(23)-M(24)-M(25)+M(26)+M(29)-M(30)-M(50)+M(51)-M(52) &
    +M(55)-M(62)-M(67)+M(74)+M(79)-M(86)+M(89)-M(90)+M(91)+M(102)-M(104)-M(108)-M(110)+M(114)+M(116)-M(119)+M(120)+M(121)+M(122) &
    -M(123)-M(124))+c(6)*(M(168)-M(171)-M(184)+M(198))) * den(37)
  T5sum(1:126,73) = T5sum(1:126,73) + Gcoeff * G5tensor(:,27)
  Gcoeff = (c(4)*(M(5)-M(6)-M(11)+M(12)-M(17)+M(18)-M(19)+M(20)+M(21)-M(22)+M(23)-M(24)-M(25)+M(26)+M(29)-M(30)-M(50)+M(51)+M(52) &
    +M(55)-M(62)-M(67)+M(74)-M(79)+M(86)+M(89)-M(90)-M(91)+M(102)+M(104)-M(108)+M(110)-M(114)-M(116)-M(119)-M(120)+M(121)-M(122) &
    +M(123)+M(124))+c(6)*(M(160)-M(174)-M(192)+M(195))) * den(37)
  T5sum(1:126,74) = T5sum(1:126,74) + Gcoeff * G5tensor(:,53)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(11)+M(12)-M(17)+M(18)+M(19)-M(20)+M(21)-M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(50)-M(51)+M(52) &
    +M(55)-M(62)+M(67)-M(74)-M(79)+M(86)+M(89)+M(90)-M(91)-M(102)+M(104)+M(108)+M(110)-M(114)-M(116)+M(119)-M(120)-M(121)-M(122) &
    +M(123)+M(124))+c(6)*(M(162)-M(165)-M(208)+M(222))) * den(37)
  T5sum(1:126,75) = T5sum(1:126,75) + Gcoeff * G5tensor(:,16)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(11)+M(12)-M(17)+M(18)+M(19)-M(20)+M(21)-M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(50)+M(51)+M(52) &
    +M(55)-M(62)-M(67)+M(74)-M(79)+M(86)+M(89)-M(90)-M(91)+M(102)+M(104)-M(108)-M(110)-M(114)+M(116)-M(119)-M(120)+M(121)+M(122) &
    +M(123)-M(124))+c(6)*(M(159)-M(173)-M(216)+M(219))) * den(37)
  T5sum(1:126,76) = T5sum(1:126,76) + Gcoeff * G5tensor(:,54)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(11)+M(12)-M(17)+M(18)+M(19)-M(20)+M(21)-M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)-M(102)-M(104)+M(108)-M(110)+M(114)+M(116)+M(119)+M(120)-M(121)+M(122) &
    -M(123)-M(124))+c(6)*(-M(154)+M(163)-M(164)+M(232))) * den(37)
  T5sum(1:126,77) = T5sum(1:126,77) + Gcoeff * G5tensor(:,9)
  Gcoeff = (c(4)*(M(5)-M(6)-M(11)+M(12)-M(17)+M(18)-M(19)+M(20)+M(21)-M(22)+M(23)-M(24)-M(25)+M(26)+M(29)-M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)-M(102)-M(104)+M(108)+M(110)+M(114)-M(116)+M(119)+M(120)-M(121)-M(122) &
    -M(123)+M(124))+c(6)*(-M(152)+M(169)-M(170)+M(230))) * den(37)
  T5sum(1:126,78) = T5sum(1:126,78) + Gcoeff * G5tensor(:,20)
  Gcoeff = (c(6)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(1316)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,89)
  Gcoeff = (c(5)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(6)*(M(157)-M(158)-M(199)-M(201) &
    +M(203)-M(204)+M(241)+M(243))) * den(208)
  T4sum(1:70,166) = T4sum(1:70,166) + Gcoeff * G4tensor(:,93)
  Gcoeff = (c(5)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(6)*(M(133)-M(134)-M(197)-M(202) &
    +M(209)-M(210)+M(239)+M(244))) * den(208)
  T4sum(1:70,167) = T4sum(1:70,167) + Gcoeff * G4tensor(:,37)
  Gcoeff = (c(6)*(M(132)-M(133)+M(134)-M(135)-M(156)+M(157)-M(158)+M(159)-M(201)+M(202)+M(219)-M(220)-M(225)+M(226)+M(243) &
    -M(244))) * den(1317)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,90)
  Gcoeff = (c(6)*(M(142)-M(152)-M(156)+M(159)+M(165)+M(167)-M(168)-M(170)+M(172)-M(175)-M(198)-M(206)+M(208)+M(219)-M(225) &
    +M(240))) * den(1320)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,91)
  Gcoeff = (c(5)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(6)*(-M(156)+M(159)+M(165) &
    -M(175)-M(206)+M(208)+M(219)-M(225))) * den(184)
  T4sum(1:70,172) = T4sum(1:70,172) + Gcoeff * G4tensor(:,94)
  Gcoeff = (c(5)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(6)*(-M(142)+M(152)-M(167) &
    +M(168)+M(170)-M(172)+M(198)-M(240))) * den(184)
  T4sum(1:70,173) = T4sum(1:70,173) + Gcoeff * G4tensor(:,38)
  Gcoeff = (c(6)*(M(142)-M(146)+M(148)-M(152)-M(156)+M(157)-M(158)+M(159)+M(166)-M(170)+M(172)-M(176)-M(201)+M(219)-M(225) &
    +M(243))) * den(1321)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,92)
  Gcoeff = (c(5)*(M(76)-M(85)+M(88)-M(97)+M(112)-M(116)+M(118)-M(122)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(156)-M(157)+M(158) &
    -M(159)+M(201)-M(219)+M(225)-M(243))) * den(358)
  T4sum(1:70,195) = T4sum(1:70,195) + Gcoeff * G4tensor(:,5)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(37)-M(38)-M(39)+M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)+M(76)-M(85)+M(88)+M(95)+M(96)-M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(156)-M(159)-M(219)+M(225))) * den(22)
  T5sum(1:126,19) = T5sum(1:126,19) + Gcoeff * G5tensor(:,3)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(58) &
    -M(63)+M(74)-M(75)+M(76)-M(85)+M(88)-M(95)+M(96)-M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(165)-M(175)-M(206)+M(208))) * den(22)
  T5sum(1:126,20) = T5sum(1:126,20) + Gcoeff * G5tensor(:,41)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(37)-M(38)-M(39)+M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)-M(102)+M(105)+M(111)-M(112)+M(116)-M(118)-M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(157)-M(158)-M(201)+M(243))) * den(22)
  T5sum(1:126,21) = T5sum(1:126,21) + Gcoeff * G5tensor(:,4)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(167)-M(168)-M(198)+M(240))) * den(22)
  T5sum(1:126,22) = T5sum(1:126,22) + Gcoeff * G5tensor(:,28)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(1328)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,93)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(134)-M(136)+M(155)-M(156)-M(158)+M(160)+M(195)-M(196)-M(201)+M(202)-M(225)+M(226)+M(249) &
    -M(250))) * den(1329)
  T3sum(1:35,73) = T3sum(1:35,73) + Gcoeff * G3tensor(:,94)
  Gcoeff = (c(5)*(M(49)-M(52)+M(61)-M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100))+c(6)*(M(155)-M(156)+M(179)-M(180) &
    -M(223)-M(225)+M(247)+M(249))) * den(211)
  T4sum(1:70,184) = T4sum(1:70,184) + Gcoeff * G4tensor(:,95)
  Gcoeff = (c(5)*(M(49)-M(52)+M(61)-M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100))+c(6)*(M(131)-M(132)+M(185)-M(186) &
    -M(221)-M(226)+M(245)+M(250))) * den(211)
  T4sum(1:70,185) = T4sum(1:70,185) + Gcoeff * G4tensor(:,21)
  Gcoeff = (c(6)*(M(148)-M(154)-M(158)+M(160)+M(161)-M(162)-M(164)+M(166)+M(171)-M(177)-M(182)+M(184)+M(195)-M(201)-M(222) &
    +M(246))) * den(1332)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,95)
  Gcoeff = (c(6)*(-M(140)+M(142)+M(148)-M(154)+M(155)-M(156)-M(158)+M(160)-M(164)+M(166)+M(172)-M(178)+M(195)-M(201)-M(225) &
    +M(249))) * den(1333)
  T3sum(1:35,53) = T3sum(1:35,53) + Gcoeff * G3tensor(:,96)
  Gcoeff = (c(5)*(-M(49)+M(52)+M(86)-M(98)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124))+c(6)*(-M(158)+M(160)+M(171) &
    -M(177)-M(182)+M(184)+M(195)-M(201))) * den(187)
  T4sum(1:70,187) = T4sum(1:70,187) + Gcoeff * G4tensor(:,96)
  Gcoeff = (c(5)*(-M(49)+M(52)+M(86)-M(98)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124))+c(6)*(-M(148)+M(154)-M(161) &
    +M(162)+M(164)-M(166)+M(222)-M(246))) * den(187)
  T4sum(1:70,188) = T4sum(1:70,188) + Gcoeff * G4tensor(:,22)
  Gcoeff = (c(5)*(-M(73)+M(76)+M(88)-M(100)-M(110)+M(112)+M(118)-M(124)-M(125)+M(126)+M(128)-M(130))+c(6)*(-M(155)+M(156)+M(158) &
    -M(160)-M(195)+M(201)+M(225)-M(249))) * den(361)
  T4sum(1:70,195) = T4sum(1:70,195) + Gcoeff * G4tensor(:,6)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)+M(49)-M(52)+M(61) &
    -M(64)-M(73)+M(76)-M(86)-M(87)+M(88)+M(98)+M(99)-M(100)-M(104)+M(106)-M(110)+M(112)+M(117)+M(118)-M(123)-M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(158)-M(160)-M(195)+M(201))) * den(26)
  T5sum(1:126,7) = T5sum(1:126,7) + Gcoeff * G5tensor(:,5)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)+M(61) &
    -M(64)-M(73)+M(76)+M(86)-M(87)+M(88)-M(98)+M(99)-M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(171)-M(177)-M(182)+M(184))) * den(26)
  T5sum(1:126,8) = T5sum(1:126,8) + Gcoeff * G5tensor(:,42)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)+M(49)-M(52)+M(61) &
    -M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100)-M(104)+M(106)+M(110)-M(112)+M(117)-M(118)-M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(155)-M(156)-M(225)+M(249))) * den(26)
  T5sum(1:126,9) = T5sum(1:126,9) + Gcoeff * G5tensor(:,6)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)+M(49)-M(52)+M(61) &
    -M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100)-M(104)+M(106)-M(110)+M(112)+M(117)+M(118)-M(123)-M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(161)-M(162)-M(222)+M(246))) * den(26)
  T5sum(1:126,10) = T5sum(1:126,10) + Gcoeff * G5tensor(:,17)
  Gcoeff = (c(4)*(M(1)+M(4)+M(6)+M(8)+M(9)+M(11)+M(13)+M(16)+M(18)+M(19)+M(22)+M(23)+M(25)+M(27)+M(30)+M(32)+M(34)+M(35)+M(37) &
    +M(40)-M(48)-M(49)-M(51)-M(53)-M(54)-M(55)-M(60)-M(61)-M(63)-M(65)-M(71)-M(72)-M(74)-M(75)-M(76)-M(77)-M(88)-M(89)-M(98)-M(99) &
    -M(101)-M(102)-M(106)-M(112)-M(115)-M(117)-M(118)-M(121)-M(126)-M(128))+c(6)*(-M(158)-M(201)))
  T6sum(1:210,73) = T6sum(1:210,73) + Gcoeff * G6tensor(:,3)
  Gcoeff = (c(4)*(M(2)+M(4)+M(6)+M(7)+M(9)+M(11)+M(13)+M(16)+M(17)+M(19)+M(22)+M(24)+M(25)+M(27)+M(30)+M(31)+M(34)+M(36)+M(38) &
    +M(39)-M(48)-M(51)-M(52)-M(53)-M(54)-M(55)-M(60)-M(63)-M(64)-M(65)-M(71)-M(72)-M(73)-M(74)-M(75)-M(77)-M(86)-M(87)-M(89) &
    -M(100)-M(101)-M(102)-M(104)-M(110)-M(115)-M(121)-M(123)-M(124)-M(125)-M(130))+c(6)*(-M(160)-M(195)))
  T6sum(1:210,76) = T6sum(1:210,76) + Gcoeff * G6tensor(:,1)
  Gcoeff = (c(4)*(M(2)+M(3)+M(5)+M(7)+M(10)+M(12)+M(14)+M(15)+M(17)+M(20)+M(21)+M(24)+M(26)+M(28)+M(29)+M(31)+M(33)+M(36)+M(38) &
    +M(39)-M(47)-M(49)-M(50)-M(53)-M(59)-M(60)-M(62)-M(63)-M(64)-M(65)-M(66)-M(67)-M(72)-M(73)-M(75)-M(78)-M(87)-M(90)-M(98) &
    -M(100)-M(106)-M(107)-M(108)-M(112)-M(113)-M(117)-M(118)-M(119)-M(125)-M(130))+c(6)*(-M(177)-M(182)))
  T6sum(1:210,79) = T6sum(1:210,79) + Gcoeff * G6tensor(:,16)
  Gcoeff = (c(4)*(M(1)+M(3)+M(5)+M(8)+M(10)+M(12)+M(14)+M(15)+M(18)+M(20)+M(21)+M(23)+M(26)+M(28)+M(29)+M(32)+M(33)+M(35)+M(37) &
    +M(40)-M(47)-M(50)-M(52)-M(53)-M(59)-M(60)-M(61)-M(62)-M(63)-M(65)-M(66)-M(67)-M(72)-M(75)-M(76)-M(78)-M(86)-M(88)-M(90)-M(99) &
    -M(104)-M(107)-M(108)-M(110)-M(113)-M(119)-M(123)-M(124)-M(126)-M(128))+c(6)*(-M(171)-M(184)))
  T6sum(1:210,82) = T6sum(1:210,82) + Gcoeff * G6tensor(:,12)
  Gcoeff = (c(4)*(M(1)+M(3)+M(6)+M(8)+M(10)+M(11)+M(14)+M(15)+M(17)+M(19)+M(22)+M(24)+M(25)+M(28)+M(30)+M(32)+M(33)+M(35)+M(37) &
    +M(40)-M(47)-M(51)-M(53)-M(55)-M(59)-M(60)-M(61)-M(63)-M(65)-M(66)-M(72)-M(74)-M(75)-M(76)-M(78)-M(79)-M(88)-M(89)-M(91)-M(99) &
    -M(102)-M(107)-M(113)-M(114)-M(116)-M(120)-M(121)-M(122)-M(126)-M(128))+c(6)*(-M(168)-M(198)))
  T6sum(1:210,85) = T6sum(1:210,85) + Gcoeff * G6tensor(:,13)
  Gcoeff = (c(4)*(M(2)+M(4)+M(5)+M(7)+M(9)+M(12)+M(13)+M(16)+M(18)+M(20)+M(21)+M(23)+M(26)+M(27)+M(29)+M(31)+M(34)+M(36)+M(38) &
    +M(39)-M(48)-M(50)-M(53)-M(54)-M(60)-M(62)-M(63)-M(64)-M(65)-M(67)-M(71)-M(72)-M(73)-M(75)-M(77)-M(79)-M(87)-M(90)-M(91) &
    -M(100)-M(101)-M(108)-M(114)-M(115)-M(116)-M(119)-M(120)-M(122)-M(125)-M(130))+c(6)*(-M(174)-M(192)))
  T6sum(1:210,88) = T6sum(1:210,88) + Gcoeff * G6tensor(:,17)
  Gcoeff = (c(4)*(M(2)+M(3)+M(5)+M(7)+M(10)+M(12)+M(14)+M(15)+M(17)+M(20)+M(21)+M(24)+M(26)+M(28)+M(29)+M(31)+M(33)+M(36)+M(38) &
    +M(39)-M(45)-M(46)-M(52)-M(53)-M(54)-M(55)-M(57)-M(58)-M(64)-M(65)-M(76)-M(77)-M(83)-M(84)-M(86)-M(87)-M(88)-M(89)-M(95)-M(96) &
    -M(103)-M(104)-M(105)-M(109)-M(111)-M(112)-M(118)-M(123)-M(126)-M(128))+c(6)*(-M(156)-M(225)))
  T6sum(1:210,91) = T6sum(1:210,91) + Gcoeff * G6tensor(:,5)
  Gcoeff = (c(4)*(M(2)+M(4)+M(5)+M(7)+M(10)+M(11)+M(13)+M(15)+M(17)+M(20)+M(22)+M(24)+M(26)+M(27)+M(29)+M(31)+M(34)+M(36)+M(37) &
    +M(40)-M(45)-M(51)-M(52)-M(53)-M(54)-M(55)-M(57)-M(63)-M(64)-M(65)-M(74)-M(75)-M(77)-M(83)-M(84)-M(85)-M(86)-M(87)-M(89)-M(97) &
    -M(102)-M(103)-M(104)-M(109)-M(116)-M(121)-M(122)-M(123)-M(127)-M(129))+c(6)*(-M(159)-M(219)))
  T6sum(1:210,94) = T6sum(1:210,94) + Gcoeff * G6tensor(:,2)
  Gcoeff = (c(4)*(M(1)+M(4)+M(6)+M(8)+M(9)+M(11)+M(13)+M(16)+M(18)+M(19)+M(22)+M(23)+M(25)+M(27)+M(30)+M(32)+M(34)+M(35)+M(37) &
    +M(40)-M(44)-M(46)-M(50)-M(54)-M(56)-M(57)-M(62)-M(63)-M(64)-M(66)-M(75)-M(77)-M(78)-M(79)-M(84)-M(85)-M(87)-M(91)-M(95)-M(97) &
    -M(105)-M(107)-M(111)-M(112)-M(113)-M(114)-M(118)-M(120)-M(127)-M(129))+c(6)*(-M(175)-M(206)))
  T6sum(1:210,97) = T6sum(1:210,97) + Gcoeff * G6tensor(:,15)
  Gcoeff = (c(4)*(M(1)+M(3)+M(5)+M(8)+M(9)+M(11)+M(14)+M(16)+M(17)+M(20)+M(22)+M(24)+M(26)+M(28)+M(29)+M(32)+M(33)+M(35)+M(38) &
    +M(39)-M(44)-M(52)-M(54)-M(55)-M(56)-M(57)-M(58)-M(64)-M(66)-M(67)-M(76)-M(77)-M(78)-M(84)-M(86)-M(87)-M(88)-M(89)-M(90)-M(96) &
    -M(104)-M(107)-M(108)-M(110)-M(113)-M(119)-M(123)-M(124)-M(126)-M(128))+c(6)*(-M(162)-M(222)))
  T6sum(1:210,100) = T6sum(1:210,100) + Gcoeff * G6tensor(:,9)
  Gcoeff = (c(4)*(M(2)+M(4)+M(6)+M(7)+M(10)+M(12)+M(13)+M(15)+M(18)+M(19)+M(21)+M(23)+M(25)+M(27)+M(30)+M(31)+M(34)+M(36)+M(37) &
    +M(40)-M(45)-M(50)-M(53)-M(54)-M(57)-M(62)-M(63)-M(64)-M(65)-M(67)-M(75)-M(77)-M(79)-M(83)-M(84)-M(85)-M(87)-M(90)-M(91)-M(97) &
    -M(103)-M(108)-M(109)-M(110)-M(114)-M(119)-M(120)-M(124)-M(127)-M(129))+c(6)*(-M(173)-M(216)))
  T6sum(1:210,103) = T6sum(1:210,103) + Gcoeff * G6tensor(:,18)
  Gcoeff = (c(4)*(M(1)+M(3)+M(6)+M(8)+M(9)+M(12)+M(14)+M(16)+M(18)+M(19)+M(21)+M(23)+M(25)+M(28)+M(30)+M(32)+M(33)+M(35)+M(38) &
    +M(39)-M(44)-M(50)-M(51)-M(54)-M(56)-M(57)-M(58)-M(62)-M(64)-M(66)-M(74)-M(76)-M(77)-M(78)-M(79)-M(84)-M(87)-M(88)-M(91)-M(96) &
    -M(102)-M(107)-M(113)-M(114)-M(116)-M(120)-M(121)-M(122)-M(126)-M(128))+c(6)*(-M(165)-M(208)))
  T6sum(1:210,106) = T6sum(1:210,106) + Gcoeff * G6tensor(:,8)
  Gcoeff = (c(4)*(M(1)+M(3)+M(5)+M(8)+M(10)+M(12)+M(14)+M(15)+M(18)+M(20)+M(21)+M(23)+M(26)+M(28)+M(29)+M(32)+M(33)+M(35)+M(37) &
    +M(40)-M(45)-M(46)-M(49)-M(53)-M(54)-M(55)-M(57)-M(58)-M(61)-M(65)-M(73)-M(77)-M(83)-M(84)-M(89)-M(95)-M(96)-M(98)-M(99) &
    -M(100)-M(103)-M(105)-M(106)-M(109)-M(110)-M(111)-M(117)-M(124)-M(125)-M(130))+c(6)*(-M(155)-M(249)))
  T6sum(1:210,109) = T6sum(1:210,109) + Gcoeff * G6tensor(:,6)
  Gcoeff = (c(4)*(M(1)+M(3)+M(6)+M(8)+M(9)+M(12)+M(14)+M(16)+M(18)+M(19)+M(21)+M(23)+M(25)+M(28)+M(30)+M(32)+M(33)+M(35)+M(38) &
    +M(39)-M(46)-M(48)-M(49)-M(53)-M(54)-M(55)-M(58)-M(60)-M(61)-M(65)-M(71)-M(72)-M(77)-M(85)-M(89)-M(95)-M(96)-M(97)-M(98)-M(99) &
    -M(101)-M(105)-M(106)-M(111)-M(115)-M(116)-M(117)-M(122)-M(127)-M(129))+c(6)*(-M(157)-M(243)))
  T6sum(1:210,112) = T6sum(1:210,112) + Gcoeff * G6tensor(:,4)
  Gcoeff = (c(4)*(M(2)+M(3)+M(5)+M(7)+M(9)+M(11)+M(14)+M(16)+M(18)+M(20)+M(22)+M(23)+M(26)+M(28)+M(29)+M(31)+M(33)+M(36)+M(37) &
    +M(40)-M(44)-M(49)-M(54)-M(55)-M(56)-M(57)-M(58)-M(61)-M(66)-M(67)-M(73)-M(77)-M(78)-M(84)-M(89)-M(90)-M(96)-M(98)-M(99) &
    -M(100)-M(106)-M(107)-M(108)-M(112)-M(113)-M(117)-M(118)-M(119)-M(125)-M(130))+c(6)*(-M(161)-M(246)))
  T6sum(1:210,115) = T6sum(1:210,115) + Gcoeff * G6tensor(:,10)
  Gcoeff = (c(4)*(M(1)+M(4)+M(6)+M(8)+M(10)+M(12)+M(13)+M(15)+M(17)+M(19)+M(21)+M(24)+M(25)+M(27)+M(30)+M(32)+M(34)+M(35)+M(38) &
    +M(39)-M(46)-M(47)-M(53)-M(55)-M(58)-M(59)-M(60)-M(61)-M(65)-M(66)-M(72)-M(78)-M(79)-M(85)-M(89)-M(91)-M(95)-M(96)-M(97)-M(99) &
    -M(105)-M(107)-M(111)-M(112)-M(113)-M(114)-M(118)-M(120)-M(127)-M(129))+c(6)*(-M(167)-M(240)))
  T6sum(1:210,118) = T6sum(1:210,118) + Gcoeff * G6tensor(:,14)
  Gcoeff = (c(4)*(M(2)+M(4)+M(6)+M(7)+M(9)+M(11)+M(13)+M(16)+M(17)+M(19)+M(22)+M(24)+M(25)+M(27)+M(30)+M(31)+M(34)+M(36)+M(38) &
    +M(39)-M(44)-M(45)-M(47)-M(55)-M(56)-M(58)-M(59)-M(60)-M(61)-M(67)-M(72)-M(79)-M(83)-M(85)-M(89)-M(90)-M(91)-M(96)-M(97)-M(99) &
    -M(103)-M(108)-M(109)-M(110)-M(114)-M(119)-M(120)-M(124)-M(127)-M(129))+c(6)*(-M(169)-M(230)))
  T6sum(1:210,121) = T6sum(1:210,121) + Gcoeff * G6tensor(:,11)
  Gcoeff = (c(4)*(M(2)+M(4)+M(5)+M(7)+M(10)+M(11)+M(13)+M(15)+M(17)+M(20)+M(22)+M(24)+M(26)+M(27)+M(29)+M(31)+M(34)+M(36)+M(37) &
    +M(40)-M(44)-M(47)-M(48)-M(55)-M(56)-M(57)-M(58)-M(59)-M(61)-M(67)-M(71)-M(73)-M(79)-M(84)-M(89)-M(90)-M(91)-M(96)-M(99) &
    -M(100)-M(101)-M(108)-M(114)-M(115)-M(116)-M(119)-M(120)-M(122)-M(125)-M(130))+c(6)*(-M(163)-M(232)))
  T6sum(1:210,124) = T6sum(1:210,124) + Gcoeff * G6tensor(:,7)

end subroutine vamp_42

end module ol_vamp_42_ppjjjj_gggggg_1_/**/REALKIND
