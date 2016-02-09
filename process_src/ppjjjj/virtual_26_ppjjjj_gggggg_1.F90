
module ol_vamp_26_ppjjjj_gggggg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_26(M)
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
  complex(REALKIND), dimension(4,15,4,202) :: G2
  complex(REALKIND), dimension(4,35,4,149) :: G3
  complex(REALKIND), dimension(4,70,4,50) :: G4
  complex(REALKIND), dimension(4,126,4,12) :: G5
  complex(REALKIND), dimension(35,159) :: G3tensor
  complex(REALKIND), dimension(70,129) :: G4tensor
  complex(REALKIND), dimension(126,38) :: G5tensor
  complex(REALKIND), dimension(210,12) :: G6tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_DV_C(G0(:,:,:,1),Q(:,0),wf(:,-3),G1(:,:,:,1))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,63),G2(:,:,:,1))
  call check_last_DV_C(l_switch,G2(:,:,:,1),Q(:,59),wf(:,-2),G3tensor(:,1))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,64),G2(:,:,:,2))
  call check_last_DV_C(l_switch,G2(:,:,:,2),Q(:,59),wf(:,-2),G3tensor(:,2))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,65),G2(:,:,:,3))
  call check_last_DV_C(l_switch,G2(:,:,:,3),Q(:,59),wf(:,-2),G3tensor(:,3))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,8),G2(:,:,:,4))
  call check_last_DV_C(l_switch,G2(:,:,:,4),Q(:,60),wf(:,61),G3tensor(:,4))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,11),G2(:,:,:,5))
  call check_last_DV_C(l_switch,G2(:,:,:,5),Q(:,60),wf(:,61),G3tensor(:,5))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,12),G2(:,:,:,6))
  call check_last_DV_C(l_switch,G2(:,:,:,6),Q(:,60),wf(:,61),G3tensor(:,6))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,92),G2(:,:,:,7))
  call check_last_DV_C(l_switch,G2(:,:,:,7),Q(:,61),wf(:,-1),G3tensor(:,7))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,93),G2(:,:,:,8))
  call check_last_DV_C(l_switch,G2(:,:,:,8),Q(:,61),wf(:,-1),G3tensor(:,8))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,94),G2(:,:,:,9))
  call check_last_DV_C(l_switch,G2(:,:,:,9),Q(:,61),wf(:,-1),G3tensor(:,9))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,856),G2(:,:,:,10))
  call check_last_DV_C(l_switch,G2(:,:,:,10),Q(:,62),wf(:,0),G3tensor(:,10))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,858),G2(:,:,:,11))
  call check_last_DV_C(l_switch,G2(:,:,:,11),Q(:,62),wf(:,0),G3tensor(:,11))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,859),G2(:,:,:,12))
  call check_last_DV_C(l_switch,G2(:,:,:,12),Q(:,62),wf(:,0),G3tensor(:,12))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,125),G2(:,:,:,13))
  call check_last_DV_C(l_switch,G2(:,:,:,13),Q(:,61),wf(:,-1),G3tensor(:,13))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,126),G2(:,:,:,14))
  call check_last_DV_C(l_switch,G2(:,:,:,14),Q(:,61),wf(:,-1),G3tensor(:,14))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,127),G2(:,:,:,15))
  call check_last_DV_C(l_switch,G2(:,:,:,15),Q(:,61),wf(:,-1),G3tensor(:,15))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,880),G2(:,:,:,16))
  call check_last_DV_C(l_switch,G2(:,:,:,16),Q(:,62),wf(:,0),G3tensor(:,16))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,881),G2(:,:,:,17))
  call check_last_DV_C(l_switch,G2(:,:,:,17),Q(:,62),wf(:,0),G3tensor(:,17))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,882),G2(:,:,:,18))
  call check_last_DV_C(l_switch,G2(:,:,:,18),Q(:,62),wf(:,0),G3tensor(:,18))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,128),G2(:,:,:,19))
  call check_last_DV_C(l_switch,G2(:,:,:,19),Q(:,61),wf(:,-1),G3tensor(:,19))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,129),G2(:,:,:,20))
  call check_last_DV_C(l_switch,G2(:,:,:,20),Q(:,61),wf(:,-1),G3tensor(:,20))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,130),G2(:,:,:,21))
  call check_last_DV_C(l_switch,G2(:,:,:,21),Q(:,61),wf(:,-1),G3tensor(:,21))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,883),G2(:,:,:,22))
  call check_last_DV_C(l_switch,G2(:,:,:,22),Q(:,62),wf(:,0),G3tensor(:,22))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,884),G2(:,:,:,23))
  call check_last_DV_C(l_switch,G2(:,:,:,23),Q(:,62),wf(:,0),G3tensor(:,23))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,885),G2(:,:,:,24))
  call check_last_DV_C(l_switch,G2(:,:,:,24),Q(:,62),wf(:,0),G3tensor(:,24))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,895),G2(:,:,:,25))
  call check_last_DV_C(l_switch,G2(:,:,:,25),Q(:,61),wf(:,-1),G3tensor(:,25))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,896),G2(:,:,:,26))
  call check_last_DV_C(l_switch,G2(:,:,:,26),Q(:,61),wf(:,-1),G3tensor(:,26))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,897),G2(:,:,:,27))
  call check_last_DV_C(l_switch,G2(:,:,:,27),Q(:,61),wf(:,-1),G3tensor(:,27))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,886),G2(:,:,:,28))
  call check_last_DV_C(l_switch,G2(:,:,:,28),Q(:,62),wf(:,0),G3tensor(:,28))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,887),G2(:,:,:,29))
  call check_last_DV_C(l_switch,G2(:,:,:,29),Q(:,62),wf(:,0),G3tensor(:,29))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,888),G2(:,:,:,30))
  call check_last_DV_C(l_switch,G2(:,:,:,30),Q(:,62),wf(:,0),G3tensor(:,30))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,-1),G2(:,:,:,31))
  call loop_DV_C(G2(:,:,:,31),Q(:,10),wf(:,8),G3(:,:,:,1))
  call check_last_DV_C(l_switch,G3(:,:,:,1),Q(:,62),wf(:,0),G4tensor(:,1))
  call loop_DV_C(G2(:,:,:,31),Q(:,10),wf(:,11),G3(:,:,:,2))
  call check_last_DV_C(l_switch,G3(:,:,:,2),Q(:,62),wf(:,0),G4tensor(:,2))
  call loop_DV_C(G2(:,:,:,31),Q(:,10),wf(:,12),G3(:,:,:,3))
  call check_last_DV_C(l_switch,G3(:,:,:,3),Q(:,62),wf(:,0),G4tensor(:,3))
  call loop_DV_C(G2(:,:,:,31),Q(:,10),wf(:,265),G3(:,:,:,4))
  call check_last_DV_C(l_switch,G3(:,:,:,4),Q(:,62),wf(:,0),G4tensor(:,4))
  call loop_DV_C(G2(:,:,:,31),Q(:,10),wf(:,66),G3(:,:,:,5))
  call loop_DV_C(G3(:,:,:,5),Q(:,30),wf(:,-5),G4(:,:,:,1))
  call check_last_DV_C(l_switch,G4(:,:,:,1),Q(:,62),wf(:,0),G5tensor(:,1))
  call loop_DV_C(G2(:,:,:,31),Q(:,10),wf(:,-5),G3(:,:,:,6))
  call loop_DV_C(G3(:,:,:,6),Q(:,42),wf(:,66),G4(:,:,:,2))
  call check_last_DV_C(l_switch,G4(:,:,:,2),Q(:,62),wf(:,0),G5tensor(:,2))
  call loop_DV_C(G3(:,:,:,6),Q(:,42),wf(:,-2),G4(:,:,:,3))
  call loop_DV_C(G4(:,:,:,3),Q(:,46),wf(:,-4),G5(:,:,:,1))
  call check_last_DV_C(l_switch,G5(:,:,:,1),Q(:,62),wf(:,0),G6tensor(:,1))
  call loop_DV_C(G2(:,:,:,31),Q(:,10),wf(:,266),G3(:,:,:,7))
  call check_last_DV_C(l_switch,G3(:,:,:,7),Q(:,62),wf(:,0),G4tensor(:,5))
  call loop_DV_C(G2(:,:,:,31),Q(:,10),wf(:,70),G3(:,:,:,8))
  call loop_DV_C(G3(:,:,:,8),Q(:,46),wf(:,-4),G4(:,:,:,4))
  call check_last_DV_C(l_switch,G4(:,:,:,4),Q(:,62),wf(:,0),G5tensor(:,3))
  call loop_DV_C(G2(:,:,:,31),Q(:,10),wf(:,-4),G3(:,:,:,9))
  call loop_DV_C(G3(:,:,:,9),Q(:,26),wf(:,70),G4(:,:,:,5))
  call check_last_DV_C(l_switch,G4(:,:,:,5),Q(:,62),wf(:,0),G5tensor(:,4))
  call loop_DV_C(G3(:,:,:,9),Q(:,26),wf(:,-2),G4(:,:,:,6))
  call loop_DV_C(G4(:,:,:,6),Q(:,30),wf(:,-5),G5(:,:,:,2))
  call check_last_DV_C(l_switch,G5(:,:,:,2),Q(:,62),wf(:,0),G6tensor(:,2))
  call loop_DV_C(G2(:,:,:,31),Q(:,10),wf(:,267),G3(:,:,:,10))
  call check_last_DV_C(l_switch,G3(:,:,:,10),Q(:,62),wf(:,0),G4tensor(:,6))
  call loop_DV_C(G2(:,:,:,31),Q(:,10),wf(:,-2),G3(:,:,:,11))
  call loop_DV_C(G3(:,:,:,11),Q(:,14),wf(:,84),G4(:,:,:,7))
  call check_last_DV_C(l_switch,G4(:,:,:,7),Q(:,62),wf(:,0),G5tensor(:,5))
  call loop_DV_C(G3(:,:,:,11),Q(:,14),wf(:,-5),G4(:,:,:,8))
  call loop_DV_C(G4(:,:,:,8),Q(:,46),wf(:,-4),G5(:,:,:,3))
  call check_last_DV_C(l_switch,G5(:,:,:,3),Q(:,62),wf(:,0),G6tensor(:,3))
  call loop_DV_C(G3(:,:,:,11),Q(:,14),wf(:,-4),G4(:,:,:,9))
  call loop_DV_C(G4(:,:,:,9),Q(:,30),wf(:,-5),G5(:,:,:,4))
  call check_last_DV_C(l_switch,G5(:,:,:,4),Q(:,62),wf(:,0),G6tensor(:,4))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,26),G2(:,:,:,32))
  call check_last_DV_C(l_switch,G2(:,:,:,32),Q(:,58),wf(:,90),G3tensor(:,31))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,29),G2(:,:,:,33))
  call check_last_DV_C(l_switch,G2(:,:,:,33),Q(:,58),wf(:,90),G3tensor(:,32))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,30),G2(:,:,:,34))
  call check_last_DV_C(l_switch,G2(:,:,:,34),Q(:,58),wf(:,90),G3tensor(:,33))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,161),G2(:,:,:,35))
  call check_last_DV_C(l_switch,G2(:,:,:,35),Q(:,59),wf(:,-2),G3tensor(:,34))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,162),G2(:,:,:,36))
  call check_last_DV_C(l_switch,G2(:,:,:,36),Q(:,59),wf(:,-2),G3tensor(:,35))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,163),G2(:,:,:,37))
  call check_last_DV_C(l_switch,G2(:,:,:,37),Q(:,59),wf(:,-2),G3tensor(:,36))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,943),G2(:,:,:,38))
  call check_last_DV_C(l_switch,G2(:,:,:,38),Q(:,62),wf(:,0),G3tensor(:,37))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,944),G2(:,:,:,39))
  call check_last_DV_C(l_switch,G2(:,:,:,39),Q(:,62),wf(:,0),G3tensor(:,38))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,945),G2(:,:,:,40))
  call check_last_DV_C(l_switch,G2(:,:,:,40),Q(:,62),wf(:,0),G3tensor(:,39))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,164),G2(:,:,:,41))
  call check_last_DV_C(l_switch,G2(:,:,:,41),Q(:,59),wf(:,-2),G3tensor(:,40))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,165),G2(:,:,:,42))
  call check_last_DV_C(l_switch,G2(:,:,:,42),Q(:,59),wf(:,-2),G3tensor(:,41))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,166),G2(:,:,:,43))
  call check_last_DV_C(l_switch,G2(:,:,:,43),Q(:,59),wf(:,-2),G3tensor(:,42))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,946),G2(:,:,:,44))
  call check_last_DV_C(l_switch,G2(:,:,:,44),Q(:,62),wf(:,0),G3tensor(:,43))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,947),G2(:,:,:,45))
  call check_last_DV_C(l_switch,G2(:,:,:,45),Q(:,62),wf(:,0),G3tensor(:,44))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,948),G2(:,:,:,46))
  call check_last_DV_C(l_switch,G2(:,:,:,46),Q(:,62),wf(:,0),G3tensor(:,45))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,958),G2(:,:,:,47))
  call check_last_DV_C(l_switch,G2(:,:,:,47),Q(:,59),wf(:,-2),G3tensor(:,46))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,959),G2(:,:,:,48))
  call check_last_DV_C(l_switch,G2(:,:,:,48),Q(:,59),wf(:,-2),G3tensor(:,47))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,960),G2(:,:,:,49))
  call check_last_DV_C(l_switch,G2(:,:,:,49),Q(:,59),wf(:,-2),G3tensor(:,48))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,949),G2(:,:,:,50))
  call check_last_DV_C(l_switch,G2(:,:,:,50),Q(:,62),wf(:,0),G3tensor(:,49))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,950),G2(:,:,:,51))
  call check_last_DV_C(l_switch,G2(:,:,:,51),Q(:,62),wf(:,0),G3tensor(:,50))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,951),G2(:,:,:,52))
  call check_last_DV_C(l_switch,G2(:,:,:,52),Q(:,62),wf(:,0),G3tensor(:,51))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,-2),G2(:,:,:,53))
  call loop_DV_C(G2(:,:,:,53),Q(:,12),wf(:,26),G3(:,:,:,12))
  call check_last_DV_C(l_switch,G3(:,:,:,12),Q(:,62),wf(:,0),G4tensor(:,7))
  call loop_DV_C(G2(:,:,:,53),Q(:,12),wf(:,29),G3(:,:,:,13))
  call check_last_DV_C(l_switch,G3(:,:,:,13),Q(:,62),wf(:,0),G4tensor(:,8))
  call loop_DV_C(G2(:,:,:,53),Q(:,12),wf(:,30),G3(:,:,:,14))
  call check_last_DV_C(l_switch,G3(:,:,:,14),Q(:,62),wf(:,0),G4tensor(:,9))
  call loop_DV_C(G2(:,:,:,53),Q(:,12),wf(:,55),G3(:,:,:,15))
  call check_last_DV_C(l_switch,G3(:,:,:,15),Q(:,61),wf(:,-1),G4tensor(:,10))
  call loop_DV_C(G2(:,:,:,53),Q(:,12),wf(:,57),G3(:,:,:,16))
  call check_last_DV_C(l_switch,G3(:,:,:,16),Q(:,61),wf(:,-1),G4tensor(:,11))
  call loop_DV_C(G2(:,:,:,53),Q(:,12),wf(:,58),G3(:,:,:,17))
  call check_last_DV_C(l_switch,G3(:,:,:,17),Q(:,61),wf(:,-1),G4tensor(:,12))
  call loop_DV_C(G2(:,:,:,53),Q(:,12),wf(:,84),G3(:,:,:,18))
  call check_last_DV_C(l_switch,G3(:,:,:,18),Q(:,60),wf(:,61),G4tensor(:,13))
  call loop_DV_C(G2(:,:,:,53),Q(:,12),wf(:,-4),G3(:,:,:,19))
  call loop_DV_C(G3(:,:,:,19),Q(:,28),wf(:,-5),G4(:,:,:,10))
  call check_last_DV_C(l_switch,G4(:,:,:,10),Q(:,60),wf(:,61),G5tensor(:,6))
  call loop_DV_C(G3(:,:,:,19),Q(:,28),wf(:,113),G4(:,:,:,11))
  call check_last_DV_C(l_switch,G4(:,:,:,11),Q(:,61),wf(:,-1),G5tensor(:,7))
  call loop_DV_C(G3(:,:,:,19),Q(:,28),wf(:,99),G4(:,:,:,12))
  call check_last_DV_C(l_switch,G4(:,:,:,12),Q(:,62),wf(:,0),G5tensor(:,8))
  call loop_DV_C(G3(:,:,:,19),Q(:,28),wf(:,-1),G4(:,:,:,13))
  call loop_DV_C(G4(:,:,:,13),Q(:,30),wf(:,-5),G5(:,:,:,5))
  call check_last_DV_C(l_switch,G5(:,:,:,5),Q(:,62),wf(:,0),G6tensor(:,5))
  call loop_DV_C(G2(:,:,:,53),Q(:,12),wf(:,-5),G3(:,:,:,20))
  call loop_DV_C(G3(:,:,:,20),Q(:,44),wf(:,-4),G4(:,:,:,14))
  call check_last_DV_C(l_switch,G4(:,:,:,14),Q(:,60),wf(:,61),G5tensor(:,9))
  call loop_DV_C(G3(:,:,:,20),Q(:,44),wf(:,109),G4(:,:,:,15))
  call check_last_DV_C(l_switch,G4(:,:,:,15),Q(:,61),wf(:,-1),G5tensor(:,10))
  call loop_DV_C(G3(:,:,:,20),Q(:,44),wf(:,95),G4(:,:,:,16))
  call check_last_DV_C(l_switch,G4(:,:,:,16),Q(:,62),wf(:,0),G5tensor(:,11))
  call loop_DV_C(G3(:,:,:,20),Q(:,44),wf(:,-1),G4(:,:,:,17))
  call loop_DV_C(G4(:,:,:,17),Q(:,46),wf(:,-4),G5(:,:,:,6))
  call check_last_DV_C(l_switch,G5(:,:,:,6),Q(:,62),wf(:,0),G6tensor(:,6))
  call loop_DV_C(G2(:,:,:,53),Q(:,12),wf(:,193),G3(:,:,:,21))
  call check_last_DV_C(l_switch,G3(:,:,:,21),Q(:,61),wf(:,-1),G4tensor(:,14))
  call loop_DV_C(G2(:,:,:,53),Q(:,12),wf(:,109),G3(:,:,:,22))
  call loop_DV_C(G3(:,:,:,22),Q(:,29),wf(:,-5),G4(:,:,:,18))
  call check_last_DV_C(l_switch,G4(:,:,:,18),Q(:,61),wf(:,-1),G5tensor(:,12))
  call loop_DV_C(G2(:,:,:,53),Q(:,12),wf(:,247),G3(:,:,:,23))
  call check_last_DV_C(l_switch,G3(:,:,:,23),Q(:,62),wf(:,0),G4tensor(:,15))
  call loop_DV_C(G2(:,:,:,53),Q(:,12),wf(:,95),G3(:,:,:,24))
  call loop_DV_C(G3(:,:,:,24),Q(:,30),wf(:,-5),G4(:,:,:,19))
  call check_last_DV_C(l_switch,G4(:,:,:,19),Q(:,62),wf(:,0),G5tensor(:,13))
  call loop_DV_C(G2(:,:,:,53),Q(:,12),wf(:,194),G3(:,:,:,25))
  call check_last_DV_C(l_switch,G3(:,:,:,25),Q(:,61),wf(:,-1),G4tensor(:,16))
  call loop_DV_C(G2(:,:,:,53),Q(:,12),wf(:,113),G3(:,:,:,26))
  call loop_DV_C(G3(:,:,:,26),Q(:,45),wf(:,-4),G4(:,:,:,20))
  call check_last_DV_C(l_switch,G4(:,:,:,20),Q(:,61),wf(:,-1),G5tensor(:,14))
  call loop_DV_C(G2(:,:,:,53),Q(:,12),wf(:,248),G3(:,:,:,27))
  call check_last_DV_C(l_switch,G3(:,:,:,27),Q(:,62),wf(:,0),G4tensor(:,17))
  call loop_DV_C(G2(:,:,:,53),Q(:,12),wf(:,99),G3(:,:,:,28))
  call loop_DV_C(G3(:,:,:,28),Q(:,46),wf(:,-4),G4(:,:,:,21))
  call check_last_DV_C(l_switch,G4(:,:,:,21),Q(:,62),wf(:,0),G5tensor(:,15))
  call loop_DV_C(G2(:,:,:,53),Q(:,12),wf(:,195),G3(:,:,:,29))
  call check_last_DV_C(l_switch,G3(:,:,:,29),Q(:,61),wf(:,-1),G4tensor(:,18))
  call loop_DV_C(G2(:,:,:,53),Q(:,12),wf(:,249),G3(:,:,:,30))
  call check_last_DV_C(l_switch,G3(:,:,:,30),Q(:,62),wf(:,0),G4tensor(:,19))
  call loop_DV_C(G2(:,:,:,53),Q(:,12),wf(:,-1),G3(:,:,:,31))
  call loop_DV_C(G3(:,:,:,31),Q(:,14),wf(:,84),G4(:,:,:,22))
  call check_last_DV_C(l_switch,G4(:,:,:,22),Q(:,62),wf(:,0),G5tensor(:,16))
  call loop_DV_C(G3(:,:,:,31),Q(:,14),wf(:,-5),G4(:,:,:,23))
  call loop_DV_C(G4(:,:,:,23),Q(:,46),wf(:,-4),G5(:,:,:,7))
  call check_last_DV_C(l_switch,G5(:,:,:,7),Q(:,62),wf(:,0),G6tensor(:,7))
  call loop_DV_C(G3(:,:,:,31),Q(:,14),wf(:,-4),G4(:,:,:,24))
  call loop_DV_C(G4(:,:,:,24),Q(:,30),wf(:,-5),G5(:,:,:,8))
  call check_last_DV_C(l_switch,G5(:,:,:,8),Q(:,62),wf(:,0),G6tensor(:,8))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,979),G2(:,:,:,54))
  call check_last_DV_C(l_switch,G2(:,:,:,54),Q(:,62),wf(:,0),G3tensor(:,52))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,980),G2(:,:,:,55))
  call check_last_DV_C(l_switch,G2(:,:,:,55),Q(:,62),wf(:,0),G3tensor(:,53))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,981),G2(:,:,:,56))
  call check_last_DV_C(l_switch,G2(:,:,:,56),Q(:,62),wf(:,0),G3tensor(:,54))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,982),G2(:,:,:,57))
  call check_last_DV_C(l_switch,G2(:,:,:,57),Q(:,62),wf(:,0),G3tensor(:,55))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,983),G2(:,:,:,58))
  call check_last_DV_C(l_switch,G2(:,:,:,58),Q(:,62),wf(:,0),G3tensor(:,56))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,984),G2(:,:,:,59))
  call check_last_DV_C(l_switch,G2(:,:,:,59),Q(:,62),wf(:,0),G3tensor(:,57))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,44),G2(:,:,:,60))
  call loop_DV_C(G2(:,:,:,60),Q(:,46),wf(:,-4),G3(:,:,:,32))
  call check_last_DV_C(l_switch,G3(:,:,:,32),Q(:,62),wf(:,0),G4tensor(:,20))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,47),G2(:,:,:,61))
  call loop_DV_C(G2(:,:,:,61),Q(:,46),wf(:,-4),G3(:,:,:,33))
  call check_last_DV_C(l_switch,G3(:,:,:,33),Q(:,62),wf(:,0),G4tensor(:,21))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,48),G2(:,:,:,62))
  call loop_DV_C(G2(:,:,:,62),Q(:,46),wf(:,-4),G3(:,:,:,34))
  call check_last_DV_C(l_switch,G3(:,:,:,34),Q(:,62),wf(:,0),G4tensor(:,22))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,-4),G2(:,:,:,63))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,44),G3(:,:,:,35))
  call check_last_DV_C(l_switch,G3(:,:,:,35),Q(:,62),wf(:,0),G4tensor(:,23))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,47),G3(:,:,:,36))
  call check_last_DV_C(l_switch,G3(:,:,:,36),Q(:,62),wf(:,0),G4tensor(:,24))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,48),G3(:,:,:,37))
  call check_last_DV_C(l_switch,G3(:,:,:,37),Q(:,62),wf(:,0),G4tensor(:,25))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,37),G3(:,:,:,38))
  call check_last_DV_C(l_switch,G3(:,:,:,38),Q(:,61),wf(:,-1),G4tensor(:,26))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,39),G3(:,:,:,39))
  call check_last_DV_C(l_switch,G3(:,:,:,39),Q(:,61),wf(:,-1),G4tensor(:,27))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,40),G3(:,:,:,40))
  call check_last_DV_C(l_switch,G3(:,:,:,40),Q(:,61),wf(:,-1),G4tensor(:,28))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,19),G3(:,:,:,41))
  call check_last_DV_C(l_switch,G3(:,:,:,41),Q(:,59),wf(:,-2),G4tensor(:,29))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,21),G3(:,:,:,42))
  call check_last_DV_C(l_switch,G3(:,:,:,42),Q(:,59),wf(:,-2),G4tensor(:,30))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,22),G3(:,:,:,43))
  call check_last_DV_C(l_switch,G3(:,:,:,43),Q(:,59),wf(:,-2),G4tensor(:,31))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,-5),G3(:,:,:,44))
  call check_last_DV_C(l_switch,G3(:,:,:,44),Q(:,56),wf(:,1),G4tensor(:,32))
  call check_last_DV_C(l_switch,G3(:,:,:,44),Q(:,56),wf(:,3),G4tensor(:,33))
  call check_last_DV_C(l_switch,G3(:,:,:,44),Q(:,56),wf(:,4),G4tensor(:,34))
  call check_last_DV_C(l_switch,G3(:,:,:,44),Q(:,56),wf(:,74),G4tensor(:,35))
  call check_last_DV_C(l_switch,G3(:,:,:,44),Q(:,56),wf(:,103),G4tensor(:,36))
  call check_last_DV_C(l_switch,G3(:,:,:,44),Q(:,56),wf(:,117),G4tensor(:,37))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,70),G3(:,:,:,45))
  call check_last_DV_C(l_switch,G3(:,:,:,45),Q(:,60),wf(:,61),G4tensor(:,38))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,89),G3(:,:,:,46))
  call check_last_DV_C(l_switch,G3(:,:,:,46),Q(:,59),wf(:,-2),G4tensor(:,39))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,-2),G3(:,:,:,47))
  call loop_DV_C(G3(:,:,:,47),Q(:,28),wf(:,-5),G4(:,:,:,25))
  call check_last_DV_C(l_switch,G4(:,:,:,25),Q(:,60),wf(:,61),G5tensor(:,17))
  call loop_DV_C(G3(:,:,:,47),Q(:,28),wf(:,113),G4(:,:,:,26))
  call check_last_DV_C(l_switch,G4(:,:,:,26),Q(:,61),wf(:,-1),G5tensor(:,18))
  call loop_DV_C(G3(:,:,:,47),Q(:,28),wf(:,99),G4(:,:,:,27))
  call check_last_DV_C(l_switch,G4(:,:,:,27),Q(:,62),wf(:,0),G5tensor(:,19))
  call loop_DV_C(G3(:,:,:,47),Q(:,28),wf(:,-1),G4(:,:,:,28))
  call loop_DV_C(G4(:,:,:,28),Q(:,30),wf(:,-5),G5(:,:,:,9))
  call check_last_DV_C(l_switch,G5(:,:,:,9),Q(:,62),wf(:,0),G6tensor(:,9))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,99),G3(:,:,:,48))
  call check_last_DV_C(l_switch,G3(:,:,:,48),Q(:,58),wf(:,90),G4tensor(:,40))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,153),G3(:,:,:,49))
  call check_last_DV_C(l_switch,G3(:,:,:,49),Q(:,61),wf(:,-1),G4tensor(:,41))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,90),G3(:,:,:,50))
  call loop_DV_C(G3(:,:,:,50),Q(:,29),wf(:,-5),G4(:,:,:,29))
  call check_last_DV_C(l_switch,G4(:,:,:,29),Q(:,61),wf(:,-1),G5tensor(:,20))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,113),G3(:,:,:,51))
  call check_last_DV_C(l_switch,G3(:,:,:,51),Q(:,57),wf(:,105),G4tensor(:,42))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,207),G3(:,:,:,52))
  call check_last_DV_C(l_switch,G3(:,:,:,52),Q(:,62),wf(:,0),G4tensor(:,43))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,105),G3(:,:,:,53))
  call loop_DV_C(G3(:,:,:,53),Q(:,30),wf(:,-5),G4(:,:,:,30))
  call check_last_DV_C(l_switch,G4(:,:,:,30),Q(:,62),wf(:,0),G5tensor(:,21))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,140),G3(:,:,:,54))
  call check_last_DV_C(l_switch,G3(:,:,:,54),Q(:,59),wf(:,-2),G4tensor(:,44))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,176),G3(:,:,:,55))
  call check_last_DV_C(l_switch,G3(:,:,:,55),Q(:,61),wf(:,-1),G4tensor(:,45))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,141),G3(:,:,:,56))
  call check_last_DV_C(l_switch,G3(:,:,:,56),Q(:,59),wf(:,-2),G4tensor(:,46))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,230),G3(:,:,:,57))
  call check_last_DV_C(l_switch,G3(:,:,:,57),Q(:,62),wf(:,0),G4tensor(:,47))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,177),G3(:,:,:,58))
  call check_last_DV_C(l_switch,G3(:,:,:,58),Q(:,61),wf(:,-1),G4tensor(:,48))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,231),G3(:,:,:,59))
  call check_last_DV_C(l_switch,G3(:,:,:,59),Q(:,62),wf(:,0),G4tensor(:,49))
  call loop_DV_C(G2(:,:,:,63),Q(:,24),wf(:,-1),G3(:,:,:,60))
  call loop_DV_C(G3(:,:,:,60),Q(:,26),wf(:,70),G4(:,:,:,31))
  call check_last_DV_C(l_switch,G4(:,:,:,31),Q(:,62),wf(:,0),G5tensor(:,22))
  call loop_DV_C(G3(:,:,:,60),Q(:,26),wf(:,-2),G4(:,:,:,32))
  call loop_DV_C(G4(:,:,:,32),Q(:,30),wf(:,-5),G5(:,:,:,10))
  call check_last_DV_C(l_switch,G5(:,:,:,10),Q(:,62),wf(:,0),G6tensor(:,10))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,988),G2(:,:,:,64))
  call check_last_DV_C(l_switch,G2(:,:,:,64),Q(:,62),wf(:,0),G3tensor(:,58))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,989),G2(:,:,:,65))
  call check_last_DV_C(l_switch,G2(:,:,:,65),Q(:,62),wf(:,0),G3tensor(:,59))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,990),G2(:,:,:,66))
  call check_last_DV_C(l_switch,G2(:,:,:,66),Q(:,62),wf(:,0),G3tensor(:,60))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,50),G2(:,:,:,67))
  call loop_DV_C(G2(:,:,:,67),Q(:,30),wf(:,-5),G3(:,:,:,61))
  call check_last_DV_C(l_switch,G3(:,:,:,61),Q(:,62),wf(:,0),G4tensor(:,50))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,53),G2(:,:,:,68))
  call loop_DV_C(G2(:,:,:,68),Q(:,30),wf(:,-5),G3(:,:,:,62))
  call check_last_DV_C(l_switch,G3(:,:,:,62),Q(:,62),wf(:,0),G4tensor(:,51))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,54),G2(:,:,:,69))
  call loop_DV_C(G2(:,:,:,69),Q(:,30),wf(:,-5),G3(:,:,:,63))
  call check_last_DV_C(l_switch,G3(:,:,:,63),Q(:,62),wf(:,0),G4tensor(:,52))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,-5),G2(:,:,:,70))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,50),G3(:,:,:,64))
  call check_last_DV_C(l_switch,G3(:,:,:,64),Q(:,62),wf(:,0),G4tensor(:,53))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,53),G3(:,:,:,65))
  call check_last_DV_C(l_switch,G3(:,:,:,65),Q(:,62),wf(:,0),G4tensor(:,54))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,54),G3(:,:,:,66))
  call check_last_DV_C(l_switch,G3(:,:,:,66),Q(:,62),wf(:,0),G4tensor(:,55))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,31),G3(:,:,:,67))
  call check_last_DV_C(l_switch,G3(:,:,:,67),Q(:,61),wf(:,-1),G4tensor(:,56))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,33),G3(:,:,:,68))
  call check_last_DV_C(l_switch,G3(:,:,:,68),Q(:,61),wf(:,-1),G4tensor(:,57))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,34),G3(:,:,:,69))
  call check_last_DV_C(l_switch,G3(:,:,:,69),Q(:,61),wf(:,-1),G4tensor(:,58))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,13),G3(:,:,:,70))
  call check_last_DV_C(l_switch,G3(:,:,:,70),Q(:,59),wf(:,-2),G4tensor(:,59))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,15),G3(:,:,:,71))
  call check_last_DV_C(l_switch,G3(:,:,:,71),Q(:,59),wf(:,-2),G4tensor(:,60))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,16),G3(:,:,:,72))
  call check_last_DV_C(l_switch,G3(:,:,:,72),Q(:,59),wf(:,-2),G4tensor(:,61))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,-4),G3(:,:,:,73))
  call check_last_DV_C(l_switch,G3(:,:,:,73),Q(:,56),wf(:,1),G4tensor(:,62))
  call check_last_DV_C(l_switch,G3(:,:,:,73),Q(:,56),wf(:,3),G4tensor(:,63))
  call check_last_DV_C(l_switch,G3(:,:,:,73),Q(:,56),wf(:,4),G4tensor(:,64))
  call check_last_DV_C(l_switch,G3(:,:,:,73),Q(:,56),wf(:,74),G4tensor(:,65))
  call check_last_DV_C(l_switch,G3(:,:,:,73),Q(:,56),wf(:,103),G4tensor(:,66))
  call check_last_DV_C(l_switch,G3(:,:,:,73),Q(:,56),wf(:,117),G4tensor(:,67))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,66),G3(:,:,:,74))
  call check_last_DV_C(l_switch,G3(:,:,:,74),Q(:,60),wf(:,61),G4tensor(:,68))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,88),G3(:,:,:,75))
  call check_last_DV_C(l_switch,G3(:,:,:,75),Q(:,59),wf(:,-2),G4tensor(:,69))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,-2),G3(:,:,:,76))
  call loop_DV_C(G3(:,:,:,76),Q(:,44),wf(:,-4),G4(:,:,:,33))
  call check_last_DV_C(l_switch,G4(:,:,:,33),Q(:,60),wf(:,61),G5tensor(:,23))
  call loop_DV_C(G3(:,:,:,76),Q(:,44),wf(:,109),G4(:,:,:,34))
  call check_last_DV_C(l_switch,G4(:,:,:,34),Q(:,61),wf(:,-1),G5tensor(:,24))
  call loop_DV_C(G3(:,:,:,76),Q(:,44),wf(:,95),G4(:,:,:,35))
  call check_last_DV_C(l_switch,G4(:,:,:,35),Q(:,62),wf(:,0),G5tensor(:,25))
  call loop_DV_C(G3(:,:,:,76),Q(:,44),wf(:,-1),G4(:,:,:,36))
  call loop_DV_C(G4(:,:,:,36),Q(:,46),wf(:,-4),G5(:,:,:,11))
  call check_last_DV_C(l_switch,G5(:,:,:,11),Q(:,62),wf(:,0),G6tensor(:,11))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,95),G3(:,:,:,77))
  call check_last_DV_C(l_switch,G3(:,:,:,77),Q(:,58),wf(:,90),G4tensor(:,70))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,152),G3(:,:,:,78))
  call check_last_DV_C(l_switch,G3(:,:,:,78),Q(:,61),wf(:,-1),G4tensor(:,71))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,90),G3(:,:,:,79))
  call loop_DV_C(G3(:,:,:,79),Q(:,45),wf(:,-4),G4(:,:,:,37))
  call check_last_DV_C(l_switch,G4(:,:,:,37),Q(:,61),wf(:,-1),G5tensor(:,26))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,109),G3(:,:,:,80))
  call check_last_DV_C(l_switch,G3(:,:,:,80),Q(:,57),wf(:,105),G4tensor(:,72))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,206),G3(:,:,:,81))
  call check_last_DV_C(l_switch,G3(:,:,:,81),Q(:,62),wf(:,0),G4tensor(:,73))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,105),G3(:,:,:,82))
  call loop_DV_C(G3(:,:,:,82),Q(:,46),wf(:,-4),G4(:,:,:,38))
  call check_last_DV_C(l_switch,G4(:,:,:,38),Q(:,62),wf(:,0),G5tensor(:,27))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,135),G3(:,:,:,83))
  call check_last_DV_C(l_switch,G3(:,:,:,83),Q(:,59),wf(:,-2),G4tensor(:,74))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,171),G3(:,:,:,84))
  call check_last_DV_C(l_switch,G3(:,:,:,84),Q(:,61),wf(:,-1),G4tensor(:,75))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,139),G3(:,:,:,85))
  call check_last_DV_C(l_switch,G3(:,:,:,85),Q(:,59),wf(:,-2),G4tensor(:,76))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,225),G3(:,:,:,86))
  call check_last_DV_C(l_switch,G3(:,:,:,86),Q(:,62),wf(:,0),G4tensor(:,77))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,175),G3(:,:,:,87))
  call check_last_DV_C(l_switch,G3(:,:,:,87),Q(:,61),wf(:,-1),G4tensor(:,78))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,229),G3(:,:,:,88))
  call check_last_DV_C(l_switch,G3(:,:,:,88),Q(:,62),wf(:,0),G4tensor(:,79))
  call loop_DV_C(G2(:,:,:,70),Q(:,40),wf(:,-1),G3(:,:,:,89))
  call loop_DV_C(G3(:,:,:,89),Q(:,42),wf(:,66),G4(:,:,:,39))
  call check_last_DV_C(l_switch,G4(:,:,:,39),Q(:,62),wf(:,0),G5tensor(:,28))
  call loop_DV_C(G3(:,:,:,89),Q(:,42),wf(:,-2),G4(:,:,:,40))
  call loop_DV_C(G4(:,:,:,40),Q(:,46),wf(:,-4),G5(:,:,:,12))
  call check_last_DV_C(l_switch,G5(:,:,:,12),Q(:,62),wf(:,0),G6tensor(:,12))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,55),G2(:,:,:,71))
  call check_last_DV_C(l_switch,G2(:,:,:,71),Q(:,57),wf(:,105),G3tensor(:,61))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,57),G2(:,:,:,72))
  call check_last_DV_C(l_switch,G2(:,:,:,72),Q(:,57),wf(:,105),G3tensor(:,62))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,58),G2(:,:,:,73))
  call check_last_DV_C(l_switch,G2(:,:,:,73),Q(:,57),wf(:,105),G3tensor(:,63))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,215),G2(:,:,:,74))
  call check_last_DV_C(l_switch,G2(:,:,:,74),Q(:,59),wf(:,-2),G3tensor(:,64))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,216),G2(:,:,:,75))
  call check_last_DV_C(l_switch,G2(:,:,:,75),Q(:,59),wf(:,-2),G3tensor(:,65))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,217),G2(:,:,:,76))
  call check_last_DV_C(l_switch,G2(:,:,:,76),Q(:,59),wf(:,-2),G3tensor(:,66))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1054),G2(:,:,:,77))
  call check_last_DV_C(l_switch,G2(:,:,:,77),Q(:,61),wf(:,-1),G3tensor(:,67))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1055),G2(:,:,:,78))
  call check_last_DV_C(l_switch,G2(:,:,:,78),Q(:,61),wf(:,-1),G3tensor(:,68))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1056),G2(:,:,:,79))
  call check_last_DV_C(l_switch,G2(:,:,:,79),Q(:,61),wf(:,-1),G3tensor(:,69))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,218),G2(:,:,:,80))
  call check_last_DV_C(l_switch,G2(:,:,:,80),Q(:,59),wf(:,-2),G3tensor(:,70))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,219),G2(:,:,:,81))
  call check_last_DV_C(l_switch,G2(:,:,:,81),Q(:,59),wf(:,-2),G3tensor(:,71))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,220),G2(:,:,:,82))
  call check_last_DV_C(l_switch,G2(:,:,:,82),Q(:,59),wf(:,-2),G3tensor(:,72))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1057),G2(:,:,:,83))
  call check_last_DV_C(l_switch,G2(:,:,:,83),Q(:,61),wf(:,-1),G3tensor(:,73))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1058),G2(:,:,:,84))
  call check_last_DV_C(l_switch,G2(:,:,:,84),Q(:,61),wf(:,-1),G3tensor(:,74))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1059),G2(:,:,:,85))
  call check_last_DV_C(l_switch,G2(:,:,:,85),Q(:,61),wf(:,-1),G3tensor(:,75))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1069),G2(:,:,:,86))
  call check_last_DV_C(l_switch,G2(:,:,:,86),Q(:,59),wf(:,-2),G3tensor(:,76))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1070),G2(:,:,:,87))
  call check_last_DV_C(l_switch,G2(:,:,:,87),Q(:,59),wf(:,-2),G3tensor(:,77))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1071),G2(:,:,:,88))
  call check_last_DV_C(l_switch,G2(:,:,:,88),Q(:,59),wf(:,-2),G3tensor(:,78))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1060),G2(:,:,:,89))
  call check_last_DV_C(l_switch,G2(:,:,:,89),Q(:,61),wf(:,-1),G3tensor(:,79))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1061),G2(:,:,:,90))
  call check_last_DV_C(l_switch,G2(:,:,:,90),Q(:,61),wf(:,-1),G3tensor(:,80))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1062),G2(:,:,:,91))
  call check_last_DV_C(l_switch,G2(:,:,:,91),Q(:,61),wf(:,-1),G3tensor(:,81))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1090),G2(:,:,:,92))
  call check_last_DV_C(l_switch,G2(:,:,:,92),Q(:,61),wf(:,-1),G3tensor(:,82))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1091),G2(:,:,:,93))
  call check_last_DV_C(l_switch,G2(:,:,:,93),Q(:,61),wf(:,-1),G3tensor(:,83))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1092),G2(:,:,:,94))
  call check_last_DV_C(l_switch,G2(:,:,:,94),Q(:,61),wf(:,-1),G3tensor(:,84))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1093),G2(:,:,:,95))
  call check_last_DV_C(l_switch,G2(:,:,:,95),Q(:,61),wf(:,-1),G3tensor(:,85))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1094),G2(:,:,:,96))
  call check_last_DV_C(l_switch,G2(:,:,:,96),Q(:,61),wf(:,-1),G3tensor(:,86))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1095),G2(:,:,:,97))
  call check_last_DV_C(l_switch,G2(:,:,:,97),Q(:,61),wf(:,-1),G3tensor(:,87))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,37),G2(:,:,:,98))
  call loop_DV_C(G2(:,:,:,98),Q(:,45),wf(:,-4),G3(:,:,:,90))
  call check_last_DV_C(l_switch,G3(:,:,:,90),Q(:,61),wf(:,-1),G4tensor(:,80))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,39),G2(:,:,:,99))
  call loop_DV_C(G2(:,:,:,99),Q(:,45),wf(:,-4),G3(:,:,:,91))
  call check_last_DV_C(l_switch,G3(:,:,:,91),Q(:,61),wf(:,-1),G4tensor(:,81))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,40),G2(:,:,:,100))
  call loop_DV_C(G2(:,:,:,100),Q(:,45),wf(:,-4),G3(:,:,:,92))
  call check_last_DV_C(l_switch,G3(:,:,:,92),Q(:,61),wf(:,-1),G4tensor(:,82))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1096),G2(:,:,:,101))
  call check_last_DV_C(l_switch,G2(:,:,:,101),Q(:,61),wf(:,-1),G3tensor(:,88))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1097),G2(:,:,:,102))
  call check_last_DV_C(l_switch,G2(:,:,:,102),Q(:,61),wf(:,-1),G3tensor(:,89))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1098),G2(:,:,:,103))
  call check_last_DV_C(l_switch,G2(:,:,:,103),Q(:,61),wf(:,-1),G3tensor(:,90))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,31),G2(:,:,:,104))
  call loop_DV_C(G2(:,:,:,104),Q(:,29),wf(:,-5),G3(:,:,:,93))
  call check_last_DV_C(l_switch,G3(:,:,:,93),Q(:,61),wf(:,-1),G4tensor(:,83))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,33),G2(:,:,:,105))
  call loop_DV_C(G2(:,:,:,105),Q(:,29),wf(:,-5),G3(:,:,:,94))
  call check_last_DV_C(l_switch,G3(:,:,:,94),Q(:,61),wf(:,-1),G4tensor(:,84))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,34),G2(:,:,:,106))
  call loop_DV_C(G2(:,:,:,106),Q(:,29),wf(:,-5),G3(:,:,:,95))
  call check_last_DV_C(l_switch,G3(:,:,:,95),Q(:,61),wf(:,-1),G4tensor(:,85))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1126),G2(:,:,:,107))
  call check_last_DV_C(l_switch,G2(:,:,:,107),Q(:,59),wf(:,-2),G3tensor(:,91))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1127),G2(:,:,:,108))
  call check_last_DV_C(l_switch,G2(:,:,:,108),Q(:,59),wf(:,-2),G3tensor(:,92))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1128),G2(:,:,:,109))
  call check_last_DV_C(l_switch,G2(:,:,:,109),Q(:,59),wf(:,-2),G3tensor(:,93))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1129),G2(:,:,:,110))
  call check_last_DV_C(l_switch,G2(:,:,:,110),Q(:,59),wf(:,-2),G3tensor(:,94))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1130),G2(:,:,:,111))
  call check_last_DV_C(l_switch,G2(:,:,:,111),Q(:,59),wf(:,-2),G3tensor(:,95))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1131),G2(:,:,:,112))
  call check_last_DV_C(l_switch,G2(:,:,:,112),Q(:,59),wf(:,-2),G3tensor(:,96))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,19),G2(:,:,:,113))
  call loop_DV_C(G2(:,:,:,113),Q(:,43),wf(:,-4),G3(:,:,:,96))
  call check_last_DV_C(l_switch,G3(:,:,:,96),Q(:,59),wf(:,-2),G4tensor(:,86))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,21),G2(:,:,:,114))
  call loop_DV_C(G2(:,:,:,114),Q(:,43),wf(:,-4),G3(:,:,:,97))
  call check_last_DV_C(l_switch,G3(:,:,:,97),Q(:,59),wf(:,-2),G4tensor(:,87))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,22),G2(:,:,:,115))
  call loop_DV_C(G2(:,:,:,115),Q(:,43),wf(:,-4),G3(:,:,:,98))
  call check_last_DV_C(l_switch,G3(:,:,:,98),Q(:,59),wf(:,-2),G4tensor(:,88))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1132),G2(:,:,:,116))
  call check_last_DV_C(l_switch,G2(:,:,:,116),Q(:,59),wf(:,-2),G3tensor(:,97))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1133),G2(:,:,:,117))
  call check_last_DV_C(l_switch,G2(:,:,:,117),Q(:,59),wf(:,-2),G3tensor(:,98))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1134),G2(:,:,:,118))
  call check_last_DV_C(l_switch,G2(:,:,:,118),Q(:,59),wf(:,-2),G3tensor(:,99))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,13),G2(:,:,:,119))
  call loop_DV_C(G2(:,:,:,119),Q(:,27),wf(:,-5),G3(:,:,:,99))
  call check_last_DV_C(l_switch,G3(:,:,:,99),Q(:,59),wf(:,-2),G4tensor(:,89))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,15),G2(:,:,:,120))
  call loop_DV_C(G2(:,:,:,120),Q(:,27),wf(:,-5),G3(:,:,:,100))
  call check_last_DV_C(l_switch,G3(:,:,:,100),Q(:,59),wf(:,-2),G4tensor(:,90))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,16),G2(:,:,:,121))
  call loop_DV_C(G2(:,:,:,121),Q(:,27),wf(:,-5),G3(:,:,:,101))
  call check_last_DV_C(l_switch,G3(:,:,:,101),Q(:,59),wf(:,-2),G4tensor(:,91))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,84),G2(:,:,:,122))
  call check_last_DV_C(l_switch,G2(:,:,:,122),Q(:,56),wf(:,1),G3tensor(:,100))
  call check_last_DV_C(l_switch,G2(:,:,:,122),Q(:,56),wf(:,3),G3tensor(:,101))
  call check_last_DV_C(l_switch,G2(:,:,:,122),Q(:,56),wf(:,4),G3tensor(:,102))
  call check_last_DV_C(l_switch,G2(:,:,:,122),Q(:,56),wf(:,74),G3tensor(:,103))
  call check_last_DV_C(l_switch,G2(:,:,:,122),Q(:,56),wf(:,103),G3tensor(:,104))
  call check_last_DV_C(l_switch,G2(:,:,:,122),Q(:,56),wf(:,117),G3tensor(:,105))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,265),G2(:,:,:,123))
  call check_last_DV_C(l_switch,G2(:,:,:,123),Q(:,60),wf(:,61),G3tensor(:,106))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,66),G2(:,:,:,124))
  call loop_DV_C(G2(:,:,:,124),Q(:,28),wf(:,-5),G3(:,:,:,102))
  call check_last_DV_C(l_switch,G3(:,:,:,102),Q(:,60),wf(:,61),G4tensor(:,92))
  call loop_DV_C(G2(:,:,:,124),Q(:,28),wf(:,113),G3(:,:,:,103))
  call check_last_DV_C(l_switch,G3(:,:,:,103),Q(:,61),wf(:,-1),G4tensor(:,93))
  call loop_DV_C(G2(:,:,:,124),Q(:,28),wf(:,99),G3(:,:,:,104))
  call check_last_DV_C(l_switch,G3(:,:,:,104),Q(:,62),wf(:,0),G4tensor(:,94))
  call loop_DV_C(G2(:,:,:,124),Q(:,28),wf(:,-1),G3(:,:,:,105))
  call loop_DV_C(G3(:,:,:,105),Q(:,30),wf(:,-5),G4(:,:,:,41))
  call check_last_DV_C(l_switch,G4(:,:,:,41),Q(:,62),wf(:,0),G5tensor(:,29))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,266),G2(:,:,:,125))
  call check_last_DV_C(l_switch,G2(:,:,:,125),Q(:,60),wf(:,61),G3tensor(:,107))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,70),G2(:,:,:,126))
  call loop_DV_C(G2(:,:,:,126),Q(:,44),wf(:,-4),G3(:,:,:,106))
  call check_last_DV_C(l_switch,G3(:,:,:,106),Q(:,60),wf(:,61),G4tensor(:,95))
  call loop_DV_C(G2(:,:,:,126),Q(:,44),wf(:,109),G3(:,:,:,107))
  call check_last_DV_C(l_switch,G3(:,:,:,107),Q(:,61),wf(:,-1),G4tensor(:,96))
  call loop_DV_C(G2(:,:,:,126),Q(:,44),wf(:,95),G3(:,:,:,108))
  call check_last_DV_C(l_switch,G3(:,:,:,108),Q(:,62),wf(:,0),G4tensor(:,97))
  call loop_DV_C(G2(:,:,:,126),Q(:,44),wf(:,-1),G3(:,:,:,109))
  call loop_DV_C(G3(:,:,:,109),Q(:,46),wf(:,-4),G4(:,:,:,42))
  call check_last_DV_C(l_switch,G4(:,:,:,42),Q(:,62),wf(:,0),G5tensor(:,30))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,267),G2(:,:,:,127))
  call check_last_DV_C(l_switch,G2(:,:,:,127),Q(:,60),wf(:,61),G3tensor(:,108))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1187),G2(:,:,:,128))
  call check_last_DV_C(l_switch,G2(:,:,:,128),Q(:,59),wf(:,-2),G3tensor(:,109))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1329),G2(:,:,:,129))
  call check_last_DV_C(l_switch,G2(:,:,:,129),Q(:,59),wf(:,-2),G3tensor(:,110))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,88),G2(:,:,:,130))
  call loop_DV_C(G2(:,:,:,130),Q(:,27),wf(:,-5),G3(:,:,:,110))
  call check_last_DV_C(l_switch,G3(:,:,:,110),Q(:,59),wf(:,-2),G4tensor(:,98))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1332),G2(:,:,:,131))
  call check_last_DV_C(l_switch,G2(:,:,:,131),Q(:,59),wf(:,-2),G3tensor(:,111))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,89),G2(:,:,:,132))
  call loop_DV_C(G2(:,:,:,132),Q(:,43),wf(:,-4),G3(:,:,:,111))
  call check_last_DV_C(l_switch,G3(:,:,:,111),Q(:,59),wf(:,-2),G4tensor(:,99))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,247),G2(:,:,:,133))
  call check_last_DV_C(l_switch,G2(:,:,:,133),Q(:,58),wf(:,90),G3tensor(:,112))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,95),G2(:,:,:,134))
  call loop_DV_C(G2(:,:,:,134),Q(:,26),wf(:,-5),G3(:,:,:,112))
  call check_last_DV_C(l_switch,G3(:,:,:,112),Q(:,58),wf(:,90),G4tensor(:,100))
  call loop_DV_C(G2(:,:,:,134),Q(:,26),wf(:,113),G3(:,:,:,113))
  call check_last_DV_C(l_switch,G3(:,:,:,113),Q(:,59),wf(:,-2),G4tensor(:,101))
  call loop_DV_C(G2(:,:,:,134),Q(:,26),wf(:,70),G3(:,:,:,114))
  call check_last_DV_C(l_switch,G3(:,:,:,114),Q(:,62),wf(:,0),G4tensor(:,102))
  call loop_DV_C(G2(:,:,:,134),Q(:,26),wf(:,-2),G3(:,:,:,115))
  call loop_DV_C(G3(:,:,:,115),Q(:,30),wf(:,-5),G4(:,:,:,43))
  call check_last_DV_C(l_switch,G4(:,:,:,43),Q(:,62),wf(:,0),G5tensor(:,31))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,248),G2(:,:,:,135))
  call check_last_DV_C(l_switch,G2(:,:,:,135),Q(:,58),wf(:,90),G3tensor(:,113))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,99),G2(:,:,:,136))
  call loop_DV_C(G2(:,:,:,136),Q(:,42),wf(:,-4),G3(:,:,:,116))
  call check_last_DV_C(l_switch,G3(:,:,:,116),Q(:,58),wf(:,90),G4tensor(:,103))
  call loop_DV_C(G2(:,:,:,136),Q(:,42),wf(:,109),G3(:,:,:,117))
  call check_last_DV_C(l_switch,G3(:,:,:,117),Q(:,59),wf(:,-2),G4tensor(:,104))
  call loop_DV_C(G2(:,:,:,136),Q(:,42),wf(:,66),G3(:,:,:,118))
  call check_last_DV_C(l_switch,G3(:,:,:,118),Q(:,62),wf(:,0),G4tensor(:,105))
  call loop_DV_C(G2(:,:,:,136),Q(:,42),wf(:,-2),G3(:,:,:,119))
  call loop_DV_C(G3(:,:,:,119),Q(:,46),wf(:,-4),G4(:,:,:,44))
  call check_last_DV_C(l_switch,G4(:,:,:,44),Q(:,62),wf(:,0),G5tensor(:,32))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,249),G2(:,:,:,137))
  call check_last_DV_C(l_switch,G2(:,:,:,137),Q(:,58),wf(:,90),G3tensor(:,114))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1218),G2(:,:,:,138))
  call check_last_DV_C(l_switch,G2(:,:,:,138),Q(:,61),wf(:,-1),G3tensor(:,115))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,90),G2(:,:,:,139))
  call loop_DV_C(G2(:,:,:,139),Q(:,13),wf(:,84),G3(:,:,:,120))
  call check_last_DV_C(l_switch,G3(:,:,:,120),Q(:,61),wf(:,-1),G4tensor(:,106))
  call loop_DV_C(G2(:,:,:,139),Q(:,13),wf(:,-5),G3(:,:,:,121))
  call loop_DV_C(G3(:,:,:,121),Q(:,45),wf(:,-4),G4(:,:,:,45))
  call check_last_DV_C(l_switch,G4(:,:,:,45),Q(:,61),wf(:,-1),G5tensor(:,33))
  call loop_DV_C(G2(:,:,:,139),Q(:,13),wf(:,-4),G3(:,:,:,122))
  call loop_DV_C(G3(:,:,:,122),Q(:,29),wf(:,-5),G4(:,:,:,46))
  call check_last_DV_C(l_switch,G4(:,:,:,46),Q(:,61),wf(:,-1),G5tensor(:,34))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1341),G2(:,:,:,140))
  call check_last_DV_C(l_switch,G2(:,:,:,140),Q(:,61),wf(:,-1),G3tensor(:,116))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,152),G2(:,:,:,141))
  call loop_DV_C(G2(:,:,:,141),Q(:,29),wf(:,-5),G3(:,:,:,123))
  call check_last_DV_C(l_switch,G3(:,:,:,123),Q(:,61),wf(:,-1),G4tensor(:,107))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1344),G2(:,:,:,142))
  call check_last_DV_C(l_switch,G2(:,:,:,142),Q(:,61),wf(:,-1),G3tensor(:,117))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,153),G2(:,:,:,143))
  call loop_DV_C(G2(:,:,:,143),Q(:,45),wf(:,-4),G3(:,:,:,124))
  call check_last_DV_C(l_switch,G3(:,:,:,124),Q(:,61),wf(:,-1),G4tensor(:,108))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,193),G2(:,:,:,144))
  call check_last_DV_C(l_switch,G2(:,:,:,144),Q(:,57),wf(:,105),G3tensor(:,118))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,109),G2(:,:,:,145))
  call loop_DV_C(G2(:,:,:,145),Q(:,25),wf(:,-5),G3(:,:,:,125))
  call check_last_DV_C(l_switch,G3(:,:,:,125),Q(:,57),wf(:,105),G4tensor(:,109))
  call loop_DV_C(G2(:,:,:,145),Q(:,25),wf(:,99),G3(:,:,:,126))
  call check_last_DV_C(l_switch,G3(:,:,:,126),Q(:,59),wf(:,-2),G4tensor(:,110))
  call loop_DV_C(G2(:,:,:,145),Q(:,25),wf(:,70),G3(:,:,:,127))
  call check_last_DV_C(l_switch,G3(:,:,:,127),Q(:,61),wf(:,-1),G4tensor(:,111))
  call loop_DV_C(G2(:,:,:,145),Q(:,25),wf(:,-2),G3(:,:,:,128))
  call loop_DV_C(G3(:,:,:,128),Q(:,29),wf(:,-5),G4(:,:,:,47))
  call check_last_DV_C(l_switch,G4(:,:,:,47),Q(:,61),wf(:,-1),G5tensor(:,35))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,194),G2(:,:,:,146))
  call check_last_DV_C(l_switch,G2(:,:,:,146),Q(:,57),wf(:,105),G3tensor(:,119))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,113),G2(:,:,:,147))
  call loop_DV_C(G2(:,:,:,147),Q(:,41),wf(:,-4),G3(:,:,:,129))
  call check_last_DV_C(l_switch,G3(:,:,:,129),Q(:,57),wf(:,105),G4tensor(:,112))
  call loop_DV_C(G2(:,:,:,147),Q(:,41),wf(:,95),G3(:,:,:,130))
  call check_last_DV_C(l_switch,G3(:,:,:,130),Q(:,59),wf(:,-2),G4tensor(:,113))
  call loop_DV_C(G2(:,:,:,147),Q(:,41),wf(:,66),G3(:,:,:,131))
  call check_last_DV_C(l_switch,G3(:,:,:,131),Q(:,61),wf(:,-1),G4tensor(:,114))
  call loop_DV_C(G2(:,:,:,147),Q(:,41),wf(:,-2),G3(:,:,:,132))
  call loop_DV_C(G3(:,:,:,132),Q(:,45),wf(:,-4),G4(:,:,:,48))
  call check_last_DV_C(l_switch,G4(:,:,:,48),Q(:,61),wf(:,-1),G5tensor(:,36))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,195),G2(:,:,:,148))
  call check_last_DV_C(l_switch,G2(:,:,:,148),Q(:,57),wf(:,105),G3tensor(:,120))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1245),G2(:,:,:,149))
  call check_last_DV_C(l_switch,G2(:,:,:,149),Q(:,62),wf(:,0),G3tensor(:,121))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,105),G2(:,:,:,150))
  call loop_DV_C(G2(:,:,:,150),Q(:,14),wf(:,84),G3(:,:,:,133))
  call check_last_DV_C(l_switch,G3(:,:,:,133),Q(:,62),wf(:,0),G4tensor(:,115))
  call loop_DV_C(G2(:,:,:,150),Q(:,14),wf(:,-5),G3(:,:,:,134))
  call loop_DV_C(G3(:,:,:,134),Q(:,46),wf(:,-4),G4(:,:,:,49))
  call check_last_DV_C(l_switch,G4(:,:,:,49),Q(:,62),wf(:,0),G5tensor(:,37))
  call loop_DV_C(G2(:,:,:,150),Q(:,14),wf(:,-4),G3(:,:,:,135))
  call loop_DV_C(G3(:,:,:,135),Q(:,30),wf(:,-5),G4(:,:,:,50))
  call check_last_DV_C(l_switch,G4(:,:,:,50),Q(:,62),wf(:,0),G5tensor(:,38))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1354),G2(:,:,:,151))
  call check_last_DV_C(l_switch,G2(:,:,:,151),Q(:,62),wf(:,0),G3tensor(:,122))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,206),G2(:,:,:,152))
  call loop_DV_C(G2(:,:,:,152),Q(:,30),wf(:,-5),G3(:,:,:,136))
  call check_last_DV_C(l_switch,G3(:,:,:,136),Q(:,62),wf(:,0),G4tensor(:,116))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1356),G2(:,:,:,153))
  call check_last_DV_C(l_switch,G2(:,:,:,153),Q(:,62),wf(:,0),G3tensor(:,123))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,207),G2(:,:,:,154))
  call loop_DV_C(G2(:,:,:,154),Q(:,46),wf(:,-4),G3(:,:,:,137))
  call check_last_DV_C(l_switch,G3(:,:,:,137),Q(:,62),wf(:,0),G4tensor(:,117))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,284),G2(:,:,:,155))
  call check_last_DV_C(l_switch,G2(:,:,:,155),Q(:,59),wf(:,-2),G3tensor(:,124))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1290),G2(:,:,:,156))
  call check_last_DV_C(l_switch,G2(:,:,:,156),Q(:,61),wf(:,-1),G3tensor(:,125))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1395),G2(:,:,:,157))
  call check_last_DV_C(l_switch,G2(:,:,:,157),Q(:,59),wf(:,-2),G3tensor(:,126))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,135),G2(:,:,:,158))
  call loop_DV_C(G2(:,:,:,158),Q(:,27),wf(:,-5),G3(:,:,:,138))
  call check_last_DV_C(l_switch,G3(:,:,:,138),Q(:,59),wf(:,-2),G4tensor(:,118))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1398),G2(:,:,:,159))
  call check_last_DV_C(l_switch,G2(:,:,:,159),Q(:,61),wf(:,-1),G3tensor(:,127))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,171),G2(:,:,:,160))
  call loop_DV_C(G2(:,:,:,160),Q(:,29),wf(:,-5),G3(:,:,:,139))
  call check_last_DV_C(l_switch,G3(:,:,:,139),Q(:,61),wf(:,-1),G4tensor(:,119))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1402),G2(:,:,:,161))
  call check_last_DV_C(l_switch,G2(:,:,:,161),Q(:,59),wf(:,-2),G3tensor(:,128))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1403),G2(:,:,:,162))
  call check_last_DV_C(l_switch,G2(:,:,:,162),Q(:,61),wf(:,-1),G3tensor(:,129))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,285),G2(:,:,:,163))
  call check_last_DV_C(l_switch,G2(:,:,:,163),Q(:,59),wf(:,-2),G3tensor(:,130))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1273),G2(:,:,:,164))
  call check_last_DV_C(l_switch,G2(:,:,:,164),Q(:,62),wf(:,0),G3tensor(:,131))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1407),G2(:,:,:,165))
  call check_last_DV_C(l_switch,G2(:,:,:,165),Q(:,59),wf(:,-2),G3tensor(:,132))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,139),G2(:,:,:,166))
  call loop_DV_C(G2(:,:,:,166),Q(:,27),wf(:,-5),G3(:,:,:,140))
  call check_last_DV_C(l_switch,G3(:,:,:,140),Q(:,59),wf(:,-2),G4tensor(:,120))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1410),G2(:,:,:,167))
  call check_last_DV_C(l_switch,G2(:,:,:,167),Q(:,59),wf(:,-2),G3tensor(:,133))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1412),G2(:,:,:,168))
  call check_last_DV_C(l_switch,G2(:,:,:,168),Q(:,62),wf(:,0),G3tensor(:,134))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,225),G2(:,:,:,169))
  call loop_DV_C(G2(:,:,:,169),Q(:,30),wf(:,-5),G3(:,:,:,141))
  call check_last_DV_C(l_switch,G3(:,:,:,141),Q(:,62),wf(:,0),G4tensor(:,121))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1415),G2(:,:,:,170))
  call check_last_DV_C(l_switch,G2(:,:,:,170),Q(:,62),wf(:,0),G3tensor(:,135))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1296),G2(:,:,:,171))
  call check_last_DV_C(l_switch,G2(:,:,:,171),Q(:,61),wf(:,-1),G3tensor(:,136))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1279),G2(:,:,:,172))
  call check_last_DV_C(l_switch,G2(:,:,:,172),Q(:,62),wf(:,0),G3tensor(:,137))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1420),G2(:,:,:,173))
  call check_last_DV_C(l_switch,G2(:,:,:,173),Q(:,61),wf(:,-1),G3tensor(:,138))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,175),G2(:,:,:,174))
  call loop_DV_C(G2(:,:,:,174),Q(:,29),wf(:,-5),G3(:,:,:,142))
  call check_last_DV_C(l_switch,G3(:,:,:,142),Q(:,61),wf(:,-1),G4tensor(:,122))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1422),G2(:,:,:,175))
  call check_last_DV_C(l_switch,G2(:,:,:,175),Q(:,61),wf(:,-1),G3tensor(:,139))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1424),G2(:,:,:,176))
  call check_last_DV_C(l_switch,G2(:,:,:,176),Q(:,62),wf(:,0),G3tensor(:,140))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,229),G2(:,:,:,177))
  call loop_DV_C(G2(:,:,:,177),Q(:,30),wf(:,-5),G3(:,:,:,143))
  call check_last_DV_C(l_switch,G3(:,:,:,143),Q(:,62),wf(:,0),G4tensor(:,123))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1426),G2(:,:,:,178))
  call check_last_DV_C(l_switch,G2(:,:,:,178),Q(:,62),wf(:,0),G3tensor(:,141))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1443),G2(:,:,:,179))
  call check_last_DV_C(l_switch,G2(:,:,:,179),Q(:,59),wf(:,-2),G3tensor(:,142))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,140),G2(:,:,:,180))
  call loop_DV_C(G2(:,:,:,180),Q(:,43),wf(:,-4),G3(:,:,:,144))
  call check_last_DV_C(l_switch,G3(:,:,:,144),Q(:,59),wf(:,-2),G4tensor(:,124))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1446),G2(:,:,:,181))
  call check_last_DV_C(l_switch,G2(:,:,:,181),Q(:,61),wf(:,-1),G3tensor(:,143))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,176),G2(:,:,:,182))
  call loop_DV_C(G2(:,:,:,182),Q(:,45),wf(:,-4),G3(:,:,:,145))
  call check_last_DV_C(l_switch,G3(:,:,:,145),Q(:,61),wf(:,-1),G4tensor(:,125))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1450),G2(:,:,:,183))
  call check_last_DV_C(l_switch,G2(:,:,:,183),Q(:,59),wf(:,-2),G3tensor(:,144))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1451),G2(:,:,:,184))
  call check_last_DV_C(l_switch,G2(:,:,:,184),Q(:,61),wf(:,-1),G3tensor(:,145))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1455),G2(:,:,:,185))
  call check_last_DV_C(l_switch,G2(:,:,:,185),Q(:,59),wf(:,-2),G3tensor(:,146))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,141),G2(:,:,:,186))
  call loop_DV_C(G2(:,:,:,186),Q(:,43),wf(:,-4),G3(:,:,:,146))
  call check_last_DV_C(l_switch,G3(:,:,:,146),Q(:,59),wf(:,-2),G4tensor(:,126))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1458),G2(:,:,:,187))
  call check_last_DV_C(l_switch,G2(:,:,:,187),Q(:,59),wf(:,-2),G3tensor(:,147))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1460),G2(:,:,:,188))
  call check_last_DV_C(l_switch,G2(:,:,:,188),Q(:,62),wf(:,0),G3tensor(:,148))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,230),G2(:,:,:,189))
  call loop_DV_C(G2(:,:,:,189),Q(:,46),wf(:,-4),G3(:,:,:,147))
  call check_last_DV_C(l_switch,G3(:,:,:,147),Q(:,62),wf(:,0),G4tensor(:,127))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1463),G2(:,:,:,190))
  call check_last_DV_C(l_switch,G2(:,:,:,190),Q(:,62),wf(:,0),G3tensor(:,149))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1468),G2(:,:,:,191))
  call check_last_DV_C(l_switch,G2(:,:,:,191),Q(:,61),wf(:,-1),G3tensor(:,150))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,177),G2(:,:,:,192))
  call loop_DV_C(G2(:,:,:,192),Q(:,45),wf(:,-4),G3(:,:,:,148))
  call check_last_DV_C(l_switch,G3(:,:,:,148),Q(:,61),wf(:,-1),G4tensor(:,128))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1470),G2(:,:,:,193))
  call check_last_DV_C(l_switch,G2(:,:,:,193),Q(:,61),wf(:,-1),G3tensor(:,151))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1472),G2(:,:,:,194))
  call check_last_DV_C(l_switch,G2(:,:,:,194),Q(:,62),wf(:,0),G3tensor(:,152))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,231),G2(:,:,:,195))
  call loop_DV_C(G2(:,:,:,195),Q(:,46),wf(:,-4),G3(:,:,:,149))
  call check_last_DV_C(l_switch,G3(:,:,:,149),Q(:,62),wf(:,0),G4tensor(:,129))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1474),G2(:,:,:,196))
  call check_last_DV_C(l_switch,G2(:,:,:,196),Q(:,62),wf(:,0),G3tensor(:,153))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1489),G2(:,:,:,197))
  call check_last_DV_C(l_switch,G2(:,:,:,197),Q(:,59),wf(:,-2),G3tensor(:,154))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1490),G2(:,:,:,198))
  call check_last_DV_C(l_switch,G2(:,:,:,198),Q(:,59),wf(:,-2),G3tensor(:,155))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1491),G2(:,:,:,199))
  call check_last_DV_C(l_switch,G2(:,:,:,199),Q(:,61),wf(:,-1),G3tensor(:,156))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1492),G2(:,:,:,200))
  call check_last_DV_C(l_switch,G2(:,:,:,200),Q(:,61),wf(:,-1),G3tensor(:,157))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1495),G2(:,:,:,201))
  call check_last_DV_C(l_switch,G2(:,:,:,201),Q(:,62),wf(:,0),G3tensor(:,158))
  call loop_DV_C(G1(:,:,:,1),Q(:,8),wf(:,1496),G2(:,:,:,202))
  call check_last_DV_C(l_switch,G2(:,:,:,202),Q(:,62),wf(:,0),G3tensor(:,159))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(6)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(305)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,1)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(305)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,2)
  Gcoeff = (c(6)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(305)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,3)
  Gcoeff = (c(6)*(M(134)-M(137)+M(139)+M(141)-M(142)-M(144)-M(150)+M(153)-M(172)+M(188)-M(194)-M(200)+M(202)+M(214)+M(238) &
    -M(248))) * den(25)
  T3sum(1:35,83) = T3sum(1:35,83) + Gcoeff * G3tensor(:,4)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(137)-M(138)-M(140)+M(142)+M(147)-M(153)+M(172)-M(178)-M(188)+M(190)+M(196)-M(202)-M(224) &
    +M(248))) * den(25)
  T3sum(1:35,83) = T3sum(1:35,83) + Gcoeff * G3tensor(:,5)
  Gcoeff = (c(6)*(-M(136)+M(138)-M(139)+M(140)-M(141)+M(144)-M(147)+M(150)+M(178)-M(190)+M(194)-M(196)+M(200)-M(214)+M(224) &
    -M(238))) * den(25)
  T3sum(1:35,83) = T3sum(1:35,83) + Gcoeff * G3tensor(:,6)
  Gcoeff = (c(6)*(M(142)-M(156)+M(162)+M(167)-M(169)-M(171)+M(172)+M(173)-M(175)-M(184)-M(206)+M(216)+M(222)-M(225)-M(230) &
    +M(240))) * den(367)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,7)
  Gcoeff = (c(6)*(M(140)-M(142)-M(155)+M(156)+M(161)-M(162)+M(171)-M(172)-M(177)+M(178)-M(182)+M(184)-M(222)+M(225)+M(246) &
    -M(249))) * den(367)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,8)
  Gcoeff = (c(6)*(-M(140)+M(155)-M(161)-M(167)+M(169)-M(173)+M(175)+M(177)-M(178)+M(182)+M(206)-M(216)+M(230)-M(240)-M(246) &
    +M(249))) * den(367)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,9)
  Gcoeff = (c(6)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(393)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,10)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(158)-M(160)+M(179)-M(180)-M(185)+M(186)-M(195)+M(196)+M(201)-M(202)+M(221)-M(223)-M(245) &
    +M(247))) * den(393)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,11)
  Gcoeff = (c(6)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(393)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,12)
  Gcoeff = (c(6)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(434)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,13)
  Gcoeff = (c(6)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(434)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,14)
  Gcoeff = (c(6)*(-M(138)+M(141)+M(173)-M(175)+M(180)-M(183)+M(205)-M(206)-M(213)+M(214)-M(215)+M(216)+M(223)-M(224)-M(233) &
    +M(235))) * den(434)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,15)
  Gcoeff = (c(6)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(421)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,16)
  Gcoeff = (c(6)*(M(147)-M(158)+M(168)+M(174)-M(177)-M(182)+M(185)-M(187)-M(189)+M(190)+M(192)+M(198)-M(201)-M(207)-M(231) &
    +M(245))) * den(421)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,17)
  Gcoeff = (c(6)*(M(144)-M(147)-M(174)+M(177)-M(181)+M(182)+M(189)-M(190)+M(191)-M(192)-M(199)+M(200)-M(204)+M(207)+M(234) &
    -M(237))) * den(421)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,18)
  Gcoeff = (c(6)*(M(137)-M(139)-M(167)+M(169)-M(179)+M(181)+M(209)-M(211)-M(229)+M(230)+M(237)-M(238)+M(239)-M(240)-M(247) &
    +M(248))) * den(438)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,19)
  Gcoeff = (c(6)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(438)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,20)
  Gcoeff = (c(6)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(438)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,21)
  Gcoeff = (c(6)*(-M(150)+M(153)+M(168)-M(171)+M(183)-M(184)-M(187)+M(188)+M(193)-M(194)-M(197)+M(198)-M(210)+M(213)+M(228) &
    -M(231))) * den(425)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,22)
  Gcoeff = (c(6)*(-M(153)+M(160)-M(168)+M(171)-M(174)+M(184)-M(186)+M(187)-M(188)+M(189)-M(192)+M(195)-M(198)+M(207)-M(221) &
    +M(231))) * den(425)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,23)
  Gcoeff = (c(6)*(M(150)-M(160)+M(174)-M(183)+M(186)-M(189)+M(192)-M(193)+M(194)-M(195)+M(197)-M(207)+M(210)-M(213)+M(221) &
    -M(228))) * den(425)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,24)
  Gcoeff = (c(6)*(M(137)-M(139)-M(141)+M(142)-M(171)+M(172)-M(179)+M(181)+M(183)-M(184)+M(213)-M(214)+M(237)-M(238)-M(247) &
    +M(248))) * den(440)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,25)
  Gcoeff = (c(6)*(-M(137)+M(138)+M(140)-M(142)+M(171)-M(172)-M(177)+M(178)+M(179)-M(180)-M(182)+M(184)-M(223)+M(224)+M(247) &
    -M(248))) * den(440)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,26)
  Gcoeff = (c(6)*(-M(138)+M(139)-M(140)+M(141)+M(177)-M(178)+M(180)-M(181)+M(182)-M(183)-M(213)+M(214)+M(223)-M(224)-M(237) &
    +M(238))) * den(440)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,27)
  Gcoeff = (c(6)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(429)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,28)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(147)-M(153)+M(171)-M(177)+M(179)-M(180)-M(182)+M(184)-M(188)+M(190)+M(196)-M(202)-M(223) &
    +M(247))) * den(429)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,29)
  Gcoeff = (c(6)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(429)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,30)
  Gcoeff = (c(5)*(-M(61)+M(82)+M(94)-M(99)-M(104)+M(114)+M(120)-M(123)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(171)-M(179)+M(181) &
    +M(183)-M(184)+M(213)+M(237)-M(247))) * den(24)
  T4sum(1:70,192) = T4sum(1:70,192) + Gcoeff * G4tensor(:,1)
  Gcoeff = (c(5)*(M(61)-M(64)-M(87)+M(99)+M(104)-M(106)-M(117)+M(123)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(171)-M(177)+M(179) &
    -M(180)-M(182)+M(184)-M(223)+M(247))) * den(24)
  T4sum(1:70,192) = T4sum(1:70,192) + Gcoeff * G4tensor(:,2)
  Gcoeff = (c(5)*(M(64)-M(82)+M(87)-M(94)+M(106)-M(114)+M(117)-M(120)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(177)+M(180)-M(181) &
    +M(182)-M(183)-M(213)+M(223)-M(237))) * den(24)
  T4sum(1:70,192) = T4sum(1:70,192) + Gcoeff * G4tensor(:,3)
  Gcoeff = (c(6)*(-M(148)+M(158)-M(161)+M(163)+M(165)-M(166)-M(168)-M(174)+M(177)+M(182)-M(192)-M(198)+M(201)+M(208)+M(232) &
    -M(246))) * den(63)
  T3sum(1:35,76) = T3sum(1:35,76) + Gcoeff * G3tensor(:,31)
  Gcoeff = (c(6)*(M(148)-M(154)-M(158)+M(160)+M(161)-M(162)-M(164)+M(166)+M(171)-M(177)-M(182)+M(184)+M(195)-M(201)-M(222) &
    +M(246))) * den(63)
  T3sum(1:35,76) = T3sum(1:35,76) + Gcoeff * G3tensor(:,32)
  Gcoeff = (c(6)*(M(154)-M(160)+M(162)-M(163)+M(164)-M(165)+M(168)-M(171)+M(174)-M(184)+M(192)-M(195)+M(198)-M(208)+M(222) &
    -M(232))) * den(63)
  T3sum(1:35,76) = T3sum(1:35,76) + Gcoeff * G3tensor(:,33)
  Gcoeff = (c(6)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(509)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,34)
  Gcoeff = (c(6)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(509)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,35)
  Gcoeff = (c(6)*(M(149)-M(151)-M(162)+M(165)+M(186)-M(189)-M(207)+M(208)+M(211)-M(212)-M(217)+M(218)+M(221)-M(222)-M(227) &
    +M(229))) * den(509)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,36)
  Gcoeff = (c(6)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(496)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,37)
  Gcoeff = (c(6)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(496)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,38)
  Gcoeff = (c(6)*(-M(150)+M(153)+M(168)-M(171)+M(183)-M(184)-M(187)+M(188)+M(193)-M(194)-M(197)+M(198)-M(210)+M(213)+M(228) &
    -M(231))) * den(496)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,39)
  Gcoeff = (c(6)*(-M(143)+M(145)+M(161)-M(163)-M(185)+M(187)+M(203)-M(205)+M(231)-M(232)-M(235)+M(236)+M(241)-M(242)-M(245) &
    +M(246))) * den(513)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,40)
  Gcoeff = (c(6)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(513)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,41)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(513)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,42)
  Gcoeff = (c(6)*(M(144)-M(147)-M(174)+M(177)-M(181)+M(182)+M(189)-M(190)+M(191)-M(192)-M(199)+M(200)-M(204)+M(207)+M(234) &
    -M(237))) * den(500)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,43)
  Gcoeff = (c(6)*(M(136)-M(144)+M(147)-M(150)-M(177)-M(180)+M(181)-M(182)+M(183)+M(190)-M(194)+M(196)-M(200)+M(213)-M(223) &
    +M(237))) * den(500)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,44)
  Gcoeff = (c(6)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(500)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,45)
  Gcoeff = (c(6)*(-M(147)+M(148)+M(161)-M(163)-M(165)+M(166)-M(185)+M(187)+M(189)-M(190)+M(207)-M(208)+M(231)-M(232)-M(245) &
    +M(246))) * den(515)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,46)
  Gcoeff = (c(6)*(M(147)-M(148)-M(153)+M(154)-M(161)+M(162)+M(164)-M(166)+M(185)-M(186)-M(188)+M(190)-M(221)+M(222)+M(245) &
    -M(246))) * den(515)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,47)
  Gcoeff = (c(6)*(M(153)-M(154)-M(162)+M(163)-M(164)+M(165)+M(186)-M(187)+M(188)-M(189)-M(207)+M(208)+M(221)-M(222)-M(231) &
    +M(232))) * den(515)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,48)
  Gcoeff = (c(6)*(-M(147)+M(158)-M(168)-M(174)+M(177)+M(182)-M(185)+M(187)+M(189)-M(190)-M(192)-M(198)+M(201)+M(207)+M(231) &
    -M(245))) * den(504)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,49)
  Gcoeff = (c(6)*(M(147)-M(153)-M(158)+M(160)+M(171)-M(177)-M(182)+M(184)+M(185)-M(186)-M(188)+M(190)+M(195)-M(201)-M(221) &
    +M(245))) * den(504)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,50)
  Gcoeff = (c(6)*(M(153)-M(160)+M(168)-M(171)+M(174)-M(184)+M(186)-M(187)+M(188)-M(189)+M(192)-M(195)+M(198)-M(207)+M(221) &
    -M(231))) * den(504)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,51)
  Gcoeff = (c(5)*(-M(49)+M(79)+M(91)-M(98)-M(106)-M(112)+M(114)+M(116)-M(117)-M(118)+M(120)+M(122))+c(6)*(-M(147)-M(185)+M(187) &
    +M(189)-M(190)+M(207)+M(231)-M(245))) * den(62)
  T4sum(1:70,189) = T4sum(1:70,189) + Gcoeff * G4tensor(:,7)
  Gcoeff = (c(5)*(M(49)-M(52)-M(86)+M(98)-M(104)+M(106)-M(110)+M(112)+M(117)+M(118)-M(123)-M(124))+c(6)*(M(147)-M(153)+M(185) &
    -M(186)-M(188)+M(190)-M(221)+M(245))) * den(62)
  T4sum(1:70,189) = T4sum(1:70,189) + Gcoeff * G4tensor(:,8)
  Gcoeff = (c(5)*(M(52)-M(79)+M(86)-M(91)+M(104)+M(110)-M(114)-M(116)-M(120)-M(122)+M(123)+M(124))+c(6)*(M(153)+M(186)-M(187) &
    +M(188)-M(189)-M(207)+M(221)-M(231))) * den(62)
  T4sum(1:70,189) = T4sum(1:70,189) + Gcoeff * G4tensor(:,9)
  Gcoeff = (c(6)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(538)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,52)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(147)-M(153)+M(171)-M(177)+M(179)-M(180)-M(182)+M(184)-M(188)+M(190)+M(196)-M(202)-M(223) &
    +M(247))) * den(538)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,53)
  Gcoeff = (c(6)*(-M(147)+M(153)+M(158)-M(160)-M(171)+M(177)+M(182)-M(184)-M(185)+M(186)+M(188)-M(190)-M(195)+M(201)+M(221) &
    -M(245))) * den(538)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,54)
  Gcoeff = (c(6)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(542)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,55)
  Gcoeff = (c(6)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(542)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,56)
  Gcoeff = (c(6)*(M(150)-M(160)+M(174)-M(183)+M(186)-M(189)+M(192)-M(193)+M(194)-M(195)+M(197)-M(207)+M(210)-M(213)+M(221) &
    -M(228))) * den(542)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,57)
  Gcoeff = (c(5)*(-M(43)+M(55)+M(89)-M(92)+M(102)+M(104)-M(105)-M(106)-M(111)-M(117)+M(121)+M(123))+c(6)*(-M(180)+M(186)+M(197) &
    -M(199)-M(204)+M(210)+M(221)-M(223))) * den(82)
  T4sum(1:70,157) = T4sum(1:70,157) + Gcoeff * G4tensor(:,20)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)+M(92)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120))+c(6)*(M(180)-M(183)-M(189) &
    +M(199)+M(204)-M(207)-M(213)+M(223))) * den(82)
  T4sum(1:70,157) = T4sum(1:70,157) + Gcoeff * G4tensor(:,21)
  Gcoeff = (c(5)*(M(50)-M(55)+M(62)-M(89)-M(102)-M(104)+M(108)+M(114)+M(119)+M(120)-M(121)-M(123))+c(6)*(M(183)-M(186)+M(189) &
    -M(197)+M(207)-M(210)+M(213)-M(221))) * den(82)
  T4sum(1:70,157) = T4sum(1:70,157) + Gcoeff * G4tensor(:,22)
  Gcoeff = (c(5)*(-M(43)+M(55)+M(89)-M(92)+M(102)+M(104)-M(105)-M(106)-M(111)-M(117)+M(121)+M(123))+c(6)*(-M(136)+M(160)-M(191) &
    +M(193)+M(195)-M(196)+M(228)-M(234))) * den(82)
  T4sum(1:70,158) = T4sum(1:70,158) + Gcoeff * G4tensor(:,23)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)+M(92)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120))+c(6)*(M(136)-M(150)-M(174) &
    +M(191)-M(192)-M(194)+M(196)+M(234))) * den(82)
  T4sum(1:70,158) = T4sum(1:70,158) + Gcoeff * G4tensor(:,24)
  Gcoeff = (c(5)*(M(50)-M(55)+M(62)-M(89)-M(102)-M(104)+M(108)+M(114)+M(119)+M(120)-M(121)-M(123))+c(6)*(M(150)-M(160)+M(174) &
    +M(192)-M(193)+M(194)-M(195)-M(228))) * den(82)
  T4sum(1:70,158) = T4sum(1:70,158) + Gcoeff * G4tensor(:,25)
  Gcoeff = (c(6)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(545)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,58)
  Gcoeff = (c(6)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(545)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,59)
  Gcoeff = (c(6)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(545)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,60)
  Gcoeff = (c(5)*(-M(42)+M(54)+M(77)-M(80)+M(101)-M(103)-M(104)+M(106)-M(109)+M(115)+M(117)-M(123))+c(6)*(-M(179)+M(185)+M(191) &
    -M(193)-M(228)+M(234)+M(245)-M(247))) * den(84)
  T4sum(1:70,160) = T4sum(1:70,160) + Gcoeff * G4tensor(:,50)
  Gcoeff = (c(5)*(M(42)-M(47)-M(59)+M(80)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123))+c(6)*(M(179)-M(181)-M(187) &
    +M(193)+M(228)-M(231)-M(237)+M(247))) * den(84)
  T4sum(1:70,160) = T4sum(1:70,160) + Gcoeff * G4tensor(:,51)
  Gcoeff = (c(5)*(M(47)-M(54)+M(59)-M(77)-M(101)-M(106)+M(107)+M(113)+M(114)-M(115)-M(117)+M(120))+c(6)*(M(181)-M(185)+M(187) &
    -M(191)+M(231)-M(234)+M(237)-M(245))) * den(84)
  T4sum(1:70,160) = T4sum(1:70,160) + Gcoeff * G4tensor(:,52)
  Gcoeff = (c(5)*(-M(42)+M(54)+M(77)-M(80)+M(101)-M(103)-M(104)+M(106)-M(109)+M(115)+M(117)-M(123))+c(6)*(-M(134)+M(158)-M(197) &
    +M(199)+M(201)-M(202)+M(204)-M(210))) * den(84)
  T4sum(1:70,161) = T4sum(1:70,161) + Gcoeff * G4tensor(:,53)
  Gcoeff = (c(5)*(M(42)-M(47)-M(59)+M(80)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123))+c(6)*(M(134)-M(144)-M(168) &
    +M(197)-M(198)-M(200)+M(202)+M(210))) * den(84)
  T4sum(1:70,161) = T4sum(1:70,161) + Gcoeff * G4tensor(:,54)
  Gcoeff = (c(5)*(M(47)-M(54)+M(59)-M(77)-M(101)-M(106)+M(107)+M(113)+M(114)-M(115)-M(117)+M(120))+c(6)*(M(144)-M(158)+M(168) &
    +M(198)-M(199)+M(200)-M(201)-M(204))) * den(84)
  T4sum(1:70,161) = T4sum(1:70,161) + Gcoeff * G4tensor(:,55)
  Gcoeff = (c(6)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(98)
  T3sum(1:35,56) = T3sum(1:35,56) + Gcoeff * G3tensor(:,61)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(98)
  T3sum(1:35,56) = T3sum(1:35,56) + Gcoeff * G3tensor(:,62)
  Gcoeff = (c(6)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(98)
  T3sum(1:35,56) = T3sum(1:35,56) + Gcoeff * G3tensor(:,63)
  Gcoeff = (c(6)*(M(143)-M(145)-M(147)+M(148)-M(165)+M(166)+M(189)-M(190)-M(203)+M(205)+M(207)-M(208)+M(235)-M(236)-M(241) &
    +M(242))) * den(635)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,64)
  Gcoeff = (c(6)*(M(147)-M(148)-M(161)+M(163)+M(165)-M(166)+M(185)-M(187)-M(189)+M(190)-M(207)+M(208)-M(231)+M(232)+M(245) &
    -M(246))) * den(635)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,65)
  Gcoeff = (c(6)*(-M(143)+M(145)+M(161)-M(163)-M(185)+M(187)+M(203)-M(205)+M(231)-M(232)-M(235)+M(236)+M(241)-M(242)-M(245) &
    +M(246))) * den(635)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,66)
  Gcoeff = (c(6)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(622)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,67)
  Gcoeff = (c(6)*(-M(137)+M(139)+M(141)-M(142)+M(171)-M(172)+M(179)-M(181)-M(183)+M(184)-M(213)+M(214)-M(237)+M(238)+M(247) &
    -M(248))) * den(622)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,68)
  Gcoeff = (c(6)*(M(137)-M(139)-M(167)+M(169)-M(179)+M(181)+M(209)-M(211)-M(229)+M(230)+M(237)-M(238)+M(239)-M(240)-M(247) &
    +M(248))) * den(622)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,69)
  Gcoeff = (c(6)*(M(149)-M(151)-M(162)+M(165)+M(186)-M(189)-M(207)+M(208)+M(211)-M(212)-M(217)+M(218)+M(221)-M(222)-M(227) &
    +M(229))) * den(639)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,70)
  Gcoeff = (c(6)*(-M(153)+M(154)+M(162)-M(163)+M(164)-M(165)-M(186)+M(187)-M(188)+M(189)+M(207)-M(208)-M(221)+M(222)+M(231) &
    -M(232))) * den(639)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,71)
  Gcoeff = (c(6)*(-M(149)+M(151)+M(153)-M(154)+M(163)-M(164)-M(187)+M(188)-M(211)+M(212)+M(217)-M(218)+M(227)-M(229)-M(231) &
    +M(232))) * den(639)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,72)
  Gcoeff = (c(6)*(-M(138)+M(141)+M(173)-M(175)+M(180)-M(183)+M(205)-M(206)-M(213)+M(214)-M(215)+M(216)+M(223)-M(224)-M(233) &
    +M(235))) * den(626)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,73)
  Gcoeff = (c(6)*(M(138)-M(139)+M(140)-M(141)-M(177)+M(178)-M(180)+M(181)-M(182)+M(183)+M(213)-M(214)-M(223)+M(224)+M(237) &
    -M(238))) * den(626)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,74)
  Gcoeff = (c(6)*(M(139)-M(140)-M(173)+M(175)+M(177)-M(178)-M(181)+M(182)-M(205)+M(206)+M(215)-M(216)+M(233)-M(235)-M(237) &
    +M(238))) * den(626)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,75)
  Gcoeff = (c(6)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(641)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,76)
  Gcoeff = (c(6)*(M(131)-M(132)-M(137)+M(138)-M(161)+M(162)+M(185)-M(186)-M(221)+M(222)+M(224)-M(226)+M(245)-M(246)-M(248) &
    +M(250))) * den(641)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,77)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(641)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,78)
  Gcoeff = (c(6)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(630)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,79)
  Gcoeff = (c(6)*(-M(137)+M(138)+M(155)-M(156)-M(161)+M(162)+M(179)-M(180)+M(222)-M(223)+M(224)-M(225)-M(246)+M(247)-M(248) &
    +M(249))) * den(630)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,80)
  Gcoeff = (c(6)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(630)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,81)
  Gcoeff = (c(5)*(-M(52)-M(64)-M(76)+M(79)+M(82)+M(85)-M(86)-M(87)-M(88)+M(91)+M(94)+M(97))+c(6)*(-M(138)-M(162)+M(205)+M(211) &
    -M(222)-M(224)+M(229)+M(235))) * den(10)
  T4sum(1:70,186) = T4sum(1:70,186) + Gcoeff * G4tensor(:,10)
  Gcoeff = (c(5)*(-M(49)+M(52)-M(61)+M(64)-M(73)+M(76)+M(86)+M(87)+M(88)-M(98)-M(99)-M(100))+c(6)*(-M(137)+M(138)-M(161)+M(162) &
    +M(222)+M(224)-M(246)-M(248))) * den(10)
  T4sum(1:70,186) = T4sum(1:70,186) + Gcoeff * G4tensor(:,11)
  Gcoeff = (c(5)*(M(49)+M(61)+M(73)-M(79)-M(82)-M(85)-M(91)-M(94)-M(97)+M(98)+M(99)+M(100))+c(6)*(M(137)+M(161)-M(205)-M(211) &
    -M(229)-M(235)+M(246)+M(248))) * den(10)
  T4sum(1:70,186) = T4sum(1:70,186) + Gcoeff * G4tensor(:,12)
  Gcoeff = (c(6)*(-M(140)+M(142)+M(155)-M(156)-M(161)+M(162)-M(171)+M(172)+M(177)-M(178)+M(182)-M(184)+M(222)-M(225)-M(246) &
    +M(249))) * den(664)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,82)
  Gcoeff = (c(6)*(-M(137)+M(138)+M(140)-M(142)+M(171)-M(172)-M(177)+M(178)+M(179)-M(180)-M(182)+M(184)-M(223)+M(224)+M(247) &
    -M(248))) * den(664)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,83)
  Gcoeff = (c(6)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(664)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,84)
  Gcoeff = (c(6)*(-M(140)+M(155)-M(161)-M(167)+M(169)-M(173)+M(175)+M(177)-M(178)+M(182)+M(206)-M(216)+M(230)-M(240)-M(246) &
    +M(249))) * den(668)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,85)
  Gcoeff = (c(6)*(-M(139)+M(140)+M(173)-M(175)-M(177)+M(178)+M(181)-M(182)+M(205)-M(206)-M(215)+M(216)-M(233)+M(235)+M(237) &
    -M(238))) * den(668)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,86)
  Gcoeff = (c(6)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(668)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,87)
  Gcoeff = (c(5)*(-M(50)+M(55)+M(58)+M(61)-M(62)-M(63)-M(64)-M(75)-M(87)+M(89)+M(96)+M(99))+c(6)*(M(161)+M(167)-M(175)-M(177) &
    -M(182)-M(206)+M(240)+M(246))) * den(7)
  T4sum(1:70,148) = T4sum(1:70,148) + Gcoeff * G4tensor(:,80)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)+M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94))+c(6)*(M(175)+M(177)-M(181)+M(182) &
    -M(205)+M(206)-M(235)-M(237))) * den(7)
  T4sum(1:70,148) = T4sum(1:70,148) + Gcoeff * G4tensor(:,81)
  Gcoeff = (c(5)*(M(43)-M(55)-M(58)-M(61)+M(70)+M(82)-M(89)+M(92)+M(93)+M(94)-M(96)-M(99))+c(6)*(-M(161)-M(167)+M(181)+M(205) &
    +M(235)+M(237)-M(240)-M(246))) * den(7)
  T4sum(1:70,148) = T4sum(1:70,148) + Gcoeff * G4tensor(:,82)
  Gcoeff = (c(5)*(-M(50)+M(55)+M(58)+M(61)-M(62)-M(63)-M(64)-M(75)-M(87)+M(89)+M(96)+M(99))+c(6)*(-M(140)+M(155)+M(169)-M(173) &
    -M(178)-M(216)+M(230)+M(249))) * den(7)
  T4sum(1:70,149) = T4sum(1:70,149) + Gcoeff * G4tensor(:,26)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)+M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94))+c(6)*(-M(139)+M(140)+M(173)+M(178) &
    -M(215)+M(216)-M(233)-M(238))) * den(7)
  T4sum(1:70,149) = T4sum(1:70,149) + Gcoeff * G4tensor(:,27)
  Gcoeff = (c(5)*(M(43)-M(55)-M(58)-M(61)+M(70)+M(82)-M(89)+M(92)+M(93)+M(94)-M(96)-M(99))+c(6)*(M(139)-M(155)-M(169)+M(215) &
    -M(230)+M(233)+M(238)-M(249))) * den(7)
  T4sum(1:70,149) = T4sum(1:70,149) + Gcoeff * G4tensor(:,28)
  Gcoeff = (c(6)*(M(142)-M(156)+M(162)+M(167)-M(169)-M(171)+M(172)+M(173)-M(175)-M(184)-M(206)+M(216)+M(222)-M(225)-M(230) &
    +M(240))) * den(671)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,88)
  Gcoeff = (c(6)*(M(141)-M(142)-M(167)+M(169)+M(171)-M(172)-M(183)+M(184)+M(209)-M(211)-M(213)+M(214)-M(229)+M(230)+M(239) &
    -M(240))) * den(671)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,89)
  Gcoeff = (c(6)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(671)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,90)
  Gcoeff = (c(5)*(-M(47)+M(54)+M(57)-M(59)-M(60)-M(61)+M(64)-M(72)+M(77)+M(84)+M(87)-M(99))+c(6)*(M(162)-M(169)-M(171)+M(173) &
    -M(184)+M(216)+M(222)-M(230))) * den(6)
  T4sum(1:70,151) = T4sum(1:70,151) + Gcoeff * G4tensor(:,83)
  Gcoeff = (c(5)*(-M(42)+M(47)+M(59)+M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99))+c(6)*(M(169)+M(171)-M(183)+M(184) &
    -M(211)-M(213)-M(229)+M(230))) * den(6)
  T4sum(1:70,151) = T4sum(1:70,151) + Gcoeff * G4tensor(:,84)
  Gcoeff = (c(5)*(M(42)-M(54)-M(57)-M(64)+M(69)-M(77)+M(80)+M(81)+M(82)-M(84)-M(87)+M(94))+c(6)*(-M(162)-M(173)+M(183)+M(211) &
    +M(213)-M(216)-M(222)+M(229))) * den(6)
  T4sum(1:70,151) = T4sum(1:70,151) + Gcoeff * G4tensor(:,85)
  Gcoeff = (c(5)*(-M(47)+M(54)+M(57)-M(59)-M(60)-M(61)+M(64)-M(72)+M(77)+M(84)+M(87)-M(99))+c(6)*(-M(142)+M(156)-M(167)-M(172) &
    +M(175)+M(206)+M(225)-M(240))) * den(6)
  T4sum(1:70,152) = T4sum(1:70,152) + Gcoeff * G4tensor(:,56)
  Gcoeff = (c(5)*(-M(42)+M(47)+M(59)+M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99))+c(6)*(-M(141)+M(142)+M(167)+M(172) &
    -M(209)-M(214)-M(239)+M(240))) * den(6)
  T4sum(1:70,152) = T4sum(1:70,152) + Gcoeff * G4tensor(:,57)
  Gcoeff = (c(5)*(M(42)-M(54)-M(57)-M(64)+M(69)-M(77)+M(80)+M(81)+M(82)-M(84)-M(87)+M(94))+c(6)*(M(141)-M(156)-M(175)-M(206) &
    +M(209)+M(214)-M(225)+M(239))) * den(6)
  T4sum(1:70,152) = T4sum(1:70,152) + Gcoeff * G4tensor(:,58)
  Gcoeff = (c(6)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(703)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,91)
  Gcoeff = (c(6)*(M(147)-M(148)-M(153)+M(154)-M(161)+M(162)+M(164)-M(166)+M(185)-M(186)-M(188)+M(190)-M(221)+M(222)+M(245) &
    -M(246))) * den(703)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,92)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(703)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,93)
  Gcoeff = (c(6)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(707)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,94)
  Gcoeff = (c(6)*(M(149)-M(151)-M(153)+M(154)-M(163)+M(164)+M(187)-M(188)+M(211)-M(212)-M(217)+M(218)-M(227)+M(229)+M(231) &
    -M(232))) * den(707)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,95)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(707)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,96)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(50)-M(51)-M(52)-M(62)-M(74)-M(86)+M(92)+M(95)+M(98))+c(6)*(M(137)+M(143)-M(151)-M(153) &
    -M(188)-M(212)+M(242)+M(248))) * den(4)
  T4sum(1:70,136) = T4sum(1:70,136) + Gcoeff * G4tensor(:,86)
  Gcoeff = (c(5)*(M(50)+M(51)+M(52)-M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91))+c(6)*(M(151)+M(153)-M(187)+M(188) &
    -M(211)+M(212)-M(229)-M(231))) * den(4)
  T4sum(1:70,136) = T4sum(1:70,136) + Gcoeff * G4tensor(:,87)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(55)+M(67)+M(79)+M(89)+M(90)+M(91)-M(92)-M(95)-M(98))+c(6)*(-M(137)-M(143)+M(187)+M(211) &
    +M(229)+M(231)-M(242)-M(248))) * den(4)
  T4sum(1:70,136) = T4sum(1:70,136) + Gcoeff * G4tensor(:,88)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(50)-M(51)-M(52)-M(62)-M(74)-M(86)+M(92)+M(95)+M(98))+c(6)*(M(131)+M(145)-M(149)-M(154) &
    -M(164)-M(218)+M(236)+M(250))) * den(4)
  T4sum(1:70,137) = T4sum(1:70,137) + Gcoeff * G4tensor(:,29)
  Gcoeff = (c(5)*(M(50)+M(51)+M(52)-M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91))+c(6)*(M(149)+M(154)-M(163)+M(164) &
    -M(217)+M(218)-M(227)-M(232))) * den(4)
  T4sum(1:70,137) = T4sum(1:70,137) + Gcoeff * G4tensor(:,30)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(55)+M(67)+M(79)+M(89)+M(90)+M(91)-M(92)-M(95)-M(98))+c(6)*(-M(131)-M(145)+M(163)+M(217) &
    +M(227)+M(232)-M(236)-M(250))) * den(4)
  T4sum(1:70,137) = T4sum(1:70,137) + Gcoeff * G4tensor(:,31)
  Gcoeff = (c(6)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(710)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,97)
  Gcoeff = (c(6)*(-M(143)+M(145)+M(147)-M(148)+M(165)-M(166)-M(189)+M(190)+M(203)-M(205)-M(207)+M(208)-M(235)+M(236)+M(241) &
    -M(242))) * den(710)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,98)
  Gcoeff = (c(6)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(710)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,99)
  Gcoeff = (c(5)*(M(42)+M(45)-M(47)-M(48)-M(49)+M(52)-M(59)-M(71)+M(80)+M(83)+M(86)-M(98))+c(6)*(M(138)-M(145)-M(147)+M(149) &
    -M(190)+M(218)+M(224)-M(236))) * den(3)
  T4sum(1:70,139) = T4sum(1:70,139) + Gcoeff * G4tensor(:,89)
  Gcoeff = (c(5)*(M(47)+M(48)+M(49)-M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98))+c(6)*(M(145)+M(147)-M(189)+M(190) &
    -M(205)-M(207)-M(235)+M(236))) * den(3)
  T4sum(1:70,139) = T4sum(1:70,139) + Gcoeff * G4tensor(:,90)
  Gcoeff = (c(5)*(-M(42)-M(45)-M(52)+M(54)+M(66)+M(77)+M(78)+M(79)-M(80)-M(83)-M(86)+M(91))+c(6)*(-M(138)-M(149)+M(189)+M(205) &
    +M(207)-M(218)-M(224)+M(235))) * den(3)
  T4sum(1:70,139) = T4sum(1:70,139) + Gcoeff * G4tensor(:,91)
  Gcoeff = (c(5)*(M(42)+M(45)-M(47)-M(48)-M(49)+M(52)-M(59)-M(71)+M(80)+M(83)+M(86)-M(98))+c(6)*(M(132)-M(143)-M(148)+M(151) &
    -M(166)+M(212)+M(226)-M(242))) * den(3)
  T4sum(1:70,140) = T4sum(1:70,140) + Gcoeff * G4tensor(:,59)
  Gcoeff = (c(5)*(M(47)+M(48)+M(49)-M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98))+c(6)*(M(143)+M(148)-M(165)+M(166) &
    -M(203)-M(208)-M(241)+M(242))) * den(3)
  T4sum(1:70,140) = T4sum(1:70,140) + Gcoeff * G4tensor(:,60)
  Gcoeff = (c(5)*(-M(42)-M(45)-M(52)+M(54)+M(66)+M(77)+M(78)+M(79)-M(80)-M(83)-M(86)+M(91))+c(6)*(-M(132)-M(151)+M(165)+M(203) &
    +M(208)-M(212)-M(226)+M(241))) * den(3)
  T4sum(1:70,140) = T4sum(1:70,140) + Gcoeff * G4tensor(:,61)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(134)-M(136)+M(140)-M(142)-M(148)+M(154)+M(164)-M(166)-M(172)+M(178)-M(196)+M(202)+M(226) &
    -M(250))) * den(140)
  T3sum(1:35,21) = T3sum(1:35,21) + Gcoeff * G3tensor(:,100)
  Gcoeff = (c(6)*(M(131)-M(132)-M(134)+M(136)-M(155)+M(156)+M(158)-M(160)-M(195)+M(196)+M(201)-M(202)+M(225)-M(226)-M(249) &
    +M(250))) * den(140)
  T3sum(1:35,21) = T3sum(1:35,21) + Gcoeff * G3tensor(:,101)
  Gcoeff = (c(6)*(-M(140)+M(142)+M(148)-M(154)+M(155)-M(156)-M(158)+M(160)-M(164)+M(166)+M(172)-M(178)+M(195)-M(201)-M(225) &
    +M(249))) * den(140)
  T3sum(1:35,21) = T3sum(1:35,21) + Gcoeff * G3tensor(:,102)
  Gcoeff = (c(5)*(-M(41)-M(42)-M(43)+M(44)+M(47)+M(50)+M(56)+M(59)+M(62)-M(68)-M(80)-M(92))+c(6)*(-M(131)-M(136)+M(140)+M(154) &
    +M(164)+M(178)-M(196)-M(250))) * den(1)
  T4sum(1:70,146) = T4sum(1:70,146) + Gcoeff * G4tensor(:,32)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(53)-M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92))+c(6)*(M(131)+M(136)-M(155)-M(160) &
    -M(195)+M(196)-M(249)+M(250))) * den(1)
  T4sum(1:70,146) = T4sum(1:70,146) + Gcoeff * G4tensor(:,33)
  Gcoeff = (c(5)*(-M(44)-M(47)-M(50)+M(53)+M(54)+M(55)-M(56)-M(59)-M(62)+M(65)+M(77)+M(89))+c(6)*(-M(140)-M(154)+M(155)+M(160) &
    -M(164)-M(178)+M(195)+M(249))) * den(1)
  T4sum(1:70,146) = T4sum(1:70,146) + Gcoeff * G4tensor(:,34)
  Gcoeff = (c(5)*(-M(41)-M(42)-M(43)+M(44)+M(47)+M(50)+M(56)+M(59)+M(62)-M(68)-M(80)-M(92))+c(6)*(-M(132)-M(134)+M(142)+M(148) &
    +M(166)+M(172)-M(202)-M(226))) * den(1)
  T4sum(1:70,147) = T4sum(1:70,147) + Gcoeff * G4tensor(:,62)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(53)-M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92))+c(6)*(M(132)+M(134)-M(156)-M(158) &
    -M(201)+M(202)-M(225)+M(226))) * den(1)
  T4sum(1:70,147) = T4sum(1:70,147) + Gcoeff * G4tensor(:,63)
  Gcoeff = (c(5)*(-M(44)-M(47)-M(50)+M(53)+M(54)+M(55)-M(56)-M(59)-M(62)+M(65)+M(77)+M(89))+c(6)*(-M(142)-M(148)+M(156)+M(158) &
    -M(166)-M(172)+M(201)+M(225))) * den(1)
  T4sum(1:70,147) = T4sum(1:70,147) + Gcoeff * G4tensor(:,64)
  Gcoeff = (c(6)*(M(134)-M(137)+M(139)+M(141)-M(142)-M(144)-M(150)+M(153)-M(172)+M(188)-M(194)-M(200)+M(202)+M(214)+M(238) &
    -M(248))) * den(326)
  T3sum(1:35,83) = T3sum(1:35,83) + Gcoeff * G3tensor(:,106)
  Gcoeff = (c(5)*(M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59)-M(80))+c(6)*(-M(134)-M(141)+M(142)+M(144) &
    +M(172)+M(200)-M(202)-M(214))) * den(15)
  T4sum(1:70,94) = T4sum(1:70,94) + Gcoeff * G4tensor(:,68)
  Gcoeff = (c(5)*(M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59)-M(80))+c(6)*(-M(137)+M(139)-M(150)+M(153) &
    +M(188)-M(194)+M(238)-M(248))) * den(15)
  T4sum(1:70,96) = T4sum(1:70,96) + Gcoeff * G4tensor(:,92)
  Gcoeff = (c(6)*(-M(136)+M(138)-M(139)+M(140)-M(141)+M(144)-M(147)+M(150)+M(178)-M(190)+M(194)-M(196)+M(200)-M(214)+M(224) &
    -M(238))) * den(329)
  T3sum(1:35,83) = T3sum(1:35,83) + Gcoeff * G3tensor(:,107)
  Gcoeff = (c(5)*(M(27)-M(28)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(43)+M(50)+M(62)-M(92))+c(6)*(-M(136)-M(139)+M(140)+M(150) &
    +M(178)+M(194)-M(196)-M(238))) * den(17)
  T4sum(1:70,91) = T4sum(1:70,91) + Gcoeff * G4tensor(:,38)
  Gcoeff = (c(5)*(M(27)-M(28)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(43)+M(50)+M(62)-M(92))+c(6)*(-M(138)+M(141)-M(144)+M(147) &
    +M(190)-M(200)+M(214)-M(224))) * den(17)
  T4sum(1:70,93) = T4sum(1:70,93) + Gcoeff * G4tensor(:,95)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(134)-M(136)+M(140)-M(142)-M(148)+M(154)+M(164)-M(166)-M(172)+M(178)-M(196)+M(202)+M(226) &
    -M(250))) * den(162)
  T3sum(1:35,21) = T3sum(1:35,21) + Gcoeff * G3tensor(:,103)
  Gcoeff = (c(6)*(M(134)-M(136)-M(137)+M(138)+M(140)-M(142)-M(147)+M(153)-M(172)+M(178)+M(188)-M(190)-M(196)+M(202)+M(224) &
    -M(248))) * den(164)
  T3sum(1:35,83) = T3sum(1:35,83) + Gcoeff * G3tensor(:,108)
  Gcoeff = (c(6)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(783)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,109)
  Gcoeff = (c(5)*(-M(31)+M(32)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)+M(86)-M(98))+c(6)*(-M(137)+M(138)-M(147)+M(153) &
    +M(188)-M(190)+M(224)-M(248))) * den(27)
  T4sum(1:70,83) = T4sum(1:70,83) + Gcoeff * G4tensor(:,13)
  Gcoeff = (c(5)*(-M(41)-M(42)-M(43)+M(44)+M(47)+M(50)+M(56)+M(59)+M(62)-M(68)-M(80)-M(92))+c(6)*(-M(131)-M(136)+M(140)+M(154) &
    +M(164)+M(178)-M(196)-M(250))) * den(152)
  T4sum(1:70,146) = T4sum(1:70,146) + Gcoeff * G4tensor(:,35)
  Gcoeff = (c(5)*(-M(41)-M(42)-M(43)+M(44)+M(47)+M(50)+M(56)+M(59)+M(62)-M(68)-M(80)-M(92))+c(6)*(-M(132)-M(134)+M(142)+M(148) &
    +M(166)+M(172)-M(202)-M(226))) * den(152)
  T4sum(1:70,147) = T4sum(1:70,147) + Gcoeff * G4tensor(:,65)
  Gcoeff = (c(6)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(1164)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,110)
  Gcoeff = (c(5)*(M(42)+M(45)-M(47)-M(48)-M(49)+M(52)-M(59)-M(71)+M(80)+M(83)+M(86)-M(98))+c(6)*(M(138)-M(145)-M(147)+M(149) &
    -M(190)+M(218)+M(224)-M(236))) * den(142)
  T4sum(1:70,139) = T4sum(1:70,139) + Gcoeff * G4tensor(:,98)
  Gcoeff = (c(5)*(M(42)+M(45)-M(47)-M(48)-M(49)+M(52)-M(59)-M(71)+M(80)+M(83)+M(86)-M(98))+c(6)*(M(132)-M(143)-M(148)+M(151) &
    -M(166)+M(212)+M(226)-M(242))) * den(142)
  T4sum(1:70,140) = T4sum(1:70,140) + Gcoeff * G4tensor(:,69)
  Gcoeff = (c(6)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(1167)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,111)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(50)-M(51)-M(52)-M(62)-M(74)-M(86)+M(92)+M(95)+M(98))+c(6)*(M(137)+M(143)-M(151)-M(153) &
    -M(188)-M(212)+M(242)+M(248))) * den(307)
  T4sum(1:70,136) = T4sum(1:70,136) + Gcoeff * G4tensor(:,99)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(50)-M(51)-M(52)-M(62)-M(74)-M(86)+M(92)+M(95)+M(98))+c(6)*(M(131)+M(145)-M(149)-M(154) &
    -M(164)-M(218)+M(236)+M(250))) * den(307)
  T4sum(1:70,137) = T4sum(1:70,137) + Gcoeff * G4tensor(:,39)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42) &
    -M(43)-M(44)-M(45)-M(46)+M(47)+M(48)-M(49)+M(50)+M(51)+M(52)-M(56)+M(59)+M(62)+M(68)+M(71)+M(74)-M(80)-M(83)+M(86)-M(92)-M(95) &
    -M(98))+c(6)*(-M(137)+M(153)+M(188)-M(248))) * den(11)
  T5sum(1:126,170) = T5sum(1:126,170) + Gcoeff * G5tensor(:,6)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42) &
    -M(43)-M(44)-M(45)-M(46)+M(47)+M(48)+M(49)+M(50)+M(51)-M(52)-M(56)+M(59)+M(62)+M(68)+M(71)+M(74)-M(80)-M(83)-M(86)-M(92)-M(95) &
    +M(98))+c(6)*(-M(138)+M(147)+M(190)-M(224))) * den(11)
  T5sum(1:126,172) = T5sum(1:126,172) + Gcoeff * G5tensor(:,9)
  Gcoeff = (c(4)*(-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42) &
    -M(43)-M(44)-M(45)-M(46)-M(47)+M(48)-M(49)+M(50)+M(51)+M(52)-M(56)-M(59)+M(62)+M(68)+M(71)+M(74)+M(80)-M(83)+M(86)-M(92)-M(95) &
    -M(98))+c(6)*(-M(139)+M(150)+M(194)-M(238))) * den(11)
  T5sum(1:126,175) = T5sum(1:126,175) + Gcoeff * G5tensor(:,17)
  Gcoeff = (c(4)*(M(25)-M(26)-M(27)+M(28)-M(29)+M(30)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42) &
    +M(43)-M(44)-M(45)-M(46)+M(47)+M(48)+M(49)-M(50)+M(51)-M(52)-M(56)+M(59)-M(62)+M(68)+M(71)+M(74)-M(80)-M(83)-M(86)+M(92)-M(95) &
    +M(98))+c(6)*(-M(141)+M(144)+M(200)-M(214))) * den(11)
  T5sum(1:126,176) = T5sum(1:126,176) + Gcoeff * G5tensor(:,23)
  Gcoeff = (c(6)*(-M(148)+M(158)-M(161)+M(163)+M(165)-M(166)-M(168)-M(174)+M(177)+M(182)-M(192)-M(198)+M(201)+M(208)+M(232) &
    -M(246))) * den(388)
  T3sum(1:35,76) = T3sum(1:35,76) + Gcoeff * G3tensor(:,112)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(17)+M(18)+M(23)-M(24)+M(47)-M(54)+M(59)-M(77))+c(6)*(M(148)-M(158)-M(165)+M(166)+M(168) &
    +M(198)-M(201)-M(208))) * den(36)
  T4sum(1:70,121) = T4sum(1:70,121) + Gcoeff * G4tensor(:,70)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(17)+M(18)+M(23)-M(24)+M(47)-M(54)+M(59)-M(77))+c(6)*(-M(161)+M(163)-M(174)+M(177) &
    +M(182)-M(192)+M(232)-M(246))) * den(36)
  T4sum(1:70,123) = T4sum(1:70,123) + Gcoeff * G4tensor(:,100)
  Gcoeff = (c(6)*(M(154)-M(160)+M(162)-M(163)+M(164)-M(165)+M(168)-M(171)+M(174)-M(184)+M(192)-M(195)+M(198)-M(208)+M(222) &
    -M(232))) * den(391)
  T3sum(1:35,76) = T3sum(1:35,76) + Gcoeff * G3tensor(:,113)
  Gcoeff = (c(5)*(M(11)-M(12)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)+M(50)-M(55)+M(62)-M(89))+c(6)*(M(154)-M(160)-M(163)+M(164) &
    +M(174)+M(192)-M(195)-M(232))) * den(38)
  T4sum(1:70,118) = T4sum(1:70,118) + Gcoeff * G4tensor(:,40)
  Gcoeff = (c(5)*(M(11)-M(12)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)+M(50)-M(55)+M(62)-M(89))+c(6)*(-M(162)+M(165)-M(168)+M(171) &
    +M(184)-M(198)+M(208)-M(222))) * den(38)
  T4sum(1:70,120) = T4sum(1:70,120) + Gcoeff * G4tensor(:,103)
  Gcoeff = (c(6)*(-M(140)+M(142)+M(148)-M(154)+M(155)-M(156)-M(158)+M(160)-M(164)+M(166)+M(172)-M(178)+M(195)-M(201)-M(225) &
    +M(249))) * den(186)
  T3sum(1:35,21) = T3sum(1:35,21) + Gcoeff * G3tensor(:,104)
  Gcoeff = (c(6)*(-M(148)+M(154)+M(158)-M(160)-M(161)+M(162)+M(164)-M(166)-M(171)+M(177)+M(182)-M(184)-M(195)+M(201)+M(222) &
    -M(246))) * den(188)
  T3sum(1:35,76) = T3sum(1:35,76) + Gcoeff * G3tensor(:,114)
  Gcoeff = (c(6)*(-M(140)+M(142)+M(155)-M(156)-M(161)+M(162)-M(171)+M(172)+M(177)-M(178)+M(182)-M(184)+M(222)-M(225)-M(246) &
    +M(249))) * den(844)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,115)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)+M(37)-M(38)-M(39)+M(40)-M(61)+M(64)+M(87)-M(99))+c(6)*(-M(161)+M(162)-M(171)+M(177) &
    +M(182)-M(184)+M(222)-M(246))) * den(64)
  T4sum(1:70,56) = T4sum(1:70,56) + Gcoeff * G4tensor(:,106)
  Gcoeff = (c(5)*(-M(44)-M(47)-M(50)+M(53)+M(54)+M(55)-M(56)-M(59)-M(62)+M(65)+M(77)+M(89))+c(6)*(-M(140)-M(154)+M(155)+M(160) &
    -M(164)-M(178)+M(195)+M(249))) * den(176)
  T4sum(1:70,146) = T4sum(1:70,146) + Gcoeff * G4tensor(:,36)
  Gcoeff = (c(5)*(-M(44)-M(47)-M(50)+M(53)+M(54)+M(55)-M(56)-M(59)-M(62)+M(65)+M(77)+M(89))+c(6)*(-M(142)-M(148)+M(156)+M(158) &
    -M(166)-M(172)+M(201)+M(225))) * den(176)
  T4sum(1:70,147) = T4sum(1:70,147) + Gcoeff * G4tensor(:,66)
  Gcoeff = (c(6)*(M(142)-M(156)+M(162)+M(167)-M(169)-M(171)+M(172)+M(173)-M(175)-M(184)-M(206)+M(216)+M(222)-M(225)-M(230) &
    +M(240))) * den(1176)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,116)
  Gcoeff = (c(5)*(-M(47)+M(54)+M(57)-M(59)-M(60)-M(61)+M(64)-M(72)+M(77)+M(84)+M(87)-M(99))+c(6)*(M(162)-M(169)-M(171)+M(173) &
    -M(184)+M(216)+M(222)-M(230))) * den(166)
  T4sum(1:70,151) = T4sum(1:70,151) + Gcoeff * G4tensor(:,107)
  Gcoeff = (c(5)*(-M(47)+M(54)+M(57)-M(59)-M(60)-M(61)+M(64)-M(72)+M(77)+M(84)+M(87)-M(99))+c(6)*(-M(142)+M(156)-M(167)-M(172) &
    +M(175)+M(206)+M(225)-M(240))) * den(166)
  T4sum(1:70,152) = T4sum(1:70,152) + Gcoeff * G4tensor(:,71)
  Gcoeff = (c(6)*(-M(140)+M(155)-M(161)-M(167)+M(169)-M(173)+M(175)+M(177)-M(178)+M(182)+M(206)-M(216)+M(230)-M(240)-M(246) &
    +M(249))) * den(1179)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,117)
  Gcoeff = (c(5)*(-M(50)+M(55)+M(58)+M(61)-M(62)-M(63)-M(64)-M(75)-M(87)+M(89)+M(96)+M(99))+c(6)*(M(161)+M(167)-M(175)-M(177) &
    -M(182)-M(206)+M(240)+M(246))) * den(369)
  T4sum(1:70,148) = T4sum(1:70,148) + Gcoeff * G4tensor(:,108)
  Gcoeff = (c(5)*(-M(50)+M(55)+M(58)+M(61)-M(62)-M(63)-M(64)-M(75)-M(87)+M(89)+M(96)+M(99))+c(6)*(-M(140)+M(155)+M(169)-M(173) &
    -M(178)-M(216)+M(230)+M(249))) * den(369)
  T4sum(1:70,149) = T4sum(1:70,149) + Gcoeff * G4tensor(:,41)
  Gcoeff = (c(4)*(-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(17)-M(18)+M(21)-M(22)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)+M(44)-M(47) &
    +M(50)-M(53)+M(54)-M(55)+M(56)+M(57)-M(58)-M(59)-M(60)-M(61)+M(62)+M(63)+M(64)-M(65)-M(72)+M(75)+M(77)+M(84)+M(87)-M(89)-M(96) &
    -M(99))+c(6)*(-M(167)+M(175)+M(206)-M(240))) * den(32)
  T5sum(1:126,157) = T5sum(1:126,157) + Gcoeff * G5tensor(:,26)
  Gcoeff = (c(4)*(-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(17)-M(18)+M(21)-M(22)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)+M(44)-M(47) &
    -M(50)-M(53)+M(54)+M(55)+M(56)+M(57)+M(58)-M(59)-M(60)+M(61)-M(62)-M(63)-M(64)-M(65)-M(72)-M(75)+M(77)+M(84)-M(87)+M(89)+M(96) &
    +M(99))+c(6)*(M(161)-M(177)-M(182)+M(246))) * den(32)
  T5sum(1:126,158) = T5sum(1:126,158) + Gcoeff * G5tensor(:,33)
  Gcoeff = (c(4)*(-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)+M(37)-M(38)-M(39)+M(40)+M(44)+M(47) &
    -M(50)-M(53)-M(54)+M(55)+M(56)-M(57)+M(58)+M(59)+M(60)+M(61)-M(62)-M(63)-M(64)-M(65)+M(72)-M(75)-M(77)-M(84)-M(87)+M(89)+M(96) &
    +M(99))+c(6)*(M(169)-M(173)-M(216)+M(230))) * den(32)
  T5sum(1:126,159) = T5sum(1:126,159) + Gcoeff * G5tensor(:,20)
  Gcoeff = (c(4)*(-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)+M(37)-M(38)-M(39)+M(40)+M(44)-M(47) &
    -M(50)-M(53)+M(54)+M(55)+M(56)+M(57)+M(58)-M(59)-M(60)-M(61)-M(62)-M(63)+M(64)-M(65)-M(72)-M(75)+M(77)+M(84)+M(87)+M(89)+M(96) &
    -M(99))+c(6)*(M(162)-M(171)-M(184)+M(222))) * den(32)
  T5sum(1:126,160) = T5sum(1:126,160) + Gcoeff * G5tensor(:,34)
  Gcoeff = (c(6)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(416)
  T3sum(1:35,56) = T3sum(1:35,56) + Gcoeff * G3tensor(:,118)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)-M(19)+M(20)+M(42)-M(54)-M(77)+M(80))+c(6)*(M(132)-M(156)-M(203)+M(209)-M(225) &
    +M(226)+M(239)-M(241))) * den(44)
  T4sum(1:70,131) = T4sum(1:70,131) + Gcoeff * G4tensor(:,72)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)-M(19)+M(20)+M(42)-M(54)-M(77)+M(80))+c(6)*(M(180)-M(186)-M(215)+M(217)-M(221) &
    +M(223)+M(227)-M(233))) * den(44)
  T4sum(1:70,132) = T4sum(1:70,132) + Gcoeff * G4tensor(:,109)
  Gcoeff = (c(6)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(419)
  T3sum(1:35,56) = T3sum(1:35,56) + Gcoeff * G3tensor(:,119)
  Gcoeff = (c(5)*(M(1)-M(2)+M(3)-M(4)-M(7)+M(8)-M(13)+M(14)+M(43)-M(55)-M(89)+M(92))+c(6)*(M(131)-M(155)+M(215)-M(217)-M(227) &
    +M(233)-M(249)+M(250))) * den(46)
  T4sum(1:70,128) = T4sum(1:70,128) + Gcoeff * G4tensor(:,42)
  Gcoeff = (c(5)*(M(1)-M(2)+M(3)-M(4)-M(7)+M(8)-M(13)+M(14)+M(43)-M(55)-M(89)+M(92))+c(6)*(M(179)-M(185)+M(203)-M(209)-M(239) &
    +M(241)-M(245)+M(247))) * den(46)
  T4sum(1:70,129) = T4sum(1:70,129) + Gcoeff * G4tensor(:,112)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(134)-M(136)+M(155)-M(156)-M(158)+M(160)+M(195)-M(196)-M(201)+M(202)-M(225)+M(226)+M(249) &
    -M(250))) * den(210)
  T3sum(1:35,21) = T3sum(1:35,21) + Gcoeff * G3tensor(:,105)
  Gcoeff = (c(6)*(M(131)-M(132)-M(155)+M(156)-M(179)+M(180)+M(185)-M(186)-M(221)+M(223)+M(225)-M(226)+M(245)-M(247)-M(249) &
    +M(250))) * den(212)
  T3sum(1:35,56) = T3sum(1:35,56) + Gcoeff * G3tensor(:,120)
  Gcoeff = (c(6)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(900)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,121)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(37)+M(38)+M(39)-M(40)-M(104)+M(106)+M(117)-M(123))+c(6)*(-M(179)+M(180)+M(185)-M(186) &
    -M(221)+M(223)+M(245)-M(247))) * den(99)
  T4sum(1:70,20) = T4sum(1:70,20) + Gcoeff * G4tensor(:,115)
  Gcoeff = (c(5)*(-M(41)-M(42)-M(43)+M(53)+M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92))+c(6)*(-M(131)-M(136)+M(155)+M(160) &
    +M(195)-M(196)+M(249)-M(250))) * den(200)
  T4sum(1:70,146) = T4sum(1:70,146) + Gcoeff * G4tensor(:,37)
  Gcoeff = (c(5)*(-M(41)-M(42)-M(43)+M(53)+M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92))+c(6)*(-M(132)-M(134)+M(156)+M(158) &
    +M(201)-M(202)+M(225)-M(226))) * den(200)
  T4sum(1:70,147) = T4sum(1:70,147) + Gcoeff * G4tensor(:,67)
  Gcoeff = (c(6)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(1189)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,122)
  Gcoeff = (c(5)*(-M(42)+M(54)+M(77)-M(80)+M(101)-M(103)-M(104)+M(106)-M(109)+M(115)+M(117)-M(123))+c(6)*(-M(179)+M(185)+M(191) &
    -M(193)-M(228)+M(234)+M(245)-M(247))) * den(192)
  T4sum(1:70,160) = T4sum(1:70,160) + Gcoeff * G4tensor(:,116)
  Gcoeff = (c(5)*(-M(42)+M(54)+M(77)-M(80)+M(101)-M(103)-M(104)+M(106)-M(109)+M(115)+M(117)-M(123))+c(6)*(-M(134)+M(158)-M(197) &
    +M(199)+M(201)-M(202)+M(204)-M(210))) * den(192)
  T4sum(1:70,161) = T4sum(1:70,161) + Gcoeff * G4tensor(:,73)
  Gcoeff = (c(6)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(1191)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,123)
  Gcoeff = (c(5)*(-M(43)+M(55)+M(89)-M(92)+M(102)+M(104)-M(105)-M(106)-M(111)-M(117)+M(121)+M(123))+c(6)*(-M(180)+M(186)+M(197) &
    -M(199)-M(204)+M(210)+M(221)-M(223))) * den(400)
  T4sum(1:70,157) = T4sum(1:70,157) + Gcoeff * G4tensor(:,117)
  Gcoeff = (c(5)*(-M(43)+M(55)+M(89)-M(92)+M(102)+M(104)-M(105)-M(106)-M(111)-M(117)+M(121)+M(123))+c(6)*(-M(136)+M(160)-M(191) &
    +M(193)+M(195)-M(196)+M(228)-M(234))) * den(400)
  T4sum(1:70,158) = T4sum(1:70,158) + Gcoeff * G4tensor(:,43)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(5)+M(6)-M(7)+M(8)+M(13)-M(14)+M(19)-M(20)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42)+M(43)-M(53) &
    +M(54)-M(55)-M(65)+M(68)+M(77)-M(80)-M(89)+M(92)+M(101)-M(102)-M(103)-M(104)+M(105)+M(106)-M(109)+M(111)+M(115)+M(117)-M(121) &
    -M(123))+c(6)*(-M(197)+M(199)+M(204)-M(210))) * den(41)
  T5sum(1:126,109) = T5sum(1:126,109) + Gcoeff * G5tensor(:,27)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(5)+M(6)-M(7)+M(8)+M(13)-M(14)+M(19)-M(20)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42)-M(43)-M(53) &
    +M(54)+M(55)-M(65)+M(68)+M(77)-M(80)+M(89)-M(92)+M(101)+M(102)-M(103)+M(104)-M(105)-M(106)-M(109)-M(111)+M(115)-M(117)+M(121) &
    +M(123))+c(6)*(-M(180)+M(186)+M(221)-M(223))) * den(41)
  T5sum(1:126,110) = T5sum(1:126,110) + Gcoeff * G5tensor(:,37)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)-M(5)+M(6)+M(7)-M(8)+M(13)-M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42)-M(43)-M(53) &
    -M(54)+M(55)-M(65)+M(68)-M(77)+M(80)+M(89)-M(92)-M(101)+M(102)+M(103)+M(104)-M(105)-M(106)+M(109)-M(111)-M(115)-M(117)+M(121) &
    +M(123))+c(6)*(-M(191)+M(193)+M(228)-M(234))) * den(41)
  T5sum(1:126,111) = T5sum(1:126,111) + Gcoeff * G5tensor(:,21)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)-M(5)+M(6)+M(7)-M(8)+M(13)-M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42)-M(43)-M(53) &
    +M(54)+M(55)-M(65)+M(68)+M(77)-M(80)+M(89)-M(92)+M(101)+M(102)-M(103)-M(104)-M(105)+M(106)-M(109)-M(111)+M(115)+M(117)+M(121) &
    -M(123))+c(6)*(-M(179)+M(185)+M(245)-M(247))) * den(41)
  T5sum(1:126,112) = T5sum(1:126,112) + Gcoeff * G5tensor(:,38)
  Gcoeff = (c(6)*(M(149)-M(151)-M(162)+M(165)+M(186)-M(189)-M(207)+M(208)+M(211)-M(212)-M(217)+M(218)+M(221)-M(222)-M(227) &
    +M(229))) * den(1046)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,124)
  Gcoeff = (c(5)*(-M(5)+M(6)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)+M(52)-M(79)+M(86)-M(91))+c(6)*(M(151)+M(162)-M(165)-M(208)-M(211) &
    +M(212)+M(222)-M(229))) * den(54)
  T4sum(1:70,100) = T4sum(1:70,100) + Gcoeff * G4tensor(:,104)
  Gcoeff = (c(5)*(-M(5)+M(6)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)+M(52)-M(79)+M(86)-M(91))+c(6)*(M(149)+M(186)-M(189)-M(207)-M(217) &
    +M(218)+M(221)-M(227))) * den(54)
  T4sum(1:70,101) = T4sum(1:70,101) + Gcoeff * G4tensor(:,110)
  Gcoeff = (c(6)*(-M(138)+M(141)+M(173)-M(175)+M(180)-M(183)+M(205)-M(206)-M(213)+M(214)-M(215)+M(216)+M(223)-M(224)-M(233) &
    +M(235))) * den(1002)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,125)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)-M(27)+M(28)+M(33)-M(34)+M(64)-M(82)+M(87)-M(94))+c(6)*(M(138)-M(141)+M(175)-M(205)+M(206) &
    -M(214)+M(224)-M(235))) * den(75)
  T4sum(1:70,64) = T4sum(1:70,64) + Gcoeff * G4tensor(:,96)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)-M(27)+M(28)+M(33)-M(34)+M(64)-M(82)+M(87)-M(94))+c(6)*(M(173)+M(180)-M(183)-M(213)-M(215) &
    +M(216)+M(223)-M(233))) * den(75)
  T4sum(1:70,65) = T4sum(1:70,65) + Gcoeff * G4tensor(:,111)
  Gcoeff = (c(6)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(1230)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,126)
  Gcoeff = (c(5)*(-M(42)-M(45)-M(52)+M(54)+M(66)+M(77)+M(78)+M(79)-M(80)-M(83)-M(86)+M(91))+c(6)*(-M(138)-M(149)+M(189)+M(205) &
    +M(207)-M(218)-M(224)+M(235))) * den(250)
  T4sum(1:70,139) = T4sum(1:70,139) + Gcoeff * G4tensor(:,118)
  Gcoeff = (c(5)*(-M(42)-M(45)-M(52)+M(54)+M(66)+M(77)+M(78)+M(79)-M(80)-M(83)-M(86)+M(91))+c(6)*(-M(132)-M(151)+M(165)+M(203) &
    +M(208)-M(212)-M(226)+M(241))) * den(250)
  T4sum(1:70,140) = T4sum(1:70,140) + Gcoeff * G4tensor(:,74)
  Gcoeff = (c(6)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(1233)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,127)
  Gcoeff = (c(5)*(M(42)-M(54)-M(57)-M(64)+M(69)-M(77)+M(80)+M(81)+M(82)-M(84)-M(87)+M(94))+c(6)*(-M(162)-M(173)+M(183)+M(211) &
    +M(213)-M(216)-M(222)+M(229))) * den(232)
  T4sum(1:70,151) = T4sum(1:70,151) + Gcoeff * G4tensor(:,119)
  Gcoeff = (c(5)*(M(42)-M(54)-M(57)-M(64)+M(69)-M(77)+M(80)+M(81)+M(82)-M(84)-M(87)+M(94))+c(6)*(M(141)-M(156)-M(175)-M(206) &
    +M(209)+M(214)-M(225)+M(239))) * den(232)
  T4sum(1:70,152) = T4sum(1:70,152) + Gcoeff * G4tensor(:,75)
  Gcoeff = (c(6)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(1237)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,128)
  Gcoeff = (c(6)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(1238)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,129)
  Gcoeff = (c(5)*(-M(52)-M(64)-M(76)+M(79)+M(82)+M(85)-M(86)-M(87)-M(88)+M(91)+M(94)+M(97))+c(6)*(-M(138)-M(162)+M(205)+M(211) &
    -M(222)-M(224)+M(229)+M(235))) * den(415)
  T4sum(1:70,186) = T4sum(1:70,186) + Gcoeff * G4tensor(:,14)
  Gcoeff = (c(4)*(M(1)-M(2)+M(5)-M(6)-M(7)+M(8)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)+M(42)+M(45)-M(52) &
    -M(54)-M(57)-M(64)-M(66)+M(69)-M(76)-M(77)-M(78)+M(79)+M(80)+M(81)+M(82)+M(83)-M(84)+M(85)-M(86)-M(87)-M(88)+M(91)+M(94) &
    +M(97))+c(6)*(-M(162)+M(211)-M(222)+M(229))) * den(43)
  T5sum(1:126,139) = T5sum(1:126,139) + Gcoeff * G5tensor(:,12)
  Gcoeff = (c(4)*(M(1)-M(2)+M(5)-M(6)-M(7)+M(8)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)-M(42)+M(45)-M(52) &
    +M(54)+M(57)+M(64)-M(66)-M(69)-M(76)+M(77)-M(78)+M(79)-M(80)-M(81)-M(82)+M(83)+M(84)+M(85)-M(86)+M(87)-M(88)+M(91)-M(94) &
    +M(97))+c(6)*(M(173)-M(183)-M(213)+M(216))) * den(43)
  T5sum(1:126,140) = T5sum(1:126,140) + Gcoeff * G5tensor(:,35)
  Gcoeff = (c(4)*(M(1)-M(2)+M(5)-M(6)-M(7)+M(8)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)+M(42)+M(45)+M(52) &
    -M(54)-M(57)+M(64)-M(66)+M(69)+M(76)-M(77)-M(78)-M(79)+M(80)+M(81)-M(82)+M(83)-M(84)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94) &
    -M(97))+c(6)*(M(138)-M(205)+M(224)-M(235))) * den(43)
  T5sum(1:126,141) = T5sum(1:126,141) + Gcoeff * G5tensor(:,10)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(11)-M(12)+M(19)-M(20)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(42)+M(45)+M(52) &
    -M(54)-M(57)-M(64)-M(66)+M(69)+M(76)-M(77)-M(78)-M(79)+M(80)+M(81)+M(82)+M(83)-M(84)-M(85)+M(86)-M(87)+M(88)-M(91)+M(94) &
    -M(97))+c(6)*(M(141)-M(175)-M(206)+M(214))) * den(43)
  T5sum(1:126,142) = T5sum(1:126,142) + Gcoeff * G5tensor(:,24)
  Gcoeff = (c(6)*(-M(143)+M(145)+M(161)-M(163)-M(185)+M(187)+M(203)-M(205)+M(231)-M(232)-M(235)+M(236)+M(241)-M(242)-M(245) &
    +M(246))) * den(1058)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,130)
  Gcoeff = (c(5)*(-M(3)+M(4)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)+M(49)-M(79)-M(91)+M(98))+c(6)*(M(145)+M(161)-M(163)-M(205)-M(232) &
    -M(235)+M(236)+M(246))) * den(56)
  T4sum(1:70,103) = T4sum(1:70,103) + Gcoeff * G4tensor(:,101)
  Gcoeff = (c(5)*(-M(3)+M(4)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)+M(49)-M(79)-M(91)+M(98))+c(6)*(M(143)+M(185)-M(187)-M(203)-M(231) &
    -M(241)+M(242)+M(245))) * den(56)
  T4sum(1:70,104) = T4sum(1:70,104) + Gcoeff * G4tensor(:,113)
  Gcoeff = (c(6)*(M(144)-M(147)-M(174)+M(177)-M(181)+M(182)+M(189)-M(190)+M(191)-M(192)-M(199)+M(200)-M(204)+M(207)+M(234) &
    -M(237))) * den(958)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,131)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)+M(27)-M(28)-M(33)+M(34)+M(106)-M(114)+M(117)-M(120))+c(6)*(-M(144)+M(147)-M(189)+M(190) &
    +M(199)-M(200)+M(204)-M(207))) * den(108)
  T4sum(1:70,28) = T4sum(1:70,28) + Gcoeff * G4tensor(:,97)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)+M(27)-M(28)-M(33)+M(34)+M(106)-M(114)+M(117)-M(120))+c(6)*(-M(174)+M(177)-M(181)+M(182) &
    +M(191)-M(192)+M(234)-M(237))) * den(108)
  T4sum(1:70,29) = T4sum(1:70,29) + Gcoeff * G4tensor(:,102)
  Gcoeff = (c(6)*(M(143)-M(145)-M(147)+M(148)-M(165)+M(166)+M(189)-M(190)-M(203)+M(205)+M(207)-M(208)+M(235)-M(236)-M(241) &
    +M(242))) * den(1242)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,132)
  Gcoeff = (c(5)*(-M(47)-M(48)-M(49)+M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98))+c(6)*(-M(145)-M(147)+M(189)-M(190) &
    +M(205)+M(207)+M(235)-M(236))) * den(255)
  T4sum(1:70,139) = T4sum(1:70,139) + Gcoeff * G4tensor(:,120)
  Gcoeff = (c(5)*(-M(47)-M(48)-M(49)+M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98))+c(6)*(-M(143)-M(148)+M(165)-M(166) &
    +M(203)+M(208)+M(241)-M(242))) * den(255)
  T4sum(1:70,140) = T4sum(1:70,140) + Gcoeff * G4tensor(:,76)
  Gcoeff = (c(6)*(-M(147)+M(148)+M(161)-M(163)-M(165)+M(166)-M(185)+M(187)+M(189)-M(190)+M(207)-M(208)+M(231)-M(232)-M(245) &
    +M(246))) * den(1245)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,133)
  Gcoeff = (c(6)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(1247)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,134)
  Gcoeff = (c(5)*(M(47)-M(54)+M(59)-M(77)-M(101)-M(106)+M(107)+M(113)+M(114)-M(115)-M(117)+M(120))+c(6)*(M(181)-M(185)+M(187) &
    -M(191)+M(231)-M(234)+M(237)-M(245))) * den(216)
  T4sum(1:70,160) = T4sum(1:70,160) + Gcoeff * G4tensor(:,121)
  Gcoeff = (c(5)*(M(47)-M(54)+M(59)-M(77)-M(101)-M(106)+M(107)+M(113)+M(114)-M(115)-M(117)+M(120))+c(6)*(M(144)-M(158)+M(168) &
    +M(198)-M(199)+M(200)-M(201)-M(204))) * den(216)
  T4sum(1:70,161) = T4sum(1:70,161) + Gcoeff * G4tensor(:,77)
  Gcoeff = (c(6)*(-M(147)+M(158)-M(168)-M(174)+M(177)+M(182)-M(185)+M(187)+M(189)-M(190)-M(192)-M(198)+M(201)+M(207)+M(231) &
    -M(245))) * den(1250)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,135)
  Gcoeff = (c(5)*(-M(49)+M(79)+M(91)-M(98)-M(106)-M(112)+M(114)+M(116)-M(117)-M(118)+M(120)+M(122))+c(6)*(-M(147)-M(185)+M(187) &
    +M(189)-M(190)+M(207)+M(231)-M(245))) * den(387)
  T4sum(1:70,189) = T4sum(1:70,189) + Gcoeff * G4tensor(:,15)
  Gcoeff = (c(4)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)+M(47)+M(48)-M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)+M(79)+M(91)-M(98)-M(101)-M(106)+M(107)-M(112)+M(113)+M(114)-M(115)+M(116)-M(117)-M(118) &
    +M(120)+M(122))+c(6)*(-M(185)+M(187)+M(231)-M(245))) * den(35)
  T5sum(1:126,91) = T5sum(1:126,91) + Gcoeff * G5tensor(:,13)
  Gcoeff = (c(4)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)-M(47)+M(48)-M(49) &
    +M(54)-M(59)-M(66)+M(71)+M(77)-M(78)+M(79)+M(91)-M(98)+M(101)+M(106)-M(107)-M(112)-M(113)-M(114)+M(115)+M(116)+M(117)-M(118) &
    -M(120)+M(122))+c(6)*(-M(181)+M(191)+M(234)-M(237))) * den(35)
  T5sum(1:126,92) = T5sum(1:126,92) + Gcoeff * G5tensor(:,31)
  Gcoeff = (c(4)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)+M(47)+M(48)+M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98)-M(101)+M(106)+M(107)+M(112)+M(113)-M(114)-M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(M(147)-M(189)+M(190)-M(207))) * den(35)
  T5sum(1:126,93) = T5sum(1:126,93) + Gcoeff * G5tensor(:,11)
  Gcoeff = (c(4)*(M(3)-M(4)-M(9)+M(10)-M(13)+M(14)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)-M(27)+M(28)+M(33)-M(34)+M(47)+M(48)+M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98)-M(101)-M(106)+M(107)+M(112)+M(113)+M(114)-M(115)-M(116)-M(117)+M(118) &
    +M(120)-M(122))+c(6)*(M(144)-M(199)+M(200)-M(204))) * den(35)
  T5sum(1:126,94) = T5sum(1:126,94) + Gcoeff * G5tensor(:,25)
  Gcoeff = (c(6)*(M(137)-M(139)-M(167)+M(169)-M(179)+M(181)+M(209)-M(211)-M(229)+M(230)+M(237)-M(238)+M(239)-M(240)-M(247) &
    +M(248))) * den(1014)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,136)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(25)+M(26)+M(29)-M(30)+M(61)-M(82)-M(94)+M(99))+c(6)*(M(137)-M(139)+M(169)-M(211)-M(229) &
    +M(230)-M(238)+M(248))) * den(77)
  T4sum(1:70,67) = T4sum(1:70,67) + Gcoeff * G4tensor(:,93)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(25)+M(26)+M(29)-M(30)+M(61)-M(82)-M(94)+M(99))+c(6)*(M(167)+M(179)-M(181)-M(209)-M(237) &
    -M(239)+M(240)+M(247))) * den(77)
  T4sum(1:70,68) = T4sum(1:70,68) + Gcoeff * G4tensor(:,114)
  Gcoeff = (c(6)*(-M(150)+M(153)+M(168)-M(171)+M(183)-M(184)-M(187)+M(188)+M(193)-M(194)-M(197)+M(198)-M(210)+M(213)+M(228) &
    -M(231))) * den(970)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,137)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)+M(104)-M(114)-M(120)+M(123))+c(6)*(-M(150)+M(153)-M(187)+M(188) &
    +M(193)-M(194)+M(228)-M(231))) * den(110)
  T4sum(1:70,31) = T4sum(1:70,31) + Gcoeff * G4tensor(:,94)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)+M(104)-M(114)-M(120)+M(123))+c(6)*(-M(168)+M(171)-M(183)+M(184) &
    +M(197)-M(198)+M(210)-M(213))) * den(110)
  T4sum(1:70,32) = T4sum(1:70,32) + Gcoeff * G4tensor(:,105)
  Gcoeff = (c(6)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(1255)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,138)
  Gcoeff = (c(5)*(M(42)-M(47)-M(59)-M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99))+c(6)*(-M(169)-M(171)+M(183)-M(184) &
    +M(211)+M(213)+M(229)-M(230))) * den(239)
  T4sum(1:70,151) = T4sum(1:70,151) + Gcoeff * G4tensor(:,122)
  Gcoeff = (c(5)*(M(42)-M(47)-M(59)-M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99))+c(6)*(M(141)-M(142)-M(167)-M(172) &
    +M(209)+M(214)+M(239)-M(240))) * den(239)
  T4sum(1:70,152) = T4sum(1:70,152) + Gcoeff * G4tensor(:,78)
  Gcoeff = (c(6)*(M(137)-M(139)-M(141)+M(142)-M(171)+M(172)-M(179)+M(181)+M(183)-M(184)+M(213)-M(214)+M(237)-M(238)-M(247) &
    +M(248))) * den(1257)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,139)
  Gcoeff = (c(6)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(1259)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,140)
  Gcoeff = (c(5)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(6)*(-M(179)+M(181)+M(187) &
    -M(193)-M(228)+M(231)+M(237)-M(247))) * den(221)
  T4sum(1:70,160) = T4sum(1:70,160) + Gcoeff * G4tensor(:,123)
  Gcoeff = (c(5)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(6)*(-M(134)+M(144)+M(168) &
    -M(197)+M(198)+M(200)-M(202)-M(210))) * den(221)
  T4sum(1:70,161) = T4sum(1:70,161) + Gcoeff * G4tensor(:,79)
  Gcoeff = (c(6)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(1261)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,141)
  Gcoeff = (c(5)*(-M(61)+M(82)+M(94)-M(99)-M(104)+M(114)+M(120)-M(123)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(171)-M(179)+M(181) &
    +M(183)-M(184)+M(213)+M(237)-M(247))) * den(325)
  T4sum(1:70,192) = T4sum(1:70,192) + Gcoeff * G4tensor(:,4)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)+M(31)-M(32)-M(35)+M(36)-M(42)+M(47)+M(59) &
    +M(60)-M(61)-M(69)+M(72)-M(80)-M(81)+M(82)+M(94)-M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(179)+M(181)+M(237)-M(247))) * den(14)
  T5sum(1:126,55) = T5sum(1:126,55) + Gcoeff * G5tensor(:,1)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)-M(61)-M(69)+M(72)+M(80)-M(81)+M(82)+M(94)-M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(187)+M(193)+M(228)-M(231))) * den(14)
  T5sum(1:126,56) = T5sum(1:126,56) + Gcoeff * G5tensor(:,29)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)+M(31)-M(32)-M(35)+M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(171)-M(183)+M(184)-M(213))) * den(14)
  T5sum(1:126,57) = T5sum(1:126,57) + Gcoeff * G5tensor(:,2)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(168)-M(197)+M(198)-M(210))) * den(14)
  T5sum(1:126,58) = T5sum(1:126,58) + Gcoeff * G5tensor(:,28)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(1278)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,142)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(55)+M(67)+M(79)+M(89)+M(90)+M(91)-M(92)-M(95)-M(98))+c(6)*(-M(137)-M(143)+M(187)+M(211) &
    +M(229)+M(231)-M(242)-M(248))) * den(259)
  T4sum(1:70,136) = T4sum(1:70,136) + Gcoeff * G4tensor(:,124)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(55)+M(67)+M(79)+M(89)+M(90)+M(91)-M(92)-M(95)-M(98))+c(6)*(-M(131)-M(145)+M(163)+M(217) &
    +M(227)+M(232)-M(236)-M(250))) * den(259)
  T4sum(1:70,137) = T4sum(1:70,137) + Gcoeff * G4tensor(:,44)
  Gcoeff = (c(6)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(1281)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,143)
  Gcoeff = (c(5)*(M(43)-M(55)-M(58)-M(61)+M(70)+M(82)-M(89)+M(92)+M(93)+M(94)-M(96)-M(99))+c(6)*(-M(161)-M(167)+M(181)+M(205) &
    +M(235)+M(237)-M(240)-M(246))) * den(241)
  T4sum(1:70,148) = T4sum(1:70,148) + Gcoeff * G4tensor(:,125)
  Gcoeff = (c(5)*(M(43)-M(55)-M(58)-M(61)+M(70)+M(82)-M(89)+M(92)+M(93)+M(94)-M(96)-M(99))+c(6)*(M(139)-M(155)-M(169)+M(215) &
    -M(230)+M(233)+M(238)-M(249))) * den(241)
  T4sum(1:70,149) = T4sum(1:70,149) + Gcoeff * G4tensor(:,45)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(1285)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,144)
  Gcoeff = (c(6)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(1286)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,145)
  Gcoeff = (c(5)*(M(49)+M(61)+M(73)-M(79)-M(82)-M(85)-M(91)-M(94)-M(97)+M(98)+M(99)+M(100))+c(6)*(M(137)+M(161)-M(205)-M(211) &
    -M(229)-M(235)+M(246)+M(248))) * den(418)
  T4sum(1:70,186) = T4sum(1:70,186) + Gcoeff * G4tensor(:,16)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(7)-M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(25)+M(26)+M(29)-M(30)+M(43)+M(46)-M(49) &
    -M(55)-M(58)-M(61)-M(67)+M(70)-M(73)+M(79)+M(82)+M(85)-M(89)-M(90)+M(91)+M(92)+M(93)+M(94)+M(95)-M(96)+M(97)-M(98)-M(99) &
    -M(100))+c(6)*(-M(161)+M(205)+M(235)-M(246))) * den(45)
  T5sum(1:126,127) = T5sum(1:126,127) + Gcoeff * G5tensor(:,14)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(7)-M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(25)+M(26)+M(29)-M(30)-M(43)+M(46)-M(49) &
    +M(55)+M(58)+M(61)-M(67)-M(70)-M(73)+M(79)-M(82)+M(85)+M(89)-M(90)+M(91)-M(92)-M(93)-M(94)+M(95)+M(96)+M(97)-M(98)+M(99) &
    -M(100))+c(6)*(M(167)-M(181)-M(237)+M(240))) * den(45)
  T5sum(1:126,128) = T5sum(1:126,128) + Gcoeff * G5tensor(:,36)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(7)-M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(25)+M(26)+M(29)-M(30)+M(43)+M(46)+M(49) &
    -M(55)-M(58)+M(61)-M(67)+M(70)+M(73)-M(79)-M(82)-M(85)-M(89)-M(90)-M(91)+M(92)+M(93)-M(94)+M(95)-M(96)-M(97)+M(98)+M(99) &
    +M(100))+c(6)*(M(137)-M(211)-M(229)+M(248))) * den(45)
  T5sum(1:126,129) = T5sum(1:126,129) + Gcoeff * G5tensor(:,7)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)+M(43)+M(46)+M(49) &
    -M(55)-M(58)-M(61)-M(67)+M(70)+M(73)-M(79)+M(82)-M(85)-M(89)-M(90)-M(91)+M(92)+M(93)+M(94)+M(95)-M(96)-M(97)+M(98)-M(99) &
    +M(100))+c(6)*(M(139)-M(169)-M(230)+M(238))) * den(45)
  T5sum(1:126,130) = T5sum(1:126,130) + Gcoeff * G5tensor(:,18)
  Gcoeff = (c(6)*(-M(149)+M(151)+M(153)-M(154)+M(163)-M(164)-M(187)+M(188)-M(211)+M(212)+M(217)-M(218)+M(227)-M(229)-M(231) &
    +M(232))) * den(1290)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,146)
  Gcoeff = (c(5)*(-M(50)-M(51)-M(52)+M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91))+c(6)*(-M(151)-M(153)+M(187)-M(188) &
    +M(211)-M(212)+M(229)+M(231))) * den(262)
  T4sum(1:70,136) = T4sum(1:70,136) + Gcoeff * G4tensor(:,126)
  Gcoeff = (c(5)*(-M(50)-M(51)-M(52)+M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91))+c(6)*(-M(149)-M(154)+M(163)-M(164) &
    +M(217)-M(218)+M(227)+M(232))) * den(262)
  T4sum(1:70,137) = T4sum(1:70,137) + Gcoeff * G4tensor(:,46)
  Gcoeff = (c(6)*(M(153)-M(154)-M(162)+M(163)-M(164)+M(165)+M(186)-M(187)+M(188)-M(189)-M(207)+M(208)+M(221)-M(222)-M(231) &
    +M(232))) * den(1293)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,147)
  Gcoeff = (c(6)*(M(150)-M(160)+M(174)-M(183)+M(186)-M(189)+M(192)-M(193)+M(194)-M(195)+M(197)-M(207)+M(210)-M(213)+M(221) &
    -M(228))) * den(1295)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,148)
  Gcoeff = (c(5)*(M(50)-M(55)+M(62)-M(89)-M(102)-M(104)+M(108)+M(114)+M(119)+M(120)-M(121)-M(123))+c(6)*(M(183)-M(186)+M(189) &
    -M(197)+M(207)-M(210)+M(213)-M(221))) * den(224)
  T4sum(1:70,157) = T4sum(1:70,157) + Gcoeff * G4tensor(:,127)
  Gcoeff = (c(5)*(M(50)-M(55)+M(62)-M(89)-M(102)-M(104)+M(108)+M(114)+M(119)+M(120)-M(121)-M(123))+c(6)*(M(150)-M(160)+M(174) &
    +M(192)-M(193)+M(194)-M(195)-M(228))) * den(224)
  T4sum(1:70,158) = T4sum(1:70,158) + Gcoeff * G4tensor(:,47)
  Gcoeff = (c(6)*(M(153)-M(160)+M(168)-M(171)+M(174)-M(184)+M(186)-M(187)+M(188)-M(189)+M(192)-M(195)+M(198)-M(207)+M(221) &
    -M(231))) * den(1298)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,149)
  Gcoeff = (c(5)*(M(52)-M(79)+M(86)-M(91)+M(104)+M(110)-M(114)-M(116)-M(120)-M(122)+M(123)+M(124))+c(6)*(M(153)+M(186)-M(187) &
    +M(188)-M(189)-M(207)+M(221)-M(231))) * den(390)
  T4sum(1:70,189) = T4sum(1:70,189) + Gcoeff * G4tensor(:,17)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)+M(50)+M(51)-M(52) &
    -M(55)+M(62)-M(67)+M(74)+M(79)-M(86)-M(89)-M(90)+M(91)-M(102)-M(104)+M(108)-M(110)+M(114)+M(116)+M(119)+M(120)-M(121)+M(122) &
    -M(123)-M(124))+c(6)*(-M(186)+M(189)+M(207)-M(221))) * den(37)
  T5sum(1:126,79) = T5sum(1:126,79) + Gcoeff * G5tensor(:,15)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(50)+M(51)-M(52) &
    +M(55)-M(62)-M(67)+M(74)+M(79)-M(86)+M(89)-M(90)+M(91)+M(102)+M(104)-M(108)-M(110)-M(114)+M(116)-M(119)-M(120)+M(121)+M(122) &
    +M(123)-M(124))+c(6)*(-M(183)+M(197)+M(210)-M(213))) * den(37)
  T5sum(1:126,80) = T5sum(1:126,80) + Gcoeff * G5tensor(:,32)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)+M(50)+M(51)+M(52) &
    -M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91)-M(102)+M(104)+M(108)+M(110)-M(114)-M(116)+M(119)-M(120)-M(121)-M(122) &
    +M(123)+M(124))+c(6)*(M(153)-M(187)+M(188)-M(231))) * den(37)
  T5sum(1:126,81) = T5sum(1:126,81) + Gcoeff * G5tensor(:,8)
  Gcoeff = (c(4)*(M(5)-M(6)-M(11)+M(12)-M(17)+M(18)-M(19)+M(20)+M(21)-M(22)+M(23)-M(24)-M(25)+M(26)+M(29)-M(30)+M(50)+M(51)+M(52) &
    -M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91)-M(102)-M(104)+M(108)+M(110)+M(114)-M(116)+M(119)+M(120)-M(121)-M(122) &
    -M(123)+M(124))+c(6)*(M(150)-M(193)+M(194)-M(228))) * den(37)
  T5sum(1:126,82) = T5sum(1:126,82) + Gcoeff * G5tensor(:,19)
  Gcoeff = (c(6)*(M(139)-M(140)-M(173)+M(175)+M(177)-M(178)-M(181)+M(182)-M(205)+M(206)+M(215)-M(216)+M(233)-M(235)-M(237) &
    +M(238))) * den(1303)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,150)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)-M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94))+c(6)*(-M(175)-M(177)+M(181)-M(182) &
    +M(205)-M(206)+M(235)+M(237))) * den(245)
  T4sum(1:70,148) = T4sum(1:70,148) + Gcoeff * G4tensor(:,128)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)-M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94))+c(6)*(M(139)-M(140)-M(173)-M(178) &
    +M(215)-M(216)+M(233)+M(238))) * den(245)
  T4sum(1:70,149) = T4sum(1:70,149) + Gcoeff * G4tensor(:,48)
  Gcoeff = (c(6)*(-M(138)+M(139)-M(140)+M(141)+M(177)-M(178)+M(180)-M(181)+M(182)-M(183)-M(213)+M(214)+M(223)-M(224)-M(237) &
    +M(238))) * den(1305)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,151)
  Gcoeff = (c(6)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(1307)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,152)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)-M(92)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120))+c(6)*(-M(180)+M(183)+M(189) &
    -M(199)-M(204)+M(207)+M(213)-M(223))) * den(227)
  T4sum(1:70,157) = T4sum(1:70,157) + Gcoeff * G4tensor(:,129)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)-M(92)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120))+c(6)*(-M(136)+M(150)+M(174) &
    -M(191)+M(192)+M(194)-M(196)-M(234))) * den(227)
  T4sum(1:70,158) = T4sum(1:70,158) + Gcoeff * G4tensor(:,49)
  Gcoeff = (c(6)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(1309)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,153)
  Gcoeff = (c(5)*(M(64)-M(82)+M(87)-M(94)+M(106)-M(114)+M(117)-M(120)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(177)+M(180)-M(181) &
    +M(182)-M(183)-M(213)+M(223)-M(237))) * den(328)
  T4sum(1:70,192) = T4sum(1:70,192) + Gcoeff * G4tensor(:,5)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(27)-M(28)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(43)+M(50)+M(62) &
    +M(63)-M(64)-M(70)+M(75)+M(82)-M(87)-M(92)-M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(180)+M(183)+M(213)-M(223))) * den(16)
  T5sum(1:126,43) = T5sum(1:126,43) + Gcoeff * G5tensor(:,3)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(27)-M(28)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    +M(63)-M(64)-M(70)+M(75)+M(82)-M(87)+M(92)-M(93)+M(94)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(189)+M(199)+M(204)-M(207))) * den(16)
  T5sum(1:126,44) = T5sum(1:126,44) + Gcoeff * G5tensor(:,30)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(27)-M(28)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(43)+M(50)+M(62) &
    +M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94)-M(105)+M(106)+M(108)-M(111)-M(114)+M(117)+M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(177)-M(181)+M(182)-M(237))) * den(16)
  T5sum(1:126,45) = T5sum(1:126,45) + Gcoeff * G5tensor(:,4)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)-M(43)+M(50)+M(62) &
    +M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(174)-M(191)+M(192)-M(234))) * den(16)
  T5sum(1:126,46) = T5sum(1:126,46) + Gcoeff * G5tensor(:,22)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(1324)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,154)
  Gcoeff = (c(6)*(-M(147)+M(148)+M(153)-M(154)+M(161)-M(162)-M(164)+M(166)-M(185)+M(186)+M(188)-M(190)+M(221)-M(222)-M(245) &
    +M(246))) * den(1325)
  T3sum(1:35,85) = T3sum(1:35,85) + Gcoeff * G3tensor(:,155)
  Gcoeff = (c(6)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(1326)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,156)
  Gcoeff = (c(6)*(M(137)-M(138)-M(140)+M(142)-M(171)+M(172)+M(177)-M(178)-M(179)+M(180)+M(182)-M(184)+M(223)-M(224)-M(247) &
    +M(248))) * den(1327)
  T3sum(1:35,78) = T3sum(1:35,78) + Gcoeff * G3tensor(:,157)
  Gcoeff = (c(5)*(M(49)-M(52)+M(61)-M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100))+c(6)*(M(137)-M(138)+M(161)-M(162) &
    -M(222)-M(224)+M(246)+M(248))) * den(211)
  T4sum(1:70,186) = T4sum(1:70,186) + Gcoeff * G4tensor(:,18)
  Gcoeff = (c(6)*(-M(147)+M(153)+M(158)-M(160)-M(171)+M(177)+M(182)-M(184)-M(185)+M(186)+M(188)-M(190)-M(195)+M(201)+M(221) &
    -M(245))) * den(1330)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,158)
  Gcoeff = (c(6)*(M(134)-M(136)-M(147)+M(153)-M(171)+M(177)-M(179)+M(180)+M(182)-M(184)+M(188)-M(190)-M(196)+M(202)+M(223) &
    -M(247))) * den(1331)
  T3sum(1:35,58) = T3sum(1:35,58) + Gcoeff * G3tensor(:,159)
  Gcoeff = (c(5)*(-M(49)+M(52)+M(86)-M(98)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124))+c(6)*(-M(147)+M(153)-M(185) &
    +M(186)+M(188)-M(190)+M(221)-M(245))) * den(187)
  T4sum(1:70,189) = T4sum(1:70,189) + Gcoeff * G4tensor(:,19)
  Gcoeff = (c(5)*(-M(61)+M(64)+M(87)-M(99)-M(104)+M(106)+M(117)-M(123)+M(125)-M(126)-M(128)+M(130))+c(6)*(-M(171)+M(177)-M(179) &
    +M(180)+M(182)-M(184)+M(223)-M(247))) * den(163)
  T4sum(1:70,192) = T4sum(1:70,192) + Gcoeff * G4tensor(:,6)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)-M(61) &
    +M(64)-M(73)+M(76)+M(86)+M(87)+M(88)-M(98)-M(99)-M(100)-M(104)+M(106)+M(110)-M(112)+M(117)-M(118)-M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(179)+M(180)+M(223)-M(247))) * den(26)
  T5sum(1:126,11) = T5sum(1:126,11) + Gcoeff * G5tensor(:,5)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)-M(49)+M(52)-M(61) &
    +M(64)-M(73)+M(76)+M(86)+M(87)+M(88)-M(98)-M(99)-M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(185)+M(186)+M(221)-M(245))) * den(26)
  T5sum(1:126,12) = T5sum(1:126,12) + Gcoeff * G5tensor(:,16)
  Gcoeff = (c(4)*(M(1)+M(4)+M(6)+M(8)+M(9)+M(11)+M(13)+M(16)+M(18)+M(19)+M(22)+M(23)+M(25)+M(27)+M(30)+M(32)+M(34)+M(35)+M(37) &
    +M(40)-M(42)-M(43)-M(52)-M(53)-M(64)-M(65)-M(66)-M(67)-M(69)-M(70)-M(76)-M(78)-M(80)-M(81)-M(86)-M(87)-M(88)-M(90)-M(92)-M(93) &
    -M(103)-M(105)-M(106)-M(109)-M(110)-M(111)-M(117)-M(124)-M(125)-M(130))+c(6)*(-M(180)-M(223)))
  T6sum(1:210,127) = T6sum(1:210,127) + Gcoeff * G6tensor(:,3)
  Gcoeff = (c(4)*(M(2)+M(3)+M(5)+M(7)+M(10)+M(12)+M(14)+M(15)+M(17)+M(20)+M(21)+M(24)+M(26)+M(28)+M(29)+M(31)+M(33)+M(36)+M(38) &
    +M(39)-M(41)-M(43)-M(51)-M(54)-M(63)-M(66)-M(68)-M(69)-M(74)-M(75)-M(76)-M(77)-M(78)-M(79)-M(81)-M(82)-M(88)-M(91)-M(92)-M(94) &
    -M(101)-M(105)-M(106)-M(111)-M(115)-M(116)-M(117)-M(122)-M(127)-M(129))+c(6)*(-M(199)-M(204)))
  T6sum(1:210,130) = T6sum(1:210,130) + Gcoeff * G6tensor(:,11)
  Gcoeff = (c(4)*(M(2)+M(3)+M(5)+M(7)+M(9)+M(11)+M(14)+M(16)+M(18)+M(20)+M(22)+M(23)+M(25)+M(27)+M(30)+M(32)+M(34)+M(35)+M(38) &
    +M(39)-M(41)-M(52)-M(54)-M(55)-M(64)-M(66)-M(67)-M(68)-M(69)-M(70)-M(76)-M(77)-M(78)-M(81)-M(86)-M(87)-M(88)-M(89)-M(90)-M(93) &
    -M(101)-M(102)-M(104)-M(110)-M(115)-M(121)-M(123)-M(124)-M(125)-M(130))+c(6)*(-M(186)-M(221)))
  T6sum(1:210,133) = T6sum(1:210,133) + Gcoeff * G6tensor(:,7)
  Gcoeff = (c(4)*(M(1)+M(4)+M(6)+M(8)+M(10)+M(12)+M(13)+M(15)+M(17)+M(19)+M(21)+M(24)+M(26)+M(28)+M(29)+M(31)+M(33)+M(36)+M(37) &
    +M(40)-M(42)-M(51)-M(53)-M(55)-M(63)-M(65)-M(66)-M(69)-M(74)-M(75)-M(76)-M(78)-M(79)-M(80)-M(81)-M(82)-M(88)-M(89)-M(91)-M(94) &
    -M(102)-M(103)-M(104)-M(109)-M(116)-M(121)-M(122)-M(123)-M(127)-M(129))+c(6)*(-M(197)-M(210)))
  T6sum(1:210,136) = T6sum(1:210,136) + Gcoeff * G6tensor(:,12)
  Gcoeff = (c(4)*(M(1)+M(4)+M(5)+M(8)+M(10)+M(11)+M(13)+M(15)+M(18)+M(20)+M(22)+M(23)+M(25)+M(28)+M(30)+M(31)+M(33)+M(36)+M(37) &
    +M(40)-M(42)-M(50)-M(52)-M(53)-M(62)-M(63)-M(65)-M(66)-M(67)-M(69)-M(75)-M(76)-M(78)-M(80)-M(81)-M(82)-M(86)-M(88)-M(90)-M(94) &
    -M(103)-M(108)-M(109)-M(110)-M(114)-M(119)-M(120)-M(124)-M(127)-M(129))+c(6)*(-M(183)-M(213)))
  T6sum(1:210,139) = T6sum(1:210,139) + Gcoeff * G6tensor(:,1)
  Gcoeff = (c(4)*(M(2)+M(3)+M(6)+M(7)+M(9)+M(12)+M(14)+M(16)+M(17)+M(19)+M(21)+M(24)+M(26)+M(27)+M(29)+M(32)+M(34)+M(35)+M(38) &
    +M(39)-M(41)-M(50)-M(51)-M(54)-M(62)-M(64)-M(66)-M(68)-M(69)-M(70)-M(74)-M(76)-M(77)-M(78)-M(79)-M(81)-M(87)-M(88)-M(91)-M(93) &
    -M(101)-M(108)-M(114)-M(115)-M(116)-M(119)-M(120)-M(122)-M(125)-M(130))+c(6)*(-M(189)-M(207)))
  T6sum(1:210,142) = T6sum(1:210,142) + Gcoeff * G6tensor(:,6)
  Gcoeff = (c(4)*(M(2)+M(4)+M(6)+M(7)+M(9)+M(11)+M(13)+M(16)+M(17)+M(19)+M(22)+M(24)+M(25)+M(27)+M(30)+M(31)+M(34)+M(36)+M(38) &
    +M(39)-M(42)-M(43)-M(49)-M(53)-M(61)-M(65)-M(66)-M(67)-M(69)-M(70)-M(73)-M(78)-M(80)-M(81)-M(90)-M(92)-M(93)-M(98)-M(99) &
    -M(100)-M(103)-M(104)-M(105)-M(109)-M(111)-M(112)-M(118)-M(123)-M(126)-M(128))+c(6)*(-M(179)-M(247)))
  T6sum(1:210,145) = T6sum(1:210,145) + Gcoeff * G6tensor(:,4)
  Gcoeff = (c(4)*(M(1)+M(3)+M(5)+M(8)+M(9)+M(11)+M(14)+M(16)+M(17)+M(20)+M(22)+M(24)+M(25)+M(27)+M(30)+M(31)+M(34)+M(36)+M(37) &
    +M(40)-M(41)-M(49)-M(54)-M(55)-M(61)-M(66)-M(67)-M(68)-M(69)-M(70)-M(73)-M(77)-M(78)-M(81)-M(89)-M(90)-M(93)-M(98)-M(99) &
    -M(100)-M(101)-M(102)-M(106)-M(112)-M(115)-M(117)-M(118)-M(121)-M(126)-M(128))+c(6)*(-M(185)-M(245)))
  T6sum(1:210,148) = T6sum(1:210,148) + Gcoeff * G6tensor(:,8)
  Gcoeff = (c(4)*(M(2)+M(4)+M(6)+M(7)+M(10)+M(12)+M(13)+M(15)+M(18)+M(19)+M(21)+M(23)+M(26)+M(28)+M(29)+M(32)+M(33)+M(35)+M(38) &
    +M(39)-M(43)-M(48)-M(53)-M(54)-M(60)-M(65)-M(67)-M(70)-M(71)-M(72)-M(73)-M(77)-M(79)-M(82)-M(90)-M(91)-M(92)-M(93)-M(94) &
    -M(100)-M(101)-M(105)-M(106)-M(111)-M(115)-M(116)-M(117)-M(122)-M(127)-M(129))+c(6)*(-M(191)-M(234)))
  T6sum(1:210,151) = T6sum(1:210,151) + Gcoeff * G6tensor(:,10)
  Gcoeff = (c(4)*(M(1)+M(3)+M(5)+M(8)+M(10)+M(12)+M(14)+M(15)+M(18)+M(20)+M(21)+M(23)+M(26)+M(28)+M(29)+M(32)+M(33)+M(35)+M(37) &
    +M(40)-M(41)-M(42)-M(48)-M(55)-M(60)-M(67)-M(68)-M(70)-M(71)-M(72)-M(73)-M(79)-M(80)-M(82)-M(89)-M(90)-M(91)-M(93)-M(94) &
    -M(100)-M(102)-M(103)-M(104)-M(109)-M(116)-M(121)-M(122)-M(123)-M(127)-M(129))+c(6)*(-M(193)-M(228)))
  T6sum(1:210,154) = T6sum(1:210,154) + Gcoeff * G6tensor(:,9)
  Gcoeff = (c(4)*(M(2)+M(3)+M(6)+M(7)+M(9)+M(12)+M(14)+M(16)+M(17)+M(19)+M(21)+M(24)+M(26)+M(27)+M(29)+M(32)+M(34)+M(35)+M(38) &
    +M(39)-M(43)-M(47)-M(49)-M(53)-M(59)-M(60)-M(65)-M(66)-M(67)-M(70)-M(72)-M(73)-M(78)-M(82)-M(90)-M(92)-M(93)-M(94)-M(98) &
    -M(100)-M(105)-M(107)-M(111)-M(112)-M(113)-M(114)-M(118)-M(120)-M(127)-M(129))+c(6)*(-M(181)-M(237)))
  T6sum(1:210,157) = T6sum(1:210,157) + Gcoeff * G6tensor(:,2)
  Gcoeff = (c(4)*(M(1)+M(4)+M(5)+M(8)+M(10)+M(11)+M(13)+M(15)+M(18)+M(20)+M(22)+M(23)+M(25)+M(28)+M(30)+M(31)+M(33)+M(36)+M(37) &
    +M(40)-M(41)-M(47)-M(48)-M(55)-M(59)-M(61)-M(67)-M(68)-M(69)-M(70)-M(71)-M(73)-M(79)-M(81)-M(89)-M(90)-M(91)-M(93)-M(99) &
    -M(100)-M(102)-M(107)-M(113)-M(114)-M(116)-M(120)-M(121)-M(122)-M(126)-M(128))+c(6)*(-M(187)-M(231)))
  T6sum(1:210,160) = T6sum(1:210,160) + Gcoeff * G6tensor(:,5)

end subroutine vamp_26

end module ol_vamp_26_ppjjjj_gggggg_1_/**/REALKIND
