
module ol_vamp_16_ppjjjj_gggggg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_16(M)
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
  complex(REALKIND), dimension(4,1,4,94) :: G0
  complex(REALKIND), dimension(4,5,4,175) :: G1
  complex(REALKIND), dimension(4,15,4,95) :: G2
  complex(REALKIND), dimension(4,35,4,17) :: G3
  complex(REALKIND), dimension(4,70,4,2) :: G4
  complex(REALKIND), dimension(5,99) :: G1tensor
  complex(REALKIND), dimension(15,186) :: G2tensor
  complex(REALKIND), dimension(35,90) :: G3tensor
  complex(REALKIND), dimension(70,15) :: G4tensor
  complex(REALKIND), dimension(126,2) :: G5tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,109),Q(:,17),G1(:,:,:,1))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,-3),G1(:,:,:,2))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,2),wf(:,-2),wf(:,-1),G1tensor(:,1))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,2),wf(:,-1),wf(:,-2),G1tensor(:,2))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,2),wf(:,-2),wf(:,-1),G1tensor(:,3))
  call check_last_UV_W(l_switch,G1(:,:,:,2),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,1))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,-5),G1(:,:,:,3))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,3),wf(:,-2),wf(:,-1),G1tensor(:,4))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,3),wf(:,-1),wf(:,-2),G1tensor(:,5))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,3),wf(:,-2),wf(:,-1),G1tensor(:,6))
  call check_last_UV_W(l_switch,G1(:,:,:,3),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,2))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,-3),G1(:,:,:,4))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,4),wf(:,-2),wf(:,-1),G1tensor(:,7))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,4),wf(:,-1),wf(:,-2),G1tensor(:,8))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,4),wf(:,-2),wf(:,-1),G1tensor(:,9))
  call check_last_UV_W(l_switch,G1(:,:,:,4),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,3))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,-2),G1(:,:,:,5))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,5),wf(:,-3),wf(:,-1),G1tensor(:,10))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,5),wf(:,-1),wf(:,-3),G1tensor(:,11))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,5),wf(:,-3),wf(:,-1),G1tensor(:,12))
  call check_last_UV_W(l_switch,G1(:,:,:,5),Q(:,53),wf(:,91),Q(:,10),G2tensor(:,4))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,-5),G1(:,:,:,6))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,6),wf(:,-3),wf(:,-1),G1tensor(:,13))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,6),wf(:,-1),wf(:,-3),G1tensor(:,14))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,6),wf(:,-3),wf(:,-1),G1tensor(:,15))
  call check_last_UV_W(l_switch,G1(:,:,:,6),Q(:,53),wf(:,91),Q(:,10),G2tensor(:,5))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,-2),G1(:,:,:,7))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,7),wf(:,-3),wf(:,-1),G1tensor(:,16))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,7),wf(:,-1),wf(:,-3),G1tensor(:,17))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,7),wf(:,-3),wf(:,-1),G1tensor(:,18))
  call check_last_UV_W(l_switch,G1(:,:,:,7),Q(:,53),wf(:,91),Q(:,10),G2tensor(:,6))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,-1),G1(:,:,:,8))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,8),wf(:,-3),wf(:,-2),G1tensor(:,19))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,8),wf(:,-2),wf(:,-3),G1tensor(:,20))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,8),wf(:,-3),wf(:,-2),G1tensor(:,21))
  call check_last_UV_W(l_switch,G1(:,:,:,8),Q(:,51),wf(:,62),Q(:,12),G2tensor(:,7))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,-5),G1(:,:,:,9))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,9),wf(:,-3),wf(:,-2),G1tensor(:,22))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,9),wf(:,-2),wf(:,-3),G1tensor(:,23))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,9),wf(:,-3),wf(:,-2),G1tensor(:,24))
  call check_last_UV_W(l_switch,G1(:,:,:,9),Q(:,51),wf(:,62),Q(:,12),G2tensor(:,8))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,-1),G1(:,:,:,10))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,10),wf(:,-3),wf(:,-2),G1tensor(:,25))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,10),wf(:,-2),wf(:,-3),G1tensor(:,26))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,10),wf(:,-3),wf(:,-2),G1tensor(:,27))
  call check_last_UV_W(l_switch,G1(:,:,:,10),Q(:,51),wf(:,62),Q(:,12),G2tensor(:,9))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,105),G1(:,:,:,11))
  call check_last_UV_W(l_switch,G1(:,:,:,11),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,10))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,105),wf(:,-5),G1(:,:,:,12))
  call check_last_UV_W(l_switch,G1(:,:,:,12),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,11))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,105),G1(:,:,:,13))
  call check_last_UV_W(l_switch,G1(:,:,:,13),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,12))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,-5),Q(:,32),G2(:,:,:,1))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,1),wf(:,-3),wf(:,105),G2tensor(:,13))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,1),wf(:,105),wf(:,-3),G2tensor(:,14))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,1),wf(:,-3),wf(:,105),G2tensor(:,15))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,1),wf(:,-2),wf(:,91),G2tensor(:,16))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,1),wf(:,91),wf(:,-2),G2tensor(:,17))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,1),wf(:,-2),wf(:,91),G2tensor(:,18))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,1),wf(:,-1),wf(:,62),G2tensor(:,19))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,1),wf(:,62),wf(:,-1),G2tensor(:,20))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,1),wf(:,-1),wf(:,62),G2tensor(:,21))
  call check_last_UV_W(l_switch,G2(:,:,:,1),Q(:,49),wf(:,56),Q(:,14),G3tensor(:,1))
  call check_last_UV_W(l_switch,G2(:,:,:,1),Q(:,49),wf(:,59),Q(:,14),G3tensor(:,2))
  call check_last_UV_W(l_switch,G2(:,:,:,1),Q(:,49),wf(:,60),Q(:,14),G3tensor(:,3))
  call check_last_UV_W(l_switch,G2(:,:,:,1),Q(:,49),wf(:,202),Q(:,14),G3tensor(:,4))
  call check_last_UV_W(l_switch,G2(:,:,:,1),Q(:,49),wf(:,214),Q(:,14),G3tensor(:,5))
  call check_last_UV_W(l_switch,G2(:,:,:,1),Q(:,49),wf(:,221),Q(:,14),G3tensor(:,6))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,91),G1(:,:,:,14))
  call check_last_UV_W(l_switch,G1(:,:,:,14),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,22))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,91),wf(:,-5),G1(:,:,:,15))
  call check_last_UV_W(l_switch,G1(:,:,:,15),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,23))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,91),G1(:,:,:,16))
  call check_last_UV_W(l_switch,G1(:,:,:,16),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,24))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,62),G1(:,:,:,17))
  call check_last_UV_W(l_switch,G1(:,:,:,17),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,25))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,62),wf(:,-5),G1(:,:,:,18))
  call check_last_UV_W(l_switch,G1(:,:,:,18),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,26))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,62),G1(:,:,:,19))
  call check_last_UV_W(l_switch,G1(:,:,:,19),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,27))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,99),G1(:,:,:,20))
  call check_last_UV_W(l_switch,G1(:,:,:,20),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,28))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,99),wf(:,-3),G1(:,:,:,21))
  call check_last_UV_W(l_switch,G1(:,:,:,21),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,29))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,99),G1(:,:,:,22))
  call check_last_UV_W(l_switch,G1(:,:,:,22),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,30))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,70),G1(:,:,:,23))
  call check_last_UV_W(l_switch,G1(:,:,:,23),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,31))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,70),wf(:,-3),G1(:,:,:,24))
  call check_last_UV_W(l_switch,G1(:,:,:,24),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,32))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,70),G1(:,:,:,25))
  call check_last_UV_W(l_switch,G1(:,:,:,25),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,33))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,-2),Q(:,4),G2(:,:,:,2))
  call loop_GGG_G_12(G2(:,:,:,2),wf(:,-5),wf(:,-3),G2(:,:,:,3))
  call check_last_UV_W(l_switch,G2(:,:,:,3),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,7))
  call loop_GGG_G_12(G2(:,:,:,2),wf(:,-3),wf(:,-5),G2(:,:,:,4))
  call check_last_UV_W(l_switch,G2(:,:,:,4),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,8))
  call loop_GGG_G_23(G2(:,:,:,2),wf(:,-5),wf(:,-3),G2(:,:,:,5))
  call check_last_UV_W(l_switch,G2(:,:,:,5),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,9))
  call loop_UV_W(G2(:,:,:,2),Q(:,21),wf(:,79),Q(:,40),G3(:,:,:,1))
  call check_last_UV_W(l_switch,G3(:,:,:,1),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,1))
  call loop_UV_W(G2(:,:,:,2),Q(:,21),wf(:,-3),Q(:,8),G3(:,:,:,2))
  call loop_UV_W(G3(:,:,:,2),Q(:,29),wf(:,-5),Q(:,32),G4(:,:,:,1))
  call check_last_UV_W(l_switch,G4(:,:,:,1),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,1))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,99),G1(:,:,:,26))
  call check_last_UV_W(l_switch,G1(:,:,:,26),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,34))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,99),wf(:,-2),G1(:,:,:,27))
  call check_last_UV_W(l_switch,G1(:,:,:,27),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,35))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,99),G1(:,:,:,28))
  call check_last_UV_W(l_switch,G1(:,:,:,28),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,36))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,79),G1(:,:,:,29))
  call check_last_UV_W(l_switch,G1(:,:,:,29),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,37))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,79),wf(:,-2),G1(:,:,:,30))
  call check_last_UV_W(l_switch,G1(:,:,:,30),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,38))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,79),G1(:,:,:,31))
  call check_last_UV_W(l_switch,G1(:,:,:,31),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,39))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,-3),Q(:,8),G2(:,:,:,6))
  call loop_GGG_G_12(G2(:,:,:,6),wf(:,-5),wf(:,-2),G2(:,:,:,7))
  call check_last_UV_W(l_switch,G2(:,:,:,7),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,10))
  call loop_GGG_G_12(G2(:,:,:,6),wf(:,-2),wf(:,-5),G2(:,:,:,8))
  call check_last_UV_W(l_switch,G2(:,:,:,8),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,11))
  call loop_GGG_G_23(G2(:,:,:,6),wf(:,-5),wf(:,-2),G2(:,:,:,9))
  call check_last_UV_W(l_switch,G2(:,:,:,9),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,12))
  call loop_GGG_G_12(G2(:,:,:,6),wf(:,-5),wf(:,-1),G2(:,:,:,10))
  call check_last_UV_W(l_switch,G2(:,:,:,10),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,13))
  call loop_GGG_G_12(G2(:,:,:,6),wf(:,-1),wf(:,-5),G2(:,:,:,11))
  call check_last_UV_W(l_switch,G2(:,:,:,11),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,14))
  call loop_GGG_G_23(G2(:,:,:,6),wf(:,-5),wf(:,-1),G2(:,:,:,12))
  call check_last_UV_W(l_switch,G2(:,:,:,12),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,15))
  call loop_UV_W(G2(:,:,:,6),Q(:,25),wf(:,-5),Q(:,32),G3(:,:,:,3))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,3),wf(:,-2),wf(:,-1),G3tensor(:,16))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,3),wf(:,-1),wf(:,-2),G3tensor(:,17))
  call check_last_GGG_G_23(l_switch,G3(:,:,:,3),wf(:,-2),wf(:,-1),G3tensor(:,18))
  call check_last_UV_W(l_switch,G3(:,:,:,3),Q(:,57),wf(:,105),Q(:,6),G4tensor(:,2))
  call loop_UV_W(G2(:,:,:,6),Q(:,25),wf(:,99),Q(:,34),G3(:,:,:,4))
  call check_last_UV_W(l_switch,G3(:,:,:,4),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,3))
  call loop_UV_W(G2(:,:,:,6),Q(:,25),wf(:,70),Q(:,36),G3(:,:,:,5))
  call check_last_UV_W(l_switch,G3(:,:,:,5),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,4))
  call loop_UV_W(G2(:,:,:,6),Q(:,25),wf(:,-2),Q(:,4),G3(:,:,:,6))
  call loop_UV_W(G3(:,:,:,6),Q(:,29),wf(:,-5),Q(:,32),G4(:,:,:,2))
  call check_last_UV_W(l_switch,G4(:,:,:,2),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,2))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,99),Q(:,34),G2(:,:,:,13))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,13),wf(:,-3),wf(:,-2),G2tensor(:,40))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,13),wf(:,-2),wf(:,-3),G2tensor(:,41))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,13),wf(:,-3),wf(:,-2),G2tensor(:,42))
  call check_last_UV_W(l_switch,G2(:,:,:,13),Q(:,51),wf(:,62),Q(:,12),G3tensor(:,19))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,-2),G1(:,:,:,32))
  call loop_UV_W(G1(:,:,:,32),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,14))
  call check_last_UV_W(l_switch,G2(:,:,:,14),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,20))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,-3),G1(:,:,:,33))
  call loop_UV_W(G1(:,:,:,33),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,15))
  call check_last_UV_W(l_switch,G2(:,:,:,15),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,21))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,-2),G1(:,:,:,34))
  call loop_UV_W(G1(:,:,:,34),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,16))
  call check_last_UV_W(l_switch,G2(:,:,:,16),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,22))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,14),Q(:,44),G2(:,:,:,17))
  call check_last_UV_W(l_switch,G2(:,:,:,17),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,23))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,17),Q(:,44),G2(:,:,:,18))
  call check_last_UV_W(l_switch,G2(:,:,:,18),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,24))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,18),Q(:,44),G2(:,:,:,19))
  call check_last_UV_W(l_switch,G2(:,:,:,19),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,25))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,70),G1(:,:,:,35))
  call check_last_UV_W(l_switch,G1(:,:,:,35),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,43))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,70),wf(:,-1),G1(:,:,:,36))
  call check_last_UV_W(l_switch,G1(:,:,:,36),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,44))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,70),G1(:,:,:,37))
  call check_last_UV_W(l_switch,G1(:,:,:,37),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,45))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,79),G1(:,:,:,38))
  call check_last_UV_W(l_switch,G1(:,:,:,38),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,46))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,79),wf(:,-1),G1(:,:,:,39))
  call check_last_UV_W(l_switch,G1(:,:,:,39),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,47))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,79),G1(:,:,:,40))
  call check_last_UV_W(l_switch,G1(:,:,:,40),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,48))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,70),Q(:,36),G2(:,:,:,20))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,20),wf(:,-3),wf(:,-1),G2tensor(:,49))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,20),wf(:,-1),wf(:,-3),G2tensor(:,50))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,20),wf(:,-3),wf(:,-1),G2tensor(:,51))
  call check_last_UV_W(l_switch,G2(:,:,:,20),Q(:,53),wf(:,91),Q(:,10),G3tensor(:,26))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,-1),G1(:,:,:,41))
  call loop_UV_W(G1(:,:,:,41),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,21))
  call check_last_UV_W(l_switch,G2(:,:,:,21),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,27))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,-3),G1(:,:,:,42))
  call loop_UV_W(G1(:,:,:,42),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,22))
  call check_last_UV_W(l_switch,G2(:,:,:,22),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,28))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,-1),G1(:,:,:,43))
  call loop_UV_W(G1(:,:,:,43),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,23))
  call check_last_UV_W(l_switch,G2(:,:,:,23),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,29))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,32),Q(:,42),G2(:,:,:,24))
  call check_last_UV_W(l_switch,G2(:,:,:,24),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,30))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,35),Q(:,42),G2(:,:,:,25))
  call check_last_UV_W(l_switch,G2(:,:,:,25),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,31))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,36),Q(:,42),G2(:,:,:,26))
  call check_last_UV_W(l_switch,G2(:,:,:,26),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,32))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,79),Q(:,40),G2(:,:,:,27))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,27),wf(:,-2),wf(:,-1),G2tensor(:,52))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,27),wf(:,-1),wf(:,-2),G2tensor(:,53))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,27),wf(:,-2),wf(:,-1),G2tensor(:,54))
  call check_last_UV_W(l_switch,G2(:,:,:,27),Q(:,57),wf(:,105),Q(:,6),G3tensor(:,33))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,44),Q(:,38),G2(:,:,:,28))
  call check_last_UV_W(l_switch,G2(:,:,:,28),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,34))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,47),Q(:,38),G2(:,:,:,29))
  call check_last_UV_W(l_switch,G2(:,:,:,29),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,35))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,48),Q(:,38),G2(:,:,:,30))
  call check_last_UV_W(l_switch,G2(:,:,:,30),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,36))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,207),Q(:,38),G2(:,:,:,31))
  call check_last_UV_W(l_switch,G2(:,:,:,31),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,37))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,236),Q(:,42),G2(:,:,:,32))
  call check_last_UV_W(l_switch,G2(:,:,:,32),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,38))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,91),Q(:,10),G2(:,:,:,33))
  call loop_UV_W(G2(:,:,:,33),Q(:,27),wf(:,-5),Q(:,32),G3(:,:,:,7))
  call check_last_UV_W(l_switch,G3(:,:,:,7),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,5))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,254),Q(:,44),G2(:,:,:,34))
  call check_last_UV_W(l_switch,G2(:,:,:,34),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,39))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,62),Q(:,12),G2(:,:,:,35))
  call loop_UV_W(G2(:,:,:,35),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,8))
  call check_last_UV_W(l_switch,G3(:,:,:,8),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,6))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,230),Q(:,38),G2(:,:,:,36))
  call check_last_UV_W(l_switch,G2(:,:,:,36),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,40))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,245),Q(:,42),G2(:,:,:,37))
  call check_last_UV_W(l_switch,G2(:,:,:,37),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,41))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,231),Q(:,38),G2(:,:,:,38))
  call check_last_UV_W(l_switch,G2(:,:,:,38),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,42))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,263),Q(:,44),G2(:,:,:,39))
  call check_last_UV_W(l_switch,G2(:,:,:,39),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,43))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,246),Q(:,42),G2(:,:,:,40))
  call check_last_UV_W(l_switch,G2(:,:,:,40),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,44))
  call loop_UV_W(G1(:,:,:,1),Q(:,17),wf(:,264),Q(:,44),G2(:,:,:,41))
  call check_last_UV_W(l_switch,G2(:,:,:,41),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,45))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,50),G0(:,:,:,2))
  call loop_UV_W(G0(:,:,:,2),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,44))
  call check_last_UV_W(l_switch,G1(:,:,:,44),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,55))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,53),G0(:,:,:,3))
  call loop_UV_W(G0(:,:,:,3),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,45))
  call check_last_UV_W(l_switch,G1(:,:,:,45),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,56))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,54),G0(:,:,:,4))
  call loop_UV_W(G0(:,:,:,4),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,46))
  call check_last_UV_W(l_switch,G1(:,:,:,46),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,57))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,50),wf(:,-3),G0(:,:,:,5))
  call loop_UV_W(G0(:,:,:,5),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,47))
  call check_last_UV_W(l_switch,G1(:,:,:,47),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,58))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,53),wf(:,-3),G0(:,:,:,6))
  call loop_UV_W(G0(:,:,:,6),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,48))
  call check_last_UV_W(l_switch,G1(:,:,:,48),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,59))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,54),wf(:,-3),G0(:,:,:,7))
  call loop_UV_W(G0(:,:,:,7),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,49))
  call check_last_UV_W(l_switch,G1(:,:,:,49),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,60))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,50),G0(:,:,:,8))
  call loop_UV_W(G0(:,:,:,8),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,50))
  call check_last_UV_W(l_switch,G1(:,:,:,50),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,61))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,53),G0(:,:,:,9))
  call loop_UV_W(G0(:,:,:,9),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,51))
  call check_last_UV_W(l_switch,G1(:,:,:,51),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,62))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,54),G0(:,:,:,10))
  call loop_UV_W(G0(:,:,:,10),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,52))
  call check_last_UV_W(l_switch,G1(:,:,:,52),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,63))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,50),Q(:,22),G1(:,:,:,53))
  call loop_GGG_G_12(G1(:,:,:,53),wf(:,-5),wf(:,-3),G1(:,:,:,54))
  call check_last_UV_W(l_switch,G1(:,:,:,54),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,64))
  call loop_GGG_G_12(G1(:,:,:,53),wf(:,-3),wf(:,-5),G1(:,:,:,55))
  call check_last_UV_W(l_switch,G1(:,:,:,55),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,65))
  call loop_GGG_G_23(G1(:,:,:,53),wf(:,-5),wf(:,-3),G1(:,:,:,56))
  call check_last_UV_W(l_switch,G1(:,:,:,56),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,66))
  call loop_GGG_G_12(G1(:,:,:,53),wf(:,-5),wf(:,0),G1(:,:,:,57))
  call check_last_UV_W(l_switch,G1(:,:,:,57),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,67))
  call loop_GGG_G_12(G1(:,:,:,53),wf(:,0),wf(:,-5),G1(:,:,:,58))
  call check_last_UV_W(l_switch,G1(:,:,:,58),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,68))
  call loop_GGG_G_23(G1(:,:,:,53),wf(:,-5),wf(:,0),G1(:,:,:,59))
  call check_last_UV_W(l_switch,G1(:,:,:,59),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,69))
  call loop_UV_W(G1(:,:,:,53),Q(:,22),wf(:,-5),Q(:,32),G2(:,:,:,42))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,42),wf(:,-3),wf(:,0),G2tensor(:,70))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,42),wf(:,0),wf(:,-3),G2tensor(:,71))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,42),wf(:,-3),wf(:,0),G2tensor(:,72))
  call check_last_UV_W(l_switch,G2(:,:,:,42),Q(:,54),wf(:,104),Q(:,9),G3tensor(:,46))
  call loop_UV_W(G1(:,:,:,53),Q(:,22),wf(:,113),Q(:,33),G2(:,:,:,43))
  call check_last_UV_W(l_switch,G2(:,:,:,43),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,47))
  call loop_UV_W(G1(:,:,:,53),Q(:,22),wf(:,79),Q(:,40),G2(:,:,:,44))
  call check_last_UV_W(l_switch,G2(:,:,:,44),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,48))
  call loop_UV_W(G1(:,:,:,53),Q(:,22),wf(:,-3),Q(:,8),G2(:,:,:,45))
  call loop_UV_W(G2(:,:,:,45),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,9))
  call check_last_UV_W(l_switch,G3(:,:,:,9),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,7))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,53),Q(:,22),G1(:,:,:,60))
  call loop_GGG_G_12(G1(:,:,:,60),wf(:,-5),wf(:,-3),G1(:,:,:,61))
  call check_last_UV_W(l_switch,G1(:,:,:,61),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,73))
  call loop_GGG_G_12(G1(:,:,:,60),wf(:,-3),wf(:,-5),G1(:,:,:,62))
  call check_last_UV_W(l_switch,G1(:,:,:,62),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,74))
  call loop_GGG_G_23(G1(:,:,:,60),wf(:,-5),wf(:,-3),G1(:,:,:,63))
  call check_last_UV_W(l_switch,G1(:,:,:,63),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,75))
  call loop_GGG_G_12(G1(:,:,:,60),wf(:,-5),wf(:,0),G1(:,:,:,64))
  call check_last_UV_W(l_switch,G1(:,:,:,64),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,76))
  call loop_GGG_G_12(G1(:,:,:,60),wf(:,0),wf(:,-5),G1(:,:,:,65))
  call check_last_UV_W(l_switch,G1(:,:,:,65),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,77))
  call loop_GGG_G_23(G1(:,:,:,60),wf(:,-5),wf(:,0),G1(:,:,:,66))
  call check_last_UV_W(l_switch,G1(:,:,:,66),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,78))
  call loop_UV_W(G1(:,:,:,60),Q(:,22),wf(:,-5),Q(:,32),G2(:,:,:,46))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,46),wf(:,-3),wf(:,0),G2tensor(:,79))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,46),wf(:,0),wf(:,-3),G2tensor(:,80))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,46),wf(:,-3),wf(:,0),G2tensor(:,81))
  call check_last_UV_W(l_switch,G2(:,:,:,46),Q(:,54),wf(:,104),Q(:,9),G3tensor(:,49))
  call loop_UV_W(G1(:,:,:,60),Q(:,22),wf(:,113),Q(:,33),G2(:,:,:,47))
  call check_last_UV_W(l_switch,G2(:,:,:,47),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,50))
  call loop_UV_W(G1(:,:,:,60),Q(:,22),wf(:,79),Q(:,40),G2(:,:,:,48))
  call check_last_UV_W(l_switch,G2(:,:,:,48),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,51))
  call loop_UV_W(G1(:,:,:,60),Q(:,22),wf(:,-3),Q(:,8),G2(:,:,:,49))
  call loop_UV_W(G2(:,:,:,49),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,10))
  call check_last_UV_W(l_switch,G3(:,:,:,10),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,8))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,54),Q(:,22),G1(:,:,:,67))
  call loop_GGG_G_12(G1(:,:,:,67),wf(:,-5),wf(:,-3),G1(:,:,:,68))
  call check_last_UV_W(l_switch,G1(:,:,:,68),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,82))
  call loop_GGG_G_12(G1(:,:,:,67),wf(:,-3),wf(:,-5),G1(:,:,:,69))
  call check_last_UV_W(l_switch,G1(:,:,:,69),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,83))
  call loop_GGG_G_23(G1(:,:,:,67),wf(:,-5),wf(:,-3),G1(:,:,:,70))
  call check_last_UV_W(l_switch,G1(:,:,:,70),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,84))
  call loop_GGG_G_12(G1(:,:,:,67),wf(:,-5),wf(:,0),G1(:,:,:,71))
  call check_last_UV_W(l_switch,G1(:,:,:,71),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,85))
  call loop_GGG_G_12(G1(:,:,:,67),wf(:,0),wf(:,-5),G1(:,:,:,72))
  call check_last_UV_W(l_switch,G1(:,:,:,72),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,86))
  call loop_GGG_G_23(G1(:,:,:,67),wf(:,-5),wf(:,0),G1(:,:,:,73))
  call check_last_UV_W(l_switch,G1(:,:,:,73),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,87))
  call loop_UV_W(G1(:,:,:,67),Q(:,22),wf(:,-5),Q(:,32),G2(:,:,:,50))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,50),wf(:,-3),wf(:,0),G2tensor(:,88))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,50),wf(:,0),wf(:,-3),G2tensor(:,89))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,50),wf(:,-3),wf(:,0),G2tensor(:,90))
  call check_last_UV_W(l_switch,G2(:,:,:,50),Q(:,54),wf(:,104),Q(:,9),G3tensor(:,52))
  call loop_UV_W(G1(:,:,:,67),Q(:,22),wf(:,113),Q(:,33),G2(:,:,:,51))
  call check_last_UV_W(l_switch,G2(:,:,:,51),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,53))
  call loop_UV_W(G1(:,:,:,67),Q(:,22),wf(:,79),Q(:,40),G2(:,:,:,52))
  call check_last_UV_W(l_switch,G2(:,:,:,52),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,54))
  call loop_UV_W(G1(:,:,:,67),Q(:,22),wf(:,-3),Q(:,8),G2(:,:,:,53))
  call loop_UV_W(G2(:,:,:,53),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,11))
  call check_last_UV_W(l_switch,G3(:,:,:,11),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,9))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,75),G0(:,:,:,11))
  call loop_GGG_G_12(G0(:,:,:,11),wf(:,-5),wf(:,-1),G0(:,:,:,12))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,28))
  call loop_GGG_G_12(G0(:,:,:,11),wf(:,-1),wf(:,-5),G0(:,:,:,13))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,29))
  call loop_GGG_G_23(G0(:,:,:,11),wf(:,-5),wf(:,-1),G0(:,:,:,14))
  call check_last_UV_W(l_switch,G0(:,:,:,14),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,30))
  call loop_GGG_G_12(G0(:,:,:,11),wf(:,-5),wf(:,0),G0(:,:,:,15))
  call check_last_UV_W(l_switch,G0(:,:,:,15),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,31))
  call loop_GGG_G_12(G0(:,:,:,11),wf(:,0),wf(:,-5),G0(:,:,:,16))
  call check_last_UV_W(l_switch,G0(:,:,:,16),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,32))
  call loop_GGG_G_23(G0(:,:,:,11),wf(:,-5),wf(:,0),G0(:,:,:,17))
  call check_last_UV_W(l_switch,G0(:,:,:,17),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,33))
  call loop_UV_W(G0(:,:,:,11),Q(:,28),wf(:,-5),Q(:,32),G1(:,:,:,74))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,74),wf(:,-1),wf(:,0),G1tensor(:,34))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,74),wf(:,0),wf(:,-1),G1tensor(:,35))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,74),wf(:,-1),wf(:,0),G1tensor(:,36))
  call check_last_UV_W(l_switch,G1(:,:,:,74),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,91))
  call loop_UV_W(G0(:,:,:,11),Q(:,28),wf(:,113),Q(:,33),G1(:,:,:,75))
  call check_last_UV_W(l_switch,G1(:,:,:,75),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,92))
  call loop_UV_W(G0(:,:,:,11),Q(:,28),wf(:,99),Q(:,34),G1(:,:,:,76))
  call check_last_UV_W(l_switch,G1(:,:,:,76),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,93))
  call loop_UV_W(G0(:,:,:,11),Q(:,28),wf(:,-1),Q(:,2),G1(:,:,:,77))
  call loop_UV_W(G1(:,:,:,77),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,54))
  call check_last_UV_W(l_switch,G2(:,:,:,54),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,55))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,75),wf(:,-2),G0(:,:,:,18))
  call loop_GGG_G_12(G0(:,:,:,18),wf(:,-5),wf(:,-1),G0(:,:,:,19))
  call check_last_UV_W(l_switch,G0(:,:,:,19),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,37))
  call loop_GGG_G_12(G0(:,:,:,18),wf(:,-1),wf(:,-5),G0(:,:,:,20))
  call check_last_UV_W(l_switch,G0(:,:,:,20),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,38))
  call loop_GGG_G_23(G0(:,:,:,18),wf(:,-5),wf(:,-1),G0(:,:,:,21))
  call check_last_UV_W(l_switch,G0(:,:,:,21),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,39))
  call loop_GGG_G_12(G0(:,:,:,18),wf(:,-5),wf(:,0),G0(:,:,:,22))
  call check_last_UV_W(l_switch,G0(:,:,:,22),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,40))
  call loop_GGG_G_12(G0(:,:,:,18),wf(:,0),wf(:,-5),G0(:,:,:,23))
  call check_last_UV_W(l_switch,G0(:,:,:,23),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,41))
  call loop_GGG_G_23(G0(:,:,:,18),wf(:,-5),wf(:,0),G0(:,:,:,24))
  call check_last_UV_W(l_switch,G0(:,:,:,24),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,42))
  call loop_UV_W(G0(:,:,:,18),Q(:,28),wf(:,-5),Q(:,32),G1(:,:,:,78))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,78),wf(:,-1),wf(:,0),G1tensor(:,43))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,78),wf(:,0),wf(:,-1),G1tensor(:,44))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,78),wf(:,-1),wf(:,0),G1tensor(:,45))
  call check_last_UV_W(l_switch,G1(:,:,:,78),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,94))
  call loop_UV_W(G0(:,:,:,18),Q(:,28),wf(:,113),Q(:,33),G1(:,:,:,79))
  call check_last_UV_W(l_switch,G1(:,:,:,79),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,95))
  call loop_UV_W(G0(:,:,:,18),Q(:,28),wf(:,99),Q(:,34),G1(:,:,:,80))
  call check_last_UV_W(l_switch,G1(:,:,:,80),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,96))
  call loop_UV_W(G0(:,:,:,18),Q(:,28),wf(:,-1),Q(:,2),G1(:,:,:,81))
  call loop_UV_W(G1(:,:,:,81),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,55))
  call check_last_UV_W(l_switch,G2(:,:,:,55),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,56))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,75),G0(:,:,:,25))
  call loop_GGG_G_12(G0(:,:,:,25),wf(:,-5),wf(:,-1),G0(:,:,:,26))
  call check_last_UV_W(l_switch,G0(:,:,:,26),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,46))
  call loop_GGG_G_12(G0(:,:,:,25),wf(:,-1),wf(:,-5),G0(:,:,:,27))
  call check_last_UV_W(l_switch,G0(:,:,:,27),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,47))
  call loop_GGG_G_23(G0(:,:,:,25),wf(:,-5),wf(:,-1),G0(:,:,:,28))
  call check_last_UV_W(l_switch,G0(:,:,:,28),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,48))
  call loop_GGG_G_12(G0(:,:,:,25),wf(:,-5),wf(:,0),G0(:,:,:,29))
  call check_last_UV_W(l_switch,G0(:,:,:,29),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,49))
  call loop_GGG_G_12(G0(:,:,:,25),wf(:,0),wf(:,-5),G0(:,:,:,30))
  call check_last_UV_W(l_switch,G0(:,:,:,30),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,50))
  call loop_GGG_G_23(G0(:,:,:,25),wf(:,-5),wf(:,0),G0(:,:,:,31))
  call check_last_UV_W(l_switch,G0(:,:,:,31),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,51))
  call loop_UV_W(G0(:,:,:,25),Q(:,28),wf(:,-5),Q(:,32),G1(:,:,:,82))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,82),wf(:,-1),wf(:,0),G1tensor(:,52))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,82),wf(:,0),wf(:,-1),G1tensor(:,53))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,82),wf(:,-1),wf(:,0),G1tensor(:,54))
  call check_last_UV_W(l_switch,G1(:,:,:,82),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,97))
  call loop_UV_W(G0(:,:,:,25),Q(:,28),wf(:,113),Q(:,33),G1(:,:,:,83))
  call check_last_UV_W(l_switch,G1(:,:,:,83),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,98))
  call loop_UV_W(G0(:,:,:,25),Q(:,28),wf(:,99),Q(:,34),G1(:,:,:,84))
  call check_last_UV_W(l_switch,G1(:,:,:,84),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,99))
  call loop_UV_W(G0(:,:,:,25),Q(:,28),wf(:,-1),Q(:,2),G1(:,:,:,85))
  call loop_UV_W(G1(:,:,:,85),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,56))
  call check_last_UV_W(l_switch,G2(:,:,:,56),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,57))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,75),G0(:,:,:,32))
  call loop_GGG_G_12(G0(:,:,:,32),wf(:,-5),wf(:,-2),G0(:,:,:,33))
  call check_last_UV_W(l_switch,G0(:,:,:,33),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,55))
  call loop_GGG_G_12(G0(:,:,:,32),wf(:,-2),wf(:,-5),G0(:,:,:,34))
  call check_last_UV_W(l_switch,G0(:,:,:,34),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,56))
  call loop_GGG_G_23(G0(:,:,:,32),wf(:,-5),wf(:,-2),G0(:,:,:,35))
  call check_last_UV_W(l_switch,G0(:,:,:,35),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,57))
  call loop_GGG_G_12(G0(:,:,:,32),wf(:,-5),wf(:,0),G0(:,:,:,36))
  call check_last_UV_W(l_switch,G0(:,:,:,36),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,58))
  call loop_GGG_G_12(G0(:,:,:,32),wf(:,0),wf(:,-5),G0(:,:,:,37))
  call check_last_UV_W(l_switch,G0(:,:,:,37),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,59))
  call loop_GGG_G_23(G0(:,:,:,32),wf(:,-5),wf(:,0),G0(:,:,:,38))
  call check_last_UV_W(l_switch,G0(:,:,:,38),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,60))
  call loop_UV_W(G0(:,:,:,32),Q(:,26),wf(:,-5),Q(:,32),G1(:,:,:,86))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,86),wf(:,-2),wf(:,0),G1tensor(:,61))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,86),wf(:,0),wf(:,-2),G1tensor(:,62))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,86),wf(:,-2),wf(:,0),G1tensor(:,63))
  call check_last_UV_W(l_switch,G1(:,:,:,86),Q(:,58),wf(:,90),Q(:,5),G2tensor(:,100))
  call loop_UV_W(G0(:,:,:,32),Q(:,26),wf(:,113),Q(:,33),G1(:,:,:,87))
  call check_last_UV_W(l_switch,G1(:,:,:,87),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,101))
  call loop_UV_W(G0(:,:,:,32),Q(:,26),wf(:,70),Q(:,36),G1(:,:,:,88))
  call check_last_UV_W(l_switch,G1(:,:,:,88),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,102))
  call loop_UV_W(G0(:,:,:,32),Q(:,26),wf(:,-2),Q(:,4),G1(:,:,:,89))
  call loop_UV_W(G1(:,:,:,89),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,57))
  call check_last_UV_W(l_switch,G2(:,:,:,57),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,58))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,75),wf(:,-1),G0(:,:,:,39))
  call loop_GGG_G_12(G0(:,:,:,39),wf(:,-5),wf(:,-2),G0(:,:,:,40))
  call check_last_UV_W(l_switch,G0(:,:,:,40),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,64))
  call loop_GGG_G_12(G0(:,:,:,39),wf(:,-2),wf(:,-5),G0(:,:,:,41))
  call check_last_UV_W(l_switch,G0(:,:,:,41),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,65))
  call loop_GGG_G_23(G0(:,:,:,39),wf(:,-5),wf(:,-2),G0(:,:,:,42))
  call check_last_UV_W(l_switch,G0(:,:,:,42),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,66))
  call loop_GGG_G_12(G0(:,:,:,39),wf(:,-5),wf(:,0),G0(:,:,:,43))
  call check_last_UV_W(l_switch,G0(:,:,:,43),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,67))
  call loop_GGG_G_12(G0(:,:,:,39),wf(:,0),wf(:,-5),G0(:,:,:,44))
  call check_last_UV_W(l_switch,G0(:,:,:,44),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,68))
  call loop_GGG_G_23(G0(:,:,:,39),wf(:,-5),wf(:,0),G0(:,:,:,45))
  call check_last_UV_W(l_switch,G0(:,:,:,45),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,69))
  call loop_UV_W(G0(:,:,:,39),Q(:,26),wf(:,-5),Q(:,32),G1(:,:,:,90))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,90),wf(:,-2),wf(:,0),G1tensor(:,70))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,90),wf(:,0),wf(:,-2),G1tensor(:,71))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,90),wf(:,-2),wf(:,0),G1tensor(:,72))
  call check_last_UV_W(l_switch,G1(:,:,:,90),Q(:,58),wf(:,90),Q(:,5),G2tensor(:,103))
  call loop_UV_W(G0(:,:,:,39),Q(:,26),wf(:,113),Q(:,33),G1(:,:,:,91))
  call check_last_UV_W(l_switch,G1(:,:,:,91),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,104))
  call loop_UV_W(G0(:,:,:,39),Q(:,26),wf(:,70),Q(:,36),G1(:,:,:,92))
  call check_last_UV_W(l_switch,G1(:,:,:,92),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,105))
  call loop_UV_W(G0(:,:,:,39),Q(:,26),wf(:,-2),Q(:,4),G1(:,:,:,93))
  call loop_UV_W(G1(:,:,:,93),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,58))
  call check_last_UV_W(l_switch,G2(:,:,:,58),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,59))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,75),G0(:,:,:,46))
  call loop_GGG_G_12(G0(:,:,:,46),wf(:,-5),wf(:,-2),G0(:,:,:,47))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,73))
  call loop_GGG_G_12(G0(:,:,:,46),wf(:,-2),wf(:,-5),G0(:,:,:,48))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,74))
  call loop_GGG_G_23(G0(:,:,:,46),wf(:,-5),wf(:,-2),G0(:,:,:,49))
  call check_last_UV_W(l_switch,G0(:,:,:,49),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,75))
  call loop_GGG_G_12(G0(:,:,:,46),wf(:,-5),wf(:,0),G0(:,:,:,50))
  call check_last_UV_W(l_switch,G0(:,:,:,50),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,76))
  call loop_GGG_G_12(G0(:,:,:,46),wf(:,0),wf(:,-5),G0(:,:,:,51))
  call check_last_UV_W(l_switch,G0(:,:,:,51),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,77))
  call loop_GGG_G_23(G0(:,:,:,46),wf(:,-5),wf(:,0),G0(:,:,:,52))
  call check_last_UV_W(l_switch,G0(:,:,:,52),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,78))
  call loop_UV_W(G0(:,:,:,46),Q(:,26),wf(:,-5),Q(:,32),G1(:,:,:,94))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,94),wf(:,-2),wf(:,0),G1tensor(:,79))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,94),wf(:,0),wf(:,-2),G1tensor(:,80))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,94),wf(:,-2),wf(:,0),G1tensor(:,81))
  call check_last_UV_W(l_switch,G1(:,:,:,94),Q(:,58),wf(:,90),Q(:,5),G2tensor(:,106))
  call loop_UV_W(G0(:,:,:,46),Q(:,26),wf(:,113),Q(:,33),G1(:,:,:,95))
  call check_last_UV_W(l_switch,G1(:,:,:,95),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,107))
  call loop_UV_W(G0(:,:,:,46),Q(:,26),wf(:,70),Q(:,36),G1(:,:,:,96))
  call check_last_UV_W(l_switch,G1(:,:,:,96),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,108))
  call loop_UV_W(G0(:,:,:,46),Q(:,26),wf(:,-2),Q(:,4),G1(:,:,:,97))
  call loop_UV_W(G1(:,:,:,97),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,59))
  call check_last_UV_W(l_switch,G2(:,:,:,59),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,60))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,38),G0(:,:,:,53))
  call loop_UV_W(G0(:,:,:,53),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,98))
  call check_last_UV_W(l_switch,G1(:,:,:,98),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,109))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,41),G0(:,:,:,54))
  call loop_UV_W(G0(:,:,:,54),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,99))
  call check_last_UV_W(l_switch,G1(:,:,:,99),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,110))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,42),G0(:,:,:,55))
  call loop_UV_W(G0(:,:,:,55),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,100))
  call check_last_UV_W(l_switch,G1(:,:,:,100),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,111))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,38),wf(:,-2),G0(:,:,:,56))
  call loop_UV_W(G0(:,:,:,56),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,101))
  call check_last_UV_W(l_switch,G1(:,:,:,101),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,112))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,41),wf(:,-2),G0(:,:,:,57))
  call loop_UV_W(G0(:,:,:,57),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,102))
  call check_last_UV_W(l_switch,G1(:,:,:,102),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,113))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,42),wf(:,-2),G0(:,:,:,58))
  call loop_UV_W(G0(:,:,:,58),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,103))
  call check_last_UV_W(l_switch,G1(:,:,:,103),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,114))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,38),G0(:,:,:,59))
  call loop_UV_W(G0(:,:,:,59),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,104))
  call check_last_UV_W(l_switch,G1(:,:,:,104),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,115))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,41),G0(:,:,:,60))
  call loop_UV_W(G0(:,:,:,60),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,105))
  call check_last_UV_W(l_switch,G1(:,:,:,105),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,116))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,42),G0(:,:,:,61))
  call loop_UV_W(G0(:,:,:,61),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,106))
  call check_last_UV_W(l_switch,G1(:,:,:,106),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,117))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,38),Q(:,26),G1(:,:,:,107))
  call loop_GGG_G_12(G1(:,:,:,107),wf(:,-5),wf(:,-2),G1(:,:,:,108))
  call check_last_UV_W(l_switch,G1(:,:,:,108),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,118))
  call loop_GGG_G_12(G1(:,:,:,107),wf(:,-2),wf(:,-5),G1(:,:,:,109))
  call check_last_UV_W(l_switch,G1(:,:,:,109),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,119))
  call loop_GGG_G_23(G1(:,:,:,107),wf(:,-5),wf(:,-2),G1(:,:,:,110))
  call check_last_UV_W(l_switch,G1(:,:,:,110),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,120))
  call loop_GGG_G_12(G1(:,:,:,107),wf(:,-5),wf(:,0),G1(:,:,:,111))
  call check_last_UV_W(l_switch,G1(:,:,:,111),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,121))
  call loop_GGG_G_12(G1(:,:,:,107),wf(:,0),wf(:,-5),G1(:,:,:,112))
  call check_last_UV_W(l_switch,G1(:,:,:,112),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,122))
  call loop_GGG_G_23(G1(:,:,:,107),wf(:,-5),wf(:,0),G1(:,:,:,113))
  call check_last_UV_W(l_switch,G1(:,:,:,113),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,123))
  call loop_UV_W(G1(:,:,:,107),Q(:,26),wf(:,-5),Q(:,32),G2(:,:,:,60))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,60),wf(:,-2),wf(:,0),G2tensor(:,124))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,60),wf(:,0),wf(:,-2),G2tensor(:,125))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,60),wf(:,-2),wf(:,0),G2tensor(:,126))
  call check_last_UV_W(l_switch,G2(:,:,:,60),Q(:,58),wf(:,90),Q(:,5),G3tensor(:,61))
  call loop_UV_W(G1(:,:,:,107),Q(:,26),wf(:,113),Q(:,33),G2(:,:,:,61))
  call check_last_UV_W(l_switch,G2(:,:,:,61),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,62))
  call loop_UV_W(G1(:,:,:,107),Q(:,26),wf(:,70),Q(:,36),G2(:,:,:,62))
  call check_last_UV_W(l_switch,G2(:,:,:,62),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,63))
  call loop_UV_W(G1(:,:,:,107),Q(:,26),wf(:,-2),Q(:,4),G2(:,:,:,63))
  call loop_UV_W(G2(:,:,:,63),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,12))
  call check_last_UV_W(l_switch,G3(:,:,:,12),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,10))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,41),Q(:,26),G1(:,:,:,114))
  call loop_GGG_G_12(G1(:,:,:,114),wf(:,-5),wf(:,-2),G1(:,:,:,115))
  call check_last_UV_W(l_switch,G1(:,:,:,115),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,127))
  call loop_GGG_G_12(G1(:,:,:,114),wf(:,-2),wf(:,-5),G1(:,:,:,116))
  call check_last_UV_W(l_switch,G1(:,:,:,116),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,128))
  call loop_GGG_G_23(G1(:,:,:,114),wf(:,-5),wf(:,-2),G1(:,:,:,117))
  call check_last_UV_W(l_switch,G1(:,:,:,117),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,129))
  call loop_GGG_G_12(G1(:,:,:,114),wf(:,-5),wf(:,0),G1(:,:,:,118))
  call check_last_UV_W(l_switch,G1(:,:,:,118),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,130))
  call loop_GGG_G_12(G1(:,:,:,114),wf(:,0),wf(:,-5),G1(:,:,:,119))
  call check_last_UV_W(l_switch,G1(:,:,:,119),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,131))
  call loop_GGG_G_23(G1(:,:,:,114),wf(:,-5),wf(:,0),G1(:,:,:,120))
  call check_last_UV_W(l_switch,G1(:,:,:,120),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,132))
  call loop_UV_W(G1(:,:,:,114),Q(:,26),wf(:,-5),Q(:,32),G2(:,:,:,64))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,64),wf(:,-2),wf(:,0),G2tensor(:,133))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,64),wf(:,0),wf(:,-2),G2tensor(:,134))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,64),wf(:,-2),wf(:,0),G2tensor(:,135))
  call check_last_UV_W(l_switch,G2(:,:,:,64),Q(:,58),wf(:,90),Q(:,5),G3tensor(:,64))
  call loop_UV_W(G1(:,:,:,114),Q(:,26),wf(:,113),Q(:,33),G2(:,:,:,65))
  call check_last_UV_W(l_switch,G2(:,:,:,65),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,65))
  call loop_UV_W(G1(:,:,:,114),Q(:,26),wf(:,70),Q(:,36),G2(:,:,:,66))
  call check_last_UV_W(l_switch,G2(:,:,:,66),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,66))
  call loop_UV_W(G1(:,:,:,114),Q(:,26),wf(:,-2),Q(:,4),G2(:,:,:,67))
  call loop_UV_W(G2(:,:,:,67),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,13))
  call check_last_UV_W(l_switch,G3(:,:,:,13),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,11))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,42),Q(:,26),G1(:,:,:,121))
  call loop_GGG_G_12(G1(:,:,:,121),wf(:,-5),wf(:,-2),G1(:,:,:,122))
  call check_last_UV_W(l_switch,G1(:,:,:,122),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,136))
  call loop_GGG_G_12(G1(:,:,:,121),wf(:,-2),wf(:,-5),G1(:,:,:,123))
  call check_last_UV_W(l_switch,G1(:,:,:,123),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,137))
  call loop_GGG_G_23(G1(:,:,:,121),wf(:,-5),wf(:,-2),G1(:,:,:,124))
  call check_last_UV_W(l_switch,G1(:,:,:,124),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,138))
  call loop_GGG_G_12(G1(:,:,:,121),wf(:,-5),wf(:,0),G1(:,:,:,125))
  call check_last_UV_W(l_switch,G1(:,:,:,125),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,139))
  call loop_GGG_G_12(G1(:,:,:,121),wf(:,0),wf(:,-5),G1(:,:,:,126))
  call check_last_UV_W(l_switch,G1(:,:,:,126),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,140))
  call loop_GGG_G_23(G1(:,:,:,121),wf(:,-5),wf(:,0),G1(:,:,:,127))
  call check_last_UV_W(l_switch,G1(:,:,:,127),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,141))
  call loop_UV_W(G1(:,:,:,121),Q(:,26),wf(:,-5),Q(:,32),G2(:,:,:,68))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,68),wf(:,-2),wf(:,0),G2tensor(:,142))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,68),wf(:,0),wf(:,-2),G2tensor(:,143))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,68),wf(:,-2),wf(:,0),G2tensor(:,144))
  call check_last_UV_W(l_switch,G2(:,:,:,68),Q(:,58),wf(:,90),Q(:,5),G3tensor(:,67))
  call loop_UV_W(G1(:,:,:,121),Q(:,26),wf(:,113),Q(:,33),G2(:,:,:,69))
  call check_last_UV_W(l_switch,G2(:,:,:,69),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,68))
  call loop_UV_W(G1(:,:,:,121),Q(:,26),wf(:,70),Q(:,36),G2(:,:,:,70))
  call check_last_UV_W(l_switch,G2(:,:,:,70),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,69))
  call loop_UV_W(G1(:,:,:,121),Q(:,26),wf(:,-2),Q(:,4),G2(:,:,:,71))
  call loop_UV_W(G2(:,:,:,71),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,14))
  call check_last_UV_W(l_switch,G3(:,:,:,14),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,12))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,20),G0(:,:,:,62))
  call loop_UV_W(G0(:,:,:,62),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,128))
  call check_last_UV_W(l_switch,G1(:,:,:,128),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,145))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,20),wf(:,-1),G0(:,:,:,63))
  call loop_UV_W(G0(:,:,:,63),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,129))
  call check_last_UV_W(l_switch,G1(:,:,:,129),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,146))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,20),G0(:,:,:,64))
  call loop_UV_W(G0(:,:,:,64),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,130))
  call check_last_UV_W(l_switch,G1(:,:,:,130),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,147))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,23),G0(:,:,:,65))
  call loop_UV_W(G0(:,:,:,65),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,131))
  call check_last_UV_W(l_switch,G1(:,:,:,131),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,148))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,23),wf(:,-1),G0(:,:,:,66))
  call loop_UV_W(G0(:,:,:,66),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,132))
  call check_last_UV_W(l_switch,G1(:,:,:,132),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,149))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,23),G0(:,:,:,67))
  call loop_UV_W(G0(:,:,:,67),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,133))
  call check_last_UV_W(l_switch,G1(:,:,:,133),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,150))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,24),G0(:,:,:,68))
  call loop_UV_W(G0(:,:,:,68),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,134))
  call check_last_UV_W(l_switch,G1(:,:,:,134),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,151))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,24),wf(:,-1),G0(:,:,:,69))
  call loop_UV_W(G0(:,:,:,69),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,135))
  call check_last_UV_W(l_switch,G1(:,:,:,135),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,152))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,24),G0(:,:,:,70))
  call loop_UV_W(G0(:,:,:,70),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,136))
  call check_last_UV_W(l_switch,G1(:,:,:,136),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,153))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,20),Q(:,28),G1(:,:,:,137))
  call loop_GGG_G_12(G1(:,:,:,137),wf(:,-5),wf(:,-1),G1(:,:,:,138))
  call check_last_UV_W(l_switch,G1(:,:,:,138),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,154))
  call loop_GGG_G_12(G1(:,:,:,137),wf(:,-1),wf(:,-5),G1(:,:,:,139))
  call check_last_UV_W(l_switch,G1(:,:,:,139),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,155))
  call loop_GGG_G_23(G1(:,:,:,137),wf(:,-5),wf(:,-1),G1(:,:,:,140))
  call check_last_UV_W(l_switch,G1(:,:,:,140),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,156))
  call loop_GGG_G_12(G1(:,:,:,137),wf(:,-5),wf(:,0),G1(:,:,:,141))
  call check_last_UV_W(l_switch,G1(:,:,:,141),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,157))
  call loop_GGG_G_12(G1(:,:,:,137),wf(:,0),wf(:,-5),G1(:,:,:,142))
  call check_last_UV_W(l_switch,G1(:,:,:,142),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,158))
  call loop_GGG_G_23(G1(:,:,:,137),wf(:,-5),wf(:,0),G1(:,:,:,143))
  call check_last_UV_W(l_switch,G1(:,:,:,143),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,159))
  call loop_UV_W(G1(:,:,:,137),Q(:,28),wf(:,-5),Q(:,32),G2(:,:,:,72))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,72),wf(:,-1),wf(:,0),G2tensor(:,160))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,72),wf(:,0),wf(:,-1),G2tensor(:,161))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,72),wf(:,-1),wf(:,0),G2tensor(:,162))
  call check_last_UV_W(l_switch,G2(:,:,:,72),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,70))
  call loop_UV_W(G1(:,:,:,137),Q(:,28),wf(:,113),Q(:,33),G2(:,:,:,73))
  call check_last_UV_W(l_switch,G2(:,:,:,73),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,71))
  call loop_UV_W(G1(:,:,:,137),Q(:,28),wf(:,99),Q(:,34),G2(:,:,:,74))
  call check_last_UV_W(l_switch,G2(:,:,:,74),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,72))
  call loop_UV_W(G1(:,:,:,137),Q(:,28),wf(:,-1),Q(:,2),G2(:,:,:,75))
  call loop_UV_W(G2(:,:,:,75),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,15))
  call check_last_UV_W(l_switch,G3(:,:,:,15),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,13))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,23),Q(:,28),G1(:,:,:,144))
  call loop_GGG_G_12(G1(:,:,:,144),wf(:,-5),wf(:,-1),G1(:,:,:,145))
  call check_last_UV_W(l_switch,G1(:,:,:,145),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,163))
  call loop_GGG_G_12(G1(:,:,:,144),wf(:,-1),wf(:,-5),G1(:,:,:,146))
  call check_last_UV_W(l_switch,G1(:,:,:,146),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,164))
  call loop_GGG_G_23(G1(:,:,:,144),wf(:,-5),wf(:,-1),G1(:,:,:,147))
  call check_last_UV_W(l_switch,G1(:,:,:,147),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,165))
  call loop_GGG_G_12(G1(:,:,:,144),wf(:,-5),wf(:,0),G1(:,:,:,148))
  call check_last_UV_W(l_switch,G1(:,:,:,148),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,166))
  call loop_GGG_G_12(G1(:,:,:,144),wf(:,0),wf(:,-5),G1(:,:,:,149))
  call check_last_UV_W(l_switch,G1(:,:,:,149),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,167))
  call loop_GGG_G_23(G1(:,:,:,144),wf(:,-5),wf(:,0),G1(:,:,:,150))
  call check_last_UV_W(l_switch,G1(:,:,:,150),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,168))
  call loop_UV_W(G1(:,:,:,144),Q(:,28),wf(:,-5),Q(:,32),G2(:,:,:,76))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,76),wf(:,-1),wf(:,0),G2tensor(:,169))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,76),wf(:,0),wf(:,-1),G2tensor(:,170))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,76),wf(:,-1),wf(:,0),G2tensor(:,171))
  call check_last_UV_W(l_switch,G2(:,:,:,76),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,73))
  call loop_UV_W(G1(:,:,:,144),Q(:,28),wf(:,113),Q(:,33),G2(:,:,:,77))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,74))
  call loop_UV_W(G1(:,:,:,144),Q(:,28),wf(:,99),Q(:,34),G2(:,:,:,78))
  call check_last_UV_W(l_switch,G2(:,:,:,78),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,75))
  call loop_UV_W(G1(:,:,:,144),Q(:,28),wf(:,-1),Q(:,2),G2(:,:,:,79))
  call loop_UV_W(G2(:,:,:,79),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,16))
  call check_last_UV_W(l_switch,G3(:,:,:,16),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,14))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,24),Q(:,28),G1(:,:,:,151))
  call loop_GGG_G_12(G1(:,:,:,151),wf(:,-5),wf(:,-1),G1(:,:,:,152))
  call check_last_UV_W(l_switch,G1(:,:,:,152),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,172))
  call loop_GGG_G_12(G1(:,:,:,151),wf(:,-1),wf(:,-5),G1(:,:,:,153))
  call check_last_UV_W(l_switch,G1(:,:,:,153),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,173))
  call loop_GGG_G_23(G1(:,:,:,151),wf(:,-5),wf(:,-1),G1(:,:,:,154))
  call check_last_UV_W(l_switch,G1(:,:,:,154),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,174))
  call loop_GGG_G_12(G1(:,:,:,151),wf(:,-5),wf(:,0),G1(:,:,:,155))
  call check_last_UV_W(l_switch,G1(:,:,:,155),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,175))
  call loop_GGG_G_12(G1(:,:,:,151),wf(:,0),wf(:,-5),G1(:,:,:,156))
  call check_last_UV_W(l_switch,G1(:,:,:,156),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,176))
  call loop_GGG_G_23(G1(:,:,:,151),wf(:,-5),wf(:,0),G1(:,:,:,157))
  call check_last_UV_W(l_switch,G1(:,:,:,157),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,177))
  call loop_UV_W(G1(:,:,:,151),Q(:,28),wf(:,-5),Q(:,32),G2(:,:,:,80))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,80),wf(:,-1),wf(:,0),G2tensor(:,178))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,80),wf(:,0),wf(:,-1),G2tensor(:,179))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,80),wf(:,-1),wf(:,0),G2tensor(:,180))
  call check_last_UV_W(l_switch,G2(:,:,:,80),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,76))
  call loop_UV_W(G1(:,:,:,151),Q(:,28),wf(:,113),Q(:,33),G2(:,:,:,81))
  call check_last_UV_W(l_switch,G2(:,:,:,81),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,77))
  call loop_UV_W(G1(:,:,:,151),Q(:,28),wf(:,99),Q(:,34),G2(:,:,:,82))
  call check_last_UV_W(l_switch,G2(:,:,:,82),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,78))
  call loop_UV_W(G1(:,:,:,151),Q(:,28),wf(:,-1),Q(:,2),G2(:,:,:,83))
  call loop_UV_W(G2(:,:,:,83),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,17))
  call check_last_UV_W(l_switch,G3(:,:,:,17),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,15))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,91),G0(:,:,:,71))
  call loop_GGG_G_12(G0(:,:,:,71),wf(:,-5),wf(:,-4),G0(:,:,:,72))
  call check_last_UV_W(l_switch,G0(:,:,:,72),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,82))
  call loop_GGG_G_12(G0(:,:,:,71),wf(:,-4),wf(:,-5),G0(:,:,:,73))
  call check_last_UV_W(l_switch,G0(:,:,:,73),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,83))
  call loop_GGG_G_23(G0(:,:,:,71),wf(:,-5),wf(:,-4),G0(:,:,:,74))
  call check_last_UV_W(l_switch,G0(:,:,:,74),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,84))
  call loop_UV_W(G0(:,:,:,71),Q(:,11),wf(:,84),Q(:,48),G1(:,:,:,158))
  call check_last_UV_W(l_switch,G1(:,:,:,158),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,181))
  call loop_UV_W(G0(:,:,:,71),Q(:,11),wf(:,-5),Q(:,32),G1(:,:,:,159))
  call loop_UV_W(G1(:,:,:,159),Q(:,43),wf(:,-4),Q(:,16),G2(:,:,:,84))
  call check_last_UV_W(l_switch,G2(:,:,:,84),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,79))
  call loop_UV_W(G0(:,:,:,71),Q(:,11),wf(:,-4),Q(:,16),G1(:,:,:,160))
  call loop_UV_W(G1(:,:,:,160),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,85))
  call check_last_UV_W(l_switch,G2(:,:,:,85),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,80))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,91),wf(:,0),G0(:,:,:,75))
  call loop_GGG_G_12(G0(:,:,:,75),wf(:,-5),wf(:,-4),G0(:,:,:,76))
  call check_last_UV_W(l_switch,G0(:,:,:,76),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,85))
  call loop_GGG_G_12(G0(:,:,:,75),wf(:,-4),wf(:,-5),G0(:,:,:,77))
  call check_last_UV_W(l_switch,G0(:,:,:,77),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,86))
  call loop_GGG_G_23(G0(:,:,:,75),wf(:,-5),wf(:,-4),G0(:,:,:,78))
  call check_last_UV_W(l_switch,G0(:,:,:,78),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,87))
  call loop_UV_W(G0(:,:,:,75),Q(:,11),wf(:,84),Q(:,48),G1(:,:,:,161))
  call check_last_UV_W(l_switch,G1(:,:,:,161),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,182))
  call loop_UV_W(G0(:,:,:,75),Q(:,11),wf(:,-5),Q(:,32),G1(:,:,:,162))
  call loop_UV_W(G1(:,:,:,162),Q(:,43),wf(:,-4),Q(:,16),G2(:,:,:,86))
  call check_last_UV_W(l_switch,G2(:,:,:,86),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,81))
  call loop_UV_W(G0(:,:,:,75),Q(:,11),wf(:,-4),Q(:,16),G1(:,:,:,163))
  call loop_UV_W(G1(:,:,:,163),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,87))
  call check_last_UV_W(l_switch,G2(:,:,:,87),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,82))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,91),G0(:,:,:,79))
  call loop_GGG_G_12(G0(:,:,:,79),wf(:,-5),wf(:,-4),G0(:,:,:,80))
  call check_last_UV_W(l_switch,G0(:,:,:,80),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,88))
  call loop_GGG_G_12(G0(:,:,:,79),wf(:,-4),wf(:,-5),G0(:,:,:,81))
  call check_last_UV_W(l_switch,G0(:,:,:,81),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,89))
  call loop_GGG_G_23(G0(:,:,:,79),wf(:,-5),wf(:,-4),G0(:,:,:,82))
  call check_last_UV_W(l_switch,G0(:,:,:,82),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,90))
  call loop_UV_W(G0(:,:,:,79),Q(:,11),wf(:,84),Q(:,48),G1(:,:,:,164))
  call check_last_UV_W(l_switch,G1(:,:,:,164),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,183))
  call loop_UV_W(G0(:,:,:,79),Q(:,11),wf(:,-5),Q(:,32),G1(:,:,:,165))
  call loop_UV_W(G1(:,:,:,165),Q(:,43),wf(:,-4),Q(:,16),G2(:,:,:,88))
  call check_last_UV_W(l_switch,G2(:,:,:,88),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,83))
  call loop_UV_W(G0(:,:,:,79),Q(:,11),wf(:,-4),Q(:,16),G1(:,:,:,166))
  call loop_UV_W(G1(:,:,:,166),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,89))
  call check_last_UV_W(l_switch,G2(:,:,:,89),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,84))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,62),G0(:,:,:,83))
  call loop_GGG_G_12(G0(:,:,:,83),wf(:,-5),wf(:,-4),G0(:,:,:,84))
  call check_last_UV_W(l_switch,G0(:,:,:,84),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,91))
  call loop_GGG_G_12(G0(:,:,:,83),wf(:,-4),wf(:,-5),G0(:,:,:,85))
  call check_last_UV_W(l_switch,G0(:,:,:,85),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,92))
  call loop_GGG_G_23(G0(:,:,:,83),wf(:,-5),wf(:,-4),G0(:,:,:,86))
  call check_last_UV_W(l_switch,G0(:,:,:,86),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,93))
  call loop_UV_W(G0(:,:,:,83),Q(:,13),wf(:,84),Q(:,48),G1(:,:,:,167))
  call check_last_UV_W(l_switch,G1(:,:,:,167),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,184))
  call loop_UV_W(G0(:,:,:,83),Q(:,13),wf(:,-5),Q(:,32),G1(:,:,:,168))
  call loop_UV_W(G1(:,:,:,168),Q(:,45),wf(:,-4),Q(:,16),G2(:,:,:,90))
  call check_last_UV_W(l_switch,G2(:,:,:,90),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,85))
  call loop_UV_W(G0(:,:,:,83),Q(:,13),wf(:,-4),Q(:,16),G1(:,:,:,169))
  call loop_UV_W(G1(:,:,:,169),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,91))
  call check_last_UV_W(l_switch,G2(:,:,:,91),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,86))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,62),wf(:,0),G0(:,:,:,87))
  call loop_GGG_G_12(G0(:,:,:,87),wf(:,-5),wf(:,-4),G0(:,:,:,88))
  call check_last_UV_W(l_switch,G0(:,:,:,88),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,94))
  call loop_GGG_G_12(G0(:,:,:,87),wf(:,-4),wf(:,-5),G0(:,:,:,89))
  call check_last_UV_W(l_switch,G0(:,:,:,89),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,95))
  call loop_GGG_G_23(G0(:,:,:,87),wf(:,-5),wf(:,-4),G0(:,:,:,90))
  call check_last_UV_W(l_switch,G0(:,:,:,90),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,96))
  call loop_UV_W(G0(:,:,:,87),Q(:,13),wf(:,84),Q(:,48),G1(:,:,:,170))
  call check_last_UV_W(l_switch,G1(:,:,:,170),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,185))
  call loop_UV_W(G0(:,:,:,87),Q(:,13),wf(:,-5),Q(:,32),G1(:,:,:,171))
  call loop_UV_W(G1(:,:,:,171),Q(:,45),wf(:,-4),Q(:,16),G2(:,:,:,92))
  call check_last_UV_W(l_switch,G2(:,:,:,92),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,87))
  call loop_UV_W(G0(:,:,:,87),Q(:,13),wf(:,-4),Q(:,16),G1(:,:,:,172))
  call loop_UV_W(G1(:,:,:,172),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,93))
  call check_last_UV_W(l_switch,G2(:,:,:,93),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,88))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,62),G0(:,:,:,91))
  call loop_GGG_G_12(G0(:,:,:,91),wf(:,-5),wf(:,-4),G0(:,:,:,92))
  call check_last_UV_W(l_switch,G0(:,:,:,92),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,97))
  call loop_GGG_G_12(G0(:,:,:,91),wf(:,-4),wf(:,-5),G0(:,:,:,93))
  call check_last_UV_W(l_switch,G0(:,:,:,93),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,98))
  call loop_GGG_G_23(G0(:,:,:,91),wf(:,-5),wf(:,-4),G0(:,:,:,94))
  call check_last_UV_W(l_switch,G0(:,:,:,94),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,99))
  call loop_UV_W(G0(:,:,:,91),Q(:,13),wf(:,84),Q(:,48),G1(:,:,:,173))
  call check_last_UV_W(l_switch,G1(:,:,:,173),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,186))
  call loop_UV_W(G0(:,:,:,91),Q(:,13),wf(:,-5),Q(:,32),G1(:,:,:,174))
  call loop_UV_W(G1(:,:,:,174),Q(:,45),wf(:,-4),Q(:,16),G2(:,:,:,94))
  call check_last_UV_W(l_switch,G2(:,:,:,94),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,89))
  call loop_UV_W(G0(:,:,:,91),Q(:,13),wf(:,-4),Q(:,16),G1(:,:,:,175))
  call loop_UV_W(G1(:,:,:,175),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,95))
  call check_last_UV_W(l_switch,G2(:,:,:,95),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,90))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(4)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(42)+M(45)+M(52) &
    -M(54)+M(57)+M(64)-M(66)-M(69)+M(76)-M(77)-M(78)-M(79)+M(80)-M(81)-M(82)+M(83)+M(84)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94) &
    -M(97))+c(6)*(M(132)-M(203)+M(226)-M(241))) * den(43)
  T3sum(1:5,8) = T3sum(1:5,8) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(11)-M(12)+M(19)-M(20)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(42)+M(45)+M(52) &
    +M(54)+M(57)+M(64)-M(66)-M(69)+M(76)+M(77)-M(78)-M(79)-M(80)-M(81)-M(82)+M(83)+M(84)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94) &
    -M(97))+c(6)*(M(156)-M(209)+M(225)-M(239))) * den(43)
  T3sum(1:5,8) = T3sum(1:5,8) + Gcoeff * G1tensor(:,2)
  Gcoeff = (c(5)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(6)*(-M(132)+M(156)+M(203)-M(209)+M(225) &
    -M(226)-M(239)+M(241))) * den(43)
  T3sum(1:5,8) = T3sum(1:5,8) + Gcoeff * G1tensor(:,3)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)-M(11)+M(12)-M(19)+M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(42)+M(45)+M(52) &
    -M(54)+M(57)+M(64)-M(66)-M(69)-M(76)-M(77)-M(78)-M(79)+M(80)-M(81)-M(82)+M(83)+M(84)+M(85)+M(86)+M(87)-M(88)-M(91)-M(94) &
    +M(97))+c(6)*(M(135)-M(199)-M(204)+M(220))) * den(43)
  T3sum(1:5,8) = T3sum(1:5,8) + Gcoeff * G1tensor(:,4)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(42)+M(45)+M(52) &
    +M(54)+M(57)+M(64)-M(66)-M(69)-M(76)+M(77)-M(78)-M(79)-M(80)-M(81)-M(82)+M(83)+M(84)+M(85)+M(86)+M(87)-M(88)-M(91)-M(94) &
    +M(97))+c(6)*(M(159)-M(197)-M(210)+M(219))) * den(43)
  T3sum(1:5,8) = T3sum(1:5,8) + Gcoeff * G1tensor(:,5)
  Gcoeff = (c(5)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(6)*(-M(135)+M(159)-M(197)+M(199)+M(204) &
    -M(210)+M(219)-M(220))) * den(43)
  T3sum(1:5,8) = T3sum(1:5,8) + Gcoeff * G1tensor(:,6)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(76)+M(85)-M(88)+M(97))+c(6)*(-M(132)+M(135)-M(199)+M(203) &
    -M(204)+M(220)-M(226)+M(241))) * den(43)
  T3sum(1:5,8) = T3sum(1:5,8) + Gcoeff * G1tensor(:,7)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(76)+M(85)-M(88)+M(97))+c(6)*(-M(156)+M(159)-M(197)+M(209) &
    -M(210)+M(219)-M(225)+M(239))) * den(43)
  T3sum(1:5,8) = T3sum(1:5,8) + Gcoeff * G1tensor(:,8)
  Gcoeff = (c(6)*(M(132)-M(135)-M(156)+M(159)-M(197)+M(199)-M(203)+M(204)+M(209)-M(210)+M(219)-M(220)-M(225)+M(226)+M(239) &
    -M(241))) * den(43)
  T3sum(1:5,8) = T3sum(1:5,8) + Gcoeff * G1tensor(:,9)
  Gcoeff = (c(5)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(6)*(M(179)-M(185)-M(191) &
    +M(193)+M(228)-M(234)-M(245)+M(247))) * den(84)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,55)
  Gcoeff = (c(5)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(6)*(-M(179)+M(181)+M(187) &
    -M(193)-M(228)+M(231)+M(237)-M(247))) * den(84)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,56)
  Gcoeff = (c(5)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(6)*(-M(181)+M(185)-M(187) &
    +M(191)-M(231)+M(234)-M(237)+M(245))) * den(84)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,57)
  Gcoeff = (c(5)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(6)*(M(133)-M(157)-M(203) &
    +M(209)+M(239)-M(241)-M(243)+M(244))) * den(84)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,58)
  Gcoeff = (c(5)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(6)*(-M(133)+M(143)+M(167) &
    -M(209)-M(239)+M(240)+M(242)-M(244))) * den(84)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,59)
  Gcoeff = (c(5)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(6)*(-M(143)+M(157)-M(167) &
    +M(203)-M(240)+M(241)-M(242)+M(243))) * den(84)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,60)
  Gcoeff = (c(6)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(84)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,61)
  Gcoeff = (c(6)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(84)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,62)
  Gcoeff = (c(6)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(84)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,63)
  Gcoeff = (c(5)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(6)*(M(134)-M(158)+M(197) &
    -M(199)-M(201)+M(202)-M(204)+M(210))) * den(84)
  T3sum(1:15,32) = T3sum(1:15,32) + Gcoeff * G2tensor(:,64)
  Gcoeff = (c(5)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(6)*(-M(134)+M(144)+M(168) &
    -M(197)+M(198)+M(200)-M(202)-M(210))) * den(84)
  T3sum(1:15,32) = T3sum(1:15,32) + Gcoeff * G2tensor(:,73)
  Gcoeff = (c(5)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(6)*(-M(144)+M(158)-M(168) &
    -M(198)+M(199)-M(200)+M(201)+M(204))) * den(84)
  T3sum(1:15,32) = T3sum(1:15,32) + Gcoeff * G2tensor(:,82)
  Gcoeff = (c(5)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(6)*(M(133)-M(157)-M(203) &
    +M(209)+M(239)-M(241)-M(243)+M(244))) * den(84)
  T3sum(1:15,32) = T3sum(1:15,32) + Gcoeff * G2tensor(:,65)
  Gcoeff = (c(5)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(6)*(-M(133)+M(143)+M(167) &
    -M(209)-M(239)+M(240)+M(242)-M(244))) * den(84)
  T3sum(1:15,32) = T3sum(1:15,32) + Gcoeff * G2tensor(:,74)
  Gcoeff = (c(5)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(6)*(-M(143)+M(157)-M(167) &
    +M(203)-M(240)+M(241)-M(242)+M(243))) * den(84)
  T3sum(1:15,32) = T3sum(1:15,32) + Gcoeff * G2tensor(:,83)
  Gcoeff = (c(6)*(M(133)-M(134)-M(157)+M(158)-M(197)+M(199)+M(201)-M(202)-M(203)+M(204)+M(209)-M(210)+M(239)-M(241)-M(243) &
    +M(244))) * den(84)
  T3sum(1:15,32) = T3sum(1:15,32) + Gcoeff * G2tensor(:,66)
  Gcoeff = (c(6)*(-M(133)+M(134)+M(143)-M(144)+M(167)-M(168)+M(197)-M(198)-M(200)+M(202)-M(209)+M(210)-M(239)+M(240)+M(242) &
    -M(244))) * den(84)
  T3sum(1:15,32) = T3sum(1:15,32) + Gcoeff * G2tensor(:,75)
  Gcoeff = (c(6)*(-M(143)+M(144)+M(157)-M(158)-M(167)+M(168)+M(198)-M(199)+M(200)-M(201)+M(203)-M(204)-M(240)+M(241)-M(242) &
    +M(243))) * den(84)
  T3sum(1:15,32) = T3sum(1:15,32) + Gcoeff * G2tensor(:,84)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(25)+M(26)+M(29)-M(30)-M(37)+M(38)+M(39)-M(40)-M(45)+M(48)+M(57) &
    -M(60)+M(71)-M(72)+M(73)-M(83)+M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)+M(110)+M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(152)+M(154)+M(164)-M(170))) * den(20)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,28)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(37)+M(38)+M(39)-M(40)-M(45)+M(48)+M(57) &
    -M(60)+M(71)-M(72)+M(73)-M(83)+M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(163)-M(169)-M(230)+M(232))) * den(20)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,29)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(110)+M(116)+M(122)-M(124))+c(6)*(M(152)-M(154)+M(163)-M(164) &
    -M(169)+M(170)-M(230)+M(232))) * den(20)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,30)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(25)+M(26)+M(29)-M(30)-M(37)+M(38)+M(39)-M(40)-M(45)+M(48)-M(57) &
    +M(60)+M(71)+M(72)+M(73)-M(83)-M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(149)+M(150)+M(194)-M(218))) * den(20)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,37)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(37)+M(38)+M(39)-M(40)-M(45)+M(48)-M(57) &
    +M(60)+M(71)+M(72)+M(73)-M(83)-M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)-M(110)-M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(193)-M(217)-M(227)+M(228))) * den(20)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,38)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(110)+M(116)+M(122)-M(124))+c(6)*(M(149)-M(150)+M(193)-M(194) &
    -M(217)+M(218)-M(227)+M(228))) * den(20)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,39)
  Gcoeff = (c(5)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(149)+M(150)+M(152) &
    -M(154)-M(164)+M(170)+M(194)-M(218))) * den(20)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,46)
  Gcoeff = (c(5)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(163)+M(169)+M(193) &
    -M(217)-M(227)+M(228)+M(230)-M(232))) * den(20)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,47)
  Gcoeff = (c(6)*(M(149)-M(150)-M(152)+M(154)-M(163)+M(164)+M(169)-M(170)+M(193)-M(194)-M(217)+M(218)-M(227)+M(228)+M(230) &
    -M(232))) * den(20)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,48)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(57) &
    +M(60)-M(71)+M(72)+M(73)+M(83)-M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(140)-M(146)-M(176)+M(178))) * den(20)
  T3sum(1:5,37) = T3sum(1:5,37) + Gcoeff * G1tensor(:,55)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)-M(45)+M(48)-M(57) &
    +M(60)+M(71)+M(72)+M(73)-M(83)-M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(173)+M(174)+M(192)-M(216))) * den(20)
  T3sum(1:5,37) = T3sum(1:5,37) + Gcoeff * G1tensor(:,64)
  Gcoeff = (c(5)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(6)*(-M(140)+M(146)-M(173) &
    +M(174)+M(176)-M(178)+M(192)-M(216))) * den(20)
  T3sum(1:5,37) = T3sum(1:5,37) + Gcoeff * G1tensor(:,73)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(57) &
    +M(60)-M(71)+M(72)+M(73)+M(83)-M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(139)-M(145)-M(236)+M(238))) * den(20)
  T3sum(1:5,37) = T3sum(1:5,37) + Gcoeff * G1tensor(:,56)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)-M(45)+M(48)-M(57) &
    +M(60)+M(71)+M(72)+M(73)-M(83)-M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(191)-M(215)-M(233)+M(234))) * den(20)
  T3sum(1:5,37) = T3sum(1:5,37) + Gcoeff * G1tensor(:,65)
  Gcoeff = (c(5)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(6)*(-M(139)+M(145)+M(191) &
    -M(215)-M(233)+M(234)+M(236)-M(238))) * den(20)
  T3sum(1:5,37) = T3sum(1:5,37) + Gcoeff * G1tensor(:,74)
  Gcoeff = (c(5)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(125)+M(127)+M(129)-M(130))+c(6)*(M(139)-M(140)-M(145)+M(146) &
    +M(176)-M(178)-M(236)+M(238))) * den(20)
  T3sum(1:5,37) = T3sum(1:5,37) + Gcoeff * G1tensor(:,57)
  Gcoeff = (c(5)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(125)+M(127)+M(129)-M(130))+c(6)*(M(173)-M(174)+M(191)-M(192) &
    -M(215)+M(216)-M(233)+M(234))) * den(20)
  T3sum(1:5,37) = T3sum(1:5,37) + Gcoeff * G1tensor(:,66)
  Gcoeff = (c(6)*(-M(139)+M(140)+M(145)-M(146)+M(173)-M(174)-M(176)+M(178)+M(191)-M(192)-M(215)+M(216)-M(233)+M(234)+M(236) &
    -M(238))) * den(20)
  T3sum(1:5,37) = T3sum(1:5,37) + Gcoeff * G1tensor(:,75)
  Gcoeff = (c(4)*(M(1)-M(2)+M(5)-M(6)-M(7)+M(8)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)+M(42)+M(45)+M(52) &
    -M(54)-M(57)+M(64)-M(66)+M(69)+M(76)-M(77)-M(78)-M(79)+M(80)+M(81)-M(82)+M(83)-M(84)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94) &
    -M(97))+c(6)*(M(138)-M(205)+M(224)-M(235))) * den(43)
  T3sum(1:5,12) = T3sum(1:5,12) + Gcoeff * G1tensor(:,10)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)-M(11)+M(12)-M(19)+M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(42)-M(45)+M(52) &
    -M(54)-M(57)+M(64)+M(66)+M(69)+M(76)-M(77)+M(78)-M(79)+M(80)+M(81)-M(82)-M(83)-M(84)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94) &
    -M(97))+c(6)*(M(180)-M(215)+M(223)-M(233))) * den(43)
  T3sum(1:5,12) = T3sum(1:5,12) + Gcoeff * G1tensor(:,11)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(6)*(-M(138)+M(180)+M(205)-M(215)+M(223) &
    -M(224)-M(233)+M(235))) * den(43)
  T3sum(1:5,12) = T3sum(1:5,12) + Gcoeff * G1tensor(:,12)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(11)-M(12)+M(19)-M(20)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(42)+M(45)+M(52) &
    -M(54)-M(57)-M(64)-M(66)+M(69)+M(76)-M(77)-M(78)-M(79)+M(80)+M(81)+M(82)+M(83)-M(84)-M(85)+M(86)-M(87)+M(88)-M(91)+M(94) &
    -M(97))+c(6)*(M(141)-M(175)-M(206)+M(214))) * den(43)
  T3sum(1:5,12) = T3sum(1:5,12) + Gcoeff * G1tensor(:,13)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(5)+M(6)+M(7)-M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)+M(42)-M(45)+M(52) &
    -M(54)-M(57)-M(64)+M(66)+M(69)+M(76)-M(77)+M(78)-M(79)+M(80)+M(81)+M(82)-M(83)-M(84)-M(85)+M(86)-M(87)+M(88)-M(91)+M(94) &
    -M(97))+c(6)*(-M(173)+M(183)+M(213)-M(216))) * den(43)
  T3sum(1:5,12) = T3sum(1:5,12) + Gcoeff * G1tensor(:,14)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(6)*(-M(141)-M(173)+M(175)+M(183)+M(206) &
    +M(213)-M(214)-M(216))) * den(43)
  T3sum(1:5,12) = T3sum(1:5,12) + Gcoeff * G1tensor(:,15)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(6)*(-M(138)+M(141)-M(175)+M(205) &
    -M(206)+M(214)-M(224)+M(235))) * den(43)
  T3sum(1:5,12) = T3sum(1:5,12) + Gcoeff * G1tensor(:,16)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(6)*(-M(173)-M(180)+M(183)+M(213) &
    +M(215)-M(216)-M(223)+M(233))) * den(43)
  T3sum(1:5,12) = T3sum(1:5,12) + Gcoeff * G1tensor(:,17)
  Gcoeff = (c(6)*(M(138)-M(141)-M(173)+M(175)-M(180)+M(183)-M(205)+M(206)+M(213)-M(214)+M(215)-M(216)-M(223)+M(224)+M(233) &
    -M(235))) * den(43)
  T3sum(1:5,12) = T3sum(1:5,12) + Gcoeff * G1tensor(:,18)
  Gcoeff = (c(5)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(6)*(M(155)-M(161)-M(167) &
    +M(169)+M(230)-M(240)-M(246)+M(249))) * den(67)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,109)
  Gcoeff = (c(5)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(6)*(-M(155)+M(157)+M(163) &
    -M(169)-M(230)+M(232)+M(243)-M(249))) * den(67)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,110)
  Gcoeff = (c(5)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(6)*(-M(157)+M(161)-M(163) &
    +M(167)-M(232)+M(240)-M(243)+M(246))) * den(67)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,111)
  Gcoeff = (c(5)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(6)*(M(139)-M(181)-M(205) &
    +M(215)+M(233)-M(235)-M(237)+M(238))) * den(67)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,112)
  Gcoeff = (c(5)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(6)*(-M(139)+M(145)+M(191) &
    -M(215)-M(233)+M(234)+M(236)-M(238))) * den(67)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,113)
  Gcoeff = (c(5)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(6)*(-M(145)+M(181)-M(191) &
    +M(205)-M(234)+M(235)-M(236)+M(237))) * den(67)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,114)
  Gcoeff = (c(6)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(67)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,115)
  Gcoeff = (c(6)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(67)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,116)
  Gcoeff = (c(6)*(-M(145)+M(157)-M(161)+M(163)-M(167)+M(181)-M(191)+M(205)+M(232)-M(234)+M(235)-M(236)+M(237)-M(240)+M(243) &
    -M(246))) * den(67)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,117)
  Gcoeff = (c(5)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(6)*(M(140)+M(173)-M(175) &
    -M(177)+M(178)-M(182)-M(206)+M(216))) * den(67)
  T3sum(1:15,37) = T3sum(1:15,37) + Gcoeff * G2tensor(:,118)
  Gcoeff = (c(5)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(6)*(-M(140)+M(146)-M(173) &
    +M(174)+M(176)-M(178)+M(192)-M(216))) * den(67)
  T3sum(1:15,37) = T3sum(1:15,37) + Gcoeff * G2tensor(:,127)
  Gcoeff = (c(5)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(6)*(-M(146)-M(174)+M(175) &
    -M(176)+M(177)+M(182)-M(192)+M(206))) * den(67)
  T3sum(1:15,37) = T3sum(1:15,37) + Gcoeff * G2tensor(:,136)
  Gcoeff = (c(5)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(6)*(M(139)-M(181)-M(205) &
    +M(215)+M(233)-M(235)-M(237)+M(238))) * den(67)
  T3sum(1:15,37) = T3sum(1:15,37) + Gcoeff * G2tensor(:,119)
  Gcoeff = (c(5)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(6)*(-M(139)+M(145)+M(191) &
    -M(215)-M(233)+M(234)+M(236)-M(238))) * den(67)
  T3sum(1:15,37) = T3sum(1:15,37) + Gcoeff * G2tensor(:,128)
  Gcoeff = (c(5)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(6)*(-M(145)+M(181)-M(191) &
    +M(205)-M(234)+M(235)-M(236)+M(237))) * den(67)
  T3sum(1:15,37) = T3sum(1:15,37) + Gcoeff * G2tensor(:,137)
  Gcoeff = (c(6)*(M(139)-M(140)-M(173)+M(175)+M(177)-M(178)-M(181)+M(182)-M(205)+M(206)+M(215)-M(216)+M(233)-M(235)-M(237) &
    +M(238))) * den(67)
  T3sum(1:15,37) = T3sum(1:15,37) + Gcoeff * G2tensor(:,120)
  Gcoeff = (c(6)*(-M(139)+M(140)+M(145)-M(146)+M(173)-M(174)-M(176)+M(178)+M(191)-M(192)-M(215)+M(216)-M(233)+M(234)+M(236) &
    -M(238))) * den(67)
  T3sum(1:15,37) = T3sum(1:15,37) + Gcoeff * G2tensor(:,129)
  Gcoeff = (c(6)*(-M(145)+M(146)+M(174)-M(175)+M(176)-M(177)+M(181)-M(182)-M(191)+M(192)+M(205)-M(206)-M(234)+M(235)-M(236) &
    +M(237))) * den(67)
  T3sum(1:15,37) = T3sum(1:15,37) + Gcoeff * G2tensor(:,138)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(5)+M(6)+M(7)-M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(42)-M(45)+M(52) &
    +M(54)+M(57)+M(64)+M(66)-M(69)+M(76)+M(77)+M(78)-M(79)-M(80)-M(81)-M(82)-M(83)+M(84)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94) &
    -M(97))+c(6)*(M(162)-M(211)+M(222)-M(229))) * den(43)
  T3sum(1:5,14) = T3sum(1:5,14) + Gcoeff * G1tensor(:,19)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(42)-M(45)-M(52) &
    +M(54)+M(57)+M(64)+M(66)-M(69)+M(76)+M(77)+M(78)+M(79)-M(80)-M(81)-M(82)-M(83)+M(84)-M(85)-M(86)+M(87)+M(88)+M(91)-M(94) &
    -M(97))+c(6)*(-M(151)+M(165)+M(208)-M(212))) * den(43)
  T3sum(1:5,14) = T3sum(1:5,14) + Gcoeff * G1tensor(:,22)
  Gcoeff = (c(5)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(6)*(-M(151)-M(162)+M(165)+M(208)+M(211) &
    -M(212)-M(222)+M(229))) * den(43)
  T3sum(1:5,14) = T3sum(1:5,14) + Gcoeff * G1tensor(:,25)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(42)-M(45)+M(52) &
    +M(54)-M(57)+M(64)+M(66)+M(69)+M(76)+M(77)+M(78)-M(79)-M(80)+M(81)-M(82)-M(83)-M(84)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94) &
    -M(97))+c(6)*(M(186)-M(217)+M(221)-M(227))) * den(43)
  T3sum(1:5,14) = T3sum(1:5,14) + Gcoeff * G1tensor(:,20)
  Gcoeff = (c(4)*(M(1)-M(2)+M(5)-M(6)-M(7)+M(8)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)-M(42)-M(45)-M(52) &
    +M(54)-M(57)+M(64)+M(66)+M(69)+M(76)+M(77)+M(78)+M(79)-M(80)+M(81)-M(82)-M(83)-M(84)-M(85)-M(86)+M(87)+M(88)+M(91)-M(94) &
    -M(97))+c(6)*(-M(149)+M(189)+M(207)-M(218))) * den(43)
  T3sum(1:5,14) = T3sum(1:5,14) + Gcoeff * G1tensor(:,23)
  Gcoeff = (c(5)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(6)*(-M(149)-M(186)+M(189)+M(207)+M(217) &
    -M(218)-M(221)+M(227))) * den(43)
  T3sum(1:5,14) = T3sum(1:5,14) + Gcoeff * G1tensor(:,26)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(27)+M(28)+M(33)-M(34)-M(57)+M(69)+M(81)-M(84))+c(6)*(-M(162)+M(186)+M(211)-M(217)+M(221) &
    -M(222)-M(227)+M(229))) * den(43)
  T3sum(1:5,14) = T3sum(1:5,14) + Gcoeff * G1tensor(:,21)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(27)+M(28)+M(33)-M(34)-M(57)+M(69)+M(81)-M(84))+c(6)*(-M(149)+M(151)-M(165)+M(189)+M(207) &
    -M(208)+M(212)-M(218))) * den(43)
  T3sum(1:5,14) = T3sum(1:5,14) + Gcoeff * G1tensor(:,24)
  Gcoeff = (c(6)*(-M(149)+M(151)+M(162)-M(165)-M(186)+M(189)+M(207)-M(208)-M(211)+M(212)+M(217)-M(218)-M(221)+M(222)+M(227) &
    -M(229))) * den(43)
  T3sum(1:5,14) = T3sum(1:5,14) + Gcoeff * G1tensor(:,27)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(131)-M(137)-M(143) &
    +M(145)+M(236)-M(242)-M(248)+M(250))) * den(30)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,145)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(163)-M(187)-M(211) &
    +M(217)+M(227)-M(229)-M(231)+M(232))) * den(30)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,146)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(30)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,147)
  Gcoeff = (c(5)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(131)+M(133)+M(139) &
    -M(145)-M(236)+M(238)+M(244)-M(250))) * den(30)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,148)
  Gcoeff = (c(5)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(163)+M(169)+M(193) &
    -M(217)-M(227)+M(228)+M(230)-M(232))) * den(30)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,149)
  Gcoeff = (c(6)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(30)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,150)
  Gcoeff = (c(5)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(6)*(-M(133)+M(137)-M(139) &
    +M(143)-M(238)+M(242)-M(244)+M(248))) * den(30)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,151)
  Gcoeff = (c(5)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(6)*(-M(169)+M(187)-M(193) &
    +M(211)-M(228)+M(229)-M(230)+M(231))) * den(30)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,152)
  Gcoeff = (c(6)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(30)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,153)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(149)-M(151)-M(153) &
    +M(154)+M(164)-M(188)-M(212)+M(218))) * den(30)
  T3sum(1:15,50) = T3sum(1:15,50) + Gcoeff * G2tensor(:,154)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(163)-M(187)-M(211) &
    +M(217)+M(227)-M(229)-M(231)+M(232))) * den(30)
  T3sum(1:15,50) = T3sum(1:15,50) + Gcoeff * G2tensor(:,155)
  Gcoeff = (c(6)*(-M(149)+M(151)+M(153)-M(154)+M(163)-M(164)-M(187)+M(188)-M(211)+M(212)+M(217)-M(218)+M(227)-M(229)-M(231) &
    +M(232))) * den(30)
  T3sum(1:15,50) = T3sum(1:15,50) + Gcoeff * G2tensor(:,156)
  Gcoeff = (c(5)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(149)+M(150)+M(152) &
    -M(154)-M(164)+M(170)+M(194)-M(218))) * den(30)
  T3sum(1:15,50) = T3sum(1:15,50) + Gcoeff * G2tensor(:,163)
  Gcoeff = (c(5)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(163)+M(169)+M(193) &
    -M(217)-M(227)+M(228)+M(230)-M(232))) * den(30)
  T3sum(1:15,50) = T3sum(1:15,50) + Gcoeff * G2tensor(:,164)
  Gcoeff = (c(6)*(M(149)-M(150)-M(152)+M(154)-M(163)+M(164)+M(169)-M(170)+M(193)-M(194)-M(217)+M(218)-M(227)+M(228)+M(230) &
    -M(232))) * den(30)
  T3sum(1:15,50) = T3sum(1:15,50) + Gcoeff * G2tensor(:,165)
  Gcoeff = (c(5)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(6)*(-M(150)+M(151)-M(152) &
    +M(153)-M(170)+M(188)-M(194)+M(212))) * den(30)
  T3sum(1:15,50) = T3sum(1:15,50) + Gcoeff * G2tensor(:,172)
  Gcoeff = (c(5)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(6)*(-M(169)+M(187)-M(193) &
    +M(211)-M(228)+M(229)-M(230)+M(231))) * den(30)
  T3sum(1:15,50) = T3sum(1:15,50) + Gcoeff * G2tensor(:,173)
  Gcoeff = (c(6)*(M(150)-M(151)+M(152)-M(153)-M(169)+M(170)+M(187)-M(188)-M(193)+M(194)+M(211)-M(212)-M(228)+M(229)-M(230) &
    +M(231))) * den(30)
  T3sum(1:15,50) = T3sum(1:15,50) + Gcoeff * G2tensor(:,174)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)-M(110)+M(111)+M(112)-M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(142)-M(171)+M(172)-M(184))) * den(33)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,82)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)-M(44)-M(45)-M(46) &
    +M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95)+M(103)+M(105)-M(107)-M(108)+M(109)-M(110)+M(111)+M(112)-M(113)+M(118) &
    -M(119)-M(124))+c(6)*(-M(137)+M(179)+M(247)-M(248))) * den(33)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,85)
  Gcoeff = (c(5)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(6)*(-M(137)-M(142)+M(171)-M(172) &
    +M(179)+M(184)+M(247)-M(248))) * den(33)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,88)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)+M(31)-M(32)-M(35)+M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)+M(110)+M(111)-M(112)-M(113)-M(118) &
    -M(119)+M(124))+c(6)*(M(140)-M(177)+M(178)-M(182))) * den(33)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,83)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)+M(31)-M(32)-M(35)+M(36)-M(44)-M(45)-M(46) &
    +M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95)+M(103)+M(105)-M(107)-M(108)+M(109)+M(110)+M(111)-M(112)-M(113)-M(118) &
    -M(119)+M(124))+c(6)*(-M(138)+M(180)+M(223)-M(224))) * den(33)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,86)
  Gcoeff = (c(5)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(6)*(-M(138)-M(140)+M(177)-M(178) &
    +M(180)+M(182)+M(223)-M(224))) * den(33)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,89)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)+M(31)-M(32)-M(35)+M(36)+M(110)-M(112)-M(118)+M(124))+c(6)*(M(140)-M(142)+M(171)-M(172) &
    -M(177)+M(178)-M(182)+M(184))) * den(33)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,84)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)+M(31)-M(32)-M(35)+M(36)+M(110)-M(112)-M(118)+M(124))+c(6)*(M(137)-M(138)-M(179)+M(180) &
    +M(223)-M(224)-M(247)+M(248))) * den(33)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,87)
  Gcoeff = (c(6)*(M(137)-M(138)-M(140)+M(142)-M(171)+M(172)+M(177)-M(178)-M(179)+M(180)+M(182)-M(184)+M(223)-M(224)-M(247) &
    +M(248))) * den(33)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,90)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    +M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(147)+M(148)+M(166)-M(190))) * den(12)
  T3sum(1:5,30) = T3sum(1:5,30) + Gcoeff * G1tensor(:,91)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(33)-M(34)+M(41)-M(44)-M(56) &
    -M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(161)+M(185)+M(245)-M(246))) * den(12)
  T3sum(1:5,30) = T3sum(1:5,30) + Gcoeff * G1tensor(:,94)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)-M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96))+c(6)*(M(147)-M(148)-M(161)-M(166) &
    +M(185)+M(190)+M(245)-M(246))) * den(12)
  T3sum(1:5,30) = T3sum(1:5,30) + Gcoeff * G1tensor(:,97)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    +M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(153)+M(154)+M(164)-M(188))) * den(12)
  T3sum(1:5,30) = T3sum(1:5,30) + Gcoeff * G1tensor(:,92)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(33)-M(34)+M(41)-M(44)-M(56) &
    -M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(162)+M(186)+M(221)-M(222))) * den(12)
  T3sum(1:5,30) = T3sum(1:5,30) + Gcoeff * G1tensor(:,95)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)-M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96))+c(6)*(M(153)-M(154)-M(162)-M(164) &
    +M(186)+M(188)+M(221)-M(222))) * den(12)
  T3sum(1:5,30) = T3sum(1:5,30) + Gcoeff * G1tensor(:,98)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(147)-M(148)-M(153)+M(154) &
    +M(164)-M(166)-M(188)+M(190))) * den(12)
  T3sum(1:5,30) = T3sum(1:5,30) + Gcoeff * G1tensor(:,93)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(161)-M(162)-M(185)+M(186) &
    +M(221)-M(222)-M(245)+M(246))) * den(12)
  T3sum(1:5,30) = T3sum(1:5,30) + Gcoeff * G1tensor(:,96)
  Gcoeff = (c(6)*(-M(147)+M(148)+M(153)-M(154)+M(161)-M(162)-M(164)+M(166)-M(185)+M(186)+M(188)-M(190)+M(221)-M(222)-M(245) &
    +M(246))) * den(12)
  T3sum(1:5,30) = T3sum(1:5,30) + Gcoeff * G1tensor(:,99)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)+M(73)+M(83)+M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)+M(110)+M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(131)-M(133)-M(244)+M(250))) * den(20)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,31)
  Gcoeff = (c(4)*(M(5)-M(6)-M(9)+M(10)+M(15)-M(16)-M(19)+M(20)-M(25)+M(26)+M(29)-M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100)+M(101)-M(103)-M(109)+M(110)+M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(193)+M(217)+M(227)-M(228))) * den(20)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,32)
  Gcoeff = (c(5)*(-M(9)+M(10)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)-M(73)+M(85)+M(97)-M(100))+c(6)*(-M(131)+M(133)-M(193)+M(217) &
    +M(227)-M(228)+M(244)-M(250))) * den(20)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,33)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(57) &
    +M(60)-M(71)+M(72)+M(73)+M(83)-M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(139)-M(145)-M(236)+M(238))) * den(20)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,40)
  Gcoeff = (c(4)*(M(5)-M(6)-M(9)+M(10)+M(15)-M(16)-M(19)+M(20)-M(25)+M(26)+M(29)-M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(57) &
    +M(60)-M(71)+M(72)-M(73)+M(83)-M(84)+M(85)+M(97)-M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(163)+M(169)+M(230)-M(232))) * den(20)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,41)
  Gcoeff = (c(5)*(-M(9)+M(10)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)-M(73)+M(85)+M(97)-M(100))+c(6)*(-M(139)+M(145)-M(163)+M(169) &
    +M(230)-M(232)+M(236)-M(238))) * den(20)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,42)
  Gcoeff = (c(5)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(131)+M(133)+M(139) &
    -M(145)-M(236)+M(238)+M(244)-M(250))) * den(20)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,49)
  Gcoeff = (c(5)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(163)+M(169)+M(193) &
    -M(217)-M(227)+M(228)+M(230)-M(232))) * den(20)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,50)
  Gcoeff = (c(6)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(20)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,51)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(131)-M(137)-M(143) &
    +M(145)+M(236)-M(242)-M(248)+M(250))) * den(30)
  T3sum(1:15,70) = T3sum(1:15,70) + Gcoeff * G2tensor(:,157)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(163)-M(187)-M(211) &
    +M(217)+M(227)-M(229)-M(231)+M(232))) * den(30)
  T3sum(1:15,70) = T3sum(1:15,70) + Gcoeff * G2tensor(:,158)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(30)
  T3sum(1:15,70) = T3sum(1:15,70) + Gcoeff * G2tensor(:,159)
  Gcoeff = (c(5)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(131)+M(133)+M(139) &
    -M(145)-M(236)+M(238)+M(244)-M(250))) * den(30)
  T3sum(1:15,70) = T3sum(1:15,70) + Gcoeff * G2tensor(:,166)
  Gcoeff = (c(5)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(163)+M(169)+M(193) &
    -M(217)-M(227)+M(228)+M(230)-M(232))) * den(30)
  T3sum(1:15,70) = T3sum(1:15,70) + Gcoeff * G2tensor(:,167)
  Gcoeff = (c(6)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(30)
  T3sum(1:15,70) = T3sum(1:15,70) + Gcoeff * G2tensor(:,168)
  Gcoeff = (c(5)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(6)*(-M(133)+M(137)-M(139) &
    +M(143)-M(238)+M(242)-M(244)+M(248))) * den(30)
  T3sum(1:15,70) = T3sum(1:15,70) + Gcoeff * G2tensor(:,175)
  Gcoeff = (c(5)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(6)*(-M(169)+M(187)-M(193) &
    +M(211)-M(228)+M(229)-M(230)+M(231))) * den(30)
  T3sum(1:15,70) = T3sum(1:15,70) + Gcoeff * G2tensor(:,176)
  Gcoeff = (c(6)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(30)
  T3sum(1:15,70) = T3sum(1:15,70) + Gcoeff * G2tensor(:,177)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(25)+M(26)+M(29)-M(30)-M(37)+M(38)+M(39)-M(40)-M(45)+M(48)+M(57) &
    -M(60)+M(71)-M(72)+M(73)-M(83)+M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)+M(110)+M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(152)+M(154)+M(164)-M(170))) * den(20)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,34)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)+M(73)+M(83)+M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)+M(110)+M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(131)-M(133)-M(244)+M(250))) * den(20)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,35)
  Gcoeff = (c(5)*(M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(71)+M(83))+c(6)*(M(131)-M(133)+M(152)-M(154) &
    -M(164)+M(170)-M(244)+M(250))) * den(20)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,36)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(25)+M(26)+M(29)-M(30)-M(37)+M(38)+M(39)-M(40)-M(45)+M(48)-M(57) &
    +M(60)+M(71)+M(72)+M(73)-M(83)-M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(149)+M(150)+M(194)-M(218))) * den(20)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,43)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(57) &
    +M(60)-M(71)+M(72)+M(73)+M(83)-M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(139)-M(145)-M(236)+M(238))) * den(20)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,44)
  Gcoeff = (c(5)*(M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(71)+M(83))+c(6)*(M(139)-M(145)+M(149)-M(150) &
    -M(194)+M(218)-M(236)+M(238))) * den(20)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,45)
  Gcoeff = (c(5)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(149)+M(150)+M(152) &
    -M(154)-M(164)+M(170)+M(194)-M(218))) * den(20)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,52)
  Gcoeff = (c(5)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(131)+M(133)+M(139) &
    -M(145)-M(236)+M(238)+M(244)-M(250))) * den(20)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,53)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(139)-M(145)+M(149)-M(150)-M(152)+M(154)+M(164)-M(170)-M(194)+M(218)-M(236)+M(238)+M(244) &
    -M(250))) * den(20)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,54)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(149)-M(151)-M(153) &
    +M(154)+M(164)-M(188)-M(212)+M(218))) * den(30)
  T3sum(1:15,82) = T3sum(1:15,82) + Gcoeff * G2tensor(:,160)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(131)-M(137)-M(143) &
    +M(145)+M(236)-M(242)-M(248)+M(250))) * den(30)
  T3sum(1:15,82) = T3sum(1:15,82) + Gcoeff * G2tensor(:,161)
  Gcoeff = (c(6)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(30)
  T3sum(1:15,82) = T3sum(1:15,82) + Gcoeff * G2tensor(:,162)
  Gcoeff = (c(5)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(149)+M(150)+M(152) &
    -M(154)-M(164)+M(170)+M(194)-M(218))) * den(30)
  T3sum(1:15,82) = T3sum(1:15,82) + Gcoeff * G2tensor(:,169)
  Gcoeff = (c(5)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(131)+M(133)+M(139) &
    -M(145)-M(236)+M(238)+M(244)-M(250))) * den(30)
  T3sum(1:15,82) = T3sum(1:15,82) + Gcoeff * G2tensor(:,170)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(139)-M(145)+M(149)-M(150)-M(152)+M(154)+M(164)-M(170)-M(194)+M(218)-M(236)+M(238)+M(244) &
    -M(250))) * den(30)
  T3sum(1:15,82) = T3sum(1:15,82) + Gcoeff * G2tensor(:,171)
  Gcoeff = (c(5)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(6)*(-M(150)+M(151)-M(152) &
    +M(153)-M(170)+M(188)-M(194)+M(212))) * den(30)
  T3sum(1:15,82) = T3sum(1:15,82) + Gcoeff * G2tensor(:,178)
  Gcoeff = (c(5)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(6)*(-M(133)+M(137)-M(139) &
    +M(143)-M(238)+M(242)-M(244)+M(248))) * den(30)
  T3sum(1:15,82) = T3sum(1:15,82) + Gcoeff * G2tensor(:,179)
  Gcoeff = (c(6)*(-M(133)+M(137)-M(139)+M(143)+M(150)-M(151)+M(152)-M(153)+M(170)-M(188)+M(194)-M(212)-M(238)+M(242)-M(244) &
    +M(248))) * den(30)
  T3sum(1:15,82) = T3sum(1:15,82) + Gcoeff * G2tensor(:,180)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(37)+M(38)+M(39)-M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)+M(73)+M(83)+M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(155)-M(157)-M(243)+M(249))) * den(20)
  T3sum(1:5,68) = T3sum(1:5,68) + Gcoeff * G1tensor(:,58)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(25)+M(26)+M(29)-M(30)-M(37)+M(38)+M(39)-M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(191)+M(215)+M(233)-M(234))) * den(20)
  T3sum(1:5,68) = T3sum(1:5,68) + Gcoeff * G1tensor(:,59)
  Gcoeff = (c(5)*(-M(9)+M(10)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)-M(73)+M(85)+M(97)-M(100))+c(6)*(-M(155)+M(157)-M(191)+M(215) &
    +M(233)-M(234)+M(243)-M(249))) * den(20)
  T3sum(1:5,68) = T3sum(1:5,68) + Gcoeff * G1tensor(:,60)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(37)+M(38)+M(39)-M(40)-M(45)+M(48)+M(57) &
    -M(60)+M(71)-M(72)+M(73)-M(83)+M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(163)-M(169)-M(230)+M(232))) * den(20)
  T3sum(1:5,68) = T3sum(1:5,68) + Gcoeff * G1tensor(:,67)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(25)+M(26)+M(29)-M(30)-M(37)+M(38)+M(39)-M(40)-M(45)+M(48)+M(57) &
    -M(60)+M(71)-M(72)-M(73)-M(83)+M(84)+M(85)+M(97)-M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(139)+M(145)+M(236)-M(238))) * den(20)
  T3sum(1:5,68) = T3sum(1:5,68) + Gcoeff * G1tensor(:,68)
  Gcoeff = (c(5)*(-M(9)+M(10)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)-M(73)+M(85)+M(97)-M(100))+c(6)*(-M(139)+M(145)-M(163)+M(169) &
    +M(230)-M(232)+M(236)-M(238))) * den(20)
  T3sum(1:5,68) = T3sum(1:5,68) + Gcoeff * G1tensor(:,69)
  Gcoeff = (c(5)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(6)*(-M(155)+M(157)+M(163) &
    -M(169)-M(230)+M(232)+M(243)-M(249))) * den(20)
  T3sum(1:5,68) = T3sum(1:5,68) + Gcoeff * G1tensor(:,76)
  Gcoeff = (c(5)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(6)*(-M(139)+M(145)+M(191) &
    -M(215)-M(233)+M(234)+M(236)-M(238))) * den(20)
  T3sum(1:5,68) = T3sum(1:5,68) + Gcoeff * G1tensor(:,77)
  Gcoeff = (c(6)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(20)
  T3sum(1:5,68) = T3sum(1:5,68) + Gcoeff * G1tensor(:,78)
  Gcoeff = (c(5)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(6)*(M(155)-M(161)-M(167) &
    +M(169)+M(230)-M(240)-M(246)+M(249))) * den(67)
  T3sum(1:15,68) = T3sum(1:15,68) + Gcoeff * G2tensor(:,121)
  Gcoeff = (c(5)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(6)*(M(139)-M(181)-M(205) &
    +M(215)+M(233)-M(235)-M(237)+M(238))) * den(67)
  T3sum(1:15,68) = T3sum(1:15,68) + Gcoeff * G2tensor(:,122)
  Gcoeff = (c(6)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(67)
  T3sum(1:15,68) = T3sum(1:15,68) + Gcoeff * G2tensor(:,123)
  Gcoeff = (c(5)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(6)*(-M(155)+M(157)+M(163) &
    -M(169)-M(230)+M(232)+M(243)-M(249))) * den(67)
  T3sum(1:15,68) = T3sum(1:15,68) + Gcoeff * G2tensor(:,130)
  Gcoeff = (c(5)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(6)*(-M(139)+M(145)+M(191) &
    -M(215)-M(233)+M(234)+M(236)-M(238))) * den(67)
  T3sum(1:15,68) = T3sum(1:15,68) + Gcoeff * G2tensor(:,131)
  Gcoeff = (c(6)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(67)
  T3sum(1:15,68) = T3sum(1:15,68) + Gcoeff * G2tensor(:,132)
  Gcoeff = (c(5)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(6)*(-M(157)+M(161)-M(163) &
    +M(167)-M(232)+M(240)-M(243)+M(246))) * den(67)
  T3sum(1:15,68) = T3sum(1:15,68) + Gcoeff * G2tensor(:,139)
  Gcoeff = (c(5)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(6)*(-M(145)+M(181)-M(191) &
    +M(205)-M(234)+M(235)-M(236)+M(237))) * den(67)
  T3sum(1:15,68) = T3sum(1:15,68) + Gcoeff * G2tensor(:,140)
  Gcoeff = (c(6)*(-M(145)+M(157)-M(161)+M(163)-M(167)+M(181)-M(191)+M(205)+M(232)-M(234)+M(235)-M(236)+M(237)-M(240)+M(243) &
    -M(246))) * den(67)
  T3sum(1:15,68) = T3sum(1:15,68) + Gcoeff * G2tensor(:,141)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(57) &
    +M(60)-M(71)+M(72)+M(73)+M(83)-M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(140)-M(146)-M(176)+M(178))) * den(20)
  T3sum(1:5,75) = T3sum(1:5,75) + Gcoeff * G1tensor(:,61)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(37)+M(38)+M(39)-M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)+M(73)+M(83)+M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(155)-M(157)-M(243)+M(249))) * den(20)
  T3sum(1:5,75) = T3sum(1:5,75) + Gcoeff * G1tensor(:,62)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(37)+M(38)+M(39)-M(40)+M(57)-M(60)-M(72)+M(84))+c(6)*(-M(140)+M(146)+M(155)-M(157) &
    +M(176)-M(178)-M(243)+M(249))) * den(20)
  T3sum(1:5,75) = T3sum(1:5,75) + Gcoeff * G1tensor(:,63)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)-M(45)+M(48)-M(57) &
    +M(60)+M(71)+M(72)+M(73)-M(83)-M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(173)+M(174)+M(192)-M(216))) * den(20)
  T3sum(1:5,75) = T3sum(1:5,75) + Gcoeff * G1tensor(:,70)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(37)+M(38)+M(39)-M(40)-M(45)+M(48)+M(57) &
    -M(60)+M(71)-M(72)+M(73)-M(83)+M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(163)-M(169)-M(230)+M(232))) * den(20)
  T3sum(1:5,75) = T3sum(1:5,75) + Gcoeff * G1tensor(:,71)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(37)+M(38)+M(39)-M(40)+M(57)-M(60)-M(72)+M(84))+c(6)*(M(163)-M(169)+M(173)-M(174)-M(192) &
    +M(216)-M(230)+M(232))) * den(20)
  T3sum(1:5,75) = T3sum(1:5,75) + Gcoeff * G1tensor(:,72)
  Gcoeff = (c(5)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(6)*(-M(140)+M(146)-M(173) &
    +M(174)+M(176)-M(178)+M(192)-M(216))) * den(20)
  T3sum(1:5,75) = T3sum(1:5,75) + Gcoeff * G1tensor(:,79)
  Gcoeff = (c(5)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(6)*(-M(155)+M(157)+M(163) &
    -M(169)-M(230)+M(232)+M(243)-M(249))) * den(20)
  T3sum(1:5,75) = T3sum(1:5,75) + Gcoeff * G1tensor(:,80)
  Gcoeff = (c(6)*(M(140)-M(146)-M(155)+M(157)+M(163)-M(169)+M(173)-M(174)-M(176)+M(178)-M(192)+M(216)-M(230)+M(232)+M(243) &
    -M(249))) * den(20)
  T3sum(1:5,75) = T3sum(1:5,75) + Gcoeff * G1tensor(:,81)
  Gcoeff = (c(5)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(6)*(M(140)+M(173)-M(175) &
    -M(177)+M(178)-M(182)-M(206)+M(216))) * den(67)
  T3sum(1:15,75) = T3sum(1:15,75) + Gcoeff * G2tensor(:,124)
  Gcoeff = (c(5)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(6)*(M(155)-M(161)-M(167) &
    +M(169)+M(230)-M(240)-M(246)+M(249))) * den(67)
  T3sum(1:15,75) = T3sum(1:15,75) + Gcoeff * G2tensor(:,125)
  Gcoeff = (c(6)*(-M(140)+M(155)-M(161)-M(167)+M(169)-M(173)+M(175)+M(177)-M(178)+M(182)+M(206)-M(216)+M(230)-M(240)-M(246) &
    +M(249))) * den(67)
  T3sum(1:15,75) = T3sum(1:15,75) + Gcoeff * G2tensor(:,126)
  Gcoeff = (c(5)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(6)*(-M(140)+M(146)-M(173) &
    +M(174)+M(176)-M(178)+M(192)-M(216))) * den(67)
  T3sum(1:15,75) = T3sum(1:15,75) + Gcoeff * G2tensor(:,133)
  Gcoeff = (c(5)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(6)*(-M(155)+M(157)+M(163) &
    -M(169)-M(230)+M(232)+M(243)-M(249))) * den(67)
  T3sum(1:15,75) = T3sum(1:15,75) + Gcoeff * G2tensor(:,134)
  Gcoeff = (c(6)*(M(140)-M(146)-M(155)+M(157)+M(163)-M(169)+M(173)-M(174)-M(176)+M(178)-M(192)+M(216)-M(230)+M(232)+M(243) &
    -M(249))) * den(67)
  T3sum(1:15,75) = T3sum(1:15,75) + Gcoeff * G2tensor(:,135)
  Gcoeff = (c(5)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(6)*(-M(146)-M(174)+M(175) &
    -M(176)+M(177)+M(182)-M(192)+M(206))) * den(67)
  T3sum(1:15,75) = T3sum(1:15,75) + Gcoeff * G2tensor(:,142)
  Gcoeff = (c(5)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(6)*(-M(157)+M(161)-M(163) &
    +M(167)-M(232)+M(240)-M(243)+M(246))) * den(67)
  T3sum(1:15,75) = T3sum(1:15,75) + Gcoeff * G2tensor(:,143)
  Gcoeff = (c(6)*(M(146)-M(157)+M(161)-M(163)+M(167)+M(174)-M(175)+M(176)-M(177)-M(182)+M(192)-M(206)-M(232)+M(240)-M(243) &
    +M(246))) * den(67)
  T3sum(1:15,75) = T3sum(1:15,75) + Gcoeff * G2tensor(:,144)
  Gcoeff = (c(5)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(6)*(M(179)-M(185)-M(191) &
    +M(193)+M(228)-M(234)-M(245)+M(247))) * den(84)
  T3sum(1:15,62) = T3sum(1:15,62) + Gcoeff * G2tensor(:,67)
  Gcoeff = (c(5)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(6)*(M(133)-M(157)-M(203) &
    +M(209)+M(239)-M(241)-M(243)+M(244))) * den(84)
  T3sum(1:15,62) = T3sum(1:15,62) + Gcoeff * G2tensor(:,68)
  Gcoeff = (c(6)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(84)
  T3sum(1:15,62) = T3sum(1:15,62) + Gcoeff * G2tensor(:,69)
  Gcoeff = (c(5)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(6)*(-M(179)+M(181)+M(187) &
    -M(193)-M(228)+M(231)+M(237)-M(247))) * den(84)
  T3sum(1:15,62) = T3sum(1:15,62) + Gcoeff * G2tensor(:,76)
  Gcoeff = (c(5)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(6)*(-M(133)+M(143)+M(167) &
    -M(209)-M(239)+M(240)+M(242)-M(244))) * den(84)
  T3sum(1:15,62) = T3sum(1:15,62) + Gcoeff * G2tensor(:,77)
  Gcoeff = (c(6)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(84)
  T3sum(1:15,62) = T3sum(1:15,62) + Gcoeff * G2tensor(:,78)
  Gcoeff = (c(5)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(6)*(-M(181)+M(185)-M(187) &
    +M(191)-M(231)+M(234)-M(237)+M(245))) * den(84)
  T3sum(1:15,62) = T3sum(1:15,62) + Gcoeff * G2tensor(:,85)
  Gcoeff = (c(5)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(6)*(-M(143)+M(157)-M(167) &
    +M(203)-M(240)+M(241)-M(242)+M(243))) * den(84)
  T3sum(1:15,62) = T3sum(1:15,62) + Gcoeff * G2tensor(:,86)
  Gcoeff = (c(6)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(84)
  T3sum(1:15,62) = T3sum(1:15,62) + Gcoeff * G2tensor(:,87)
  Gcoeff = (c(5)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(6)*(M(134)-M(158)+M(197) &
    -M(199)-M(201)+M(202)-M(204)+M(210))) * den(84)
  T3sum(1:15,64) = T3sum(1:15,64) + Gcoeff * G2tensor(:,70)
  Gcoeff = (c(5)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(6)*(M(179)-M(185)-M(191) &
    +M(193)+M(228)-M(234)-M(245)+M(247))) * den(84)
  T3sum(1:15,64) = T3sum(1:15,64) + Gcoeff * G2tensor(:,71)
  Gcoeff = (c(6)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(84)
  T3sum(1:15,64) = T3sum(1:15,64) + Gcoeff * G2tensor(:,72)
  Gcoeff = (c(5)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(6)*(-M(134)+M(144)+M(168) &
    -M(197)+M(198)+M(200)-M(202)-M(210))) * den(84)
  T3sum(1:15,64) = T3sum(1:15,64) + Gcoeff * G2tensor(:,79)
  Gcoeff = (c(5)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(6)*(-M(179)+M(181)+M(187) &
    -M(193)-M(228)+M(231)+M(237)-M(247))) * den(84)
  T3sum(1:15,64) = T3sum(1:15,64) + Gcoeff * G2tensor(:,80)
  Gcoeff = (c(6)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(84)
  T3sum(1:15,64) = T3sum(1:15,64) + Gcoeff * G2tensor(:,81)
  Gcoeff = (c(5)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(6)*(-M(144)+M(158)-M(168) &
    -M(198)+M(199)-M(200)+M(201)+M(204))) * den(84)
  T3sum(1:15,64) = T3sum(1:15,64) + Gcoeff * G2tensor(:,88)
  Gcoeff = (c(5)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(6)*(-M(181)+M(185)-M(187) &
    +M(191)-M(231)+M(234)-M(237)+M(245))) * den(84)
  T3sum(1:15,64) = T3sum(1:15,64) + Gcoeff * G2tensor(:,89)
  Gcoeff = (c(6)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(84)
  T3sum(1:15,64) = T3sum(1:15,64) + Gcoeff * G2tensor(:,90)
  Gcoeff = (c(5)*(M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(71)+M(83))+c(6)*(M(131)-M(133)+M(152)-M(154) &
    -M(164)+M(170)-M(244)+M(250))) * den(21)
  T3sum(1:15,82) = T3sum(1:15,82) + Gcoeff * G2tensor(:,91)
  Gcoeff = (c(5)*(M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(71)+M(83))+c(6)*(M(139)-M(145)+M(149)-M(150) &
    -M(194)+M(218)-M(236)+M(238))) * den(21)
  T3sum(1:15,82) = T3sum(1:15,82) + Gcoeff * G2tensor(:,94)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(139)-M(145)+M(149)-M(150)-M(152)+M(154)+M(164)-M(170)-M(194)+M(218)-M(236)+M(238)+M(244) &
    -M(250))) * den(21)
  T3sum(1:15,82) = T3sum(1:15,82) + Gcoeff * G2tensor(:,97)
  Gcoeff = (c(6)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(31)
  T3sum(1:35,82) = T3sum(1:35,82) + Gcoeff * G3tensor(:,70)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(139)-M(145)+M(149)-M(150)-M(152)+M(154)+M(164)-M(170)-M(194)+M(218)-M(236)+M(238)+M(244) &
    -M(250))) * den(31)
  T3sum(1:35,82) = T3sum(1:35,82) + Gcoeff * G3tensor(:,73)
  Gcoeff = (c(6)*(-M(133)+M(137)-M(139)+M(143)+M(150)-M(151)+M(152)-M(153)+M(170)-M(188)+M(194)-M(212)-M(238)+M(242)-M(244) &
    +M(248))) * den(31)
  T3sum(1:35,82) = T3sum(1:35,82) + Gcoeff * G3tensor(:,76)
  Gcoeff = (c(5)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(6)*(-M(180)+M(186)+M(215)-M(217)+M(221) &
    -M(223)-M(227)+M(233))) * den(44)
  T3sum(1:15,61) = T3sum(1:15,61) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(5)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(6)*(-M(135)+M(159)-M(197)+M(199)+M(204) &
    -M(210)+M(219)-M(220))) * den(44)
  T3sum(1:15,61) = T3sum(1:15,61) + Gcoeff * G2tensor(:,11)
  Gcoeff = (c(6)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(44)
  T3sum(1:15,61) = T3sum(1:15,61) + Gcoeff * G2tensor(:,12)
  Gcoeff = (c(5)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(6)*(-M(132)+M(156)+M(203)-M(209)+M(225) &
    -M(226)-M(239)+M(241))) * den(44)
  T3sum(1:15,66) = T3sum(1:15,66) + Gcoeff * G2tensor(:,13)
  Gcoeff = (c(5)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(6)*(-M(180)+M(186)+M(215)-M(217)+M(221) &
    -M(223)-M(227)+M(233))) * den(44)
  T3sum(1:15,66) = T3sum(1:15,66) + Gcoeff * G2tensor(:,14)
  Gcoeff = (c(6)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(44)
  T3sum(1:15,66) = T3sum(1:15,66) + Gcoeff * G2tensor(:,15)
  Gcoeff = (c(5)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(6)*(-M(132)+M(156)+M(203)-M(209)+M(225) &
    -M(226)-M(239)+M(241))) * den(44)
  T3sum(1:15,8) = T3sum(1:15,8) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(5)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(6)*(-M(135)+M(159)-M(197)+M(199)+M(204) &
    -M(210)+M(219)-M(220))) * den(44)
  T3sum(1:15,8) = T3sum(1:15,8) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(6)*(M(132)-M(135)-M(156)+M(159)-M(197)+M(199)-M(203)+M(204)+M(209)-M(210)+M(219)-M(220)-M(225)+M(226)+M(239) &
    -M(241))) * den(44)
  T3sum(1:15,8) = T3sum(1:15,8) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(6)*(-M(156)+M(162)+M(209)-M(211)+M(222) &
    -M(225)-M(229)+M(239))) * den(51)
  T3sum(1:15,67) = T3sum(1:15,67) + Gcoeff * G2tensor(:,22)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(6)*(-M(141)-M(173)+M(175)+M(183)+M(206) &
    +M(213)-M(214)-M(216))) * den(51)
  T3sum(1:15,67) = T3sum(1:15,67) + Gcoeff * G2tensor(:,23)
  Gcoeff = (c(6)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(51)
  T3sum(1:15,67) = T3sum(1:15,67) + Gcoeff * G2tensor(:,24)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(27)+M(28)+M(33)-M(34)-M(57)+M(69)+M(81)-M(84))+c(6)*(-M(132)+M(138)+M(203)-M(205)+M(224) &
    -M(226)-M(235)+M(241))) * den(72)
  T3sum(1:15,69) = T3sum(1:15,69) + Gcoeff * G2tensor(:,25)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(27)+M(28)+M(33)-M(34)-M(57)+M(69)+M(81)-M(84))+c(6)*(-M(149)+M(151)-M(165)+M(189)+M(207) &
    -M(208)+M(212)-M(218))) * den(72)
  T3sum(1:15,69) = T3sum(1:15,69) + Gcoeff * G2tensor(:,26)
  Gcoeff = (c(6)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(72)
  T3sum(1:15,69) = T3sum(1:15,69) + Gcoeff * G2tensor(:,27)
  Gcoeff = (c(5)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(6)*(-M(159)+M(173)-M(183)+M(197)+M(210) &
    -M(213)+M(216)-M(219))) * den(54)
  T3sum(1:15,67) = T3sum(1:15,67) + Gcoeff * G2tensor(:,28)
  Gcoeff = (c(5)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(6)*(-M(151)-M(162)+M(165)+M(208)+M(211) &
    -M(212)-M(222)+M(229))) * den(54)
  T3sum(1:15,67) = T3sum(1:15,67) + Gcoeff * G2tensor(:,29)
  Gcoeff = (c(6)*(-M(151)+M(159)-M(162)+M(165)-M(173)+M(183)-M(197)+M(208)-M(210)+M(211)-M(212)+M(213)-M(216)+M(219)-M(222) &
    +M(229))) * den(54)
  T3sum(1:15,67) = T3sum(1:15,67) + Gcoeff * G2tensor(:,30)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(6)*(-M(135)+M(149)-M(189)+M(199) &
    +M(204)-M(207)+M(218)-M(220))) * den(75)
  T3sum(1:15,69) = T3sum(1:15,69) + Gcoeff * G2tensor(:,31)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(6)*(-M(138)+M(141)-M(175)+M(205) &
    -M(206)+M(214)-M(224)+M(235))) * den(75)
  T3sum(1:15,69) = T3sum(1:15,69) + Gcoeff * G2tensor(:,32)
  Gcoeff = (c(6)*(M(135)-M(138)+M(141)-M(149)-M(175)+M(189)-M(199)-M(204)+M(205)-M(206)+M(207)+M(214)-M(218)+M(220)-M(224) &
    +M(235))) * den(75)
  T3sum(1:15,69) = T3sum(1:15,69) + Gcoeff * G2tensor(:,33)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(11)-M(12)+M(19)-M(20)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(42)+M(45)+M(52) &
    -M(54)-M(57)-M(64)-M(66)+M(69)+M(76)-M(77)-M(78)-M(79)+M(80)+M(81)+M(82)+M(83)-M(84)-M(85)+M(86)-M(87)+M(88)-M(91)+M(94) &
    -M(97))+c(6)*(M(141)-M(175)-M(206)+M(214))) * den(43)
  T4sum(1:35,51) = T4sum(1:35,51) + Gcoeff * G3tensor(:,7)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(42)+M(45)+M(52) &
    -M(54)-M(57)-M(64)-M(66)+M(69)-M(76)-M(77)-M(78)-M(79)+M(80)+M(81)+M(82)+M(83)-M(84)+M(85)+M(86)-M(87)-M(88)-M(91)+M(94) &
    +M(97))+c(6)*(M(151)-M(165)-M(208)+M(212))) * den(43)
  T4sum(1:35,51) = T4sum(1:35,51) + Gcoeff * G3tensor(:,8)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(76)+M(85)-M(88)+M(97))+c(6)*(-M(141)+M(151)-M(165)+M(175) &
    +M(206)-M(208)+M(212)-M(214))) * den(43)
  T4sum(1:35,51) = T4sum(1:35,51) + Gcoeff * G3tensor(:,9)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(6)*(-M(138)+M(180)+M(205)-M(215)+M(223) &
    -M(224)-M(233)+M(235))) * den(51)
  T3sum(1:15,66) = T3sum(1:15,66) + Gcoeff * G2tensor(:,16)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(6)*(-M(156)+M(162)+M(209)-M(211)+M(222) &
    -M(225)-M(229)+M(239))) * den(51)
  T3sum(1:15,66) = T3sum(1:15,66) + Gcoeff * G2tensor(:,17)
  Gcoeff = (c(6)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(51)
  T3sum(1:15,66) = T3sum(1:15,66) + Gcoeff * G2tensor(:,18)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(6)*(-M(138)+M(180)+M(205)-M(215)+M(223) &
    -M(224)-M(233)+M(235))) * den(51)
  T3sum(1:15,12) = T3sum(1:15,12) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(6)*(-M(141)-M(173)+M(175)+M(183)+M(206) &
    +M(213)-M(214)-M(216))) * den(51)
  T3sum(1:15,12) = T3sum(1:15,12) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(6)*(M(138)-M(141)-M(173)+M(175)-M(180)+M(183)-M(205)+M(206)+M(213)-M(214)+M(215)-M(216)-M(223)+M(224)+M(233) &
    -M(235))) * den(51)
  T3sum(1:15,12) = T3sum(1:15,12) + Gcoeff * G2tensor(:,6)
  Gcoeff = (c(5)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(6)*(-M(159)+M(173)-M(183)+M(197)+M(210) &
    -M(213)+M(216)-M(219))) * den(54)
  T3sum(1:15,61) = T3sum(1:15,61) + Gcoeff * G2tensor(:,34)
  Gcoeff = (c(5)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(6)*(-M(149)-M(186)+M(189)+M(207)+M(217) &
    -M(218)-M(221)+M(227))) * den(54)
  T3sum(1:15,61) = T3sum(1:15,61) + Gcoeff * G2tensor(:,35)
  Gcoeff = (c(6)*(-M(149)+M(159)-M(173)+M(183)-M(186)+M(189)-M(197)+M(207)-M(210)+M(213)-M(216)+M(217)-M(218)+M(219)-M(221) &
    +M(227))) * den(54)
  T3sum(1:15,61) = T3sum(1:15,61) + Gcoeff * G2tensor(:,36)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(76)+M(85)-M(88)+M(97))+c(6)*(-M(141)+M(151)-M(165)+M(175) &
    +M(206)-M(208)+M(212)-M(214))) * den(86)
  T3sum(1:15,69) = T3sum(1:15,69) + Gcoeff * G2tensor(:,37)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(76)+M(85)-M(88)+M(97))+c(6)*(-M(132)+M(135)-M(199)+M(203) &
    -M(204)+M(220)-M(226)+M(241))) * den(86)
  T3sum(1:15,69) = T3sum(1:15,69) + Gcoeff * G2tensor(:,38)
  Gcoeff = (c(6)*(-M(132)+M(135)+M(141)-M(151)+M(165)-M(175)-M(199)+M(203)-M(204)-M(206)+M(208)-M(212)+M(214)+M(220)-M(226) &
    +M(241))) * den(86)
  T3sum(1:15,69) = T3sum(1:15,69) + Gcoeff * G2tensor(:,39)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)-M(11)+M(12)-M(19)+M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(42)+M(45)+M(52) &
    -M(54)+M(57)+M(64)-M(66)-M(69)-M(76)-M(77)-M(78)-M(79)+M(80)-M(81)-M(82)+M(83)+M(84)+M(85)+M(86)+M(87)-M(88)-M(91)-M(94) &
    +M(97))+c(6)*(M(135)-M(199)-M(204)+M(220))) * den(43)
  T4sum(1:35,66) = T4sum(1:35,66) + Gcoeff * G3tensor(:,10)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(5)+M(6)+M(7)-M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)+M(42)+M(45)+M(52) &
    -M(54)+M(57)-M(64)-M(66)-M(69)-M(76)-M(77)-M(78)-M(79)+M(80)-M(81)+M(82)+M(83)+M(84)+M(85)+M(86)-M(87)-M(88)-M(91)+M(94) &
    +M(97))+c(6)*(M(149)-M(189)-M(207)+M(218))) * den(43)
  T4sum(1:35,66) = T4sum(1:35,66) + Gcoeff * G3tensor(:,11)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(6)*(-M(135)+M(149)-M(189)+M(199) &
    +M(204)-M(207)+M(218)-M(220))) * den(43)
  T4sum(1:35,66) = T4sum(1:35,66) + Gcoeff * G3tensor(:,12)
  Gcoeff = (c(5)*(-M(9)+M(10)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)-M(73)+M(85)+M(97)-M(100))+c(6)*(-M(131)+M(133)-M(193)+M(217) &
    +M(227)-M(228)+M(244)-M(250))) * den(88)
  T3sum(1:15,70) = T3sum(1:15,70) + Gcoeff * G2tensor(:,92)
  Gcoeff = (c(5)*(-M(9)+M(10)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)-M(73)+M(85)+M(97)-M(100))+c(6)*(-M(139)+M(145)-M(163)+M(169) &
    +M(230)-M(232)+M(236)-M(238))) * den(88)
  T3sum(1:15,70) = T3sum(1:15,70) + Gcoeff * G2tensor(:,95)
  Gcoeff = (c(6)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(88)
  T3sum(1:15,70) = T3sum(1:15,70) + Gcoeff * G2tensor(:,98)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(110)+M(116)+M(122)-M(124))+c(6)*(M(152)-M(154)+M(163)-M(164) &
    -M(169)+M(170)-M(230)+M(232))) * den(119)
  T3sum(1:15,50) = T3sum(1:15,50) + Gcoeff * G2tensor(:,93)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(110)+M(116)+M(122)-M(124))+c(6)*(M(149)-M(150)+M(193)-M(194) &
    -M(217)+M(218)-M(227)+M(228))) * den(119)
  T3sum(1:15,50) = T3sum(1:15,50) + Gcoeff * G2tensor(:,96)
  Gcoeff = (c(6)*(M(149)-M(150)-M(152)+M(154)-M(163)+M(164)+M(169)-M(170)+M(193)-M(194)-M(217)+M(218)-M(227)+M(228)+M(230) &
    -M(232))) * den(119)
  T3sum(1:15,50) = T3sum(1:15,50) + Gcoeff * G2tensor(:,99)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(37)+M(38)+M(39)-M(40)-M(45)+M(48)+M(57) &
    -M(60)+M(71)-M(72)+M(73)-M(83)+M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(163)-M(169)-M(230)+M(232))) * den(20)
  T4sum(1:35,183) = T4sum(1:35,183) + Gcoeff * G3tensor(:,55)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(37)+M(38)+M(39)-M(40)-M(45)+M(48)-M(57) &
    +M(60)+M(71)+M(72)+M(73)-M(83)-M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)-M(110)-M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(193)-M(217)-M(227)+M(228))) * den(20)
  T4sum(1:35,183) = T4sum(1:35,183) + Gcoeff * G3tensor(:,56)
  Gcoeff = (c(5)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(163)+M(169)+M(193) &
    -M(217)-M(227)+M(228)+M(230)-M(232))) * den(20)
  T4sum(1:35,183) = T4sum(1:35,183) + Gcoeff * G3tensor(:,57)
  Gcoeff = (c(5)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(6)*(-M(151)-M(162)+M(165)+M(208)+M(211) &
    -M(212)-M(222)+M(229))) * den(54)
  T3sum(1:15,14) = T3sum(1:15,14) + Gcoeff * G2tensor(:,40)
  Gcoeff = (c(5)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(6)*(-M(149)-M(186)+M(189)+M(207)+M(217) &
    -M(218)-M(221)+M(227))) * den(54)
  T3sum(1:15,14) = T3sum(1:15,14) + Gcoeff * G2tensor(:,41)
  Gcoeff = (c(6)*(-M(149)+M(151)+M(162)-M(165)-M(186)+M(189)+M(207)-M(208)-M(211)+M(212)+M(217)-M(218)-M(221)+M(222)+M(227) &
    -M(229))) * den(54)
  T3sum(1:15,14) = T3sum(1:15,14) + Gcoeff * G2tensor(:,42)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(5)+M(6)+M(7)-M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)+M(42)+M(45)+M(52) &
    -M(54)+M(57)-M(64)-M(66)-M(69)-M(76)-M(77)-M(78)-M(79)+M(80)-M(81)+M(82)+M(83)+M(84)+M(85)+M(86)-M(87)-M(88)-M(91)+M(94) &
    +M(97))+c(6)*(M(149)-M(189)-M(207)+M(218))) * den(43)
  T4sum(1:35,79) = T4sum(1:35,79) + Gcoeff * G3tensor(:,20)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(42)+M(45)+M(52) &
    -M(54)-M(57)-M(64)-M(66)+M(69)-M(76)-M(77)-M(78)-M(79)+M(80)+M(81)+M(82)+M(83)-M(84)+M(85)+M(86)-M(87)-M(88)-M(91)+M(94) &
    +M(97))+c(6)*(M(151)-M(165)-M(208)+M(212))) * den(43)
  T4sum(1:35,79) = T4sum(1:35,79) + Gcoeff * G3tensor(:,21)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(27)+M(28)+M(33)-M(34)-M(57)+M(69)+M(81)-M(84))+c(6)*(-M(149)+M(151)-M(165)+M(189)+M(207) &
    -M(208)+M(212)-M(218))) * den(43)
  T4sum(1:35,79) = T4sum(1:35,79) + Gcoeff * G3tensor(:,22)
  Gcoeff = (c(6)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(55)
  T3sum(1:35,69) = T3sum(1:35,69) + Gcoeff * G3tensor(:,23)
  Gcoeff = (c(6)*(-M(132)+M(135)+M(141)-M(151)+M(165)-M(175)-M(199)+M(203)-M(204)-M(206)+M(208)-M(212)+M(214)+M(220)-M(226) &
    +M(241))) * den(55)
  T3sum(1:35,69) = T3sum(1:35,69) + Gcoeff * G3tensor(:,24)
  Gcoeff = (c(6)*(-M(135)+M(138)-M(141)+M(149)+M(175)-M(189)+M(199)+M(204)-M(205)+M(206)-M(207)-M(214)+M(218)-M(220)+M(224) &
    -M(235))) * den(55)
  T3sum(1:35,69) = T3sum(1:35,69) + Gcoeff * G3tensor(:,25)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(58)
  T3sum(1:35,70) = T3sum(1:35,70) + Gcoeff * G3tensor(:,71)
  Gcoeff = (c(6)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(58)
  T3sum(1:35,70) = T3sum(1:35,70) + Gcoeff * G3tensor(:,74)
  Gcoeff = (c(6)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(58)
  T3sum(1:35,70) = T3sum(1:35,70) + Gcoeff * G3tensor(:,77)
  Gcoeff = (c(6)*(-M(149)+M(151)+M(153)-M(154)+M(163)-M(164)-M(187)+M(188)-M(211)+M(212)+M(217)-M(218)+M(227)-M(229)-M(231) &
    +M(232))) * den(59)
  T3sum(1:35,50) = T3sum(1:35,50) + Gcoeff * G3tensor(:,72)
  Gcoeff = (c(6)*(M(149)-M(150)-M(152)+M(154)-M(163)+M(164)+M(169)-M(170)+M(193)-M(194)-M(217)+M(218)-M(227)+M(228)+M(230) &
    -M(232))) * den(59)
  T3sum(1:35,50) = T3sum(1:35,50) + Gcoeff * G3tensor(:,75)
  Gcoeff = (c(6)*(M(150)-M(151)+M(152)-M(153)-M(169)+M(170)+M(187)-M(188)-M(193)+M(194)+M(211)-M(212)-M(228)+M(229)-M(230) &
    +M(231))) * den(59)
  T3sum(1:35,50) = T3sum(1:35,50) + Gcoeff * G3tensor(:,78)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(163)-M(187)-M(211) &
    +M(217)+M(227)-M(229)-M(231)+M(232))) * den(30)
  T4sum(1:70,183) = T4sum(1:70,183) + Gcoeff * G4tensor(:,13)
  Gcoeff = (c(5)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(163)+M(169)+M(193) &
    -M(217)-M(227)+M(228)+M(230)-M(232))) * den(30)
  T4sum(1:70,183) = T4sum(1:70,183) + Gcoeff * G4tensor(:,14)
  Gcoeff = (c(5)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(6)*(-M(169)+M(187)-M(193) &
    +M(211)-M(228)+M(229)-M(230)+M(231))) * den(30)
  T4sum(1:70,183) = T4sum(1:70,183) + Gcoeff * G4tensor(:,15)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(37)+M(38)+M(39)-M(40)+M(57)-M(60)-M(72)+M(84))+c(6)*(-M(140)+M(146)+M(155)-M(157) &
    +M(176)-M(178)-M(243)+M(249))) * den(60)
  T3sum(1:15,75) = T3sum(1:15,75) + Gcoeff * G2tensor(:,100)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(37)+M(38)+M(39)-M(40)+M(57)-M(60)-M(72)+M(84))+c(6)*(M(163)-M(169)+M(173)-M(174)-M(192) &
    +M(216)-M(230)+M(232))) * den(60)
  T3sum(1:15,75) = T3sum(1:15,75) + Gcoeff * G2tensor(:,103)
  Gcoeff = (c(6)*(M(140)-M(146)-M(155)+M(157)+M(163)-M(169)+M(173)-M(174)-M(176)+M(178)-M(192)+M(216)-M(230)+M(232)+M(243) &
    -M(249))) * den(60)
  T3sum(1:15,75) = T3sum(1:15,75) + Gcoeff * G2tensor(:,106)
  Gcoeff = (c(6)*(-M(140)+M(155)-M(161)-M(167)+M(169)-M(173)+M(175)+M(177)-M(178)+M(182)+M(206)-M(216)+M(230)-M(240)-M(246) &
    +M(249))) * den(68)
  T3sum(1:35,75) = T3sum(1:35,75) + Gcoeff * G3tensor(:,61)
  Gcoeff = (c(6)*(M(140)-M(146)-M(155)+M(157)+M(163)-M(169)+M(173)-M(174)-M(176)+M(178)-M(192)+M(216)-M(230)+M(232)+M(243) &
    -M(249))) * den(68)
  T3sum(1:35,75) = T3sum(1:35,75) + Gcoeff * G3tensor(:,64)
  Gcoeff = (c(6)*(M(146)-M(157)+M(161)-M(163)+M(167)+M(174)-M(175)+M(176)-M(177)-M(182)+M(192)-M(206)-M(232)+M(240)-M(243) &
    +M(246))) * den(68)
  T3sum(1:35,75) = T3sum(1:35,75) + Gcoeff * G3tensor(:,67)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(27)+M(28)+M(33)-M(34)-M(57)+M(69)+M(81)-M(84))+c(6)*(-M(162)+M(186)+M(211)-M(217)+M(221) &
    -M(222)-M(227)+M(229))) * den(72)
  T3sum(1:15,66) = T3sum(1:15,66) + Gcoeff * G2tensor(:,19)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(27)+M(28)+M(33)-M(34)-M(57)+M(69)+M(81)-M(84))+c(6)*(-M(132)+M(138)+M(203)-M(205)+M(224) &
    -M(226)-M(235)+M(241))) * den(72)
  T3sum(1:15,66) = T3sum(1:15,66) + Gcoeff * G2tensor(:,20)
  Gcoeff = (c(6)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(72)
  T3sum(1:15,66) = T3sum(1:15,66) + Gcoeff * G2tensor(:,21)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(27)+M(28)+M(33)-M(34)-M(57)+M(69)+M(81)-M(84))+c(6)*(-M(162)+M(186)+M(211)-M(217)+M(221) &
    -M(222)-M(227)+M(229))) * den(72)
  T3sum(1:15,14) = T3sum(1:15,14) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(27)+M(28)+M(33)-M(34)-M(57)+M(69)+M(81)-M(84))+c(6)*(-M(149)+M(151)-M(165)+M(189)+M(207) &
    -M(208)+M(212)-M(218))) * den(72)
  T3sum(1:15,14) = T3sum(1:15,14) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(6)*(-M(149)+M(151)+M(162)-M(165)-M(186)+M(189)+M(207)-M(208)-M(211)+M(212)+M(217)-M(218)-M(221)+M(222)+M(227) &
    -M(229))) * den(72)
  T3sum(1:15,14) = T3sum(1:15,14) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(6)*(-M(135)+M(149)-M(189)+M(199) &
    +M(204)-M(207)+M(218)-M(220))) * den(75)
  T3sum(1:15,61) = T3sum(1:15,61) + Gcoeff * G2tensor(:,43)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(6)*(-M(173)-M(180)+M(183)+M(213) &
    +M(215)-M(216)-M(223)+M(233))) * den(75)
  T3sum(1:15,61) = T3sum(1:15,61) + Gcoeff * G2tensor(:,44)
  Gcoeff = (c(6)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(75)
  T3sum(1:15,61) = T3sum(1:15,61) + Gcoeff * G2tensor(:,45)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(76)+M(85)-M(88)+M(97))+c(6)*(-M(141)+M(151)-M(165)+M(175) &
    +M(206)-M(208)+M(212)-M(214))) * den(86)
  T3sum(1:15,67) = T3sum(1:15,67) + Gcoeff * G2tensor(:,46)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(76)+M(85)-M(88)+M(97))+c(6)*(-M(156)+M(159)-M(197)+M(209) &
    -M(210)+M(219)-M(225)+M(239))) * den(86)
  T3sum(1:15,67) = T3sum(1:15,67) + Gcoeff * G2tensor(:,47)
  Gcoeff = (c(6)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(86)
  T3sum(1:15,67) = T3sum(1:15,67) + Gcoeff * G2tensor(:,48)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(42)+M(45)+M(52) &
    +M(54)+M(57)+M(64)-M(66)-M(69)-M(76)+M(77)-M(78)-M(79)-M(80)-M(81)-M(82)+M(83)+M(84)+M(85)+M(86)+M(87)-M(88)-M(91)-M(94) &
    +M(97))+c(6)*(M(159)-M(197)-M(210)+M(219))) * den(43)
  T4sum(1:35,102) = T4sum(1:35,102) + Gcoeff * G3tensor(:,13)
  Gcoeff = (c(4)*(M(1)-M(2)+M(5)-M(6)-M(7)+M(8)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)-M(42)+M(45)-M(52) &
    +M(54)+M(57)+M(64)-M(66)-M(69)-M(76)+M(77)-M(78)+M(79)-M(80)-M(81)-M(82)+M(83)+M(84)+M(85)-M(86)+M(87)-M(88)+M(91)-M(94) &
    +M(97))+c(6)*(M(173)-M(183)-M(213)+M(216))) * den(43)
  T4sum(1:35,102) = T4sum(1:35,102) + Gcoeff * G3tensor(:,14)
  Gcoeff = (c(5)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(6)*(-M(159)+M(173)-M(183)+M(197)+M(210) &
    -M(213)+M(216)-M(219))) * den(43)
  T4sum(1:35,102) = T4sum(1:35,102) + Gcoeff * G3tensor(:,15)
  Gcoeff = (c(5)*(-M(9)+M(10)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)-M(73)+M(85)+M(97)-M(100))+c(6)*(-M(155)+M(157)-M(191)+M(215) &
    +M(233)-M(234)+M(243)-M(249))) * den(88)
  T3sum(1:15,68) = T3sum(1:15,68) + Gcoeff * G2tensor(:,101)
  Gcoeff = (c(5)*(-M(9)+M(10)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)-M(73)+M(85)+M(97)-M(100))+c(6)*(-M(139)+M(145)-M(163)+M(169) &
    +M(230)-M(232)+M(236)-M(238))) * den(88)
  T3sum(1:15,68) = T3sum(1:15,68) + Gcoeff * G2tensor(:,104)
  Gcoeff = (c(6)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(88)
  T3sum(1:15,68) = T3sum(1:15,68) + Gcoeff * G2tensor(:,107)
  Gcoeff = (c(5)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(125)+M(127)+M(129)-M(130))+c(6)*(M(139)-M(140)-M(145)+M(146) &
    +M(176)-M(178)-M(236)+M(238))) * den(131)
  T3sum(1:15,37) = T3sum(1:15,37) + Gcoeff * G2tensor(:,102)
  Gcoeff = (c(5)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(125)+M(127)+M(129)-M(130))+c(6)*(M(173)-M(174)+M(191)-M(192) &
    -M(215)+M(216)-M(233)+M(234))) * den(131)
  T3sum(1:15,37) = T3sum(1:15,37) + Gcoeff * G2tensor(:,105)
  Gcoeff = (c(6)*(-M(139)+M(140)+M(145)-M(146)+M(173)-M(174)-M(176)+M(178)+M(191)-M(192)-M(215)+M(216)-M(233)+M(234)+M(236) &
    -M(238))) * den(131)
  T3sum(1:15,37) = T3sum(1:15,37) + Gcoeff * G2tensor(:,108)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(57) &
    +M(60)-M(71)+M(72)+M(73)+M(83)-M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(139)-M(145)-M(236)+M(238))) * den(20)
  T4sum(1:35,177) = T4sum(1:35,177) + Gcoeff * G3tensor(:,58)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)-M(45)+M(48)-M(57) &
    +M(60)+M(71)+M(72)+M(73)-M(83)-M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(191)-M(215)-M(233)+M(234))) * den(20)
  T4sum(1:35,177) = T4sum(1:35,177) + Gcoeff * G3tensor(:,59)
  Gcoeff = (c(5)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(6)*(-M(139)+M(145)+M(191) &
    -M(215)-M(233)+M(234)+M(236)-M(238))) * den(20)
  T4sum(1:35,177) = T4sum(1:35,177) + Gcoeff * G3tensor(:,60)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(6)*(-M(138)+M(141)-M(175)+M(205) &
    -M(206)+M(214)-M(224)+M(235))) * den(75)
  T3sum(1:15,12) = T3sum(1:15,12) + Gcoeff * G2tensor(:,49)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(6)*(-M(173)-M(180)+M(183)+M(213) &
    +M(215)-M(216)-M(223)+M(233))) * den(75)
  T3sum(1:15,12) = T3sum(1:15,12) + Gcoeff * G2tensor(:,50)
  Gcoeff = (c(6)*(M(138)-M(141)-M(173)+M(175)-M(180)+M(183)-M(205)+M(206)+M(213)-M(214)+M(215)-M(216)-M(223)+M(224)+M(233) &
    -M(235))) * den(75)
  T3sum(1:15,12) = T3sum(1:15,12) + Gcoeff * G2tensor(:,51)
  Gcoeff = (c(4)*(M(1)-M(2)+M(5)-M(6)-M(7)+M(8)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)-M(42)+M(45)-M(52) &
    +M(54)+M(57)+M(64)-M(66)-M(69)-M(76)+M(77)-M(78)+M(79)-M(80)-M(81)-M(82)+M(83)+M(84)+M(85)-M(86)+M(87)-M(88)+M(91)-M(94) &
    +M(97))+c(6)*(M(173)-M(183)-M(213)+M(216))) * den(43)
  T4sum(1:35,115) = T4sum(1:35,115) + Gcoeff * G3tensor(:,27)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)-M(11)+M(12)-M(19)+M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(42)-M(45)-M(52) &
    +M(54)+M(57)+M(64)+M(66)-M(69)-M(76)+M(77)+M(78)+M(79)-M(80)-M(81)-M(82)-M(83)+M(84)+M(85)-M(86)+M(87)-M(88)+M(91)-M(94) &
    +M(97))+c(6)*(-M(141)+M(175)+M(206)-M(214))) * den(43)
  T4sum(1:35,115) = T4sum(1:35,115) + Gcoeff * G3tensor(:,28)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(6)*(-M(141)-M(173)+M(175)+M(183)+M(206) &
    +M(213)-M(214)-M(216))) * den(43)
  T4sum(1:35,115) = T4sum(1:35,115) + Gcoeff * G3tensor(:,29)
  Gcoeff = (c(6)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(76)
  T3sum(1:35,67) = T3sum(1:35,67) + Gcoeff * G3tensor(:,30)
  Gcoeff = (c(6)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(76)
  T3sum(1:35,67) = T3sum(1:35,67) + Gcoeff * G3tensor(:,31)
  Gcoeff = (c(6)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(76)
  T3sum(1:35,67) = T3sum(1:35,67) + Gcoeff * G3tensor(:,32)
  Gcoeff = (c(6)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(79)
  T3sum(1:35,68) = T3sum(1:35,68) + Gcoeff * G3tensor(:,62)
  Gcoeff = (c(6)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(79)
  T3sum(1:35,68) = T3sum(1:35,68) + Gcoeff * G3tensor(:,65)
  Gcoeff = (c(6)*(-M(145)+M(157)-M(161)+M(163)-M(167)+M(181)-M(191)+M(205)+M(232)-M(234)+M(235)-M(236)+M(237)-M(240)+M(243) &
    -M(246))) * den(79)
  T3sum(1:35,68) = T3sum(1:35,68) + Gcoeff * G3tensor(:,68)
  Gcoeff = (c(6)*(M(139)-M(140)-M(173)+M(175)+M(177)-M(178)-M(181)+M(182)-M(205)+M(206)+M(215)-M(216)+M(233)-M(235)-M(237) &
    +M(238))) * den(80)
  T3sum(1:35,37) = T3sum(1:35,37) + Gcoeff * G3tensor(:,63)
  Gcoeff = (c(6)*(-M(139)+M(140)+M(145)-M(146)+M(173)-M(174)-M(176)+M(178)+M(191)-M(192)-M(215)+M(216)-M(233)+M(234)+M(236) &
    -M(238))) * den(80)
  T3sum(1:35,37) = T3sum(1:35,37) + Gcoeff * G3tensor(:,66)
  Gcoeff = (c(6)*(-M(145)+M(146)+M(174)-M(175)+M(176)-M(177)+M(181)-M(182)-M(191)+M(192)+M(205)-M(206)-M(234)+M(235)-M(236) &
    +M(237))) * den(80)
  T3sum(1:35,37) = T3sum(1:35,37) + Gcoeff * G3tensor(:,69)
  Gcoeff = (c(5)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(6)*(M(139)-M(181)-M(205) &
    +M(215)+M(233)-M(235)-M(237)+M(238))) * den(67)
  T4sum(1:70,177) = T4sum(1:70,177) + Gcoeff * G4tensor(:,10)
  Gcoeff = (c(5)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(6)*(-M(139)+M(145)+M(191) &
    -M(215)-M(233)+M(234)+M(236)-M(238))) * den(67)
  T4sum(1:70,177) = T4sum(1:70,177) + Gcoeff * G4tensor(:,11)
  Gcoeff = (c(5)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(6)*(-M(145)+M(181)-M(191) &
    +M(205)-M(234)+M(235)-M(236)+M(237))) * den(67)
  T4sum(1:70,177) = T4sum(1:70,177) + Gcoeff * G4tensor(:,12)
  Gcoeff = (c(6)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(85)
  T3sum(1:35,64) = T3sum(1:35,64) + Gcoeff * G3tensor(:,46)
  Gcoeff = (c(6)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(85)
  T3sum(1:35,64) = T3sum(1:35,64) + Gcoeff * G3tensor(:,49)
  Gcoeff = (c(6)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(85)
  T3sum(1:35,64) = T3sum(1:35,64) + Gcoeff * G3tensor(:,52)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(76)+M(85)-M(88)+M(97))+c(6)*(-M(132)+M(135)-M(199)+M(203) &
    -M(204)+M(220)-M(226)+M(241))) * den(86)
  T3sum(1:15,8) = T3sum(1:15,8) + Gcoeff * G2tensor(:,52)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(76)+M(85)-M(88)+M(97))+c(6)*(-M(156)+M(159)-M(197)+M(209) &
    -M(210)+M(219)-M(225)+M(239))) * den(86)
  T3sum(1:15,8) = T3sum(1:15,8) + Gcoeff * G2tensor(:,53)
  Gcoeff = (c(6)*(M(132)-M(135)-M(156)+M(159)-M(197)+M(199)-M(203)+M(204)+M(209)-M(210)+M(219)-M(220)-M(225)+M(226)+M(239) &
    -M(241))) * den(86)
  T3sum(1:15,8) = T3sum(1:15,8) + Gcoeff * G2tensor(:,54)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)-M(11)+M(12)-M(19)+M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(42)+M(45)+M(52) &
    -M(54)+M(57)+M(64)-M(66)-M(69)-M(76)-M(77)-M(78)-M(79)+M(80)-M(81)-M(82)+M(83)+M(84)+M(85)+M(86)+M(87)-M(88)-M(91)-M(94) &
    +M(97))+c(6)*(M(135)-M(199)-M(204)+M(220))) * den(43)
  T4sum(1:35,130) = T4sum(1:35,130) + Gcoeff * G3tensor(:,16)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(42)+M(45)+M(52) &
    +M(54)+M(57)+M(64)-M(66)-M(69)-M(76)+M(77)-M(78)-M(79)-M(80)-M(81)-M(82)+M(83)+M(84)+M(85)+M(86)+M(87)-M(88)-M(91)-M(94) &
    +M(97))+c(6)*(M(159)-M(197)-M(210)+M(219))) * den(43)
  T4sum(1:35,130) = T4sum(1:35,130) + Gcoeff * G3tensor(:,17)
  Gcoeff = (c(5)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(6)*(-M(135)+M(159)-M(197)+M(199)+M(204) &
    -M(210)+M(219)-M(220))) * den(43)
  T4sum(1:35,130) = T4sum(1:35,130) + Gcoeff * G3tensor(:,18)
  Gcoeff = (c(6)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(87)
  T3sum(1:35,61) = T3sum(1:35,61) + Gcoeff * G3tensor(:,34)
  Gcoeff = (c(6)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(87)
  T3sum(1:35,61) = T3sum(1:35,61) + Gcoeff * G3tensor(:,35)
  Gcoeff = (c(6)*(M(149)-M(159)+M(173)-M(183)+M(186)-M(189)+M(197)-M(207)+M(210)-M(213)+M(216)-M(217)+M(218)-M(219)+M(221) &
    -M(227))) * den(87)
  T3sum(1:35,61) = T3sum(1:35,61) + Gcoeff * G3tensor(:,36)
  Gcoeff = (c(6)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(90)
  T3sum(1:35,62) = T3sum(1:35,62) + Gcoeff * G3tensor(:,47)
  Gcoeff = (c(6)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(90)
  T3sum(1:35,62) = T3sum(1:35,62) + Gcoeff * G3tensor(:,50)
  Gcoeff = (c(6)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(90)
  T3sum(1:35,62) = T3sum(1:35,62) + Gcoeff * G3tensor(:,53)
  Gcoeff = (c(6)*(M(133)-M(134)-M(157)+M(158)-M(197)+M(199)+M(201)-M(202)-M(203)+M(204)+M(209)-M(210)+M(239)-M(241)-M(243) &
    +M(244))) * den(91)
  T3sum(1:35,32) = T3sum(1:35,32) + Gcoeff * G3tensor(:,48)
  Gcoeff = (c(6)*(-M(133)+M(134)+M(143)-M(144)+M(167)-M(168)+M(197)-M(198)-M(200)+M(202)-M(209)+M(210)-M(239)+M(240)+M(242) &
    -M(244))) * den(91)
  T3sum(1:35,32) = T3sum(1:35,32) + Gcoeff * G3tensor(:,51)
  Gcoeff = (c(6)*(-M(143)+M(144)+M(157)-M(158)-M(167)+M(168)+M(198)-M(199)+M(200)-M(201)+M(203)-M(204)-M(240)+M(241)-M(242) &
    +M(243))) * den(91)
  T3sum(1:35,32) = T3sum(1:35,32) + Gcoeff * G3tensor(:,54)
  Gcoeff = (c(5)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(6)*(M(133)-M(157)-M(203) &
    +M(209)+M(239)-M(241)-M(243)+M(244))) * den(84)
  T4sum(1:70,162) = T4sum(1:70,162) + Gcoeff * G4tensor(:,7)
  Gcoeff = (c(5)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(6)*(-M(133)+M(143)+M(167) &
    -M(209)-M(239)+M(240)+M(242)-M(244))) * den(84)
  T4sum(1:70,162) = T4sum(1:70,162) + Gcoeff * G4tensor(:,8)
  Gcoeff = (c(5)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(6)*(-M(143)+M(157)-M(167) &
    +M(203)-M(240)+M(241)-M(242)+M(243))) * den(84)
  T4sum(1:70,162) = T4sum(1:70,162) + Gcoeff * G4tensor(:,9)
  Gcoeff = (c(6)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(93)
  T3sum(1:35,66) = T3sum(1:35,66) + Gcoeff * G3tensor(:,1)
  Gcoeff = (c(6)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(93)
  T3sum(1:35,66) = T3sum(1:35,66) + Gcoeff * G3tensor(:,2)
  Gcoeff = (c(6)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(93)
  T3sum(1:35,66) = T3sum(1:35,66) + Gcoeff * G3tensor(:,3)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)+M(31)-M(32)-M(35)+M(36)+M(110)-M(112)-M(118)+M(124))+c(6)*(M(140)-M(142)+M(171)-M(172) &
    -M(177)+M(178)-M(182)+M(184))) * den(114)
  T3sum(1:15,18) = T3sum(1:15,18) + Gcoeff * G2tensor(:,181)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)+M(31)-M(32)-M(35)+M(36)+M(110)-M(112)-M(118)+M(124))+c(6)*(M(137)-M(138)-M(179)+M(180) &
    +M(223)-M(224)-M(247)+M(248))) * den(114)
  T3sum(1:15,18) = T3sum(1:15,18) + Gcoeff * G2tensor(:,182)
  Gcoeff = (c(6)*(M(137)-M(138)-M(140)+M(142)-M(171)+M(172)+M(177)-M(178)-M(179)+M(180)+M(182)-M(184)+M(223)-M(224)-M(247) &
    +M(248))) * den(114)
  T3sum(1:15,18) = T3sum(1:15,18) + Gcoeff * G2tensor(:,183)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)-M(110)+M(111)+M(112)-M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(142)-M(171)+M(172)-M(184))) * den(33)
  T4sum(1:35,143) = T4sum(1:35,143) + Gcoeff * G3tensor(:,79)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)-M(44)-M(45)-M(46) &
    +M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95)+M(103)+M(105)-M(107)-M(108)+M(109)-M(110)+M(111)+M(112)-M(113)+M(118) &
    -M(119)-M(124))+c(6)*(-M(137)+M(179)+M(247)-M(248))) * den(33)
  T4sum(1:35,143) = T4sum(1:35,143) + Gcoeff * G3tensor(:,81)
  Gcoeff = (c(5)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(6)*(-M(137)-M(142)+M(171)-M(172) &
    +M(179)+M(184)+M(247)-M(248))) * den(33)
  T4sum(1:35,143) = T4sum(1:35,143) + Gcoeff * G3tensor(:,83)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)+M(31)-M(32)-M(35)+M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)+M(110)+M(111)-M(112)-M(113)-M(118) &
    -M(119)+M(124))+c(6)*(M(140)-M(177)+M(178)-M(182))) * den(33)
  T4sum(1:35,144) = T4sum(1:35,144) + Gcoeff * G3tensor(:,80)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)+M(31)-M(32)-M(35)+M(36)-M(44)-M(45)-M(46) &
    +M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95)+M(103)+M(105)-M(107)-M(108)+M(109)+M(110)+M(111)-M(112)-M(113)-M(118) &
    -M(119)+M(124))+c(6)*(-M(138)+M(180)+M(223)-M(224))) * den(33)
  T4sum(1:35,144) = T4sum(1:35,144) + Gcoeff * G3tensor(:,82)
  Gcoeff = (c(5)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(6)*(-M(138)-M(140)+M(177)-M(178) &
    +M(180)+M(182)+M(223)-M(224))) * den(33)
  T4sum(1:35,144) = T4sum(1:35,144) + Gcoeff * G3tensor(:,84)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(147)-M(148)-M(153)+M(154) &
    +M(164)-M(166)-M(188)+M(190))) * den(126)
  T3sum(1:15,30) = T3sum(1:15,30) + Gcoeff * G2tensor(:,184)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(161)-M(162)-M(185)+M(186) &
    +M(221)-M(222)-M(245)+M(246))) * den(126)
  T3sum(1:15,30) = T3sum(1:15,30) + Gcoeff * G2tensor(:,185)
  Gcoeff = (c(6)*(-M(147)+M(148)+M(153)-M(154)+M(161)-M(162)-M(164)+M(166)-M(185)+M(186)+M(188)-M(190)+M(221)-M(222)-M(245) &
    +M(246))) * den(126)
  T3sum(1:15,30) = T3sum(1:15,30) + Gcoeff * G2tensor(:,186)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    +M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(147)+M(148)+M(166)-M(190))) * den(12)
  T4sum(1:35,155) = T4sum(1:35,155) + Gcoeff * G3tensor(:,85)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(33)-M(34)+M(41)-M(44)-M(56) &
    -M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(161)+M(185)+M(245)-M(246))) * den(12)
  T4sum(1:35,155) = T4sum(1:35,155) + Gcoeff * G3tensor(:,87)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)-M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96))+c(6)*(M(147)-M(148)-M(161)-M(166) &
    +M(185)+M(190)+M(245)-M(246))) * den(12)
  T4sum(1:35,155) = T4sum(1:35,155) + Gcoeff * G3tensor(:,89)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    +M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(153)+M(154)+M(164)-M(188))) * den(12)
  T4sum(1:35,156) = T4sum(1:35,156) + Gcoeff * G3tensor(:,86)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(33)-M(34)+M(41)-M(44)-M(56) &
    -M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(162)+M(186)+M(221)-M(222))) * den(12)
  T4sum(1:35,156) = T4sum(1:35,156) + Gcoeff * G3tensor(:,88)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)-M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96))+c(6)*(M(153)-M(154)-M(162)-M(164) &
    +M(186)+M(188)+M(221)-M(222))) * den(12)
  T4sum(1:35,156) = T4sum(1:35,156) + Gcoeff * G3tensor(:,90)
  Gcoeff = (c(6)*(M(132)-M(135)-M(156)+M(159)-M(197)+M(199)-M(203)+M(204)+M(209)-M(210)+M(219)-M(220)-M(225)+M(226)+M(239) &
    -M(241))) * den(1343)
  T3sum(1:35,8) = T3sum(1:35,8) + Gcoeff * G3tensor(:,33)
  Gcoeff = (c(6)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(198)
  T3sum(1:35,66) = T3sum(1:35,66) + Gcoeff * G3tensor(:,4)
  Gcoeff = (c(6)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(417)
  T3sum(1:35,61) = T3sum(1:35,61) + Gcoeff * G3tensor(:,37)
  Gcoeff = (c(5)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(6)*(-M(135)+M(159)-M(197)+M(199)+M(204) &
    -M(210)+M(219)-M(220))) * den(44)
  T4sum(1:70,130) = T4sum(1:70,130) + Gcoeff * G4tensor(:,2)
  Gcoeff = (c(6)*(M(138)-M(141)-M(173)+M(175)-M(180)+M(183)-M(205)+M(206)+M(213)-M(214)+M(215)-M(216)-M(223)+M(224)+M(233) &
    -M(235))) * den(1347)
  T3sum(1:35,12) = T3sum(1:35,12) + Gcoeff * G3tensor(:,26)
  Gcoeff = (c(6)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(235)
  T3sum(1:35,66) = T3sum(1:35,66) + Gcoeff * G3tensor(:,5)
  Gcoeff = (c(6)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(468)
  T3sum(1:35,67) = T3sum(1:35,67) + Gcoeff * G3tensor(:,38)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(6)*(-M(141)-M(173)+M(175)+M(183)+M(206) &
    +M(213)-M(214)-M(216))) * den(51)
  T4sum(1:70,115) = T4sum(1:70,115) + Gcoeff * G4tensor(:,5)
  Gcoeff = (c(6)*(-M(149)+M(151)+M(162)-M(165)-M(186)+M(189)+M(207)-M(208)-M(211)+M(212)+M(217)-M(218)-M(221)+M(222)+M(227) &
    -M(229))) * den(1349)
  T3sum(1:35,14) = T3sum(1:35,14) + Gcoeff * G3tensor(:,19)
  Gcoeff = (c(6)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(253)
  T3sum(1:35,66) = T3sum(1:35,66) + Gcoeff * G3tensor(:,6)
  Gcoeff = (c(6)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(590)
  T3sum(1:35,69) = T3sum(1:35,69) + Gcoeff * G3tensor(:,39)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(27)+M(28)+M(33)-M(34)-M(57)+M(69)+M(81)-M(84))+c(6)*(-M(149)+M(151)-M(165)+M(189)+M(207) &
    -M(208)+M(212)-M(218))) * den(72)
  T4sum(1:70,79) = T4sum(1:70,79) + Gcoeff * G4tensor(:,6)
  Gcoeff = (c(6)*(M(149)-M(159)+M(173)-M(183)+M(186)-M(189)+M(197)-M(207)+M(210)-M(213)+M(216)-M(217)+M(218)-M(219)+M(221) &
    -M(227))) * den(268)
  T3sum(1:35,61) = T3sum(1:35,61) + Gcoeff * G3tensor(:,40)
  Gcoeff = (c(6)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(472)
  T3sum(1:35,67) = T3sum(1:35,67) + Gcoeff * G3tensor(:,41)
  Gcoeff = (c(5)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(6)*(-M(159)+M(173)-M(183)+M(197)+M(210) &
    -M(213)+M(216)-M(219))) * den(54)
  T4sum(1:70,102) = T4sum(1:70,102) + Gcoeff * G4tensor(:,3)
  Gcoeff = (c(6)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(270)
  T3sum(1:35,61) = T3sum(1:35,61) + Gcoeff * G3tensor(:,42)
  Gcoeff = (c(6)*(-M(135)+M(138)-M(141)+M(149)+M(175)-M(189)+M(199)+M(204)-M(205)+M(206)-M(207)-M(214)+M(218)-M(220)+M(224) &
    -M(235))) * den(594)
  T3sum(1:35,69) = T3sum(1:35,69) + Gcoeff * G3tensor(:,43)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(6)*(-M(135)+M(149)-M(189)+M(199) &
    +M(204)-M(207)+M(218)-M(220))) * den(75)
  T4sum(1:70,66) = T4sum(1:70,66) + Gcoeff * G4tensor(:,4)
  Gcoeff = (c(6)*(-M(141)+M(151)+M(156)-M(159)-M(165)+M(175)+M(197)+M(206)-M(208)-M(209)+M(210)+M(212)-M(214)-M(219)+M(225) &
    -M(239))) * den(272)
  T3sum(1:35,67) = T3sum(1:35,67) + Gcoeff * G3tensor(:,44)
  Gcoeff = (c(6)*(M(132)-M(135)-M(141)+M(151)-M(165)+M(175)+M(199)-M(203)+M(204)+M(206)-M(208)+M(212)-M(214)-M(220)+M(226) &
    -M(241))) * den(596)
  T3sum(1:35,69) = T3sum(1:35,69) + Gcoeff * G3tensor(:,45)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(76)+M(85)-M(88)+M(97))+c(6)*(-M(141)+M(151)-M(165)+M(175) &
    +M(206)-M(208)+M(212)-M(214))) * den(86)
  T4sum(1:70,51) = T4sum(1:70,51) + Gcoeff * G4tensor(:,1)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(5)+M(6)+M(7)-M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)+M(42)+M(45)+M(52) &
    -M(54)+M(57)-M(64)-M(66)-M(69)-M(76)-M(77)-M(78)-M(79)+M(80)-M(81)+M(82)+M(83)+M(84)+M(85)+M(86)-M(87)-M(88)-M(91)+M(94) &
    +M(97))+c(6)*(M(149)-M(189)-M(207)+M(218))) * den(43)
  T5sum(1:126,143) = T5sum(1:126,143) + Gcoeff * G5tensor(:,2)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(42)+M(45)+M(52) &
    -M(54)-M(57)-M(64)-M(66)+M(69)-M(76)-M(77)-M(78)-M(79)+M(80)+M(81)+M(82)+M(83)-M(84)+M(85)+M(86)-M(87)-M(88)-M(91)+M(94) &
    +M(97))+c(6)*(M(151)-M(165)-M(208)+M(212))) * den(43)
  T5sum(1:126,144) = T5sum(1:126,144) + Gcoeff * G5tensor(:,1)

end subroutine vamp_16

end module ol_vamp_16_ppjjjj_gggggg_1_/**/REALKIND
