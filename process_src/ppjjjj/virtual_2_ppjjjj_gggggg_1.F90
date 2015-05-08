
module ol_vamp_2_ppjjjj_gggggg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_2(M)
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
  complex(REALKIND), dimension(4,1,4,130) :: G0
  complex(REALKIND), dimension(4,5,4,123) :: G1
  complex(REALKIND), dimension(4,15,4,24) :: G2
  complex(REALKIND), dimension(4,35,4,6) :: G3
  complex(REALKIND), dimension(1,81) :: G0tensor
  complex(REALKIND), dimension(5,162) :: G1tensor
  complex(REALKIND), dimension(15,135) :: G2tensor
  complex(REALKIND), dimension(35,18) :: G3tensor
  complex(REALKIND), dimension(70,6) :: G4tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,-2),G0(:,:,:,2))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-5),wf(:,-3),G0(:,:,:,3))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-1),wf(:,0),G0tensor(:,1))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,0),wf(:,-1),G0tensor(:,2))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-1),wf(:,0),G0tensor(:,3))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,60),wf(:,61),Q(:,3),G1tensor(:,1))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-3),wf(:,-5),G0(:,:,:,4))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-1),wf(:,0),G0tensor(:,4))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,0),wf(:,-1),G0tensor(:,5))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-1),wf(:,0),G0tensor(:,6))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,60),wf(:,61),Q(:,3),G1tensor(:,2))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-5),wf(:,-3),G0(:,:,:,5))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,5),wf(:,-1),wf(:,0),G0tensor(:,7))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,5),wf(:,0),wf(:,-1),G0tensor(:,8))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,5),wf(:,-1),wf(:,0),G0tensor(:,9))
  call check_last_UV_W(l_switch,G0(:,:,:,5),Q(:,60),wf(:,61),Q(:,3),G1tensor(:,3))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-5),wf(:,-1),G0(:,:,:,6))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,6),wf(:,-3),wf(:,0),G0tensor(:,10))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,6),wf(:,0),wf(:,-3),G0tensor(:,11))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,6),wf(:,-3),wf(:,0),G0tensor(:,12))
  call check_last_UV_W(l_switch,G0(:,:,:,6),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,4))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-1),wf(:,-5),G0(:,:,:,7))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,7),wf(:,-3),wf(:,0),G0tensor(:,13))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,7),wf(:,0),wf(:,-3),G0tensor(:,14))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,7),wf(:,-3),wf(:,0),G0tensor(:,15))
  call check_last_UV_W(l_switch,G0(:,:,:,7),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,5))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-5),wf(:,-1),G0(:,:,:,8))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,8),wf(:,-3),wf(:,0),G0tensor(:,16))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,8),wf(:,0),wf(:,-3),G0tensor(:,17))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,8),wf(:,-3),wf(:,0),G0tensor(:,18))
  call check_last_UV_W(l_switch,G0(:,:,:,8),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,6))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-5),wf(:,0),G0(:,:,:,9))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,9),wf(:,-3),wf(:,-1),G0tensor(:,19))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,9),wf(:,-1),wf(:,-3),G0tensor(:,20))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,9),wf(:,-3),wf(:,-1),G0tensor(:,21))
  call check_last_UV_W(l_switch,G0(:,:,:,9),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,7))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,0),wf(:,-5),G0(:,:,:,10))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,10),wf(:,-3),wf(:,-1),G0tensor(:,22))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,10),wf(:,-1),wf(:,-3),G0tensor(:,23))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,10),wf(:,-3),wf(:,-1),G0tensor(:,24))
  call check_last_UV_W(l_switch,G0(:,:,:,10),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,8))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-5),wf(:,0),G0(:,:,:,11))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,11),wf(:,-3),wf(:,-1),G0tensor(:,25))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,11),wf(:,-1),wf(:,-3),G0tensor(:,26))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,11),wf(:,-3),wf(:,-1),G0tensor(:,27))
  call check_last_UV_W(l_switch,G0(:,:,:,11),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,9))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-5),wf(:,61),G0(:,:,:,12))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,10))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,61),wf(:,-5),G0(:,:,:,13))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,11))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-5),wf(:,61),G0(:,:,:,14))
  call check_last_UV_W(l_switch,G0(:,:,:,14),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,12))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,-5),Q(:,32),G1(:,:,:,1))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,1),wf(:,-3),wf(:,61),G1tensor(:,13))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,1),wf(:,61),wf(:,-3),G1tensor(:,14))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,1),wf(:,-3),wf(:,61),G1tensor(:,15))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,1),wf(:,-1),wf(:,104),G1tensor(:,16))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,1),wf(:,104),wf(:,-1),G1tensor(:,17))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,1),wf(:,-1),wf(:,104),G1tensor(:,18))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,1),wf(:,0),wf(:,91),G1tensor(:,19))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,1),wf(:,91),wf(:,0),G1tensor(:,20))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,1),wf(:,0),wf(:,91),G1tensor(:,21))
  call check_last_UV_W(l_switch,G1(:,:,:,1),Q(:,52),wf(:,7),Q(:,11),G2tensor(:,1))
  call check_last_UV_W(l_switch,G1(:,:,:,1),Q(:,52),wf(:,9),Q(:,11),G2tensor(:,2))
  call check_last_UV_W(l_switch,G1(:,:,:,1),Q(:,52),wf(:,10),Q(:,11),G2tensor(:,3))
  call check_last_UV_W(l_switch,G1(:,:,:,1),Q(:,52),wf(:,83),Q(:,11),G2tensor(:,4))
  call check_last_UV_W(l_switch,G1(:,:,:,1),Q(:,52),wf(:,124),Q(:,11),G2tensor(:,5))
  call check_last_UV_W(l_switch,G1(:,:,:,1),Q(:,52),wf(:,131),Q(:,11),G2tensor(:,6))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-5),wf(:,104),G0(:,:,:,15))
  call check_last_UV_W(l_switch,G0(:,:,:,15),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,22))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,104),wf(:,-5),G0(:,:,:,16))
  call check_last_UV_W(l_switch,G0(:,:,:,16),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,23))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-5),wf(:,104),G0(:,:,:,17))
  call check_last_UV_W(l_switch,G0(:,:,:,17),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,24))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-5),wf(:,91),G0(:,:,:,18))
  call check_last_UV_W(l_switch,G0(:,:,:,18),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,25))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,91),wf(:,-5),G0(:,:,:,19))
  call check_last_UV_W(l_switch,G0(:,:,:,19),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,26))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-5),wf(:,91),G0(:,:,:,20))
  call check_last_UV_W(l_switch,G0(:,:,:,20),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,27))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-3),wf(:,113),G0(:,:,:,21))
  call check_last_UV_W(l_switch,G0(:,:,:,21),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,28))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,113),wf(:,-3),G0(:,:,:,22))
  call check_last_UV_W(l_switch,G0(:,:,:,22),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,29))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-3),wf(:,113),G0(:,:,:,23))
  call check_last_UV_W(l_switch,G0(:,:,:,23),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,30))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-3),wf(:,99),G0(:,:,:,24))
  call check_last_UV_W(l_switch,G0(:,:,:,24),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,31))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,99),wf(:,-3),G0(:,:,:,25))
  call check_last_UV_W(l_switch,G0(:,:,:,25),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,32))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-3),wf(:,99),G0(:,:,:,26))
  call check_last_UV_W(l_switch,G0(:,:,:,26),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,33))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,-1),Q(:,2),G1(:,:,:,2))
  call loop_GGG_G_12(G1(:,:,:,2),wf(:,-5),wf(:,-3),G1(:,:,:,3))
  call check_last_UV_W(l_switch,G1(:,:,:,3),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,7))
  call loop_GGG_G_12(G1(:,:,:,2),wf(:,-3),wf(:,-5),G1(:,:,:,4))
  call check_last_UV_W(l_switch,G1(:,:,:,4),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,8))
  call loop_GGG_G_23(G1(:,:,:,2),wf(:,-5),wf(:,-3),G1(:,:,:,5))
  call check_last_UV_W(l_switch,G1(:,:,:,5),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,9))
  call loop_UV_W(G1(:,:,:,2),Q(:,22),wf(:,79),Q(:,40),G2(:,:,:,1))
  call check_last_UV_W(l_switch,G2(:,:,:,1),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,1))
  call loop_UV_W(G1(:,:,:,2),Q(:,22),wf(:,-3),Q(:,8),G2(:,:,:,2))
  call loop_UV_W(G2(:,:,:,2),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,1))
  call check_last_UV_W(l_switch,G3(:,:,:,1),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,1))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-1),wf(:,113),G0(:,:,:,27))
  call check_last_UV_W(l_switch,G0(:,:,:,27),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,34))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,113),wf(:,-1),G0(:,:,:,28))
  call check_last_UV_W(l_switch,G0(:,:,:,28),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,35))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-1),wf(:,113),G0(:,:,:,29))
  call check_last_UV_W(l_switch,G0(:,:,:,29),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,36))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-1),wf(:,79),G0(:,:,:,30))
  call check_last_UV_W(l_switch,G0(:,:,:,30),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,37))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,79),wf(:,-1),G0(:,:,:,31))
  call check_last_UV_W(l_switch,G0(:,:,:,31),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,38))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-1),wf(:,79),G0(:,:,:,32))
  call check_last_UV_W(l_switch,G0(:,:,:,32),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,39))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,-3),Q(:,8),G1(:,:,:,6))
  call loop_GGG_G_12(G1(:,:,:,6),wf(:,-5),wf(:,-1),G1(:,:,:,7))
  call check_last_UV_W(l_switch,G1(:,:,:,7),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,10))
  call loop_GGG_G_12(G1(:,:,:,6),wf(:,-1),wf(:,-5),G1(:,:,:,8))
  call check_last_UV_W(l_switch,G1(:,:,:,8),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,11))
  call loop_GGG_G_23(G1(:,:,:,6),wf(:,-5),wf(:,-1),G1(:,:,:,9))
  call check_last_UV_W(l_switch,G1(:,:,:,9),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,12))
  call loop_GGG_G_12(G1(:,:,:,6),wf(:,-5),wf(:,0),G1(:,:,:,10))
  call check_last_UV_W(l_switch,G1(:,:,:,10),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,13))
  call loop_GGG_G_12(G1(:,:,:,6),wf(:,0),wf(:,-5),G1(:,:,:,11))
  call check_last_UV_W(l_switch,G1(:,:,:,11),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,14))
  call loop_GGG_G_23(G1(:,:,:,6),wf(:,-5),wf(:,0),G1(:,:,:,12))
  call check_last_UV_W(l_switch,G1(:,:,:,12),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,15))
  call loop_UV_W(G1(:,:,:,6),Q(:,28),wf(:,-5),Q(:,32),G2(:,:,:,3))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,3),wf(:,-1),wf(:,0),G2tensor(:,16))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,3),wf(:,0),wf(:,-1),G2tensor(:,17))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,3),wf(:,-1),wf(:,0),G2tensor(:,18))
  call check_last_UV_W(l_switch,G2(:,:,:,3),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,2))
  call loop_UV_W(G1(:,:,:,6),Q(:,28),wf(:,113),Q(:,33),G2(:,:,:,4))
  call check_last_UV_W(l_switch,G2(:,:,:,4),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,3))
  call loop_UV_W(G1(:,:,:,6),Q(:,28),wf(:,99),Q(:,34),G2(:,:,:,5))
  call check_last_UV_W(l_switch,G2(:,:,:,5),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,4))
  call loop_UV_W(G1(:,:,:,6),Q(:,28),wf(:,-1),Q(:,2),G2(:,:,:,6))
  call loop_UV_W(G2(:,:,:,6),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,2))
  call check_last_UV_W(l_switch,G3(:,:,:,2),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,2))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,113),Q(:,33),G1(:,:,:,13))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,13),wf(:,-3),wf(:,-1),G1tensor(:,40))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,13),wf(:,-1),wf(:,-3),G1tensor(:,41))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,13),wf(:,-3),wf(:,-1),G1tensor(:,42))
  call check_last_UV_W(l_switch,G1(:,:,:,13),Q(:,53),wf(:,91),Q(:,10),G2tensor(:,19))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-3),wf(:,-1),G0(:,:,:,33))
  call loop_UV_W(G0(:,:,:,33),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,14))
  call check_last_UV_W(l_switch,G1(:,:,:,14),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,20))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-1),wf(:,-3),G0(:,:,:,34))
  call loop_UV_W(G0(:,:,:,34),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,15))
  call check_last_UV_W(l_switch,G1(:,:,:,15),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,21))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-3),wf(:,-1),G0(:,:,:,35))
  call loop_UV_W(G0(:,:,:,35),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,16))
  call check_last_UV_W(l_switch,G1(:,:,:,16),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,22))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,32),Q(:,42),G1(:,:,:,17))
  call check_last_UV_W(l_switch,G1(:,:,:,17),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,23))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,35),Q(:,42),G1(:,:,:,18))
  call check_last_UV_W(l_switch,G1(:,:,:,18),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,24))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,36),Q(:,42),G1(:,:,:,19))
  call check_last_UV_W(l_switch,G1(:,:,:,19),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,25))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,0),wf(:,99),G0(:,:,:,36))
  call check_last_UV_W(l_switch,G0(:,:,:,36),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,43))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,99),wf(:,0),G0(:,:,:,37))
  call check_last_UV_W(l_switch,G0(:,:,:,37),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,44))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,0),wf(:,99),G0(:,:,:,38))
  call check_last_UV_W(l_switch,G0(:,:,:,38),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,45))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,0),wf(:,79),G0(:,:,:,39))
  call check_last_UV_W(l_switch,G0(:,:,:,39),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,46))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,79),wf(:,0),G0(:,:,:,40))
  call check_last_UV_W(l_switch,G0(:,:,:,40),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,47))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,0),wf(:,79),G0(:,:,:,41))
  call check_last_UV_W(l_switch,G0(:,:,:,41),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,48))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,99),Q(:,34),G1(:,:,:,20))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,20),wf(:,-3),wf(:,0),G1tensor(:,49))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,20),wf(:,0),wf(:,-3),G1tensor(:,50))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,20),wf(:,-3),wf(:,0),G1tensor(:,51))
  call check_last_UV_W(l_switch,G1(:,:,:,20),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,26))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-3),wf(:,0),G0(:,:,:,42))
  call loop_UV_W(G0(:,:,:,42),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,21))
  call check_last_UV_W(l_switch,G1(:,:,:,21),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,27))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,0),wf(:,-3),G0(:,:,:,43))
  call loop_UV_W(G0(:,:,:,43),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,22))
  call check_last_UV_W(l_switch,G1(:,:,:,22),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,28))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-3),wf(:,0),G0(:,:,:,44))
  call loop_UV_W(G0(:,:,:,44),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,23))
  call check_last_UV_W(l_switch,G1(:,:,:,23),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,29))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,49),Q(:,41),G1(:,:,:,24))
  call check_last_UV_W(l_switch,G1(:,:,:,24),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,30))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,51),Q(:,41),G1(:,:,:,25))
  call check_last_UV_W(l_switch,G1(:,:,:,25),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,31))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,52),Q(:,41),G1(:,:,:,26))
  call check_last_UV_W(l_switch,G1(:,:,:,26),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,32))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,79),Q(:,40),G1(:,:,:,27))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,27),wf(:,-1),wf(:,0),G1tensor(:,52))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,27),wf(:,0),wf(:,-1),G1tensor(:,53))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,27),wf(:,-1),wf(:,0),G1tensor(:,54))
  call check_last_UV_W(l_switch,G1(:,:,:,27),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,33))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,19),Q(:,35),G1(:,:,:,28))
  call check_last_UV_W(l_switch,G1(:,:,:,28),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,34))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,21),Q(:,35),G1(:,:,:,29))
  call check_last_UV_W(l_switch,G1(:,:,:,29),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,35))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,22),Q(:,35),G1(:,:,:,30))
  call check_last_UV_W(l_switch,G1(:,:,:,30),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,36))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,89),Q(:,35),G1(:,:,:,31))
  call check_last_UV_W(l_switch,G1(:,:,:,31),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,37))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,182),Q(:,41),G1(:,:,:,32))
  call check_last_UV_W(l_switch,G1(:,:,:,32),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,38))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,104),Q(:,9),G1(:,:,:,33))
  call loop_UV_W(G1(:,:,:,33),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,7))
  call check_last_UV_W(l_switch,G2(:,:,:,7),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,5))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,236),Q(:,42),G1(:,:,:,34))
  call check_last_UV_W(l_switch,G1(:,:,:,34),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,39))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,91),Q(:,10),G1(:,:,:,35))
  call loop_UV_W(G1(:,:,:,35),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,8))
  call check_last_UV_W(l_switch,G2(:,:,:,8),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,6))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,140),Q(:,35),G1(:,:,:,36))
  call check_last_UV_W(l_switch,G1(:,:,:,36),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,40))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,191),Q(:,41),G1(:,:,:,37))
  call check_last_UV_W(l_switch,G1(:,:,:,37),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,41))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,141),Q(:,35),G1(:,:,:,38))
  call check_last_UV_W(l_switch,G1(:,:,:,38),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,42))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,245),Q(:,42),G1(:,:,:,39))
  call check_last_UV_W(l_switch,G1(:,:,:,39),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,43))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,192),Q(:,41),G1(:,:,:,40))
  call check_last_UV_W(l_switch,G1(:,:,:,40),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,44))
  call loop_UV_W(G0(:,:,:,2),Q(:,20),wf(:,246),Q(:,42),G1(:,:,:,41))
  call check_last_UV_W(l_switch,G1(:,:,:,41),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,45))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,-4),G0(:,:,:,45))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,-5),wf(:,-3),G0(:,:,:,46))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,46),wf(:,-1),wf(:,0),G0tensor(:,28))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,46),wf(:,0),wf(:,-1),G0tensor(:,29))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,46),wf(:,-1),wf(:,0),G0tensor(:,30))
  call check_last_UV_W(l_switch,G0(:,:,:,46),Q(:,60),wf(:,61),Q(:,3),G1tensor(:,55))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,-3),wf(:,-5),G0(:,:,:,47))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-1),wf(:,0),G0tensor(:,31))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,0),wf(:,-1),G0tensor(:,32))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,-1),wf(:,0),G0tensor(:,33))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,60),wf(:,61),Q(:,3),G1tensor(:,56))
  call loop_GGG_G_23(G0(:,:,:,45),wf(:,-5),wf(:,-3),G0(:,:,:,48))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-1),wf(:,0),G0tensor(:,34))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,0),wf(:,-1),G0tensor(:,35))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,-1),wf(:,0),G0tensor(:,36))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,60),wf(:,61),Q(:,3),G1tensor(:,57))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,-5),wf(:,-1),G0(:,:,:,49))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,49),wf(:,-3),wf(:,0),G0tensor(:,37))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,49),wf(:,0),wf(:,-3),G0tensor(:,38))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,49),wf(:,-3),wf(:,0),G0tensor(:,39))
  call check_last_UV_W(l_switch,G0(:,:,:,49),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,58))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,-1),wf(:,-5),G0(:,:,:,50))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,50),wf(:,-3),wf(:,0),G0tensor(:,40))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,50),wf(:,0),wf(:,-3),G0tensor(:,41))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,50),wf(:,-3),wf(:,0),G0tensor(:,42))
  call check_last_UV_W(l_switch,G0(:,:,:,50),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,59))
  call loop_GGG_G_23(G0(:,:,:,45),wf(:,-5),wf(:,-1),G0(:,:,:,51))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,51),wf(:,-3),wf(:,0),G0tensor(:,43))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,51),wf(:,0),wf(:,-3),G0tensor(:,44))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,51),wf(:,-3),wf(:,0),G0tensor(:,45))
  call check_last_UV_W(l_switch,G0(:,:,:,51),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,60))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,-5),wf(:,0),G0(:,:,:,52))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,52),wf(:,-3),wf(:,-1),G0tensor(:,46))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,52),wf(:,-1),wf(:,-3),G0tensor(:,47))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,52),wf(:,-3),wf(:,-1),G0tensor(:,48))
  call check_last_UV_W(l_switch,G0(:,:,:,52),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,61))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,0),wf(:,-5),G0(:,:,:,53))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,53),wf(:,-3),wf(:,-1),G0tensor(:,49))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,53),wf(:,-1),wf(:,-3),G0tensor(:,50))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,53),wf(:,-3),wf(:,-1),G0tensor(:,51))
  call check_last_UV_W(l_switch,G0(:,:,:,53),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,62))
  call loop_GGG_G_23(G0(:,:,:,45),wf(:,-5),wf(:,0),G0(:,:,:,54))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,54),wf(:,-3),wf(:,-1),G0tensor(:,52))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,54),wf(:,-1),wf(:,-3),G0tensor(:,53))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,54),wf(:,-3),wf(:,-1),G0tensor(:,54))
  call check_last_UV_W(l_switch,G0(:,:,:,54),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,63))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,-5),wf(:,61),G0(:,:,:,55))
  call check_last_UV_W(l_switch,G0(:,:,:,55),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,64))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,61),wf(:,-5),G0(:,:,:,56))
  call check_last_UV_W(l_switch,G0(:,:,:,56),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,65))
  call loop_GGG_G_23(G0(:,:,:,45),wf(:,-5),wf(:,61),G0(:,:,:,57))
  call check_last_UV_W(l_switch,G0(:,:,:,57),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,66))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,-5),Q(:,32),G1(:,:,:,42))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,42),wf(:,-3),wf(:,61),G1tensor(:,67))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,42),wf(:,61),wf(:,-3),G1tensor(:,68))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,42),wf(:,-3),wf(:,61),G1tensor(:,69))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,42),wf(:,-1),wf(:,104),G1tensor(:,70))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,42),wf(:,104),wf(:,-1),G1tensor(:,71))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,42),wf(:,-1),wf(:,104),G1tensor(:,72))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,42),wf(:,0),wf(:,91),G1tensor(:,73))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,42),wf(:,91),wf(:,0),G1tensor(:,74))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,42),wf(:,0),wf(:,91),G1tensor(:,75))
  call check_last_UV_W(l_switch,G1(:,:,:,42),Q(:,52),wf(:,7),Q(:,11),G2tensor(:,46))
  call check_last_UV_W(l_switch,G1(:,:,:,42),Q(:,52),wf(:,9),Q(:,11),G2tensor(:,47))
  call check_last_UV_W(l_switch,G1(:,:,:,42),Q(:,52),wf(:,10),Q(:,11),G2tensor(:,48))
  call check_last_UV_W(l_switch,G1(:,:,:,42),Q(:,52),wf(:,83),Q(:,11),G2tensor(:,49))
  call check_last_UV_W(l_switch,G1(:,:,:,42),Q(:,52),wf(:,124),Q(:,11),G2tensor(:,50))
  call check_last_UV_W(l_switch,G1(:,:,:,42),Q(:,52),wf(:,131),Q(:,11),G2tensor(:,51))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,-5),wf(:,104),G0(:,:,:,58))
  call check_last_UV_W(l_switch,G0(:,:,:,58),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,76))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,104),wf(:,-5),G0(:,:,:,59))
  call check_last_UV_W(l_switch,G0(:,:,:,59),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,77))
  call loop_GGG_G_23(G0(:,:,:,45),wf(:,-5),wf(:,104),G0(:,:,:,60))
  call check_last_UV_W(l_switch,G0(:,:,:,60),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,78))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,-5),wf(:,91),G0(:,:,:,61))
  call check_last_UV_W(l_switch,G0(:,:,:,61),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,79))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,91),wf(:,-5),G0(:,:,:,62))
  call check_last_UV_W(l_switch,G0(:,:,:,62),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,80))
  call loop_GGG_G_23(G0(:,:,:,45),wf(:,-5),wf(:,91),G0(:,:,:,63))
  call check_last_UV_W(l_switch,G0(:,:,:,63),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,81))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,-3),wf(:,113),G0(:,:,:,64))
  call check_last_UV_W(l_switch,G0(:,:,:,64),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,82))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,113),wf(:,-3),G0(:,:,:,65))
  call check_last_UV_W(l_switch,G0(:,:,:,65),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,83))
  call loop_GGG_G_23(G0(:,:,:,45),wf(:,-3),wf(:,113),G0(:,:,:,66))
  call check_last_UV_W(l_switch,G0(:,:,:,66),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,84))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,-3),wf(:,99),G0(:,:,:,67))
  call check_last_UV_W(l_switch,G0(:,:,:,67),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,85))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,99),wf(:,-3),G0(:,:,:,68))
  call check_last_UV_W(l_switch,G0(:,:,:,68),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,86))
  call loop_GGG_G_23(G0(:,:,:,45),wf(:,-3),wf(:,99),G0(:,:,:,69))
  call check_last_UV_W(l_switch,G0(:,:,:,69),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,87))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,-1),Q(:,2),G1(:,:,:,43))
  call loop_GGG_G_12(G1(:,:,:,43),wf(:,-5),wf(:,-3),G1(:,:,:,44))
  call check_last_UV_W(l_switch,G1(:,:,:,44),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,52))
  call loop_GGG_G_12(G1(:,:,:,43),wf(:,-3),wf(:,-5),G1(:,:,:,45))
  call check_last_UV_W(l_switch,G1(:,:,:,45),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,53))
  call loop_GGG_G_23(G1(:,:,:,43),wf(:,-5),wf(:,-3),G1(:,:,:,46))
  call check_last_UV_W(l_switch,G1(:,:,:,46),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,54))
  call loop_UV_W(G1(:,:,:,43),Q(:,22),wf(:,79),Q(:,40),G2(:,:,:,9))
  call check_last_UV_W(l_switch,G2(:,:,:,9),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,7))
  call loop_UV_W(G1(:,:,:,43),Q(:,22),wf(:,-3),Q(:,8),G2(:,:,:,10))
  call loop_UV_W(G2(:,:,:,10),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,3))
  call check_last_UV_W(l_switch,G3(:,:,:,3),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,3))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,-1),wf(:,113),G0(:,:,:,70))
  call check_last_UV_W(l_switch,G0(:,:,:,70),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,88))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,113),wf(:,-1),G0(:,:,:,71))
  call check_last_UV_W(l_switch,G0(:,:,:,71),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,89))
  call loop_GGG_G_23(G0(:,:,:,45),wf(:,-1),wf(:,113),G0(:,:,:,72))
  call check_last_UV_W(l_switch,G0(:,:,:,72),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,90))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,-1),wf(:,79),G0(:,:,:,73))
  call check_last_UV_W(l_switch,G0(:,:,:,73),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,91))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,79),wf(:,-1),G0(:,:,:,74))
  call check_last_UV_W(l_switch,G0(:,:,:,74),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,92))
  call loop_GGG_G_23(G0(:,:,:,45),wf(:,-1),wf(:,79),G0(:,:,:,75))
  call check_last_UV_W(l_switch,G0(:,:,:,75),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,93))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,-3),Q(:,8),G1(:,:,:,47))
  call loop_GGG_G_12(G1(:,:,:,47),wf(:,-5),wf(:,-1),G1(:,:,:,48))
  call check_last_UV_W(l_switch,G1(:,:,:,48),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,55))
  call loop_GGG_G_12(G1(:,:,:,47),wf(:,-1),wf(:,-5),G1(:,:,:,49))
  call check_last_UV_W(l_switch,G1(:,:,:,49),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,56))
  call loop_GGG_G_23(G1(:,:,:,47),wf(:,-5),wf(:,-1),G1(:,:,:,50))
  call check_last_UV_W(l_switch,G1(:,:,:,50),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,57))
  call loop_GGG_G_12(G1(:,:,:,47),wf(:,-5),wf(:,0),G1(:,:,:,51))
  call check_last_UV_W(l_switch,G1(:,:,:,51),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,58))
  call loop_GGG_G_12(G1(:,:,:,47),wf(:,0),wf(:,-5),G1(:,:,:,52))
  call check_last_UV_W(l_switch,G1(:,:,:,52),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,59))
  call loop_GGG_G_23(G1(:,:,:,47),wf(:,-5),wf(:,0),G1(:,:,:,53))
  call check_last_UV_W(l_switch,G1(:,:,:,53),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,60))
  call loop_UV_W(G1(:,:,:,47),Q(:,28),wf(:,-5),Q(:,32),G2(:,:,:,11))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,11),wf(:,-1),wf(:,0),G2tensor(:,61))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,11),wf(:,0),wf(:,-1),G2tensor(:,62))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,11),wf(:,-1),wf(:,0),G2tensor(:,63))
  call check_last_UV_W(l_switch,G2(:,:,:,11),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,8))
  call loop_UV_W(G1(:,:,:,47),Q(:,28),wf(:,113),Q(:,33),G2(:,:,:,12))
  call check_last_UV_W(l_switch,G2(:,:,:,12),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,9))
  call loop_UV_W(G1(:,:,:,47),Q(:,28),wf(:,99),Q(:,34),G2(:,:,:,13))
  call check_last_UV_W(l_switch,G2(:,:,:,13),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,10))
  call loop_UV_W(G1(:,:,:,47),Q(:,28),wf(:,-1),Q(:,2),G2(:,:,:,14))
  call loop_UV_W(G2(:,:,:,14),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,4))
  call check_last_UV_W(l_switch,G3(:,:,:,4),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,4))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,113),Q(:,33),G1(:,:,:,54))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,54),wf(:,-3),wf(:,-1),G1tensor(:,94))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,54),wf(:,-1),wf(:,-3),G1tensor(:,95))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,54),wf(:,-3),wf(:,-1),G1tensor(:,96))
  call check_last_UV_W(l_switch,G1(:,:,:,54),Q(:,53),wf(:,91),Q(:,10),G2tensor(:,64))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,-3),wf(:,-1),G0(:,:,:,76))
  call loop_UV_W(G0(:,:,:,76),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,55))
  call check_last_UV_W(l_switch,G1(:,:,:,55),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,65))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,-1),wf(:,-3),G0(:,:,:,77))
  call loop_UV_W(G0(:,:,:,77),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,56))
  call check_last_UV_W(l_switch,G1(:,:,:,56),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,66))
  call loop_GGG_G_23(G0(:,:,:,45),wf(:,-3),wf(:,-1),G0(:,:,:,78))
  call loop_UV_W(G0(:,:,:,78),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,57))
  call check_last_UV_W(l_switch,G1(:,:,:,57),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,67))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,32),Q(:,42),G1(:,:,:,58))
  call check_last_UV_W(l_switch,G1(:,:,:,58),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,68))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,35),Q(:,42),G1(:,:,:,59))
  call check_last_UV_W(l_switch,G1(:,:,:,59),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,69))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,36),Q(:,42),G1(:,:,:,60))
  call check_last_UV_W(l_switch,G1(:,:,:,60),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,70))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,0),wf(:,99),G0(:,:,:,79))
  call check_last_UV_W(l_switch,G0(:,:,:,79),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,97))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,99),wf(:,0),G0(:,:,:,80))
  call check_last_UV_W(l_switch,G0(:,:,:,80),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,98))
  call loop_GGG_G_23(G0(:,:,:,45),wf(:,0),wf(:,99),G0(:,:,:,81))
  call check_last_UV_W(l_switch,G0(:,:,:,81),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,99))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,0),wf(:,79),G0(:,:,:,82))
  call check_last_UV_W(l_switch,G0(:,:,:,82),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,100))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,79),wf(:,0),G0(:,:,:,83))
  call check_last_UV_W(l_switch,G0(:,:,:,83),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,101))
  call loop_GGG_G_23(G0(:,:,:,45),wf(:,0),wf(:,79),G0(:,:,:,84))
  call check_last_UV_W(l_switch,G0(:,:,:,84),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,102))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,99),Q(:,34),G1(:,:,:,61))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,61),wf(:,-3),wf(:,0),G1tensor(:,103))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,61),wf(:,0),wf(:,-3),G1tensor(:,104))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,61),wf(:,-3),wf(:,0),G1tensor(:,105))
  call check_last_UV_W(l_switch,G1(:,:,:,61),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,71))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,-3),wf(:,0),G0(:,:,:,85))
  call loop_UV_W(G0(:,:,:,85),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,62))
  call check_last_UV_W(l_switch,G1(:,:,:,62),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,72))
  call loop_GGG_G_12(G0(:,:,:,45),wf(:,0),wf(:,-3),G0(:,:,:,86))
  call loop_UV_W(G0(:,:,:,86),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,63))
  call check_last_UV_W(l_switch,G1(:,:,:,63),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,73))
  call loop_GGG_G_23(G0(:,:,:,45),wf(:,-3),wf(:,0),G0(:,:,:,87))
  call loop_UV_W(G0(:,:,:,87),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,64))
  call check_last_UV_W(l_switch,G1(:,:,:,64),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,74))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,49),Q(:,41),G1(:,:,:,65))
  call check_last_UV_W(l_switch,G1(:,:,:,65),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,75))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,51),Q(:,41),G1(:,:,:,66))
  call check_last_UV_W(l_switch,G1(:,:,:,66),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,76))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,52),Q(:,41),G1(:,:,:,67))
  call check_last_UV_W(l_switch,G1(:,:,:,67),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,77))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,79),Q(:,40),G1(:,:,:,68))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,68),wf(:,-1),wf(:,0),G1tensor(:,106))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,68),wf(:,0),wf(:,-1),G1tensor(:,107))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,68),wf(:,-1),wf(:,0),G1tensor(:,108))
  call check_last_UV_W(l_switch,G1(:,:,:,68),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,78))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,19),Q(:,35),G1(:,:,:,69))
  call check_last_UV_W(l_switch,G1(:,:,:,69),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,79))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,21),Q(:,35),G1(:,:,:,70))
  call check_last_UV_W(l_switch,G1(:,:,:,70),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,80))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,22),Q(:,35),G1(:,:,:,71))
  call check_last_UV_W(l_switch,G1(:,:,:,71),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,81))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,89),Q(:,35),G1(:,:,:,72))
  call check_last_UV_W(l_switch,G1(:,:,:,72),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,82))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,182),Q(:,41),G1(:,:,:,73))
  call check_last_UV_W(l_switch,G1(:,:,:,73),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,83))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,104),Q(:,9),G1(:,:,:,74))
  call loop_UV_W(G1(:,:,:,74),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,15))
  call check_last_UV_W(l_switch,G2(:,:,:,15),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,11))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,236),Q(:,42),G1(:,:,:,75))
  call check_last_UV_W(l_switch,G1(:,:,:,75),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,84))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,91),Q(:,10),G1(:,:,:,76))
  call loop_UV_W(G1(:,:,:,76),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,16))
  call check_last_UV_W(l_switch,G2(:,:,:,16),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,12))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,140),Q(:,35),G1(:,:,:,77))
  call check_last_UV_W(l_switch,G1(:,:,:,77),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,85))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,191),Q(:,41),G1(:,:,:,78))
  call check_last_UV_W(l_switch,G1(:,:,:,78),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,86))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,141),Q(:,35),G1(:,:,:,79))
  call check_last_UV_W(l_switch,G1(:,:,:,79),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,87))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,245),Q(:,42),G1(:,:,:,80))
  call check_last_UV_W(l_switch,G1(:,:,:,80),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,88))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,192),Q(:,41),G1(:,:,:,81))
  call check_last_UV_W(l_switch,G1(:,:,:,81),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,89))
  call loop_UV_W(G0(:,:,:,45),Q(:,20),wf(:,246),Q(:,42),G1(:,:,:,82))
  call check_last_UV_W(l_switch,G1(:,:,:,82),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,90))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,-2),G0(:,:,:,88))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,-5),wf(:,-3),G0(:,:,:,89))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,89),wf(:,-1),wf(:,0),G0tensor(:,55))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,89),wf(:,0),wf(:,-1),G0tensor(:,56))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,89),wf(:,-1),wf(:,0),G0tensor(:,57))
  call check_last_UV_W(l_switch,G0(:,:,:,89),Q(:,60),wf(:,61),Q(:,3),G1tensor(:,109))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,-3),wf(:,-5),G0(:,:,:,90))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,90),wf(:,-1),wf(:,0),G0tensor(:,58))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,90),wf(:,0),wf(:,-1),G0tensor(:,59))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,90),wf(:,-1),wf(:,0),G0tensor(:,60))
  call check_last_UV_W(l_switch,G0(:,:,:,90),Q(:,60),wf(:,61),Q(:,3),G1tensor(:,110))
  call loop_GGG_G_23(G0(:,:,:,88),wf(:,-5),wf(:,-3),G0(:,:,:,91))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,91),wf(:,-1),wf(:,0),G0tensor(:,61))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,91),wf(:,0),wf(:,-1),G0tensor(:,62))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,91),wf(:,-1),wf(:,0),G0tensor(:,63))
  call check_last_UV_W(l_switch,G0(:,:,:,91),Q(:,60),wf(:,61),Q(:,3),G1tensor(:,111))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,-5),wf(:,-1),G0(:,:,:,92))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,92),wf(:,-3),wf(:,0),G0tensor(:,64))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,92),wf(:,0),wf(:,-3),G0tensor(:,65))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,92),wf(:,-3),wf(:,0),G0tensor(:,66))
  call check_last_UV_W(l_switch,G0(:,:,:,92),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,112))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,-1),wf(:,-5),G0(:,:,:,93))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,93),wf(:,-3),wf(:,0),G0tensor(:,67))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,93),wf(:,0),wf(:,-3),G0tensor(:,68))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,93),wf(:,-3),wf(:,0),G0tensor(:,69))
  call check_last_UV_W(l_switch,G0(:,:,:,93),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,113))
  call loop_GGG_G_23(G0(:,:,:,88),wf(:,-5),wf(:,-1),G0(:,:,:,94))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,94),wf(:,-3),wf(:,0),G0tensor(:,70))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,94),wf(:,0),wf(:,-3),G0tensor(:,71))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,94),wf(:,-3),wf(:,0),G0tensor(:,72))
  call check_last_UV_W(l_switch,G0(:,:,:,94),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,114))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,-5),wf(:,0),G0(:,:,:,95))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,95),wf(:,-3),wf(:,-1),G0tensor(:,73))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,95),wf(:,-1),wf(:,-3),G0tensor(:,74))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,95),wf(:,-3),wf(:,-1),G0tensor(:,75))
  call check_last_UV_W(l_switch,G0(:,:,:,95),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,115))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,0),wf(:,-5),G0(:,:,:,96))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,96),wf(:,-3),wf(:,-1),G0tensor(:,76))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,96),wf(:,-1),wf(:,-3),G0tensor(:,77))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,96),wf(:,-3),wf(:,-1),G0tensor(:,78))
  call check_last_UV_W(l_switch,G0(:,:,:,96),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,116))
  call loop_GGG_G_23(G0(:,:,:,88),wf(:,-5),wf(:,0),G0(:,:,:,97))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,97),wf(:,-3),wf(:,-1),G0tensor(:,79))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,97),wf(:,-1),wf(:,-3),G0tensor(:,80))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,97),wf(:,-3),wf(:,-1),G0tensor(:,81))
  call check_last_UV_W(l_switch,G0(:,:,:,97),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,117))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,-5),wf(:,61),G0(:,:,:,98))
  call check_last_UV_W(l_switch,G0(:,:,:,98),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,118))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,61),wf(:,-5),G0(:,:,:,99))
  call check_last_UV_W(l_switch,G0(:,:,:,99),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,119))
  call loop_GGG_G_23(G0(:,:,:,88),wf(:,-5),wf(:,61),G0(:,:,:,100))
  call check_last_UV_W(l_switch,G0(:,:,:,100),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,120))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,-5),Q(:,32),G1(:,:,:,83))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,83),wf(:,-3),wf(:,61),G1tensor(:,121))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,83),wf(:,61),wf(:,-3),G1tensor(:,122))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,83),wf(:,-3),wf(:,61),G1tensor(:,123))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,83),wf(:,-1),wf(:,104),G1tensor(:,124))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,83),wf(:,104),wf(:,-1),G1tensor(:,125))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,83),wf(:,-1),wf(:,104),G1tensor(:,126))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,83),wf(:,0),wf(:,91),G1tensor(:,127))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,83),wf(:,91),wf(:,0),G1tensor(:,128))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,83),wf(:,0),wf(:,91),G1tensor(:,129))
  call check_last_UV_W(l_switch,G1(:,:,:,83),Q(:,52),wf(:,7),Q(:,11),G2tensor(:,91))
  call check_last_UV_W(l_switch,G1(:,:,:,83),Q(:,52),wf(:,9),Q(:,11),G2tensor(:,92))
  call check_last_UV_W(l_switch,G1(:,:,:,83),Q(:,52),wf(:,10),Q(:,11),G2tensor(:,93))
  call check_last_UV_W(l_switch,G1(:,:,:,83),Q(:,52),wf(:,83),Q(:,11),G2tensor(:,94))
  call check_last_UV_W(l_switch,G1(:,:,:,83),Q(:,52),wf(:,124),Q(:,11),G2tensor(:,95))
  call check_last_UV_W(l_switch,G1(:,:,:,83),Q(:,52),wf(:,131),Q(:,11),G2tensor(:,96))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,-5),wf(:,104),G0(:,:,:,101))
  call check_last_UV_W(l_switch,G0(:,:,:,101),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,130))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,104),wf(:,-5),G0(:,:,:,102))
  call check_last_UV_W(l_switch,G0(:,:,:,102),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,131))
  call loop_GGG_G_23(G0(:,:,:,88),wf(:,-5),wf(:,104),G0(:,:,:,103))
  call check_last_UV_W(l_switch,G0(:,:,:,103),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,132))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,-5),wf(:,91),G0(:,:,:,104))
  call check_last_UV_W(l_switch,G0(:,:,:,104),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,133))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,91),wf(:,-5),G0(:,:,:,105))
  call check_last_UV_W(l_switch,G0(:,:,:,105),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,134))
  call loop_GGG_G_23(G0(:,:,:,88),wf(:,-5),wf(:,91),G0(:,:,:,106))
  call check_last_UV_W(l_switch,G0(:,:,:,106),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,135))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,-3),wf(:,113),G0(:,:,:,107))
  call check_last_UV_W(l_switch,G0(:,:,:,107),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,136))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,113),wf(:,-3),G0(:,:,:,108))
  call check_last_UV_W(l_switch,G0(:,:,:,108),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,137))
  call loop_GGG_G_23(G0(:,:,:,88),wf(:,-3),wf(:,113),G0(:,:,:,109))
  call check_last_UV_W(l_switch,G0(:,:,:,109),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,138))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,-3),wf(:,99),G0(:,:,:,110))
  call check_last_UV_W(l_switch,G0(:,:,:,110),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,139))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,99),wf(:,-3),G0(:,:,:,111))
  call check_last_UV_W(l_switch,G0(:,:,:,111),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,140))
  call loop_GGG_G_23(G0(:,:,:,88),wf(:,-3),wf(:,99),G0(:,:,:,112))
  call check_last_UV_W(l_switch,G0(:,:,:,112),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,141))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,-1),Q(:,2),G1(:,:,:,84))
  call loop_GGG_G_12(G1(:,:,:,84),wf(:,-5),wf(:,-3),G1(:,:,:,85))
  call check_last_UV_W(l_switch,G1(:,:,:,85),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,97))
  call loop_GGG_G_12(G1(:,:,:,84),wf(:,-3),wf(:,-5),G1(:,:,:,86))
  call check_last_UV_W(l_switch,G1(:,:,:,86),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,98))
  call loop_GGG_G_23(G1(:,:,:,84),wf(:,-5),wf(:,-3),G1(:,:,:,87))
  call check_last_UV_W(l_switch,G1(:,:,:,87),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,99))
  call loop_UV_W(G1(:,:,:,84),Q(:,22),wf(:,79),Q(:,40),G2(:,:,:,17))
  call check_last_UV_W(l_switch,G2(:,:,:,17),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,13))
  call loop_UV_W(G1(:,:,:,84),Q(:,22),wf(:,-3),Q(:,8),G2(:,:,:,18))
  call loop_UV_W(G2(:,:,:,18),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,5))
  call check_last_UV_W(l_switch,G3(:,:,:,5),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,5))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,-1),wf(:,113),G0(:,:,:,113))
  call check_last_UV_W(l_switch,G0(:,:,:,113),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,142))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,113),wf(:,-1),G0(:,:,:,114))
  call check_last_UV_W(l_switch,G0(:,:,:,114),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,143))
  call loop_GGG_G_23(G0(:,:,:,88),wf(:,-1),wf(:,113),G0(:,:,:,115))
  call check_last_UV_W(l_switch,G0(:,:,:,115),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,144))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,-1),wf(:,79),G0(:,:,:,116))
  call check_last_UV_W(l_switch,G0(:,:,:,116),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,145))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,79),wf(:,-1),G0(:,:,:,117))
  call check_last_UV_W(l_switch,G0(:,:,:,117),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,146))
  call loop_GGG_G_23(G0(:,:,:,88),wf(:,-1),wf(:,79),G0(:,:,:,118))
  call check_last_UV_W(l_switch,G0(:,:,:,118),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,147))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,-3),Q(:,8),G1(:,:,:,88))
  call loop_GGG_G_12(G1(:,:,:,88),wf(:,-5),wf(:,-1),G1(:,:,:,89))
  call check_last_UV_W(l_switch,G1(:,:,:,89),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,100))
  call loop_GGG_G_12(G1(:,:,:,88),wf(:,-1),wf(:,-5),G1(:,:,:,90))
  call check_last_UV_W(l_switch,G1(:,:,:,90),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,101))
  call loop_GGG_G_23(G1(:,:,:,88),wf(:,-5),wf(:,-1),G1(:,:,:,91))
  call check_last_UV_W(l_switch,G1(:,:,:,91),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,102))
  call loop_GGG_G_12(G1(:,:,:,88),wf(:,-5),wf(:,0),G1(:,:,:,92))
  call check_last_UV_W(l_switch,G1(:,:,:,92),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,103))
  call loop_GGG_G_12(G1(:,:,:,88),wf(:,0),wf(:,-5),G1(:,:,:,93))
  call check_last_UV_W(l_switch,G1(:,:,:,93),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,104))
  call loop_GGG_G_23(G1(:,:,:,88),wf(:,-5),wf(:,0),G1(:,:,:,94))
  call check_last_UV_W(l_switch,G1(:,:,:,94),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,105))
  call loop_UV_W(G1(:,:,:,88),Q(:,28),wf(:,-5),Q(:,32),G2(:,:,:,19))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,19),wf(:,-1),wf(:,0),G2tensor(:,106))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,19),wf(:,0),wf(:,-1),G2tensor(:,107))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,19),wf(:,-1),wf(:,0),G2tensor(:,108))
  call check_last_UV_W(l_switch,G2(:,:,:,19),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,14))
  call loop_UV_W(G1(:,:,:,88),Q(:,28),wf(:,113),Q(:,33),G2(:,:,:,20))
  call check_last_UV_W(l_switch,G2(:,:,:,20),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,15))
  call loop_UV_W(G1(:,:,:,88),Q(:,28),wf(:,99),Q(:,34),G2(:,:,:,21))
  call check_last_UV_W(l_switch,G2(:,:,:,21),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,16))
  call loop_UV_W(G1(:,:,:,88),Q(:,28),wf(:,-1),Q(:,2),G2(:,:,:,22))
  call loop_UV_W(G2(:,:,:,22),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,6))
  call check_last_UV_W(l_switch,G3(:,:,:,6),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,6))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,113),Q(:,33),G1(:,:,:,95))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,95),wf(:,-3),wf(:,-1),G1tensor(:,148))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,95),wf(:,-1),wf(:,-3),G1tensor(:,149))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,95),wf(:,-3),wf(:,-1),G1tensor(:,150))
  call check_last_UV_W(l_switch,G1(:,:,:,95),Q(:,53),wf(:,91),Q(:,10),G2tensor(:,109))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,-3),wf(:,-1),G0(:,:,:,119))
  call loop_UV_W(G0(:,:,:,119),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,96))
  call check_last_UV_W(l_switch,G1(:,:,:,96),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,110))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,-1),wf(:,-3),G0(:,:,:,120))
  call loop_UV_W(G0(:,:,:,120),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,97))
  call check_last_UV_W(l_switch,G1(:,:,:,97),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,111))
  call loop_GGG_G_23(G0(:,:,:,88),wf(:,-3),wf(:,-1),G0(:,:,:,121))
  call loop_UV_W(G0(:,:,:,121),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,98))
  call check_last_UV_W(l_switch,G1(:,:,:,98),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,112))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,32),Q(:,42),G1(:,:,:,99))
  call check_last_UV_W(l_switch,G1(:,:,:,99),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,113))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,35),Q(:,42),G1(:,:,:,100))
  call check_last_UV_W(l_switch,G1(:,:,:,100),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,114))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,36),Q(:,42),G1(:,:,:,101))
  call check_last_UV_W(l_switch,G1(:,:,:,101),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,115))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,0),wf(:,99),G0(:,:,:,122))
  call check_last_UV_W(l_switch,G0(:,:,:,122),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,151))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,99),wf(:,0),G0(:,:,:,123))
  call check_last_UV_W(l_switch,G0(:,:,:,123),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,152))
  call loop_GGG_G_23(G0(:,:,:,88),wf(:,0),wf(:,99),G0(:,:,:,124))
  call check_last_UV_W(l_switch,G0(:,:,:,124),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,153))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,0),wf(:,79),G0(:,:,:,125))
  call check_last_UV_W(l_switch,G0(:,:,:,125),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,154))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,79),wf(:,0),G0(:,:,:,126))
  call check_last_UV_W(l_switch,G0(:,:,:,126),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,155))
  call loop_GGG_G_23(G0(:,:,:,88),wf(:,0),wf(:,79),G0(:,:,:,127))
  call check_last_UV_W(l_switch,G0(:,:,:,127),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,156))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,99),Q(:,34),G1(:,:,:,102))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,102),wf(:,-3),wf(:,0),G1tensor(:,157))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,102),wf(:,0),wf(:,-3),G1tensor(:,158))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,102),wf(:,-3),wf(:,0),G1tensor(:,159))
  call check_last_UV_W(l_switch,G1(:,:,:,102),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,116))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,-3),wf(:,0),G0(:,:,:,128))
  call loop_UV_W(G0(:,:,:,128),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,103))
  call check_last_UV_W(l_switch,G1(:,:,:,103),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,117))
  call loop_GGG_G_12(G0(:,:,:,88),wf(:,0),wf(:,-3),G0(:,:,:,129))
  call loop_UV_W(G0(:,:,:,129),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,104))
  call check_last_UV_W(l_switch,G1(:,:,:,104),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,118))
  call loop_GGG_G_23(G0(:,:,:,88),wf(:,-3),wf(:,0),G0(:,:,:,130))
  call loop_UV_W(G0(:,:,:,130),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,105))
  call check_last_UV_W(l_switch,G1(:,:,:,105),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,119))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,49),Q(:,41),G1(:,:,:,106))
  call check_last_UV_W(l_switch,G1(:,:,:,106),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,120))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,51),Q(:,41),G1(:,:,:,107))
  call check_last_UV_W(l_switch,G1(:,:,:,107),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,121))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,52),Q(:,41),G1(:,:,:,108))
  call check_last_UV_W(l_switch,G1(:,:,:,108),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,122))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,79),Q(:,40),G1(:,:,:,109))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,109),wf(:,-1),wf(:,0),G1tensor(:,160))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,109),wf(:,0),wf(:,-1),G1tensor(:,161))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,109),wf(:,-1),wf(:,0),G1tensor(:,162))
  call check_last_UV_W(l_switch,G1(:,:,:,109),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,123))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,19),Q(:,35),G1(:,:,:,110))
  call check_last_UV_W(l_switch,G1(:,:,:,110),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,124))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,21),Q(:,35),G1(:,:,:,111))
  call check_last_UV_W(l_switch,G1(:,:,:,111),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,125))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,22),Q(:,35),G1(:,:,:,112))
  call check_last_UV_W(l_switch,G1(:,:,:,112),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,126))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,89),Q(:,35),G1(:,:,:,113))
  call check_last_UV_W(l_switch,G1(:,:,:,113),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,127))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,182),Q(:,41),G1(:,:,:,114))
  call check_last_UV_W(l_switch,G1(:,:,:,114),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,128))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,104),Q(:,9),G1(:,:,:,115))
  call loop_UV_W(G1(:,:,:,115),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,23))
  call check_last_UV_W(l_switch,G2(:,:,:,23),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,17))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,236),Q(:,42),G1(:,:,:,116))
  call check_last_UV_W(l_switch,G1(:,:,:,116),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,129))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,91),Q(:,10),G1(:,:,:,117))
  call loop_UV_W(G1(:,:,:,117),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,24))
  call check_last_UV_W(l_switch,G2(:,:,:,24),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,18))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,140),Q(:,35),G1(:,:,:,118))
  call check_last_UV_W(l_switch,G1(:,:,:,118),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,130))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,191),Q(:,41),G1(:,:,:,119))
  call check_last_UV_W(l_switch,G1(:,:,:,119),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,131))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,141),Q(:,35),G1(:,:,:,120))
  call check_last_UV_W(l_switch,G1(:,:,:,120),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,132))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,245),Q(:,42),G1(:,:,:,121))
  call check_last_UV_W(l_switch,G1(:,:,:,121),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,133))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,192),Q(:,41),G1(:,:,:,122))
  call check_last_UV_W(l_switch,G1(:,:,:,122),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,134))
  call loop_UV_W(G0(:,:,:,88),Q(:,20),wf(:,246),Q(:,42),G1(:,:,:,123))
  call check_last_UV_W(l_switch,G1(:,:,:,123),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,135))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(42)+M(44)+M(45)+M(46)+M(50)+M(52)+M(56)+M(62)+M(63)+M(69)+M(75)+M(76)+M(80)+M(81)+M(82)+M(83)+M(86)+M(88)+M(94)+M(95) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(141)+M(214)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,1)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(41)+M(43)+M(47)+M(48)+M(49)+M(51)+M(59)+M(63)+M(68)+M(69)+M(71)+M(74)+M(75)+M(76)+M(81)+M(82)+M(88)+M(92)+M(94)+M(98) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(144)+M(200)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,2)
  Gcoeff = (c(4)*(M(25)-M(26)-M(27)+M(28)-M(29)+M(30)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42) &
    +M(43)-M(44)-M(45)-M(46)+M(47)+M(48)+M(49)-M(50)+M(51)-M(52)-M(56)+M(59)-M(62)+M(68)+M(71)+M(74)-M(80)-M(83)-M(86)+M(92)-M(95) &
    +M(98))+c(6)*(-M(141)+M(144)+M(200)-M(214)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,3)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(44)+M(45)+M(46)+M(47)+M(50)+M(52)+M(56)+M(59)+M(60)+M(61)+M(62)+M(63)+M(72)+M(75)+M(76)+M(83)+M(86)+M(88)+M(95)+M(99) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(142)+M(172)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,28)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(48)+M(49)+M(51)+M(60)+M(61)+M(63)+M(68)+M(71)+M(72)+M(74)+M(75)+M(76)+M(80)+M(88)+M(92)+M(98)+M(99) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(134)+M(202)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,29)
  Gcoeff = (c(4)*(-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(31)-M(32)+M(33)-M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42) &
    +M(43)-M(44)-M(45)-M(46)-M(47)+M(48)+M(49)-M(50)+M(51)-M(52)-M(56)-M(59)-M(62)+M(68)+M(71)+M(74)+M(80)-M(83)-M(86)+M(92)-M(95) &
    +M(98))+c(6)*(M(134)-M(142)-M(172)+M(202)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,30)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(141)+M(142)+M(172)-M(214)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,55)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)+M(61)-M(69)+M(72)+M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(134)-M(144)-M(200)+M(202)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,56)
  Gcoeff = (c(5)*(-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59)+M(80))+c(6)*(M(134)+M(141)-M(142)-M(144) &
    -M(172)-M(200)+M(202)+M(214)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,57)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(44)+M(45)+M(50)+M(51)+M(52)+M(56)+M(58)+M(62)+M(69)+M(74)+M(80)+M(81)+M(82)+M(83)+M(85)+M(86)+M(94)+M(96)+M(97) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(151)+M(212)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,4)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(43)+M(46)+M(47)+M(48)+M(49)+M(58)+M(59)+M(68)+M(69)+M(71)+M(81)+M(82)+M(85)+M(92)+M(94)+M(95)+M(96)+M(97)+M(98) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(143)+M(242)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,5)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42) &
    +M(43)-M(44)-M(45)+M(46)+M(47)+M(48)+M(49)-M(50)-M(51)-M(52)-M(56)+M(59)-M(62)+M(68)+M(71)-M(74)-M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(143)-M(151)-M(212)+M(242)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,6)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(44)+M(45)+M(47)+M(50)+M(51)+M(52)+M(56)+M(58)+M(59)+M(60)+M(61)+M(62)+M(72)+M(74)+M(83)+M(85)+M(86)+M(96)+M(97)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(152)+M(170)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,31)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(46)+M(48)+M(49)+M(58)+M(60)+M(61)+M(68)+M(71)+M(72)+M(80)+M(85)+M(92)+M(95)+M(96)+M(97)+M(98)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(133)+M(244)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,32)
  Gcoeff = (c(4)*(-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42) &
    +M(43)-M(44)-M(45)+M(46)-M(47)+M(48)+M(49)-M(50)-M(51)-M(52)-M(56)-M(59)-M(62)+M(68)+M(71)-M(74)+M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(133)-M(152)-M(170)+M(244)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,33)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(151)+M(152)+M(170)-M(212)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,58)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)+M(61)-M(69)+M(72)+M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(143)-M(242)+M(244)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,59)
  Gcoeff = (c(5)*(-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59)+M(80))+c(6)*(M(133)-M(143)+M(151)-M(152) &
    -M(170)+M(212)-M(242)+M(244)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,60)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(58) &
    -M(63)+M(74)-M(75)-M(76)+M(85)-M(88)-M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(141)+M(151)+M(212)-M(214)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,7)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(143)-M(144)-M(200)+M(242)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,8)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(141)+M(143)-M(144)-M(151) &
    -M(200)-M(212)+M(214)+M(242)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,9)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(58) &
    -M(63)+M(74)-M(75)-M(76)+M(85)-M(88)-M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(142)+M(152)+M(170)-M(172)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,34)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(134)-M(202)+M(244)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,35)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(133)-M(134)+M(142)-M(152) &
    -M(170)+M(172)-M(202)+M(244)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,36)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(141)-M(142)-M(151)+M(152) &
    +M(170)-M(172)-M(212)+M(214)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,61)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(133)-M(134)-M(143)+M(144) &
    +M(200)-M(202)-M(242)+M(244)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,62)
  Gcoeff = (c(6)*(M(133)-M(134)-M(141)+M(142)-M(143)+M(144)+M(151)-M(152)-M(170)+M(172)+M(200)-M(202)+M(212)-M(214)-M(242) &
    +M(244)))
  T3sum(1:1,2) = T3sum(1:1,2) + Gcoeff * G0tensor(:,63)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(5)-M(8)-M(10)-M(11)-M(13)-M(15)-M(18)-M(20)-M(22)-M(23)-M(25)-M(28)-M(30)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(42)+M(50)+M(52)+M(53)+M(62)+M(63)+M(65)+M(66)+M(67)+M(69)+M(75)+M(76)+M(78)+M(80)+M(81)+M(82)+M(86)+M(88)+M(90)+M(94) &
    +M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(183)+M(213)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,10)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(48)+M(50)+M(51)+M(52)+M(60)+M(62)+M(68)+M(70)+M(71)+M(72)+M(73)+M(74)+M(80)+M(82)+M(86)+M(93)+M(94) &
    +M(100)+M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(150)+M(194)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,11)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)-M(63)-M(65)-M(66)-M(67)+M(68)-M(69)+M(70)+M(71)+M(72)+M(73)+M(74)-M(75)-M(76)-M(78)-M(81)-M(88)-M(90)+M(93) &
    +M(100))+c(6)*(M(150)-M(183)+M(194)-M(213)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,12)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(42)+M(51)+M(53)+M(55)+M(63)+M(65)+M(66)+M(69)+M(74)+M(75)+M(76)+M(78)+M(79)+M(80)+M(81)+M(82)+M(88)+M(89)+M(91)+M(94) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(197)+M(210)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,13)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(10)-M(12)-M(14)-M(15)-M(18)-M(20)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(48)+M(55)+M(60)+M(67)+M(68)+M(70)+M(71)+M(72)+M(73)+M(79)+M(80)+M(82)+M(89)+M(90)+M(91)+M(93)+M(94) &
    +M(100)+M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(193)+M(228)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,14)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)-M(51) &
    -M(53)+M(60)-M(63)-M(65)-M(66)+M(67)+M(68)-M(69)+M(70)+M(71)+M(72)+M(73)-M(74)-M(75)-M(76)-M(78)-M(81)-M(88)+M(90)+M(93) &
    +M(100))+c(6)*(M(193)-M(197)-M(210)+M(228)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,15)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(50)+M(51)-M(52) &
    +M(55)-M(62)-M(67)+M(74)+M(79)-M(86)+M(89)-M(90)+M(91)+M(102)+M(104)-M(108)-M(110)-M(114)+M(116)-M(119)-M(120)+M(121)+M(122) &
    +M(123)-M(124))+c(6)*(-M(183)+M(197)+M(210)-M(213)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,16)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(11)-M(12)+M(17)-M(18)+M(19)-M(20)-M(21)+M(22)-M(23)+M(24)+M(25)-M(26)-M(29)+M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)+M(102)+M(104)-M(108)-M(110)-M(114)+M(116)-M(119)-M(120)+M(121)+M(122) &
    +M(123)-M(124))+c(6)*(-M(150)+M(193)-M(194)+M(228)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,17)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(150)+M(183)+M(193)-M(194) &
    -M(197)-M(210)+M(213)+M(228)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,18)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(10)-M(12)-M(14)-M(15)-M(18)-M(20)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(47)+M(50)+M(52)+M(53)+M(59)+M(60)+M(61)+M(62)+M(63)+M(65)+M(66)+M(67)+M(72)+M(75)+M(76)+M(78)+M(86)+M(88)+M(90)+M(99) &
    +M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(171)+M(184)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,37)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(47)+M(48)+M(50)+M(51)+M(52)+M(59)+M(61)+M(62)+M(68)+M(69)+M(70)+M(71)+M(73)+M(74)+M(81)+M(86)+M(93)+M(99) &
    +M(100)+M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(153)+M(188)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,38)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)-M(63)-M(65)-M(66)-M(67)+M(68)+M(69)+M(70)+M(71)-M(72)+M(73)+M(74)-M(75)-M(76)-M(78)+M(81)-M(88)-M(90)+M(93) &
    +M(100))+c(6)*(M(153)-M(171)-M(184)+M(188)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,39)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(47)+M(51)+M(53)+M(55)+M(59)+M(60)+M(61)+M(63)+M(65)+M(66)+M(72)+M(74)+M(75)+M(76)+M(78)+M(79)+M(88)+M(89)+M(91)+M(99) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(168)+M(198)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,40)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(5)-M(8)-M(10)-M(11)-M(13)-M(15)-M(18)-M(20)-M(22)-M(23)-M(25)-M(28)-M(30)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(47)+M(48)+M(55)+M(59)+M(61)+M(67)+M(68)+M(69)+M(70)+M(71)+M(73)+M(79)+M(81)+M(89)+M(90)+M(91)+M(93)+M(99) &
    +M(100)+M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(187)+M(231)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,41)
  Gcoeff = (c(4)*(M(3)-M(4)-M(5)+M(6)-M(13)+M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)-M(51) &
    -M(53)-M(60)-M(63)-M(65)-M(66)+M(67)+M(68)+M(69)+M(70)+M(71)-M(72)+M(73)-M(74)-M(75)-M(76)-M(78)+M(81)-M(88)+M(90)+M(93) &
    +M(100))+c(6)*(-M(168)+M(187)-M(198)+M(231)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,42)
  Gcoeff = (c(4)*(M(5)-M(6)-M(11)+M(12)-M(17)+M(18)-M(19)+M(20)+M(21)-M(22)+M(23)-M(24)-M(25)+M(26)+M(29)-M(30)-M(50)+M(51)-M(52) &
    +M(55)-M(62)-M(67)+M(74)+M(79)-M(86)+M(89)-M(90)+M(91)+M(102)-M(104)-M(108)-M(110)+M(114)+M(116)-M(119)+M(120)+M(121)+M(122) &
    -M(123)-M(124))+c(6)*(M(168)-M(171)-M(184)+M(198)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,43)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(11)+M(12)+M(17)-M(18)+M(19)-M(20)+M(21)-M(22)-M(23)+M(24)-M(25)+M(26)+M(29)-M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)+M(102)-M(104)-M(108)-M(110)+M(114)+M(116)-M(119)+M(120)+M(121)+M(122) &
    -M(123)-M(124))+c(6)*(-M(153)+M(187)-M(188)+M(231)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,44)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(153)-M(168)+M(171)+M(184) &
    +M(187)-M(188)-M(198)+M(231)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,45)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)+M(31)-M(32)-M(35)+M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(171)-M(183)+M(184)-M(213)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,64)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    -M(60)+M(61)+M(69)-M(72)-M(80)+M(81)-M(82)-M(94)+M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(150)+M(153)+M(188)-M(194)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,65)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(150)+M(153)-M(171)+M(183)-M(184) &
    +M(188)-M(194)+M(213)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,66)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(168)-M(197)+M(198)-M(210)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,67)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    -M(60)+M(61)+M(69)-M(72)-M(80)+M(81)-M(82)-M(94)+M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(187)-M(193)-M(228)+M(231)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,68)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(168)+M(187)-M(193)+M(197)-M(198) &
    +M(210)-M(228)+M(231)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,69)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(104)+M(114)+M(120)-M(123))+c(6)*(M(168)-M(171)+M(183)-M(184) &
    -M(197)+M(198)-M(210)+M(213)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,70)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(104)+M(114)+M(120)-M(123))+c(6)*(M(150)-M(153)+M(187)-M(188) &
    -M(193)+M(194)-M(228)+M(231)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,71)
  Gcoeff = (c(6)*(M(150)-M(153)-M(168)+M(171)-M(183)+M(184)+M(187)-M(188)-M(193)+M(194)+M(197)-M(198)+M(210)-M(213)-M(228) &
    +M(231)))
  T3sum(1:1,11) = T3sum(1:1,11) + Gcoeff * G0tensor(:,72)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(6)-M(7)-M(9)-M(12)-M(14)-M(16)-M(17)-M(19)-M(21)-M(24)-M(26)-M(27)-M(29)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(43)+M(47)+M(49)+M(53)+M(59)+M(60)+M(65)+M(66)+M(67)+M(70)+M(72)+M(73)+M(78)+M(82)+M(90)+M(92)+M(93)+M(94)+M(98) &
    +M(100)+M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(181)+M(237)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,19)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(46)+M(47)+M(53)+M(55)+M(58)+M(59)+M(60)+M(61)+M(65)+M(66)+M(72)+M(78)+M(79)+M(85)+M(89)+M(91)+M(95)+M(96)+M(97)+M(99) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(167)+M(240)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,22)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(7)-M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(25)+M(26)+M(29)-M(30)-M(43)+M(46)-M(49) &
    +M(55)+M(58)+M(61)-M(67)-M(70)-M(73)+M(79)-M(82)+M(85)+M(89)-M(90)+M(91)-M(92)-M(93)-M(94)+M(95)+M(96)+M(97)-M(98)+M(99) &
    -M(100))+c(6)*(M(167)-M(181)-M(237)+M(240)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,25)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(43)+M(44)+M(45)+M(46)+M(47)+M(49)+M(56)+M(59)+M(60)+M(70)+M(72)+M(73)+M(82)+M(83)+M(92)+M(93)+M(94)+M(95)+M(98) &
    +M(100)+M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(139)+M(238)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,20)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(9)-M(11)-M(13)-M(16)-M(17)-M(19)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(44)+M(45)+M(47)+M(55)+M(56)+M(58)+M(59)+M(60)+M(61)+M(67)+M(72)+M(79)+M(83)+M(85)+M(89)+M(90)+M(91)+M(96)+M(97)+M(99) &
    +M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(169)+M(230)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,23)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(7)+M(8)-M(9)+M(10)-M(13)+M(14)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)-M(43)-M(46)-M(49)+M(55) &
    +M(58)+M(61)+M(67)-M(70)-M(73)+M(79)-M(82)+M(85)+M(89)+M(90)+M(91)-M(92)-M(93)-M(94)-M(95)+M(96)+M(97)-M(98)+M(99)-M(100)) &
    +c(6)*(-M(139)+M(169)+M(230)-M(238)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,26)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)-M(105)-M(107)+M(108)+M(109)+M(110)-M(111)-M(112)-M(113)-M(118) &
    +M(119)+M(124))+c(6)*(M(139)-M(181)-M(237)+M(238)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,21)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)+M(44)+M(45)-M(46) &
    -M(53)+M(56)-M(65)-M(66)+M(67)-M(78)+M(83)+M(90)-M(95)+M(103)-M(105)-M(107)+M(108)+M(109)+M(110)-M(111)-M(112)-M(113)-M(118) &
    +M(119)+M(124))+c(6)*(-M(167)+M(169)+M(230)-M(240)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,24)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(139)-M(167)+M(169)+M(181)+M(230) &
    +M(237)-M(238)-M(240)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,27)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(9)-M(11)-M(13)-M(16)-M(17)-M(19)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(42)+M(43)+M(49)+M(53)+M(61)+M(65)+M(66)+M(67)+M(69)+M(70)+M(73)+M(78)+M(80)+M(81)+M(90)+M(92)+M(93)+M(98)+M(99) &
    +M(100)+M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(179)+M(247)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,46)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(42)+M(46)+M(53)+M(55)+M(58)+M(65)+M(66)+M(69)+M(78)+M(79)+M(80)+M(81)+M(82)+M(85)+M(89)+M(91)+M(94)+M(95)+M(96)+M(97) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(209)+M(239)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,49)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)-M(43)+M(46)-M(49) &
    +M(55)+M(58)-M(61)-M(67)-M(70)-M(73)+M(79)+M(82)+M(85)+M(89)-M(90)+M(91)-M(92)-M(93)+M(94)+M(95)+M(96)+M(97)-M(98)-M(99) &
    -M(100))+c(6)*(-M(179)+M(209)+M(239)-M(247)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,52)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(43)+M(44)+M(45)+M(46)+M(49)+M(56)+M(61)+M(69)+M(70)+M(73)+M(80)+M(81)+M(83)+M(92)+M(93)+M(95)+M(98)+M(99) &
    +M(100)+M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(137)+M(248)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,47)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(6)-M(7)-M(9)-M(12)-M(14)-M(16)-M(17)-M(19)-M(21)-M(24)-M(26)-M(27)-M(29)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(44)+M(45)+M(55)+M(56)+M(58)+M(67)+M(69)+M(79)+M(80)+M(81)+M(82)+M(83)+M(85)+M(89)+M(90)+M(91)+M(94)+M(96)+M(97) &
    +M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(211)+M(229)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,50)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(7)+M(8)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)+M(25)-M(26)-M(29)+M(30)-M(43)-M(46)-M(49)+M(55) &
    +M(58)-M(61)+M(67)-M(70)-M(73)+M(79)+M(82)+M(85)+M(89)+M(90)+M(91)-M(92)-M(93)+M(94)-M(95)+M(96)+M(97)-M(98)-M(99)-M(100)) &
    +c(6)*(-M(137)+M(211)+M(229)-M(248)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,53)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)-M(103)-M(105)+M(107)+M(108)-M(109)+M(110)-M(111)-M(112)+M(113)-M(118) &
    +M(119)+M(124))+c(6)*(M(137)-M(179)-M(247)+M(248)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,48)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)+M(44)+M(45)-M(46) &
    -M(53)+M(56)-M(65)-M(66)+M(67)-M(78)+M(83)+M(90)-M(95)-M(103)-M(105)+M(107)+M(108)-M(109)+M(110)-M(111)-M(112)+M(113)-M(118) &
    +M(119)+M(124))+c(6)*(-M(209)+M(211)+M(229)-M(239)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,51)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(137)+M(179)-M(209)+M(211)+M(229) &
    -M(239)+M(247)-M(248)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,54)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(31)+M(32)+M(35)-M(36)+M(42)-M(47)-M(59) &
    -M(60)+M(61)+M(69)-M(72)+M(80)+M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(179)-M(181)-M(237)+M(247)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,73)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)+M(42)-M(47)-M(59) &
    -M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(167)+M(209)+M(239)-M(240)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,76)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(6)*(-M(167)-M(179)+M(181)+M(209) &
    +M(237)+M(239)-M(240)-M(247)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,79)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    -M(60)+M(61)+M(69)-M(72)+M(80)+M(81)-M(82)-M(94)+M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(137)-M(139)-M(238)+M(248)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,74)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    -M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(169)+M(211)+M(229)-M(230)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,77)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(6)*(-M(137)+M(139)-M(169)+M(211) &
    +M(229)-M(230)+M(238)-M(248)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,80)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(6)*(M(137)-M(139)-M(179)+M(181) &
    +M(237)-M(238)-M(247)+M(248)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,75)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(6)*(M(167)-M(169)-M(209)+M(211) &
    +M(229)-M(230)-M(239)+M(240)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,78)
  Gcoeff = (c(6)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248)))
  T3sum(1:1,13) = T3sum(1:1,13) + Gcoeff * G0tensor(:,81)
  Gcoeff = (c(4)*(M(25)-M(26)-M(27)+M(28)-M(29)+M(30)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)-M(41)-M(42) &
    +M(43)+M(44)+M(45)+M(46)+M(47)-M(48)+M(49)-M(50)-M(51)-M(52)+M(56)+M(59)-M(62)-M(68)-M(71)-M(74)-M(80)+M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(139)-M(150)-M(194)+M(238))) * den(11)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,10)
  Gcoeff = (c(4)*(-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(31)-M(32)+M(33)-M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)-M(41)+M(42) &
    +M(43)+M(44)+M(45)+M(46)-M(47)-M(48)+M(49)-M(50)-M(51)-M(52)+M(56)-M(59)-M(62)-M(68)-M(71)-M(74)+M(80)+M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(137)-M(153)-M(188)+M(248))) * den(11)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,64)
  Gcoeff = (c(5)*(-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59)+M(80))+c(6)*(M(137)-M(139)+M(150)-M(153) &
    -M(188)+M(194)-M(238)+M(248))) * den(11)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,118)
  Gcoeff = (c(4)*(M(25)-M(26)-M(27)+M(28)-M(29)+M(30)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)-M(41)-M(42) &
    -M(43)+M(44)+M(45)-M(46)+M(47)-M(48)-M(49)+M(50)+M(51)+M(52)+M(56)+M(59)+M(62)-M(68)-M(71)+M(74)-M(80)+M(83)+M(86)-M(92)-M(95) &
    -M(98))+c(6)*(-M(133)+M(152)+M(170)-M(244))) * den(11)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,11)
  Gcoeff = (c(4)*(-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(31)-M(32)+M(33)-M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)-M(41)+M(42) &
    -M(43)+M(44)+M(45)-M(46)-M(47)-M(48)-M(49)+M(50)+M(51)+M(52)+M(56)-M(59)+M(62)-M(68)-M(71)+M(74)+M(80)+M(83)+M(86)-M(92)-M(95) &
    -M(98))+c(6)*(-M(143)+M(151)+M(212)-M(242))) * den(11)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,65)
  Gcoeff = (c(5)*(-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59)+M(80))+c(6)*(M(133)-M(143)+M(151)-M(152) &
    -M(170)+M(212)-M(242)+M(244))) * den(11)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,119)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(133)-M(139)+M(150)+M(152) &
    +M(170)+M(194)-M(238)-M(244))) * den(11)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,12)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(137)-M(143)+M(151)+M(153) &
    +M(188)+M(212)-M(242)-M(248))) * den(11)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,66)
  Gcoeff = (c(6)*(M(133)-M(137)+M(139)-M(143)-M(150)+M(151)-M(152)+M(153)-M(170)+M(188)-M(194)+M(212)+M(238)-M(242)+M(244) &
    -M(248))) * den(11)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,120)
  Gcoeff = (c(4)*(M(25)-M(26)-M(27)+M(28)-M(29)+M(30)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42) &
    +M(43)-M(44)-M(45)-M(46)+M(47)+M(48)+M(49)-M(50)+M(51)-M(52)-M(56)+M(59)-M(62)+M(68)+M(71)+M(74)-M(80)-M(83)-M(86)+M(92)-M(95) &
    +M(98))+c(6)*(-M(141)+M(144)+M(200)-M(214))) * den(11)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,13)
  Gcoeff = (c(4)*(-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(31)-M(32)+M(33)-M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42) &
    +M(43)-M(44)-M(45)-M(46)-M(47)+M(48)+M(49)-M(50)+M(51)-M(52)-M(56)-M(59)-M(62)+M(68)+M(71)+M(74)+M(80)-M(83)-M(86)+M(92)-M(95) &
    +M(98))+c(6)*(M(134)-M(142)-M(172)+M(202))) * den(11)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,67)
  Gcoeff = (c(5)*(-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59)+M(80))+c(6)*(M(134)+M(141)-M(142)-M(144) &
    -M(172)-M(200)+M(202)+M(214))) * den(11)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,121)
  Gcoeff = (c(4)*(M(25)-M(26)-M(27)+M(28)-M(29)+M(30)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)-M(41)-M(42) &
    +M(43)+M(44)+M(45)+M(46)+M(47)-M(48)+M(49)-M(50)-M(51)-M(52)+M(56)+M(59)-M(62)-M(68)-M(71)-M(74)-M(80)+M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(139)-M(150)-M(194)+M(238))) * den(11)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,14)
  Gcoeff = (c(4)*(-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(31)-M(32)+M(33)-M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)-M(41)+M(42) &
    +M(43)+M(44)+M(45)+M(46)-M(47)-M(48)+M(49)-M(50)-M(51)-M(52)+M(56)-M(59)-M(62)-M(68)-M(71)-M(74)+M(80)+M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(137)-M(153)-M(188)+M(248))) * den(11)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,68)
  Gcoeff = (c(5)*(-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59)+M(80))+c(6)*(M(137)-M(139)+M(150)-M(153) &
    -M(188)+M(194)-M(238)+M(248))) * den(11)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,122)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(M(139)+M(141)-M(144)-M(150) &
    -M(194)-M(200)+M(214)+M(238))) * den(11)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,15)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(-M(134)+M(137)+M(142)-M(153) &
    +M(172)-M(188)-M(202)+M(248))) * den(11)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,69)
  Gcoeff = (c(6)*(-M(134)+M(137)-M(139)-M(141)+M(142)+M(144)+M(150)-M(153)+M(172)-M(188)+M(194)+M(200)-M(202)-M(214)-M(238) &
    +M(248))) * den(11)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,123)
  Gcoeff = (c(4)*(M(25)-M(26)-M(27)+M(28)-M(29)+M(30)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42) &
    +M(43)-M(44)-M(45)-M(46)+M(47)+M(48)+M(49)-M(50)+M(51)-M(52)-M(56)+M(59)-M(62)+M(68)+M(71)+M(74)-M(80)-M(83)-M(86)+M(92)-M(95) &
    +M(98))+c(6)*(-M(141)+M(144)+M(200)-M(214))) * den(11)
  T3sum(1:5,2) = T3sum(1:5,2) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(4)*(-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(31)-M(32)+M(33)-M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42) &
    +M(43)-M(44)-M(45)-M(46)-M(47)+M(48)+M(49)-M(50)+M(51)-M(52)-M(56)-M(59)-M(62)+M(68)+M(71)+M(74)+M(80)-M(83)-M(86)+M(92)-M(95) &
    +M(98))+c(6)*(M(134)-M(142)-M(172)+M(202))) * den(11)
  T3sum(1:5,2) = T3sum(1:5,2) + Gcoeff * G1tensor(:,55)
  Gcoeff = (c(5)*(-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59)+M(80))+c(6)*(M(134)+M(141)-M(142)-M(144) &
    -M(172)-M(200)+M(202)+M(214))) * den(11)
  T3sum(1:5,2) = T3sum(1:5,2) + Gcoeff * G1tensor(:,109)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42) &
    +M(43)-M(44)-M(45)+M(46)+M(47)+M(48)+M(49)-M(50)-M(51)-M(52)-M(56)+M(59)-M(62)+M(68)+M(71)-M(74)-M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(143)-M(151)-M(212)+M(242))) * den(11)
  T3sum(1:5,2) = T3sum(1:5,2) + Gcoeff * G1tensor(:,2)
  Gcoeff = (c(4)*(-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42) &
    +M(43)-M(44)-M(45)+M(46)-M(47)+M(48)+M(49)-M(50)-M(51)-M(52)-M(56)-M(59)-M(62)+M(68)+M(71)-M(74)+M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(133)-M(152)-M(170)+M(244))) * den(11)
  T3sum(1:5,2) = T3sum(1:5,2) + Gcoeff * G1tensor(:,56)
  Gcoeff = (c(5)*(-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59)+M(80))+c(6)*(M(133)-M(143)+M(151)-M(152) &
    -M(170)+M(212)-M(242)+M(244))) * den(11)
  T3sum(1:5,2) = T3sum(1:5,2) + Gcoeff * G1tensor(:,110)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(141)+M(143)-M(144)-M(151) &
    -M(200)-M(212)+M(214)+M(242))) * den(11)
  T3sum(1:5,2) = T3sum(1:5,2) + Gcoeff * G1tensor(:,3)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(133)-M(134)+M(142)-M(152) &
    -M(170)+M(172)-M(202)+M(244))) * den(11)
  T3sum(1:5,2) = T3sum(1:5,2) + Gcoeff * G1tensor(:,57)
  Gcoeff = (c(6)*(M(133)-M(134)-M(141)+M(142)-M(143)+M(144)+M(151)-M(152)-M(170)+M(172)+M(200)-M(202)+M(212)-M(214)-M(242) &
    +M(244))) * den(11)
  T3sum(1:5,2) = T3sum(1:5,2) + Gcoeff * G1tensor(:,111)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)-M(41)-M(48)-M(51) &
    +M(53)+M(60)-M(63)+M(65)+M(66)+M(67)-M(68)-M(69)+M(70)-M(71)+M(72)+M(73)-M(74)-M(75)-M(76)+M(78)-M(81)-M(88)+M(90)+M(93) &
    +M(100))+c(6)*(-M(144)+M(181)-M(200)+M(237))) * den(40)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,22)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)-M(48)-M(51) &
    +M(53)-M(60)-M(63)+M(65)+M(66)+M(67)-M(68)+M(69)+M(70)-M(71)-M(72)+M(73)-M(74)-M(75)-M(76)+M(78)+M(81)-M(88)+M(90)+M(93) &
    +M(100))+c(6)*(-M(134)+M(179)-M(202)+M(247))) * den(40)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,76)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(134)+M(144)+M(179)-M(181)+M(200) &
    -M(202)-M(237)+M(247))) * den(40)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,130)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)-M(41)-M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)+M(66)-M(67)-M(68)-M(69)-M(70)-M(71)+M(72)-M(73)+M(74)+M(75)+M(76)+M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(168)-M(187)+M(198)-M(231))) * den(40)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,23)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)-M(48)+M(51) &
    +M(53)-M(60)+M(63)+M(65)+M(66)-M(67)-M(68)+M(69)-M(70)-M(71)-M(72)-M(73)+M(74)+M(75)+M(76)+M(78)+M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(-M(193)+M(197)+M(210)-M(228))) * den(40)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,77)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(168)+M(187)-M(193)+M(197)-M(198) &
    +M(210)-M(228)+M(231))) * den(40)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,131)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(144)+M(168)-M(181)-M(187) &
    +M(198)+M(200)-M(231)-M(237))) * den(40)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,24)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(134)-M(179)-M(193)+M(197) &
    +M(202)+M(210)-M(228)-M(247))) * den(40)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,78)
  Gcoeff = (c(6)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(40)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,132)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)-M(44)-M(45)-M(46) &
    +M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95)+M(103)-M(105)-M(107)+M(108)+M(109)+M(110)-M(111)-M(112)-M(113)-M(118) &
    +M(119)+M(124))+c(6)*(-M(141)+M(183)+M(213)-M(214))) * den(33)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,25)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)-M(45)-M(46) &
    +M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95)-M(103)-M(105)+M(107)+M(108)-M(109)+M(110)-M(111)-M(112)+M(113)-M(118) &
    +M(119)+M(124))+c(6)*(-M(142)+M(171)-M(172)+M(184))) * den(33)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,79)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(6)*(M(141)-M(142)+M(171)-M(172) &
    -M(183)+M(184)-M(213)+M(214))) * den(33)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,133)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)-M(44)-M(45)+M(46) &
    +M(53)-M(56)+M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)-M(110)+M(111)+M(112)-M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(209)-M(211)-M(229)+M(239))) * den(33)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,26)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)-M(45)+M(46) &
    +M(53)-M(56)+M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)-M(103)+M(105)+M(107)-M(108)-M(109)-M(110)+M(111)+M(112)+M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(167)-M(169)-M(230)+M(240))) * den(33)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,80)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(6)*(M(167)-M(169)-M(209)+M(211) &
    +M(229)-M(230)-M(239)+M(240))) * den(33)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,134)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(141)-M(183)+M(209) &
    -M(211)-M(213)+M(214)-M(229)+M(239))) * den(33)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,27)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(142)+M(167)-M(169) &
    -M(171)+M(172)-M(184)-M(230)+M(240))) * den(33)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,81)
  Gcoeff = (c(6)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(33)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,135)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(7)-M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(25)+M(26)+M(29)-M(30)-M(43)-M(46)-M(49) &
    +M(55)-M(58)+M(61)+M(67)+M(70)+M(73)+M(79)-M(82)-M(85)+M(89)+M(90)+M(91)-M(92)+M(93)-M(94)-M(95)-M(96)-M(97)-M(98)+M(99) &
    +M(100))+c(6)*(-M(143)+M(187)+M(231)-M(242))) * den(45)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,28)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)-M(43)-M(46)-M(49) &
    +M(55)-M(58)-M(61)+M(67)+M(70)+M(73)+M(79)+M(82)-M(85)+M(89)+M(90)+M(91)-M(92)+M(93)+M(94)-M(95)-M(96)-M(97)-M(98)-M(99) &
    +M(100))+c(6)*(-M(133)+M(193)+M(228)-M(244))) * den(45)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,82)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(6)*(-M(133)+M(143)-M(187)+M(193) &
    +M(228)-M(231)+M(242)-M(244))) * den(45)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,136)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(7)-M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(25)+M(26)+M(29)-M(30)-M(43)+M(46)-M(49) &
    +M(55)+M(58)+M(61)-M(67)-M(70)-M(73)+M(79)-M(82)+M(85)+M(89)-M(90)+M(91)-M(92)-M(93)-M(94)+M(95)+M(96)+M(97)-M(98)+M(99) &
    -M(100))+c(6)*(M(167)-M(181)-M(237)+M(240))) * den(45)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,29)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)-M(43)+M(46)-M(49) &
    +M(55)+M(58)-M(61)-M(67)-M(70)-M(73)+M(79)+M(82)+M(85)+M(89)-M(90)+M(91)-M(92)-M(93)+M(94)+M(95)+M(96)+M(97)-M(98)-M(99) &
    -M(100))+c(6)*(-M(179)+M(209)+M(239)-M(247))) * den(45)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,83)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(6)*(-M(167)-M(179)+M(181)+M(209) &
    +M(237)+M(239)-M(240)-M(247))) * den(45)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,137)
  Gcoeff = (c(5)*(M(46)+M(58)-M(67)-M(70)-M(73)+M(85)-M(90)-M(93)+M(95)+M(96)+M(97)-M(100))+c(6)*(M(143)+M(167)-M(181)-M(187) &
    -M(231)-M(237)+M(240)+M(242))) * den(45)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,30)
  Gcoeff = (c(5)*(M(46)+M(58)-M(67)-M(70)-M(73)+M(85)-M(90)-M(93)+M(95)+M(96)+M(97)-M(100))+c(6)*(M(133)-M(179)-M(193)+M(209) &
    -M(228)+M(239)+M(244)-M(247))) * den(45)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,84)
  Gcoeff = (c(6)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(45)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,138)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)-M(102)+M(104)+M(108)+M(110)-M(114)-M(116)+M(119)-M(120)-M(121)-M(122) &
    +M(123)+M(124))+c(6)*(-M(151)+M(211)-M(212)+M(229))) * den(37)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,31)
  Gcoeff = (c(4)*(M(5)-M(6)-M(11)+M(12)-M(17)+M(18)-M(19)+M(20)+M(21)-M(22)+M(23)-M(24)-M(25)+M(26)+M(29)-M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)-M(102)-M(104)+M(108)+M(110)+M(114)-M(116)+M(119)+M(120)-M(121)-M(122) &
    -M(123)+M(124))+c(6)*(-M(152)+M(169)-M(170)+M(230))) * den(37)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,85)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(104)+M(114)+M(120)-M(123))+c(6)*(M(151)-M(152)+M(169)-M(170) &
    -M(211)+M(212)-M(229)+M(230))) * den(37)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,139)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(50)+M(51)-M(52) &
    +M(55)-M(62)-M(67)+M(74)+M(79)-M(86)+M(89)-M(90)+M(91)+M(102)+M(104)-M(108)-M(110)-M(114)+M(116)-M(119)-M(120)+M(121)+M(122) &
    +M(123)-M(124))+c(6)*(-M(183)+M(197)+M(210)-M(213))) * den(37)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,32)
  Gcoeff = (c(4)*(M(5)-M(6)-M(11)+M(12)-M(17)+M(18)-M(19)+M(20)+M(21)-M(22)+M(23)-M(24)-M(25)+M(26)+M(29)-M(30)-M(50)+M(51)-M(52) &
    +M(55)-M(62)-M(67)+M(74)+M(79)-M(86)+M(89)-M(90)+M(91)+M(102)-M(104)-M(108)-M(110)+M(114)+M(116)-M(119)+M(120)+M(121)+M(122) &
    -M(123)-M(124))+c(6)*(M(168)-M(171)-M(184)+M(198))) * den(37)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,86)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(104)+M(114)+M(120)-M(123))+c(6)*(M(168)-M(171)+M(183)-M(184) &
    -M(197)+M(198)-M(210)+M(213))) * den(37)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,140)
  Gcoeff = (c(5)*(M(51)-M(67)+M(74)-M(90)+M(102)-M(108)-M(110)+M(116)-M(119)+M(121)+M(122)-M(124))+c(6)*(M(151)-M(183)+M(197) &
    +M(210)-M(211)+M(212)-M(213)-M(229))) * den(37)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,33)
  Gcoeff = (c(5)*(M(51)-M(67)+M(74)-M(90)+M(102)-M(108)-M(110)+M(116)-M(119)+M(121)+M(122)-M(124))+c(6)*(M(152)+M(168)-M(169) &
    +M(170)-M(171)-M(184)+M(198)-M(230))) * den(37)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,87)
  Gcoeff = (c(6)*(-M(151)+M(152)+M(168)-M(169)+M(170)-M(171)+M(183)-M(184)-M(197)+M(198)-M(210)+M(211)-M(212)+M(213)+M(229) &
    -M(230))) * den(37)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,141)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(42)+M(51)+M(53)+M(55)+M(63)+M(65)+M(66)+M(69)+M(74)+M(75)+M(76)+M(78)+M(79)+M(80)+M(81)+M(82)+M(88)+M(89)+M(91)+M(94) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(197)+M(210)))
  T4sum(1:15,6) = T4sum(1:15,6) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(47)+M(51)+M(53)+M(55)+M(59)+M(60)+M(61)+M(63)+M(65)+M(66)+M(72)+M(74)+M(75)+M(76)+M(78)+M(79)+M(88)+M(89)+M(91)+M(99) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(168)+M(198)))
  T4sum(1:15,6) = T4sum(1:15,6) + Gcoeff * G2tensor(:,52)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(168)-M(197)+M(198)-M(210)))
  T4sum(1:15,6) = T4sum(1:15,6) + Gcoeff * G2tensor(:,97)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(42)+M(46)+M(53)+M(55)+M(58)+M(65)+M(66)+M(69)+M(78)+M(79)+M(80)+M(81)+M(82)+M(85)+M(89)+M(91)+M(94)+M(95)+M(96)+M(97) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(209)+M(239)))
  T4sum(1:15,6) = T4sum(1:15,6) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(46)+M(47)+M(53)+M(55)+M(58)+M(59)+M(60)+M(61)+M(65)+M(66)+M(72)+M(78)+M(79)+M(85)+M(89)+M(91)+M(95)+M(96)+M(97)+M(99) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(167)+M(240)))
  T4sum(1:15,6) = T4sum(1:15,6) + Gcoeff * G2tensor(:,53)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(167)-M(209)-M(239)+M(240)))
  T4sum(1:15,6) = T4sum(1:15,6) + Gcoeff * G2tensor(:,98)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(197)+M(209)-M(210)+M(239)))
  T4sum(1:15,6) = T4sum(1:15,6) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(167)-M(168)-M(198)+M(240)))
  T4sum(1:15,6) = T4sum(1:15,6) + Gcoeff * G2tensor(:,54)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(167)-M(168)+M(197)-M(198) &
    -M(209)+M(210)-M(239)+M(240)))
  T4sum(1:15,6) = T4sum(1:15,6) + Gcoeff * G2tensor(:,99)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)-M(63)-M(65)-M(66)-M(67)+M(68)-M(69)+M(70)+M(71)+M(72)+M(73)+M(74)-M(75)-M(76)-M(78)-M(81)-M(88)-M(90)+M(93) &
    +M(100))+c(6)*(M(150)-M(183)+M(194)-M(213))) * den(40)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,16)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)-M(41)-M(48)-M(51) &
    +M(53)+M(60)-M(63)+M(65)+M(66)+M(67)-M(68)-M(69)+M(70)-M(71)+M(72)+M(73)-M(74)-M(75)-M(76)+M(78)-M(81)-M(88)+M(90)+M(93) &
    +M(100))+c(6)*(-M(144)+M(181)-M(200)+M(237))) * den(40)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,17)
  Gcoeff = (c(5)*(-M(41)-M(48)-M(51)+M(53)+M(65)+M(66)+M(67)-M(68)-M(71)-M(74)+M(78)+M(90))+c(6)*(-M(144)-M(150)+M(181)+M(183) &
    -M(194)-M(200)+M(213)+M(237))) * den(40)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,18)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)-M(63)-M(65)-M(66)-M(67)+M(68)+M(69)+M(70)+M(71)-M(72)+M(73)+M(74)-M(75)-M(76)-M(78)+M(81)-M(88)-M(90)+M(93) &
    +M(100))+c(6)*(M(153)-M(171)-M(184)+M(188))) * den(40)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,70)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)-M(48)-M(51) &
    +M(53)-M(60)-M(63)+M(65)+M(66)+M(67)-M(68)+M(69)+M(70)-M(71)-M(72)+M(73)-M(74)-M(75)-M(76)+M(78)+M(81)-M(88)+M(90)+M(93) &
    +M(100))+c(6)*(-M(134)+M(179)-M(202)+M(247))) * den(40)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,71)
  Gcoeff = (c(5)*(-M(41)-M(48)-M(51)+M(53)+M(65)+M(66)+M(67)-M(68)-M(71)-M(74)+M(78)+M(90))+c(6)*(-M(134)-M(153)+M(171)+M(179) &
    +M(184)-M(188)-M(202)+M(247))) * den(40)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,72)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(150)+M(153)-M(171)+M(183)-M(184) &
    +M(188)-M(194)+M(213))) * den(40)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,124)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(134)+M(144)+M(179)-M(181)+M(200) &
    -M(202)-M(237)+M(247))) * den(40)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,125)
  Gcoeff = (c(6)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(40)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,126)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)-M(63)-M(65)-M(66)-M(67)+M(68)-M(69)+M(70)+M(71)+M(72)+M(73)+M(74)-M(75)-M(76)-M(78)-M(81)-M(88)-M(90)+M(93) &
    +M(100))+c(6)*(M(150)-M(183)+M(194)-M(213))) * den(40)
  T3sum(1:5,11) = T3sum(1:5,11) + Gcoeff * G1tensor(:,4)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)-M(51) &
    -M(53)+M(60)-M(63)-M(65)-M(66)+M(67)+M(68)-M(69)+M(70)+M(71)+M(72)+M(73)-M(74)-M(75)-M(76)-M(78)-M(81)-M(88)+M(90)+M(93) &
    +M(100))+c(6)*(M(193)-M(197)-M(210)+M(228))) * den(40)
  T3sum(1:5,11) = T3sum(1:5,11) + Gcoeff * G1tensor(:,5)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(150)+M(183)+M(193)-M(194) &
    -M(197)-M(210)+M(213)+M(228))) * den(40)
  T3sum(1:5,11) = T3sum(1:5,11) + Gcoeff * G1tensor(:,6)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)-M(63)-M(65)-M(66)-M(67)+M(68)+M(69)+M(70)+M(71)-M(72)+M(73)+M(74)-M(75)-M(76)-M(78)+M(81)-M(88)-M(90)+M(93) &
    +M(100))+c(6)*(M(153)-M(171)-M(184)+M(188))) * den(40)
  T3sum(1:5,11) = T3sum(1:5,11) + Gcoeff * G1tensor(:,58)
  Gcoeff = (c(4)*(M(3)-M(4)-M(5)+M(6)-M(13)+M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)-M(51) &
    -M(53)-M(60)-M(63)-M(65)-M(66)+M(67)+M(68)+M(69)+M(70)+M(71)-M(72)+M(73)-M(74)-M(75)-M(76)-M(78)+M(81)-M(88)+M(90)+M(93) &
    +M(100))+c(6)*(-M(168)+M(187)-M(198)+M(231))) * den(40)
  T3sum(1:5,11) = T3sum(1:5,11) + Gcoeff * G1tensor(:,59)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(153)-M(168)+M(171)+M(184) &
    +M(187)-M(188)-M(198)+M(231))) * den(40)
  T3sum(1:5,11) = T3sum(1:5,11) + Gcoeff * G1tensor(:,60)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(150)+M(153)-M(171)+M(183)-M(184) &
    +M(188)-M(194)+M(213))) * den(40)
  T3sum(1:5,11) = T3sum(1:5,11) + Gcoeff * G1tensor(:,112)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(168)+M(187)-M(193)+M(197)-M(198) &
    +M(210)-M(228)+M(231))) * den(40)
  T3sum(1:5,11) = T3sum(1:5,11) + Gcoeff * G1tensor(:,113)
  Gcoeff = (c(6)*(M(150)-M(153)-M(168)+M(171)-M(183)+M(184)+M(187)-M(188)-M(193)+M(194)+M(197)-M(198)+M(210)-M(213)-M(228) &
    +M(231))) * den(40)
  T3sum(1:5,11) = T3sum(1:5,11) + Gcoeff * G1tensor(:,114)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(7)+M(8)-M(9)+M(10)-M(13)+M(14)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)+M(43)+M(46)+M(49)-M(55) &
    +M(58)+M(61)-M(67)-M(70)-M(73)-M(79)-M(82)+M(85)-M(89)-M(90)-M(91)+M(92)-M(93)-M(94)+M(95)+M(96)+M(97)+M(98)+M(99)-M(100)) &
    +c(6)*(M(133)-M(193)-M(228)+M(244))) * den(45)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,34)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(7)+M(8)-M(9)+M(10)-M(13)+M(14)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)-M(43)-M(46)-M(49)+M(55) &
    +M(58)+M(61)+M(67)-M(70)-M(73)+M(79)-M(82)+M(85)+M(89)+M(90)+M(91)-M(92)-M(93)-M(94)-M(95)+M(96)+M(97)-M(98)+M(99)-M(100)) &
    +c(6)*(-M(139)+M(169)+M(230)-M(238))) * den(45)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,35)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(55)+M(67)+M(79)+M(89)+M(90)+M(91)-M(92)-M(95)-M(98))+c(6)*(-M(133)-M(139)+M(169)+M(193) &
    +M(228)+M(230)-M(238)-M(244))) * den(45)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,36)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(7)+M(8)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)+M(25)-M(26)-M(29)+M(30)+M(43)+M(46)+M(49)-M(55) &
    +M(58)-M(61)-M(67)-M(70)-M(73)-M(79)+M(82)+M(85)-M(89)-M(90)-M(91)+M(92)-M(93)+M(94)+M(95)+M(96)+M(97)+M(98)-M(99)-M(100)) &
    +c(6)*(M(143)-M(187)-M(231)+M(242))) * den(45)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,88)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(7)+M(8)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)+M(25)-M(26)-M(29)+M(30)-M(43)-M(46)-M(49)+M(55) &
    +M(58)-M(61)+M(67)-M(70)-M(73)+M(79)+M(82)+M(85)+M(89)+M(90)+M(91)-M(92)-M(93)+M(94)-M(95)+M(96)+M(97)-M(98)-M(99)-M(100)) &
    +c(6)*(-M(137)+M(211)+M(229)-M(248))) * den(45)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,89)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(55)+M(67)+M(79)+M(89)+M(90)+M(91)-M(92)-M(95)-M(98))+c(6)*(-M(137)-M(143)+M(187)+M(211) &
    +M(229)+M(231)-M(242)-M(248))) * den(45)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,90)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(6)*(-M(133)+M(143)-M(187)+M(193) &
    +M(228)-M(231)+M(242)-M(244))) * den(45)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,142)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(6)*(-M(137)+M(139)-M(169)+M(211) &
    +M(229)-M(230)+M(238)-M(248))) * den(45)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,143)
  Gcoeff = (c(6)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(45)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,144)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(197)+M(209)-M(210)+M(239))) * den(22)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,37)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(58) &
    -M(63)+M(74)-M(75)-M(76)+M(85)-M(88)-M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(141)+M(151)+M(212)-M(214))) * den(22)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,38)
  Gcoeff = (c(5)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(6)*(-M(141)+M(151)+M(197) &
    -M(209)+M(210)+M(212)-M(214)-M(239))) * den(22)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,39)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(167)-M(168)-M(198)+M(240))) * den(22)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,91)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(58) &
    -M(63)+M(74)-M(75)-M(76)+M(85)-M(88)-M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(142)+M(152)+M(170)-M(172))) * den(22)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,92)
  Gcoeff = (c(5)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(6)*(-M(142)+M(152)-M(167) &
    +M(168)+M(170)-M(172)+M(198)-M(240))) * den(22)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,93)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(167)-M(168)+M(197)-M(198) &
    -M(209)+M(210)-M(239)+M(240))) * den(22)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,145)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(141)-M(142)-M(151)+M(152) &
    +M(170)-M(172)-M(212)+M(214))) * den(22)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,146)
  Gcoeff = (c(6)*(M(141)-M(142)-M(151)+M(152)-M(167)+M(168)+M(170)-M(172)-M(197)+M(198)+M(209)-M(210)-M(212)+M(214)+M(239) &
    -M(240))) * den(22)
  T3sum(1:5,36) = T3sum(1:5,36) + Gcoeff * G1tensor(:,147)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(44)+M(45)+M(50)+M(51)+M(52)+M(56)+M(58)+M(62)+M(69)+M(74)+M(80)+M(81)+M(82)+M(83)+M(85)+M(86)+M(94)+M(96)+M(97) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(151)+M(212)))
  T4sum(1:15,33) = T4sum(1:15,33) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(6)-M(7)-M(9)-M(12)-M(14)-M(16)-M(17)-M(19)-M(21)-M(24)-M(26)-M(27)-M(29)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(44)+M(45)+M(55)+M(56)+M(58)+M(67)+M(69)+M(79)+M(80)+M(81)+M(82)+M(83)+M(85)+M(89)+M(90)+M(91)+M(94)+M(96)+M(97) &
    +M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(211)+M(229)))
  T4sum(1:15,33) = T4sum(1:15,33) + Gcoeff * G2tensor(:,11)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)-M(102)+M(104)+M(108)+M(110)-M(114)-M(116)+M(119)-M(120)-M(121)-M(122) &
    +M(123)+M(124))+c(6)*(-M(151)+M(211)-M(212)+M(229)))
  T4sum(1:15,33) = T4sum(1:15,33) + Gcoeff * G2tensor(:,12)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(44)+M(45)+M(47)+M(50)+M(51)+M(52)+M(56)+M(58)+M(59)+M(60)+M(61)+M(62)+M(72)+M(74)+M(83)+M(85)+M(86)+M(96)+M(97)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(152)+M(170)))
  T4sum(1:15,33) = T4sum(1:15,33) + Gcoeff * G2tensor(:,55)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(9)-M(11)-M(13)-M(16)-M(17)-M(19)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(44)+M(45)+M(47)+M(55)+M(56)+M(58)+M(59)+M(60)+M(61)+M(67)+M(72)+M(79)+M(83)+M(85)+M(89)+M(90)+M(91)+M(96)+M(97)+M(99) &
    +M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(169)+M(230)))
  T4sum(1:15,33) = T4sum(1:15,33) + Gcoeff * G2tensor(:,56)
  Gcoeff = (c(4)*(M(5)-M(6)-M(11)+M(12)-M(17)+M(18)-M(19)+M(20)+M(21)-M(22)+M(23)-M(24)-M(25)+M(26)+M(29)-M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)-M(102)-M(104)+M(108)+M(110)+M(114)-M(116)+M(119)+M(120)-M(121)-M(122) &
    -M(123)+M(124))+c(6)*(-M(152)+M(169)-M(170)+M(230)))
  T4sum(1:15,33) = T4sum(1:15,33) + Gcoeff * G2tensor(:,57)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(151)+M(152)+M(170)-M(212)))
  T4sum(1:15,33) = T4sum(1:15,33) + Gcoeff * G2tensor(:,100)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)+M(103)-M(104)-M(107)+M(109)-M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(169)-M(211)-M(229)+M(230)))
  T4sum(1:15,33) = T4sum(1:15,33) + Gcoeff * G2tensor(:,101)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(104)+M(114)+M(120)-M(123))+c(6)*(M(151)-M(152)+M(169)-M(170) &
    -M(211)+M(212)-M(229)+M(230)))
  T4sum(1:15,33) = T4sum(1:15,33) + Gcoeff * G2tensor(:,102)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(7)-M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(25)+M(26)+M(29)-M(30)-M(43)+M(46)-M(49) &
    +M(55)+M(58)+M(61)-M(67)-M(70)-M(73)+M(79)-M(82)+M(85)+M(89)-M(90)+M(91)-M(92)-M(93)-M(94)+M(95)+M(96)+M(97)-M(98)+M(99) &
    -M(100))+c(6)*(M(167)-M(181)-M(237)+M(240))) * den(45)
  T3sum(1:5,13) = T3sum(1:5,13) + Gcoeff * G1tensor(:,40)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(7)+M(8)-M(9)+M(10)-M(13)+M(14)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)-M(43)-M(46)-M(49)+M(55) &
    +M(58)+M(61)+M(67)-M(70)-M(73)+M(79)-M(82)+M(85)+M(89)+M(90)+M(91)-M(92)-M(93)-M(94)-M(95)+M(96)+M(97)-M(98)+M(99)-M(100)) &
    +c(6)*(-M(139)+M(169)+M(230)-M(238))) * den(45)
  T3sum(1:5,13) = T3sum(1:5,13) + Gcoeff * G1tensor(:,41)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(139)-M(167)+M(169)+M(181)+M(230) &
    +M(237)-M(238)-M(240))) * den(45)
  T3sum(1:5,13) = T3sum(1:5,13) + Gcoeff * G1tensor(:,42)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)-M(43)+M(46)-M(49) &
    +M(55)+M(58)-M(61)-M(67)-M(70)-M(73)+M(79)+M(82)+M(85)+M(89)-M(90)+M(91)-M(92)-M(93)+M(94)+M(95)+M(96)+M(97)-M(98)-M(99) &
    -M(100))+c(6)*(-M(179)+M(209)+M(239)-M(247))) * den(45)
  T3sum(1:5,13) = T3sum(1:5,13) + Gcoeff * G1tensor(:,94)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(7)+M(8)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)+M(25)-M(26)-M(29)+M(30)-M(43)-M(46)-M(49)+M(55) &
    +M(58)-M(61)+M(67)-M(70)-M(73)+M(79)+M(82)+M(85)+M(89)+M(90)+M(91)-M(92)-M(93)+M(94)-M(95)+M(96)+M(97)-M(98)-M(99)-M(100)) &
    +c(6)*(-M(137)+M(211)+M(229)-M(248))) * den(45)
  T3sum(1:5,13) = T3sum(1:5,13) + Gcoeff * G1tensor(:,95)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(137)+M(179)-M(209)+M(211)+M(229) &
    -M(239)+M(247)-M(248))) * den(45)
  T3sum(1:5,13) = T3sum(1:5,13) + Gcoeff * G1tensor(:,96)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(6)*(-M(167)-M(179)+M(181)+M(209) &
    +M(237)+M(239)-M(240)-M(247))) * den(45)
  T3sum(1:5,13) = T3sum(1:5,13) + Gcoeff * G1tensor(:,148)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(6)*(-M(137)+M(139)-M(169)+M(211) &
    +M(229)-M(230)+M(238)-M(248))) * den(45)
  T3sum(1:5,13) = T3sum(1:5,13) + Gcoeff * G1tensor(:,149)
  Gcoeff = (c(6)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248))) * den(45)
  T3sum(1:5,13) = T3sum(1:5,13) + Gcoeff * G1tensor(:,150)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(6)-M(7)-M(9)-M(12)-M(14)-M(16)-M(17)-M(19)-M(21)-M(24)-M(26)-M(27)-M(29)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(44)+M(45)+M(55)+M(56)+M(58)+M(67)+M(69)+M(79)+M(80)+M(81)+M(82)+M(83)+M(85)+M(89)+M(90)+M(91)+M(94)+M(96)+M(97) &
    +M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(211)+M(229)))
  T4sum(1:15,38) = T4sum(1:15,38) + Gcoeff * G2tensor(:,20)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(42)+M(46)+M(53)+M(55)+M(58)+M(65)+M(66)+M(69)+M(78)+M(79)+M(80)+M(81)+M(82)+M(85)+M(89)+M(91)+M(94)+M(95)+M(96)+M(97) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(209)+M(239)))
  T4sum(1:15,38) = T4sum(1:15,38) + Gcoeff * G2tensor(:,21)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)-M(44)-M(45)+M(46) &
    +M(53)-M(56)+M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)-M(110)+M(111)+M(112)-M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(209)-M(211)-M(229)+M(239)))
  T4sum(1:15,38) = T4sum(1:15,38) + Gcoeff * G2tensor(:,22)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(9)-M(11)-M(13)-M(16)-M(17)-M(19)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(44)+M(45)+M(47)+M(55)+M(56)+M(58)+M(59)+M(60)+M(61)+M(67)+M(72)+M(79)+M(83)+M(85)+M(89)+M(90)+M(91)+M(96)+M(97)+M(99) &
    +M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(169)+M(230)))
  T4sum(1:15,38) = T4sum(1:15,38) + Gcoeff * G2tensor(:,65)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(46)+M(47)+M(53)+M(55)+M(58)+M(59)+M(60)+M(61)+M(65)+M(66)+M(72)+M(78)+M(79)+M(85)+M(89)+M(91)+M(95)+M(96)+M(97)+M(99) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(167)+M(240)))
  T4sum(1:15,38) = T4sum(1:15,38) + Gcoeff * G2tensor(:,66)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)-M(45)+M(46) &
    +M(53)-M(56)+M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)-M(103)+M(105)+M(107)-M(108)-M(109)-M(110)+M(111)+M(112)+M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(167)-M(169)-M(230)+M(240)))
  T4sum(1:15,38) = T4sum(1:15,38) + Gcoeff * G2tensor(:,67)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)+M(103)-M(104)-M(107)+M(109)-M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(169)-M(211)-M(229)+M(230)))
  T4sum(1:15,38) = T4sum(1:15,38) + Gcoeff * G2tensor(:,110)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(167)-M(209)-M(239)+M(240)))
  T4sum(1:15,38) = T4sum(1:15,38) + Gcoeff * G2tensor(:,111)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(6)*(M(167)-M(169)-M(209)+M(211) &
    +M(229)-M(230)-M(239)+M(240)))
  T4sum(1:15,38) = T4sum(1:15,38) + Gcoeff * G2tensor(:,112)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(141)-M(183)+M(209) &
    -M(211)-M(213)+M(214)-M(229)+M(239))) * den(65)
  T3sum(1:15,36) = T3sum(1:15,36) + Gcoeff * G2tensor(:,23)
  Gcoeff = (c(5)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(6)*(-M(141)+M(151)+M(197) &
    -M(209)+M(210)+M(212)-M(214)-M(239))) * den(65)
  T3sum(1:15,36) = T3sum(1:15,36) + Gcoeff * G2tensor(:,24)
  Gcoeff = (c(5)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(6)*(-M(151)+M(183)-M(197) &
    -M(210)+M(211)-M(212)+M(213)+M(229))) * den(65)
  T3sum(1:15,36) = T3sum(1:15,36) + Gcoeff * G2tensor(:,25)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(142)+M(167)-M(169) &
    -M(171)+M(172)-M(184)-M(230)+M(240))) * den(65)
  T3sum(1:15,36) = T3sum(1:15,36) + Gcoeff * G2tensor(:,68)
  Gcoeff = (c(5)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(6)*(-M(142)+M(152)-M(167) &
    +M(168)+M(170)-M(172)+M(198)-M(240))) * den(65)
  T3sum(1:15,36) = T3sum(1:15,36) + Gcoeff * G2tensor(:,69)
  Gcoeff = (c(5)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(6)*(-M(152)-M(168)+M(169) &
    -M(170)+M(171)+M(184)-M(198)+M(230))) * den(65)
  T3sum(1:15,36) = T3sum(1:15,36) + Gcoeff * G2tensor(:,70)
  Gcoeff = (c(6)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(65)
  T3sum(1:15,36) = T3sum(1:15,36) + Gcoeff * G2tensor(:,113)
  Gcoeff = (c(6)*(M(141)-M(142)-M(151)+M(152)-M(167)+M(168)+M(170)-M(172)-M(197)+M(198)+M(209)-M(210)-M(212)+M(214)+M(239) &
    -M(240))) * den(65)
  T3sum(1:15,36) = T3sum(1:15,36) + Gcoeff * G2tensor(:,114)
  Gcoeff = (c(6)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(65)
  T3sum(1:15,36) = T3sum(1:15,36) + Gcoeff * G2tensor(:,115)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)-M(105)-M(107)+M(108)+M(109)+M(110)-M(111)-M(112)-M(113)-M(118) &
    +M(119)+M(124))+c(6)*(M(139)-M(181)-M(237)+M(238))) * den(33)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,19)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)-M(44)-M(45)-M(46) &
    +M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95)+M(103)-M(105)-M(107)+M(108)+M(109)+M(110)-M(111)-M(112)-M(113)-M(118) &
    +M(119)+M(124))+c(6)*(-M(141)+M(183)+M(213)-M(214))) * den(33)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,20)
  Gcoeff = (c(5)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(6)*(-M(139)-M(141)+M(181)+M(183) &
    +M(213)-M(214)+M(237)-M(238))) * den(33)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,21)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)-M(103)-M(105)+M(107)+M(108)-M(109)+M(110)-M(111)-M(112)+M(113)-M(118) &
    +M(119)+M(124))+c(6)*(M(137)-M(179)-M(247)+M(248))) * den(33)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,73)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)-M(45)-M(46) &
    +M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95)-M(103)-M(105)+M(107)+M(108)-M(109)+M(110)-M(111)-M(112)+M(113)-M(118) &
    +M(119)+M(124))+c(6)*(-M(142)+M(171)-M(172)+M(184))) * den(33)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,74)
  Gcoeff = (c(5)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(6)*(-M(137)-M(142)+M(171)-M(172) &
    +M(179)+M(184)+M(247)-M(248))) * den(33)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,75)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(6)*(M(137)-M(139)-M(179)+M(181) &
    +M(237)-M(238)-M(247)+M(248))) * den(33)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,127)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(6)*(M(141)-M(142)+M(171)-M(172) &
    -M(183)+M(184)-M(213)+M(214))) * den(33)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,128)
  Gcoeff = (c(6)*(-M(137)+M(139)+M(141)-M(142)+M(171)-M(172)+M(179)-M(181)-M(183)+M(184)-M(213)+M(214)-M(237)+M(238)+M(247) &
    -M(248))) * den(33)
  T3sum(1:5,26) = T3sum(1:5,26) + Gcoeff * G1tensor(:,129)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)-M(105)-M(107)+M(108)+M(109)+M(110)-M(111)-M(112)-M(113)-M(118) &
    +M(119)+M(124))+c(6)*(M(139)-M(181)-M(237)+M(238))) * den(33)
  T3sum(1:5,13) = T3sum(1:5,13) + Gcoeff * G1tensor(:,7)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)+M(44)+M(45)-M(46) &
    -M(53)+M(56)-M(65)-M(66)+M(67)-M(78)+M(83)+M(90)-M(95)+M(103)-M(105)-M(107)+M(108)+M(109)+M(110)-M(111)-M(112)-M(113)-M(118) &
    +M(119)+M(124))+c(6)*(-M(167)+M(169)+M(230)-M(240))) * den(33)
  T3sum(1:5,13) = T3sum(1:5,13) + Gcoeff * G1tensor(:,8)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(139)-M(167)+M(169)+M(181)+M(230) &
    +M(237)-M(238)-M(240))) * den(33)
  T3sum(1:5,13) = T3sum(1:5,13) + Gcoeff * G1tensor(:,9)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)-M(103)-M(105)+M(107)+M(108)-M(109)+M(110)-M(111)-M(112)+M(113)-M(118) &
    +M(119)+M(124))+c(6)*(M(137)-M(179)-M(247)+M(248))) * den(33)
  T3sum(1:5,13) = T3sum(1:5,13) + Gcoeff * G1tensor(:,61)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)+M(44)+M(45)-M(46) &
    -M(53)+M(56)-M(65)-M(66)+M(67)-M(78)+M(83)+M(90)-M(95)-M(103)-M(105)+M(107)+M(108)-M(109)+M(110)-M(111)-M(112)+M(113)-M(118) &
    +M(119)+M(124))+c(6)*(-M(209)+M(211)+M(229)-M(239))) * den(33)
  T3sum(1:5,13) = T3sum(1:5,13) + Gcoeff * G1tensor(:,62)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(137)+M(179)-M(209)+M(211)+M(229) &
    -M(239)+M(247)-M(248))) * den(33)
  T3sum(1:5,13) = T3sum(1:5,13) + Gcoeff * G1tensor(:,63)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(6)*(M(137)-M(139)-M(179)+M(181) &
    +M(237)-M(238)-M(247)+M(248))) * den(33)
  T3sum(1:5,13) = T3sum(1:5,13) + Gcoeff * G1tensor(:,115)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(6)*(M(167)-M(169)-M(209)+M(211) &
    +M(229)-M(230)-M(239)+M(240))) * den(33)
  T3sum(1:5,13) = T3sum(1:5,13) + Gcoeff * G1tensor(:,116)
  Gcoeff = (c(6)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248))) * den(33)
  T3sum(1:5,13) = T3sum(1:5,13) + Gcoeff * G1tensor(:,117)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(11)-M(12)+M(17)-M(18)+M(19)-M(20)-M(21)+M(22)-M(23)+M(24)+M(25)-M(26)-M(29)+M(30)+M(50)+M(51)+M(52) &
    -M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91)+M(102)+M(104)-M(108)-M(110)-M(114)+M(116)-M(119)-M(120)+M(121)+M(122) &
    +M(123)-M(124))+c(6)*(M(152)-M(169)+M(170)-M(230))) * den(37)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,43)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(11)-M(12)+M(17)-M(18)+M(19)-M(20)-M(21)+M(22)-M(23)+M(24)+M(25)-M(26)-M(29)+M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)+M(102)+M(104)-M(108)-M(110)-M(114)+M(116)-M(119)-M(120)+M(121)+M(122) &
    +M(123)-M(124))+c(6)*(-M(150)+M(193)-M(194)+M(228))) * den(37)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,44)
  Gcoeff = (c(5)*(-M(50)-M(51)-M(52)+M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91))+c(6)*(-M(150)-M(152)+M(169)-M(170) &
    +M(193)-M(194)+M(228)+M(230))) * den(37)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,45)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(11)+M(12)+M(17)-M(18)+M(19)-M(20)+M(21)-M(22)-M(23)+M(24)-M(25)+M(26)+M(29)-M(30)+M(50)+M(51)+M(52) &
    -M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91)+M(102)-M(104)-M(108)-M(110)+M(114)+M(116)-M(119)+M(120)+M(121)+M(122) &
    -M(123)-M(124))+c(6)*(M(151)-M(211)+M(212)-M(229))) * den(37)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,97)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(11)+M(12)+M(17)-M(18)+M(19)-M(20)+M(21)-M(22)-M(23)+M(24)-M(25)+M(26)+M(29)-M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)+M(102)-M(104)-M(108)-M(110)+M(114)+M(116)-M(119)+M(120)+M(121)+M(122) &
    -M(123)-M(124))+c(6)*(-M(153)+M(187)-M(188)+M(231))) * den(37)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,98)
  Gcoeff = (c(5)*(-M(50)-M(51)-M(52)+M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91))+c(6)*(-M(151)-M(153)+M(187)-M(188) &
    +M(211)-M(212)+M(229)+M(231))) * den(37)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,99)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(104)+M(114)+M(120)-M(123))+c(6)*(M(151)-M(152)+M(169)-M(170) &
    -M(211)+M(212)-M(229)+M(230))) * den(37)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,151)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(104)+M(114)+M(120)-M(123))+c(6)*(M(150)-M(153)+M(187)-M(188) &
    -M(193)+M(194)-M(228)+M(231))) * den(37)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,152)
  Gcoeff = (c(6)*(M(150)-M(151)+M(152)-M(153)-M(169)+M(170)+M(187)-M(188)-M(193)+M(194)+M(211)-M(212)-M(228)+M(229)-M(230) &
    +M(231))) * den(37)
  T3sum(1:5,19) = T3sum(1:5,19) + Gcoeff * G1tensor(:,153)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)-M(46)+M(51)-M(58) &
    +M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(167)+M(168)+M(198)-M(240))) * den(22)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,46)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(143)-M(144)-M(200)+M(242))) * den(22)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,47)
  Gcoeff = (c(5)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(6)*(M(143)-M(144)+M(167)-M(168) &
    -M(198)-M(200)+M(240)+M(242))) * den(22)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,48)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)-M(46)+M(51)-M(58) &
    +M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(197)-M(209)+M(210)-M(239))) * den(22)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,100)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(134)-M(202)+M(244))) * den(22)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,101)
  Gcoeff = (c(5)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(6)*(M(133)-M(134)-M(197)-M(202) &
    +M(209)-M(210)+M(239)+M(244))) * den(22)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,102)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(167)-M(168)+M(197)-M(198) &
    -M(209)+M(210)-M(239)+M(240))) * den(22)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,154)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(133)-M(134)-M(143)+M(144) &
    +M(200)-M(202)-M(242)+M(244))) * den(22)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,155)
  Gcoeff = (c(6)*(M(133)-M(134)-M(143)+M(144)-M(167)+M(168)-M(197)+M(198)+M(200)-M(202)+M(209)-M(210)+M(239)-M(240)-M(242) &
    +M(244))) * den(22)
  T3sum(1:5,34) = T3sum(1:5,34) + Gcoeff * G1tensor(:,156)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(43)+M(46)+M(47)+M(48)+M(49)+M(58)+M(59)+M(68)+M(69)+M(71)+M(81)+M(82)+M(85)+M(92)+M(94)+M(95)+M(96)+M(97)+M(98) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(143)+M(242)))
  T4sum(1:15,69) = T4sum(1:15,69) + Gcoeff * G2tensor(:,13)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(5)-M(8)-M(10)-M(11)-M(13)-M(15)-M(18)-M(20)-M(22)-M(23)-M(25)-M(28)-M(30)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(47)+M(48)+M(55)+M(59)+M(61)+M(67)+M(68)+M(69)+M(70)+M(71)+M(73)+M(79)+M(81)+M(89)+M(90)+M(91)+M(93)+M(99) &
    +M(100)+M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(187)+M(231)))
  T4sum(1:15,69) = T4sum(1:15,69) + Gcoeff * G2tensor(:,14)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(7)-M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(25)+M(26)+M(29)-M(30)-M(43)-M(46)-M(49) &
    +M(55)-M(58)+M(61)+M(67)+M(70)+M(73)+M(79)-M(82)-M(85)+M(89)+M(90)+M(91)-M(92)+M(93)-M(94)-M(95)-M(96)-M(97)-M(98)+M(99) &
    +M(100))+c(6)*(-M(143)+M(187)+M(231)-M(242)))
  T4sum(1:15,69) = T4sum(1:15,69) + Gcoeff * G2tensor(:,15)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(46)+M(48)+M(49)+M(58)+M(60)+M(61)+M(68)+M(71)+M(72)+M(80)+M(85)+M(92)+M(95)+M(96)+M(97)+M(98)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(133)+M(244)))
  T4sum(1:15,69) = T4sum(1:15,69) + Gcoeff * G2tensor(:,58)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(10)-M(12)-M(14)-M(15)-M(18)-M(20)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(48)+M(55)+M(60)+M(67)+M(68)+M(70)+M(71)+M(72)+M(73)+M(79)+M(80)+M(82)+M(89)+M(90)+M(91)+M(93)+M(94) &
    +M(100)+M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(193)+M(228)))
  T4sum(1:15,69) = T4sum(1:15,69) + Gcoeff * G2tensor(:,59)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)-M(43)-M(46)-M(49) &
    +M(55)-M(58)-M(61)+M(67)+M(70)+M(73)+M(79)+M(82)-M(85)+M(89)+M(90)+M(91)-M(92)+M(93)+M(94)-M(95)-M(96)-M(97)-M(98)-M(99) &
    +M(100))+c(6)*(-M(133)+M(193)+M(228)-M(244)))
  T4sum(1:15,69) = T4sum(1:15,69) + Gcoeff * G2tensor(:,60)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)+M(61)-M(69)+M(72)+M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(143)-M(242)+M(244)))
  T4sum(1:15,69) = T4sum(1:15,69) + Gcoeff * G2tensor(:,103)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)-M(61)-M(69)+M(72)+M(80)-M(81)+M(82)+M(94)-M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(187)+M(193)+M(228)-M(231)))
  T4sum(1:15,69) = T4sum(1:15,69) + Gcoeff * G2tensor(:,104)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(6)*(-M(133)+M(143)-M(187)+M(193) &
    +M(228)-M(231)+M(242)-M(244)))
  T4sum(1:15,69) = T4sum(1:15,69) + Gcoeff * G2tensor(:,105)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(50)+M(51)-M(52) &
    +M(55)-M(62)-M(67)+M(74)+M(79)-M(86)+M(89)-M(90)+M(91)+M(102)+M(104)-M(108)-M(110)-M(114)+M(116)-M(119)-M(120)+M(121)+M(122) &
    +M(123)-M(124))+c(6)*(-M(183)+M(197)+M(210)-M(213))) * den(37)
  T3sum(1:5,11) = T3sum(1:5,11) + Gcoeff * G1tensor(:,49)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(11)-M(12)+M(17)-M(18)+M(19)-M(20)-M(21)+M(22)-M(23)+M(24)+M(25)-M(26)-M(29)+M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)+M(102)+M(104)-M(108)-M(110)-M(114)+M(116)-M(119)-M(120)+M(121)+M(122) &
    +M(123)-M(124))+c(6)*(-M(150)+M(193)-M(194)+M(228))) * den(37)
  T3sum(1:5,11) = T3sum(1:5,11) + Gcoeff * G1tensor(:,50)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(150)+M(183)+M(193)-M(194) &
    -M(197)-M(210)+M(213)+M(228))) * den(37)
  T3sum(1:5,11) = T3sum(1:5,11) + Gcoeff * G1tensor(:,51)
  Gcoeff = (c(4)*(M(5)-M(6)-M(11)+M(12)-M(17)+M(18)-M(19)+M(20)+M(21)-M(22)+M(23)-M(24)-M(25)+M(26)+M(29)-M(30)-M(50)+M(51)-M(52) &
    +M(55)-M(62)-M(67)+M(74)+M(79)-M(86)+M(89)-M(90)+M(91)+M(102)-M(104)-M(108)-M(110)+M(114)+M(116)-M(119)+M(120)+M(121)+M(122) &
    -M(123)-M(124))+c(6)*(M(168)-M(171)-M(184)+M(198))) * den(37)
  T3sum(1:5,11) = T3sum(1:5,11) + Gcoeff * G1tensor(:,103)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(11)+M(12)+M(17)-M(18)+M(19)-M(20)+M(21)-M(22)-M(23)+M(24)-M(25)+M(26)+M(29)-M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)+M(102)-M(104)-M(108)-M(110)+M(114)+M(116)-M(119)+M(120)+M(121)+M(122) &
    -M(123)-M(124))+c(6)*(-M(153)+M(187)-M(188)+M(231))) * den(37)
  T3sum(1:5,11) = T3sum(1:5,11) + Gcoeff * G1tensor(:,104)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(153)-M(168)+M(171)+M(184) &
    +M(187)-M(188)-M(198)+M(231))) * den(37)
  T3sum(1:5,11) = T3sum(1:5,11) + Gcoeff * G1tensor(:,105)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(104)+M(114)+M(120)-M(123))+c(6)*(M(168)-M(171)+M(183)-M(184) &
    -M(197)+M(198)-M(210)+M(213))) * den(37)
  T3sum(1:5,11) = T3sum(1:5,11) + Gcoeff * G1tensor(:,157)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(104)+M(114)+M(120)-M(123))+c(6)*(M(150)-M(153)+M(187)-M(188) &
    -M(193)+M(194)-M(228)+M(231))) * den(37)
  T3sum(1:5,11) = T3sum(1:5,11) + Gcoeff * G1tensor(:,158)
  Gcoeff = (c(6)*(M(150)-M(153)-M(168)+M(171)-M(183)+M(184)+M(187)-M(188)-M(193)+M(194)+M(197)-M(198)+M(210)-M(213)-M(228) &
    +M(231))) * den(37)
  T3sum(1:5,11) = T3sum(1:5,11) + Gcoeff * G1tensor(:,159)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(5)-M(8)-M(10)-M(11)-M(13)-M(15)-M(18)-M(20)-M(22)-M(23)-M(25)-M(28)-M(30)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(47)+M(48)+M(55)+M(59)+M(61)+M(67)+M(68)+M(69)+M(70)+M(71)+M(73)+M(79)+M(81)+M(89)+M(90)+M(91)+M(93)+M(99) &
    +M(100)+M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(187)+M(231)))
  T4sum(1:15,74) = T4sum(1:15,74) + Gcoeff * G2tensor(:,27)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(47)+M(51)+M(53)+M(55)+M(59)+M(60)+M(61)+M(63)+M(65)+M(66)+M(72)+M(74)+M(75)+M(76)+M(78)+M(79)+M(88)+M(89)+M(91)+M(99) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(168)+M(198)))
  T4sum(1:15,74) = T4sum(1:15,74) + Gcoeff * G2tensor(:,28)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)-M(41)-M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)+M(66)-M(67)-M(68)-M(69)-M(70)-M(71)+M(72)-M(73)+M(74)+M(75)+M(76)+M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(168)-M(187)+M(198)-M(231)))
  T4sum(1:15,74) = T4sum(1:15,74) + Gcoeff * G2tensor(:,29)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(10)-M(12)-M(14)-M(15)-M(18)-M(20)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(48)+M(55)+M(60)+M(67)+M(68)+M(70)+M(71)+M(72)+M(73)+M(79)+M(80)+M(82)+M(89)+M(90)+M(91)+M(93)+M(94) &
    +M(100)+M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(193)+M(228)))
  T4sum(1:15,74) = T4sum(1:15,74) + Gcoeff * G2tensor(:,72)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(42)+M(51)+M(53)+M(55)+M(63)+M(65)+M(66)+M(69)+M(74)+M(75)+M(76)+M(78)+M(79)+M(80)+M(81)+M(82)+M(88)+M(89)+M(91)+M(94) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(197)+M(210)))
  T4sum(1:15,74) = T4sum(1:15,74) + Gcoeff * G2tensor(:,73)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)-M(48)+M(51) &
    +M(53)-M(60)+M(63)+M(65)+M(66)-M(67)-M(68)+M(69)-M(70)-M(71)-M(72)-M(73)+M(74)+M(75)+M(76)+M(78)+M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(-M(193)+M(197)+M(210)-M(228)))
  T4sum(1:15,74) = T4sum(1:15,74) + Gcoeff * G2tensor(:,74)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)-M(61)-M(69)+M(72)+M(80)-M(81)+M(82)+M(94)-M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(187)+M(193)+M(228)-M(231)))
  T4sum(1:15,74) = T4sum(1:15,74) + Gcoeff * G2tensor(:,117)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)+M(42)-M(47)-M(59) &
    -M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(168)+M(197)-M(198)+M(210)))
  T4sum(1:15,74) = T4sum(1:15,74) + Gcoeff * G2tensor(:,118)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(168)+M(187)-M(193)+M(197)-M(198) &
    +M(210)-M(228)+M(231)))
  T4sum(1:15,74) = T4sum(1:15,74) + Gcoeff * G2tensor(:,119)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(144)+M(168)-M(181)-M(187) &
    +M(198)+M(200)-M(231)-M(237))) * den(9)
  T3sum(1:15,34) = T3sum(1:15,34) + Gcoeff * G2tensor(:,30)
  Gcoeff = (c(5)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(6)*(M(143)-M(144)+M(167)-M(168) &
    -M(198)-M(200)+M(240)+M(242))) * den(9)
  T3sum(1:15,34) = T3sum(1:15,34) + Gcoeff * G2tensor(:,31)
  Gcoeff = (c(5)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(6)*(-M(143)-M(167)+M(181)+M(187) &
    +M(231)+M(237)-M(240)-M(242))) * den(9)
  T3sum(1:15,34) = T3sum(1:15,34) + Gcoeff * G2tensor(:,32)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(134)-M(179)-M(193)+M(197) &
    +M(202)+M(210)-M(228)-M(247))) * den(9)
  T3sum(1:15,34) = T3sum(1:15,34) + Gcoeff * G2tensor(:,75)
  Gcoeff = (c(5)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(6)*(M(133)-M(134)-M(197)-M(202) &
    +M(209)-M(210)+M(239)+M(244))) * den(9)
  T3sum(1:15,34) = T3sum(1:15,34) + Gcoeff * G2tensor(:,76)
  Gcoeff = (c(5)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(6)*(-M(133)+M(179)+M(193)-M(209) &
    +M(228)-M(239)-M(244)+M(247))) * den(9)
  T3sum(1:15,34) = T3sum(1:15,34) + Gcoeff * G2tensor(:,77)
  Gcoeff = (c(6)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(9)
  T3sum(1:15,34) = T3sum(1:15,34) + Gcoeff * G2tensor(:,120)
  Gcoeff = (c(6)*(M(133)-M(134)-M(143)+M(144)-M(167)+M(168)-M(197)+M(198)+M(200)-M(202)+M(209)-M(210)+M(239)-M(240)-M(242) &
    +M(244))) * den(9)
  T3sum(1:15,34) = T3sum(1:15,34) + Gcoeff * G2tensor(:,121)
  Gcoeff = (c(6)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(9)
  T3sum(1:15,34) = T3sum(1:15,34) + Gcoeff * G2tensor(:,122)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(58) &
    -M(63)+M(74)-M(75)-M(76)+M(85)-M(88)-M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(141)+M(151)+M(212)-M(214))) * den(22)
  T3sum(1:5,2) = T3sum(1:5,2) + Gcoeff * G1tensor(:,52)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(143)-M(144)-M(200)+M(242))) * den(22)
  T3sum(1:5,2) = T3sum(1:5,2) + Gcoeff * G1tensor(:,53)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(141)+M(143)-M(144)-M(151) &
    -M(200)-M(212)+M(214)+M(242))) * den(22)
  T3sum(1:5,2) = T3sum(1:5,2) + Gcoeff * G1tensor(:,54)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(58) &
    -M(63)+M(74)-M(75)-M(76)+M(85)-M(88)-M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(142)+M(152)+M(170)-M(172))) * den(22)
  T3sum(1:5,2) = T3sum(1:5,2) + Gcoeff * G1tensor(:,106)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(134)-M(202)+M(244))) * den(22)
  T3sum(1:5,2) = T3sum(1:5,2) + Gcoeff * G1tensor(:,107)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(133)-M(134)+M(142)-M(152) &
    -M(170)+M(172)-M(202)+M(244))) * den(22)
  T3sum(1:5,2) = T3sum(1:5,2) + Gcoeff * G1tensor(:,108)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(141)-M(142)-M(151)+M(152) &
    +M(170)-M(172)-M(212)+M(214))) * den(22)
  T3sum(1:5,2) = T3sum(1:5,2) + Gcoeff * G1tensor(:,160)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(133)-M(134)-M(143)+M(144) &
    +M(200)-M(202)-M(242)+M(244))) * den(22)
  T3sum(1:5,2) = T3sum(1:5,2) + Gcoeff * G1tensor(:,161)
  Gcoeff = (c(6)*(M(133)-M(134)-M(141)+M(142)-M(143)+M(144)+M(151)-M(152)-M(170)+M(172)+M(200)-M(202)+M(212)-M(214)-M(242) &
    +M(244))) * den(22)
  T3sum(1:5,2) = T3sum(1:5,2) + Gcoeff * G1tensor(:,162)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(44)+M(45)+M(50)+M(51)+M(52)+M(56)+M(58)+M(62)+M(69)+M(74)+M(80)+M(81)+M(82)+M(83)+M(85)+M(86)+M(94)+M(96)+M(97) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(151)+M(212)))
  T4sum(1:15,95) = T4sum(1:15,95) + Gcoeff * G2tensor(:,16)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(43)+M(46)+M(47)+M(48)+M(49)+M(58)+M(59)+M(68)+M(69)+M(71)+M(81)+M(82)+M(85)+M(92)+M(94)+M(95)+M(96)+M(97)+M(98) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(143)+M(242)))
  T4sum(1:15,95) = T4sum(1:15,95) + Gcoeff * G2tensor(:,17)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42) &
    +M(43)-M(44)-M(45)+M(46)+M(47)+M(48)+M(49)-M(50)-M(51)-M(52)-M(56)+M(59)-M(62)+M(68)+M(71)-M(74)-M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(143)-M(151)-M(212)+M(242)))
  T4sum(1:15,95) = T4sum(1:15,95) + Gcoeff * G2tensor(:,18)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(44)+M(45)+M(47)+M(50)+M(51)+M(52)+M(56)+M(58)+M(59)+M(60)+M(61)+M(62)+M(72)+M(74)+M(83)+M(85)+M(86)+M(96)+M(97)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(152)+M(170)))
  T4sum(1:15,95) = T4sum(1:15,95) + Gcoeff * G2tensor(:,61)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(46)+M(48)+M(49)+M(58)+M(60)+M(61)+M(68)+M(71)+M(72)+M(80)+M(85)+M(92)+M(95)+M(96)+M(97)+M(98)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(133)+M(244)))
  T4sum(1:15,95) = T4sum(1:15,95) + Gcoeff * G2tensor(:,62)
  Gcoeff = (c(4)*(-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42) &
    +M(43)-M(44)-M(45)+M(46)-M(47)+M(48)+M(49)-M(50)-M(51)-M(52)-M(56)-M(59)-M(62)+M(68)+M(71)-M(74)+M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(133)-M(152)-M(170)+M(244)))
  T4sum(1:15,95) = T4sum(1:15,95) + Gcoeff * G2tensor(:,63)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(151)+M(152)+M(170)-M(212)))
  T4sum(1:15,95) = T4sum(1:15,95) + Gcoeff * G2tensor(:,106)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)+M(61)-M(69)+M(72)+M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(143)-M(242)+M(244)))
  T4sum(1:15,95) = T4sum(1:15,95) + Gcoeff * G2tensor(:,107)
  Gcoeff = (c(5)*(-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59)+M(80))+c(6)*(M(133)-M(143)+M(151)-M(152) &
    -M(170)+M(212)-M(242)+M(244)))
  T4sum(1:15,95) = T4sum(1:15,95) + Gcoeff * G2tensor(:,108)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(133)-M(139)+M(150)+M(152) &
    +M(170)+M(194)-M(238)-M(244))) * den(4)
  T3sum(1:15,19) = T3sum(1:15,19) + Gcoeff * G2tensor(:,34)
  Gcoeff = (c(5)*(-M(50)-M(51)-M(52)+M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91))+c(6)*(-M(150)-M(152)+M(169)-M(170) &
    +M(193)-M(194)+M(228)+M(230))) * den(4)
  T3sum(1:15,19) = T3sum(1:15,19) + Gcoeff * G2tensor(:,35)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(55)-M(67)-M(79)-M(89)-M(90)-M(91)+M(92)+M(95)+M(98))+c(6)*(M(133)+M(139)-M(169)-M(193) &
    -M(228)-M(230)+M(238)+M(244))) * den(4)
  T3sum(1:15,19) = T3sum(1:15,19) + Gcoeff * G2tensor(:,36)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(137)-M(143)+M(151)+M(153) &
    +M(188)+M(212)-M(242)-M(248))) * den(4)
  T3sum(1:15,19) = T3sum(1:15,19) + Gcoeff * G2tensor(:,79)
  Gcoeff = (c(5)*(-M(50)-M(51)-M(52)+M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91))+c(6)*(-M(151)-M(153)+M(187)-M(188) &
    +M(211)-M(212)+M(229)+M(231))) * den(4)
  T3sum(1:15,19) = T3sum(1:15,19) + Gcoeff * G2tensor(:,80)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(55)-M(67)-M(79)-M(89)-M(90)-M(91)+M(92)+M(95)+M(98))+c(6)*(M(137)+M(143)-M(187)-M(211) &
    -M(229)-M(231)+M(242)+M(248))) * den(4)
  T3sum(1:15,19) = T3sum(1:15,19) + Gcoeff * G2tensor(:,81)
  Gcoeff = (c(6)*(M(133)-M(137)+M(139)-M(143)-M(150)+M(151)-M(152)+M(153)-M(170)+M(188)-M(194)+M(212)+M(238)-M(242)+M(244) &
    -M(248))) * den(4)
  T3sum(1:15,19) = T3sum(1:15,19) + Gcoeff * G2tensor(:,124)
  Gcoeff = (c(6)*(M(150)-M(151)+M(152)-M(153)-M(169)+M(170)+M(187)-M(188)-M(193)+M(194)+M(211)-M(212)-M(228)+M(229)-M(230) &
    +M(231))) * den(4)
  T3sum(1:15,19) = T3sum(1:15,19) + Gcoeff * G2tensor(:,125)
  Gcoeff = (c(6)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(4)
  T3sum(1:15,19) = T3sum(1:15,19) + Gcoeff * G2tensor(:,126)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(M(139)+M(141)-M(144)-M(150) &
    -M(194)-M(200)+M(214)+M(238))) * den(2)
  T3sum(1:15,26) = T3sum(1:15,26) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(5)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(6)*(-M(139)-M(141)+M(181)+M(183) &
    +M(213)-M(214)+M(237)-M(238))) * den(2)
  T3sum(1:15,26) = T3sum(1:15,26) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(5)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(6)*(M(144)+M(150)-M(181)-M(183) &
    +M(194)+M(200)-M(213)-M(237))) * den(2)
  T3sum(1:15,26) = T3sum(1:15,26) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(-M(134)+M(137)+M(142)-M(153) &
    +M(172)-M(188)-M(202)+M(248))) * den(2)
  T3sum(1:15,26) = T3sum(1:15,26) + Gcoeff * G2tensor(:,46)
  Gcoeff = (c(5)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(6)*(-M(137)-M(142)+M(171)-M(172) &
    +M(179)+M(184)+M(247)-M(248))) * den(2)
  T3sum(1:15,26) = T3sum(1:15,26) + Gcoeff * G2tensor(:,47)
  Gcoeff = (c(5)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(6)*(M(134)+M(153)-M(171)-M(179) &
    -M(184)+M(188)+M(202)-M(247))) * den(2)
  T3sum(1:15,26) = T3sum(1:15,26) + Gcoeff * G2tensor(:,48)
  Gcoeff = (c(6)*(-M(134)+M(137)-M(139)-M(141)+M(142)+M(144)+M(150)-M(153)+M(172)-M(188)+M(194)+M(200)-M(202)-M(214)-M(238) &
    +M(248))) * den(2)
  T3sum(1:15,26) = T3sum(1:15,26) + Gcoeff * G2tensor(:,91)
  Gcoeff = (c(6)*(-M(137)+M(139)+M(141)-M(142)+M(171)-M(172)+M(179)-M(181)-M(183)+M(184)-M(213)+M(214)-M(237)+M(238)+M(247) &
    -M(248))) * den(2)
  T3sum(1:15,26) = T3sum(1:15,26) + Gcoeff * G2tensor(:,92)
  Gcoeff = (c(6)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(2)
  T3sum(1:15,26) = T3sum(1:15,26) + Gcoeff * G2tensor(:,93)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(141)+M(143)-M(144)-M(151) &
    -M(200)-M(212)+M(214)+M(242))) * den(23)
  T3sum(1:15,2) = T3sum(1:15,2) + Gcoeff * G2tensor(:,33)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(133)-M(134)+M(142)-M(152) &
    -M(170)+M(172)-M(202)+M(244))) * den(23)
  T3sum(1:15,2) = T3sum(1:15,2) + Gcoeff * G2tensor(:,78)
  Gcoeff = (c(6)*(M(133)-M(134)-M(141)+M(142)-M(143)+M(144)+M(151)-M(152)-M(170)+M(172)+M(200)-M(202)+M(212)-M(214)-M(242) &
    +M(244))) * den(23)
  T3sum(1:15,2) = T3sum(1:15,2) + Gcoeff * G2tensor(:,123)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(M(139)+M(141)-M(144)-M(150) &
    -M(194)-M(200)+M(214)+M(238))) * den(147)
  T3sum(1:15,26) = T3sum(1:15,26) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(-M(134)+M(137)+M(142)-M(153) &
    +M(172)-M(188)-M(202)+M(248))) * den(147)
  T3sum(1:15,26) = T3sum(1:15,26) + Gcoeff * G2tensor(:,49)
  Gcoeff = (c(6)*(-M(134)+M(137)-M(139)-M(141)+M(142)+M(144)+M(150)-M(153)+M(172)-M(188)+M(194)+M(200)-M(202)-M(214)-M(238) &
    +M(248))) * den(147)
  T3sum(1:15,26) = T3sum(1:15,26) + Gcoeff * G2tensor(:,94)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(133)-M(139)+M(150)+M(152) &
    +M(170)+M(194)-M(238)-M(244))) * den(307)
  T3sum(1:15,19) = T3sum(1:15,19) + Gcoeff * G2tensor(:,37)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(137)-M(143)+M(151)+M(153) &
    +M(188)+M(212)-M(242)-M(248))) * den(307)
  T3sum(1:15,19) = T3sum(1:15,19) + Gcoeff * G2tensor(:,82)
  Gcoeff = (c(6)*(M(133)-M(137)+M(139)-M(143)-M(150)+M(151)-M(152)+M(153)-M(170)+M(188)-M(194)+M(212)+M(238)-M(242)+M(244) &
    -M(248))) * den(307)
  T3sum(1:15,19) = T3sum(1:15,19) + Gcoeff * G2tensor(:,127)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42) &
    +M(43)-M(44)-M(45)+M(46)+M(47)+M(48)+M(49)-M(50)-M(51)-M(52)-M(56)+M(59)-M(62)+M(68)+M(71)-M(74)-M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(143)-M(151)-M(212)+M(242))) * den(11)
  T4sum(1:35,95) = T4sum(1:35,95) + Gcoeff * G3tensor(:,2)
  Gcoeff = (c(4)*(-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42) &
    +M(43)-M(44)-M(45)+M(46)-M(47)+M(48)+M(49)-M(50)-M(51)-M(52)-M(56)-M(59)-M(62)+M(68)+M(71)-M(74)+M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(133)-M(152)-M(170)+M(244))) * den(11)
  T4sum(1:35,95) = T4sum(1:35,95) + Gcoeff * G3tensor(:,8)
  Gcoeff = (c(5)*(-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59)+M(80))+c(6)*(M(133)-M(143)+M(151)-M(152) &
    -M(170)+M(212)-M(242)+M(244))) * den(11)
  T4sum(1:35,95) = T4sum(1:35,95) + Gcoeff * G3tensor(:,14)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(150)+M(183)+M(193)-M(194) &
    -M(197)-M(210)+M(213)+M(228))) * den(49)
  T3sum(1:15,11) = T3sum(1:15,11) + Gcoeff * G2tensor(:,26)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(153)-M(168)+M(171)+M(184) &
    +M(187)-M(188)-M(198)+M(231))) * den(49)
  T3sum(1:15,11) = T3sum(1:15,11) + Gcoeff * G2tensor(:,71)
  Gcoeff = (c(6)*(M(150)-M(153)-M(168)+M(171)-M(183)+M(184)+M(187)-M(188)-M(193)+M(194)+M(197)-M(198)+M(210)-M(213)-M(228) &
    +M(231))) * den(49)
  T3sum(1:15,11) = T3sum(1:15,11) + Gcoeff * G2tensor(:,116)
  Gcoeff = (c(5)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(6)*(M(144)+M(150)-M(181)-M(183) &
    +M(194)+M(200)-M(213)-M(237))) * den(219)
  T3sum(1:15,26) = T3sum(1:15,26) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(5)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(6)*(M(134)+M(153)-M(171)-M(179) &
    -M(184)+M(188)+M(202)-M(247))) * den(219)
  T3sum(1:15,26) = T3sum(1:15,26) + Gcoeff * G2tensor(:,50)
  Gcoeff = (c(6)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(219)
  T3sum(1:15,26) = T3sum(1:15,26) + Gcoeff * G2tensor(:,95)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(144)+M(168)-M(181)-M(187) &
    +M(198)+M(200)-M(231)-M(237))) * den(398)
  T3sum(1:15,34) = T3sum(1:15,34) + Gcoeff * G2tensor(:,38)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(134)-M(179)-M(193)+M(197) &
    +M(202)+M(210)-M(228)-M(247))) * den(398)
  T3sum(1:15,34) = T3sum(1:15,34) + Gcoeff * G2tensor(:,83)
  Gcoeff = (c(6)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(398)
  T3sum(1:15,34) = T3sum(1:15,34) + Gcoeff * G2tensor(:,128)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)-M(41)-M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)+M(66)-M(67)-M(68)-M(69)-M(70)-M(71)+M(72)-M(73)+M(74)+M(75)+M(76)+M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(168)-M(187)+M(198)-M(231))) * den(40)
  T4sum(1:35,74) = T4sum(1:35,74) + Gcoeff * G3tensor(:,5)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)-M(48)+M(51) &
    +M(53)-M(60)+M(63)+M(65)+M(66)-M(67)-M(68)+M(69)-M(70)-M(71)-M(72)-M(73)+M(74)+M(75)+M(76)+M(78)+M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(-M(193)+M(197)+M(210)-M(228))) * den(40)
  T4sum(1:35,74) = T4sum(1:35,74) + Gcoeff * G3tensor(:,11)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(168)+M(187)-M(193)+M(197)-M(198) &
    +M(210)-M(228)+M(231))) * den(40)
  T4sum(1:35,74) = T4sum(1:35,74) + Gcoeff * G3tensor(:,17)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(139)-M(167)+M(169)+M(181)+M(230) &
    +M(237)-M(238)-M(240))) * den(52)
  T3sum(1:15,13) = T3sum(1:15,13) + Gcoeff * G2tensor(:,19)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(137)+M(179)-M(209)+M(211)+M(229) &
    -M(239)+M(247)-M(248))) * den(52)
  T3sum(1:15,13) = T3sum(1:15,13) + Gcoeff * G2tensor(:,64)
  Gcoeff = (c(6)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248))) * den(52)
  T3sum(1:15,13) = T3sum(1:15,13) + Gcoeff * G2tensor(:,109)
  Gcoeff = (c(5)*(M(44)+M(45)+M(46)-M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95))+c(6)*(M(139)+M(141)-M(181)-M(183) &
    -M(213)+M(214)-M(237)+M(238))) * den(237)
  T3sum(1:15,26) = T3sum(1:15,26) + Gcoeff * G2tensor(:,6)
  Gcoeff = (c(5)*(M(44)+M(45)+M(46)-M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95))+c(6)*(M(137)+M(142)-M(171)+M(172) &
    -M(179)-M(184)-M(247)+M(248))) * den(237)
  T3sum(1:15,26) = T3sum(1:15,26) + Gcoeff * G2tensor(:,51)
  Gcoeff = (c(6)*(M(137)-M(139)-M(141)+M(142)-M(171)+M(172)-M(179)+M(181)+M(183)-M(184)+M(213)-M(214)+M(237)-M(238)-M(247) &
    +M(248))) * den(237)
  T3sum(1:15,26) = T3sum(1:15,26) + Gcoeff * G2tensor(:,96)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(141)-M(183)+M(209) &
    -M(211)-M(213)+M(214)-M(229)+M(239))) * den(371)
  T3sum(1:15,36) = T3sum(1:15,36) + Gcoeff * G2tensor(:,39)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(142)+M(167)-M(169) &
    -M(171)+M(172)-M(184)-M(230)+M(240))) * den(371)
  T3sum(1:15,36) = T3sum(1:15,36) + Gcoeff * G2tensor(:,84)
  Gcoeff = (c(6)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(371)
  T3sum(1:15,36) = T3sum(1:15,36) + Gcoeff * G2tensor(:,129)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)-M(44)-M(45)+M(46) &
    +M(53)-M(56)+M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)-M(110)+M(111)+M(112)-M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(209)-M(211)-M(229)+M(239))) * den(33)
  T4sum(1:35,38) = T4sum(1:35,38) + Gcoeff * G3tensor(:,6)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)-M(45)+M(46) &
    +M(53)-M(56)+M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)-M(103)+M(105)+M(107)-M(108)-M(109)-M(110)+M(111)+M(112)+M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(167)-M(169)-M(230)+M(240))) * den(33)
  T4sum(1:35,38) = T4sum(1:35,38) + Gcoeff * G3tensor(:,12)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(6)*(M(167)-M(169)-M(209)+M(211) &
    +M(229)-M(230)-M(239)+M(240))) * den(33)
  T4sum(1:35,38) = T4sum(1:35,38) + Gcoeff * G3tensor(:,18)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(55)-M(67)-M(79)-M(89)-M(90)-M(91)+M(92)+M(95)+M(98))+c(6)*(M(133)+M(139)-M(169)-M(193) &
    -M(228)-M(230)+M(238)+M(244))) * den(259)
  T3sum(1:15,19) = T3sum(1:15,19) + Gcoeff * G2tensor(:,40)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(55)-M(67)-M(79)-M(89)-M(90)-M(91)+M(92)+M(95)+M(98))+c(6)*(M(137)+M(143)-M(187)-M(211) &
    -M(229)-M(231)+M(242)+M(248))) * den(259)
  T3sum(1:15,19) = T3sum(1:15,19) + Gcoeff * G2tensor(:,85)
  Gcoeff = (c(6)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(259)
  T3sum(1:15,19) = T3sum(1:15,19) + Gcoeff * G2tensor(:,130)
  Gcoeff = (c(5)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(6)*(-M(143)-M(167)+M(181)+M(187) &
    +M(231)+M(237)-M(240)-M(242))) * den(204)
  T3sum(1:15,34) = T3sum(1:15,34) + Gcoeff * G2tensor(:,41)
  Gcoeff = (c(5)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(6)*(-M(133)+M(179)+M(193)-M(209) &
    +M(228)-M(239)-M(244)+M(247))) * den(204)
  T3sum(1:15,34) = T3sum(1:15,34) + Gcoeff * G2tensor(:,86)
  Gcoeff = (c(6)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(204)
  T3sum(1:15,34) = T3sum(1:15,34) + Gcoeff * G2tensor(:,131)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(7)-M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(25)+M(26)+M(29)-M(30)-M(43)-M(46)-M(49) &
    +M(55)-M(58)+M(61)+M(67)+M(70)+M(73)+M(79)-M(82)-M(85)+M(89)+M(90)+M(91)-M(92)+M(93)-M(94)-M(95)-M(96)-M(97)-M(98)+M(99) &
    +M(100))+c(6)*(-M(143)+M(187)+M(231)-M(242))) * den(45)
  T4sum(1:35,69) = T4sum(1:35,69) + Gcoeff * G3tensor(:,3)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)-M(43)-M(46)-M(49) &
    +M(55)-M(58)-M(61)+M(67)+M(70)+M(73)+M(79)+M(82)-M(85)+M(89)+M(90)+M(91)-M(92)+M(93)+M(94)-M(95)-M(96)-M(97)-M(98)-M(99) &
    +M(100))+c(6)*(-M(133)+M(193)+M(228)-M(244))) * den(45)
  T4sum(1:35,69) = T4sum(1:35,69) + Gcoeff * G3tensor(:,9)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(6)*(-M(133)+M(143)-M(187)+M(193) &
    +M(228)-M(231)+M(242)-M(244))) * den(45)
  T4sum(1:35,69) = T4sum(1:35,69) + Gcoeff * G3tensor(:,15)
  Gcoeff = (c(5)*(M(50)+M(51)+M(52)-M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91))+c(6)*(M(150)+M(152)-M(169)+M(170) &
    -M(193)+M(194)-M(228)-M(230))) * den(262)
  T3sum(1:15,19) = T3sum(1:15,19) + Gcoeff * G2tensor(:,42)
  Gcoeff = (c(5)*(M(50)+M(51)+M(52)-M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91))+c(6)*(M(151)+M(153)-M(187)+M(188) &
    -M(211)+M(212)-M(229)-M(231))) * den(262)
  T3sum(1:15,19) = T3sum(1:15,19) + Gcoeff * G2tensor(:,87)
  Gcoeff = (c(6)*(-M(150)+M(151)-M(152)+M(153)+M(169)-M(170)-M(187)+M(188)+M(193)-M(194)-M(211)+M(212)+M(228)-M(229)+M(230) &
    -M(231))) * den(262)
  T3sum(1:15,19) = T3sum(1:15,19) + Gcoeff * G2tensor(:,132)
  Gcoeff = (c(5)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(6)*(-M(151)+M(183)-M(197) &
    -M(210)+M(211)-M(212)+M(213)+M(229))) * den(181)
  T3sum(1:15,36) = T3sum(1:15,36) + Gcoeff * G2tensor(:,43)
  Gcoeff = (c(5)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(6)*(-M(152)-M(168)+M(169) &
    -M(170)+M(171)+M(184)-M(198)+M(230))) * den(181)
  T3sum(1:15,36) = T3sum(1:15,36) + Gcoeff * G2tensor(:,88)
  Gcoeff = (c(6)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(181)
  T3sum(1:15,36) = T3sum(1:15,36) + Gcoeff * G2tensor(:,133)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)-M(102)+M(104)+M(108)+M(110)-M(114)-M(116)+M(119)-M(120)-M(121)-M(122) &
    +M(123)+M(124))+c(6)*(-M(151)+M(211)-M(212)+M(229))) * den(37)
  T4sum(1:35,33) = T4sum(1:35,33) + Gcoeff * G3tensor(:,4)
  Gcoeff = (c(4)*(M(5)-M(6)-M(11)+M(12)-M(17)+M(18)-M(19)+M(20)+M(21)-M(22)+M(23)-M(24)-M(25)+M(26)+M(29)-M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)-M(102)-M(104)+M(108)+M(110)+M(114)-M(116)+M(119)+M(120)-M(121)-M(122) &
    -M(123)+M(124))+c(6)*(-M(152)+M(169)-M(170)+M(230))) * den(37)
  T4sum(1:35,33) = T4sum(1:35,33) + Gcoeff * G3tensor(:,10)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(104)+M(114)+M(120)-M(123))+c(6)*(M(151)-M(152)+M(169)-M(170) &
    -M(211)+M(212)-M(229)+M(230))) * den(37)
  T4sum(1:35,33) = T4sum(1:35,33) + Gcoeff * G3tensor(:,16)
  Gcoeff = (c(5)*(-M(46)+M(51)-M(58)+M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97))+c(6)*(-M(143)+M(144)-M(167)+M(168) &
    +M(198)+M(200)-M(240)-M(242))) * den(208)
  T3sum(1:15,34) = T3sum(1:15,34) + Gcoeff * G2tensor(:,44)
  Gcoeff = (c(5)*(-M(46)+M(51)-M(58)+M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97))+c(6)*(-M(133)+M(134)+M(197)+M(202) &
    -M(209)+M(210)-M(239)-M(244))) * den(208)
  T3sum(1:15,34) = T3sum(1:15,34) + Gcoeff * G2tensor(:,89)
  Gcoeff = (c(6)*(-M(133)+M(134)+M(143)-M(144)+M(167)-M(168)+M(197)-M(198)-M(200)+M(202)-M(209)+M(210)-M(239)+M(240)+M(242) &
    -M(244))) * den(208)
  T3sum(1:15,34) = T3sum(1:15,34) + Gcoeff * G2tensor(:,134)
  Gcoeff = (c(5)*(M(46)-M(51)-M(74)+M(95)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122))+c(6)*(M(141)-M(151)-M(197) &
    +M(209)-M(210)-M(212)+M(214)+M(239))) * den(184)
  T3sum(1:15,36) = T3sum(1:15,36) + Gcoeff * G2tensor(:,45)
  Gcoeff = (c(5)*(M(46)-M(51)-M(74)+M(95)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122))+c(6)*(M(142)-M(152)+M(167) &
    -M(168)-M(170)+M(172)-M(198)+M(240))) * den(184)
  T3sum(1:15,36) = T3sum(1:15,36) + Gcoeff * G2tensor(:,90)
  Gcoeff = (c(6)*(-M(141)+M(142)+M(151)-M(152)+M(167)-M(168)-M(170)+M(172)+M(197)-M(198)-M(209)+M(210)+M(212)-M(214)-M(239) &
    +M(240))) * den(184)
  T3sum(1:15,36) = T3sum(1:15,36) + Gcoeff * G2tensor(:,135)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(197)+M(209)-M(210)+M(239))) * den(22)
  T4sum(1:35,6) = T4sum(1:35,6) + Gcoeff * G3tensor(:,1)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(167)-M(168)-M(198)+M(240))) * den(22)
  T4sum(1:35,6) = T4sum(1:35,6) + Gcoeff * G3tensor(:,7)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(167)-M(168)+M(197)-M(198) &
    -M(209)+M(210)-M(239)+M(240))) * den(22)
  T4sum(1:35,6) = T4sum(1:35,6) + Gcoeff * G3tensor(:,13)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(6)-M(7)-M(9)-M(12)-M(14)-M(16)-M(17)-M(19)-M(21)-M(24)-M(26)-M(27)-M(29)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(44)+M(45)+M(55)+M(56)+M(58)+M(67)+M(69)+M(79)+M(80)+M(81)+M(82)+M(83)+M(85)+M(89)+M(90)+M(91)+M(94)+M(96)+M(97) &
    +M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(211)+M(229)))
  T5sum(1:70,59) = T5sum(1:70,59) + Gcoeff * G4tensor(:,2)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(9)-M(11)-M(13)-M(16)-M(17)-M(19)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(44)+M(45)+M(47)+M(55)+M(56)+M(58)+M(59)+M(60)+M(61)+M(67)+M(72)+M(79)+M(83)+M(85)+M(89)+M(90)+M(91)+M(96)+M(97)+M(99) &
    +M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(169)+M(230)))
  T5sum(1:70,59) = T5sum(1:70,59) + Gcoeff * G4tensor(:,4)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)+M(103)-M(104)-M(107)+M(109)-M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(169)-M(211)-M(229)+M(230)))
  T5sum(1:70,59) = T5sum(1:70,59) + Gcoeff * G4tensor(:,6)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(42)+M(46)+M(53)+M(55)+M(58)+M(65)+M(66)+M(69)+M(78)+M(79)+M(80)+M(81)+M(82)+M(85)+M(89)+M(91)+M(94)+M(95)+M(96)+M(97) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(209)+M(239)))
  T5sum(1:70,60) = T5sum(1:70,60) + Gcoeff * G4tensor(:,1)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(46)+M(47)+M(53)+M(55)+M(58)+M(59)+M(60)+M(61)+M(65)+M(66)+M(72)+M(78)+M(79)+M(85)+M(89)+M(91)+M(95)+M(96)+M(97)+M(99) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(167)+M(240)))
  T5sum(1:70,60) = T5sum(1:70,60) + Gcoeff * G4tensor(:,3)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(167)-M(209)-M(239)+M(240)))
  T5sum(1:70,60) = T5sum(1:70,60) + Gcoeff * G4tensor(:,5)

end subroutine vamp_2

end module ol_vamp_2_ppjjjj_gggggg_1_/**/REALKIND
