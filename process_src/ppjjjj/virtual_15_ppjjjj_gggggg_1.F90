
module ol_vamp_15_ppjjjj_gggggg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_15(M)
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
  complex(REALKIND), dimension(4,5,4,181) :: G1
  complex(REALKIND), dimension(4,15,4,101) :: G2
  complex(REALKIND), dimension(4,35,4,39) :: G3
  complex(REALKIND), dimension(4,70,4,4) :: G4
  complex(REALKIND), dimension(5,117) :: G1tensor
  complex(REALKIND), dimension(15,141) :: G2tensor
  complex(REALKIND), dimension(35,76) :: G3tensor
  complex(REALKIND), dimension(70,35) :: G4tensor
  complex(REALKIND), dimension(126,4) :: G5tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,109),G0(:,:,:,2))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-5),wf(:,-3),G0(:,:,:,3))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,1))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-3),wf(:,-5),G0(:,:,:,4))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,2))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-5),wf(:,-3),G0(:,:,:,5))
  call check_last_UV_W(l_switch,G0(:,:,:,5),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,3))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-5),wf(:,-2),G0(:,:,:,6))
  call check_last_UV_W(l_switch,G0(:,:,:,6),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,4))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-2),wf(:,-5),G0(:,:,:,7))
  call check_last_UV_W(l_switch,G0(:,:,:,7),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,5))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-5),wf(:,-2),G0(:,:,:,8))
  call check_last_UV_W(l_switch,G0(:,:,:,8),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,6))
  call loop_UV_W(G0(:,:,:,2),Q(:,19),wf(:,-5),Q(:,32),G1(:,:,:,1))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,1),wf(:,-3),wf(:,-2),G1tensor(:,7))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,1),wf(:,-2),wf(:,-3),G1tensor(:,8))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,1),wf(:,-3),wf(:,-2),G1tensor(:,9))
  call check_last_UV_W(l_switch,G1(:,:,:,1),Q(:,51),wf(:,62),Q(:,12),G2tensor(:,1))
  call loop_UV_W(G0(:,:,:,2),Q(:,19),wf(:,70),Q(:,36),G1(:,:,:,2))
  call check_last_UV_W(l_switch,G1(:,:,:,2),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,2))
  call loop_UV_W(G0(:,:,:,2),Q(:,19),wf(:,79),Q(:,40),G1(:,:,:,3))
  call check_last_UV_W(l_switch,G1(:,:,:,3),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,3))
  call loop_UV_W(G0(:,:,:,2),Q(:,19),wf(:,-3),Q(:,8),G1(:,:,:,4))
  call loop_UV_W(G1(:,:,:,4),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,1))
  call check_last_UV_W(l_switch,G2(:,:,:,1),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,1))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,109),wf(:,-1),G0(:,:,:,9))
  call loop_GGG_G_12(G0(:,:,:,9),wf(:,-5),wf(:,-3),G0(:,:,:,10))
  call check_last_UV_W(l_switch,G0(:,:,:,10),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,10))
  call loop_GGG_G_12(G0(:,:,:,9),wf(:,-3),wf(:,-5),G0(:,:,:,11))
  call check_last_UV_W(l_switch,G0(:,:,:,11),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,11))
  call loop_GGG_G_23(G0(:,:,:,9),wf(:,-5),wf(:,-3),G0(:,:,:,12))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,12))
  call loop_GGG_G_12(G0(:,:,:,9),wf(:,-5),wf(:,-2),G0(:,:,:,13))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,13))
  call loop_GGG_G_12(G0(:,:,:,9),wf(:,-2),wf(:,-5),G0(:,:,:,14))
  call check_last_UV_W(l_switch,G0(:,:,:,14),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,14))
  call loop_GGG_G_23(G0(:,:,:,9),wf(:,-5),wf(:,-2),G0(:,:,:,15))
  call check_last_UV_W(l_switch,G0(:,:,:,15),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,15))
  call loop_UV_W(G0(:,:,:,9),Q(:,19),wf(:,-5),Q(:,32),G1(:,:,:,5))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,5),wf(:,-3),wf(:,-2),G1tensor(:,16))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,5),wf(:,-2),wf(:,-3),G1tensor(:,17))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,5),wf(:,-3),wf(:,-2),G1tensor(:,18))
  call check_last_UV_W(l_switch,G1(:,:,:,5),Q(:,51),wf(:,62),Q(:,12),G2tensor(:,4))
  call loop_UV_W(G0(:,:,:,9),Q(:,19),wf(:,70),Q(:,36),G1(:,:,:,6))
  call check_last_UV_W(l_switch,G1(:,:,:,6),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,5))
  call loop_UV_W(G0(:,:,:,9),Q(:,19),wf(:,79),Q(:,40),G1(:,:,:,7))
  call check_last_UV_W(l_switch,G1(:,:,:,7),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,6))
  call loop_UV_W(G0(:,:,:,9),Q(:,19),wf(:,-3),Q(:,8),G1(:,:,:,8))
  call loop_UV_W(G1(:,:,:,8),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,2))
  call check_last_UV_W(l_switch,G2(:,:,:,2),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,2))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,109),G0(:,:,:,16))
  call loop_GGG_G_12(G0(:,:,:,16),wf(:,-5),wf(:,-3),G0(:,:,:,17))
  call check_last_UV_W(l_switch,G0(:,:,:,17),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,19))
  call loop_GGG_G_12(G0(:,:,:,16),wf(:,-3),wf(:,-5),G0(:,:,:,18))
  call check_last_UV_W(l_switch,G0(:,:,:,18),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,20))
  call loop_GGG_G_23(G0(:,:,:,16),wf(:,-5),wf(:,-3),G0(:,:,:,19))
  call check_last_UV_W(l_switch,G0(:,:,:,19),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,21))
  call loop_GGG_G_12(G0(:,:,:,16),wf(:,-5),wf(:,-2),G0(:,:,:,20))
  call check_last_UV_W(l_switch,G0(:,:,:,20),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,22))
  call loop_GGG_G_12(G0(:,:,:,16),wf(:,-2),wf(:,-5),G0(:,:,:,21))
  call check_last_UV_W(l_switch,G0(:,:,:,21),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,23))
  call loop_GGG_G_23(G0(:,:,:,16),wf(:,-5),wf(:,-2),G0(:,:,:,22))
  call check_last_UV_W(l_switch,G0(:,:,:,22),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,24))
  call loop_UV_W(G0(:,:,:,16),Q(:,19),wf(:,-5),Q(:,32),G1(:,:,:,9))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,9),wf(:,-3),wf(:,-2),G1tensor(:,25))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,9),wf(:,-2),wf(:,-3),G1tensor(:,26))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,9),wf(:,-3),wf(:,-2),G1tensor(:,27))
  call check_last_UV_W(l_switch,G1(:,:,:,9),Q(:,51),wf(:,62),Q(:,12),G2tensor(:,7))
  call loop_UV_W(G0(:,:,:,16),Q(:,19),wf(:,70),Q(:,36),G1(:,:,:,10))
  call check_last_UV_W(l_switch,G1(:,:,:,10),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,8))
  call loop_UV_W(G0(:,:,:,16),Q(:,19),wf(:,79),Q(:,40),G1(:,:,:,11))
  call check_last_UV_W(l_switch,G1(:,:,:,11),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,9))
  call loop_UV_W(G0(:,:,:,16),Q(:,19),wf(:,-3),Q(:,8),G1(:,:,:,12))
  call loop_UV_W(G1(:,:,:,12),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,3))
  call check_last_UV_W(l_switch,G2(:,:,:,3),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,3))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,66),G0(:,:,:,23))
  call loop_GGG_G_12(G0(:,:,:,23),wf(:,-5),wf(:,-1),G0(:,:,:,24))
  call check_last_UV_W(l_switch,G0(:,:,:,24),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,28))
  call loop_GGG_G_12(G0(:,:,:,23),wf(:,-1),wf(:,-5),G0(:,:,:,25))
  call check_last_UV_W(l_switch,G0(:,:,:,25),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,29))
  call loop_GGG_G_23(G0(:,:,:,23),wf(:,-5),wf(:,-1),G0(:,:,:,26))
  call check_last_UV_W(l_switch,G0(:,:,:,26),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,30))
  call loop_GGG_G_12(G0(:,:,:,23),wf(:,-5),wf(:,0),G0(:,:,:,27))
  call check_last_UV_W(l_switch,G0(:,:,:,27),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,31))
  call loop_GGG_G_12(G0(:,:,:,23),wf(:,0),wf(:,-5),G0(:,:,:,28))
  call check_last_UV_W(l_switch,G0(:,:,:,28),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,32))
  call loop_GGG_G_23(G0(:,:,:,23),wf(:,-5),wf(:,0),G0(:,:,:,29))
  call check_last_UV_W(l_switch,G0(:,:,:,29),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,33))
  call loop_UV_W(G0(:,:,:,23),Q(:,28),wf(:,-5),Q(:,32),G1(:,:,:,13))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,13),wf(:,-1),wf(:,0),G1tensor(:,34))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,13),wf(:,0),wf(:,-1),G1tensor(:,35))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,13),wf(:,-1),wf(:,0),G1tensor(:,36))
  call check_last_UV_W(l_switch,G1(:,:,:,13),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,10))
  call loop_UV_W(G0(:,:,:,23),Q(:,28),wf(:,113),Q(:,33),G1(:,:,:,14))
  call check_last_UV_W(l_switch,G1(:,:,:,14),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,11))
  call loop_UV_W(G0(:,:,:,23),Q(:,28),wf(:,99),Q(:,34),G1(:,:,:,15))
  call check_last_UV_W(l_switch,G1(:,:,:,15),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,12))
  call loop_UV_W(G0(:,:,:,23),Q(:,28),wf(:,-1),Q(:,2),G1(:,:,:,16))
  call loop_UV_W(G1(:,:,:,16),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,4))
  call check_last_UV_W(l_switch,G2(:,:,:,4),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,4))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,66),wf(:,-3),G0(:,:,:,30))
  call loop_GGG_G_12(G0(:,:,:,30),wf(:,-5),wf(:,-1),G0(:,:,:,31))
  call check_last_UV_W(l_switch,G0(:,:,:,31),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,37))
  call loop_GGG_G_12(G0(:,:,:,30),wf(:,-1),wf(:,-5),G0(:,:,:,32))
  call check_last_UV_W(l_switch,G0(:,:,:,32),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,38))
  call loop_GGG_G_23(G0(:,:,:,30),wf(:,-5),wf(:,-1),G0(:,:,:,33))
  call check_last_UV_W(l_switch,G0(:,:,:,33),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,39))
  call loop_GGG_G_12(G0(:,:,:,30),wf(:,-5),wf(:,0),G0(:,:,:,34))
  call check_last_UV_W(l_switch,G0(:,:,:,34),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,40))
  call loop_GGG_G_12(G0(:,:,:,30),wf(:,0),wf(:,-5),G0(:,:,:,35))
  call check_last_UV_W(l_switch,G0(:,:,:,35),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,41))
  call loop_GGG_G_23(G0(:,:,:,30),wf(:,-5),wf(:,0),G0(:,:,:,36))
  call check_last_UV_W(l_switch,G0(:,:,:,36),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,42))
  call loop_UV_W(G0(:,:,:,30),Q(:,28),wf(:,-5),Q(:,32),G1(:,:,:,17))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,17),wf(:,-1),wf(:,0),G1tensor(:,43))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,17),wf(:,0),wf(:,-1),G1tensor(:,44))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,17),wf(:,-1),wf(:,0),G1tensor(:,45))
  call check_last_UV_W(l_switch,G1(:,:,:,17),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,13))
  call loop_UV_W(G0(:,:,:,30),Q(:,28),wf(:,113),Q(:,33),G1(:,:,:,18))
  call check_last_UV_W(l_switch,G1(:,:,:,18),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,14))
  call loop_UV_W(G0(:,:,:,30),Q(:,28),wf(:,99),Q(:,34),G1(:,:,:,19))
  call check_last_UV_W(l_switch,G1(:,:,:,19),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,15))
  call loop_UV_W(G0(:,:,:,30),Q(:,28),wf(:,-1),Q(:,2),G1(:,:,:,20))
  call loop_UV_W(G1(:,:,:,20),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,5))
  call check_last_UV_W(l_switch,G2(:,:,:,5),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,5))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,66),G0(:,:,:,37))
  call loop_GGG_G_12(G0(:,:,:,37),wf(:,-5),wf(:,-1),G0(:,:,:,38))
  call check_last_UV_W(l_switch,G0(:,:,:,38),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,46))
  call loop_GGG_G_12(G0(:,:,:,37),wf(:,-1),wf(:,-5),G0(:,:,:,39))
  call check_last_UV_W(l_switch,G0(:,:,:,39),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,47))
  call loop_GGG_G_23(G0(:,:,:,37),wf(:,-5),wf(:,-1),G0(:,:,:,40))
  call check_last_UV_W(l_switch,G0(:,:,:,40),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,48))
  call loop_GGG_G_12(G0(:,:,:,37),wf(:,-5),wf(:,0),G0(:,:,:,41))
  call check_last_UV_W(l_switch,G0(:,:,:,41),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,49))
  call loop_GGG_G_12(G0(:,:,:,37),wf(:,0),wf(:,-5),G0(:,:,:,42))
  call check_last_UV_W(l_switch,G0(:,:,:,42),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,50))
  call loop_GGG_G_23(G0(:,:,:,37),wf(:,-5),wf(:,0),G0(:,:,:,43))
  call check_last_UV_W(l_switch,G0(:,:,:,43),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,51))
  call loop_UV_W(G0(:,:,:,37),Q(:,28),wf(:,-5),Q(:,32),G1(:,:,:,21))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,21),wf(:,-1),wf(:,0),G1tensor(:,52))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,21),wf(:,0),wf(:,-1),G1tensor(:,53))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,21),wf(:,-1),wf(:,0),G1tensor(:,54))
  call check_last_UV_W(l_switch,G1(:,:,:,21),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,16))
  call loop_UV_W(G0(:,:,:,37),Q(:,28),wf(:,113),Q(:,33),G1(:,:,:,22))
  call check_last_UV_W(l_switch,G1(:,:,:,22),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,17))
  call loop_UV_W(G0(:,:,:,37),Q(:,28),wf(:,99),Q(:,34),G1(:,:,:,23))
  call check_last_UV_W(l_switch,G1(:,:,:,23),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,18))
  call loop_UV_W(G0(:,:,:,37),Q(:,28),wf(:,-1),Q(:,2),G1(:,:,:,24))
  call loop_UV_W(G1(:,:,:,24),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,6))
  call check_last_UV_W(l_switch,G2(:,:,:,6),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,6))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,66),G0(:,:,:,44))
  call loop_GGG_G_12(G0(:,:,:,44),wf(:,-5),wf(:,-3),G0(:,:,:,45))
  call check_last_UV_W(l_switch,G0(:,:,:,45),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,55))
  call loop_GGG_G_12(G0(:,:,:,44),wf(:,-3),wf(:,-5),G0(:,:,:,46))
  call check_last_UV_W(l_switch,G0(:,:,:,46),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,56))
  call loop_GGG_G_23(G0(:,:,:,44),wf(:,-5),wf(:,-3),G0(:,:,:,47))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,57))
  call loop_GGG_G_12(G0(:,:,:,44),wf(:,-5),wf(:,0),G0(:,:,:,48))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,58))
  call loop_GGG_G_12(G0(:,:,:,44),wf(:,0),wf(:,-5),G0(:,:,:,49))
  call check_last_UV_W(l_switch,G0(:,:,:,49),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,59))
  call loop_GGG_G_23(G0(:,:,:,44),wf(:,-5),wf(:,0),G0(:,:,:,50))
  call check_last_UV_W(l_switch,G0(:,:,:,50),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,60))
  call loop_UV_W(G0(:,:,:,44),Q(:,22),wf(:,-5),Q(:,32),G1(:,:,:,25))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,25),wf(:,-3),wf(:,0),G1tensor(:,61))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,25),wf(:,0),wf(:,-3),G1tensor(:,62))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,25),wf(:,-3),wf(:,0),G1tensor(:,63))
  call check_last_UV_W(l_switch,G1(:,:,:,25),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,19))
  call loop_UV_W(G0(:,:,:,44),Q(:,22),wf(:,113),Q(:,33),G1(:,:,:,26))
  call check_last_UV_W(l_switch,G1(:,:,:,26),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,20))
  call loop_UV_W(G0(:,:,:,44),Q(:,22),wf(:,79),Q(:,40),G1(:,:,:,27))
  call check_last_UV_W(l_switch,G1(:,:,:,27),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,21))
  call loop_UV_W(G0(:,:,:,44),Q(:,22),wf(:,-3),Q(:,8),G1(:,:,:,28))
  call loop_UV_W(G1(:,:,:,28),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,7))
  call check_last_UV_W(l_switch,G2(:,:,:,7),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,7))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,66),wf(:,-1),G0(:,:,:,51))
  call loop_GGG_G_12(G0(:,:,:,51),wf(:,-5),wf(:,-3),G0(:,:,:,52))
  call check_last_UV_W(l_switch,G0(:,:,:,52),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,64))
  call loop_GGG_G_12(G0(:,:,:,51),wf(:,-3),wf(:,-5),G0(:,:,:,53))
  call check_last_UV_W(l_switch,G0(:,:,:,53),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,65))
  call loop_GGG_G_23(G0(:,:,:,51),wf(:,-5),wf(:,-3),G0(:,:,:,54))
  call check_last_UV_W(l_switch,G0(:,:,:,54),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,66))
  call loop_GGG_G_12(G0(:,:,:,51),wf(:,-5),wf(:,0),G0(:,:,:,55))
  call check_last_UV_W(l_switch,G0(:,:,:,55),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,67))
  call loop_GGG_G_12(G0(:,:,:,51),wf(:,0),wf(:,-5),G0(:,:,:,56))
  call check_last_UV_W(l_switch,G0(:,:,:,56),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,68))
  call loop_GGG_G_23(G0(:,:,:,51),wf(:,-5),wf(:,0),G0(:,:,:,57))
  call check_last_UV_W(l_switch,G0(:,:,:,57),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,69))
  call loop_UV_W(G0(:,:,:,51),Q(:,22),wf(:,-5),Q(:,32),G1(:,:,:,29))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,29),wf(:,-3),wf(:,0),G1tensor(:,70))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,29),wf(:,0),wf(:,-3),G1tensor(:,71))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,29),wf(:,-3),wf(:,0),G1tensor(:,72))
  call check_last_UV_W(l_switch,G1(:,:,:,29),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,22))
  call loop_UV_W(G0(:,:,:,51),Q(:,22),wf(:,113),Q(:,33),G1(:,:,:,30))
  call check_last_UV_W(l_switch,G1(:,:,:,30),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,23))
  call loop_UV_W(G0(:,:,:,51),Q(:,22),wf(:,79),Q(:,40),G1(:,:,:,31))
  call check_last_UV_W(l_switch,G1(:,:,:,31),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,24))
  call loop_UV_W(G0(:,:,:,51),Q(:,22),wf(:,-3),Q(:,8),G1(:,:,:,32))
  call loop_UV_W(G1(:,:,:,32),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,8))
  call check_last_UV_W(l_switch,G2(:,:,:,8),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,8))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,66),G0(:,:,:,58))
  call loop_GGG_G_12(G0(:,:,:,58),wf(:,-5),wf(:,-3),G0(:,:,:,59))
  call check_last_UV_W(l_switch,G0(:,:,:,59),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,73))
  call loop_GGG_G_12(G0(:,:,:,58),wf(:,-3),wf(:,-5),G0(:,:,:,60))
  call check_last_UV_W(l_switch,G0(:,:,:,60),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,74))
  call loop_GGG_G_23(G0(:,:,:,58),wf(:,-5),wf(:,-3),G0(:,:,:,61))
  call check_last_UV_W(l_switch,G0(:,:,:,61),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,75))
  call loop_GGG_G_12(G0(:,:,:,58),wf(:,-5),wf(:,0),G0(:,:,:,62))
  call check_last_UV_W(l_switch,G0(:,:,:,62),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,76))
  call loop_GGG_G_12(G0(:,:,:,58),wf(:,0),wf(:,-5),G0(:,:,:,63))
  call check_last_UV_W(l_switch,G0(:,:,:,63),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,77))
  call loop_GGG_G_23(G0(:,:,:,58),wf(:,-5),wf(:,0),G0(:,:,:,64))
  call check_last_UV_W(l_switch,G0(:,:,:,64),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,78))
  call loop_UV_W(G0(:,:,:,58),Q(:,22),wf(:,-5),Q(:,32),G1(:,:,:,33))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,33),wf(:,-3),wf(:,0),G1tensor(:,79))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,33),wf(:,0),wf(:,-3),G1tensor(:,80))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,33),wf(:,-3),wf(:,0),G1tensor(:,81))
  call check_last_UV_W(l_switch,G1(:,:,:,33),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,25))
  call loop_UV_W(G0(:,:,:,58),Q(:,22),wf(:,113),Q(:,33),G1(:,:,:,34))
  call check_last_UV_W(l_switch,G1(:,:,:,34),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,26))
  call loop_UV_W(G0(:,:,:,58),Q(:,22),wf(:,79),Q(:,40),G1(:,:,:,35))
  call check_last_UV_W(l_switch,G1(:,:,:,35),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,27))
  call loop_UV_W(G0(:,:,:,58),Q(:,22),wf(:,-3),Q(:,8),G1(:,:,:,36))
  call loop_UV_W(G1(:,:,:,36),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,9))
  call check_last_UV_W(l_switch,G2(:,:,:,9),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,9))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,104),Q(:,9),G1(:,:,:,37))
  call loop_GGG_G_12(G1(:,:,:,37),wf(:,-5),wf(:,-4),G1(:,:,:,38))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,38),wf(:,-2),wf(:,-1),G1tensor(:,82))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,38),wf(:,-1),wf(:,-2),G1tensor(:,83))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,38),wf(:,-2),wf(:,-1),G1tensor(:,84))
  call check_last_UV_W(l_switch,G1(:,:,:,38),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,28))
  call loop_GGG_G_12(G1(:,:,:,37),wf(:,-4),wf(:,-5),G1(:,:,:,39))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,39),wf(:,-2),wf(:,-1),G1tensor(:,85))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,39),wf(:,-1),wf(:,-2),G1tensor(:,86))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,39),wf(:,-2),wf(:,-1),G1tensor(:,87))
  call check_last_UV_W(l_switch,G1(:,:,:,39),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,29))
  call loop_GGG_G_23(G1(:,:,:,37),wf(:,-5),wf(:,-4),G1(:,:,:,40))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,40),wf(:,-2),wf(:,-1),G1tensor(:,88))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,40),wf(:,-1),wf(:,-2),G1tensor(:,89))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,40),wf(:,-2),wf(:,-1),G1tensor(:,90))
  call check_last_UV_W(l_switch,G1(:,:,:,40),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,30))
  call loop_GGG_G_12(G1(:,:,:,37),wf(:,-5),wf(:,95),G1(:,:,:,41))
  call check_last_UV_W(l_switch,G1(:,:,:,41),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,31))
  call loop_GGG_G_12(G1(:,:,:,37),wf(:,95),wf(:,-5),G1(:,:,:,42))
  call check_last_UV_W(l_switch,G1(:,:,:,42),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,32))
  call loop_GGG_G_23(G1(:,:,:,37),wf(:,-5),wf(:,95),G1(:,:,:,43))
  call check_last_UV_W(l_switch,G1(:,:,:,43),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,33))
  call loop_GGG_G_12(G1(:,:,:,37),wf(:,-5),wf(:,66),G1(:,:,:,44))
  call check_last_UV_W(l_switch,G1(:,:,:,44),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,34))
  call loop_GGG_G_12(G1(:,:,:,37),wf(:,66),wf(:,-5),G1(:,:,:,45))
  call check_last_UV_W(l_switch,G1(:,:,:,45),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,35))
  call loop_GGG_G_23(G1(:,:,:,37),wf(:,-5),wf(:,66),G1(:,:,:,46))
  call check_last_UV_W(l_switch,G1(:,:,:,46),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,36))
  call loop_GGG_G_12(G1(:,:,:,37),wf(:,-4),wf(:,99),G1(:,:,:,47))
  call check_last_UV_W(l_switch,G1(:,:,:,47),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,37))
  call loop_GGG_G_12(G1(:,:,:,37),wf(:,99),wf(:,-4),G1(:,:,:,48))
  call check_last_UV_W(l_switch,G1(:,:,:,48),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,38))
  call loop_GGG_G_23(G1(:,:,:,37),wf(:,-4),wf(:,99),G1(:,:,:,49))
  call check_last_UV_W(l_switch,G1(:,:,:,49),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,39))
  call loop_GGG_G_12(G1(:,:,:,37),wf(:,-4),wf(:,70),G1(:,:,:,50))
  call check_last_UV_W(l_switch,G1(:,:,:,50),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,40))
  call loop_GGG_G_12(G1(:,:,:,37),wf(:,70),wf(:,-4),G1(:,:,:,51))
  call check_last_UV_W(l_switch,G1(:,:,:,51),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,41))
  call loop_GGG_G_23(G1(:,:,:,37),wf(:,-4),wf(:,70),G1(:,:,:,52))
  call check_last_UV_W(l_switch,G1(:,:,:,52),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,42))
  call loop_UV_W(G1(:,:,:,37),Q(:,9),wf(:,-2),Q(:,4),G2(:,:,:,10))
  call loop_GGG_G_12(G2(:,:,:,10),wf(:,-5),wf(:,-4),G2(:,:,:,11))
  call check_last_UV_W(l_switch,G2(:,:,:,11),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,10))
  call loop_GGG_G_12(G2(:,:,:,10),wf(:,-4),wf(:,-5),G2(:,:,:,12))
  call check_last_UV_W(l_switch,G2(:,:,:,12),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,11))
  call loop_GGG_G_23(G2(:,:,:,10),wf(:,-5),wf(:,-4),G2(:,:,:,13))
  call check_last_UV_W(l_switch,G2(:,:,:,13),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,12))
  call loop_UV_W(G2(:,:,:,10),Q(:,13),wf(:,84),Q(:,48),G3(:,:,:,1))
  call check_last_UV_W(l_switch,G3(:,:,:,1),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,1))
  call loop_UV_W(G2(:,:,:,10),Q(:,13),wf(:,-5),Q(:,32),G3(:,:,:,2))
  call loop_UV_W(G3(:,:,:,2),Q(:,45),wf(:,-4),Q(:,16),G4(:,:,:,1))
  call check_last_UV_W(l_switch,G4(:,:,:,1),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,1))
  call loop_UV_W(G2(:,:,:,10),Q(:,13),wf(:,-4),Q(:,16),G3(:,:,:,3))
  call loop_UV_W(G3(:,:,:,3),Q(:,29),wf(:,-5),Q(:,32),G4(:,:,:,2))
  call check_last_UV_W(l_switch,G4(:,:,:,2),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,2))
  call loop_GGG_G_12(G1(:,:,:,37),wf(:,-2),wf(:,84),G1(:,:,:,53))
  call check_last_UV_W(l_switch,G1(:,:,:,53),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,43))
  call loop_GGG_G_12(G1(:,:,:,37),wf(:,84),wf(:,-2),G1(:,:,:,54))
  call check_last_UV_W(l_switch,G1(:,:,:,54),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,44))
  call loop_GGG_G_23(G1(:,:,:,37),wf(:,-2),wf(:,84),G1(:,:,:,55))
  call check_last_UV_W(l_switch,G1(:,:,:,55),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,45))
  call loop_GGG_G_12(G1(:,:,:,37),wf(:,-5),wf(:,-2),G1(:,:,:,56))
  call loop_UV_W(G1(:,:,:,56),Q(:,45),wf(:,-4),Q(:,16),G2(:,:,:,14))
  call check_last_UV_W(l_switch,G2(:,:,:,14),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,13))
  call loop_GGG_G_12(G1(:,:,:,37),wf(:,-2),wf(:,-5),G1(:,:,:,57))
  call loop_UV_W(G1(:,:,:,57),Q(:,45),wf(:,-4),Q(:,16),G2(:,:,:,15))
  call check_last_UV_W(l_switch,G2(:,:,:,15),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,14))
  call loop_GGG_G_23(G1(:,:,:,37),wf(:,-5),wf(:,-2),G1(:,:,:,58))
  call loop_UV_W(G1(:,:,:,58),Q(:,45),wf(:,-4),Q(:,16),G2(:,:,:,16))
  call check_last_UV_W(l_switch,G2(:,:,:,16),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,15))
  call loop_UV_W(G1(:,:,:,37),Q(:,9),wf(:,-4),Q(:,16),G2(:,:,:,17))
  call loop_GGG_G_12(G2(:,:,:,17),wf(:,-5),wf(:,-2),G2(:,:,:,18))
  call check_last_UV_W(l_switch,G2(:,:,:,18),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,16))
  call loop_GGG_G_12(G2(:,:,:,17),wf(:,-2),wf(:,-5),G2(:,:,:,19))
  call check_last_UV_W(l_switch,G2(:,:,:,19),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,17))
  call loop_GGG_G_23(G2(:,:,:,17),wf(:,-5),wf(:,-2),G2(:,:,:,20))
  call check_last_UV_W(l_switch,G2(:,:,:,20),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,18))
  call loop_GGG_G_12(G2(:,:,:,17),wf(:,-5),wf(:,-1),G2(:,:,:,21))
  call check_last_UV_W(l_switch,G2(:,:,:,21),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,19))
  call loop_GGG_G_12(G2(:,:,:,17),wf(:,-1),wf(:,-5),G2(:,:,:,22))
  call check_last_UV_W(l_switch,G2(:,:,:,22),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,20))
  call loop_GGG_G_23(G2(:,:,:,17),wf(:,-5),wf(:,-1),G2(:,:,:,23))
  call check_last_UV_W(l_switch,G2(:,:,:,23),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,21))
  call loop_UV_W(G2(:,:,:,17),Q(:,25),wf(:,-5),Q(:,32),G3(:,:,:,4))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,4),wf(:,-2),wf(:,-1),G3tensor(:,22))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,4),wf(:,-1),wf(:,-2),G3tensor(:,23))
  call check_last_GGG_G_23(l_switch,G3(:,:,:,4),wf(:,-2),wf(:,-1),G3tensor(:,24))
  call check_last_UV_W(l_switch,G3(:,:,:,4),Q(:,57),wf(:,105),Q(:,6),G4tensor(:,2))
  call loop_UV_W(G2(:,:,:,17),Q(:,25),wf(:,99),Q(:,34),G3(:,:,:,5))
  call check_last_UV_W(l_switch,G3(:,:,:,5),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,3))
  call loop_UV_W(G2(:,:,:,17),Q(:,25),wf(:,70),Q(:,36),G3(:,:,:,6))
  call check_last_UV_W(l_switch,G3(:,:,:,6),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,4))
  call loop_UV_W(G2(:,:,:,17),Q(:,25),wf(:,-2),Q(:,4),G3(:,:,:,7))
  call loop_UV_W(G3(:,:,:,7),Q(:,29),wf(:,-5),Q(:,32),G4(:,:,:,3))
  call check_last_UV_W(l_switch,G4(:,:,:,3),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,3))
  call loop_GGG_G_12(G1(:,:,:,37),wf(:,-4),wf(:,-2),G1(:,:,:,59))
  call loop_UV_W(G1(:,:,:,59),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,24))
  call check_last_UV_W(l_switch,G2(:,:,:,24),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,25))
  call loop_GGG_G_12(G1(:,:,:,37),wf(:,-2),wf(:,-4),G1(:,:,:,60))
  call loop_UV_W(G1(:,:,:,60),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,25))
  call check_last_UV_W(l_switch,G2(:,:,:,25),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,26))
  call loop_GGG_G_23(G1(:,:,:,37),wf(:,-4),wf(:,-2),G1(:,:,:,61))
  call loop_UV_W(G1(:,:,:,61),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,26))
  call check_last_UV_W(l_switch,G2(:,:,:,26),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,27))
  call loop_UV_W(G1(:,:,:,37),Q(:,9),wf(:,-5),Q(:,32),G2(:,:,:,27))
  call loop_GGG_G_12(G2(:,:,:,27),wf(:,-4),wf(:,-2),G2(:,:,:,28))
  call check_last_UV_W(l_switch,G2(:,:,:,28),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,28))
  call loop_GGG_G_12(G2(:,:,:,27),wf(:,-2),wf(:,-4),G2(:,:,:,29))
  call check_last_UV_W(l_switch,G2(:,:,:,29),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,29))
  call loop_GGG_G_23(G2(:,:,:,27),wf(:,-4),wf(:,-2),G2(:,:,:,30))
  call check_last_UV_W(l_switch,G2(:,:,:,30),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,30))
  call loop_GGG_G_12(G2(:,:,:,27),wf(:,-4),wf(:,-1),G2(:,:,:,31))
  call check_last_UV_W(l_switch,G2(:,:,:,31),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,31))
  call loop_GGG_G_12(G2(:,:,:,27),wf(:,-1),wf(:,-4),G2(:,:,:,32))
  call check_last_UV_W(l_switch,G2(:,:,:,32),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,32))
  call loop_GGG_G_23(G2(:,:,:,27),wf(:,-4),wf(:,-1),G2(:,:,:,33))
  call check_last_UV_W(l_switch,G2(:,:,:,33),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,33))
  call loop_UV_W(G2(:,:,:,27),Q(:,41),wf(:,-4),Q(:,16),G3(:,:,:,8))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,8),wf(:,-2),wf(:,-1),G3tensor(:,34))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,8),wf(:,-1),wf(:,-2),G3tensor(:,35))
  call check_last_GGG_G_23(l_switch,G3(:,:,:,8),wf(:,-2),wf(:,-1),G3tensor(:,36))
  call check_last_UV_W(l_switch,G3(:,:,:,8),Q(:,57),wf(:,105),Q(:,6),G4tensor(:,5))
  call loop_UV_W(G2(:,:,:,27),Q(:,41),wf(:,95),Q(:,18),G3(:,:,:,9))
  call check_last_UV_W(l_switch,G3(:,:,:,9),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,6))
  call loop_UV_W(G2(:,:,:,27),Q(:,41),wf(:,66),Q(:,20),G3(:,:,:,10))
  call check_last_UV_W(l_switch,G3(:,:,:,10),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,7))
  call loop_UV_W(G2(:,:,:,27),Q(:,41),wf(:,-2),Q(:,4),G3(:,:,:,11))
  call loop_UV_W(G3(:,:,:,11),Q(:,45),wf(:,-4),Q(:,16),G4(:,:,:,4))
  call check_last_UV_W(l_switch,G4(:,:,:,4),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,4))
  call loop_UV_W(G1(:,:,:,37),Q(:,9),wf(:,8),Q(:,52),G2(:,:,:,34))
  call check_last_UV_W(l_switch,G2(:,:,:,34),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,37))
  call loop_UV_W(G1(:,:,:,37),Q(:,9),wf(:,11),Q(:,52),G2(:,:,:,35))
  call check_last_UV_W(l_switch,G2(:,:,:,35),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,38))
  call loop_UV_W(G1(:,:,:,37),Q(:,9),wf(:,12),Q(:,52),G2(:,:,:,36))
  call check_last_UV_W(l_switch,G2(:,:,:,36),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,39))
  call loop_GGG_G_12(G1(:,:,:,37),wf(:,-1),wf(:,84),G1(:,:,:,62))
  call check_last_UV_W(l_switch,G1(:,:,:,62),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,46))
  call loop_GGG_G_12(G1(:,:,:,37),wf(:,84),wf(:,-1),G1(:,:,:,63))
  call check_last_UV_W(l_switch,G1(:,:,:,63),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,47))
  call loop_GGG_G_23(G1(:,:,:,37),wf(:,-1),wf(:,84),G1(:,:,:,64))
  call check_last_UV_W(l_switch,G1(:,:,:,64),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,48))
  call loop_GGG_G_12(G1(:,:,:,37),wf(:,-5),wf(:,-1),G1(:,:,:,65))
  call loop_UV_W(G1(:,:,:,65),Q(:,43),wf(:,-4),Q(:,16),G2(:,:,:,37))
  call check_last_UV_W(l_switch,G2(:,:,:,37),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,40))
  call loop_GGG_G_12(G1(:,:,:,37),wf(:,-1),wf(:,-5),G1(:,:,:,66))
  call loop_UV_W(G1(:,:,:,66),Q(:,43),wf(:,-4),Q(:,16),G2(:,:,:,38))
  call check_last_UV_W(l_switch,G2(:,:,:,38),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,41))
  call loop_GGG_G_23(G1(:,:,:,37),wf(:,-5),wf(:,-1),G1(:,:,:,67))
  call loop_UV_W(G1(:,:,:,67),Q(:,43),wf(:,-4),Q(:,16),G2(:,:,:,39))
  call check_last_UV_W(l_switch,G2(:,:,:,39),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,42))
  call loop_GGG_G_12(G1(:,:,:,37),wf(:,-4),wf(:,-1),G1(:,:,:,68))
  call loop_UV_W(G1(:,:,:,68),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,40))
  call check_last_UV_W(l_switch,G2(:,:,:,40),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,43))
  call loop_GGG_G_12(G1(:,:,:,37),wf(:,-1),wf(:,-4),G1(:,:,:,69))
  call loop_UV_W(G1(:,:,:,69),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,41))
  call check_last_UV_W(l_switch,G2(:,:,:,41),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,44))
  call loop_GGG_G_23(G1(:,:,:,37),wf(:,-4),wf(:,-1),G1(:,:,:,70))
  call loop_UV_W(G1(:,:,:,70),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,42))
  call check_last_UV_W(l_switch,G2(:,:,:,42),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,45))
  call loop_UV_W(G1(:,:,:,37),Q(:,9),wf(:,26),Q(:,50),G2(:,:,:,43))
  call check_last_UV_W(l_switch,G2(:,:,:,43),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,46))
  call loop_UV_W(G1(:,:,:,37),Q(:,9),wf(:,29),Q(:,50),G2(:,:,:,44))
  call check_last_UV_W(l_switch,G2(:,:,:,44),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,47))
  call loop_UV_W(G1(:,:,:,37),Q(:,9),wf(:,30),Q(:,50),G2(:,:,:,45))
  call check_last_UV_W(l_switch,G2(:,:,:,45),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,48))
  call loop_UV_W(G1(:,:,:,37),Q(:,9),wf(:,84),Q(:,48),G2(:,:,:,46))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,46),wf(:,-2),wf(:,-1),G2tensor(:,49))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,46),wf(:,-1),wf(:,-2),G2tensor(:,50))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,46),wf(:,-2),wf(:,-1),G2tensor(:,51))
  call check_last_UV_W(l_switch,G2(:,:,:,46),Q(:,57),wf(:,105),Q(:,6),G3tensor(:,49))
  call loop_UV_W(G1(:,:,:,37),Q(:,9),wf(:,247),Q(:,50),G2(:,:,:,47))
  call check_last_UV_W(l_switch,G2(:,:,:,47),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,50))
  call loop_UV_W(G1(:,:,:,37),Q(:,9),wf(:,95),Q(:,18),G2(:,:,:,48))
  call loop_UV_W(G2(:,:,:,48),Q(:,27),wf(:,-5),Q(:,32),G3(:,:,:,12))
  call check_last_UV_W(l_switch,G3(:,:,:,12),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,8))
  call loop_UV_W(G1(:,:,:,37),Q(:,9),wf(:,265),Q(:,52),G2(:,:,:,49))
  call check_last_UV_W(l_switch,G2(:,:,:,49),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,51))
  call loop_UV_W(G1(:,:,:,37),Q(:,9),wf(:,66),Q(:,20),G2(:,:,:,50))
  call loop_UV_W(G2(:,:,:,50),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,13))
  call check_last_UV_W(l_switch,G3(:,:,:,13),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,9))
  call loop_UV_W(G1(:,:,:,37),Q(:,9),wf(:,248),Q(:,50),G2(:,:,:,51))
  call check_last_UV_W(l_switch,G2(:,:,:,51),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,52))
  call loop_UV_W(G1(:,:,:,37),Q(:,9),wf(:,99),Q(:,34),G2(:,:,:,52))
  call loop_UV_W(G2(:,:,:,52),Q(:,43),wf(:,-4),Q(:,16),G3(:,:,:,14))
  call check_last_UV_W(l_switch,G3(:,:,:,14),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,10))
  call loop_UV_W(G1(:,:,:,37),Q(:,9),wf(:,266),Q(:,52),G2(:,:,:,53))
  call check_last_UV_W(l_switch,G2(:,:,:,53),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,53))
  call loop_UV_W(G1(:,:,:,37),Q(:,9),wf(:,70),Q(:,36),G2(:,:,:,54))
  call loop_UV_W(G2(:,:,:,54),Q(:,45),wf(:,-4),Q(:,16),G3(:,:,:,15))
  call check_last_UV_W(l_switch,G3(:,:,:,15),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,11))
  call loop_UV_W(G1(:,:,:,37),Q(:,9),wf(:,249),Q(:,50),G2(:,:,:,55))
  call check_last_UV_W(l_switch,G2(:,:,:,55),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,54))
  call loop_UV_W(G1(:,:,:,37),Q(:,9),wf(:,267),Q(:,52),G2(:,:,:,56))
  call check_last_UV_W(l_switch,G2(:,:,:,56),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,55))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,-1),G0(:,:,:,65))
  call loop_GGG_G_12(G0(:,:,:,65),wf(:,-5),wf(:,75),G0(:,:,:,66))
  call check_last_UV_W(l_switch,G0(:,:,:,66),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,91))
  call loop_GGG_G_12(G0(:,:,:,65),wf(:,75),wf(:,-5),G0(:,:,:,67))
  call check_last_UV_W(l_switch,G0(:,:,:,67),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,92))
  call loop_GGG_G_23(G0(:,:,:,65),wf(:,-5),wf(:,75),G0(:,:,:,68))
  call check_last_UV_W(l_switch,G0(:,:,:,68),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,93))
  call loop_GGG_G_12(G0(:,:,:,65),wf(:,-4),wf(:,79),G0(:,:,:,69))
  call check_last_UV_W(l_switch,G0(:,:,:,69),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,94))
  call loop_GGG_G_12(G0(:,:,:,65),wf(:,79),wf(:,-4),G0(:,:,:,70))
  call check_last_UV_W(l_switch,G0(:,:,:,70),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,95))
  call loop_GGG_G_23(G0(:,:,:,65),wf(:,-4),wf(:,79),G0(:,:,:,71))
  call check_last_UV_W(l_switch,G0(:,:,:,71),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,96))
  call loop_GGG_G_12(G0(:,:,:,65),wf(:,-5),wf(:,-4),G0(:,:,:,72))
  call loop_UV_W(G0(:,:,:,72),Q(:,54),wf(:,-3),Q(:,8),G1(:,:,:,71))
  call check_last_UV_W(l_switch,G1(:,:,:,71),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,52))
  call loop_GGG_G_12(G0(:,:,:,65),wf(:,-4),wf(:,-5),G0(:,:,:,73))
  call loop_UV_W(G0(:,:,:,73),Q(:,54),wf(:,-3),Q(:,8),G1(:,:,:,72))
  call check_last_UV_W(l_switch,G1(:,:,:,72),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,53))
  call loop_GGG_G_23(G0(:,:,:,65),wf(:,-5),wf(:,-4),G0(:,:,:,74))
  call loop_UV_W(G0(:,:,:,74),Q(:,54),wf(:,-3),Q(:,8),G1(:,:,:,73))
  call check_last_UV_W(l_switch,G1(:,:,:,73),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,54))
  call loop_UV_W(G0(:,:,:,65),Q(:,6),wf(:,-3),Q(:,8),G1(:,:,:,74))
  call loop_GGG_G_12(G1(:,:,:,74),wf(:,-5),wf(:,-4),G1(:,:,:,75))
  call check_last_UV_W(l_switch,G1(:,:,:,75),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,55))
  call loop_GGG_G_12(G1(:,:,:,74),wf(:,-4),wf(:,-5),G1(:,:,:,76))
  call check_last_UV_W(l_switch,G1(:,:,:,76),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,56))
  call loop_GGG_G_23(G1(:,:,:,74),wf(:,-5),wf(:,-4),G1(:,:,:,77))
  call check_last_UV_W(l_switch,G1(:,:,:,77),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,57))
  call loop_UV_W(G1(:,:,:,74),Q(:,14),wf(:,84),Q(:,48),G2(:,:,:,57))
  call check_last_UV_W(l_switch,G2(:,:,:,57),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,56))
  call loop_UV_W(G1(:,:,:,74),Q(:,14),wf(:,-5),Q(:,32),G2(:,:,:,58))
  call loop_UV_W(G2(:,:,:,58),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,16))
  call check_last_UV_W(l_switch,G3(:,:,:,16),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,12))
  call loop_UV_W(G1(:,:,:,74),Q(:,14),wf(:,-4),Q(:,16),G2(:,:,:,59))
  call loop_UV_W(G2(:,:,:,59),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,17))
  call check_last_UV_W(l_switch,G3(:,:,:,17),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,13))
  call loop_GGG_G_12(G0(:,:,:,65),wf(:,-3),wf(:,84),G0(:,:,:,75))
  call check_last_UV_W(l_switch,G0(:,:,:,75),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,97))
  call loop_GGG_G_12(G0(:,:,:,65),wf(:,84),wf(:,-3),G0(:,:,:,76))
  call check_last_UV_W(l_switch,G0(:,:,:,76),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,98))
  call loop_GGG_G_23(G0(:,:,:,65),wf(:,-3),wf(:,84),G0(:,:,:,77))
  call check_last_UV_W(l_switch,G0(:,:,:,77),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,99))
  call loop_GGG_G_12(G0(:,:,:,65),wf(:,-5),wf(:,-3),G0(:,:,:,78))
  call loop_UV_W(G0(:,:,:,78),Q(:,46),wf(:,-4),Q(:,16),G1(:,:,:,78))
  call check_last_UV_W(l_switch,G1(:,:,:,78),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,58))
  call loop_GGG_G_12(G0(:,:,:,65),wf(:,-3),wf(:,-5),G0(:,:,:,79))
  call loop_UV_W(G0(:,:,:,79),Q(:,46),wf(:,-4),Q(:,16),G1(:,:,:,79))
  call check_last_UV_W(l_switch,G1(:,:,:,79),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,59))
  call loop_GGG_G_23(G0(:,:,:,65),wf(:,-5),wf(:,-3),G0(:,:,:,80))
  call loop_UV_W(G0(:,:,:,80),Q(:,46),wf(:,-4),Q(:,16),G1(:,:,:,80))
  call check_last_UV_W(l_switch,G1(:,:,:,80),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,60))
  call loop_UV_W(G0(:,:,:,65),Q(:,6),wf(:,-4),Q(:,16),G1(:,:,:,81))
  call loop_GGG_G_12(G1(:,:,:,81),wf(:,-5),wf(:,-3),G1(:,:,:,82))
  call check_last_UV_W(l_switch,G1(:,:,:,82),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,61))
  call loop_GGG_G_12(G1(:,:,:,81),wf(:,-3),wf(:,-5),G1(:,:,:,83))
  call check_last_UV_W(l_switch,G1(:,:,:,83),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,62))
  call loop_GGG_G_23(G1(:,:,:,81),wf(:,-5),wf(:,-3),G1(:,:,:,84))
  call check_last_UV_W(l_switch,G1(:,:,:,84),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,63))
  call loop_UV_W(G1(:,:,:,81),Q(:,22),wf(:,79),Q(:,40),G2(:,:,:,60))
  call check_last_UV_W(l_switch,G2(:,:,:,60),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,57))
  call loop_UV_W(G1(:,:,:,81),Q(:,22),wf(:,-5),Q(:,32),G2(:,:,:,61))
  call loop_UV_W(G2(:,:,:,61),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,18))
  call check_last_UV_W(l_switch,G3(:,:,:,18),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,14))
  call loop_UV_W(G1(:,:,:,81),Q(:,22),wf(:,-3),Q(:,8),G2(:,:,:,62))
  call loop_UV_W(G2(:,:,:,62),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,19))
  call check_last_UV_W(l_switch,G3(:,:,:,19),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,15))
  call loop_GGG_G_12(G0(:,:,:,65),wf(:,-4),wf(:,-3),G0(:,:,:,81))
  call loop_UV_W(G0(:,:,:,81),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,85))
  call check_last_UV_W(l_switch,G1(:,:,:,85),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,64))
  call loop_GGG_G_12(G0(:,:,:,65),wf(:,-3),wf(:,-4),G0(:,:,:,82))
  call loop_UV_W(G0(:,:,:,82),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,86))
  call check_last_UV_W(l_switch,G1(:,:,:,86),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,65))
  call loop_GGG_G_23(G0(:,:,:,65),wf(:,-4),wf(:,-3),G0(:,:,:,83))
  call loop_UV_W(G0(:,:,:,83),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,87))
  call check_last_UV_W(l_switch,G1(:,:,:,87),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,66))
  call loop_UV_W(G0(:,:,:,65),Q(:,6),wf(:,-5),Q(:,32),G1(:,:,:,88))
  call loop_GGG_G_12(G1(:,:,:,88),wf(:,-4),wf(:,-3),G1(:,:,:,89))
  call check_last_UV_W(l_switch,G1(:,:,:,89),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,67))
  call loop_GGG_G_12(G1(:,:,:,88),wf(:,-3),wf(:,-4),G1(:,:,:,90))
  call check_last_UV_W(l_switch,G1(:,:,:,90),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,68))
  call loop_GGG_G_23(G1(:,:,:,88),wf(:,-4),wf(:,-3),G1(:,:,:,91))
  call check_last_UV_W(l_switch,G1(:,:,:,91),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,69))
  call loop_UV_W(G1(:,:,:,88),Q(:,38),wf(:,75),Q(:,24),G2(:,:,:,63))
  call check_last_UV_W(l_switch,G2(:,:,:,63),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,58))
  call loop_UV_W(G1(:,:,:,88),Q(:,38),wf(:,-4),Q(:,16),G2(:,:,:,64))
  call loop_UV_W(G2(:,:,:,64),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,20))
  call check_last_UV_W(l_switch,G3(:,:,:,20),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,16))
  call loop_UV_W(G1(:,:,:,88),Q(:,38),wf(:,-3),Q(:,8),G2(:,:,:,65))
  call loop_UV_W(G2(:,:,:,65),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,21))
  call check_last_UV_W(l_switch,G3(:,:,:,21),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,17))
  call loop_UV_W(G0(:,:,:,65),Q(:,6),wf(:,2),Q(:,56),G1(:,:,:,92))
  call check_last_UV_W(l_switch,G1(:,:,:,92),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,70))
  call loop_UV_W(G0(:,:,:,65),Q(:,6),wf(:,5),Q(:,56),G1(:,:,:,93))
  call check_last_UV_W(l_switch,G1(:,:,:,93),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,71))
  call loop_UV_W(G0(:,:,:,65),Q(:,6),wf(:,6),Q(:,56),G1(:,:,:,94))
  call check_last_UV_W(l_switch,G1(:,:,:,94),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,72))
  call loop_UV_W(G0(:,:,:,65),Q(:,6),wf(:,268),Q(:,56),G1(:,:,:,95))
  call check_last_UV_W(l_switch,G1(:,:,:,95),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,73))
  call loop_UV_W(G0(:,:,:,65),Q(:,6),wf(:,75),Q(:,24),G1(:,:,:,96))
  call loop_UV_W(G1(:,:,:,96),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,66))
  call check_last_UV_W(l_switch,G2(:,:,:,66),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,59))
  call loop_UV_W(G0(:,:,:,65),Q(:,6),wf(:,269),Q(:,56),G1(:,:,:,97))
  call check_last_UV_W(l_switch,G1(:,:,:,97),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,74))
  call loop_UV_W(G0(:,:,:,65),Q(:,6),wf(:,79),Q(:,40),G1(:,:,:,98))
  call loop_UV_W(G1(:,:,:,98),Q(:,46),wf(:,-4),Q(:,16),G2(:,:,:,67))
  call check_last_UV_W(l_switch,G2(:,:,:,67),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,60))
  call loop_UV_W(G0(:,:,:,65),Q(:,6),wf(:,270),Q(:,56),G1(:,:,:,99))
  call check_last_UV_W(l_switch,G1(:,:,:,99),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,75))
  call loop_UV_W(G0(:,:,:,65),Q(:,6),wf(:,84),Q(:,48),G1(:,:,:,100))
  call loop_UV_W(G1(:,:,:,100),Q(:,54),wf(:,-3),Q(:,8),G2(:,:,:,68))
  call check_last_UV_W(l_switch,G2(:,:,:,68),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,61))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,-2),G0(:,:,:,84))
  call loop_GGG_G_12(G0(:,:,:,84),wf(:,-5),wf(:,75),G0(:,:,:,85))
  call check_last_UV_W(l_switch,G0(:,:,:,85),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,100))
  call loop_GGG_G_12(G0(:,:,:,84),wf(:,75),wf(:,-5),G0(:,:,:,86))
  call check_last_UV_W(l_switch,G0(:,:,:,86),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,101))
  call loop_GGG_G_23(G0(:,:,:,84),wf(:,-5),wf(:,75),G0(:,:,:,87))
  call check_last_UV_W(l_switch,G0(:,:,:,87),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,102))
  call loop_GGG_G_12(G0(:,:,:,84),wf(:,-4),wf(:,79),G0(:,:,:,88))
  call check_last_UV_W(l_switch,G0(:,:,:,88),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,103))
  call loop_GGG_G_12(G0(:,:,:,84),wf(:,79),wf(:,-4),G0(:,:,:,89))
  call check_last_UV_W(l_switch,G0(:,:,:,89),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,104))
  call loop_GGG_G_23(G0(:,:,:,84),wf(:,-4),wf(:,79),G0(:,:,:,90))
  call check_last_UV_W(l_switch,G0(:,:,:,90),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,105))
  call loop_GGG_G_12(G0(:,:,:,84),wf(:,-5),wf(:,-4),G0(:,:,:,91))
  call loop_UV_W(G0(:,:,:,91),Q(:,54),wf(:,-3),Q(:,8),G1(:,:,:,101))
  call check_last_UV_W(l_switch,G1(:,:,:,101),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,76))
  call loop_GGG_G_12(G0(:,:,:,84),wf(:,-4),wf(:,-5),G0(:,:,:,92))
  call loop_UV_W(G0(:,:,:,92),Q(:,54),wf(:,-3),Q(:,8),G1(:,:,:,102))
  call check_last_UV_W(l_switch,G1(:,:,:,102),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,77))
  call loop_GGG_G_23(G0(:,:,:,84),wf(:,-5),wf(:,-4),G0(:,:,:,93))
  call loop_UV_W(G0(:,:,:,93),Q(:,54),wf(:,-3),Q(:,8),G1(:,:,:,103))
  call check_last_UV_W(l_switch,G1(:,:,:,103),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,78))
  call loop_UV_W(G0(:,:,:,84),Q(:,6),wf(:,-3),Q(:,8),G1(:,:,:,104))
  call loop_GGG_G_12(G1(:,:,:,104),wf(:,-5),wf(:,-4),G1(:,:,:,105))
  call check_last_UV_W(l_switch,G1(:,:,:,105),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,79))
  call loop_GGG_G_12(G1(:,:,:,104),wf(:,-4),wf(:,-5),G1(:,:,:,106))
  call check_last_UV_W(l_switch,G1(:,:,:,106),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,80))
  call loop_GGG_G_23(G1(:,:,:,104),wf(:,-5),wf(:,-4),G1(:,:,:,107))
  call check_last_UV_W(l_switch,G1(:,:,:,107),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,81))
  call loop_UV_W(G1(:,:,:,104),Q(:,14),wf(:,84),Q(:,48),G2(:,:,:,69))
  call check_last_UV_W(l_switch,G2(:,:,:,69),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,62))
  call loop_UV_W(G1(:,:,:,104),Q(:,14),wf(:,-5),Q(:,32),G2(:,:,:,70))
  call loop_UV_W(G2(:,:,:,70),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,22))
  call check_last_UV_W(l_switch,G3(:,:,:,22),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,18))
  call loop_UV_W(G1(:,:,:,104),Q(:,14),wf(:,-4),Q(:,16),G2(:,:,:,71))
  call loop_UV_W(G2(:,:,:,71),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,23))
  call check_last_UV_W(l_switch,G3(:,:,:,23),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,19))
  call loop_GGG_G_12(G0(:,:,:,84),wf(:,-3),wf(:,84),G0(:,:,:,94))
  call check_last_UV_W(l_switch,G0(:,:,:,94),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,106))
  call loop_GGG_G_12(G0(:,:,:,84),wf(:,84),wf(:,-3),G0(:,:,:,95))
  call check_last_UV_W(l_switch,G0(:,:,:,95),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,107))
  call loop_GGG_G_23(G0(:,:,:,84),wf(:,-3),wf(:,84),G0(:,:,:,96))
  call check_last_UV_W(l_switch,G0(:,:,:,96),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,108))
  call loop_GGG_G_12(G0(:,:,:,84),wf(:,-5),wf(:,-3),G0(:,:,:,97))
  call loop_UV_W(G0(:,:,:,97),Q(:,46),wf(:,-4),Q(:,16),G1(:,:,:,108))
  call check_last_UV_W(l_switch,G1(:,:,:,108),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,82))
  call loop_GGG_G_12(G0(:,:,:,84),wf(:,-3),wf(:,-5),G0(:,:,:,98))
  call loop_UV_W(G0(:,:,:,98),Q(:,46),wf(:,-4),Q(:,16),G1(:,:,:,109))
  call check_last_UV_W(l_switch,G1(:,:,:,109),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,83))
  call loop_GGG_G_23(G0(:,:,:,84),wf(:,-5),wf(:,-3),G0(:,:,:,99))
  call loop_UV_W(G0(:,:,:,99),Q(:,46),wf(:,-4),Q(:,16),G1(:,:,:,110))
  call check_last_UV_W(l_switch,G1(:,:,:,110),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,84))
  call loop_UV_W(G0(:,:,:,84),Q(:,6),wf(:,-4),Q(:,16),G1(:,:,:,111))
  call loop_GGG_G_12(G1(:,:,:,111),wf(:,-5),wf(:,-3),G1(:,:,:,112))
  call check_last_UV_W(l_switch,G1(:,:,:,112),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,85))
  call loop_GGG_G_12(G1(:,:,:,111),wf(:,-3),wf(:,-5),G1(:,:,:,113))
  call check_last_UV_W(l_switch,G1(:,:,:,113),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,86))
  call loop_GGG_G_23(G1(:,:,:,111),wf(:,-5),wf(:,-3),G1(:,:,:,114))
  call check_last_UV_W(l_switch,G1(:,:,:,114),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,87))
  call loop_UV_W(G1(:,:,:,111),Q(:,22),wf(:,79),Q(:,40),G2(:,:,:,72))
  call check_last_UV_W(l_switch,G2(:,:,:,72),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,63))
  call loop_UV_W(G1(:,:,:,111),Q(:,22),wf(:,-5),Q(:,32),G2(:,:,:,73))
  call loop_UV_W(G2(:,:,:,73),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,24))
  call check_last_UV_W(l_switch,G3(:,:,:,24),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,20))
  call loop_UV_W(G1(:,:,:,111),Q(:,22),wf(:,-3),Q(:,8),G2(:,:,:,74))
  call loop_UV_W(G2(:,:,:,74),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,25))
  call check_last_UV_W(l_switch,G3(:,:,:,25),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,21))
  call loop_GGG_G_12(G0(:,:,:,84),wf(:,-4),wf(:,-3),G0(:,:,:,100))
  call loop_UV_W(G0(:,:,:,100),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,115))
  call check_last_UV_W(l_switch,G1(:,:,:,115),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,88))
  call loop_GGG_G_12(G0(:,:,:,84),wf(:,-3),wf(:,-4),G0(:,:,:,101))
  call loop_UV_W(G0(:,:,:,101),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,116))
  call check_last_UV_W(l_switch,G1(:,:,:,116),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,89))
  call loop_GGG_G_23(G0(:,:,:,84),wf(:,-4),wf(:,-3),G0(:,:,:,102))
  call loop_UV_W(G0(:,:,:,102),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,117))
  call check_last_UV_W(l_switch,G1(:,:,:,117),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,90))
  call loop_UV_W(G0(:,:,:,84),Q(:,6),wf(:,-5),Q(:,32),G1(:,:,:,118))
  call loop_GGG_G_12(G1(:,:,:,118),wf(:,-4),wf(:,-3),G1(:,:,:,119))
  call check_last_UV_W(l_switch,G1(:,:,:,119),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,91))
  call loop_GGG_G_12(G1(:,:,:,118),wf(:,-3),wf(:,-4),G1(:,:,:,120))
  call check_last_UV_W(l_switch,G1(:,:,:,120),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,92))
  call loop_GGG_G_23(G1(:,:,:,118),wf(:,-4),wf(:,-3),G1(:,:,:,121))
  call check_last_UV_W(l_switch,G1(:,:,:,121),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,93))
  call loop_UV_W(G1(:,:,:,118),Q(:,38),wf(:,75),Q(:,24),G2(:,:,:,75))
  call check_last_UV_W(l_switch,G2(:,:,:,75),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,64))
  call loop_UV_W(G1(:,:,:,118),Q(:,38),wf(:,-4),Q(:,16),G2(:,:,:,76))
  call loop_UV_W(G2(:,:,:,76),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,26))
  call check_last_UV_W(l_switch,G3(:,:,:,26),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,22))
  call loop_UV_W(G1(:,:,:,118),Q(:,38),wf(:,-3),Q(:,8),G2(:,:,:,77))
  call loop_UV_W(G2(:,:,:,77),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,27))
  call check_last_UV_W(l_switch,G3(:,:,:,27),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,23))
  call loop_UV_W(G0(:,:,:,84),Q(:,6),wf(:,2),Q(:,56),G1(:,:,:,122))
  call check_last_UV_W(l_switch,G1(:,:,:,122),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,94))
  call loop_UV_W(G0(:,:,:,84),Q(:,6),wf(:,5),Q(:,56),G1(:,:,:,123))
  call check_last_UV_W(l_switch,G1(:,:,:,123),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,95))
  call loop_UV_W(G0(:,:,:,84),Q(:,6),wf(:,6),Q(:,56),G1(:,:,:,124))
  call check_last_UV_W(l_switch,G1(:,:,:,124),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,96))
  call loop_UV_W(G0(:,:,:,84),Q(:,6),wf(:,268),Q(:,56),G1(:,:,:,125))
  call check_last_UV_W(l_switch,G1(:,:,:,125),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,97))
  call loop_UV_W(G0(:,:,:,84),Q(:,6),wf(:,75),Q(:,24),G1(:,:,:,126))
  call loop_UV_W(G1(:,:,:,126),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,78))
  call check_last_UV_W(l_switch,G2(:,:,:,78),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,65))
  call loop_UV_W(G0(:,:,:,84),Q(:,6),wf(:,269),Q(:,56),G1(:,:,:,127))
  call check_last_UV_W(l_switch,G1(:,:,:,127),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,98))
  call loop_UV_W(G0(:,:,:,84),Q(:,6),wf(:,79),Q(:,40),G1(:,:,:,128))
  call loop_UV_W(G1(:,:,:,128),Q(:,46),wf(:,-4),Q(:,16),G2(:,:,:,79))
  call check_last_UV_W(l_switch,G2(:,:,:,79),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,66))
  call loop_UV_W(G0(:,:,:,84),Q(:,6),wf(:,270),Q(:,56),G1(:,:,:,129))
  call check_last_UV_W(l_switch,G1(:,:,:,129),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,99))
  call loop_UV_W(G0(:,:,:,84),Q(:,6),wf(:,84),Q(:,48),G1(:,:,:,130))
  call loop_UV_W(G1(:,:,:,130),Q(:,54),wf(:,-3),Q(:,8),G2(:,:,:,80))
  call check_last_UV_W(l_switch,G2(:,:,:,80),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,67))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,-1),G0(:,:,:,103))
  call loop_GGG_G_12(G0(:,:,:,103),wf(:,-5),wf(:,75),G0(:,:,:,104))
  call check_last_UV_W(l_switch,G0(:,:,:,104),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,109))
  call loop_GGG_G_12(G0(:,:,:,103),wf(:,75),wf(:,-5),G0(:,:,:,105))
  call check_last_UV_W(l_switch,G0(:,:,:,105),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,110))
  call loop_GGG_G_23(G0(:,:,:,103),wf(:,-5),wf(:,75),G0(:,:,:,106))
  call check_last_UV_W(l_switch,G0(:,:,:,106),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,111))
  call loop_GGG_G_12(G0(:,:,:,103),wf(:,-4),wf(:,79),G0(:,:,:,107))
  call check_last_UV_W(l_switch,G0(:,:,:,107),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,112))
  call loop_GGG_G_12(G0(:,:,:,103),wf(:,79),wf(:,-4),G0(:,:,:,108))
  call check_last_UV_W(l_switch,G0(:,:,:,108),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,113))
  call loop_GGG_G_23(G0(:,:,:,103),wf(:,-4),wf(:,79),G0(:,:,:,109))
  call check_last_UV_W(l_switch,G0(:,:,:,109),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,114))
  call loop_GGG_G_12(G0(:,:,:,103),wf(:,-5),wf(:,-4),G0(:,:,:,110))
  call loop_UV_W(G0(:,:,:,110),Q(:,54),wf(:,-3),Q(:,8),G1(:,:,:,131))
  call check_last_UV_W(l_switch,G1(:,:,:,131),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,100))
  call loop_GGG_G_12(G0(:,:,:,103),wf(:,-4),wf(:,-5),G0(:,:,:,111))
  call loop_UV_W(G0(:,:,:,111),Q(:,54),wf(:,-3),Q(:,8),G1(:,:,:,132))
  call check_last_UV_W(l_switch,G1(:,:,:,132),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,101))
  call loop_GGG_G_23(G0(:,:,:,103),wf(:,-5),wf(:,-4),G0(:,:,:,112))
  call loop_UV_W(G0(:,:,:,112),Q(:,54),wf(:,-3),Q(:,8),G1(:,:,:,133))
  call check_last_UV_W(l_switch,G1(:,:,:,133),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,102))
  call loop_UV_W(G0(:,:,:,103),Q(:,6),wf(:,-3),Q(:,8),G1(:,:,:,134))
  call loop_GGG_G_12(G1(:,:,:,134),wf(:,-5),wf(:,-4),G1(:,:,:,135))
  call check_last_UV_W(l_switch,G1(:,:,:,135),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,103))
  call loop_GGG_G_12(G1(:,:,:,134),wf(:,-4),wf(:,-5),G1(:,:,:,136))
  call check_last_UV_W(l_switch,G1(:,:,:,136),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,104))
  call loop_GGG_G_23(G1(:,:,:,134),wf(:,-5),wf(:,-4),G1(:,:,:,137))
  call check_last_UV_W(l_switch,G1(:,:,:,137),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,105))
  call loop_UV_W(G1(:,:,:,134),Q(:,14),wf(:,84),Q(:,48),G2(:,:,:,81))
  call check_last_UV_W(l_switch,G2(:,:,:,81),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,68))
  call loop_UV_W(G1(:,:,:,134),Q(:,14),wf(:,-5),Q(:,32),G2(:,:,:,82))
  call loop_UV_W(G2(:,:,:,82),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,28))
  call check_last_UV_W(l_switch,G3(:,:,:,28),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,24))
  call loop_UV_W(G1(:,:,:,134),Q(:,14),wf(:,-4),Q(:,16),G2(:,:,:,83))
  call loop_UV_W(G2(:,:,:,83),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,29))
  call check_last_UV_W(l_switch,G3(:,:,:,29),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,25))
  call loop_GGG_G_12(G0(:,:,:,103),wf(:,-3),wf(:,84),G0(:,:,:,113))
  call check_last_UV_W(l_switch,G0(:,:,:,113),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,115))
  call loop_GGG_G_12(G0(:,:,:,103),wf(:,84),wf(:,-3),G0(:,:,:,114))
  call check_last_UV_W(l_switch,G0(:,:,:,114),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,116))
  call loop_GGG_G_23(G0(:,:,:,103),wf(:,-3),wf(:,84),G0(:,:,:,115))
  call check_last_UV_W(l_switch,G0(:,:,:,115),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,117))
  call loop_GGG_G_12(G0(:,:,:,103),wf(:,-5),wf(:,-3),G0(:,:,:,116))
  call loop_UV_W(G0(:,:,:,116),Q(:,46),wf(:,-4),Q(:,16),G1(:,:,:,138))
  call check_last_UV_W(l_switch,G1(:,:,:,138),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,106))
  call loop_GGG_G_12(G0(:,:,:,103),wf(:,-3),wf(:,-5),G0(:,:,:,117))
  call loop_UV_W(G0(:,:,:,117),Q(:,46),wf(:,-4),Q(:,16),G1(:,:,:,139))
  call check_last_UV_W(l_switch,G1(:,:,:,139),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,107))
  call loop_GGG_G_23(G0(:,:,:,103),wf(:,-5),wf(:,-3),G0(:,:,:,118))
  call loop_UV_W(G0(:,:,:,118),Q(:,46),wf(:,-4),Q(:,16),G1(:,:,:,140))
  call check_last_UV_W(l_switch,G1(:,:,:,140),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,108))
  call loop_UV_W(G0(:,:,:,103),Q(:,6),wf(:,-4),Q(:,16),G1(:,:,:,141))
  call loop_GGG_G_12(G1(:,:,:,141),wf(:,-5),wf(:,-3),G1(:,:,:,142))
  call check_last_UV_W(l_switch,G1(:,:,:,142),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,109))
  call loop_GGG_G_12(G1(:,:,:,141),wf(:,-3),wf(:,-5),G1(:,:,:,143))
  call check_last_UV_W(l_switch,G1(:,:,:,143),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,110))
  call loop_GGG_G_23(G1(:,:,:,141),wf(:,-5),wf(:,-3),G1(:,:,:,144))
  call check_last_UV_W(l_switch,G1(:,:,:,144),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,111))
  call loop_UV_W(G1(:,:,:,141),Q(:,22),wf(:,79),Q(:,40),G2(:,:,:,84))
  call check_last_UV_W(l_switch,G2(:,:,:,84),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,69))
  call loop_UV_W(G1(:,:,:,141),Q(:,22),wf(:,-5),Q(:,32),G2(:,:,:,85))
  call loop_UV_W(G2(:,:,:,85),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,30))
  call check_last_UV_W(l_switch,G3(:,:,:,30),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,26))
  call loop_UV_W(G1(:,:,:,141),Q(:,22),wf(:,-3),Q(:,8),G2(:,:,:,86))
  call loop_UV_W(G2(:,:,:,86),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,31))
  call check_last_UV_W(l_switch,G3(:,:,:,31),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,27))
  call loop_GGG_G_12(G0(:,:,:,103),wf(:,-4),wf(:,-3),G0(:,:,:,119))
  call loop_UV_W(G0(:,:,:,119),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,145))
  call check_last_UV_W(l_switch,G1(:,:,:,145),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,112))
  call loop_GGG_G_12(G0(:,:,:,103),wf(:,-3),wf(:,-4),G0(:,:,:,120))
  call loop_UV_W(G0(:,:,:,120),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,146))
  call check_last_UV_W(l_switch,G1(:,:,:,146),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,113))
  call loop_GGG_G_23(G0(:,:,:,103),wf(:,-4),wf(:,-3),G0(:,:,:,121))
  call loop_UV_W(G0(:,:,:,121),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,147))
  call check_last_UV_W(l_switch,G1(:,:,:,147),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,114))
  call loop_UV_W(G0(:,:,:,103),Q(:,6),wf(:,-5),Q(:,32),G1(:,:,:,148))
  call loop_GGG_G_12(G1(:,:,:,148),wf(:,-4),wf(:,-3),G1(:,:,:,149))
  call check_last_UV_W(l_switch,G1(:,:,:,149),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,115))
  call loop_GGG_G_12(G1(:,:,:,148),wf(:,-3),wf(:,-4),G1(:,:,:,150))
  call check_last_UV_W(l_switch,G1(:,:,:,150),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,116))
  call loop_GGG_G_23(G1(:,:,:,148),wf(:,-4),wf(:,-3),G1(:,:,:,151))
  call check_last_UV_W(l_switch,G1(:,:,:,151),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,117))
  call loop_UV_W(G1(:,:,:,148),Q(:,38),wf(:,75),Q(:,24),G2(:,:,:,87))
  call check_last_UV_W(l_switch,G2(:,:,:,87),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,70))
  call loop_UV_W(G1(:,:,:,148),Q(:,38),wf(:,-4),Q(:,16),G2(:,:,:,88))
  call loop_UV_W(G2(:,:,:,88),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,32))
  call check_last_UV_W(l_switch,G3(:,:,:,32),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,28))
  call loop_UV_W(G1(:,:,:,148),Q(:,38),wf(:,-3),Q(:,8),G2(:,:,:,89))
  call loop_UV_W(G2(:,:,:,89),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,33))
  call check_last_UV_W(l_switch,G3(:,:,:,33),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,29))
  call loop_UV_W(G0(:,:,:,103),Q(:,6),wf(:,2),Q(:,56),G1(:,:,:,152))
  call check_last_UV_W(l_switch,G1(:,:,:,152),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,118))
  call loop_UV_W(G0(:,:,:,103),Q(:,6),wf(:,5),Q(:,56),G1(:,:,:,153))
  call check_last_UV_W(l_switch,G1(:,:,:,153),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,119))
  call loop_UV_W(G0(:,:,:,103),Q(:,6),wf(:,6),Q(:,56),G1(:,:,:,154))
  call check_last_UV_W(l_switch,G1(:,:,:,154),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,120))
  call loop_UV_W(G0(:,:,:,103),Q(:,6),wf(:,268),Q(:,56),G1(:,:,:,155))
  call check_last_UV_W(l_switch,G1(:,:,:,155),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,121))
  call loop_UV_W(G0(:,:,:,103),Q(:,6),wf(:,75),Q(:,24),G1(:,:,:,156))
  call loop_UV_W(G1(:,:,:,156),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,90))
  call check_last_UV_W(l_switch,G2(:,:,:,90),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,71))
  call loop_UV_W(G0(:,:,:,103),Q(:,6),wf(:,269),Q(:,56),G1(:,:,:,157))
  call check_last_UV_W(l_switch,G1(:,:,:,157),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,122))
  call loop_UV_W(G0(:,:,:,103),Q(:,6),wf(:,79),Q(:,40),G1(:,:,:,158))
  call loop_UV_W(G1(:,:,:,158),Q(:,46),wf(:,-4),Q(:,16),G2(:,:,:,91))
  call check_last_UV_W(l_switch,G2(:,:,:,91),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,72))
  call loop_UV_W(G0(:,:,:,103),Q(:,6),wf(:,270),Q(:,56),G1(:,:,:,159))
  call check_last_UV_W(l_switch,G1(:,:,:,159),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,123))
  call loop_UV_W(G0(:,:,:,103),Q(:,6),wf(:,84),Q(:,48),G1(:,:,:,160))
  call loop_UV_W(G1(:,:,:,160),Q(:,54),wf(:,-3),Q(:,8),G2(:,:,:,92))
  call check_last_UV_W(l_switch,G2(:,:,:,92),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,73))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,56),G0(:,:,:,122))
  call loop_UV_W(G0(:,:,:,122),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,161))
  call check_last_UV_W(l_switch,G1(:,:,:,161),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,124))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,59),G0(:,:,:,123))
  call loop_UV_W(G0(:,:,:,123),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,162))
  call check_last_UV_W(l_switch,G1(:,:,:,162),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,125))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,60),G0(:,:,:,124))
  call loop_UV_W(G0(:,:,:,124),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,163))
  call check_last_UV_W(l_switch,G1(:,:,:,163),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,126))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,56),wf(:,-4),G0(:,:,:,125))
  call loop_UV_W(G0(:,:,:,125),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,164))
  call check_last_UV_W(l_switch,G1(:,:,:,164),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,127))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,59),wf(:,-4),G0(:,:,:,126))
  call loop_UV_W(G0(:,:,:,126),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,165))
  call check_last_UV_W(l_switch,G1(:,:,:,165),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,128))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,60),wf(:,-4),G0(:,:,:,127))
  call loop_UV_W(G0(:,:,:,127),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,166))
  call check_last_UV_W(l_switch,G1(:,:,:,166),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,129))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,56),G0(:,:,:,128))
  call loop_UV_W(G0(:,:,:,128),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,167))
  call check_last_UV_W(l_switch,G1(:,:,:,167),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,130))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,59),G0(:,:,:,129))
  call loop_UV_W(G0(:,:,:,129),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,168))
  call check_last_UV_W(l_switch,G1(:,:,:,168),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,131))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,60),G0(:,:,:,130))
  call loop_UV_W(G0(:,:,:,130),Q(:,30),wf(:,-5),Q(:,32),G1(:,:,:,169))
  call check_last_UV_W(l_switch,G1(:,:,:,169),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,132))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,56),Q(:,14),G1(:,:,:,170))
  call loop_GGG_G_12(G1(:,:,:,170),wf(:,-5),wf(:,-4),G1(:,:,:,171))
  call check_last_UV_W(l_switch,G1(:,:,:,171),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,133))
  call loop_GGG_G_12(G1(:,:,:,170),wf(:,-4),wf(:,-5),G1(:,:,:,172))
  call check_last_UV_W(l_switch,G1(:,:,:,172),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,134))
  call loop_GGG_G_23(G1(:,:,:,170),wf(:,-5),wf(:,-4),G1(:,:,:,173))
  call check_last_UV_W(l_switch,G1(:,:,:,173),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,135))
  call loop_UV_W(G1(:,:,:,170),Q(:,14),wf(:,84),Q(:,48),G2(:,:,:,93))
  call check_last_UV_W(l_switch,G2(:,:,:,93),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,74))
  call loop_UV_W(G1(:,:,:,170),Q(:,14),wf(:,-5),Q(:,32),G2(:,:,:,94))
  call loop_UV_W(G2(:,:,:,94),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,34))
  call check_last_UV_W(l_switch,G3(:,:,:,34),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,30))
  call loop_UV_W(G1(:,:,:,170),Q(:,14),wf(:,-4),Q(:,16),G2(:,:,:,95))
  call loop_UV_W(G2(:,:,:,95),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,35))
  call check_last_UV_W(l_switch,G3(:,:,:,35),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,31))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,59),Q(:,14),G1(:,:,:,174))
  call loop_GGG_G_12(G1(:,:,:,174),wf(:,-5),wf(:,-4),G1(:,:,:,175))
  call check_last_UV_W(l_switch,G1(:,:,:,175),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,136))
  call loop_GGG_G_12(G1(:,:,:,174),wf(:,-4),wf(:,-5),G1(:,:,:,176))
  call check_last_UV_W(l_switch,G1(:,:,:,176),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,137))
  call loop_GGG_G_23(G1(:,:,:,174),wf(:,-5),wf(:,-4),G1(:,:,:,177))
  call check_last_UV_W(l_switch,G1(:,:,:,177),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,138))
  call loop_UV_W(G1(:,:,:,174),Q(:,14),wf(:,84),Q(:,48),G2(:,:,:,96))
  call check_last_UV_W(l_switch,G2(:,:,:,96),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,75))
  call loop_UV_W(G1(:,:,:,174),Q(:,14),wf(:,-5),Q(:,32),G2(:,:,:,97))
  call loop_UV_W(G2(:,:,:,97),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,36))
  call check_last_UV_W(l_switch,G3(:,:,:,36),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,32))
  call loop_UV_W(G1(:,:,:,174),Q(:,14),wf(:,-4),Q(:,16),G2(:,:,:,98))
  call loop_UV_W(G2(:,:,:,98),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,37))
  call check_last_UV_W(l_switch,G3(:,:,:,37),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,33))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,60),Q(:,14),G1(:,:,:,178))
  call loop_GGG_G_12(G1(:,:,:,178),wf(:,-5),wf(:,-4),G1(:,:,:,179))
  call check_last_UV_W(l_switch,G1(:,:,:,179),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,139))
  call loop_GGG_G_12(G1(:,:,:,178),wf(:,-4),wf(:,-5),G1(:,:,:,180))
  call check_last_UV_W(l_switch,G1(:,:,:,180),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,140))
  call loop_GGG_G_23(G1(:,:,:,178),wf(:,-5),wf(:,-4),G1(:,:,:,181))
  call check_last_UV_W(l_switch,G1(:,:,:,181),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,141))
  call loop_UV_W(G1(:,:,:,178),Q(:,14),wf(:,84),Q(:,48),G2(:,:,:,99))
  call check_last_UV_W(l_switch,G2(:,:,:,99),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,76))
  call loop_UV_W(G1(:,:,:,178),Q(:,14),wf(:,-5),Q(:,32),G2(:,:,:,100))
  call loop_UV_W(G2(:,:,:,100),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,38))
  call check_last_UV_W(l_switch,G3(:,:,:,38),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,34))
  call loop_UV_W(G1(:,:,:,178),Q(:,14),wf(:,-4),Q(:,16),G2(:,:,:,101))
  call loop_UV_W(G2(:,:,:,101),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,39))
  call check_last_UV_W(l_switch,G3(:,:,:,39),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,35))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(4)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(42)+M(45)+M(52) &
    -M(54)+M(57)+M(64)-M(66)-M(69)+M(76)-M(77)-M(78)-M(79)+M(80)-M(81)-M(82)+M(83)+M(84)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94) &
    -M(97))+c(6)*(M(132)-M(203)+M(226)-M(241))) * den(43)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(42)-M(45)-M(52) &
    +M(54)+M(57)+M(64)+M(66)-M(69)+M(76)+M(77)+M(78)+M(79)-M(80)-M(81)-M(82)-M(83)+M(84)-M(85)-M(86)+M(87)+M(88)+M(91)-M(94) &
    -M(97))+c(6)*(-M(151)+M(165)+M(208)-M(212))) * den(43)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,10)
  Gcoeff = (c(5)*(-M(42)-M(45)-M(52)+M(54)+M(66)+M(77)+M(78)+M(79)-M(80)-M(83)-M(86)+M(91))+c(6)*(-M(132)-M(151)+M(165)+M(203) &
    +M(208)-M(212)-M(226)+M(241))) * den(43)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,19)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)-M(11)+M(12)-M(19)+M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(42)+M(45)+M(52) &
    -M(54)+M(57)+M(64)-M(66)-M(69)-M(76)-M(77)-M(78)-M(79)+M(80)-M(81)-M(82)+M(83)+M(84)+M(85)+M(86)+M(87)-M(88)-M(91)-M(94) &
    +M(97))+c(6)*(M(135)-M(199)-M(204)+M(220))) * den(43)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,2)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)-M(11)+M(12)-M(19)+M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(42)-M(45)-M(52) &
    +M(54)+M(57)+M(64)+M(66)-M(69)-M(76)+M(77)+M(78)+M(79)-M(80)-M(81)-M(82)-M(83)+M(84)+M(85)-M(86)+M(87)-M(88)+M(91)-M(94) &
    +M(97))+c(6)*(-M(141)+M(175)+M(206)-M(214))) * den(43)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,11)
  Gcoeff = (c(5)*(-M(42)-M(45)-M(52)+M(54)+M(66)+M(77)+M(78)+M(79)-M(80)-M(83)-M(86)+M(91))+c(6)*(-M(135)-M(141)+M(175)+M(199) &
    +M(204)+M(206)-M(214)-M(220))) * den(43)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,20)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(76)+M(85)-M(88)+M(97))+c(6)*(-M(132)+M(135)-M(199)+M(203) &
    -M(204)+M(220)-M(226)+M(241))) * den(43)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,3)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(76)+M(85)-M(88)+M(97))+c(6)*(-M(141)+M(151)-M(165)+M(175) &
    +M(206)-M(208)+M(212)-M(214))) * den(43)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,12)
  Gcoeff = (c(6)*(M(132)-M(135)-M(141)+M(151)-M(165)+M(175)+M(199)-M(203)+M(204)+M(206)-M(208)+M(212)-M(214)-M(220)+M(226) &
    -M(241))) * den(43)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,21)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    -M(60)+M(61)+M(69)-M(72)-M(80)+M(81)-M(82)-M(94)+M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(150)+M(153)+M(188)-M(194))) * den(14)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,28)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    -M(60)+M(61)+M(69)-M(72)-M(80)+M(81)-M(82)-M(94)+M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(187)-M(193)-M(228)+M(231))) * den(14)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,29)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(104)+M(114)+M(120)-M(123))+c(6)*(M(150)-M(153)+M(187)-M(188) &
    -M(193)+M(194)-M(228)+M(231))) * den(14)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,30)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(151)+M(152)+M(170)-M(212))) * den(14)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,37)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)+M(103)-M(104)-M(107)+M(109)-M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(169)-M(211)-M(229)+M(230))) * den(14)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,38)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(104)+M(114)+M(120)-M(123))+c(6)*(M(151)-M(152)+M(169)-M(170) &
    -M(211)+M(212)-M(229)+M(230))) * den(14)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,39)
  Gcoeff = (c(5)*(M(60)-M(69)+M(72)-M(81)+M(103)-M(107)+M(109)-M(113)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(150)-M(151)+M(152) &
    -M(153)+M(170)-M(188)+M(194)-M(212))) * den(14)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,46)
  Gcoeff = (c(5)*(M(60)-M(69)+M(72)-M(81)+M(103)-M(107)+M(109)-M(113)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(169)-M(187)+M(193) &
    -M(211)+M(228)-M(229)+M(230)-M(231))) * den(14)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,47)
  Gcoeff = (c(6)*(-M(150)+M(151)-M(152)+M(153)+M(169)-M(170)-M(187)+M(188)+M(193)-M(194)-M(211)+M(212)+M(228)-M(229)+M(230) &
    -M(231))) * den(14)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,48)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)+M(61)-M(69)+M(72)+M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(134)-M(144)-M(200)+M(202))) * den(14)
  T3sum(1:5,32) = T3sum(1:5,32) + Gcoeff * G1tensor(:,55)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(168)-M(197)+M(198)-M(210))) * den(14)
  T3sum(1:5,32) = T3sum(1:5,32) + Gcoeff * G1tensor(:,64)
  Gcoeff = (c(5)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(6)*(-M(134)+M(144)+M(168) &
    -M(197)+M(198)+M(200)-M(202)-M(210))) * den(14)
  T3sum(1:5,32) = T3sum(1:5,32) + Gcoeff * G1tensor(:,73)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)+M(61)-M(69)+M(72)+M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(143)-M(242)+M(244))) * den(14)
  T3sum(1:5,32) = T3sum(1:5,32) + Gcoeff * G1tensor(:,56)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(167)-M(209)-M(239)+M(240))) * den(14)
  T3sum(1:5,32) = T3sum(1:5,32) + Gcoeff * G1tensor(:,65)
  Gcoeff = (c(5)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(6)*(-M(133)+M(143)+M(167) &
    -M(209)-M(239)+M(240)+M(242)-M(244))) * den(14)
  T3sum(1:5,32) = T3sum(1:5,32) + Gcoeff * G1tensor(:,74)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(133)-M(134)-M(143)+M(144) &
    +M(200)-M(202)-M(242)+M(244))) * den(14)
  T3sum(1:5,32) = T3sum(1:5,32) + Gcoeff * G1tensor(:,57)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(167)-M(168)+M(197)-M(198) &
    -M(209)+M(210)-M(239)+M(240))) * den(14)
  T3sum(1:5,32) = T3sum(1:5,32) + Gcoeff * G1tensor(:,66)
  Gcoeff = (c(6)*(-M(133)+M(134)+M(143)-M(144)+M(167)-M(168)+M(197)-M(198)-M(200)+M(202)-M(209)+M(210)-M(239)+M(240)+M(242) &
    -M(244))) * den(14)
  T3sum(1:5,32) = T3sum(1:5,32) + Gcoeff * G1tensor(:,75)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)+M(63)-M(65)-M(66)-M(67)+M(68)-M(69)-M(70)+M(71)+M(72)-M(73)+M(74)+M(75)+M(76)-M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(134)-M(179)+M(202)-M(247))) * den(40)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,82)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)+M(17)-M(18)-M(19)+M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(41)+M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)-M(66)-M(67)-M(68)-M(69)-M(70)+M(71)+M(72)-M(73)+M(74)+M(75)+M(76)-M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(158)-M(185)+M(201)-M(245))) * den(40)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,83)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(-M(134)+M(158)+M(179)-M(185)+M(201) &
    -M(202)-M(245)+M(247))) * den(40)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,84)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)-M(17)+M(18)+M(19)-M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)+M(63)-M(65)-M(66)-M(67)+M(68)-M(69)-M(70)+M(71)+M(72)+M(73)+M(74)+M(75)-M(76)-M(78)-M(81)-M(88)-M(90)-M(93) &
    +M(100))+c(6)*(M(136)-M(180)+M(196)-M(223))) * den(40)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,85)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)+M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)-M(66)-M(67)-M(68)-M(69)-M(70)+M(71)+M(72)+M(73)+M(74)+M(75)-M(76)-M(78)-M(81)-M(88)-M(90)-M(93) &
    +M(100))+c(6)*(M(160)-M(186)+M(195)-M(221))) * den(40)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,86)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(-M(136)+M(160)+M(180)-M(186)+M(195) &
    -M(196)-M(221)+M(223))) * den(40)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,87)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(134)+M(136)+M(179)-M(180) &
    +M(196)-M(202)-M(223)+M(247))) * den(40)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,88)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(158)+M(160)+M(185)-M(186) &
    +M(195)-M(201)-M(221)+M(245))) * den(40)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,89)
  Gcoeff = (c(6)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(40)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,90)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(37)+M(38)+M(39)-M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100)-M(101)+M(103)+M(109)-M(110)-M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(159)-M(160)-M(195)+M(219))) * den(20)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,91)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(135)-M(136)-M(196)+M(220))) * den(20)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,100)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(101)-M(103)-M(109)+M(115))+c(6)*(M(135)-M(136)-M(159)+M(160) &
    +M(195)-M(196)-M(219)+M(220))) * den(20)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,109)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(37)+M(38)+M(39)-M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)+M(73)+M(83)+M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(155)-M(157)-M(243)+M(249))) * den(20)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,92)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)+M(73)+M(83)+M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)+M(110)+M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(131)-M(133)-M(244)+M(250))) * den(20)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,101)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(101)-M(103)-M(109)+M(115))+c(6)*(M(131)-M(133)-M(155)+M(157) &
    +M(243)-M(244)-M(249)+M(250))) * den(20)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,110)
  Gcoeff = (c(5)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(155)-M(157)-M(159) &
    +M(160)+M(195)-M(219)-M(243)+M(249))) * den(20)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,93)
  Gcoeff = (c(5)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(131)-M(133)-M(135) &
    +M(136)+M(196)-M(220)-M(244)+M(250))) * den(20)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,102)
  Gcoeff = (c(6)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(20)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,111)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(37)-M(38)-M(39)+M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)-M(102)+M(105)+M(111)-M(112)+M(116)-M(118)-M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(157)-M(158)-M(201)+M(243))) * den(22)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,94)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(134)-M(202)+M(244))) * den(22)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,103)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(37)+M(38)+M(39)-M(40)+M(102)-M(105)-M(111)+M(121))+c(6)*(M(133)-M(134)-M(157)+M(158) &
    +M(201)-M(202)-M(243)+M(244))) * den(22)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,112)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(37)-M(38)-M(39)+M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)+M(76)-M(85)+M(88)+M(95)+M(96)-M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(156)-M(159)-M(219)+M(225))) * den(22)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,95)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)+M(76)-M(85)+M(88)+M(95)+M(96)-M(97)+M(102)-M(105)-M(111)+M(112)-M(116)+M(118)+M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(132)-M(135)-M(220)+M(226))) * den(22)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,104)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(37)+M(38)+M(39)-M(40)+M(102)-M(105)-M(111)+M(121))+c(6)*(M(132)-M(135)-M(156)+M(159) &
    +M(219)-M(220)-M(225)+M(226))) * den(22)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,113)
  Gcoeff = (c(5)*(M(76)-M(85)+M(88)-M(97)+M(112)-M(116)+M(118)-M(122)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(156)-M(157)+M(158) &
    -M(159)+M(201)-M(219)+M(225)-M(243))) * den(22)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,96)
  Gcoeff = (c(5)*(M(76)-M(85)+M(88)-M(97)+M(112)-M(116)+M(118)-M(122)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(132)-M(133)+M(134) &
    -M(135)+M(202)-M(220)+M(226)-M(244))) * den(22)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,105)
  Gcoeff = (c(6)*(M(132)-M(133)+M(134)-M(135)-M(156)+M(157)-M(158)+M(159)-M(201)+M(202)+M(219)-M(220)-M(225)+M(226)+M(243) &
    -M(244))) * den(22)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,114)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(9)-M(11)-M(13)-M(16)-M(17)-M(19)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(48)+M(51)+M(52)+M(53)+M(54)+M(55)+M(60)+M(63)+M(64)+M(65)+M(71)+M(72)+M(73)+M(74)+M(75)+M(77)+M(86)+M(87)+M(89) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(160)+M(195)))
  T4sum(1:15,19) = T4sum(1:15,19) + Gcoeff * G2tensor(:,52)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(48)+M(51)+M(52)+M(60)+M(63)+M(64)+M(68)+M(71)+M(72)+M(73)+M(74)+M(75)+M(80)+M(86)+M(87)+M(92) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(136)+M(196)))
  T4sum(1:15,19) = T4sum(1:15,19) + Gcoeff * G2tensor(:,76)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)-M(5)+M(6)+M(7)-M(8)+M(13)-M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)-M(101)-M(102)+M(103)-M(104)+M(105)+M(106)+M(109)+M(111)-M(115)+M(117)-M(121) &
    -M(123))+c(6)*(M(136)-M(160)-M(195)+M(196)))
  T4sum(1:15,19) = T4sum(1:15,19) + Gcoeff * G2tensor(:,100)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(9)-M(11)-M(13)-M(16)-M(18)-M(19)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(37) &
    -M(40)+M(48)+M(49)+M(51)+M(53)+M(54)+M(55)+M(60)+M(61)+M(63)+M(65)+M(71)+M(72)+M(74)+M(75)+M(76)+M(77)+M(88)+M(89)+M(98)+M(99) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(158)+M(201)))
  T4sum(1:15,19) = T4sum(1:15,19) + Gcoeff * G2tensor(:,53)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(48)+M(49)+M(51)+M(60)+M(61)+M(63)+M(68)+M(71)+M(72)+M(74)+M(75)+M(76)+M(80)+M(88)+M(92)+M(98)+M(99) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(134)+M(202)))
  T4sum(1:15,19) = T4sum(1:15,19) + Gcoeff * G2tensor(:,77)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(5)+M(6)-M(7)+M(8)+M(13)-M(14)+M(19)-M(20)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)-M(101)-M(102)+M(103)+M(104)+M(105)-M(106)+M(109)+M(111)-M(115)-M(117)-M(121) &
    +M(123))+c(6)*(M(134)-M(158)-M(201)+M(202)))
  T4sum(1:15,19) = T4sum(1:15,19) + Gcoeff * G2tensor(:,101)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)+M(49)-M(52)+M(61) &
    -M(64)-M(73)+M(76)-M(86)-M(87)+M(88)+M(98)+M(99)-M(100)-M(104)+M(106)-M(110)+M(112)+M(117)+M(118)-M(123)-M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(158)-M(160)-M(195)+M(201)))
  T4sum(1:15,19) = T4sum(1:15,19) + Gcoeff * G2tensor(:,54)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)+M(61) &
    -M(64)-M(73)+M(76)-M(86)-M(87)+M(88)+M(98)+M(99)-M(100)+M(104)-M(106)-M(110)+M(112)-M(117)+M(118)+M(123)-M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(134)-M(136)-M(196)+M(202)))
  T4sum(1:15,19) = T4sum(1:15,19) + Gcoeff * G2tensor(:,78)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(37)-M(38)-M(39)+M(40)+M(104)-M(106)-M(117)+M(123))+c(6)*(M(134)-M(136)-M(158)+M(160) &
    +M(195)-M(196)-M(201)+M(202)))
  T4sum(1:15,19) = T4sum(1:15,19) + Gcoeff * G2tensor(:,102)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(10)-M(12)-M(14)-M(15)-M(17)-M(20)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(38) &
    -M(39)+M(45)+M(46)+M(52)+M(53)+M(54)+M(55)+M(57)+M(58)+M(64)+M(65)+M(76)+M(77)+M(83)+M(84)+M(86)+M(87)+M(88)+M(89)+M(95)+M(96) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(156)+M(225)))
  T4sum(1:15,21) = T4sum(1:15,21) + Gcoeff * G2tensor(:,55)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(45)+M(46)+M(52)+M(57)+M(58)+M(64)+M(68)+M(76)+M(80)+M(83)+M(84)+M(86)+M(87)+M(88)+M(92)+M(95)+M(96) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(132)+M(226)))
  T4sum(1:15,21) = T4sum(1:15,21) + Gcoeff * G2tensor(:,79)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(5)-M(6)+M(7)-M(8)-M(13)+M(14)-M(19)+M(20)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)+M(101)+M(102)-M(103)-M(104)-M(105)+M(106)-M(109)-M(111)+M(115)+M(117)+M(121) &
    -M(123))+c(6)*(M(132)-M(156)-M(225)+M(226)))
  T4sum(1:15,21) = T4sum(1:15,21) + Gcoeff * G2tensor(:,103)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(10)-M(12)-M(14)-M(15)-M(18)-M(20)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(45)+M(46)+M(49)+M(53)+M(54)+M(55)+M(57)+M(58)+M(61)+M(65)+M(73)+M(77)+M(83)+M(84)+M(89)+M(95)+M(96)+M(98)+M(99) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(155)+M(249)))
  T4sum(1:15,21) = T4sum(1:15,21) + Gcoeff * G2tensor(:,56)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(45)+M(46)+M(49)+M(57)+M(58)+M(61)+M(68)+M(73)+M(80)+M(83)+M(84)+M(92)+M(95)+M(96)+M(98)+M(99) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(131)+M(250)))
  T4sum(1:15,21) = T4sum(1:15,21) + Gcoeff * G2tensor(:,80)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)+M(5)-M(6)-M(7)+M(8)-M(13)+M(14)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)+M(101)+M(102)-M(103)+M(104)-M(105)-M(106)-M(109)-M(111)+M(115)-M(117)+M(121) &
    +M(123))+c(6)*(M(131)-M(155)-M(249)+M(250)))
  T4sum(1:15,21) = T4sum(1:15,21) + Gcoeff * G2tensor(:,104)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)+M(49)-M(52)+M(61) &
    -M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100)-M(104)+M(106)+M(110)-M(112)+M(117)-M(118)-M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(155)-M(156)-M(225)+M(249)))
  T4sum(1:15,21) = T4sum(1:15,21) + Gcoeff * G2tensor(:,57)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)+M(61) &
    -M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(131)-M(132)-M(226)+M(250)))
  T4sum(1:15,21) = T4sum(1:15,21) + Gcoeff * G2tensor(:,81)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(37)-M(38)-M(39)+M(40)+M(104)-M(106)-M(117)+M(123))+c(6)*(M(131)-M(132)-M(155)+M(156) &
    +M(225)-M(226)-M(249)+M(250)))
  T4sum(1:15,21) = T4sum(1:15,21) + Gcoeff * G2tensor(:,105)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(203)-M(209)-M(215) &
    +M(217)+M(227)-M(233)-M(239)+M(241))) * den(92)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,124)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(6)*(-M(203)+M(205)+M(211) &
    -M(217)-M(227)+M(229)+M(235)-M(241))) * den(92)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,125)
  Gcoeff = (c(5)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(6)*(-M(205)+M(209)-M(211) &
    +M(215)-M(229)+M(233)-M(235)+M(239))) * den(92)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,126)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(131)-M(155)-M(179) &
    +M(185)+M(245)-M(247)-M(249)+M(250))) * den(92)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,127)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(6)*(-M(131)+M(137)+M(161) &
    -M(185)-M(245)+M(246)+M(248)-M(250))) * den(92)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,128)
  Gcoeff = (c(5)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(6)*(-M(137)+M(155)-M(161) &
    +M(179)-M(246)+M(247)-M(248)+M(249))) * den(92)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,129)
  Gcoeff = (c(6)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(92)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,130)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(92)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,131)
  Gcoeff = (c(6)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(92)
  T3sum(1:15,60) = T3sum(1:15,60) + Gcoeff * G2tensor(:,132)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(132)-M(156)-M(180) &
    +M(186)+M(221)-M(223)-M(225)+M(226))) * den(92)
  T3sum(1:15,33) = T3sum(1:15,33) + Gcoeff * G2tensor(:,133)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(6)*(-M(132)+M(138)+M(162) &
    -M(186)-M(221)+M(222)+M(224)-M(226))) * den(92)
  T3sum(1:15,33) = T3sum(1:15,33) + Gcoeff * G2tensor(:,136)
  Gcoeff = (c(5)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(6)*(-M(138)+M(156)-M(162) &
    +M(180)-M(222)+M(223)-M(224)+M(225))) * den(92)
  T3sum(1:15,33) = T3sum(1:15,33) + Gcoeff * G2tensor(:,139)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(131)-M(155)-M(179) &
    +M(185)+M(245)-M(247)-M(249)+M(250))) * den(92)
  T3sum(1:15,33) = T3sum(1:15,33) + Gcoeff * G2tensor(:,134)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(6)*(-M(131)+M(137)+M(161) &
    -M(185)-M(245)+M(246)+M(248)-M(250))) * den(92)
  T3sum(1:15,33) = T3sum(1:15,33) + Gcoeff * G2tensor(:,137)
  Gcoeff = (c(5)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(6)*(-M(137)+M(155)-M(161) &
    +M(179)-M(246)+M(247)-M(248)+M(249))) * den(92)
  T3sum(1:15,33) = T3sum(1:15,33) + Gcoeff * G2tensor(:,140)
  Gcoeff = (c(6)*(M(131)-M(132)-M(155)+M(156)-M(179)+M(180)+M(185)-M(186)-M(221)+M(223)+M(225)-M(226)+M(245)-M(247)-M(249) &
    +M(250))) * den(92)
  T3sum(1:15,33) = T3sum(1:15,33) + Gcoeff * G2tensor(:,135)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(92)
  T3sum(1:15,33) = T3sum(1:15,33) + Gcoeff * G2tensor(:,138)
  Gcoeff = (c(6)*(-M(137)+M(138)+M(155)-M(156)-M(161)+M(162)+M(179)-M(180)+M(222)-M(223)+M(224)-M(225)-M(246)+M(247)-M(248) &
    +M(249))) * den(92)
  T3sum(1:15,33) = T3sum(1:15,33) + Gcoeff * G2tensor(:,141)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)+M(49)-M(52)+M(61) &
    -M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100)-M(104)+M(106)+M(110)-M(112)+M(117)-M(118)-M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(155)-M(156)-M(225)+M(249))) * den(26)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,97)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)+M(61) &
    -M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(131)-M(132)-M(226)+M(250))) * den(26)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,106)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(37)-M(38)-M(39)+M(40)+M(104)-M(106)-M(117)+M(123))+c(6)*(M(131)-M(132)-M(155)+M(156) &
    +M(225)-M(226)-M(249)+M(250))) * den(26)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,115)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)+M(49)-M(52)+M(61) &
    -M(64)-M(73)+M(76)-M(86)-M(87)+M(88)+M(98)+M(99)-M(100)-M(104)+M(106)-M(110)+M(112)+M(117)+M(118)-M(123)-M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(158)-M(160)-M(195)+M(201))) * den(26)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,98)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)+M(61) &
    -M(64)-M(73)+M(76)-M(86)-M(87)+M(88)+M(98)+M(99)-M(100)+M(104)-M(106)-M(110)+M(112)-M(117)+M(118)+M(123)-M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(134)-M(136)-M(196)+M(202))) * den(26)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,107)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(37)-M(38)-M(39)+M(40)+M(104)-M(106)-M(117)+M(123))+c(6)*(M(134)-M(136)-M(158)+M(160) &
    +M(195)-M(196)-M(201)+M(202))) * den(26)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,116)
  Gcoeff = (c(5)*(-M(73)+M(76)+M(88)-M(100)-M(110)+M(112)+M(118)-M(124)-M(125)+M(126)+M(128)-M(130))+c(6)*(-M(155)+M(156)+M(158) &
    -M(160)-M(195)+M(201)+M(225)-M(249))) * den(26)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,99)
  Gcoeff = (c(5)*(-M(73)+M(76)+M(88)-M(100)-M(110)+M(112)+M(118)-M(124)-M(125)+M(126)+M(128)-M(130))+c(6)*(-M(131)+M(132)+M(134) &
    -M(136)-M(196)+M(202)+M(226)-M(250))) * den(26)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,108)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(134)-M(136)+M(155)-M(156)-M(158)+M(160)+M(195)-M(196)-M(201)+M(202)-M(225)+M(226)+M(249) &
    -M(250))) * den(26)
  T3sum(1:5,57) = T3sum(1:5,57) + Gcoeff * G1tensor(:,117)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(10)-M(11)-M(13)-M(15)-M(17)-M(20)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(45)+M(51)+M(52)+M(53)+M(54)+M(55)+M(57)+M(63)+M(64)+M(65)+M(74)+M(75)+M(77)+M(83)+M(84)+M(85)+M(86)+M(87)+M(89)+M(97) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(159)+M(219)))
  T4sum(1:15,22) = T4sum(1:15,22) + Gcoeff * G2tensor(:,58)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(45)+M(51)+M(52)+M(57)+M(63)+M(64)+M(68)+M(74)+M(75)+M(80)+M(83)+M(84)+M(85)+M(86)+M(87)+M(92)+M(97) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(135)+M(220)))
  T4sum(1:15,22) = T4sum(1:15,22) + Gcoeff * G2tensor(:,82)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(5)-M(6)+M(7)-M(8)+M(13)-M(14)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)+M(101)-M(102)-M(103)-M(104)+M(105)+M(106)-M(109)+M(111)+M(115)+M(117)-M(121) &
    -M(123))+c(6)*(M(135)-M(159)-M(219)+M(220)))
  T4sum(1:15,22) = T4sum(1:15,22) + Gcoeff * G2tensor(:,106)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(10)-M(12)-M(14)-M(15)-M(17)-M(20)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(38) &
    -M(39)+M(45)+M(46)+M(52)+M(53)+M(54)+M(55)+M(57)+M(58)+M(64)+M(65)+M(76)+M(77)+M(83)+M(84)+M(86)+M(87)+M(88)+M(89)+M(95)+M(96) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(156)+M(225)))
  T4sum(1:15,22) = T4sum(1:15,22) + Gcoeff * G2tensor(:,59)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(45)+M(46)+M(52)+M(57)+M(58)+M(64)+M(68)+M(76)+M(80)+M(83)+M(84)+M(86)+M(87)+M(88)+M(92)+M(95)+M(96) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(132)+M(226)))
  T4sum(1:15,22) = T4sum(1:15,22) + Gcoeff * G2tensor(:,83)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(5)-M(6)+M(7)-M(8)-M(13)+M(14)-M(19)+M(20)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)+M(101)+M(102)-M(103)-M(104)-M(105)+M(106)-M(109)-M(111)+M(115)+M(117)+M(121) &
    -M(123))+c(6)*(M(132)-M(156)-M(225)+M(226)))
  T4sum(1:15,22) = T4sum(1:15,22) + Gcoeff * G2tensor(:,107)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(37)-M(38)-M(39)+M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)+M(76)-M(85)+M(88)+M(95)+M(96)-M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(156)-M(159)-M(219)+M(225)))
  T4sum(1:15,22) = T4sum(1:15,22) + Gcoeff * G2tensor(:,60)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)+M(76)-M(85)+M(88)+M(95)+M(96)-M(97)+M(102)-M(105)-M(111)+M(112)-M(116)+M(118)+M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(132)-M(135)-M(220)+M(226)))
  T4sum(1:15,22) = T4sum(1:15,22) + Gcoeff * G2tensor(:,84)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(37)+M(38)+M(39)-M(40)+M(102)-M(105)-M(111)+M(121))+c(6)*(M(132)-M(135)-M(156)+M(159) &
    +M(219)-M(220)-M(225)+M(226)))
  T4sum(1:15,22) = T4sum(1:15,22) + Gcoeff * G2tensor(:,108)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(9)-M(11)-M(13)-M(16)-M(18)-M(19)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(37) &
    -M(40)+M(48)+M(49)+M(51)+M(53)+M(54)+M(55)+M(60)+M(61)+M(63)+M(65)+M(71)+M(72)+M(74)+M(75)+M(76)+M(77)+M(88)+M(89)+M(98)+M(99) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(158)+M(201)))
  T4sum(1:15,24) = T4sum(1:15,24) + Gcoeff * G2tensor(:,61)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(48)+M(49)+M(51)+M(60)+M(61)+M(63)+M(68)+M(71)+M(72)+M(74)+M(75)+M(76)+M(80)+M(88)+M(92)+M(98)+M(99) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(134)+M(202)))
  T4sum(1:15,24) = T4sum(1:15,24) + Gcoeff * G2tensor(:,85)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(5)+M(6)-M(7)+M(8)+M(13)-M(14)+M(19)-M(20)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)-M(101)-M(102)+M(103)+M(104)+M(105)-M(106)+M(109)+M(111)-M(115)-M(117)-M(121) &
    +M(123))+c(6)*(M(134)-M(158)-M(201)+M(202)))
  T4sum(1:15,24) = T4sum(1:15,24) + Gcoeff * G2tensor(:,109)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(9)-M(12)-M(14)-M(16)-M(18)-M(19)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(46)+M(48)+M(49)+M(53)+M(54)+M(55)+M(58)+M(60)+M(61)+M(65)+M(71)+M(72)+M(77)+M(85)+M(89)+M(95)+M(96)+M(97)+M(98)+M(99) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(157)+M(243)))
  T4sum(1:15,24) = T4sum(1:15,24) + Gcoeff * G2tensor(:,62)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(46)+M(48)+M(49)+M(58)+M(60)+M(61)+M(68)+M(71)+M(72)+M(80)+M(85)+M(92)+M(95)+M(96)+M(97)+M(98)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(133)+M(244)))
  T4sum(1:15,24) = T4sum(1:15,24) + Gcoeff * G2tensor(:,86)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(5)+M(6)-M(7)+M(8)-M(13)+M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)-M(101)+M(102)+M(103)+M(104)-M(105)-M(106)+M(109)-M(111)-M(115)-M(117)+M(121) &
    +M(123))+c(6)*(M(133)-M(157)-M(243)+M(244)))
  T4sum(1:15,24) = T4sum(1:15,24) + Gcoeff * G2tensor(:,110)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(37)-M(38)-M(39)+M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)-M(102)+M(105)+M(111)-M(112)+M(116)-M(118)-M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(157)-M(158)-M(201)+M(243)))
  T4sum(1:15,24) = T4sum(1:15,24) + Gcoeff * G2tensor(:,63)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(134)-M(202)+M(244)))
  T4sum(1:15,24) = T4sum(1:15,24) + Gcoeff * G2tensor(:,87)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(37)+M(38)+M(39)-M(40)+M(102)-M(105)-M(111)+M(121))+c(6)*(M(133)-M(134)-M(157)+M(158) &
    +M(201)-M(202)-M(243)+M(244)))
  T4sum(1:15,24) = T4sum(1:15,24) + Gcoeff * G2tensor(:,111)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(9)-M(12)-M(14)-M(16)-M(18)-M(19)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(46)+M(48)+M(49)+M(53)+M(54)+M(55)+M(58)+M(60)+M(61)+M(65)+M(71)+M(72)+M(77)+M(85)+M(89)+M(95)+M(96)+M(97)+M(98)+M(99) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(157)+M(243)))
  T4sum(1:15,25) = T4sum(1:15,25) + Gcoeff * G2tensor(:,64)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(46)+M(48)+M(49)+M(58)+M(60)+M(61)+M(68)+M(71)+M(72)+M(80)+M(85)+M(92)+M(95)+M(96)+M(97)+M(98)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(133)+M(244)))
  T4sum(1:15,25) = T4sum(1:15,25) + Gcoeff * G2tensor(:,88)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(5)+M(6)-M(7)+M(8)-M(13)+M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)-M(101)+M(102)+M(103)+M(104)-M(105)-M(106)+M(109)-M(111)-M(115)-M(117)+M(121) &
    +M(123))+c(6)*(M(133)-M(157)-M(243)+M(244)))
  T4sum(1:15,25) = T4sum(1:15,25) + Gcoeff * G2tensor(:,112)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(10)-M(12)-M(14)-M(15)-M(18)-M(20)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(45)+M(46)+M(49)+M(53)+M(54)+M(55)+M(57)+M(58)+M(61)+M(65)+M(73)+M(77)+M(83)+M(84)+M(89)+M(95)+M(96)+M(98)+M(99) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(155)+M(249)))
  T4sum(1:15,25) = T4sum(1:15,25) + Gcoeff * G2tensor(:,65)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(45)+M(46)+M(49)+M(57)+M(58)+M(61)+M(68)+M(73)+M(80)+M(83)+M(84)+M(92)+M(95)+M(96)+M(98)+M(99) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(131)+M(250)))
  T4sum(1:15,25) = T4sum(1:15,25) + Gcoeff * G2tensor(:,89)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)+M(5)-M(6)-M(7)+M(8)-M(13)+M(14)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)+M(101)+M(102)-M(103)+M(104)-M(105)-M(106)-M(109)-M(111)+M(115)-M(117)+M(121) &
    +M(123))+c(6)*(M(131)-M(155)-M(249)+M(250)))
  T4sum(1:15,25) = T4sum(1:15,25) + Gcoeff * G2tensor(:,113)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(37)+M(38)+M(39)-M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)+M(73)+M(83)+M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(155)-M(157)-M(243)+M(249)))
  T4sum(1:15,25) = T4sum(1:15,25) + Gcoeff * G2tensor(:,66)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)+M(73)+M(83)+M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)+M(110)+M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(131)-M(133)-M(244)+M(250)))
  T4sum(1:15,25) = T4sum(1:15,25) + Gcoeff * G2tensor(:,90)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(101)-M(103)-M(109)+M(115))+c(6)*(M(131)-M(133)-M(155)+M(157) &
    +M(243)-M(244)-M(249)+M(250)))
  T4sum(1:15,25) = T4sum(1:15,25) + Gcoeff * G2tensor(:,114)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(9)-M(11)-M(13)-M(16)-M(17)-M(19)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(48)+M(51)+M(52)+M(53)+M(54)+M(55)+M(60)+M(63)+M(64)+M(65)+M(71)+M(72)+M(73)+M(74)+M(75)+M(77)+M(86)+M(87)+M(89) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(160)+M(195)))
  T4sum(1:15,27) = T4sum(1:15,27) + Gcoeff * G2tensor(:,67)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(48)+M(51)+M(52)+M(60)+M(63)+M(64)+M(68)+M(71)+M(72)+M(73)+M(74)+M(75)+M(80)+M(86)+M(87)+M(92) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(136)+M(196)))
  T4sum(1:15,27) = T4sum(1:15,27) + Gcoeff * G2tensor(:,91)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)-M(5)+M(6)+M(7)-M(8)+M(13)-M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)-M(101)-M(102)+M(103)-M(104)+M(105)+M(106)+M(109)+M(111)-M(115)+M(117)-M(121) &
    -M(123))+c(6)*(M(136)-M(160)-M(195)+M(196)))
  T4sum(1:15,27) = T4sum(1:15,27) + Gcoeff * G2tensor(:,115)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(10)-M(11)-M(13)-M(15)-M(17)-M(20)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(45)+M(51)+M(52)+M(53)+M(54)+M(55)+M(57)+M(63)+M(64)+M(65)+M(74)+M(75)+M(77)+M(83)+M(84)+M(85)+M(86)+M(87)+M(89)+M(97) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(159)+M(219)))
  T4sum(1:15,27) = T4sum(1:15,27) + Gcoeff * G2tensor(:,68)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(45)+M(51)+M(52)+M(57)+M(63)+M(64)+M(68)+M(74)+M(75)+M(80)+M(83)+M(84)+M(85)+M(86)+M(87)+M(92)+M(97) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(135)+M(220)))
  T4sum(1:15,27) = T4sum(1:15,27) + Gcoeff * G2tensor(:,92)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(5)-M(6)+M(7)-M(8)+M(13)-M(14)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)+M(101)-M(102)-M(103)-M(104)+M(105)+M(106)-M(109)+M(111)+M(115)+M(117)-M(121) &
    -M(123))+c(6)*(M(135)-M(159)-M(219)+M(220)))
  T4sum(1:15,27) = T4sum(1:15,27) + Gcoeff * G2tensor(:,116)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(37)+M(38)+M(39)-M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100)-M(101)+M(103)+M(109)-M(110)-M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(159)-M(160)-M(195)+M(219)))
  T4sum(1:15,27) = T4sum(1:15,27) + Gcoeff * G2tensor(:,69)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(135)-M(136)-M(196)+M(220)))
  T4sum(1:15,27) = T4sum(1:15,27) + Gcoeff * G2tensor(:,93)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(101)-M(103)-M(109)+M(115))+c(6)*(M(135)-M(136)-M(159)+M(160) &
    +M(195)-M(196)-M(219)+M(220)))
  T4sum(1:15,27) = T4sum(1:15,27) + Gcoeff * G2tensor(:,117)
  Gcoeff = (c(5)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(155)-M(157)-M(159) &
    +M(160)+M(195)-M(219)-M(243)+M(249))) * den(18)
  T3sum(1:15,57) = T3sum(1:15,57) + Gcoeff * G2tensor(:,70)
  Gcoeff = (c(5)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(131)-M(133)-M(135) &
    +M(136)+M(196)-M(220)-M(244)+M(250))) * den(18)
  T3sum(1:15,57) = T3sum(1:15,57) + Gcoeff * G2tensor(:,94)
  Gcoeff = (c(6)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(18)
  T3sum(1:15,57) = T3sum(1:15,57) + Gcoeff * G2tensor(:,118)
  Gcoeff = (c(5)*(-M(73)+M(76)+M(88)-M(100)-M(110)+M(112)+M(118)-M(124)-M(125)+M(126)+M(128)-M(130))+c(6)*(-M(155)+M(156)+M(158) &
    -M(160)-M(195)+M(201)+M(225)-M(249))) * den(18)
  T3sum(1:15,57) = T3sum(1:15,57) + Gcoeff * G2tensor(:,71)
  Gcoeff = (c(5)*(-M(73)+M(76)+M(88)-M(100)-M(110)+M(112)+M(118)-M(124)-M(125)+M(126)+M(128)-M(130))+c(6)*(-M(131)+M(132)+M(134) &
    -M(136)-M(196)+M(202)+M(226)-M(250))) * den(18)
  T3sum(1:15,57) = T3sum(1:15,57) + Gcoeff * G2tensor(:,95)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(134)-M(136)+M(155)-M(156)-M(158)+M(160)+M(195)-M(196)-M(201)+M(202)-M(225)+M(226)+M(249) &
    -M(250))) * den(18)
  T3sum(1:15,57) = T3sum(1:15,57) + Gcoeff * G2tensor(:,119)
  Gcoeff = (c(5)*(-M(76)+M(85)-M(88)+M(97)-M(112)+M(116)-M(118)+M(122)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(156)+M(157)-M(158) &
    +M(159)-M(201)+M(219)-M(225)+M(243))) * den(18)
  T3sum(1:15,57) = T3sum(1:15,57) + Gcoeff * G2tensor(:,72)
  Gcoeff = (c(5)*(-M(76)+M(85)-M(88)+M(97)-M(112)+M(116)-M(118)+M(122)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(132)+M(133)-M(134) &
    +M(135)-M(202)+M(220)-M(226)+M(244))) * den(18)
  T3sum(1:15,57) = T3sum(1:15,57) + Gcoeff * G2tensor(:,96)
  Gcoeff = (c(6)*(-M(132)+M(133)-M(134)+M(135)+M(156)-M(157)+M(158)-M(159)+M(201)-M(202)-M(219)+M(220)+M(225)-M(226)-M(243) &
    +M(244))) * den(18)
  T3sum(1:15,57) = T3sum(1:15,57) + Gcoeff * G2tensor(:,120)
  Gcoeff = (c(4)*(M(1)-M(2)+M(5)-M(6)-M(7)+M(8)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)+M(42)+M(45)+M(52) &
    -M(54)-M(57)+M(64)-M(66)+M(69)+M(76)-M(77)-M(78)-M(79)+M(80)+M(81)-M(82)+M(83)-M(84)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94) &
    -M(97))+c(6)*(M(138)-M(205)+M(224)-M(235))) * den(43)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,4)
  Gcoeff = (c(4)*(M(1)-M(2)+M(5)-M(6)-M(7)+M(8)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)-M(42)-M(45)-M(52) &
    +M(54)-M(57)+M(64)+M(66)+M(69)+M(76)+M(77)+M(78)+M(79)-M(80)+M(81)-M(82)-M(83)-M(84)-M(85)-M(86)+M(87)+M(88)+M(91)-M(94) &
    -M(97))+c(6)*(-M(149)+M(189)+M(207)-M(218))) * den(43)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,13)
  Gcoeff = (c(5)*(-M(42)-M(45)-M(52)+M(54)+M(66)+M(77)+M(78)+M(79)-M(80)-M(83)-M(86)+M(91))+c(6)*(-M(138)-M(149)+M(189)+M(205) &
    +M(207)-M(218)-M(224)+M(235))) * den(43)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,22)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(11)-M(12)+M(19)-M(20)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(42)+M(45)+M(52) &
    -M(54)-M(57)-M(64)-M(66)+M(69)+M(76)-M(77)-M(78)-M(79)+M(80)+M(81)+M(82)+M(83)-M(84)-M(85)+M(86)-M(87)+M(88)-M(91)+M(94) &
    -M(97))+c(6)*(M(141)-M(175)-M(206)+M(214))) * den(43)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,5)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(11)-M(12)+M(19)-M(20)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(42)-M(45)-M(52) &
    +M(54)-M(57)-M(64)+M(66)+M(69)+M(76)+M(77)+M(78)+M(79)-M(80)+M(81)+M(82)-M(83)-M(84)-M(85)-M(86)-M(87)+M(88)+M(91)+M(94) &
    -M(97))+c(6)*(-M(135)+M(199)+M(204)-M(220))) * den(43)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,14)
  Gcoeff = (c(5)*(-M(42)-M(45)-M(52)+M(54)+M(66)+M(77)+M(78)+M(79)-M(80)-M(83)-M(86)+M(91))+c(6)*(-M(135)-M(141)+M(175)+M(199) &
    +M(204)+M(206)-M(214)-M(220))) * den(43)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,23)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(6)*(-M(138)+M(141)-M(175)+M(205) &
    -M(206)+M(214)-M(224)+M(235))) * den(43)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,6)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(6)*(-M(135)+M(149)-M(189)+M(199) &
    +M(204)-M(207)+M(218)-M(220))) * den(43)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,15)
  Gcoeff = (c(6)*(-M(135)+M(138)-M(141)+M(149)+M(175)-M(189)+M(199)+M(204)-M(205)+M(206)-M(207)-M(214)+M(218)-M(220)+M(224) &
    -M(235))) * den(43)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,24)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(42)+M(45)+M(52) &
    -M(54)+M(57)+M(64)-M(66)-M(69)+M(76)-M(77)-M(78)-M(79)+M(80)-M(81)-M(82)+M(83)+M(84)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94) &
    -M(97))+c(6)*(M(132)-M(203)+M(226)-M(241))) * den(43)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,7)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(42)-M(45)-M(52) &
    +M(54)+M(57)+M(64)+M(66)-M(69)+M(76)+M(77)+M(78)+M(79)-M(80)-M(81)-M(82)-M(83)+M(84)-M(85)-M(86)+M(87)+M(88)+M(91)-M(94) &
    -M(97))+c(6)*(-M(151)+M(165)+M(208)-M(212))) * den(43)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,16)
  Gcoeff = (c(5)*(-M(42)-M(45)-M(52)+M(54)+M(66)+M(77)+M(78)+M(79)-M(80)-M(83)-M(86)+M(91))+c(6)*(-M(132)-M(151)+M(165)+M(203) &
    +M(208)-M(212)-M(226)+M(241))) * den(43)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,25)
  Gcoeff = (c(4)*(M(1)-M(2)+M(5)-M(6)-M(7)+M(8)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)+M(42)+M(45)+M(52) &
    -M(54)-M(57)+M(64)-M(66)+M(69)+M(76)-M(77)-M(78)-M(79)+M(80)+M(81)-M(82)+M(83)-M(84)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94) &
    -M(97))+c(6)*(M(138)-M(205)+M(224)-M(235))) * den(43)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,8)
  Gcoeff = (c(4)*(M(1)-M(2)+M(5)-M(6)-M(7)+M(8)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)-M(42)-M(45)-M(52) &
    +M(54)-M(57)+M(64)+M(66)+M(69)+M(76)+M(77)+M(78)+M(79)-M(80)+M(81)-M(82)-M(83)-M(84)-M(85)-M(86)+M(87)+M(88)+M(91)-M(94) &
    -M(97))+c(6)*(-M(149)+M(189)+M(207)-M(218))) * den(43)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,17)
  Gcoeff = (c(5)*(-M(42)-M(45)-M(52)+M(54)+M(66)+M(77)+M(78)+M(79)-M(80)-M(83)-M(86)+M(91))+c(6)*(-M(138)-M(149)+M(189)+M(205) &
    +M(207)-M(218)-M(224)+M(235))) * den(43)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,26)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(27)+M(28)+M(33)-M(34)-M(57)+M(69)+M(81)-M(84))+c(6)*(-M(132)+M(138)+M(203)-M(205)+M(224) &
    -M(226)-M(235)+M(241))) * den(43)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,9)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(27)+M(28)+M(33)-M(34)-M(57)+M(69)+M(81)-M(84))+c(6)*(-M(149)+M(151)-M(165)+M(189)+M(207) &
    -M(208)+M(212)-M(218))) * den(43)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,18)
  Gcoeff = (c(6)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(43)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,27)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    -M(60)+M(61)+M(69)-M(72)+M(80)+M(81)-M(82)-M(94)+M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(137)-M(139)-M(238)+M(248))) * den(14)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,31)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    -M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(169)+M(211)+M(229)-M(230))) * den(14)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,32)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(6)*(-M(137)+M(139)-M(169)+M(211) &
    +M(229)-M(230)+M(238)-M(248))) * den(14)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,33)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)+M(61)-M(69)+M(72)+M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(143)-M(242)+M(244))) * den(14)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,40)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)-M(61)-M(69)+M(72)+M(80)-M(81)+M(82)+M(94)-M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(187)+M(193)+M(228)-M(231))) * den(14)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,41)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(6)*(-M(133)+M(143)-M(187)+M(193) &
    +M(228)-M(231)+M(242)-M(244))) * den(14)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,42)
  Gcoeff = (c(5)*(M(60)-M(69)+M(72)-M(81)+M(103)-M(107)+M(109)-M(113)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(133)-M(137)+M(139) &
    -M(143)+M(238)-M(242)+M(244)-M(248))) * den(14)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,49)
  Gcoeff = (c(5)*(M(60)-M(69)+M(72)-M(81)+M(103)-M(107)+M(109)-M(113)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(169)-M(187)+M(193) &
    -M(211)+M(228)-M(229)+M(230)-M(231))) * den(14)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,50)
  Gcoeff = (c(6)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(14)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,51)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    -M(60)+M(61)+M(69)-M(72)-M(80)+M(81)-M(82)-M(94)+M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(150)+M(153)+M(188)-M(194))) * den(14)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,34)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    -M(60)+M(61)+M(69)-M(72)+M(80)+M(81)-M(82)-M(94)+M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(137)-M(139)-M(238)+M(248))) * den(14)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,35)
  Gcoeff = (c(5)*(-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59)+M(80))+c(6)*(M(137)-M(139)+M(150)-M(153) &
    -M(188)+M(194)-M(238)+M(248))) * den(14)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,36)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(151)+M(152)+M(170)-M(212))) * den(14)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,43)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)+M(61)-M(69)+M(72)+M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(143)-M(242)+M(244))) * den(14)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,44)
  Gcoeff = (c(5)*(-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59)+M(80))+c(6)*(M(133)-M(143)+M(151)-M(152) &
    -M(170)+M(212)-M(242)+M(244))) * den(14)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,45)
  Gcoeff = (c(5)*(M(60)-M(69)+M(72)-M(81)+M(103)-M(107)+M(109)-M(113)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(150)-M(151)+M(152) &
    -M(153)+M(170)-M(188)+M(194)-M(212))) * den(14)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,52)
  Gcoeff = (c(5)*(M(60)-M(69)+M(72)-M(81)+M(103)-M(107)+M(109)-M(113)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(133)-M(137)+M(139) &
    -M(143)+M(238)-M(242)+M(244)-M(248))) * den(14)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,53)
  Gcoeff = (c(6)*(M(133)-M(137)+M(139)-M(143)-M(150)+M(151)-M(152)+M(153)-M(170)+M(188)-M(194)+M(212)+M(238)-M(242)+M(244) &
    -M(248))) * den(14)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,54)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(31)+M(32)+M(35)-M(36)+M(42)-M(47)-M(59) &
    -M(60)+M(61)+M(69)-M(72)+M(80)+M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(179)-M(181)-M(237)+M(247))) * den(14)
  T3sum(1:5,62) = T3sum(1:5,62) + Gcoeff * G1tensor(:,58)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)+M(42)-M(47)-M(59) &
    -M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(167)+M(209)+M(239)-M(240))) * den(14)
  T3sum(1:5,62) = T3sum(1:5,62) + Gcoeff * G1tensor(:,59)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(6)*(-M(167)-M(179)+M(181)+M(209) &
    +M(237)+M(239)-M(240)-M(247))) * den(14)
  T3sum(1:5,62) = T3sum(1:5,62) + Gcoeff * G1tensor(:,60)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    -M(60)+M(61)+M(69)-M(72)-M(80)+M(81)-M(82)-M(94)+M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(187)-M(193)-M(228)+M(231))) * den(14)
  T3sum(1:5,62) = T3sum(1:5,62) + Gcoeff * G1tensor(:,67)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    -M(60)-M(61)+M(69)-M(72)-M(80)+M(81)+M(82)+M(94)-M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(133)+M(143)+M(242)-M(244))) * den(14)
  T3sum(1:5,62) = T3sum(1:5,62) + Gcoeff * G1tensor(:,68)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(6)*(-M(133)+M(143)-M(187)+M(193) &
    +M(228)-M(231)+M(242)-M(244))) * den(14)
  T3sum(1:5,62) = T3sum(1:5,62) + Gcoeff * G1tensor(:,69)
  Gcoeff = (c(5)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(6)*(-M(179)+M(181)+M(187) &
    -M(193)-M(228)+M(231)+M(237)-M(247))) * den(14)
  T3sum(1:5,62) = T3sum(1:5,62) + Gcoeff * G1tensor(:,76)
  Gcoeff = (c(5)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(6)*(-M(133)+M(143)+M(167) &
    -M(209)-M(239)+M(240)+M(242)-M(244))) * den(14)
  T3sum(1:5,62) = T3sum(1:5,62) + Gcoeff * G1tensor(:,77)
  Gcoeff = (c(6)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(14)
  T3sum(1:5,62) = T3sum(1:5,62) + Gcoeff * G1tensor(:,78)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)+M(61)-M(69)+M(72)+M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(134)-M(144)-M(200)+M(202))) * den(14)
  T3sum(1:5,64) = T3sum(1:5,64) + Gcoeff * G1tensor(:,61)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(31)+M(32)+M(35)-M(36)+M(42)-M(47)-M(59) &
    -M(60)+M(61)+M(69)-M(72)+M(80)+M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(179)-M(181)-M(237)+M(247))) * den(14)
  T3sum(1:5,64) = T3sum(1:5,64) + Gcoeff * G1tensor(:,62)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(134)+M(144)+M(179)-M(181)+M(200) &
    -M(202)-M(237)+M(247))) * den(14)
  T3sum(1:5,64) = T3sum(1:5,64) + Gcoeff * G1tensor(:,63)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(168)-M(197)+M(198)-M(210))) * den(14)
  T3sum(1:5,64) = T3sum(1:5,64) + Gcoeff * G1tensor(:,70)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    -M(60)+M(61)+M(69)-M(72)-M(80)+M(81)-M(82)-M(94)+M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(187)-M(193)-M(228)+M(231))) * den(14)
  T3sum(1:5,64) = T3sum(1:5,64) + Gcoeff * G1tensor(:,71)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(168)+M(187)-M(193)+M(197)-M(198) &
    +M(210)-M(228)+M(231))) * den(14)
  T3sum(1:5,64) = T3sum(1:5,64) + Gcoeff * G1tensor(:,72)
  Gcoeff = (c(5)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(6)*(-M(134)+M(144)+M(168) &
    -M(197)+M(198)+M(200)-M(202)-M(210))) * den(14)
  T3sum(1:5,64) = T3sum(1:5,64) + Gcoeff * G1tensor(:,79)
  Gcoeff = (c(5)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(6)*(-M(179)+M(181)+M(187) &
    -M(193)-M(228)+M(231)+M(237)-M(247))) * den(14)
  T3sum(1:5,64) = T3sum(1:5,64) + Gcoeff * G1tensor(:,80)
  Gcoeff = (c(6)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(14)
  T3sum(1:5,64) = T3sum(1:5,64) + Gcoeff * G1tensor(:,81)
  Gcoeff = (c(5)*(-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59)+M(80))+c(6)*(M(137)-M(139)+M(150)-M(153) &
    -M(188)+M(194)-M(238)+M(248))) * den(15)
  T3sum(1:15,82) = T3sum(1:15,82) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(5)*(-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59)+M(80))+c(6)*(M(133)-M(143)+M(151)-M(152) &
    -M(170)+M(212)-M(242)+M(244))) * den(15)
  T3sum(1:15,82) = T3sum(1:15,82) + Gcoeff * G2tensor(:,13)
  Gcoeff = (c(6)*(M(133)-M(137)+M(139)-M(143)-M(150)+M(151)-M(152)+M(153)-M(170)+M(188)-M(194)+M(212)+M(238)-M(242)+M(244) &
    -M(248))) * den(15)
  T3sum(1:15,82) = T3sum(1:15,82) + Gcoeff * G2tensor(:,16)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(-M(134)+M(158)+M(179)-M(185)+M(201) &
    -M(202)-M(245)+M(247))) * den(42)
  T3sum(1:15,7) = T3sum(1:15,7) + Gcoeff * G2tensor(:,28)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(-M(136)+M(160)+M(180)-M(186)+M(195) &
    -M(196)-M(221)+M(223))) * den(42)
  T3sum(1:15,7) = T3sum(1:15,7) + Gcoeff * G2tensor(:,29)
  Gcoeff = (c(6)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(42)
  T3sum(1:15,7) = T3sum(1:15,7) + Gcoeff * G2tensor(:,30)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(6)*(-M(158)+M(168)+M(185)-M(187) &
    +M(198)-M(201)-M(231)+M(245))) * den(48)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,31)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(6)*(-M(147)-M(174)+M(177)+M(182) &
    +M(189)-M(190)-M(192)+M(207))) * den(48)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,32)
  Gcoeff = (c(6)*(-M(147)+M(158)-M(168)-M(174)+M(177)+M(182)-M(185)+M(187)+M(189)-M(190)-M(192)-M(198)+M(201)+M(207)+M(231) &
    -M(245))) * den(48)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,33)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(134)+M(144)+M(179)-M(181)+M(200) &
    -M(202)-M(237)+M(247))) * den(69)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,34)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(150)+M(153)-M(171)+M(183)-M(184) &
    +M(188)-M(194)+M(213))) * den(69)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,35)
  Gcoeff = (c(6)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(69)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,36)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(160)+M(174)+M(186)-M(189) &
    +M(192)-M(195)-M(207)+M(221))) * den(49)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,37)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(153)-M(168)+M(171)+M(184) &
    +M(187)-M(188)-M(198)+M(231))) * den(49)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,38)
  Gcoeff = (c(6)*(-M(153)+M(160)-M(168)+M(171)-M(174)+M(184)-M(186)+M(187)-M(188)+M(189)-M(192)+M(195)-M(198)+M(207)-M(221) &
    +M(231))) * den(49)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,39)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(136)+M(150)+M(180)-M(183)+M(194) &
    -M(196)-M(213)+M(223))) * den(70)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,40)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(144)+M(147)-M(177)+M(181)-M(182) &
    +M(190)-M(200)+M(237))) * den(70)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,41)
  Gcoeff = (c(6)*(M(136)-M(144)+M(147)-M(150)-M(177)-M(180)+M(181)-M(182)+M(183)+M(190)-M(194)+M(196)-M(200)+M(213)-M(223) &
    +M(237))) * den(70)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,42)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)+M(17)-M(18)-M(19)+M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)-M(63)-M(65)-M(66)-M(67)+M(68)+M(69)+M(70)+M(71)-M(72)-M(73)+M(74)-M(75)+M(76)-M(78)+M(81)+M(88)-M(90)+M(93) &
    -M(100))+c(6)*(M(147)-M(177)-M(182)+M(190))) * den(40)
  T4sum(1:35,48) = T4sum(1:35,48) + Gcoeff * G3tensor(:,10)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)-M(63)-M(65)-M(66)-M(67)+M(68)+M(69)+M(70)+M(71)-M(72)+M(73)+M(74)-M(75)-M(76)-M(78)+M(81)-M(88)-M(90)+M(93) &
    +M(100))+c(6)*(M(153)-M(171)-M(184)+M(188))) * den(40)
  T4sum(1:35,48) = T4sum(1:35,48) + Gcoeff * G3tensor(:,11)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(147)+M(153)-M(171)+M(177) &
    +M(182)-M(184)+M(188)-M(190))) * den(40)
  T4sum(1:35,48) = T4sum(1:35,48) + Gcoeff * G3tensor(:,12)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(6)*(-M(137)+M(139)-M(169)+M(211) &
    +M(229)-M(230)+M(238)-M(248))) * den(77)
  T3sum(1:15,70) = T3sum(1:15,70) + Gcoeff * G2tensor(:,11)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(6)*(-M(133)+M(143)-M(187)+M(193) &
    +M(228)-M(231)+M(242)-M(244))) * den(77)
  T3sum(1:15,70) = T3sum(1:15,70) + Gcoeff * G2tensor(:,14)
  Gcoeff = (c(6)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(77)
  T3sum(1:15,70) = T3sum(1:15,70) + Gcoeff * G2tensor(:,17)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(104)+M(114)+M(120)-M(123))+c(6)*(M(150)-M(153)+M(187)-M(188) &
    -M(193)+M(194)-M(228)+M(231))) * den(110)
  T3sum(1:15,50) = T3sum(1:15,50) + Gcoeff * G2tensor(:,12)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(104)+M(114)+M(120)-M(123))+c(6)*(M(151)-M(152)+M(169)-M(170) &
    -M(211)+M(212)-M(229)+M(230))) * den(110)
  T3sum(1:15,50) = T3sum(1:15,50) + Gcoeff * G2tensor(:,15)
  Gcoeff = (c(6)*(-M(150)+M(151)-M(152)+M(153)+M(169)-M(170)-M(187)+M(188)+M(193)-M(194)-M(211)+M(212)+M(228)-M(229)+M(230) &
    -M(231))) * den(110)
  T3sum(1:15,50) = T3sum(1:15,50) + Gcoeff * G2tensor(:,18)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    -M(60)+M(61)+M(69)-M(72)-M(80)+M(81)-M(82)-M(94)+M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(187)-M(193)-M(228)+M(231))) * den(14)
  T4sum(1:35,183) = T4sum(1:35,183) + Gcoeff * G3tensor(:,4)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)+M(103)-M(104)-M(107)+M(109)-M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(169)-M(211)-M(229)+M(230))) * den(14)
  T4sum(1:35,183) = T4sum(1:35,183) + Gcoeff * G3tensor(:,5)
  Gcoeff = (c(5)*(M(60)-M(69)+M(72)-M(81)+M(103)-M(107)+M(109)-M(113)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(169)-M(187)+M(193) &
    -M(211)+M(228)-M(229)+M(230)-M(231))) * den(14)
  T4sum(1:35,183) = T4sum(1:35,183) + Gcoeff * G3tensor(:,6)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(147)+M(153)-M(171)+M(177) &
    +M(182)-M(184)+M(188)-M(190))) * den(81)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,43)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(134)+M(136)+M(179)-M(180) &
    +M(196)-M(202)-M(223)+M(247))) * den(81)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,44)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(147)-M(153)+M(171)-M(177)+M(179)-M(180)-M(182)+M(184)-M(188)+M(190)+M(196)-M(202)-M(223) &
    +M(247))) * den(81)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,45)
  Gcoeff = (c(4)*(M(3)-M(4)-M(5)+M(6)-M(13)+M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)+M(63)-M(65)-M(66)-M(67)+M(68)+M(69)-M(70)+M(71)-M(72)-M(73)+M(74)+M(75)+M(76)-M(78)+M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(144)-M(181)+M(200)-M(237))) * den(40)
  T4sum(1:35,70) = T4sum(1:35,70) + Gcoeff * G3tensor(:,13)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)+M(17)-M(18)-M(19)+M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)-M(63)-M(65)-M(66)-M(67)+M(68)+M(69)+M(70)+M(71)-M(72)-M(73)+M(74)-M(75)+M(76)-M(78)+M(81)+M(88)-M(90)+M(93) &
    -M(100))+c(6)*(M(147)-M(177)-M(182)+M(190))) * den(40)
  T4sum(1:35,70) = T4sum(1:35,70) + Gcoeff * G3tensor(:,14)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(144)+M(147)-M(177)+M(181)-M(182) &
    +M(190)-M(200)+M(237))) * den(40)
  T4sum(1:35,70) = T4sum(1:35,70) + Gcoeff * G3tensor(:,15)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)-M(17)+M(18)+M(19)-M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)+M(63)-M(65)-M(66)-M(67)+M(68)-M(69)-M(70)+M(71)+M(72)+M(73)+M(74)+M(75)-M(76)-M(78)-M(81)-M(88)-M(90)-M(93) &
    +M(100))+c(6)*(M(136)-M(180)+M(196)-M(223))) * den(40)
  T4sum(1:35,72) = T4sum(1:35,72) + Gcoeff * G3tensor(:,16)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)-M(63)-M(65)-M(66)-M(67)+M(68)-M(69)+M(70)+M(71)+M(72)+M(73)+M(74)-M(75)-M(76)-M(78)-M(81)-M(88)-M(90)+M(93) &
    +M(100))+c(6)*(M(150)-M(183)+M(194)-M(213))) * den(40)
  T4sum(1:35,72) = T4sum(1:35,72) + Gcoeff * G3tensor(:,17)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(136)+M(150)+M(180)-M(183)+M(194) &
    -M(196)-M(213)+M(223))) * den(40)
  T4sum(1:35,72) = T4sum(1:35,72) + Gcoeff * G3tensor(:,18)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)-M(63)-M(65)-M(66)-M(67)+M(68)-M(69)+M(70)+M(71)+M(72)+M(73)+M(74)-M(75)-M(76)-M(78)-M(81)-M(88)-M(90)+M(93) &
    +M(100))+c(6)*(M(150)-M(183)+M(194)-M(213))) * den(40)
  T4sum(1:35,73) = T4sum(1:35,73) + Gcoeff * G3tensor(:,25)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)-M(63)-M(65)-M(66)-M(67)+M(68)+M(69)+M(70)+M(71)-M(72)+M(73)+M(74)-M(75)-M(76)-M(78)+M(81)-M(88)-M(90)+M(93) &
    +M(100))+c(6)*(M(153)-M(171)-M(184)+M(188))) * den(40)
  T4sum(1:35,73) = T4sum(1:35,73) + Gcoeff * G3tensor(:,26)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(150)+M(153)-M(171)+M(183)-M(184) &
    +M(188)-M(194)+M(213))) * den(40)
  T4sum(1:35,73) = T4sum(1:35,73) + Gcoeff * G3tensor(:,27)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)+M(63)-M(65)-M(66)-M(67)+M(68)-M(69)-M(70)+M(71)+M(72)-M(73)+M(74)+M(75)+M(76)-M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(134)-M(179)+M(202)-M(247))) * den(40)
  T4sum(1:35,75) = T4sum(1:35,75) + Gcoeff * G3tensor(:,28)
  Gcoeff = (c(4)*(M(3)-M(4)-M(5)+M(6)-M(13)+M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)+M(63)-M(65)-M(66)-M(67)+M(68)+M(69)-M(70)+M(71)-M(72)-M(73)+M(74)+M(75)+M(76)-M(78)+M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(144)-M(181)+M(200)-M(237))) * den(40)
  T4sum(1:35,75) = T4sum(1:35,75) + Gcoeff * G3tensor(:,29)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(134)+M(144)+M(179)-M(181)+M(200) &
    -M(202)-M(237)+M(247))) * den(40)
  T4sum(1:35,75) = T4sum(1:35,75) + Gcoeff * G3tensor(:,30)
  Gcoeff = (c(6)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(50)
  T3sum(1:35,72) = T3sum(1:35,72) + Gcoeff * G3tensor(:,37)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(147)-M(153)+M(171)-M(177)+M(179)-M(180)-M(182)+M(184)-M(188)+M(190)+M(196)-M(202)-M(223) &
    +M(247))) * den(50)
  T3sum(1:35,72) = T3sum(1:35,72) + Gcoeff * G3tensor(:,38)
  Gcoeff = (c(6)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(50)
  T3sum(1:35,72) = T3sum(1:35,72) + Gcoeff * G3tensor(:,39)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(134)+M(144)+M(179)-M(181)+M(200) &
    -M(202)-M(237)+M(247))) * den(69)
  T3sum(1:15,64) = T3sum(1:15,64) + Gcoeff * G2tensor(:,19)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(168)+M(187)-M(193)+M(197)-M(198) &
    +M(210)-M(228)+M(231))) * den(69)
  T3sum(1:15,64) = T3sum(1:15,64) + Gcoeff * G2tensor(:,22)
  Gcoeff = (c(6)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(69)
  T3sum(1:15,64) = T3sum(1:15,64) + Gcoeff * G2tensor(:,25)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(147)+M(153)-M(171)+M(177) &
    +M(182)-M(184)+M(188)-M(190))) * den(81)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,46)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(158)+M(160)+M(185)-M(186) &
    +M(195)-M(201)-M(221)+M(245))) * den(81)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,47)
  Gcoeff = (c(6)*(M(147)-M(153)-M(158)+M(160)+M(171)-M(177)-M(182)+M(184)+M(185)-M(186)-M(188)+M(190)+M(195)-M(201)-M(221) &
    +M(245))) * den(81)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,48)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)-M(41)-M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)+M(66)-M(67)-M(68)-M(69)-M(70)-M(71)+M(72)-M(73)+M(74)+M(75)+M(76)+M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(168)-M(187)+M(198)-M(231))) * den(40)
  T4sum(1:35,106) = T4sum(1:35,106) + Gcoeff * G3tensor(:,40)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(41)-M(48)-M(51) &
    +M(53)+M(60)+M(63)+M(65)+M(66)+M(67)-M(68)-M(69)-M(70)-M(71)+M(72)-M(73)-M(74)+M(75)+M(76)+M(78)-M(81)+M(88)+M(90)-M(93) &
    -M(100))+c(6)*(-M(153)+M(171)+M(184)-M(188))) * den(40)
  T4sum(1:35,106) = T4sum(1:35,106) + Gcoeff * G3tensor(:,41)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(153)-M(168)+M(171)+M(184) &
    +M(187)-M(188)-M(198)+M(231))) * den(40)
  T4sum(1:35,106) = T4sum(1:35,106) + Gcoeff * G3tensor(:,42)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)+M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)-M(66)-M(67)-M(68)-M(69)-M(70)+M(71)+M(72)+M(73)+M(74)+M(75)-M(76)-M(78)-M(81)-M(88)-M(90)-M(93) &
    +M(100))+c(6)*(M(160)-M(186)+M(195)-M(221))) * den(40)
  T4sum(1:35,108) = T4sum(1:35,108) + Gcoeff * G3tensor(:,19)
  Gcoeff = (c(4)*(M(3)-M(4)-M(5)+M(6)-M(13)+M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)-M(41)+M(48)-M(51) &
    +M(53)+M(60)+M(63)+M(65)-M(66)+M(67)-M(68)-M(69)-M(70)+M(71)+M(72)+M(73)-M(74)+M(75)-M(76)-M(78)-M(81)-M(88)+M(90)-M(93) &
    +M(100))+c(6)*(M(174)-M(189)+M(192)-M(207))) * den(40)
  T4sum(1:35,108) = T4sum(1:35,108) + Gcoeff * G3tensor(:,20)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(160)+M(174)+M(186)-M(189) &
    +M(192)-M(195)-M(207)+M(221))) * den(40)
  T4sum(1:35,108) = T4sum(1:35,108) + Gcoeff * G3tensor(:,21)
  Gcoeff = (c(4)*(M(3)-M(4)-M(5)+M(6)-M(13)+M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)-M(41)+M(48)-M(51) &
    +M(53)+M(60)+M(63)+M(65)-M(66)+M(67)-M(68)-M(69)-M(70)+M(71)+M(72)+M(73)-M(74)+M(75)-M(76)-M(78)-M(81)-M(88)+M(90)-M(93) &
    +M(100))+c(6)*(M(174)-M(189)+M(192)-M(207))) * den(40)
  T4sum(1:35,109) = T4sum(1:35,109) + Gcoeff * G3tensor(:,43)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)-M(17)+M(18)+M(19)-M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)-M(48)-M(51) &
    +M(53)+M(60)+M(63)+M(65)+M(66)+M(67)-M(68)-M(69)-M(70)-M(71)+M(72)+M(73)-M(74)+M(75)-M(76)+M(78)-M(81)-M(88)+M(90)-M(93) &
    +M(100))+c(6)*(-M(147)+M(177)+M(182)-M(190))) * den(40)
  T4sum(1:35,109) = T4sum(1:35,109) + Gcoeff * G3tensor(:,44)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(6)*(-M(147)-M(174)+M(177)+M(182) &
    +M(189)-M(190)-M(192)+M(207))) * den(40)
  T4sum(1:35,109) = T4sum(1:35,109) + Gcoeff * G3tensor(:,45)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)+M(17)-M(18)-M(19)+M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(41)+M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)-M(66)-M(67)-M(68)-M(69)-M(70)+M(71)+M(72)-M(73)+M(74)+M(75)+M(76)-M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(158)-M(185)+M(201)-M(245))) * den(40)
  T4sum(1:35,111) = T4sum(1:35,111) + Gcoeff * G3tensor(:,31)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)-M(41)-M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)+M(66)-M(67)-M(68)-M(69)-M(70)-M(71)+M(72)-M(73)+M(74)+M(75)+M(76)+M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(168)-M(187)+M(198)-M(231))) * den(40)
  T4sum(1:35,111) = T4sum(1:35,111) + Gcoeff * G3tensor(:,32)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(6)*(-M(158)+M(168)+M(185)-M(187) &
    +M(198)-M(201)-M(231)+M(245))) * den(40)
  T4sum(1:35,111) = T4sum(1:35,111) + Gcoeff * G3tensor(:,33)
  Gcoeff = (c(6)*(-M(147)+M(158)-M(168)-M(174)+M(177)+M(182)-M(185)+M(187)+M(189)-M(190)-M(192)-M(198)+M(201)+M(207)+M(231) &
    -M(245))) * den(71)
  T3sum(1:35,71) = T3sum(1:35,71) + Gcoeff * G3tensor(:,46)
  Gcoeff = (c(6)*(M(147)-M(153)-M(158)+M(160)+M(171)-M(177)-M(182)+M(184)+M(185)-M(186)-M(188)+M(190)+M(195)-M(201)-M(221) &
    +M(245))) * den(71)
  T3sum(1:35,71) = T3sum(1:35,71) + Gcoeff * G3tensor(:,47)
  Gcoeff = (c(6)*(M(153)-M(160)+M(168)-M(171)+M(174)-M(184)+M(186)-M(187)+M(188)-M(189)+M(192)-M(195)+M(198)-M(207)+M(221) &
    -M(231))) * den(71)
  T3sum(1:35,71) = T3sum(1:35,71) + Gcoeff * G3tensor(:,48)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(27)+M(28)+M(33)-M(34)-M(57)+M(69)+M(81)-M(84))+c(6)*(-M(132)+M(138)+M(203)-M(205)+M(224) &
    -M(226)-M(235)+M(241))) * den(72)
  T3sum(1:15,23) = T3sum(1:15,23) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(27)+M(28)+M(33)-M(34)-M(57)+M(69)+M(81)-M(84))+c(6)*(-M(149)+M(151)-M(165)+M(189)+M(207) &
    -M(208)+M(212)-M(218))) * den(72)
  T3sum(1:15,23) = T3sum(1:15,23) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(6)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(72)
  T3sum(1:15,23) = T3sum(1:15,23) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(6)*(-M(138)+M(141)-M(175)+M(205) &
    -M(206)+M(214)-M(224)+M(235))) * den(75)
  T3sum(1:15,20) = T3sum(1:15,20) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(6)*(-M(135)+M(149)-M(189)+M(199) &
    +M(204)-M(207)+M(218)-M(220))) * den(75)
  T3sum(1:15,20) = T3sum(1:15,20) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(6)*(-M(135)+M(138)-M(141)+M(149)+M(175)-M(189)+M(199)+M(204)-M(205)+M(206)-M(207)-M(214)+M(218)-M(220)+M(224) &
    -M(235))) * den(75)
  T3sum(1:15,20) = T3sum(1:15,20) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(76)+M(85)-M(88)+M(97))+c(6)*(-M(132)+M(135)-M(199)+M(203) &
    -M(204)+M(220)-M(226)+M(241))) * den(86)
  T3sum(1:15,17) = T3sum(1:15,17) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(76)+M(85)-M(88)+M(97))+c(6)*(-M(141)+M(151)-M(165)+M(175) &
    +M(206)-M(208)+M(212)-M(214))) * den(86)
  T3sum(1:15,17) = T3sum(1:15,17) + Gcoeff * G2tensor(:,6)
  Gcoeff = (c(6)*(M(132)-M(135)-M(141)+M(151)-M(165)+M(175)+M(199)-M(203)+M(204)+M(206)-M(208)+M(212)-M(214)-M(220)+M(226) &
    -M(241))) * den(86)
  T3sum(1:15,17) = T3sum(1:15,17) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)-M(11)+M(12)-M(19)+M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(42)+M(45)+M(52) &
    -M(54)+M(57)+M(64)-M(66)-M(69)-M(76)-M(77)-M(78)-M(79)+M(80)-M(81)-M(82)+M(83)+M(84)+M(85)+M(86)+M(87)-M(88)-M(91)-M(94) &
    +M(97))+c(6)*(M(135)-M(199)-M(204)+M(220))) * den(43)
  T4sum(1:35,141) = T4sum(1:35,141) + Gcoeff * G3tensor(:,1)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)-M(11)+M(12)-M(19)+M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(42)-M(45)-M(52) &
    +M(54)+M(57)+M(64)+M(66)-M(69)-M(76)+M(77)+M(78)+M(79)-M(80)-M(81)-M(82)-M(83)+M(84)+M(85)-M(86)+M(87)-M(88)+M(91)-M(94) &
    +M(97))+c(6)*(-M(141)+M(175)+M(206)-M(214))) * den(43)
  T4sum(1:35,141) = T4sum(1:35,141) + Gcoeff * G3tensor(:,2)
  Gcoeff = (c(5)*(-M(42)-M(45)-M(52)+M(54)+M(66)+M(77)+M(78)+M(79)-M(80)-M(83)-M(86)+M(91))+c(6)*(-M(135)-M(141)+M(175)+M(199) &
    +M(204)+M(206)-M(214)-M(220))) * den(43)
  T4sum(1:35,141) = T4sum(1:35,141) + Gcoeff * G3tensor(:,3)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(6)*(-M(167)-M(179)+M(181)+M(209) &
    +M(237)+M(239)-M(240)-M(247))) * den(77)
  T3sum(1:15,62) = T3sum(1:15,62) + Gcoeff * G2tensor(:,20)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(6)*(-M(133)+M(143)-M(187)+M(193) &
    +M(228)-M(231)+M(242)-M(244))) * den(77)
  T3sum(1:15,62) = T3sum(1:15,62) + Gcoeff * G2tensor(:,23)
  Gcoeff = (c(6)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(77)
  T3sum(1:15,62) = T3sum(1:15,62) + Gcoeff * G2tensor(:,26)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(133)-M(134)-M(143)+M(144) &
    +M(200)-M(202)-M(242)+M(244))) * den(129)
  T3sum(1:15,32) = T3sum(1:15,32) + Gcoeff * G2tensor(:,21)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(167)-M(168)+M(197)-M(198) &
    -M(209)+M(210)-M(239)+M(240))) * den(129)
  T3sum(1:15,32) = T3sum(1:15,32) + Gcoeff * G2tensor(:,24)
  Gcoeff = (c(6)*(-M(133)+M(134)+M(143)-M(144)+M(167)-M(168)+M(197)-M(198)-M(200)+M(202)-M(209)+M(210)-M(239)+M(240)+M(242) &
    -M(244))) * den(129)
  T3sum(1:15,32) = T3sum(1:15,32) + Gcoeff * G2tensor(:,27)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)+M(61)-M(69)+M(72)+M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(143)-M(242)+M(244))) * den(14)
  T4sum(1:35,162) = T4sum(1:35,162) + Gcoeff * G3tensor(:,7)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(167)-M(209)-M(239)+M(240))) * den(14)
  T4sum(1:35,162) = T4sum(1:35,162) + Gcoeff * G3tensor(:,8)
  Gcoeff = (c(5)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(6)*(-M(133)+M(143)+M(167) &
    -M(209)-M(239)+M(240)+M(242)-M(244))) * den(14)
  T4sum(1:35,162) = T4sum(1:35,162) + Gcoeff * G3tensor(:,9)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(134)+M(136)+M(179)-M(180) &
    +M(196)-M(202)-M(223)+M(247))) * den(81)
  T3sum(1:15,7) = T3sum(1:15,7) + Gcoeff * G2tensor(:,49)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(158)+M(160)+M(185)-M(186) &
    +M(195)-M(201)-M(221)+M(245))) * den(81)
  T3sum(1:15,7) = T3sum(1:15,7) + Gcoeff * G2tensor(:,50)
  Gcoeff = (c(6)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(81)
  T3sum(1:15,7) = T3sum(1:15,7) + Gcoeff * G2tensor(:,51)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)-M(17)+M(18)+M(19)-M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)+M(63)-M(65)-M(66)-M(67)+M(68)-M(69)-M(70)+M(71)+M(72)+M(73)+M(74)+M(75)-M(76)-M(78)-M(81)-M(88)-M(90)-M(93) &
    +M(100))+c(6)*(M(136)-M(180)+M(196)-M(223))) * den(40)
  T4sum(1:35,133) = T4sum(1:35,133) + Gcoeff * G3tensor(:,22)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)+M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)-M(66)-M(67)-M(68)-M(69)-M(70)+M(71)+M(72)+M(73)+M(74)+M(75)-M(76)-M(78)-M(81)-M(88)-M(90)-M(93) &
    +M(100))+c(6)*(M(160)-M(186)+M(195)-M(221))) * den(40)
  T4sum(1:35,133) = T4sum(1:35,133) + Gcoeff * G3tensor(:,23)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(-M(136)+M(160)+M(180)-M(186)+M(195) &
    -M(196)-M(221)+M(223))) * den(40)
  T4sum(1:35,133) = T4sum(1:35,133) + Gcoeff * G3tensor(:,24)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)+M(63)-M(65)-M(66)-M(67)+M(68)-M(69)-M(70)+M(71)+M(72)-M(73)+M(74)+M(75)+M(76)-M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(134)-M(179)+M(202)-M(247))) * den(40)
  T4sum(1:35,134) = T4sum(1:35,134) + Gcoeff * G3tensor(:,34)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)+M(17)-M(18)-M(19)+M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(41)+M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)-M(66)-M(67)-M(68)-M(69)-M(70)+M(71)+M(72)-M(73)+M(74)+M(75)+M(76)-M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(158)-M(185)+M(201)-M(245))) * den(40)
  T4sum(1:35,134) = T4sum(1:35,134) + Gcoeff * G3tensor(:,35)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(-M(134)+M(158)+M(179)-M(185)+M(201) &
    -M(202)-M(245)+M(247))) * den(40)
  T4sum(1:35,134) = T4sum(1:35,134) + Gcoeff * G3tensor(:,36)
  Gcoeff = (c(5)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(155)-M(157)-M(159) &
    +M(160)+M(195)-M(219)-M(243)+M(249))) * den(355)
  T3sum(1:15,57) = T3sum(1:15,57) + Gcoeff * G2tensor(:,73)
  Gcoeff = (c(5)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(131)-M(133)-M(135) &
    +M(136)+M(196)-M(220)-M(244)+M(250))) * den(355)
  T3sum(1:15,57) = T3sum(1:15,57) + Gcoeff * G2tensor(:,97)
  Gcoeff = (c(6)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(355)
  T3sum(1:15,57) = T3sum(1:15,57) + Gcoeff * G2tensor(:,121)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(37)+M(38)+M(39)-M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)+M(73)+M(83)+M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(155)-M(157)-M(243)+M(249))) * den(20)
  T4sum(1:35,25) = T4sum(1:35,25) + Gcoeff * G3tensor(:,59)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)+M(73)+M(83)+M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)+M(110)+M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(131)-M(133)-M(244)+M(250))) * den(20)
  T4sum(1:35,25) = T4sum(1:35,25) + Gcoeff * G3tensor(:,65)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(101)-M(103)-M(109)+M(115))+c(6)*(M(131)-M(133)-M(155)+M(157) &
    +M(243)-M(244)-M(249)+M(250))) * den(20)
  T4sum(1:35,25) = T4sum(1:35,25) + Gcoeff * G3tensor(:,71)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(37)+M(38)+M(39)-M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100)-M(101)+M(103)+M(109)-M(110)-M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(159)-M(160)-M(195)+M(219))) * den(20)
  T4sum(1:35,27) = T4sum(1:35,27) + Gcoeff * G3tensor(:,58)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(135)-M(136)-M(196)+M(220))) * den(20)
  T4sum(1:35,27) = T4sum(1:35,27) + Gcoeff * G3tensor(:,64)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(101)-M(103)-M(109)+M(115))+c(6)*(M(135)-M(136)-M(159)+M(160) &
    +M(195)-M(196)-M(219)+M(220))) * den(20)
  T4sum(1:35,27) = T4sum(1:35,27) + Gcoeff * G3tensor(:,70)
  Gcoeff = (c(5)*(-M(76)+M(85)-M(88)+M(97)-M(112)+M(116)-M(118)+M(122)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(156)+M(157)-M(158) &
    +M(159)-M(201)+M(219)-M(225)+M(243))) * den(358)
  T3sum(1:15,57) = T3sum(1:15,57) + Gcoeff * G2tensor(:,74)
  Gcoeff = (c(5)*(-M(76)+M(85)-M(88)+M(97)-M(112)+M(116)-M(118)+M(122)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(132)+M(133)-M(134) &
    +M(135)-M(202)+M(220)-M(226)+M(244))) * den(358)
  T3sum(1:15,57) = T3sum(1:15,57) + Gcoeff * G2tensor(:,98)
  Gcoeff = (c(6)*(-M(132)+M(133)-M(134)+M(135)+M(156)-M(157)+M(158)-M(159)+M(201)-M(202)-M(219)+M(220)+M(225)-M(226)-M(243) &
    +M(244))) * den(358)
  T3sum(1:15,57) = T3sum(1:15,57) + Gcoeff * G2tensor(:,122)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(37)-M(38)-M(39)+M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)+M(76)-M(85)+M(88)+M(95)+M(96)-M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(156)-M(159)-M(219)+M(225))) * den(22)
  T4sum(1:35,22) = T4sum(1:35,22) + Gcoeff * G3tensor(:,60)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)+M(76)-M(85)+M(88)+M(95)+M(96)-M(97)+M(102)-M(105)-M(111)+M(112)-M(116)+M(118)+M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(132)-M(135)-M(220)+M(226))) * den(22)
  T4sum(1:35,22) = T4sum(1:35,22) + Gcoeff * G3tensor(:,66)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(37)+M(38)+M(39)-M(40)+M(102)-M(105)-M(111)+M(121))+c(6)*(M(132)-M(135)-M(156)+M(159) &
    +M(219)-M(220)-M(225)+M(226))) * den(22)
  T4sum(1:35,22) = T4sum(1:35,22) + Gcoeff * G3tensor(:,72)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(37)-M(38)-M(39)+M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)-M(102)+M(105)+M(111)-M(112)+M(116)-M(118)-M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(157)-M(158)-M(201)+M(243))) * den(22)
  T4sum(1:35,24) = T4sum(1:35,24) + Gcoeff * G3tensor(:,57)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(134)-M(202)+M(244))) * den(22)
  T4sum(1:35,24) = T4sum(1:35,24) + Gcoeff * G3tensor(:,63)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(37)+M(38)+M(39)-M(40)+M(102)-M(105)-M(111)+M(121))+c(6)*(M(133)-M(134)-M(157)+M(158) &
    +M(201)-M(202)-M(243)+M(244))) * den(22)
  T4sum(1:35,24) = T4sum(1:35,24) + Gcoeff * G3tensor(:,69)
  Gcoeff = (c(5)*(M(73)-M(76)-M(88)+M(100)+M(110)-M(112)-M(118)+M(124)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(155)-M(156)-M(158) &
    +M(160)+M(195)-M(201)-M(225)+M(249))) * den(361)
  T3sum(1:15,57) = T3sum(1:15,57) + Gcoeff * G2tensor(:,75)
  Gcoeff = (c(5)*(M(73)-M(76)-M(88)+M(100)+M(110)-M(112)-M(118)+M(124)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(131)-M(132)-M(134) &
    +M(136)+M(196)-M(202)-M(226)+M(250))) * den(361)
  T3sum(1:15,57) = T3sum(1:15,57) + Gcoeff * G2tensor(:,99)
  Gcoeff = (c(6)*(M(131)-M(132)-M(134)+M(136)-M(155)+M(156)+M(158)-M(160)-M(195)+M(196)+M(201)-M(202)+M(225)-M(226)-M(249) &
    +M(250))) * den(361)
  T3sum(1:15,57) = T3sum(1:15,57) + Gcoeff * G2tensor(:,123)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)+M(49)-M(52)+M(61) &
    -M(64)-M(73)+M(76)-M(86)-M(87)+M(88)+M(98)+M(99)-M(100)-M(104)+M(106)-M(110)+M(112)+M(117)+M(118)-M(123)-M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(158)-M(160)-M(195)+M(201))) * den(26)
  T4sum(1:35,19) = T4sum(1:35,19) + Gcoeff * G3tensor(:,61)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)+M(61) &
    -M(64)-M(73)+M(76)-M(86)-M(87)+M(88)+M(98)+M(99)-M(100)+M(104)-M(106)-M(110)+M(112)-M(117)+M(118)+M(123)-M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(134)-M(136)-M(196)+M(202))) * den(26)
  T4sum(1:35,19) = T4sum(1:35,19) + Gcoeff * G3tensor(:,67)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(37)-M(38)-M(39)+M(40)+M(104)-M(106)-M(117)+M(123))+c(6)*(M(134)-M(136)-M(158)+M(160) &
    +M(195)-M(196)-M(201)+M(202))) * den(26)
  T4sum(1:35,19) = T4sum(1:35,19) + Gcoeff * G3tensor(:,73)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)+M(49)-M(52)+M(61) &
    -M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100)-M(104)+M(106)+M(110)-M(112)+M(117)-M(118)-M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(155)-M(156)-M(225)+M(249))) * den(26)
  T4sum(1:35,21) = T4sum(1:35,21) + Gcoeff * G3tensor(:,56)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)+M(61) &
    -M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(131)-M(132)-M(226)+M(250))) * den(26)
  T4sum(1:35,21) = T4sum(1:35,21) + Gcoeff * G3tensor(:,62)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(37)-M(38)-M(39)+M(40)+M(104)-M(106)-M(117)+M(123))+c(6)*(M(131)-M(132)-M(155)+M(156) &
    +M(225)-M(226)-M(249)+M(250))) * den(26)
  T4sum(1:35,21) = T4sum(1:35,21) + Gcoeff * G3tensor(:,68)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(9)-M(11)-M(13)-M(16)-M(17)-M(19)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(48)+M(51)+M(52)+M(53)+M(54)+M(55)+M(60)+M(63)+M(64)+M(65)+M(71)+M(72)+M(73)+M(74)+M(75)+M(77)+M(86)+M(87)+M(89) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(160)+M(195)))
  T5sum(1:70,113) = T5sum(1:70,113) + Gcoeff * G4tensor(:,16)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(48)+M(51)+M(52)+M(60)+M(63)+M(64)+M(68)+M(71)+M(72)+M(73)+M(74)+M(75)+M(80)+M(86)+M(87)+M(92) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(136)+M(196)))
  T5sum(1:70,113) = T5sum(1:70,113) + Gcoeff * G4tensor(:,22)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)-M(5)+M(6)+M(7)-M(8)+M(13)-M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)-M(101)-M(102)+M(103)-M(104)+M(105)+M(106)+M(109)+M(111)-M(115)+M(117)-M(121) &
    -M(123))+c(6)*(M(136)-M(160)-M(195)+M(196)))
  T5sum(1:70,113) = T5sum(1:70,113) + Gcoeff * G4tensor(:,28)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(9)-M(11)-M(13)-M(16)-M(18)-M(19)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(37) &
    -M(40)+M(48)+M(49)+M(51)+M(53)+M(54)+M(55)+M(60)+M(61)+M(63)+M(65)+M(71)+M(72)+M(74)+M(75)+M(76)+M(77)+M(88)+M(89)+M(98)+M(99) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(158)+M(201)))
  T5sum(1:70,114) = T5sum(1:70,114) + Gcoeff * G4tensor(:,14)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(48)+M(49)+M(51)+M(60)+M(61)+M(63)+M(68)+M(71)+M(72)+M(74)+M(75)+M(76)+M(80)+M(88)+M(92)+M(98)+M(99) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(134)+M(202)))
  T5sum(1:70,114) = T5sum(1:70,114) + Gcoeff * G4tensor(:,20)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(5)+M(6)-M(7)+M(8)+M(13)-M(14)+M(19)-M(20)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)-M(101)-M(102)+M(103)+M(104)+M(105)-M(106)+M(109)+M(111)-M(115)-M(117)-M(121) &
    +M(123))+c(6)*(M(134)-M(158)-M(201)+M(202)))
  T5sum(1:70,114) = T5sum(1:70,114) + Gcoeff * G4tensor(:,26)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(10)-M(11)-M(13)-M(15)-M(17)-M(20)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(45)+M(51)+M(52)+M(53)+M(54)+M(55)+M(57)+M(63)+M(64)+M(65)+M(74)+M(75)+M(77)+M(83)+M(84)+M(85)+M(86)+M(87)+M(89)+M(97) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(159)+M(219)))
  T5sum(1:70,117) = T5sum(1:70,117) + Gcoeff * G4tensor(:,17)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(45)+M(51)+M(52)+M(57)+M(63)+M(64)+M(68)+M(74)+M(75)+M(80)+M(83)+M(84)+M(85)+M(86)+M(87)+M(92)+M(97) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(135)+M(220)))
  T5sum(1:70,117) = T5sum(1:70,117) + Gcoeff * G4tensor(:,23)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(5)-M(6)+M(7)-M(8)+M(13)-M(14)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)+M(101)-M(102)-M(103)-M(104)+M(105)+M(106)-M(109)+M(111)+M(115)+M(117)-M(121) &
    -M(123))+c(6)*(M(135)-M(159)-M(219)+M(220)))
  T5sum(1:70,117) = T5sum(1:70,117) + Gcoeff * G4tensor(:,29)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(10)-M(12)-M(14)-M(15)-M(17)-M(20)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(38) &
    -M(39)+M(45)+M(46)+M(52)+M(53)+M(54)+M(55)+M(57)+M(58)+M(64)+M(65)+M(76)+M(77)+M(83)+M(84)+M(86)+M(87)+M(88)+M(89)+M(95)+M(96) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(156)+M(225)))
  T5sum(1:70,118) = T5sum(1:70,118) + Gcoeff * G4tensor(:,12)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(45)+M(46)+M(52)+M(57)+M(58)+M(64)+M(68)+M(76)+M(80)+M(83)+M(84)+M(86)+M(87)+M(88)+M(92)+M(95)+M(96) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(132)+M(226)))
  T5sum(1:70,118) = T5sum(1:70,118) + Gcoeff * G4tensor(:,18)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(5)-M(6)+M(7)-M(8)-M(13)+M(14)-M(19)+M(20)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)+M(101)+M(102)-M(103)-M(104)-M(105)+M(106)-M(109)-M(111)+M(115)+M(117)+M(121) &
    -M(123))+c(6)*(M(132)-M(156)-M(225)+M(226)))
  T5sum(1:70,118) = T5sum(1:70,118) + Gcoeff * G4tensor(:,24)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(9)-M(12)-M(14)-M(16)-M(18)-M(19)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(46)+M(48)+M(49)+M(53)+M(54)+M(55)+M(58)+M(60)+M(61)+M(65)+M(71)+M(72)+M(77)+M(85)+M(89)+M(95)+M(96)+M(97)+M(98)+M(99) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(157)+M(243)))
  T5sum(1:70,119) = T5sum(1:70,119) + Gcoeff * G4tensor(:,15)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(46)+M(48)+M(49)+M(58)+M(60)+M(61)+M(68)+M(71)+M(72)+M(80)+M(85)+M(92)+M(95)+M(96)+M(97)+M(98)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(133)+M(244)))
  T5sum(1:70,119) = T5sum(1:70,119) + Gcoeff * G4tensor(:,21)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(5)+M(6)-M(7)+M(8)-M(13)+M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)-M(101)+M(102)+M(103)+M(104)-M(105)-M(106)+M(109)-M(111)-M(115)-M(117)+M(121) &
    +M(123))+c(6)*(M(133)-M(157)-M(243)+M(244)))
  T5sum(1:70,119) = T5sum(1:70,119) + Gcoeff * G4tensor(:,27)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(10)-M(12)-M(14)-M(15)-M(18)-M(20)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(45)+M(46)+M(49)+M(53)+M(54)+M(55)+M(57)+M(58)+M(61)+M(65)+M(73)+M(77)+M(83)+M(84)+M(89)+M(95)+M(96)+M(98)+M(99) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(155)+M(249)))
  T5sum(1:70,120) = T5sum(1:70,120) + Gcoeff * G4tensor(:,13)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(45)+M(46)+M(49)+M(57)+M(58)+M(61)+M(68)+M(73)+M(80)+M(83)+M(84)+M(92)+M(95)+M(96)+M(98)+M(99) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(131)+M(250)))
  T5sum(1:70,120) = T5sum(1:70,120) + Gcoeff * G4tensor(:,19)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)+M(5)-M(6)-M(7)+M(8)-M(13)+M(14)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)+M(101)+M(102)-M(103)+M(104)-M(105)-M(106)-M(109)-M(111)+M(115)-M(117)+M(121) &
    +M(123))+c(6)*(M(131)-M(155)-M(249)+M(250)))
  T5sum(1:70,120) = T5sum(1:70,120) + Gcoeff * G4tensor(:,25)
  Gcoeff = (c(6)*(M(131)-M(132)-M(155)+M(156)-M(179)+M(180)+M(185)-M(186)-M(221)+M(223)+M(225)-M(226)+M(245)-M(247)-M(249) &
    +M(250))) * den(95)
  T3sum(1:35,33) = T3sum(1:35,33) + Gcoeff * G3tensor(:,74)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(95)
  T3sum(1:35,33) = T3sum(1:35,33) + Gcoeff * G3tensor(:,75)
  Gcoeff = (c(6)*(-M(137)+M(138)+M(155)-M(156)-M(161)+M(162)+M(179)-M(180)+M(222)-M(223)+M(224)-M(225)-M(246)+M(247)-M(248) &
    +M(249))) * den(95)
  T3sum(1:35,33) = T3sum(1:35,33) + Gcoeff * G3tensor(:,76)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(132)-M(156)-M(180) &
    +M(186)+M(221)-M(223)-M(225)+M(226))) * den(92)
  T4sum(1:70,164) = T4sum(1:70,164) + Gcoeff * G4tensor(:,30)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(6)*(-M(132)+M(138)+M(162) &
    -M(186)-M(221)+M(222)+M(224)-M(226))) * den(92)
  T4sum(1:70,164) = T4sum(1:70,164) + Gcoeff * G4tensor(:,32)
  Gcoeff = (c(5)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(6)*(-M(138)+M(156)-M(162) &
    +M(180)-M(222)+M(223)-M(224)+M(225))) * den(92)
  T4sum(1:70,164) = T4sum(1:70,164) + Gcoeff * G4tensor(:,34)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(131)-M(155)-M(179) &
    +M(185)+M(245)-M(247)-M(249)+M(250))) * den(92)
  T4sum(1:70,165) = T4sum(1:70,165) + Gcoeff * G4tensor(:,31)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(6)*(-M(131)+M(137)+M(161) &
    -M(185)-M(245)+M(246)+M(248)-M(250))) * den(92)
  T4sum(1:70,165) = T4sum(1:70,165) + Gcoeff * G4tensor(:,33)
  Gcoeff = (c(5)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(6)*(-M(137)+M(155)-M(161) &
    +M(179)-M(246)+M(247)-M(248)+M(249))) * den(92)
  T4sum(1:70,165) = T4sum(1:70,165) + Gcoeff * G4tensor(:,35)
  Gcoeff = (c(6)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(1342)
  T3sum(1:35,7) = T3sum(1:35,7) + Gcoeff * G3tensor(:,49)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(-M(136)+M(160)+M(180)-M(186)+M(195) &
    -M(196)-M(221)+M(223))) * den(42)
  T4sum(1:70,133) = T4sum(1:70,133) + Gcoeff * G4tensor(:,2)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(-M(134)+M(158)+M(179)-M(185)+M(201) &
    -M(202)-M(245)+M(247))) * den(42)
  T4sum(1:70,134) = T4sum(1:70,134) + Gcoeff * G4tensor(:,5)
  Gcoeff = (c(6)*(-M(147)+M(158)-M(168)-M(174)+M(177)+M(182)-M(185)+M(187)+M(189)-M(190)-M(192)-M(198)+M(201)+M(207)+M(231) &
    -M(245))) * den(464)
  T3sum(1:35,71) = T3sum(1:35,71) + Gcoeff * G3tensor(:,50)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(6)*(-M(147)-M(174)+M(177)+M(182) &
    +M(189)-M(190)-M(192)+M(207))) * den(48)
  T4sum(1:70,109) = T4sum(1:70,109) + Gcoeff * G4tensor(:,8)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(6)*(-M(158)+M(168)+M(185)-M(187) &
    +M(198)-M(201)-M(231)+M(245))) * den(48)
  T4sum(1:70,111) = T4sum(1:70,111) + Gcoeff * G4tensor(:,6)
  Gcoeff = (c(6)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(584)
  T3sum(1:35,72) = T3sum(1:35,72) + Gcoeff * G3tensor(:,51)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(150)+M(153)-M(171)+M(183)-M(184) &
    +M(188)-M(194)+M(213))) * den(69)
  T4sum(1:70,73) = T4sum(1:70,73) + Gcoeff * G4tensor(:,9)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(134)+M(144)+M(179)-M(181)+M(200) &
    -M(202)-M(237)+M(247))) * den(69)
  T4sum(1:70,75) = T4sum(1:70,75) + Gcoeff * G4tensor(:,7)
  Gcoeff = (c(6)*(M(153)-M(160)+M(168)-M(171)+M(174)-M(184)+M(186)-M(187)+M(188)-M(189)+M(192)-M(195)+M(198)-M(207)+M(221) &
    -M(231))) * den(466)
  T3sum(1:35,71) = T3sum(1:35,71) + Gcoeff * G3tensor(:,52)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(153)-M(168)+M(171)+M(184) &
    +M(187)-M(188)-M(198)+M(231))) * den(49)
  T4sum(1:70,106) = T4sum(1:70,106) + Gcoeff * G4tensor(:,10)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(160)+M(174)+M(186)-M(189) &
    +M(192)-M(195)-M(207)+M(221))) * den(49)
  T4sum(1:70,108) = T4sum(1:70,108) + Gcoeff * G4tensor(:,3)
  Gcoeff = (c(6)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(586)
  T3sum(1:35,72) = T3sum(1:35,72) + Gcoeff * G3tensor(:,53)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(144)+M(147)-M(177)+M(181)-M(182) &
    +M(190)-M(200)+M(237))) * den(70)
  T4sum(1:70,70) = T4sum(1:70,70) + Gcoeff * G4tensor(:,11)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(136)+M(150)+M(180)-M(183)+M(194) &
    -M(196)-M(213)+M(223))) * den(70)
  T4sum(1:70,72) = T4sum(1:70,72) + Gcoeff * G4tensor(:,4)
  Gcoeff = (c(6)*(-M(147)+M(153)+M(158)-M(160)-M(171)+M(177)+M(182)-M(184)-M(185)+M(186)+M(188)-M(190)-M(195)+M(201)+M(221) &
    -M(245))) * den(230)
  T3sum(1:35,71) = T3sum(1:35,71) + Gcoeff * G3tensor(:,54)
  Gcoeff = (c(6)*(M(134)-M(136)-M(147)+M(153)-M(171)+M(177)-M(179)+M(180)+M(182)-M(184)+M(188)-M(190)-M(196)+M(202)+M(223) &
    -M(247))) * den(588)
  T3sum(1:35,72) = T3sum(1:35,72) + Gcoeff * G3tensor(:,55)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(147)+M(153)-M(171)+M(177) &
    +M(182)-M(184)+M(188)-M(190))) * den(81)
  T4sum(1:70,48) = T4sum(1:70,48) + Gcoeff * G4tensor(:,1)
  Gcoeff = (c(4)*(M(3)-M(4)-M(5)+M(6)-M(13)+M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)+M(63)-M(65)-M(66)-M(67)+M(68)+M(69)-M(70)+M(71)-M(72)-M(73)+M(74)+M(75)+M(76)-M(78)+M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(144)-M(181)+M(200)-M(237))) * den(40)
  T5sum(1:126,153) = T5sum(1:126,153) + Gcoeff * G5tensor(:,4)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)+M(17)-M(18)-M(19)+M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)-M(63)-M(65)-M(66)-M(67)+M(68)+M(69)+M(70)+M(71)-M(72)-M(73)+M(74)-M(75)+M(76)-M(78)+M(81)+M(88)-M(90)+M(93) &
    -M(100))+c(6)*(M(147)-M(177)-M(182)+M(190))) * den(40)
  T5sum(1:126,154) = T5sum(1:126,154) + Gcoeff * G5tensor(:,1)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)-M(63)-M(65)-M(66)-M(67)+M(68)-M(69)+M(70)+M(71)+M(72)+M(73)+M(74)-M(75)-M(76)-M(78)-M(81)-M(88)-M(90)+M(93) &
    +M(100))+c(6)*(M(150)-M(183)+M(194)-M(213))) * den(40)
  T5sum(1:126,155) = T5sum(1:126,155) + Gcoeff * G5tensor(:,3)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)-M(63)-M(65)-M(66)-M(67)+M(68)+M(69)+M(70)+M(71)-M(72)+M(73)+M(74)-M(75)-M(76)-M(78)+M(81)-M(88)-M(90)+M(93) &
    +M(100))+c(6)*(M(153)-M(171)-M(184)+M(188))) * den(40)
  T5sum(1:126,156) = T5sum(1:126,156) + Gcoeff * G5tensor(:,2)

end subroutine vamp_15

end module ol_vamp_15_ppjjjj_gggggg_1_/**/REALKIND
