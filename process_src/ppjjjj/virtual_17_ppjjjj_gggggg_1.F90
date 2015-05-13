
module ol_vamp_17_ppjjjj_gggggg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_17(M)
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
  complex(REALKIND), dimension(4,1,4,109) :: G0
  complex(REALKIND), dimension(4,5,4,169) :: G1
  complex(REALKIND), dimension(4,15,4,98) :: G2
  complex(REALKIND), dimension(4,35,4,39) :: G3
  complex(REALKIND), dimension(4,70,4,4) :: G4
  complex(REALKIND), dimension(5,90) :: G1tensor
  complex(REALKIND), dimension(15,132) :: G2tensor
  complex(REALKIND), dimension(35,73) :: G3tensor
  complex(REALKIND), dimension(70,35) :: G4tensor
  complex(REALKIND), dimension(126,4) :: G5tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,95),G0(:,:,:,2))
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
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,95),wf(:,0),G0(:,:,:,9))
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
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,95),G0(:,:,:,16))
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
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,66),G0(:,:,:,23))
  call loop_GGG_G_12(G0(:,:,:,23),wf(:,-5),wf(:,-3),G0(:,:,:,24))
  call check_last_UV_W(l_switch,G0(:,:,:,24),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,28))
  call loop_GGG_G_12(G0(:,:,:,23),wf(:,-3),wf(:,-5),G0(:,:,:,25))
  call check_last_UV_W(l_switch,G0(:,:,:,25),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,29))
  call loop_GGG_G_23(G0(:,:,:,23),wf(:,-5),wf(:,-3),G0(:,:,:,26))
  call check_last_UV_W(l_switch,G0(:,:,:,26),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,30))
  call loop_GGG_G_12(G0(:,:,:,23),wf(:,-5),wf(:,-1),G0(:,:,:,27))
  call check_last_UV_W(l_switch,G0(:,:,:,27),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,31))
  call loop_GGG_G_12(G0(:,:,:,23),wf(:,-1),wf(:,-5),G0(:,:,:,28))
  call check_last_UV_W(l_switch,G0(:,:,:,28),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,32))
  call loop_GGG_G_23(G0(:,:,:,23),wf(:,-5),wf(:,-1),G0(:,:,:,29))
  call check_last_UV_W(l_switch,G0(:,:,:,29),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,33))
  call loop_UV_W(G0(:,:,:,23),Q(:,21),wf(:,-5),Q(:,32),G1(:,:,:,13))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,13),wf(:,-3),wf(:,-1),G1tensor(:,34))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,13),wf(:,-1),wf(:,-3),G1tensor(:,35))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,13),wf(:,-3),wf(:,-1),G1tensor(:,36))
  call check_last_UV_W(l_switch,G1(:,:,:,13),Q(:,53),wf(:,91),Q(:,10),G2tensor(:,10))
  call loop_UV_W(G0(:,:,:,23),Q(:,21),wf(:,99),Q(:,34),G1(:,:,:,14))
  call check_last_UV_W(l_switch,G1(:,:,:,14),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,11))
  call loop_UV_W(G0(:,:,:,23),Q(:,21),wf(:,79),Q(:,40),G1(:,:,:,15))
  call check_last_UV_W(l_switch,G1(:,:,:,15),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,12))
  call loop_UV_W(G0(:,:,:,23),Q(:,21),wf(:,-3),Q(:,8),G1(:,:,:,16))
  call loop_UV_W(G1(:,:,:,16),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,4))
  call check_last_UV_W(l_switch,G2(:,:,:,4),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,4))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,66),wf(:,0),G0(:,:,:,30))
  call loop_GGG_G_12(G0(:,:,:,30),wf(:,-5),wf(:,-3),G0(:,:,:,31))
  call check_last_UV_W(l_switch,G0(:,:,:,31),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,37))
  call loop_GGG_G_12(G0(:,:,:,30),wf(:,-3),wf(:,-5),G0(:,:,:,32))
  call check_last_UV_W(l_switch,G0(:,:,:,32),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,38))
  call loop_GGG_G_23(G0(:,:,:,30),wf(:,-5),wf(:,-3),G0(:,:,:,33))
  call check_last_UV_W(l_switch,G0(:,:,:,33),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,39))
  call loop_GGG_G_12(G0(:,:,:,30),wf(:,-5),wf(:,-1),G0(:,:,:,34))
  call check_last_UV_W(l_switch,G0(:,:,:,34),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,40))
  call loop_GGG_G_12(G0(:,:,:,30),wf(:,-1),wf(:,-5),G0(:,:,:,35))
  call check_last_UV_W(l_switch,G0(:,:,:,35),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,41))
  call loop_GGG_G_23(G0(:,:,:,30),wf(:,-5),wf(:,-1),G0(:,:,:,36))
  call check_last_UV_W(l_switch,G0(:,:,:,36),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,42))
  call loop_UV_W(G0(:,:,:,30),Q(:,21),wf(:,-5),Q(:,32),G1(:,:,:,17))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,17),wf(:,-3),wf(:,-1),G1tensor(:,43))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,17),wf(:,-1),wf(:,-3),G1tensor(:,44))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,17),wf(:,-3),wf(:,-1),G1tensor(:,45))
  call check_last_UV_W(l_switch,G1(:,:,:,17),Q(:,53),wf(:,91),Q(:,10),G2tensor(:,13))
  call loop_UV_W(G0(:,:,:,30),Q(:,21),wf(:,99),Q(:,34),G1(:,:,:,18))
  call check_last_UV_W(l_switch,G1(:,:,:,18),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,14))
  call loop_UV_W(G0(:,:,:,30),Q(:,21),wf(:,79),Q(:,40),G1(:,:,:,19))
  call check_last_UV_W(l_switch,G1(:,:,:,19),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,15))
  call loop_UV_W(G0(:,:,:,30),Q(:,21),wf(:,-3),Q(:,8),G1(:,:,:,20))
  call loop_UV_W(G1(:,:,:,20),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,5))
  call check_last_UV_W(l_switch,G2(:,:,:,5),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,5))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,66),G0(:,:,:,37))
  call loop_GGG_G_12(G0(:,:,:,37),wf(:,-5),wf(:,-3),G0(:,:,:,38))
  call check_last_UV_W(l_switch,G0(:,:,:,38),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,46))
  call loop_GGG_G_12(G0(:,:,:,37),wf(:,-3),wf(:,-5),G0(:,:,:,39))
  call check_last_UV_W(l_switch,G0(:,:,:,39),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,47))
  call loop_GGG_G_23(G0(:,:,:,37),wf(:,-5),wf(:,-3),G0(:,:,:,40))
  call check_last_UV_W(l_switch,G0(:,:,:,40),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,48))
  call loop_GGG_G_12(G0(:,:,:,37),wf(:,-5),wf(:,-1),G0(:,:,:,41))
  call check_last_UV_W(l_switch,G0(:,:,:,41),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,49))
  call loop_GGG_G_12(G0(:,:,:,37),wf(:,-1),wf(:,-5),G0(:,:,:,42))
  call check_last_UV_W(l_switch,G0(:,:,:,42),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,50))
  call loop_GGG_G_23(G0(:,:,:,37),wf(:,-5),wf(:,-1),G0(:,:,:,43))
  call check_last_UV_W(l_switch,G0(:,:,:,43),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,51))
  call loop_UV_W(G0(:,:,:,37),Q(:,21),wf(:,-5),Q(:,32),G1(:,:,:,21))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,21),wf(:,-3),wf(:,-1),G1tensor(:,52))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,21),wf(:,-1),wf(:,-3),G1tensor(:,53))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,21),wf(:,-3),wf(:,-1),G1tensor(:,54))
  call check_last_UV_W(l_switch,G1(:,:,:,21),Q(:,53),wf(:,91),Q(:,10),G2tensor(:,16))
  call loop_UV_W(G0(:,:,:,37),Q(:,21),wf(:,99),Q(:,34),G1(:,:,:,22))
  call check_last_UV_W(l_switch,G1(:,:,:,22),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,17))
  call loop_UV_W(G0(:,:,:,37),Q(:,21),wf(:,79),Q(:,40),G1(:,:,:,23))
  call check_last_UV_W(l_switch,G1(:,:,:,23),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,18))
  call loop_UV_W(G0(:,:,:,37),Q(:,21),wf(:,-3),Q(:,8),G1(:,:,:,24))
  call loop_UV_W(G1(:,:,:,24),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,6))
  call check_last_UV_W(l_switch,G2(:,:,:,6),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,6))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,91),Q(:,10),G1(:,:,:,25))
  call loop_GGG_G_12(G1(:,:,:,25),wf(:,-5),wf(:,-4),G1(:,:,:,26))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,26),wf(:,-2),wf(:,0),G1tensor(:,55))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,26),wf(:,0),wf(:,-2),G1tensor(:,56))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,26),wf(:,-2),wf(:,0),G1tensor(:,57))
  call check_last_UV_W(l_switch,G1(:,:,:,26),Q(:,58),wf(:,90),Q(:,5),G2tensor(:,19))
  call loop_GGG_G_12(G1(:,:,:,25),wf(:,-4),wf(:,-5),G1(:,:,:,27))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,27),wf(:,-2),wf(:,0),G1tensor(:,58))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,27),wf(:,0),wf(:,-2),G1tensor(:,59))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,27),wf(:,-2),wf(:,0),G1tensor(:,60))
  call check_last_UV_W(l_switch,G1(:,:,:,27),Q(:,58),wf(:,90),Q(:,5),G2tensor(:,20))
  call loop_GGG_G_23(G1(:,:,:,25),wf(:,-5),wf(:,-4),G1(:,:,:,28))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,28),wf(:,-2),wf(:,0),G1tensor(:,61))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,28),wf(:,0),wf(:,-2),G1tensor(:,62))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,28),wf(:,-2),wf(:,0),G1tensor(:,63))
  call check_last_UV_W(l_switch,G1(:,:,:,28),Q(:,58),wf(:,90),Q(:,5),G2tensor(:,21))
  call loop_GGG_G_12(G1(:,:,:,25),wf(:,-5),wf(:,109),G1(:,:,:,29))
  call check_last_UV_W(l_switch,G1(:,:,:,29),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,22))
  call loop_GGG_G_12(G1(:,:,:,25),wf(:,109),wf(:,-5),G1(:,:,:,30))
  call check_last_UV_W(l_switch,G1(:,:,:,30),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,23))
  call loop_GGG_G_23(G1(:,:,:,25),wf(:,-5),wf(:,109),G1(:,:,:,31))
  call check_last_UV_W(l_switch,G1(:,:,:,31),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,24))
  call loop_GGG_G_12(G1(:,:,:,25),wf(:,-5),wf(:,66),G1(:,:,:,32))
  call check_last_UV_W(l_switch,G1(:,:,:,32),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,25))
  call loop_GGG_G_12(G1(:,:,:,25),wf(:,66),wf(:,-5),G1(:,:,:,33))
  call check_last_UV_W(l_switch,G1(:,:,:,33),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,26))
  call loop_GGG_G_23(G1(:,:,:,25),wf(:,-5),wf(:,66),G1(:,:,:,34))
  call check_last_UV_W(l_switch,G1(:,:,:,34),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,27))
  call loop_GGG_G_12(G1(:,:,:,25),wf(:,-4),wf(:,113),G1(:,:,:,35))
  call check_last_UV_W(l_switch,G1(:,:,:,35),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,28))
  call loop_GGG_G_12(G1(:,:,:,25),wf(:,113),wf(:,-4),G1(:,:,:,36))
  call check_last_UV_W(l_switch,G1(:,:,:,36),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,29))
  call loop_GGG_G_23(G1(:,:,:,25),wf(:,-4),wf(:,113),G1(:,:,:,37))
  call check_last_UV_W(l_switch,G1(:,:,:,37),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,30))
  call loop_GGG_G_12(G1(:,:,:,25),wf(:,-4),wf(:,70),G1(:,:,:,38))
  call check_last_UV_W(l_switch,G1(:,:,:,38),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,31))
  call loop_GGG_G_12(G1(:,:,:,25),wf(:,70),wf(:,-4),G1(:,:,:,39))
  call check_last_UV_W(l_switch,G1(:,:,:,39),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,32))
  call loop_GGG_G_23(G1(:,:,:,25),wf(:,-4),wf(:,70),G1(:,:,:,40))
  call check_last_UV_W(l_switch,G1(:,:,:,40),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,33))
  call loop_UV_W(G1(:,:,:,25),Q(:,10),wf(:,-2),Q(:,4),G2(:,:,:,7))
  call loop_GGG_G_12(G2(:,:,:,7),wf(:,-5),wf(:,-4),G2(:,:,:,8))
  call check_last_UV_W(l_switch,G2(:,:,:,8),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,7))
  call loop_GGG_G_12(G2(:,:,:,7),wf(:,-4),wf(:,-5),G2(:,:,:,9))
  call check_last_UV_W(l_switch,G2(:,:,:,9),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,8))
  call loop_GGG_G_23(G2(:,:,:,7),wf(:,-5),wf(:,-4),G2(:,:,:,10))
  call check_last_UV_W(l_switch,G2(:,:,:,10),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,9))
  call loop_UV_W(G2(:,:,:,7),Q(:,14),wf(:,84),Q(:,48),G3(:,:,:,1))
  call check_last_UV_W(l_switch,G3(:,:,:,1),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,1))
  call loop_UV_W(G2(:,:,:,7),Q(:,14),wf(:,-5),Q(:,32),G3(:,:,:,2))
  call loop_UV_W(G3(:,:,:,2),Q(:,46),wf(:,-4),Q(:,16),G4(:,:,:,1))
  call check_last_UV_W(l_switch,G4(:,:,:,1),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,1))
  call loop_UV_W(G2(:,:,:,7),Q(:,14),wf(:,-4),Q(:,16),G3(:,:,:,3))
  call loop_UV_W(G3(:,:,:,3),Q(:,30),wf(:,-5),Q(:,32),G4(:,:,:,2))
  call check_last_UV_W(l_switch,G4(:,:,:,2),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,2))
  call loop_GGG_G_12(G1(:,:,:,25),wf(:,-2),wf(:,84),G1(:,:,:,41))
  call check_last_UV_W(l_switch,G1(:,:,:,41),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,34))
  call loop_GGG_G_12(G1(:,:,:,25),wf(:,84),wf(:,-2),G1(:,:,:,42))
  call check_last_UV_W(l_switch,G1(:,:,:,42),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,35))
  call loop_GGG_G_23(G1(:,:,:,25),wf(:,-2),wf(:,84),G1(:,:,:,43))
  call check_last_UV_W(l_switch,G1(:,:,:,43),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,36))
  call loop_GGG_G_12(G1(:,:,:,25),wf(:,-5),wf(:,-2),G1(:,:,:,44))
  call loop_UV_W(G1(:,:,:,44),Q(:,46),wf(:,-4),Q(:,16),G2(:,:,:,11))
  call check_last_UV_W(l_switch,G2(:,:,:,11),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,10))
  call loop_GGG_G_12(G1(:,:,:,25),wf(:,-2),wf(:,-5),G1(:,:,:,45))
  call loop_UV_W(G1(:,:,:,45),Q(:,46),wf(:,-4),Q(:,16),G2(:,:,:,12))
  call check_last_UV_W(l_switch,G2(:,:,:,12),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,11))
  call loop_GGG_G_23(G1(:,:,:,25),wf(:,-5),wf(:,-2),G1(:,:,:,46))
  call loop_UV_W(G1(:,:,:,46),Q(:,46),wf(:,-4),Q(:,16),G2(:,:,:,13))
  call check_last_UV_W(l_switch,G2(:,:,:,13),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,12))
  call loop_UV_W(G1(:,:,:,25),Q(:,10),wf(:,-4),Q(:,16),G2(:,:,:,14))
  call loop_GGG_G_12(G2(:,:,:,14),wf(:,-5),wf(:,-2),G2(:,:,:,15))
  call check_last_UV_W(l_switch,G2(:,:,:,15),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,13))
  call loop_GGG_G_12(G2(:,:,:,14),wf(:,-2),wf(:,-5),G2(:,:,:,16))
  call check_last_UV_W(l_switch,G2(:,:,:,16),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,14))
  call loop_GGG_G_23(G2(:,:,:,14),wf(:,-5),wf(:,-2),G2(:,:,:,17))
  call check_last_UV_W(l_switch,G2(:,:,:,17),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,15))
  call loop_GGG_G_12(G2(:,:,:,14),wf(:,-5),wf(:,0),G2(:,:,:,18))
  call check_last_UV_W(l_switch,G2(:,:,:,18),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,16))
  call loop_GGG_G_12(G2(:,:,:,14),wf(:,0),wf(:,-5),G2(:,:,:,19))
  call check_last_UV_W(l_switch,G2(:,:,:,19),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,17))
  call loop_GGG_G_23(G2(:,:,:,14),wf(:,-5),wf(:,0),G2(:,:,:,20))
  call check_last_UV_W(l_switch,G2(:,:,:,20),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,18))
  call loop_UV_W(G2(:,:,:,14),Q(:,26),wf(:,-5),Q(:,32),G3(:,:,:,4))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,4),wf(:,-2),wf(:,0),G3tensor(:,19))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,4),wf(:,0),wf(:,-2),G3tensor(:,20))
  call check_last_GGG_G_23(l_switch,G3(:,:,:,4),wf(:,-2),wf(:,0),G3tensor(:,21))
  call check_last_UV_W(l_switch,G3(:,:,:,4),Q(:,58),wf(:,90),Q(:,5),G4tensor(:,2))
  call loop_UV_W(G2(:,:,:,14),Q(:,26),wf(:,113),Q(:,33),G3(:,:,:,5))
  call check_last_UV_W(l_switch,G3(:,:,:,5),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,3))
  call loop_UV_W(G2(:,:,:,14),Q(:,26),wf(:,70),Q(:,36),G3(:,:,:,6))
  call check_last_UV_W(l_switch,G3(:,:,:,6),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,4))
  call loop_UV_W(G2(:,:,:,14),Q(:,26),wf(:,-2),Q(:,4),G3(:,:,:,7))
  call loop_UV_W(G3(:,:,:,7),Q(:,30),wf(:,-5),Q(:,32),G4(:,:,:,3))
  call check_last_UV_W(l_switch,G4(:,:,:,3),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,3))
  call loop_GGG_G_12(G1(:,:,:,25),wf(:,-4),wf(:,-2),G1(:,:,:,47))
  call loop_UV_W(G1(:,:,:,47),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,21))
  call check_last_UV_W(l_switch,G2(:,:,:,21),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,22))
  call loop_GGG_G_12(G1(:,:,:,25),wf(:,-2),wf(:,-4),G1(:,:,:,48))
  call loop_UV_W(G1(:,:,:,48),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,22))
  call check_last_UV_W(l_switch,G2(:,:,:,22),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,23))
  call loop_GGG_G_23(G1(:,:,:,25),wf(:,-4),wf(:,-2),G1(:,:,:,49))
  call loop_UV_W(G1(:,:,:,49),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,23))
  call check_last_UV_W(l_switch,G2(:,:,:,23),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,24))
  call loop_UV_W(G1(:,:,:,25),Q(:,10),wf(:,-5),Q(:,32),G2(:,:,:,24))
  call loop_GGG_G_12(G2(:,:,:,24),wf(:,-4),wf(:,-2),G2(:,:,:,25))
  call check_last_UV_W(l_switch,G2(:,:,:,25),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,25))
  call loop_GGG_G_12(G2(:,:,:,24),wf(:,-2),wf(:,-4),G2(:,:,:,26))
  call check_last_UV_W(l_switch,G2(:,:,:,26),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,26))
  call loop_GGG_G_23(G2(:,:,:,24),wf(:,-4),wf(:,-2),G2(:,:,:,27))
  call check_last_UV_W(l_switch,G2(:,:,:,27),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,27))
  call loop_GGG_G_12(G2(:,:,:,24),wf(:,-4),wf(:,0),G2(:,:,:,28))
  call check_last_UV_W(l_switch,G2(:,:,:,28),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,28))
  call loop_GGG_G_12(G2(:,:,:,24),wf(:,0),wf(:,-4),G2(:,:,:,29))
  call check_last_UV_W(l_switch,G2(:,:,:,29),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,29))
  call loop_GGG_G_23(G2(:,:,:,24),wf(:,-4),wf(:,0),G2(:,:,:,30))
  call check_last_UV_W(l_switch,G2(:,:,:,30),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,30))
  call loop_UV_W(G2(:,:,:,24),Q(:,42),wf(:,-4),Q(:,16),G3(:,:,:,8))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,8),wf(:,-2),wf(:,0),G3tensor(:,31))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,8),wf(:,0),wf(:,-2),G3tensor(:,32))
  call check_last_GGG_G_23(l_switch,G3(:,:,:,8),wf(:,-2),wf(:,0),G3tensor(:,33))
  call check_last_UV_W(l_switch,G3(:,:,:,8),Q(:,58),wf(:,90),Q(:,5),G4tensor(:,5))
  call loop_UV_W(G2(:,:,:,24),Q(:,42),wf(:,109),Q(:,17),G3(:,:,:,9))
  call check_last_UV_W(l_switch,G3(:,:,:,9),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,6))
  call loop_UV_W(G2(:,:,:,24),Q(:,42),wf(:,66),Q(:,20),G3(:,:,:,10))
  call check_last_UV_W(l_switch,G3(:,:,:,10),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,7))
  call loop_UV_W(G2(:,:,:,24),Q(:,42),wf(:,-2),Q(:,4),G3(:,:,:,11))
  call loop_UV_W(G3(:,:,:,11),Q(:,46),wf(:,-4),Q(:,16),G4(:,:,:,4))
  call check_last_UV_W(l_switch,G4(:,:,:,4),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,4))
  call loop_UV_W(G1(:,:,:,25),Q(:,10),wf(:,8),Q(:,52),G2(:,:,:,31))
  call check_last_UV_W(l_switch,G2(:,:,:,31),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,34))
  call loop_UV_W(G1(:,:,:,25),Q(:,10),wf(:,11),Q(:,52),G2(:,:,:,32))
  call check_last_UV_W(l_switch,G2(:,:,:,32),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,35))
  call loop_UV_W(G1(:,:,:,25),Q(:,10),wf(:,12),Q(:,52),G2(:,:,:,33))
  call check_last_UV_W(l_switch,G2(:,:,:,33),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,36))
  call loop_GGG_G_12(G1(:,:,:,25),wf(:,0),wf(:,84),G1(:,:,:,50))
  call check_last_UV_W(l_switch,G1(:,:,:,50),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,37))
  call loop_GGG_G_12(G1(:,:,:,25),wf(:,84),wf(:,0),G1(:,:,:,51))
  call check_last_UV_W(l_switch,G1(:,:,:,51),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,38))
  call loop_GGG_G_23(G1(:,:,:,25),wf(:,0),wf(:,84),G1(:,:,:,52))
  call check_last_UV_W(l_switch,G1(:,:,:,52),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,39))
  call loop_GGG_G_12(G1(:,:,:,25),wf(:,-5),wf(:,0),G1(:,:,:,53))
  call loop_UV_W(G1(:,:,:,53),Q(:,43),wf(:,-4),Q(:,16),G2(:,:,:,34))
  call check_last_UV_W(l_switch,G2(:,:,:,34),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,37))
  call loop_GGG_G_12(G1(:,:,:,25),wf(:,0),wf(:,-5),G1(:,:,:,54))
  call loop_UV_W(G1(:,:,:,54),Q(:,43),wf(:,-4),Q(:,16),G2(:,:,:,35))
  call check_last_UV_W(l_switch,G2(:,:,:,35),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,38))
  call loop_GGG_G_23(G1(:,:,:,25),wf(:,-5),wf(:,0),G1(:,:,:,55))
  call loop_UV_W(G1(:,:,:,55),Q(:,43),wf(:,-4),Q(:,16),G2(:,:,:,36))
  call check_last_UV_W(l_switch,G2(:,:,:,36),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,39))
  call loop_GGG_G_12(G1(:,:,:,25),wf(:,-4),wf(:,0),G1(:,:,:,56))
  call loop_UV_W(G1(:,:,:,56),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,37))
  call check_last_UV_W(l_switch,G2(:,:,:,37),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,40))
  call loop_GGG_G_12(G1(:,:,:,25),wf(:,0),wf(:,-4),G1(:,:,:,57))
  call loop_UV_W(G1(:,:,:,57),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,38))
  call check_last_UV_W(l_switch,G2(:,:,:,38),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,41))
  call loop_GGG_G_23(G1(:,:,:,25),wf(:,-4),wf(:,0),G1(:,:,:,58))
  call loop_UV_W(G1(:,:,:,58),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,39))
  call check_last_UV_W(l_switch,G2(:,:,:,39),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,42))
  call loop_UV_W(G1(:,:,:,25),Q(:,10),wf(:,55),Q(:,49),G2(:,:,:,40))
  call check_last_UV_W(l_switch,G2(:,:,:,40),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,43))
  call loop_UV_W(G1(:,:,:,25),Q(:,10),wf(:,57),Q(:,49),G2(:,:,:,41))
  call check_last_UV_W(l_switch,G2(:,:,:,41),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,44))
  call loop_UV_W(G1(:,:,:,25),Q(:,10),wf(:,58),Q(:,49),G2(:,:,:,42))
  call check_last_UV_W(l_switch,G2(:,:,:,42),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,45))
  call loop_UV_W(G1(:,:,:,25),Q(:,10),wf(:,84),Q(:,48),G2(:,:,:,43))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,43),wf(:,-2),wf(:,0),G2tensor(:,40))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,43),wf(:,0),wf(:,-2),G2tensor(:,41))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,43),wf(:,-2),wf(:,0),G2tensor(:,42))
  call check_last_UV_W(l_switch,G2(:,:,:,43),Q(:,58),wf(:,90),Q(:,5),G3tensor(:,46))
  call loop_UV_W(G1(:,:,:,25),Q(:,10),wf(:,193),Q(:,49),G2(:,:,:,44))
  call check_last_UV_W(l_switch,G2(:,:,:,44),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,47))
  call loop_UV_W(G1(:,:,:,25),Q(:,10),wf(:,109),Q(:,17),G2(:,:,:,45))
  call loop_UV_W(G2(:,:,:,45),Q(:,27),wf(:,-5),Q(:,32),G3(:,:,:,12))
  call check_last_UV_W(l_switch,G3(:,:,:,12),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,8))
  call loop_UV_W(G1(:,:,:,25),Q(:,10),wf(:,265),Q(:,52),G2(:,:,:,46))
  call check_last_UV_W(l_switch,G2(:,:,:,46),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,48))
  call loop_UV_W(G1(:,:,:,25),Q(:,10),wf(:,66),Q(:,20),G2(:,:,:,47))
  call loop_UV_W(G2(:,:,:,47),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,13))
  call check_last_UV_W(l_switch,G3(:,:,:,13),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,9))
  call loop_UV_W(G1(:,:,:,25),Q(:,10),wf(:,194),Q(:,49),G2(:,:,:,48))
  call check_last_UV_W(l_switch,G2(:,:,:,48),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,49))
  call loop_UV_W(G1(:,:,:,25),Q(:,10),wf(:,113),Q(:,33),G2(:,:,:,49))
  call loop_UV_W(G2(:,:,:,49),Q(:,43),wf(:,-4),Q(:,16),G3(:,:,:,14))
  call check_last_UV_W(l_switch,G3(:,:,:,14),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,10))
  call loop_UV_W(G1(:,:,:,25),Q(:,10),wf(:,266),Q(:,52),G2(:,:,:,50))
  call check_last_UV_W(l_switch,G2(:,:,:,50),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,50))
  call loop_UV_W(G1(:,:,:,25),Q(:,10),wf(:,70),Q(:,36),G2(:,:,:,51))
  call loop_UV_W(G2(:,:,:,51),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,15))
  call check_last_UV_W(l_switch,G3(:,:,:,15),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,11))
  call loop_UV_W(G1(:,:,:,25),Q(:,10),wf(:,195),Q(:,49),G2(:,:,:,52))
  call check_last_UV_W(l_switch,G2(:,:,:,52),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,51))
  call loop_UV_W(G1(:,:,:,25),Q(:,10),wf(:,267),Q(:,52),G2(:,:,:,53))
  call check_last_UV_W(l_switch,G2(:,:,:,53),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,52))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,0),G0(:,:,:,44))
  call loop_GGG_G_12(G0(:,:,:,44),wf(:,-5),wf(:,75),G0(:,:,:,45))
  call check_last_UV_W(l_switch,G0(:,:,:,45),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,64))
  call loop_GGG_G_12(G0(:,:,:,44),wf(:,75),wf(:,-5),G0(:,:,:,46))
  call check_last_UV_W(l_switch,G0(:,:,:,46),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,65))
  call loop_GGG_G_23(G0(:,:,:,44),wf(:,-5),wf(:,75),G0(:,:,:,47))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,66))
  call loop_GGG_G_12(G0(:,:,:,44),wf(:,-4),wf(:,79),G0(:,:,:,48))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,67))
  call loop_GGG_G_12(G0(:,:,:,44),wf(:,79),wf(:,-4),G0(:,:,:,49))
  call check_last_UV_W(l_switch,G0(:,:,:,49),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,68))
  call loop_GGG_G_23(G0(:,:,:,44),wf(:,-4),wf(:,79),G0(:,:,:,50))
  call check_last_UV_W(l_switch,G0(:,:,:,50),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,69))
  call loop_GGG_G_12(G0(:,:,:,44),wf(:,-5),wf(:,-4),G0(:,:,:,51))
  call loop_UV_W(G0(:,:,:,51),Q(:,53),wf(:,-3),Q(:,8),G1(:,:,:,59))
  call check_last_UV_W(l_switch,G1(:,:,:,59),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,43))
  call loop_GGG_G_12(G0(:,:,:,44),wf(:,-4),wf(:,-5),G0(:,:,:,52))
  call loop_UV_W(G0(:,:,:,52),Q(:,53),wf(:,-3),Q(:,8),G1(:,:,:,60))
  call check_last_UV_W(l_switch,G1(:,:,:,60),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,44))
  call loop_GGG_G_23(G0(:,:,:,44),wf(:,-5),wf(:,-4),G0(:,:,:,53))
  call loop_UV_W(G0(:,:,:,53),Q(:,53),wf(:,-3),Q(:,8),G1(:,:,:,61))
  call check_last_UV_W(l_switch,G1(:,:,:,61),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,45))
  call loop_UV_W(G0(:,:,:,44),Q(:,5),wf(:,-3),Q(:,8),G1(:,:,:,62))
  call loop_GGG_G_12(G1(:,:,:,62),wf(:,-5),wf(:,-4),G1(:,:,:,63))
  call check_last_UV_W(l_switch,G1(:,:,:,63),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,46))
  call loop_GGG_G_12(G1(:,:,:,62),wf(:,-4),wf(:,-5),G1(:,:,:,64))
  call check_last_UV_W(l_switch,G1(:,:,:,64),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,47))
  call loop_GGG_G_23(G1(:,:,:,62),wf(:,-5),wf(:,-4),G1(:,:,:,65))
  call check_last_UV_W(l_switch,G1(:,:,:,65),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,48))
  call loop_UV_W(G1(:,:,:,62),Q(:,13),wf(:,84),Q(:,48),G2(:,:,:,54))
  call check_last_UV_W(l_switch,G2(:,:,:,54),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,53))
  call loop_UV_W(G1(:,:,:,62),Q(:,13),wf(:,-5),Q(:,32),G2(:,:,:,55))
  call loop_UV_W(G2(:,:,:,55),Q(:,45),wf(:,-4),Q(:,16),G3(:,:,:,16))
  call check_last_UV_W(l_switch,G3(:,:,:,16),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,12))
  call loop_UV_W(G1(:,:,:,62),Q(:,13),wf(:,-4),Q(:,16),G2(:,:,:,56))
  call loop_UV_W(G2(:,:,:,56),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,17))
  call check_last_UV_W(l_switch,G3(:,:,:,17),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,13))
  call loop_GGG_G_12(G0(:,:,:,44),wf(:,-3),wf(:,84),G0(:,:,:,54))
  call check_last_UV_W(l_switch,G0(:,:,:,54),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,70))
  call loop_GGG_G_12(G0(:,:,:,44),wf(:,84),wf(:,-3),G0(:,:,:,55))
  call check_last_UV_W(l_switch,G0(:,:,:,55),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,71))
  call loop_GGG_G_23(G0(:,:,:,44),wf(:,-3),wf(:,84),G0(:,:,:,56))
  call check_last_UV_W(l_switch,G0(:,:,:,56),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,72))
  call loop_GGG_G_12(G0(:,:,:,44),wf(:,-5),wf(:,-3),G0(:,:,:,57))
  call loop_UV_W(G0(:,:,:,57),Q(:,45),wf(:,-4),Q(:,16),G1(:,:,:,66))
  call check_last_UV_W(l_switch,G1(:,:,:,66),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,49))
  call loop_GGG_G_12(G0(:,:,:,44),wf(:,-3),wf(:,-5),G0(:,:,:,58))
  call loop_UV_W(G0(:,:,:,58),Q(:,45),wf(:,-4),Q(:,16),G1(:,:,:,67))
  call check_last_UV_W(l_switch,G1(:,:,:,67),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,50))
  call loop_GGG_G_23(G0(:,:,:,44),wf(:,-5),wf(:,-3),G0(:,:,:,59))
  call loop_UV_W(G0(:,:,:,59),Q(:,45),wf(:,-4),Q(:,16),G1(:,:,:,68))
  call check_last_UV_W(l_switch,G1(:,:,:,68),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,51))
  call loop_UV_W(G0(:,:,:,44),Q(:,5),wf(:,-4),Q(:,16),G1(:,:,:,69))
  call loop_GGG_G_12(G1(:,:,:,69),wf(:,-5),wf(:,-3),G1(:,:,:,70))
  call check_last_UV_W(l_switch,G1(:,:,:,70),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,52))
  call loop_GGG_G_12(G1(:,:,:,69),wf(:,-3),wf(:,-5),G1(:,:,:,71))
  call check_last_UV_W(l_switch,G1(:,:,:,71),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,53))
  call loop_GGG_G_23(G1(:,:,:,69),wf(:,-5),wf(:,-3),G1(:,:,:,72))
  call check_last_UV_W(l_switch,G1(:,:,:,72),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,54))
  call loop_UV_W(G1(:,:,:,69),Q(:,21),wf(:,79),Q(:,40),G2(:,:,:,57))
  call check_last_UV_W(l_switch,G2(:,:,:,57),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,54))
  call loop_UV_W(G1(:,:,:,69),Q(:,21),wf(:,-5),Q(:,32),G2(:,:,:,58))
  call loop_UV_W(G2(:,:,:,58),Q(:,53),wf(:,-3),Q(:,8),G3(:,:,:,18))
  call check_last_UV_W(l_switch,G3(:,:,:,18),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,14))
  call loop_UV_W(G1(:,:,:,69),Q(:,21),wf(:,-3),Q(:,8),G2(:,:,:,59))
  call loop_UV_W(G2(:,:,:,59),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,19))
  call check_last_UV_W(l_switch,G3(:,:,:,19),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,15))
  call loop_GGG_G_12(G0(:,:,:,44),wf(:,-4),wf(:,-3),G0(:,:,:,60))
  call loop_UV_W(G0(:,:,:,60),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,73))
  call check_last_UV_W(l_switch,G1(:,:,:,73),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,55))
  call loop_GGG_G_12(G0(:,:,:,44),wf(:,-3),wf(:,-4),G0(:,:,:,61))
  call loop_UV_W(G0(:,:,:,61),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,74))
  call check_last_UV_W(l_switch,G1(:,:,:,74),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,56))
  call loop_GGG_G_23(G0(:,:,:,44),wf(:,-4),wf(:,-3),G0(:,:,:,62))
  call loop_UV_W(G0(:,:,:,62),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,75))
  call check_last_UV_W(l_switch,G1(:,:,:,75),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,57))
  call loop_UV_W(G0(:,:,:,44),Q(:,5),wf(:,-5),Q(:,32),G1(:,:,:,76))
  call loop_GGG_G_12(G1(:,:,:,76),wf(:,-4),wf(:,-3),G1(:,:,:,77))
  call check_last_UV_W(l_switch,G1(:,:,:,77),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,58))
  call loop_GGG_G_12(G1(:,:,:,76),wf(:,-3),wf(:,-4),G1(:,:,:,78))
  call check_last_UV_W(l_switch,G1(:,:,:,78),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,59))
  call loop_GGG_G_23(G1(:,:,:,76),wf(:,-4),wf(:,-3),G1(:,:,:,79))
  call check_last_UV_W(l_switch,G1(:,:,:,79),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,60))
  call loop_UV_W(G1(:,:,:,76),Q(:,37),wf(:,75),Q(:,24),G2(:,:,:,60))
  call check_last_UV_W(l_switch,G2(:,:,:,60),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,55))
  call loop_UV_W(G1(:,:,:,76),Q(:,37),wf(:,-4),Q(:,16),G2(:,:,:,61))
  call loop_UV_W(G2(:,:,:,61),Q(:,53),wf(:,-3),Q(:,8),G3(:,:,:,20))
  call check_last_UV_W(l_switch,G3(:,:,:,20),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,16))
  call loop_UV_W(G1(:,:,:,76),Q(:,37),wf(:,-3),Q(:,8),G2(:,:,:,62))
  call loop_UV_W(G2(:,:,:,62),Q(:,45),wf(:,-4),Q(:,16),G3(:,:,:,21))
  call check_last_UV_W(l_switch,G3(:,:,:,21),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,17))
  call loop_UV_W(G0(:,:,:,44),Q(:,5),wf(:,2),Q(:,56),G1(:,:,:,80))
  call check_last_UV_W(l_switch,G1(:,:,:,80),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,61))
  call loop_UV_W(G0(:,:,:,44),Q(:,5),wf(:,5),Q(:,56),G1(:,:,:,81))
  call check_last_UV_W(l_switch,G1(:,:,:,81),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,62))
  call loop_UV_W(G0(:,:,:,44),Q(:,5),wf(:,6),Q(:,56),G1(:,:,:,82))
  call check_last_UV_W(l_switch,G1(:,:,:,82),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,63))
  call loop_UV_W(G0(:,:,:,44),Q(:,5),wf(:,268),Q(:,56),G1(:,:,:,83))
  call check_last_UV_W(l_switch,G1(:,:,:,83),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,64))
  call loop_UV_W(G0(:,:,:,44),Q(:,5),wf(:,75),Q(:,24),G1(:,:,:,84))
  call loop_UV_W(G1(:,:,:,84),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,63))
  call check_last_UV_W(l_switch,G2(:,:,:,63),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,56))
  call loop_UV_W(G0(:,:,:,44),Q(:,5),wf(:,269),Q(:,56),G1(:,:,:,85))
  call check_last_UV_W(l_switch,G1(:,:,:,85),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,65))
  call loop_UV_W(G0(:,:,:,44),Q(:,5),wf(:,79),Q(:,40),G1(:,:,:,86))
  call loop_UV_W(G1(:,:,:,86),Q(:,45),wf(:,-4),Q(:,16),G2(:,:,:,64))
  call check_last_UV_W(l_switch,G2(:,:,:,64),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,57))
  call loop_UV_W(G0(:,:,:,44),Q(:,5),wf(:,270),Q(:,56),G1(:,:,:,87))
  call check_last_UV_W(l_switch,G1(:,:,:,87),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,66))
  call loop_UV_W(G0(:,:,:,44),Q(:,5),wf(:,84),Q(:,48),G1(:,:,:,88))
  call loop_UV_W(G1(:,:,:,88),Q(:,53),wf(:,-3),Q(:,8),G2(:,:,:,65))
  call check_last_UV_W(l_switch,G2(:,:,:,65),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,58))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,-2),G0(:,:,:,63))
  call loop_GGG_G_12(G0(:,:,:,63),wf(:,-5),wf(:,75),G0(:,:,:,64))
  call check_last_UV_W(l_switch,G0(:,:,:,64),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,73))
  call loop_GGG_G_12(G0(:,:,:,63),wf(:,75),wf(:,-5),G0(:,:,:,65))
  call check_last_UV_W(l_switch,G0(:,:,:,65),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,74))
  call loop_GGG_G_23(G0(:,:,:,63),wf(:,-5),wf(:,75),G0(:,:,:,66))
  call check_last_UV_W(l_switch,G0(:,:,:,66),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,75))
  call loop_GGG_G_12(G0(:,:,:,63),wf(:,-4),wf(:,79),G0(:,:,:,67))
  call check_last_UV_W(l_switch,G0(:,:,:,67),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,76))
  call loop_GGG_G_12(G0(:,:,:,63),wf(:,79),wf(:,-4),G0(:,:,:,68))
  call check_last_UV_W(l_switch,G0(:,:,:,68),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,77))
  call loop_GGG_G_23(G0(:,:,:,63),wf(:,-4),wf(:,79),G0(:,:,:,69))
  call check_last_UV_W(l_switch,G0(:,:,:,69),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,78))
  call loop_GGG_G_12(G0(:,:,:,63),wf(:,-5),wf(:,-4),G0(:,:,:,70))
  call loop_UV_W(G0(:,:,:,70),Q(:,53),wf(:,-3),Q(:,8),G1(:,:,:,89))
  call check_last_UV_W(l_switch,G1(:,:,:,89),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,67))
  call loop_GGG_G_12(G0(:,:,:,63),wf(:,-4),wf(:,-5),G0(:,:,:,71))
  call loop_UV_W(G0(:,:,:,71),Q(:,53),wf(:,-3),Q(:,8),G1(:,:,:,90))
  call check_last_UV_W(l_switch,G1(:,:,:,90),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,68))
  call loop_GGG_G_23(G0(:,:,:,63),wf(:,-5),wf(:,-4),G0(:,:,:,72))
  call loop_UV_W(G0(:,:,:,72),Q(:,53),wf(:,-3),Q(:,8),G1(:,:,:,91))
  call check_last_UV_W(l_switch,G1(:,:,:,91),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,69))
  call loop_UV_W(G0(:,:,:,63),Q(:,5),wf(:,-3),Q(:,8),G1(:,:,:,92))
  call loop_GGG_G_12(G1(:,:,:,92),wf(:,-5),wf(:,-4),G1(:,:,:,93))
  call check_last_UV_W(l_switch,G1(:,:,:,93),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,70))
  call loop_GGG_G_12(G1(:,:,:,92),wf(:,-4),wf(:,-5),G1(:,:,:,94))
  call check_last_UV_W(l_switch,G1(:,:,:,94),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,71))
  call loop_GGG_G_23(G1(:,:,:,92),wf(:,-5),wf(:,-4),G1(:,:,:,95))
  call check_last_UV_W(l_switch,G1(:,:,:,95),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,72))
  call loop_UV_W(G1(:,:,:,92),Q(:,13),wf(:,84),Q(:,48),G2(:,:,:,66))
  call check_last_UV_W(l_switch,G2(:,:,:,66),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,59))
  call loop_UV_W(G1(:,:,:,92),Q(:,13),wf(:,-5),Q(:,32),G2(:,:,:,67))
  call loop_UV_W(G2(:,:,:,67),Q(:,45),wf(:,-4),Q(:,16),G3(:,:,:,22))
  call check_last_UV_W(l_switch,G3(:,:,:,22),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,18))
  call loop_UV_W(G1(:,:,:,92),Q(:,13),wf(:,-4),Q(:,16),G2(:,:,:,68))
  call loop_UV_W(G2(:,:,:,68),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,23))
  call check_last_UV_W(l_switch,G3(:,:,:,23),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,19))
  call loop_GGG_G_12(G0(:,:,:,63),wf(:,-3),wf(:,84),G0(:,:,:,73))
  call check_last_UV_W(l_switch,G0(:,:,:,73),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,79))
  call loop_GGG_G_12(G0(:,:,:,63),wf(:,84),wf(:,-3),G0(:,:,:,74))
  call check_last_UV_W(l_switch,G0(:,:,:,74),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,80))
  call loop_GGG_G_23(G0(:,:,:,63),wf(:,-3),wf(:,84),G0(:,:,:,75))
  call check_last_UV_W(l_switch,G0(:,:,:,75),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,81))
  call loop_GGG_G_12(G0(:,:,:,63),wf(:,-5),wf(:,-3),G0(:,:,:,76))
  call loop_UV_W(G0(:,:,:,76),Q(:,45),wf(:,-4),Q(:,16),G1(:,:,:,96))
  call check_last_UV_W(l_switch,G1(:,:,:,96),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,73))
  call loop_GGG_G_12(G0(:,:,:,63),wf(:,-3),wf(:,-5),G0(:,:,:,77))
  call loop_UV_W(G0(:,:,:,77),Q(:,45),wf(:,-4),Q(:,16),G1(:,:,:,97))
  call check_last_UV_W(l_switch,G1(:,:,:,97),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,74))
  call loop_GGG_G_23(G0(:,:,:,63),wf(:,-5),wf(:,-3),G0(:,:,:,78))
  call loop_UV_W(G0(:,:,:,78),Q(:,45),wf(:,-4),Q(:,16),G1(:,:,:,98))
  call check_last_UV_W(l_switch,G1(:,:,:,98),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,75))
  call loop_UV_W(G0(:,:,:,63),Q(:,5),wf(:,-4),Q(:,16),G1(:,:,:,99))
  call loop_GGG_G_12(G1(:,:,:,99),wf(:,-5),wf(:,-3),G1(:,:,:,100))
  call check_last_UV_W(l_switch,G1(:,:,:,100),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,76))
  call loop_GGG_G_12(G1(:,:,:,99),wf(:,-3),wf(:,-5),G1(:,:,:,101))
  call check_last_UV_W(l_switch,G1(:,:,:,101),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,77))
  call loop_GGG_G_23(G1(:,:,:,99),wf(:,-5),wf(:,-3),G1(:,:,:,102))
  call check_last_UV_W(l_switch,G1(:,:,:,102),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,78))
  call loop_UV_W(G1(:,:,:,99),Q(:,21),wf(:,79),Q(:,40),G2(:,:,:,69))
  call check_last_UV_W(l_switch,G2(:,:,:,69),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,60))
  call loop_UV_W(G1(:,:,:,99),Q(:,21),wf(:,-5),Q(:,32),G2(:,:,:,70))
  call loop_UV_W(G2(:,:,:,70),Q(:,53),wf(:,-3),Q(:,8),G3(:,:,:,24))
  call check_last_UV_W(l_switch,G3(:,:,:,24),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,20))
  call loop_UV_W(G1(:,:,:,99),Q(:,21),wf(:,-3),Q(:,8),G2(:,:,:,71))
  call loop_UV_W(G2(:,:,:,71),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,25))
  call check_last_UV_W(l_switch,G3(:,:,:,25),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,21))
  call loop_GGG_G_12(G0(:,:,:,63),wf(:,-4),wf(:,-3),G0(:,:,:,79))
  call loop_UV_W(G0(:,:,:,79),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,103))
  call check_last_UV_W(l_switch,G1(:,:,:,103),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,79))
  call loop_GGG_G_12(G0(:,:,:,63),wf(:,-3),wf(:,-4),G0(:,:,:,80))
  call loop_UV_W(G0(:,:,:,80),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,104))
  call check_last_UV_W(l_switch,G1(:,:,:,104),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,80))
  call loop_GGG_G_23(G0(:,:,:,63),wf(:,-4),wf(:,-3),G0(:,:,:,81))
  call loop_UV_W(G0(:,:,:,81),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,105))
  call check_last_UV_W(l_switch,G1(:,:,:,105),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,81))
  call loop_UV_W(G0(:,:,:,63),Q(:,5),wf(:,-5),Q(:,32),G1(:,:,:,106))
  call loop_GGG_G_12(G1(:,:,:,106),wf(:,-4),wf(:,-3),G1(:,:,:,107))
  call check_last_UV_W(l_switch,G1(:,:,:,107),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,82))
  call loop_GGG_G_12(G1(:,:,:,106),wf(:,-3),wf(:,-4),G1(:,:,:,108))
  call check_last_UV_W(l_switch,G1(:,:,:,108),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,83))
  call loop_GGG_G_23(G1(:,:,:,106),wf(:,-4),wf(:,-3),G1(:,:,:,109))
  call check_last_UV_W(l_switch,G1(:,:,:,109),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,84))
  call loop_UV_W(G1(:,:,:,106),Q(:,37),wf(:,75),Q(:,24),G2(:,:,:,72))
  call check_last_UV_W(l_switch,G2(:,:,:,72),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,61))
  call loop_UV_W(G1(:,:,:,106),Q(:,37),wf(:,-4),Q(:,16),G2(:,:,:,73))
  call loop_UV_W(G2(:,:,:,73),Q(:,53),wf(:,-3),Q(:,8),G3(:,:,:,26))
  call check_last_UV_W(l_switch,G3(:,:,:,26),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,22))
  call loop_UV_W(G1(:,:,:,106),Q(:,37),wf(:,-3),Q(:,8),G2(:,:,:,74))
  call loop_UV_W(G2(:,:,:,74),Q(:,45),wf(:,-4),Q(:,16),G3(:,:,:,27))
  call check_last_UV_W(l_switch,G3(:,:,:,27),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,23))
  call loop_UV_W(G0(:,:,:,63),Q(:,5),wf(:,2),Q(:,56),G1(:,:,:,110))
  call check_last_UV_W(l_switch,G1(:,:,:,110),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,85))
  call loop_UV_W(G0(:,:,:,63),Q(:,5),wf(:,5),Q(:,56),G1(:,:,:,111))
  call check_last_UV_W(l_switch,G1(:,:,:,111),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,86))
  call loop_UV_W(G0(:,:,:,63),Q(:,5),wf(:,6),Q(:,56),G1(:,:,:,112))
  call check_last_UV_W(l_switch,G1(:,:,:,112),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,87))
  call loop_UV_W(G0(:,:,:,63),Q(:,5),wf(:,268),Q(:,56),G1(:,:,:,113))
  call check_last_UV_W(l_switch,G1(:,:,:,113),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,88))
  call loop_UV_W(G0(:,:,:,63),Q(:,5),wf(:,75),Q(:,24),G1(:,:,:,114))
  call loop_UV_W(G1(:,:,:,114),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,75))
  call check_last_UV_W(l_switch,G2(:,:,:,75),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,62))
  call loop_UV_W(G0(:,:,:,63),Q(:,5),wf(:,269),Q(:,56),G1(:,:,:,115))
  call check_last_UV_W(l_switch,G1(:,:,:,115),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,89))
  call loop_UV_W(G0(:,:,:,63),Q(:,5),wf(:,79),Q(:,40),G1(:,:,:,116))
  call loop_UV_W(G1(:,:,:,116),Q(:,45),wf(:,-4),Q(:,16),G2(:,:,:,76))
  call check_last_UV_W(l_switch,G2(:,:,:,76),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,63))
  call loop_UV_W(G0(:,:,:,63),Q(:,5),wf(:,270),Q(:,56),G1(:,:,:,117))
  call check_last_UV_W(l_switch,G1(:,:,:,117),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,90))
  call loop_UV_W(G0(:,:,:,63),Q(:,5),wf(:,84),Q(:,48),G1(:,:,:,118))
  call loop_UV_W(G1(:,:,:,118),Q(:,53),wf(:,-3),Q(:,8),G2(:,:,:,77))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,64))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,0),G0(:,:,:,82))
  call loop_GGG_G_12(G0(:,:,:,82),wf(:,-5),wf(:,75),G0(:,:,:,83))
  call check_last_UV_W(l_switch,G0(:,:,:,83),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,82))
  call loop_GGG_G_12(G0(:,:,:,82),wf(:,75),wf(:,-5),G0(:,:,:,84))
  call check_last_UV_W(l_switch,G0(:,:,:,84),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,83))
  call loop_GGG_G_23(G0(:,:,:,82),wf(:,-5),wf(:,75),G0(:,:,:,85))
  call check_last_UV_W(l_switch,G0(:,:,:,85),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,84))
  call loop_GGG_G_12(G0(:,:,:,82),wf(:,-4),wf(:,79),G0(:,:,:,86))
  call check_last_UV_W(l_switch,G0(:,:,:,86),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,85))
  call loop_GGG_G_12(G0(:,:,:,82),wf(:,79),wf(:,-4),G0(:,:,:,87))
  call check_last_UV_W(l_switch,G0(:,:,:,87),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,86))
  call loop_GGG_G_23(G0(:,:,:,82),wf(:,-4),wf(:,79),G0(:,:,:,88))
  call check_last_UV_W(l_switch,G0(:,:,:,88),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,87))
  call loop_GGG_G_12(G0(:,:,:,82),wf(:,-5),wf(:,-4),G0(:,:,:,89))
  call loop_UV_W(G0(:,:,:,89),Q(:,53),wf(:,-3),Q(:,8),G1(:,:,:,119))
  call check_last_UV_W(l_switch,G1(:,:,:,119),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,91))
  call loop_GGG_G_12(G0(:,:,:,82),wf(:,-4),wf(:,-5),G0(:,:,:,90))
  call loop_UV_W(G0(:,:,:,90),Q(:,53),wf(:,-3),Q(:,8),G1(:,:,:,120))
  call check_last_UV_W(l_switch,G1(:,:,:,120),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,92))
  call loop_GGG_G_23(G0(:,:,:,82),wf(:,-5),wf(:,-4),G0(:,:,:,91))
  call loop_UV_W(G0(:,:,:,91),Q(:,53),wf(:,-3),Q(:,8),G1(:,:,:,121))
  call check_last_UV_W(l_switch,G1(:,:,:,121),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,93))
  call loop_UV_W(G0(:,:,:,82),Q(:,5),wf(:,-3),Q(:,8),G1(:,:,:,122))
  call loop_GGG_G_12(G1(:,:,:,122),wf(:,-5),wf(:,-4),G1(:,:,:,123))
  call check_last_UV_W(l_switch,G1(:,:,:,123),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,94))
  call loop_GGG_G_12(G1(:,:,:,122),wf(:,-4),wf(:,-5),G1(:,:,:,124))
  call check_last_UV_W(l_switch,G1(:,:,:,124),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,95))
  call loop_GGG_G_23(G1(:,:,:,122),wf(:,-5),wf(:,-4),G1(:,:,:,125))
  call check_last_UV_W(l_switch,G1(:,:,:,125),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,96))
  call loop_UV_W(G1(:,:,:,122),Q(:,13),wf(:,84),Q(:,48),G2(:,:,:,78))
  call check_last_UV_W(l_switch,G2(:,:,:,78),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,65))
  call loop_UV_W(G1(:,:,:,122),Q(:,13),wf(:,-5),Q(:,32),G2(:,:,:,79))
  call loop_UV_W(G2(:,:,:,79),Q(:,45),wf(:,-4),Q(:,16),G3(:,:,:,28))
  call check_last_UV_W(l_switch,G3(:,:,:,28),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,24))
  call loop_UV_W(G1(:,:,:,122),Q(:,13),wf(:,-4),Q(:,16),G2(:,:,:,80))
  call loop_UV_W(G2(:,:,:,80),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,29))
  call check_last_UV_W(l_switch,G3(:,:,:,29),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,25))
  call loop_GGG_G_12(G0(:,:,:,82),wf(:,-3),wf(:,84),G0(:,:,:,92))
  call check_last_UV_W(l_switch,G0(:,:,:,92),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,88))
  call loop_GGG_G_12(G0(:,:,:,82),wf(:,84),wf(:,-3),G0(:,:,:,93))
  call check_last_UV_W(l_switch,G0(:,:,:,93),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,89))
  call loop_GGG_G_23(G0(:,:,:,82),wf(:,-3),wf(:,84),G0(:,:,:,94))
  call check_last_UV_W(l_switch,G0(:,:,:,94),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,90))
  call loop_GGG_G_12(G0(:,:,:,82),wf(:,-5),wf(:,-3),G0(:,:,:,95))
  call loop_UV_W(G0(:,:,:,95),Q(:,45),wf(:,-4),Q(:,16),G1(:,:,:,126))
  call check_last_UV_W(l_switch,G1(:,:,:,126),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,97))
  call loop_GGG_G_12(G0(:,:,:,82),wf(:,-3),wf(:,-5),G0(:,:,:,96))
  call loop_UV_W(G0(:,:,:,96),Q(:,45),wf(:,-4),Q(:,16),G1(:,:,:,127))
  call check_last_UV_W(l_switch,G1(:,:,:,127),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,98))
  call loop_GGG_G_23(G0(:,:,:,82),wf(:,-5),wf(:,-3),G0(:,:,:,97))
  call loop_UV_W(G0(:,:,:,97),Q(:,45),wf(:,-4),Q(:,16),G1(:,:,:,128))
  call check_last_UV_W(l_switch,G1(:,:,:,128),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,99))
  call loop_UV_W(G0(:,:,:,82),Q(:,5),wf(:,-4),Q(:,16),G1(:,:,:,129))
  call loop_GGG_G_12(G1(:,:,:,129),wf(:,-5),wf(:,-3),G1(:,:,:,130))
  call check_last_UV_W(l_switch,G1(:,:,:,130),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,100))
  call loop_GGG_G_12(G1(:,:,:,129),wf(:,-3),wf(:,-5),G1(:,:,:,131))
  call check_last_UV_W(l_switch,G1(:,:,:,131),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,101))
  call loop_GGG_G_23(G1(:,:,:,129),wf(:,-5),wf(:,-3),G1(:,:,:,132))
  call check_last_UV_W(l_switch,G1(:,:,:,132),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,102))
  call loop_UV_W(G1(:,:,:,129),Q(:,21),wf(:,79),Q(:,40),G2(:,:,:,81))
  call check_last_UV_W(l_switch,G2(:,:,:,81),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,66))
  call loop_UV_W(G1(:,:,:,129),Q(:,21),wf(:,-5),Q(:,32),G2(:,:,:,82))
  call loop_UV_W(G2(:,:,:,82),Q(:,53),wf(:,-3),Q(:,8),G3(:,:,:,30))
  call check_last_UV_W(l_switch,G3(:,:,:,30),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,26))
  call loop_UV_W(G1(:,:,:,129),Q(:,21),wf(:,-3),Q(:,8),G2(:,:,:,83))
  call loop_UV_W(G2(:,:,:,83),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,31))
  call check_last_UV_W(l_switch,G3(:,:,:,31),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,27))
  call loop_GGG_G_12(G0(:,:,:,82),wf(:,-4),wf(:,-3),G0(:,:,:,98))
  call loop_UV_W(G0(:,:,:,98),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,133))
  call check_last_UV_W(l_switch,G1(:,:,:,133),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,103))
  call loop_GGG_G_12(G0(:,:,:,82),wf(:,-3),wf(:,-4),G0(:,:,:,99))
  call loop_UV_W(G0(:,:,:,99),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,134))
  call check_last_UV_W(l_switch,G1(:,:,:,134),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,104))
  call loop_GGG_G_23(G0(:,:,:,82),wf(:,-4),wf(:,-3),G0(:,:,:,100))
  call loop_UV_W(G0(:,:,:,100),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,135))
  call check_last_UV_W(l_switch,G1(:,:,:,135),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,105))
  call loop_UV_W(G0(:,:,:,82),Q(:,5),wf(:,-5),Q(:,32),G1(:,:,:,136))
  call loop_GGG_G_12(G1(:,:,:,136),wf(:,-4),wf(:,-3),G1(:,:,:,137))
  call check_last_UV_W(l_switch,G1(:,:,:,137),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,106))
  call loop_GGG_G_12(G1(:,:,:,136),wf(:,-3),wf(:,-4),G1(:,:,:,138))
  call check_last_UV_W(l_switch,G1(:,:,:,138),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,107))
  call loop_GGG_G_23(G1(:,:,:,136),wf(:,-4),wf(:,-3),G1(:,:,:,139))
  call check_last_UV_W(l_switch,G1(:,:,:,139),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,108))
  call loop_UV_W(G1(:,:,:,136),Q(:,37),wf(:,75),Q(:,24),G2(:,:,:,84))
  call check_last_UV_W(l_switch,G2(:,:,:,84),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,67))
  call loop_UV_W(G1(:,:,:,136),Q(:,37),wf(:,-4),Q(:,16),G2(:,:,:,85))
  call loop_UV_W(G2(:,:,:,85),Q(:,53),wf(:,-3),Q(:,8),G3(:,:,:,32))
  call check_last_UV_W(l_switch,G3(:,:,:,32),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,28))
  call loop_UV_W(G1(:,:,:,136),Q(:,37),wf(:,-3),Q(:,8),G2(:,:,:,86))
  call loop_UV_W(G2(:,:,:,86),Q(:,45),wf(:,-4),Q(:,16),G3(:,:,:,33))
  call check_last_UV_W(l_switch,G3(:,:,:,33),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,29))
  call loop_UV_W(G0(:,:,:,82),Q(:,5),wf(:,2),Q(:,56),G1(:,:,:,140))
  call check_last_UV_W(l_switch,G1(:,:,:,140),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,109))
  call loop_UV_W(G0(:,:,:,82),Q(:,5),wf(:,5),Q(:,56),G1(:,:,:,141))
  call check_last_UV_W(l_switch,G1(:,:,:,141),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,110))
  call loop_UV_W(G0(:,:,:,82),Q(:,5),wf(:,6),Q(:,56),G1(:,:,:,142))
  call check_last_UV_W(l_switch,G1(:,:,:,142),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,111))
  call loop_UV_W(G0(:,:,:,82),Q(:,5),wf(:,268),Q(:,56),G1(:,:,:,143))
  call check_last_UV_W(l_switch,G1(:,:,:,143),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,112))
  call loop_UV_W(G0(:,:,:,82),Q(:,5),wf(:,75),Q(:,24),G1(:,:,:,144))
  call loop_UV_W(G1(:,:,:,144),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,87))
  call check_last_UV_W(l_switch,G2(:,:,:,87),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,68))
  call loop_UV_W(G0(:,:,:,82),Q(:,5),wf(:,269),Q(:,56),G1(:,:,:,145))
  call check_last_UV_W(l_switch,G1(:,:,:,145),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,113))
  call loop_UV_W(G0(:,:,:,82),Q(:,5),wf(:,79),Q(:,40),G1(:,:,:,146))
  call loop_UV_W(G1(:,:,:,146),Q(:,45),wf(:,-4),Q(:,16),G2(:,:,:,88))
  call check_last_UV_W(l_switch,G2(:,:,:,88),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,69))
  call loop_UV_W(G0(:,:,:,82),Q(:,5),wf(:,270),Q(:,56),G1(:,:,:,147))
  call check_last_UV_W(l_switch,G1(:,:,:,147),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,114))
  call loop_UV_W(G0(:,:,:,82),Q(:,5),wf(:,84),Q(:,48),G1(:,:,:,148))
  call loop_UV_W(G1(:,:,:,148),Q(:,53),wf(:,-3),Q(:,8),G2(:,:,:,89))
  call check_last_UV_W(l_switch,G2(:,:,:,89),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,70))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,25),G0(:,:,:,101))
  call loop_UV_W(G0(:,:,:,101),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,149))
  call check_last_UV_W(l_switch,G1(:,:,:,149),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,115))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,27),G0(:,:,:,102))
  call loop_UV_W(G0(:,:,:,102),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,150))
  call check_last_UV_W(l_switch,G1(:,:,:,150),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,116))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,28),G0(:,:,:,103))
  call loop_UV_W(G0(:,:,:,103),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,151))
  call check_last_UV_W(l_switch,G1(:,:,:,151),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,117))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,25),wf(:,-4),G0(:,:,:,104))
  call loop_UV_W(G0(:,:,:,104),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,152))
  call check_last_UV_W(l_switch,G1(:,:,:,152),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,118))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,27),wf(:,-4),G0(:,:,:,105))
  call loop_UV_W(G0(:,:,:,105),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,153))
  call check_last_UV_W(l_switch,G1(:,:,:,153),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,119))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,28),wf(:,-4),G0(:,:,:,106))
  call loop_UV_W(G0(:,:,:,106),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,154))
  call check_last_UV_W(l_switch,G1(:,:,:,154),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,120))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,25),G0(:,:,:,107))
  call loop_UV_W(G0(:,:,:,107),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,155))
  call check_last_UV_W(l_switch,G1(:,:,:,155),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,121))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,27),G0(:,:,:,108))
  call loop_UV_W(G0(:,:,:,108),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,156))
  call check_last_UV_W(l_switch,G1(:,:,:,156),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,122))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,28),G0(:,:,:,109))
  call loop_UV_W(G0(:,:,:,109),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,157))
  call check_last_UV_W(l_switch,G1(:,:,:,157),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,123))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,25),Q(:,13),G1(:,:,:,158))
  call loop_GGG_G_12(G1(:,:,:,158),wf(:,-5),wf(:,-4),G1(:,:,:,159))
  call check_last_UV_W(l_switch,G1(:,:,:,159),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,124))
  call loop_GGG_G_12(G1(:,:,:,158),wf(:,-4),wf(:,-5),G1(:,:,:,160))
  call check_last_UV_W(l_switch,G1(:,:,:,160),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,125))
  call loop_GGG_G_23(G1(:,:,:,158),wf(:,-5),wf(:,-4),G1(:,:,:,161))
  call check_last_UV_W(l_switch,G1(:,:,:,161),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,126))
  call loop_UV_W(G1(:,:,:,158),Q(:,13),wf(:,84),Q(:,48),G2(:,:,:,90))
  call check_last_UV_W(l_switch,G2(:,:,:,90),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,71))
  call loop_UV_W(G1(:,:,:,158),Q(:,13),wf(:,-5),Q(:,32),G2(:,:,:,91))
  call loop_UV_W(G2(:,:,:,91),Q(:,45),wf(:,-4),Q(:,16),G3(:,:,:,34))
  call check_last_UV_W(l_switch,G3(:,:,:,34),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,30))
  call loop_UV_W(G1(:,:,:,158),Q(:,13),wf(:,-4),Q(:,16),G2(:,:,:,92))
  call loop_UV_W(G2(:,:,:,92),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,35))
  call check_last_UV_W(l_switch,G3(:,:,:,35),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,31))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,27),Q(:,13),G1(:,:,:,162))
  call loop_GGG_G_12(G1(:,:,:,162),wf(:,-5),wf(:,-4),G1(:,:,:,163))
  call check_last_UV_W(l_switch,G1(:,:,:,163),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,127))
  call loop_GGG_G_12(G1(:,:,:,162),wf(:,-4),wf(:,-5),G1(:,:,:,164))
  call check_last_UV_W(l_switch,G1(:,:,:,164),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,128))
  call loop_GGG_G_23(G1(:,:,:,162),wf(:,-5),wf(:,-4),G1(:,:,:,165))
  call check_last_UV_W(l_switch,G1(:,:,:,165),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,129))
  call loop_UV_W(G1(:,:,:,162),Q(:,13),wf(:,84),Q(:,48),G2(:,:,:,93))
  call check_last_UV_W(l_switch,G2(:,:,:,93),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,72))
  call loop_UV_W(G1(:,:,:,162),Q(:,13),wf(:,-5),Q(:,32),G2(:,:,:,94))
  call loop_UV_W(G2(:,:,:,94),Q(:,45),wf(:,-4),Q(:,16),G3(:,:,:,36))
  call check_last_UV_W(l_switch,G3(:,:,:,36),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,32))
  call loop_UV_W(G1(:,:,:,162),Q(:,13),wf(:,-4),Q(:,16),G2(:,:,:,95))
  call loop_UV_W(G2(:,:,:,95),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,37))
  call check_last_UV_W(l_switch,G3(:,:,:,37),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,33))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,28),Q(:,13),G1(:,:,:,166))
  call loop_GGG_G_12(G1(:,:,:,166),wf(:,-5),wf(:,-4),G1(:,:,:,167))
  call check_last_UV_W(l_switch,G1(:,:,:,167),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,130))
  call loop_GGG_G_12(G1(:,:,:,166),wf(:,-4),wf(:,-5),G1(:,:,:,168))
  call check_last_UV_W(l_switch,G1(:,:,:,168),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,131))
  call loop_GGG_G_23(G1(:,:,:,166),wf(:,-5),wf(:,-4),G1(:,:,:,169))
  call check_last_UV_W(l_switch,G1(:,:,:,169),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,132))
  call loop_UV_W(G1(:,:,:,166),Q(:,13),wf(:,84),Q(:,48),G2(:,:,:,96))
  call check_last_UV_W(l_switch,G2(:,:,:,96),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,73))
  call loop_UV_W(G1(:,:,:,166),Q(:,13),wf(:,-5),Q(:,32),G2(:,:,:,97))
  call loop_UV_W(G2(:,:,:,97),Q(:,45),wf(:,-4),Q(:,16),G3(:,:,:,38))
  call check_last_UV_W(l_switch,G3(:,:,:,38),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,34))
  call loop_UV_W(G1(:,:,:,166),Q(:,13),wf(:,-4),Q(:,16),G2(:,:,:,98))
  call loop_UV_W(G2(:,:,:,98),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,39))
  call check_last_UV_W(l_switch,G3(:,:,:,39),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,35))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(4)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(17)+M(18)+M(23)-M(24)-M(27)+M(28)+M(33)-M(34)+M(47)+M(48)+M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98)+M(101)+M(106)-M(107)+M(112)-M(113)-M(114)+M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(M(148)-M(165)+M(166)-M(208))) * den(35)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(4)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(17)+M(18)+M(23)-M(24)-M(27)+M(28)+M(33)-M(34)-M(47)-M(48)-M(49) &
    +M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98)+M(101)+M(106)-M(107)+M(112)-M(113)-M(114)+M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(-M(143)+M(203)+M(241)-M(242))) * den(35)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,10)
  Gcoeff = (c(5)*(-M(47)-M(48)-M(49)+M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98))+c(6)*(-M(143)-M(148)+M(165)-M(166) &
    +M(203)+M(208)+M(241)-M(242))) * den(35)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,19)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)-M(17)+M(18)+M(23)-M(24)+M(27)-M(28)-M(33)+M(34)+M(47)+M(48)+M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98)+M(101)+M(106)-M(107)-M(112)-M(113)-M(114)+M(115)+M(116)+M(117)-M(118) &
    -M(120)+M(122))+c(6)*(M(146)-M(175)+M(176)-M(206))) * den(35)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,2)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)-M(17)+M(18)+M(23)-M(24)+M(27)-M(28)-M(33)+M(34)-M(47)-M(48)-M(49) &
    +M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98)+M(101)+M(106)-M(107)-M(112)-M(113)-M(114)+M(115)+M(116)+M(117)-M(118) &
    -M(120)+M(122))+c(6)*(-M(144)+M(199)-M(200)+M(204))) * den(35)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,11)
  Gcoeff = (c(5)*(-M(47)-M(48)-M(49)+M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98))+c(6)*(-M(144)-M(146)+M(175)-M(176) &
    +M(199)-M(200)+M(204)+M(206))) * den(35)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,20)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(27)-M(28)-M(33)+M(34)-M(112)+M(116)-M(118)+M(122))+c(6)*(M(146)-M(148)+M(165)-M(166) &
    -M(175)+M(176)-M(206)+M(208))) * den(35)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,3)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(27)-M(28)-M(33)+M(34)-M(112)+M(116)-M(118)+M(122))+c(6)*(M(143)-M(144)+M(199)-M(200) &
    -M(203)+M(204)-M(241)+M(242))) * den(35)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,12)
  Gcoeff = (c(6)*(M(143)-M(144)-M(146)+M(148)-M(165)+M(166)+M(175)-M(176)+M(199)-M(200)-M(203)+M(204)+M(206)-M(208)-M(241) &
    +M(242))) * den(35)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,21)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(141)+M(142)+M(172)-M(214))) * den(14)
  T3sum(1:5,29) = T3sum(1:5,29) + Gcoeff * G1tensor(:,28)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)+M(42)-M(47)-M(59) &
    -M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(167)+M(209)+M(239)-M(240))) * den(14)
  T3sum(1:5,29) = T3sum(1:5,29) + Gcoeff * G1tensor(:,37)
  Gcoeff = (c(5)*(M(42)-M(47)-M(59)-M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99))+c(6)*(M(141)-M(142)-M(167)-M(172) &
    +M(209)+M(214)+M(239)-M(240))) * den(14)
  T3sum(1:5,29) = T3sum(1:5,29) + Gcoeff * G1tensor(:,46)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(151)+M(152)+M(170)-M(212))) * den(14)
  T3sum(1:5,29) = T3sum(1:5,29) + Gcoeff * G1tensor(:,29)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)+M(42)-M(47)-M(59) &
    -M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(168)+M(197)-M(198)+M(210))) * den(14)
  T3sum(1:5,29) = T3sum(1:5,29) + Gcoeff * G1tensor(:,38)
  Gcoeff = (c(5)*(M(42)-M(47)-M(59)-M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99))+c(6)*(M(151)-M(152)-M(168)-M(170) &
    +M(197)-M(198)+M(210)+M(212))) * den(14)
  T3sum(1:5,29) = T3sum(1:5,29) + Gcoeff * G1tensor(:,47)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(141)-M(142)-M(151)+M(152) &
    +M(170)-M(172)-M(212)+M(214))) * den(14)
  T3sum(1:5,29) = T3sum(1:5,29) + Gcoeff * G1tensor(:,30)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(167)-M(168)+M(197)-M(198) &
    -M(209)+M(210)-M(239)+M(240))) * den(14)
  T3sum(1:5,29) = T3sum(1:5,29) + Gcoeff * G1tensor(:,39)
  Gcoeff = (c(6)*(-M(141)+M(142)+M(151)-M(152)+M(167)-M(168)-M(170)+M(172)+M(197)-M(198)-M(209)+M(210)+M(212)-M(214)-M(239) &
    +M(240))) * den(14)
  T3sum(1:5,29) = T3sum(1:5,29) + Gcoeff * G1tensor(:,48)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)-M(110)+M(111)+M(112)-M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(142)-M(171)+M(172)-M(184))) * den(33)
  T3sum(1:5,4) = T3sum(1:5,4) + Gcoeff * G1tensor(:,55)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(31)+M(32)+M(35)-M(36)-M(44)+M(45)+M(46) &
    +M(53)-M(56)+M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)-M(110)+M(111)+M(112)-M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(156)-M(162)-M(222)+M(225))) * den(33)
  T3sum(1:5,4) = T3sum(1:5,4) + Gcoeff * G1tensor(:,56)
  Gcoeff = (c(5)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(44)+M(53)-M(56)+M(65))+c(6)*(-M(142)+M(156)-M(162)+M(171) &
    -M(172)+M(184)-M(222)+M(225))) * den(33)
  T3sum(1:5,4) = T3sum(1:5,4) + Gcoeff * G1tensor(:,57)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)+M(31)-M(32)-M(35)+M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)+M(110)+M(111)-M(112)-M(113)-M(118) &
    -M(119)+M(124))+c(6)*(M(140)-M(177)+M(178)-M(182))) * den(33)
  T3sum(1:5,4) = T3sum(1:5,4) + Gcoeff * G1tensor(:,58)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)+M(45)+M(46) &
    +M(53)-M(56)+M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)+M(110)+M(111)-M(112)-M(113)-M(118) &
    -M(119)+M(124))+c(6)*(M(155)-M(161)-M(246)+M(249))) * den(33)
  T3sum(1:5,4) = T3sum(1:5,4) + Gcoeff * G1tensor(:,59)
  Gcoeff = (c(5)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(44)+M(53)-M(56)+M(65))+c(6)*(-M(140)+M(155)-M(161)+M(177) &
    -M(178)+M(182)-M(246)+M(249))) * den(33)
  T3sum(1:5,4) = T3sum(1:5,4) + Gcoeff * G1tensor(:,60)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)+M(31)-M(32)-M(35)+M(36)+M(110)-M(112)-M(118)+M(124))+c(6)*(M(140)-M(142)+M(171)-M(172) &
    -M(177)+M(178)-M(182)+M(184))) * den(33)
  T3sum(1:5,4) = T3sum(1:5,4) + Gcoeff * G1tensor(:,61)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)+M(31)-M(32)-M(35)+M(36)+M(110)-M(112)-M(118)+M(124))+c(6)*(M(155)-M(156)-M(161)+M(162) &
    +M(222)-M(225)-M(246)+M(249))) * den(33)
  T3sum(1:5,4) = T3sum(1:5,4) + Gcoeff * G1tensor(:,62)
  Gcoeff = (c(6)*(-M(140)+M(142)+M(155)-M(156)-M(161)+M(162)-M(171)+M(172)+M(177)-M(178)+M(182)-M(184)+M(222)-M(225)-M(246) &
    +M(249))) * den(33)
  T3sum(1:5,4) = T3sum(1:5,4) + Gcoeff * G1tensor(:,63)
  Gcoeff = (c(4)*(M(5)-M(6)-M(9)+M(10)+M(15)-M(16)-M(19)+M(20)-M(25)+M(26)+M(29)-M(30)+M(37)-M(38)-M(39)+M(40)-M(45)+M(48)-M(57) &
    +M(60)+M(71)+M(72)-M(73)-M(83)-M(84)+M(85)+M(97)-M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(155)+M(157)+M(243)-M(249))) * den(20)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,64)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(25)+M(26)+M(29)-M(30)-M(37)+M(38)+M(39)-M(40)-M(45)+M(48)+M(57) &
    -M(60)+M(71)-M(72)-M(73)-M(83)+M(84)+M(85)+M(97)-M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(140)+M(146)+M(176)-M(178))) * den(20)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,73)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(37)+M(38)+M(39)-M(40)+M(57)-M(60)-M(72)+M(84))+c(6)*(-M(140)+M(146)+M(155)-M(157) &
    +M(176)-M(178)-M(243)+M(249))) * den(20)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,82)
  Gcoeff = (c(4)*(M(5)-M(6)-M(9)+M(10)+M(15)-M(16)-M(19)+M(20)-M(25)+M(26)+M(29)-M(30)+M(37)-M(38)-M(39)+M(40)-M(45)+M(48)-M(57) &
    +M(60)+M(71)+M(72)+M(73)-M(83)-M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)+M(110)+M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(159)+M(160)+M(195)-M(219))) * den(20)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,65)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(25)+M(26)+M(29)-M(30)-M(37)+M(38)+M(39)-M(40)-M(45)+M(48)+M(57) &
    -M(60)+M(71)-M(72)+M(73)-M(83)+M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)+M(110)+M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(152)+M(154)+M(164)-M(170))) * den(20)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,74)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(37)+M(38)+M(39)-M(40)+M(57)-M(60)-M(72)+M(84))+c(6)*(-M(152)+M(154)+M(159)-M(160) &
    +M(164)-M(170)-M(195)+M(219))) * den(20)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,83)
  Gcoeff = (c(5)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(155)-M(157)-M(159) &
    +M(160)+M(195)-M(219)-M(243)+M(249))) * den(20)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,66)
  Gcoeff = (c(5)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(140)-M(146)-M(152) &
    +M(154)+M(164)-M(170)-M(176)+M(178))) * den(20)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,75)
  Gcoeff = (c(6)*(M(140)-M(146)-M(152)+M(154)-M(155)+M(157)+M(159)-M(160)+M(164)-M(170)-M(176)+M(178)-M(195)+M(219)+M(243) &
    -M(249))) * den(20)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,84)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(37)+M(38)+M(39)-M(40)-M(46)+M(51)-M(58) &
    +M(63)+M(74)+M(75)-M(76)+M(85)-M(88)-M(95)-M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(156)+M(159)+M(219)-M(225))) * den(22)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,67)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(58) &
    -M(63)+M(74)-M(75)-M(76)+M(85)-M(88)-M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(142)+M(152)+M(170)-M(172))) * den(22)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,76)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(37)-M(38)-M(39)+M(40)+M(58)-M(63)-M(75)+M(96))+c(6)*(-M(142)+M(152)+M(156)-M(159) &
    +M(170)-M(172)-M(219)+M(225))) * den(22)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,85)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(37)+M(38)+M(39)-M(40)-M(46)+M(51)-M(58) &
    +M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97)+M(102)-M(105)-M(111)+M(112)-M(116)+M(118)+M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(157)+M(158)+M(201)-M(243))) * den(22)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,68)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(58) &
    -M(63)+M(74)-M(75)+M(76)-M(85)+M(88)-M(95)+M(96)-M(97)+M(102)-M(105)-M(111)+M(112)-M(116)+M(118)+M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(146)+M(148)+M(166)-M(176))) * den(22)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,77)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(37)-M(38)-M(39)+M(40)+M(58)-M(63)-M(75)+M(96))+c(6)*(-M(146)+M(148)+M(157)-M(158) &
    +M(166)-M(176)-M(201)+M(243))) * den(22)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,86)
  Gcoeff = (c(5)*(M(76)-M(85)+M(88)-M(97)+M(112)-M(116)+M(118)-M(122)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(156)-M(157)+M(158) &
    -M(159)+M(201)-M(219)+M(225)-M(243))) * den(22)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,69)
  Gcoeff = (c(5)*(M(76)-M(85)+M(88)-M(97)+M(112)-M(116)+M(118)-M(122)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(142)-M(146)+M(148) &
    -M(152)+M(166)-M(170)+M(172)-M(176))) * den(22)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,78)
  Gcoeff = (c(6)*(M(142)-M(146)+M(148)-M(152)-M(156)+M(157)-M(158)+M(159)+M(166)-M(170)+M(172)-M(176)-M(201)+M(219)-M(225) &
    +M(243))) * den(22)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,87)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(10)-M(12)-M(14)-M(15)-M(18)-M(20)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(45)+M(46)+M(49)+M(53)+M(54)+M(55)+M(57)+M(58)+M(61)+M(65)+M(73)+M(77)+M(83)+M(84)+M(89)+M(95)+M(96)+M(98)+M(99) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(155)+M(249)))
  T4sum(1:15,55) = T4sum(1:15,55) + Gcoeff * G2tensor(:,43)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(44)+M(45)+M(46)+M(47)+M(49)+M(50)+M(56)+M(59)+M(60)+M(62)+M(63)+M(64)+M(72)+M(73)+M(75)+M(83)+M(87)+M(95)+M(98) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(140)+M(178)))
  T4sum(1:15,55) = T4sum(1:15,55) + Gcoeff * G2tensor(:,67)
  Gcoeff = (c(4)*(-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)+M(37)-M(38)-M(39)+M(40)+M(44)+M(47) &
    +M(50)-M(53)-M(54)-M(55)+M(56)-M(57)-M(58)+M(59)+M(60)-M(61)+M(62)+M(63)+M(64)-M(65)+M(72)+M(75)-M(77)-M(84)+M(87)-M(89)-M(96) &
    -M(99))+c(6)*(M(140)-M(155)+M(178)-M(249)))
  T4sum(1:15,55) = T4sum(1:15,55) + Gcoeff * G2tensor(:,91)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(10)-M(12)-M(14)-M(15)-M(17)-M(20)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(38) &
    -M(39)+M(45)+M(46)+M(52)+M(53)+M(54)+M(55)+M(57)+M(58)+M(64)+M(65)+M(76)+M(77)+M(83)+M(84)+M(86)+M(87)+M(88)+M(89)+M(95)+M(96) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(156)+M(225)))
  T4sum(1:15,55) = T4sum(1:15,55) + Gcoeff * G2tensor(:,44)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(44)+M(45)+M(46)+M(47)+M(50)+M(52)+M(56)+M(59)+M(60)+M(61)+M(62)+M(63)+M(72)+M(75)+M(76)+M(83)+M(86)+M(88)+M(95)+M(99) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(142)+M(172)))
  T4sum(1:15,55) = T4sum(1:15,55) + Gcoeff * G2tensor(:,68)
  Gcoeff = (c(4)*(-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(17)-M(18)+M(21)-M(22)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)+M(44)+M(47) &
    +M(50)-M(53)-M(54)-M(55)+M(56)-M(57)-M(58)+M(59)+M(60)+M(61)+M(62)+M(63)-M(64)-M(65)+M(72)+M(75)-M(77)-M(84)-M(87)-M(89)-M(96) &
    +M(99))+c(6)*(M(142)-M(156)+M(172)-M(225)))
  T4sum(1:15,55) = T4sum(1:15,55) + Gcoeff * G2tensor(:,92)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)-M(49)+M(52)-M(61) &
    +M(64)-M(73)+M(76)+M(86)+M(87)+M(88)-M(98)-M(99)-M(100)+M(104)-M(106)-M(110)+M(112)-M(117)+M(118)+M(123)-M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(155)+M(156)+M(225)-M(249)))
  T4sum(1:15,55) = T4sum(1:15,55) + Gcoeff * G2tensor(:,45)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)+M(61) &
    -M(64)-M(73)+M(76)+M(86)-M(87)+M(88)-M(98)+M(99)-M(100)+M(104)-M(106)-M(110)+M(112)-M(117)+M(118)+M(123)-M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(140)+M(142)+M(172)-M(178)))
  T4sum(1:15,55) = T4sum(1:15,55) + Gcoeff * G2tensor(:,69)
  Gcoeff = (c(5)*(M(17)-M(18)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)+M(61)-M(64)-M(87)+M(99))+c(6)*(-M(140)+M(142)+M(155)-M(156) &
    +M(172)-M(178)-M(225)+M(249)))
  T4sum(1:15,55) = T4sum(1:15,55) + Gcoeff * G2tensor(:,93)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(9)-M(11)-M(13)-M(16)-M(18)-M(19)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(37) &
    -M(40)+M(48)+M(49)+M(51)+M(53)+M(54)+M(55)+M(60)+M(61)+M(63)+M(65)+M(71)+M(72)+M(74)+M(75)+M(76)+M(77)+M(88)+M(89)+M(98)+M(99) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(158)+M(201)))
  T4sum(1:15,57) = T4sum(1:15,57) + Gcoeff * G2tensor(:,46)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(44)+M(47)+M(48)+M(49)+M(50)+M(51)+M(56)+M(57)+M(58)+M(59)+M(62)+M(64)+M(71)+M(74)+M(76)+M(84)+M(87)+M(88)+M(96)+M(98) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(148)+M(166)))
  T4sum(1:15,57) = T4sum(1:15,57) + Gcoeff * G2tensor(:,70)
  Gcoeff = (c(4)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(17)+M(18)-M(21)+M(22)+M(23)-M(24)+M(37)-M(38)-M(39)+M(40)+M(44)+M(47)+M(50) &
    -M(53)-M(54)-M(55)+M(56)+M(57)+M(58)+M(59)-M(60)-M(61)+M(62)-M(63)+M(64)-M(65)-M(72)-M(75)-M(77)+M(84)+M(87)-M(89)+M(96) &
    -M(99))+c(6)*(M(148)-M(158)+M(166)-M(201)))
  T4sum(1:15,57) = T4sum(1:15,57) + Gcoeff * G2tensor(:,94)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(9)-M(11)-M(13)-M(16)-M(17)-M(19)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(48)+M(51)+M(52)+M(53)+M(54)+M(55)+M(60)+M(63)+M(64)+M(65)+M(71)+M(72)+M(73)+M(74)+M(75)+M(77)+M(86)+M(87)+M(89) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(160)+M(195)))
  T4sum(1:15,57) = T4sum(1:15,57) + Gcoeff * G2tensor(:,47)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(44)+M(47)+M(48)+M(50)+M(51)+M(52)+M(56)+M(57)+M(58)+M(59)+M(61)+M(62)+M(71)+M(73)+M(74)+M(84)+M(86)+M(96)+M(99) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(154)+M(164)))
  T4sum(1:15,57) = T4sum(1:15,57) + Gcoeff * G2tensor(:,71)
  Gcoeff = (c(4)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)+M(44)+M(47)+M(50) &
    -M(53)-M(54)-M(55)+M(56)+M(57)+M(58)+M(59)-M(60)+M(61)+M(62)-M(63)-M(64)-M(65)-M(72)-M(75)-M(77)+M(84)-M(87)-M(89)+M(96) &
    +M(99))+c(6)*(M(154)-M(160)+M(164)-M(195)))
  T4sum(1:15,57) = T4sum(1:15,57) + Gcoeff * G2tensor(:,95)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)-M(49)+M(52)-M(61) &
    +M(64)+M(73)-M(76)+M(86)+M(87)-M(88)-M(98)-M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(158)+M(160)+M(195)-M(201)))
  T4sum(1:15,57) = T4sum(1:15,57) + Gcoeff * G2tensor(:,48)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)+M(61) &
    -M(64)+M(73)-M(76)+M(86)-M(87)-M(88)-M(98)+M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(148)+M(154)+M(164)-M(166)))
  T4sum(1:15,57) = T4sum(1:15,57) + Gcoeff * G2tensor(:,72)
  Gcoeff = (c(5)*(M(17)-M(18)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)+M(61)-M(64)-M(87)+M(99))+c(6)*(-M(148)+M(154)+M(158)-M(160) &
    +M(164)-M(166)-M(195)+M(201)))
  T4sum(1:15,57) = T4sum(1:15,57) + Gcoeff * G2tensor(:,96)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(163)+M(165)-M(168)-M(174) &
    -M(192)-M(198)+M(208)+M(232))) * den(5)
  T3sum(1:15,80) = T3sum(1:15,80) + Gcoeff * G2tensor(:,115)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)-M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96))+c(6)*(-M(163)-M(165)+M(187)+M(189) &
    +M(207)-M(208)+M(231)-M(232))) * den(5)
  T3sum(1:15,80) = T3sum(1:15,80) + Gcoeff * G2tensor(:,116)
  Gcoeff = (c(5)*(-M(41)+M(53)+M(60)+M(63)+M(65)-M(68)-M(69)-M(70)+M(72)+M(75)-M(81)-M(93))+c(6)*(M(168)+M(174)-M(187)-M(189) &
    +M(192)+M(198)-M(207)-M(231))) * den(5)
  T3sum(1:15,80) = T3sum(1:15,80) + Gcoeff * G2tensor(:,117)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(154)-M(160)+M(162)+M(164) &
    -M(171)-M(184)-M(195)+M(222))) * den(5)
  T3sum(1:15,80) = T3sum(1:15,80) + Gcoeff * G2tensor(:,118)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)-M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96))+c(6)*(M(153)-M(154)-M(162)-M(164) &
    +M(186)+M(188)+M(221)-M(222))) * den(5)
  T3sum(1:15,80) = T3sum(1:15,80) + Gcoeff * G2tensor(:,119)
  Gcoeff = (c(5)*(-M(41)+M(53)+M(60)+M(63)+M(65)-M(68)-M(69)-M(70)+M(72)+M(75)-M(81)-M(93))+c(6)*(-M(153)+M(160)+M(171)+M(184) &
    -M(186)-M(188)+M(195)-M(221))) * den(5)
  T3sum(1:15,80) = T3sum(1:15,80) + Gcoeff * G2tensor(:,120)
  Gcoeff = (c(6)*(M(154)-M(160)+M(162)-M(163)+M(164)-M(165)+M(168)-M(171)+M(174)-M(184)+M(192)-M(195)+M(198)-M(208)+M(222) &
    -M(232))) * den(5)
  T3sum(1:15,80) = T3sum(1:15,80) + Gcoeff * G2tensor(:,121)
  Gcoeff = (c(6)*(M(153)-M(154)-M(162)+M(163)-M(164)+M(165)+M(186)-M(187)+M(188)-M(189)-M(207)+M(208)+M(221)-M(222)-M(231) &
    +M(232))) * den(5)
  T3sum(1:15,80) = T3sum(1:15,80) + Gcoeff * G2tensor(:,122)
  Gcoeff = (c(6)*(-M(153)+M(160)-M(168)+M(171)-M(174)+M(184)-M(186)+M(187)-M(188)+M(189)-M(192)+M(195)-M(198)+M(207)-M(221) &
    +M(231))) * den(5)
  T3sum(1:15,80) = T3sum(1:15,80) + Gcoeff * G2tensor(:,123)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(148)-M(158)+M(161)+M(166) &
    -M(177)-M(182)-M(201)+M(246))) * den(5)
  T3sum(1:15,30) = T3sum(1:15,30) + Gcoeff * G2tensor(:,124)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)-M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96))+c(6)*(M(147)-M(148)-M(161)-M(166) &
    +M(185)+M(190)+M(245)-M(246))) * den(5)
  T3sum(1:15,30) = T3sum(1:15,30) + Gcoeff * G2tensor(:,127)
  Gcoeff = (c(5)*(-M(41)+M(53)+M(60)+M(63)+M(65)-M(68)-M(69)-M(70)+M(72)+M(75)-M(81)-M(93))+c(6)*(-M(147)+M(158)+M(177)+M(182) &
    -M(185)-M(190)+M(201)-M(245))) * den(5)
  T3sum(1:15,30) = T3sum(1:15,30) + Gcoeff * G2tensor(:,130)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(154)-M(160)+M(162)+M(164) &
    -M(171)-M(184)-M(195)+M(222))) * den(5)
  T3sum(1:15,30) = T3sum(1:15,30) + Gcoeff * G2tensor(:,125)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)-M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96))+c(6)*(M(153)-M(154)-M(162)-M(164) &
    +M(186)+M(188)+M(221)-M(222))) * den(5)
  T3sum(1:15,30) = T3sum(1:15,30) + Gcoeff * G2tensor(:,128)
  Gcoeff = (c(5)*(-M(41)+M(53)+M(60)+M(63)+M(65)-M(68)-M(69)-M(70)+M(72)+M(75)-M(81)-M(93))+c(6)*(-M(153)+M(160)+M(171)+M(184) &
    -M(186)-M(188)+M(195)-M(221))) * den(5)
  T3sum(1:15,30) = T3sum(1:15,30) + Gcoeff * G2tensor(:,131)
  Gcoeff = (c(6)*(-M(148)+M(154)+M(158)-M(160)-M(161)+M(162)+M(164)-M(166)-M(171)+M(177)+M(182)-M(184)-M(195)+M(201)+M(222) &
    -M(246))) * den(5)
  T3sum(1:15,30) = T3sum(1:15,30) + Gcoeff * G2tensor(:,126)
  Gcoeff = (c(6)*(-M(147)+M(148)+M(153)-M(154)+M(161)-M(162)-M(164)+M(166)-M(185)+M(186)+M(188)-M(190)+M(221)-M(222)-M(245) &
    +M(246))) * den(5)
  T3sum(1:15,30) = T3sum(1:15,30) + Gcoeff * G2tensor(:,129)
  Gcoeff = (c(6)*(M(147)-M(153)-M(158)+M(160)+M(171)-M(177)-M(182)+M(184)+M(185)-M(186)-M(188)+M(190)+M(195)-M(201)-M(221) &
    +M(245))) * den(5)
  T3sum(1:15,30) = T3sum(1:15,30) + Gcoeff * G2tensor(:,132)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)-M(49)+M(52)-M(61) &
    +M(64)+M(73)-M(76)+M(86)+M(87)-M(88)-M(98)-M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(158)+M(160)+M(195)-M(201))) * den(26)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,70)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)+M(61) &
    -M(64)+M(73)-M(76)+M(86)-M(87)-M(88)-M(98)+M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(148)+M(154)+M(164)-M(166))) * den(26)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,79)
  Gcoeff = (c(5)*(M(17)-M(18)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)+M(61)-M(64)-M(87)+M(99))+c(6)*(-M(148)+M(154)+M(158)-M(160) &
    +M(164)-M(166)-M(195)+M(201))) * den(26)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,88)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)-M(49)+M(52)-M(61) &
    +M(64)-M(73)+M(76)+M(86)+M(87)+M(88)-M(98)-M(99)-M(100)+M(104)-M(106)-M(110)+M(112)-M(117)+M(118)+M(123)-M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(155)+M(156)+M(225)-M(249))) * den(26)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,71)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)+M(61) &
    -M(64)-M(73)+M(76)+M(86)-M(87)+M(88)-M(98)+M(99)-M(100)+M(104)-M(106)-M(110)+M(112)-M(117)+M(118)+M(123)-M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(140)+M(142)+M(172)-M(178))) * den(26)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,80)
  Gcoeff = (c(5)*(M(17)-M(18)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)+M(61)-M(64)-M(87)+M(99))+c(6)*(-M(140)+M(142)+M(155)-M(156) &
    +M(172)-M(178)-M(225)+M(249))) * den(26)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,89)
  Gcoeff = (c(5)*(-M(73)+M(76)+M(88)-M(100)-M(110)+M(112)+M(118)-M(124)-M(125)+M(126)+M(128)-M(130))+c(6)*(-M(155)+M(156)+M(158) &
    -M(160)-M(195)+M(201)+M(225)-M(249))) * den(26)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,72)
  Gcoeff = (c(5)*(-M(73)+M(76)+M(88)-M(100)-M(110)+M(112)+M(118)-M(124)-M(125)+M(126)+M(128)-M(130))+c(6)*(-M(140)+M(142)+M(148) &
    -M(154)-M(164)+M(166)+M(172)-M(178))) * den(26)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,81)
  Gcoeff = (c(6)*(-M(140)+M(142)+M(148)-M(154)+M(155)-M(156)-M(158)+M(160)-M(164)+M(166)+M(172)-M(178)+M(195)-M(201)-M(225) &
    +M(249))) * den(26)
  T3sum(1:5,77) = T3sum(1:5,77) + Gcoeff * G1tensor(:,90)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(9)-M(12)-M(14)-M(16)-M(18)-M(19)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(46)+M(48)+M(49)+M(53)+M(54)+M(55)+M(58)+M(60)+M(61)+M(65)+M(71)+M(72)+M(77)+M(85)+M(89)+M(95)+M(96)+M(97)+M(98)+M(99) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(157)+M(243)))
  T4sum(1:15,58) = T4sum(1:15,58) + Gcoeff * G2tensor(:,49)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(44)+M(46)+M(47)+M(48)+M(49)+M(50)+M(56)+M(57)+M(59)+M(62)+M(63)+M(64)+M(71)+M(75)+M(84)+M(85)+M(87)+M(95)+M(97)+M(98) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(146)+M(176)))
  T4sum(1:15,58) = T4sum(1:15,58) + Gcoeff * G2tensor(:,73)
  Gcoeff = (c(4)*(M(9)-M(10)-M(11)+M(12)-M(15)+M(16)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(37)+M(38)+M(39)-M(40)+M(44)+M(47)+M(50) &
    -M(53)-M(54)-M(55)+M(56)+M(57)-M(58)+M(59)-M(60)-M(61)+M(62)+M(63)+M(64)-M(65)-M(72)+M(75)-M(77)+M(84)+M(87)-M(89)-M(96) &
    -M(99))+c(6)*(M(146)-M(157)+M(176)-M(243)))
  T4sum(1:15,58) = T4sum(1:15,58) + Gcoeff * G2tensor(:,97)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(9)-M(11)-M(13)-M(16)-M(18)-M(19)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(37) &
    -M(40)+M(48)+M(49)+M(51)+M(53)+M(54)+M(55)+M(60)+M(61)+M(63)+M(65)+M(71)+M(72)+M(74)+M(75)+M(76)+M(77)+M(88)+M(89)+M(98)+M(99) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(158)+M(201)))
  T4sum(1:15,58) = T4sum(1:15,58) + Gcoeff * G2tensor(:,50)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(44)+M(47)+M(48)+M(49)+M(50)+M(51)+M(56)+M(57)+M(58)+M(59)+M(62)+M(64)+M(71)+M(74)+M(76)+M(84)+M(87)+M(88)+M(96)+M(98) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(148)+M(166)))
  T4sum(1:15,58) = T4sum(1:15,58) + Gcoeff * G2tensor(:,74)
  Gcoeff = (c(4)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(17)+M(18)-M(21)+M(22)+M(23)-M(24)+M(37)-M(38)-M(39)+M(40)+M(44)+M(47)+M(50) &
    -M(53)-M(54)-M(55)+M(56)+M(57)+M(58)+M(59)-M(60)-M(61)+M(62)-M(63)+M(64)-M(65)-M(72)-M(75)-M(77)+M(84)+M(87)-M(89)+M(96) &
    -M(99))+c(6)*(M(148)-M(158)+M(166)-M(201)))
  T4sum(1:15,58) = T4sum(1:15,58) + Gcoeff * G2tensor(:,98)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(37)+M(38)+M(39)-M(40)-M(46)+M(51)-M(58) &
    +M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97)+M(102)-M(105)-M(111)+M(112)-M(116)+M(118)+M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(157)+M(158)+M(201)-M(243)))
  T4sum(1:15,58) = T4sum(1:15,58) + Gcoeff * G2tensor(:,51)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(58) &
    -M(63)+M(74)-M(75)+M(76)-M(85)+M(88)-M(95)+M(96)-M(97)+M(102)-M(105)-M(111)+M(112)-M(116)+M(118)+M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(146)+M(148)+M(166)-M(176)))
  T4sum(1:15,58) = T4sum(1:15,58) + Gcoeff * G2tensor(:,75)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(37)-M(38)-M(39)+M(40)+M(58)-M(63)-M(75)+M(96))+c(6)*(-M(146)+M(148)+M(157)-M(158) &
    +M(166)-M(176)-M(201)+M(243)))
  T4sum(1:15,58) = T4sum(1:15,58) + Gcoeff * G2tensor(:,99)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(10)-M(12)-M(14)-M(15)-M(17)-M(20)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(38) &
    -M(39)+M(45)+M(46)+M(52)+M(53)+M(54)+M(55)+M(57)+M(58)+M(64)+M(65)+M(76)+M(77)+M(83)+M(84)+M(86)+M(87)+M(88)+M(89)+M(95)+M(96) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(156)+M(225)))
  T4sum(1:15,60) = T4sum(1:15,60) + Gcoeff * G2tensor(:,52)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(44)+M(45)+M(46)+M(47)+M(50)+M(52)+M(56)+M(59)+M(60)+M(61)+M(62)+M(63)+M(72)+M(75)+M(76)+M(83)+M(86)+M(88)+M(95)+M(99) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(142)+M(172)))
  T4sum(1:15,60) = T4sum(1:15,60) + Gcoeff * G2tensor(:,76)
  Gcoeff = (c(4)*(-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(17)-M(18)+M(21)-M(22)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)+M(44)+M(47) &
    +M(50)-M(53)-M(54)-M(55)+M(56)-M(57)-M(58)+M(59)+M(60)+M(61)+M(62)+M(63)-M(64)-M(65)+M(72)+M(75)-M(77)-M(84)-M(87)-M(89)-M(96) &
    +M(99))+c(6)*(M(142)-M(156)+M(172)-M(225)))
  T4sum(1:15,60) = T4sum(1:15,60) + Gcoeff * G2tensor(:,100)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(10)-M(11)-M(13)-M(15)-M(17)-M(20)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(45)+M(51)+M(52)+M(53)+M(54)+M(55)+M(57)+M(63)+M(64)+M(65)+M(74)+M(75)+M(77)+M(83)+M(84)+M(85)+M(86)+M(87)+M(89)+M(97) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(159)+M(219)))
  T4sum(1:15,60) = T4sum(1:15,60) + Gcoeff * G2tensor(:,53)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(44)+M(45)+M(47)+M(50)+M(51)+M(52)+M(56)+M(58)+M(59)+M(60)+M(61)+M(62)+M(72)+M(74)+M(83)+M(85)+M(86)+M(96)+M(97)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(152)+M(170)))
  T4sum(1:15,60) = T4sum(1:15,60) + Gcoeff * G2tensor(:,77)
  Gcoeff = (c(4)*(-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)+M(37)-M(38)-M(39)+M(40)+M(44)+M(47) &
    +M(50)-M(53)-M(54)-M(55)+M(56)-M(57)+M(58)+M(59)+M(60)+M(61)+M(62)-M(63)-M(64)-M(65)+M(72)-M(75)-M(77)-M(84)-M(87)-M(89)+M(96) &
    +M(99))+c(6)*(M(152)-M(159)+M(170)-M(219)))
  T4sum(1:15,60) = T4sum(1:15,60) + Gcoeff * G2tensor(:,101)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(37)+M(38)+M(39)-M(40)-M(46)+M(51)-M(58) &
    +M(63)+M(74)+M(75)-M(76)+M(85)-M(88)-M(95)-M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(156)+M(159)+M(219)-M(225)))
  T4sum(1:15,60) = T4sum(1:15,60) + Gcoeff * G2tensor(:,54)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(58) &
    -M(63)+M(74)-M(75)-M(76)+M(85)-M(88)-M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(142)+M(152)+M(170)-M(172)))
  T4sum(1:15,60) = T4sum(1:15,60) + Gcoeff * G2tensor(:,78)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(37)-M(38)-M(39)+M(40)+M(58)-M(63)-M(75)+M(96))+c(6)*(-M(142)+M(152)+M(156)-M(159) &
    +M(170)-M(172)-M(219)+M(225)))
  T4sum(1:15,60) = T4sum(1:15,60) + Gcoeff * G2tensor(:,102)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(10)-M(11)-M(13)-M(15)-M(17)-M(20)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(45)+M(51)+M(52)+M(53)+M(54)+M(55)+M(57)+M(63)+M(64)+M(65)+M(74)+M(75)+M(77)+M(83)+M(84)+M(85)+M(86)+M(87)+M(89)+M(97) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(159)+M(219)))
  T4sum(1:15,61) = T4sum(1:15,61) + Gcoeff * G2tensor(:,55)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(44)+M(45)+M(47)+M(50)+M(51)+M(52)+M(56)+M(58)+M(59)+M(60)+M(61)+M(62)+M(72)+M(74)+M(83)+M(85)+M(86)+M(96)+M(97)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(152)+M(170)))
  T4sum(1:15,61) = T4sum(1:15,61) + Gcoeff * G2tensor(:,79)
  Gcoeff = (c(4)*(-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)+M(37)-M(38)-M(39)+M(40)+M(44)+M(47) &
    +M(50)-M(53)-M(54)-M(55)+M(56)-M(57)+M(58)+M(59)+M(60)+M(61)+M(62)-M(63)-M(64)-M(65)+M(72)-M(75)-M(77)-M(84)-M(87)-M(89)+M(96) &
    +M(99))+c(6)*(M(152)-M(159)+M(170)-M(219)))
  T4sum(1:15,61) = T4sum(1:15,61) + Gcoeff * G2tensor(:,103)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(9)-M(11)-M(13)-M(16)-M(17)-M(19)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(48)+M(51)+M(52)+M(53)+M(54)+M(55)+M(60)+M(63)+M(64)+M(65)+M(71)+M(72)+M(73)+M(74)+M(75)+M(77)+M(86)+M(87)+M(89) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(160)+M(195)))
  T4sum(1:15,61) = T4sum(1:15,61) + Gcoeff * G2tensor(:,56)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(44)+M(47)+M(48)+M(50)+M(51)+M(52)+M(56)+M(57)+M(58)+M(59)+M(61)+M(62)+M(71)+M(73)+M(74)+M(84)+M(86)+M(96)+M(99) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(154)+M(164)))
  T4sum(1:15,61) = T4sum(1:15,61) + Gcoeff * G2tensor(:,80)
  Gcoeff = (c(4)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)+M(44)+M(47)+M(50) &
    -M(53)-M(54)-M(55)+M(56)+M(57)+M(58)+M(59)-M(60)+M(61)+M(62)-M(63)-M(64)-M(65)-M(72)-M(75)-M(77)+M(84)-M(87)-M(89)+M(96) &
    +M(99))+c(6)*(M(154)-M(160)+M(164)-M(195)))
  T4sum(1:15,61) = T4sum(1:15,61) + Gcoeff * G2tensor(:,104)
  Gcoeff = (c(4)*(M(5)-M(6)-M(9)+M(10)+M(15)-M(16)-M(19)+M(20)-M(25)+M(26)+M(29)-M(30)+M(37)-M(38)-M(39)+M(40)-M(45)+M(48)-M(57) &
    +M(60)+M(71)+M(72)+M(73)-M(83)-M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)+M(110)+M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(159)+M(160)+M(195)-M(219)))
  T4sum(1:15,61) = T4sum(1:15,61) + Gcoeff * G2tensor(:,57)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(25)+M(26)+M(29)-M(30)-M(37)+M(38)+M(39)-M(40)-M(45)+M(48)+M(57) &
    -M(60)+M(71)-M(72)+M(73)-M(83)+M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)+M(110)+M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(152)+M(154)+M(164)-M(170)))
  T4sum(1:15,61) = T4sum(1:15,61) + Gcoeff * G2tensor(:,81)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(37)+M(38)+M(39)-M(40)+M(57)-M(60)-M(72)+M(84))+c(6)*(-M(152)+M(154)+M(159)-M(160) &
    +M(164)-M(170)-M(195)+M(219)))
  T4sum(1:15,61) = T4sum(1:15,61) + Gcoeff * G2tensor(:,105)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(10)-M(12)-M(14)-M(15)-M(18)-M(20)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(45)+M(46)+M(49)+M(53)+M(54)+M(55)+M(57)+M(58)+M(61)+M(65)+M(73)+M(77)+M(83)+M(84)+M(89)+M(95)+M(96)+M(98)+M(99) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(155)+M(249)))
  T4sum(1:15,63) = T4sum(1:15,63) + Gcoeff * G2tensor(:,58)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(44)+M(45)+M(46)+M(47)+M(49)+M(50)+M(56)+M(59)+M(60)+M(62)+M(63)+M(64)+M(72)+M(73)+M(75)+M(83)+M(87)+M(95)+M(98) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(140)+M(178)))
  T4sum(1:15,63) = T4sum(1:15,63) + Gcoeff * G2tensor(:,82)
  Gcoeff = (c(4)*(-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)+M(37)-M(38)-M(39)+M(40)+M(44)+M(47) &
    +M(50)-M(53)-M(54)-M(55)+M(56)-M(57)-M(58)+M(59)+M(60)-M(61)+M(62)+M(63)+M(64)-M(65)+M(72)+M(75)-M(77)-M(84)+M(87)-M(89)-M(96) &
    -M(99))+c(6)*(M(140)-M(155)+M(178)-M(249)))
  T4sum(1:15,63) = T4sum(1:15,63) + Gcoeff * G2tensor(:,106)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(9)-M(12)-M(14)-M(16)-M(18)-M(19)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(46)+M(48)+M(49)+M(53)+M(54)+M(55)+M(58)+M(60)+M(61)+M(65)+M(71)+M(72)+M(77)+M(85)+M(89)+M(95)+M(96)+M(97)+M(98)+M(99) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(157)+M(243)))
  T4sum(1:15,63) = T4sum(1:15,63) + Gcoeff * G2tensor(:,59)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(44)+M(46)+M(47)+M(48)+M(49)+M(50)+M(56)+M(57)+M(59)+M(62)+M(63)+M(64)+M(71)+M(75)+M(84)+M(85)+M(87)+M(95)+M(97)+M(98) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(146)+M(176)))
  T4sum(1:15,63) = T4sum(1:15,63) + Gcoeff * G2tensor(:,83)
  Gcoeff = (c(4)*(M(9)-M(10)-M(11)+M(12)-M(15)+M(16)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(37)+M(38)+M(39)-M(40)+M(44)+M(47)+M(50) &
    -M(53)-M(54)-M(55)+M(56)+M(57)-M(58)+M(59)-M(60)-M(61)+M(62)+M(63)+M(64)-M(65)-M(72)+M(75)-M(77)+M(84)+M(87)-M(89)-M(96) &
    -M(99))+c(6)*(M(146)-M(157)+M(176)-M(243)))
  T4sum(1:15,63) = T4sum(1:15,63) + Gcoeff * G2tensor(:,107)
  Gcoeff = (c(4)*(M(5)-M(6)-M(9)+M(10)+M(15)-M(16)-M(19)+M(20)-M(25)+M(26)+M(29)-M(30)+M(37)-M(38)-M(39)+M(40)-M(45)+M(48)-M(57) &
    +M(60)+M(71)+M(72)-M(73)-M(83)-M(84)+M(85)+M(97)-M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(155)+M(157)+M(243)-M(249)))
  T4sum(1:15,63) = T4sum(1:15,63) + Gcoeff * G2tensor(:,60)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(25)+M(26)+M(29)-M(30)-M(37)+M(38)+M(39)-M(40)-M(45)+M(48)+M(57) &
    -M(60)+M(71)-M(72)-M(73)-M(83)+M(84)+M(85)+M(97)-M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(140)+M(146)+M(176)-M(178)))
  T4sum(1:15,63) = T4sum(1:15,63) + Gcoeff * G2tensor(:,84)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(37)+M(38)+M(39)-M(40)+M(57)-M(60)-M(72)+M(84))+c(6)*(-M(140)+M(146)+M(155)-M(157) &
    +M(176)-M(178)-M(243)+M(249)))
  T4sum(1:15,63) = T4sum(1:15,63) + Gcoeff * G2tensor(:,108)
  Gcoeff = (c(5)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(155)-M(157)-M(159) &
    +M(160)+M(195)-M(219)-M(243)+M(249))) * den(18)
  T3sum(1:15,77) = T3sum(1:15,77) + Gcoeff * G2tensor(:,61)
  Gcoeff = (c(5)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(140)-M(146)-M(152) &
    +M(154)+M(164)-M(170)-M(176)+M(178))) * den(18)
  T3sum(1:15,77) = T3sum(1:15,77) + Gcoeff * G2tensor(:,85)
  Gcoeff = (c(6)*(M(140)-M(146)-M(152)+M(154)-M(155)+M(157)+M(159)-M(160)+M(164)-M(170)-M(176)+M(178)-M(195)+M(219)+M(243) &
    -M(249))) * den(18)
  T3sum(1:15,77) = T3sum(1:15,77) + Gcoeff * G2tensor(:,109)
  Gcoeff = (c(5)*(-M(73)+M(76)+M(88)-M(100)-M(110)+M(112)+M(118)-M(124)-M(125)+M(126)+M(128)-M(130))+c(6)*(-M(155)+M(156)+M(158) &
    -M(160)-M(195)+M(201)+M(225)-M(249))) * den(18)
  T3sum(1:15,77) = T3sum(1:15,77) + Gcoeff * G2tensor(:,62)
  Gcoeff = (c(5)*(-M(73)+M(76)+M(88)-M(100)-M(110)+M(112)+M(118)-M(124)-M(125)+M(126)+M(128)-M(130))+c(6)*(-M(140)+M(142)+M(148) &
    -M(154)-M(164)+M(166)+M(172)-M(178))) * den(18)
  T3sum(1:15,77) = T3sum(1:15,77) + Gcoeff * G2tensor(:,86)
  Gcoeff = (c(6)*(-M(140)+M(142)+M(148)-M(154)+M(155)-M(156)-M(158)+M(160)-M(164)+M(166)+M(172)-M(178)+M(195)-M(201)-M(225) &
    +M(249))) * den(18)
  T3sum(1:15,77) = T3sum(1:15,77) + Gcoeff * G2tensor(:,110)
  Gcoeff = (c(5)*(-M(76)+M(85)-M(88)+M(97)-M(112)+M(116)-M(118)+M(122)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(156)+M(157)-M(158) &
    +M(159)-M(201)+M(219)-M(225)+M(243))) * den(18)
  T3sum(1:15,77) = T3sum(1:15,77) + Gcoeff * G2tensor(:,63)
  Gcoeff = (c(5)*(-M(76)+M(85)-M(88)+M(97)-M(112)+M(116)-M(118)+M(122)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(142)+M(146)-M(148) &
    +M(152)-M(166)+M(170)-M(172)+M(176))) * den(18)
  T3sum(1:15,77) = T3sum(1:15,77) + Gcoeff * G2tensor(:,87)
  Gcoeff = (c(6)*(-M(142)+M(146)-M(148)+M(152)+M(156)-M(157)+M(158)-M(159)-M(166)+M(170)-M(172)+M(176)+M(201)-M(219)+M(225) &
    -M(243))) * den(18)
  T3sum(1:15,77) = T3sum(1:15,77) + Gcoeff * G2tensor(:,111)
  Gcoeff = (c(4)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)+M(47)+M(48)+M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98)-M(101)+M(106)+M(107)+M(112)+M(113)-M(114)-M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(M(147)-M(189)+M(190)-M(207))) * den(35)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,4)
  Gcoeff = (c(4)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)-M(47)-M(48)-M(49) &
    +M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98)-M(101)+M(106)+M(107)+M(112)+M(113)-M(114)-M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(-M(145)+M(205)+M(235)-M(236))) * den(35)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,13)
  Gcoeff = (c(5)*(-M(47)-M(48)-M(49)+M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98))+c(6)*(-M(145)-M(147)+M(189)-M(190) &
    +M(205)+M(207)+M(235)-M(236))) * den(35)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,22)
  Gcoeff = (c(4)*(M(3)-M(4)-M(9)+M(10)-M(13)+M(14)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)-M(27)+M(28)+M(33)-M(34)+M(47)+M(48)+M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98)-M(101)-M(106)+M(107)+M(112)+M(113)+M(114)-M(115)-M(116)-M(117)+M(118) &
    +M(120)-M(122))+c(6)*(M(144)-M(199)+M(200)-M(204))) * den(35)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,5)
  Gcoeff = (c(4)*(M(3)-M(4)-M(9)+M(10)-M(13)+M(14)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)-M(27)+M(28)+M(33)-M(34)-M(47)-M(48)-M(49) &
    +M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98)-M(101)-M(106)+M(107)+M(112)+M(113)+M(114)-M(115)-M(116)-M(117)+M(118) &
    +M(120)-M(122))+c(6)*(-M(146)+M(175)-M(176)+M(206))) * den(35)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,14)
  Gcoeff = (c(5)*(-M(47)-M(48)-M(49)+M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98))+c(6)*(-M(144)-M(146)+M(175)-M(176) &
    +M(199)-M(200)+M(204)+M(206))) * den(35)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,23)
  Gcoeff = (c(5)*(-M(9)+M(10)+M(15)-M(16)-M(27)+M(28)+M(33)-M(34)-M(106)+M(114)-M(117)+M(120))+c(6)*(M(144)-M(147)+M(189)-M(190) &
    -M(199)+M(200)-M(204)+M(207))) * den(35)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,6)
  Gcoeff = (c(5)*(-M(9)+M(10)+M(15)-M(16)-M(27)+M(28)+M(33)-M(34)-M(106)+M(114)-M(117)+M(120))+c(6)*(M(145)-M(146)+M(175)-M(176) &
    -M(205)+M(206)-M(235)+M(236))) * den(35)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,15)
  Gcoeff = (c(6)*(-M(144)+M(145)-M(146)+M(147)+M(175)-M(176)-M(189)+M(190)+M(199)-M(200)+M(204)-M(205)+M(206)-M(207)-M(235) &
    +M(236))) * den(35)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,24)
  Gcoeff = (c(4)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(17)+M(18)+M(23)-M(24)-M(27)+M(28)+M(33)-M(34)+M(47)+M(48)+M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98)+M(101)+M(106)-M(107)+M(112)-M(113)-M(114)+M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(M(148)-M(165)+M(166)-M(208))) * den(35)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,7)
  Gcoeff = (c(4)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(17)+M(18)+M(23)-M(24)-M(27)+M(28)+M(33)-M(34)-M(47)-M(48)-M(49) &
    +M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98)+M(101)+M(106)-M(107)+M(112)-M(113)-M(114)+M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(-M(143)+M(203)+M(241)-M(242))) * den(35)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,16)
  Gcoeff = (c(5)*(-M(47)-M(48)-M(49)+M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98))+c(6)*(-M(143)-M(148)+M(165)-M(166) &
    +M(203)+M(208)+M(241)-M(242))) * den(35)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,25)
  Gcoeff = (c(4)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)+M(47)+M(48)+M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98)-M(101)+M(106)+M(107)+M(112)+M(113)-M(114)-M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(M(147)-M(189)+M(190)-M(207))) * den(35)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,8)
  Gcoeff = (c(4)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)-M(47)-M(48)-M(49) &
    +M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98)-M(101)+M(106)+M(107)+M(112)+M(113)-M(114)-M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(-M(145)+M(205)+M(235)-M(236))) * den(35)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,17)
  Gcoeff = (c(5)*(-M(47)-M(48)-M(49)+M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98))+c(6)*(-M(145)-M(147)+M(189)-M(190) &
    +M(205)+M(207)+M(235)-M(236))) * den(35)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,26)
  Gcoeff = (c(5)*(M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)-M(101)+M(107)+M(113)-M(115))+c(6)*(M(147)-M(148)+M(165)-M(166) &
    -M(189)+M(190)-M(207)+M(208))) * den(35)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,9)
  Gcoeff = (c(5)*(M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)-M(101)+M(107)+M(113)-M(115))+c(6)*(M(143)-M(145)-M(203)+M(205) &
    +M(235)-M(236)-M(241)+M(242))) * den(35)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,18)
  Gcoeff = (c(6)*(M(143)-M(145)-M(147)+M(148)-M(165)+M(166)+M(189)-M(190)-M(203)+M(205)+M(207)-M(208)+M(235)-M(236)-M(241) &
    +M(242))) * den(35)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,27)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)+M(31)-M(32)-M(35)+M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(171)-M(183)+M(184)-M(213))) * den(14)
  T3sum(1:5,42) = T3sum(1:5,42) + Gcoeff * G1tensor(:,31)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    -M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(169)+M(211)+M(229)-M(230))) * den(14)
  T3sum(1:5,42) = T3sum(1:5,42) + Gcoeff * G1tensor(:,40)
  Gcoeff = (c(5)*(M(42)-M(47)-M(59)-M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99))+c(6)*(-M(169)-M(171)+M(183)-M(184) &
    +M(211)+M(213)+M(229)-M(230))) * den(14)
  T3sum(1:5,42) = T3sum(1:5,42) + Gcoeff * G1tensor(:,49)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(168)-M(197)+M(198)-M(210))) * den(14)
  T3sum(1:5,42) = T3sum(1:5,42) + Gcoeff * G1tensor(:,32)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    -M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(151)-M(152)-M(170)+M(212))) * den(14)
  T3sum(1:5,42) = T3sum(1:5,42) + Gcoeff * G1tensor(:,41)
  Gcoeff = (c(5)*(M(42)-M(47)-M(59)-M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99))+c(6)*(M(151)-M(152)-M(168)-M(170) &
    +M(197)-M(198)+M(210)+M(212))) * den(14)
  T3sum(1:5,42) = T3sum(1:5,42) + Gcoeff * G1tensor(:,50)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(104)+M(114)+M(120)-M(123))+c(6)*(M(168)-M(171)+M(183)-M(184) &
    -M(197)+M(198)-M(210)+M(213))) * den(14)
  T3sum(1:5,42) = T3sum(1:5,42) + Gcoeff * G1tensor(:,33)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(104)+M(114)+M(120)-M(123))+c(6)*(M(151)-M(152)+M(169)-M(170) &
    -M(211)+M(212)-M(229)+M(230))) * den(14)
  T3sum(1:5,42) = T3sum(1:5,42) + Gcoeff * G1tensor(:,42)
  Gcoeff = (c(6)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(14)
  T3sum(1:5,42) = T3sum(1:5,42) + Gcoeff * G1tensor(:,51)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(141)+M(142)+M(172)-M(214))) * den(14)
  T3sum(1:5,44) = T3sum(1:5,44) + Gcoeff * G1tensor(:,34)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)+M(42)-M(47)-M(59) &
    -M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(167)+M(209)+M(239)-M(240))) * den(14)
  T3sum(1:5,44) = T3sum(1:5,44) + Gcoeff * G1tensor(:,43)
  Gcoeff = (c(5)*(M(42)-M(47)-M(59)-M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99))+c(6)*(M(141)-M(142)-M(167)-M(172) &
    +M(209)+M(214)+M(239)-M(240))) * den(14)
  T3sum(1:5,44) = T3sum(1:5,44) + Gcoeff * G1tensor(:,52)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)+M(31)-M(32)-M(35)+M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(171)-M(183)+M(184)-M(213))) * den(14)
  T3sum(1:5,44) = T3sum(1:5,44) + Gcoeff * G1tensor(:,35)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    -M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(169)+M(211)+M(229)-M(230))) * den(14)
  T3sum(1:5,44) = T3sum(1:5,44) + Gcoeff * G1tensor(:,44)
  Gcoeff = (c(5)*(M(42)-M(47)-M(59)-M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99))+c(6)*(-M(169)-M(171)+M(183)-M(184) &
    +M(211)+M(213)+M(229)-M(230))) * den(14)
  T3sum(1:5,44) = T3sum(1:5,44) + Gcoeff * G1tensor(:,53)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(6)*(M(141)-M(142)+M(171)-M(172) &
    -M(183)+M(184)-M(213)+M(214))) * den(14)
  T3sum(1:5,44) = T3sum(1:5,44) + Gcoeff * G1tensor(:,36)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(6)*(M(167)-M(169)-M(209)+M(211) &
    +M(229)-M(230)-M(239)+M(240))) * den(14)
  T3sum(1:5,44) = T3sum(1:5,44) + Gcoeff * G1tensor(:,45)
  Gcoeff = (c(6)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(14)
  T3sum(1:5,44) = T3sum(1:5,44) + Gcoeff * G1tensor(:,54)
  Gcoeff = (c(5)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(44)+M(53)-M(56)+M(65))+c(6)*(-M(142)+M(156)-M(162)+M(171) &
    -M(172)+M(184)-M(222)+M(225))) * den(34)
  T3sum(1:15,4) = T3sum(1:15,4) + Gcoeff * G2tensor(:,19)
  Gcoeff = (c(5)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(44)+M(53)-M(56)+M(65))+c(6)*(-M(140)+M(155)-M(161)+M(177) &
    -M(178)+M(182)-M(246)+M(249))) * den(34)
  T3sum(1:15,4) = T3sum(1:15,4) + Gcoeff * G2tensor(:,20)
  Gcoeff = (c(6)*(-M(140)+M(142)+M(155)-M(156)-M(161)+M(162)-M(171)+M(172)+M(177)-M(178)+M(182)-M(184)+M(222)-M(225)-M(246) &
    +M(249))) * den(34)
  T3sum(1:15,4) = T3sum(1:15,4) + Gcoeff * G2tensor(:,21)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(6)*(-M(156)+M(162)+M(209)-M(211)+M(222) &
    -M(225)-M(229)+M(239))) * den(51)
  T3sum(1:15,51) = T3sum(1:15,51) + Gcoeff * G2tensor(:,22)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(6)*(-M(138)+M(180)+M(205)-M(215)+M(223) &
    -M(224)-M(233)+M(235))) * den(51)
  T3sum(1:15,51) = T3sum(1:15,51) + Gcoeff * G2tensor(:,23)
  Gcoeff = (c(6)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(51)
  T3sum(1:15,51) = T3sum(1:15,51) + Gcoeff * G2tensor(:,24)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(6)*(M(141)-M(142)+M(171)-M(172) &
    -M(183)+M(184)-M(213)+M(214))) * den(102)
  T3sum(1:15,52) = T3sum(1:15,52) + Gcoeff * G2tensor(:,25)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(6)*(M(137)-M(139)-M(179)+M(181) &
    +M(237)-M(238)-M(247)+M(248))) * den(102)
  T3sum(1:15,52) = T3sum(1:15,52) + Gcoeff * G2tensor(:,26)
  Gcoeff = (c(6)*(M(137)-M(139)-M(141)+M(142)-M(171)+M(172)-M(179)+M(181)+M(183)-M(184)+M(213)-M(214)+M(237)-M(238)-M(247) &
    +M(248))) * den(102)
  T3sum(1:15,52) = T3sum(1:15,52) + Gcoeff * G2tensor(:,27)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(155)+M(161)-M(205)+M(215)+M(233) &
    -M(235)+M(246)-M(249))) * den(52)
  T3sum(1:15,51) = T3sum(1:15,51) + Gcoeff * G2tensor(:,28)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(137)+M(179)-M(209)+M(211)+M(229) &
    -M(239)+M(247)-M(248))) * den(52)
  T3sum(1:15,51) = T3sum(1:15,51) + Gcoeff * G2tensor(:,29)
  Gcoeff = (c(6)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(52)
  T3sum(1:15,51) = T3sum(1:15,51) + Gcoeff * G2tensor(:,30)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(31)+M(32)+M(35)-M(36)-M(105)+M(108)-M(111)+M(119))+c(6)*(M(139)-M(140)+M(177)-M(178) &
    -M(181)+M(182)-M(237)+M(238))) * den(103)
  T3sum(1:15,52) = T3sum(1:15,52) + Gcoeff * G2tensor(:,31)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(31)+M(32)+M(35)-M(36)-M(105)+M(108)-M(111)+M(119))+c(6)*(M(138)-M(141)-M(180)+M(183) &
    +M(213)-M(214)-M(223)+M(224))) * den(103)
  T3sum(1:15,52) = T3sum(1:15,52) + Gcoeff * G2tensor(:,32)
  Gcoeff = (c(6)*(M(138)-M(139)+M(140)-M(141)-M(177)+M(178)-M(180)+M(181)-M(182)+M(183)+M(213)-M(214)-M(223)+M(224)+M(237) &
    -M(238))) * den(103)
  T3sum(1:15,52) = T3sum(1:15,52) + Gcoeff * G2tensor(:,33)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(31)+M(32)+M(35)-M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)-M(103)-M(105)+M(107)+M(108)-M(109)-M(110)-M(111)+M(112)+M(113)+M(118) &
    +M(119)-M(124))+c(6)*(M(138)-M(180)-M(223)+M(224))) * den(33)
  T4sum(1:35,12) = T4sum(1:35,12) + Gcoeff * G3tensor(:,7)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)-M(103)-M(105)+M(107)+M(108)-M(109)+M(110)-M(111)-M(112)+M(113)-M(118) &
    +M(119)+M(124))+c(6)*(M(137)-M(179)-M(247)+M(248))) * den(33)
  T4sum(1:35,12) = T4sum(1:35,12) + Gcoeff * G3tensor(:,8)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)+M(31)-M(32)-M(35)+M(36)+M(110)-M(112)-M(118)+M(124))+c(6)*(M(137)-M(138)-M(179)+M(180) &
    +M(223)-M(224)-M(247)+M(248))) * den(33)
  T4sum(1:35,12) = T4sum(1:35,12) + Gcoeff * G3tensor(:,9)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)+M(31)-M(32)-M(35)+M(36)+M(110)-M(112)-M(118)+M(124))+c(6)*(M(137)-M(138)-M(179)+M(180) &
    +M(223)-M(224)-M(247)+M(248))) * den(114)
  T3sum(1:15,52) = T3sum(1:15,52) + Gcoeff * G2tensor(:,34)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)+M(31)-M(32)-M(35)+M(36)+M(110)-M(112)-M(118)+M(124))+c(6)*(M(140)-M(142)+M(171)-M(172) &
    -M(177)+M(178)-M(182)+M(184))) * den(114)
  T3sum(1:15,52) = T3sum(1:15,52) + Gcoeff * G2tensor(:,35)
  Gcoeff = (c(6)*(-M(137)+M(138)+M(140)-M(142)+M(171)-M(172)-M(177)+M(178)+M(179)-M(180)-M(182)+M(184)-M(223)+M(224)+M(247) &
    -M(248))) * den(114)
  T3sum(1:15,52) = T3sum(1:15,52) + Gcoeff * G2tensor(:,36)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)-M(103)+M(105)+M(107)-M(108)-M(109)-M(110)+M(111)+M(112)+M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(141)-M(183)-M(213)+M(214))) * den(33)
  T4sum(1:35,34) = T4sum(1:35,34) + Gcoeff * G3tensor(:,10)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(31)+M(32)+M(35)-M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)-M(103)-M(105)+M(107)+M(108)-M(109)-M(110)-M(111)+M(112)+M(113)+M(118) &
    +M(119)-M(124))+c(6)*(M(138)-M(180)-M(223)+M(224))) * den(33)
  T4sum(1:35,34) = T4sum(1:35,34) + Gcoeff * G3tensor(:,11)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(31)+M(32)+M(35)-M(36)-M(105)+M(108)-M(111)+M(119))+c(6)*(M(138)-M(141)-M(180)+M(183) &
    +M(213)-M(214)-M(223)+M(224))) * den(33)
  T4sum(1:35,34) = T4sum(1:35,34) + Gcoeff * G3tensor(:,12)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)+M(31)-M(32)-M(35)+M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)+M(110)+M(111)-M(112)-M(113)-M(118) &
    -M(119)+M(124))+c(6)*(M(140)-M(177)+M(178)-M(182))) * den(33)
  T4sum(1:35,36) = T4sum(1:35,36) + Gcoeff * G3tensor(:,13)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)-M(105)-M(107)+M(108)+M(109)+M(110)-M(111)-M(112)-M(113)-M(118) &
    +M(119)+M(124))+c(6)*(M(139)-M(181)-M(237)+M(238))) * den(33)
  T4sum(1:35,36) = T4sum(1:35,36) + Gcoeff * G3tensor(:,14)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(31)+M(32)+M(35)-M(36)-M(105)+M(108)-M(111)+M(119))+c(6)*(M(139)-M(140)+M(177)-M(178) &
    -M(181)+M(182)-M(237)+M(238))) * den(33)
  T4sum(1:35,36) = T4sum(1:35,36) + Gcoeff * G3tensor(:,15)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)-M(105)-M(107)+M(108)+M(109)+M(110)-M(111)-M(112)-M(113)-M(118) &
    +M(119)+M(124))+c(6)*(M(139)-M(181)-M(237)+M(238))) * den(33)
  T4sum(1:35,37) = T4sum(1:35,37) + Gcoeff * G3tensor(:,22)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)-M(103)-M(105)+M(107)+M(108)-M(109)+M(110)-M(111)-M(112)+M(113)-M(118) &
    +M(119)+M(124))+c(6)*(M(137)-M(179)-M(247)+M(248))) * den(33)
  T4sum(1:35,37) = T4sum(1:35,37) + Gcoeff * G3tensor(:,23)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(6)*(M(137)-M(139)-M(179)+M(181) &
    +M(237)-M(238)-M(247)+M(248))) * den(33)
  T4sum(1:35,37) = T4sum(1:35,37) + Gcoeff * G3tensor(:,24)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)-M(110)+M(111)+M(112)-M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(142)-M(171)+M(172)-M(184))) * den(33)
  T4sum(1:35,39) = T4sum(1:35,39) + Gcoeff * G3tensor(:,25)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)-M(103)+M(105)+M(107)-M(108)-M(109)-M(110)+M(111)+M(112)+M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(141)-M(183)-M(213)+M(214))) * den(33)
  T4sum(1:35,39) = T4sum(1:35,39) + Gcoeff * G3tensor(:,26)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(6)*(M(141)-M(142)+M(171)-M(172) &
    -M(183)+M(184)-M(213)+M(214))) * den(33)
  T4sum(1:35,39) = T4sum(1:35,39) + Gcoeff * G3tensor(:,27)
  Gcoeff = (c(6)*(M(137)-M(139)-M(141)+M(142)-M(171)+M(172)-M(179)+M(181)+M(183)-M(184)+M(213)-M(214)+M(237)-M(238)-M(247) &
    +M(248))) * den(53)
  T3sum(1:35,52) = T3sum(1:35,52) + Gcoeff * G3tensor(:,34)
  Gcoeff = (c(6)*(-M(137)+M(138)+M(140)-M(142)+M(171)-M(172)-M(177)+M(178)+M(179)-M(180)-M(182)+M(184)-M(223)+M(224)+M(247) &
    -M(248))) * den(53)
  T3sum(1:35,52) = T3sum(1:35,52) + Gcoeff * G3tensor(:,35)
  Gcoeff = (c(6)*(-M(138)+M(139)-M(140)+M(141)+M(177)-M(178)+M(180)-M(181)+M(182)-M(183)-M(213)+M(214)+M(223)-M(224)-M(237) &
    +M(238))) * den(53)
  T3sum(1:35,52) = T3sum(1:35,52) + Gcoeff * G3tensor(:,36)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(6)*(M(141)-M(142)+M(171)-M(172) &
    -M(183)+M(184)-M(213)+M(214))) * den(102)
  T3sum(1:15,44) = T3sum(1:15,44) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(6)*(M(167)-M(169)-M(209)+M(211) &
    +M(229)-M(230)-M(239)+M(240))) * den(102)
  T3sum(1:15,44) = T3sum(1:15,44) + Gcoeff * G2tensor(:,13)
  Gcoeff = (c(6)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(102)
  T3sum(1:15,44) = T3sum(1:15,44) + Gcoeff * G2tensor(:,16)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)+M(31)-M(32)-M(35)+M(36)+M(110)-M(112)-M(118)+M(124))+c(6)*(M(137)-M(138)-M(179)+M(180) &
    +M(223)-M(224)-M(247)+M(248))) * den(114)
  T3sum(1:15,51) = T3sum(1:15,51) + Gcoeff * G2tensor(:,37)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)+M(31)-M(32)-M(35)+M(36)+M(110)-M(112)-M(118)+M(124))+c(6)*(M(155)-M(156)-M(161)+M(162) &
    +M(222)-M(225)-M(246)+M(249))) * den(114)
  T3sum(1:15,51) = T3sum(1:15,51) + Gcoeff * G2tensor(:,38)
  Gcoeff = (c(6)*(-M(137)+M(138)+M(155)-M(156)-M(161)+M(162)+M(179)-M(180)+M(222)-M(223)+M(224)-M(225)-M(246)+M(247)-M(248) &
    +M(249))) * den(114)
  T3sum(1:15,51) = T3sum(1:15,51) + Gcoeff * G2tensor(:,39)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)-M(44)-M(45)+M(46) &
    +M(53)-M(56)+M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)-M(110)+M(111)+M(112)-M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(209)-M(211)-M(229)+M(239))) * den(33)
  T4sum(1:35,113) = T4sum(1:35,113) + Gcoeff * G3tensor(:,37)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)-M(44)-M(45)-M(46) &
    +M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95)+M(103)+M(105)-M(107)-M(108)+M(109)-M(110)+M(111)+M(112)-M(113)+M(118) &
    -M(119)-M(124))+c(6)*(-M(137)+M(179)+M(247)-M(248))) * den(33)
  T4sum(1:35,113) = T4sum(1:35,113) + Gcoeff * G3tensor(:,38)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(137)+M(179)-M(209)+M(211)+M(229) &
    -M(239)+M(247)-M(248))) * den(33)
  T4sum(1:35,113) = T4sum(1:35,113) + Gcoeff * G3tensor(:,39)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)+M(45)+M(46) &
    +M(53)-M(56)+M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)+M(110)+M(111)-M(112)-M(113)-M(118) &
    -M(119)+M(124))+c(6)*(M(155)-M(161)-M(246)+M(249))) * den(33)
  T4sum(1:35,114) = T4sum(1:35,114) + Gcoeff * G3tensor(:,16)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)+M(45)-M(46) &
    +M(53)-M(56)+M(65)-M(66)+M(67)-M(78)+M(83)+M(90)-M(95)+M(103)+M(105)-M(107)-M(108)+M(109)+M(110)+M(111)-M(112)-M(113)-M(118) &
    -M(119)+M(124))+c(6)*(-M(205)+M(215)+M(233)-M(235))) * den(33)
  T4sum(1:35,114) = T4sum(1:35,114) + Gcoeff * G3tensor(:,17)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(155)+M(161)-M(205)+M(215)+M(233) &
    -M(235)+M(246)-M(249))) * den(33)
  T4sum(1:35,114) = T4sum(1:35,114) + Gcoeff * G3tensor(:,18)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)+M(45)-M(46) &
    +M(53)-M(56)+M(65)-M(66)+M(67)-M(78)+M(83)+M(90)-M(95)+M(103)+M(105)-M(107)-M(108)+M(109)+M(110)+M(111)-M(112)-M(113)-M(118) &
    -M(119)+M(124))+c(6)*(-M(205)+M(215)+M(233)-M(235))) * den(33)
  T4sum(1:35,116) = T4sum(1:35,116) + Gcoeff * G3tensor(:,40)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)+M(31)-M(32)-M(35)+M(36)-M(44)-M(45)-M(46) &
    +M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95)+M(103)+M(105)-M(107)-M(108)+M(109)+M(110)+M(111)-M(112)-M(113)-M(118) &
    -M(119)+M(124))+c(6)*(-M(138)+M(180)+M(223)-M(224))) * den(33)
  T4sum(1:35,116) = T4sum(1:35,116) + Gcoeff * G3tensor(:,41)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(6)*(-M(138)+M(180)+M(205)-M(215)+M(223) &
    -M(224)-M(233)+M(235))) * den(33)
  T4sum(1:35,116) = T4sum(1:35,116) + Gcoeff * G3tensor(:,42)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(31)+M(32)+M(35)-M(36)-M(44)+M(45)+M(46) &
    +M(53)-M(56)+M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)-M(110)+M(111)+M(112)-M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(156)-M(162)-M(222)+M(225))) * den(33)
  T4sum(1:35,117) = T4sum(1:35,117) + Gcoeff * G3tensor(:,28)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)-M(44)-M(45)+M(46) &
    +M(53)-M(56)+M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)-M(110)+M(111)+M(112)-M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(209)-M(211)-M(229)+M(239))) * den(33)
  T4sum(1:35,117) = T4sum(1:35,117) + Gcoeff * G3tensor(:,29)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(6)*(-M(156)+M(162)+M(209)-M(211)+M(222) &
    -M(225)-M(229)+M(239))) * den(33)
  T4sum(1:35,117) = T4sum(1:35,117) + Gcoeff * G3tensor(:,30)
  Gcoeff = (c(6)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(104)
  T3sum(1:35,51) = T3sum(1:35,51) + Gcoeff * G3tensor(:,43)
  Gcoeff = (c(6)*(-M(137)+M(138)+M(155)-M(156)-M(161)+M(162)+M(179)-M(180)+M(222)-M(223)+M(224)-M(225)-M(246)+M(247)-M(248) &
    +M(249))) * den(104)
  T3sum(1:35,51) = T3sum(1:35,51) + Gcoeff * G3tensor(:,44)
  Gcoeff = (c(6)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(104)
  T3sum(1:35,51) = T3sum(1:35,51) + Gcoeff * G3tensor(:,45)
  Gcoeff = (c(5)*(M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)-M(101)+M(107)+M(113)-M(115))+c(6)*(M(147)-M(148)+M(165)-M(166) &
    -M(189)+M(190)-M(207)+M(208))) * den(105)
  T3sum(1:15,23) = T3sum(1:15,23) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(5)*(M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)-M(101)+M(107)+M(113)-M(115))+c(6)*(M(143)-M(145)-M(203)+M(205) &
    +M(235)-M(236)-M(241)+M(242))) * den(105)
  T3sum(1:15,23) = T3sum(1:15,23) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(6)*(M(143)-M(145)-M(147)+M(148)-M(165)+M(166)+M(189)-M(190)-M(203)+M(205)+M(207)-M(208)+M(235)-M(236)-M(241) &
    +M(242))) * den(105)
  T3sum(1:15,23) = T3sum(1:15,23) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(5)*(-M(9)+M(10)+M(15)-M(16)-M(27)+M(28)+M(33)-M(34)-M(106)+M(114)-M(117)+M(120))+c(6)*(M(144)-M(147)+M(189)-M(190) &
    -M(199)+M(200)-M(204)+M(207))) * den(108)
  T3sum(1:15,20) = T3sum(1:15,20) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(5)*(-M(9)+M(10)+M(15)-M(16)-M(27)+M(28)+M(33)-M(34)-M(106)+M(114)-M(117)+M(120))+c(6)*(M(145)-M(146)+M(175)-M(176) &
    -M(205)+M(206)-M(235)+M(236))) * den(108)
  T3sum(1:15,20) = T3sum(1:15,20) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(6)*(-M(144)+M(145)-M(146)+M(147)+M(175)-M(176)-M(189)+M(190)+M(199)-M(200)+M(204)-M(205)+M(206)-M(207)-M(235) &
    +M(236))) * den(108)
  T3sum(1:15,20) = T3sum(1:15,20) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(27)-M(28)-M(33)+M(34)-M(112)+M(116)-M(118)+M(122))+c(6)*(M(146)-M(148)+M(165)-M(166) &
    -M(175)+M(176)-M(206)+M(208))) * den(117)
  T3sum(1:15,17) = T3sum(1:15,17) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(27)-M(28)-M(33)+M(34)-M(112)+M(116)-M(118)+M(122))+c(6)*(M(143)-M(144)+M(199)-M(200) &
    -M(203)+M(204)-M(241)+M(242))) * den(117)
  T3sum(1:15,17) = T3sum(1:15,17) + Gcoeff * G2tensor(:,6)
  Gcoeff = (c(6)*(M(143)-M(144)-M(146)+M(148)-M(165)+M(166)+M(175)-M(176)+M(199)-M(200)-M(203)+M(204)+M(206)-M(208)-M(241) &
    +M(242))) * den(117)
  T3sum(1:15,17) = T3sum(1:15,17) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)-M(17)+M(18)+M(23)-M(24)+M(27)-M(28)-M(33)+M(34)+M(47)+M(48)+M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98)+M(101)+M(106)-M(107)-M(112)-M(113)-M(114)+M(115)+M(116)+M(117)-M(118) &
    -M(120)+M(122))+c(6)*(M(146)-M(175)+M(176)-M(206))) * den(35)
  T4sum(1:35,141) = T4sum(1:35,141) + Gcoeff * G3tensor(:,1)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)-M(17)+M(18)+M(23)-M(24)+M(27)-M(28)-M(33)+M(34)-M(47)-M(48)-M(49) &
    +M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98)+M(101)+M(106)-M(107)-M(112)-M(113)-M(114)+M(115)+M(116)+M(117)-M(118) &
    -M(120)+M(122))+c(6)*(-M(144)+M(199)-M(200)+M(204))) * den(35)
  T4sum(1:35,141) = T4sum(1:35,141) + Gcoeff * G3tensor(:,2)
  Gcoeff = (c(5)*(-M(47)-M(48)-M(49)+M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98))+c(6)*(-M(144)-M(146)+M(175)-M(176) &
    +M(199)-M(200)+M(204)+M(206))) * den(35)
  T4sum(1:35,141) = T4sum(1:35,141) + Gcoeff * G3tensor(:,3)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(104)+M(114)+M(120)-M(123))+c(6)*(M(168)-M(171)+M(183)-M(184) &
    -M(197)+M(198)-M(210)+M(213))) * den(110)
  T3sum(1:15,42) = T3sum(1:15,42) + Gcoeff * G2tensor(:,11)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(104)+M(114)+M(120)-M(123))+c(6)*(M(151)-M(152)+M(169)-M(170) &
    -M(211)+M(212)-M(229)+M(230))) * den(110)
  T3sum(1:15,42) = T3sum(1:15,42) + Gcoeff * G2tensor(:,14)
  Gcoeff = (c(6)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(110)
  T3sum(1:15,42) = T3sum(1:15,42) + Gcoeff * G2tensor(:,17)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(141)-M(142)-M(151)+M(152) &
    +M(170)-M(172)-M(212)+M(214))) * den(129)
  T3sum(1:15,29) = T3sum(1:15,29) + Gcoeff * G2tensor(:,12)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(167)-M(168)+M(197)-M(198) &
    -M(209)+M(210)-M(239)+M(240))) * den(129)
  T3sum(1:15,29) = T3sum(1:15,29) + Gcoeff * G2tensor(:,15)
  Gcoeff = (c(6)*(-M(141)+M(142)+M(151)-M(152)+M(167)-M(168)-M(170)+M(172)+M(197)-M(198)-M(209)+M(210)+M(212)-M(214)-M(239) &
    +M(240))) * den(129)
  T3sum(1:15,29) = T3sum(1:15,29) + Gcoeff * G2tensor(:,18)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(151)+M(152)+M(170)-M(212))) * den(14)
  T4sum(1:35,153) = T4sum(1:35,153) + Gcoeff * G3tensor(:,4)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)+M(42)-M(47)-M(59) &
    -M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(168)+M(197)-M(198)+M(210))) * den(14)
  T4sum(1:35,153) = T4sum(1:35,153) + Gcoeff * G3tensor(:,5)
  Gcoeff = (c(5)*(M(42)-M(47)-M(59)-M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99))+c(6)*(M(151)-M(152)-M(168)-M(170) &
    +M(197)-M(198)+M(210)+M(212))) * den(14)
  T4sum(1:35,153) = T4sum(1:35,153) + Gcoeff * G3tensor(:,6)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)+M(31)-M(32)-M(35)+M(36)+M(110)-M(112)-M(118)+M(124))+c(6)*(M(140)-M(142)+M(171)-M(172) &
    -M(177)+M(178)-M(182)+M(184))) * den(114)
  T3sum(1:15,4) = T3sum(1:15,4) + Gcoeff * G2tensor(:,40)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)+M(31)-M(32)-M(35)+M(36)+M(110)-M(112)-M(118)+M(124))+c(6)*(M(155)-M(156)-M(161)+M(162) &
    +M(222)-M(225)-M(246)+M(249))) * den(114)
  T3sum(1:15,4) = T3sum(1:15,4) + Gcoeff * G2tensor(:,41)
  Gcoeff = (c(6)*(-M(140)+M(142)+M(155)-M(156)-M(161)+M(162)-M(171)+M(172)+M(177)-M(178)+M(182)-M(184)+M(222)-M(225)-M(246) &
    +M(249))) * den(114)
  T3sum(1:15,4) = T3sum(1:15,4) + Gcoeff * G2tensor(:,42)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)+M(31)-M(32)-M(35)+M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)+M(110)+M(111)-M(112)-M(113)-M(118) &
    -M(119)+M(124))+c(6)*(M(140)-M(177)+M(178)-M(182))) * den(33)
  T4sum(1:35,125) = T4sum(1:35,125) + Gcoeff * G3tensor(:,19)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)+M(45)+M(46) &
    +M(53)-M(56)+M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)+M(110)+M(111)-M(112)-M(113)-M(118) &
    -M(119)+M(124))+c(6)*(M(155)-M(161)-M(246)+M(249))) * den(33)
  T4sum(1:35,125) = T4sum(1:35,125) + Gcoeff * G3tensor(:,20)
  Gcoeff = (c(5)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(44)+M(53)-M(56)+M(65))+c(6)*(-M(140)+M(155)-M(161)+M(177) &
    -M(178)+M(182)-M(246)+M(249))) * den(33)
  T4sum(1:35,125) = T4sum(1:35,125) + Gcoeff * G3tensor(:,21)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)-M(110)+M(111)+M(112)-M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(142)-M(171)+M(172)-M(184))) * den(33)
  T4sum(1:35,124) = T4sum(1:35,124) + Gcoeff * G3tensor(:,31)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(31)+M(32)+M(35)-M(36)-M(44)+M(45)+M(46) &
    +M(53)-M(56)+M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)-M(110)+M(111)+M(112)-M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(156)-M(162)-M(222)+M(225))) * den(33)
  T4sum(1:35,124) = T4sum(1:35,124) + Gcoeff * G3tensor(:,32)
  Gcoeff = (c(5)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(44)+M(53)-M(56)+M(65))+c(6)*(-M(142)+M(156)-M(162)+M(171) &
    -M(172)+M(184)-M(222)+M(225))) * den(33)
  T4sum(1:35,124) = T4sum(1:35,124) + Gcoeff * G3tensor(:,33)
  Gcoeff = (c(5)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(155)-M(157)-M(159) &
    +M(160)+M(195)-M(219)-M(243)+M(249))) * den(355)
  T3sum(1:15,77) = T3sum(1:15,77) + Gcoeff * G2tensor(:,64)
  Gcoeff = (c(5)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(140)-M(146)-M(152) &
    +M(154)+M(164)-M(170)-M(176)+M(178))) * den(355)
  T3sum(1:15,77) = T3sum(1:15,77) + Gcoeff * G2tensor(:,88)
  Gcoeff = (c(6)*(M(140)-M(146)-M(152)+M(154)-M(155)+M(157)+M(159)-M(160)+M(164)-M(170)-M(176)+M(178)-M(195)+M(219)+M(243) &
    -M(249))) * den(355)
  T3sum(1:15,77) = T3sum(1:15,77) + Gcoeff * G2tensor(:,112)
  Gcoeff = (c(4)*(M(5)-M(6)-M(9)+M(10)+M(15)-M(16)-M(19)+M(20)-M(25)+M(26)+M(29)-M(30)+M(37)-M(38)-M(39)+M(40)-M(45)+M(48)-M(57) &
    +M(60)+M(71)+M(72)+M(73)-M(83)-M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)+M(110)+M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(159)+M(160)+M(195)-M(219))) * den(20)
  T4sum(1:35,61) = T4sum(1:35,61) + Gcoeff * G3tensor(:,56)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(25)+M(26)+M(29)-M(30)-M(37)+M(38)+M(39)-M(40)-M(45)+M(48)+M(57) &
    -M(60)+M(71)-M(72)+M(73)-M(83)+M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)+M(110)+M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(152)+M(154)+M(164)-M(170))) * den(20)
  T4sum(1:35,61) = T4sum(1:35,61) + Gcoeff * G3tensor(:,62)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(37)+M(38)+M(39)-M(40)+M(57)-M(60)-M(72)+M(84))+c(6)*(-M(152)+M(154)+M(159)-M(160) &
    +M(164)-M(170)-M(195)+M(219))) * den(20)
  T4sum(1:35,61) = T4sum(1:35,61) + Gcoeff * G3tensor(:,68)
  Gcoeff = (c(4)*(M(5)-M(6)-M(9)+M(10)+M(15)-M(16)-M(19)+M(20)-M(25)+M(26)+M(29)-M(30)+M(37)-M(38)-M(39)+M(40)-M(45)+M(48)-M(57) &
    +M(60)+M(71)+M(72)-M(73)-M(83)-M(84)+M(85)+M(97)-M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(155)+M(157)+M(243)-M(249))) * den(20)
  T4sum(1:35,63) = T4sum(1:35,63) + Gcoeff * G3tensor(:,55)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(25)+M(26)+M(29)-M(30)-M(37)+M(38)+M(39)-M(40)-M(45)+M(48)+M(57) &
    -M(60)+M(71)-M(72)-M(73)-M(83)+M(84)+M(85)+M(97)-M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(140)+M(146)+M(176)-M(178))) * den(20)
  T4sum(1:35,63) = T4sum(1:35,63) + Gcoeff * G3tensor(:,61)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(37)+M(38)+M(39)-M(40)+M(57)-M(60)-M(72)+M(84))+c(6)*(-M(140)+M(146)+M(155)-M(157) &
    +M(176)-M(178)-M(243)+M(249))) * den(20)
  T4sum(1:35,63) = T4sum(1:35,63) + Gcoeff * G3tensor(:,67)
  Gcoeff = (c(5)*(-M(76)+M(85)-M(88)+M(97)-M(112)+M(116)-M(118)+M(122)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(156)+M(157)-M(158) &
    +M(159)-M(201)+M(219)-M(225)+M(243))) * den(358)
  T3sum(1:15,77) = T3sum(1:15,77) + Gcoeff * G2tensor(:,65)
  Gcoeff = (c(5)*(-M(76)+M(85)-M(88)+M(97)-M(112)+M(116)-M(118)+M(122)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(142)+M(146)-M(148) &
    +M(152)-M(166)+M(170)-M(172)+M(176))) * den(358)
  T3sum(1:15,77) = T3sum(1:15,77) + Gcoeff * G2tensor(:,89)
  Gcoeff = (c(6)*(-M(142)+M(146)-M(148)+M(152)+M(156)-M(157)+M(158)-M(159)-M(166)+M(170)-M(172)+M(176)+M(201)-M(219)+M(225) &
    -M(243))) * den(358)
  T3sum(1:15,77) = T3sum(1:15,77) + Gcoeff * G2tensor(:,113)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(37)+M(38)+M(39)-M(40)-M(46)+M(51)-M(58) &
    +M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97)+M(102)-M(105)-M(111)+M(112)-M(116)+M(118)+M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(157)+M(158)+M(201)-M(243))) * den(22)
  T4sum(1:35,58) = T4sum(1:35,58) + Gcoeff * G3tensor(:,57)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(58) &
    -M(63)+M(74)-M(75)+M(76)-M(85)+M(88)-M(95)+M(96)-M(97)+M(102)-M(105)-M(111)+M(112)-M(116)+M(118)+M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(146)+M(148)+M(166)-M(176))) * den(22)
  T4sum(1:35,58) = T4sum(1:35,58) + Gcoeff * G3tensor(:,63)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(37)-M(38)-M(39)+M(40)+M(58)-M(63)-M(75)+M(96))+c(6)*(-M(146)+M(148)+M(157)-M(158) &
    +M(166)-M(176)-M(201)+M(243))) * den(22)
  T4sum(1:35,58) = T4sum(1:35,58) + Gcoeff * G3tensor(:,69)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(37)+M(38)+M(39)-M(40)-M(46)+M(51)-M(58) &
    +M(63)+M(74)+M(75)-M(76)+M(85)-M(88)-M(95)-M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(156)+M(159)+M(219)-M(225))) * den(22)
  T4sum(1:35,60) = T4sum(1:35,60) + Gcoeff * G3tensor(:,54)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(58) &
    -M(63)+M(74)-M(75)-M(76)+M(85)-M(88)-M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(142)+M(152)+M(170)-M(172))) * den(22)
  T4sum(1:35,60) = T4sum(1:35,60) + Gcoeff * G3tensor(:,60)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(37)-M(38)-M(39)+M(40)+M(58)-M(63)-M(75)+M(96))+c(6)*(-M(142)+M(152)+M(156)-M(159) &
    +M(170)-M(172)-M(219)+M(225))) * den(22)
  T4sum(1:35,60) = T4sum(1:35,60) + Gcoeff * G3tensor(:,66)
  Gcoeff = (c(5)*(M(73)-M(76)-M(88)+M(100)+M(110)-M(112)-M(118)+M(124)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(155)-M(156)-M(158) &
    +M(160)+M(195)-M(201)-M(225)+M(249))) * den(361)
  T3sum(1:15,77) = T3sum(1:15,77) + Gcoeff * G2tensor(:,66)
  Gcoeff = (c(5)*(M(73)-M(76)-M(88)+M(100)+M(110)-M(112)-M(118)+M(124)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(140)-M(142)-M(148) &
    +M(154)+M(164)-M(166)-M(172)+M(178))) * den(361)
  T3sum(1:15,77) = T3sum(1:15,77) + Gcoeff * G2tensor(:,90)
  Gcoeff = (c(6)*(M(140)-M(142)-M(148)+M(154)-M(155)+M(156)+M(158)-M(160)+M(164)-M(166)-M(172)+M(178)-M(195)+M(201)+M(225) &
    -M(249))) * den(361)
  T3sum(1:15,77) = T3sum(1:15,77) + Gcoeff * G2tensor(:,114)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)-M(49)+M(52)-M(61) &
    +M(64)-M(73)+M(76)+M(86)+M(87)+M(88)-M(98)-M(99)-M(100)+M(104)-M(106)-M(110)+M(112)-M(117)+M(118)+M(123)-M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(155)+M(156)+M(225)-M(249))) * den(26)
  T4sum(1:35,55) = T4sum(1:35,55) + Gcoeff * G3tensor(:,58)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)+M(61) &
    -M(64)-M(73)+M(76)+M(86)-M(87)+M(88)-M(98)+M(99)-M(100)+M(104)-M(106)-M(110)+M(112)-M(117)+M(118)+M(123)-M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(140)+M(142)+M(172)-M(178))) * den(26)
  T4sum(1:35,55) = T4sum(1:35,55) + Gcoeff * G3tensor(:,64)
  Gcoeff = (c(5)*(M(17)-M(18)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)+M(61)-M(64)-M(87)+M(99))+c(6)*(-M(140)+M(142)+M(155)-M(156) &
    +M(172)-M(178)-M(225)+M(249))) * den(26)
  T4sum(1:35,55) = T4sum(1:35,55) + Gcoeff * G3tensor(:,70)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)-M(49)+M(52)-M(61) &
    +M(64)+M(73)-M(76)+M(86)+M(87)-M(88)-M(98)-M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(158)+M(160)+M(195)-M(201))) * den(26)
  T4sum(1:35,57) = T4sum(1:35,57) + Gcoeff * G3tensor(:,53)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)+M(61) &
    -M(64)+M(73)-M(76)+M(86)-M(87)-M(88)-M(98)+M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(148)+M(154)+M(164)-M(166))) * den(26)
  T4sum(1:35,57) = T4sum(1:35,57) + Gcoeff * G3tensor(:,59)
  Gcoeff = (c(5)*(M(17)-M(18)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)+M(61)-M(64)-M(87)+M(99))+c(6)*(-M(148)+M(154)+M(158)-M(160) &
    +M(164)-M(166)-M(195)+M(201))) * den(26)
  T4sum(1:35,57) = T4sum(1:35,57) + Gcoeff * G3tensor(:,65)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(10)-M(12)-M(14)-M(15)-M(18)-M(20)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(45)+M(46)+M(49)+M(53)+M(54)+M(55)+M(57)+M(58)+M(61)+M(65)+M(73)+M(77)+M(83)+M(84)+M(89)+M(95)+M(96)+M(98)+M(99) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(155)+M(249)))
  T5sum(1:70,161) = T5sum(1:70,161) + Gcoeff * G4tensor(:,16)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(44)+M(45)+M(46)+M(47)+M(49)+M(50)+M(56)+M(59)+M(60)+M(62)+M(63)+M(64)+M(72)+M(73)+M(75)+M(83)+M(87)+M(95)+M(98) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(140)+M(178)))
  T5sum(1:70,161) = T5sum(1:70,161) + Gcoeff * G4tensor(:,22)
  Gcoeff = (c(4)*(-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)+M(37)-M(38)-M(39)+M(40)+M(44)+M(47) &
    +M(50)-M(53)-M(54)-M(55)+M(56)-M(57)-M(58)+M(59)+M(60)-M(61)+M(62)+M(63)+M(64)-M(65)+M(72)+M(75)-M(77)-M(84)+M(87)-M(89)-M(96) &
    -M(99))+c(6)*(M(140)-M(155)+M(178)-M(249)))
  T5sum(1:70,161) = T5sum(1:70,161) + Gcoeff * G4tensor(:,28)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(10)-M(12)-M(14)-M(15)-M(17)-M(20)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(38) &
    -M(39)+M(45)+M(46)+M(52)+M(53)+M(54)+M(55)+M(57)+M(58)+M(64)+M(65)+M(76)+M(77)+M(83)+M(84)+M(86)+M(87)+M(88)+M(89)+M(95)+M(96) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(156)+M(225)))
  T5sum(1:70,162) = T5sum(1:70,162) + Gcoeff * G4tensor(:,14)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(44)+M(45)+M(46)+M(47)+M(50)+M(52)+M(56)+M(59)+M(60)+M(61)+M(62)+M(63)+M(72)+M(75)+M(76)+M(83)+M(86)+M(88)+M(95)+M(99) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(142)+M(172)))
  T5sum(1:70,162) = T5sum(1:70,162) + Gcoeff * G4tensor(:,20)
  Gcoeff = (c(4)*(-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(17)-M(18)+M(21)-M(22)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)+M(44)+M(47) &
    +M(50)-M(53)-M(54)-M(55)+M(56)-M(57)-M(58)+M(59)+M(60)+M(61)+M(62)+M(63)-M(64)-M(65)+M(72)+M(75)-M(77)-M(84)-M(87)-M(89)-M(96) &
    +M(99))+c(6)*(M(142)-M(156)+M(172)-M(225)))
  T5sum(1:70,162) = T5sum(1:70,162) + Gcoeff * G4tensor(:,26)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(9)-M(12)-M(14)-M(16)-M(18)-M(19)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(46)+M(48)+M(49)+M(53)+M(54)+M(55)+M(58)+M(60)+M(61)+M(65)+M(71)+M(72)+M(77)+M(85)+M(89)+M(95)+M(96)+M(97)+M(98)+M(99) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(157)+M(243)))
  T5sum(1:70,165) = T5sum(1:70,165) + Gcoeff * G4tensor(:,17)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(44)+M(46)+M(47)+M(48)+M(49)+M(50)+M(56)+M(57)+M(59)+M(62)+M(63)+M(64)+M(71)+M(75)+M(84)+M(85)+M(87)+M(95)+M(97)+M(98) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(146)+M(176)))
  T5sum(1:70,165) = T5sum(1:70,165) + Gcoeff * G4tensor(:,23)
  Gcoeff = (c(4)*(M(9)-M(10)-M(11)+M(12)-M(15)+M(16)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(37)+M(38)+M(39)-M(40)+M(44)+M(47)+M(50) &
    -M(53)-M(54)-M(55)+M(56)+M(57)-M(58)+M(59)-M(60)-M(61)+M(62)+M(63)+M(64)-M(65)-M(72)+M(75)-M(77)+M(84)+M(87)-M(89)-M(96) &
    -M(99))+c(6)*(M(146)-M(157)+M(176)-M(243)))
  T5sum(1:70,165) = T5sum(1:70,165) + Gcoeff * G4tensor(:,29)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(9)-M(11)-M(13)-M(16)-M(18)-M(19)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(37) &
    -M(40)+M(48)+M(49)+M(51)+M(53)+M(54)+M(55)+M(60)+M(61)+M(63)+M(65)+M(71)+M(72)+M(74)+M(75)+M(76)+M(77)+M(88)+M(89)+M(98)+M(99) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(158)+M(201)))
  T5sum(1:70,166) = T5sum(1:70,166) + Gcoeff * G4tensor(:,12)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(44)+M(47)+M(48)+M(49)+M(50)+M(51)+M(56)+M(57)+M(58)+M(59)+M(62)+M(64)+M(71)+M(74)+M(76)+M(84)+M(87)+M(88)+M(96)+M(98) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(148)+M(166)))
  T5sum(1:70,166) = T5sum(1:70,166) + Gcoeff * G4tensor(:,18)
  Gcoeff = (c(4)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(17)+M(18)-M(21)+M(22)+M(23)-M(24)+M(37)-M(38)-M(39)+M(40)+M(44)+M(47)+M(50) &
    -M(53)-M(54)-M(55)+M(56)+M(57)+M(58)+M(59)-M(60)-M(61)+M(62)-M(63)+M(64)-M(65)-M(72)-M(75)-M(77)+M(84)+M(87)-M(89)+M(96) &
    -M(99))+c(6)*(M(148)-M(158)+M(166)-M(201)))
  T5sum(1:70,166) = T5sum(1:70,166) + Gcoeff * G4tensor(:,24)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(10)-M(11)-M(13)-M(15)-M(17)-M(20)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(45)+M(51)+M(52)+M(53)+M(54)+M(55)+M(57)+M(63)+M(64)+M(65)+M(74)+M(75)+M(77)+M(83)+M(84)+M(85)+M(86)+M(87)+M(89)+M(97) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(159)+M(219)))
  T5sum(1:70,167) = T5sum(1:70,167) + Gcoeff * G4tensor(:,15)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(44)+M(45)+M(47)+M(50)+M(51)+M(52)+M(56)+M(58)+M(59)+M(60)+M(61)+M(62)+M(72)+M(74)+M(83)+M(85)+M(86)+M(96)+M(97)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(152)+M(170)))
  T5sum(1:70,167) = T5sum(1:70,167) + Gcoeff * G4tensor(:,21)
  Gcoeff = (c(4)*(-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)+M(37)-M(38)-M(39)+M(40)+M(44)+M(47) &
    +M(50)-M(53)-M(54)-M(55)+M(56)-M(57)+M(58)+M(59)+M(60)+M(61)+M(62)-M(63)-M(64)-M(65)+M(72)-M(75)-M(77)-M(84)-M(87)-M(89)+M(96) &
    +M(99))+c(6)*(M(152)-M(159)+M(170)-M(219)))
  T5sum(1:70,167) = T5sum(1:70,167) + Gcoeff * G4tensor(:,27)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(9)-M(11)-M(13)-M(16)-M(17)-M(19)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(48)+M(51)+M(52)+M(53)+M(54)+M(55)+M(60)+M(63)+M(64)+M(65)+M(71)+M(72)+M(73)+M(74)+M(75)+M(77)+M(86)+M(87)+M(89) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(160)+M(195)))
  T5sum(1:70,168) = T5sum(1:70,168) + Gcoeff * G4tensor(:,13)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(44)+M(47)+M(48)+M(50)+M(51)+M(52)+M(56)+M(57)+M(58)+M(59)+M(61)+M(62)+M(71)+M(73)+M(74)+M(84)+M(86)+M(96)+M(99) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(154)+M(164)))
  T5sum(1:70,168) = T5sum(1:70,168) + Gcoeff * G4tensor(:,19)
  Gcoeff = (c(4)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)+M(44)+M(47)+M(50) &
    -M(53)-M(54)-M(55)+M(56)+M(57)+M(58)+M(59)-M(60)+M(61)+M(62)-M(63)-M(64)-M(65)-M(72)-M(75)-M(77)+M(84)-M(87)-M(89)+M(96) &
    +M(99))+c(6)*(M(154)-M(160)+M(164)-M(195)))
  T5sum(1:70,168) = T5sum(1:70,168) + Gcoeff * G4tensor(:,25)
  Gcoeff = (c(6)*(-M(148)+M(154)+M(158)-M(160)-M(161)+M(162)+M(164)-M(166)-M(171)+M(177)+M(182)-M(184)-M(195)+M(201)+M(222) &
    -M(246))) * den(125)
  T3sum(1:35,30) = T3sum(1:35,30) + Gcoeff * G3tensor(:,71)
  Gcoeff = (c(6)*(-M(147)+M(148)+M(153)-M(154)+M(161)-M(162)-M(164)+M(166)-M(185)+M(186)+M(188)-M(190)+M(221)-M(222)-M(245) &
    +M(246))) * den(125)
  T3sum(1:35,30) = T3sum(1:35,30) + Gcoeff * G3tensor(:,72)
  Gcoeff = (c(6)*(M(147)-M(153)-M(158)+M(160)+M(171)-M(177)-M(182)+M(184)+M(185)-M(186)-M(188)+M(190)+M(195)-M(201)-M(221) &
    +M(245))) * den(125)
  T3sum(1:35,30) = T3sum(1:35,30) + Gcoeff * G3tensor(:,73)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(148)-M(158)+M(161)+M(166) &
    -M(177)-M(182)-M(201)+M(246))) * den(5)
  T4sum(1:70,155) = T4sum(1:70,155) + Gcoeff * G4tensor(:,30)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)-M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96))+c(6)*(M(147)-M(148)-M(161)-M(166) &
    +M(185)+M(190)+M(245)-M(246))) * den(5)
  T4sum(1:70,155) = T4sum(1:70,155) + Gcoeff * G4tensor(:,32)
  Gcoeff = (c(5)*(-M(41)+M(53)+M(60)+M(63)+M(65)-M(68)-M(69)-M(70)+M(72)+M(75)-M(81)-M(93))+c(6)*(-M(147)+M(158)+M(177)+M(182) &
    -M(185)-M(190)+M(201)-M(245))) * den(5)
  T4sum(1:70,155) = T4sum(1:70,155) + Gcoeff * G4tensor(:,34)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(154)-M(160)+M(162)+M(164) &
    -M(171)-M(184)-M(195)+M(222))) * den(5)
  T4sum(1:70,156) = T4sum(1:70,156) + Gcoeff * G4tensor(:,31)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)-M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96))+c(6)*(M(153)-M(154)-M(162)-M(164) &
    +M(186)+M(188)+M(221)-M(222))) * den(5)
  T4sum(1:70,156) = T4sum(1:70,156) + Gcoeff * G4tensor(:,33)
  Gcoeff = (c(5)*(-M(41)+M(53)+M(60)+M(63)+M(65)-M(68)-M(69)-M(70)+M(72)+M(75)-M(81)-M(93))+c(6)*(-M(153)+M(160)+M(171)+M(184) &
    -M(186)-M(188)+M(195)-M(221))) * den(5)
  T4sum(1:70,156) = T4sum(1:70,156) + Gcoeff * G4tensor(:,35)
  Gcoeff = (c(6)*(-M(140)+M(142)+M(155)-M(156)-M(161)+M(162)-M(171)+M(172)+M(177)-M(178)+M(182)-M(184)+M(222)-M(225)-M(246) &
    +M(249))) * den(1339)
  T3sum(1:35,4) = T3sum(1:35,4) + Gcoeff * G3tensor(:,46)
  Gcoeff = (c(5)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(44)+M(53)-M(56)+M(65))+c(6)*(-M(142)+M(156)-M(162)+M(171) &
    -M(172)+M(184)-M(222)+M(225))) * den(34)
  T4sum(1:70,124) = T4sum(1:70,124) + Gcoeff * G4tensor(:,5)
  Gcoeff = (c(5)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(44)+M(53)-M(56)+M(65))+c(6)*(-M(140)+M(155)-M(161)+M(177) &
    -M(178)+M(182)-M(246)+M(249))) * den(34)
  T4sum(1:70,125) = T4sum(1:70,125) + Gcoeff * G4tensor(:,2)
  Gcoeff = (c(6)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(467)
  T3sum(1:35,51) = T3sum(1:35,51) + Gcoeff * G3tensor(:,47)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(6)*(-M(138)+M(180)+M(205)-M(215)+M(223) &
    -M(224)-M(233)+M(235))) * den(51)
  T4sum(1:70,116) = T4sum(1:70,116) + Gcoeff * G4tensor(:,8)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(6)*(-M(156)+M(162)+M(209)-M(211)+M(222) &
    -M(225)-M(229)+M(239))) * den(51)
  T4sum(1:70,117) = T4sum(1:70,117) + Gcoeff * G4tensor(:,6)
  Gcoeff = (c(6)*(M(137)-M(139)-M(141)+M(142)-M(171)+M(172)-M(179)+M(181)+M(183)-M(184)+M(213)-M(214)+M(237)-M(238)-M(247) &
    +M(248))) * den(757)
  T3sum(1:35,52) = T3sum(1:35,52) + Gcoeff * G3tensor(:,48)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(6)*(M(137)-M(139)-M(179)+M(181) &
    +M(237)-M(238)-M(247)+M(248))) * den(102)
  T4sum(1:70,37) = T4sum(1:70,37) + Gcoeff * G4tensor(:,9)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(6)*(M(141)-M(142)+M(171)-M(172) &
    -M(183)+M(184)-M(213)+M(214))) * den(102)
  T4sum(1:70,39) = T4sum(1:70,39) + Gcoeff * G4tensor(:,7)
  Gcoeff = (c(6)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(469)
  T3sum(1:35,51) = T3sum(1:35,51) + Gcoeff * G3tensor(:,49)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(137)+M(179)-M(209)+M(211)+M(229) &
    -M(239)+M(247)-M(248))) * den(52)
  T4sum(1:70,113) = T4sum(1:70,113) + Gcoeff * G4tensor(:,10)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(155)+M(161)-M(205)+M(215)+M(233) &
    -M(235)+M(246)-M(249))) * den(52)
  T4sum(1:70,114) = T4sum(1:70,114) + Gcoeff * G4tensor(:,3)
  Gcoeff = (c(6)*(-M(138)+M(139)-M(140)+M(141)+M(177)-M(178)+M(180)-M(181)+M(182)-M(183)-M(213)+M(214)+M(223)-M(224)-M(237) &
    +M(238))) * den(759)
  T3sum(1:35,52) = T3sum(1:35,52) + Gcoeff * G3tensor(:,50)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(31)+M(32)+M(35)-M(36)-M(105)+M(108)-M(111)+M(119))+c(6)*(M(138)-M(141)-M(180)+M(183) &
    +M(213)-M(214)-M(223)+M(224))) * den(103)
  T4sum(1:70,34) = T4sum(1:70,34) + Gcoeff * G4tensor(:,11)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(31)+M(32)+M(35)-M(36)-M(105)+M(108)-M(111)+M(119))+c(6)*(M(139)-M(140)+M(177)-M(178) &
    -M(181)+M(182)-M(237)+M(238))) * den(103)
  T4sum(1:70,36) = T4sum(1:70,36) + Gcoeff * G4tensor(:,4)
  Gcoeff = (c(6)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(248)
  T3sum(1:35,51) = T3sum(1:35,51) + Gcoeff * G3tensor(:,51)
  Gcoeff = (c(6)*(M(137)-M(138)-M(140)+M(142)-M(171)+M(172)+M(177)-M(178)-M(179)+M(180)+M(182)-M(184)+M(223)-M(224)-M(247) &
    +M(248))) * den(761)
  T3sum(1:35,52) = T3sum(1:35,52) + Gcoeff * G3tensor(:,52)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)+M(31)-M(32)-M(35)+M(36)+M(110)-M(112)-M(118)+M(124))+c(6)*(M(137)-M(138)-M(179)+M(180) &
    +M(223)-M(224)-M(247)+M(248))) * den(114)
  T4sum(1:70,12) = T4sum(1:70,12) + Gcoeff * G4tensor(:,1)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)-M(103)+M(105)+M(107)-M(108)-M(109)-M(110)+M(111)+M(112)+M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(141)-M(183)-M(213)+M(214))) * den(33)
  T5sum(1:126,105) = T5sum(1:126,105) + Gcoeff * G5tensor(:,4)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(31)+M(32)+M(35)-M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)-M(103)-M(105)+M(107)+M(108)-M(109)-M(110)-M(111)+M(112)+M(113)+M(118) &
    +M(119)-M(124))+c(6)*(M(138)-M(180)-M(223)+M(224))) * den(33)
  T5sum(1:126,106) = T5sum(1:126,106) + Gcoeff * G5tensor(:,1)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)-M(105)-M(107)+M(108)+M(109)+M(110)-M(111)-M(112)-M(113)-M(118) &
    +M(119)+M(124))+c(6)*(M(139)-M(181)-M(237)+M(238))) * den(33)
  T5sum(1:126,107) = T5sum(1:126,107) + Gcoeff * G5tensor(:,3)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)-M(103)-M(105)+M(107)+M(108)-M(109)+M(110)-M(111)-M(112)+M(113)-M(118) &
    +M(119)+M(124))+c(6)*(M(137)-M(179)-M(247)+M(248))) * den(33)
  T5sum(1:126,108) = T5sum(1:126,108) + Gcoeff * G5tensor(:,2)

end subroutine vamp_17

end module ol_vamp_17_ppjjjj_gggggg_1_/**/REALKIND
