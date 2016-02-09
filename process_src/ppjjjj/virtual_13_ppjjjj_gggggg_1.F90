
module ol_vamp_13_ppjjjj_gggggg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_13(M)
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
  complex(REALKIND), dimension(4,5,4,78) :: G1
  complex(REALKIND), dimension(4,15,4,27) :: G2
  complex(REALKIND), dimension(5,153) :: G1tensor
  complex(REALKIND), dimension(15,51) :: G2tensor
  complex(REALKIND), dimension(35,27) :: G3tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,109),G0(:,:,:,2))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-5),wf(:,-2),G0(:,:,:,3))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,1))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-2),wf(:,-5),G0(:,:,:,4))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,2))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-5),wf(:,-2),G0(:,:,:,5))
  call check_last_UV_W(l_switch,G0(:,:,:,5),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,3))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-5),wf(:,-1),G0(:,:,:,6))
  call check_last_UV_W(l_switch,G0(:,:,:,6),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,4))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-1),wf(:,-5),G0(:,:,:,7))
  call check_last_UV_W(l_switch,G0(:,:,:,7),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,5))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-5),wf(:,-1),G0(:,:,:,8))
  call check_last_UV_W(l_switch,G0(:,:,:,8),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,6))
  call loop_UV_W(G0(:,:,:,2),Q(:,25),wf(:,-5),Q(:,32),G1(:,:,:,1))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,1),wf(:,-2),wf(:,-1),G1tensor(:,7))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,1),wf(:,-1),wf(:,-2),G1tensor(:,8))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,1),wf(:,-2),wf(:,-1),G1tensor(:,9))
  call check_last_UV_W(l_switch,G1(:,:,:,1),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,1))
  call loop_UV_W(G0(:,:,:,2),Q(:,25),wf(:,99),Q(:,34),G1(:,:,:,2))
  call check_last_UV_W(l_switch,G1(:,:,:,2),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,2))
  call loop_UV_W(G0(:,:,:,2),Q(:,25),wf(:,70),Q(:,36),G1(:,:,:,3))
  call check_last_UV_W(l_switch,G1(:,:,:,3),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,3))
  call loop_UV_W(G0(:,:,:,2),Q(:,25),wf(:,-2),Q(:,4),G1(:,:,:,4))
  call loop_UV_W(G1(:,:,:,4),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,1))
  call check_last_UV_W(l_switch,G2(:,:,:,1),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,1))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,109),wf(:,-3),G0(:,:,:,9))
  call loop_GGG_G_12(G0(:,:,:,9),wf(:,-5),wf(:,-2),G0(:,:,:,10))
  call check_last_UV_W(l_switch,G0(:,:,:,10),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,10))
  call loop_GGG_G_12(G0(:,:,:,9),wf(:,-2),wf(:,-5),G0(:,:,:,11))
  call check_last_UV_W(l_switch,G0(:,:,:,11),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,11))
  call loop_GGG_G_23(G0(:,:,:,9),wf(:,-5),wf(:,-2),G0(:,:,:,12))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,12))
  call loop_GGG_G_12(G0(:,:,:,9),wf(:,-5),wf(:,-1),G0(:,:,:,13))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,13))
  call loop_GGG_G_12(G0(:,:,:,9),wf(:,-1),wf(:,-5),G0(:,:,:,14))
  call check_last_UV_W(l_switch,G0(:,:,:,14),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,14))
  call loop_GGG_G_23(G0(:,:,:,9),wf(:,-5),wf(:,-1),G0(:,:,:,15))
  call check_last_UV_W(l_switch,G0(:,:,:,15),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,15))
  call loop_UV_W(G0(:,:,:,9),Q(:,25),wf(:,-5),Q(:,32),G1(:,:,:,5))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,5),wf(:,-2),wf(:,-1),G1tensor(:,16))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,5),wf(:,-1),wf(:,-2),G1tensor(:,17))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,5),wf(:,-2),wf(:,-1),G1tensor(:,18))
  call check_last_UV_W(l_switch,G1(:,:,:,5),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,4))
  call loop_UV_W(G0(:,:,:,9),Q(:,25),wf(:,99),Q(:,34),G1(:,:,:,6))
  call check_last_UV_W(l_switch,G1(:,:,:,6),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,5))
  call loop_UV_W(G0(:,:,:,9),Q(:,25),wf(:,70),Q(:,36),G1(:,:,:,7))
  call check_last_UV_W(l_switch,G1(:,:,:,7),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,6))
  call loop_UV_W(G0(:,:,:,9),Q(:,25),wf(:,-2),Q(:,4),G1(:,:,:,8))
  call loop_UV_W(G1(:,:,:,8),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,2))
  call check_last_UV_W(l_switch,G2(:,:,:,2),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,2))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,109),G0(:,:,:,16))
  call loop_GGG_G_12(G0(:,:,:,16),wf(:,-5),wf(:,-2),G0(:,:,:,17))
  call check_last_UV_W(l_switch,G0(:,:,:,17),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,19))
  call loop_GGG_G_12(G0(:,:,:,16),wf(:,-2),wf(:,-5),G0(:,:,:,18))
  call check_last_UV_W(l_switch,G0(:,:,:,18),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,20))
  call loop_GGG_G_23(G0(:,:,:,16),wf(:,-5),wf(:,-2),G0(:,:,:,19))
  call check_last_UV_W(l_switch,G0(:,:,:,19),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,21))
  call loop_GGG_G_12(G0(:,:,:,16),wf(:,-5),wf(:,-1),G0(:,:,:,20))
  call check_last_UV_W(l_switch,G0(:,:,:,20),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,22))
  call loop_GGG_G_12(G0(:,:,:,16),wf(:,-1),wf(:,-5),G0(:,:,:,21))
  call check_last_UV_W(l_switch,G0(:,:,:,21),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,23))
  call loop_GGG_G_23(G0(:,:,:,16),wf(:,-5),wf(:,-1),G0(:,:,:,22))
  call check_last_UV_W(l_switch,G0(:,:,:,22),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,24))
  call loop_UV_W(G0(:,:,:,16),Q(:,25),wf(:,-5),Q(:,32),G1(:,:,:,9))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,9),wf(:,-2),wf(:,-1),G1tensor(:,25))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,9),wf(:,-1),wf(:,-2),G1tensor(:,26))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,9),wf(:,-2),wf(:,-1),G1tensor(:,27))
  call check_last_UV_W(l_switch,G1(:,:,:,9),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,7))
  call loop_UV_W(G0(:,:,:,16),Q(:,25),wf(:,99),Q(:,34),G1(:,:,:,10))
  call check_last_UV_W(l_switch,G1(:,:,:,10),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,8))
  call loop_UV_W(G0(:,:,:,16),Q(:,25),wf(:,70),Q(:,36),G1(:,:,:,11))
  call check_last_UV_W(l_switch,G1(:,:,:,11),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,9))
  call loop_UV_W(G0(:,:,:,16),Q(:,25),wf(:,-2),Q(:,4),G1(:,:,:,12))
  call loop_UV_W(G1(:,:,:,12),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,3))
  call check_last_UV_W(l_switch,G2(:,:,:,3),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,3))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,109),G0(:,:,:,23))
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
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,109),wf(:,-2),G0(:,:,:,30))
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
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,109),G0(:,:,:,37))
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
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,95),G0(:,:,:,44))
  call loop_GGG_G_12(G0(:,:,:,44),wf(:,-5),wf(:,-2),G0(:,:,:,45))
  call check_last_UV_W(l_switch,G0(:,:,:,45),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,55))
  call loop_GGG_G_12(G0(:,:,:,44),wf(:,-2),wf(:,-5),G0(:,:,:,46))
  call check_last_UV_W(l_switch,G0(:,:,:,46),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,56))
  call loop_GGG_G_23(G0(:,:,:,44),wf(:,-5),wf(:,-2),G0(:,:,:,47))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,57))
  call loop_GGG_G_12(G0(:,:,:,44),wf(:,-5),wf(:,0),G0(:,:,:,48))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,58))
  call loop_GGG_G_12(G0(:,:,:,44),wf(:,0),wf(:,-5),G0(:,:,:,49))
  call check_last_UV_W(l_switch,G0(:,:,:,49),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,59))
  call loop_GGG_G_23(G0(:,:,:,44),wf(:,-5),wf(:,0),G0(:,:,:,50))
  call check_last_UV_W(l_switch,G0(:,:,:,50),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,60))
  call loop_UV_W(G0(:,:,:,44),Q(:,26),wf(:,-5),Q(:,32),G1(:,:,:,25))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,25),wf(:,-2),wf(:,0),G1tensor(:,61))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,25),wf(:,0),wf(:,-2),G1tensor(:,62))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,25),wf(:,-2),wf(:,0),G1tensor(:,63))
  call check_last_UV_W(l_switch,G1(:,:,:,25),Q(:,58),wf(:,90),Q(:,5),G2tensor(:,19))
  call loop_UV_W(G0(:,:,:,44),Q(:,26),wf(:,113),Q(:,33),G1(:,:,:,26))
  call check_last_UV_W(l_switch,G1(:,:,:,26),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,20))
  call loop_UV_W(G0(:,:,:,44),Q(:,26),wf(:,70),Q(:,36),G1(:,:,:,27))
  call check_last_UV_W(l_switch,G1(:,:,:,27),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,21))
  call loop_UV_W(G0(:,:,:,44),Q(:,26),wf(:,-2),Q(:,4),G1(:,:,:,28))
  call loop_UV_W(G1(:,:,:,28),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,7))
  call check_last_UV_W(l_switch,G2(:,:,:,7),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,7))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,95),wf(:,-3),G0(:,:,:,51))
  call loop_GGG_G_12(G0(:,:,:,51),wf(:,-5),wf(:,-2),G0(:,:,:,52))
  call check_last_UV_W(l_switch,G0(:,:,:,52),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,64))
  call loop_GGG_G_12(G0(:,:,:,51),wf(:,-2),wf(:,-5),G0(:,:,:,53))
  call check_last_UV_W(l_switch,G0(:,:,:,53),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,65))
  call loop_GGG_G_23(G0(:,:,:,51),wf(:,-5),wf(:,-2),G0(:,:,:,54))
  call check_last_UV_W(l_switch,G0(:,:,:,54),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,66))
  call loop_GGG_G_12(G0(:,:,:,51),wf(:,-5),wf(:,0),G0(:,:,:,55))
  call check_last_UV_W(l_switch,G0(:,:,:,55),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,67))
  call loop_GGG_G_12(G0(:,:,:,51),wf(:,0),wf(:,-5),G0(:,:,:,56))
  call check_last_UV_W(l_switch,G0(:,:,:,56),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,68))
  call loop_GGG_G_23(G0(:,:,:,51),wf(:,-5),wf(:,0),G0(:,:,:,57))
  call check_last_UV_W(l_switch,G0(:,:,:,57),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,69))
  call loop_UV_W(G0(:,:,:,51),Q(:,26),wf(:,-5),Q(:,32),G1(:,:,:,29))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,29),wf(:,-2),wf(:,0),G1tensor(:,70))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,29),wf(:,0),wf(:,-2),G1tensor(:,71))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,29),wf(:,-2),wf(:,0),G1tensor(:,72))
  call check_last_UV_W(l_switch,G1(:,:,:,29),Q(:,58),wf(:,90),Q(:,5),G2tensor(:,22))
  call loop_UV_W(G0(:,:,:,51),Q(:,26),wf(:,113),Q(:,33),G1(:,:,:,30))
  call check_last_UV_W(l_switch,G1(:,:,:,30),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,23))
  call loop_UV_W(G0(:,:,:,51),Q(:,26),wf(:,70),Q(:,36),G1(:,:,:,31))
  call check_last_UV_W(l_switch,G1(:,:,:,31),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,24))
  call loop_UV_W(G0(:,:,:,51),Q(:,26),wf(:,-2),Q(:,4),G1(:,:,:,32))
  call loop_UV_W(G1(:,:,:,32),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,8))
  call check_last_UV_W(l_switch,G2(:,:,:,8),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,8))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,95),G0(:,:,:,58))
  call loop_GGG_G_12(G0(:,:,:,58),wf(:,-5),wf(:,-2),G0(:,:,:,59))
  call check_last_UV_W(l_switch,G0(:,:,:,59),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,73))
  call loop_GGG_G_12(G0(:,:,:,58),wf(:,-2),wf(:,-5),G0(:,:,:,60))
  call check_last_UV_W(l_switch,G0(:,:,:,60),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,74))
  call loop_GGG_G_23(G0(:,:,:,58),wf(:,-5),wf(:,-2),G0(:,:,:,61))
  call check_last_UV_W(l_switch,G0(:,:,:,61),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,75))
  call loop_GGG_G_12(G0(:,:,:,58),wf(:,-5),wf(:,0),G0(:,:,:,62))
  call check_last_UV_W(l_switch,G0(:,:,:,62),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,76))
  call loop_GGG_G_12(G0(:,:,:,58),wf(:,0),wf(:,-5),G0(:,:,:,63))
  call check_last_UV_W(l_switch,G0(:,:,:,63),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,77))
  call loop_GGG_G_23(G0(:,:,:,58),wf(:,-5),wf(:,0),G0(:,:,:,64))
  call check_last_UV_W(l_switch,G0(:,:,:,64),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,78))
  call loop_UV_W(G0(:,:,:,58),Q(:,26),wf(:,-5),Q(:,32),G1(:,:,:,33))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,33),wf(:,-2),wf(:,0),G1tensor(:,79))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,33),wf(:,0),wf(:,-2),G1tensor(:,80))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,33),wf(:,-2),wf(:,0),G1tensor(:,81))
  call check_last_UV_W(l_switch,G1(:,:,:,33),Q(:,58),wf(:,90),Q(:,5),G2tensor(:,25))
  call loop_UV_W(G0(:,:,:,58),Q(:,26),wf(:,113),Q(:,33),G1(:,:,:,34))
  call check_last_UV_W(l_switch,G1(:,:,:,34),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,26))
  call loop_UV_W(G0(:,:,:,58),Q(:,26),wf(:,70),Q(:,36),G1(:,:,:,35))
  call check_last_UV_W(l_switch,G1(:,:,:,35),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,27))
  call loop_UV_W(G0(:,:,:,58),Q(:,26),wf(:,-2),Q(:,4),G1(:,:,:,36))
  call loop_UV_W(G1(:,:,:,36),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,9))
  call check_last_UV_W(l_switch,G2(:,:,:,9),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,9))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,95),G0(:,:,:,65))
  call loop_GGG_G_12(G0(:,:,:,65),wf(:,-5),wf(:,-3),G0(:,:,:,66))
  call check_last_UV_W(l_switch,G0(:,:,:,66),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,82))
  call loop_GGG_G_12(G0(:,:,:,65),wf(:,-3),wf(:,-5),G0(:,:,:,67))
  call check_last_UV_W(l_switch,G0(:,:,:,67),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,83))
  call loop_GGG_G_23(G0(:,:,:,65),wf(:,-5),wf(:,-3),G0(:,:,:,68))
  call check_last_UV_W(l_switch,G0(:,:,:,68),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,84))
  call loop_GGG_G_12(G0(:,:,:,65),wf(:,-5),wf(:,0),G0(:,:,:,69))
  call check_last_UV_W(l_switch,G0(:,:,:,69),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,85))
  call loop_GGG_G_12(G0(:,:,:,65),wf(:,0),wf(:,-5),G0(:,:,:,70))
  call check_last_UV_W(l_switch,G0(:,:,:,70),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,86))
  call loop_GGG_G_23(G0(:,:,:,65),wf(:,-5),wf(:,0),G0(:,:,:,71))
  call check_last_UV_W(l_switch,G0(:,:,:,71),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,87))
  call loop_UV_W(G0(:,:,:,65),Q(:,22),wf(:,-5),Q(:,32),G1(:,:,:,37))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,37),wf(:,-3),wf(:,0),G1tensor(:,88))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,37),wf(:,0),wf(:,-3),G1tensor(:,89))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,37),wf(:,-3),wf(:,0),G1tensor(:,90))
  call check_last_UV_W(l_switch,G1(:,:,:,37),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,28))
  call loop_UV_W(G0(:,:,:,65),Q(:,22),wf(:,113),Q(:,33),G1(:,:,:,38))
  call check_last_UV_W(l_switch,G1(:,:,:,38),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,29))
  call loop_UV_W(G0(:,:,:,65),Q(:,22),wf(:,79),Q(:,40),G1(:,:,:,39))
  call check_last_UV_W(l_switch,G1(:,:,:,39),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,30))
  call loop_UV_W(G0(:,:,:,65),Q(:,22),wf(:,-3),Q(:,8),G1(:,:,:,40))
  call loop_UV_W(G1(:,:,:,40),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,10))
  call check_last_UV_W(l_switch,G2(:,:,:,10),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,10))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,95),wf(:,-2),G0(:,:,:,72))
  call loop_GGG_G_12(G0(:,:,:,72),wf(:,-5),wf(:,-3),G0(:,:,:,73))
  call check_last_UV_W(l_switch,G0(:,:,:,73),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,91))
  call loop_GGG_G_12(G0(:,:,:,72),wf(:,-3),wf(:,-5),G0(:,:,:,74))
  call check_last_UV_W(l_switch,G0(:,:,:,74),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,92))
  call loop_GGG_G_23(G0(:,:,:,72),wf(:,-5),wf(:,-3),G0(:,:,:,75))
  call check_last_UV_W(l_switch,G0(:,:,:,75),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,93))
  call loop_GGG_G_12(G0(:,:,:,72),wf(:,-5),wf(:,0),G0(:,:,:,76))
  call check_last_UV_W(l_switch,G0(:,:,:,76),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,94))
  call loop_GGG_G_12(G0(:,:,:,72),wf(:,0),wf(:,-5),G0(:,:,:,77))
  call check_last_UV_W(l_switch,G0(:,:,:,77),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,95))
  call loop_GGG_G_23(G0(:,:,:,72),wf(:,-5),wf(:,0),G0(:,:,:,78))
  call check_last_UV_W(l_switch,G0(:,:,:,78),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,96))
  call loop_UV_W(G0(:,:,:,72),Q(:,22),wf(:,-5),Q(:,32),G1(:,:,:,41))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,41),wf(:,-3),wf(:,0),G1tensor(:,97))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,41),wf(:,0),wf(:,-3),G1tensor(:,98))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,41),wf(:,-3),wf(:,0),G1tensor(:,99))
  call check_last_UV_W(l_switch,G1(:,:,:,41),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,31))
  call loop_UV_W(G0(:,:,:,72),Q(:,22),wf(:,113),Q(:,33),G1(:,:,:,42))
  call check_last_UV_W(l_switch,G1(:,:,:,42),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,32))
  call loop_UV_W(G0(:,:,:,72),Q(:,22),wf(:,79),Q(:,40),G1(:,:,:,43))
  call check_last_UV_W(l_switch,G1(:,:,:,43),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,33))
  call loop_UV_W(G0(:,:,:,72),Q(:,22),wf(:,-3),Q(:,8),G1(:,:,:,44))
  call loop_UV_W(G1(:,:,:,44),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,11))
  call check_last_UV_W(l_switch,G2(:,:,:,11),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,11))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,95),G0(:,:,:,79))
  call loop_GGG_G_12(G0(:,:,:,79),wf(:,-5),wf(:,-3),G0(:,:,:,80))
  call check_last_UV_W(l_switch,G0(:,:,:,80),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,100))
  call loop_GGG_G_12(G0(:,:,:,79),wf(:,-3),wf(:,-5),G0(:,:,:,81))
  call check_last_UV_W(l_switch,G0(:,:,:,81),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,101))
  call loop_GGG_G_23(G0(:,:,:,79),wf(:,-5),wf(:,-3),G0(:,:,:,82))
  call check_last_UV_W(l_switch,G0(:,:,:,82),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,102))
  call loop_GGG_G_12(G0(:,:,:,79),wf(:,-5),wf(:,0),G0(:,:,:,83))
  call check_last_UV_W(l_switch,G0(:,:,:,83),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,103))
  call loop_GGG_G_12(G0(:,:,:,79),wf(:,0),wf(:,-5),G0(:,:,:,84))
  call check_last_UV_W(l_switch,G0(:,:,:,84),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,104))
  call loop_GGG_G_23(G0(:,:,:,79),wf(:,-5),wf(:,0),G0(:,:,:,85))
  call check_last_UV_W(l_switch,G0(:,:,:,85),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,105))
  call loop_UV_W(G0(:,:,:,79),Q(:,22),wf(:,-5),Q(:,32),G1(:,:,:,45))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,45),wf(:,-3),wf(:,0),G1tensor(:,106))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,45),wf(:,0),wf(:,-3),G1tensor(:,107))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,45),wf(:,-3),wf(:,0),G1tensor(:,108))
  call check_last_UV_W(l_switch,G1(:,:,:,45),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,34))
  call loop_UV_W(G0(:,:,:,79),Q(:,22),wf(:,113),Q(:,33),G1(:,:,:,46))
  call check_last_UV_W(l_switch,G1(:,:,:,46),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,35))
  call loop_UV_W(G0(:,:,:,79),Q(:,22),wf(:,79),Q(:,40),G1(:,:,:,47))
  call check_last_UV_W(l_switch,G1(:,:,:,47),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,36))
  call loop_UV_W(G0(:,:,:,79),Q(:,22),wf(:,-3),Q(:,8),G1(:,:,:,48))
  call loop_UV_W(G1(:,:,:,48),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,12))
  call check_last_UV_W(l_switch,G2(:,:,:,12),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,12))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,104),G0(:,:,:,86))
  call loop_GGG_G_12(G0(:,:,:,86),wf(:,-5),wf(:,-4),G0(:,:,:,87))
  call check_last_UV_W(l_switch,G0(:,:,:,87),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,109))
  call loop_GGG_G_12(G0(:,:,:,86),wf(:,-4),wf(:,-5),G0(:,:,:,88))
  call check_last_UV_W(l_switch,G0(:,:,:,88),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,110))
  call loop_GGG_G_23(G0(:,:,:,86),wf(:,-5),wf(:,-4),G0(:,:,:,89))
  call check_last_UV_W(l_switch,G0(:,:,:,89),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,111))
  call loop_UV_W(G0(:,:,:,86),Q(:,11),wf(:,84),Q(:,48),G1(:,:,:,49))
  call check_last_UV_W(l_switch,G1(:,:,:,49),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,37))
  call loop_UV_W(G0(:,:,:,86),Q(:,11),wf(:,-5),Q(:,32),G1(:,:,:,50))
  call loop_UV_W(G1(:,:,:,50),Q(:,43),wf(:,-4),Q(:,16),G2(:,:,:,13))
  call check_last_UV_W(l_switch,G2(:,:,:,13),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,13))
  call loop_UV_W(G0(:,:,:,86),Q(:,11),wf(:,-4),Q(:,16),G1(:,:,:,51))
  call loop_UV_W(G1(:,:,:,51),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,14))
  call check_last_UV_W(l_switch,G2(:,:,:,14),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,14))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,104),wf(:,-1),G0(:,:,:,90))
  call loop_GGG_G_12(G0(:,:,:,90),wf(:,-5),wf(:,-4),G0(:,:,:,91))
  call check_last_UV_W(l_switch,G0(:,:,:,91),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,112))
  call loop_GGG_G_12(G0(:,:,:,90),wf(:,-4),wf(:,-5),G0(:,:,:,92))
  call check_last_UV_W(l_switch,G0(:,:,:,92),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,113))
  call loop_GGG_G_23(G0(:,:,:,90),wf(:,-5),wf(:,-4),G0(:,:,:,93))
  call check_last_UV_W(l_switch,G0(:,:,:,93),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,114))
  call loop_UV_W(G0(:,:,:,90),Q(:,11),wf(:,84),Q(:,48),G1(:,:,:,52))
  call check_last_UV_W(l_switch,G1(:,:,:,52),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,38))
  call loop_UV_W(G0(:,:,:,90),Q(:,11),wf(:,-5),Q(:,32),G1(:,:,:,53))
  call loop_UV_W(G1(:,:,:,53),Q(:,43),wf(:,-4),Q(:,16),G2(:,:,:,15))
  call check_last_UV_W(l_switch,G2(:,:,:,15),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,15))
  call loop_UV_W(G0(:,:,:,90),Q(:,11),wf(:,-4),Q(:,16),G1(:,:,:,54))
  call loop_UV_W(G1(:,:,:,54),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,16))
  call check_last_UV_W(l_switch,G2(:,:,:,16),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,16))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,104),G0(:,:,:,94))
  call loop_GGG_G_12(G0(:,:,:,94),wf(:,-5),wf(:,-4),G0(:,:,:,95))
  call check_last_UV_W(l_switch,G0(:,:,:,95),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,115))
  call loop_GGG_G_12(G0(:,:,:,94),wf(:,-4),wf(:,-5),G0(:,:,:,96))
  call check_last_UV_W(l_switch,G0(:,:,:,96),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,116))
  call loop_GGG_G_23(G0(:,:,:,94),wf(:,-5),wf(:,-4),G0(:,:,:,97))
  call check_last_UV_W(l_switch,G0(:,:,:,97),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,117))
  call loop_UV_W(G0(:,:,:,94),Q(:,11),wf(:,84),Q(:,48),G1(:,:,:,55))
  call check_last_UV_W(l_switch,G1(:,:,:,55),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,39))
  call loop_UV_W(G0(:,:,:,94),Q(:,11),wf(:,-5),Q(:,32),G1(:,:,:,56))
  call loop_UV_W(G1(:,:,:,56),Q(:,43),wf(:,-4),Q(:,16),G2(:,:,:,17))
  call check_last_UV_W(l_switch,G2(:,:,:,17),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,17))
  call loop_UV_W(G0(:,:,:,94),Q(:,11),wf(:,-4),Q(:,16),G1(:,:,:,57))
  call loop_UV_W(G1(:,:,:,57),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,18))
  call check_last_UV_W(l_switch,G2(:,:,:,18),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,18))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,62),G0(:,:,:,98))
  call loop_GGG_G_12(G0(:,:,:,98),wf(:,-5),wf(:,-1),G0(:,:,:,99))
  call check_last_UV_W(l_switch,G0(:,:,:,99),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,118))
  call loop_GGG_G_12(G0(:,:,:,98),wf(:,-1),wf(:,-5),G0(:,:,:,100))
  call check_last_UV_W(l_switch,G0(:,:,:,100),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,119))
  call loop_GGG_G_23(G0(:,:,:,98),wf(:,-5),wf(:,-1),G0(:,:,:,101))
  call check_last_UV_W(l_switch,G0(:,:,:,101),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,120))
  call loop_GGG_G_12(G0(:,:,:,98),wf(:,-5),wf(:,0),G0(:,:,:,102))
  call check_last_UV_W(l_switch,G0(:,:,:,102),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,121))
  call loop_GGG_G_12(G0(:,:,:,98),wf(:,0),wf(:,-5),G0(:,:,:,103))
  call check_last_UV_W(l_switch,G0(:,:,:,103),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,122))
  call loop_GGG_G_23(G0(:,:,:,98),wf(:,-5),wf(:,0),G0(:,:,:,104))
  call check_last_UV_W(l_switch,G0(:,:,:,104),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,123))
  call loop_UV_W(G0(:,:,:,98),Q(:,28),wf(:,-5),Q(:,32),G1(:,:,:,58))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,58),wf(:,-1),wf(:,0),G1tensor(:,124))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,58),wf(:,0),wf(:,-1),G1tensor(:,125))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,58),wf(:,-1),wf(:,0),G1tensor(:,126))
  call check_last_UV_W(l_switch,G1(:,:,:,58),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,40))
  call loop_UV_W(G0(:,:,:,98),Q(:,28),wf(:,113),Q(:,33),G1(:,:,:,59))
  call check_last_UV_W(l_switch,G1(:,:,:,59),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,41))
  call loop_UV_W(G0(:,:,:,98),Q(:,28),wf(:,99),Q(:,34),G1(:,:,:,60))
  call check_last_UV_W(l_switch,G1(:,:,:,60),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,42))
  call loop_UV_W(G0(:,:,:,98),Q(:,28),wf(:,-1),Q(:,2),G1(:,:,:,61))
  call loop_UV_W(G1(:,:,:,61),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,19))
  call check_last_UV_W(l_switch,G2(:,:,:,19),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,19))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,62),wf(:,-4),G0(:,:,:,105))
  call loop_GGG_G_12(G0(:,:,:,105),wf(:,-5),wf(:,-1),G0(:,:,:,106))
  call check_last_UV_W(l_switch,G0(:,:,:,106),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,127))
  call loop_GGG_G_12(G0(:,:,:,105),wf(:,-1),wf(:,-5),G0(:,:,:,107))
  call check_last_UV_W(l_switch,G0(:,:,:,107),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,128))
  call loop_GGG_G_23(G0(:,:,:,105),wf(:,-5),wf(:,-1),G0(:,:,:,108))
  call check_last_UV_W(l_switch,G0(:,:,:,108),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,129))
  call loop_GGG_G_12(G0(:,:,:,105),wf(:,-5),wf(:,0),G0(:,:,:,109))
  call check_last_UV_W(l_switch,G0(:,:,:,109),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,130))
  call loop_GGG_G_12(G0(:,:,:,105),wf(:,0),wf(:,-5),G0(:,:,:,110))
  call check_last_UV_W(l_switch,G0(:,:,:,110),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,131))
  call loop_GGG_G_23(G0(:,:,:,105),wf(:,-5),wf(:,0),G0(:,:,:,111))
  call check_last_UV_W(l_switch,G0(:,:,:,111),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,132))
  call loop_UV_W(G0(:,:,:,105),Q(:,28),wf(:,-5),Q(:,32),G1(:,:,:,62))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,62),wf(:,-1),wf(:,0),G1tensor(:,133))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,62),wf(:,0),wf(:,-1),G1tensor(:,134))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,62),wf(:,-1),wf(:,0),G1tensor(:,135))
  call check_last_UV_W(l_switch,G1(:,:,:,62),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,43))
  call loop_UV_W(G0(:,:,:,105),Q(:,28),wf(:,113),Q(:,33),G1(:,:,:,63))
  call check_last_UV_W(l_switch,G1(:,:,:,63),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,44))
  call loop_UV_W(G0(:,:,:,105),Q(:,28),wf(:,99),Q(:,34),G1(:,:,:,64))
  call check_last_UV_W(l_switch,G1(:,:,:,64),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,45))
  call loop_UV_W(G0(:,:,:,105),Q(:,28),wf(:,-1),Q(:,2),G1(:,:,:,65))
  call loop_UV_W(G1(:,:,:,65),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,20))
  call check_last_UV_W(l_switch,G2(:,:,:,20),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,20))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,62),G0(:,:,:,112))
  call loop_GGG_G_12(G0(:,:,:,112),wf(:,-5),wf(:,-1),G0(:,:,:,113))
  call check_last_UV_W(l_switch,G0(:,:,:,113),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,136))
  call loop_GGG_G_12(G0(:,:,:,112),wf(:,-1),wf(:,-5),G0(:,:,:,114))
  call check_last_UV_W(l_switch,G0(:,:,:,114),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,137))
  call loop_GGG_G_23(G0(:,:,:,112),wf(:,-5),wf(:,-1),G0(:,:,:,115))
  call check_last_UV_W(l_switch,G0(:,:,:,115),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,138))
  call loop_GGG_G_12(G0(:,:,:,112),wf(:,-5),wf(:,0),G0(:,:,:,116))
  call check_last_UV_W(l_switch,G0(:,:,:,116),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,139))
  call loop_GGG_G_12(G0(:,:,:,112),wf(:,0),wf(:,-5),G0(:,:,:,117))
  call check_last_UV_W(l_switch,G0(:,:,:,117),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,140))
  call loop_GGG_G_23(G0(:,:,:,112),wf(:,-5),wf(:,0),G0(:,:,:,118))
  call check_last_UV_W(l_switch,G0(:,:,:,118),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,141))
  call loop_UV_W(G0(:,:,:,112),Q(:,28),wf(:,-5),Q(:,32),G1(:,:,:,66))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,66),wf(:,-1),wf(:,0),G1tensor(:,142))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,66),wf(:,0),wf(:,-1),G1tensor(:,143))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,66),wf(:,-1),wf(:,0),G1tensor(:,144))
  call check_last_UV_W(l_switch,G1(:,:,:,66),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,46))
  call loop_UV_W(G0(:,:,:,112),Q(:,28),wf(:,113),Q(:,33),G1(:,:,:,67))
  call check_last_UV_W(l_switch,G1(:,:,:,67),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,47))
  call loop_UV_W(G0(:,:,:,112),Q(:,28),wf(:,99),Q(:,34),G1(:,:,:,68))
  call check_last_UV_W(l_switch,G1(:,:,:,68),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,48))
  call loop_UV_W(G0(:,:,:,112),Q(:,28),wf(:,-1),Q(:,2),G1(:,:,:,69))
  call loop_UV_W(G1(:,:,:,69),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,21))
  call check_last_UV_W(l_switch,G2(:,:,:,21),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,21))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,62),G0(:,:,:,119))
  call loop_GGG_G_12(G0(:,:,:,119),wf(:,-5),wf(:,-4),G0(:,:,:,120))
  call check_last_UV_W(l_switch,G0(:,:,:,120),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,145))
  call loop_GGG_G_12(G0(:,:,:,119),wf(:,-4),wf(:,-5),G0(:,:,:,121))
  call check_last_UV_W(l_switch,G0(:,:,:,121),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,146))
  call loop_GGG_G_23(G0(:,:,:,119),wf(:,-5),wf(:,-4),G0(:,:,:,122))
  call check_last_UV_W(l_switch,G0(:,:,:,122),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,147))
  call loop_UV_W(G0(:,:,:,119),Q(:,14),wf(:,84),Q(:,48),G1(:,:,:,70))
  call check_last_UV_W(l_switch,G1(:,:,:,70),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,49))
  call loop_UV_W(G0(:,:,:,119),Q(:,14),wf(:,-5),Q(:,32),G1(:,:,:,71))
  call loop_UV_W(G1(:,:,:,71),Q(:,46),wf(:,-4),Q(:,16),G2(:,:,:,22))
  call check_last_UV_W(l_switch,G2(:,:,:,22),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,22))
  call loop_UV_W(G0(:,:,:,119),Q(:,14),wf(:,-4),Q(:,16),G1(:,:,:,72))
  call loop_UV_W(G1(:,:,:,72),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,23))
  call check_last_UV_W(l_switch,G2(:,:,:,23),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,23))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,62),wf(:,-1),G0(:,:,:,123))
  call loop_GGG_G_12(G0(:,:,:,123),wf(:,-5),wf(:,-4),G0(:,:,:,124))
  call check_last_UV_W(l_switch,G0(:,:,:,124),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,148))
  call loop_GGG_G_12(G0(:,:,:,123),wf(:,-4),wf(:,-5),G0(:,:,:,125))
  call check_last_UV_W(l_switch,G0(:,:,:,125),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,149))
  call loop_GGG_G_23(G0(:,:,:,123),wf(:,-5),wf(:,-4),G0(:,:,:,126))
  call check_last_UV_W(l_switch,G0(:,:,:,126),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,150))
  call loop_UV_W(G0(:,:,:,123),Q(:,14),wf(:,84),Q(:,48),G1(:,:,:,73))
  call check_last_UV_W(l_switch,G1(:,:,:,73),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,50))
  call loop_UV_W(G0(:,:,:,123),Q(:,14),wf(:,-5),Q(:,32),G1(:,:,:,74))
  call loop_UV_W(G1(:,:,:,74),Q(:,46),wf(:,-4),Q(:,16),G2(:,:,:,24))
  call check_last_UV_W(l_switch,G2(:,:,:,24),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,24))
  call loop_UV_W(G0(:,:,:,123),Q(:,14),wf(:,-4),Q(:,16),G1(:,:,:,75))
  call loop_UV_W(G1(:,:,:,75),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,25))
  call check_last_UV_W(l_switch,G2(:,:,:,25),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,25))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,62),G0(:,:,:,127))
  call loop_GGG_G_12(G0(:,:,:,127),wf(:,-5),wf(:,-4),G0(:,:,:,128))
  call check_last_UV_W(l_switch,G0(:,:,:,128),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,151))
  call loop_GGG_G_12(G0(:,:,:,127),wf(:,-4),wf(:,-5),G0(:,:,:,129))
  call check_last_UV_W(l_switch,G0(:,:,:,129),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,152))
  call loop_GGG_G_23(G0(:,:,:,127),wf(:,-5),wf(:,-4),G0(:,:,:,130))
  call check_last_UV_W(l_switch,G0(:,:,:,130),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,153))
  call loop_UV_W(G0(:,:,:,127),Q(:,14),wf(:,84),Q(:,48),G1(:,:,:,76))
  call check_last_UV_W(l_switch,G1(:,:,:,76),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,51))
  call loop_UV_W(G0(:,:,:,127),Q(:,14),wf(:,-5),Q(:,32),G1(:,:,:,77))
  call loop_UV_W(G1(:,:,:,77),Q(:,46),wf(:,-4),Q(:,16),G2(:,:,:,26))
  call check_last_UV_W(l_switch,G2(:,:,:,26),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,26))
  call loop_UV_W(G0(:,:,:,127),Q(:,14),wf(:,-4),Q(:,16),G1(:,:,:,78))
  call loop_UV_W(G1(:,:,:,78),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,27))
  call check_last_UV_W(l_switch,G2(:,:,:,27),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,27))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(4)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)-M(11)+M(12)-M(19)+M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(42)-M(45)+M(52) &
    -M(54)-M(57)+M(64)+M(66)+M(69)+M(76)-M(77)+M(78)-M(79)+M(80)+M(81)-M(82)-M(83)-M(84)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94) &
    -M(97))+c(6)*(M(180)-M(215)+M(223)-M(233))) * den(43)
  T3sum(1:5,35) = T3sum(1:5,35) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(5)+M(6)+M(7)-M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)+M(42)-M(45)+M(52) &
    -M(54)-M(57)-M(64)+M(66)+M(69)+M(76)-M(77)+M(78)-M(79)+M(80)+M(81)+M(82)-M(83)-M(84)-M(85)+M(86)-M(87)+M(88)-M(91)+M(94) &
    -M(97))+c(6)*(-M(173)+M(183)+M(213)-M(216))) * den(43)
  T3sum(1:5,35) = T3sum(1:5,35) + Gcoeff * G1tensor(:,2)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(6)*(-M(173)-M(180)+M(183)+M(213) &
    +M(215)-M(216)-M(223)+M(233))) * den(43)
  T3sum(1:5,35) = T3sum(1:5,35) + Gcoeff * G1tensor(:,3)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)-M(11)+M(12)-M(19)+M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(42)+M(45)+M(52) &
    -M(54)+M(57)+M(64)-M(66)-M(69)-M(76)-M(77)-M(78)-M(79)+M(80)-M(81)-M(82)+M(83)+M(84)+M(85)+M(86)+M(87)-M(88)-M(91)-M(94) &
    +M(97))+c(6)*(M(135)-M(199)-M(204)+M(220))) * den(43)
  T3sum(1:5,35) = T3sum(1:5,35) + Gcoeff * G1tensor(:,10)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(5)+M(6)+M(7)-M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)+M(42)+M(45)+M(52) &
    -M(54)+M(57)-M(64)-M(66)-M(69)-M(76)-M(77)-M(78)-M(79)+M(80)-M(81)+M(82)+M(83)+M(84)+M(85)+M(86)-M(87)-M(88)-M(91)+M(94) &
    +M(97))+c(6)*(M(149)-M(189)-M(207)+M(218))) * den(43)
  T3sum(1:5,35) = T3sum(1:5,35) + Gcoeff * G1tensor(:,11)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(6)*(-M(135)+M(149)-M(189)+M(199) &
    +M(204)-M(207)+M(218)-M(220))) * den(43)
  T3sum(1:5,35) = T3sum(1:5,35) + Gcoeff * G1tensor(:,12)
  Gcoeff = (c(5)*(M(45)+M(57)-M(66)-M(69)-M(76)-M(78)-M(81)+M(83)+M(84)+M(85)-M(88)+M(97))+c(6)*(M(135)-M(180)-M(199)-M(204) &
    +M(215)+M(220)-M(223)+M(233))) * den(43)
  T3sum(1:5,35) = T3sum(1:5,35) + Gcoeff * G1tensor(:,19)
  Gcoeff = (c(5)*(M(45)+M(57)-M(66)-M(69)-M(76)-M(78)-M(81)+M(83)+M(84)+M(85)-M(88)+M(97))+c(6)*(M(149)+M(173)-M(183)-M(189) &
    -M(207)-M(213)+M(216)+M(218))) * den(43)
  T3sum(1:5,35) = T3sum(1:5,35) + Gcoeff * G1tensor(:,20)
  Gcoeff = (c(6)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(43)
  T3sum(1:5,35) = T3sum(1:5,35) + Gcoeff * G1tensor(:,21)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(11)-M(12)+M(19)-M(20)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(42)+M(45)+M(52) &
    +M(54)+M(57)+M(64)-M(66)-M(69)+M(76)+M(77)-M(78)-M(79)-M(80)-M(81)-M(82)+M(83)+M(84)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94) &
    -M(97))+c(6)*(M(156)-M(209)+M(225)-M(239))) * den(43)
  T3sum(1:5,29) = T3sum(1:5,29) + Gcoeff * G1tensor(:,28)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(11)-M(12)+M(19)-M(20)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(42)+M(45)+M(52) &
    -M(54)-M(57)-M(64)-M(66)+M(69)+M(76)-M(77)-M(78)-M(79)+M(80)+M(81)+M(82)+M(83)-M(84)-M(85)+M(86)-M(87)+M(88)-M(91)+M(94) &
    -M(97))+c(6)*(M(141)-M(175)-M(206)+M(214))) * den(43)
  T3sum(1:5,29) = T3sum(1:5,29) + Gcoeff * G1tensor(:,37)
  Gcoeff = (c(5)*(M(42)-M(54)-M(57)-M(64)+M(69)-M(77)+M(80)+M(81)+M(82)-M(84)-M(87)+M(94))+c(6)*(M(141)-M(156)-M(175)-M(206) &
    +M(209)+M(214)-M(225)+M(239))) * den(43)
  T3sum(1:5,29) = T3sum(1:5,29) + Gcoeff * G1tensor(:,46)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(42)+M(45)+M(52) &
    +M(54)+M(57)+M(64)-M(66)-M(69)-M(76)+M(77)-M(78)-M(79)-M(80)-M(81)-M(82)+M(83)+M(84)+M(85)+M(86)+M(87)-M(88)-M(91)-M(94) &
    +M(97))+c(6)*(M(159)-M(197)-M(210)+M(219))) * den(43)
  T3sum(1:5,29) = T3sum(1:5,29) + Gcoeff * G1tensor(:,29)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(42)+M(45)+M(52) &
    -M(54)-M(57)-M(64)-M(66)+M(69)-M(76)-M(77)-M(78)-M(79)+M(80)+M(81)+M(82)+M(83)-M(84)+M(85)+M(86)-M(87)-M(88)-M(91)+M(94) &
    +M(97))+c(6)*(M(151)-M(165)-M(208)+M(212))) * den(43)
  T3sum(1:5,29) = T3sum(1:5,29) + Gcoeff * G1tensor(:,38)
  Gcoeff = (c(5)*(M(42)-M(54)-M(57)-M(64)+M(69)-M(77)+M(80)+M(81)+M(82)-M(84)-M(87)+M(94))+c(6)*(M(151)-M(159)-M(165)+M(197) &
    -M(208)+M(210)+M(212)-M(219))) * den(43)
  T3sum(1:5,29) = T3sum(1:5,29) + Gcoeff * G1tensor(:,47)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(76)+M(85)-M(88)+M(97))+c(6)*(-M(156)+M(159)-M(197)+M(209) &
    -M(210)+M(219)-M(225)+M(239))) * den(43)
  T3sum(1:5,29) = T3sum(1:5,29) + Gcoeff * G1tensor(:,30)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(76)+M(85)-M(88)+M(97))+c(6)*(-M(141)+M(151)-M(165)+M(175) &
    +M(206)-M(208)+M(212)-M(214))) * den(43)
  T3sum(1:5,29) = T3sum(1:5,29) + Gcoeff * G1tensor(:,39)
  Gcoeff = (c(6)*(-M(141)+M(151)+M(156)-M(159)-M(165)+M(175)+M(197)+M(206)-M(208)-M(209)+M(210)+M(212)-M(214)-M(219)+M(225) &
    -M(239))) * den(43)
  T3sum(1:5,29) = T3sum(1:5,29) + Gcoeff * G1tensor(:,48)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)-M(17)+M(18)+M(23)-M(24)+M(27)-M(28)-M(33)+M(34)+M(47)-M(48)+M(49) &
    -M(54)+M(59)+M(66)-M(71)-M(77)+M(78)-M(79)-M(91)+M(98)-M(101)+M(106)+M(107)+M(112)+M(113)-M(114)-M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(-M(174)+M(177)+M(182)-M(192))) * den(35)
  T3sum(1:5,37) = T3sum(1:5,37) + Gcoeff * G1tensor(:,55)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)-M(17)+M(18)+M(23)-M(24)-M(27)+M(28)+M(33)-M(34)+M(47)-M(48)+M(49) &
    -M(54)+M(59)+M(66)-M(71)-M(77)+M(78)-M(79)-M(91)+M(98)-M(101)-M(106)+M(107)+M(112)+M(113)+M(114)-M(115)-M(116)-M(117)+M(118) &
    +M(120)-M(122))+c(6)*(M(181)-M(191)-M(234)+M(237))) * den(35)
  T3sum(1:5,37) = T3sum(1:5,37) + Gcoeff * G1tensor(:,56)
  Gcoeff = (c(5)*(-M(9)+M(10)+M(15)-M(16)-M(27)+M(28)+M(33)-M(34)-M(106)+M(114)-M(117)+M(120))+c(6)*(M(174)-M(177)+M(181)-M(182) &
    -M(191)+M(192)-M(234)+M(237))) * den(35)
  T3sum(1:5,37) = T3sum(1:5,37) + Gcoeff * G1tensor(:,57)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)-M(17)+M(18)+M(23)-M(24)+M(27)-M(28)-M(33)+M(34)+M(47)+M(48)+M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98)+M(101)+M(106)-M(107)-M(112)-M(113)-M(114)+M(115)+M(116)+M(117)-M(118) &
    -M(120)+M(122))+c(6)*(M(146)-M(175)+M(176)-M(206))) * den(35)
  T3sum(1:5,37) = T3sum(1:5,37) + Gcoeff * G1tensor(:,64)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)-M(17)+M(18)+M(23)-M(24)-M(27)+M(28)+M(33)-M(34)+M(47)+M(48)+M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98)+M(101)-M(106)-M(107)-M(112)-M(113)+M(114)+M(115)+M(116)-M(117)-M(118) &
    +M(120)+M(122))+c(6)*(M(145)-M(205)-M(235)+M(236))) * den(35)
  T3sum(1:5,37) = T3sum(1:5,37) + Gcoeff * G1tensor(:,65)
  Gcoeff = (c(5)*(-M(9)+M(10)+M(15)-M(16)-M(27)+M(28)+M(33)-M(34)-M(106)+M(114)-M(117)+M(120))+c(6)*(M(145)-M(146)+M(175)-M(176) &
    -M(205)+M(206)-M(235)+M(236))) * den(35)
  T3sum(1:5,37) = T3sum(1:5,37) + Gcoeff * G1tensor(:,66)
  Gcoeff = (c(5)*(M(48)-M(66)+M(71)-M(78)+M(101)-M(107)-M(112)-M(113)+M(115)+M(116)-M(118)+M(122))+c(6)*(M(146)+M(174)-M(175) &
    +M(176)-M(177)-M(182)+M(192)-M(206))) * den(35)
  T3sum(1:5,37) = T3sum(1:5,37) + Gcoeff * G1tensor(:,73)
  Gcoeff = (c(5)*(M(48)-M(66)+M(71)-M(78)+M(101)-M(107)-M(112)-M(113)+M(115)+M(116)-M(118)+M(122))+c(6)*(M(145)-M(181)+M(191) &
    -M(205)+M(234)-M(235)+M(236)-M(237))) * den(35)
  T3sum(1:5,37) = T3sum(1:5,37) + Gcoeff * G1tensor(:,74)
  Gcoeff = (c(6)*(M(145)-M(146)-M(174)+M(175)-M(176)+M(177)-M(181)+M(182)+M(191)-M(192)-M(205)+M(206)+M(234)-M(235)+M(236) &
    -M(237))) * den(35)
  T3sum(1:5,37) = T3sum(1:5,37) + Gcoeff * G1tensor(:,75)
  Gcoeff = (c(4)*(M(3)-M(4)-M(9)+M(10)-M(13)+M(14)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)-M(27)+M(28)+M(33)-M(34)-M(47)+M(48)+M(49) &
    +M(54)-M(59)-M(66)+M(71)+M(77)-M(78)-M(79)-M(91)+M(98)+M(101)+M(106)-M(107)+M(112)-M(113)-M(114)+M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(M(158)-M(168)-M(198)+M(201))) * den(35)
  T3sum(1:5,32) = T3sum(1:5,32) + Gcoeff * G1tensor(:,82)
  Gcoeff = (c(4)*(M(3)-M(4)-M(9)+M(10)-M(13)+M(14)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)-M(27)+M(28)+M(33)-M(34)+M(47)+M(48)+M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98)-M(101)-M(106)+M(107)+M(112)+M(113)+M(114)-M(115)-M(116)-M(117)+M(118) &
    +M(120)-M(122))+c(6)*(M(144)-M(199)+M(200)-M(204))) * den(35)
  T3sum(1:5,32) = T3sum(1:5,32) + Gcoeff * G1tensor(:,91)
  Gcoeff = (c(5)*(M(47)-M(54)+M(59)-M(77)-M(101)-M(106)+M(107)+M(113)+M(114)-M(115)-M(117)+M(120))+c(6)*(M(144)-M(158)+M(168) &
    +M(198)-M(199)+M(200)-M(201)-M(204))) * den(35)
  T3sum(1:5,32) = T3sum(1:5,32) + Gcoeff * G1tensor(:,100)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)-M(47)+M(48)+M(49) &
    +M(54)-M(59)-M(66)+M(71)+M(77)-M(78)-M(79)-M(91)+M(98)+M(101)+M(106)-M(107)-M(112)-M(113)-M(114)+M(115)+M(116)+M(117)-M(118) &
    -M(120)+M(122))+c(6)*(M(157)-M(167)-M(240)+M(243))) * den(35)
  T3sum(1:5,32) = T3sum(1:5,32) + Gcoeff * G1tensor(:,83)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)+M(47)+M(48)+M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98)-M(101)-M(106)+M(107)-M(112)+M(113)+M(114)-M(115)+M(116)-M(117)-M(118) &
    +M(120)+M(122))+c(6)*(M(143)-M(203)-M(241)+M(242))) * den(35)
  T3sum(1:5,32) = T3sum(1:5,32) + Gcoeff * G1tensor(:,92)
  Gcoeff = (c(5)*(M(47)-M(54)+M(59)-M(77)-M(101)-M(106)+M(107)+M(113)+M(114)-M(115)-M(117)+M(120))+c(6)*(M(143)-M(157)+M(167) &
    -M(203)+M(240)-M(241)+M(242)-M(243))) * den(35)
  T3sum(1:5,32) = T3sum(1:5,32) + Gcoeff * G1tensor(:,101)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(27)-M(28)-M(33)+M(34)-M(112)+M(116)-M(118)+M(122))+c(6)*(M(157)-M(158)-M(167)+M(168) &
    +M(198)-M(201)-M(240)+M(243))) * den(35)
  T3sum(1:5,32) = T3sum(1:5,32) + Gcoeff * G1tensor(:,84)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(27)-M(28)-M(33)+M(34)-M(112)+M(116)-M(118)+M(122))+c(6)*(M(143)-M(144)+M(199)-M(200) &
    -M(203)+M(204)-M(241)+M(242))) * den(35)
  T3sum(1:5,32) = T3sum(1:5,32) + Gcoeff * G1tensor(:,93)
  Gcoeff = (c(6)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(35)
  T3sum(1:5,32) = T3sum(1:5,32) + Gcoeff * G1tensor(:,102)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)+M(63)-M(65)-M(66)-M(67)+M(68)-M(69)-M(70)+M(71)+M(72)-M(73)+M(74)+M(75)+M(76)-M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(134)-M(179)+M(202)-M(247))) * den(40)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,109)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(41)-M(48)-M(51) &
    +M(53)+M(60)+M(63)+M(65)+M(66)+M(67)-M(68)-M(69)-M(70)-M(71)+M(72)-M(73)-M(74)+M(75)+M(76)+M(78)-M(81)+M(88)+M(90)-M(93) &
    -M(100))+c(6)*(-M(153)+M(171)+M(184)-M(188))) * den(40)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,112)
  Gcoeff = (c(5)*(-M(41)-M(48)-M(51)+M(53)+M(65)+M(66)+M(67)-M(68)-M(71)-M(74)+M(78)+M(90))+c(6)*(-M(134)-M(153)+M(171)+M(179) &
    +M(184)-M(188)-M(202)+M(247))) * den(40)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,115)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)-M(17)+M(18)+M(19)-M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)+M(63)-M(65)-M(66)-M(67)+M(68)-M(69)-M(70)+M(71)+M(72)+M(73)+M(74)+M(75)-M(76)-M(78)-M(81)-M(88)-M(90)-M(93) &
    +M(100))+c(6)*(M(136)-M(180)+M(196)-M(223))) * den(40)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,110)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)-M(17)+M(18)+M(19)-M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)-M(48)-M(51) &
    +M(53)+M(60)+M(63)+M(65)+M(66)+M(67)-M(68)-M(69)-M(70)-M(71)+M(72)+M(73)-M(74)+M(75)-M(76)+M(78)-M(81)-M(88)+M(90)-M(93) &
    +M(100))+c(6)*(-M(147)+M(177)+M(182)-M(190))) * den(40)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,113)
  Gcoeff = (c(5)*(-M(41)-M(48)-M(51)+M(53)+M(65)+M(66)+M(67)-M(68)-M(71)-M(74)+M(78)+M(90))+c(6)*(-M(136)-M(147)+M(177)+M(180) &
    +M(182)-M(190)-M(196)+M(223))) * den(40)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,116)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(134)+M(136)+M(179)-M(180) &
    +M(196)-M(202)-M(223)+M(247))) * den(40)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,111)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(147)+M(153)-M(171)+M(177) &
    +M(182)-M(184)+M(188)-M(190))) * den(40)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,114)
  Gcoeff = (c(6)*(M(134)-M(136)-M(147)+M(153)-M(171)+M(177)-M(179)+M(180)+M(182)-M(184)+M(188)-M(190)-M(196)+M(202)+M(223) &
    -M(247))) * den(40)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,117)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    -M(57)+M(58)-M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(149)+M(151)+M(212)-M(218))) * den(12)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,118)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(27)+M(28)-M(29)+M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    -M(57)+M(58)-M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(211)-M(217)-M(227)+M(229))) * den(12)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,119)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(102)+M(108)+M(119)-M(121))+c(6)*(M(149)-M(151)+M(211)-M(212) &
    -M(217)+M(218)-M(227)+M(229))) * den(12)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,120)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    +M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(153)+M(154)+M(164)-M(188))) * den(12)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,127)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(27)+M(28)-M(29)+M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    +M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)-M(102)-M(107)+M(108)-M(113)+M(115)+M(119)-M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(163)-M(187)-M(231)+M(232))) * den(12)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,128)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(102)+M(108)+M(119)-M(121))+c(6)*(M(153)-M(154)+M(163)-M(164) &
    -M(187)+M(188)-M(231)+M(232))) * den(12)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,129)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(149)-M(151)-M(153) &
    +M(154)+M(164)-M(188)-M(212)+M(218))) * den(12)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,136)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(163)-M(187)-M(211) &
    +M(217)+M(227)-M(229)-M(231)+M(232))) * den(12)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,137)
  Gcoeff = (c(6)*(-M(149)+M(151)+M(153)-M(154)+M(163)-M(164)-M(187)+M(188)-M(211)+M(212)+M(217)-M(218)+M(227)-M(229)-M(231) &
    +M(232))) * den(12)
  T3sum(1:5,50) = T3sum(1:5,50) + Gcoeff * G1tensor(:,138)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)+M(58)+M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(132)-M(138)-M(224)+M(226))) * den(12)
  T3sum(1:5,33) = T3sum(1:5,33) + Gcoeff * G1tensor(:,145)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)-M(41)+M(44)+M(56) &
    +M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(162)-M(186)-M(221)+M(222))) * den(12)
  T3sum(1:5,33) = T3sum(1:5,33) + Gcoeff * G1tensor(:,148)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(6)*(-M(132)+M(138)+M(162) &
    -M(186)-M(221)+M(222)+M(224)-M(226))) * den(12)
  T3sum(1:5,33) = T3sum(1:5,33) + Gcoeff * G1tensor(:,151)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)+M(58)+M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(131)-M(137)-M(248)+M(250))) * den(12)
  T3sum(1:5,33) = T3sum(1:5,33) + Gcoeff * G1tensor(:,146)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)-M(41)+M(44)+M(56) &
    +M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(161)-M(185)-M(245)+M(246))) * den(12)
  T3sum(1:5,33) = T3sum(1:5,33) + Gcoeff * G1tensor(:,149)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(6)*(-M(131)+M(137)+M(161) &
    -M(185)-M(245)+M(246)+M(248)-M(250))) * den(12)
  T3sum(1:5,33) = T3sum(1:5,33) + Gcoeff * G1tensor(:,152)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(131)-M(132)-M(137)+M(138) &
    +M(224)-M(226)-M(248)+M(250))) * den(12)
  T3sum(1:5,33) = T3sum(1:5,33) + Gcoeff * G1tensor(:,147)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(161)-M(162)-M(185)+M(186) &
    +M(221)-M(222)-M(245)+M(246))) * den(12)
  T3sum(1:5,33) = T3sum(1:5,33) + Gcoeff * G1tensor(:,150)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(12)
  T3sum(1:5,33) = T3sum(1:5,33) + Gcoeff * G1tensor(:,153)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(42)-M(45)+M(52) &
    +M(54)-M(57)+M(64)+M(66)+M(69)+M(76)+M(77)+M(78)-M(79)-M(80)+M(81)-M(82)-M(83)-M(84)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94) &
    -M(97))+c(6)*(M(186)-M(217)+M(221)-M(227))) * den(43)
  T3sum(1:5,48) = T3sum(1:5,48) + Gcoeff * G1tensor(:,4)
  Gcoeff = (c(4)*(M(1)-M(2)+M(5)-M(6)-M(7)+M(8)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)-M(42)-M(45)-M(52) &
    +M(54)-M(57)+M(64)+M(66)+M(69)+M(76)+M(77)+M(78)+M(79)-M(80)+M(81)-M(82)-M(83)-M(84)-M(85)-M(86)+M(87)+M(88)+M(91)-M(94) &
    -M(97))+c(6)*(-M(149)+M(189)+M(207)-M(218))) * den(43)
  T3sum(1:5,48) = T3sum(1:5,48) + Gcoeff * G1tensor(:,5)
  Gcoeff = (c(5)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(6)*(-M(149)-M(186)+M(189)+M(207)+M(217) &
    -M(218)-M(221)+M(227))) * den(43)
  T3sum(1:5,48) = T3sum(1:5,48) + Gcoeff * G1tensor(:,6)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(42)+M(45)+M(52) &
    +M(54)+M(57)+M(64)-M(66)-M(69)-M(76)+M(77)-M(78)-M(79)-M(80)-M(81)-M(82)+M(83)+M(84)+M(85)+M(86)+M(87)-M(88)-M(91)-M(94) &
    +M(97))+c(6)*(M(159)-M(197)-M(210)+M(219))) * den(43)
  T3sum(1:5,48) = T3sum(1:5,48) + Gcoeff * G1tensor(:,13)
  Gcoeff = (c(4)*(M(1)-M(2)+M(5)-M(6)-M(7)+M(8)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)-M(42)+M(45)-M(52) &
    +M(54)+M(57)+M(64)-M(66)-M(69)-M(76)+M(77)-M(78)+M(79)-M(80)-M(81)-M(82)+M(83)+M(84)+M(85)-M(86)+M(87)-M(88)+M(91)-M(94) &
    +M(97))+c(6)*(M(173)-M(183)-M(213)+M(216))) * den(43)
  T3sum(1:5,48) = T3sum(1:5,48) + Gcoeff * G1tensor(:,14)
  Gcoeff = (c(5)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(6)*(-M(159)+M(173)-M(183)+M(197)+M(210) &
    -M(213)+M(216)-M(219))) * den(43)
  T3sum(1:5,48) = T3sum(1:5,48) + Gcoeff * G1tensor(:,15)
  Gcoeff = (c(5)*(M(45)+M(57)-M(66)-M(69)-M(76)-M(78)-M(81)+M(83)+M(84)+M(85)-M(88)+M(97))+c(6)*(M(159)-M(186)-M(197)-M(210) &
    +M(217)+M(219)-M(221)+M(227))) * den(43)
  T3sum(1:5,48) = T3sum(1:5,48) + Gcoeff * G1tensor(:,22)
  Gcoeff = (c(5)*(M(45)+M(57)-M(66)-M(69)-M(76)-M(78)-M(81)+M(83)+M(84)+M(85)-M(88)+M(97))+c(6)*(M(149)+M(173)-M(183)-M(189) &
    -M(207)-M(213)+M(216)+M(218))) * den(43)
  T3sum(1:5,48) = T3sum(1:5,48) + Gcoeff * G1tensor(:,23)
  Gcoeff = (c(6)*(M(149)-M(159)+M(173)-M(183)+M(186)-M(189)+M(197)-M(207)+M(210)-M(213)+M(216)-M(217)+M(218)-M(219)+M(221) &
    -M(227))) * den(43)
  T3sum(1:5,48) = T3sum(1:5,48) + Gcoeff * G1tensor(:,24)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)-M(11)+M(12)-M(19)+M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(42)-M(45)+M(52) &
    -M(54)-M(57)+M(64)+M(66)+M(69)+M(76)-M(77)+M(78)-M(79)+M(80)+M(81)-M(82)-M(83)-M(84)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94) &
    -M(97))+c(6)*(M(180)-M(215)+M(223)-M(233))) * den(43)
  T3sum(1:5,55) = T3sum(1:5,55) + Gcoeff * G1tensor(:,7)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(42)-M(45)+M(52) &
    +M(54)-M(57)+M(64)+M(66)+M(69)+M(76)+M(77)+M(78)-M(79)-M(80)+M(81)-M(82)-M(83)-M(84)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94) &
    -M(97))+c(6)*(M(186)-M(217)+M(221)-M(227))) * den(43)
  T3sum(1:5,55) = T3sum(1:5,55) + Gcoeff * G1tensor(:,8)
  Gcoeff = (c(5)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(6)*(-M(180)+M(186)+M(215)-M(217)+M(221) &
    -M(223)-M(227)+M(233))) * den(43)
  T3sum(1:5,55) = T3sum(1:5,55) + Gcoeff * G1tensor(:,9)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)-M(11)+M(12)-M(19)+M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(42)+M(45)+M(52) &
    -M(54)+M(57)+M(64)-M(66)-M(69)-M(76)-M(77)-M(78)-M(79)+M(80)-M(81)-M(82)+M(83)+M(84)+M(85)+M(86)+M(87)-M(88)-M(91)-M(94) &
    +M(97))+c(6)*(M(135)-M(199)-M(204)+M(220))) * den(43)
  T3sum(1:5,55) = T3sum(1:5,55) + Gcoeff * G1tensor(:,16)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(42)+M(45)+M(52) &
    +M(54)+M(57)+M(64)-M(66)-M(69)-M(76)+M(77)-M(78)-M(79)-M(80)-M(81)-M(82)+M(83)+M(84)+M(85)+M(86)+M(87)-M(88)-M(91)-M(94) &
    +M(97))+c(6)*(M(159)-M(197)-M(210)+M(219))) * den(43)
  T3sum(1:5,55) = T3sum(1:5,55) + Gcoeff * G1tensor(:,17)
  Gcoeff = (c(5)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(6)*(-M(135)+M(159)-M(197)+M(199)+M(204) &
    -M(210)+M(219)-M(220))) * den(43)
  T3sum(1:5,55) = T3sum(1:5,55) + Gcoeff * G1tensor(:,18)
  Gcoeff = (c(5)*(M(45)+M(57)-M(66)-M(69)-M(76)-M(78)-M(81)+M(83)+M(84)+M(85)-M(88)+M(97))+c(6)*(M(135)-M(180)-M(199)-M(204) &
    +M(215)+M(220)-M(223)+M(233))) * den(43)
  T3sum(1:5,55) = T3sum(1:5,55) + Gcoeff * G1tensor(:,25)
  Gcoeff = (c(5)*(M(45)+M(57)-M(66)-M(69)-M(76)-M(78)-M(81)+M(83)+M(84)+M(85)-M(88)+M(97))+c(6)*(M(159)-M(186)-M(197)-M(210) &
    +M(217)+M(219)-M(221)+M(227))) * den(43)
  T3sum(1:5,55) = T3sum(1:5,55) + Gcoeff * G1tensor(:,26)
  Gcoeff = (c(6)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(43)
  T3sum(1:5,55) = T3sum(1:5,55) + Gcoeff * G1tensor(:,27)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(5)+M(6)+M(7)-M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(42)-M(45)+M(52) &
    +M(54)+M(57)+M(64)+M(66)-M(69)+M(76)+M(77)+M(78)-M(79)-M(80)-M(81)-M(82)-M(83)+M(84)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94) &
    -M(97))+c(6)*(M(162)-M(211)+M(222)-M(229))) * den(43)
  T3sum(1:5,42) = T3sum(1:5,42) + Gcoeff * G1tensor(:,31)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(42)-M(45)-M(52) &
    +M(54)+M(57)+M(64)+M(66)-M(69)+M(76)+M(77)+M(78)+M(79)-M(80)-M(81)-M(82)-M(83)+M(84)-M(85)-M(86)+M(87)+M(88)+M(91)-M(94) &
    -M(97))+c(6)*(-M(151)+M(165)+M(208)-M(212))) * den(43)
  T3sum(1:5,42) = T3sum(1:5,42) + Gcoeff * G1tensor(:,32)
  Gcoeff = (c(5)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(6)*(-M(151)-M(162)+M(165)+M(208)+M(211) &
    -M(212)-M(222)+M(229))) * den(43)
  T3sum(1:5,42) = T3sum(1:5,42) + Gcoeff * G1tensor(:,33)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(5)+M(6)+M(7)-M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)+M(42)-M(45)+M(52) &
    -M(54)-M(57)-M(64)+M(66)+M(69)+M(76)-M(77)+M(78)-M(79)+M(80)+M(81)+M(82)-M(83)-M(84)-M(85)+M(86)-M(87)+M(88)-M(91)+M(94) &
    -M(97))+c(6)*(-M(173)+M(183)+M(213)-M(216))) * den(43)
  T3sum(1:5,42) = T3sum(1:5,42) + Gcoeff * G1tensor(:,40)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(5)-M(6)+M(7)-M(8)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(42)-M(45)-M(52) &
    -M(54)-M(57)-M(64)+M(66)+M(69)+M(76)-M(77)+M(78)+M(79)+M(80)+M(81)+M(82)-M(83)-M(84)-M(85)-M(86)-M(87)+M(88)+M(91)+M(94) &
    -M(97))+c(6)*(-M(159)+M(197)+M(210)-M(219))) * den(43)
  T3sum(1:5,42) = T3sum(1:5,42) + Gcoeff * G1tensor(:,41)
  Gcoeff = (c(5)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(6)*(-M(159)+M(173)-M(183)+M(197)+M(210) &
    -M(213)+M(216)-M(219))) * den(43)
  T3sum(1:5,42) = T3sum(1:5,42) + Gcoeff * G1tensor(:,42)
  Gcoeff = (c(5)*(M(42)-M(54)-M(57)-M(64)+M(69)-M(77)+M(80)+M(81)+M(82)-M(84)-M(87)+M(94))+c(6)*(-M(162)-M(173)+M(183)+M(211) &
    +M(213)-M(216)-M(222)+M(229))) * den(43)
  T3sum(1:5,42) = T3sum(1:5,42) + Gcoeff * G1tensor(:,49)
  Gcoeff = (c(5)*(M(42)-M(54)-M(57)-M(64)+M(69)-M(77)+M(80)+M(81)+M(82)-M(84)-M(87)+M(94))+c(6)*(M(151)-M(159)-M(165)+M(197) &
    -M(208)+M(210)+M(212)-M(219))) * den(43)
  T3sum(1:5,42) = T3sum(1:5,42) + Gcoeff * G1tensor(:,50)
  Gcoeff = (c(6)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(43)
  T3sum(1:5,42) = T3sum(1:5,42) + Gcoeff * G1tensor(:,51)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(11)-M(12)+M(19)-M(20)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(42)+M(45)+M(52) &
    +M(54)+M(57)+M(64)-M(66)-M(69)+M(76)+M(77)-M(78)-M(79)-M(80)-M(81)-M(82)+M(83)+M(84)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94) &
    -M(97))+c(6)*(M(156)-M(209)+M(225)-M(239))) * den(43)
  T3sum(1:5,44) = T3sum(1:5,44) + Gcoeff * G1tensor(:,34)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(5)+M(6)+M(7)-M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(42)-M(45)+M(52) &
    +M(54)+M(57)+M(64)+M(66)-M(69)+M(76)+M(77)+M(78)-M(79)-M(80)-M(81)-M(82)-M(83)+M(84)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94) &
    -M(97))+c(6)*(M(162)-M(211)+M(222)-M(229))) * den(43)
  T3sum(1:5,44) = T3sum(1:5,44) + Gcoeff * G1tensor(:,35)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(6)*(-M(156)+M(162)+M(209)-M(211)+M(222) &
    -M(225)-M(229)+M(239))) * den(43)
  T3sum(1:5,44) = T3sum(1:5,44) + Gcoeff * G1tensor(:,36)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(11)-M(12)+M(19)-M(20)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(42)+M(45)+M(52) &
    -M(54)-M(57)-M(64)-M(66)+M(69)+M(76)-M(77)-M(78)-M(79)+M(80)+M(81)+M(82)+M(83)-M(84)-M(85)+M(86)-M(87)+M(88)-M(91)+M(94) &
    -M(97))+c(6)*(M(141)-M(175)-M(206)+M(214))) * den(43)
  T3sum(1:5,44) = T3sum(1:5,44) + Gcoeff * G1tensor(:,43)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(5)+M(6)+M(7)-M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)+M(42)-M(45)+M(52) &
    -M(54)-M(57)-M(64)+M(66)+M(69)+M(76)-M(77)+M(78)-M(79)+M(80)+M(81)+M(82)-M(83)-M(84)-M(85)+M(86)-M(87)+M(88)-M(91)+M(94) &
    -M(97))+c(6)*(-M(173)+M(183)+M(213)-M(216))) * den(43)
  T3sum(1:5,44) = T3sum(1:5,44) + Gcoeff * G1tensor(:,44)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(6)*(-M(141)-M(173)+M(175)+M(183)+M(206) &
    +M(213)-M(214)-M(216))) * den(43)
  T3sum(1:5,44) = T3sum(1:5,44) + Gcoeff * G1tensor(:,45)
  Gcoeff = (c(5)*(M(42)-M(54)-M(57)-M(64)+M(69)-M(77)+M(80)+M(81)+M(82)-M(84)-M(87)+M(94))+c(6)*(M(141)-M(156)-M(175)-M(206) &
    +M(209)+M(214)-M(225)+M(239))) * den(43)
  T3sum(1:5,44) = T3sum(1:5,44) + Gcoeff * G1tensor(:,52)
  Gcoeff = (c(5)*(M(42)-M(54)-M(57)-M(64)+M(69)-M(77)+M(80)+M(81)+M(82)-M(84)-M(87)+M(94))+c(6)*(-M(162)-M(173)+M(183)+M(211) &
    +M(213)-M(216)-M(222)+M(229))) * den(43)
  T3sum(1:5,44) = T3sum(1:5,44) + Gcoeff * G1tensor(:,53)
  Gcoeff = (c(6)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(43)
  T3sum(1:5,44) = T3sum(1:5,44) + Gcoeff * G1tensor(:,54)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)+M(58)+M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(143)-M(145)-M(236)+M(242))) * den(12)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,121)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(163)+M(187)+M(231)-M(232))) * den(12)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,122)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(6)*(-M(143)+M(145)-M(163)+M(187)+M(231) &
    -M(232)+M(236)-M(242))) * den(12)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,123)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)+M(58)+M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(131)-M(137)-M(248)+M(250))) * den(12)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,130)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)-M(58)+M(68)-M(69)+M(70)-M(81)+M(84)+M(93)-M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(211)+M(217)+M(227)-M(229))) * den(12)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,131)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(6)*(-M(131)+M(137)-M(211)+M(217)+M(227) &
    -M(229)+M(248)-M(250))) * den(12)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,132)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(131)-M(137)-M(143) &
    +M(145)+M(236)-M(242)-M(248)+M(250))) * den(12)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,139)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(163)-M(187)-M(211) &
    +M(217)+M(227)-M(229)-M(231)+M(232))) * den(12)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,140)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(12)
  T3sum(1:5,70) = T3sum(1:5,70) + Gcoeff * G1tensor(:,141)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)-M(47)-M(48)+M(49) &
    +M(54)-M(59)+M(66)-M(71)+M(77)+M(78)-M(79)-M(91)+M(98)-M(101)+M(106)+M(107)+M(112)+M(113)-M(114)-M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(M(161)-M(163)-M(232)+M(246))) * den(35)
  T3sum(1:5,68) = T3sum(1:5,68) + Gcoeff * G1tensor(:,58)
  Gcoeff = (c(4)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)-M(47)-M(48)-M(49) &
    +M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98)-M(101)+M(106)+M(107)+M(112)+M(113)-M(114)-M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(-M(145)+M(205)+M(235)-M(236))) * den(35)
  T3sum(1:5,68) = T3sum(1:5,68) + Gcoeff * G1tensor(:,59)
  Gcoeff = (c(5)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(49)+M(79)+M(91)-M(98))+c(6)*(-M(145)-M(161)+M(163)+M(205)+M(232) &
    +M(235)-M(236)-M(246))) * den(35)
  T3sum(1:5,68) = T3sum(1:5,68) + Gcoeff * G1tensor(:,60)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)-M(47)+M(48)+M(49) &
    +M(54)-M(59)-M(66)+M(71)+M(77)-M(78)-M(79)-M(91)+M(98)+M(101)+M(106)-M(107)-M(112)-M(113)-M(114)+M(115)+M(116)+M(117)-M(118) &
    -M(120)+M(122))+c(6)*(M(157)-M(167)-M(240)+M(243))) * den(35)
  T3sum(1:5,68) = T3sum(1:5,68) + Gcoeff * G1tensor(:,67)
  Gcoeff = (c(4)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)-M(47)+M(48)-M(49) &
    +M(54)-M(59)-M(66)+M(71)+M(77)-M(78)+M(79)+M(91)-M(98)+M(101)+M(106)-M(107)-M(112)-M(113)-M(114)+M(115)+M(116)+M(117)-M(118) &
    -M(120)+M(122))+c(6)*(-M(181)+M(191)+M(234)-M(237))) * den(35)
  T3sum(1:5,68) = T3sum(1:5,68) + Gcoeff * G1tensor(:,68)
  Gcoeff = (c(5)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(49)+M(79)+M(91)-M(98))+c(6)*(-M(157)+M(167)-M(181)+M(191)+M(234) &
    -M(237)+M(240)-M(243))) * den(35)
  T3sum(1:5,68) = T3sum(1:5,68) + Gcoeff * G1tensor(:,69)
  Gcoeff = (c(5)*(M(48)-M(66)+M(71)-M(78)+M(101)-M(107)-M(112)-M(113)+M(115)+M(116)-M(118)+M(122))+c(6)*(M(157)-M(161)+M(163) &
    -M(167)+M(232)-M(240)+M(243)-M(246))) * den(35)
  T3sum(1:5,68) = T3sum(1:5,68) + Gcoeff * G1tensor(:,76)
  Gcoeff = (c(5)*(M(48)-M(66)+M(71)-M(78)+M(101)-M(107)-M(112)-M(113)+M(115)+M(116)-M(118)+M(122))+c(6)*(M(145)-M(181)+M(191) &
    -M(205)+M(234)-M(235)+M(236)-M(237))) * den(35)
  T3sum(1:5,68) = T3sum(1:5,68) + Gcoeff * G1tensor(:,77)
  Gcoeff = (c(6)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(35)
  T3sum(1:5,68) = T3sum(1:5,68) + Gcoeff * G1tensor(:,78)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)-M(17)+M(18)+M(23)-M(24)+M(27)-M(28)-M(33)+M(34)+M(47)-M(48)+M(49) &
    -M(54)+M(59)+M(66)-M(71)-M(77)+M(78)-M(79)-M(91)+M(98)-M(101)+M(106)+M(107)+M(112)+M(113)-M(114)-M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(-M(174)+M(177)+M(182)-M(192))) * den(35)
  T3sum(1:5,75) = T3sum(1:5,75) + Gcoeff * G1tensor(:,61)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)-M(47)-M(48)+M(49) &
    +M(54)-M(59)+M(66)-M(71)+M(77)+M(78)-M(79)-M(91)+M(98)-M(101)+M(106)+M(107)+M(112)+M(113)-M(114)-M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(M(161)-M(163)-M(232)+M(246))) * den(35)
  T3sum(1:5,75) = T3sum(1:5,75) + Gcoeff * G1tensor(:,62)
  Gcoeff = (c(5)*(-M(9)+M(10)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)-M(47)+M(54)-M(59)+M(77))+c(6)*(M(161)-M(163)+M(174)-M(177) &
    -M(182)+M(192)-M(232)+M(246))) * den(35)
  T3sum(1:5,75) = T3sum(1:5,75) + Gcoeff * G1tensor(:,63)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)-M(17)+M(18)+M(23)-M(24)+M(27)-M(28)-M(33)+M(34)+M(47)+M(48)+M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98)+M(101)+M(106)-M(107)-M(112)-M(113)-M(114)+M(115)+M(116)+M(117)-M(118) &
    -M(120)+M(122))+c(6)*(M(146)-M(175)+M(176)-M(206))) * den(35)
  T3sum(1:5,75) = T3sum(1:5,75) + Gcoeff * G1tensor(:,70)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)-M(47)+M(48)+M(49) &
    +M(54)-M(59)-M(66)+M(71)+M(77)-M(78)-M(79)-M(91)+M(98)+M(101)+M(106)-M(107)-M(112)-M(113)-M(114)+M(115)+M(116)+M(117)-M(118) &
    -M(120)+M(122))+c(6)*(M(157)-M(167)-M(240)+M(243))) * den(35)
  T3sum(1:5,75) = T3sum(1:5,75) + Gcoeff * G1tensor(:,71)
  Gcoeff = (c(5)*(-M(9)+M(10)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)-M(47)+M(54)-M(59)+M(77))+c(6)*(-M(146)+M(157)-M(167)+M(175) &
    -M(176)+M(206)-M(240)+M(243))) * den(35)
  T3sum(1:5,75) = T3sum(1:5,75) + Gcoeff * G1tensor(:,72)
  Gcoeff = (c(5)*(M(48)-M(66)+M(71)-M(78)+M(101)-M(107)-M(112)-M(113)+M(115)+M(116)-M(118)+M(122))+c(6)*(M(146)+M(174)-M(175) &
    +M(176)-M(177)-M(182)+M(192)-M(206))) * den(35)
  T3sum(1:5,75) = T3sum(1:5,75) + Gcoeff * G1tensor(:,79)
  Gcoeff = (c(5)*(M(48)-M(66)+M(71)-M(78)+M(101)-M(107)-M(112)-M(113)+M(115)+M(116)-M(118)+M(122))+c(6)*(M(157)-M(161)+M(163) &
    -M(167)+M(232)-M(240)+M(243)-M(246))) * den(35)
  T3sum(1:5,75) = T3sum(1:5,75) + Gcoeff * G1tensor(:,80)
  Gcoeff = (c(6)*(-M(146)+M(157)-M(161)+M(163)-M(167)-M(174)+M(175)-M(176)+M(177)+M(182)-M(192)+M(206)+M(232)-M(240)+M(243) &
    -M(246))) * den(35)
  T3sum(1:5,75) = T3sum(1:5,75) + Gcoeff * G1tensor(:,81)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)-M(17)+M(18)+M(23)-M(24)-M(27)+M(28)+M(33)-M(34)-M(47)-M(48)+M(49) &
    +M(54)-M(59)+M(66)-M(71)+M(77)+M(78)-M(79)-M(91)+M(98)+M(101)+M(106)-M(107)+M(112)-M(113)-M(114)+M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(M(185)-M(187)-M(231)+M(245))) * den(35)
  T3sum(1:5,62) = T3sum(1:5,62) + Gcoeff * G1tensor(:,85)
  Gcoeff = (c(4)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(17)+M(18)+M(23)-M(24)-M(27)+M(28)+M(33)-M(34)-M(47)-M(48)-M(49) &
    +M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98)+M(101)+M(106)-M(107)+M(112)-M(113)-M(114)+M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(-M(143)+M(203)+M(241)-M(242))) * den(35)
  T3sum(1:5,62) = T3sum(1:5,62) + Gcoeff * G1tensor(:,86)
  Gcoeff = (c(5)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(49)+M(79)+M(91)-M(98))+c(6)*(-M(143)-M(185)+M(187)+M(203)+M(231) &
    +M(241)-M(242)-M(245))) * den(35)
  T3sum(1:5,62) = T3sum(1:5,62) + Gcoeff * G1tensor(:,87)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)-M(17)+M(18)+M(23)-M(24)-M(27)+M(28)+M(33)-M(34)+M(47)-M(48)+M(49) &
    -M(54)+M(59)+M(66)-M(71)-M(77)+M(78)-M(79)-M(91)+M(98)-M(101)-M(106)+M(107)+M(112)+M(113)+M(114)-M(115)-M(116)-M(117)+M(118) &
    +M(120)-M(122))+c(6)*(M(181)-M(191)-M(234)+M(237))) * den(35)
  T3sum(1:5,62) = T3sum(1:5,62) + Gcoeff * G1tensor(:,94)
  Gcoeff = (c(4)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(17)+M(18)+M(23)-M(24)-M(27)+M(28)+M(33)-M(34)+M(47)-M(48)-M(49) &
    -M(54)+M(59)+M(66)-M(71)-M(77)+M(78)+M(79)+M(91)-M(98)-M(101)-M(106)+M(107)+M(112)+M(113)+M(114)-M(115)-M(116)-M(117)+M(118) &
    +M(120)-M(122))+c(6)*(-M(157)+M(167)+M(240)-M(243))) * den(35)
  T3sum(1:5,62) = T3sum(1:5,62) + Gcoeff * G1tensor(:,95)
  Gcoeff = (c(5)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(49)+M(79)+M(91)-M(98))+c(6)*(-M(157)+M(167)-M(181)+M(191)+M(234) &
    -M(237)+M(240)-M(243))) * den(35)
  T3sum(1:5,62) = T3sum(1:5,62) + Gcoeff * G1tensor(:,96)
  Gcoeff = (c(5)*(M(47)-M(54)+M(59)-M(77)-M(101)-M(106)+M(107)+M(113)+M(114)-M(115)-M(117)+M(120))+c(6)*(M(181)-M(185)+M(187) &
    -M(191)+M(231)-M(234)+M(237)-M(245))) * den(35)
  T3sum(1:5,62) = T3sum(1:5,62) + Gcoeff * G1tensor(:,103)
  Gcoeff = (c(5)*(M(47)-M(54)+M(59)-M(77)-M(101)-M(106)+M(107)+M(113)+M(114)-M(115)-M(117)+M(120))+c(6)*(M(143)-M(157)+M(167) &
    -M(203)+M(240)-M(241)+M(242)-M(243))) * den(35)
  T3sum(1:5,62) = T3sum(1:5,62) + Gcoeff * G1tensor(:,104)
  Gcoeff = (c(6)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(35)
  T3sum(1:5,62) = T3sum(1:5,62) + Gcoeff * G1tensor(:,105)
  Gcoeff = (c(4)*(M(3)-M(4)-M(9)+M(10)-M(13)+M(14)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)-M(27)+M(28)+M(33)-M(34)-M(47)+M(48)+M(49) &
    +M(54)-M(59)-M(66)+M(71)+M(77)-M(78)-M(79)-M(91)+M(98)+M(101)+M(106)-M(107)+M(112)-M(113)-M(114)+M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(M(158)-M(168)-M(198)+M(201))) * den(35)
  T3sum(1:5,64) = T3sum(1:5,64) + Gcoeff * G1tensor(:,88)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)-M(17)+M(18)+M(23)-M(24)-M(27)+M(28)+M(33)-M(34)-M(47)-M(48)+M(49) &
    +M(54)-M(59)+M(66)-M(71)+M(77)+M(78)-M(79)-M(91)+M(98)+M(101)+M(106)-M(107)+M(112)-M(113)-M(114)+M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(M(185)-M(187)-M(231)+M(245))) * den(35)
  T3sum(1:5,64) = T3sum(1:5,64) + Gcoeff * G1tensor(:,89)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(6)*(-M(158)+M(168)+M(185)-M(187) &
    +M(198)-M(201)-M(231)+M(245))) * den(35)
  T3sum(1:5,64) = T3sum(1:5,64) + Gcoeff * G1tensor(:,90)
  Gcoeff = (c(4)*(M(3)-M(4)-M(9)+M(10)-M(13)+M(14)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)-M(27)+M(28)+M(33)-M(34)+M(47)+M(48)+M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98)-M(101)-M(106)+M(107)+M(112)+M(113)+M(114)-M(115)-M(116)-M(117)+M(118) &
    +M(120)-M(122))+c(6)*(M(144)-M(199)+M(200)-M(204))) * den(35)
  T3sum(1:5,64) = T3sum(1:5,64) + Gcoeff * G1tensor(:,97)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)-M(17)+M(18)+M(23)-M(24)-M(27)+M(28)+M(33)-M(34)+M(47)-M(48)+M(49) &
    -M(54)+M(59)+M(66)-M(71)-M(77)+M(78)-M(79)-M(91)+M(98)-M(101)-M(106)+M(107)+M(112)+M(113)+M(114)-M(115)-M(116)-M(117)+M(118) &
    +M(120)-M(122))+c(6)*(M(181)-M(191)-M(234)+M(237))) * den(35)
  T3sum(1:5,64) = T3sum(1:5,64) + Gcoeff * G1tensor(:,98)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(6)*(-M(144)+M(181)-M(191)+M(199) &
    -M(200)+M(204)-M(234)+M(237))) * den(35)
  T3sum(1:5,64) = T3sum(1:5,64) + Gcoeff * G1tensor(:,99)
  Gcoeff = (c(5)*(M(47)-M(54)+M(59)-M(77)-M(101)-M(106)+M(107)+M(113)+M(114)-M(115)-M(117)+M(120))+c(6)*(M(144)-M(158)+M(168) &
    +M(198)-M(199)+M(200)-M(201)-M(204))) * den(35)
  T3sum(1:5,64) = T3sum(1:5,64) + Gcoeff * G1tensor(:,106)
  Gcoeff = (c(5)*(M(47)-M(54)+M(59)-M(77)-M(101)-M(106)+M(107)+M(113)+M(114)-M(115)-M(117)+M(120))+c(6)*(M(181)-M(185)+M(187) &
    -M(191)+M(231)-M(234)+M(237)-M(245))) * den(35)
  T3sum(1:5,64) = T3sum(1:5,64) + Gcoeff * G1tensor(:,107)
  Gcoeff = (c(6)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(35)
  T3sum(1:5,64) = T3sum(1:5,64) + Gcoeff * G1tensor(:,108)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    -M(57)+M(58)-M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(149)+M(151)+M(212)-M(218))) * den(12)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,124)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)+M(58)+M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(143)-M(145)-M(236)+M(242))) * den(12)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,125)
  Gcoeff = (c(5)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56)+M(68))+c(6)*(M(143)-M(145)+M(149)-M(151) &
    -M(212)+M(218)-M(236)+M(242))) * den(12)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,126)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    +M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(153)+M(154)+M(164)-M(188))) * den(12)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,133)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)+M(58)+M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(131)-M(137)-M(248)+M(250))) * den(12)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,134)
  Gcoeff = (c(5)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56)+M(68))+c(6)*(M(131)-M(137)+M(153)-M(154) &
    -M(164)+M(188)-M(248)+M(250))) * den(12)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,135)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(149)-M(151)-M(153) &
    +M(154)+M(164)-M(188)-M(212)+M(218))) * den(12)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,142)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(131)-M(137)-M(143) &
    +M(145)+M(236)-M(242)-M(248)+M(250))) * den(12)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,143)
  Gcoeff = (c(6)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(12)
  T3sum(1:5,82) = T3sum(1:5,82) + Gcoeff * G1tensor(:,144)
  Gcoeff = (c(5)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56)+M(68))+c(6)*(M(143)-M(145)+M(149)-M(151) &
    -M(212)+M(218)-M(236)+M(242))) * den(13)
  T3sum(1:15,82) = T3sum(1:15,82) + Gcoeff * G2tensor(:,40)
  Gcoeff = (c(5)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56)+M(68))+c(6)*(M(131)-M(137)+M(153)-M(154) &
    -M(164)+M(188)-M(248)+M(250))) * den(13)
  T3sum(1:15,82) = T3sum(1:15,82) + Gcoeff * G2tensor(:,43)
  Gcoeff = (c(6)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(13)
  T3sum(1:15,82) = T3sum(1:15,82) + Gcoeff * G2tensor(:,46)
  Gcoeff = (c(5)*(-M(9)+M(10)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)-M(47)+M(54)-M(59)+M(77))+c(6)*(M(161)-M(163)+M(174)-M(177) &
    -M(182)+M(192)-M(232)+M(246))) * den(36)
  T3sum(1:15,75) = T3sum(1:15,75) + Gcoeff * G2tensor(:,19)
  Gcoeff = (c(5)*(-M(9)+M(10)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)-M(47)+M(54)-M(59)+M(77))+c(6)*(-M(146)+M(157)-M(167)+M(175) &
    -M(176)+M(206)-M(240)+M(243))) * den(36)
  T3sum(1:15,75) = T3sum(1:15,75) + Gcoeff * G2tensor(:,22)
  Gcoeff = (c(6)*(-M(146)+M(157)-M(161)+M(163)-M(167)-M(174)+M(175)-M(176)+M(177)+M(182)-M(192)+M(206)+M(232)-M(240)+M(243) &
    -M(246))) * den(36)
  T3sum(1:15,75) = T3sum(1:15,75) + Gcoeff * G2tensor(:,25)
  Gcoeff = (c(5)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(6)*(-M(180)+M(186)+M(215)-M(217)+M(221) &
    -M(223)-M(227)+M(233))) * den(44)
  T3sum(1:15,55) = T3sum(1:15,55) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(5)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(6)*(-M(135)+M(159)-M(197)+M(199)+M(204) &
    -M(210)+M(219)-M(220))) * den(44)
  T3sum(1:15,55) = T3sum(1:15,55) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(6)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(44)
  T3sum(1:15,55) = T3sum(1:15,55) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(6)*(-M(143)+M(145)-M(163)+M(187)+M(231) &
    -M(232)+M(236)-M(242))) * den(73)
  T3sum(1:15,70) = T3sum(1:15,70) + Gcoeff * G2tensor(:,41)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(6)*(-M(131)+M(137)-M(211)+M(217)+M(227) &
    -M(229)+M(248)-M(250))) * den(73)
  T3sum(1:15,70) = T3sum(1:15,70) + Gcoeff * G2tensor(:,44)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(73)
  T3sum(1:15,70) = T3sum(1:15,70) + Gcoeff * G2tensor(:,47)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(102)+M(108)+M(119)-M(121))+c(6)*(M(149)-M(151)+M(211)-M(212) &
    -M(217)+M(218)-M(227)+M(229))) * den(106)
  T3sum(1:15,50) = T3sum(1:15,50) + Gcoeff * G2tensor(:,42)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(102)+M(108)+M(119)-M(121))+c(6)*(M(153)-M(154)+M(163)-M(164) &
    -M(187)+M(188)-M(231)+M(232))) * den(106)
  T3sum(1:15,50) = T3sum(1:15,50) + Gcoeff * G2tensor(:,45)
  Gcoeff = (c(6)*(-M(149)+M(151)+M(153)-M(154)+M(163)-M(164)-M(187)+M(188)-M(211)+M(212)+M(217)-M(218)+M(227)-M(229)-M(231) &
    +M(232))) * den(106)
  T3sum(1:15,50) = T3sum(1:15,50) + Gcoeff * G2tensor(:,48)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(27)+M(28)-M(29)+M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    -M(57)+M(58)-M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(211)-M(217)-M(227)+M(229))) * den(12)
  T4sum(1:35,183) = T4sum(1:35,183) + Gcoeff * G3tensor(:,19)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(27)+M(28)-M(29)+M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    +M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)-M(102)-M(107)+M(108)-M(113)+M(115)+M(119)-M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(163)-M(187)-M(231)+M(232))) * den(12)
  T4sum(1:35,183) = T4sum(1:35,183) + Gcoeff * G3tensor(:,20)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(163)-M(187)-M(211) &
    +M(217)+M(227)-M(229)-M(231)+M(232))) * den(12)
  T4sum(1:35,183) = T4sum(1:35,183) + Gcoeff * G3tensor(:,21)
  Gcoeff = (c(5)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(6)*(-M(149)-M(186)+M(189)+M(207)+M(217) &
    -M(218)-M(221)+M(227))) * den(54)
  T3sum(1:15,48) = T3sum(1:15,48) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(5)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(6)*(-M(159)+M(173)-M(183)+M(197)+M(210) &
    -M(213)+M(216)-M(219))) * den(54)
  T3sum(1:15,48) = T3sum(1:15,48) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(6)*(M(149)-M(159)+M(173)-M(183)+M(186)-M(189)+M(197)-M(207)+M(210)-M(213)+M(216)-M(217)+M(218)-M(219)+M(221) &
    -M(227))) * den(54)
  T3sum(1:15,48) = T3sum(1:15,48) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(6)*(-M(173)-M(180)+M(183)+M(213) &
    +M(215)-M(216)-M(223)+M(233))) * den(75)
  T3sum(1:15,35) = T3sum(1:15,35) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(6)*(-M(135)+M(149)-M(189)+M(199) &
    +M(204)-M(207)+M(218)-M(220))) * den(75)
  T3sum(1:15,35) = T3sum(1:15,35) + Gcoeff * G2tensor(:,6)
  Gcoeff = (c(6)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(75)
  T3sum(1:15,35) = T3sum(1:15,35) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(5)+M(6)+M(7)-M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)+M(42)-M(45)+M(52) &
    -M(54)-M(57)-M(64)+M(66)+M(69)+M(76)-M(77)+M(78)-M(79)+M(80)+M(81)+M(82)-M(83)-M(84)-M(85)+M(86)-M(87)+M(88)-M(91)+M(94) &
    -M(97))+c(6)*(-M(173)+M(183)+M(213)-M(216))) * den(43)
  T4sum(1:35,171) = T4sum(1:35,171) + Gcoeff * G3tensor(:,1)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(5)+M(6)+M(7)-M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)+M(42)+M(45)+M(52) &
    -M(54)+M(57)-M(64)-M(66)-M(69)-M(76)-M(77)-M(78)-M(79)+M(80)-M(81)+M(82)+M(83)+M(84)+M(85)+M(86)-M(87)-M(88)-M(91)+M(94) &
    +M(97))+c(6)*(M(149)-M(189)-M(207)+M(218))) * den(43)
  T4sum(1:35,171) = T4sum(1:35,171) + Gcoeff * G3tensor(:,2)
  Gcoeff = (c(5)*(M(45)+M(57)-M(66)-M(69)-M(76)-M(78)-M(81)+M(83)+M(84)+M(85)-M(88)+M(97))+c(6)*(M(149)+M(173)-M(183)-M(189) &
    -M(207)-M(213)+M(216)+M(218))) * den(43)
  T4sum(1:35,171) = T4sum(1:35,171) + Gcoeff * G3tensor(:,3)
  Gcoeff = (c(5)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(49)+M(79)+M(91)-M(98))+c(6)*(-M(145)-M(161)+M(163)+M(205)+M(232) &
    +M(235)-M(236)-M(246))) * den(56)
  T3sum(1:15,68) = T3sum(1:15,68) + Gcoeff * G2tensor(:,20)
  Gcoeff = (c(5)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(49)+M(79)+M(91)-M(98))+c(6)*(-M(157)+M(167)-M(181)+M(191)+M(234) &
    -M(237)+M(240)-M(243))) * den(56)
  T3sum(1:15,68) = T3sum(1:15,68) + Gcoeff * G2tensor(:,23)
  Gcoeff = (c(6)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(56)
  T3sum(1:15,68) = T3sum(1:15,68) + Gcoeff * G2tensor(:,26)
  Gcoeff = (c(5)*(-M(9)+M(10)+M(15)-M(16)-M(27)+M(28)+M(33)-M(34)-M(106)+M(114)-M(117)+M(120))+c(6)*(M(174)-M(177)+M(181)-M(182) &
    -M(191)+M(192)-M(234)+M(237))) * den(108)
  T3sum(1:15,37) = T3sum(1:15,37) + Gcoeff * G2tensor(:,21)
  Gcoeff = (c(5)*(-M(9)+M(10)+M(15)-M(16)-M(27)+M(28)+M(33)-M(34)-M(106)+M(114)-M(117)+M(120))+c(6)*(M(145)-M(146)+M(175)-M(176) &
    -M(205)+M(206)-M(235)+M(236))) * den(108)
  T3sum(1:15,37) = T3sum(1:15,37) + Gcoeff * G2tensor(:,24)
  Gcoeff = (c(6)*(M(145)-M(146)-M(174)+M(175)-M(176)+M(177)-M(181)+M(182)+M(191)-M(192)-M(205)+M(206)+M(234)-M(235)+M(236) &
    -M(237))) * den(108)
  T3sum(1:15,37) = T3sum(1:15,37) + Gcoeff * G2tensor(:,27)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)-M(17)+M(18)+M(23)-M(24)-M(27)+M(28)+M(33)-M(34)+M(47)-M(48)+M(49) &
    -M(54)+M(59)+M(66)-M(71)-M(77)+M(78)-M(79)-M(91)+M(98)-M(101)-M(106)+M(107)+M(112)+M(113)+M(114)-M(115)-M(116)-M(117)+M(118) &
    +M(120)-M(122))+c(6)*(M(181)-M(191)-M(234)+M(237))) * den(35)
  T4sum(1:35,177) = T4sum(1:35,177) + Gcoeff * G3tensor(:,7)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)-M(17)+M(18)+M(23)-M(24)-M(27)+M(28)+M(33)-M(34)+M(47)+M(48)+M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98)+M(101)-M(106)-M(107)-M(112)-M(113)+M(114)+M(115)+M(116)-M(117)-M(118) &
    +M(120)+M(122))+c(6)*(M(145)-M(205)-M(235)+M(236))) * den(35)
  T4sum(1:35,177) = T4sum(1:35,177) + Gcoeff * G3tensor(:,8)
  Gcoeff = (c(5)*(M(48)-M(66)+M(71)-M(78)+M(101)-M(107)-M(112)-M(113)+M(115)+M(116)-M(118)+M(122))+c(6)*(M(145)-M(181)+M(191) &
    -M(205)+M(234)-M(235)+M(236)-M(237))) * den(35)
  T4sum(1:35,177) = T4sum(1:35,177) + Gcoeff * G3tensor(:,9)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(6)*(-M(158)+M(168)+M(185)-M(187) &
    +M(198)-M(201)-M(231)+M(245))) * den(48)
  T3sum(1:15,64) = T3sum(1:15,64) + Gcoeff * G2tensor(:,28)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(6)*(-M(144)+M(181)-M(191)+M(199) &
    -M(200)+M(204)-M(234)+M(237))) * den(48)
  T3sum(1:15,64) = T3sum(1:15,64) + Gcoeff * G2tensor(:,31)
  Gcoeff = (c(6)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(48)
  T3sum(1:15,64) = T3sum(1:15,64) + Gcoeff * G2tensor(:,34)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(6)*(-M(156)+M(162)+M(209)-M(211)+M(222) &
    -M(225)-M(229)+M(239))) * den(51)
  T3sum(1:15,44) = T3sum(1:15,44) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(6)*(-M(141)-M(173)+M(175)+M(183)+M(206) &
    +M(213)-M(214)-M(216))) * den(51)
  T3sum(1:15,44) = T3sum(1:15,44) + Gcoeff * G2tensor(:,13)
  Gcoeff = (c(6)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(51)
  T3sum(1:15,44) = T3sum(1:15,44) + Gcoeff * G2tensor(:,16)
  Gcoeff = (c(5)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(6)*(-M(151)-M(162)+M(165)+M(208)+M(211) &
    -M(212)-M(222)+M(229))) * den(54)
  T3sum(1:15,42) = T3sum(1:15,42) + Gcoeff * G2tensor(:,11)
  Gcoeff = (c(5)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(6)*(-M(159)+M(173)-M(183)+M(197)+M(210) &
    -M(213)+M(216)-M(219))) * den(54)
  T3sum(1:15,42) = T3sum(1:15,42) + Gcoeff * G2tensor(:,14)
  Gcoeff = (c(6)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(54)
  T3sum(1:15,42) = T3sum(1:15,42) + Gcoeff * G2tensor(:,17)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(76)+M(85)-M(88)+M(97))+c(6)*(-M(156)+M(159)-M(197)+M(209) &
    -M(210)+M(219)-M(225)+M(239))) * den(86)
  T3sum(1:15,29) = T3sum(1:15,29) + Gcoeff * G2tensor(:,12)
  Gcoeff = (c(5)*(-M(11)+M(12)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(76)+M(85)-M(88)+M(97))+c(6)*(-M(141)+M(151)-M(165)+M(175) &
    +M(206)-M(208)+M(212)-M(214))) * den(86)
  T3sum(1:15,29) = T3sum(1:15,29) + Gcoeff * G2tensor(:,15)
  Gcoeff = (c(6)*(-M(141)+M(151)+M(156)-M(159)-M(165)+M(175)+M(197)+M(206)-M(208)-M(209)+M(210)+M(212)-M(214)-M(219)+M(225) &
    -M(239))) * den(86)
  T3sum(1:15,29) = T3sum(1:15,29) + Gcoeff * G2tensor(:,18)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(42)+M(45)+M(52) &
    +M(54)+M(57)+M(64)-M(66)-M(69)-M(76)+M(77)-M(78)-M(79)-M(80)-M(81)-M(82)+M(83)+M(84)+M(85)+M(86)+M(87)-M(88)-M(91)-M(94) &
    +M(97))+c(6)*(M(159)-M(197)-M(210)+M(219))) * den(43)
  T4sum(1:35,153) = T4sum(1:35,153) + Gcoeff * G3tensor(:,4)
  Gcoeff = (c(4)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)-M(11)+M(12)+M(19)-M(20)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(42)+M(45)+M(52) &
    -M(54)-M(57)-M(64)-M(66)+M(69)-M(76)-M(77)-M(78)-M(79)+M(80)+M(81)+M(82)+M(83)-M(84)+M(85)+M(86)-M(87)-M(88)-M(91)+M(94) &
    +M(97))+c(6)*(M(151)-M(165)-M(208)+M(212))) * den(43)
  T4sum(1:35,153) = T4sum(1:35,153) + Gcoeff * G3tensor(:,5)
  Gcoeff = (c(5)*(M(42)-M(54)-M(57)-M(64)+M(69)-M(77)+M(80)+M(81)+M(82)-M(84)-M(87)+M(94))+c(6)*(M(151)-M(159)-M(165)+M(197) &
    -M(208)+M(210)+M(212)-M(219))) * den(43)
  T4sum(1:35,153) = T4sum(1:35,153) + Gcoeff * G3tensor(:,6)
  Gcoeff = (c(5)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(49)+M(79)+M(91)-M(98))+c(6)*(-M(143)-M(185)+M(187)+M(203)+M(231) &
    +M(241)-M(242)-M(245))) * den(56)
  T3sum(1:15,62) = T3sum(1:15,62) + Gcoeff * G2tensor(:,29)
  Gcoeff = (c(5)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(49)+M(79)+M(91)-M(98))+c(6)*(-M(157)+M(167)-M(181)+M(191)+M(234) &
    -M(237)+M(240)-M(243))) * den(56)
  T3sum(1:15,62) = T3sum(1:15,62) + Gcoeff * G2tensor(:,32)
  Gcoeff = (c(6)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(56)
  T3sum(1:15,62) = T3sum(1:15,62) + Gcoeff * G2tensor(:,35)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(27)-M(28)-M(33)+M(34)-M(112)+M(116)-M(118)+M(122))+c(6)*(M(157)-M(158)-M(167)+M(168) &
    +M(198)-M(201)-M(240)+M(243))) * den(117)
  T3sum(1:15,32) = T3sum(1:15,32) + Gcoeff * G2tensor(:,30)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)+M(27)-M(28)-M(33)+M(34)-M(112)+M(116)-M(118)+M(122))+c(6)*(M(143)-M(144)+M(199)-M(200) &
    -M(203)+M(204)-M(241)+M(242))) * den(117)
  T3sum(1:15,32) = T3sum(1:15,32) + Gcoeff * G2tensor(:,33)
  Gcoeff = (c(6)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(117)
  T3sum(1:15,32) = T3sum(1:15,32) + Gcoeff * G2tensor(:,36)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)-M(47)+M(48)+M(49) &
    +M(54)-M(59)-M(66)+M(71)+M(77)-M(78)-M(79)-M(91)+M(98)+M(101)+M(106)-M(107)-M(112)-M(113)-M(114)+M(115)+M(116)+M(117)-M(118) &
    -M(120)+M(122))+c(6)*(M(157)-M(167)-M(240)+M(243))) * den(35)
  T4sum(1:35,162) = T4sum(1:35,162) + Gcoeff * G3tensor(:,10)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)+M(47)+M(48)+M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98)-M(101)-M(106)+M(107)-M(112)+M(113)+M(114)-M(115)+M(116)-M(117)-M(118) &
    +M(120)+M(122))+c(6)*(M(143)-M(203)-M(241)+M(242))) * den(35)
  T4sum(1:35,162) = T4sum(1:35,162) + Gcoeff * G3tensor(:,11)
  Gcoeff = (c(5)*(M(47)-M(54)+M(59)-M(77)-M(101)-M(106)+M(107)+M(113)+M(114)-M(115)-M(117)+M(120))+c(6)*(M(143)-M(157)+M(167) &
    -M(203)+M(240)-M(241)+M(242)-M(243))) * den(35)
  T4sum(1:35,162) = T4sum(1:35,162) + Gcoeff * G3tensor(:,12)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(134)+M(136)+M(179)-M(180) &
    +M(196)-M(202)-M(223)+M(247))) * den(81)
  T3sum(1:15,18) = T3sum(1:15,18) + Gcoeff * G2tensor(:,37)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(147)+M(153)-M(171)+M(177) &
    +M(182)-M(184)+M(188)-M(190))) * den(81)
  T3sum(1:15,18) = T3sum(1:15,18) + Gcoeff * G2tensor(:,38)
  Gcoeff = (c(6)*(M(134)-M(136)-M(147)+M(153)-M(171)+M(177)-M(179)+M(180)+M(182)-M(184)+M(188)-M(190)-M(196)+M(202)+M(223) &
    -M(247))) * den(81)
  T3sum(1:15,18) = T3sum(1:15,18) + Gcoeff * G2tensor(:,39)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)+M(63)-M(65)-M(66)-M(67)+M(68)-M(69)-M(70)+M(71)+M(72)-M(73)+M(74)+M(75)+M(76)-M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(134)-M(179)+M(202)-M(247))) * den(40)
  T4sum(1:35,143) = T4sum(1:35,143) + Gcoeff * G3tensor(:,13)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(41)-M(48)-M(51) &
    +M(53)+M(60)+M(63)+M(65)+M(66)+M(67)-M(68)-M(69)-M(70)-M(71)+M(72)-M(73)-M(74)+M(75)+M(76)+M(78)-M(81)+M(88)+M(90)-M(93) &
    -M(100))+c(6)*(-M(153)+M(171)+M(184)-M(188))) * den(40)
  T4sum(1:35,143) = T4sum(1:35,143) + Gcoeff * G3tensor(:,15)
  Gcoeff = (c(5)*(-M(41)-M(48)-M(51)+M(53)+M(65)+M(66)+M(67)-M(68)-M(71)-M(74)+M(78)+M(90))+c(6)*(-M(134)-M(153)+M(171)+M(179) &
    +M(184)-M(188)-M(202)+M(247))) * den(40)
  T4sum(1:35,143) = T4sum(1:35,143) + Gcoeff * G3tensor(:,17)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)-M(17)+M(18)+M(19)-M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)+M(63)-M(65)-M(66)-M(67)+M(68)-M(69)-M(70)+M(71)+M(72)+M(73)+M(74)+M(75)-M(76)-M(78)-M(81)-M(88)-M(90)-M(93) &
    +M(100))+c(6)*(M(136)-M(180)+M(196)-M(223))) * den(40)
  T4sum(1:35,144) = T4sum(1:35,144) + Gcoeff * G3tensor(:,14)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)-M(17)+M(18)+M(19)-M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)-M(48)-M(51) &
    +M(53)+M(60)+M(63)+M(65)+M(66)+M(67)-M(68)-M(69)-M(70)-M(71)+M(72)+M(73)-M(74)+M(75)-M(76)+M(78)-M(81)-M(88)+M(90)-M(93) &
    +M(100))+c(6)*(-M(147)+M(177)+M(182)-M(190))) * den(40)
  T4sum(1:35,144) = T4sum(1:35,144) + Gcoeff * G3tensor(:,16)
  Gcoeff = (c(5)*(-M(41)-M(48)-M(51)+M(53)+M(65)+M(66)+M(67)-M(68)-M(71)-M(74)+M(78)+M(90))+c(6)*(-M(136)-M(147)+M(177)+M(180) &
    +M(182)-M(190)-M(196)+M(223))) * den(40)
  T4sum(1:35,144) = T4sum(1:35,144) + Gcoeff * G3tensor(:,18)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(131)-M(132)-M(137)+M(138) &
    +M(224)-M(226)-M(248)+M(250))) * den(126)
  T3sum(1:15,33) = T3sum(1:15,33) + Gcoeff * G2tensor(:,49)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(161)-M(162)-M(185)+M(186) &
    +M(221)-M(222)-M(245)+M(246))) * den(126)
  T3sum(1:15,33) = T3sum(1:15,33) + Gcoeff * G2tensor(:,50)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(126)
  T3sum(1:15,33) = T3sum(1:15,33) + Gcoeff * G2tensor(:,51)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)+M(58)+M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(132)-M(138)-M(224)+M(226))) * den(12)
  T4sum(1:35,164) = T4sum(1:35,164) + Gcoeff * G3tensor(:,22)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)-M(41)+M(44)+M(56) &
    +M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(162)-M(186)-M(221)+M(222))) * den(12)
  T4sum(1:35,164) = T4sum(1:35,164) + Gcoeff * G3tensor(:,24)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(6)*(-M(132)+M(138)+M(162) &
    -M(186)-M(221)+M(222)+M(224)-M(226))) * den(12)
  T4sum(1:35,164) = T4sum(1:35,164) + Gcoeff * G3tensor(:,26)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)+M(58)+M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(131)-M(137)-M(248)+M(250))) * den(12)
  T4sum(1:35,165) = T4sum(1:35,165) + Gcoeff * G3tensor(:,23)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)-M(41)+M(44)+M(56) &
    +M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(161)-M(185)-M(245)+M(246))) * den(12)
  T4sum(1:35,165) = T4sum(1:35,165) + Gcoeff * G3tensor(:,25)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(6)*(-M(131)+M(137)+M(161) &
    -M(185)-M(245)+M(246)+M(248)-M(250))) * den(12)
  T4sum(1:35,165) = T4sum(1:35,165) + Gcoeff * G3tensor(:,27)

end subroutine vamp_13

end module ol_vamp_13_ppjjjj_gggggg_1_/**/REALKIND
