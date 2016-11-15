
module ol_vamp_6_ppjjjj_gggggg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_6(M)
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
  complex(REALKIND), dimension(4,1,4,103) :: G0
  complex(REALKIND), dimension(4,5,4,141) :: G1
  complex(REALKIND), dimension(4,15,4,45) :: G2
  complex(REALKIND), dimension(4,35,4,12) :: G3
  complex(REALKIND), dimension(1,27) :: G0tensor
  complex(REALKIND), dimension(5,72) :: G1tensor
  complex(REALKIND), dimension(15,138) :: G2tensor
  complex(REALKIND), dimension(35,33) :: G3tensor
  complex(REALKIND), dimension(70,12) :: G4tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,0),G0(:,:,:,2))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-5),wf(:,-4),G0(:,:,:,3))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-2),wf(:,-1),G0tensor(:,1))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-1),wf(:,-2),G0tensor(:,2))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-2),wf(:,-1),G0tensor(:,3))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,1))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-4),wf(:,-5),G0(:,:,:,4))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-2),wf(:,-1),G0tensor(:,4))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-1),wf(:,-2),G0tensor(:,5))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-2),wf(:,-1),G0tensor(:,6))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,2))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-5),wf(:,-4),G0(:,:,:,5))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,5),wf(:,-2),wf(:,-1),G0tensor(:,7))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,5),wf(:,-1),wf(:,-2),G0tensor(:,8))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,5),wf(:,-2),wf(:,-1),G0tensor(:,9))
  call check_last_UV_W(l_switch,G0(:,:,:,5),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,3))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-5),wf(:,95),G0(:,:,:,6))
  call check_last_UV_W(l_switch,G0(:,:,:,6),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,4))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,95),wf(:,-5),G0(:,:,:,7))
  call check_last_UV_W(l_switch,G0(:,:,:,7),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,5))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-5),wf(:,95),G0(:,:,:,8))
  call check_last_UV_W(l_switch,G0(:,:,:,8),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,6))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-5),wf(:,66),G0(:,:,:,9))
  call check_last_UV_W(l_switch,G0(:,:,:,9),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,7))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,66),wf(:,-5),G0(:,:,:,10))
  call check_last_UV_W(l_switch,G0(:,:,:,10),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,8))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-5),wf(:,66),G0(:,:,:,11))
  call check_last_UV_W(l_switch,G0(:,:,:,11),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,9))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-4),wf(:,99),G0(:,:,:,12))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,10))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,99),wf(:,-4),G0(:,:,:,13))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,11))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-4),wf(:,99),G0(:,:,:,14))
  call check_last_UV_W(l_switch,G0(:,:,:,14),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,12))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-4),wf(:,70),G0(:,:,:,15))
  call check_last_UV_W(l_switch,G0(:,:,:,15),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,13))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,70),wf(:,-4),G0(:,:,:,16))
  call check_last_UV_W(l_switch,G0(:,:,:,16),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,14))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-4),wf(:,70),G0(:,:,:,17))
  call check_last_UV_W(l_switch,G0(:,:,:,17),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,15))
  call loop_UV_W(G0(:,:,:,2),Q(:,9),wf(:,-2),Q(:,4),G1(:,:,:,1))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,-4),G1(:,:,:,2))
  call check_last_UV_W(l_switch,G1(:,:,:,2),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,1))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-4),wf(:,-5),G1(:,:,:,3))
  call check_last_UV_W(l_switch,G1(:,:,:,3),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,2))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,-4),G1(:,:,:,4))
  call check_last_UV_W(l_switch,G1(:,:,:,4),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,3))
  call loop_UV_W(G1(:,:,:,1),Q(:,13),wf(:,84),Q(:,48),G2(:,:,:,1))
  call check_last_UV_W(l_switch,G2(:,:,:,1),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,1))
  call loop_UV_W(G1(:,:,:,1),Q(:,13),wf(:,-5),Q(:,32),G2(:,:,:,2))
  call loop_UV_W(G2(:,:,:,2),Q(:,45),wf(:,-4),Q(:,16),G3(:,:,:,1))
  call check_last_UV_W(l_switch,G3(:,:,:,1),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,1))
  call loop_UV_W(G1(:,:,:,1),Q(:,13),wf(:,-4),Q(:,16),G2(:,:,:,3))
  call loop_UV_W(G2(:,:,:,3),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,2))
  call check_last_UV_W(l_switch,G3(:,:,:,2),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,2))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-2),wf(:,84),G0(:,:,:,18))
  call check_last_UV_W(l_switch,G0(:,:,:,18),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,16))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,84),wf(:,-2),G0(:,:,:,19))
  call check_last_UV_W(l_switch,G0(:,:,:,19),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,17))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-2),wf(:,84),G0(:,:,:,20))
  call check_last_UV_W(l_switch,G0(:,:,:,20),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,18))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-5),wf(:,-2),G0(:,:,:,21))
  call loop_UV_W(G0(:,:,:,21),Q(:,45),wf(:,-4),Q(:,16),G1(:,:,:,5))
  call check_last_UV_W(l_switch,G1(:,:,:,5),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,4))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-2),wf(:,-5),G0(:,:,:,22))
  call loop_UV_W(G0(:,:,:,22),Q(:,45),wf(:,-4),Q(:,16),G1(:,:,:,6))
  call check_last_UV_W(l_switch,G1(:,:,:,6),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,5))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-5),wf(:,-2),G0(:,:,:,23))
  call loop_UV_W(G0(:,:,:,23),Q(:,45),wf(:,-4),Q(:,16),G1(:,:,:,7))
  call check_last_UV_W(l_switch,G1(:,:,:,7),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,6))
  call loop_UV_W(G0(:,:,:,2),Q(:,9),wf(:,-4),Q(:,16),G1(:,:,:,8))
  call loop_GGG_G_12(G1(:,:,:,8),wf(:,-5),wf(:,-2),G1(:,:,:,9))
  call check_last_UV_W(l_switch,G1(:,:,:,9),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,7))
  call loop_GGG_G_12(G1(:,:,:,8),wf(:,-2),wf(:,-5),G1(:,:,:,10))
  call check_last_UV_W(l_switch,G1(:,:,:,10),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,8))
  call loop_GGG_G_23(G1(:,:,:,8),wf(:,-5),wf(:,-2),G1(:,:,:,11))
  call check_last_UV_W(l_switch,G1(:,:,:,11),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,9))
  call loop_GGG_G_12(G1(:,:,:,8),wf(:,-5),wf(:,-1),G1(:,:,:,12))
  call check_last_UV_W(l_switch,G1(:,:,:,12),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,10))
  call loop_GGG_G_12(G1(:,:,:,8),wf(:,-1),wf(:,-5),G1(:,:,:,13))
  call check_last_UV_W(l_switch,G1(:,:,:,13),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,11))
  call loop_GGG_G_23(G1(:,:,:,8),wf(:,-5),wf(:,-1),G1(:,:,:,14))
  call check_last_UV_W(l_switch,G1(:,:,:,14),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,12))
  call loop_UV_W(G1(:,:,:,8),Q(:,25),wf(:,-5),Q(:,32),G2(:,:,:,4))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,4),wf(:,-2),wf(:,-1),G2tensor(:,13))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,4),wf(:,-1),wf(:,-2),G2tensor(:,14))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,4),wf(:,-2),wf(:,-1),G2tensor(:,15))
  call check_last_UV_W(l_switch,G2(:,:,:,4),Q(:,57),wf(:,105),Q(:,6),G3tensor(:,2))
  call loop_UV_W(G1(:,:,:,8),Q(:,25),wf(:,99),Q(:,34),G2(:,:,:,5))
  call check_last_UV_W(l_switch,G2(:,:,:,5),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,3))
  call loop_UV_W(G1(:,:,:,8),Q(:,25),wf(:,70),Q(:,36),G2(:,:,:,6))
  call check_last_UV_W(l_switch,G2(:,:,:,6),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,4))
  call loop_UV_W(G1(:,:,:,8),Q(:,25),wf(:,-2),Q(:,4),G2(:,:,:,7))
  call loop_UV_W(G2(:,:,:,7),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,3))
  call check_last_UV_W(l_switch,G3(:,:,:,3),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,3))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-4),wf(:,-2),G0(:,:,:,24))
  call loop_UV_W(G0(:,:,:,24),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,15))
  call check_last_UV_W(l_switch,G1(:,:,:,15),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,16))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-2),wf(:,-4),G0(:,:,:,25))
  call loop_UV_W(G0(:,:,:,25),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,16))
  call check_last_UV_W(l_switch,G1(:,:,:,16),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,17))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-4),wf(:,-2),G0(:,:,:,26))
  call loop_UV_W(G0(:,:,:,26),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,17))
  call check_last_UV_W(l_switch,G1(:,:,:,17),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,18))
  call loop_UV_W(G0(:,:,:,2),Q(:,9),wf(:,-5),Q(:,32),G1(:,:,:,18))
  call loop_GGG_G_12(G1(:,:,:,18),wf(:,-4),wf(:,-2),G1(:,:,:,19))
  call check_last_UV_W(l_switch,G1(:,:,:,19),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,19))
  call loop_GGG_G_12(G1(:,:,:,18),wf(:,-2),wf(:,-4),G1(:,:,:,20))
  call check_last_UV_W(l_switch,G1(:,:,:,20),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,20))
  call loop_GGG_G_23(G1(:,:,:,18),wf(:,-4),wf(:,-2),G1(:,:,:,21))
  call check_last_UV_W(l_switch,G1(:,:,:,21),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,21))
  call loop_GGG_G_12(G1(:,:,:,18),wf(:,-4),wf(:,-1),G1(:,:,:,22))
  call check_last_UV_W(l_switch,G1(:,:,:,22),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,22))
  call loop_GGG_G_12(G1(:,:,:,18),wf(:,-1),wf(:,-4),G1(:,:,:,23))
  call check_last_UV_W(l_switch,G1(:,:,:,23),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,23))
  call loop_GGG_G_23(G1(:,:,:,18),wf(:,-4),wf(:,-1),G1(:,:,:,24))
  call check_last_UV_W(l_switch,G1(:,:,:,24),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,24))
  call loop_UV_W(G1(:,:,:,18),Q(:,41),wf(:,-4),Q(:,16),G2(:,:,:,8))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,8),wf(:,-2),wf(:,-1),G2tensor(:,25))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,8),wf(:,-1),wf(:,-2),G2tensor(:,26))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,8),wf(:,-2),wf(:,-1),G2tensor(:,27))
  call check_last_UV_W(l_switch,G2(:,:,:,8),Q(:,57),wf(:,105),Q(:,6),G3tensor(:,5))
  call loop_UV_W(G1(:,:,:,18),Q(:,41),wf(:,95),Q(:,18),G2(:,:,:,9))
  call check_last_UV_W(l_switch,G2(:,:,:,9),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,6))
  call loop_UV_W(G1(:,:,:,18),Q(:,41),wf(:,66),Q(:,20),G2(:,:,:,10))
  call check_last_UV_W(l_switch,G2(:,:,:,10),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,7))
  call loop_UV_W(G1(:,:,:,18),Q(:,41),wf(:,-2),Q(:,4),G2(:,:,:,11))
  call loop_UV_W(G2(:,:,:,11),Q(:,45),wf(:,-4),Q(:,16),G3(:,:,:,4))
  call check_last_UV_W(l_switch,G3(:,:,:,4),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,4))
  call loop_UV_W(G0(:,:,:,2),Q(:,9),wf(:,8),Q(:,52),G1(:,:,:,25))
  call check_last_UV_W(l_switch,G1(:,:,:,25),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,28))
  call loop_UV_W(G0(:,:,:,2),Q(:,9),wf(:,11),Q(:,52),G1(:,:,:,26))
  call check_last_UV_W(l_switch,G1(:,:,:,26),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,29))
  call loop_UV_W(G0(:,:,:,2),Q(:,9),wf(:,12),Q(:,52),G1(:,:,:,27))
  call check_last_UV_W(l_switch,G1(:,:,:,27),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,30))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-1),wf(:,84),G0(:,:,:,27))
  call check_last_UV_W(l_switch,G0(:,:,:,27),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,19))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,84),wf(:,-1),G0(:,:,:,28))
  call check_last_UV_W(l_switch,G0(:,:,:,28),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,20))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-1),wf(:,84),G0(:,:,:,29))
  call check_last_UV_W(l_switch,G0(:,:,:,29),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,21))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-5),wf(:,-1),G0(:,:,:,30))
  call loop_UV_W(G0(:,:,:,30),Q(:,43),wf(:,-4),Q(:,16),G1(:,:,:,28))
  call check_last_UV_W(l_switch,G1(:,:,:,28),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,31))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-1),wf(:,-5),G0(:,:,:,31))
  call loop_UV_W(G0(:,:,:,31),Q(:,43),wf(:,-4),Q(:,16),G1(:,:,:,29))
  call check_last_UV_W(l_switch,G1(:,:,:,29),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,32))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-5),wf(:,-1),G0(:,:,:,32))
  call loop_UV_W(G0(:,:,:,32),Q(:,43),wf(:,-4),Q(:,16),G1(:,:,:,30))
  call check_last_UV_W(l_switch,G1(:,:,:,30),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,33))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-4),wf(:,-1),G0(:,:,:,33))
  call loop_UV_W(G0(:,:,:,33),Q(:,27),wf(:,-5),Q(:,32),G1(:,:,:,31))
  call check_last_UV_W(l_switch,G1(:,:,:,31),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,34))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-1),wf(:,-4),G0(:,:,:,34))
  call loop_UV_W(G0(:,:,:,34),Q(:,27),wf(:,-5),Q(:,32),G1(:,:,:,32))
  call check_last_UV_W(l_switch,G1(:,:,:,32),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,35))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-4),wf(:,-1),G0(:,:,:,35))
  call loop_UV_W(G0(:,:,:,35),Q(:,27),wf(:,-5),Q(:,32),G1(:,:,:,33))
  call check_last_UV_W(l_switch,G1(:,:,:,33),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,36))
  call loop_UV_W(G0(:,:,:,2),Q(:,9),wf(:,26),Q(:,50),G1(:,:,:,34))
  call check_last_UV_W(l_switch,G1(:,:,:,34),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,37))
  call loop_UV_W(G0(:,:,:,2),Q(:,9),wf(:,29),Q(:,50),G1(:,:,:,35))
  call check_last_UV_W(l_switch,G1(:,:,:,35),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,38))
  call loop_UV_W(G0(:,:,:,2),Q(:,9),wf(:,30),Q(:,50),G1(:,:,:,36))
  call check_last_UV_W(l_switch,G1(:,:,:,36),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,39))
  call loop_UV_W(G0(:,:,:,2),Q(:,9),wf(:,84),Q(:,48),G1(:,:,:,37))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,37),wf(:,-2),wf(:,-1),G1tensor(:,22))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,37),wf(:,-1),wf(:,-2),G1tensor(:,23))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,37),wf(:,-2),wf(:,-1),G1tensor(:,24))
  call check_last_UV_W(l_switch,G1(:,:,:,37),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,40))
  call loop_UV_W(G0(:,:,:,2),Q(:,9),wf(:,247),Q(:,50),G1(:,:,:,38))
  call check_last_UV_W(l_switch,G1(:,:,:,38),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,41))
  call loop_UV_W(G0(:,:,:,2),Q(:,9),wf(:,95),Q(:,18),G1(:,:,:,39))
  call loop_UV_W(G1(:,:,:,39),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,12))
  call check_last_UV_W(l_switch,G2(:,:,:,12),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,8))
  call loop_UV_W(G0(:,:,:,2),Q(:,9),wf(:,265),Q(:,52),G1(:,:,:,40))
  call check_last_UV_W(l_switch,G1(:,:,:,40),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,42))
  call loop_UV_W(G0(:,:,:,2),Q(:,9),wf(:,66),Q(:,20),G1(:,:,:,41))
  call loop_UV_W(G1(:,:,:,41),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,13))
  call check_last_UV_W(l_switch,G2(:,:,:,13),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,9))
  call loop_UV_W(G0(:,:,:,2),Q(:,9),wf(:,248),Q(:,50),G1(:,:,:,42))
  call check_last_UV_W(l_switch,G1(:,:,:,42),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,43))
  call loop_UV_W(G0(:,:,:,2),Q(:,9),wf(:,99),Q(:,34),G1(:,:,:,43))
  call loop_UV_W(G1(:,:,:,43),Q(:,43),wf(:,-4),Q(:,16),G2(:,:,:,14))
  call check_last_UV_W(l_switch,G2(:,:,:,14),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,10))
  call loop_UV_W(G0(:,:,:,2),Q(:,9),wf(:,266),Q(:,52),G1(:,:,:,44))
  call check_last_UV_W(l_switch,G1(:,:,:,44),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,44))
  call loop_UV_W(G0(:,:,:,2),Q(:,9),wf(:,70),Q(:,36),G1(:,:,:,45))
  call loop_UV_W(G1(:,:,:,45),Q(:,45),wf(:,-4),Q(:,16),G2(:,:,:,15))
  call check_last_UV_W(l_switch,G2(:,:,:,15),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,11))
  call loop_UV_W(G0(:,:,:,2),Q(:,9),wf(:,249),Q(:,50),G1(:,:,:,46))
  call check_last_UV_W(l_switch,G1(:,:,:,46),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,45))
  call loop_UV_W(G0(:,:,:,2),Q(:,9),wf(:,267),Q(:,52),G1(:,:,:,47))
  call check_last_UV_W(l_switch,G1(:,:,:,47),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,46))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,-3),G0(:,:,:,36))
  call loop_GGG_G_12(G0(:,:,:,36),wf(:,-5),wf(:,-4),G0(:,:,:,37))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,37),wf(:,-2),wf(:,-1),G0tensor(:,10))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,37),wf(:,-1),wf(:,-2),G0tensor(:,11))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,37),wf(:,-2),wf(:,-1),G0tensor(:,12))
  call check_last_UV_W(l_switch,G0(:,:,:,37),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,25))
  call loop_GGG_G_12(G0(:,:,:,36),wf(:,-4),wf(:,-5),G0(:,:,:,38))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,38),wf(:,-2),wf(:,-1),G0tensor(:,13))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,38),wf(:,-1),wf(:,-2),G0tensor(:,14))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,38),wf(:,-2),wf(:,-1),G0tensor(:,15))
  call check_last_UV_W(l_switch,G0(:,:,:,38),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,26))
  call loop_GGG_G_23(G0(:,:,:,36),wf(:,-5),wf(:,-4),G0(:,:,:,39))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,39),wf(:,-2),wf(:,-1),G0tensor(:,16))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,39),wf(:,-1),wf(:,-2),G0tensor(:,17))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,39),wf(:,-2),wf(:,-1),G0tensor(:,18))
  call check_last_UV_W(l_switch,G0(:,:,:,39),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,27))
  call loop_GGG_G_12(G0(:,:,:,36),wf(:,-5),wf(:,95),G0(:,:,:,40))
  call check_last_UV_W(l_switch,G0(:,:,:,40),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,28))
  call loop_GGG_G_12(G0(:,:,:,36),wf(:,95),wf(:,-5),G0(:,:,:,41))
  call check_last_UV_W(l_switch,G0(:,:,:,41),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,29))
  call loop_GGG_G_23(G0(:,:,:,36),wf(:,-5),wf(:,95),G0(:,:,:,42))
  call check_last_UV_W(l_switch,G0(:,:,:,42),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,30))
  call loop_GGG_G_12(G0(:,:,:,36),wf(:,-5),wf(:,66),G0(:,:,:,43))
  call check_last_UV_W(l_switch,G0(:,:,:,43),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,31))
  call loop_GGG_G_12(G0(:,:,:,36),wf(:,66),wf(:,-5),G0(:,:,:,44))
  call check_last_UV_W(l_switch,G0(:,:,:,44),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,32))
  call loop_GGG_G_23(G0(:,:,:,36),wf(:,-5),wf(:,66),G0(:,:,:,45))
  call check_last_UV_W(l_switch,G0(:,:,:,45),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,33))
  call loop_GGG_G_12(G0(:,:,:,36),wf(:,-4),wf(:,99),G0(:,:,:,46))
  call check_last_UV_W(l_switch,G0(:,:,:,46),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,34))
  call loop_GGG_G_12(G0(:,:,:,36),wf(:,99),wf(:,-4),G0(:,:,:,47))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,35))
  call loop_GGG_G_23(G0(:,:,:,36),wf(:,-4),wf(:,99),G0(:,:,:,48))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,36))
  call loop_GGG_G_12(G0(:,:,:,36),wf(:,-4),wf(:,70),G0(:,:,:,49))
  call check_last_UV_W(l_switch,G0(:,:,:,49),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,37))
  call loop_GGG_G_12(G0(:,:,:,36),wf(:,70),wf(:,-4),G0(:,:,:,50))
  call check_last_UV_W(l_switch,G0(:,:,:,50),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,38))
  call loop_GGG_G_23(G0(:,:,:,36),wf(:,-4),wf(:,70),G0(:,:,:,51))
  call check_last_UV_W(l_switch,G0(:,:,:,51),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,39))
  call loop_UV_W(G0(:,:,:,36),Q(:,9),wf(:,-2),Q(:,4),G1(:,:,:,48))
  call loop_GGG_G_12(G1(:,:,:,48),wf(:,-5),wf(:,-4),G1(:,:,:,49))
  call check_last_UV_W(l_switch,G1(:,:,:,49),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,47))
  call loop_GGG_G_12(G1(:,:,:,48),wf(:,-4),wf(:,-5),G1(:,:,:,50))
  call check_last_UV_W(l_switch,G1(:,:,:,50),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,48))
  call loop_GGG_G_23(G1(:,:,:,48),wf(:,-5),wf(:,-4),G1(:,:,:,51))
  call check_last_UV_W(l_switch,G1(:,:,:,51),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,49))
  call loop_UV_W(G1(:,:,:,48),Q(:,13),wf(:,84),Q(:,48),G2(:,:,:,16))
  call check_last_UV_W(l_switch,G2(:,:,:,16),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,12))
  call loop_UV_W(G1(:,:,:,48),Q(:,13),wf(:,-5),Q(:,32),G2(:,:,:,17))
  call loop_UV_W(G2(:,:,:,17),Q(:,45),wf(:,-4),Q(:,16),G3(:,:,:,5))
  call check_last_UV_W(l_switch,G3(:,:,:,5),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,5))
  call loop_UV_W(G1(:,:,:,48),Q(:,13),wf(:,-4),Q(:,16),G2(:,:,:,18))
  call loop_UV_W(G2(:,:,:,18),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,6))
  call check_last_UV_W(l_switch,G3(:,:,:,6),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,6))
  call loop_GGG_G_12(G0(:,:,:,36),wf(:,-2),wf(:,84),G0(:,:,:,52))
  call check_last_UV_W(l_switch,G0(:,:,:,52),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,40))
  call loop_GGG_G_12(G0(:,:,:,36),wf(:,84),wf(:,-2),G0(:,:,:,53))
  call check_last_UV_W(l_switch,G0(:,:,:,53),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,41))
  call loop_GGG_G_23(G0(:,:,:,36),wf(:,-2),wf(:,84),G0(:,:,:,54))
  call check_last_UV_W(l_switch,G0(:,:,:,54),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,42))
  call loop_GGG_G_12(G0(:,:,:,36),wf(:,-5),wf(:,-2),G0(:,:,:,55))
  call loop_UV_W(G0(:,:,:,55),Q(:,45),wf(:,-4),Q(:,16),G1(:,:,:,52))
  call check_last_UV_W(l_switch,G1(:,:,:,52),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,50))
  call loop_GGG_G_12(G0(:,:,:,36),wf(:,-2),wf(:,-5),G0(:,:,:,56))
  call loop_UV_W(G0(:,:,:,56),Q(:,45),wf(:,-4),Q(:,16),G1(:,:,:,53))
  call check_last_UV_W(l_switch,G1(:,:,:,53),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,51))
  call loop_GGG_G_23(G0(:,:,:,36),wf(:,-5),wf(:,-2),G0(:,:,:,57))
  call loop_UV_W(G0(:,:,:,57),Q(:,45),wf(:,-4),Q(:,16),G1(:,:,:,54))
  call check_last_UV_W(l_switch,G1(:,:,:,54),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,52))
  call loop_UV_W(G0(:,:,:,36),Q(:,9),wf(:,-4),Q(:,16),G1(:,:,:,55))
  call loop_GGG_G_12(G1(:,:,:,55),wf(:,-5),wf(:,-2),G1(:,:,:,56))
  call check_last_UV_W(l_switch,G1(:,:,:,56),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,53))
  call loop_GGG_G_12(G1(:,:,:,55),wf(:,-2),wf(:,-5),G1(:,:,:,57))
  call check_last_UV_W(l_switch,G1(:,:,:,57),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,54))
  call loop_GGG_G_23(G1(:,:,:,55),wf(:,-5),wf(:,-2),G1(:,:,:,58))
  call check_last_UV_W(l_switch,G1(:,:,:,58),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,55))
  call loop_GGG_G_12(G1(:,:,:,55),wf(:,-5),wf(:,-1),G1(:,:,:,59))
  call check_last_UV_W(l_switch,G1(:,:,:,59),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,56))
  call loop_GGG_G_12(G1(:,:,:,55),wf(:,-1),wf(:,-5),G1(:,:,:,60))
  call check_last_UV_W(l_switch,G1(:,:,:,60),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,57))
  call loop_GGG_G_23(G1(:,:,:,55),wf(:,-5),wf(:,-1),G1(:,:,:,61))
  call check_last_UV_W(l_switch,G1(:,:,:,61),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,58))
  call loop_UV_W(G1(:,:,:,55),Q(:,25),wf(:,-5),Q(:,32),G2(:,:,:,19))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,19),wf(:,-2),wf(:,-1),G2tensor(:,59))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,19),wf(:,-1),wf(:,-2),G2tensor(:,60))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,19),wf(:,-2),wf(:,-1),G2tensor(:,61))
  call check_last_UV_W(l_switch,G2(:,:,:,19),Q(:,57),wf(:,105),Q(:,6),G3tensor(:,13))
  call loop_UV_W(G1(:,:,:,55),Q(:,25),wf(:,99),Q(:,34),G2(:,:,:,20))
  call check_last_UV_W(l_switch,G2(:,:,:,20),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,14))
  call loop_UV_W(G1(:,:,:,55),Q(:,25),wf(:,70),Q(:,36),G2(:,:,:,21))
  call check_last_UV_W(l_switch,G2(:,:,:,21),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,15))
  call loop_UV_W(G1(:,:,:,55),Q(:,25),wf(:,-2),Q(:,4),G2(:,:,:,22))
  call loop_UV_W(G2(:,:,:,22),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,7))
  call check_last_UV_W(l_switch,G3(:,:,:,7),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,7))
  call loop_GGG_G_12(G0(:,:,:,36),wf(:,-4),wf(:,-2),G0(:,:,:,58))
  call loop_UV_W(G0(:,:,:,58),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,62))
  call check_last_UV_W(l_switch,G1(:,:,:,62),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,62))
  call loop_GGG_G_12(G0(:,:,:,36),wf(:,-2),wf(:,-4),G0(:,:,:,59))
  call loop_UV_W(G0(:,:,:,59),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,63))
  call check_last_UV_W(l_switch,G1(:,:,:,63),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,63))
  call loop_GGG_G_23(G0(:,:,:,36),wf(:,-4),wf(:,-2),G0(:,:,:,60))
  call loop_UV_W(G0(:,:,:,60),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,64))
  call check_last_UV_W(l_switch,G1(:,:,:,64),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,64))
  call loop_UV_W(G0(:,:,:,36),Q(:,9),wf(:,-5),Q(:,32),G1(:,:,:,65))
  call loop_GGG_G_12(G1(:,:,:,65),wf(:,-4),wf(:,-2),G1(:,:,:,66))
  call check_last_UV_W(l_switch,G1(:,:,:,66),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,65))
  call loop_GGG_G_12(G1(:,:,:,65),wf(:,-2),wf(:,-4),G1(:,:,:,67))
  call check_last_UV_W(l_switch,G1(:,:,:,67),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,66))
  call loop_GGG_G_23(G1(:,:,:,65),wf(:,-4),wf(:,-2),G1(:,:,:,68))
  call check_last_UV_W(l_switch,G1(:,:,:,68),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,67))
  call loop_GGG_G_12(G1(:,:,:,65),wf(:,-4),wf(:,-1),G1(:,:,:,69))
  call check_last_UV_W(l_switch,G1(:,:,:,69),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,68))
  call loop_GGG_G_12(G1(:,:,:,65),wf(:,-1),wf(:,-4),G1(:,:,:,70))
  call check_last_UV_W(l_switch,G1(:,:,:,70),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,69))
  call loop_GGG_G_23(G1(:,:,:,65),wf(:,-4),wf(:,-1),G1(:,:,:,71))
  call check_last_UV_W(l_switch,G1(:,:,:,71),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,70))
  call loop_UV_W(G1(:,:,:,65),Q(:,41),wf(:,-4),Q(:,16),G2(:,:,:,23))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,23),wf(:,-2),wf(:,-1),G2tensor(:,71))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,23),wf(:,-1),wf(:,-2),G2tensor(:,72))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,23),wf(:,-2),wf(:,-1),G2tensor(:,73))
  call check_last_UV_W(l_switch,G2(:,:,:,23),Q(:,57),wf(:,105),Q(:,6),G3tensor(:,16))
  call loop_UV_W(G1(:,:,:,65),Q(:,41),wf(:,95),Q(:,18),G2(:,:,:,24))
  call check_last_UV_W(l_switch,G2(:,:,:,24),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,17))
  call loop_UV_W(G1(:,:,:,65),Q(:,41),wf(:,66),Q(:,20),G2(:,:,:,25))
  call check_last_UV_W(l_switch,G2(:,:,:,25),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,18))
  call loop_UV_W(G1(:,:,:,65),Q(:,41),wf(:,-2),Q(:,4),G2(:,:,:,26))
  call loop_UV_W(G2(:,:,:,26),Q(:,45),wf(:,-4),Q(:,16),G3(:,:,:,8))
  call check_last_UV_W(l_switch,G3(:,:,:,8),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,8))
  call loop_UV_W(G0(:,:,:,36),Q(:,9),wf(:,8),Q(:,52),G1(:,:,:,72))
  call check_last_UV_W(l_switch,G1(:,:,:,72),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,74))
  call loop_UV_W(G0(:,:,:,36),Q(:,9),wf(:,11),Q(:,52),G1(:,:,:,73))
  call check_last_UV_W(l_switch,G1(:,:,:,73),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,75))
  call loop_UV_W(G0(:,:,:,36),Q(:,9),wf(:,12),Q(:,52),G1(:,:,:,74))
  call check_last_UV_W(l_switch,G1(:,:,:,74),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,76))
  call loop_GGG_G_12(G0(:,:,:,36),wf(:,-1),wf(:,84),G0(:,:,:,61))
  call check_last_UV_W(l_switch,G0(:,:,:,61),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,43))
  call loop_GGG_G_12(G0(:,:,:,36),wf(:,84),wf(:,-1),G0(:,:,:,62))
  call check_last_UV_W(l_switch,G0(:,:,:,62),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,44))
  call loop_GGG_G_23(G0(:,:,:,36),wf(:,-1),wf(:,84),G0(:,:,:,63))
  call check_last_UV_W(l_switch,G0(:,:,:,63),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,45))
  call loop_GGG_G_12(G0(:,:,:,36),wf(:,-5),wf(:,-1),G0(:,:,:,64))
  call loop_UV_W(G0(:,:,:,64),Q(:,43),wf(:,-4),Q(:,16),G1(:,:,:,75))
  call check_last_UV_W(l_switch,G1(:,:,:,75),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,77))
  call loop_GGG_G_12(G0(:,:,:,36),wf(:,-1),wf(:,-5),G0(:,:,:,65))
  call loop_UV_W(G0(:,:,:,65),Q(:,43),wf(:,-4),Q(:,16),G1(:,:,:,76))
  call check_last_UV_W(l_switch,G1(:,:,:,76),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,78))
  call loop_GGG_G_23(G0(:,:,:,36),wf(:,-5),wf(:,-1),G0(:,:,:,66))
  call loop_UV_W(G0(:,:,:,66),Q(:,43),wf(:,-4),Q(:,16),G1(:,:,:,77))
  call check_last_UV_W(l_switch,G1(:,:,:,77),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,79))
  call loop_GGG_G_12(G0(:,:,:,36),wf(:,-4),wf(:,-1),G0(:,:,:,67))
  call loop_UV_W(G0(:,:,:,67),Q(:,27),wf(:,-5),Q(:,32),G1(:,:,:,78))
  call check_last_UV_W(l_switch,G1(:,:,:,78),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,80))
  call loop_GGG_G_12(G0(:,:,:,36),wf(:,-1),wf(:,-4),G0(:,:,:,68))
  call loop_UV_W(G0(:,:,:,68),Q(:,27),wf(:,-5),Q(:,32),G1(:,:,:,79))
  call check_last_UV_W(l_switch,G1(:,:,:,79),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,81))
  call loop_GGG_G_23(G0(:,:,:,36),wf(:,-4),wf(:,-1),G0(:,:,:,69))
  call loop_UV_W(G0(:,:,:,69),Q(:,27),wf(:,-5),Q(:,32),G1(:,:,:,80))
  call check_last_UV_W(l_switch,G1(:,:,:,80),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,82))
  call loop_UV_W(G0(:,:,:,36),Q(:,9),wf(:,26),Q(:,50),G1(:,:,:,81))
  call check_last_UV_W(l_switch,G1(:,:,:,81),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,83))
  call loop_UV_W(G0(:,:,:,36),Q(:,9),wf(:,29),Q(:,50),G1(:,:,:,82))
  call check_last_UV_W(l_switch,G1(:,:,:,82),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,84))
  call loop_UV_W(G0(:,:,:,36),Q(:,9),wf(:,30),Q(:,50),G1(:,:,:,83))
  call check_last_UV_W(l_switch,G1(:,:,:,83),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,85))
  call loop_UV_W(G0(:,:,:,36),Q(:,9),wf(:,84),Q(:,48),G1(:,:,:,84))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,84),wf(:,-2),wf(:,-1),G1tensor(:,46))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,84),wf(:,-1),wf(:,-2),G1tensor(:,47))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,84),wf(:,-2),wf(:,-1),G1tensor(:,48))
  call check_last_UV_W(l_switch,G1(:,:,:,84),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,86))
  call loop_UV_W(G0(:,:,:,36),Q(:,9),wf(:,247),Q(:,50),G1(:,:,:,85))
  call check_last_UV_W(l_switch,G1(:,:,:,85),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,87))
  call loop_UV_W(G0(:,:,:,36),Q(:,9),wf(:,95),Q(:,18),G1(:,:,:,86))
  call loop_UV_W(G1(:,:,:,86),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,27))
  call check_last_UV_W(l_switch,G2(:,:,:,27),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,19))
  call loop_UV_W(G0(:,:,:,36),Q(:,9),wf(:,265),Q(:,52),G1(:,:,:,87))
  call check_last_UV_W(l_switch,G1(:,:,:,87),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,88))
  call loop_UV_W(G0(:,:,:,36),Q(:,9),wf(:,66),Q(:,20),G1(:,:,:,88))
  call loop_UV_W(G1(:,:,:,88),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,28))
  call check_last_UV_W(l_switch,G2(:,:,:,28),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,20))
  call loop_UV_W(G0(:,:,:,36),Q(:,9),wf(:,248),Q(:,50),G1(:,:,:,89))
  call check_last_UV_W(l_switch,G1(:,:,:,89),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,89))
  call loop_UV_W(G0(:,:,:,36),Q(:,9),wf(:,99),Q(:,34),G1(:,:,:,90))
  call loop_UV_W(G1(:,:,:,90),Q(:,43),wf(:,-4),Q(:,16),G2(:,:,:,29))
  call check_last_UV_W(l_switch,G2(:,:,:,29),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,21))
  call loop_UV_W(G0(:,:,:,36),Q(:,9),wf(:,266),Q(:,52),G1(:,:,:,91))
  call check_last_UV_W(l_switch,G1(:,:,:,91),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,90))
  call loop_UV_W(G0(:,:,:,36),Q(:,9),wf(:,70),Q(:,36),G1(:,:,:,92))
  call loop_UV_W(G1(:,:,:,92),Q(:,45),wf(:,-4),Q(:,16),G2(:,:,:,30))
  call check_last_UV_W(l_switch,G2(:,:,:,30),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,22))
  call loop_UV_W(G0(:,:,:,36),Q(:,9),wf(:,249),Q(:,50),G1(:,:,:,93))
  call check_last_UV_W(l_switch,G1(:,:,:,93),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,91))
  call loop_UV_W(G0(:,:,:,36),Q(:,9),wf(:,267),Q(:,52),G1(:,:,:,94))
  call check_last_UV_W(l_switch,G1(:,:,:,94),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,92))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,0),G0(:,:,:,70))
  call loop_GGG_G_12(G0(:,:,:,70),wf(:,-5),wf(:,-4),G0(:,:,:,71))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,71),wf(:,-2),wf(:,-1),G0tensor(:,19))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,71),wf(:,-1),wf(:,-2),G0tensor(:,20))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,71),wf(:,-2),wf(:,-1),G0tensor(:,21))
  call check_last_UV_W(l_switch,G0(:,:,:,71),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,49))
  call loop_GGG_G_12(G0(:,:,:,70),wf(:,-4),wf(:,-5),G0(:,:,:,72))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,72),wf(:,-2),wf(:,-1),G0tensor(:,22))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,72),wf(:,-1),wf(:,-2),G0tensor(:,23))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,72),wf(:,-2),wf(:,-1),G0tensor(:,24))
  call check_last_UV_W(l_switch,G0(:,:,:,72),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,50))
  call loop_GGG_G_23(G0(:,:,:,70),wf(:,-5),wf(:,-4),G0(:,:,:,73))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,73),wf(:,-2),wf(:,-1),G0tensor(:,25))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,73),wf(:,-1),wf(:,-2),G0tensor(:,26))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,73),wf(:,-2),wf(:,-1),G0tensor(:,27))
  call check_last_UV_W(l_switch,G0(:,:,:,73),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,51))
  call loop_GGG_G_12(G0(:,:,:,70),wf(:,-5),wf(:,95),G0(:,:,:,74))
  call check_last_UV_W(l_switch,G0(:,:,:,74),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,52))
  call loop_GGG_G_12(G0(:,:,:,70),wf(:,95),wf(:,-5),G0(:,:,:,75))
  call check_last_UV_W(l_switch,G0(:,:,:,75),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,53))
  call loop_GGG_G_23(G0(:,:,:,70),wf(:,-5),wf(:,95),G0(:,:,:,76))
  call check_last_UV_W(l_switch,G0(:,:,:,76),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,54))
  call loop_GGG_G_12(G0(:,:,:,70),wf(:,-5),wf(:,66),G0(:,:,:,77))
  call check_last_UV_W(l_switch,G0(:,:,:,77),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,55))
  call loop_GGG_G_12(G0(:,:,:,70),wf(:,66),wf(:,-5),G0(:,:,:,78))
  call check_last_UV_W(l_switch,G0(:,:,:,78),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,56))
  call loop_GGG_G_23(G0(:,:,:,70),wf(:,-5),wf(:,66),G0(:,:,:,79))
  call check_last_UV_W(l_switch,G0(:,:,:,79),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,57))
  call loop_GGG_G_12(G0(:,:,:,70),wf(:,-4),wf(:,99),G0(:,:,:,80))
  call check_last_UV_W(l_switch,G0(:,:,:,80),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,58))
  call loop_GGG_G_12(G0(:,:,:,70),wf(:,99),wf(:,-4),G0(:,:,:,81))
  call check_last_UV_W(l_switch,G0(:,:,:,81),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,59))
  call loop_GGG_G_23(G0(:,:,:,70),wf(:,-4),wf(:,99),G0(:,:,:,82))
  call check_last_UV_W(l_switch,G0(:,:,:,82),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,60))
  call loop_GGG_G_12(G0(:,:,:,70),wf(:,-4),wf(:,70),G0(:,:,:,83))
  call check_last_UV_W(l_switch,G0(:,:,:,83),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,61))
  call loop_GGG_G_12(G0(:,:,:,70),wf(:,70),wf(:,-4),G0(:,:,:,84))
  call check_last_UV_W(l_switch,G0(:,:,:,84),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,62))
  call loop_GGG_G_23(G0(:,:,:,70),wf(:,-4),wf(:,70),G0(:,:,:,85))
  call check_last_UV_W(l_switch,G0(:,:,:,85),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,63))
  call loop_UV_W(G0(:,:,:,70),Q(:,9),wf(:,-2),Q(:,4),G1(:,:,:,95))
  call loop_GGG_G_12(G1(:,:,:,95),wf(:,-5),wf(:,-4),G1(:,:,:,96))
  call check_last_UV_W(l_switch,G1(:,:,:,96),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,93))
  call loop_GGG_G_12(G1(:,:,:,95),wf(:,-4),wf(:,-5),G1(:,:,:,97))
  call check_last_UV_W(l_switch,G1(:,:,:,97),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,94))
  call loop_GGG_G_23(G1(:,:,:,95),wf(:,-5),wf(:,-4),G1(:,:,:,98))
  call check_last_UV_W(l_switch,G1(:,:,:,98),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,95))
  call loop_UV_W(G1(:,:,:,95),Q(:,13),wf(:,84),Q(:,48),G2(:,:,:,31))
  call check_last_UV_W(l_switch,G2(:,:,:,31),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,23))
  call loop_UV_W(G1(:,:,:,95),Q(:,13),wf(:,-5),Q(:,32),G2(:,:,:,32))
  call loop_UV_W(G2(:,:,:,32),Q(:,45),wf(:,-4),Q(:,16),G3(:,:,:,9))
  call check_last_UV_W(l_switch,G3(:,:,:,9),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,9))
  call loop_UV_W(G1(:,:,:,95),Q(:,13),wf(:,-4),Q(:,16),G2(:,:,:,33))
  call loop_UV_W(G2(:,:,:,33),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,10))
  call check_last_UV_W(l_switch,G3(:,:,:,10),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,10))
  call loop_GGG_G_12(G0(:,:,:,70),wf(:,-2),wf(:,84),G0(:,:,:,86))
  call check_last_UV_W(l_switch,G0(:,:,:,86),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,64))
  call loop_GGG_G_12(G0(:,:,:,70),wf(:,84),wf(:,-2),G0(:,:,:,87))
  call check_last_UV_W(l_switch,G0(:,:,:,87),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,65))
  call loop_GGG_G_23(G0(:,:,:,70),wf(:,-2),wf(:,84),G0(:,:,:,88))
  call check_last_UV_W(l_switch,G0(:,:,:,88),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,66))
  call loop_GGG_G_12(G0(:,:,:,70),wf(:,-5),wf(:,-2),G0(:,:,:,89))
  call loop_UV_W(G0(:,:,:,89),Q(:,45),wf(:,-4),Q(:,16),G1(:,:,:,99))
  call check_last_UV_W(l_switch,G1(:,:,:,99),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,96))
  call loop_GGG_G_12(G0(:,:,:,70),wf(:,-2),wf(:,-5),G0(:,:,:,90))
  call loop_UV_W(G0(:,:,:,90),Q(:,45),wf(:,-4),Q(:,16),G1(:,:,:,100))
  call check_last_UV_W(l_switch,G1(:,:,:,100),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,97))
  call loop_GGG_G_23(G0(:,:,:,70),wf(:,-5),wf(:,-2),G0(:,:,:,91))
  call loop_UV_W(G0(:,:,:,91),Q(:,45),wf(:,-4),Q(:,16),G1(:,:,:,101))
  call check_last_UV_W(l_switch,G1(:,:,:,101),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,98))
  call loop_UV_W(G0(:,:,:,70),Q(:,9),wf(:,-4),Q(:,16),G1(:,:,:,102))
  call loop_GGG_G_12(G1(:,:,:,102),wf(:,-5),wf(:,-2),G1(:,:,:,103))
  call check_last_UV_W(l_switch,G1(:,:,:,103),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,99))
  call loop_GGG_G_12(G1(:,:,:,102),wf(:,-2),wf(:,-5),G1(:,:,:,104))
  call check_last_UV_W(l_switch,G1(:,:,:,104),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,100))
  call loop_GGG_G_23(G1(:,:,:,102),wf(:,-5),wf(:,-2),G1(:,:,:,105))
  call check_last_UV_W(l_switch,G1(:,:,:,105),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,101))
  call loop_GGG_G_12(G1(:,:,:,102),wf(:,-5),wf(:,-1),G1(:,:,:,106))
  call check_last_UV_W(l_switch,G1(:,:,:,106),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,102))
  call loop_GGG_G_12(G1(:,:,:,102),wf(:,-1),wf(:,-5),G1(:,:,:,107))
  call check_last_UV_W(l_switch,G1(:,:,:,107),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,103))
  call loop_GGG_G_23(G1(:,:,:,102),wf(:,-5),wf(:,-1),G1(:,:,:,108))
  call check_last_UV_W(l_switch,G1(:,:,:,108),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,104))
  call loop_UV_W(G1(:,:,:,102),Q(:,25),wf(:,-5),Q(:,32),G2(:,:,:,34))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,34),wf(:,-2),wf(:,-1),G2tensor(:,105))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,34),wf(:,-1),wf(:,-2),G2tensor(:,106))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,34),wf(:,-2),wf(:,-1),G2tensor(:,107))
  call check_last_UV_W(l_switch,G2(:,:,:,34),Q(:,57),wf(:,105),Q(:,6),G3tensor(:,24))
  call loop_UV_W(G1(:,:,:,102),Q(:,25),wf(:,99),Q(:,34),G2(:,:,:,35))
  call check_last_UV_W(l_switch,G2(:,:,:,35),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,25))
  call loop_UV_W(G1(:,:,:,102),Q(:,25),wf(:,70),Q(:,36),G2(:,:,:,36))
  call check_last_UV_W(l_switch,G2(:,:,:,36),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,26))
  call loop_UV_W(G1(:,:,:,102),Q(:,25),wf(:,-2),Q(:,4),G2(:,:,:,37))
  call loop_UV_W(G2(:,:,:,37),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,11))
  call check_last_UV_W(l_switch,G3(:,:,:,11),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,11))
  call loop_GGG_G_12(G0(:,:,:,70),wf(:,-4),wf(:,-2),G0(:,:,:,92))
  call loop_UV_W(G0(:,:,:,92),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,109))
  call check_last_UV_W(l_switch,G1(:,:,:,109),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,108))
  call loop_GGG_G_12(G0(:,:,:,70),wf(:,-2),wf(:,-4),G0(:,:,:,93))
  call loop_UV_W(G0(:,:,:,93),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,110))
  call check_last_UV_W(l_switch,G1(:,:,:,110),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,109))
  call loop_GGG_G_23(G0(:,:,:,70),wf(:,-4),wf(:,-2),G0(:,:,:,94))
  call loop_UV_W(G0(:,:,:,94),Q(:,29),wf(:,-5),Q(:,32),G1(:,:,:,111))
  call check_last_UV_W(l_switch,G1(:,:,:,111),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,110))
  call loop_UV_W(G0(:,:,:,70),Q(:,9),wf(:,-5),Q(:,32),G1(:,:,:,112))
  call loop_GGG_G_12(G1(:,:,:,112),wf(:,-4),wf(:,-2),G1(:,:,:,113))
  call check_last_UV_W(l_switch,G1(:,:,:,113),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,111))
  call loop_GGG_G_12(G1(:,:,:,112),wf(:,-2),wf(:,-4),G1(:,:,:,114))
  call check_last_UV_W(l_switch,G1(:,:,:,114),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,112))
  call loop_GGG_G_23(G1(:,:,:,112),wf(:,-4),wf(:,-2),G1(:,:,:,115))
  call check_last_UV_W(l_switch,G1(:,:,:,115),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,113))
  call loop_GGG_G_12(G1(:,:,:,112),wf(:,-4),wf(:,-1),G1(:,:,:,116))
  call check_last_UV_W(l_switch,G1(:,:,:,116),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,114))
  call loop_GGG_G_12(G1(:,:,:,112),wf(:,-1),wf(:,-4),G1(:,:,:,117))
  call check_last_UV_W(l_switch,G1(:,:,:,117),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,115))
  call loop_GGG_G_23(G1(:,:,:,112),wf(:,-4),wf(:,-1),G1(:,:,:,118))
  call check_last_UV_W(l_switch,G1(:,:,:,118),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,116))
  call loop_UV_W(G1(:,:,:,112),Q(:,41),wf(:,-4),Q(:,16),G2(:,:,:,38))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,38),wf(:,-2),wf(:,-1),G2tensor(:,117))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,38),wf(:,-1),wf(:,-2),G2tensor(:,118))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,38),wf(:,-2),wf(:,-1),G2tensor(:,119))
  call check_last_UV_W(l_switch,G2(:,:,:,38),Q(:,57),wf(:,105),Q(:,6),G3tensor(:,27))
  call loop_UV_W(G1(:,:,:,112),Q(:,41),wf(:,95),Q(:,18),G2(:,:,:,39))
  call check_last_UV_W(l_switch,G2(:,:,:,39),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,28))
  call loop_UV_W(G1(:,:,:,112),Q(:,41),wf(:,66),Q(:,20),G2(:,:,:,40))
  call check_last_UV_W(l_switch,G2(:,:,:,40),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,29))
  call loop_UV_W(G1(:,:,:,112),Q(:,41),wf(:,-2),Q(:,4),G2(:,:,:,41))
  call loop_UV_W(G2(:,:,:,41),Q(:,45),wf(:,-4),Q(:,16),G3(:,:,:,12))
  call check_last_UV_W(l_switch,G3(:,:,:,12),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,12))
  call loop_UV_W(G0(:,:,:,70),Q(:,9),wf(:,8),Q(:,52),G1(:,:,:,119))
  call check_last_UV_W(l_switch,G1(:,:,:,119),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,120))
  call loop_UV_W(G0(:,:,:,70),Q(:,9),wf(:,11),Q(:,52),G1(:,:,:,120))
  call check_last_UV_W(l_switch,G1(:,:,:,120),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,121))
  call loop_UV_W(G0(:,:,:,70),Q(:,9),wf(:,12),Q(:,52),G1(:,:,:,121))
  call check_last_UV_W(l_switch,G1(:,:,:,121),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,122))
  call loop_GGG_G_12(G0(:,:,:,70),wf(:,-1),wf(:,84),G0(:,:,:,95))
  call check_last_UV_W(l_switch,G0(:,:,:,95),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,67))
  call loop_GGG_G_12(G0(:,:,:,70),wf(:,84),wf(:,-1),G0(:,:,:,96))
  call check_last_UV_W(l_switch,G0(:,:,:,96),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,68))
  call loop_GGG_G_23(G0(:,:,:,70),wf(:,-1),wf(:,84),G0(:,:,:,97))
  call check_last_UV_W(l_switch,G0(:,:,:,97),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,69))
  call loop_GGG_G_12(G0(:,:,:,70),wf(:,-5),wf(:,-1),G0(:,:,:,98))
  call loop_UV_W(G0(:,:,:,98),Q(:,43),wf(:,-4),Q(:,16),G1(:,:,:,122))
  call check_last_UV_W(l_switch,G1(:,:,:,122),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,123))
  call loop_GGG_G_12(G0(:,:,:,70),wf(:,-1),wf(:,-5),G0(:,:,:,99))
  call loop_UV_W(G0(:,:,:,99),Q(:,43),wf(:,-4),Q(:,16),G1(:,:,:,123))
  call check_last_UV_W(l_switch,G1(:,:,:,123),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,124))
  call loop_GGG_G_23(G0(:,:,:,70),wf(:,-5),wf(:,-1),G0(:,:,:,100))
  call loop_UV_W(G0(:,:,:,100),Q(:,43),wf(:,-4),Q(:,16),G1(:,:,:,124))
  call check_last_UV_W(l_switch,G1(:,:,:,124),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,125))
  call loop_GGG_G_12(G0(:,:,:,70),wf(:,-4),wf(:,-1),G0(:,:,:,101))
  call loop_UV_W(G0(:,:,:,101),Q(:,27),wf(:,-5),Q(:,32),G1(:,:,:,125))
  call check_last_UV_W(l_switch,G1(:,:,:,125),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,126))
  call loop_GGG_G_12(G0(:,:,:,70),wf(:,-1),wf(:,-4),G0(:,:,:,102))
  call loop_UV_W(G0(:,:,:,102),Q(:,27),wf(:,-5),Q(:,32),G1(:,:,:,126))
  call check_last_UV_W(l_switch,G1(:,:,:,126),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,127))
  call loop_GGG_G_23(G0(:,:,:,70),wf(:,-4),wf(:,-1),G0(:,:,:,103))
  call loop_UV_W(G0(:,:,:,103),Q(:,27),wf(:,-5),Q(:,32),G1(:,:,:,127))
  call check_last_UV_W(l_switch,G1(:,:,:,127),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,128))
  call loop_UV_W(G0(:,:,:,70),Q(:,9),wf(:,26),Q(:,50),G1(:,:,:,128))
  call check_last_UV_W(l_switch,G1(:,:,:,128),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,129))
  call loop_UV_W(G0(:,:,:,70),Q(:,9),wf(:,29),Q(:,50),G1(:,:,:,129))
  call check_last_UV_W(l_switch,G1(:,:,:,129),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,130))
  call loop_UV_W(G0(:,:,:,70),Q(:,9),wf(:,30),Q(:,50),G1(:,:,:,130))
  call check_last_UV_W(l_switch,G1(:,:,:,130),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,131))
  call loop_UV_W(G0(:,:,:,70),Q(:,9),wf(:,84),Q(:,48),G1(:,:,:,131))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,131),wf(:,-2),wf(:,-1),G1tensor(:,70))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,131),wf(:,-1),wf(:,-2),G1tensor(:,71))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,131),wf(:,-2),wf(:,-1),G1tensor(:,72))
  call check_last_UV_W(l_switch,G1(:,:,:,131),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,132))
  call loop_UV_W(G0(:,:,:,70),Q(:,9),wf(:,247),Q(:,50),G1(:,:,:,132))
  call check_last_UV_W(l_switch,G1(:,:,:,132),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,133))
  call loop_UV_W(G0(:,:,:,70),Q(:,9),wf(:,95),Q(:,18),G1(:,:,:,133))
  call loop_UV_W(G1(:,:,:,133),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,42))
  call check_last_UV_W(l_switch,G2(:,:,:,42),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,30))
  call loop_UV_W(G0(:,:,:,70),Q(:,9),wf(:,265),Q(:,52),G1(:,:,:,134))
  call check_last_UV_W(l_switch,G1(:,:,:,134),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,134))
  call loop_UV_W(G0(:,:,:,70),Q(:,9),wf(:,66),Q(:,20),G1(:,:,:,135))
  call loop_UV_W(G1(:,:,:,135),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,43))
  call check_last_UV_W(l_switch,G2(:,:,:,43),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,31))
  call loop_UV_W(G0(:,:,:,70),Q(:,9),wf(:,248),Q(:,50),G1(:,:,:,136))
  call check_last_UV_W(l_switch,G1(:,:,:,136),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,135))
  call loop_UV_W(G0(:,:,:,70),Q(:,9),wf(:,99),Q(:,34),G1(:,:,:,137))
  call loop_UV_W(G1(:,:,:,137),Q(:,43),wf(:,-4),Q(:,16),G2(:,:,:,44))
  call check_last_UV_W(l_switch,G2(:,:,:,44),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,32))
  call loop_UV_W(G0(:,:,:,70),Q(:,9),wf(:,266),Q(:,52),G1(:,:,:,138))
  call check_last_UV_W(l_switch,G1(:,:,:,138),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,136))
  call loop_UV_W(G0(:,:,:,70),Q(:,9),wf(:,70),Q(:,36),G1(:,:,:,139))
  call loop_UV_W(G1(:,:,:,139),Q(:,45),wf(:,-4),Q(:,16),G2(:,:,:,45))
  call check_last_UV_W(l_switch,G2(:,:,:,45),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,33))
  call loop_UV_W(G0(:,:,:,70),Q(:,9),wf(:,249),Q(:,50),G1(:,:,:,140))
  call check_last_UV_W(l_switch,G1(:,:,:,140),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,137))
  call loop_UV_W(G0(:,:,:,70),Q(:,9),wf(:,267),Q(:,52),G1(:,:,:,141))
  call check_last_UV_W(l_switch,G1(:,:,:,141),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,138))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(9)-M(11)-M(13)-M(16)-M(17)-M(19)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(42)+M(43)+M(49)+M(53)+M(61)+M(65)+M(66)+M(67)+M(69)+M(70)+M(73)+M(78)+M(80)+M(81)+M(90)+M(92)+M(93)+M(98)+M(99) &
    +M(100)+M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(179)+M(247)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,1)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(48)+M(49)+M(51)+M(60)+M(61)+M(63)+M(68)+M(71)+M(72)+M(74)+M(75)+M(76)+M(80)+M(88)+M(92)+M(98)+M(99) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(134)+M(202)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,10)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)+M(63)-M(65)-M(66)-M(67)+M(68)-M(69)-M(70)+M(71)+M(72)-M(73)+M(74)+M(75)+M(76)-M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(134)-M(179)+M(202)-M(247)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,19)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(41)+M(49)+M(54)+M(55)+M(61)+M(66)+M(67)+M(68)+M(69)+M(70)+M(73)+M(77)+M(78)+M(81)+M(89)+M(90)+M(93)+M(98)+M(99) &
    +M(100)+M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(185)+M(245)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,2)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(9)-M(11)-M(13)-M(16)-M(18)-M(19)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(37) &
    -M(40)+M(48)+M(49)+M(51)+M(53)+M(54)+M(55)+M(60)+M(61)+M(63)+M(65)+M(71)+M(72)+M(74)+M(75)+M(76)+M(77)+M(88)+M(89)+M(98)+M(99) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(158)+M(201)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,11)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)+M(17)-M(18)-M(19)+M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(41)+M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)-M(66)-M(67)-M(68)-M(69)-M(70)+M(71)+M(72)-M(73)+M(74)+M(75)+M(76)-M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(158)-M(185)+M(201)-M(245)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,20)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)-M(5)+M(6)+M(7)-M(8)+M(13)-M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42)-M(43)-M(53) &
    +M(54)+M(55)-M(65)+M(68)+M(77)-M(80)+M(89)-M(92)+M(101)+M(102)-M(103)-M(104)-M(105)+M(106)-M(109)-M(111)+M(115)+M(117)+M(121) &
    -M(123))+c(6)*(-M(179)+M(185)+M(245)-M(247)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,3)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(5)-M(6)+M(7)-M(8)-M(13)+M(14)-M(19)+M(20)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42)-M(43)+M(53) &
    +M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92)+M(101)+M(102)-M(103)-M(104)-M(105)+M(106)-M(109)-M(111)+M(115)+M(117)+M(121) &
    -M(123))+c(6)*(-M(134)+M(158)+M(201)-M(202)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,12)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(-M(134)+M(158)+M(179)-M(185)+M(201) &
    -M(202)-M(245)+M(247)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,21)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(9)-M(11)-M(13)-M(16)-M(18)-M(19)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(37) &
    -M(40)+M(42)+M(43)+M(52)+M(53)+M(64)+M(65)+M(66)+M(67)+M(69)+M(70)+M(76)+M(78)+M(80)+M(81)+M(86)+M(87)+M(88)+M(90)+M(92)+M(93) &
    +M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(180)+M(223)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,4)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(48)+M(51)+M(52)+M(60)+M(63)+M(64)+M(68)+M(71)+M(72)+M(73)+M(74)+M(75)+M(80)+M(86)+M(87)+M(92) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(136)+M(196)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,13)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)-M(17)+M(18)+M(19)-M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)+M(63)-M(65)-M(66)-M(67)+M(68)-M(69)-M(70)+M(71)+M(72)+M(73)+M(74)+M(75)-M(76)-M(78)-M(81)-M(88)-M(90)-M(93) &
    +M(100))+c(6)*(M(136)-M(180)+M(196)-M(223)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,22)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(41)+M(52)+M(54)+M(55)+M(64)+M(66)+M(67)+M(68)+M(69)+M(70)+M(76)+M(77)+M(78)+M(81)+M(86)+M(87)+M(88)+M(89)+M(90)+M(93) &
    +M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(186)+M(221)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,5)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(9)-M(11)-M(13)-M(16)-M(17)-M(19)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(48)+M(51)+M(52)+M(53)+M(54)+M(55)+M(60)+M(63)+M(64)+M(65)+M(71)+M(72)+M(73)+M(74)+M(75)+M(77)+M(86)+M(87)+M(89) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(160)+M(195)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,14)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)+M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)-M(66)-M(67)-M(68)-M(69)-M(70)+M(71)+M(72)+M(73)+M(74)+M(75)-M(76)-M(78)-M(81)-M(88)-M(90)-M(93) &
    +M(100))+c(6)*(M(160)-M(186)+M(195)-M(221)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,23)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(5)+M(6)-M(7)+M(8)+M(13)-M(14)+M(19)-M(20)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42)-M(43)-M(53) &
    +M(54)+M(55)-M(65)+M(68)+M(77)-M(80)+M(89)-M(92)+M(101)+M(102)-M(103)+M(104)-M(105)-M(106)-M(109)-M(111)+M(115)-M(117)+M(121) &
    +M(123))+c(6)*(-M(180)+M(186)+M(221)-M(223)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,6)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)+M(5)-M(6)-M(7)+M(8)-M(13)+M(14)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)-M(41)-M(42)-M(43)+M(53) &
    +M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92)+M(101)+M(102)-M(103)+M(104)-M(105)-M(106)-M(109)-M(111)+M(115)-M(117)+M(121) &
    +M(123))+c(6)*(-M(136)+M(160)+M(195)-M(196)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,15)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(-M(136)+M(160)+M(180)-M(186)+M(195) &
    -M(196)-M(221)+M(223)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,24)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)-M(61) &
    +M(64)-M(73)+M(76)+M(86)+M(87)+M(88)-M(98)-M(99)-M(100)-M(104)+M(106)+M(110)-M(112)+M(117)-M(118)-M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(179)+M(180)+M(223)-M(247)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,7)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)-M(61) &
    +M(64)+M(73)-M(76)+M(86)+M(87)-M(88)-M(98)-M(99)+M(100)-M(104)+M(106)+M(110)-M(112)+M(117)-M(118)-M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(134)+M(136)+M(196)-M(202)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,16)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(134)+M(136)+M(179)-M(180) &
    +M(196)-M(202)-M(223)+M(247)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,25)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)-M(49)+M(52)-M(61) &
    +M(64)-M(73)+M(76)+M(86)+M(87)+M(88)-M(98)-M(99)-M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(185)+M(186)+M(221)-M(245)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,8)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)-M(49)+M(52)-M(61) &
    +M(64)+M(73)-M(76)+M(86)+M(87)-M(88)-M(98)-M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(158)+M(160)+M(195)-M(201)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,17)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(158)+M(160)+M(185)-M(186) &
    +M(195)-M(201)-M(221)+M(245)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,26)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(37)-M(38)-M(39)+M(40)+M(104)-M(106)-M(117)+M(123))+c(6)*(M(179)-M(180)-M(185)+M(186) &
    +M(221)-M(223)-M(245)+M(247)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,9)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(37)-M(38)-M(39)+M(40)+M(104)-M(106)-M(117)+M(123))+c(6)*(M(134)-M(136)-M(158)+M(160) &
    +M(195)-M(196)-M(201)+M(202)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,18)
  Gcoeff = (c(6)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247)))
  T3sum(1:1,7) = T3sum(1:1,7) + Gcoeff * G0tensor(:,27)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)-M(5)+M(6)+M(7)-M(8)+M(13)-M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42)-M(43)-M(53) &
    +M(54)+M(55)-M(65)+M(68)+M(77)-M(80)+M(89)-M(92)+M(101)+M(102)-M(103)-M(104)-M(105)+M(106)-M(109)-M(111)+M(115)+M(117)+M(121) &
    -M(123))+c(6)*(-M(179)+M(185)+M(245)-M(247))) * den(41)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(5)-M(6)+M(7)-M(8)-M(13)+M(14)-M(19)+M(20)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42)-M(43)+M(53) &
    +M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92)+M(101)+M(102)-M(103)-M(104)-M(105)+M(106)-M(109)-M(111)+M(115)+M(117)+M(121) &
    -M(123))+c(6)*(-M(134)+M(158)+M(201)-M(202))) * den(41)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,25)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(-M(134)+M(158)+M(179)-M(185)+M(201) &
    -M(202)-M(245)+M(247))) * den(41)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,49)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(5)+M(6)-M(7)+M(8)+M(13)-M(14)+M(19)-M(20)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42)-M(43)-M(53) &
    +M(54)+M(55)-M(65)+M(68)+M(77)-M(80)+M(89)-M(92)+M(101)+M(102)-M(103)+M(104)-M(105)-M(106)-M(109)-M(111)+M(115)-M(117)+M(121) &
    +M(123))+c(6)*(-M(180)+M(186)+M(221)-M(223))) * den(41)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,2)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)+M(5)-M(6)-M(7)+M(8)-M(13)+M(14)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)-M(41)-M(42)-M(43)+M(53) &
    +M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92)+M(101)+M(102)-M(103)+M(104)-M(105)-M(106)-M(109)-M(111)+M(115)-M(117)+M(121) &
    +M(123))+c(6)*(-M(136)+M(160)+M(195)-M(196))) * den(41)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,26)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(-M(136)+M(160)+M(180)-M(186)+M(195) &
    -M(196)-M(221)+M(223))) * den(41)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,50)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(37)-M(38)-M(39)+M(40)+M(104)-M(106)-M(117)+M(123))+c(6)*(M(179)-M(180)-M(185)+M(186) &
    +M(221)-M(223)-M(245)+M(247))) * den(41)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,3)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(37)-M(38)-M(39)+M(40)+M(104)-M(106)-M(117)+M(123))+c(6)*(M(134)-M(136)-M(158)+M(160) &
    +M(195)-M(196)-M(201)+M(202))) * den(41)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,27)
  Gcoeff = (c(6)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(41)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,51)
  Gcoeff = (c(4)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)+M(47)+M(48)-M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)+M(79)+M(91)-M(98)-M(101)-M(106)+M(107)-M(112)+M(113)+M(114)-M(115)+M(116)-M(117)-M(118) &
    +M(120)+M(122))+c(6)*(-M(185)+M(187)+M(231)-M(245))) * den(35)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,4)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)-M(17)+M(18)+M(23)-M(24)+M(27)-M(28)-M(33)+M(34)+M(47)-M(48)-M(49) &
    -M(54)+M(59)+M(66)-M(71)-M(77)+M(78)+M(79)+M(91)-M(98)-M(101)-M(106)+M(107)-M(112)+M(113)+M(114)-M(115)+M(116)-M(117)-M(118) &
    +M(120)+M(122))+c(6)*(-M(158)+M(168)+M(198)-M(201))) * den(35)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,28)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(6)*(-M(158)+M(168)+M(185)-M(187) &
    +M(198)-M(201)-M(231)+M(245))) * den(35)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,52)
  Gcoeff = (c(4)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)+M(47)+M(48)+M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98)-M(101)+M(106)+M(107)+M(112)+M(113)-M(114)-M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(M(147)-M(189)+M(190)-M(207))) * den(35)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,5)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)-M(17)+M(18)+M(23)-M(24)+M(27)-M(28)-M(33)+M(34)+M(47)-M(48)+M(49) &
    -M(54)+M(59)+M(66)-M(71)-M(77)+M(78)-M(79)-M(91)+M(98)-M(101)+M(106)+M(107)+M(112)+M(113)-M(114)-M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(-M(174)+M(177)+M(182)-M(192))) * den(35)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,29)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(6)*(-M(147)-M(174)+M(177)+M(182) &
    +M(189)-M(190)-M(192)+M(207))) * den(35)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,53)
  Gcoeff = (c(5)*(M(49)-M(79)-M(91)+M(98)+M(106)+M(112)-M(114)-M(116)+M(117)+M(118)-M(120)-M(122))+c(6)*(M(147)+M(185)-M(187) &
    -M(189)+M(190)-M(207)-M(231)+M(245))) * den(35)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,6)
  Gcoeff = (c(5)*(M(49)-M(79)-M(91)+M(98)+M(106)+M(112)-M(114)-M(116)+M(117)+M(118)-M(120)-M(122))+c(6)*(M(158)-M(168)-M(174) &
    +M(177)+M(182)-M(192)-M(198)+M(201))) * den(35)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,30)
  Gcoeff = (c(6)*(-M(147)+M(158)-M(168)-M(174)+M(177)+M(182)-M(185)+M(187)+M(189)-M(190)-M(192)-M(198)+M(201)+M(207)+M(231) &
    -M(245))) * den(35)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,54)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)+M(31)-M(32)-M(35)+M(36)-M(42)+M(47)+M(59) &
    +M(60)-M(61)-M(69)+M(72)-M(80)-M(81)+M(82)+M(94)-M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(179)+M(181)+M(237)-M(247))) * den(14)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,7)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    -M(60)-M(61)+M(69)-M(72)-M(80)+M(81)+M(82)+M(94)-M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(134)+M(144)+M(200)-M(202))) * den(14)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,31)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(134)+M(144)+M(179)-M(181)+M(200) &
    -M(202)-M(237)+M(247))) * den(14)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,55)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)+M(31)-M(32)-M(35)+M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(171)-M(183)+M(184)-M(213))) * den(14)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,8)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    -M(60)+M(61)+M(69)-M(72)-M(80)+M(81)-M(82)-M(94)+M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(150)+M(153)+M(188)-M(194))) * den(14)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,32)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(150)+M(153)-M(171)+M(183)-M(184) &
    +M(188)-M(194)+M(213))) * den(14)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,56)
  Gcoeff = (c(5)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(171)+M(179)-M(181) &
    -M(183)+M(184)-M(213)-M(237)+M(247))) * den(14)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,9)
  Gcoeff = (c(5)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(134)-M(144)-M(150) &
    +M(153)+M(188)-M(194)-M(200)+M(202))) * den(14)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,33)
  Gcoeff = (c(6)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(14)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,57)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)+M(50)+M(51)-M(52) &
    -M(55)+M(62)-M(67)+M(74)+M(79)-M(86)-M(89)-M(90)+M(91)-M(102)-M(104)+M(108)-M(110)+M(114)+M(116)+M(119)+M(120)-M(121)+M(122) &
    -M(123)-M(124))+c(6)*(-M(186)+M(189)+M(207)-M(221))) * den(37)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,10)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(11)-M(12)+M(17)-M(18)+M(19)-M(20)-M(21)+M(22)-M(23)+M(24)+M(25)-M(26)-M(29)+M(30)+M(50)-M(51)-M(52) &
    -M(55)+M(62)+M(67)-M(74)+M(79)-M(86)-M(89)+M(90)+M(91)-M(102)-M(104)+M(108)-M(110)+M(114)+M(116)+M(119)+M(120)-M(121)+M(122) &
    -M(123)-M(124))+c(6)*(-M(160)+M(174)+M(192)-M(195))) * den(37)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,34)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(160)+M(174)+M(186)-M(189) &
    +M(192)-M(195)-M(207)+M(221))) * den(37)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,58)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)+M(50)+M(51)+M(52) &
    -M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91)-M(102)+M(104)+M(108)+M(110)-M(114)-M(116)+M(119)-M(120)-M(121)-M(122) &
    +M(123)+M(124))+c(6)*(M(153)-M(187)+M(188)-M(231))) * den(37)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,11)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(11)-M(12)+M(17)-M(18)+M(19)-M(20)-M(21)+M(22)-M(23)+M(24)+M(25)-M(26)-M(29)+M(30)+M(50)-M(51)+M(52) &
    -M(55)+M(62)+M(67)-M(74)-M(79)+M(86)-M(89)+M(90)-M(91)-M(102)+M(104)+M(108)+M(110)-M(114)-M(116)+M(119)-M(120)-M(121)-M(122) &
    +M(123)+M(124))+c(6)*(-M(168)+M(171)+M(184)-M(198))) * den(37)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,35)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(153)-M(168)+M(171)+M(184) &
    +M(187)-M(188)-M(198)+M(231))) * den(37)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,59)
  Gcoeff = (c(5)*(M(52)-M(79)+M(86)-M(91)+M(104)+M(110)-M(114)-M(116)-M(120)-M(122)+M(123)+M(124))+c(6)*(M(153)+M(186)-M(187) &
    +M(188)-M(189)-M(207)+M(221)-M(231))) * den(37)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,12)
  Gcoeff = (c(5)*(M(52)-M(79)+M(86)-M(91)+M(104)+M(110)-M(114)-M(116)-M(120)-M(122)+M(123)+M(124))+c(6)*(M(160)-M(168)+M(171) &
    -M(174)+M(184)-M(192)+M(195)-M(198))) * den(37)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,36)
  Gcoeff = (c(6)*(-M(153)+M(160)-M(168)+M(171)-M(174)+M(184)-M(186)+M(187)-M(188)+M(189)-M(192)+M(195)-M(198)+M(207)-M(221) &
    +M(231))) * den(37)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,60)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(27)-M(28)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(43)+M(50)+M(62) &
    +M(63)-M(64)-M(70)+M(75)+M(82)-M(87)-M(92)-M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(180)+M(183)+M(213)-M(223))) * den(16)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,13)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(27)-M(28)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(43)+M(50)+M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)-M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(136)+M(150)+M(194)-M(196))) * den(16)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,37)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(136)+M(150)+M(180)-M(183)+M(194) &
    -M(196)-M(213)+M(223))) * den(16)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,61)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(27)-M(28)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(43)+M(50)+M(62) &
    +M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94)-M(105)+M(106)+M(108)-M(111)-M(114)+M(117)+M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(177)-M(181)+M(182)-M(237))) * den(16)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,14)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(27)-M(28)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(43)+M(50)+M(62) &
    -M(63)+M(64)+M(70)-M(75)-M(82)+M(87)-M(92)+M(93)-M(94)-M(105)+M(106)+M(108)-M(111)-M(114)+M(117)+M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(144)+M(147)+M(190)-M(200))) * den(16)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,38)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(144)+M(147)-M(177)+M(181)-M(182) &
    +M(190)-M(200)+M(237))) * den(16)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,62)
  Gcoeff = (c(5)*(M(64)-M(82)+M(87)-M(94)+M(106)-M(114)+M(117)-M(120)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(177)+M(180)-M(181) &
    +M(182)-M(183)-M(213)+M(223)-M(237))) * den(16)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,15)
  Gcoeff = (c(5)*(M(64)-M(82)+M(87)-M(94)+M(106)-M(114)+M(117)-M(120)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(136)-M(144)+M(147) &
    -M(150)+M(190)-M(194)+M(196)-M(200))) * den(16)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,39)
  Gcoeff = (c(6)*(M(136)-M(144)+M(147)-M(150)-M(177)-M(180)+M(181)-M(182)+M(183)+M(190)-M(194)+M(196)-M(200)+M(213)-M(223) &
    +M(237))) * den(16)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,63)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(10)-M(12)-M(14)-M(15)-M(17)-M(20)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(38) &
    -M(39)+M(47)+M(49)+M(50)+M(53)+M(59)+M(60)+M(62)+M(63)+M(64)+M(65)+M(66)+M(67)+M(72)+M(73)+M(75)+M(78)+M(87)+M(90)+M(98) &
    +M(100)+M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(177)+M(182)))
  T4sum(1:15,48) = T4sum(1:15,48) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(47)+M(48)+M(49)+M(50)+M(51)+M(59)+M(62)+M(64)+M(68)+M(69)+M(70)+M(71)+M(74)+M(76)+M(81)+M(87)+M(88)+M(93)+M(98) &
    +M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(147)+M(190)))
  T4sum(1:15,48) = T4sum(1:15,48) + Gcoeff * G2tensor(:,47)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)+M(17)-M(18)-M(19)+M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)-M(63)-M(65)-M(66)-M(67)+M(68)+M(69)+M(70)+M(71)-M(72)-M(73)+M(74)-M(75)+M(76)-M(78)+M(81)+M(88)-M(90)+M(93) &
    -M(100))+c(6)*(M(147)-M(177)-M(182)+M(190)))
  T4sum(1:15,48) = T4sum(1:15,48) + Gcoeff * G2tensor(:,93)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(10)-M(12)-M(14)-M(15)-M(18)-M(20)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(47)+M(50)+M(52)+M(53)+M(59)+M(60)+M(61)+M(62)+M(63)+M(65)+M(66)+M(67)+M(72)+M(75)+M(76)+M(78)+M(86)+M(88)+M(90)+M(99) &
    +M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(171)+M(184)))
  T4sum(1:15,48) = T4sum(1:15,48) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(47)+M(48)+M(50)+M(51)+M(52)+M(59)+M(61)+M(62)+M(68)+M(69)+M(70)+M(71)+M(73)+M(74)+M(81)+M(86)+M(93)+M(99) &
    +M(100)+M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(153)+M(188)))
  T4sum(1:15,48) = T4sum(1:15,48) + Gcoeff * G2tensor(:,48)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)-M(63)-M(65)-M(66)-M(67)+M(68)+M(69)+M(70)+M(71)-M(72)+M(73)+M(74)-M(75)-M(76)-M(78)+M(81)-M(88)-M(90)+M(93) &
    +M(100))+c(6)*(M(153)-M(171)-M(184)+M(188)))
  T4sum(1:15,48) = T4sum(1:15,48) + Gcoeff * G2tensor(:,94)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)+M(61) &
    -M(64)-M(73)+M(76)+M(86)-M(87)+M(88)-M(98)+M(99)-M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(171)-M(177)-M(182)+M(184)))
  T4sum(1:15,48) = T4sum(1:15,48) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)+M(61) &
    -M(64)+M(73)-M(76)+M(86)-M(87)-M(88)-M(98)+M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(147)+M(153)+M(188)-M(190)))
  T4sum(1:15,48) = T4sum(1:15,48) + Gcoeff * G2tensor(:,49)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(147)+M(153)-M(171)+M(177) &
    +M(182)-M(184)+M(188)-M(190)))
  T4sum(1:15,48) = T4sum(1:15,48) + Gcoeff * G2tensor(:,95)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)+M(61) &
    -M(64)-M(73)+M(76)+M(86)-M(87)+M(88)-M(98)+M(99)-M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(171)-M(177)-M(182)+M(184))) * den(26)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,16)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)+M(61) &
    -M(64)+M(73)-M(76)+M(86)-M(87)-M(88)-M(98)+M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(147)+M(153)+M(188)-M(190))) * den(26)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,40)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(147)+M(153)-M(171)+M(177) &
    +M(182)-M(184)+M(188)-M(190))) * den(26)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,64)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)-M(61) &
    +M(64)-M(73)+M(76)+M(86)+M(87)+M(88)-M(98)-M(99)-M(100)-M(104)+M(106)+M(110)-M(112)+M(117)-M(118)-M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(179)+M(180)+M(223)-M(247))) * den(26)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,17)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)-M(61) &
    +M(64)+M(73)-M(76)+M(86)+M(87)-M(88)-M(98)-M(99)+M(100)-M(104)+M(106)+M(110)-M(112)+M(117)-M(118)-M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(134)+M(136)+M(196)-M(202))) * den(26)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,41)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(134)+M(136)+M(179)-M(180) &
    +M(196)-M(202)-M(223)+M(247))) * den(26)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,65)
  Gcoeff = (c(5)*(-M(61)+M(64)+M(87)-M(99)-M(104)+M(106)+M(117)-M(123)+M(125)-M(126)-M(128)+M(130))+c(6)*(-M(171)+M(177)-M(179) &
    +M(180)+M(182)-M(184)+M(223)-M(247))) * den(26)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,18)
  Gcoeff = (c(5)*(-M(61)+M(64)+M(87)-M(99)-M(104)+M(106)+M(117)-M(123)+M(125)-M(126)-M(128)+M(130))+c(6)*(-M(134)+M(136)+M(147) &
    -M(153)-M(188)+M(190)+M(196)-M(202))) * den(26)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,42)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(147)-M(153)+M(171)-M(177)+M(179)-M(180)-M(182)+M(184)-M(188)+M(190)+M(196)-M(202)-M(223) &
    +M(247))) * den(26)
  T3sum(1:5,72) = T3sum(1:5,72) + Gcoeff * G1tensor(:,66)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(6)-M(7)-M(9)-M(12)-M(14)-M(16)-M(17)-M(19)-M(21)-M(24)-M(26)-M(27)-M(29)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(43)+M(47)+M(49)+M(53)+M(59)+M(60)+M(65)+M(66)+M(67)+M(70)+M(72)+M(73)+M(78)+M(82)+M(90)+M(92)+M(93)+M(94)+M(98) &
    +M(100)+M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(181)+M(237)))
  T4sum(1:15,70) = T4sum(1:15,70) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(41)+M(43)+M(47)+M(48)+M(49)+M(51)+M(59)+M(63)+M(68)+M(69)+M(71)+M(74)+M(75)+M(76)+M(81)+M(82)+M(88)+M(92)+M(94)+M(98) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(144)+M(200)))
  T4sum(1:15,70) = T4sum(1:15,70) + Gcoeff * G2tensor(:,50)
  Gcoeff = (c(4)*(M(3)-M(4)-M(5)+M(6)-M(13)+M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)+M(63)-M(65)-M(66)-M(67)+M(68)+M(69)-M(70)+M(71)-M(72)-M(73)+M(74)+M(75)+M(76)-M(78)+M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(144)-M(181)+M(200)-M(237)))
  T4sum(1:15,70) = T4sum(1:15,70) + Gcoeff * G2tensor(:,96)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(10)-M(12)-M(14)-M(15)-M(17)-M(20)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(38) &
    -M(39)+M(47)+M(49)+M(50)+M(53)+M(59)+M(60)+M(62)+M(63)+M(64)+M(65)+M(66)+M(67)+M(72)+M(73)+M(75)+M(78)+M(87)+M(90)+M(98) &
    +M(100)+M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(177)+M(182)))
  T4sum(1:15,70) = T4sum(1:15,70) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(47)+M(48)+M(49)+M(50)+M(51)+M(59)+M(62)+M(64)+M(68)+M(69)+M(70)+M(71)+M(74)+M(76)+M(81)+M(87)+M(88)+M(93)+M(98) &
    +M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(147)+M(190)))
  T4sum(1:15,70) = T4sum(1:15,70) + Gcoeff * G2tensor(:,51)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)+M(17)-M(18)-M(19)+M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)-M(63)-M(65)-M(66)-M(67)+M(68)+M(69)+M(70)+M(71)-M(72)-M(73)+M(74)-M(75)+M(76)-M(78)+M(81)+M(88)-M(90)+M(93) &
    -M(100))+c(6)*(M(147)-M(177)-M(182)+M(190)))
  T4sum(1:15,70) = T4sum(1:15,70) + Gcoeff * G2tensor(:,97)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(27)-M(28)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(43)+M(50)+M(62) &
    +M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94)-M(105)+M(106)+M(108)-M(111)-M(114)+M(117)+M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(177)-M(181)+M(182)-M(237)))
  T4sum(1:15,70) = T4sum(1:15,70) + Gcoeff * G2tensor(:,6)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(27)-M(28)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(43)+M(50)+M(62) &
    -M(63)+M(64)+M(70)-M(75)-M(82)+M(87)-M(92)+M(93)-M(94)-M(105)+M(106)+M(108)-M(111)-M(114)+M(117)+M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(144)+M(147)+M(190)-M(200)))
  T4sum(1:15,70) = T4sum(1:15,70) + Gcoeff * G2tensor(:,52)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(144)+M(147)-M(177)+M(181)-M(182) &
    +M(190)-M(200)+M(237)))
  T4sum(1:15,70) = T4sum(1:15,70) + Gcoeff * G2tensor(:,98)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(9)-M(11)-M(13)-M(16)-M(18)-M(19)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(37) &
    -M(40)+M(42)+M(43)+M(52)+M(53)+M(64)+M(65)+M(66)+M(67)+M(69)+M(70)+M(76)+M(78)+M(80)+M(81)+M(86)+M(87)+M(88)+M(90)+M(92)+M(93) &
    +M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(180)+M(223)))
  T4sum(1:15,72) = T4sum(1:15,72) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(48)+M(51)+M(52)+M(60)+M(63)+M(64)+M(68)+M(71)+M(72)+M(73)+M(74)+M(75)+M(80)+M(86)+M(87)+M(92) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(136)+M(196)))
  T4sum(1:15,72) = T4sum(1:15,72) + Gcoeff * G2tensor(:,53)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)-M(17)+M(18)+M(19)-M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)+M(63)-M(65)-M(66)-M(67)+M(68)-M(69)-M(70)+M(71)+M(72)+M(73)+M(74)+M(75)-M(76)-M(78)-M(81)-M(88)-M(90)-M(93) &
    +M(100))+c(6)*(M(136)-M(180)+M(196)-M(223)))
  T4sum(1:15,72) = T4sum(1:15,72) + Gcoeff * G2tensor(:,99)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(5)-M(8)-M(10)-M(11)-M(13)-M(15)-M(18)-M(20)-M(22)-M(23)-M(25)-M(28)-M(30)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(42)+M(50)+M(52)+M(53)+M(62)+M(63)+M(65)+M(66)+M(67)+M(69)+M(75)+M(76)+M(78)+M(80)+M(81)+M(82)+M(86)+M(88)+M(90)+M(94) &
    +M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(183)+M(213)))
  T4sum(1:15,72) = T4sum(1:15,72) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(48)+M(50)+M(51)+M(52)+M(60)+M(62)+M(68)+M(70)+M(71)+M(72)+M(73)+M(74)+M(80)+M(82)+M(86)+M(93)+M(94) &
    +M(100)+M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(150)+M(194)))
  T4sum(1:15,72) = T4sum(1:15,72) + Gcoeff * G2tensor(:,54)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)-M(63)-M(65)-M(66)-M(67)+M(68)-M(69)+M(70)+M(71)+M(72)+M(73)+M(74)-M(75)-M(76)-M(78)-M(81)-M(88)-M(90)+M(93) &
    +M(100))+c(6)*(M(150)-M(183)+M(194)-M(213)))
  T4sum(1:15,72) = T4sum(1:15,72) + Gcoeff * G2tensor(:,100)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(27)-M(28)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(43)+M(50)+M(62) &
    +M(63)-M(64)-M(70)+M(75)+M(82)-M(87)-M(92)-M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(180)+M(183)+M(213)-M(223)))
  T4sum(1:15,72) = T4sum(1:15,72) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(27)-M(28)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(43)+M(50)+M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)-M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(136)+M(150)+M(194)-M(196)))
  T4sum(1:15,72) = T4sum(1:15,72) + Gcoeff * G2tensor(:,55)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(136)+M(150)+M(180)-M(183)+M(194) &
    -M(196)-M(213)+M(223)))
  T4sum(1:15,72) = T4sum(1:15,72) + Gcoeff * G2tensor(:,101)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(5)-M(8)-M(10)-M(11)-M(13)-M(15)-M(18)-M(20)-M(22)-M(23)-M(25)-M(28)-M(30)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(42)+M(50)+M(52)+M(53)+M(62)+M(63)+M(65)+M(66)+M(67)+M(69)+M(75)+M(76)+M(78)+M(80)+M(81)+M(82)+M(86)+M(88)+M(90)+M(94) &
    +M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(183)+M(213)))
  T4sum(1:15,73) = T4sum(1:15,73) + Gcoeff * G2tensor(:,16)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(48)+M(50)+M(51)+M(52)+M(60)+M(62)+M(68)+M(70)+M(71)+M(72)+M(73)+M(74)+M(80)+M(82)+M(86)+M(93)+M(94) &
    +M(100)+M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(150)+M(194)))
  T4sum(1:15,73) = T4sum(1:15,73) + Gcoeff * G2tensor(:,62)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)-M(63)-M(65)-M(66)-M(67)+M(68)-M(69)+M(70)+M(71)+M(72)+M(73)+M(74)-M(75)-M(76)-M(78)-M(81)-M(88)-M(90)+M(93) &
    +M(100))+c(6)*(M(150)-M(183)+M(194)-M(213)))
  T4sum(1:15,73) = T4sum(1:15,73) + Gcoeff * G2tensor(:,108)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(10)-M(12)-M(14)-M(15)-M(18)-M(20)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(47)+M(50)+M(52)+M(53)+M(59)+M(60)+M(61)+M(62)+M(63)+M(65)+M(66)+M(67)+M(72)+M(75)+M(76)+M(78)+M(86)+M(88)+M(90)+M(99) &
    +M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(171)+M(184)))
  T4sum(1:15,73) = T4sum(1:15,73) + Gcoeff * G2tensor(:,17)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(47)+M(48)+M(50)+M(51)+M(52)+M(59)+M(61)+M(62)+M(68)+M(69)+M(70)+M(71)+M(73)+M(74)+M(81)+M(86)+M(93)+M(99) &
    +M(100)+M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(153)+M(188)))
  T4sum(1:15,73) = T4sum(1:15,73) + Gcoeff * G2tensor(:,63)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)-M(63)-M(65)-M(66)-M(67)+M(68)+M(69)+M(70)+M(71)-M(72)+M(73)+M(74)-M(75)-M(76)-M(78)+M(81)-M(88)-M(90)+M(93) &
    +M(100))+c(6)*(M(153)-M(171)-M(184)+M(188)))
  T4sum(1:15,73) = T4sum(1:15,73) + Gcoeff * G2tensor(:,109)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)+M(31)-M(32)-M(35)+M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(171)-M(183)+M(184)-M(213)))
  T4sum(1:15,73) = T4sum(1:15,73) + Gcoeff * G2tensor(:,18)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    -M(60)+M(61)+M(69)-M(72)-M(80)+M(81)-M(82)-M(94)+M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(150)+M(153)+M(188)-M(194)))
  T4sum(1:15,73) = T4sum(1:15,73) + Gcoeff * G2tensor(:,64)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(150)+M(153)-M(171)+M(183)-M(184) &
    +M(188)-M(194)+M(213)))
  T4sum(1:15,73) = T4sum(1:15,73) + Gcoeff * G2tensor(:,110)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(9)-M(11)-M(13)-M(16)-M(17)-M(19)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(42)+M(43)+M(49)+M(53)+M(61)+M(65)+M(66)+M(67)+M(69)+M(70)+M(73)+M(78)+M(80)+M(81)+M(90)+M(92)+M(93)+M(98)+M(99) &
    +M(100)+M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(179)+M(247)))
  T4sum(1:15,75) = T4sum(1:15,75) + Gcoeff * G2tensor(:,19)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(48)+M(49)+M(51)+M(60)+M(61)+M(63)+M(68)+M(71)+M(72)+M(74)+M(75)+M(76)+M(80)+M(88)+M(92)+M(98)+M(99) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(134)+M(202)))
  T4sum(1:15,75) = T4sum(1:15,75) + Gcoeff * G2tensor(:,65)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)+M(63)-M(65)-M(66)-M(67)+M(68)-M(69)-M(70)+M(71)+M(72)-M(73)+M(74)+M(75)+M(76)-M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(134)-M(179)+M(202)-M(247)))
  T4sum(1:15,75) = T4sum(1:15,75) + Gcoeff * G2tensor(:,111)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(6)-M(7)-M(9)-M(12)-M(14)-M(16)-M(17)-M(19)-M(21)-M(24)-M(26)-M(27)-M(29)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(43)+M(47)+M(49)+M(53)+M(59)+M(60)+M(65)+M(66)+M(67)+M(70)+M(72)+M(73)+M(78)+M(82)+M(90)+M(92)+M(93)+M(94)+M(98) &
    +M(100)+M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(181)+M(237)))
  T4sum(1:15,75) = T4sum(1:15,75) + Gcoeff * G2tensor(:,20)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(41)+M(43)+M(47)+M(48)+M(49)+M(51)+M(59)+M(63)+M(68)+M(69)+M(71)+M(74)+M(75)+M(76)+M(81)+M(82)+M(88)+M(92)+M(94)+M(98) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(144)+M(200)))
  T4sum(1:15,75) = T4sum(1:15,75) + Gcoeff * G2tensor(:,66)
  Gcoeff = (c(4)*(M(3)-M(4)-M(5)+M(6)-M(13)+M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)+M(63)-M(65)-M(66)-M(67)+M(68)+M(69)-M(70)+M(71)-M(72)-M(73)+M(74)+M(75)+M(76)-M(78)+M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(144)-M(181)+M(200)-M(237)))
  T4sum(1:15,75) = T4sum(1:15,75) + Gcoeff * G2tensor(:,112)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)+M(31)-M(32)-M(35)+M(36)-M(42)+M(47)+M(59) &
    +M(60)-M(61)-M(69)+M(72)-M(80)-M(81)+M(82)+M(94)-M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(179)+M(181)+M(237)-M(247)))
  T4sum(1:15,75) = T4sum(1:15,75) + Gcoeff * G2tensor(:,21)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    -M(60)-M(61)+M(69)-M(72)-M(80)+M(81)+M(82)+M(94)-M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(134)+M(144)+M(200)-M(202)))
  T4sum(1:15,75) = T4sum(1:15,75) + Gcoeff * G2tensor(:,67)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(134)+M(144)+M(179)-M(181)+M(200) &
    -M(202)-M(237)+M(247)))
  T4sum(1:15,75) = T4sum(1:15,75) + Gcoeff * G2tensor(:,113)
  Gcoeff = (c(5)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(171)+M(179)-M(181) &
    -M(183)+M(184)-M(213)-M(237)+M(247))) * den(24)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,28)
  Gcoeff = (c(5)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(134)-M(144)-M(150) &
    +M(153)+M(188)-M(194)-M(200)+M(202))) * den(24)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,74)
  Gcoeff = (c(6)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(24)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,120)
  Gcoeff = (c(5)*(-M(61)+M(64)+M(87)-M(99)-M(104)+M(106)+M(117)-M(123)+M(125)-M(126)-M(128)+M(130))+c(6)*(-M(171)+M(177)-M(179) &
    +M(180)+M(182)-M(184)+M(223)-M(247))) * den(24)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,29)
  Gcoeff = (c(5)*(-M(61)+M(64)+M(87)-M(99)-M(104)+M(106)+M(117)-M(123)+M(125)-M(126)-M(128)+M(130))+c(6)*(-M(134)+M(136)+M(147) &
    -M(153)-M(188)+M(190)+M(196)-M(202))) * den(24)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,75)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(147)-M(153)+M(171)-M(177)+M(179)-M(180)-M(182)+M(184)-M(188)+M(190)+M(196)-M(202)-M(223) &
    +M(247))) * den(24)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,121)
  Gcoeff = (c(5)*(-M(64)+M(82)-M(87)+M(94)-M(106)+M(114)-M(117)+M(120)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(177)-M(180)+M(181) &
    -M(182)+M(183)+M(213)-M(223)+M(237))) * den(24)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,30)
  Gcoeff = (c(5)*(-M(64)+M(82)-M(87)+M(94)-M(106)+M(114)-M(117)+M(120)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(136)+M(144)-M(147) &
    +M(150)-M(190)+M(194)-M(196)+M(200))) * den(24)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,76)
  Gcoeff = (c(6)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(24)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,122)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(61) &
    +M(64)-M(73)+M(76)-M(86)+M(87)+M(88)+M(98)-M(99)-M(100)-M(104)+M(106)-M(110)+M(112)+M(117)+M(118)-M(123)-M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(147)-M(153)-M(188)+M(190))) * den(26)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,19)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(61) &
    +M(64)+M(73)-M(76)-M(86)+M(87)-M(88)+M(98)-M(99)+M(100)-M(104)+M(106)-M(110)+M(112)+M(117)+M(118)-M(123)-M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(171)+M(177)+M(182)-M(184))) * den(26)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,43)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(147)+M(153)-M(171)+M(177) &
    +M(182)-M(184)+M(188)-M(190))) * den(26)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,67)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)-M(49)+M(52)-M(61) &
    +M(64)-M(73)+M(76)+M(86)+M(87)+M(88)-M(98)-M(99)-M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(185)+M(186)+M(221)-M(245))) * den(26)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,20)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)-M(49)+M(52)-M(61) &
    +M(64)+M(73)-M(76)+M(86)+M(87)-M(88)-M(98)-M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(158)+M(160)+M(195)-M(201))) * den(26)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,44)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(158)+M(160)+M(185)-M(186) &
    +M(195)-M(201)-M(221)+M(245))) * den(26)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,68)
  Gcoeff = (c(5)*(-M(49)+M(52)+M(86)-M(98)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124))+c(6)*(-M(147)+M(153)-M(185) &
    +M(186)+M(188)-M(190)+M(221)-M(245))) * den(26)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,21)
  Gcoeff = (c(5)*(-M(49)+M(52)+M(86)-M(98)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124))+c(6)*(-M(158)+M(160)+M(171) &
    -M(177)-M(182)+M(184)+M(195)-M(201))) * den(26)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,45)
  Gcoeff = (c(6)*(M(147)-M(153)-M(158)+M(160)+M(171)-M(177)-M(182)+M(184)+M(185)-M(186)-M(188)+M(190)+M(195)-M(201)-M(221) &
    +M(245))) * den(26)
  T3sum(1:5,71) = T3sum(1:5,71) + Gcoeff * G1tensor(:,69)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(5)-M(8)-M(10)-M(11)-M(13)-M(15)-M(18)-M(20)-M(22)-M(23)-M(25)-M(28)-M(30)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(47)+M(48)+M(55)+M(59)+M(61)+M(67)+M(68)+M(69)+M(70)+M(71)+M(73)+M(79)+M(81)+M(89)+M(90)+M(91)+M(93)+M(99) &
    +M(100)+M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(187)+M(231)))
  T4sum(1:15,106) = T4sum(1:15,106) + Gcoeff * G2tensor(:,31)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(47)+M(51)+M(53)+M(55)+M(59)+M(60)+M(61)+M(63)+M(65)+M(66)+M(72)+M(74)+M(75)+M(76)+M(78)+M(79)+M(88)+M(89)+M(91)+M(99) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(168)+M(198)))
  T4sum(1:15,106) = T4sum(1:15,106) + Gcoeff * G2tensor(:,77)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)-M(41)-M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)+M(66)-M(67)-M(68)-M(69)-M(70)-M(71)+M(72)-M(73)+M(74)+M(75)+M(76)+M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(168)-M(187)+M(198)-M(231)))
  T4sum(1:15,106) = T4sum(1:15,106) + Gcoeff * G2tensor(:,123)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(47)+M(48)+M(50)+M(51)+M(52)+M(59)+M(61)+M(62)+M(68)+M(69)+M(70)+M(71)+M(73)+M(74)+M(81)+M(86)+M(93)+M(99) &
    +M(100)+M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(153)+M(188)))
  T4sum(1:15,106) = T4sum(1:15,106) + Gcoeff * G2tensor(:,32)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(10)-M(12)-M(14)-M(15)-M(18)-M(20)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(47)+M(50)+M(52)+M(53)+M(59)+M(60)+M(61)+M(62)+M(63)+M(65)+M(66)+M(67)+M(72)+M(75)+M(76)+M(78)+M(86)+M(88)+M(90)+M(99) &
    +M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(171)+M(184)))
  T4sum(1:15,106) = T4sum(1:15,106) + Gcoeff * G2tensor(:,78)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(41)-M(48)-M(51) &
    +M(53)+M(60)+M(63)+M(65)+M(66)+M(67)-M(68)-M(69)-M(70)-M(71)+M(72)-M(73)-M(74)+M(75)+M(76)+M(78)-M(81)+M(88)+M(90)-M(93) &
    -M(100))+c(6)*(-M(153)+M(171)+M(184)-M(188)))
  T4sum(1:15,106) = T4sum(1:15,106) + Gcoeff * G2tensor(:,124)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)+M(50)+M(51)+M(52) &
    -M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91)-M(102)+M(104)+M(108)+M(110)-M(114)-M(116)+M(119)-M(120)-M(121)-M(122) &
    +M(123)+M(124))+c(6)*(M(153)-M(187)+M(188)-M(231)))
  T4sum(1:15,106) = T4sum(1:15,106) + Gcoeff * G2tensor(:,33)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(11)-M(12)+M(17)-M(18)+M(19)-M(20)-M(21)+M(22)-M(23)+M(24)+M(25)-M(26)-M(29)+M(30)+M(50)-M(51)+M(52) &
    -M(55)+M(62)+M(67)-M(74)-M(79)+M(86)-M(89)+M(90)-M(91)-M(102)+M(104)+M(108)+M(110)-M(114)-M(116)+M(119)-M(120)-M(121)-M(122) &
    +M(123)+M(124))+c(6)*(-M(168)+M(171)+M(184)-M(198)))
  T4sum(1:15,106) = T4sum(1:15,106) + Gcoeff * G2tensor(:,79)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(153)-M(168)+M(171)+M(184) &
    +M(187)-M(188)-M(198)+M(231)))
  T4sum(1:15,106) = T4sum(1:15,106) + Gcoeff * G2tensor(:,125)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(41)+M(52)+M(54)+M(55)+M(64)+M(66)+M(67)+M(68)+M(69)+M(70)+M(76)+M(77)+M(78)+M(81)+M(86)+M(87)+M(88)+M(89)+M(90)+M(93) &
    +M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(186)+M(221)))
  T4sum(1:15,108) = T4sum(1:15,108) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(9)-M(11)-M(13)-M(16)-M(17)-M(19)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(48)+M(51)+M(52)+M(53)+M(54)+M(55)+M(60)+M(63)+M(64)+M(65)+M(71)+M(72)+M(73)+M(74)+M(75)+M(77)+M(86)+M(87)+M(89) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(160)+M(195)))
  T4sum(1:15,108) = T4sum(1:15,108) + Gcoeff * G2tensor(:,56)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)+M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)-M(66)-M(67)-M(68)-M(69)-M(70)+M(71)+M(72)+M(73)+M(74)+M(75)-M(76)-M(78)-M(81)-M(88)-M(90)-M(93) &
    +M(100))+c(6)*(M(160)-M(186)+M(195)-M(221)))
  T4sum(1:15,108) = T4sum(1:15,108) + Gcoeff * G2tensor(:,102)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(6)-M(7)-M(9)-M(12)-M(14)-M(16)-M(17)-M(19)-M(21)-M(24)-M(26)-M(27)-M(29)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(41)+M(50)+M(51)+M(54)+M(62)+M(64)+M(66)+M(68)+M(69)+M(70)+M(74)+M(76)+M(77)+M(78)+M(79)+M(81)+M(87)+M(88)+M(91)+M(93) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(189)+M(207)))
  T4sum(1:15,108) = T4sum(1:15,108) + Gcoeff * G2tensor(:,11)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(48)+M(50)+M(53)+M(54)+M(60)+M(62)+M(63)+M(64)+M(65)+M(67)+M(71)+M(72)+M(73)+M(75)+M(77)+M(79)+M(87)+M(90)+M(91) &
    +M(100)+M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(174)+M(192)))
  T4sum(1:15,108) = T4sum(1:15,108) + Gcoeff * G2tensor(:,57)
  Gcoeff = (c(4)*(M(3)-M(4)-M(5)+M(6)-M(13)+M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)-M(41)+M(48)-M(51) &
    +M(53)+M(60)+M(63)+M(65)-M(66)+M(67)-M(68)-M(69)-M(70)+M(71)+M(72)+M(73)-M(74)+M(75)-M(76)-M(78)-M(81)-M(88)+M(90)-M(93) &
    +M(100))+c(6)*(M(174)-M(189)+M(192)-M(207)))
  T4sum(1:15,108) = T4sum(1:15,108) + Gcoeff * G2tensor(:,103)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)+M(50)+M(51)-M(52) &
    -M(55)+M(62)-M(67)+M(74)+M(79)-M(86)-M(89)-M(90)+M(91)-M(102)-M(104)+M(108)-M(110)+M(114)+M(116)+M(119)+M(120)-M(121)+M(122) &
    -M(123)-M(124))+c(6)*(-M(186)+M(189)+M(207)-M(221)))
  T4sum(1:15,108) = T4sum(1:15,108) + Gcoeff * G2tensor(:,12)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(11)-M(12)+M(17)-M(18)+M(19)-M(20)-M(21)+M(22)-M(23)+M(24)+M(25)-M(26)-M(29)+M(30)+M(50)-M(51)-M(52) &
    -M(55)+M(62)+M(67)-M(74)+M(79)-M(86)-M(89)+M(90)+M(91)-M(102)-M(104)+M(108)-M(110)+M(114)+M(116)+M(119)+M(120)-M(121)+M(122) &
    -M(123)-M(124))+c(6)*(-M(160)+M(174)+M(192)-M(195)))
  T4sum(1:15,108) = T4sum(1:15,108) + Gcoeff * G2tensor(:,58)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(160)+M(174)+M(186)-M(189) &
    +M(192)-M(195)-M(207)+M(221)))
  T4sum(1:15,108) = T4sum(1:15,108) + Gcoeff * G2tensor(:,104)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(6)-M(7)-M(9)-M(12)-M(14)-M(16)-M(17)-M(19)-M(21)-M(24)-M(26)-M(27)-M(29)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(41)+M(50)+M(51)+M(54)+M(62)+M(64)+M(66)+M(68)+M(69)+M(70)+M(74)+M(76)+M(77)+M(78)+M(79)+M(81)+M(87)+M(88)+M(91)+M(93) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(189)+M(207)))
  T4sum(1:15,109) = T4sum(1:15,109) + Gcoeff * G2tensor(:,34)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(48)+M(50)+M(53)+M(54)+M(60)+M(62)+M(63)+M(64)+M(65)+M(67)+M(71)+M(72)+M(73)+M(75)+M(77)+M(79)+M(87)+M(90)+M(91) &
    +M(100)+M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(174)+M(192)))
  T4sum(1:15,109) = T4sum(1:15,109) + Gcoeff * G2tensor(:,80)
  Gcoeff = (c(4)*(M(3)-M(4)-M(5)+M(6)-M(13)+M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)-M(41)+M(48)-M(51) &
    +M(53)+M(60)+M(63)+M(65)-M(66)+M(67)-M(68)-M(69)-M(70)+M(71)+M(72)+M(73)-M(74)+M(75)-M(76)-M(78)-M(81)-M(88)+M(90)-M(93) &
    +M(100))+c(6)*(M(174)-M(189)+M(192)-M(207)))
  T4sum(1:15,109) = T4sum(1:15,109) + Gcoeff * G2tensor(:,126)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(47)+M(48)+M(49)+M(50)+M(51)+M(59)+M(62)+M(64)+M(68)+M(69)+M(70)+M(71)+M(74)+M(76)+M(81)+M(87)+M(88)+M(93)+M(98) &
    +M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(147)+M(190)))
  T4sum(1:15,109) = T4sum(1:15,109) + Gcoeff * G2tensor(:,35)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(10)-M(12)-M(14)-M(15)-M(17)-M(20)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(38) &
    -M(39)+M(47)+M(49)+M(50)+M(53)+M(59)+M(60)+M(62)+M(63)+M(64)+M(65)+M(66)+M(67)+M(72)+M(73)+M(75)+M(78)+M(87)+M(90)+M(98) &
    +M(100)+M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(177)+M(182)))
  T4sum(1:15,109) = T4sum(1:15,109) + Gcoeff * G2tensor(:,81)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)-M(17)+M(18)+M(19)-M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)-M(48)-M(51) &
    +M(53)+M(60)+M(63)+M(65)+M(66)+M(67)-M(68)-M(69)-M(70)-M(71)+M(72)+M(73)-M(74)+M(75)-M(76)+M(78)-M(81)-M(88)+M(90)-M(93) &
    +M(100))+c(6)*(-M(147)+M(177)+M(182)-M(190)))
  T4sum(1:15,109) = T4sum(1:15,109) + Gcoeff * G2tensor(:,127)
  Gcoeff = (c(4)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)+M(47)+M(48)+M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98)-M(101)+M(106)+M(107)+M(112)+M(113)-M(114)-M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(M(147)-M(189)+M(190)-M(207)))
  T4sum(1:15,109) = T4sum(1:15,109) + Gcoeff * G2tensor(:,36)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)-M(17)+M(18)+M(23)-M(24)+M(27)-M(28)-M(33)+M(34)+M(47)-M(48)+M(49) &
    -M(54)+M(59)+M(66)-M(71)-M(77)+M(78)-M(79)-M(91)+M(98)-M(101)+M(106)+M(107)+M(112)+M(113)-M(114)-M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(-M(174)+M(177)+M(182)-M(192)))
  T4sum(1:15,109) = T4sum(1:15,109) + Gcoeff * G2tensor(:,82)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(6)*(-M(147)-M(174)+M(177)+M(182) &
    +M(189)-M(190)-M(192)+M(207)))
  T4sum(1:15,109) = T4sum(1:15,109) + Gcoeff * G2tensor(:,128)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(41)+M(49)+M(54)+M(55)+M(61)+M(66)+M(67)+M(68)+M(69)+M(70)+M(73)+M(77)+M(78)+M(81)+M(89)+M(90)+M(93)+M(98)+M(99) &
    +M(100)+M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(185)+M(245)))
  T4sum(1:15,111) = T4sum(1:15,111) + Gcoeff * G2tensor(:,22)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(9)-M(11)-M(13)-M(16)-M(18)-M(19)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(37) &
    -M(40)+M(48)+M(49)+M(51)+M(53)+M(54)+M(55)+M(60)+M(61)+M(63)+M(65)+M(71)+M(72)+M(74)+M(75)+M(76)+M(77)+M(88)+M(89)+M(98)+M(99) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(158)+M(201)))
  T4sum(1:15,111) = T4sum(1:15,111) + Gcoeff * G2tensor(:,68)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)+M(17)-M(18)-M(19)+M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(41)+M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)-M(66)-M(67)-M(68)-M(69)-M(70)+M(71)+M(72)-M(73)+M(74)+M(75)+M(76)-M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(158)-M(185)+M(201)-M(245)))
  T4sum(1:15,111) = T4sum(1:15,111) + Gcoeff * G2tensor(:,114)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(5)-M(8)-M(10)-M(11)-M(13)-M(15)-M(18)-M(20)-M(22)-M(23)-M(25)-M(28)-M(30)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(47)+M(48)+M(55)+M(59)+M(61)+M(67)+M(68)+M(69)+M(70)+M(71)+M(73)+M(79)+M(81)+M(89)+M(90)+M(91)+M(93)+M(99) &
    +M(100)+M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(187)+M(231)))
  T4sum(1:15,111) = T4sum(1:15,111) + Gcoeff * G2tensor(:,23)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(47)+M(51)+M(53)+M(55)+M(59)+M(60)+M(61)+M(63)+M(65)+M(66)+M(72)+M(74)+M(75)+M(76)+M(78)+M(79)+M(88)+M(89)+M(91)+M(99) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(168)+M(198)))
  T4sum(1:15,111) = T4sum(1:15,111) + Gcoeff * G2tensor(:,69)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)-M(41)-M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)+M(66)-M(67)-M(68)-M(69)-M(70)-M(71)+M(72)-M(73)+M(74)+M(75)+M(76)+M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(168)-M(187)+M(198)-M(231)))
  T4sum(1:15,111) = T4sum(1:15,111) + Gcoeff * G2tensor(:,115)
  Gcoeff = (c(4)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)+M(47)+M(48)-M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)+M(79)+M(91)-M(98)-M(101)-M(106)+M(107)-M(112)+M(113)+M(114)-M(115)+M(116)-M(117)-M(118) &
    +M(120)+M(122))+c(6)*(-M(185)+M(187)+M(231)-M(245)))
  T4sum(1:15,111) = T4sum(1:15,111) + Gcoeff * G2tensor(:,24)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)-M(17)+M(18)+M(23)-M(24)+M(27)-M(28)-M(33)+M(34)+M(47)-M(48)-M(49) &
    -M(54)+M(59)+M(66)-M(71)-M(77)+M(78)+M(79)+M(91)-M(98)-M(101)-M(106)+M(107)-M(112)+M(113)+M(114)-M(115)+M(116)-M(117)-M(118) &
    +M(120)+M(122))+c(6)*(-M(158)+M(168)+M(198)-M(201)))
  T4sum(1:15,111) = T4sum(1:15,111) + Gcoeff * G2tensor(:,70)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(6)*(-M(158)+M(168)+M(185)-M(187) &
    +M(198)-M(201)-M(231)+M(245)))
  T4sum(1:15,111) = T4sum(1:15,111) + Gcoeff * G2tensor(:,116)
  Gcoeff = (c(5)*(M(49)-M(79)-M(91)+M(98)+M(106)+M(112)-M(114)-M(116)+M(117)+M(118)-M(120)-M(122))+c(6)*(M(147)+M(185)-M(187) &
    -M(189)+M(190)-M(207)-M(231)+M(245))) * den(62)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,37)
  Gcoeff = (c(5)*(M(49)-M(79)-M(91)+M(98)+M(106)+M(112)-M(114)-M(116)+M(117)+M(118)-M(120)-M(122))+c(6)*(M(158)-M(168)-M(174) &
    +M(177)+M(182)-M(192)-M(198)+M(201))) * den(62)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,83)
  Gcoeff = (c(6)*(-M(147)+M(158)-M(168)-M(174)+M(177)+M(182)-M(185)+M(187)+M(189)-M(190)-M(192)-M(198)+M(201)+M(207)+M(231) &
    -M(245))) * den(62)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,129)
  Gcoeff = (c(5)*(-M(49)+M(52)+M(86)-M(98)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124))+c(6)*(-M(147)+M(153)-M(185) &
    +M(186)+M(188)-M(190)+M(221)-M(245))) * den(62)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,38)
  Gcoeff = (c(5)*(-M(49)+M(52)+M(86)-M(98)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124))+c(6)*(-M(158)+M(160)+M(171) &
    -M(177)-M(182)+M(184)+M(195)-M(201))) * den(62)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,84)
  Gcoeff = (c(6)*(M(147)-M(153)-M(158)+M(160)+M(171)-M(177)-M(182)+M(184)+M(185)-M(186)-M(188)+M(190)+M(195)-M(201)-M(221) &
    +M(245))) * den(62)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,130)
  Gcoeff = (c(5)*(-M(52)+M(79)-M(86)+M(91)-M(104)-M(110)+M(114)+M(116)+M(120)+M(122)-M(123)-M(124))+c(6)*(-M(153)-M(186)+M(187) &
    -M(188)+M(189)+M(207)-M(221)+M(231))) * den(62)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,39)
  Gcoeff = (c(5)*(-M(52)+M(79)-M(86)+M(91)-M(104)-M(110)+M(114)+M(116)+M(120)+M(122)-M(123)-M(124))+c(6)*(-M(160)+M(168)-M(171) &
    +M(174)-M(184)+M(192)-M(195)+M(198))) * den(62)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,85)
  Gcoeff = (c(6)*(M(153)-M(160)+M(168)-M(171)+M(174)-M(184)+M(186)-M(187)+M(188)-M(189)+M(192)-M(195)+M(198)-M(207)+M(221) &
    -M(231))) * den(62)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,131)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)-M(61) &
    +M(64)-M(73)+M(76)+M(86)+M(87)+M(88)-M(98)-M(99)-M(100)-M(104)+M(106)+M(110)-M(112)+M(117)-M(118)-M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(179)+M(180)+M(223)-M(247))) * den(26)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,22)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)-M(61) &
    +M(64)+M(73)-M(76)+M(86)+M(87)-M(88)-M(98)-M(99)+M(100)-M(104)+M(106)+M(110)-M(112)+M(117)-M(118)-M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(134)+M(136)+M(196)-M(202))) * den(26)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,46)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(134)+M(136)+M(179)-M(180) &
    +M(196)-M(202)-M(223)+M(247))) * den(26)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,70)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)-M(49)+M(52)-M(61) &
    +M(64)-M(73)+M(76)+M(86)+M(87)+M(88)-M(98)-M(99)-M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(185)+M(186)+M(221)-M(245))) * den(26)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,23)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)-M(49)+M(52)-M(61) &
    +M(64)+M(73)-M(76)+M(86)+M(87)-M(88)-M(98)-M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(158)+M(160)+M(195)-M(201))) * den(26)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,47)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(158)+M(160)+M(185)-M(186) &
    +M(195)-M(201)-M(221)+M(245))) * den(26)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,71)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(37)-M(38)-M(39)+M(40)+M(104)-M(106)-M(117)+M(123))+c(6)*(M(179)-M(180)-M(185)+M(186) &
    +M(221)-M(223)-M(245)+M(247))) * den(26)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,24)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(37)-M(38)-M(39)+M(40)+M(104)-M(106)-M(117)+M(123))+c(6)*(M(134)-M(136)-M(158)+M(160) &
    +M(195)-M(196)-M(201)+M(202))) * den(26)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,48)
  Gcoeff = (c(6)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(26)
  T3sum(1:5,7) = T3sum(1:5,7) + Gcoeff * G1tensor(:,72)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(9)-M(11)-M(13)-M(16)-M(18)-M(19)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(37) &
    -M(40)+M(42)+M(43)+M(52)+M(53)+M(64)+M(65)+M(66)+M(67)+M(69)+M(70)+M(76)+M(78)+M(80)+M(81)+M(86)+M(87)+M(88)+M(90)+M(92)+M(93) &
    +M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(180)+M(223)))
  T4sum(1:15,133) = T4sum(1:15,133) + Gcoeff * G2tensor(:,13)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(48)+M(51)+M(52)+M(60)+M(63)+M(64)+M(68)+M(71)+M(72)+M(73)+M(74)+M(75)+M(80)+M(86)+M(87)+M(92) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(136)+M(196)))
  T4sum(1:15,133) = T4sum(1:15,133) + Gcoeff * G2tensor(:,59)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)-M(17)+M(18)+M(19)-M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)+M(63)-M(65)-M(66)-M(67)+M(68)-M(69)-M(70)+M(71)+M(72)+M(73)+M(74)+M(75)-M(76)-M(78)-M(81)-M(88)-M(90)-M(93) &
    +M(100))+c(6)*(M(136)-M(180)+M(196)-M(223)))
  T4sum(1:15,133) = T4sum(1:15,133) + Gcoeff * G2tensor(:,105)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(41)+M(52)+M(54)+M(55)+M(64)+M(66)+M(67)+M(68)+M(69)+M(70)+M(76)+M(77)+M(78)+M(81)+M(86)+M(87)+M(88)+M(89)+M(90)+M(93) &
    +M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(186)+M(221)))
  T4sum(1:15,133) = T4sum(1:15,133) + Gcoeff * G2tensor(:,14)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(9)-M(11)-M(13)-M(16)-M(17)-M(19)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(48)+M(51)+M(52)+M(53)+M(54)+M(55)+M(60)+M(63)+M(64)+M(65)+M(71)+M(72)+M(73)+M(74)+M(75)+M(77)+M(86)+M(87)+M(89) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(160)+M(195)))
  T4sum(1:15,133) = T4sum(1:15,133) + Gcoeff * G2tensor(:,60)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)+M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)-M(66)-M(67)-M(68)-M(69)-M(70)+M(71)+M(72)+M(73)+M(74)+M(75)-M(76)-M(78)-M(81)-M(88)-M(90)-M(93) &
    +M(100))+c(6)*(M(160)-M(186)+M(195)-M(221)))
  T4sum(1:15,133) = T4sum(1:15,133) + Gcoeff * G2tensor(:,106)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(5)+M(6)-M(7)+M(8)+M(13)-M(14)+M(19)-M(20)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42)-M(43)-M(53) &
    +M(54)+M(55)-M(65)+M(68)+M(77)-M(80)+M(89)-M(92)+M(101)+M(102)-M(103)+M(104)-M(105)-M(106)-M(109)-M(111)+M(115)-M(117)+M(121) &
    +M(123))+c(6)*(-M(180)+M(186)+M(221)-M(223)))
  T4sum(1:15,133) = T4sum(1:15,133) + Gcoeff * G2tensor(:,15)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)+M(5)-M(6)-M(7)+M(8)-M(13)+M(14)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)-M(41)-M(42)-M(43)+M(53) &
    +M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92)+M(101)+M(102)-M(103)+M(104)-M(105)-M(106)-M(109)-M(111)+M(115)-M(117)+M(121) &
    +M(123))+c(6)*(-M(136)+M(160)+M(195)-M(196)))
  T4sum(1:15,133) = T4sum(1:15,133) + Gcoeff * G2tensor(:,61)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(-M(136)+M(160)+M(180)-M(186)+M(195) &
    -M(196)-M(221)+M(223)))
  T4sum(1:15,133) = T4sum(1:15,133) + Gcoeff * G2tensor(:,107)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(9)-M(11)-M(13)-M(16)-M(17)-M(19)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(42)+M(43)+M(49)+M(53)+M(61)+M(65)+M(66)+M(67)+M(69)+M(70)+M(73)+M(78)+M(80)+M(81)+M(90)+M(92)+M(93)+M(98)+M(99) &
    +M(100)+M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(179)+M(247)))
  T4sum(1:15,134) = T4sum(1:15,134) + Gcoeff * G2tensor(:,25)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(48)+M(49)+M(51)+M(60)+M(61)+M(63)+M(68)+M(71)+M(72)+M(74)+M(75)+M(76)+M(80)+M(88)+M(92)+M(98)+M(99) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(134)+M(202)))
  T4sum(1:15,134) = T4sum(1:15,134) + Gcoeff * G2tensor(:,71)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)+M(63)-M(65)-M(66)-M(67)+M(68)-M(69)-M(70)+M(71)+M(72)-M(73)+M(74)+M(75)+M(76)-M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(134)-M(179)+M(202)-M(247)))
  T4sum(1:15,134) = T4sum(1:15,134) + Gcoeff * G2tensor(:,117)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(41)+M(49)+M(54)+M(55)+M(61)+M(66)+M(67)+M(68)+M(69)+M(70)+M(73)+M(77)+M(78)+M(81)+M(89)+M(90)+M(93)+M(98)+M(99) &
    +M(100)+M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(185)+M(245)))
  T4sum(1:15,134) = T4sum(1:15,134) + Gcoeff * G2tensor(:,26)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(9)-M(11)-M(13)-M(16)-M(18)-M(19)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(37) &
    -M(40)+M(48)+M(49)+M(51)+M(53)+M(54)+M(55)+M(60)+M(61)+M(63)+M(65)+M(71)+M(72)+M(74)+M(75)+M(76)+M(77)+M(88)+M(89)+M(98)+M(99) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(158)+M(201)))
  T4sum(1:15,134) = T4sum(1:15,134) + Gcoeff * G2tensor(:,72)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)+M(17)-M(18)-M(19)+M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(41)+M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)-M(66)-M(67)-M(68)-M(69)-M(70)+M(71)+M(72)-M(73)+M(74)+M(75)+M(76)-M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(158)-M(185)+M(201)-M(245)))
  T4sum(1:15,134) = T4sum(1:15,134) + Gcoeff * G2tensor(:,118)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)-M(5)+M(6)+M(7)-M(8)+M(13)-M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42)-M(43)-M(53) &
    +M(54)+M(55)-M(65)+M(68)+M(77)-M(80)+M(89)-M(92)+M(101)+M(102)-M(103)-M(104)-M(105)+M(106)-M(109)-M(111)+M(115)+M(117)+M(121) &
    -M(123))+c(6)*(-M(179)+M(185)+M(245)-M(247)))
  T4sum(1:15,134) = T4sum(1:15,134) + Gcoeff * G2tensor(:,27)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(5)-M(6)+M(7)-M(8)-M(13)+M(14)-M(19)+M(20)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42)-M(43)+M(53) &
    +M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92)+M(101)+M(102)-M(103)-M(104)-M(105)+M(106)-M(109)-M(111)+M(115)+M(117)+M(121) &
    -M(123))+c(6)*(-M(134)+M(158)+M(201)-M(202)))
  T4sum(1:15,134) = T4sum(1:15,134) + Gcoeff * G2tensor(:,73)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(-M(134)+M(158)+M(179)-M(185)+M(201) &
    -M(202)-M(245)+M(247)))
  T4sum(1:15,134) = T4sum(1:15,134) + Gcoeff * G2tensor(:,119)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(37)-M(38)-M(39)+M(40)+M(104)-M(106)-M(117)+M(123))+c(6)*(M(179)-M(180)-M(185)+M(186) &
    +M(221)-M(223)-M(245)+M(247))) * den(99)
  T3sum(1:15,7) = T3sum(1:15,7) + Gcoeff * G2tensor(:,40)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(37)-M(38)-M(39)+M(40)+M(104)-M(106)-M(117)+M(123))+c(6)*(M(134)-M(136)-M(158)+M(160) &
    +M(195)-M(196)-M(201)+M(202))) * den(99)
  T3sum(1:15,7) = T3sum(1:15,7) + Gcoeff * G2tensor(:,86)
  Gcoeff = (c(6)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(99)
  T3sum(1:15,7) = T3sum(1:15,7) + Gcoeff * G2tensor(:,132)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)-M(5)+M(6)+M(7)-M(8)+M(13)-M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42)-M(43)-M(53) &
    +M(54)+M(55)-M(65)+M(68)+M(77)-M(80)+M(89)-M(92)+M(101)+M(102)-M(103)-M(104)-M(105)+M(106)-M(109)-M(111)+M(115)+M(117)+M(121) &
    -M(123))+c(6)*(-M(179)+M(185)+M(245)-M(247))) * den(41)
  T4sum(1:35,134) = T4sum(1:35,134) + Gcoeff * G3tensor(:,5)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(5)-M(6)+M(7)-M(8)-M(13)+M(14)-M(19)+M(20)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42)-M(43)+M(53) &
    +M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92)+M(101)+M(102)-M(103)-M(104)-M(105)+M(106)-M(109)-M(111)+M(115)+M(117)+M(121) &
    -M(123))+c(6)*(-M(134)+M(158)+M(201)-M(202))) * den(41)
  T4sum(1:35,134) = T4sum(1:35,134) + Gcoeff * G3tensor(:,16)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(-M(134)+M(158)+M(179)-M(185)+M(201) &
    -M(202)-M(245)+M(247))) * den(41)
  T4sum(1:35,134) = T4sum(1:35,134) + Gcoeff * G3tensor(:,27)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(5)+M(6)-M(7)+M(8)+M(13)-M(14)+M(19)-M(20)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42)-M(43)-M(53) &
    +M(54)+M(55)-M(65)+M(68)+M(77)-M(80)+M(89)-M(92)+M(101)+M(102)-M(103)+M(104)-M(105)-M(106)-M(109)-M(111)+M(115)-M(117)+M(121) &
    +M(123))+c(6)*(-M(180)+M(186)+M(221)-M(223))) * den(41)
  T4sum(1:35,133) = T4sum(1:35,133) + Gcoeff * G3tensor(:,2)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)+M(5)-M(6)-M(7)+M(8)-M(13)+M(14)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)-M(41)-M(42)-M(43)+M(53) &
    +M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92)+M(101)+M(102)-M(103)+M(104)-M(105)-M(106)-M(109)-M(111)+M(115)-M(117)+M(121) &
    +M(123))+c(6)*(-M(136)+M(160)+M(195)-M(196))) * den(41)
  T4sum(1:35,133) = T4sum(1:35,133) + Gcoeff * G3tensor(:,13)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(-M(136)+M(160)+M(180)-M(186)+M(195) &
    -M(196)-M(221)+M(223))) * den(41)
  T4sum(1:35,133) = T4sum(1:35,133) + Gcoeff * G3tensor(:,24)
  Gcoeff = (c(5)*(M(49)-M(79)-M(91)+M(98)+M(106)+M(112)-M(114)-M(116)+M(117)+M(118)-M(120)-M(122))+c(6)*(M(147)+M(185)-M(187) &
    -M(189)+M(190)-M(207)-M(231)+M(245))) * den(387)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,41)
  Gcoeff = (c(5)*(M(49)-M(79)-M(91)+M(98)+M(106)+M(112)-M(114)-M(116)+M(117)+M(118)-M(120)-M(122))+c(6)*(M(158)-M(168)-M(174) &
    +M(177)+M(182)-M(192)-M(198)+M(201))) * den(387)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,87)
  Gcoeff = (c(6)*(-M(147)+M(158)-M(168)-M(174)+M(177)+M(182)-M(185)+M(187)+M(189)-M(190)-M(192)-M(198)+M(201)+M(207)+M(231) &
    -M(245))) * den(387)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,133)
  Gcoeff = (c(4)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)+M(47)+M(48)+M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98)-M(101)+M(106)+M(107)+M(112)+M(113)-M(114)-M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(M(147)-M(189)+M(190)-M(207))) * den(35)
  T4sum(1:35,109) = T4sum(1:35,109) + Gcoeff * G3tensor(:,8)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)-M(17)+M(18)+M(23)-M(24)+M(27)-M(28)-M(33)+M(34)+M(47)-M(48)+M(49) &
    -M(54)+M(59)+M(66)-M(71)-M(77)+M(78)-M(79)-M(91)+M(98)-M(101)+M(106)+M(107)+M(112)+M(113)-M(114)-M(115)-M(116)+M(117)+M(118) &
    -M(120)-M(122))+c(6)*(-M(174)+M(177)+M(182)-M(192))) * den(35)
  T4sum(1:35,109) = T4sum(1:35,109) + Gcoeff * G3tensor(:,19)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(6)*(-M(147)-M(174)+M(177)+M(182) &
    +M(189)-M(190)-M(192)+M(207))) * den(35)
  T4sum(1:35,109) = T4sum(1:35,109) + Gcoeff * G3tensor(:,30)
  Gcoeff = (c(4)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)+M(47)+M(48)-M(49) &
    -M(54)+M(59)-M(66)+M(71)-M(77)-M(78)+M(79)+M(91)-M(98)-M(101)-M(106)+M(107)-M(112)+M(113)+M(114)-M(115)+M(116)-M(117)-M(118) &
    +M(120)+M(122))+c(6)*(-M(185)+M(187)+M(231)-M(245))) * den(35)
  T4sum(1:35,111) = T4sum(1:35,111) + Gcoeff * G3tensor(:,6)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)-M(17)+M(18)+M(23)-M(24)+M(27)-M(28)-M(33)+M(34)+M(47)-M(48)-M(49) &
    -M(54)+M(59)+M(66)-M(71)-M(77)+M(78)+M(79)+M(91)-M(98)-M(101)-M(106)+M(107)-M(112)+M(113)+M(114)-M(115)+M(116)-M(117)-M(118) &
    +M(120)+M(122))+c(6)*(-M(158)+M(168)+M(198)-M(201))) * den(35)
  T4sum(1:35,111) = T4sum(1:35,111) + Gcoeff * G3tensor(:,17)
  Gcoeff = (c(5)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(6)*(-M(158)+M(168)+M(185)-M(187) &
    +M(198)-M(201)-M(231)+M(245))) * den(35)
  T4sum(1:35,111) = T4sum(1:35,111) + Gcoeff * G3tensor(:,28)
  Gcoeff = (c(5)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(171)+M(179)-M(181) &
    -M(183)+M(184)-M(213)-M(237)+M(247))) * den(325)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,42)
  Gcoeff = (c(5)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(134)-M(144)-M(150) &
    +M(153)+M(188)-M(194)-M(200)+M(202))) * den(325)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,88)
  Gcoeff = (c(6)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(325)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,134)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)+M(31)-M(32)-M(35)+M(36)-M(42)+M(47)+M(59) &
    +M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(171)-M(183)+M(184)-M(213))) * den(14)
  T4sum(1:35,73) = T4sum(1:35,73) + Gcoeff * G3tensor(:,9)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    -M(60)+M(61)+M(69)-M(72)-M(80)+M(81)-M(82)-M(94)+M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(150)+M(153)+M(188)-M(194))) * den(14)
  T4sum(1:35,73) = T4sum(1:35,73) + Gcoeff * G3tensor(:,20)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(150)+M(153)-M(171)+M(183)-M(184) &
    +M(188)-M(194)+M(213))) * den(14)
  T4sum(1:35,73) = T4sum(1:35,73) + Gcoeff * G3tensor(:,31)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)+M(31)-M(32)-M(35)+M(36)-M(42)+M(47)+M(59) &
    +M(60)-M(61)-M(69)+M(72)-M(80)-M(81)+M(82)+M(94)-M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(179)+M(181)+M(237)-M(247))) * den(14)
  T4sum(1:35,75) = T4sum(1:35,75) + Gcoeff * G3tensor(:,7)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(25)-M(26)-M(29)+M(30)-M(31)+M(32)+M(35)-M(36)-M(42)+M(47)+M(59) &
    -M(60)-M(61)+M(69)-M(72)-M(80)+M(81)+M(82)+M(94)-M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(134)+M(144)+M(200)-M(202))) * den(14)
  T4sum(1:35,75) = T4sum(1:35,75) + Gcoeff * G3tensor(:,18)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(6)*(-M(134)+M(144)+M(179)-M(181)+M(200) &
    -M(202)-M(237)+M(247))) * den(14)
  T4sum(1:35,75) = T4sum(1:35,75) + Gcoeff * G3tensor(:,29)
  Gcoeff = (c(5)*(-M(52)+M(79)-M(86)+M(91)-M(104)-M(110)+M(114)+M(116)+M(120)+M(122)-M(123)-M(124))+c(6)*(-M(153)-M(186)+M(187) &
    -M(188)+M(189)+M(207)-M(221)+M(231))) * den(390)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,43)
  Gcoeff = (c(5)*(-M(52)+M(79)-M(86)+M(91)-M(104)-M(110)+M(114)+M(116)+M(120)+M(122)-M(123)-M(124))+c(6)*(-M(160)+M(168)-M(171) &
    +M(174)-M(184)+M(192)-M(195)+M(198))) * den(390)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,89)
  Gcoeff = (c(6)*(M(153)-M(160)+M(168)-M(171)+M(174)-M(184)+M(186)-M(187)+M(188)-M(189)+M(192)-M(195)+M(198)-M(207)+M(221) &
    -M(231))) * den(390)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,135)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)+M(50)+M(51)+M(52) &
    -M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91)-M(102)+M(104)+M(108)+M(110)-M(114)-M(116)+M(119)-M(120)-M(121)-M(122) &
    +M(123)+M(124))+c(6)*(M(153)-M(187)+M(188)-M(231))) * den(37)
  T4sum(1:35,106) = T4sum(1:35,106) + Gcoeff * G3tensor(:,10)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(11)-M(12)+M(17)-M(18)+M(19)-M(20)-M(21)+M(22)-M(23)+M(24)+M(25)-M(26)-M(29)+M(30)+M(50)-M(51)+M(52) &
    -M(55)+M(62)+M(67)-M(74)-M(79)+M(86)-M(89)+M(90)-M(91)-M(102)+M(104)+M(108)+M(110)-M(114)-M(116)+M(119)-M(120)-M(121)-M(122) &
    +M(123)+M(124))+c(6)*(-M(168)+M(171)+M(184)-M(198))) * den(37)
  T4sum(1:35,106) = T4sum(1:35,106) + Gcoeff * G3tensor(:,21)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(153)-M(168)+M(171)+M(184) &
    +M(187)-M(188)-M(198)+M(231))) * den(37)
  T4sum(1:35,106) = T4sum(1:35,106) + Gcoeff * G3tensor(:,32)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)+M(50)+M(51)-M(52) &
    -M(55)+M(62)-M(67)+M(74)+M(79)-M(86)-M(89)-M(90)+M(91)-M(102)-M(104)+M(108)-M(110)+M(114)+M(116)+M(119)+M(120)-M(121)+M(122) &
    -M(123)-M(124))+c(6)*(-M(186)+M(189)+M(207)-M(221))) * den(37)
  T4sum(1:35,108) = T4sum(1:35,108) + Gcoeff * G3tensor(:,3)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(11)-M(12)+M(17)-M(18)+M(19)-M(20)-M(21)+M(22)-M(23)+M(24)+M(25)-M(26)-M(29)+M(30)+M(50)-M(51)-M(52) &
    -M(55)+M(62)+M(67)-M(74)+M(79)-M(86)-M(89)+M(90)+M(91)-M(102)-M(104)+M(108)-M(110)+M(114)+M(116)+M(119)+M(120)-M(121)+M(122) &
    -M(123)-M(124))+c(6)*(-M(160)+M(174)+M(192)-M(195))) * den(37)
  T4sum(1:35,108) = T4sum(1:35,108) + Gcoeff * G3tensor(:,14)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(160)+M(174)+M(186)-M(189) &
    +M(192)-M(195)-M(207)+M(221))) * den(37)
  T4sum(1:35,108) = T4sum(1:35,108) + Gcoeff * G3tensor(:,25)
  Gcoeff = (c(5)*(-M(64)+M(82)-M(87)+M(94)-M(106)+M(114)-M(117)+M(120)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(177)-M(180)+M(181) &
    -M(182)+M(183)+M(213)-M(223)+M(237))) * den(328)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,44)
  Gcoeff = (c(5)*(-M(64)+M(82)-M(87)+M(94)-M(106)+M(114)-M(117)+M(120)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(136)+M(144)-M(147) &
    +M(150)-M(190)+M(194)-M(196)+M(200))) * den(328)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,90)
  Gcoeff = (c(6)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(328)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,136)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(27)-M(28)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(43)+M(50)+M(62) &
    +M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94)-M(105)+M(106)+M(108)-M(111)-M(114)+M(117)+M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(177)-M(181)+M(182)-M(237))) * den(16)
  T4sum(1:35,70) = T4sum(1:35,70) + Gcoeff * G3tensor(:,11)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(27)-M(28)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(43)+M(50)+M(62) &
    -M(63)+M(64)+M(70)-M(75)-M(82)+M(87)-M(92)+M(93)-M(94)-M(105)+M(106)+M(108)-M(111)-M(114)+M(117)+M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(144)+M(147)+M(190)-M(200))) * den(16)
  T4sum(1:35,70) = T4sum(1:35,70) + Gcoeff * G3tensor(:,22)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(144)+M(147)-M(177)+M(181)-M(182) &
    +M(190)-M(200)+M(237))) * den(16)
  T4sum(1:35,70) = T4sum(1:35,70) + Gcoeff * G3tensor(:,33)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(27)-M(28)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(43)+M(50)+M(62) &
    +M(63)-M(64)-M(70)+M(75)+M(82)-M(87)-M(92)-M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(180)+M(183)+M(213)-M(223))) * den(16)
  T4sum(1:35,72) = T4sum(1:35,72) + Gcoeff * G3tensor(:,4)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(27)-M(28)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(43)+M(50)+M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)-M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(136)+M(150)+M(194)-M(196))) * den(16)
  T4sum(1:35,72) = T4sum(1:35,72) + Gcoeff * G3tensor(:,15)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(136)+M(150)+M(180)-M(183)+M(194) &
    -M(196)-M(213)+M(223))) * den(16)
  T4sum(1:35,72) = T4sum(1:35,72) + Gcoeff * G3tensor(:,26)
  Gcoeff = (c(5)*(M(49)-M(52)-M(86)+M(98)-M(104)+M(106)-M(110)+M(112)+M(117)+M(118)-M(123)-M(124))+c(6)*(M(147)-M(153)+M(185) &
    -M(186)-M(188)+M(190)-M(221)+M(245))) * den(187)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,45)
  Gcoeff = (c(5)*(M(49)-M(52)-M(86)+M(98)-M(104)+M(106)-M(110)+M(112)+M(117)+M(118)-M(123)-M(124))+c(6)*(M(158)-M(160)-M(171) &
    +M(177)+M(182)-M(184)-M(195)+M(201))) * den(187)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,91)
  Gcoeff = (c(6)*(-M(147)+M(153)+M(158)-M(160)-M(171)+M(177)+M(182)-M(184)-M(185)+M(186)+M(188)-M(190)-M(195)+M(201)+M(221) &
    -M(245))) * den(187)
  T3sum(1:15,71) = T3sum(1:15,71) + Gcoeff * G2tensor(:,137)
  Gcoeff = (c(5)*(M(61)-M(64)-M(87)+M(99)+M(104)-M(106)-M(117)+M(123)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(171)-M(177)+M(179) &
    -M(180)-M(182)+M(184)-M(223)+M(247))) * den(163)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,46)
  Gcoeff = (c(5)*(M(61)-M(64)-M(87)+M(99)+M(104)-M(106)-M(117)+M(123)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(134)-M(136)-M(147) &
    +M(153)+M(188)-M(190)-M(196)+M(202))) * den(163)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,92)
  Gcoeff = (c(6)*(M(134)-M(136)-M(147)+M(153)-M(171)+M(177)-M(179)+M(180)+M(182)-M(184)+M(188)-M(190)-M(196)+M(202)+M(223) &
    -M(247))) * den(163)
  T3sum(1:15,72) = T3sum(1:15,72) + Gcoeff * G2tensor(:,138)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)+M(61) &
    -M(64)-M(73)+M(76)+M(86)-M(87)+M(88)-M(98)+M(99)-M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(171)-M(177)-M(182)+M(184))) * den(26)
  T4sum(1:35,48) = T4sum(1:35,48) + Gcoeff * G3tensor(:,1)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(49)+M(52)+M(61) &
    -M(64)+M(73)-M(76)+M(86)-M(87)-M(88)-M(98)+M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(147)+M(153)+M(188)-M(190))) * den(26)
  T4sum(1:35,48) = T4sum(1:35,48) + Gcoeff * G3tensor(:,12)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(73)-M(76)-M(88)+M(100))+c(6)*(-M(147)+M(153)-M(171)+M(177) &
    +M(182)-M(184)+M(188)-M(190))) * den(26)
  T4sum(1:35,48) = T4sum(1:35,48) + Gcoeff * G3tensor(:,23)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(6)-M(7)-M(9)-M(12)-M(14)-M(16)-M(17)-M(19)-M(21)-M(24)-M(26)-M(27)-M(29)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(43)+M(47)+M(49)+M(53)+M(59)+M(60)+M(65)+M(66)+M(67)+M(70)+M(72)+M(73)+M(78)+M(82)+M(90)+M(92)+M(93)+M(94)+M(98) &
    +M(100)+M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(181)+M(237)))
  T5sum(1:70,153) = T5sum(1:70,153) + Gcoeff * G4tensor(:,4)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(41)+M(43)+M(47)+M(48)+M(49)+M(51)+M(59)+M(63)+M(68)+M(69)+M(71)+M(74)+M(75)+M(76)+M(81)+M(82)+M(88)+M(92)+M(94)+M(98) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(144)+M(200)))
  T5sum(1:70,153) = T5sum(1:70,153) + Gcoeff * G4tensor(:,8)
  Gcoeff = (c(4)*(M(3)-M(4)-M(5)+M(6)-M(13)+M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)+M(63)-M(65)-M(66)-M(67)+M(68)+M(69)-M(70)+M(71)-M(72)-M(73)+M(74)+M(75)+M(76)-M(78)+M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(144)-M(181)+M(200)-M(237)))
  T5sum(1:70,153) = T5sum(1:70,153) + Gcoeff * G4tensor(:,12)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(10)-M(12)-M(14)-M(15)-M(17)-M(20)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(38) &
    -M(39)+M(47)+M(49)+M(50)+M(53)+M(59)+M(60)+M(62)+M(63)+M(64)+M(65)+M(66)+M(67)+M(72)+M(73)+M(75)+M(78)+M(87)+M(90)+M(98) &
    +M(100)+M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(177)+M(182)))
  T5sum(1:70,154) = T5sum(1:70,154) + Gcoeff * G4tensor(:,1)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(47)+M(48)+M(49)+M(50)+M(51)+M(59)+M(62)+M(64)+M(68)+M(69)+M(70)+M(71)+M(74)+M(76)+M(81)+M(87)+M(88)+M(93)+M(98) &
    +M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(147)+M(190)))
  T5sum(1:70,154) = T5sum(1:70,154) + Gcoeff * G4tensor(:,5)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)+M(17)-M(18)-M(19)+M(20)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)-M(63)-M(65)-M(66)-M(67)+M(68)+M(69)+M(70)+M(71)-M(72)-M(73)+M(74)-M(75)+M(76)-M(78)+M(81)+M(88)-M(90)+M(93) &
    -M(100))+c(6)*(M(147)-M(177)-M(182)+M(190)))
  T5sum(1:70,154) = T5sum(1:70,154) + Gcoeff * G4tensor(:,9)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(5)-M(8)-M(10)-M(11)-M(13)-M(15)-M(18)-M(20)-M(22)-M(23)-M(25)-M(28)-M(30)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(42)+M(50)+M(52)+M(53)+M(62)+M(63)+M(65)+M(66)+M(67)+M(69)+M(75)+M(76)+M(78)+M(80)+M(81)+M(82)+M(86)+M(88)+M(90)+M(94) &
    +M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(183)+M(213)))
  T5sum(1:70,155) = T5sum(1:70,155) + Gcoeff * G4tensor(:,3)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(48)+M(50)+M(51)+M(52)+M(60)+M(62)+M(68)+M(70)+M(71)+M(72)+M(73)+M(74)+M(80)+M(82)+M(86)+M(93)+M(94) &
    +M(100)+M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(150)+M(194)))
  T5sum(1:70,155) = T5sum(1:70,155) + Gcoeff * G4tensor(:,7)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)-M(63)-M(65)-M(66)-M(67)+M(68)-M(69)+M(70)+M(71)+M(72)+M(73)+M(74)-M(75)-M(76)-M(78)-M(81)-M(88)-M(90)+M(93) &
    +M(100))+c(6)*(M(150)-M(183)+M(194)-M(213)))
  T5sum(1:70,155) = T5sum(1:70,155) + Gcoeff * G4tensor(:,11)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(10)-M(12)-M(14)-M(15)-M(18)-M(20)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(47)+M(50)+M(52)+M(53)+M(59)+M(60)+M(61)+M(62)+M(63)+M(65)+M(66)+M(67)+M(72)+M(75)+M(76)+M(78)+M(86)+M(88)+M(90)+M(99) &
    +M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(171)+M(184)))
  T5sum(1:70,156) = T5sum(1:70,156) + Gcoeff * G4tensor(:,2)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(47)+M(48)+M(50)+M(51)+M(52)+M(59)+M(61)+M(62)+M(68)+M(69)+M(70)+M(71)+M(73)+M(74)+M(81)+M(86)+M(93)+M(99) &
    +M(100)+M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(153)+M(188)))
  T5sum(1:70,156) = T5sum(1:70,156) + Gcoeff * G4tensor(:,6)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)-M(63)-M(65)-M(66)-M(67)+M(68)+M(69)+M(70)+M(71)-M(72)+M(73)+M(74)-M(75)-M(76)-M(78)+M(81)-M(88)-M(90)+M(93) &
    +M(100))+c(6)*(M(153)-M(171)-M(184)+M(188)))
  T5sum(1:70,156) = T5sum(1:70,156) + Gcoeff * G4tensor(:,10)

end subroutine vamp_6

end module ol_vamp_6_ppjjjj_gggggg_1_/**/REALKIND
