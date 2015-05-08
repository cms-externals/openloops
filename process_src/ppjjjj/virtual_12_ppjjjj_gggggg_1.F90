
module ol_vamp_12_ppjjjj_gggggg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_12(M)
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
  complex(REALKIND), dimension(4,5,4,190) :: G1
  complex(REALKIND), dimension(4,15,4,259) :: G2
  complex(REALKIND), dimension(4,35,4,198) :: G3
  complex(REALKIND), dimension(4,70,4,60) :: G4
  complex(REALKIND), dimension(4,126,4,24) :: G5
  complex(REALKIND), dimension(15,135) :: G2tensor
  complex(REALKIND), dimension(35,153) :: G3tensor
  complex(REALKIND), dimension(70,162) :: G4tensor
  complex(REALKIND), dimension(126,36) :: G5tensor
  complex(REALKIND), dimension(210,24) :: G6tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,-1),Q(:,2),G1(:,:,:,1))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,20),G1(:,:,:,2))
  call check_last_UV_W(l_switch,G1(:,:,:,2),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,1))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,23),G1(:,:,:,3))
  call check_last_UV_W(l_switch,G1(:,:,:,3),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,2))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,24),G1(:,:,:,4))
  call check_last_UV_W(l_switch,G1(:,:,:,4),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,3))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,20),wf(:,-5),G1(:,:,:,5))
  call check_last_UV_W(l_switch,G1(:,:,:,5),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,4))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,23),wf(:,-5),G1(:,:,:,6))
  call check_last_UV_W(l_switch,G1(:,:,:,6),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,5))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,24),wf(:,-5),G1(:,:,:,7))
  call check_last_UV_W(l_switch,G1(:,:,:,7),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,6))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,20),G1(:,:,:,8))
  call check_last_UV_W(l_switch,G1(:,:,:,8),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,7))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,23),G1(:,:,:,9))
  call check_last_UV_W(l_switch,G1(:,:,:,9),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,8))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,24),G1(:,:,:,10))
  call check_last_UV_W(l_switch,G1(:,:,:,10),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,9))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-4),wf(:,14),G1(:,:,:,11))
  call check_last_UV_W(l_switch,G1(:,:,:,11),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,10))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-4),wf(:,17),G1(:,:,:,12))
  call check_last_UV_W(l_switch,G1(:,:,:,12),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,11))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-4),wf(:,18),G1(:,:,:,13))
  call check_last_UV_W(l_switch,G1(:,:,:,13),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,12))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,14),wf(:,-4),G1(:,:,:,14))
  call check_last_UV_W(l_switch,G1(:,:,:,14),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,13))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,17),wf(:,-4),G1(:,:,:,15))
  call check_last_UV_W(l_switch,G1(:,:,:,15),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,14))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,18),wf(:,-4),G1(:,:,:,16))
  call check_last_UV_W(l_switch,G1(:,:,:,16),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,15))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-4),wf(:,14),G1(:,:,:,17))
  call check_last_UV_W(l_switch,G1(:,:,:,17),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,16))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-4),wf(:,17),G1(:,:,:,18))
  call check_last_UV_W(l_switch,G1(:,:,:,18),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,17))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-4),wf(:,18),G1(:,:,:,19))
  call check_last_UV_W(l_switch,G1(:,:,:,19),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,18))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,-4),G1(:,:,:,20))
  call loop_GGG_G_12(G1(:,:,:,20),wf(:,-3),wf(:,-2),G1(:,:,:,21))
  call check_last_UV_W(l_switch,G1(:,:,:,21),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,19))
  call loop_GGG_G_12(G1(:,:,:,20),wf(:,-2),wf(:,-3),G1(:,:,:,22))
  call check_last_UV_W(l_switch,G1(:,:,:,22),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,20))
  call loop_GGG_G_23(G1(:,:,:,20),wf(:,-3),wf(:,-2),G1(:,:,:,23))
  call check_last_UV_W(l_switch,G1(:,:,:,23),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,21))
  call loop_UV_W(G1(:,:,:,20),Q(:,50),wf(:,62),Q(:,12),G2(:,:,:,1))
  call check_last_UV_W(l_switch,G2(:,:,:,1),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,1))
  call loop_UV_W(G1(:,:,:,20),Q(:,50),wf(:,-3),Q(:,8),G2(:,:,:,2))
  call loop_UV_W(G2(:,:,:,2),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,1))
  call check_last_UV_W(l_switch,G3(:,:,:,1),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,1))
  call loop_UV_W(G1(:,:,:,20),Q(:,50),wf(:,-2),Q(:,4),G2(:,:,:,3))
  call loop_UV_W(G2(:,:,:,3),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,2))
  call check_last_UV_W(l_switch,G3(:,:,:,2),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,2))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-4),wf(:,-5),G1(:,:,:,24))
  call loop_GGG_G_12(G1(:,:,:,24),wf(:,-3),wf(:,-2),G1(:,:,:,25))
  call check_last_UV_W(l_switch,G1(:,:,:,25),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,22))
  call loop_GGG_G_12(G1(:,:,:,24),wf(:,-2),wf(:,-3),G1(:,:,:,26))
  call check_last_UV_W(l_switch,G1(:,:,:,26),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,23))
  call loop_GGG_G_23(G1(:,:,:,24),wf(:,-3),wf(:,-2),G1(:,:,:,27))
  call check_last_UV_W(l_switch,G1(:,:,:,27),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,24))
  call loop_UV_W(G1(:,:,:,24),Q(:,50),wf(:,62),Q(:,12),G2(:,:,:,4))
  call check_last_UV_W(l_switch,G2(:,:,:,4),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,2))
  call loop_UV_W(G1(:,:,:,24),Q(:,50),wf(:,-3),Q(:,8),G2(:,:,:,5))
  call loop_UV_W(G2(:,:,:,5),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,3))
  call check_last_UV_W(l_switch,G3(:,:,:,3),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,3))
  call loop_UV_W(G1(:,:,:,24),Q(:,50),wf(:,-2),Q(:,4),G2(:,:,:,6))
  call loop_UV_W(G2(:,:,:,6),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,4))
  call check_last_UV_W(l_switch,G3(:,:,:,4),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,4))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,-4),G1(:,:,:,28))
  call loop_GGG_G_12(G1(:,:,:,28),wf(:,-3),wf(:,-2),G1(:,:,:,29))
  call check_last_UV_W(l_switch,G1(:,:,:,29),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,25))
  call loop_GGG_G_12(G1(:,:,:,28),wf(:,-2),wf(:,-3),G1(:,:,:,30))
  call check_last_UV_W(l_switch,G1(:,:,:,30),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,26))
  call loop_GGG_G_23(G1(:,:,:,28),wf(:,-3),wf(:,-2),G1(:,:,:,31))
  call check_last_UV_W(l_switch,G1(:,:,:,31),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,27))
  call loop_UV_W(G1(:,:,:,28),Q(:,50),wf(:,62),Q(:,12),G2(:,:,:,7))
  call check_last_UV_W(l_switch,G2(:,:,:,7),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,3))
  call loop_UV_W(G1(:,:,:,28),Q(:,50),wf(:,-3),Q(:,8),G2(:,:,:,8))
  call loop_UV_W(G2(:,:,:,8),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,5))
  call check_last_UV_W(l_switch,G3(:,:,:,5),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,5))
  call loop_UV_W(G1(:,:,:,28),Q(:,50),wf(:,-2),Q(:,4),G2(:,:,:,9))
  call loop_UV_W(G2(:,:,:,9),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,6))
  call check_last_UV_W(l_switch,G3(:,:,:,6),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,6))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,-2),G1(:,:,:,32))
  call loop_GGG_G_12(G1(:,:,:,32),wf(:,-5),wf(:,-4),G1(:,:,:,33))
  call check_last_UV_W(l_switch,G1(:,:,:,33),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,28))
  call loop_GGG_G_12(G1(:,:,:,32),wf(:,-4),wf(:,-5),G1(:,:,:,34))
  call check_last_UV_W(l_switch,G1(:,:,:,34),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,29))
  call loop_GGG_G_23(G1(:,:,:,32),wf(:,-5),wf(:,-4),G1(:,:,:,35))
  call check_last_UV_W(l_switch,G1(:,:,:,35),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,30))
  call loop_UV_W(G1(:,:,:,32),Q(:,14),wf(:,84),Q(:,48),G2(:,:,:,10))
  call check_last_UV_W(l_switch,G2(:,:,:,10),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,4))
  call loop_UV_W(G1(:,:,:,32),Q(:,14),wf(:,-5),Q(:,32),G2(:,:,:,11))
  call loop_UV_W(G2(:,:,:,11),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,7))
  call check_last_UV_W(l_switch,G3(:,:,:,7),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,7))
  call loop_UV_W(G1(:,:,:,32),Q(:,14),wf(:,-4),Q(:,16),G2(:,:,:,12))
  call loop_UV_W(G2(:,:,:,12),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,8))
  call check_last_UV_W(l_switch,G3(:,:,:,8),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,8))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,-3),G1(:,:,:,36))
  call loop_GGG_G_12(G1(:,:,:,36),wf(:,-5),wf(:,-4),G1(:,:,:,37))
  call check_last_UV_W(l_switch,G1(:,:,:,37),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,31))
  call loop_GGG_G_12(G1(:,:,:,36),wf(:,-4),wf(:,-5),G1(:,:,:,38))
  call check_last_UV_W(l_switch,G1(:,:,:,38),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,32))
  call loop_GGG_G_23(G1(:,:,:,36),wf(:,-5),wf(:,-4),G1(:,:,:,39))
  call check_last_UV_W(l_switch,G1(:,:,:,39),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,33))
  call loop_UV_W(G1(:,:,:,36),Q(:,14),wf(:,84),Q(:,48),G2(:,:,:,13))
  call check_last_UV_W(l_switch,G2(:,:,:,13),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,5))
  call loop_UV_W(G1(:,:,:,36),Q(:,14),wf(:,-5),Q(:,32),G2(:,:,:,14))
  call loop_UV_W(G2(:,:,:,14),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,9))
  call check_last_UV_W(l_switch,G3(:,:,:,9),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,9))
  call loop_UV_W(G1(:,:,:,36),Q(:,14),wf(:,-4),Q(:,16),G2(:,:,:,15))
  call loop_UV_W(G2(:,:,:,15),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,10))
  call check_last_UV_W(l_switch,G3(:,:,:,10),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,10))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,-2),G1(:,:,:,40))
  call loop_GGG_G_12(G1(:,:,:,40),wf(:,-5),wf(:,-4),G1(:,:,:,41))
  call check_last_UV_W(l_switch,G1(:,:,:,41),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,34))
  call loop_GGG_G_12(G1(:,:,:,40),wf(:,-4),wf(:,-5),G1(:,:,:,42))
  call check_last_UV_W(l_switch,G1(:,:,:,42),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,35))
  call loop_GGG_G_23(G1(:,:,:,40),wf(:,-5),wf(:,-4),G1(:,:,:,43))
  call check_last_UV_W(l_switch,G1(:,:,:,43),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,36))
  call loop_UV_W(G1(:,:,:,40),Q(:,14),wf(:,84),Q(:,48),G2(:,:,:,16))
  call check_last_UV_W(l_switch,G2(:,:,:,16),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,6))
  call loop_UV_W(G1(:,:,:,40),Q(:,14),wf(:,-5),Q(:,32),G2(:,:,:,17))
  call loop_UV_W(G2(:,:,:,17),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,11))
  call check_last_UV_W(l_switch,G3(:,:,:,11),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,11))
  call loop_UV_W(G1(:,:,:,40),Q(:,14),wf(:,-4),Q(:,16),G2(:,:,:,18))
  call loop_UV_W(G2(:,:,:,18),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,12))
  call check_last_UV_W(l_switch,G3(:,:,:,12),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,12))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,8),G1(:,:,:,44))
  call check_last_UV_W(l_switch,G1(:,:,:,44),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,37))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,11),G1(:,:,:,45))
  call check_last_UV_W(l_switch,G1(:,:,:,45),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,38))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,12),G1(:,:,:,46))
  call check_last_UV_W(l_switch,G1(:,:,:,46),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,39))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,8),wf(:,-3),G1(:,:,:,47))
  call check_last_UV_W(l_switch,G1(:,:,:,47),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,40))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,11),wf(:,-3),G1(:,:,:,48))
  call check_last_UV_W(l_switch,G1(:,:,:,48),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,41))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,12),wf(:,-3),G1(:,:,:,49))
  call check_last_UV_W(l_switch,G1(:,:,:,49),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,42))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,8),G1(:,:,:,50))
  call check_last_UV_W(l_switch,G1(:,:,:,50),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,43))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,11),G1(:,:,:,51))
  call check_last_UV_W(l_switch,G1(:,:,:,51),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,44))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,12),G1(:,:,:,52))
  call check_last_UV_W(l_switch,G1(:,:,:,52),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,45))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,-3),G1(:,:,:,53))
  call loop_GGG_G_12(G1(:,:,:,53),wf(:,-4),wf(:,-2),G1(:,:,:,54))
  call check_last_UV_W(l_switch,G1(:,:,:,54),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,46))
  call loop_GGG_G_12(G1(:,:,:,53),wf(:,-2),wf(:,-4),G1(:,:,:,55))
  call check_last_UV_W(l_switch,G1(:,:,:,55),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,47))
  call loop_GGG_G_23(G1(:,:,:,53),wf(:,-4),wf(:,-2),G1(:,:,:,56))
  call check_last_UV_W(l_switch,G1(:,:,:,56),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,48))
  call loop_UV_W(G1(:,:,:,53),Q(:,42),wf(:,66),Q(:,20),G2(:,:,:,19))
  call check_last_UV_W(l_switch,G2(:,:,:,19),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,7))
  call loop_UV_W(G1(:,:,:,53),Q(:,42),wf(:,-4),Q(:,16),G2(:,:,:,20))
  call loop_UV_W(G2(:,:,:,20),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,13))
  call check_last_UV_W(l_switch,G3(:,:,:,13),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,13))
  call loop_UV_W(G1(:,:,:,53),Q(:,42),wf(:,-2),Q(:,4),G2(:,:,:,21))
  call loop_UV_W(G2(:,:,:,21),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,14))
  call check_last_UV_W(l_switch,G3(:,:,:,14),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,14))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,-5),G1(:,:,:,57))
  call loop_GGG_G_12(G1(:,:,:,57),wf(:,-4),wf(:,-2),G1(:,:,:,58))
  call check_last_UV_W(l_switch,G1(:,:,:,58),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,49))
  call loop_GGG_G_12(G1(:,:,:,57),wf(:,-2),wf(:,-4),G1(:,:,:,59))
  call check_last_UV_W(l_switch,G1(:,:,:,59),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,50))
  call loop_GGG_G_23(G1(:,:,:,57),wf(:,-4),wf(:,-2),G1(:,:,:,60))
  call check_last_UV_W(l_switch,G1(:,:,:,60),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,51))
  call loop_UV_W(G1(:,:,:,57),Q(:,42),wf(:,66),Q(:,20),G2(:,:,:,22))
  call check_last_UV_W(l_switch,G2(:,:,:,22),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,8))
  call loop_UV_W(G1(:,:,:,57),Q(:,42),wf(:,-4),Q(:,16),G2(:,:,:,23))
  call loop_UV_W(G2(:,:,:,23),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,15))
  call check_last_UV_W(l_switch,G3(:,:,:,15),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,15))
  call loop_UV_W(G1(:,:,:,57),Q(:,42),wf(:,-2),Q(:,4),G2(:,:,:,24))
  call loop_UV_W(G2(:,:,:,24),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,16))
  call check_last_UV_W(l_switch,G3(:,:,:,16),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,16))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,-3),G1(:,:,:,61))
  call loop_GGG_G_12(G1(:,:,:,61),wf(:,-4),wf(:,-2),G1(:,:,:,62))
  call check_last_UV_W(l_switch,G1(:,:,:,62),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,52))
  call loop_GGG_G_12(G1(:,:,:,61),wf(:,-2),wf(:,-4),G1(:,:,:,63))
  call check_last_UV_W(l_switch,G1(:,:,:,63),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,53))
  call loop_GGG_G_23(G1(:,:,:,61),wf(:,-4),wf(:,-2),G1(:,:,:,64))
  call check_last_UV_W(l_switch,G1(:,:,:,64),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,54))
  call loop_UV_W(G1(:,:,:,61),Q(:,42),wf(:,66),Q(:,20),G2(:,:,:,25))
  call check_last_UV_W(l_switch,G2(:,:,:,25),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,9))
  call loop_UV_W(G1(:,:,:,61),Q(:,42),wf(:,-4),Q(:,16),G2(:,:,:,26))
  call loop_UV_W(G2(:,:,:,26),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,17))
  call check_last_UV_W(l_switch,G3(:,:,:,17),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,17))
  call loop_UV_W(G1(:,:,:,61),Q(:,42),wf(:,-2),Q(:,4),G2(:,:,:,27))
  call loop_UV_W(G2(:,:,:,27),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,18))
  call check_last_UV_W(l_switch,G3(:,:,:,18),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,18))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-4),wf(:,-2),G1(:,:,:,65))
  call loop_GGG_G_12(G1(:,:,:,65),wf(:,-5),wf(:,-3),G1(:,:,:,66))
  call check_last_UV_W(l_switch,G1(:,:,:,66),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,55))
  call loop_GGG_G_12(G1(:,:,:,65),wf(:,-3),wf(:,-5),G1(:,:,:,67))
  call check_last_UV_W(l_switch,G1(:,:,:,67),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,56))
  call loop_GGG_G_23(G1(:,:,:,65),wf(:,-5),wf(:,-3),G1(:,:,:,68))
  call check_last_UV_W(l_switch,G1(:,:,:,68),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,57))
  call loop_UV_W(G1(:,:,:,65),Q(:,22),wf(:,79),Q(:,40),G2(:,:,:,28))
  call check_last_UV_W(l_switch,G2(:,:,:,28),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,10))
  call loop_UV_W(G1(:,:,:,65),Q(:,22),wf(:,-5),Q(:,32),G2(:,:,:,29))
  call loop_UV_W(G2(:,:,:,29),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,19))
  call check_last_UV_W(l_switch,G3(:,:,:,19),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,19))
  call loop_UV_W(G1(:,:,:,65),Q(:,22),wf(:,-3),Q(:,8),G2(:,:,:,30))
  call loop_UV_W(G2(:,:,:,30),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,20))
  call check_last_UV_W(l_switch,G3(:,:,:,20),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,20))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,-4),G1(:,:,:,69))
  call loop_GGG_G_12(G1(:,:,:,69),wf(:,-5),wf(:,-3),G1(:,:,:,70))
  call check_last_UV_W(l_switch,G1(:,:,:,70),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,58))
  call loop_GGG_G_12(G1(:,:,:,69),wf(:,-3),wf(:,-5),G1(:,:,:,71))
  call check_last_UV_W(l_switch,G1(:,:,:,71),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,59))
  call loop_GGG_G_23(G1(:,:,:,69),wf(:,-5),wf(:,-3),G1(:,:,:,72))
  call check_last_UV_W(l_switch,G1(:,:,:,72),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,60))
  call loop_UV_W(G1(:,:,:,69),Q(:,22),wf(:,79),Q(:,40),G2(:,:,:,31))
  call check_last_UV_W(l_switch,G2(:,:,:,31),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,11))
  call loop_UV_W(G1(:,:,:,69),Q(:,22),wf(:,-5),Q(:,32),G2(:,:,:,32))
  call loop_UV_W(G2(:,:,:,32),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,21))
  call check_last_UV_W(l_switch,G3(:,:,:,21),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,21))
  call loop_UV_W(G1(:,:,:,69),Q(:,22),wf(:,-3),Q(:,8),G2(:,:,:,33))
  call loop_UV_W(G2(:,:,:,33),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,22))
  call check_last_UV_W(l_switch,G3(:,:,:,22),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,22))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-4),wf(:,-2),G1(:,:,:,73))
  call loop_GGG_G_12(G1(:,:,:,73),wf(:,-5),wf(:,-3),G1(:,:,:,74))
  call check_last_UV_W(l_switch,G1(:,:,:,74),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,61))
  call loop_GGG_G_12(G1(:,:,:,73),wf(:,-3),wf(:,-5),G1(:,:,:,75))
  call check_last_UV_W(l_switch,G1(:,:,:,75),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,62))
  call loop_GGG_G_23(G1(:,:,:,73),wf(:,-5),wf(:,-3),G1(:,:,:,76))
  call check_last_UV_W(l_switch,G1(:,:,:,76),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,63))
  call loop_UV_W(G1(:,:,:,73),Q(:,22),wf(:,79),Q(:,40),G2(:,:,:,34))
  call check_last_UV_W(l_switch,G2(:,:,:,34),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,12))
  call loop_UV_W(G1(:,:,:,73),Q(:,22),wf(:,-5),Q(:,32),G2(:,:,:,35))
  call loop_UV_W(G2(:,:,:,35),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,23))
  call check_last_UV_W(l_switch,G3(:,:,:,23),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,23))
  call loop_UV_W(G1(:,:,:,73),Q(:,22),wf(:,-3),Q(:,8),G2(:,:,:,36))
  call loop_UV_W(G2(:,:,:,36),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,24))
  call check_last_UV_W(l_switch,G3(:,:,:,24),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,24))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-4),wf(:,-3),G1(:,:,:,77))
  call loop_GGG_G_12(G1(:,:,:,77),wf(:,-5),wf(:,-2),G1(:,:,:,78))
  call check_last_UV_W(l_switch,G1(:,:,:,78),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,64))
  call loop_GGG_G_12(G1(:,:,:,77),wf(:,-2),wf(:,-5),G1(:,:,:,79))
  call check_last_UV_W(l_switch,G1(:,:,:,79),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,65))
  call loop_GGG_G_23(G1(:,:,:,77),wf(:,-5),wf(:,-2),G1(:,:,:,80))
  call check_last_UV_W(l_switch,G1(:,:,:,80),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,66))
  call loop_UV_W(G1(:,:,:,77),Q(:,26),wf(:,70),Q(:,36),G2(:,:,:,37))
  call check_last_UV_W(l_switch,G2(:,:,:,37),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,13))
  call loop_UV_W(G1(:,:,:,77),Q(:,26),wf(:,-5),Q(:,32),G2(:,:,:,38))
  call loop_UV_W(G2(:,:,:,38),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,25))
  call check_last_UV_W(l_switch,G3(:,:,:,25),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,25))
  call loop_UV_W(G1(:,:,:,77),Q(:,26),wf(:,-2),Q(:,4),G2(:,:,:,39))
  call loop_UV_W(G2(:,:,:,39),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,26))
  call check_last_UV_W(l_switch,G3(:,:,:,26),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,26))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,-4),G1(:,:,:,81))
  call loop_GGG_G_12(G1(:,:,:,81),wf(:,-5),wf(:,-2),G1(:,:,:,82))
  call check_last_UV_W(l_switch,G1(:,:,:,82),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,67))
  call loop_GGG_G_12(G1(:,:,:,81),wf(:,-2),wf(:,-5),G1(:,:,:,83))
  call check_last_UV_W(l_switch,G1(:,:,:,83),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,68))
  call loop_GGG_G_23(G1(:,:,:,81),wf(:,-5),wf(:,-2),G1(:,:,:,84))
  call check_last_UV_W(l_switch,G1(:,:,:,84),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,69))
  call loop_UV_W(G1(:,:,:,81),Q(:,26),wf(:,70),Q(:,36),G2(:,:,:,40))
  call check_last_UV_W(l_switch,G2(:,:,:,40),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,14))
  call loop_UV_W(G1(:,:,:,81),Q(:,26),wf(:,-5),Q(:,32),G2(:,:,:,41))
  call loop_UV_W(G2(:,:,:,41),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,27))
  call check_last_UV_W(l_switch,G3(:,:,:,27),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,27))
  call loop_UV_W(G1(:,:,:,81),Q(:,26),wf(:,-2),Q(:,4),G2(:,:,:,42))
  call loop_UV_W(G2(:,:,:,42),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,28))
  call check_last_UV_W(l_switch,G3(:,:,:,28),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,28))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-4),wf(:,-3),G1(:,:,:,85))
  call loop_GGG_G_12(G1(:,:,:,85),wf(:,-5),wf(:,-2),G1(:,:,:,86))
  call check_last_UV_W(l_switch,G1(:,:,:,86),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,70))
  call loop_GGG_G_12(G1(:,:,:,85),wf(:,-2),wf(:,-5),G1(:,:,:,87))
  call check_last_UV_W(l_switch,G1(:,:,:,87),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,71))
  call loop_GGG_G_23(G1(:,:,:,85),wf(:,-5),wf(:,-2),G1(:,:,:,88))
  call check_last_UV_W(l_switch,G1(:,:,:,88),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,72))
  call loop_UV_W(G1(:,:,:,85),Q(:,26),wf(:,70),Q(:,36),G2(:,:,:,43))
  call check_last_UV_W(l_switch,G2(:,:,:,43),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,15))
  call loop_UV_W(G1(:,:,:,85),Q(:,26),wf(:,-5),Q(:,32),G2(:,:,:,44))
  call loop_UV_W(G2(:,:,:,44),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,29))
  call check_last_UV_W(l_switch,G3(:,:,:,29),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,29))
  call loop_UV_W(G1(:,:,:,85),Q(:,26),wf(:,-2),Q(:,4),G2(:,:,:,45))
  call loop_UV_W(G2(:,:,:,45),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,30))
  call check_last_UV_W(l_switch,G3(:,:,:,30),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,30))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,-2),G1(:,:,:,89))
  call loop_GGG_G_12(G1(:,:,:,89),wf(:,-4),wf(:,-3),G1(:,:,:,90))
  call check_last_UV_W(l_switch,G1(:,:,:,90),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,73))
  call loop_GGG_G_12(G1(:,:,:,89),wf(:,-3),wf(:,-4),G1(:,:,:,91))
  call check_last_UV_W(l_switch,G1(:,:,:,91),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,74))
  call loop_GGG_G_23(G1(:,:,:,89),wf(:,-4),wf(:,-3),G1(:,:,:,92))
  call check_last_UV_W(l_switch,G1(:,:,:,92),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,75))
  call loop_UV_W(G1(:,:,:,89),Q(:,38),wf(:,75),Q(:,24),G2(:,:,:,46))
  call check_last_UV_W(l_switch,G2(:,:,:,46),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,16))
  call loop_UV_W(G1(:,:,:,89),Q(:,38),wf(:,-4),Q(:,16),G2(:,:,:,47))
  call loop_UV_W(G2(:,:,:,47),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,31))
  call check_last_UV_W(l_switch,G3(:,:,:,31),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,31))
  call loop_UV_W(G1(:,:,:,89),Q(:,38),wf(:,-3),Q(:,8),G2(:,:,:,48))
  call loop_UV_W(G2(:,:,:,48),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,32))
  call check_last_UV_W(l_switch,G3(:,:,:,32),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,32))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,-5),G1(:,:,:,93))
  call loop_GGG_G_12(G1(:,:,:,93),wf(:,-4),wf(:,-3),G1(:,:,:,94))
  call check_last_UV_W(l_switch,G1(:,:,:,94),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,76))
  call loop_GGG_G_12(G1(:,:,:,93),wf(:,-3),wf(:,-4),G1(:,:,:,95))
  call check_last_UV_W(l_switch,G1(:,:,:,95),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,77))
  call loop_GGG_G_23(G1(:,:,:,93),wf(:,-4),wf(:,-3),G1(:,:,:,96))
  call check_last_UV_W(l_switch,G1(:,:,:,96),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,78))
  call loop_UV_W(G1(:,:,:,93),Q(:,38),wf(:,75),Q(:,24),G2(:,:,:,49))
  call check_last_UV_W(l_switch,G2(:,:,:,49),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,17))
  call loop_UV_W(G1(:,:,:,93),Q(:,38),wf(:,-4),Q(:,16),G2(:,:,:,50))
  call loop_UV_W(G2(:,:,:,50),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,33))
  call check_last_UV_W(l_switch,G3(:,:,:,33),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,33))
  call loop_UV_W(G1(:,:,:,93),Q(:,38),wf(:,-3),Q(:,8),G2(:,:,:,51))
  call loop_UV_W(G2(:,:,:,51),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,34))
  call check_last_UV_W(l_switch,G3(:,:,:,34),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,34))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,-2),G1(:,:,:,97))
  call loop_GGG_G_12(G1(:,:,:,97),wf(:,-4),wf(:,-3),G1(:,:,:,98))
  call check_last_UV_W(l_switch,G1(:,:,:,98),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,79))
  call loop_GGG_G_12(G1(:,:,:,97),wf(:,-3),wf(:,-4),G1(:,:,:,99))
  call check_last_UV_W(l_switch,G1(:,:,:,99),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,80))
  call loop_GGG_G_23(G1(:,:,:,97),wf(:,-4),wf(:,-3),G1(:,:,:,100))
  call check_last_UV_W(l_switch,G1(:,:,:,100),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,81))
  call loop_UV_W(G1(:,:,:,97),Q(:,38),wf(:,75),Q(:,24),G2(:,:,:,52))
  call check_last_UV_W(l_switch,G2(:,:,:,52),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,18))
  call loop_UV_W(G1(:,:,:,97),Q(:,38),wf(:,-4),Q(:,16),G2(:,:,:,53))
  call loop_UV_W(G2(:,:,:,53),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,35))
  call check_last_UV_W(l_switch,G3(:,:,:,35),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,35))
  call loop_UV_W(G1(:,:,:,97),Q(:,38),wf(:,-3),Q(:,8),G2(:,:,:,54))
  call loop_UV_W(G2(:,:,:,54),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,36))
  call check_last_UV_W(l_switch,G3(:,:,:,36),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,36))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,2),G1(:,:,:,101))
  call check_last_UV_W(l_switch,G1(:,:,:,101),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,82))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,2),wf(:,-2),G1(:,:,:,102))
  call check_last_UV_W(l_switch,G1(:,:,:,102),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,83))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,2),G1(:,:,:,103))
  call check_last_UV_W(l_switch,G1(:,:,:,103),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,84))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,5),G1(:,:,:,104))
  call check_last_UV_W(l_switch,G1(:,:,:,104),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,85))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,5),wf(:,-2),G1(:,:,:,105))
  call check_last_UV_W(l_switch,G1(:,:,:,105),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,86))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,5),G1(:,:,:,106))
  call check_last_UV_W(l_switch,G1(:,:,:,106),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,87))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,6),G1(:,:,:,107))
  call check_last_UV_W(l_switch,G1(:,:,:,107),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,88))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,6),wf(:,-2),G1(:,:,:,108))
  call check_last_UV_W(l_switch,G1(:,:,:,108),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,89))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,6),G1(:,:,:,109))
  call check_last_UV_W(l_switch,G1(:,:,:,109),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,90))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,62),wf(:,84),G1(:,:,:,110))
  call check_last_UV_W(l_switch,G1(:,:,:,110),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,91))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,84),wf(:,62),G1(:,:,:,111))
  call check_last_UV_W(l_switch,G1(:,:,:,111),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,92))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,62),wf(:,84),G1(:,:,:,112))
  call check_last_UV_W(l_switch,G1(:,:,:,112),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,93))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,253),G1(:,:,:,113))
  call check_last_UV_W(l_switch,G1(:,:,:,113),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,94))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,253),wf(:,-5),G1(:,:,:,114))
  call check_last_UV_W(l_switch,G1(:,:,:,114),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,95))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,253),G1(:,:,:,115))
  call check_last_UV_W(l_switch,G1(:,:,:,115),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,96))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,62),G1(:,:,:,116))
  call loop_UV_W(G1(:,:,:,116),Q(:,46),wf(:,-4),Q(:,16),G2(:,:,:,55))
  call check_last_UV_W(l_switch,G2(:,:,:,55),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,19))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,62),wf(:,-5),G1(:,:,:,117))
  call loop_UV_W(G1(:,:,:,117),Q(:,46),wf(:,-4),Q(:,16),G2(:,:,:,56))
  call check_last_UV_W(l_switch,G2(:,:,:,56),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,20))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,62),G1(:,:,:,118))
  call loop_UV_W(G1(:,:,:,118),Q(:,46),wf(:,-4),Q(:,16),G2(:,:,:,57))
  call check_last_UV_W(l_switch,G2(:,:,:,57),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,21))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,-4),Q(:,16),G2(:,:,:,58))
  call loop_GGG_G_12(G2(:,:,:,58),wf(:,-5),wf(:,62),G2(:,:,:,59))
  call check_last_UV_W(l_switch,G2(:,:,:,59),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,22))
  call loop_GGG_G_12(G2(:,:,:,58),wf(:,62),wf(:,-5),G2(:,:,:,60))
  call check_last_UV_W(l_switch,G2(:,:,:,60),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,23))
  call loop_GGG_G_23(G2(:,:,:,58),wf(:,-5),wf(:,62),G2(:,:,:,61))
  call check_last_UV_W(l_switch,G2(:,:,:,61),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,24))
  call loop_GGG_G_12(G2(:,:,:,58),wf(:,-3),wf(:,70),G2(:,:,:,62))
  call check_last_UV_W(l_switch,G2(:,:,:,62),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,25))
  call loop_GGG_G_12(G2(:,:,:,58),wf(:,70),wf(:,-3),G2(:,:,:,63))
  call check_last_UV_W(l_switch,G2(:,:,:,63),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,26))
  call loop_GGG_G_23(G2(:,:,:,58),wf(:,-3),wf(:,70),G2(:,:,:,64))
  call check_last_UV_W(l_switch,G2(:,:,:,64),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,27))
  call loop_GGG_G_12(G2(:,:,:,58),wf(:,-5),wf(:,-3),G2(:,:,:,65))
  call loop_UV_W(G2(:,:,:,65),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,37))
  call check_last_UV_W(l_switch,G3(:,:,:,37),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,37))
  call loop_GGG_G_12(G2(:,:,:,58),wf(:,-3),wf(:,-5),G2(:,:,:,66))
  call loop_UV_W(G2(:,:,:,66),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,38))
  call check_last_UV_W(l_switch,G3(:,:,:,38),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,38))
  call loop_GGG_G_23(G2(:,:,:,58),wf(:,-5),wf(:,-3),G2(:,:,:,67))
  call loop_UV_W(G2(:,:,:,67),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,39))
  call check_last_UV_W(l_switch,G3(:,:,:,39),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,39))
  call loop_UV_W(G2(:,:,:,58),Q(:,18),wf(:,-2),Q(:,4),G3(:,:,:,40))
  call loop_GGG_G_12(G3(:,:,:,40),wf(:,-5),wf(:,-3),G3(:,:,:,41))
  call check_last_UV_W(l_switch,G3(:,:,:,41),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,40))
  call loop_GGG_G_12(G3(:,:,:,40),wf(:,-3),wf(:,-5),G3(:,:,:,42))
  call check_last_UV_W(l_switch,G3(:,:,:,42),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,41))
  call loop_GGG_G_23(G3(:,:,:,40),wf(:,-5),wf(:,-3),G3(:,:,:,43))
  call check_last_UV_W(l_switch,G3(:,:,:,43),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,42))
  call loop_UV_W(G3(:,:,:,40),Q(:,22),wf(:,79),Q(:,40),G4(:,:,:,1))
  call check_last_UV_W(l_switch,G4(:,:,:,1),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,1))
  call loop_UV_W(G3(:,:,:,40),Q(:,22),wf(:,-5),Q(:,32),G4(:,:,:,2))
  call loop_UV_W(G4(:,:,:,2),Q(:,54),wf(:,-3),Q(:,8),G5(:,:,:,1))
  call check_last_UV_W(l_switch,G5(:,:,:,1),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,1))
  call loop_UV_W(G3(:,:,:,40),Q(:,22),wf(:,-3),Q(:,8),G4(:,:,:,3))
  call loop_UV_W(G4(:,:,:,3),Q(:,30),wf(:,-5),Q(:,32),G5(:,:,:,2))
  call check_last_UV_W(l_switch,G5(:,:,:,2),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,2))
  call loop_GGG_G_12(G2(:,:,:,58),wf(:,-2),wf(:,79),G2(:,:,:,68))
  call check_last_UV_W(l_switch,G2(:,:,:,68),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,28))
  call loop_GGG_G_12(G2(:,:,:,58),wf(:,79),wf(:,-2),G2(:,:,:,69))
  call check_last_UV_W(l_switch,G2(:,:,:,69),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,29))
  call loop_GGG_G_23(G2(:,:,:,58),wf(:,-2),wf(:,79),G2(:,:,:,70))
  call check_last_UV_W(l_switch,G2(:,:,:,70),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,30))
  call loop_GGG_G_12(G2(:,:,:,58),wf(:,-5),wf(:,-2),G2(:,:,:,71))
  call loop_UV_W(G2(:,:,:,71),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,44))
  call check_last_UV_W(l_switch,G3(:,:,:,44),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,43))
  call loop_GGG_G_12(G2(:,:,:,58),wf(:,-2),wf(:,-5),G2(:,:,:,72))
  call loop_UV_W(G2(:,:,:,72),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,45))
  call check_last_UV_W(l_switch,G3(:,:,:,45),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,44))
  call loop_GGG_G_23(G2(:,:,:,58),wf(:,-5),wf(:,-2),G2(:,:,:,73))
  call loop_UV_W(G2(:,:,:,73),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,46))
  call check_last_UV_W(l_switch,G3(:,:,:,46),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,45))
  call loop_UV_W(G2(:,:,:,58),Q(:,18),wf(:,-3),Q(:,8),G3(:,:,:,47))
  call loop_GGG_G_12(G3(:,:,:,47),wf(:,-5),wf(:,-2),G3(:,:,:,48))
  call check_last_UV_W(l_switch,G3(:,:,:,48),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,46))
  call loop_GGG_G_12(G3(:,:,:,47),wf(:,-2),wf(:,-5),G3(:,:,:,49))
  call check_last_UV_W(l_switch,G3(:,:,:,49),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,47))
  call loop_GGG_G_23(G3(:,:,:,47),wf(:,-5),wf(:,-2),G3(:,:,:,50))
  call check_last_UV_W(l_switch,G3(:,:,:,50),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,48))
  call loop_UV_W(G3(:,:,:,47),Q(:,26),wf(:,70),Q(:,36),G4(:,:,:,4))
  call check_last_UV_W(l_switch,G4(:,:,:,4),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,2))
  call loop_UV_W(G3(:,:,:,47),Q(:,26),wf(:,-5),Q(:,32),G4(:,:,:,5))
  call loop_UV_W(G4(:,:,:,5),Q(:,58),wf(:,-2),Q(:,4),G5(:,:,:,3))
  call check_last_UV_W(l_switch,G5(:,:,:,3),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,3))
  call loop_UV_W(G3(:,:,:,47),Q(:,26),wf(:,-2),Q(:,4),G4(:,:,:,6))
  call loop_UV_W(G4(:,:,:,6),Q(:,30),wf(:,-5),Q(:,32),G5(:,:,:,4))
  call check_last_UV_W(l_switch,G5(:,:,:,4),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,4))
  call loop_GGG_G_12(G2(:,:,:,58),wf(:,-3),wf(:,-2),G2(:,:,:,74))
  call loop_UV_W(G2(:,:,:,74),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,51))
  call check_last_UV_W(l_switch,G3(:,:,:,51),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,49))
  call loop_GGG_G_12(G2(:,:,:,58),wf(:,-2),wf(:,-3),G2(:,:,:,75))
  call loop_UV_W(G2(:,:,:,75),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,52))
  call check_last_UV_W(l_switch,G3(:,:,:,52),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,50))
  call loop_GGG_G_23(G2(:,:,:,58),wf(:,-3),wf(:,-2),G2(:,:,:,76))
  call loop_UV_W(G2(:,:,:,76),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,53))
  call check_last_UV_W(l_switch,G3(:,:,:,53),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,51))
  call loop_UV_W(G2(:,:,:,58),Q(:,18),wf(:,-5),Q(:,32),G3(:,:,:,54))
  call loop_GGG_G_12(G3(:,:,:,54),wf(:,-3),wf(:,-2),G3(:,:,:,55))
  call check_last_UV_W(l_switch,G3(:,:,:,55),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,52))
  call loop_GGG_G_12(G3(:,:,:,54),wf(:,-2),wf(:,-3),G3(:,:,:,56))
  call check_last_UV_W(l_switch,G3(:,:,:,56),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,53))
  call loop_GGG_G_23(G3(:,:,:,54),wf(:,-3),wf(:,-2),G3(:,:,:,57))
  call check_last_UV_W(l_switch,G3(:,:,:,57),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,54))
  call loop_UV_W(G3(:,:,:,54),Q(:,50),wf(:,62),Q(:,12),G4(:,:,:,7))
  call check_last_UV_W(l_switch,G4(:,:,:,7),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,3))
  call loop_UV_W(G3(:,:,:,54),Q(:,50),wf(:,-3),Q(:,8),G4(:,:,:,8))
  call loop_UV_W(G4(:,:,:,8),Q(:,58),wf(:,-2),Q(:,4),G5(:,:,:,5))
  call check_last_UV_W(l_switch,G5(:,:,:,5),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,5))
  call loop_UV_W(G3(:,:,:,54),Q(:,50),wf(:,-2),Q(:,4),G4(:,:,:,9))
  call loop_UV_W(G4(:,:,:,9),Q(:,54),wf(:,-3),Q(:,8),G5(:,:,:,6))
  call check_last_UV_W(l_switch,G5(:,:,:,6),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,6))
  call loop_UV_W(G2(:,:,:,58),Q(:,18),wf(:,14),Q(:,44),G3(:,:,:,58))
  call check_last_UV_W(l_switch,G3(:,:,:,58),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,55))
  call loop_UV_W(G2(:,:,:,58),Q(:,18),wf(:,17),Q(:,44),G3(:,:,:,59))
  call check_last_UV_W(l_switch,G3(:,:,:,59),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,56))
  call loop_UV_W(G2(:,:,:,58),Q(:,18),wf(:,18),Q(:,44),G3(:,:,:,60))
  call check_last_UV_W(l_switch,G3(:,:,:,60),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,57))
  call loop_UV_W(G2(:,:,:,58),Q(:,18),wf(:,254),Q(:,44),G3(:,:,:,61))
  call check_last_UV_W(l_switch,G3(:,:,:,61),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,58))
  call loop_UV_W(G2(:,:,:,58),Q(:,18),wf(:,62),Q(:,12),G3(:,:,:,62))
  call loop_UV_W(G3(:,:,:,62),Q(:,30),wf(:,-5),Q(:,32),G4(:,:,:,10))
  call check_last_UV_W(l_switch,G4(:,:,:,10),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,4))
  call loop_UV_W(G2(:,:,:,58),Q(:,18),wf(:,263),Q(:,44),G3(:,:,:,63))
  call check_last_UV_W(l_switch,G3(:,:,:,63),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,59))
  call loop_UV_W(G2(:,:,:,58),Q(:,18),wf(:,70),Q(:,36),G3(:,:,:,64))
  call loop_UV_W(G3(:,:,:,64),Q(:,54),wf(:,-3),Q(:,8),G4(:,:,:,11))
  call check_last_UV_W(l_switch,G4(:,:,:,11),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,5))
  call loop_UV_W(G2(:,:,:,58),Q(:,18),wf(:,264),Q(:,44),G3(:,:,:,65))
  call check_last_UV_W(l_switch,G3(:,:,:,65),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,60))
  call loop_UV_W(G2(:,:,:,58),Q(:,18),wf(:,79),Q(:,40),G3(:,:,:,66))
  call loop_UV_W(G3(:,:,:,66),Q(:,58),wf(:,-2),Q(:,4),G4(:,:,:,12))
  call check_last_UV_W(l_switch,G4(:,:,:,12),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,6))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-4),wf(:,254),G1(:,:,:,119))
  call check_last_UV_W(l_switch,G1(:,:,:,119),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,97))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,254),wf(:,-4),G1(:,:,:,120))
  call check_last_UV_W(l_switch,G1(:,:,:,120),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,98))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-4),wf(:,254),G1(:,:,:,121))
  call check_last_UV_W(l_switch,G1(:,:,:,121),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,99))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-4),wf(:,62),G1(:,:,:,122))
  call loop_UV_W(G1(:,:,:,122),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,77))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,31))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,62),wf(:,-4),G1(:,:,:,123))
  call loop_UV_W(G1(:,:,:,123),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,78))
  call check_last_UV_W(l_switch,G2(:,:,:,78),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,32))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-4),wf(:,62),G1(:,:,:,124))
  call loop_UV_W(G1(:,:,:,124),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,79))
  call check_last_UV_W(l_switch,G2(:,:,:,79),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,33))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,-5),Q(:,32),G2(:,:,:,80))
  call loop_GGG_G_12(G2(:,:,:,80),wf(:,-4),wf(:,62),G2(:,:,:,81))
  call check_last_UV_W(l_switch,G2(:,:,:,81),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,34))
  call loop_GGG_G_12(G2(:,:,:,80),wf(:,62),wf(:,-4),G2(:,:,:,82))
  call check_last_UV_W(l_switch,G2(:,:,:,82),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,35))
  call loop_GGG_G_23(G2(:,:,:,80),wf(:,-4),wf(:,62),G2(:,:,:,83))
  call check_last_UV_W(l_switch,G2(:,:,:,83),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,36))
  call loop_GGG_G_12(G2(:,:,:,80),wf(:,-3),wf(:,66),G2(:,:,:,84))
  call check_last_UV_W(l_switch,G2(:,:,:,84),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,37))
  call loop_GGG_G_12(G2(:,:,:,80),wf(:,66),wf(:,-3),G2(:,:,:,85))
  call check_last_UV_W(l_switch,G2(:,:,:,85),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,38))
  call loop_GGG_G_23(G2(:,:,:,80),wf(:,-3),wf(:,66),G2(:,:,:,86))
  call check_last_UV_W(l_switch,G2(:,:,:,86),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,39))
  call loop_GGG_G_12(G2(:,:,:,80),wf(:,-4),wf(:,-3),G2(:,:,:,87))
  call loop_UV_W(G2(:,:,:,87),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,67))
  call check_last_UV_W(l_switch,G3(:,:,:,67),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,61))
  call loop_GGG_G_12(G2(:,:,:,80),wf(:,-3),wf(:,-4),G2(:,:,:,88))
  call loop_UV_W(G2(:,:,:,88),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,68))
  call check_last_UV_W(l_switch,G3(:,:,:,68),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,62))
  call loop_GGG_G_23(G2(:,:,:,80),wf(:,-4),wf(:,-3),G2(:,:,:,89))
  call loop_UV_W(G2(:,:,:,89),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,69))
  call check_last_UV_W(l_switch,G3(:,:,:,69),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,63))
  call loop_UV_W(G2(:,:,:,80),Q(:,34),wf(:,-2),Q(:,4),G3(:,:,:,70))
  call loop_GGG_G_12(G3(:,:,:,70),wf(:,-4),wf(:,-3),G3(:,:,:,71))
  call check_last_UV_W(l_switch,G3(:,:,:,71),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,64))
  call loop_GGG_G_12(G3(:,:,:,70),wf(:,-3),wf(:,-4),G3(:,:,:,72))
  call check_last_UV_W(l_switch,G3(:,:,:,72),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,65))
  call loop_GGG_G_23(G3(:,:,:,70),wf(:,-4),wf(:,-3),G3(:,:,:,73))
  call check_last_UV_W(l_switch,G3(:,:,:,73),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,66))
  call loop_UV_W(G3(:,:,:,70),Q(:,38),wf(:,75),Q(:,24),G4(:,:,:,13))
  call check_last_UV_W(l_switch,G4(:,:,:,13),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,7))
  call loop_UV_W(G3(:,:,:,70),Q(:,38),wf(:,-4),Q(:,16),G4(:,:,:,14))
  call loop_UV_W(G4(:,:,:,14),Q(:,54),wf(:,-3),Q(:,8),G5(:,:,:,7))
  call check_last_UV_W(l_switch,G5(:,:,:,7),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,7))
  call loop_UV_W(G3(:,:,:,70),Q(:,38),wf(:,-3),Q(:,8),G4(:,:,:,15))
  call loop_UV_W(G4(:,:,:,15),Q(:,46),wf(:,-4),Q(:,16),G5(:,:,:,8))
  call check_last_UV_W(l_switch,G5(:,:,:,8),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,8))
  call loop_GGG_G_12(G2(:,:,:,80),wf(:,-2),wf(:,75),G2(:,:,:,90))
  call check_last_UV_W(l_switch,G2(:,:,:,90),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,40))
  call loop_GGG_G_12(G2(:,:,:,80),wf(:,75),wf(:,-2),G2(:,:,:,91))
  call check_last_UV_W(l_switch,G2(:,:,:,91),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,41))
  call loop_GGG_G_23(G2(:,:,:,80),wf(:,-2),wf(:,75),G2(:,:,:,92))
  call check_last_UV_W(l_switch,G2(:,:,:,92),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,42))
  call loop_GGG_G_12(G2(:,:,:,80),wf(:,-4),wf(:,-2),G2(:,:,:,93))
  call loop_UV_W(G2(:,:,:,93),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,74))
  call check_last_UV_W(l_switch,G3(:,:,:,74),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,67))
  call loop_GGG_G_12(G2(:,:,:,80),wf(:,-2),wf(:,-4),G2(:,:,:,94))
  call loop_UV_W(G2(:,:,:,94),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,75))
  call check_last_UV_W(l_switch,G3(:,:,:,75),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,68))
  call loop_GGG_G_23(G2(:,:,:,80),wf(:,-4),wf(:,-2),G2(:,:,:,95))
  call loop_UV_W(G2(:,:,:,95),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,76))
  call check_last_UV_W(l_switch,G3(:,:,:,76),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,69))
  call loop_UV_W(G2(:,:,:,80),Q(:,34),wf(:,-3),Q(:,8),G3(:,:,:,77))
  call loop_GGG_G_12(G3(:,:,:,77),wf(:,-4),wf(:,-2),G3(:,:,:,78))
  call check_last_UV_W(l_switch,G3(:,:,:,78),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,70))
  call loop_GGG_G_12(G3(:,:,:,77),wf(:,-2),wf(:,-4),G3(:,:,:,79))
  call check_last_UV_W(l_switch,G3(:,:,:,79),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,71))
  call loop_GGG_G_23(G3(:,:,:,77),wf(:,-4),wf(:,-2),G3(:,:,:,80))
  call check_last_UV_W(l_switch,G3(:,:,:,80),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,72))
  call loop_UV_W(G3(:,:,:,77),Q(:,42),wf(:,66),Q(:,20),G4(:,:,:,16))
  call check_last_UV_W(l_switch,G4(:,:,:,16),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,8))
  call loop_UV_W(G3(:,:,:,77),Q(:,42),wf(:,-4),Q(:,16),G4(:,:,:,17))
  call loop_UV_W(G4(:,:,:,17),Q(:,58),wf(:,-2),Q(:,4),G5(:,:,:,9))
  call check_last_UV_W(l_switch,G5(:,:,:,9),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,9))
  call loop_UV_W(G3(:,:,:,77),Q(:,42),wf(:,-2),Q(:,4),G4(:,:,:,18))
  call loop_UV_W(G4(:,:,:,18),Q(:,46),wf(:,-4),Q(:,16),G5(:,:,:,10))
  call check_last_UV_W(l_switch,G5(:,:,:,10),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,10))
  call loop_GGG_G_12(G2(:,:,:,80),wf(:,-3),wf(:,-2),G2(:,:,:,96))
  call loop_UV_W(G2(:,:,:,96),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,81))
  call check_last_UV_W(l_switch,G3(:,:,:,81),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,73))
  call loop_GGG_G_12(G2(:,:,:,80),wf(:,-2),wf(:,-3),G2(:,:,:,97))
  call loop_UV_W(G2(:,:,:,97),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,82))
  call check_last_UV_W(l_switch,G3(:,:,:,82),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,74))
  call loop_GGG_G_23(G2(:,:,:,80),wf(:,-3),wf(:,-2),G2(:,:,:,98))
  call loop_UV_W(G2(:,:,:,98),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,83))
  call check_last_UV_W(l_switch,G3(:,:,:,83),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,75))
  call loop_UV_W(G2(:,:,:,80),Q(:,34),wf(:,-4),Q(:,16),G3(:,:,:,84))
  call loop_GGG_G_12(G3(:,:,:,84),wf(:,-3),wf(:,-2),G3(:,:,:,85))
  call check_last_UV_W(l_switch,G3(:,:,:,85),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,76))
  call loop_GGG_G_12(G3(:,:,:,84),wf(:,-2),wf(:,-3),G3(:,:,:,86))
  call check_last_UV_W(l_switch,G3(:,:,:,86),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,77))
  call loop_GGG_G_23(G3(:,:,:,84),wf(:,-3),wf(:,-2),G3(:,:,:,87))
  call check_last_UV_W(l_switch,G3(:,:,:,87),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,78))
  call loop_UV_W(G3(:,:,:,84),Q(:,50),wf(:,62),Q(:,12),G4(:,:,:,19))
  call check_last_UV_W(l_switch,G4(:,:,:,19),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,9))
  call loop_UV_W(G3(:,:,:,84),Q(:,50),wf(:,-3),Q(:,8),G4(:,:,:,20))
  call loop_UV_W(G4(:,:,:,20),Q(:,58),wf(:,-2),Q(:,4),G5(:,:,:,11))
  call check_last_UV_W(l_switch,G5(:,:,:,11),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,11))
  call loop_UV_W(G3(:,:,:,84),Q(:,50),wf(:,-2),Q(:,4),G4(:,:,:,21))
  call loop_UV_W(G4(:,:,:,21),Q(:,54),wf(:,-3),Q(:,8),G5(:,:,:,12))
  call check_last_UV_W(l_switch,G5(:,:,:,12),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,12))
  call loop_UV_W(G2(:,:,:,80),Q(:,34),wf(:,20),Q(:,28),G3(:,:,:,88))
  call check_last_UV_W(l_switch,G3(:,:,:,88),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,79))
  call loop_UV_W(G2(:,:,:,80),Q(:,34),wf(:,23),Q(:,28),G3(:,:,:,89))
  call check_last_UV_W(l_switch,G3(:,:,:,89),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,80))
  call loop_UV_W(G2(:,:,:,80),Q(:,34),wf(:,24),Q(:,28),G3(:,:,:,90))
  call check_last_UV_W(l_switch,G3(:,:,:,90),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,81))
  call loop_UV_W(G2(:,:,:,80),Q(:,34),wf(:,253),Q(:,28),G3(:,:,:,91))
  call check_last_UV_W(l_switch,G3(:,:,:,91),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,82))
  call loop_UV_W(G2(:,:,:,80),Q(:,34),wf(:,62),Q(:,12),G3(:,:,:,92))
  call loop_UV_W(G3(:,:,:,92),Q(:,46),wf(:,-4),Q(:,16),G4(:,:,:,22))
  call check_last_UV_W(l_switch,G4(:,:,:,22),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,10))
  call loop_UV_W(G2(:,:,:,80),Q(:,34),wf(:,258),Q(:,28),G3(:,:,:,93))
  call check_last_UV_W(l_switch,G3(:,:,:,93),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,83))
  call loop_UV_W(G2(:,:,:,80),Q(:,34),wf(:,66),Q(:,20),G3(:,:,:,94))
  call loop_UV_W(G3(:,:,:,94),Q(:,54),wf(:,-3),Q(:,8),G4(:,:,:,23))
  call check_last_UV_W(l_switch,G4(:,:,:,23),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,11))
  call loop_UV_W(G2(:,:,:,80),Q(:,34),wf(:,262),Q(:,28),G3(:,:,:,95))
  call check_last_UV_W(l_switch,G3(:,:,:,95),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,84))
  call loop_UV_W(G2(:,:,:,80),Q(:,34),wf(:,75),Q(:,24),G3(:,:,:,96))
  call loop_UV_W(G3(:,:,:,96),Q(:,58),wf(:,-2),Q(:,4),G4(:,:,:,24))
  call check_last_UV_W(l_switch,G4(:,:,:,24),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,12))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,62),Q(:,12),G2(:,:,:,99))
  call loop_GGG_G_12(G2(:,:,:,99),wf(:,-5),wf(:,-4),G2(:,:,:,100))
  call check_last_UV_W(l_switch,G2(:,:,:,100),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,43))
  call loop_GGG_G_12(G2(:,:,:,99),wf(:,-4),wf(:,-5),G2(:,:,:,101))
  call check_last_UV_W(l_switch,G2(:,:,:,101),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,44))
  call loop_GGG_G_23(G2(:,:,:,99),wf(:,-5),wf(:,-4),G2(:,:,:,102))
  call check_last_UV_W(l_switch,G2(:,:,:,102),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,45))
  call loop_UV_W(G2(:,:,:,99),Q(:,14),wf(:,84),Q(:,48),G3(:,:,:,97))
  call check_last_UV_W(l_switch,G3(:,:,:,97),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,85))
  call loop_UV_W(G2(:,:,:,99),Q(:,14),wf(:,-5),Q(:,32),G3(:,:,:,98))
  call loop_UV_W(G3(:,:,:,98),Q(:,46),wf(:,-4),Q(:,16),G4(:,:,:,25))
  call check_last_UV_W(l_switch,G4(:,:,:,25),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,13))
  call loop_UV_W(G2(:,:,:,99),Q(:,14),wf(:,-4),Q(:,16),G3(:,:,:,99))
  call loop_UV_W(G3(:,:,:,99),Q(:,30),wf(:,-5),Q(:,32),G4(:,:,:,26))
  call check_last_UV_W(l_switch,G4(:,:,:,26),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,14))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,782),Q(:,60),G2(:,:,:,103))
  call check_last_UV_W(l_switch,G2(:,:,:,103),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,46))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,784),Q(:,60),G2(:,:,:,104))
  call check_last_UV_W(l_switch,G2(:,:,:,104),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,47))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,785),Q(:,60),G2(:,:,:,105))
  call check_last_UV_W(l_switch,G2(:,:,:,105),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,48))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,66),wf(:,79),G1(:,:,:,125))
  call check_last_UV_W(l_switch,G1(:,:,:,125),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,100))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,79),wf(:,66),G1(:,:,:,126))
  call check_last_UV_W(l_switch,G1(:,:,:,126),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,101))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,66),wf(:,79),G1(:,:,:,127))
  call check_last_UV_W(l_switch,G1(:,:,:,127),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,102))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,258),G1(:,:,:,128))
  call check_last_UV_W(l_switch,G1(:,:,:,128),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,103))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,258),wf(:,-5),G1(:,:,:,129))
  call check_last_UV_W(l_switch,G1(:,:,:,129),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,104))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,258),G1(:,:,:,130))
  call check_last_UV_W(l_switch,G1(:,:,:,130),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,105))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,66),G1(:,:,:,131))
  call loop_UV_W(G1(:,:,:,131),Q(:,54),wf(:,-3),Q(:,8),G2(:,:,:,106))
  call check_last_UV_W(l_switch,G2(:,:,:,106),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,49))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,66),wf(:,-5),G1(:,:,:,132))
  call loop_UV_W(G1(:,:,:,132),Q(:,54),wf(:,-3),Q(:,8),G2(:,:,:,107))
  call check_last_UV_W(l_switch,G2(:,:,:,107),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,50))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,66),G1(:,:,:,133))
  call loop_UV_W(G1(:,:,:,133),Q(:,54),wf(:,-3),Q(:,8),G2(:,:,:,108))
  call check_last_UV_W(l_switch,G2(:,:,:,108),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,51))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,-3),Q(:,8),G2(:,:,:,109))
  call loop_GGG_G_12(G2(:,:,:,109),wf(:,-5),wf(:,66),G2(:,:,:,110))
  call check_last_UV_W(l_switch,G2(:,:,:,110),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,52))
  call loop_GGG_G_12(G2(:,:,:,109),wf(:,66),wf(:,-5),G2(:,:,:,111))
  call check_last_UV_W(l_switch,G2(:,:,:,111),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,53))
  call loop_GGG_G_23(G2(:,:,:,109),wf(:,-5),wf(:,66),G2(:,:,:,112))
  call check_last_UV_W(l_switch,G2(:,:,:,112),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,54))
  call loop_GGG_G_12(G2(:,:,:,109),wf(:,-4),wf(:,70),G2(:,:,:,113))
  call check_last_UV_W(l_switch,G2(:,:,:,113),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,55))
  call loop_GGG_G_12(G2(:,:,:,109),wf(:,70),wf(:,-4),G2(:,:,:,114))
  call check_last_UV_W(l_switch,G2(:,:,:,114),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,56))
  call loop_GGG_G_23(G2(:,:,:,109),wf(:,-4),wf(:,70),G2(:,:,:,115))
  call check_last_UV_W(l_switch,G2(:,:,:,115),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,57))
  call loop_GGG_G_12(G2(:,:,:,109),wf(:,-5),wf(:,-4),G2(:,:,:,116))
  call loop_UV_W(G2(:,:,:,116),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,100))
  call check_last_UV_W(l_switch,G3(:,:,:,100),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,86))
  call loop_GGG_G_12(G2(:,:,:,109),wf(:,-4),wf(:,-5),G2(:,:,:,117))
  call loop_UV_W(G2(:,:,:,117),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,101))
  call check_last_UV_W(l_switch,G3(:,:,:,101),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,87))
  call loop_GGG_G_23(G2(:,:,:,109),wf(:,-5),wf(:,-4),G2(:,:,:,118))
  call loop_UV_W(G2(:,:,:,118),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,102))
  call check_last_UV_W(l_switch,G3(:,:,:,102),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,88))
  call loop_UV_W(G2(:,:,:,109),Q(:,10),wf(:,-2),Q(:,4),G3(:,:,:,103))
  call loop_GGG_G_12(G3(:,:,:,103),wf(:,-5),wf(:,-4),G3(:,:,:,104))
  call check_last_UV_W(l_switch,G3(:,:,:,104),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,89))
  call loop_GGG_G_12(G3(:,:,:,103),wf(:,-4),wf(:,-5),G3(:,:,:,105))
  call check_last_UV_W(l_switch,G3(:,:,:,105),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,90))
  call loop_GGG_G_23(G3(:,:,:,103),wf(:,-5),wf(:,-4),G3(:,:,:,106))
  call check_last_UV_W(l_switch,G3(:,:,:,106),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,91))
  call loop_UV_W(G3(:,:,:,103),Q(:,14),wf(:,84),Q(:,48),G4(:,:,:,27))
  call check_last_UV_W(l_switch,G4(:,:,:,27),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,15))
  call loop_UV_W(G3(:,:,:,103),Q(:,14),wf(:,-5),Q(:,32),G4(:,:,:,28))
  call loop_UV_W(G4(:,:,:,28),Q(:,46),wf(:,-4),Q(:,16),G5(:,:,:,13))
  call check_last_UV_W(l_switch,G5(:,:,:,13),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,13))
  call loop_UV_W(G3(:,:,:,103),Q(:,14),wf(:,-4),Q(:,16),G4(:,:,:,29))
  call loop_UV_W(G4(:,:,:,29),Q(:,30),wf(:,-5),Q(:,32),G5(:,:,:,14))
  call check_last_UV_W(l_switch,G5(:,:,:,14),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,14))
  call loop_GGG_G_12(G2(:,:,:,109),wf(:,-2),wf(:,84),G2(:,:,:,119))
  call check_last_UV_W(l_switch,G2(:,:,:,119),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,58))
  call loop_GGG_G_12(G2(:,:,:,109),wf(:,84),wf(:,-2),G2(:,:,:,120))
  call check_last_UV_W(l_switch,G2(:,:,:,120),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,59))
  call loop_GGG_G_23(G2(:,:,:,109),wf(:,-2),wf(:,84),G2(:,:,:,121))
  call check_last_UV_W(l_switch,G2(:,:,:,121),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,60))
  call loop_GGG_G_12(G2(:,:,:,109),wf(:,-5),wf(:,-2),G2(:,:,:,122))
  call loop_UV_W(G2(:,:,:,122),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,107))
  call check_last_UV_W(l_switch,G3(:,:,:,107),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,92))
  call loop_GGG_G_12(G2(:,:,:,109),wf(:,-2),wf(:,-5),G2(:,:,:,123))
  call loop_UV_W(G2(:,:,:,123),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,108))
  call check_last_UV_W(l_switch,G3(:,:,:,108),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,93))
  call loop_GGG_G_23(G2(:,:,:,109),wf(:,-5),wf(:,-2),G2(:,:,:,124))
  call loop_UV_W(G2(:,:,:,124),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,109))
  call check_last_UV_W(l_switch,G3(:,:,:,109),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,94))
  call loop_UV_W(G2(:,:,:,109),Q(:,10),wf(:,-4),Q(:,16),G3(:,:,:,110))
  call loop_GGG_G_12(G3(:,:,:,110),wf(:,-5),wf(:,-2),G3(:,:,:,111))
  call check_last_UV_W(l_switch,G3(:,:,:,111),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,95))
  call loop_GGG_G_12(G3(:,:,:,110),wf(:,-2),wf(:,-5),G3(:,:,:,112))
  call check_last_UV_W(l_switch,G3(:,:,:,112),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,96))
  call loop_GGG_G_23(G3(:,:,:,110),wf(:,-5),wf(:,-2),G3(:,:,:,113))
  call check_last_UV_W(l_switch,G3(:,:,:,113),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,97))
  call loop_UV_W(G3(:,:,:,110),Q(:,26),wf(:,70),Q(:,36),G4(:,:,:,30))
  call check_last_UV_W(l_switch,G4(:,:,:,30),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,16))
  call loop_UV_W(G3(:,:,:,110),Q(:,26),wf(:,-5),Q(:,32),G4(:,:,:,31))
  call loop_UV_W(G4(:,:,:,31),Q(:,58),wf(:,-2),Q(:,4),G5(:,:,:,15))
  call check_last_UV_W(l_switch,G5(:,:,:,15),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,15))
  call loop_UV_W(G3(:,:,:,110),Q(:,26),wf(:,-2),Q(:,4),G4(:,:,:,32))
  call loop_UV_W(G4(:,:,:,32),Q(:,30),wf(:,-5),Q(:,32),G5(:,:,:,16))
  call check_last_UV_W(l_switch,G5(:,:,:,16),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,16))
  call loop_GGG_G_12(G2(:,:,:,109),wf(:,-4),wf(:,-2),G2(:,:,:,125))
  call loop_UV_W(G2(:,:,:,125),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,114))
  call check_last_UV_W(l_switch,G3(:,:,:,114),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,98))
  call loop_GGG_G_12(G2(:,:,:,109),wf(:,-2),wf(:,-4),G2(:,:,:,126))
  call loop_UV_W(G2(:,:,:,126),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,115))
  call check_last_UV_W(l_switch,G3(:,:,:,115),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,99))
  call loop_GGG_G_23(G2(:,:,:,109),wf(:,-4),wf(:,-2),G2(:,:,:,127))
  call loop_UV_W(G2(:,:,:,127),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,116))
  call check_last_UV_W(l_switch,G3(:,:,:,116),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,100))
  call loop_UV_W(G2(:,:,:,109),Q(:,10),wf(:,-5),Q(:,32),G3(:,:,:,117))
  call loop_GGG_G_12(G3(:,:,:,117),wf(:,-4),wf(:,-2),G3(:,:,:,118))
  call check_last_UV_W(l_switch,G3(:,:,:,118),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,101))
  call loop_GGG_G_12(G3(:,:,:,117),wf(:,-2),wf(:,-4),G3(:,:,:,119))
  call check_last_UV_W(l_switch,G3(:,:,:,119),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,102))
  call loop_GGG_G_23(G3(:,:,:,117),wf(:,-4),wf(:,-2),G3(:,:,:,120))
  call check_last_UV_W(l_switch,G3(:,:,:,120),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,103))
  call loop_UV_W(G3(:,:,:,117),Q(:,42),wf(:,66),Q(:,20),G4(:,:,:,33))
  call check_last_UV_W(l_switch,G4(:,:,:,33),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,17))
  call loop_UV_W(G3(:,:,:,117),Q(:,42),wf(:,-4),Q(:,16),G4(:,:,:,34))
  call loop_UV_W(G4(:,:,:,34),Q(:,58),wf(:,-2),Q(:,4),G5(:,:,:,17))
  call check_last_UV_W(l_switch,G5(:,:,:,17),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,17))
  call loop_UV_W(G3(:,:,:,117),Q(:,42),wf(:,-2),Q(:,4),G4(:,:,:,35))
  call loop_UV_W(G4(:,:,:,35),Q(:,46),wf(:,-4),Q(:,16),G5(:,:,:,18))
  call check_last_UV_W(l_switch,G5(:,:,:,18),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,18))
  call loop_UV_W(G2(:,:,:,109),Q(:,10),wf(:,8),Q(:,52),G3(:,:,:,121))
  call check_last_UV_W(l_switch,G3(:,:,:,121),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,104))
  call loop_UV_W(G2(:,:,:,109),Q(:,10),wf(:,11),Q(:,52),G3(:,:,:,122))
  call check_last_UV_W(l_switch,G3(:,:,:,122),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,105))
  call loop_UV_W(G2(:,:,:,109),Q(:,10),wf(:,12),Q(:,52),G3(:,:,:,123))
  call check_last_UV_W(l_switch,G3(:,:,:,123),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,106))
  call loop_UV_W(G2(:,:,:,109),Q(:,10),wf(:,265),Q(:,52),G3(:,:,:,124))
  call check_last_UV_W(l_switch,G3(:,:,:,124),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,107))
  call loop_UV_W(G2(:,:,:,109),Q(:,10),wf(:,66),Q(:,20),G3(:,:,:,125))
  call loop_UV_W(G3(:,:,:,125),Q(:,30),wf(:,-5),Q(:,32),G4(:,:,:,36))
  call check_last_UV_W(l_switch,G4(:,:,:,36),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,18))
  call loop_UV_W(G2(:,:,:,109),Q(:,10),wf(:,266),Q(:,52),G3(:,:,:,126))
  call check_last_UV_W(l_switch,G3(:,:,:,126),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,108))
  call loop_UV_W(G2(:,:,:,109),Q(:,10),wf(:,70),Q(:,36),G3(:,:,:,127))
  call loop_UV_W(G3(:,:,:,127),Q(:,46),wf(:,-4),Q(:,16),G4(:,:,:,37))
  call check_last_UV_W(l_switch,G4(:,:,:,37),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,19))
  call loop_UV_W(G2(:,:,:,109),Q(:,10),wf(:,267),Q(:,52),G3(:,:,:,128))
  call check_last_UV_W(l_switch,G3(:,:,:,128),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,109))
  call loop_UV_W(G2(:,:,:,109),Q(:,10),wf(:,84),Q(:,48),G3(:,:,:,129))
  call loop_UV_W(G3(:,:,:,129),Q(:,58),wf(:,-2),Q(:,4),G4(:,:,:,38))
  call check_last_UV_W(l_switch,G4(:,:,:,38),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,20))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,70),wf(:,75),G1(:,:,:,134))
  call check_last_UV_W(l_switch,G1(:,:,:,134),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,106))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,75),wf(:,70),G1(:,:,:,135))
  call check_last_UV_W(l_switch,G1(:,:,:,135),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,107))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,70),wf(:,75),G1(:,:,:,136))
  call check_last_UV_W(l_switch,G1(:,:,:,136),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,108))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,262),G1(:,:,:,137))
  call check_last_UV_W(l_switch,G1(:,:,:,137),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,109))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,262),wf(:,-5),G1(:,:,:,138))
  call check_last_UV_W(l_switch,G1(:,:,:,138),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,110))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,262),G1(:,:,:,139))
  call check_last_UV_W(l_switch,G1(:,:,:,139),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,111))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,75),G1(:,:,:,140))
  call loop_UV_W(G1(:,:,:,140),Q(:,58),wf(:,-2),Q(:,4),G2(:,:,:,128))
  call check_last_UV_W(l_switch,G2(:,:,:,128),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,61))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,75),wf(:,-5),G1(:,:,:,141))
  call loop_UV_W(G1(:,:,:,141),Q(:,58),wf(:,-2),Q(:,4),G2(:,:,:,129))
  call check_last_UV_W(l_switch,G2(:,:,:,129),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,62))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,75),G1(:,:,:,142))
  call loop_UV_W(G1(:,:,:,142),Q(:,58),wf(:,-2),Q(:,4),G2(:,:,:,130))
  call check_last_UV_W(l_switch,G2(:,:,:,130),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,63))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,-2),Q(:,4),G2(:,:,:,131))
  call loop_GGG_G_12(G2(:,:,:,131),wf(:,-5),wf(:,75),G2(:,:,:,132))
  call check_last_UV_W(l_switch,G2(:,:,:,132),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,64))
  call loop_GGG_G_12(G2(:,:,:,131),wf(:,75),wf(:,-5),G2(:,:,:,133))
  call check_last_UV_W(l_switch,G2(:,:,:,133),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,65))
  call loop_GGG_G_23(G2(:,:,:,131),wf(:,-5),wf(:,75),G2(:,:,:,134))
  call check_last_UV_W(l_switch,G2(:,:,:,134),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,66))
  call loop_GGG_G_12(G2(:,:,:,131),wf(:,-4),wf(:,79),G2(:,:,:,135))
  call check_last_UV_W(l_switch,G2(:,:,:,135),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,67))
  call loop_GGG_G_12(G2(:,:,:,131),wf(:,79),wf(:,-4),G2(:,:,:,136))
  call check_last_UV_W(l_switch,G2(:,:,:,136),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,68))
  call loop_GGG_G_23(G2(:,:,:,131),wf(:,-4),wf(:,79),G2(:,:,:,137))
  call check_last_UV_W(l_switch,G2(:,:,:,137),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,69))
  call loop_GGG_G_12(G2(:,:,:,131),wf(:,-5),wf(:,-4),G2(:,:,:,138))
  call loop_UV_W(G2(:,:,:,138),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,130))
  call check_last_UV_W(l_switch,G3(:,:,:,130),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,110))
  call loop_GGG_G_12(G2(:,:,:,131),wf(:,-4),wf(:,-5),G2(:,:,:,139))
  call loop_UV_W(G2(:,:,:,139),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,131))
  call check_last_UV_W(l_switch,G3(:,:,:,131),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,111))
  call loop_GGG_G_23(G2(:,:,:,131),wf(:,-5),wf(:,-4),G2(:,:,:,140))
  call loop_UV_W(G2(:,:,:,140),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,132))
  call check_last_UV_W(l_switch,G3(:,:,:,132),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,112))
  call loop_UV_W(G2(:,:,:,131),Q(:,6),wf(:,-3),Q(:,8),G3(:,:,:,133))
  call loop_GGG_G_12(G3(:,:,:,133),wf(:,-5),wf(:,-4),G3(:,:,:,134))
  call check_last_UV_W(l_switch,G3(:,:,:,134),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,113))
  call loop_GGG_G_12(G3(:,:,:,133),wf(:,-4),wf(:,-5),G3(:,:,:,135))
  call check_last_UV_W(l_switch,G3(:,:,:,135),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,114))
  call loop_GGG_G_23(G3(:,:,:,133),wf(:,-5),wf(:,-4),G3(:,:,:,136))
  call check_last_UV_W(l_switch,G3(:,:,:,136),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,115))
  call loop_UV_W(G3(:,:,:,133),Q(:,14),wf(:,84),Q(:,48),G4(:,:,:,39))
  call check_last_UV_W(l_switch,G4(:,:,:,39),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,21))
  call loop_UV_W(G3(:,:,:,133),Q(:,14),wf(:,-5),Q(:,32),G4(:,:,:,40))
  call loop_UV_W(G4(:,:,:,40),Q(:,46),wf(:,-4),Q(:,16),G5(:,:,:,19))
  call check_last_UV_W(l_switch,G5(:,:,:,19),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,19))
  call loop_UV_W(G3(:,:,:,133),Q(:,14),wf(:,-4),Q(:,16),G4(:,:,:,41))
  call loop_UV_W(G4(:,:,:,41),Q(:,30),wf(:,-5),Q(:,32),G5(:,:,:,20))
  call check_last_UV_W(l_switch,G5(:,:,:,20),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,20))
  call loop_GGG_G_12(G2(:,:,:,131),wf(:,-3),wf(:,84),G2(:,:,:,141))
  call check_last_UV_W(l_switch,G2(:,:,:,141),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,70))
  call loop_GGG_G_12(G2(:,:,:,131),wf(:,84),wf(:,-3),G2(:,:,:,142))
  call check_last_UV_W(l_switch,G2(:,:,:,142),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,71))
  call loop_GGG_G_23(G2(:,:,:,131),wf(:,-3),wf(:,84),G2(:,:,:,143))
  call check_last_UV_W(l_switch,G2(:,:,:,143),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,72))
  call loop_GGG_G_12(G2(:,:,:,131),wf(:,-5),wf(:,-3),G2(:,:,:,144))
  call loop_UV_W(G2(:,:,:,144),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,137))
  call check_last_UV_W(l_switch,G3(:,:,:,137),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,116))
  call loop_GGG_G_12(G2(:,:,:,131),wf(:,-3),wf(:,-5),G2(:,:,:,145))
  call loop_UV_W(G2(:,:,:,145),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,138))
  call check_last_UV_W(l_switch,G3(:,:,:,138),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,117))
  call loop_GGG_G_23(G2(:,:,:,131),wf(:,-5),wf(:,-3),G2(:,:,:,146))
  call loop_UV_W(G2(:,:,:,146),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,139))
  call check_last_UV_W(l_switch,G3(:,:,:,139),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,118))
  call loop_UV_W(G2(:,:,:,131),Q(:,6),wf(:,-4),Q(:,16),G3(:,:,:,140))
  call loop_GGG_G_12(G3(:,:,:,140),wf(:,-5),wf(:,-3),G3(:,:,:,141))
  call check_last_UV_W(l_switch,G3(:,:,:,141),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,119))
  call loop_GGG_G_12(G3(:,:,:,140),wf(:,-3),wf(:,-5),G3(:,:,:,142))
  call check_last_UV_W(l_switch,G3(:,:,:,142),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,120))
  call loop_GGG_G_23(G3(:,:,:,140),wf(:,-5),wf(:,-3),G3(:,:,:,143))
  call check_last_UV_W(l_switch,G3(:,:,:,143),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,121))
  call loop_UV_W(G3(:,:,:,140),Q(:,22),wf(:,79),Q(:,40),G4(:,:,:,42))
  call check_last_UV_W(l_switch,G4(:,:,:,42),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,22))
  call loop_UV_W(G3(:,:,:,140),Q(:,22),wf(:,-5),Q(:,32),G4(:,:,:,43))
  call loop_UV_W(G4(:,:,:,43),Q(:,54),wf(:,-3),Q(:,8),G5(:,:,:,21))
  call check_last_UV_W(l_switch,G5(:,:,:,21),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,21))
  call loop_UV_W(G3(:,:,:,140),Q(:,22),wf(:,-3),Q(:,8),G4(:,:,:,44))
  call loop_UV_W(G4(:,:,:,44),Q(:,30),wf(:,-5),Q(:,32),G5(:,:,:,22))
  call check_last_UV_W(l_switch,G5(:,:,:,22),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,22))
  call loop_GGG_G_12(G2(:,:,:,131),wf(:,-4),wf(:,-3),G2(:,:,:,147))
  call loop_UV_W(G2(:,:,:,147),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,144))
  call check_last_UV_W(l_switch,G3(:,:,:,144),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,122))
  call loop_GGG_G_12(G2(:,:,:,131),wf(:,-3),wf(:,-4),G2(:,:,:,148))
  call loop_UV_W(G2(:,:,:,148),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,145))
  call check_last_UV_W(l_switch,G3(:,:,:,145),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,123))
  call loop_GGG_G_23(G2(:,:,:,131),wf(:,-4),wf(:,-3),G2(:,:,:,149))
  call loop_UV_W(G2(:,:,:,149),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,146))
  call check_last_UV_W(l_switch,G3(:,:,:,146),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,124))
  call loop_UV_W(G2(:,:,:,131),Q(:,6),wf(:,-5),Q(:,32),G3(:,:,:,147))
  call loop_GGG_G_12(G3(:,:,:,147),wf(:,-4),wf(:,-3),G3(:,:,:,148))
  call check_last_UV_W(l_switch,G3(:,:,:,148),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,125))
  call loop_GGG_G_12(G3(:,:,:,147),wf(:,-3),wf(:,-4),G3(:,:,:,149))
  call check_last_UV_W(l_switch,G3(:,:,:,149),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,126))
  call loop_GGG_G_23(G3(:,:,:,147),wf(:,-4),wf(:,-3),G3(:,:,:,150))
  call check_last_UV_W(l_switch,G3(:,:,:,150),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,127))
  call loop_UV_W(G3(:,:,:,147),Q(:,38),wf(:,75),Q(:,24),G4(:,:,:,45))
  call check_last_UV_W(l_switch,G4(:,:,:,45),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,23))
  call loop_UV_W(G3(:,:,:,147),Q(:,38),wf(:,-4),Q(:,16),G4(:,:,:,46))
  call loop_UV_W(G4(:,:,:,46),Q(:,54),wf(:,-3),Q(:,8),G5(:,:,:,23))
  call check_last_UV_W(l_switch,G5(:,:,:,23),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,23))
  call loop_UV_W(G3(:,:,:,147),Q(:,38),wf(:,-3),Q(:,8),G4(:,:,:,47))
  call loop_UV_W(G4(:,:,:,47),Q(:,46),wf(:,-4),Q(:,16),G5(:,:,:,24))
  call check_last_UV_W(l_switch,G5(:,:,:,24),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,24))
  call loop_UV_W(G2(:,:,:,131),Q(:,6),wf(:,2),Q(:,56),G3(:,:,:,151))
  call check_last_UV_W(l_switch,G3(:,:,:,151),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,128))
  call loop_UV_W(G2(:,:,:,131),Q(:,6),wf(:,5),Q(:,56),G3(:,:,:,152))
  call check_last_UV_W(l_switch,G3(:,:,:,152),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,129))
  call loop_UV_W(G2(:,:,:,131),Q(:,6),wf(:,6),Q(:,56),G3(:,:,:,153))
  call check_last_UV_W(l_switch,G3(:,:,:,153),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,130))
  call loop_UV_W(G2(:,:,:,131),Q(:,6),wf(:,268),Q(:,56),G3(:,:,:,154))
  call check_last_UV_W(l_switch,G3(:,:,:,154),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,131))
  call loop_UV_W(G2(:,:,:,131),Q(:,6),wf(:,75),Q(:,24),G3(:,:,:,155))
  call loop_UV_W(G3(:,:,:,155),Q(:,30),wf(:,-5),Q(:,32),G4(:,:,:,48))
  call check_last_UV_W(l_switch,G4(:,:,:,48),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,24))
  call loop_UV_W(G2(:,:,:,131),Q(:,6),wf(:,269),Q(:,56),G3(:,:,:,156))
  call check_last_UV_W(l_switch,G3(:,:,:,156),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,132))
  call loop_UV_W(G2(:,:,:,131),Q(:,6),wf(:,79),Q(:,40),G3(:,:,:,157))
  call loop_UV_W(G3(:,:,:,157),Q(:,46),wf(:,-4),Q(:,16),G4(:,:,:,49))
  call check_last_UV_W(l_switch,G4(:,:,:,49),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,25))
  call loop_UV_W(G2(:,:,:,131),Q(:,6),wf(:,270),Q(:,56),G3(:,:,:,158))
  call check_last_UV_W(l_switch,G3(:,:,:,158),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,133))
  call loop_UV_W(G2(:,:,:,131),Q(:,6),wf(:,84),Q(:,48),G3(:,:,:,159))
  call loop_UV_W(G3(:,:,:,159),Q(:,54),wf(:,-3),Q(:,8),G4(:,:,:,50))
  call check_last_UV_W(l_switch,G4(:,:,:,50),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,26))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-4),wf(:,263),G1(:,:,:,143))
  call check_last_UV_W(l_switch,G1(:,:,:,143),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,112))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,263),wf(:,-4),G1(:,:,:,144))
  call check_last_UV_W(l_switch,G1(:,:,:,144),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,113))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-4),wf(:,263),G1(:,:,:,145))
  call check_last_UV_W(l_switch,G1(:,:,:,145),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,114))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-4),wf(:,70),G1(:,:,:,146))
  call loop_UV_W(G1(:,:,:,146),Q(:,54),wf(:,-3),Q(:,8),G2(:,:,:,150))
  call check_last_UV_W(l_switch,G2(:,:,:,150),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,73))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,70),wf(:,-4),G1(:,:,:,147))
  call loop_UV_W(G1(:,:,:,147),Q(:,54),wf(:,-3),Q(:,8),G2(:,:,:,151))
  call check_last_UV_W(l_switch,G2(:,:,:,151),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,74))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-4),wf(:,70),G1(:,:,:,148))
  call loop_UV_W(G1(:,:,:,148),Q(:,54),wf(:,-3),Q(:,8),G2(:,:,:,152))
  call check_last_UV_W(l_switch,G2(:,:,:,152),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,75))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-4),wf(:,264),G1(:,:,:,149))
  call check_last_UV_W(l_switch,G1(:,:,:,149),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,115))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,264),wf(:,-4),G1(:,:,:,150))
  call check_last_UV_W(l_switch,G1(:,:,:,150),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,116))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-4),wf(:,264),G1(:,:,:,151))
  call check_last_UV_W(l_switch,G1(:,:,:,151),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,117))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-4),wf(:,79),G1(:,:,:,152))
  call loop_UV_W(G1(:,:,:,152),Q(:,58),wf(:,-2),Q(:,4),G2(:,:,:,153))
  call check_last_UV_W(l_switch,G2(:,:,:,153),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,76))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,79),wf(:,-4),G1(:,:,:,153))
  call loop_UV_W(G1(:,:,:,153),Q(:,58),wf(:,-2),Q(:,4),G2(:,:,:,154))
  call check_last_UV_W(l_switch,G2(:,:,:,154),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,77))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-4),wf(:,79),G1(:,:,:,154))
  call loop_UV_W(G1(:,:,:,154),Q(:,58),wf(:,-2),Q(:,4),G2(:,:,:,155))
  call check_last_UV_W(l_switch,G2(:,:,:,155),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,78))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,265),G1(:,:,:,155))
  call check_last_UV_W(l_switch,G1(:,:,:,155),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,118))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,265),wf(:,-3),G1(:,:,:,156))
  call check_last_UV_W(l_switch,G1(:,:,:,156),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,119))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,265),G1(:,:,:,157))
  call check_last_UV_W(l_switch,G1(:,:,:,157),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,120))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,66),G1(:,:,:,158))
  call loop_UV_W(G1(:,:,:,158),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,156))
  call check_last_UV_W(l_switch,G2(:,:,:,156),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,79))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,66),wf(:,-3),G1(:,:,:,159))
  call loop_UV_W(G1(:,:,:,159),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,157))
  call check_last_UV_W(l_switch,G2(:,:,:,157),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,80))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,66),G1(:,:,:,160))
  call loop_UV_W(G1(:,:,:,160),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,158))
  call check_last_UV_W(l_switch,G2(:,:,:,158),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,81))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,66),Q(:,20),G2(:,:,:,159))
  call loop_GGG_G_12(G2(:,:,:,159),wf(:,-5),wf(:,-3),G2(:,:,:,160))
  call check_last_UV_W(l_switch,G2(:,:,:,160),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,82))
  call loop_GGG_G_12(G2(:,:,:,159),wf(:,-3),wf(:,-5),G2(:,:,:,161))
  call check_last_UV_W(l_switch,G2(:,:,:,161),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,83))
  call loop_GGG_G_23(G2(:,:,:,159),wf(:,-5),wf(:,-3),G2(:,:,:,162))
  call check_last_UV_W(l_switch,G2(:,:,:,162),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,84))
  call loop_UV_W(G2(:,:,:,159),Q(:,22),wf(:,79),Q(:,40),G3(:,:,:,160))
  call check_last_UV_W(l_switch,G3(:,:,:,160),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,134))
  call loop_UV_W(G2(:,:,:,159),Q(:,22),wf(:,-5),Q(:,32),G3(:,:,:,161))
  call loop_UV_W(G3(:,:,:,161),Q(:,54),wf(:,-3),Q(:,8),G4(:,:,:,51))
  call check_last_UV_W(l_switch,G4(:,:,:,51),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,27))
  call loop_UV_W(G2(:,:,:,159),Q(:,22),wf(:,-3),Q(:,8),G3(:,:,:,162))
  call loop_UV_W(G3(:,:,:,162),Q(:,30),wf(:,-5),Q(:,32),G4(:,:,:,52))
  call check_last_UV_W(l_switch,G4(:,:,:,52),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,28))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,789),Q(:,60),G2(:,:,:,163))
  call check_last_UV_W(l_switch,G2(:,:,:,163),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,85))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,790),Q(:,60),G2(:,:,:,164))
  call check_last_UV_W(l_switch,G2(:,:,:,164),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,86))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,791),Q(:,60),G2(:,:,:,165))
  call check_last_UV_W(l_switch,G2(:,:,:,165),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,87))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,266),G1(:,:,:,161))
  call check_last_UV_W(l_switch,G1(:,:,:,161),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,121))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,266),wf(:,-3),G1(:,:,:,162))
  call check_last_UV_W(l_switch,G1(:,:,:,162),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,122))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,266),G1(:,:,:,163))
  call check_last_UV_W(l_switch,G1(:,:,:,163),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,123))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,70),G1(:,:,:,164))
  call loop_UV_W(G1(:,:,:,164),Q(:,46),wf(:,-4),Q(:,16),G2(:,:,:,166))
  call check_last_UV_W(l_switch,G2(:,:,:,166),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,88))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,70),wf(:,-3),G1(:,:,:,165))
  call loop_UV_W(G1(:,:,:,165),Q(:,46),wf(:,-4),Q(:,16),G2(:,:,:,167))
  call check_last_UV_W(l_switch,G2(:,:,:,167),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,89))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,70),G1(:,:,:,166))
  call loop_UV_W(G1(:,:,:,166),Q(:,46),wf(:,-4),Q(:,16),G2(:,:,:,168))
  call check_last_UV_W(l_switch,G2(:,:,:,168),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,90))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,267),G1(:,:,:,167))
  call check_last_UV_W(l_switch,G1(:,:,:,167),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,124))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,267),wf(:,-3),G1(:,:,:,168))
  call check_last_UV_W(l_switch,G1(:,:,:,168),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,125))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,267),G1(:,:,:,169))
  call check_last_UV_W(l_switch,G1(:,:,:,169),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,126))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,84),G1(:,:,:,170))
  call loop_UV_W(G1(:,:,:,170),Q(:,58),wf(:,-2),Q(:,4),G2(:,:,:,169))
  call check_last_UV_W(l_switch,G2(:,:,:,169),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,91))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,84),wf(:,-3),G1(:,:,:,171))
  call loop_UV_W(G1(:,:,:,171),Q(:,58),wf(:,-2),Q(:,4),G2(:,:,:,170))
  call check_last_UV_W(l_switch,G2(:,:,:,170),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,92))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,84),G1(:,:,:,172))
  call loop_UV_W(G1(:,:,:,172),Q(:,58),wf(:,-2),Q(:,4),G2(:,:,:,171))
  call check_last_UV_W(l_switch,G2(:,:,:,171),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,93))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,70),Q(:,36),G2(:,:,:,172))
  call loop_GGG_G_12(G2(:,:,:,172),wf(:,-4),wf(:,-3),G2(:,:,:,173))
  call check_last_UV_W(l_switch,G2(:,:,:,173),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,94))
  call loop_GGG_G_12(G2(:,:,:,172),wf(:,-3),wf(:,-4),G2(:,:,:,174))
  call check_last_UV_W(l_switch,G2(:,:,:,174),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,95))
  call loop_GGG_G_23(G2(:,:,:,172),wf(:,-4),wf(:,-3),G2(:,:,:,175))
  call check_last_UV_W(l_switch,G2(:,:,:,175),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,96))
  call loop_UV_W(G2(:,:,:,172),Q(:,38),wf(:,75),Q(:,24),G3(:,:,:,163))
  call check_last_UV_W(l_switch,G3(:,:,:,163),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,135))
  call loop_UV_W(G2(:,:,:,172),Q(:,38),wf(:,-4),Q(:,16),G3(:,:,:,164))
  call loop_UV_W(G3(:,:,:,164),Q(:,54),wf(:,-3),Q(:,8),G4(:,:,:,53))
  call check_last_UV_W(l_switch,G4(:,:,:,53),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,29))
  call loop_UV_W(G2(:,:,:,172),Q(:,38),wf(:,-3),Q(:,8),G3(:,:,:,165))
  call loop_UV_W(G3(:,:,:,165),Q(:,46),wf(:,-4),Q(:,16),G4(:,:,:,54))
  call check_last_UV_W(l_switch,G4(:,:,:,54),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,30))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,793),Q(:,60),G2(:,:,:,176))
  call check_last_UV_W(l_switch,G2(:,:,:,176),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,97))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,794),Q(:,60),G2(:,:,:,177))
  call check_last_UV_W(l_switch,G2(:,:,:,177),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,98))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,795),Q(:,60),G2(:,:,:,178))
  call check_last_UV_W(l_switch,G2(:,:,:,178),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,99))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,797),Q(:,60),G2(:,:,:,179))
  call check_last_UV_W(l_switch,G2(:,:,:,179),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,100))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,798),Q(:,60),G2(:,:,:,180))
  call check_last_UV_W(l_switch,G2(:,:,:,180),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,101))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,799),Q(:,60),G2(:,:,:,181))
  call check_last_UV_W(l_switch,G2(:,:,:,181),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,102))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,2),Q(:,56),G2(:,:,:,182))
  call loop_UV_W(G2(:,:,:,182),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,166))
  call check_last_UV_W(l_switch,G3(:,:,:,166),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,136))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,5),Q(:,56),G2(:,:,:,183))
  call loop_UV_W(G2(:,:,:,183),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,167))
  call check_last_UV_W(l_switch,G3(:,:,:,167),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,137))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,6),Q(:,56),G2(:,:,:,184))
  call loop_UV_W(G2(:,:,:,184),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,168))
  call check_last_UV_W(l_switch,G3(:,:,:,168),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,138))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,268),G1(:,:,:,173))
  call check_last_UV_W(l_switch,G1(:,:,:,173),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,127))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,268),wf(:,-2),G1(:,:,:,174))
  call check_last_UV_W(l_switch,G1(:,:,:,174),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,128))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,268),G1(:,:,:,175))
  call check_last_UV_W(l_switch,G1(:,:,:,175),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,129))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,75),G1(:,:,:,176))
  call loop_UV_W(G1(:,:,:,176),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,185))
  call check_last_UV_W(l_switch,G2(:,:,:,185),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,103))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,75),wf(:,-2),G1(:,:,:,177))
  call loop_UV_W(G1(:,:,:,177),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,186))
  call check_last_UV_W(l_switch,G2(:,:,:,186),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,104))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,75),G1(:,:,:,178))
  call loop_UV_W(G1(:,:,:,178),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,187))
  call check_last_UV_W(l_switch,G2(:,:,:,187),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,105))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,75),Q(:,24),G2(:,:,:,188))
  call loop_GGG_G_12(G2(:,:,:,188),wf(:,-5),wf(:,-2),G2(:,:,:,189))
  call check_last_UV_W(l_switch,G2(:,:,:,189),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,106))
  call loop_GGG_G_12(G2(:,:,:,188),wf(:,-2),wf(:,-5),G2(:,:,:,190))
  call check_last_UV_W(l_switch,G2(:,:,:,190),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,107))
  call loop_GGG_G_23(G2(:,:,:,188),wf(:,-5),wf(:,-2),G2(:,:,:,191))
  call check_last_UV_W(l_switch,G2(:,:,:,191),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,108))
  call loop_UV_W(G2(:,:,:,188),Q(:,26),wf(:,70),Q(:,36),G3(:,:,:,169))
  call check_last_UV_W(l_switch,G3(:,:,:,169),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,139))
  call loop_UV_W(G2(:,:,:,188),Q(:,26),wf(:,-5),Q(:,32),G3(:,:,:,170))
  call loop_UV_W(G3(:,:,:,170),Q(:,58),wf(:,-2),Q(:,4),G4(:,:,:,55))
  call check_last_UV_W(l_switch,G4(:,:,:,55),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,31))
  call loop_UV_W(G2(:,:,:,188),Q(:,26),wf(:,-2),Q(:,4),G3(:,:,:,171))
  call loop_UV_W(G3(:,:,:,171),Q(:,30),wf(:,-5),Q(:,32),G4(:,:,:,56))
  call check_last_UV_W(l_switch,G4(:,:,:,56),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,32))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,803),Q(:,60),G2(:,:,:,192))
  call check_last_UV_W(l_switch,G2(:,:,:,192),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,109))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,804),Q(:,60),G2(:,:,:,193))
  call check_last_UV_W(l_switch,G2(:,:,:,193),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,110))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,805),Q(:,60),G2(:,:,:,194))
  call check_last_UV_W(l_switch,G2(:,:,:,194),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,111))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,269),G1(:,:,:,179))
  call check_last_UV_W(l_switch,G1(:,:,:,179),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,130))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,269),wf(:,-2),G1(:,:,:,180))
  call check_last_UV_W(l_switch,G1(:,:,:,180),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,131))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,269),G1(:,:,:,181))
  call check_last_UV_W(l_switch,G1(:,:,:,181),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,132))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,79),G1(:,:,:,182))
  call loop_UV_W(G1(:,:,:,182),Q(:,46),wf(:,-4),Q(:,16),G2(:,:,:,195))
  call check_last_UV_W(l_switch,G2(:,:,:,195),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,112))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,79),wf(:,-2),G1(:,:,:,183))
  call loop_UV_W(G1(:,:,:,183),Q(:,46),wf(:,-4),Q(:,16),G2(:,:,:,196))
  call check_last_UV_W(l_switch,G2(:,:,:,196),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,113))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,79),G1(:,:,:,184))
  call loop_UV_W(G1(:,:,:,184),Q(:,46),wf(:,-4),Q(:,16),G2(:,:,:,197))
  call check_last_UV_W(l_switch,G2(:,:,:,197),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,114))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,270),G1(:,:,:,185))
  call check_last_UV_W(l_switch,G1(:,:,:,185),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,133))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,270),wf(:,-2),G1(:,:,:,186))
  call check_last_UV_W(l_switch,G1(:,:,:,186),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,134))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,270),G1(:,:,:,187))
  call check_last_UV_W(l_switch,G1(:,:,:,187),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,135))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,84),G1(:,:,:,188))
  call loop_UV_W(G1(:,:,:,188),Q(:,54),wf(:,-3),Q(:,8),G2(:,:,:,198))
  call check_last_UV_W(l_switch,G2(:,:,:,198),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,115))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,84),wf(:,-2),G1(:,:,:,189))
  call loop_UV_W(G1(:,:,:,189),Q(:,54),wf(:,-3),Q(:,8),G2(:,:,:,199))
  call check_last_UV_W(l_switch,G2(:,:,:,199),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,116))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,84),G1(:,:,:,190))
  call loop_UV_W(G1(:,:,:,190),Q(:,54),wf(:,-3),Q(:,8),G2(:,:,:,200))
  call check_last_UV_W(l_switch,G2(:,:,:,200),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,117))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,79),Q(:,40),G2(:,:,:,201))
  call loop_GGG_G_12(G2(:,:,:,201),wf(:,-4),wf(:,-2),G2(:,:,:,202))
  call check_last_UV_W(l_switch,G2(:,:,:,202),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,118))
  call loop_GGG_G_12(G2(:,:,:,201),wf(:,-2),wf(:,-4),G2(:,:,:,203))
  call check_last_UV_W(l_switch,G2(:,:,:,203),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,119))
  call loop_GGG_G_23(G2(:,:,:,201),wf(:,-4),wf(:,-2),G2(:,:,:,204))
  call check_last_UV_W(l_switch,G2(:,:,:,204),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,120))
  call loop_UV_W(G2(:,:,:,201),Q(:,42),wf(:,66),Q(:,20),G3(:,:,:,172))
  call check_last_UV_W(l_switch,G3(:,:,:,172),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,140))
  call loop_UV_W(G2(:,:,:,201),Q(:,42),wf(:,-4),Q(:,16),G3(:,:,:,173))
  call loop_UV_W(G3(:,:,:,173),Q(:,58),wf(:,-2),Q(:,4),G4(:,:,:,57))
  call check_last_UV_W(l_switch,G4(:,:,:,57),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,33))
  call loop_UV_W(G2(:,:,:,201),Q(:,42),wf(:,-2),Q(:,4),G3(:,:,:,174))
  call loop_UV_W(G3(:,:,:,174),Q(:,46),wf(:,-4),Q(:,16),G4(:,:,:,58))
  call check_last_UV_W(l_switch,G4(:,:,:,58),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,34))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,807),Q(:,60),G2(:,:,:,205))
  call check_last_UV_W(l_switch,G2(:,:,:,205),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,121))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,808),Q(:,60),G2(:,:,:,206))
  call check_last_UV_W(l_switch,G2(:,:,:,206),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,122))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,809),Q(:,60),G2(:,:,:,207))
  call check_last_UV_W(l_switch,G2(:,:,:,207),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,123))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,811),Q(:,60),G2(:,:,:,208))
  call check_last_UV_W(l_switch,G2(:,:,:,208),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,124))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,812),Q(:,60),G2(:,:,:,209))
  call check_last_UV_W(l_switch,G2(:,:,:,209),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,125))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,813),Q(:,60),G2(:,:,:,210))
  call check_last_UV_W(l_switch,G2(:,:,:,210),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,126))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,8),Q(:,52),G2(:,:,:,211))
  call loop_UV_W(G2(:,:,:,211),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,175))
  call check_last_UV_W(l_switch,G3(:,:,:,175),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,141))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,11),Q(:,52),G2(:,:,:,212))
  call loop_UV_W(G2(:,:,:,212),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,176))
  call check_last_UV_W(l_switch,G3(:,:,:,176),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,142))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,12),Q(:,52),G2(:,:,:,213))
  call loop_UV_W(G2(:,:,:,213),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,177))
  call check_last_UV_W(l_switch,G3(:,:,:,177),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,143))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,84),Q(:,48),G2(:,:,:,214))
  call loop_GGG_G_12(G2(:,:,:,214),wf(:,-3),wf(:,-2),G2(:,:,:,215))
  call check_last_UV_W(l_switch,G2(:,:,:,215),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,127))
  call loop_GGG_G_12(G2(:,:,:,214),wf(:,-2),wf(:,-3),G2(:,:,:,216))
  call check_last_UV_W(l_switch,G2(:,:,:,216),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,128))
  call loop_GGG_G_23(G2(:,:,:,214),wf(:,-3),wf(:,-2),G2(:,:,:,217))
  call check_last_UV_W(l_switch,G2(:,:,:,217),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,129))
  call loop_UV_W(G2(:,:,:,214),Q(:,50),wf(:,62),Q(:,12),G3(:,:,:,178))
  call check_last_UV_W(l_switch,G3(:,:,:,178),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,144))
  call loop_UV_W(G2(:,:,:,214),Q(:,50),wf(:,-3),Q(:,8),G3(:,:,:,179))
  call loop_UV_W(G3(:,:,:,179),Q(:,58),wf(:,-2),Q(:,4),G4(:,:,:,59))
  call check_last_UV_W(l_switch,G4(:,:,:,59),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,35))
  call loop_UV_W(G2(:,:,:,214),Q(:,50),wf(:,-2),Q(:,4),G3(:,:,:,180))
  call loop_UV_W(G3(:,:,:,180),Q(:,54),wf(:,-3),Q(:,8),G4(:,:,:,60))
  call check_last_UV_W(l_switch,G4(:,:,:,60),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,36))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,817),Q(:,60),G2(:,:,:,218))
  call check_last_UV_W(l_switch,G2(:,:,:,218),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,130))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,818),Q(:,60),G2(:,:,:,219))
  call check_last_UV_W(l_switch,G2(:,:,:,219),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,131))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,819),Q(:,60),G2(:,:,:,220))
  call check_last_UV_W(l_switch,G2(:,:,:,220),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,132))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,821),Q(:,60),G2(:,:,:,221))
  call check_last_UV_W(l_switch,G2(:,:,:,221),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,133))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,822),Q(:,60),G2(:,:,:,222))
  call check_last_UV_W(l_switch,G2(:,:,:,222),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,134))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,823),Q(:,60),G2(:,:,:,223))
  call check_last_UV_W(l_switch,G2(:,:,:,223),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,135))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,14),Q(:,44),G2(:,:,:,224))
  call loop_UV_W(G2(:,:,:,224),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,181))
  call check_last_UV_W(l_switch,G3(:,:,:,181),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,145))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,17),Q(:,44),G2(:,:,:,225))
  call loop_UV_W(G2(:,:,:,225),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,182))
  call check_last_UV_W(l_switch,G3(:,:,:,182),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,146))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,18),Q(:,44),G2(:,:,:,226))
  call loop_UV_W(G2(:,:,:,226),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,183))
  call check_last_UV_W(l_switch,G3(:,:,:,183),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,147))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,827),Q(:,60),G2(:,:,:,227))
  call check_last_UV_W(l_switch,G2(:,:,:,227),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,136))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,828),Q(:,60),G2(:,:,:,228))
  call check_last_UV_W(l_switch,G2(:,:,:,228),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,137))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,829),Q(:,60),G2(:,:,:,229))
  call check_last_UV_W(l_switch,G2(:,:,:,229),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,138))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,20),Q(:,28),G2(:,:,:,230))
  call loop_UV_W(G2(:,:,:,230),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,184))
  call check_last_UV_W(l_switch,G3(:,:,:,184),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,148))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,23),Q(:,28),G2(:,:,:,231))
  call loop_UV_W(G2(:,:,:,231),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,185))
  call check_last_UV_W(l_switch,G3(:,:,:,185),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,149))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,24),Q(:,28),G2(:,:,:,232))
  call loop_UV_W(G2(:,:,:,232),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,186))
  call check_last_UV_W(l_switch,G3(:,:,:,186),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,150))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,1186),Q(:,60),G2(:,:,:,233))
  call check_last_UV_W(l_switch,G2(:,:,:,233),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,139))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,1391),Q(:,60),G2(:,:,:,234))
  call check_last_UV_W(l_switch,G2(:,:,:,234),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,140))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,253),Q(:,28),G2(:,:,:,235))
  call loop_UV_W(G2(:,:,:,235),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,187))
  call check_last_UV_W(l_switch,G3(:,:,:,187),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,151))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,1392),Q(:,60),G2(:,:,:,236))
  call check_last_UV_W(l_switch,G2(:,:,:,236),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,141))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,254),Q(:,44),G2(:,:,:,237))
  call loop_UV_W(G2(:,:,:,237),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,188))
  call check_last_UV_W(l_switch,G3(:,:,:,188),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,152))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,1194),Q(:,60),G2(:,:,:,238))
  call check_last_UV_W(l_switch,G2(:,:,:,238),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,142))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,1427),Q(:,60),G2(:,:,:,239))
  call check_last_UV_W(l_switch,G2(:,:,:,239),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,143))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,258),Q(:,28),G2(:,:,:,240))
  call loop_UV_W(G2(:,:,:,240),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,189))
  call check_last_UV_W(l_switch,G3(:,:,:,189),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,153))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,1428),Q(:,60),G2(:,:,:,241))
  call check_last_UV_W(l_switch,G2(:,:,:,241),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,144))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,265),Q(:,52),G2(:,:,:,242))
  call loop_UV_W(G2(:,:,:,242),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,190))
  call check_last_UV_W(l_switch,G3(:,:,:,190),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,154))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,1201),Q(:,60),G2(:,:,:,243))
  call check_last_UV_W(l_switch,G2(:,:,:,243),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,145))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,1439),Q(:,60),G2(:,:,:,244))
  call check_last_UV_W(l_switch,G2(:,:,:,244),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,146))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,262),Q(:,28),G2(:,:,:,245))
  call loop_UV_W(G2(:,:,:,245),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,191))
  call check_last_UV_W(l_switch,G3(:,:,:,191),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,155))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,1440),Q(:,60),G2(:,:,:,246))
  call check_last_UV_W(l_switch,G2(:,:,:,246),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,147))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,268),Q(:,56),G2(:,:,:,247))
  call loop_UV_W(G2(:,:,:,247),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,192))
  call check_last_UV_W(l_switch,G3(:,:,:,192),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,156))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,1475),Q(:,60),G2(:,:,:,248))
  call check_last_UV_W(l_switch,G2(:,:,:,248),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,148))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,263),Q(:,44),G2(:,:,:,249))
  call loop_UV_W(G2(:,:,:,249),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,193))
  call check_last_UV_W(l_switch,G3(:,:,:,193),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,157))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,1476),Q(:,60),G2(:,:,:,250))
  call check_last_UV_W(l_switch,G2(:,:,:,250),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,149))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,266),Q(:,52),G2(:,:,:,251))
  call loop_UV_W(G2(:,:,:,251),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,194))
  call check_last_UV_W(l_switch,G3(:,:,:,194),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,158))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,1487),Q(:,60),G2(:,:,:,252))
  call check_last_UV_W(l_switch,G2(:,:,:,252),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,150))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,264),Q(:,44),G2(:,:,:,253))
  call loop_UV_W(G2(:,:,:,253),Q(:,46),wf(:,-4),Q(:,16),G3(:,:,:,195))
  call check_last_UV_W(l_switch,G3(:,:,:,195),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,159))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,1488),Q(:,60),G2(:,:,:,254))
  call check_last_UV_W(l_switch,G2(:,:,:,254),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,151))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,269),Q(:,56),G2(:,:,:,255))
  call loop_UV_W(G2(:,:,:,255),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,196))
  call check_last_UV_W(l_switch,G3(:,:,:,196),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,160))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,1499),Q(:,60),G2(:,:,:,256))
  call check_last_UV_W(l_switch,G2(:,:,:,256),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,152))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,1500),Q(:,60),G2(:,:,:,257))
  call check_last_UV_W(l_switch,G2(:,:,:,257),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,153))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,267),Q(:,52),G2(:,:,:,258))
  call loop_UV_W(G2(:,:,:,258),Q(:,54),wf(:,-3),Q(:,8),G3(:,:,:,197))
  call check_last_UV_W(l_switch,G3(:,:,:,197),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,161))
  call loop_UV_W(G1(:,:,:,1),Q(:,2),wf(:,270),Q(:,56),G2(:,:,:,259))
  call loop_UV_W(G2(:,:,:,259),Q(:,58),wf(:,-2),Q(:,4),G3(:,:,:,198))
  call check_last_UV_W(l_switch,G3(:,:,:,198),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,162))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(149)-M(151)-M(153) &
    +M(154)+M(164)-M(188)-M(212)+M(218))) * den(30)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(5)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(149)+M(150)+M(152) &
    -M(154)-M(164)+M(170)+M(194)-M(218))) * den(30)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(5)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(6)*(-M(150)+M(151)-M(152) &
    +M(153)-M(170)+M(188)-M(194)+M(212))) * den(30)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(131)-M(137)-M(143) &
    +M(145)+M(236)-M(242)-M(248)+M(250))) * den(30)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(5)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(131)+M(133)+M(139) &
    -M(145)-M(236)+M(238)+M(244)-M(250))) * den(30)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(5)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(6)*(-M(133)+M(137)-M(139) &
    +M(143)-M(238)+M(242)-M(244)+M(248))) * den(30)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,6)
  Gcoeff = (c(6)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(30)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(139)-M(145)+M(149)-M(150)-M(152)+M(154)+M(164)-M(170)-M(194)+M(218)-M(236)+M(238)+M(244) &
    -M(250))) * den(30)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(6)*(-M(133)+M(137)-M(139)+M(143)+M(150)-M(151)+M(152)-M(153)+M(170)-M(188)+M(194)-M(212)-M(238)+M(242)-M(244) &
    +M(248))) * den(30)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(143)-M(145)-M(147) &
    +M(148)+M(166)-M(190)-M(236)+M(242))) * den(28)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(5)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(143)+M(144)+M(146) &
    -M(148)-M(166)+M(176)+M(200)-M(242))) * den(28)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,11)
  Gcoeff = (c(5)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(6)*(-M(144)+M(145)-M(146) &
    +M(147)-M(176)+M(190)-M(200)+M(236))) * den(28)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,12)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(132)-M(138)-M(149) &
    +M(151)+M(212)-M(218)-M(224)+M(226))) * den(28)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,13)
  Gcoeff = (c(5)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(132)+M(135)+M(141) &
    -M(151)-M(212)+M(214)+M(220)-M(226))) * den(28)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,14)
  Gcoeff = (c(5)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(6)*(-M(135)+M(138)-M(141) &
    +M(149)-M(214)+M(218)-M(220)+M(224))) * den(28)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,15)
  Gcoeff = (c(6)*(M(132)-M(138)-M(143)+M(145)+M(147)-M(148)-M(149)+M(151)-M(166)+M(190)+M(212)-M(218)-M(224)+M(226)+M(236) &
    -M(242))) * den(28)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,16)
  Gcoeff = (c(6)*(-M(132)+M(135)+M(141)+M(143)-M(144)-M(146)+M(148)-M(151)+M(166)-M(176)-M(200)-M(212)+M(214)+M(220)-M(226) &
    +M(242))) * den(28)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,17)
  Gcoeff = (c(6)*(-M(135)+M(138)-M(141)+M(144)-M(145)+M(146)-M(147)+M(149)+M(176)-M(190)+M(200)-M(214)+M(218)-M(220)+M(224) &
    -M(236))) * den(28)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,18)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(44)+M(47)+M(48)+M(50)+M(51)+M(52)+M(56)+M(57)+M(58)+M(59)+M(61)+M(62)+M(71)+M(73)+M(74)+M(84)+M(86)+M(96)+M(99) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(154)+M(164)))
  T4sum(1:15,1) = T4sum(1:15,1) + Gcoeff * G2tensor(:,19)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(47)+M(48)+M(50)+M(51)+M(52)+M(59)+M(61)+M(62)+M(68)+M(69)+M(70)+M(71)+M(73)+M(74)+M(81)+M(86)+M(93)+M(99) &
    +M(100)+M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(153)+M(188)))
  T4sum(1:15,1) = T4sum(1:15,1) + Gcoeff * G2tensor(:,20)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(153)-M(154)-M(164)+M(188)))
  T4sum(1:15,1) = T4sum(1:15,1) + Gcoeff * G2tensor(:,21)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(44)+M(47)+M(48)+M(49)+M(50)+M(51)+M(56)+M(57)+M(58)+M(59)+M(62)+M(64)+M(71)+M(74)+M(76)+M(84)+M(87)+M(88)+M(96)+M(98) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(148)+M(166)))
  T4sum(1:15,1) = T4sum(1:15,1) + Gcoeff * G2tensor(:,22)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(47)+M(48)+M(49)+M(50)+M(51)+M(59)+M(62)+M(64)+M(68)+M(69)+M(70)+M(71)+M(74)+M(76)+M(81)+M(87)+M(88)+M(93)+M(98) &
    +M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(147)+M(190)))
  T4sum(1:15,1) = T4sum(1:15,1) + Gcoeff * G2tensor(:,23)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(147)-M(148)-M(166)+M(190)))
  T4sum(1:15,1) = T4sum(1:15,1) + Gcoeff * G2tensor(:,24)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(61) &
    +M(64)-M(73)+M(76)-M(86)+M(87)+M(88)+M(98)-M(99)-M(100)-M(104)+M(106)-M(110)+M(112)+M(117)+M(118)-M(123)-M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(148)-M(154)-M(164)+M(166)))
  T4sum(1:15,1) = T4sum(1:15,1) + Gcoeff * G2tensor(:,25)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(61) &
    +M(64)-M(73)+M(76)-M(86)+M(87)+M(88)+M(98)-M(99)-M(100)-M(104)+M(106)-M(110)+M(112)+M(117)+M(118)-M(123)-M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(147)-M(153)-M(188)+M(190)))
  T4sum(1:15,1) = T4sum(1:15,1) + Gcoeff * G2tensor(:,26)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(147)-M(148)-M(153)+M(154) &
    +M(164)-M(166)-M(188)+M(190)))
  T4sum(1:15,1) = T4sum(1:15,1) + Gcoeff * G2tensor(:,27)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(42)+M(43)+M(44)+M(45)+M(46)+M(52)+M(56)+M(64)+M(69)+M(70)+M(76)+M(80)+M(81)+M(83)+M(86)+M(87)+M(88)+M(92)+M(93)+M(95) &
    +M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(138)+M(224)))
  T4sum(1:15,2) = T4sum(1:15,2) + Gcoeff * G2tensor(:,28)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(45)+M(46)+M(52)+M(57)+M(58)+M(64)+M(68)+M(76)+M(80)+M(83)+M(84)+M(86)+M(87)+M(88)+M(92)+M(95)+M(96) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(132)+M(226)))
  T4sum(1:15,2) = T4sum(1:15,2) + Gcoeff * G2tensor(:,31)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)+M(58)+M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(132)-M(138)-M(224)+M(226)))
  T4sum(1:15,2) = T4sum(1:15,2) + Gcoeff * G2tensor(:,34)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(43)+M(44)+M(45)+M(46)+M(49)+M(56)+M(61)+M(69)+M(70)+M(73)+M(80)+M(81)+M(83)+M(92)+M(93)+M(95)+M(98)+M(99) &
    +M(100)+M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(137)+M(248)))
  T4sum(1:15,2) = T4sum(1:15,2) + Gcoeff * G2tensor(:,29)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(45)+M(46)+M(49)+M(57)+M(58)+M(61)+M(68)+M(73)+M(80)+M(83)+M(84)+M(92)+M(95)+M(96)+M(98)+M(99) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(131)+M(250)))
  T4sum(1:15,2) = T4sum(1:15,2) + Gcoeff * G2tensor(:,32)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)+M(58)+M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(131)-M(137)-M(248)+M(250)))
  T4sum(1:15,2) = T4sum(1:15,2) + Gcoeff * G2tensor(:,35)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)+M(61) &
    -M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(137)-M(138)-M(224)+M(248)))
  T4sum(1:15,2) = T4sum(1:15,2) + Gcoeff * G2tensor(:,30)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)+M(61) &
    -M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(131)-M(132)-M(226)+M(250)))
  T4sum(1:15,2) = T4sum(1:15,2) + Gcoeff * G2tensor(:,33)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(131)-M(132)-M(137)+M(138) &
    +M(224)-M(226)-M(248)+M(250)))
  T4sum(1:15,2) = T4sum(1:15,2) + Gcoeff * G2tensor(:,36)
  Gcoeff = (c(5)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(137)-M(139)-M(141) &
    +M(142)+M(172)-M(214)-M(238)+M(248))) * den(24)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,37)
  Gcoeff = (c(5)*(-M(61)+M(64)+M(87)-M(99)-M(104)+M(106)+M(117)-M(123)+M(125)-M(126)-M(128)+M(130))+c(6)*(-M(137)+M(138)+M(140) &
    -M(142)-M(172)+M(178)+M(224)-M(248))) * den(24)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,38)
  Gcoeff = (c(5)*(-M(64)+M(82)-M(87)+M(94)-M(106)+M(114)-M(117)+M(120)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(138)+M(139)-M(140) &
    +M(141)-M(178)+M(214)-M(224)+M(238))) * den(24)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,39)
  Gcoeff = (c(5)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(134)-M(144)-M(150) &
    +M(153)+M(188)-M(194)-M(200)+M(202))) * den(24)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,40)
  Gcoeff = (c(5)*(-M(61)+M(64)+M(87)-M(99)-M(104)+M(106)+M(117)-M(123)+M(125)-M(126)-M(128)+M(130))+c(6)*(-M(134)+M(136)+M(147) &
    -M(153)-M(188)+M(190)+M(196)-M(202))) * den(24)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,41)
  Gcoeff = (c(5)*(-M(64)+M(82)-M(87)+M(94)-M(106)+M(114)-M(117)+M(120)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(136)+M(144)-M(147) &
    +M(150)-M(190)+M(194)-M(196)+M(200))) * den(24)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,42)
  Gcoeff = (c(6)*(M(134)-M(137)+M(139)+M(141)-M(142)-M(144)-M(150)+M(153)-M(172)+M(188)-M(194)-M(200)+M(202)+M(214)+M(238) &
    -M(248))) * den(24)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,43)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(137)-M(138)-M(140)+M(142)+M(147)-M(153)+M(172)-M(178)-M(188)+M(190)+M(196)-M(202)-M(224) &
    +M(248))) * den(24)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,44)
  Gcoeff = (c(6)*(-M(136)+M(138)-M(139)+M(140)-M(141)+M(144)-M(147)+M(150)+M(178)-M(190)+M(194)-M(196)+M(200)-M(214)+M(224) &
    -M(238))) * den(24)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,45)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(44)+M(45)+M(47)+M(50)+M(51)+M(52)+M(56)+M(58)+M(59)+M(60)+M(61)+M(62)+M(72)+M(74)+M(83)+M(85)+M(86)+M(96)+M(97)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(152)+M(170)))
  T4sum(1:15,4) = T4sum(1:15,4) + Gcoeff * G2tensor(:,46)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(44)+M(45)+M(50)+M(51)+M(52)+M(56)+M(58)+M(62)+M(69)+M(74)+M(80)+M(81)+M(82)+M(83)+M(85)+M(86)+M(94)+M(96)+M(97) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(151)+M(212)))
  T4sum(1:15,4) = T4sum(1:15,4) + Gcoeff * G2tensor(:,47)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    -M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(151)-M(152)-M(170)+M(212)))
  T4sum(1:15,4) = T4sum(1:15,4) + Gcoeff * G2tensor(:,48)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(44)+M(45)+M(46)+M(47)+M(50)+M(52)+M(56)+M(59)+M(60)+M(61)+M(62)+M(63)+M(72)+M(75)+M(76)+M(83)+M(86)+M(88)+M(95)+M(99) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(142)+M(172)))
  T4sum(1:15,4) = T4sum(1:15,4) + Gcoeff * G2tensor(:,49)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(42)+M(44)+M(45)+M(46)+M(50)+M(52)+M(56)+M(62)+M(63)+M(69)+M(75)+M(76)+M(80)+M(81)+M(82)+M(83)+M(86)+M(88)+M(94)+M(95) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(141)+M(214)))
  T4sum(1:15,4) = T4sum(1:15,4) + Gcoeff * G2tensor(:,50)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    -M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(141)-M(142)-M(172)+M(214)))
  T4sum(1:15,4) = T4sum(1:15,4) + Gcoeff * G2tensor(:,51)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(58) &
    +M(63)-M(74)+M(75)+M(76)-M(85)+M(88)+M(95)-M(96)-M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(142)-M(152)-M(170)+M(172)))
  T4sum(1:15,4) = T4sum(1:15,4) + Gcoeff * G2tensor(:,52)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(58) &
    +M(63)-M(74)+M(75)+M(76)-M(85)+M(88)+M(95)-M(96)-M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(141)-M(151)-M(212)+M(214)))
  T4sum(1:15,4) = T4sum(1:15,4) + Gcoeff * G2tensor(:,53)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(141)-M(142)-M(151)+M(152) &
    +M(170)-M(172)-M(212)+M(214)))
  T4sum(1:15,4) = T4sum(1:15,4) + Gcoeff * G2tensor(:,54)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(41)+M(43)+M(47)+M(48)+M(49)+M(51)+M(59)+M(63)+M(68)+M(69)+M(71)+M(74)+M(75)+M(76)+M(81)+M(82)+M(88)+M(92)+M(94)+M(98) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(144)+M(200)))
  T4sum(1:15,5) = T4sum(1:15,5) + Gcoeff * G2tensor(:,55)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(48)+M(49)+M(51)+M(60)+M(61)+M(63)+M(68)+M(71)+M(72)+M(74)+M(75)+M(76)+M(80)+M(88)+M(92)+M(98)+M(99) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(134)+M(202)))
  T4sum(1:15,5) = T4sum(1:15,5) + Gcoeff * G2tensor(:,58)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)+M(61)-M(69)+M(72)+M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(134)-M(144)-M(200)+M(202)))
  T4sum(1:15,5) = T4sum(1:15,5) + Gcoeff * G2tensor(:,61)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(43)+M(46)+M(47)+M(48)+M(49)+M(58)+M(59)+M(68)+M(69)+M(71)+M(81)+M(82)+M(85)+M(92)+M(94)+M(95)+M(96)+M(97)+M(98) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(143)+M(242)))
  T4sum(1:15,5) = T4sum(1:15,5) + Gcoeff * G2tensor(:,56)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(46)+M(48)+M(49)+M(58)+M(60)+M(61)+M(68)+M(71)+M(72)+M(80)+M(85)+M(92)+M(95)+M(96)+M(97)+M(98)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(133)+M(244)))
  T4sum(1:15,5) = T4sum(1:15,5) + Gcoeff * G2tensor(:,59)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)+M(61)-M(69)+M(72)+M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(143)-M(242)+M(244)))
  T4sum(1:15,5) = T4sum(1:15,5) + Gcoeff * G2tensor(:,62)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(143)-M(144)-M(200)+M(242)))
  T4sum(1:15,5) = T4sum(1:15,5) + Gcoeff * G2tensor(:,57)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(134)-M(202)+M(244)))
  T4sum(1:15,5) = T4sum(1:15,5) + Gcoeff * G2tensor(:,60)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(133)-M(134)-M(143)+M(144) &
    +M(200)-M(202)-M(242)+M(244)))
  T4sum(1:15,5) = T4sum(1:15,5) + Gcoeff * G2tensor(:,63)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(44)+M(46)+M(47)+M(48)+M(49)+M(50)+M(56)+M(57)+M(59)+M(62)+M(63)+M(64)+M(71)+M(75)+M(84)+M(85)+M(87)+M(95)+M(97)+M(98) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(146)+M(176)))
  T4sum(1:15,7) = T4sum(1:15,7) + Gcoeff * G2tensor(:,64)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(43)+M(44)+M(46)+M(47)+M(48)+M(49)+M(56)+M(57)+M(59)+M(70)+M(71)+M(82)+M(84)+M(85)+M(92)+M(93)+M(94)+M(95)+M(97)+M(98) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(145)+M(236)))
  T4sum(1:15,7) = T4sum(1:15,7) + Gcoeff * G2tensor(:,65)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(145)-M(146)-M(176)+M(236)))
  T4sum(1:15,7) = T4sum(1:15,7) + Gcoeff * G2tensor(:,66)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(44)+M(45)+M(46)+M(47)+M(49)+M(50)+M(56)+M(59)+M(60)+M(62)+M(63)+M(64)+M(72)+M(73)+M(75)+M(83)+M(87)+M(95)+M(98) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(140)+M(178)))
  T4sum(1:15,7) = T4sum(1:15,7) + Gcoeff * G2tensor(:,67)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(43)+M(44)+M(45)+M(46)+M(47)+M(49)+M(56)+M(59)+M(60)+M(70)+M(72)+M(73)+M(82)+M(83)+M(92)+M(93)+M(94)+M(95)+M(98) &
    +M(100)+M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(139)+M(238)))
  T4sum(1:15,7) = T4sum(1:15,7) + Gcoeff * G2tensor(:,68)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(139)-M(140)-M(178)+M(238)))
  T4sum(1:15,7) = T4sum(1:15,7) + Gcoeff * G2tensor(:,69)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(57) &
    +M(60)-M(71)+M(72)+M(73)+M(83)-M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(140)-M(146)-M(176)+M(178)))
  T4sum(1:15,7) = T4sum(1:15,7) + Gcoeff * G2tensor(:,70)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(57) &
    +M(60)-M(71)+M(72)+M(73)+M(83)-M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(139)-M(145)-M(236)+M(238)))
  T4sum(1:15,7) = T4sum(1:15,7) + Gcoeff * G2tensor(:,71)
  Gcoeff = (c(5)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(125)+M(127)+M(129)-M(130))+c(6)*(M(139)-M(140)-M(145)+M(146) &
    +M(176)-M(178)-M(236)+M(238)))
  T4sum(1:15,7) = T4sum(1:15,7) + Gcoeff * G2tensor(:,72)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(48)+M(50)+M(51)+M(52)+M(60)+M(62)+M(68)+M(70)+M(71)+M(72)+M(73)+M(74)+M(80)+M(82)+M(86)+M(93)+M(94) &
    +M(100)+M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(150)+M(194)))
  T4sum(1:15,8) = T4sum(1:15,8) + Gcoeff * G2tensor(:,73)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(48)+M(51)+M(52)+M(60)+M(63)+M(64)+M(68)+M(71)+M(72)+M(73)+M(74)+M(75)+M(80)+M(86)+M(87)+M(92) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(136)+M(196)))
  T4sum(1:15,8) = T4sum(1:15,8) + Gcoeff * G2tensor(:,76)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    +M(63)+M(64)-M(70)+M(75)-M(82)+M(87)+M(92)-M(93)-M(94)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(136)-M(150)-M(194)+M(196)))
  T4sum(1:15,8) = T4sum(1:15,8) + Gcoeff * G2tensor(:,79)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(45)+M(50)+M(51)+M(52)+M(57)+M(62)+M(68)+M(70)+M(74)+M(80)+M(82)+M(83)+M(84)+M(85)+M(86)+M(93)+M(94)+M(97) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(149)+M(218)))
  T4sum(1:15,8) = T4sum(1:15,8) + Gcoeff * G2tensor(:,74)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(45)+M(51)+M(52)+M(57)+M(63)+M(64)+M(68)+M(74)+M(75)+M(80)+M(83)+M(84)+M(85)+M(86)+M(87)+M(92)+M(97) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(135)+M(220)))
  T4sum(1:15,8) = T4sum(1:15,8) + Gcoeff * G2tensor(:,77)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    +M(63)+M(64)-M(70)+M(75)-M(82)+M(87)+M(92)-M(93)-M(94)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(135)-M(149)-M(218)+M(220)))
  T4sum(1:15,8) = T4sum(1:15,8) + Gcoeff * G2tensor(:,80)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(149)-M(150)-M(194)+M(218)))
  T4sum(1:15,8) = T4sum(1:15,8) + Gcoeff * G2tensor(:,75)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(135)-M(136)-M(196)+M(220)))
  T4sum(1:15,8) = T4sum(1:15,8) + Gcoeff * G2tensor(:,78)
  Gcoeff = (c(5)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(125)+M(127)+M(129)-M(130))+c(6)*(M(135)-M(136)-M(149)+M(150) &
    +M(194)-M(196)-M(218)+M(220)))
  T4sum(1:15,8) = T4sum(1:15,8) + Gcoeff * G2tensor(:,81)
  Gcoeff = (c(5)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(131)-M(133)-M(135) &
    +M(136)+M(196)-M(220)-M(244)+M(250))) * den(18)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,82)
  Gcoeff = (c(5)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(140)-M(146)-M(152) &
    +M(154)+M(164)-M(170)-M(176)+M(178))) * den(18)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,83)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(135)-M(136)+M(140)-M(146)-M(152)+M(154)+M(164)-M(170)-M(176)+M(178)-M(196)+M(220)+M(244) &
    -M(250))) * den(18)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,84)
  Gcoeff = (c(5)*(-M(73)+M(76)+M(88)-M(100)-M(110)+M(112)+M(118)-M(124)-M(125)+M(126)+M(128)-M(130))+c(6)*(-M(131)+M(132)+M(134) &
    -M(136)-M(196)+M(202)+M(226)-M(250))) * den(18)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,85)
  Gcoeff = (c(5)*(-M(73)+M(76)+M(88)-M(100)-M(110)+M(112)+M(118)-M(124)-M(125)+M(126)+M(128)-M(130))+c(6)*(-M(140)+M(142)+M(148) &
    -M(154)-M(164)+M(166)+M(172)-M(178))) * den(18)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,86)
  Gcoeff = (c(6)*(M(131)-M(132)-M(134)+M(136)-M(140)+M(142)+M(148)-M(154)-M(164)+M(166)+M(172)-M(178)+M(196)-M(202)-M(226) &
    +M(250))) * den(18)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,87)
  Gcoeff = (c(5)*(-M(76)+M(85)-M(88)+M(97)-M(112)+M(116)-M(118)+M(122)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(132)+M(133)-M(134) &
    +M(135)-M(202)+M(220)-M(226)+M(244))) * den(18)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,88)
  Gcoeff = (c(5)*(-M(76)+M(85)-M(88)+M(97)-M(112)+M(116)-M(118)+M(122)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(142)+M(146)-M(148) &
    +M(152)-M(166)+M(170)-M(172)+M(176))) * den(18)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,89)
  Gcoeff = (c(6)*(M(132)-M(133)+M(134)-M(135)-M(142)+M(146)-M(148)+M(152)-M(166)+M(170)-M(172)+M(176)+M(202)-M(220)+M(226) &
    -M(244))) * den(18)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,90)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(131)-M(132)-M(137)+M(138) &
    +M(224)-M(226)-M(248)+M(250))) * den(126)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,91)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(147)-M(148)-M(153)+M(154) &
    +M(164)-M(166)-M(188)+M(190))) * den(126)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,92)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(126)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,93)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(149)-M(151)-M(153) &
    +M(154)+M(164)-M(188)-M(212)+M(218))) * den(144)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,94)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(131)-M(137)-M(143) &
    +M(145)+M(236)-M(242)-M(248)+M(250))) * den(144)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,95)
  Gcoeff = (c(6)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(144)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,96)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)-M(58)+M(68)-M(69)+M(70)-M(81)+M(84)+M(93)-M(96)+M(101)-M(102)-M(107)+M(108)-M(113)+M(115)+M(119)-M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(149)-M(151)-M(212)+M(218))) * den(12)
  T4sum(1:35,178) = T4sum(1:35,178) + Gcoeff * G3tensor(:,19)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)+M(58)+M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(132)-M(138)-M(224)+M(226))) * den(12)
  T4sum(1:35,178) = T4sum(1:35,178) + Gcoeff * G3tensor(:,20)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(132)-M(138)-M(149) &
    +M(151)+M(212)-M(218)-M(224)+M(226))) * den(12)
  T4sum(1:35,178) = T4sum(1:35,178) + Gcoeff * G3tensor(:,21)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(147)-M(148)-M(166)+M(190))) * den(12)
  T4sum(1:35,179) = T4sum(1:35,179) + Gcoeff * G3tensor(:,22)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)+M(58)+M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(143)-M(145)-M(236)+M(242))) * den(12)
  T4sum(1:35,179) = T4sum(1:35,179) + Gcoeff * G3tensor(:,23)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(143)-M(145)-M(147) &
    +M(148)+M(166)-M(190)-M(236)+M(242))) * den(12)
  T4sum(1:35,179) = T4sum(1:35,179) + Gcoeff * G3tensor(:,24)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(143)-M(145)-M(147) &
    +M(148)+M(166)-M(190)-M(236)+M(242))) * den(309)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,97)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(132)-M(138)-M(149) &
    +M(151)+M(212)-M(218)-M(224)+M(226))) * den(309)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,98)
  Gcoeff = (c(6)*(M(132)-M(138)-M(143)+M(145)+M(147)-M(148)-M(149)+M(151)-M(166)+M(190)+M(212)-M(218)-M(224)+M(226)+M(236) &
    -M(242))) * den(309)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,99)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)+M(58)+M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(143)-M(145)-M(236)+M(242))) * den(12)
  T4sum(1:35,181) = T4sum(1:35,181) + Gcoeff * G3tensor(:,31)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)+M(58)+M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(131)-M(137)-M(248)+M(250))) * den(12)
  T4sum(1:35,181) = T4sum(1:35,181) + Gcoeff * G3tensor(:,32)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(131)-M(137)-M(143) &
    +M(145)+M(236)-M(242)-M(248)+M(250))) * den(12)
  T4sum(1:35,181) = T4sum(1:35,181) + Gcoeff * G3tensor(:,33)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(153)-M(154)-M(164)+M(188))) * den(12)
  T4sum(1:35,182) = T4sum(1:35,182) + Gcoeff * G3tensor(:,34)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)-M(58)+M(68)-M(69)+M(70)-M(81)+M(84)+M(93)-M(96)+M(101)-M(102)-M(107)+M(108)-M(113)+M(115)+M(119)-M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(149)-M(151)-M(212)+M(218))) * den(12)
  T4sum(1:35,182) = T4sum(1:35,182) + Gcoeff * G3tensor(:,35)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(149)-M(151)-M(153) &
    +M(154)+M(164)-M(188)-M(212)+M(218))) * den(12)
  T4sum(1:35,182) = T4sum(1:35,182) + Gcoeff * G3tensor(:,36)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(153)-M(154)-M(164)+M(188))) * den(12)
  T4sum(1:35,1) = T4sum(1:35,1) + Gcoeff * G3tensor(:,1)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(147)-M(148)-M(166)+M(190))) * den(12)
  T4sum(1:35,1) = T4sum(1:35,1) + Gcoeff * G3tensor(:,2)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(147)-M(148)-M(153)+M(154) &
    +M(164)-M(166)-M(188)+M(190))) * den(12)
  T4sum(1:35,1) = T4sum(1:35,1) + Gcoeff * G3tensor(:,3)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)+M(58)+M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(132)-M(138)-M(224)+M(226))) * den(12)
  T4sum(1:35,2) = T4sum(1:35,2) + Gcoeff * G3tensor(:,43)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)+M(58)+M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(131)-M(137)-M(248)+M(250))) * den(12)
  T4sum(1:35,2) = T4sum(1:35,2) + Gcoeff * G3tensor(:,44)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(131)-M(132)-M(137)+M(138) &
    +M(224)-M(226)-M(248)+M(250))) * den(12)
  T4sum(1:35,2) = T4sum(1:35,2) + Gcoeff * G3tensor(:,45)
  Gcoeff = (c(6)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(302)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,46)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(302)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,47)
  Gcoeff = (c(6)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(302)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,48)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(133)-M(134)-M(143)+M(144) &
    +M(200)-M(202)-M(242)+M(244))) * den(129)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,100)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(141)-M(142)-M(151)+M(152) &
    +M(170)-M(172)-M(212)+M(214))) * den(129)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,101)
  Gcoeff = (c(6)*(-M(133)+M(134)+M(141)-M(142)+M(143)-M(144)-M(151)+M(152)+M(170)-M(172)-M(200)+M(202)-M(212)+M(214)+M(242) &
    -M(244))) * den(129)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,102)
  Gcoeff = (c(5)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(6)*(-M(150)+M(151)-M(152) &
    +M(153)-M(170)+M(188)-M(194)+M(212))) * den(149)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,103)
  Gcoeff = (c(5)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(6)*(-M(133)+M(137)-M(139) &
    +M(143)-M(238)+M(242)-M(244)+M(248))) * den(149)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,104)
  Gcoeff = (c(6)*(-M(133)+M(137)-M(139)+M(143)+M(150)-M(151)+M(152)-M(153)+M(170)-M(188)+M(194)-M(212)-M(238)+M(242)-M(244) &
    +M(248))) * den(149)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,105)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)-M(61)-M(69)+M(72)+M(80)-M(81)+M(82)+M(94)-M(99)+M(103)-M(104)-M(107)+M(109)-M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(150)-M(153)-M(188)+M(194))) * den(14)
  T4sum(1:35,190) = T4sum(1:35,190) + Gcoeff * G3tensor(:,49)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)+M(61)-M(69)+M(72)+M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(134)-M(144)-M(200)+M(202))) * den(14)
  T4sum(1:35,190) = T4sum(1:35,190) + Gcoeff * G3tensor(:,50)
  Gcoeff = (c(5)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(134)-M(144)-M(150) &
    +M(153)+M(188)-M(194)-M(200)+M(202))) * den(14)
  T4sum(1:35,190) = T4sum(1:35,190) + Gcoeff * G3tensor(:,51)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    -M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(141)-M(142)-M(172)+M(214))) * den(14)
  T4sum(1:35,191) = T4sum(1:35,191) + Gcoeff * G3tensor(:,52)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    -M(60)+M(61)+M(69)-M(72)+M(80)+M(81)-M(82)-M(94)+M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(137)-M(139)-M(238)+M(248))) * den(14)
  T4sum(1:35,191) = T4sum(1:35,191) + Gcoeff * G3tensor(:,53)
  Gcoeff = (c(5)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(137)-M(139)-M(141) &
    +M(142)+M(172)-M(214)-M(238)+M(248))) * den(14)
  T4sum(1:35,191) = T4sum(1:35,191) + Gcoeff * G3tensor(:,54)
  Gcoeff = (c(5)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(125)+M(127)+M(129)-M(130))+c(6)*(M(135)-M(136)-M(149)+M(150) &
    +M(194)-M(196)-M(218)+M(220))) * den(131)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,106)
  Gcoeff = (c(5)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(125)+M(127)+M(129)-M(130))+c(6)*(M(139)-M(140)-M(145)+M(146) &
    +M(176)-M(178)-M(236)+M(238))) * den(131)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,107)
  Gcoeff = (c(6)*(-M(135)+M(136)+M(139)-M(140)-M(145)+M(146)+M(149)-M(150)+M(176)-M(178)-M(194)+M(196)+M(218)-M(220)-M(236) &
    +M(238))) * den(131)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,108)
  Gcoeff = (c(5)*(M(57)-M(60)-M(72)+M(84)+M(101)-M(103)-M(109)+M(115)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(149)-M(150)-M(152) &
    +M(154)+M(164)-M(170)-M(194)+M(218))) * den(154)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,109)
  Gcoeff = (c(5)*(M(57)-M(60)-M(72)+M(84)+M(101)-M(103)-M(109)+M(115)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(131)-M(133)-M(139) &
    +M(145)+M(236)-M(238)-M(244)+M(250))) * den(154)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,110)
  Gcoeff = (c(6)*(M(131)-M(133)-M(139)+M(145)-M(149)+M(150)+M(152)-M(154)-M(164)+M(170)+M(194)-M(218)+M(236)-M(238)-M(244) &
    +M(250))) * den(154)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,111)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(57) &
    +M(60)-M(71)+M(72)-M(73)+M(83)-M(84)+M(85)+M(97)-M(100)-M(101)+M(103)+M(109)-M(110)-M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(152)-M(154)-M(164)+M(170))) * den(20)
  T4sum(1:35,193) = T4sum(1:35,193) + Gcoeff * G3tensor(:,61)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(57) &
    +M(60)-M(71)+M(72)+M(73)+M(83)-M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(140)-M(146)-M(176)+M(178))) * den(20)
  T4sum(1:35,193) = T4sum(1:35,193) + Gcoeff * G3tensor(:,62)
  Gcoeff = (c(5)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(140)-M(146)-M(152) &
    +M(154)+M(164)-M(170)-M(176)+M(178))) * den(20)
  T4sum(1:35,193) = T4sum(1:35,193) + Gcoeff * G3tensor(:,63)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(135)-M(136)-M(196)+M(220))) * den(20)
  T4sum(1:35,194) = T4sum(1:35,194) + Gcoeff * G3tensor(:,64)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)+M(73)+M(83)+M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)+M(110)+M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(131)-M(133)-M(244)+M(250))) * den(20)
  T4sum(1:35,194) = T4sum(1:35,194) + Gcoeff * G3tensor(:,65)
  Gcoeff = (c(5)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(131)-M(133)-M(135) &
    +M(136)+M(196)-M(220)-M(244)+M(250))) * den(20)
  T4sum(1:35,194) = T4sum(1:35,194) + Gcoeff * G3tensor(:,66)
  Gcoeff = (c(5)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(6)*(-M(144)+M(145)-M(146) &
    +M(147)-M(176)+M(190)-M(200)+M(236))) * den(157)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,112)
  Gcoeff = (c(5)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(6)*(-M(135)+M(138)-M(141) &
    +M(149)-M(214)+M(218)-M(220)+M(224))) * den(157)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,113)
  Gcoeff = (c(6)*(-M(135)+M(138)-M(141)+M(144)-M(145)+M(146)-M(147)+M(149)+M(176)-M(190)+M(200)-M(214)+M(218)-M(220)+M(224) &
    -M(236))) * den(157)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,114)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    +M(63)-M(64)-M(70)+M(75)+M(82)-M(87)+M(92)-M(93)+M(94)+M(105)-M(106)-M(108)+M(111)+M(114)-M(117)-M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(144)-M(147)-M(190)+M(200))) * den(16)
  T4sum(1:35,190) = T4sum(1:35,190) + Gcoeff * G3tensor(:,73)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    +M(63)+M(64)-M(70)+M(75)-M(82)+M(87)+M(92)-M(93)-M(94)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(136)-M(150)-M(194)+M(196))) * den(16)
  T4sum(1:35,190) = T4sum(1:35,190) + Gcoeff * G3tensor(:,74)
  Gcoeff = (c(5)*(M(64)-M(82)+M(87)-M(94)+M(106)-M(114)+M(117)-M(120)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(136)-M(144)+M(147) &
    -M(150)+M(190)-M(194)+M(196)-M(200))) * den(16)
  T4sum(1:35,190) = T4sum(1:35,190) + Gcoeff * G3tensor(:,75)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(139)-M(140)-M(178)+M(238))) * den(16)
  T4sum(1:35,191) = T4sum(1:35,191) + Gcoeff * G3tensor(:,55)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)+M(64)+M(70)-M(75)-M(82)+M(87)+M(92)+M(93)-M(94)-M(105)+M(106)+M(108)-M(111)-M(114)+M(117)+M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(138)-M(141)-M(214)+M(224))) * den(16)
  T4sum(1:35,191) = T4sum(1:35,191) + Gcoeff * G3tensor(:,56)
  Gcoeff = (c(5)*(M(64)-M(82)+M(87)-M(94)+M(106)-M(114)+M(117)-M(120)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(138)-M(139)+M(140) &
    -M(141)+M(178)-M(214)+M(224)-M(238))) * den(16)
  T4sum(1:35,191) = T4sum(1:35,191) + Gcoeff * G3tensor(:,57)
  Gcoeff = (c(5)*(M(58)-M(63)-M(75)+M(96)+M(102)-M(105)-M(111)+M(121)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(143)-M(144)-M(146) &
    +M(148)+M(166)-M(176)-M(200)+M(242))) * den(160)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,115)
  Gcoeff = (c(5)*(M(58)-M(63)-M(75)+M(96)+M(102)-M(105)-M(111)+M(121)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(132)-M(135)-M(141) &
    +M(151)+M(212)-M(214)-M(220)+M(226))) * den(160)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,116)
  Gcoeff = (c(6)*(M(132)-M(135)-M(141)-M(143)+M(144)+M(146)-M(148)+M(151)-M(166)+M(176)+M(200)+M(212)-M(214)-M(220)+M(226) &
    -M(242))) * den(160)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,117)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(58) &
    +M(63)-M(74)+M(75)-M(76)+M(85)-M(88)+M(95)-M(96)+M(97)-M(102)+M(105)+M(111)-M(112)+M(116)-M(118)-M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(146)-M(148)-M(166)+M(176))) * den(22)
  T4sum(1:35,193) = T4sum(1:35,193) + Gcoeff * G3tensor(:,76)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(58) &
    +M(63)-M(74)+M(75)+M(76)-M(85)+M(88)+M(95)-M(96)-M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(142)-M(152)-M(170)+M(172))) * den(22)
  T4sum(1:35,193) = T4sum(1:35,193) + Gcoeff * G3tensor(:,77)
  Gcoeff = (c(5)*(M(76)-M(85)+M(88)-M(97)+M(112)-M(116)+M(118)-M(122)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(142)-M(146)+M(148) &
    -M(152)+M(166)-M(170)+M(172)-M(176))) * den(22)
  T4sum(1:35,193) = T4sum(1:35,193) + Gcoeff * G3tensor(:,78)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(134)-M(202)+M(244))) * den(22)
  T4sum(1:35,194) = T4sum(1:35,194) + Gcoeff * G3tensor(:,67)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)+M(76)-M(85)+M(88)+M(95)+M(96)-M(97)+M(102)-M(105)-M(111)+M(112)-M(116)+M(118)+M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(132)-M(135)-M(220)+M(226))) * den(22)
  T4sum(1:35,194) = T4sum(1:35,194) + Gcoeff * G3tensor(:,68)
  Gcoeff = (c(5)*(M(76)-M(85)+M(88)-M(97)+M(112)-M(116)+M(118)-M(122)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(132)-M(133)+M(134) &
    -M(135)+M(202)-M(220)+M(226)-M(244))) * den(22)
  T4sum(1:35,194) = T4sum(1:35,194) + Gcoeff * G3tensor(:,69)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(44)+M(45)+M(46)+M(47)+M(50)+M(52)+M(56)+M(59)+M(60)+M(61)+M(62)+M(63)+M(72)+M(75)+M(76)+M(83)+M(86)+M(88)+M(95)+M(99) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(142)+M(172)))
  T5sum(1:70,1) = T5sum(1:70,1) + Gcoeff * G4tensor(:,86)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(44)+M(45)+M(46)+M(47)+M(49)+M(50)+M(56)+M(59)+M(60)+M(62)+M(63)+M(64)+M(72)+M(73)+M(75)+M(83)+M(87)+M(95)+M(98) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(140)+M(178)))
  T5sum(1:70,1) = T5sum(1:70,1) + Gcoeff * G4tensor(:,87)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(61) &
    +M(64)+M(73)-M(76)-M(86)+M(87)-M(88)+M(98)-M(99)+M(100)-M(104)+M(106)+M(110)-M(112)+M(117)-M(118)-M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(140)-M(142)-M(172)+M(178)))
  T5sum(1:70,1) = T5sum(1:70,1) + Gcoeff * G4tensor(:,88)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(44)+M(47)+M(48)+M(50)+M(51)+M(52)+M(56)+M(57)+M(58)+M(59)+M(61)+M(62)+M(71)+M(73)+M(74)+M(84)+M(86)+M(96)+M(99) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(154)+M(164)))
  T5sum(1:70,2) = T5sum(1:70,2) + Gcoeff * G4tensor(:,1)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(44)+M(47)+M(48)+M(49)+M(50)+M(51)+M(56)+M(57)+M(58)+M(59)+M(62)+M(64)+M(71)+M(74)+M(76)+M(84)+M(87)+M(88)+M(96)+M(98) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(148)+M(166)))
  T5sum(1:70,2) = T5sum(1:70,2) + Gcoeff * G4tensor(:,3)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(61) &
    +M(64)-M(73)+M(76)-M(86)+M(87)+M(88)+M(98)-M(99)-M(100)-M(104)+M(106)-M(110)+M(112)+M(117)+M(118)-M(123)-M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(148)-M(154)-M(164)+M(166)))
  T5sum(1:70,2) = T5sum(1:70,2) + Gcoeff * G4tensor(:,5)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(48)+M(51)+M(52)+M(60)+M(63)+M(64)+M(68)+M(71)+M(72)+M(73)+M(74)+M(75)+M(80)+M(86)+M(87)+M(92) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(136)+M(196)))
  T5sum(1:70,3) = T5sum(1:70,3) + Gcoeff * G4tensor(:,110)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(48)+M(49)+M(51)+M(60)+M(61)+M(63)+M(68)+M(71)+M(72)+M(74)+M(75)+M(76)+M(80)+M(88)+M(92)+M(98)+M(99) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(134)+M(202)))
  T5sum(1:70,3) = T5sum(1:70,3) + Gcoeff * G4tensor(:,111)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)+M(61) &
    -M(64)-M(73)+M(76)-M(86)-M(87)+M(88)+M(98)+M(99)-M(100)+M(104)-M(106)-M(110)+M(112)-M(117)+M(118)+M(123)-M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(134)-M(136)-M(196)+M(202)))
  T5sum(1:70,3) = T5sum(1:70,3) + Gcoeff * G4tensor(:,112)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(47)+M(48)+M(50)+M(51)+M(52)+M(59)+M(61)+M(62)+M(68)+M(69)+M(70)+M(71)+M(73)+M(74)+M(81)+M(86)+M(93)+M(99) &
    +M(100)+M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(153)+M(188)))
  T5sum(1:70,4) = T5sum(1:70,4) + Gcoeff * G4tensor(:,2)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(47)+M(48)+M(49)+M(50)+M(51)+M(59)+M(62)+M(64)+M(68)+M(69)+M(70)+M(71)+M(74)+M(76)+M(81)+M(87)+M(88)+M(93)+M(98) &
    +M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(147)+M(190)))
  T5sum(1:70,4) = T5sum(1:70,4) + Gcoeff * G4tensor(:,4)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(61) &
    +M(64)-M(73)+M(76)-M(86)+M(87)+M(88)+M(98)-M(99)-M(100)-M(104)+M(106)-M(110)+M(112)+M(117)+M(118)-M(123)-M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(147)-M(153)-M(188)+M(190)))
  T5sum(1:70,4) = T5sum(1:70,4) + Gcoeff * G4tensor(:,6)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(45)+M(46)+M(52)+M(57)+M(58)+M(64)+M(68)+M(76)+M(80)+M(83)+M(84)+M(86)+M(87)+M(88)+M(92)+M(95)+M(96) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(132)+M(226)))
  T5sum(1:70,5) = T5sum(1:70,5) + Gcoeff * G4tensor(:,113)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(45)+M(46)+M(49)+M(57)+M(58)+M(61)+M(68)+M(73)+M(80)+M(83)+M(84)+M(92)+M(95)+M(96)+M(98)+M(99) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(131)+M(250)))
  T5sum(1:70,5) = T5sum(1:70,5) + Gcoeff * G4tensor(:,114)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)+M(61) &
    -M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(131)-M(132)-M(226)+M(250)))
  T5sum(1:70,5) = T5sum(1:70,5) + Gcoeff * G4tensor(:,115)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(42)+M(43)+M(44)+M(45)+M(46)+M(52)+M(56)+M(64)+M(69)+M(70)+M(76)+M(80)+M(81)+M(83)+M(86)+M(87)+M(88)+M(92)+M(93)+M(95) &
    +M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(138)+M(224)))
  T5sum(1:70,6) = T5sum(1:70,6) + Gcoeff * G4tensor(:,89)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(43)+M(44)+M(45)+M(46)+M(49)+M(56)+M(61)+M(69)+M(70)+M(73)+M(80)+M(81)+M(83)+M(92)+M(93)+M(95)+M(98)+M(99) &
    +M(100)+M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(137)+M(248)))
  T5sum(1:70,6) = T5sum(1:70,6) + Gcoeff * G4tensor(:,90)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)+M(61) &
    -M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(137)-M(138)-M(224)+M(248)))
  T5sum(1:70,6) = T5sum(1:70,6) + Gcoeff * G4tensor(:,91)
  Gcoeff = (c(5)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(137)-M(139)-M(141) &
    +M(142)+M(172)-M(214)-M(238)+M(248))) * den(325)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,118)
  Gcoeff = (c(5)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(134)-M(144)-M(150) &
    +M(153)+M(188)-M(194)-M(200)+M(202))) * den(325)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,119)
  Gcoeff = (c(6)*(M(134)-M(137)+M(139)+M(141)-M(142)-M(144)-M(150)+M(153)-M(172)+M(188)-M(194)-M(200)+M(202)+M(214)+M(238) &
    -M(248))) * den(325)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,120)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    -M(60)+M(61)+M(69)-M(72)+M(80)+M(81)-M(82)-M(94)+M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(137)-M(139)-M(238)+M(248))) * den(14)
  T4sum(1:35,181) = T4sum(1:35,181) + Gcoeff * G3tensor(:,79)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)+M(61)-M(69)+M(72)+M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(143)-M(242)+M(244))) * den(14)
  T4sum(1:35,181) = T4sum(1:35,181) + Gcoeff * G3tensor(:,80)
  Gcoeff = (c(5)*(M(60)-M(69)+M(72)-M(81)+M(103)-M(107)+M(109)-M(113)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(133)-M(137)+M(139) &
    -M(143)+M(238)-M(242)+M(244)-M(248))) * den(14)
  T4sum(1:35,181) = T4sum(1:35,181) + Gcoeff * G3tensor(:,81)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    -M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(151)-M(152)-M(170)+M(212))) * den(14)
  T4sum(1:35,182) = T4sum(1:35,182) + Gcoeff * G3tensor(:,37)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)-M(61)-M(69)+M(72)+M(80)-M(81)+M(82)+M(94)-M(99)+M(103)-M(104)-M(107)+M(109)-M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(150)-M(153)-M(188)+M(194))) * den(14)
  T4sum(1:35,182) = T4sum(1:35,182) + Gcoeff * G3tensor(:,38)
  Gcoeff = (c(5)*(M(60)-M(69)+M(72)-M(81)+M(103)-M(107)+M(109)-M(113)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(150)-M(151)+M(152) &
    -M(153)+M(170)-M(188)+M(194)-M(212))) * den(14)
  T4sum(1:35,182) = T4sum(1:35,182) + Gcoeff * G3tensor(:,39)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    -M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(151)-M(152)-M(170)+M(212))) * den(14)
  T4sum(1:35,4) = T4sum(1:35,4) + Gcoeff * G3tensor(:,7)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    -M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(141)-M(142)-M(172)+M(214))) * den(14)
  T4sum(1:35,4) = T4sum(1:35,4) + Gcoeff * G3tensor(:,8)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(141)-M(142)-M(151)+M(152) &
    +M(170)-M(172)-M(212)+M(214))) * den(14)
  T4sum(1:35,4) = T4sum(1:35,4) + Gcoeff * G3tensor(:,9)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)+M(61)-M(69)+M(72)+M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(134)-M(144)-M(200)+M(202))) * den(14)
  T4sum(1:35,5) = T4sum(1:35,5) + Gcoeff * G3tensor(:,82)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)+M(61)-M(69)+M(72)+M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(143)-M(242)+M(244))) * den(14)
  T4sum(1:35,5) = T4sum(1:35,5) + Gcoeff * G3tensor(:,83)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(133)-M(134)-M(143)+M(144) &
    +M(200)-M(202)-M(242)+M(244))) * den(14)
  T4sum(1:35,5) = T4sum(1:35,5) + Gcoeff * G3tensor(:,84)
  Gcoeff = (c(6)*(-M(133)+M(137)-M(139)+M(143)+M(150)-M(151)+M(152)-M(153)+M(170)-M(188)+M(194)-M(212)-M(238)+M(242)-M(244) &
    +M(248))) * den(311)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,85)
  Gcoeff = (c(6)*(M(134)-M(137)+M(139)+M(141)-M(142)-M(144)-M(150)+M(153)-M(172)+M(188)-M(194)-M(200)+M(202)+M(214)+M(238) &
    -M(248))) * den(311)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,86)
  Gcoeff = (c(6)*(M(133)-M(134)-M(141)+M(142)-M(143)+M(144)+M(151)-M(152)-M(170)+M(172)+M(200)-M(202)+M(212)-M(214)-M(242) &
    +M(244))) * den(311)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,87)
  Gcoeff = (c(5)*(-M(64)+M(82)-M(87)+M(94)-M(106)+M(114)-M(117)+M(120)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(138)+M(139)-M(140) &
    +M(141)-M(178)+M(214)-M(224)+M(238))) * den(328)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,121)
  Gcoeff = (c(5)*(-M(64)+M(82)-M(87)+M(94)-M(106)+M(114)-M(117)+M(120)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(136)+M(144)-M(147) &
    +M(150)-M(190)+M(194)-M(196)+M(200))) * den(328)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,122)
  Gcoeff = (c(6)*(-M(136)+M(138)-M(139)+M(140)-M(141)+M(144)-M(147)+M(150)+M(178)-M(190)+M(194)-M(196)+M(200)-M(214)+M(224) &
    -M(238))) * den(328)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,123)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)+M(64)+M(70)-M(75)-M(82)+M(87)+M(92)+M(93)-M(94)-M(105)+M(106)+M(108)-M(111)-M(114)+M(117)+M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(138)-M(141)-M(214)+M(224))) * den(16)
  T4sum(1:35,178) = T4sum(1:35,178) + Gcoeff * G3tensor(:,88)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    +M(63)+M(64)-M(70)+M(75)-M(82)+M(87)+M(92)-M(93)-M(94)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(135)-M(149)-M(218)+M(220))) * den(16)
  T4sum(1:35,178) = T4sum(1:35,178) + Gcoeff * G3tensor(:,89)
  Gcoeff = (c(5)*(M(63)-M(70)+M(75)-M(93)+M(105)-M(108)+M(111)-M(119)-M(125)+M(127)+M(129)-M(130))+c(6)*(M(135)-M(138)+M(141) &
    -M(149)+M(214)-M(218)+M(220)-M(224))) * den(16)
  T4sum(1:35,178) = T4sum(1:35,178) + Gcoeff * G3tensor(:,90)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(145)-M(146)-M(176)+M(236))) * den(16)
  T4sum(1:35,179) = T4sum(1:35,179) + Gcoeff * G3tensor(:,25)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    +M(63)-M(64)-M(70)+M(75)+M(82)-M(87)+M(92)-M(93)+M(94)+M(105)-M(106)-M(108)+M(111)+M(114)-M(117)-M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(144)-M(147)-M(190)+M(200))) * den(16)
  T4sum(1:35,179) = T4sum(1:35,179) + Gcoeff * G3tensor(:,26)
  Gcoeff = (c(5)*(M(63)-M(70)+M(75)-M(93)+M(105)-M(108)+M(111)-M(119)-M(125)+M(127)+M(129)-M(130))+c(6)*(M(144)-M(145)+M(146) &
    -M(147)+M(176)-M(190)+M(200)-M(236))) * den(16)
  T4sum(1:35,179) = T4sum(1:35,179) + Gcoeff * G3tensor(:,27)
  Gcoeff = (c(5)*(M(61)-M(64)-M(87)+M(99)+M(104)-M(106)-M(117)+M(123)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(137)-M(138)-M(140) &
    +M(142)+M(172)-M(178)-M(224)+M(248))) * den(163)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,124)
  Gcoeff = (c(5)*(M(61)-M(64)-M(87)+M(99)+M(104)-M(106)-M(117)+M(123)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(134)-M(136)-M(147) &
    +M(153)+M(188)-M(190)-M(196)+M(202))) * den(163)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,125)
  Gcoeff = (c(6)*(M(134)-M(136)-M(137)+M(138)+M(140)-M(142)-M(147)+M(153)-M(172)+M(178)+M(188)-M(190)-M(196)+M(202)+M(224) &
    -M(248))) * den(163)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,126)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(61) &
    +M(64)+M(73)-M(76)-M(86)+M(87)-M(88)+M(98)-M(99)+M(100)-M(104)+M(106)+M(110)-M(112)+M(117)-M(118)-M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(140)-M(142)-M(172)+M(178))) * den(26)
  T4sum(1:35,193) = T4sum(1:35,193) + Gcoeff * G3tensor(:,91)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(61) &
    +M(64)-M(73)+M(76)-M(86)+M(87)+M(88)+M(98)-M(99)-M(100)-M(104)+M(106)-M(110)+M(112)+M(117)+M(118)-M(123)-M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(148)-M(154)-M(164)+M(166))) * den(26)
  T4sum(1:35,193) = T4sum(1:35,193) + Gcoeff * G3tensor(:,92)
  Gcoeff = (c(5)*(-M(73)+M(76)+M(88)-M(100)-M(110)+M(112)+M(118)-M(124)-M(125)+M(126)+M(128)-M(130))+c(6)*(-M(140)+M(142)+M(148) &
    -M(154)-M(164)+M(166)+M(172)-M(178))) * den(26)
  T4sum(1:35,193) = T4sum(1:35,193) + Gcoeff * G3tensor(:,93)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)+M(61) &
    -M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(131)-M(132)-M(226)+M(250))) * den(26)
  T4sum(1:35,194) = T4sum(1:35,194) + Gcoeff * G3tensor(:,70)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)+M(61) &
    -M(64)-M(73)+M(76)-M(86)-M(87)+M(88)+M(98)+M(99)-M(100)+M(104)-M(106)-M(110)+M(112)-M(117)+M(118)+M(123)-M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(134)-M(136)-M(196)+M(202))) * den(26)
  T4sum(1:35,194) = T4sum(1:35,194) + Gcoeff * G3tensor(:,71)
  Gcoeff = (c(5)*(-M(73)+M(76)+M(88)-M(100)-M(110)+M(112)+M(118)-M(124)-M(125)+M(126)+M(128)-M(130))+c(6)*(-M(131)+M(132)+M(134) &
    -M(136)-M(196)+M(202)+M(226)-M(250))) * den(26)
  T4sum(1:35,194) = T4sum(1:35,194) + Gcoeff * G3tensor(:,72)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(44)+M(47)+M(48)+M(49)+M(50)+M(51)+M(56)+M(57)+M(58)+M(59)+M(62)+M(64)+M(71)+M(74)+M(76)+M(84)+M(87)+M(88)+M(96)+M(98) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(148)+M(166)))
  T5sum(1:70,13) = T5sum(1:70,13) + Gcoeff * G4tensor(:,37)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(44)+M(46)+M(47)+M(48)+M(49)+M(50)+M(56)+M(57)+M(59)+M(62)+M(63)+M(64)+M(71)+M(75)+M(84)+M(85)+M(87)+M(95)+M(97)+M(98) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(146)+M(176)))
  T5sum(1:70,13) = T5sum(1:70,13) + Gcoeff * G4tensor(:,38)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(58) &
    +M(63)-M(74)+M(75)-M(76)+M(85)-M(88)+M(95)-M(96)+M(97)-M(102)+M(105)+M(111)-M(112)+M(116)-M(118)-M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(146)-M(148)-M(166)+M(176)))
  T5sum(1:70,13) = T5sum(1:70,13) + Gcoeff * G4tensor(:,39)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(44)+M(45)+M(47)+M(50)+M(51)+M(52)+M(56)+M(58)+M(59)+M(60)+M(61)+M(62)+M(72)+M(74)+M(83)+M(85)+M(86)+M(96)+M(97)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(152)+M(170)))
  T5sum(1:70,14) = T5sum(1:70,14) + Gcoeff * G4tensor(:,13)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(44)+M(45)+M(46)+M(47)+M(50)+M(52)+M(56)+M(59)+M(60)+M(61)+M(62)+M(63)+M(72)+M(75)+M(76)+M(83)+M(86)+M(88)+M(95)+M(99) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(142)+M(172)))
  T5sum(1:70,14) = T5sum(1:70,14) + Gcoeff * G4tensor(:,15)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(58) &
    +M(63)-M(74)+M(75)+M(76)-M(85)+M(88)+M(95)-M(96)-M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(142)-M(152)-M(170)+M(172)))
  T5sum(1:70,14) = T5sum(1:70,14) + Gcoeff * G4tensor(:,17)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(45)+M(51)+M(52)+M(57)+M(63)+M(64)+M(68)+M(74)+M(75)+M(80)+M(83)+M(84)+M(85)+M(86)+M(87)+M(92)+M(97) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(135)+M(220)))
  T5sum(1:70,15) = T5sum(1:70,15) + Gcoeff * G4tensor(:,116)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(45)+M(46)+M(52)+M(57)+M(58)+M(64)+M(68)+M(76)+M(80)+M(83)+M(84)+M(86)+M(87)+M(88)+M(92)+M(95)+M(96) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(132)+M(226)))
  T5sum(1:70,15) = T5sum(1:70,15) + Gcoeff * G4tensor(:,117)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)+M(76)-M(85)+M(88)+M(95)+M(96)-M(97)+M(102)-M(105)-M(111)+M(112)-M(116)+M(118)+M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(132)-M(135)-M(220)+M(226)))
  T5sum(1:70,15) = T5sum(1:70,15) + Gcoeff * G4tensor(:,118)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(44)+M(45)+M(50)+M(51)+M(52)+M(56)+M(58)+M(62)+M(69)+M(74)+M(80)+M(81)+M(82)+M(83)+M(85)+M(86)+M(94)+M(96)+M(97) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(151)+M(212)))
  T5sum(1:70,16) = T5sum(1:70,16) + Gcoeff * G4tensor(:,14)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(42)+M(44)+M(45)+M(46)+M(50)+M(52)+M(56)+M(62)+M(63)+M(69)+M(75)+M(76)+M(80)+M(81)+M(82)+M(83)+M(86)+M(88)+M(94)+M(95) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(141)+M(214)))
  T5sum(1:70,16) = T5sum(1:70,16) + Gcoeff * G4tensor(:,16)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(58) &
    +M(63)-M(74)+M(75)+M(76)-M(85)+M(88)+M(95)-M(96)-M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(141)-M(151)-M(212)+M(214)))
  T5sum(1:70,16) = T5sum(1:70,16) + Gcoeff * G4tensor(:,18)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(48)+M(49)+M(51)+M(60)+M(61)+M(63)+M(68)+M(71)+M(72)+M(74)+M(75)+M(76)+M(80)+M(88)+M(92)+M(98)+M(99) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(134)+M(202)))
  T5sum(1:70,17) = T5sum(1:70,17) + Gcoeff * G4tensor(:,119)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(46)+M(48)+M(49)+M(58)+M(60)+M(61)+M(68)+M(71)+M(72)+M(80)+M(85)+M(92)+M(95)+M(96)+M(97)+M(98)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(133)+M(244)))
  T5sum(1:70,17) = T5sum(1:70,17) + Gcoeff * G4tensor(:,120)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(134)-M(202)+M(244)))
  T5sum(1:70,17) = T5sum(1:70,17) + Gcoeff * G4tensor(:,121)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(41)+M(43)+M(47)+M(48)+M(49)+M(51)+M(59)+M(63)+M(68)+M(69)+M(71)+M(74)+M(75)+M(76)+M(81)+M(82)+M(88)+M(92)+M(94)+M(98) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(144)+M(200)))
  T5sum(1:70,18) = T5sum(1:70,18) + Gcoeff * G4tensor(:,40)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(43)+M(46)+M(47)+M(48)+M(49)+M(58)+M(59)+M(68)+M(69)+M(71)+M(81)+M(82)+M(85)+M(92)+M(94)+M(95)+M(96)+M(97)+M(98) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(143)+M(242)))
  T5sum(1:70,18) = T5sum(1:70,18) + Gcoeff * G4tensor(:,41)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(143)-M(144)-M(200)+M(242)))
  T5sum(1:70,18) = T5sum(1:70,18) + Gcoeff * G4tensor(:,42)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(145)-M(146)-M(176)+M(236))) * den(16)
  T4sum(1:35,7) = T4sum(1:35,7) + Gcoeff * G3tensor(:,13)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(139)-M(140)-M(178)+M(238))) * den(16)
  T4sum(1:35,7) = T4sum(1:35,7) + Gcoeff * G3tensor(:,14)
  Gcoeff = (c(5)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(125)+M(127)+M(129)-M(130))+c(6)*(M(139)-M(140)-M(145)+M(146) &
    +M(176)-M(178)-M(236)+M(238))) * den(16)
  T4sum(1:35,7) = T4sum(1:35,7) + Gcoeff * G3tensor(:,15)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    +M(63)+M(64)-M(70)+M(75)-M(82)+M(87)+M(92)-M(93)-M(94)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(136)-M(150)-M(194)+M(196))) * den(16)
  T4sum(1:35,8) = T4sum(1:35,8) + Gcoeff * G3tensor(:,94)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    +M(63)+M(64)-M(70)+M(75)-M(82)+M(87)+M(92)-M(93)-M(94)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(135)-M(149)-M(218)+M(220))) * den(16)
  T4sum(1:35,8) = T4sum(1:35,8) + Gcoeff * G3tensor(:,95)
  Gcoeff = (c(5)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(125)+M(127)+M(129)-M(130))+c(6)*(M(135)-M(136)-M(149)+M(150) &
    +M(194)-M(196)-M(218)+M(220))) * den(16)
  T4sum(1:35,8) = T4sum(1:35,8) + Gcoeff * G3tensor(:,96)
  Gcoeff = (c(6)*(-M(135)+M(136)+M(139)-M(140)-M(145)+M(146)+M(149)-M(150)+M(176)-M(178)-M(194)+M(196)+M(218)-M(220)-M(236) &
    +M(238))) * den(316)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,97)
  Gcoeff = (c(6)*(-M(136)+M(138)-M(139)+M(140)-M(141)+M(144)-M(147)+M(150)+M(178)-M(190)+M(194)-M(196)+M(200)-M(214)+M(224) &
    -M(238))) * den(316)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,98)
  Gcoeff = (c(6)*(M(135)-M(138)+M(141)-M(144)+M(145)-M(146)+M(147)-M(149)-M(176)+M(190)-M(200)+M(214)-M(218)+M(220)-M(224) &
    +M(236))) * den(316)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,99)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(44)+M(47)+M(48)+M(50)+M(51)+M(52)+M(56)+M(57)+M(58)+M(59)+M(61)+M(62)+M(71)+M(73)+M(74)+M(84)+M(86)+M(96)+M(99) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(154)+M(164)))
  T5sum(1:70,25) = T5sum(1:70,25) + Gcoeff * G4tensor(:,61)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(44)+M(45)+M(47)+M(50)+M(51)+M(52)+M(56)+M(58)+M(59)+M(60)+M(61)+M(62)+M(72)+M(74)+M(83)+M(85)+M(86)+M(96)+M(97)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(152)+M(170)))
  T5sum(1:70,25) = T5sum(1:70,25) + Gcoeff * G4tensor(:,62)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(57) &
    +M(60)-M(71)+M(72)-M(73)+M(83)-M(84)+M(85)+M(97)-M(100)-M(101)+M(103)+M(109)-M(110)-M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(152)-M(154)-M(164)+M(170)))
  T5sum(1:70,25) = T5sum(1:70,25) + Gcoeff * G4tensor(:,63)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(44)+M(46)+M(47)+M(48)+M(49)+M(50)+M(56)+M(57)+M(59)+M(62)+M(63)+M(64)+M(71)+M(75)+M(84)+M(85)+M(87)+M(95)+M(97)+M(98) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(146)+M(176)))
  T5sum(1:70,26) = T5sum(1:70,26) + Gcoeff * G4tensor(:,25)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(44)+M(45)+M(46)+M(47)+M(49)+M(50)+M(56)+M(59)+M(60)+M(62)+M(63)+M(64)+M(72)+M(73)+M(75)+M(83)+M(87)+M(95)+M(98) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(140)+M(178)))
  T5sum(1:70,26) = T5sum(1:70,26) + Gcoeff * G4tensor(:,27)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(57) &
    +M(60)-M(71)+M(72)+M(73)+M(83)-M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(140)-M(146)-M(176)+M(178)))
  T5sum(1:70,26) = T5sum(1:70,26) + Gcoeff * G4tensor(:,29)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(46)+M(48)+M(49)+M(58)+M(60)+M(61)+M(68)+M(71)+M(72)+M(80)+M(85)+M(92)+M(95)+M(96)+M(97)+M(98)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(133)+M(244)))
  T5sum(1:70,27) = T5sum(1:70,27) + Gcoeff * G4tensor(:,122)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(45)+M(46)+M(49)+M(57)+M(58)+M(61)+M(68)+M(73)+M(80)+M(83)+M(84)+M(92)+M(95)+M(96)+M(98)+M(99) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(131)+M(250)))
  T5sum(1:70,27) = T5sum(1:70,27) + Gcoeff * G4tensor(:,123)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)+M(73)+M(83)+M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)+M(110)+M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(131)-M(133)-M(244)+M(250)))
  T5sum(1:70,27) = T5sum(1:70,27) + Gcoeff * G4tensor(:,124)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(43)+M(44)+M(46)+M(47)+M(48)+M(49)+M(56)+M(57)+M(59)+M(70)+M(71)+M(82)+M(84)+M(85)+M(92)+M(93)+M(94)+M(95)+M(97)+M(98) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(145)+M(236)))
  T5sum(1:70,28) = T5sum(1:70,28) + Gcoeff * G4tensor(:,26)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(43)+M(44)+M(45)+M(46)+M(47)+M(49)+M(56)+M(59)+M(60)+M(70)+M(72)+M(73)+M(82)+M(83)+M(92)+M(93)+M(94)+M(95)+M(98) &
    +M(100)+M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(139)+M(238)))
  T5sum(1:70,28) = T5sum(1:70,28) + Gcoeff * G4tensor(:,28)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(57) &
    +M(60)-M(71)+M(72)+M(73)+M(83)-M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(139)-M(145)-M(236)+M(238)))
  T5sum(1:70,28) = T5sum(1:70,28) + Gcoeff * G4tensor(:,30)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(48)+M(51)+M(52)+M(60)+M(63)+M(64)+M(68)+M(71)+M(72)+M(73)+M(74)+M(75)+M(80)+M(86)+M(87)+M(92) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(136)+M(196)))
  T5sum(1:70,29) = T5sum(1:70,29) + Gcoeff * G4tensor(:,125)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(45)+M(51)+M(52)+M(57)+M(63)+M(64)+M(68)+M(74)+M(75)+M(80)+M(83)+M(84)+M(85)+M(86)+M(87)+M(92)+M(97) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(135)+M(220)))
  T5sum(1:70,29) = T5sum(1:70,29) + Gcoeff * G4tensor(:,126)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(135)-M(136)-M(196)+M(220)))
  T5sum(1:70,29) = T5sum(1:70,29) + Gcoeff * G4tensor(:,127)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(48)+M(50)+M(51)+M(52)+M(60)+M(62)+M(68)+M(70)+M(71)+M(72)+M(73)+M(74)+M(80)+M(82)+M(86)+M(93)+M(94) &
    +M(100)+M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(150)+M(194)))
  T5sum(1:70,30) = T5sum(1:70,30) + Gcoeff * G4tensor(:,64)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(45)+M(50)+M(51)+M(52)+M(57)+M(62)+M(68)+M(70)+M(74)+M(80)+M(82)+M(83)+M(84)+M(85)+M(86)+M(93)+M(94)+M(97) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(149)+M(218)))
  T5sum(1:70,30) = T5sum(1:70,30) + Gcoeff * G4tensor(:,65)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(149)-M(150)-M(194)+M(218)))
  T5sum(1:70,30) = T5sum(1:70,30) + Gcoeff * G4tensor(:,66)
  Gcoeff = (c(6)*(M(131)-M(133)-M(135)+M(136)-M(140)+M(146)+M(152)-M(154)-M(164)+M(170)+M(176)-M(178)+M(196)-M(220)-M(244) &
    +M(250))) * den(321)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,100)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(134)-M(136)+M(140)-M(142)-M(148)+M(154)+M(164)-M(166)-M(172)+M(178)-M(196)+M(202)+M(226) &
    -M(250))) * den(321)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,101)
  Gcoeff = (c(6)*(-M(132)+M(133)-M(134)+M(135)+M(142)-M(146)+M(148)-M(152)+M(166)-M(170)+M(172)-M(176)-M(202)+M(220)-M(226) &
    +M(244))) * den(321)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,102)
  Gcoeff = (c(5)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(140)-M(146)-M(152) &
    +M(154)+M(164)-M(170)-M(176)+M(178))) * den(18)
  T4sum(1:70,193) = T4sum(1:70,193) + Gcoeff * G4tensor(:,136)
  Gcoeff = (c(5)*(-M(73)+M(76)+M(88)-M(100)-M(110)+M(112)+M(118)-M(124)-M(125)+M(126)+M(128)-M(130))+c(6)*(-M(140)+M(142)+M(148) &
    -M(154)-M(164)+M(166)+M(172)-M(178))) * den(18)
  T4sum(1:70,193) = T4sum(1:70,193) + Gcoeff * G4tensor(:,137)
  Gcoeff = (c(5)*(-M(76)+M(85)-M(88)+M(97)-M(112)+M(116)-M(118)+M(122)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(142)+M(146)-M(148) &
    +M(152)-M(166)+M(170)-M(172)+M(176))) * den(18)
  T4sum(1:70,193) = T4sum(1:70,193) + Gcoeff * G4tensor(:,138)
  Gcoeff = (c(5)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(131)-M(133)-M(135) &
    +M(136)+M(196)-M(220)-M(244)+M(250))) * den(18)
  T4sum(1:70,194) = T4sum(1:70,194) + Gcoeff * G4tensor(:,128)
  Gcoeff = (c(5)*(-M(73)+M(76)+M(88)-M(100)-M(110)+M(112)+M(118)-M(124)-M(125)+M(126)+M(128)-M(130))+c(6)*(-M(131)+M(132)+M(134) &
    -M(136)-M(196)+M(202)+M(226)-M(250))) * den(18)
  T4sum(1:70,194) = T4sum(1:70,194) + Gcoeff * G4tensor(:,129)
  Gcoeff = (c(5)*(-M(76)+M(85)-M(88)+M(97)-M(112)+M(116)-M(118)+M(122)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(132)+M(133)-M(134) &
    +M(135)-M(202)+M(220)-M(226)+M(244))) * den(18)
  T4sum(1:70,194) = T4sum(1:70,194) + Gcoeff * G4tensor(:,130)
  Gcoeff = (c(5)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(131)-M(133)-M(135) &
    +M(136)+M(196)-M(220)-M(244)+M(250))) * den(355)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,127)
  Gcoeff = (c(5)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(140)-M(146)-M(152) &
    +M(154)+M(164)-M(170)-M(176)+M(178))) * den(355)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,128)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(135)-M(136)+M(140)-M(146)-M(152)+M(154)+M(164)-M(170)-M(176)+M(178)-M(196)+M(220)+M(244) &
    -M(250))) * den(355)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,129)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)+M(73)+M(83)+M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)+M(110)+M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(131)-M(133)-M(244)+M(250))) * den(20)
  T4sum(1:35,181) = T4sum(1:35,181) + Gcoeff * G3tensor(:,103)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(57) &
    +M(60)-M(71)+M(72)+M(73)+M(83)-M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(139)-M(145)-M(236)+M(238))) * den(20)
  T4sum(1:35,181) = T4sum(1:35,181) + Gcoeff * G3tensor(:,104)
  Gcoeff = (c(5)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(131)+M(133)+M(139) &
    -M(145)-M(236)+M(238)+M(244)-M(250))) * den(20)
  T4sum(1:35,181) = T4sum(1:35,181) + Gcoeff * G3tensor(:,105)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(149)-M(150)-M(194)+M(218))) * den(20)
  T4sum(1:35,182) = T4sum(1:35,182) + Gcoeff * G3tensor(:,40)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(57) &
    +M(60)-M(71)+M(72)-M(73)+M(83)-M(84)+M(85)+M(97)-M(100)-M(101)+M(103)+M(109)-M(110)-M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(152)-M(154)-M(164)+M(170))) * den(20)
  T4sum(1:35,182) = T4sum(1:35,182) + Gcoeff * G3tensor(:,41)
  Gcoeff = (c(5)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(149)+M(150)+M(152) &
    -M(154)-M(164)+M(170)+M(194)-M(218))) * den(20)
  T4sum(1:35,182) = T4sum(1:35,182) + Gcoeff * G3tensor(:,42)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(149)-M(150)-M(194)+M(218))) * den(20)
  T4sum(1:35,8) = T4sum(1:35,8) + Gcoeff * G3tensor(:,16)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(135)-M(136)-M(196)+M(220))) * den(20)
  T4sum(1:35,8) = T4sum(1:35,8) + Gcoeff * G3tensor(:,17)
  Gcoeff = (c(5)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(125)+M(127)+M(129)-M(130))+c(6)*(M(135)-M(136)-M(149)+M(150) &
    +M(194)-M(196)-M(218)+M(220))) * den(20)
  T4sum(1:35,8) = T4sum(1:35,8) + Gcoeff * G3tensor(:,18)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(57) &
    +M(60)-M(71)+M(72)+M(73)+M(83)-M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(140)-M(146)-M(176)+M(178))) * den(20)
  T4sum(1:35,7) = T4sum(1:35,7) + Gcoeff * G3tensor(:,106)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(57) &
    +M(60)-M(71)+M(72)+M(73)+M(83)-M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(139)-M(145)-M(236)+M(238))) * den(20)
  T4sum(1:35,7) = T4sum(1:35,7) + Gcoeff * G3tensor(:,107)
  Gcoeff = (c(5)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(125)+M(127)+M(129)-M(130))+c(6)*(M(139)-M(140)-M(145)+M(146) &
    +M(176)-M(178)-M(236)+M(238))) * den(20)
  T4sum(1:35,7) = T4sum(1:35,7) + Gcoeff * G3tensor(:,108)
  Gcoeff = (c(6)*(M(131)-M(133)-M(139)+M(145)-M(149)+M(150)+M(152)-M(154)-M(164)+M(170)+M(194)-M(218)+M(236)-M(238)-M(244) &
    +M(250))) * den(330)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,109)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(135)-M(136)+M(140)-M(146)-M(152)+M(154)+M(164)-M(170)-M(176)+M(178)-M(196)+M(220)+M(244) &
    -M(250))) * den(330)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,110)
  Gcoeff = (c(6)*(-M(135)+M(136)+M(139)-M(140)-M(145)+M(146)+M(149)-M(150)+M(176)-M(178)-M(194)+M(196)+M(218)-M(220)-M(236) &
    +M(238))) * den(330)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,111)
  Gcoeff = (c(5)*(-M(76)+M(85)-M(88)+M(97)-M(112)+M(116)-M(118)+M(122)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(132)+M(133)-M(134) &
    +M(135)-M(202)+M(220)-M(226)+M(244))) * den(358)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,130)
  Gcoeff = (c(5)*(-M(76)+M(85)-M(88)+M(97)-M(112)+M(116)-M(118)+M(122)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(142)+M(146)-M(148) &
    +M(152)-M(166)+M(170)-M(172)+M(176))) * den(358)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,131)
  Gcoeff = (c(6)*(M(132)-M(133)+M(134)-M(135)-M(142)+M(146)-M(148)+M(152)-M(166)+M(170)-M(172)+M(176)+M(202)-M(220)+M(226) &
    -M(244))) * den(358)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,132)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)+M(76)-M(85)+M(88)+M(95)+M(96)-M(97)+M(102)-M(105)-M(111)+M(112)-M(116)+M(118)+M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(132)-M(135)-M(220)+M(226))) * den(22)
  T4sum(1:35,178) = T4sum(1:35,178) + Gcoeff * G3tensor(:,112)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(58) &
    +M(63)-M(74)+M(75)+M(76)-M(85)+M(88)+M(95)-M(96)-M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(141)-M(151)-M(212)+M(214))) * den(22)
  T4sum(1:35,178) = T4sum(1:35,178) + Gcoeff * G3tensor(:,113)
  Gcoeff = (c(5)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(132)+M(135)+M(141) &
    -M(151)-M(212)+M(214)+M(220)-M(226))) * den(22)
  T4sum(1:35,178) = T4sum(1:35,178) + Gcoeff * G3tensor(:,114)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(143)-M(144)-M(200)+M(242))) * den(22)
  T4sum(1:35,179) = T4sum(1:35,179) + Gcoeff * G3tensor(:,28)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(58) &
    +M(63)-M(74)+M(75)-M(76)+M(85)-M(88)+M(95)-M(96)+M(97)-M(102)+M(105)+M(111)-M(112)+M(116)-M(118)-M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(146)-M(148)-M(166)+M(176))) * den(22)
  T4sum(1:35,179) = T4sum(1:35,179) + Gcoeff * G3tensor(:,29)
  Gcoeff = (c(5)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(143)+M(144)+M(146) &
    -M(148)-M(166)+M(176)+M(200)-M(242))) * den(22)
  T4sum(1:35,179) = T4sum(1:35,179) + Gcoeff * G3tensor(:,30)
  Gcoeff = (c(5)*(M(73)-M(76)-M(88)+M(100)+M(110)-M(112)-M(118)+M(124)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(131)-M(132)-M(134) &
    +M(136)+M(196)-M(202)-M(226)+M(250))) * den(361)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,133)
  Gcoeff = (c(5)*(M(73)-M(76)-M(88)+M(100)+M(110)-M(112)-M(118)+M(124)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(140)-M(142)-M(148) &
    +M(154)+M(164)-M(166)-M(172)+M(178))) * den(361)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,134)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(134)-M(136)+M(140)-M(142)-M(148)+M(154)+M(164)-M(166)-M(172)+M(178)-M(196)+M(202)+M(226) &
    -M(250))) * den(361)
  T3sum(1:15,40) = T3sum(1:15,40) + Gcoeff * G2tensor(:,135)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)+M(61) &
    -M(64)-M(73)+M(76)-M(86)-M(87)+M(88)+M(98)+M(99)-M(100)+M(104)-M(106)-M(110)+M(112)-M(117)+M(118)+M(123)-M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(134)-M(136)-M(196)+M(202))) * den(26)
  T4sum(1:35,190) = T4sum(1:35,190) + Gcoeff * G3tensor(:,115)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(61) &
    +M(64)-M(73)+M(76)-M(86)+M(87)+M(88)+M(98)-M(99)-M(100)-M(104)+M(106)-M(110)+M(112)+M(117)+M(118)-M(123)-M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(147)-M(153)-M(188)+M(190))) * den(26)
  T4sum(1:35,190) = T4sum(1:35,190) + Gcoeff * G3tensor(:,116)
  Gcoeff = (c(5)*(-M(61)+M(64)+M(87)-M(99)-M(104)+M(106)+M(117)-M(123)+M(125)-M(126)-M(128)+M(130))+c(6)*(-M(134)+M(136)+M(147) &
    -M(153)-M(188)+M(190)+M(196)-M(202))) * den(26)
  T4sum(1:35,190) = T4sum(1:35,190) + Gcoeff * G3tensor(:,117)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)+M(61) &
    -M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(137)-M(138)-M(224)+M(248))) * den(26)
  T4sum(1:35,191) = T4sum(1:35,191) + Gcoeff * G3tensor(:,58)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(61) &
    +M(64)+M(73)-M(76)-M(86)+M(87)-M(88)+M(98)-M(99)+M(100)-M(104)+M(106)+M(110)-M(112)+M(117)-M(118)-M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(140)-M(142)-M(172)+M(178))) * den(26)
  T4sum(1:35,191) = T4sum(1:35,191) + Gcoeff * G3tensor(:,59)
  Gcoeff = (c(5)*(-M(61)+M(64)+M(87)-M(99)-M(104)+M(106)+M(117)-M(123)+M(125)-M(126)-M(128)+M(130))+c(6)*(-M(137)+M(138)+M(140) &
    -M(142)-M(172)+M(178)+M(224)-M(248))) * den(26)
  T4sum(1:35,191) = T4sum(1:35,191) + Gcoeff * G3tensor(:,60)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(47)+M(48)+M(49)+M(50)+M(51)+M(59)+M(62)+M(64)+M(68)+M(69)+M(70)+M(71)+M(74)+M(76)+M(81)+M(87)+M(88)+M(93)+M(98) &
    +M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(147)+M(190)))
  T5sum(1:70,37) = T5sum(1:70,37) + Gcoeff * G4tensor(:,43)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(41)+M(43)+M(47)+M(48)+M(49)+M(51)+M(59)+M(63)+M(68)+M(69)+M(71)+M(74)+M(75)+M(76)+M(81)+M(82)+M(88)+M(92)+M(94)+M(98) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(144)+M(200)))
  T5sum(1:70,37) = T5sum(1:70,37) + Gcoeff * G4tensor(:,44)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    +M(63)-M(64)-M(70)+M(75)+M(82)-M(87)+M(92)-M(93)+M(94)+M(105)-M(106)-M(108)+M(111)+M(114)-M(117)-M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(144)-M(147)-M(190)+M(200)))
  T5sum(1:70,37) = T5sum(1:70,37) + Gcoeff * G4tensor(:,45)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(48)+M(50)+M(51)+M(52)+M(60)+M(62)+M(68)+M(70)+M(71)+M(72)+M(73)+M(74)+M(80)+M(82)+M(86)+M(93)+M(94) &
    +M(100)+M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(150)+M(194)))
  T5sum(1:70,38) = T5sum(1:70,38) + Gcoeff * G4tensor(:,31)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(48)+M(51)+M(52)+M(60)+M(63)+M(64)+M(68)+M(71)+M(72)+M(73)+M(74)+M(75)+M(80)+M(86)+M(87)+M(92) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(136)+M(196)))
  T5sum(1:70,38) = T5sum(1:70,38) + Gcoeff * G4tensor(:,33)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    +M(63)+M(64)-M(70)+M(75)-M(82)+M(87)+M(92)-M(93)-M(94)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(136)-M(150)-M(194)+M(196)))
  T5sum(1:70,38) = T5sum(1:70,38) + Gcoeff * G4tensor(:,35)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(42)+M(44)+M(45)+M(46)+M(50)+M(52)+M(56)+M(62)+M(63)+M(69)+M(75)+M(76)+M(80)+M(81)+M(82)+M(83)+M(86)+M(88)+M(94)+M(95) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(141)+M(214)))
  T5sum(1:70,39) = T5sum(1:70,39) + Gcoeff * G4tensor(:,92)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(42)+M(43)+M(44)+M(45)+M(46)+M(52)+M(56)+M(64)+M(69)+M(70)+M(76)+M(80)+M(81)+M(83)+M(86)+M(87)+M(88)+M(92)+M(93)+M(95) &
    +M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(138)+M(224)))
  T5sum(1:70,39) = T5sum(1:70,39) + Gcoeff * G4tensor(:,93)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)+M(64)+M(70)-M(75)-M(82)+M(87)+M(92)+M(93)-M(94)-M(105)+M(106)+M(108)-M(111)-M(114)+M(117)+M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(138)-M(141)-M(214)+M(224)))
  T5sum(1:70,39) = T5sum(1:70,39) + Gcoeff * G4tensor(:,94)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(45)+M(50)+M(51)+M(52)+M(57)+M(62)+M(68)+M(70)+M(74)+M(80)+M(82)+M(83)+M(84)+M(85)+M(86)+M(93)+M(94)+M(97) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(149)+M(218)))
  T5sum(1:70,40) = T5sum(1:70,40) + Gcoeff * G4tensor(:,32)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(45)+M(51)+M(52)+M(57)+M(63)+M(64)+M(68)+M(74)+M(75)+M(80)+M(83)+M(84)+M(85)+M(86)+M(87)+M(92)+M(97) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(135)+M(220)))
  T5sum(1:70,40) = T5sum(1:70,40) + Gcoeff * G4tensor(:,34)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    +M(63)+M(64)-M(70)+M(75)-M(82)+M(87)+M(92)-M(93)-M(94)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(135)-M(149)-M(218)+M(220)))
  T5sum(1:70,40) = T5sum(1:70,40) + Gcoeff * G4tensor(:,36)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(44)+M(45)+M(46)+M(47)+M(49)+M(50)+M(56)+M(59)+M(60)+M(62)+M(63)+M(64)+M(72)+M(73)+M(75)+M(83)+M(87)+M(95)+M(98) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(140)+M(178)))
  T5sum(1:70,41) = T5sum(1:70,41) + Gcoeff * G4tensor(:,95)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(43)+M(44)+M(45)+M(46)+M(47)+M(49)+M(56)+M(59)+M(60)+M(70)+M(72)+M(73)+M(82)+M(83)+M(92)+M(93)+M(94)+M(95)+M(98) &
    +M(100)+M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(139)+M(238)))
  T5sum(1:70,41) = T5sum(1:70,41) + Gcoeff * G4tensor(:,96)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(139)-M(140)-M(178)+M(238)))
  T5sum(1:70,41) = T5sum(1:70,41) + Gcoeff * G4tensor(:,97)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(44)+M(46)+M(47)+M(48)+M(49)+M(50)+M(56)+M(57)+M(59)+M(62)+M(63)+M(64)+M(71)+M(75)+M(84)+M(85)+M(87)+M(95)+M(97)+M(98) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(146)+M(176)))
  T5sum(1:70,42) = T5sum(1:70,42) + Gcoeff * G4tensor(:,46)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(43)+M(44)+M(46)+M(47)+M(48)+M(49)+M(56)+M(57)+M(59)+M(70)+M(71)+M(82)+M(84)+M(85)+M(92)+M(93)+M(94)+M(95)+M(97)+M(98) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(145)+M(236)))
  T5sum(1:70,42) = T5sum(1:70,42) + Gcoeff * G4tensor(:,47)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(145)-M(146)-M(176)+M(236)))
  T5sum(1:70,42) = T5sum(1:70,42) + Gcoeff * G4tensor(:,48)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(143)-M(144)-M(200)+M(242))) * den(22)
  T4sum(1:35,5) = T4sum(1:35,5) + Gcoeff * G3tensor(:,10)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(134)-M(202)+M(244))) * den(22)
  T4sum(1:35,5) = T4sum(1:35,5) + Gcoeff * G3tensor(:,11)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(133)-M(134)-M(143)+M(144) &
    +M(200)-M(202)-M(242)+M(244))) * den(22)
  T4sum(1:35,5) = T4sum(1:35,5) + Gcoeff * G3tensor(:,12)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(58) &
    +M(63)-M(74)+M(75)+M(76)-M(85)+M(88)+M(95)-M(96)-M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(142)-M(152)-M(170)+M(172))) * den(22)
  T4sum(1:35,4) = T4sum(1:35,4) + Gcoeff * G3tensor(:,118)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(58) &
    +M(63)-M(74)+M(75)+M(76)-M(85)+M(88)+M(95)-M(96)-M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(141)-M(151)-M(212)+M(214))) * den(22)
  T4sum(1:35,4) = T4sum(1:35,4) + Gcoeff * G3tensor(:,119)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(141)-M(142)-M(151)+M(152) &
    +M(170)-M(172)-M(212)+M(214))) * den(22)
  T4sum(1:35,4) = T4sum(1:35,4) + Gcoeff * G3tensor(:,120)
  Gcoeff = (c(6)*(M(133)-M(134)-M(141)+M(142)-M(143)+M(144)+M(151)-M(152)-M(170)+M(172)+M(200)-M(202)+M(212)-M(214)-M(242) &
    +M(244))) * den(335)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,121)
  Gcoeff = (c(6)*(M(132)-M(133)+M(134)-M(135)-M(142)+M(146)-M(148)+M(152)-M(166)+M(170)-M(172)+M(176)+M(202)-M(220)+M(226) &
    -M(244))) * den(335)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,122)
  Gcoeff = (c(6)*(-M(132)+M(135)+M(141)+M(143)-M(144)-M(146)+M(148)-M(151)+M(166)-M(176)-M(200)-M(212)+M(214)+M(220)-M(226) &
    +M(242))) * den(335)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,123)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(47)+M(48)+M(50)+M(51)+M(52)+M(59)+M(61)+M(62)+M(68)+M(69)+M(70)+M(71)+M(73)+M(74)+M(81)+M(86)+M(93)+M(99) &
    +M(100)+M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(153)+M(188)))
  T5sum(1:70,49) = T5sum(1:70,49) + Gcoeff * G4tensor(:,67)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(48)+M(50)+M(51)+M(52)+M(60)+M(62)+M(68)+M(70)+M(71)+M(72)+M(73)+M(74)+M(80)+M(82)+M(86)+M(93)+M(94) &
    +M(100)+M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(150)+M(194)))
  T5sum(1:70,49) = T5sum(1:70,49) + Gcoeff * G4tensor(:,68)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)-M(61)-M(69)+M(72)+M(80)-M(81)+M(82)+M(94)-M(99)+M(103)-M(104)-M(107)+M(109)-M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(150)-M(153)-M(188)+M(194)))
  T5sum(1:70,49) = T5sum(1:70,49) + Gcoeff * G4tensor(:,69)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(41)+M(43)+M(47)+M(48)+M(49)+M(51)+M(59)+M(63)+M(68)+M(69)+M(71)+M(74)+M(75)+M(76)+M(81)+M(82)+M(88)+M(92)+M(94)+M(98) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(144)+M(200)))
  T5sum(1:70,50) = T5sum(1:70,50) + Gcoeff * G4tensor(:,19)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(48)+M(49)+M(51)+M(60)+M(61)+M(63)+M(68)+M(71)+M(72)+M(74)+M(75)+M(76)+M(80)+M(88)+M(92)+M(98)+M(99) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(134)+M(202)))
  T5sum(1:70,50) = T5sum(1:70,50) + Gcoeff * G4tensor(:,21)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)+M(61)-M(69)+M(72)+M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(134)-M(144)-M(200)+M(202)))
  T5sum(1:70,50) = T5sum(1:70,50) + Gcoeff * G4tensor(:,23)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(43)+M(44)+M(45)+M(46)+M(47)+M(49)+M(56)+M(59)+M(60)+M(70)+M(72)+M(73)+M(82)+M(83)+M(92)+M(93)+M(94)+M(95)+M(98) &
    +M(100)+M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(139)+M(238)))
  T5sum(1:70,51) = T5sum(1:70,51) + Gcoeff * G4tensor(:,98)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(43)+M(44)+M(45)+M(46)+M(49)+M(56)+M(61)+M(69)+M(70)+M(73)+M(80)+M(81)+M(83)+M(92)+M(93)+M(95)+M(98)+M(99) &
    +M(100)+M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(137)+M(248)))
  T5sum(1:70,51) = T5sum(1:70,51) + Gcoeff * G4tensor(:,99)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    -M(60)+M(61)+M(69)-M(72)+M(80)+M(81)-M(82)-M(94)+M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(137)-M(139)-M(238)+M(248)))
  T5sum(1:70,51) = T5sum(1:70,51) + Gcoeff * G4tensor(:,100)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(43)+M(46)+M(47)+M(48)+M(49)+M(58)+M(59)+M(68)+M(69)+M(71)+M(81)+M(82)+M(85)+M(92)+M(94)+M(95)+M(96)+M(97)+M(98) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(143)+M(242)))
  T5sum(1:70,52) = T5sum(1:70,52) + Gcoeff * G4tensor(:,20)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(46)+M(48)+M(49)+M(58)+M(60)+M(61)+M(68)+M(71)+M(72)+M(80)+M(85)+M(92)+M(95)+M(96)+M(97)+M(98)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(133)+M(244)))
  T5sum(1:70,52) = T5sum(1:70,52) + Gcoeff * G4tensor(:,22)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)+M(61)-M(69)+M(72)+M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(143)-M(242)+M(244)))
  T5sum(1:70,52) = T5sum(1:70,52) + Gcoeff * G4tensor(:,24)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(44)+M(45)+M(46)+M(47)+M(50)+M(52)+M(56)+M(59)+M(60)+M(61)+M(62)+M(63)+M(72)+M(75)+M(76)+M(83)+M(86)+M(88)+M(95)+M(99) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(142)+M(172)))
  T5sum(1:70,53) = T5sum(1:70,53) + Gcoeff * G4tensor(:,101)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(42)+M(44)+M(45)+M(46)+M(50)+M(52)+M(56)+M(62)+M(63)+M(69)+M(75)+M(76)+M(80)+M(81)+M(82)+M(83)+M(86)+M(88)+M(94)+M(95) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(141)+M(214)))
  T5sum(1:70,53) = T5sum(1:70,53) + Gcoeff * G4tensor(:,102)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    -M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(141)-M(142)-M(172)+M(214)))
  T5sum(1:70,53) = T5sum(1:70,53) + Gcoeff * G4tensor(:,103)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(44)+M(45)+M(47)+M(50)+M(51)+M(52)+M(56)+M(58)+M(59)+M(60)+M(61)+M(62)+M(72)+M(74)+M(83)+M(85)+M(86)+M(96)+M(97)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(152)+M(170)))
  T5sum(1:70,54) = T5sum(1:70,54) + Gcoeff * G4tensor(:,70)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(44)+M(45)+M(50)+M(51)+M(52)+M(56)+M(58)+M(62)+M(69)+M(74)+M(80)+M(81)+M(82)+M(83)+M(85)+M(86)+M(94)+M(96)+M(97) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(151)+M(212)))
  T5sum(1:70,54) = T5sum(1:70,54) + Gcoeff * G4tensor(:,71)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    -M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(151)-M(152)-M(170)+M(212)))
  T5sum(1:70,54) = T5sum(1:70,54) + Gcoeff * G4tensor(:,72)
  Gcoeff = (c(6)*(-M(134)+M(137)-M(139)-M(141)+M(142)+M(144)+M(150)-M(153)+M(172)-M(188)+M(194)+M(200)-M(202)-M(214)-M(238) &
    +M(248))) * den(340)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,124)
  Gcoeff = (c(6)*(M(134)-M(136)-M(137)+M(138)+M(140)-M(142)-M(147)+M(153)-M(172)+M(178)+M(188)-M(190)-M(196)+M(202)+M(224) &
    -M(248))) * den(340)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,125)
  Gcoeff = (c(6)*(M(136)-M(138)+M(139)-M(140)+M(141)-M(144)+M(147)-M(150)-M(178)+M(190)-M(194)+M(196)-M(200)+M(214)-M(224) &
    +M(238))) * den(340)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,126)
  Gcoeff = (c(5)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(134)-M(144)-M(150) &
    +M(153)+M(188)-M(194)-M(200)+M(202))) * den(24)
  T4sum(1:70,190) = T4sum(1:70,190) + Gcoeff * G4tensor(:,141)
  Gcoeff = (c(5)*(-M(61)+M(64)+M(87)-M(99)-M(104)+M(106)+M(117)-M(123)+M(125)-M(126)-M(128)+M(130))+c(6)*(-M(134)+M(136)+M(147) &
    -M(153)-M(188)+M(190)+M(196)-M(202))) * den(24)
  T4sum(1:70,190) = T4sum(1:70,190) + Gcoeff * G4tensor(:,142)
  Gcoeff = (c(5)*(-M(64)+M(82)-M(87)+M(94)-M(106)+M(114)-M(117)+M(120)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(136)+M(144)-M(147) &
    +M(150)-M(190)+M(194)-M(196)+M(200))) * den(24)
  T4sum(1:70,190) = T4sum(1:70,190) + Gcoeff * G4tensor(:,143)
  Gcoeff = (c(5)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(137)-M(139)-M(141) &
    +M(142)+M(172)-M(214)-M(238)+M(248))) * den(24)
  T4sum(1:70,191) = T4sum(1:70,191) + Gcoeff * G4tensor(:,104)
  Gcoeff = (c(5)*(-M(61)+M(64)+M(87)-M(99)-M(104)+M(106)+M(117)-M(123)+M(125)-M(126)-M(128)+M(130))+c(6)*(-M(137)+M(138)+M(140) &
    -M(142)-M(172)+M(178)+M(224)-M(248))) * den(24)
  T4sum(1:70,191) = T4sum(1:70,191) + Gcoeff * G4tensor(:,105)
  Gcoeff = (c(5)*(-M(64)+M(82)-M(87)+M(94)-M(106)+M(114)-M(117)+M(120)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(138)+M(139)-M(140) &
    +M(141)-M(178)+M(214)-M(224)+M(238))) * den(24)
  T4sum(1:70,191) = T4sum(1:70,191) + Gcoeff * G4tensor(:,106)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)+M(61) &
    -M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(137)-M(138)-M(224)+M(248))) * den(26)
  T4sum(1:35,2) = T4sum(1:35,2) + Gcoeff * G3tensor(:,4)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)+M(61) &
    -M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(131)-M(132)-M(226)+M(250))) * den(26)
  T4sum(1:35,2) = T4sum(1:35,2) + Gcoeff * G3tensor(:,5)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(131)-M(132)-M(137)+M(138) &
    +M(224)-M(226)-M(248)+M(250))) * den(26)
  T4sum(1:35,2) = T4sum(1:35,2) + Gcoeff * G3tensor(:,6)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(61) &
    +M(64)-M(73)+M(76)-M(86)+M(87)+M(88)+M(98)-M(99)-M(100)-M(104)+M(106)-M(110)+M(112)+M(117)+M(118)-M(123)-M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(148)-M(154)-M(164)+M(166))) * den(26)
  T4sum(1:35,1) = T4sum(1:35,1) + Gcoeff * G3tensor(:,127)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(61) &
    +M(64)-M(73)+M(76)-M(86)+M(87)+M(88)+M(98)-M(99)-M(100)-M(104)+M(106)-M(110)+M(112)+M(117)+M(118)-M(123)-M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(147)-M(153)-M(188)+M(190))) * den(26)
  T4sum(1:35,1) = T4sum(1:35,1) + Gcoeff * G3tensor(:,128)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(147)-M(148)-M(153)+M(154) &
    +M(164)-M(166)-M(188)+M(190))) * den(26)
  T4sum(1:35,1) = T4sum(1:35,1) + Gcoeff * G3tensor(:,129)
  Gcoeff = (c(6)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(343)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,130)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(134)-M(136)+M(140)-M(142)-M(148)+M(154)+M(164)-M(166)-M(172)+M(178)-M(196)+M(202)+M(226) &
    -M(250))) * den(343)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,131)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(137)-M(138)-M(140)+M(142)+M(147)-M(153)+M(172)-M(178)-M(188)+M(190)+M(196)-M(202)-M(224) &
    +M(248))) * den(343)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,132)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(44)+M(45)+M(50)+M(51)+M(52)+M(56)+M(58)+M(62)+M(69)+M(74)+M(80)+M(81)+M(82)+M(83)+M(85)+M(86)+M(94)+M(96)+M(97) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(151)+M(212)))
  T5sum(1:70,61) = T5sum(1:70,61) + Gcoeff * G4tensor(:,73)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(45)+M(50)+M(51)+M(52)+M(57)+M(62)+M(68)+M(70)+M(74)+M(80)+M(82)+M(83)+M(84)+M(85)+M(86)+M(93)+M(94)+M(97) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(149)+M(218)))
  T5sum(1:70,61) = T5sum(1:70,61) + Gcoeff * G4tensor(:,74)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)-M(58)+M(68)-M(69)+M(70)-M(81)+M(84)+M(93)-M(96)+M(101)-M(102)-M(107)+M(108)-M(113)+M(115)+M(119)-M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(149)-M(151)-M(212)+M(218)))
  T5sum(1:70,61) = T5sum(1:70,61) + Gcoeff * G4tensor(:,75)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(42)+M(43)+M(44)+M(45)+M(46)+M(52)+M(56)+M(64)+M(69)+M(70)+M(76)+M(80)+M(81)+M(83)+M(86)+M(87)+M(88)+M(92)+M(93)+M(95) &
    +M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(138)+M(224)))
  T5sum(1:70,62) = T5sum(1:70,62) + Gcoeff * G4tensor(:,7)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(45)+M(46)+M(52)+M(57)+M(58)+M(64)+M(68)+M(76)+M(80)+M(83)+M(84)+M(86)+M(87)+M(88)+M(92)+M(95)+M(96) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(132)+M(226)))
  T5sum(1:70,62) = T5sum(1:70,62) + Gcoeff * G4tensor(:,9)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)+M(58)+M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(132)-M(138)-M(224)+M(226)))
  T5sum(1:70,62) = T5sum(1:70,62) + Gcoeff * G4tensor(:,11)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(43)+M(44)+M(46)+M(47)+M(48)+M(49)+M(56)+M(57)+M(59)+M(70)+M(71)+M(82)+M(84)+M(85)+M(92)+M(93)+M(94)+M(95)+M(97)+M(98) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(145)+M(236)))
  T5sum(1:70,63) = T5sum(1:70,63) + Gcoeff * G4tensor(:,49)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(43)+M(46)+M(47)+M(48)+M(49)+M(58)+M(59)+M(68)+M(69)+M(71)+M(81)+M(82)+M(85)+M(92)+M(94)+M(95)+M(96)+M(97)+M(98) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(143)+M(242)))
  T5sum(1:70,63) = T5sum(1:70,63) + Gcoeff * G4tensor(:,50)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)+M(58)+M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(143)-M(145)-M(236)+M(242)))
  T5sum(1:70,63) = T5sum(1:70,63) + Gcoeff * G4tensor(:,51)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(43)+M(44)+M(45)+M(46)+M(49)+M(56)+M(61)+M(69)+M(70)+M(73)+M(80)+M(81)+M(83)+M(92)+M(93)+M(95)+M(98)+M(99) &
    +M(100)+M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(137)+M(248)))
  T5sum(1:70,64) = T5sum(1:70,64) + Gcoeff * G4tensor(:,8)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(45)+M(46)+M(49)+M(57)+M(58)+M(61)+M(68)+M(73)+M(80)+M(83)+M(84)+M(92)+M(95)+M(96)+M(98)+M(99) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(131)+M(250)))
  T5sum(1:70,64) = T5sum(1:70,64) + Gcoeff * G4tensor(:,10)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)+M(58)+M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(131)-M(137)-M(248)+M(250)))
  T5sum(1:70,64) = T5sum(1:70,64) + Gcoeff * G4tensor(:,12)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(44)+M(47)+M(48)+M(49)+M(50)+M(51)+M(56)+M(57)+M(58)+M(59)+M(62)+M(64)+M(71)+M(74)+M(76)+M(84)+M(87)+M(88)+M(96)+M(98) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(148)+M(166)))
  T5sum(1:70,65) = T5sum(1:70,65) + Gcoeff * G4tensor(:,52)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(47)+M(48)+M(49)+M(50)+M(51)+M(59)+M(62)+M(64)+M(68)+M(69)+M(70)+M(71)+M(74)+M(76)+M(81)+M(87)+M(88)+M(93)+M(98) &
    +M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(147)+M(190)))
  T5sum(1:70,65) = T5sum(1:70,65) + Gcoeff * G4tensor(:,53)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(147)-M(148)-M(166)+M(190)))
  T5sum(1:70,65) = T5sum(1:70,65) + Gcoeff * G4tensor(:,54)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(44)+M(47)+M(48)+M(50)+M(51)+M(52)+M(56)+M(57)+M(58)+M(59)+M(61)+M(62)+M(71)+M(73)+M(74)+M(84)+M(86)+M(96)+M(99) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(154)+M(164)))
  T5sum(1:70,66) = T5sum(1:70,66) + Gcoeff * G4tensor(:,76)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(47)+M(48)+M(50)+M(51)+M(52)+M(59)+M(61)+M(62)+M(68)+M(69)+M(70)+M(71)+M(73)+M(74)+M(81)+M(86)+M(93)+M(99) &
    +M(100)+M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(153)+M(188)))
  T5sum(1:70,66) = T5sum(1:70,66) + Gcoeff * G4tensor(:,77)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(153)-M(154)-M(164)+M(188)))
  T5sum(1:70,66) = T5sum(1:70,66) + Gcoeff * G4tensor(:,78)
  Gcoeff = (c(6)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(348)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,133)
  Gcoeff = (c(6)*(M(132)-M(135)-M(141)-M(143)+M(144)+M(146)-M(148)+M(151)-M(166)+M(176)+M(200)+M(212)-M(214)-M(220)+M(226) &
    -M(242))) * den(348)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,134)
  Gcoeff = (c(6)*(M(135)-M(138)+M(141)-M(144)+M(145)-M(146)+M(147)-M(149)-M(176)+M(190)-M(200)+M(214)-M(218)+M(220)-M(224) &
    +M(236))) * den(348)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,135)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(132)-M(138)-M(149) &
    +M(151)+M(212)-M(218)-M(224)+M(226))) * den(28)
  T4sum(1:70,178) = T4sum(1:70,178) + Gcoeff * G4tensor(:,145)
  Gcoeff = (c(5)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(132)+M(135)+M(141) &
    -M(151)-M(212)+M(214)+M(220)-M(226))) * den(28)
  T4sum(1:70,178) = T4sum(1:70,178) + Gcoeff * G4tensor(:,146)
  Gcoeff = (c(5)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(6)*(-M(135)+M(138)-M(141) &
    +M(149)-M(214)+M(218)-M(220)+M(224))) * den(28)
  T4sum(1:70,178) = T4sum(1:70,178) + Gcoeff * G4tensor(:,147)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(143)-M(145)-M(147) &
    +M(148)+M(166)-M(190)-M(236)+M(242))) * den(28)
  T4sum(1:70,179) = T4sum(1:70,179) + Gcoeff * G4tensor(:,55)
  Gcoeff = (c(5)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(143)+M(144)+M(146) &
    -M(148)-M(166)+M(176)+M(200)-M(242))) * den(28)
  T4sum(1:70,179) = T4sum(1:70,179) + Gcoeff * G4tensor(:,56)
  Gcoeff = (c(5)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(6)*(-M(144)+M(145)-M(146) &
    +M(147)-M(176)+M(190)-M(200)+M(236))) * den(28)
  T4sum(1:70,179) = T4sum(1:70,179) + Gcoeff * G4tensor(:,57)
  Gcoeff = (c(6)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(351)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,136)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(139)-M(145)+M(149)-M(150)-M(152)+M(154)+M(164)-M(170)-M(194)+M(218)-M(236)+M(238)+M(244) &
    -M(250))) * den(351)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,137)
  Gcoeff = (c(6)*(-M(133)+M(137)-M(139)+M(143)+M(150)-M(151)+M(152)-M(153)+M(170)-M(188)+M(194)-M(212)-M(238)+M(242)-M(244) &
    +M(248))) * den(351)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,138)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(131)-M(137)-M(143) &
    +M(145)+M(236)-M(242)-M(248)+M(250))) * den(30)
  T4sum(1:70,181) = T4sum(1:70,181) + Gcoeff * G4tensor(:,148)
  Gcoeff = (c(5)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(131)+M(133)+M(139) &
    -M(145)-M(236)+M(238)+M(244)-M(250))) * den(30)
  T4sum(1:70,181) = T4sum(1:70,181) + Gcoeff * G4tensor(:,149)
  Gcoeff = (c(5)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(6)*(-M(133)+M(137)-M(139) &
    +M(143)-M(238)+M(242)-M(244)+M(248))) * den(30)
  T4sum(1:70,181) = T4sum(1:70,181) + Gcoeff * G4tensor(:,150)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(149)-M(151)-M(153) &
    +M(154)+M(164)-M(188)-M(212)+M(218))) * den(30)
  T4sum(1:70,182) = T4sum(1:70,182) + Gcoeff * G4tensor(:,79)
  Gcoeff = (c(5)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(149)+M(150)+M(152) &
    -M(154)-M(164)+M(170)+M(194)-M(218))) * den(30)
  T4sum(1:70,182) = T4sum(1:70,182) + Gcoeff * G4tensor(:,80)
  Gcoeff = (c(5)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(6)*(-M(150)+M(151)-M(152) &
    +M(153)-M(170)+M(188)-M(194)+M(212))) * den(30)
  T4sum(1:70,182) = T4sum(1:70,182) + Gcoeff * G4tensor(:,81)
  Gcoeff = (c(6)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(781)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,139)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(147)-M(148)-M(153)+M(154) &
    +M(164)-M(166)-M(188)+M(190))) * den(126)
  T4sum(1:70,1) = T4sum(1:70,1) + Gcoeff * G4tensor(:,144)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(131)-M(132)-M(137)+M(138) &
    +M(224)-M(226)-M(248)+M(250))) * den(126)
  T4sum(1:70,2) = T4sum(1:70,2) + Gcoeff * G4tensor(:,85)
  Gcoeff = (c(6)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(1226)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,140)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(131)-M(137)-M(143) &
    +M(145)+M(236)-M(242)-M(248)+M(250))) * den(144)
  T4sum(1:70,181) = T4sum(1:70,181) + Gcoeff * G4tensor(:,151)
  Gcoeff = (c(5)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(149)-M(151)-M(153) &
    +M(154)+M(164)-M(188)-M(212)+M(218))) * den(144)
  T4sum(1:70,182) = T4sum(1:70,182) + Gcoeff * G4tensor(:,82)
  Gcoeff = (c(6)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(1227)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,141)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(132)-M(138)-M(149) &
    +M(151)+M(212)-M(218)-M(224)+M(226))) * den(309)
  T4sum(1:70,178) = T4sum(1:70,178) + Gcoeff * G4tensor(:,152)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(143)-M(145)-M(147) &
    +M(148)+M(166)-M(190)-M(236)+M(242))) * den(309)
  T4sum(1:70,179) = T4sum(1:70,179) + Gcoeff * G4tensor(:,58)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)-M(58)+M(68)-M(69)+M(70)-M(81)+M(84)+M(93)-M(96)+M(101)-M(102)-M(107)+M(108)-M(113)+M(115)+M(119)-M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(149)-M(151)-M(212)+M(218))) * den(12)
  T5sum(1:126,61) = T5sum(1:126,61) + Gcoeff * G5tensor(:,10)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)+M(58)+M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(132)-M(138)-M(224)+M(226))) * den(12)
  T5sum(1:126,62) = T5sum(1:126,62) + Gcoeff * G5tensor(:,13)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)+M(58)+M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(143)-M(145)-M(236)+M(242))) * den(12)
  T5sum(1:126,63) = T5sum(1:126,63) + Gcoeff * G5tensor(:,4)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)+M(58)+M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(131)-M(137)-M(248)+M(250))) * den(12)
  T5sum(1:126,64) = T5sum(1:126,64) + Gcoeff * G5tensor(:,14)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(147)-M(148)-M(166)+M(190))) * den(12)
  T5sum(1:126,65) = T5sum(1:126,65) + Gcoeff * G5tensor(:,3)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(153)-M(154)-M(164)+M(188))) * den(12)
  T5sum(1:126,66) = T5sum(1:126,66) + Gcoeff * G5tensor(:,9)
  Gcoeff = (c(6)*(M(133)-M(134)-M(141)+M(142)-M(143)+M(144)+M(151)-M(152)-M(170)+M(172)+M(200)-M(202)+M(212)-M(214)-M(242) &
    +M(244))) * den(795)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,142)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(141)-M(142)-M(151)+M(152) &
    +M(170)-M(172)-M(212)+M(214))) * den(129)
  T4sum(1:70,4) = T4sum(1:70,4) + Gcoeff * G4tensor(:,140)
  Gcoeff = (c(5)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(126)+M(127)-M(128)+M(129))+c(6)*(M(133)-M(134)-M(143)+M(144) &
    +M(200)-M(202)-M(242)+M(244))) * den(129)
  T4sum(1:70,5) = T4sum(1:70,5) + Gcoeff * G4tensor(:,134)
  Gcoeff = (c(6)*(-M(133)+M(137)-M(139)+M(143)+M(150)-M(151)+M(152)-M(153)+M(170)-M(188)+M(194)-M(212)-M(238)+M(242)-M(244) &
    +M(248))) * den(1262)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,143)
  Gcoeff = (c(5)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(6)*(-M(133)+M(137)-M(139) &
    +M(143)-M(238)+M(242)-M(244)+M(248))) * den(149)
  T4sum(1:70,181) = T4sum(1:70,181) + Gcoeff * G4tensor(:,153)
  Gcoeff = (c(5)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(6)*(-M(150)+M(151)-M(152) &
    +M(153)-M(170)+M(188)-M(194)+M(212))) * den(149)
  T4sum(1:70,182) = T4sum(1:70,182) + Gcoeff * G4tensor(:,83)
  Gcoeff = (c(6)*(-M(134)+M(137)-M(139)-M(141)+M(142)+M(144)+M(150)-M(153)+M(172)-M(188)+M(194)+M(200)-M(202)-M(214)-M(238) &
    +M(248))) * den(1263)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,144)
  Gcoeff = (c(5)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(134)-M(144)-M(150) &
    +M(153)+M(188)-M(194)-M(200)+M(202))) * den(325)
  T4sum(1:70,190) = T4sum(1:70,190) + Gcoeff * G4tensor(:,154)
  Gcoeff = (c(5)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(137)-M(139)-M(141) &
    +M(142)+M(172)-M(214)-M(238)+M(248))) * den(325)
  T4sum(1:70,191) = T4sum(1:70,191) + Gcoeff * G4tensor(:,107)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)-M(61)-M(69)+M(72)+M(80)-M(81)+M(82)+M(94)-M(99)+M(103)-M(104)-M(107)+M(109)-M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(150)-M(153)-M(188)+M(194))) * den(14)
  T5sum(1:126,49) = T5sum(1:126,49) + Gcoeff * G5tensor(:,11)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)+M(61)-M(69)+M(72)+M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(134)-M(144)-M(200)+M(202))) * den(14)
  T5sum(1:126,50) = T5sum(1:126,50) + Gcoeff * G5tensor(:,27)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    -M(60)+M(61)+M(69)-M(72)+M(80)+M(81)-M(82)-M(94)+M(99)-M(103)+M(104)+M(107)-M(109)+M(113)-M(114)-M(120)+M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(137)-M(139)-M(238)+M(248))) * den(14)
  T5sum(1:126,51) = T5sum(1:126,51) + Gcoeff * G5tensor(:,18)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    +M(60)+M(61)-M(69)+M(72)+M(80)-M(81)-M(82)-M(94)+M(99)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(143)-M(242)+M(244))) * den(14)
  T5sum(1:126,52) = T5sum(1:126,52) + Gcoeff * G5tensor(:,28)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    -M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(141)-M(142)-M(172)+M(214))) * den(14)
  T5sum(1:126,53) = T5sum(1:126,53) + Gcoeff * G5tensor(:,17)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59) &
    -M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(151)-M(152)-M(170)+M(212))) * den(14)
  T5sum(1:126,54) = T5sum(1:126,54) + Gcoeff * G5tensor(:,8)
  Gcoeff = (c(6)*(-M(135)+M(136)+M(139)-M(140)-M(145)+M(146)+M(149)-M(150)+M(176)-M(178)-M(194)+M(196)+M(218)-M(220)-M(236) &
    +M(238))) * den(808)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,145)
  Gcoeff = (c(5)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(125)+M(127)+M(129)-M(130))+c(6)*(M(139)-M(140)-M(145)+M(146) &
    +M(176)-M(178)-M(236)+M(238))) * den(131)
  T4sum(1:70,7) = T4sum(1:70,7) + Gcoeff * G4tensor(:,139)
  Gcoeff = (c(5)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(125)+M(127)+M(129)-M(130))+c(6)*(M(135)-M(136)-M(149)+M(150) &
    +M(194)-M(196)-M(218)+M(220))) * den(131)
  T4sum(1:70,8) = T4sum(1:70,8) + Gcoeff * G4tensor(:,135)
  Gcoeff = (c(6)*(M(131)-M(133)-M(139)+M(145)-M(149)+M(150)+M(152)-M(154)-M(164)+M(170)+M(194)-M(218)+M(236)-M(238)-M(244) &
    +M(250))) * den(1274)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,146)
  Gcoeff = (c(5)*(M(57)-M(60)-M(72)+M(84)+M(101)-M(103)-M(109)+M(115)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(131)-M(133)-M(139) &
    +M(145)+M(236)-M(238)-M(244)+M(250))) * den(154)
  T4sum(1:70,181) = T4sum(1:70,181) + Gcoeff * G4tensor(:,155)
  Gcoeff = (c(5)*(M(57)-M(60)-M(72)+M(84)+M(101)-M(103)-M(109)+M(115)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(149)-M(150)-M(152) &
    +M(154)+M(164)-M(170)-M(194)+M(218))) * den(154)
  T4sum(1:70,182) = T4sum(1:70,182) + Gcoeff * G4tensor(:,84)
  Gcoeff = (c(6)*(M(131)-M(133)-M(135)+M(136)-M(140)+M(146)+M(152)-M(154)-M(164)+M(170)+M(176)-M(178)+M(196)-M(220)-M(244) &
    +M(250))) * den(1275)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,147)
  Gcoeff = (c(5)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(140)-M(146)-M(152) &
    +M(154)+M(164)-M(170)-M(176)+M(178))) * den(355)
  T4sum(1:70,193) = T4sum(1:70,193) + Gcoeff * G4tensor(:,156)
  Gcoeff = (c(5)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(6)*(M(131)-M(133)-M(135) &
    +M(136)+M(196)-M(220)-M(244)+M(250))) * den(355)
  T4sum(1:70,194) = T4sum(1:70,194) + Gcoeff * G4tensor(:,131)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(57) &
    +M(60)-M(71)+M(72)-M(73)+M(83)-M(84)+M(85)+M(97)-M(100)-M(101)+M(103)+M(109)-M(110)-M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(152)-M(154)-M(164)+M(170))) * den(20)
  T5sum(1:126,25) = T5sum(1:126,25) + Gcoeff * G5tensor(:,12)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(57) &
    +M(60)-M(71)+M(72)+M(73)+M(83)-M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(140)-M(146)-M(176)+M(178))) * den(20)
  T5sum(1:126,26) = T5sum(1:126,26) + Gcoeff * G5tensor(:,31)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)+M(73)+M(83)+M(84)-M(85)-M(97)+M(100)+M(101)-M(103)-M(109)+M(110)+M(115)-M(116)-M(122)+M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(131)-M(133)-M(244)+M(250))) * den(20)
  T5sum(1:126,27) = T5sum(1:126,27) + Gcoeff * G5tensor(:,24)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(57) &
    +M(60)-M(71)+M(72)+M(73)+M(83)-M(84)-M(85)-M(97)+M(100)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(139)-M(145)-M(236)+M(238))) * den(20)
  T5sum(1:126,28) = T5sum(1:126,28) + Gcoeff * G5tensor(:,32)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(135)-M(136)-M(196)+M(220))) * den(20)
  T5sum(1:126,29) = T5sum(1:126,29) + Gcoeff * G5tensor(:,23)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)+M(57) &
    -M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(149)-M(150)-M(194)+M(218))) * den(20)
  T5sum(1:126,30) = T5sum(1:126,30) + Gcoeff * G5tensor(:,7)
  Gcoeff = (c(6)*(M(135)-M(138)+M(141)-M(144)+M(145)-M(146)+M(147)-M(149)-M(176)+M(190)-M(200)+M(214)-M(218)+M(220)-M(224) &
    +M(236))) * den(1310)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,148)
  Gcoeff = (c(5)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(6)*(-M(135)+M(138)-M(141) &
    +M(149)-M(214)+M(218)-M(220)+M(224))) * den(157)
  T4sum(1:70,178) = T4sum(1:70,178) + Gcoeff * G4tensor(:,157)
  Gcoeff = (c(5)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(6)*(-M(144)+M(145)-M(146) &
    +M(147)-M(176)+M(190)-M(200)+M(236))) * den(157)
  T4sum(1:70,179) = T4sum(1:70,179) + Gcoeff * G4tensor(:,59)
  Gcoeff = (c(6)*(M(136)-M(138)+M(139)-M(140)+M(141)-M(144)+M(147)-M(150)-M(178)+M(190)-M(194)+M(196)-M(200)+M(214)-M(224) &
    +M(238))) * den(1311)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,149)
  Gcoeff = (c(5)*(-M(64)+M(82)-M(87)+M(94)-M(106)+M(114)-M(117)+M(120)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(136)+M(144)-M(147) &
    +M(150)-M(190)+M(194)-M(196)+M(200))) * den(328)
  T4sum(1:70,190) = T4sum(1:70,190) + Gcoeff * G4tensor(:,158)
  Gcoeff = (c(5)*(-M(64)+M(82)-M(87)+M(94)-M(106)+M(114)-M(117)+M(120)-M(125)+M(127)+M(129)-M(130))+c(6)*(-M(138)+M(139)-M(140) &
    +M(141)-M(178)+M(214)-M(224)+M(238))) * den(328)
  T4sum(1:70,191) = T4sum(1:70,191) + Gcoeff * G4tensor(:,108)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    +M(63)-M(64)-M(70)+M(75)+M(82)-M(87)+M(92)-M(93)+M(94)+M(105)-M(106)-M(108)+M(111)+M(114)-M(117)-M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(144)-M(147)-M(190)+M(200))) * den(16)
  T5sum(1:126,37) = T5sum(1:126,37) + Gcoeff * G5tensor(:,5)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    +M(63)+M(64)-M(70)+M(75)-M(82)+M(87)+M(92)-M(93)-M(94)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(136)-M(150)-M(194)+M(196))) * den(16)
  T5sum(1:126,38) = T5sum(1:126,38) + Gcoeff * G5tensor(:,29)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)+M(64)+M(70)-M(75)-M(82)+M(87)+M(92)+M(93)-M(94)-M(105)+M(106)+M(108)-M(111)-M(114)+M(117)+M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(138)-M(141)-M(214)+M(224))) * den(16)
  T5sum(1:126,39) = T5sum(1:126,39) + Gcoeff * G5tensor(:,19)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    +M(63)+M(64)-M(70)+M(75)-M(82)+M(87)+M(92)-M(93)-M(94)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(135)-M(149)-M(218)+M(220))) * den(16)
  T5sum(1:126,40) = T5sum(1:126,40) + Gcoeff * G5tensor(:,30)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(139)-M(140)-M(178)+M(238))) * den(16)
  T5sum(1:126,41) = T5sum(1:126,41) + Gcoeff * G5tensor(:,16)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(145)-M(146)-M(176)+M(236))) * den(16)
  T5sum(1:126,42) = T5sum(1:126,42) + Gcoeff * G5tensor(:,2)
  Gcoeff = (c(6)*(-M(132)+M(135)+M(141)+M(143)-M(144)-M(146)+M(148)-M(151)+M(166)-M(176)-M(200)-M(212)+M(214)+M(220)-M(226) &
    +M(242))) * den(1322)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,150)
  Gcoeff = (c(5)*(M(58)-M(63)-M(75)+M(96)+M(102)-M(105)-M(111)+M(121)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(132)-M(135)-M(141) &
    +M(151)+M(212)-M(214)-M(220)+M(226))) * den(160)
  T4sum(1:70,178) = T4sum(1:70,178) + Gcoeff * G4tensor(:,159)
  Gcoeff = (c(5)*(M(58)-M(63)-M(75)+M(96)+M(102)-M(105)-M(111)+M(121)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(143)-M(144)-M(146) &
    +M(148)+M(166)-M(176)-M(200)+M(242))) * den(160)
  T4sum(1:70,179) = T4sum(1:70,179) + Gcoeff * G4tensor(:,60)
  Gcoeff = (c(6)*(-M(132)+M(133)-M(134)+M(135)+M(142)-M(146)+M(148)-M(152)+M(166)-M(170)+M(172)-M(176)-M(202)+M(220)-M(226) &
    +M(244))) * den(1323)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,151)
  Gcoeff = (c(5)*(-M(76)+M(85)-M(88)+M(97)-M(112)+M(116)-M(118)+M(122)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(142)+M(146)-M(148) &
    +M(152)-M(166)+M(170)-M(172)+M(176))) * den(358)
  T4sum(1:70,193) = T4sum(1:70,193) + Gcoeff * G4tensor(:,160)
  Gcoeff = (c(5)*(-M(76)+M(85)-M(88)+M(97)-M(112)+M(116)-M(118)+M(122)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(132)+M(133)-M(134) &
    +M(135)-M(202)+M(220)-M(226)+M(244))) * den(358)
  T4sum(1:70,194) = T4sum(1:70,194) + Gcoeff * G4tensor(:,132)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(58) &
    +M(63)-M(74)+M(75)-M(76)+M(85)-M(88)+M(95)-M(96)+M(97)-M(102)+M(105)+M(111)-M(112)+M(116)-M(118)-M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(146)-M(148)-M(166)+M(176))) * den(22)
  T5sum(1:126,13) = T5sum(1:126,13) + Gcoeff * G5tensor(:,6)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(58) &
    +M(63)-M(74)+M(75)+M(76)-M(85)+M(88)+M(95)-M(96)-M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(142)-M(152)-M(170)+M(172))) * den(22)
  T5sum(1:126,14) = T5sum(1:126,14) + Gcoeff * G5tensor(:,33)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)+M(76)-M(85)+M(88)+M(95)+M(96)-M(97)+M(102)-M(105)-M(111)+M(112)-M(116)+M(118)+M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(132)-M(135)-M(220)+M(226))) * den(22)
  T5sum(1:126,15) = T5sum(1:126,15) + Gcoeff * G5tensor(:,25)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(58) &
    +M(63)-M(74)+M(75)+M(76)-M(85)+M(88)+M(95)-M(96)-M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(141)-M(151)-M(212)+M(214))) * den(22)
  T5sum(1:126,16) = T5sum(1:126,16) + Gcoeff * G5tensor(:,34)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(134)-M(202)+M(244))) * den(22)
  T5sum(1:126,17) = T5sum(1:126,17) + Gcoeff * G5tensor(:,22)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(143)-M(144)-M(200)+M(242))) * den(22)
  T5sum(1:126,18) = T5sum(1:126,18) + Gcoeff * G5tensor(:,1)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(137)-M(138)-M(140)+M(142)+M(147)-M(153)+M(172)-M(178)-M(188)+M(190)+M(196)-M(202)-M(224) &
    +M(248))) * den(1334)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,152)
  Gcoeff = (c(6)*(M(131)-M(132)-M(134)+M(136)-M(140)+M(142)+M(148)-M(154)-M(164)+M(166)+M(172)-M(178)+M(196)-M(202)-M(226) &
    +M(250))) * den(1335)
  T3sum(1:35,40) = T3sum(1:35,40) + Gcoeff * G3tensor(:,153)
  Gcoeff = (c(5)*(M(61)-M(64)-M(87)+M(99)+M(104)-M(106)-M(117)+M(123)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(134)-M(136)-M(147) &
    +M(153)+M(188)-M(190)-M(196)+M(202))) * den(163)
  T4sum(1:70,190) = T4sum(1:70,190) + Gcoeff * G4tensor(:,161)
  Gcoeff = (c(5)*(M(61)-M(64)-M(87)+M(99)+M(104)-M(106)-M(117)+M(123)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(137)-M(138)-M(140) &
    +M(142)+M(172)-M(178)-M(224)+M(248))) * den(163)
  T4sum(1:70,191) = T4sum(1:70,191) + Gcoeff * G4tensor(:,109)
  Gcoeff = (c(5)*(M(73)-M(76)-M(88)+M(100)+M(110)-M(112)-M(118)+M(124)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(140)-M(142)-M(148) &
    +M(154)+M(164)-M(166)-M(172)+M(178))) * den(361)
  T4sum(1:70,193) = T4sum(1:70,193) + Gcoeff * G4tensor(:,162)
  Gcoeff = (c(5)*(M(73)-M(76)-M(88)+M(100)+M(110)-M(112)-M(118)+M(124)+M(125)-M(126)-M(128)+M(130))+c(6)*(M(131)-M(132)-M(134) &
    +M(136)+M(196)-M(202)-M(226)+M(250))) * den(361)
  T4sum(1:70,194) = T4sum(1:70,194) + Gcoeff * G4tensor(:,133)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(61) &
    +M(64)+M(73)-M(76)-M(86)+M(87)-M(88)+M(98)-M(99)+M(100)-M(104)+M(106)+M(110)-M(112)+M(117)-M(118)-M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(140)-M(142)-M(172)+M(178))) * den(26)
  T5sum(1:126,1) = T5sum(1:126,1) + Gcoeff * G5tensor(:,20)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(61) &
    +M(64)-M(73)+M(76)-M(86)+M(87)+M(88)+M(98)-M(99)-M(100)-M(104)+M(106)-M(110)+M(112)+M(117)+M(118)-M(123)-M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(148)-M(154)-M(164)+M(166))) * den(26)
  T5sum(1:126,2) = T5sum(1:126,2) + Gcoeff * G5tensor(:,35)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)+M(61) &
    -M(64)-M(73)+M(76)-M(86)-M(87)+M(88)+M(98)+M(99)-M(100)+M(104)-M(106)-M(110)+M(112)-M(117)+M(118)+M(123)-M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(134)-M(136)-M(196)+M(202))) * den(26)
  T5sum(1:126,3) = T5sum(1:126,3) + Gcoeff * G5tensor(:,26)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(61) &
    +M(64)-M(73)+M(76)-M(86)+M(87)+M(88)+M(98)-M(99)-M(100)-M(104)+M(106)-M(110)+M(112)+M(117)+M(118)-M(123)-M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(147)-M(153)-M(188)+M(190))) * den(26)
  T5sum(1:126,4) = T5sum(1:126,4) + Gcoeff * G5tensor(:,36)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)+M(61) &
    -M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(131)-M(132)-M(226)+M(250))) * den(26)
  T5sum(1:126,5) = T5sum(1:126,5) + Gcoeff * G5tensor(:,21)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(17)+M(18)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)+M(61) &
    -M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(137)-M(138)-M(224)+M(248))) * den(26)
  T5sum(1:126,6) = T5sum(1:126,6) + Gcoeff * G5tensor(:,15)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(44)+M(45)+M(46)+M(47)+M(50)+M(52)+M(56)+M(59)+M(60)+M(61)+M(62)+M(63)+M(72)+M(75)+M(76)+M(83)+M(86)+M(88)+M(95)+M(99) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(142)+M(172)))
  T6sum(1:210,1) = T6sum(1:210,1) + Gcoeff * G6tensor(:,17)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(44)+M(45)+M(46)+M(47)+M(49)+M(50)+M(56)+M(59)+M(60)+M(62)+M(63)+M(64)+M(72)+M(73)+M(75)+M(83)+M(87)+M(95)+M(98) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(140)+M(178)))
  T6sum(1:210,4) = T6sum(1:210,4) + Gcoeff * G6tensor(:,15)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(44)+M(47)+M(48)+M(49)+M(50)+M(51)+M(56)+M(57)+M(58)+M(59)+M(62)+M(64)+M(71)+M(74)+M(76)+M(84)+M(87)+M(88)+M(96)+M(98) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(148)+M(166)))
  T6sum(1:210,7) = T6sum(1:210,7) + Gcoeff * G6tensor(:,5)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(44)+M(46)+M(47)+M(48)+M(49)+M(50)+M(56)+M(57)+M(59)+M(62)+M(63)+M(64)+M(71)+M(75)+M(84)+M(85)+M(87)+M(95)+M(97)+M(98) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(146)+M(176)))
  T6sum(1:210,10) = T6sum(1:210,10) + Gcoeff * G6tensor(:,3)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(44)+M(47)+M(48)+M(50)+M(51)+M(52)+M(56)+M(57)+M(58)+M(59)+M(61)+M(62)+M(71)+M(73)+M(74)+M(84)+M(86)+M(96)+M(99) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(154)+M(164)))
  T6sum(1:210,13) = T6sum(1:210,13) + Gcoeff * G6tensor(:,11)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(44)+M(45)+M(47)+M(50)+M(51)+M(52)+M(56)+M(58)+M(59)+M(60)+M(61)+M(62)+M(72)+M(74)+M(83)+M(85)+M(86)+M(96)+M(97)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(152)+M(170)))
  T6sum(1:210,16) = T6sum(1:210,16) + Gcoeff * G6tensor(:,9)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(48)+M(49)+M(51)+M(60)+M(61)+M(63)+M(68)+M(71)+M(72)+M(74)+M(75)+M(76)+M(80)+M(88)+M(92)+M(98)+M(99) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(134)+M(202)))
  T6sum(1:210,19) = T6sum(1:210,19) + Gcoeff * G6tensor(:,21)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(48)+M(51)+M(52)+M(60)+M(63)+M(64)+M(68)+M(71)+M(72)+M(73)+M(74)+M(75)+M(80)+M(86)+M(87)+M(92) &
    +M(100)+M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(136)+M(196)))
  T6sum(1:210,22) = T6sum(1:210,22) + Gcoeff * G6tensor(:,23)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(47)+M(48)+M(49)+M(50)+M(51)+M(59)+M(62)+M(64)+M(68)+M(69)+M(70)+M(71)+M(74)+M(76)+M(81)+M(87)+M(88)+M(93)+M(98) &
    +M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(147)+M(190)))
  T6sum(1:210,25) = T6sum(1:210,25) + Gcoeff * G6tensor(:,6)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(41)+M(43)+M(47)+M(48)+M(49)+M(51)+M(59)+M(63)+M(68)+M(69)+M(71)+M(74)+M(75)+M(76)+M(81)+M(82)+M(88)+M(92)+M(94)+M(98) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(144)+M(200)))
  T6sum(1:210,28) = T6sum(1:210,28) + Gcoeff * G6tensor(:,1)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(47)+M(48)+M(50)+M(51)+M(52)+M(59)+M(61)+M(62)+M(68)+M(69)+M(70)+M(71)+M(73)+M(74)+M(81)+M(86)+M(93)+M(99) &
    +M(100)+M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(153)+M(188)))
  T6sum(1:210,31) = T6sum(1:210,31) + Gcoeff * G6tensor(:,12)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(48)+M(50)+M(51)+M(52)+M(60)+M(62)+M(68)+M(70)+M(71)+M(72)+M(73)+M(74)+M(80)+M(82)+M(86)+M(93)+M(94) &
    +M(100)+M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(150)+M(194)))
  T6sum(1:210,34) = T6sum(1:210,34) + Gcoeff * G6tensor(:,7)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(45)+M(46)+M(52)+M(57)+M(58)+M(64)+M(68)+M(76)+M(80)+M(83)+M(84)+M(86)+M(87)+M(88)+M(92)+M(95)+M(96) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(132)+M(226)))
  T6sum(1:210,37) = T6sum(1:210,37) + Gcoeff * G6tensor(:,19)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(45)+M(51)+M(52)+M(57)+M(63)+M(64)+M(68)+M(74)+M(75)+M(80)+M(83)+M(84)+M(85)+M(86)+M(87)+M(92)+M(97) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(135)+M(220)))
  T6sum(1:210,40) = T6sum(1:210,40) + Gcoeff * G6tensor(:,24)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(42)+M(43)+M(44)+M(45)+M(46)+M(52)+M(56)+M(64)+M(69)+M(70)+M(76)+M(80)+M(81)+M(83)+M(86)+M(87)+M(88)+M(92)+M(93)+M(95) &
    +M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(138)+M(224)))
  T6sum(1:210,43) = T6sum(1:210,43) + Gcoeff * G6tensor(:,13)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(42)+M(44)+M(45)+M(46)+M(50)+M(52)+M(56)+M(62)+M(63)+M(69)+M(75)+M(76)+M(80)+M(81)+M(82)+M(83)+M(86)+M(88)+M(94)+M(95) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(141)+M(214)))
  T6sum(1:210,46) = T6sum(1:210,46) + Gcoeff * G6tensor(:,18)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(44)+M(45)+M(50)+M(51)+M(52)+M(56)+M(58)+M(62)+M(69)+M(74)+M(80)+M(81)+M(82)+M(83)+M(85)+M(86)+M(94)+M(96)+M(97) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(151)+M(212)))
  T6sum(1:210,49) = T6sum(1:210,49) + Gcoeff * G6tensor(:,10)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(45)+M(50)+M(51)+M(52)+M(57)+M(62)+M(68)+M(70)+M(74)+M(80)+M(82)+M(83)+M(84)+M(85)+M(86)+M(93)+M(94)+M(97) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(149)+M(218)))
  T6sum(1:210,52) = T6sum(1:210,52) + Gcoeff * G6tensor(:,8)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(45)+M(46)+M(49)+M(57)+M(58)+M(61)+M(68)+M(73)+M(80)+M(83)+M(84)+M(92)+M(95)+M(96)+M(98)+M(99) &
    +M(100)+M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(131)+M(250)))
  T6sum(1:210,55) = T6sum(1:210,55) + Gcoeff * G6tensor(:,20)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(46)+M(48)+M(49)+M(58)+M(60)+M(61)+M(68)+M(71)+M(72)+M(80)+M(85)+M(92)+M(95)+M(96)+M(97)+M(98)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(133)+M(244)))
  T6sum(1:210,58) = T6sum(1:210,58) + Gcoeff * G6tensor(:,22)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(43)+M(44)+M(45)+M(46)+M(49)+M(56)+M(61)+M(69)+M(70)+M(73)+M(80)+M(81)+M(83)+M(92)+M(93)+M(95)+M(98)+M(99) &
    +M(100)+M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(137)+M(248)))
  T6sum(1:210,61) = T6sum(1:210,61) + Gcoeff * G6tensor(:,14)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(43)+M(44)+M(45)+M(46)+M(47)+M(49)+M(56)+M(59)+M(60)+M(70)+M(72)+M(73)+M(82)+M(83)+M(92)+M(93)+M(94)+M(95)+M(98) &
    +M(100)+M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(139)+M(238)))
  T6sum(1:210,64) = T6sum(1:210,64) + Gcoeff * G6tensor(:,16)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(43)+M(46)+M(47)+M(48)+M(49)+M(58)+M(59)+M(68)+M(69)+M(71)+M(81)+M(82)+M(85)+M(92)+M(94)+M(95)+M(96)+M(97)+M(98) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(143)+M(242)))
  T6sum(1:210,67) = T6sum(1:210,67) + Gcoeff * G6tensor(:,2)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(43)+M(44)+M(46)+M(47)+M(48)+M(49)+M(56)+M(57)+M(59)+M(70)+M(71)+M(82)+M(84)+M(85)+M(92)+M(93)+M(94)+M(95)+M(97)+M(98) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(145)+M(236)))
  T6sum(1:210,70) = T6sum(1:210,70) + Gcoeff * G6tensor(:,4)

end subroutine vamp_12

end module ol_vamp_12_ppjjjj_gggggg_1_/**/REALKIND
