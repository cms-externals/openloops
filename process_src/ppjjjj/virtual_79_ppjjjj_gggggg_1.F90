
module ol_vamp_79_ppjjjj_gggggg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_79(M)
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
  complex(REALKIND), dimension(4,1,4,257) :: G0
  complex(REALKIND), dimension(1,150) :: G0tensor
  complex(REALKIND), dimension(5,286) :: G1tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,160),wf(:,-5),G0(:,:,:,2))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-4),wf(:,-1),G0tensor(:,1))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,-4),G0tensor(:,2))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-4),wf(:,-1),G0tensor(:,3))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,1))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,160),G0(:,:,:,3))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-4),wf(:,-1),G0tensor(:,4))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-1),wf(:,-4),G0tensor(:,5))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-4),wf(:,-1),G0tensor(:,6))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,2))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,157),G0(:,:,:,4))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,3))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,158),G0(:,:,:,5))
  call check_last_UV_W(l_switch,G0(:,:,:,5),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,4))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,159),G0(:,:,:,6))
  call check_last_UV_W(l_switch,G0(:,:,:,6),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,5))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,157),wf(:,-5),G0(:,:,:,7))
  call check_last_UV_W(l_switch,G0(:,:,:,7),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,6))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,158),wf(:,-5),G0(:,:,:,8))
  call check_last_UV_W(l_switch,G0(:,:,:,8),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,7))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,159),wf(:,-5),G0(:,:,:,9))
  call check_last_UV_W(l_switch,G0(:,:,:,9),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,8))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,157),G0(:,:,:,10))
  call check_last_UV_W(l_switch,G0(:,:,:,10),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,9))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,158),G0(:,:,:,11))
  call check_last_UV_W(l_switch,G0(:,:,:,11),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,10))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,159),G0(:,:,:,12))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,11))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,70),G0(:,:,:,13))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-4),wf(:,104),G0tensor(:,7))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,104),wf(:,-4),G0tensor(:,8))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-4),wf(:,104),G0tensor(:,9))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-3),wf(:,109),G0tensor(:,10))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,109),wf(:,-3),G0tensor(:,11))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-3),wf(:,109),G0tensor(:,12))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,0),wf(:,75),G0tensor(:,13))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,75),wf(:,0),G0tensor(:,14))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,0),wf(:,75),G0tensor(:,15))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,38),wf(:,43),Q(:,25),G1tensor(:,12))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,38),wf(:,45),Q(:,25),G1tensor(:,13))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,38),wf(:,46),Q(:,25),G1tensor(:,14))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,38),wf(:,181),Q(:,25),G1tensor(:,15))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,38),wf(:,186),Q(:,25),G1tensor(:,16))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,38),wf(:,190),Q(:,25),G1tensor(:,17))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,70),wf(:,-1),G0(:,:,:,14))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,14),wf(:,-4),wf(:,104),G0tensor(:,16))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,14),wf(:,104),wf(:,-4),G0tensor(:,17))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,14),wf(:,-4),wf(:,104),G0tensor(:,18))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,14),wf(:,-3),wf(:,109),G0tensor(:,19))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,14),wf(:,109),wf(:,-3),G0tensor(:,20))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,14),wf(:,-3),wf(:,109),G0tensor(:,21))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,14),wf(:,0),wf(:,75),G0tensor(:,22))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,14),wf(:,75),wf(:,0),G0tensor(:,23))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,14),wf(:,0),wf(:,75),G0tensor(:,24))
  call check_last_UV_W(l_switch,G0(:,:,:,14),Q(:,38),wf(:,43),Q(:,25),G1tensor(:,18))
  call check_last_UV_W(l_switch,G0(:,:,:,14),Q(:,38),wf(:,45),Q(:,25),G1tensor(:,19))
  call check_last_UV_W(l_switch,G0(:,:,:,14),Q(:,38),wf(:,46),Q(:,25),G1tensor(:,20))
  call check_last_UV_W(l_switch,G0(:,:,:,14),Q(:,38),wf(:,181),Q(:,25),G1tensor(:,21))
  call check_last_UV_W(l_switch,G0(:,:,:,14),Q(:,38),wf(:,186),Q(:,25),G1tensor(:,22))
  call check_last_UV_W(l_switch,G0(:,:,:,14),Q(:,38),wf(:,190),Q(:,25),G1tensor(:,23))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,70),G0(:,:,:,15))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,15),wf(:,-4),wf(:,104),G0tensor(:,25))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,15),wf(:,104),wf(:,-4),G0tensor(:,26))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,15),wf(:,-4),wf(:,104),G0tensor(:,27))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,15),wf(:,-3),wf(:,109),G0tensor(:,28))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,15),wf(:,109),wf(:,-3),G0tensor(:,29))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,15),wf(:,-3),wf(:,109),G0tensor(:,30))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,15),wf(:,0),wf(:,75),G0tensor(:,31))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,15),wf(:,75),wf(:,0),G0tensor(:,32))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,15),wf(:,0),wf(:,75),G0tensor(:,33))
  call check_last_UV_W(l_switch,G0(:,:,:,15),Q(:,38),wf(:,43),Q(:,25),G1tensor(:,24))
  call check_last_UV_W(l_switch,G0(:,:,:,15),Q(:,38),wf(:,45),Q(:,25),G1tensor(:,25))
  call check_last_UV_W(l_switch,G0(:,:,:,15),Q(:,38),wf(:,46),Q(:,25),G1tensor(:,26))
  call check_last_UV_W(l_switch,G0(:,:,:,15),Q(:,38),wf(:,181),Q(:,25),G1tensor(:,27))
  call check_last_UV_W(l_switch,G0(:,:,:,15),Q(:,38),wf(:,186),Q(:,25),G1tensor(:,28))
  call check_last_UV_W(l_switch,G0(:,:,:,15),Q(:,38),wf(:,190),Q(:,25),G1tensor(:,29))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,154),G0(:,:,:,16))
  call check_last_UV_W(l_switch,G0(:,:,:,16),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,30))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,155),G0(:,:,:,17))
  call check_last_UV_W(l_switch,G0(:,:,:,17),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,31))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,156),G0(:,:,:,18))
  call check_last_UV_W(l_switch,G0(:,:,:,18),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,32))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,154),wf(:,-4),G0(:,:,:,19))
  call check_last_UV_W(l_switch,G0(:,:,:,19),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,33))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,155),wf(:,-4),G0(:,:,:,20))
  call check_last_UV_W(l_switch,G0(:,:,:,20),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,34))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,156),wf(:,-4),G0(:,:,:,21))
  call check_last_UV_W(l_switch,G0(:,:,:,21),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,35))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,154),G0(:,:,:,22))
  call check_last_UV_W(l_switch,G0(:,:,:,22),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,36))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,155),G0(:,:,:,23))
  call check_last_UV_W(l_switch,G0(:,:,:,23),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,37))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,156),G0(:,:,:,24))
  call check_last_UV_W(l_switch,G0(:,:,:,24),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,38))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,106),G0(:,:,:,25))
  call check_last_UV_W(l_switch,G0(:,:,:,25),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,39))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,106),wf(:,-1),G0(:,:,:,26))
  call check_last_UV_W(l_switch,G0(:,:,:,26),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,40))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,106),G0(:,:,:,27))
  call check_last_UV_W(l_switch,G0(:,:,:,27),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,41))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,107),G0(:,:,:,28))
  call check_last_UV_W(l_switch,G0(:,:,:,28),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,42))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,107),wf(:,-1),G0(:,:,:,29))
  call check_last_UV_W(l_switch,G0(:,:,:,29),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,43))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,107),G0(:,:,:,30))
  call check_last_UV_W(l_switch,G0(:,:,:,30),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,44))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,108),G0(:,:,:,31))
  call check_last_UV_W(l_switch,G0(:,:,:,31),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,45))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,108),wf(:,-1),G0(:,:,:,32))
  call check_last_UV_W(l_switch,G0(:,:,:,32),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,46))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,108),G0(:,:,:,33))
  call check_last_UV_W(l_switch,G0(:,:,:,33),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,47))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,62),wf(:,113),G0(:,:,:,34))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,34),wf(:,-4),wf(:,-1),G0tensor(:,34))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,34),wf(:,-1),wf(:,-4),G0tensor(:,35))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,34),wf(:,-4),wf(:,-1),G0tensor(:,36))
  call check_last_UV_W(l_switch,G0(:,:,:,34),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,48))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,113),wf(:,62),G0(:,:,:,35))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,35),wf(:,-4),wf(:,-1),G0tensor(:,37))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,35),wf(:,-1),wf(:,-4),G0tensor(:,38))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,35),wf(:,-4),wf(:,-1),G0tensor(:,39))
  call check_last_UV_W(l_switch,G0(:,:,:,35),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,49))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,62),wf(:,113),G0(:,:,:,36))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,36),wf(:,-4),wf(:,-1),G0tensor(:,40))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,36),wf(:,-1),wf(:,-4),G0tensor(:,41))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,36),wf(:,-4),wf(:,-1),G0tensor(:,42))
  call check_last_UV_W(l_switch,G0(:,:,:,36),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,50))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,26),wf(:,62),G0(:,:,:,37))
  call check_last_UV_W(l_switch,G0(:,:,:,37),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,51))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,29),wf(:,62),G0(:,:,:,38))
  call check_last_UV_W(l_switch,G0(:,:,:,38),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,52))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,30),wf(:,62),G0(:,:,:,39))
  call check_last_UV_W(l_switch,G0(:,:,:,39),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,53))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,62),wf(:,26),G0(:,:,:,40))
  call check_last_UV_W(l_switch,G0(:,:,:,40),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,54))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,62),wf(:,29),G0(:,:,:,41))
  call check_last_UV_W(l_switch,G0(:,:,:,41),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,55))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,62),wf(:,30),G0(:,:,:,42))
  call check_last_UV_W(l_switch,G0(:,:,:,42),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,56))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,26),wf(:,62),G0(:,:,:,43))
  call check_last_UV_W(l_switch,G0(:,:,:,43),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,57))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,29),wf(:,62),G0(:,:,:,44))
  call check_last_UV_W(l_switch,G0(:,:,:,44),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,58))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,30),wf(:,62),G0(:,:,:,45))
  call check_last_UV_W(l_switch,G0(:,:,:,45),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,59))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,167),G0(:,:,:,46))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,46),wf(:,-4),wf(:,-1),G0tensor(:,43))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,46),wf(:,-1),wf(:,-4),G0tensor(:,44))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,46),wf(:,-4),wf(:,-1),G0tensor(:,45))
  call check_last_UV_W(l_switch,G0(:,:,:,46),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,60))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,167),wf(:,-5),G0(:,:,:,47))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-4),wf(:,-1),G0tensor(:,46))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-1),wf(:,-4),G0tensor(:,47))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,-4),wf(:,-1),G0tensor(:,48))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,61))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,167),G0(:,:,:,48))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-4),wf(:,-1),G0tensor(:,49))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-1),wf(:,-4),G0tensor(:,50))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,-4),wf(:,-1),G0tensor(:,51))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,62))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,955),G0(:,:,:,49))
  call check_last_UV_W(l_switch,G0(:,:,:,49),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,63))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,956),G0(:,:,:,50))
  call check_last_UV_W(l_switch,G0(:,:,:,50),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,64))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,957),G0(:,:,:,51))
  call check_last_UV_W(l_switch,G0(:,:,:,51),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,65))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,955),wf(:,-5),G0(:,:,:,52))
  call check_last_UV_W(l_switch,G0(:,:,:,52),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,66))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,956),wf(:,-5),G0(:,:,:,53))
  call check_last_UV_W(l_switch,G0(:,:,:,53),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,67))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,957),wf(:,-5),G0(:,:,:,54))
  call check_last_UV_W(l_switch,G0(:,:,:,54),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,68))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,955),G0(:,:,:,55))
  call check_last_UV_W(l_switch,G0(:,:,:,55),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,69))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,956),G0(:,:,:,56))
  call check_last_UV_W(l_switch,G0(:,:,:,56),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,70))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,957),G0(:,:,:,57))
  call check_last_UV_W(l_switch,G0(:,:,:,57),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,71))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,113),G0(:,:,:,58))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,58),wf(:,-4),wf(:,62),G0tensor(:,52))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,58),wf(:,62),wf(:,-4),G0tensor(:,53))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,58),wf(:,-4),wf(:,62),G0tensor(:,54))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,58),wf(:,-3),wf(:,66),G0tensor(:,55))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,58),wf(:,66),wf(:,-3),G0tensor(:,56))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,58),wf(:,-3),wf(:,66),G0tensor(:,57))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,58),wf(:,-2),wf(:,75),G0tensor(:,58))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,58),wf(:,75),wf(:,-2),G0tensor(:,59))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,58),wf(:,-2),wf(:,75),G0tensor(:,60))
  call check_last_UV_W(l_switch,G0(:,:,:,58),Q(:,35),wf(:,20),Q(:,28),G1tensor(:,72))
  call check_last_UV_W(l_switch,G0(:,:,:,58),Q(:,35),wf(:,23),Q(:,28),G1tensor(:,73))
  call check_last_UV_W(l_switch,G0(:,:,:,58),Q(:,35),wf(:,24),Q(:,28),G1tensor(:,74))
  call check_last_UV_W(l_switch,G0(:,:,:,58),Q(:,35),wf(:,253),Q(:,28),G1tensor(:,75))
  call check_last_UV_W(l_switch,G0(:,:,:,58),Q(:,35),wf(:,258),Q(:,28),G1tensor(:,76))
  call check_last_UV_W(l_switch,G0(:,:,:,58),Q(:,35),wf(:,262),Q(:,28),G1tensor(:,77))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,113),wf(:,-1),G0(:,:,:,59))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,59),wf(:,-4),wf(:,62),G0tensor(:,61))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,59),wf(:,62),wf(:,-4),G0tensor(:,62))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,59),wf(:,-4),wf(:,62),G0tensor(:,63))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,59),wf(:,-3),wf(:,66),G0tensor(:,64))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,59),wf(:,66),wf(:,-3),G0tensor(:,65))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,59),wf(:,-3),wf(:,66),G0tensor(:,66))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,59),wf(:,-2),wf(:,75),G0tensor(:,67))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,59),wf(:,75),wf(:,-2),G0tensor(:,68))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,59),wf(:,-2),wf(:,75),G0tensor(:,69))
  call check_last_UV_W(l_switch,G0(:,:,:,59),Q(:,35),wf(:,20),Q(:,28),G1tensor(:,78))
  call check_last_UV_W(l_switch,G0(:,:,:,59),Q(:,35),wf(:,23),Q(:,28),G1tensor(:,79))
  call check_last_UV_W(l_switch,G0(:,:,:,59),Q(:,35),wf(:,24),Q(:,28),G1tensor(:,80))
  call check_last_UV_W(l_switch,G0(:,:,:,59),Q(:,35),wf(:,253),Q(:,28),G1tensor(:,81))
  call check_last_UV_W(l_switch,G0(:,:,:,59),Q(:,35),wf(:,258),Q(:,28),G1tensor(:,82))
  call check_last_UV_W(l_switch,G0(:,:,:,59),Q(:,35),wf(:,262),Q(:,28),G1tensor(:,83))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,113),G0(:,:,:,60))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,60),wf(:,-4),wf(:,62),G0tensor(:,70))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,60),wf(:,62),wf(:,-4),G0tensor(:,71))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,60),wf(:,-4),wf(:,62),G0tensor(:,72))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,60),wf(:,-3),wf(:,66),G0tensor(:,73))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,60),wf(:,66),wf(:,-3),G0tensor(:,74))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,60),wf(:,-3),wf(:,66),G0tensor(:,75))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,60),wf(:,-2),wf(:,75),G0tensor(:,76))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,60),wf(:,75),wf(:,-2),G0tensor(:,77))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,60),wf(:,-2),wf(:,75),G0tensor(:,78))
  call check_last_UV_W(l_switch,G0(:,:,:,60),Q(:,35),wf(:,20),Q(:,28),G1tensor(:,84))
  call check_last_UV_W(l_switch,G0(:,:,:,60),Q(:,35),wf(:,23),Q(:,28),G1tensor(:,85))
  call check_last_UV_W(l_switch,G0(:,:,:,60),Q(:,35),wf(:,24),Q(:,28),G1tensor(:,86))
  call check_last_UV_W(l_switch,G0(:,:,:,60),Q(:,35),wf(:,253),Q(:,28),G1tensor(:,87))
  call check_last_UV_W(l_switch,G0(:,:,:,60),Q(:,35),wf(:,258),Q(:,28),G1tensor(:,88))
  call check_last_UV_W(l_switch,G0(:,:,:,60),Q(:,35),wf(:,262),Q(:,28),G1tensor(:,89))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,952),G0(:,:,:,61))
  call check_last_UV_W(l_switch,G0(:,:,:,61),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,90))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,953),G0(:,:,:,62))
  call check_last_UV_W(l_switch,G0(:,:,:,62),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,91))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,954),G0(:,:,:,63))
  call check_last_UV_W(l_switch,G0(:,:,:,63),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,92))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,952),wf(:,-4),G0(:,:,:,64))
  call check_last_UV_W(l_switch,G0(:,:,:,64),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,93))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,953),wf(:,-4),G0(:,:,:,65))
  call check_last_UV_W(l_switch,G0(:,:,:,65),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,94))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,954),wf(:,-4),G0(:,:,:,66))
  call check_last_UV_W(l_switch,G0(:,:,:,66),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,95))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,952),G0(:,:,:,67))
  call check_last_UV_W(l_switch,G0(:,:,:,67),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,96))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,953),G0(:,:,:,68))
  call check_last_UV_W(l_switch,G0(:,:,:,68),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,97))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,954),G0(:,:,:,69))
  call check_last_UV_W(l_switch,G0(:,:,:,69),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,98))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,782),G0(:,:,:,70))
  call check_last_UV_W(l_switch,G0(:,:,:,70),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,99))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,782),wf(:,-1),G0(:,:,:,71))
  call check_last_UV_W(l_switch,G0(:,:,:,71),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,100))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,782),G0(:,:,:,72))
  call check_last_UV_W(l_switch,G0(:,:,:,72),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,101))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,784),G0(:,:,:,73))
  call check_last_UV_W(l_switch,G0(:,:,:,73),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,102))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,784),wf(:,-1),G0(:,:,:,74))
  call check_last_UV_W(l_switch,G0(:,:,:,74),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,103))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,784),G0(:,:,:,75))
  call check_last_UV_W(l_switch,G0(:,:,:,75),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,104))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,785),G0(:,:,:,76))
  call check_last_UV_W(l_switch,G0(:,:,:,76),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,105))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,785),wf(:,-1),G0(:,:,:,77))
  call check_last_UV_W(l_switch,G0(:,:,:,77),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,106))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,785),G0(:,:,:,78))
  call check_last_UV_W(l_switch,G0(:,:,:,78),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,107))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,70),wf(:,109),G0(:,:,:,79))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,79),wf(:,-3),wf(:,-1),G0tensor(:,79))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,79),wf(:,-1),wf(:,-3),G0tensor(:,80))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,79),wf(:,-3),wf(:,-1),G0tensor(:,81))
  call check_last_UV_W(l_switch,G0(:,:,:,79),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,108))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,109),wf(:,70),G0(:,:,:,80))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,80),wf(:,-3),wf(:,-1),G0tensor(:,82))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,80),wf(:,-1),wf(:,-3),G0tensor(:,83))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,80),wf(:,-3),wf(:,-1),G0tensor(:,84))
  call check_last_UV_W(l_switch,G0(:,:,:,80),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,109))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,70),wf(:,109),G0(:,:,:,81))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,81),wf(:,-3),wf(:,-1),G0tensor(:,85))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,81),wf(:,-1),wf(:,-3),G0tensor(:,86))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,81),wf(:,-3),wf(:,-1),G0tensor(:,87))
  call check_last_UV_W(l_switch,G0(:,:,:,81),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,110))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,32),wf(:,109),G0(:,:,:,82))
  call check_last_UV_W(l_switch,G0(:,:,:,82),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,111))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,35),wf(:,109),G0(:,:,:,83))
  call check_last_UV_W(l_switch,G0(:,:,:,83),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,112))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,36),wf(:,109),G0(:,:,:,84))
  call check_last_UV_W(l_switch,G0(:,:,:,84),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,113))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,109),wf(:,32),G0(:,:,:,85))
  call check_last_UV_W(l_switch,G0(:,:,:,85),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,114))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,109),wf(:,35),G0(:,:,:,86))
  call check_last_UV_W(l_switch,G0(:,:,:,86),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,115))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,109),wf(:,36),G0(:,:,:,87))
  call check_last_UV_W(l_switch,G0(:,:,:,87),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,116))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,32),wf(:,109),G0(:,:,:,88))
  call check_last_UV_W(l_switch,G0(:,:,:,88),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,117))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,35),wf(:,109),G0(:,:,:,89))
  call check_last_UV_W(l_switch,G0(:,:,:,89),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,118))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,36),wf(:,109),G0(:,:,:,90))
  call check_last_UV_W(l_switch,G0(:,:,:,90),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,119))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,171),G0(:,:,:,91))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,91),wf(:,-3),wf(:,-1),G0tensor(:,88))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,91),wf(:,-1),wf(:,-3),G0tensor(:,89))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,91),wf(:,-3),wf(:,-1),G0tensor(:,90))
  call check_last_UV_W(l_switch,G0(:,:,:,91),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,120))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,171),wf(:,-5),G0(:,:,:,92))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,92),wf(:,-3),wf(:,-1),G0tensor(:,91))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,92),wf(:,-1),wf(:,-3),G0tensor(:,92))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,92),wf(:,-3),wf(:,-1),G0tensor(:,93))
  call check_last_UV_W(l_switch,G0(:,:,:,92),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,121))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,171),G0(:,:,:,93))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,93),wf(:,-3),wf(:,-1),G0tensor(:,94))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,93),wf(:,-1),wf(:,-3),G0tensor(:,95))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,93),wf(:,-3),wf(:,-1),G0tensor(:,96))
  call check_last_UV_W(l_switch,G0(:,:,:,93),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,122))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,168),G0(:,:,:,94))
  call check_last_UV_W(l_switch,G0(:,:,:,94),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,123))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,169),G0(:,:,:,95))
  call check_last_UV_W(l_switch,G0(:,:,:,95),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,124))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,170),G0(:,:,:,96))
  call check_last_UV_W(l_switch,G0(:,:,:,96),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,125))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,168),wf(:,-5),G0(:,:,:,97))
  call check_last_UV_W(l_switch,G0(:,:,:,97),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,126))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,169),wf(:,-5),G0(:,:,:,98))
  call check_last_UV_W(l_switch,G0(:,:,:,98),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,127))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,170),wf(:,-5),G0(:,:,:,99))
  call check_last_UV_W(l_switch,G0(:,:,:,99),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,128))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,168),G0(:,:,:,100))
  call check_last_UV_W(l_switch,G0(:,:,:,100),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,129))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,169),G0(:,:,:,101))
  call check_last_UV_W(l_switch,G0(:,:,:,101),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,130))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,170),G0(:,:,:,102))
  call check_last_UV_W(l_switch,G0(:,:,:,102),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,131))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,66),wf(:,113),G0(:,:,:,103))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,103),wf(:,-3),wf(:,-1),G0tensor(:,97))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,103),wf(:,-1),wf(:,-3),G0tensor(:,98))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,103),wf(:,-3),wf(:,-1),G0tensor(:,99))
  call check_last_UV_W(l_switch,G0(:,:,:,103),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,132))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,113),wf(:,66),G0(:,:,:,104))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,104),wf(:,-3),wf(:,-1),G0tensor(:,100))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,104),wf(:,-1),wf(:,-3),G0tensor(:,101))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,104),wf(:,-3),wf(:,-1),G0tensor(:,102))
  call check_last_UV_W(l_switch,G0(:,:,:,104),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,133))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,66),wf(:,113),G0(:,:,:,105))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,105),wf(:,-3),wf(:,-1),G0tensor(:,103))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,105),wf(:,-1),wf(:,-3),G0tensor(:,104))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,105),wf(:,-3),wf(:,-1),G0tensor(:,105))
  call check_last_UV_W(l_switch,G0(:,:,:,105),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,134))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,32),wf(:,66),G0(:,:,:,106))
  call check_last_UV_W(l_switch,G0(:,:,:,106),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,135))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,35),wf(:,66),G0(:,:,:,107))
  call check_last_UV_W(l_switch,G0(:,:,:,107),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,136))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,36),wf(:,66),G0(:,:,:,108))
  call check_last_UV_W(l_switch,G0(:,:,:,108),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,137))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,66),wf(:,32),G0(:,:,:,109))
  call check_last_UV_W(l_switch,G0(:,:,:,109),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,138))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,66),wf(:,35),G0(:,:,:,110))
  call check_last_UV_W(l_switch,G0(:,:,:,110),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,139))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,66),wf(:,36),G0(:,:,:,111))
  call check_last_UV_W(l_switch,G0(:,:,:,111),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,140))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,32),wf(:,66),G0(:,:,:,112))
  call check_last_UV_W(l_switch,G0(:,:,:,112),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,141))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,35),wf(:,66),G0(:,:,:,113))
  call check_last_UV_W(l_switch,G0(:,:,:,113),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,142))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,36),wf(:,66),G0(:,:,:,114))
  call check_last_UV_W(l_switch,G0(:,:,:,114),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,143))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,175),G0(:,:,:,115))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,115),wf(:,-3),wf(:,-1),G0tensor(:,106))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,115),wf(:,-1),wf(:,-3),G0tensor(:,107))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,115),wf(:,-3),wf(:,-1),G0tensor(:,108))
  call check_last_UV_W(l_switch,G0(:,:,:,115),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,144))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,175),wf(:,-5),G0(:,:,:,116))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,116),wf(:,-3),wf(:,-1),G0tensor(:,109))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,116),wf(:,-1),wf(:,-3),G0tensor(:,110))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,116),wf(:,-3),wf(:,-1),G0tensor(:,111))
  call check_last_UV_W(l_switch,G0(:,:,:,116),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,145))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,175),G0(:,:,:,117))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,117),wf(:,-3),wf(:,-1),G0tensor(:,112))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,117),wf(:,-1),wf(:,-3),G0tensor(:,113))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,117),wf(:,-3),wf(:,-1),G0tensor(:,114))
  call check_last_UV_W(l_switch,G0(:,:,:,117),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,146))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,967),G0(:,:,:,118))
  call check_last_UV_W(l_switch,G0(:,:,:,118),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,147))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,968),G0(:,:,:,119))
  call check_last_UV_W(l_switch,G0(:,:,:,119),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,148))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,969),G0(:,:,:,120))
  call check_last_UV_W(l_switch,G0(:,:,:,120),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,149))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,967),wf(:,-5),G0(:,:,:,121))
  call check_last_UV_W(l_switch,G0(:,:,:,121),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,150))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,968),wf(:,-5),G0(:,:,:,122))
  call check_last_UV_W(l_switch,G0(:,:,:,122),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,151))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,969),wf(:,-5),G0(:,:,:,123))
  call check_last_UV_W(l_switch,G0(:,:,:,123),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,152))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,967),G0(:,:,:,124))
  call check_last_UV_W(l_switch,G0(:,:,:,124),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,153))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,968),G0(:,:,:,125))
  call check_last_UV_W(l_switch,G0(:,:,:,125),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,154))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,969),G0(:,:,:,126))
  call check_last_UV_W(l_switch,G0(:,:,:,126),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,155))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,38),wf(:,113),G0(:,:,:,127))
  call check_last_UV_W(l_switch,G0(:,:,:,127),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,156))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,41),wf(:,113),G0(:,:,:,128))
  call check_last_UV_W(l_switch,G0(:,:,:,128),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,157))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,42),wf(:,113),G0(:,:,:,129))
  call check_last_UV_W(l_switch,G0(:,:,:,129),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,158))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,113),wf(:,38),G0(:,:,:,130))
  call check_last_UV_W(l_switch,G0(:,:,:,130),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,159))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,113),wf(:,41),G0(:,:,:,131))
  call check_last_UV_W(l_switch,G0(:,:,:,131),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,160))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,113),wf(:,42),G0(:,:,:,132))
  call check_last_UV_W(l_switch,G0(:,:,:,132),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,161))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,38),wf(:,113),G0(:,:,:,133))
  call check_last_UV_W(l_switch,G0(:,:,:,133),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,162))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,41),wf(:,113),G0(:,:,:,134))
  call check_last_UV_W(l_switch,G0(:,:,:,134),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,163))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,42),wf(:,113),G0(:,:,:,135))
  call check_last_UV_W(l_switch,G0(:,:,:,135),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,164))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,38),wf(:,70),G0(:,:,:,136))
  call check_last_UV_W(l_switch,G0(:,:,:,136),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,165))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,41),wf(:,70),G0(:,:,:,137))
  call check_last_UV_W(l_switch,G0(:,:,:,137),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,166))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,42),wf(:,70),G0(:,:,:,138))
  call check_last_UV_W(l_switch,G0(:,:,:,138),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,167))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,70),wf(:,38),G0(:,:,:,139))
  call check_last_UV_W(l_switch,G0(:,:,:,139),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,168))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,70),wf(:,41),G0(:,:,:,140))
  call check_last_UV_W(l_switch,G0(:,:,:,140),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,169))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,70),wf(:,42),G0(:,:,:,141))
  call check_last_UV_W(l_switch,G0(:,:,:,141),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,170))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,38),wf(:,70),G0(:,:,:,142))
  call check_last_UV_W(l_switch,G0(:,:,:,142),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,171))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,41),wf(:,70),G0(:,:,:,143))
  call check_last_UV_W(l_switch,G0(:,:,:,143),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,172))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,42),wf(:,70),G0(:,:,:,144))
  call check_last_UV_W(l_switch,G0(:,:,:,144),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,173))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,976),G0(:,:,:,145))
  call check_last_UV_W(l_switch,G0(:,:,:,145),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,174))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,977),G0(:,:,:,146))
  call check_last_UV_W(l_switch,G0(:,:,:,146),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,175))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,978),G0(:,:,:,147))
  call check_last_UV_W(l_switch,G0(:,:,:,147),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,176))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,976),wf(:,-5),G0(:,:,:,148))
  call check_last_UV_W(l_switch,G0(:,:,:,148),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,177))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,977),wf(:,-5),G0(:,:,:,149))
  call check_last_UV_W(l_switch,G0(:,:,:,149),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,178))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,978),wf(:,-5),G0(:,:,:,150))
  call check_last_UV_W(l_switch,G0(:,:,:,150),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,179))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,976),G0(:,:,:,151))
  call check_last_UV_W(l_switch,G0(:,:,:,151),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,180))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,977),G0(:,:,:,152))
  call check_last_UV_W(l_switch,G0(:,:,:,152),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,181))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,978),G0(:,:,:,153))
  call check_last_UV_W(l_switch,G0(:,:,:,153),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,182))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,973),G0(:,:,:,154))
  call check_last_UV_W(l_switch,G0(:,:,:,154),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,183))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,974),G0(:,:,:,155))
  call check_last_UV_W(l_switch,G0(:,:,:,155),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,184))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,975),G0(:,:,:,156))
  call check_last_UV_W(l_switch,G0(:,:,:,156),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,185))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,973),wf(:,-5),G0(:,:,:,157))
  call check_last_UV_W(l_switch,G0(:,:,:,157),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,186))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,974),wf(:,-5),G0(:,:,:,158))
  call check_last_UV_W(l_switch,G0(:,:,:,158),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,187))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,975),wf(:,-5),G0(:,:,:,159))
  call check_last_UV_W(l_switch,G0(:,:,:,159),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,188))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,973),G0(:,:,:,160))
  call check_last_UV_W(l_switch,G0(:,:,:,160),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,189))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,974),G0(:,:,:,161))
  call check_last_UV_W(l_switch,G0(:,:,:,161),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,190))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,975),G0(:,:,:,162))
  call check_last_UV_W(l_switch,G0(:,:,:,162),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,191))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,176),G0(:,:,:,163))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,163),wf(:,-3),wf(:,-1),G0tensor(:,115))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,163),wf(:,-1),wf(:,-3),G0tensor(:,116))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,163),wf(:,-3),wf(:,-1),G0tensor(:,117))
  call check_last_UV_W(l_switch,G0(:,:,:,163),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,192))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,176),wf(:,-4),G0(:,:,:,164))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,164),wf(:,-3),wf(:,-1),G0tensor(:,118))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,164),wf(:,-1),wf(:,-3),G0tensor(:,119))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,164),wf(:,-3),wf(:,-1),G0tensor(:,120))
  call check_last_UV_W(l_switch,G0(:,:,:,164),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,193))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,176),G0(:,:,:,165))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,165),wf(:,-3),wf(:,-1),G0tensor(:,121))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,165),wf(:,-1),wf(:,-3),G0tensor(:,122))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,165),wf(:,-3),wf(:,-1),G0tensor(:,123))
  call check_last_UV_W(l_switch,G0(:,:,:,165),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,194))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,172),G0(:,:,:,166))
  call check_last_UV_W(l_switch,G0(:,:,:,166),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,195))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,173),G0(:,:,:,167))
  call check_last_UV_W(l_switch,G0(:,:,:,167),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,196))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,174),G0(:,:,:,168))
  call check_last_UV_W(l_switch,G0(:,:,:,168),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,197))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,172),wf(:,-4),G0(:,:,:,169))
  call check_last_UV_W(l_switch,G0(:,:,:,169),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,198))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,173),wf(:,-4),G0(:,:,:,170))
  call check_last_UV_W(l_switch,G0(:,:,:,170),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,199))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,174),wf(:,-4),G0(:,:,:,171))
  call check_last_UV_W(l_switch,G0(:,:,:,171),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,200))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,172),G0(:,:,:,172))
  call check_last_UV_W(l_switch,G0(:,:,:,172),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,201))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,173),G0(:,:,:,173))
  call check_last_UV_W(l_switch,G0(:,:,:,173),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,202))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,174),G0(:,:,:,174))
  call check_last_UV_W(l_switch,G0(:,:,:,174),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,203))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,177),G0(:,:,:,175))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,175),wf(:,-3),wf(:,-1),G0tensor(:,124))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,175),wf(:,-1),wf(:,-3),G0tensor(:,125))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,175),wf(:,-3),wf(:,-1),G0tensor(:,126))
  call check_last_UV_W(l_switch,G0(:,:,:,175),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,204))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,177),wf(:,-4),G0(:,:,:,176))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,176),wf(:,-3),wf(:,-1),G0tensor(:,127))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,176),wf(:,-1),wf(:,-3),G0tensor(:,128))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,176),wf(:,-3),wf(:,-1),G0tensor(:,129))
  call check_last_UV_W(l_switch,G0(:,:,:,176),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,205))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,177),G0(:,:,:,177))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,177),wf(:,-3),wf(:,-1),G0tensor(:,130))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,177),wf(:,-1),wf(:,-3),G0tensor(:,131))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,177),wf(:,-3),wf(:,-1),G0tensor(:,132))
  call check_last_UV_W(l_switch,G0(:,:,:,177),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,206))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,961),G0(:,:,:,178))
  call check_last_UV_W(l_switch,G0(:,:,:,178),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,207))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,962),G0(:,:,:,179))
  call check_last_UV_W(l_switch,G0(:,:,:,179),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,208))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,963),G0(:,:,:,180))
  call check_last_UV_W(l_switch,G0(:,:,:,180),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,209))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,961),wf(:,-4),G0(:,:,:,181))
  call check_last_UV_W(l_switch,G0(:,:,:,181),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,210))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,962),wf(:,-4),G0(:,:,:,182))
  call check_last_UV_W(l_switch,G0(:,:,:,182),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,211))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,963),wf(:,-4),G0(:,:,:,183))
  call check_last_UV_W(l_switch,G0(:,:,:,183),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,212))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,961),G0(:,:,:,184))
  call check_last_UV_W(l_switch,G0(:,:,:,184),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,213))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,962),G0(:,:,:,185))
  call check_last_UV_W(l_switch,G0(:,:,:,185),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,214))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,963),G0(:,:,:,186))
  call check_last_UV_W(l_switch,G0(:,:,:,186),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,215))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,970),G0(:,:,:,187))
  call check_last_UV_W(l_switch,G0(:,:,:,187),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,216))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,971),G0(:,:,:,188))
  call check_last_UV_W(l_switch,G0(:,:,:,188),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,217))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,972),G0(:,:,:,189))
  call check_last_UV_W(l_switch,G0(:,:,:,189),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,218))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,970),wf(:,-4),G0(:,:,:,190))
  call check_last_UV_W(l_switch,G0(:,:,:,190),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,219))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,971),wf(:,-4),G0(:,:,:,191))
  call check_last_UV_W(l_switch,G0(:,:,:,191),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,220))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,972),wf(:,-4),G0(:,:,:,192))
  call check_last_UV_W(l_switch,G0(:,:,:,192),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,221))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,970),G0(:,:,:,193))
  call check_last_UV_W(l_switch,G0(:,:,:,193),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,222))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,971),G0(:,:,:,194))
  call check_last_UV_W(l_switch,G0(:,:,:,194),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,223))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,972),G0(:,:,:,195))
  call check_last_UV_W(l_switch,G0(:,:,:,195),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,224))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,964),G0(:,:,:,196))
  call check_last_UV_W(l_switch,G0(:,:,:,196),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,225))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,965),G0(:,:,:,197))
  call check_last_UV_W(l_switch,G0(:,:,:,197),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,226))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,966),G0(:,:,:,198))
  call check_last_UV_W(l_switch,G0(:,:,:,198),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,227))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,964),wf(:,-4),G0(:,:,:,199))
  call check_last_UV_W(l_switch,G0(:,:,:,199),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,228))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,965),wf(:,-4),G0(:,:,:,200))
  call check_last_UV_W(l_switch,G0(:,:,:,200),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,229))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,966),wf(:,-4),G0(:,:,:,201))
  call check_last_UV_W(l_switch,G0(:,:,:,201),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,230))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,964),G0(:,:,:,202))
  call check_last_UV_W(l_switch,G0(:,:,:,202),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,231))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,965),G0(:,:,:,203))
  call check_last_UV_W(l_switch,G0(:,:,:,203),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,232))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,966),G0(:,:,:,204))
  call check_last_UV_W(l_switch,G0(:,:,:,204),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,233))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,161),G0(:,:,:,205))
  call check_last_UV_W(l_switch,G0(:,:,:,205),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,234))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,162),G0(:,:,:,206))
  call check_last_UV_W(l_switch,G0(:,:,:,206),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,235))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,163),G0(:,:,:,207))
  call check_last_UV_W(l_switch,G0(:,:,:,207),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,236))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,161),wf(:,-3),G0(:,:,:,208))
  call check_last_UV_W(l_switch,G0(:,:,:,208),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,237))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,162),wf(:,-3),G0(:,:,:,209))
  call check_last_UV_W(l_switch,G0(:,:,:,209),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,238))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,163),wf(:,-3),G0(:,:,:,210))
  call check_last_UV_W(l_switch,G0(:,:,:,210),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,239))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,161),G0(:,:,:,211))
  call check_last_UV_W(l_switch,G0(:,:,:,211),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,240))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,162),G0(:,:,:,212))
  call check_last_UV_W(l_switch,G0(:,:,:,212),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,241))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,163),G0(:,:,:,213))
  call check_last_UV_W(l_switch,G0(:,:,:,213),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,242))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,110),G0(:,:,:,214))
  call check_last_UV_W(l_switch,G0(:,:,:,214),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,243))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,110),wf(:,-1),G0(:,:,:,215))
  call check_last_UV_W(l_switch,G0(:,:,:,215),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,244))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,110),G0(:,:,:,216))
  call check_last_UV_W(l_switch,G0(:,:,:,216),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,245))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,111),G0(:,:,:,217))
  call check_last_UV_W(l_switch,G0(:,:,:,217),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,246))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,111),wf(:,-1),G0(:,:,:,218))
  call check_last_UV_W(l_switch,G0(:,:,:,218),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,247))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,111),G0(:,:,:,219))
  call check_last_UV_W(l_switch,G0(:,:,:,219),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,248))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,112),G0(:,:,:,220))
  call check_last_UV_W(l_switch,G0(:,:,:,220),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,249))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,112),wf(:,-1),G0(:,:,:,221))
  call check_last_UV_W(l_switch,G0(:,:,:,221),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,250))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,112),G0(:,:,:,222))
  call check_last_UV_W(l_switch,G0(:,:,:,222),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,251))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,943),G0(:,:,:,223))
  call check_last_UV_W(l_switch,G0(:,:,:,223),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,252))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,944),G0(:,:,:,224))
  call check_last_UV_W(l_switch,G0(:,:,:,224),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,253))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,945),G0(:,:,:,225))
  call check_last_UV_W(l_switch,G0(:,:,:,225),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,254))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,943),wf(:,-3),G0(:,:,:,226))
  call check_last_UV_W(l_switch,G0(:,:,:,226),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,255))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,944),wf(:,-3),G0(:,:,:,227))
  call check_last_UV_W(l_switch,G0(:,:,:,227),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,256))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,945),wf(:,-3),G0(:,:,:,228))
  call check_last_UV_W(l_switch,G0(:,:,:,228),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,257))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,943),G0(:,:,:,229))
  call check_last_UV_W(l_switch,G0(:,:,:,229),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,258))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,944),G0(:,:,:,230))
  call check_last_UV_W(l_switch,G0(:,:,:,230),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,259))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,945),G0(:,:,:,231))
  call check_last_UV_W(l_switch,G0(:,:,:,231),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,260))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,789),G0(:,:,:,232))
  call check_last_UV_W(l_switch,G0(:,:,:,232),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,261))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,789),wf(:,-1),G0(:,:,:,233))
  call check_last_UV_W(l_switch,G0(:,:,:,233),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,262))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,789),G0(:,:,:,234))
  call check_last_UV_W(l_switch,G0(:,:,:,234),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,263))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,790),G0(:,:,:,235))
  call check_last_UV_W(l_switch,G0(:,:,:,235),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,264))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,790),wf(:,-1),G0(:,:,:,236))
  call check_last_UV_W(l_switch,G0(:,:,:,236),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,265))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,790),G0(:,:,:,237))
  call check_last_UV_W(l_switch,G0(:,:,:,237),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,266))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,791),G0(:,:,:,238))
  call check_last_UV_W(l_switch,G0(:,:,:,238),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,267))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,791),wf(:,-1),G0(:,:,:,239))
  call check_last_UV_W(l_switch,G0(:,:,:,239),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,268))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,791),G0(:,:,:,240))
  call check_last_UV_W(l_switch,G0(:,:,:,240),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,269))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,176),G0(:,:,:,241))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,241),wf(:,-4),wf(:,-1),G0tensor(:,133))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,241),wf(:,-1),wf(:,-4),G0tensor(:,134))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,241),wf(:,-4),wf(:,-1),G0tensor(:,135))
  call check_last_UV_W(l_switch,G0(:,:,:,241),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,270))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,176),wf(:,-3),G0(:,:,:,242))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,242),wf(:,-4),wf(:,-1),G0tensor(:,136))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,242),wf(:,-1),wf(:,-4),G0tensor(:,137))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,242),wf(:,-4),wf(:,-1),G0tensor(:,138))
  call check_last_UV_W(l_switch,G0(:,:,:,242),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,271))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,176),G0(:,:,:,243))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,243),wf(:,-4),wf(:,-1),G0tensor(:,139))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,243),wf(:,-1),wf(:,-4),G0tensor(:,140))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,243),wf(:,-4),wf(:,-1),G0tensor(:,141))
  call check_last_UV_W(l_switch,G0(:,:,:,243),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,272))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,164),G0(:,:,:,244))
  call check_last_UV_W(l_switch,G0(:,:,:,244),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,273))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,165),G0(:,:,:,245))
  call check_last_UV_W(l_switch,G0(:,:,:,245),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,274))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,166),G0(:,:,:,246))
  call check_last_UV_W(l_switch,G0(:,:,:,246),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,275))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,164),wf(:,-3),G0(:,:,:,247))
  call check_last_UV_W(l_switch,G0(:,:,:,247),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,276))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,165),wf(:,-3),G0(:,:,:,248))
  call check_last_UV_W(l_switch,G0(:,:,:,248),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,277))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,166),wf(:,-3),G0(:,:,:,249))
  call check_last_UV_W(l_switch,G0(:,:,:,249),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,278))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,164),G0(:,:,:,250))
  call check_last_UV_W(l_switch,G0(:,:,:,250),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,279))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,165),G0(:,:,:,251))
  call check_last_UV_W(l_switch,G0(:,:,:,251),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,280))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,166),G0(:,:,:,252))
  call check_last_UV_W(l_switch,G0(:,:,:,252),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,281))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,177),G0(:,:,:,253))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,253),wf(:,-4),wf(:,-1),G0tensor(:,142))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,253),wf(:,-1),wf(:,-4),G0tensor(:,143))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,253),wf(:,-4),wf(:,-1),G0tensor(:,144))
  call check_last_UV_W(l_switch,G0(:,:,:,253),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,282))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,177),wf(:,-3),G0(:,:,:,254))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,254),wf(:,-4),wf(:,-1),G0tensor(:,145))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,254),wf(:,-1),wf(:,-4),G0tensor(:,146))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,254),wf(:,-4),wf(:,-1),G0tensor(:,147))
  call check_last_UV_W(l_switch,G0(:,:,:,254),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,283))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,177),G0(:,:,:,255))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,255),wf(:,-4),wf(:,-1),G0tensor(:,148))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,255),wf(:,-1),wf(:,-4),G0tensor(:,149))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,255),wf(:,-4),wf(:,-1),G0tensor(:,150))
  call check_last_UV_W(l_switch,G0(:,:,:,255),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,284))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,946),G0(:,:,:,256))
  call check_last_UV_W(l_switch,G0(:,:,:,256),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,285))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,947),G0(:,:,:,257))
  call check_last_UV_W(l_switch,G0(:,:,:,257),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,286))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(1)*(-M(41)+M(53)+M(60)+M(63)+M(65)-M(68)-M(69)-M(70)+M(72)+M(75)-M(81)-M(93))+c(2)*(-M(147)+M(158)+M(177)+M(182) &
    -M(185)-M(190)+M(201)-M(245))) * den(214)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,1)
  Gcoeff = (c(1)*(-M(41)+M(53)+M(60)+M(63)+M(65)-M(68)-M(69)-M(70)+M(72)+M(75)-M(81)-M(93))+c(2)*(M(168)+M(174)-M(187)-M(189) &
    +M(192)+M(198)-M(207)-M(231))) * den(214)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,2)
  Gcoeff = (c(2)*(M(147)-M(158)+M(168)+M(174)-M(177)-M(182)+M(185)-M(187)-M(189)+M(190)+M(192)+M(198)-M(201)-M(207)-M(231) &
    +M(245))) * den(214)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,3)
  Gcoeff = (c(2)*(-M(147)+M(158)-M(168)-M(174)+M(177)+M(182)-M(185)+M(187)+M(189)-M(190)-M(192)-M(198)+M(201)+M(207)+M(231) &
    -M(245))) * den(214)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,4)
  Gcoeff = (c(2)*(M(147)-M(158)+M(168)+M(174)-M(177)-M(182)+M(185)-M(187)-M(189)+M(190)+M(192)+M(198)-M(201)-M(207)-M(231) &
    +M(245))) * den(214)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,5)
  Gcoeff = (c(3)*(M(147)-M(158)+M(168)+M(174)-M(177)-M(182)+M(185)-M(187)-M(189)+M(190)+M(192)+M(198)-M(201)-M(207)-M(231) &
    +M(245))) * den(214)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,6)
  Gcoeff = (c(2)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(502)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,3)
  Gcoeff = (c(2)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(502)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,4)
  Gcoeff = (c(2)*(-M(144)+M(147)+M(174)-M(177)+M(181)-M(182)-M(189)+M(190)-M(191)+M(192)+M(199)-M(200)+M(204)-M(207)-M(234) &
    +M(237))) * den(502)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,5)
  Gcoeff = (c(2)*(M(136)-M(144)+M(147)-M(150)-M(177)-M(180)+M(181)-M(182)+M(183)+M(190)-M(194)+M(196)-M(200)+M(213)-M(223) &
    +M(237))) * den(502)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,6)
  Gcoeff = (c(2)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(502)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,7)
  Gcoeff = (c(2)*(M(144)-M(147)-M(174)+M(177)-M(181)+M(182)+M(189)-M(190)+M(191)-M(192)-M(199)+M(200)-M(204)+M(207)+M(234) &
    -M(237))) * den(502)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,8)
  Gcoeff = (c(3)*(M(136)-M(144)+M(147)-M(150)-M(177)-M(180)+M(181)-M(182)+M(183)+M(190)-M(194)+M(196)-M(200)+M(213)-M(223) &
    +M(237))) * den(502)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,9)
  Gcoeff = (c(3)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(502)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,10)
  Gcoeff = (c(3)*(M(144)-M(147)-M(174)+M(177)-M(181)+M(182)+M(189)-M(190)+M(191)-M(192)-M(199)+M(200)-M(204)+M(207)+M(234) &
    -M(237))) * den(502)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,11)
  Gcoeff = (c(1)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(2)*(-M(136)+M(150)+M(180)-M(183)+M(194) &
    -M(196)-M(213)+M(223))) * den(70)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,7)
  Gcoeff = (c(1)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(2)*(-M(174)+M(189)+M(191)-M(192)-M(199) &
    -M(204)+M(207)+M(234))) * den(70)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,16)
  Gcoeff = (c(2)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(70)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,25)
  Gcoeff = (c(1)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(2)*(-M(174)+M(189)+M(191)-M(192)-M(199) &
    -M(204)+M(207)+M(234))) * den(70)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,8)
  Gcoeff = (c(1)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(2)*(-M(136)+M(150)+M(180)-M(183)+M(194) &
    -M(196)-M(213)+M(223))) * den(70)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,17)
  Gcoeff = (c(2)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(70)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,26)
  Gcoeff = (c(2)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(70)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,9)
  Gcoeff = (c(2)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(70)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,18)
  Gcoeff = (c(3)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(70)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,27)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(498)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,30)
  Gcoeff = (c(2)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(498)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,31)
  Gcoeff = (c(2)*(-M(150)+M(153)+M(168)-M(171)+M(183)-M(184)-M(187)+M(188)+M(193)-M(194)-M(197)+M(198)-M(210)+M(213)+M(228) &
    -M(231))) * den(498)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,32)
  Gcoeff = (c(2)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(498)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,33)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(498)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,34)
  Gcoeff = (c(2)*(M(150)-M(153)-M(168)+M(171)-M(183)+M(184)+M(187)-M(188)-M(193)+M(194)+M(197)-M(198)+M(210)-M(213)-M(228) &
    +M(231))) * den(498)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,35)
  Gcoeff = (c(3)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(498)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,36)
  Gcoeff = (c(3)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(498)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,37)
  Gcoeff = (c(3)*(M(150)-M(153)-M(168)+M(171)-M(183)+M(184)+M(187)-M(188)-M(193)+M(194)+M(197)-M(198)+M(210)-M(213)-M(228) &
    +M(231))) * den(498)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,38)
  Gcoeff = (c(2)*(M(136)-M(160)-M(180)+M(186)+M(191)-M(193)-M(195)+M(196)+M(197)-M(199)-M(204)+M(210)+M(221)-M(223)-M(228) &
    +M(234))) * den(396)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,39)
  Gcoeff = (c(2)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(396)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,40)
  Gcoeff = (c(3)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(396)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,41)
  Gcoeff = (c(2)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(396)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,42)
  Gcoeff = (c(2)*(-M(134)+M(136)+M(158)-M(160)+M(179)-M(180)-M(185)+M(186)-M(195)+M(196)+M(201)-M(202)+M(221)-M(223)-M(245) &
    +M(247))) * den(396)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,43)
  Gcoeff = (c(3)*(-M(134)+M(136)+M(158)-M(160)+M(179)-M(180)-M(185)+M(186)-M(195)+M(196)+M(201)-M(202)+M(221)-M(223)-M(245) &
    +M(247))) * den(396)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,44)
  Gcoeff = (c(2)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(396)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,45)
  Gcoeff = (c(2)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(396)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,46)
  Gcoeff = (c(3)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(396)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,47)
  Gcoeff = (c(1)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(2)*(-M(161)+M(185)-M(203)+M(205)+M(235) &
    -M(241)+M(245)-M(246))) * den(73)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,34)
  Gcoeff = (c(1)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(2)*(-M(143)+M(145)-M(163)+M(187)+M(231) &
    -M(232)+M(236)-M(242))) * den(73)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,35)
  Gcoeff = (c(2)*(-M(143)+M(145)+M(161)-M(163)-M(185)+M(187)+M(203)-M(205)+M(231)-M(232)-M(235)+M(236)+M(241)-M(242)-M(245) &
    +M(246))) * den(73)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,36)
  Gcoeff = (c(1)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(2)*(-M(143)+M(145)-M(163)+M(187)+M(231) &
    -M(232)+M(236)-M(242))) * den(73)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,37)
  Gcoeff = (c(1)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(2)*(-M(161)+M(185)-M(203)+M(205)+M(235) &
    -M(241)+M(245)-M(246))) * den(73)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,38)
  Gcoeff = (c(2)*(M(143)-M(145)-M(161)+M(163)+M(185)-M(187)-M(203)+M(205)-M(231)+M(232)+M(235)-M(236)-M(241)+M(242)+M(245) &
    -M(246))) * den(73)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,39)
  Gcoeff = (c(2)*(-M(143)+M(145)+M(161)-M(163)-M(185)+M(187)+M(203)-M(205)+M(231)-M(232)-M(235)+M(236)+M(241)-M(242)-M(245) &
    +M(246))) * den(73)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,40)
  Gcoeff = (c(2)*(M(143)-M(145)-M(161)+M(163)+M(185)-M(187)-M(203)+M(205)-M(231)+M(232)+M(235)-M(236)-M(241)+M(242)+M(245) &
    -M(246))) * den(73)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,41)
  Gcoeff = (c(3)*(M(143)-M(145)-M(161)+M(163)+M(185)-M(187)-M(203)+M(205)-M(231)+M(232)+M(235)-M(236)-M(241)+M(242)+M(245) &
    -M(246))) * den(73)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,42)
  Gcoeff = (c(2)*(M(147)-M(148)-M(161)+M(163)+M(165)-M(166)+M(185)-M(187)-M(189)+M(190)-M(207)+M(208)-M(231)+M(232)+M(245) &
    -M(246))) * den(74)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,51)
  Gcoeff = (c(2)*(-M(147)+M(148)+M(153)-M(154)+M(161)-M(162)-M(164)+M(166)-M(185)+M(186)+M(188)-M(190)+M(221)-M(222)-M(245) &
    +M(246))) * den(74)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,52)
  Gcoeff = (c(2)*(-M(153)+M(154)+M(162)-M(163)+M(164)-M(165)-M(186)+M(187)-M(188)+M(189)+M(207)-M(208)-M(221)+M(222)+M(231) &
    -M(232))) * den(74)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,53)
  Gcoeff = (c(2)*(-M(147)+M(148)+M(161)-M(163)-M(165)+M(166)-M(185)+M(187)+M(189)-M(190)+M(207)-M(208)+M(231)-M(232)-M(245) &
    +M(246))) * den(74)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,54)
  Gcoeff = (c(2)*(M(147)-M(148)-M(153)+M(154)-M(161)+M(162)+M(164)-M(166)+M(185)-M(186)-M(188)+M(190)-M(221)+M(222)+M(245) &
    -M(246))) * den(74)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,55)
  Gcoeff = (c(2)*(M(153)-M(154)-M(162)+M(163)-M(164)+M(165)+M(186)-M(187)+M(188)-M(189)-M(207)+M(208)+M(221)-M(222)-M(231) &
    +M(232))) * den(74)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,56)
  Gcoeff = (c(3)*(-M(147)+M(148)+M(161)-M(163)-M(165)+M(166)-M(185)+M(187)+M(189)-M(190)+M(207)-M(208)+M(231)-M(232)-M(245) &
    +M(246))) * den(74)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,57)
  Gcoeff = (c(3)*(M(147)-M(148)-M(153)+M(154)-M(161)+M(162)+M(164)-M(166)+M(185)-M(186)-M(188)+M(190)-M(221)+M(222)+M(245) &
    -M(246))) * den(74)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,58)
  Gcoeff = (c(3)*(M(153)-M(154)-M(162)+M(163)-M(164)+M(165)+M(186)-M(187)+M(188)-M(189)-M(207)+M(208)+M(221)-M(222)-M(231) &
    +M(232))) * den(74)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,59)
  Gcoeff = (c(1)*(-M(41)+M(44)+M(56)+M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96))+c(2)*(M(163)+M(165)-M(187)-M(189) &
    -M(207)+M(208)-M(231)+M(232))) * den(257)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,43)
  Gcoeff = (c(1)*(-M(41)+M(44)+M(56)+M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96))+c(2)*(-M(147)+M(148)+M(161)+M(166) &
    -M(185)-M(190)-M(245)+M(246))) * den(257)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,44)
  Gcoeff = (c(2)*(-M(147)+M(148)+M(161)-M(163)-M(165)+M(166)-M(185)+M(187)+M(189)-M(190)+M(207)-M(208)+M(231)-M(232)-M(245) &
    +M(246))) * den(257)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,45)
  Gcoeff = (c(1)*(-M(41)+M(44)+M(56)+M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96))+c(2)*(-M(147)+M(148)+M(161)+M(166) &
    -M(185)-M(190)-M(245)+M(246))) * den(257)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,46)
  Gcoeff = (c(1)*(-M(41)+M(44)+M(56)+M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96))+c(2)*(M(163)+M(165)-M(187)-M(189) &
    -M(207)+M(208)-M(231)+M(232))) * den(257)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,47)
  Gcoeff = (c(2)*(M(147)-M(148)-M(161)+M(163)+M(165)-M(166)+M(185)-M(187)-M(189)+M(190)-M(207)+M(208)-M(231)+M(232)+M(245) &
    -M(246))) * den(257)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,48)
  Gcoeff = (c(2)*(-M(147)+M(148)+M(161)-M(163)-M(165)+M(166)-M(185)+M(187)+M(189)-M(190)+M(207)-M(208)+M(231)-M(232)-M(245) &
    +M(246))) * den(257)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,49)
  Gcoeff = (c(2)*(M(147)-M(148)-M(161)+M(163)+M(165)-M(166)+M(185)-M(187)-M(189)+M(190)-M(207)+M(208)-M(231)+M(232)+M(245) &
    -M(246))) * den(257)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,50)
  Gcoeff = (c(3)*(M(147)-M(148)-M(161)+M(163)+M(165)-M(166)+M(185)-M(187)-M(189)+M(190)-M(207)+M(208)-M(231)+M(232)+M(245) &
    -M(246))) * den(257)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,51)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(511)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,63)
  Gcoeff = (c(2)*(M(131)-M(137)-M(143)+M(145)-M(163)+M(187)+M(211)-M(217)-M(227)+M(229)+M(231)-M(232)+M(236)-M(242)-M(248) &
    +M(250))) * den(511)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,64)
  Gcoeff = (c(2)*(M(143)-M(145)-M(161)+M(163)+M(185)-M(187)-M(203)+M(205)-M(231)+M(232)+M(235)-M(236)-M(241)+M(242)+M(245) &
    -M(246))) * den(511)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,65)
  Gcoeff = (c(2)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(511)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,66)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(511)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,67)
  Gcoeff = (c(2)*(-M(143)+M(145)+M(161)-M(163)-M(185)+M(187)+M(203)-M(205)+M(231)-M(232)-M(235)+M(236)+M(241)-M(242)-M(245) &
    +M(246))) * den(511)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,68)
  Gcoeff = (c(3)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(511)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,69)
  Gcoeff = (c(3)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(511)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,70)
  Gcoeff = (c(3)*(-M(143)+M(145)+M(161)-M(163)-M(185)+M(187)+M(203)-M(205)+M(231)-M(232)-M(235)+M(236)+M(241)-M(242)-M(245) &
    +M(246))) * den(511)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,71)
  Gcoeff = (c(1)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(2)*(-M(131)+M(137)-M(211)+M(217)+M(227) &
    -M(229)+M(248)-M(250))) * den(73)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,52)
  Gcoeff = (c(1)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(2)*(-M(143)+M(145)-M(163)+M(187)+M(231) &
    -M(232)+M(236)-M(242))) * den(73)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,61)
  Gcoeff = (c(2)*(M(131)-M(137)-M(143)+M(145)-M(163)+M(187)+M(211)-M(217)-M(227)+M(229)+M(231)-M(232)+M(236)-M(242)-M(248) &
    +M(250))) * den(73)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,70)
  Gcoeff = (c(1)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(2)*(-M(143)+M(145)-M(163)+M(187)+M(231) &
    -M(232)+M(236)-M(242))) * den(73)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,53)
  Gcoeff = (c(1)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(2)*(-M(131)+M(137)-M(211)+M(217)+M(227) &
    -M(229)+M(248)-M(250))) * den(73)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,62)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(73)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,71)
  Gcoeff = (c(2)*(M(131)-M(137)-M(143)+M(145)-M(163)+M(187)+M(211)-M(217)-M(227)+M(229)+M(231)-M(232)+M(236)-M(242)-M(248) &
    +M(250))) * den(73)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,54)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(73)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,63)
  Gcoeff = (c(3)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(73)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,72)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(507)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,90)
  Gcoeff = (c(2)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(507)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,91)
  Gcoeff = (c(2)*(M(149)-M(151)-M(162)+M(165)+M(186)-M(189)-M(207)+M(208)+M(211)-M(212)-M(217)+M(218)+M(221)-M(222)-M(227) &
    +M(229))) * den(507)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,92)
  Gcoeff = (c(2)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(507)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,93)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(149)-M(151)+M(165)-M(189)+M(203)-M(205)-M(207)+M(208)-M(212)+M(218)+M(224)-M(226)-M(235) &
    +M(241))) * den(507)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,94)
  Gcoeff = (c(2)*(-M(149)+M(151)+M(162)-M(165)-M(186)+M(189)+M(207)-M(208)-M(211)+M(212)+M(217)-M(218)-M(221)+M(222)+M(227) &
    -M(229))) * den(507)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,95)
  Gcoeff = (c(3)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(507)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,96)
  Gcoeff = (c(3)*(-M(132)+M(138)+M(149)-M(151)+M(165)-M(189)+M(203)-M(205)-M(207)+M(208)-M(212)+M(218)+M(224)-M(226)-M(235) &
    +M(241))) * den(507)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,97)
  Gcoeff = (c(3)*(-M(149)+M(151)+M(162)-M(165)-M(186)+M(189)+M(207)-M(208)-M(211)+M(212)+M(217)-M(218)-M(221)+M(222)+M(227) &
    -M(229))) * den(507)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,98)
  Gcoeff = (c(2)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(302)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,99)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(143)-M(145)+M(149)-M(151)-M(153)+M(154)+M(164)-M(188)-M(212)+M(218)-M(236)+M(242)+M(248) &
    -M(250))) * den(302)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,100)
  Gcoeff = (c(3)*(-M(131)+M(137)+M(143)-M(145)+M(149)-M(151)-M(153)+M(154)+M(164)-M(188)-M(212)+M(218)-M(236)+M(242)+M(248) &
    -M(250))) * den(302)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,101)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(302)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,102)
  Gcoeff = (c(2)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(302)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,103)
  Gcoeff = (c(3)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(302)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,104)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(302)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,105)
  Gcoeff = (c(2)*(M(132)-M(138)-M(143)+M(145)+M(147)-M(148)-M(149)+M(151)-M(166)+M(190)+M(212)-M(218)-M(224)+M(226)+M(236) &
    -M(242))) * den(302)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,106)
  Gcoeff = (c(3)*(M(132)-M(138)-M(143)+M(145)+M(147)-M(148)-M(149)+M(151)-M(166)+M(190)+M(212)-M(218)-M(224)+M(226)+M(236) &
    -M(242))) * den(302)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,107)
  Gcoeff = (c(1)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(2)*(-M(173)-M(180)+M(183)+M(213) &
    +M(215)-M(216)-M(223)+M(233))) * den(75)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,79)
  Gcoeff = (c(1)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(2)*(-M(138)+M(141)-M(175)+M(205) &
    -M(206)+M(214)-M(224)+M(235))) * den(75)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,80)
  Gcoeff = (c(2)*(-M(138)+M(141)+M(173)-M(175)+M(180)-M(183)+M(205)-M(206)-M(213)+M(214)-M(215)+M(216)+M(223)-M(224)-M(233) &
    +M(235))) * den(75)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,81)
  Gcoeff = (c(1)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(2)*(-M(138)+M(141)-M(175)+M(205) &
    -M(206)+M(214)-M(224)+M(235))) * den(75)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,82)
  Gcoeff = (c(1)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(2)*(-M(173)-M(180)+M(183)+M(213) &
    +M(215)-M(216)-M(223)+M(233))) * den(75)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,83)
  Gcoeff = (c(2)*(M(138)-M(141)-M(173)+M(175)-M(180)+M(183)-M(205)+M(206)+M(213)-M(214)+M(215)-M(216)-M(223)+M(224)+M(233) &
    -M(235))) * den(75)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,84)
  Gcoeff = (c(2)*(-M(138)+M(141)+M(173)-M(175)+M(180)-M(183)+M(205)-M(206)-M(213)+M(214)-M(215)+M(216)+M(223)-M(224)-M(233) &
    +M(235))) * den(75)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,85)
  Gcoeff = (c(2)*(M(138)-M(141)-M(173)+M(175)-M(180)+M(183)-M(205)+M(206)+M(213)-M(214)+M(215)-M(216)-M(223)+M(224)+M(233) &
    -M(235))) * den(75)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,86)
  Gcoeff = (c(3)*(M(138)-M(141)-M(173)+M(175)-M(180)+M(183)-M(205)+M(206)+M(213)-M(214)+M(215)-M(216)-M(223)+M(224)+M(233) &
    -M(235))) * den(75)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,87)
  Gcoeff = (c(2)*(M(141)-M(156)+M(162)+M(173)-M(175)-M(183)-M(206)+M(209)-M(211)-M(213)+M(214)+M(216)+M(222)-M(225)-M(229) &
    +M(239))) * den(76)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,111)
  Gcoeff = (c(2)*(-M(141)+M(151)+M(156)-M(159)-M(165)+M(175)+M(197)+M(206)-M(208)-M(209)+M(210)+M(212)-M(214)-M(219)+M(225) &
    -M(239))) * den(76)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,112)
  Gcoeff = (c(2)*(-M(151)+M(159)-M(162)+M(165)-M(173)+M(183)-M(197)+M(208)-M(210)+M(211)-M(212)+M(213)-M(216)+M(219)-M(222) &
    +M(229))) * den(76)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,113)
  Gcoeff = (c(2)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(76)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,114)
  Gcoeff = (c(2)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(76)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,115)
  Gcoeff = (c(2)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(76)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,116)
  Gcoeff = (c(3)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(76)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,117)
  Gcoeff = (c(3)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(76)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,118)
  Gcoeff = (c(3)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(76)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,119)
  Gcoeff = (c(1)*(-M(42)+M(54)+M(57)+M(64)-M(69)+M(77)-M(80)-M(81)-M(82)+M(84)+M(87)-M(94))+c(2)*(M(162)+M(173)-M(183)-M(211) &
    -M(213)+M(216)+M(222)-M(229))) * den(232)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,88)
  Gcoeff = (c(1)*(-M(42)+M(54)+M(57)+M(64)-M(69)+M(77)-M(80)-M(81)-M(82)+M(84)+M(87)-M(94))+c(2)*(-M(141)+M(156)+M(175)+M(206) &
    -M(209)-M(214)+M(225)-M(239))) * den(232)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,89)
  Gcoeff = (c(2)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(232)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,90)
  Gcoeff = (c(1)*(-M(42)+M(54)+M(57)+M(64)-M(69)+M(77)-M(80)-M(81)-M(82)+M(84)+M(87)-M(94))+c(2)*(-M(141)+M(156)+M(175)+M(206) &
    -M(209)-M(214)+M(225)-M(239))) * den(232)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,91)
  Gcoeff = (c(1)*(-M(42)+M(54)+M(57)+M(64)-M(69)+M(77)-M(80)-M(81)-M(82)+M(84)+M(87)-M(94))+c(2)*(M(162)+M(173)-M(183)-M(211) &
    -M(213)+M(216)+M(222)-M(229))) * den(232)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,92)
  Gcoeff = (c(2)*(M(141)-M(156)+M(162)+M(173)-M(175)-M(183)-M(206)+M(209)-M(211)-M(213)+M(214)+M(216)+M(222)-M(225)-M(229) &
    +M(239))) * den(232)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,93)
  Gcoeff = (c(2)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(232)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,94)
  Gcoeff = (c(2)*(M(141)-M(156)+M(162)+M(173)-M(175)-M(183)-M(206)+M(209)-M(211)-M(213)+M(214)+M(216)+M(222)-M(225)-M(229) &
    +M(239))) * den(232)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,95)
  Gcoeff = (c(3)*(M(141)-M(156)+M(162)+M(173)-M(175)-M(183)-M(206)+M(209)-M(211)-M(213)+M(214)+M(216)+M(222)-M(225)-M(229) &
    +M(239))) * den(232)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,96)
  Gcoeff = (c(2)*(M(138)-M(141)-M(173)+M(175)-M(180)+M(183)-M(205)+M(206)+M(213)-M(214)+M(215)-M(216)-M(223)+M(224)+M(233) &
    -M(235))) * den(520)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,123)
  Gcoeff = (c(2)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(520)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,124)
  Gcoeff = (c(2)*(M(135)-M(138)+M(141)-M(149)-M(175)+M(189)-M(199)-M(204)+M(205)-M(206)+M(207)+M(214)-M(218)+M(220)-M(224) &
    +M(235))) * den(520)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,125)
  Gcoeff = (c(2)*(-M(138)+M(141)+M(173)-M(175)+M(180)-M(183)+M(205)-M(206)-M(213)+M(214)-M(215)+M(216)+M(223)-M(224)-M(233) &
    +M(235))) * den(520)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,126)
  Gcoeff = (c(2)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(520)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,127)
  Gcoeff = (c(2)*(-M(135)+M(138)-M(141)+M(149)+M(175)-M(189)+M(199)+M(204)-M(205)+M(206)-M(207)-M(214)+M(218)-M(220)+M(224) &
    -M(235))) * den(520)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,128)
  Gcoeff = (c(3)*(-M(138)+M(141)+M(173)-M(175)+M(180)-M(183)+M(205)-M(206)-M(213)+M(214)-M(215)+M(216)+M(223)-M(224)-M(233) &
    +M(235))) * den(520)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,129)
  Gcoeff = (c(3)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(520)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,130)
  Gcoeff = (c(3)*(-M(135)+M(138)-M(141)+M(149)+M(175)-M(189)+M(199)+M(204)-M(205)+M(206)-M(207)-M(214)+M(218)-M(220)+M(224) &
    -M(235))) * den(520)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,131)
  Gcoeff = (c(1)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(2)*(-M(167)-M(179)+M(181)+M(209) &
    +M(237)+M(239)-M(240)-M(247))) * den(77)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,97)
  Gcoeff = (c(1)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(2)*(-M(137)+M(139)-M(169)+M(211) &
    +M(229)-M(230)+M(238)-M(248))) * den(77)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,98)
  Gcoeff = (c(2)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248))) * den(77)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,99)
  Gcoeff = (c(1)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(2)*(-M(137)+M(139)-M(169)+M(211) &
    +M(229)-M(230)+M(238)-M(248))) * den(77)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,100)
  Gcoeff = (c(1)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(2)*(-M(167)-M(179)+M(181)+M(209) &
    +M(237)+M(239)-M(240)-M(247))) * den(77)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,101)
  Gcoeff = (c(2)*(M(137)-M(139)-M(167)+M(169)-M(179)+M(181)+M(209)-M(211)-M(229)+M(230)+M(237)-M(238)+M(239)-M(240)-M(247) &
    +M(248))) * den(77)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,102)
  Gcoeff = (c(2)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248))) * den(77)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,103)
  Gcoeff = (c(2)*(M(137)-M(139)-M(167)+M(169)-M(179)+M(181)+M(209)-M(211)-M(229)+M(230)+M(237)-M(238)+M(239)-M(240)-M(247) &
    +M(248))) * den(77)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,104)
  Gcoeff = (c(3)*(M(137)-M(139)-M(167)+M(169)-M(179)+M(181)+M(209)-M(211)-M(229)+M(230)+M(237)-M(238)+M(239)-M(240)-M(247) &
    +M(248))) * den(77)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,105)
  Gcoeff = (c(2)*(M(141)-M(142)-M(167)+M(169)+M(171)-M(172)-M(183)+M(184)+M(209)-M(211)-M(213)+M(214)-M(229)+M(230)+M(239) &
    -M(240))) * den(78)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,135)
  Gcoeff = (c(2)*(-M(141)+M(142)+M(151)-M(152)+M(167)-M(168)-M(170)+M(172)+M(197)-M(198)-M(209)+M(210)+M(212)-M(214)-M(239) &
    +M(240))) * den(78)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,136)
  Gcoeff = (c(2)*(-M(151)+M(152)+M(168)-M(169)+M(170)-M(171)+M(183)-M(184)-M(197)+M(198)-M(210)+M(211)-M(212)+M(213)+M(229) &
    -M(230))) * den(78)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,137)
  Gcoeff = (c(2)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(78)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,138)
  Gcoeff = (c(2)*(M(141)-M(142)-M(151)+M(152)-M(167)+M(168)+M(170)-M(172)-M(197)+M(198)+M(209)-M(210)-M(212)+M(214)+M(239) &
    -M(240))) * den(78)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,139)
  Gcoeff = (c(2)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(78)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,140)
  Gcoeff = (c(3)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(78)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,141)
  Gcoeff = (c(3)*(M(141)-M(142)-M(151)+M(152)-M(167)+M(168)+M(170)-M(172)-M(197)+M(198)+M(209)-M(210)-M(212)+M(214)+M(239) &
    -M(240))) * den(78)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,142)
  Gcoeff = (c(3)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(78)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,143)
  Gcoeff = (c(1)*(-M(42)+M(47)+M(59)+M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99))+c(2)*(M(169)+M(171)-M(183)+M(184) &
    -M(211)-M(213)-M(229)+M(230))) * den(239)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,106)
  Gcoeff = (c(1)*(-M(42)+M(47)+M(59)+M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99))+c(2)*(-M(141)+M(142)+M(167)+M(172) &
    -M(209)-M(214)-M(239)+M(240))) * den(239)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,107)
  Gcoeff = (c(2)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(239)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,108)
  Gcoeff = (c(1)*(-M(42)+M(47)+M(59)+M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99))+c(2)*(-M(141)+M(142)+M(167)+M(172) &
    -M(209)-M(214)-M(239)+M(240))) * den(239)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,109)
  Gcoeff = (c(1)*(-M(42)+M(47)+M(59)+M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99))+c(2)*(M(169)+M(171)-M(183)+M(184) &
    -M(211)-M(213)-M(229)+M(230))) * den(239)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,110)
  Gcoeff = (c(2)*(M(141)-M(142)-M(167)+M(169)+M(171)-M(172)-M(183)+M(184)+M(209)-M(211)-M(213)+M(214)-M(229)+M(230)+M(239) &
    -M(240))) * den(239)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,111)
  Gcoeff = (c(2)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(239)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,112)
  Gcoeff = (c(2)*(M(141)-M(142)-M(167)+M(169)+M(171)-M(172)-M(183)+M(184)+M(209)-M(211)-M(213)+M(214)-M(229)+M(230)+M(239) &
    -M(240))) * den(239)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,113)
  Gcoeff = (c(3)*(M(141)-M(142)-M(167)+M(169)+M(171)-M(172)-M(183)+M(184)+M(209)-M(211)-M(213)+M(214)-M(229)+M(230)+M(239) &
    -M(240))) * den(239)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,114)
  Gcoeff = (c(2)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248))) * den(525)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,147)
  Gcoeff = (c(2)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(525)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,148)
  Gcoeff = (c(2)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(525)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,149)
  Gcoeff = (c(2)*(M(137)-M(139)-M(167)+M(169)-M(179)+M(181)+M(209)-M(211)-M(229)+M(230)+M(237)-M(238)+M(239)-M(240)-M(247) &
    +M(248))) * den(525)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,150)
  Gcoeff = (c(2)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(525)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,151)
  Gcoeff = (c(2)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(525)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,152)
  Gcoeff = (c(3)*(M(137)-M(139)-M(167)+M(169)-M(179)+M(181)+M(209)-M(211)-M(229)+M(230)+M(237)-M(238)+M(239)-M(240)-M(247) &
    +M(248))) * den(525)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,153)
  Gcoeff = (c(3)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(525)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,154)
  Gcoeff = (c(3)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(525)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,155)
  Gcoeff = (c(2)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(79)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,156)
  Gcoeff = (c(2)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(79)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,157)
  Gcoeff = (c(2)*(-M(145)+M(157)-M(161)+M(163)-M(167)+M(181)-M(191)+M(205)+M(232)-M(234)+M(235)-M(236)+M(237)-M(240)+M(243) &
    -M(246))) * den(79)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,158)
  Gcoeff = (c(2)*(-M(139)+M(155)-M(161)-M(167)+M(169)+M(181)+M(205)-M(215)+M(230)-M(233)+M(235)+M(237)-M(238)-M(240)-M(246) &
    +M(249))) * den(79)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,159)
  Gcoeff = (c(2)*(M(139)-M(145)-M(155)+M(157)+M(163)-M(169)-M(191)+M(215)-M(230)+M(232)+M(233)-M(234)-M(236)+M(238)+M(243) &
    -M(249))) * den(79)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,160)
  Gcoeff = (c(2)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(79)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,161)
  Gcoeff = (c(3)*(-M(139)+M(155)-M(161)-M(167)+M(169)+M(181)+M(205)-M(215)+M(230)-M(233)+M(235)+M(237)-M(238)-M(240)-M(246) &
    +M(249))) * den(79)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,162)
  Gcoeff = (c(3)*(M(139)-M(145)-M(155)+M(157)+M(163)-M(169)-M(191)+M(215)-M(230)+M(232)+M(233)-M(234)-M(236)+M(238)+M(243) &
    -M(249))) * den(79)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,163)
  Gcoeff = (c(3)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(79)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,164)
  Gcoeff = (c(2)*(M(139)-M(140)-M(173)+M(175)+M(177)-M(178)-M(181)+M(182)-M(205)+M(206)+M(215)-M(216)+M(233)-M(235)-M(237) &
    +M(238))) * den(80)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,165)
  Gcoeff = (c(2)*(-M(139)+M(140)+M(145)-M(146)+M(173)-M(174)-M(176)+M(178)+M(191)-M(192)-M(215)+M(216)-M(233)+M(234)+M(236) &
    -M(238))) * den(80)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,166)
  Gcoeff = (c(2)*(-M(145)+M(146)+M(174)-M(175)+M(176)-M(177)+M(181)-M(182)-M(191)+M(192)+M(205)-M(206)-M(234)+M(235)-M(236) &
    +M(237))) * den(80)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,167)
  Gcoeff = (c(2)*(-M(139)+M(140)+M(173)-M(175)-M(177)+M(178)+M(181)-M(182)+M(205)-M(206)-M(215)+M(216)-M(233)+M(235)+M(237) &
    -M(238))) * den(80)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,168)
  Gcoeff = (c(2)*(M(139)-M(140)-M(145)+M(146)-M(173)+M(174)+M(176)-M(178)-M(191)+M(192)+M(215)-M(216)+M(233)-M(234)-M(236) &
    +M(238))) * den(80)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,169)
  Gcoeff = (c(2)*(M(145)-M(146)-M(174)+M(175)-M(176)+M(177)-M(181)+M(182)+M(191)-M(192)-M(205)+M(206)+M(234)-M(235)+M(236) &
    -M(237))) * den(80)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,170)
  Gcoeff = (c(3)*(-M(139)+M(140)+M(173)-M(175)-M(177)+M(178)+M(181)-M(182)+M(205)-M(206)-M(215)+M(216)-M(233)+M(235)+M(237) &
    -M(238))) * den(80)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,171)
  Gcoeff = (c(3)*(M(139)-M(140)-M(145)+M(146)-M(173)+M(174)+M(176)-M(178)-M(191)+M(192)+M(215)-M(216)+M(233)-M(234)-M(236) &
    +M(238))) * den(80)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,172)
  Gcoeff = (c(3)*(M(145)-M(146)-M(174)+M(175)-M(176)+M(177)-M(181)+M(182)+M(191)-M(192)-M(205)+M(206)+M(234)-M(235)+M(236) &
    -M(237))) * den(80)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,173)
  Gcoeff = (c(2)*(M(139)-M(140)-M(173)+M(175)+M(177)-M(178)-M(181)+M(182)-M(205)+M(206)+M(215)-M(216)+M(233)-M(235)-M(237) &
    +M(238))) * den(535)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,174)
  Gcoeff = (c(2)*(-M(139)+M(140)+M(145)-M(146)+M(173)-M(174)-M(176)+M(178)+M(191)-M(192)-M(215)+M(216)-M(233)+M(234)+M(236) &
    -M(238))) * den(535)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,175)
  Gcoeff = (c(2)*(-M(145)+M(146)+M(174)-M(175)+M(176)-M(177)+M(181)-M(182)-M(191)+M(192)+M(205)-M(206)-M(234)+M(235)-M(236) &
    +M(237))) * den(535)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,176)
  Gcoeff = (c(2)*(-M(139)+M(140)+M(173)-M(175)-M(177)+M(178)+M(181)-M(182)+M(205)-M(206)-M(215)+M(216)-M(233)+M(235)+M(237) &
    -M(238))) * den(535)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,177)
  Gcoeff = (c(2)*(M(139)-M(140)-M(145)+M(146)-M(173)+M(174)+M(176)-M(178)-M(191)+M(192)+M(215)-M(216)+M(233)-M(234)-M(236) &
    +M(238))) * den(535)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,178)
  Gcoeff = (c(2)*(M(145)-M(146)-M(174)+M(175)-M(176)+M(177)-M(181)+M(182)+M(191)-M(192)-M(205)+M(206)+M(234)-M(235)+M(236) &
    -M(237))) * den(535)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,179)
  Gcoeff = (c(3)*(-M(139)+M(140)+M(173)-M(175)-M(177)+M(178)+M(181)-M(182)+M(205)-M(206)-M(215)+M(216)-M(233)+M(235)+M(237) &
    -M(238))) * den(535)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,180)
  Gcoeff = (c(3)*(M(139)-M(140)-M(145)+M(146)-M(173)+M(174)+M(176)-M(178)-M(191)+M(192)+M(215)-M(216)+M(233)-M(234)-M(236) &
    +M(238))) * den(535)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,181)
  Gcoeff = (c(3)*(M(145)-M(146)-M(174)+M(175)-M(176)+M(177)-M(181)+M(182)+M(191)-M(192)-M(205)+M(206)+M(234)-M(235)+M(236) &
    -M(237))) * den(535)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,182)
  Gcoeff = (c(2)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(532)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,183)
  Gcoeff = (c(2)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(532)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,184)
  Gcoeff = (c(2)*(-M(145)+M(157)-M(161)+M(163)-M(167)+M(181)-M(191)+M(205)+M(232)-M(234)+M(235)-M(236)+M(237)-M(240)+M(243) &
    -M(246))) * den(532)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,185)
  Gcoeff = (c(2)*(-M(139)+M(155)-M(161)-M(167)+M(169)+M(181)+M(205)-M(215)+M(230)-M(233)+M(235)+M(237)-M(238)-M(240)-M(246) &
    +M(249))) * den(532)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,186)
  Gcoeff = (c(2)*(M(139)-M(145)-M(155)+M(157)+M(163)-M(169)-M(191)+M(215)-M(230)+M(232)+M(233)-M(234)-M(236)+M(238)+M(243) &
    -M(249))) * den(532)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,187)
  Gcoeff = (c(2)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(532)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,188)
  Gcoeff = (c(3)*(-M(139)+M(155)-M(161)-M(167)+M(169)+M(181)+M(205)-M(215)+M(230)-M(233)+M(235)+M(237)-M(238)-M(240)-M(246) &
    +M(249))) * den(532)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,189)
  Gcoeff = (c(3)*(M(139)-M(145)-M(155)+M(157)+M(163)-M(169)-M(191)+M(215)-M(230)+M(232)+M(233)-M(234)-M(236)+M(238)+M(243) &
    -M(249))) * den(532)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,190)
  Gcoeff = (c(3)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(532)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,191)
  Gcoeff = (c(1)*(-M(43)+M(55)+M(58)+M(61)-M(70)-M(82)+M(89)-M(92)-M(93)-M(94)+M(96)+M(99))+c(2)*(M(161)+M(167)-M(181)-M(205) &
    -M(235)-M(237)+M(240)+M(246))) * den(241)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,115)
  Gcoeff = (c(1)*(-M(43)+M(55)+M(58)+M(61)-M(70)-M(82)+M(89)-M(92)-M(93)-M(94)+M(96)+M(99))+c(2)*(-M(139)+M(155)+M(169)-M(215) &
    +M(230)-M(233)-M(238)+M(249))) * den(241)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,116)
  Gcoeff = (c(2)*(-M(139)+M(155)-M(161)-M(167)+M(169)+M(181)+M(205)-M(215)+M(230)-M(233)+M(235)+M(237)-M(238)-M(240)-M(246) &
    +M(249))) * den(241)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,117)
  Gcoeff = (c(1)*(-M(43)+M(55)+M(58)+M(61)-M(70)-M(82)+M(89)-M(92)-M(93)-M(94)+M(96)+M(99))+c(2)*(-M(139)+M(155)+M(169)-M(215) &
    +M(230)-M(233)-M(238)+M(249))) * den(241)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,118)
  Gcoeff = (c(1)*(-M(43)+M(55)+M(58)+M(61)-M(70)-M(82)+M(89)-M(92)-M(93)-M(94)+M(96)+M(99))+c(2)*(M(161)+M(167)-M(181)-M(205) &
    -M(235)-M(237)+M(240)+M(246))) * den(241)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,119)
  Gcoeff = (c(2)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(241)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,120)
  Gcoeff = (c(2)*(-M(139)+M(155)-M(161)-M(167)+M(169)+M(181)+M(205)-M(215)+M(230)-M(233)+M(235)+M(237)-M(238)-M(240)-M(246) &
    +M(249))) * den(241)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,121)
  Gcoeff = (c(2)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(241)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,122)
  Gcoeff = (c(3)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(241)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,123)
  Gcoeff = (c(2)*(M(137)-M(139)-M(167)+M(169)-M(179)+M(181)+M(209)-M(211)-M(229)+M(230)+M(237)-M(238)+M(239)-M(240)-M(247) &
    +M(248))) * den(527)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,195)
  Gcoeff = (c(2)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(527)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,196)
  Gcoeff = (c(2)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(527)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,197)
  Gcoeff = (c(2)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248))) * den(527)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,198)
  Gcoeff = (c(2)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(527)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,199)
  Gcoeff = (c(2)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(527)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,200)
  Gcoeff = (c(3)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248))) * den(527)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,201)
  Gcoeff = (c(3)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(527)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,202)
  Gcoeff = (c(3)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(527)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,203)
  Gcoeff = (c(1)*(-M(43)+M(50)+M(62)+M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94))+c(2)*(M(175)+M(177)-M(181)+M(182) &
    -M(205)+M(206)-M(235)-M(237))) * den(245)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,124)
  Gcoeff = (c(1)*(-M(43)+M(50)+M(62)+M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94))+c(2)*(-M(139)+M(140)+M(173)+M(178) &
    -M(215)+M(216)-M(233)-M(238))) * den(245)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,125)
  Gcoeff = (c(2)*(-M(139)+M(140)+M(173)-M(175)-M(177)+M(178)+M(181)-M(182)+M(205)-M(206)-M(215)+M(216)-M(233)+M(235)+M(237) &
    -M(238))) * den(245)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,126)
  Gcoeff = (c(1)*(-M(43)+M(50)+M(62)+M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94))+c(2)*(-M(139)+M(140)+M(173)+M(178) &
    -M(215)+M(216)-M(233)-M(238))) * den(245)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,127)
  Gcoeff = (c(1)*(-M(43)+M(50)+M(62)+M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94))+c(2)*(M(175)+M(177)-M(181)+M(182) &
    -M(205)+M(206)-M(235)-M(237))) * den(245)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,128)
  Gcoeff = (c(2)*(M(139)-M(140)-M(173)+M(175)+M(177)-M(178)-M(181)+M(182)-M(205)+M(206)+M(215)-M(216)+M(233)-M(235)-M(237) &
    +M(238))) * den(245)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,129)
  Gcoeff = (c(2)*(-M(139)+M(140)+M(173)-M(175)-M(177)+M(178)+M(181)-M(182)+M(205)-M(206)-M(215)+M(216)-M(233)+M(235)+M(237) &
    -M(238))) * den(245)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,130)
  Gcoeff = (c(2)*(M(139)-M(140)-M(173)+M(175)+M(177)-M(178)-M(181)+M(182)-M(205)+M(206)+M(215)-M(216)+M(233)-M(235)-M(237) &
    +M(238))) * den(245)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,131)
  Gcoeff = (c(3)*(M(139)-M(140)-M(173)+M(175)+M(177)-M(178)-M(181)+M(182)-M(205)+M(206)+M(215)-M(216)+M(233)-M(235)-M(237) &
    +M(238))) * den(245)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,132)
  Gcoeff = (c(2)*(-M(138)+M(141)+M(173)-M(175)+M(180)-M(183)+M(205)-M(206)-M(213)+M(214)-M(215)+M(216)+M(223)-M(224)-M(233) &
    +M(235))) * den(518)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,207)
  Gcoeff = (c(2)*(-M(135)+M(138)-M(141)+M(149)+M(175)-M(189)+M(199)+M(204)-M(205)+M(206)-M(207)-M(214)+M(218)-M(220)+M(224) &
    -M(235))) * den(518)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,208)
  Gcoeff = (c(2)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(518)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,209)
  Gcoeff = (c(2)*(M(138)-M(141)-M(173)+M(175)-M(180)+M(183)-M(205)+M(206)+M(213)-M(214)+M(215)-M(216)-M(223)+M(224)+M(233) &
    -M(235))) * den(518)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,210)
  Gcoeff = (c(2)*(M(135)-M(138)+M(141)-M(149)-M(175)+M(189)-M(199)-M(204)+M(205)-M(206)+M(207)+M(214)-M(218)+M(220)-M(224) &
    +M(235))) * den(518)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,211)
  Gcoeff = (c(2)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(518)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,212)
  Gcoeff = (c(3)*(M(138)-M(141)-M(173)+M(175)-M(180)+M(183)-M(205)+M(206)+M(213)-M(214)+M(215)-M(216)-M(223)+M(224)+M(233) &
    -M(235))) * den(518)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,213)
  Gcoeff = (c(3)*(M(135)-M(138)+M(141)-M(149)-M(175)+M(189)-M(199)-M(204)+M(205)-M(206)+M(207)+M(214)-M(218)+M(220)-M(224) &
    +M(235))) * den(518)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,214)
  Gcoeff = (c(3)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(518)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,215)
  Gcoeff = (c(2)*(M(141)-M(142)-M(167)+M(169)+M(171)-M(172)-M(183)+M(184)+M(209)-M(211)-M(213)+M(214)-M(229)+M(230)+M(239) &
    -M(240))) * den(529)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,216)
  Gcoeff = (c(2)*(-M(141)+M(142)+M(151)-M(152)+M(167)-M(168)-M(170)+M(172)+M(197)-M(198)-M(209)+M(210)+M(212)-M(214)-M(239) &
    +M(240))) * den(529)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,217)
  Gcoeff = (c(2)*(-M(151)+M(152)+M(168)-M(169)+M(170)-M(171)+M(183)-M(184)-M(197)+M(198)-M(210)+M(211)-M(212)+M(213)+M(229) &
    -M(230))) * den(529)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,218)
  Gcoeff = (c(2)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(529)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,219)
  Gcoeff = (c(2)*(M(141)-M(142)-M(151)+M(152)-M(167)+M(168)+M(170)-M(172)-M(197)+M(198)+M(209)-M(210)-M(212)+M(214)+M(239) &
    -M(240))) * den(529)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,220)
  Gcoeff = (c(2)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(529)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,221)
  Gcoeff = (c(3)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(529)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,222)
  Gcoeff = (c(3)*(M(141)-M(142)-M(151)+M(152)-M(167)+M(168)+M(170)-M(172)-M(197)+M(198)+M(209)-M(210)-M(212)+M(214)+M(239) &
    -M(240))) * den(529)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,223)
  Gcoeff = (c(3)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(529)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,224)
  Gcoeff = (c(2)*(M(141)-M(156)+M(162)+M(173)-M(175)-M(183)-M(206)+M(209)-M(211)-M(213)+M(214)+M(216)+M(222)-M(225)-M(229) &
    +M(239))) * den(522)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,225)
  Gcoeff = (c(2)*(-M(141)+M(151)+M(156)-M(159)-M(165)+M(175)+M(197)+M(206)-M(208)-M(209)+M(210)+M(212)-M(214)-M(219)+M(225) &
    -M(239))) * den(522)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,226)
  Gcoeff = (c(2)*(-M(151)+M(159)-M(162)+M(165)-M(173)+M(183)-M(197)+M(208)-M(210)+M(211)-M(212)+M(213)-M(216)+M(219)-M(222) &
    +M(229))) * den(522)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,227)
  Gcoeff = (c(2)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(522)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,228)
  Gcoeff = (c(2)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(522)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,229)
  Gcoeff = (c(2)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(522)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,230)
  Gcoeff = (c(3)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(522)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,231)
  Gcoeff = (c(3)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(522)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,232)
  Gcoeff = (c(3)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(522)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,233)
  Gcoeff = (c(1)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(2)*(-M(135)+M(149)-M(189)+M(199) &
    +M(204)-M(207)+M(218)-M(220))) * den(75)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,10)
  Gcoeff = (c(1)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(2)*(-M(173)-M(180)+M(183)+M(213) &
    +M(215)-M(216)-M(223)+M(233))) * den(75)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,19)
  Gcoeff = (c(2)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(75)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,28)
  Gcoeff = (c(1)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(2)*(-M(173)-M(180)+M(183)+M(213) &
    +M(215)-M(216)-M(223)+M(233))) * den(75)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,11)
  Gcoeff = (c(1)*(-M(5)+M(6)+M(19)-M(20)+M(27)-M(28)-M(33)+M(34)-M(64)+M(82)-M(87)+M(94))+c(2)*(-M(135)+M(149)-M(189)+M(199) &
    +M(204)-M(207)+M(218)-M(220))) * den(75)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,20)
  Gcoeff = (c(2)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(75)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,29)
  Gcoeff = (c(2)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(75)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,12)
  Gcoeff = (c(2)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(75)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,21)
  Gcoeff = (c(3)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(75)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,30)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(149)-M(151)+M(165)-M(189)+M(203)-M(205)-M(207)+M(208)-M(212)+M(218)+M(224)-M(226)-M(235) &
    +M(241))) * den(509)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,234)
  Gcoeff = (c(2)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(509)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,235)
  Gcoeff = (c(2)*(-M(149)+M(151)+M(162)-M(165)-M(186)+M(189)+M(207)-M(208)-M(211)+M(212)+M(217)-M(218)-M(221)+M(222)+M(227) &
    -M(229))) * den(509)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,236)
  Gcoeff = (c(2)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(509)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,237)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(509)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,238)
  Gcoeff = (c(2)*(M(149)-M(151)-M(162)+M(165)+M(186)-M(189)-M(207)+M(208)+M(211)-M(212)-M(217)+M(218)+M(221)-M(222)-M(227) &
    +M(229))) * den(509)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,239)
  Gcoeff = (c(3)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(509)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,240)
  Gcoeff = (c(3)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(509)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,241)
  Gcoeff = (c(3)*(M(149)-M(151)-M(162)+M(165)+M(186)-M(189)-M(207)+M(208)+M(211)-M(212)-M(217)+M(218)+M(221)-M(222)-M(227) &
    +M(229))) * den(509)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,242)
  Gcoeff = (c(2)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(405)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,243)
  Gcoeff = (c(2)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(405)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,244)
  Gcoeff = (c(3)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(405)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,245)
  Gcoeff = (c(2)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(405)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,246)
  Gcoeff = (c(2)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(405)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,247)
  Gcoeff = (c(3)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(405)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,248)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(156)-M(159)+M(197)-M(199)+M(203)-M(204)-M(209)+M(210)-M(219)+M(220)+M(225)-M(226)-M(239) &
    +M(241))) * den(405)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,249)
  Gcoeff = (c(2)*(M(132)-M(135)-M(156)+M(159)-M(197)+M(199)-M(203)+M(204)+M(209)-M(210)+M(219)-M(220)-M(225)+M(226)+M(239) &
    -M(241))) * den(405)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,250)
  Gcoeff = (c(3)*(M(132)-M(135)-M(156)+M(159)-M(197)+M(199)-M(203)+M(204)+M(209)-M(210)+M(219)-M(220)-M(225)+M(226)+M(239) &
    -M(241))) * den(405)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,251)
  Gcoeff = (c(1)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(2)*(-M(133)+M(143)-M(187)+M(193) &
    +M(228)-M(231)+M(242)-M(244))) * den(77)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,55)
  Gcoeff = (c(1)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(2)*(-M(137)+M(139)-M(169)+M(211) &
    +M(229)-M(230)+M(238)-M(248))) * den(77)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,64)
  Gcoeff = (c(2)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(77)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,73)
  Gcoeff = (c(1)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(2)*(-M(137)+M(139)-M(169)+M(211) &
    +M(229)-M(230)+M(238)-M(248))) * den(77)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,56)
  Gcoeff = (c(1)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(2)*(-M(133)+M(143)-M(187)+M(193) &
    +M(228)-M(231)+M(242)-M(244))) * den(77)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,65)
  Gcoeff = (c(2)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(77)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,74)
  Gcoeff = (c(2)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(77)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,57)
  Gcoeff = (c(2)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(77)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,66)
  Gcoeff = (c(3)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(77)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,75)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(496)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,252)
  Gcoeff = (c(2)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(496)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,253)
  Gcoeff = (c(2)*(M(150)-M(153)-M(168)+M(171)-M(183)+M(184)+M(187)-M(188)-M(193)+M(194)+M(197)-M(198)+M(210)-M(213)-M(228) &
    +M(231))) * den(496)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,254)
  Gcoeff = (c(2)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(496)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,255)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(496)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,256)
  Gcoeff = (c(2)*(-M(150)+M(153)+M(168)-M(171)+M(183)-M(184)-M(187)+M(188)+M(193)-M(194)-M(197)+M(198)-M(210)+M(213)+M(228) &
    -M(231))) * den(496)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,257)
  Gcoeff = (c(3)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(496)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,258)
  Gcoeff = (c(3)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(496)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,259)
  Gcoeff = (c(3)*(-M(150)+M(153)+M(168)-M(171)+M(183)-M(184)-M(187)+M(188)+M(193)-M(194)-M(197)+M(198)-M(210)+M(213)+M(228) &
    -M(231))) * den(496)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,260)
  Gcoeff = (c(2)*(-M(133)+M(137)-M(139)+M(143)+M(150)-M(151)+M(152)-M(153)+M(170)-M(188)+M(194)-M(212)-M(238)+M(242)-M(244) &
    +M(248))) * den(311)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,261)
  Gcoeff = (c(2)*(M(133)-M(137)+M(139)-M(143)-M(150)+M(151)-M(152)+M(153)-M(170)+M(188)-M(194)+M(212)+M(238)-M(242)+M(244) &
    -M(248))) * den(311)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,262)
  Gcoeff = (c(3)*(M(133)-M(137)+M(139)-M(143)-M(150)+M(151)-M(152)+M(153)-M(170)+M(188)-M(194)+M(212)+M(238)-M(242)+M(244) &
    -M(248))) * den(311)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,263)
  Gcoeff = (c(2)*(M(134)-M(137)+M(139)+M(141)-M(142)-M(144)-M(150)+M(153)-M(172)+M(188)-M(194)-M(200)+M(202)+M(214)+M(238) &
    -M(248))) * den(311)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,264)
  Gcoeff = (c(2)*(-M(134)+M(137)-M(139)-M(141)+M(142)+M(144)+M(150)-M(153)+M(172)-M(188)+M(194)+M(200)-M(202)-M(214)-M(238) &
    +M(248))) * den(311)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,265)
  Gcoeff = (c(3)*(-M(134)+M(137)-M(139)-M(141)+M(142)+M(144)+M(150)-M(153)+M(172)-M(188)+M(194)+M(200)-M(202)-M(214)-M(238) &
    +M(248))) * den(311)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,266)
  Gcoeff = (c(2)*(M(133)-M(134)-M(141)+M(142)-M(143)+M(144)+M(151)-M(152)-M(170)+M(172)+M(200)-M(202)+M(212)-M(214)-M(242) &
    +M(244))) * den(311)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,267)
  Gcoeff = (c(2)*(-M(133)+M(134)+M(141)-M(142)+M(143)-M(144)-M(151)+M(152)+M(170)-M(172)-M(200)+M(202)-M(212)+M(214)+M(242) &
    -M(244))) * den(311)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,268)
  Gcoeff = (c(3)*(-M(133)+M(134)+M(141)-M(142)+M(143)-M(144)-M(151)+M(152)+M(170)-M(172)-M(200)+M(202)-M(212)+M(214)+M(242) &
    -M(244))) * den(311)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,269)
  Gcoeff = (c(1)*(-M(43)+M(55)+M(58)+M(61)-M(70)-M(82)+M(89)-M(92)-M(93)-M(94)+M(96)+M(99))+c(2)*(M(161)+M(167)-M(181)-M(205) &
    -M(235)-M(237)+M(240)+M(246))) * den(241)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,133)
  Gcoeff = (c(1)*(-M(43)+M(55)+M(58)+M(61)-M(70)-M(82)+M(89)-M(92)-M(93)-M(94)+M(96)+M(99))+c(2)*(-M(145)+M(157)+M(163)-M(191) &
    +M(232)-M(234)-M(236)+M(243))) * den(241)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,134)
  Gcoeff = (c(2)*(-M(145)+M(157)-M(161)+M(163)-M(167)+M(181)-M(191)+M(205)+M(232)-M(234)+M(235)-M(236)+M(237)-M(240)+M(243) &
    -M(246))) * den(241)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,135)
  Gcoeff = (c(1)*(-M(43)+M(55)+M(58)+M(61)-M(70)-M(82)+M(89)-M(92)-M(93)-M(94)+M(96)+M(99))+c(2)*(-M(145)+M(157)+M(163)-M(191) &
    +M(232)-M(234)-M(236)+M(243))) * den(241)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,136)
  Gcoeff = (c(1)*(-M(43)+M(55)+M(58)+M(61)-M(70)-M(82)+M(89)-M(92)-M(93)-M(94)+M(96)+M(99))+c(2)*(M(161)+M(167)-M(181)-M(205) &
    -M(235)-M(237)+M(240)+M(246))) * den(241)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,137)
  Gcoeff = (c(2)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(241)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,138)
  Gcoeff = (c(2)*(-M(145)+M(157)-M(161)+M(163)-M(167)+M(181)-M(191)+M(205)+M(232)-M(234)+M(235)-M(236)+M(237)-M(240)+M(243) &
    -M(246))) * den(241)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,139)
  Gcoeff = (c(2)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(241)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,140)
  Gcoeff = (c(3)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(241)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,141)
  Gcoeff = (c(2)*(M(143)-M(145)-M(161)+M(163)+M(185)-M(187)-M(203)+M(205)-M(231)+M(232)+M(235)-M(236)-M(241)+M(242)+M(245) &
    -M(246))) * den(513)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,273)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(513)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,274)
  Gcoeff = (c(2)*(M(131)-M(137)-M(143)+M(145)-M(163)+M(187)+M(211)-M(217)-M(227)+M(229)+M(231)-M(232)+M(236)-M(242)-M(248) &
    +M(250))) * den(513)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,275)
  Gcoeff = (c(2)*(-M(143)+M(145)+M(161)-M(163)-M(185)+M(187)+M(203)-M(205)+M(231)-M(232)-M(235)+M(236)+M(241)-M(242)-M(245) &
    +M(246))) * den(513)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,276)
  Gcoeff = (c(2)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(513)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,277)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(513)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,278)
  Gcoeff = (c(3)*(-M(143)+M(145)+M(161)-M(163)-M(185)+M(187)+M(203)-M(205)+M(231)-M(232)-M(235)+M(236)+M(241)-M(242)-M(245) &
    +M(246))) * den(513)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,279)
  Gcoeff = (c(3)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(513)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,280)
  Gcoeff = (c(3)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(513)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,281)
  Gcoeff = (c(1)*(-M(43)+M(50)+M(62)+M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94))+c(2)*(M(175)+M(177)-M(181)+M(182) &
    -M(205)+M(206)-M(235)-M(237))) * den(245)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,142)
  Gcoeff = (c(1)*(-M(43)+M(50)+M(62)+M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94))+c(2)*(-M(145)+M(146)+M(174)+M(176) &
    -M(191)+M(192)-M(234)-M(236))) * den(245)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,143)
  Gcoeff = (c(2)*(-M(145)+M(146)+M(174)-M(175)+M(176)-M(177)+M(181)-M(182)-M(191)+M(192)+M(205)-M(206)-M(234)+M(235)-M(236) &
    +M(237))) * den(245)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,144)
  Gcoeff = (c(1)*(-M(43)+M(50)+M(62)+M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94))+c(2)*(-M(145)+M(146)+M(174)+M(176) &
    -M(191)+M(192)-M(234)-M(236))) * den(245)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,145)
  Gcoeff = (c(1)*(-M(43)+M(50)+M(62)+M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94))+c(2)*(M(175)+M(177)-M(181)+M(182) &
    -M(205)+M(206)-M(235)-M(237))) * den(245)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,146)
  Gcoeff = (c(2)*(M(145)-M(146)-M(174)+M(175)-M(176)+M(177)-M(181)+M(182)+M(191)-M(192)-M(205)+M(206)+M(234)-M(235)+M(236) &
    -M(237))) * den(245)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,147)
  Gcoeff = (c(2)*(-M(145)+M(146)+M(174)-M(175)+M(176)-M(177)+M(181)-M(182)-M(191)+M(192)+M(205)-M(206)-M(234)+M(235)-M(236) &
    +M(237))) * den(245)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,148)
  Gcoeff = (c(2)*(M(145)-M(146)-M(174)+M(175)-M(176)+M(177)-M(181)+M(182)+M(191)-M(192)-M(205)+M(206)+M(234)-M(235)+M(236) &
    -M(237))) * den(245)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,149)
  Gcoeff = (c(3)*(M(145)-M(146)-M(174)+M(175)-M(176)+M(177)-M(181)+M(182)+M(191)-M(192)-M(205)+M(206)+M(234)-M(235)+M(236) &
    -M(237))) * den(245)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,150)
  Gcoeff = (c(2)*(-M(144)+M(147)+M(174)-M(177)+M(181)-M(182)-M(189)+M(190)-M(191)+M(192)+M(199)-M(200)+M(204)-M(207)-M(234) &
    +M(237))) * den(500)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,285)
  Gcoeff = (c(2)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(500)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,286)
  Gcoeff = (c(1)*(-M(9)+M(10)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)-M(73)+M(85)+M(97)-M(100))+c(2)*(-M(139)+M(145)-M(163)+M(169) &
    +M(230)-M(232)+M(236)-M(238))) * den(88)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,58)
  Gcoeff = (c(1)*(-M(9)+M(10)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)-M(73)+M(85)+M(97)-M(100))+c(2)*(-M(131)+M(133)-M(193)+M(217) &
    +M(227)-M(228)+M(244)-M(250))) * den(88)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,67)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(139)-M(145)+M(163)-M(169)-M(193)+M(217)+M(227)-M(228)-M(230)+M(232)-M(236)+M(238)+M(244) &
    -M(250))) * den(88)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,76)
  Gcoeff = (c(1)*(-M(9)+M(10)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)-M(73)+M(85)+M(97)-M(100))+c(2)*(-M(131)+M(133)-M(193)+M(217) &
    +M(227)-M(228)+M(244)-M(250))) * den(88)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,59)
  Gcoeff = (c(1)*(-M(9)+M(10)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)-M(73)+M(85)+M(97)-M(100))+c(2)*(-M(139)+M(145)-M(163)+M(169) &
    +M(230)-M(232)+M(236)-M(238))) * den(88)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,68)
  Gcoeff = (c(2)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(88)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,77)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(139)-M(145)+M(163)-M(169)-M(193)+M(217)+M(227)-M(228)-M(230)+M(232)-M(236)+M(238)+M(244) &
    -M(250))) * den(88)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,60)
  Gcoeff = (c(2)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(88)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,69)
  Gcoeff = (c(3)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(88)
  T2sum(1:1,17) = T2sum(1:1,17) + Gcoeff * G0tensor(:,78)
  Gcoeff = (c(2)*(M(131)-M(137)-M(143)+M(145)-M(163)+M(187)+M(211)-M(217)-M(227)+M(229)+M(231)-M(232)+M(236)-M(242)-M(248) &
    +M(250))) * den(58)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,72)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(58)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,78)
  Gcoeff = (c(3)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(58)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,84)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(139)-M(145)+M(163)-M(169)-M(193)+M(217)+M(227)-M(228)-M(230)+M(232)-M(236)+M(238)+M(244) &
    -M(250))) * den(58)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,73)
  Gcoeff = (c(2)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(58)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,79)
  Gcoeff = (c(3)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(58)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,85)
  Gcoeff = (c(2)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(58)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,74)
  Gcoeff = (c(2)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(58)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,80)
  Gcoeff = (c(3)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(58)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,86)
  Gcoeff = (c(1)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(125)+M(127)+M(129)-M(130))+c(2)*(M(173)-M(174)+M(191)-M(192) &
    -M(215)+M(216)-M(233)+M(234))) * den(131)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,13)
  Gcoeff = (c(1)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(125)+M(127)+M(129)-M(130))+c(2)*(M(135)-M(136)-M(149)+M(150) &
    +M(194)-M(196)-M(218)+M(220))) * den(131)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,14)
  Gcoeff = (c(2)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(131)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,15)
  Gcoeff = (c(1)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(125)+M(127)+M(129)-M(130))+c(2)*(M(135)-M(136)-M(149)+M(150) &
    +M(194)-M(196)-M(218)+M(220))) * den(131)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,22)
  Gcoeff = (c(1)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(125)+M(127)+M(129)-M(130))+c(2)*(M(173)-M(174)+M(191)-M(192) &
    -M(215)+M(216)-M(233)+M(234))) * den(131)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,23)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(131)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,24)
  Gcoeff = (c(2)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(131)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,31)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(131)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,32)
  Gcoeff = (c(3)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(131)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,33)
  Gcoeff = (c(2)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(113)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,12)
  Gcoeff = (c(2)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(113)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,13)
  Gcoeff = (c(2)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(113)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,14)
  Gcoeff = (c(2)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(113)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,18)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(113)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,19)
  Gcoeff = (c(2)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(113)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,20)
  Gcoeff = (c(3)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(113)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,24)
  Gcoeff = (c(3)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(113)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,25)
  Gcoeff = (c(3)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(113)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,26)
  Gcoeff = (c(2)*(M(147)-M(158)+M(168)+M(174)-M(177)-M(182)+M(185)-M(187)-M(189)+M(190)+M(192)+M(198)-M(201)-M(207)-M(231) &
    +M(245))) * den(215)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(3)*(M(147)-M(158)+M(168)+M(174)-M(177)-M(182)+M(185)-M(187)-M(189)+M(190)+M(192)+M(198)-M(201)-M(207)-M(231) &
    +M(245))) * den(215)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,2)
  Gcoeff = (c(2)*(-M(138)+M(141)+M(173)-M(175)+M(180)-M(183)+M(205)-M(206)-M(213)+M(214)-M(215)+M(216)+M(223)-M(224)-M(233) &
    +M(235))) * den(1374)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,108)
  Gcoeff = (c(2)*(M(138)-M(141)-M(173)+M(175)-M(180)+M(183)-M(205)+M(206)+M(213)-M(214)+M(215)-M(216)-M(223)+M(224)+M(233) &
    -M(235))) * den(1374)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,109)
  Gcoeff = (c(3)*(M(138)-M(141)-M(173)+M(175)-M(180)+M(183)-M(205)+M(206)+M(213)-M(214)+M(215)-M(216)-M(223)+M(224)+M(233) &
    -M(235))) * den(1374)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,110)
  Gcoeff = (c(2)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(233)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,120)
  Gcoeff = (c(2)*(M(141)-M(156)+M(162)+M(173)-M(175)-M(183)-M(206)+M(209)-M(211)-M(213)+M(214)+M(216)+M(222)-M(225)-M(229) &
    +M(239))) * den(233)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,121)
  Gcoeff = (c(3)*(M(141)-M(156)+M(162)+M(173)-M(175)-M(183)-M(206)+M(209)-M(211)-M(213)+M(214)+M(216)+M(222)-M(225)-M(229) &
    +M(239))) * den(233)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,122)
  Gcoeff = (c(2)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248))) * den(1376)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,132)
  Gcoeff = (c(2)*(M(137)-M(139)-M(167)+M(169)-M(179)+M(181)+M(209)-M(211)-M(229)+M(230)+M(237)-M(238)+M(239)-M(240)-M(247) &
    +M(248))) * den(1376)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,133)
  Gcoeff = (c(3)*(M(137)-M(139)-M(167)+M(169)-M(179)+M(181)+M(209)-M(211)-M(229)+M(230)+M(237)-M(238)+M(239)-M(240)-M(247) &
    +M(248))) * den(1376)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,134)
  Gcoeff = (c(2)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(240)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,144)
  Gcoeff = (c(2)*(M(141)-M(142)-M(167)+M(169)+M(171)-M(172)-M(183)+M(184)+M(209)-M(211)-M(213)+M(214)-M(229)+M(230)+M(239) &
    -M(240))) * den(240)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,145)
  Gcoeff = (c(3)*(M(141)-M(142)-M(167)+M(169)+M(171)-M(172)-M(183)+M(184)+M(209)-M(211)-M(213)+M(214)-M(229)+M(230)+M(239) &
    -M(240))) * den(240)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,146)
  Gcoeff = (c(2)*(-M(139)+M(155)-M(161)-M(167)+M(169)+M(181)+M(205)-M(215)+M(230)-M(233)+M(235)+M(237)-M(238)-M(240)-M(246) &
    +M(249))) * den(242)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,192)
  Gcoeff = (c(2)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(242)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,193)
  Gcoeff = (c(3)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(242)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,194)
  Gcoeff = (c(2)*(-M(139)+M(140)+M(173)-M(175)-M(177)+M(178)+M(181)-M(182)+M(205)-M(206)-M(215)+M(216)-M(233)+M(235)+M(237) &
    -M(238))) * den(246)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,204)
  Gcoeff = (c(2)*(M(139)-M(140)-M(173)+M(175)+M(177)-M(178)-M(181)+M(182)-M(205)+M(206)+M(215)-M(216)+M(233)-M(235)-M(237) &
    +M(238))) * den(246)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,205)
  Gcoeff = (c(3)*(M(139)-M(140)-M(173)+M(175)+M(177)-M(178)-M(181)+M(182)-M(205)+M(206)+M(215)-M(216)+M(233)-M(235)-M(237) &
    +M(238))) * den(246)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,206)
  Gcoeff = (c(2)*(-M(143)+M(145)+M(161)-M(163)-M(185)+M(187)+M(203)-M(205)+M(231)-M(232)-M(235)+M(236)+M(241)-M(242)-M(245) &
    +M(246))) * den(1380)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,48)
  Gcoeff = (c(2)*(M(143)-M(145)-M(161)+M(163)+M(185)-M(187)-M(203)+M(205)-M(231)+M(232)+M(235)-M(236)-M(241)+M(242)+M(245) &
    -M(246))) * den(1380)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,49)
  Gcoeff = (c(3)*(M(143)-M(145)-M(161)+M(163)+M(185)-M(187)-M(203)+M(205)-M(231)+M(232)+M(235)-M(236)-M(241)+M(242)+M(245) &
    -M(246))) * den(1380)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,50)
  Gcoeff = (c(2)*(-M(147)+M(148)+M(161)-M(163)-M(165)+M(166)-M(185)+M(187)+M(189)-M(190)+M(207)-M(208)+M(231)-M(232)-M(245) &
    +M(246))) * den(258)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,60)
  Gcoeff = (c(2)*(M(147)-M(148)-M(161)+M(163)+M(165)-M(166)+M(185)-M(187)-M(189)+M(190)-M(207)+M(208)-M(231)+M(232)+M(245) &
    -M(246))) * den(258)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,61)
  Gcoeff = (c(3)*(M(147)-M(148)-M(161)+M(163)+M(165)-M(166)+M(185)-M(187)-M(189)+M(190)-M(207)+M(208)-M(231)+M(232)+M(245) &
    -M(246))) * den(258)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,62)
  Gcoeff = (c(2)*(-M(145)+M(157)-M(161)+M(163)-M(167)+M(181)-M(191)+M(205)+M(232)-M(234)+M(235)-M(236)+M(237)-M(240)+M(243) &
    -M(246))) * den(273)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,270)
  Gcoeff = (c(2)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(273)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,271)
  Gcoeff = (c(3)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(273)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,272)
  Gcoeff = (c(2)*(-M(145)+M(146)+M(174)-M(175)+M(176)-M(177)+M(181)-M(182)-M(191)+M(192)+M(205)-M(206)-M(234)+M(235)-M(236) &
    +M(237))) * den(276)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,282)
  Gcoeff = (c(2)*(M(145)-M(146)-M(174)+M(175)-M(176)+M(177)-M(181)+M(182)+M(191)-M(192)-M(205)+M(206)+M(234)-M(235)+M(236) &
    -M(237))) * den(276)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,283)
  Gcoeff = (c(3)*(M(145)-M(146)-M(174)+M(175)-M(176)+M(177)-M(181)+M(182)+M(191)-M(192)-M(205)+M(206)+M(234)-M(235)+M(236) &
    -M(237))) * den(276)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,284)
  Gcoeff = (c(2)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(585)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,15)
  Gcoeff = (c(2)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(585)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,21)
  Gcoeff = (c(3)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(585)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,27)
  Gcoeff = (c(2)*(M(131)-M(137)-M(143)+M(145)-M(163)+M(187)+M(211)-M(217)-M(227)+M(229)+M(231)-M(232)+M(236)-M(242)-M(248) &
    +M(250))) * den(592)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,75)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(592)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,81)
  Gcoeff = (c(3)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(592)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,87)
  Gcoeff = (c(2)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(593)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,16)
  Gcoeff = (c(2)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(593)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,22)
  Gcoeff = (c(3)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(593)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,28)
  Gcoeff = (c(2)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(598)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,76)
  Gcoeff = (c(2)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(598)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,82)
  Gcoeff = (c(3)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(598)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,88)
  Gcoeff = (c(2)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(600)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,77)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(139)-M(145)+M(163)-M(169)-M(193)+M(217)+M(227)-M(228)-M(230)+M(232)-M(236)+M(238)+M(244) &
    -M(250))) * den(600)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,83)
  Gcoeff = (c(3)*(-M(131)+M(133)+M(139)-M(145)+M(163)-M(169)-M(193)+M(217)+M(227)-M(228)-M(230)+M(232)-M(236)+M(238)+M(244) &
    -M(250))) * den(600)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,89)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(290)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,17)
  Gcoeff = (c(2)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(290)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,23)
  Gcoeff = (c(3)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(290)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,29)

end subroutine vamp_79

end module ol_vamp_79_ppjjjj_gggggg_1_/**/REALKIND
