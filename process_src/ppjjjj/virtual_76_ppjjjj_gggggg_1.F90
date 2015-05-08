
module ol_vamp_76_ppjjjj_gggggg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_76(M)
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
  complex(REALKIND), dimension(4,1,4,117) :: G0
  complex(REALKIND), dimension(1,237) :: G0tensor
  complex(REALKIND), dimension(5,226) :: G1tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,103),G0(:,:,:,2))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-4),wf(:,-3),G0tensor(:,1))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,-4),G0tensor(:,2))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-4),wf(:,-3),G0tensor(:,3))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,1))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,100),G0(:,:,:,3))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,2))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,101),G0(:,:,:,4))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,3))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,102),G0(:,:,:,5))
  call check_last_UV_W(l_switch,G0(:,:,:,5),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,4))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,100),wf(:,-5),G0(:,:,:,6))
  call check_last_UV_W(l_switch,G0(:,:,:,6),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,5))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,101),wf(:,-5),G0(:,:,:,7))
  call check_last_UV_W(l_switch,G0(:,:,:,7),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,6))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,102),wf(:,-5),G0(:,:,:,8))
  call check_last_UV_W(l_switch,G0(:,:,:,8),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,7))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,100),G0(:,:,:,9))
  call check_last_UV_W(l_switch,G0(:,:,:,9),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,8))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,101),G0(:,:,:,10))
  call check_last_UV_W(l_switch,G0(:,:,:,10),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,9))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,102),G0(:,:,:,11))
  call check_last_UV_W(l_switch,G0(:,:,:,11),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,10))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,99),G0(:,:,:,12))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-3),wf(:,90),G0tensor(:,4))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,90),wf(:,-3),G0tensor(:,5))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,-3),wf(:,90),G0tensor(:,6))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-2),wf(:,104),G0tensor(:,7))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,104),wf(:,-2),G0tensor(:,8))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,-2),wf(:,104),G0tensor(:,9))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,0),wf(:,62),G0tensor(:,10))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,62),wf(:,0),G0tensor(:,11))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,0),wf(:,62),G0tensor(:,12))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,50),wf(:,25),Q(:,13),G1tensor(:,11))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,50),wf(:,27),Q(:,13),G1tensor(:,12))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,50),wf(:,28),Q(:,13),G1tensor(:,13))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,50),wf(:,148),Q(:,13),G1tensor(:,14))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,50),wf(:,160),Q(:,13),G1tensor(:,15))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,50),wf(:,167),Q(:,13),G1tensor(:,16))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,99),wf(:,-4),G0(:,:,:,13))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-3),wf(:,90),G0tensor(:,13))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,90),wf(:,-3),G0tensor(:,14))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-3),wf(:,90),G0tensor(:,15))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-2),wf(:,104),G0tensor(:,16))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,104),wf(:,-2),G0tensor(:,17))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-2),wf(:,104),G0tensor(:,18))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,0),wf(:,62),G0tensor(:,19))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,62),wf(:,0),G0tensor(:,20))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,0),wf(:,62),G0tensor(:,21))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,50),wf(:,25),Q(:,13),G1tensor(:,17))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,50),wf(:,27),Q(:,13),G1tensor(:,18))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,50),wf(:,28),Q(:,13),G1tensor(:,19))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,50),wf(:,148),Q(:,13),G1tensor(:,20))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,50),wf(:,160),Q(:,13),G1tensor(:,21))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,50),wf(:,167),Q(:,13),G1tensor(:,22))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,99),G0(:,:,:,14))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,14),wf(:,-3),wf(:,90),G0tensor(:,22))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,14),wf(:,90),wf(:,-3),G0tensor(:,23))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,14),wf(:,-3),wf(:,90),G0tensor(:,24))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,14),wf(:,-2),wf(:,104),G0tensor(:,25))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,14),wf(:,104),wf(:,-2),G0tensor(:,26))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,14),wf(:,-2),wf(:,104),G0tensor(:,27))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,14),wf(:,0),wf(:,62),G0tensor(:,28))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,14),wf(:,62),wf(:,0),G0tensor(:,29))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,14),wf(:,0),wf(:,62),G0tensor(:,30))
  call check_last_UV_W(l_switch,G0(:,:,:,14),Q(:,50),wf(:,25),Q(:,13),G1tensor(:,23))
  call check_last_UV_W(l_switch,G0(:,:,:,14),Q(:,50),wf(:,27),Q(:,13),G1tensor(:,24))
  call check_last_UV_W(l_switch,G0(:,:,:,14),Q(:,50),wf(:,28),Q(:,13),G1tensor(:,25))
  call check_last_UV_W(l_switch,G0(:,:,:,14),Q(:,50),wf(:,148),Q(:,13),G1tensor(:,26))
  call check_last_UV_W(l_switch,G0(:,:,:,14),Q(:,50),wf(:,160),Q(:,13),G1tensor(:,27))
  call check_last_UV_W(l_switch,G0(:,:,:,14),Q(:,50),wf(:,167),Q(:,13),G1tensor(:,28))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,99),G0(:,:,:,15))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,15),wf(:,-4),wf(:,90),G0tensor(:,31))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,15),wf(:,90),wf(:,-4),G0tensor(:,32))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,15),wf(:,-4),wf(:,90),G0tensor(:,33))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,15),wf(:,-2),wf(:,109),G0tensor(:,34))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,15),wf(:,109),wf(:,-2),G0tensor(:,35))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,15),wf(:,-2),wf(:,109),G0tensor(:,36))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,15),wf(:,0),wf(:,66),G0tensor(:,37))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,15),wf(:,66),wf(:,0),G0tensor(:,38))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,15),wf(:,0),wf(:,66),G0tensor(:,39))
  call check_last_UV_W(l_switch,G0(:,:,:,15),Q(:,42),wf(:,31),Q(:,21),G1tensor(:,29))
  call check_last_UV_W(l_switch,G0(:,:,:,15),Q(:,42),wf(:,33),Q(:,21),G1tensor(:,30))
  call check_last_UV_W(l_switch,G0(:,:,:,15),Q(:,42),wf(:,34),Q(:,21),G1tensor(:,31))
  call check_last_UV_W(l_switch,G0(:,:,:,15),Q(:,42),wf(:,152),Q(:,21),G1tensor(:,32))
  call check_last_UV_W(l_switch,G0(:,:,:,15),Q(:,42),wf(:,171),Q(:,21),G1tensor(:,33))
  call check_last_UV_W(l_switch,G0(:,:,:,15),Q(:,42),wf(:,175),Q(:,21),G1tensor(:,34))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,99),wf(:,-3),G0(:,:,:,16))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,16),wf(:,-4),wf(:,90),G0tensor(:,40))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,16),wf(:,90),wf(:,-4),G0tensor(:,41))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,16),wf(:,-4),wf(:,90),G0tensor(:,42))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,16),wf(:,-2),wf(:,109),G0tensor(:,43))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,16),wf(:,109),wf(:,-2),G0tensor(:,44))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,16),wf(:,-2),wf(:,109),G0tensor(:,45))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,16),wf(:,0),wf(:,66),G0tensor(:,46))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,16),wf(:,66),wf(:,0),G0tensor(:,47))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,16),wf(:,0),wf(:,66),G0tensor(:,48))
  call check_last_UV_W(l_switch,G0(:,:,:,16),Q(:,42),wf(:,31),Q(:,21),G1tensor(:,35))
  call check_last_UV_W(l_switch,G0(:,:,:,16),Q(:,42),wf(:,33),Q(:,21),G1tensor(:,36))
  call check_last_UV_W(l_switch,G0(:,:,:,16),Q(:,42),wf(:,34),Q(:,21),G1tensor(:,37))
  call check_last_UV_W(l_switch,G0(:,:,:,16),Q(:,42),wf(:,152),Q(:,21),G1tensor(:,38))
  call check_last_UV_W(l_switch,G0(:,:,:,16),Q(:,42),wf(:,171),Q(:,21),G1tensor(:,39))
  call check_last_UV_W(l_switch,G0(:,:,:,16),Q(:,42),wf(:,175),Q(:,21),G1tensor(:,40))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,99),G0(:,:,:,17))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,17),wf(:,-4),wf(:,90),G0tensor(:,49))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,17),wf(:,90),wf(:,-4),G0tensor(:,50))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,17),wf(:,-4),wf(:,90),G0tensor(:,51))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,17),wf(:,-2),wf(:,109),G0tensor(:,52))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,17),wf(:,109),wf(:,-2),G0tensor(:,53))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,17),wf(:,-2),wf(:,109),G0tensor(:,54))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,17),wf(:,0),wf(:,66),G0tensor(:,55))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,17),wf(:,66),wf(:,0),G0tensor(:,56))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,17),wf(:,0),wf(:,66),G0tensor(:,57))
  call check_last_UV_W(l_switch,G0(:,:,:,17),Q(:,42),wf(:,31),Q(:,21),G1tensor(:,41))
  call check_last_UV_W(l_switch,G0(:,:,:,17),Q(:,42),wf(:,33),Q(:,21),G1tensor(:,42))
  call check_last_UV_W(l_switch,G0(:,:,:,17),Q(:,42),wf(:,34),Q(:,21),G1tensor(:,43))
  call check_last_UV_W(l_switch,G0(:,:,:,17),Q(:,42),wf(:,152),Q(:,21),G1tensor(:,44))
  call check_last_UV_W(l_switch,G0(:,:,:,17),Q(:,42),wf(:,171),Q(:,21),G1tensor(:,45))
  call check_last_UV_W(l_switch,G0(:,:,:,17),Q(:,42),wf(:,175),Q(:,21),G1tensor(:,46))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,96),G0(:,:,:,18))
  call check_last_UV_W(l_switch,G0(:,:,:,18),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,47))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,97),G0(:,:,:,19))
  call check_last_UV_W(l_switch,G0(:,:,:,19),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,48))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,98),G0(:,:,:,20))
  call check_last_UV_W(l_switch,G0(:,:,:,20),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,49))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,96),wf(:,-4),G0(:,:,:,21))
  call check_last_UV_W(l_switch,G0(:,:,:,21),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,50))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,97),wf(:,-4),G0(:,:,:,22))
  call check_last_UV_W(l_switch,G0(:,:,:,22),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,51))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,98),wf(:,-4),G0(:,:,:,23))
  call check_last_UV_W(l_switch,G0(:,:,:,23),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,52))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,96),G0(:,:,:,24))
  call check_last_UV_W(l_switch,G0(:,:,:,24),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,53))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,97),G0(:,:,:,25))
  call check_last_UV_W(l_switch,G0(:,:,:,25),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,54))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,98),G0(:,:,:,26))
  call check_last_UV_W(l_switch,G0(:,:,:,26),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,55))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,92),G0(:,:,:,27))
  call check_last_UV_W(l_switch,G0(:,:,:,27),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,56))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,92),wf(:,-3),G0(:,:,:,28))
  call check_last_UV_W(l_switch,G0(:,:,:,28),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,57))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,92),G0(:,:,:,29))
  call check_last_UV_W(l_switch,G0(:,:,:,29),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,58))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,93),G0(:,:,:,30))
  call check_last_UV_W(l_switch,G0(:,:,:,30),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,59))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,93),wf(:,-3),G0(:,:,:,31))
  call check_last_UV_W(l_switch,G0(:,:,:,31),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,60))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,93),G0(:,:,:,32))
  call check_last_UV_W(l_switch,G0(:,:,:,32),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,61))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,94),G0(:,:,:,33))
  call check_last_UV_W(l_switch,G0(:,:,:,33),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,62))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,94),wf(:,-3),G0(:,:,:,34))
  call check_last_UV_W(l_switch,G0(:,:,:,34),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,63))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,94),G0(:,:,:,35))
  call check_last_UV_W(l_switch,G0(:,:,:,35),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,64))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,105),G0(:,:,:,36))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,36),wf(:,-4),wf(:,104),G0tensor(:,58))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,36),wf(:,104),wf(:,-4),G0tensor(:,59))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,36),wf(:,-4),wf(:,104),G0tensor(:,60))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,36),wf(:,-3),wf(:,109),G0tensor(:,61))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,36),wf(:,109),wf(:,-3),G0tensor(:,62))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,36),wf(:,-3),wf(:,109),G0tensor(:,63))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,36),wf(:,0),wf(:,75),G0tensor(:,64))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,36),wf(:,75),wf(:,0),G0tensor(:,65))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,36),wf(:,0),wf(:,75),G0tensor(:,66))
  call check_last_UV_W(l_switch,G0(:,:,:,36),Q(:,38),wf(:,43),Q(:,25),G1tensor(:,65))
  call check_last_UV_W(l_switch,G0(:,:,:,36),Q(:,38),wf(:,45),Q(:,25),G1tensor(:,66))
  call check_last_UV_W(l_switch,G0(:,:,:,36),Q(:,38),wf(:,46),Q(:,25),G1tensor(:,67))
  call check_last_UV_W(l_switch,G0(:,:,:,36),Q(:,38),wf(:,181),Q(:,25),G1tensor(:,68))
  call check_last_UV_W(l_switch,G0(:,:,:,36),Q(:,38),wf(:,186),Q(:,25),G1tensor(:,69))
  call check_last_UV_W(l_switch,G0(:,:,:,36),Q(:,38),wf(:,190),Q(:,25),G1tensor(:,70))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,105),wf(:,-5),G0(:,:,:,37))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,37),wf(:,-4),wf(:,104),G0tensor(:,67))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,37),wf(:,104),wf(:,-4),G0tensor(:,68))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,37),wf(:,-4),wf(:,104),G0tensor(:,69))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,37),wf(:,-3),wf(:,109),G0tensor(:,70))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,37),wf(:,109),wf(:,-3),G0tensor(:,71))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,37),wf(:,-3),wf(:,109),G0tensor(:,72))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,37),wf(:,0),wf(:,75),G0tensor(:,73))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,37),wf(:,75),wf(:,0),G0tensor(:,74))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,37),wf(:,0),wf(:,75),G0tensor(:,75))
  call check_last_UV_W(l_switch,G0(:,:,:,37),Q(:,38),wf(:,43),Q(:,25),G1tensor(:,71))
  call check_last_UV_W(l_switch,G0(:,:,:,37),Q(:,38),wf(:,45),Q(:,25),G1tensor(:,72))
  call check_last_UV_W(l_switch,G0(:,:,:,37),Q(:,38),wf(:,46),Q(:,25),G1tensor(:,73))
  call check_last_UV_W(l_switch,G0(:,:,:,37),Q(:,38),wf(:,181),Q(:,25),G1tensor(:,74))
  call check_last_UV_W(l_switch,G0(:,:,:,37),Q(:,38),wf(:,186),Q(:,25),G1tensor(:,75))
  call check_last_UV_W(l_switch,G0(:,:,:,37),Q(:,38),wf(:,190),Q(:,25),G1tensor(:,76))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,105),G0(:,:,:,38))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,38),wf(:,-4),wf(:,104),G0tensor(:,76))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,38),wf(:,104),wf(:,-4),G0tensor(:,77))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,38),wf(:,-4),wf(:,104),G0tensor(:,78))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,38),wf(:,-3),wf(:,109),G0tensor(:,79))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,38),wf(:,109),wf(:,-3),G0tensor(:,80))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,38),wf(:,-3),wf(:,109),G0tensor(:,81))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,38),wf(:,0),wf(:,75),G0tensor(:,82))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,38),wf(:,75),wf(:,0),G0tensor(:,83))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,38),wf(:,0),wf(:,75),G0tensor(:,84))
  call check_last_UV_W(l_switch,G0(:,:,:,38),Q(:,38),wf(:,43),Q(:,25),G1tensor(:,77))
  call check_last_UV_W(l_switch,G0(:,:,:,38),Q(:,38),wf(:,45),Q(:,25),G1tensor(:,78))
  call check_last_UV_W(l_switch,G0(:,:,:,38),Q(:,38),wf(:,46),Q(:,25),G1tensor(:,79))
  call check_last_UV_W(l_switch,G0(:,:,:,38),Q(:,38),wf(:,181),Q(:,25),G1tensor(:,80))
  call check_last_UV_W(l_switch,G0(:,:,:,38),Q(:,38),wf(:,186),Q(:,25),G1tensor(:,81))
  call check_last_UV_W(l_switch,G0(:,:,:,38),Q(:,38),wf(:,190),Q(:,25),G1tensor(:,82))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,104),G0(:,:,:,39))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,39),wf(:,-4),wf(:,105),G0tensor(:,85))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,39),wf(:,105),wf(:,-4),G0tensor(:,86))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,39),wf(:,-4),wf(:,105),G0tensor(:,87))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,39),wf(:,-2),wf(:,95),G0tensor(:,88))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,39),wf(:,95),wf(:,-2),G0tensor(:,89))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,39),wf(:,-2),wf(:,95),G0tensor(:,90))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,39),wf(:,-1),wf(:,66),G0tensor(:,91))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,39),wf(:,66),wf(:,-1),G0tensor(:,92))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,39),wf(:,-1),wf(:,66),G0tensor(:,93))
  call check_last_UV_W(l_switch,G0(:,:,:,39),Q(:,41),wf(:,50),Q(:,22),G1tensor(:,83))
  call check_last_UV_W(l_switch,G0(:,:,:,39),Q(:,41),wf(:,53),Q(:,22),G1tensor(:,84))
  call check_last_UV_W(l_switch,G0(:,:,:,39),Q(:,41),wf(:,54),Q(:,22),G1tensor(:,85))
  call check_last_UV_W(l_switch,G0(:,:,:,39),Q(:,41),wf(:,206),Q(:,22),G1tensor(:,86))
  call check_last_UV_W(l_switch,G0(:,:,:,39),Q(:,41),wf(:,225),Q(:,22),G1tensor(:,87))
  call check_last_UV_W(l_switch,G0(:,:,:,39),Q(:,41),wf(:,229),Q(:,22),G1tensor(:,88))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,104),wf(:,-5),G0(:,:,:,40))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,40),wf(:,-4),wf(:,105),G0tensor(:,94))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,40),wf(:,105),wf(:,-4),G0tensor(:,95))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,40),wf(:,-4),wf(:,105),G0tensor(:,96))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,40),wf(:,-2),wf(:,95),G0tensor(:,97))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,40),wf(:,95),wf(:,-2),G0tensor(:,98))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,40),wf(:,-2),wf(:,95),G0tensor(:,99))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,40),wf(:,-1),wf(:,66),G0tensor(:,100))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,40),wf(:,66),wf(:,-1),G0tensor(:,101))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,40),wf(:,-1),wf(:,66),G0tensor(:,102))
  call check_last_UV_W(l_switch,G0(:,:,:,40),Q(:,41),wf(:,50),Q(:,22),G1tensor(:,89))
  call check_last_UV_W(l_switch,G0(:,:,:,40),Q(:,41),wf(:,53),Q(:,22),G1tensor(:,90))
  call check_last_UV_W(l_switch,G0(:,:,:,40),Q(:,41),wf(:,54),Q(:,22),G1tensor(:,91))
  call check_last_UV_W(l_switch,G0(:,:,:,40),Q(:,41),wf(:,206),Q(:,22),G1tensor(:,92))
  call check_last_UV_W(l_switch,G0(:,:,:,40),Q(:,41),wf(:,225),Q(:,22),G1tensor(:,93))
  call check_last_UV_W(l_switch,G0(:,:,:,40),Q(:,41),wf(:,229),Q(:,22),G1tensor(:,94))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,104),G0(:,:,:,41))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,41),wf(:,-4),wf(:,105),G0tensor(:,103))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,41),wf(:,105),wf(:,-4),G0tensor(:,104))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,41),wf(:,-4),wf(:,105),G0tensor(:,105))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,41),wf(:,-2),wf(:,95),G0tensor(:,106))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,41),wf(:,95),wf(:,-2),G0tensor(:,107))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,41),wf(:,-2),wf(:,95),G0tensor(:,108))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,41),wf(:,-1),wf(:,66),G0tensor(:,109))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,41),wf(:,66),wf(:,-1),G0tensor(:,110))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,41),wf(:,-1),wf(:,66),G0tensor(:,111))
  call check_last_UV_W(l_switch,G0(:,:,:,41),Q(:,41),wf(:,50),Q(:,22),G1tensor(:,95))
  call check_last_UV_W(l_switch,G0(:,:,:,41),Q(:,41),wf(:,53),Q(:,22),G1tensor(:,96))
  call check_last_UV_W(l_switch,G0(:,:,:,41),Q(:,41),wf(:,54),Q(:,22),G1tensor(:,97))
  call check_last_UV_W(l_switch,G0(:,:,:,41),Q(:,41),wf(:,206),Q(:,22),G1tensor(:,98))
  call check_last_UV_W(l_switch,G0(:,:,:,41),Q(:,41),wf(:,225),Q(:,22),G1tensor(:,99))
  call check_last_UV_W(l_switch,G0(:,:,:,41),Q(:,41),wf(:,229),Q(:,22),G1tensor(:,100))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,109),G0(:,:,:,42))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,42),wf(:,-3),wf(:,105),G0tensor(:,112))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,42),wf(:,105),wf(:,-3),G0tensor(:,113))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,42),wf(:,-3),wf(:,105),G0tensor(:,114))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,42),wf(:,-2),wf(:,91),G0tensor(:,115))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,42),wf(:,91),wf(:,-2),G0tensor(:,116))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,42),wf(:,-2),wf(:,91),G0tensor(:,117))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,42),wf(:,-1),wf(:,62),G0tensor(:,118))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,42),wf(:,62),wf(:,-1),G0tensor(:,119))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,42),wf(:,-1),wf(:,62),G0tensor(:,120))
  call check_last_UV_W(l_switch,G0(:,:,:,42),Q(:,49),wf(:,56),Q(:,14),G1tensor(:,101))
  call check_last_UV_W(l_switch,G0(:,:,:,42),Q(:,49),wf(:,59),Q(:,14),G1tensor(:,102))
  call check_last_UV_W(l_switch,G0(:,:,:,42),Q(:,49),wf(:,60),Q(:,14),G1tensor(:,103))
  call check_last_UV_W(l_switch,G0(:,:,:,42),Q(:,49),wf(:,202),Q(:,14),G1tensor(:,104))
  call check_last_UV_W(l_switch,G0(:,:,:,42),Q(:,49),wf(:,214),Q(:,14),G1tensor(:,105))
  call check_last_UV_W(l_switch,G0(:,:,:,42),Q(:,49),wf(:,221),Q(:,14),G1tensor(:,106))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,109),wf(:,-5),G0(:,:,:,43))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,43),wf(:,-3),wf(:,105),G0tensor(:,121))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,43),wf(:,105),wf(:,-3),G0tensor(:,122))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,43),wf(:,-3),wf(:,105),G0tensor(:,123))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,43),wf(:,-2),wf(:,91),G0tensor(:,124))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,43),wf(:,91),wf(:,-2),G0tensor(:,125))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,43),wf(:,-2),wf(:,91),G0tensor(:,126))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,43),wf(:,-1),wf(:,62),G0tensor(:,127))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,43),wf(:,62),wf(:,-1),G0tensor(:,128))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,43),wf(:,-1),wf(:,62),G0tensor(:,129))
  call check_last_UV_W(l_switch,G0(:,:,:,43),Q(:,49),wf(:,56),Q(:,14),G1tensor(:,107))
  call check_last_UV_W(l_switch,G0(:,:,:,43),Q(:,49),wf(:,59),Q(:,14),G1tensor(:,108))
  call check_last_UV_W(l_switch,G0(:,:,:,43),Q(:,49),wf(:,60),Q(:,14),G1tensor(:,109))
  call check_last_UV_W(l_switch,G0(:,:,:,43),Q(:,49),wf(:,202),Q(:,14),G1tensor(:,110))
  call check_last_UV_W(l_switch,G0(:,:,:,43),Q(:,49),wf(:,214),Q(:,14),G1tensor(:,111))
  call check_last_UV_W(l_switch,G0(:,:,:,43),Q(:,49),wf(:,221),Q(:,14),G1tensor(:,112))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,109),G0(:,:,:,44))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,44),wf(:,-3),wf(:,105),G0tensor(:,130))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,44),wf(:,105),wf(:,-3),G0tensor(:,131))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,44),wf(:,-3),wf(:,105),G0tensor(:,132))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,44),wf(:,-2),wf(:,91),G0tensor(:,133))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,44),wf(:,91),wf(:,-2),G0tensor(:,134))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,44),wf(:,-2),wf(:,91),G0tensor(:,135))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,44),wf(:,-1),wf(:,62),G0tensor(:,136))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,44),wf(:,62),wf(:,-1),G0tensor(:,137))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,44),wf(:,-1),wf(:,62),G0tensor(:,138))
  call check_last_UV_W(l_switch,G0(:,:,:,44),Q(:,49),wf(:,56),Q(:,14),G1tensor(:,113))
  call check_last_UV_W(l_switch,G0(:,:,:,44),Q(:,49),wf(:,59),Q(:,14),G1tensor(:,114))
  call check_last_UV_W(l_switch,G0(:,:,:,44),Q(:,49),wf(:,60),Q(:,14),G1tensor(:,115))
  call check_last_UV_W(l_switch,G0(:,:,:,44),Q(:,49),wf(:,202),Q(:,14),G1tensor(:,116))
  call check_last_UV_W(l_switch,G0(:,:,:,44),Q(:,49),wf(:,214),Q(:,14),G1tensor(:,117))
  call check_last_UV_W(l_switch,G0(:,:,:,44),Q(:,49),wf(:,221),Q(:,14),G1tensor(:,118))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,105),wf(:,113),G0(:,:,:,45))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,45),wf(:,-4),wf(:,-3),G0tensor(:,139))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,45),wf(:,-3),wf(:,-4),G0tensor(:,140))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,45),wf(:,-4),wf(:,-3),G0tensor(:,141))
  call check_last_UV_W(l_switch,G0(:,:,:,45),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,119))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,113),wf(:,105),G0(:,:,:,46))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,46),wf(:,-4),wf(:,-3),G0tensor(:,142))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,46),wf(:,-3),wf(:,-4),G0tensor(:,143))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,46),wf(:,-4),wf(:,-3),G0tensor(:,144))
  call check_last_UV_W(l_switch,G0(:,:,:,46),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,120))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,105),wf(:,113),G0(:,:,:,47))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-4),wf(:,-3),G0tensor(:,145))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-3),wf(:,-4),G0tensor(:,146))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,-4),wf(:,-3),G0tensor(:,147))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,121))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,2),wf(:,105),G0(:,:,:,48))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,122))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,5),wf(:,105),G0(:,:,:,49))
  call check_last_UV_W(l_switch,G0(:,:,:,49),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,123))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,6),wf(:,105),G0(:,:,:,50))
  call check_last_UV_W(l_switch,G0(:,:,:,50),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,124))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,105),wf(:,2),G0(:,:,:,51))
  call check_last_UV_W(l_switch,G0(:,:,:,51),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,125))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,105),wf(:,5),G0(:,:,:,52))
  call check_last_UV_W(l_switch,G0(:,:,:,52),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,126))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,105),wf(:,6),G0(:,:,:,53))
  call check_last_UV_W(l_switch,G0(:,:,:,53),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,127))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,2),wf(:,105),G0(:,:,:,54))
  call check_last_UV_W(l_switch,G0(:,:,:,54),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,128))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,5),wf(:,105),G0(:,:,:,55))
  call check_last_UV_W(l_switch,G0(:,:,:,55),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,129))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,6),wf(:,105),G0(:,:,:,56))
  call check_last_UV_W(l_switch,G0(:,:,:,56),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,130))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,117),G0(:,:,:,57))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,57),wf(:,-4),wf(:,-3),G0tensor(:,148))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,57),wf(:,-3),wf(:,-4),G0tensor(:,149))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,57),wf(:,-4),wf(:,-3),G0tensor(:,150))
  call check_last_UV_W(l_switch,G0(:,:,:,57),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,131))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,117),wf(:,-5),G0(:,:,:,58))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,58),wf(:,-4),wf(:,-3),G0tensor(:,151))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,58),wf(:,-3),wf(:,-4),G0tensor(:,152))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,58),wf(:,-4),wf(:,-3),G0tensor(:,153))
  call check_last_UV_W(l_switch,G0(:,:,:,58),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,132))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,117),G0(:,:,:,59))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,59),wf(:,-4),wf(:,-3),G0tensor(:,154))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,59),wf(:,-3),wf(:,-4),G0tensor(:,155))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,59),wf(:,-4),wf(:,-3),G0tensor(:,156))
  call check_last_UV_W(l_switch,G0(:,:,:,59),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,133))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,867),G0(:,:,:,60))
  call check_last_UV_W(l_switch,G0(:,:,:,60),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,134))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,869),G0(:,:,:,61))
  call check_last_UV_W(l_switch,G0(:,:,:,61),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,135))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,870),G0(:,:,:,62))
  call check_last_UV_W(l_switch,G0(:,:,:,62),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,136))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,867),wf(:,-5),G0(:,:,:,63))
  call check_last_UV_W(l_switch,G0(:,:,:,63),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,137))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,869),wf(:,-5),G0(:,:,:,64))
  call check_last_UV_W(l_switch,G0(:,:,:,64),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,138))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,870),wf(:,-5),G0(:,:,:,65))
  call check_last_UV_W(l_switch,G0(:,:,:,65),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,139))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,867),G0(:,:,:,66))
  call check_last_UV_W(l_switch,G0(:,:,:,66),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,140))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,869),G0(:,:,:,67))
  call check_last_UV_W(l_switch,G0(:,:,:,67),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,141))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,870),G0(:,:,:,68))
  call check_last_UV_W(l_switch,G0(:,:,:,68),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,142))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,113),G0(:,:,:,69))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,69),wf(:,-4),wf(:,105),G0tensor(:,157))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,69),wf(:,105),wf(:,-4),G0tensor(:,158))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,69),wf(:,-4),wf(:,105),G0tensor(:,159))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,69),wf(:,-2),wf(:,95),G0tensor(:,160))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,69),wf(:,95),wf(:,-2),G0tensor(:,161))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,69),wf(:,-2),wf(:,95),G0tensor(:,162))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,69),wf(:,-1),wf(:,66),G0tensor(:,163))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,69),wf(:,66),wf(:,-1),G0tensor(:,164))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,69),wf(:,-1),wf(:,66),G0tensor(:,165))
  call check_last_UV_W(l_switch,G0(:,:,:,69),Q(:,41),wf(:,50),Q(:,22),G1tensor(:,143))
  call check_last_UV_W(l_switch,G0(:,:,:,69),Q(:,41),wf(:,53),Q(:,22),G1tensor(:,144))
  call check_last_UV_W(l_switch,G0(:,:,:,69),Q(:,41),wf(:,54),Q(:,22),G1tensor(:,145))
  call check_last_UV_W(l_switch,G0(:,:,:,69),Q(:,41),wf(:,206),Q(:,22),G1tensor(:,146))
  call check_last_UV_W(l_switch,G0(:,:,:,69),Q(:,41),wf(:,225),Q(:,22),G1tensor(:,147))
  call check_last_UV_W(l_switch,G0(:,:,:,69),Q(:,41),wf(:,229),Q(:,22),G1tensor(:,148))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,113),wf(:,-3),G0(:,:,:,70))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,70),wf(:,-4),wf(:,105),G0tensor(:,166))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,70),wf(:,105),wf(:,-4),G0tensor(:,167))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,70),wf(:,-4),wf(:,105),G0tensor(:,168))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,70),wf(:,-2),wf(:,95),G0tensor(:,169))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,70),wf(:,95),wf(:,-2),G0tensor(:,170))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,70),wf(:,-2),wf(:,95),G0tensor(:,171))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,70),wf(:,-1),wf(:,66),G0tensor(:,172))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,70),wf(:,66),wf(:,-1),G0tensor(:,173))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,70),wf(:,-1),wf(:,66),G0tensor(:,174))
  call check_last_UV_W(l_switch,G0(:,:,:,70),Q(:,41),wf(:,50),Q(:,22),G1tensor(:,149))
  call check_last_UV_W(l_switch,G0(:,:,:,70),Q(:,41),wf(:,53),Q(:,22),G1tensor(:,150))
  call check_last_UV_W(l_switch,G0(:,:,:,70),Q(:,41),wf(:,54),Q(:,22),G1tensor(:,151))
  call check_last_UV_W(l_switch,G0(:,:,:,70),Q(:,41),wf(:,206),Q(:,22),G1tensor(:,152))
  call check_last_UV_W(l_switch,G0(:,:,:,70),Q(:,41),wf(:,225),Q(:,22),G1tensor(:,153))
  call check_last_UV_W(l_switch,G0(:,:,:,70),Q(:,41),wf(:,229),Q(:,22),G1tensor(:,154))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,113),G0(:,:,:,71))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,71),wf(:,-4),wf(:,105),G0tensor(:,175))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,71),wf(:,105),wf(:,-4),G0tensor(:,176))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,71),wf(:,-4),wf(:,105),G0tensor(:,177))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,71),wf(:,-2),wf(:,95),G0tensor(:,178))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,71),wf(:,95),wf(:,-2),G0tensor(:,179))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,71),wf(:,-2),wf(:,95),G0tensor(:,180))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,71),wf(:,-1),wf(:,66),G0tensor(:,181))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,71),wf(:,66),wf(:,-1),G0tensor(:,182))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,71),wf(:,-1),wf(:,66),G0tensor(:,183))
  call check_last_UV_W(l_switch,G0(:,:,:,71),Q(:,41),wf(:,50),Q(:,22),G1tensor(:,155))
  call check_last_UV_W(l_switch,G0(:,:,:,71),Q(:,41),wf(:,53),Q(:,22),G1tensor(:,156))
  call check_last_UV_W(l_switch,G0(:,:,:,71),Q(:,41),wf(:,54),Q(:,22),G1tensor(:,157))
  call check_last_UV_W(l_switch,G0(:,:,:,71),Q(:,41),wf(:,206),Q(:,22),G1tensor(:,158))
  call check_last_UV_W(l_switch,G0(:,:,:,71),Q(:,41),wf(:,225),Q(:,22),G1tensor(:,159))
  call check_last_UV_W(l_switch,G0(:,:,:,71),Q(:,41),wf(:,229),Q(:,22),G1tensor(:,160))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,113),G0(:,:,:,72))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,72),wf(:,-3),wf(:,105),G0tensor(:,184))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,72),wf(:,105),wf(:,-3),G0tensor(:,185))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,72),wf(:,-3),wf(:,105),G0tensor(:,186))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,72),wf(:,-2),wf(:,91),G0tensor(:,187))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,72),wf(:,91),wf(:,-2),G0tensor(:,188))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,72),wf(:,-2),wf(:,91),G0tensor(:,189))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,72),wf(:,-1),wf(:,62),G0tensor(:,190))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,72),wf(:,62),wf(:,-1),G0tensor(:,191))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,72),wf(:,-1),wf(:,62),G0tensor(:,192))
  call check_last_UV_W(l_switch,G0(:,:,:,72),Q(:,49),wf(:,56),Q(:,14),G1tensor(:,161))
  call check_last_UV_W(l_switch,G0(:,:,:,72),Q(:,49),wf(:,59),Q(:,14),G1tensor(:,162))
  call check_last_UV_W(l_switch,G0(:,:,:,72),Q(:,49),wf(:,60),Q(:,14),G1tensor(:,163))
  call check_last_UV_W(l_switch,G0(:,:,:,72),Q(:,49),wf(:,202),Q(:,14),G1tensor(:,164))
  call check_last_UV_W(l_switch,G0(:,:,:,72),Q(:,49),wf(:,214),Q(:,14),G1tensor(:,165))
  call check_last_UV_W(l_switch,G0(:,:,:,72),Q(:,49),wf(:,221),Q(:,14),G1tensor(:,166))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,113),wf(:,-4),G0(:,:,:,73))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,73),wf(:,-3),wf(:,105),G0tensor(:,193))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,73),wf(:,105),wf(:,-3),G0tensor(:,194))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,73),wf(:,-3),wf(:,105),G0tensor(:,195))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,73),wf(:,-2),wf(:,91),G0tensor(:,196))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,73),wf(:,91),wf(:,-2),G0tensor(:,197))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,73),wf(:,-2),wf(:,91),G0tensor(:,198))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,73),wf(:,-1),wf(:,62),G0tensor(:,199))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,73),wf(:,62),wf(:,-1),G0tensor(:,200))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,73),wf(:,-1),wf(:,62),G0tensor(:,201))
  call check_last_UV_W(l_switch,G0(:,:,:,73),Q(:,49),wf(:,56),Q(:,14),G1tensor(:,167))
  call check_last_UV_W(l_switch,G0(:,:,:,73),Q(:,49),wf(:,59),Q(:,14),G1tensor(:,168))
  call check_last_UV_W(l_switch,G0(:,:,:,73),Q(:,49),wf(:,60),Q(:,14),G1tensor(:,169))
  call check_last_UV_W(l_switch,G0(:,:,:,73),Q(:,49),wf(:,202),Q(:,14),G1tensor(:,170))
  call check_last_UV_W(l_switch,G0(:,:,:,73),Q(:,49),wf(:,214),Q(:,14),G1tensor(:,171))
  call check_last_UV_W(l_switch,G0(:,:,:,73),Q(:,49),wf(:,221),Q(:,14),G1tensor(:,172))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,113),G0(:,:,:,74))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,74),wf(:,-3),wf(:,105),G0tensor(:,202))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,74),wf(:,105),wf(:,-3),G0tensor(:,203))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,74),wf(:,-3),wf(:,105),G0tensor(:,204))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,74),wf(:,-2),wf(:,91),G0tensor(:,205))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,74),wf(:,91),wf(:,-2),G0tensor(:,206))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,74),wf(:,-2),wf(:,91),G0tensor(:,207))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,74),wf(:,-1),wf(:,62),G0tensor(:,208))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,74),wf(:,62),wf(:,-1),G0tensor(:,209))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,74),wf(:,-1),wf(:,62),G0tensor(:,210))
  call check_last_UV_W(l_switch,G0(:,:,:,74),Q(:,49),wf(:,56),Q(:,14),G1tensor(:,173))
  call check_last_UV_W(l_switch,G0(:,:,:,74),Q(:,49),wf(:,59),Q(:,14),G1tensor(:,174))
  call check_last_UV_W(l_switch,G0(:,:,:,74),Q(:,49),wf(:,60),Q(:,14),G1tensor(:,175))
  call check_last_UV_W(l_switch,G0(:,:,:,74),Q(:,49),wf(:,202),Q(:,14),G1tensor(:,176))
  call check_last_UV_W(l_switch,G0(:,:,:,74),Q(:,49),wf(:,214),Q(:,14),G1tensor(:,177))
  call check_last_UV_W(l_switch,G0(:,:,:,74),Q(:,49),wf(:,221),Q(:,14),G1tensor(:,178))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,863),G0(:,:,:,75))
  call check_last_UV_W(l_switch,G0(:,:,:,75),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,179))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,865),G0(:,:,:,76))
  call check_last_UV_W(l_switch,G0(:,:,:,76),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,180))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-4),wf(:,866),G0(:,:,:,77))
  call check_last_UV_W(l_switch,G0(:,:,:,77),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,181))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,863),wf(:,-4),G0(:,:,:,78))
  call check_last_UV_W(l_switch,G0(:,:,:,78),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,182))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,865),wf(:,-4),G0(:,:,:,79))
  call check_last_UV_W(l_switch,G0(:,:,:,79),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,183))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,866),wf(:,-4),G0(:,:,:,80))
  call check_last_UV_W(l_switch,G0(:,:,:,80),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,184))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,863),G0(:,:,:,81))
  call check_last_UV_W(l_switch,G0(:,:,:,81),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,185))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,865),G0(:,:,:,82))
  call check_last_UV_W(l_switch,G0(:,:,:,82),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,186))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,866),G0(:,:,:,83))
  call check_last_UV_W(l_switch,G0(:,:,:,83),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,187))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,856),G0(:,:,:,84))
  call check_last_UV_W(l_switch,G0(:,:,:,84),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,188))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,856),wf(:,-3),G0(:,:,:,85))
  call check_last_UV_W(l_switch,G0(:,:,:,85),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,189))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,856),G0(:,:,:,86))
  call check_last_UV_W(l_switch,G0(:,:,:,86),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,190))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,858),G0(:,:,:,87))
  call check_last_UV_W(l_switch,G0(:,:,:,87),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,191))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,858),wf(:,-3),G0(:,:,:,88))
  call check_last_UV_W(l_switch,G0(:,:,:,88),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,192))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,858),G0(:,:,:,89))
  call check_last_UV_W(l_switch,G0(:,:,:,89),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,193))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,859),G0(:,:,:,90))
  call check_last_UV_W(l_switch,G0(:,:,:,90),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,194))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,859),wf(:,-3),G0(:,:,:,91))
  call check_last_UV_W(l_switch,G0(:,:,:,91),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,195))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,859),G0(:,:,:,92))
  call check_last_UV_W(l_switch,G0(:,:,:,92),Q(:,62),wf(:,0),Q(:,1),G1tensor(:,196))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,99),wf(:,104),G0(:,:,:,93))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,93),wf(:,-4),wf(:,-2),G0tensor(:,211))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,93),wf(:,-2),wf(:,-4),G0tensor(:,212))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,93),wf(:,-4),wf(:,-2),G0tensor(:,213))
  call check_last_UV_W(l_switch,G0(:,:,:,93),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,197))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,104),wf(:,99),G0(:,:,:,94))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,94),wf(:,-4),wf(:,-2),G0tensor(:,214))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,94),wf(:,-2),wf(:,-4),G0tensor(:,215))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,94),wf(:,-4),wf(:,-2),G0tensor(:,216))
  call check_last_UV_W(l_switch,G0(:,:,:,94),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,198))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,99),wf(:,104),G0(:,:,:,95))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,95),wf(:,-4),wf(:,-2),G0tensor(:,217))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,95),wf(:,-2),wf(:,-4),G0tensor(:,218))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,95),wf(:,-4),wf(:,-2),G0tensor(:,219))
  call check_last_UV_W(l_switch,G0(:,:,:,95),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,199))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,8),wf(:,104),G0(:,:,:,96))
  call check_last_UV_W(l_switch,G0(:,:,:,96),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,200))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,11),wf(:,104),G0(:,:,:,97))
  call check_last_UV_W(l_switch,G0(:,:,:,97),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,201))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,12),wf(:,104),G0(:,:,:,98))
  call check_last_UV_W(l_switch,G0(:,:,:,98),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,202))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,104),wf(:,8),G0(:,:,:,99))
  call check_last_UV_W(l_switch,G0(:,:,:,99),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,203))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,104),wf(:,11),G0(:,:,:,100))
  call check_last_UV_W(l_switch,G0(:,:,:,100),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,204))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,104),wf(:,12),G0(:,:,:,101))
  call check_last_UV_W(l_switch,G0(:,:,:,101),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,205))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,8),wf(:,104),G0(:,:,:,102))
  call check_last_UV_W(l_switch,G0(:,:,:,102),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,206))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,11),wf(:,104),G0(:,:,:,103))
  call check_last_UV_W(l_switch,G0(:,:,:,103),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,207))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,12),wf(:,104),G0(:,:,:,104))
  call check_last_UV_W(l_switch,G0(:,:,:,104),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,208))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,124),G0(:,:,:,105))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,105),wf(:,-4),wf(:,-2),G0tensor(:,220))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,105),wf(:,-2),wf(:,-4),G0tensor(:,221))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,105),wf(:,-4),wf(:,-2),G0tensor(:,222))
  call check_last_UV_W(l_switch,G0(:,:,:,105),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,209))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,124),wf(:,-5),G0(:,:,:,106))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,106),wf(:,-4),wf(:,-2),G0tensor(:,223))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,106),wf(:,-2),wf(:,-4),G0tensor(:,224))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,106),wf(:,-4),wf(:,-2),G0tensor(:,225))
  call check_last_UV_W(l_switch,G0(:,:,:,106),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,210))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,124),G0(:,:,:,107))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,107),wf(:,-4),wf(:,-2),G0tensor(:,226))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,107),wf(:,-2),wf(:,-4),G0tensor(:,227))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,107),wf(:,-4),wf(:,-2),G0tensor(:,228))
  call check_last_UV_W(l_switch,G0(:,:,:,107),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,211))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,121),G0(:,:,:,108))
  call check_last_UV_W(l_switch,G0(:,:,:,108),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,212))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,122),G0(:,:,:,109))
  call check_last_UV_W(l_switch,G0(:,:,:,109),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,213))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,123),G0(:,:,:,110))
  call check_last_UV_W(l_switch,G0(:,:,:,110),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,214))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,121),wf(:,-5),G0(:,:,:,111))
  call check_last_UV_W(l_switch,G0(:,:,:,111),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,215))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,122),wf(:,-5),G0(:,:,:,112))
  call check_last_UV_W(l_switch,G0(:,:,:,112),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,216))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,123),wf(:,-5),G0(:,:,:,113))
  call check_last_UV_W(l_switch,G0(:,:,:,113),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,217))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,121),G0(:,:,:,114))
  call check_last_UV_W(l_switch,G0(:,:,:,114),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,218))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,122),G0(:,:,:,115))
  call check_last_UV_W(l_switch,G0(:,:,:,115),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,219))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,123),G0(:,:,:,116))
  call check_last_UV_W(l_switch,G0(:,:,:,116),Q(:,61),wf(:,-1),Q(:,2),G1tensor(:,220))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,99),G0(:,:,:,117))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,117),wf(:,-4),wf(:,104),G0tensor(:,229))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,117),wf(:,104),wf(:,-4),G0tensor(:,230))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,117),wf(:,-4),wf(:,104),G0tensor(:,231))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,117),wf(:,-3),wf(:,109),G0tensor(:,232))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,117),wf(:,109),wf(:,-3),G0tensor(:,233))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,117),wf(:,-3),wf(:,109),G0tensor(:,234))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,117),wf(:,0),wf(:,75),G0tensor(:,235))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,117),wf(:,75),wf(:,0),G0tensor(:,236))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,117),wf(:,0),wf(:,75),G0tensor(:,237))
  call check_last_UV_W(l_switch,G0(:,:,:,117),Q(:,38),wf(:,43),Q(:,25),G1tensor(:,221))
  call check_last_UV_W(l_switch,G0(:,:,:,117),Q(:,38),wf(:,45),Q(:,25),G1tensor(:,222))
  call check_last_UV_W(l_switch,G0(:,:,:,117),Q(:,38),wf(:,46),Q(:,25),G1tensor(:,223))
  call check_last_UV_W(l_switch,G0(:,:,:,117),Q(:,38),wf(:,181),Q(:,25),G1tensor(:,224))
  call check_last_UV_W(l_switch,G0(:,:,:,117),Q(:,38),wf(:,186),Q(:,25),G1tensor(:,225))
  call check_last_UV_W(l_switch,G0(:,:,:,117),Q(:,38),wf(:,190),Q(:,25),G1tensor(:,226))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(2)*(M(140)-M(146)-M(152)+M(154)-M(155)+M(157)+M(159)-M(160)+M(164)-M(170)-M(176)+M(178)-M(195)+M(219)+M(243) &
    -M(249))) * den(176)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,1)
  Gcoeff = (c(2)*(-M(140)+M(146)+M(152)-M(154)+M(155)-M(157)-M(159)+M(160)-M(164)+M(170)+M(176)-M(178)+M(195)-M(219)-M(243) &
    +M(249))) * den(176)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,2)
  Gcoeff = (c(3)*(-M(140)+M(146)+M(152)-M(154)+M(155)-M(157)-M(159)+M(160)-M(164)+M(170)+M(176)-M(178)+M(195)-M(219)-M(243) &
    +M(249))) * den(176)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,3)
  Gcoeff = (c(2)*(-M(154)+M(160)-M(162)+M(163)-M(164)+M(165)-M(168)+M(171)-M(174)+M(184)-M(192)+M(195)-M(198)+M(208)-M(222) &
    +M(232))) * den(381)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,2)
  Gcoeff = (c(2)*(-M(152)+M(154)+M(159)-M(160)-M(163)+M(164)+M(169)-M(170)-M(173)+M(174)+M(192)-M(195)-M(216)+M(219)+M(230) &
    -M(232))) * den(381)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,3)
  Gcoeff = (c(2)*(M(152)-M(159)+M(162)-M(165)+M(168)-M(169)+M(170)-M(171)+M(173)-M(184)+M(198)-M(208)+M(216)-M(219)+M(222) &
    -M(230))) * den(381)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,4)
  Gcoeff = (c(2)*(M(154)-M(160)+M(162)-M(163)+M(164)-M(165)+M(168)-M(171)+M(174)-M(184)+M(192)-M(195)+M(198)-M(208)+M(222) &
    -M(232))) * den(381)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,5)
  Gcoeff = (c(2)*(M(152)-M(154)-M(159)+M(160)+M(163)-M(164)-M(169)+M(170)+M(173)-M(174)-M(192)+M(195)+M(216)-M(219)-M(230) &
    +M(232))) * den(381)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,6)
  Gcoeff = (c(2)*(-M(152)+M(159)-M(162)+M(165)-M(168)+M(169)-M(170)+M(171)-M(173)+M(184)-M(198)+M(208)-M(216)+M(219)-M(222) &
    +M(230))) * den(381)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,7)
  Gcoeff = (c(3)*(M(154)-M(160)+M(162)-M(163)+M(164)-M(165)+M(168)-M(171)+M(174)-M(184)+M(192)-M(195)+M(198)-M(208)+M(222) &
    -M(232))) * den(381)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,8)
  Gcoeff = (c(3)*(M(152)-M(154)-M(159)+M(160)+M(163)-M(164)-M(169)+M(170)+M(173)-M(174)-M(192)+M(195)+M(216)-M(219)-M(230) &
    +M(232))) * den(381)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,9)
  Gcoeff = (c(3)*(-M(152)+M(159)-M(162)+M(165)-M(168)+M(169)-M(170)+M(171)-M(173)+M(184)-M(198)+M(208)-M(216)+M(219)-M(222) &
    +M(230))) * den(381)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,10)
  Gcoeff = (c(1)*(-M(11)+M(12)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(50)+M(55)-M(62)+M(89))+c(2)*(M(162)-M(165)+M(168)-M(171) &
    -M(184)+M(198)-M(208)+M(222))) * den(38)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,4)
  Gcoeff = (c(1)*(-M(11)+M(12)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(50)+M(55)-M(62)+M(89))+c(2)*(-M(154)+M(160)+M(163)-M(164) &
    -M(174)-M(192)+M(195)+M(232))) * den(38)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,5)
  Gcoeff = (c(2)*(-M(154)+M(160)-M(162)+M(163)-M(164)+M(165)-M(168)+M(171)-M(174)+M(184)-M(192)+M(195)-M(198)+M(208)-M(222) &
    +M(232))) * den(38)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,6)
  Gcoeff = (c(1)*(-M(11)+M(12)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(50)+M(55)-M(62)+M(89))+c(2)*(-M(154)+M(160)+M(163)-M(164) &
    -M(174)-M(192)+M(195)+M(232))) * den(38)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,13)
  Gcoeff = (c(1)*(-M(11)+M(12)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(50)+M(55)-M(62)+M(89))+c(2)*(M(162)-M(165)+M(168)-M(171) &
    -M(184)+M(198)-M(208)+M(222))) * den(38)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,14)
  Gcoeff = (c(2)*(M(154)-M(160)+M(162)-M(163)+M(164)-M(165)+M(168)-M(171)+M(174)-M(184)+M(192)-M(195)+M(198)-M(208)+M(222) &
    -M(232))) * den(38)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,15)
  Gcoeff = (c(2)*(-M(154)+M(160)-M(162)+M(163)-M(164)+M(165)-M(168)+M(171)-M(174)+M(184)-M(192)+M(195)-M(198)+M(208)-M(222) &
    +M(232))) * den(38)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,22)
  Gcoeff = (c(2)*(M(154)-M(160)+M(162)-M(163)+M(164)-M(165)+M(168)-M(171)+M(174)-M(184)+M(192)-M(195)+M(198)-M(208)+M(222) &
    -M(232))) * den(38)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,23)
  Gcoeff = (c(3)*(M(154)-M(160)+M(162)-M(163)+M(164)-M(165)+M(168)-M(171)+M(174)-M(184)+M(192)-M(195)+M(198)-M(208)+M(222) &
    -M(232))) * den(38)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,24)
  Gcoeff = (c(1)*(-M(11)+M(12)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(50)+M(55)-M(62)+M(89))+c(2)*(M(162)-M(165)+M(168)-M(171) &
    -M(184)+M(198)-M(208)+M(222))) * den(38)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,31)
  Gcoeff = (c(1)*(-M(11)+M(12)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(50)+M(55)-M(62)+M(89))+c(2)*(-M(152)+M(159)+M(169)-M(170) &
    -M(173)-M(216)+M(219)+M(230))) * den(38)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,40)
  Gcoeff = (c(2)*(-M(152)+M(159)-M(162)+M(165)-M(168)+M(169)-M(170)+M(171)-M(173)+M(184)-M(198)+M(208)-M(216)+M(219)-M(222) &
    +M(230))) * den(38)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,49)
  Gcoeff = (c(1)*(-M(11)+M(12)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(50)+M(55)-M(62)+M(89))+c(2)*(-M(152)+M(159)+M(169)-M(170) &
    -M(173)-M(216)+M(219)+M(230))) * den(38)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,32)
  Gcoeff = (c(1)*(-M(11)+M(12)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(50)+M(55)-M(62)+M(89))+c(2)*(M(162)-M(165)+M(168)-M(171) &
    -M(184)+M(198)-M(208)+M(222))) * den(38)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,41)
  Gcoeff = (c(2)*(M(152)-M(159)+M(162)-M(165)+M(168)-M(169)+M(170)-M(171)+M(173)-M(184)+M(198)-M(208)+M(216)-M(219)+M(222) &
    -M(230))) * den(38)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,50)
  Gcoeff = (c(2)*(-M(152)+M(159)-M(162)+M(165)-M(168)+M(169)-M(170)+M(171)-M(173)+M(184)-M(198)+M(208)-M(216)+M(219)-M(222) &
    +M(230))) * den(38)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,33)
  Gcoeff = (c(2)*(M(152)-M(159)+M(162)-M(165)+M(168)-M(169)+M(170)-M(171)+M(173)-M(184)+M(198)-M(208)+M(216)-M(219)+M(222) &
    -M(230))) * den(38)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,42)
  Gcoeff = (c(3)*(M(152)-M(159)+M(162)-M(165)+M(168)-M(169)+M(170)-M(171)+M(173)-M(184)+M(198)-M(208)+M(216)-M(219)+M(222) &
    -M(230))) * den(38)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,51)
  Gcoeff = (c(2)*(-M(148)+M(158)-M(161)+M(163)+M(165)-M(166)-M(168)-M(174)+M(177)+M(182)-M(192)-M(198)+M(201)+M(208)+M(232) &
    -M(246))) * den(376)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,47)
  Gcoeff = (c(2)*(-M(146)+M(148)+M(157)-M(158)-M(165)+M(166)-M(167)+M(168)+M(175)-M(176)+M(198)-M(201)+M(206)-M(208)-M(240) &
    +M(243))) * den(376)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,48)
  Gcoeff = (c(2)*(M(146)-M(157)+M(161)-M(163)+M(167)+M(174)-M(175)+M(176)-M(177)-M(182)+M(192)-M(206)-M(232)+M(240)-M(243) &
    +M(246))) * den(376)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,49)
  Gcoeff = (c(2)*(M(148)-M(158)+M(161)-M(163)-M(165)+M(166)+M(168)+M(174)-M(177)-M(182)+M(192)+M(198)-M(201)-M(208)-M(232) &
    +M(246))) * den(376)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,50)
  Gcoeff = (c(2)*(M(146)-M(148)-M(157)+M(158)+M(165)-M(166)+M(167)-M(168)-M(175)+M(176)-M(198)+M(201)-M(206)+M(208)+M(240) &
    -M(243))) * den(376)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,51)
  Gcoeff = (c(2)*(-M(146)+M(157)-M(161)+M(163)-M(167)-M(174)+M(175)-M(176)+M(177)+M(182)-M(192)+M(206)+M(232)-M(240)+M(243) &
    -M(246))) * den(376)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,52)
  Gcoeff = (c(3)*(M(148)-M(158)+M(161)-M(163)-M(165)+M(166)+M(168)+M(174)-M(177)-M(182)+M(192)+M(198)-M(201)-M(208)-M(232) &
    +M(246))) * den(376)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,53)
  Gcoeff = (c(3)*(M(146)-M(148)-M(157)+M(158)+M(165)-M(166)+M(167)-M(168)-M(175)+M(176)-M(198)+M(201)-M(206)+M(208)+M(240) &
    -M(243))) * den(376)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,54)
  Gcoeff = (c(3)*(-M(146)+M(157)-M(161)+M(163)-M(167)-M(174)+M(175)-M(176)+M(177)+M(182)-M(192)+M(206)+M(232)-M(240)+M(243) &
    -M(246))) * den(376)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,55)
  Gcoeff = (c(2)*(-M(142)+M(156)-M(162)-M(167)+M(169)+M(171)-M(172)-M(173)+M(175)+M(184)+M(206)-M(216)-M(222)+M(225)+M(230) &
    -M(240))) * den(367)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,56)
  Gcoeff = (c(2)*(M(142)-M(156)+M(162)+M(167)-M(169)-M(171)+M(172)+M(173)-M(175)-M(184)-M(206)+M(216)+M(222)-M(225)-M(230) &
    +M(240))) * den(367)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,57)
  Gcoeff = (c(3)*(M(142)-M(156)+M(162)+M(167)-M(169)-M(171)+M(172)+M(173)-M(175)-M(184)-M(206)+M(216)+M(222)-M(225)-M(230) &
    +M(240))) * den(367)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,58)
  Gcoeff = (c(2)*(-M(140)+M(142)+M(155)-M(156)-M(161)+M(162)-M(171)+M(172)+M(177)-M(178)+M(182)-M(184)+M(222)-M(225)-M(246) &
    +M(249))) * den(367)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,59)
  Gcoeff = (c(2)*(M(140)-M(142)-M(155)+M(156)+M(161)-M(162)+M(171)-M(172)-M(177)+M(178)-M(182)+M(184)-M(222)+M(225)+M(246) &
    -M(249))) * den(367)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,60)
  Gcoeff = (c(3)*(M(140)-M(142)-M(155)+M(156)+M(161)-M(162)+M(171)-M(172)-M(177)+M(178)-M(182)+M(184)-M(222)+M(225)+M(246) &
    -M(249))) * den(367)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,61)
  Gcoeff = (c(2)*(M(140)-M(155)+M(161)+M(167)-M(169)+M(173)-M(175)-M(177)+M(178)-M(182)-M(206)+M(216)-M(230)+M(240)+M(246) &
    -M(249))) * den(367)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,62)
  Gcoeff = (c(2)*(-M(140)+M(155)-M(161)-M(167)+M(169)-M(173)+M(175)+M(177)-M(178)+M(182)+M(206)-M(216)+M(230)-M(240)-M(246) &
    +M(249))) * den(367)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,63)
  Gcoeff = (c(3)*(-M(140)+M(155)-M(161)-M(167)+M(169)-M(173)+M(175)+M(177)-M(178)+M(182)+M(206)-M(216)+M(230)-M(240)-M(246) &
    +M(249))) * den(367)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,64)
  Gcoeff = (c(1)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(2)*(M(191)-M(193)+M(197)-M(199)-M(204) &
    +M(210)-M(228)+M(234))) * den(42)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,58)
  Gcoeff = (c(1)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(2)*(-M(136)+M(160)+M(180)-M(186)+M(195) &
    -M(196)-M(221)+M(223))) * den(42)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,59)
  Gcoeff = (c(2)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(42)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,60)
  Gcoeff = (c(1)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(2)*(-M(136)+M(160)+M(180)-M(186)+M(195) &
    -M(196)-M(221)+M(223))) * den(42)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,67)
  Gcoeff = (c(1)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(2)*(M(191)-M(193)+M(197)-M(199)-M(204) &
    +M(210)-M(228)+M(234))) * den(42)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,68)
  Gcoeff = (c(2)*(M(136)-M(160)-M(180)+M(186)+M(191)-M(193)-M(195)+M(196)+M(197)-M(199)-M(204)+M(210)+M(221)-M(223)-M(228) &
    +M(234))) * den(42)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,69)
  Gcoeff = (c(2)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(42)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,76)
  Gcoeff = (c(2)*(M(136)-M(160)-M(180)+M(186)+M(191)-M(193)-M(195)+M(196)+M(197)-M(199)-M(204)+M(210)+M(221)-M(223)-M(228) &
    +M(234))) * den(42)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,77)
  Gcoeff = (c(3)*(M(136)-M(160)-M(180)+M(186)+M(191)-M(193)-M(195)+M(196)+M(197)-M(199)-M(204)+M(210)+M(221)-M(223)-M(228) &
    +M(234))) * den(42)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,78)
  Gcoeff = (c(1)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(2)*(M(191)-M(193)+M(197)-M(199)-M(204) &
    +M(210)-M(228)+M(234))) * den(42)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,85)
  Gcoeff = (c(1)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(2)*(-M(134)+M(158)+M(179)-M(185)+M(201) &
    -M(202)-M(245)+M(247))) * den(42)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,86)
  Gcoeff = (c(2)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(42)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,87)
  Gcoeff = (c(1)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(2)*(-M(134)+M(158)+M(179)-M(185)+M(201) &
    -M(202)-M(245)+M(247))) * den(42)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,94)
  Gcoeff = (c(1)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(2)*(M(191)-M(193)+M(197)-M(199)-M(204) &
    +M(210)-M(228)+M(234))) * den(42)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,95)
  Gcoeff = (c(2)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(42)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,96)
  Gcoeff = (c(2)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(42)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,103)
  Gcoeff = (c(2)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(42)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,104)
  Gcoeff = (c(3)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(42)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,105)
  Gcoeff = (c(1)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(2)*(-M(180)+M(186)+M(215)-M(217)+M(221) &
    -M(223)-M(227)+M(233))) * den(44)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,61)
  Gcoeff = (c(1)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(2)*(-M(135)+M(159)-M(197)+M(199)+M(204) &
    -M(210)+M(219)-M(220))) * den(44)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,62)
  Gcoeff = (c(2)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(44)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,63)
  Gcoeff = (c(1)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(2)*(-M(135)+M(159)-M(197)+M(199)+M(204) &
    -M(210)+M(219)-M(220))) * den(44)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,70)
  Gcoeff = (c(1)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(2)*(-M(180)+M(186)+M(215)-M(217)+M(221) &
    -M(223)-M(227)+M(233))) * den(44)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,71)
  Gcoeff = (c(2)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(44)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,72)
  Gcoeff = (c(2)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(44)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,79)
  Gcoeff = (c(2)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(44)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,80)
  Gcoeff = (c(3)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(44)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,81)
  Gcoeff = (c(1)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(2)*(-M(180)+M(186)+M(215)-M(217)+M(221) &
    -M(223)-M(227)+M(233))) * den(44)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,112)
  Gcoeff = (c(1)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(2)*(-M(132)+M(156)+M(203)-M(209)+M(225) &
    -M(226)-M(239)+M(241))) * den(44)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,113)
  Gcoeff = (c(2)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(44)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,114)
  Gcoeff = (c(1)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(2)*(-M(132)+M(156)+M(203)-M(209)+M(225) &
    -M(226)-M(239)+M(241))) * den(44)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,121)
  Gcoeff = (c(1)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(2)*(-M(180)+M(186)+M(215)-M(217)+M(221) &
    -M(223)-M(227)+M(233))) * den(44)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,122)
  Gcoeff = (c(2)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(44)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,123)
  Gcoeff = (c(2)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(44)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,130)
  Gcoeff = (c(2)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(44)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,131)
  Gcoeff = (c(3)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(44)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,132)
  Gcoeff = (c(1)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(2)*(-M(131)+M(155)-M(215)+M(217)+M(227) &
    -M(233)+M(249)-M(250))) * den(46)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,139)
  Gcoeff = (c(1)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(2)*(-M(133)+M(157)-M(191)+M(193)+M(228) &
    -M(234)+M(243)-M(244))) * den(46)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,140)
  Gcoeff = (c(2)*(M(131)-M(133)-M(155)+M(157)-M(191)+M(193)+M(215)-M(217)-M(227)+M(228)+M(233)-M(234)+M(243)-M(244)-M(249) &
    +M(250))) * den(46)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,141)
  Gcoeff = (c(1)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(2)*(-M(133)+M(157)-M(191)+M(193)+M(228) &
    -M(234)+M(243)-M(244))) * den(46)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,142)
  Gcoeff = (c(1)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(2)*(-M(131)+M(155)-M(215)+M(217)+M(227) &
    -M(233)+M(249)-M(250))) * den(46)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,143)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(155)-M(157)+M(191)-M(193)-M(215)+M(217)+M(227)-M(228)-M(233)+M(234)-M(243)+M(244)+M(249) &
    -M(250))) * den(46)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,144)
  Gcoeff = (c(2)*(M(131)-M(133)-M(155)+M(157)-M(191)+M(193)+M(215)-M(217)-M(227)+M(228)+M(233)-M(234)+M(243)-M(244)-M(249) &
    +M(250))) * den(46)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,145)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(155)-M(157)+M(191)-M(193)-M(215)+M(217)+M(227)-M(228)-M(233)+M(234)-M(243)+M(244)+M(249) &
    -M(250))) * den(46)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,146)
  Gcoeff = (c(3)*(-M(131)+M(133)+M(155)-M(157)+M(191)-M(193)-M(215)+M(217)+M(227)-M(228)-M(233)+M(234)-M(243)+M(244)+M(249) &
    -M(250))) * den(46)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,147)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(135)-M(136)+M(155)-M(157)-M(159)+M(160)+M(195)-M(196)-M(219)+M(220)-M(243)+M(244)+M(249) &
    -M(250))) * den(47)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,122)
  Gcoeff = (c(2)*(M(131)-M(132)-M(134)+M(136)-M(155)+M(156)+M(158)-M(160)-M(195)+M(196)+M(201)-M(202)+M(225)-M(226)-M(249) &
    +M(250))) * den(47)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,123)
  Gcoeff = (c(2)*(M(132)-M(133)+M(134)-M(135)-M(156)+M(157)-M(158)+M(159)-M(201)+M(202)+M(219)-M(220)-M(225)+M(226)+M(243) &
    -M(244))) * den(47)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,124)
  Gcoeff = (c(2)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(47)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,125)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(134)-M(136)+M(155)-M(156)-M(158)+M(160)+M(195)-M(196)-M(201)+M(202)-M(225)+M(226)+M(249) &
    -M(250))) * den(47)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,126)
  Gcoeff = (c(2)*(-M(132)+M(133)-M(134)+M(135)+M(156)-M(157)+M(158)-M(159)+M(201)-M(202)-M(219)+M(220)+M(225)-M(226)-M(243) &
    +M(244))) * den(47)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,127)
  Gcoeff = (c(3)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(47)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,128)
  Gcoeff = (c(3)*(-M(131)+M(132)+M(134)-M(136)+M(155)-M(156)-M(158)+M(160)+M(195)-M(196)-M(201)+M(202)-M(225)+M(226)+M(249) &
    -M(250))) * den(47)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,129)
  Gcoeff = (c(3)*(-M(132)+M(133)-M(134)+M(135)+M(156)-M(157)+M(158)-M(159)+M(201)-M(202)-M(219)+M(220)+M(225)-M(226)-M(243) &
    +M(244))) * den(47)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,130)
  Gcoeff = (c(1)*(M(41)+M(42)+M(43)-M(53)-M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92))+c(2)*(M(133)+M(135)-M(157)-M(159) &
    -M(219)+M(220)-M(243)+M(244))) * den(200)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,148)
  Gcoeff = (c(1)*(M(41)+M(42)+M(43)-M(53)-M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92))+c(2)*(M(131)+M(136)-M(155)-M(160) &
    -M(195)+M(196)-M(249)+M(250))) * den(200)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,149)
  Gcoeff = (c(2)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(200)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,150)
  Gcoeff = (c(1)*(M(41)+M(42)+M(43)-M(53)-M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92))+c(2)*(M(131)+M(136)-M(155)-M(160) &
    -M(195)+M(196)-M(249)+M(250))) * den(200)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,151)
  Gcoeff = (c(1)*(M(41)+M(42)+M(43)-M(53)-M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92))+c(2)*(M(133)+M(135)-M(157)-M(159) &
    -M(219)+M(220)-M(243)+M(244))) * den(200)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,152)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(135)-M(136)+M(155)-M(157)-M(159)+M(160)+M(195)-M(196)-M(219)+M(220)-M(243)+M(244)+M(249) &
    -M(250))) * den(200)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,153)
  Gcoeff = (c(2)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(200)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,154)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(135)-M(136)+M(155)-M(157)-M(159)+M(160)+M(195)-M(196)-M(219)+M(220)-M(243)+M(244)+M(249) &
    -M(250))) * den(200)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,155)
  Gcoeff = (c(3)*(-M(131)+M(133)+M(135)-M(136)+M(155)-M(157)-M(159)+M(160)+M(195)-M(196)-M(219)+M(220)-M(243)+M(244)+M(249) &
    -M(250))) * den(200)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,156)
  Gcoeff = (c(2)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(408)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,134)
  Gcoeff = (c(2)*(M(131)-M(133)-M(155)+M(157)-M(191)+M(193)+M(215)-M(217)-M(227)+M(228)+M(233)-M(234)+M(243)-M(244)-M(249) &
    +M(250))) * den(408)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,135)
  Gcoeff = (c(2)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(408)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,136)
  Gcoeff = (c(2)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(408)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,137)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(155)-M(157)+M(191)-M(193)-M(215)+M(217)+M(227)-M(228)-M(233)+M(234)-M(243)+M(244)+M(249) &
    -M(250))) * den(408)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,138)
  Gcoeff = (c(2)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(408)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,139)
  Gcoeff = (c(3)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(408)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,140)
  Gcoeff = (c(3)*(-M(131)+M(133)+M(155)-M(157)+M(191)-M(193)-M(215)+M(217)+M(227)-M(228)-M(233)+M(234)-M(243)+M(244)+M(249) &
    -M(250))) * den(408)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,141)
  Gcoeff = (c(3)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(408)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,142)
  Gcoeff = (c(1)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(2)*(-M(179)+M(185)-M(203)+M(209)+M(239) &
    -M(241)+M(245)-M(247))) * den(46)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,157)
  Gcoeff = (c(1)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(2)*(-M(133)+M(157)-M(191)+M(193)+M(228) &
    -M(234)+M(243)-M(244))) * den(46)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,166)
  Gcoeff = (c(2)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(46)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,175)
  Gcoeff = (c(1)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(2)*(-M(133)+M(157)-M(191)+M(193)+M(228) &
    -M(234)+M(243)-M(244))) * den(46)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,158)
  Gcoeff = (c(1)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(2)*(-M(179)+M(185)-M(203)+M(209)+M(239) &
    -M(241)+M(245)-M(247))) * den(46)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,167)
  Gcoeff = (c(2)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(46)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,176)
  Gcoeff = (c(2)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(46)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,159)
  Gcoeff = (c(2)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(46)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,168)
  Gcoeff = (c(3)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(46)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,177)
  Gcoeff = (c(1)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(2)*(-M(179)+M(185)-M(203)+M(209)+M(239) &
    -M(241)+M(245)-M(247))) * den(46)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,184)
  Gcoeff = (c(1)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(2)*(-M(131)+M(155)-M(215)+M(217)+M(227) &
    -M(233)+M(249)-M(250))) * den(46)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,185)
  Gcoeff = (c(2)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(46)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,186)
  Gcoeff = (c(1)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(2)*(-M(131)+M(155)-M(215)+M(217)+M(227) &
    -M(233)+M(249)-M(250))) * den(46)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,193)
  Gcoeff = (c(1)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(2)*(-M(179)+M(185)-M(203)+M(209)+M(239) &
    -M(241)+M(245)-M(247))) * den(46)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,194)
  Gcoeff = (c(2)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(46)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,195)
  Gcoeff = (c(2)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(46)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,202)
  Gcoeff = (c(2)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(46)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,203)
  Gcoeff = (c(3)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(46)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,204)
  Gcoeff = (c(2)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(403)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,179)
  Gcoeff = (c(2)*(M(132)-M(135)-M(156)+M(159)-M(197)+M(199)-M(203)+M(204)+M(209)-M(210)+M(219)-M(220)-M(225)+M(226)+M(239) &
    -M(241))) * den(403)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,180)
  Gcoeff = (c(2)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(403)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,181)
  Gcoeff = (c(2)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(403)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,182)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(156)-M(159)+M(197)-M(199)+M(203)-M(204)-M(209)+M(210)-M(219)+M(220)+M(225)-M(226)-M(239) &
    +M(241))) * den(403)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,183)
  Gcoeff = (c(2)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(403)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,184)
  Gcoeff = (c(3)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(403)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,185)
  Gcoeff = (c(3)*(-M(132)+M(135)+M(156)-M(159)+M(197)-M(199)+M(203)-M(204)-M(209)+M(210)-M(219)+M(220)+M(225)-M(226)-M(239) &
    +M(241))) * den(403)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,186)
  Gcoeff = (c(3)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(403)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,187)
  Gcoeff = (c(2)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(393)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,188)
  Gcoeff = (c(2)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(393)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,189)
  Gcoeff = (c(3)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(393)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,190)
  Gcoeff = (c(2)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(393)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,191)
  Gcoeff = (c(2)*(-M(134)+M(136)+M(158)-M(160)+M(179)-M(180)-M(185)+M(186)-M(195)+M(196)+M(201)-M(202)+M(221)-M(223)-M(245) &
    +M(247))) * den(393)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,192)
  Gcoeff = (c(3)*(-M(134)+M(136)+M(158)-M(160)+M(179)-M(180)-M(185)+M(186)-M(195)+M(196)+M(201)-M(202)+M(221)-M(223)-M(245) &
    +M(247))) * den(393)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,193)
  Gcoeff = (c(2)*(M(136)-M(160)-M(180)+M(186)+M(191)-M(193)-M(195)+M(196)+M(197)-M(199)-M(204)+M(210)+M(221)-M(223)-M(228) &
    +M(234))) * den(393)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,194)
  Gcoeff = (c(2)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(393)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,195)
  Gcoeff = (c(3)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(393)
  T1sum(1:5,6) = T1sum(1:5,6) + Gcoeff * G1tensor(:,196)
  Gcoeff = (c(1)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(2)*(-M(158)+M(168)+M(185)-M(187) &
    +M(198)-M(201)-M(231)+M(245))) * den(48)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,88)
  Gcoeff = (c(1)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(2)*(-M(144)+M(181)-M(191)+M(199) &
    -M(200)+M(204)-M(234)+M(237))) * den(48)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,89)
  Gcoeff = (c(2)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(48)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,90)
  Gcoeff = (c(1)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(2)*(-M(144)+M(181)-M(191)+M(199) &
    -M(200)+M(204)-M(234)+M(237))) * den(48)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,97)
  Gcoeff = (c(1)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(2)*(-M(158)+M(168)+M(185)-M(187) &
    +M(198)-M(201)-M(231)+M(245))) * den(48)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,98)
  Gcoeff = (c(2)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(48)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,99)
  Gcoeff = (c(2)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(48)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,106)
  Gcoeff = (c(2)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(48)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,107)
  Gcoeff = (c(3)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(48)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,108)
  Gcoeff = (c(1)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(2)*(-M(150)+M(183)+M(193)-M(194) &
    -M(197)-M(210)+M(213)+M(228))) * den(49)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,211)
  Gcoeff = (c(1)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(2)*(-M(153)-M(168)+M(171)+M(184) &
    +M(187)-M(188)-M(198)+M(231))) * den(49)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,212)
  Gcoeff = (c(2)*(M(150)-M(153)-M(168)+M(171)-M(183)+M(184)+M(187)-M(188)-M(193)+M(194)+M(197)-M(198)+M(210)-M(213)-M(228) &
    +M(231))) * den(49)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,213)
  Gcoeff = (c(1)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(2)*(-M(153)-M(168)+M(171)+M(184) &
    +M(187)-M(188)-M(198)+M(231))) * den(49)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,214)
  Gcoeff = (c(1)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(2)*(-M(150)+M(183)+M(193)-M(194) &
    -M(197)-M(210)+M(213)+M(228))) * den(49)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,215)
  Gcoeff = (c(2)*(-M(150)+M(153)+M(168)-M(171)+M(183)-M(184)-M(187)+M(188)+M(193)-M(194)-M(197)+M(198)-M(210)+M(213)+M(228) &
    -M(231))) * den(49)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,216)
  Gcoeff = (c(2)*(M(150)-M(153)-M(168)+M(171)-M(183)+M(184)+M(187)-M(188)-M(193)+M(194)+M(197)-M(198)+M(210)-M(213)-M(228) &
    +M(231))) * den(49)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,217)
  Gcoeff = (c(2)*(-M(150)+M(153)+M(168)-M(171)+M(183)-M(184)-M(187)+M(188)+M(193)-M(194)-M(197)+M(198)-M(210)+M(213)+M(228) &
    -M(231))) * den(49)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,218)
  Gcoeff = (c(3)*(-M(150)+M(153)+M(168)-M(171)+M(183)-M(184)-M(187)+M(188)+M(193)-M(194)-M(197)+M(198)-M(210)+M(213)+M(228) &
    -M(231))) * den(49)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,219)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(50)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,200)
  Gcoeff = (c(2)*(M(134)-M(136)-M(147)+M(153)-M(171)+M(177)-M(179)+M(180)+M(182)-M(184)+M(188)-M(190)-M(196)+M(202)+M(223) &
    -M(247))) * den(50)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,201)
  Gcoeff = (c(2)*(M(136)-M(144)+M(147)-M(150)-M(177)-M(180)+M(181)-M(182)+M(183)+M(190)-M(194)+M(196)-M(200)+M(213)-M(223) &
    +M(237))) * den(50)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,202)
  Gcoeff = (c(2)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(50)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,203)
  Gcoeff = (c(2)*(-M(134)+M(136)+M(147)-M(153)+M(171)-M(177)+M(179)-M(180)-M(182)+M(184)-M(188)+M(190)+M(196)-M(202)-M(223) &
    +M(247))) * den(50)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,204)
  Gcoeff = (c(2)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(50)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,205)
  Gcoeff = (c(3)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(50)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,206)
  Gcoeff = (c(3)*(-M(134)+M(136)+M(147)-M(153)+M(171)-M(177)+M(179)-M(180)-M(182)+M(184)-M(188)+M(190)+M(196)-M(202)-M(223) &
    +M(247))) * den(50)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,207)
  Gcoeff = (c(3)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(50)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,208)
  Gcoeff = (c(1)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(2)*(M(144)+M(150)-M(181)-M(183) &
    +M(194)+M(200)-M(213)-M(237))) * den(219)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,220)
  Gcoeff = (c(1)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(2)*(M(134)+M(153)-M(171)-M(179) &
    -M(184)+M(188)+M(202)-M(247))) * den(219)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,221)
  Gcoeff = (c(2)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(219)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,222)
  Gcoeff = (c(1)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(2)*(M(134)+M(153)-M(171)-M(179) &
    -M(184)+M(188)+M(202)-M(247))) * den(219)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,223)
  Gcoeff = (c(1)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(2)*(M(144)+M(150)-M(181)-M(183) &
    +M(194)+M(200)-M(213)-M(237))) * den(219)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,224)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(219)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,225)
  Gcoeff = (c(2)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(219)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,226)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(219)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,227)
  Gcoeff = (c(3)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(219)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,228)
  Gcoeff = (c(2)*(M(153)-M(160)+M(168)-M(171)+M(174)-M(184)+M(186)-M(187)+M(188)-M(189)+M(192)-M(195)+M(198)-M(207)+M(221) &
    -M(231))) * den(427)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,212)
  Gcoeff = (c(2)*(-M(150)+M(160)-M(174)+M(183)-M(186)+M(189)-M(192)+M(193)-M(194)+M(195)-M(197)+M(207)-M(210)+M(213)-M(221) &
    +M(228))) * den(427)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,213)
  Gcoeff = (c(2)*(M(150)-M(153)-M(168)+M(171)-M(183)+M(184)+M(187)-M(188)-M(193)+M(194)+M(197)-M(198)+M(210)-M(213)-M(228) &
    +M(231))) * den(427)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,214)
  Gcoeff = (c(2)*(-M(153)+M(160)-M(168)+M(171)-M(174)+M(184)-M(186)+M(187)-M(188)+M(189)-M(192)+M(195)-M(198)+M(207)-M(221) &
    +M(231))) * den(427)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,215)
  Gcoeff = (c(2)*(M(150)-M(160)+M(174)-M(183)+M(186)-M(189)+M(192)-M(193)+M(194)-M(195)+M(197)-M(207)+M(210)-M(213)+M(221) &
    -M(228))) * den(427)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,216)
  Gcoeff = (c(2)*(-M(150)+M(153)+M(168)-M(171)+M(183)-M(184)-M(187)+M(188)+M(193)-M(194)-M(197)+M(198)-M(210)+M(213)+M(228) &
    -M(231))) * den(427)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,217)
  Gcoeff = (c(3)*(-M(153)+M(160)-M(168)+M(171)-M(174)+M(184)-M(186)+M(187)-M(188)+M(189)-M(192)+M(195)-M(198)+M(207)-M(221) &
    +M(231))) * den(427)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,218)
  Gcoeff = (c(3)*(M(150)-M(160)+M(174)-M(183)+M(186)-M(189)+M(192)-M(193)+M(194)-M(195)+M(197)-M(207)+M(210)-M(213)+M(221) &
    -M(228))) * den(427)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,219)
  Gcoeff = (c(3)*(-M(150)+M(153)+M(168)-M(171)+M(183)-M(184)-M(187)+M(188)+M(193)-M(194)-M(197)+M(198)-M(210)+M(213)+M(228) &
    -M(231))) * den(427)
  T1sum(1:5,5) = T1sum(1:5,5) + Gcoeff * G1tensor(:,220)
  Gcoeff = (c(1)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(2)*(-M(160)+M(174)+M(186)-M(189) &
    +M(192)-M(195)-M(207)+M(221))) * den(49)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,7)
  Gcoeff = (c(1)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(2)*(-M(153)-M(168)+M(171)+M(184) &
    +M(187)-M(188)-M(198)+M(231))) * den(49)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,8)
  Gcoeff = (c(2)*(-M(153)+M(160)-M(168)+M(171)-M(174)+M(184)-M(186)+M(187)-M(188)+M(189)-M(192)+M(195)-M(198)+M(207)-M(221) &
    +M(231))) * den(49)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,9)
  Gcoeff = (c(1)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(2)*(-M(153)-M(168)+M(171)+M(184) &
    +M(187)-M(188)-M(198)+M(231))) * den(49)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,16)
  Gcoeff = (c(1)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(2)*(-M(160)+M(174)+M(186)-M(189) &
    +M(192)-M(195)-M(207)+M(221))) * den(49)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,17)
  Gcoeff = (c(2)*(M(153)-M(160)+M(168)-M(171)+M(174)-M(184)+M(186)-M(187)+M(188)-M(189)+M(192)-M(195)+M(198)-M(207)+M(221) &
    -M(231))) * den(49)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,18)
  Gcoeff = (c(2)*(-M(153)+M(160)-M(168)+M(171)-M(174)+M(184)-M(186)+M(187)-M(188)+M(189)-M(192)+M(195)-M(198)+M(207)-M(221) &
    +M(231))) * den(49)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,25)
  Gcoeff = (c(2)*(M(153)-M(160)+M(168)-M(171)+M(174)-M(184)+M(186)-M(187)+M(188)-M(189)+M(192)-M(195)+M(198)-M(207)+M(221) &
    -M(231))) * den(49)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,26)
  Gcoeff = (c(3)*(M(153)-M(160)+M(168)-M(171)+M(174)-M(184)+M(186)-M(187)+M(188)-M(189)+M(192)-M(195)+M(198)-M(207)+M(221) &
    -M(231))) * den(49)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,27)
  Gcoeff = (c(1)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(2)*(-M(160)+M(174)+M(186)-M(189) &
    +M(192)-M(195)-M(207)+M(221))) * den(49)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,229)
  Gcoeff = (c(1)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(2)*(-M(150)+M(183)+M(193)-M(194) &
    -M(197)-M(210)+M(213)+M(228))) * den(49)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,230)
  Gcoeff = (c(2)*(-M(150)+M(160)-M(174)+M(183)-M(186)+M(189)-M(192)+M(193)-M(194)+M(195)-M(197)+M(207)-M(210)+M(213)-M(221) &
    +M(228))) * den(49)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,231)
  Gcoeff = (c(1)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(2)*(-M(156)+M(162)+M(209)-M(211)+M(222) &
    -M(225)-M(229)+M(239))) * den(51)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,115)
  Gcoeff = (c(1)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(2)*(-M(138)+M(180)+M(205)-M(215)+M(223) &
    -M(224)-M(233)+M(235))) * den(51)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,116)
  Gcoeff = (c(2)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(51)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,117)
  Gcoeff = (c(1)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(2)*(-M(138)+M(180)+M(205)-M(215)+M(223) &
    -M(224)-M(233)+M(235))) * den(51)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,124)
  Gcoeff = (c(1)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(2)*(-M(156)+M(162)+M(209)-M(211)+M(222) &
    -M(225)-M(229)+M(239))) * den(51)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,125)
  Gcoeff = (c(2)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(51)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,126)
  Gcoeff = (c(2)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(51)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,133)
  Gcoeff = (c(2)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(51)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,134)
  Gcoeff = (c(3)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(51)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,135)
  Gcoeff = (c(1)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(2)*(-M(155)+M(161)-M(205)+M(215)+M(233) &
    -M(235)+M(246)-M(249))) * den(52)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,187)
  Gcoeff = (c(1)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(2)*(-M(137)+M(179)-M(209)+M(211)+M(229) &
    -M(239)+M(247)-M(248))) * den(52)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,188)
  Gcoeff = (c(2)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(52)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,189)
  Gcoeff = (c(1)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(2)*(-M(137)+M(179)-M(209)+M(211)+M(229) &
    -M(239)+M(247)-M(248))) * den(52)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,196)
  Gcoeff = (c(1)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(2)*(-M(155)+M(161)-M(205)+M(215)+M(233) &
    -M(235)+M(246)-M(249))) * den(52)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,197)
  Gcoeff = (c(2)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(52)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,198)
  Gcoeff = (c(2)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(52)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,205)
  Gcoeff = (c(2)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(52)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,206)
  Gcoeff = (c(3)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(52)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,207)
  Gcoeff = (c(1)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(2)*(-M(159)+M(173)-M(183)+M(197)+M(210) &
    -M(213)+M(216)-M(219))) * den(54)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,34)
  Gcoeff = (c(1)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(2)*(-M(151)-M(162)+M(165)+M(208)+M(211) &
    -M(212)-M(222)+M(229))) * den(54)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,35)
  Gcoeff = (c(2)*(-M(151)+M(159)-M(162)+M(165)-M(173)+M(183)-M(197)+M(208)-M(210)+M(211)-M(212)+M(213)-M(216)+M(219)-M(222) &
    +M(229))) * den(54)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,36)
  Gcoeff = (c(1)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(2)*(-M(151)-M(162)+M(165)+M(208)+M(211) &
    -M(212)-M(222)+M(229))) * den(54)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,43)
  Gcoeff = (c(1)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(2)*(-M(159)+M(173)-M(183)+M(197)+M(210) &
    -M(213)+M(216)-M(219))) * den(54)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,44)
  Gcoeff = (c(2)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(54)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,45)
  Gcoeff = (c(2)*(-M(151)+M(159)-M(162)+M(165)-M(173)+M(183)-M(197)+M(208)-M(210)+M(211)-M(212)+M(213)-M(216)+M(219)-M(222) &
    +M(229))) * den(54)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,52)
  Gcoeff = (c(2)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(54)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,53)
  Gcoeff = (c(3)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(54)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,54)
  Gcoeff = (c(1)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(2)*(-M(159)+M(173)-M(183)+M(197)+M(210) &
    -M(213)+M(216)-M(219))) * den(54)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,232)
  Gcoeff = (c(1)*(M(5)-M(6)+M(11)-M(12)-M(19)+M(20)-M(21)+M(22)-M(52)+M(79)-M(86)+M(91))+c(2)*(-M(149)-M(186)+M(189)+M(207)+M(217) &
    -M(218)-M(221)+M(227))) * den(54)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,233)
  Gcoeff = (c(2)*(-M(149)+M(159)-M(173)+M(183)-M(186)+M(189)-M(197)+M(207)-M(210)+M(213)-M(216)+M(217)-M(218)+M(219)-M(221) &
    +M(227))) * den(54)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,234)
  Gcoeff = (c(1)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(49)+M(79)+M(91)-M(98))+c(2)*(-M(157)+M(167)-M(181)+M(191)+M(234) &
    -M(237)+M(240)-M(243))) * den(56)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,160)
  Gcoeff = (c(1)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(49)+M(79)+M(91)-M(98))+c(2)*(-M(143)-M(185)+M(187)+M(203)+M(231) &
    +M(241)-M(242)-M(245))) * den(56)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,161)
  Gcoeff = (c(2)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(56)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,162)
  Gcoeff = (c(1)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(49)+M(79)+M(91)-M(98))+c(2)*(-M(143)-M(185)+M(187)+M(203)+M(231) &
    +M(241)-M(242)-M(245))) * den(56)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,169)
  Gcoeff = (c(1)*(M(3)-M(4)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(49)+M(79)+M(91)-M(98))+c(2)*(-M(157)+M(167)-M(181)+M(191)+M(234) &
    -M(237)+M(240)-M(243))) * den(56)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,170)
  Gcoeff = (c(2)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(56)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,171)
  Gcoeff = (c(2)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(56)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,178)
  Gcoeff = (c(2)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(56)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,179)
  Gcoeff = (c(3)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(56)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,180)
  Gcoeff = (c(1)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(2)*(-M(134)+M(144)+M(179)-M(181)+M(200) &
    -M(202)-M(237)+M(247))) * den(69)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,91)
  Gcoeff = (c(1)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(2)*(-M(168)+M(187)-M(193)+M(197)-M(198) &
    +M(210)-M(228)+M(231))) * den(69)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,92)
  Gcoeff = (c(2)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(69)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,93)
  Gcoeff = (c(1)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(2)*(-M(168)+M(187)-M(193)+M(197)-M(198) &
    +M(210)-M(228)+M(231))) * den(69)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,100)
  Gcoeff = (c(1)*(M(3)-M(4)-M(13)+M(14)-M(31)+M(32)+M(35)-M(36)-M(60)+M(69)-M(72)+M(81))+c(2)*(-M(134)+M(144)+M(179)-M(181)+M(200) &
    -M(202)-M(237)+M(247))) * den(69)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,101)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(69)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,102)
  Gcoeff = (c(2)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(69)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,109)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(69)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,110)
  Gcoeff = (c(3)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(69)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,111)
  Gcoeff = (c(1)*(M(1)-M(2)-M(7)+M(8)-M(27)+M(28)+M(33)-M(34)-M(57)+M(69)+M(81)-M(84))+c(2)*(-M(132)+M(138)+M(203)-M(205)+M(224) &
    -M(226)-M(235)+M(241))) * den(72)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,118)
  Gcoeff = (c(1)*(M(1)-M(2)-M(7)+M(8)-M(27)+M(28)+M(33)-M(34)-M(57)+M(69)+M(81)-M(84))+c(2)*(-M(162)+M(186)+M(211)-M(217)+M(221) &
    -M(222)-M(227)+M(229))) * den(72)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,119)
  Gcoeff = (c(2)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(72)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,120)
  Gcoeff = (c(1)*(M(1)-M(2)-M(7)+M(8)-M(27)+M(28)+M(33)-M(34)-M(57)+M(69)+M(81)-M(84))+c(2)*(-M(162)+M(186)+M(211)-M(217)+M(221) &
    -M(222)-M(227)+M(229))) * den(72)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,127)
  Gcoeff = (c(1)*(M(1)-M(2)-M(7)+M(8)-M(27)+M(28)+M(33)-M(34)-M(57)+M(69)+M(81)-M(84))+c(2)*(-M(132)+M(138)+M(203)-M(205)+M(224) &
    -M(226)-M(235)+M(241))) * den(72)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,128)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(72)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,129)
  Gcoeff = (c(2)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(72)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,136)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(72)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,137)
  Gcoeff = (c(3)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(72)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,138)
  Gcoeff = (c(1)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(2)*(-M(131)+M(137)-M(211)+M(217)+M(227) &
    -M(229)+M(248)-M(250))) * den(73)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,190)
  Gcoeff = (c(1)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(2)*(-M(161)+M(185)-M(203)+M(205)+M(235) &
    -M(241)+M(245)-M(246))) * den(73)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,191)
  Gcoeff = (c(2)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(73)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,192)
  Gcoeff = (c(1)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(2)*(-M(161)+M(185)-M(203)+M(205)+M(235) &
    -M(241)+M(245)-M(246))) * den(73)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,199)
  Gcoeff = (c(1)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(2)*(-M(131)+M(137)-M(211)+M(217)+M(227) &
    -M(229)+M(248)-M(250))) * den(73)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,200)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(73)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,201)
  Gcoeff = (c(2)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(73)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,208)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(73)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,209)
  Gcoeff = (c(3)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(73)
  T2sum(1:1,25) = T2sum(1:1,25) + Gcoeff * G0tensor(:,210)
  Gcoeff = (c(1)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(2)*(-M(133)+M(143)-M(187)+M(193) &
    +M(228)-M(231)+M(242)-M(244))) * den(77)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,163)
  Gcoeff = (c(1)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(2)*(-M(167)-M(179)+M(181)+M(209) &
    +M(237)+M(239)-M(240)-M(247))) * den(77)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,164)
  Gcoeff = (c(2)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(77)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,165)
  Gcoeff = (c(1)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(2)*(-M(167)-M(179)+M(181)+M(209) &
    +M(237)+M(239)-M(240)-M(247))) * den(77)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,172)
  Gcoeff = (c(1)*(-M(3)+M(4)+M(13)-M(14)+M(25)-M(26)-M(29)+M(30)-M(61)+M(82)+M(94)-M(99))+c(2)*(-M(133)+M(143)-M(187)+M(193) &
    +M(228)-M(231)+M(242)-M(244))) * den(77)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,173)
  Gcoeff = (c(2)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(77)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,174)
  Gcoeff = (c(2)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(77)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,181)
  Gcoeff = (c(2)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(77)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,182)
  Gcoeff = (c(3)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(77)
  T2sum(1:1,24) = T2sum(1:1,24) + Gcoeff * G0tensor(:,183)
  Gcoeff = (c(2)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(85)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,83)
  Gcoeff = (c(2)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(85)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,84)
  Gcoeff = (c(2)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(85)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,85)
  Gcoeff = (c(2)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(85)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,89)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(85)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,90)
  Gcoeff = (c(2)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(85)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,91)
  Gcoeff = (c(3)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(85)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,95)
  Gcoeff = (c(3)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(85)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,96)
  Gcoeff = (c(3)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(85)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,97)
  Gcoeff = (c(2)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(93)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,101)
  Gcoeff = (c(2)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(93)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,102)
  Gcoeff = (c(2)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(93)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,103)
  Gcoeff = (c(2)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(93)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,107)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(93)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,108)
  Gcoeff = (c(2)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(93)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,109)
  Gcoeff = (c(3)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(93)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,113)
  Gcoeff = (c(3)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(93)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,114)
  Gcoeff = (c(3)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(93)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,115)
  Gcoeff = (c(2)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(94)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,161)
  Gcoeff = (c(2)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(94)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,162)
  Gcoeff = (c(2)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(94)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,163)
  Gcoeff = (c(2)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(94)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,167)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(94)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,168)
  Gcoeff = (c(2)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(94)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,169)
  Gcoeff = (c(3)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(94)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,173)
  Gcoeff = (c(3)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(94)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,174)
  Gcoeff = (c(3)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(94)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,175)
  Gcoeff = (c(2)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(90)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,143)
  Gcoeff = (c(2)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(90)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,144)
  Gcoeff = (c(2)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(90)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,145)
  Gcoeff = (c(2)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(90)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,149)
  Gcoeff = (c(2)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(90)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,150)
  Gcoeff = (c(2)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(90)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,151)
  Gcoeff = (c(3)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(90)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,155)
  Gcoeff = (c(3)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(90)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,156)
  Gcoeff = (c(3)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(90)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,157)
  Gcoeff = (c(1)*(M(5)-M(6)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(101)-M(103)-M(109)+M(115))+c(2)*(M(135)-M(136)-M(159)+M(160) &
    +M(195)-M(196)-M(219)+M(220))) * den(96)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,64)
  Gcoeff = (c(1)*(M(5)-M(6)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(101)-M(103)-M(109)+M(115))+c(2)*(M(191)-M(193)-M(215)+M(217) &
    +M(227)-M(228)-M(233)+M(234))) * den(96)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,65)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(96)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,66)
  Gcoeff = (c(1)*(M(5)-M(6)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(101)-M(103)-M(109)+M(115))+c(2)*(M(191)-M(193)-M(215)+M(217) &
    +M(227)-M(228)-M(233)+M(234))) * den(96)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,73)
  Gcoeff = (c(1)*(M(5)-M(6)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(101)-M(103)-M(109)+M(115))+c(2)*(M(135)-M(136)-M(159)+M(160) &
    +M(195)-M(196)-M(219)+M(220))) * den(96)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,74)
  Gcoeff = (c(2)*(M(135)-M(136)-M(159)+M(160)-M(191)+M(193)+M(195)-M(196)+M(215)-M(217)-M(219)+M(220)-M(227)+M(228)+M(233) &
    -M(234))) * den(96)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,75)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(96)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,82)
  Gcoeff = (c(2)*(M(135)-M(136)-M(159)+M(160)-M(191)+M(193)+M(195)-M(196)+M(215)-M(217)-M(219)+M(220)-M(227)+M(228)+M(233) &
    -M(234))) * den(96)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,83)
  Gcoeff = (c(3)*(M(135)-M(136)-M(159)+M(160)-M(191)+M(193)+M(195)-M(196)+M(215)-M(217)-M(219)+M(220)-M(227)+M(228)+M(233) &
    -M(234))) * den(96)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,84)
  Gcoeff = (c(2)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(101)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,65)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(101)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,66)
  Gcoeff = (c(2)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(101)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,67)
  Gcoeff = (c(2)*(M(136)-M(160)-M(180)+M(186)+M(191)-M(193)-M(195)+M(196)+M(197)-M(199)-M(204)+M(210)+M(221)-M(223)-M(228) &
    +M(234))) * den(101)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,71)
  Gcoeff = (c(2)*(M(135)-M(136)-M(159)+M(160)-M(191)+M(193)+M(195)-M(196)+M(215)-M(217)-M(219)+M(220)-M(227)+M(228)+M(233) &
    -M(234))) * den(101)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,72)
  Gcoeff = (c(2)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(101)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,73)
  Gcoeff = (c(3)*(M(136)-M(160)-M(180)+M(186)+M(191)-M(193)-M(195)+M(196)+M(197)-M(199)-M(204)+M(210)+M(221)-M(223)-M(228) &
    +M(234))) * den(101)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,77)
  Gcoeff = (c(3)*(M(135)-M(136)-M(159)+M(160)-M(191)+M(193)+M(195)-M(196)+M(215)-M(217)-M(219)+M(220)-M(227)+M(228)+M(233) &
    -M(234))) * den(101)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,78)
  Gcoeff = (c(3)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(101)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,79)
  Gcoeff = (c(1)*(-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(102)+M(108)+M(119)-M(121))+c(2)*(M(153)-M(154)+M(163)-M(164) &
    -M(187)+M(188)-M(231)+M(232))) * den(106)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,10)
  Gcoeff = (c(1)*(-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(102)+M(108)+M(119)-M(121))+c(2)*(M(162)-M(165)-M(186)+M(189) &
    +M(207)-M(208)-M(221)+M(222))) * den(106)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,11)
  Gcoeff = (c(2)*(-M(153)+M(154)+M(162)-M(163)+M(164)-M(165)-M(186)+M(187)-M(188)+M(189)+M(207)-M(208)-M(221)+M(222)+M(231) &
    -M(232))) * den(106)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,12)
  Gcoeff = (c(1)*(-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(102)+M(108)+M(119)-M(121))+c(2)*(M(162)-M(165)-M(186)+M(189) &
    +M(207)-M(208)-M(221)+M(222))) * den(106)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,19)
  Gcoeff = (c(1)*(-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(102)+M(108)+M(119)-M(121))+c(2)*(M(153)-M(154)+M(163)-M(164) &
    -M(187)+M(188)-M(231)+M(232))) * den(106)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,20)
  Gcoeff = (c(2)*(M(153)-M(154)-M(162)+M(163)-M(164)+M(165)+M(186)-M(187)+M(188)-M(189)-M(207)+M(208)+M(221)-M(222)-M(231) &
    +M(232))) * den(106)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,21)
  Gcoeff = (c(2)*(-M(153)+M(154)+M(162)-M(163)+M(164)-M(165)-M(186)+M(187)-M(188)+M(189)+M(207)-M(208)-M(221)+M(222)+M(231) &
    -M(232))) * den(106)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,28)
  Gcoeff = (c(2)*(M(153)-M(154)-M(162)+M(163)-M(164)+M(165)+M(186)-M(187)+M(188)-M(189)-M(207)+M(208)+M(221)-M(222)-M(231) &
    +M(232))) * den(106)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,29)
  Gcoeff = (c(3)*(M(153)-M(154)-M(162)+M(163)-M(164)+M(165)+M(186)-M(187)+M(188)-M(189)-M(207)+M(208)+M(221)-M(222)-M(231) &
    +M(232))) * den(106)
  T2sum(1:1,22) = T2sum(1:1,22) + Gcoeff * G0tensor(:,30)
  Gcoeff = (c(1)*(-M(11)+M(12)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(104)+M(114)+M(120)-M(123))+c(2)*(M(151)-M(152)+M(169)-M(170) &
    -M(211)+M(212)-M(229)+M(230))) * den(110)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,37)
  Gcoeff = (c(1)*(-M(11)+M(12)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(104)+M(114)+M(120)-M(123))+c(2)*(M(168)-M(171)+M(183)-M(184) &
    -M(197)+M(198)-M(210)+M(213))) * den(110)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,38)
  Gcoeff = (c(2)*(-M(151)+M(152)+M(168)-M(169)+M(170)-M(171)+M(183)-M(184)-M(197)+M(198)-M(210)+M(211)-M(212)+M(213)+M(229) &
    -M(230))) * den(110)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,39)
  Gcoeff = (c(1)*(-M(11)+M(12)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(104)+M(114)+M(120)-M(123))+c(2)*(M(168)-M(171)+M(183)-M(184) &
    -M(197)+M(198)-M(210)+M(213))) * den(110)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,46)
  Gcoeff = (c(1)*(-M(11)+M(12)+M(21)-M(22)-M(25)+M(26)+M(29)-M(30)-M(104)+M(114)+M(120)-M(123))+c(2)*(M(151)-M(152)+M(169)-M(170) &
    -M(211)+M(212)-M(229)+M(230))) * den(110)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,47)
  Gcoeff = (c(2)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(110)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,48)
  Gcoeff = (c(2)*(-M(151)+M(152)+M(168)-M(169)+M(170)-M(171)+M(183)-M(184)-M(197)+M(198)-M(210)+M(211)-M(212)+M(213)+M(229) &
    -M(230))) * den(110)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,55)
  Gcoeff = (c(2)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(110)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,56)
  Gcoeff = (c(3)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(110)
  T2sum(1:1,20) = T2sum(1:1,20) + Gcoeff * G0tensor(:,57)
  Gcoeff = (c(2)*(-M(154)+M(160)-M(162)+M(163)-M(164)+M(165)-M(168)+M(171)-M(174)+M(184)-M(192)+M(195)-M(198)+M(208)-M(222) &
    +M(232))) * den(124)
  T2sum(1:5,22) = T2sum(1:5,22) + Gcoeff * G1tensor(:,11)
  Gcoeff = (c(2)*(-M(153)+M(154)+M(162)-M(163)+M(164)-M(165)-M(186)+M(187)-M(188)+M(189)+M(207)-M(208)-M(221)+M(222)+M(231) &
    -M(232))) * den(124)
  T2sum(1:5,22) = T2sum(1:5,22) + Gcoeff * G1tensor(:,12)
  Gcoeff = (c(2)*(M(153)-M(160)+M(168)-M(171)+M(174)-M(184)+M(186)-M(187)+M(188)-M(189)+M(192)-M(195)+M(198)-M(207)+M(221) &
    -M(231))) * den(124)
  T2sum(1:5,22) = T2sum(1:5,22) + Gcoeff * G1tensor(:,13)
  Gcoeff = (c(2)*(M(154)-M(160)+M(162)-M(163)+M(164)-M(165)+M(168)-M(171)+M(174)-M(184)+M(192)-M(195)+M(198)-M(208)+M(222) &
    -M(232))) * den(124)
  T2sum(1:5,22) = T2sum(1:5,22) + Gcoeff * G1tensor(:,17)
  Gcoeff = (c(2)*(M(153)-M(154)-M(162)+M(163)-M(164)+M(165)+M(186)-M(187)+M(188)-M(189)-M(207)+M(208)+M(221)-M(222)-M(231) &
    +M(232))) * den(124)
  T2sum(1:5,22) = T2sum(1:5,22) + Gcoeff * G1tensor(:,18)
  Gcoeff = (c(2)*(-M(153)+M(160)-M(168)+M(171)-M(174)+M(184)-M(186)+M(187)-M(188)+M(189)-M(192)+M(195)-M(198)+M(207)-M(221) &
    +M(231))) * den(124)
  T2sum(1:5,22) = T2sum(1:5,22) + Gcoeff * G1tensor(:,19)
  Gcoeff = (c(3)*(M(154)-M(160)+M(162)-M(163)+M(164)-M(165)+M(168)-M(171)+M(174)-M(184)+M(192)-M(195)+M(198)-M(208)+M(222) &
    -M(232))) * den(124)
  T2sum(1:5,22) = T2sum(1:5,22) + Gcoeff * G1tensor(:,23)
  Gcoeff = (c(3)*(M(153)-M(154)-M(162)+M(163)-M(164)+M(165)+M(186)-M(187)+M(188)-M(189)-M(207)+M(208)+M(221)-M(222)-M(231) &
    +M(232))) * den(124)
  T2sum(1:5,22) = T2sum(1:5,22) + Gcoeff * G1tensor(:,24)
  Gcoeff = (c(3)*(-M(153)+M(160)-M(168)+M(171)-M(174)+M(184)-M(186)+M(187)-M(188)+M(189)-M(192)+M(195)-M(198)+M(207)-M(221) &
    +M(231))) * den(124)
  T2sum(1:5,22) = T2sum(1:5,22) + Gcoeff * G1tensor(:,25)
  Gcoeff = (c(2)*(-M(152)+M(159)-M(162)+M(165)-M(168)+M(169)-M(170)+M(171)-M(173)+M(184)-M(198)+M(208)-M(216)+M(219)-M(222) &
    +M(230))) * den(121)
  T2sum(1:5,20) = T2sum(1:5,20) + Gcoeff * G1tensor(:,29)
  Gcoeff = (c(2)*(-M(151)+M(152)+M(168)-M(169)+M(170)-M(171)+M(183)-M(184)-M(197)+M(198)-M(210)+M(211)-M(212)+M(213)+M(229) &
    -M(230))) * den(121)
  T2sum(1:5,20) = T2sum(1:5,20) + Gcoeff * G1tensor(:,30)
  Gcoeff = (c(2)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(121)
  T2sum(1:5,20) = T2sum(1:5,20) + Gcoeff * G1tensor(:,31)
  Gcoeff = (c(2)*(M(152)-M(159)+M(162)-M(165)+M(168)-M(169)+M(170)-M(171)+M(173)-M(184)+M(198)-M(208)+M(216)-M(219)+M(222) &
    -M(230))) * den(121)
  T2sum(1:5,20) = T2sum(1:5,20) + Gcoeff * G1tensor(:,35)
  Gcoeff = (c(2)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(121)
  T2sum(1:5,20) = T2sum(1:5,20) + Gcoeff * G1tensor(:,36)
  Gcoeff = (c(2)*(-M(151)+M(159)-M(162)+M(165)-M(173)+M(183)-M(197)+M(208)-M(210)+M(211)-M(212)+M(213)-M(216)+M(219)-M(222) &
    +M(229))) * den(121)
  T2sum(1:5,20) = T2sum(1:5,20) + Gcoeff * G1tensor(:,37)
  Gcoeff = (c(3)*(M(152)-M(159)+M(162)-M(165)+M(168)-M(169)+M(170)-M(171)+M(173)-M(184)+M(198)-M(208)+M(216)-M(219)+M(222) &
    -M(230))) * den(121)
  T2sum(1:5,20) = T2sum(1:5,20) + Gcoeff * G1tensor(:,41)
  Gcoeff = (c(3)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(121)
  T2sum(1:5,20) = T2sum(1:5,20) + Gcoeff * G1tensor(:,42)
  Gcoeff = (c(3)*(-M(151)+M(159)-M(162)+M(165)-M(173)+M(183)-M(197)+M(208)-M(210)+M(211)-M(212)+M(213)-M(216)+M(219)-M(222) &
    +M(229))) * den(121)
  T2sum(1:5,20) = T2sum(1:5,20) + Gcoeff * G1tensor(:,43)
  Gcoeff = (c(1)*(-M(5)+M(6)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(110)+M(116)+M(122)-M(124))+c(2)*(M(149)-M(150)+M(193)-M(194) &
    -M(217)+M(218)-M(227)+M(228))) * den(119)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,235)
  Gcoeff = (c(1)*(-M(5)+M(6)+M(19)-M(20)+M(25)-M(26)-M(29)+M(30)-M(110)+M(116)+M(122)-M(124))+c(2)*(M(159)-M(160)-M(173)+M(174) &
    +M(192)-M(195)-M(216)+M(219))) * den(119)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,236)
  Gcoeff = (c(2)*(-M(149)+M(150)+M(159)-M(160)-M(173)+M(174)+M(192)-M(193)+M(194)-M(195)-M(216)+M(217)-M(218)+M(219)+M(227) &
    -M(228))) * den(119)
  T2sum(1:1,23) = T2sum(1:1,23) + Gcoeff * G0tensor(:,237)
  Gcoeff = (c(2)*(-M(150)+M(160)-M(174)+M(183)-M(186)+M(189)-M(192)+M(193)-M(194)+M(195)-M(197)+M(207)-M(210)+M(213)-M(221) &
    +M(228))) * den(112)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,221)
  Gcoeff = (c(2)*(-M(149)+M(150)+M(159)-M(160)-M(173)+M(174)+M(192)-M(193)+M(194)-M(195)-M(216)+M(217)-M(218)+M(219)+M(227) &
    -M(228))) * den(112)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,222)
  Gcoeff = (c(2)*(M(149)-M(159)+M(173)-M(183)+M(186)-M(189)+M(197)-M(207)+M(210)-M(213)+M(216)-M(217)+M(218)-M(219)+M(221) &
    -M(227))) * den(112)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,223)
  Gcoeff = (c(3)*(-M(140)+M(146)+M(152)-M(154)+M(155)-M(157)-M(159)+M(160)-M(164)+M(170)+M(176)-M(178)+M(195)-M(219)-M(243) &
    +M(249))) * den(177)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(2)*(-M(154)+M(160)-M(162)+M(163)-M(164)+M(165)-M(168)+M(171)-M(174)+M(184)-M(192)+M(195)-M(198)+M(208)-M(222) &
    +M(232))) * den(180)
  T2sum(1:5,22) = T2sum(1:5,22) + Gcoeff * G1tensor(:,14)
  Gcoeff = (c(2)*(M(154)-M(160)+M(162)-M(163)+M(164)-M(165)+M(168)-M(171)+M(174)-M(184)+M(192)-M(195)+M(198)-M(208)+M(222) &
    -M(232))) * den(180)
  T2sum(1:5,22) = T2sum(1:5,22) + Gcoeff * G1tensor(:,20)
  Gcoeff = (c(3)*(M(154)-M(160)+M(162)-M(163)+M(164)-M(165)+M(168)-M(171)+M(174)-M(184)+M(192)-M(195)+M(198)-M(208)+M(222) &
    -M(232))) * den(180)
  T2sum(1:5,22) = T2sum(1:5,22) + Gcoeff * G1tensor(:,26)
  Gcoeff = (c(2)*(-M(152)+M(159)-M(162)+M(165)-M(168)+M(169)-M(170)+M(171)-M(173)+M(184)-M(198)+M(208)-M(216)+M(219)-M(222) &
    +M(230))) * den(389)
  T2sum(1:5,20) = T2sum(1:5,20) + Gcoeff * G1tensor(:,32)
  Gcoeff = (c(2)*(M(152)-M(159)+M(162)-M(165)+M(168)-M(169)+M(170)-M(171)+M(173)-M(184)+M(198)-M(208)+M(216)-M(219)+M(222) &
    -M(230))) * den(389)
  T2sum(1:5,20) = T2sum(1:5,20) + Gcoeff * G1tensor(:,38)
  Gcoeff = (c(3)*(M(152)-M(159)+M(162)-M(165)+M(168)-M(169)+M(170)-M(171)+M(173)-M(184)+M(198)-M(208)+M(216)-M(219)+M(222) &
    -M(230))) * den(389)
  T2sum(1:5,20) = T2sum(1:5,20) + Gcoeff * G1tensor(:,44)
  Gcoeff = (c(2)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(191)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,68)
  Gcoeff = (c(2)*(M(136)-M(160)-M(180)+M(186)+M(191)-M(193)-M(195)+M(196)+M(197)-M(199)-M(204)+M(210)+M(221)-M(223)-M(228) &
    +M(234))) * den(191)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,74)
  Gcoeff = (c(3)*(M(136)-M(160)-M(180)+M(186)+M(191)-M(193)-M(195)+M(196)+M(197)-M(199)-M(204)+M(210)+M(221)-M(223)-M(228) &
    +M(234))) * den(191)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,80)
  Gcoeff = (c(2)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(193)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,86)
  Gcoeff = (c(2)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(193)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,92)
  Gcoeff = (c(3)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(193)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,98)
  Gcoeff = (c(2)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(196)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,69)
  Gcoeff = (c(2)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(196)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,75)
  Gcoeff = (c(3)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(196)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,81)
  Gcoeff = (c(2)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(198)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,104)
  Gcoeff = (c(2)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(198)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,110)
  Gcoeff = (c(3)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(198)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,116)
  Gcoeff = (c(2)*(M(131)-M(133)-M(155)+M(157)-M(191)+M(193)+M(215)-M(217)-M(227)+M(228)+M(233)-M(234)+M(243)-M(244)-M(249) &
    +M(250))) * den(199)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,119)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(155)-M(157)+M(191)-M(193)-M(215)+M(217)+M(227)-M(228)-M(233)+M(234)-M(243)+M(244)+M(249) &
    -M(250))) * den(199)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,120)
  Gcoeff = (c(3)*(-M(131)+M(133)+M(155)-M(157)+M(191)-M(193)-M(215)+M(217)+M(227)-M(228)-M(233)+M(234)-M(243)+M(244)+M(249) &
    -M(250))) * den(199)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,121)
  Gcoeff = (c(2)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(201)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,131)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(135)-M(136)+M(155)-M(157)-M(159)+M(160)+M(195)-M(196)-M(219)+M(220)-M(243)+M(244)+M(249) &
    -M(250))) * den(201)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,132)
  Gcoeff = (c(3)*(-M(131)+M(133)+M(135)-M(136)+M(155)-M(157)-M(159)+M(160)+M(195)-M(196)-M(219)+M(220)-M(243)+M(244)+M(249) &
    -M(250))) * den(201)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,133)
  Gcoeff = (c(2)*(M(135)-M(136)-M(159)+M(160)-M(191)+M(193)+M(195)-M(196)+M(215)-M(217)-M(219)+M(220)-M(227)+M(228)+M(233) &
    -M(234))) * den(203)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,70)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(203)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,76)
  Gcoeff = (c(3)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(203)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,82)
  Gcoeff = (c(2)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(206)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,164)
  Gcoeff = (c(2)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(206)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,170)
  Gcoeff = (c(3)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(206)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,176)
  Gcoeff = (c(2)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(420)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,146)
  Gcoeff = (c(2)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(420)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,152)
  Gcoeff = (c(3)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(420)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,158)
  Gcoeff = (c(2)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(217)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,87)
  Gcoeff = (c(2)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(217)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,93)
  Gcoeff = (c(3)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(217)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,99)
  Gcoeff = (c(2)*(M(150)-M(153)-M(168)+M(171)-M(183)+M(184)+M(187)-M(188)-M(193)+M(194)+M(197)-M(198)+M(210)-M(213)-M(228) &
    +M(231))) * den(218)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,197)
  Gcoeff = (c(2)*(-M(150)+M(153)+M(168)-M(171)+M(183)-M(184)-M(187)+M(188)+M(193)-M(194)-M(197)+M(198)-M(210)+M(213)+M(228) &
    -M(231))) * den(218)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,198)
  Gcoeff = (c(3)*(-M(150)+M(153)+M(168)-M(171)+M(183)-M(184)-M(187)+M(188)+M(193)-M(194)-M(197)+M(198)-M(210)+M(213)+M(228) &
    -M(231))) * den(218)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,199)
  Gcoeff = (c(2)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(220)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,209)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(220)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,210)
  Gcoeff = (c(3)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(220)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,211)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(222)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,88)
  Gcoeff = (c(2)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(222)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,94)
  Gcoeff = (c(3)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(222)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,100)
  Gcoeff = (c(2)*(M(153)-M(160)+M(168)-M(171)+M(174)-M(184)+M(186)-M(187)+M(188)-M(189)+M(192)-M(195)+M(198)-M(207)+M(221) &
    -M(231))) * den(223)
  T2sum(1:5,22) = T2sum(1:5,22) + Gcoeff * G1tensor(:,15)
  Gcoeff = (c(2)*(-M(153)+M(160)-M(168)+M(171)-M(174)+M(184)-M(186)+M(187)-M(188)+M(189)-M(192)+M(195)-M(198)+M(207)-M(221) &
    +M(231))) * den(223)
  T2sum(1:5,22) = T2sum(1:5,22) + Gcoeff * G1tensor(:,21)
  Gcoeff = (c(3)*(-M(153)+M(160)-M(168)+M(171)-M(174)+M(184)-M(186)+M(187)-M(188)+M(189)-M(192)+M(195)-M(198)+M(207)-M(221) &
    +M(231))) * den(223)
  T2sum(1:5,22) = T2sum(1:5,22) + Gcoeff * G1tensor(:,27)
  Gcoeff = (c(2)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(235)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,105)
  Gcoeff = (c(2)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(235)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,111)
  Gcoeff = (c(3)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(235)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,117)
  Gcoeff = (c(2)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(243)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,165)
  Gcoeff = (c(2)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(243)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,171)
  Gcoeff = (c(3)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(243)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,177)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(253)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,106)
  Gcoeff = (c(2)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(253)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,112)
  Gcoeff = (c(3)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(253)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,118)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(261)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,166)
  Gcoeff = (c(2)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(261)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,172)
  Gcoeff = (c(3)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(261)
  T2sum(1:5,25) = T2sum(1:5,25) + Gcoeff * G1tensor(:,178)
  Gcoeff = (c(2)*(M(153)-M(154)-M(162)+M(163)-M(164)+M(165)+M(186)-M(187)+M(188)-M(189)-M(207)+M(208)+M(221)-M(222)-M(231) &
    +M(232))) * den(264)
  T2sum(1:5,22) = T2sum(1:5,22) + Gcoeff * G1tensor(:,16)
  Gcoeff = (c(2)*(-M(153)+M(154)+M(162)-M(163)+M(164)-M(165)-M(186)+M(187)-M(188)+M(189)+M(207)-M(208)-M(221)+M(222)+M(231) &
    -M(232))) * den(264)
  T2sum(1:5,22) = T2sum(1:5,22) + Gcoeff * G1tensor(:,22)
  Gcoeff = (c(3)*(-M(153)+M(154)+M(162)-M(163)+M(164)-M(165)-M(186)+M(187)-M(188)+M(189)+M(207)-M(208)-M(221)+M(222)+M(231) &
    -M(232))) * den(264)
  T2sum(1:5,22) = T2sum(1:5,22) + Gcoeff * G1tensor(:,28)
  Gcoeff = (c(2)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(267)
  T2sum(1:5,20) = T2sum(1:5,20) + Gcoeff * G1tensor(:,33)
  Gcoeff = (c(2)*(-M(151)+M(159)-M(162)+M(165)-M(173)+M(183)-M(197)+M(208)-M(210)+M(211)-M(212)+M(213)-M(216)+M(219)-M(222) &
    +M(229))) * den(267)
  T2sum(1:5,20) = T2sum(1:5,20) + Gcoeff * G1tensor(:,39)
  Gcoeff = (c(3)*(-M(151)+M(159)-M(162)+M(165)-M(173)+M(183)-M(197)+M(208)-M(210)+M(211)-M(212)+M(213)-M(216)+M(219)-M(222) &
    +M(229))) * den(267)
  T2sum(1:5,20) = T2sum(1:5,20) + Gcoeff * G1tensor(:,45)
  Gcoeff = (c(2)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(274)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,147)
  Gcoeff = (c(2)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(274)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,153)
  Gcoeff = (c(3)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(274)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,159)
  Gcoeff = (c(2)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(280)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,148)
  Gcoeff = (c(2)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(280)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,154)
  Gcoeff = (c(3)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(280)
  T2sum(1:5,24) = T2sum(1:5,24) + Gcoeff * G1tensor(:,160)
  Gcoeff = (c(2)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(282)
  T2sum(1:5,20) = T2sum(1:5,20) + Gcoeff * G1tensor(:,34)
  Gcoeff = (c(2)*(-M(151)+M(152)+M(168)-M(169)+M(170)-M(171)+M(183)-M(184)-M(197)+M(198)-M(210)+M(211)-M(212)+M(213)+M(229) &
    -M(230))) * den(282)
  T2sum(1:5,20) = T2sum(1:5,20) + Gcoeff * G1tensor(:,40)
  Gcoeff = (c(3)*(-M(151)+M(152)+M(168)-M(169)+M(170)-M(171)+M(183)-M(184)-M(197)+M(198)-M(210)+M(211)-M(212)+M(213)+M(229) &
    -M(230))) * den(282)
  T2sum(1:5,20) = T2sum(1:5,20) + Gcoeff * G1tensor(:,46)
  Gcoeff = (c(2)*(-M(150)+M(160)-M(174)+M(183)-M(186)+M(189)-M(192)+M(193)-M(194)+M(195)-M(197)+M(207)-M(210)+M(213)-M(221) &
    +M(228))) * den(465)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,224)
  Gcoeff = (c(2)*(M(149)-M(159)+M(173)-M(183)+M(186)-M(189)+M(197)-M(207)+M(210)-M(213)+M(216)-M(217)+M(218)-M(219)+M(221) &
    -M(227))) * den(471)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,225)
  Gcoeff = (c(2)*(M(149)-M(150)-M(159)+M(160)+M(173)-M(174)-M(192)+M(193)-M(194)+M(195)+M(216)-M(217)+M(218)-M(219)-M(227) &
    +M(228))) * den(288)
  T2sum(1:5,23) = T2sum(1:5,23) + Gcoeff * G1tensor(:,226)

end subroutine vamp_76

end module ol_vamp_76_ppjjjj_gggggg_1_/**/REALKIND
