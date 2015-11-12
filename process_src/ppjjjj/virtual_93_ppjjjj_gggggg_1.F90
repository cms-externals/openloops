
module ol_vamp_93_ppjjjj_gggggg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_93(M)
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
  complex(REALKIND), dimension(4,1,4,105) :: G0
  complex(REALKIND), dimension(4,5,4,275) :: G1
  complex(REALKIND), dimension(5,176) :: G1tensor
  complex(REALKIND), dimension(15,285) :: G2tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,983),G0(:,:,:,2))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,1))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,984),G0(:,:,:,3))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,2))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,984),wf(:,0),G0(:,:,:,4))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,3))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,984),G0(:,:,:,5))
  call check_last_UV_W(l_switch,G0(:,:,:,5),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,4))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1039),Q(:,57),G1(:,:,:,1))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,1),wf(:,-2),wf(:,-1),G1tensor(:,5))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,1),wf(:,-1),wf(:,-2),G1tensor(:,6))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,1),wf(:,-2),wf(:,-1),G1tensor(:,7))
  call check_last_UV_W(l_switch,G1(:,:,:,1),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,1))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1040),Q(:,57),G1(:,:,:,2))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,2),wf(:,-2),wf(:,-1),G1tensor(:,8))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,2),wf(:,-1),wf(:,-2),G1tensor(:,9))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,2),wf(:,-2),wf(:,-1),G1tensor(:,10))
  call check_last_UV_W(l_switch,G1(:,:,:,2),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,2))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1041),Q(:,57),G1(:,:,:,3))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,3),wf(:,-2),wf(:,-1),G1tensor(:,11))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,3),wf(:,-1),wf(:,-2),G1tensor(:,12))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,3),wf(:,-2),wf(:,-1),G1tensor(:,13))
  call check_last_UV_W(l_switch,G1(:,:,:,3),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,3))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,994),Q(:,46),G1(:,:,:,4))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,4),wf(:,-4),wf(:,0),G1tensor(:,14))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,4),wf(:,0),wf(:,-4),G1tensor(:,15))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,4),wf(:,-4),wf(:,0),G1tensor(:,16))
  call check_last_UV_W(l_switch,G1(:,:,:,4),Q(:,46),wf(:,109),Q(:,17),G2tensor(:,4))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,995),Q(:,46),G1(:,:,:,5))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,5),wf(:,-4),wf(:,0),G1tensor(:,17))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,5),wf(:,0),wf(:,-4),G1tensor(:,18))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,5),wf(:,-4),wf(:,0),G1tensor(:,19))
  call check_last_UV_W(l_switch,G1(:,:,:,5),Q(:,46),wf(:,109),Q(:,17),G2tensor(:,5))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,996),Q(:,46),G1(:,:,:,6))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,6),wf(:,-4),wf(:,0),G1tensor(:,20))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,6),wf(:,0),wf(:,-4),G1tensor(:,21))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,6),wf(:,-4),wf(:,0),G1tensor(:,22))
  call check_last_UV_W(l_switch,G1(:,:,:,6),Q(:,46),wf(:,109),Q(:,17),G2tensor(:,6))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1042),Q(:,57),G1(:,:,:,7))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,7),wf(:,-2),wf(:,-1),G1tensor(:,23))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,7),wf(:,-1),wf(:,-2),G1tensor(:,24))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,7),wf(:,-2),wf(:,-1),G1tensor(:,25))
  call check_last_UV_W(l_switch,G1(:,:,:,7),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,7))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1043),Q(:,57),G1(:,:,:,8))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,8),wf(:,-2),wf(:,-1),G1tensor(:,26))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,8),wf(:,-1),wf(:,-2),G1tensor(:,27))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,8),wf(:,-2),wf(:,-1),G1tensor(:,28))
  call check_last_UV_W(l_switch,G1(:,:,:,8),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,8))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1044),Q(:,57),G1(:,:,:,9))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,9),wf(:,-2),wf(:,-1),G1tensor(:,29))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,9),wf(:,-1),wf(:,-2),G1tensor(:,30))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,9),wf(:,-2),wf(:,-1),G1tensor(:,31))
  call check_last_UV_W(l_switch,G1(:,:,:,9),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,9))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,997),Q(:,46),G1(:,:,:,10))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,10),wf(:,-4),wf(:,0),G1tensor(:,32))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,10),wf(:,0),wf(:,-4),G1tensor(:,33))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,10),wf(:,-4),wf(:,0),G1tensor(:,34))
  call check_last_UV_W(l_switch,G1(:,:,:,10),Q(:,46),wf(:,109),Q(:,17),G2tensor(:,10))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,998),Q(:,46),G1(:,:,:,11))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,11),wf(:,-4),wf(:,0),G1tensor(:,35))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,11),wf(:,0),wf(:,-4),G1tensor(:,36))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,11),wf(:,-4),wf(:,0),G1tensor(:,37))
  call check_last_UV_W(l_switch,G1(:,:,:,11),Q(:,46),wf(:,109),Q(:,17),G2tensor(:,11))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,999),Q(:,46),G1(:,:,:,12))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,12),wf(:,-4),wf(:,0),G1tensor(:,38))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,12),wf(:,0),wf(:,-4),G1tensor(:,39))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,12),wf(:,-4),wf(:,0),G1tensor(:,40))
  call check_last_UV_W(l_switch,G1(:,:,:,12),Q(:,46),wf(:,109),Q(:,17),G2tensor(:,12))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,988),G0(:,:,:,6))
  call check_last_UV_W(l_switch,G0(:,:,:,6),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,41))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,988),wf(:,0),G0(:,:,:,7))
  call check_last_UV_W(l_switch,G0(:,:,:,7),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,42))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,988),G0(:,:,:,8))
  call check_last_UV_W(l_switch,G0(:,:,:,8),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,43))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,989),G0(:,:,:,9))
  call check_last_UV_W(l_switch,G0(:,:,:,9),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,44))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,989),wf(:,0),G0(:,:,:,10))
  call check_last_UV_W(l_switch,G0(:,:,:,10),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,45))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,989),G0(:,:,:,11))
  call check_last_UV_W(l_switch,G0(:,:,:,11),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,46))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,990),G0(:,:,:,12))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,47))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,990),wf(:,0),G0(:,:,:,13))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,48))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,990),G0(:,:,:,14))
  call check_last_UV_W(l_switch,G0(:,:,:,14),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,49))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1045),Q(:,57),G1(:,:,:,13))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,13),wf(:,-2),wf(:,-1),G1tensor(:,50))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,13),wf(:,-1),wf(:,-2),G1tensor(:,51))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,13),wf(:,-2),wf(:,-1),G1tensor(:,52))
  call check_last_UV_W(l_switch,G1(:,:,:,13),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,13))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1046),Q(:,57),G1(:,:,:,14))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,14),wf(:,-2),wf(:,-1),G1tensor(:,53))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,14),wf(:,-1),wf(:,-2),G1tensor(:,54))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,14),wf(:,-2),wf(:,-1),G1tensor(:,55))
  call check_last_UV_W(l_switch,G1(:,:,:,14),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,14))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1047),Q(:,57),G1(:,:,:,15))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,15),wf(:,-2),wf(:,-1),G1tensor(:,56))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,15),wf(:,-1),wf(:,-2),G1tensor(:,57))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,15),wf(:,-2),wf(:,-1),G1tensor(:,58))
  call check_last_UV_W(l_switch,G1(:,:,:,15),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,15))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,979),Q(:,54),G1(:,:,:,16))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,16),wf(:,-3),wf(:,0),G1tensor(:,59))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,16),wf(:,0),wf(:,-3),G1tensor(:,60))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,16),wf(:,-3),wf(:,0),G1tensor(:,61))
  call check_last_UV_W(l_switch,G1(:,:,:,16),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,16))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,980),Q(:,54),G1(:,:,:,17))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,17),wf(:,-3),wf(:,0),G1tensor(:,62))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,17),wf(:,0),wf(:,-3),G1tensor(:,63))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,17),wf(:,-3),wf(:,0),G1tensor(:,64))
  call check_last_UV_W(l_switch,G1(:,:,:,17),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,17))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,981),Q(:,54),G1(:,:,:,18))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,18),wf(:,-3),wf(:,0),G1tensor(:,65))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,18),wf(:,0),wf(:,-3),G1tensor(:,66))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,18),wf(:,-3),wf(:,0),G1tensor(:,67))
  call check_last_UV_W(l_switch,G1(:,:,:,18),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,18))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1048),Q(:,57),G1(:,:,:,19))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,19),wf(:,-2),wf(:,-1),G1tensor(:,68))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,19),wf(:,-1),wf(:,-2),G1tensor(:,69))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,19),wf(:,-2),wf(:,-1),G1tensor(:,70))
  call check_last_UV_W(l_switch,G1(:,:,:,19),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,19))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1049),Q(:,57),G1(:,:,:,20))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,20),wf(:,-2),wf(:,-1),G1tensor(:,71))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,20),wf(:,-1),wf(:,-2),G1tensor(:,72))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,20),wf(:,-2),wf(:,-1),G1tensor(:,73))
  call check_last_UV_W(l_switch,G1(:,:,:,20),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,20))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1050),Q(:,57),G1(:,:,:,21))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,21),wf(:,-2),wf(:,-1),G1tensor(:,74))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,21),wf(:,-1),wf(:,-2),G1tensor(:,75))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,21),wf(:,-2),wf(:,-1),G1tensor(:,76))
  call check_last_UV_W(l_switch,G1(:,:,:,21),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,21))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,982),Q(:,54),G1(:,:,:,22))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,22),wf(:,-3),wf(:,0),G1tensor(:,77))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,22),wf(:,0),wf(:,-3),G1tensor(:,78))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,22),wf(:,-3),wf(:,0),G1tensor(:,79))
  call check_last_UV_W(l_switch,G1(:,:,:,22),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,22))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,983),Q(:,54),G1(:,:,:,23))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,23),wf(:,-3),wf(:,0),G1tensor(:,80))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,23),wf(:,0),wf(:,-3),G1tensor(:,81))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,23),wf(:,-3),wf(:,0),G1tensor(:,82))
  call check_last_UV_W(l_switch,G1(:,:,:,23),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,23))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,984),Q(:,54),G1(:,:,:,24))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,24),wf(:,-3),wf(:,0),G1tensor(:,83))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,24),wf(:,0),wf(:,-3),G1tensor(:,84))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,24),wf(:,-3),wf(:,0),G1tensor(:,85))
  call check_last_UV_W(l_switch,G1(:,:,:,24),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,24))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1051),Q(:,57),G1(:,:,:,25))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,25),wf(:,-2),wf(:,-1),G1tensor(:,86))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,25),wf(:,-1),wf(:,-2),G1tensor(:,87))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,25),wf(:,-2),wf(:,-1),G1tensor(:,88))
  call check_last_UV_W(l_switch,G1(:,:,:,25),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,25))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1052),Q(:,57),G1(:,:,:,26))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,26),wf(:,-2),wf(:,-1),G1tensor(:,89))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,26),wf(:,-1),wf(:,-2),G1tensor(:,90))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,26),wf(:,-2),wf(:,-1),G1tensor(:,91))
  call check_last_UV_W(l_switch,G1(:,:,:,26),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,26))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1053),Q(:,57),G1(:,:,:,27))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,27),wf(:,-2),wf(:,-1),G1tensor(:,92))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,27),wf(:,-1),wf(:,-2),G1tensor(:,93))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,27),wf(:,-2),wf(:,-1),G1tensor(:,94))
  call check_last_UV_W(l_switch,G1(:,:,:,27),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,27))
  call loop_QV_A(G0(:,:,:,1),wf(:,44),G0(:,:,:,15))
  call loop_Q_A(G0(:,:,:,15),Q(:,38),ZERO,G1(:,:,:,28))
  call loop_QV_A(G1(:,:,:,28),wf(:,43),G1(:,:,:,29))
  call check_last_Q_A(l_switch,G1(:,:,:,29),Q(:,63),ZERO,G2tensor(:,28))
  call loop_QV_A(G1(:,:,:,28),wf(:,45),G1(:,:,:,30))
  call check_last_Q_A(l_switch,G1(:,:,:,30),Q(:,63),ZERO,G2tensor(:,29))
  call loop_QV_A(G1(:,:,:,28),wf(:,46),G1(:,:,:,31))
  call check_last_Q_A(l_switch,G1(:,:,:,31),Q(:,63),ZERO,G2tensor(:,30))
  call loop_QV_A(G1(:,:,:,28),wf(:,181),G1(:,:,:,32))
  call check_last_Q_A(l_switch,G1(:,:,:,32),Q(:,63),ZERO,G2tensor(:,31))
  call loop_QV_A(G1(:,:,:,28),wf(:,186),G1(:,:,:,33))
  call check_last_Q_A(l_switch,G1(:,:,:,33),Q(:,63),ZERO,G2tensor(:,32))
  call loop_QV_A(G1(:,:,:,28),wf(:,190),G1(:,:,:,34))
  call check_last_Q_A(l_switch,G1(:,:,:,34),Q(:,63),ZERO,G2tensor(:,33))
  call loop_QV_A(G0(:,:,:,1),wf(:,47),G0(:,:,:,16))
  call loop_Q_A(G0(:,:,:,16),Q(:,38),ZERO,G1(:,:,:,35))
  call loop_QV_A(G1(:,:,:,35),wf(:,43),G1(:,:,:,36))
  call check_last_Q_A(l_switch,G1(:,:,:,36),Q(:,63),ZERO,G2tensor(:,34))
  call loop_QV_A(G1(:,:,:,35),wf(:,45),G1(:,:,:,37))
  call check_last_Q_A(l_switch,G1(:,:,:,37),Q(:,63),ZERO,G2tensor(:,35))
  call loop_QV_A(G1(:,:,:,35),wf(:,46),G1(:,:,:,38))
  call check_last_Q_A(l_switch,G1(:,:,:,38),Q(:,63),ZERO,G2tensor(:,36))
  call loop_QV_A(G1(:,:,:,35),wf(:,181),G1(:,:,:,39))
  call check_last_Q_A(l_switch,G1(:,:,:,39),Q(:,63),ZERO,G2tensor(:,37))
  call loop_QV_A(G1(:,:,:,35),wf(:,186),G1(:,:,:,40))
  call check_last_Q_A(l_switch,G1(:,:,:,40),Q(:,63),ZERO,G2tensor(:,38))
  call loop_QV_A(G1(:,:,:,35),wf(:,190),G1(:,:,:,41))
  call check_last_Q_A(l_switch,G1(:,:,:,41),Q(:,63),ZERO,G2tensor(:,39))
  call loop_QV_A(G0(:,:,:,1),wf(:,48),G0(:,:,:,17))
  call loop_Q_A(G0(:,:,:,17),Q(:,38),ZERO,G1(:,:,:,42))
  call loop_QV_A(G1(:,:,:,42),wf(:,43),G1(:,:,:,43))
  call check_last_Q_A(l_switch,G1(:,:,:,43),Q(:,63),ZERO,G2tensor(:,40))
  call loop_QV_A(G1(:,:,:,42),wf(:,45),G1(:,:,:,44))
  call check_last_Q_A(l_switch,G1(:,:,:,44),Q(:,63),ZERO,G2tensor(:,41))
  call loop_QV_A(G1(:,:,:,42),wf(:,46),G1(:,:,:,45))
  call check_last_Q_A(l_switch,G1(:,:,:,45),Q(:,63),ZERO,G2tensor(:,42))
  call loop_QV_A(G1(:,:,:,42),wf(:,181),G1(:,:,:,46))
  call check_last_Q_A(l_switch,G1(:,:,:,46),Q(:,63),ZERO,G2tensor(:,43))
  call loop_QV_A(G1(:,:,:,42),wf(:,186),G1(:,:,:,47))
  call check_last_Q_A(l_switch,G1(:,:,:,47),Q(:,63),ZERO,G2tensor(:,44))
  call loop_QV_A(G1(:,:,:,42),wf(:,190),G1(:,:,:,48))
  call check_last_Q_A(l_switch,G1(:,:,:,48),Q(:,63),ZERO,G2tensor(:,45))
  call loop_QV_A(G0(:,:,:,1),wf(:,44),G0(:,:,:,18))
  call loop_Q_A(G0(:,:,:,18),Q(:,38),MT,G1(:,:,:,49))
  call loop_QV_A(G1(:,:,:,49),wf(:,43),G1(:,:,:,50))
  call check_last_Q_A(l_switch,G1(:,:,:,50),Q(:,63),MT,G2tensor(:,46))
  call loop_QV_A(G1(:,:,:,49),wf(:,45),G1(:,:,:,51))
  call check_last_Q_A(l_switch,G1(:,:,:,51),Q(:,63),MT,G2tensor(:,47))
  call loop_QV_A(G1(:,:,:,49),wf(:,46),G1(:,:,:,52))
  call check_last_Q_A(l_switch,G1(:,:,:,52),Q(:,63),MT,G2tensor(:,48))
  call loop_QV_A(G1(:,:,:,49),wf(:,181),G1(:,:,:,53))
  call check_last_Q_A(l_switch,G1(:,:,:,53),Q(:,63),MT,G2tensor(:,49))
  call loop_QV_A(G1(:,:,:,49),wf(:,186),G1(:,:,:,54))
  call check_last_Q_A(l_switch,G1(:,:,:,54),Q(:,63),MT,G2tensor(:,50))
  call loop_QV_A(G1(:,:,:,49),wf(:,190),G1(:,:,:,55))
  call check_last_Q_A(l_switch,G1(:,:,:,55),Q(:,63),MT,G2tensor(:,51))
  call loop_QV_A(G0(:,:,:,1),wf(:,47),G0(:,:,:,19))
  call loop_Q_A(G0(:,:,:,19),Q(:,38),MT,G1(:,:,:,56))
  call loop_QV_A(G1(:,:,:,56),wf(:,43),G1(:,:,:,57))
  call check_last_Q_A(l_switch,G1(:,:,:,57),Q(:,63),MT,G2tensor(:,52))
  call loop_QV_A(G1(:,:,:,56),wf(:,45),G1(:,:,:,58))
  call check_last_Q_A(l_switch,G1(:,:,:,58),Q(:,63),MT,G2tensor(:,53))
  call loop_QV_A(G1(:,:,:,56),wf(:,46),G1(:,:,:,59))
  call check_last_Q_A(l_switch,G1(:,:,:,59),Q(:,63),MT,G2tensor(:,54))
  call loop_QV_A(G1(:,:,:,56),wf(:,181),G1(:,:,:,60))
  call check_last_Q_A(l_switch,G1(:,:,:,60),Q(:,63),MT,G2tensor(:,55))
  call loop_QV_A(G1(:,:,:,56),wf(:,186),G1(:,:,:,61))
  call check_last_Q_A(l_switch,G1(:,:,:,61),Q(:,63),MT,G2tensor(:,56))
  call loop_QV_A(G1(:,:,:,56),wf(:,190),G1(:,:,:,62))
  call check_last_Q_A(l_switch,G1(:,:,:,62),Q(:,63),MT,G2tensor(:,57))
  call loop_QV_A(G0(:,:,:,1),wf(:,48),G0(:,:,:,20))
  call loop_Q_A(G0(:,:,:,20),Q(:,38),MT,G1(:,:,:,63))
  call loop_QV_A(G1(:,:,:,63),wf(:,43),G1(:,:,:,64))
  call check_last_Q_A(l_switch,G1(:,:,:,64),Q(:,63),MT,G2tensor(:,58))
  call loop_QV_A(G1(:,:,:,63),wf(:,45),G1(:,:,:,65))
  call check_last_Q_A(l_switch,G1(:,:,:,65),Q(:,63),MT,G2tensor(:,59))
  call loop_QV_A(G1(:,:,:,63),wf(:,46),G1(:,:,:,66))
  call check_last_Q_A(l_switch,G1(:,:,:,66),Q(:,63),MT,G2tensor(:,60))
  call loop_QV_A(G1(:,:,:,63),wf(:,181),G1(:,:,:,67))
  call check_last_Q_A(l_switch,G1(:,:,:,67),Q(:,63),MT,G2tensor(:,61))
  call loop_QV_A(G1(:,:,:,63),wf(:,186),G1(:,:,:,68))
  call check_last_Q_A(l_switch,G1(:,:,:,68),Q(:,63),MT,G2tensor(:,62))
  call loop_QV_A(G1(:,:,:,63),wf(:,190),G1(:,:,:,69))
  call check_last_Q_A(l_switch,G1(:,:,:,69),Q(:,63),MT,G2tensor(:,63))
  call loop_QV_A(G0(:,:,:,1),wf(:,44),G0(:,:,:,21))
  call loop_Q_A(G0(:,:,:,21),Q(:,38),MB,G1(:,:,:,70))
  call loop_QV_A(G1(:,:,:,70),wf(:,43),G1(:,:,:,71))
  call check_last_Q_A(l_switch,G1(:,:,:,71),Q(:,63),MB,G2tensor(:,64))
  call loop_QV_A(G1(:,:,:,70),wf(:,45),G1(:,:,:,72))
  call check_last_Q_A(l_switch,G1(:,:,:,72),Q(:,63),MB,G2tensor(:,65))
  call loop_QV_A(G1(:,:,:,70),wf(:,46),G1(:,:,:,73))
  call check_last_Q_A(l_switch,G1(:,:,:,73),Q(:,63),MB,G2tensor(:,66))
  call loop_QV_A(G1(:,:,:,70),wf(:,181),G1(:,:,:,74))
  call check_last_Q_A(l_switch,G1(:,:,:,74),Q(:,63),MB,G2tensor(:,67))
  call loop_QV_A(G1(:,:,:,70),wf(:,186),G1(:,:,:,75))
  call check_last_Q_A(l_switch,G1(:,:,:,75),Q(:,63),MB,G2tensor(:,68))
  call loop_QV_A(G1(:,:,:,70),wf(:,190),G1(:,:,:,76))
  call check_last_Q_A(l_switch,G1(:,:,:,76),Q(:,63),MB,G2tensor(:,69))
  call loop_QV_A(G0(:,:,:,1),wf(:,47),G0(:,:,:,22))
  call loop_Q_A(G0(:,:,:,22),Q(:,38),MB,G1(:,:,:,77))
  call loop_QV_A(G1(:,:,:,77),wf(:,43),G1(:,:,:,78))
  call check_last_Q_A(l_switch,G1(:,:,:,78),Q(:,63),MB,G2tensor(:,70))
  call loop_QV_A(G1(:,:,:,77),wf(:,45),G1(:,:,:,79))
  call check_last_Q_A(l_switch,G1(:,:,:,79),Q(:,63),MB,G2tensor(:,71))
  call loop_QV_A(G1(:,:,:,77),wf(:,46),G1(:,:,:,80))
  call check_last_Q_A(l_switch,G1(:,:,:,80),Q(:,63),MB,G2tensor(:,72))
  call loop_QV_A(G1(:,:,:,77),wf(:,181),G1(:,:,:,81))
  call check_last_Q_A(l_switch,G1(:,:,:,81),Q(:,63),MB,G2tensor(:,73))
  call loop_QV_A(G1(:,:,:,77),wf(:,186),G1(:,:,:,82))
  call check_last_Q_A(l_switch,G1(:,:,:,82),Q(:,63),MB,G2tensor(:,74))
  call loop_QV_A(G1(:,:,:,77),wf(:,190),G1(:,:,:,83))
  call check_last_Q_A(l_switch,G1(:,:,:,83),Q(:,63),MB,G2tensor(:,75))
  call loop_QV_A(G0(:,:,:,1),wf(:,48),G0(:,:,:,23))
  call loop_Q_A(G0(:,:,:,23),Q(:,38),MB,G1(:,:,:,84))
  call loop_QV_A(G1(:,:,:,84),wf(:,43),G1(:,:,:,85))
  call check_last_Q_A(l_switch,G1(:,:,:,85),Q(:,63),MB,G2tensor(:,76))
  call loop_QV_A(G1(:,:,:,84),wf(:,45),G1(:,:,:,86))
  call check_last_Q_A(l_switch,G1(:,:,:,86),Q(:,63),MB,G2tensor(:,77))
  call loop_QV_A(G1(:,:,:,84),wf(:,46),G1(:,:,:,87))
  call check_last_Q_A(l_switch,G1(:,:,:,87),Q(:,63),MB,G2tensor(:,78))
  call loop_QV_A(G1(:,:,:,84),wf(:,181),G1(:,:,:,88))
  call check_last_Q_A(l_switch,G1(:,:,:,88),Q(:,63),MB,G2tensor(:,79))
  call loop_QV_A(G1(:,:,:,84),wf(:,186),G1(:,:,:,89))
  call check_last_Q_A(l_switch,G1(:,:,:,89),Q(:,63),MB,G2tensor(:,80))
  call loop_QV_A(G1(:,:,:,84),wf(:,190),G1(:,:,:,90))
  call check_last_Q_A(l_switch,G1(:,:,:,90),Q(:,63),MB,G2tensor(:,81))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,44),Q(:,38),G1(:,:,:,91))
  call check_last_CV_D(l_switch,G1(:,:,:,91),Q(:,38),wf(:,43),Q(:,25),G2tensor(:,82))
  call check_last_CV_D(l_switch,G1(:,:,:,91),Q(:,38),wf(:,45),Q(:,25),G2tensor(:,83))
  call check_last_CV_D(l_switch,G1(:,:,:,91),Q(:,38),wf(:,46),Q(:,25),G2tensor(:,84))
  call check_last_CV_D(l_switch,G1(:,:,:,91),Q(:,38),wf(:,181),Q(:,25),G2tensor(:,85))
  call check_last_CV_D(l_switch,G1(:,:,:,91),Q(:,38),wf(:,186),Q(:,25),G2tensor(:,86))
  call check_last_CV_D(l_switch,G1(:,:,:,91),Q(:,38),wf(:,190),Q(:,25),G2tensor(:,87))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,47),Q(:,38),G1(:,:,:,92))
  call check_last_CV_D(l_switch,G1(:,:,:,92),Q(:,38),wf(:,43),Q(:,25),G2tensor(:,88))
  call check_last_CV_D(l_switch,G1(:,:,:,92),Q(:,38),wf(:,45),Q(:,25),G2tensor(:,89))
  call check_last_CV_D(l_switch,G1(:,:,:,92),Q(:,38),wf(:,46),Q(:,25),G2tensor(:,90))
  call check_last_CV_D(l_switch,G1(:,:,:,92),Q(:,38),wf(:,181),Q(:,25),G2tensor(:,91))
  call check_last_CV_D(l_switch,G1(:,:,:,92),Q(:,38),wf(:,186),Q(:,25),G2tensor(:,92))
  call check_last_CV_D(l_switch,G1(:,:,:,92),Q(:,38),wf(:,190),Q(:,25),G2tensor(:,93))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,48),Q(:,38),G1(:,:,:,93))
  call check_last_CV_D(l_switch,G1(:,:,:,93),Q(:,38),wf(:,43),Q(:,25),G2tensor(:,94))
  call check_last_CV_D(l_switch,G1(:,:,:,93),Q(:,38),wf(:,45),Q(:,25),G2tensor(:,95))
  call check_last_CV_D(l_switch,G1(:,:,:,93),Q(:,38),wf(:,46),Q(:,25),G2tensor(:,96))
  call check_last_CV_D(l_switch,G1(:,:,:,93),Q(:,38),wf(:,181),Q(:,25),G2tensor(:,97))
  call check_last_CV_D(l_switch,G1(:,:,:,93),Q(:,38),wf(:,186),Q(:,25),G2tensor(:,98))
  call check_last_CV_D(l_switch,G1(:,:,:,93),Q(:,38),wf(:,190),Q(:,25),G2tensor(:,99))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,988),Q(:,54),G1(:,:,:,94))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,94),wf(:,-3),wf(:,0),G1tensor(:,95))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,94),wf(:,0),wf(:,-3),G1tensor(:,96))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,94),wf(:,-3),wf(:,0),G1tensor(:,97))
  call check_last_UV_W(l_switch,G1(:,:,:,94),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,100))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,989),Q(:,54),G1(:,:,:,95))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,95),wf(:,-3),wf(:,0),G1tensor(:,98))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,95),wf(:,0),wf(:,-3),G1tensor(:,99))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,95),wf(:,-3),wf(:,0),G1tensor(:,100))
  call check_last_UV_W(l_switch,G1(:,:,:,95),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,101))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,990),Q(:,54),G1(:,:,:,96))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,96),wf(:,-3),wf(:,0),G1tensor(:,101))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,96),wf(:,0),wf(:,-3),G1tensor(:,102))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,96),wf(:,-3),wf(:,0),G1tensor(:,103))
  call check_last_UV_W(l_switch,G1(:,:,:,96),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,102))
  call loop_QV_A(G0(:,:,:,1),wf(:,49),G0(:,:,:,24))
  call loop_Q_A(G0(:,:,:,24),Q(:,41),ZERO,G1(:,:,:,97))
  call loop_QV_A(G1(:,:,:,97),wf(:,50),G1(:,:,:,98))
  call check_last_Q_A(l_switch,G1(:,:,:,98),Q(:,63),ZERO,G2tensor(:,103))
  call loop_QV_A(G1(:,:,:,97),wf(:,53),G1(:,:,:,99))
  call check_last_Q_A(l_switch,G1(:,:,:,99),Q(:,63),ZERO,G2tensor(:,104))
  call loop_QV_A(G1(:,:,:,97),wf(:,54),G1(:,:,:,100))
  call check_last_Q_A(l_switch,G1(:,:,:,100),Q(:,63),ZERO,G2tensor(:,105))
  call loop_QV_A(G1(:,:,:,97),wf(:,206),G1(:,:,:,101))
  call check_last_Q_A(l_switch,G1(:,:,:,101),Q(:,63),ZERO,G2tensor(:,106))
  call loop_QV_A(G1(:,:,:,97),wf(:,225),G1(:,:,:,102))
  call check_last_Q_A(l_switch,G1(:,:,:,102),Q(:,63),ZERO,G2tensor(:,107))
  call loop_QV_A(G1(:,:,:,97),wf(:,229),G1(:,:,:,103))
  call check_last_Q_A(l_switch,G1(:,:,:,103),Q(:,63),ZERO,G2tensor(:,108))
  call loop_QV_A(G0(:,:,:,1),wf(:,51),G0(:,:,:,25))
  call loop_Q_A(G0(:,:,:,25),Q(:,41),ZERO,G1(:,:,:,104))
  call loop_QV_A(G1(:,:,:,104),wf(:,50),G1(:,:,:,105))
  call check_last_Q_A(l_switch,G1(:,:,:,105),Q(:,63),ZERO,G2tensor(:,109))
  call loop_QV_A(G1(:,:,:,104),wf(:,53),G1(:,:,:,106))
  call check_last_Q_A(l_switch,G1(:,:,:,106),Q(:,63),ZERO,G2tensor(:,110))
  call loop_QV_A(G1(:,:,:,104),wf(:,54),G1(:,:,:,107))
  call check_last_Q_A(l_switch,G1(:,:,:,107),Q(:,63),ZERO,G2tensor(:,111))
  call loop_QV_A(G1(:,:,:,104),wf(:,206),G1(:,:,:,108))
  call check_last_Q_A(l_switch,G1(:,:,:,108),Q(:,63),ZERO,G2tensor(:,112))
  call loop_QV_A(G1(:,:,:,104),wf(:,225),G1(:,:,:,109))
  call check_last_Q_A(l_switch,G1(:,:,:,109),Q(:,63),ZERO,G2tensor(:,113))
  call loop_QV_A(G1(:,:,:,104),wf(:,229),G1(:,:,:,110))
  call check_last_Q_A(l_switch,G1(:,:,:,110),Q(:,63),ZERO,G2tensor(:,114))
  call loop_QV_A(G0(:,:,:,1),wf(:,52),G0(:,:,:,26))
  call loop_Q_A(G0(:,:,:,26),Q(:,41),ZERO,G1(:,:,:,111))
  call loop_QV_A(G1(:,:,:,111),wf(:,50),G1(:,:,:,112))
  call check_last_Q_A(l_switch,G1(:,:,:,112),Q(:,63),ZERO,G2tensor(:,115))
  call loop_QV_A(G1(:,:,:,111),wf(:,53),G1(:,:,:,113))
  call check_last_Q_A(l_switch,G1(:,:,:,113),Q(:,63),ZERO,G2tensor(:,116))
  call loop_QV_A(G1(:,:,:,111),wf(:,54),G1(:,:,:,114))
  call check_last_Q_A(l_switch,G1(:,:,:,114),Q(:,63),ZERO,G2tensor(:,117))
  call loop_QV_A(G1(:,:,:,111),wf(:,206),G1(:,:,:,115))
  call check_last_Q_A(l_switch,G1(:,:,:,115),Q(:,63),ZERO,G2tensor(:,118))
  call loop_QV_A(G1(:,:,:,111),wf(:,225),G1(:,:,:,116))
  call check_last_Q_A(l_switch,G1(:,:,:,116),Q(:,63),ZERO,G2tensor(:,119))
  call loop_QV_A(G1(:,:,:,111),wf(:,229),G1(:,:,:,117))
  call check_last_Q_A(l_switch,G1(:,:,:,117),Q(:,63),ZERO,G2tensor(:,120))
  call loop_QV_A(G0(:,:,:,1),wf(:,49),G0(:,:,:,27))
  call loop_Q_A(G0(:,:,:,27),Q(:,41),MT,G1(:,:,:,118))
  call loop_QV_A(G1(:,:,:,118),wf(:,50),G1(:,:,:,119))
  call check_last_Q_A(l_switch,G1(:,:,:,119),Q(:,63),MT,G2tensor(:,121))
  call loop_QV_A(G1(:,:,:,118),wf(:,53),G1(:,:,:,120))
  call check_last_Q_A(l_switch,G1(:,:,:,120),Q(:,63),MT,G2tensor(:,122))
  call loop_QV_A(G1(:,:,:,118),wf(:,54),G1(:,:,:,121))
  call check_last_Q_A(l_switch,G1(:,:,:,121),Q(:,63),MT,G2tensor(:,123))
  call loop_QV_A(G1(:,:,:,118),wf(:,206),G1(:,:,:,122))
  call check_last_Q_A(l_switch,G1(:,:,:,122),Q(:,63),MT,G2tensor(:,124))
  call loop_QV_A(G1(:,:,:,118),wf(:,225),G1(:,:,:,123))
  call check_last_Q_A(l_switch,G1(:,:,:,123),Q(:,63),MT,G2tensor(:,125))
  call loop_QV_A(G1(:,:,:,118),wf(:,229),G1(:,:,:,124))
  call check_last_Q_A(l_switch,G1(:,:,:,124),Q(:,63),MT,G2tensor(:,126))
  call loop_QV_A(G0(:,:,:,1),wf(:,51),G0(:,:,:,28))
  call loop_Q_A(G0(:,:,:,28),Q(:,41),MT,G1(:,:,:,125))
  call loop_QV_A(G1(:,:,:,125),wf(:,50),G1(:,:,:,126))
  call check_last_Q_A(l_switch,G1(:,:,:,126),Q(:,63),MT,G2tensor(:,127))
  call loop_QV_A(G1(:,:,:,125),wf(:,53),G1(:,:,:,127))
  call check_last_Q_A(l_switch,G1(:,:,:,127),Q(:,63),MT,G2tensor(:,128))
  call loop_QV_A(G1(:,:,:,125),wf(:,54),G1(:,:,:,128))
  call check_last_Q_A(l_switch,G1(:,:,:,128),Q(:,63),MT,G2tensor(:,129))
  call loop_QV_A(G1(:,:,:,125),wf(:,206),G1(:,:,:,129))
  call check_last_Q_A(l_switch,G1(:,:,:,129),Q(:,63),MT,G2tensor(:,130))
  call loop_QV_A(G1(:,:,:,125),wf(:,225),G1(:,:,:,130))
  call check_last_Q_A(l_switch,G1(:,:,:,130),Q(:,63),MT,G2tensor(:,131))
  call loop_QV_A(G1(:,:,:,125),wf(:,229),G1(:,:,:,131))
  call check_last_Q_A(l_switch,G1(:,:,:,131),Q(:,63),MT,G2tensor(:,132))
  call loop_QV_A(G0(:,:,:,1),wf(:,52),G0(:,:,:,29))
  call loop_Q_A(G0(:,:,:,29),Q(:,41),MT,G1(:,:,:,132))
  call loop_QV_A(G1(:,:,:,132),wf(:,50),G1(:,:,:,133))
  call check_last_Q_A(l_switch,G1(:,:,:,133),Q(:,63),MT,G2tensor(:,133))
  call loop_QV_A(G1(:,:,:,132),wf(:,53),G1(:,:,:,134))
  call check_last_Q_A(l_switch,G1(:,:,:,134),Q(:,63),MT,G2tensor(:,134))
  call loop_QV_A(G1(:,:,:,132),wf(:,54),G1(:,:,:,135))
  call check_last_Q_A(l_switch,G1(:,:,:,135),Q(:,63),MT,G2tensor(:,135))
  call loop_QV_A(G1(:,:,:,132),wf(:,206),G1(:,:,:,136))
  call check_last_Q_A(l_switch,G1(:,:,:,136),Q(:,63),MT,G2tensor(:,136))
  call loop_QV_A(G1(:,:,:,132),wf(:,225),G1(:,:,:,137))
  call check_last_Q_A(l_switch,G1(:,:,:,137),Q(:,63),MT,G2tensor(:,137))
  call loop_QV_A(G1(:,:,:,132),wf(:,229),G1(:,:,:,138))
  call check_last_Q_A(l_switch,G1(:,:,:,138),Q(:,63),MT,G2tensor(:,138))
  call loop_QV_A(G0(:,:,:,1),wf(:,49),G0(:,:,:,30))
  call loop_Q_A(G0(:,:,:,30),Q(:,41),MB,G1(:,:,:,139))
  call loop_QV_A(G1(:,:,:,139),wf(:,50),G1(:,:,:,140))
  call check_last_Q_A(l_switch,G1(:,:,:,140),Q(:,63),MB,G2tensor(:,139))
  call loop_QV_A(G1(:,:,:,139),wf(:,53),G1(:,:,:,141))
  call check_last_Q_A(l_switch,G1(:,:,:,141),Q(:,63),MB,G2tensor(:,140))
  call loop_QV_A(G1(:,:,:,139),wf(:,54),G1(:,:,:,142))
  call check_last_Q_A(l_switch,G1(:,:,:,142),Q(:,63),MB,G2tensor(:,141))
  call loop_QV_A(G1(:,:,:,139),wf(:,206),G1(:,:,:,143))
  call check_last_Q_A(l_switch,G1(:,:,:,143),Q(:,63),MB,G2tensor(:,142))
  call loop_QV_A(G1(:,:,:,139),wf(:,225),G1(:,:,:,144))
  call check_last_Q_A(l_switch,G1(:,:,:,144),Q(:,63),MB,G2tensor(:,143))
  call loop_QV_A(G1(:,:,:,139),wf(:,229),G1(:,:,:,145))
  call check_last_Q_A(l_switch,G1(:,:,:,145),Q(:,63),MB,G2tensor(:,144))
  call loop_QV_A(G0(:,:,:,1),wf(:,51),G0(:,:,:,31))
  call loop_Q_A(G0(:,:,:,31),Q(:,41),MB,G1(:,:,:,146))
  call loop_QV_A(G1(:,:,:,146),wf(:,50),G1(:,:,:,147))
  call check_last_Q_A(l_switch,G1(:,:,:,147),Q(:,63),MB,G2tensor(:,145))
  call loop_QV_A(G1(:,:,:,146),wf(:,53),G1(:,:,:,148))
  call check_last_Q_A(l_switch,G1(:,:,:,148),Q(:,63),MB,G2tensor(:,146))
  call loop_QV_A(G1(:,:,:,146),wf(:,54),G1(:,:,:,149))
  call check_last_Q_A(l_switch,G1(:,:,:,149),Q(:,63),MB,G2tensor(:,147))
  call loop_QV_A(G1(:,:,:,146),wf(:,206),G1(:,:,:,150))
  call check_last_Q_A(l_switch,G1(:,:,:,150),Q(:,63),MB,G2tensor(:,148))
  call loop_QV_A(G1(:,:,:,146),wf(:,225),G1(:,:,:,151))
  call check_last_Q_A(l_switch,G1(:,:,:,151),Q(:,63),MB,G2tensor(:,149))
  call loop_QV_A(G1(:,:,:,146),wf(:,229),G1(:,:,:,152))
  call check_last_Q_A(l_switch,G1(:,:,:,152),Q(:,63),MB,G2tensor(:,150))
  call loop_QV_A(G0(:,:,:,1),wf(:,52),G0(:,:,:,32))
  call loop_Q_A(G0(:,:,:,32),Q(:,41),MB,G1(:,:,:,153))
  call loop_QV_A(G1(:,:,:,153),wf(:,50),G1(:,:,:,154))
  call check_last_Q_A(l_switch,G1(:,:,:,154),Q(:,63),MB,G2tensor(:,151))
  call loop_QV_A(G1(:,:,:,153),wf(:,53),G1(:,:,:,155))
  call check_last_Q_A(l_switch,G1(:,:,:,155),Q(:,63),MB,G2tensor(:,152))
  call loop_QV_A(G1(:,:,:,153),wf(:,54),G1(:,:,:,156))
  call check_last_Q_A(l_switch,G1(:,:,:,156),Q(:,63),MB,G2tensor(:,153))
  call loop_QV_A(G1(:,:,:,153),wf(:,206),G1(:,:,:,157))
  call check_last_Q_A(l_switch,G1(:,:,:,157),Q(:,63),MB,G2tensor(:,154))
  call loop_QV_A(G1(:,:,:,153),wf(:,225),G1(:,:,:,158))
  call check_last_Q_A(l_switch,G1(:,:,:,158),Q(:,63),MB,G2tensor(:,155))
  call loop_QV_A(G1(:,:,:,153),wf(:,229),G1(:,:,:,159))
  call check_last_Q_A(l_switch,G1(:,:,:,159),Q(:,63),MB,G2tensor(:,156))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,49),Q(:,41),G1(:,:,:,160))
  call check_last_CV_D(l_switch,G1(:,:,:,160),Q(:,41),wf(:,50),Q(:,22),G2tensor(:,157))
  call check_last_CV_D(l_switch,G1(:,:,:,160),Q(:,41),wf(:,53),Q(:,22),G2tensor(:,158))
  call check_last_CV_D(l_switch,G1(:,:,:,160),Q(:,41),wf(:,54),Q(:,22),G2tensor(:,159))
  call check_last_CV_D(l_switch,G1(:,:,:,160),Q(:,41),wf(:,206),Q(:,22),G2tensor(:,160))
  call check_last_CV_D(l_switch,G1(:,:,:,160),Q(:,41),wf(:,225),Q(:,22),G2tensor(:,161))
  call check_last_CV_D(l_switch,G1(:,:,:,160),Q(:,41),wf(:,229),Q(:,22),G2tensor(:,162))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,51),Q(:,41),G1(:,:,:,161))
  call check_last_CV_D(l_switch,G1(:,:,:,161),Q(:,41),wf(:,50),Q(:,22),G2tensor(:,163))
  call check_last_CV_D(l_switch,G1(:,:,:,161),Q(:,41),wf(:,53),Q(:,22),G2tensor(:,164))
  call check_last_CV_D(l_switch,G1(:,:,:,161),Q(:,41),wf(:,54),Q(:,22),G2tensor(:,165))
  call check_last_CV_D(l_switch,G1(:,:,:,161),Q(:,41),wf(:,206),Q(:,22),G2tensor(:,166))
  call check_last_CV_D(l_switch,G1(:,:,:,161),Q(:,41),wf(:,225),Q(:,22),G2tensor(:,167))
  call check_last_CV_D(l_switch,G1(:,:,:,161),Q(:,41),wf(:,229),Q(:,22),G2tensor(:,168))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,52),Q(:,41),G1(:,:,:,162))
  call check_last_CV_D(l_switch,G1(:,:,:,162),Q(:,41),wf(:,50),Q(:,22),G2tensor(:,169))
  call check_last_CV_D(l_switch,G1(:,:,:,162),Q(:,41),wf(:,53),Q(:,22),G2tensor(:,170))
  call check_last_CV_D(l_switch,G1(:,:,:,162),Q(:,41),wf(:,54),Q(:,22),G2tensor(:,171))
  call check_last_CV_D(l_switch,G1(:,:,:,162),Q(:,41),wf(:,206),Q(:,22),G2tensor(:,172))
  call check_last_CV_D(l_switch,G1(:,:,:,162),Q(:,41),wf(:,225),Q(:,22),G2tensor(:,173))
  call check_last_CV_D(l_switch,G1(:,:,:,162),Q(:,41),wf(:,229),Q(:,22),G2tensor(:,174))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,1012),G0(:,:,:,33))
  call check_last_UV_W(l_switch,G0(:,:,:,33),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,104))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,1012),wf(:,0),G0(:,:,:,34))
  call check_last_UV_W(l_switch,G0(:,:,:,34),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,105))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,1012),G0(:,:,:,35))
  call check_last_UV_W(l_switch,G0(:,:,:,35),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,106))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,1013),G0(:,:,:,36))
  call check_last_UV_W(l_switch,G0(:,:,:,36),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,107))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,1013),wf(:,0),G0(:,:,:,37))
  call check_last_UV_W(l_switch,G0(:,:,:,37),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,108))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,1013),G0(:,:,:,38))
  call check_last_UV_W(l_switch,G0(:,:,:,38),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,109))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,1014),G0(:,:,:,39))
  call check_last_UV_W(l_switch,G0(:,:,:,39),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,110))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,1014),wf(:,0),G0(:,:,:,40))
  call check_last_UV_W(l_switch,G0(:,:,:,40),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,111))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,1014),G0(:,:,:,41))
  call check_last_UV_W(l_switch,G0(:,:,:,41),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,112))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1012),Q(:,46),G1(:,:,:,163))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,163),wf(:,-4),wf(:,0),G1tensor(:,113))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,163),wf(:,0),wf(:,-4),G1tensor(:,114))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,163),wf(:,-4),wf(:,0),G1tensor(:,115))
  call check_last_UV_W(l_switch,G1(:,:,:,163),Q(:,46),wf(:,109),Q(:,17),G2tensor(:,175))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1013),Q(:,46),G1(:,:,:,164))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,164),wf(:,-4),wf(:,0),G1tensor(:,116))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,164),wf(:,0),wf(:,-4),G1tensor(:,117))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,164),wf(:,-4),wf(:,0),G1tensor(:,118))
  call check_last_UV_W(l_switch,G1(:,:,:,164),Q(:,46),wf(:,109),Q(:,17),G2tensor(:,176))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1014),Q(:,46),G1(:,:,:,165))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,165),wf(:,-4),wf(:,0),G1tensor(:,119))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,165),wf(:,0),wf(:,-4),G1tensor(:,120))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,165),wf(:,-4),wf(:,0),G1tensor(:,121))
  call check_last_UV_W(l_switch,G1(:,:,:,165),Q(:,46),wf(:,109),Q(:,17),G2tensor(:,177))
  call loop_QV_A(G0(:,:,:,1),wf(:,55),G0(:,:,:,42))
  call loop_Q_A(G0(:,:,:,42),Q(:,49),ZERO,G1(:,:,:,166))
  call loop_QV_A(G1(:,:,:,166),wf(:,56),G1(:,:,:,167))
  call check_last_Q_A(l_switch,G1(:,:,:,167),Q(:,63),ZERO,G2tensor(:,178))
  call loop_QV_A(G1(:,:,:,166),wf(:,59),G1(:,:,:,168))
  call check_last_Q_A(l_switch,G1(:,:,:,168),Q(:,63),ZERO,G2tensor(:,179))
  call loop_QV_A(G1(:,:,:,166),wf(:,60),G1(:,:,:,169))
  call check_last_Q_A(l_switch,G1(:,:,:,169),Q(:,63),ZERO,G2tensor(:,180))
  call loop_QV_A(G1(:,:,:,166),wf(:,202),G1(:,:,:,170))
  call check_last_Q_A(l_switch,G1(:,:,:,170),Q(:,63),ZERO,G2tensor(:,181))
  call loop_QV_A(G1(:,:,:,166),wf(:,214),G1(:,:,:,171))
  call check_last_Q_A(l_switch,G1(:,:,:,171),Q(:,63),ZERO,G2tensor(:,182))
  call loop_QV_A(G1(:,:,:,166),wf(:,221),G1(:,:,:,172))
  call check_last_Q_A(l_switch,G1(:,:,:,172),Q(:,63),ZERO,G2tensor(:,183))
  call loop_QV_A(G0(:,:,:,1),wf(:,57),G0(:,:,:,43))
  call loop_Q_A(G0(:,:,:,43),Q(:,49),ZERO,G1(:,:,:,173))
  call loop_QV_A(G1(:,:,:,173),wf(:,56),G1(:,:,:,174))
  call check_last_Q_A(l_switch,G1(:,:,:,174),Q(:,63),ZERO,G2tensor(:,184))
  call loop_QV_A(G1(:,:,:,173),wf(:,59),G1(:,:,:,175))
  call check_last_Q_A(l_switch,G1(:,:,:,175),Q(:,63),ZERO,G2tensor(:,185))
  call loop_QV_A(G1(:,:,:,173),wf(:,60),G1(:,:,:,176))
  call check_last_Q_A(l_switch,G1(:,:,:,176),Q(:,63),ZERO,G2tensor(:,186))
  call loop_QV_A(G1(:,:,:,173),wf(:,202),G1(:,:,:,177))
  call check_last_Q_A(l_switch,G1(:,:,:,177),Q(:,63),ZERO,G2tensor(:,187))
  call loop_QV_A(G1(:,:,:,173),wf(:,214),G1(:,:,:,178))
  call check_last_Q_A(l_switch,G1(:,:,:,178),Q(:,63),ZERO,G2tensor(:,188))
  call loop_QV_A(G1(:,:,:,173),wf(:,221),G1(:,:,:,179))
  call check_last_Q_A(l_switch,G1(:,:,:,179),Q(:,63),ZERO,G2tensor(:,189))
  call loop_QV_A(G0(:,:,:,1),wf(:,58),G0(:,:,:,44))
  call loop_Q_A(G0(:,:,:,44),Q(:,49),ZERO,G1(:,:,:,180))
  call loop_QV_A(G1(:,:,:,180),wf(:,56),G1(:,:,:,181))
  call check_last_Q_A(l_switch,G1(:,:,:,181),Q(:,63),ZERO,G2tensor(:,190))
  call loop_QV_A(G1(:,:,:,180),wf(:,59),G1(:,:,:,182))
  call check_last_Q_A(l_switch,G1(:,:,:,182),Q(:,63),ZERO,G2tensor(:,191))
  call loop_QV_A(G1(:,:,:,180),wf(:,60),G1(:,:,:,183))
  call check_last_Q_A(l_switch,G1(:,:,:,183),Q(:,63),ZERO,G2tensor(:,192))
  call loop_QV_A(G1(:,:,:,180),wf(:,202),G1(:,:,:,184))
  call check_last_Q_A(l_switch,G1(:,:,:,184),Q(:,63),ZERO,G2tensor(:,193))
  call loop_QV_A(G1(:,:,:,180),wf(:,214),G1(:,:,:,185))
  call check_last_Q_A(l_switch,G1(:,:,:,185),Q(:,63),ZERO,G2tensor(:,194))
  call loop_QV_A(G1(:,:,:,180),wf(:,221),G1(:,:,:,186))
  call check_last_Q_A(l_switch,G1(:,:,:,186),Q(:,63),ZERO,G2tensor(:,195))
  call loop_QV_A(G0(:,:,:,1),wf(:,55),G0(:,:,:,45))
  call loop_Q_A(G0(:,:,:,45),Q(:,49),MT,G1(:,:,:,187))
  call loop_QV_A(G1(:,:,:,187),wf(:,56),G1(:,:,:,188))
  call check_last_Q_A(l_switch,G1(:,:,:,188),Q(:,63),MT,G2tensor(:,196))
  call loop_QV_A(G1(:,:,:,187),wf(:,59),G1(:,:,:,189))
  call check_last_Q_A(l_switch,G1(:,:,:,189),Q(:,63),MT,G2tensor(:,197))
  call loop_QV_A(G1(:,:,:,187),wf(:,60),G1(:,:,:,190))
  call check_last_Q_A(l_switch,G1(:,:,:,190),Q(:,63),MT,G2tensor(:,198))
  call loop_QV_A(G1(:,:,:,187),wf(:,202),G1(:,:,:,191))
  call check_last_Q_A(l_switch,G1(:,:,:,191),Q(:,63),MT,G2tensor(:,199))
  call loop_QV_A(G1(:,:,:,187),wf(:,214),G1(:,:,:,192))
  call check_last_Q_A(l_switch,G1(:,:,:,192),Q(:,63),MT,G2tensor(:,200))
  call loop_QV_A(G1(:,:,:,187),wf(:,221),G1(:,:,:,193))
  call check_last_Q_A(l_switch,G1(:,:,:,193),Q(:,63),MT,G2tensor(:,201))
  call loop_QV_A(G0(:,:,:,1),wf(:,57),G0(:,:,:,46))
  call loop_Q_A(G0(:,:,:,46),Q(:,49),MT,G1(:,:,:,194))
  call loop_QV_A(G1(:,:,:,194),wf(:,56),G1(:,:,:,195))
  call check_last_Q_A(l_switch,G1(:,:,:,195),Q(:,63),MT,G2tensor(:,202))
  call loop_QV_A(G1(:,:,:,194),wf(:,59),G1(:,:,:,196))
  call check_last_Q_A(l_switch,G1(:,:,:,196),Q(:,63),MT,G2tensor(:,203))
  call loop_QV_A(G1(:,:,:,194),wf(:,60),G1(:,:,:,197))
  call check_last_Q_A(l_switch,G1(:,:,:,197),Q(:,63),MT,G2tensor(:,204))
  call loop_QV_A(G1(:,:,:,194),wf(:,202),G1(:,:,:,198))
  call check_last_Q_A(l_switch,G1(:,:,:,198),Q(:,63),MT,G2tensor(:,205))
  call loop_QV_A(G1(:,:,:,194),wf(:,214),G1(:,:,:,199))
  call check_last_Q_A(l_switch,G1(:,:,:,199),Q(:,63),MT,G2tensor(:,206))
  call loop_QV_A(G1(:,:,:,194),wf(:,221),G1(:,:,:,200))
  call check_last_Q_A(l_switch,G1(:,:,:,200),Q(:,63),MT,G2tensor(:,207))
  call loop_QV_A(G0(:,:,:,1),wf(:,58),G0(:,:,:,47))
  call loop_Q_A(G0(:,:,:,47),Q(:,49),MT,G1(:,:,:,201))
  call loop_QV_A(G1(:,:,:,201),wf(:,56),G1(:,:,:,202))
  call check_last_Q_A(l_switch,G1(:,:,:,202),Q(:,63),MT,G2tensor(:,208))
  call loop_QV_A(G1(:,:,:,201),wf(:,59),G1(:,:,:,203))
  call check_last_Q_A(l_switch,G1(:,:,:,203),Q(:,63),MT,G2tensor(:,209))
  call loop_QV_A(G1(:,:,:,201),wf(:,60),G1(:,:,:,204))
  call check_last_Q_A(l_switch,G1(:,:,:,204),Q(:,63),MT,G2tensor(:,210))
  call loop_QV_A(G1(:,:,:,201),wf(:,202),G1(:,:,:,205))
  call check_last_Q_A(l_switch,G1(:,:,:,205),Q(:,63),MT,G2tensor(:,211))
  call loop_QV_A(G1(:,:,:,201),wf(:,214),G1(:,:,:,206))
  call check_last_Q_A(l_switch,G1(:,:,:,206),Q(:,63),MT,G2tensor(:,212))
  call loop_QV_A(G1(:,:,:,201),wf(:,221),G1(:,:,:,207))
  call check_last_Q_A(l_switch,G1(:,:,:,207),Q(:,63),MT,G2tensor(:,213))
  call loop_QV_A(G0(:,:,:,1),wf(:,55),G0(:,:,:,48))
  call loop_Q_A(G0(:,:,:,48),Q(:,49),MB,G1(:,:,:,208))
  call loop_QV_A(G1(:,:,:,208),wf(:,56),G1(:,:,:,209))
  call check_last_Q_A(l_switch,G1(:,:,:,209),Q(:,63),MB,G2tensor(:,214))
  call loop_QV_A(G1(:,:,:,208),wf(:,59),G1(:,:,:,210))
  call check_last_Q_A(l_switch,G1(:,:,:,210),Q(:,63),MB,G2tensor(:,215))
  call loop_QV_A(G1(:,:,:,208),wf(:,60),G1(:,:,:,211))
  call check_last_Q_A(l_switch,G1(:,:,:,211),Q(:,63),MB,G2tensor(:,216))
  call loop_QV_A(G1(:,:,:,208),wf(:,202),G1(:,:,:,212))
  call check_last_Q_A(l_switch,G1(:,:,:,212),Q(:,63),MB,G2tensor(:,217))
  call loop_QV_A(G1(:,:,:,208),wf(:,214),G1(:,:,:,213))
  call check_last_Q_A(l_switch,G1(:,:,:,213),Q(:,63),MB,G2tensor(:,218))
  call loop_QV_A(G1(:,:,:,208),wf(:,221),G1(:,:,:,214))
  call check_last_Q_A(l_switch,G1(:,:,:,214),Q(:,63),MB,G2tensor(:,219))
  call loop_QV_A(G0(:,:,:,1),wf(:,57),G0(:,:,:,49))
  call loop_Q_A(G0(:,:,:,49),Q(:,49),MB,G1(:,:,:,215))
  call loop_QV_A(G1(:,:,:,215),wf(:,56),G1(:,:,:,216))
  call check_last_Q_A(l_switch,G1(:,:,:,216),Q(:,63),MB,G2tensor(:,220))
  call loop_QV_A(G1(:,:,:,215),wf(:,59),G1(:,:,:,217))
  call check_last_Q_A(l_switch,G1(:,:,:,217),Q(:,63),MB,G2tensor(:,221))
  call loop_QV_A(G1(:,:,:,215),wf(:,60),G1(:,:,:,218))
  call check_last_Q_A(l_switch,G1(:,:,:,218),Q(:,63),MB,G2tensor(:,222))
  call loop_QV_A(G1(:,:,:,215),wf(:,202),G1(:,:,:,219))
  call check_last_Q_A(l_switch,G1(:,:,:,219),Q(:,63),MB,G2tensor(:,223))
  call loop_QV_A(G1(:,:,:,215),wf(:,214),G1(:,:,:,220))
  call check_last_Q_A(l_switch,G1(:,:,:,220),Q(:,63),MB,G2tensor(:,224))
  call loop_QV_A(G1(:,:,:,215),wf(:,221),G1(:,:,:,221))
  call check_last_Q_A(l_switch,G1(:,:,:,221),Q(:,63),MB,G2tensor(:,225))
  call loop_QV_A(G0(:,:,:,1),wf(:,58),G0(:,:,:,50))
  call loop_Q_A(G0(:,:,:,50),Q(:,49),MB,G1(:,:,:,222))
  call loop_QV_A(G1(:,:,:,222),wf(:,56),G1(:,:,:,223))
  call check_last_Q_A(l_switch,G1(:,:,:,223),Q(:,63),MB,G2tensor(:,226))
  call loop_QV_A(G1(:,:,:,222),wf(:,59),G1(:,:,:,224))
  call check_last_Q_A(l_switch,G1(:,:,:,224),Q(:,63),MB,G2tensor(:,227))
  call loop_QV_A(G1(:,:,:,222),wf(:,60),G1(:,:,:,225))
  call check_last_Q_A(l_switch,G1(:,:,:,225),Q(:,63),MB,G2tensor(:,228))
  call loop_QV_A(G1(:,:,:,222),wf(:,202),G1(:,:,:,226))
  call check_last_Q_A(l_switch,G1(:,:,:,226),Q(:,63),MB,G2tensor(:,229))
  call loop_QV_A(G1(:,:,:,222),wf(:,214),G1(:,:,:,227))
  call check_last_Q_A(l_switch,G1(:,:,:,227),Q(:,63),MB,G2tensor(:,230))
  call loop_QV_A(G1(:,:,:,222),wf(:,221),G1(:,:,:,228))
  call check_last_Q_A(l_switch,G1(:,:,:,228),Q(:,63),MB,G2tensor(:,231))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,55),Q(:,49),G1(:,:,:,229))
  call check_last_CV_D(l_switch,G1(:,:,:,229),Q(:,49),wf(:,56),Q(:,14),G2tensor(:,232))
  call check_last_CV_D(l_switch,G1(:,:,:,229),Q(:,49),wf(:,59),Q(:,14),G2tensor(:,233))
  call check_last_CV_D(l_switch,G1(:,:,:,229),Q(:,49),wf(:,60),Q(:,14),G2tensor(:,234))
  call check_last_CV_D(l_switch,G1(:,:,:,229),Q(:,49),wf(:,202),Q(:,14),G2tensor(:,235))
  call check_last_CV_D(l_switch,G1(:,:,:,229),Q(:,49),wf(:,214),Q(:,14),G2tensor(:,236))
  call check_last_CV_D(l_switch,G1(:,:,:,229),Q(:,49),wf(:,221),Q(:,14),G2tensor(:,237))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,57),Q(:,49),G1(:,:,:,230))
  call check_last_CV_D(l_switch,G1(:,:,:,230),Q(:,49),wf(:,56),Q(:,14),G2tensor(:,238))
  call check_last_CV_D(l_switch,G1(:,:,:,230),Q(:,49),wf(:,59),Q(:,14),G2tensor(:,239))
  call check_last_CV_D(l_switch,G1(:,:,:,230),Q(:,49),wf(:,60),Q(:,14),G2tensor(:,240))
  call check_last_CV_D(l_switch,G1(:,:,:,230),Q(:,49),wf(:,202),Q(:,14),G2tensor(:,241))
  call check_last_CV_D(l_switch,G1(:,:,:,230),Q(:,49),wf(:,214),Q(:,14),G2tensor(:,242))
  call check_last_CV_D(l_switch,G1(:,:,:,230),Q(:,49),wf(:,221),Q(:,14),G2tensor(:,243))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,58),Q(:,49),G1(:,:,:,231))
  call check_last_CV_D(l_switch,G1(:,:,:,231),Q(:,49),wf(:,56),Q(:,14),G2tensor(:,244))
  call check_last_CV_D(l_switch,G1(:,:,:,231),Q(:,49),wf(:,59),Q(:,14),G2tensor(:,245))
  call check_last_CV_D(l_switch,G1(:,:,:,231),Q(:,49),wf(:,60),Q(:,14),G2tensor(:,246))
  call check_last_CV_D(l_switch,G1(:,:,:,231),Q(:,49),wf(:,202),Q(:,14),G2tensor(:,247))
  call check_last_CV_D(l_switch,G1(:,:,:,231),Q(:,49),wf(:,214),Q(:,14),G2tensor(:,248))
  call check_last_CV_D(l_switch,G1(:,:,:,231),Q(:,49),wf(:,221),Q(:,14),G2tensor(:,249))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,62),wf(:,89),G0(:,:,:,51))
  call check_last_UV_W(l_switch,G0(:,:,:,51),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,122))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,89),wf(:,62),G0(:,:,:,52))
  call check_last_UV_W(l_switch,G0(:,:,:,52),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,123))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,62),wf(:,89),G0(:,:,:,53))
  call check_last_UV_W(l_switch,G0(:,:,:,53),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,124))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,61),wf(:,254),G0(:,:,:,54))
  call check_last_UV_W(l_switch,G0(:,:,:,54),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,125))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,254),wf(:,61),G0(:,:,:,55))
  call check_last_UV_W(l_switch,G0(:,:,:,55),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,126))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,61),wf(:,254),G0(:,:,:,56))
  call check_last_UV_W(l_switch,G0(:,:,:,56),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,127))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,271),G0(:,:,:,57))
  call check_last_UV_W(l_switch,G0(:,:,:,57),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,128))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,271),wf(:,-5),G0(:,:,:,58))
  call check_last_UV_W(l_switch,G0(:,:,:,58),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,129))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,271),G0(:,:,:,59))
  call check_last_UV_W(l_switch,G0(:,:,:,59),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,130))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,89),Q(:,35),G1(:,:,:,232))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,232),wf(:,-4),wf(:,62),G1tensor(:,131))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,232),wf(:,62),wf(:,-4),G1tensor(:,132))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,232),wf(:,-4),wf(:,62),G1tensor(:,133))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,232),wf(:,-3),wf(:,66),G1tensor(:,134))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,232),wf(:,66),wf(:,-3),G1tensor(:,135))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,232),wf(:,-3),wf(:,66),G1tensor(:,136))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,232),wf(:,-2),wf(:,75),G1tensor(:,137))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,232),wf(:,75),wf(:,-2),G1tensor(:,138))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,232),wf(:,-2),wf(:,75),G1tensor(:,139))
  call check_last_UV_W(l_switch,G1(:,:,:,232),Q(:,35),wf(:,20),Q(:,28),G2tensor(:,250))
  call check_last_UV_W(l_switch,G1(:,:,:,232),Q(:,35),wf(:,23),Q(:,28),G2tensor(:,251))
  call check_last_UV_W(l_switch,G1(:,:,:,232),Q(:,35),wf(:,24),Q(:,28),G2tensor(:,252))
  call check_last_UV_W(l_switch,G1(:,:,:,232),Q(:,35),wf(:,253),Q(:,28),G2tensor(:,253))
  call check_last_UV_W(l_switch,G1(:,:,:,232),Q(:,35),wf(:,258),Q(:,28),G2tensor(:,254))
  call check_last_UV_W(l_switch,G1(:,:,:,232),Q(:,35),wf(:,262),Q(:,28),G2tensor(:,255))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,254),Q(:,44),G1(:,:,:,233))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,233),wf(:,-4),wf(:,61),G1tensor(:,140))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,233),wf(:,61),wf(:,-4),G1tensor(:,141))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,233),wf(:,-4),wf(:,61),G1tensor(:,142))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,233),wf(:,-1),wf(:,109),G1tensor(:,143))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,233),wf(:,109),wf(:,-1),G1tensor(:,144))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,233),wf(:,-1),wf(:,109),G1tensor(:,145))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,233),wf(:,0),wf(:,95),G1tensor(:,146))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,233),wf(:,95),wf(:,0),G1tensor(:,147))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,233),wf(:,0),wf(:,95),G1tensor(:,148))
  call check_last_UV_W(l_switch,G1(:,:,:,233),Q(:,44),wf(:,13),Q(:,19),G2tensor(:,256))
  call check_last_UV_W(l_switch,G1(:,:,:,233),Q(:,44),wf(:,15),Q(:,19),G2tensor(:,257))
  call check_last_UV_W(l_switch,G1(:,:,:,233),Q(:,44),wf(:,16),Q(:,19),G2tensor(:,258))
  call check_last_UV_W(l_switch,G1(:,:,:,233),Q(:,44),wf(:,88),Q(:,19),G2tensor(:,259))
  call check_last_UV_W(l_switch,G1(:,:,:,233),Q(:,44),wf(:,135),Q(:,19),G2tensor(:,260))
  call check_last_UV_W(l_switch,G1(:,:,:,233),Q(:,44),wf(:,139),Q(:,19),G2tensor(:,261))
  call loop_QV_A(G0(:,:,:,1),wf(:,782),G0(:,:,:,60))
  call loop_Q_A(G0(:,:,:,60),Q(:,60),ZERO,G1(:,:,:,234))
  call loop_QV_A(G1(:,:,:,234),wf(:,61),G1(:,:,:,235))
  call check_last_Q_A(l_switch,G1(:,:,:,235),Q(:,63),ZERO,G2tensor(:,262))
  call loop_QV_A(G0(:,:,:,1),wf(:,784),G0(:,:,:,61))
  call loop_Q_A(G0(:,:,:,61),Q(:,60),ZERO,G1(:,:,:,236))
  call loop_QV_A(G1(:,:,:,236),wf(:,61),G1(:,:,:,237))
  call check_last_Q_A(l_switch,G1(:,:,:,237),Q(:,63),ZERO,G2tensor(:,263))
  call loop_QV_A(G0(:,:,:,1),wf(:,785),G0(:,:,:,62))
  call loop_Q_A(G0(:,:,:,62),Q(:,60),ZERO,G1(:,:,:,238))
  call loop_QV_A(G1(:,:,:,238),wf(:,61),G1(:,:,:,239))
  call check_last_Q_A(l_switch,G1(:,:,:,239),Q(:,63),ZERO,G2tensor(:,264))
  call loop_QV_A(G0(:,:,:,1),wf(:,782),G0(:,:,:,63))
  call loop_Q_A(G0(:,:,:,63),Q(:,60),MT,G1(:,:,:,240))
  call loop_QV_A(G1(:,:,:,240),wf(:,61),G1(:,:,:,241))
  call check_last_Q_A(l_switch,G1(:,:,:,241),Q(:,63),MT,G2tensor(:,265))
  call loop_QV_A(G0(:,:,:,1),wf(:,784),G0(:,:,:,64))
  call loop_Q_A(G0(:,:,:,64),Q(:,60),MT,G1(:,:,:,242))
  call loop_QV_A(G1(:,:,:,242),wf(:,61),G1(:,:,:,243))
  call check_last_Q_A(l_switch,G1(:,:,:,243),Q(:,63),MT,G2tensor(:,266))
  call loop_QV_A(G0(:,:,:,1),wf(:,785),G0(:,:,:,65))
  call loop_Q_A(G0(:,:,:,65),Q(:,60),MT,G1(:,:,:,244))
  call loop_QV_A(G1(:,:,:,244),wf(:,61),G1(:,:,:,245))
  call check_last_Q_A(l_switch,G1(:,:,:,245),Q(:,63),MT,G2tensor(:,267))
  call loop_QV_A(G0(:,:,:,1),wf(:,782),G0(:,:,:,66))
  call loop_Q_A(G0(:,:,:,66),Q(:,60),MB,G1(:,:,:,246))
  call loop_QV_A(G1(:,:,:,246),wf(:,61),G1(:,:,:,247))
  call check_last_Q_A(l_switch,G1(:,:,:,247),Q(:,63),MB,G2tensor(:,268))
  call loop_QV_A(G0(:,:,:,1),wf(:,784),G0(:,:,:,67))
  call loop_Q_A(G0(:,:,:,67),Q(:,60),MB,G1(:,:,:,248))
  call loop_QV_A(G1(:,:,:,248),wf(:,61),G1(:,:,:,249))
  call check_last_Q_A(l_switch,G1(:,:,:,249),Q(:,63),MB,G2tensor(:,269))
  call loop_QV_A(G0(:,:,:,1),wf(:,785),G0(:,:,:,68))
  call loop_Q_A(G0(:,:,:,68),Q(:,60),MB,G1(:,:,:,250))
  call loop_QV_A(G1(:,:,:,250),wf(:,61),G1(:,:,:,251))
  call check_last_Q_A(l_switch,G1(:,:,:,251),Q(:,63),MB,G2tensor(:,270))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,782),Q(:,60),G1(:,:,:,252))
  call check_last_CV_D(l_switch,G1(:,:,:,252),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,271))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,784),Q(:,60),G1(:,:,:,253))
  call check_last_CV_D(l_switch,G1(:,:,:,253),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,272))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,785),Q(:,60),G1(:,:,:,254))
  call check_last_CV_D(l_switch,G1(:,:,:,254),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,273))
  call loop_QV_A(G0(:,:,:,1),wf(:,63),G0(:,:,:,69))
  call loop_Q_A(G0(:,:,:,69),Q(:,51),ZERO,G1(:,:,:,255))
  call loop_QV_A(G1(:,:,:,255),wf(:,62),G1(:,:,:,256))
  call check_last_Q_A(l_switch,G1(:,:,:,256),Q(:,63),ZERO,G2tensor(:,274))
  call loop_QV_A(G0(:,:,:,1),wf(:,64),G0(:,:,:,70))
  call loop_Q_A(G0(:,:,:,70),Q(:,51),ZERO,G1(:,:,:,257))
  call loop_QV_A(G1(:,:,:,257),wf(:,62),G1(:,:,:,258))
  call check_last_Q_A(l_switch,G1(:,:,:,258),Q(:,63),ZERO,G2tensor(:,275))
  call loop_QV_A(G0(:,:,:,1),wf(:,65),G0(:,:,:,71))
  call loop_Q_A(G0(:,:,:,71),Q(:,51),ZERO,G1(:,:,:,259))
  call loop_QV_A(G1(:,:,:,259),wf(:,62),G1(:,:,:,260))
  call check_last_Q_A(l_switch,G1(:,:,:,260),Q(:,63),ZERO,G2tensor(:,276))
  call loop_QV_A(G0(:,:,:,1),wf(:,63),G0(:,:,:,72))
  call loop_Q_A(G0(:,:,:,72),Q(:,51),MT,G1(:,:,:,261))
  call loop_QV_A(G1(:,:,:,261),wf(:,62),G1(:,:,:,262))
  call check_last_Q_A(l_switch,G1(:,:,:,262),Q(:,63),MT,G2tensor(:,277))
  call loop_QV_A(G0(:,:,:,1),wf(:,64),G0(:,:,:,73))
  call loop_Q_A(G0(:,:,:,73),Q(:,51),MT,G1(:,:,:,263))
  call loop_QV_A(G1(:,:,:,263),wf(:,62),G1(:,:,:,264))
  call check_last_Q_A(l_switch,G1(:,:,:,264),Q(:,63),MT,G2tensor(:,278))
  call loop_QV_A(G0(:,:,:,1),wf(:,65),G0(:,:,:,74))
  call loop_Q_A(G0(:,:,:,74),Q(:,51),MT,G1(:,:,:,265))
  call loop_QV_A(G1(:,:,:,265),wf(:,62),G1(:,:,:,266))
  call check_last_Q_A(l_switch,G1(:,:,:,266),Q(:,63),MT,G2tensor(:,279))
  call loop_QV_A(G0(:,:,:,1),wf(:,63),G0(:,:,:,75))
  call loop_Q_A(G0(:,:,:,75),Q(:,51),MB,G1(:,:,:,267))
  call loop_QV_A(G1(:,:,:,267),wf(:,62),G1(:,:,:,268))
  call check_last_Q_A(l_switch,G1(:,:,:,268),Q(:,63),MB,G2tensor(:,280))
  call loop_QV_A(G0(:,:,:,1),wf(:,64),G0(:,:,:,76))
  call loop_Q_A(G0(:,:,:,76),Q(:,51),MB,G1(:,:,:,269))
  call loop_QV_A(G1(:,:,:,269),wf(:,62),G1(:,:,:,270))
  call check_last_Q_A(l_switch,G1(:,:,:,270),Q(:,63),MB,G2tensor(:,281))
  call loop_QV_A(G0(:,:,:,1),wf(:,65),G0(:,:,:,77))
  call loop_Q_A(G0(:,:,:,77),Q(:,51),MB,G1(:,:,:,271))
  call loop_QV_A(G1(:,:,:,271),wf(:,62),G1(:,:,:,272))
  call check_last_Q_A(l_switch,G1(:,:,:,272),Q(:,63),MB,G2tensor(:,282))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,63),Q(:,51),G1(:,:,:,273))
  call check_last_CV_D(l_switch,G1(:,:,:,273),Q(:,51),wf(:,62),Q(:,12),G2tensor(:,283))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,64),Q(:,51),G1(:,:,:,274))
  call check_last_CV_D(l_switch,G1(:,:,:,274),Q(:,51),wf(:,62),Q(:,12),G2tensor(:,284))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,65),Q(:,51),G1(:,:,:,275))
  call check_last_CV_D(l_switch,G1(:,:,:,275),Q(:,51),wf(:,62),Q(:,12),G2tensor(:,285))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,66),wf(:,89),G0(:,:,:,78))
  call check_last_UV_W(l_switch,G0(:,:,:,78),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,149))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,89),wf(:,66),G0(:,:,:,79))
  call check_last_UV_W(l_switch,G0(:,:,:,79),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,150))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,66),wf(:,89),G0(:,:,:,80))
  call check_last_UV_W(l_switch,G0(:,:,:,80),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,151))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,61),wf(:,265),G0(:,:,:,81))
  call check_last_UV_W(l_switch,G0(:,:,:,81),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,152))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,265),wf(:,61),G0(:,:,:,82))
  call check_last_UV_W(l_switch,G0(:,:,:,82),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,153))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,61),wf(:,265),G0(:,:,:,83))
  call check_last_UV_W(l_switch,G0(:,:,:,83),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,154))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,272),G0(:,:,:,84))
  call check_last_UV_W(l_switch,G0(:,:,:,84),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,155))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,272),wf(:,-5),G0(:,:,:,85))
  call check_last_UV_W(l_switch,G0(:,:,:,85),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,156))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,272),G0(:,:,:,86))
  call check_last_UV_W(l_switch,G0(:,:,:,86),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,157))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,75),wf(:,89),G0(:,:,:,87))
  call check_last_UV_W(l_switch,G0(:,:,:,87),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,158))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,89),wf(:,75),G0(:,:,:,88))
  call check_last_UV_W(l_switch,G0(:,:,:,88),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,159))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,75),wf(:,89),G0(:,:,:,89))
  call check_last_UV_W(l_switch,G0(:,:,:,89),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,160))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,61),wf(:,268),G0(:,:,:,90))
  call check_last_UV_W(l_switch,G0(:,:,:,90),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,161))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,268),wf(:,61),G0(:,:,:,91))
  call check_last_UV_W(l_switch,G0(:,:,:,91),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,162))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,61),wf(:,268),G0(:,:,:,92))
  call check_last_UV_W(l_switch,G0(:,:,:,92),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,163))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,1202),G0(:,:,:,93))
  call check_last_UV_W(l_switch,G0(:,:,:,93),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,164))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,1202),wf(:,-5),G0(:,:,:,94))
  call check_last_UV_W(l_switch,G0(:,:,:,94),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,165))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,1202),G0(:,:,:,95))
  call check_last_UV_W(l_switch,G0(:,:,:,95),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,166))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,70),wf(:,83),G0(:,:,:,96))
  call check_last_UV_W(l_switch,G0(:,:,:,96),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,167))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,83),wf(:,70),G0(:,:,:,97))
  call check_last_UV_W(l_switch,G0(:,:,:,97),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,168))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,70),wf(:,83),G0(:,:,:,98))
  call check_last_UV_W(l_switch,G0(:,:,:,98),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,169))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,70),wf(:,88),G0(:,:,:,99))
  call check_last_UV_W(l_switch,G0(:,:,:,99),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,170))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,88),wf(:,70),G0(:,:,:,100))
  call check_last_UV_W(l_switch,G0(:,:,:,100),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,171))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,70),wf(:,88),G0(:,:,:,101))
  call check_last_UV_W(l_switch,G0(:,:,:,101),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,172))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,61),wf(:,263),G0(:,:,:,102))
  call check_last_UV_W(l_switch,G0(:,:,:,102),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,173))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,263),wf(:,61),G0(:,:,:,103))
  call check_last_UV_W(l_switch,G0(:,:,:,103),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,174))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,61),wf(:,263),G0(:,:,:,104))
  call check_last_UV_W(l_switch,G0(:,:,:,104),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,175))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,61),wf(:,266),G0(:,:,:,105))
  call check_last_UV_W(l_switch,G0(:,:,:,105),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,176))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(3)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(542)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(2)*(M(150)-M(160)+M(174)-M(183)+M(186)-M(189)+M(192)-M(193)+M(194)-M(195)+M(197)-M(207)+M(210)-M(213)+M(221) &
    -M(228))) * den(542)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,2)
  Gcoeff = (c(2)*(-M(150)+M(160)-M(174)+M(183)-M(186)+M(189)-M(192)+M(193)-M(194)+M(195)-M(197)+M(207)-M(210)+M(213)-M(221) &
    +M(228))) * den(542)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,3)
  Gcoeff = (c(3)*(-M(150)+M(160)-M(174)+M(183)-M(186)+M(189)-M(192)+M(193)-M(194)+M(195)-M(197)+M(207)-M(210)+M(213)-M(221) &
    +M(228))) * den(542)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,4)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(156)-M(159)+M(197)-M(199)+M(203)-M(204)-M(209)+M(210)-M(219)+M(220)+M(225)-M(226)-M(239) &
    +M(241))) * den(605)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,5)
  Gcoeff = (c(2)*(M(132)-M(133)+M(134)-M(135)-M(156)+M(157)-M(158)+M(159)-M(201)+M(202)+M(219)-M(220)-M(225)+M(226)+M(243) &
    -M(244))) * den(605)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,8)
  Gcoeff = (c(2)*(M(133)-M(134)-M(157)+M(158)-M(197)+M(199)+M(201)-M(202)-M(203)+M(204)+M(209)-M(210)+M(239)-M(241)-M(243) &
    +M(244))) * den(605)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,11)
  Gcoeff = (c(2)*(M(132)-M(135)-M(156)+M(159)-M(197)+M(199)-M(203)+M(204)+M(209)-M(210)+M(219)-M(220)-M(225)+M(226)+M(239) &
    -M(241))) * den(605)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,6)
  Gcoeff = (c(2)*(-M(132)+M(133)-M(134)+M(135)+M(156)-M(157)+M(158)-M(159)+M(201)-M(202)-M(219)+M(220)+M(225)-M(226)-M(243) &
    +M(244))) * den(605)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,9)
  Gcoeff = (c(2)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(605)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,12)
  Gcoeff = (c(3)*(M(132)-M(135)-M(156)+M(159)-M(197)+M(199)-M(203)+M(204)+M(209)-M(210)+M(219)-M(220)-M(225)+M(226)+M(239) &
    -M(241))) * den(605)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,7)
  Gcoeff = (c(3)*(-M(132)+M(133)-M(134)+M(135)+M(156)-M(157)+M(158)-M(159)+M(201)-M(202)-M(219)+M(220)+M(225)-M(226)-M(243) &
    +M(244))) * den(605)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,10)
  Gcoeff = (c(3)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(605)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,13)
  Gcoeff = (c(2)*(M(132)-M(135)-M(156)+M(159)-M(197)+M(199)-M(203)+M(204)+M(209)-M(210)+M(219)-M(220)-M(225)+M(226)+M(239) &
    -M(241))) * den(548)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,14)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(156)-M(159)+M(197)-M(199)+M(203)-M(204)-M(209)+M(210)-M(219)+M(220)+M(225)-M(226)-M(239) &
    +M(241))) * den(548)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,15)
  Gcoeff = (c(3)*(-M(132)+M(135)+M(156)-M(159)+M(197)-M(199)+M(203)-M(204)-M(209)+M(210)-M(219)+M(220)+M(225)-M(226)-M(239) &
    +M(241))) * den(548)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,16)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(141)-M(151)+M(165)-M(175)-M(199)+M(203)-M(204)-M(206)+M(208)-M(212)+M(214)+M(220)-M(226) &
    +M(241))) * den(548)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,17)
  Gcoeff = (c(2)*(M(132)-M(135)-M(141)+M(151)-M(165)+M(175)+M(199)-M(203)+M(204)+M(206)-M(208)+M(212)-M(214)-M(220)+M(226) &
    -M(241))) * den(548)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,18)
  Gcoeff = (c(3)*(M(132)-M(135)-M(141)+M(151)-M(165)+M(175)+M(199)-M(203)+M(204)+M(206)-M(208)+M(212)-M(214)-M(220)+M(226) &
    -M(241))) * den(548)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,19)
  Gcoeff = (c(2)*(-M(141)+M(151)+M(156)-M(159)-M(165)+M(175)+M(197)+M(206)-M(208)-M(209)+M(210)+M(212)-M(214)-M(219)+M(225) &
    -M(239))) * den(548)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,20)
  Gcoeff = (c(2)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(548)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,21)
  Gcoeff = (c(3)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(548)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,22)
  Gcoeff = (c(2)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(609)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,23)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(609)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,26)
  Gcoeff = (c(2)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(609)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,29)
  Gcoeff = (c(2)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(609)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,24)
  Gcoeff = (c(2)*(M(131)-M(132)-M(155)+M(156)-M(179)+M(180)+M(185)-M(186)-M(221)+M(223)+M(225)-M(226)+M(245)-M(247)-M(249) &
    +M(250))) * den(609)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,27)
  Gcoeff = (c(2)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(609)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,30)
  Gcoeff = (c(3)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(609)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,25)
  Gcoeff = (c(3)*(M(131)-M(132)-M(155)+M(156)-M(179)+M(180)+M(185)-M(186)-M(221)+M(223)+M(225)-M(226)+M(245)-M(247)-M(249) &
    +M(250))) * den(609)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,28)
  Gcoeff = (c(3)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(609)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,31)
  Gcoeff = (c(2)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(552)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,32)
  Gcoeff = (c(2)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(552)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,33)
  Gcoeff = (c(3)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(552)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,34)
  Gcoeff = (c(2)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(552)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,35)
  Gcoeff = (c(2)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(552)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,36)
  Gcoeff = (c(3)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(552)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,37)
  Gcoeff = (c(2)*(M(149)-M(159)+M(173)-M(183)+M(186)-M(189)+M(197)-M(207)+M(210)-M(213)+M(216)-M(217)+M(218)-M(219)+M(221) &
    -M(227))) * den(552)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,38)
  Gcoeff = (c(2)*(-M(149)+M(159)-M(173)+M(183)-M(186)+M(189)-M(197)+M(207)-M(210)+M(213)-M(216)+M(217)-M(218)+M(219)-M(221) &
    +M(227))) * den(552)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,39)
  Gcoeff = (c(3)*(-M(149)+M(159)-M(173)+M(183)-M(186)+M(189)-M(197)+M(207)-M(210)+M(213)-M(216)+M(217)-M(218)+M(219)-M(221) &
    +M(227))) * den(552)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,40)
  Gcoeff = (c(2)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(545)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,41)
  Gcoeff = (c(2)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(545)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,42)
  Gcoeff = (c(3)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(545)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,43)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(545)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,44)
  Gcoeff = (c(2)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(545)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,45)
  Gcoeff = (c(3)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(545)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,46)
  Gcoeff = (c(2)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(545)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,47)
  Gcoeff = (c(2)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(545)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,48)
  Gcoeff = (c(3)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(545)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,49)
  Gcoeff = (c(2)*(-M(134)+M(136)+M(158)-M(160)+M(179)-M(180)-M(185)+M(186)-M(195)+M(196)+M(201)-M(202)+M(221)-M(223)-M(245) &
    +M(247))) * den(612)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,50)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(134)-M(136)+M(155)-M(156)-M(158)+M(160)+M(195)-M(196)-M(201)+M(202)-M(225)+M(226)+M(249) &
    -M(250))) * den(612)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,53)
  Gcoeff = (c(2)*(M(131)-M(132)-M(155)+M(156)-M(179)+M(180)+M(185)-M(186)-M(221)+M(223)+M(225)-M(226)+M(245)-M(247)-M(249) &
    +M(250))) * den(612)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,56)
  Gcoeff = (c(2)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(612)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,51)
  Gcoeff = (c(2)*(M(131)-M(132)-M(134)+M(136)-M(155)+M(156)+M(158)-M(160)-M(195)+M(196)+M(201)-M(202)+M(225)-M(226)-M(249) &
    +M(250))) * den(612)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,54)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(612)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,57)
  Gcoeff = (c(3)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(612)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,52)
  Gcoeff = (c(3)*(M(131)-M(132)-M(134)+M(136)-M(155)+M(156)+M(158)-M(160)-M(195)+M(196)+M(201)-M(202)+M(225)-M(226)-M(249) &
    +M(250))) * den(612)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,55)
  Gcoeff = (c(3)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(612)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,58)
  Gcoeff = (c(2)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(538)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,59)
  Gcoeff = (c(2)*(-M(134)+M(136)+M(158)-M(160)+M(179)-M(180)-M(185)+M(186)-M(195)+M(196)+M(201)-M(202)+M(221)-M(223)-M(245) &
    +M(247))) * den(538)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,60)
  Gcoeff = (c(3)*(-M(134)+M(136)+M(158)-M(160)+M(179)-M(180)-M(185)+M(186)-M(195)+M(196)+M(201)-M(202)+M(221)-M(223)-M(245) &
    +M(247))) * den(538)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,61)
  Gcoeff = (c(2)*(-M(134)+M(136)+M(147)-M(153)+M(171)-M(177)+M(179)-M(180)-M(182)+M(184)-M(188)+M(190)+M(196)-M(202)-M(223) &
    +M(247))) * den(538)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,62)
  Gcoeff = (c(2)*(M(134)-M(136)-M(147)+M(153)-M(171)+M(177)-M(179)+M(180)+M(182)-M(184)+M(188)-M(190)-M(196)+M(202)+M(223) &
    -M(247))) * den(538)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,63)
  Gcoeff = (c(3)*(M(134)-M(136)-M(147)+M(153)-M(171)+M(177)-M(179)+M(180)+M(182)-M(184)+M(188)-M(190)-M(196)+M(202)+M(223) &
    -M(247))) * den(538)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,64)
  Gcoeff = (c(2)*(-M(147)+M(153)+M(158)-M(160)-M(171)+M(177)+M(182)-M(184)-M(185)+M(186)+M(188)-M(190)-M(195)+M(201)+M(221) &
    -M(245))) * den(538)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,65)
  Gcoeff = (c(2)*(M(147)-M(153)-M(158)+M(160)+M(171)-M(177)-M(182)+M(184)+M(185)-M(186)-M(188)+M(190)+M(195)-M(201)-M(221) &
    +M(245))) * den(538)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,66)
  Gcoeff = (c(3)*(M(147)-M(153)-M(158)+M(160)+M(171)-M(177)-M(182)+M(184)+M(185)-M(186)-M(188)+M(190)+M(195)-M(201)-M(221) &
    +M(245))) * den(538)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,67)
  Gcoeff = (c(2)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(616)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,68)
  Gcoeff = (c(2)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(616)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,71)
  Gcoeff = (c(2)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(616)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,74)
  Gcoeff = (c(2)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(616)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,69)
  Gcoeff = (c(2)*(M(133)-M(134)-M(157)+M(158)-M(197)+M(199)+M(201)-M(202)-M(203)+M(204)+M(209)-M(210)+M(239)-M(241)-M(243) &
    +M(244))) * den(616)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,72)
  Gcoeff = (c(2)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(616)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,75)
  Gcoeff = (c(3)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(616)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,70)
  Gcoeff = (c(3)*(M(133)-M(134)-M(157)+M(158)-M(197)+M(199)+M(201)-M(202)-M(203)+M(204)+M(209)-M(210)+M(239)-M(241)-M(243) &
    +M(244))) * den(616)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,73)
  Gcoeff = (c(3)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(616)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,76)
  Gcoeff = (c(2)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(542)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,77)
  Gcoeff = (c(2)*(M(136)-M(160)-M(180)+M(186)+M(191)-M(193)-M(195)+M(196)+M(197)-M(199)-M(204)+M(210)+M(221)-M(223)-M(228) &
    +M(234))) * den(542)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,78)
  Gcoeff = (c(3)*(M(136)-M(160)-M(180)+M(186)+M(191)-M(193)-M(195)+M(196)+M(197)-M(199)-M(204)+M(210)+M(221)-M(223)-M(228) &
    +M(234))) * den(542)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,79)
  Gcoeff = (c(2)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(542)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,80)
  Gcoeff = (c(2)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(542)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,81)
  Gcoeff = (c(3)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(542)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,82)
  Gcoeff = (c(2)*(M(150)-M(160)+M(174)-M(183)+M(186)-M(189)+M(192)-M(193)+M(194)-M(195)+M(197)-M(207)+M(210)-M(213)+M(221) &
    -M(228))) * den(542)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,83)
  Gcoeff = (c(2)*(-M(150)+M(160)-M(174)+M(183)-M(186)+M(189)-M(192)+M(193)-M(194)+M(195)-M(197)+M(207)-M(210)+M(213)-M(221) &
    +M(228))) * den(542)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,84)
  Gcoeff = (c(3)*(-M(150)+M(160)-M(174)+M(183)-M(186)+M(189)-M(192)+M(193)-M(194)+M(195)-M(197)+M(207)-M(210)+M(213)-M(221) &
    +M(228))) * den(542)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,85)
  Gcoeff = (c(2)*(M(136)-M(160)-M(180)+M(186)+M(191)-M(193)-M(195)+M(196)+M(197)-M(199)-M(204)+M(210)+M(221)-M(223)-M(228) &
    +M(234))) * den(619)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,86)
  Gcoeff = (c(2)*(M(135)-M(136)-M(159)+M(160)-M(191)+M(193)+M(195)-M(196)+M(215)-M(217)-M(219)+M(220)-M(227)+M(228)+M(233) &
    -M(234))) * den(619)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,89)
  Gcoeff = (c(2)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(619)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,92)
  Gcoeff = (c(2)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(619)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,87)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(619)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,90)
  Gcoeff = (c(2)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(619)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,93)
  Gcoeff = (c(3)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(619)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,88)
  Gcoeff = (c(3)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(619)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,91)
  Gcoeff = (c(3)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(619)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,94)
  Gcoeff = (c(11)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,28)
  Gcoeff = (c(11)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,29)
  Gcoeff = (c(11)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,30)
  Gcoeff = (c(11)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,34)
  Gcoeff = (c(11)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,35)
  Gcoeff = (c(11)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,36)
  Gcoeff = (c(11)*(M(150)-M(160)+M(174)-M(183)+M(186)-M(189)+M(192)-M(193)+M(194)-M(195)+M(197)-M(207)+M(210)-M(213)+M(221) &
    -M(228))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,40)
  Gcoeff = (c(11)*(M(149)-M(150)-M(159)+M(160)+M(173)-M(174)-M(192)+M(193)-M(194)+M(195)+M(216)-M(217)+M(218)-M(219)-M(227) &
    +M(228))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,41)
  Gcoeff = (c(11)*(-M(149)+M(159)-M(173)+M(183)-M(186)+M(189)-M(197)+M(207)-M(210)+M(213)-M(216)+M(217)-M(218)+M(219)-M(221) &
    +M(227))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,42)
  Gcoeff = (c(10)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(298)
  T2sum(1:15,40) = T2sum(1:15,40) + Gcoeff * G2tensor(:,46)
  Gcoeff = (c(10)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(298)
  T2sum(1:15,40) = T2sum(1:15,40) + Gcoeff * G2tensor(:,47)
  Gcoeff = (c(10)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(298)
  T2sum(1:15,40) = T2sum(1:15,40) + Gcoeff * G2tensor(:,48)
  Gcoeff = (c(10)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(298)
  T2sum(1:15,40) = T2sum(1:15,40) + Gcoeff * G2tensor(:,52)
  Gcoeff = (c(10)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(298)
  T2sum(1:15,40) = T2sum(1:15,40) + Gcoeff * G2tensor(:,53)
  Gcoeff = (c(10)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(298)
  T2sum(1:15,40) = T2sum(1:15,40) + Gcoeff * G2tensor(:,54)
  Gcoeff = (c(10)*(M(150)-M(160)+M(174)-M(183)+M(186)-M(189)+M(192)-M(193)+M(194)-M(195)+M(197)-M(207)+M(210)-M(213)+M(221) &
    -M(228))) * den(298)
  T2sum(1:15,40) = T2sum(1:15,40) + Gcoeff * G2tensor(:,58)
  Gcoeff = (c(10)*(M(149)-M(150)-M(159)+M(160)+M(173)-M(174)-M(192)+M(193)-M(194)+M(195)+M(216)-M(217)+M(218)-M(219)-M(227) &
    +M(228))) * den(298)
  T2sum(1:15,40) = T2sum(1:15,40) + Gcoeff * G2tensor(:,59)
  Gcoeff = (c(10)*(-M(149)+M(159)-M(173)+M(183)-M(186)+M(189)-M(197)+M(207)-M(210)+M(213)-M(216)+M(217)-M(218)+M(219)-M(221) &
    +M(227))) * den(298)
  T2sum(1:15,40) = T2sum(1:15,40) + Gcoeff * G2tensor(:,60)
  Gcoeff = (c(11)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,28)
  Gcoeff = (c(11)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,29)
  Gcoeff = (c(11)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,30)
  Gcoeff = (c(11)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,34)
  Gcoeff = (c(11)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,35)
  Gcoeff = (c(11)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,36)
  Gcoeff = (c(11)*(M(150)-M(160)+M(174)-M(183)+M(186)-M(189)+M(192)-M(193)+M(194)-M(195)+M(197)-M(207)+M(210)-M(213)+M(221) &
    -M(228))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,40)
  Gcoeff = (c(11)*(M(149)-M(150)-M(159)+M(160)+M(173)-M(174)-M(192)+M(193)-M(194)+M(195)+M(216)-M(217)+M(218)-M(219)-M(227) &
    +M(228))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,41)
  Gcoeff = (c(11)*(-M(149)+M(159)-M(173)+M(183)-M(186)+M(189)-M(197)+M(207)-M(210)+M(213)-M(216)+M(217)-M(218)+M(219)-M(221) &
    +M(227))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,42)
  Gcoeff = (c(10)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(298)
  T2sum(1:15,41) = T2sum(1:15,41) + Gcoeff * G2tensor(:,64)
  Gcoeff = (c(10)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(298)
  T2sum(1:15,41) = T2sum(1:15,41) + Gcoeff * G2tensor(:,65)
  Gcoeff = (c(10)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(298)
  T2sum(1:15,41) = T2sum(1:15,41) + Gcoeff * G2tensor(:,66)
  Gcoeff = (c(10)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(298)
  T2sum(1:15,41) = T2sum(1:15,41) + Gcoeff * G2tensor(:,70)
  Gcoeff = (c(10)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(298)
  T2sum(1:15,41) = T2sum(1:15,41) + Gcoeff * G2tensor(:,71)
  Gcoeff = (c(10)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(298)
  T2sum(1:15,41) = T2sum(1:15,41) + Gcoeff * G2tensor(:,72)
  Gcoeff = (c(10)*(M(150)-M(160)+M(174)-M(183)+M(186)-M(189)+M(192)-M(193)+M(194)-M(195)+M(197)-M(207)+M(210)-M(213)+M(221) &
    -M(228))) * den(298)
  T2sum(1:15,41) = T2sum(1:15,41) + Gcoeff * G2tensor(:,76)
  Gcoeff = (c(10)*(M(149)-M(150)-M(159)+M(160)+M(173)-M(174)-M(192)+M(193)-M(194)+M(195)+M(216)-M(217)+M(218)-M(219)-M(227) &
    +M(228))) * den(298)
  T2sum(1:15,41) = T2sum(1:15,41) + Gcoeff * G2tensor(:,77)
  Gcoeff = (c(10)*(-M(149)+M(159)-M(173)+M(183)-M(186)+M(189)-M(197)+M(207)-M(210)+M(213)-M(216)+M(217)-M(218)+M(219)-M(221) &
    +M(227))) * den(298)
  T2sum(1:15,41) = T2sum(1:15,41) + Gcoeff * G2tensor(:,78)
  Gcoeff = (c(7)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,82)
  Gcoeff = (c(7)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,83)
  Gcoeff = (c(7)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,84)
  Gcoeff = (c(7)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,88)
  Gcoeff = (c(7)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,89)
  Gcoeff = (c(7)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,90)
  Gcoeff = (c(7)*(M(150)-M(160)+M(174)-M(183)+M(186)-M(189)+M(192)-M(193)+M(194)-M(195)+M(197)-M(207)+M(210)-M(213)+M(221) &
    -M(228))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,94)
  Gcoeff = (c(7)*(M(149)-M(150)-M(159)+M(160)+M(173)-M(174)-M(192)+M(193)-M(194)+M(195)+M(216)-M(217)+M(218)-M(219)-M(227) &
    +M(228))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,95)
  Gcoeff = (c(7)*(-M(149)+M(159)-M(173)+M(183)-M(186)+M(189)-M(197)+M(207)-M(210)+M(213)-M(216)+M(217)-M(218)+M(219)-M(221) &
    +M(227))) * den(298)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,96)
  Gcoeff = (c(2)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(545)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,95)
  Gcoeff = (c(2)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(545)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,96)
  Gcoeff = (c(3)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(545)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,97)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(545)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,98)
  Gcoeff = (c(2)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(545)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,99)
  Gcoeff = (c(3)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(545)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,100)
  Gcoeff = (c(2)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(545)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,101)
  Gcoeff = (c(2)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(545)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,102)
  Gcoeff = (c(3)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(545)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,103)
  Gcoeff = (c(11)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,103)
  Gcoeff = (c(11)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,109)
  Gcoeff = (c(11)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,115)
  Gcoeff = (c(11)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,104)
  Gcoeff = (c(11)*(M(133)-M(134)-M(143)+M(144)-M(167)+M(168)-M(197)+M(198)+M(200)-M(202)+M(209)-M(210)+M(239)-M(240)-M(242) &
    +M(244))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,110)
  Gcoeff = (c(11)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,116)
  Gcoeff = (c(11)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,105)
  Gcoeff = (c(11)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,111)
  Gcoeff = (c(11)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,117)
  Gcoeff = (c(10)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(299)
  T2sum(1:15,42) = T2sum(1:15,42) + Gcoeff * G2tensor(:,121)
  Gcoeff = (c(10)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(299)
  T2sum(1:15,42) = T2sum(1:15,42) + Gcoeff * G2tensor(:,127)
  Gcoeff = (c(10)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(299)
  T2sum(1:15,42) = T2sum(1:15,42) + Gcoeff * G2tensor(:,133)
  Gcoeff = (c(10)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(299)
  T2sum(1:15,42) = T2sum(1:15,42) + Gcoeff * G2tensor(:,122)
  Gcoeff = (c(10)*(M(133)-M(134)-M(143)+M(144)-M(167)+M(168)-M(197)+M(198)+M(200)-M(202)+M(209)-M(210)+M(239)-M(240)-M(242) &
    +M(244))) * den(299)
  T2sum(1:15,42) = T2sum(1:15,42) + Gcoeff * G2tensor(:,128)
  Gcoeff = (c(10)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(299)
  T2sum(1:15,42) = T2sum(1:15,42) + Gcoeff * G2tensor(:,134)
  Gcoeff = (c(10)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(299)
  T2sum(1:15,42) = T2sum(1:15,42) + Gcoeff * G2tensor(:,123)
  Gcoeff = (c(10)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(299)
  T2sum(1:15,42) = T2sum(1:15,42) + Gcoeff * G2tensor(:,129)
  Gcoeff = (c(10)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(299)
  T2sum(1:15,42) = T2sum(1:15,42) + Gcoeff * G2tensor(:,135)
  Gcoeff = (c(11)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,103)
  Gcoeff = (c(11)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,109)
  Gcoeff = (c(11)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,115)
  Gcoeff = (c(11)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,104)
  Gcoeff = (c(11)*(M(133)-M(134)-M(143)+M(144)-M(167)+M(168)-M(197)+M(198)+M(200)-M(202)+M(209)-M(210)+M(239)-M(240)-M(242) &
    +M(244))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,110)
  Gcoeff = (c(11)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,116)
  Gcoeff = (c(11)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,105)
  Gcoeff = (c(11)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,111)
  Gcoeff = (c(11)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,117)
  Gcoeff = (c(10)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(299)
  T2sum(1:15,43) = T2sum(1:15,43) + Gcoeff * G2tensor(:,139)
  Gcoeff = (c(10)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(299)
  T2sum(1:15,43) = T2sum(1:15,43) + Gcoeff * G2tensor(:,145)
  Gcoeff = (c(10)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(299)
  T2sum(1:15,43) = T2sum(1:15,43) + Gcoeff * G2tensor(:,151)
  Gcoeff = (c(10)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(299)
  T2sum(1:15,43) = T2sum(1:15,43) + Gcoeff * G2tensor(:,140)
  Gcoeff = (c(10)*(M(133)-M(134)-M(143)+M(144)-M(167)+M(168)-M(197)+M(198)+M(200)-M(202)+M(209)-M(210)+M(239)-M(240)-M(242) &
    +M(244))) * den(299)
  T2sum(1:15,43) = T2sum(1:15,43) + Gcoeff * G2tensor(:,146)
  Gcoeff = (c(10)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(299)
  T2sum(1:15,43) = T2sum(1:15,43) + Gcoeff * G2tensor(:,152)
  Gcoeff = (c(10)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(299)
  T2sum(1:15,43) = T2sum(1:15,43) + Gcoeff * G2tensor(:,141)
  Gcoeff = (c(10)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(299)
  T2sum(1:15,43) = T2sum(1:15,43) + Gcoeff * G2tensor(:,147)
  Gcoeff = (c(10)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(299)
  T2sum(1:15,43) = T2sum(1:15,43) + Gcoeff * G2tensor(:,153)
  Gcoeff = (c(7)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,157)
  Gcoeff = (c(7)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,163)
  Gcoeff = (c(7)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,169)
  Gcoeff = (c(7)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,158)
  Gcoeff = (c(7)*(M(133)-M(134)-M(143)+M(144)-M(167)+M(168)-M(197)+M(198)+M(200)-M(202)+M(209)-M(210)+M(239)-M(240)-M(242) &
    +M(244))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,164)
  Gcoeff = (c(7)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,170)
  Gcoeff = (c(7)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,159)
  Gcoeff = (c(7)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,165)
  Gcoeff = (c(7)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(299)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,171)
  Gcoeff = (c(2)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(568)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,104)
  Gcoeff = (c(2)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(568)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,105)
  Gcoeff = (c(3)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(568)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,106)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(568)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,107)
  Gcoeff = (c(2)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(568)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,108)
  Gcoeff = (c(3)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(568)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,109)
  Gcoeff = (c(2)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(568)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,110)
  Gcoeff = (c(2)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(568)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,111)
  Gcoeff = (c(3)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(568)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,112)
  Gcoeff = (c(2)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(568)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,113)
  Gcoeff = (c(2)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(568)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,114)
  Gcoeff = (c(3)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(568)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,115)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(568)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,116)
  Gcoeff = (c(2)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(568)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,117)
  Gcoeff = (c(3)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(568)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,118)
  Gcoeff = (c(2)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(568)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,119)
  Gcoeff = (c(2)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(568)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,120)
  Gcoeff = (c(3)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(568)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,121)
  Gcoeff = (c(11)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,178)
  Gcoeff = (c(11)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,184)
  Gcoeff = (c(11)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,190)
  Gcoeff = (c(11)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,179)
  Gcoeff = (c(11)*(M(131)-M(132)-M(137)+M(138)-M(161)+M(162)+M(185)-M(186)-M(221)+M(222)+M(224)-M(226)+M(245)-M(246)-M(248) &
    +M(250))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,185)
  Gcoeff = (c(11)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,191)
  Gcoeff = (c(11)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,180)
  Gcoeff = (c(11)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,186)
  Gcoeff = (c(11)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,192)
  Gcoeff = (c(10)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(300)
  T2sum(1:15,44) = T2sum(1:15,44) + Gcoeff * G2tensor(:,196)
  Gcoeff = (c(10)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(300)
  T2sum(1:15,44) = T2sum(1:15,44) + Gcoeff * G2tensor(:,202)
  Gcoeff = (c(10)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(300)
  T2sum(1:15,44) = T2sum(1:15,44) + Gcoeff * G2tensor(:,208)
  Gcoeff = (c(10)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(300)
  T2sum(1:15,44) = T2sum(1:15,44) + Gcoeff * G2tensor(:,197)
  Gcoeff = (c(10)*(M(131)-M(132)-M(137)+M(138)-M(161)+M(162)+M(185)-M(186)-M(221)+M(222)+M(224)-M(226)+M(245)-M(246)-M(248) &
    +M(250))) * den(300)
  T2sum(1:15,44) = T2sum(1:15,44) + Gcoeff * G2tensor(:,203)
  Gcoeff = (c(10)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(300)
  T2sum(1:15,44) = T2sum(1:15,44) + Gcoeff * G2tensor(:,209)
  Gcoeff = (c(10)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(300)
  T2sum(1:15,44) = T2sum(1:15,44) + Gcoeff * G2tensor(:,198)
  Gcoeff = (c(10)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(300)
  T2sum(1:15,44) = T2sum(1:15,44) + Gcoeff * G2tensor(:,204)
  Gcoeff = (c(10)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(300)
  T2sum(1:15,44) = T2sum(1:15,44) + Gcoeff * G2tensor(:,210)
  Gcoeff = (c(11)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,178)
  Gcoeff = (c(11)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,184)
  Gcoeff = (c(11)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,190)
  Gcoeff = (c(11)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,179)
  Gcoeff = (c(11)*(M(131)-M(132)-M(137)+M(138)-M(161)+M(162)+M(185)-M(186)-M(221)+M(222)+M(224)-M(226)+M(245)-M(246)-M(248) &
    +M(250))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,185)
  Gcoeff = (c(11)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,191)
  Gcoeff = (c(11)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,180)
  Gcoeff = (c(11)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,186)
  Gcoeff = (c(11)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,192)
  Gcoeff = (c(10)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(300)
  T2sum(1:15,45) = T2sum(1:15,45) + Gcoeff * G2tensor(:,214)
  Gcoeff = (c(10)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(300)
  T2sum(1:15,45) = T2sum(1:15,45) + Gcoeff * G2tensor(:,220)
  Gcoeff = (c(10)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(300)
  T2sum(1:15,45) = T2sum(1:15,45) + Gcoeff * G2tensor(:,226)
  Gcoeff = (c(10)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(300)
  T2sum(1:15,45) = T2sum(1:15,45) + Gcoeff * G2tensor(:,215)
  Gcoeff = (c(10)*(M(131)-M(132)-M(137)+M(138)-M(161)+M(162)+M(185)-M(186)-M(221)+M(222)+M(224)-M(226)+M(245)-M(246)-M(248) &
    +M(250))) * den(300)
  T2sum(1:15,45) = T2sum(1:15,45) + Gcoeff * G2tensor(:,221)
  Gcoeff = (c(10)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(300)
  T2sum(1:15,45) = T2sum(1:15,45) + Gcoeff * G2tensor(:,227)
  Gcoeff = (c(10)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(300)
  T2sum(1:15,45) = T2sum(1:15,45) + Gcoeff * G2tensor(:,216)
  Gcoeff = (c(10)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(300)
  T2sum(1:15,45) = T2sum(1:15,45) + Gcoeff * G2tensor(:,222)
  Gcoeff = (c(10)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(300)
  T2sum(1:15,45) = T2sum(1:15,45) + Gcoeff * G2tensor(:,228)
  Gcoeff = (c(7)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,232)
  Gcoeff = (c(7)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,238)
  Gcoeff = (c(7)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,244)
  Gcoeff = (c(7)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,233)
  Gcoeff = (c(7)*(M(131)-M(132)-M(137)+M(138)-M(161)+M(162)+M(185)-M(186)-M(221)+M(222)+M(224)-M(226)+M(245)-M(246)-M(248) &
    +M(250))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,239)
  Gcoeff = (c(7)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,245)
  Gcoeff = (c(7)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,234)
  Gcoeff = (c(7)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,240)
  Gcoeff = (c(7)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(300)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,246)
  Gcoeff = (c(2)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(308)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,122)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(143)-M(145)+M(149)-M(151)-M(153)+M(154)+M(164)-M(188)-M(212)+M(218)-M(236)+M(242)+M(248) &
    -M(250))) * den(308)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,123)
  Gcoeff = (c(3)*(-M(131)+M(137)+M(143)-M(145)+M(149)-M(151)-M(153)+M(154)+M(164)-M(188)-M(212)+M(218)-M(236)+M(242)+M(248) &
    -M(250))) * den(308)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,124)
  Gcoeff = (c(2)*(M(132)-M(138)-M(143)+M(145)+M(147)-M(148)-M(149)+M(151)-M(166)+M(190)+M(212)-M(218)-M(224)+M(226)+M(236) &
    -M(242))) * den(310)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,125)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(310)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,126)
  Gcoeff = (c(3)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(310)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,127)
  Gcoeff = (c(2)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(785)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,128)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(785)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,129)
  Gcoeff = (c(3)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(785)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,130)
  Gcoeff = (c(2)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(308)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,131)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(143)-M(145)+M(149)-M(151)-M(153)+M(154)+M(164)-M(188)-M(212)+M(218)-M(236)+M(242)+M(248) &
    -M(250))) * den(308)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,132)
  Gcoeff = (c(3)*(-M(131)+M(137)+M(143)-M(145)+M(149)-M(151)-M(153)+M(154)+M(164)-M(188)-M(212)+M(218)-M(236)+M(242)+M(248) &
    -M(250))) * den(308)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,133)
  Gcoeff = (c(2)*(M(132)-M(138)-M(143)+M(145)+M(147)-M(148)-M(149)+M(151)-M(166)+M(190)+M(212)-M(218)-M(224)+M(226)+M(236) &
    -M(242))) * den(310)
  T2sum(1:5,16) = T2sum(1:5,16) + Gcoeff * G1tensor(:,140)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(310)
  T2sum(1:5,16) = T2sum(1:5,16) + Gcoeff * G1tensor(:,141)
  Gcoeff = (c(3)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(310)
  T2sum(1:5,16) = T2sum(1:5,16) + Gcoeff * G1tensor(:,142)
  Gcoeff = (c(11)*(-M(131)+M(137)+M(143)-M(145)+M(149)-M(151)-M(153)+M(154)+M(164)-M(188)-M(212)+M(218)-M(236)+M(242)+M(248) &
    -M(250))) * den(303)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,262)
  Gcoeff = (c(11)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(303)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,263)
  Gcoeff = (c(11)*(M(132)-M(138)-M(143)+M(145)+M(147)-M(148)-M(149)+M(151)-M(166)+M(190)+M(212)-M(218)-M(224)+M(226)+M(236) &
    -M(242))) * den(303)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,264)
  Gcoeff = (c(10)*(-M(131)+M(137)+M(143)-M(145)+M(149)-M(151)-M(153)+M(154)+M(164)-M(188)-M(212)+M(218)-M(236)+M(242)+M(248) &
    -M(250))) * den(303)
  T2sum(1:15,46) = T2sum(1:15,46) + Gcoeff * G2tensor(:,265)
  Gcoeff = (c(10)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(303)
  T2sum(1:15,46) = T2sum(1:15,46) + Gcoeff * G2tensor(:,266)
  Gcoeff = (c(10)*(M(132)-M(138)-M(143)+M(145)+M(147)-M(148)-M(149)+M(151)-M(166)+M(190)+M(212)-M(218)-M(224)+M(226)+M(236) &
    -M(242))) * den(303)
  T2sum(1:15,46) = T2sum(1:15,46) + Gcoeff * G2tensor(:,267)
  Gcoeff = (c(11)*(-M(131)+M(137)+M(143)-M(145)+M(149)-M(151)-M(153)+M(154)+M(164)-M(188)-M(212)+M(218)-M(236)+M(242)+M(248) &
    -M(250))) * den(303)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,262)
  Gcoeff = (c(11)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(303)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,263)
  Gcoeff = (c(11)*(M(132)-M(138)-M(143)+M(145)+M(147)-M(148)-M(149)+M(151)-M(166)+M(190)+M(212)-M(218)-M(224)+M(226)+M(236) &
    -M(242))) * den(303)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,264)
  Gcoeff = (c(10)*(-M(131)+M(137)+M(143)-M(145)+M(149)-M(151)-M(153)+M(154)+M(164)-M(188)-M(212)+M(218)-M(236)+M(242)+M(248) &
    -M(250))) * den(303)
  T2sum(1:15,47) = T2sum(1:15,47) + Gcoeff * G2tensor(:,268)
  Gcoeff = (c(10)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(303)
  T2sum(1:15,47) = T2sum(1:15,47) + Gcoeff * G2tensor(:,269)
  Gcoeff = (c(10)*(M(132)-M(138)-M(143)+M(145)+M(147)-M(148)-M(149)+M(151)-M(166)+M(190)+M(212)-M(218)-M(224)+M(226)+M(236) &
    -M(242))) * den(303)
  T2sum(1:15,47) = T2sum(1:15,47) + Gcoeff * G2tensor(:,270)
  Gcoeff = (c(7)*(-M(131)+M(137)+M(143)-M(145)+M(149)-M(151)-M(153)+M(154)+M(164)-M(188)-M(212)+M(218)-M(236)+M(242)+M(248) &
    -M(250))) * den(303)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,271)
  Gcoeff = (c(7)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(303)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,272)
  Gcoeff = (c(7)*(M(132)-M(138)-M(143)+M(145)+M(147)-M(148)-M(149)+M(151)-M(166)+M(190)+M(212)-M(218)-M(224)+M(226)+M(236) &
    -M(242))) * den(303)
  T2sum(1:15,7) = T2sum(1:15,7) + Gcoeff * G2tensor(:,273)
  Gcoeff = (c(11)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(306)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,274)
  Gcoeff = (c(11)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(306)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,275)
  Gcoeff = (c(11)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(306)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,276)
  Gcoeff = (c(10)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(306)
  T2sum(1:15,48) = T2sum(1:15,48) + Gcoeff * G2tensor(:,277)
  Gcoeff = (c(10)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(306)
  T2sum(1:15,48) = T2sum(1:15,48) + Gcoeff * G2tensor(:,278)
  Gcoeff = (c(10)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(306)
  T2sum(1:15,48) = T2sum(1:15,48) + Gcoeff * G2tensor(:,279)
  Gcoeff = (c(11)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(306)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,274)
  Gcoeff = (c(11)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(306)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,275)
  Gcoeff = (c(11)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(306)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,276)
  Gcoeff = (c(10)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(306)
  T2sum(1:15,49) = T2sum(1:15,49) + Gcoeff * G2tensor(:,280)
  Gcoeff = (c(10)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(306)
  T2sum(1:15,49) = T2sum(1:15,49) + Gcoeff * G2tensor(:,281)
  Gcoeff = (c(10)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(306)
  T2sum(1:15,49) = T2sum(1:15,49) + Gcoeff * G2tensor(:,282)
  Gcoeff = (c(7)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(306)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,283)
  Gcoeff = (c(7)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(306)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,284)
  Gcoeff = (c(7)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(306)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,285)
  Gcoeff = (c(2)*(M(133)-M(137)+M(139)-M(143)-M(150)+M(151)-M(152)+M(153)-M(170)+M(188)-M(194)+M(212)+M(238)-M(242)+M(244) &
    -M(248))) * den(324)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,149)
  Gcoeff = (c(2)*(-M(133)+M(137)-M(139)+M(143)+M(150)-M(151)+M(152)-M(153)+M(170)-M(188)+M(194)-M(212)-M(238)+M(242)-M(244) &
    +M(248))) * den(324)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,150)
  Gcoeff = (c(3)*(-M(133)+M(137)-M(139)+M(143)+M(150)-M(151)+M(152)-M(153)+M(170)-M(188)+M(194)-M(212)-M(238)+M(242)-M(244) &
    +M(248))) * den(324)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,151)
  Gcoeff = (c(2)*(M(134)-M(137)+M(139)+M(141)-M(142)-M(144)-M(150)+M(153)-M(172)+M(188)-M(194)-M(200)+M(202)+M(214)+M(238) &
    -M(248))) * den(326)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,152)
  Gcoeff = (c(2)*(-M(134)+M(137)-M(139)-M(141)+M(142)+M(144)+M(150)-M(153)+M(172)-M(188)+M(194)+M(200)-M(202)-M(214)-M(238) &
    +M(248))) * den(326)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,153)
  Gcoeff = (c(3)*(-M(134)+M(137)-M(139)-M(141)+M(142)+M(144)+M(150)-M(153)+M(172)-M(188)+M(194)+M(200)-M(202)-M(214)-M(238) &
    +M(248))) * den(326)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,154)
  Gcoeff = (c(2)*(M(133)-M(134)-M(141)+M(142)-M(143)+M(144)+M(151)-M(152)-M(170)+M(172)+M(200)-M(202)+M(212)-M(214)-M(242) &
    +M(244))) * den(799)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,155)
  Gcoeff = (c(2)*(-M(133)+M(134)+M(141)-M(142)+M(143)-M(144)-M(151)+M(152)+M(170)-M(172)-M(200)+M(202)-M(212)+M(214)+M(242) &
    -M(244))) * den(799)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,156)
  Gcoeff = (c(3)*(-M(133)+M(134)+M(141)-M(142)+M(143)-M(144)-M(151)+M(152)+M(170)-M(172)-M(200)+M(202)-M(212)+M(214)+M(242) &
    -M(244))) * den(799)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,157)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(139)-M(145)+M(149)-M(150)-M(152)+M(154)+M(164)-M(170)-M(194)+M(218)-M(236)+M(238)+M(244) &
    -M(250))) * den(354)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,158)
  Gcoeff = (c(2)*(M(131)-M(133)-M(139)+M(145)-M(149)+M(150)+M(152)-M(154)-M(164)+M(170)+M(194)-M(218)+M(236)-M(238)-M(244) &
    +M(250))) * den(354)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,159)
  Gcoeff = (c(3)*(M(131)-M(133)-M(139)+M(145)-M(149)+M(150)+M(152)-M(154)-M(164)+M(170)+M(194)-M(218)+M(236)-M(238)-M(244) &
    +M(250))) * den(354)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,160)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(135)-M(136)+M(140)-M(146)-M(152)+M(154)+M(164)-M(170)-M(176)+M(178)-M(196)+M(220)+M(244) &
    -M(250))) * den(356)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,161)
  Gcoeff = (c(2)*(M(131)-M(133)-M(135)+M(136)-M(140)+M(146)+M(152)-M(154)-M(164)+M(170)+M(176)-M(178)+M(196)-M(220)-M(244) &
    +M(250))) * den(356)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,162)
  Gcoeff = (c(3)*(M(131)-M(133)-M(135)+M(136)-M(140)+M(146)+M(152)-M(154)-M(164)+M(170)+M(176)-M(178)+M(196)-M(220)-M(244) &
    +M(250))) * den(356)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,163)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(139)-M(140)-M(145)+M(146)+M(149)-M(150)+M(176)-M(178)-M(194)+M(196)+M(218)-M(220)-M(236) &
    +M(238))) * den(810)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,164)
  Gcoeff = (c(2)*(M(135)-M(136)-M(139)+M(140)+M(145)-M(146)-M(149)+M(150)-M(176)+M(178)+M(194)-M(196)-M(218)+M(220)+M(236) &
    -M(238))) * den(810)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,165)
  Gcoeff = (c(3)*(M(135)-M(136)-M(139)+M(140)+M(145)-M(146)-M(149)+M(150)-M(176)+M(178)+M(194)-M(196)-M(218)+M(220)+M(236) &
    -M(238))) * den(810)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,166)
  Gcoeff = (c(2)*(M(136)-M(138)+M(139)-M(140)+M(141)-M(144)+M(147)-M(150)-M(178)+M(190)-M(194)+M(196)-M(200)+M(214)-M(224) &
    +M(238))) * den(156)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,167)
  Gcoeff = (c(2)*(-M(136)+M(138)-M(139)+M(140)-M(141)+M(144)-M(147)+M(150)+M(178)-M(190)+M(194)-M(196)+M(200)-M(214)+M(224) &
    -M(238))) * den(156)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,168)
  Gcoeff = (c(3)*(-M(136)+M(138)-M(139)+M(140)-M(141)+M(144)-M(147)+M(150)+M(178)-M(190)+M(194)-M(196)+M(200)-M(214)+M(224) &
    -M(238))) * den(156)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,169)
  Gcoeff = (c(2)*(M(135)-M(138)+M(141)-M(144)+M(145)-M(146)+M(147)-M(149)-M(176)+M(190)-M(200)+M(214)-M(218)+M(220)-M(224) &
    +M(236))) * den(327)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,170)
  Gcoeff = (c(2)*(-M(135)+M(138)-M(141)+M(144)-M(145)+M(146)-M(147)+M(149)+M(176)-M(190)+M(200)-M(214)+M(218)-M(220)+M(224) &
    -M(236))) * den(327)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,171)
  Gcoeff = (c(3)*(-M(135)+M(138)-M(141)+M(144)-M(145)+M(146)-M(147)+M(149)+M(176)-M(190)+M(200)-M(214)+M(218)-M(220)+M(224) &
    -M(236))) * den(327)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,172)
  Gcoeff = (c(2)*(-M(135)+M(138)-M(141)+M(144)-M(145)+M(146)-M(147)+M(149)+M(176)-M(190)+M(200)-M(214)+M(218)-M(220)+M(224) &
    -M(236))) * den(158)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,173)
  Gcoeff = (c(2)*(M(135)-M(138)+M(141)-M(144)+M(145)-M(146)+M(147)-M(149)-M(176)+M(190)-M(200)+M(214)-M(218)+M(220)-M(224) &
    +M(236))) * den(158)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,174)
  Gcoeff = (c(3)*(M(135)-M(138)+M(141)-M(144)+M(145)-M(146)+M(147)-M(149)-M(176)+M(190)-M(200)+M(214)-M(218)+M(220)-M(224) &
    +M(236))) * den(158)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,175)
  Gcoeff = (c(2)*(-M(136)+M(138)-M(139)+M(140)-M(141)+M(144)-M(147)+M(150)+M(178)-M(190)+M(194)-M(196)+M(200)-M(214)+M(224) &
    -M(238))) * den(329)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,176)
  Gcoeff = (c(2)*(M(133)-M(137)+M(139)-M(143)-M(150)+M(151)-M(152)+M(153)-M(170)+M(188)-M(194)+M(212)+M(238)-M(242)+M(244) &
    -M(248))) * den(324)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,134)
  Gcoeff = (c(2)*(-M(133)+M(137)-M(139)+M(143)+M(150)-M(151)+M(152)-M(153)+M(170)-M(188)+M(194)-M(212)-M(238)+M(242)-M(244) &
    +M(248))) * den(324)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,135)
  Gcoeff = (c(3)*(-M(133)+M(137)-M(139)+M(143)+M(150)-M(151)+M(152)-M(153)+M(170)-M(188)+M(194)-M(212)-M(238)+M(242)-M(244) &
    +M(248))) * den(324)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,136)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(139)-M(145)+M(149)-M(150)-M(152)+M(154)+M(164)-M(170)-M(194)+M(218)-M(236)+M(238)+M(244) &
    -M(250))) * den(354)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,137)
  Gcoeff = (c(2)*(M(131)-M(133)-M(139)+M(145)-M(149)+M(150)+M(152)-M(154)-M(164)+M(170)+M(194)-M(218)+M(236)-M(238)-M(244) &
    +M(250))) * den(354)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,138)
  Gcoeff = (c(3)*(M(131)-M(133)-M(139)+M(145)-M(149)+M(150)+M(152)-M(154)-M(164)+M(170)+M(194)-M(218)+M(236)-M(238)-M(244) &
    +M(250))) * den(354)
  T2sum(1:5,17) = T2sum(1:5,17) + Gcoeff * G1tensor(:,139)
  Gcoeff = (c(3)*(-M(131)+M(137)+M(143)-M(145)+M(149)-M(151)-M(153)+M(154)+M(164)-M(188)-M(212)+M(218)-M(236)+M(242)+M(248) &
    -M(250))) * den(353)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,250)
  Gcoeff = (c(3)*(M(131)-M(133)-M(139)+M(145)-M(149)+M(150)+M(152)-M(154)-M(164)+M(170)+M(194)-M(218)+M(236)-M(238)-M(244) &
    +M(250))) * den(353)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,251)
  Gcoeff = (c(3)*(M(133)-M(137)+M(139)-M(143)-M(150)+M(151)-M(152)+M(153)-M(170)+M(188)-M(194)+M(212)+M(238)-M(242)+M(244) &
    -M(248))) * den(353)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,252)
  Gcoeff = (c(2)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(590)
  T2sum(1:5,16) = T2sum(1:5,16) + Gcoeff * G1tensor(:,143)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(149)-M(151)+M(165)-M(189)+M(203)-M(205)-M(207)+M(208)-M(212)+M(218)+M(224)-M(226)-M(235) &
    +M(241))) * den(590)
  T2sum(1:5,16) = T2sum(1:5,16) + Gcoeff * G1tensor(:,144)
  Gcoeff = (c(3)*(-M(132)+M(138)+M(149)-M(151)+M(165)-M(189)+M(203)-M(205)-M(207)+M(208)-M(212)+M(218)+M(224)-M(226)-M(235) &
    +M(241))) * den(590)
  T2sum(1:5,16) = T2sum(1:5,16) + Gcoeff * G1tensor(:,145)
  Gcoeff = (c(3)*(-M(134)+M(136)+M(158)-M(160)+M(179)-M(180)-M(185)+M(186)-M(195)+M(196)+M(201)-M(202)+M(221)-M(223)-M(245) &
    +M(247))) * den(539)
  T2sum(1:15,13) = T2sum(1:15,13) + Gcoeff * G2tensor(:,16)
  Gcoeff = (c(3)*(M(134)-M(136)-M(147)+M(153)-M(171)+M(177)-M(179)+M(180)+M(182)-M(184)+M(188)-M(190)-M(196)+M(202)+M(223) &
    -M(247))) * den(539)
  T2sum(1:15,13) = T2sum(1:15,13) + Gcoeff * G2tensor(:,17)
  Gcoeff = (c(3)*(M(147)-M(153)-M(158)+M(160)+M(171)-M(177)-M(182)+M(184)+M(185)-M(186)-M(188)+M(190)+M(195)-M(201)-M(221) &
    +M(245))) * den(539)
  T2sum(1:15,13) = T2sum(1:15,13) + Gcoeff * G2tensor(:,18)
  Gcoeff = (c(11)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(544)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,31)
  Gcoeff = (c(11)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(544)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,37)
  Gcoeff = (c(11)*(M(150)-M(160)+M(174)-M(183)+M(186)-M(189)+M(192)-M(193)+M(194)-M(195)+M(197)-M(207)+M(210)-M(213)+M(221) &
    -M(228))) * den(544)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,43)
  Gcoeff = (c(10)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(544)
  T2sum(1:15,40) = T2sum(1:15,40) + Gcoeff * G2tensor(:,49)
  Gcoeff = (c(10)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(544)
  T2sum(1:15,40) = T2sum(1:15,40) + Gcoeff * G2tensor(:,55)
  Gcoeff = (c(10)*(M(150)-M(160)+M(174)-M(183)+M(186)-M(189)+M(192)-M(193)+M(194)-M(195)+M(197)-M(207)+M(210)-M(213)+M(221) &
    -M(228))) * den(544)
  T2sum(1:15,40) = T2sum(1:15,40) + Gcoeff * G2tensor(:,61)
  Gcoeff = (c(11)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(544)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,31)
  Gcoeff = (c(11)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(544)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,37)
  Gcoeff = (c(11)*(M(150)-M(160)+M(174)-M(183)+M(186)-M(189)+M(192)-M(193)+M(194)-M(195)+M(197)-M(207)+M(210)-M(213)+M(221) &
    -M(228))) * den(544)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,43)
  Gcoeff = (c(10)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(544)
  T2sum(1:15,41) = T2sum(1:15,41) + Gcoeff * G2tensor(:,67)
  Gcoeff = (c(10)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(544)
  T2sum(1:15,41) = T2sum(1:15,41) + Gcoeff * G2tensor(:,73)
  Gcoeff = (c(10)*(M(150)-M(160)+M(174)-M(183)+M(186)-M(189)+M(192)-M(193)+M(194)-M(195)+M(197)-M(207)+M(210)-M(213)+M(221) &
    -M(228))) * den(544)
  T2sum(1:15,41) = T2sum(1:15,41) + Gcoeff * G2tensor(:,79)
  Gcoeff = (c(7)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(544)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,85)
  Gcoeff = (c(7)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(544)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,91)
  Gcoeff = (c(7)*(M(150)-M(160)+M(174)-M(183)+M(186)-M(189)+M(192)-M(193)+M(194)-M(195)+M(197)-M(207)+M(210)-M(213)+M(221) &
    -M(228))) * den(544)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,97)
  Gcoeff = (c(3)*(M(136)-M(160)-M(180)+M(186)+M(191)-M(193)-M(195)+M(196)+M(197)-M(199)-M(204)+M(210)+M(221)-M(223)-M(228) &
    +M(234))) * den(543)
  T2sum(1:15,13) = T2sum(1:15,13) + Gcoeff * G2tensor(:,22)
  Gcoeff = (c(3)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(543)
  T2sum(1:15,13) = T2sum(1:15,13) + Gcoeff * G2tensor(:,23)
  Gcoeff = (c(3)*(-M(150)+M(160)-M(174)+M(183)-M(186)+M(189)-M(192)+M(193)-M(194)+M(195)-M(197)+M(207)-M(210)+M(213)-M(221) &
    +M(228))) * den(543)
  T2sum(1:15,13) = T2sum(1:15,13) + Gcoeff * G2tensor(:,24)
  Gcoeff = (c(3)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(546)
  T2sum(1:15,13) = T2sum(1:15,13) + Gcoeff * G2tensor(:,100)
  Gcoeff = (c(3)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(546)
  T2sum(1:15,13) = T2sum(1:15,13) + Gcoeff * G2tensor(:,101)
  Gcoeff = (c(3)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(546)
  T2sum(1:15,13) = T2sum(1:15,13) + Gcoeff * G2tensor(:,102)
  Gcoeff = (c(3)*(-M(132)+M(135)+M(156)-M(159)+M(197)-M(199)+M(203)-M(204)-M(209)+M(210)-M(219)+M(220)+M(225)-M(226)-M(239) &
    +M(241))) * den(549)
  T2sum(1:15,14) = T2sum(1:15,14) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(3)*(M(132)-M(135)-M(141)+M(151)-M(165)+M(175)+M(199)-M(203)+M(204)+M(206)-M(208)+M(212)-M(214)-M(220)+M(226) &
    -M(241))) * den(549)
  T2sum(1:15,14) = T2sum(1:15,14) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(3)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(549)
  T2sum(1:15,14) = T2sum(1:15,14) + Gcoeff * G2tensor(:,6)
  Gcoeff = (c(11)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(554)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,32)
  Gcoeff = (c(11)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(554)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,38)
  Gcoeff = (c(11)*(-M(149)+M(159)-M(173)+M(183)-M(186)+M(189)-M(197)+M(207)-M(210)+M(213)-M(216)+M(217)-M(218)+M(219)-M(221) &
    +M(227))) * den(554)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,44)
  Gcoeff = (c(10)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(554)
  T2sum(1:15,40) = T2sum(1:15,40) + Gcoeff * G2tensor(:,50)
  Gcoeff = (c(10)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(554)
  T2sum(1:15,40) = T2sum(1:15,40) + Gcoeff * G2tensor(:,56)
  Gcoeff = (c(10)*(-M(149)+M(159)-M(173)+M(183)-M(186)+M(189)-M(197)+M(207)-M(210)+M(213)-M(216)+M(217)-M(218)+M(219)-M(221) &
    +M(227))) * den(554)
  T2sum(1:15,40) = T2sum(1:15,40) + Gcoeff * G2tensor(:,62)
  Gcoeff = (c(11)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(554)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,32)
  Gcoeff = (c(11)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(554)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,38)
  Gcoeff = (c(11)*(-M(149)+M(159)-M(173)+M(183)-M(186)+M(189)-M(197)+M(207)-M(210)+M(213)-M(216)+M(217)-M(218)+M(219)-M(221) &
    +M(227))) * den(554)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,44)
  Gcoeff = (c(10)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(554)
  T2sum(1:15,41) = T2sum(1:15,41) + Gcoeff * G2tensor(:,68)
  Gcoeff = (c(10)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(554)
  T2sum(1:15,41) = T2sum(1:15,41) + Gcoeff * G2tensor(:,74)
  Gcoeff = (c(10)*(-M(149)+M(159)-M(173)+M(183)-M(186)+M(189)-M(197)+M(207)-M(210)+M(213)-M(216)+M(217)-M(218)+M(219)-M(221) &
    +M(227))) * den(554)
  T2sum(1:15,41) = T2sum(1:15,41) + Gcoeff * G2tensor(:,80)
  Gcoeff = (c(7)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(554)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,86)
  Gcoeff = (c(7)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(554)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,92)
  Gcoeff = (c(7)*(-M(149)+M(159)-M(173)+M(183)-M(186)+M(189)-M(197)+M(207)-M(210)+M(213)-M(216)+M(217)-M(218)+M(219)-M(221) &
    +M(227))) * den(554)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,98)
  Gcoeff = (c(3)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(553)
  T2sum(1:15,14) = T2sum(1:15,14) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(3)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(553)
  T2sum(1:15,14) = T2sum(1:15,14) + Gcoeff * G2tensor(:,11)
  Gcoeff = (c(3)*(-M(149)+M(159)-M(173)+M(183)-M(186)+M(189)-M(197)+M(207)-M(210)+M(213)-M(216)+M(217)-M(218)+M(219)-M(221) &
    +M(227))) * den(553)
  T2sum(1:15,14) = T2sum(1:15,14) + Gcoeff * G2tensor(:,12)
  Gcoeff = (c(11)*(M(135)-M(136)-M(159)+M(160)-M(191)+M(193)+M(195)-M(196)+M(215)-M(217)-M(219)+M(220)-M(227)+M(228)+M(233) &
    -M(234))) * den(561)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,33)
  Gcoeff = (c(11)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(561)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,39)
  Gcoeff = (c(11)*(-M(149)+M(150)+M(159)-M(160)-M(173)+M(174)+M(192)-M(193)+M(194)-M(195)-M(216)+M(217)-M(218)+M(219)+M(227) &
    -M(228))) * den(561)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,45)
  Gcoeff = (c(10)*(M(135)-M(136)-M(159)+M(160)-M(191)+M(193)+M(195)-M(196)+M(215)-M(217)-M(219)+M(220)-M(227)+M(228)+M(233) &
    -M(234))) * den(561)
  T2sum(1:15,40) = T2sum(1:15,40) + Gcoeff * G2tensor(:,51)
  Gcoeff = (c(10)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(561)
  T2sum(1:15,40) = T2sum(1:15,40) + Gcoeff * G2tensor(:,57)
  Gcoeff = (c(10)*(-M(149)+M(150)+M(159)-M(160)-M(173)+M(174)+M(192)-M(193)+M(194)-M(195)-M(216)+M(217)-M(218)+M(219)+M(227) &
    -M(228))) * den(561)
  T2sum(1:15,40) = T2sum(1:15,40) + Gcoeff * G2tensor(:,63)
  Gcoeff = (c(11)*(M(135)-M(136)-M(159)+M(160)-M(191)+M(193)+M(195)-M(196)+M(215)-M(217)-M(219)+M(220)-M(227)+M(228)+M(233) &
    -M(234))) * den(561)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,33)
  Gcoeff = (c(11)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(561)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,39)
  Gcoeff = (c(11)*(-M(149)+M(150)+M(159)-M(160)-M(173)+M(174)+M(192)-M(193)+M(194)-M(195)-M(216)+M(217)-M(218)+M(219)+M(227) &
    -M(228))) * den(561)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,45)
  Gcoeff = (c(10)*(M(135)-M(136)-M(159)+M(160)-M(191)+M(193)+M(195)-M(196)+M(215)-M(217)-M(219)+M(220)-M(227)+M(228)+M(233) &
    -M(234))) * den(561)
  T2sum(1:15,41) = T2sum(1:15,41) + Gcoeff * G2tensor(:,69)
  Gcoeff = (c(10)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(561)
  T2sum(1:15,41) = T2sum(1:15,41) + Gcoeff * G2tensor(:,75)
  Gcoeff = (c(10)*(-M(149)+M(150)+M(159)-M(160)-M(173)+M(174)+M(192)-M(193)+M(194)-M(195)-M(216)+M(217)-M(218)+M(219)+M(227) &
    -M(228))) * den(561)
  T2sum(1:15,41) = T2sum(1:15,41) + Gcoeff * G2tensor(:,81)
  Gcoeff = (c(7)*(M(135)-M(136)-M(159)+M(160)-M(191)+M(193)+M(195)-M(196)+M(215)-M(217)-M(219)+M(220)-M(227)+M(228)+M(233) &
    -M(234))) * den(561)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,87)
  Gcoeff = (c(7)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(561)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,93)
  Gcoeff = (c(7)*(-M(149)+M(150)+M(159)-M(160)-M(173)+M(174)+M(192)-M(193)+M(194)-M(195)-M(216)+M(217)-M(218)+M(219)+M(227) &
    -M(228))) * den(561)
  T2sum(1:15,23) = T2sum(1:15,23) + Gcoeff * G2tensor(:,99)
  Gcoeff = (c(3)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(569)
  T2sum(1:15,14) = T2sum(1:15,14) + Gcoeff * G2tensor(:,175)
  Gcoeff = (c(3)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(569)
  T2sum(1:15,14) = T2sum(1:15,14) + Gcoeff * G2tensor(:,176)
  Gcoeff = (c(3)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(569)
  T2sum(1:15,14) = T2sum(1:15,14) + Gcoeff * G2tensor(:,177)
  Gcoeff = (c(3)*(M(132)-M(135)-M(156)+M(159)-M(197)+M(199)-M(203)+M(204)+M(209)-M(210)+M(219)-M(220)-M(225)+M(226)+M(239) &
    -M(241))) * den(606)
  T2sum(1:15,12) = T2sum(1:15,12) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(3)*(-M(132)+M(133)-M(134)+M(135)+M(156)-M(157)+M(158)-M(159)+M(201)-M(202)-M(219)+M(220)+M(225)-M(226)-M(243) &
    +M(244))) * den(606)
  T2sum(1:15,12) = T2sum(1:15,12) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(3)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(606)
  T2sum(1:15,12) = T2sum(1:15,12) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(11)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(611)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,181)
  Gcoeff = (c(11)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(611)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,187)
  Gcoeff = (c(11)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(611)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,193)
  Gcoeff = (c(10)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(611)
  T2sum(1:15,44) = T2sum(1:15,44) + Gcoeff * G2tensor(:,199)
  Gcoeff = (c(10)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(611)
  T2sum(1:15,44) = T2sum(1:15,44) + Gcoeff * G2tensor(:,205)
  Gcoeff = (c(10)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(611)
  T2sum(1:15,44) = T2sum(1:15,44) + Gcoeff * G2tensor(:,211)
  Gcoeff = (c(11)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(611)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,181)
  Gcoeff = (c(11)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(611)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,187)
  Gcoeff = (c(11)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(611)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,193)
  Gcoeff = (c(10)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(611)
  T2sum(1:15,45) = T2sum(1:15,45) + Gcoeff * G2tensor(:,217)
  Gcoeff = (c(10)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(611)
  T2sum(1:15,45) = T2sum(1:15,45) + Gcoeff * G2tensor(:,223)
  Gcoeff = (c(10)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(611)
  T2sum(1:15,45) = T2sum(1:15,45) + Gcoeff * G2tensor(:,229)
  Gcoeff = (c(7)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(611)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,235)
  Gcoeff = (c(7)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(611)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,241)
  Gcoeff = (c(7)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(611)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,247)
  Gcoeff = (c(3)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(610)
  T2sum(1:15,12) = T2sum(1:15,12) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(3)*(M(131)-M(132)-M(155)+M(156)-M(179)+M(180)+M(185)-M(186)-M(221)+M(223)+M(225)-M(226)+M(245)-M(247)-M(249) &
    +M(250))) * den(610)
  T2sum(1:15,12) = T2sum(1:15,12) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(3)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(610)
  T2sum(1:15,12) = T2sum(1:15,12) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(3)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(613)
  T2sum(1:15,12) = T2sum(1:15,12) + Gcoeff * G2tensor(:,13)
  Gcoeff = (c(3)*(M(131)-M(132)-M(134)+M(136)-M(155)+M(156)+M(158)-M(160)-M(195)+M(196)+M(201)-M(202)+M(225)-M(226)-M(249) &
    +M(250))) * den(613)
  T2sum(1:15,12) = T2sum(1:15,12) + Gcoeff * G2tensor(:,14)
  Gcoeff = (c(3)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(613)
  T2sum(1:15,12) = T2sum(1:15,12) + Gcoeff * G2tensor(:,15)
  Gcoeff = (c(11)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(618)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,106)
  Gcoeff = (c(11)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(618)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,112)
  Gcoeff = (c(11)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(618)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,118)
  Gcoeff = (c(10)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(618)
  T2sum(1:15,42) = T2sum(1:15,42) + Gcoeff * G2tensor(:,124)
  Gcoeff = (c(10)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(618)
  T2sum(1:15,42) = T2sum(1:15,42) + Gcoeff * G2tensor(:,130)
  Gcoeff = (c(10)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(618)
  T2sum(1:15,42) = T2sum(1:15,42) + Gcoeff * G2tensor(:,136)
  Gcoeff = (c(11)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(618)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,106)
  Gcoeff = (c(11)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(618)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,112)
  Gcoeff = (c(11)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(618)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,118)
  Gcoeff = (c(10)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(618)
  T2sum(1:15,43) = T2sum(1:15,43) + Gcoeff * G2tensor(:,142)
  Gcoeff = (c(10)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(618)
  T2sum(1:15,43) = T2sum(1:15,43) + Gcoeff * G2tensor(:,148)
  Gcoeff = (c(10)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(618)
  T2sum(1:15,43) = T2sum(1:15,43) + Gcoeff * G2tensor(:,154)
  Gcoeff = (c(7)*(-M(134)+M(158)+M(179)-M(185)-M(191)+M(193)-M(197)+M(199)+M(201)-M(202)+M(204)-M(210)+M(228)-M(234)-M(245) &
    +M(247))) * den(618)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,160)
  Gcoeff = (c(7)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(618)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,166)
  Gcoeff = (c(7)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(618)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,172)
  Gcoeff = (c(3)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(617)
  T2sum(1:15,12) = T2sum(1:15,12) + Gcoeff * G2tensor(:,19)
  Gcoeff = (c(3)*(M(133)-M(134)-M(157)+M(158)-M(197)+M(199)+M(201)-M(202)-M(203)+M(204)+M(209)-M(210)+M(239)-M(241)-M(243) &
    +M(244))) * den(617)
  T2sum(1:15,12) = T2sum(1:15,12) + Gcoeff * G2tensor(:,20)
  Gcoeff = (c(3)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(617)
  T2sum(1:15,12) = T2sum(1:15,12) + Gcoeff * G2tensor(:,21)
  Gcoeff = (c(3)*(-M(136)+M(160)+M(180)-M(186)-M(191)+M(193)+M(195)-M(196)-M(197)+M(199)+M(204)-M(210)-M(221)+M(223)+M(228) &
    -M(234))) * den(620)
  T2sum(1:15,12) = T2sum(1:15,12) + Gcoeff * G2tensor(:,25)
  Gcoeff = (c(3)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(620)
  T2sum(1:15,12) = T2sum(1:15,12) + Gcoeff * G2tensor(:,26)
  Gcoeff = (c(3)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(620)
  T2sum(1:15,12) = T2sum(1:15,12) + Gcoeff * G2tensor(:,27)
  Gcoeff = (c(11)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(632)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,182)
  Gcoeff = (c(11)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(632)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,188)
  Gcoeff = (c(11)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(632)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,194)
  Gcoeff = (c(10)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(632)
  T2sum(1:15,44) = T2sum(1:15,44) + Gcoeff * G2tensor(:,200)
  Gcoeff = (c(10)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(632)
  T2sum(1:15,44) = T2sum(1:15,44) + Gcoeff * G2tensor(:,206)
  Gcoeff = (c(10)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(632)
  T2sum(1:15,44) = T2sum(1:15,44) + Gcoeff * G2tensor(:,212)
  Gcoeff = (c(11)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(632)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,182)
  Gcoeff = (c(11)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(632)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,188)
  Gcoeff = (c(11)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(632)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,194)
  Gcoeff = (c(10)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(632)
  T2sum(1:15,45) = T2sum(1:15,45) + Gcoeff * G2tensor(:,218)
  Gcoeff = (c(10)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(632)
  T2sum(1:15,45) = T2sum(1:15,45) + Gcoeff * G2tensor(:,224)
  Gcoeff = (c(10)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(632)
  T2sum(1:15,45) = T2sum(1:15,45) + Gcoeff * G2tensor(:,230)
  Gcoeff = (c(7)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(632)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,236)
  Gcoeff = (c(7)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(632)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,242)
  Gcoeff = (c(7)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(632)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,248)
  Gcoeff = (c(2)*(M(143)-M(145)-M(147)+M(148)-M(165)+M(166)+M(189)-M(190)-M(203)+M(205)+M(207)-M(208)+M(235)-M(236)-M(241) &
    +M(242))) * den(763)
  T2sum(1:5,16) = T2sum(1:5,16) + Gcoeff * G1tensor(:,146)
  Gcoeff = (c(2)*(-M(143)+M(145)+M(147)-M(148)+M(165)-M(166)-M(189)+M(190)+M(203)-M(205)-M(207)+M(208)-M(235)+M(236)+M(241) &
    -M(242))) * den(763)
  T2sum(1:5,16) = T2sum(1:5,16) + Gcoeff * G1tensor(:,147)
  Gcoeff = (c(3)*(-M(143)+M(145)+M(147)-M(148)+M(165)-M(166)-M(189)+M(190)+M(203)-M(205)-M(207)+M(208)-M(235)+M(236)+M(241) &
    -M(242))) * den(763)
  T2sum(1:5,16) = T2sum(1:5,16) + Gcoeff * G1tensor(:,148)
  Gcoeff = (c(11)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(643)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,183)
  Gcoeff = (c(11)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(643)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,189)
  Gcoeff = (c(11)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(643)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,195)
  Gcoeff = (c(10)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(643)
  T2sum(1:15,44) = T2sum(1:15,44) + Gcoeff * G2tensor(:,201)
  Gcoeff = (c(10)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(643)
  T2sum(1:15,44) = T2sum(1:15,44) + Gcoeff * G2tensor(:,207)
  Gcoeff = (c(10)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(643)
  T2sum(1:15,44) = T2sum(1:15,44) + Gcoeff * G2tensor(:,213)
  Gcoeff = (c(11)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(643)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,183)
  Gcoeff = (c(11)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(643)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,189)
  Gcoeff = (c(11)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(643)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,195)
  Gcoeff = (c(10)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(643)
  T2sum(1:15,45) = T2sum(1:15,45) + Gcoeff * G2tensor(:,219)
  Gcoeff = (c(10)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(643)
  T2sum(1:15,45) = T2sum(1:15,45) + Gcoeff * G2tensor(:,225)
  Gcoeff = (c(10)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(643)
  T2sum(1:15,45) = T2sum(1:15,45) + Gcoeff * G2tensor(:,231)
  Gcoeff = (c(7)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(643)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,237)
  Gcoeff = (c(7)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(643)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,243)
  Gcoeff = (c(7)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(643)
  T2sum(1:15,25) = T2sum(1:15,25) + Gcoeff * G2tensor(:,249)
  Gcoeff = (c(11)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(650)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,107)
  Gcoeff = (c(11)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(650)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,113)
  Gcoeff = (c(11)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(650)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,119)
  Gcoeff = (c(10)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(650)
  T2sum(1:15,42) = T2sum(1:15,42) + Gcoeff * G2tensor(:,125)
  Gcoeff = (c(10)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(650)
  T2sum(1:15,42) = T2sum(1:15,42) + Gcoeff * G2tensor(:,131)
  Gcoeff = (c(10)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(650)
  T2sum(1:15,42) = T2sum(1:15,42) + Gcoeff * G2tensor(:,137)
  Gcoeff = (c(11)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(650)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,107)
  Gcoeff = (c(11)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(650)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,113)
  Gcoeff = (c(11)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(650)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,119)
  Gcoeff = (c(10)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(650)
  T2sum(1:15,43) = T2sum(1:15,43) + Gcoeff * G2tensor(:,143)
  Gcoeff = (c(10)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(650)
  T2sum(1:15,43) = T2sum(1:15,43) + Gcoeff * G2tensor(:,149)
  Gcoeff = (c(10)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(650)
  T2sum(1:15,43) = T2sum(1:15,43) + Gcoeff * G2tensor(:,155)
  Gcoeff = (c(7)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(650)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,161)
  Gcoeff = (c(7)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(650)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,167)
  Gcoeff = (c(7)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(650)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,173)
  Gcoeff = (c(11)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(657)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,108)
  Gcoeff = (c(11)*(-M(133)+M(134)+M(143)-M(144)+M(167)-M(168)+M(197)-M(198)-M(200)+M(202)-M(209)+M(210)-M(239)+M(240)+M(242) &
    -M(244))) * den(657)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,114)
  Gcoeff = (c(11)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(657)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,120)
  Gcoeff = (c(10)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(657)
  T2sum(1:15,42) = T2sum(1:15,42) + Gcoeff * G2tensor(:,126)
  Gcoeff = (c(10)*(-M(133)+M(134)+M(143)-M(144)+M(167)-M(168)+M(197)-M(198)-M(200)+M(202)-M(209)+M(210)-M(239)+M(240)+M(242) &
    -M(244))) * den(657)
  T2sum(1:15,42) = T2sum(1:15,42) + Gcoeff * G2tensor(:,132)
  Gcoeff = (c(10)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(657)
  T2sum(1:15,42) = T2sum(1:15,42) + Gcoeff * G2tensor(:,138)
  Gcoeff = (c(11)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(657)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,108)
  Gcoeff = (c(11)*(-M(133)+M(134)+M(143)-M(144)+M(167)-M(168)+M(197)-M(198)-M(200)+M(202)-M(209)+M(210)-M(239)+M(240)+M(242) &
    -M(244))) * den(657)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,114)
  Gcoeff = (c(11)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(657)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,120)
  Gcoeff = (c(10)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(657)
  T2sum(1:15,43) = T2sum(1:15,43) + Gcoeff * G2tensor(:,144)
  Gcoeff = (c(10)*(-M(133)+M(134)+M(143)-M(144)+M(167)-M(168)+M(197)-M(198)-M(200)+M(202)-M(209)+M(210)-M(239)+M(240)+M(242) &
    -M(244))) * den(657)
  T2sum(1:15,43) = T2sum(1:15,43) + Gcoeff * G2tensor(:,150)
  Gcoeff = (c(10)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(657)
  T2sum(1:15,43) = T2sum(1:15,43) + Gcoeff * G2tensor(:,156)
  Gcoeff = (c(7)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(657)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,162)
  Gcoeff = (c(7)*(-M(133)+M(134)+M(143)-M(144)+M(167)-M(168)+M(197)-M(198)-M(200)+M(202)-M(209)+M(210)-M(239)+M(240)+M(242) &
    -M(244))) * den(657)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,168)
  Gcoeff = (c(7)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(657)
  T2sum(1:15,24) = T2sum(1:15,24) + Gcoeff * G2tensor(:,174)
  Gcoeff = (c(3)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(712)
  T2sum(1:15,16) = T2sum(1:15,16) + Gcoeff * G2tensor(:,256)
  Gcoeff = (c(3)*(-M(143)+M(145)+M(147)-M(148)+M(165)-M(166)-M(189)+M(190)+M(203)-M(205)-M(207)+M(208)-M(235)+M(236)+M(241) &
    -M(242))) * den(712)
  T2sum(1:15,16) = T2sum(1:15,16) + Gcoeff * G2tensor(:,257)
  Gcoeff = (c(3)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(712)
  T2sum(1:15,16) = T2sum(1:15,16) + Gcoeff * G2tensor(:,258)
  Gcoeff = (c(3)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(793)
  T2sum(1:15,16) = T2sum(1:15,16) + Gcoeff * G2tensor(:,259)
  Gcoeff = (c(3)*(-M(131)+M(137)+M(143)-M(145)+M(149)-M(151)-M(153)+M(154)+M(164)-M(188)-M(212)+M(218)-M(236)+M(242)+M(248) &
    -M(250))) * den(794)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,253)
  Gcoeff = (c(3)*(M(133)-M(137)+M(139)-M(143)-M(150)+M(151)-M(152)+M(153)-M(170)+M(188)-M(194)+M(212)+M(238)-M(242)+M(244) &
    -M(248))) * den(807)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,254)
  Gcoeff = (c(3)*(-M(131)+M(133)+M(139)-M(145)+M(149)-M(150)-M(152)+M(154)+M(164)-M(170)-M(194)+M(218)-M(236)+M(238)+M(244) &
    -M(250))) * den(819)
  T2sum(1:15,17) = T2sum(1:15,17) + Gcoeff * G2tensor(:,255)
  Gcoeff = (c(3)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(1051)
  T2sum(1:15,16) = T2sum(1:15,16) + Gcoeff * G2tensor(:,260)
  Gcoeff = (c(3)*(M(143)-M(145)-M(147)+M(148)-M(165)+M(166)+M(189)-M(190)-M(203)+M(205)+M(207)-M(208)+M(235)-M(236)-M(241) &
    +M(242))) * den(1062)
  T2sum(1:15,16) = T2sum(1:15,16) + Gcoeff * G2tensor(:,261)

end subroutine vamp_93

end module ol_vamp_93_ppjjjj_gggggg_1_/**/REALKIND
