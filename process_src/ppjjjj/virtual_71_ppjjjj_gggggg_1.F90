
module ol_vamp_71_ppjjjj_gggggg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_71(M)
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
  complex(REALKIND), dimension(1,348) :: G0tensor
  complex(REALKIND), dimension(5,116) :: G1tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,2),wf(:,0),G0(:,:,:,2))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,-1),G0tensor(:,1))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,-2),G0tensor(:,2))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,-1),G0tensor(:,3))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,1))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,2),G0(:,:,:,3))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-2),wf(:,-1),G0tensor(:,4))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-1),wf(:,-2),G0tensor(:,5))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-2),wf(:,-1),G0tensor(:,6))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,2))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,5),G0(:,:,:,4))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-2),wf(:,-1),G0tensor(:,7))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-1),wf(:,-2),G0tensor(:,8))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-2),wf(:,-1),G0tensor(:,9))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,3))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,5),wf(:,0),G0(:,:,:,5))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,5),wf(:,-2),wf(:,-1),G0tensor(:,10))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,5),wf(:,-1),wf(:,-2),G0tensor(:,11))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,5),wf(:,-2),wf(:,-1),G0tensor(:,12))
  call check_last_UV_W(l_switch,G0(:,:,:,5),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,4))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,5),G0(:,:,:,6))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,6),wf(:,-2),wf(:,-1),G0tensor(:,13))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,6),wf(:,-1),wf(:,-2),G0tensor(:,14))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,6),wf(:,-2),wf(:,-1),G0tensor(:,15))
  call check_last_UV_W(l_switch,G0(:,:,:,6),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,5))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,6),G0(:,:,:,7))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,7),wf(:,-2),wf(:,-1),G0tensor(:,16))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,7),wf(:,-1),wf(:,-2),G0tensor(:,17))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,7),wf(:,-2),wf(:,-1),G0tensor(:,18))
  call check_last_UV_W(l_switch,G0(:,:,:,7),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,6))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,6),wf(:,0),G0(:,:,:,8))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,8),wf(:,-2),wf(:,-1),G0tensor(:,19))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,8),wf(:,-1),wf(:,-2),G0tensor(:,20))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,8),wf(:,-2),wf(:,-1),G0tensor(:,21))
  call check_last_UV_W(l_switch,G0(:,:,:,8),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,7))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,6),G0(:,:,:,9))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,9),wf(:,-2),wf(:,-1),G0tensor(:,22))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,9),wf(:,-1),wf(:,-2),G0tensor(:,23))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,9),wf(:,-2),wf(:,-1),G0tensor(:,24))
  call check_last_UV_W(l_switch,G0(:,:,:,9),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,8))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,49),G0(:,:,:,10))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,10),wf(:,-4),wf(:,-1),G0tensor(:,25))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,10),wf(:,-1),wf(:,-4),G0tensor(:,26))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,10),wf(:,-4),wf(:,-1),G0tensor(:,27))
  call check_last_UV_W(l_switch,G0(:,:,:,10),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,9))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,51),G0(:,:,:,11))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,11),wf(:,-4),wf(:,-1),G0tensor(:,28))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,11),wf(:,-1),wf(:,-4),G0tensor(:,29))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,11),wf(:,-4),wf(:,-1),G0tensor(:,30))
  call check_last_UV_W(l_switch,G0(:,:,:,11),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,10))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,52),G0(:,:,:,12))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-4),wf(:,-1),G0tensor(:,31))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-1),wf(:,-4),G0tensor(:,32))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,-4),wf(:,-1),G0tensor(:,33))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,11))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,49),wf(:,-2),G0(:,:,:,13))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-4),wf(:,-1),G0tensor(:,34))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-1),wf(:,-4),G0tensor(:,35))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-4),wf(:,-1),G0tensor(:,36))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,12))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,51),wf(:,-2),G0(:,:,:,14))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,14),wf(:,-4),wf(:,-1),G0tensor(:,37))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,14),wf(:,-1),wf(:,-4),G0tensor(:,38))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,14),wf(:,-4),wf(:,-1),G0tensor(:,39))
  call check_last_UV_W(l_switch,G0(:,:,:,14),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,13))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,52),wf(:,-2),G0(:,:,:,15))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,15),wf(:,-4),wf(:,-1),G0tensor(:,40))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,15),wf(:,-1),wf(:,-4),G0tensor(:,41))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,15),wf(:,-4),wf(:,-1),G0tensor(:,42))
  call check_last_UV_W(l_switch,G0(:,:,:,15),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,14))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,49),G0(:,:,:,16))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,16),wf(:,-4),wf(:,-1),G0tensor(:,43))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,16),wf(:,-1),wf(:,-4),G0tensor(:,44))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,16),wf(:,-4),wf(:,-1),G0tensor(:,45))
  call check_last_UV_W(l_switch,G0(:,:,:,16),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,15))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,51),G0(:,:,:,17))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,17),wf(:,-4),wf(:,-1),G0tensor(:,46))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,17),wf(:,-1),wf(:,-4),G0tensor(:,47))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,17),wf(:,-4),wf(:,-1),G0tensor(:,48))
  call check_last_UV_W(l_switch,G0(:,:,:,17),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,16))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,52),G0(:,:,:,18))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,18),wf(:,-4),wf(:,-1),G0tensor(:,49))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,18),wf(:,-1),wf(:,-4),G0tensor(:,50))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,18),wf(:,-4),wf(:,-1),G0tensor(:,51))
  call check_last_UV_W(l_switch,G0(:,:,:,18),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,17))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,26),G0(:,:,:,19))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,19),wf(:,-3),wf(:,0),G0tensor(:,52))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,19),wf(:,0),wf(:,-3),G0tensor(:,53))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,19),wf(:,-3),wf(:,0),G0tensor(:,54))
  call check_last_UV_W(l_switch,G0(:,:,:,19),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,18))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,29),G0(:,:,:,20))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,20),wf(:,-3),wf(:,0),G0tensor(:,55))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,20),wf(:,0),wf(:,-3),G0tensor(:,56))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,20),wf(:,-3),wf(:,0),G0tensor(:,57))
  call check_last_UV_W(l_switch,G0(:,:,:,20),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,19))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,30),G0(:,:,:,21))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,21),wf(:,-3),wf(:,0),G0tensor(:,58))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,21),wf(:,0),wf(:,-3),G0tensor(:,59))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,21),wf(:,-3),wf(:,0),G0tensor(:,60))
  call check_last_UV_W(l_switch,G0(:,:,:,21),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,20))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,26),wf(:,-2),G0(:,:,:,22))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,22),wf(:,-3),wf(:,0),G0tensor(:,61))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,22),wf(:,0),wf(:,-3),G0tensor(:,62))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,22),wf(:,-3),wf(:,0),G0tensor(:,63))
  call check_last_UV_W(l_switch,G0(:,:,:,22),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,21))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,29),wf(:,-2),G0(:,:,:,23))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,23),wf(:,-3),wf(:,0),G0tensor(:,64))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,23),wf(:,0),wf(:,-3),G0tensor(:,65))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,23),wf(:,-3),wf(:,0),G0tensor(:,66))
  call check_last_UV_W(l_switch,G0(:,:,:,23),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,22))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,30),wf(:,-2),G0(:,:,:,24))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,24),wf(:,-3),wf(:,0),G0tensor(:,67))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,24),wf(:,0),wf(:,-3),G0tensor(:,68))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,24),wf(:,-3),wf(:,0),G0tensor(:,69))
  call check_last_UV_W(l_switch,G0(:,:,:,24),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,23))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,26),G0(:,:,:,25))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,25),wf(:,-3),wf(:,0),G0tensor(:,70))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,25),wf(:,0),wf(:,-3),G0tensor(:,71))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,25),wf(:,-3),wf(:,0),G0tensor(:,72))
  call check_last_UV_W(l_switch,G0(:,:,:,25),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,24))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,29),G0(:,:,:,26))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,26),wf(:,-3),wf(:,0),G0tensor(:,73))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,26),wf(:,0),wf(:,-3),G0tensor(:,74))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,26),wf(:,-3),wf(:,0),G0tensor(:,75))
  call check_last_UV_W(l_switch,G0(:,:,:,26),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,25))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,30),G0(:,:,:,27))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,27),wf(:,-3),wf(:,0),G0tensor(:,76))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,27),wf(:,0),wf(:,-3),G0tensor(:,77))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,27),wf(:,-3),wf(:,0),G0tensor(:,78))
  call check_last_UV_W(l_switch,G0(:,:,:,27),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,26))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,49),G0(:,:,:,28))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,28),wf(:,-4),wf(:,-2),G0tensor(:,79))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,28),wf(:,-2),wf(:,-4),G0tensor(:,80))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,28),wf(:,-4),wf(:,-2),G0tensor(:,81))
  call check_last_UV_W(l_switch,G0(:,:,:,28),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,27))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,51),G0(:,:,:,29))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,29),wf(:,-4),wf(:,-2),G0tensor(:,82))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,29),wf(:,-2),wf(:,-4),G0tensor(:,83))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,29),wf(:,-4),wf(:,-2),G0tensor(:,84))
  call check_last_UV_W(l_switch,G0(:,:,:,29),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,28))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,52),G0(:,:,:,30))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,30),wf(:,-4),wf(:,-2),G0tensor(:,85))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,30),wf(:,-2),wf(:,-4),G0tensor(:,86))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,30),wf(:,-4),wf(:,-2),G0tensor(:,87))
  call check_last_UV_W(l_switch,G0(:,:,:,30),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,29))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,49),wf(:,-1),G0(:,:,:,31))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,31),wf(:,-4),wf(:,-2),G0tensor(:,88))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,31),wf(:,-2),wf(:,-4),G0tensor(:,89))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,31),wf(:,-4),wf(:,-2),G0tensor(:,90))
  call check_last_UV_W(l_switch,G0(:,:,:,31),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,30))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,51),wf(:,-1),G0(:,:,:,32))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,32),wf(:,-4),wf(:,-2),G0tensor(:,91))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,32),wf(:,-2),wf(:,-4),G0tensor(:,92))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,32),wf(:,-4),wf(:,-2),G0tensor(:,93))
  call check_last_UV_W(l_switch,G0(:,:,:,32),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,31))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,52),wf(:,-1),G0(:,:,:,33))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,33),wf(:,-4),wf(:,-2),G0tensor(:,94))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,33),wf(:,-2),wf(:,-4),G0tensor(:,95))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,33),wf(:,-4),wf(:,-2),G0tensor(:,96))
  call check_last_UV_W(l_switch,G0(:,:,:,33),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,32))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,49),G0(:,:,:,34))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,34),wf(:,-4),wf(:,-2),G0tensor(:,97))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,34),wf(:,-2),wf(:,-4),G0tensor(:,98))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,34),wf(:,-4),wf(:,-2),G0tensor(:,99))
  call check_last_UV_W(l_switch,G0(:,:,:,34),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,33))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,51),G0(:,:,:,35))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,35),wf(:,-4),wf(:,-2),G0tensor(:,100))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,35),wf(:,-2),wf(:,-4),G0tensor(:,101))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,35),wf(:,-4),wf(:,-2),G0tensor(:,102))
  call check_last_UV_W(l_switch,G0(:,:,:,35),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,34))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,52),G0(:,:,:,36))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,36),wf(:,-4),wf(:,-2),G0tensor(:,103))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,36),wf(:,-2),wf(:,-4),G0tensor(:,104))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,36),wf(:,-4),wf(:,-2),G0tensor(:,105))
  call check_last_UV_W(l_switch,G0(:,:,:,36),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,35))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,8),G0(:,:,:,37))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,37),wf(:,-3),wf(:,0),G0tensor(:,106))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,37),wf(:,0),wf(:,-3),G0tensor(:,107))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,37),wf(:,-3),wf(:,0),G0tensor(:,108))
  call check_last_UV_W(l_switch,G0(:,:,:,37),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,36))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,8),wf(:,-1),G0(:,:,:,38))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,38),wf(:,-3),wf(:,0),G0tensor(:,109))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,38),wf(:,0),wf(:,-3),G0tensor(:,110))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,38),wf(:,-3),wf(:,0),G0tensor(:,111))
  call check_last_UV_W(l_switch,G0(:,:,:,38),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,37))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,8),G0(:,:,:,39))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,39),wf(:,-3),wf(:,0),G0tensor(:,112))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,39),wf(:,0),wf(:,-3),G0tensor(:,113))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,39),wf(:,-3),wf(:,0),G0tensor(:,114))
  call check_last_UV_W(l_switch,G0(:,:,:,39),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,38))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,11),G0(:,:,:,40))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,40),wf(:,-3),wf(:,0),G0tensor(:,115))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,40),wf(:,0),wf(:,-3),G0tensor(:,116))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,40),wf(:,-3),wf(:,0),G0tensor(:,117))
  call check_last_UV_W(l_switch,G0(:,:,:,40),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,39))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,11),wf(:,-1),G0(:,:,:,41))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,41),wf(:,-3),wf(:,0),G0tensor(:,118))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,41),wf(:,0),wf(:,-3),G0tensor(:,119))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,41),wf(:,-3),wf(:,0),G0tensor(:,120))
  call check_last_UV_W(l_switch,G0(:,:,:,41),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,40))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,11),G0(:,:,:,42))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,42),wf(:,-3),wf(:,0),G0tensor(:,121))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,42),wf(:,0),wf(:,-3),G0tensor(:,122))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,42),wf(:,-3),wf(:,0),G0tensor(:,123))
  call check_last_UV_W(l_switch,G0(:,:,:,42),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,41))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,12),G0(:,:,:,43))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,43),wf(:,-3),wf(:,0),G0tensor(:,124))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,43),wf(:,0),wf(:,-3),G0tensor(:,125))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,43),wf(:,-3),wf(:,0),G0tensor(:,126))
  call check_last_UV_W(l_switch,G0(:,:,:,43),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,42))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,12),wf(:,-1),G0(:,:,:,44))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,44),wf(:,-3),wf(:,0),G0tensor(:,127))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,44),wf(:,0),wf(:,-3),G0tensor(:,128))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,44),wf(:,-3),wf(:,0),G0tensor(:,129))
  call check_last_UV_W(l_switch,G0(:,:,:,44),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,43))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,12),G0(:,:,:,45))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,45),wf(:,-3),wf(:,0),G0tensor(:,130))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,45),wf(:,0),wf(:,-3),G0tensor(:,131))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,45),wf(:,-3),wf(:,0),G0tensor(:,132))
  call check_last_UV_W(l_switch,G0(:,:,:,45),Q(:,54),wf(:,104),Q(:,9),G1tensor(:,44))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,55),G0(:,:,:,46))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,46),wf(:,-3),wf(:,-1),G0tensor(:,133))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,46),wf(:,-1),wf(:,-3),G0tensor(:,134))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,46),wf(:,-3),wf(:,-1),G0tensor(:,135))
  call check_last_UV_W(l_switch,G0(:,:,:,46),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,45))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,57),G0(:,:,:,47))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-3),wf(:,-1),G0tensor(:,136))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-1),wf(:,-3),G0tensor(:,137))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,-3),wf(:,-1),G0tensor(:,138))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,46))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,58),G0(:,:,:,48))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-3),wf(:,-1),G0tensor(:,139))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-1),wf(:,-3),G0tensor(:,140))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,-3),wf(:,-1),G0tensor(:,141))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,47))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,55),wf(:,-2),G0(:,:,:,49))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,49),wf(:,-3),wf(:,-1),G0tensor(:,142))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,49),wf(:,-1),wf(:,-3),G0tensor(:,143))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,49),wf(:,-3),wf(:,-1),G0tensor(:,144))
  call check_last_UV_W(l_switch,G0(:,:,:,49),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,48))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,57),wf(:,-2),G0(:,:,:,50))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,50),wf(:,-3),wf(:,-1),G0tensor(:,145))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,50),wf(:,-1),wf(:,-3),G0tensor(:,146))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,50),wf(:,-3),wf(:,-1),G0tensor(:,147))
  call check_last_UV_W(l_switch,G0(:,:,:,50),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,49))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,58),wf(:,-2),G0(:,:,:,51))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,51),wf(:,-3),wf(:,-1),G0tensor(:,148))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,51),wf(:,-1),wf(:,-3),G0tensor(:,149))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,51),wf(:,-3),wf(:,-1),G0tensor(:,150))
  call check_last_UV_W(l_switch,G0(:,:,:,51),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,50))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,55),G0(:,:,:,52))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,52),wf(:,-3),wf(:,-1),G0tensor(:,151))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,52),wf(:,-1),wf(:,-3),G0tensor(:,152))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,52),wf(:,-3),wf(:,-1),G0tensor(:,153))
  call check_last_UV_W(l_switch,G0(:,:,:,52),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,51))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,57),G0(:,:,:,53))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,53),wf(:,-3),wf(:,-1),G0tensor(:,154))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,53),wf(:,-1),wf(:,-3),G0tensor(:,155))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,53),wf(:,-3),wf(:,-1),G0tensor(:,156))
  call check_last_UV_W(l_switch,G0(:,:,:,53),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,52))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,58),G0(:,:,:,54))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,54),wf(:,-3),wf(:,-1),G0tensor(:,157))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,54),wf(:,-1),wf(:,-3),G0tensor(:,158))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,54),wf(:,-3),wf(:,-1),G0tensor(:,159))
  call check_last_UV_W(l_switch,G0(:,:,:,54),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,53))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,32),G0(:,:,:,55))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,55),wf(:,-4),wf(:,0),G0tensor(:,160))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,55),wf(:,0),wf(:,-4),G0tensor(:,161))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,55),wf(:,-4),wf(:,0),G0tensor(:,162))
  call check_last_UV_W(l_switch,G0(:,:,:,55),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,54))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,35),G0(:,:,:,56))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,56),wf(:,-4),wf(:,0),G0tensor(:,163))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,56),wf(:,0),wf(:,-4),G0tensor(:,164))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,56),wf(:,-4),wf(:,0),G0tensor(:,165))
  call check_last_UV_W(l_switch,G0(:,:,:,56),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,55))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,36),G0(:,:,:,57))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,57),wf(:,-4),wf(:,0),G0tensor(:,166))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,57),wf(:,0),wf(:,-4),G0tensor(:,167))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,57),wf(:,-4),wf(:,0),G0tensor(:,168))
  call check_last_UV_W(l_switch,G0(:,:,:,57),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,56))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,32),wf(:,-2),G0(:,:,:,58))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,58),wf(:,-4),wf(:,0),G0tensor(:,169))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,58),wf(:,0),wf(:,-4),G0tensor(:,170))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,58),wf(:,-4),wf(:,0),G0tensor(:,171))
  call check_last_UV_W(l_switch,G0(:,:,:,58),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,57))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,35),wf(:,-2),G0(:,:,:,59))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,59),wf(:,-4),wf(:,0),G0tensor(:,172))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,59),wf(:,0),wf(:,-4),G0tensor(:,173))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,59),wf(:,-4),wf(:,0),G0tensor(:,174))
  call check_last_UV_W(l_switch,G0(:,:,:,59),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,58))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,36),wf(:,-2),G0(:,:,:,60))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,60),wf(:,-4),wf(:,0),G0tensor(:,175))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,60),wf(:,0),wf(:,-4),G0tensor(:,176))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,60),wf(:,-4),wf(:,0),G0tensor(:,177))
  call check_last_UV_W(l_switch,G0(:,:,:,60),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,59))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,32),G0(:,:,:,61))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,61),wf(:,-4),wf(:,0),G0tensor(:,178))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,61),wf(:,0),wf(:,-4),G0tensor(:,179))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,61),wf(:,-4),wf(:,0),G0tensor(:,180))
  call check_last_UV_W(l_switch,G0(:,:,:,61),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,60))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,35),G0(:,:,:,62))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,62),wf(:,-4),wf(:,0),G0tensor(:,181))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,62),wf(:,0),wf(:,-4),G0tensor(:,182))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,62),wf(:,-4),wf(:,0),G0tensor(:,183))
  call check_last_UV_W(l_switch,G0(:,:,:,62),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,61))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,36),G0(:,:,:,63))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,63),wf(:,-4),wf(:,0),G0tensor(:,184))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,63),wf(:,0),wf(:,-4),G0tensor(:,185))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,63),wf(:,-4),wf(:,0),G0tensor(:,186))
  call check_last_UV_W(l_switch,G0(:,:,:,63),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,62))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,32),G0(:,:,:,64))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,64),wf(:,-4),wf(:,-2),G0tensor(:,187))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,64),wf(:,-2),wf(:,-4),G0tensor(:,188))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,64),wf(:,-4),wf(:,-2),G0tensor(:,189))
  call check_last_UV_W(l_switch,G0(:,:,:,64),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,63))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,32),wf(:,0),G0(:,:,:,65))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,65),wf(:,-4),wf(:,-2),G0tensor(:,190))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,65),wf(:,-2),wf(:,-4),G0tensor(:,191))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,65),wf(:,-4),wf(:,-2),G0tensor(:,192))
  call check_last_UV_W(l_switch,G0(:,:,:,65),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,64))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,32),G0(:,:,:,66))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,66),wf(:,-4),wf(:,-2),G0tensor(:,193))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,66),wf(:,-2),wf(:,-4),G0tensor(:,194))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,66),wf(:,-4),wf(:,-2),G0tensor(:,195))
  call check_last_UV_W(l_switch,G0(:,:,:,66),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,65))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,35),G0(:,:,:,67))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,67),wf(:,-4),wf(:,-2),G0tensor(:,196))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,67),wf(:,-2),wf(:,-4),G0tensor(:,197))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,67),wf(:,-4),wf(:,-2),G0tensor(:,198))
  call check_last_UV_W(l_switch,G0(:,:,:,67),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,66))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,35),wf(:,0),G0(:,:,:,68))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,68),wf(:,-4),wf(:,-2),G0tensor(:,199))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,68),wf(:,-2),wf(:,-4),G0tensor(:,200))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,68),wf(:,-4),wf(:,-2),G0tensor(:,201))
  call check_last_UV_W(l_switch,G0(:,:,:,68),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,67))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,35),G0(:,:,:,69))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,69),wf(:,-4),wf(:,-2),G0tensor(:,202))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,69),wf(:,-2),wf(:,-4),G0tensor(:,203))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,69),wf(:,-4),wf(:,-2),G0tensor(:,204))
  call check_last_UV_W(l_switch,G0(:,:,:,69),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,68))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,36),G0(:,:,:,70))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,70),wf(:,-4),wf(:,-2),G0tensor(:,205))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,70),wf(:,-2),wf(:,-4),G0tensor(:,206))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,70),wf(:,-4),wf(:,-2),G0tensor(:,207))
  call check_last_UV_W(l_switch,G0(:,:,:,70),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,69))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,36),wf(:,0),G0(:,:,:,71))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,71),wf(:,-4),wf(:,-2),G0tensor(:,208))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,71),wf(:,-2),wf(:,-4),G0tensor(:,209))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,71),wf(:,-4),wf(:,-2),G0tensor(:,210))
  call check_last_UV_W(l_switch,G0(:,:,:,71),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,70))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,36),G0(:,:,:,72))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,72),wf(:,-4),wf(:,-2),G0tensor(:,211))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,72),wf(:,-2),wf(:,-4),G0tensor(:,212))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,72),wf(:,-4),wf(:,-2),G0tensor(:,213))
  call check_last_UV_W(l_switch,G0(:,:,:,72),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,71))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,8),G0(:,:,:,73))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,73),wf(:,-3),wf(:,-1),G0tensor(:,214))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,73),wf(:,-1),wf(:,-3),G0tensor(:,215))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,73),wf(:,-3),wf(:,-1),G0tensor(:,216))
  call check_last_UV_W(l_switch,G0(:,:,:,73),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,72))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,8),wf(:,0),G0(:,:,:,74))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,74),wf(:,-3),wf(:,-1),G0tensor(:,217))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,74),wf(:,-1),wf(:,-3),G0tensor(:,218))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,74),wf(:,-3),wf(:,-1),G0tensor(:,219))
  call check_last_UV_W(l_switch,G0(:,:,:,74),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,73))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,8),G0(:,:,:,75))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,75),wf(:,-3),wf(:,-1),G0tensor(:,220))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,75),wf(:,-1),wf(:,-3),G0tensor(:,221))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,75),wf(:,-3),wf(:,-1),G0tensor(:,222))
  call check_last_UV_W(l_switch,G0(:,:,:,75),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,74))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,11),G0(:,:,:,76))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,76),wf(:,-3),wf(:,-1),G0tensor(:,223))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,76),wf(:,-1),wf(:,-3),G0tensor(:,224))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,76),wf(:,-3),wf(:,-1),G0tensor(:,225))
  call check_last_UV_W(l_switch,G0(:,:,:,76),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,75))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,11),wf(:,0),G0(:,:,:,77))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,77),wf(:,-3),wf(:,-1),G0tensor(:,226))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,77),wf(:,-1),wf(:,-3),G0tensor(:,227))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,77),wf(:,-3),wf(:,-1),G0tensor(:,228))
  call check_last_UV_W(l_switch,G0(:,:,:,77),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,76))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,11),G0(:,:,:,78))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,78),wf(:,-3),wf(:,-1),G0tensor(:,229))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,78),wf(:,-1),wf(:,-3),G0tensor(:,230))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,78),wf(:,-3),wf(:,-1),G0tensor(:,231))
  call check_last_UV_W(l_switch,G0(:,:,:,78),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,77))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,12),G0(:,:,:,79))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,79),wf(:,-3),wf(:,-1),G0tensor(:,232))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,79),wf(:,-1),wf(:,-3),G0tensor(:,233))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,79),wf(:,-3),wf(:,-1),G0tensor(:,234))
  call check_last_UV_W(l_switch,G0(:,:,:,79),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,78))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,12),wf(:,0),G0(:,:,:,80))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,80),wf(:,-3),wf(:,-1),G0tensor(:,235))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,80),wf(:,-1),wf(:,-3),G0tensor(:,236))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,80),wf(:,-3),wf(:,-1),G0tensor(:,237))
  call check_last_UV_W(l_switch,G0(:,:,:,80),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,79))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,12),G0(:,:,:,81))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,81),wf(:,-3),wf(:,-1),G0tensor(:,238))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,81),wf(:,-1),wf(:,-3),G0tensor(:,239))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,81),wf(:,-3),wf(:,-1),G0tensor(:,240))
  call check_last_UV_W(l_switch,G0(:,:,:,81),Q(:,53),wf(:,91),Q(:,10),G1tensor(:,80))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,55),G0(:,:,:,82))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,82),wf(:,-3),wf(:,-2),G0tensor(:,241))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,82),wf(:,-2),wf(:,-3),G0tensor(:,242))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,82),wf(:,-3),wf(:,-2),G0tensor(:,243))
  call check_last_UV_W(l_switch,G0(:,:,:,82),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,81))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,57),G0(:,:,:,83))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,83),wf(:,-3),wf(:,-2),G0tensor(:,244))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,83),wf(:,-2),wf(:,-3),G0tensor(:,245))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,83),wf(:,-3),wf(:,-2),G0tensor(:,246))
  call check_last_UV_W(l_switch,G0(:,:,:,83),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,82))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,58),G0(:,:,:,84))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,84),wf(:,-3),wf(:,-2),G0tensor(:,247))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,84),wf(:,-2),wf(:,-3),G0tensor(:,248))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,84),wf(:,-3),wf(:,-2),G0tensor(:,249))
  call check_last_UV_W(l_switch,G0(:,:,:,84),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,83))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,55),wf(:,-1),G0(:,:,:,85))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,85),wf(:,-3),wf(:,-2),G0tensor(:,250))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,85),wf(:,-2),wf(:,-3),G0tensor(:,251))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,85),wf(:,-3),wf(:,-2),G0tensor(:,252))
  call check_last_UV_W(l_switch,G0(:,:,:,85),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,84))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,57),wf(:,-1),G0(:,:,:,86))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,86),wf(:,-3),wf(:,-2),G0tensor(:,253))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,86),wf(:,-2),wf(:,-3),G0tensor(:,254))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,86),wf(:,-3),wf(:,-2),G0tensor(:,255))
  call check_last_UV_W(l_switch,G0(:,:,:,86),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,85))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,58),wf(:,-1),G0(:,:,:,87))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,87),wf(:,-3),wf(:,-2),G0tensor(:,256))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,87),wf(:,-2),wf(:,-3),G0tensor(:,257))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,87),wf(:,-3),wf(:,-2),G0tensor(:,258))
  call check_last_UV_W(l_switch,G0(:,:,:,87),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,86))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,55),G0(:,:,:,88))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,88),wf(:,-3),wf(:,-2),G0tensor(:,259))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,88),wf(:,-2),wf(:,-3),G0tensor(:,260))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,88),wf(:,-3),wf(:,-2),G0tensor(:,261))
  call check_last_UV_W(l_switch,G0(:,:,:,88),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,87))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,57),G0(:,:,:,89))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,89),wf(:,-3),wf(:,-2),G0tensor(:,262))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,89),wf(:,-2),wf(:,-3),G0tensor(:,263))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,89),wf(:,-3),wf(:,-2),G0tensor(:,264))
  call check_last_UV_W(l_switch,G0(:,:,:,89),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,88))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,58),G0(:,:,:,90))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,90),wf(:,-3),wf(:,-2),G0tensor(:,265))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,90),wf(:,-2),wf(:,-3),G0tensor(:,266))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,90),wf(:,-3),wf(:,-2),G0tensor(:,267))
  call check_last_UV_W(l_switch,G0(:,:,:,90),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,89))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,14),G0(:,:,:,91))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,91),wf(:,-4),wf(:,0),G0tensor(:,268))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,91),wf(:,0),wf(:,-4),G0tensor(:,269))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,91),wf(:,-4),wf(:,0),G0tensor(:,270))
  call check_last_UV_W(l_switch,G0(:,:,:,91),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,90))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,14),wf(:,-1),G0(:,:,:,92))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,92),wf(:,-4),wf(:,0),G0tensor(:,271))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,92),wf(:,0),wf(:,-4),G0tensor(:,272))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,92),wf(:,-4),wf(:,0),G0tensor(:,273))
  call check_last_UV_W(l_switch,G0(:,:,:,92),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,91))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,14),G0(:,:,:,93))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,93),wf(:,-4),wf(:,0),G0tensor(:,274))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,93),wf(:,0),wf(:,-4),G0tensor(:,275))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,93),wf(:,-4),wf(:,0),G0tensor(:,276))
  call check_last_UV_W(l_switch,G0(:,:,:,93),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,92))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,17),G0(:,:,:,94))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,94),wf(:,-4),wf(:,0),G0tensor(:,277))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,94),wf(:,0),wf(:,-4),G0tensor(:,278))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,94),wf(:,-4),wf(:,0),G0tensor(:,279))
  call check_last_UV_W(l_switch,G0(:,:,:,94),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,93))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,17),wf(:,-1),G0(:,:,:,95))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,95),wf(:,-4),wf(:,0),G0tensor(:,280))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,95),wf(:,0),wf(:,-4),G0tensor(:,281))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,95),wf(:,-4),wf(:,0),G0tensor(:,282))
  call check_last_UV_W(l_switch,G0(:,:,:,95),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,94))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,17),G0(:,:,:,96))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,96),wf(:,-4),wf(:,0),G0tensor(:,283))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,96),wf(:,0),wf(:,-4),G0tensor(:,284))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,96),wf(:,-4),wf(:,0),G0tensor(:,285))
  call check_last_UV_W(l_switch,G0(:,:,:,96),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,95))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-1),wf(:,18),G0(:,:,:,97))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,97),wf(:,-4),wf(:,0),G0tensor(:,286))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,97),wf(:,0),wf(:,-4),G0tensor(:,287))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,97),wf(:,-4),wf(:,0),G0tensor(:,288))
  call check_last_UV_W(l_switch,G0(:,:,:,97),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,96))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,18),wf(:,-1),G0(:,:,:,98))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,98),wf(:,-4),wf(:,0),G0tensor(:,289))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,98),wf(:,0),wf(:,-4),G0tensor(:,290))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,98),wf(:,-4),wf(:,0),G0tensor(:,291))
  call check_last_UV_W(l_switch,G0(:,:,:,98),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,97))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-1),wf(:,18),G0(:,:,:,99))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,99),wf(:,-4),wf(:,0),G0tensor(:,292))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,99),wf(:,0),wf(:,-4),G0tensor(:,293))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,99),wf(:,-4),wf(:,0),G0tensor(:,294))
  call check_last_UV_W(l_switch,G0(:,:,:,99),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,98))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,26),G0(:,:,:,100))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,100),wf(:,-3),wf(:,-2),G0tensor(:,295))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,100),wf(:,-2),wf(:,-3),G0tensor(:,296))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,100),wf(:,-3),wf(:,-2),G0tensor(:,297))
  call check_last_UV_W(l_switch,G0(:,:,:,100),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,99))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,26),wf(:,0),G0(:,:,:,101))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,101),wf(:,-3),wf(:,-2),G0tensor(:,298))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,101),wf(:,-2),wf(:,-3),G0tensor(:,299))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,101),wf(:,-3),wf(:,-2),G0tensor(:,300))
  call check_last_UV_W(l_switch,G0(:,:,:,101),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,100))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,26),G0(:,:,:,102))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,102),wf(:,-3),wf(:,-2),G0tensor(:,301))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,102),wf(:,-2),wf(:,-3),G0tensor(:,302))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,102),wf(:,-3),wf(:,-2),G0tensor(:,303))
  call check_last_UV_W(l_switch,G0(:,:,:,102),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,101))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,29),G0(:,:,:,103))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,103),wf(:,-3),wf(:,-2),G0tensor(:,304))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,103),wf(:,-2),wf(:,-3),G0tensor(:,305))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,103),wf(:,-3),wf(:,-2),G0tensor(:,306))
  call check_last_UV_W(l_switch,G0(:,:,:,103),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,102))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,29),wf(:,0),G0(:,:,:,104))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,104),wf(:,-3),wf(:,-2),G0tensor(:,307))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,104),wf(:,-2),wf(:,-3),G0tensor(:,308))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,104),wf(:,-3),wf(:,-2),G0tensor(:,309))
  call check_last_UV_W(l_switch,G0(:,:,:,104),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,103))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,29),G0(:,:,:,105))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,105),wf(:,-3),wf(:,-2),G0tensor(:,310))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,105),wf(:,-2),wf(:,-3),G0tensor(:,311))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,105),wf(:,-3),wf(:,-2),G0tensor(:,312))
  call check_last_UV_W(l_switch,G0(:,:,:,105),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,104))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,30),G0(:,:,:,106))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,106),wf(:,-3),wf(:,-2),G0tensor(:,313))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,106),wf(:,-2),wf(:,-3),G0tensor(:,314))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,106),wf(:,-3),wf(:,-2),G0tensor(:,315))
  call check_last_UV_W(l_switch,G0(:,:,:,106),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,105))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,30),wf(:,0),G0(:,:,:,107))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,107),wf(:,-3),wf(:,-2),G0tensor(:,316))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,107),wf(:,-2),wf(:,-3),G0tensor(:,317))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,107),wf(:,-3),wf(:,-2),G0tensor(:,318))
  call check_last_UV_W(l_switch,G0(:,:,:,107),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,106))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,30),G0(:,:,:,108))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,108),wf(:,-3),wf(:,-2),G0tensor(:,319))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,108),wf(:,-2),wf(:,-3),G0tensor(:,320))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,108),wf(:,-3),wf(:,-2),G0tensor(:,321))
  call check_last_UV_W(l_switch,G0(:,:,:,108),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,107))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,14),G0(:,:,:,109))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,109),wf(:,-4),wf(:,-1),G0tensor(:,322))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,109),wf(:,-1),wf(:,-4),G0tensor(:,323))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,109),wf(:,-4),wf(:,-1),G0tensor(:,324))
  call check_last_UV_W(l_switch,G0(:,:,:,109),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,108))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,14),wf(:,0),G0(:,:,:,110))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,110),wf(:,-4),wf(:,-1),G0tensor(:,325))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,110),wf(:,-1),wf(:,-4),G0tensor(:,326))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,110),wf(:,-4),wf(:,-1),G0tensor(:,327))
  call check_last_UV_W(l_switch,G0(:,:,:,110),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,109))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,14),G0(:,:,:,111))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,111),wf(:,-4),wf(:,-1),G0tensor(:,328))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,111),wf(:,-1),wf(:,-4),G0tensor(:,329))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,111),wf(:,-4),wf(:,-1),G0tensor(:,330))
  call check_last_UV_W(l_switch,G0(:,:,:,111),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,110))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,17),G0(:,:,:,112))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,112),wf(:,-4),wf(:,-1),G0tensor(:,331))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,112),wf(:,-1),wf(:,-4),G0tensor(:,332))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,112),wf(:,-4),wf(:,-1),G0tensor(:,333))
  call check_last_UV_W(l_switch,G0(:,:,:,112),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,111))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,17),wf(:,0),G0(:,:,:,113))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,113),wf(:,-4),wf(:,-1),G0tensor(:,334))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,113),wf(:,-1),wf(:,-4),G0tensor(:,335))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,113),wf(:,-4),wf(:,-1),G0tensor(:,336))
  call check_last_UV_W(l_switch,G0(:,:,:,113),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,112))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,17),G0(:,:,:,114))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,114),wf(:,-4),wf(:,-1),G0tensor(:,337))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,114),wf(:,-1),wf(:,-4),G0tensor(:,338))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,114),wf(:,-4),wf(:,-1),G0tensor(:,339))
  call check_last_UV_W(l_switch,G0(:,:,:,114),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,113))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,18),G0(:,:,:,115))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,115),wf(:,-4),wf(:,-1),G0tensor(:,340))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,115),wf(:,-1),wf(:,-4),G0tensor(:,341))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,115),wf(:,-4),wf(:,-1),G0tensor(:,342))
  call check_last_UV_W(l_switch,G0(:,:,:,115),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,114))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,18),wf(:,0),G0(:,:,:,116))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,116),wf(:,-4),wf(:,-1),G0tensor(:,343))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,116),wf(:,-1),wf(:,-4),G0tensor(:,344))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,116),wf(:,-4),wf(:,-1),G0tensor(:,345))
  call check_last_UV_W(l_switch,G0(:,:,:,116),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,115))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,18),G0(:,:,:,117))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,117),wf(:,-4),wf(:,-1),G0tensor(:,346))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,117),wf(:,-1),wf(:,-4),G0tensor(:,347))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,117),wf(:,-4),wf(:,-1),G0tensor(:,348))
  call check_last_UV_W(l_switch,G0(:,:,:,117),Q(:,45),wf(:,95),Q(:,18),G1tensor(:,116))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(1)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(2)*(M(155)-M(157)-M(159) &
    +M(160)+M(195)-M(219)-M(243)+M(249))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,1)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(135)-M(136)+M(155)-M(157)-M(159)+M(160)+M(195)-M(196)-M(219)+M(220)-M(243)+M(244)+M(249) &
    -M(250))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,4)
  Gcoeff = (c(1)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(2)*(M(131)-M(133)-M(135) &
    +M(136)+M(196)-M(220)-M(244)+M(250))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,2)
  Gcoeff = (c(2)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,5)
  Gcoeff = (c(2)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,3)
  Gcoeff = (c(3)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,6)
  Gcoeff = (c(1)*(-M(73)+M(76)+M(88)-M(100)-M(110)+M(112)+M(118)-M(124)-M(125)+M(126)+M(128)-M(130))+c(2)*(-M(131)+M(132)+M(134) &
    -M(136)-M(196)+M(202)+M(226)-M(250))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,7)
  Gcoeff = (c(1)*(-M(73)+M(76)+M(88)-M(100)-M(110)+M(112)+M(118)-M(124)-M(125)+M(126)+M(128)-M(130))+c(2)*(-M(155)+M(156)+M(158) &
    -M(160)-M(195)+M(201)+M(225)-M(249))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,10)
  Gcoeff = (c(2)*(M(131)-M(132)-M(134)+M(136)-M(155)+M(156)+M(158)-M(160)-M(195)+M(196)+M(201)-M(202)+M(225)-M(226)-M(249) &
    +M(250))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,13)
  Gcoeff = (c(1)*(-M(73)+M(76)+M(88)-M(100)-M(110)+M(112)+M(118)-M(124)-M(125)+M(126)+M(128)-M(130))+c(2)*(-M(155)+M(156)+M(158) &
    -M(160)-M(195)+M(201)+M(225)-M(249))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,8)
  Gcoeff = (c(1)*(-M(73)+M(76)+M(88)-M(100)-M(110)+M(112)+M(118)-M(124)-M(125)+M(126)+M(128)-M(130))+c(2)*(-M(131)+M(132)+M(134) &
    -M(136)-M(196)+M(202)+M(226)-M(250))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,11)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(134)-M(136)+M(155)-M(156)-M(158)+M(160)+M(195)-M(196)-M(201)+M(202)-M(225)+M(226)+M(249) &
    -M(250))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,14)
  Gcoeff = (c(2)*(M(131)-M(132)-M(134)+M(136)-M(155)+M(156)+M(158)-M(160)-M(195)+M(196)+M(201)-M(202)+M(225)-M(226)-M(249) &
    +M(250))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,9)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(134)-M(136)+M(155)-M(156)-M(158)+M(160)+M(195)-M(196)-M(201)+M(202)-M(225)+M(226)+M(249) &
    -M(250))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,12)
  Gcoeff = (c(3)*(-M(131)+M(132)+M(134)-M(136)+M(155)-M(156)-M(158)+M(160)+M(195)-M(196)-M(201)+M(202)-M(225)+M(226)+M(249) &
    -M(250))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,15)
  Gcoeff = (c(1)*(-M(76)+M(85)-M(88)+M(97)-M(112)+M(116)-M(118)+M(122)-M(126)+M(127)-M(128)+M(129))+c(2)*(-M(132)+M(133)-M(134) &
    +M(135)-M(202)+M(220)-M(226)+M(244))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,16)
  Gcoeff = (c(1)*(-M(76)+M(85)-M(88)+M(97)-M(112)+M(116)-M(118)+M(122)-M(126)+M(127)-M(128)+M(129))+c(2)*(-M(156)+M(157)-M(158) &
    +M(159)-M(201)+M(219)-M(225)+M(243))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,19)
  Gcoeff = (c(2)*(M(132)-M(133)+M(134)-M(135)-M(156)+M(157)-M(158)+M(159)-M(201)+M(202)+M(219)-M(220)-M(225)+M(226)+M(243) &
    -M(244))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,22)
  Gcoeff = (c(1)*(-M(76)+M(85)-M(88)+M(97)-M(112)+M(116)-M(118)+M(122)-M(126)+M(127)-M(128)+M(129))+c(2)*(-M(156)+M(157)-M(158) &
    +M(159)-M(201)+M(219)-M(225)+M(243))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,17)
  Gcoeff = (c(1)*(-M(76)+M(85)-M(88)+M(97)-M(112)+M(116)-M(118)+M(122)-M(126)+M(127)-M(128)+M(129))+c(2)*(-M(132)+M(133)-M(134) &
    +M(135)-M(202)+M(220)-M(226)+M(244))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,20)
  Gcoeff = (c(2)*(-M(132)+M(133)-M(134)+M(135)+M(156)-M(157)+M(158)-M(159)+M(201)-M(202)-M(219)+M(220)+M(225)-M(226)-M(243) &
    +M(244))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,23)
  Gcoeff = (c(2)*(M(132)-M(133)+M(134)-M(135)-M(156)+M(157)-M(158)+M(159)-M(201)+M(202)+M(219)-M(220)-M(225)+M(226)+M(243) &
    -M(244))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,18)
  Gcoeff = (c(2)*(-M(132)+M(133)-M(134)+M(135)+M(156)-M(157)+M(158)-M(159)+M(201)-M(202)-M(219)+M(220)+M(225)-M(226)-M(243) &
    +M(244))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,21)
  Gcoeff = (c(3)*(-M(132)+M(133)-M(134)+M(135)+M(156)-M(157)+M(158)-M(159)+M(201)-M(202)-M(219)+M(220)+M(225)-M(226)-M(243) &
    +M(244))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,24)
  Gcoeff = (c(1)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(2)*(M(158)-M(185)-M(191)+M(199) &
    +M(201)+M(204)-M(234)-M(245))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,25)
  Gcoeff = (c(1)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(2)*(M(157)-M(158)-M(199)-M(201) &
    +M(203)-M(204)+M(241)+M(243))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,28)
  Gcoeff = (c(1)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(2)*(-M(157)+M(185)+M(191)-M(203) &
    +M(234)-M(241)-M(243)+M(245))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,31)
  Gcoeff = (c(1)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(2)*(M(144)+M(168)-M(181)-M(187) &
    +M(198)+M(200)-M(231)-M(237))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,26)
  Gcoeff = (c(1)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(2)*(M(143)-M(144)+M(167)-M(168) &
    -M(198)-M(200)+M(240)+M(242))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,29)
  Gcoeff = (c(1)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(2)*(-M(143)-M(167)+M(181)+M(187) &
    +M(231)+M(237)-M(240)-M(242))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,32)
  Gcoeff = (c(2)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,27)
  Gcoeff = (c(2)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,30)
  Gcoeff = (c(2)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,33)
  Gcoeff = (c(1)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(2)*(M(144)+M(168)-M(181)-M(187) &
    +M(198)+M(200)-M(231)-M(237))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,34)
  Gcoeff = (c(1)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(2)*(M(143)-M(144)+M(167)-M(168) &
    -M(198)-M(200)+M(240)+M(242))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,37)
  Gcoeff = (c(1)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(2)*(-M(143)-M(167)+M(181)+M(187) &
    +M(231)+M(237)-M(240)-M(242))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,40)
  Gcoeff = (c(1)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(2)*(M(158)-M(185)-M(191)+M(199) &
    +M(201)+M(204)-M(234)-M(245))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,35)
  Gcoeff = (c(1)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(2)*(M(157)-M(158)-M(199)-M(201) &
    +M(203)-M(204)+M(241)+M(243))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,38)
  Gcoeff = (c(1)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(2)*(-M(157)+M(185)+M(191)-M(203) &
    +M(234)-M(241)-M(243)+M(245))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,41)
  Gcoeff = (c(2)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,36)
  Gcoeff = (c(2)*(-M(143)+M(144)+M(157)-M(158)-M(167)+M(168)+M(198)-M(199)+M(200)-M(201)+M(203)-M(204)-M(240)+M(241)-M(242) &
    +M(243))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,39)
  Gcoeff = (c(2)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,42)
  Gcoeff = (c(2)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,43)
  Gcoeff = (c(2)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,46)
  Gcoeff = (c(2)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,49)
  Gcoeff = (c(2)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,44)
  Gcoeff = (c(2)*(-M(143)+M(144)+M(157)-M(158)-M(167)+M(168)+M(198)-M(199)+M(200)-M(201)+M(203)-M(204)-M(240)+M(241)-M(242) &
    +M(243))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,47)
  Gcoeff = (c(2)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,50)
  Gcoeff = (c(3)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,45)
  Gcoeff = (c(3)*(-M(143)+M(144)+M(157)-M(158)-M(167)+M(168)+M(198)-M(199)+M(200)-M(201)+M(203)-M(204)-M(240)+M(241)-M(242) &
    +M(243))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,48)
  Gcoeff = (c(3)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(9)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,51)
  Gcoeff = (c(1)*(M(49)-M(79)-M(91)+M(98)+M(106)+M(112)-M(114)-M(116)+M(117)+M(118)-M(120)-M(122))+c(2)*(M(158)-M(168)-M(174) &
    +M(177)+M(182)-M(192)-M(198)+M(201))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,52)
  Gcoeff = (c(1)*(M(49)-M(79)-M(91)+M(98)+M(106)+M(112)-M(114)-M(116)+M(117)+M(118)-M(120)-M(122))+c(2)*(M(147)+M(185)-M(187) &
    -M(189)+M(190)-M(207)-M(231)+M(245))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,53)
  Gcoeff = (c(2)*(M(147)-M(158)+M(168)+M(174)-M(177)-M(182)+M(185)-M(187)-M(189)+M(190)+M(192)+M(198)-M(201)-M(207)-M(231) &
    +M(245))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,54)
  Gcoeff = (c(1)*(-M(49)+M(52)+M(86)-M(98)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124))+c(2)*(-M(158)+M(160)+M(171) &
    -M(177)-M(182)+M(184)+M(195)-M(201))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,55)
  Gcoeff = (c(1)*(-M(49)+M(52)+M(86)-M(98)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124))+c(2)*(-M(147)+M(153)-M(185) &
    +M(186)+M(188)-M(190)+M(221)-M(245))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,56)
  Gcoeff = (c(2)*(-M(147)+M(153)+M(158)-M(160)-M(171)+M(177)+M(182)-M(184)-M(185)+M(186)+M(188)-M(190)-M(195)+M(201)+M(221) &
    -M(245))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,57)
  Gcoeff = (c(1)*(-M(52)+M(79)-M(86)+M(91)-M(104)-M(110)+M(114)+M(116)+M(120)+M(122)-M(123)-M(124))+c(2)*(-M(160)+M(168)-M(171) &
    +M(174)-M(184)+M(192)-M(195)+M(198))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,58)
  Gcoeff = (c(1)*(-M(52)+M(79)-M(86)+M(91)-M(104)-M(110)+M(114)+M(116)+M(120)+M(122)-M(123)-M(124))+c(2)*(-M(153)-M(186)+M(187) &
    -M(188)+M(189)+M(207)-M(221)+M(231))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,59)
  Gcoeff = (c(2)*(-M(153)+M(160)-M(168)+M(171)-M(174)+M(184)-M(186)+M(187)-M(188)+M(189)-M(192)+M(195)-M(198)+M(207)-M(221) &
    +M(231))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,60)
  Gcoeff = (c(1)*(M(49)-M(79)-M(91)+M(98)+M(106)+M(112)-M(114)-M(116)+M(117)+M(118)-M(120)-M(122))+c(2)*(M(147)+M(185)-M(187) &
    -M(189)+M(190)-M(207)-M(231)+M(245))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,61)
  Gcoeff = (c(1)*(M(49)-M(79)-M(91)+M(98)+M(106)+M(112)-M(114)-M(116)+M(117)+M(118)-M(120)-M(122))+c(2)*(M(158)-M(168)-M(174) &
    +M(177)+M(182)-M(192)-M(198)+M(201))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,62)
  Gcoeff = (c(2)*(-M(147)+M(158)-M(168)-M(174)+M(177)+M(182)-M(185)+M(187)+M(189)-M(190)-M(192)-M(198)+M(201)+M(207)+M(231) &
    -M(245))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,63)
  Gcoeff = (c(1)*(-M(49)+M(52)+M(86)-M(98)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124))+c(2)*(-M(147)+M(153)-M(185) &
    +M(186)+M(188)-M(190)+M(221)-M(245))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,64)
  Gcoeff = (c(1)*(-M(49)+M(52)+M(86)-M(98)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124))+c(2)*(-M(158)+M(160)+M(171) &
    -M(177)-M(182)+M(184)+M(195)-M(201))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,65)
  Gcoeff = (c(2)*(M(147)-M(153)-M(158)+M(160)+M(171)-M(177)-M(182)+M(184)+M(185)-M(186)-M(188)+M(190)+M(195)-M(201)-M(221) &
    +M(245))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,66)
  Gcoeff = (c(1)*(-M(52)+M(79)-M(86)+M(91)-M(104)-M(110)+M(114)+M(116)+M(120)+M(122)-M(123)-M(124))+c(2)*(-M(153)-M(186)+M(187) &
    -M(188)+M(189)+M(207)-M(221)+M(231))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,67)
  Gcoeff = (c(1)*(-M(52)+M(79)-M(86)+M(91)-M(104)-M(110)+M(114)+M(116)+M(120)+M(122)-M(123)-M(124))+c(2)*(-M(160)+M(168)-M(171) &
    +M(174)-M(184)+M(192)-M(195)+M(198))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,68)
  Gcoeff = (c(2)*(M(153)-M(160)+M(168)-M(171)+M(174)-M(184)+M(186)-M(187)+M(188)-M(189)+M(192)-M(195)+M(198)-M(207)+M(221) &
    -M(231))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,69)
  Gcoeff = (c(2)*(M(147)-M(158)+M(168)+M(174)-M(177)-M(182)+M(185)-M(187)-M(189)+M(190)+M(192)+M(198)-M(201)-M(207)-M(231) &
    +M(245))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,70)
  Gcoeff = (c(2)*(-M(147)+M(158)-M(168)-M(174)+M(177)+M(182)-M(185)+M(187)+M(189)-M(190)-M(192)-M(198)+M(201)+M(207)+M(231) &
    -M(245))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,71)
  Gcoeff = (c(3)*(-M(147)+M(158)-M(168)-M(174)+M(177)+M(182)-M(185)+M(187)+M(189)-M(190)-M(192)-M(198)+M(201)+M(207)+M(231) &
    -M(245))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,72)
  Gcoeff = (c(2)*(-M(147)+M(153)+M(158)-M(160)-M(171)+M(177)+M(182)-M(184)-M(185)+M(186)+M(188)-M(190)-M(195)+M(201)+M(221) &
    -M(245))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,73)
  Gcoeff = (c(2)*(M(147)-M(153)-M(158)+M(160)+M(171)-M(177)-M(182)+M(184)+M(185)-M(186)-M(188)+M(190)+M(195)-M(201)-M(221) &
    +M(245))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,74)
  Gcoeff = (c(3)*(M(147)-M(153)-M(158)+M(160)+M(171)-M(177)-M(182)+M(184)+M(185)-M(186)-M(188)+M(190)+M(195)-M(201)-M(221) &
    +M(245))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,75)
  Gcoeff = (c(2)*(-M(153)+M(160)-M(168)+M(171)-M(174)+M(184)-M(186)+M(187)-M(188)+M(189)-M(192)+M(195)-M(198)+M(207)-M(221) &
    +M(231))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,76)
  Gcoeff = (c(2)*(M(153)-M(160)+M(168)-M(171)+M(174)-M(184)+M(186)-M(187)+M(188)-M(189)+M(192)-M(195)+M(198)-M(207)+M(221) &
    -M(231))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,77)
  Gcoeff = (c(3)*(M(153)-M(160)+M(168)-M(171)+M(174)-M(184)+M(186)-M(187)+M(188)-M(189)+M(192)-M(195)+M(198)-M(207)+M(221) &
    -M(231))) * den(62)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,78)
  Gcoeff = (c(1)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(2)*(M(134)-M(179)-M(193)+M(197) &
    +M(202)+M(210)-M(228)-M(247))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,79)
  Gcoeff = (c(1)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(2)*(M(133)-M(134)-M(197)-M(202) &
    +M(209)-M(210)+M(239)+M(244))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,82)
  Gcoeff = (c(1)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(2)*(-M(133)+M(179)+M(193)-M(209) &
    +M(228)-M(239)-M(244)+M(247))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,85)
  Gcoeff = (c(1)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(2)*(M(144)+M(168)-M(181)-M(187) &
    +M(198)+M(200)-M(231)-M(237))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,88)
  Gcoeff = (c(1)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(2)*(M(143)-M(144)+M(167)-M(168) &
    -M(198)-M(200)+M(240)+M(242))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,91)
  Gcoeff = (c(1)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(2)*(-M(143)-M(167)+M(181)+M(187) &
    +M(231)+M(237)-M(240)-M(242))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,94)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,97)
  Gcoeff = (c(2)*(-M(133)+M(134)+M(143)-M(144)+M(167)-M(168)+M(197)-M(198)-M(200)+M(202)-M(209)+M(210)-M(239)+M(240)+M(242) &
    -M(244))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,100)
  Gcoeff = (c(2)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,103)
  Gcoeff = (c(1)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(2)*(M(144)+M(168)-M(181)-M(187) &
    +M(198)+M(200)-M(231)-M(237))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,80)
  Gcoeff = (c(1)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(2)*(M(143)-M(144)+M(167)-M(168) &
    -M(198)-M(200)+M(240)+M(242))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,83)
  Gcoeff = (c(1)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(2)*(-M(143)-M(167)+M(181)+M(187) &
    +M(231)+M(237)-M(240)-M(242))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,86)
  Gcoeff = (c(1)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(2)*(M(134)-M(179)-M(193)+M(197) &
    +M(202)+M(210)-M(228)-M(247))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,89)
  Gcoeff = (c(1)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(2)*(M(133)-M(134)-M(197)-M(202) &
    +M(209)-M(210)+M(239)+M(244))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,92)
  Gcoeff = (c(1)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(2)*(-M(133)+M(179)+M(193)-M(209) &
    +M(228)-M(239)-M(244)+M(247))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,95)
  Gcoeff = (c(2)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,98)
  Gcoeff = (c(2)*(M(133)-M(134)-M(143)+M(144)-M(167)+M(168)-M(197)+M(198)+M(200)-M(202)+M(209)-M(210)+M(239)-M(240)-M(242) &
    +M(244))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,101)
  Gcoeff = (c(2)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,104)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,81)
  Gcoeff = (c(2)*(-M(133)+M(134)+M(143)-M(144)+M(167)-M(168)+M(197)-M(198)-M(200)+M(202)-M(209)+M(210)-M(239)+M(240)+M(242) &
    -M(244))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,84)
  Gcoeff = (c(2)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,87)
  Gcoeff = (c(2)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,90)
  Gcoeff = (c(2)*(M(133)-M(134)-M(143)+M(144)-M(167)+M(168)-M(197)+M(198)+M(200)-M(202)+M(209)-M(210)+M(239)-M(240)-M(242) &
    +M(244))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,93)
  Gcoeff = (c(2)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,96)
  Gcoeff = (c(3)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,99)
  Gcoeff = (c(3)*(M(133)-M(134)-M(143)+M(144)-M(167)+M(168)-M(197)+M(198)+M(200)-M(202)+M(209)-M(210)+M(239)-M(240)-M(242) &
    +M(244))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,102)
  Gcoeff = (c(3)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(9)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,105)
  Gcoeff = (c(1)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(2)*(M(134)-M(144)-M(150) &
    +M(153)+M(188)-M(194)-M(200)+M(202))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,106)
  Gcoeff = (c(1)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(2)*(M(171)+M(179)-M(181) &
    -M(183)+M(184)-M(213)-M(237)+M(247))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,107)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,108)
  Gcoeff = (c(1)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(2)*(M(171)+M(179)-M(181) &
    -M(183)+M(184)-M(213)-M(237)+M(247))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,109)
  Gcoeff = (c(1)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(2)*(M(134)-M(144)-M(150) &
    +M(153)+M(188)-M(194)-M(200)+M(202))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,110)
  Gcoeff = (c(2)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,111)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,112)
  Gcoeff = (c(2)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,113)
  Gcoeff = (c(3)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,114)
  Gcoeff = (c(1)*(-M(61)+M(64)+M(87)-M(99)-M(104)+M(106)+M(117)-M(123)+M(125)-M(126)-M(128)+M(130))+c(2)*(-M(134)+M(136)+M(147) &
    -M(153)-M(188)+M(190)+M(196)-M(202))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,115)
  Gcoeff = (c(1)*(-M(61)+M(64)+M(87)-M(99)-M(104)+M(106)+M(117)-M(123)+M(125)-M(126)-M(128)+M(130))+c(2)*(-M(171)+M(177)-M(179) &
    +M(180)+M(182)-M(184)+M(223)-M(247))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,116)
  Gcoeff = (c(2)*(M(134)-M(136)-M(147)+M(153)-M(171)+M(177)-M(179)+M(180)+M(182)-M(184)+M(188)-M(190)-M(196)+M(202)+M(223) &
    -M(247))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,117)
  Gcoeff = (c(1)*(-M(61)+M(64)+M(87)-M(99)-M(104)+M(106)+M(117)-M(123)+M(125)-M(126)-M(128)+M(130))+c(2)*(-M(171)+M(177)-M(179) &
    +M(180)+M(182)-M(184)+M(223)-M(247))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,118)
  Gcoeff = (c(1)*(-M(61)+M(64)+M(87)-M(99)-M(104)+M(106)+M(117)-M(123)+M(125)-M(126)-M(128)+M(130))+c(2)*(-M(134)+M(136)+M(147) &
    -M(153)-M(188)+M(190)+M(196)-M(202))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,119)
  Gcoeff = (c(2)*(-M(134)+M(136)+M(147)-M(153)+M(171)-M(177)+M(179)-M(180)-M(182)+M(184)-M(188)+M(190)+M(196)-M(202)-M(223) &
    +M(247))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,120)
  Gcoeff = (c(2)*(M(134)-M(136)-M(147)+M(153)-M(171)+M(177)-M(179)+M(180)+M(182)-M(184)+M(188)-M(190)-M(196)+M(202)+M(223) &
    -M(247))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,121)
  Gcoeff = (c(2)*(-M(134)+M(136)+M(147)-M(153)+M(171)-M(177)+M(179)-M(180)-M(182)+M(184)-M(188)+M(190)+M(196)-M(202)-M(223) &
    +M(247))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,122)
  Gcoeff = (c(3)*(-M(134)+M(136)+M(147)-M(153)+M(171)-M(177)+M(179)-M(180)-M(182)+M(184)-M(188)+M(190)+M(196)-M(202)-M(223) &
    +M(247))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,123)
  Gcoeff = (c(1)*(-M(64)+M(82)-M(87)+M(94)-M(106)+M(114)-M(117)+M(120)-M(125)+M(127)+M(129)-M(130))+c(2)*(-M(136)+M(144)-M(147) &
    +M(150)-M(190)+M(194)-M(196)+M(200))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,124)
  Gcoeff = (c(1)*(-M(64)+M(82)-M(87)+M(94)-M(106)+M(114)-M(117)+M(120)-M(125)+M(127)+M(129)-M(130))+c(2)*(-M(177)-M(180)+M(181) &
    -M(182)+M(183)+M(213)-M(223)+M(237))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,125)
  Gcoeff = (c(2)*(M(136)-M(144)+M(147)-M(150)-M(177)-M(180)+M(181)-M(182)+M(183)+M(190)-M(194)+M(196)-M(200)+M(213)-M(223) &
    +M(237))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,126)
  Gcoeff = (c(1)*(-M(64)+M(82)-M(87)+M(94)-M(106)+M(114)-M(117)+M(120)-M(125)+M(127)+M(129)-M(130))+c(2)*(-M(177)-M(180)+M(181) &
    -M(182)+M(183)+M(213)-M(223)+M(237))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,127)
  Gcoeff = (c(1)*(-M(64)+M(82)-M(87)+M(94)-M(106)+M(114)-M(117)+M(120)-M(125)+M(127)+M(129)-M(130))+c(2)*(-M(136)+M(144)-M(147) &
    +M(150)-M(190)+M(194)-M(196)+M(200))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,128)
  Gcoeff = (c(2)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,129)
  Gcoeff = (c(2)*(M(136)-M(144)+M(147)-M(150)-M(177)-M(180)+M(181)-M(182)+M(183)+M(190)-M(194)+M(196)-M(200)+M(213)-M(223) &
    +M(237))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,130)
  Gcoeff = (c(2)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,131)
  Gcoeff = (c(3)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(24)
  T2sum(1:1,13) = T2sum(1:1,13) + Gcoeff * G0tensor(:,132)
  Gcoeff = (c(1)*(M(52)+M(64)+M(76)-M(79)-M(82)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94)-M(97))+c(2)*(M(156)+M(180)-M(209)-M(215) &
    +M(223)+M(225)-M(233)-M(239))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,133)
  Gcoeff = (c(1)*(M(49)-M(52)+M(61)-M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100))+c(2)*(M(155)-M(156)+M(179)-M(180) &
    -M(223)-M(225)+M(247)+M(249))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,136)
  Gcoeff = (c(1)*(-M(49)-M(61)-M(73)+M(79)+M(82)+M(85)+M(91)+M(94)+M(97)-M(98)-M(99)-M(100))+c(2)*(-M(155)-M(179)+M(209)+M(215) &
    +M(233)+M(239)-M(247)-M(249))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,139)
  Gcoeff = (c(1)*(M(52)+M(64)+M(76)-M(79)-M(82)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94)-M(97))+c(2)*(M(138)+M(162)-M(205)-M(211) &
    +M(222)+M(224)-M(229)-M(235))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,134)
  Gcoeff = (c(1)*(M(49)-M(52)+M(61)-M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100))+c(2)*(M(137)-M(138)+M(161)-M(162) &
    -M(222)-M(224)+M(246)+M(248))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,137)
  Gcoeff = (c(1)*(-M(49)-M(61)-M(73)+M(79)+M(82)+M(85)+M(91)+M(94)+M(97)-M(98)-M(99)-M(100))+c(2)*(-M(137)-M(161)+M(205)+M(211) &
    +M(229)+M(235)-M(246)-M(248))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,140)
  Gcoeff = (c(2)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,135)
  Gcoeff = (c(2)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,138)
  Gcoeff = (c(2)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,141)
  Gcoeff = (c(1)*(M(52)+M(64)+M(76)-M(79)-M(82)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94)-M(97))+c(2)*(M(138)+M(162)-M(205)-M(211) &
    +M(222)+M(224)-M(229)-M(235))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,142)
  Gcoeff = (c(1)*(M(49)-M(52)+M(61)-M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100))+c(2)*(M(137)-M(138)+M(161)-M(162) &
    -M(222)-M(224)+M(246)+M(248))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,145)
  Gcoeff = (c(1)*(-M(49)-M(61)-M(73)+M(79)+M(82)+M(85)+M(91)+M(94)+M(97)-M(98)-M(99)-M(100))+c(2)*(-M(137)-M(161)+M(205)+M(211) &
    +M(229)+M(235)-M(246)-M(248))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,148)
  Gcoeff = (c(1)*(M(52)+M(64)+M(76)-M(79)-M(82)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94)-M(97))+c(2)*(M(156)+M(180)-M(209)-M(215) &
    +M(223)+M(225)-M(233)-M(239))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,143)
  Gcoeff = (c(1)*(M(49)-M(52)+M(61)-M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100))+c(2)*(M(155)-M(156)+M(179)-M(180) &
    -M(223)-M(225)+M(247)+M(249))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,146)
  Gcoeff = (c(1)*(-M(49)-M(61)-M(73)+M(79)+M(82)+M(85)+M(91)+M(94)+M(97)-M(98)-M(99)-M(100))+c(2)*(-M(155)-M(179)+M(209)+M(215) &
    +M(233)+M(239)-M(247)-M(249))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,149)
  Gcoeff = (c(2)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,144)
  Gcoeff = (c(2)*(-M(137)+M(138)+M(155)-M(156)-M(161)+M(162)+M(179)-M(180)+M(222)-M(223)+M(224)-M(225)-M(246)+M(247)-M(248) &
    +M(249))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,147)
  Gcoeff = (c(2)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,150)
  Gcoeff = (c(2)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,151)
  Gcoeff = (c(2)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,154)
  Gcoeff = (c(2)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,157)
  Gcoeff = (c(2)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,152)
  Gcoeff = (c(2)*(-M(137)+M(138)+M(155)-M(156)-M(161)+M(162)+M(179)-M(180)+M(222)-M(223)+M(224)-M(225)-M(246)+M(247)-M(248) &
    +M(249))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,155)
  Gcoeff = (c(2)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,158)
  Gcoeff = (c(3)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,153)
  Gcoeff = (c(3)*(-M(137)+M(138)+M(155)-M(156)-M(161)+M(162)+M(179)-M(180)+M(222)-M(223)+M(224)-M(225)-M(246)+M(247)-M(248) &
    +M(249))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,156)
  Gcoeff = (c(3)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(10)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,159)
  Gcoeff = (c(1)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(2)*(M(156)-M(162)-M(173) &
    +M(175)+M(206)-M(216)-M(222)+M(225))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,160)
  Gcoeff = (c(1)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(2)*(M(141)-M(183)+M(209) &
    -M(211)-M(213)+M(214)-M(229)+M(239))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,161)
  Gcoeff = (c(2)*(M(141)-M(156)+M(162)+M(173)-M(175)-M(183)-M(206)+M(209)-M(211)-M(213)+M(214)+M(216)+M(222)-M(225)-M(229) &
    +M(239))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,162)
  Gcoeff = (c(1)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(2)*(-M(156)+M(159)+M(165) &
    -M(175)-M(206)+M(208)+M(219)-M(225))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,163)
  Gcoeff = (c(1)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(2)*(-M(141)+M(151)+M(197) &
    -M(209)+M(210)+M(212)-M(214)-M(239))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,164)
  Gcoeff = (c(2)*(-M(141)+M(151)+M(156)-M(159)-M(165)+M(175)+M(197)+M(206)-M(208)-M(209)+M(210)+M(212)-M(214)-M(219)+M(225) &
    -M(239))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,165)
  Gcoeff = (c(1)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(2)*(-M(159)+M(162)-M(165) &
    +M(173)-M(208)+M(216)-M(219)+M(222))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,166)
  Gcoeff = (c(1)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(2)*(-M(151)+M(183)-M(197) &
    -M(210)+M(211)-M(212)+M(213)+M(229))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,167)
  Gcoeff = (c(2)*(-M(151)+M(159)-M(162)+M(165)-M(173)+M(183)-M(197)+M(208)-M(210)+M(211)-M(212)+M(213)-M(216)+M(219)-M(222) &
    +M(229))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,168)
  Gcoeff = (c(1)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(2)*(M(141)-M(183)+M(209) &
    -M(211)-M(213)+M(214)-M(229)+M(239))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,169)
  Gcoeff = (c(1)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(2)*(M(156)-M(162)-M(173) &
    +M(175)+M(206)-M(216)-M(222)+M(225))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,170)
  Gcoeff = (c(2)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,171)
  Gcoeff = (c(1)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(2)*(-M(141)+M(151)+M(197) &
    -M(209)+M(210)+M(212)-M(214)-M(239))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,172)
  Gcoeff = (c(1)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(2)*(-M(156)+M(159)+M(165) &
    -M(175)-M(206)+M(208)+M(219)-M(225))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,173)
  Gcoeff = (c(2)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,174)
  Gcoeff = (c(1)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(2)*(-M(151)+M(183)-M(197) &
    -M(210)+M(211)-M(212)+M(213)+M(229))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,175)
  Gcoeff = (c(1)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(2)*(-M(159)+M(162)-M(165) &
    +M(173)-M(208)+M(216)-M(219)+M(222))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,176)
  Gcoeff = (c(2)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,177)
  Gcoeff = (c(2)*(M(141)-M(156)+M(162)+M(173)-M(175)-M(183)-M(206)+M(209)-M(211)-M(213)+M(214)+M(216)+M(222)-M(225)-M(229) &
    +M(239))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,178)
  Gcoeff = (c(2)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,179)
  Gcoeff = (c(3)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,180)
  Gcoeff = (c(2)*(-M(141)+M(151)+M(156)-M(159)-M(165)+M(175)+M(197)+M(206)-M(208)-M(209)+M(210)+M(212)-M(214)-M(219)+M(225) &
    -M(239))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,181)
  Gcoeff = (c(2)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,182)
  Gcoeff = (c(3)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,183)
  Gcoeff = (c(2)*(-M(151)+M(159)-M(162)+M(165)-M(173)+M(183)-M(197)+M(208)-M(210)+M(211)-M(212)+M(213)-M(216)+M(219)-M(222) &
    +M(229))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,184)
  Gcoeff = (c(2)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,185)
  Gcoeff = (c(3)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(65)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,186)
  Gcoeff = (c(1)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(2)*(M(142)+M(167)-M(169) &
    -M(171)+M(172)-M(184)-M(230)+M(240))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,187)
  Gcoeff = (c(1)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(2)*(M(141)-M(183)+M(209) &
    -M(211)-M(213)+M(214)-M(229)+M(239))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,190)
  Gcoeff = (c(2)*(M(141)-M(142)-M(167)+M(169)+M(171)-M(172)-M(183)+M(184)+M(209)-M(211)-M(213)+M(214)-M(229)+M(230)+M(239) &
    -M(240))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,193)
  Gcoeff = (c(1)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(2)*(-M(142)+M(152)-M(167) &
    +M(168)+M(170)-M(172)+M(198)-M(240))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,196)
  Gcoeff = (c(1)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(2)*(-M(141)+M(151)+M(197) &
    -M(209)+M(210)+M(212)-M(214)-M(239))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,199)
  Gcoeff = (c(2)*(-M(141)+M(142)+M(151)-M(152)+M(167)-M(168)-M(170)+M(172)+M(197)-M(198)-M(209)+M(210)+M(212)-M(214)-M(239) &
    +M(240))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,202)
  Gcoeff = (c(1)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(2)*(-M(152)-M(168)+M(169) &
    -M(170)+M(171)+M(184)-M(198)+M(230))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,205)
  Gcoeff = (c(1)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(2)*(-M(151)+M(183)-M(197) &
    -M(210)+M(211)-M(212)+M(213)+M(229))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,208)
  Gcoeff = (c(2)*(-M(151)+M(152)+M(168)-M(169)+M(170)-M(171)+M(183)-M(184)-M(197)+M(198)-M(210)+M(211)-M(212)+M(213)+M(229) &
    -M(230))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,211)
  Gcoeff = (c(1)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(2)*(M(141)-M(183)+M(209) &
    -M(211)-M(213)+M(214)-M(229)+M(239))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,188)
  Gcoeff = (c(1)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(2)*(M(142)+M(167)-M(169) &
    -M(171)+M(172)-M(184)-M(230)+M(240))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,191)
  Gcoeff = (c(2)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,194)
  Gcoeff = (c(1)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(2)*(-M(141)+M(151)+M(197) &
    -M(209)+M(210)+M(212)-M(214)-M(239))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,197)
  Gcoeff = (c(1)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(2)*(-M(142)+M(152)-M(167) &
    +M(168)+M(170)-M(172)+M(198)-M(240))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,200)
  Gcoeff = (c(2)*(M(141)-M(142)-M(151)+M(152)-M(167)+M(168)+M(170)-M(172)-M(197)+M(198)+M(209)-M(210)-M(212)+M(214)+M(239) &
    -M(240))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,203)
  Gcoeff = (c(1)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(2)*(-M(151)+M(183)-M(197) &
    -M(210)+M(211)-M(212)+M(213)+M(229))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,206)
  Gcoeff = (c(1)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(2)*(-M(152)-M(168)+M(169) &
    -M(170)+M(171)+M(184)-M(198)+M(230))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,209)
  Gcoeff = (c(2)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,212)
  Gcoeff = (c(2)*(M(141)-M(142)-M(167)+M(169)+M(171)-M(172)-M(183)+M(184)+M(209)-M(211)-M(213)+M(214)-M(229)+M(230)+M(239) &
    -M(240))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,189)
  Gcoeff = (c(2)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,192)
  Gcoeff = (c(3)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,195)
  Gcoeff = (c(2)*(-M(141)+M(142)+M(151)-M(152)+M(167)-M(168)-M(170)+M(172)+M(197)-M(198)-M(209)+M(210)+M(212)-M(214)-M(239) &
    +M(240))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,198)
  Gcoeff = (c(2)*(M(141)-M(142)-M(151)+M(152)-M(167)+M(168)+M(170)-M(172)-M(197)+M(198)+M(209)-M(210)-M(212)+M(214)+M(239) &
    -M(240))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,201)
  Gcoeff = (c(3)*(M(141)-M(142)-M(151)+M(152)-M(167)+M(168)+M(170)-M(172)-M(197)+M(198)+M(209)-M(210)-M(212)+M(214)+M(239) &
    -M(240))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,204)
  Gcoeff = (c(2)*(-M(151)+M(152)+M(168)-M(169)+M(170)-M(171)+M(183)-M(184)-M(197)+M(198)-M(210)+M(211)-M(212)+M(213)+M(229) &
    -M(230))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,207)
  Gcoeff = (c(2)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,210)
  Gcoeff = (c(3)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(65)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,213)
  Gcoeff = (c(1)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(2)*(M(137)-M(139)-M(141) &
    +M(142)+M(172)-M(214)-M(238)+M(248))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,214)
  Gcoeff = (c(1)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(2)*(M(171)+M(179)-M(181) &
    -M(183)+M(184)-M(213)-M(237)+M(247))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,217)
  Gcoeff = (c(2)*(-M(137)+M(139)+M(141)-M(142)+M(171)-M(172)+M(179)-M(181)-M(183)+M(184)-M(213)+M(214)-M(237)+M(238)+M(247) &
    -M(248))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,220)
  Gcoeff = (c(1)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(2)*(M(171)+M(179)-M(181) &
    -M(183)+M(184)-M(213)-M(237)+M(247))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,215)
  Gcoeff = (c(1)*(M(61)-M(82)-M(94)+M(99)+M(104)-M(114)-M(120)+M(123)+M(126)-M(127)+M(128)-M(129))+c(2)*(M(137)-M(139)-M(141) &
    +M(142)+M(172)-M(214)-M(238)+M(248))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,218)
  Gcoeff = (c(2)*(M(137)-M(139)-M(141)+M(142)-M(171)+M(172)-M(179)+M(181)+M(183)-M(184)+M(213)-M(214)+M(237)-M(238)-M(247) &
    +M(248))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,221)
  Gcoeff = (c(2)*(-M(137)+M(139)+M(141)-M(142)+M(171)-M(172)+M(179)-M(181)-M(183)+M(184)-M(213)+M(214)-M(237)+M(238)+M(247) &
    -M(248))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,216)
  Gcoeff = (c(2)*(M(137)-M(139)-M(141)+M(142)-M(171)+M(172)-M(179)+M(181)+M(183)-M(184)+M(213)-M(214)+M(237)-M(238)-M(247) &
    +M(248))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,219)
  Gcoeff = (c(3)*(M(137)-M(139)-M(141)+M(142)-M(171)+M(172)-M(179)+M(181)+M(183)-M(184)+M(213)-M(214)+M(237)-M(238)-M(247) &
    +M(248))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,222)
  Gcoeff = (c(1)*(-M(61)+M(64)+M(87)-M(99)-M(104)+M(106)+M(117)-M(123)+M(125)-M(126)-M(128)+M(130))+c(2)*(-M(137)+M(138)+M(140) &
    -M(142)-M(172)+M(178)+M(224)-M(248))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,223)
  Gcoeff = (c(1)*(-M(61)+M(64)+M(87)-M(99)-M(104)+M(106)+M(117)-M(123)+M(125)-M(126)-M(128)+M(130))+c(2)*(-M(171)+M(177)-M(179) &
    +M(180)+M(182)-M(184)+M(223)-M(247))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,226)
  Gcoeff = (c(2)*(M(137)-M(138)-M(140)+M(142)-M(171)+M(172)+M(177)-M(178)-M(179)+M(180)+M(182)-M(184)+M(223)-M(224)-M(247) &
    +M(248))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,229)
  Gcoeff = (c(1)*(-M(61)+M(64)+M(87)-M(99)-M(104)+M(106)+M(117)-M(123)+M(125)-M(126)-M(128)+M(130))+c(2)*(-M(171)+M(177)-M(179) &
    +M(180)+M(182)-M(184)+M(223)-M(247))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,224)
  Gcoeff = (c(1)*(-M(61)+M(64)+M(87)-M(99)-M(104)+M(106)+M(117)-M(123)+M(125)-M(126)-M(128)+M(130))+c(2)*(-M(137)+M(138)+M(140) &
    -M(142)-M(172)+M(178)+M(224)-M(248))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,227)
  Gcoeff = (c(2)*(-M(137)+M(138)+M(140)-M(142)+M(171)-M(172)-M(177)+M(178)+M(179)-M(180)-M(182)+M(184)-M(223)+M(224)+M(247) &
    -M(248))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,230)
  Gcoeff = (c(2)*(M(137)-M(138)-M(140)+M(142)-M(171)+M(172)+M(177)-M(178)-M(179)+M(180)+M(182)-M(184)+M(223)-M(224)-M(247) &
    +M(248))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,225)
  Gcoeff = (c(2)*(-M(137)+M(138)+M(140)-M(142)+M(171)-M(172)-M(177)+M(178)+M(179)-M(180)-M(182)+M(184)-M(223)+M(224)+M(247) &
    -M(248))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,228)
  Gcoeff = (c(3)*(-M(137)+M(138)+M(140)-M(142)+M(171)-M(172)-M(177)+M(178)+M(179)-M(180)-M(182)+M(184)-M(223)+M(224)+M(247) &
    -M(248))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,231)
  Gcoeff = (c(1)*(-M(64)+M(82)-M(87)+M(94)-M(106)+M(114)-M(117)+M(120)-M(125)+M(127)+M(129)-M(130))+c(2)*(-M(138)+M(139)-M(140) &
    +M(141)-M(178)+M(214)-M(224)+M(238))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,232)
  Gcoeff = (c(1)*(-M(64)+M(82)-M(87)+M(94)-M(106)+M(114)-M(117)+M(120)-M(125)+M(127)+M(129)-M(130))+c(2)*(-M(177)-M(180)+M(181) &
    -M(182)+M(183)+M(213)-M(223)+M(237))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,235)
  Gcoeff = (c(2)*(M(138)-M(139)+M(140)-M(141)-M(177)+M(178)-M(180)+M(181)-M(182)+M(183)+M(213)-M(214)-M(223)+M(224)+M(237) &
    -M(238))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,238)
  Gcoeff = (c(1)*(-M(64)+M(82)-M(87)+M(94)-M(106)+M(114)-M(117)+M(120)-M(125)+M(127)+M(129)-M(130))+c(2)*(-M(177)-M(180)+M(181) &
    -M(182)+M(183)+M(213)-M(223)+M(237))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,233)
  Gcoeff = (c(1)*(-M(64)+M(82)-M(87)+M(94)-M(106)+M(114)-M(117)+M(120)-M(125)+M(127)+M(129)-M(130))+c(2)*(-M(138)+M(139)-M(140) &
    +M(141)-M(178)+M(214)-M(224)+M(238))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,236)
  Gcoeff = (c(2)*(-M(138)+M(139)-M(140)+M(141)+M(177)-M(178)+M(180)-M(181)+M(182)-M(183)-M(213)+M(214)+M(223)-M(224)-M(237) &
    +M(238))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,239)
  Gcoeff = (c(2)*(M(138)-M(139)+M(140)-M(141)-M(177)+M(178)-M(180)+M(181)-M(182)+M(183)+M(213)-M(214)-M(223)+M(224)+M(237) &
    -M(238))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,234)
  Gcoeff = (c(2)*(-M(138)+M(139)-M(140)+M(141)+M(177)-M(178)+M(180)-M(181)+M(182)-M(183)-M(213)+M(214)+M(223)-M(224)-M(237) &
    +M(238))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,237)
  Gcoeff = (c(3)*(-M(138)+M(139)-M(140)+M(141)+M(177)-M(178)+M(180)-M(181)+M(182)-M(183)-M(213)+M(214)+M(223)-M(224)-M(237) &
    +M(238))) * den(24)
  T2sum(1:1,10) = T2sum(1:1,10) + Gcoeff * G0tensor(:,240)
  Gcoeff = (c(1)*(M(52)+M(64)+M(76)-M(79)-M(82)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94)-M(97))+c(2)*(M(132)+M(186)-M(203)-M(217) &
    +M(221)+M(226)-M(227)-M(241))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,241)
  Gcoeff = (c(1)*(M(49)-M(52)+M(61)-M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100))+c(2)*(M(131)-M(132)+M(185)-M(186) &
    -M(221)-M(226)+M(245)+M(250))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,244)
  Gcoeff = (c(1)*(-M(49)-M(61)-M(73)+M(79)+M(82)+M(85)+M(91)+M(94)+M(97)-M(98)-M(99)-M(100))+c(2)*(-M(131)-M(185)+M(203)+M(217) &
    +M(227)+M(241)-M(245)-M(250))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,247)
  Gcoeff = (c(1)*(M(52)+M(64)+M(76)-M(79)-M(82)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94)-M(97))+c(2)*(M(138)+M(162)-M(205)-M(211) &
    +M(222)+M(224)-M(229)-M(235))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,250)
  Gcoeff = (c(1)*(M(49)-M(52)+M(61)-M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100))+c(2)*(M(137)-M(138)+M(161)-M(162) &
    -M(222)-M(224)+M(246)+M(248))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,253)
  Gcoeff = (c(1)*(-M(49)-M(61)-M(73)+M(79)+M(82)+M(85)+M(91)+M(94)+M(97)-M(98)-M(99)-M(100))+c(2)*(-M(137)-M(161)+M(205)+M(211) &
    +M(229)+M(235)-M(246)-M(248))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,256)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,259)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,262)
  Gcoeff = (c(2)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,265)
  Gcoeff = (c(1)*(M(52)+M(64)+M(76)-M(79)-M(82)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94)-M(97))+c(2)*(M(138)+M(162)-M(205)-M(211) &
    +M(222)+M(224)-M(229)-M(235))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,242)
  Gcoeff = (c(1)*(M(49)-M(52)+M(61)-M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100))+c(2)*(M(137)-M(138)+M(161)-M(162) &
    -M(222)-M(224)+M(246)+M(248))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,245)
  Gcoeff = (c(1)*(-M(49)-M(61)-M(73)+M(79)+M(82)+M(85)+M(91)+M(94)+M(97)-M(98)-M(99)-M(100))+c(2)*(-M(137)-M(161)+M(205)+M(211) &
    +M(229)+M(235)-M(246)-M(248))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,248)
  Gcoeff = (c(1)*(M(52)+M(64)+M(76)-M(79)-M(82)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94)-M(97))+c(2)*(M(132)+M(186)-M(203)-M(217) &
    +M(221)+M(226)-M(227)-M(241))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,251)
  Gcoeff = (c(1)*(M(49)-M(52)+M(61)-M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100))+c(2)*(M(131)-M(132)+M(185)-M(186) &
    -M(221)-M(226)+M(245)+M(250))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,254)
  Gcoeff = (c(1)*(-M(49)-M(61)-M(73)+M(79)+M(82)+M(85)+M(91)+M(94)+M(97)-M(98)-M(99)-M(100))+c(2)*(-M(131)-M(185)+M(203)+M(217) &
    +M(227)+M(241)-M(245)-M(250))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,257)
  Gcoeff = (c(2)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,260)
  Gcoeff = (c(2)*(M(131)-M(132)-M(137)+M(138)-M(161)+M(162)+M(185)-M(186)-M(221)+M(222)+M(224)-M(226)+M(245)-M(246)-M(248) &
    +M(250))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,263)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,266)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,243)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,246)
  Gcoeff = (c(2)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,249)
  Gcoeff = (c(2)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,252)
  Gcoeff = (c(2)*(M(131)-M(132)-M(137)+M(138)-M(161)+M(162)+M(185)-M(186)-M(221)+M(222)+M(224)-M(226)+M(245)-M(246)-M(248) &
    +M(250))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,255)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,258)
  Gcoeff = (c(3)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,261)
  Gcoeff = (c(3)*(M(131)-M(132)-M(137)+M(138)-M(161)+M(162)+M(185)-M(186)-M(221)+M(222)+M(224)-M(226)+M(245)-M(246)-M(248) &
    +M(250))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,264)
  Gcoeff = (c(3)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(10)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,267)
  Gcoeff = (c(1)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(2)*(M(132)-M(138)-M(149) &
    +M(151)+M(212)-M(218)-M(224)+M(226))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,268)
  Gcoeff = (c(1)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(2)*(M(165)-M(189)+M(203) &
    -M(205)-M(207)+M(208)-M(235)+M(241))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,269)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(149)-M(151)+M(165)-M(189)+M(203)-M(205)-M(207)+M(208)-M(212)+M(218)+M(224)-M(226)-M(235) &
    +M(241))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,270)
  Gcoeff = (c(1)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(2)*(M(165)-M(189)+M(203) &
    -M(205)-M(207)+M(208)-M(235)+M(241))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,271)
  Gcoeff = (c(1)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(2)*(M(132)-M(138)-M(149) &
    +M(151)+M(212)-M(218)-M(224)+M(226))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,272)
  Gcoeff = (c(2)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,273)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(149)-M(151)+M(165)-M(189)+M(203)-M(205)-M(207)+M(208)-M(212)+M(218)+M(224)-M(226)-M(235) &
    +M(241))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,274)
  Gcoeff = (c(2)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,275)
  Gcoeff = (c(3)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,276)
  Gcoeff = (c(1)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(2)*(-M(132)+M(135)+M(141) &
    -M(151)-M(212)+M(214)+M(220)-M(226))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,277)
  Gcoeff = (c(1)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(2)*(-M(165)+M(175)+M(199) &
    -M(203)+M(204)+M(206)-M(208)-M(241))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,278)
  Gcoeff = (c(2)*(M(132)-M(135)-M(141)+M(151)-M(165)+M(175)+M(199)-M(203)+M(204)+M(206)-M(208)+M(212)-M(214)-M(220)+M(226) &
    -M(241))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,279)
  Gcoeff = (c(1)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(2)*(-M(165)+M(175)+M(199) &
    -M(203)+M(204)+M(206)-M(208)-M(241))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,280)
  Gcoeff = (c(1)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(2)*(-M(132)+M(135)+M(141) &
    -M(151)-M(212)+M(214)+M(220)-M(226))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,281)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(141)-M(151)+M(165)-M(175)-M(199)+M(203)-M(204)-M(206)+M(208)-M(212)+M(214)+M(220)-M(226) &
    +M(241))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,282)
  Gcoeff = (c(2)*(M(132)-M(135)-M(141)+M(151)-M(165)+M(175)+M(199)-M(203)+M(204)+M(206)-M(208)+M(212)-M(214)-M(220)+M(226) &
    -M(241))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,283)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(141)-M(151)+M(165)-M(175)-M(199)+M(203)-M(204)-M(206)+M(208)-M(212)+M(214)+M(220)-M(226) &
    +M(241))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,284)
  Gcoeff = (c(3)*(-M(132)+M(135)+M(141)-M(151)+M(165)-M(175)-M(199)+M(203)-M(204)-M(206)+M(208)-M(212)+M(214)+M(220)-M(226) &
    +M(241))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,285)
  Gcoeff = (c(1)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(2)*(-M(135)+M(138)-M(141) &
    +M(149)-M(214)+M(218)-M(220)+M(224))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,286)
  Gcoeff = (c(1)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(2)*(-M(175)+M(189)-M(199) &
    -M(204)+M(205)-M(206)+M(207)+M(235))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,287)
  Gcoeff = (c(2)*(M(135)-M(138)+M(141)-M(149)-M(175)+M(189)-M(199)-M(204)+M(205)-M(206)+M(207)+M(214)-M(218)+M(220)-M(224) &
    +M(235))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,288)
  Gcoeff = (c(1)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(2)*(-M(175)+M(189)-M(199) &
    -M(204)+M(205)-M(206)+M(207)+M(235))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,289)
  Gcoeff = (c(1)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(2)*(-M(135)+M(138)-M(141) &
    +M(149)-M(214)+M(218)-M(220)+M(224))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,290)
  Gcoeff = (c(2)*(-M(135)+M(138)-M(141)+M(149)+M(175)-M(189)+M(199)+M(204)-M(205)+M(206)-M(207)-M(214)+M(218)-M(220)+M(224) &
    -M(235))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,291)
  Gcoeff = (c(2)*(M(135)-M(138)+M(141)-M(149)-M(175)+M(189)-M(199)-M(204)+M(205)-M(206)+M(207)+M(214)-M(218)+M(220)-M(224) &
    +M(235))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,292)
  Gcoeff = (c(2)*(-M(135)+M(138)-M(141)+M(149)+M(175)-M(189)+M(199)+M(204)-M(205)+M(206)-M(207)-M(214)+M(218)-M(220)+M(224) &
    -M(235))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,293)
  Gcoeff = (c(3)*(-M(135)+M(138)-M(141)+M(149)+M(175)-M(189)+M(199)+M(204)-M(205)+M(206)-M(207)-M(214)+M(218)-M(220)+M(224) &
    -M(235))) * den(28)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,294)
  Gcoeff = (c(1)*(M(49)-M(79)-M(91)+M(98)+M(106)+M(112)-M(114)-M(116)+M(117)+M(118)-M(120)-M(122))+c(2)*(M(148)+M(161)-M(163) &
    -M(165)+M(166)-M(208)-M(232)+M(246))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,295)
  Gcoeff = (c(1)*(M(49)-M(79)-M(91)+M(98)+M(106)+M(112)-M(114)-M(116)+M(117)+M(118)-M(120)-M(122))+c(2)*(M(147)+M(185)-M(187) &
    -M(189)+M(190)-M(207)-M(231)+M(245))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,298)
  Gcoeff = (c(2)*(M(147)-M(148)-M(161)+M(163)+M(165)-M(166)+M(185)-M(187)-M(189)+M(190)-M(207)+M(208)-M(231)+M(232)+M(245) &
    -M(246))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,301)
  Gcoeff = (c(1)*(-M(49)+M(52)+M(86)-M(98)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124))+c(2)*(-M(148)+M(154)-M(161) &
    +M(162)+M(164)-M(166)+M(222)-M(246))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,304)
  Gcoeff = (c(1)*(-M(49)+M(52)+M(86)-M(98)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124))+c(2)*(-M(147)+M(153)-M(185) &
    +M(186)+M(188)-M(190)+M(221)-M(245))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,307)
  Gcoeff = (c(2)*(-M(147)+M(148)+M(153)-M(154)+M(161)-M(162)-M(164)+M(166)-M(185)+M(186)+M(188)-M(190)+M(221)-M(222)-M(245) &
    +M(246))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,310)
  Gcoeff = (c(1)*(-M(52)+M(79)-M(86)+M(91)-M(104)-M(110)+M(114)+M(116)+M(120)+M(122)-M(123)-M(124))+c(2)*(-M(154)-M(162)+M(163) &
    -M(164)+M(165)+M(208)-M(222)+M(232))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,313)
  Gcoeff = (c(1)*(-M(52)+M(79)-M(86)+M(91)-M(104)-M(110)+M(114)+M(116)+M(120)+M(122)-M(123)-M(124))+c(2)*(-M(153)-M(186)+M(187) &
    -M(188)+M(189)+M(207)-M(221)+M(231))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,316)
  Gcoeff = (c(2)*(-M(153)+M(154)+M(162)-M(163)+M(164)-M(165)-M(186)+M(187)-M(188)+M(189)+M(207)-M(208)-M(221)+M(222)+M(231) &
    -M(232))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,319)
  Gcoeff = (c(1)*(M(49)-M(79)-M(91)+M(98)+M(106)+M(112)-M(114)-M(116)+M(117)+M(118)-M(120)-M(122))+c(2)*(M(147)+M(185)-M(187) &
    -M(189)+M(190)-M(207)-M(231)+M(245))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,296)
  Gcoeff = (c(1)*(M(49)-M(79)-M(91)+M(98)+M(106)+M(112)-M(114)-M(116)+M(117)+M(118)-M(120)-M(122))+c(2)*(M(148)+M(161)-M(163) &
    -M(165)+M(166)-M(208)-M(232)+M(246))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,299)
  Gcoeff = (c(2)*(-M(147)+M(148)+M(161)-M(163)-M(165)+M(166)-M(185)+M(187)+M(189)-M(190)+M(207)-M(208)+M(231)-M(232)-M(245) &
    +M(246))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,302)
  Gcoeff = (c(1)*(-M(49)+M(52)+M(86)-M(98)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124))+c(2)*(-M(147)+M(153)-M(185) &
    +M(186)+M(188)-M(190)+M(221)-M(245))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,305)
  Gcoeff = (c(1)*(-M(49)+M(52)+M(86)-M(98)+M(104)-M(106)+M(110)-M(112)-M(117)-M(118)+M(123)+M(124))+c(2)*(-M(148)+M(154)-M(161) &
    +M(162)+M(164)-M(166)+M(222)-M(246))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,308)
  Gcoeff = (c(2)*(M(147)-M(148)-M(153)+M(154)-M(161)+M(162)+M(164)-M(166)+M(185)-M(186)-M(188)+M(190)-M(221)+M(222)+M(245) &
    -M(246))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,311)
  Gcoeff = (c(1)*(-M(52)+M(79)-M(86)+M(91)-M(104)-M(110)+M(114)+M(116)+M(120)+M(122)-M(123)-M(124))+c(2)*(-M(153)-M(186)+M(187) &
    -M(188)+M(189)+M(207)-M(221)+M(231))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,314)
  Gcoeff = (c(1)*(-M(52)+M(79)-M(86)+M(91)-M(104)-M(110)+M(114)+M(116)+M(120)+M(122)-M(123)-M(124))+c(2)*(-M(154)-M(162)+M(163) &
    -M(164)+M(165)+M(208)-M(222)+M(232))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,317)
  Gcoeff = (c(2)*(M(153)-M(154)-M(162)+M(163)-M(164)+M(165)+M(186)-M(187)+M(188)-M(189)-M(207)+M(208)+M(221)-M(222)-M(231) &
    +M(232))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,320)
  Gcoeff = (c(2)*(M(147)-M(148)-M(161)+M(163)+M(165)-M(166)+M(185)-M(187)-M(189)+M(190)-M(207)+M(208)-M(231)+M(232)+M(245) &
    -M(246))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,297)
  Gcoeff = (c(2)*(-M(147)+M(148)+M(161)-M(163)-M(165)+M(166)-M(185)+M(187)+M(189)-M(190)+M(207)-M(208)+M(231)-M(232)-M(245) &
    +M(246))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,300)
  Gcoeff = (c(3)*(-M(147)+M(148)+M(161)-M(163)-M(165)+M(166)-M(185)+M(187)+M(189)-M(190)+M(207)-M(208)+M(231)-M(232)-M(245) &
    +M(246))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,303)
  Gcoeff = (c(2)*(-M(147)+M(148)+M(153)-M(154)+M(161)-M(162)-M(164)+M(166)-M(185)+M(186)+M(188)-M(190)+M(221)-M(222)-M(245) &
    +M(246))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,306)
  Gcoeff = (c(2)*(M(147)-M(148)-M(153)+M(154)-M(161)+M(162)+M(164)-M(166)+M(185)-M(186)-M(188)+M(190)-M(221)+M(222)+M(245) &
    -M(246))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,309)
  Gcoeff = (c(3)*(M(147)-M(148)-M(153)+M(154)-M(161)+M(162)+M(164)-M(166)+M(185)-M(186)-M(188)+M(190)-M(221)+M(222)+M(245) &
    -M(246))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,312)
  Gcoeff = (c(2)*(-M(153)+M(154)+M(162)-M(163)+M(164)-M(165)-M(186)+M(187)-M(188)+M(189)+M(207)-M(208)-M(221)+M(222)+M(231) &
    -M(232))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,315)
  Gcoeff = (c(2)*(M(153)-M(154)-M(162)+M(163)-M(164)+M(165)+M(186)-M(187)+M(188)-M(189)-M(207)+M(208)+M(221)-M(222)-M(231) &
    +M(232))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,318)
  Gcoeff = (c(3)*(M(153)-M(154)-M(162)+M(163)-M(164)+M(165)+M(186)-M(187)+M(188)-M(189)-M(207)+M(208)+M(221)-M(222)-M(231) &
    +M(232))) * den(62)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,321)
  Gcoeff = (c(1)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(2)*(M(143)-M(145)-M(147) &
    +M(148)+M(166)-M(190)-M(236)+M(242))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,322)
  Gcoeff = (c(1)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(2)*(M(165)-M(189)+M(203) &
    -M(205)-M(207)+M(208)-M(235)+M(241))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,325)
  Gcoeff = (c(2)*(-M(143)+M(145)+M(147)-M(148)+M(165)-M(166)-M(189)+M(190)+M(203)-M(205)-M(207)+M(208)-M(235)+M(236)+M(241) &
    -M(242))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,328)
  Gcoeff = (c(1)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(2)*(M(165)-M(189)+M(203) &
    -M(205)-M(207)+M(208)-M(235)+M(241))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,323)
  Gcoeff = (c(1)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(2)*(M(143)-M(145)-M(147) &
    +M(148)+M(166)-M(190)-M(236)+M(242))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,326)
  Gcoeff = (c(2)*(M(143)-M(145)-M(147)+M(148)-M(165)+M(166)+M(189)-M(190)-M(203)+M(205)+M(207)-M(208)+M(235)-M(236)-M(241) &
    +M(242))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,329)
  Gcoeff = (c(2)*(-M(143)+M(145)+M(147)-M(148)+M(165)-M(166)-M(189)+M(190)+M(203)-M(205)-M(207)+M(208)-M(235)+M(236)+M(241) &
    -M(242))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,324)
  Gcoeff = (c(2)*(M(143)-M(145)-M(147)+M(148)-M(165)+M(166)+M(189)-M(190)-M(203)+M(205)+M(207)-M(208)+M(235)-M(236)-M(241) &
    +M(242))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,327)
  Gcoeff = (c(3)*(M(143)-M(145)-M(147)+M(148)-M(165)+M(166)+M(189)-M(190)-M(203)+M(205)+M(207)-M(208)+M(235)-M(236)-M(241) &
    +M(242))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,330)
  Gcoeff = (c(1)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(2)*(-M(143)+M(144)+M(146) &
    -M(148)-M(166)+M(176)+M(200)-M(242))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,331)
  Gcoeff = (c(1)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(2)*(-M(165)+M(175)+M(199) &
    -M(203)+M(204)+M(206)-M(208)-M(241))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,334)
  Gcoeff = (c(2)*(M(143)-M(144)-M(146)+M(148)-M(165)+M(166)+M(175)-M(176)+M(199)-M(200)-M(203)+M(204)+M(206)-M(208)-M(241) &
    +M(242))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,337)
  Gcoeff = (c(1)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(2)*(-M(165)+M(175)+M(199) &
    -M(203)+M(204)+M(206)-M(208)-M(241))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,332)
  Gcoeff = (c(1)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(2)*(-M(143)+M(144)+M(146) &
    -M(148)-M(166)+M(176)+M(200)-M(242))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,335)
  Gcoeff = (c(2)*(-M(143)+M(144)+M(146)-M(148)+M(165)-M(166)-M(175)+M(176)-M(199)+M(200)+M(203)-M(204)-M(206)+M(208)+M(241) &
    -M(242))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,338)
  Gcoeff = (c(2)*(M(143)-M(144)-M(146)+M(148)-M(165)+M(166)+M(175)-M(176)+M(199)-M(200)-M(203)+M(204)+M(206)-M(208)-M(241) &
    +M(242))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,333)
  Gcoeff = (c(2)*(-M(143)+M(144)+M(146)-M(148)+M(165)-M(166)-M(175)+M(176)-M(199)+M(200)+M(203)-M(204)-M(206)+M(208)+M(241) &
    -M(242))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,336)
  Gcoeff = (c(3)*(-M(143)+M(144)+M(146)-M(148)+M(165)-M(166)-M(175)+M(176)-M(199)+M(200)+M(203)-M(204)-M(206)+M(208)+M(241) &
    -M(242))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,339)
  Gcoeff = (c(1)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(2)*(-M(144)+M(145)-M(146) &
    +M(147)-M(176)+M(190)-M(200)+M(236))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,340)
  Gcoeff = (c(1)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(2)*(-M(175)+M(189)-M(199) &
    -M(204)+M(205)-M(206)+M(207)+M(235))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,343)
  Gcoeff = (c(2)*(M(144)-M(145)+M(146)-M(147)-M(175)+M(176)+M(189)-M(190)-M(199)+M(200)-M(204)+M(205)-M(206)+M(207)+M(235) &
    -M(236))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,346)
  Gcoeff = (c(1)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(2)*(-M(175)+M(189)-M(199) &
    -M(204)+M(205)-M(206)+M(207)+M(235))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,341)
  Gcoeff = (c(1)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(2)*(-M(144)+M(145)-M(146) &
    +M(147)-M(176)+M(190)-M(200)+M(236))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,344)
  Gcoeff = (c(2)*(-M(144)+M(145)-M(146)+M(147)+M(175)-M(176)-M(189)+M(190)+M(199)-M(200)+M(204)-M(205)+M(206)-M(207)-M(235) &
    +M(236))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,347)
  Gcoeff = (c(2)*(M(144)-M(145)+M(146)-M(147)-M(175)+M(176)+M(189)-M(190)-M(199)+M(200)-M(204)+M(205)-M(206)+M(207)+M(235) &
    -M(236))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,342)
  Gcoeff = (c(2)*(-M(144)+M(145)-M(146)+M(147)+M(175)-M(176)-M(189)+M(190)+M(199)-M(200)+M(204)-M(205)+M(206)-M(207)-M(235) &
    +M(236))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,345)
  Gcoeff = (c(3)*(-M(144)+M(145)-M(146)+M(147)+M(175)-M(176)-M(189)+M(190)+M(199)-M(200)+M(204)-M(205)+M(206)-M(207)-M(235) &
    +M(236))) * den(28)
  T2sum(1:1,8) = T2sum(1:1,8) + Gcoeff * G0tensor(:,348)
  Gcoeff = (c(2)*(M(147)-M(158)+M(168)+M(174)-M(177)-M(182)+M(185)-M(187)-M(189)+M(190)+M(192)+M(198)-M(201)-M(207)-M(231) &
    +M(245))) * den(71)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,18)
  Gcoeff = (c(2)*(-M(147)+M(153)+M(158)-M(160)-M(171)+M(177)+M(182)-M(184)-M(185)+M(186)+M(188)-M(190)-M(195)+M(201)+M(221) &
    -M(245))) * den(71)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,19)
  Gcoeff = (c(2)*(-M(153)+M(160)-M(168)+M(171)-M(174)+M(184)-M(186)+M(187)-M(188)+M(189)-M(192)+M(195)-M(198)+M(207)-M(221) &
    +M(231))) * den(71)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,20)
  Gcoeff = (c(2)*(-M(147)+M(158)-M(168)-M(174)+M(177)+M(182)-M(185)+M(187)+M(189)-M(190)-M(192)-M(198)+M(201)+M(207)+M(231) &
    -M(245))) * den(71)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,21)
  Gcoeff = (c(2)*(M(147)-M(153)-M(158)+M(160)+M(171)-M(177)-M(182)+M(184)+M(185)-M(186)-M(188)+M(190)+M(195)-M(201)-M(221) &
    +M(245))) * den(71)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,22)
  Gcoeff = (c(2)*(M(153)-M(160)+M(168)-M(171)+M(174)-M(184)+M(186)-M(187)+M(188)-M(189)+M(192)-M(195)+M(198)-M(207)+M(221) &
    -M(231))) * den(71)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,23)
  Gcoeff = (c(3)*(-M(147)+M(158)-M(168)-M(174)+M(177)+M(182)-M(185)+M(187)+M(189)-M(190)-M(192)-M(198)+M(201)+M(207)+M(231) &
    -M(245))) * den(71)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,24)
  Gcoeff = (c(3)*(M(147)-M(153)-M(158)+M(160)+M(171)-M(177)-M(182)+M(184)+M(185)-M(186)-M(188)+M(190)+M(195)-M(201)-M(221) &
    +M(245))) * den(71)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,25)
  Gcoeff = (c(3)*(M(153)-M(160)+M(168)-M(171)+M(174)-M(184)+M(186)-M(187)+M(188)-M(189)+M(192)-M(195)+M(198)-M(207)+M(221) &
    -M(231))) * den(71)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,26)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(50)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,36)
  Gcoeff = (c(2)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(50)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,37)
  Gcoeff = (c(3)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(50)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,38)
  Gcoeff = (c(2)*(M(134)-M(136)-M(147)+M(153)-M(171)+M(177)-M(179)+M(180)+M(182)-M(184)+M(188)-M(190)-M(196)+M(202)+M(223) &
    -M(247))) * den(50)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,39)
  Gcoeff = (c(2)*(-M(134)+M(136)+M(147)-M(153)+M(171)-M(177)+M(179)-M(180)-M(182)+M(184)-M(188)+M(190)+M(196)-M(202)-M(223) &
    +M(247))) * den(50)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,40)
  Gcoeff = (c(3)*(-M(134)+M(136)+M(147)-M(153)+M(171)-M(177)+M(179)-M(180)-M(182)+M(184)-M(188)+M(190)+M(196)-M(202)-M(223) &
    +M(247))) * den(50)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,41)
  Gcoeff = (c(2)*(M(136)-M(144)+M(147)-M(150)-M(177)-M(180)+M(181)-M(182)+M(183)+M(190)-M(194)+M(196)-M(200)+M(213)-M(223) &
    +M(237))) * den(50)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,42)
  Gcoeff = (c(2)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(50)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,43)
  Gcoeff = (c(3)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(50)
  T2sum(1:5,13) = T2sum(1:5,13) + Gcoeff * G1tensor(:,44)
  Gcoeff = (c(2)*(M(141)-M(156)+M(162)+M(173)-M(175)-M(183)-M(206)+M(209)-M(211)-M(213)+M(214)+M(216)+M(222)-M(225)-M(229) &
    +M(239))) * den(76)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,54)
  Gcoeff = (c(2)*(-M(141)+M(151)+M(156)-M(159)-M(165)+M(175)+M(197)+M(206)-M(208)-M(209)+M(210)+M(212)-M(214)-M(219)+M(225) &
    -M(239))) * den(76)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,55)
  Gcoeff = (c(2)*(-M(151)+M(159)-M(162)+M(165)-M(173)+M(183)-M(197)+M(208)-M(210)+M(211)-M(212)+M(213)-M(216)+M(219)-M(222) &
    +M(229))) * den(76)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,56)
  Gcoeff = (c(2)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(76)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,57)
  Gcoeff = (c(2)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(76)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,58)
  Gcoeff = (c(2)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(76)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,59)
  Gcoeff = (c(3)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(76)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,60)
  Gcoeff = (c(3)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(76)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,61)
  Gcoeff = (c(3)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(76)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,62)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(149)-M(151)+M(165)-M(189)+M(203)-M(205)-M(207)+M(208)-M(212)+M(218)+M(224)-M(226)-M(235) &
    +M(241))) * den(55)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,90)
  Gcoeff = (c(2)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(55)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,91)
  Gcoeff = (c(3)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(55)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,92)
  Gcoeff = (c(2)*(M(132)-M(135)-M(141)+M(151)-M(165)+M(175)+M(199)-M(203)+M(204)+M(206)-M(208)+M(212)-M(214)-M(220)+M(226) &
    -M(241))) * den(55)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,93)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(141)-M(151)+M(165)-M(175)-M(199)+M(203)-M(204)-M(206)+M(208)-M(212)+M(214)+M(220)-M(226) &
    +M(241))) * den(55)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,94)
  Gcoeff = (c(3)*(-M(132)+M(135)+M(141)-M(151)+M(165)-M(175)-M(199)+M(203)-M(204)-M(206)+M(208)-M(212)+M(214)+M(220)-M(226) &
    +M(241))) * den(55)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,95)
  Gcoeff = (c(2)*(M(135)-M(138)+M(141)-M(149)-M(175)+M(189)-M(199)-M(204)+M(205)-M(206)+M(207)+M(214)-M(218)+M(220)-M(224) &
    +M(235))) * den(55)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,96)
  Gcoeff = (c(2)*(-M(135)+M(138)-M(141)+M(149)+M(175)-M(189)+M(199)+M(204)-M(205)+M(206)-M(207)-M(214)+M(218)-M(220)+M(224) &
    -M(235))) * den(55)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,97)
  Gcoeff = (c(3)*(-M(135)+M(138)-M(141)+M(149)+M(175)-M(189)+M(199)+M(204)-M(205)+M(206)-M(207)-M(214)+M(218)-M(220)+M(224) &
    -M(235))) * den(55)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,98)
  Gcoeff = (c(2)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(47)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(3)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(47)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,2)
  Gcoeff = (c(2)*(M(131)-M(132)-M(134)+M(136)-M(155)+M(156)+M(158)-M(160)-M(195)+M(196)+M(201)-M(202)+M(225)-M(226)-M(249) &
    +M(250))) * den(47)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,3)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(134)-M(136)+M(155)-M(156)-M(158)+M(160)+M(195)-M(196)-M(201)+M(202)-M(225)+M(226)+M(249) &
    -M(250))) * den(47)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,4)
  Gcoeff = (c(3)*(-M(131)+M(132)+M(134)-M(136)+M(155)-M(156)-M(158)+M(160)+M(195)-M(196)-M(201)+M(202)-M(225)+M(226)+M(249) &
    -M(250))) * den(47)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,5)
  Gcoeff = (c(2)*(M(132)-M(133)+M(134)-M(135)-M(156)+M(157)-M(158)+M(159)-M(201)+M(202)+M(219)-M(220)-M(225)+M(226)+M(243) &
    -M(244))) * den(47)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,6)
  Gcoeff = (c(2)*(-M(132)+M(133)-M(134)+M(135)+M(156)-M(157)+M(158)-M(159)+M(201)-M(202)-M(219)+M(220)+M(225)-M(226)-M(243) &
    +M(244))) * den(47)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,7)
  Gcoeff = (c(3)*(-M(132)+M(133)-M(134)+M(135)+M(156)-M(157)+M(158)-M(159)+M(201)-M(202)-M(219)+M(220)+M(225)-M(226)-M(243) &
    +M(244))) * den(47)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,8)
  Gcoeff = (c(2)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(104)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,45)
  Gcoeff = (c(2)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(104)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,46)
  Gcoeff = (c(2)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(104)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,47)
  Gcoeff = (c(2)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(104)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,48)
  Gcoeff = (c(2)*(-M(137)+M(138)+M(155)-M(156)-M(161)+M(162)+M(179)-M(180)+M(222)-M(223)+M(224)-M(225)-M(246)+M(247)-M(248) &
    +M(249))) * den(104)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,49)
  Gcoeff = (c(2)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(104)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,50)
  Gcoeff = (c(3)*(-M(138)+M(156)-M(162)+M(180)+M(205)-M(209)+M(211)-M(215)-M(222)+M(223)-M(224)+M(225)+M(229)-M(233)+M(235) &
    -M(239))) * den(104)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,51)
  Gcoeff = (c(3)*(-M(137)+M(138)+M(155)-M(156)-M(161)+M(162)+M(179)-M(180)+M(222)-M(223)+M(224)-M(225)-M(246)+M(247)-M(248) &
    +M(249))) * den(104)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,52)
  Gcoeff = (c(3)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(104)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,53)
  Gcoeff = (c(2)*(-M(137)+M(139)+M(141)-M(142)+M(171)-M(172)+M(179)-M(181)-M(183)+M(184)-M(213)+M(214)-M(237)+M(238)+M(247) &
    -M(248))) * den(53)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,72)
  Gcoeff = (c(2)*(M(137)-M(139)-M(141)+M(142)-M(171)+M(172)-M(179)+M(181)+M(183)-M(184)+M(213)-M(214)+M(237)-M(238)-M(247) &
    +M(248))) * den(53)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,73)
  Gcoeff = (c(3)*(M(137)-M(139)-M(141)+M(142)-M(171)+M(172)-M(179)+M(181)+M(183)-M(184)+M(213)-M(214)+M(237)-M(238)-M(247) &
    +M(248))) * den(53)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,74)
  Gcoeff = (c(2)*(M(137)-M(138)-M(140)+M(142)-M(171)+M(172)+M(177)-M(178)-M(179)+M(180)+M(182)-M(184)+M(223)-M(224)-M(247) &
    +M(248))) * den(53)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,75)
  Gcoeff = (c(2)*(-M(137)+M(138)+M(140)-M(142)+M(171)-M(172)-M(177)+M(178)+M(179)-M(180)-M(182)+M(184)-M(223)+M(224)+M(247) &
    -M(248))) * den(53)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,76)
  Gcoeff = (c(3)*(-M(137)+M(138)+M(140)-M(142)+M(171)-M(172)-M(177)+M(178)+M(179)-M(180)-M(182)+M(184)-M(223)+M(224)+M(247) &
    -M(248))) * den(53)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,77)
  Gcoeff = (c(2)*(M(138)-M(139)+M(140)-M(141)-M(177)+M(178)-M(180)+M(181)-M(182)+M(183)+M(213)-M(214)-M(223)+M(224)+M(237) &
    -M(238))) * den(53)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,78)
  Gcoeff = (c(2)*(-M(138)+M(139)-M(140)+M(141)+M(177)-M(178)+M(180)-M(181)+M(182)-M(183)-M(213)+M(214)+M(223)-M(224)-M(237) &
    +M(238))) * den(53)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,79)
  Gcoeff = (c(3)*(-M(138)+M(139)-M(140)+M(141)+M(177)-M(178)+M(180)-M(181)+M(182)-M(183)-M(213)+M(214)+M(223)-M(224)-M(237) &
    +M(238))) * den(53)
  T2sum(1:5,10) = T2sum(1:5,10) + Gcoeff * G1tensor(:,80)
  Gcoeff = (c(2)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(109)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,9)
  Gcoeff = (c(2)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(109)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,10)
  Gcoeff = (c(2)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(109)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,11)
  Gcoeff = (c(2)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(109)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,12)
  Gcoeff = (c(2)*(-M(143)+M(144)+M(157)-M(158)-M(167)+M(168)+M(198)-M(199)+M(200)-M(201)+M(203)-M(204)-M(240)+M(241)-M(242) &
    +M(243))) * den(109)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,13)
  Gcoeff = (c(2)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(109)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,14)
  Gcoeff = (c(3)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(109)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,15)
  Gcoeff = (c(3)*(-M(143)+M(144)+M(157)-M(158)-M(167)+M(168)+M(198)-M(199)+M(200)-M(201)+M(203)-M(204)-M(240)+M(241)-M(242) &
    +M(243))) * den(109)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,16)
  Gcoeff = (c(3)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(109)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,17)
  Gcoeff = (c(2)*(-M(143)+M(145)+M(147)-M(148)+M(165)-M(166)-M(189)+M(190)+M(203)-M(205)-M(207)+M(208)-M(235)+M(236)+M(241) &
    -M(242))) * den(57)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,108)
  Gcoeff = (c(2)*(M(143)-M(145)-M(147)+M(148)-M(165)+M(166)+M(189)-M(190)-M(203)+M(205)+M(207)-M(208)+M(235)-M(236)-M(241) &
    +M(242))) * den(57)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,109)
  Gcoeff = (c(3)*(M(143)-M(145)-M(147)+M(148)-M(165)+M(166)+M(189)-M(190)-M(203)+M(205)+M(207)-M(208)+M(235)-M(236)-M(241) &
    +M(242))) * den(57)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,110)
  Gcoeff = (c(2)*(M(143)-M(144)-M(146)+M(148)-M(165)+M(166)+M(175)-M(176)+M(199)-M(200)-M(203)+M(204)+M(206)-M(208)-M(241) &
    +M(242))) * den(57)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,111)
  Gcoeff = (c(2)*(-M(143)+M(144)+M(146)-M(148)+M(165)-M(166)-M(175)+M(176)-M(199)+M(200)+M(203)-M(204)-M(206)+M(208)+M(241) &
    -M(242))) * den(57)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,112)
  Gcoeff = (c(3)*(-M(143)+M(144)+M(146)-M(148)+M(165)-M(166)-M(175)+M(176)-M(199)+M(200)+M(203)-M(204)-M(206)+M(208)+M(241) &
    -M(242))) * den(57)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,113)
  Gcoeff = (c(2)*(M(144)-M(145)+M(146)-M(147)-M(175)+M(176)+M(189)-M(190)-M(199)+M(200)-M(204)+M(205)-M(206)+M(207)+M(235) &
    -M(236))) * den(57)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,114)
  Gcoeff = (c(2)*(-M(144)+M(145)-M(146)+M(147)+M(175)-M(176)-M(189)+M(190)+M(199)-M(200)+M(204)-M(205)+M(206)-M(207)-M(235) &
    +M(236))) * den(57)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,115)
  Gcoeff = (c(3)*(-M(144)+M(145)-M(146)+M(147)+M(175)-M(176)-M(189)+M(190)+M(199)-M(200)+M(204)-M(205)+M(206)-M(207)-M(235) &
    +M(236))) * den(57)
  T2sum(1:5,8) = T2sum(1:5,8) + Gcoeff * G1tensor(:,116)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(107)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,81)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(107)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,82)
  Gcoeff = (c(2)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(107)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,83)
  Gcoeff = (c(2)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(107)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,84)
  Gcoeff = (c(2)*(M(131)-M(132)-M(137)+M(138)-M(161)+M(162)+M(185)-M(186)-M(221)+M(222)+M(224)-M(226)+M(245)-M(246)-M(248) &
    +M(250))) * den(107)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,85)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(107)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,86)
  Gcoeff = (c(3)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(107)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,87)
  Gcoeff = (c(3)*(M(131)-M(132)-M(137)+M(138)-M(161)+M(162)+M(185)-M(186)-M(221)+M(222)+M(224)-M(226)+M(245)-M(246)-M(248) &
    +M(250))) * den(107)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,88)
  Gcoeff = (c(3)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(107)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,89)
  Gcoeff = (c(2)*(M(147)-M(148)-M(161)+M(163)+M(165)-M(166)+M(185)-M(187)-M(189)+M(190)-M(207)+M(208)-M(231)+M(232)+M(245) &
    -M(246))) * den(74)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,99)
  Gcoeff = (c(2)*(-M(147)+M(148)+M(161)-M(163)-M(165)+M(166)-M(185)+M(187)+M(189)-M(190)+M(207)-M(208)+M(231)-M(232)-M(245) &
    +M(246))) * den(74)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,100)
  Gcoeff = (c(3)*(-M(147)+M(148)+M(161)-M(163)-M(165)+M(166)-M(185)+M(187)+M(189)-M(190)+M(207)-M(208)+M(231)-M(232)-M(245) &
    +M(246))) * den(74)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,101)
  Gcoeff = (c(2)*(-M(147)+M(148)+M(153)-M(154)+M(161)-M(162)-M(164)+M(166)-M(185)+M(186)+M(188)-M(190)+M(221)-M(222)-M(245) &
    +M(246))) * den(74)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,102)
  Gcoeff = (c(2)*(M(147)-M(148)-M(153)+M(154)-M(161)+M(162)+M(164)-M(166)+M(185)-M(186)-M(188)+M(190)-M(221)+M(222)+M(245) &
    -M(246))) * den(74)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,103)
  Gcoeff = (c(3)*(M(147)-M(148)-M(153)+M(154)-M(161)+M(162)+M(164)-M(166)+M(185)-M(186)-M(188)+M(190)-M(221)+M(222)+M(245) &
    -M(246))) * den(74)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,104)
  Gcoeff = (c(2)*(-M(153)+M(154)+M(162)-M(163)+M(164)-M(165)-M(186)+M(187)-M(188)+M(189)+M(207)-M(208)-M(221)+M(222)+M(231) &
    -M(232))) * den(74)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,105)
  Gcoeff = (c(2)*(M(153)-M(154)-M(162)+M(163)-M(164)+M(165)+M(186)-M(187)+M(188)-M(189)-M(207)+M(208)+M(221)-M(222)-M(231) &
    +M(232))) * den(74)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,106)
  Gcoeff = (c(3)*(M(153)-M(154)-M(162)+M(163)-M(164)+M(165)+M(186)-M(187)+M(188)-M(189)-M(207)+M(208)+M(221)-M(222)-M(231) &
    +M(232))) * den(74)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,107)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(111)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,27)
  Gcoeff = (c(2)*(-M(133)+M(134)+M(143)-M(144)+M(167)-M(168)+M(197)-M(198)-M(200)+M(202)-M(209)+M(210)-M(239)+M(240)+M(242) &
    -M(244))) * den(111)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,28)
  Gcoeff = (c(2)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(111)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,29)
  Gcoeff = (c(2)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(111)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,30)
  Gcoeff = (c(2)*(M(133)-M(134)-M(143)+M(144)-M(167)+M(168)-M(197)+M(198)+M(200)-M(202)+M(209)-M(210)+M(239)-M(240)-M(242) &
    +M(244))) * den(111)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,31)
  Gcoeff = (c(2)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(111)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,32)
  Gcoeff = (c(3)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(111)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,33)
  Gcoeff = (c(3)*(M(133)-M(134)-M(143)+M(144)-M(167)+M(168)-M(197)+M(198)+M(200)-M(202)+M(209)-M(210)+M(239)-M(240)-M(242) &
    +M(244))) * den(111)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,34)
  Gcoeff = (c(3)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(111)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,35)
  Gcoeff = (c(2)*(M(141)-M(142)-M(167)+M(169)+M(171)-M(172)-M(183)+M(184)+M(209)-M(211)-M(213)+M(214)-M(229)+M(230)+M(239) &
    -M(240))) * den(78)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,63)
  Gcoeff = (c(2)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(78)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,64)
  Gcoeff = (c(3)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(78)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,65)
  Gcoeff = (c(2)*(-M(141)+M(142)+M(151)-M(152)+M(167)-M(168)-M(170)+M(172)+M(197)-M(198)-M(209)+M(210)+M(212)-M(214)-M(239) &
    +M(240))) * den(78)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,66)
  Gcoeff = (c(2)*(M(141)-M(142)-M(151)+M(152)-M(167)+M(168)+M(170)-M(172)-M(197)+M(198)+M(209)-M(210)-M(212)+M(214)+M(239) &
    -M(240))) * den(78)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,67)
  Gcoeff = (c(3)*(M(141)-M(142)-M(151)+M(152)-M(167)+M(168)+M(170)-M(172)-M(197)+M(198)+M(209)-M(210)-M(212)+M(214)+M(239) &
    -M(240))) * den(78)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,68)
  Gcoeff = (c(2)*(-M(151)+M(152)+M(168)-M(169)+M(170)-M(171)+M(183)-M(184)-M(197)+M(198)-M(210)+M(211)-M(212)+M(213)+M(229) &
    -M(230))) * den(78)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,69)
  Gcoeff = (c(2)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(78)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,70)
  Gcoeff = (c(3)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(78)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,71)

end subroutine vamp_71

end module ol_vamp_71_ppjjjj_gggggg_1_/**/REALKIND
