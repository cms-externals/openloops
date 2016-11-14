
module ol_vamp_66_ppjjjj_gggggg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_66(M)
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
  complex(REALKIND), dimension(4,1,4,13) :: G0
  complex(REALKIND), dimension(1,270) :: G0tensor
  complex(REALKIND), dimension(5,144) :: G1tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,-4),G0(:,:,:,2))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,1),G0tensor(:,1))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,3),G0tensor(:,2))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,4),G0tensor(:,3))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,1),wf(:,-3),G0tensor(:,4))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,3),wf(:,-3),G0tensor(:,5))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,4),wf(:,-3),G0tensor(:,6))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,1),G0tensor(:,7))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,3),G0tensor(:,8))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,4),G0tensor(:,9))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,7),G0tensor(:,10))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,9),G0tensor(:,11))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,10),G0tensor(:,12))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,7),wf(:,-2),G0tensor(:,13))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,9),wf(:,-2),G0tensor(:,14))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,10),wf(:,-2),G0tensor(:,15))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,7),G0tensor(:,16))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,9),G0tensor(:,17))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,10),G0tensor(:,18))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,25),G0tensor(:,19))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,27),G0tensor(:,20))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,28),G0tensor(:,21))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,25),wf(:,-1),G0tensor(:,22))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,27),wf(:,-1),G0tensor(:,23))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,28),wf(:,-1),G0tensor(:,24))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,25),G0tensor(:,25))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,27),G0tensor(:,26))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,28),G0tensor(:,27))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,0),wf(:,56),G0tensor(:,28))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,56),wf(:,0),G0tensor(:,29))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,0),wf(:,56),G0tensor(:,30))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,0),wf(:,59),G0tensor(:,31))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,59),wf(:,0),G0tensor(:,32))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,0),wf(:,59),G0tensor(:,33))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,0),wf(:,60),G0tensor(:,34))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,60),wf(:,0),G0tensor(:,35))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,0),wf(:,60),G0tensor(:,36))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,61),wf(:,62),G0tensor(:,37))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,62),wf(:,61),G0tensor(:,38))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,61),wf(:,62),G0tensor(:,39))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,74),G0tensor(:,40))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,74),wf(:,-3),G0tensor(:,41))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,74),G0tensor(:,42))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,83),G0tensor(:,43))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,83),wf(:,-2),G0tensor(:,44))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,83),G0tensor(:,45))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,85),Q(:,15),G1tensor(:,1))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,86),Q(:,15),G1tensor(:,2))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,87),Q(:,15),G1tensor(:,3))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,90),wf(:,91),G0tensor(:,46))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,91),wf(:,90),G0tensor(:,47))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,90),wf(:,91),G0tensor(:,48))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,103),G0tensor(:,49))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,103),wf(:,-3),G0tensor(:,50))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,103),G0tensor(:,51))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,104),wf(:,105),G0tensor(:,52))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,105),wf(:,104),G0tensor(:,53))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,104),wf(:,105),G0tensor(:,54))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,117),G0tensor(:,55))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,117),wf(:,-3),G0tensor(:,56))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,117),G0tensor(:,57))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,124),G0tensor(:,58))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,124),wf(:,-2),G0tensor(:,59))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,124),G0tensor(:,60))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,131),G0tensor(:,61))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,131),wf(:,-2),G0tensor(:,62))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,131),G0tensor(:,63))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,148),G0tensor(:,64))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,148),wf(:,-1),G0tensor(:,65))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,148),G0tensor(:,66))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,149),Q(:,15),G1tensor(:,4))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,150),Q(:,15),G1tensor(:,5))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,151),Q(:,15),G1tensor(:,6))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,160),G0tensor(:,67))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,160),wf(:,-1),G0tensor(:,68))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,160),G0tensor(:,69))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,167),G0tensor(:,70))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,167),wf(:,-1),G0tensor(:,71))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,167),G0tensor(:,72))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,178),Q(:,15),G1tensor(:,7))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,179),Q(:,15),G1tensor(:,8))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,180),Q(:,15),G1tensor(:,9))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1021),Q(:,15),G1tensor(:,10))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1022),Q(:,15),G1tensor(:,11))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1023),Q(:,15),G1tensor(:,12))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,0),wf(:,202),G0tensor(:,73))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,202),wf(:,0),G0tensor(:,74))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,0),wf(:,202),G0tensor(:,75))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,203),Q(:,15),G1tensor(:,13))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,204),Q(:,15),G1tensor(:,14))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,205),Q(:,15),G1tensor(:,15))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,0),wf(:,214),G0tensor(:,76))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,214),wf(:,0),G0tensor(:,77))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,0),wf(:,214),G0tensor(:,78))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,0),wf(:,221),G0tensor(:,79))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,221),wf(:,0),G0tensor(:,80))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,0),wf(:,221),G0tensor(:,81))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,232),Q(:,15),G1tensor(:,16))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,233),Q(:,15),G1tensor(:,17))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,234),Q(:,15),G1tensor(:,18))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1123),Q(:,15),G1tensor(:,19))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1124),Q(:,15),G1tensor(:,20))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1125),Q(:,15),G1tensor(:,21))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,250),Q(:,15),G1tensor(:,22))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,251),Q(:,15),G1tensor(:,23))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,252),Q(:,15),G1tensor(:,24))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1159),Q(:,15),G1tensor(:,25))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1160),Q(:,15),G1tensor(:,26))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1161),Q(:,15),G1tensor(:,27))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1168),Q(:,15),G1tensor(:,28))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1169),Q(:,15),G1tensor(:,29))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1170),Q(:,15),G1tensor(:,30))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,271),Q(:,15),G1tensor(:,31))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1321),Q(:,15),G1tensor(:,32))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1324),Q(:,15),G1tensor(:,33))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,274),Q(:,15),G1tensor(:,34))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1333),Q(:,15),G1tensor(:,35))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1336),Q(:,15),G1tensor(:,36))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,277),Q(:,15),G1tensor(:,37))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1345),Q(:,15),G1tensor(:,38))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1348),Q(:,15),G1tensor(:,39))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1357),Q(:,15),G1tensor(:,40))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1360),Q(:,15),G1tensor(:,41))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1369),Q(:,15),G1tensor(:,42))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1372),Q(:,15),G1tensor(:,43))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1381),Q(:,15),G1tensor(:,44))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,48),wf(:,1382),Q(:,15),G1tensor(:,45))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,7),G0(:,:,:,3))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-4),wf(:,-2),G0tensor(:,82))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-2),wf(:,-4),G0tensor(:,83))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-4),wf(:,-2),G0tensor(:,84))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,46))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,9),G0(:,:,:,4))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-4),wf(:,-2),G0tensor(:,85))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-2),wf(:,-4),G0tensor(:,86))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-4),wf(:,-2),G0tensor(:,87))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,47))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,10),G0(:,:,:,5))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,5),wf(:,-4),wf(:,-2),G0tensor(:,88))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,5),wf(:,-2),wf(:,-4),G0tensor(:,89))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,5),wf(:,-4),wf(:,-2),G0tensor(:,90))
  call check_last_UV_W(l_switch,G0(:,:,:,5),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,48))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,7),wf(:,-5),G0(:,:,:,6))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,6),wf(:,-4),wf(:,-2),G0tensor(:,91))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,6),wf(:,-2),wf(:,-4),G0tensor(:,92))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,6),wf(:,-4),wf(:,-2),G0tensor(:,93))
  call check_last_UV_W(l_switch,G0(:,:,:,6),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,49))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,9),wf(:,-5),G0(:,:,:,7))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,7),wf(:,-4),wf(:,-2),G0tensor(:,94))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,7),wf(:,-2),wf(:,-4),G0tensor(:,95))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,7),wf(:,-4),wf(:,-2),G0tensor(:,96))
  call check_last_UV_W(l_switch,G0(:,:,:,7),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,50))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,10),wf(:,-5),G0(:,:,:,8))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,8),wf(:,-4),wf(:,-2),G0tensor(:,97))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,8),wf(:,-2),wf(:,-4),G0tensor(:,98))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,8),wf(:,-4),wf(:,-2),G0tensor(:,99))
  call check_last_UV_W(l_switch,G0(:,:,:,8),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,51))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,7),G0(:,:,:,9))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,9),wf(:,-4),wf(:,-2),G0tensor(:,100))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,9),wf(:,-2),wf(:,-4),G0tensor(:,101))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,9),wf(:,-4),wf(:,-2),G0tensor(:,102))
  call check_last_UV_W(l_switch,G0(:,:,:,9),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,52))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,9),G0(:,:,:,10))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,10),wf(:,-4),wf(:,-2),G0tensor(:,103))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,10),wf(:,-2),wf(:,-4),G0tensor(:,104))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,10),wf(:,-4),wf(:,-2),G0tensor(:,105))
  call check_last_UV_W(l_switch,G0(:,:,:,10),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,53))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,10),G0(:,:,:,11))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,11),wf(:,-4),wf(:,-2),G0tensor(:,106))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,11),wf(:,-2),wf(:,-4),G0tensor(:,107))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,11),wf(:,-4),wf(:,-2),G0tensor(:,108))
  call check_last_UV_W(l_switch,G0(:,:,:,11),Q(:,43),wf(:,66),Q(:,20),G1tensor(:,54))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,-2),G0(:,:,:,12))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-4),wf(:,7),G0tensor(:,109))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-4),wf(:,9),G0tensor(:,110))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-4),wf(:,10),G0tensor(:,111))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,7),wf(:,-4),G0tensor(:,112))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,9),wf(:,-4),G0tensor(:,113))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,10),wf(:,-4),G0tensor(:,114))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,-4),wf(:,7),G0tensor(:,115))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,-4),wf(:,9),G0tensor(:,116))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,-4),wf(:,10),G0tensor(:,117))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-3),wf(:,13),G0tensor(:,118))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-3),wf(:,15),G0tensor(:,119))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-3),wf(:,16),G0tensor(:,120))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,13),wf(:,-3),G0tensor(:,121))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,15),wf(:,-3),G0tensor(:,122))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,16),wf(:,-3),G0tensor(:,123))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,-3),wf(:,13),G0tensor(:,124))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,-3),wf(:,15),G0tensor(:,125))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,-3),wf(:,16),G0tensor(:,126))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-1),wf(:,43),G0tensor(:,127))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-1),wf(:,45),G0tensor(:,128))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-1),wf(:,46),G0tensor(:,129))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,43),wf(:,-1),G0tensor(:,130))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,45),wf(:,-1),G0tensor(:,131))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,46),wf(:,-1),G0tensor(:,132))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,-1),wf(:,43),G0tensor(:,133))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,-1),wf(:,45),G0tensor(:,134))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,-1),wf(:,46),G0tensor(:,135))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,0),wf(:,38),G0tensor(:,136))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,38),wf(:,0),G0tensor(:,137))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,0),wf(:,38),G0tensor(:,138))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,0),wf(:,41),G0tensor(:,139))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,41),wf(:,0),G0tensor(:,140))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,0),wf(:,41),G0tensor(:,141))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,0),wf(:,42),G0tensor(:,142))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,42),wf(:,0),G0tensor(:,143))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,0),wf(:,42),G0tensor(:,144))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,61),wf(:,75),G0tensor(:,145))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,75),wf(:,61),G0tensor(:,146))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,61),wf(:,75),G0tensor(:,147))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-4),wf(:,83),G0tensor(:,148))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,83),wf(:,-4),G0tensor(:,149))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,-4),wf(:,83),G0tensor(:,150))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-3),wf(:,88),G0tensor(:,151))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,88),wf(:,-3),G0tensor(:,152))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,-3),wf(:,88),G0tensor(:,153))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,71),Q(:,27),G1tensor(:,55))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,72),Q(:,27),G1tensor(:,56))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,73),Q(:,27),G1tensor(:,57))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,95),wf(:,104),G0tensor(:,154))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,104),wf(:,95),G0tensor(:,155))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,95),wf(:,104),G0tensor(:,156))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-4),wf(:,124),G0tensor(:,157))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,124),wf(:,-4),G0tensor(:,158))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,-4),wf(:,124),G0tensor(:,159))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,91),wf(:,109),G0tensor(:,160))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,109),wf(:,91),G0tensor(:,161))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,91),wf(:,109),G0tensor(:,162))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-4),wf(:,131),G0tensor(:,163))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,131),wf(:,-4),G0tensor(:,164))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,-4),wf(:,131),G0tensor(:,165))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-3),wf(:,135),G0tensor(:,166))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,135),wf(:,-3),G0tensor(:,167))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,-3),wf(:,135),G0tensor(:,168))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-3),wf(:,139),G0tensor(:,169))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,139),wf(:,-3),G0tensor(:,170))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,-3),wf(:,139),G0tensor(:,171))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-1),wf(:,181),G0tensor(:,172))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,181),wf(:,-1),G0tensor(:,173))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,-1),wf(:,181),G0tensor(:,174))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,157),Q(:,27),G1tensor(:,58))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,158),Q(:,27),G1tensor(:,59))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,159),Q(:,27),G1tensor(:,60))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-1),wf(:,186),G0tensor(:,175))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,186),wf(:,-1),G0tensor(:,176))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,-1),wf(:,186),G0tensor(:,177))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-1),wf(:,190),G0tensor(:,178))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,190),wf(:,-1),G0tensor(:,179))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,-1),wf(:,190),G0tensor(:,180))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,168),Q(:,27),G1tensor(:,61))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,169),Q(:,27),G1tensor(:,62))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,170),Q(:,27),G1tensor(:,63))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,976),Q(:,27),G1tensor(:,64))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,977),Q(:,27),G1tensor(:,65))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,978),Q(:,27),G1tensor(:,66))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,0),wf(:,235),G0tensor(:,181))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,235),wf(:,0),G0tensor(:,182))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,0),wf(:,235),G0tensor(:,183))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,211),Q(:,27),G1tensor(:,67))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,212),Q(:,27),G1tensor(:,68))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,213),Q(:,27),G1tensor(:,69))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,0),wf(:,240),G0tensor(:,184))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,240),wf(:,0),G0tensor(:,185))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,0),wf(:,240),G0tensor(:,186))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,0),wf(:,244),G0tensor(:,187))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,244),wf(:,0),G0tensor(:,188))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,0),wf(:,244),G0tensor(:,189))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,222),Q(:,27),G1tensor(:,70))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,223),Q(:,27),G1tensor(:,71))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,224),Q(:,27),G1tensor(:,72))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1087),Q(:,27),G1tensor(:,73))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1088),Q(:,27),G1tensor(:,74))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1089),Q(:,27),G1tensor(:,75))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1141),Q(:,27),G1tensor(:,76))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1142),Q(:,27),G1tensor(:,77))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1143),Q(:,27),G1tensor(:,78))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1147),Q(:,27),G1tensor(:,79))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1148),Q(:,27),G1tensor(:,80))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1149),Q(:,27),G1tensor(:,81))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1156),Q(:,27),G1tensor(:,82))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1157),Q(:,27),G1tensor(:,83))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1158),Q(:,27),G1tensor(:,84))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1202),Q(:,27),G1tensor(:,85))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1325),Q(:,27),G1tensor(:,86))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1328),Q(:,27),G1tensor(:,87))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,280),Q(:,27),G1tensor(:,88))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1358),Q(:,27),G1tensor(:,89))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1363),Q(:,27),G1tensor(:,90))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,282),Q(:,27),G1tensor(:,91))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1370),Q(:,27),G1tensor(:,92))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1373),Q(:,27),G1tensor(:,93))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1394),Q(:,27),G1tensor(:,94))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1399),Q(:,27),G1tensor(:,95))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1406),Q(:,27),G1tensor(:,96))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1409),Q(:,27),G1tensor(:,97))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1429),Q(:,27),G1tensor(:,98))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,36),wf(:,1430),Q(:,27),G1tensor(:,99))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,-5),G0(:,:,:,13))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-4),wf(:,7),G0tensor(:,190))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-4),wf(:,9),G0tensor(:,191))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-4),wf(:,10),G0tensor(:,192))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,7),wf(:,-4),G0tensor(:,193))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,9),wf(:,-4),G0tensor(:,194))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,10),wf(:,-4),G0tensor(:,195))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-4),wf(:,7),G0tensor(:,196))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-4),wf(:,9),G0tensor(:,197))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-4),wf(:,10),G0tensor(:,198))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-3),wf(:,13),G0tensor(:,199))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-3),wf(:,15),G0tensor(:,200))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-3),wf(:,16),G0tensor(:,201))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,13),wf(:,-3),G0tensor(:,202))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,15),wf(:,-3),G0tensor(:,203))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,16),wf(:,-3),G0tensor(:,204))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-3),wf(:,13),G0tensor(:,205))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-3),wf(:,15),G0tensor(:,206))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-3),wf(:,16),G0tensor(:,207))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-1),wf(:,43),G0tensor(:,208))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-1),wf(:,45),G0tensor(:,209))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-1),wf(:,46),G0tensor(:,210))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,43),wf(:,-1),G0tensor(:,211))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,45),wf(:,-1),G0tensor(:,212))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,46),wf(:,-1),G0tensor(:,213))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-1),wf(:,43),G0tensor(:,214))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-1),wf(:,45),G0tensor(:,215))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-1),wf(:,46),G0tensor(:,216))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,0),wf(:,38),G0tensor(:,217))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,38),wf(:,0),G0tensor(:,218))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,0),wf(:,38),G0tensor(:,219))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,0),wf(:,41),G0tensor(:,220))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,41),wf(:,0),G0tensor(:,221))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,0),wf(:,41),G0tensor(:,222))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,0),wf(:,42),G0tensor(:,223))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,42),wf(:,0),G0tensor(:,224))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,0),wf(:,42),G0tensor(:,225))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,61),wf(:,75),G0tensor(:,226))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,75),wf(:,61),G0tensor(:,227))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,61),wf(:,75),G0tensor(:,228))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-4),wf(:,83),G0tensor(:,229))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,83),wf(:,-4),G0tensor(:,230))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-4),wf(:,83),G0tensor(:,231))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-3),wf(:,88),G0tensor(:,232))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,88),wf(:,-3),G0tensor(:,233))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-3),wf(:,88),G0tensor(:,234))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,71),Q(:,27),G1tensor(:,100))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,72),Q(:,27),G1tensor(:,101))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,73),Q(:,27),G1tensor(:,102))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,95),wf(:,104),G0tensor(:,235))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,104),wf(:,95),G0tensor(:,236))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,95),wf(:,104),G0tensor(:,237))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-4),wf(:,124),G0tensor(:,238))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,124),wf(:,-4),G0tensor(:,239))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-4),wf(:,124),G0tensor(:,240))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,91),wf(:,109),G0tensor(:,241))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,109),wf(:,91),G0tensor(:,242))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,91),wf(:,109),G0tensor(:,243))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-4),wf(:,131),G0tensor(:,244))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,131),wf(:,-4),G0tensor(:,245))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-4),wf(:,131),G0tensor(:,246))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-3),wf(:,135),G0tensor(:,247))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,135),wf(:,-3),G0tensor(:,248))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-3),wf(:,135),G0tensor(:,249))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-3),wf(:,139),G0tensor(:,250))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,139),wf(:,-3),G0tensor(:,251))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-3),wf(:,139),G0tensor(:,252))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-1),wf(:,181),G0tensor(:,253))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,181),wf(:,-1),G0tensor(:,254))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-1),wf(:,181),G0tensor(:,255))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,157),Q(:,27),G1tensor(:,103))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,158),Q(:,27),G1tensor(:,104))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,159),Q(:,27),G1tensor(:,105))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-1),wf(:,186),G0tensor(:,256))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,186),wf(:,-1),G0tensor(:,257))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-1),wf(:,186),G0tensor(:,258))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-1),wf(:,190),G0tensor(:,259))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,190),wf(:,-1),G0tensor(:,260))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-1),wf(:,190),G0tensor(:,261))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,168),Q(:,27),G1tensor(:,106))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,169),Q(:,27),G1tensor(:,107))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,170),Q(:,27),G1tensor(:,108))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,976),Q(:,27),G1tensor(:,109))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,977),Q(:,27),G1tensor(:,110))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,978),Q(:,27),G1tensor(:,111))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,0),wf(:,235),G0tensor(:,262))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,235),wf(:,0),G0tensor(:,263))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,0),wf(:,235),G0tensor(:,264))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,211),Q(:,27),G1tensor(:,112))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,212),Q(:,27),G1tensor(:,113))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,213),Q(:,27),G1tensor(:,114))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,0),wf(:,240),G0tensor(:,265))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,240),wf(:,0),G0tensor(:,266))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,0),wf(:,240),G0tensor(:,267))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,0),wf(:,244),G0tensor(:,268))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,244),wf(:,0),G0tensor(:,269))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,0),wf(:,244),G0tensor(:,270))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,222),Q(:,27),G1tensor(:,115))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,223),Q(:,27),G1tensor(:,116))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,224),Q(:,27),G1tensor(:,117))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1087),Q(:,27),G1tensor(:,118))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1088),Q(:,27),G1tensor(:,119))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1089),Q(:,27),G1tensor(:,120))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1141),Q(:,27),G1tensor(:,121))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1142),Q(:,27),G1tensor(:,122))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1143),Q(:,27),G1tensor(:,123))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1147),Q(:,27),G1tensor(:,124))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1148),Q(:,27),G1tensor(:,125))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1149),Q(:,27),G1tensor(:,126))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1156),Q(:,27),G1tensor(:,127))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1157),Q(:,27),G1tensor(:,128))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1158),Q(:,27),G1tensor(:,129))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1202),Q(:,27),G1tensor(:,130))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1325),Q(:,27),G1tensor(:,131))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1328),Q(:,27),G1tensor(:,132))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,280),Q(:,27),G1tensor(:,133))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1358),Q(:,27),G1tensor(:,134))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1363),Q(:,27),G1tensor(:,135))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,282),Q(:,27),G1tensor(:,136))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1370),Q(:,27),G1tensor(:,137))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1373),Q(:,27),G1tensor(:,138))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1394),Q(:,27),G1tensor(:,139))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1399),Q(:,27),G1tensor(:,140))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1406),Q(:,27),G1tensor(:,141))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1409),Q(:,27),G1tensor(:,142))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1429),Q(:,27),G1tensor(:,143))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,36),wf(:,1430),Q(:,27),G1tensor(:,144))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(2)*(-M(131)+M(132)+M(134)-M(136)+M(140)-M(142)-M(148)+M(154)+M(164)-M(166)-M(172)+M(178)-M(196)+M(202)+M(226) &
    -M(250))) * den(1)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,1)
  Gcoeff = (c(2)*(M(131)-M(132)-M(134)+M(136)-M(155)+M(156)+M(158)-M(160)-M(195)+M(196)+M(201)-M(202)+M(225)-M(226)-M(249) &
    +M(250))) * den(1)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,2)
  Gcoeff = (c(2)*(-M(140)+M(142)+M(148)-M(154)+M(155)-M(156)-M(158)+M(160)-M(164)+M(166)+M(172)-M(178)+M(195)-M(201)-M(225) &
    +M(249))) * den(1)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,3)
  Gcoeff = (c(2)*(M(131)-M(132)-M(134)+M(136)-M(140)+M(142)+M(148)-M(154)-M(164)+M(166)+M(172)-M(178)+M(196)-M(202)-M(226) &
    +M(250))) * den(1)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,4)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(134)-M(136)+M(155)-M(156)-M(158)+M(160)+M(195)-M(196)-M(201)+M(202)-M(225)+M(226)+M(249) &
    -M(250))) * den(1)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,5)
  Gcoeff = (c(2)*(M(140)-M(142)-M(148)+M(154)-M(155)+M(156)+M(158)-M(160)+M(164)-M(166)-M(172)+M(178)-M(195)+M(201)+M(225) &
    -M(249))) * den(1)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,6)
  Gcoeff = (c(3)*(M(131)-M(132)-M(134)+M(136)-M(140)+M(142)+M(148)-M(154)-M(164)+M(166)+M(172)-M(178)+M(196)-M(202)-M(226) &
    +M(250))) * den(1)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,7)
  Gcoeff = (c(3)*(-M(131)+M(132)+M(134)-M(136)+M(155)-M(156)-M(158)+M(160)+M(195)-M(196)-M(201)+M(202)-M(225)+M(226)+M(249) &
    -M(250))) * den(1)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,8)
  Gcoeff = (c(3)*(M(140)-M(142)-M(148)+M(154)-M(155)+M(156)+M(158)-M(160)+M(164)-M(166)-M(172)+M(178)-M(195)+M(201)+M(225) &
    -M(249))) * den(1)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,9)
  Gcoeff = (c(1)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(2)*(M(139)+M(141)-M(144)-M(150) &
    -M(194)-M(200)+M(214)+M(238))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,82)
  Gcoeff = (c(1)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(2)*(-M(139)-M(141)+M(181)+M(183) &
    +M(213)-M(214)+M(237)-M(238))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,85)
  Gcoeff = (c(1)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(2)*(M(144)+M(150)-M(181)-M(183) &
    +M(194)+M(200)-M(213)-M(237))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,88)
  Gcoeff = (c(1)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(2)*(-M(134)+M(137)+M(142)-M(153) &
    +M(172)-M(188)-M(202)+M(248))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,83)
  Gcoeff = (c(1)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(2)*(-M(137)-M(142)+M(171)-M(172) &
    +M(179)+M(184)+M(247)-M(248))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,86)
  Gcoeff = (c(1)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(2)*(M(134)+M(153)-M(171)-M(179) &
    -M(184)+M(188)+M(202)-M(247))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,89)
  Gcoeff = (c(2)*(-M(134)+M(137)-M(139)-M(141)+M(142)+M(144)+M(150)-M(153)+M(172)-M(188)+M(194)+M(200)-M(202)-M(214)-M(238) &
    +M(248))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,84)
  Gcoeff = (c(2)*(-M(137)+M(139)+M(141)-M(142)+M(171)-M(172)+M(179)-M(181)-M(183)+M(184)-M(213)+M(214)-M(237)+M(238)+M(247) &
    -M(248))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,87)
  Gcoeff = (c(2)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,90)
  Gcoeff = (c(1)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(2)*(-M(134)+M(137)+M(142)-M(153) &
    +M(172)-M(188)-M(202)+M(248))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,91)
  Gcoeff = (c(1)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(2)*(-M(137)-M(142)+M(171)-M(172) &
    +M(179)+M(184)+M(247)-M(248))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,94)
  Gcoeff = (c(1)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(2)*(M(134)+M(153)-M(171)-M(179) &
    -M(184)+M(188)+M(202)-M(247))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,97)
  Gcoeff = (c(1)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(2)*(M(139)+M(141)-M(144)-M(150) &
    -M(194)-M(200)+M(214)+M(238))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,92)
  Gcoeff = (c(1)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(2)*(-M(139)-M(141)+M(181)+M(183) &
    +M(213)-M(214)+M(237)-M(238))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,95)
  Gcoeff = (c(1)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(2)*(M(144)+M(150)-M(181)-M(183) &
    +M(194)+M(200)-M(213)-M(237))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,98)
  Gcoeff = (c(2)*(M(134)-M(137)+M(139)+M(141)-M(142)-M(144)-M(150)+M(153)-M(172)+M(188)-M(194)-M(200)+M(202)+M(214)+M(238) &
    -M(248))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,93)
  Gcoeff = (c(2)*(M(137)-M(139)-M(141)+M(142)-M(171)+M(172)-M(179)+M(181)+M(183)-M(184)+M(213)-M(214)+M(237)-M(238)-M(247) &
    +M(248))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,96)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,99)
  Gcoeff = (c(2)*(-M(134)+M(137)-M(139)-M(141)+M(142)+M(144)+M(150)-M(153)+M(172)-M(188)+M(194)+M(200)-M(202)-M(214)-M(238) &
    +M(248))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,100)
  Gcoeff = (c(2)*(-M(137)+M(139)+M(141)-M(142)+M(171)-M(172)+M(179)-M(181)-M(183)+M(184)-M(213)+M(214)-M(237)+M(238)+M(247) &
    -M(248))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,103)
  Gcoeff = (c(2)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,106)
  Gcoeff = (c(2)*(M(134)-M(137)+M(139)+M(141)-M(142)-M(144)-M(150)+M(153)-M(172)+M(188)-M(194)-M(200)+M(202)+M(214)+M(238) &
    -M(248))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,101)
  Gcoeff = (c(2)*(M(137)-M(139)-M(141)+M(142)-M(171)+M(172)-M(179)+M(181)+M(183)-M(184)+M(213)-M(214)+M(237)-M(238)-M(247) &
    +M(248))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,104)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,107)
  Gcoeff = (c(3)*(M(134)-M(137)+M(139)+M(141)-M(142)-M(144)-M(150)+M(153)-M(172)+M(188)-M(194)-M(200)+M(202)+M(214)+M(238) &
    -M(248))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,102)
  Gcoeff = (c(3)*(M(137)-M(139)-M(141)+M(142)-M(171)+M(172)-M(179)+M(181)+M(183)-M(184)+M(213)-M(214)+M(237)-M(238)-M(247) &
    +M(248))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,105)
  Gcoeff = (c(3)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(2)
  T2sum(1:1,4) = T2sum(1:1,4) + Gcoeff * G0tensor(:,108)
  Gcoeff = (c(1)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(2)*(M(139)+M(141)-M(144)-M(150) &
    -M(194)-M(200)+M(214)+M(238))) * den(2)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,109)
  Gcoeff = (c(1)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(2)*(-M(139)-M(141)+M(181)+M(183) &
    +M(213)-M(214)+M(237)-M(238))) * den(2)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,110)
  Gcoeff = (c(1)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(2)*(M(144)+M(150)-M(181)-M(183) &
    +M(194)+M(200)-M(213)-M(237))) * den(2)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,111)
  Gcoeff = (c(1)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(2)*(-M(136)+M(138)+M(140)-M(147) &
    +M(178)-M(190)-M(196)+M(224))) * den(2)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,190)
  Gcoeff = (c(1)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(2)*(-M(138)-M(140)+M(177)-M(178) &
    +M(180)+M(182)+M(223)-M(224))) * den(2)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,191)
  Gcoeff = (c(1)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(2)*(M(136)+M(147)-M(177)-M(180) &
    -M(182)+M(190)+M(196)-M(223))) * den(2)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,192)
  Gcoeff = (c(1)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(2)*(-M(136)+M(138)+M(140)-M(147) &
    +M(178)-M(190)-M(196)+M(224))) * den(2)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,112)
  Gcoeff = (c(1)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(2)*(-M(138)-M(140)+M(177)-M(178) &
    +M(180)+M(182)+M(223)-M(224))) * den(2)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,113)
  Gcoeff = (c(1)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(2)*(M(136)+M(147)-M(177)-M(180) &
    -M(182)+M(190)+M(196)-M(223))) * den(2)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,114)
  Gcoeff = (c(1)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(2)*(M(139)+M(141)-M(144)-M(150) &
    -M(194)-M(200)+M(214)+M(238))) * den(2)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,193)
  Gcoeff = (c(1)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(2)*(-M(139)-M(141)+M(181)+M(183) &
    +M(213)-M(214)+M(237)-M(238))) * den(2)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,194)
  Gcoeff = (c(1)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(2)*(M(144)+M(150)-M(181)-M(183) &
    +M(194)+M(200)-M(213)-M(237))) * den(2)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,195)
  Gcoeff = (c(2)*(-M(136)+M(138)-M(139)+M(140)-M(141)+M(144)-M(147)+M(150)+M(178)-M(190)+M(194)-M(196)+M(200)-M(214)+M(224) &
    -M(238))) * den(2)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,115)
  Gcoeff = (c(2)*(-M(138)+M(139)-M(140)+M(141)+M(177)-M(178)+M(180)-M(181)+M(182)-M(183)-M(213)+M(214)+M(223)-M(224)-M(237) &
    +M(238))) * den(2)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,116)
  Gcoeff = (c(2)*(M(136)-M(144)+M(147)-M(150)-M(177)-M(180)+M(181)-M(182)+M(183)+M(190)-M(194)+M(196)-M(200)+M(213)-M(223) &
    +M(237))) * den(2)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,117)
  Gcoeff = (c(2)*(M(136)-M(138)+M(139)-M(140)+M(141)-M(144)+M(147)-M(150)-M(178)+M(190)-M(194)+M(196)-M(200)+M(214)-M(224) &
    +M(238))) * den(2)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,196)
  Gcoeff = (c(2)*(M(138)-M(139)+M(140)-M(141)-M(177)+M(178)-M(180)+M(181)-M(182)+M(183)+M(213)-M(214)-M(223)+M(224)+M(237) &
    -M(238))) * den(2)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,197)
  Gcoeff = (c(2)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(2)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,198)
  Gcoeff = (c(2)*(M(134)-M(136)-M(137)+M(138)+M(140)-M(142)-M(147)+M(153)-M(172)+M(178)+M(188)-M(190)-M(196)+M(202)+M(224) &
    -M(248))) * den(2)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,10)
  Gcoeff = (c(2)*(M(137)-M(138)-M(140)+M(142)-M(171)+M(172)+M(177)-M(178)-M(179)+M(180)+M(182)-M(184)+M(223)-M(224)-M(247) &
    +M(248))) * den(2)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,11)
  Gcoeff = (c(2)*(-M(134)+M(136)+M(147)-M(153)+M(171)-M(177)+M(179)-M(180)-M(182)+M(184)-M(188)+M(190)+M(196)-M(202)-M(223) &
    +M(247))) * den(2)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,12)
  Gcoeff = (c(2)*(-M(134)+M(136)+M(137)-M(138)-M(140)+M(142)+M(147)-M(153)+M(172)-M(178)-M(188)+M(190)+M(196)-M(202)-M(224) &
    +M(248))) * den(2)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,13)
  Gcoeff = (c(2)*(-M(137)+M(138)+M(140)-M(142)+M(171)-M(172)-M(177)+M(178)+M(179)-M(180)-M(182)+M(184)-M(223)+M(224)+M(247) &
    -M(248))) * den(2)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,14)
  Gcoeff = (c(2)*(M(134)-M(136)-M(147)+M(153)-M(171)+M(177)-M(179)+M(180)+M(182)-M(184)+M(188)-M(190)-M(196)+M(202)+M(223) &
    -M(247))) * den(2)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,15)
  Gcoeff = (c(3)*(-M(134)+M(136)+M(137)-M(138)-M(140)+M(142)+M(147)-M(153)+M(172)-M(178)-M(188)+M(190)+M(196)-M(202)-M(224) &
    +M(248))) * den(2)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,16)
  Gcoeff = (c(3)*(-M(137)+M(138)+M(140)-M(142)+M(171)-M(172)-M(177)+M(178)+M(179)-M(180)-M(182)+M(184)-M(223)+M(224)+M(247) &
    -M(248))) * den(2)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,17)
  Gcoeff = (c(3)*(M(134)-M(136)-M(147)+M(153)-M(171)+M(177)-M(179)+M(180)+M(182)-M(184)+M(188)-M(190)-M(196)+M(202)+M(223) &
    -M(247))) * den(2)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,18)
  Gcoeff = (c(1)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(2)*(-M(138)+M(145)+M(147)-M(149) &
    +M(190)-M(218)-M(224)+M(236))) * den(3)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,118)
  Gcoeff = (c(1)*(-M(47)-M(48)-M(49)+M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98))+c(2)*(-M(145)-M(147)+M(189)-M(190) &
    +M(205)+M(207)+M(235)-M(236))) * den(3)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,119)
  Gcoeff = (c(1)*(M(42)+M(45)+M(52)-M(54)-M(66)-M(77)-M(78)-M(79)+M(80)+M(83)+M(86)-M(91))+c(2)*(M(138)+M(149)-M(189)-M(205) &
    -M(207)+M(218)+M(224)-M(235))) * den(3)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,120)
  Gcoeff = (c(1)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(2)*(-M(135)-M(141)+M(144)+M(146) &
    +M(176)+M(200)-M(214)-M(220))) * den(3)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,199)
  Gcoeff = (c(1)*(-M(47)-M(48)-M(49)+M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98))+c(2)*(-M(144)-M(146)+M(175)-M(176) &
    +M(199)-M(200)+M(204)+M(206))) * den(3)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,200)
  Gcoeff = (c(1)*(M(42)+M(45)+M(52)-M(54)-M(66)-M(77)-M(78)-M(79)+M(80)+M(83)+M(86)-M(91))+c(2)*(M(135)+M(141)-M(175)-M(199) &
    -M(204)-M(206)+M(214)+M(220))) * den(3)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,201)
  Gcoeff = (c(1)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(2)*(-M(135)-M(141)+M(144)+M(146) &
    +M(176)+M(200)-M(214)-M(220))) * den(3)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,121)
  Gcoeff = (c(1)*(-M(47)-M(48)-M(49)+M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98))+c(2)*(-M(144)-M(146)+M(175)-M(176) &
    +M(199)-M(200)+M(204)+M(206))) * den(3)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,122)
  Gcoeff = (c(1)*(M(42)+M(45)+M(52)-M(54)-M(66)-M(77)-M(78)-M(79)+M(80)+M(83)+M(86)-M(91))+c(2)*(M(135)+M(141)-M(175)-M(199) &
    -M(204)-M(206)+M(214)+M(220))) * den(3)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,123)
  Gcoeff = (c(1)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(2)*(-M(138)+M(145)+M(147)-M(149) &
    +M(190)-M(218)-M(224)+M(236))) * den(3)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,202)
  Gcoeff = (c(1)*(-M(47)-M(48)-M(49)+M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98))+c(2)*(-M(145)-M(147)+M(189)-M(190) &
    +M(205)+M(207)+M(235)-M(236))) * den(3)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,203)
  Gcoeff = (c(1)*(M(42)+M(45)+M(52)-M(54)-M(66)-M(77)-M(78)-M(79)+M(80)+M(83)+M(86)-M(91))+c(2)*(M(138)+M(149)-M(189)-M(205) &
    -M(207)+M(218)+M(224)-M(235))) * den(3)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,204)
  Gcoeff = (c(2)*(-M(135)+M(138)-M(141)+M(144)-M(145)+M(146)-M(147)+M(149)+M(176)-M(190)+M(200)-M(214)+M(218)-M(220)+M(224) &
    -M(236))) * den(3)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,124)
  Gcoeff = (c(2)*(-M(144)+M(145)-M(146)+M(147)+M(175)-M(176)-M(189)+M(190)+M(199)-M(200)+M(204)-M(205)+M(206)-M(207)-M(235) &
    +M(236))) * den(3)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,125)
  Gcoeff = (c(2)*(M(135)-M(138)+M(141)-M(149)-M(175)+M(189)-M(199)-M(204)+M(205)-M(206)+M(207)+M(214)-M(218)+M(220)-M(224) &
    +M(235))) * den(3)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,126)
  Gcoeff = (c(2)*(M(135)-M(138)+M(141)-M(144)+M(145)-M(146)+M(147)-M(149)-M(176)+M(190)-M(200)+M(214)-M(218)+M(220)-M(224) &
    +M(236))) * den(3)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,205)
  Gcoeff = (c(2)*(M(144)-M(145)+M(146)-M(147)-M(175)+M(176)+M(189)-M(190)-M(199)+M(200)-M(204)+M(205)-M(206)+M(207)+M(235) &
    -M(236))) * den(3)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,206)
  Gcoeff = (c(2)*(-M(135)+M(138)-M(141)+M(149)+M(175)-M(189)+M(199)+M(204)-M(205)+M(206)-M(207)-M(214)+M(218)-M(220)+M(224) &
    -M(235))) * den(3)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,207)
  Gcoeff = (c(2)*(-M(148)+M(154)+M(158)-M(160)-M(161)+M(162)+M(164)-M(166)-M(171)+M(177)+M(182)-M(184)-M(195)+M(201)+M(222) &
    -M(246))) * den(5)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,19)
  Gcoeff = (c(2)*(-M(147)+M(148)+M(153)-M(154)+M(161)-M(162)-M(164)+M(166)-M(185)+M(186)+M(188)-M(190)+M(221)-M(222)-M(245) &
    +M(246))) * den(5)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,20)
  Gcoeff = (c(2)*(M(147)-M(153)-M(158)+M(160)+M(171)-M(177)-M(182)+M(184)+M(185)-M(186)-M(188)+M(190)+M(195)-M(201)-M(221) &
    +M(245))) * den(5)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,21)
  Gcoeff = (c(2)*(M(148)-M(154)-M(158)+M(160)+M(161)-M(162)-M(164)+M(166)+M(171)-M(177)-M(182)+M(184)+M(195)-M(201)-M(222) &
    +M(246))) * den(5)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,22)
  Gcoeff = (c(2)*(M(147)-M(148)-M(153)+M(154)-M(161)+M(162)+M(164)-M(166)+M(185)-M(186)-M(188)+M(190)-M(221)+M(222)+M(245) &
    -M(246))) * den(5)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,23)
  Gcoeff = (c(2)*(-M(147)+M(153)+M(158)-M(160)-M(171)+M(177)+M(182)-M(184)-M(185)+M(186)+M(188)-M(190)-M(195)+M(201)+M(221) &
    -M(245))) * den(5)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,24)
  Gcoeff = (c(3)*(M(148)-M(154)-M(158)+M(160)+M(161)-M(162)-M(164)+M(166)+M(171)-M(177)-M(182)+M(184)+M(195)-M(201)-M(222) &
    +M(246))) * den(5)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,25)
  Gcoeff = (c(3)*(M(147)-M(148)-M(153)+M(154)-M(161)+M(162)+M(164)-M(166)+M(185)-M(186)-M(188)+M(190)-M(221)+M(222)+M(245) &
    -M(246))) * den(5)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,26)
  Gcoeff = (c(3)*(-M(147)+M(153)+M(158)-M(160)-M(171)+M(177)+M(182)-M(184)-M(185)+M(186)+M(188)-M(190)-M(195)+M(201)+M(221) &
    -M(245))) * den(5)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,27)
  Gcoeff = (c(2)*(M(131)-M(132)-M(155)+M(156)-M(179)+M(180)+M(185)-M(186)-M(221)+M(223)+M(225)-M(226)+M(245)-M(247)-M(249) &
    +M(250))) * den(92)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,28)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(92)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,29)
  Gcoeff = (c(3)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(92)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,30)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(92)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,31)
  Gcoeff = (c(2)*(M(131)-M(132)-M(137)+M(138)-M(161)+M(162)+M(185)-M(186)-M(221)+M(222)+M(224)-M(226)+M(245)-M(246)-M(248) &
    +M(250))) * den(92)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,32)
  Gcoeff = (c(3)*(M(131)-M(132)-M(137)+M(138)-M(161)+M(162)+M(185)-M(186)-M(221)+M(222)+M(224)-M(226)+M(245)-M(246)-M(248) &
    +M(250))) * den(92)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,33)
  Gcoeff = (c(2)*(-M(137)+M(138)+M(155)-M(156)-M(161)+M(162)+M(179)-M(180)+M(222)-M(223)+M(224)-M(225)-M(246)+M(247)-M(248) &
    +M(249))) * den(92)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,34)
  Gcoeff = (c(2)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(92)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,35)
  Gcoeff = (c(3)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(92)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,36)
  Gcoeff = (c(1)*(M(48)+M(60)-M(66)-M(69)+M(71)+M(72)+M(73)-M(76)-M(78)-M(81)-M(88)+M(100))+c(2)*(M(136)-M(180)+M(191)+M(196) &
    -M(199)-M(204)-M(223)+M(234))) * den(8)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,127)
  Gcoeff = (c(1)*(M(45)-M(48)+M(57)-M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100))+c(2)*(M(135)-M(136)-M(191)-M(196) &
    +M(215)+M(220)+M(233)-M(234))) * den(8)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,128)
  Gcoeff = (c(1)*(-M(45)-M(57)+M(66)+M(69)+M(76)+M(78)+M(81)-M(83)-M(84)-M(85)+M(88)-M(97))+c(2)*(-M(135)+M(180)+M(199)+M(204) &
    -M(215)-M(220)+M(223)-M(233))) * den(8)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,129)
  Gcoeff = (c(1)*(M(48)+M(60)-M(66)-M(69)+M(71)+M(72)+M(73)-M(76)-M(78)-M(81)-M(88)+M(100))+c(2)*(M(150)+M(174)-M(183)-M(189) &
    +M(192)+M(194)-M(207)-M(213))) * den(8)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,130)
  Gcoeff = (c(1)*(M(45)-M(48)+M(57)-M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100))+c(2)*(M(149)-M(150)+M(173)-M(174) &
    -M(192)-M(194)+M(216)+M(218))) * den(8)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,131)
  Gcoeff = (c(1)*(-M(45)-M(57)+M(66)+M(69)+M(76)+M(78)+M(81)-M(83)-M(84)-M(85)+M(88)-M(97))+c(2)*(-M(149)-M(173)+M(183)+M(189) &
    +M(207)+M(213)-M(216)-M(218))) * den(8)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,132)
  Gcoeff = (c(2)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(8)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,133)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(8)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,134)
  Gcoeff = (c(2)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(8)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,135)
  Gcoeff = (c(1)*(M(48)+M(60)-M(66)-M(69)+M(71)+M(72)+M(73)-M(76)-M(78)-M(81)-M(88)+M(100))+c(2)*(M(150)+M(174)-M(183)-M(189) &
    +M(192)+M(194)-M(207)-M(213))) * den(8)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,208)
  Gcoeff = (c(1)*(M(45)-M(48)+M(57)-M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100))+c(2)*(M(149)-M(150)+M(173)-M(174) &
    -M(192)-M(194)+M(216)+M(218))) * den(8)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,209)
  Gcoeff = (c(1)*(-M(45)-M(57)+M(66)+M(69)+M(76)+M(78)+M(81)-M(83)-M(84)-M(85)+M(88)-M(97))+c(2)*(-M(149)-M(173)+M(183)+M(189) &
    +M(207)+M(213)-M(216)-M(218))) * den(8)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,210)
  Gcoeff = (c(1)*(M(48)+M(60)-M(66)-M(69)+M(71)+M(72)+M(73)-M(76)-M(78)-M(81)-M(88)+M(100))+c(2)*(M(136)-M(180)+M(191)+M(196) &
    -M(199)-M(204)-M(223)+M(234))) * den(8)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,211)
  Gcoeff = (c(1)*(M(45)-M(48)+M(57)-M(60)-M(71)-M(72)-M(73)+M(83)+M(84)+M(85)+M(97)-M(100))+c(2)*(M(135)-M(136)-M(191)-M(196) &
    +M(215)+M(220)+M(233)-M(234))) * den(8)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,212)
  Gcoeff = (c(1)*(-M(45)-M(57)+M(66)+M(69)+M(76)+M(78)+M(81)-M(83)-M(84)-M(85)+M(88)-M(97))+c(2)*(-M(135)+M(180)+M(199)+M(204) &
    -M(215)-M(220)+M(223)-M(233))) * den(8)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,213)
  Gcoeff = (c(2)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(8)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,214)
  Gcoeff = (c(2)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(8)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,215)
  Gcoeff = (c(2)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(8)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,216)
  Gcoeff = (c(1)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(2)*(M(140)+M(173)-M(175) &
    -M(177)+M(178)-M(182)-M(206)+M(216))) * den(67)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,136)
  Gcoeff = (c(1)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(2)*(M(139)-M(181)-M(205) &
    +M(215)+M(233)-M(235)-M(237)+M(238))) * den(67)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,137)
  Gcoeff = (c(2)*(M(139)-M(140)-M(173)+M(175)+M(177)-M(178)-M(181)+M(182)-M(205)+M(206)+M(215)-M(216)+M(233)-M(235)-M(237) &
    +M(238))) * den(67)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,138)
  Gcoeff = (c(1)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(2)*(-M(140)+M(146)-M(173) &
    +M(174)+M(176)-M(178)+M(192)-M(216))) * den(67)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,139)
  Gcoeff = (c(1)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(2)*(-M(139)+M(145)+M(191) &
    -M(215)-M(233)+M(234)+M(236)-M(238))) * den(67)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,140)
  Gcoeff = (c(2)*(-M(139)+M(140)+M(145)-M(146)+M(173)-M(174)-M(176)+M(178)+M(191)-M(192)-M(215)+M(216)-M(233)+M(234)+M(236) &
    -M(238))) * den(67)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,141)
  Gcoeff = (c(1)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(2)*(-M(146)-M(174)+M(175) &
    -M(176)+M(177)+M(182)-M(192)+M(206))) * den(67)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,142)
  Gcoeff = (c(1)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(2)*(-M(145)+M(181)-M(191) &
    +M(205)-M(234)+M(235)-M(236)+M(237))) * den(67)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,143)
  Gcoeff = (c(2)*(-M(145)+M(146)+M(174)-M(175)+M(176)-M(177)+M(181)-M(182)-M(191)+M(192)+M(205)-M(206)-M(234)+M(235)-M(236) &
    +M(237))) * den(67)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,144)
  Gcoeff = (c(1)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(2)*(M(139)-M(181)-M(205) &
    +M(215)+M(233)-M(235)-M(237)+M(238))) * den(67)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,217)
  Gcoeff = (c(1)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(2)*(M(140)+M(173)-M(175) &
    -M(177)+M(178)-M(182)-M(206)+M(216))) * den(67)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,218)
  Gcoeff = (c(2)*(-M(139)+M(140)+M(173)-M(175)-M(177)+M(178)+M(181)-M(182)+M(205)-M(206)-M(215)+M(216)-M(233)+M(235)+M(237) &
    -M(238))) * den(67)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,219)
  Gcoeff = (c(1)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(2)*(-M(139)+M(145)+M(191) &
    -M(215)-M(233)+M(234)+M(236)-M(238))) * den(67)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,220)
  Gcoeff = (c(1)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(2)*(-M(140)+M(146)-M(173) &
    +M(174)+M(176)-M(178)+M(192)-M(216))) * den(67)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,221)
  Gcoeff = (c(2)*(M(139)-M(140)-M(145)+M(146)-M(173)+M(174)+M(176)-M(178)-M(191)+M(192)+M(215)-M(216)+M(233)-M(234)-M(236) &
    +M(238))) * den(67)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,222)
  Gcoeff = (c(1)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(2)*(-M(145)+M(181)-M(191) &
    +M(205)-M(234)+M(235)-M(236)+M(237))) * den(67)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,223)
  Gcoeff = (c(1)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(2)*(-M(146)-M(174)+M(175) &
    -M(176)+M(177)+M(182)-M(192)+M(206))) * den(67)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,224)
  Gcoeff = (c(2)*(M(145)-M(146)-M(174)+M(175)-M(176)+M(177)-M(181)+M(182)+M(191)-M(192)-M(205)+M(206)+M(234)-M(235)+M(236) &
    -M(237))) * den(67)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,225)
  Gcoeff = (c(2)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(13)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,37)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(13)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,38)
  Gcoeff = (c(3)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(13)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,39)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(134)-M(136)+M(140)-M(142)-M(148)+M(154)+M(164)-M(166)-M(172)+M(178)-M(196)+M(202)+M(226) &
    -M(250))) * den(152)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,40)
  Gcoeff = (c(2)*(M(131)-M(132)-M(134)+M(136)-M(140)+M(142)+M(148)-M(154)-M(164)+M(166)+M(172)-M(178)+M(196)-M(202)-M(226) &
    +M(250))) * den(152)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,41)
  Gcoeff = (c(3)*(M(131)-M(132)-M(134)+M(136)-M(140)+M(142)+M(148)-M(154)-M(164)+M(166)+M(172)-M(178)+M(196)-M(202)-M(226) &
    +M(250))) * den(152)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,42)
  Gcoeff = (c(1)*(M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(71)+M(83))+c(2)*(M(135)-M(136)+M(140)-M(146) &
    -M(176)+M(178)-M(196)+M(220))) * den(21)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,145)
  Gcoeff = (c(1)*(M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(71)+M(83))+c(2)*(M(139)-M(145)+M(149)-M(150) &
    -M(194)+M(218)-M(236)+M(238))) * den(21)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,226)
  Gcoeff = (c(1)*(M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(71)+M(83))+c(2)*(M(139)-M(145)+M(149)-M(150) &
    -M(194)+M(218)-M(236)+M(238))) * den(21)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,146)
  Gcoeff = (c(1)*(M(25)-M(26)-M(29)+M(30)+M(37)-M(38)-M(39)+M(40)+M(45)-M(48)-M(71)+M(83))+c(2)*(M(135)-M(136)+M(140)-M(146) &
    -M(176)+M(178)-M(196)+M(220))) * den(21)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,227)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(139)-M(140)-M(145)+M(146)+M(149)-M(150)+M(176)-M(178)-M(194)+M(196)+M(218)-M(220)-M(236) &
    +M(238))) * den(21)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,147)
  Gcoeff = (c(2)*(M(135)-M(136)-M(139)+M(140)+M(145)-M(146)-M(149)+M(150)-M(176)+M(178)+M(194)-M(196)-M(218)+M(220)+M(236) &
    -M(238))) * den(21)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,228)
  Gcoeff = (c(1)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(2)*(M(139)+M(141)-M(144)-M(150) &
    -M(194)-M(200)+M(214)+M(238))) * den(147)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,148)
  Gcoeff = (c(1)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(2)*(-M(136)+M(138)+M(140)-M(147) &
    +M(178)-M(190)-M(196)+M(224))) * den(147)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,229)
  Gcoeff = (c(1)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(2)*(-M(136)+M(138)+M(140)-M(147) &
    +M(178)-M(190)-M(196)+M(224))) * den(147)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,149)
  Gcoeff = (c(1)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(2)*(M(139)+M(141)-M(144)-M(150) &
    -M(194)-M(200)+M(214)+M(238))) * den(147)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,230)
  Gcoeff = (c(2)*(-M(136)+M(138)-M(139)+M(140)-M(141)+M(144)-M(147)+M(150)+M(178)-M(190)+M(194)-M(196)+M(200)-M(214)+M(224) &
    -M(238))) * den(147)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,150)
  Gcoeff = (c(2)*(M(136)-M(138)+M(139)-M(140)+M(141)-M(144)+M(147)-M(150)-M(178)+M(190)-M(194)+M(196)-M(200)+M(214)-M(224) &
    +M(238))) * den(147)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,231)
  Gcoeff = (c(2)*(M(134)-M(136)-M(137)+M(138)+M(140)-M(142)-M(147)+M(153)-M(172)+M(178)+M(188)-M(190)-M(196)+M(202)+M(224) &
    -M(248))) * den(147)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,43)
  Gcoeff = (c(2)*(-M(134)+M(136)+M(137)-M(138)-M(140)+M(142)+M(147)-M(153)+M(172)-M(178)-M(188)+M(190)+M(196)-M(202)-M(224) &
    +M(248))) * den(147)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,44)
  Gcoeff = (c(3)*(-M(134)+M(136)+M(137)-M(138)-M(140)+M(142)+M(147)-M(153)+M(172)-M(178)-M(188)+M(190)+M(196)-M(202)-M(224) &
    +M(248))) * den(147)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,45)
  Gcoeff = (c(3)*(M(131)-M(132)-M(134)+M(136)-M(140)+M(142)+M(148)-M(154)-M(164)+M(166)+M(172)-M(178)+M(196)-M(202)-M(226) &
    +M(250))) * den(346)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(3)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(346)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,2)
  Gcoeff = (c(3)*(M(134)-M(136)-M(137)+M(138)+M(140)-M(142)-M(147)+M(153)-M(172)+M(178)+M(188)-M(190)-M(196)+M(202)+M(224) &
    -M(248))) * den(346)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,3)
  Gcoeff = (c(1)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(2)*(-M(138)+M(145)+M(147)-M(149) &
    +M(190)-M(218)-M(224)+M(236))) * den(142)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,151)
  Gcoeff = (c(1)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(2)*(-M(135)-M(141)+M(144)+M(146) &
    +M(176)+M(200)-M(214)-M(220))) * den(142)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,232)
  Gcoeff = (c(1)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(2)*(-M(135)-M(141)+M(144)+M(146) &
    +M(176)+M(200)-M(214)-M(220))) * den(142)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,152)
  Gcoeff = (c(1)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(2)*(-M(138)+M(145)+M(147)-M(149) &
    +M(190)-M(218)-M(224)+M(236))) * den(142)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,233)
  Gcoeff = (c(2)*(-M(135)+M(138)-M(141)+M(144)-M(145)+M(146)-M(147)+M(149)+M(176)-M(190)+M(200)-M(214)+M(218)-M(220)+M(224) &
    -M(236))) * den(142)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,153)
  Gcoeff = (c(2)*(M(135)-M(138)+M(141)-M(144)+M(145)-M(146)+M(147)-M(149)-M(176)+M(190)-M(200)+M(214)-M(218)+M(220)-M(224) &
    +M(236))) * den(142)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,234)
  Gcoeff = (c(2)*(-M(136)+M(138)-M(139)+M(140)-M(141)+M(144)-M(147)+M(150)+M(178)-M(190)+M(194)-M(196)+M(200)-M(214)+M(224) &
    -M(238))) * den(319)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,55)
  Gcoeff = (c(2)*(M(136)-M(138)+M(139)-M(140)+M(141)-M(144)+M(147)-M(150)-M(178)+M(190)-M(194)+M(196)-M(200)+M(214)-M(224) &
    +M(238))) * den(319)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,100)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(139)-M(140)-M(145)+M(146)+M(149)-M(150)+M(176)-M(178)-M(194)+M(196)+M(218)-M(220)-M(236) &
    +M(238))) * den(319)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,56)
  Gcoeff = (c(2)*(M(135)-M(136)-M(139)+M(140)+M(145)-M(146)-M(149)+M(150)-M(176)+M(178)+M(194)-M(196)-M(218)+M(220)+M(236) &
    -M(238))) * den(319)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,101)
  Gcoeff = (c(2)*(M(135)-M(138)+M(141)-M(144)+M(145)-M(146)+M(147)-M(149)-M(176)+M(190)-M(200)+M(214)-M(218)+M(220)-M(224) &
    +M(236))) * den(319)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,57)
  Gcoeff = (c(2)*(-M(135)+M(138)-M(141)+M(144)-M(145)+M(146)-M(147)+M(149)+M(176)-M(190)+M(200)-M(214)+M(218)-M(220)+M(224) &
    -M(236))) * den(319)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,102)
  Gcoeff = (c(2)*(-M(140)+M(142)+M(155)-M(156)-M(161)+M(162)-M(171)+M(172)+M(177)-M(178)+M(182)-M(184)+M(222)-M(225)-M(246) &
    +M(249))) * den(34)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,46)
  Gcoeff = (c(2)*(M(140)-M(142)-M(155)+M(156)+M(161)-M(162)+M(171)-M(172)-M(177)+M(178)-M(182)+M(184)-M(222)+M(225)+M(246) &
    -M(249))) * den(34)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,47)
  Gcoeff = (c(3)*(M(140)-M(142)-M(155)+M(156)+M(161)-M(162)+M(171)-M(172)-M(177)+M(178)-M(182)+M(184)-M(222)+M(225)+M(246) &
    -M(249))) * den(34)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,48)
  Gcoeff = (c(2)*(-M(140)+M(142)+M(148)-M(154)+M(155)-M(156)-M(158)+M(160)-M(164)+M(166)+M(172)-M(178)+M(195)-M(201)-M(225) &
    +M(249))) * den(176)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,49)
  Gcoeff = (c(2)*(M(140)-M(142)-M(148)+M(154)-M(155)+M(156)+M(158)-M(160)+M(164)-M(166)-M(172)+M(178)-M(195)+M(201)+M(225) &
    -M(249))) * den(176)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,50)
  Gcoeff = (c(3)*(M(140)-M(142)-M(148)+M(154)-M(155)+M(156)+M(158)-M(160)+M(164)-M(166)-M(172)+M(178)-M(195)+M(201)+M(225) &
    -M(249))) * den(176)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,51)
  Gcoeff = (c(2)*(-M(134)+M(136)+M(158)-M(160)+M(179)-M(180)-M(185)+M(186)-M(195)+M(196)+M(201)-M(202)+M(221)-M(223)-M(245) &
    +M(247))) * den(42)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,52)
  Gcoeff = (c(2)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(42)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,53)
  Gcoeff = (c(3)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(42)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,54)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(134)-M(136)+M(155)-M(156)-M(158)+M(160)+M(195)-M(196)-M(201)+M(202)-M(225)+M(226)+M(249) &
    -M(250))) * den(200)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,55)
  Gcoeff = (c(2)*(M(131)-M(132)-M(134)+M(136)-M(155)+M(156)+M(158)-M(160)-M(195)+M(196)+M(201)-M(202)+M(225)-M(226)-M(249) &
    +M(250))) * den(200)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,56)
  Gcoeff = (c(3)*(M(131)-M(132)-M(134)+M(136)-M(155)+M(156)+M(158)-M(160)-M(195)+M(196)+M(201)-M(202)+M(225)-M(226)-M(249) &
    +M(250))) * den(200)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,57)
  Gcoeff = (c(1)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(2)*(-M(144)+M(181)-M(191)+M(199) &
    -M(200)+M(204)-M(234)+M(237))) * den(48)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,154)
  Gcoeff = (c(1)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(2)*(-M(147)-M(174)+M(177)+M(182) &
    +M(189)-M(190)-M(192)+M(207))) * den(48)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,235)
  Gcoeff = (c(1)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(2)*(-M(147)-M(174)+M(177)+M(182) &
    +M(189)-M(190)-M(192)+M(207))) * den(48)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,155)
  Gcoeff = (c(1)*(-M(3)+M(4)+M(13)-M(14)-M(17)+M(18)+M(23)-M(24)-M(48)+M(66)-M(71)+M(78))+c(2)*(-M(144)+M(181)-M(191)+M(199) &
    -M(200)+M(204)-M(234)+M(237))) * den(48)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,236)
  Gcoeff = (c(2)*(M(144)-M(147)-M(174)+M(177)-M(181)+M(182)+M(189)-M(190)+M(191)-M(192)-M(199)+M(200)-M(204)+M(207)+M(234) &
    -M(237))) * den(48)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,156)
  Gcoeff = (c(2)*(-M(144)+M(147)+M(174)-M(177)+M(181)-M(182)-M(189)+M(190)-M(191)+M(192)+M(199)-M(200)+M(204)-M(207)-M(234) &
    +M(237))) * den(48)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,237)
  Gcoeff = (c(1)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(2)*(M(144)+M(150)-M(181)-M(183) &
    +M(194)+M(200)-M(213)-M(237))) * den(219)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,157)
  Gcoeff = (c(1)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(2)*(M(136)+M(147)-M(177)-M(180) &
    -M(182)+M(190)+M(196)-M(223))) * den(219)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,238)
  Gcoeff = (c(1)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(2)*(M(136)+M(147)-M(177)-M(180) &
    -M(182)+M(190)+M(196)-M(223))) * den(219)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,158)
  Gcoeff = (c(1)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(2)*(M(144)+M(150)-M(181)-M(183) &
    +M(194)+M(200)-M(213)-M(237))) * den(219)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,239)
  Gcoeff = (c(2)*(M(136)-M(144)+M(147)-M(150)-M(177)-M(180)+M(181)-M(182)+M(183)+M(190)-M(194)+M(196)-M(200)+M(213)-M(223) &
    +M(237))) * den(219)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,159)
  Gcoeff = (c(2)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(219)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,240)
  Gcoeff = (c(2)*(-M(134)+M(136)+M(147)-M(153)+M(171)-M(177)+M(179)-M(180)-M(182)+M(184)-M(188)+M(190)+M(196)-M(202)-M(223) &
    +M(247))) * den(219)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,58)
  Gcoeff = (c(2)*(M(134)-M(136)-M(147)+M(153)-M(171)+M(177)-M(179)+M(180)+M(182)-M(184)+M(188)-M(190)-M(196)+M(202)+M(223) &
    -M(247))) * den(219)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,59)
  Gcoeff = (c(3)*(M(134)-M(136)-M(147)+M(153)-M(171)+M(177)-M(179)+M(180)+M(182)-M(184)+M(188)-M(190)-M(196)+M(202)+M(223) &
    -M(247))) * den(219)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,60)
  Gcoeff = (c(1)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(2)*(-M(138)+M(180)+M(205)-M(215)+M(223) &
    -M(224)-M(233)+M(235))) * den(51)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,160)
  Gcoeff = (c(1)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(2)*(-M(141)-M(173)+M(175)+M(183)+M(206) &
    +M(213)-M(214)-M(216))) * den(51)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,241)
  Gcoeff = (c(1)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(2)*(-M(141)-M(173)+M(175)+M(183)+M(206) &
    +M(213)-M(214)-M(216))) * den(51)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,161)
  Gcoeff = (c(1)*(-M(1)+M(2)+M(7)-M(8)-M(11)+M(12)+M(21)-M(22)-M(45)+M(66)+M(78)-M(83))+c(2)*(-M(138)+M(180)+M(205)-M(215)+M(223) &
    -M(224)-M(233)+M(235))) * den(51)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,242)
  Gcoeff = (c(2)*(M(138)-M(141)-M(173)+M(175)-M(180)+M(183)-M(205)+M(206)+M(213)-M(214)+M(215)-M(216)-M(223)+M(224)+M(233) &
    -M(235))) * den(51)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,162)
  Gcoeff = (c(2)*(-M(138)+M(141)+M(173)-M(175)+M(180)-M(183)+M(205)-M(206)-M(213)+M(214)-M(215)+M(216)+M(223)-M(224)-M(233) &
    +M(235))) * den(51)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,243)
  Gcoeff = (c(1)*(M(44)+M(45)+M(46)-M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95))+c(2)*(M(139)+M(141)-M(181)-M(183) &
    -M(213)+M(214)-M(237)+M(238))) * den(237)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,163)
  Gcoeff = (c(1)*(M(44)+M(45)+M(46)-M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95))+c(2)*(M(138)+M(140)-M(177)+M(178) &
    -M(180)-M(182)-M(223)+M(224))) * den(237)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,244)
  Gcoeff = (c(1)*(M(44)+M(45)+M(46)-M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95))+c(2)*(M(138)+M(140)-M(177)+M(178) &
    -M(180)-M(182)-M(223)+M(224))) * den(237)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,164)
  Gcoeff = (c(1)*(M(44)+M(45)+M(46)-M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95))+c(2)*(M(139)+M(141)-M(181)-M(183) &
    -M(213)+M(214)-M(237)+M(238))) * den(237)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,245)
  Gcoeff = (c(2)*(M(138)-M(139)+M(140)-M(141)-M(177)+M(178)-M(180)+M(181)-M(182)+M(183)+M(213)-M(214)-M(223)+M(224)+M(237) &
    -M(238))) * den(237)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,165)
  Gcoeff = (c(2)*(-M(138)+M(139)-M(140)+M(141)+M(177)-M(178)+M(180)-M(181)+M(182)-M(183)-M(213)+M(214)+M(223)-M(224)-M(237) &
    +M(238))) * den(237)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,246)
  Gcoeff = (c(2)*(-M(137)+M(138)+M(140)-M(142)+M(171)-M(172)-M(177)+M(178)+M(179)-M(180)-M(182)+M(184)-M(223)+M(224)+M(247) &
    -M(248))) * den(237)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,61)
  Gcoeff = (c(2)*(M(137)-M(138)-M(140)+M(142)-M(171)+M(172)+M(177)-M(178)-M(179)+M(180)+M(182)-M(184)+M(223)-M(224)-M(247) &
    +M(248))) * den(237)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,62)
  Gcoeff = (c(3)*(M(137)-M(138)-M(140)+M(142)-M(171)+M(172)+M(177)-M(178)-M(179)+M(180)+M(182)-M(184)+M(223)-M(224)-M(247) &
    +M(248))) * den(237)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,63)
  Gcoeff = (c(1)*(M(42)+M(45)+M(52)-M(54)-M(66)-M(77)-M(78)-M(79)+M(80)+M(83)+M(86)-M(91))+c(2)*(M(138)+M(149)-M(189)-M(205) &
    -M(207)+M(218)+M(224)-M(235))) * den(250)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,166)
  Gcoeff = (c(1)*(M(42)+M(45)+M(52)-M(54)-M(66)-M(77)-M(78)-M(79)+M(80)+M(83)+M(86)-M(91))+c(2)*(M(135)+M(141)-M(175)-M(199) &
    -M(204)-M(206)+M(214)+M(220))) * den(250)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,247)
  Gcoeff = (c(1)*(M(42)+M(45)+M(52)-M(54)-M(66)-M(77)-M(78)-M(79)+M(80)+M(83)+M(86)-M(91))+c(2)*(M(135)+M(141)-M(175)-M(199) &
    -M(204)-M(206)+M(214)+M(220))) * den(250)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,167)
  Gcoeff = (c(1)*(M(42)+M(45)+M(52)-M(54)-M(66)-M(77)-M(78)-M(79)+M(80)+M(83)+M(86)-M(91))+c(2)*(M(138)+M(149)-M(189)-M(205) &
    -M(207)+M(218)+M(224)-M(235))) * den(250)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,248)
  Gcoeff = (c(2)*(M(135)-M(138)+M(141)-M(149)-M(175)+M(189)-M(199)-M(204)+M(205)-M(206)+M(207)+M(214)-M(218)+M(220)-M(224) &
    +M(235))) * den(250)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,168)
  Gcoeff = (c(2)*(-M(135)+M(138)-M(141)+M(149)+M(175)-M(189)+M(199)+M(204)-M(205)+M(206)-M(207)-M(214)+M(218)-M(220)+M(224) &
    -M(235))) * den(250)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,249)
  Gcoeff = (c(1)*(M(47)+M(48)+M(49)-M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98))+c(2)*(M(145)+M(147)-M(189)+M(190) &
    -M(205)-M(207)-M(235)+M(236))) * den(255)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,169)
  Gcoeff = (c(1)*(M(47)+M(48)+M(49)-M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98))+c(2)*(M(144)+M(146)-M(175)+M(176) &
    -M(199)+M(200)-M(204)-M(206))) * den(255)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,250)
  Gcoeff = (c(1)*(M(47)+M(48)+M(49)-M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98))+c(2)*(M(144)+M(146)-M(175)+M(176) &
    -M(199)+M(200)-M(204)-M(206))) * den(255)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,170)
  Gcoeff = (c(1)*(M(47)+M(48)+M(49)-M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98))+c(2)*(M(145)+M(147)-M(189)+M(190) &
    -M(205)-M(207)-M(235)+M(236))) * den(255)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,251)
  Gcoeff = (c(2)*(M(144)-M(145)+M(146)-M(147)-M(175)+M(176)+M(189)-M(190)-M(199)+M(200)-M(204)+M(205)-M(206)+M(207)+M(235) &
    -M(236))) * den(255)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,171)
  Gcoeff = (c(2)*(-M(144)+M(145)-M(146)+M(147)+M(175)-M(176)-M(189)+M(190)+M(199)-M(200)+M(204)-M(205)+M(206)-M(207)-M(235) &
    +M(236))) * den(255)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,252)
  Gcoeff = (c(2)*(-M(148)+M(154)+M(158)-M(160)-M(161)+M(162)+M(164)-M(166)-M(171)+M(177)+M(182)-M(184)-M(195)+M(201)+M(222) &
    -M(246))) * den(171)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,64)
  Gcoeff = (c(2)*(M(148)-M(154)-M(158)+M(160)+M(161)-M(162)-M(164)+M(166)+M(171)-M(177)-M(182)+M(184)+M(195)-M(201)-M(222) &
    +M(246))) * den(171)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,65)
  Gcoeff = (c(3)*(M(148)-M(154)-M(158)+M(160)+M(161)-M(162)-M(164)+M(166)+M(171)-M(177)-M(182)+M(184)+M(195)-M(201)-M(222) &
    +M(246))) * den(171)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,66)
  Gcoeff = (c(3)*(M(140)-M(142)-M(148)+M(154)-M(155)+M(156)+M(158)-M(160)+M(164)-M(166)-M(172)+M(178)-M(195)+M(201)+M(225) &
    -M(249))) * den(488)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,4)
  Gcoeff = (c(3)*(M(148)-M(154)-M(158)+M(160)+M(161)-M(162)-M(164)+M(166)+M(171)-M(177)-M(182)+M(184)+M(195)-M(201)-M(222) &
    +M(246))) * den(488)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,5)
  Gcoeff = (c(3)*(-M(140)+M(142)+M(155)-M(156)-M(161)+M(162)-M(171)+M(172)+M(177)-M(178)+M(182)-M(184)+M(222)-M(225)-M(246) &
    +M(249))) * den(488)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,6)
  Gcoeff = (c(2)*(M(147)-M(153)-M(158)+M(160)+M(171)-M(177)-M(182)+M(184)+M(185)-M(186)-M(188)+M(190)+M(195)-M(201)-M(221) &
    +M(245))) * den(214)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,67)
  Gcoeff = (c(2)*(-M(147)+M(153)+M(158)-M(160)-M(171)+M(177)+M(182)-M(184)-M(185)+M(186)+M(188)-M(190)-M(195)+M(201)+M(221) &
    -M(245))) * den(214)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,68)
  Gcoeff = (c(3)*(-M(147)+M(153)+M(158)-M(160)-M(171)+M(177)+M(182)-M(184)-M(185)+M(186)+M(188)-M(190)-M(195)+M(201)+M(221) &
    -M(245))) * den(214)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,69)
  Gcoeff = (c(2)*(M(147)-M(148)-M(153)+M(154)-M(161)+M(162)+M(164)-M(166)+M(185)-M(186)-M(188)+M(190)-M(221)+M(222)+M(245) &
    -M(246))) * den(257)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,70)
  Gcoeff = (c(2)*(-M(147)+M(148)+M(153)-M(154)+M(161)-M(162)-M(164)+M(166)-M(185)+M(186)+M(188)-M(190)+M(221)-M(222)-M(245) &
    +M(246))) * den(257)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,71)
  Gcoeff = (c(3)*(-M(147)+M(148)+M(153)-M(154)+M(161)-M(162)-M(164)+M(166)-M(185)+M(186)+M(188)-M(190)+M(221)-M(222)-M(245) &
    +M(246))) * den(257)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,72)
  Gcoeff = (c(3)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(540)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,7)
  Gcoeff = (c(3)*(-M(147)+M(153)+M(158)-M(160)-M(171)+M(177)+M(182)-M(184)-M(185)+M(186)+M(188)-M(190)-M(195)+M(201)+M(221) &
    -M(245))) * den(540)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,8)
  Gcoeff = (c(3)*(-M(134)+M(136)+M(147)-M(153)+M(171)-M(177)+M(179)-M(180)-M(182)+M(184)-M(188)+M(190)+M(196)-M(202)-M(223) &
    +M(247))) * den(540)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,9)
  Gcoeff = (c(3)*(M(131)-M(132)-M(155)+M(156)-M(179)+M(180)+M(185)-M(186)-M(221)+M(223)+M(225)-M(226)+M(245)-M(247)-M(249) &
    +M(250))) * den(574)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,10)
  Gcoeff = (c(3)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(574)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,11)
  Gcoeff = (c(3)*(-M(137)+M(138)+M(155)-M(156)-M(161)+M(162)+M(179)-M(180)+M(222)-M(223)+M(224)-M(225)-M(246)+M(247)-M(248) &
    +M(249))) * den(574)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,12)
  Gcoeff = (c(1)*(M(48)+M(60)-M(66)-M(69)+M(71)+M(72)+M(73)-M(76)-M(78)-M(81)-M(88)+M(100))+c(2)*(M(136)-M(180)+M(191)+M(196) &
    -M(199)-M(204)-M(223)+M(234))) * den(190)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,172)
  Gcoeff = (c(1)*(M(48)+M(60)-M(66)-M(69)+M(71)+M(72)+M(73)-M(76)-M(78)-M(81)-M(88)+M(100))+c(2)*(M(150)+M(174)-M(183)-M(189) &
    +M(192)+M(194)-M(207)-M(213))) * den(190)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,173)
  Gcoeff = (c(2)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(190)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,174)
  Gcoeff = (c(1)*(M(48)+M(60)-M(66)-M(69)+M(71)+M(72)+M(73)-M(76)-M(78)-M(81)-M(88)+M(100))+c(2)*(M(150)+M(174)-M(183)-M(189) &
    +M(192)+M(194)-M(207)-M(213))) * den(190)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,253)
  Gcoeff = (c(1)*(M(48)+M(60)-M(66)-M(69)+M(71)+M(72)+M(73)-M(76)-M(78)-M(81)-M(88)+M(100))+c(2)*(M(136)-M(180)+M(191)+M(196) &
    -M(199)-M(204)-M(223)+M(234))) * den(190)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,254)
  Gcoeff = (c(2)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(190)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,255)
  Gcoeff = (c(2)*(M(136)-M(144)+M(147)-M(150)-M(177)-M(180)+M(181)-M(182)+M(183)+M(190)-M(194)+M(196)-M(200)+M(213)-M(223) &
    +M(237))) * den(502)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,58)
  Gcoeff = (c(2)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(502)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,59)
  Gcoeff = (c(2)*(M(144)-M(147)-M(174)+M(177)-M(181)+M(182)+M(189)-M(190)+M(191)-M(192)-M(199)+M(200)-M(204)+M(207)+M(234) &
    -M(237))) * den(502)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,60)
  Gcoeff = (c(2)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(502)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,103)
  Gcoeff = (c(2)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(502)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,104)
  Gcoeff = (c(2)*(-M(144)+M(147)+M(174)-M(177)+M(181)-M(182)-M(189)+M(190)-M(191)+M(192)+M(199)-M(200)+M(204)-M(207)-M(234) &
    +M(237))) * den(502)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,105)
  Gcoeff = (c(1)*(-M(45)-M(57)+M(66)+M(69)+M(76)+M(78)+M(81)-M(83)-M(84)-M(85)+M(88)-M(97))+c(2)*(-M(135)+M(180)+M(199)+M(204) &
    -M(215)-M(220)+M(223)-M(233))) * den(195)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,175)
  Gcoeff = (c(1)*(-M(45)-M(57)+M(66)+M(69)+M(76)+M(78)+M(81)-M(83)-M(84)-M(85)+M(88)-M(97))+c(2)*(-M(149)-M(173)+M(183)+M(189) &
    +M(207)+M(213)-M(216)-M(218))) * den(195)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,176)
  Gcoeff = (c(2)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(195)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,177)
  Gcoeff = (c(1)*(-M(45)-M(57)+M(66)+M(69)+M(76)+M(78)+M(81)-M(83)-M(84)-M(85)+M(88)-M(97))+c(2)*(-M(149)-M(173)+M(183)+M(189) &
    +M(207)+M(213)-M(216)-M(218))) * den(195)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,256)
  Gcoeff = (c(1)*(-M(45)-M(57)+M(66)+M(69)+M(76)+M(78)+M(81)-M(83)-M(84)-M(85)+M(88)-M(97))+c(2)*(-M(135)+M(180)+M(199)+M(204) &
    -M(215)-M(220)+M(223)-M(233))) * den(195)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,257)
  Gcoeff = (c(2)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(195)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,258)
  Gcoeff = (c(1)*(-M(45)+M(48)-M(57)+M(60)+M(71)+M(72)+M(73)-M(83)-M(84)-M(85)-M(97)+M(100))+c(2)*(-M(135)+M(136)+M(191)+M(196) &
    -M(215)-M(220)-M(233)+M(234))) * den(202)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,178)
  Gcoeff = (c(1)*(-M(45)+M(48)-M(57)+M(60)+M(71)+M(72)+M(73)-M(83)-M(84)-M(85)-M(97)+M(100))+c(2)*(-M(149)+M(150)-M(173)+M(174) &
    +M(192)+M(194)-M(216)-M(218))) * den(202)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,179)
  Gcoeff = (c(2)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(202)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,180)
  Gcoeff = (c(1)*(-M(45)+M(48)-M(57)+M(60)+M(71)+M(72)+M(73)-M(83)-M(84)-M(85)-M(97)+M(100))+c(2)*(-M(149)+M(150)-M(173)+M(174) &
    +M(192)+M(194)-M(216)-M(218))) * den(202)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,259)
  Gcoeff = (c(1)*(-M(45)+M(48)-M(57)+M(60)+M(71)+M(72)+M(73)-M(83)-M(84)-M(85)-M(97)+M(100))+c(2)*(-M(135)+M(136)+M(191)+M(196) &
    -M(215)-M(220)-M(233)+M(234))) * den(202)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,260)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(202)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,261)
  Gcoeff = (c(2)*(-M(138)+M(141)+M(173)-M(175)+M(180)-M(183)+M(205)-M(206)-M(213)+M(214)-M(215)+M(216)+M(223)-M(224)-M(233) &
    +M(235))) * den(520)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,61)
  Gcoeff = (c(2)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(520)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,62)
  Gcoeff = (c(2)*(-M(135)+M(138)-M(141)+M(149)+M(175)-M(189)+M(199)+M(204)-M(205)+M(206)-M(207)-M(214)+M(218)-M(220)+M(224) &
    -M(235))) * den(520)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,63)
  Gcoeff = (c(2)*(M(138)-M(141)-M(173)+M(175)-M(180)+M(183)-M(205)+M(206)+M(213)-M(214)+M(215)-M(216)-M(223)+M(224)+M(233) &
    -M(235))) * den(520)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,106)
  Gcoeff = (c(2)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(520)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,107)
  Gcoeff = (c(2)*(M(135)-M(138)+M(141)-M(149)-M(175)+M(189)-M(199)-M(204)+M(205)-M(206)+M(207)+M(214)-M(218)+M(220)-M(224) &
    +M(235))) * den(520)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,108)
  Gcoeff = (c(2)*(-M(139)+M(140)+M(173)-M(175)-M(177)+M(178)+M(181)-M(182)+M(205)-M(206)-M(215)+M(216)-M(233)+M(235)+M(237) &
    -M(238))) * den(535)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,64)
  Gcoeff = (c(2)*(M(139)-M(140)-M(145)+M(146)-M(173)+M(174)+M(176)-M(178)-M(191)+M(192)+M(215)-M(216)+M(233)-M(234)-M(236) &
    +M(238))) * den(535)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,65)
  Gcoeff = (c(2)*(M(145)-M(146)-M(174)+M(175)-M(176)+M(177)-M(181)+M(182)+M(191)-M(192)-M(205)+M(206)+M(234)-M(235)+M(236) &
    -M(237))) * den(535)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,66)
  Gcoeff = (c(2)*(M(139)-M(140)-M(173)+M(175)+M(177)-M(178)-M(181)+M(182)-M(205)+M(206)+M(215)-M(216)+M(233)-M(235)-M(237) &
    +M(238))) * den(535)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,109)
  Gcoeff = (c(2)*(-M(139)+M(140)+M(145)-M(146)+M(173)-M(174)-M(176)+M(178)+M(191)-M(192)-M(215)+M(216)-M(233)+M(234)+M(236) &
    -M(238))) * den(535)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,110)
  Gcoeff = (c(2)*(-M(145)+M(146)+M(174)-M(175)+M(176)-M(177)+M(181)-M(182)-M(191)+M(192)+M(205)-M(206)-M(234)+M(235)-M(236) &
    +M(237))) * den(535)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,111)
  Gcoeff = (c(2)*(M(131)-M(132)-M(155)+M(156)-M(179)+M(180)+M(185)-M(186)-M(221)+M(223)+M(225)-M(226)+M(245)-M(247)-M(249) &
    +M(250))) * den(197)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,73)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(197)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,74)
  Gcoeff = (c(3)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(197)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,75)
  Gcoeff = (c(3)*(M(131)-M(132)-M(134)+M(136)-M(155)+M(156)+M(158)-M(160)-M(195)+M(196)+M(201)-M(202)+M(225)-M(226)-M(249) &
    +M(250))) * den(614)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,13)
  Gcoeff = (c(3)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(614)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,14)
  Gcoeff = (c(3)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(614)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,15)
  Gcoeff = (c(2)*(-M(137)+M(138)+M(155)-M(156)-M(161)+M(162)+M(179)-M(180)+M(222)-M(223)+M(224)-M(225)-M(246)+M(247)-M(248) &
    +M(249))) * den(234)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,76)
  Gcoeff = (c(2)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(234)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,77)
  Gcoeff = (c(3)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(234)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,78)
  Gcoeff = (c(2)*(M(131)-M(132)-M(137)+M(138)-M(161)+M(162)+M(185)-M(186)-M(221)+M(222)+M(224)-M(226)+M(245)-M(246)-M(248) &
    +M(250))) * den(252)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,79)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(252)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,80)
  Gcoeff = (c(3)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(252)
  T2sum(1:1,3) = T2sum(1:1,3) + Gcoeff * G0tensor(:,81)
  Gcoeff = (c(3)*(-M(140)+M(142)+M(155)-M(156)-M(161)+M(162)-M(171)+M(172)+M(177)-M(178)+M(182)-M(184)+M(222)-M(225)-M(246) &
    +M(249))) * den(666)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,16)
  Gcoeff = (c(3)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(666)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,17)
  Gcoeff = (c(3)*(-M(137)+M(138)+M(140)-M(142)+M(171)-M(172)-M(177)+M(178)+M(179)-M(180)-M(182)+M(184)-M(223)+M(224)+M(247) &
    -M(248))) * den(666)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,18)
  Gcoeff = (c(3)*(-M(148)+M(154)+M(158)-M(160)-M(161)+M(162)+M(164)-M(166)-M(171)+M(177)+M(182)-M(184)-M(195)+M(201)+M(222) &
    -M(246))) * den(700)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,19)
  Gcoeff = (c(3)*(-M(147)+M(148)+M(153)-M(154)+M(161)-M(162)-M(164)+M(166)-M(185)+M(186)+M(188)-M(190)+M(221)-M(222)-M(245) &
    +M(246))) * den(700)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,20)
  Gcoeff = (c(3)*(M(147)-M(153)-M(158)+M(160)+M(171)-M(177)-M(182)+M(184)+M(185)-M(186)-M(188)+M(190)+M(195)-M(201)-M(221) &
    +M(245))) * den(700)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,21)
  Gcoeff = (c(1)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(2)*(M(140)+M(173)-M(175) &
    -M(177)+M(178)-M(182)-M(206)+M(216))) * den(168)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,181)
  Gcoeff = (c(1)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(2)*(M(139)-M(181)-M(205) &
    +M(215)+M(233)-M(235)-M(237)+M(238))) * den(168)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,182)
  Gcoeff = (c(2)*(M(139)-M(140)-M(173)+M(175)+M(177)-M(178)-M(181)+M(182)-M(205)+M(206)+M(215)-M(216)+M(233)-M(235)-M(237) &
    +M(238))) * den(168)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,183)
  Gcoeff = (c(1)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(2)*(M(139)-M(181)-M(205) &
    +M(215)+M(233)-M(235)-M(237)+M(238))) * den(168)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,262)
  Gcoeff = (c(1)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(2)*(M(140)+M(173)-M(175) &
    -M(177)+M(178)-M(182)-M(206)+M(216))) * den(168)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,263)
  Gcoeff = (c(2)*(-M(139)+M(140)+M(173)-M(175)-M(177)+M(178)+M(181)-M(182)+M(205)-M(206)-M(215)+M(216)-M(233)+M(235)+M(237) &
    -M(238))) * den(168)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,264)
  Gcoeff = (c(2)*(M(138)-M(139)+M(140)-M(141)-M(177)+M(178)-M(180)+M(181)-M(182)+M(183)+M(213)-M(214)-M(223)+M(224)+M(237) &
    -M(238))) * den(628)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,67)
  Gcoeff = (c(2)*(M(139)-M(140)-M(173)+M(175)+M(177)-M(178)-M(181)+M(182)-M(205)+M(206)+M(215)-M(216)+M(233)-M(235)-M(237) &
    +M(238))) * den(628)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,68)
  Gcoeff = (c(2)*(-M(138)+M(141)+M(173)-M(175)+M(180)-M(183)+M(205)-M(206)-M(213)+M(214)-M(215)+M(216)+M(223)-M(224)-M(233) &
    +M(235))) * den(628)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,69)
  Gcoeff = (c(2)*(-M(138)+M(139)-M(140)+M(141)+M(177)-M(178)+M(180)-M(181)+M(182)-M(183)-M(213)+M(214)+M(223)-M(224)-M(237) &
    +M(238))) * den(628)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,112)
  Gcoeff = (c(2)*(-M(139)+M(140)+M(173)-M(175)-M(177)+M(178)+M(181)-M(182)+M(205)-M(206)-M(215)+M(216)-M(233)+M(235)+M(237) &
    -M(238))) * den(628)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,113)
  Gcoeff = (c(2)*(M(138)-M(141)-M(173)+M(175)-M(180)+M(183)-M(205)+M(206)+M(213)-M(214)+M(215)-M(216)-M(223)+M(224)+M(233) &
    -M(235))) * den(628)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,114)
  Gcoeff = (c(1)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(2)*(-M(146)-M(174)+M(175) &
    -M(176)+M(177)+M(182)-M(192)+M(206))) * den(173)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,184)
  Gcoeff = (c(1)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(2)*(-M(145)+M(181)-M(191) &
    +M(205)-M(234)+M(235)-M(236)+M(237))) * den(173)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,185)
  Gcoeff = (c(2)*(-M(145)+M(146)+M(174)-M(175)+M(176)-M(177)+M(181)-M(182)-M(191)+M(192)+M(205)-M(206)-M(234)+M(235)-M(236) &
    +M(237))) * den(173)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,186)
  Gcoeff = (c(1)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(2)*(-M(145)+M(181)-M(191) &
    +M(205)-M(234)+M(235)-M(236)+M(237))) * den(173)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,265)
  Gcoeff = (c(1)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(2)*(-M(146)-M(174)+M(175) &
    -M(176)+M(177)+M(182)-M(192)+M(206))) * den(173)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,266)
  Gcoeff = (c(2)*(M(145)-M(146)-M(174)+M(175)-M(176)+M(177)-M(181)+M(182)+M(191)-M(192)-M(205)+M(206)+M(234)-M(235)+M(236) &
    -M(237))) * den(173)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,267)
  Gcoeff = (c(1)*(M(45)-M(48)-M(71)+M(83)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124))+c(2)*(M(140)-M(146)+M(173) &
    -M(174)-M(176)+M(178)-M(192)+M(216))) * den(178)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,187)
  Gcoeff = (c(1)*(M(45)-M(48)-M(71)+M(83)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124))+c(2)*(M(139)-M(145)-M(191) &
    +M(215)+M(233)-M(234)-M(236)+M(238))) * den(178)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,188)
  Gcoeff = (c(2)*(M(139)-M(140)-M(145)+M(146)-M(173)+M(174)+M(176)-M(178)-M(191)+M(192)+M(215)-M(216)+M(233)-M(234)-M(236) &
    +M(238))) * den(178)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,189)
  Gcoeff = (c(1)*(M(45)-M(48)-M(71)+M(83)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124))+c(2)*(M(139)-M(145)-M(191) &
    +M(215)+M(233)-M(234)-M(236)+M(238))) * den(178)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,268)
  Gcoeff = (c(1)*(M(45)-M(48)-M(71)+M(83)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124))+c(2)*(M(140)-M(146)+M(173) &
    -M(174)-M(176)+M(178)-M(192)+M(216))) * den(178)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,269)
  Gcoeff = (c(2)*(-M(139)+M(140)+M(145)-M(146)+M(173)-M(174)-M(176)+M(178)+M(191)-M(192)-M(215)+M(216)-M(233)+M(234)+M(236) &
    -M(238))) * den(178)
  T2sum(1:1,5) = T2sum(1:1,5) + Gcoeff * G0tensor(:,270)
  Gcoeff = (c(2)*(M(144)-M(147)-M(174)+M(177)-M(181)+M(182)+M(189)-M(190)+M(191)-M(192)-M(199)+M(200)-M(204)+M(207)+M(234) &
    -M(237))) * den(646)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,70)
  Gcoeff = (c(2)*(-M(145)+M(146)+M(174)-M(175)+M(176)-M(177)+M(181)-M(182)-M(191)+M(192)+M(205)-M(206)-M(234)+M(235)-M(236) &
    +M(237))) * den(646)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,71)
  Gcoeff = (c(2)*(-M(144)+M(145)-M(146)+M(147)+M(175)-M(176)-M(189)+M(190)+M(199)-M(200)+M(204)-M(205)+M(206)-M(207)-M(235) &
    +M(236))) * den(646)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,72)
  Gcoeff = (c(2)*(-M(144)+M(147)+M(174)-M(177)+M(181)-M(182)-M(189)+M(190)-M(191)+M(192)+M(199)-M(200)+M(204)-M(207)-M(234) &
    +M(237))) * den(646)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,115)
  Gcoeff = (c(2)*(M(145)-M(146)-M(174)+M(175)-M(176)+M(177)-M(181)+M(182)+M(191)-M(192)-M(205)+M(206)+M(234)-M(235)+M(236) &
    -M(237))) * den(646)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,116)
  Gcoeff = (c(2)*(M(144)-M(145)+M(146)-M(147)-M(175)+M(176)+M(189)-M(190)-M(199)+M(200)-M(204)+M(205)-M(206)+M(207)+M(235) &
    -M(236))) * den(646)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,117)
  Gcoeff = (c(2)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(661)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,73)
  Gcoeff = (c(2)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(661)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,74)
  Gcoeff = (c(2)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(661)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,75)
  Gcoeff = (c(2)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(661)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,118)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(661)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,119)
  Gcoeff = (c(2)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(661)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,120)
  Gcoeff = (c(3)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(705)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,22)
  Gcoeff = (c(3)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(705)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,23)
  Gcoeff = (c(3)*(M(147)-M(148)-M(153)+M(154)-M(161)+M(162)+M(164)-M(166)+M(185)-M(186)-M(188)+M(190)-M(221)+M(222)+M(245) &
    -M(246))) * den(705)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,24)
  Gcoeff = (c(2)*(-M(134)+M(137)-M(139)-M(141)+M(142)+M(144)+M(150)-M(153)+M(172)-M(188)+M(194)+M(200)-M(202)-M(214)-M(238) &
    +M(248))) * den(135)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,46)
  Gcoeff = (c(2)*(-M(137)+M(139)+M(141)-M(142)+M(171)-M(172)+M(179)-M(181)-M(183)+M(184)-M(213)+M(214)-M(237)+M(238)+M(247) &
    -M(248))) * den(135)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,47)
  Gcoeff = (c(2)*(M(134)-M(144)-M(150)+M(153)-M(171)-M(179)+M(181)+M(183)-M(184)+M(188)-M(194)-M(200)+M(202)+M(213)+M(237) &
    -M(247))) * den(135)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,48)
  Gcoeff = (c(2)*(M(134)-M(137)+M(139)+M(141)-M(142)-M(144)-M(150)+M(153)-M(172)+M(188)-M(194)-M(200)+M(202)+M(214)+M(238) &
    -M(248))) * den(135)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,49)
  Gcoeff = (c(2)*(M(137)-M(139)-M(141)+M(142)-M(171)+M(172)-M(179)+M(181)+M(183)-M(184)+M(213)-M(214)+M(237)-M(238)-M(247) &
    +M(248))) * den(135)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,50)
  Gcoeff = (c(2)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(135)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,51)
  Gcoeff = (c(3)*(M(134)-M(137)+M(139)+M(141)-M(142)-M(144)-M(150)+M(153)-M(172)+M(188)-M(194)-M(200)+M(202)+M(214)+M(238) &
    -M(248))) * den(135)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,52)
  Gcoeff = (c(3)*(M(137)-M(139)-M(141)+M(142)-M(171)+M(172)-M(179)+M(181)+M(183)-M(184)+M(213)-M(214)+M(237)-M(238)-M(247) &
    +M(248))) * den(135)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,53)
  Gcoeff = (c(3)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(135)
  T2sum(1:5,4) = T2sum(1:5,4) + Gcoeff * G1tensor(:,54)
  Gcoeff = (c(3)*(M(134)-M(136)-M(137)+M(138)+M(140)-M(142)-M(147)+M(153)-M(172)+M(178)+M(188)-M(190)-M(196)+M(202)+M(224) &
    -M(248))) * den(739)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,25)
  Gcoeff = (c(3)*(M(137)-M(138)-M(140)+M(142)-M(171)+M(172)+M(177)-M(178)-M(179)+M(180)+M(182)-M(184)+M(223)-M(224)-M(247) &
    +M(248))) * den(739)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,26)
  Gcoeff = (c(3)*(-M(134)+M(136)+M(147)-M(153)+M(171)-M(177)+M(179)-M(180)-M(182)+M(184)-M(188)+M(190)+M(196)-M(202)-M(223) &
    +M(247))) * den(739)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,27)
  Gcoeff = (c(3)*(M(131)-M(132)-M(134)+M(136)-M(140)+M(142)+M(148)-M(154)-M(164)+M(166)+M(172)-M(178)+M(196)-M(202)-M(226) &
    +M(250))) * den(748)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,28)
  Gcoeff = (c(3)*(-M(131)+M(132)+M(134)-M(136)+M(155)-M(156)-M(158)+M(160)+M(195)-M(196)-M(201)+M(202)-M(225)+M(226)+M(249) &
    -M(250))) * den(748)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,29)
  Gcoeff = (c(3)*(M(140)-M(142)-M(148)+M(154)-M(155)+M(156)+M(158)-M(160)+M(164)-M(166)-M(172)+M(178)-M(195)+M(201)+M(225) &
    -M(249))) * den(748)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,30)
  Gcoeff = (c(2)*(M(135)-M(136)-M(139)+M(140)+M(145)-M(146)-M(149)+M(150)-M(176)+M(178)+M(194)-M(196)-M(218)+M(220)+M(236) &
    -M(238))) * den(720)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,76)
  Gcoeff = (c(2)*(M(139)-M(140)-M(145)+M(146)-M(173)+M(174)+M(176)-M(178)-M(191)+M(192)+M(215)-M(216)+M(233)-M(234)-M(236) &
    +M(238))) * den(720)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,77)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(720)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,78)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(139)-M(140)-M(145)+M(146)+M(149)-M(150)+M(176)-M(178)-M(194)+M(196)+M(218)-M(220)-M(236) &
    +M(238))) * den(720)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,121)
  Gcoeff = (c(2)*(-M(139)+M(140)+M(145)-M(146)+M(173)-M(174)-M(176)+M(178)+M(191)-M(192)-M(215)+M(216)-M(233)+M(234)+M(236) &
    -M(238))) * den(720)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,122)
  Gcoeff = (c(2)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(720)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,123)
  Gcoeff = (c(2)*(M(135)-M(138)+M(141)-M(144)+M(145)-M(146)+M(147)-M(149)-M(176)+M(190)-M(200)+M(214)-M(218)+M(220)-M(224) &
    +M(236))) * den(727)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,79)
  Gcoeff = (c(2)*(M(144)-M(145)+M(146)-M(147)-M(175)+M(176)+M(189)-M(190)-M(199)+M(200)-M(204)+M(205)-M(206)+M(207)+M(235) &
    -M(236))) * den(727)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,80)
  Gcoeff = (c(2)*(-M(135)+M(138)-M(141)+M(149)+M(175)-M(189)+M(199)+M(204)-M(205)+M(206)-M(207)-M(214)+M(218)-M(220)+M(224) &
    -M(235))) * den(727)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,81)
  Gcoeff = (c(2)*(-M(135)+M(138)-M(141)+M(144)-M(145)+M(146)-M(147)+M(149)+M(176)-M(190)+M(200)-M(214)+M(218)-M(220)+M(224) &
    -M(236))) * den(727)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,124)
  Gcoeff = (c(2)*(-M(144)+M(145)-M(146)+M(147)+M(175)-M(176)-M(189)+M(190)+M(199)-M(200)+M(204)-M(205)+M(206)-M(207)-M(235) &
    +M(236))) * den(727)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,125)
  Gcoeff = (c(2)*(M(135)-M(138)+M(141)-M(149)-M(175)+M(189)-M(199)-M(204)+M(205)-M(206)+M(207)+M(214)-M(218)+M(220)-M(224) &
    +M(235))) * den(727)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,126)
  Gcoeff = (c(2)*(-M(136)+M(138)-M(139)+M(140)-M(141)+M(144)-M(147)+M(150)+M(178)-M(190)+M(194)-M(196)+M(200)-M(214)+M(224) &
    -M(238))) * den(736)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,82)
  Gcoeff = (c(2)*(-M(138)+M(139)-M(140)+M(141)+M(177)-M(178)+M(180)-M(181)+M(182)-M(183)-M(213)+M(214)+M(223)-M(224)-M(237) &
    +M(238))) * den(736)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,83)
  Gcoeff = (c(2)*(M(136)-M(144)+M(147)-M(150)-M(177)-M(180)+M(181)-M(182)+M(183)+M(190)-M(194)+M(196)-M(200)+M(213)-M(223) &
    +M(237))) * den(736)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,84)
  Gcoeff = (c(2)*(M(136)-M(138)+M(139)-M(140)+M(141)-M(144)+M(147)-M(150)-M(178)+M(190)-M(194)+M(196)-M(200)+M(214)-M(224) &
    +M(238))) * den(736)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,127)
  Gcoeff = (c(2)*(M(138)-M(139)+M(140)-M(141)-M(177)+M(178)-M(180)+M(181)-M(182)+M(183)+M(213)-M(214)-M(223)+M(224)+M(237) &
    -M(238))) * den(736)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,128)
  Gcoeff = (c(2)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(736)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,129)
  Gcoeff = (c(3)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(785)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,31)
  Gcoeff = (c(3)*(M(131)-M(132)-M(134)+M(136)-M(140)+M(142)+M(148)-M(154)-M(164)+M(166)+M(172)-M(178)+M(196)-M(202)-M(226) &
    +M(250))) * den(1156)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,32)
  Gcoeff = (c(3)*(M(134)-M(136)-M(137)+M(138)+M(140)-M(142)-M(147)+M(153)-M(172)+M(178)+M(188)-M(190)-M(196)+M(202)+M(224) &
    -M(248))) * den(1159)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,33)
  Gcoeff = (c(2)*(M(135)-M(136)-M(139)+M(140)+M(145)-M(146)-M(149)+M(150)-M(176)+M(178)+M(194)-M(196)-M(218)+M(220)+M(236) &
    -M(238))) * den(810)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,85)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(139)-M(140)-M(145)+M(146)+M(149)-M(150)+M(176)-M(178)-M(194)+M(196)+M(218)-M(220)-M(236) &
    +M(238))) * den(810)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,130)
  Gcoeff = (c(2)*(-M(136)+M(138)-M(139)+M(140)-M(141)+M(144)-M(147)+M(150)+M(178)-M(190)+M(194)-M(196)+M(200)-M(214)+M(224) &
    -M(238))) * den(1160)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,86)
  Gcoeff = (c(2)*(M(136)-M(138)+M(139)-M(140)+M(141)-M(144)+M(147)-M(150)-M(178)+M(190)-M(194)+M(196)-M(200)+M(214)-M(224) &
    +M(238))) * den(1160)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,131)
  Gcoeff = (c(2)*(M(135)-M(138)+M(141)-M(144)+M(145)-M(146)+M(147)-M(149)-M(176)+M(190)-M(200)+M(214)-M(218)+M(220)-M(224) &
    +M(236))) * den(1163)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,87)
  Gcoeff = (c(2)*(-M(135)+M(138)-M(141)+M(144)-M(145)+M(146)-M(147)+M(149)+M(176)-M(190)+M(200)-M(214)+M(218)-M(220)+M(224) &
    -M(236))) * den(1163)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,132)
  Gcoeff = (c(3)*(-M(140)+M(142)+M(155)-M(156)-M(161)+M(162)-M(171)+M(172)+M(177)-M(178)+M(182)-M(184)+M(222)-M(225)-M(246) &
    +M(249))) * den(846)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,34)
  Gcoeff = (c(3)*(M(140)-M(142)-M(148)+M(154)-M(155)+M(156)+M(158)-M(160)+M(164)-M(166)-M(172)+M(178)-M(195)+M(201)+M(225) &
    -M(249))) * den(1168)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,35)
  Gcoeff = (c(3)*(-M(148)+M(154)+M(158)-M(160)-M(161)+M(162)+M(164)-M(166)-M(171)+M(177)+M(182)-M(184)-M(195)+M(201)+M(222) &
    -M(246))) * den(1171)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,36)
  Gcoeff = (c(3)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(904)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,37)
  Gcoeff = (c(3)*(M(131)-M(132)-M(134)+M(136)-M(155)+M(156)+M(158)-M(160)-M(195)+M(196)+M(201)-M(202)+M(225)-M(226)-M(249) &
    +M(250))) * den(1180)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,38)
  Gcoeff = (c(3)*(M(131)-M(132)-M(155)+M(156)-M(179)+M(180)+M(185)-M(186)-M(221)+M(223)+M(225)-M(226)+M(245)-M(247)-M(249) &
    +M(250))) * den(1183)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,39)
  Gcoeff = (c(3)*(-M(134)+M(136)+M(147)-M(153)+M(171)-M(177)+M(179)-M(180)-M(182)+M(184)-M(188)+M(190)+M(196)-M(202)-M(223) &
    +M(247))) * den(1192)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,40)
  Gcoeff = (c(3)*(M(147)-M(153)-M(158)+M(160)+M(171)-M(177)-M(182)+M(184)+M(185)-M(186)-M(188)+M(190)+M(195)-M(201)-M(221) &
    +M(245))) * den(1195)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,41)
  Gcoeff = (c(3)*(-M(137)+M(138)+M(140)-M(142)+M(171)-M(172)-M(177)+M(178)+M(179)-M(180)-M(182)+M(184)-M(223)+M(224)+M(247) &
    -M(248))) * den(1204)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,42)
  Gcoeff = (c(3)*(-M(137)+M(138)+M(155)-M(156)-M(161)+M(162)+M(179)-M(180)+M(222)-M(223)+M(224)-M(225)-M(246)+M(247)-M(248) &
    +M(249))) * den(1207)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,43)
  Gcoeff = (c(3)*(M(147)-M(148)-M(153)+M(154)-M(161)+M(162)+M(164)-M(166)+M(185)-M(186)-M(188)+M(190)-M(221)+M(222)+M(245) &
    -M(246))) * den(1216)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,44)
  Gcoeff = (c(3)*(M(131)-M(132)-M(137)+M(138)-M(161)+M(162)+M(185)-M(186)-M(221)+M(222)+M(224)-M(226)+M(245)-M(246)-M(248) &
    +M(250))) * den(1217)
  T2sum(1:5,3) = T2sum(1:5,3) + Gcoeff * G1tensor(:,45)
  Gcoeff = (c(2)*(M(144)-M(147)-M(174)+M(177)-M(181)+M(182)+M(189)-M(190)+M(191)-M(192)-M(199)+M(200)-M(204)+M(207)+M(234) &
    -M(237))) * den(962)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,88)
  Gcoeff = (c(2)*(-M(144)+M(147)+M(174)-M(177)+M(181)-M(182)-M(189)+M(190)-M(191)+M(192)+M(199)-M(200)+M(204)-M(207)-M(234) &
    +M(237))) * den(962)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,133)
  Gcoeff = (c(2)*(M(136)-M(144)+M(147)-M(150)-M(177)-M(180)+M(181)-M(182)+M(183)+M(190)-M(194)+M(196)-M(200)+M(213)-M(223) &
    +M(237))) * den(1193)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,89)
  Gcoeff = (c(2)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(1193)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,134)
  Gcoeff = (c(2)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(1198)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,90)
  Gcoeff = (c(2)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(1198)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,135)
  Gcoeff = (c(2)*(-M(138)+M(141)+M(173)-M(175)+M(180)-M(183)+M(205)-M(206)-M(213)+M(214)-M(215)+M(216)+M(223)-M(224)-M(233) &
    +M(235))) * den(1004)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,91)
  Gcoeff = (c(2)*(M(138)-M(141)-M(173)+M(175)-M(180)+M(183)-M(205)+M(206)+M(213)-M(214)+M(215)-M(216)-M(223)+M(224)+M(233) &
    -M(235))) * den(1004)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,136)
  Gcoeff = (c(2)*(M(138)-M(139)+M(140)-M(141)-M(177)+M(178)-M(180)+M(181)-M(182)+M(183)+M(213)-M(214)-M(223)+M(224)+M(237) &
    -M(238))) * den(1205)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,92)
  Gcoeff = (c(2)*(-M(138)+M(139)-M(140)+M(141)+M(177)-M(178)+M(180)-M(181)+M(182)-M(183)-M(213)+M(214)+M(223)-M(224)-M(237) &
    +M(238))) * den(1205)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,137)
  Gcoeff = (c(2)*(-M(139)+M(140)+M(173)-M(175)-M(177)+M(178)+M(181)-M(182)+M(205)-M(206)-M(215)+M(216)-M(233)+M(235)+M(237) &
    -M(238))) * den(1208)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,93)
  Gcoeff = (c(2)*(M(139)-M(140)-M(173)+M(175)+M(177)-M(178)-M(181)+M(182)-M(205)+M(206)+M(215)-M(216)+M(233)-M(235)-M(237) &
    +M(238))) * den(1208)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,138)
  Gcoeff = (c(2)*(-M(135)+M(138)-M(141)+M(149)+M(175)-M(189)+M(199)+M(204)-M(205)+M(206)-M(207)-M(214)+M(218)-M(220)+M(224) &
    -M(235))) * den(1229)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,94)
  Gcoeff = (c(2)*(M(135)-M(138)+M(141)-M(149)-M(175)+M(189)-M(199)-M(204)+M(205)-M(206)+M(207)+M(214)-M(218)+M(220)-M(224) &
    +M(235))) * den(1229)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,139)
  Gcoeff = (c(2)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(1234)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,95)
  Gcoeff = (c(2)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(1234)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,140)
  Gcoeff = (c(2)*(-M(144)+M(145)-M(146)+M(147)+M(175)-M(176)-M(189)+M(190)+M(199)-M(200)+M(204)-M(205)+M(206)-M(207)-M(235) &
    +M(236))) * den(1241)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,96)
  Gcoeff = (c(2)*(M(144)-M(145)+M(146)-M(147)-M(175)+M(176)+M(189)-M(190)-M(199)+M(200)-M(204)+M(205)-M(206)+M(207)+M(235) &
    -M(236))) * den(1241)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,141)
  Gcoeff = (c(2)*(M(145)-M(146)-M(174)+M(175)-M(176)+M(177)-M(181)+M(182)+M(191)-M(192)-M(205)+M(206)+M(234)-M(235)+M(236) &
    -M(237))) * den(1244)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,97)
  Gcoeff = (c(2)*(-M(145)+M(146)+M(174)-M(175)+M(176)-M(177)+M(181)-M(182)-M(191)+M(192)+M(205)-M(206)-M(234)+M(235)-M(236) &
    +M(237))) * den(1244)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,142)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(1264)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,98)
  Gcoeff = (c(2)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(1264)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,143)
  Gcoeff = (c(2)*(-M(139)+M(140)+M(145)-M(146)+M(173)-M(174)-M(176)+M(178)+M(191)-M(192)-M(215)+M(216)-M(233)+M(234)+M(236) &
    -M(238))) * den(1265)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,99)
  Gcoeff = (c(2)*(M(139)-M(140)-M(145)+M(146)-M(173)+M(174)+M(176)-M(178)-M(191)+M(192)+M(215)-M(216)+M(233)-M(234)-M(236) &
    +M(238))) * den(1265)
  T2sum(1:5,5) = T2sum(1:5,5) + Gcoeff * G1tensor(:,144)

end subroutine vamp_66

end module ol_vamp_66_ppjjjj_gggggg_1_/**/REALKIND
