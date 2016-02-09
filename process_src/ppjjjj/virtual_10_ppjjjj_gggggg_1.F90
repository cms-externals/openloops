
module ol_vamp_10_ppjjjj_gggggg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_10(M)
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
  complex(REALKIND), dimension(4,5,4,505) :: G1
  complex(REALKIND), dimension(4,15,4,427) :: G2
  complex(REALKIND), dimension(4,35,4,140) :: G3
  complex(REALKIND), dimension(4,70,4,26) :: G4
  complex(REALKIND), dimension(4,126,4,6) :: G5
  complex(REALKIND), dimension(5,270) :: G1tensor
  complex(REALKIND), dimension(15,810) :: G2tensor
  complex(REALKIND), dimension(35,465) :: G3tensor
  complex(REALKIND), dimension(70,135) :: G4tensor
  complex(REALKIND), dimension(126,20) :: G5tensor
  complex(REALKIND), dimension(210,6) :: G6tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,-4),Q(:,16),G1(:,:,:,1))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,61),G1(:,:,:,2))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,2),wf(:,-3),wf(:,-2),G1tensor(:,1))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,2),wf(:,-2),wf(:,-3),G1tensor(:,2))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,2),wf(:,-3),wf(:,-2),G1tensor(:,3))
  call check_last_UV_W(l_switch,G1(:,:,:,2),Q(:,51),wf(:,62),Q(:,12),G2tensor(:,1))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,61),wf(:,-5),G1(:,:,:,3))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,3),wf(:,-3),wf(:,-2),G1tensor(:,4))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,3),wf(:,-2),wf(:,-3),G1tensor(:,5))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,3),wf(:,-3),wf(:,-2),G1tensor(:,6))
  call check_last_UV_W(l_switch,G1(:,:,:,3),Q(:,51),wf(:,62),Q(:,12),G2tensor(:,2))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,61),G1(:,:,:,4))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,4),wf(:,-3),wf(:,-2),G1tensor(:,7))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,4),wf(:,-2),wf(:,-3),G1tensor(:,8))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,4),wf(:,-3),wf(:,-2),G1tensor(:,9))
  call check_last_UV_W(l_switch,G1(:,:,:,4),Q(:,51),wf(:,62),Q(:,12),G2tensor(:,3))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,-2),G1(:,:,:,5))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,5),wf(:,-3),wf(:,61),G1tensor(:,10))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,5),wf(:,61),wf(:,-3),G1tensor(:,11))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,5),wf(:,-3),wf(:,61),G1tensor(:,12))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,5),wf(:,-1),wf(:,104),G1tensor(:,13))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,5),wf(:,104),wf(:,-1),G1tensor(:,14))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,5),wf(:,-1),wf(:,104),G1tensor(:,15))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,5),wf(:,0),wf(:,91),G1tensor(:,16))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,5),wf(:,91),wf(:,0),G1tensor(:,17))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,5),wf(:,0),wf(:,91),G1tensor(:,18))
  call check_last_UV_W(l_switch,G1(:,:,:,5),Q(:,52),wf(:,7),Q(:,11),G2tensor(:,4))
  call check_last_UV_W(l_switch,G1(:,:,:,5),Q(:,52),wf(:,9),Q(:,11),G2tensor(:,5))
  call check_last_UV_W(l_switch,G1(:,:,:,5),Q(:,52),wf(:,10),Q(:,11),G2tensor(:,6))
  call check_last_UV_W(l_switch,G1(:,:,:,5),Q(:,52),wf(:,83),Q(:,11),G2tensor(:,7))
  call check_last_UV_W(l_switch,G1(:,:,:,5),Q(:,52),wf(:,124),Q(:,11),G2tensor(:,8))
  call check_last_UV_W(l_switch,G1(:,:,:,5),Q(:,52),wf(:,131),Q(:,11),G2tensor(:,9))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,-5),G1(:,:,:,6))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,6),wf(:,-3),wf(:,61),G1tensor(:,19))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,6),wf(:,61),wf(:,-3),G1tensor(:,20))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,6),wf(:,-3),wf(:,61),G1tensor(:,21))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,6),wf(:,-1),wf(:,104),G1tensor(:,22))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,6),wf(:,104),wf(:,-1),G1tensor(:,23))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,6),wf(:,-1),wf(:,104),G1tensor(:,24))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,6),wf(:,0),wf(:,91),G1tensor(:,25))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,6),wf(:,91),wf(:,0),G1tensor(:,26))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,6),wf(:,0),wf(:,91),G1tensor(:,27))
  call check_last_UV_W(l_switch,G1(:,:,:,6),Q(:,52),wf(:,7),Q(:,11),G2tensor(:,10))
  call check_last_UV_W(l_switch,G1(:,:,:,6),Q(:,52),wf(:,9),Q(:,11),G2tensor(:,11))
  call check_last_UV_W(l_switch,G1(:,:,:,6),Q(:,52),wf(:,10),Q(:,11),G2tensor(:,12))
  call check_last_UV_W(l_switch,G1(:,:,:,6),Q(:,52),wf(:,83),Q(:,11),G2tensor(:,13))
  call check_last_UV_W(l_switch,G1(:,:,:,6),Q(:,52),wf(:,124),Q(:,11),G2tensor(:,14))
  call check_last_UV_W(l_switch,G1(:,:,:,6),Q(:,52),wf(:,131),Q(:,11),G2tensor(:,15))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,-2),G1(:,:,:,7))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,7),wf(:,-3),wf(:,61),G1tensor(:,28))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,7),wf(:,61),wf(:,-3),G1tensor(:,29))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,7),wf(:,-3),wf(:,61),G1tensor(:,30))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,7),wf(:,-1),wf(:,104),G1tensor(:,31))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,7),wf(:,104),wf(:,-1),G1tensor(:,32))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,7),wf(:,-1),wf(:,104),G1tensor(:,33))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,7),wf(:,0),wf(:,91),G1tensor(:,34))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,7),wf(:,91),wf(:,0),G1tensor(:,35))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,7),wf(:,0),wf(:,91),G1tensor(:,36))
  call check_last_UV_W(l_switch,G1(:,:,:,7),Q(:,52),wf(:,7),Q(:,11),G2tensor(:,16))
  call check_last_UV_W(l_switch,G1(:,:,:,7),Q(:,52),wf(:,9),Q(:,11),G2tensor(:,17))
  call check_last_UV_W(l_switch,G1(:,:,:,7),Q(:,52),wf(:,10),Q(:,11),G2tensor(:,18))
  call check_last_UV_W(l_switch,G1(:,:,:,7),Q(:,52),wf(:,83),Q(:,11),G2tensor(:,19))
  call check_last_UV_W(l_switch,G1(:,:,:,7),Q(:,52),wf(:,124),Q(:,11),G2tensor(:,20))
  call check_last_UV_W(l_switch,G1(:,:,:,7),Q(:,52),wf(:,131),Q(:,11),G2tensor(:,21))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,-3),G1(:,:,:,8))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,8),wf(:,-2),wf(:,61),G1tensor(:,37))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,8),wf(:,61),wf(:,-2),G1tensor(:,38))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,8),wf(:,-2),wf(:,61),G1tensor(:,39))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,8),wf(:,-1),wf(:,90),G1tensor(:,40))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,8),wf(:,90),wf(:,-1),G1tensor(:,41))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,8),wf(:,-1),wf(:,90),G1tensor(:,42))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,8),wf(:,0),wf(:,105),G1tensor(:,43))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,8),wf(:,105),wf(:,0),G1tensor(:,44))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,8),wf(:,0),wf(:,105),G1tensor(:,45))
  call check_last_UV_W(l_switch,G1(:,:,:,8),Q(:,56),wf(:,1),Q(:,7),G2tensor(:,22))
  call check_last_UV_W(l_switch,G1(:,:,:,8),Q(:,56),wf(:,3),Q(:,7),G2tensor(:,23))
  call check_last_UV_W(l_switch,G1(:,:,:,8),Q(:,56),wf(:,4),Q(:,7),G2tensor(:,24))
  call check_last_UV_W(l_switch,G1(:,:,:,8),Q(:,56),wf(:,74),Q(:,7),G2tensor(:,25))
  call check_last_UV_W(l_switch,G1(:,:,:,8),Q(:,56),wf(:,103),Q(:,7),G2tensor(:,26))
  call check_last_UV_W(l_switch,G1(:,:,:,8),Q(:,56),wf(:,117),Q(:,7),G2tensor(:,27))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,-5),G1(:,:,:,9))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,9),wf(:,-2),wf(:,61),G1tensor(:,46))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,9),wf(:,61),wf(:,-2),G1tensor(:,47))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,9),wf(:,-2),wf(:,61),G1tensor(:,48))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,9),wf(:,-1),wf(:,90),G1tensor(:,49))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,9),wf(:,90),wf(:,-1),G1tensor(:,50))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,9),wf(:,-1),wf(:,90),G1tensor(:,51))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,9),wf(:,0),wf(:,105),G1tensor(:,52))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,9),wf(:,105),wf(:,0),G1tensor(:,53))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,9),wf(:,0),wf(:,105),G1tensor(:,54))
  call check_last_UV_W(l_switch,G1(:,:,:,9),Q(:,56),wf(:,1),Q(:,7),G2tensor(:,28))
  call check_last_UV_W(l_switch,G1(:,:,:,9),Q(:,56),wf(:,3),Q(:,7),G2tensor(:,29))
  call check_last_UV_W(l_switch,G1(:,:,:,9),Q(:,56),wf(:,4),Q(:,7),G2tensor(:,30))
  call check_last_UV_W(l_switch,G1(:,:,:,9),Q(:,56),wf(:,74),Q(:,7),G2tensor(:,31))
  call check_last_UV_W(l_switch,G1(:,:,:,9),Q(:,56),wf(:,103),Q(:,7),G2tensor(:,32))
  call check_last_UV_W(l_switch,G1(:,:,:,9),Q(:,56),wf(:,117),Q(:,7),G2tensor(:,33))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,-3),G1(:,:,:,10))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,10),wf(:,-2),wf(:,61),G1tensor(:,55))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,10),wf(:,61),wf(:,-2),G1tensor(:,56))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,10),wf(:,-2),wf(:,61),G1tensor(:,57))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,10),wf(:,-1),wf(:,90),G1tensor(:,58))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,10),wf(:,90),wf(:,-1),G1tensor(:,59))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,10),wf(:,-1),wf(:,90),G1tensor(:,60))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,10),wf(:,0),wf(:,105),G1tensor(:,61))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,10),wf(:,105),wf(:,0),G1tensor(:,62))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,10),wf(:,0),wf(:,105),G1tensor(:,63))
  call check_last_UV_W(l_switch,G1(:,:,:,10),Q(:,56),wf(:,1),Q(:,7),G2tensor(:,34))
  call check_last_UV_W(l_switch,G1(:,:,:,10),Q(:,56),wf(:,3),Q(:,7),G2tensor(:,35))
  call check_last_UV_W(l_switch,G1(:,:,:,10),Q(:,56),wf(:,4),Q(:,7),G2tensor(:,36))
  call check_last_UV_W(l_switch,G1(:,:,:,10),Q(:,56),wf(:,74),Q(:,7),G2tensor(:,37))
  call check_last_UV_W(l_switch,G1(:,:,:,10),Q(:,56),wf(:,103),Q(:,7),G2tensor(:,38))
  call check_last_UV_W(l_switch,G1(:,:,:,10),Q(:,56),wf(:,117),Q(:,7),G2tensor(:,39))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,90),G1(:,:,:,11))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,11),wf(:,-3),wf(:,-1),G1tensor(:,64))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,11),wf(:,-1),wf(:,-3),G1tensor(:,65))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,11),wf(:,-3),wf(:,-1),G1tensor(:,66))
  call check_last_UV_W(l_switch,G1(:,:,:,11),Q(:,53),wf(:,91),Q(:,10),G2tensor(:,40))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,90),wf(:,-5),G1(:,:,:,12))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,12),wf(:,-3),wf(:,-1),G1tensor(:,67))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,12),wf(:,-1),wf(:,-3),G1tensor(:,68))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,12),wf(:,-3),wf(:,-1),G1tensor(:,69))
  call check_last_UV_W(l_switch,G1(:,:,:,12),Q(:,53),wf(:,91),Q(:,10),G2tensor(:,41))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,90),G1(:,:,:,13))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,13),wf(:,-3),wf(:,-1),G1tensor(:,70))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,13),wf(:,-1),wf(:,-3),G1tensor(:,71))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,13),wf(:,-3),wf(:,-1),G1tensor(:,72))
  call check_last_UV_W(l_switch,G1(:,:,:,13),Q(:,53),wf(:,91),Q(:,10),G2tensor(:,42))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,-1),G1(:,:,:,14))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,14),wf(:,-3),wf(:,90),G1tensor(:,73))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,14),wf(:,90),wf(:,-3),G1tensor(:,74))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,14),wf(:,-3),wf(:,90),G1tensor(:,75))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,14),wf(:,-2),wf(:,104),G1tensor(:,76))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,14),wf(:,104),wf(:,-2),G1tensor(:,77))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,14),wf(:,-2),wf(:,104),G1tensor(:,78))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,14),wf(:,0),wf(:,62),G1tensor(:,79))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,14),wf(:,62),wf(:,0),G1tensor(:,80))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,14),wf(:,0),wf(:,62),G1tensor(:,81))
  call check_last_UV_W(l_switch,G1(:,:,:,14),Q(:,50),wf(:,25),Q(:,13),G2tensor(:,43))
  call check_last_UV_W(l_switch,G1(:,:,:,14),Q(:,50),wf(:,27),Q(:,13),G2tensor(:,44))
  call check_last_UV_W(l_switch,G1(:,:,:,14),Q(:,50),wf(:,28),Q(:,13),G2tensor(:,45))
  call check_last_UV_W(l_switch,G1(:,:,:,14),Q(:,50),wf(:,148),Q(:,13),G2tensor(:,46))
  call check_last_UV_W(l_switch,G1(:,:,:,14),Q(:,50),wf(:,160),Q(:,13),G2tensor(:,47))
  call check_last_UV_W(l_switch,G1(:,:,:,14),Q(:,50),wf(:,167),Q(:,13),G2tensor(:,48))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,-5),G1(:,:,:,15))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,15),wf(:,-3),wf(:,90),G1tensor(:,82))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,15),wf(:,90),wf(:,-3),G1tensor(:,83))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,15),wf(:,-3),wf(:,90),G1tensor(:,84))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,15),wf(:,-2),wf(:,104),G1tensor(:,85))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,15),wf(:,104),wf(:,-2),G1tensor(:,86))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,15),wf(:,-2),wf(:,104),G1tensor(:,87))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,15),wf(:,0),wf(:,62),G1tensor(:,88))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,15),wf(:,62),wf(:,0),G1tensor(:,89))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,15),wf(:,0),wf(:,62),G1tensor(:,90))
  call check_last_UV_W(l_switch,G1(:,:,:,15),Q(:,50),wf(:,25),Q(:,13),G2tensor(:,49))
  call check_last_UV_W(l_switch,G1(:,:,:,15),Q(:,50),wf(:,27),Q(:,13),G2tensor(:,50))
  call check_last_UV_W(l_switch,G1(:,:,:,15),Q(:,50),wf(:,28),Q(:,13),G2tensor(:,51))
  call check_last_UV_W(l_switch,G1(:,:,:,15),Q(:,50),wf(:,148),Q(:,13),G2tensor(:,52))
  call check_last_UV_W(l_switch,G1(:,:,:,15),Q(:,50),wf(:,160),Q(:,13),G2tensor(:,53))
  call check_last_UV_W(l_switch,G1(:,:,:,15),Q(:,50),wf(:,167),Q(:,13),G2tensor(:,54))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,-1),G1(:,:,:,16))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,16),wf(:,-3),wf(:,90),G1tensor(:,91))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,16),wf(:,90),wf(:,-3),G1tensor(:,92))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,16),wf(:,-3),wf(:,90),G1tensor(:,93))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,16),wf(:,-2),wf(:,104),G1tensor(:,94))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,16),wf(:,104),wf(:,-2),G1tensor(:,95))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,16),wf(:,-2),wf(:,104),G1tensor(:,96))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,16),wf(:,0),wf(:,62),G1tensor(:,97))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,16),wf(:,62),wf(:,0),G1tensor(:,98))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,16),wf(:,0),wf(:,62),G1tensor(:,99))
  call check_last_UV_W(l_switch,G1(:,:,:,16),Q(:,50),wf(:,25),Q(:,13),G2tensor(:,55))
  call check_last_UV_W(l_switch,G1(:,:,:,16),Q(:,50),wf(:,27),Q(:,13),G2tensor(:,56))
  call check_last_UV_W(l_switch,G1(:,:,:,16),Q(:,50),wf(:,28),Q(:,13),G2tensor(:,57))
  call check_last_UV_W(l_switch,G1(:,:,:,16),Q(:,50),wf(:,148),Q(:,13),G2tensor(:,58))
  call check_last_UV_W(l_switch,G1(:,:,:,16),Q(:,50),wf(:,160),Q(:,13),G2tensor(:,59))
  call check_last_UV_W(l_switch,G1(:,:,:,16),Q(:,50),wf(:,167),Q(:,13),G2tensor(:,60))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,104),G1(:,:,:,17))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,17),wf(:,-2),wf(:,-1),G1tensor(:,100))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,17),wf(:,-1),wf(:,-2),G1tensor(:,101))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,17),wf(:,-2),wf(:,-1),G1tensor(:,102))
  call check_last_UV_W(l_switch,G1(:,:,:,17),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,61))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,104),wf(:,-5),G1(:,:,:,18))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,18),wf(:,-2),wf(:,-1),G1tensor(:,103))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,18),wf(:,-1),wf(:,-2),G1tensor(:,104))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,18),wf(:,-2),wf(:,-1),G1tensor(:,105))
  call check_last_UV_W(l_switch,G1(:,:,:,18),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,62))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,104),G1(:,:,:,19))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,19),wf(:,-2),wf(:,-1),G1tensor(:,106))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,19),wf(:,-1),wf(:,-2),G1tensor(:,107))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,19),wf(:,-2),wf(:,-1),G1tensor(:,108))
  call check_last_UV_W(l_switch,G1(:,:,:,19),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,63))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,56),G1(:,:,:,20))
  call check_last_UV_W(l_switch,G1(:,:,:,20),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,64))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,59),G1(:,:,:,21))
  call check_last_UV_W(l_switch,G1(:,:,:,21),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,65))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,60),G1(:,:,:,22))
  call check_last_UV_W(l_switch,G1(:,:,:,22),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,66))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,56),wf(:,-5),G1(:,:,:,23))
  call check_last_UV_W(l_switch,G1(:,:,:,23),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,67))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,59),wf(:,-5),G1(:,:,:,24))
  call check_last_UV_W(l_switch,G1(:,:,:,24),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,68))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,60),wf(:,-5),G1(:,:,:,25))
  call check_last_UV_W(l_switch,G1(:,:,:,25),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,69))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,56),G1(:,:,:,26))
  call check_last_UV_W(l_switch,G1(:,:,:,26),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,70))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,59),G1(:,:,:,27))
  call check_last_UV_W(l_switch,G1(:,:,:,27),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,71))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,60),G1(:,:,:,28))
  call check_last_UV_W(l_switch,G1(:,:,:,28),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,72))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,113),G1(:,:,:,29))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,29),wf(:,-2),wf(:,-1),G1tensor(:,109))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,29),wf(:,-1),wf(:,-2),G1tensor(:,110))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,29),wf(:,-2),wf(:,-1),G1tensor(:,111))
  call check_last_UV_W(l_switch,G1(:,:,:,29),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,73))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,113),wf(:,-3),G1(:,:,:,30))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,30),wf(:,-2),wf(:,-1),G1tensor(:,112))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,30),wf(:,-1),wf(:,-2),G1tensor(:,113))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,30),wf(:,-2),wf(:,-1),G1tensor(:,114))
  call check_last_UV_W(l_switch,G1(:,:,:,30),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,74))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,113),G1(:,:,:,31))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,31),wf(:,-2),wf(:,-1),G1tensor(:,115))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,31),wf(:,-1),wf(:,-2),G1tensor(:,116))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,31),wf(:,-2),wf(:,-1),G1tensor(:,117))
  call check_last_UV_W(l_switch,G1(:,:,:,31),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,75))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,44),G1(:,:,:,32))
  call check_last_UV_W(l_switch,G1(:,:,:,32),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,76))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,47),G1(:,:,:,33))
  call check_last_UV_W(l_switch,G1(:,:,:,33),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,77))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,48),G1(:,:,:,34))
  call check_last_UV_W(l_switch,G1(:,:,:,34),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,78))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,44),wf(:,-3),G1(:,:,:,35))
  call check_last_UV_W(l_switch,G1(:,:,:,35),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,79))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,47),wf(:,-3),G1(:,:,:,36))
  call check_last_UV_W(l_switch,G1(:,:,:,36),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,80))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,48),wf(:,-3),G1(:,:,:,37))
  call check_last_UV_W(l_switch,G1(:,:,:,37),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,81))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,44),G1(:,:,:,38))
  call check_last_UV_W(l_switch,G1(:,:,:,38),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,82))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,47),G1(:,:,:,39))
  call check_last_UV_W(l_switch,G1(:,:,:,39),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,83))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,48),G1(:,:,:,40))
  call check_last_UV_W(l_switch,G1(:,:,:,40),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,84))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,-1),G1(:,:,:,41))
  call loop_GGG_G_12(G1(:,:,:,41),wf(:,-5),wf(:,-3),G1(:,:,:,42))
  call check_last_UV_W(l_switch,G1(:,:,:,42),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,85))
  call loop_GGG_G_12(G1(:,:,:,41),wf(:,-3),wf(:,-5),G1(:,:,:,43))
  call check_last_UV_W(l_switch,G1(:,:,:,43),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,86))
  call loop_GGG_G_23(G1(:,:,:,41),wf(:,-5),wf(:,-3),G1(:,:,:,44))
  call check_last_UV_W(l_switch,G1(:,:,:,44),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,87))
  call loop_UV_W(G1(:,:,:,41),Q(:,22),wf(:,79),Q(:,40),G2(:,:,:,1))
  call check_last_UV_W(l_switch,G2(:,:,:,1),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,1))
  call loop_UV_W(G1(:,:,:,41),Q(:,22),wf(:,-3),Q(:,8),G2(:,:,:,2))
  call loop_UV_W(G2(:,:,:,2),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,1))
  call check_last_UV_W(l_switch,G3(:,:,:,1),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,1))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,-2),G1(:,:,:,45))
  call loop_GGG_G_12(G1(:,:,:,45),wf(:,-5),wf(:,-3),G1(:,:,:,46))
  call check_last_UV_W(l_switch,G1(:,:,:,46),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,88))
  call loop_GGG_G_12(G1(:,:,:,45),wf(:,-3),wf(:,-5),G1(:,:,:,47))
  call check_last_UV_W(l_switch,G1(:,:,:,47),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,89))
  call loop_GGG_G_23(G1(:,:,:,45),wf(:,-5),wf(:,-3),G1(:,:,:,48))
  call check_last_UV_W(l_switch,G1(:,:,:,48),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,90))
  call loop_UV_W(G1(:,:,:,45),Q(:,22),wf(:,79),Q(:,40),G2(:,:,:,3))
  call check_last_UV_W(l_switch,G2(:,:,:,3),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,2))
  call loop_UV_W(G1(:,:,:,45),Q(:,22),wf(:,-3),Q(:,8),G2(:,:,:,4))
  call loop_UV_W(G2(:,:,:,4),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,2))
  call check_last_UV_W(l_switch,G3(:,:,:,2),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,2))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,-1),G1(:,:,:,49))
  call loop_GGG_G_12(G1(:,:,:,49),wf(:,-5),wf(:,-3),G1(:,:,:,50))
  call check_last_UV_W(l_switch,G1(:,:,:,50),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,91))
  call loop_GGG_G_12(G1(:,:,:,49),wf(:,-3),wf(:,-5),G1(:,:,:,51))
  call check_last_UV_W(l_switch,G1(:,:,:,51),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,92))
  call loop_GGG_G_23(G1(:,:,:,49),wf(:,-5),wf(:,-3),G1(:,:,:,52))
  call check_last_UV_W(l_switch,G1(:,:,:,52),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,93))
  call loop_UV_W(G1(:,:,:,49),Q(:,22),wf(:,79),Q(:,40),G2(:,:,:,5))
  call check_last_UV_W(l_switch,G2(:,:,:,5),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,3))
  call loop_UV_W(G1(:,:,:,49),Q(:,22),wf(:,-3),Q(:,8),G2(:,:,:,6))
  call loop_UV_W(G2(:,:,:,6),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,3))
  call check_last_UV_W(l_switch,G3(:,:,:,3),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,3))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,113),G1(:,:,:,53))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,53),wf(:,-3),wf(:,-1),G1tensor(:,118))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,53),wf(:,-1),wf(:,-3),G1tensor(:,119))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,53),wf(:,-3),wf(:,-1),G1tensor(:,120))
  call check_last_UV_W(l_switch,G1(:,:,:,53),Q(:,53),wf(:,91),Q(:,10),G2tensor(:,94))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,113),wf(:,-2),G1(:,:,:,54))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,54),wf(:,-3),wf(:,-1),G1tensor(:,121))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,54),wf(:,-1),wf(:,-3),G1tensor(:,122))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,54),wf(:,-3),wf(:,-1),G1tensor(:,123))
  call check_last_UV_W(l_switch,G1(:,:,:,54),Q(:,53),wf(:,91),Q(:,10),G2tensor(:,95))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,113),G1(:,:,:,55))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,55),wf(:,-3),wf(:,-1),G1tensor(:,124))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,55),wf(:,-1),wf(:,-3),G1tensor(:,125))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,55),wf(:,-3),wf(:,-1),G1tensor(:,126))
  call check_last_UV_W(l_switch,G1(:,:,:,55),Q(:,53),wf(:,91),Q(:,10),G2tensor(:,96))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,32),G1(:,:,:,56))
  call check_last_UV_W(l_switch,G1(:,:,:,56),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,97))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,35),G1(:,:,:,57))
  call check_last_UV_W(l_switch,G1(:,:,:,57),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,98))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,36),G1(:,:,:,58))
  call check_last_UV_W(l_switch,G1(:,:,:,58),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,99))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,32),wf(:,-2),G1(:,:,:,59))
  call check_last_UV_W(l_switch,G1(:,:,:,59),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,100))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,35),wf(:,-2),G1(:,:,:,60))
  call check_last_UV_W(l_switch,G1(:,:,:,60),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,101))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,36),wf(:,-2),G1(:,:,:,61))
  call check_last_UV_W(l_switch,G1(:,:,:,61),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,102))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,32),G1(:,:,:,62))
  call check_last_UV_W(l_switch,G1(:,:,:,62),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,103))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,35),G1(:,:,:,63))
  call check_last_UV_W(l_switch,G1(:,:,:,63),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,104))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,36),G1(:,:,:,64))
  call check_last_UV_W(l_switch,G1(:,:,:,64),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,105))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,-1),G1(:,:,:,65))
  call loop_GGG_G_12(G1(:,:,:,65),wf(:,-5),wf(:,-2),G1(:,:,:,66))
  call check_last_UV_W(l_switch,G1(:,:,:,66),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,106))
  call loop_GGG_G_12(G1(:,:,:,65),wf(:,-2),wf(:,-5),G1(:,:,:,67))
  call check_last_UV_W(l_switch,G1(:,:,:,67),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,107))
  call loop_GGG_G_23(G1(:,:,:,65),wf(:,-5),wf(:,-2),G1(:,:,:,68))
  call check_last_UV_W(l_switch,G1(:,:,:,68),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,108))
  call loop_GGG_G_12(G1(:,:,:,65),wf(:,-5),wf(:,0),G1(:,:,:,69))
  call check_last_UV_W(l_switch,G1(:,:,:,69),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,109))
  call loop_GGG_G_12(G1(:,:,:,65),wf(:,0),wf(:,-5),G1(:,:,:,70))
  call check_last_UV_W(l_switch,G1(:,:,:,70),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,110))
  call loop_GGG_G_23(G1(:,:,:,65),wf(:,-5),wf(:,0),G1(:,:,:,71))
  call check_last_UV_W(l_switch,G1(:,:,:,71),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,111))
  call loop_UV_W(G1(:,:,:,65),Q(:,26),wf(:,-5),Q(:,32),G2(:,:,:,7))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,7),wf(:,-2),wf(:,0),G2tensor(:,112))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,7),wf(:,0),wf(:,-2),G2tensor(:,113))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,7),wf(:,-2),wf(:,0),G2tensor(:,114))
  call check_last_UV_W(l_switch,G2(:,:,:,7),Q(:,58),wf(:,90),Q(:,5),G3tensor(:,4))
  call loop_UV_W(G1(:,:,:,65),Q(:,26),wf(:,113),Q(:,33),G2(:,:,:,8))
  call check_last_UV_W(l_switch,G2(:,:,:,8),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,5))
  call loop_UV_W(G1(:,:,:,65),Q(:,26),wf(:,70),Q(:,36),G2(:,:,:,9))
  call check_last_UV_W(l_switch,G2(:,:,:,9),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,6))
  call loop_UV_W(G1(:,:,:,65),Q(:,26),wf(:,-2),Q(:,4),G2(:,:,:,10))
  call loop_UV_W(G2(:,:,:,10),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,4))
  call check_last_UV_W(l_switch,G3(:,:,:,4),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,4))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,-3),G1(:,:,:,72))
  call loop_GGG_G_12(G1(:,:,:,72),wf(:,-5),wf(:,-2),G1(:,:,:,73))
  call check_last_UV_W(l_switch,G1(:,:,:,73),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,115))
  call loop_GGG_G_12(G1(:,:,:,72),wf(:,-2),wf(:,-5),G1(:,:,:,74))
  call check_last_UV_W(l_switch,G1(:,:,:,74),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,116))
  call loop_GGG_G_23(G1(:,:,:,72),wf(:,-5),wf(:,-2),G1(:,:,:,75))
  call check_last_UV_W(l_switch,G1(:,:,:,75),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,117))
  call loop_GGG_G_12(G1(:,:,:,72),wf(:,-5),wf(:,0),G1(:,:,:,76))
  call check_last_UV_W(l_switch,G1(:,:,:,76),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,118))
  call loop_GGG_G_12(G1(:,:,:,72),wf(:,0),wf(:,-5),G1(:,:,:,77))
  call check_last_UV_W(l_switch,G1(:,:,:,77),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,119))
  call loop_GGG_G_23(G1(:,:,:,72),wf(:,-5),wf(:,0),G1(:,:,:,78))
  call check_last_UV_W(l_switch,G1(:,:,:,78),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,120))
  call loop_UV_W(G1(:,:,:,72),Q(:,26),wf(:,-5),Q(:,32),G2(:,:,:,11))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,11),wf(:,-2),wf(:,0),G2tensor(:,121))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,11),wf(:,0),wf(:,-2),G2tensor(:,122))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,11),wf(:,-2),wf(:,0),G2tensor(:,123))
  call check_last_UV_W(l_switch,G2(:,:,:,11),Q(:,58),wf(:,90),Q(:,5),G3tensor(:,7))
  call loop_UV_W(G1(:,:,:,72),Q(:,26),wf(:,113),Q(:,33),G2(:,:,:,12))
  call check_last_UV_W(l_switch,G2(:,:,:,12),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,8))
  call loop_UV_W(G1(:,:,:,72),Q(:,26),wf(:,70),Q(:,36),G2(:,:,:,13))
  call check_last_UV_W(l_switch,G2(:,:,:,13),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,9))
  call loop_UV_W(G1(:,:,:,72),Q(:,26),wf(:,-2),Q(:,4),G2(:,:,:,14))
  call loop_UV_W(G2(:,:,:,14),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,5))
  call check_last_UV_W(l_switch,G3(:,:,:,5),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,5))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,-1),G1(:,:,:,79))
  call loop_GGG_G_12(G1(:,:,:,79),wf(:,-5),wf(:,-2),G1(:,:,:,80))
  call check_last_UV_W(l_switch,G1(:,:,:,80),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,124))
  call loop_GGG_G_12(G1(:,:,:,79),wf(:,-2),wf(:,-5),G1(:,:,:,81))
  call check_last_UV_W(l_switch,G1(:,:,:,81),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,125))
  call loop_GGG_G_23(G1(:,:,:,79),wf(:,-5),wf(:,-2),G1(:,:,:,82))
  call check_last_UV_W(l_switch,G1(:,:,:,82),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,126))
  call loop_GGG_G_12(G1(:,:,:,79),wf(:,-5),wf(:,0),G1(:,:,:,83))
  call check_last_UV_W(l_switch,G1(:,:,:,83),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,127))
  call loop_GGG_G_12(G1(:,:,:,79),wf(:,0),wf(:,-5),G1(:,:,:,84))
  call check_last_UV_W(l_switch,G1(:,:,:,84),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,128))
  call loop_GGG_G_23(G1(:,:,:,79),wf(:,-5),wf(:,0),G1(:,:,:,85))
  call check_last_UV_W(l_switch,G1(:,:,:,85),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,129))
  call loop_UV_W(G1(:,:,:,79),Q(:,26),wf(:,-5),Q(:,32),G2(:,:,:,15))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,15),wf(:,-2),wf(:,0),G2tensor(:,130))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,15),wf(:,0),wf(:,-2),G2tensor(:,131))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,15),wf(:,-2),wf(:,0),G2tensor(:,132))
  call check_last_UV_W(l_switch,G2(:,:,:,15),Q(:,58),wf(:,90),Q(:,5),G3tensor(:,10))
  call loop_UV_W(G1(:,:,:,79),Q(:,26),wf(:,113),Q(:,33),G2(:,:,:,16))
  call check_last_UV_W(l_switch,G2(:,:,:,16),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,11))
  call loop_UV_W(G1(:,:,:,79),Q(:,26),wf(:,70),Q(:,36),G2(:,:,:,17))
  call check_last_UV_W(l_switch,G2(:,:,:,17),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,12))
  call loop_UV_W(G1(:,:,:,79),Q(:,26),wf(:,-2),Q(:,4),G2(:,:,:,18))
  call loop_UV_W(G2(:,:,:,18),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,6))
  call check_last_UV_W(l_switch,G3(:,:,:,6),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,6))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,113),G1(:,:,:,86))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,86),wf(:,-3),wf(:,-2),G1tensor(:,127))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,86),wf(:,-2),wf(:,-3),G1tensor(:,128))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,86),wf(:,-3),wf(:,-2),G1tensor(:,129))
  call check_last_UV_W(l_switch,G1(:,:,:,86),Q(:,51),wf(:,62),Q(:,12),G2tensor(:,133))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,113),wf(:,-1),G1(:,:,:,87))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,87),wf(:,-3),wf(:,-2),G1tensor(:,130))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,87),wf(:,-2),wf(:,-3),G1tensor(:,131))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,87),wf(:,-3),wf(:,-2),G1tensor(:,132))
  call check_last_UV_W(l_switch,G1(:,:,:,87),Q(:,51),wf(:,62),Q(:,12),G2tensor(:,134))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,113),G1(:,:,:,88))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,88),wf(:,-3),wf(:,-2),G1tensor(:,133))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,88),wf(:,-2),wf(:,-3),G1tensor(:,134))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,88),wf(:,-3),wf(:,-2),G1tensor(:,135))
  call check_last_UV_W(l_switch,G1(:,:,:,88),Q(:,51),wf(:,62),Q(:,12),G2tensor(:,135))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,-2),G1(:,:,:,89))
  call loop_GGG_G_12(G1(:,:,:,89),wf(:,-5),wf(:,-1),G1(:,:,:,90))
  call check_last_UV_W(l_switch,G1(:,:,:,90),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,136))
  call loop_GGG_G_12(G1(:,:,:,89),wf(:,-1),wf(:,-5),G1(:,:,:,91))
  call check_last_UV_W(l_switch,G1(:,:,:,91),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,137))
  call loop_GGG_G_23(G1(:,:,:,89),wf(:,-5),wf(:,-1),G1(:,:,:,92))
  call check_last_UV_W(l_switch,G1(:,:,:,92),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,138))
  call loop_GGG_G_12(G1(:,:,:,89),wf(:,-5),wf(:,0),G1(:,:,:,93))
  call check_last_UV_W(l_switch,G1(:,:,:,93),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,139))
  call loop_GGG_G_12(G1(:,:,:,89),wf(:,0),wf(:,-5),G1(:,:,:,94))
  call check_last_UV_W(l_switch,G1(:,:,:,94),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,140))
  call loop_GGG_G_23(G1(:,:,:,89),wf(:,-5),wf(:,0),G1(:,:,:,95))
  call check_last_UV_W(l_switch,G1(:,:,:,95),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,141))
  call loop_UV_W(G1(:,:,:,89),Q(:,28),wf(:,-5),Q(:,32),G2(:,:,:,19))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,19),wf(:,-1),wf(:,0),G2tensor(:,142))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,19),wf(:,0),wf(:,-1),G2tensor(:,143))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,19),wf(:,-1),wf(:,0),G2tensor(:,144))
  call check_last_UV_W(l_switch,G2(:,:,:,19),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,13))
  call loop_UV_W(G1(:,:,:,89),Q(:,28),wf(:,113),Q(:,33),G2(:,:,:,20))
  call check_last_UV_W(l_switch,G2(:,:,:,20),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,14))
  call loop_UV_W(G1(:,:,:,89),Q(:,28),wf(:,99),Q(:,34),G2(:,:,:,21))
  call check_last_UV_W(l_switch,G2(:,:,:,21),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,15))
  call loop_UV_W(G1(:,:,:,89),Q(:,28),wf(:,-1),Q(:,2),G2(:,:,:,22))
  call loop_UV_W(G2(:,:,:,22),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,7))
  call check_last_UV_W(l_switch,G3(:,:,:,7),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,7))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,-3),G1(:,:,:,96))
  call loop_GGG_G_12(G1(:,:,:,96),wf(:,-5),wf(:,-1),G1(:,:,:,97))
  call check_last_UV_W(l_switch,G1(:,:,:,97),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,145))
  call loop_GGG_G_12(G1(:,:,:,96),wf(:,-1),wf(:,-5),G1(:,:,:,98))
  call check_last_UV_W(l_switch,G1(:,:,:,98),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,146))
  call loop_GGG_G_23(G1(:,:,:,96),wf(:,-5),wf(:,-1),G1(:,:,:,99))
  call check_last_UV_W(l_switch,G1(:,:,:,99),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,147))
  call loop_GGG_G_12(G1(:,:,:,96),wf(:,-5),wf(:,0),G1(:,:,:,100))
  call check_last_UV_W(l_switch,G1(:,:,:,100),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,148))
  call loop_GGG_G_12(G1(:,:,:,96),wf(:,0),wf(:,-5),G1(:,:,:,101))
  call check_last_UV_W(l_switch,G1(:,:,:,101),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,149))
  call loop_GGG_G_23(G1(:,:,:,96),wf(:,-5),wf(:,0),G1(:,:,:,102))
  call check_last_UV_W(l_switch,G1(:,:,:,102),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,150))
  call loop_UV_W(G1(:,:,:,96),Q(:,28),wf(:,-5),Q(:,32),G2(:,:,:,23))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,23),wf(:,-1),wf(:,0),G2tensor(:,151))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,23),wf(:,0),wf(:,-1),G2tensor(:,152))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,23),wf(:,-1),wf(:,0),G2tensor(:,153))
  call check_last_UV_W(l_switch,G2(:,:,:,23),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,16))
  call loop_UV_W(G1(:,:,:,96),Q(:,28),wf(:,113),Q(:,33),G2(:,:,:,24))
  call check_last_UV_W(l_switch,G2(:,:,:,24),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,17))
  call loop_UV_W(G1(:,:,:,96),Q(:,28),wf(:,99),Q(:,34),G2(:,:,:,25))
  call check_last_UV_W(l_switch,G2(:,:,:,25),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,18))
  call loop_UV_W(G1(:,:,:,96),Q(:,28),wf(:,-1),Q(:,2),G2(:,:,:,26))
  call loop_UV_W(G2(:,:,:,26),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,8))
  call check_last_UV_W(l_switch,G3(:,:,:,8),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,8))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,-2),G1(:,:,:,103))
  call loop_GGG_G_12(G1(:,:,:,103),wf(:,-5),wf(:,-1),G1(:,:,:,104))
  call check_last_UV_W(l_switch,G1(:,:,:,104),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,154))
  call loop_GGG_G_12(G1(:,:,:,103),wf(:,-1),wf(:,-5),G1(:,:,:,105))
  call check_last_UV_W(l_switch,G1(:,:,:,105),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,155))
  call loop_GGG_G_23(G1(:,:,:,103),wf(:,-5),wf(:,-1),G1(:,:,:,106))
  call check_last_UV_W(l_switch,G1(:,:,:,106),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,156))
  call loop_GGG_G_12(G1(:,:,:,103),wf(:,-5),wf(:,0),G1(:,:,:,107))
  call check_last_UV_W(l_switch,G1(:,:,:,107),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,157))
  call loop_GGG_G_12(G1(:,:,:,103),wf(:,0),wf(:,-5),G1(:,:,:,108))
  call check_last_UV_W(l_switch,G1(:,:,:,108),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,158))
  call loop_GGG_G_23(G1(:,:,:,103),wf(:,-5),wf(:,0),G1(:,:,:,109))
  call check_last_UV_W(l_switch,G1(:,:,:,109),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,159))
  call loop_UV_W(G1(:,:,:,103),Q(:,28),wf(:,-5),Q(:,32),G2(:,:,:,27))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,27),wf(:,-1),wf(:,0),G2tensor(:,160))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,27),wf(:,0),wf(:,-1),G2tensor(:,161))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,27),wf(:,-1),wf(:,0),G2tensor(:,162))
  call check_last_UV_W(l_switch,G2(:,:,:,27),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,19))
  call loop_UV_W(G1(:,:,:,103),Q(:,28),wf(:,113),Q(:,33),G2(:,:,:,28))
  call check_last_UV_W(l_switch,G2(:,:,:,28),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,20))
  call loop_UV_W(G1(:,:,:,103),Q(:,28),wf(:,99),Q(:,34),G2(:,:,:,29))
  call check_last_UV_W(l_switch,G2(:,:,:,29),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,21))
  call loop_UV_W(G1(:,:,:,103),Q(:,28),wf(:,-1),Q(:,2),G2(:,:,:,30))
  call loop_UV_W(G2(:,:,:,30),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,9))
  call check_last_UV_W(l_switch,G3(:,:,:,9),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,9))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,14),G1(:,:,:,110))
  call check_last_UV_W(l_switch,G1(:,:,:,110),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,163))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,14),wf(:,-1),G1(:,:,:,111))
  call check_last_UV_W(l_switch,G1(:,:,:,111),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,164))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,14),G1(:,:,:,112))
  call check_last_UV_W(l_switch,G1(:,:,:,112),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,165))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,17),G1(:,:,:,113))
  call check_last_UV_W(l_switch,G1(:,:,:,113),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,166))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,17),wf(:,-1),G1(:,:,:,114))
  call check_last_UV_W(l_switch,G1(:,:,:,114),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,167))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,17),G1(:,:,:,115))
  call check_last_UV_W(l_switch,G1(:,:,:,115),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,168))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,18),G1(:,:,:,116))
  call check_last_UV_W(l_switch,G1(:,:,:,116),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,169))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,18),wf(:,-1),G1(:,:,:,117))
  call check_last_UV_W(l_switch,G1(:,:,:,117),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,170))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,18),G1(:,:,:,118))
  call check_last_UV_W(l_switch,G1(:,:,:,118),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,171))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,105),G1(:,:,:,119))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,119),wf(:,-3),wf(:,0),G1tensor(:,136))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,119),wf(:,0),wf(:,-3),G1tensor(:,137))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,119),wf(:,-3),wf(:,0),G1tensor(:,138))
  call check_last_UV_W(l_switch,G1(:,:,:,119),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,172))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,105),wf(:,-5),G1(:,:,:,120))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,120),wf(:,-3),wf(:,0),G1tensor(:,139))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,120),wf(:,0),wf(:,-3),G1tensor(:,140))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,120),wf(:,-3),wf(:,0),G1tensor(:,141))
  call check_last_UV_W(l_switch,G1(:,:,:,120),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,173))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,105),G1(:,:,:,121))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,121),wf(:,-3),wf(:,0),G1tensor(:,142))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,121),wf(:,0),wf(:,-3),G1tensor(:,143))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,121),wf(:,-3),wf(:,0),G1tensor(:,144))
  call check_last_UV_W(l_switch,G1(:,:,:,121),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,174))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,0),G1(:,:,:,122))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,122),wf(:,-3),wf(:,105),G1tensor(:,145))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,122),wf(:,105),wf(:,-3),G1tensor(:,146))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,122),wf(:,-3),wf(:,105),G1tensor(:,147))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,122),wf(:,-2),wf(:,91),G1tensor(:,148))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,122),wf(:,91),wf(:,-2),G1tensor(:,149))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,122),wf(:,-2),wf(:,91),G1tensor(:,150))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,122),wf(:,-1),wf(:,62),G1tensor(:,151))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,122),wf(:,62),wf(:,-1),G1tensor(:,152))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,122),wf(:,-1),wf(:,62),G1tensor(:,153))
  call check_last_UV_W(l_switch,G1(:,:,:,122),Q(:,49),wf(:,56),Q(:,14),G2tensor(:,175))
  call check_last_UV_W(l_switch,G1(:,:,:,122),Q(:,49),wf(:,59),Q(:,14),G2tensor(:,176))
  call check_last_UV_W(l_switch,G1(:,:,:,122),Q(:,49),wf(:,60),Q(:,14),G2tensor(:,177))
  call check_last_UV_W(l_switch,G1(:,:,:,122),Q(:,49),wf(:,202),Q(:,14),G2tensor(:,178))
  call check_last_UV_W(l_switch,G1(:,:,:,122),Q(:,49),wf(:,214),Q(:,14),G2tensor(:,179))
  call check_last_UV_W(l_switch,G1(:,:,:,122),Q(:,49),wf(:,221),Q(:,14),G2tensor(:,180))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,-5),G1(:,:,:,123))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,123),wf(:,-3),wf(:,105),G1tensor(:,154))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,123),wf(:,105),wf(:,-3),G1tensor(:,155))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,123),wf(:,-3),wf(:,105),G1tensor(:,156))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,123),wf(:,-2),wf(:,91),G1tensor(:,157))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,123),wf(:,91),wf(:,-2),G1tensor(:,158))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,123),wf(:,-2),wf(:,91),G1tensor(:,159))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,123),wf(:,-1),wf(:,62),G1tensor(:,160))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,123),wf(:,62),wf(:,-1),G1tensor(:,161))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,123),wf(:,-1),wf(:,62),G1tensor(:,162))
  call check_last_UV_W(l_switch,G1(:,:,:,123),Q(:,49),wf(:,56),Q(:,14),G2tensor(:,181))
  call check_last_UV_W(l_switch,G1(:,:,:,123),Q(:,49),wf(:,59),Q(:,14),G2tensor(:,182))
  call check_last_UV_W(l_switch,G1(:,:,:,123),Q(:,49),wf(:,60),Q(:,14),G2tensor(:,183))
  call check_last_UV_W(l_switch,G1(:,:,:,123),Q(:,49),wf(:,202),Q(:,14),G2tensor(:,184))
  call check_last_UV_W(l_switch,G1(:,:,:,123),Q(:,49),wf(:,214),Q(:,14),G2tensor(:,185))
  call check_last_UV_W(l_switch,G1(:,:,:,123),Q(:,49),wf(:,221),Q(:,14),G2tensor(:,186))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,0),G1(:,:,:,124))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,124),wf(:,-3),wf(:,105),G1tensor(:,163))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,124),wf(:,105),wf(:,-3),G1tensor(:,164))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,124),wf(:,-3),wf(:,105),G1tensor(:,165))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,124),wf(:,-2),wf(:,91),G1tensor(:,166))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,124),wf(:,91),wf(:,-2),G1tensor(:,167))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,124),wf(:,-2),wf(:,91),G1tensor(:,168))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,124),wf(:,-1),wf(:,62),G1tensor(:,169))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,124),wf(:,62),wf(:,-1),G1tensor(:,170))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,124),wf(:,-1),wf(:,62),G1tensor(:,171))
  call check_last_UV_W(l_switch,G1(:,:,:,124),Q(:,49),wf(:,56),Q(:,14),G2tensor(:,187))
  call check_last_UV_W(l_switch,G1(:,:,:,124),Q(:,49),wf(:,59),Q(:,14),G2tensor(:,188))
  call check_last_UV_W(l_switch,G1(:,:,:,124),Q(:,49),wf(:,60),Q(:,14),G2tensor(:,189))
  call check_last_UV_W(l_switch,G1(:,:,:,124),Q(:,49),wf(:,202),Q(:,14),G2tensor(:,190))
  call check_last_UV_W(l_switch,G1(:,:,:,124),Q(:,49),wf(:,214),Q(:,14),G2tensor(:,191))
  call check_last_UV_W(l_switch,G1(:,:,:,124),Q(:,49),wf(:,221),Q(:,14),G2tensor(:,192))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,91),G1(:,:,:,125))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,125),wf(:,-2),wf(:,0),G1tensor(:,172))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,125),wf(:,0),wf(:,-2),G1tensor(:,173))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,125),wf(:,-2),wf(:,0),G1tensor(:,174))
  call check_last_UV_W(l_switch,G1(:,:,:,125),Q(:,58),wf(:,90),Q(:,5),G2tensor(:,193))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,91),wf(:,-5),G1(:,:,:,126))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,126),wf(:,-2),wf(:,0),G1tensor(:,175))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,126),wf(:,0),wf(:,-2),G1tensor(:,176))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,126),wf(:,-2),wf(:,0),G1tensor(:,177))
  call check_last_UV_W(l_switch,G1(:,:,:,126),Q(:,58),wf(:,90),Q(:,5),G2tensor(:,194))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,91),G1(:,:,:,127))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,127),wf(:,-2),wf(:,0),G1tensor(:,178))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,127),wf(:,0),wf(:,-2),G1tensor(:,179))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,127),wf(:,-2),wf(:,0),G1tensor(:,180))
  call check_last_UV_W(l_switch,G1(:,:,:,127),Q(:,58),wf(:,90),Q(:,5),G2tensor(:,195))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,25),G1(:,:,:,128))
  call check_last_UV_W(l_switch,G1(:,:,:,128),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,196))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,27),G1(:,:,:,129))
  call check_last_UV_W(l_switch,G1(:,:,:,129),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,197))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,28),G1(:,:,:,130))
  call check_last_UV_W(l_switch,G1(:,:,:,130),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,198))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,25),wf(:,-5),G1(:,:,:,131))
  call check_last_UV_W(l_switch,G1(:,:,:,131),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,199))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,27),wf(:,-5),G1(:,:,:,132))
  call check_last_UV_W(l_switch,G1(:,:,:,132),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,200))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,28),wf(:,-5),G1(:,:,:,133))
  call check_last_UV_W(l_switch,G1(:,:,:,133),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,201))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,25),G1(:,:,:,134))
  call check_last_UV_W(l_switch,G1(:,:,:,134),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,202))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,27),G1(:,:,:,135))
  call check_last_UV_W(l_switch,G1(:,:,:,135),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,203))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,28),G1(:,:,:,136))
  call check_last_UV_W(l_switch,G1(:,:,:,136),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,204))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,99),G1(:,:,:,137))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,137),wf(:,-2),wf(:,0),G1tensor(:,181))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,137),wf(:,0),wf(:,-2),G1tensor(:,182))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,137),wf(:,-2),wf(:,0),G1tensor(:,183))
  call check_last_UV_W(l_switch,G1(:,:,:,137),Q(:,58),wf(:,90),Q(:,5),G2tensor(:,205))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,99),wf(:,-3),G1(:,:,:,138))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,138),wf(:,-2),wf(:,0),G1tensor(:,184))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,138),wf(:,0),wf(:,-2),G1tensor(:,185))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,138),wf(:,-2),wf(:,0),G1tensor(:,186))
  call check_last_UV_W(l_switch,G1(:,:,:,138),Q(:,58),wf(:,90),Q(:,5),G2tensor(:,206))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,99),G1(:,:,:,139))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,139),wf(:,-2),wf(:,0),G1tensor(:,187))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,139),wf(:,0),wf(:,-2),G1tensor(:,188))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,139),wf(:,-2),wf(:,0),G1tensor(:,189))
  call check_last_UV_W(l_switch,G1(:,:,:,139),Q(:,58),wf(:,90),Q(:,5),G2tensor(:,207))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,37),G1(:,:,:,140))
  call check_last_UV_W(l_switch,G1(:,:,:,140),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,208))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,39),G1(:,:,:,141))
  call check_last_UV_W(l_switch,G1(:,:,:,141),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,209))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,40),G1(:,:,:,142))
  call check_last_UV_W(l_switch,G1(:,:,:,142),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,210))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,37),wf(:,-3),G1(:,:,:,143))
  call check_last_UV_W(l_switch,G1(:,:,:,143),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,211))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,39),wf(:,-3),G1(:,:,:,144))
  call check_last_UV_W(l_switch,G1(:,:,:,144),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,212))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,40),wf(:,-3),G1(:,:,:,145))
  call check_last_UV_W(l_switch,G1(:,:,:,145),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,213))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,37),G1(:,:,:,146))
  call check_last_UV_W(l_switch,G1(:,:,:,146),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,214))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,39),G1(:,:,:,147))
  call check_last_UV_W(l_switch,G1(:,:,:,147),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,215))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,40),G1(:,:,:,148))
  call check_last_UV_W(l_switch,G1(:,:,:,148),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,216))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,0),G1(:,:,:,149))
  call loop_GGG_G_12(G1(:,:,:,149),wf(:,-5),wf(:,-3),G1(:,:,:,150))
  call check_last_UV_W(l_switch,G1(:,:,:,150),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,217))
  call loop_GGG_G_12(G1(:,:,:,149),wf(:,-3),wf(:,-5),G1(:,:,:,151))
  call check_last_UV_W(l_switch,G1(:,:,:,151),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,218))
  call loop_GGG_G_23(G1(:,:,:,149),wf(:,-5),wf(:,-3),G1(:,:,:,152))
  call check_last_UV_W(l_switch,G1(:,:,:,152),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,219))
  call loop_UV_W(G1(:,:,:,149),Q(:,21),wf(:,79),Q(:,40),G2(:,:,:,31))
  call check_last_UV_W(l_switch,G2(:,:,:,31),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,22))
  call loop_UV_W(G1(:,:,:,149),Q(:,21),wf(:,-3),Q(:,8),G2(:,:,:,32))
  call loop_UV_W(G2(:,:,:,32),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,10))
  call check_last_UV_W(l_switch,G3(:,:,:,10),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,10))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,-2),G1(:,:,:,153))
  call loop_GGG_G_12(G1(:,:,:,153),wf(:,-5),wf(:,-3),G1(:,:,:,154))
  call check_last_UV_W(l_switch,G1(:,:,:,154),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,220))
  call loop_GGG_G_12(G1(:,:,:,153),wf(:,-3),wf(:,-5),G1(:,:,:,155))
  call check_last_UV_W(l_switch,G1(:,:,:,155),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,221))
  call loop_GGG_G_23(G1(:,:,:,153),wf(:,-5),wf(:,-3),G1(:,:,:,156))
  call check_last_UV_W(l_switch,G1(:,:,:,156),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,222))
  call loop_UV_W(G1(:,:,:,153),Q(:,21),wf(:,79),Q(:,40),G2(:,:,:,33))
  call check_last_UV_W(l_switch,G2(:,:,:,33),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,23))
  call loop_UV_W(G1(:,:,:,153),Q(:,21),wf(:,-3),Q(:,8),G2(:,:,:,34))
  call loop_UV_W(G2(:,:,:,34),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,11))
  call check_last_UV_W(l_switch,G3(:,:,:,11),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,11))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,0),G1(:,:,:,157))
  call loop_GGG_G_12(G1(:,:,:,157),wf(:,-5),wf(:,-3),G1(:,:,:,158))
  call check_last_UV_W(l_switch,G1(:,:,:,158),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,223))
  call loop_GGG_G_12(G1(:,:,:,157),wf(:,-3),wf(:,-5),G1(:,:,:,159))
  call check_last_UV_W(l_switch,G1(:,:,:,159),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,224))
  call loop_GGG_G_23(G1(:,:,:,157),wf(:,-5),wf(:,-3),G1(:,:,:,160))
  call check_last_UV_W(l_switch,G1(:,:,:,160),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,225))
  call loop_UV_W(G1(:,:,:,157),Q(:,21),wf(:,79),Q(:,40),G2(:,:,:,35))
  call check_last_UV_W(l_switch,G2(:,:,:,35),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,24))
  call loop_UV_W(G1(:,:,:,157),Q(:,21),wf(:,-3),Q(:,8),G2(:,:,:,36))
  call loop_UV_W(G2(:,:,:,36),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,12))
  call check_last_UV_W(l_switch,G3(:,:,:,12),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,12))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,99),G1(:,:,:,161))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,161),wf(:,-3),wf(:,0),G1tensor(:,190))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,161),wf(:,0),wf(:,-3),G1tensor(:,191))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,161),wf(:,-3),wf(:,0),G1tensor(:,192))
  call check_last_UV_W(l_switch,G1(:,:,:,161),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,226))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,99),wf(:,-2),G1(:,:,:,162))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,162),wf(:,-3),wf(:,0),G1tensor(:,193))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,162),wf(:,0),wf(:,-3),G1tensor(:,194))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,162),wf(:,-3),wf(:,0),G1tensor(:,195))
  call check_last_UV_W(l_switch,G1(:,:,:,162),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,227))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,99),G1(:,:,:,163))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,163),wf(:,-3),wf(:,0),G1tensor(:,196))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,163),wf(:,0),wf(:,-3),G1tensor(:,197))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,163),wf(:,-3),wf(:,0),G1tensor(:,198))
  call check_last_UV_W(l_switch,G1(:,:,:,163),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,228))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,49),G1(:,:,:,164))
  call check_last_UV_W(l_switch,G1(:,:,:,164),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,229))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,51),G1(:,:,:,165))
  call check_last_UV_W(l_switch,G1(:,:,:,165),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,230))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,52),G1(:,:,:,166))
  call check_last_UV_W(l_switch,G1(:,:,:,166),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,231))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,49),wf(:,-2),G1(:,:,:,167))
  call check_last_UV_W(l_switch,G1(:,:,:,167),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,232))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,51),wf(:,-2),G1(:,:,:,168))
  call check_last_UV_W(l_switch,G1(:,:,:,168),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,233))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,52),wf(:,-2),G1(:,:,:,169))
  call check_last_UV_W(l_switch,G1(:,:,:,169),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,234))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,49),G1(:,:,:,170))
  call check_last_UV_W(l_switch,G1(:,:,:,170),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,235))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,51),G1(:,:,:,171))
  call check_last_UV_W(l_switch,G1(:,:,:,171),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,236))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,52),G1(:,:,:,172))
  call check_last_UV_W(l_switch,G1(:,:,:,172),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,237))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,0),G1(:,:,:,173))
  call loop_GGG_G_12(G1(:,:,:,173),wf(:,-5),wf(:,-2),G1(:,:,:,174))
  call check_last_UV_W(l_switch,G1(:,:,:,174),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,238))
  call loop_GGG_G_12(G1(:,:,:,173),wf(:,-2),wf(:,-5),G1(:,:,:,175))
  call check_last_UV_W(l_switch,G1(:,:,:,175),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,239))
  call loop_GGG_G_23(G1(:,:,:,173),wf(:,-5),wf(:,-2),G1(:,:,:,176))
  call check_last_UV_W(l_switch,G1(:,:,:,176),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,240))
  call loop_GGG_G_12(G1(:,:,:,173),wf(:,-5),wf(:,-1),G1(:,:,:,177))
  call check_last_UV_W(l_switch,G1(:,:,:,177),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,241))
  call loop_GGG_G_12(G1(:,:,:,173),wf(:,-1),wf(:,-5),G1(:,:,:,178))
  call check_last_UV_W(l_switch,G1(:,:,:,178),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,242))
  call loop_GGG_G_23(G1(:,:,:,173),wf(:,-5),wf(:,-1),G1(:,:,:,179))
  call check_last_UV_W(l_switch,G1(:,:,:,179),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,243))
  call loop_UV_W(G1(:,:,:,173),Q(:,25),wf(:,-5),Q(:,32),G2(:,:,:,37))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,37),wf(:,-2),wf(:,-1),G2tensor(:,244))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,37),wf(:,-1),wf(:,-2),G2tensor(:,245))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,37),wf(:,-2),wf(:,-1),G2tensor(:,246))
  call check_last_UV_W(l_switch,G2(:,:,:,37),Q(:,57),wf(:,105),Q(:,6),G3tensor(:,25))
  call loop_UV_W(G1(:,:,:,173),Q(:,25),wf(:,99),Q(:,34),G2(:,:,:,38))
  call check_last_UV_W(l_switch,G2(:,:,:,38),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,26))
  call loop_UV_W(G1(:,:,:,173),Q(:,25),wf(:,70),Q(:,36),G2(:,:,:,39))
  call check_last_UV_W(l_switch,G2(:,:,:,39),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,27))
  call loop_UV_W(G1(:,:,:,173),Q(:,25),wf(:,-2),Q(:,4),G2(:,:,:,40))
  call loop_UV_W(G2(:,:,:,40),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,13))
  call check_last_UV_W(l_switch,G3(:,:,:,13),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,13))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,-3),G1(:,:,:,180))
  call loop_GGG_G_12(G1(:,:,:,180),wf(:,-5),wf(:,-2),G1(:,:,:,181))
  call check_last_UV_W(l_switch,G1(:,:,:,181),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,247))
  call loop_GGG_G_12(G1(:,:,:,180),wf(:,-2),wf(:,-5),G1(:,:,:,182))
  call check_last_UV_W(l_switch,G1(:,:,:,182),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,248))
  call loop_GGG_G_23(G1(:,:,:,180),wf(:,-5),wf(:,-2),G1(:,:,:,183))
  call check_last_UV_W(l_switch,G1(:,:,:,183),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,249))
  call loop_GGG_G_12(G1(:,:,:,180),wf(:,-5),wf(:,-1),G1(:,:,:,184))
  call check_last_UV_W(l_switch,G1(:,:,:,184),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,250))
  call loop_GGG_G_12(G1(:,:,:,180),wf(:,-1),wf(:,-5),G1(:,:,:,185))
  call check_last_UV_W(l_switch,G1(:,:,:,185),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,251))
  call loop_GGG_G_23(G1(:,:,:,180),wf(:,-5),wf(:,-1),G1(:,:,:,186))
  call check_last_UV_W(l_switch,G1(:,:,:,186),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,252))
  call loop_UV_W(G1(:,:,:,180),Q(:,25),wf(:,-5),Q(:,32),G2(:,:,:,41))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,41),wf(:,-2),wf(:,-1),G2tensor(:,253))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,41),wf(:,-1),wf(:,-2),G2tensor(:,254))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,41),wf(:,-2),wf(:,-1),G2tensor(:,255))
  call check_last_UV_W(l_switch,G2(:,:,:,41),Q(:,57),wf(:,105),Q(:,6),G3tensor(:,28))
  call loop_UV_W(G1(:,:,:,180),Q(:,25),wf(:,99),Q(:,34),G2(:,:,:,42))
  call check_last_UV_W(l_switch,G2(:,:,:,42),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,29))
  call loop_UV_W(G1(:,:,:,180),Q(:,25),wf(:,70),Q(:,36),G2(:,:,:,43))
  call check_last_UV_W(l_switch,G2(:,:,:,43),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,30))
  call loop_UV_W(G1(:,:,:,180),Q(:,25),wf(:,-2),Q(:,4),G2(:,:,:,44))
  call loop_UV_W(G2(:,:,:,44),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,14))
  call check_last_UV_W(l_switch,G3(:,:,:,14),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,14))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,0),G1(:,:,:,187))
  call loop_GGG_G_12(G1(:,:,:,187),wf(:,-5),wf(:,-2),G1(:,:,:,188))
  call check_last_UV_W(l_switch,G1(:,:,:,188),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,256))
  call loop_GGG_G_12(G1(:,:,:,187),wf(:,-2),wf(:,-5),G1(:,:,:,189))
  call check_last_UV_W(l_switch,G1(:,:,:,189),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,257))
  call loop_GGG_G_23(G1(:,:,:,187),wf(:,-5),wf(:,-2),G1(:,:,:,190))
  call check_last_UV_W(l_switch,G1(:,:,:,190),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,258))
  call loop_GGG_G_12(G1(:,:,:,187),wf(:,-5),wf(:,-1),G1(:,:,:,191))
  call check_last_UV_W(l_switch,G1(:,:,:,191),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,259))
  call loop_GGG_G_12(G1(:,:,:,187),wf(:,-1),wf(:,-5),G1(:,:,:,192))
  call check_last_UV_W(l_switch,G1(:,:,:,192),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,260))
  call loop_GGG_G_23(G1(:,:,:,187),wf(:,-5),wf(:,-1),G1(:,:,:,193))
  call check_last_UV_W(l_switch,G1(:,:,:,193),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,261))
  call loop_UV_W(G1(:,:,:,187),Q(:,25),wf(:,-5),Q(:,32),G2(:,:,:,45))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,45),wf(:,-2),wf(:,-1),G2tensor(:,262))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,45),wf(:,-1),wf(:,-2),G2tensor(:,263))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,45),wf(:,-2),wf(:,-1),G2tensor(:,264))
  call check_last_UV_W(l_switch,G2(:,:,:,45),Q(:,57),wf(:,105),Q(:,6),G3tensor(:,31))
  call loop_UV_W(G1(:,:,:,187),Q(:,25),wf(:,99),Q(:,34),G2(:,:,:,46))
  call check_last_UV_W(l_switch,G2(:,:,:,46),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,32))
  call loop_UV_W(G1(:,:,:,187),Q(:,25),wf(:,70),Q(:,36),G2(:,:,:,47))
  call check_last_UV_W(l_switch,G2(:,:,:,47),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,33))
  call loop_UV_W(G1(:,:,:,187),Q(:,25),wf(:,-2),Q(:,4),G2(:,:,:,48))
  call loop_UV_W(G2(:,:,:,48),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,15))
  call check_last_UV_W(l_switch,G3(:,:,:,15),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,15))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,99),G1(:,:,:,194))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,194),wf(:,-3),wf(:,-2),G1tensor(:,199))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,194),wf(:,-2),wf(:,-3),G1tensor(:,200))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,194),wf(:,-3),wf(:,-2),G1tensor(:,201))
  call check_last_UV_W(l_switch,G1(:,:,:,194),Q(:,51),wf(:,62),Q(:,12),G2tensor(:,265))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,99),wf(:,0),G1(:,:,:,195))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,195),wf(:,-3),wf(:,-2),G1tensor(:,202))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,195),wf(:,-2),wf(:,-3),G1tensor(:,203))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,195),wf(:,-3),wf(:,-2),G1tensor(:,204))
  call check_last_UV_W(l_switch,G1(:,:,:,195),Q(:,51),wf(:,62),Q(:,12),G2tensor(:,266))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,99),G1(:,:,:,196))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,196),wf(:,-3),wf(:,-2),G1tensor(:,205))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,196),wf(:,-2),wf(:,-3),G1tensor(:,206))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,196),wf(:,-3),wf(:,-2),G1tensor(:,207))
  call check_last_UV_W(l_switch,G1(:,:,:,196),Q(:,51),wf(:,62),Q(:,12),G2tensor(:,267))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,14),G1(:,:,:,197))
  call check_last_UV_W(l_switch,G1(:,:,:,197),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,268))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,14),wf(:,0),G1(:,:,:,198))
  call check_last_UV_W(l_switch,G1(:,:,:,198),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,269))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,14),G1(:,:,:,199))
  call check_last_UV_W(l_switch,G1(:,:,:,199),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,270))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,17),G1(:,:,:,200))
  call check_last_UV_W(l_switch,G1(:,:,:,200),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,271))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,17),wf(:,0),G1(:,:,:,201))
  call check_last_UV_W(l_switch,G1(:,:,:,201),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,272))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,17),G1(:,:,:,202))
  call check_last_UV_W(l_switch,G1(:,:,:,202),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,273))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,18),G1(:,:,:,203))
  call check_last_UV_W(l_switch,G1(:,:,:,203),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,274))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,18),wf(:,0),G1(:,:,:,204))
  call check_last_UV_W(l_switch,G1(:,:,:,204),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,275))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,18),G1(:,:,:,205))
  call check_last_UV_W(l_switch,G1(:,:,:,205),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,276))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,62),G1(:,:,:,206))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,206),wf(:,-1),wf(:,0),G1tensor(:,208))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,206),wf(:,0),wf(:,-1),G1tensor(:,209))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,206),wf(:,-1),wf(:,0),G1tensor(:,210))
  call check_last_UV_W(l_switch,G1(:,:,:,206),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,277))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,62),wf(:,-5),G1(:,:,:,207))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,207),wf(:,-1),wf(:,0),G1tensor(:,211))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,207),wf(:,0),wf(:,-1),G1tensor(:,212))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,207),wf(:,-1),wf(:,0),G1tensor(:,213))
  call check_last_UV_W(l_switch,G1(:,:,:,207),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,278))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,62),G1(:,:,:,208))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,208),wf(:,-1),wf(:,0),G1tensor(:,214))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,208),wf(:,0),wf(:,-1),G1tensor(:,215))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,208),wf(:,-1),wf(:,0),G1tensor(:,216))
  call check_last_UV_W(l_switch,G1(:,:,:,208),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,279))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,7),G1(:,:,:,209))
  call check_last_UV_W(l_switch,G1(:,:,:,209),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,280))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,9),G1(:,:,:,210))
  call check_last_UV_W(l_switch,G1(:,:,:,210),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,281))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,10),G1(:,:,:,211))
  call check_last_UV_W(l_switch,G1(:,:,:,211),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,282))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,7),wf(:,-5),G1(:,:,:,212))
  call check_last_UV_W(l_switch,G1(:,:,:,212),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,283))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,9),wf(:,-5),G1(:,:,:,213))
  call check_last_UV_W(l_switch,G1(:,:,:,213),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,284))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,10),wf(:,-5),G1(:,:,:,214))
  call check_last_UV_W(l_switch,G1(:,:,:,214),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,285))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,7),G1(:,:,:,215))
  call check_last_UV_W(l_switch,G1(:,:,:,215),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,286))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,9),G1(:,:,:,216))
  call check_last_UV_W(l_switch,G1(:,:,:,216),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,287))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,10),G1(:,:,:,217))
  call check_last_UV_W(l_switch,G1(:,:,:,217),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,288))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,70),G1(:,:,:,218))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,218),wf(:,-1),wf(:,0),G1tensor(:,217))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,218),wf(:,0),wf(:,-1),G1tensor(:,218))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,218),wf(:,-1),wf(:,0),G1tensor(:,219))
  call check_last_UV_W(l_switch,G1(:,:,:,218),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,289))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,70),wf(:,-3),G1(:,:,:,219))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,219),wf(:,-1),wf(:,0),G1tensor(:,220))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,219),wf(:,0),wf(:,-1),G1tensor(:,221))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,219),wf(:,-1),wf(:,0),G1tensor(:,222))
  call check_last_UV_W(l_switch,G1(:,:,:,219),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,290))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,70),G1(:,:,:,220))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,220),wf(:,-1),wf(:,0),G1tensor(:,223))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,220),wf(:,0),wf(:,-1),G1tensor(:,224))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,220),wf(:,-1),wf(:,0),G1tensor(:,225))
  call check_last_UV_W(l_switch,G1(:,:,:,220),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,291))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,19),G1(:,:,:,221))
  call check_last_UV_W(l_switch,G1(:,:,:,221),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,292))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,21),G1(:,:,:,222))
  call check_last_UV_W(l_switch,G1(:,:,:,222),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,293))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,22),G1(:,:,:,223))
  call check_last_UV_W(l_switch,G1(:,:,:,223),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,294))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,19),wf(:,-3),G1(:,:,:,224))
  call check_last_UV_W(l_switch,G1(:,:,:,224),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,295))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,21),wf(:,-3),G1(:,:,:,225))
  call check_last_UV_W(l_switch,G1(:,:,:,225),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,296))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,22),wf(:,-3),G1(:,:,:,226))
  call check_last_UV_W(l_switch,G1(:,:,:,226),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,297))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,19),G1(:,:,:,227))
  call check_last_UV_W(l_switch,G1(:,:,:,227),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,298))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,21),G1(:,:,:,228))
  call check_last_UV_W(l_switch,G1(:,:,:,228),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,299))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,22),G1(:,:,:,229))
  call check_last_UV_W(l_switch,G1(:,:,:,229),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,300))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,-2),Q(:,4),G2(:,:,:,49))
  call loop_GGG_G_12(G2(:,:,:,49),wf(:,-5),wf(:,-3),G2(:,:,:,50))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,50),wf(:,-1),wf(:,0),G2tensor(:,301))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,50),wf(:,0),wf(:,-1),G2tensor(:,302))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,50),wf(:,-1),wf(:,0),G2tensor(:,303))
  call check_last_UV_W(l_switch,G2(:,:,:,50),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,34))
  call loop_GGG_G_12(G2(:,:,:,49),wf(:,-3),wf(:,-5),G2(:,:,:,51))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,51),wf(:,-1),wf(:,0),G2tensor(:,304))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,51),wf(:,0),wf(:,-1),G2tensor(:,305))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,51),wf(:,-1),wf(:,0),G2tensor(:,306))
  call check_last_UV_W(l_switch,G2(:,:,:,51),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,35))
  call loop_GGG_G_23(G2(:,:,:,49),wf(:,-5),wf(:,-3),G2(:,:,:,52))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,52),wf(:,-1),wf(:,0),G2tensor(:,307))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,52),wf(:,0),wf(:,-1),G2tensor(:,308))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,52),wf(:,-1),wf(:,0),G2tensor(:,309))
  call check_last_UV_W(l_switch,G2(:,:,:,52),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,36))
  call loop_GGG_G_12(G2(:,:,:,49),wf(:,-5),wf(:,104),G2(:,:,:,53))
  call check_last_UV_W(l_switch,G2(:,:,:,53),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,37))
  call loop_GGG_G_12(G2(:,:,:,49),wf(:,104),wf(:,-5),G2(:,:,:,54))
  call check_last_UV_W(l_switch,G2(:,:,:,54),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,38))
  call loop_GGG_G_23(G2(:,:,:,49),wf(:,-5),wf(:,104),G2(:,:,:,55))
  call check_last_UV_W(l_switch,G2(:,:,:,55),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,39))
  call loop_GGG_G_12(G2(:,:,:,49),wf(:,-5),wf(:,91),G2(:,:,:,56))
  call check_last_UV_W(l_switch,G2(:,:,:,56),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,40))
  call loop_GGG_G_12(G2(:,:,:,49),wf(:,91),wf(:,-5),G2(:,:,:,57))
  call check_last_UV_W(l_switch,G2(:,:,:,57),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,41))
  call loop_GGG_G_23(G2(:,:,:,49),wf(:,-5),wf(:,91),G2(:,:,:,58))
  call check_last_UV_W(l_switch,G2(:,:,:,58),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,42))
  call loop_GGG_G_12(G2(:,:,:,49),wf(:,-3),wf(:,113),G2(:,:,:,59))
  call check_last_UV_W(l_switch,G2(:,:,:,59),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,43))
  call loop_GGG_G_12(G2(:,:,:,49),wf(:,113),wf(:,-3),G2(:,:,:,60))
  call check_last_UV_W(l_switch,G2(:,:,:,60),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,44))
  call loop_GGG_G_23(G2(:,:,:,49),wf(:,-3),wf(:,113),G2(:,:,:,61))
  call check_last_UV_W(l_switch,G2(:,:,:,61),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,45))
  call loop_GGG_G_12(G2(:,:,:,49),wf(:,-3),wf(:,99),G2(:,:,:,62))
  call check_last_UV_W(l_switch,G2(:,:,:,62),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,46))
  call loop_GGG_G_12(G2(:,:,:,49),wf(:,99),wf(:,-3),G2(:,:,:,63))
  call check_last_UV_W(l_switch,G2(:,:,:,63),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,47))
  call loop_GGG_G_23(G2(:,:,:,49),wf(:,-3),wf(:,99),G2(:,:,:,64))
  call check_last_UV_W(l_switch,G2(:,:,:,64),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,48))
  call loop_UV_W(G2(:,:,:,49),Q(:,20),wf(:,-1),Q(:,2),G3(:,:,:,16))
  call loop_GGG_G_12(G3(:,:,:,16),wf(:,-5),wf(:,-3),G3(:,:,:,17))
  call check_last_UV_W(l_switch,G3(:,:,:,17),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,16))
  call loop_GGG_G_12(G3(:,:,:,16),wf(:,-3),wf(:,-5),G3(:,:,:,18))
  call check_last_UV_W(l_switch,G3(:,:,:,18),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,17))
  call loop_GGG_G_23(G3(:,:,:,16),wf(:,-5),wf(:,-3),G3(:,:,:,19))
  call check_last_UV_W(l_switch,G3(:,:,:,19),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,18))
  call loop_UV_W(G3(:,:,:,16),Q(:,22),wf(:,79),Q(:,40),G4(:,:,:,1))
  call check_last_UV_W(l_switch,G4(:,:,:,1),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,1))
  call loop_UV_W(G3(:,:,:,16),Q(:,22),wf(:,-3),Q(:,8),G4(:,:,:,2))
  call loop_UV_W(G4(:,:,:,2),Q(:,30),wf(:,-5),Q(:,32),G5(:,:,:,1))
  call check_last_UV_W(l_switch,G5(:,:,:,1),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,1))
  call loop_GGG_G_12(G2(:,:,:,49),wf(:,-1),wf(:,79),G2(:,:,:,65))
  call check_last_UV_W(l_switch,G2(:,:,:,65),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,49))
  call loop_GGG_G_12(G2(:,:,:,49),wf(:,79),wf(:,-1),G2(:,:,:,66))
  call check_last_UV_W(l_switch,G2(:,:,:,66),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,50))
  call loop_GGG_G_23(G2(:,:,:,49),wf(:,-1),wf(:,79),G2(:,:,:,67))
  call check_last_UV_W(l_switch,G2(:,:,:,67),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,51))
  call loop_UV_W(G2(:,:,:,49),Q(:,20),wf(:,-3),Q(:,8),G3(:,:,:,20))
  call loop_GGG_G_12(G3(:,:,:,20),wf(:,-5),wf(:,-1),G3(:,:,:,21))
  call check_last_UV_W(l_switch,G3(:,:,:,21),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,19))
  call loop_GGG_G_12(G3(:,:,:,20),wf(:,-1),wf(:,-5),G3(:,:,:,22))
  call check_last_UV_W(l_switch,G3(:,:,:,22),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,20))
  call loop_GGG_G_23(G3(:,:,:,20),wf(:,-5),wf(:,-1),G3(:,:,:,23))
  call check_last_UV_W(l_switch,G3(:,:,:,23),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,21))
  call loop_GGG_G_12(G3(:,:,:,20),wf(:,-5),wf(:,0),G3(:,:,:,24))
  call check_last_UV_W(l_switch,G3(:,:,:,24),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,22))
  call loop_GGG_G_12(G3(:,:,:,20),wf(:,0),wf(:,-5),G3(:,:,:,25))
  call check_last_UV_W(l_switch,G3(:,:,:,25),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,23))
  call loop_GGG_G_23(G3(:,:,:,20),wf(:,-5),wf(:,0),G3(:,:,:,26))
  call check_last_UV_W(l_switch,G3(:,:,:,26),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,24))
  call loop_UV_W(G3(:,:,:,20),Q(:,28),wf(:,-5),Q(:,32),G4(:,:,:,3))
  call check_last_GGG_G_12(l_switch,G4(:,:,:,3),wf(:,-1),wf(:,0),G4tensor(:,25))
  call check_last_GGG_G_12(l_switch,G4(:,:,:,3),wf(:,0),wf(:,-1),G4tensor(:,26))
  call check_last_GGG_G_23(l_switch,G4(:,:,:,3),wf(:,-1),wf(:,0),G4tensor(:,27))
  call check_last_UV_W(l_switch,G4(:,:,:,3),Q(:,60),wf(:,61),Q(:,3),G5tensor(:,2))
  call loop_UV_W(G3(:,:,:,20),Q(:,28),wf(:,113),Q(:,33),G4(:,:,:,4))
  call check_last_UV_W(l_switch,G4(:,:,:,4),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,3))
  call loop_UV_W(G3(:,:,:,20),Q(:,28),wf(:,99),Q(:,34),G4(:,:,:,5))
  call check_last_UV_W(l_switch,G4(:,:,:,5),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,4))
  call loop_UV_W(G3(:,:,:,20),Q(:,28),wf(:,-1),Q(:,2),G4(:,:,:,6))
  call loop_UV_W(G4(:,:,:,6),Q(:,30),wf(:,-5),Q(:,32),G5(:,:,:,2))
  call check_last_UV_W(l_switch,G5(:,:,:,2),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,2))
  call loop_GGG_G_12(G2(:,:,:,49),wf(:,-3),wf(:,-1),G2(:,:,:,68))
  call loop_UV_W(G2(:,:,:,68),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,27))
  call check_last_UV_W(l_switch,G3(:,:,:,27),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,28))
  call loop_GGG_G_12(G2(:,:,:,49),wf(:,-1),wf(:,-3),G2(:,:,:,69))
  call loop_UV_W(G2(:,:,:,69),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,28))
  call check_last_UV_W(l_switch,G3(:,:,:,28),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,29))
  call loop_GGG_G_23(G2(:,:,:,49),wf(:,-3),wf(:,-1),G2(:,:,:,70))
  call loop_UV_W(G2(:,:,:,70),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,29))
  call check_last_UV_W(l_switch,G3(:,:,:,29),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,30))
  call loop_UV_W(G2(:,:,:,49),Q(:,20),wf(:,32),Q(:,42),G3(:,:,:,30))
  call check_last_UV_W(l_switch,G3(:,:,:,30),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,31))
  call loop_UV_W(G2(:,:,:,49),Q(:,20),wf(:,35),Q(:,42),G3(:,:,:,31))
  call check_last_UV_W(l_switch,G3(:,:,:,31),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,32))
  call loop_UV_W(G2(:,:,:,49),Q(:,20),wf(:,36),Q(:,42),G3(:,:,:,32))
  call check_last_UV_W(l_switch,G3(:,:,:,32),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,33))
  call loop_GGG_G_12(G2(:,:,:,49),wf(:,0),wf(:,79),G2(:,:,:,71))
  call check_last_UV_W(l_switch,G2(:,:,:,71),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,52))
  call loop_GGG_G_12(G2(:,:,:,49),wf(:,79),wf(:,0),G2(:,:,:,72))
  call check_last_UV_W(l_switch,G2(:,:,:,72),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,53))
  call loop_GGG_G_23(G2(:,:,:,49),wf(:,0),wf(:,79),G2(:,:,:,73))
  call check_last_UV_W(l_switch,G2(:,:,:,73),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,54))
  call loop_GGG_G_12(G2(:,:,:,49),wf(:,-3),wf(:,0),G2(:,:,:,74))
  call loop_UV_W(G2(:,:,:,74),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,33))
  call check_last_UV_W(l_switch,G3(:,:,:,33),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,34))
  call loop_GGG_G_12(G2(:,:,:,49),wf(:,0),wf(:,-3),G2(:,:,:,75))
  call loop_UV_W(G2(:,:,:,75),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,34))
  call check_last_UV_W(l_switch,G3(:,:,:,34),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,35))
  call loop_GGG_G_23(G2(:,:,:,49),wf(:,-3),wf(:,0),G2(:,:,:,76))
  call loop_UV_W(G2(:,:,:,76),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,35))
  call check_last_UV_W(l_switch,G3(:,:,:,35),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,36))
  call loop_UV_W(G2(:,:,:,49),Q(:,20),wf(:,49),Q(:,41),G3(:,:,:,36))
  call check_last_UV_W(l_switch,G3(:,:,:,36),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,37))
  call loop_UV_W(G2(:,:,:,49),Q(:,20),wf(:,51),Q(:,41),G3(:,:,:,37))
  call check_last_UV_W(l_switch,G3(:,:,:,37),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,38))
  call loop_UV_W(G2(:,:,:,49),Q(:,20),wf(:,52),Q(:,41),G3(:,:,:,38))
  call check_last_UV_W(l_switch,G3(:,:,:,38),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,39))
  call loop_UV_W(G2(:,:,:,49),Q(:,20),wf(:,79),Q(:,40),G3(:,:,:,39))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,39),wf(:,-1),wf(:,0),G3tensor(:,55))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,39),wf(:,0),wf(:,-1),G3tensor(:,56))
  call check_last_GGG_G_23(l_switch,G3(:,:,:,39),wf(:,-1),wf(:,0),G3tensor(:,57))
  call check_last_UV_W(l_switch,G3(:,:,:,39),Q(:,60),wf(:,61),Q(:,3),G4tensor(:,40))
  call loop_UV_W(G2(:,:,:,49),Q(:,20),wf(:,182),Q(:,41),G3(:,:,:,40))
  call check_last_UV_W(l_switch,G3(:,:,:,40),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,41))
  call loop_UV_W(G2(:,:,:,49),Q(:,20),wf(:,104),Q(:,9),G3(:,:,:,41))
  call loop_UV_W(G3(:,:,:,41),Q(:,29),wf(:,-5),Q(:,32),G4(:,:,:,7))
  call check_last_UV_W(l_switch,G4(:,:,:,7),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,5))
  call loop_UV_W(G2(:,:,:,49),Q(:,20),wf(:,236),Q(:,42),G3(:,:,:,42))
  call check_last_UV_W(l_switch,G3(:,:,:,42),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,42))
  call loop_UV_W(G2(:,:,:,49),Q(:,20),wf(:,91),Q(:,10),G3(:,:,:,43))
  call loop_UV_W(G3(:,:,:,43),Q(:,30),wf(:,-5),Q(:,32),G4(:,:,:,8))
  call check_last_UV_W(l_switch,G4(:,:,:,8),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,6))
  call loop_UV_W(G2(:,:,:,49),Q(:,20),wf(:,191),Q(:,41),G3(:,:,:,44))
  call check_last_UV_W(l_switch,G3(:,:,:,44),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,43))
  call loop_UV_W(G2(:,:,:,49),Q(:,20),wf(:,245),Q(:,42),G3(:,:,:,45))
  call check_last_UV_W(l_switch,G3(:,:,:,45),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,44))
  call loop_UV_W(G2(:,:,:,49),Q(:,20),wf(:,192),Q(:,41),G3(:,:,:,46))
  call check_last_UV_W(l_switch,G3(:,:,:,46),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,45))
  call loop_UV_W(G2(:,:,:,49),Q(:,20),wf(:,246),Q(:,42),G3(:,:,:,47))
  call check_last_UV_W(l_switch,G3(:,:,:,47),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,46))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,1),G1(:,:,:,230))
  call check_last_UV_W(l_switch,G1(:,:,:,230),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,310))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,3),G1(:,:,:,231))
  call check_last_UV_W(l_switch,G1(:,:,:,231),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,311))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,4),G1(:,:,:,232))
  call check_last_UV_W(l_switch,G1(:,:,:,232),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,312))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,1),wf(:,-5),G1(:,:,:,233))
  call check_last_UV_W(l_switch,G1(:,:,:,233),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,313))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,3),wf(:,-5),G1(:,:,:,234))
  call check_last_UV_W(l_switch,G1(:,:,:,234),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,314))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,4),wf(:,-5),G1(:,:,:,235))
  call check_last_UV_W(l_switch,G1(:,:,:,235),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,315))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,1),G1(:,:,:,236))
  call check_last_UV_W(l_switch,G1(:,:,:,236),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,316))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,3),G1(:,:,:,237))
  call check_last_UV_W(l_switch,G1(:,:,:,237),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,317))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,4),G1(:,:,:,238))
  call check_last_UV_W(l_switch,G1(:,:,:,238),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,318))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,-5),Q(:,32),G2(:,:,:,77))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,-3),wf(:,1),G2tensor(:,319))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,-3),wf(:,3),G2tensor(:,320))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,-3),wf(:,4),G2tensor(:,321))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,1),wf(:,-3),G2tensor(:,322))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,3),wf(:,-3),G2tensor(:,323))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,4),wf(:,-3),G2tensor(:,324))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,-3),wf(:,1),G2tensor(:,325))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,-3),wf(:,3),G2tensor(:,326))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,-3),wf(:,4),G2tensor(:,327))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,-2),wf(:,7),G2tensor(:,328))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,-2),wf(:,9),G2tensor(:,329))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,-2),wf(:,10),G2tensor(:,330))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,7),wf(:,-2),G2tensor(:,331))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,9),wf(:,-2),G2tensor(:,332))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,10),wf(:,-2),G2tensor(:,333))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,-2),wf(:,7),G2tensor(:,334))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,-2),wf(:,9),G2tensor(:,335))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,-2),wf(:,10),G2tensor(:,336))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,-1),wf(:,25),G2tensor(:,337))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,-1),wf(:,27),G2tensor(:,338))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,-1),wf(:,28),G2tensor(:,339))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,25),wf(:,-1),G2tensor(:,340))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,27),wf(:,-1),G2tensor(:,341))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,28),wf(:,-1),G2tensor(:,342))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,-1),wf(:,25),G2tensor(:,343))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,-1),wf(:,27),G2tensor(:,344))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,-1),wf(:,28),G2tensor(:,345))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,0),wf(:,56),G2tensor(:,346))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,56),wf(:,0),G2tensor(:,347))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,0),wf(:,56),G2tensor(:,348))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,0),wf(:,59),G2tensor(:,349))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,59),wf(:,0),G2tensor(:,350))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,0),wf(:,59),G2tensor(:,351))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,0),wf(:,60),G2tensor(:,352))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,60),wf(:,0),G2tensor(:,353))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,0),wf(:,60),G2tensor(:,354))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,61),wf(:,62),G2tensor(:,355))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,62),wf(:,61),G2tensor(:,356))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,61),wf(:,62),G2tensor(:,357))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,-3),wf(:,74),G2tensor(:,358))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,74),wf(:,-3),G2tensor(:,359))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,-3),wf(:,74),G2tensor(:,360))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,-2),wf(:,83),G2tensor(:,361))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,83),wf(:,-2),G2tensor(:,362))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,-2),wf(:,83),G2tensor(:,363))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,85),Q(:,15),G3tensor(:,58))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,86),Q(:,15),G3tensor(:,59))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,87),Q(:,15),G3tensor(:,60))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,90),wf(:,91),G2tensor(:,364))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,91),wf(:,90),G2tensor(:,365))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,90),wf(:,91),G2tensor(:,366))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,-3),wf(:,103),G2tensor(:,367))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,103),wf(:,-3),G2tensor(:,368))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,-3),wf(:,103),G2tensor(:,369))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,104),wf(:,105),G2tensor(:,370))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,105),wf(:,104),G2tensor(:,371))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,104),wf(:,105),G2tensor(:,372))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,-3),wf(:,117),G2tensor(:,373))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,117),wf(:,-3),G2tensor(:,374))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,-3),wf(:,117),G2tensor(:,375))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,-2),wf(:,124),G2tensor(:,376))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,124),wf(:,-2),G2tensor(:,377))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,-2),wf(:,124),G2tensor(:,378))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,-2),wf(:,131),G2tensor(:,379))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,131),wf(:,-2),G2tensor(:,380))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,-2),wf(:,131),G2tensor(:,381))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,-1),wf(:,148),G2tensor(:,382))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,148),wf(:,-1),G2tensor(:,383))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,-1),wf(:,148),G2tensor(:,384))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,149),Q(:,15),G3tensor(:,61))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,150),Q(:,15),G3tensor(:,62))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,151),Q(:,15),G3tensor(:,63))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,-1),wf(:,160),G2tensor(:,385))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,160),wf(:,-1),G2tensor(:,386))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,-1),wf(:,160),G2tensor(:,387))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,-1),wf(:,167),G2tensor(:,388))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,167),wf(:,-1),G2tensor(:,389))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,-1),wf(:,167),G2tensor(:,390))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,178),Q(:,15),G3tensor(:,64))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,179),Q(:,15),G3tensor(:,65))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,180),Q(:,15),G3tensor(:,66))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1021),Q(:,15),G3tensor(:,67))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1022),Q(:,15),G3tensor(:,68))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1023),Q(:,15),G3tensor(:,69))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,0),wf(:,202),G2tensor(:,391))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,202),wf(:,0),G2tensor(:,392))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,0),wf(:,202),G2tensor(:,393))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,203),Q(:,15),G3tensor(:,70))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,204),Q(:,15),G3tensor(:,71))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,205),Q(:,15),G3tensor(:,72))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,0),wf(:,214),G2tensor(:,394))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,214),wf(:,0),G2tensor(:,395))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,0),wf(:,214),G2tensor(:,396))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,0),wf(:,221),G2tensor(:,397))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,77),wf(:,221),wf(:,0),G2tensor(:,398))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,77),wf(:,0),wf(:,221),G2tensor(:,399))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,232),Q(:,15),G3tensor(:,73))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,233),Q(:,15),G3tensor(:,74))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,234),Q(:,15),G3tensor(:,75))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1123),Q(:,15),G3tensor(:,76))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1124),Q(:,15),G3tensor(:,77))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1125),Q(:,15),G3tensor(:,78))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,250),Q(:,15),G3tensor(:,79))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,251),Q(:,15),G3tensor(:,80))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,252),Q(:,15),G3tensor(:,81))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1159),Q(:,15),G3tensor(:,82))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1160),Q(:,15),G3tensor(:,83))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1161),Q(:,15),G3tensor(:,84))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1168),Q(:,15),G3tensor(:,85))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1169),Q(:,15),G3tensor(:,86))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1170),Q(:,15),G3tensor(:,87))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,271),Q(:,15),G3tensor(:,88))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1321),Q(:,15),G3tensor(:,89))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1324),Q(:,15),G3tensor(:,90))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,274),Q(:,15),G3tensor(:,91))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1333),Q(:,15),G3tensor(:,92))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1336),Q(:,15),G3tensor(:,93))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,277),Q(:,15),G3tensor(:,94))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1345),Q(:,15),G3tensor(:,95))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1348),Q(:,15),G3tensor(:,96))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1357),Q(:,15),G3tensor(:,97))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1360),Q(:,15),G3tensor(:,98))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1369),Q(:,15),G3tensor(:,99))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1372),Q(:,15),G3tensor(:,100))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1381),Q(:,15),G3tensor(:,101))
  call check_last_UV_W(l_switch,G2(:,:,:,77),Q(:,48),wf(:,1382),Q(:,15),G3tensor(:,102))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,79),G1(:,:,:,239))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,239),wf(:,-1),wf(:,0),G1tensor(:,226))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,239),wf(:,0),wf(:,-1),G1tensor(:,227))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,239),wf(:,-1),wf(:,0),G1tensor(:,228))
  call check_last_UV_W(l_switch,G1(:,:,:,239),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,400))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,79),wf(:,-2),G1(:,:,:,240))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,240),wf(:,-1),wf(:,0),G1tensor(:,229))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,240),wf(:,0),wf(:,-1),G1tensor(:,230))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,240),wf(:,-1),wf(:,0),G1tensor(:,231))
  call check_last_UV_W(l_switch,G1(:,:,:,240),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,401))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,79),G1(:,:,:,241))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,241),wf(:,-1),wf(:,0),G1tensor(:,232))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,241),wf(:,0),wf(:,-1),G1tensor(:,233))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,241),wf(:,-1),wf(:,0),G1tensor(:,234))
  call check_last_UV_W(l_switch,G1(:,:,:,241),Q(:,60),wf(:,61),Q(:,3),G2tensor(:,402))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,19),G1(:,:,:,242))
  call check_last_UV_W(l_switch,G1(:,:,:,242),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,403))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,21),G1(:,:,:,243))
  call check_last_UV_W(l_switch,G1(:,:,:,243),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,404))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,22),G1(:,:,:,244))
  call check_last_UV_W(l_switch,G1(:,:,:,244),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,405))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,19),wf(:,-2),G1(:,:,:,245))
  call check_last_UV_W(l_switch,G1(:,:,:,245),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,406))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,21),wf(:,-2),G1(:,:,:,246))
  call check_last_UV_W(l_switch,G1(:,:,:,246),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,407))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,22),wf(:,-2),G1(:,:,:,247))
  call check_last_UV_W(l_switch,G1(:,:,:,247),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,408))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,19),G1(:,:,:,248))
  call check_last_UV_W(l_switch,G1(:,:,:,248),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,409))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,21),G1(:,:,:,249))
  call check_last_UV_W(l_switch,G1(:,:,:,249),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,410))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,22),G1(:,:,:,250))
  call check_last_UV_W(l_switch,G1(:,:,:,250),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,411))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,-3),Q(:,8),G2(:,:,:,78))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,-5),wf(:,-2),G2(:,:,:,79))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,79),wf(:,-1),wf(:,0),G2tensor(:,412))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,79),wf(:,0),wf(:,-1),G2tensor(:,413))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,79),wf(:,-1),wf(:,0),G2tensor(:,414))
  call check_last_UV_W(l_switch,G2(:,:,:,79),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,103))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,-2),wf(:,-5),G2(:,:,:,80))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,80),wf(:,-1),wf(:,0),G2tensor(:,415))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,80),wf(:,0),wf(:,-1),G2tensor(:,416))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,80),wf(:,-1),wf(:,0),G2tensor(:,417))
  call check_last_UV_W(l_switch,G2(:,:,:,80),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,104))
  call loop_GGG_G_23(G2(:,:,:,78),wf(:,-5),wf(:,-2),G2(:,:,:,81))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,81),wf(:,-1),wf(:,0),G2tensor(:,418))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,81),wf(:,0),wf(:,-1),G2tensor(:,419))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,81),wf(:,-1),wf(:,0),G2tensor(:,420))
  call check_last_UV_W(l_switch,G2(:,:,:,81),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,105))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,-5),wf(:,-1),G2(:,:,:,82))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,82),wf(:,-2),wf(:,0),G2tensor(:,421))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,82),wf(:,0),wf(:,-2),G2tensor(:,422))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,82),wf(:,-2),wf(:,0),G2tensor(:,423))
  call check_last_UV_W(l_switch,G2(:,:,:,82),Q(:,58),wf(:,90),Q(:,5),G3tensor(:,106))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,-1),wf(:,-5),G2(:,:,:,83))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,83),wf(:,-2),wf(:,0),G2tensor(:,424))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,83),wf(:,0),wf(:,-2),G2tensor(:,425))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,83),wf(:,-2),wf(:,0),G2tensor(:,426))
  call check_last_UV_W(l_switch,G2(:,:,:,83),Q(:,58),wf(:,90),Q(:,5),G3tensor(:,107))
  call loop_GGG_G_23(G2(:,:,:,78),wf(:,-5),wf(:,-1),G2(:,:,:,84))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,84),wf(:,-2),wf(:,0),G2tensor(:,427))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,84),wf(:,0),wf(:,-2),G2tensor(:,428))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,84),wf(:,-2),wf(:,0),G2tensor(:,429))
  call check_last_UV_W(l_switch,G2(:,:,:,84),Q(:,58),wf(:,90),Q(:,5),G3tensor(:,108))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,-5),wf(:,0),G2(:,:,:,85))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,85),wf(:,-2),wf(:,-1),G2tensor(:,430))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,85),wf(:,-1),wf(:,-2),G2tensor(:,431))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,85),wf(:,-2),wf(:,-1),G2tensor(:,432))
  call check_last_UV_W(l_switch,G2(:,:,:,85),Q(:,57),wf(:,105),Q(:,6),G3tensor(:,109))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,0),wf(:,-5),G2(:,:,:,86))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,86),wf(:,-2),wf(:,-1),G2tensor(:,433))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,86),wf(:,-1),wf(:,-2),G2tensor(:,434))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,86),wf(:,-2),wf(:,-1),G2tensor(:,435))
  call check_last_UV_W(l_switch,G2(:,:,:,86),Q(:,57),wf(:,105),Q(:,6),G3tensor(:,110))
  call loop_GGG_G_23(G2(:,:,:,78),wf(:,-5),wf(:,0),G2(:,:,:,87))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,87),wf(:,-2),wf(:,-1),G2tensor(:,436))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,87),wf(:,-1),wf(:,-2),G2tensor(:,437))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,87),wf(:,-2),wf(:,-1),G2tensor(:,438))
  call check_last_UV_W(l_switch,G2(:,:,:,87),Q(:,57),wf(:,105),Q(:,6),G3tensor(:,111))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,-5),wf(:,61),G2(:,:,:,88))
  call check_last_UV_W(l_switch,G2(:,:,:,88),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,112))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,61),wf(:,-5),G2(:,:,:,89))
  call check_last_UV_W(l_switch,G2(:,:,:,89),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,113))
  call loop_GGG_G_23(G2(:,:,:,78),wf(:,-5),wf(:,61),G2(:,:,:,90))
  call check_last_UV_W(l_switch,G2(:,:,:,90),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,114))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,-5),Q(:,32),G3(:,:,:,48))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,48),wf(:,-2),wf(:,61),G3tensor(:,115))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,48),wf(:,61),wf(:,-2),G3tensor(:,116))
  call check_last_GGG_G_23(l_switch,G3(:,:,:,48),wf(:,-2),wf(:,61),G3tensor(:,117))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,48),wf(:,-1),wf(:,90),G3tensor(:,118))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,48),wf(:,90),wf(:,-1),G3tensor(:,119))
  call check_last_GGG_G_23(l_switch,G3(:,:,:,48),wf(:,-1),wf(:,90),G3tensor(:,120))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,48),wf(:,0),wf(:,105),G3tensor(:,121))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,48),wf(:,105),wf(:,0),G3tensor(:,122))
  call check_last_GGG_G_23(l_switch,G3(:,:,:,48),wf(:,0),wf(:,105),G3tensor(:,123))
  call check_last_UV_W(l_switch,G3(:,:,:,48),Q(:,56),wf(:,1),Q(:,7),G4tensor(:,47))
  call check_last_UV_W(l_switch,G3(:,:,:,48),Q(:,56),wf(:,3),Q(:,7),G4tensor(:,48))
  call check_last_UV_W(l_switch,G3(:,:,:,48),Q(:,56),wf(:,4),Q(:,7),G4tensor(:,49))
  call check_last_UV_W(l_switch,G3(:,:,:,48),Q(:,56),wf(:,74),Q(:,7),G4tensor(:,50))
  call check_last_UV_W(l_switch,G3(:,:,:,48),Q(:,56),wf(:,103),Q(:,7),G4tensor(:,51))
  call check_last_UV_W(l_switch,G3(:,:,:,48),Q(:,56),wf(:,117),Q(:,7),G4tensor(:,52))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,-5),wf(:,90),G2(:,:,:,91))
  call check_last_UV_W(l_switch,G2(:,:,:,91),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,124))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,90),wf(:,-5),G2(:,:,:,92))
  call check_last_UV_W(l_switch,G2(:,:,:,92),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,125))
  call loop_GGG_G_23(G2(:,:,:,78),wf(:,-5),wf(:,90),G2(:,:,:,93))
  call check_last_UV_W(l_switch,G2(:,:,:,93),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,126))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,-5),wf(:,105),G2(:,:,:,94))
  call check_last_UV_W(l_switch,G2(:,:,:,94),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,127))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,105),wf(:,-5),G2(:,:,:,95))
  call check_last_UV_W(l_switch,G2(:,:,:,95),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,128))
  call loop_GGG_G_23(G2(:,:,:,78),wf(:,-5),wf(:,105),G2(:,:,:,96))
  call check_last_UV_W(l_switch,G2(:,:,:,96),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,129))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,-2),wf(:,113),G2(:,:,:,97))
  call check_last_UV_W(l_switch,G2(:,:,:,97),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,130))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,113),wf(:,-2),G2(:,:,:,98))
  call check_last_UV_W(l_switch,G2(:,:,:,98),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,131))
  call loop_GGG_G_23(G2(:,:,:,78),wf(:,-2),wf(:,113),G2(:,:,:,99))
  call check_last_UV_W(l_switch,G2(:,:,:,99),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,132))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,-2),wf(:,99),G2(:,:,:,100))
  call check_last_UV_W(l_switch,G2(:,:,:,100),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,133))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,99),wf(:,-2),G2(:,:,:,101))
  call check_last_UV_W(l_switch,G2(:,:,:,101),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,134))
  call loop_GGG_G_23(G2(:,:,:,78),wf(:,-2),wf(:,99),G2(:,:,:,102))
  call check_last_UV_W(l_switch,G2(:,:,:,102),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,135))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,-1),Q(:,2),G3(:,:,:,49))
  call loop_GGG_G_12(G3(:,:,:,49),wf(:,-5),wf(:,-2),G3(:,:,:,50))
  call check_last_UV_W(l_switch,G3(:,:,:,50),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,53))
  call loop_GGG_G_12(G3(:,:,:,49),wf(:,-2),wf(:,-5),G3(:,:,:,51))
  call check_last_UV_W(l_switch,G3(:,:,:,51),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,54))
  call loop_GGG_G_23(G3(:,:,:,49),wf(:,-5),wf(:,-2),G3(:,:,:,52))
  call check_last_UV_W(l_switch,G3(:,:,:,52),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,55))
  call loop_UV_W(G3(:,:,:,49),Q(:,26),wf(:,70),Q(:,36),G4(:,:,:,9))
  call check_last_UV_W(l_switch,G4(:,:,:,9),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,7))
  call loop_UV_W(G3(:,:,:,49),Q(:,26),wf(:,-2),Q(:,4),G4(:,:,:,10))
  call loop_UV_W(G4(:,:,:,10),Q(:,30),wf(:,-5),Q(:,32),G5(:,:,:,3))
  call check_last_UV_W(l_switch,G5(:,:,:,3),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,3))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,-1),wf(:,113),G2(:,:,:,103))
  call check_last_UV_W(l_switch,G2(:,:,:,103),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,136))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,113),wf(:,-1),G2(:,:,:,104))
  call check_last_UV_W(l_switch,G2(:,:,:,104),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,137))
  call loop_GGG_G_23(G2(:,:,:,78),wf(:,-1),wf(:,113),G2(:,:,:,105))
  call check_last_UV_W(l_switch,G2(:,:,:,105),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,138))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,-1),wf(:,70),G2(:,:,:,106))
  call check_last_UV_W(l_switch,G2(:,:,:,106),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,139))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,70),wf(:,-1),G2(:,:,:,107))
  call check_last_UV_W(l_switch,G2(:,:,:,107),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,140))
  call loop_GGG_G_23(G2(:,:,:,78),wf(:,-1),wf(:,70),G2(:,:,:,108))
  call check_last_UV_W(l_switch,G2(:,:,:,108),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,141))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,-2),Q(:,4),G3(:,:,:,53))
  call loop_GGG_G_12(G3(:,:,:,53),wf(:,-5),wf(:,-1),G3(:,:,:,54))
  call check_last_UV_W(l_switch,G3(:,:,:,54),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,56))
  call loop_GGG_G_12(G3(:,:,:,53),wf(:,-1),wf(:,-5),G3(:,:,:,55))
  call check_last_UV_W(l_switch,G3(:,:,:,55),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,57))
  call loop_GGG_G_23(G3(:,:,:,53),wf(:,-5),wf(:,-1),G3(:,:,:,56))
  call check_last_UV_W(l_switch,G3(:,:,:,56),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,58))
  call loop_GGG_G_12(G3(:,:,:,53),wf(:,-5),wf(:,0),G3(:,:,:,57))
  call check_last_UV_W(l_switch,G3(:,:,:,57),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,59))
  call loop_GGG_G_12(G3(:,:,:,53),wf(:,0),wf(:,-5),G3(:,:,:,58))
  call check_last_UV_W(l_switch,G3(:,:,:,58),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,60))
  call loop_GGG_G_23(G3(:,:,:,53),wf(:,-5),wf(:,0),G3(:,:,:,59))
  call check_last_UV_W(l_switch,G3(:,:,:,59),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,61))
  call loop_UV_W(G3(:,:,:,53),Q(:,28),wf(:,-5),Q(:,32),G4(:,:,:,11))
  call check_last_GGG_G_12(l_switch,G4(:,:,:,11),wf(:,-1),wf(:,0),G4tensor(:,62))
  call check_last_GGG_G_12(l_switch,G4(:,:,:,11),wf(:,0),wf(:,-1),G4tensor(:,63))
  call check_last_GGG_G_23(l_switch,G4(:,:,:,11),wf(:,-1),wf(:,0),G4tensor(:,64))
  call check_last_UV_W(l_switch,G4(:,:,:,11),Q(:,60),wf(:,61),Q(:,3),G5tensor(:,8))
  call loop_UV_W(G3(:,:,:,53),Q(:,28),wf(:,113),Q(:,33),G4(:,:,:,12))
  call check_last_UV_W(l_switch,G4(:,:,:,12),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,9))
  call loop_UV_W(G3(:,:,:,53),Q(:,28),wf(:,99),Q(:,34),G4(:,:,:,13))
  call check_last_UV_W(l_switch,G4(:,:,:,13),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,10))
  call loop_UV_W(G3(:,:,:,53),Q(:,28),wf(:,-1),Q(:,2),G4(:,:,:,14))
  call loop_UV_W(G4(:,:,:,14),Q(:,30),wf(:,-5),Q(:,32),G5(:,:,:,4))
  call check_last_UV_W(l_switch,G5(:,:,:,4),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,4))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,113),Q(:,33),G3(:,:,:,60))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,60),wf(:,-2),wf(:,-1),G3tensor(:,142))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,60),wf(:,-1),wf(:,-2),G3tensor(:,143))
  call check_last_GGG_G_23(l_switch,G3(:,:,:,60),wf(:,-2),wf(:,-1),G3tensor(:,144))
  call check_last_UV_W(l_switch,G3(:,:,:,60),Q(:,57),wf(:,105),Q(:,6),G4tensor(:,65))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,-2),wf(:,-1),G2(:,:,:,109))
  call loop_UV_W(G2(:,:,:,109),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,61))
  call check_last_UV_W(l_switch,G3(:,:,:,61),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,66))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,-1),wf(:,-2),G2(:,:,:,110))
  call loop_UV_W(G2(:,:,:,110),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,62))
  call check_last_UV_W(l_switch,G3(:,:,:,62),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,67))
  call loop_GGG_G_23(G2(:,:,:,78),wf(:,-2),wf(:,-1),G2(:,:,:,111))
  call loop_UV_W(G2(:,:,:,111),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,63))
  call check_last_UV_W(l_switch,G3(:,:,:,63),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,68))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,44),Q(:,38),G3(:,:,:,64))
  call check_last_UV_W(l_switch,G3(:,:,:,64),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,69))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,47),Q(:,38),G3(:,:,:,65))
  call check_last_UV_W(l_switch,G3(:,:,:,65),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,70))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,48),Q(:,38),G3(:,:,:,66))
  call check_last_UV_W(l_switch,G3(:,:,:,66),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,71))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,0),wf(:,99),G2(:,:,:,112))
  call check_last_UV_W(l_switch,G2(:,:,:,112),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,145))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,99),wf(:,0),G2(:,:,:,113))
  call check_last_UV_W(l_switch,G2(:,:,:,113),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,146))
  call loop_GGG_G_23(G2(:,:,:,78),wf(:,0),wf(:,99),G2(:,:,:,114))
  call check_last_UV_W(l_switch,G2(:,:,:,114),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,147))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,0),wf(:,70),G2(:,:,:,115))
  call check_last_UV_W(l_switch,G2(:,:,:,115),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,148))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,70),wf(:,0),G2(:,:,:,116))
  call check_last_UV_W(l_switch,G2(:,:,:,116),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,149))
  call loop_GGG_G_23(G2(:,:,:,78),wf(:,0),wf(:,70),G2(:,:,:,117))
  call check_last_UV_W(l_switch,G2(:,:,:,117),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,150))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,99),Q(:,34),G3(:,:,:,67))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,67),wf(:,-2),wf(:,0),G3tensor(:,151))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,67),wf(:,0),wf(:,-2),G3tensor(:,152))
  call check_last_GGG_G_23(l_switch,G3(:,:,:,67),wf(:,-2),wf(:,0),G3tensor(:,153))
  call check_last_UV_W(l_switch,G3(:,:,:,67),Q(:,58),wf(:,90),Q(:,5),G4tensor(:,72))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,-2),wf(:,0),G2(:,:,:,118))
  call loop_UV_W(G2(:,:,:,118),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,68))
  call check_last_UV_W(l_switch,G3(:,:,:,68),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,73))
  call loop_GGG_G_12(G2(:,:,:,78),wf(:,0),wf(:,-2),G2(:,:,:,119))
  call loop_UV_W(G2(:,:,:,119),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,69))
  call check_last_UV_W(l_switch,G3(:,:,:,69),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,74))
  call loop_GGG_G_23(G2(:,:,:,78),wf(:,-2),wf(:,0),G2(:,:,:,120))
  call loop_UV_W(G2(:,:,:,120),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,70))
  call check_last_UV_W(l_switch,G3(:,:,:,70),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,75))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,37),Q(:,37),G3(:,:,:,71))
  call check_last_UV_W(l_switch,G3(:,:,:,71),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,76))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,39),Q(:,37),G3(:,:,:,72))
  call check_last_UV_W(l_switch,G3(:,:,:,72),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,77))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,40),Q(:,37),G3(:,:,:,73))
  call check_last_UV_W(l_switch,G3(:,:,:,73),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,78))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,70),Q(:,36),G3(:,:,:,74))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,74),wf(:,-1),wf(:,0),G3tensor(:,154))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,74),wf(:,0),wf(:,-1),G3tensor(:,155))
  call check_last_GGG_G_23(l_switch,G3(:,:,:,74),wf(:,-1),wf(:,0),G3tensor(:,156))
  call check_last_UV_W(l_switch,G3(:,:,:,74),Q(:,60),wf(:,61),Q(:,3),G4tensor(:,79))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,19),Q(:,35),G3(:,:,:,75))
  call check_last_UV_W(l_switch,G3(:,:,:,75),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,80))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,21),Q(:,35),G3(:,:,:,76))
  call check_last_UV_W(l_switch,G3(:,:,:,76),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,81))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,22),Q(:,35),G3(:,:,:,77))
  call check_last_UV_W(l_switch,G3(:,:,:,77),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,82))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,89),Q(:,35),G3(:,:,:,78))
  call check_last_UV_W(l_switch,G3(:,:,:,78),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,83))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,153),Q(:,37),G3(:,:,:,79))
  call check_last_UV_W(l_switch,G3(:,:,:,79),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,84))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,90),Q(:,5),G3(:,:,:,80))
  call loop_UV_W(G3(:,:,:,80),Q(:,29),wf(:,-5),Q(:,32),G4(:,:,:,15))
  call check_last_UV_W(l_switch,G4(:,:,:,15),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,11))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,207),Q(:,38),G3(:,:,:,81))
  call check_last_UV_W(l_switch,G3(:,:,:,81),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,85))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,105),Q(:,6),G3(:,:,:,82))
  call loop_UV_W(G3(:,:,:,82),Q(:,30),wf(:,-5),Q(:,32),G4(:,:,:,16))
  call check_last_UV_W(l_switch,G4(:,:,:,16),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,12))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,140),Q(:,35),G3(:,:,:,83))
  call check_last_UV_W(l_switch,G3(:,:,:,83),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,86))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,176),Q(:,37),G3(:,:,:,84))
  call check_last_UV_W(l_switch,G3(:,:,:,84),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,87))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,141),Q(:,35),G3(:,:,:,85))
  call check_last_UV_W(l_switch,G3(:,:,:,85),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,88))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,230),Q(:,38),G3(:,:,:,86))
  call check_last_UV_W(l_switch,G3(:,:,:,86),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,89))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,177),Q(:,37),G3(:,:,:,87))
  call check_last_UV_W(l_switch,G3(:,:,:,87),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,90))
  call loop_UV_W(G2(:,:,:,78),Q(:,24),wf(:,231),Q(:,38),G3(:,:,:,88))
  call check_last_UV_W(l_switch,G3(:,:,:,88),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,91))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,19),Q(:,35),G2(:,:,:,121))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,121),wf(:,-3),wf(:,-2),G2tensor(:,439))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,121),wf(:,-2),wf(:,-3),G2tensor(:,440))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,121),wf(:,-3),wf(:,-2),G2tensor(:,441))
  call check_last_UV_W(l_switch,G2(:,:,:,121),Q(:,51),wf(:,62),Q(:,12),G3tensor(:,157))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,21),Q(:,35),G2(:,:,:,122))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,122),wf(:,-3),wf(:,-2),G2tensor(:,442))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,122),wf(:,-2),wf(:,-3),G2tensor(:,443))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,122),wf(:,-3),wf(:,-2),G2tensor(:,444))
  call check_last_UV_W(l_switch,G2(:,:,:,122),Q(:,51),wf(:,62),Q(:,12),G3tensor(:,158))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,22),Q(:,35),G2(:,:,:,123))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,123),wf(:,-3),wf(:,-2),G2tensor(:,445))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,123),wf(:,-2),wf(:,-3),G2tensor(:,446))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,123),wf(:,-3),wf(:,-2),G2tensor(:,447))
  call check_last_UV_W(l_switch,G2(:,:,:,123),Q(:,51),wf(:,62),Q(:,12),G3tensor(:,159))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,14),Q(:,44),G2(:,:,:,124))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,124),wf(:,-1),wf(:,0),G2tensor(:,448))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,124),wf(:,0),wf(:,-1),G2tensor(:,449))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,124),wf(:,-1),wf(:,0),G2tensor(:,450))
  call check_last_UV_W(l_switch,G2(:,:,:,124),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,160))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,17),Q(:,44),G2(:,:,:,125))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,125),wf(:,-1),wf(:,0),G2tensor(:,451))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,125),wf(:,0),wf(:,-1),G2tensor(:,452))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,125),wf(:,-1),wf(:,0),G2tensor(:,453))
  call check_last_UV_W(l_switch,G2(:,:,:,125),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,161))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,18),Q(:,44),G2(:,:,:,126))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,126),wf(:,-1),wf(:,0),G2tensor(:,454))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,126),wf(:,0),wf(:,-1),G2tensor(:,455))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,126),wf(:,-1),wf(:,0),G2tensor(:,456))
  call check_last_UV_W(l_switch,G2(:,:,:,126),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,162))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,70),G1(:,:,:,251))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,251),wf(:,-3),wf(:,0),G1tensor(:,235))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,251),wf(:,0),wf(:,-3),G1tensor(:,236))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,251),wf(:,-3),wf(:,0),G1tensor(:,237))
  call check_last_UV_W(l_switch,G1(:,:,:,251),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,457))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,70),wf(:,-1),G1(:,:,:,252))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,252),wf(:,-3),wf(:,0),G1tensor(:,238))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,252),wf(:,0),wf(:,-3),G1tensor(:,239))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,252),wf(:,-3),wf(:,0),G1tensor(:,240))
  call check_last_UV_W(l_switch,G1(:,:,:,252),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,458))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,70),G1(:,:,:,253))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,253),wf(:,-3),wf(:,0),G1tensor(:,241))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,253),wf(:,0),wf(:,-3),G1tensor(:,242))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,253),wf(:,-3),wf(:,0),G1tensor(:,243))
  call check_last_UV_W(l_switch,G1(:,:,:,253),Q(:,54),wf(:,104),Q(:,9),G2tensor(:,459))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,49),G1(:,:,:,254))
  call check_last_UV_W(l_switch,G1(:,:,:,254),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,460))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,51),G1(:,:,:,255))
  call check_last_UV_W(l_switch,G1(:,:,:,255),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,461))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,52),G1(:,:,:,256))
  call check_last_UV_W(l_switch,G1(:,:,:,256),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,462))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,49),wf(:,-1),G1(:,:,:,257))
  call check_last_UV_W(l_switch,G1(:,:,:,257),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,463))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,51),wf(:,-1),G1(:,:,:,258))
  call check_last_UV_W(l_switch,G1(:,:,:,258),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,464))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,52),wf(:,-1),G1(:,:,:,259))
  call check_last_UV_W(l_switch,G1(:,:,:,259),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,465))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,49),G1(:,:,:,260))
  call check_last_UV_W(l_switch,G1(:,:,:,260),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,466))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,51),G1(:,:,:,261))
  call check_last_UV_W(l_switch,G1(:,:,:,261),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,467))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,52),G1(:,:,:,262))
  call check_last_UV_W(l_switch,G1(:,:,:,262),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,468))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,70),G1(:,:,:,263))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,263),wf(:,-3),wf(:,-1),G1tensor(:,244))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,263),wf(:,-1),wf(:,-3),G1tensor(:,245))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,263),wf(:,-3),wf(:,-1),G1tensor(:,246))
  call check_last_UV_W(l_switch,G1(:,:,:,263),Q(:,53),wf(:,91),Q(:,10),G2tensor(:,469))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,70),wf(:,0),G1(:,:,:,264))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,264),wf(:,-3),wf(:,-1),G1tensor(:,247))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,264),wf(:,-1),wf(:,-3),G1tensor(:,248))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,264),wf(:,-3),wf(:,-1),G1tensor(:,249))
  call check_last_UV_W(l_switch,G1(:,:,:,264),Q(:,53),wf(:,91),Q(:,10),G2tensor(:,470))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,70),G1(:,:,:,265))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,265),wf(:,-3),wf(:,-1),G1tensor(:,250))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,265),wf(:,-1),wf(:,-3),G1tensor(:,251))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,265),wf(:,-3),wf(:,-1),G1tensor(:,252))
  call check_last_UV_W(l_switch,G1(:,:,:,265),Q(:,53),wf(:,91),Q(:,10),G2tensor(:,471))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,32),G1(:,:,:,266))
  call check_last_UV_W(l_switch,G1(:,:,:,266),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,472))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,32),wf(:,0),G1(:,:,:,267))
  call check_last_UV_W(l_switch,G1(:,:,:,267),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,473))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,32),G1(:,:,:,268))
  call check_last_UV_W(l_switch,G1(:,:,:,268),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,474))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,35),G1(:,:,:,269))
  call check_last_UV_W(l_switch,G1(:,:,:,269),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,475))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,35),wf(:,0),G1(:,:,:,270))
  call check_last_UV_W(l_switch,G1(:,:,:,270),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,476))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,35),G1(:,:,:,271))
  call check_last_UV_W(l_switch,G1(:,:,:,271),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,477))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,36),G1(:,:,:,272))
  call check_last_UV_W(l_switch,G1(:,:,:,272),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,478))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,36),wf(:,0),G1(:,:,:,273))
  call check_last_UV_W(l_switch,G1(:,:,:,273),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,479))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,36),G1(:,:,:,274))
  call check_last_UV_W(l_switch,G1(:,:,:,274),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,480))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,79),G1(:,:,:,275))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,275),wf(:,-2),wf(:,0),G1tensor(:,253))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,275),wf(:,0),wf(:,-2),G1tensor(:,254))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,275),wf(:,-2),wf(:,0),G1tensor(:,255))
  call check_last_UV_W(l_switch,G1(:,:,:,275),Q(:,58),wf(:,90),Q(:,5),G2tensor(:,481))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,79),wf(:,-1),G1(:,:,:,276))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,276),wf(:,-2),wf(:,0),G1tensor(:,256))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,276),wf(:,0),wf(:,-2),G1tensor(:,257))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,276),wf(:,-2),wf(:,0),G1tensor(:,258))
  call check_last_UV_W(l_switch,G1(:,:,:,276),Q(:,58),wf(:,90),Q(:,5),G2tensor(:,482))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,79),G1(:,:,:,277))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,277),wf(:,-2),wf(:,0),G1tensor(:,259))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,277),wf(:,0),wf(:,-2),G1tensor(:,260))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,277),wf(:,-2),wf(:,0),G1tensor(:,261))
  call check_last_UV_W(l_switch,G1(:,:,:,277),Q(:,58),wf(:,90),Q(:,5),G2tensor(:,483))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,37),G1(:,:,:,278))
  call check_last_UV_W(l_switch,G1(:,:,:,278),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,484))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,39),G1(:,:,:,279))
  call check_last_UV_W(l_switch,G1(:,:,:,279),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,485))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,40),G1(:,:,:,280))
  call check_last_UV_W(l_switch,G1(:,:,:,280),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,486))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,37),wf(:,-1),G1(:,:,:,281))
  call check_last_UV_W(l_switch,G1(:,:,:,281),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,487))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,39),wf(:,-1),G1(:,:,:,282))
  call check_last_UV_W(l_switch,G1(:,:,:,282),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,488))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,40),wf(:,-1),G1(:,:,:,283))
  call check_last_UV_W(l_switch,G1(:,:,:,283),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,489))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,37),G1(:,:,:,284))
  call check_last_UV_W(l_switch,G1(:,:,:,284),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,490))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,39),G1(:,:,:,285))
  call check_last_UV_W(l_switch,G1(:,:,:,285),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,491))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,40),G1(:,:,:,286))
  call check_last_UV_W(l_switch,G1(:,:,:,286),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,492))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,37),Q(:,37),G2(:,:,:,127))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,127),wf(:,-3),wf(:,-1),G2tensor(:,493))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,127),wf(:,-1),wf(:,-3),G2tensor(:,494))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,127),wf(:,-3),wf(:,-1),G2tensor(:,495))
  call check_last_UV_W(l_switch,G2(:,:,:,127),Q(:,53),wf(:,91),Q(:,10),G3tensor(:,163))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,39),Q(:,37),G2(:,:,:,128))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,128),wf(:,-3),wf(:,-1),G2tensor(:,496))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,128),wf(:,-1),wf(:,-3),G2tensor(:,497))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,128),wf(:,-3),wf(:,-1),G2tensor(:,498))
  call check_last_UV_W(l_switch,G2(:,:,:,128),Q(:,53),wf(:,91),Q(:,10),G3tensor(:,164))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,40),Q(:,37),G2(:,:,:,129))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,129),wf(:,-3),wf(:,-1),G2tensor(:,499))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,129),wf(:,-1),wf(:,-3),G2tensor(:,500))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,129),wf(:,-3),wf(:,-1),G2tensor(:,501))
  call check_last_UV_W(l_switch,G2(:,:,:,129),Q(:,53),wf(:,91),Q(:,10),G3tensor(:,165))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,32),Q(:,42),G2(:,:,:,130))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,130),wf(:,-2),wf(:,0),G2tensor(:,502))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,130),wf(:,0),wf(:,-2),G2tensor(:,503))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,130),wf(:,-2),wf(:,0),G2tensor(:,504))
  call check_last_UV_W(l_switch,G2(:,:,:,130),Q(:,58),wf(:,90),Q(:,5),G3tensor(:,166))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,35),Q(:,42),G2(:,:,:,131))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,131),wf(:,-2),wf(:,0),G2tensor(:,505))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,131),wf(:,0),wf(:,-2),G2tensor(:,506))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,131),wf(:,-2),wf(:,0),G2tensor(:,507))
  call check_last_UV_W(l_switch,G2(:,:,:,131),Q(:,58),wf(:,90),Q(:,5),G3tensor(:,167))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,36),Q(:,42),G2(:,:,:,132))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,132),wf(:,-2),wf(:,0),G2tensor(:,508))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,132),wf(:,0),wf(:,-2),G2tensor(:,509))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,132),wf(:,-2),wf(:,0),G2tensor(:,510))
  call check_last_UV_W(l_switch,G2(:,:,:,132),Q(:,58),wf(:,90),Q(:,5),G3tensor(:,168))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,79),G1(:,:,:,287))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,287),wf(:,-2),wf(:,-1),G1tensor(:,262))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,287),wf(:,-1),wf(:,-2),G1tensor(:,263))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,287),wf(:,-2),wf(:,-1),G1tensor(:,264))
  call check_last_UV_W(l_switch,G1(:,:,:,287),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,511))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,79),wf(:,0),G1(:,:,:,288))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,288),wf(:,-2),wf(:,-1),G1tensor(:,265))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,288),wf(:,-1),wf(:,-2),G1tensor(:,266))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,288),wf(:,-2),wf(:,-1),G1tensor(:,267))
  call check_last_UV_W(l_switch,G1(:,:,:,288),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,512))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,79),G1(:,:,:,289))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,289),wf(:,-2),wf(:,-1),G1tensor(:,268))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,289),wf(:,-1),wf(:,-2),G1tensor(:,269))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,289),wf(:,-2),wf(:,-1),G1tensor(:,270))
  call check_last_UV_W(l_switch,G1(:,:,:,289),Q(:,57),wf(:,105),Q(:,6),G2tensor(:,513))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,44),G1(:,:,:,290))
  call check_last_UV_W(l_switch,G1(:,:,:,290),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,514))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,44),wf(:,0),G1(:,:,:,291))
  call check_last_UV_W(l_switch,G1(:,:,:,291),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,515))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,44),G1(:,:,:,292))
  call check_last_UV_W(l_switch,G1(:,:,:,292),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,516))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,47),G1(:,:,:,293))
  call check_last_UV_W(l_switch,G1(:,:,:,293),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,517))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,47),wf(:,0),G1(:,:,:,294))
  call check_last_UV_W(l_switch,G1(:,:,:,294),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,518))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,47),G1(:,:,:,295))
  call check_last_UV_W(l_switch,G1(:,:,:,295),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,519))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,48),G1(:,:,:,296))
  call check_last_UV_W(l_switch,G1(:,:,:,296),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,520))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,48),wf(:,0),G1(:,:,:,297))
  call check_last_UV_W(l_switch,G1(:,:,:,297),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,521))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,48),G1(:,:,:,298))
  call check_last_UV_W(l_switch,G1(:,:,:,298),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,522))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,49),Q(:,41),G2(:,:,:,133))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,133),wf(:,-2),wf(:,-1),G2tensor(:,523))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,133),wf(:,-1),wf(:,-2),G2tensor(:,524))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,133),wf(:,-2),wf(:,-1),G2tensor(:,525))
  call check_last_UV_W(l_switch,G2(:,:,:,133),Q(:,57),wf(:,105),Q(:,6),G3tensor(:,169))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,51),Q(:,41),G2(:,:,:,134))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,134),wf(:,-2),wf(:,-1),G2tensor(:,526))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,134),wf(:,-1),wf(:,-2),G2tensor(:,527))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,134),wf(:,-2),wf(:,-1),G2tensor(:,528))
  call check_last_UV_W(l_switch,G2(:,:,:,134),Q(:,57),wf(:,105),Q(:,6),G3tensor(:,170))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,52),Q(:,41),G2(:,:,:,135))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,135),wf(:,-2),wf(:,-1),G2tensor(:,529))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,135),wf(:,-1),wf(:,-2),G2tensor(:,530))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,135),wf(:,-2),wf(:,-1),G2tensor(:,531))
  call check_last_UV_W(l_switch,G2(:,:,:,135),Q(:,57),wf(:,105),Q(:,6),G3tensor(:,171))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,44),Q(:,38),G2(:,:,:,136))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,136),wf(:,-3),wf(:,0),G2tensor(:,532))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,136),wf(:,0),wf(:,-3),G2tensor(:,533))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,136),wf(:,-3),wf(:,0),G2tensor(:,534))
  call check_last_UV_W(l_switch,G2(:,:,:,136),Q(:,54),wf(:,104),Q(:,9),G3tensor(:,172))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,47),Q(:,38),G2(:,:,:,137))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,137),wf(:,-3),wf(:,0),G2tensor(:,535))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,137),wf(:,0),wf(:,-3),G2tensor(:,536))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,137),wf(:,-3),wf(:,0),G2tensor(:,537))
  call check_last_UV_W(l_switch,G2(:,:,:,137),Q(:,54),wf(:,104),Q(:,9),G3tensor(:,173))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,48),Q(:,38),G2(:,:,:,138))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,138),wf(:,-3),wf(:,0),G2tensor(:,538))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,138),wf(:,0),wf(:,-3),G2tensor(:,539))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,138),wf(:,-3),wf(:,0),G2tensor(:,540))
  call check_last_UV_W(l_switch,G2(:,:,:,138),Q(:,54),wf(:,104),Q(:,9),G3tensor(:,174))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,61),wf(:,70),G1(:,:,:,299))
  call check_last_UV_W(l_switch,G1(:,:,:,299),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,541))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,70),wf(:,61),G1(:,:,:,300))
  call check_last_UV_W(l_switch,G1(:,:,:,300),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,542))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,61),wf(:,70),G1(:,:,:,301))
  call check_last_UV_W(l_switch,G1(:,:,:,301),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,543))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,61),wf(:,79),G1(:,:,:,302))
  call check_last_UV_W(l_switch,G1(:,:,:,302),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,544))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,79),wf(:,61),G1(:,:,:,303))
  call check_last_UV_W(l_switch,G1(:,:,:,303),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,545))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,61),wf(:,79),G1(:,:,:,304))
  call check_last_UV_W(l_switch,G1(:,:,:,304),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,546))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,74),G1(:,:,:,305))
  call check_last_UV_W(l_switch,G1(:,:,:,305),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,547))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,74),wf(:,-5),G1(:,:,:,306))
  call check_last_UV_W(l_switch,G1(:,:,:,306),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,548))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,74),G1(:,:,:,307))
  call check_last_UV_W(l_switch,G1(:,:,:,307),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,549))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,83),G1(:,:,:,308))
  call check_last_UV_W(l_switch,G1(:,:,:,308),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,550))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,83),wf(:,-5),G1(:,:,:,309))
  call check_last_UV_W(l_switch,G1(:,:,:,309),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,551))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,83),G1(:,:,:,310))
  call check_last_UV_W(l_switch,G1(:,:,:,310),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,552))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,70),Q(:,36),G2(:,:,:,139))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,139),wf(:,-3),wf(:,61),G2tensor(:,553))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,139),wf(:,61),wf(:,-3),G2tensor(:,554))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,139),wf(:,-3),wf(:,61),G2tensor(:,555))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,139),wf(:,-1),wf(:,104),G2tensor(:,556))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,139),wf(:,104),wf(:,-1),G2tensor(:,557))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,139),wf(:,-1),wf(:,104),G2tensor(:,558))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,139),wf(:,0),wf(:,91),G2tensor(:,559))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,139),wf(:,91),wf(:,0),G2tensor(:,560))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,139),wf(:,0),wf(:,91),G2tensor(:,561))
  call check_last_UV_W(l_switch,G2(:,:,:,139),Q(:,52),wf(:,7),Q(:,11),G3tensor(:,175))
  call check_last_UV_W(l_switch,G2(:,:,:,139),Q(:,52),wf(:,9),Q(:,11),G3tensor(:,176))
  call check_last_UV_W(l_switch,G2(:,:,:,139),Q(:,52),wf(:,10),Q(:,11),G3tensor(:,177))
  call check_last_UV_W(l_switch,G2(:,:,:,139),Q(:,52),wf(:,83),Q(:,11),G3tensor(:,178))
  call check_last_UV_W(l_switch,G2(:,:,:,139),Q(:,52),wf(:,124),Q(:,11),G3tensor(:,179))
  call check_last_UV_W(l_switch,G2(:,:,:,139),Q(:,52),wf(:,131),Q(:,11),G3tensor(:,180))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,89),G1(:,:,:,311))
  call check_last_UV_W(l_switch,G1(:,:,:,311),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,562))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,89),wf(:,-3),G1(:,:,:,312))
  call check_last_UV_W(l_switch,G1(:,:,:,312),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,563))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,89),G1(:,:,:,313))
  call check_last_UV_W(l_switch,G1(:,:,:,313),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,564))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,61),G1(:,:,:,314))
  call loop_UV_W(G1(:,:,:,314),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,140))
  call check_last_UV_W(l_switch,G2(:,:,:,140),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,181))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,61),wf(:,-3),G1(:,:,:,315))
  call loop_UV_W(G1(:,:,:,315),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,141))
  call check_last_UV_W(l_switch,G2(:,:,:,141),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,182))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,61),G1(:,:,:,316))
  call loop_UV_W(G1(:,:,:,316),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,142))
  call check_last_UV_W(l_switch,G2(:,:,:,142),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,183))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,67),Q(:,43),G2(:,:,:,143))
  call check_last_UV_W(l_switch,G2(:,:,:,143),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,184))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,68),Q(:,43),G2(:,:,:,144))
  call check_last_UV_W(l_switch,G2(:,:,:,144),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,185))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,69),Q(:,43),G2(:,:,:,145))
  call check_last_UV_W(l_switch,G2(:,:,:,145),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,186))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,79),Q(:,40),G2(:,:,:,146))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,146),wf(:,-2),wf(:,61),G2tensor(:,565))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,146),wf(:,61),wf(:,-2),G2tensor(:,566))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,146),wf(:,-2),wf(:,61),G2tensor(:,567))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,146),wf(:,-1),wf(:,90),G2tensor(:,568))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,146),wf(:,90),wf(:,-1),G2tensor(:,569))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,146),wf(:,-1),wf(:,90),G2tensor(:,570))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,146),wf(:,0),wf(:,105),G2tensor(:,571))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,146),wf(:,105),wf(:,0),G2tensor(:,572))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,146),wf(:,0),wf(:,105),G2tensor(:,573))
  call check_last_UV_W(l_switch,G2(:,:,:,146),Q(:,56),wf(:,1),Q(:,7),G3tensor(:,187))
  call check_last_UV_W(l_switch,G2(:,:,:,146),Q(:,56),wf(:,3),Q(:,7),G3tensor(:,188))
  call check_last_UV_W(l_switch,G2(:,:,:,146),Q(:,56),wf(:,4),Q(:,7),G3tensor(:,189))
  call check_last_UV_W(l_switch,G2(:,:,:,146),Q(:,56),wf(:,74),Q(:,7),G3tensor(:,190))
  call check_last_UV_W(l_switch,G2(:,:,:,146),Q(:,56),wf(:,103),Q(:,7),G3tensor(:,191))
  call check_last_UV_W(l_switch,G2(:,:,:,146),Q(:,56),wf(:,117),Q(:,7),G3tensor(:,192))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,89),G1(:,:,:,317))
  call check_last_UV_W(l_switch,G1(:,:,:,317),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,574))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,89),wf(:,-2),G1(:,:,:,318))
  call check_last_UV_W(l_switch,G1(:,:,:,318),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,575))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,89),G1(:,:,:,319))
  call check_last_UV_W(l_switch,G1(:,:,:,319),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,576))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,76),Q(:,39),G2(:,:,:,147))
  call check_last_UV_W(l_switch,G2(:,:,:,147),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,193))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,77),Q(:,39),G2(:,:,:,148))
  call check_last_UV_W(l_switch,G2(:,:,:,148),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,194))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,78),Q(:,39),G2(:,:,:,149))
  call check_last_UV_W(l_switch,G2(:,:,:,149),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,195))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,89),Q(:,35),G2(:,:,:,150))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,150),wf(:,-3),wf(:,-2),G2tensor(:,577))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,150),wf(:,-2),wf(:,-3),G2tensor(:,578))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,150),wf(:,-3),wf(:,-2),G2tensor(:,579))
  call check_last_UV_W(l_switch,G2(:,:,:,150),Q(:,51),wf(:,62),Q(:,12),G3tensor(:,196))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,90),wf(:,99),G1(:,:,:,320))
  call check_last_UV_W(l_switch,G1(:,:,:,320),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,580))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,99),wf(:,90),G1(:,:,:,321))
  call check_last_UV_W(l_switch,G1(:,:,:,321),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,581))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,90),wf(:,99),G1(:,:,:,322))
  call check_last_UV_W(l_switch,G1(:,:,:,322),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,582))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,79),wf(:,90),G1(:,:,:,323))
  call check_last_UV_W(l_switch,G1(:,:,:,323),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,583))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,90),wf(:,79),G1(:,:,:,324))
  call check_last_UV_W(l_switch,G1(:,:,:,324),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,584))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,79),wf(:,90),G1(:,:,:,325))
  call check_last_UV_W(l_switch,G1(:,:,:,325),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,585))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,103),G1(:,:,:,326))
  call check_last_UV_W(l_switch,G1(:,:,:,326),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,586))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,103),wf(:,-5),G1(:,:,:,327))
  call check_last_UV_W(l_switch,G1(:,:,:,327),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,587))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,103),G1(:,:,:,328))
  call check_last_UV_W(l_switch,G1(:,:,:,328),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,588))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,148),G1(:,:,:,329))
  call check_last_UV_W(l_switch,G1(:,:,:,329),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,589))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,148),wf(:,-5),G1(:,:,:,330))
  call check_last_UV_W(l_switch,G1(:,:,:,330),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,590))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,148),G1(:,:,:,331))
  call check_last_UV_W(l_switch,G1(:,:,:,331),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,591))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,99),Q(:,34),G2(:,:,:,151))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,151),wf(:,-3),wf(:,90),G2tensor(:,592))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,151),wf(:,90),wf(:,-3),G2tensor(:,593))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,151),wf(:,-3),wf(:,90),G2tensor(:,594))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,151),wf(:,-2),wf(:,104),G2tensor(:,595))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,151),wf(:,104),wf(:,-2),G2tensor(:,596))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,151),wf(:,-2),wf(:,104),G2tensor(:,597))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,151),wf(:,0),wf(:,62),G2tensor(:,598))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,151),wf(:,62),wf(:,0),G2tensor(:,599))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,151),wf(:,0),wf(:,62),G2tensor(:,600))
  call check_last_UV_W(l_switch,G2(:,:,:,151),Q(:,50),wf(:,25),Q(:,13),G3tensor(:,197))
  call check_last_UV_W(l_switch,G2(:,:,:,151),Q(:,50),wf(:,27),Q(:,13),G3tensor(:,198))
  call check_last_UV_W(l_switch,G2(:,:,:,151),Q(:,50),wf(:,28),Q(:,13),G3tensor(:,199))
  call check_last_UV_W(l_switch,G2(:,:,:,151),Q(:,50),wf(:,148),Q(:,13),G3tensor(:,200))
  call check_last_UV_W(l_switch,G2(:,:,:,151),Q(:,50),wf(:,160),Q(:,13),G3tensor(:,201))
  call check_last_UV_W(l_switch,G2(:,:,:,151),Q(:,50),wf(:,167),Q(:,13),G3tensor(:,202))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,153),G1(:,:,:,332))
  call check_last_UV_W(l_switch,G1(:,:,:,332),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,601))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,153),wf(:,-3),G1(:,:,:,333))
  call check_last_UV_W(l_switch,G1(:,:,:,333),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,602))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,153),G1(:,:,:,334))
  call check_last_UV_W(l_switch,G1(:,:,:,334),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,603))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,90),G1(:,:,:,335))
  call loop_UV_W(G1(:,:,:,335),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,152))
  call check_last_UV_W(l_switch,G2(:,:,:,152),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,203))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,90),wf(:,-3),G1(:,:,:,336))
  call loop_UV_W(G1(:,:,:,336),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,153))
  call check_last_UV_W(l_switch,G2(:,:,:,153),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,204))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,90),G1(:,:,:,337))
  call loop_UV_W(G1(:,:,:,337),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,154))
  call check_last_UV_W(l_switch,G2(:,:,:,154),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,205))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,90),Q(:,5),G2(:,:,:,155))
  call loop_GGG_G_12(G2(:,:,:,155),wf(:,-5),wf(:,-3),G2(:,:,:,156))
  call check_last_UV_W(l_switch,G2(:,:,:,156),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,206))
  call loop_GGG_G_12(G2(:,:,:,155),wf(:,-3),wf(:,-5),G2(:,:,:,157))
  call check_last_UV_W(l_switch,G2(:,:,:,157),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,207))
  call loop_GGG_G_23(G2(:,:,:,155),wf(:,-5),wf(:,-3),G2(:,:,:,158))
  call check_last_UV_W(l_switch,G2(:,:,:,158),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,208))
  call loop_UV_W(G2(:,:,:,155),Q(:,21),wf(:,79),Q(:,40),G3(:,:,:,89))
  call check_last_UV_W(l_switch,G3(:,:,:,89),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,92))
  call loop_UV_W(G2(:,:,:,155),Q(:,21),wf(:,-3),Q(:,8),G3(:,:,:,90))
  call loop_UV_W(G3(:,:,:,90),Q(:,29),wf(:,-5),Q(:,32),G4(:,:,:,17))
  call check_last_UV_W(l_switch,G4(:,:,:,17),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,13))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,96),Q(:,45),G2(:,:,:,159))
  call check_last_UV_W(l_switch,G2(:,:,:,159),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,209))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,97),Q(:,45),G2(:,:,:,160))
  call check_last_UV_W(l_switch,G2(:,:,:,160),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,210))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,98),Q(:,45),G2(:,:,:,161))
  call check_last_UV_W(l_switch,G2(:,:,:,161),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,211))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,105),wf(:,113),G1(:,:,:,338))
  call check_last_UV_W(l_switch,G1(:,:,:,338),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,604))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,113),wf(:,105),G1(:,:,:,339))
  call check_last_UV_W(l_switch,G1(:,:,:,339),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,605))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,105),wf(:,113),G1(:,:,:,340))
  call check_last_UV_W(l_switch,G1(:,:,:,340),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,606))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,79),wf(:,105),G1(:,:,:,341))
  call check_last_UV_W(l_switch,G1(:,:,:,341),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,607))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,105),wf(:,79),G1(:,:,:,342))
  call check_last_UV_W(l_switch,G1(:,:,:,342),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,608))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,79),wf(:,105),G1(:,:,:,343))
  call check_last_UV_W(l_switch,G1(:,:,:,343),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,609))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,117),G1(:,:,:,344))
  call check_last_UV_W(l_switch,G1(:,:,:,344),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,610))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,117),wf(:,-5),G1(:,:,:,345))
  call check_last_UV_W(l_switch,G1(:,:,:,345),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,611))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,117),G1(:,:,:,346))
  call check_last_UV_W(l_switch,G1(:,:,:,346),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,612))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,202),G1(:,:,:,347))
  call check_last_UV_W(l_switch,G1(:,:,:,347),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,613))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,202),wf(:,-5),G1(:,:,:,348))
  call check_last_UV_W(l_switch,G1(:,:,:,348),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,614))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,202),G1(:,:,:,349))
  call check_last_UV_W(l_switch,G1(:,:,:,349),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,615))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,113),Q(:,33),G2(:,:,:,162))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,162),wf(:,-3),wf(:,105),G2tensor(:,616))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,162),wf(:,105),wf(:,-3),G2tensor(:,617))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,162),wf(:,-3),wf(:,105),G2tensor(:,618))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,162),wf(:,-2),wf(:,91),G2tensor(:,619))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,162),wf(:,91),wf(:,-2),G2tensor(:,620))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,162),wf(:,-2),wf(:,91),G2tensor(:,621))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,162),wf(:,-1),wf(:,62),G2tensor(:,622))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,162),wf(:,62),wf(:,-1),G2tensor(:,623))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,162),wf(:,-1),wf(:,62),G2tensor(:,624))
  call check_last_UV_W(l_switch,G2(:,:,:,162),Q(:,49),wf(:,56),Q(:,14),G3tensor(:,212))
  call check_last_UV_W(l_switch,G2(:,:,:,162),Q(:,49),wf(:,59),Q(:,14),G3tensor(:,213))
  call check_last_UV_W(l_switch,G2(:,:,:,162),Q(:,49),wf(:,60),Q(:,14),G3tensor(:,214))
  call check_last_UV_W(l_switch,G2(:,:,:,162),Q(:,49),wf(:,202),Q(:,14),G3tensor(:,215))
  call check_last_UV_W(l_switch,G2(:,:,:,162),Q(:,49),wf(:,214),Q(:,14),G3tensor(:,216))
  call check_last_UV_W(l_switch,G2(:,:,:,162),Q(:,49),wf(:,221),Q(:,14),G3tensor(:,217))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,207),G1(:,:,:,350))
  call check_last_UV_W(l_switch,G1(:,:,:,350),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,625))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,207),wf(:,-3),G1(:,:,:,351))
  call check_last_UV_W(l_switch,G1(:,:,:,351),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,626))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,207),G1(:,:,:,352))
  call check_last_UV_W(l_switch,G1(:,:,:,352),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,627))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,105),G1(:,:,:,353))
  call loop_UV_W(G1(:,:,:,353),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,163))
  call check_last_UV_W(l_switch,G2(:,:,:,163),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,218))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,105),wf(:,-3),G1(:,:,:,354))
  call loop_UV_W(G1(:,:,:,354),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,164))
  call check_last_UV_W(l_switch,G2(:,:,:,164),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,219))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,105),G1(:,:,:,355))
  call loop_UV_W(G1(:,:,:,355),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,165))
  call check_last_UV_W(l_switch,G2(:,:,:,165),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,220))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,105),Q(:,6),G2(:,:,:,166))
  call loop_GGG_G_12(G2(:,:,:,166),wf(:,-5),wf(:,-3),G2(:,:,:,167))
  call check_last_UV_W(l_switch,G2(:,:,:,167),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,221))
  call loop_GGG_G_12(G2(:,:,:,166),wf(:,-3),wf(:,-5),G2(:,:,:,168))
  call check_last_UV_W(l_switch,G2(:,:,:,168),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,222))
  call loop_GGG_G_23(G2(:,:,:,166),wf(:,-5),wf(:,-3),G2(:,:,:,169))
  call check_last_UV_W(l_switch,G2(:,:,:,169),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,223))
  call loop_UV_W(G2(:,:,:,166),Q(:,22),wf(:,79),Q(:,40),G3(:,:,:,91))
  call check_last_UV_W(l_switch,G3(:,:,:,91),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,93))
  call loop_UV_W(G2(:,:,:,166),Q(:,22),wf(:,-3),Q(:,8),G3(:,:,:,92))
  call loop_UV_W(G3(:,:,:,92),Q(:,30),wf(:,-5),Q(:,32),G4(:,:,:,18))
  call check_last_UV_W(l_switch,G4(:,:,:,18),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,14))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,863),Q(:,46),G2(:,:,:,170))
  call check_last_UV_W(l_switch,G2(:,:,:,170),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,224))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,865),Q(:,46),G2(:,:,:,171))
  call check_last_UV_W(l_switch,G2(:,:,:,171),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,225))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,866),Q(:,46),G2(:,:,:,172))
  call check_last_UV_W(l_switch,G2(:,:,:,172),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,226))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,99),wf(:,104),G1(:,:,:,356))
  call check_last_UV_W(l_switch,G1(:,:,:,356),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,628))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,104),wf(:,99),G1(:,:,:,357))
  call check_last_UV_W(l_switch,G1(:,:,:,357),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,629))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,99),wf(:,104),G1(:,:,:,358))
  call check_last_UV_W(l_switch,G1(:,:,:,358),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,630))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,70),wf(:,104),G1(:,:,:,359))
  call check_last_UV_W(l_switch,G1(:,:,:,359),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,631))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,104),wf(:,70),G1(:,:,:,360))
  call check_last_UV_W(l_switch,G1(:,:,:,360),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,632))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,70),wf(:,104),G1(:,:,:,361))
  call check_last_UV_W(l_switch,G1(:,:,:,361),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,633))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,124),G1(:,:,:,362))
  call check_last_UV_W(l_switch,G1(:,:,:,362),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,634))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,124),wf(:,-5),G1(:,:,:,363))
  call check_last_UV_W(l_switch,G1(:,:,:,363),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,635))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,124),G1(:,:,:,364))
  call check_last_UV_W(l_switch,G1(:,:,:,364),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,636))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,160),G1(:,:,:,365))
  call check_last_UV_W(l_switch,G1(:,:,:,365),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,637))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,160),wf(:,-5),G1(:,:,:,366))
  call check_last_UV_W(l_switch,G1(:,:,:,366),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,638))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,160),G1(:,:,:,367))
  call check_last_UV_W(l_switch,G1(:,:,:,367),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,639))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,91),wf(:,113),G1(:,:,:,368))
  call check_last_UV_W(l_switch,G1(:,:,:,368),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,640))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,113),wf(:,91),G1(:,:,:,369))
  call check_last_UV_W(l_switch,G1(:,:,:,369),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,641))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,91),wf(:,113),G1(:,:,:,370))
  call check_last_UV_W(l_switch,G1(:,:,:,370),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,642))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,70),wf(:,91),G1(:,:,:,371))
  call check_last_UV_W(l_switch,G1(:,:,:,371),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,643))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,91),wf(:,70),G1(:,:,:,372))
  call check_last_UV_W(l_switch,G1(:,:,:,372),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,644))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,70),wf(:,91),G1(:,:,:,373))
  call check_last_UV_W(l_switch,G1(:,:,:,373),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,645))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,131),G1(:,:,:,374))
  call check_last_UV_W(l_switch,G1(:,:,:,374),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,646))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,131),wf(:,-5),G1(:,:,:,375))
  call check_last_UV_W(l_switch,G1(:,:,:,375),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,647))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,131),G1(:,:,:,376))
  call check_last_UV_W(l_switch,G1(:,:,:,376),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,648))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,214),G1(:,:,:,377))
  call check_last_UV_W(l_switch,G1(:,:,:,377),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,649))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,214),wf(:,-5),G1(:,:,:,378))
  call check_last_UV_W(l_switch,G1(:,:,:,378),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,650))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,214),G1(:,:,:,379))
  call check_last_UV_W(l_switch,G1(:,:,:,379),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,651))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,62),wf(:,113),G1(:,:,:,380))
  call check_last_UV_W(l_switch,G1(:,:,:,380),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,652))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,113),wf(:,62),G1(:,:,:,381))
  call check_last_UV_W(l_switch,G1(:,:,:,381),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,653))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,62),wf(:,113),G1(:,:,:,382))
  call check_last_UV_W(l_switch,G1(:,:,:,382),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,654))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,62),wf(:,99),G1(:,:,:,383))
  call check_last_UV_W(l_switch,G1(:,:,:,383),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,655))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,99),wf(:,62),G1(:,:,:,384))
  call check_last_UV_W(l_switch,G1(:,:,:,384),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,656))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,62),wf(:,99),G1(:,:,:,385))
  call check_last_UV_W(l_switch,G1(:,:,:,385),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,657))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,167),G1(:,:,:,386))
  call check_last_UV_W(l_switch,G1(:,:,:,386),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,658))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,167),wf(:,-5),G1(:,:,:,387))
  call check_last_UV_W(l_switch,G1(:,:,:,387),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,659))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,167),G1(:,:,:,388))
  call check_last_UV_W(l_switch,G1(:,:,:,388),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,660))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-5),wf(:,221),G1(:,:,:,389))
  call check_last_UV_W(l_switch,G1(:,:,:,389),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,661))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,221),wf(:,-5),G1(:,:,:,390))
  call check_last_UV_W(l_switch,G1(:,:,:,390),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,662))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-5),wf(:,221),G1(:,:,:,391))
  call check_last_UV_W(l_switch,G1(:,:,:,391),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,663))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,-1),Q(:,2),G2(:,:,:,173))
  call loop_GGG_G_12(G2(:,:,:,173),wf(:,-5),wf(:,62),G2(:,:,:,174))
  call check_last_UV_W(l_switch,G2(:,:,:,174),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,227))
  call loop_GGG_G_12(G2(:,:,:,173),wf(:,62),wf(:,-5),G2(:,:,:,175))
  call check_last_UV_W(l_switch,G2(:,:,:,175),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,228))
  call loop_GGG_G_23(G2(:,:,:,173),wf(:,-5),wf(:,62),G2(:,:,:,176))
  call check_last_UV_W(l_switch,G2(:,:,:,176),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,229))
  call loop_GGG_G_12(G2(:,:,:,173),wf(:,-3),wf(:,70),G2(:,:,:,177))
  call check_last_UV_W(l_switch,G2(:,:,:,177),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,230))
  call loop_GGG_G_12(G2(:,:,:,173),wf(:,70),wf(:,-3),G2(:,:,:,178))
  call check_last_UV_W(l_switch,G2(:,:,:,178),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,231))
  call loop_GGG_G_23(G2(:,:,:,173),wf(:,-3),wf(:,70),G2(:,:,:,179))
  call check_last_UV_W(l_switch,G2(:,:,:,179),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,232))
  call loop_UV_W(G2(:,:,:,173),Q(:,18),wf(:,-2),Q(:,4),G3(:,:,:,93))
  call loop_GGG_G_12(G3(:,:,:,93),wf(:,-5),wf(:,-3),G3(:,:,:,94))
  call check_last_UV_W(l_switch,G3(:,:,:,94),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,94))
  call loop_GGG_G_12(G3(:,:,:,93),wf(:,-3),wf(:,-5),G3(:,:,:,95))
  call check_last_UV_W(l_switch,G3(:,:,:,95),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,95))
  call loop_GGG_G_23(G3(:,:,:,93),wf(:,-5),wf(:,-3),G3(:,:,:,96))
  call check_last_UV_W(l_switch,G3(:,:,:,96),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,96))
  call loop_UV_W(G3(:,:,:,93),Q(:,22),wf(:,79),Q(:,40),G4(:,:,:,19))
  call check_last_UV_W(l_switch,G4(:,:,:,19),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,15))
  call loop_UV_W(G3(:,:,:,93),Q(:,22),wf(:,-3),Q(:,8),G4(:,:,:,20))
  call loop_UV_W(G4(:,:,:,20),Q(:,30),wf(:,-5),Q(:,32),G5(:,:,:,5))
  call check_last_UV_W(l_switch,G5(:,:,:,5),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,5))
  call loop_GGG_G_12(G2(:,:,:,173),wf(:,-2),wf(:,79),G2(:,:,:,180))
  call check_last_UV_W(l_switch,G2(:,:,:,180),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,233))
  call loop_GGG_G_12(G2(:,:,:,173),wf(:,79),wf(:,-2),G2(:,:,:,181))
  call check_last_UV_W(l_switch,G2(:,:,:,181),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,234))
  call loop_GGG_G_23(G2(:,:,:,173),wf(:,-2),wf(:,79),G2(:,:,:,182))
  call check_last_UV_W(l_switch,G2(:,:,:,182),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,235))
  call loop_UV_W(G2(:,:,:,173),Q(:,18),wf(:,-3),Q(:,8),G3(:,:,:,97))
  call loop_GGG_G_12(G3(:,:,:,97),wf(:,-5),wf(:,-2),G3(:,:,:,98))
  call check_last_UV_W(l_switch,G3(:,:,:,98),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,97))
  call loop_GGG_G_12(G3(:,:,:,97),wf(:,-2),wf(:,-5),G3(:,:,:,99))
  call check_last_UV_W(l_switch,G3(:,:,:,99),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,98))
  call loop_GGG_G_23(G3(:,:,:,97),wf(:,-5),wf(:,-2),G3(:,:,:,100))
  call check_last_UV_W(l_switch,G3(:,:,:,100),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,99))
  call loop_UV_W(G3(:,:,:,97),Q(:,26),wf(:,70),Q(:,36),G4(:,:,:,21))
  call check_last_UV_W(l_switch,G4(:,:,:,21),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,16))
  call loop_UV_W(G3(:,:,:,97),Q(:,26),wf(:,-2),Q(:,4),G4(:,:,:,22))
  call loop_UV_W(G4(:,:,:,22),Q(:,30),wf(:,-5),Q(:,32),G5(:,:,:,6))
  call check_last_UV_W(l_switch,G5(:,:,:,6),Q(:,62),wf(:,0),Q(:,1),G6tensor(:,6))
  call loop_GGG_G_12(G2(:,:,:,173),wf(:,-3),wf(:,-2),G2(:,:,:,183))
  call loop_UV_W(G2(:,:,:,183),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,101))
  call check_last_UV_W(l_switch,G3(:,:,:,101),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,100))
  call loop_GGG_G_12(G2(:,:,:,173),wf(:,-2),wf(:,-3),G2(:,:,:,184))
  call loop_UV_W(G2(:,:,:,184),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,102))
  call check_last_UV_W(l_switch,G3(:,:,:,102),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,101))
  call loop_GGG_G_23(G2(:,:,:,173),wf(:,-3),wf(:,-2),G2(:,:,:,185))
  call loop_UV_W(G2(:,:,:,185),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,103))
  call check_last_UV_W(l_switch,G3(:,:,:,103),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,102))
  call loop_UV_W(G2(:,:,:,173),Q(:,18),wf(:,14),Q(:,44),G3(:,:,:,104))
  call check_last_UV_W(l_switch,G3(:,:,:,104),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,103))
  call loop_UV_W(G2(:,:,:,173),Q(:,18),wf(:,17),Q(:,44),G3(:,:,:,105))
  call check_last_UV_W(l_switch,G3(:,:,:,105),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,104))
  call loop_UV_W(G2(:,:,:,173),Q(:,18),wf(:,18),Q(:,44),G3(:,:,:,106))
  call check_last_UV_W(l_switch,G3(:,:,:,106),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,105))
  call loop_UV_W(G2(:,:,:,173),Q(:,18),wf(:,254),Q(:,44),G3(:,:,:,107))
  call check_last_UV_W(l_switch,G3(:,:,:,107),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,106))
  call loop_UV_W(G2(:,:,:,173),Q(:,18),wf(:,62),Q(:,12),G3(:,:,:,108))
  call loop_UV_W(G3(:,:,:,108),Q(:,30),wf(:,-5),Q(:,32),G4(:,:,:,23))
  call check_last_UV_W(l_switch,G4(:,:,:,23),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,17))
  call loop_UV_W(G2(:,:,:,173),Q(:,18),wf(:,263),Q(:,44),G3(:,:,:,109))
  call check_last_UV_W(l_switch,G3(:,:,:,109),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,107))
  call loop_UV_W(G2(:,:,:,173),Q(:,18),wf(:,264),Q(:,44),G3(:,:,:,110))
  call check_last_UV_W(l_switch,G3(:,:,:,110),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,108))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,140),G1(:,:,:,392))
  call check_last_UV_W(l_switch,G1(:,:,:,392),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,664))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,140),wf(:,-3),G1(:,:,:,393))
  call check_last_UV_W(l_switch,G1(:,:,:,393),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,665))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,140),G1(:,:,:,394))
  call check_last_UV_W(l_switch,G1(:,:,:,394),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,666))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,176),G1(:,:,:,395))
  call check_last_UV_W(l_switch,G1(:,:,:,395),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,667))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,176),wf(:,-3),G1(:,:,:,396))
  call check_last_UV_W(l_switch,G1(:,:,:,396),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,668))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,176),G1(:,:,:,397))
  call check_last_UV_W(l_switch,G1(:,:,:,397),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,669))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,141),G1(:,:,:,398))
  call check_last_UV_W(l_switch,G1(:,:,:,398),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,670))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,141),wf(:,-3),G1(:,:,:,399))
  call check_last_UV_W(l_switch,G1(:,:,:,399),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,671))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,141),G1(:,:,:,400))
  call check_last_UV_W(l_switch,G1(:,:,:,400),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,672))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,230),G1(:,:,:,401))
  call check_last_UV_W(l_switch,G1(:,:,:,401),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,673))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,230),wf(:,-3),G1(:,:,:,402))
  call check_last_UV_W(l_switch,G1(:,:,:,402),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,674))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,230),G1(:,:,:,403))
  call check_last_UV_W(l_switch,G1(:,:,:,403),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,675))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,177),G1(:,:,:,404))
  call check_last_UV_W(l_switch,G1(:,:,:,404),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,676))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,177),wf(:,-3),G1(:,:,:,405))
  call check_last_UV_W(l_switch,G1(:,:,:,405),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,677))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,177),G1(:,:,:,406))
  call check_last_UV_W(l_switch,G1(:,:,:,406),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,678))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-3),wf(:,231),G1(:,:,:,407))
  call check_last_UV_W(l_switch,G1(:,:,:,407),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,679))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,231),wf(:,-3),G1(:,:,:,408))
  call check_last_UV_W(l_switch,G1(:,:,:,408),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,680))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-3),wf(:,231),G1(:,:,:,409))
  call check_last_UV_W(l_switch,G1(:,:,:,409),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,681))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,182),G1(:,:,:,410))
  call check_last_UV_W(l_switch,G1(:,:,:,410),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,682))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,182),wf(:,-2),G1(:,:,:,411))
  call check_last_UV_W(l_switch,G1(:,:,:,411),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,683))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,182),G1(:,:,:,412))
  call check_last_UV_W(l_switch,G1(:,:,:,412),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,684))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,104),G1(:,:,:,413))
  call loop_UV_W(G1(:,:,:,413),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,186))
  call check_last_UV_W(l_switch,G2(:,:,:,186),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,236))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,104),wf(:,-2),G1(:,:,:,414))
  call loop_UV_W(G1(:,:,:,414),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,187))
  call check_last_UV_W(l_switch,G2(:,:,:,187),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,237))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,104),G1(:,:,:,415))
  call loop_UV_W(G1(:,:,:,415),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,188))
  call check_last_UV_W(l_switch,G2(:,:,:,188),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,238))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,104),Q(:,9),G2(:,:,:,189))
  call loop_GGG_G_12(G2(:,:,:,189),wf(:,-5),wf(:,-2),G2(:,:,:,190))
  call check_last_UV_W(l_switch,G2(:,:,:,190),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,239))
  call loop_GGG_G_12(G2(:,:,:,189),wf(:,-2),wf(:,-5),G2(:,:,:,191))
  call check_last_UV_W(l_switch,G2(:,:,:,191),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,240))
  call loop_GGG_G_23(G2(:,:,:,189),wf(:,-5),wf(:,-2),G2(:,:,:,192))
  call check_last_UV_W(l_switch,G2(:,:,:,192),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,241))
  call loop_GGG_G_12(G2(:,:,:,189),wf(:,-5),wf(:,-1),G2(:,:,:,193))
  call check_last_UV_W(l_switch,G2(:,:,:,193),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,242))
  call loop_GGG_G_12(G2(:,:,:,189),wf(:,-1),wf(:,-5),G2(:,:,:,194))
  call check_last_UV_W(l_switch,G2(:,:,:,194),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,243))
  call loop_GGG_G_23(G2(:,:,:,189),wf(:,-5),wf(:,-1),G2(:,:,:,195))
  call check_last_UV_W(l_switch,G2(:,:,:,195),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,244))
  call loop_UV_W(G2(:,:,:,189),Q(:,25),wf(:,-5),Q(:,32),G3(:,:,:,111))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,111),wf(:,-2),wf(:,-1),G3tensor(:,245))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,111),wf(:,-1),wf(:,-2),G3tensor(:,246))
  call check_last_GGG_G_23(l_switch,G3(:,:,:,111),wf(:,-2),wf(:,-1),G3tensor(:,247))
  call check_last_UV_W(l_switch,G3(:,:,:,111),Q(:,57),wf(:,105),Q(:,6),G4tensor(:,109))
  call loop_UV_W(G2(:,:,:,189),Q(:,25),wf(:,99),Q(:,34),G3(:,:,:,112))
  call check_last_UV_W(l_switch,G3(:,:,:,112),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,110))
  call loop_UV_W(G2(:,:,:,189),Q(:,25),wf(:,70),Q(:,36),G3(:,:,:,113))
  call check_last_UV_W(l_switch,G3(:,:,:,113),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,111))
  call loop_UV_W(G2(:,:,:,189),Q(:,25),wf(:,-2),Q(:,4),G3(:,:,:,114))
  call loop_UV_W(G3(:,:,:,114),Q(:,29),wf(:,-5),Q(:,32),G4(:,:,:,24))
  call check_last_UV_W(l_switch,G4(:,:,:,24),Q(:,61),wf(:,-1),Q(:,2),G5tensor(:,18))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,118),Q(:,45),G2(:,:,:,196))
  call check_last_UV_W(l_switch,G2(:,:,:,196),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,248))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,119),Q(:,45),G2(:,:,:,197))
  call check_last_UV_W(l_switch,G2(:,:,:,197),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,249))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,120),Q(:,45),G2(:,:,:,198))
  call check_last_UV_W(l_switch,G2(:,:,:,198),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,250))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,236),G1(:,:,:,416))
  call check_last_UV_W(l_switch,G1(:,:,:,416),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,685))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,236),wf(:,-2),G1(:,:,:,417))
  call check_last_UV_W(l_switch,G1(:,:,:,417),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,686))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,236),G1(:,:,:,418))
  call check_last_UV_W(l_switch,G1(:,:,:,418),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,687))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,91),G1(:,:,:,419))
  call loop_UV_W(G1(:,:,:,419),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,199))
  call check_last_UV_W(l_switch,G2(:,:,:,199),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,251))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,91),wf(:,-2),G1(:,:,:,420))
  call loop_UV_W(G1(:,:,:,420),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,200))
  call check_last_UV_W(l_switch,G2(:,:,:,200),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,252))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,91),G1(:,:,:,421))
  call loop_UV_W(G1(:,:,:,421),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,201))
  call check_last_UV_W(l_switch,G2(:,:,:,201),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,253))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,91),Q(:,10),G2(:,:,:,202))
  call loop_GGG_G_12(G2(:,:,:,202),wf(:,-5),wf(:,-2),G2(:,:,:,203))
  call check_last_UV_W(l_switch,G2(:,:,:,203),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,254))
  call loop_GGG_G_12(G2(:,:,:,202),wf(:,-2),wf(:,-5),G2(:,:,:,204))
  call check_last_UV_W(l_switch,G2(:,:,:,204),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,255))
  call loop_GGG_G_23(G2(:,:,:,202),wf(:,-5),wf(:,-2),G2(:,:,:,205))
  call check_last_UV_W(l_switch,G2(:,:,:,205),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,256))
  call loop_GGG_G_12(G2(:,:,:,202),wf(:,-5),wf(:,0),G2(:,:,:,206))
  call check_last_UV_W(l_switch,G2(:,:,:,206),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,257))
  call loop_GGG_G_12(G2(:,:,:,202),wf(:,0),wf(:,-5),G2(:,:,:,207))
  call check_last_UV_W(l_switch,G2(:,:,:,207),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,258))
  call loop_GGG_G_23(G2(:,:,:,202),wf(:,-5),wf(:,0),G2(:,:,:,208))
  call check_last_UV_W(l_switch,G2(:,:,:,208),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,259))
  call loop_UV_W(G2(:,:,:,202),Q(:,26),wf(:,-5),Q(:,32),G3(:,:,:,115))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,115),wf(:,-2),wf(:,0),G3tensor(:,260))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,115),wf(:,0),wf(:,-2),G3tensor(:,261))
  call check_last_GGG_G_23(l_switch,G3(:,:,:,115),wf(:,-2),wf(:,0),G3tensor(:,262))
  call check_last_UV_W(l_switch,G3(:,:,:,115),Q(:,58),wf(:,90),Q(:,5),G4tensor(:,112))
  call loop_UV_W(G2(:,:,:,202),Q(:,26),wf(:,113),Q(:,33),G3(:,:,:,116))
  call check_last_UV_W(l_switch,G3(:,:,:,116),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,113))
  call loop_UV_W(G2(:,:,:,202),Q(:,26),wf(:,70),Q(:,36),G3(:,:,:,117))
  call check_last_UV_W(l_switch,G3(:,:,:,117),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,114))
  call loop_UV_W(G2(:,:,:,202),Q(:,26),wf(:,-2),Q(:,4),G3(:,:,:,118))
  call loop_UV_W(G3(:,:,:,118),Q(:,30),wf(:,-5),Q(:,32),G4(:,:,:,25))
  call check_last_UV_W(l_switch,G4(:,:,:,25),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,19))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,889),Q(:,46),G2(:,:,:,209))
  call check_last_UV_W(l_switch,G2(:,:,:,209),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,263))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,890),Q(:,46),G2(:,:,:,210))
  call check_last_UV_W(l_switch,G2(:,:,:,210),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,264))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,891),Q(:,46),G2(:,:,:,211))
  call check_last_UV_W(l_switch,G2(:,:,:,211),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,265))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,140),G1(:,:,:,422))
  call check_last_UV_W(l_switch,G1(:,:,:,422),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,688))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,140),wf(:,-2),G1(:,:,:,423))
  call check_last_UV_W(l_switch,G1(:,:,:,423),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,689))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,140),G1(:,:,:,424))
  call check_last_UV_W(l_switch,G1(:,:,:,424),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,690))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,191),G1(:,:,:,425))
  call check_last_UV_W(l_switch,G1(:,:,:,425),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,691))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,191),wf(:,-2),G1(:,:,:,426))
  call check_last_UV_W(l_switch,G1(:,:,:,426),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,692))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,191),G1(:,:,:,427))
  call check_last_UV_W(l_switch,G1(:,:,:,427),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,693))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,141),G1(:,:,:,428))
  call check_last_UV_W(l_switch,G1(:,:,:,428),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,694))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,141),wf(:,-2),G1(:,:,:,429))
  call check_last_UV_W(l_switch,G1(:,:,:,429),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,695))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,141),G1(:,:,:,430))
  call check_last_UV_W(l_switch,G1(:,:,:,430),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,696))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,245),G1(:,:,:,431))
  call check_last_UV_W(l_switch,G1(:,:,:,431),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,697))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,245),wf(:,-2),G1(:,:,:,432))
  call check_last_UV_W(l_switch,G1(:,:,:,432),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,698))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,245),G1(:,:,:,433))
  call check_last_UV_W(l_switch,G1(:,:,:,433),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,699))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,192),G1(:,:,:,434))
  call check_last_UV_W(l_switch,G1(:,:,:,434),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,700))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,192),wf(:,-2),G1(:,:,:,435))
  call check_last_UV_W(l_switch,G1(:,:,:,435),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,701))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,192),G1(:,:,:,436))
  call check_last_UV_W(l_switch,G1(:,:,:,436),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,702))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-2),wf(:,246),G1(:,:,:,437))
  call check_last_UV_W(l_switch,G1(:,:,:,437),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,703))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,246),wf(:,-2),G1(:,:,:,438))
  call check_last_UV_W(l_switch,G1(:,:,:,438),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,704))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-2),wf(:,246),G1(:,:,:,439))
  call check_last_UV_W(l_switch,G1(:,:,:,439),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,705))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,140),Q(:,35),G2(:,:,:,212))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,212),wf(:,-3),wf(:,-2),G2tensor(:,706))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,212),wf(:,-2),wf(:,-3),G2tensor(:,707))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,212),wf(:,-3),wf(:,-2),G2tensor(:,708))
  call check_last_UV_W(l_switch,G2(:,:,:,212),Q(:,51),wf(:,62),Q(:,12),G3tensor(:,266))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,136),Q(:,45),G2(:,:,:,213))
  call check_last_UV_W(l_switch,G2(:,:,:,213),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,267))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,137),Q(:,45),G2(:,:,:,214))
  call check_last_UV_W(l_switch,G2(:,:,:,214),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,268))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,138),Q(:,45),G2(:,:,:,215))
  call check_last_UV_W(l_switch,G2(:,:,:,215),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,269))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,141),Q(:,35),G2(:,:,:,216))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,216),wf(:,-3),wf(:,-2),G2tensor(:,709))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,216),wf(:,-2),wf(:,-3),G2tensor(:,710))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,216),wf(:,-3),wf(:,-2),G2tensor(:,711))
  call check_last_UV_W(l_switch,G2(:,:,:,216),Q(:,51),wf(:,62),Q(:,12),G3tensor(:,270))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,898),Q(:,46),G2(:,:,:,217))
  call check_last_UV_W(l_switch,G2(:,:,:,217),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,271))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,899),Q(:,46),G2(:,:,:,218))
  call check_last_UV_W(l_switch,G2(:,:,:,218),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,272))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,900),Q(:,46),G2(:,:,:,219))
  call check_last_UV_W(l_switch,G2(:,:,:,219),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,273))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,907),Q(:,45),G2(:,:,:,220))
  call check_last_UV_W(l_switch,G2(:,:,:,220),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,274))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,908),Q(:,45),G2(:,:,:,221))
  call check_last_UV_W(l_switch,G2(:,:,:,221),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,275))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,909),Q(:,45),G2(:,:,:,222))
  call check_last_UV_W(l_switch,G2(:,:,:,222),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,276))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,901),Q(:,46),G2(:,:,:,223))
  call check_last_UV_W(l_switch,G2(:,:,:,223),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,277))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,902),Q(:,46),G2(:,:,:,224))
  call check_last_UV_W(l_switch,G2(:,:,:,224),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,278))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,903),Q(:,46),G2(:,:,:,225))
  call check_last_UV_W(l_switch,G2(:,:,:,225),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,279))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,153),G1(:,:,:,440))
  call check_last_UV_W(l_switch,G1(:,:,:,440),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,712))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,153),wf(:,-1),G1(:,:,:,441))
  call check_last_UV_W(l_switch,G1(:,:,:,441),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,713))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,153),G1(:,:,:,442))
  call check_last_UV_W(l_switch,G1(:,:,:,442),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,714))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,142),Q(:,39),G2(:,:,:,226))
  call check_last_UV_W(l_switch,G2(:,:,:,226),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,280))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,143),Q(:,39),G2(:,:,:,227))
  call check_last_UV_W(l_switch,G2(:,:,:,227),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,281))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,144),Q(:,39),G2(:,:,:,228))
  call check_last_UV_W(l_switch,G2(:,:,:,228),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,282))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,153),Q(:,37),G2(:,:,:,229))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,229),wf(:,-3),wf(:,-1),G2tensor(:,715))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,229),wf(:,-1),wf(:,-3),G2tensor(:,716))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,229),wf(:,-3),wf(:,-1),G2tensor(:,717))
  call check_last_UV_W(l_switch,G2(:,:,:,229),Q(:,53),wf(:,91),Q(:,10),G3tensor(:,283))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,182),G1(:,:,:,443))
  call check_last_UV_W(l_switch,G1(:,:,:,443),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,718))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,182),wf(:,-1),G1(:,:,:,444))
  call check_last_UV_W(l_switch,G1(:,:,:,444),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,719))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,182),G1(:,:,:,445))
  call check_last_UV_W(l_switch,G1(:,:,:,445),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,720))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,104),G1(:,:,:,446))
  call loop_UV_W(G1(:,:,:,446),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,230))
  call check_last_UV_W(l_switch,G2(:,:,:,230),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,284))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,104),wf(:,-1),G1(:,:,:,447))
  call loop_UV_W(G1(:,:,:,447),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,231))
  call check_last_UV_W(l_switch,G2(:,:,:,231),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,285))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,104),G1(:,:,:,448))
  call loop_UV_W(G1(:,:,:,448),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,232))
  call check_last_UV_W(l_switch,G2(:,:,:,232),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,286))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,154),Q(:,43),G2(:,:,:,233))
  call check_last_UV_W(l_switch,G2(:,:,:,233),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,287))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,155),Q(:,43),G2(:,:,:,234))
  call check_last_UV_W(l_switch,G2(:,:,:,234),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,288))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,156),Q(:,43),G2(:,:,:,235))
  call check_last_UV_W(l_switch,G2(:,:,:,235),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,289))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,254),G1(:,:,:,449))
  call check_last_UV_W(l_switch,G1(:,:,:,449),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,721))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,254),wf(:,-1),G1(:,:,:,450))
  call check_last_UV_W(l_switch,G1(:,:,:,450),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,722))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,254),G1(:,:,:,451))
  call check_last_UV_W(l_switch,G1(:,:,:,451),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,723))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,62),G1(:,:,:,452))
  call loop_UV_W(G1(:,:,:,452),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,236))
  call check_last_UV_W(l_switch,G2(:,:,:,236),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,290))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,62),wf(:,-1),G1(:,:,:,453))
  call loop_UV_W(G1(:,:,:,453),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,237))
  call check_last_UV_W(l_switch,G2(:,:,:,237),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,291))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,62),G1(:,:,:,454))
  call loop_UV_W(G1(:,:,:,454),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,238))
  call check_last_UV_W(l_switch,G2(:,:,:,238),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,292))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,62),Q(:,12),G2(:,:,:,239))
  call loop_GGG_G_12(G2(:,:,:,239),wf(:,-5),wf(:,-1),G2(:,:,:,240))
  call check_last_UV_W(l_switch,G2(:,:,:,240),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,293))
  call loop_GGG_G_12(G2(:,:,:,239),wf(:,-1),wf(:,-5),G2(:,:,:,241))
  call check_last_UV_W(l_switch,G2(:,:,:,241),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,294))
  call loop_GGG_G_23(G2(:,:,:,239),wf(:,-5),wf(:,-1),G2(:,:,:,242))
  call check_last_UV_W(l_switch,G2(:,:,:,242),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,295))
  call loop_GGG_G_12(G2(:,:,:,239),wf(:,-5),wf(:,0),G2(:,:,:,243))
  call check_last_UV_W(l_switch,G2(:,:,:,243),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,296))
  call loop_GGG_G_12(G2(:,:,:,239),wf(:,0),wf(:,-5),G2(:,:,:,244))
  call check_last_UV_W(l_switch,G2(:,:,:,244),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,297))
  call loop_GGG_G_23(G2(:,:,:,239),wf(:,-5),wf(:,0),G2(:,:,:,245))
  call check_last_UV_W(l_switch,G2(:,:,:,245),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,298))
  call loop_UV_W(G2(:,:,:,239),Q(:,28),wf(:,-5),Q(:,32),G3(:,:,:,119))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,119),wf(:,-1),wf(:,0),G3tensor(:,299))
  call check_last_GGG_G_12(l_switch,G3(:,:,:,119),wf(:,0),wf(:,-1),G3tensor(:,300))
  call check_last_GGG_G_23(l_switch,G3(:,:,:,119),wf(:,-1),wf(:,0),G3tensor(:,301))
  call check_last_UV_W(l_switch,G3(:,:,:,119),Q(:,60),wf(:,61),Q(:,3),G4tensor(:,115))
  call loop_UV_W(G2(:,:,:,239),Q(:,28),wf(:,113),Q(:,33),G3(:,:,:,120))
  call check_last_UV_W(l_switch,G3(:,:,:,120),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,116))
  call loop_UV_W(G2(:,:,:,239),Q(:,28),wf(:,99),Q(:,34),G3(:,:,:,121))
  call check_last_UV_W(l_switch,G3(:,:,:,121),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,117))
  call loop_UV_W(G2(:,:,:,239),Q(:,28),wf(:,-1),Q(:,2),G3(:,:,:,122))
  call loop_UV_W(G3(:,:,:,122),Q(:,30),wf(:,-5),Q(:,32),G4(:,:,:,26))
  call check_last_UV_W(l_switch,G4(:,:,:,26),Q(:,62),wf(:,0),Q(:,1),G5tensor(:,20))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,952),Q(:,46),G2(:,:,:,246))
  call check_last_UV_W(l_switch,G2(:,:,:,246),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,302))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,953),Q(:,46),G2(:,:,:,247))
  call check_last_UV_W(l_switch,G2(:,:,:,247),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,303))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,954),Q(:,46),G2(:,:,:,248))
  call check_last_UV_W(l_switch,G2(:,:,:,248),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,304))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,176),G1(:,:,:,455))
  call check_last_UV_W(l_switch,G1(:,:,:,455),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,724))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,176),wf(:,-1),G1(:,:,:,456))
  call check_last_UV_W(l_switch,G1(:,:,:,456),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,725))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,176),G1(:,:,:,457))
  call check_last_UV_W(l_switch,G1(:,:,:,457),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,726))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,191),G1(:,:,:,458))
  call check_last_UV_W(l_switch,G1(:,:,:,458),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,727))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,191),wf(:,-1),G1(:,:,:,459))
  call check_last_UV_W(l_switch,G1(:,:,:,459),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,728))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,191),G1(:,:,:,460))
  call check_last_UV_W(l_switch,G1(:,:,:,460),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,729))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,177),G1(:,:,:,461))
  call check_last_UV_W(l_switch,G1(:,:,:,461),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,730))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,177),wf(:,-1),G1(:,:,:,462))
  call check_last_UV_W(l_switch,G1(:,:,:,462),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,731))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,177),G1(:,:,:,463))
  call check_last_UV_W(l_switch,G1(:,:,:,463),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,732))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,263),G1(:,:,:,464))
  call check_last_UV_W(l_switch,G1(:,:,:,464),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,733))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,263),wf(:,-1),G1(:,:,:,465))
  call check_last_UV_W(l_switch,G1(:,:,:,465),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,734))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,263),G1(:,:,:,466))
  call check_last_UV_W(l_switch,G1(:,:,:,466),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,735))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,192),G1(:,:,:,467))
  call check_last_UV_W(l_switch,G1(:,:,:,467),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,736))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,192),wf(:,-1),G1(:,:,:,468))
  call check_last_UV_W(l_switch,G1(:,:,:,468),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,737))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,192),G1(:,:,:,469))
  call check_last_UV_W(l_switch,G1(:,:,:,469),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,738))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,-1),wf(:,264),G1(:,:,:,470))
  call check_last_UV_W(l_switch,G1(:,:,:,470),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,739))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,264),wf(:,-1),G1(:,:,:,471))
  call check_last_UV_W(l_switch,G1(:,:,:,471),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,740))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,-1),wf(:,264),G1(:,:,:,472))
  call check_last_UV_W(l_switch,G1(:,:,:,472),Q(:,62),wf(:,0),Q(:,1),G2tensor(:,741))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,176),Q(:,37),G2(:,:,:,249))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,249),wf(:,-3),wf(:,-1),G2tensor(:,742))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,249),wf(:,-1),wf(:,-3),G2tensor(:,743))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,249),wf(:,-3),wf(:,-1),G2tensor(:,744))
  call check_last_UV_W(l_switch,G2(:,:,:,249),Q(:,53),wf(:,91),Q(:,10),G3tensor(:,305))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,172),Q(:,43),G2(:,:,:,250))
  call check_last_UV_W(l_switch,G2(:,:,:,250),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,306))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,173),Q(:,43),G2(:,:,:,251))
  call check_last_UV_W(l_switch,G2(:,:,:,251),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,307))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,174),Q(:,43),G2(:,:,:,252))
  call check_last_UV_W(l_switch,G2(:,:,:,252),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,308))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,177),Q(:,37),G2(:,:,:,253))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,253),wf(:,-3),wf(:,-1),G2tensor(:,745))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,253),wf(:,-1),wf(:,-3),G2tensor(:,746))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,253),wf(:,-3),wf(:,-1),G2tensor(:,747))
  call check_last_UV_W(l_switch,G2(:,:,:,253),Q(:,53),wf(:,91),Q(:,10),G3tensor(:,309))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,961),Q(:,46),G2(:,:,:,254))
  call check_last_UV_W(l_switch,G2(:,:,:,254),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,310))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,962),Q(:,46),G2(:,:,:,255))
  call check_last_UV_W(l_switch,G2(:,:,:,255),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,311))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,963),Q(:,46),G2(:,:,:,256))
  call check_last_UV_W(l_switch,G2(:,:,:,256),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,312))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,970),Q(:,43),G2(:,:,:,257))
  call check_last_UV_W(l_switch,G2(:,:,:,257),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,313))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,971),Q(:,43),G2(:,:,:,258))
  call check_last_UV_W(l_switch,G2(:,:,:,258),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,314))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,972),Q(:,43),G2(:,:,:,259))
  call check_last_UV_W(l_switch,G2(:,:,:,259),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,315))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,964),Q(:,46),G2(:,:,:,260))
  call check_last_UV_W(l_switch,G2(:,:,:,260),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,316))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,965),Q(:,46),G2(:,:,:,261))
  call check_last_UV_W(l_switch,G2(:,:,:,261),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,317))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,966),Q(:,46),G2(:,:,:,262))
  call check_last_UV_W(l_switch,G2(:,:,:,262),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,318))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,182),Q(:,41),G2(:,:,:,263))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,263),wf(:,-2),wf(:,-1),G2tensor(:,748))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,263),wf(:,-1),wf(:,-2),G2tensor(:,749))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,263),wf(:,-2),wf(:,-1),G2tensor(:,750))
  call check_last_UV_W(l_switch,G2(:,:,:,263),Q(:,57),wf(:,105),Q(:,6),G3tensor(:,319))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,191),Q(:,41),G2(:,:,:,264))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,264),wf(:,-2),wf(:,-1),G2tensor(:,751))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,264),wf(:,-1),wf(:,-2),G2tensor(:,752))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,264),wf(:,-2),wf(:,-1),G2tensor(:,753))
  call check_last_UV_W(l_switch,G2(:,:,:,264),Q(:,57),wf(:,105),Q(:,6),G3tensor(:,320))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,187),Q(:,39),G2(:,:,:,265))
  call check_last_UV_W(l_switch,G2(:,:,:,265),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,321))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,188),Q(:,39),G2(:,:,:,266))
  call check_last_UV_W(l_switch,G2(:,:,:,266),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,322))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,189),Q(:,39),G2(:,:,:,267))
  call check_last_UV_W(l_switch,G2(:,:,:,267),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,323))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,192),Q(:,41),G2(:,:,:,268))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,268),wf(:,-2),wf(:,-1),G2tensor(:,754))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,268),wf(:,-1),wf(:,-2),G2tensor(:,755))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,268),wf(:,-2),wf(:,-1),G2tensor(:,756))
  call check_last_UV_W(l_switch,G2(:,:,:,268),Q(:,57),wf(:,105),Q(:,6),G3tensor(:,324))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,994),Q(:,46),G2(:,:,:,269))
  call check_last_UV_W(l_switch,G2(:,:,:,269),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,325))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,995),Q(:,46),G2(:,:,:,270))
  call check_last_UV_W(l_switch,G2(:,:,:,270),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,326))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,996),Q(:,46),G2(:,:,:,271))
  call check_last_UV_W(l_switch,G2(:,:,:,271),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,327))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1003),Q(:,39),G2(:,:,:,272))
  call check_last_UV_W(l_switch,G2(:,:,:,272),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,328))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1004),Q(:,39),G2(:,:,:,273))
  call check_last_UV_W(l_switch,G2(:,:,:,273),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,329))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1005),Q(:,39),G2(:,:,:,274))
  call check_last_UV_W(l_switch,G2(:,:,:,274),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,330))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,997),Q(:,46),G2(:,:,:,275))
  call check_last_UV_W(l_switch,G2(:,:,:,275),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,331))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,998),Q(:,46),G2(:,:,:,276))
  call check_last_UV_W(l_switch,G2(:,:,:,276),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,332))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,999),Q(:,46),G2(:,:,:,277))
  call check_last_UV_W(l_switch,G2(:,:,:,277),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,333))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1012),Q(:,46),G2(:,:,:,278))
  call check_last_UV_W(l_switch,G2(:,:,:,278),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,334))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1013),Q(:,46),G2(:,:,:,279))
  call check_last_UV_W(l_switch,G2(:,:,:,279),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,335))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1014),Q(:,46),G2(:,:,:,280))
  call check_last_UV_W(l_switch,G2(:,:,:,280),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,336))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,56),Q(:,14),G2(:,:,:,281))
  call loop_UV_W(G2(:,:,:,281),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,123))
  call check_last_UV_W(l_switch,G3(:,:,:,123),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,118))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,59),Q(:,14),G2(:,:,:,282))
  call loop_UV_W(G2(:,:,:,282),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,124))
  call check_last_UV_W(l_switch,G3(:,:,:,124),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,119))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,60),Q(:,14),G2(:,:,:,283))
  call loop_UV_W(G2(:,:,:,283),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,125))
  call check_last_UV_W(l_switch,G3(:,:,:,125),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,120))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,207),G1(:,:,:,473))
  call check_last_UV_W(l_switch,G1(:,:,:,473),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,757))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,207),wf(:,0),G1(:,:,:,474))
  call check_last_UV_W(l_switch,G1(:,:,:,474),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,758))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,207),G1(:,:,:,475))
  call check_last_UV_W(l_switch,G1(:,:,:,475),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,759))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,196),Q(:,39),G2(:,:,:,284))
  call check_last_UV_W(l_switch,G2(:,:,:,284),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,337))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,197),Q(:,39),G2(:,:,:,285))
  call check_last_UV_W(l_switch,G2(:,:,:,285),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,338))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,198),Q(:,39),G2(:,:,:,286))
  call check_last_UV_W(l_switch,G2(:,:,:,286),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,339))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,207),Q(:,38),G2(:,:,:,287))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,287),wf(:,-3),wf(:,0),G2tensor(:,760))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,287),wf(:,0),wf(:,-3),G2tensor(:,761))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,287),wf(:,-3),wf(:,0),G2tensor(:,762))
  call check_last_UV_W(l_switch,G2(:,:,:,287),Q(:,54),wf(:,104),Q(:,9),G3tensor(:,340))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,236),G1(:,:,:,476))
  call check_last_UV_W(l_switch,G1(:,:,:,476),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,763))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,236),wf(:,0),G1(:,:,:,477))
  call check_last_UV_W(l_switch,G1(:,:,:,477),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,764))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,236),G1(:,:,:,478))
  call check_last_UV_W(l_switch,G1(:,:,:,478),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,765))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,91),G1(:,:,:,479))
  call loop_UV_W(G1(:,:,:,479),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,288))
  call check_last_UV_W(l_switch,G2(:,:,:,288),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,341))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,91),wf(:,0),G1(:,:,:,480))
  call loop_UV_W(G1(:,:,:,480),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,289))
  call check_last_UV_W(l_switch,G2(:,:,:,289),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,342))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,91),G1(:,:,:,481))
  call loop_UV_W(G1(:,:,:,481),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,290))
  call check_last_UV_W(l_switch,G2(:,:,:,290),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,343))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,208),Q(:,43),G2(:,:,:,291))
  call check_last_UV_W(l_switch,G2(:,:,:,291),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,344))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,209),Q(:,43),G2(:,:,:,292))
  call check_last_UV_W(l_switch,G2(:,:,:,292),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,345))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,210),Q(:,43),G2(:,:,:,293))
  call check_last_UV_W(l_switch,G2(:,:,:,293),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,346))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,254),G1(:,:,:,482))
  call check_last_UV_W(l_switch,G1(:,:,:,482),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,766))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,254),wf(:,0),G1(:,:,:,483))
  call check_last_UV_W(l_switch,G1(:,:,:,483),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,767))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,254),G1(:,:,:,484))
  call check_last_UV_W(l_switch,G1(:,:,:,484),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,768))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,62),G1(:,:,:,485))
  call loop_UV_W(G1(:,:,:,485),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,294))
  call check_last_UV_W(l_switch,G2(:,:,:,294),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,347))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,62),wf(:,0),G1(:,:,:,486))
  call loop_UV_W(G1(:,:,:,486),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,295))
  call check_last_UV_W(l_switch,G2(:,:,:,295),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,348))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,62),G1(:,:,:,487))
  call loop_UV_W(G1(:,:,:,487),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,296))
  call check_last_UV_W(l_switch,G2(:,:,:,296),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,349))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1063),Q(:,45),G2(:,:,:,297))
  call check_last_UV_W(l_switch,G2(:,:,:,297),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,350))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1064),Q(:,45),G2(:,:,:,298))
  call check_last_UV_W(l_switch,G2(:,:,:,298),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,351))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1065),Q(:,45),G2(:,:,:,299))
  call check_last_UV_W(l_switch,G2(:,:,:,299),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,352))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,230),G1(:,:,:,488))
  call check_last_UV_W(l_switch,G1(:,:,:,488),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,769))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,230),wf(:,0),G1(:,:,:,489))
  call check_last_UV_W(l_switch,G1(:,:,:,489),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,770))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,230),G1(:,:,:,490))
  call check_last_UV_W(l_switch,G1(:,:,:,490),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,771))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,245),G1(:,:,:,491))
  call check_last_UV_W(l_switch,G1(:,:,:,491),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,772))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,245),wf(:,0),G1(:,:,:,492))
  call check_last_UV_W(l_switch,G1(:,:,:,492),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,773))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,245),G1(:,:,:,493))
  call check_last_UV_W(l_switch,G1(:,:,:,493),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,774))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,231),G1(:,:,:,494))
  call check_last_UV_W(l_switch,G1(:,:,:,494),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,775))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,231),wf(:,0),G1(:,:,:,495))
  call check_last_UV_W(l_switch,G1(:,:,:,495),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,776))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,231),G1(:,:,:,496))
  call check_last_UV_W(l_switch,G1(:,:,:,496),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,777))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,263),G1(:,:,:,497))
  call check_last_UV_W(l_switch,G1(:,:,:,497),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,778))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,263),wf(:,0),G1(:,:,:,498))
  call check_last_UV_W(l_switch,G1(:,:,:,498),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,779))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,263),G1(:,:,:,499))
  call check_last_UV_W(l_switch,G1(:,:,:,499),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,780))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,246),G1(:,:,:,500))
  call check_last_UV_W(l_switch,G1(:,:,:,500),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,781))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,246),wf(:,0),G1(:,:,:,501))
  call check_last_UV_W(l_switch,G1(:,:,:,501),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,782))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,246),G1(:,:,:,502))
  call check_last_UV_W(l_switch,G1(:,:,:,502),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,783))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,0),wf(:,264),G1(:,:,:,503))
  call check_last_UV_W(l_switch,G1(:,:,:,503),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,784))
  call loop_GGG_G_12(G1(:,:,:,1),wf(:,264),wf(:,0),G1(:,:,:,504))
  call check_last_UV_W(l_switch,G1(:,:,:,504),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,785))
  call loop_GGG_G_23(G1(:,:,:,1),wf(:,0),wf(:,264),G1(:,:,:,505))
  call check_last_UV_W(l_switch,G1(:,:,:,505),Q(:,61),wf(:,-1),Q(:,2),G2tensor(:,786))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,230),Q(:,38),G2(:,:,:,300))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,300),wf(:,-3),wf(:,0),G2tensor(:,787))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,300),wf(:,0),wf(:,-3),G2tensor(:,788))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,300),wf(:,-3),wf(:,0),G2tensor(:,789))
  call check_last_UV_W(l_switch,G2(:,:,:,300),Q(:,54),wf(:,104),Q(:,9),G3tensor(:,353))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,226),Q(:,43),G2(:,:,:,301))
  call check_last_UV_W(l_switch,G2(:,:,:,301),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,354))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,227),Q(:,43),G2(:,:,:,302))
  call check_last_UV_W(l_switch,G2(:,:,:,302),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,355))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,228),Q(:,43),G2(:,:,:,303))
  call check_last_UV_W(l_switch,G2(:,:,:,303),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,356))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,231),Q(:,38),G2(:,:,:,304))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,304),wf(:,-3),wf(:,0),G2tensor(:,790))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,304),wf(:,0),wf(:,-3),G2tensor(:,791))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,304),wf(:,-3),wf(:,0),G2tensor(:,792))
  call check_last_UV_W(l_switch,G2(:,:,:,304),Q(:,54),wf(:,104),Q(:,9),G3tensor(:,357))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1072),Q(:,45),G2(:,:,:,305))
  call check_last_UV_W(l_switch,G2(:,:,:,305),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,358))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1073),Q(:,45),G2(:,:,:,306))
  call check_last_UV_W(l_switch,G2(:,:,:,306),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,359))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1074),Q(:,45),G2(:,:,:,307))
  call check_last_UV_W(l_switch,G2(:,:,:,307),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,360))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1081),Q(:,43),G2(:,:,:,308))
  call check_last_UV_W(l_switch,G2(:,:,:,308),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,361))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1082),Q(:,43),G2(:,:,:,309))
  call check_last_UV_W(l_switch,G2(:,:,:,309),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,362))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1083),Q(:,43),G2(:,:,:,310))
  call check_last_UV_W(l_switch,G2(:,:,:,310),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,363))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1075),Q(:,45),G2(:,:,:,311))
  call check_last_UV_W(l_switch,G2(:,:,:,311),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,364))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1076),Q(:,45),G2(:,:,:,312))
  call check_last_UV_W(l_switch,G2(:,:,:,312),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,365))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1077),Q(:,45),G2(:,:,:,313))
  call check_last_UV_W(l_switch,G2(:,:,:,313),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,366))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,236),Q(:,42),G2(:,:,:,314))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,314),wf(:,-2),wf(:,0),G2tensor(:,793))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,314),wf(:,0),wf(:,-2),G2tensor(:,794))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,314),wf(:,-2),wf(:,0),G2tensor(:,795))
  call check_last_UV_W(l_switch,G2(:,:,:,314),Q(:,58),wf(:,90),Q(:,5),G3tensor(:,367))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,245),Q(:,42),G2(:,:,:,315))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,315),wf(:,-2),wf(:,0),G2tensor(:,796))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,315),wf(:,0),wf(:,-2),G2tensor(:,797))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,315),wf(:,-2),wf(:,0),G2tensor(:,798))
  call check_last_UV_W(l_switch,G2(:,:,:,315),Q(:,58),wf(:,90),Q(:,5),G3tensor(:,368))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,241),Q(:,39),G2(:,:,:,316))
  call check_last_UV_W(l_switch,G2(:,:,:,316),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,369))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,242),Q(:,39),G2(:,:,:,317))
  call check_last_UV_W(l_switch,G2(:,:,:,317),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,370))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,243),Q(:,39),G2(:,:,:,318))
  call check_last_UV_W(l_switch,G2(:,:,:,318),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,371))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,246),Q(:,42),G2(:,:,:,319))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,319),wf(:,-2),wf(:,0),G2tensor(:,799))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,319),wf(:,0),wf(:,-2),G2tensor(:,800))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,319),wf(:,-2),wf(:,0),G2tensor(:,801))
  call check_last_UV_W(l_switch,G2(:,:,:,319),Q(:,58),wf(:,90),Q(:,5),G3tensor(:,372))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1099),Q(:,45),G2(:,:,:,320))
  call check_last_UV_W(l_switch,G2(:,:,:,320),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,373))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1100),Q(:,45),G2(:,:,:,321))
  call check_last_UV_W(l_switch,G2(:,:,:,321),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,374))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1101),Q(:,45),G2(:,:,:,322))
  call check_last_UV_W(l_switch,G2(:,:,:,322),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,375))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1108),Q(:,39),G2(:,:,:,323))
  call check_last_UV_W(l_switch,G2(:,:,:,323),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,376))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1109),Q(:,39),G2(:,:,:,324))
  call check_last_UV_W(l_switch,G2(:,:,:,324),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,377))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1110),Q(:,39),G2(:,:,:,325))
  call check_last_UV_W(l_switch,G2(:,:,:,325),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,378))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1102),Q(:,45),G2(:,:,:,326))
  call check_last_UV_W(l_switch,G2(:,:,:,326),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,379))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1103),Q(:,45),G2(:,:,:,327))
  call check_last_UV_W(l_switch,G2(:,:,:,327),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,380))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1104),Q(:,45),G2(:,:,:,328))
  call check_last_UV_W(l_switch,G2(:,:,:,328),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,381))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1117),Q(:,45),G2(:,:,:,329))
  call check_last_UV_W(l_switch,G2(:,:,:,329),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,382))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1118),Q(:,45),G2(:,:,:,330))
  call check_last_UV_W(l_switch,G2(:,:,:,330),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,383))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1119),Q(:,45),G2(:,:,:,331))
  call check_last_UV_W(l_switch,G2(:,:,:,331),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,384))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,25),Q(:,13),G2(:,:,:,332))
  call loop_UV_W(G2(:,:,:,332),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,126))
  call check_last_UV_W(l_switch,G3(:,:,:,126),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,121))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,27),Q(:,13),G2(:,:,:,333))
  call loop_UV_W(G2(:,:,:,333),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,127))
  call check_last_UV_W(l_switch,G3(:,:,:,127),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,122))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,28),Q(:,13),G2(:,:,:,334))
  call loop_UV_W(G2(:,:,:,334),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,128))
  call check_last_UV_W(l_switch,G3(:,:,:,128),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,123))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,254),Q(:,44),G2(:,:,:,335))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,335),wf(:,-1),wf(:,0),G2tensor(:,802))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,335),wf(:,0),wf(:,-1),G2tensor(:,803))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,335),wf(:,-1),wf(:,0),G2tensor(:,804))
  call check_last_UV_W(l_switch,G2(:,:,:,335),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,385))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,263),Q(:,44),G2(:,:,:,336))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,336),wf(:,-1),wf(:,0),G2tensor(:,805))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,336),wf(:,0),wf(:,-1),G2tensor(:,806))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,336),wf(:,-1),wf(:,0),G2tensor(:,807))
  call check_last_UV_W(l_switch,G2(:,:,:,336),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,386))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,259),Q(:,39),G2(:,:,:,337))
  call check_last_UV_W(l_switch,G2(:,:,:,337),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,387))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,260),Q(:,39),G2(:,:,:,338))
  call check_last_UV_W(l_switch,G2(:,:,:,338),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,388))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,261),Q(:,39),G2(:,:,:,339))
  call check_last_UV_W(l_switch,G2(:,:,:,339),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,389))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,264),Q(:,44),G2(:,:,:,340))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,340),wf(:,-1),wf(:,0),G2tensor(:,808))
  call check_last_GGG_G_12(l_switch,G2(:,:,:,340),wf(:,0),wf(:,-1),G2tensor(:,809))
  call check_last_GGG_G_23(l_switch,G2(:,:,:,340),wf(:,-1),wf(:,0),G2tensor(:,810))
  call check_last_UV_W(l_switch,G2(:,:,:,340),Q(:,60),wf(:,61),Q(:,3),G3tensor(:,390))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1135),Q(:,43),G2(:,:,:,341))
  call check_last_UV_W(l_switch,G2(:,:,:,341),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,391))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1136),Q(:,43),G2(:,:,:,342))
  call check_last_UV_W(l_switch,G2(:,:,:,342),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,392))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1137),Q(:,43),G2(:,:,:,343))
  call check_last_UV_W(l_switch,G2(:,:,:,343),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,393))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1144),Q(:,39),G2(:,:,:,344))
  call check_last_UV_W(l_switch,G2(:,:,:,344),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,394))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1145),Q(:,39),G2(:,:,:,345))
  call check_last_UV_W(l_switch,G2(:,:,:,345),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,395))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1146),Q(:,39),G2(:,:,:,346))
  call check_last_UV_W(l_switch,G2(:,:,:,346),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,396))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1138),Q(:,43),G2(:,:,:,347))
  call check_last_UV_W(l_switch,G2(:,:,:,347),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,397))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1139),Q(:,43),G2(:,:,:,348))
  call check_last_UV_W(l_switch,G2(:,:,:,348),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,398))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1140),Q(:,43),G2(:,:,:,349))
  call check_last_UV_W(l_switch,G2(:,:,:,349),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,399))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1153),Q(:,43),G2(:,:,:,350))
  call check_last_UV_W(l_switch,G2(:,:,:,350),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,400))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1154),Q(:,43),G2(:,:,:,351))
  call check_last_UV_W(l_switch,G2(:,:,:,351),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,401))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1155),Q(:,43),G2(:,:,:,352))
  call check_last_UV_W(l_switch,G2(:,:,:,352),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,402))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,7),Q(:,11),G2(:,:,:,353))
  call loop_UV_W(G2(:,:,:,353),Q(:,27),wf(:,-5),Q(:,32),G3(:,:,:,129))
  call check_last_UV_W(l_switch,G3(:,:,:,129),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,124))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,9),Q(:,11),G2(:,:,:,354))
  call loop_UV_W(G2(:,:,:,354),Q(:,27),wf(:,-5),Q(:,32),G3(:,:,:,130))
  call check_last_UV_W(l_switch,G3(:,:,:,130),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,125))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,10),Q(:,11),G2(:,:,:,355))
  call loop_UV_W(G2(:,:,:,355),Q(:,27),wf(:,-5),Q(:,32),G3(:,:,:,131))
  call check_last_UV_W(l_switch,G3(:,:,:,131),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,126))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1162),Q(:,39),G2(:,:,:,356))
  call check_last_UV_W(l_switch,G2(:,:,:,356),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,403))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1163),Q(:,39),G2(:,:,:,357))
  call check_last_UV_W(l_switch,G2(:,:,:,357),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,404))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1164),Q(:,39),G2(:,:,:,358))
  call check_last_UV_W(l_switch,G2(:,:,:,358),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,405))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,273),Q(:,39),G2(:,:,:,359))
  call check_last_UV_W(l_switch,G2(:,:,:,359),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,406))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1195),Q(:,43),G2(:,:,:,360))
  call check_last_UV_W(l_switch,G2(:,:,:,360),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,407))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1323),Q(:,39),G2(:,:,:,361))
  call check_last_UV_W(l_switch,G2(:,:,:,361),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,408))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1326),Q(:,43),G2(:,:,:,362))
  call check_last_UV_W(l_switch,G2(:,:,:,362),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,409))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,83),Q(:,11),G2(:,:,:,363))
  call loop_UV_W(G2(:,:,:,363),Q(:,27),wf(:,-5),Q(:,32),G3(:,:,:,132))
  call check_last_UV_W(l_switch,G3(:,:,:,132),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,127))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1330),Q(:,39),G2(:,:,:,364))
  call check_last_UV_W(l_switch,G2(:,:,:,364),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,410))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1331),Q(:,43),G2(:,:,:,365))
  call check_last_UV_W(l_switch,G2(:,:,:,365),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,411))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,276),Q(:,39),G2(:,:,:,366))
  call check_last_UV_W(l_switch,G2(:,:,:,366),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,412))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1226),Q(:,45),G2(:,:,:,367))
  call check_last_UV_W(l_switch,G2(:,:,:,367),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,413))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1335),Q(:,39),G2(:,:,:,368))
  call check_last_UV_W(l_switch,G2(:,:,:,368),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,414))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1338),Q(:,45),G2(:,:,:,369))
  call check_last_UV_W(l_switch,G2(:,:,:,369),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,415))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,148),Q(:,13),G2(:,:,:,370))
  call loop_UV_W(G2(:,:,:,370),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,133))
  call check_last_UV_W(l_switch,G3(:,:,:,133),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,128))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1342),Q(:,39),G2(:,:,:,371))
  call check_last_UV_W(l_switch,G2(:,:,:,371),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,416))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1343),Q(:,45),G2(:,:,:,372))
  call check_last_UV_W(l_switch,G2(:,:,:,372),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,417))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,279),Q(:,39),G2(:,:,:,373))
  call check_last_UV_W(l_switch,G2(:,:,:,373),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,418))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1253),Q(:,46),G2(:,:,:,374))
  call check_last_UV_W(l_switch,G2(:,:,:,374),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,419))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1347),Q(:,39),G2(:,:,:,375))
  call check_last_UV_W(l_switch,G2(:,:,:,375),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,420))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1350),Q(:,39),G2(:,:,:,376))
  call check_last_UV_W(l_switch,G2(:,:,:,376),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,421))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1352),Q(:,46),G2(:,:,:,377))
  call check_last_UV_W(l_switch,G2(:,:,:,377),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,422))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,202),Q(:,14),G2(:,:,:,378))
  call loop_UV_W(G2(:,:,:,378),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,134))
  call check_last_UV_W(l_switch,G3(:,:,:,134),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,129))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1355),Q(:,46),G2(:,:,:,379))
  call check_last_UV_W(l_switch,G2(:,:,:,379),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,423))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,281),Q(:,43),G2(:,:,:,380))
  call check_last_UV_W(l_switch,G2(:,:,:,380),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,424))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1274),Q(:,45),G2(:,:,:,381))
  call check_last_UV_W(l_switch,G2(:,:,:,381),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,425))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1359),Q(:,43),G2(:,:,:,382))
  call check_last_UV_W(l_switch,G2(:,:,:,382),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,426))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,124),Q(:,11),G2(:,:,:,383))
  call loop_UV_W(G2(:,:,:,383),Q(:,27),wf(:,-5),Q(:,32),G3(:,:,:,135))
  call check_last_UV_W(l_switch,G3(:,:,:,135),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,130))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1362),Q(:,45),G2(:,:,:,384))
  call check_last_UV_W(l_switch,G2(:,:,:,384),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,427))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,160),Q(:,13),G2(:,:,:,385))
  call loop_UV_W(G2(:,:,:,385),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,136))
  call check_last_UV_W(l_switch,G3(:,:,:,136),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,131))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1366),Q(:,43),G2(:,:,:,386))
  call check_last_UV_W(l_switch,G2(:,:,:,386),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,428))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1367),Q(:,45),G2(:,:,:,387))
  call check_last_UV_W(l_switch,G2(:,:,:,387),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,429))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,283),Q(:,43),G2(:,:,:,388))
  call check_last_UV_W(l_switch,G2(:,:,:,388),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,430))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1289),Q(:,46),G2(:,:,:,389))
  call check_last_UV_W(l_switch,G2(:,:,:,389),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,431))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1371),Q(:,43),G2(:,:,:,390))
  call check_last_UV_W(l_switch,G2(:,:,:,390),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,432))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,131),Q(:,11),G2(:,:,:,391))
  call loop_UV_W(G2(:,:,:,391),Q(:,27),wf(:,-5),Q(:,32),G3(:,:,:,137))
  call check_last_UV_W(l_switch,G3(:,:,:,137),Q(:,59),wf(:,-2),Q(:,4),G4tensor(:,132))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1374),Q(:,43),G2(:,:,:,392))
  call check_last_UV_W(l_switch,G2(:,:,:,392),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,433))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1376),Q(:,46),G2(:,:,:,393))
  call check_last_UV_W(l_switch,G2(:,:,:,393),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,434))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,214),Q(:,14),G2(:,:,:,394))
  call loop_UV_W(G2(:,:,:,394),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,138))
  call check_last_UV_W(l_switch,G3(:,:,:,138),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,133))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1379),Q(:,46),G2(:,:,:,395))
  call check_last_UV_W(l_switch,G2(:,:,:,395),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,435))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1312),Q(:,45),G2(:,:,:,396))
  call check_last_UV_W(l_switch,G2(:,:,:,396),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,436))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1305),Q(:,46),G2(:,:,:,397))
  call check_last_UV_W(l_switch,G2(:,:,:,397),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,437))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1384),Q(:,45),G2(:,:,:,398))
  call check_last_UV_W(l_switch,G2(:,:,:,398),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,438))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,167),Q(:,13),G2(:,:,:,399))
  call loop_UV_W(G2(:,:,:,399),Q(:,29),wf(:,-5),Q(:,32),G3(:,:,:,139))
  call check_last_UV_W(l_switch,G3(:,:,:,139),Q(:,61),wf(:,-1),Q(:,2),G4tensor(:,134))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1386),Q(:,45),G2(:,:,:,400))
  call check_last_UV_W(l_switch,G2(:,:,:,400),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,439))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1388),Q(:,46),G2(:,:,:,401))
  call check_last_UV_W(l_switch,G2(:,:,:,401),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,440))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,221),Q(:,14),G2(:,:,:,402))
  call loop_UV_W(G2(:,:,:,402),Q(:,30),wf(:,-5),Q(:,32),G3(:,:,:,140))
  call check_last_UV_W(l_switch,G3(:,:,:,140),Q(:,62),wf(:,0),Q(:,1),G4tensor(:,135))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1390),Q(:,46),G2(:,:,:,403))
  call check_last_UV_W(l_switch,G2(:,:,:,403),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,441))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1441),Q(:,39),G2(:,:,:,404))
  call check_last_UV_W(l_switch,G2(:,:,:,404),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,442))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1442),Q(:,43),G2(:,:,:,405))
  call check_last_UV_W(l_switch,G2(:,:,:,405),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,443))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1444),Q(:,39),G2(:,:,:,406))
  call check_last_UV_W(l_switch,G2(:,:,:,406),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,444))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1445),Q(:,45),G2(:,:,:,407))
  call check_last_UV_W(l_switch,G2(:,:,:,407),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,445))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1447),Q(:,43),G2(:,:,:,408))
  call check_last_UV_W(l_switch,G2(:,:,:,408),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,446))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1448),Q(:,45),G2(:,:,:,409))
  call check_last_UV_W(l_switch,G2(:,:,:,409),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,447))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1453),Q(:,39),G2(:,:,:,410))
  call check_last_UV_W(l_switch,G2(:,:,:,410),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,448))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1454),Q(:,43),G2(:,:,:,411))
  call check_last_UV_W(l_switch,G2(:,:,:,411),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,449))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1456),Q(:,39),G2(:,:,:,412))
  call check_last_UV_W(l_switch,G2(:,:,:,412),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,450))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1457),Q(:,43),G2(:,:,:,413))
  call check_last_UV_W(l_switch,G2(:,:,:,413),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,451))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1459),Q(:,46),G2(:,:,:,414))
  call check_last_UV_W(l_switch,G2(:,:,:,414),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,452))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1461),Q(:,46),G2(:,:,:,415))
  call check_last_UV_W(l_switch,G2(:,:,:,415),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,453))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1465),Q(:,39),G2(:,:,:,416))
  call check_last_UV_W(l_switch,G2(:,:,:,416),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,454))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1466),Q(:,39),G2(:,:,:,417))
  call check_last_UV_W(l_switch,G2(:,:,:,417),Q(:,55),wf(:,-3),Q(:,8),G3tensor(:,455))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1467),Q(:,45),G2(:,:,:,418))
  call check_last_UV_W(l_switch,G2(:,:,:,418),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,456))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1469),Q(:,45),G2(:,:,:,419))
  call check_last_UV_W(l_switch,G2(:,:,:,419),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,457))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1471),Q(:,46),G2(:,:,:,420))
  call check_last_UV_W(l_switch,G2(:,:,:,420),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,458))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1473),Q(:,46),G2(:,:,:,421))
  call check_last_UV_W(l_switch,G2(:,:,:,421),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,459))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1477),Q(:,43),G2(:,:,:,422))
  call check_last_UV_W(l_switch,G2(:,:,:,422),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,460))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1478),Q(:,43),G2(:,:,:,423))
  call check_last_UV_W(l_switch,G2(:,:,:,423),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,461))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1479),Q(:,45),G2(:,:,:,424))
  call check_last_UV_W(l_switch,G2(:,:,:,424),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,462))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1480),Q(:,45),G2(:,:,:,425))
  call check_last_UV_W(l_switch,G2(:,:,:,425),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,463))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1483),Q(:,46),G2(:,:,:,426))
  call check_last_UV_W(l_switch,G2(:,:,:,426),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,464))
  call loop_UV_W(G1(:,:,:,1),Q(:,16),wf(:,1484),Q(:,46),G2(:,:,:,427))
  call check_last_UV_W(l_switch,G2(:,:,:,427),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,465))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(4)*(-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(31)-M(32)+M(33)-M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)-M(41)+M(42) &
    +M(43)+M(44)+M(45)+M(46)-M(47)-M(48)+M(49)-M(50)-M(51)-M(52)+M(56)-M(59)-M(62)-M(68)-M(71)-M(74)+M(80)+M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(137)-M(153)-M(188)+M(248))) * den(11)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42) &
    +M(43)-M(44)+M(45)+M(46)-M(47)-M(48)+M(49)-M(50)-M(51)-M(52)-M(56)-M(59)-M(62)+M(68)-M(71)-M(74)+M(80)+M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(131)-M(154)-M(164)+M(250))) * den(11)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,2)
  Gcoeff = (c(5)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56)+M(68))+c(6)*(M(131)-M(137)+M(153)-M(154) &
    -M(164)+M(188)-M(248)+M(250))) * den(11)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,3)
  Gcoeff = (c(4)*(-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(31)-M(32)+M(33)-M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)-M(41)+M(42) &
    -M(43)+M(44)+M(45)-M(46)-M(47)-M(48)-M(49)+M(50)+M(51)+M(52)+M(56)-M(59)+M(62)-M(68)-M(71)+M(74)+M(80)+M(83)+M(86)-M(92)-M(95) &
    -M(98))+c(6)*(-M(143)+M(151)+M(212)-M(242))) * den(11)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,4)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42) &
    -M(43)-M(44)+M(45)-M(46)-M(47)-M(48)-M(49)+M(50)+M(51)+M(52)-M(56)-M(59)+M(62)+M(68)-M(71)+M(74)+M(80)+M(83)+M(86)-M(92)-M(95) &
    -M(98))+c(6)*(-M(145)+M(149)+M(218)-M(236))) * den(11)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,5)
  Gcoeff = (c(5)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56)+M(68))+c(6)*(M(143)-M(145)+M(149)-M(151) &
    -M(212)+M(218)-M(236)+M(242))) * den(11)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,6)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(137)-M(143)+M(151)+M(153) &
    +M(188)+M(212)-M(242)-M(248))) * den(11)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,7)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(131)-M(145)+M(149)+M(154) &
    +M(164)+M(218)-M(236)-M(250))) * den(11)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,8)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(143)-M(145)+M(149)-M(151)-M(153)+M(154)+M(164)-M(188)-M(212)+M(218)-M(236)+M(242)+M(248) &
    -M(250))) * den(11)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,9)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42) &
    -M(43)-M(44)-M(45)-M(46)+M(47)+M(48)+M(49)+M(50)+M(51)-M(52)-M(56)+M(59)+M(62)+M(68)+M(71)+M(74)-M(80)-M(83)-M(86)-M(92)-M(95) &
    +M(98))+c(6)*(-M(138)+M(147)+M(190)-M(224))) * den(11)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,10)
  Gcoeff = (c(4)*(M(25)-M(26)-M(27)+M(28)-M(29)+M(30)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42) &
    +M(43)-M(44)-M(45)-M(46)+M(47)+M(48)+M(49)-M(50)+M(51)-M(52)-M(56)+M(59)-M(62)+M(68)+M(71)+M(74)-M(80)-M(83)-M(86)+M(92)-M(95) &
    +M(98))+c(6)*(-M(141)+M(144)+M(200)-M(214))) * den(11)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,19)
  Gcoeff = (c(5)*(-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62)+M(92))+c(6)*(M(138)-M(141)+M(144)-M(147) &
    -M(190)+M(200)-M(214)+M(224))) * den(11)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,28)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)-M(41)-M(42) &
    -M(43)+M(44)+M(45)+M(46)+M(47)-M(48)+M(49)+M(50)-M(51)-M(52)+M(56)+M(59)+M(62)-M(68)-M(71)-M(74)-M(80)+M(83)-M(86)-M(92)+M(95) &
    +M(98))+c(6)*(-M(136)+M(140)+M(178)-M(196))) * den(11)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,11)
  Gcoeff = (c(4)*(M(25)-M(26)-M(27)+M(28)-M(29)+M(30)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)-M(41)-M(42) &
    +M(43)+M(44)+M(45)+M(46)+M(47)-M(48)+M(49)-M(50)-M(51)-M(52)+M(56)+M(59)-M(62)-M(68)-M(71)-M(74)-M(80)+M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(139)-M(150)-M(194)+M(238))) * den(11)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,20)
  Gcoeff = (c(5)*(-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62)+M(92))+c(6)*(M(136)+M(139)-M(140)-M(150) &
    -M(178)-M(194)+M(196)+M(238))) * den(11)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,29)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(-M(136)+M(138)+M(140)-M(147) &
    +M(178)-M(190)-M(196)+M(224))) * den(11)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,12)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(M(139)+M(141)-M(144)-M(150) &
    -M(194)-M(200)+M(214)+M(238))) * den(11)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,21)
  Gcoeff = (c(6)*(M(136)-M(138)+M(139)-M(140)+M(141)-M(144)+M(147)-M(150)-M(178)+M(190)-M(194)+M(196)-M(200)+M(214)-M(224) &
    +M(238))) * den(11)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,30)
  Gcoeff = (c(4)*(-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(31)-M(32)+M(33)-M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)-M(41)-M(42) &
    -M(43)+M(44)-M(45)-M(46)+M(47)+M(48)+M(49)+M(50)+M(51)-M(52)+M(56)+M(59)+M(62)-M(68)+M(71)+M(74)-M(80)-M(83)-M(86)-M(92)-M(95) &
    +M(98))+c(6)*(-M(132)+M(148)+M(166)-M(226))) * den(11)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,37)
  Gcoeff = (c(4)*(-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(31)-M(32)+M(33)-M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42) &
    +M(43)-M(44)-M(45)-M(46)-M(47)+M(48)+M(49)-M(50)+M(51)-M(52)-M(56)-M(59)-M(62)+M(68)+M(71)+M(74)+M(80)-M(83)-M(86)+M(92)-M(95) &
    +M(98))+c(6)*(M(134)-M(142)-M(172)+M(202))) * den(11)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,38)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(6)*(M(132)+M(134)-M(142)-M(148) &
    -M(166)-M(172)+M(202)+M(226))) * den(11)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,39)
  Gcoeff = (c(4)*(-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42) &
    -M(43)+M(44)-M(45)+M(46)+M(47)+M(48)+M(49)+M(50)-M(51)-M(52)+M(56)+M(59)+M(62)-M(68)+M(71)-M(74)-M(80)-M(83)-M(86)-M(92)+M(95) &
    +M(98))+c(6)*(-M(135)+M(146)+M(176)-M(220))) * den(11)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,46)
  Gcoeff = (c(4)*(-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42) &
    +M(43)-M(44)-M(45)+M(46)-M(47)+M(48)+M(49)-M(50)-M(51)-M(52)-M(56)-M(59)-M(62)+M(68)+M(71)-M(74)+M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(133)-M(152)-M(170)+M(244))) * den(11)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,47)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(6)*(M(133)+M(135)-M(146)-M(152) &
    -M(170)-M(176)+M(220)+M(244))) * den(11)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,48)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(132)-M(135)+M(146)-M(148) &
    -M(166)+M(176)-M(220)+M(226))) * den(11)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,55)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(133)-M(134)+M(142)-M(152) &
    -M(170)+M(172)-M(202)+M(244))) * den(11)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,56)
  Gcoeff = (c(6)*(-M(132)+M(133)-M(134)+M(135)+M(142)-M(146)+M(148)-M(152)+M(166)-M(170)+M(172)-M(176)-M(202)+M(220)-M(226) &
    +M(244))) * den(11)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,57)
  Gcoeff = (c(4)*(-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(17)-M(18)+M(21)-M(22)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)+M(44)-M(47) &
    -M(50)-M(53)+M(54)+M(55)+M(56)+M(57)+M(58)-M(59)-M(60)+M(61)-M(62)-M(63)-M(64)-M(65)-M(72)-M(75)+M(77)+M(84)-M(87)+M(89)+M(96) &
    +M(99))+c(6)*(M(161)-M(177)-M(182)+M(246))) * den(32)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,64)
  Gcoeff = (c(4)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)-M(44)-M(47)-M(50) &
    +M(53)+M(54)+M(55)-M(56)+M(57)+M(58)-M(59)-M(60)+M(61)-M(62)-M(63)-M(64)+M(65)-M(72)-M(75)+M(77)+M(84)-M(87)+M(89)+M(96) &
    +M(99))+c(6)*(-M(140)+M(155)-M(178)+M(249))) * den(32)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,65)
  Gcoeff = (c(5)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(44)+M(53)-M(56)+M(65))+c(6)*(-M(140)+M(155)-M(161)+M(177) &
    -M(178)+M(182)-M(246)+M(249))) * den(32)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,66)
  Gcoeff = (c(4)*(-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(17)-M(18)+M(21)-M(22)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)+M(44)-M(47) &
    +M(50)-M(53)+M(54)-M(55)+M(56)+M(57)-M(58)-M(59)-M(60)-M(61)+M(62)+M(63)+M(64)-M(65)-M(72)+M(75)+M(77)+M(84)+M(87)-M(89)-M(96) &
    -M(99))+c(6)*(-M(167)+M(175)+M(206)-M(240))) * den(32)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,67)
  Gcoeff = (c(4)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)-M(44)-M(47)+M(50) &
    +M(53)+M(54)-M(55)-M(56)+M(57)-M(58)-M(59)-M(60)-M(61)+M(62)+M(63)+M(64)+M(65)-M(72)+M(75)+M(77)+M(84)+M(87)-M(89)-M(96) &
    -M(99))+c(6)*(-M(169)+M(173)+M(216)-M(230))) * den(32)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,68)
  Gcoeff = (c(5)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(44)+M(53)-M(56)+M(65))+c(6)*(M(167)-M(169)+M(173)-M(175)-M(206) &
    +M(216)-M(230)+M(240))) * den(32)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,69)
  Gcoeff = (c(5)*(M(50)-M(55)-M(58)-M(61)+M(62)+M(63)+M(64)+M(75)+M(87)-M(89)-M(96)-M(99))+c(6)*(-M(161)-M(167)+M(175)+M(177) &
    +M(182)+M(206)-M(240)-M(246))) * den(32)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,70)
  Gcoeff = (c(5)*(M(50)-M(55)-M(58)-M(61)+M(62)+M(63)+M(64)+M(75)+M(87)-M(89)-M(96)-M(99))+c(6)*(M(140)-M(155)-M(169)+M(173) &
    +M(178)+M(216)-M(230)-M(249))) * den(32)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,71)
  Gcoeff = (c(6)*(M(140)-M(155)+M(161)+M(167)-M(169)+M(173)-M(175)-M(177)+M(178)-M(182)-M(206)+M(216)-M(230)+M(240)+M(246) &
    -M(249))) * den(32)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,72)
  Gcoeff = (c(4)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)-M(44)+M(47)+M(50) &
    +M(53)-M(54)-M(55)-M(56)-M(57)-M(58)+M(59)+M(60)+M(61)+M(62)+M(63)-M(64)+M(65)+M(72)+M(75)-M(77)-M(84)-M(87)-M(89)-M(96) &
    +M(99))+c(6)*(-M(162)+M(171)+M(184)-M(222))) * den(32)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,73)
  Gcoeff = (c(4)*(M(9)-M(10)-M(11)+M(12)-M(15)+M(16)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(37)+M(38)+M(39)-M(40)-M(44)+M(47)-M(50) &
    +M(53)-M(54)+M(55)-M(56)-M(57)-M(58)+M(59)+M(60)+M(61)-M(62)+M(63)-M(64)+M(65)+M(72)+M(75)-M(77)-M(84)-M(87)+M(89)-M(96) &
    +M(99))+c(6)*(-M(165)+M(168)+M(198)-M(208))) * den(32)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,82)
  Gcoeff = (c(5)*(-M(11)+M(12)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(50)+M(55)-M(62)+M(89))+c(6)*(M(162)-M(165)+M(168)-M(171) &
    -M(184)+M(198)-M(208)+M(222))) * den(32)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,91)
  Gcoeff = (c(4)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)+M(44)+M(47)+M(50) &
    -M(53)-M(54)-M(55)+M(56)+M(57)+M(58)+M(59)-M(60)+M(61)+M(62)-M(63)-M(64)-M(65)-M(72)-M(75)-M(77)+M(84)-M(87)-M(89)+M(96) &
    +M(99))+c(6)*(M(154)-M(160)+M(164)-M(195))) * den(32)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,74)
  Gcoeff = (c(4)*(M(9)-M(10)-M(11)+M(12)-M(15)+M(16)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(37)+M(38)+M(39)-M(40)+M(44)+M(47)-M(50) &
    -M(53)-M(54)+M(55)+M(56)+M(57)+M(58)+M(59)-M(60)+M(61)-M(62)-M(63)-M(64)-M(65)-M(72)-M(75)-M(77)+M(84)-M(87)+M(89)+M(96) &
    +M(99))+c(6)*(M(163)-M(174)-M(192)+M(232))) * den(32)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,83)
  Gcoeff = (c(5)*(-M(11)+M(12)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(50)+M(55)-M(62)+M(89))+c(6)*(-M(154)+M(160)+M(163)-M(164) &
    -M(174)-M(192)+M(195)+M(232))) * den(32)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,92)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(154)-M(160)+M(162)+M(164) &
    -M(171)-M(184)-M(195)+M(222))) * den(32)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,75)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(163)+M(165)-M(168)-M(174) &
    -M(192)-M(198)+M(208)+M(232))) * den(32)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,84)
  Gcoeff = (c(6)*(-M(154)+M(160)-M(162)+M(163)-M(164)+M(165)-M(168)+M(171)-M(174)+M(184)-M(192)+M(195)-M(198)+M(208)-M(222) &
    +M(232))) * den(32)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,93)
  Gcoeff = (c(4)*(-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(17)-M(18)+M(21)-M(22)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)+M(44)+M(47) &
    +M(50)-M(53)-M(54)-M(55)+M(56)-M(57)-M(58)+M(59)+M(60)+M(61)+M(62)+M(63)-M(64)-M(65)+M(72)+M(75)-M(77)-M(84)-M(87)-M(89)-M(96) &
    +M(99))+c(6)*(M(142)-M(156)+M(172)-M(225))) * den(32)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,40)
  Gcoeff = (c(4)*(-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(17)-M(18)+M(21)-M(22)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)-M(44)-M(47) &
    -M(50)+M(53)+M(54)+M(55)-M(56)-M(57)-M(58)-M(59)+M(60)+M(61)-M(62)+M(63)-M(64)+M(65)+M(72)+M(75)+M(77)-M(84)-M(87)+M(89)-M(96) &
    +M(99))+c(6)*(-M(148)+M(158)-M(166)+M(201))) * den(32)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,41)
  Gcoeff = (c(5)*(-M(44)-M(47)-M(50)+M(53)+M(54)+M(55)-M(56)-M(59)-M(62)+M(65)+M(77)+M(89))+c(6)*(-M(142)-M(148)+M(156)+M(158) &
    -M(166)-M(172)+M(201)+M(225))) * den(32)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,42)
  Gcoeff = (c(4)*(-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)+M(37)-M(38)-M(39)+M(40)+M(44)+M(47) &
    +M(50)-M(53)-M(54)-M(55)+M(56)-M(57)+M(58)+M(59)+M(60)+M(61)+M(62)-M(63)-M(64)-M(65)+M(72)-M(75)-M(77)-M(84)-M(87)-M(89)+M(96) &
    +M(99))+c(6)*(M(152)-M(159)+M(170)-M(219))) * den(32)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,49)
  Gcoeff = (c(4)*(-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)+M(37)-M(38)-M(39)+M(40)-M(44)-M(47) &
    -M(50)+M(53)+M(54)+M(55)-M(56)-M(57)+M(58)-M(59)+M(60)+M(61)-M(62)-M(63)-M(64)+M(65)+M(72)-M(75)+M(77)-M(84)-M(87)+M(89)+M(96) &
    +M(99))+c(6)*(-M(146)+M(157)-M(176)+M(243))) * den(32)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,50)
  Gcoeff = (c(5)*(-M(44)-M(47)-M(50)+M(53)+M(54)+M(55)-M(56)-M(59)-M(62)+M(65)+M(77)+M(89))+c(6)*(-M(146)-M(152)+M(157)+M(159) &
    -M(170)-M(176)+M(219)+M(243))) * den(32)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,51)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(37)-M(38)-M(39)+M(40)+M(58)-M(63)-M(75)+M(96))+c(6)*(-M(142)+M(152)+M(156)-M(159) &
    +M(170)-M(172)-M(219)+M(225))) * den(32)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,58)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(37)-M(38)-M(39)+M(40)+M(58)-M(63)-M(75)+M(96))+c(6)*(-M(146)+M(148)+M(157)-M(158) &
    +M(166)-M(176)-M(201)+M(243))) * den(32)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,59)
  Gcoeff = (c(6)*(M(142)-M(146)+M(148)-M(152)-M(156)+M(157)-M(158)+M(159)+M(166)-M(170)+M(172)-M(176)-M(201)+M(219)-M(225) &
    +M(243))) * den(32)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,60)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)-M(17)+M(18)+M(19)-M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)-M(48)-M(51) &
    -M(53)-M(60)-M(63)-M(65)+M(66)+M(67)+M(68)+M(69)+M(70)-M(71)-M(72)+M(73)-M(74)-M(75)-M(76)+M(78)+M(81)-M(88)+M(90)+M(93) &
    +M(100))+c(6)*(-M(158)+M(185)-M(201)+M(245))) * den(40)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,100)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)-M(48)-M(51) &
    +M(53)-M(60)-M(63)+M(65)+M(66)+M(67)-M(68)+M(69)+M(70)-M(71)-M(72)+M(73)-M(74)-M(75)-M(76)+M(78)+M(81)-M(88)+M(90)+M(93) &
    +M(100))+c(6)*(-M(134)+M(179)-M(202)+M(247))) * den(40)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,101)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(-M(134)+M(158)+M(179)-M(185)+M(201) &
    -M(202)-M(245)+M(247))) * den(40)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,102)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)-M(17)+M(18)+M(19)-M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)-M(48)+M(51) &
    -M(53)-M(60)+M(63)-M(65)+M(66)-M(67)+M(68)+M(69)-M(70)-M(71)-M(72)-M(73)+M(74)+M(75)+M(76)+M(78)+M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(-M(191)+M(199)+M(204)-M(234))) * den(40)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,103)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)-M(48)+M(51) &
    +M(53)-M(60)+M(63)+M(65)+M(66)-M(67)-M(68)+M(69)-M(70)-M(71)-M(72)-M(73)+M(74)+M(75)+M(76)+M(78)+M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(-M(193)+M(197)+M(210)-M(228))) * den(40)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,104)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(M(191)-M(193)+M(197)-M(199)-M(204) &
    +M(210)-M(228)+M(234))) * den(40)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,105)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(158)-M(185)-M(191)+M(199) &
    +M(201)+M(204)-M(234)-M(245))) * den(40)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,106)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(134)-M(179)-M(193)+M(197) &
    +M(202)+M(210)-M(228)-M(247))) * den(40)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,107)
  Gcoeff = (c(6)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(40)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,108)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(132)-M(156)-M(180) &
    +M(186)+M(221)-M(223)-M(225)+M(226))) * den(92)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,64)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(6)*(-M(132)+M(138)+M(162) &
    -M(186)-M(221)+M(222)+M(224)-M(226))) * den(92)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,65)
  Gcoeff = (c(5)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(6)*(-M(138)+M(156)-M(162) &
    +M(180)-M(222)+M(223)-M(224)+M(225))) * den(92)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,66)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(203)-M(209)-M(215) &
    +M(217)+M(227)-M(233)-M(239)+M(241))) * den(92)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,67)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(6)*(-M(203)+M(205)+M(211) &
    -M(217)-M(227)+M(229)+M(235)-M(241))) * den(92)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,68)
  Gcoeff = (c(5)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(6)*(-M(205)+M(209)-M(211) &
    +M(215)-M(229)+M(233)-M(235)+M(239))) * den(92)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,69)
  Gcoeff = (c(6)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(92)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,70)
  Gcoeff = (c(6)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(92)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,71)
  Gcoeff = (c(6)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(92)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,72)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(7)+M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)+M(43)-M(46)-M(49)-M(55) &
    -M(58)-M(61)+M(67)+M(70)+M(73)+M(79)+M(82)-M(85)-M(89)+M(90)+M(91)+M(92)+M(93)+M(94)-M(95)-M(96)-M(97)-M(98)-M(99)+M(100)) &
    +c(6)*(-M(157)+M(191)+M(234)-M(243))) * den(45)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,109)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)-M(43)-M(46)-M(49) &
    +M(55)-M(58)-M(61)+M(67)+M(70)+M(73)+M(79)+M(82)-M(85)+M(89)+M(90)+M(91)-M(92)+M(93)+M(94)-M(95)-M(96)-M(97)-M(98)-M(99) &
    +M(100))+c(6)*(-M(133)+M(193)+M(228)-M(244))) * den(45)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,110)
  Gcoeff = (c(5)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(6)*(-M(133)+M(157)-M(191)+M(193)+M(228) &
    -M(234)+M(243)-M(244))) * den(45)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,111)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(7)+M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)+M(43)+M(46)-M(49)-M(55) &
    +M(58)-M(61)-M(67)-M(70)-M(73)+M(79)+M(82)+M(85)-M(89)-M(90)+M(91)+M(92)-M(93)+M(94)+M(95)+M(96)+M(97)-M(98)-M(99)-M(100)) &
    +c(6)*(-M(185)+M(203)+M(241)-M(245))) * den(45)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,112)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)-M(43)+M(46)-M(49) &
    +M(55)+M(58)-M(61)-M(67)-M(70)-M(73)+M(79)+M(82)+M(85)+M(89)-M(90)+M(91)-M(92)-M(93)+M(94)+M(95)+M(96)+M(97)-M(98)-M(99) &
    -M(100))+c(6)*(-M(179)+M(209)+M(239)-M(247))) * den(45)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,113)
  Gcoeff = (c(5)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(6)*(-M(179)+M(185)-M(203)+M(209)+M(239) &
    -M(241)+M(245)-M(247))) * den(45)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,114)
  Gcoeff = (c(5)*(M(46)+M(58)-M(67)-M(70)-M(73)+M(85)-M(90)-M(93)+M(95)+M(96)+M(97)-M(100))+c(6)*(M(157)-M(185)-M(191)+M(203) &
    -M(234)+M(241)+M(243)-M(245))) * den(45)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,115)
  Gcoeff = (c(5)*(M(46)+M(58)-M(67)-M(70)-M(73)+M(85)-M(90)-M(93)+M(95)+M(96)+M(97)-M(100))+c(6)*(M(133)-M(179)-M(193)+M(209) &
    -M(228)+M(239)+M(244)-M(247))) * den(45)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,116)
  Gcoeff = (c(6)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(45)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,117)
  Gcoeff = (c(5)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(6)*(M(135)-M(159)+M(215) &
    -M(217)-M(219)+M(220)-M(227)+M(233))) * den(82)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,76)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)-M(92)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120))+c(6)*(-M(135)+M(149)+M(173) &
    -M(215)+M(216)+M(218)-M(220)-M(233))) * den(82)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,77)
  Gcoeff = (c(5)*(-M(50)+M(55)-M(62)+M(89)+M(102)+M(104)-M(108)-M(114)-M(119)-M(120)+M(121)+M(123))+c(6)*(-M(149)+M(159)-M(173) &
    -M(216)+M(217)-M(218)+M(219)+M(227))) * den(82)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,78)
  Gcoeff = (c(5)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(6)*(M(180)-M(186)-M(197) &
    +M(199)+M(204)-M(210)-M(221)+M(223))) * den(82)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,79)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)-M(92)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120))+c(6)*(-M(180)+M(183)+M(189) &
    -M(199)-M(204)+M(207)+M(213)-M(223))) * den(82)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,80)
  Gcoeff = (c(5)*(-M(50)+M(55)-M(62)+M(89)+M(102)+M(104)-M(108)-M(114)-M(119)-M(120)+M(121)+M(123))+c(6)*(-M(183)+M(186)-M(189) &
    +M(197)-M(207)+M(210)-M(213)+M(221))) * den(82)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,81)
  Gcoeff = (c(6)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(82)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,82)
  Gcoeff = (c(6)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(82)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,83)
  Gcoeff = (c(6)*(M(149)-M(159)+M(173)-M(183)+M(186)-M(189)+M(197)-M(207)+M(210)-M(213)+M(216)-M(217)+M(218)-M(219)+M(221) &
    -M(227))) * den(82)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,84)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(42)+M(51)+M(53)+M(55)+M(63)+M(65)+M(66)+M(69)+M(74)+M(75)+M(76)+M(78)+M(79)+M(80)+M(81)+M(82)+M(88)+M(89)+M(91)+M(94) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(197)+M(210)))
  T4sum(1:15,23) = T4sum(1:15,23) + Gcoeff * G2tensor(:,85)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(10)-M(12)-M(14)-M(15)-M(17)-M(20)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(38) &
    -M(39)+M(41)+M(43)+M(51)+M(54)+M(63)+M(66)+M(68)+M(69)+M(74)+M(75)+M(76)+M(77)+M(78)+M(79)+M(81)+M(82)+M(88)+M(91)+M(92)+M(94) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(199)+M(204)))
  T4sum(1:15,23) = T4sum(1:15,23) + Gcoeff * G2tensor(:,88)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(5)+M(6)-M(7)+M(8)+M(13)-M(14)+M(19)-M(20)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42)+M(43)-M(53) &
    +M(54)-M(55)-M(65)+M(68)+M(77)-M(80)-M(89)+M(92)+M(101)-M(102)-M(103)-M(104)+M(105)+M(106)-M(109)+M(111)+M(115)+M(117)-M(121) &
    -M(123))+c(6)*(-M(197)+M(199)+M(204)-M(210)))
  T4sum(1:15,23) = T4sum(1:15,23) + Gcoeff * G2tensor(:,91)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(42)+M(46)+M(53)+M(55)+M(58)+M(65)+M(66)+M(69)+M(78)+M(79)+M(80)+M(81)+M(82)+M(85)+M(89)+M(91)+M(94)+M(95)+M(96)+M(97) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(209)+M(239)))
  T4sum(1:15,23) = T4sum(1:15,23) + Gcoeff * G2tensor(:,86)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(10)-M(11)-M(13)-M(15)-M(17)-M(20)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(41)+M(43)+M(46)+M(54)+M(58)+M(66)+M(68)+M(69)+M(77)+M(78)+M(79)+M(81)+M(82)+M(85)+M(91)+M(92)+M(94)+M(95)+M(96)+M(97) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(203)+M(241)))
  T4sum(1:15,23) = T4sum(1:15,23) + Gcoeff * G2tensor(:,89)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(5)+M(6)-M(7)+M(8)-M(13)+M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42)+M(43)-M(53) &
    +M(54)-M(55)-M(65)+M(68)+M(77)-M(80)-M(89)+M(92)+M(101)+M(102)-M(103)-M(104)-M(105)+M(106)-M(109)-M(111)+M(115)+M(117)+M(121) &
    -M(123))+c(6)*(M(203)-M(209)-M(239)+M(241)))
  T4sum(1:15,23) = T4sum(1:15,23) + Gcoeff * G2tensor(:,92)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(197)+M(209)-M(210)+M(239)))
  T4sum(1:15,23) = T4sum(1:15,23) + Gcoeff * G2tensor(:,87)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)+M(112)-M(116)+M(118)+M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(199)+M(203)-M(204)+M(241)))
  T4sum(1:15,23) = T4sum(1:15,23) + Gcoeff * G2tensor(:,90)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(37)+M(38)+M(39)-M(40)+M(102)-M(105)-M(111)+M(121))+c(6)*(M(197)-M(199)+M(203)-M(204) &
    -M(209)+M(210)-M(239)+M(241)))
  T4sum(1:15,23) = T4sum(1:15,23) + Gcoeff * G2tensor(:,93)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)+M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)-M(66)-M(67)-M(68)-M(69)-M(70)+M(71)+M(72)+M(73)+M(74)+M(75)-M(76)-M(78)-M(81)-M(88)-M(90)-M(93) &
    +M(100))+c(6)*(M(160)-M(186)+M(195)-M(221))) * den(40)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,76)
  Gcoeff = (c(4)*(M(3)-M(4)-M(5)+M(6)-M(13)+M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)-M(41)+M(48)-M(51) &
    +M(53)+M(60)+M(63)+M(65)-M(66)+M(67)-M(68)-M(69)-M(70)+M(71)+M(72)+M(73)-M(74)+M(75)-M(76)-M(78)-M(81)-M(88)+M(90)-M(93) &
    +M(100))+c(6)*(M(174)-M(189)+M(192)-M(207))) * den(40)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,85)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(160)+M(174)+M(186)-M(189) &
    +M(192)-M(195)-M(207)+M(221))) * den(40)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,94)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)-M(63)-M(65)-M(66)-M(67)+M(68)+M(69)+M(70)+M(71)-M(72)+M(73)+M(74)-M(75)-M(76)-M(78)+M(81)-M(88)-M(90)+M(93) &
    +M(100))+c(6)*(M(153)-M(171)-M(184)+M(188))) * den(40)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,77)
  Gcoeff = (c(4)*(M(3)-M(4)-M(5)+M(6)-M(13)+M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)-M(51) &
    -M(53)-M(60)-M(63)-M(65)-M(66)+M(67)+M(68)+M(69)+M(70)+M(71)-M(72)+M(73)-M(74)-M(75)-M(76)-M(78)+M(81)-M(88)+M(90)+M(93) &
    +M(100))+c(6)*(-M(168)+M(187)-M(198)+M(231))) * den(40)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,86)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(153)-M(168)+M(171)+M(184) &
    +M(187)-M(188)-M(198)+M(231))) * den(40)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,95)
  Gcoeff = (c(5)*(M(41)-M(53)-M(60)-M(63)-M(65)+M(68)+M(69)+M(70)-M(72)-M(75)+M(81)+M(93))+c(6)*(M(153)-M(160)-M(171)-M(184) &
    +M(186)+M(188)-M(195)+M(221))) * den(40)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,78)
  Gcoeff = (c(5)*(M(41)-M(53)-M(60)-M(63)-M(65)+M(68)+M(69)+M(70)-M(72)-M(75)+M(81)+M(93))+c(6)*(-M(168)-M(174)+M(187)+M(189) &
    -M(192)-M(198)+M(207)+M(231))) * den(40)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,87)
  Gcoeff = (c(6)*(-M(153)+M(160)-M(168)+M(171)-M(174)+M(184)-M(186)+M(187)-M(188)+M(189)-M(192)+M(195)-M(198)+M(207)-M(221) &
    +M(231))) * den(40)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,96)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)-M(17)+M(18)+M(19)-M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)+M(63)-M(65)-M(66)-M(67)+M(68)-M(69)-M(70)+M(71)+M(72)+M(73)+M(74)+M(75)-M(76)-M(78)-M(81)-M(88)-M(90)-M(93) &
    +M(100))+c(6)*(M(136)-M(180)+M(196)-M(223))) * den(40)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,13)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)-M(17)+M(18)+M(19)-M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)-M(48)-M(51) &
    +M(53)+M(60)+M(63)+M(65)+M(66)+M(67)-M(68)-M(69)-M(70)-M(71)+M(72)+M(73)-M(74)+M(75)-M(76)+M(78)-M(81)-M(88)+M(90)-M(93) &
    +M(100))+c(6)*(-M(147)+M(177)+M(182)-M(190))) * den(40)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,14)
  Gcoeff = (c(5)*(-M(41)-M(48)-M(51)+M(53)+M(65)+M(66)+M(67)-M(68)-M(71)-M(74)+M(78)+M(90))+c(6)*(-M(136)-M(147)+M(177)+M(180) &
    +M(182)-M(190)-M(196)+M(223))) * den(40)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,15)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(41)+M(48)+M(51) &
    -M(53)+M(60)-M(63)-M(65)-M(66)-M(67)+M(68)-M(69)+M(70)+M(71)+M(72)+M(73)+M(74)-M(75)-M(76)-M(78)-M(81)-M(88)-M(90)+M(93) &
    +M(100))+c(6)*(M(150)-M(183)+M(194)-M(213))) * den(40)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,22)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)-M(41)-M(48)-M(51) &
    +M(53)+M(60)-M(63)+M(65)+M(66)+M(67)-M(68)-M(69)+M(70)-M(71)+M(72)+M(73)-M(74)-M(75)-M(76)+M(78)-M(81)-M(88)+M(90)+M(93) &
    +M(100))+c(6)*(-M(144)+M(181)-M(200)+M(237))) * den(40)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,23)
  Gcoeff = (c(5)*(-M(41)-M(48)-M(51)+M(53)+M(65)+M(66)+M(67)-M(68)-M(71)-M(74)+M(78)+M(90))+c(6)*(-M(144)-M(150)+M(181)+M(183) &
    -M(194)-M(200)+M(213)+M(237))) * den(40)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,24)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(136)+M(150)+M(180)-M(183)+M(194) &
    -M(196)-M(213)+M(223))) * den(40)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,31)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(144)+M(147)-M(177)+M(181)-M(182) &
    +M(190)-M(200)+M(237))) * den(40)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,32)
  Gcoeff = (c(6)*(M(136)-M(144)+M(147)-M(150)-M(177)-M(180)+M(181)-M(182)+M(183)+M(190)-M(194)+M(196)-M(200)+M(213)-M(223) &
    +M(237))) * den(40)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,33)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(7)-M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(25)+M(26)+M(29)-M(30)-M(43)+M(46)-M(49) &
    +M(55)+M(58)+M(61)-M(67)-M(70)-M(73)+M(79)-M(82)+M(85)+M(89)-M(90)+M(91)-M(92)-M(93)-M(94)+M(95)+M(96)+M(97)-M(98)+M(99) &
    -M(100))+c(6)*(M(167)-M(181)-M(237)+M(240))) * den(45)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,118)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(7)+M(8)-M(9)+M(10)-M(13)+M(14)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)-M(43)-M(46)-M(49)+M(55) &
    +M(58)+M(61)+M(67)-M(70)-M(73)+M(79)-M(82)+M(85)+M(89)+M(90)+M(91)-M(92)-M(93)-M(94)-M(95)+M(96)+M(97)-M(98)+M(99)-M(100)) &
    +c(6)*(-M(139)+M(169)+M(230)-M(238))) * den(45)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,119)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(139)-M(167)+M(169)+M(181)+M(230) &
    +M(237)-M(238)-M(240))) * den(45)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,120)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(7)-M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(25)+M(26)+M(29)-M(30)+M(43)+M(46)-M(49) &
    -M(55)-M(58)-M(61)-M(67)+M(70)-M(73)+M(79)+M(82)+M(85)-M(89)-M(90)+M(91)+M(92)+M(93)+M(94)+M(95)-M(96)+M(97)-M(98)-M(99) &
    -M(100))+c(6)*(-M(161)+M(205)+M(235)-M(246))) * den(45)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,121)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(7)+M(8)-M(9)+M(10)-M(13)+M(14)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)+M(43)-M(46)-M(49)-M(55) &
    -M(58)-M(61)+M(67)+M(70)-M(73)+M(79)+M(82)+M(85)-M(89)+M(90)+M(91)+M(92)+M(93)+M(94)-M(95)-M(96)+M(97)-M(98)-M(99)-M(100)) &
    +c(6)*(-M(155)+M(215)+M(233)-M(249))) * den(45)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,122)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(155)+M(161)-M(205)+M(215)+M(233) &
    -M(235)+M(246)-M(249))) * den(45)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,123)
  Gcoeff = (c(5)*(M(43)-M(55)-M(58)-M(61)+M(70)+M(82)-M(89)+M(92)+M(93)+M(94)-M(96)-M(99))+c(6)*(-M(161)-M(167)+M(181)+M(205) &
    +M(235)+M(237)-M(240)-M(246))) * den(45)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,124)
  Gcoeff = (c(5)*(M(43)-M(55)-M(58)-M(61)+M(70)+M(82)-M(89)+M(92)+M(93)+M(94)-M(96)-M(99))+c(6)*(M(139)-M(155)-M(169)+M(215) &
    -M(230)+M(233)+M(238)-M(249))) * den(45)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,125)
  Gcoeff = (c(6)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(45)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,126)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(141)-M(183)+M(209) &
    -M(211)-M(213)+M(214)-M(229)+M(239))) * den(65)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,97)
  Gcoeff = (c(5)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(6)*(-M(141)+M(151)+M(197) &
    -M(209)+M(210)+M(212)-M(214)-M(239))) * den(65)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,98)
  Gcoeff = (c(5)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(6)*(-M(151)+M(183)-M(197) &
    -M(210)+M(211)-M(212)+M(213)+M(229))) * den(65)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,99)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(156)-M(162)-M(173) &
    +M(175)+M(206)-M(216)-M(222)+M(225))) * den(65)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,100)
  Gcoeff = (c(5)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(6)*(-M(156)+M(159)+M(165) &
    -M(175)-M(206)+M(208)+M(219)-M(225))) * den(65)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,101)
  Gcoeff = (c(5)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(6)*(-M(159)+M(162)-M(165) &
    +M(173)-M(208)+M(216)-M(219)+M(222))) * den(65)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,102)
  Gcoeff = (c(6)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(65)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,103)
  Gcoeff = (c(6)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(65)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,104)
  Gcoeff = (c(6)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(65)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,105)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(45)+M(50)+M(53)+M(54)+M(57)+M(62)+M(63)+M(64)+M(65)+M(67)+M(75)+M(77)+M(79)+M(83)+M(84)+M(85)+M(87)+M(90)+M(91)+M(97) &
    +M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(173)+M(216)))
  T4sum(1:15,35) = T4sum(1:15,35) + Gcoeff * G2tensor(:,106)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(9)-M(11)-M(13)-M(16)-M(18)-M(19)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(37) &
    -M(40)+M(44)+M(46)+M(50)+M(54)+M(56)+M(57)+M(62)+M(63)+M(64)+M(66)+M(75)+M(77)+M(78)+M(79)+M(84)+M(85)+M(87)+M(91)+M(95)+M(97) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(175)+M(206)))
  T4sum(1:15,35) = T4sum(1:15,35) + Gcoeff * G2tensor(:,115)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)+M(31)-M(32)-M(35)+M(36)+M(44)-M(45)+M(46) &
    -M(53)+M(56)-M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)-M(103)+M(105)+M(107)-M(108)-M(109)-M(110)+M(111)+M(112)+M(113)+M(118) &
    -M(119)-M(124))+c(6)*(-M(173)+M(175)+M(206)-M(216)))
  T4sum(1:15,35) = T4sum(1:15,35) + Gcoeff * G2tensor(:,124)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(43)+M(45)+M(53)+M(54)+M(57)+M(65)+M(67)+M(70)+M(77)+M(79)+M(82)+M(83)+M(84)+M(85)+M(90)+M(91)+M(92)+M(93)+M(94)+M(97) &
    +M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(215)+M(233)))
  T4sum(1:15,35) = T4sum(1:15,35) + Gcoeff * G2tensor(:,107)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(5)-M(8)-M(10)-M(11)-M(13)-M(15)-M(18)-M(20)-M(22)-M(23)-M(25)-M(28)-M(30)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(43)+M(44)+M(46)+M(54)+M(56)+M(57)+M(66)+M(70)+M(77)+M(78)+M(79)+M(82)+M(84)+M(85)+M(91)+M(92)+M(93)+M(94)+M(95)+M(97) &
    +M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(205)+M(235)))
  T4sum(1:15,35) = T4sum(1:15,35) + Gcoeff * G2tensor(:,116)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)+M(44)-M(45)+M(46) &
    -M(53)+M(56)-M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)-M(103)-M(105)+M(107)+M(108)-M(109)-M(110)-M(111)+M(112)+M(113)+M(118) &
    +M(119)-M(124))+c(6)*(M(205)-M(215)-M(233)+M(235)))
  T4sum(1:15,35) = T4sum(1:15,35) + Gcoeff * G2tensor(:,125)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(27)-M(28)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(173)+M(215)-M(216)+M(233)))
  T4sum(1:15,35) = T4sum(1:15,35) + Gcoeff * G2tensor(:,108)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(27)-M(28)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)-M(105)+M(106)+M(108)-M(111)-M(114)+M(117)+M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(175)+M(205)-M(206)+M(235)))
  T4sum(1:15,35) = T4sum(1:15,35) + Gcoeff * G2tensor(:,117)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(31)+M(32)+M(35)-M(36)-M(105)+M(108)-M(111)+M(119))+c(6)*(M(173)-M(175)+M(205)-M(206) &
    -M(215)+M(216)-M(233)+M(235)))
  T4sum(1:15,35) = T4sum(1:15,35) + Gcoeff * G2tensor(:,126)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(7)+M(8)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)+M(25)-M(26)-M(29)+M(30)+M(43)+M(46)+M(49)-M(55) &
    +M(58)-M(61)-M(67)-M(70)-M(73)-M(79)+M(82)+M(85)-M(89)-M(90)-M(91)+M(92)-M(93)+M(94)+M(95)+M(96)+M(97)+M(98)-M(99)-M(100)) &
    +c(6)*(M(143)-M(187)-M(231)+M(242))) * den(45)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,127)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(7)+M(8)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)+M(25)-M(26)-M(29)+M(30)-M(43)-M(46)-M(49)+M(55) &
    +M(58)-M(61)+M(67)-M(70)-M(73)+M(79)+M(82)+M(85)+M(89)+M(90)+M(91)-M(92)-M(93)+M(94)-M(95)+M(96)+M(97)-M(98)-M(99)-M(100)) &
    +c(6)*(-M(137)+M(211)+M(229)-M(248))) * den(45)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,130)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(55)+M(67)+M(79)+M(89)+M(90)+M(91)-M(92)-M(95)-M(98))+c(6)*(-M(137)-M(143)+M(187)+M(211) &
    +M(229)+M(231)-M(242)-M(248))) * den(45)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,133)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)+M(43)+M(46)+M(49) &
    -M(55)-M(58)-M(61)-M(67)+M(70)-M(73)-M(79)+M(82)+M(85)-M(89)-M(90)-M(91)+M(92)+M(93)+M(94)+M(95)-M(96)+M(97)+M(98)-M(99) &
    -M(100))+c(6)*(M(145)-M(163)-M(232)+M(236))) * den(45)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,128)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)-M(25)+M(26)+M(29)-M(30)-M(43)-M(46)-M(49) &
    +M(55)-M(58)-M(61)+M(67)+M(70)-M(73)+M(79)+M(82)+M(85)+M(89)+M(90)+M(91)-M(92)+M(93)+M(94)-M(95)-M(96)+M(97)-M(98)-M(99) &
    -M(100))+c(6)*(-M(131)+M(217)+M(227)-M(250))) * den(45)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,131)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(55)+M(67)+M(79)+M(89)+M(90)+M(91)-M(92)-M(95)-M(98))+c(6)*(-M(131)-M(145)+M(163)+M(217) &
    +M(227)+M(232)-M(236)-M(250))) * den(45)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,134)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(6)*(-M(143)+M(145)-M(163)+M(187)+M(231) &
    -M(232)+M(236)-M(242))) * den(45)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,129)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(6)*(-M(131)+M(137)-M(211)+M(217)+M(227) &
    -M(229)+M(248)-M(250))) * den(45)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,132)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(45)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,135)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(45)+M(50)+M(51)+M(52)+M(57)+M(62)+M(68)+M(70)+M(74)+M(80)+M(82)+M(83)+M(84)+M(85)+M(86)+M(93)+M(94)+M(97) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(149)+M(218)))
  T4sum(1:15,40) = T4sum(1:15,40) + Gcoeff * G2tensor(:,136)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(9)-M(12)-M(14)-M(16)-M(18)-M(19)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(45)+M(55)+M(57)+M(67)+M(68)+M(70)+M(79)+M(80)+M(82)+M(83)+M(84)+M(85)+M(89)+M(90)+M(91)+M(93)+M(94)+M(97) &
    +M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(217)+M(227)))
  T4sum(1:15,40) = T4sum(1:15,40) + Gcoeff * G2tensor(:,137)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)+M(17)-M(18)-M(19)+M(20)-M(21)+M(22)-M(23)+M(24)-M(25)+M(26)+M(29)-M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)+M(102)+M(104)-M(108)+M(110)-M(114)-M(116)-M(119)-M(120)+M(121)-M(122) &
    +M(123)+M(124))+c(6)*(-M(149)+M(217)-M(218)+M(227)))
  T4sum(1:15,40) = T4sum(1:15,40) + Gcoeff * G2tensor(:,138)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(44)+M(45)+M(50)+M(51)+M(52)+M(56)+M(58)+M(62)+M(69)+M(74)+M(80)+M(81)+M(82)+M(83)+M(85)+M(86)+M(94)+M(96)+M(97) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(151)+M(212)))
  T4sum(1:15,40) = T4sum(1:15,40) + Gcoeff * G2tensor(:,145)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(6)-M(7)-M(9)-M(12)-M(14)-M(16)-M(17)-M(19)-M(21)-M(24)-M(26)-M(27)-M(29)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(44)+M(45)+M(55)+M(56)+M(58)+M(67)+M(69)+M(79)+M(80)+M(81)+M(82)+M(83)+M(85)+M(89)+M(90)+M(91)+M(94)+M(96)+M(97) &
    +M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(211)+M(229)))
  T4sum(1:15,40) = T4sum(1:15,40) + Gcoeff * G2tensor(:,146)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)-M(102)+M(104)+M(108)+M(110)-M(114)-M(116)+M(119)-M(120)-M(121)-M(122) &
    +M(123)+M(124))+c(6)*(-M(151)+M(211)-M(212)+M(229)))
  T4sum(1:15,40) = T4sum(1:15,40) + Gcoeff * G2tensor(:,147)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    -M(57)+M(58)-M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(149)+M(151)+M(212)-M(218)))
  T4sum(1:15,40) = T4sum(1:15,40) + Gcoeff * G2tensor(:,154)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(27)+M(28)-M(29)+M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    -M(57)+M(58)-M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(211)-M(217)-M(227)+M(229)))
  T4sum(1:15,40) = T4sum(1:15,40) + Gcoeff * G2tensor(:,155)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(102)+M(108)+M(119)-M(121))+c(6)*(M(149)-M(151)+M(211)-M(212) &
    -M(217)+M(218)-M(227)+M(229)))
  T4sum(1:15,40) = T4sum(1:15,40) + Gcoeff * G2tensor(:,156)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(165)-M(189)+M(203) &
    -M(205)-M(207)+M(208)-M(235)+M(241))) * den(28)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,163)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(132)-M(138)-M(149) &
    +M(151)+M(212)-M(218)-M(224)+M(226))) * den(28)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,164)
  Gcoeff = (c(6)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(28)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,165)
  Gcoeff = (c(5)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(165)+M(175)+M(199) &
    -M(203)+M(204)+M(206)-M(208)-M(241))) * den(28)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,166)
  Gcoeff = (c(5)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(132)+M(135)+M(141) &
    -M(151)-M(212)+M(214)+M(220)-M(226))) * den(28)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,167)
  Gcoeff = (c(6)*(-M(132)+M(135)+M(141)-M(151)+M(165)-M(175)-M(199)+M(203)-M(204)-M(206)+M(208)-M(212)+M(214)+M(220)-M(226) &
    +M(241))) * den(28)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,168)
  Gcoeff = (c(5)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(6)*(-M(175)+M(189)-M(199) &
    -M(204)+M(205)-M(206)+M(207)+M(235))) * den(28)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,169)
  Gcoeff = (c(5)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(6)*(-M(135)+M(138)-M(141) &
    +M(149)-M(214)+M(218)-M(220)+M(224))) * den(28)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,170)
  Gcoeff = (c(6)*(-M(135)+M(138)-M(141)+M(149)+M(175)-M(189)+M(199)+M(204)-M(205)+M(206)-M(207)-M(214)+M(218)-M(220)+M(224) &
    -M(235))) * den(28)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,171)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(5)+M(6)-M(7)+M(8)+M(13)-M(14)+M(19)-M(20)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42)-M(43)-M(53) &
    +M(54)+M(55)-M(65)+M(68)+M(77)-M(80)+M(89)-M(92)+M(101)+M(102)-M(103)+M(104)-M(105)-M(106)-M(109)-M(111)+M(115)-M(117)+M(121) &
    +M(123))+c(6)*(-M(180)+M(186)+M(221)-M(223))) * den(41)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,136)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)+M(5)-M(6)-M(7)+M(8)-M(13)+M(14)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)-M(41)-M(42)-M(43)+M(53) &
    +M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92)+M(101)+M(102)-M(103)+M(104)-M(105)-M(106)-M(109)-M(111)+M(115)-M(117)+M(121) &
    +M(123))+c(6)*(-M(136)+M(160)+M(195)-M(196))) * den(41)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,137)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(-M(136)+M(160)+M(180)-M(186)+M(195) &
    -M(196)-M(221)+M(223))) * den(41)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,138)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(5)+M(6)-M(7)+M(8)+M(13)-M(14)+M(19)-M(20)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42)+M(43)-M(53) &
    +M(54)-M(55)-M(65)+M(68)+M(77)-M(80)-M(89)+M(92)+M(101)-M(102)-M(103)-M(104)+M(105)+M(106)-M(109)+M(111)+M(115)+M(117)-M(121) &
    -M(123))+c(6)*(-M(197)+M(199)+M(204)-M(210))) * den(41)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,139)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)+M(5)-M(6)-M(7)+M(8)-M(13)+M(14)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)-M(41)-M(42)+M(43)+M(53) &
    +M(54)-M(55)+M(65)-M(68)+M(77)-M(80)-M(89)+M(92)+M(101)-M(102)-M(103)-M(104)+M(105)+M(106)-M(109)+M(111)+M(115)+M(117)-M(121) &
    -M(123))+c(6)*(M(191)-M(193)-M(228)+M(234))) * den(41)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,140)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(M(191)-M(193)+M(197)-M(199)-M(204) &
    +M(210)-M(228)+M(234))) * den(41)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,141)
  Gcoeff = (c(5)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(6)*(M(180)-M(186)-M(197) &
    +M(199)+M(204)-M(210)-M(221)+M(223))) * den(41)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,142)
  Gcoeff = (c(5)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(6)*(M(136)-M(160)+M(191) &
    -M(193)-M(195)+M(196)-M(228)+M(234))) * den(41)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,143)
  Gcoeff = (c(6)*(M(136)-M(160)-M(180)+M(186)+M(191)-M(193)-M(195)+M(196)+M(197)-M(199)-M(204)+M(210)+M(221)-M(223)-M(228) &
    +M(234))) * den(41)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,144)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)+M(5)-M(6)-M(7)+M(8)-M(13)+M(14)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)-M(41)+M(42)+M(43)+M(53) &
    -M(54)-M(55)+M(65)-M(68)-M(77)+M(80)-M(89)+M(92)-M(101)-M(102)+M(103)+M(104)+M(105)-M(106)+M(109)+M(111)-M(115)-M(117)-M(121) &
    +M(123))+c(6)*(M(179)-M(185)-M(245)+M(247))) * den(41)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,145)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(5)-M(6)+M(7)-M(8)+M(13)-M(14)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)-M(41)+M(42)-M(43)+M(53) &
    -M(54)+M(55)+M(65)-M(68)-M(77)+M(80)+M(89)-M(92)-M(101)-M(102)+M(103)+M(104)+M(105)-M(106)+M(109)+M(111)-M(115)-M(117)-M(121) &
    +M(123))+c(6)*(-M(203)+M(209)+M(239)-M(241))) * den(41)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,154)
  Gcoeff = (c(5)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(6)*(-M(179)+M(185)-M(203)+M(209)+M(239) &
    -M(241)+M(245)-M(247))) * den(41)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,163)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)+M(5)-M(6)-M(7)+M(8)-M(13)+M(14)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)+M(101)+M(102)-M(103)+M(104)-M(105)-M(106)-M(109)-M(111)+M(115)-M(117)+M(121) &
    +M(123))+c(6)*(M(131)-M(155)-M(249)+M(250))) * den(41)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,146)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(5)-M(6)+M(7)-M(8)+M(13)-M(14)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42)-M(43)-M(53) &
    -M(54)+M(55)-M(65)+M(68)-M(77)+M(80)+M(89)-M(92)+M(101)+M(102)-M(103)+M(104)-M(105)-M(106)-M(109)-M(111)+M(115)-M(117)+M(121) &
    +M(123))+c(6)*(-M(215)+M(217)+M(227)-M(233))) * den(41)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,155)
  Gcoeff = (c(5)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(6)*(-M(131)+M(155)-M(215)+M(217)+M(227) &
    -M(233)+M(249)-M(250))) * den(41)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,164)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(131)-M(155)-M(179) &
    +M(185)+M(245)-M(247)-M(249)+M(250))) * den(41)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,147)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(203)-M(209)-M(215) &
    +M(217)+M(227)-M(233)-M(239)+M(241))) * den(41)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,156)
  Gcoeff = (c(6)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(41)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,165)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(5)+M(6)-M(7)+M(8)+M(13)-M(14)+M(19)-M(20)+M(37)-M(38)-M(39)+M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)-M(101)-M(102)+M(103)+M(104)+M(105)-M(106)+M(109)+M(111)-M(115)-M(117)-M(121) &
    +M(123))+c(6)*(M(134)-M(158)-M(201)+M(202))) * den(41)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,43)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(5)+M(6)-M(7)+M(8)+M(13)-M(14)+M(19)-M(20)+M(37)-M(38)-M(39)+M(40)-M(41)-M(42)-M(43)+M(53) &
    +M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92)-M(101)-M(102)+M(103)+M(104)+M(105)-M(106)+M(109)+M(111)-M(115)-M(117)-M(121) &
    +M(123))+c(6)*(-M(132)+M(156)+M(225)-M(226))) * den(41)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,44)
  Gcoeff = (c(5)*(-M(41)-M(42)-M(43)+M(53)+M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92))+c(6)*(-M(132)-M(134)+M(156)+M(158) &
    +M(201)-M(202)+M(225)-M(226))) * den(41)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,45)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(5)+M(6)-M(7)+M(8)-M(13)+M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)-M(101)+M(102)+M(103)+M(104)-M(105)-M(106)+M(109)-M(111)-M(115)-M(117)+M(121) &
    +M(123))+c(6)*(M(133)-M(157)-M(243)+M(244))) * den(41)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,52)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(5)+M(6)-M(7)+M(8)-M(13)+M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42)-M(43)+M(53) &
    +M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92)-M(101)+M(102)+M(103)+M(104)-M(105)-M(106)+M(109)-M(111)-M(115)-M(117)+M(121) &
    +M(123))+c(6)*(-M(135)+M(159)+M(219)-M(220))) * den(41)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,53)
  Gcoeff = (c(5)*(-M(41)-M(42)-M(43)+M(53)+M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92))+c(6)*(-M(133)-M(135)+M(157)+M(159) &
    +M(219)-M(220)+M(243)-M(244))) * den(41)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,54)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(37)+M(38)+M(39)-M(40)+M(102)-M(105)-M(111)+M(121))+c(6)*(M(133)-M(134)-M(157)+M(158) &
    +M(201)-M(202)-M(243)+M(244))) * den(41)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,61)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(37)+M(38)+M(39)-M(40)+M(102)-M(105)-M(111)+M(121))+c(6)*(M(132)-M(135)-M(156)+M(159) &
    +M(219)-M(220)-M(225)+M(226))) * den(41)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,62)
  Gcoeff = (c(6)*(M(132)-M(133)+M(134)-M(135)-M(156)+M(157)-M(158)+M(159)-M(201)+M(202)+M(219)-M(220)-M(225)+M(226)+M(243) &
    -M(244))) * den(41)
  T3sum(1:5,25) = T3sum(1:5,25) + Gcoeff * G1tensor(:,63)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)+M(31)-M(32)-M(35)+M(36)+M(44)-M(45)-M(46) &
    -M(53)+M(56)-M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95)-M(103)-M(105)+M(107)+M(108)-M(109)+M(110)-M(111)-M(112)+M(113)-M(118) &
    +M(119)+M(124))+c(6)*(-M(156)+M(162)+M(222)-M(225))) * den(33)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,172)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)-M(45)-M(46) &
    +M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95)-M(103)-M(105)+M(107)+M(108)-M(109)+M(110)-M(111)-M(112)+M(113)-M(118) &
    +M(119)+M(124))+c(6)*(-M(142)+M(171)-M(172)+M(184))) * den(33)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,173)
  Gcoeff = (c(5)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(44)+M(53)-M(56)+M(65))+c(6)*(-M(142)+M(156)-M(162)+M(171) &
    -M(172)+M(184)-M(222)+M(225))) * den(33)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,174)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)+M(31)-M(32)-M(35)+M(36)+M(44)-M(45)+M(46) &
    -M(53)+M(56)-M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)-M(103)+M(105)+M(107)-M(108)-M(109)-M(110)+M(111)+M(112)+M(113)+M(118) &
    -M(119)-M(124))+c(6)*(-M(173)+M(175)+M(206)-M(216))) * den(33)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,175)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)-M(45)+M(46) &
    +M(53)-M(56)+M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)-M(103)+M(105)+M(107)-M(108)-M(109)-M(110)+M(111)+M(112)+M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(167)-M(169)-M(230)+M(240))) * den(33)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,176)
  Gcoeff = (c(5)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(44)+M(53)-M(56)+M(65))+c(6)*(M(167)-M(169)+M(173)-M(175)-M(206) &
    +M(216)-M(230)+M(240))) * den(33)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,177)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(156)-M(162)-M(173) &
    +M(175)+M(206)-M(216)-M(222)+M(225))) * den(33)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,178)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(142)+M(167)-M(169) &
    -M(171)+M(172)-M(184)-M(230)+M(240))) * den(33)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,179)
  Gcoeff = (c(6)*(M(142)-M(156)+M(162)+M(167)-M(169)-M(171)+M(172)+M(173)-M(175)-M(184)-M(206)+M(216)+M(222)-M(225)-M(230) &
    +M(240))) * den(33)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,180)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(148)-M(158)+M(161)+M(166) &
    -M(177)-M(182)-M(201)+M(246))) * den(5)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,196)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)-M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96))+c(6)*(M(147)-M(148)-M(161)-M(166) &
    +M(185)+M(190)+M(245)-M(246))) * den(5)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,197)
  Gcoeff = (c(5)*(-M(41)+M(53)+M(60)+M(63)+M(65)-M(68)-M(69)-M(70)+M(72)+M(75)-M(81)-M(93))+c(6)*(-M(147)+M(158)+M(177)+M(182) &
    -M(185)-M(190)+M(201)-M(245))) * den(5)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,198)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(163)+M(165)-M(168)-M(174) &
    -M(192)-M(198)+M(208)+M(232))) * den(5)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,199)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)-M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96))+c(6)*(-M(163)-M(165)+M(187)+M(189) &
    +M(207)-M(208)+M(231)-M(232))) * den(5)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,200)
  Gcoeff = (c(5)*(-M(41)+M(53)+M(60)+M(63)+M(65)-M(68)-M(69)-M(70)+M(72)+M(75)-M(81)-M(93))+c(6)*(M(168)+M(174)-M(187)-M(189) &
    +M(192)+M(198)-M(207)-M(231))) * den(5)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,201)
  Gcoeff = (c(6)*(-M(148)+M(158)-M(161)+M(163)+M(165)-M(166)-M(168)-M(174)+M(177)+M(182)-M(192)-M(198)+M(201)+M(208)+M(232) &
    -M(246))) * den(5)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,202)
  Gcoeff = (c(6)*(-M(147)+M(148)+M(161)-M(163)-M(165)+M(166)-M(185)+M(187)+M(189)-M(190)+M(207)-M(208)+M(231)-M(232)-M(245) &
    +M(246))) * den(5)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,203)
  Gcoeff = (c(6)*(M(147)-M(158)+M(168)+M(174)-M(177)-M(182)+M(185)-M(187)-M(189)+M(190)+M(192)+M(198)-M(201)-M(207)-M(231) &
    +M(245))) * den(5)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,204)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)+M(17)-M(18)-M(19)+M(20)-M(21)+M(22)-M(23)+M(24)-M(25)+M(26)+M(29)-M(30)+M(50)-M(51)-M(52) &
    -M(55)+M(62)+M(67)-M(74)+M(79)-M(86)-M(89)+M(90)+M(91)-M(102)-M(104)+M(108)+M(110)+M(114)-M(116)+M(119)+M(120)-M(121)-M(122) &
    -M(123)+M(124))+c(6)*(-M(159)+M(173)+M(216)-M(219))) * den(37)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,181)
  Gcoeff = (c(4)*(M(5)-M(6)-M(11)+M(12)-M(17)+M(18)-M(19)+M(20)+M(21)-M(22)+M(23)-M(24)-M(25)+M(26)+M(29)-M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)-M(102)-M(104)+M(108)+M(110)+M(114)-M(116)+M(119)+M(120)-M(121)-M(122) &
    -M(123)+M(124))+c(6)*(-M(152)+M(169)-M(170)+M(230))) * den(37)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,182)
  Gcoeff = (c(5)*(-M(11)+M(12)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(50)+M(55)-M(62)+M(89))+c(6)*(-M(152)+M(159)+M(169)-M(170) &
    -M(173)-M(216)+M(219)+M(230))) * den(37)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,183)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)+M(17)-M(18)-M(19)+M(20)-M(21)+M(22)-M(23)+M(24)-M(25)+M(26)+M(29)-M(30)+M(50)+M(51)-M(52) &
    -M(55)+M(62)-M(67)+M(74)+M(79)-M(86)-M(89)-M(90)+M(91)+M(102)-M(104)-M(108)-M(110)+M(114)+M(116)-M(119)+M(120)+M(121)+M(122) &
    -M(123)-M(124))+c(6)*(-M(162)+M(165)+M(208)-M(222))) * den(37)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,184)
  Gcoeff = (c(4)*(M(5)-M(6)-M(11)+M(12)-M(17)+M(18)-M(19)+M(20)+M(21)-M(22)+M(23)-M(24)-M(25)+M(26)+M(29)-M(30)-M(50)+M(51)-M(52) &
    +M(55)-M(62)-M(67)+M(74)+M(79)-M(86)+M(89)-M(90)+M(91)+M(102)-M(104)-M(108)-M(110)+M(114)+M(116)-M(119)+M(120)+M(121)+M(122) &
    -M(123)-M(124))+c(6)*(M(168)-M(171)-M(184)+M(198))) * den(37)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,185)
  Gcoeff = (c(5)*(-M(11)+M(12)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(50)+M(55)-M(62)+M(89))+c(6)*(M(162)-M(165)+M(168)-M(171) &
    -M(184)+M(198)-M(208)+M(222))) * den(37)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,186)
  Gcoeff = (c(5)*(M(51)-M(67)+M(74)-M(90)+M(102)-M(108)-M(110)+M(116)-M(119)+M(121)+M(122)-M(124))+c(6)*(M(159)-M(162)+M(165) &
    -M(173)+M(208)-M(216)+M(219)-M(222))) * den(37)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,187)
  Gcoeff = (c(5)*(M(51)-M(67)+M(74)-M(90)+M(102)-M(108)-M(110)+M(116)-M(119)+M(121)+M(122)-M(124))+c(6)*(M(152)+M(168)-M(169) &
    +M(170)-M(171)-M(184)+M(198)-M(230))) * den(37)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,188)
  Gcoeff = (c(6)*(M(152)-M(159)+M(162)-M(165)+M(168)-M(169)+M(170)-M(171)+M(173)-M(184)+M(198)-M(208)+M(216)-M(219)+M(222) &
    -M(230))) * den(37)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,189)
  Gcoeff = (c(5)*(M(50)-M(55)-M(58)-M(61)+M(62)+M(63)+M(64)+M(75)+M(87)-M(89)-M(96)-M(99))+c(6)*(M(146)-M(157)-M(163)+M(174) &
    +M(176)+M(192)-M(232)-M(243))) * den(7)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,208)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)-M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94))+c(6)*(M(145)-M(146)-M(174)-M(176) &
    +M(191)-M(192)+M(234)+M(236))) * den(7)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,209)
  Gcoeff = (c(5)*(-M(43)+M(55)+M(58)+M(61)-M(70)-M(82)+M(89)-M(92)-M(93)-M(94)+M(96)+M(99))+c(6)*(-M(145)+M(157)+M(163)-M(191) &
    +M(232)-M(234)-M(236)+M(243))) * den(7)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,210)
  Gcoeff = (c(5)*(M(50)-M(55)-M(58)-M(61)+M(62)+M(63)+M(64)+M(75)+M(87)-M(89)-M(96)-M(99))+c(6)*(-M(161)-M(167)+M(175)+M(177) &
    +M(182)+M(206)-M(240)-M(246))) * den(7)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,211)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)-M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94))+c(6)*(-M(175)-M(177)+M(181)-M(182) &
    +M(205)-M(206)+M(235)+M(237))) * den(7)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,212)
  Gcoeff = (c(5)*(-M(43)+M(55)+M(58)+M(61)-M(70)-M(82)+M(89)-M(92)-M(93)-M(94)+M(96)+M(99))+c(6)*(M(161)+M(167)-M(181)-M(205) &
    -M(235)-M(237)+M(240)+M(246))) * den(7)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,213)
  Gcoeff = (c(6)*(-M(146)+M(157)-M(161)+M(163)-M(167)-M(174)+M(175)-M(176)+M(177)+M(182)-M(192)+M(206)+M(232)-M(240)+M(243) &
    -M(246))) * den(7)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,214)
  Gcoeff = (c(6)*(-M(145)+M(146)+M(174)-M(175)+M(176)-M(177)+M(181)-M(182)-M(191)+M(192)+M(205)-M(206)-M(234)+M(235)-M(236) &
    +M(237))) * den(7)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,215)
  Gcoeff = (c(6)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(7)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,216)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(46)+M(47)+M(53)+M(55)+M(58)+M(59)+M(60)+M(61)+M(65)+M(66)+M(72)+M(78)+M(79)+M(85)+M(89)+M(91)+M(95)+M(96)+M(97)+M(99) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(167)+M(240)))
  T4sum(1:15,59) = T4sum(1:15,59) + Gcoeff * G2tensor(:,217)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(9)-M(11)-M(13)-M(16)-M(18)-M(19)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(37) &
    -M(40)+M(44)+M(46)+M(50)+M(54)+M(56)+M(57)+M(62)+M(63)+M(64)+M(66)+M(75)+M(77)+M(78)+M(79)+M(84)+M(85)+M(87)+M(91)+M(95)+M(97) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(175)+M(206)))
  T4sum(1:15,59) = T4sum(1:15,59) + Gcoeff * G2tensor(:,220)
  Gcoeff = (c(4)*(-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(17)-M(18)+M(21)-M(22)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)+M(44)-M(47) &
    +M(50)-M(53)+M(54)-M(55)+M(56)+M(57)-M(58)-M(59)-M(60)-M(61)+M(62)+M(63)+M(64)-M(65)-M(72)+M(75)+M(77)+M(84)+M(87)-M(89)-M(96) &
    -M(99))+c(6)*(-M(167)+M(175)+M(206)-M(240)))
  T4sum(1:15,59) = T4sum(1:15,59) + Gcoeff * G2tensor(:,223)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(47)+M(51)+M(53)+M(55)+M(59)+M(60)+M(61)+M(63)+M(65)+M(66)+M(72)+M(74)+M(75)+M(76)+M(78)+M(79)+M(88)+M(89)+M(91)+M(99) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(168)+M(198)))
  T4sum(1:15,59) = T4sum(1:15,59) + Gcoeff * G2tensor(:,218)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(9)-M(12)-M(14)-M(16)-M(18)-M(19)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(44)+M(50)+M(51)+M(54)+M(56)+M(57)+M(58)+M(62)+M(64)+M(66)+M(74)+M(76)+M(77)+M(78)+M(79)+M(84)+M(87)+M(88)+M(91)+M(96) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(165)+M(208)))
  T4sum(1:15,59) = T4sum(1:15,59) + Gcoeff * G2tensor(:,221)
  Gcoeff = (c(4)*(-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)+M(37)-M(38)-M(39)+M(40)+M(44)-M(47) &
    +M(50)-M(53)+M(54)-M(55)+M(56)+M(57)+M(58)-M(59)-M(60)-M(61)+M(62)-M(63)+M(64)-M(65)-M(72)-M(75)+M(77)+M(84)+M(87)-M(89)+M(96) &
    -M(99))+c(6)*(M(165)-M(168)-M(198)+M(208)))
  T4sum(1:15,59) = T4sum(1:15,59) + Gcoeff * G2tensor(:,224)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)-M(46)+M(51)-M(58) &
    +M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(167)+M(168)+M(198)-M(240)))
  T4sum(1:15,59) = T4sum(1:15,59) + Gcoeff * G2tensor(:,219)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(58) &
    -M(63)+M(74)-M(75)+M(76)-M(85)+M(88)-M(95)+M(96)-M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(165)-M(175)-M(206)+M(208)))
  T4sum(1:15,59) = T4sum(1:15,59) + Gcoeff * G2tensor(:,222)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(37)-M(38)-M(39)+M(40)+M(58)-M(63)-M(75)+M(96))+c(6)*(M(165)+M(167)-M(168)-M(175) &
    -M(198)-M(206)+M(208)+M(240)))
  T4sum(1:15,59) = T4sum(1:15,59) + Gcoeff * G2tensor(:,225)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)+M(45)+M(46) &
    +M(53)-M(56)+M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)+M(110)+M(111)-M(112)-M(113)-M(118) &
    -M(119)+M(124))+c(6)*(M(155)-M(161)-M(246)+M(249))) * den(33)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,148)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)+M(45)-M(46) &
    +M(53)-M(56)+M(65)-M(66)+M(67)-M(78)+M(83)+M(90)-M(95)+M(103)+M(105)-M(107)-M(108)+M(109)+M(110)+M(111)-M(112)-M(113)-M(118) &
    -M(119)+M(124))+c(6)*(-M(205)+M(215)+M(233)-M(235))) * den(33)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,157)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(155)+M(161)-M(205)+M(215)+M(233) &
    -M(235)+M(246)-M(249))) * den(33)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,166)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)-M(103)-M(105)+M(107)+M(108)-M(109)+M(110)-M(111)-M(112)+M(113)-M(118) &
    +M(119)+M(124))+c(6)*(M(137)-M(179)-M(247)+M(248))) * den(33)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,149)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)+M(44)+M(45)-M(46) &
    -M(53)+M(56)-M(65)-M(66)+M(67)-M(78)+M(83)+M(90)-M(95)-M(103)-M(105)+M(107)+M(108)-M(109)+M(110)-M(111)-M(112)+M(113)-M(118) &
    +M(119)+M(124))+c(6)*(-M(209)+M(211)+M(229)-M(239))) * den(33)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,158)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(137)+M(179)-M(209)+M(211)+M(229) &
    -M(239)+M(247)-M(248))) * den(33)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,167)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)-M(65)-M(103)-M(105)+M(107)+M(108)-M(109)-M(111)+M(113)+M(119))+c(6)*(M(137)-M(155)+M(161) &
    -M(179)+M(246)-M(247)+M(248)-M(249))) * den(33)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,150)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)-M(65)-M(103)-M(105)+M(107)+M(108)-M(109)-M(111)+M(113)+M(119))+c(6)*(M(205)-M(209)+M(211) &
    -M(215)+M(229)-M(233)+M(235)-M(239))) * den(33)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,159)
  Gcoeff = (c(6)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(33)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,168)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)+M(31)-M(32)-M(35)+M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)+M(110)+M(111)-M(112)-M(113)-M(118) &
    -M(119)+M(124))+c(6)*(M(140)-M(177)+M(178)-M(182))) * den(33)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,16)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)+M(31)-M(32)-M(35)+M(36)-M(44)-M(45)-M(46) &
    +M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95)+M(103)+M(105)-M(107)-M(108)+M(109)+M(110)+M(111)-M(112)-M(113)-M(118) &
    -M(119)+M(124))+c(6)*(-M(138)+M(180)+M(223)-M(224))) * den(33)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,17)
  Gcoeff = (c(5)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(6)*(-M(138)-M(140)+M(177)-M(178) &
    +M(180)+M(182)+M(223)-M(224))) * den(33)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,18)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)+M(103)-M(105)-M(107)+M(108)+M(109)+M(110)-M(111)-M(112)-M(113)-M(118) &
    +M(119)+M(124))+c(6)*(M(139)-M(181)-M(237)+M(238))) * den(33)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,25)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)-M(44)-M(45)-M(46) &
    +M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95)+M(103)-M(105)-M(107)+M(108)+M(109)+M(110)-M(111)-M(112)-M(113)-M(118) &
    +M(119)+M(124))+c(6)*(-M(141)+M(183)+M(213)-M(214))) * den(33)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,26)
  Gcoeff = (c(5)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(6)*(-M(139)-M(141)+M(181)+M(183) &
    +M(213)-M(214)+M(237)-M(238))) * den(33)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,27)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(31)+M(32)+M(35)-M(36)-M(105)+M(108)-M(111)+M(119))+c(6)*(M(139)-M(140)+M(177)-M(178) &
    -M(181)+M(182)-M(237)+M(238))) * den(33)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,34)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(31)+M(32)+M(35)-M(36)-M(105)+M(108)-M(111)+M(119))+c(6)*(M(138)-M(141)-M(180)+M(183) &
    +M(213)-M(214)-M(223)+M(224))) * den(33)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,35)
  Gcoeff = (c(6)*(M(138)-M(139)+M(140)-M(141)-M(177)+M(178)-M(180)+M(181)-M(182)+M(183)+M(213)-M(214)-M(223)+M(224)+M(237) &
    -M(238))) * den(33)
  T3sum(1:5,24) = T3sum(1:5,24) + Gcoeff * G1tensor(:,36)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(50)+M(51)-M(52) &
    +M(55)-M(62)-M(67)+M(74)+M(79)-M(86)+M(89)-M(90)+M(91)+M(102)+M(104)-M(108)-M(110)-M(114)+M(116)-M(119)-M(120)+M(121)+M(122) &
    +M(123)-M(124))+c(6)*(-M(183)+M(197)+M(210)-M(213))) * den(37)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,190)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(11)-M(12)+M(17)-M(18)+M(19)-M(20)-M(21)+M(22)-M(23)+M(24)+M(25)-M(26)-M(29)+M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)+M(102)+M(104)-M(108)-M(110)-M(114)+M(116)-M(119)-M(120)+M(121)+M(122) &
    +M(123)-M(124))+c(6)*(-M(150)+M(193)-M(194)+M(228))) * den(37)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,191)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(150)+M(183)+M(193)-M(194) &
    -M(197)-M(210)+M(213)+M(228))) * den(37)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,192)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)+M(50)+M(51)-M(52) &
    -M(55)+M(62)-M(67)+M(74)+M(79)-M(86)-M(89)-M(90)+M(91)-M(102)-M(104)+M(108)-M(110)+M(114)+M(116)+M(119)+M(120)-M(121)+M(122) &
    -M(123)-M(124))+c(6)*(-M(186)+M(189)+M(207)-M(221))) * den(37)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,193)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(11)-M(12)+M(17)-M(18)+M(19)-M(20)-M(21)+M(22)-M(23)+M(24)+M(25)-M(26)-M(29)+M(30)+M(50)-M(51)-M(52) &
    -M(55)+M(62)+M(67)-M(74)+M(79)-M(86)-M(89)+M(90)+M(91)-M(102)-M(104)+M(108)-M(110)+M(114)+M(116)+M(119)+M(120)-M(121)+M(122) &
    -M(123)-M(124))+c(6)*(-M(160)+M(174)+M(192)-M(195))) * den(37)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,194)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(160)+M(174)+M(186)-M(189) &
    +M(192)-M(195)-M(207)+M(221))) * den(37)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,195)
  Gcoeff = (c(5)*(M(50)-M(55)+M(62)-M(89)-M(102)-M(104)+M(108)+M(114)+M(119)+M(120)-M(121)-M(123))+c(6)*(M(183)-M(186)+M(189) &
    -M(197)+M(207)-M(210)+M(213)-M(221))) * den(37)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,196)
  Gcoeff = (c(5)*(M(50)-M(55)+M(62)-M(89)-M(102)-M(104)+M(108)+M(114)+M(119)+M(120)-M(121)-M(123))+c(6)*(M(150)-M(160)+M(174) &
    +M(192)-M(193)+M(194)-M(195)-M(228))) * den(37)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,197)
  Gcoeff = (c(6)*(M(150)-M(160)+M(174)-M(183)+M(186)-M(189)+M(192)-M(193)+M(194)-M(195)+M(197)-M(207)+M(210)-M(213)+M(221) &
    -M(228))) * den(37)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,198)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(144)+M(168)-M(181)-M(187) &
    +M(198)+M(200)-M(231)-M(237))) * den(9)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,229)
  Gcoeff = (c(5)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(6)*(M(143)-M(144)+M(167)-M(168) &
    -M(198)-M(200)+M(240)+M(242))) * den(9)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,230)
  Gcoeff = (c(5)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(6)*(-M(143)-M(167)+M(181)+M(187) &
    +M(231)+M(237)-M(240)-M(242))) * den(9)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,231)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(158)-M(185)-M(191)+M(199) &
    +M(201)+M(204)-M(234)-M(245))) * den(9)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,232)
  Gcoeff = (c(5)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(6)*(M(157)-M(158)-M(199)-M(201) &
    +M(203)-M(204)+M(241)+M(243))) * den(9)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,233)
  Gcoeff = (c(5)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(6)*(-M(157)+M(185)+M(191)-M(203) &
    +M(234)-M(241)-M(243)+M(245))) * den(9)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,234)
  Gcoeff = (c(6)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(9)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,235)
  Gcoeff = (c(6)*(-M(143)+M(144)+M(157)-M(158)-M(167)+M(168)+M(198)-M(199)+M(200)-M(201)+M(203)-M(204)-M(240)+M(241)-M(242) &
    +M(243))) * den(9)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,236)
  Gcoeff = (c(6)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(9)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,237)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(43)+M(48)+M(53)+M(54)+M(60)+M(65)+M(67)+M(70)+M(71)+M(72)+M(73)+M(77)+M(79)+M(82)+M(90)+M(91)+M(92)+M(93)+M(94) &
    +M(100)+M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(191)+M(234)))
  T4sum(1:15,71) = T4sum(1:15,71) + Gcoeff * G2tensor(:,238)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(10)-M(12)-M(14)-M(15)-M(17)-M(20)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(38) &
    -M(39)+M(41)+M(43)+M(51)+M(54)+M(63)+M(66)+M(68)+M(69)+M(74)+M(75)+M(76)+M(77)+M(78)+M(79)+M(81)+M(82)+M(88)+M(91)+M(92)+M(94) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(199)+M(204)))
  T4sum(1:15,71) = T4sum(1:15,71) + Gcoeff * G2tensor(:,247)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)-M(17)+M(18)+M(19)-M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)-M(48)+M(51) &
    -M(53)-M(60)+M(63)-M(65)+M(66)-M(67)+M(68)+M(69)-M(70)-M(71)-M(72)-M(73)+M(74)+M(75)+M(76)+M(78)+M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(-M(191)+M(199)+M(204)-M(234)))
  T4sum(1:15,71) = T4sum(1:15,71) + Gcoeff * G2tensor(:,256)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(48)+M(50)+M(53)+M(54)+M(60)+M(62)+M(63)+M(64)+M(65)+M(67)+M(71)+M(72)+M(73)+M(75)+M(77)+M(79)+M(87)+M(90)+M(91) &
    +M(100)+M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(174)+M(192)))
  T4sum(1:15,71) = T4sum(1:15,71) + Gcoeff * G2tensor(:,239)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(6)-M(7)-M(9)-M(12)-M(14)-M(16)-M(17)-M(19)-M(21)-M(24)-M(26)-M(27)-M(29)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(41)+M(50)+M(51)+M(54)+M(62)+M(64)+M(66)+M(68)+M(69)+M(70)+M(74)+M(76)+M(77)+M(78)+M(79)+M(81)+M(87)+M(88)+M(91)+M(93) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(189)+M(207)))
  T4sum(1:15,71) = T4sum(1:15,71) + Gcoeff * G2tensor(:,248)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(41)-M(48)+M(51) &
    -M(53)-M(60)-M(63)-M(65)+M(66)-M(67)+M(68)+M(69)+M(70)-M(71)-M(72)-M(73)+M(74)-M(75)+M(76)+M(78)+M(81)+M(88)-M(90)+M(93) &
    -M(100))+c(6)*(-M(174)+M(189)-M(192)+M(207)))
  T4sum(1:15,71) = T4sum(1:15,71) + Gcoeff * G2tensor(:,257)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)-M(43)+M(50)+M(62) &
    +M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(174)-M(191)+M(192)-M(234)))
  T4sum(1:15,71) = T4sum(1:15,71) + Gcoeff * G2tensor(:,240)
  Gcoeff = (c(4)*(M(5)-M(6)-M(9)+M(10)+M(15)-M(16)-M(19)+M(20)-M(27)+M(28)+M(31)-M(32)+M(33)-M(34)-M(35)+M(36)-M(43)+M(50)+M(62) &
    -M(63)+M(64)+M(70)-M(75)-M(82)+M(87)-M(92)+M(93)-M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(189)-M(199)-M(204)+M(207)))
  T4sum(1:15,71) = T4sum(1:15,71) + Gcoeff * G2tensor(:,249)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(174)+M(189)+M(191)-M(192)-M(199) &
    -M(204)+M(207)+M(234)))
  T4sum(1:15,71) = T4sum(1:15,71) + Gcoeff * G2tensor(:,258)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(11)+M(12)+M(17)-M(18)+M(19)-M(20)+M(21)-M(22)-M(23)+M(24)-M(25)+M(26)+M(29)-M(30)+M(50)+M(51)+M(52) &
    -M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91)+M(102)-M(104)-M(108)-M(110)+M(114)+M(116)-M(119)+M(120)+M(121)+M(122) &
    -M(123)-M(124))+c(6)*(M(151)-M(211)+M(212)-M(229))) * den(37)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,199)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(11)+M(12)+M(17)-M(18)+M(19)-M(20)+M(21)-M(22)-M(23)+M(24)-M(25)+M(26)+M(29)-M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)+M(102)-M(104)-M(108)-M(110)+M(114)+M(116)-M(119)+M(120)+M(121)+M(122) &
    -M(123)-M(124))+c(6)*(-M(153)+M(187)-M(188)+M(231))) * den(37)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,202)
  Gcoeff = (c(5)*(-M(50)-M(51)-M(52)+M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91))+c(6)*(-M(151)-M(153)+M(187)-M(188) &
    +M(211)-M(212)+M(229)+M(231))) * den(37)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,205)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(11)+M(12)-M(17)+M(18)+M(19)-M(20)+M(21)-M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)+M(50)+M(51)+M(52) &
    -M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91)-M(102)-M(104)+M(108)-M(110)+M(114)+M(116)+M(119)+M(120)-M(121)+M(122) &
    -M(123)-M(124))+c(6)*(M(149)-M(217)+M(218)-M(227))) * den(37)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,200)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(11)+M(12)-M(17)+M(18)+M(19)-M(20)+M(21)-M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)-M(102)-M(104)+M(108)-M(110)+M(114)+M(116)+M(119)+M(120)-M(121)+M(122) &
    -M(123)-M(124))+c(6)*(-M(154)+M(163)-M(164)+M(232))) * den(37)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,203)
  Gcoeff = (c(5)*(-M(50)-M(51)-M(52)+M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91))+c(6)*(-M(149)-M(154)+M(163)-M(164) &
    +M(217)-M(218)+M(227)+M(232))) * den(37)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,206)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(102)+M(108)+M(119)-M(121))+c(6)*(M(149)-M(151)+M(211)-M(212) &
    -M(217)+M(218)-M(227)+M(229))) * den(37)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,201)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(102)+M(108)+M(119)-M(121))+c(6)*(M(153)-M(154)+M(163)-M(164) &
    -M(187)+M(188)-M(231)+M(232))) * den(37)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,204)
  Gcoeff = (c(6)*(-M(149)+M(151)+M(153)-M(154)+M(163)-M(164)-M(187)+M(188)-M(211)+M(212)+M(217)-M(218)+M(227)-M(229)-M(231) &
    +M(232))) * den(37)
  T3sum(1:5,22) = T3sum(1:5,22) + Gcoeff * G1tensor(:,207)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(43)+M(44)+M(46)+M(47)+M(48)+M(49)+M(56)+M(57)+M(59)+M(70)+M(71)+M(82)+M(84)+M(85)+M(92)+M(93)+M(94)+M(95)+M(97)+M(98) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(145)+M(236)))
  T4sum(1:15,76) = T4sum(1:15,76) + Gcoeff * G2tensor(:,139)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(10)-M(11)-M(13)-M(15)-M(17)-M(20)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(44)+M(47)+M(48)+M(55)+M(56)+M(57)+M(58)+M(59)+M(61)+M(67)+M(71)+M(73)+M(79)+M(84)+M(89)+M(90)+M(91)+M(96)+M(99) &
    +M(100)+M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(163)+M(232)))
  T4sum(1:15,76) = T4sum(1:15,76) + Gcoeff * G2tensor(:,140)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(7)+M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)-M(43)-M(46)-M(49)+M(55) &
    +M(58)+M(61)+M(67)-M(70)+M(73)+M(79)-M(82)-M(85)+M(89)+M(90)+M(91)-M(92)-M(93)-M(94)-M(95)+M(96)-M(97)-M(98)+M(99)+M(100)) &
    +c(6)*(-M(145)+M(163)+M(232)-M(236)))
  T4sum(1:15,76) = T4sum(1:15,76) + Gcoeff * G2tensor(:,141)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(43)+M(46)+M(47)+M(48)+M(49)+M(58)+M(59)+M(68)+M(69)+M(71)+M(81)+M(82)+M(85)+M(92)+M(94)+M(95)+M(96)+M(97)+M(98) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(143)+M(242)))
  T4sum(1:15,76) = T4sum(1:15,76) + Gcoeff * G2tensor(:,148)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(5)-M(8)-M(10)-M(11)-M(13)-M(15)-M(18)-M(20)-M(22)-M(23)-M(25)-M(28)-M(30)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(47)+M(48)+M(55)+M(59)+M(61)+M(67)+M(68)+M(69)+M(70)+M(71)+M(73)+M(79)+M(81)+M(89)+M(90)+M(91)+M(93)+M(99) &
    +M(100)+M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(187)+M(231)))
  T4sum(1:15,76) = T4sum(1:15,76) + Gcoeff * G2tensor(:,149)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(7)-M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(25)+M(26)+M(29)-M(30)-M(43)-M(46)-M(49) &
    +M(55)-M(58)+M(61)+M(67)+M(70)+M(73)+M(79)-M(82)-M(85)+M(89)+M(90)+M(91)-M(92)+M(93)-M(94)-M(95)-M(96)-M(97)-M(98)+M(99) &
    +M(100))+c(6)*(-M(143)+M(187)+M(231)-M(242)))
  T4sum(1:15,76) = T4sum(1:15,76) + Gcoeff * G2tensor(:,150)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)+M(58)+M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(143)-M(145)-M(236)+M(242)))
  T4sum(1:15,76) = T4sum(1:15,76) + Gcoeff * G2tensor(:,157)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(163)+M(187)+M(231)-M(232)))
  T4sum(1:15,76) = T4sum(1:15,76) + Gcoeff * G2tensor(:,158)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(6)*(-M(143)+M(145)-M(163)+M(187)+M(231) &
    -M(232)+M(236)-M(242)))
  T4sum(1:15,76) = T4sum(1:15,76) + Gcoeff * G2tensor(:,159)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(165)-M(189)+M(203) &
    -M(205)-M(207)+M(208)-M(235)+M(241))) * den(28)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,268)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(143)-M(145)-M(147) &
    +M(148)+M(166)-M(190)-M(236)+M(242))) * den(28)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,269)
  Gcoeff = (c(6)*(M(143)-M(145)-M(147)+M(148)-M(165)+M(166)+M(189)-M(190)-M(203)+M(205)+M(207)-M(208)+M(235)-M(236)-M(241) &
    +M(242))) * den(28)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,270)
  Gcoeff = (c(5)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(165)+M(175)+M(199) &
    -M(203)+M(204)+M(206)-M(208)-M(241))) * den(28)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,271)
  Gcoeff = (c(5)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(143)+M(144)+M(146) &
    -M(148)-M(166)+M(176)+M(200)-M(242))) * den(28)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,272)
  Gcoeff = (c(6)*(-M(143)+M(144)+M(146)-M(148)+M(165)-M(166)-M(175)+M(176)-M(199)+M(200)+M(203)-M(204)-M(206)+M(208)+M(241) &
    -M(242))) * den(28)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,273)
  Gcoeff = (c(5)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(6)*(-M(175)+M(189)-M(199) &
    -M(204)+M(205)-M(206)+M(207)+M(235))) * den(28)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,274)
  Gcoeff = (c(5)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(6)*(-M(144)+M(145)-M(146) &
    +M(147)-M(176)+M(190)-M(200)+M(236))) * den(28)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,275)
  Gcoeff = (c(6)*(-M(144)+M(145)-M(146)+M(147)+M(175)-M(176)-M(189)+M(190)+M(199)-M(200)+M(204)-M(205)+M(206)-M(207)-M(235) &
    +M(236))) * den(28)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,276)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    -M(57)-M(58)-M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(132)+M(138)+M(224)-M(226))) * den(12)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,208)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(147)-M(148)-M(166)+M(190))) * den(12)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,209)
  Gcoeff = (c(5)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56)+M(68))+c(6)*(M(132)-M(138)+M(147)-M(148) &
    -M(166)+M(190)-M(224)+M(226))) * den(12)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,210)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    -M(57)+M(58)-M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(149)+M(151)+M(212)-M(218))) * den(12)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,211)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)+M(58)+M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(143)-M(145)-M(236)+M(242))) * den(12)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,212)
  Gcoeff = (c(5)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56)+M(68))+c(6)*(M(143)-M(145)+M(149)-M(151) &
    -M(212)+M(218)-M(236)+M(242))) * den(12)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,213)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(132)-M(138)-M(149) &
    +M(151)+M(212)-M(218)-M(224)+M(226))) * den(12)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,214)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(143)-M(145)-M(147) &
    +M(148)+M(166)-M(190)-M(236)+M(242))) * den(12)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,215)
  Gcoeff = (c(6)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(12)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,216)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(-M(134)+M(137)+M(142)-M(153) &
    +M(172)-M(188)-M(202)+M(248))) * den(2)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,280)
  Gcoeff = (c(5)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(6)*(-M(137)-M(142)+M(171)-M(172) &
    +M(179)+M(184)+M(247)-M(248))) * den(2)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,281)
  Gcoeff = (c(5)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(6)*(M(134)+M(153)-M(171)-M(179) &
    -M(184)+M(188)+M(202)-M(247))) * den(2)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,282)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(M(139)+M(141)-M(144)-M(150) &
    -M(194)-M(200)+M(214)+M(238))) * den(2)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,283)
  Gcoeff = (c(5)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(6)*(-M(139)-M(141)+M(181)+M(183) &
    +M(213)-M(214)+M(237)-M(238))) * den(2)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,284)
  Gcoeff = (c(5)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(6)*(M(144)+M(150)-M(181)-M(183) &
    +M(194)+M(200)-M(213)-M(237))) * den(2)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,285)
  Gcoeff = (c(6)*(M(134)-M(137)+M(139)+M(141)-M(142)-M(144)-M(150)+M(153)-M(172)+M(188)-M(194)-M(200)+M(202)+M(214)+M(238) &
    -M(248))) * den(2)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,286)
  Gcoeff = (c(6)*(M(137)-M(139)-M(141)+M(142)-M(171)+M(172)-M(179)+M(181)+M(183)-M(184)+M(213)-M(214)+M(237)-M(238)-M(247) &
    +M(248))) * den(2)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,287)
  Gcoeff = (c(6)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(2)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,288)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(27)-M(28)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(43)+M(50)+M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)-M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(135)+M(149)+M(218)-M(220))) * den(16)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,217)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(145)-M(146)-M(176)+M(236))) * den(16)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,218)
  Gcoeff = (c(5)*(-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62)+M(92))+c(6)*(M(135)+M(145)-M(146)-M(149) &
    -M(176)-M(218)+M(220)+M(236))) * den(16)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,219)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(27)-M(28)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(43)+M(50)+M(62) &
    +M(63)-M(64)-M(70)+M(75)+M(82)-M(87)-M(92)-M(93)+M(94)+M(105)-M(106)-M(108)+M(111)+M(114)-M(117)-M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(138)+M(141)+M(214)-M(224))) * den(16)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,220)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    +M(63)-M(64)-M(70)+M(75)+M(82)-M(87)+M(92)-M(93)+M(94)+M(105)-M(106)-M(108)+M(111)+M(114)-M(117)-M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(144)-M(147)-M(190)+M(200))) * den(16)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,221)
  Gcoeff = (c(5)*(-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62)+M(92))+c(6)*(M(138)-M(141)+M(144)-M(147) &
    -M(190)+M(200)-M(214)+M(224))) * den(16)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,222)
  Gcoeff = (c(5)*(M(63)-M(70)+M(75)-M(93)+M(105)-M(108)+M(111)-M(119)-M(125)+M(127)+M(129)-M(130))+c(6)*(M(135)-M(138)+M(141) &
    -M(149)+M(214)-M(218)+M(220)-M(224))) * den(16)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,223)
  Gcoeff = (c(5)*(M(63)-M(70)+M(75)-M(93)+M(105)-M(108)+M(111)-M(119)-M(125)+M(127)+M(129)-M(130))+c(6)*(M(144)-M(145)+M(146) &
    -M(147)+M(176)-M(190)+M(200)-M(236))) * den(16)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,224)
  Gcoeff = (c(6)*(-M(135)+M(138)-M(141)+M(144)-M(145)+M(146)-M(147)+M(149)+M(176)-M(190)+M(200)-M(214)+M(218)-M(220)+M(224) &
    -M(236))) * den(16)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,225)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(133)-M(139)+M(150)+M(152) &
    +M(170)+M(194)-M(238)-M(244))) * den(4)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,292)
  Gcoeff = (c(5)*(-M(50)-M(51)-M(52)+M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91))+c(6)*(-M(150)-M(152)+M(169)-M(170) &
    +M(193)-M(194)+M(228)+M(230))) * den(4)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,293)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(55)-M(67)-M(79)-M(89)-M(90)-M(91)+M(92)+M(95)+M(98))+c(6)*(M(133)+M(139)-M(169)-M(193) &
    -M(228)-M(230)+M(238)+M(244))) * den(4)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,294)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(137)-M(143)+M(151)+M(153) &
    +M(188)+M(212)-M(242)-M(248))) * den(4)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,295)
  Gcoeff = (c(5)*(-M(50)-M(51)-M(52)+M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91))+c(6)*(-M(151)-M(153)+M(187)-M(188) &
    +M(211)-M(212)+M(229)+M(231))) * den(4)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,296)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(55)-M(67)-M(79)-M(89)-M(90)-M(91)+M(92)+M(95)+M(98))+c(6)*(M(137)+M(143)-M(187)-M(211) &
    -M(229)-M(231)+M(242)+M(248))) * den(4)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,297)
  Gcoeff = (c(6)*(M(133)-M(137)+M(139)-M(143)-M(150)+M(151)-M(152)+M(153)-M(170)+M(188)-M(194)+M(212)+M(238)-M(242)+M(244) &
    -M(248))) * den(4)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,298)
  Gcoeff = (c(6)*(M(150)-M(151)+M(152)-M(153)-M(169)+M(170)+M(187)-M(188)-M(193)+M(194)+M(211)-M(212)-M(228)+M(229)-M(230) &
    +M(231))) * den(4)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,299)
  Gcoeff = (c(6)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(4)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,300)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(42)+M(44)+M(45)+M(46)+M(50)+M(52)+M(56)+M(62)+M(63)+M(69)+M(75)+M(76)+M(80)+M(81)+M(82)+M(83)+M(86)+M(88)+M(94)+M(95) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(141)+M(214)))
  T4sum(1:15,86) = T4sum(1:15,86) + Gcoeff * G2tensor(:,301)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(41)+M(43)+M(47)+M(48)+M(49)+M(51)+M(59)+M(63)+M(68)+M(69)+M(71)+M(74)+M(75)+M(76)+M(81)+M(82)+M(88)+M(92)+M(94)+M(98) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(144)+M(200)))
  T4sum(1:15,86) = T4sum(1:15,86) + Gcoeff * G2tensor(:,302)
  Gcoeff = (c(4)*(M(25)-M(26)-M(27)+M(28)-M(29)+M(30)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42) &
    +M(43)-M(44)-M(45)-M(46)+M(47)+M(48)+M(49)-M(50)+M(51)-M(52)-M(56)+M(59)-M(62)+M(68)+M(71)+M(74)-M(80)-M(83)-M(86)+M(92)-M(95) &
    +M(98))+c(6)*(-M(141)+M(144)+M(200)-M(214)))
  T4sum(1:15,86) = T4sum(1:15,86) + Gcoeff * G2tensor(:,303)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(44)+M(45)+M(50)+M(51)+M(52)+M(56)+M(58)+M(62)+M(69)+M(74)+M(80)+M(81)+M(82)+M(83)+M(85)+M(86)+M(94)+M(96)+M(97) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(151)+M(212)))
  T4sum(1:15,86) = T4sum(1:15,86) + Gcoeff * G2tensor(:,304)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(43)+M(46)+M(47)+M(48)+M(49)+M(58)+M(59)+M(68)+M(69)+M(71)+M(81)+M(82)+M(85)+M(92)+M(94)+M(95)+M(96)+M(97)+M(98) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(143)+M(242)))
  T4sum(1:15,86) = T4sum(1:15,86) + Gcoeff * G2tensor(:,305)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42) &
    +M(43)-M(44)-M(45)+M(46)+M(47)+M(48)+M(49)-M(50)-M(51)-M(52)-M(56)+M(59)-M(62)+M(68)+M(71)-M(74)-M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(143)-M(151)-M(212)+M(242)))
  T4sum(1:15,86) = T4sum(1:15,86) + Gcoeff * G2tensor(:,306)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(58) &
    -M(63)+M(74)-M(75)-M(76)+M(85)-M(88)-M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(141)+M(151)+M(212)-M(214)))
  T4sum(1:15,86) = T4sum(1:15,86) + Gcoeff * G2tensor(:,307)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(143)-M(144)-M(200)+M(242)))
  T4sum(1:15,86) = T4sum(1:15,86) + Gcoeff * G2tensor(:,308)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(141)+M(143)-M(144)-M(151) &
    -M(200)-M(212)+M(214)+M(242)))
  T4sum(1:15,86) = T4sum(1:15,86) + Gcoeff * G2tensor(:,309)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(6)*(M(131)+M(136)-M(140)-M(154) &
    -M(164)-M(178)+M(196)+M(250))) * den(1)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,310)
  Gcoeff = (c(5)*(-M(41)-M(42)-M(43)+M(53)+M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92))+c(6)*(-M(131)-M(136)+M(155)+M(160) &
    +M(195)-M(196)+M(249)-M(250))) * den(1)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,311)
  Gcoeff = (c(5)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(6)*(M(140)+M(154)-M(155)-M(160) &
    +M(164)+M(178)-M(195)-M(249))) * den(1)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,312)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(6)*(M(133)+M(135)-M(146)-M(152) &
    -M(170)-M(176)+M(220)+M(244))) * den(1)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,313)
  Gcoeff = (c(5)*(-M(41)-M(42)-M(43)+M(53)+M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92))+c(6)*(-M(133)-M(135)+M(157)+M(159) &
    +M(219)-M(220)+M(243)-M(244))) * den(1)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,314)
  Gcoeff = (c(5)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(6)*(M(146)+M(152)-M(157)-M(159) &
    +M(170)+M(176)-M(219)-M(243))) * den(1)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,315)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(135)-M(136)+M(140)-M(146)-M(152)+M(154)+M(164)-M(170)-M(176)+M(178)-M(196)+M(220)+M(244) &
    -M(250))) * den(1)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,316)
  Gcoeff = (c(6)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(1)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,317)
  Gcoeff = (c(6)*(-M(140)+M(146)+M(152)-M(154)+M(155)-M(157)-M(159)+M(160)-M(164)+M(170)+M(176)-M(178)+M(195)-M(219)-M(243) &
    +M(249))) * den(1)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,318)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(6)*(M(132)+M(134)-M(142)-M(148) &
    -M(166)-M(172)+M(202)+M(226))) * den(1)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,319)
  Gcoeff = (c(5)*(-M(41)-M(42)-M(43)+M(53)+M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92))+c(6)*(-M(132)-M(134)+M(156)+M(158) &
    +M(201)-M(202)+M(225)-M(226))) * den(1)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,320)
  Gcoeff = (c(5)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(6)*(M(142)+M(148)-M(156)-M(158) &
    +M(166)+M(172)-M(201)-M(225))) * den(1)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,321)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(6)*(M(131)+M(136)-M(140)-M(154) &
    -M(164)-M(178)+M(196)+M(250))) * den(1)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,322)
  Gcoeff = (c(5)*(-M(41)-M(42)-M(43)+M(53)+M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92))+c(6)*(-M(131)-M(136)+M(155)+M(160) &
    +M(195)-M(196)+M(249)-M(250))) * den(1)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,323)
  Gcoeff = (c(5)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(6)*(M(140)+M(154)-M(155)-M(160) &
    +M(164)+M(178)-M(195)-M(249))) * den(1)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,324)
  Gcoeff = (c(6)*(M(131)-M(132)-M(134)+M(136)-M(140)+M(142)+M(148)-M(154)-M(164)+M(166)+M(172)-M(178)+M(196)-M(202)-M(226) &
    +M(250))) * den(1)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,325)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(134)-M(136)+M(155)-M(156)-M(158)+M(160)+M(195)-M(196)-M(201)+M(202)-M(225)+M(226)+M(249) &
    -M(250))) * den(1)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,326)
  Gcoeff = (c(6)*(M(140)-M(142)-M(148)+M(154)-M(155)+M(156)+M(158)-M(160)+M(164)-M(166)-M(172)+M(178)-M(195)+M(201)+M(225) &
    -M(249))) * den(1)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,327)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(6)*(M(132)+M(134)-M(142)-M(148) &
    -M(166)-M(172)+M(202)+M(226))) * den(1)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,22)
  Gcoeff = (c(5)*(-M(41)-M(42)-M(43)+M(53)+M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92))+c(6)*(-M(132)-M(134)+M(156)+M(158) &
    +M(201)-M(202)+M(225)-M(226))) * den(1)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,23)
  Gcoeff = (c(5)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(6)*(M(142)+M(148)-M(156)-M(158) &
    +M(166)+M(172)-M(201)-M(225))) * den(1)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,24)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(6)*(M(133)+M(135)-M(146)-M(152) &
    -M(170)-M(176)+M(220)+M(244))) * den(1)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,28)
  Gcoeff = (c(5)*(-M(41)-M(42)-M(43)+M(53)+M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92))+c(6)*(-M(133)-M(135)+M(157)+M(159) &
    +M(219)-M(220)+M(243)-M(244))) * den(1)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,29)
  Gcoeff = (c(5)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(6)*(M(146)+M(152)-M(157)-M(159) &
    +M(170)+M(176)-M(219)-M(243))) * den(1)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,30)
  Gcoeff = (c(6)*(-M(132)+M(133)-M(134)+M(135)+M(142)-M(146)+M(148)-M(152)+M(166)-M(170)+M(172)-M(176)-M(202)+M(220)-M(226) &
    +M(244))) * den(1)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,34)
  Gcoeff = (c(6)*(M(132)-M(133)+M(134)-M(135)-M(156)+M(157)-M(158)+M(159)-M(201)+M(202)+M(219)-M(220)-M(225)+M(226)+M(243) &
    -M(244))) * den(1)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,35)
  Gcoeff = (c(6)*(-M(142)+M(146)-M(148)+M(152)+M(156)-M(157)+M(158)-M(159)-M(166)+M(170)-M(172)+M(176)+M(201)-M(219)+M(225) &
    -M(243))) * den(1)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,36)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(58) &
    -M(63)+M(74)-M(75)-M(76)+M(85)-M(88)-M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(141)+M(151)+M(212)-M(214))) * den(22)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,226)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(143)-M(144)-M(200)+M(242))) * den(22)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,227)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(141)+M(143)-M(144)-M(151) &
    -M(200)-M(212)+M(214)+M(242))) * den(22)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,228)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)-M(58) &
    +M(63)+M(74)+M(75)-M(76)+M(85)-M(88)-M(95)-M(96)+M(97)-M(102)+M(105)+M(111)-M(112)+M(116)-M(118)-M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(132)+M(135)+M(220)-M(226))) * den(22)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,229)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(58) &
    +M(63)-M(74)+M(75)-M(76)+M(85)-M(88)+M(95)-M(96)+M(97)-M(102)+M(105)+M(111)-M(112)+M(116)-M(118)-M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(146)-M(148)-M(166)+M(176))) * den(22)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,230)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(132)-M(135)+M(146)-M(148) &
    -M(166)+M(176)-M(220)+M(226))) * den(22)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,231)
  Gcoeff = (c(5)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(132)+M(135)+M(141) &
    -M(151)-M(212)+M(214)+M(220)-M(226))) * den(22)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,232)
  Gcoeff = (c(5)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(143)+M(144)+M(146) &
    -M(148)-M(166)+M(176)+M(200)-M(242))) * den(22)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,233)
  Gcoeff = (c(6)*(M(132)-M(135)-M(141)-M(143)+M(144)+M(146)-M(148)+M(151)-M(166)+M(176)+M(200)+M(212)-M(214)-M(220)+M(226) &
    -M(242))) * den(22)
  T3sum(1:5,81) = T3sum(1:5,81) + Gcoeff * G1tensor(:,234)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(133)-M(139)+M(150)+M(152) &
    +M(170)+M(194)-M(238)-M(244))) * den(4)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,403)
  Gcoeff = (c(5)*(-M(50)-M(51)-M(52)+M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91))+c(6)*(-M(150)-M(152)+M(169)-M(170) &
    +M(193)-M(194)+M(228)+M(230))) * den(4)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,404)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(55)-M(67)-M(79)-M(89)-M(90)-M(91)+M(92)+M(95)+M(98))+c(6)*(M(133)+M(139)-M(169)-M(193) &
    -M(228)-M(230)+M(238)+M(244))) * den(4)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,405)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(131)-M(145)+M(149)+M(154) &
    +M(164)+M(218)-M(236)-M(250))) * den(4)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,406)
  Gcoeff = (c(5)*(-M(50)-M(51)-M(52)+M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91))+c(6)*(-M(149)-M(154)+M(163)-M(164) &
    +M(217)-M(218)+M(227)+M(232))) * den(4)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,407)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(55)-M(67)-M(79)-M(89)-M(90)-M(91)+M(92)+M(95)+M(98))+c(6)*(M(131)+M(145)-M(163)-M(217) &
    -M(227)-M(232)+M(236)+M(250))) * den(4)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,408)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(139)-M(145)+M(149)-M(150)-M(152)+M(154)+M(164)-M(170)-M(194)+M(218)-M(236)+M(238)+M(244) &
    -M(250))) * den(4)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,409)
  Gcoeff = (c(6)*(-M(149)+M(150)+M(152)-M(154)+M(163)-M(164)-M(169)+M(170)-M(193)+M(194)+M(217)-M(218)+M(227)-M(228)-M(230) &
    +M(232))) * den(4)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,410)
  Gcoeff = (c(6)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(4)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,411)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(41)+M(42)+M(43)+M(45)+M(51)+M(52)+M(57)+M(63)+M(64)+M(68)+M(74)+M(75)+M(80)+M(83)+M(84)+M(85)+M(86)+M(87)+M(92)+M(97) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(135)+M(220)))
  T4sum(1:15,92) = T4sum(1:15,92) + Gcoeff * G2tensor(:,412)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(44)+M(46)+M(47)+M(48)+M(49)+M(50)+M(56)+M(57)+M(59)+M(62)+M(63)+M(64)+M(71)+M(75)+M(84)+M(85)+M(87)+M(95)+M(97)+M(98) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(146)+M(176)))
  T4sum(1:15,92) = T4sum(1:15,92) + Gcoeff * G2tensor(:,413)
  Gcoeff = (c(4)*(-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42) &
    -M(43)+M(44)-M(45)+M(46)+M(47)+M(48)+M(49)+M(50)-M(51)-M(52)+M(56)+M(59)+M(62)-M(68)+M(71)-M(74)-M(80)-M(83)-M(86)-M(92)+M(95) &
    +M(98))+c(6)*(-M(135)+M(146)+M(176)-M(220)))
  T4sum(1:15,92) = T4sum(1:15,92) + Gcoeff * G2tensor(:,414)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(45)+M(50)+M(51)+M(52)+M(57)+M(62)+M(68)+M(70)+M(74)+M(80)+M(82)+M(83)+M(84)+M(85)+M(86)+M(93)+M(94)+M(97) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(149)+M(218)))
  T4sum(1:15,92) = T4sum(1:15,92) + Gcoeff * G2tensor(:,415)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(43)+M(44)+M(46)+M(47)+M(48)+M(49)+M(56)+M(57)+M(59)+M(70)+M(71)+M(82)+M(84)+M(85)+M(92)+M(93)+M(94)+M(95)+M(97)+M(98) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(145)+M(236)))
  T4sum(1:15,92) = T4sum(1:15,92) + Gcoeff * G2tensor(:,416)
  Gcoeff = (c(4)*(-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42) &
    +M(43)+M(44)-M(45)+M(46)+M(47)+M(48)+M(49)-M(50)-M(51)-M(52)+M(56)+M(59)-M(62)-M(68)+M(71)-M(74)-M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(145)-M(149)-M(218)+M(236)))
  T4sum(1:15,92) = T4sum(1:15,92) + Gcoeff * G2tensor(:,417)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(27)-M(28)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(43)+M(50)+M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)-M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(135)+M(149)+M(218)-M(220)))
  T4sum(1:15,92) = T4sum(1:15,92) + Gcoeff * G2tensor(:,418)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(145)-M(146)-M(176)+M(236)))
  T4sum(1:15,92) = T4sum(1:15,92) + Gcoeff * G2tensor(:,419)
  Gcoeff = (c(5)*(-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62)+M(92))+c(6)*(M(135)+M(145)-M(146)-M(149) &
    -M(176)-M(218)+M(220)+M(236)))
  T4sum(1:15,92) = T4sum(1:15,92) + Gcoeff * G2tensor(:,420)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(-M(136)+M(138)+M(140)-M(147) &
    +M(178)-M(190)-M(196)+M(224))) * den(2)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,328)
  Gcoeff = (c(5)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(6)*(-M(138)-M(140)+M(177)-M(178) &
    +M(180)+M(182)+M(223)-M(224))) * den(2)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,329)
  Gcoeff = (c(5)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(6)*(M(136)+M(147)-M(177)-M(180) &
    -M(182)+M(190)+M(196)-M(223))) * den(2)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,330)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(-M(134)+M(137)+M(142)-M(153) &
    +M(172)-M(188)-M(202)+M(248))) * den(2)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,331)
  Gcoeff = (c(5)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(6)*(-M(137)-M(142)+M(171)-M(172) &
    +M(179)+M(184)+M(247)-M(248))) * den(2)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,332)
  Gcoeff = (c(5)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(6)*(M(134)+M(153)-M(171)-M(179) &
    -M(184)+M(188)+M(202)-M(247))) * den(2)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,333)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(137)-M(138)-M(140)+M(142)+M(147)-M(153)+M(172)-M(178)-M(188)+M(190)+M(196)-M(202)-M(224) &
    +M(248))) * den(2)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,334)
  Gcoeff = (c(6)*(-M(137)+M(138)+M(140)-M(142)+M(171)-M(172)-M(177)+M(178)+M(179)-M(180)-M(182)+M(184)-M(223)+M(224)+M(247) &
    -M(248))) * den(2)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,335)
  Gcoeff = (c(6)*(M(134)-M(136)-M(147)+M(153)-M(171)+M(177)-M(179)+M(180)+M(182)-M(184)+M(188)-M(190)-M(196)+M(202)+M(223) &
    -M(247))) * den(2)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,336)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(-M(136)+M(138)+M(140)-M(147) &
    +M(178)-M(190)-M(196)+M(224))) * den(2)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(5)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(6)*(-M(138)-M(140)+M(177)-M(178) &
    +M(180)+M(182)+M(223)-M(224))) * den(2)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(5)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(6)*(M(136)+M(147)-M(177)-M(180) &
    -M(182)+M(190)+M(196)-M(223))) * den(2)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,6)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(M(139)+M(141)-M(144)-M(150) &
    -M(194)-M(200)+M(214)+M(238))) * den(2)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(5)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(6)*(-M(139)-M(141)+M(181)+M(183) &
    +M(213)-M(214)+M(237)-M(238))) * den(2)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,11)
  Gcoeff = (c(5)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(6)*(M(144)+M(150)-M(181)-M(183) &
    +M(194)+M(200)-M(213)-M(237))) * den(2)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,12)
  Gcoeff = (c(6)*(M(136)-M(138)+M(139)-M(140)+M(141)-M(144)+M(147)-M(150)-M(178)+M(190)-M(194)+M(196)-M(200)+M(214)-M(224) &
    +M(238))) * den(2)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,16)
  Gcoeff = (c(6)*(M(138)-M(139)+M(140)-M(141)-M(177)+M(178)-M(180)+M(181)-M(182)+M(183)+M(213)-M(214)-M(223)+M(224)+M(237) &
    -M(238))) * den(2)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,17)
  Gcoeff = (c(6)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(2)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,18)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(45)+M(50)+M(51)+M(52)+M(57)+M(62)+M(68)+M(70)+M(74)+M(80)+M(82)+M(83)+M(84)+M(85)+M(86)+M(93)+M(94)+M(97) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(149)+M(218)))
  T4sum(1:15,99) = T4sum(1:15,99) + Gcoeff * G2tensor(:,142)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(43)+M(44)+M(46)+M(47)+M(48)+M(49)+M(56)+M(57)+M(59)+M(70)+M(71)+M(82)+M(84)+M(85)+M(92)+M(93)+M(94)+M(95)+M(97)+M(98) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(145)+M(236)))
  T4sum(1:15,99) = T4sum(1:15,99) + Gcoeff * G2tensor(:,143)
  Gcoeff = (c(4)*(-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42) &
    +M(43)+M(44)-M(45)+M(46)+M(47)+M(48)+M(49)-M(50)-M(51)-M(52)+M(56)+M(59)-M(62)-M(68)+M(71)-M(74)-M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(145)-M(149)-M(218)+M(236)))
  T4sum(1:15,99) = T4sum(1:15,99) + Gcoeff * G2tensor(:,144)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(44)+M(45)+M(50)+M(51)+M(52)+M(56)+M(58)+M(62)+M(69)+M(74)+M(80)+M(81)+M(82)+M(83)+M(85)+M(86)+M(94)+M(96)+M(97) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(151)+M(212)))
  T4sum(1:15,99) = T4sum(1:15,99) + Gcoeff * G2tensor(:,151)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(43)+M(46)+M(47)+M(48)+M(49)+M(58)+M(59)+M(68)+M(69)+M(71)+M(81)+M(82)+M(85)+M(92)+M(94)+M(95)+M(96)+M(97)+M(98) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(143)+M(242)))
  T4sum(1:15,99) = T4sum(1:15,99) + Gcoeff * G2tensor(:,152)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42) &
    +M(43)-M(44)-M(45)+M(46)+M(47)+M(48)+M(49)-M(50)-M(51)-M(52)-M(56)+M(59)-M(62)+M(68)+M(71)-M(74)-M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(143)-M(151)-M(212)+M(242)))
  T4sum(1:15,99) = T4sum(1:15,99) + Gcoeff * G2tensor(:,153)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    -M(57)+M(58)-M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(149)+M(151)+M(212)-M(218)))
  T4sum(1:15,99) = T4sum(1:15,99) + Gcoeff * G2tensor(:,160)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)+M(58)+M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(143)-M(145)-M(236)+M(242)))
  T4sum(1:15,99) = T4sum(1:15,99) + Gcoeff * G2tensor(:,161)
  Gcoeff = (c(5)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56)+M(68))+c(6)*(M(143)-M(145)+M(149)-M(151) &
    -M(212)+M(218)-M(236)+M(242)))
  T4sum(1:15,99) = T4sum(1:15,99) + Gcoeff * G2tensor(:,162)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(137)-M(143)+M(151)+M(153) &
    +M(188)+M(212)-M(242)-M(248))) * den(4)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,439)
  Gcoeff = (c(5)*(-M(50)-M(51)-M(52)+M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91))+c(6)*(-M(151)-M(153)+M(187)-M(188) &
    +M(211)-M(212)+M(229)+M(231))) * den(4)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,442)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(55)-M(67)-M(79)-M(89)-M(90)-M(91)+M(92)+M(95)+M(98))+c(6)*(M(137)+M(143)-M(187)-M(211) &
    -M(229)-M(231)+M(242)+M(248))) * den(4)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,445)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(131)-M(145)+M(149)+M(154) &
    +M(164)+M(218)-M(236)-M(250))) * den(4)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,440)
  Gcoeff = (c(5)*(-M(50)-M(51)-M(52)+M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91))+c(6)*(-M(149)-M(154)+M(163)-M(164) &
    +M(217)-M(218)+M(227)+M(232))) * den(4)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,443)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(55)-M(67)-M(79)-M(89)-M(90)-M(91)+M(92)+M(95)+M(98))+c(6)*(M(131)+M(145)-M(163)-M(217) &
    -M(227)-M(232)+M(236)+M(250))) * den(4)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,446)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(143)-M(145)+M(149)-M(151)-M(153)+M(154)+M(164)-M(188)-M(212)+M(218)-M(236)+M(242)+M(248) &
    -M(250))) * den(4)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,441)
  Gcoeff = (c(6)*(-M(149)+M(151)+M(153)-M(154)+M(163)-M(164)-M(187)+M(188)-M(211)+M(212)+M(217)-M(218)+M(227)-M(229)-M(231) &
    +M(232))) * den(4)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,444)
  Gcoeff = (c(6)*(M(131)-M(137)-M(143)+M(145)-M(163)+M(187)+M(211)-M(217)-M(227)+M(229)+M(231)-M(232)+M(236)-M(242)-M(248) &
    +M(250))) * den(4)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,447)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(132)-M(138)-M(149) &
    +M(151)+M(212)-M(218)-M(224)+M(226))) * den(28)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,448)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(143)-M(145)-M(147) &
    +M(148)+M(166)-M(190)-M(236)+M(242))) * den(28)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,449)
  Gcoeff = (c(6)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(28)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,450)
  Gcoeff = (c(5)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(132)+M(135)+M(141) &
    -M(151)-M(212)+M(214)+M(220)-M(226))) * den(28)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,451)
  Gcoeff = (c(5)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(143)+M(144)+M(146) &
    -M(148)-M(166)+M(176)+M(200)-M(242))) * den(28)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,452)
  Gcoeff = (c(6)*(M(132)-M(135)-M(141)-M(143)+M(144)+M(146)-M(148)+M(151)-M(166)+M(176)+M(200)+M(212)-M(214)-M(220)+M(226) &
    -M(242))) * den(28)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,453)
  Gcoeff = (c(5)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(6)*(-M(135)+M(138)-M(141) &
    +M(149)-M(214)+M(218)-M(220)+M(224))) * den(28)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,454)
  Gcoeff = (c(5)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(6)*(-M(144)+M(145)-M(146) &
    +M(147)-M(176)+M(190)-M(200)+M(236))) * den(28)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,455)
  Gcoeff = (c(6)*(M(135)-M(138)+M(141)-M(144)+M(145)-M(146)+M(147)-M(149)-M(176)+M(190)-M(200)+M(214)-M(218)+M(220)-M(224) &
    +M(236))) * den(28)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,456)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)+M(58)+M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(131)-M(137)-M(248)+M(250))) * den(12)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,151)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    +M(57)-M(58)+M(68)-M(69)+M(70)-M(81)+M(84)+M(93)-M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(211)+M(217)+M(227)-M(229))) * den(12)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,160)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(6)*(-M(131)+M(137)-M(211)+M(217)+M(227) &
    -M(229)+M(248)-M(250))) * den(12)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,169)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)-M(41)+M(44)+M(56) &
    +M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(161)-M(185)-M(245)+M(246))) * den(12)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,152)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)-M(33)+M(34)-M(41)+M(44)+M(56) &
    +M(57)-M(58)-M(68)-M(69)+M(70)-M(81)+M(84)+M(93)-M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(203)+M(205)+M(235)-M(241))) * den(12)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,161)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(6)*(-M(161)+M(185)-M(203)+M(205)+M(235) &
    -M(241)+M(245)-M(246))) * den(12)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,170)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(6)*(-M(131)+M(137)+M(161) &
    -M(185)-M(245)+M(246)+M(248)-M(250))) * den(12)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,153)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(6)*(-M(203)+M(205)+M(211) &
    -M(217)-M(227)+M(229)+M(235)-M(241))) * den(12)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,162)
  Gcoeff = (c(6)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(12)
  T3sum(1:5,65) = T3sum(1:5,65) + Gcoeff * G1tensor(:,171)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    +M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(153)+M(154)+M(164)-M(188))) * den(12)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,79)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(33)-M(34)+M(41)-M(44)-M(56) &
    -M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(162)+M(186)+M(221)-M(222))) * den(12)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,80)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)-M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96))+c(6)*(M(153)-M(154)-M(162)-M(164) &
    +M(186)+M(188)+M(221)-M(222))) * den(12)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,81)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(27)+M(28)-M(29)+M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    +M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)+M(101)-M(102)-M(107)+M(108)-M(113)+M(115)+M(119)-M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(M(163)-M(187)-M(231)+M(232))) * den(12)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,88)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(27)+M(28)-M(29)+M(30)+M(33)-M(34)+M(41)-M(44)-M(56) &
    -M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)+M(101)-M(102)-M(107)+M(108)-M(113)+M(115)+M(119)-M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(165)+M(189)+M(207)-M(208))) * den(12)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,89)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)-M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96))+c(6)*(-M(163)-M(165)+M(187)+M(189) &
    +M(207)-M(208)+M(231)-M(232))) * den(12)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,90)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(102)+M(108)+M(119)-M(121))+c(6)*(M(153)-M(154)+M(163)-M(164) &
    -M(187)+M(188)-M(231)+M(232))) * den(12)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,97)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(102)+M(108)+M(119)-M(121))+c(6)*(M(162)-M(165)-M(186)+M(189) &
    +M(207)-M(208)-M(221)+M(222))) * den(12)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,98)
  Gcoeff = (c(6)*(-M(153)+M(154)+M(162)-M(163)+M(164)-M(165)-M(186)+M(187)-M(188)+M(189)+M(207)-M(208)-M(221)+M(222)+M(231) &
    -M(232))) * den(12)
  T3sum(1:5,45) = T3sum(1:5,45) + Gcoeff * G1tensor(:,99)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(27)-M(28)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    +M(63)-M(64)-M(70)+M(75)+M(82)-M(87)+M(92)-M(93)+M(94)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(189)+M(199)+M(204)-M(207))) * den(16)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,235)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(27)-M(28)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(174)+M(191)-M(192)+M(234))) * den(16)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,236)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(174)+M(189)+M(191)-M(192)-M(199) &
    -M(204)+M(207)+M(234))) * den(16)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,237)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(27)-M(28)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(43)+M(50)+M(62) &
    +M(63)-M(64)-M(70)+M(75)+M(82)-M(87)-M(92)-M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(180)+M(183)+M(213)-M(223))) * den(16)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,238)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)+M(27)-M(28)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(43)+M(50)+M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)-M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(136)+M(150)+M(194)-M(196))) * den(16)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,239)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(136)+M(150)+M(180)-M(183)+M(194) &
    -M(196)-M(213)+M(223))) * den(16)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,240)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)-M(92)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120))+c(6)*(-M(180)+M(183)+M(189) &
    -M(199)-M(204)+M(207)+M(213)-M(223))) * den(16)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,241)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)-M(92)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120))+c(6)*(-M(136)+M(150)+M(174) &
    -M(191)+M(192)+M(194)-M(196)-M(234))) * den(16)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,242)
  Gcoeff = (c(6)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(16)
  T3sum(1:5,63) = T3sum(1:5,63) + Gcoeff * G1tensor(:,243)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(144)+M(168)-M(181)-M(187) &
    +M(198)+M(200)-M(231)-M(237))) * den(9)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,460)
  Gcoeff = (c(5)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(6)*(M(143)-M(144)+M(167)-M(168) &
    -M(198)-M(200)+M(240)+M(242))) * den(9)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,461)
  Gcoeff = (c(5)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(6)*(-M(143)-M(167)+M(181)+M(187) &
    +M(231)+M(237)-M(240)-M(242))) * den(9)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,462)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(134)-M(179)-M(193)+M(197) &
    +M(202)+M(210)-M(228)-M(247))) * den(9)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,463)
  Gcoeff = (c(5)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(6)*(M(133)-M(134)-M(197)-M(202) &
    +M(209)-M(210)+M(239)+M(244))) * den(9)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,464)
  Gcoeff = (c(5)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(6)*(-M(133)+M(179)+M(193)-M(209) &
    +M(228)-M(239)-M(244)+M(247))) * den(9)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,465)
  Gcoeff = (c(6)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(9)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,466)
  Gcoeff = (c(6)*(M(133)-M(134)-M(143)+M(144)-M(167)+M(168)-M(197)+M(198)+M(200)-M(202)+M(209)-M(210)+M(239)-M(240)-M(242) &
    +M(244))) * den(9)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,467)
  Gcoeff = (c(6)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(9)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,468)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(10)-M(12)-M(14)-M(15)-M(18)-M(20)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(48)+M(55)+M(60)+M(67)+M(68)+M(70)+M(71)+M(72)+M(73)+M(79)+M(80)+M(82)+M(89)+M(90)+M(91)+M(93)+M(94) &
    +M(100)+M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(193)+M(228)))
  T4sum(1:15,107) = T4sum(1:15,107) + Gcoeff * G2tensor(:,241)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(42)+M(51)+M(53)+M(55)+M(63)+M(65)+M(66)+M(69)+M(74)+M(75)+M(76)+M(78)+M(79)+M(80)+M(81)+M(82)+M(88)+M(89)+M(91)+M(94) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(197)+M(210)))
  T4sum(1:15,107) = T4sum(1:15,107) + Gcoeff * G2tensor(:,250)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)-M(48)+M(51) &
    +M(53)-M(60)+M(63)+M(65)+M(66)-M(67)-M(68)+M(69)-M(70)-M(71)-M(72)-M(73)+M(74)+M(75)+M(76)+M(78)+M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(-M(193)+M(197)+M(210)-M(228)))
  T4sum(1:15,107) = T4sum(1:15,107) + Gcoeff * G2tensor(:,259)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(48)+M(50)+M(51)+M(52)+M(60)+M(62)+M(68)+M(70)+M(71)+M(72)+M(73)+M(74)+M(80)+M(82)+M(86)+M(93)+M(94) &
    +M(100)+M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(150)+M(194)))
  T4sum(1:15,107) = T4sum(1:15,107) + Gcoeff * G2tensor(:,242)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(5)-M(8)-M(10)-M(11)-M(13)-M(15)-M(18)-M(20)-M(22)-M(23)-M(25)-M(28)-M(30)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(42)+M(50)+M(52)+M(53)+M(62)+M(63)+M(65)+M(66)+M(67)+M(69)+M(75)+M(76)+M(78)+M(80)+M(81)+M(82)+M(86)+M(88)+M(90)+M(94) &
    +M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(183)+M(213)))
  T4sum(1:15,107) = T4sum(1:15,107) + Gcoeff * G2tensor(:,251)
  Gcoeff = (c(4)*(M(3)-M(4)-M(5)+M(6)-M(13)+M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)-M(41)-M(48)-M(51) &
    +M(53)-M(60)+M(63)+M(65)+M(66)+M(67)-M(68)+M(69)-M(70)-M(71)-M(72)-M(73)-M(74)+M(75)+M(76)+M(78)+M(81)+M(88)+M(90)-M(93) &
    -M(100))+c(6)*(-M(150)+M(183)-M(194)+M(213)))
  T4sum(1:15,107) = T4sum(1:15,107) + Gcoeff * G2tensor(:,260)
  Gcoeff = (c(4)*(M(5)-M(6)-M(11)+M(12)-M(17)+M(18)-M(19)+M(20)+M(21)-M(22)+M(23)-M(24)-M(25)+M(26)+M(29)-M(30)+M(50)+M(51)+M(52) &
    -M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91)-M(102)-M(104)+M(108)+M(110)+M(114)-M(116)+M(119)+M(120)-M(121)-M(122) &
    -M(123)+M(124))+c(6)*(M(150)-M(193)+M(194)-M(228)))
  T4sum(1:15,107) = T4sum(1:15,107) + Gcoeff * G2tensor(:,243)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(11)+M(12)+M(17)-M(18)+M(19)-M(20)+M(21)-M(22)-M(23)+M(24)-M(25)+M(26)+M(29)-M(30)+M(50)-M(51)+M(52) &
    -M(55)+M(62)+M(67)-M(74)-M(79)+M(86)-M(89)+M(90)-M(91)-M(102)-M(104)+M(108)+M(110)+M(114)-M(116)+M(119)+M(120)-M(121)-M(122) &
    -M(123)+M(124))+c(6)*(M(183)-M(197)-M(210)+M(213)))
  T4sum(1:15,107) = T4sum(1:15,107) + Gcoeff * G2tensor(:,252)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(150)+M(183)+M(193)-M(194) &
    -M(197)-M(210)+M(213)+M(228)))
  T4sum(1:15,107) = T4sum(1:15,107) + Gcoeff * G2tensor(:,261)
  Gcoeff = (c(4)*(M(5)-M(6)-M(9)+M(10)+M(15)-M(16)-M(19)+M(20)-M(27)+M(28)+M(31)-M(32)+M(33)-M(34)-M(35)+M(36)-M(43)+M(50)+M(62) &
    +M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94)+M(105)-M(106)-M(108)+M(111)+M(114)-M(117)-M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(175)-M(205)+M(206)-M(235))) * den(16)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,244)
  Gcoeff = (c(4)*(M(5)-M(6)-M(9)+M(10)+M(15)-M(16)-M(19)+M(20)-M(27)+M(28)+M(31)-M(32)+M(33)-M(34)-M(35)+M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)+M(105)-M(106)-M(108)+M(111)+M(114)-M(117)-M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(177)+M(181)-M(182)+M(237))) * den(16)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,247)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)-M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94))+c(6)*(-M(175)-M(177)+M(181)-M(182) &
    +M(205)-M(206)+M(235)+M(237))) * den(16)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,250)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)-M(43)+M(50)+M(62) &
    +M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(173)-M(215)+M(216)-M(233))) * den(16)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,245)
  Gcoeff = (c(4)*(M(5)-M(6)+M(9)-M(10)-M(15)+M(16)-M(19)+M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(M(139)-M(140)-M(178)+M(238))) * den(16)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,248)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)-M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94))+c(6)*(M(139)-M(140)-M(173)-M(178) &
    +M(215)-M(216)+M(233)+M(238))) * den(16)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,251)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(31)+M(32)+M(35)-M(36)-M(105)+M(108)-M(111)+M(119))+c(6)*(M(173)-M(175)+M(205)-M(206) &
    -M(215)+M(216)-M(233)+M(235))) * den(16)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,246)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(31)+M(32)+M(35)-M(36)-M(105)+M(108)-M(111)+M(119))+c(6)*(M(139)-M(140)+M(177)-M(178) &
    -M(181)+M(182)-M(237)+M(238))) * den(16)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,249)
  Gcoeff = (c(6)*(M(139)-M(140)-M(173)+M(175)+M(177)-M(178)-M(181)+M(182)-M(205)+M(206)+M(215)-M(216)+M(233)-M(235)-M(237) &
    +M(238))) * den(16)
  T3sum(1:5,43) = T3sum(1:5,43) + Gcoeff * G1tensor(:,252)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(9)-M(11)-M(13)-M(16)-M(17)-M(19)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(44)+M(45)+M(47)+M(55)+M(56)+M(58)+M(59)+M(60)+M(61)+M(67)+M(72)+M(79)+M(83)+M(85)+M(89)+M(90)+M(91)+M(96)+M(97)+M(99) &
    +M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(169)+M(230)))
  T4sum(1:15,112) = T4sum(1:15,112) + Gcoeff * G2tensor(:,109)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(43)+M(44)+M(45)+M(46)+M(47)+M(49)+M(56)+M(59)+M(60)+M(70)+M(72)+M(73)+M(82)+M(83)+M(92)+M(93)+M(94)+M(95)+M(98) &
    +M(100)+M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(139)+M(238)))
  T4sum(1:15,112) = T4sum(1:15,112) + Gcoeff * G2tensor(:,110)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)+M(43)+M(46)+M(49) &
    -M(55)-M(58)-M(61)-M(67)+M(70)+M(73)-M(79)+M(82)-M(85)-M(89)-M(90)-M(91)+M(92)+M(93)+M(94)+M(95)-M(96)-M(97)+M(98)-M(99) &
    +M(100))+c(6)*(M(139)-M(169)-M(230)+M(238)))
  T4sum(1:15,112) = T4sum(1:15,112) + Gcoeff * G2tensor(:,111)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(46)+M(47)+M(53)+M(55)+M(58)+M(59)+M(60)+M(61)+M(65)+M(66)+M(72)+M(78)+M(79)+M(85)+M(89)+M(91)+M(95)+M(96)+M(97)+M(99) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(167)+M(240)))
  T4sum(1:15,112) = T4sum(1:15,112) + Gcoeff * G2tensor(:,118)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(6)-M(7)-M(9)-M(12)-M(14)-M(16)-M(17)-M(19)-M(21)-M(24)-M(26)-M(27)-M(29)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(43)+M(47)+M(49)+M(53)+M(59)+M(60)+M(65)+M(66)+M(67)+M(70)+M(72)+M(73)+M(78)+M(82)+M(90)+M(92)+M(93)+M(94)+M(98) &
    +M(100)+M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(181)+M(237)))
  T4sum(1:15,112) = T4sum(1:15,112) + Gcoeff * G2tensor(:,119)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(7)+M(8)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)+M(25)-M(26)-M(29)+M(30)+M(43)-M(46)+M(49)-M(55) &
    -M(58)-M(61)+M(67)+M(70)+M(73)-M(79)+M(82)-M(85)-M(89)+M(90)-M(91)+M(92)+M(93)+M(94)-M(95)-M(96)-M(97)+M(98)-M(99)+M(100)) &
    +c(6)*(-M(167)+M(181)+M(237)-M(240)))
  T4sum(1:15,112) = T4sum(1:15,112) + Gcoeff * G2tensor(:,120)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)-M(45)+M(46) &
    +M(53)-M(56)+M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)-M(103)+M(105)+M(107)-M(108)-M(109)-M(110)+M(111)+M(112)+M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(167)-M(169)-M(230)+M(240)))
  T4sum(1:15,112) = T4sum(1:15,112) + Gcoeff * G2tensor(:,127)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)-M(45)-M(46) &
    +M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95)-M(103)+M(105)+M(107)-M(108)-M(109)-M(110)+M(111)+M(112)+M(113)+M(118) &
    -M(119)-M(124))+c(6)*(-M(139)+M(181)+M(237)-M(238)))
  T4sum(1:15,112) = T4sum(1:15,112) + Gcoeff * G2tensor(:,128)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(139)-M(167)+M(169)+M(181)+M(230) &
    +M(237)-M(238)-M(240)))
  T4sum(1:15,112) = T4sum(1:15,112) + Gcoeff * G2tensor(:,129)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(141)-M(183)+M(209) &
    -M(211)-M(213)+M(214)-M(229)+M(239))) * den(65)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,472)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(142)+M(167)-M(169) &
    -M(171)+M(172)-M(184)-M(230)+M(240))) * den(65)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,473)
  Gcoeff = (c(6)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(65)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,474)
  Gcoeff = (c(5)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(6)*(-M(141)+M(151)+M(197) &
    -M(209)+M(210)+M(212)-M(214)-M(239))) * den(65)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,475)
  Gcoeff = (c(5)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(6)*(-M(142)+M(152)-M(167) &
    +M(168)+M(170)-M(172)+M(198)-M(240))) * den(65)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,476)
  Gcoeff = (c(6)*(M(141)-M(142)-M(151)+M(152)-M(167)+M(168)+M(170)-M(172)-M(197)+M(198)+M(209)-M(210)-M(212)+M(214)+M(239) &
    -M(240))) * den(65)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,477)
  Gcoeff = (c(5)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(6)*(-M(151)+M(183)-M(197) &
    -M(210)+M(211)-M(212)+M(213)+M(229))) * den(65)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,478)
  Gcoeff = (c(5)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(6)*(-M(152)-M(168)+M(169) &
    -M(170)+M(171)+M(184)-M(198)+M(230))) * den(65)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,479)
  Gcoeff = (c(6)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(65)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,480)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(58) &
    +M(63)-M(74)+M(75)-M(76)+M(85)-M(88)+M(95)-M(96)+M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(165)+M(175)+M(206)-M(208))) * den(22)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,253)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(167)-M(168)-M(198)+M(240))) * den(22)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,254)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(37)-M(38)-M(39)+M(40)+M(58)-M(63)-M(75)+M(96))+c(6)*(M(165)+M(167)-M(168)-M(175) &
    -M(198)-M(206)+M(208)+M(240))) * den(22)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,255)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(37)+M(38)+M(39)-M(40)-M(46)+M(51)-M(58) &
    +M(63)+M(74)+M(75)-M(76)+M(85)-M(88)-M(95)-M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(156)+M(159)+M(219)-M(225))) * den(22)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,256)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(58) &
    -M(63)+M(74)-M(75)-M(76)+M(85)-M(88)-M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(142)+M(152)+M(170)-M(172))) * den(22)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,257)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(37)-M(38)-M(39)+M(40)+M(58)-M(63)-M(75)+M(96))+c(6)*(-M(142)+M(152)+M(156)-M(159) &
    +M(170)-M(172)-M(219)+M(225))) * den(22)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,258)
  Gcoeff = (c(5)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(6)*(-M(156)+M(159)+M(165) &
    -M(175)-M(206)+M(208)+M(219)-M(225))) * den(22)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,259)
  Gcoeff = (c(5)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(6)*(-M(142)+M(152)-M(167) &
    +M(168)+M(170)-M(172)+M(198)-M(240))) * den(22)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,260)
  Gcoeff = (c(6)*(-M(142)+M(152)+M(156)-M(159)-M(165)-M(167)+M(168)+M(170)-M(172)+M(175)+M(198)+M(206)-M(208)-M(219)+M(225) &
    -M(240))) * den(22)
  T3sum(1:5,74) = T3sum(1:5,74) + Gcoeff * G1tensor(:,261)
  Gcoeff = (c(5)*(M(50)-M(55)-M(58)-M(61)+M(62)+M(63)+M(64)+M(75)+M(87)-M(89)-M(96)-M(99))+c(6)*(M(146)-M(157)-M(163)+M(174) &
    +M(176)+M(192)-M(232)-M(243))) * den(7)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,484)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)-M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94))+c(6)*(M(145)-M(146)-M(174)-M(176) &
    +M(191)-M(192)+M(234)+M(236))) * den(7)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,485)
  Gcoeff = (c(5)*(-M(43)+M(55)+M(58)+M(61)-M(70)-M(82)+M(89)-M(92)-M(93)-M(94)+M(96)+M(99))+c(6)*(-M(145)+M(157)+M(163)-M(191) &
    +M(232)-M(234)-M(236)+M(243))) * den(7)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,486)
  Gcoeff = (c(5)*(M(50)-M(55)-M(58)-M(61)+M(62)+M(63)+M(64)+M(75)+M(87)-M(89)-M(96)-M(99))+c(6)*(M(140)-M(155)-M(169)+M(173) &
    +M(178)+M(216)-M(230)-M(249))) * den(7)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,487)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)-M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94))+c(6)*(M(139)-M(140)-M(173)-M(178) &
    +M(215)-M(216)+M(233)+M(238))) * den(7)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,488)
  Gcoeff = (c(5)*(-M(43)+M(55)+M(58)+M(61)-M(70)-M(82)+M(89)-M(92)-M(93)-M(94)+M(96)+M(99))+c(6)*(-M(139)+M(155)+M(169)-M(215) &
    +M(230)-M(233)-M(238)+M(249))) * den(7)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,489)
  Gcoeff = (c(6)*(M(140)-M(146)-M(155)+M(157)+M(163)-M(169)+M(173)-M(174)-M(176)+M(178)-M(192)+M(216)-M(230)+M(232)+M(243) &
    -M(249))) * den(7)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,490)
  Gcoeff = (c(6)*(M(139)-M(140)-M(145)+M(146)-M(173)+M(174)+M(176)-M(178)-M(191)+M(192)+M(215)-M(216)+M(233)-M(234)-M(236) &
    +M(238))) * den(7)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,491)
  Gcoeff = (c(6)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(7)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,492)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(10)-M(11)-M(13)-M(15)-M(17)-M(20)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(45)+M(51)+M(52)+M(53)+M(54)+M(55)+M(57)+M(63)+M(64)+M(65)+M(74)+M(75)+M(77)+M(83)+M(84)+M(85)+M(86)+M(87)+M(89)+M(97) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(159)+M(219)))
  T4sum(1:15,119) = T4sum(1:15,119) + Gcoeff * G2tensor(:,421)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(44)+M(45)+M(47)+M(50)+M(51)+M(52)+M(56)+M(58)+M(59)+M(60)+M(61)+M(62)+M(72)+M(74)+M(83)+M(85)+M(86)+M(96)+M(97)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(152)+M(170)))
  T4sum(1:15,119) = T4sum(1:15,119) + Gcoeff * G2tensor(:,422)
  Gcoeff = (c(4)*(-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)+M(37)-M(38)-M(39)+M(40)+M(44)+M(47) &
    +M(50)-M(53)-M(54)-M(55)+M(56)-M(57)+M(58)+M(59)+M(60)+M(61)+M(62)-M(63)-M(64)-M(65)+M(72)-M(75)-M(77)-M(84)-M(87)-M(89)+M(96) &
    +M(99))+c(6)*(M(152)-M(159)+M(170)-M(219)))
  T4sum(1:15,119) = T4sum(1:15,119) + Gcoeff * G2tensor(:,423)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(45)+M(50)+M(53)+M(54)+M(57)+M(62)+M(63)+M(64)+M(65)+M(67)+M(75)+M(77)+M(79)+M(83)+M(84)+M(85)+M(87)+M(90)+M(91)+M(97) &
    +M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(173)+M(216)))
  T4sum(1:15,119) = T4sum(1:15,119) + Gcoeff * G2tensor(:,424)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(9)-M(11)-M(13)-M(16)-M(17)-M(19)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(44)+M(45)+M(47)+M(55)+M(56)+M(58)+M(59)+M(60)+M(61)+M(67)+M(72)+M(79)+M(83)+M(85)+M(89)+M(90)+M(91)+M(96)+M(97)+M(99) &
    +M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(169)+M(230)))
  T4sum(1:15,119) = T4sum(1:15,119) + Gcoeff * G2tensor(:,425)
  Gcoeff = (c(4)*(-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)+M(37)-M(38)-M(39)+M(40)+M(44)+M(47) &
    -M(50)-M(53)-M(54)+M(55)+M(56)-M(57)+M(58)+M(59)+M(60)+M(61)-M(62)-M(63)-M(64)-M(65)+M(72)-M(75)-M(77)-M(84)-M(87)+M(89)+M(96) &
    +M(99))+c(6)*(M(169)-M(173)-M(216)+M(230)))
  T4sum(1:15,119) = T4sum(1:15,119) + Gcoeff * G2tensor(:,426)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)+M(17)-M(18)-M(19)+M(20)-M(21)+M(22)-M(23)+M(24)-M(25)+M(26)+M(29)-M(30)+M(50)-M(51)-M(52) &
    -M(55)+M(62)+M(67)-M(74)+M(79)-M(86)-M(89)+M(90)+M(91)-M(102)-M(104)+M(108)+M(110)+M(114)-M(116)+M(119)+M(120)-M(121)-M(122) &
    -M(123)+M(124))+c(6)*(-M(159)+M(173)+M(216)-M(219)))
  T4sum(1:15,119) = T4sum(1:15,119) + Gcoeff * G2tensor(:,427)
  Gcoeff = (c(4)*(M(5)-M(6)-M(11)+M(12)-M(17)+M(18)-M(19)+M(20)+M(21)-M(22)+M(23)-M(24)-M(25)+M(26)+M(29)-M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)-M(102)-M(104)+M(108)+M(110)+M(114)-M(116)+M(119)+M(120)-M(121)-M(122) &
    -M(123)+M(124))+c(6)*(-M(152)+M(169)-M(170)+M(230)))
  T4sum(1:15,119) = T4sum(1:15,119) + Gcoeff * G2tensor(:,428)
  Gcoeff = (c(5)*(-M(11)+M(12)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(50)+M(55)-M(62)+M(89))+c(6)*(-M(152)+M(159)+M(169)-M(170) &
    -M(173)-M(216)+M(219)+M(230)))
  T4sum(1:15,119) = T4sum(1:15,119) + Gcoeff * G2tensor(:,429)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(154)-M(160)+M(162)+M(164) &
    -M(171)-M(184)-M(195)+M(222))) * den(5)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,337)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)-M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96))+c(6)*(M(153)-M(154)-M(162)-M(164) &
    +M(186)+M(188)+M(221)-M(222))) * den(5)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,338)
  Gcoeff = (c(5)*(-M(41)+M(53)+M(60)+M(63)+M(65)-M(68)-M(69)-M(70)+M(72)+M(75)-M(81)-M(93))+c(6)*(-M(153)+M(160)+M(171)+M(184) &
    -M(186)-M(188)+M(195)-M(221))) * den(5)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,339)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(148)-M(158)+M(161)+M(166) &
    -M(177)-M(182)-M(201)+M(246))) * den(5)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,340)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)-M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96))+c(6)*(M(147)-M(148)-M(161)-M(166) &
    +M(185)+M(190)+M(245)-M(246))) * den(5)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,341)
  Gcoeff = (c(5)*(-M(41)+M(53)+M(60)+M(63)+M(65)-M(68)-M(69)-M(70)+M(72)+M(75)-M(81)-M(93))+c(6)*(-M(147)+M(158)+M(177)+M(182) &
    -M(185)-M(190)+M(201)-M(245))) * den(5)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,342)
  Gcoeff = (c(6)*(M(148)-M(154)-M(158)+M(160)+M(161)-M(162)-M(164)+M(166)+M(171)-M(177)-M(182)+M(184)+M(195)-M(201)-M(222) &
    +M(246))) * den(5)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,343)
  Gcoeff = (c(6)*(M(147)-M(148)-M(153)+M(154)-M(161)+M(162)+M(164)-M(166)+M(185)-M(186)-M(188)+M(190)-M(221)+M(222)+M(245) &
    -M(246))) * den(5)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,344)
  Gcoeff = (c(6)*(-M(147)+M(153)+M(158)-M(160)-M(171)+M(177)+M(182)-M(184)-M(185)+M(186)+M(188)-M(190)-M(195)+M(201)+M(221) &
    -M(245))) * den(5)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,345)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(154)-M(160)+M(162)+M(164) &
    -M(171)-M(184)-M(195)+M(222))) * den(5)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,43)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)-M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96))+c(6)*(M(153)-M(154)-M(162)-M(164) &
    +M(186)+M(188)+M(221)-M(222))) * den(5)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,44)
  Gcoeff = (c(5)*(-M(41)+M(53)+M(60)+M(63)+M(65)-M(68)-M(69)-M(70)+M(72)+M(75)-M(81)-M(93))+c(6)*(-M(153)+M(160)+M(171)+M(184) &
    -M(186)-M(188)+M(195)-M(221))) * den(5)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,45)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(163)+M(165)-M(168)-M(174) &
    -M(192)-M(198)+M(208)+M(232))) * den(5)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,49)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)-M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96))+c(6)*(-M(163)-M(165)+M(187)+M(189) &
    +M(207)-M(208)+M(231)-M(232))) * den(5)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,50)
  Gcoeff = (c(5)*(-M(41)+M(53)+M(60)+M(63)+M(65)-M(68)-M(69)-M(70)+M(72)+M(75)-M(81)-M(93))+c(6)*(M(168)+M(174)-M(187)-M(189) &
    +M(192)+M(198)-M(207)-M(231))) * den(5)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,51)
  Gcoeff = (c(6)*(-M(154)+M(160)-M(162)+M(163)-M(164)+M(165)-M(168)+M(171)-M(174)+M(184)-M(192)+M(195)-M(198)+M(208)-M(222) &
    +M(232))) * den(5)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,55)
  Gcoeff = (c(6)*(-M(153)+M(154)+M(162)-M(163)+M(164)-M(165)-M(186)+M(187)-M(188)+M(189)+M(207)-M(208)-M(221)+M(222)+M(231) &
    -M(232))) * den(5)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,56)
  Gcoeff = (c(6)*(M(153)-M(160)+M(168)-M(171)+M(174)-M(184)+M(186)-M(187)+M(188)-M(189)+M(192)-M(195)+M(198)-M(207)+M(221) &
    -M(231))) * den(5)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,57)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(45)+M(50)+M(53)+M(54)+M(57)+M(62)+M(63)+M(64)+M(65)+M(67)+M(75)+M(77)+M(79)+M(83)+M(84)+M(85)+M(87)+M(90)+M(91)+M(97) &
    +M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(173)+M(216)))
  T4sum(1:15,126) = T4sum(1:15,126) + Gcoeff * G2tensor(:,112)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(9)-M(11)-M(13)-M(16)-M(17)-M(19)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(44)+M(45)+M(47)+M(55)+M(56)+M(58)+M(59)+M(60)+M(61)+M(67)+M(72)+M(79)+M(83)+M(85)+M(89)+M(90)+M(91)+M(96)+M(97)+M(99) &
    +M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(169)+M(230)))
  T4sum(1:15,126) = T4sum(1:15,126) + Gcoeff * G2tensor(:,113)
  Gcoeff = (c(4)*(-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)+M(37)-M(38)-M(39)+M(40)+M(44)+M(47) &
    -M(50)-M(53)-M(54)+M(55)+M(56)-M(57)+M(58)+M(59)+M(60)+M(61)-M(62)-M(63)-M(64)-M(65)+M(72)-M(75)-M(77)-M(84)-M(87)+M(89)+M(96) &
    +M(99))+c(6)*(M(169)-M(173)-M(216)+M(230)))
  T4sum(1:15,126) = T4sum(1:15,126) + Gcoeff * G2tensor(:,114)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(9)-M(11)-M(13)-M(16)-M(18)-M(19)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(37) &
    -M(40)+M(44)+M(46)+M(50)+M(54)+M(56)+M(57)+M(62)+M(63)+M(64)+M(66)+M(75)+M(77)+M(78)+M(79)+M(84)+M(85)+M(87)+M(91)+M(95)+M(97) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(175)+M(206)))
  T4sum(1:15,126) = T4sum(1:15,126) + Gcoeff * G2tensor(:,121)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(46)+M(47)+M(53)+M(55)+M(58)+M(59)+M(60)+M(61)+M(65)+M(66)+M(72)+M(78)+M(79)+M(85)+M(89)+M(91)+M(95)+M(96)+M(97)+M(99) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(167)+M(240)))
  T4sum(1:15,126) = T4sum(1:15,126) + Gcoeff * G2tensor(:,122)
  Gcoeff = (c(4)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(17)+M(18)-M(21)+M(22)+M(23)-M(24)+M(37)-M(38)-M(39)+M(40)-M(44)+M(47)-M(50) &
    +M(53)-M(54)+M(55)-M(56)-M(57)+M(58)+M(59)+M(60)+M(61)-M(62)-M(63)-M(64)+M(65)+M(72)-M(75)-M(77)-M(84)-M(87)+M(89)+M(96) &
    +M(99))+c(6)*(M(167)-M(175)-M(206)+M(240)))
  T4sum(1:15,126) = T4sum(1:15,126) + Gcoeff * G2tensor(:,123)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)+M(31)-M(32)-M(35)+M(36)+M(44)-M(45)+M(46) &
    -M(53)+M(56)-M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)-M(103)+M(105)+M(107)-M(108)-M(109)-M(110)+M(111)+M(112)+M(113)+M(118) &
    -M(119)-M(124))+c(6)*(-M(173)+M(175)+M(206)-M(216)))
  T4sum(1:15,126) = T4sum(1:15,126) + Gcoeff * G2tensor(:,130)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)-M(45)+M(46) &
    +M(53)-M(56)+M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)-M(103)+M(105)+M(107)-M(108)-M(109)-M(110)+M(111)+M(112)+M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(167)-M(169)-M(230)+M(240)))
  T4sum(1:15,126) = T4sum(1:15,126) + Gcoeff * G2tensor(:,131)
  Gcoeff = (c(5)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(44)+M(53)-M(56)+M(65))+c(6)*(M(167)-M(169)+M(173)-M(175)-M(206) &
    +M(216)-M(230)+M(240)))
  T4sum(1:15,126) = T4sum(1:15,126) + Gcoeff * G2tensor(:,132)
  Gcoeff = (c(5)*(M(50)-M(55)-M(58)-M(61)+M(62)+M(63)+M(64)+M(75)+M(87)-M(89)-M(96)-M(99))+c(6)*(-M(161)-M(167)+M(175)+M(177) &
    +M(182)+M(206)-M(240)-M(246))) * den(7)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,493)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)-M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94))+c(6)*(-M(175)-M(177)+M(181)-M(182) &
    +M(205)-M(206)+M(235)+M(237))) * den(7)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,496)
  Gcoeff = (c(5)*(-M(43)+M(55)+M(58)+M(61)-M(70)-M(82)+M(89)-M(92)-M(93)-M(94)+M(96)+M(99))+c(6)*(M(161)+M(167)-M(181)-M(205) &
    -M(235)-M(237)+M(240)+M(246))) * den(7)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,499)
  Gcoeff = (c(5)*(M(50)-M(55)-M(58)-M(61)+M(62)+M(63)+M(64)+M(75)+M(87)-M(89)-M(96)-M(99))+c(6)*(M(140)-M(155)-M(169)+M(173) &
    +M(178)+M(216)-M(230)-M(249))) * den(7)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,494)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)-M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94))+c(6)*(M(139)-M(140)-M(173)-M(178) &
    +M(215)-M(216)+M(233)+M(238))) * den(7)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,497)
  Gcoeff = (c(5)*(-M(43)+M(55)+M(58)+M(61)-M(70)-M(82)+M(89)-M(92)-M(93)-M(94)+M(96)+M(99))+c(6)*(-M(139)+M(155)+M(169)-M(215) &
    +M(230)-M(233)-M(238)+M(249))) * den(7)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,500)
  Gcoeff = (c(6)*(M(140)-M(155)+M(161)+M(167)-M(169)+M(173)-M(175)-M(177)+M(178)-M(182)-M(206)+M(216)-M(230)+M(240)+M(246) &
    -M(249))) * den(7)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,495)
  Gcoeff = (c(6)*(M(139)-M(140)-M(173)+M(175)+M(177)-M(178)-M(181)+M(182)-M(205)+M(206)+M(215)-M(216)+M(233)-M(235)-M(237) &
    +M(238))) * den(7)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,498)
  Gcoeff = (c(6)*(-M(139)+M(155)-M(161)-M(167)+M(169)+M(181)+M(205)-M(215)+M(230)-M(233)+M(235)+M(237)-M(238)-M(240)-M(246) &
    +M(249))) * den(7)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,501)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(156)-M(162)-M(173) &
    +M(175)+M(206)-M(216)-M(222)+M(225))) * den(65)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,502)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(142)+M(167)-M(169) &
    -M(171)+M(172)-M(184)-M(230)+M(240))) * den(65)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,503)
  Gcoeff = (c(6)*(M(142)-M(156)+M(162)+M(167)-M(169)-M(171)+M(172)+M(173)-M(175)-M(184)-M(206)+M(216)+M(222)-M(225)-M(230) &
    +M(240))) * den(65)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,504)
  Gcoeff = (c(5)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(6)*(-M(156)+M(159)+M(165) &
    -M(175)-M(206)+M(208)+M(219)-M(225))) * den(65)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,505)
  Gcoeff = (c(5)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(6)*(-M(142)+M(152)-M(167) &
    +M(168)+M(170)-M(172)+M(198)-M(240))) * den(65)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,506)
  Gcoeff = (c(6)*(-M(142)+M(152)+M(156)-M(159)-M(165)-M(167)+M(168)+M(170)-M(172)+M(175)+M(198)+M(206)-M(208)-M(219)+M(225) &
    -M(240))) * den(65)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,507)
  Gcoeff = (c(5)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(6)*(-M(159)+M(162)-M(165) &
    +M(173)-M(208)+M(216)-M(219)+M(222))) * den(65)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,508)
  Gcoeff = (c(5)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(6)*(-M(152)-M(168)+M(169) &
    -M(170)+M(171)+M(184)-M(198)+M(230))) * den(65)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,509)
  Gcoeff = (c(6)*(-M(152)+M(159)-M(162)+M(165)-M(168)+M(169)-M(170)+M(171)-M(173)+M(184)-M(198)+M(208)-M(216)+M(219)-M(222) &
    +M(230))) * den(65)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,510)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)-M(58) &
    +M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97)-M(102)+M(105)+M(111)-M(112)+M(116)-M(118)-M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(199)-M(203)+M(204)-M(241))) * den(22)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,262)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(37)-M(38)-M(39)+M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)-M(102)+M(105)+M(111)-M(112)+M(116)-M(118)-M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(157)-M(158)-M(201)+M(243))) * den(22)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,265)
  Gcoeff = (c(5)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(6)*(M(157)-M(158)-M(199)-M(201) &
    +M(203)-M(204)+M(241)+M(243))) * den(22)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,268)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)-M(46)+M(51)-M(58) &
    +M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(197)-M(209)+M(210)-M(239))) * den(22)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,263)
  Gcoeff = (c(4)*(M(3)-M(4)+M(11)-M(12)-M(13)+M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(M(133)-M(134)-M(202)+M(244))) * den(22)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,266)
  Gcoeff = (c(5)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(6)*(M(133)-M(134)-M(197)-M(202) &
    +M(209)-M(210)+M(239)+M(244))) * den(22)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,269)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(37)+M(38)+M(39)-M(40)+M(102)-M(105)-M(111)+M(121))+c(6)*(M(197)-M(199)+M(203)-M(204) &
    -M(209)+M(210)-M(239)+M(241))) * den(22)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,264)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(37)+M(38)+M(39)-M(40)+M(102)-M(105)-M(111)+M(121))+c(6)*(M(133)-M(134)-M(157)+M(158) &
    +M(201)-M(202)-M(243)+M(244))) * den(22)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,267)
  Gcoeff = (c(6)*(M(133)-M(134)-M(157)+M(158)-M(197)+M(199)+M(201)-M(202)-M(203)+M(204)+M(209)-M(210)+M(239)-M(241)-M(243) &
    +M(244))) * den(22)
  T3sum(1:5,54) = T3sum(1:5,54) + Gcoeff * G1tensor(:,270)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(9)-M(12)-M(14)-M(16)-M(18)-M(19)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(46)+M(48)+M(49)+M(53)+M(54)+M(55)+M(58)+M(60)+M(61)+M(65)+M(71)+M(72)+M(77)+M(85)+M(89)+M(95)+M(96)+M(97)+M(98)+M(99) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(157)+M(243)))
  T4sum(1:15,127) = T4sum(1:15,127) + Gcoeff * G2tensor(:,430)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(43)+M(48)+M(53)+M(54)+M(60)+M(65)+M(67)+M(70)+M(71)+M(72)+M(73)+M(77)+M(79)+M(82)+M(90)+M(91)+M(92)+M(93)+M(94) &
    +M(100)+M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(191)+M(234)))
  T4sum(1:15,127) = T4sum(1:15,127) + Gcoeff * G2tensor(:,433)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(7)+M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)+M(43)-M(46)-M(49)-M(55) &
    -M(58)-M(61)+M(67)+M(70)+M(73)+M(79)+M(82)-M(85)-M(89)+M(90)+M(91)+M(92)+M(93)+M(94)-M(95)-M(96)-M(97)-M(98)-M(99)+M(100)) &
    +c(6)*(-M(157)+M(191)+M(234)-M(243)))
  T4sum(1:15,127) = T4sum(1:15,127) + Gcoeff * G2tensor(:,436)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(43)+M(46)+M(48)+M(49)+M(58)+M(60)+M(61)+M(68)+M(71)+M(72)+M(80)+M(85)+M(92)+M(95)+M(96)+M(97)+M(98)+M(99) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(133)+M(244)))
  T4sum(1:15,127) = T4sum(1:15,127) + Gcoeff * G2tensor(:,431)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(10)-M(12)-M(14)-M(15)-M(18)-M(20)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(48)+M(55)+M(60)+M(67)+M(68)+M(70)+M(71)+M(72)+M(73)+M(79)+M(80)+M(82)+M(89)+M(90)+M(91)+M(93)+M(94) &
    +M(100)+M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(193)+M(228)))
  T4sum(1:15,127) = T4sum(1:15,127) + Gcoeff * G2tensor(:,434)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)-M(43)-M(46)-M(49) &
    +M(55)-M(58)-M(61)+M(67)+M(70)+M(73)+M(79)+M(82)-M(85)+M(89)+M(90)+M(91)-M(92)+M(93)+M(94)-M(95)-M(96)-M(97)-M(98)-M(99) &
    +M(100))+c(6)*(-M(133)+M(193)+M(228)-M(244)))
  T4sum(1:15,127) = T4sum(1:15,127) + Gcoeff * G2tensor(:,437)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(5)+M(6)-M(7)+M(8)-M(13)+M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)-M(101)+M(102)+M(103)+M(104)-M(105)-M(106)+M(109)-M(111)-M(115)-M(117)+M(121) &
    +M(123))+c(6)*(M(133)-M(157)-M(243)+M(244)))
  T4sum(1:15,127) = T4sum(1:15,127) + Gcoeff * G2tensor(:,432)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)-M(5)+M(6)+M(7)-M(8)+M(13)-M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42)-M(43)-M(53) &
    -M(54)+M(55)-M(65)+M(68)-M(77)+M(80)+M(89)-M(92)-M(101)+M(102)+M(103)+M(104)-M(105)-M(106)+M(109)-M(111)-M(115)-M(117)+M(121) &
    +M(123))+c(6)*(-M(191)+M(193)+M(228)-M(234)))
  T4sum(1:15,127) = T4sum(1:15,127) + Gcoeff * G2tensor(:,435)
  Gcoeff = (c(5)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(6)*(-M(133)+M(157)-M(191)+M(193)+M(228) &
    -M(234)+M(243)-M(244)))
  T4sum(1:15,127) = T4sum(1:15,127) + Gcoeff * G2tensor(:,438)
  Gcoeff = (c(5)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(6)*(M(135)-M(159)+M(215) &
    -M(217)-M(219)+M(220)-M(227)+M(233))) * den(82)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,514)
  Gcoeff = (c(5)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(6)*(M(136)-M(160)+M(191) &
    -M(193)-M(195)+M(196)-M(228)+M(234))) * den(82)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,515)
  Gcoeff = (c(6)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(82)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,516)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)-M(92)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120))+c(6)*(-M(135)+M(149)+M(173) &
    -M(215)+M(216)+M(218)-M(220)-M(233))) * den(82)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,517)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)-M(92)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120))+c(6)*(-M(136)+M(150)+M(174) &
    -M(191)+M(192)+M(194)-M(196)-M(234))) * den(82)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,518)
  Gcoeff = (c(6)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(82)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,519)
  Gcoeff = (c(5)*(-M(50)+M(55)-M(62)+M(89)+M(102)+M(104)-M(108)-M(114)-M(119)-M(120)+M(121)+M(123))+c(6)*(-M(149)+M(159)-M(173) &
    -M(216)+M(217)-M(218)+M(219)+M(227))) * den(82)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,520)
  Gcoeff = (c(5)*(-M(50)+M(55)-M(62)+M(89)+M(102)+M(104)-M(108)-M(114)-M(119)-M(120)+M(121)+M(123))+c(6)*(-M(150)+M(160)-M(174) &
    -M(192)+M(193)-M(194)+M(195)+M(228))) * den(82)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,521)
  Gcoeff = (c(6)*(M(149)-M(150)-M(159)+M(160)+M(173)-M(174)-M(192)+M(193)-M(194)+M(195)+M(216)-M(217)+M(218)-M(219)-M(227) &
    +M(228))) * den(82)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,522)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(43)+M(48)+M(53)+M(54)+M(60)+M(65)+M(67)+M(70)+M(71)+M(72)+M(73)+M(77)+M(79)+M(82)+M(90)+M(91)+M(92)+M(93)+M(94) &
    +M(100)+M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(191)+M(234)))
  T4sum(1:15,135) = T4sum(1:15,135) + Gcoeff * G2tensor(:,244)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(10)-M(12)-M(14)-M(15)-M(17)-M(20)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(38) &
    -M(39)+M(41)+M(43)+M(51)+M(54)+M(63)+M(66)+M(68)+M(69)+M(74)+M(75)+M(76)+M(77)+M(78)+M(79)+M(81)+M(82)+M(88)+M(91)+M(92)+M(94) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(199)+M(204)))
  T4sum(1:15,135) = T4sum(1:15,135) + Gcoeff * G2tensor(:,253)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)-M(17)+M(18)+M(19)-M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)-M(48)+M(51) &
    -M(53)-M(60)+M(63)-M(65)+M(66)-M(67)+M(68)+M(69)-M(70)-M(71)-M(72)-M(73)+M(74)+M(75)+M(76)+M(78)+M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(-M(191)+M(199)+M(204)-M(234)))
  T4sum(1:15,135) = T4sum(1:15,135) + Gcoeff * G2tensor(:,262)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(10)-M(12)-M(14)-M(15)-M(18)-M(20)-M(21)-M(23)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(41)+M(42)+M(48)+M(55)+M(60)+M(67)+M(68)+M(70)+M(71)+M(72)+M(73)+M(79)+M(80)+M(82)+M(89)+M(90)+M(91)+M(93)+M(94) &
    +M(100)+M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(193)+M(228)))
  T4sum(1:15,135) = T4sum(1:15,135) + Gcoeff * G2tensor(:,245)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(42)+M(51)+M(53)+M(55)+M(63)+M(65)+M(66)+M(69)+M(74)+M(75)+M(76)+M(78)+M(79)+M(80)+M(81)+M(82)+M(88)+M(89)+M(91)+M(94) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(197)+M(210)))
  T4sum(1:15,135) = T4sum(1:15,135) + Gcoeff * G2tensor(:,254)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)-M(48)+M(51) &
    +M(53)-M(60)+M(63)+M(65)+M(66)-M(67)-M(68)+M(69)-M(70)-M(71)-M(72)-M(73)+M(74)+M(75)+M(76)+M(78)+M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(-M(193)+M(197)+M(210)-M(228)))
  T4sum(1:15,135) = T4sum(1:15,135) + Gcoeff * G2tensor(:,263)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)-M(5)+M(6)+M(7)-M(8)+M(13)-M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42)-M(43)-M(53) &
    -M(54)+M(55)-M(65)+M(68)-M(77)+M(80)+M(89)-M(92)-M(101)+M(102)+M(103)+M(104)-M(105)-M(106)+M(109)-M(111)-M(115)-M(117)+M(121) &
    +M(123))+c(6)*(-M(191)+M(193)+M(228)-M(234)))
  T4sum(1:15,135) = T4sum(1:15,135) + Gcoeff * G2tensor(:,246)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(5)-M(6)+M(7)-M(8)-M(13)+M(14)-M(19)+M(20)-M(37)+M(38)+M(39)-M(40)-M(41)+M(42)-M(43)+M(53) &
    -M(54)+M(55)+M(65)-M(68)-M(77)+M(80)+M(89)-M(92)-M(101)+M(102)+M(103)+M(104)-M(105)-M(106)+M(109)-M(111)-M(115)-M(117)+M(121) &
    +M(123))+c(6)*(M(197)-M(199)-M(204)+M(210)))
  T4sum(1:15,135) = T4sum(1:15,135) + Gcoeff * G2tensor(:,255)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(M(191)-M(193)+M(197)-M(199)-M(204) &
    +M(210)-M(228)+M(234)))
  T4sum(1:15,135) = T4sum(1:15,135) + Gcoeff * G2tensor(:,264)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(158)-M(185)-M(191)+M(199) &
    +M(201)+M(204)-M(234)-M(245))) * den(9)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,523)
  Gcoeff = (c(5)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(6)*(M(157)-M(158)-M(199)-M(201) &
    +M(203)-M(204)+M(241)+M(243))) * den(9)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,526)
  Gcoeff = (c(5)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(6)*(-M(157)+M(185)+M(191)-M(203) &
    +M(234)-M(241)-M(243)+M(245))) * den(9)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,529)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(134)-M(179)-M(193)+M(197) &
    +M(202)+M(210)-M(228)-M(247))) * den(9)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,524)
  Gcoeff = (c(5)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(6)*(M(133)-M(134)-M(197)-M(202) &
    +M(209)-M(210)+M(239)+M(244))) * den(9)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,527)
  Gcoeff = (c(5)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(6)*(-M(133)+M(179)+M(193)-M(209) &
    +M(228)-M(239)-M(244)+M(247))) * den(9)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,530)
  Gcoeff = (c(6)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(9)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,525)
  Gcoeff = (c(6)*(M(133)-M(134)-M(157)+M(158)-M(197)+M(199)+M(201)-M(202)-M(203)+M(204)+M(209)-M(210)+M(239)-M(241)-M(243) &
    +M(244))) * den(9)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,528)
  Gcoeff = (c(6)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(9)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,531)
  Gcoeff = (c(5)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(6)*(M(180)-M(186)-M(197) &
    +M(199)+M(204)-M(210)-M(221)+M(223))) * den(82)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,532)
  Gcoeff = (c(5)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(6)*(M(136)-M(160)+M(191) &
    -M(193)-M(195)+M(196)-M(228)+M(234))) * den(82)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,533)
  Gcoeff = (c(6)*(M(136)-M(160)-M(180)+M(186)+M(191)-M(193)-M(195)+M(196)+M(197)-M(199)-M(204)+M(210)+M(221)-M(223)-M(228) &
    +M(234))) * den(82)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,534)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)-M(92)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120))+c(6)*(-M(180)+M(183)+M(189) &
    -M(199)-M(204)+M(207)+M(213)-M(223))) * den(82)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,535)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)-M(92)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120))+c(6)*(-M(136)+M(150)+M(174) &
    -M(191)+M(192)+M(194)-M(196)-M(234))) * den(82)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,536)
  Gcoeff = (c(6)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(82)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,537)
  Gcoeff = (c(5)*(-M(50)+M(55)-M(62)+M(89)+M(102)+M(104)-M(108)-M(114)-M(119)-M(120)+M(121)+M(123))+c(6)*(-M(183)+M(186)-M(189) &
    +M(197)-M(207)+M(210)-M(213)+M(221))) * den(82)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,538)
  Gcoeff = (c(5)*(-M(50)+M(55)-M(62)+M(89)+M(102)+M(104)-M(108)-M(114)-M(119)-M(120)+M(121)+M(123))+c(6)*(-M(150)+M(160)-M(174) &
    -M(192)+M(193)-M(194)+M(195)+M(228))) * den(82)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,539)
  Gcoeff = (c(6)*(-M(150)+M(160)-M(174)+M(183)-M(186)+M(189)-M(192)+M(193)-M(194)+M(195)-M(197)+M(207)-M(210)+M(213)-M(221) &
    +M(228))) * den(82)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,540)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(131)-M(155)-M(179) &
    +M(185)+M(245)-M(247)-M(249)+M(250))) * den(92)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,346)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(132)-M(156)-M(180) &
    +M(186)+M(221)-M(223)-M(225)+M(226))) * den(92)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,347)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(92)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,348)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(6)*(-M(131)+M(137)+M(161) &
    -M(185)-M(245)+M(246)+M(248)-M(250))) * den(92)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,349)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(6)*(-M(132)+M(138)+M(162) &
    -M(186)-M(221)+M(222)+M(224)-M(226))) * den(92)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,350)
  Gcoeff = (c(6)*(M(131)-M(132)-M(137)+M(138)-M(161)+M(162)+M(185)-M(186)-M(221)+M(222)+M(224)-M(226)+M(245)-M(246)-M(248) &
    +M(250))) * den(92)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,351)
  Gcoeff = (c(5)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(6)*(-M(137)+M(155)-M(161) &
    +M(179)-M(246)+M(247)-M(248)+M(249))) * den(92)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,352)
  Gcoeff = (c(5)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(6)*(-M(138)+M(156)-M(162) &
    +M(180)-M(222)+M(223)-M(224)+M(225))) * den(92)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,353)
  Gcoeff = (c(6)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(92)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,354)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(131)-M(155)-M(179) &
    +M(185)+M(245)-M(247)-M(249)+M(250))) * den(92)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,175)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(203)-M(209)-M(215) &
    +M(217)+M(227)-M(233)-M(239)+M(241))) * den(92)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,181)
  Gcoeff = (c(6)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(92)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,187)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(6)*(-M(131)+M(137)+M(161) &
    -M(185)-M(245)+M(246)+M(248)-M(250))) * den(92)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,176)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(6)*(-M(203)+M(205)+M(211) &
    -M(217)-M(227)+M(229)+M(235)-M(241))) * den(92)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,182)
  Gcoeff = (c(6)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(92)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,188)
  Gcoeff = (c(5)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(6)*(-M(137)+M(155)-M(161) &
    +M(179)-M(246)+M(247)-M(248)+M(249))) * den(92)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,177)
  Gcoeff = (c(5)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(6)*(-M(205)+M(209)-M(211) &
    +M(215)-M(229)+M(233)-M(235)+M(239))) * den(92)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,183)
  Gcoeff = (c(6)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(92)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,189)
  Gcoeff = (c(5)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56)+M(68))+c(6)*(M(131)-M(137)+M(153)-M(154) &
    -M(164)+M(188)-M(248)+M(250))) * den(13)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,355)
  Gcoeff = (c(5)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56)+M(68))+c(6)*(M(132)-M(138)+M(147)-M(148) &
    -M(166)+M(190)-M(224)+M(226))) * den(13)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,356)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(13)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,357)
  Gcoeff = (c(5)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56)+M(68))+c(6)*(M(132)-M(138)+M(147)-M(148) &
    -M(166)+M(190)-M(224)+M(226))) * den(13)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,277)
  Gcoeff = (c(5)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56)+M(68))+c(6)*(M(143)-M(145)+M(149)-M(151) &
    -M(212)+M(218)-M(236)+M(242))) * den(13)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,278)
  Gcoeff = (c(6)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(13)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,279)
  Gcoeff = (c(5)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56)+M(68))+c(6)*(M(131)-M(137)+M(153)-M(154) &
    -M(164)+M(188)-M(248)+M(250))) * den(13)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(5)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56)+M(68))+c(6)*(M(143)-M(145)+M(149)-M(151) &
    -M(212)+M(218)-M(236)+M(242))) * den(13)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(143)-M(145)+M(149)-M(151)-M(153)+M(154)+M(164)-M(188)-M(212)+M(218)-M(236)+M(242)+M(248) &
    -M(250))) * den(13)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(5)*(-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62)+M(92))+c(6)*(M(135)+M(145)-M(146)-M(149) &
    -M(176)-M(218)+M(220)+M(236))) * den(17)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,541)
  Gcoeff = (c(5)*(-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62)+M(92))+c(6)*(M(136)+M(139)-M(140)-M(150) &
    -M(178)-M(194)+M(196)+M(238))) * den(17)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,542)
  Gcoeff = (c(6)*(-M(135)+M(136)+M(139)-M(140)-M(145)+M(146)+M(149)-M(150)+M(176)-M(178)-M(194)+M(196)+M(218)-M(220)-M(236) &
    +M(238))) * den(17)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,543)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(141)+M(143)-M(144)-M(151) &
    -M(200)-M(212)+M(214)+M(242))) * den(23)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,544)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(133)-M(134)+M(142)-M(152) &
    -M(170)+M(172)-M(202)+M(244))) * den(23)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,545)
  Gcoeff = (c(6)*(M(133)-M(134)-M(141)+M(142)-M(143)+M(144)+M(151)-M(152)-M(170)+M(172)+M(200)-M(202)+M(212)-M(214)-M(242) &
    +M(244))) * den(23)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,546)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(6)*(M(131)+M(136)-M(140)-M(154) &
    -M(164)-M(178)+M(196)+M(250))) * den(152)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,547)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(6)*(M(133)+M(135)-M(146)-M(152) &
    -M(170)-M(176)+M(220)+M(244))) * den(152)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,548)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(135)-M(136)+M(140)-M(146)-M(152)+M(154)+M(164)-M(170)-M(176)+M(178)-M(196)+M(220)+M(244) &
    -M(250))) * den(152)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,549)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(-M(134)+M(137)+M(142)-M(153) &
    +M(172)-M(188)-M(202)+M(248))) * den(147)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,550)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(M(139)+M(141)-M(144)-M(150) &
    -M(194)-M(200)+M(214)+M(238))) * den(147)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,551)
  Gcoeff = (c(6)*(M(134)-M(137)+M(139)+M(141)-M(142)-M(144)-M(150)+M(153)-M(172)+M(188)-M(194)-M(200)+M(202)+M(214)+M(238) &
    -M(248))) * den(147)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,552)
  Gcoeff = (c(4)*(-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42) &
    +M(43)-M(44)-M(45)+M(46)-M(47)+M(48)+M(49)-M(50)-M(51)-M(52)-M(56)-M(59)-M(62)+M(68)+M(71)-M(74)+M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(133)-M(152)-M(170)+M(244))) * den(11)
  T4sum(1:35,138) = T4sum(1:35,138) + Gcoeff * G3tensor(:,112)
  Gcoeff = (c(4)*(-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42) &
    -M(43)-M(44)-M(45)-M(46)-M(47)+M(48)-M(49)+M(50)+M(51)+M(52)-M(56)-M(59)+M(62)+M(68)+M(71)+M(74)+M(80)-M(83)+M(86)-M(92)-M(95) &
    -M(98))+c(6)*(-M(139)+M(150)+M(194)-M(238))) * den(11)
  T4sum(1:35,138) = T4sum(1:35,138) + Gcoeff * G3tensor(:,113)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(133)-M(139)+M(150)+M(152) &
    +M(170)+M(194)-M(238)-M(244))) * den(11)
  T4sum(1:35,138) = T4sum(1:35,138) + Gcoeff * G3tensor(:,114)
  Gcoeff = (c(5)*(-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62)+M(92))+c(6)*(M(135)+M(145)-M(146)-M(149) &
    -M(176)-M(218)+M(220)+M(236))) * den(17)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,289)
  Gcoeff = (c(5)*(-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62)+M(92))+c(6)*(M(138)-M(141)+M(144)-M(147) &
    -M(190)+M(200)-M(214)+M(224))) * den(17)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,290)
  Gcoeff = (c(6)*(-M(135)+M(138)-M(141)+M(144)-M(145)+M(146)-M(147)+M(149)+M(176)-M(190)+M(200)-M(214)+M(218)-M(220)+M(224) &
    -M(236))) * den(17)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,291)
  Gcoeff = (c(5)*(-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62)+M(92))+c(6)*(M(138)-M(141)+M(144)-M(147) &
    -M(190)+M(200)-M(214)+M(224))) * den(17)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,553)
  Gcoeff = (c(5)*(-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62)+M(92))+c(6)*(M(136)+M(139)-M(140)-M(150) &
    -M(178)-M(194)+M(196)+M(238))) * den(17)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,554)
  Gcoeff = (c(6)*(M(136)-M(138)+M(139)-M(140)+M(141)-M(144)+M(147)-M(150)-M(178)+M(190)-M(194)+M(196)-M(200)+M(214)-M(224) &
    +M(238))) * den(17)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,555)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(6)*(M(132)+M(134)-M(142)-M(148) &
    -M(166)-M(172)+M(202)+M(226))) * den(152)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,358)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(6)*(M(131)+M(136)-M(140)-M(154) &
    -M(164)-M(178)+M(196)+M(250))) * den(152)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,359)
  Gcoeff = (c(6)*(M(131)-M(132)-M(134)+M(136)-M(140)+M(142)+M(148)-M(154)-M(164)+M(166)+M(172)-M(178)+M(196)-M(202)-M(226) &
    +M(250))) * den(152)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,360)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(133)-M(139)+M(150)+M(152) &
    +M(170)+M(194)-M(238)-M(244))) * den(307)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,562)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(137)-M(143)+M(151)+M(153) &
    +M(188)+M(212)-M(242)-M(248))) * den(307)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,563)
  Gcoeff = (c(6)*(M(133)-M(137)+M(139)-M(143)-M(150)+M(151)-M(152)+M(153)-M(170)+M(188)-M(194)+M(212)+M(238)-M(242)+M(244) &
    -M(248))) * den(307)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,564)
  Gcoeff = (c(4)*(-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42) &
    -M(43)-M(44)-M(45)-M(46)-M(47)+M(48)-M(49)+M(50)+M(51)+M(52)-M(56)-M(59)+M(62)+M(68)+M(71)+M(74)+M(80)-M(83)+M(86)-M(92)-M(95) &
    -M(98))+c(6)*(-M(139)+M(150)+M(194)-M(238))) * den(11)
  T4sum(1:35,142) = T4sum(1:35,142) + Gcoeff * G3tensor(:,181)
  Gcoeff = (c(4)*(-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)-M(41)+M(42) &
    -M(43)+M(44)+M(45)+M(46)-M(47)-M(48)-M(49)+M(50)-M(51)+M(52)+M(56)-M(59)+M(62)-M(68)-M(71)-M(74)+M(80)+M(83)+M(86)-M(92)+M(95) &
    -M(98))+c(6)*(M(141)-M(144)-M(200)+M(214))) * den(11)
  T4sum(1:35,142) = T4sum(1:35,142) + Gcoeff * G3tensor(:,182)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(M(139)+M(141)-M(144)-M(150) &
    -M(194)-M(200)+M(214)+M(238))) * den(11)
  T4sum(1:35,142) = T4sum(1:35,142) + Gcoeff * G3tensor(:,183)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(6)*(M(132)+M(134)-M(142)-M(148) &
    -M(166)-M(172)+M(202)+M(226))) * den(152)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,25)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(6)*(M(133)+M(135)-M(146)-M(152) &
    -M(170)-M(176)+M(220)+M(244))) * den(152)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,31)
  Gcoeff = (c(6)*(-M(132)+M(133)-M(134)+M(135)+M(142)-M(146)+M(148)-M(152)+M(166)-M(170)+M(172)-M(176)-M(202)+M(220)-M(226) &
    +M(244))) * den(152)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,37)
  Gcoeff = (c(4)*(M(25)-M(26)-M(27)+M(28)-M(29)+M(30)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42) &
    +M(43)-M(44)-M(45)-M(46)+M(47)+M(48)+M(49)-M(50)+M(51)-M(52)-M(56)+M(59)-M(62)+M(68)+M(71)+M(74)-M(80)-M(83)-M(86)+M(92)-M(95) &
    +M(98))+c(6)*(-M(141)+M(144)+M(200)-M(214))) * den(11)
  T4sum(1:35,86) = T4sum(1:35,86) + Gcoeff * G3tensor(:,34)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42) &
    +M(43)-M(44)-M(45)+M(46)+M(47)+M(48)+M(49)-M(50)-M(51)-M(52)-M(56)+M(59)-M(62)+M(68)+M(71)-M(74)-M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(143)-M(151)-M(212)+M(242))) * den(11)
  T4sum(1:35,86) = T4sum(1:35,86) + Gcoeff * G3tensor(:,35)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(141)+M(143)-M(144)-M(151) &
    -M(200)-M(212)+M(214)+M(242))) * den(11)
  T4sum(1:35,86) = T4sum(1:35,86) + Gcoeff * G3tensor(:,36)
  Gcoeff = (c(6)*(M(134)-M(137)+M(139)+M(141)-M(142)-M(144)-M(150)+M(153)-M(172)+M(188)-M(194)-M(200)+M(202)+M(214)+M(238) &
    -M(248))) * den(314)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,184)
  Gcoeff = (c(6)*(M(133)-M(134)-M(141)+M(142)-M(143)+M(144)+M(151)-M(152)-M(170)+M(172)+M(200)-M(202)+M(212)-M(214)-M(242) &
    +M(244))) * den(314)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,185)
  Gcoeff = (c(6)*(-M(133)+M(137)-M(139)+M(143)+M(150)-M(151)+M(152)-M(153)+M(170)-M(188)+M(194)-M(212)-M(238)+M(242)-M(244) &
    +M(248))) * den(314)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,186)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(141)+M(143)-M(144)-M(151) &
    -M(200)-M(212)+M(214)+M(242))) * den(23)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,400)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(132)-M(135)+M(146)-M(148) &
    -M(166)+M(176)-M(220)+M(226))) * den(23)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,401)
  Gcoeff = (c(6)*(M(132)-M(135)-M(141)-M(143)+M(144)+M(146)-M(148)+M(151)-M(166)+M(176)+M(200)+M(212)-M(214)-M(220)+M(226) &
    -M(242))) * den(23)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,402)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(132)-M(135)+M(146)-M(148) &
    -M(166)+M(176)-M(220)+M(226))) * den(23)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,565)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(133)-M(134)+M(142)-M(152) &
    -M(170)+M(172)-M(202)+M(244))) * den(23)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,566)
  Gcoeff = (c(6)*(-M(132)+M(133)-M(134)+M(135)+M(142)-M(146)+M(148)-M(152)+M(166)-M(170)+M(172)-M(176)-M(202)+M(220)-M(226) &
    +M(244))) * den(23)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,567)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(-M(136)+M(138)+M(140)-M(147) &
    +M(178)-M(190)-M(196)+M(224))) * den(147)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,361)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(-M(134)+M(137)+M(142)-M(153) &
    +M(172)-M(188)-M(202)+M(248))) * den(147)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,362)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(137)-M(138)-M(140)+M(142)+M(147)-M(153)+M(172)-M(178)-M(188)+M(190)+M(196)-M(202)-M(224) &
    +M(248))) * den(147)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,363)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(133)-M(139)+M(150)+M(152) &
    +M(170)+M(194)-M(238)-M(244))) * den(307)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,574)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(131)-M(145)+M(149)+M(154) &
    +M(164)+M(218)-M(236)-M(250))) * den(307)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,575)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(139)-M(145)+M(149)-M(150)-M(152)+M(154)+M(164)-M(170)-M(194)+M(218)-M(236)+M(238)+M(244) &
    -M(250))) * den(307)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,576)
  Gcoeff = (c(4)*(-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42) &
    -M(43)+M(44)-M(45)+M(46)+M(47)+M(48)+M(49)+M(50)-M(51)-M(52)+M(56)+M(59)+M(62)-M(68)+M(71)-M(74)-M(80)-M(83)-M(86)-M(92)+M(95) &
    +M(98))+c(6)*(-M(135)+M(146)+M(176)-M(220))) * den(11)
  T4sum(1:35,145) = T4sum(1:35,145) + Gcoeff * G3tensor(:,115)
  Gcoeff = (c(4)*(-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42) &
    +M(43)-M(44)-M(45)+M(46)-M(47)+M(48)+M(49)-M(50)-M(51)-M(52)-M(56)-M(59)-M(62)+M(68)+M(71)-M(74)+M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(133)-M(152)-M(170)+M(244))) * den(11)
  T4sum(1:35,145) = T4sum(1:35,145) + Gcoeff * G3tensor(:,116)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(6)*(M(133)+M(135)-M(146)-M(152) &
    -M(170)-M(176)+M(220)+M(244))) * den(11)
  T4sum(1:35,145) = T4sum(1:35,145) + Gcoeff * G3tensor(:,117)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(-M(136)+M(138)+M(140)-M(147) &
    +M(178)-M(190)-M(196)+M(224))) * den(147)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(M(139)+M(141)-M(144)-M(150) &
    -M(194)-M(200)+M(214)+M(238))) * den(147)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,13)
  Gcoeff = (c(6)*(M(136)-M(138)+M(139)-M(140)+M(141)-M(144)+M(147)-M(150)-M(178)+M(190)-M(194)+M(196)-M(200)+M(214)-M(224) &
    +M(238))) * den(147)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,19)
  Gcoeff = (c(4)*(-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42) &
    -M(43)+M(44)-M(45)+M(46)+M(47)+M(48)+M(49)+M(50)-M(51)-M(52)+M(56)+M(59)+M(62)-M(68)+M(71)-M(74)-M(80)-M(83)-M(86)-M(92)+M(95) &
    +M(98))+c(6)*(-M(135)+M(146)+M(176)-M(220))) * den(11)
  T4sum(1:35,92) = T4sum(1:35,92) + Gcoeff * G3tensor(:,103)
  Gcoeff = (c(4)*(-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42) &
    +M(43)+M(44)-M(45)+M(46)+M(47)+M(48)+M(49)-M(50)-M(51)-M(52)+M(56)+M(59)-M(62)-M(68)+M(71)-M(74)-M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(145)-M(149)-M(218)+M(236))) * den(11)
  T4sum(1:35,92) = T4sum(1:35,92) + Gcoeff * G3tensor(:,104)
  Gcoeff = (c(5)*(-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62)+M(92))+c(6)*(M(135)+M(145)-M(146)-M(149) &
    -M(176)-M(218)+M(220)+M(236))) * den(11)
  T4sum(1:35,92) = T4sum(1:35,92) + Gcoeff * G3tensor(:,105)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(135)-M(136)+M(140)-M(146)-M(152)+M(154)+M(164)-M(170)-M(176)+M(178)-M(196)+M(220)+M(244) &
    -M(250))) * den(333)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,193)
  Gcoeff = (c(6)*(-M(135)+M(136)+M(139)-M(140)-M(145)+M(146)+M(149)-M(150)+M(176)-M(178)-M(194)+M(196)+M(218)-M(220)-M(236) &
    +M(238))) * den(333)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,194)
  Gcoeff = (c(6)*(M(131)-M(133)-M(139)+M(145)-M(149)+M(150)+M(152)-M(154)-M(164)+M(170)+M(194)-M(218)+M(236)-M(238)-M(244) &
    +M(250))) * den(333)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,195)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(137)-M(143)+M(151)+M(153) &
    +M(188)+M(212)-M(242)-M(248))) * den(307)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,577)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(131)-M(145)+M(149)+M(154) &
    +M(164)+M(218)-M(236)-M(250))) * den(307)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,578)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(143)-M(145)+M(149)-M(151)-M(153)+M(154)+M(164)-M(188)-M(212)+M(218)-M(236)+M(242)+M(248) &
    -M(250))) * den(307)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,579)
  Gcoeff = (c(4)*(-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42) &
    +M(43)+M(44)-M(45)+M(46)+M(47)+M(48)+M(49)-M(50)-M(51)-M(52)+M(56)+M(59)-M(62)-M(68)+M(71)-M(74)-M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(145)-M(149)-M(218)+M(236))) * den(11)
  T4sum(1:35,99) = T4sum(1:35,99) + Gcoeff * G3tensor(:,13)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42) &
    +M(43)-M(44)-M(45)+M(46)+M(47)+M(48)+M(49)-M(50)-M(51)-M(52)-M(56)+M(59)-M(62)+M(68)+M(71)-M(74)-M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(143)-M(151)-M(212)+M(242))) * den(11)
  T4sum(1:35,99) = T4sum(1:35,99) + Gcoeff * G3tensor(:,16)
  Gcoeff = (c(5)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56)+M(68))+c(6)*(M(143)-M(145)+M(149)-M(151) &
    -M(212)+M(218)-M(236)+M(242))) * den(11)
  T4sum(1:35,99) = T4sum(1:35,99) + Gcoeff * G3tensor(:,19)
  Gcoeff = (c(6)*(M(131)-M(132)-M(134)+M(136)-M(140)+M(142)+M(148)-M(154)-M(164)+M(166)+M(172)-M(178)+M(196)-M(202)-M(226) &
    +M(250))) * den(346)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,58)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(346)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,59)
  Gcoeff = (c(6)*(M(134)-M(136)-M(137)+M(138)+M(140)-M(142)-M(147)+M(153)-M(172)+M(178)+M(188)-M(190)-M(196)+M(202)+M(224) &
    -M(248))) * den(346)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,60)
  Gcoeff = (c(6)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(29)
  T3sum(1:35,81) = T3sum(1:35,81) + Gcoeff * G3tensor(:,160)
  Gcoeff = (c(6)*(M(132)-M(135)-M(141)-M(143)+M(144)+M(146)-M(148)+M(151)-M(166)+M(176)+M(200)+M(212)-M(214)-M(220)+M(226) &
    -M(242))) * den(29)
  T3sum(1:35,81) = T3sum(1:35,81) + Gcoeff * G3tensor(:,161)
  Gcoeff = (c(6)*(M(135)-M(138)+M(141)-M(144)+M(145)-M(146)+M(147)-M(149)-M(176)+M(190)-M(200)+M(214)-M(218)+M(220)-M(224) &
    +M(236))) * den(29)
  T3sum(1:35,81) = T3sum(1:35,81) + Gcoeff * G3tensor(:,162)
  Gcoeff = (c(5)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(44)+M(53)-M(56)+M(65))+c(6)*(-M(140)+M(155)-M(161)+M(177) &
    -M(178)+M(182)-M(246)+M(249))) * den(34)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,364)
  Gcoeff = (c(5)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(44)+M(53)-M(56)+M(65))+c(6)*(-M(142)+M(156)-M(162)+M(171) &
    -M(172)+M(184)-M(222)+M(225))) * den(34)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,365)
  Gcoeff = (c(6)*(M(140)-M(142)-M(155)+M(156)+M(161)-M(162)+M(171)-M(172)-M(177)+M(178)-M(182)+M(184)-M(222)+M(225)+M(246) &
    -M(249))) * den(34)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,366)
  Gcoeff = (c(5)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(44)+M(53)-M(56)+M(65))+c(6)*(-M(142)+M(156)-M(162)+M(171) &
    -M(172)+M(184)-M(222)+M(225))) * den(34)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,193)
  Gcoeff = (c(5)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(44)+M(53)-M(56)+M(65))+c(6)*(M(167)-M(169)+M(173)-M(175)-M(206) &
    +M(216)-M(230)+M(240))) * den(34)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,194)
  Gcoeff = (c(6)*(M(142)-M(156)+M(162)+M(167)-M(169)-M(171)+M(172)+M(173)-M(175)-M(184)-M(206)+M(216)+M(222)-M(225)-M(230) &
    +M(240))) * den(34)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,195)
  Gcoeff = (c(5)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(44)+M(53)-M(56)+M(65))+c(6)*(-M(140)+M(155)-M(161)+M(177) &
    -M(178)+M(182)-M(246)+M(249))) * den(34)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,40)
  Gcoeff = (c(5)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(44)+M(53)-M(56)+M(65))+c(6)*(M(167)-M(169)+M(173)-M(175)-M(206) &
    +M(216)-M(230)+M(240))) * den(34)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,41)
  Gcoeff = (c(6)*(M(140)-M(155)+M(161)+M(167)-M(169)+M(173)-M(175)-M(177)+M(178)-M(182)-M(206)+M(216)-M(230)+M(240)+M(246) &
    -M(249))) * den(34)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,42)
  Gcoeff = (c(5)*(-M(11)+M(12)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(50)+M(55)-M(62)+M(89))+c(6)*(-M(152)+M(159)+M(169)-M(170) &
    -M(173)-M(216)+M(219)+M(230))) * den(38)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,580)
  Gcoeff = (c(5)*(-M(11)+M(12)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(50)+M(55)-M(62)+M(89))+c(6)*(-M(154)+M(160)+M(163)-M(164) &
    -M(174)-M(192)+M(195)+M(232))) * den(38)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,581)
  Gcoeff = (c(6)*(M(152)-M(154)-M(159)+M(160)+M(163)-M(164)-M(169)+M(170)+M(173)-M(174)-M(192)+M(195)+M(216)-M(219)-M(230) &
    +M(232))) * den(38)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,582)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(37)-M(38)-M(39)+M(40)+M(58)-M(63)-M(75)+M(96))+c(6)*(-M(146)+M(148)+M(157)-M(158) &
    +M(166)-M(176)-M(201)+M(243))) * den(61)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,583)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(37)-M(38)-M(39)+M(40)+M(58)-M(63)-M(75)+M(96))+c(6)*(M(165)+M(167)-M(168)-M(175) &
    -M(198)-M(206)+M(208)+M(240))) * den(61)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,584)
  Gcoeff = (c(6)*(M(146)-M(148)-M(157)+M(158)+M(165)-M(166)+M(167)-M(168)-M(175)+M(176)-M(198)+M(201)-M(206)+M(208)+M(240) &
    -M(243))) * den(61)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,585)
  Gcoeff = (c(5)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(6)*(M(140)+M(154)-M(155)-M(160) &
    +M(164)+M(178)-M(195)-M(249))) * den(176)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,586)
  Gcoeff = (c(5)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(6)*(M(146)+M(152)-M(157)-M(159) &
    +M(170)+M(176)-M(219)-M(243))) * den(176)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,587)
  Gcoeff = (c(6)*(-M(140)+M(146)+M(152)-M(154)+M(155)-M(157)-M(159)+M(160)-M(164)+M(170)+M(176)-M(178)+M(195)-M(219)-M(243) &
    +M(249))) * den(176)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,588)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(148)-M(158)+M(161)+M(166) &
    -M(177)-M(182)-M(201)+M(246))) * den(171)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,589)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(163)+M(165)-M(168)-M(174) &
    -M(192)-M(198)+M(208)+M(232))) * den(171)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,590)
  Gcoeff = (c(6)*(-M(148)+M(158)-M(161)+M(163)+M(165)-M(166)-M(168)-M(174)+M(177)+M(182)-M(192)-M(198)+M(201)+M(208)+M(232) &
    -M(246))) * den(171)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,591)
  Gcoeff = (c(4)*(-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)+M(37)-M(38)-M(39)+M(40)-M(44)-M(47) &
    -M(50)+M(53)+M(54)+M(55)-M(56)-M(57)+M(58)-M(59)+M(60)+M(61)-M(62)-M(63)-M(64)+M(65)+M(72)-M(75)+M(77)-M(84)-M(87)+M(89)+M(96) &
    +M(99))+c(6)*(-M(146)+M(157)-M(176)+M(243))) * den(32)
  T4sum(1:35,150) = T4sum(1:35,150) + Gcoeff * G3tensor(:,124)
  Gcoeff = (c(4)*(-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)+M(37)-M(38)-M(39)+M(40)-M(44)-M(47) &
    +M(50)+M(53)+M(54)-M(55)-M(56)-M(57)-M(58)-M(59)+M(60)-M(61)+M(62)+M(63)+M(64)+M(65)+M(72)+M(75)+M(77)-M(84)+M(87)-M(89)-M(96) &
    -M(99))+c(6)*(-M(163)+M(174)+M(192)-M(232))) * den(32)
  T4sum(1:35,150) = T4sum(1:35,150) + Gcoeff * G3tensor(:,125)
  Gcoeff = (c(5)*(M(50)-M(55)-M(58)-M(61)+M(62)+M(63)+M(64)+M(75)+M(87)-M(89)-M(96)-M(99))+c(6)*(M(146)-M(157)-M(163)+M(174) &
    +M(176)+M(192)-M(232)-M(243))) * den(32)
  T4sum(1:35,150) = T4sum(1:35,150) + Gcoeff * G3tensor(:,126)
  Gcoeff = (c(5)*(-M(11)+M(12)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(50)+M(55)-M(62)+M(89))+c(6)*(-M(152)+M(159)+M(169)-M(170) &
    -M(173)-M(216)+M(219)+M(230))) * den(38)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,205)
  Gcoeff = (c(5)*(-M(11)+M(12)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(50)+M(55)-M(62)+M(89))+c(6)*(M(162)-M(165)+M(168)-M(171) &
    -M(184)+M(198)-M(208)+M(222))) * den(38)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,206)
  Gcoeff = (c(6)*(M(152)-M(159)+M(162)-M(165)+M(168)-M(169)+M(170)-M(171)+M(173)-M(184)+M(198)-M(208)+M(216)-M(219)+M(222) &
    -M(230))) * den(38)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,207)
  Gcoeff = (c(5)*(-M(11)+M(12)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(50)+M(55)-M(62)+M(89))+c(6)*(M(162)-M(165)+M(168)-M(171) &
    -M(184)+M(198)-M(208)+M(222))) * den(38)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,592)
  Gcoeff = (c(5)*(-M(11)+M(12)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(50)+M(55)-M(62)+M(89))+c(6)*(-M(154)+M(160)+M(163)-M(164) &
    -M(174)-M(192)+M(195)+M(232))) * den(38)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,593)
  Gcoeff = (c(6)*(-M(154)+M(160)-M(162)+M(163)-M(164)+M(165)-M(168)+M(171)-M(174)+M(184)-M(192)+M(195)-M(198)+M(208)-M(222) &
    +M(232))) * den(38)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,594)
  Gcoeff = (c(5)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(6)*(M(142)+M(148)-M(156)-M(158) &
    +M(166)+M(172)-M(201)-M(225))) * den(176)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,367)
  Gcoeff = (c(5)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(6)*(M(140)+M(154)-M(155)-M(160) &
    +M(164)+M(178)-M(195)-M(249))) * den(176)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,368)
  Gcoeff = (c(6)*(M(140)-M(142)-M(148)+M(154)-M(155)+M(156)+M(158)-M(160)+M(164)-M(166)-M(172)+M(178)-M(195)+M(201)+M(225) &
    -M(249))) * den(176)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,369)
  Gcoeff = (c(5)*(M(50)-M(55)-M(58)-M(61)+M(62)+M(63)+M(64)+M(75)+M(87)-M(89)-M(96)-M(99))+c(6)*(M(146)-M(157)-M(163)+M(174) &
    +M(176)+M(192)-M(232)-M(243))) * den(369)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,601)
  Gcoeff = (c(5)*(M(50)-M(55)-M(58)-M(61)+M(62)+M(63)+M(64)+M(75)+M(87)-M(89)-M(96)-M(99))+c(6)*(-M(161)-M(167)+M(175)+M(177) &
    +M(182)+M(206)-M(240)-M(246))) * den(369)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,602)
  Gcoeff = (c(6)*(-M(146)+M(157)-M(161)+M(163)-M(167)-M(174)+M(175)-M(176)+M(177)+M(182)-M(192)+M(206)+M(232)-M(240)+M(243) &
    -M(246))) * den(369)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,603)
  Gcoeff = (c(4)*(-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)+M(37)-M(38)-M(39)+M(40)-M(44)-M(47) &
    +M(50)+M(53)+M(54)-M(55)-M(56)-M(57)-M(58)-M(59)+M(60)-M(61)+M(62)+M(63)+M(64)+M(65)+M(72)+M(75)+M(77)-M(84)+M(87)-M(89)-M(96) &
    -M(99))+c(6)*(-M(163)+M(174)+M(192)-M(232))) * den(32)
  T4sum(1:35,154) = T4sum(1:35,154) + Gcoeff * G3tensor(:,203)
  Gcoeff = (c(4)*(-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)+M(37)-M(38)-M(39)+M(40)+M(44)-M(47) &
    +M(50)-M(53)+M(54)-M(55)+M(56)+M(57)+M(58)-M(59)-M(60)-M(61)+M(62)-M(63)+M(64)-M(65)-M(72)-M(75)+M(77)+M(84)+M(87)-M(89)+M(96) &
    -M(99))+c(6)*(M(165)-M(168)-M(198)+M(208))) * den(32)
  T4sum(1:35,154) = T4sum(1:35,154) + Gcoeff * G3tensor(:,204)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(163)+M(165)-M(168)-M(174) &
    -M(192)-M(198)+M(208)+M(232))) * den(32)
  T4sum(1:35,154) = T4sum(1:35,154) + Gcoeff * G3tensor(:,205)
  Gcoeff = (c(5)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(6)*(M(142)+M(148)-M(156)-M(158) &
    +M(166)+M(172)-M(201)-M(225))) * den(176)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,26)
  Gcoeff = (c(5)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(6)*(M(146)+M(152)-M(157)-M(159) &
    +M(170)+M(176)-M(219)-M(243))) * den(176)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,32)
  Gcoeff = (c(6)*(-M(142)+M(146)-M(148)+M(152)+M(156)-M(157)+M(158)-M(159)-M(166)+M(170)-M(172)+M(176)+M(201)-M(219)+M(225) &
    -M(243))) * den(176)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,38)
  Gcoeff = (c(4)*(-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(17)-M(18)+M(21)-M(22)-M(23)+M(24)-M(37)+M(38)+M(39)-M(40)+M(44)-M(47) &
    +M(50)-M(53)+M(54)-M(55)+M(56)+M(57)-M(58)-M(59)-M(60)-M(61)+M(62)+M(63)+M(64)-M(65)-M(72)+M(75)+M(77)+M(84)+M(87)-M(89)-M(96) &
    -M(99))+c(6)*(-M(167)+M(175)+M(206)-M(240))) * den(32)
  T4sum(1:35,59) = T4sum(1:35,59) + Gcoeff * G3tensor(:,206)
  Gcoeff = (c(4)*(-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)+M(37)-M(38)-M(39)+M(40)+M(44)-M(47) &
    +M(50)-M(53)+M(54)-M(55)+M(56)+M(57)+M(58)-M(59)-M(60)-M(61)+M(62)-M(63)+M(64)-M(65)-M(72)-M(75)+M(77)+M(84)+M(87)-M(89)+M(96) &
    -M(99))+c(6)*(M(165)-M(168)-M(198)+M(208))) * den(32)
  T4sum(1:35,59) = T4sum(1:35,59) + Gcoeff * G3tensor(:,207)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(37)-M(38)-M(39)+M(40)+M(58)-M(63)-M(75)+M(96))+c(6)*(M(165)+M(167)-M(168)-M(175) &
    -M(198)-M(206)+M(208)+M(240))) * den(32)
  T4sum(1:35,59) = T4sum(1:35,59) + Gcoeff * G3tensor(:,208)
  Gcoeff = (c(6)*(-M(148)+M(158)-M(161)+M(163)+M(165)-M(166)-M(168)-M(174)+M(177)+M(182)-M(192)-M(198)+M(201)+M(208)+M(232) &
    -M(246))) * den(376)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,209)
  Gcoeff = (c(6)*(-M(146)+M(148)+M(157)-M(158)-M(165)+M(166)-M(167)+M(168)+M(175)-M(176)+M(198)-M(201)+M(206)-M(208)-M(240) &
    +M(243))) * den(376)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,210)
  Gcoeff = (c(6)*(M(146)-M(157)+M(161)-M(163)+M(167)+M(174)-M(175)+M(176)-M(177)-M(182)+M(192)-M(206)-M(232)+M(240)-M(243) &
    +M(246))) * den(376)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,211)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(-M(134)+M(158)+M(179)-M(185)+M(201) &
    -M(202)-M(245)+M(247))) * den(42)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,370)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(-M(136)+M(160)+M(180)-M(186)+M(195) &
    -M(196)-M(221)+M(223))) * den(42)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,371)
  Gcoeff = (c(6)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(42)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,372)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(-M(136)+M(160)+M(180)-M(186)+M(195) &
    -M(196)-M(221)+M(223))) * den(42)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,172)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(M(191)-M(193)+M(197)-M(199)-M(204) &
    +M(210)-M(228)+M(234))) * den(42)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,173)
  Gcoeff = (c(6)*(M(136)-M(160)-M(180)+M(186)+M(191)-M(193)-M(195)+M(196)+M(197)-M(199)-M(204)+M(210)+M(221)-M(223)-M(228) &
    +M(234))) * den(42)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,174)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(-M(134)+M(158)+M(179)-M(185)+M(201) &
    -M(202)-M(245)+M(247))) * den(42)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,61)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(M(191)-M(193)+M(197)-M(199)-M(204) &
    +M(210)-M(228)+M(234))) * den(42)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,62)
  Gcoeff = (c(6)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(42)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,63)
  Gcoeff = (c(5)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(6)*(-M(133)+M(157)-M(191)+M(193)+M(228) &
    -M(234)+M(243)-M(244))) * den(46)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,604)
  Gcoeff = (c(5)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(6)*(-M(131)+M(155)-M(215)+M(217)+M(227) &
    -M(233)+M(249)-M(250))) * den(46)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,605)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(155)-M(157)+M(191)-M(193)-M(215)+M(217)+M(227)-M(228)-M(233)+M(234)-M(243)+M(244)+M(249) &
    -M(250))) * den(46)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,606)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(37)+M(38)+M(39)-M(40)+M(102)-M(105)-M(111)+M(121))+c(6)*(M(132)-M(135)-M(156)+M(159) &
    +M(219)-M(220)-M(225)+M(226))) * den(97)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,607)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(37)+M(38)+M(39)-M(40)+M(102)-M(105)-M(111)+M(121))+c(6)*(M(197)-M(199)+M(203)-M(204) &
    -M(209)+M(210)-M(239)+M(241))) * den(97)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,608)
  Gcoeff = (c(6)*(-M(132)+M(135)+M(156)-M(159)+M(197)-M(199)+M(203)-M(204)-M(209)+M(210)-M(219)+M(220)+M(225)-M(226)-M(239) &
    +M(241))) * den(97)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,609)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(53)-M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92))+c(6)*(M(131)+M(136)-M(155)-M(160) &
    -M(195)+M(196)-M(249)+M(250))) * den(200)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,610)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(53)-M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92))+c(6)*(M(133)+M(135)-M(157)-M(159) &
    -M(219)+M(220)-M(243)+M(244))) * den(200)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,611)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(135)-M(136)+M(155)-M(157)-M(159)+M(160)+M(195)-M(196)-M(219)+M(220)-M(243)+M(244)+M(249) &
    -M(250))) * den(200)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,612)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(132)-M(156)-M(180) &
    +M(186)+M(221)-M(223)-M(225)+M(226))) * den(197)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,613)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(203)-M(209)-M(215) &
    +M(217)+M(227)-M(233)-M(239)+M(241))) * den(197)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,614)
  Gcoeff = (c(6)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(197)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,615)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(5)+M(6)-M(7)+M(8)-M(13)+M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42)-M(43)+M(53) &
    +M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92)-M(101)+M(102)+M(103)+M(104)-M(105)-M(106)+M(109)-M(111)-M(115)-M(117)+M(121) &
    +M(123))+c(6)*(-M(135)+M(159)+M(219)-M(220))) * den(41)
  T4sum(1:35,159) = T4sum(1:35,159) + Gcoeff * G3tensor(:,127)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(5)+M(6)-M(7)+M(8)-M(13)+M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42)+M(43)+M(53) &
    +M(54)-M(55)+M(65)-M(68)+M(77)-M(80)-M(89)+M(92)-M(101)-M(102)+M(103)-M(104)+M(105)+M(106)+M(109)+M(111)-M(115)+M(117)-M(121) &
    -M(123))+c(6)*(M(215)-M(217)-M(227)+M(233))) * den(41)
  T4sum(1:35,159) = T4sum(1:35,159) + Gcoeff * G3tensor(:,128)
  Gcoeff = (c(5)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(6)*(M(135)-M(159)+M(215) &
    -M(217)-M(219)+M(220)-M(227)+M(233))) * den(41)
  T4sum(1:35,159) = T4sum(1:35,159) + Gcoeff * G3tensor(:,129)
  Gcoeff = (c(5)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(6)*(-M(179)+M(185)-M(203)+M(209)+M(239) &
    -M(241)+M(245)-M(247))) * den(46)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,616)
  Gcoeff = (c(5)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(6)*(-M(131)+M(155)-M(215)+M(217)+M(227) &
    -M(233)+M(249)-M(250))) * den(46)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,617)
  Gcoeff = (c(6)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(46)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,618)
  Gcoeff = (c(5)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(6)*(-M(133)+M(157)-M(191)+M(193)+M(228) &
    -M(234)+M(243)-M(244))) * den(46)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,73)
  Gcoeff = (c(5)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(6)*(-M(179)+M(185)-M(203)+M(209)+M(239) &
    -M(241)+M(245)-M(247))) * den(46)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,74)
  Gcoeff = (c(6)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(46)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,75)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(53)-M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92))+c(6)*(M(132)+M(134)-M(156)-M(158) &
    -M(201)+M(202)-M(225)+M(226))) * den(200)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,373)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(53)-M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92))+c(6)*(M(131)+M(136)-M(155)-M(160) &
    -M(195)+M(196)-M(249)+M(250))) * den(200)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,374)
  Gcoeff = (c(6)*(M(131)-M(132)-M(134)+M(136)-M(155)+M(156)+M(158)-M(160)-M(195)+M(196)+M(201)-M(202)+M(225)-M(226)-M(249) &
    +M(250))) * den(200)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,375)
  Gcoeff = (c(5)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(6)*(M(135)-M(159)+M(215) &
    -M(217)-M(219)+M(220)-M(227)+M(233))) * den(400)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,625)
  Gcoeff = (c(5)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(6)*(M(180)-M(186)-M(197) &
    +M(199)+M(204)-M(210)-M(221)+M(223))) * den(400)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,626)
  Gcoeff = (c(6)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(400)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,627)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(5)+M(6)-M(7)+M(8)-M(13)+M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42)+M(43)+M(53) &
    +M(54)-M(55)+M(65)-M(68)+M(77)-M(80)-M(89)+M(92)-M(101)-M(102)+M(103)-M(104)+M(105)+M(106)+M(109)+M(111)-M(115)+M(117)-M(121) &
    -M(123))+c(6)*(M(215)-M(217)-M(227)+M(233))) * den(41)
  T4sum(1:35,163) = T4sum(1:35,163) + Gcoeff * G3tensor(:,218)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(5)+M(6)-M(7)+M(8)-M(13)+M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42)+M(43)-M(53) &
    +M(54)-M(55)-M(65)+M(68)+M(77)-M(80)-M(89)+M(92)+M(101)+M(102)-M(103)-M(104)-M(105)+M(106)-M(109)-M(111)+M(115)+M(117)+M(121) &
    -M(123))+c(6)*(M(203)-M(209)-M(239)+M(241))) * den(41)
  T4sum(1:35,163) = T4sum(1:35,163) + Gcoeff * G3tensor(:,219)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(203)-M(209)-M(215) &
    +M(217)+M(227)-M(233)-M(239)+M(241))) * den(41)
  T4sum(1:35,163) = T4sum(1:35,163) + Gcoeff * G3tensor(:,220)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(53)-M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92))+c(6)*(M(132)+M(134)-M(156)-M(158) &
    -M(201)+M(202)-M(225)+M(226))) * den(200)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,27)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(53)-M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92))+c(6)*(M(133)+M(135)-M(157)-M(159) &
    -M(219)+M(220)-M(243)+M(244))) * den(200)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,33)
  Gcoeff = (c(6)*(-M(132)+M(133)-M(134)+M(135)+M(156)-M(157)+M(158)-M(159)+M(201)-M(202)-M(219)+M(220)+M(225)-M(226)-M(243) &
    +M(244))) * den(200)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,39)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(5)+M(6)-M(7)+M(8)+M(13)-M(14)+M(19)-M(20)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42)+M(43)-M(53) &
    +M(54)-M(55)-M(65)+M(68)+M(77)-M(80)-M(89)+M(92)+M(101)-M(102)-M(103)-M(104)+M(105)+M(106)-M(109)+M(111)+M(115)+M(117)-M(121) &
    -M(123))+c(6)*(-M(197)+M(199)+M(204)-M(210))) * den(41)
  T4sum(1:35,23) = T4sum(1:35,23) + Gcoeff * G3tensor(:,221)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(5)+M(6)-M(7)+M(8)-M(13)+M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42)+M(43)-M(53) &
    +M(54)-M(55)-M(65)+M(68)+M(77)-M(80)-M(89)+M(92)+M(101)+M(102)-M(103)-M(104)-M(105)+M(106)-M(109)-M(111)+M(115)+M(117)+M(121) &
    -M(123))+c(6)*(M(203)-M(209)-M(239)+M(241))) * den(41)
  T4sum(1:35,23) = T4sum(1:35,23) + Gcoeff * G3tensor(:,222)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(37)+M(38)+M(39)-M(40)+M(102)-M(105)-M(111)+M(121))+c(6)*(M(197)-M(199)+M(203)-M(204) &
    -M(209)+M(210)-M(239)+M(241))) * den(41)
  T4sum(1:35,23) = T4sum(1:35,23) + Gcoeff * G3tensor(:,223)
  Gcoeff = (c(6)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(403)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,224)
  Gcoeff = (c(6)*(M(132)-M(135)-M(156)+M(159)-M(197)+M(199)-M(203)+M(204)+M(209)-M(210)+M(219)-M(220)-M(225)+M(226)+M(239) &
    -M(241))) * den(403)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,225)
  Gcoeff = (c(6)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(403)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,226)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(153)-M(168)+M(171)+M(184) &
    +M(187)-M(188)-M(198)+M(231))) * den(49)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,628)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(150)+M(183)+M(193)-M(194) &
    -M(197)-M(210)+M(213)+M(228))) * den(49)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,629)
  Gcoeff = (c(6)*(-M(150)+M(153)+M(168)-M(171)+M(183)-M(184)-M(187)+M(188)+M(193)-M(194)-M(197)+M(198)-M(210)+M(213)+M(228) &
    -M(231))) * den(49)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,630)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(144)+M(147)-M(177)+M(181)-M(182) &
    +M(190)-M(200)+M(237))) * den(70)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,631)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(174)+M(189)+M(191)-M(192)-M(199) &
    -M(204)+M(207)+M(234))) * den(70)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,632)
  Gcoeff = (c(6)*(M(144)-M(147)-M(174)+M(177)-M(181)+M(182)+M(189)-M(190)+M(191)-M(192)-M(199)+M(200)-M(204)+M(207)+M(234) &
    -M(237))) * den(70)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,633)
  Gcoeff = (c(5)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(6)*(M(134)+M(153)-M(171)-M(179) &
    -M(184)+M(188)+M(202)-M(247))) * den(219)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,634)
  Gcoeff = (c(5)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(6)*(M(144)+M(150)-M(181)-M(183) &
    +M(194)+M(200)-M(213)-M(237))) * den(219)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,635)
  Gcoeff = (c(6)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(219)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,636)
  Gcoeff = (c(5)*(-M(41)+M(53)+M(60)+M(63)+M(65)-M(68)-M(69)-M(70)+M(72)+M(75)-M(81)-M(93))+c(6)*(-M(147)+M(158)+M(177)+M(182) &
    -M(185)-M(190)+M(201)-M(245))) * den(214)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,637)
  Gcoeff = (c(5)*(-M(41)+M(53)+M(60)+M(63)+M(65)-M(68)-M(69)-M(70)+M(72)+M(75)-M(81)-M(93))+c(6)*(M(168)+M(174)-M(187)-M(189) &
    +M(192)+M(198)-M(207)-M(231))) * den(214)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,638)
  Gcoeff = (c(6)*(M(147)-M(158)+M(168)+M(174)-M(177)-M(182)+M(185)-M(187)-M(189)+M(190)+M(192)+M(198)-M(201)-M(207)-M(231) &
    +M(245))) * den(214)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,639)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)-M(41)-M(48)-M(51) &
    +M(53)+M(60)-M(63)+M(65)+M(66)+M(67)-M(68)-M(69)+M(70)-M(71)+M(72)+M(73)-M(74)-M(75)-M(76)+M(78)-M(81)-M(88)+M(90)+M(93) &
    +M(100))+c(6)*(-M(144)+M(181)-M(200)+M(237))) * den(40)
  T4sum(1:35,168) = T4sum(1:35,168) + Gcoeff * G3tensor(:,37)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)-M(41)-M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)+M(66)-M(67)-M(68)-M(69)-M(70)-M(71)+M(72)-M(73)+M(74)+M(75)+M(76)+M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(168)-M(187)+M(198)-M(231))) * den(40)
  T4sum(1:35,168) = T4sum(1:35,168) + Gcoeff * G3tensor(:,38)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(144)+M(168)-M(181)-M(187) &
    +M(198)+M(200)-M(231)-M(237))) * den(40)
  T4sum(1:35,168) = T4sum(1:35,168) + Gcoeff * G3tensor(:,39)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(139)-M(167)+M(169)+M(181)+M(230) &
    +M(237)-M(238)-M(240))) * den(52)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,640)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(137)+M(179)-M(209)+M(211)+M(229) &
    -M(239)+M(247)-M(248))) * den(52)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,641)
  Gcoeff = (c(6)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248))) * den(52)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,642)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(31)+M(32)+M(35)-M(36)-M(105)+M(108)-M(111)+M(119))+c(6)*(M(138)-M(141)-M(180)+M(183) &
    +M(213)-M(214)-M(223)+M(224))) * den(103)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,643)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(31)+M(32)+M(35)-M(36)-M(105)+M(108)-M(111)+M(119))+c(6)*(M(173)-M(175)+M(205)-M(206) &
    -M(215)+M(216)-M(233)+M(235))) * den(103)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,644)
  Gcoeff = (c(6)*(-M(138)+M(141)+M(173)-M(175)+M(180)-M(183)+M(205)-M(206)-M(213)+M(214)-M(215)+M(216)+M(223)-M(224)-M(233) &
    +M(235))) * den(103)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,645)
  Gcoeff = (c(5)*(M(44)+M(45)+M(46)-M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95))+c(6)*(M(137)+M(142)-M(171)+M(172) &
    -M(179)-M(184)-M(247)+M(248))) * den(237)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,646)
  Gcoeff = (c(5)*(M(44)+M(45)+M(46)-M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95))+c(6)*(M(139)+M(141)-M(181)-M(183) &
    -M(213)+M(214)-M(237)+M(238))) * den(237)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,647)
  Gcoeff = (c(6)*(-M(137)+M(139)+M(141)-M(142)+M(171)-M(172)+M(179)-M(181)-M(183)+M(184)-M(213)+M(214)-M(237)+M(238)+M(247) &
    -M(248))) * den(237)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,648)
  Gcoeff = (c(5)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(6)*(-M(138)+M(156)-M(162) &
    +M(180)-M(222)+M(223)-M(224)+M(225))) * den(234)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,649)
  Gcoeff = (c(5)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(6)*(-M(205)+M(209)-M(211) &
    +M(215)-M(229)+M(233)-M(235)+M(239))) * den(234)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,650)
  Gcoeff = (c(6)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(234)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,651)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)-M(44)-M(45)-M(46) &
    +M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95)+M(103)-M(105)-M(107)+M(108)+M(109)+M(110)-M(111)-M(112)-M(113)-M(118) &
    +M(119)+M(124))+c(6)*(-M(141)+M(183)+M(213)-M(214))) * den(33)
  T4sum(1:35,174) = T4sum(1:35,174) + Gcoeff * G3tensor(:,40)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)-M(44)-M(45)+M(46) &
    +M(53)-M(56)+M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)-M(110)+M(111)+M(112)-M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(209)-M(211)-M(229)+M(239))) * den(33)
  T4sum(1:35,174) = T4sum(1:35,174) + Gcoeff * G3tensor(:,41)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(141)-M(183)+M(209) &
    -M(211)-M(213)+M(214)-M(229)+M(239))) * den(33)
  T4sum(1:35,174) = T4sum(1:35,174) + Gcoeff * G3tensor(:,42)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(6)*(-M(143)+M(145)-M(163)+M(187)+M(231) &
    -M(232)+M(236)-M(242))) * den(73)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,652)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(6)*(-M(161)+M(185)-M(203)+M(205)+M(235) &
    -M(241)+M(245)-M(246))) * den(73)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,653)
  Gcoeff = (c(6)*(M(143)-M(145)-M(161)+M(163)+M(185)-M(187)-M(203)+M(205)-M(231)+M(232)+M(235)-M(236)-M(241)+M(242)+M(245) &
    -M(246))) * den(73)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,654)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(102)+M(108)+M(119)-M(121))+c(6)*(M(149)-M(151)+M(211)-M(212) &
    -M(217)+M(218)-M(227)+M(229))) * den(106)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,655)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(102)+M(108)+M(119)-M(121))+c(6)*(M(162)-M(165)-M(186)+M(189) &
    +M(207)-M(208)-M(221)+M(222))) * den(106)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,656)
  Gcoeff = (c(6)*(-M(149)+M(151)+M(162)-M(165)-M(186)+M(189)+M(207)-M(208)-M(211)+M(212)+M(217)-M(218)-M(221)+M(222)+M(227) &
    -M(229))) * den(106)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,657)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)+M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96))+c(6)*(-M(147)+M(148)+M(161)+M(166) &
    -M(185)-M(190)-M(245)+M(246))) * den(257)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,658)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)+M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96))+c(6)*(M(163)+M(165)-M(187)-M(189) &
    -M(207)+M(208)-M(231)+M(232))) * den(257)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,659)
  Gcoeff = (c(6)*(M(147)-M(148)-M(161)+M(163)+M(165)-M(166)+M(185)-M(187)-M(189)+M(190)-M(207)+M(208)-M(231)+M(232)+M(245) &
    -M(246))) * den(257)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,660)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)+M(68)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121))+c(6)*(M(132)-M(138)-M(162) &
    +M(186)+M(221)-M(222)-M(224)+M(226))) * den(252)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,661)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)+M(68)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121))+c(6)*(M(203)-M(205)-M(211) &
    +M(217)+M(227)-M(229)-M(235)+M(241))) * den(252)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,662)
  Gcoeff = (c(6)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(252)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,663)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(27)+M(28)-M(29)+M(30)+M(33)-M(34)+M(41)-M(44)-M(56) &
    -M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)+M(101)-M(102)-M(107)+M(108)-M(113)+M(115)+M(119)-M(121)+M(125)-M(126) &
    -M(128)+M(130))+c(6)*(-M(165)+M(189)+M(207)-M(208))) * den(12)
  T4sum(1:35,180) = T4sum(1:35,180) + Gcoeff * G3tensor(:,227)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(27)+M(28)-M(29)+M(30)+M(33)-M(34)+M(41)-M(44)-M(56) &
    -M(57)+M(58)+M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(203)-M(205)-M(235)+M(241))) * den(12)
  T4sum(1:35,180) = T4sum(1:35,180) + Gcoeff * G3tensor(:,228)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(165)-M(189)+M(203) &
    -M(205)-M(207)+M(208)-M(235)+M(241))) * den(12)
  T4sum(1:35,180) = T4sum(1:35,180) + Gcoeff * G3tensor(:,229)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(55)-M(67)-M(79)-M(89)-M(90)-M(91)+M(92)+M(95)+M(98))+c(6)*(M(133)+M(139)-M(169)-M(193) &
    -M(228)-M(230)+M(238)+M(244))) * den(259)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,664)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(55)-M(67)-M(79)-M(89)-M(90)-M(91)+M(92)+M(95)+M(98))+c(6)*(M(137)+M(143)-M(187)-M(211) &
    -M(229)-M(231)+M(242)+M(248))) * den(259)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,665)
  Gcoeff = (c(6)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(259)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,666)
  Gcoeff = (c(5)*(-M(43)+M(55)+M(58)+M(61)-M(70)-M(82)+M(89)-M(92)-M(93)-M(94)+M(96)+M(99))+c(6)*(-M(145)+M(157)+M(163)-M(191) &
    +M(232)-M(234)-M(236)+M(243))) * den(241)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,667)
  Gcoeff = (c(5)*(-M(43)+M(55)+M(58)+M(61)-M(70)-M(82)+M(89)-M(92)-M(93)-M(94)+M(96)+M(99))+c(6)*(M(161)+M(167)-M(181)-M(205) &
    -M(235)-M(237)+M(240)+M(246))) * den(241)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,668)
  Gcoeff = (c(6)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(241)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,669)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(7)-M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(25)+M(26)+M(29)-M(30)-M(43)-M(46)-M(49) &
    +M(55)-M(58)+M(61)+M(67)+M(70)+M(73)+M(79)-M(82)-M(85)+M(89)+M(90)+M(91)-M(92)+M(93)-M(94)-M(95)-M(96)-M(97)-M(98)+M(99) &
    +M(100))+c(6)*(-M(143)+M(187)+M(231)-M(242))) * den(45)
  T4sum(1:35,168) = T4sum(1:35,168) + Gcoeff * G3tensor(:,43)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(7)-M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(25)+M(26)+M(29)-M(30)-M(43)+M(46)-M(49) &
    +M(55)+M(58)+M(61)-M(67)-M(70)-M(73)+M(79)-M(82)+M(85)+M(89)-M(90)+M(91)-M(92)-M(93)-M(94)+M(95)+M(96)+M(97)-M(98)+M(99) &
    -M(100))+c(6)*(M(167)-M(181)-M(237)+M(240))) * den(45)
  T4sum(1:35,168) = T4sum(1:35,168) + Gcoeff * G3tensor(:,44)
  Gcoeff = (c(5)*(M(46)+M(58)-M(67)-M(70)-M(73)+M(85)-M(90)-M(93)+M(95)+M(96)+M(97)-M(100))+c(6)*(M(143)+M(167)-M(181)-M(187) &
    -M(231)-M(237)+M(240)+M(242))) * den(45)
  T4sum(1:35,168) = T4sum(1:35,168) + Gcoeff * G3tensor(:,45)
  Gcoeff = (c(5)*(M(50)+M(51)+M(52)-M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91))+c(6)*(M(150)+M(152)-M(169)+M(170) &
    -M(193)+M(194)-M(228)-M(230))) * den(262)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,670)
  Gcoeff = (c(5)*(M(50)+M(51)+M(52)-M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91))+c(6)*(M(151)+M(153)-M(187)+M(188) &
    -M(211)+M(212)-M(229)-M(231))) * den(262)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,671)
  Gcoeff = (c(6)*(-M(150)+M(151)-M(152)+M(153)+M(169)-M(170)-M(187)+M(188)+M(193)-M(194)-M(211)+M(212)+M(228)-M(229)+M(230) &
    -M(231))) * den(262)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,672)
  Gcoeff = (c(5)*(-M(50)+M(55)-M(62)+M(89)+M(102)+M(104)-M(108)-M(114)-M(119)-M(120)+M(121)+M(123))+c(6)*(-M(149)+M(159)-M(173) &
    -M(216)+M(217)-M(218)+M(219)+M(227))) * den(224)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,673)
  Gcoeff = (c(5)*(-M(50)+M(55)-M(62)+M(89)+M(102)+M(104)-M(108)-M(114)-M(119)-M(120)+M(121)+M(123))+c(6)*(-M(183)+M(186)-M(189) &
    +M(197)-M(207)+M(210)-M(213)+M(221))) * den(224)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,674)
  Gcoeff = (c(6)*(M(149)-M(159)+M(173)-M(183)+M(186)-M(189)+M(197)-M(207)+M(210)-M(213)+M(216)-M(217)+M(218)-M(219)+M(221) &
    -M(227))) * den(224)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,675)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)-M(102)+M(104)+M(108)+M(110)-M(114)-M(116)+M(119)-M(120)-M(121)-M(122) &
    +M(123)+M(124))+c(6)*(-M(151)+M(211)-M(212)+M(229))) * den(37)
  T4sum(1:35,174) = T4sum(1:35,174) + Gcoeff * G3tensor(:,46)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(50)+M(51)-M(52) &
    +M(55)-M(62)-M(67)+M(74)+M(79)-M(86)+M(89)-M(90)+M(91)+M(102)+M(104)-M(108)-M(110)-M(114)+M(116)-M(119)-M(120)+M(121)+M(122) &
    +M(123)-M(124))+c(6)*(-M(183)+M(197)+M(210)-M(213))) * den(37)
  T4sum(1:35,174) = T4sum(1:35,174) + Gcoeff * G3tensor(:,47)
  Gcoeff = (c(5)*(M(51)-M(67)+M(74)-M(90)+M(102)-M(108)-M(110)+M(116)-M(119)+M(121)+M(122)-M(124))+c(6)*(M(151)-M(183)+M(197) &
    +M(210)-M(211)+M(212)-M(213)-M(229))) * den(37)
  T4sum(1:35,174) = T4sum(1:35,174) + Gcoeff * G3tensor(:,48)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)+M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94))+c(6)*(-M(145)+M(146)+M(174)+M(176) &
    -M(191)+M(192)-M(234)-M(236))) * den(245)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,676)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)+M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94))+c(6)*(M(175)+M(177)-M(181)+M(182) &
    -M(205)+M(206)-M(235)-M(237))) * den(245)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,677)
  Gcoeff = (c(6)*(M(145)-M(146)-M(174)+M(175)-M(176)+M(177)-M(181)+M(182)+M(191)-M(192)-M(205)+M(206)+M(234)-M(235)+M(236) &
    -M(237))) * den(245)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,678)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)+M(92)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120))+c(6)*(M(135)-M(149)-M(173) &
    +M(215)-M(216)-M(218)+M(220)+M(233))) * den(227)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,679)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)+M(92)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120))+c(6)*(M(180)-M(183)-M(189) &
    +M(199)+M(204)-M(207)-M(213)+M(223))) * den(227)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,680)
  Gcoeff = (c(6)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(227)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,681)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(27)-M(28)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)-M(105)+M(106)+M(108)-M(111)-M(114)+M(117)+M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(175)+M(205)-M(206)+M(235))) * den(16)
  T4sum(1:35,180) = T4sum(1:35,180) + Gcoeff * G3tensor(:,230)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(27)-M(28)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    +M(63)-M(64)-M(70)+M(75)+M(82)-M(87)+M(92)-M(93)+M(94)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120)-M(125)+M(127) &
    +M(129)-M(130))+c(6)*(-M(189)+M(199)+M(204)-M(207))) * den(16)
  T4sum(1:35,180) = T4sum(1:35,180) + Gcoeff * G3tensor(:,231)
  Gcoeff = (c(5)*(M(63)-M(70)+M(75)-M(93)+M(105)-M(108)+M(111)-M(119)-M(125)+M(127)+M(129)-M(130))+c(6)*(M(175)-M(189)+M(199) &
    +M(204)-M(205)+M(206)-M(207)-M(235))) * den(16)
  T4sum(1:35,180) = T4sum(1:35,180) + Gcoeff * G3tensor(:,232)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(10)-M(12)-M(14)-M(15)-M(17)-M(20)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(38) &
    -M(39)+M(41)+M(43)+M(51)+M(54)+M(63)+M(66)+M(68)+M(69)+M(74)+M(75)+M(76)+M(77)+M(78)+M(79)+M(81)+M(82)+M(88)+M(91)+M(92)+M(94) &
    +M(101)+M(105)+M(106)+M(111)+M(115)+M(116)+M(117)+M(122)+M(127)+M(129))+c(6)*(M(199)+M(204)))
  T5sum(1:70,23) = T5sum(1:70,23) + Gcoeff * G4tensor(:,94)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(10)-M(11)-M(13)-M(15)-M(17)-M(20)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(41)+M(43)+M(46)+M(54)+M(58)+M(66)+M(68)+M(69)+M(77)+M(78)+M(79)+M(81)+M(82)+M(85)+M(91)+M(92)+M(94)+M(95)+M(96)+M(97) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(203)+M(241)))
  T5sum(1:70,23) = T5sum(1:70,23) + Gcoeff * G4tensor(:,95)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)+M(112)-M(116)+M(118)+M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(199)+M(203)-M(204)+M(241)))
  T5sum(1:70,23) = T5sum(1:70,23) + Gcoeff * G4tensor(:,96)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(10)-M(12)-M(13)-M(15)-M(17)-M(19)-M(21)-M(24)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(42)+M(51)+M(53)+M(55)+M(63)+M(65)+M(66)+M(69)+M(74)+M(75)+M(76)+M(78)+M(79)+M(80)+M(81)+M(82)+M(88)+M(89)+M(91)+M(94) &
    +M(102)+M(103)+M(104)+M(109)+M(116)+M(121)+M(122)+M(123)+M(127)+M(129))+c(6)*(M(197)+M(210)))
  T5sum(1:70,24) = T5sum(1:70,24) + Gcoeff * G4tensor(:,16)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(42)+M(46)+M(53)+M(55)+M(58)+M(65)+M(66)+M(69)+M(78)+M(79)+M(80)+M(81)+M(82)+M(85)+M(89)+M(91)+M(94)+M(95)+M(96)+M(97) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(209)+M(239)))
  T5sum(1:70,24) = T5sum(1:70,24) + Gcoeff * G4tensor(:,17)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(197)+M(209)-M(210)+M(239)))
  T5sum(1:70,24) = T5sum(1:70,24) + Gcoeff * G4tensor(:,18)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(150)+M(183)+M(193)-M(194) &
    -M(197)-M(210)+M(213)+M(228))) * den(49)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,226)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(160)+M(174)+M(186)-M(189) &
    +M(192)-M(195)-M(207)+M(221))) * den(49)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,227)
  Gcoeff = (c(6)*(M(150)-M(160)+M(174)-M(183)+M(186)-M(189)+M(192)-M(193)+M(194)-M(195)+M(197)-M(207)+M(210)-M(213)+M(221) &
    -M(228))) * den(49)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,228)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(160)+M(174)+M(186)-M(189) &
    +M(192)-M(195)-M(207)+M(221))) * den(49)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,595)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(153)-M(168)+M(171)+M(184) &
    +M(187)-M(188)-M(198)+M(231))) * den(49)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,596)
  Gcoeff = (c(6)*(-M(153)+M(160)-M(168)+M(171)-M(174)+M(184)-M(186)+M(187)-M(188)+M(189)-M(192)+M(195)-M(198)+M(207)-M(221) &
    +M(231))) * den(49)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,597)
  Gcoeff = (c(5)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(6)*(M(136)+M(147)-M(177)-M(180) &
    -M(182)+M(190)+M(196)-M(223))) * den(219)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,376)
  Gcoeff = (c(5)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(6)*(M(134)+M(153)-M(171)-M(179) &
    -M(184)+M(188)+M(202)-M(247))) * den(219)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,377)
  Gcoeff = (c(6)*(M(134)-M(136)-M(147)+M(153)-M(171)+M(177)-M(179)+M(180)+M(182)-M(184)+M(188)-M(190)-M(196)+M(202)+M(223) &
    -M(247))) * den(219)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,378)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(144)+M(168)-M(181)-M(187) &
    +M(198)+M(200)-M(231)-M(237))) * den(398)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,682)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(158)-M(185)-M(191)+M(199) &
    +M(201)+M(204)-M(234)-M(245))) * den(398)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,683)
  Gcoeff = (c(6)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(398)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,684)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)-M(41)-M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)+M(66)-M(67)-M(68)-M(69)-M(70)-M(71)+M(72)-M(73)+M(74)+M(75)+M(76)+M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(168)-M(187)+M(198)-M(231))) * den(40)
  T4sum(1:35,154) = T4sum(1:35,154) + Gcoeff * G3tensor(:,236)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(41)-M(48)+M(51) &
    -M(53)-M(60)-M(63)-M(65)+M(66)-M(67)+M(68)+M(69)+M(70)-M(71)-M(72)-M(73)+M(74)-M(75)+M(76)+M(78)+M(81)+M(88)-M(90)+M(93) &
    -M(100))+c(6)*(-M(174)+M(189)-M(192)+M(207))) * den(40)
  T4sum(1:35,154) = T4sum(1:35,154) + Gcoeff * G3tensor(:,237)
  Gcoeff = (c(5)*(M(41)-M(53)-M(60)-M(63)-M(65)+M(68)+M(69)+M(70)-M(72)-M(75)+M(81)+M(93))+c(6)*(-M(168)-M(174)+M(187)+M(189) &
    -M(192)-M(198)+M(207)+M(231))) * den(40)
  T4sum(1:35,154) = T4sum(1:35,154) + Gcoeff * G3tensor(:,238)
  Gcoeff = (c(5)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(6)*(M(136)+M(147)-M(177)-M(180) &
    -M(182)+M(190)+M(196)-M(223))) * den(219)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(5)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(6)*(M(144)+M(150)-M(181)-M(183) &
    +M(194)+M(200)-M(213)-M(237))) * den(219)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,14)
  Gcoeff = (c(6)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(219)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,20)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)-M(17)+M(18)+M(19)-M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)-M(48)+M(51) &
    -M(53)-M(60)+M(63)-M(65)+M(66)-M(67)+M(68)+M(69)-M(70)-M(71)-M(72)-M(73)+M(74)+M(75)+M(76)+M(78)+M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(-M(191)+M(199)+M(204)-M(234))) * den(40)
  T4sum(1:35,71) = T4sum(1:35,71) + Gcoeff * G3tensor(:,239)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(41)-M(48)+M(51) &
    -M(53)-M(60)-M(63)-M(65)+M(66)-M(67)+M(68)+M(69)+M(70)-M(71)-M(72)-M(73)+M(74)-M(75)+M(76)+M(78)+M(81)+M(88)-M(90)+M(93) &
    -M(100))+c(6)*(-M(174)+M(189)-M(192)+M(207))) * den(40)
  T4sum(1:35,71) = T4sum(1:35,71) + Gcoeff * G3tensor(:,240)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(174)+M(189)+M(191)-M(192)-M(199) &
    -M(204)+M(207)+M(234))) * den(40)
  T4sum(1:35,71) = T4sum(1:35,71) + Gcoeff * G3tensor(:,241)
  Gcoeff = (c(6)*(M(147)-M(158)+M(168)+M(174)-M(177)-M(182)+M(185)-M(187)-M(189)+M(190)+M(192)+M(198)-M(201)-M(207)-M(231) &
    +M(245))) * den(423)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,248)
  Gcoeff = (c(6)*(-M(144)+M(158)-M(168)+M(181)-M(185)+M(187)-M(191)-M(198)+M(199)-M(200)+M(201)+M(204)+M(231)-M(234)+M(237) &
    -M(245))) * den(423)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,249)
  Gcoeff = (c(6)*(M(144)-M(147)-M(174)+M(177)-M(181)+M(182)+M(189)-M(190)+M(191)-M(192)-M(199)+M(200)-M(204)+M(207)+M(234) &
    -M(237))) * den(423)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,250)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(155)+M(161)-M(205)+M(215)+M(233) &
    -M(235)+M(246)-M(249))) * den(52)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,619)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(137)+M(179)-M(209)+M(211)+M(229) &
    -M(239)+M(247)-M(248))) * den(52)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,620)
  Gcoeff = (c(6)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(52)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,621)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(139)-M(167)+M(169)+M(181)+M(230) &
    +M(237)-M(238)-M(240))) * den(52)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,94)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(155)+M(161)-M(205)+M(215)+M(233) &
    -M(235)+M(246)-M(249))) * den(52)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,95)
  Gcoeff = (c(6)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(52)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,96)
  Gcoeff = (c(5)*(M(44)+M(45)+M(46)-M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95))+c(6)*(M(138)+M(140)-M(177)+M(178) &
    -M(180)-M(182)-M(223)+M(224))) * den(237)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,379)
  Gcoeff = (c(5)*(M(44)+M(45)+M(46)-M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95))+c(6)*(M(137)+M(142)-M(171)+M(172) &
    -M(179)-M(184)-M(247)+M(248))) * den(237)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,380)
  Gcoeff = (c(6)*(M(137)-M(138)-M(140)+M(142)-M(171)+M(172)+M(177)-M(178)-M(179)+M(180)+M(182)-M(184)+M(223)-M(224)-M(247) &
    +M(248))) * den(237)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,381)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(141)-M(183)+M(209) &
    -M(211)-M(213)+M(214)-M(229)+M(239))) * den(371)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,685)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(156)-M(162)-M(173) &
    +M(175)+M(206)-M(216)-M(222)+M(225))) * den(371)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,686)
  Gcoeff = (c(6)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(371)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,687)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)-M(44)-M(45)+M(46) &
    +M(53)-M(56)+M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)-M(110)+M(111)+M(112)-M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(209)-M(211)-M(229)+M(239))) * den(33)
  T4sum(1:35,163) = T4sum(1:35,163) + Gcoeff * G3tensor(:,251)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)+M(44)-M(45)+M(46) &
    -M(53)+M(56)-M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)-M(103)-M(105)+M(107)+M(108)-M(109)-M(110)-M(111)+M(112)+M(113)+M(118) &
    +M(119)-M(124))+c(6)*(M(205)-M(215)-M(233)+M(235))) * den(33)
  T4sum(1:35,163) = T4sum(1:35,163) + Gcoeff * G3tensor(:,252)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)-M(65)-M(103)-M(105)+M(107)+M(108)-M(109)-M(111)+M(113)+M(119))+c(6)*(M(205)-M(209)+M(211) &
    -M(215)+M(229)-M(233)+M(235)-M(239))) * den(33)
  T4sum(1:35,163) = T4sum(1:35,163) + Gcoeff * G3tensor(:,253)
  Gcoeff = (c(5)*(M(44)+M(45)+M(46)-M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95))+c(6)*(M(138)+M(140)-M(177)+M(178) &
    -M(180)-M(182)-M(223)+M(224))) * den(237)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(5)*(M(44)+M(45)+M(46)-M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95))+c(6)*(M(139)+M(141)-M(181)-M(183) &
    -M(213)+M(214)-M(237)+M(238))) * den(237)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,15)
  Gcoeff = (c(6)*(-M(138)+M(139)-M(140)+M(141)+M(177)-M(178)+M(180)-M(181)+M(182)-M(183)-M(213)+M(214)+M(223)-M(224)-M(237) &
    +M(238))) * den(237)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,21)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)+M(31)-M(32)-M(35)+M(36)+M(44)-M(45)+M(46) &
    -M(53)+M(56)-M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)-M(103)+M(105)+M(107)-M(108)-M(109)-M(110)+M(111)+M(112)+M(113)+M(118) &
    -M(119)-M(124))+c(6)*(-M(173)+M(175)+M(206)-M(216))) * den(33)
  T4sum(1:35,35) = T4sum(1:35,35) + Gcoeff * G3tensor(:,254)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)+M(44)-M(45)+M(46) &
    -M(53)+M(56)-M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)-M(103)-M(105)+M(107)+M(108)-M(109)-M(110)-M(111)+M(112)+M(113)+M(118) &
    +M(119)-M(124))+c(6)*(M(205)-M(215)-M(233)+M(235))) * den(33)
  T4sum(1:35,35) = T4sum(1:35,35) + Gcoeff * G3tensor(:,255)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(31)+M(32)+M(35)-M(36)-M(105)+M(108)-M(111)+M(119))+c(6)*(M(173)-M(175)+M(205)-M(206) &
    -M(215)+M(216)-M(233)+M(235))) * den(33)
  T4sum(1:35,35) = T4sum(1:35,35) + Gcoeff * G3tensor(:,256)
  Gcoeff = (c(6)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(432)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,263)
  Gcoeff = (c(6)*(-M(141)+M(156)-M(162)-M(173)+M(175)+M(183)+M(206)-M(209)+M(211)+M(213)-M(214)-M(216)-M(222)+M(225)+M(229) &
    -M(239))) * den(432)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,264)
  Gcoeff = (c(6)*(-M(138)+M(141)+M(173)-M(175)+M(180)-M(183)+M(205)-M(206)-M(213)+M(214)-M(215)+M(216)+M(223)-M(224)-M(233) &
    +M(235))) * den(432)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,265)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(55)-M(67)-M(79)-M(89)-M(90)-M(91)+M(92)+M(95)+M(98))+c(6)*(M(133)+M(139)-M(169)-M(193) &
    -M(228)-M(230)+M(238)+M(244))) * den(259)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,688)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(55)-M(67)-M(79)-M(89)-M(90)-M(91)+M(92)+M(95)+M(98))+c(6)*(M(131)+M(145)-M(163)-M(217) &
    -M(227)-M(232)+M(236)+M(250))) * den(259)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,689)
  Gcoeff = (c(6)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(259)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,690)
  Gcoeff = (c(5)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(6)*(-M(143)-M(167)+M(181)+M(187) &
    +M(231)+M(237)-M(240)-M(242))) * den(204)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,691)
  Gcoeff = (c(5)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(6)*(-M(157)+M(185)+M(191)-M(203) &
    +M(234)-M(241)-M(243)+M(245))) * den(204)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,692)
  Gcoeff = (c(6)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(204)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,693)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(7)+M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)-M(43)-M(46)-M(49)+M(55) &
    +M(58)+M(61)+M(67)-M(70)+M(73)+M(79)-M(82)-M(85)+M(89)+M(90)+M(91)-M(92)-M(93)-M(94)-M(95)+M(96)-M(97)-M(98)+M(99)+M(100)) &
    +c(6)*(-M(145)+M(163)+M(232)-M(236))) * den(45)
  T4sum(1:35,150) = T4sum(1:35,150) + Gcoeff * G3tensor(:,130)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(7)+M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)+M(43)-M(46)-M(49)-M(55) &
    -M(58)-M(61)+M(67)+M(70)+M(73)+M(79)+M(82)-M(85)-M(89)+M(90)+M(91)+M(92)+M(93)+M(94)-M(95)-M(96)-M(97)-M(98)-M(99)+M(100)) &
    +c(6)*(-M(157)+M(191)+M(234)-M(243))) * den(45)
  T4sum(1:35,150) = T4sum(1:35,150) + Gcoeff * G3tensor(:,131)
  Gcoeff = (c(5)*(M(43)-M(55)-M(58)-M(61)+M(70)+M(82)-M(89)+M(92)+M(93)+M(94)-M(96)-M(99))+c(6)*(M(145)-M(157)-M(163)+M(191) &
    -M(232)+M(234)+M(236)-M(243))) * den(45)
  T4sum(1:35,150) = T4sum(1:35,150) + Gcoeff * G3tensor(:,132)
  Gcoeff = (c(5)*(M(50)+M(51)+M(52)-M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91))+c(6)*(M(150)+M(152)-M(169)+M(170) &
    -M(193)+M(194)-M(228)-M(230))) * den(262)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,694)
  Gcoeff = (c(5)*(M(50)+M(51)+M(52)-M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91))+c(6)*(M(149)+M(154)-M(163)+M(164) &
    -M(217)+M(218)-M(227)-M(232))) * den(262)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,695)
  Gcoeff = (c(6)*(M(149)-M(150)-M(152)+M(154)-M(163)+M(164)+M(169)-M(170)+M(193)-M(194)-M(217)+M(218)-M(227)+M(228)+M(230) &
    -M(232))) * den(262)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,696)
  Gcoeff = (c(5)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(6)*(-M(151)+M(183)-M(197) &
    -M(210)+M(211)-M(212)+M(213)+M(229))) * den(181)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,697)
  Gcoeff = (c(5)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(6)*(-M(159)+M(162)-M(165) &
    +M(173)-M(208)+M(216)-M(219)+M(222))) * den(181)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,698)
  Gcoeff = (c(6)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(181)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,699)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)+M(17)-M(18)-M(19)+M(20)-M(21)+M(22)-M(23)+M(24)-M(25)+M(26)+M(29)-M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)+M(102)+M(104)-M(108)+M(110)-M(114)-M(116)-M(119)-M(120)+M(121)-M(122) &
    +M(123)+M(124))+c(6)*(-M(149)+M(217)-M(218)+M(227))) * den(37)
  T4sum(1:35,159) = T4sum(1:35,159) + Gcoeff * G3tensor(:,133)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)+M(17)-M(18)-M(19)+M(20)-M(21)+M(22)-M(23)+M(24)-M(25)+M(26)+M(29)-M(30)+M(50)-M(51)-M(52) &
    -M(55)+M(62)+M(67)-M(74)+M(79)-M(86)-M(89)+M(90)+M(91)-M(102)-M(104)+M(108)+M(110)+M(114)-M(116)+M(119)+M(120)-M(121)-M(122) &
    -M(123)+M(124))+c(6)*(-M(159)+M(173)+M(216)-M(219))) * den(37)
  T4sum(1:35,159) = T4sum(1:35,159) + Gcoeff * G3tensor(:,134)
  Gcoeff = (c(5)*(M(50)-M(55)+M(62)-M(89)-M(102)-M(104)+M(108)+M(114)+M(119)+M(120)-M(121)-M(123))+c(6)*(M(149)-M(159)+M(173) &
    +M(216)-M(217)+M(218)-M(219)-M(227))) * den(37)
  T4sum(1:35,159) = T4sum(1:35,159) + Gcoeff * G3tensor(:,135)
  Gcoeff = (c(5)*(-M(46)+M(51)-M(58)+M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97))+c(6)*(-M(143)+M(144)-M(167)+M(168) &
    +M(198)+M(200)-M(240)-M(242))) * den(208)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,700)
  Gcoeff = (c(5)*(-M(46)+M(51)-M(58)+M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97))+c(6)*(-M(157)+M(158)+M(199)+M(201) &
    -M(203)+M(204)-M(241)-M(243))) * den(208)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,701)
  Gcoeff = (c(6)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(208)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,702)
  Gcoeff = (c(5)*(M(46)-M(51)-M(74)+M(95)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122))+c(6)*(M(141)-M(151)-M(197) &
    +M(209)-M(210)-M(212)+M(214)+M(239))) * den(184)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,703)
  Gcoeff = (c(5)*(M(46)-M(51)-M(74)+M(95)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122))+c(6)*(M(156)-M(159)-M(165) &
    +M(175)+M(206)-M(208)-M(219)+M(225))) * den(184)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,704)
  Gcoeff = (c(6)*(-M(141)+M(151)+M(156)-M(159)-M(165)+M(175)+M(197)+M(206)-M(208)-M(209)+M(210)+M(212)-M(214)-M(219)+M(225) &
    -M(239))) * den(184)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,705)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)+M(112)-M(116)+M(118)+M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(199)+M(203)-M(204)+M(241))) * den(22)
  T4sum(1:35,180) = T4sum(1:35,180) + Gcoeff * G3tensor(:,233)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(58) &
    +M(63)-M(74)+M(75)-M(76)+M(85)-M(88)+M(95)-M(96)+M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)-M(126)+M(127) &
    -M(128)+M(129))+c(6)*(-M(165)+M(175)+M(206)-M(208))) * den(22)
  T4sum(1:35,180) = T4sum(1:35,180) + Gcoeff * G3tensor(:,234)
  Gcoeff = (c(5)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(165)+M(175)+M(199) &
    -M(203)+M(204)+M(206)-M(208)-M(241))) * den(22)
  T4sum(1:35,180) = T4sum(1:35,180) + Gcoeff * G3tensor(:,235)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(6)-M(8)-M(9)-M(11)-M(13)-M(16)-M(18)-M(19)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(37) &
    -M(40)+M(44)+M(46)+M(50)+M(54)+M(56)+M(57)+M(62)+M(63)+M(64)+M(66)+M(75)+M(77)+M(78)+M(79)+M(84)+M(85)+M(87)+M(91)+M(95)+M(97) &
    +M(105)+M(107)+M(111)+M(112)+M(113)+M(114)+M(118)+M(120)+M(127)+M(129))+c(6)*(M(175)+M(206)))
  T5sum(1:70,47) = T5sum(1:70,47) + Gcoeff * G4tensor(:,97)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(5)-M(8)-M(10)-M(11)-M(13)-M(15)-M(18)-M(20)-M(22)-M(23)-M(25)-M(28)-M(30)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(43)+M(44)+M(46)+M(54)+M(56)+M(57)+M(66)+M(70)+M(77)+M(78)+M(79)+M(82)+M(84)+M(85)+M(91)+M(92)+M(93)+M(94)+M(95)+M(97) &
    +M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(205)+M(235)))
  T5sum(1:70,47) = T5sum(1:70,47) + Gcoeff * G4tensor(:,98)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(27)-M(28)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)-M(105)+M(106)+M(108)-M(111)-M(114)+M(117)+M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(175)+M(205)-M(206)+M(235)))
  T5sum(1:70,47) = T5sum(1:70,47) + Gcoeff * G4tensor(:,99)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(6)-M(7)-M(10)-M(12)-M(13)-M(15)-M(18)-M(19)-M(21)-M(23)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(45)+M(50)+M(53)+M(54)+M(57)+M(62)+M(63)+M(64)+M(65)+M(67)+M(75)+M(77)+M(79)+M(83)+M(84)+M(85)+M(87)+M(90)+M(91)+M(97) &
    +M(103)+M(108)+M(109)+M(110)+M(114)+M(119)+M(120)+M(124)+M(127)+M(129))+c(6)*(M(173)+M(216)))
  T5sum(1:70,48) = T5sum(1:70,48) + Gcoeff * G4tensor(:,53)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(43)+M(45)+M(53)+M(54)+M(57)+M(65)+M(67)+M(70)+M(77)+M(79)+M(82)+M(83)+M(84)+M(85)+M(90)+M(91)+M(92)+M(93)+M(94)+M(97) &
    +M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(215)+M(233)))
  T5sum(1:70,48) = T5sum(1:70,48) + Gcoeff * G4tensor(:,54)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(27)-M(28)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(173)+M(215)-M(216)+M(233)))
  T5sum(1:70,48) = T5sum(1:70,48) + Gcoeff * G4tensor(:,55)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(55)-M(67)-M(79)-M(89)-M(90)-M(91)+M(92)+M(95)+M(98))+c(6)*(M(137)+M(143)-M(187)-M(211) &
    -M(229)-M(231)+M(242)+M(248))) * den(259)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,706)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(55)-M(67)-M(79)-M(89)-M(90)-M(91)+M(92)+M(95)+M(98))+c(6)*(M(131)+M(145)-M(163)-M(217) &
    -M(227)-M(232)+M(236)+M(250))) * den(259)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,707)
  Gcoeff = (c(6)*(M(131)-M(137)-M(143)+M(145)-M(163)+M(187)+M(211)-M(217)-M(227)+M(229)+M(231)-M(232)+M(236)-M(242)-M(248) &
    +M(250))) * den(259)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,708)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(7)+M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)-M(43)-M(46)-M(49)+M(55) &
    +M(58)+M(61)+M(67)-M(70)+M(73)+M(79)-M(82)-M(85)+M(89)+M(90)+M(91)-M(92)-M(93)-M(94)-M(95)+M(96)-M(97)-M(98)+M(99)+M(100)) &
    +c(6)*(-M(145)+M(163)+M(232)-M(236))) * den(45)
  T4sum(1:35,76) = T4sum(1:35,76) + Gcoeff * G3tensor(:,14)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(7)-M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(25)+M(26)+M(29)-M(30)-M(43)-M(46)-M(49) &
    +M(55)-M(58)+M(61)+M(67)+M(70)+M(73)+M(79)-M(82)-M(85)+M(89)+M(90)+M(91)-M(92)+M(93)-M(94)-M(95)-M(96)-M(97)-M(98)+M(99) &
    +M(100))+c(6)*(-M(143)+M(187)+M(231)-M(242))) * den(45)
  T4sum(1:35,76) = T4sum(1:35,76) + Gcoeff * G3tensor(:,17)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(6)*(-M(143)+M(145)-M(163)+M(187)+M(231) &
    -M(232)+M(236)-M(242))) * den(45)
  T4sum(1:35,76) = T4sum(1:35,76) + Gcoeff * G3tensor(:,20)
  Gcoeff = (c(6)*(-M(143)+M(145)+M(161)-M(163)-M(185)+M(187)+M(203)-M(205)+M(231)-M(232)-M(235)+M(236)+M(241)-M(242)-M(245) &
    +M(246))) * den(452)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,267)
  Gcoeff = (c(6)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(452)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,268)
  Gcoeff = (c(6)*(-M(145)+M(157)-M(161)+M(163)-M(167)+M(181)-M(191)+M(205)+M(232)-M(234)+M(235)-M(236)+M(237)-M(240)+M(243) &
    -M(246))) * den(452)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,269)
  Gcoeff = (c(5)*(M(50)+M(51)+M(52)-M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91))+c(6)*(M(151)+M(153)-M(187)+M(188) &
    -M(211)+M(212)-M(229)-M(231))) * den(262)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,709)
  Gcoeff = (c(5)*(M(50)+M(51)+M(52)-M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91))+c(6)*(M(149)+M(154)-M(163)+M(164) &
    -M(217)+M(218)-M(227)-M(232))) * den(262)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,710)
  Gcoeff = (c(6)*(M(149)-M(151)-M(153)+M(154)-M(163)+M(164)+M(187)-M(188)+M(211)-M(212)-M(217)+M(218)-M(227)+M(229)+M(231) &
    -M(232))) * den(262)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,711)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)+M(17)-M(18)-M(19)+M(20)-M(21)+M(22)-M(23)+M(24)-M(25)+M(26)+M(29)-M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)+M(102)+M(104)-M(108)+M(110)-M(114)-M(116)-M(119)-M(120)+M(121)-M(122) &
    +M(123)+M(124))+c(6)*(-M(149)+M(217)-M(218)+M(227))) * den(37)
  T4sum(1:35,40) = T4sum(1:35,40) + Gcoeff * G3tensor(:,15)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)-M(102)+M(104)+M(108)+M(110)-M(114)-M(116)+M(119)-M(120)-M(121)-M(122) &
    +M(123)+M(124))+c(6)*(-M(151)+M(211)-M(212)+M(229))) * den(37)
  T4sum(1:35,40) = T4sum(1:35,40) + Gcoeff * G3tensor(:,18)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(102)+M(108)+M(119)-M(121))+c(6)*(M(149)-M(151)+M(211)-M(212) &
    -M(217)+M(218)-M(227)+M(229))) * den(37)
  T4sum(1:35,40) = T4sum(1:35,40) + Gcoeff * G3tensor(:,21)
  Gcoeff = (c(6)*(M(149)-M(151)-M(162)+M(165)+M(186)-M(189)-M(207)+M(208)+M(211)-M(212)-M(217)+M(218)+M(221)-M(222)-M(227) &
    +M(229))) * den(443)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,271)
  Gcoeff = (c(6)*(M(151)-M(159)+M(162)-M(165)+M(173)-M(183)+M(197)-M(208)+M(210)-M(211)+M(212)-M(213)+M(216)-M(219)+M(222) &
    -M(229))) * den(443)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,272)
  Gcoeff = (c(6)*(-M(149)+M(159)-M(173)+M(183)-M(186)+M(189)-M(197)+M(207)-M(210)+M(213)-M(216)+M(217)-M(218)+M(219)-M(221) &
    +M(227))) * den(443)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,273)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(5)-M(8)-M(10)-M(11)-M(13)-M(15)-M(18)-M(20)-M(22)-M(23)-M(25)-M(28)-M(30)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(43)+M(44)+M(46)+M(54)+M(56)+M(57)+M(66)+M(70)+M(77)+M(78)+M(79)+M(82)+M(84)+M(85)+M(91)+M(92)+M(93)+M(94)+M(95)+M(97) &
    +M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(205)+M(235)))
  T5sum(1:70,67) = T5sum(1:70,67) + Gcoeff * G4tensor(:,100)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(10)-M(11)-M(13)-M(15)-M(17)-M(20)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(41)+M(43)+M(46)+M(54)+M(58)+M(66)+M(68)+M(69)+M(77)+M(78)+M(79)+M(81)+M(82)+M(85)+M(91)+M(92)+M(94)+M(95)+M(96)+M(97) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(203)+M(241)))
  T5sum(1:70,67) = T5sum(1:70,67) + Gcoeff * G4tensor(:,101)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(27)+M(28)-M(29)+M(30)+M(33)-M(34)+M(41)-M(44)-M(56) &
    -M(57)+M(58)+M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(203)-M(205)-M(235)+M(241)))
  T5sum(1:70,67) = T5sum(1:70,67) + Gcoeff * G4tensor(:,102)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(9)-M(12)-M(14)-M(16)-M(18)-M(19)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(45)+M(55)+M(57)+M(67)+M(68)+M(70)+M(79)+M(80)+M(82)+M(83)+M(84)+M(85)+M(89)+M(90)+M(91)+M(93)+M(94)+M(97) &
    +M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(217)+M(227)))
  T5sum(1:70,68) = T5sum(1:70,68) + Gcoeff * G4tensor(:,7)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(6)-M(7)-M(9)-M(12)-M(14)-M(16)-M(17)-M(19)-M(21)-M(24)-M(26)-M(27)-M(29)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(44)+M(45)+M(55)+M(56)+M(58)+M(67)+M(69)+M(79)+M(80)+M(81)+M(82)+M(83)+M(85)+M(89)+M(90)+M(91)+M(94)+M(96)+M(97) &
    +M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(211)+M(229)))
  T5sum(1:70,68) = T5sum(1:70,68) + Gcoeff * G4tensor(:,8)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(27)+M(28)-M(29)+M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    -M(57)+M(58)-M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(211)-M(217)-M(227)+M(229)))
  T5sum(1:70,68) = T5sum(1:70,68) + Gcoeff * G4tensor(:,9)
  Gcoeff = (c(6)*(-M(143)+M(145)+M(147)-M(148)+M(165)-M(166)-M(189)+M(190)+M(203)-M(205)-M(207)+M(208)-M(235)+M(236)+M(241) &
    -M(242))) * den(454)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,274)
  Gcoeff = (c(6)*(M(143)-M(144)-M(146)+M(148)-M(165)+M(166)+M(175)-M(176)+M(199)-M(200)-M(203)+M(204)+M(206)-M(208)-M(241) &
    +M(242))) * den(454)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,275)
  Gcoeff = (c(6)*(M(144)-M(145)+M(146)-M(147)-M(175)+M(176)+M(189)-M(190)-M(199)+M(200)-M(204)+M(205)-M(206)+M(207)+M(235) &
    -M(236))) * den(454)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,276)
  Gcoeff = (c(6)*(-M(132)+M(138)+M(149)-M(151)+M(165)-M(189)+M(203)-M(205)-M(207)+M(208)-M(212)+M(218)+M(224)-M(226)-M(235) &
    +M(241))) * den(447)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,277)
  Gcoeff = (c(6)*(M(132)-M(135)-M(141)+M(151)-M(165)+M(175)+M(199)-M(203)+M(204)+M(206)-M(208)+M(212)-M(214)-M(220)+M(226) &
    -M(241))) * den(447)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,278)
  Gcoeff = (c(6)*(M(135)-M(138)+M(141)-M(149)-M(175)+M(189)-M(199)-M(204)+M(205)-M(206)+M(207)+M(214)-M(218)+M(220)-M(224) &
    +M(235))) * den(447)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,279)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(165)-M(189)+M(203) &
    -M(205)-M(207)+M(208)-M(235)+M(241))) * den(28)
  T4sum(1:70,180) = T4sum(1:70,180) + Gcoeff * G4tensor(:,103)
  Gcoeff = (c(5)*(-M(58)+M(63)+M(75)-M(96)-M(102)+M(105)+M(111)-M(121)-M(126)+M(127)-M(128)+M(129))+c(6)*(-M(165)+M(175)+M(199) &
    -M(203)+M(204)+M(206)-M(208)-M(241))) * den(28)
  T4sum(1:70,180) = T4sum(1:70,180) + Gcoeff * G4tensor(:,104)
  Gcoeff = (c(5)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(6)*(-M(175)+M(189)-M(199) &
    -M(204)+M(205)-M(206)+M(207)+M(235))) * den(28)
  T4sum(1:70,180) = T4sum(1:70,180) + Gcoeff * G4tensor(:,105)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(37)-M(38)-M(39)+M(40)+M(58)-M(63)-M(75)+M(96))+c(6)*(M(165)+M(167)-M(168)-M(175) &
    -M(198)-M(206)+M(208)+M(240))) * den(61)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,481)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(37)-M(38)-M(39)+M(40)+M(58)-M(63)-M(75)+M(96))+c(6)*(-M(142)+M(152)+M(156)-M(159) &
    +M(170)-M(172)-M(219)+M(225))) * den(61)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,482)
  Gcoeff = (c(6)*(-M(142)+M(152)+M(156)-M(159)-M(165)-M(167)+M(168)+M(170)-M(172)+M(175)+M(198)+M(206)-M(208)-M(219)+M(225) &
    -M(240))) * den(61)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,483)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(37)-M(38)-M(39)+M(40)+M(58)-M(63)-M(75)+M(96))+c(6)*(-M(142)+M(152)+M(156)-M(159) &
    +M(170)-M(172)-M(219)+M(225))) * den(61)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,568)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(37)-M(38)-M(39)+M(40)+M(58)-M(63)-M(75)+M(96))+c(6)*(-M(146)+M(148)+M(157)-M(158) &
    +M(166)-M(176)-M(201)+M(243))) * den(61)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,569)
  Gcoeff = (c(6)*(M(142)-M(146)+M(148)-M(152)-M(156)+M(157)-M(158)+M(159)+M(166)-M(170)+M(172)-M(176)-M(201)+M(219)-M(225) &
    +M(243))) * den(61)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,570)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(154)-M(160)+M(162)+M(164) &
    -M(171)-M(184)-M(195)+M(222))) * den(171)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,382)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(148)-M(158)+M(161)+M(166) &
    -M(177)-M(182)-M(201)+M(246))) * den(171)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,383)
  Gcoeff = (c(6)*(M(148)-M(154)-M(158)+M(160)+M(161)-M(162)-M(164)+M(166)+M(171)-M(177)-M(182)+M(184)+M(195)-M(201)-M(222) &
    +M(246))) * den(171)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,384)
  Gcoeff = (c(5)*(M(50)-M(55)-M(58)-M(61)+M(62)+M(63)+M(64)+M(75)+M(87)-M(89)-M(96)-M(99))+c(6)*(M(146)-M(157)-M(163)+M(174) &
    +M(176)+M(192)-M(232)-M(243))) * den(369)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,712)
  Gcoeff = (c(5)*(M(50)-M(55)-M(58)-M(61)+M(62)+M(63)+M(64)+M(75)+M(87)-M(89)-M(96)-M(99))+c(6)*(M(140)-M(155)-M(169)+M(173) &
    +M(178)+M(216)-M(230)-M(249))) * den(369)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,713)
  Gcoeff = (c(6)*(M(140)-M(146)-M(155)+M(157)+M(163)-M(169)+M(173)-M(174)-M(176)+M(178)-M(192)+M(216)-M(230)+M(232)+M(243) &
    -M(249))) * den(369)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,714)
  Gcoeff = (c(4)*(-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)+M(37)-M(38)-M(39)+M(40)+M(44)+M(47) &
    +M(50)-M(53)-M(54)-M(55)+M(56)-M(57)+M(58)+M(59)+M(60)+M(61)+M(62)-M(63)-M(64)-M(65)+M(72)-M(75)-M(77)-M(84)-M(87)-M(89)+M(96) &
    +M(99))+c(6)*(M(152)-M(159)+M(170)-M(219))) * den(32)
  T4sum(1:35,145) = T4sum(1:35,145) + Gcoeff * G3tensor(:,118)
  Gcoeff = (c(4)*(-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)+M(37)-M(38)-M(39)+M(40)-M(44)-M(47) &
    -M(50)+M(53)+M(54)+M(55)-M(56)-M(57)+M(58)-M(59)+M(60)+M(61)-M(62)-M(63)-M(64)+M(65)+M(72)-M(75)+M(77)-M(84)-M(87)+M(89)+M(96) &
    +M(99))+c(6)*(-M(146)+M(157)-M(176)+M(243))) * den(32)
  T4sum(1:35,145) = T4sum(1:35,145) + Gcoeff * G3tensor(:,119)
  Gcoeff = (c(5)*(-M(44)-M(47)-M(50)+M(53)+M(54)+M(55)-M(56)-M(59)-M(62)+M(65)+M(77)+M(89))+c(6)*(-M(146)-M(152)+M(157)+M(159) &
    -M(170)-M(176)+M(219)+M(243))) * den(32)
  T4sum(1:35,145) = T4sum(1:35,145) + Gcoeff * G3tensor(:,120)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(154)-M(160)+M(162)+M(164) &
    -M(171)-M(184)-M(195)+M(222))) * den(171)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,46)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(163)+M(165)-M(168)-M(174) &
    -M(192)-M(198)+M(208)+M(232))) * den(171)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,52)
  Gcoeff = (c(6)*(-M(154)+M(160)-M(162)+M(163)-M(164)+M(165)-M(168)+M(171)-M(174)+M(184)-M(192)+M(195)-M(198)+M(208)-M(222) &
    +M(232))) * den(171)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,58)
  Gcoeff = (c(4)*(-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)+M(37)-M(38)-M(39)+M(40)+M(44)+M(47) &
    +M(50)-M(53)-M(54)-M(55)+M(56)-M(57)+M(58)+M(59)+M(60)+M(61)+M(62)-M(63)-M(64)-M(65)+M(72)-M(75)-M(77)-M(84)-M(87)-M(89)+M(96) &
    +M(99))+c(6)*(M(152)-M(159)+M(170)-M(219))) * den(32)
  T4sum(1:35,119) = T4sum(1:35,119) + Gcoeff * G3tensor(:,106)
  Gcoeff = (c(4)*(-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)+M(37)-M(38)-M(39)+M(40)+M(44)+M(47) &
    -M(50)-M(53)-M(54)+M(55)+M(56)-M(57)+M(58)+M(59)+M(60)+M(61)-M(62)-M(63)-M(64)-M(65)+M(72)-M(75)-M(77)-M(84)-M(87)+M(89)+M(96) &
    +M(99))+c(6)*(M(169)-M(173)-M(216)+M(230))) * den(32)
  T4sum(1:35,119) = T4sum(1:35,119) + Gcoeff * G3tensor(:,107)
  Gcoeff = (c(5)*(-M(11)+M(12)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(50)+M(55)-M(62)+M(89))+c(6)*(-M(152)+M(159)+M(169)-M(170) &
    -M(173)-M(216)+M(219)+M(230))) * den(32)
  T4sum(1:35,119) = T4sum(1:35,119) + Gcoeff * G3tensor(:,108)
  Gcoeff = (c(6)*(-M(140)+M(146)+M(152)-M(154)+M(155)-M(157)-M(159)+M(160)-M(164)+M(170)+M(176)-M(178)+M(195)-M(219)-M(243) &
    +M(249))) * den(477)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,280)
  Gcoeff = (c(6)*(M(140)-M(146)-M(155)+M(157)+M(163)-M(169)+M(173)-M(174)-M(176)+M(178)-M(192)+M(216)-M(230)+M(232)+M(243) &
    -M(249))) * den(477)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,281)
  Gcoeff = (c(6)*(-M(152)+M(154)+M(159)-M(160)-M(163)+M(164)+M(169)-M(170)-M(173)+M(174)+M(192)-M(195)-M(216)+M(219)+M(230) &
    -M(232))) * den(477)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,282)
  Gcoeff = (c(5)*(M(50)-M(55)-M(58)-M(61)+M(62)+M(63)+M(64)+M(75)+M(87)-M(89)-M(96)-M(99))+c(6)*(-M(161)-M(167)+M(175)+M(177) &
    +M(182)+M(206)-M(240)-M(246))) * den(369)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,715)
  Gcoeff = (c(5)*(M(50)-M(55)-M(58)-M(61)+M(62)+M(63)+M(64)+M(75)+M(87)-M(89)-M(96)-M(99))+c(6)*(M(140)-M(155)-M(169)+M(173) &
    +M(178)+M(216)-M(230)-M(249))) * den(369)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,716)
  Gcoeff = (c(6)*(M(140)-M(155)+M(161)+M(167)-M(169)+M(173)-M(175)-M(177)+M(178)-M(182)-M(206)+M(216)-M(230)+M(240)+M(246) &
    -M(249))) * den(369)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,717)
  Gcoeff = (c(4)*(-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)+M(37)-M(38)-M(39)+M(40)+M(44)+M(47) &
    -M(50)-M(53)-M(54)+M(55)+M(56)-M(57)+M(58)+M(59)+M(60)+M(61)-M(62)-M(63)-M(64)-M(65)+M(72)-M(75)-M(77)-M(84)-M(87)+M(89)+M(96) &
    +M(99))+c(6)*(M(169)-M(173)-M(216)+M(230))) * den(32)
  T4sum(1:35,126) = T4sum(1:35,126) + Gcoeff * G3tensor(:,4)
  Gcoeff = (c(4)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(17)+M(18)-M(21)+M(22)+M(23)-M(24)+M(37)-M(38)-M(39)+M(40)-M(44)+M(47)-M(50) &
    +M(53)-M(54)+M(55)-M(56)-M(57)+M(58)+M(59)+M(60)+M(61)-M(62)-M(63)-M(64)+M(65)+M(72)-M(75)-M(77)-M(84)-M(87)+M(89)+M(96) &
    +M(99))+c(6)*(M(167)-M(175)-M(206)+M(240))) * den(32)
  T4sum(1:35,126) = T4sum(1:35,126) + Gcoeff * G3tensor(:,7)
  Gcoeff = (c(5)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(44)+M(53)-M(56)+M(65))+c(6)*(M(167)-M(169)+M(173)-M(175)-M(206) &
    +M(216)-M(230)+M(240))) * den(32)
  T4sum(1:35,126) = T4sum(1:35,126) + Gcoeff * G3tensor(:,10)
  Gcoeff = (c(6)*(M(140)-M(142)-M(148)+M(154)-M(155)+M(156)+M(158)-M(160)+M(164)-M(166)-M(172)+M(178)-M(195)+M(201)+M(225) &
    -M(249))) * den(488)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,61)
  Gcoeff = (c(6)*(M(148)-M(154)-M(158)+M(160)+M(161)-M(162)-M(164)+M(166)+M(171)-M(177)-M(182)+M(184)+M(195)-M(201)-M(222) &
    +M(246))) * den(488)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,62)
  Gcoeff = (c(6)*(-M(140)+M(142)+M(155)-M(156)-M(161)+M(162)-M(171)+M(172)+M(177)-M(178)+M(182)-M(184)+M(222)-M(225)-M(246) &
    +M(249))) * den(488)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,63)
  Gcoeff = (c(6)*(M(142)-M(156)+M(162)+M(167)-M(169)-M(171)+M(172)+M(173)-M(175)-M(184)-M(206)+M(216)+M(222)-M(225)-M(230) &
    +M(240))) * den(66)
  T3sum(1:35,74) = T3sum(1:35,74) + Gcoeff * G3tensor(:,166)
  Gcoeff = (c(6)*(-M(142)+M(152)+M(156)-M(159)-M(165)-M(167)+M(168)+M(170)-M(172)+M(175)+M(198)+M(206)-M(208)-M(219)+M(225) &
    -M(240))) * den(66)
  T3sum(1:35,74) = T3sum(1:35,74) + Gcoeff * G3tensor(:,167)
  Gcoeff = (c(6)*(-M(152)+M(159)-M(162)+M(165)-M(168)+M(169)-M(170)+M(171)-M(173)+M(184)-M(198)+M(208)-M(216)+M(219)-M(222) &
    +M(230))) * den(66)
  T3sum(1:35,74) = T3sum(1:35,74) + Gcoeff * G3tensor(:,168)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(174)+M(189)+M(191)-M(192)-M(199) &
    -M(204)+M(207)+M(234))) * den(70)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,457)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(136)+M(150)+M(180)-M(183)+M(194) &
    -M(196)-M(213)+M(223))) * den(70)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,458)
  Gcoeff = (c(6)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(70)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,459)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(136)+M(150)+M(180)-M(183)+M(194) &
    -M(196)-M(213)+M(223))) * den(70)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,556)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(144)+M(147)-M(177)+M(181)-M(182) &
    +M(190)-M(200)+M(237))) * den(70)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,557)
  Gcoeff = (c(6)*(M(136)-M(144)+M(147)-M(150)-M(177)-M(180)+M(181)-M(182)+M(183)+M(190)-M(194)+M(196)-M(200)+M(213)-M(223) &
    +M(237))) * den(70)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,558)
  Gcoeff = (c(5)*(-M(41)+M(53)+M(60)+M(63)+M(65)-M(68)-M(69)-M(70)+M(72)+M(75)-M(81)-M(93))+c(6)*(-M(153)+M(160)+M(171)+M(184) &
    -M(186)-M(188)+M(195)-M(221))) * den(214)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,385)
  Gcoeff = (c(5)*(-M(41)+M(53)+M(60)+M(63)+M(65)-M(68)-M(69)-M(70)+M(72)+M(75)-M(81)-M(93))+c(6)*(-M(147)+M(158)+M(177)+M(182) &
    -M(185)-M(190)+M(201)-M(245))) * den(214)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,386)
  Gcoeff = (c(6)*(-M(147)+M(153)+M(158)-M(160)-M(171)+M(177)+M(182)-M(184)-M(185)+M(186)+M(188)-M(190)-M(195)+M(201)+M(221) &
    -M(245))) * den(214)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,387)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(144)+M(168)-M(181)-M(187) &
    +M(198)+M(200)-M(231)-M(237))) * den(398)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,718)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(134)-M(179)-M(193)+M(197) &
    +M(202)+M(210)-M(228)-M(247))) * den(398)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,719)
  Gcoeff = (c(6)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(398)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,720)
  Gcoeff = (c(4)*(M(3)-M(4)-M(5)+M(6)-M(13)+M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)+M(41)+M(48)+M(51) &
    -M(53)-M(60)+M(63)-M(65)-M(66)-M(67)+M(68)+M(69)-M(70)+M(71)-M(72)-M(73)+M(74)+M(75)+M(76)-M(78)+M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(144)-M(181)+M(200)-M(237))) * den(40)
  T4sum(1:35,142) = T4sum(1:35,142) + Gcoeff * G3tensor(:,284)
  Gcoeff = (c(4)*(M(3)-M(4)-M(5)+M(6)-M(13)+M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)-M(41)-M(48)-M(51) &
    +M(53)-M(60)+M(63)+M(65)+M(66)+M(67)-M(68)+M(69)-M(70)-M(71)-M(72)-M(73)-M(74)+M(75)+M(76)+M(78)+M(81)+M(88)+M(90)-M(93) &
    -M(100))+c(6)*(-M(150)+M(183)-M(194)+M(213))) * den(40)
  T4sum(1:35,142) = T4sum(1:35,142) + Gcoeff * G3tensor(:,285)
  Gcoeff = (c(5)*(-M(41)-M(48)-M(51)+M(53)+M(65)+M(66)+M(67)-M(68)-M(71)-M(74)+M(78)+M(90))+c(6)*(-M(144)-M(150)+M(181)+M(183) &
    -M(194)-M(200)+M(213)+M(237))) * den(40)
  T4sum(1:35,142) = T4sum(1:35,142) + Gcoeff * G3tensor(:,286)
  Gcoeff = (c(5)*(-M(41)+M(53)+M(60)+M(63)+M(65)-M(68)-M(69)-M(70)+M(72)+M(75)-M(81)-M(93))+c(6)*(-M(153)+M(160)+M(171)+M(184) &
    -M(186)-M(188)+M(195)-M(221))) * den(214)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,47)
  Gcoeff = (c(5)*(-M(41)+M(53)+M(60)+M(63)+M(65)-M(68)-M(69)-M(70)+M(72)+M(75)-M(81)-M(93))+c(6)*(M(168)+M(174)-M(187)-M(189) &
    +M(192)+M(198)-M(207)-M(231))) * den(214)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,53)
  Gcoeff = (c(6)*(M(153)-M(160)+M(168)-M(171)+M(174)-M(184)+M(186)-M(187)+M(188)-M(189)+M(192)-M(195)+M(198)-M(207)+M(221) &
    -M(231))) * den(214)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,59)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)-M(48)+M(51) &
    +M(53)-M(60)+M(63)+M(65)+M(66)-M(67)-M(68)+M(69)-M(70)-M(71)-M(72)-M(73)+M(74)+M(75)+M(76)+M(78)+M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(-M(193)+M(197)+M(210)-M(228))) * den(40)
  T4sum(1:35,107) = T4sum(1:35,107) + Gcoeff * G3tensor(:,242)
  Gcoeff = (c(4)*(M(3)-M(4)-M(5)+M(6)-M(13)+M(14)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(31)+M(32)+M(35)-M(36)-M(41)-M(48)-M(51) &
    +M(53)-M(60)+M(63)+M(65)+M(66)+M(67)-M(68)+M(69)-M(70)-M(71)-M(72)-M(73)-M(74)+M(75)+M(76)+M(78)+M(81)+M(88)+M(90)-M(93) &
    -M(100))+c(6)*(-M(150)+M(183)-M(194)+M(213))) * den(40)
  T4sum(1:35,107) = T4sum(1:35,107) + Gcoeff * G3tensor(:,243)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(150)+M(183)+M(193)-M(194) &
    -M(197)-M(210)+M(213)+M(228))) * den(40)
  T4sum(1:35,107) = T4sum(1:35,107) + Gcoeff * G3tensor(:,244)
  Gcoeff = (c(6)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(498)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,287)
  Gcoeff = (c(6)*(M(134)-M(144)-M(168)-M(179)+M(181)+M(187)-M(193)+M(197)-M(198)-M(200)+M(202)+M(210)-M(228)+M(231)+M(237) &
    -M(247))) * den(498)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,288)
  Gcoeff = (c(6)*(-M(150)+M(153)+M(168)-M(171)+M(183)-M(184)-M(187)+M(188)+M(193)-M(194)-M(197)+M(198)-M(210)+M(213)+M(228) &
    -M(231))) * den(498)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,289)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(6)*(-M(131)+M(137)-M(211)+M(217)+M(227) &
    -M(229)+M(248)-M(250))) * den(73)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,622)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(6)*(-M(161)+M(185)-M(203)+M(205)+M(235) &
    -M(241)+M(245)-M(246))) * den(73)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,623)
  Gcoeff = (c(6)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(73)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,624)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(6)*(-M(143)+M(145)-M(163)+M(187)+M(231) &
    -M(232)+M(236)-M(242))) * den(73)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,133)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(6)*(-M(131)+M(137)-M(211)+M(217)+M(227) &
    -M(229)+M(248)-M(250))) * den(73)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,134)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(73)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,135)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)+M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96))+c(6)*(-M(153)+M(154)+M(162)+M(164) &
    -M(186)-M(188)-M(221)+M(222))) * den(257)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,388)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)+M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96))+c(6)*(-M(147)+M(148)+M(161)+M(166) &
    -M(185)-M(190)-M(245)+M(246))) * den(257)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,389)
  Gcoeff = (c(6)*(-M(147)+M(148)+M(153)-M(154)+M(161)-M(162)-M(164)+M(166)-M(185)+M(186)+M(188)-M(190)+M(221)-M(222)-M(245) &
    +M(246))) * den(257)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,390)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(165)-M(189)+M(203) &
    -M(205)-M(207)+M(208)-M(235)+M(241))) * den(309)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,721)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(132)-M(138)-M(149) &
    +M(151)+M(212)-M(218)-M(224)+M(226))) * den(309)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,722)
  Gcoeff = (c(6)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(309)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,723)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(27)+M(28)-M(29)+M(30)+M(33)-M(34)+M(41)-M(44)-M(56) &
    -M(57)+M(58)+M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(203)-M(205)-M(235)+M(241))) * den(12)
  T4sum(1:35,163) = T4sum(1:35,163) + Gcoeff * G3tensor(:,290)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(27)+M(28)-M(29)+M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    -M(57)+M(58)-M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(211)-M(217)-M(227)+M(229))) * den(12)
  T4sum(1:35,163) = T4sum(1:35,163) + Gcoeff * G3tensor(:,291)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(6)*(-M(203)+M(205)+M(211) &
    -M(217)-M(227)+M(229)+M(235)-M(241))) * den(12)
  T4sum(1:35,163) = T4sum(1:35,163) + Gcoeff * G3tensor(:,292)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)+M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96))+c(6)*(-M(153)+M(154)+M(162)+M(164) &
    -M(186)-M(188)-M(221)+M(222))) * den(257)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,48)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)+M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96))+c(6)*(M(163)+M(165)-M(187)-M(189) &
    -M(207)+M(208)-M(231)+M(232))) * den(257)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,54)
  Gcoeff = (c(6)*(M(153)-M(154)-M(162)+M(163)-M(164)+M(165)+M(186)-M(187)+M(188)-M(189)-M(207)+M(208)+M(221)-M(222)-M(231) &
    +M(232))) * den(257)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,60)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    -M(57)+M(58)-M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(149)+M(151)+M(212)-M(218))) * den(12)
  T4sum(1:35,40) = T4sum(1:35,40) + Gcoeff * G3tensor(:,293)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(27)+M(28)-M(29)+M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    -M(57)+M(58)-M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(211)-M(217)-M(227)+M(229))) * den(12)
  T4sum(1:35,40) = T4sum(1:35,40) + Gcoeff * G3tensor(:,294)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(102)+M(108)+M(119)-M(121))+c(6)*(M(149)-M(151)+M(211)-M(212) &
    -M(217)+M(218)-M(227)+M(229))) * den(12)
  T4sum(1:35,40) = T4sum(1:35,40) + Gcoeff * G3tensor(:,295)
  Gcoeff = (c(6)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(507)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,302)
  Gcoeff = (c(6)*(M(132)-M(138)-M(149)+M(151)-M(165)+M(189)-M(203)+M(205)+M(207)-M(208)+M(212)-M(218)-M(224)+M(226)+M(235) &
    -M(241))) * den(507)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,303)
  Gcoeff = (c(6)*(M(149)-M(151)-M(162)+M(165)+M(186)-M(189)-M(207)+M(208)+M(211)-M(212)-M(217)+M(218)+M(221)-M(222)-M(227) &
    +M(229))) * den(507)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,304)
  Gcoeff = (c(5)*(-M(43)+M(55)+M(58)+M(61)-M(70)-M(82)+M(89)-M(92)-M(93)-M(94)+M(96)+M(99))+c(6)*(-M(145)+M(157)+M(163)-M(191) &
    +M(232)-M(234)-M(236)+M(243))) * den(241)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,724)
  Gcoeff = (c(5)*(-M(43)+M(55)+M(58)+M(61)-M(70)-M(82)+M(89)-M(92)-M(93)-M(94)+M(96)+M(99))+c(6)*(-M(139)+M(155)+M(169)-M(215) &
    +M(230)-M(233)-M(238)+M(249))) * den(241)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,725)
  Gcoeff = (c(6)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(241)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,726)
  Gcoeff = (c(5)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(6)*(-M(143)-M(167)+M(181)+M(187) &
    +M(231)+M(237)-M(240)-M(242))) * den(204)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,727)
  Gcoeff = (c(5)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(6)*(-M(133)+M(179)+M(193)-M(209) &
    +M(228)-M(239)-M(244)+M(247))) * den(204)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,728)
  Gcoeff = (c(6)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(204)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,729)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)+M(43)+M(46)+M(49) &
    -M(55)-M(58)-M(61)-M(67)+M(70)+M(73)-M(79)+M(82)-M(85)-M(89)-M(90)-M(91)+M(92)+M(93)+M(94)+M(95)-M(96)-M(97)+M(98)-M(99) &
    +M(100))+c(6)*(M(139)-M(169)-M(230)+M(238))) * den(45)
  T4sum(1:35,138) = T4sum(1:35,138) + Gcoeff * G3tensor(:,136)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)-M(43)-M(46)-M(49) &
    +M(55)-M(58)-M(61)+M(67)+M(70)+M(73)+M(79)+M(82)-M(85)+M(89)+M(90)+M(91)-M(92)+M(93)+M(94)-M(95)-M(96)-M(97)-M(98)-M(99) &
    +M(100))+c(6)*(-M(133)+M(193)+M(228)-M(244))) * den(45)
  T4sum(1:35,138) = T4sum(1:35,138) + Gcoeff * G3tensor(:,137)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(55)+M(67)+M(79)+M(89)+M(90)+M(91)-M(92)-M(95)-M(98))+c(6)*(-M(133)-M(139)+M(169)+M(193) &
    +M(228)+M(230)-M(238)-M(244))) * den(45)
  T4sum(1:35,138) = T4sum(1:35,138) + Gcoeff * G3tensor(:,138)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)+M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94))+c(6)*(-M(145)+M(146)+M(174)+M(176) &
    -M(191)+M(192)-M(234)-M(236))) * den(245)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,730)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)+M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94))+c(6)*(-M(139)+M(140)+M(173)+M(178) &
    -M(215)+M(216)-M(233)-M(238))) * den(245)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,731)
  Gcoeff = (c(6)*(-M(139)+M(140)+M(145)-M(146)+M(173)-M(174)-M(176)+M(178)+M(191)-M(192)-M(215)+M(216)-M(233)+M(234)+M(236) &
    -M(238))) * den(245)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,732)
  Gcoeff = (c(5)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(6)*(-M(175)+M(189)-M(199) &
    -M(204)+M(205)-M(206)+M(207)+M(235))) * den(157)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,733)
  Gcoeff = (c(5)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(6)*(-M(135)+M(138)-M(141) &
    +M(149)-M(214)+M(218)-M(220)+M(224))) * den(157)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,734)
  Gcoeff = (c(6)*(-M(135)+M(138)-M(141)+M(149)+M(175)-M(189)+M(199)+M(204)-M(205)+M(206)-M(207)-M(214)+M(218)-M(220)+M(224) &
    -M(235))) * den(157)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,735)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(27)-M(28)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(173)+M(215)-M(216)+M(233))) * den(16)
  T4sum(1:35,159) = T4sum(1:35,159) + Gcoeff * G3tensor(:,139)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(27)-M(28)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(43)+M(50)+M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)-M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(135)+M(149)+M(218)-M(220))) * den(16)
  T4sum(1:35,159) = T4sum(1:35,159) + Gcoeff * G3tensor(:,140)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)-M(92)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120))+c(6)*(-M(135)+M(149)+M(173) &
    -M(215)+M(216)+M(218)-M(220)-M(233))) * den(16)
  T4sum(1:35,159) = T4sum(1:35,159) + Gcoeff * G3tensor(:,141)
  Gcoeff = (c(5)*(-M(46)+M(51)-M(58)+M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97))+c(6)*(-M(143)+M(144)-M(167)+M(168) &
    +M(198)+M(200)-M(240)-M(242))) * den(208)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,736)
  Gcoeff = (c(5)*(-M(46)+M(51)-M(58)+M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97))+c(6)*(-M(133)+M(134)+M(197)+M(202) &
    -M(209)+M(210)-M(239)-M(244))) * den(208)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,737)
  Gcoeff = (c(6)*(-M(133)+M(134)+M(143)-M(144)+M(167)-M(168)+M(197)-M(198)-M(200)+M(202)-M(209)+M(210)-M(239)+M(240)+M(242) &
    -M(244))) * den(208)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,738)
  Gcoeff = (c(5)*(M(58)-M(63)-M(75)+M(96)+M(102)-M(105)-M(111)+M(121)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(165)-M(175)-M(199) &
    +M(203)-M(204)-M(206)+M(208)+M(241))) * den(160)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,739)
  Gcoeff = (c(5)*(M(58)-M(63)-M(75)+M(96)+M(102)-M(105)-M(111)+M(121)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(132)-M(135)-M(141) &
    +M(151)+M(212)-M(214)-M(220)+M(226))) * den(160)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,740)
  Gcoeff = (c(6)*(M(132)-M(135)-M(141)+M(151)-M(165)+M(175)+M(199)-M(203)+M(204)+M(206)-M(208)+M(212)-M(214)-M(220)+M(226) &
    -M(241))) * den(160)
  T3sum(1:15,59) = T3sum(1:15,59) + Gcoeff * G2tensor(:,741)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(197)+M(209)-M(210)+M(239))) * den(22)
  T4sum(1:35,174) = T4sum(1:35,174) + Gcoeff * G3tensor(:,49)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(58) &
    -M(63)+M(74)-M(75)-M(76)+M(85)-M(88)-M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(141)+M(151)+M(212)-M(214))) * den(22)
  T4sum(1:35,174) = T4sum(1:35,174) + Gcoeff * G3tensor(:,50)
  Gcoeff = (c(5)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(6)*(-M(141)+M(151)+M(197) &
    -M(209)+M(210)+M(212)-M(214)-M(239))) * den(22)
  T4sum(1:35,174) = T4sum(1:35,174) + Gcoeff * G3tensor(:,51)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(44)+M(45)+M(50)+M(51)+M(52)+M(56)+M(58)+M(62)+M(69)+M(74)+M(80)+M(81)+M(82)+M(83)+M(85)+M(86)+M(94)+M(96)+M(97) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(151)+M(212)))
  T5sum(1:70,83) = T5sum(1:70,83) + Gcoeff * G4tensor(:,19)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(6)-M(7)-M(9)-M(12)-M(14)-M(16)-M(17)-M(19)-M(21)-M(24)-M(26)-M(27)-M(29)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(44)+M(45)+M(55)+M(56)+M(58)+M(67)+M(69)+M(79)+M(80)+M(81)+M(82)+M(83)+M(85)+M(89)+M(90)+M(91)+M(94)+M(96)+M(97) &
    +M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(211)+M(229)))
  T5sum(1:70,83) = T5sum(1:70,83) + Gcoeff * G4tensor(:,20)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)-M(102)+M(104)+M(108)+M(110)-M(114)-M(116)+M(119)-M(120)-M(121)-M(122) &
    +M(123)+M(124))+c(6)*(-M(151)+M(211)-M(212)+M(229)))
  T5sum(1:70,83) = T5sum(1:70,83) + Gcoeff * G4tensor(:,21)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(45)+M(50)+M(51)+M(52)+M(57)+M(62)+M(68)+M(70)+M(74)+M(80)+M(82)+M(83)+M(84)+M(85)+M(86)+M(93)+M(94)+M(97) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(149)+M(218)))
  T5sum(1:70,84) = T5sum(1:70,84) + Gcoeff * G4tensor(:,56)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(9)-M(12)-M(14)-M(16)-M(18)-M(19)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(45)+M(55)+M(57)+M(67)+M(68)+M(70)+M(79)+M(80)+M(82)+M(83)+M(84)+M(85)+M(89)+M(90)+M(91)+M(93)+M(94)+M(97) &
    +M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(217)+M(227)))
  T5sum(1:70,84) = T5sum(1:70,84) + Gcoeff * G4tensor(:,57)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)+M(17)-M(18)-M(19)+M(20)-M(21)+M(22)-M(23)+M(24)-M(25)+M(26)+M(29)-M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)+M(102)+M(104)-M(108)+M(110)-M(114)-M(116)-M(119)-M(120)+M(121)-M(122) &
    +M(123)+M(124))+c(6)*(-M(149)+M(217)-M(218)+M(227)))
  T5sum(1:70,84) = T5sum(1:70,84) + Gcoeff * G4tensor(:,58)
  Gcoeff = (c(5)*(-M(43)+M(55)+M(58)+M(61)-M(70)-M(82)+M(89)-M(92)-M(93)-M(94)+M(96)+M(99))+c(6)*(M(161)+M(167)-M(181)-M(205) &
    -M(235)-M(237)+M(240)+M(246))) * den(241)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,742)
  Gcoeff = (c(5)*(-M(43)+M(55)+M(58)+M(61)-M(70)-M(82)+M(89)-M(92)-M(93)-M(94)+M(96)+M(99))+c(6)*(-M(139)+M(155)+M(169)-M(215) &
    +M(230)-M(233)-M(238)+M(249))) * den(241)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,743)
  Gcoeff = (c(6)*(-M(139)+M(155)-M(161)-M(167)+M(169)+M(181)+M(205)-M(215)+M(230)-M(233)+M(235)+M(237)-M(238)-M(240)-M(246) &
    +M(249))) * den(241)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,744)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)+M(43)+M(46)+M(49) &
    -M(55)-M(58)-M(61)-M(67)+M(70)+M(73)-M(79)+M(82)-M(85)-M(89)-M(90)-M(91)+M(92)+M(93)+M(94)+M(95)-M(96)-M(97)+M(98)-M(99) &
    +M(100))+c(6)*(M(139)-M(169)-M(230)+M(238))) * den(45)
  T4sum(1:35,112) = T4sum(1:35,112) + Gcoeff * G3tensor(:,5)
  Gcoeff = (c(4)*(M(1)-M(2)-M(3)+M(4)-M(7)+M(8)-M(9)+M(10)+M(13)-M(14)+M(15)-M(16)+M(25)-M(26)-M(29)+M(30)+M(43)-M(46)+M(49)-M(55) &
    -M(58)-M(61)+M(67)+M(70)+M(73)-M(79)+M(82)-M(85)-M(89)+M(90)-M(91)+M(92)+M(93)+M(94)-M(95)-M(96)-M(97)+M(98)-M(99)+M(100)) &
    +c(6)*(-M(167)+M(181)+M(237)-M(240))) * den(45)
  T4sum(1:35,112) = T4sum(1:35,112) + Gcoeff * G3tensor(:,8)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(139)-M(167)+M(169)+M(181)+M(230) &
    +M(237)-M(238)-M(240))) * den(45)
  T4sum(1:35,112) = T4sum(1:35,112) + Gcoeff * G3tensor(:,11)
  Gcoeff = (c(6)*(M(137)-M(139)-M(167)+M(169)-M(179)+M(181)+M(209)-M(211)-M(229)+M(230)+M(237)-M(238)+M(239)-M(240)-M(247) &
    +M(248))) * den(527)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,306)
  Gcoeff = (c(6)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(527)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,307)
  Gcoeff = (c(6)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(527)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,308)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)+M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94))+c(6)*(M(175)+M(177)-M(181)+M(182) &
    -M(205)+M(206)-M(235)-M(237))) * den(245)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,745)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)+M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94))+c(6)*(-M(139)+M(140)+M(173)+M(178) &
    -M(215)+M(216)-M(233)-M(238))) * den(245)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,746)
  Gcoeff = (c(6)*(-M(139)+M(140)+M(173)-M(175)-M(177)+M(178)+M(181)-M(182)+M(205)-M(206)-M(215)+M(216)-M(233)+M(235)+M(237) &
    -M(238))) * den(245)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,747)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(27)-M(28)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(173)+M(215)-M(216)+M(233))) * den(16)
  T4sum(1:35,35) = T4sum(1:35,35) + Gcoeff * G3tensor(:,6)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(27)-M(28)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)-M(105)+M(106)+M(108)-M(111)-M(114)+M(117)+M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(175)+M(205)-M(206)+M(235))) * den(16)
  T4sum(1:35,35) = T4sum(1:35,35) + Gcoeff * G3tensor(:,9)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(31)+M(32)+M(35)-M(36)-M(105)+M(108)-M(111)+M(119))+c(6)*(M(173)-M(175)+M(205)-M(206) &
    -M(215)+M(216)-M(233)+M(235))) * den(16)
  T4sum(1:35,35) = T4sum(1:35,35) + Gcoeff * G3tensor(:,12)
  Gcoeff = (c(6)*(-M(138)+M(141)+M(173)-M(175)+M(180)-M(183)+M(205)-M(206)-M(213)+M(214)-M(215)+M(216)+M(223)-M(224)-M(233) &
    +M(235))) * den(518)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,310)
  Gcoeff = (c(6)*(-M(135)+M(138)-M(141)+M(149)+M(175)-M(189)+M(199)+M(204)-M(205)+M(206)-M(207)-M(214)+M(218)-M(220)+M(224) &
    -M(235))) * den(518)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,311)
  Gcoeff = (c(6)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(518)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,312)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(6)-M(7)-M(9)-M(12)-M(14)-M(16)-M(17)-M(19)-M(21)-M(24)-M(26)-M(27)-M(29)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(44)+M(45)+M(55)+M(56)+M(58)+M(67)+M(69)+M(79)+M(80)+M(81)+M(82)+M(83)+M(85)+M(89)+M(90)+M(91)+M(94)+M(96)+M(97) &
    +M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(211)+M(229)))
  T5sum(1:70,103) = T5sum(1:70,103) + Gcoeff * G4tensor(:,28)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(42)+M(46)+M(53)+M(55)+M(58)+M(65)+M(66)+M(69)+M(78)+M(79)+M(80)+M(81)+M(82)+M(85)+M(89)+M(91)+M(94)+M(95)+M(96)+M(97) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(209)+M(239)))
  T5sum(1:70,103) = T5sum(1:70,103) + Gcoeff * G4tensor(:,29)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)-M(44)-M(45)+M(46) &
    +M(53)-M(56)+M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)-M(110)+M(111)+M(112)-M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(209)-M(211)-M(229)+M(239)))
  T5sum(1:70,103) = T5sum(1:70,103) + Gcoeff * G4tensor(:,30)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(43)+M(45)+M(53)+M(54)+M(57)+M(65)+M(67)+M(70)+M(77)+M(79)+M(82)+M(83)+M(84)+M(85)+M(90)+M(91)+M(92)+M(93)+M(94)+M(97) &
    +M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(215)+M(233)))
  T5sum(1:70,104) = T5sum(1:70,104) + Gcoeff * G4tensor(:,4)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(5)-M(8)-M(10)-M(11)-M(13)-M(15)-M(18)-M(20)-M(22)-M(23)-M(25)-M(28)-M(30)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(43)+M(44)+M(46)+M(54)+M(56)+M(57)+M(66)+M(70)+M(77)+M(78)+M(79)+M(82)+M(84)+M(85)+M(91)+M(92)+M(93)+M(94)+M(95)+M(97) &
    +M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(205)+M(235)))
  T5sum(1:70,104) = T5sum(1:70,104) + Gcoeff * G4tensor(:,5)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)+M(44)-M(45)+M(46) &
    -M(53)+M(56)-M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)-M(103)-M(105)+M(107)+M(108)-M(109)-M(110)-M(111)+M(112)+M(113)+M(118) &
    +M(119)-M(124))+c(6)*(M(205)-M(215)-M(233)+M(235)))
  T5sum(1:70,104) = T5sum(1:70,104) + Gcoeff * G4tensor(:,6)
  Gcoeff = (c(6)*(M(141)-M(142)-M(167)+M(169)+M(171)-M(172)-M(183)+M(184)+M(209)-M(211)-M(213)+M(214)-M(229)+M(230)+M(239) &
    -M(240))) * den(529)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,313)
  Gcoeff = (c(6)*(-M(141)+M(142)+M(151)-M(152)+M(167)-M(168)-M(170)+M(172)+M(197)-M(198)-M(209)+M(210)+M(212)-M(214)-M(239) &
    +M(240))) * den(529)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,314)
  Gcoeff = (c(6)*(-M(151)+M(152)+M(168)-M(169)+M(170)-M(171)+M(183)-M(184)-M(197)+M(198)-M(210)+M(211)-M(212)+M(213)+M(229) &
    -M(230))) * den(529)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,315)
  Gcoeff = (c(6)*(M(141)-M(156)+M(162)+M(173)-M(175)-M(183)-M(206)+M(209)-M(211)-M(213)+M(214)+M(216)+M(222)-M(225)-M(229) &
    +M(239))) * den(522)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,316)
  Gcoeff = (c(6)*(-M(141)+M(151)+M(156)-M(159)-M(165)+M(175)+M(197)+M(206)-M(208)-M(209)+M(210)+M(212)-M(214)-M(219)+M(225) &
    -M(239))) * den(522)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,317)
  Gcoeff = (c(6)*(-M(151)+M(159)-M(162)+M(165)-M(173)+M(183)-M(197)+M(208)-M(210)+M(211)-M(212)+M(213)-M(216)+M(219)-M(222) &
    +M(229))) * den(522)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,318)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(141)-M(183)+M(209) &
    -M(211)-M(213)+M(214)-M(229)+M(239))) * den(65)
  T4sum(1:70,174) = T4sum(1:70,174) + Gcoeff * G4tensor(:,31)
  Gcoeff = (c(5)*(-M(46)+M(51)+M(74)-M(95)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122))+c(6)*(-M(141)+M(151)+M(197) &
    -M(209)+M(210)+M(212)-M(214)-M(239))) * den(65)
  T4sum(1:70,174) = T4sum(1:70,174) + Gcoeff * G4tensor(:,32)
  Gcoeff = (c(5)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(6)*(-M(151)+M(183)-M(197) &
    -M(210)+M(211)-M(212)+M(213)+M(229))) * den(65)
  T4sum(1:70,174) = T4sum(1:70,174) + Gcoeff * G4tensor(:,33)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(158)-M(185)-M(191)+M(199) &
    +M(201)+M(204)-M(234)-M(245))) * den(398)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,748)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(134)-M(179)-M(193)+M(197) &
    +M(202)+M(210)-M(228)-M(247))) * den(398)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,749)
  Gcoeff = (c(6)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(398)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,750)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(5)+M(6)+M(13)-M(14)-M(17)+M(18)+M(19)-M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)+M(41)-M(48)+M(51) &
    -M(53)-M(60)+M(63)-M(65)+M(66)-M(67)+M(68)+M(69)-M(70)-M(71)-M(72)-M(73)+M(74)+M(75)+M(76)+M(78)+M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(-M(191)+M(199)+M(204)-M(234))) * den(40)
  T4sum(1:35,135) = T4sum(1:35,135) + Gcoeff * G3tensor(:,245)
  Gcoeff = (c(4)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)-M(31)+M(32)+M(35)-M(36)-M(41)-M(48)+M(51) &
    +M(53)-M(60)+M(63)+M(65)+M(66)-M(67)-M(68)+M(69)-M(70)-M(71)-M(72)-M(73)+M(74)+M(75)+M(76)+M(78)+M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(-M(193)+M(197)+M(210)-M(228))) * den(40)
  T4sum(1:35,135) = T4sum(1:35,135) + Gcoeff * G3tensor(:,246)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(M(191)-M(193)+M(197)-M(199)-M(204) &
    +M(210)-M(228)+M(234))) * den(40)
  T4sum(1:35,135) = T4sum(1:35,135) + Gcoeff * G3tensor(:,247)
  Gcoeff = (c(6)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(540)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,64)
  Gcoeff = (c(6)*(-M(147)+M(153)+M(158)-M(160)-M(171)+M(177)+M(182)-M(184)-M(185)+M(186)+M(188)-M(190)-M(195)+M(201)+M(221) &
    -M(245))) * den(540)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,65)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(147)-M(153)+M(171)-M(177)+M(179)-M(180)-M(182)+M(184)-M(188)+M(190)+M(196)-M(202)-M(223) &
    +M(247))) * den(540)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,66)
  Gcoeff = (c(6)*(M(136)-M(160)-M(180)+M(186)+M(191)-M(193)-M(195)+M(196)+M(197)-M(199)-M(204)+M(210)+M(221)-M(223)-M(228) &
    +M(234))) * den(83)
  T3sum(1:35,63) = T3sum(1:35,63) + Gcoeff * G3tensor(:,172)
  Gcoeff = (c(6)*(-M(136)+M(150)+M(174)+M(180)-M(183)-M(189)-M(191)+M(192)+M(194)-M(196)+M(199)+M(204)-M(207)-M(213)+M(223) &
    -M(234))) * den(83)
  T3sum(1:35,63) = T3sum(1:35,63) + Gcoeff * G3tensor(:,173)
  Gcoeff = (c(6)*(-M(150)+M(160)-M(174)+M(183)-M(186)+M(189)-M(192)+M(193)-M(194)+M(195)-M(197)+M(207)-M(210)+M(213)-M(221) &
    +M(228))) * den(83)
  T3sum(1:35,63) = T3sum(1:35,63) + Gcoeff * G3tensor(:,174)
  Gcoeff = (c(5)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(6)*(-M(157)+M(185)+M(191)-M(203) &
    +M(234)-M(241)-M(243)+M(245))) * den(204)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,751)
  Gcoeff = (c(5)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(6)*(-M(133)+M(179)+M(193)-M(209) &
    +M(228)-M(239)-M(244)+M(247))) * den(204)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,752)
  Gcoeff = (c(6)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(204)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,753)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(7)+M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)+M(43)-M(46)-M(49)-M(55) &
    -M(58)-M(61)+M(67)+M(70)+M(73)+M(79)+M(82)-M(85)-M(89)+M(90)+M(91)+M(92)+M(93)+M(94)-M(95)-M(96)-M(97)-M(98)-M(99)+M(100)) &
    +c(6)*(-M(157)+M(191)+M(234)-M(243))) * den(45)
  T4sum(1:35,127) = T4sum(1:35,127) + Gcoeff * G3tensor(:,142)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(9)-M(10)+M(13)-M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)-M(43)-M(46)-M(49) &
    +M(55)-M(58)-M(61)+M(67)+M(70)+M(73)+M(79)+M(82)-M(85)+M(89)+M(90)+M(91)-M(92)+M(93)+M(94)-M(95)-M(96)-M(97)-M(98)-M(99) &
    +M(100))+c(6)*(-M(133)+M(193)+M(228)-M(244))) * den(45)
  T4sum(1:35,127) = T4sum(1:35,127) + Gcoeff * G3tensor(:,143)
  Gcoeff = (c(5)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(6)*(-M(133)+M(157)-M(191)+M(193)+M(228) &
    -M(234)+M(243)-M(244))) * den(45)
  T4sum(1:35,127) = T4sum(1:35,127) + Gcoeff * G3tensor(:,144)
  Gcoeff = (c(6)*(M(131)-M(133)-M(155)+M(157)-M(191)+M(193)+M(215)-M(217)-M(227)+M(228)+M(233)-M(234)+M(243)-M(244)-M(249) &
    +M(250))) * den(557)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,321)
  Gcoeff = (c(6)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(557)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,322)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(139)-M(145)+M(163)-M(169)-M(193)+M(217)+M(227)-M(228)-M(230)+M(232)-M(236)+M(238)+M(244) &
    -M(250))) * den(557)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,323)
  Gcoeff = (c(5)*(-M(46)+M(51)-M(58)+M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97))+c(6)*(-M(157)+M(158)+M(199)+M(201) &
    -M(203)+M(204)-M(241)-M(243))) * den(208)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,754)
  Gcoeff = (c(5)*(-M(46)+M(51)-M(58)+M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97))+c(6)*(-M(133)+M(134)+M(197)+M(202) &
    -M(209)+M(210)-M(239)-M(244))) * den(208)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,755)
  Gcoeff = (c(6)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(208)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,756)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(197)+M(209)-M(210)+M(239))) * den(22)
  T4sum(1:35,23) = T4sum(1:35,23) + Gcoeff * G3tensor(:,1)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)+M(112)-M(116)+M(118)+M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(199)+M(203)-M(204)+M(241))) * den(22)
  T4sum(1:35,23) = T4sum(1:35,23) + Gcoeff * G3tensor(:,2)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(37)+M(38)+M(39)-M(40)+M(102)-M(105)-M(111)+M(121))+c(6)*(M(197)-M(199)+M(203)-M(204) &
    -M(209)+M(210)-M(239)+M(241))) * den(22)
  T4sum(1:35,23) = T4sum(1:35,23) + Gcoeff * G3tensor(:,3)
  Gcoeff = (c(6)*(-M(132)+M(135)+M(156)-M(159)+M(197)-M(199)+M(203)-M(204)-M(209)+M(210)-M(219)+M(220)+M(225)-M(226)-M(239) &
    +M(241))) * den(548)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,325)
  Gcoeff = (c(6)*(M(132)-M(135)-M(141)+M(151)-M(165)+M(175)+M(199)-M(203)+M(204)+M(206)-M(208)+M(212)-M(214)-M(220)+M(226) &
    -M(241))) * den(548)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,326)
  Gcoeff = (c(6)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(548)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,327)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(9)-M(12)-M(14)-M(16)-M(18)-M(19)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(45)+M(55)+M(57)+M(67)+M(68)+M(70)+M(79)+M(80)+M(82)+M(83)+M(84)+M(85)+M(89)+M(90)+M(91)+M(93)+M(94)+M(97) &
    +M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(217)+M(227)))
  T5sum(1:70,115) = T5sum(1:70,115) + Gcoeff * G4tensor(:,66)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(43)+M(45)+M(53)+M(54)+M(57)+M(65)+M(67)+M(70)+M(77)+M(79)+M(82)+M(83)+M(84)+M(85)+M(90)+M(91)+M(92)+M(93)+M(94)+M(97) &
    +M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(215)+M(233)))
  T5sum(1:70,115) = T5sum(1:70,115) + Gcoeff * G4tensor(:,67)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(5)+M(6)-M(7)+M(8)-M(13)+M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42)+M(43)+M(53) &
    +M(54)-M(55)+M(65)-M(68)+M(77)-M(80)-M(89)+M(92)-M(101)-M(102)+M(103)-M(104)+M(105)+M(106)+M(109)+M(111)-M(115)+M(117)-M(121) &
    -M(123))+c(6)*(M(215)-M(217)-M(227)+M(233)))
  T5sum(1:70,115) = T5sum(1:70,115) + Gcoeff * G4tensor(:,68)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(42)+M(46)+M(53)+M(55)+M(58)+M(65)+M(66)+M(69)+M(78)+M(79)+M(80)+M(81)+M(82)+M(85)+M(89)+M(91)+M(94)+M(95)+M(96)+M(97) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(209)+M(239)))
  T5sum(1:70,116) = T5sum(1:70,116) + Gcoeff * G4tensor(:,1)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(10)-M(11)-M(13)-M(15)-M(17)-M(20)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(41)+M(43)+M(46)+M(54)+M(58)+M(66)+M(68)+M(69)+M(77)+M(78)+M(79)+M(81)+M(82)+M(85)+M(91)+M(92)+M(94)+M(95)+M(96)+M(97) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(203)+M(241)))
  T5sum(1:70,116) = T5sum(1:70,116) + Gcoeff * G4tensor(:,2)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(5)+M(6)-M(7)+M(8)-M(13)+M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42)+M(43)-M(53) &
    +M(54)-M(55)-M(65)+M(68)+M(77)-M(80)-M(89)+M(92)+M(101)+M(102)-M(103)-M(104)-M(105)+M(106)-M(109)-M(111)+M(115)+M(117)+M(121) &
    -M(123))+c(6)*(M(203)-M(209)-M(239)+M(241)))
  T5sum(1:70,116) = T5sum(1:70,116) + Gcoeff * G4tensor(:,3)
  Gcoeff = (c(6)*(M(135)-M(136)-M(159)+M(160)-M(191)+M(193)+M(195)-M(196)+M(215)-M(217)-M(219)+M(220)-M(227)+M(228)+M(233) &
    -M(234))) * den(559)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,328)
  Gcoeff = (c(6)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(559)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,329)
  Gcoeff = (c(6)*(-M(149)+M(150)+M(159)-M(160)-M(173)+M(174)+M(192)-M(193)+M(194)-M(195)-M(216)+M(217)-M(218)+M(219)+M(227) &
    -M(228))) * den(559)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,330)
  Gcoeff = (c(6)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(552)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,331)
  Gcoeff = (c(6)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(552)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,332)
  Gcoeff = (c(6)*(-M(149)+M(159)-M(173)+M(183)-M(186)+M(189)-M(197)+M(207)-M(210)+M(213)-M(216)+M(217)-M(218)+M(219)-M(221) &
    +M(227))) * den(552)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,333)
  Gcoeff = (c(5)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(6)*(M(135)-M(159)+M(215) &
    -M(217)-M(219)+M(220)-M(227)+M(233))) * den(82)
  T4sum(1:70,159) = T4sum(1:70,159) + Gcoeff * G4tensor(:,69)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)-M(92)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120))+c(6)*(-M(135)+M(149)+M(173) &
    -M(215)+M(216)+M(218)-M(220)-M(233))) * den(82)
  T4sum(1:70,159) = T4sum(1:70,159) + Gcoeff * G4tensor(:,70)
  Gcoeff = (c(5)*(-M(50)+M(55)-M(62)+M(89)+M(102)+M(104)-M(108)-M(114)-M(119)-M(120)+M(121)+M(123))+c(6)*(-M(149)+M(159)-M(173) &
    -M(216)+M(217)-M(218)+M(219)+M(227))) * den(82)
  T4sum(1:70,159) = T4sum(1:70,159) + Gcoeff * G4tensor(:,71)
  Gcoeff = (c(6)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(94)
  T3sum(1:35,65) = T3sum(1:35,65) + Gcoeff * G3tensor(:,212)
  Gcoeff = (c(6)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(94)
  T3sum(1:35,65) = T3sum(1:35,65) + Gcoeff * G3tensor(:,213)
  Gcoeff = (c(6)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(94)
  T3sum(1:35,65) = T3sum(1:35,65) + Gcoeff * G3tensor(:,214)
  Gcoeff = (c(6)*(M(131)-M(132)-M(155)+M(156)-M(179)+M(180)+M(185)-M(186)-M(221)+M(223)+M(225)-M(226)+M(245)-M(247)-M(249) &
    +M(250))) * den(574)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,67)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(574)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,68)
  Gcoeff = (c(6)*(-M(137)+M(138)+M(155)-M(156)-M(161)+M(162)+M(179)-M(180)+M(222)-M(223)+M(224)-M(225)-M(246)+M(247)-M(248) &
    +M(249))) * den(574)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,69)
  Gcoeff = (c(6)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(568)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,334)
  Gcoeff = (c(6)*(M(132)-M(138)-M(162)+M(186)-M(203)+M(205)+M(211)-M(217)+M(221)-M(222)-M(224)+M(226)-M(227)+M(229)+M(235) &
    -M(241))) * den(568)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,335)
  Gcoeff = (c(6)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(568)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,336)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(203)-M(209)-M(215) &
    +M(217)+M(227)-M(233)-M(239)+M(241))) * den(92)
  T4sum(1:70,163) = T4sum(1:70,163) + Gcoeff * G4tensor(:,118)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(6)*(-M(203)+M(205)+M(211) &
    -M(217)-M(227)+M(229)+M(235)-M(241))) * den(92)
  T4sum(1:70,163) = T4sum(1:70,163) + Gcoeff * G4tensor(:,119)
  Gcoeff = (c(5)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(6)*(-M(205)+M(209)-M(211) &
    +M(215)-M(229)+M(233)-M(235)+M(239))) * den(92)
  T4sum(1:70,163) = T4sum(1:70,163) + Gcoeff * G4tensor(:,120)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(37)+M(38)+M(39)-M(40)+M(102)-M(105)-M(111)+M(121))+c(6)*(M(197)-M(199)+M(203)-M(204) &
    -M(209)+M(210)-M(239)+M(241))) * den(97)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,511)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(37)+M(38)+M(39)-M(40)+M(102)-M(105)-M(111)+M(121))+c(6)*(M(133)-M(134)-M(157)+M(158) &
    +M(201)-M(202)-M(243)+M(244))) * den(97)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,512)
  Gcoeff = (c(6)*(M(133)-M(134)-M(157)+M(158)-M(197)+M(199)+M(201)-M(202)-M(203)+M(204)+M(209)-M(210)+M(239)-M(241)-M(243) &
    +M(244))) * den(97)
  T3sum(1:15,54) = T3sum(1:15,54) + Gcoeff * G2tensor(:,513)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(37)+M(38)+M(39)-M(40)+M(102)-M(105)-M(111)+M(121))+c(6)*(M(133)-M(134)-M(157)+M(158) &
    +M(201)-M(202)-M(243)+M(244))) * den(97)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,571)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(37)+M(38)+M(39)-M(40)+M(102)-M(105)-M(111)+M(121))+c(6)*(M(132)-M(135)-M(156)+M(159) &
    +M(219)-M(220)-M(225)+M(226))) * den(97)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,572)
  Gcoeff = (c(6)*(M(132)-M(133)+M(134)-M(135)-M(156)+M(157)-M(158)+M(159)-M(201)+M(202)+M(219)-M(220)-M(225)+M(226)+M(243) &
    -M(244))) * den(97)
  T3sum(1:15,25) = T3sum(1:15,25) + Gcoeff * G2tensor(:,573)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(131)-M(155)-M(179) &
    +M(185)+M(245)-M(247)-M(249)+M(250))) * den(197)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,391)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(132)-M(156)-M(180) &
    +M(186)+M(221)-M(223)-M(225)+M(226))) * den(197)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,392)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(197)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,393)
  Gcoeff = (c(5)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(6)*(M(135)-M(159)+M(215) &
    -M(217)-M(219)+M(220)-M(227)+M(233))) * den(400)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,757)
  Gcoeff = (c(5)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(6)*(M(136)-M(160)+M(191) &
    -M(193)-M(195)+M(196)-M(228)+M(234))) * den(400)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,758)
  Gcoeff = (c(6)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(400)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,759)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(5)+M(6)-M(7)+M(8)-M(13)+M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)-M(101)+M(102)+M(103)+M(104)-M(105)-M(106)+M(109)-M(111)-M(115)-M(117)+M(121) &
    +M(123))+c(6)*(M(133)-M(157)-M(243)+M(244))) * den(41)
  T4sum(1:35,145) = T4sum(1:35,145) + Gcoeff * G3tensor(:,121)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(5)+M(6)-M(7)+M(8)-M(13)+M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42)-M(43)+M(53) &
    +M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92)-M(101)+M(102)+M(103)+M(104)-M(105)-M(106)+M(109)-M(111)-M(115)-M(117)+M(121) &
    +M(123))+c(6)*(-M(135)+M(159)+M(219)-M(220))) * den(41)
  T4sum(1:35,145) = T4sum(1:35,145) + Gcoeff * G3tensor(:,122)
  Gcoeff = (c(5)*(-M(41)-M(42)-M(43)+M(53)+M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92))+c(6)*(-M(133)-M(135)+M(157)+M(159) &
    +M(219)-M(220)+M(243)-M(244))) * den(41)
  T4sum(1:35,145) = T4sum(1:35,145) + Gcoeff * G3tensor(:,123)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(131)-M(155)-M(179) &
    +M(185)+M(245)-M(247)-M(249)+M(250))) * den(197)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,178)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(203)-M(209)-M(215) &
    +M(217)+M(227)-M(233)-M(239)+M(241))) * den(197)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,184)
  Gcoeff = (c(6)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(197)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,190)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(5)+M(6)-M(7)+M(8)-M(13)+M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42)+M(43)-M(53) &
    -M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92)-M(101)+M(102)+M(103)+M(104)-M(105)-M(106)+M(109)-M(111)-M(115)-M(117)+M(121) &
    +M(123))+c(6)*(M(133)-M(157)-M(243)+M(244))) * den(41)
  T4sum(1:35,127) = T4sum(1:35,127) + Gcoeff * G3tensor(:,109)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)-M(5)+M(6)+M(7)-M(8)+M(13)-M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42)-M(43)-M(53) &
    -M(54)+M(55)-M(65)+M(68)-M(77)+M(80)+M(89)-M(92)-M(101)+M(102)+M(103)+M(104)-M(105)-M(106)+M(109)-M(111)-M(115)-M(117)+M(121) &
    +M(123))+c(6)*(-M(191)+M(193)+M(228)-M(234))) * den(41)
  T4sum(1:35,127) = T4sum(1:35,127) + Gcoeff * G3tensor(:,110)
  Gcoeff = (c(5)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(6)*(-M(133)+M(157)-M(191)+M(193)+M(228) &
    -M(234)+M(243)-M(244))) * den(41)
  T4sum(1:35,127) = T4sum(1:35,127) + Gcoeff * G3tensor(:,111)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(135)-M(136)+M(155)-M(157)-M(159)+M(160)+M(195)-M(196)-M(219)+M(220)-M(243)+M(244)+M(249) &
    -M(250))) * den(603)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,337)
  Gcoeff = (c(6)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(603)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,338)
  Gcoeff = (c(6)*(M(131)-M(133)-M(155)+M(157)-M(191)+M(193)+M(215)-M(217)-M(227)+M(228)+M(233)-M(234)+M(243)-M(244)-M(249) &
    +M(250))) * den(603)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,339)
  Gcoeff = (c(5)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(6)*(M(180)-M(186)-M(197) &
    +M(199)+M(204)-M(210)-M(221)+M(223))) * den(400)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,760)
  Gcoeff = (c(5)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(6)*(M(136)-M(160)+M(191) &
    -M(193)-M(195)+M(196)-M(228)+M(234))) * den(400)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,761)
  Gcoeff = (c(6)*(M(136)-M(160)-M(180)+M(186)+M(191)-M(193)-M(195)+M(196)+M(197)-M(199)-M(204)+M(210)+M(221)-M(223)-M(228) &
    +M(234))) * den(400)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,762)
  Gcoeff = (c(4)*(-M(1)+M(2)-M(3)+M(4)-M(5)+M(6)+M(7)-M(8)+M(13)-M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)+M(42)-M(43)-M(53) &
    -M(54)+M(55)-M(65)+M(68)-M(77)+M(80)+M(89)-M(92)-M(101)+M(102)+M(103)+M(104)-M(105)-M(106)+M(109)-M(111)-M(115)-M(117)+M(121) &
    +M(123))+c(6)*(-M(191)+M(193)+M(228)-M(234))) * den(41)
  T4sum(1:35,135) = T4sum(1:35,135) + Gcoeff * G3tensor(:,25)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(5)-M(6)+M(7)-M(8)-M(13)+M(14)-M(19)+M(20)-M(37)+M(38)+M(39)-M(40)-M(41)+M(42)-M(43)+M(53) &
    -M(54)+M(55)+M(65)-M(68)-M(77)+M(80)+M(89)-M(92)-M(101)+M(102)+M(103)+M(104)-M(105)-M(106)+M(109)-M(111)-M(115)-M(117)+M(121) &
    +M(123))+c(6)*(M(197)-M(199)-M(204)+M(210))) * den(41)
  T4sum(1:35,135) = T4sum(1:35,135) + Gcoeff * G3tensor(:,28)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(M(191)-M(193)+M(197)-M(199)-M(204) &
    +M(210)-M(228)+M(234))) * den(41)
  T4sum(1:35,135) = T4sum(1:35,135) + Gcoeff * G3tensor(:,31)
  Gcoeff = (c(6)*(M(131)-M(132)-M(134)+M(136)-M(155)+M(156)+M(158)-M(160)-M(195)+M(196)+M(201)-M(202)+M(225)-M(226)-M(249) &
    +M(250))) * den(614)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,70)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(614)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,71)
  Gcoeff = (c(6)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(614)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,72)
  Gcoeff = (c(6)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(100)
  T3sum(1:35,54) = T3sum(1:35,54) + Gcoeff * G3tensor(:,169)
  Gcoeff = (c(6)*(M(133)-M(134)-M(157)+M(158)-M(197)+M(199)+M(201)-M(202)-M(203)+M(204)+M(209)-M(210)+M(239)-M(241)-M(243) &
    +M(244))) * den(100)
  T3sum(1:35,54) = T3sum(1:35,54) + Gcoeff * G3tensor(:,170)
  Gcoeff = (c(6)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(100)
  T3sum(1:35,54) = T3sum(1:35,54) + Gcoeff * G3tensor(:,171)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(31)+M(32)+M(35)-M(36)-M(105)+M(108)-M(111)+M(119))+c(6)*(M(173)-M(175)+M(205)-M(206) &
    -M(215)+M(216)-M(233)+M(235))) * den(103)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,469)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(31)+M(32)+M(35)-M(36)-M(105)+M(108)-M(111)+M(119))+c(6)*(M(139)-M(140)+M(177)-M(178) &
    -M(181)+M(182)-M(237)+M(238))) * den(103)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,470)
  Gcoeff = (c(6)*(M(139)-M(140)-M(173)+M(175)+M(177)-M(178)-M(181)+M(182)-M(205)+M(206)+M(215)-M(216)+M(233)-M(235)-M(237) &
    +M(238))) * den(103)
  T3sum(1:15,43) = T3sum(1:15,43) + Gcoeff * G2tensor(:,471)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(31)+M(32)+M(35)-M(36)-M(105)+M(108)-M(111)+M(119))+c(6)*(M(139)-M(140)+M(177)-M(178) &
    -M(181)+M(182)-M(237)+M(238))) * den(103)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,559)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(31)+M(32)+M(35)-M(36)-M(105)+M(108)-M(111)+M(119))+c(6)*(M(138)-M(141)-M(180)+M(183) &
    +M(213)-M(214)-M(223)+M(224))) * den(103)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,560)
  Gcoeff = (c(6)*(M(138)-M(139)+M(140)-M(141)-M(177)+M(178)-M(180)+M(181)-M(182)+M(183)+M(213)-M(214)-M(223)+M(224)+M(237) &
    -M(238))) * den(103)
  T3sum(1:15,24) = T3sum(1:15,24) + Gcoeff * G2tensor(:,561)
  Gcoeff = (c(5)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(6)*(-M(137)+M(155)-M(161) &
    +M(179)-M(246)+M(247)-M(248)+M(249))) * den(234)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,394)
  Gcoeff = (c(5)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(6)*(-M(138)+M(156)-M(162) &
    +M(180)-M(222)+M(223)-M(224)+M(225))) * den(234)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,395)
  Gcoeff = (c(6)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(234)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,396)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(141)-M(183)+M(209) &
    -M(211)-M(213)+M(214)-M(229)+M(239))) * den(371)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,763)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(142)+M(167)-M(169) &
    -M(171)+M(172)-M(184)-M(230)+M(240))) * den(371)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,764)
  Gcoeff = (c(6)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(371)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,765)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)+M(44)+M(45)+M(46) &
    -M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95)-M(103)+M(105)+M(107)-M(108)-M(109)-M(110)+M(111)+M(112)+M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(141)-M(183)-M(213)+M(214))) * den(33)
  T4sum(1:35,142) = T4sum(1:35,142) + Gcoeff * G3tensor(:,341)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)-M(45)-M(46) &
    +M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95)-M(103)+M(105)+M(107)-M(108)-M(109)-M(110)+M(111)+M(112)+M(113)+M(118) &
    -M(119)-M(124))+c(6)*(-M(139)+M(181)+M(237)-M(238))) * den(33)
  T4sum(1:35,142) = T4sum(1:35,142) + Gcoeff * G3tensor(:,342)
  Gcoeff = (c(5)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(6)*(-M(139)-M(141)+M(181)+M(183) &
    +M(213)-M(214)+M(237)-M(238))) * den(33)
  T4sum(1:35,142) = T4sum(1:35,142) + Gcoeff * G3tensor(:,343)
  Gcoeff = (c(5)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(6)*(-M(137)+M(155)-M(161) &
    +M(179)-M(246)+M(247)-M(248)+M(249))) * den(234)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,179)
  Gcoeff = (c(5)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(6)*(-M(205)+M(209)-M(211) &
    +M(215)-M(229)+M(233)-M(235)+M(239))) * den(234)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,185)
  Gcoeff = (c(6)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(234)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,191)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)-M(45)+M(46) &
    +M(53)-M(56)+M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)-M(103)+M(105)+M(107)-M(108)-M(109)-M(110)+M(111)+M(112)+M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(167)-M(169)-M(230)+M(240))) * den(33)
  T4sum(1:35,112) = T4sum(1:35,112) + Gcoeff * G3tensor(:,257)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)-M(45)-M(46) &
    +M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95)-M(103)+M(105)+M(107)-M(108)-M(109)-M(110)+M(111)+M(112)+M(113)+M(118) &
    -M(119)-M(124))+c(6)*(-M(139)+M(181)+M(237)-M(238))) * den(33)
  T4sum(1:35,112) = T4sum(1:35,112) + Gcoeff * G3tensor(:,258)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(139)-M(167)+M(169)+M(181)+M(230) &
    +M(237)-M(238)-M(240))) * den(33)
  T4sum(1:35,112) = T4sum(1:35,112) + Gcoeff * G3tensor(:,259)
  Gcoeff = (c(6)*(-M(137)+M(139)+M(141)-M(142)+M(171)-M(172)+M(179)-M(181)-M(183)+M(184)-M(213)+M(214)-M(237)+M(238)+M(247) &
    -M(248))) * den(624)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,344)
  Gcoeff = (c(6)*(-M(141)+M(142)+M(167)-M(169)-M(171)+M(172)+M(183)-M(184)-M(209)+M(211)+M(213)-M(214)+M(229)-M(230)-M(239) &
    +M(240))) * den(624)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,345)
  Gcoeff = (c(6)*(M(137)-M(139)-M(167)+M(169)-M(179)+M(181)+M(209)-M(211)-M(229)+M(230)+M(237)-M(238)+M(239)-M(240)-M(247) &
    +M(248))) * den(624)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,346)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(102)+M(108)+M(119)-M(121))+c(6)*(M(153)-M(154)+M(163)-M(164) &
    -M(187)+M(188)-M(231)+M(232))) * den(106)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,598)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(102)+M(108)+M(119)-M(121))+c(6)*(M(162)-M(165)-M(186)+M(189) &
    +M(207)-M(208)-M(221)+M(222))) * den(106)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,599)
  Gcoeff = (c(6)*(-M(153)+M(154)+M(162)-M(163)+M(164)-M(165)-M(186)+M(187)-M(188)+M(189)+M(207)-M(208)-M(221)+M(222)+M(231) &
    -M(232))) * den(106)
  T3sum(1:15,45) = T3sum(1:15,45) + Gcoeff * G2tensor(:,600)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(102)+M(108)+M(119)-M(121))+c(6)*(M(149)-M(151)+M(211)-M(212) &
    -M(217)+M(218)-M(227)+M(229))) * den(106)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,265)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(102)+M(108)+M(119)-M(121))+c(6)*(M(153)-M(154)+M(163)-M(164) &
    -M(187)+M(188)-M(231)+M(232))) * den(106)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,266)
  Gcoeff = (c(6)*(-M(149)+M(151)+M(153)-M(154)+M(163)-M(164)-M(187)+M(188)-M(211)+M(212)+M(217)-M(218)+M(227)-M(229)-M(231) &
    +M(232))) * den(106)
  T3sum(1:15,22) = T3sum(1:15,22) + Gcoeff * G2tensor(:,267)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)+M(68)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121))+c(6)*(M(131)-M(137)-M(161) &
    +M(185)+M(245)-M(246)-M(248)+M(250))) * den(252)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,397)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)+M(68)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121))+c(6)*(M(132)-M(138)-M(162) &
    +M(186)+M(221)-M(222)-M(224)+M(226))) * den(252)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,398)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(252)
  T3sum(1:15,90) = T3sum(1:15,90) + Gcoeff * G2tensor(:,399)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(165)-M(189)+M(203) &
    -M(205)-M(207)+M(208)-M(235)+M(241))) * den(309)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,766)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(143)-M(145)-M(147) &
    +M(148)+M(166)-M(190)-M(236)+M(242))) * den(309)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,767)
  Gcoeff = (c(6)*(M(143)-M(145)-M(147)+M(148)-M(165)+M(166)+M(189)-M(190)-M(203)+M(205)+M(207)-M(208)+M(235)-M(236)-M(241) &
    +M(242))) * den(309)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,768)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)-M(33)+M(34)-M(41)+M(44)+M(56) &
    +M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(165)-M(189)-M(207)+M(208))) * den(12)
  T4sum(1:35,154) = T4sum(1:35,154) + Gcoeff * G3tensor(:,347)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(163)+M(187)+M(231)-M(232))) * den(12)
  T4sum(1:35,154) = T4sum(1:35,154) + Gcoeff * G3tensor(:,348)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)-M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96))+c(6)*(-M(163)-M(165)+M(187)+M(189) &
    +M(207)-M(208)+M(231)-M(232))) * den(12)
  T4sum(1:35,154) = T4sum(1:35,154) + Gcoeff * G3tensor(:,349)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)+M(68)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121))+c(6)*(M(131)-M(137)-M(161) &
    +M(185)+M(245)-M(246)-M(248)+M(250))) * den(252)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,180)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)+M(68)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121))+c(6)*(M(203)-M(205)-M(211) &
    +M(217)+M(227)-M(229)-M(235)+M(241))) * den(252)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,186)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(252)
  T3sum(1:15,65) = T3sum(1:15,65) + Gcoeff * G2tensor(:,192)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)+M(58)+M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(143)-M(145)-M(236)+M(242))) * den(12)
  T4sum(1:35,76) = T4sum(1:35,76) + Gcoeff * G3tensor(:,296)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(163)+M(187)+M(231)-M(232))) * den(12)
  T4sum(1:35,76) = T4sum(1:35,76) + Gcoeff * G3tensor(:,297)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(6)*(-M(143)+M(145)-M(163)+M(187)+M(231) &
    -M(232)+M(236)-M(242))) * den(12)
  T4sum(1:35,76) = T4sum(1:35,76) + Gcoeff * G3tensor(:,298)
  Gcoeff = (c(6)*(M(147)-M(148)-M(161)+M(163)+M(165)-M(166)+M(185)-M(187)-M(189)+M(190)-M(207)+M(208)-M(231)+M(232)+M(245) &
    -M(246))) * den(633)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,350)
  Gcoeff = (c(6)*(M(143)-M(145)-M(147)+M(148)-M(165)+M(166)+M(189)-M(190)-M(203)+M(205)+M(207)-M(208)+M(235)-M(236)-M(241) &
    +M(242))) * den(633)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,351)
  Gcoeff = (c(6)*(-M(143)+M(145)+M(161)-M(163)-M(185)+M(187)+M(203)-M(205)+M(231)-M(232)-M(235)+M(236)+M(241)-M(242)-M(245) &
    +M(246))) * den(633)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,352)
  Gcoeff = (c(5)*(-M(50)+M(55)-M(62)+M(89)+M(102)+M(104)-M(108)-M(114)-M(119)-M(120)+M(121)+M(123))+c(6)*(-M(149)+M(159)-M(173) &
    -M(216)+M(217)-M(218)+M(219)+M(227))) * den(224)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,769)
  Gcoeff = (c(5)*(-M(50)+M(55)-M(62)+M(89)+M(102)+M(104)-M(108)-M(114)-M(119)-M(120)+M(121)+M(123))+c(6)*(-M(150)+M(160)-M(174) &
    -M(192)+M(193)-M(194)+M(195)+M(228))) * den(224)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,770)
  Gcoeff = (c(6)*(M(149)-M(150)-M(159)+M(160)+M(173)-M(174)-M(192)+M(193)-M(194)+M(195)+M(216)-M(217)+M(218)-M(219)-M(227) &
    +M(228))) * den(224)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,771)
  Gcoeff = (c(5)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(6)*(-M(151)+M(183)-M(197) &
    -M(210)+M(211)-M(212)+M(213)+M(229))) * den(181)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,772)
  Gcoeff = (c(5)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(6)*(-M(152)-M(168)+M(169) &
    -M(170)+M(171)+M(184)-M(198)+M(230))) * den(181)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,773)
  Gcoeff = (c(6)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(181)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,774)
  Gcoeff = (c(4)*(M(5)-M(6)-M(11)+M(12)-M(17)+M(18)-M(19)+M(20)+M(21)-M(22)+M(23)-M(24)-M(25)+M(26)+M(29)-M(30)+M(50)+M(51)+M(52) &
    -M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91)-M(102)-M(104)+M(108)+M(110)+M(114)-M(116)+M(119)+M(120)-M(121)-M(122) &
    -M(123)+M(124))+c(6)*(M(150)-M(193)+M(194)-M(228))) * den(37)
  T4sum(1:35,138) = T4sum(1:35,138) + Gcoeff * G3tensor(:,145)
  Gcoeff = (c(4)*(M(5)-M(6)-M(11)+M(12)-M(17)+M(18)-M(19)+M(20)+M(21)-M(22)+M(23)-M(24)-M(25)+M(26)+M(29)-M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)-M(102)-M(104)+M(108)+M(110)+M(114)-M(116)+M(119)+M(120)-M(121)-M(122) &
    -M(123)+M(124))+c(6)*(-M(152)+M(169)-M(170)+M(230))) * den(37)
  T4sum(1:35,138) = T4sum(1:35,138) + Gcoeff * G3tensor(:,146)
  Gcoeff = (c(5)*(-M(50)-M(51)-M(52)+M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91))+c(6)*(-M(150)-M(152)+M(169)-M(170) &
    +M(193)-M(194)+M(228)+M(230))) * den(37)
  T4sum(1:35,138) = T4sum(1:35,138) + Gcoeff * G3tensor(:,147)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)+M(92)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120))+c(6)*(M(135)-M(149)-M(173) &
    +M(215)-M(216)-M(218)+M(220)+M(233))) * den(227)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,775)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)+M(92)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120))+c(6)*(M(136)-M(150)-M(174) &
    +M(191)-M(192)-M(194)+M(196)+M(234))) * den(227)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,776)
  Gcoeff = (c(6)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(227)
  T3sum(1:15,88) = T3sum(1:15,88) + Gcoeff * G2tensor(:,777)
  Gcoeff = (c(5)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(6)*(-M(175)+M(189)-M(199) &
    -M(204)+M(205)-M(206)+M(207)+M(235))) * den(157)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,778)
  Gcoeff = (c(5)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(6)*(-M(144)+M(145)-M(146) &
    +M(147)-M(176)+M(190)-M(200)+M(236))) * den(157)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,779)
  Gcoeff = (c(6)*(-M(144)+M(145)-M(146)+M(147)+M(175)-M(176)-M(189)+M(190)+M(199)-M(200)+M(204)-M(205)+M(206)-M(207)-M(235) &
    +M(236))) * den(157)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,780)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)-M(43)+M(50)+M(62) &
    +M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(174)-M(191)+M(192)-M(234))) * den(16)
  T4sum(1:35,150) = T4sum(1:35,150) + Gcoeff * G3tensor(:,148)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(145)-M(146)-M(176)+M(236))) * den(16)
  T4sum(1:35,150) = T4sum(1:35,150) + Gcoeff * G3tensor(:,149)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)-M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94))+c(6)*(M(145)-M(146)-M(174)-M(176) &
    +M(191)-M(192)+M(234)+M(236))) * den(16)
  T4sum(1:35,150) = T4sum(1:35,150) + Gcoeff * G3tensor(:,150)
  Gcoeff = (c(5)*(M(46)-M(51)-M(74)+M(95)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122))+c(6)*(M(141)-M(151)-M(197) &
    +M(209)-M(210)-M(212)+M(214)+M(239))) * den(184)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,781)
  Gcoeff = (c(5)*(M(46)-M(51)-M(74)+M(95)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122))+c(6)*(M(142)-M(152)+M(167) &
    -M(168)-M(170)+M(172)-M(198)+M(240))) * den(184)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,782)
  Gcoeff = (c(6)*(-M(141)+M(142)+M(151)-M(152)+M(167)-M(168)-M(170)+M(172)+M(197)-M(198)-M(209)+M(210)+M(212)-M(214)-M(239) &
    +M(240))) * den(184)
  T3sum(1:15,86) = T3sum(1:15,86) + Gcoeff * G2tensor(:,783)
  Gcoeff = (c(5)*(M(58)-M(63)-M(75)+M(96)+M(102)-M(105)-M(111)+M(121)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(165)-M(175)-M(199) &
    +M(203)-M(204)-M(206)+M(208)+M(241))) * den(160)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,784)
  Gcoeff = (c(5)*(M(58)-M(63)-M(75)+M(96)+M(102)-M(105)-M(111)+M(121)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(143)-M(144)-M(146) &
    +M(148)+M(166)-M(176)-M(200)+M(242))) * den(160)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,785)
  Gcoeff = (c(6)*(M(143)-M(144)-M(146)+M(148)-M(165)+M(166)+M(175)-M(176)+M(199)-M(200)-M(203)+M(204)+M(206)-M(208)-M(241) &
    +M(242))) * den(160)
  T3sum(1:15,79) = T3sum(1:15,79) + Gcoeff * G2tensor(:,786)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)-M(46)+M(51)-M(58) &
    +M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(167)+M(168)+M(198)-M(240))) * den(22)
  T4sum(1:35,168) = T4sum(1:35,168) + Gcoeff * G3tensor(:,52)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(143)-M(144)-M(200)+M(242))) * den(22)
  T4sum(1:35,168) = T4sum(1:35,168) + Gcoeff * G3tensor(:,53)
  Gcoeff = (c(5)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(6)*(M(143)-M(144)+M(167)-M(168) &
    -M(198)-M(200)+M(240)+M(242))) * den(22)
  T4sum(1:35,168) = T4sum(1:35,168) + Gcoeff * G3tensor(:,54)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(43)+M(46)+M(47)+M(48)+M(49)+M(58)+M(59)+M(68)+M(69)+M(71)+M(81)+M(82)+M(85)+M(92)+M(94)+M(95)+M(96)+M(97)+M(98) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(143)+M(242)))
  T5sum(1:70,131) = T5sum(1:70,131) + Gcoeff * G4tensor(:,22)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(5)-M(8)-M(10)-M(11)-M(13)-M(15)-M(18)-M(20)-M(22)-M(23)-M(25)-M(28)-M(30)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(47)+M(48)+M(55)+M(59)+M(61)+M(67)+M(68)+M(69)+M(70)+M(71)+M(73)+M(79)+M(81)+M(89)+M(90)+M(91)+M(93)+M(99) &
    +M(100)+M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(187)+M(231)))
  T5sum(1:70,131) = T5sum(1:70,131) + Gcoeff * G4tensor(:,23)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(7)-M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(25)+M(26)+M(29)-M(30)-M(43)-M(46)-M(49) &
    +M(55)-M(58)+M(61)+M(67)+M(70)+M(73)+M(79)-M(82)-M(85)+M(89)+M(90)+M(91)-M(92)+M(93)-M(94)-M(95)-M(96)-M(97)-M(98)+M(99) &
    +M(100))+c(6)*(-M(143)+M(187)+M(231)-M(242)))
  T5sum(1:70,131) = T5sum(1:70,131) + Gcoeff * G4tensor(:,24)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(43)+M(44)+M(46)+M(47)+M(48)+M(49)+M(56)+M(57)+M(59)+M(70)+M(71)+M(82)+M(84)+M(85)+M(92)+M(93)+M(94)+M(95)+M(97)+M(98) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(145)+M(236)))
  T5sum(1:70,132) = T5sum(1:70,132) + Gcoeff * G4tensor(:,59)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(10)-M(11)-M(13)-M(15)-M(17)-M(20)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(44)+M(47)+M(48)+M(55)+M(56)+M(57)+M(58)+M(59)+M(61)+M(67)+M(71)+M(73)+M(79)+M(84)+M(89)+M(90)+M(91)+M(96)+M(99) &
    +M(100)+M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(163)+M(232)))
  T5sum(1:70,132) = T5sum(1:70,132) + Gcoeff * G4tensor(:,60)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(7)+M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)-M(43)-M(46)-M(49)+M(55) &
    +M(58)+M(61)+M(67)-M(70)+M(73)+M(79)-M(82)-M(85)+M(89)+M(90)+M(91)-M(92)-M(93)-M(94)-M(95)+M(96)-M(97)-M(98)+M(99)+M(100)) &
    +c(6)*(-M(145)+M(163)+M(232)-M(236)))
  T5sum(1:70,132) = T5sum(1:70,132) + Gcoeff * G4tensor(:,61)
  Gcoeff = (c(5)*(-M(50)+M(55)-M(62)+M(89)+M(102)+M(104)-M(108)-M(114)-M(119)-M(120)+M(121)+M(123))+c(6)*(-M(183)+M(186)-M(189) &
    +M(197)-M(207)+M(210)-M(213)+M(221))) * den(224)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,787)
  Gcoeff = (c(5)*(-M(50)+M(55)-M(62)+M(89)+M(102)+M(104)-M(108)-M(114)-M(119)-M(120)+M(121)+M(123))+c(6)*(-M(150)+M(160)-M(174) &
    -M(192)+M(193)-M(194)+M(195)+M(228))) * den(224)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,788)
  Gcoeff = (c(6)*(-M(150)+M(160)-M(174)+M(183)-M(186)+M(189)-M(192)+M(193)-M(194)+M(195)-M(197)+M(207)-M(210)+M(213)-M(221) &
    +M(228))) * den(224)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,789)
  Gcoeff = (c(4)*(M(5)-M(6)-M(11)+M(12)-M(17)+M(18)-M(19)+M(20)+M(21)-M(22)+M(23)-M(24)-M(25)+M(26)+M(29)-M(30)+M(50)+M(51)+M(52) &
    -M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91)-M(102)-M(104)+M(108)+M(110)+M(114)-M(116)+M(119)+M(120)-M(121)-M(122) &
    -M(123)+M(124))+c(6)*(M(150)-M(193)+M(194)-M(228))) * den(37)
  T4sum(1:35,107) = T4sum(1:35,107) + Gcoeff * G3tensor(:,26)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(11)+M(12)+M(17)-M(18)+M(19)-M(20)+M(21)-M(22)-M(23)+M(24)-M(25)+M(26)+M(29)-M(30)+M(50)-M(51)+M(52) &
    -M(55)+M(62)+M(67)-M(74)-M(79)+M(86)-M(89)+M(90)-M(91)-M(102)-M(104)+M(108)+M(110)+M(114)-M(116)+M(119)+M(120)-M(121)-M(122) &
    -M(123)+M(124))+c(6)*(M(183)-M(197)-M(210)+M(213))) * den(37)
  T4sum(1:35,107) = T4sum(1:35,107) + Gcoeff * G3tensor(:,29)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(150)+M(183)+M(193)-M(194) &
    -M(197)-M(210)+M(213)+M(228))) * den(37)
  T4sum(1:35,107) = T4sum(1:35,107) + Gcoeff * G3tensor(:,32)
  Gcoeff = (c(6)*(-M(150)+M(153)+M(168)-M(171)+M(183)-M(184)-M(187)+M(188)+M(193)-M(194)-M(197)+M(198)-M(210)+M(213)+M(228) &
    -M(231))) * den(653)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,354)
  Gcoeff = (c(6)*(M(151)-M(152)-M(168)+M(169)-M(170)+M(171)-M(183)+M(184)+M(197)-M(198)+M(210)-M(211)+M(212)-M(213)-M(229) &
    +M(230))) * den(653)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,355)
  Gcoeff = (c(6)*(M(150)-M(151)+M(152)-M(153)-M(169)+M(170)+M(187)-M(188)-M(193)+M(194)+M(211)-M(212)-M(228)+M(229)-M(230) &
    +M(231))) * den(653)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,356)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)+M(92)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120))+c(6)*(M(180)-M(183)-M(189) &
    +M(199)+M(204)-M(207)-M(213)+M(223))) * den(227)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,790)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)+M(92)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120))+c(6)*(M(136)-M(150)-M(174) &
    +M(191)-M(192)-M(194)+M(196)+M(234))) * den(227)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,791)
  Gcoeff = (c(6)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(227)
  T3sum(1:15,63) = T3sum(1:15,63) + Gcoeff * G2tensor(:,792)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)-M(43)+M(50)+M(62) &
    +M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(174)-M(191)+M(192)-M(234))) * den(16)
  T4sum(1:35,71) = T4sum(1:35,71) + Gcoeff * G3tensor(:,27)
  Gcoeff = (c(4)*(M(5)-M(6)-M(9)+M(10)+M(15)-M(16)-M(19)+M(20)-M(27)+M(28)+M(31)-M(32)+M(33)-M(34)-M(35)+M(36)-M(43)+M(50)+M(62) &
    -M(63)+M(64)+M(70)-M(75)-M(82)+M(87)-M(92)+M(93)-M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(189)-M(199)-M(204)+M(207))) * den(16)
  T4sum(1:35,71) = T4sum(1:35,71) + Gcoeff * G3tensor(:,30)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(174)+M(189)+M(191)-M(192)-M(199) &
    -M(204)+M(207)+M(234))) * den(16)
  T4sum(1:35,71) = T4sum(1:35,71) + Gcoeff * G3tensor(:,33)
  Gcoeff = (c(6)*(M(144)-M(147)-M(174)+M(177)-M(181)+M(182)+M(189)-M(190)+M(191)-M(192)-M(199)+M(200)-M(204)+M(207)+M(234) &
    -M(237))) * den(644)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,358)
  Gcoeff = (c(6)*(-M(144)+M(145)-M(146)+M(147)+M(175)-M(176)-M(189)+M(190)+M(199)-M(200)+M(204)-M(205)+M(206)-M(207)-M(235) &
    +M(236))) * den(644)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,359)
  Gcoeff = (c(6)*(-M(145)+M(146)+M(174)-M(175)+M(176)-M(177)+M(181)-M(182)-M(191)+M(192)+M(205)-M(206)-M(234)+M(235)-M(236) &
    +M(237))) * den(644)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,360)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(5)-M(8)-M(10)-M(11)-M(13)-M(15)-M(18)-M(20)-M(22)-M(23)-M(25)-M(28)-M(30)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(47)+M(48)+M(55)+M(59)+M(61)+M(67)+M(68)+M(69)+M(70)+M(71)+M(73)+M(79)+M(81)+M(89)+M(90)+M(91)+M(93)+M(99) &
    +M(100)+M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(187)+M(231)))
  T5sum(1:70,151) = T5sum(1:70,151) + Gcoeff * G4tensor(:,34)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(47)+M(51)+M(53)+M(55)+M(59)+M(60)+M(61)+M(63)+M(65)+M(66)+M(72)+M(74)+M(75)+M(76)+M(78)+M(79)+M(88)+M(89)+M(91)+M(99) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(168)+M(198)))
  T5sum(1:70,151) = T5sum(1:70,151) + Gcoeff * G4tensor(:,35)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)-M(41)-M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)+M(66)-M(67)-M(68)-M(69)-M(70)-M(71)+M(72)-M(73)+M(74)+M(75)+M(76)+M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(168)-M(187)+M(198)-M(231)))
  T5sum(1:70,151) = T5sum(1:70,151) + Gcoeff * G4tensor(:,36)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(48)+M(50)+M(53)+M(54)+M(60)+M(62)+M(63)+M(64)+M(65)+M(67)+M(71)+M(72)+M(73)+M(75)+M(77)+M(79)+M(87)+M(90)+M(91) &
    +M(100)+M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(174)+M(192)))
  T5sum(1:70,152) = T5sum(1:70,152) + Gcoeff * G4tensor(:,13)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(6)-M(7)-M(9)-M(12)-M(14)-M(16)-M(17)-M(19)-M(21)-M(24)-M(26)-M(27)-M(29)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(41)+M(50)+M(51)+M(54)+M(62)+M(64)+M(66)+M(68)+M(69)+M(70)+M(74)+M(76)+M(77)+M(78)+M(79)+M(81)+M(87)+M(88)+M(91)+M(93) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(189)+M(207)))
  T5sum(1:70,152) = T5sum(1:70,152) + Gcoeff * G4tensor(:,14)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(41)-M(48)+M(51) &
    -M(53)-M(60)-M(63)-M(65)+M(66)-M(67)+M(68)+M(69)+M(70)-M(71)-M(72)-M(73)+M(74)-M(75)+M(76)+M(78)+M(81)+M(88)-M(90)+M(93) &
    -M(100))+c(6)*(-M(174)+M(189)-M(192)+M(207)))
  T5sum(1:70,152) = T5sum(1:70,152) + Gcoeff * G4tensor(:,15)
  Gcoeff = (c(6)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(655)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,361)
  Gcoeff = (c(6)*(-M(133)+M(134)+M(143)-M(144)+M(167)-M(168)+M(197)-M(198)-M(200)+M(202)-M(209)+M(210)-M(239)+M(240)+M(242) &
    -M(244))) * den(655)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,362)
  Gcoeff = (c(6)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(655)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,363)
  Gcoeff = (c(6)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(648)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,364)
  Gcoeff = (c(6)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(648)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,365)
  Gcoeff = (c(6)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(648)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,366)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(144)+M(168)-M(181)-M(187) &
    +M(198)+M(200)-M(231)-M(237))) * den(9)
  T4sum(1:70,168) = T4sum(1:70,168) + Gcoeff * G4tensor(:,37)
  Gcoeff = (c(5)*(M(46)-M(51)+M(58)-M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97))+c(6)*(M(143)-M(144)+M(167)-M(168) &
    -M(198)-M(200)+M(240)+M(242))) * den(9)
  T4sum(1:70,168) = T4sum(1:70,168) + Gcoeff * G4tensor(:,38)
  Gcoeff = (c(5)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(6)*(-M(143)-M(167)+M(181)+M(187) &
    +M(231)+M(237)-M(240)-M(242))) * den(9)
  T4sum(1:70,168) = T4sum(1:70,168) + Gcoeff * G4tensor(:,39)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(156)-M(162)-M(173) &
    +M(175)+M(206)-M(216)-M(222)+M(225))) * den(371)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,793)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(142)+M(167)-M(169) &
    -M(171)+M(172)-M(184)-M(230)+M(240))) * den(371)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,794)
  Gcoeff = (c(6)*(M(142)-M(156)+M(162)+M(167)-M(169)-M(171)+M(172)+M(173)-M(175)-M(184)-M(206)+M(216)+M(222)-M(225)-M(230) &
    +M(240))) * den(371)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,795)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)-M(9)+M(10)-M(11)+M(12)+M(15)-M(16)+M(21)-M(22)+M(31)-M(32)-M(35)+M(36)+M(44)-M(45)+M(46) &
    -M(53)+M(56)-M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)-M(103)+M(105)+M(107)-M(108)-M(109)-M(110)+M(111)+M(112)+M(113)+M(118) &
    -M(119)-M(124))+c(6)*(-M(173)+M(175)+M(206)-M(216))) * den(33)
  T4sum(1:35,126) = T4sum(1:35,126) + Gcoeff * G3tensor(:,260)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(44)-M(45)+M(46) &
    +M(53)-M(56)+M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)-M(103)+M(105)+M(107)-M(108)-M(109)-M(110)+M(111)+M(112)+M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(167)-M(169)-M(230)+M(240))) * den(33)
  T4sum(1:35,126) = T4sum(1:35,126) + Gcoeff * G3tensor(:,261)
  Gcoeff = (c(5)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(44)+M(53)-M(56)+M(65))+c(6)*(M(167)-M(169)+M(173)-M(175)-M(206) &
    +M(216)-M(230)+M(240))) * den(33)
  T4sum(1:35,126) = T4sum(1:35,126) + Gcoeff * G3tensor(:,262)
  Gcoeff = (c(6)*(-M(140)+M(142)+M(155)-M(156)-M(161)+M(162)-M(171)+M(172)+M(177)-M(178)+M(182)-M(184)+M(222)-M(225)-M(246) &
    +M(249))) * den(666)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,73)
  Gcoeff = (c(6)*(M(137)-M(138)-M(155)+M(156)+M(161)-M(162)-M(179)+M(180)-M(222)+M(223)-M(224)+M(225)+M(246)-M(247)+M(248) &
    -M(249))) * den(666)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,74)
  Gcoeff = (c(6)*(-M(137)+M(138)+M(140)-M(142)+M(171)-M(172)-M(177)+M(178)+M(179)-M(180)-M(182)+M(184)-M(223)+M(224)+M(247) &
    -M(248))) * den(666)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,75)
  Gcoeff = (c(6)*(M(140)-M(155)+M(161)+M(167)-M(169)+M(173)-M(175)-M(177)+M(178)-M(182)-M(206)+M(216)-M(230)+M(240)+M(246) &
    -M(249))) * den(115)
  T3sum(1:35,43) = T3sum(1:35,43) + Gcoeff * G3tensor(:,163)
  Gcoeff = (c(6)*(M(139)-M(140)-M(173)+M(175)+M(177)-M(178)-M(181)+M(182)-M(205)+M(206)+M(215)-M(216)+M(233)-M(235)-M(237) &
    +M(238))) * den(115)
  T3sum(1:35,43) = T3sum(1:35,43) + Gcoeff * G3tensor(:,164)
  Gcoeff = (c(6)*(-M(139)+M(155)-M(161)-M(167)+M(169)+M(181)+M(205)-M(215)+M(230)-M(233)+M(235)+M(237)-M(238)-M(240)-M(246) &
    +M(249))) * den(115)
  T3sum(1:35,43) = T3sum(1:35,43) + Gcoeff * G3tensor(:,165)
  Gcoeff = (c(5)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(6)*(-M(159)+M(162)-M(165) &
    +M(173)-M(208)+M(216)-M(219)+M(222))) * den(181)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,796)
  Gcoeff = (c(5)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(6)*(-M(152)-M(168)+M(169) &
    -M(170)+M(171)+M(184)-M(198)+M(230))) * den(181)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,797)
  Gcoeff = (c(6)*(-M(152)+M(159)-M(162)+M(165)-M(168)+M(169)-M(170)+M(171)-M(173)+M(184)-M(198)+M(208)-M(216)+M(219)-M(222) &
    +M(230))) * den(181)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,798)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)+M(17)-M(18)-M(19)+M(20)-M(21)+M(22)-M(23)+M(24)-M(25)+M(26)+M(29)-M(30)+M(50)-M(51)-M(52) &
    -M(55)+M(62)+M(67)-M(74)+M(79)-M(86)-M(89)+M(90)+M(91)-M(102)-M(104)+M(108)+M(110)+M(114)-M(116)+M(119)+M(120)-M(121)-M(122) &
    -M(123)+M(124))+c(6)*(-M(159)+M(173)+M(216)-M(219))) * den(37)
  T4sum(1:35,119) = T4sum(1:35,119) + Gcoeff * G3tensor(:,151)
  Gcoeff = (c(4)*(M(5)-M(6)-M(11)+M(12)-M(17)+M(18)-M(19)+M(20)+M(21)-M(22)+M(23)-M(24)-M(25)+M(26)+M(29)-M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)-M(102)-M(104)+M(108)+M(110)+M(114)-M(116)+M(119)+M(120)-M(121)-M(122) &
    -M(123)+M(124))+c(6)*(-M(152)+M(169)-M(170)+M(230))) * den(37)
  T4sum(1:35,119) = T4sum(1:35,119) + Gcoeff * G3tensor(:,152)
  Gcoeff = (c(5)*(-M(11)+M(12)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(50)+M(55)-M(62)+M(89))+c(6)*(-M(152)+M(159)+M(169)-M(170) &
    -M(173)-M(216)+M(219)+M(230))) * den(37)
  T4sum(1:35,119) = T4sum(1:35,119) + Gcoeff * G3tensor(:,153)
  Gcoeff = (c(6)*(-M(152)+M(154)+M(159)-M(160)-M(163)+M(164)+M(169)-M(170)-M(173)+M(174)+M(192)-M(195)-M(216)+M(219)+M(230) &
    -M(232))) * den(683)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,369)
  Gcoeff = (c(6)*(M(149)-M(150)-M(159)+M(160)+M(173)-M(174)-M(192)+M(193)-M(194)+M(195)+M(216)-M(217)+M(218)-M(219)-M(227) &
    +M(228))) * den(683)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,370)
  Gcoeff = (c(6)*(-M(149)+M(150)+M(152)-M(154)+M(163)-M(164)-M(169)+M(170)-M(193)+M(194)+M(217)-M(218)+M(227)-M(228)-M(230) &
    +M(232))) * den(683)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,371)
  Gcoeff = (c(5)*(M(46)-M(51)-M(74)+M(95)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122))+c(6)*(M(156)-M(159)-M(165) &
    +M(175)+M(206)-M(208)-M(219)+M(225))) * den(184)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,799)
  Gcoeff = (c(5)*(M(46)-M(51)-M(74)+M(95)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122))+c(6)*(M(142)-M(152)+M(167) &
    -M(168)-M(170)+M(172)-M(198)+M(240))) * den(184)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,800)
  Gcoeff = (c(6)*(M(142)-M(152)-M(156)+M(159)+M(165)+M(167)-M(168)-M(170)+M(172)-M(175)-M(198)-M(206)+M(208)+M(219)-M(225) &
    +M(240))) * den(184)
  T3sum(1:15,74) = T3sum(1:15,74) + Gcoeff * G2tensor(:,801)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)-M(46)+M(51)-M(58) &
    +M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(167)+M(168)+M(198)-M(240))) * den(22)
  T4sum(1:35,59) = T4sum(1:35,59) + Gcoeff * G3tensor(:,22)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(11)-M(12)+M(13)-M(14)-M(21)+M(22)+M(27)-M(28)-M(33)+M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(58) &
    -M(63)+M(74)-M(75)+M(76)-M(85)+M(88)-M(95)+M(96)-M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(165)-M(175)-M(206)+M(208))) * den(22)
  T4sum(1:35,59) = T4sum(1:35,59) + Gcoeff * G3tensor(:,23)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(37)-M(38)-M(39)+M(40)+M(58)-M(63)-M(75)+M(96))+c(6)*(M(165)+M(167)-M(168)-M(175) &
    -M(198)-M(206)+M(208)+M(240))) * den(22)
  T4sum(1:35,59) = T4sum(1:35,59) + Gcoeff * G3tensor(:,24)
  Gcoeff = (c(6)*(M(146)-M(148)-M(157)+M(158)+M(165)-M(166)+M(167)-M(168)-M(175)+M(176)-M(198)+M(201)-M(206)+M(208)+M(240) &
    -M(243))) * den(674)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,373)
  Gcoeff = (c(6)*(M(143)-M(144)-M(146)+M(148)-M(165)+M(166)+M(175)-M(176)+M(199)-M(200)-M(203)+M(204)+M(206)-M(208)-M(241) &
    +M(242))) * den(674)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,374)
  Gcoeff = (c(6)*(-M(143)+M(144)+M(157)-M(158)-M(167)+M(168)+M(198)-M(199)+M(200)-M(201)+M(203)-M(204)-M(240)+M(241)-M(242) &
    +M(243))) * den(674)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,375)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(10)-M(11)-M(13)-M(15)-M(17)-M(20)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(44)+M(47)+M(48)+M(55)+M(56)+M(57)+M(58)+M(59)+M(61)+M(67)+M(71)+M(73)+M(79)+M(84)+M(89)+M(90)+M(91)+M(96)+M(99) &
    +M(100)+M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(163)+M(232)))
  T5sum(1:70,163) = T5sum(1:70,163) + Gcoeff * G4tensor(:,73)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(48)+M(50)+M(53)+M(54)+M(60)+M(62)+M(63)+M(64)+M(65)+M(67)+M(71)+M(72)+M(73)+M(75)+M(77)+M(79)+M(87)+M(90)+M(91) &
    +M(100)+M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(174)+M(192)))
  T5sum(1:70,163) = T5sum(1:70,163) + Gcoeff * G4tensor(:,74)
  Gcoeff = (c(4)*(-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)+M(37)-M(38)-M(39)+M(40)-M(44)-M(47) &
    +M(50)+M(53)+M(54)-M(55)-M(56)-M(57)-M(58)-M(59)+M(60)-M(61)+M(62)+M(63)+M(64)+M(65)+M(72)+M(75)+M(77)-M(84)+M(87)-M(89)-M(96) &
    -M(99))+c(6)*(-M(163)+M(174)+M(192)-M(232)))
  T5sum(1:70,163) = T5sum(1:70,163) + Gcoeff * G4tensor(:,75)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(47)+M(51)+M(53)+M(55)+M(59)+M(60)+M(61)+M(63)+M(65)+M(66)+M(72)+M(74)+M(75)+M(76)+M(78)+M(79)+M(88)+M(89)+M(91)+M(99) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(168)+M(198)))
  T5sum(1:70,164) = T5sum(1:70,164) + Gcoeff * G4tensor(:,10)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(9)-M(12)-M(14)-M(16)-M(18)-M(19)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(44)+M(50)+M(51)+M(54)+M(56)+M(57)+M(58)+M(62)+M(64)+M(66)+M(74)+M(76)+M(77)+M(78)+M(79)+M(84)+M(87)+M(88)+M(91)+M(96) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(165)+M(208)))
  T5sum(1:70,164) = T5sum(1:70,164) + Gcoeff * G4tensor(:,11)
  Gcoeff = (c(4)*(-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)+M(37)-M(38)-M(39)+M(40)+M(44)-M(47) &
    +M(50)-M(53)+M(54)-M(55)+M(56)+M(57)+M(58)-M(59)-M(60)-M(61)+M(62)-M(63)+M(64)-M(65)-M(72)-M(75)+M(77)+M(84)+M(87)-M(89)+M(96) &
    -M(99))+c(6)*(M(165)-M(168)-M(198)+M(208)))
  T5sum(1:70,164) = T5sum(1:70,164) + Gcoeff * G4tensor(:,12)
  Gcoeff = (c(6)*(-M(140)+M(146)+M(155)-M(157)-M(163)+M(169)-M(173)+M(174)+M(176)-M(178)+M(192)-M(216)+M(230)-M(232)-M(243) &
    +M(249))) * den(685)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,376)
  Gcoeff = (c(6)*(-M(139)+M(140)+M(145)-M(146)+M(173)-M(174)-M(176)+M(178)+M(191)-M(192)-M(215)+M(216)-M(233)+M(234)+M(236) &
    -M(238))) * den(685)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,377)
  Gcoeff = (c(6)*(M(139)-M(145)-M(155)+M(157)+M(163)-M(169)-M(191)+M(215)-M(230)+M(232)+M(233)-M(234)-M(236)+M(238)+M(243) &
    -M(249))) * den(685)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,378)
  Gcoeff = (c(6)*(M(146)-M(157)+M(161)-M(163)+M(167)+M(174)-M(175)+M(176)-M(177)-M(182)+M(192)-M(206)-M(232)+M(240)-M(243) &
    +M(246))) * den(678)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,379)
  Gcoeff = (c(6)*(M(145)-M(146)-M(174)+M(175)-M(176)+M(177)-M(181)+M(182)+M(191)-M(192)-M(205)+M(206)+M(234)-M(235)+M(236) &
    -M(237))) * den(678)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,380)
  Gcoeff = (c(6)*(-M(145)+M(157)-M(161)+M(163)-M(167)+M(181)-M(191)+M(205)+M(232)-M(234)+M(235)-M(236)+M(237)-M(240)+M(243) &
    -M(246))) * den(678)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,381)
  Gcoeff = (c(5)*(M(50)-M(55)-M(58)-M(61)+M(62)+M(63)+M(64)+M(75)+M(87)-M(89)-M(96)-M(99))+c(6)*(M(146)-M(157)-M(163)+M(174) &
    +M(176)+M(192)-M(232)-M(243))) * den(7)
  T4sum(1:70,150) = T4sum(1:70,150) + Gcoeff * G4tensor(:,76)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)-M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94))+c(6)*(M(145)-M(146)-M(174)-M(176) &
    +M(191)-M(192)+M(234)+M(236))) * den(7)
  T4sum(1:70,150) = T4sum(1:70,150) + Gcoeff * G4tensor(:,77)
  Gcoeff = (c(5)*(-M(43)+M(55)+M(58)+M(61)-M(70)-M(82)+M(89)-M(92)-M(93)-M(94)+M(96)+M(99))+c(6)*(-M(145)+M(157)+M(163)-M(191) &
    +M(232)-M(234)-M(236)+M(243))) * den(7)
  T4sum(1:70,150) = T4sum(1:70,150) + Gcoeff * G4tensor(:,78)
  Gcoeff = (c(6)*(-M(154)+M(160)-M(162)+M(163)-M(164)+M(165)-M(168)+M(171)-M(174)+M(184)-M(192)+M(195)-M(198)+M(208)-M(222) &
    +M(232))) * den(124)
  T3sum(1:35,45) = T3sum(1:35,45) + Gcoeff * G3tensor(:,197)
  Gcoeff = (c(6)*(-M(153)+M(154)+M(162)-M(163)+M(164)-M(165)-M(186)+M(187)-M(188)+M(189)+M(207)-M(208)-M(221)+M(222)+M(231) &
    -M(232))) * den(124)
  T3sum(1:35,45) = T3sum(1:35,45) + Gcoeff * G3tensor(:,198)
  Gcoeff = (c(6)*(M(153)-M(160)+M(168)-M(171)+M(174)-M(184)+M(186)-M(187)+M(188)-M(189)+M(192)-M(195)+M(198)-M(207)+M(221) &
    -M(231))) * den(124)
  T3sum(1:35,45) = T3sum(1:35,45) + Gcoeff * G3tensor(:,199)
  Gcoeff = (c(6)*(-M(148)+M(154)+M(158)-M(160)-M(161)+M(162)+M(164)-M(166)-M(171)+M(177)+M(182)-M(184)-M(195)+M(201)+M(222) &
    -M(246))) * den(700)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,76)
  Gcoeff = (c(6)*(-M(147)+M(148)+M(153)-M(154)+M(161)-M(162)-M(164)+M(166)-M(185)+M(186)+M(188)-M(190)+M(221)-M(222)-M(245) &
    +M(246))) * den(700)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,77)
  Gcoeff = (c(6)*(M(147)-M(153)-M(158)+M(160)+M(171)-M(177)-M(182)+M(184)+M(185)-M(186)-M(188)+M(190)+M(195)-M(201)-M(221) &
    +M(245))) * den(700)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,78)
  Gcoeff = (c(6)*(-M(148)+M(158)-M(161)+M(163)+M(165)-M(166)-M(168)-M(174)+M(177)+M(182)-M(192)-M(198)+M(201)+M(208)+M(232) &
    -M(246))) * den(694)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,382)
  Gcoeff = (c(6)*(-M(147)+M(148)+M(161)-M(163)-M(165)+M(166)-M(185)+M(187)+M(189)-M(190)+M(207)-M(208)+M(231)-M(232)-M(245) &
    +M(246))) * den(694)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,383)
  Gcoeff = (c(6)*(M(147)-M(158)+M(168)+M(174)-M(177)-M(182)+M(185)-M(187)-M(189)+M(190)+M(192)+M(198)-M(201)-M(207)-M(231) &
    +M(245))) * den(694)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,384)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(163)+M(165)-M(168)-M(174) &
    -M(192)-M(198)+M(208)+M(232))) * den(5)
  T4sum(1:70,154) = T4sum(1:70,154) + Gcoeff * G4tensor(:,121)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)-M(57)-M(58)+M(68)+M(69)+M(70)+M(81)-M(84)+M(93)-M(96))+c(6)*(-M(163)-M(165)+M(187)+M(189) &
    +M(207)-M(208)+M(231)-M(232))) * den(5)
  T4sum(1:70,154) = T4sum(1:70,154) + Gcoeff * G4tensor(:,122)
  Gcoeff = (c(5)*(-M(41)+M(53)+M(60)+M(63)+M(65)-M(68)-M(69)-M(70)+M(72)+M(75)-M(81)-M(93))+c(6)*(M(168)+M(174)-M(187)-M(189) &
    +M(192)+M(198)-M(207)-M(231))) * den(5)
  T4sum(1:70,154) = T4sum(1:70,154) + Gcoeff * G4tensor(:,123)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(132)-M(138)-M(149) &
    +M(151)+M(212)-M(218)-M(224)+M(226))) * den(309)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,802)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(143)-M(145)-M(147) &
    +M(148)+M(166)-M(190)-M(236)+M(242))) * den(309)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,803)
  Gcoeff = (c(6)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(309)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,804)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    -M(57)+M(58)-M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(-M(149)+M(151)+M(212)-M(218))) * den(12)
  T4sum(1:35,99) = T4sum(1:35,99) + Gcoeff * G3tensor(:,299)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)+M(17)-M(18)-M(23)+M(24)+M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56) &
    -M(57)+M(58)+M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)+M(102)+M(107)-M(108)+M(113)-M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(143)-M(145)-M(236)+M(242))) * den(12)
  T4sum(1:35,99) = T4sum(1:35,99) + Gcoeff * G3tensor(:,300)
  Gcoeff = (c(5)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56)+M(68))+c(6)*(M(143)-M(145)+M(149)-M(151) &
    -M(212)+M(218)-M(236)+M(242))) * den(12)
  T4sum(1:35,99) = T4sum(1:35,99) + Gcoeff * G3tensor(:,301)
  Gcoeff = (c(6)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(705)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,79)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(137)-M(138)+M(161)-M(162)-M(185)+M(186)+M(221)-M(222)-M(224)+M(226)-M(245)+M(246)+M(248) &
    -M(250))) * den(705)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,80)
  Gcoeff = (c(6)*(M(147)-M(148)-M(153)+M(154)-M(161)+M(162)+M(164)-M(166)+M(185)-M(186)-M(188)+M(190)-M(221)+M(222)+M(245) &
    -M(246))) * den(705)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,81)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(143)-M(145)+M(149)-M(151)-M(153)+M(154)+M(164)-M(188)-M(212)+M(218)-M(236)+M(242)+M(248) &
    -M(250))) * den(127)
  T3sum(1:35,22) = T3sum(1:35,22) + Gcoeff * G3tensor(:,157)
  Gcoeff = (c(6)*(-M(149)+M(151)+M(153)-M(154)+M(163)-M(164)-M(187)+M(188)-M(211)+M(212)+M(217)-M(218)+M(227)-M(229)-M(231) &
    +M(232))) * den(127)
  T3sum(1:35,22) = T3sum(1:35,22) + Gcoeff * G3tensor(:,158)
  Gcoeff = (c(6)*(M(131)-M(137)-M(143)+M(145)-M(163)+M(187)+M(211)-M(217)-M(227)+M(229)+M(231)-M(232)+M(236)-M(242)-M(248) &
    +M(250))) * den(127)
  T3sum(1:35,22) = T3sum(1:35,22) + Gcoeff * G3tensor(:,159)
  Gcoeff = (c(5)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(6)*(-M(135)+M(138)-M(141) &
    +M(149)-M(214)+M(218)-M(220)+M(224))) * den(157)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,805)
  Gcoeff = (c(5)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(6)*(-M(144)+M(145)-M(146) &
    +M(147)-M(176)+M(190)-M(200)+M(236))) * den(157)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,806)
  Gcoeff = (c(6)*(M(135)-M(138)+M(141)-M(144)+M(145)-M(146)+M(147)-M(149)-M(176)+M(190)-M(200)+M(214)-M(218)+M(220)-M(224) &
    +M(236))) * den(157)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,807)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(27)-M(28)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(43)+M(50)+M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)-M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(135)+M(149)+M(218)-M(220))) * den(16)
  T4sum(1:35,92) = T4sum(1:35,92) + Gcoeff * G3tensor(:,154)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(M(145)-M(146)-M(176)+M(236))) * den(16)
  T4sum(1:35,92) = T4sum(1:35,92) + Gcoeff * G3tensor(:,155)
  Gcoeff = (c(5)*(-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62)+M(92))+c(6)*(M(135)+M(145)-M(146)-M(149) &
    -M(176)-M(218)+M(220)+M(236))) * den(16)
  T4sum(1:35,92) = T4sum(1:35,92) + Gcoeff * G3tensor(:,156)
  Gcoeff = (c(6)*(M(135)-M(136)-M(139)+M(140)+M(145)-M(146)-M(149)+M(150)-M(176)+M(178)+M(194)-M(196)-M(218)+M(220)+M(236) &
    -M(238))) * den(722)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,387)
  Gcoeff = (c(6)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(722)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,388)
  Gcoeff = (c(6)*(M(139)-M(140)-M(145)+M(146)-M(173)+M(174)+M(176)-M(178)-M(191)+M(192)+M(215)-M(216)+M(233)-M(234)-M(236) &
    +M(238))) * den(722)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,389)
  Gcoeff = (c(5)*(M(58)-M(63)-M(75)+M(96)+M(102)-M(105)-M(111)+M(121)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(132)-M(135)-M(141) &
    +M(151)+M(212)-M(214)-M(220)+M(226))) * den(160)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,808)
  Gcoeff = (c(5)*(M(58)-M(63)-M(75)+M(96)+M(102)-M(105)-M(111)+M(121)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(143)-M(144)-M(146) &
    +M(148)+M(166)-M(176)-M(200)+M(242))) * den(160)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,809)
  Gcoeff = (c(6)*(-M(132)+M(135)+M(141)+M(143)-M(144)-M(146)+M(148)-M(151)+M(166)-M(176)-M(200)-M(212)+M(214)+M(220)-M(226) &
    +M(242))) * den(160)
  T3sum(1:15,81) = T3sum(1:15,81) + Gcoeff * G2tensor(:,810)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)-M(46)+M(51)+M(58) &
    -M(63)+M(74)-M(75)-M(76)+M(85)-M(88)-M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(141)+M(151)+M(212)-M(214))) * den(22)
  T4sum(1:35,86) = T4sum(1:35,86) + Gcoeff * G3tensor(:,55)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)+M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)-M(112)+M(116)-M(118)+M(121)+M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(M(143)-M(144)-M(200)+M(242))) * den(22)
  T4sum(1:35,86) = T4sum(1:35,86) + Gcoeff * G3tensor(:,56)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(141)+M(143)-M(144)-M(151) &
    -M(200)-M(212)+M(214)+M(242))) * den(22)
  T4sum(1:35,86) = T4sum(1:35,86) + Gcoeff * G3tensor(:,57)
  Gcoeff = (c(6)*(-M(133)+M(134)+M(141)-M(142)+M(143)-M(144)-M(151)+M(152)+M(170)-M(172)-M(200)+M(202)-M(212)+M(214)+M(242) &
    -M(244))) * den(713)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,391)
  Gcoeff = (c(6)*(-M(141)+M(142)+M(151)-M(152)+M(167)-M(168)-M(170)+M(172)+M(197)-M(198)-M(209)+M(210)+M(212)-M(214)-M(239) &
    +M(240))) * den(713)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,392)
  Gcoeff = (c(6)*(M(133)-M(134)-M(143)+M(144)-M(167)+M(168)-M(197)+M(198)+M(200)-M(202)+M(209)-M(210)+M(239)-M(240)-M(242) &
    +M(244))) * den(713)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,393)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(25)-M(27)-M(30)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(44)+M(45)+M(50)+M(51)+M(52)+M(56)+M(58)+M(62)+M(69)+M(74)+M(80)+M(81)+M(82)+M(83)+M(85)+M(86)+M(94)+M(96)+M(97) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(151)+M(212)))
  T5sum(1:70,169) = T5sum(1:70,169) + Gcoeff * G4tensor(:,25)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(5)-M(7)-M(9)-M(11)-M(14)-M(16)-M(18)-M(20)-M(22)-M(23)-M(26)-M(28)-M(29)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(41)+M(43)+M(46)+M(47)+M(48)+M(49)+M(58)+M(59)+M(68)+M(69)+M(71)+M(81)+M(82)+M(85)+M(92)+M(94)+M(95)+M(96)+M(97)+M(98) &
    +M(102)+M(107)+M(113)+M(114)+M(116)+M(120)+M(121)+M(122)+M(126)+M(128))+c(6)*(M(143)+M(242)))
  T5sum(1:70,169) = T5sum(1:70,169) + Gcoeff * G4tensor(:,26)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42) &
    +M(43)-M(44)-M(45)+M(46)+M(47)+M(48)+M(49)-M(50)-M(51)-M(52)-M(56)+M(59)-M(62)+M(68)+M(71)-M(74)-M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(143)-M(151)-M(212)+M(242)))
  T5sum(1:70,169) = T5sum(1:70,169) + Gcoeff * G4tensor(:,27)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(26)-M(28)-M(29)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(45)+M(50)+M(51)+M(52)+M(57)+M(62)+M(68)+M(70)+M(74)+M(80)+M(82)+M(83)+M(84)+M(85)+M(86)+M(93)+M(94)+M(97) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(149)+M(218)))
  T5sum(1:70,171) = T5sum(1:70,171) + Gcoeff * G4tensor(:,62)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(5)-M(8)-M(9)-M(11)-M(14)-M(16)-M(17)-M(20)-M(22)-M(24)-M(25)-M(27)-M(30)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(43)+M(44)+M(46)+M(47)+M(48)+M(49)+M(56)+M(57)+M(59)+M(70)+M(71)+M(82)+M(84)+M(85)+M(92)+M(93)+M(94)+M(95)+M(97)+M(98) &
    +M(101)+M(108)+M(114)+M(115)+M(116)+M(119)+M(120)+M(122)+M(125)+M(130))+c(6)*(M(145)+M(236)))
  T5sum(1:70,171) = T5sum(1:70,171) + Gcoeff * G4tensor(:,63)
  Gcoeff = (c(4)*(-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42) &
    +M(43)+M(44)-M(45)+M(46)+M(47)+M(48)+M(49)-M(50)-M(51)-M(52)+M(56)+M(59)-M(62)-M(68)+M(71)-M(74)-M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(145)-M(149)-M(218)+M(236)))
  T5sum(1:70,171) = T5sum(1:70,171) + Gcoeff * G4tensor(:,64)
  Gcoeff = (c(6)*(M(131)-M(133)-M(139)+M(145)-M(149)+M(150)+M(152)-M(154)-M(164)+M(170)+M(194)-M(218)+M(236)-M(238)-M(244) &
    +M(250))) * den(724)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,394)
  Gcoeff = (c(6)*(M(149)-M(150)-M(152)+M(154)-M(163)+M(164)+M(169)-M(170)+M(193)-M(194)-M(217)+M(218)-M(227)+M(228)+M(230) &
    -M(232))) * den(724)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,395)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(139)-M(145)+M(163)-M(169)-M(193)+M(217)+M(227)-M(228)-M(230)+M(232)-M(236)+M(238)+M(244) &
    -M(250))) * den(724)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,396)
  Gcoeff = (c(6)*(-M(133)+M(137)-M(139)+M(143)+M(150)-M(151)+M(152)-M(153)+M(170)-M(188)+M(194)-M(212)-M(238)+M(242)-M(244) &
    +M(248))) * den(717)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,397)
  Gcoeff = (c(6)*(-M(150)+M(151)-M(152)+M(153)+M(169)-M(170)-M(187)+M(188)+M(193)-M(194)-M(211)+M(212)+M(228)-M(229)+M(230) &
    -M(231))) * den(717)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,398)
  Gcoeff = (c(6)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(717)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,399)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(133)-M(139)+M(150)+M(152) &
    +M(170)+M(194)-M(238)-M(244))) * den(4)
  T4sum(1:70,138) = T4sum(1:70,138) + Gcoeff * G4tensor(:,80)
  Gcoeff = (c(5)*(-M(50)-M(51)-M(52)+M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91))+c(6)*(-M(150)-M(152)+M(169)-M(170) &
    +M(193)-M(194)+M(228)+M(230))) * den(4)
  T4sum(1:70,138) = T4sum(1:70,138) + Gcoeff * G4tensor(:,81)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(55)-M(67)-M(79)-M(89)-M(90)-M(91)+M(92)+M(95)+M(98))+c(6)*(M(133)+M(139)-M(169)-M(193) &
    -M(228)-M(230)+M(238)+M(244))) * den(4)
  T4sum(1:70,138) = T4sum(1:70,138) + Gcoeff * G4tensor(:,82)
  Gcoeff = (c(6)*(M(136)-M(138)+M(139)-M(140)+M(141)-M(144)+M(147)-M(150)-M(178)+M(190)-M(194)+M(196)-M(200)+M(214)-M(224) &
    +M(238))) * den(136)
  T3sum(1:35,24) = T3sum(1:35,24) + Gcoeff * G3tensor(:,175)
  Gcoeff = (c(6)*(M(138)-M(139)+M(140)-M(141)-M(177)+M(178)-M(180)+M(181)-M(182)+M(183)+M(213)-M(214)-M(223)+M(224)+M(237) &
    -M(238))) * den(136)
  T3sum(1:35,24) = T3sum(1:35,24) + Gcoeff * G3tensor(:,176)
  Gcoeff = (c(6)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(136)
  T3sum(1:35,24) = T3sum(1:35,24) + Gcoeff * G3tensor(:,177)
  Gcoeff = (c(6)*(M(134)-M(136)-M(137)+M(138)+M(140)-M(142)-M(147)+M(153)-M(172)+M(178)+M(188)-M(190)-M(196)+M(202)+M(224) &
    -M(248))) * den(739)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,82)
  Gcoeff = (c(6)*(M(137)-M(138)-M(140)+M(142)-M(171)+M(172)+M(177)-M(178)-M(179)+M(180)+M(182)-M(184)+M(223)-M(224)-M(247) &
    +M(248))) * den(739)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,83)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(147)-M(153)+M(171)-M(177)+M(179)-M(180)-M(182)+M(184)-M(188)+M(190)+M(196)-M(202)-M(223) &
    +M(247))) * den(739)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,84)
  Gcoeff = (c(6)*(M(134)-M(137)+M(139)+M(141)-M(142)-M(144)-M(150)+M(153)-M(172)+M(188)-M(194)-M(200)+M(202)+M(214)+M(238) &
    -M(248))) * den(733)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,400)
  Gcoeff = (c(6)*(M(137)-M(139)-M(141)+M(142)-M(171)+M(172)-M(179)+M(181)+M(183)-M(184)+M(213)-M(214)+M(237)-M(238)-M(247) &
    +M(248))) * den(733)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,401)
  Gcoeff = (c(6)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(733)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,402)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(M(139)+M(141)-M(144)-M(150) &
    -M(194)-M(200)+M(214)+M(238))) * den(2)
  T4sum(1:70,142) = T4sum(1:70,142) + Gcoeff * G4tensor(:,124)
  Gcoeff = (c(5)*(-M(44)-M(45)-M(46)+M(53)-M(56)+M(65)+M(66)+M(67)+M(78)-M(83)+M(90)-M(95))+c(6)*(-M(139)-M(141)+M(181)+M(183) &
    +M(213)-M(214)+M(237)-M(238))) * den(2)
  T4sum(1:70,142) = T4sum(1:70,142) + Gcoeff * G4tensor(:,125)
  Gcoeff = (c(5)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(6)*(M(144)+M(150)-M(181)-M(183) &
    +M(194)+M(200)-M(213)-M(237))) * den(2)
  T4sum(1:70,142) = T4sum(1:70,142) + Gcoeff * G4tensor(:,126)
  Gcoeff = (c(6)*(-M(132)+M(133)-M(134)+M(135)+M(142)-M(146)+M(148)-M(152)+M(166)-M(170)+M(172)-M(176)-M(202)+M(220)-M(226) &
    +M(244))) * den(139)
  T3sum(1:35,25) = T3sum(1:35,25) + Gcoeff * G3tensor(:,187)
  Gcoeff = (c(6)*(M(132)-M(133)+M(134)-M(135)-M(156)+M(157)-M(158)+M(159)-M(201)+M(202)+M(219)-M(220)-M(225)+M(226)+M(243) &
    -M(244))) * den(139)
  T3sum(1:35,25) = T3sum(1:35,25) + Gcoeff * G3tensor(:,188)
  Gcoeff = (c(6)*(-M(142)+M(146)-M(148)+M(152)+M(156)-M(157)+M(158)-M(159)-M(166)+M(170)-M(172)+M(176)+M(201)-M(219)+M(225) &
    -M(243))) * den(139)
  T3sum(1:35,25) = T3sum(1:35,25) + Gcoeff * G3tensor(:,189)
  Gcoeff = (c(6)*(M(131)-M(132)-M(134)+M(136)-M(140)+M(142)+M(148)-M(154)-M(164)+M(166)+M(172)-M(178)+M(196)-M(202)-M(226) &
    +M(250))) * den(748)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,85)
  Gcoeff = (c(6)*(-M(131)+M(132)+M(134)-M(136)+M(155)-M(156)-M(158)+M(160)+M(195)-M(196)-M(201)+M(202)-M(225)+M(226)+M(249) &
    -M(250))) * den(748)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,86)
  Gcoeff = (c(6)*(M(140)-M(142)-M(148)+M(154)-M(155)+M(156)+M(158)-M(160)+M(164)-M(166)-M(172)+M(178)-M(195)+M(201)+M(225) &
    -M(249))) * den(748)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,87)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(135)-M(136)+M(140)-M(146)-M(152)+M(154)+M(164)-M(170)-M(176)+M(178)-M(196)+M(220)+M(244) &
    -M(250))) * den(742)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,403)
  Gcoeff = (c(6)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(742)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,404)
  Gcoeff = (c(6)*(-M(140)+M(146)+M(152)-M(154)+M(155)-M(157)-M(159)+M(160)-M(164)+M(170)+M(176)-M(178)+M(195)-M(219)-M(243) &
    +M(249))) * den(742)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,405)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(6)*(M(133)+M(135)-M(146)-M(152) &
    -M(170)-M(176)+M(220)+M(244))) * den(1)
  T4sum(1:70,145) = T4sum(1:70,145) + Gcoeff * G4tensor(:,47)
  Gcoeff = (c(5)*(-M(41)-M(42)-M(43)+M(53)+M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92))+c(6)*(-M(133)-M(135)+M(157)+M(159) &
    +M(219)-M(220)+M(243)-M(244))) * den(1)
  T4sum(1:70,145) = T4sum(1:70,145) + Gcoeff * G4tensor(:,48)
  Gcoeff = (c(5)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(6)*(M(146)+M(152)-M(157)-M(159) &
    +M(170)+M(176)-M(219)-M(243))) * den(1)
  T4sum(1:70,145) = T4sum(1:70,145) + Gcoeff * G4tensor(:,49)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(143)-M(145)+M(149)-M(151)-M(153)+M(154)+M(164)-M(188)-M(212)+M(218)-M(236)+M(242)+M(248) &
    -M(250))) * den(308)
  T3sum(1:35,22) = T3sum(1:35,22) + Gcoeff * G3tensor(:,196)
  Gcoeff = (c(6)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(310)
  T3sum(1:35,81) = T3sum(1:35,81) + Gcoeff * G3tensor(:,385)
  Gcoeff = (c(6)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(785)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,88)
  Gcoeff = (c(5)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56)+M(68))+c(6)*(M(143)-M(145)+M(149)-M(151) &
    -M(212)+M(218)-M(236)+M(242))) * den(13)
  T4sum(1:70,99) = T4sum(1:70,99) + Gcoeff * G4tensor(:,115)
  Gcoeff = (c(6)*(M(136)-M(138)+M(139)-M(140)+M(141)-M(144)+M(147)-M(150)-M(178)+M(190)-M(194)+M(196)-M(200)+M(214)-M(224) &
    +M(238))) * den(156)
  T3sum(1:35,24) = T3sum(1:35,24) + Gcoeff * G3tensor(:,178)
  Gcoeff = (c(6)*(M(135)-M(138)+M(141)-M(144)+M(145)-M(146)+M(147)-M(149)-M(176)+M(190)-M(200)+M(214)-M(218)+M(220)-M(224) &
    +M(236))) * den(158)
  T3sum(1:35,81) = T3sum(1:35,81) + Gcoeff * G3tensor(:,386)
  Gcoeff = (c(6)*(M(135)-M(136)-M(139)+M(140)+M(145)-M(146)-M(149)+M(150)-M(176)+M(178)+M(194)-M(196)-M(218)+M(220)+M(236) &
    -M(238))) * den(812)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,406)
  Gcoeff = (c(5)*(-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62)+M(92))+c(6)*(M(135)+M(145)-M(146)-M(149) &
    -M(176)-M(218)+M(220)+M(236))) * den(17)
  T4sum(1:70,92) = T4sum(1:70,92) + Gcoeff * G4tensor(:,79)
  Gcoeff = (c(6)*(-M(132)+M(133)-M(134)+M(135)+M(142)-M(146)+M(148)-M(152)+M(166)-M(170)+M(172)-M(176)-M(202)+M(220)-M(226) &
    +M(244))) * den(159)
  T3sum(1:35,25) = T3sum(1:35,25) + Gcoeff * G3tensor(:,190)
  Gcoeff = (c(6)*(-M(132)+M(135)+M(141)+M(143)-M(144)-M(146)+M(148)-M(151)+M(166)-M(176)-M(200)-M(212)+M(214)+M(220)-M(226) &
    +M(242))) * den(161)
  T3sum(1:35,81) = T3sum(1:35,81) + Gcoeff * G3tensor(:,390)
  Gcoeff = (c(6)*(-M(133)+M(134)+M(141)-M(142)+M(143)-M(144)-M(151)+M(152)+M(170)-M(172)-M(200)+M(202)-M(212)+M(214)+M(242) &
    -M(244))) * den(797)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,407)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(141)+M(143)-M(144)-M(151) &
    -M(200)-M(212)+M(214)+M(242))) * den(23)
  T4sum(1:70,86) = T4sum(1:70,86) + Gcoeff * G4tensor(:,40)
  Gcoeff = (c(6)*(M(131)-M(132)-M(134)+M(136)-M(140)+M(142)+M(148)-M(154)-M(164)+M(166)+M(172)-M(178)+M(196)-M(202)-M(226) &
    +M(250))) * den(1156)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,89)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(135)-M(136)+M(140)-M(146)-M(152)+M(154)+M(164)-M(170)-M(176)+M(178)-M(196)+M(220)+M(244) &
    -M(250))) * den(1158)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,408)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(6)*(M(133)+M(135)-M(146)-M(152) &
    -M(170)-M(176)+M(220)+M(244))) * den(152)
  T4sum(1:70,145) = T4sum(1:70,145) + Gcoeff * G4tensor(:,50)
  Gcoeff = (c(6)*(M(134)-M(136)-M(137)+M(138)+M(140)-M(142)-M(147)+M(153)-M(172)+M(178)+M(188)-M(190)-M(196)+M(202)+M(224) &
    -M(248))) * den(1159)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,90)
  Gcoeff = (c(6)*(M(134)-M(137)+M(139)+M(141)-M(142)-M(144)-M(150)+M(153)-M(172)+M(188)-M(194)-M(200)+M(202)+M(214)+M(238) &
    -M(248))) * den(1161)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,409)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(M(139)+M(141)-M(144)-M(150) &
    -M(194)-M(200)+M(214)+M(238))) * den(147)
  T4sum(1:70,142) = T4sum(1:70,142) + Gcoeff * G4tensor(:,127)
  Gcoeff = (c(6)*(M(131)-M(133)-M(139)+M(145)-M(149)+M(150)+M(152)-M(154)-M(164)+M(170)+M(194)-M(218)+M(236)-M(238)-M(244) &
    +M(250))) * den(1165)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,410)
  Gcoeff = (c(6)*(-M(133)+M(137)-M(139)+M(143)+M(150)-M(151)+M(152)-M(153)+M(170)-M(188)+M(194)-M(212)-M(238)+M(242)-M(244) &
    +M(248))) * den(1166)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,411)
  Gcoeff = (c(5)*(-M(43)-M(46)-M(49)+M(50)+M(51)+M(52)+M(62)+M(74)+M(86)-M(92)-M(95)-M(98))+c(6)*(-M(133)-M(139)+M(150)+M(152) &
    +M(170)+M(194)-M(238)-M(244))) * den(307)
  T4sum(1:70,138) = T4sum(1:70,138) + Gcoeff * G4tensor(:,83)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42) &
    +M(43)-M(44)-M(45)+M(46)+M(47)+M(48)+M(49)-M(50)-M(51)-M(52)-M(56)+M(59)-M(62)+M(68)+M(71)-M(74)-M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(143)-M(151)-M(212)+M(242))) * den(11)
  T5sum(1:126,169) = T5sum(1:126,169) + Gcoeff * G5tensor(:,2)
  Gcoeff = (c(4)*(-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42) &
    +M(43)+M(44)-M(45)+M(46)+M(47)+M(48)+M(49)-M(50)-M(51)-M(52)+M(56)+M(59)-M(62)-M(68)+M(71)-M(74)-M(80)-M(83)-M(86)+M(92)+M(95) &
    +M(98))+c(6)*(M(145)-M(149)-M(218)+M(236))) * den(11)
  T5sum(1:126,171) = T5sum(1:126,171) + Gcoeff * G5tensor(:,8)
  Gcoeff = (c(6)*(M(140)-M(155)+M(161)+M(167)-M(169)+M(173)-M(175)-M(177)+M(178)-M(182)-M(206)+M(216)-M(230)+M(240)+M(246) &
    -M(249))) * den(370)
  T3sum(1:35,43) = T3sum(1:35,43) + Gcoeff * G3tensor(:,283)
  Gcoeff = (c(6)*(M(142)-M(156)+M(162)+M(167)-M(169)-M(171)+M(172)+M(173)-M(175)-M(184)-M(206)+M(216)+M(222)-M(225)-M(230) &
    +M(240))) * den(372)
  T3sum(1:35,74) = T3sum(1:35,74) + Gcoeff * G3tensor(:,367)
  Gcoeff = (c(6)*(-M(140)+M(142)+M(155)-M(156)-M(161)+M(162)-M(171)+M(172)+M(177)-M(178)+M(182)-M(184)+M(222)-M(225)-M(246) &
    +M(249))) * den(846)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,91)
  Gcoeff = (c(5)*(M(9)-M(10)+M(11)-M(12)-M(15)+M(16)-M(21)+M(22)-M(44)+M(53)-M(56)+M(65))+c(6)*(M(167)-M(169)+M(173)-M(175)-M(206) &
    +M(216)-M(230)+M(240))) * den(34)
  T4sum(1:70,126) = T4sum(1:70,126) + Gcoeff * G4tensor(:,112)
  Gcoeff = (c(6)*(-M(154)+M(160)-M(162)+M(163)-M(164)+M(165)-M(168)+M(171)-M(174)+M(184)-M(192)+M(195)-M(198)+M(208)-M(222) &
    +M(232))) * den(180)
  T3sum(1:35,45) = T3sum(1:35,45) + Gcoeff * G3tensor(:,200)
  Gcoeff = (c(6)*(-M(152)+M(159)-M(162)+M(165)-M(168)+M(169)-M(170)+M(171)-M(173)+M(184)-M(198)+M(208)-M(216)+M(219)-M(222) &
    +M(230))) * den(182)
  T3sum(1:35,74) = T3sum(1:35,74) + Gcoeff * G3tensor(:,368)
  Gcoeff = (c(6)*(-M(152)+M(154)+M(159)-M(160)-M(163)+M(164)+M(169)-M(170)-M(173)+M(174)+M(192)-M(195)-M(216)+M(219)+M(230) &
    -M(232))) * den(873)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,412)
  Gcoeff = (c(5)*(-M(11)+M(12)-M(17)+M(18)+M(21)-M(22)+M(23)-M(24)-M(50)+M(55)-M(62)+M(89))+c(6)*(-M(152)+M(159)+M(169)-M(170) &
    -M(173)-M(216)+M(219)+M(230))) * den(38)
  T4sum(1:70,119) = T4sum(1:70,119) + Gcoeff * G4tensor(:,72)
  Gcoeff = (c(6)*(-M(142)+M(146)-M(148)+M(152)+M(156)-M(157)+M(158)-M(159)-M(166)+M(170)-M(172)+M(176)+M(201)-M(219)+M(225) &
    -M(243))) * den(183)
  T3sum(1:35,25) = T3sum(1:35,25) + Gcoeff * G3tensor(:,191)
  Gcoeff = (c(6)*(M(142)-M(152)-M(156)+M(159)+M(165)+M(167)-M(168)-M(170)+M(172)-M(175)-M(198)-M(206)+M(208)+M(219)-M(225) &
    +M(240))) * den(185)
  T3sum(1:35,74) = T3sum(1:35,74) + Gcoeff * G3tensor(:,372)
  Gcoeff = (c(6)*(M(146)-M(148)-M(157)+M(158)+M(165)-M(166)+M(167)-M(168)-M(175)+M(176)-M(198)+M(201)-M(206)+M(208)+M(240) &
    -M(243))) * den(858)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,413)
  Gcoeff = (c(5)*(M(11)-M(12)-M(21)+M(22)+M(37)-M(38)-M(39)+M(40)+M(58)-M(63)-M(75)+M(96))+c(6)*(M(165)+M(167)-M(168)-M(175) &
    -M(198)-M(206)+M(208)+M(240))) * den(61)
  T4sum(1:70,59) = T4sum(1:70,59) + Gcoeff * G4tensor(:,92)
  Gcoeff = (c(6)*(M(140)-M(142)-M(148)+M(154)-M(155)+M(156)+M(158)-M(160)+M(164)-M(166)-M(172)+M(178)-M(195)+M(201)+M(225) &
    -M(249))) * den(1168)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,92)
  Gcoeff = (c(6)*(-M(140)+M(146)+M(152)-M(154)+M(155)-M(157)-M(159)+M(160)-M(164)+M(170)+M(176)-M(178)+M(195)-M(219)-M(243) &
    +M(249))) * den(1170)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,414)
  Gcoeff = (c(5)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(6)*(M(146)+M(152)-M(157)-M(159) &
    +M(170)+M(176)-M(219)-M(243))) * den(176)
  T4sum(1:70,145) = T4sum(1:70,145) + Gcoeff * G4tensor(:,51)
  Gcoeff = (c(6)*(-M(148)+M(154)+M(158)-M(160)-M(161)+M(162)+M(164)-M(166)-M(171)+M(177)+M(182)-M(184)-M(195)+M(201)+M(222) &
    -M(246))) * den(1171)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,93)
  Gcoeff = (c(6)*(-M(148)+M(158)-M(161)+M(163)+M(165)-M(166)-M(168)-M(174)+M(177)+M(182)-M(192)-M(198)+M(201)+M(208)+M(232) &
    -M(246))) * den(1173)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,415)
  Gcoeff = (c(5)*(M(44)-M(53)+M(56)+M(57)+M(58)-M(60)-M(63)-M(65)-M(72)-M(75)+M(84)+M(96))+c(6)*(M(163)+M(165)-M(168)-M(174) &
    -M(192)-M(198)+M(208)+M(232))) * den(171)
  T4sum(1:70,154) = T4sum(1:70,154) + Gcoeff * G4tensor(:,128)
  Gcoeff = (c(6)*(-M(140)+M(146)+M(155)-M(157)-M(163)+M(169)-M(173)+M(174)+M(176)-M(178)+M(192)-M(216)+M(230)-M(232)-M(243) &
    +M(249))) * den(1177)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,416)
  Gcoeff = (c(6)*(M(146)-M(157)+M(161)-M(163)+M(167)+M(174)-M(175)+M(176)-M(177)-M(182)+M(192)-M(206)-M(232)+M(240)-M(243) &
    +M(246))) * den(1178)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,417)
  Gcoeff = (c(5)*(M(50)-M(55)-M(58)-M(61)+M(62)+M(63)+M(64)+M(75)+M(87)-M(89)-M(96)-M(99))+c(6)*(M(146)-M(157)-M(163)+M(174) &
    +M(176)+M(192)-M(232)-M(243))) * den(369)
  T4sum(1:70,150) = T4sum(1:70,150) + Gcoeff * G4tensor(:,84)
  Gcoeff = (c(4)*(-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)+M(37)-M(38)-M(39)+M(40)-M(44)-M(47) &
    +M(50)+M(53)+M(54)-M(55)-M(56)-M(57)-M(58)-M(59)+M(60)-M(61)+M(62)+M(63)+M(64)+M(65)+M(72)+M(75)+M(77)-M(84)+M(87)-M(89)-M(96) &
    -M(99))+c(6)*(-M(163)+M(174)+M(192)-M(232))) * den(32)
  T5sum(1:126,163) = T5sum(1:126,163) + Gcoeff * G5tensor(:,11)
  Gcoeff = (c(4)*(-M(9)+M(10)+M(11)-M(12)+M(15)-M(16)+M(17)-M(18)-M(21)+M(22)-M(23)+M(24)+M(37)-M(38)-M(39)+M(40)+M(44)-M(47) &
    +M(50)-M(53)+M(54)-M(55)+M(56)+M(57)+M(58)-M(59)-M(60)-M(61)+M(62)-M(63)+M(64)-M(65)-M(72)-M(75)+M(77)+M(84)+M(87)-M(89)+M(96) &
    -M(99))+c(6)*(M(165)-M(168)-M(198)+M(208))) * den(32)
  T5sum(1:126,164) = T5sum(1:126,164) + Gcoeff * G5tensor(:,13)
  Gcoeff = (c(6)*(M(134)-M(158)-M(179)+M(185)+M(191)-M(193)+M(197)-M(199)-M(201)+M(202)-M(204)+M(210)-M(228)+M(234)+M(245) &
    -M(247))) * den(399)
  T3sum(1:35,54) = T3sum(1:35,54) + Gcoeff * G3tensor(:,319)
  Gcoeff = (c(6)*(M(136)-M(160)-M(180)+M(186)+M(191)-M(193)-M(195)+M(196)+M(197)-M(199)-M(204)+M(210)+M(221)-M(223)-M(228) &
    +M(234))) * den(401)
  T3sum(1:35,63) = T3sum(1:35,63) + Gcoeff * G3tensor(:,340)
  Gcoeff = (c(6)*(M(134)-M(136)-M(158)+M(160)-M(179)+M(180)+M(185)-M(186)+M(195)-M(196)-M(201)+M(202)-M(221)+M(223)+M(245) &
    -M(247))) * den(904)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,94)
  Gcoeff = (c(5)*(M(3)-M(4)+M(5)-M(6)-M(13)+M(14)-M(19)+M(20)-M(41)+M(53)+M(65)-M(68))+c(6)*(M(191)-M(193)+M(197)-M(199)-M(204) &
    +M(210)-M(228)+M(234))) * den(42)
  T4sum(1:70,135) = T4sum(1:70,135) + Gcoeff * G4tensor(:,109)
  Gcoeff = (c(6)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(205)
  T3sum(1:35,54) = T3sum(1:35,54) + Gcoeff * G3tensor(:,320)
  Gcoeff = (c(6)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(206)
  T3sum(1:35,65) = T3sum(1:35,65) + Gcoeff * G3tensor(:,215)
  Gcoeff = (c(6)*(M(131)-M(133)-M(155)+M(157)-M(191)+M(193)+M(215)-M(217)-M(227)+M(228)+M(233)-M(234)+M(243)-M(244)-M(249) &
    +M(250))) * den(931)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,418)
  Gcoeff = (c(5)*(-M(1)+M(2)-M(3)+M(4)+M(7)-M(8)+M(13)-M(14)-M(43)+M(55)+M(89)-M(92))+c(6)*(-M(133)+M(157)-M(191)+M(193)+M(228) &
    -M(234)+M(243)-M(244))) * den(46)
  T4sum(1:70,127) = T4sum(1:70,127) + Gcoeff * G4tensor(:,65)
  Gcoeff = (c(6)*(-M(132)+M(133)-M(134)+M(135)+M(156)-M(157)+M(158)-M(159)+M(201)-M(202)-M(219)+M(220)+M(225)-M(226)-M(243) &
    +M(244))) * den(207)
  T3sum(1:35,25) = T3sum(1:35,25) + Gcoeff * G3tensor(:,192)
  Gcoeff = (c(6)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(209)
  T3sum(1:35,54) = T3sum(1:35,54) + Gcoeff * G3tensor(:,324)
  Gcoeff = (c(6)*(-M(132)+M(135)+M(156)-M(159)+M(197)-M(199)+M(203)-M(204)-M(209)+M(210)-M(219)+M(220)+M(225)-M(226)-M(239) &
    +M(241))) * den(914)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,419)
  Gcoeff = (c(5)*(M(3)-M(4)-M(13)+M(14)-M(37)+M(38)+M(39)-M(40)+M(102)-M(105)-M(111)+M(121))+c(6)*(M(197)-M(199)+M(203)-M(204) &
    -M(209)+M(210)-M(239)+M(241))) * den(97)
  T4sum(1:70,23) = T4sum(1:70,23) + Gcoeff * G4tensor(:,93)
  Gcoeff = (c(6)*(M(131)-M(132)-M(134)+M(136)-M(155)+M(156)+M(158)-M(160)-M(195)+M(196)+M(201)-M(202)+M(225)-M(226)-M(249) &
    +M(250))) * den(1180)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,95)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(135)-M(136)+M(155)-M(157)-M(159)+M(160)+M(195)-M(196)-M(219)+M(220)-M(243)+M(244)+M(249) &
    -M(250))) * den(1182)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,420)
  Gcoeff = (c(5)*(M(41)+M(42)+M(43)-M(53)-M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92))+c(6)*(M(133)+M(135)-M(157)-M(159) &
    -M(219)+M(220)-M(243)+M(244))) * den(200)
  T4sum(1:70,145) = T4sum(1:70,145) + Gcoeff * G4tensor(:,52)
  Gcoeff = (c(6)*(M(131)-M(132)-M(155)+M(156)-M(179)+M(180)+M(185)-M(186)-M(221)+M(223)+M(225)-M(226)+M(245)-M(247)-M(249) &
    +M(250))) * den(1183)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,96)
  Gcoeff = (c(6)*(M(135)-M(136)-M(159)+M(160)-M(191)+M(193)+M(195)-M(196)+M(215)-M(217)-M(219)+M(220)-M(227)+M(228)+M(233) &
    -M(234))) * den(1185)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,421)
  Gcoeff = (c(6)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(1187)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,422)
  Gcoeff = (c(5)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(6)*(M(203)-M(209)-M(215) &
    +M(217)+M(227)-M(233)-M(239)+M(241))) * den(197)
  T4sum(1:70,163) = T4sum(1:70,163) + Gcoeff * G4tensor(:,129)
  Gcoeff = (c(6)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(1190)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,423)
  Gcoeff = (c(5)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(6)*(M(135)-M(159)+M(215) &
    -M(217)-M(219)+M(220)-M(227)+M(233))) * den(400)
  T4sum(1:70,159) = T4sum(1:70,159) + Gcoeff * G4tensor(:,85)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(5)+M(6)-M(7)+M(8)-M(13)+M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42)+M(43)+M(53) &
    +M(54)-M(55)+M(65)-M(68)+M(77)-M(80)-M(89)+M(92)-M(101)-M(102)+M(103)-M(104)+M(105)+M(106)+M(109)+M(111)-M(115)+M(117)-M(121) &
    -M(123))+c(6)*(M(215)-M(217)-M(227)+M(233))) * den(41)
  T5sum(1:126,115) = T5sum(1:126,115) + Gcoeff * G5tensor(:,12)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(5)+M(6)-M(7)+M(8)-M(13)+M(14)+M(19)-M(20)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42)+M(43)-M(53) &
    +M(54)-M(55)-M(65)+M(68)+M(77)-M(80)-M(89)+M(92)+M(101)+M(102)-M(103)-M(104)-M(105)+M(106)-M(109)-M(111)+M(115)+M(117)+M(121) &
    -M(123))+c(6)*(M(203)-M(209)-M(239)+M(241))) * den(41)
  T5sum(1:126,116) = T5sum(1:126,116) + Gcoeff * G5tensor(:,14)
  Gcoeff = (c(6)*(M(153)-M(160)+M(168)-M(171)+M(174)-M(184)+M(186)-M(187)+M(188)-M(189)+M(192)-M(195)+M(198)-M(207)+M(221) &
    -M(231))) * den(223)
  T3sum(1:35,45) = T3sum(1:35,45) + Gcoeff * G3tensor(:,201)
  Gcoeff = (c(6)*(-M(150)+M(160)-M(174)+M(183)-M(186)+M(189)-M(192)+M(193)-M(194)+M(195)-M(197)+M(207)-M(210)+M(213)-M(221) &
    +M(228))) * den(225)
  T3sum(1:35,63) = T3sum(1:35,63) + Gcoeff * G3tensor(:,353)
  Gcoeff = (c(6)*(-M(150)+M(153)+M(168)-M(171)+M(183)-M(184)-M(187)+M(188)+M(193)-M(194)-M(197)+M(198)-M(210)+M(213)+M(228) &
    -M(231))) * den(974)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,424)
  Gcoeff = (c(5)*(-M(5)+M(6)+M(17)-M(18)+M(19)-M(20)-M(23)+M(24)-M(51)+M(67)-M(74)+M(90))+c(6)*(-M(150)+M(183)+M(193)-M(194) &
    -M(197)-M(210)+M(213)+M(228))) * den(49)
  T4sum(1:70,107) = T4sum(1:70,107) + Gcoeff * G4tensor(:,110)
  Gcoeff = (c(6)*(-M(136)+M(144)-M(147)+M(150)+M(177)+M(180)-M(181)+M(182)-M(183)-M(190)+M(194)-M(196)+M(200)-M(213)+M(223) &
    -M(237))) * den(226)
  T3sum(1:35,24) = T3sum(1:35,24) + Gcoeff * G3tensor(:,179)
  Gcoeff = (c(6)*(M(136)-M(150)-M(174)-M(180)+M(183)+M(189)+M(191)-M(192)-M(194)+M(196)-M(199)-M(204)+M(207)+M(213)-M(223) &
    +M(234))) * den(228)
  T3sum(1:35,63) = T3sum(1:35,63) + Gcoeff * G3tensor(:,357)
  Gcoeff = (c(6)*(M(144)-M(147)-M(174)+M(177)-M(181)+M(182)+M(189)-M(190)+M(191)-M(192)-M(199)+M(200)-M(204)+M(207)+M(234) &
    -M(237))) * den(960)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,425)
  Gcoeff = (c(5)*(M(5)-M(6)-M(19)+M(20)+M(31)-M(32)-M(35)+M(36)-M(63)+M(70)-M(75)+M(93))+c(6)*(-M(174)+M(189)+M(191)-M(192)-M(199) &
    -M(204)+M(207)+M(234))) * den(70)
  T4sum(1:70,71) = T4sum(1:70,71) + Gcoeff * G4tensor(:,111)
  Gcoeff = (c(6)*(-M(134)+M(136)+M(147)-M(153)+M(171)-M(177)+M(179)-M(180)-M(182)+M(184)-M(188)+M(190)+M(196)-M(202)-M(223) &
    +M(247))) * den(1192)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,97)
  Gcoeff = (c(6)*(-M(134)+M(144)+M(150)-M(153)+M(171)+M(179)-M(181)-M(183)+M(184)-M(188)+M(194)+M(200)-M(202)-M(213)-M(237) &
    +M(247))) * den(1194)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,426)
  Gcoeff = (c(5)*(M(41)+M(48)+M(51)-M(53)-M(65)-M(66)-M(67)+M(68)+M(71)+M(74)-M(78)-M(90))+c(6)*(M(144)+M(150)-M(181)-M(183) &
    +M(194)+M(200)-M(213)-M(237))) * den(219)
  T4sum(1:70,142) = T4sum(1:70,142) + Gcoeff * G4tensor(:,130)
  Gcoeff = (c(6)*(M(147)-M(153)-M(158)+M(160)+M(171)-M(177)-M(182)+M(184)+M(185)-M(186)-M(188)+M(190)+M(195)-M(201)-M(221) &
    +M(245))) * den(1195)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,98)
  Gcoeff = (c(6)*(M(147)-M(158)+M(168)+M(174)-M(177)-M(182)+M(185)-M(187)-M(189)+M(190)+M(192)+M(198)-M(201)-M(207)-M(231) &
    +M(245))) * den(1197)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,427)
  Gcoeff = (c(5)*(-M(41)+M(53)+M(60)+M(63)+M(65)-M(68)-M(69)-M(70)+M(72)+M(75)-M(81)-M(93))+c(6)*(M(168)+M(174)-M(187)-M(189) &
    +M(192)+M(198)-M(207)-M(231))) * den(214)
  T4sum(1:70,154) = T4sum(1:70,154) + Gcoeff * G4tensor(:,131)
  Gcoeff = (c(6)*(-M(134)+M(144)+M(168)+M(179)-M(181)-M(187)+M(193)-M(197)+M(198)+M(200)-M(202)-M(210)+M(228)-M(231)-M(237) &
    +M(247))) * den(1201)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,428)
  Gcoeff = (c(6)*(M(144)-M(158)+M(168)-M(181)+M(185)-M(187)+M(191)+M(198)-M(199)+M(200)-M(201)-M(204)-M(231)+M(234)-M(237) &
    +M(245))) * den(1202)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,429)
  Gcoeff = (c(5)*(M(51)+M(63)-M(67)-M(70)-M(73)+M(74)+M(75)+M(76)+M(88)-M(90)-M(93)-M(100))+c(6)*(M(144)+M(168)-M(181)-M(187) &
    +M(198)+M(200)-M(231)-M(237))) * den(398)
  T4sum(1:70,168) = T4sum(1:70,168) + Gcoeff * G4tensor(:,41)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)-M(41)-M(48)+M(51) &
    +M(53)+M(60)+M(63)+M(65)+M(66)-M(67)-M(68)-M(69)-M(70)-M(71)+M(72)-M(73)+M(74)+M(75)+M(76)+M(78)-M(81)+M(88)-M(90)-M(93) &
    -M(100))+c(6)*(M(168)-M(187)+M(198)-M(231))) * den(40)
  T5sum(1:126,151) = T5sum(1:126,151) + Gcoeff * G5tensor(:,5)
  Gcoeff = (c(4)*(-M(3)+M(4)+M(5)-M(6)+M(13)-M(14)-M(17)+M(18)-M(19)+M(20)+M(23)-M(24)+M(31)-M(32)-M(35)+M(36)+M(41)-M(48)+M(51) &
    -M(53)-M(60)-M(63)-M(65)+M(66)-M(67)+M(68)+M(69)+M(70)-M(71)-M(72)-M(73)+M(74)-M(75)+M(76)+M(78)+M(81)+M(88)-M(90)+M(93) &
    -M(100))+c(6)*(-M(174)+M(189)-M(192)+M(207))) * den(40)
  T5sum(1:126,152) = T5sum(1:126,152) + Gcoeff * G5tensor(:,18)
  Gcoeff = (c(6)*(-M(139)+M(155)-M(161)-M(167)+M(169)+M(181)+M(205)-M(215)+M(230)-M(233)+M(235)+M(237)-M(238)-M(240)-M(246) &
    +M(249))) * den(242)
  T3sum(1:35,43) = T3sum(1:35,43) + Gcoeff * G3tensor(:,305)
  Gcoeff = (c(6)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(243)
  T3sum(1:35,65) = T3sum(1:35,65) + Gcoeff * G3tensor(:,216)
  Gcoeff = (c(6)*(M(137)-M(139)-M(167)+M(169)-M(179)+M(181)+M(209)-M(211)-M(229)+M(230)+M(237)-M(238)+M(239)-M(240)-M(247) &
    +M(248))) * den(1016)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,430)
  Gcoeff = (c(5)*(M(1)-M(2)-M(7)+M(8)-M(9)+M(10)+M(15)-M(16)-M(46)+M(67)+M(90)-M(95))+c(6)*(-M(139)-M(167)+M(169)+M(181)+M(230) &
    +M(237)-M(238)-M(240))) * den(52)
  T4sum(1:70,112) = T4sum(1:70,112) + Gcoeff * G4tensor(:,113)
  Gcoeff = (c(6)*(-M(138)+M(139)-M(140)+M(141)+M(177)-M(178)+M(180)-M(181)+M(182)-M(183)-M(213)+M(214)+M(223)-M(224)-M(237) &
    +M(238))) * den(244)
  T3sum(1:35,24) = T3sum(1:35,24) + Gcoeff * G3tensor(:,180)
  Gcoeff = (c(6)*(-M(139)+M(140)+M(173)-M(175)-M(177)+M(178)+M(181)-M(182)+M(205)-M(206)-M(215)+M(216)-M(233)+M(235)+M(237) &
    -M(238))) * den(246)
  T3sum(1:35,43) = T3sum(1:35,43) + Gcoeff * G3tensor(:,309)
  Gcoeff = (c(6)*(-M(138)+M(141)+M(173)-M(175)+M(180)-M(183)+M(205)-M(206)-M(213)+M(214)-M(215)+M(216)+M(223)-M(224)-M(233) &
    +M(235))) * den(1000)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,431)
  Gcoeff = (c(5)*(M(9)-M(10)-M(15)+M(16)-M(31)+M(32)+M(35)-M(36)-M(105)+M(108)-M(111)+M(119))+c(6)*(M(173)-M(175)+M(205)-M(206) &
    -M(215)+M(216)-M(233)+M(235))) * den(103)
  T4sum(1:70,35) = T4sum(1:70,35) + Gcoeff * G4tensor(:,114)
  Gcoeff = (c(6)*(-M(137)+M(138)+M(140)-M(142)+M(171)-M(172)-M(177)+M(178)+M(179)-M(180)-M(182)+M(184)-M(223)+M(224)+M(247) &
    -M(248))) * den(1204)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,99)
  Gcoeff = (c(6)*(-M(137)+M(139)+M(141)-M(142)+M(171)-M(172)+M(179)-M(181)-M(183)+M(184)-M(213)+M(214)-M(237)+M(238)+M(247) &
    -M(248))) * den(1206)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,432)
  Gcoeff = (c(5)*(M(44)+M(45)+M(46)-M(53)+M(56)-M(65)-M(66)-M(67)-M(78)+M(83)-M(90)+M(95))+c(6)*(M(139)+M(141)-M(181)-M(183) &
    -M(213)+M(214)-M(237)+M(238))) * den(237)
  T4sum(1:70,142) = T4sum(1:70,142) + Gcoeff * G4tensor(:,132)
  Gcoeff = (c(6)*(-M(137)+M(138)+M(155)-M(156)-M(161)+M(162)+M(179)-M(180)+M(222)-M(223)+M(224)-M(225)-M(246)+M(247)-M(248) &
    +M(249))) * den(1207)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,100)
  Gcoeff = (c(6)*(M(141)-M(142)-M(167)+M(169)+M(171)-M(172)-M(183)+M(184)+M(209)-M(211)-M(213)+M(214)-M(229)+M(230)+M(239) &
    -M(240))) * den(1209)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,433)
  Gcoeff = (c(6)*(M(138)-M(156)+M(162)-M(180)-M(205)+M(209)-M(211)+M(215)+M(222)-M(223)+M(224)-M(225)-M(229)+M(233)-M(235) &
    +M(239))) * den(1211)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,434)
  Gcoeff = (c(5)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(6)*(-M(205)+M(209)-M(211) &
    +M(215)-M(229)+M(233)-M(235)+M(239))) * den(234)
  T4sum(1:70,163) = T4sum(1:70,163) + Gcoeff * G4tensor(:,133)
  Gcoeff = (c(6)*(M(141)-M(156)+M(162)+M(173)-M(175)-M(183)-M(206)+M(209)-M(211)-M(213)+M(214)+M(216)+M(222)-M(225)-M(229) &
    +M(239))) * den(1214)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,435)
  Gcoeff = (c(5)*(M(46)-M(67)-M(90)+M(95)+M(105)-M(108)-M(110)+M(111)+M(112)+M(118)-M(119)-M(124))+c(6)*(M(141)-M(183)+M(209) &
    -M(211)-M(213)+M(214)-M(229)+M(239))) * den(371)
  T4sum(1:70,174) = T4sum(1:70,174) + Gcoeff * G4tensor(:,42)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)-M(44)-M(45)+M(46) &
    +M(53)-M(56)+M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)+M(103)+M(105)-M(107)-M(108)+M(109)-M(110)+M(111)+M(112)-M(113)+M(118) &
    -M(119)-M(124))+c(6)*(M(209)-M(211)-M(229)+M(239))) * den(33)
  T5sum(1:126,103) = T5sum(1:126,103) + Gcoeff * G5tensor(:,6)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(7)-M(8)+M(9)-M(10)-M(11)+M(12)-M(15)+M(16)+M(21)-M(22)-M(31)+M(32)+M(35)-M(36)+M(44)-M(45)+M(46) &
    -M(53)+M(56)-M(65)+M(66)-M(67)+M(78)-M(83)-M(90)+M(95)-M(103)-M(105)+M(107)+M(108)-M(109)-M(110)-M(111)+M(112)+M(113)+M(118) &
    +M(119)-M(124))+c(6)*(M(205)-M(215)-M(233)+M(235))) * den(33)
  T5sum(1:126,104) = T5sum(1:126,104) + Gcoeff * G5tensor(:,19)
  Gcoeff = (c(6)*(M(131)-M(137)-M(143)+M(145)-M(163)+M(187)+M(211)-M(217)-M(227)+M(229)+M(231)-M(232)+M(236)-M(242)-M(248) &
    +M(250))) * den(260)
  T3sum(1:35,22) = T3sum(1:35,22) + Gcoeff * G3tensor(:,266)
  Gcoeff = (c(6)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(261)
  T3sum(1:35,65) = T3sum(1:35,65) + Gcoeff * G3tensor(:,217)
  Gcoeff = (c(6)*(-M(143)+M(145)+M(161)-M(163)-M(185)+M(187)+M(203)-M(205)+M(231)-M(232)-M(235)+M(236)+M(241)-M(242)-M(245) &
    +M(246))) * den(1056)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,436)
  Gcoeff = (c(5)*(-M(1)+M(2)+M(7)-M(8)-M(25)+M(26)+M(29)-M(30)-M(58)+M(70)+M(93)-M(96))+c(6)*(-M(143)+M(145)-M(163)+M(187)+M(231) &
    -M(232)+M(236)-M(242))) * den(73)
  T4sum(1:70,76) = T4sum(1:70,76) + Gcoeff * G4tensor(:,116)
  Gcoeff = (c(6)*(M(149)-M(151)-M(153)+M(154)-M(163)+M(164)+M(187)-M(188)+M(211)-M(212)-M(217)+M(218)-M(227)+M(229)+M(231) &
    -M(232))) * den(263)
  T3sum(1:35,22) = T3sum(1:35,22) + Gcoeff * G3tensor(:,270)
  Gcoeff = (c(6)*(M(153)-M(154)-M(162)+M(163)-M(164)+M(165)+M(186)-M(187)+M(188)-M(189)-M(207)+M(208)+M(221)-M(222)-M(231) &
    +M(232))) * den(264)
  T3sum(1:35,45) = T3sum(1:35,45) + Gcoeff * G3tensor(:,202)
  Gcoeff = (c(6)*(M(149)-M(151)-M(162)+M(165)+M(186)-M(189)-M(207)+M(208)+M(211)-M(212)-M(217)+M(218)+M(221)-M(222)-M(227) &
    +M(229))) * den(1042)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,437)
  Gcoeff = (c(5)*(-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(102)+M(108)+M(119)-M(121))+c(6)*(M(149)-M(151)+M(211)-M(212) &
    -M(217)+M(218)-M(227)+M(229))) * den(106)
  T4sum(1:70,40) = T4sum(1:70,40) + Gcoeff * G4tensor(:,117)
  Gcoeff = (c(6)*(M(147)-M(148)-M(153)+M(154)-M(161)+M(162)+M(164)-M(166)+M(185)-M(186)-M(188)+M(190)-M(221)+M(222)+M(245) &
    -M(246))) * den(1216)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,101)
  Gcoeff = (c(6)*(M(131)-M(132)-M(137)+M(138)-M(161)+M(162)+M(185)-M(186)-M(221)+M(222)+M(224)-M(226)+M(245)-M(246)-M(248) &
    +M(250))) * den(1217)
  T3sum(1:35,90) = T3sum(1:35,90) + Gcoeff * G3tensor(:,102)
  Gcoeff = (c(6)*(M(147)-M(148)-M(161)+M(163)+M(165)-M(166)+M(185)-M(187)-M(189)+M(190)-M(207)+M(208)-M(231)+M(232)+M(245) &
    -M(246))) * den(1219)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,438)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(56)+M(57)+M(58)-M(68)-M(69)-M(70)-M(81)+M(84)-M(93)+M(96))+c(6)*(M(163)+M(165)-M(187)-M(189) &
    -M(207)+M(208)-M(231)+M(232))) * den(257)
  T4sum(1:70,154) = T4sum(1:70,154) + Gcoeff * G4tensor(:,134)
  Gcoeff = (c(6)*(-M(143)+M(145)+M(147)-M(148)+M(165)-M(166)-M(189)+M(190)+M(203)-M(205)-M(207)+M(208)-M(235)+M(236)+M(241) &
    -M(242))) * den(1221)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,439)
  Gcoeff = (c(6)*(-M(132)+M(138)+M(162)-M(186)+M(203)-M(205)-M(211)+M(217)-M(221)+M(222)+M(224)-M(226)+M(227)-M(229)-M(235) &
    +M(241))) * den(1223)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,440)
  Gcoeff = (c(5)*(M(41)-M(44)-M(56)+M(68)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121))+c(6)*(M(203)-M(205)-M(211) &
    +M(217)+M(227)-M(229)-M(235)+M(241))) * den(252)
  T4sum(1:70,163) = T4sum(1:70,163) + Gcoeff * G4tensor(:,135)
  Gcoeff = (c(6)*(-M(132)+M(138)+M(149)-M(151)+M(165)-M(189)+M(203)-M(205)-M(207)+M(208)-M(212)+M(218)+M(224)-M(226)-M(235) &
    +M(241))) * den(1225)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,441)
  Gcoeff = (c(5)*(M(58)-M(70)-M(93)+M(96)+M(102)-M(108)-M(119)+M(121)-M(125)+M(126)+M(128)-M(130))+c(6)*(M(165)-M(189)+M(203) &
    -M(205)-M(207)+M(208)-M(235)+M(241))) * den(309)
  T4sum(1:70,180) = T4sum(1:70,180) + Gcoeff * G4tensor(:,106)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(27)+M(28)-M(29)+M(30)+M(33)-M(34)+M(41)-M(44)-M(56) &
    -M(57)+M(58)+M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(203)-M(205)-M(235)+M(241))) * den(12)
  T5sum(1:126,67) = T5sum(1:126,67) + Gcoeff * G5tensor(:,17)
  Gcoeff = (c(4)*(M(1)-M(2)-M(7)+M(8)-M(17)+M(18)+M(23)-M(24)+M(25)-M(26)-M(27)+M(28)-M(29)+M(30)+M(33)-M(34)-M(41)+M(44)+M(56) &
    -M(57)+M(58)-M(68)+M(69)-M(70)+M(81)-M(84)-M(93)+M(96)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121)-M(125)+M(126) &
    +M(128)-M(130))+c(6)*(M(211)-M(217)-M(227)+M(229))) * den(12)
  T5sum(1:126,68) = T5sum(1:126,68) + Gcoeff * G5tensor(:,20)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(139)-M(145)+M(163)-M(169)-M(193)+M(217)+M(227)-M(228)-M(230)+M(232)-M(236)+M(238)+M(244) &
    -M(250))) * den(1276)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,442)
  Gcoeff = (c(6)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(1277)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,443)
  Gcoeff = (c(5)*(M(43)+M(46)+M(49)-M(55)-M(67)-M(79)-M(89)-M(90)-M(91)+M(92)+M(95)+M(98))+c(6)*(M(133)+M(139)-M(169)-M(193) &
    -M(228)-M(230)+M(238)+M(244))) * den(259)
  T4sum(1:70,138) = T4sum(1:70,138) + Gcoeff * G4tensor(:,86)
  Gcoeff = (c(6)*(M(139)-M(145)-M(155)+M(157)+M(163)-M(169)-M(191)+M(215)-M(230)+M(232)+M(233)-M(234)-M(236)+M(238)+M(243) &
    -M(249))) * den(1279)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,444)
  Gcoeff = (c(6)*(-M(145)+M(157)-M(161)+M(163)-M(167)+M(181)-M(191)+M(205)+M(232)-M(234)+M(235)-M(236)+M(237)-M(240)+M(243) &
    -M(246))) * den(1280)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,445)
  Gcoeff = (c(5)*(-M(43)+M(55)+M(58)+M(61)-M(70)-M(82)+M(89)-M(92)-M(93)-M(94)+M(96)+M(99))+c(6)*(-M(145)+M(157)+M(163)-M(191) &
    +M(232)-M(234)-M(236)+M(243))) * den(241)
  T4sum(1:70,150) = T4sum(1:70,150) + Gcoeff * G4tensor(:,87)
  Gcoeff = (c(6)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(1282)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,446)
  Gcoeff = (c(6)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(1283)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,447)
  Gcoeff = (c(5)*(-M(46)-M(58)+M(67)+M(70)+M(73)-M(85)+M(90)+M(93)-M(95)-M(96)-M(97)+M(100))+c(6)*(-M(143)-M(167)+M(181)+M(187) &
    +M(231)+M(237)-M(240)-M(242))) * den(204)
  T4sum(1:70,168) = T4sum(1:70,168) + Gcoeff * G4tensor(:,43)
  Gcoeff = (c(4)*(-M(1)+M(2)+M(3)-M(4)+M(7)-M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)-M(25)+M(26)+M(29)-M(30)-M(43)-M(46)-M(49) &
    +M(55)-M(58)+M(61)+M(67)+M(70)+M(73)+M(79)-M(82)-M(85)+M(89)+M(90)+M(91)-M(92)+M(93)-M(94)-M(95)-M(96)-M(97)-M(98)+M(99) &
    +M(100))+c(6)*(-M(143)+M(187)+M(231)-M(242))) * den(45)
  T5sum(1:126,131) = T5sum(1:126,131) + Gcoeff * G5tensor(:,3)
  Gcoeff = (c(4)*(M(1)-M(2)+M(3)-M(4)-M(7)+M(8)+M(9)-M(10)-M(13)+M(14)-M(15)+M(16)+M(25)-M(26)-M(29)+M(30)-M(43)-M(46)-M(49)+M(55) &
    +M(58)+M(61)+M(67)-M(70)+M(73)+M(79)-M(82)-M(85)+M(89)+M(90)+M(91)-M(92)-M(93)-M(94)-M(95)+M(96)-M(97)-M(98)+M(99)+M(100)) &
    +c(6)*(-M(145)+M(163)+M(232)-M(236))) * den(45)
  T5sum(1:126,132) = T5sum(1:126,132) + Gcoeff * G5tensor(:,9)
  Gcoeff = (c(6)*(-M(149)+M(150)+M(152)-M(154)+M(163)-M(164)-M(169)+M(170)-M(193)+M(194)+M(217)-M(218)+M(227)-M(228)-M(230) &
    +M(232))) * den(1288)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,448)
  Gcoeff = (c(6)*(M(150)-M(151)+M(152)-M(153)-M(169)+M(170)+M(187)-M(188)-M(193)+M(194)+M(211)-M(212)-M(228)+M(229)-M(230) &
    +M(231))) * den(1289)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,449)
  Gcoeff = (c(5)*(M(50)+M(51)+M(52)-M(55)+M(62)-M(67)+M(74)-M(79)+M(86)-M(89)-M(90)-M(91))+c(6)*(M(150)+M(152)-M(169)+M(170) &
    -M(193)+M(194)-M(228)-M(230))) * den(262)
  T4sum(1:70,138) = T4sum(1:70,138) + Gcoeff * G4tensor(:,88)
  Gcoeff = (c(6)*(-M(149)+M(150)+M(159)-M(160)-M(173)+M(174)+M(192)-M(193)+M(194)-M(195)-M(216)+M(217)-M(218)+M(219)+M(227) &
    -M(228))) * den(1291)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,450)
  Gcoeff = (c(6)*(-M(151)+M(152)+M(168)-M(169)+M(170)-M(171)+M(183)-M(184)-M(197)+M(198)-M(210)+M(211)-M(212)+M(213)+M(229) &
    -M(230))) * den(1292)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,451)
  Gcoeff = (c(6)*(-M(149)+M(159)-M(173)+M(183)-M(186)+M(189)-M(197)+M(207)-M(210)+M(213)-M(216)+M(217)-M(218)+M(219)-M(221) &
    +M(227))) * den(1294)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,452)
  Gcoeff = (c(5)*(-M(50)+M(55)-M(62)+M(89)+M(102)+M(104)-M(108)-M(114)-M(119)-M(120)+M(121)+M(123))+c(6)*(-M(149)+M(159)-M(173) &
    -M(216)+M(217)-M(218)+M(219)+M(227))) * den(224)
  T4sum(1:70,159) = T4sum(1:70,159) + Gcoeff * G4tensor(:,89)
  Gcoeff = (c(6)*(-M(151)+M(159)-M(162)+M(165)-M(173)+M(183)-M(197)+M(208)-M(210)+M(211)-M(212)+M(213)-M(216)+M(219)-M(222) &
    +M(229))) * den(1296)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,453)
  Gcoeff = (c(5)*(-M(51)+M(67)-M(74)+M(90)-M(102)+M(108)+M(110)-M(116)+M(119)-M(121)-M(122)+M(124))+c(6)*(-M(151)+M(183)-M(197) &
    -M(210)+M(211)-M(212)+M(213)+M(229))) * den(181)
  T4sum(1:70,174) = T4sum(1:70,174) + Gcoeff * G4tensor(:,44)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)-M(17)+M(18)-M(19)+M(20)-M(21)+M(22)+M(23)-M(24)+M(25)-M(26)-M(29)+M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)-M(102)+M(104)+M(108)+M(110)-M(114)-M(116)+M(119)-M(120)-M(121)-M(122) &
    +M(123)+M(124))+c(6)*(-M(151)+M(211)-M(212)+M(229))) * den(37)
  T5sum(1:126,83) = T5sum(1:126,83) + Gcoeff * G5tensor(:,4)
  Gcoeff = (c(4)*(M(5)-M(6)+M(11)-M(12)+M(17)-M(18)-M(19)+M(20)-M(21)+M(22)-M(23)+M(24)-M(25)+M(26)+M(29)-M(30)-M(50)-M(51)-M(52) &
    +M(55)-M(62)+M(67)-M(74)+M(79)-M(86)+M(89)+M(90)+M(91)+M(102)+M(104)-M(108)+M(110)-M(114)-M(116)-M(119)-M(120)+M(121)-M(122) &
    +M(123)+M(124))+c(6)*(-M(149)+M(217)-M(218)+M(227))) * den(37)
  T5sum(1:126,84) = T5sum(1:126,84) + Gcoeff * G5tensor(:,10)
  Gcoeff = (c(6)*(M(139)-M(140)-M(145)+M(146)-M(173)+M(174)+M(176)-M(178)-M(191)+M(192)+M(215)-M(216)+M(233)-M(234)-M(236) &
    +M(238))) * den(1300)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,454)
  Gcoeff = (c(6)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(1301)
  T3sum(1:35,88) = T3sum(1:35,88) + Gcoeff * G3tensor(:,455)
  Gcoeff = (c(6)*(-M(145)+M(146)+M(174)-M(175)+M(176)-M(177)+M(181)-M(182)-M(191)+M(192)+M(205)-M(206)-M(234)+M(235)-M(236) &
    +M(237))) * den(1302)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,456)
  Gcoeff = (c(5)*(-M(43)+M(50)+M(62)+M(63)+M(64)-M(70)+M(75)-M(82)+M(87)-M(92)-M(93)-M(94))+c(6)*(-M(145)+M(146)+M(174)+M(176) &
    -M(191)+M(192)-M(234)-M(236))) * den(245)
  T4sum(1:70,150) = T4sum(1:70,150) + Gcoeff * G4tensor(:,90)
  Gcoeff = (c(6)*(M(144)-M(145)+M(146)-M(147)-M(175)+M(176)+M(189)-M(190)-M(199)+M(200)-M(204)+M(205)-M(206)+M(207)+M(235) &
    -M(236))) * den(1304)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,457)
  Gcoeff = (c(6)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(1306)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,458)
  Gcoeff = (c(5)*(M(43)-M(50)-M(62)+M(92)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120))+c(6)*(M(135)-M(149)-M(173) &
    +M(215)-M(216)-M(218)+M(220)+M(233))) * den(227)
  T4sum(1:70,159) = T4sum(1:70,159) + Gcoeff * G4tensor(:,91)
  Gcoeff = (c(6)*(M(135)-M(138)+M(141)-M(149)-M(175)+M(189)-M(199)-M(204)+M(205)-M(206)+M(207)+M(214)-M(218)+M(220)-M(224) &
    +M(235))) * den(1308)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,459)
  Gcoeff = (c(5)*(-M(63)+M(70)-M(75)+M(93)-M(105)+M(108)-M(111)+M(119)+M(125)-M(127)-M(129)+M(130))+c(6)*(-M(175)+M(189)-M(199) &
    -M(204)+M(205)-M(206)+M(207)+M(235))) * den(157)
  T4sum(1:70,180) = T4sum(1:70,180) + Gcoeff * G4tensor(:,107)
  Gcoeff = (c(4)*(-M(5)+M(6)+M(9)-M(10)-M(15)+M(16)+M(19)-M(20)+M(27)-M(28)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)-M(105)+M(106)+M(108)-M(111)-M(114)+M(117)+M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(175)+M(205)-M(206)+M(235))) * den(16)
  T5sum(1:126,47) = T5sum(1:126,47) + Gcoeff * G5tensor(:,16)
  Gcoeff = (c(4)*(-M(5)+M(6)-M(9)+M(10)+M(15)-M(16)+M(19)-M(20)+M(27)-M(28)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)+M(43)-M(50)-M(62) &
    -M(63)-M(64)+M(70)-M(75)+M(82)-M(87)+M(92)+M(93)+M(94)+M(105)+M(106)-M(108)+M(111)-M(114)+M(117)-M(119)-M(120)+M(125)-M(127) &
    -M(129)+M(130))+c(6)*(-M(173)+M(215)-M(216)+M(233))) * den(16)
  T5sum(1:126,48) = T5sum(1:126,48) + Gcoeff * G5tensor(:,7)
  Gcoeff = (c(6)*(M(133)-M(134)-M(143)+M(144)-M(167)+M(168)-M(197)+M(198)+M(200)-M(202)+M(209)-M(210)+M(239)-M(240)-M(242) &
    +M(244))) * den(1312)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,460)
  Gcoeff = (c(6)*(M(141)-M(142)-M(151)+M(152)-M(167)+M(168)+M(170)-M(172)-M(197)+M(198)+M(209)-M(210)-M(212)+M(214)+M(239) &
    -M(240))) * den(1313)
  T3sum(1:35,86) = T3sum(1:35,86) + Gcoeff * G3tensor(:,461)
  Gcoeff = (c(6)*(-M(143)+M(144)+M(157)-M(158)-M(167)+M(168)+M(198)-M(199)+M(200)-M(201)+M(203)-M(204)-M(240)+M(241)-M(242) &
    +M(243))) * den(1314)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,462)
  Gcoeff = (c(6)*(-M(143)+M(144)+M(146)-M(148)+M(165)-M(166)-M(175)+M(176)-M(199)+M(200)+M(203)-M(204)-M(206)+M(208)+M(241) &
    -M(242))) * den(1315)
  T3sum(1:35,79) = T3sum(1:35,79) + Gcoeff * G3tensor(:,463)
  Gcoeff = (c(5)*(-M(46)+M(51)-M(58)+M(63)+M(74)+M(75)+M(76)-M(85)+M(88)-M(95)-M(96)-M(97))+c(6)*(-M(143)+M(144)-M(167)+M(168) &
    +M(198)+M(200)-M(240)-M(242))) * den(208)
  T4sum(1:70,168) = T4sum(1:70,168) + Gcoeff * G4tensor(:,45)
  Gcoeff = (c(6)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(1318)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,464)
  Gcoeff = (c(6)*(-M(132)+M(135)+M(141)-M(151)+M(165)-M(175)-M(199)+M(203)-M(204)-M(206)+M(208)-M(212)+M(214)+M(220)-M(226) &
    +M(241))) * den(1319)
  T3sum(1:35,59) = T3sum(1:35,59) + Gcoeff * G3tensor(:,465)
  Gcoeff = (c(5)*(M(46)-M(51)-M(74)+M(95)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122))+c(6)*(M(141)-M(151)-M(197) &
    +M(209)-M(210)-M(212)+M(214)+M(239))) * den(184)
  T4sum(1:70,174) = T4sum(1:70,174) + Gcoeff * G4tensor(:,46)
  Gcoeff = (c(5)*(M(58)-M(63)-M(75)+M(96)+M(102)-M(105)-M(111)+M(121)+M(126)-M(127)+M(128)-M(129))+c(6)*(M(165)-M(175)-M(199) &
    +M(203)-M(204)-M(206)+M(208)+M(241))) * den(160)
  T4sum(1:70,180) = T4sum(1:70,180) + Gcoeff * G4tensor(:,108)
  Gcoeff = (c(4)*(M(3)-M(4)-M(11)+M(12)-M(13)+M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)+M(102)-M(105)-M(111)+M(112)-M(116)+M(118)+M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(199)+M(203)-M(204)+M(241))) * den(22)
  T5sum(1:126,23) = T5sum(1:126,23) + Gcoeff * G5tensor(:,15)
  Gcoeff = (c(4)*(-M(3)+M(4)-M(11)+M(12)+M(13)-M(14)+M(21)-M(22)-M(27)+M(28)+M(33)-M(34)+M(37)-M(38)-M(39)+M(40)+M(46)-M(51)+M(58) &
    -M(63)-M(74)-M(75)-M(76)+M(85)-M(88)+M(95)+M(96)+M(97)-M(102)+M(105)+M(111)+M(112)-M(116)+M(118)-M(121)-M(122)+M(126)-M(127) &
    +M(128)-M(129))+c(6)*(-M(197)+M(209)-M(210)+M(239))) * den(22)
  T5sum(1:126,24) = T5sum(1:126,24) + Gcoeff * G5tensor(:,1)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(10)-M(11)-M(14)-M(15)-M(17)-M(19)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(38) &
    -M(39)+M(42)+M(46)+M(53)+M(55)+M(58)+M(65)+M(66)+M(69)+M(78)+M(79)+M(80)+M(81)+M(82)+M(85)+M(89)+M(91)+M(94)+M(95)+M(96)+M(97) &
    +M(103)+M(104)+M(105)+M(109)+M(111)+M(112)+M(118)+M(123)+M(126)+M(128))+c(6)*(M(209)+M(239)))
  T6sum(1:210,163) = T6sum(1:210,163) + Gcoeff * G6tensor(:,1)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(9)-M(12)-M(13)-M(16)-M(18)-M(20)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(37) &
    -M(40)+M(43)+M(45)+M(53)+M(54)+M(57)+M(65)+M(67)+M(70)+M(77)+M(79)+M(82)+M(83)+M(84)+M(85)+M(90)+M(91)+M(92)+M(93)+M(94)+M(97) &
    +M(103)+M(105)+M(106)+M(109)+M(110)+M(111)+M(117)+M(124)+M(125)+M(130))+c(6)*(M(215)+M(233)))
  T6sum(1:210,166) = T6sum(1:210,166) + Gcoeff * G6tensor(:,3)
  Gcoeff = (c(4)*(-M(2)-M(4)-M(5)-M(7)-M(10)-M(11)-M(13)-M(15)-M(17)-M(20)-M(22)-M(24)-M(26)-M(27)-M(29)-M(31)-M(34)-M(36)-M(37) &
    -M(40)+M(41)+M(43)+M(46)+M(54)+M(58)+M(66)+M(68)+M(69)+M(77)+M(78)+M(79)+M(81)+M(82)+M(85)+M(91)+M(92)+M(94)+M(95)+M(96)+M(97) &
    +M(101)+M(102)+M(106)+M(112)+M(115)+M(117)+M(118)+M(121)+M(126)+M(128))+c(6)*(M(203)+M(241)))
  T6sum(1:210,169) = T6sum(1:210,169) + Gcoeff * G6tensor(:,5)
  Gcoeff = (c(4)*(-M(1)-M(3)-M(6)-M(8)-M(9)-M(12)-M(14)-M(16)-M(18)-M(19)-M(21)-M(23)-M(25)-M(28)-M(30)-M(32)-M(33)-M(35)-M(38) &
    -M(39)+M(41)+M(42)+M(45)+M(55)+M(57)+M(67)+M(68)+M(70)+M(79)+M(80)+M(82)+M(83)+M(84)+M(85)+M(89)+M(90)+M(91)+M(93)+M(94)+M(97) &
    +M(101)+M(102)+M(104)+M(110)+M(115)+M(121)+M(123)+M(124)+M(125)+M(130))+c(6)*(M(217)+M(227)))
  T6sum(1:210,172) = T6sum(1:210,172) + Gcoeff * G6tensor(:,4)
  Gcoeff = (c(4)*(-M(1)-M(4)-M(5)-M(8)-M(10)-M(11)-M(13)-M(15)-M(18)-M(20)-M(22)-M(23)-M(25)-M(28)-M(30)-M(31)-M(33)-M(36)-M(37) &
    -M(40)+M(43)+M(44)+M(46)+M(54)+M(56)+M(57)+M(66)+M(70)+M(77)+M(78)+M(79)+M(82)+M(84)+M(85)+M(91)+M(92)+M(93)+M(94)+M(95)+M(97) &
    +M(106)+M(107)+M(108)+M(112)+M(113)+M(117)+M(118)+M(119)+M(125)+M(130))+c(6)*(M(205)+M(235)))
  T6sum(1:210,175) = T6sum(1:210,175) + Gcoeff * G6tensor(:,6)
  Gcoeff = (c(4)*(-M(2)-M(3)-M(6)-M(7)-M(9)-M(12)-M(14)-M(16)-M(17)-M(19)-M(21)-M(24)-M(26)-M(27)-M(29)-M(32)-M(34)-M(35)-M(38) &
    -M(39)+M(42)+M(44)+M(45)+M(55)+M(56)+M(58)+M(67)+M(69)+M(79)+M(80)+M(81)+M(82)+M(83)+M(85)+M(89)+M(90)+M(91)+M(94)+M(96)+M(97) &
    +M(104)+M(107)+M(108)+M(110)+M(113)+M(119)+M(123)+M(124)+M(126)+M(128))+c(6)*(M(211)+M(229)))
  T6sum(1:210,178) = T6sum(1:210,178) + Gcoeff * G6tensor(:,2)

end subroutine vamp_10

end module ol_vamp_10_ppjjjj_gggggg_1_/**/REALKIND
