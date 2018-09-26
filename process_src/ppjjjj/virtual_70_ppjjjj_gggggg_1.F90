
module ol_vamp_70_ppjjjj_gggggg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_70(M)
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
  complex(REALKIND), dimension(4,1,4,32) :: G0
  complex(REALKIND), dimension(1,327) :: G0tensor
  complex(REALKIND), dimension(5,163) :: G1tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,0),G0(:,:,:,2))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-4),wf(:,56),G0tensor(:,1))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-4),wf(:,59),G0tensor(:,2))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-4),wf(:,60),G0tensor(:,3))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,56),wf(:,-4),G0tensor(:,4))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,59),wf(:,-4),G0tensor(:,5))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,60),wf(:,-4),G0tensor(:,6))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-4),wf(:,56),G0tensor(:,7))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-4),wf(:,59),G0tensor(:,8))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-4),wf(:,60),G0tensor(:,9))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,50),G0tensor(:,10))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,53),G0tensor(:,11))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,54),G0tensor(:,12))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,50),wf(:,-3),G0tensor(:,13))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,53),wf(:,-3),G0tensor(:,14))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,54),wf(:,-3),G0tensor(:,15))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,50),G0tensor(:,16))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,53),G0tensor(:,17))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,54),G0tensor(:,18))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,38),G0tensor(:,19))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,41),G0tensor(:,20))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,42),G0tensor(:,21))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,38),wf(:,-2),G0tensor(:,22))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,41),wf(:,-2),G0tensor(:,23))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,42),wf(:,-2),G0tensor(:,24))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,38),G0tensor(:,25))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,41),G0tensor(:,26))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,42),G0tensor(:,27))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,20),G0tensor(:,28))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,20),wf(:,-1),G0tensor(:,29))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,20),G0tensor(:,30))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,23),G0tensor(:,31))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,23),wf(:,-1),G0tensor(:,32))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,23),G0tensor(:,33))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,24),G0tensor(:,34))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,24),wf(:,-1),G0tensor(:,35))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,24),G0tensor(:,36))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,75),wf(:,105),G0tensor(:,37))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,105),wf(:,75),G0tensor(:,38))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,75),wf(:,105),G0tensor(:,39))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-4),wf(:,202),G0tensor(:,40))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,202),wf(:,-4),G0tensor(:,41))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-4),wf(:,202),G0tensor(:,42))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,206),G0tensor(:,43))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,206),wf(:,-3),G0tensor(:,44))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,206),G0tensor(:,45))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,867),Q(:,30),G1tensor(:,1))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,869),Q(:,30),G1tensor(:,2))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,870),Q(:,30),G1tensor(:,3))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,66),wf(:,91),G0tensor(:,46))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,91),wf(:,66),G0tensor(:,47))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,66),wf(:,91),G0tensor(:,48))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-4),wf(:,214),G0tensor(:,49))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,214),wf(:,-4),G0tensor(:,50))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-4),wf(:,214),G0tensor(:,51))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,62),wf(:,95),G0tensor(:,52))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,95),wf(:,62),G0tensor(:,53))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,62),wf(:,95),G0tensor(:,54))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-4),wf(:,221),G0tensor(:,55))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,221),wf(:,-4),G0tensor(:,56))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-4),wf(:,221),G0tensor(:,57))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,225),G0tensor(:,58))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,225),wf(:,-3),G0tensor(:,59))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,225),G0tensor(:,60))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,229),G0tensor(:,61))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,229),wf(:,-3),G0tensor(:,62))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-3),wf(:,229),G0tensor(:,63))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,235),G0tensor(:,64))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,235),wf(:,-2),G0tensor(:,65))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,235),G0tensor(:,66))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,892),Q(:,30),G1tensor(:,4))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,893),Q(:,30),G1tensor(:,5))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,894),Q(:,30),G1tensor(:,6))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,240),G0tensor(:,67))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,240),wf(:,-2),G0tensor(:,68))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,240),G0tensor(:,69))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,244),G0tensor(:,70))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,244),wf(:,-2),G0tensor(:,71))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-2),wf(:,244),G0tensor(:,72))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,904),Q(:,30),G1tensor(:,7))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,905),Q(:,30),G1tensor(:,8))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,906),Q(:,30),G1tensor(:,9))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,910),Q(:,30),G1tensor(:,10))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,911),Q(:,30),G1tensor(:,11))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,912),Q(:,30),G1tensor(:,12))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,253),G0tensor(:,73))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,253),wf(:,-1),G0tensor(:,74))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,253),G0tensor(:,75))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,955),Q(:,30),G1tensor(:,13))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,956),Q(:,30),G1tensor(:,14))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,957),Q(:,30),G1tensor(:,15))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,258),G0tensor(:,76))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,258),wf(:,-1),G0tensor(:,77))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,258),G0tensor(:,78))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,262),G0tensor(:,79))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,2),wf(:,262),wf(:,-1),G0tensor(:,80))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,2),wf(:,-1),wf(:,262),G0tensor(:,81))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,967),Q(:,30),G1tensor(:,16))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,968),Q(:,30),G1tensor(:,17))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,969),Q(:,30),G1tensor(:,18))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,973),Q(:,30),G1tensor(:,19))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,974),Q(:,30),G1tensor(:,20))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,975),Q(:,30),G1tensor(:,21))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1000),Q(:,30),G1tensor(:,22))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1001),Q(:,30),G1tensor(:,23))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1002),Q(:,30),G1tensor(:,24))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1006),Q(:,30),G1tensor(:,25))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1007),Q(:,30),G1tensor(:,26))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1008),Q(:,30),G1tensor(:,27))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1018),Q(:,30),G1tensor(:,28))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1019),Q(:,30),G1tensor(:,29))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1020),Q(:,30),G1tensor(:,30))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1260),Q(:,30),G1tensor(:,31))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1351),Q(:,30),G1tensor(:,32))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1353),Q(:,30),G1tensor(:,33))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1295),Q(:,30),G1tensor(:,34))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1375),Q(:,30),G1tensor(:,35))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1377),Q(:,30),G1tensor(:,36))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1311),Q(:,30),G1tensor(:,37))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1387),Q(:,30),G1tensor(:,38))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1389),Q(:,30),G1tensor(:,39))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1411),Q(:,30),G1tensor(:,40))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1413),Q(:,30),G1tensor(:,41))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1423),Q(:,30),G1tensor(:,42))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1425),Q(:,30),G1tensor(:,43))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1435),Q(:,30),G1tensor(:,44))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,33),wf(:,1436),Q(:,30),G1tensor(:,45))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,-5),G0(:,:,:,3))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-4),wf(:,56),G0tensor(:,82))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-4),wf(:,59),G0tensor(:,83))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-4),wf(:,60),G0tensor(:,84))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,56),wf(:,-4),G0tensor(:,85))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,59),wf(:,-4),G0tensor(:,86))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,60),wf(:,-4),G0tensor(:,87))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-4),wf(:,56),G0tensor(:,88))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-4),wf(:,59),G0tensor(:,89))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-4),wf(:,60),G0tensor(:,90))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-3),wf(:,50),G0tensor(:,91))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-3),wf(:,53),G0tensor(:,92))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-3),wf(:,54),G0tensor(:,93))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,50),wf(:,-3),G0tensor(:,94))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,53),wf(:,-3),G0tensor(:,95))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,54),wf(:,-3),G0tensor(:,96))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-3),wf(:,50),G0tensor(:,97))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-3),wf(:,53),G0tensor(:,98))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-3),wf(:,54),G0tensor(:,99))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-2),wf(:,38),G0tensor(:,100))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-2),wf(:,41),G0tensor(:,101))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-2),wf(:,42),G0tensor(:,102))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,38),wf(:,-2),G0tensor(:,103))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,41),wf(:,-2),G0tensor(:,104))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,42),wf(:,-2),G0tensor(:,105))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-2),wf(:,38),G0tensor(:,106))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-2),wf(:,41),G0tensor(:,107))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-2),wf(:,42),G0tensor(:,108))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-1),wf(:,20),G0tensor(:,109))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,20),wf(:,-1),G0tensor(:,110))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-1),wf(:,20),G0tensor(:,111))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-1),wf(:,23),G0tensor(:,112))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,23),wf(:,-1),G0tensor(:,113))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-1),wf(:,23),G0tensor(:,114))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-1),wf(:,24),G0tensor(:,115))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,24),wf(:,-1),G0tensor(:,116))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-1),wf(:,24),G0tensor(:,117))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,75),wf(:,105),G0tensor(:,118))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,105),wf(:,75),G0tensor(:,119))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,75),wf(:,105),G0tensor(:,120))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-4),wf(:,202),G0tensor(:,121))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,202),wf(:,-4),G0tensor(:,122))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-4),wf(:,202),G0tensor(:,123))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-3),wf(:,206),G0tensor(:,124))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,206),wf(:,-3),G0tensor(:,125))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-3),wf(:,206),G0tensor(:,126))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,867),Q(:,30),G1tensor(:,46))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,869),Q(:,30),G1tensor(:,47))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,870),Q(:,30),G1tensor(:,48))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,66),wf(:,91),G0tensor(:,127))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,91),wf(:,66),G0tensor(:,128))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,66),wf(:,91),G0tensor(:,129))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-4),wf(:,214),G0tensor(:,130))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,214),wf(:,-4),G0tensor(:,131))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-4),wf(:,214),G0tensor(:,132))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,62),wf(:,95),G0tensor(:,133))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,95),wf(:,62),G0tensor(:,134))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,62),wf(:,95),G0tensor(:,135))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-4),wf(:,221),G0tensor(:,136))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,221),wf(:,-4),G0tensor(:,137))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-4),wf(:,221),G0tensor(:,138))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-3),wf(:,225),G0tensor(:,139))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,225),wf(:,-3),G0tensor(:,140))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-3),wf(:,225),G0tensor(:,141))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-3),wf(:,229),G0tensor(:,142))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,229),wf(:,-3),G0tensor(:,143))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-3),wf(:,229),G0tensor(:,144))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-2),wf(:,235),G0tensor(:,145))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,235),wf(:,-2),G0tensor(:,146))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-2),wf(:,235),G0tensor(:,147))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,892),Q(:,30),G1tensor(:,49))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,893),Q(:,30),G1tensor(:,50))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,894),Q(:,30),G1tensor(:,51))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-2),wf(:,240),G0tensor(:,148))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,240),wf(:,-2),G0tensor(:,149))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-2),wf(:,240),G0tensor(:,150))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-2),wf(:,244),G0tensor(:,151))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,244),wf(:,-2),G0tensor(:,152))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-2),wf(:,244),G0tensor(:,153))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,904),Q(:,30),G1tensor(:,52))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,905),Q(:,30),G1tensor(:,53))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,906),Q(:,30),G1tensor(:,54))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,910),Q(:,30),G1tensor(:,55))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,911),Q(:,30),G1tensor(:,56))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,912),Q(:,30),G1tensor(:,57))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-1),wf(:,253),G0tensor(:,154))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,253),wf(:,-1),G0tensor(:,155))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-1),wf(:,253),G0tensor(:,156))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,955),Q(:,30),G1tensor(:,58))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,956),Q(:,30),G1tensor(:,59))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,957),Q(:,30),G1tensor(:,60))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-1),wf(:,258),G0tensor(:,157))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,258),wf(:,-1),G0tensor(:,158))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-1),wf(:,258),G0tensor(:,159))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,-1),wf(:,262),G0tensor(:,160))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,3),wf(:,262),wf(:,-1),G0tensor(:,161))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,3),wf(:,-1),wf(:,262),G0tensor(:,162))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,967),Q(:,30),G1tensor(:,61))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,968),Q(:,30),G1tensor(:,62))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,969),Q(:,30),G1tensor(:,63))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,973),Q(:,30),G1tensor(:,64))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,974),Q(:,30),G1tensor(:,65))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,975),Q(:,30),G1tensor(:,66))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1000),Q(:,30),G1tensor(:,67))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1001),Q(:,30),G1tensor(:,68))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1002),Q(:,30),G1tensor(:,69))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1006),Q(:,30),G1tensor(:,70))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1007),Q(:,30),G1tensor(:,71))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1008),Q(:,30),G1tensor(:,72))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1018),Q(:,30),G1tensor(:,73))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1019),Q(:,30),G1tensor(:,74))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1020),Q(:,30),G1tensor(:,75))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1260),Q(:,30),G1tensor(:,76))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1351),Q(:,30),G1tensor(:,77))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1353),Q(:,30),G1tensor(:,78))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1295),Q(:,30),G1tensor(:,79))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1375),Q(:,30),G1tensor(:,80))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1377),Q(:,30),G1tensor(:,81))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1311),Q(:,30),G1tensor(:,82))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1387),Q(:,30),G1tensor(:,83))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1389),Q(:,30),G1tensor(:,84))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1411),Q(:,30),G1tensor(:,85))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1413),Q(:,30),G1tensor(:,86))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1423),Q(:,30),G1tensor(:,87))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1425),Q(:,30),G1tensor(:,88))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1435),Q(:,30),G1tensor(:,89))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,33),wf(:,1436),Q(:,30),G1tensor(:,90))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,0),G0(:,:,:,4))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-4),wf(:,56),G0tensor(:,163))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-4),wf(:,59),G0tensor(:,164))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-4),wf(:,60),G0tensor(:,165))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,56),wf(:,-4),G0tensor(:,166))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,59),wf(:,-4),G0tensor(:,167))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,60),wf(:,-4),G0tensor(:,168))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-4),wf(:,56),G0tensor(:,169))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-4),wf(:,59),G0tensor(:,170))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-4),wf(:,60),G0tensor(:,171))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-3),wf(:,50),G0tensor(:,172))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-3),wf(:,53),G0tensor(:,173))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-3),wf(:,54),G0tensor(:,174))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,50),wf(:,-3),G0tensor(:,175))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,53),wf(:,-3),G0tensor(:,176))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,54),wf(:,-3),G0tensor(:,177))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-3),wf(:,50),G0tensor(:,178))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-3),wf(:,53),G0tensor(:,179))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-3),wf(:,54),G0tensor(:,180))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-2),wf(:,38),G0tensor(:,181))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-2),wf(:,41),G0tensor(:,182))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-2),wf(:,42),G0tensor(:,183))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,38),wf(:,-2),G0tensor(:,184))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,41),wf(:,-2),G0tensor(:,185))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,42),wf(:,-2),G0tensor(:,186))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-2),wf(:,38),G0tensor(:,187))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-2),wf(:,41),G0tensor(:,188))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-2),wf(:,42),G0tensor(:,189))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-1),wf(:,20),G0tensor(:,190))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,20),wf(:,-1),G0tensor(:,191))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-1),wf(:,20),G0tensor(:,192))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-1),wf(:,23),G0tensor(:,193))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,23),wf(:,-1),G0tensor(:,194))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-1),wf(:,23),G0tensor(:,195))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-1),wf(:,24),G0tensor(:,196))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,24),wf(:,-1),G0tensor(:,197))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-1),wf(:,24),G0tensor(:,198))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,75),wf(:,105),G0tensor(:,199))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,105),wf(:,75),G0tensor(:,200))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,75),wf(:,105),G0tensor(:,201))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-4),wf(:,202),G0tensor(:,202))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,202),wf(:,-4),G0tensor(:,203))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-4),wf(:,202),G0tensor(:,204))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-3),wf(:,206),G0tensor(:,205))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,206),wf(:,-3),G0tensor(:,206))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-3),wf(:,206),G0tensor(:,207))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,867),Q(:,30),G1tensor(:,91))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,869),Q(:,30),G1tensor(:,92))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,870),Q(:,30),G1tensor(:,93))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,66),wf(:,91),G0tensor(:,208))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,91),wf(:,66),G0tensor(:,209))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,66),wf(:,91),G0tensor(:,210))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-4),wf(:,214),G0tensor(:,211))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,214),wf(:,-4),G0tensor(:,212))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-4),wf(:,214),G0tensor(:,213))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,62),wf(:,95),G0tensor(:,214))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,95),wf(:,62),G0tensor(:,215))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,62),wf(:,95),G0tensor(:,216))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-4),wf(:,221),G0tensor(:,217))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,221),wf(:,-4),G0tensor(:,218))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-4),wf(:,221),G0tensor(:,219))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-3),wf(:,225),G0tensor(:,220))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,225),wf(:,-3),G0tensor(:,221))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-3),wf(:,225),G0tensor(:,222))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-3),wf(:,229),G0tensor(:,223))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,229),wf(:,-3),G0tensor(:,224))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-3),wf(:,229),G0tensor(:,225))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-2),wf(:,235),G0tensor(:,226))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,235),wf(:,-2),G0tensor(:,227))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-2),wf(:,235),G0tensor(:,228))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,892),Q(:,30),G1tensor(:,94))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,893),Q(:,30),G1tensor(:,95))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,894),Q(:,30),G1tensor(:,96))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-2),wf(:,240),G0tensor(:,229))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,240),wf(:,-2),G0tensor(:,230))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-2),wf(:,240),G0tensor(:,231))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-2),wf(:,244),G0tensor(:,232))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,244),wf(:,-2),G0tensor(:,233))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-2),wf(:,244),G0tensor(:,234))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,904),Q(:,30),G1tensor(:,97))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,905),Q(:,30),G1tensor(:,98))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,906),Q(:,30),G1tensor(:,99))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,910),Q(:,30),G1tensor(:,100))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,911),Q(:,30),G1tensor(:,101))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,912),Q(:,30),G1tensor(:,102))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-1),wf(:,253),G0tensor(:,235))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,253),wf(:,-1),G0tensor(:,236))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-1),wf(:,253),G0tensor(:,237))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,955),Q(:,30),G1tensor(:,103))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,956),Q(:,30),G1tensor(:,104))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,957),Q(:,30),G1tensor(:,105))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-1),wf(:,258),G0tensor(:,238))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,258),wf(:,-1),G0tensor(:,239))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-1),wf(:,258),G0tensor(:,240))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,-1),wf(:,262),G0tensor(:,241))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,4),wf(:,262),wf(:,-1),G0tensor(:,242))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,4),wf(:,-1),wf(:,262),G0tensor(:,243))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,967),Q(:,30),G1tensor(:,106))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,968),Q(:,30),G1tensor(:,107))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,969),Q(:,30),G1tensor(:,108))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,973),Q(:,30),G1tensor(:,109))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,974),Q(:,30),G1tensor(:,110))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,975),Q(:,30),G1tensor(:,111))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1000),Q(:,30),G1tensor(:,112))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1001),Q(:,30),G1tensor(:,113))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1002),Q(:,30),G1tensor(:,114))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1006),Q(:,30),G1tensor(:,115))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1007),Q(:,30),G1tensor(:,116))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1008),Q(:,30),G1tensor(:,117))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1018),Q(:,30),G1tensor(:,118))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1019),Q(:,30),G1tensor(:,119))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1020),Q(:,30),G1tensor(:,120))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1260),Q(:,30),G1tensor(:,121))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1351),Q(:,30),G1tensor(:,122))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1353),Q(:,30),G1tensor(:,123))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1295),Q(:,30),G1tensor(:,124))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1375),Q(:,30),G1tensor(:,125))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1377),Q(:,30),G1tensor(:,126))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1311),Q(:,30),G1tensor(:,127))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1387),Q(:,30),G1tensor(:,128))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1389),Q(:,30),G1tensor(:,129))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1411),Q(:,30),G1tensor(:,130))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1413),Q(:,30),G1tensor(:,131))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1423),Q(:,30),G1tensor(:,132))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1425),Q(:,30),G1tensor(:,133))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1435),Q(:,30),G1tensor(:,134))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,33),wf(:,1436),Q(:,30),G1tensor(:,135))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,55),G0(:,:,:,5))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,5),wf(:,-2),wf(:,-1),G0tensor(:,244))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,5),wf(:,-1),wf(:,-2),G0tensor(:,245))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,5),wf(:,-2),wf(:,-1),G0tensor(:,246))
  call check_last_UV_W(l_switch,G0(:,:,:,5),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,136))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,57),G0(:,:,:,6))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,6),wf(:,-2),wf(:,-1),G0tensor(:,247))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,6),wf(:,-1),wf(:,-2),G0tensor(:,248))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,6),wf(:,-2),wf(:,-1),G0tensor(:,249))
  call check_last_UV_W(l_switch,G0(:,:,:,6),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,137))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,58),G0(:,:,:,7))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,7),wf(:,-2),wf(:,-1),G0tensor(:,250))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,7),wf(:,-1),wf(:,-2),G0tensor(:,251))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,7),wf(:,-2),wf(:,-1),G0tensor(:,252))
  call check_last_UV_W(l_switch,G0(:,:,:,7),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,138))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,55),wf(:,-3),G0(:,:,:,8))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,8),wf(:,-2),wf(:,-1),G0tensor(:,253))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,8),wf(:,-1),wf(:,-2),G0tensor(:,254))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,8),wf(:,-2),wf(:,-1),G0tensor(:,255))
  call check_last_UV_W(l_switch,G0(:,:,:,8),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,139))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,57),wf(:,-3),G0(:,:,:,9))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,9),wf(:,-2),wf(:,-1),G0tensor(:,256))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,9),wf(:,-1),wf(:,-2),G0tensor(:,257))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,9),wf(:,-2),wf(:,-1),G0tensor(:,258))
  call check_last_UV_W(l_switch,G0(:,:,:,9),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,140))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,58),wf(:,-3),G0(:,:,:,10))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,10),wf(:,-2),wf(:,-1),G0tensor(:,259))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,10),wf(:,-1),wf(:,-2),G0tensor(:,260))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,10),wf(:,-2),wf(:,-1),G0tensor(:,261))
  call check_last_UV_W(l_switch,G0(:,:,:,10),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,141))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,55),G0(:,:,:,11))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,11),wf(:,-2),wf(:,-1),G0tensor(:,262))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,11),wf(:,-1),wf(:,-2),G0tensor(:,263))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,11),wf(:,-2),wf(:,-1),G0tensor(:,264))
  call check_last_UV_W(l_switch,G0(:,:,:,11),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,142))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,57),G0(:,:,:,12))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-2),wf(:,-1),G0tensor(:,265))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,12),wf(:,-1),wf(:,-2),G0tensor(:,266))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,12),wf(:,-2),wf(:,-1),G0tensor(:,267))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,143))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,58),G0(:,:,:,13))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-2),wf(:,-1),G0tensor(:,268))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,13),wf(:,-1),wf(:,-2),G0tensor(:,269))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,13),wf(:,-2),wf(:,-1),G0tensor(:,270))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,144))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,44),G0(:,:,:,14))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,14),wf(:,-4),wf(:,0),G0tensor(:,271))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,14),wf(:,0),wf(:,-4),G0tensor(:,272))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,14),wf(:,-4),wf(:,0),G0tensor(:,273))
  call check_last_UV_W(l_switch,G0(:,:,:,14),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,145))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,47),G0(:,:,:,15))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,15),wf(:,-4),wf(:,0),G0tensor(:,274))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,15),wf(:,0),wf(:,-4),G0tensor(:,275))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,15),wf(:,-4),wf(:,0),G0tensor(:,276))
  call check_last_UV_W(l_switch,G0(:,:,:,15),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,146))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,48),G0(:,:,:,16))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,16),wf(:,-4),wf(:,0),G0tensor(:,277))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,16),wf(:,0),wf(:,-4),G0tensor(:,278))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,16),wf(:,-4),wf(:,0),G0tensor(:,279))
  call check_last_UV_W(l_switch,G0(:,:,:,16),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,147))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,44),wf(:,-3),G0(:,:,:,17))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,17),wf(:,-4),wf(:,0),G0tensor(:,280))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,17),wf(:,0),wf(:,-4),G0tensor(:,281))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,17),wf(:,-4),wf(:,0),G0tensor(:,282))
  call check_last_UV_W(l_switch,G0(:,:,:,17),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,148))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,47),wf(:,-3),G0(:,:,:,18))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,18),wf(:,-4),wf(:,0),G0tensor(:,283))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,18),wf(:,0),wf(:,-4),G0tensor(:,284))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,18),wf(:,-4),wf(:,0),G0tensor(:,285))
  call check_last_UV_W(l_switch,G0(:,:,:,18),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,149))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,48),wf(:,-3),G0(:,:,:,19))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,19),wf(:,-4),wf(:,0),G0tensor(:,286))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,19),wf(:,0),wf(:,-4),G0tensor(:,287))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,19),wf(:,-4),wf(:,0),G0tensor(:,288))
  call check_last_UV_W(l_switch,G0(:,:,:,19),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,150))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,44),G0(:,:,:,20))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,20),wf(:,-4),wf(:,0),G0tensor(:,289))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,20),wf(:,0),wf(:,-4),G0tensor(:,290))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,20),wf(:,-4),wf(:,0),G0tensor(:,291))
  call check_last_UV_W(l_switch,G0(:,:,:,20),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,151))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,47),G0(:,:,:,21))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,21),wf(:,-4),wf(:,0),G0tensor(:,292))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,21),wf(:,0),wf(:,-4),G0tensor(:,293))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,21),wf(:,-4),wf(:,0),G0tensor(:,294))
  call check_last_UV_W(l_switch,G0(:,:,:,21),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,152))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,48),G0(:,:,:,22))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,22),wf(:,-4),wf(:,0),G0tensor(:,295))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,22),wf(:,0),wf(:,-4),G0tensor(:,296))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,22),wf(:,-4),wf(:,0),G0tensor(:,297))
  call check_last_UV_W(l_switch,G0(:,:,:,22),Q(:,46),wf(:,109),Q(:,17),G1tensor(:,153))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,44),G0(:,:,:,23))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,23),wf(:,-4),wf(:,-3),G0tensor(:,298))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,23),wf(:,-3),wf(:,-4),G0tensor(:,299))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,23),wf(:,-4),wf(:,-3),G0tensor(:,300))
  call check_last_UV_W(l_switch,G0(:,:,:,23),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,154))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,44),wf(:,0),G0(:,:,:,24))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,24),wf(:,-4),wf(:,-3),G0tensor(:,301))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,24),wf(:,-3),wf(:,-4),G0tensor(:,302))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,24),wf(:,-4),wf(:,-3),G0tensor(:,303))
  call check_last_UV_W(l_switch,G0(:,:,:,24),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,155))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,44),G0(:,:,:,25))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,25),wf(:,-4),wf(:,-3),G0tensor(:,304))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,25),wf(:,-3),wf(:,-4),G0tensor(:,305))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,25),wf(:,-4),wf(:,-3),G0tensor(:,306))
  call check_last_UV_W(l_switch,G0(:,:,:,25),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,156))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,47),G0(:,:,:,26))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,26),wf(:,-4),wf(:,-3),G0tensor(:,307))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,26),wf(:,-3),wf(:,-4),G0tensor(:,308))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,26),wf(:,-4),wf(:,-3),G0tensor(:,309))
  call check_last_UV_W(l_switch,G0(:,:,:,26),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,157))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,47),wf(:,0),G0(:,:,:,27))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,27),wf(:,-4),wf(:,-3),G0tensor(:,310))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,27),wf(:,-3),wf(:,-4),G0tensor(:,311))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,27),wf(:,-4),wf(:,-3),G0tensor(:,312))
  call check_last_UV_W(l_switch,G0(:,:,:,27),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,158))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,47),G0(:,:,:,28))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,28),wf(:,-4),wf(:,-3),G0tensor(:,313))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,28),wf(:,-3),wf(:,-4),G0tensor(:,314))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,28),wf(:,-4),wf(:,-3),G0tensor(:,315))
  call check_last_UV_W(l_switch,G0(:,:,:,28),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,159))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,48),G0(:,:,:,29))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,29),wf(:,-4),wf(:,-3),G0tensor(:,316))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,29),wf(:,-3),wf(:,-4),G0tensor(:,317))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,29),wf(:,-4),wf(:,-3),G0tensor(:,318))
  call check_last_UV_W(l_switch,G0(:,:,:,29),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,160))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,48),wf(:,0),G0(:,:,:,30))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,30),wf(:,-4),wf(:,-3),G0tensor(:,319))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,30),wf(:,-3),wf(:,-4),G0tensor(:,320))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,30),wf(:,-4),wf(:,-3),G0tensor(:,321))
  call check_last_UV_W(l_switch,G0(:,:,:,30),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,161))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,0),wf(:,48),G0(:,:,:,31))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,31),wf(:,-4),wf(:,-3),G0tensor(:,322))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,31),wf(:,-3),wf(:,-4),G0tensor(:,323))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,31),wf(:,-4),wf(:,-3),G0tensor(:,324))
  call check_last_UV_W(l_switch,G0(:,:,:,31),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,162))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,0),wf(:,2),G0(:,:,:,32))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,32),wf(:,-2),wf(:,-1),G0tensor(:,325))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,32),wf(:,-1),wf(:,-2),G0tensor(:,326))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,32),wf(:,-2),wf(:,-1),G0tensor(:,327))
  call check_last_UV_W(l_switch,G0(:,:,:,32),Q(:,57),wf(:,105),Q(:,6),G1tensor(:,163))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(1)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(2)*(M(203)-M(209)-M(215) &
    +M(217)+M(227)-M(233)-M(239)+M(241))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,1)
  Gcoeff = (c(1)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(2)*(M(131)-M(155)-M(179) &
    +M(185)+M(245)-M(247)-M(249)+M(250))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,82)
  Gcoeff = (c(2)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,163)
  Gcoeff = (c(1)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(2)*(-M(203)+M(205)+M(211) &
    -M(217)-M(227)+M(229)+M(235)-M(241))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,2)
  Gcoeff = (c(1)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(2)*(-M(131)+M(137)+M(161) &
    -M(185)-M(245)+M(246)+M(248)-M(250))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,83)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,164)
  Gcoeff = (c(1)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(2)*(-M(205)+M(209)-M(211) &
    +M(215)-M(229)+M(233)-M(235)+M(239))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,3)
  Gcoeff = (c(1)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(2)*(-M(137)+M(155)-M(161) &
    +M(179)-M(246)+M(247)-M(248)+M(249))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,84)
  Gcoeff = (c(2)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,165)
  Gcoeff = (c(1)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(2)*(M(131)-M(155)-M(179) &
    +M(185)+M(245)-M(247)-M(249)+M(250))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,4)
  Gcoeff = (c(1)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(2)*(M(203)-M(209)-M(215) &
    +M(217)+M(227)-M(233)-M(239)+M(241))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,85)
  Gcoeff = (c(2)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,166)
  Gcoeff = (c(1)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(2)*(-M(131)+M(137)+M(161) &
    -M(185)-M(245)+M(246)+M(248)-M(250))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,5)
  Gcoeff = (c(1)*(-M(41)+M(44)+M(56)-M(68)-M(101)-M(102)+M(107)+M(108)+M(113)-M(115)+M(119)-M(121))+c(2)*(-M(203)+M(205)+M(211) &
    -M(217)-M(227)+M(229)+M(235)-M(241))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,86)
  Gcoeff = (c(2)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,167)
  Gcoeff = (c(1)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(2)*(-M(137)+M(155)-M(161) &
    +M(179)-M(246)+M(247)-M(248)+M(249))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,6)
  Gcoeff = (c(1)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(2)*(-M(205)+M(209)-M(211) &
    +M(215)-M(229)+M(233)-M(235)+M(239))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,87)
  Gcoeff = (c(2)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,168)
  Gcoeff = (c(2)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,7)
  Gcoeff = (c(2)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,88)
  Gcoeff = (c(3)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,169)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,8)
  Gcoeff = (c(2)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,89)
  Gcoeff = (c(3)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,170)
  Gcoeff = (c(2)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,9)
  Gcoeff = (c(2)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,90)
  Gcoeff = (c(3)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(92)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,171)
  Gcoeff = (c(1)*(M(52)+M(64)+M(76)-M(79)-M(82)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94)-M(97))+c(2)*(M(156)+M(180)-M(209)-M(215) &
    +M(223)+M(225)-M(233)-M(239))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,244)
  Gcoeff = (c(1)*(M(49)-M(52)+M(61)-M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100))+c(2)*(M(155)-M(156)+M(179)-M(180) &
    -M(223)-M(225)+M(247)+M(249))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,247)
  Gcoeff = (c(1)*(-M(49)-M(61)-M(73)+M(79)+M(82)+M(85)+M(91)+M(94)+M(97)-M(98)-M(99)-M(100))+c(2)*(-M(155)-M(179)+M(209)+M(215) &
    +M(233)+M(239)-M(247)-M(249))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,250)
  Gcoeff = (c(1)*(M(52)+M(64)+M(76)-M(79)-M(82)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94)-M(97))+c(2)*(M(132)+M(186)-M(203)-M(217) &
    +M(221)+M(226)-M(227)-M(241))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,245)
  Gcoeff = (c(1)*(M(49)-M(52)+M(61)-M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100))+c(2)*(M(131)-M(132)+M(185)-M(186) &
    -M(221)-M(226)+M(245)+M(250))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,248)
  Gcoeff = (c(1)*(-M(49)-M(61)-M(73)+M(79)+M(82)+M(85)+M(91)+M(94)+M(97)-M(98)-M(99)-M(100))+c(2)*(-M(131)-M(185)+M(203)+M(217) &
    +M(227)+M(241)-M(245)-M(250))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,251)
  Gcoeff = (c(2)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,246)
  Gcoeff = (c(2)*(M(131)-M(132)-M(155)+M(156)-M(179)+M(180)+M(185)-M(186)-M(221)+M(223)+M(225)-M(226)+M(245)-M(247)-M(249) &
    +M(250))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,249)
  Gcoeff = (c(2)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,252)
  Gcoeff = (c(1)*(M(52)+M(64)+M(76)-M(79)-M(82)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94)-M(97))+c(2)*(M(132)+M(186)-M(203)-M(217) &
    +M(221)+M(226)-M(227)-M(241))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,253)
  Gcoeff = (c(1)*(M(49)-M(52)+M(61)-M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100))+c(2)*(M(131)-M(132)+M(185)-M(186) &
    -M(221)-M(226)+M(245)+M(250))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,256)
  Gcoeff = (c(1)*(-M(49)-M(61)-M(73)+M(79)+M(82)+M(85)+M(91)+M(94)+M(97)-M(98)-M(99)-M(100))+c(2)*(-M(131)-M(185)+M(203)+M(217) &
    +M(227)+M(241)-M(245)-M(250))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,259)
  Gcoeff = (c(1)*(M(52)+M(64)+M(76)-M(79)-M(82)-M(85)+M(86)+M(87)+M(88)-M(91)-M(94)-M(97))+c(2)*(M(156)+M(180)-M(209)-M(215) &
    +M(223)+M(225)-M(233)-M(239))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,254)
  Gcoeff = (c(1)*(M(49)-M(52)+M(61)-M(64)+M(73)-M(76)-M(86)-M(87)-M(88)+M(98)+M(99)+M(100))+c(2)*(M(155)-M(156)+M(179)-M(180) &
    -M(223)-M(225)+M(247)+M(249))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,257)
  Gcoeff = (c(1)*(-M(49)-M(61)-M(73)+M(79)+M(82)+M(85)+M(91)+M(94)+M(97)-M(98)-M(99)-M(100))+c(2)*(-M(155)-M(179)+M(209)+M(215) &
    +M(233)+M(239)-M(247)-M(249))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,260)
  Gcoeff = (c(2)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,255)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,258)
  Gcoeff = (c(2)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,261)
  Gcoeff = (c(2)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,262)
  Gcoeff = (c(2)*(M(131)-M(132)-M(155)+M(156)-M(179)+M(180)+M(185)-M(186)-M(221)+M(223)+M(225)-M(226)+M(245)-M(247)-M(249) &
    +M(250))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,265)
  Gcoeff = (c(2)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,268)
  Gcoeff = (c(2)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,263)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,266)
  Gcoeff = (c(2)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,269)
  Gcoeff = (c(3)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,264)
  Gcoeff = (c(3)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,267)
  Gcoeff = (c(3)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(10)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,270)
  Gcoeff = (c(1)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(2)*(M(180)-M(186)-M(197) &
    +M(199)+M(204)-M(210)-M(221)+M(223))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,271)
  Gcoeff = (c(1)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(2)*(M(135)-M(159)+M(215) &
    -M(217)-M(219)+M(220)-M(227)+M(233))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,272)
  Gcoeff = (c(2)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,273)
  Gcoeff = (c(1)*(-M(43)+M(50)+M(62)-M(92)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120))+c(2)*(-M(180)+M(183)+M(189) &
    -M(199)-M(204)+M(207)+M(213)-M(223))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,274)
  Gcoeff = (c(1)*(-M(43)+M(50)+M(62)-M(92)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120))+c(2)*(-M(135)+M(149)+M(173) &
    -M(215)+M(216)+M(218)-M(220)-M(233))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,275)
  Gcoeff = (c(2)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,276)
  Gcoeff = (c(1)*(-M(50)+M(55)-M(62)+M(89)+M(102)+M(104)-M(108)-M(114)-M(119)-M(120)+M(121)+M(123))+c(2)*(-M(183)+M(186)-M(189) &
    +M(197)-M(207)+M(210)-M(213)+M(221))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,277)
  Gcoeff = (c(1)*(-M(50)+M(55)-M(62)+M(89)+M(102)+M(104)-M(108)-M(114)-M(119)-M(120)+M(121)+M(123))+c(2)*(-M(149)+M(159)-M(173) &
    -M(216)+M(217)-M(218)+M(219)+M(227))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,278)
  Gcoeff = (c(2)*(-M(149)+M(159)-M(173)+M(183)-M(186)+M(189)-M(197)+M(207)-M(210)+M(213)-M(216)+M(217)-M(218)+M(219)-M(221) &
    +M(227))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,279)
  Gcoeff = (c(1)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(2)*(M(135)-M(159)+M(215) &
    -M(217)-M(219)+M(220)-M(227)+M(233))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,280)
  Gcoeff = (c(1)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(2)*(M(180)-M(186)-M(197) &
    +M(199)+M(204)-M(210)-M(221)+M(223))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,281)
  Gcoeff = (c(2)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,282)
  Gcoeff = (c(1)*(-M(43)+M(50)+M(62)-M(92)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120))+c(2)*(-M(135)+M(149)+M(173) &
    -M(215)+M(216)+M(218)-M(220)-M(233))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,283)
  Gcoeff = (c(1)*(-M(43)+M(50)+M(62)-M(92)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120))+c(2)*(-M(180)+M(183)+M(189) &
    -M(199)-M(204)+M(207)+M(213)-M(223))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,284)
  Gcoeff = (c(2)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,285)
  Gcoeff = (c(1)*(-M(50)+M(55)-M(62)+M(89)+M(102)+M(104)-M(108)-M(114)-M(119)-M(120)+M(121)+M(123))+c(2)*(-M(149)+M(159)-M(173) &
    -M(216)+M(217)-M(218)+M(219)+M(227))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,286)
  Gcoeff = (c(1)*(-M(50)+M(55)-M(62)+M(89)+M(102)+M(104)-M(108)-M(114)-M(119)-M(120)+M(121)+M(123))+c(2)*(-M(183)+M(186)-M(189) &
    +M(197)-M(207)+M(210)-M(213)+M(221))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,287)
  Gcoeff = (c(2)*(M(149)-M(159)+M(173)-M(183)+M(186)-M(189)+M(197)-M(207)+M(210)-M(213)+M(216)-M(217)+M(218)-M(219)+M(221) &
    -M(227))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,288)
  Gcoeff = (c(2)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,289)
  Gcoeff = (c(2)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,290)
  Gcoeff = (c(3)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,291)
  Gcoeff = (c(2)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,292)
  Gcoeff = (c(2)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,293)
  Gcoeff = (c(3)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,294)
  Gcoeff = (c(2)*(-M(149)+M(159)-M(173)+M(183)-M(186)+M(189)-M(197)+M(207)-M(210)+M(213)-M(216)+M(217)-M(218)+M(219)-M(221) &
    +M(227))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,295)
  Gcoeff = (c(2)*(M(149)-M(159)+M(173)-M(183)+M(186)-M(189)+M(197)-M(207)+M(210)-M(213)+M(216)-M(217)+M(218)-M(219)+M(221) &
    -M(227))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,296)
  Gcoeff = (c(3)*(M(149)-M(159)+M(173)-M(183)+M(186)-M(189)+M(197)-M(207)+M(210)-M(213)+M(216)-M(217)+M(218)-M(219)+M(221) &
    -M(227))) * den(82)
  T2sum(1:1,14) = T2sum(1:1,14) + Gcoeff * G0tensor(:,297)
  Gcoeff = (c(1)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(2)*(M(179)-M(185)-M(191) &
    +M(193)+M(228)-M(234)-M(245)+M(247))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,10)
  Gcoeff = (c(1)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(2)*(M(133)-M(157)-M(203) &
    +M(209)+M(239)-M(241)-M(243)+M(244))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,91)
  Gcoeff = (c(2)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,172)
  Gcoeff = (c(1)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(2)*(-M(179)+M(181)+M(187) &
    -M(193)-M(228)+M(231)+M(237)-M(247))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,11)
  Gcoeff = (c(1)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(2)*(-M(133)+M(143)+M(167) &
    -M(209)-M(239)+M(240)+M(242)-M(244))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,92)
  Gcoeff = (c(2)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,173)
  Gcoeff = (c(1)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(2)*(-M(181)+M(185)-M(187) &
    +M(191)-M(231)+M(234)-M(237)+M(245))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,12)
  Gcoeff = (c(1)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(2)*(-M(143)+M(157)-M(167) &
    +M(203)-M(240)+M(241)-M(242)+M(243))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,93)
  Gcoeff = (c(2)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,174)
  Gcoeff = (c(1)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(2)*(M(133)-M(157)-M(203) &
    +M(209)+M(239)-M(241)-M(243)+M(244))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,13)
  Gcoeff = (c(1)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(2)*(M(179)-M(185)-M(191) &
    +M(193)+M(228)-M(234)-M(245)+M(247))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,94)
  Gcoeff = (c(2)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,175)
  Gcoeff = (c(1)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(2)*(-M(133)+M(143)+M(167) &
    -M(209)-M(239)+M(240)+M(242)-M(244))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,14)
  Gcoeff = (c(1)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(2)*(-M(179)+M(181)+M(187) &
    -M(193)-M(228)+M(231)+M(237)-M(247))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,95)
  Gcoeff = (c(2)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,176)
  Gcoeff = (c(1)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(2)*(-M(143)+M(157)-M(167) &
    +M(203)-M(240)+M(241)-M(242)+M(243))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,15)
  Gcoeff = (c(1)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(2)*(-M(181)+M(185)-M(187) &
    +M(191)-M(231)+M(234)-M(237)+M(245))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,96)
  Gcoeff = (c(2)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,177)
  Gcoeff = (c(2)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,16)
  Gcoeff = (c(2)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,97)
  Gcoeff = (c(3)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,178)
  Gcoeff = (c(2)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,17)
  Gcoeff = (c(2)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,98)
  Gcoeff = (c(3)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,179)
  Gcoeff = (c(2)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,18)
  Gcoeff = (c(2)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,99)
  Gcoeff = (c(3)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(84)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,180)
  Gcoeff = (c(1)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(2)*(M(136)-M(160)+M(191) &
    -M(193)-M(195)+M(196)-M(228)+M(234))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,298)
  Gcoeff = (c(1)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(2)*(M(135)-M(159)+M(215) &
    -M(217)-M(219)+M(220)-M(227)+M(233))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,301)
  Gcoeff = (c(2)*(M(135)-M(136)-M(159)+M(160)-M(191)+M(193)+M(195)-M(196)+M(215)-M(217)-M(219)+M(220)-M(227)+M(228)+M(233) &
    -M(234))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,304)
  Gcoeff = (c(1)*(-M(43)+M(50)+M(62)-M(92)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120))+c(2)*(-M(136)+M(150)+M(174) &
    -M(191)+M(192)+M(194)-M(196)-M(234))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,307)
  Gcoeff = (c(1)*(-M(43)+M(50)+M(62)-M(92)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120))+c(2)*(-M(135)+M(149)+M(173) &
    -M(215)+M(216)+M(218)-M(220)-M(233))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,310)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,313)
  Gcoeff = (c(1)*(-M(50)+M(55)-M(62)+M(89)+M(102)+M(104)-M(108)-M(114)-M(119)-M(120)+M(121)+M(123))+c(2)*(-M(150)+M(160)-M(174) &
    -M(192)+M(193)-M(194)+M(195)+M(228))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,316)
  Gcoeff = (c(1)*(-M(50)+M(55)-M(62)+M(89)+M(102)+M(104)-M(108)-M(114)-M(119)-M(120)+M(121)+M(123))+c(2)*(-M(149)+M(159)-M(173) &
    -M(216)+M(217)-M(218)+M(219)+M(227))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,319)
  Gcoeff = (c(2)*(-M(149)+M(150)+M(159)-M(160)-M(173)+M(174)+M(192)-M(193)+M(194)-M(195)-M(216)+M(217)-M(218)+M(219)+M(227) &
    -M(228))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,322)
  Gcoeff = (c(1)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(2)*(M(135)-M(159)+M(215) &
    -M(217)-M(219)+M(220)-M(227)+M(233))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,299)
  Gcoeff = (c(1)*(M(43)-M(55)-M(89)+M(92)-M(102)-M(104)+M(105)+M(106)+M(111)+M(117)-M(121)-M(123))+c(2)*(M(136)-M(160)+M(191) &
    -M(193)-M(195)+M(196)-M(228)+M(234))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,302)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,305)
  Gcoeff = (c(1)*(-M(43)+M(50)+M(62)-M(92)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120))+c(2)*(-M(135)+M(149)+M(173) &
    -M(215)+M(216)+M(218)-M(220)-M(233))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,308)
  Gcoeff = (c(1)*(-M(43)+M(50)+M(62)-M(92)-M(105)-M(106)+M(108)-M(111)+M(114)-M(117)+M(119)+M(120))+c(2)*(-M(136)+M(150)+M(174) &
    -M(191)+M(192)+M(194)-M(196)-M(234))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,311)
  Gcoeff = (c(2)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,314)
  Gcoeff = (c(1)*(-M(50)+M(55)-M(62)+M(89)+M(102)+M(104)-M(108)-M(114)-M(119)-M(120)+M(121)+M(123))+c(2)*(-M(149)+M(159)-M(173) &
    -M(216)+M(217)-M(218)+M(219)+M(227))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,317)
  Gcoeff = (c(1)*(-M(50)+M(55)-M(62)+M(89)+M(102)+M(104)-M(108)-M(114)-M(119)-M(120)+M(121)+M(123))+c(2)*(-M(150)+M(160)-M(174) &
    -M(192)+M(193)-M(194)+M(195)+M(228))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,320)
  Gcoeff = (c(2)*(M(149)-M(150)-M(159)+M(160)+M(173)-M(174)-M(192)+M(193)-M(194)+M(195)+M(216)-M(217)+M(218)-M(219)-M(227) &
    +M(228))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,323)
  Gcoeff = (c(2)*(M(135)-M(136)-M(159)+M(160)-M(191)+M(193)+M(195)-M(196)+M(215)-M(217)-M(219)+M(220)-M(227)+M(228)+M(233) &
    -M(234))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,300)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,303)
  Gcoeff = (c(3)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,306)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,309)
  Gcoeff = (c(2)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,312)
  Gcoeff = (c(3)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,315)
  Gcoeff = (c(2)*(-M(149)+M(150)+M(159)-M(160)-M(173)+M(174)+M(192)-M(193)+M(194)-M(195)-M(216)+M(217)-M(218)+M(219)+M(227) &
    -M(228))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,318)
  Gcoeff = (c(2)*(M(149)-M(150)-M(159)+M(160)+M(173)-M(174)-M(192)+M(193)-M(194)+M(195)+M(216)-M(217)+M(218)-M(219)-M(227) &
    +M(228))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,321)
  Gcoeff = (c(3)*(M(149)-M(150)-M(159)+M(160)+M(173)-M(174)-M(192)+M(193)-M(194)+M(195)+M(216)-M(217)+M(218)-M(219)-M(227) &
    +M(228))) * den(82)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,324)
  Gcoeff = (c(1)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(2)*(M(131)-M(133)-M(135) &
    +M(136)+M(196)-M(220)-M(244)+M(250))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,325)
  Gcoeff = (c(1)*(M(73)-M(85)-M(97)+M(100)+M(110)-M(116)-M(122)+M(124)+M(125)-M(127)-M(129)+M(130))+c(2)*(M(155)-M(157)-M(159) &
    +M(160)+M(195)-M(219)-M(243)+M(249))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,326)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(135)-M(136)+M(155)-M(157)-M(159)+M(160)+M(195)-M(196)-M(219)+M(220)-M(243)+M(244)+M(249) &
    -M(250))) * den(18)
  T2sum(1:1,12) = T2sum(1:1,12) + Gcoeff * G0tensor(:,327)
  Gcoeff = (c(1)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(2)*(M(155)-M(161)-M(167) &
    +M(169)+M(230)-M(240)-M(246)+M(249))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,19)
  Gcoeff = (c(1)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(2)*(M(139)-M(181)-M(205) &
    +M(215)+M(233)-M(235)-M(237)+M(238))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,100)
  Gcoeff = (c(2)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,181)
  Gcoeff = (c(1)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(2)*(-M(155)+M(157)+M(163) &
    -M(169)-M(230)+M(232)+M(243)-M(249))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,20)
  Gcoeff = (c(1)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(2)*(-M(139)+M(145)+M(191) &
    -M(215)-M(233)+M(234)+M(236)-M(238))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,101)
  Gcoeff = (c(2)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,182)
  Gcoeff = (c(1)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(2)*(-M(157)+M(161)-M(163) &
    +M(167)-M(232)+M(240)-M(243)+M(246))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,21)
  Gcoeff = (c(1)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(2)*(-M(145)+M(181)-M(191) &
    +M(205)-M(234)+M(235)-M(236)+M(237))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,102)
  Gcoeff = (c(2)*(-M(145)+M(157)-M(161)+M(163)-M(167)+M(181)-M(191)+M(205)+M(232)-M(234)+M(235)-M(236)+M(237)-M(240)+M(243) &
    -M(246))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,183)
  Gcoeff = (c(1)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(2)*(M(139)-M(181)-M(205) &
    +M(215)+M(233)-M(235)-M(237)+M(238))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,22)
  Gcoeff = (c(1)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(2)*(M(155)-M(161)-M(167) &
    +M(169)+M(230)-M(240)-M(246)+M(249))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,103)
  Gcoeff = (c(2)*(-M(139)+M(155)-M(161)-M(167)+M(169)+M(181)+M(205)-M(215)+M(230)-M(233)+M(235)+M(237)-M(238)-M(240)-M(246) &
    +M(249))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,184)
  Gcoeff = (c(1)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(2)*(-M(139)+M(145)+M(191) &
    -M(215)-M(233)+M(234)+M(236)-M(238))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,23)
  Gcoeff = (c(1)*(-M(45)+M(48)+M(71)-M(83)+M(101)-M(103)-M(109)-M(110)+M(115)+M(116)+M(122)-M(124))+c(2)*(-M(155)+M(157)+M(163) &
    -M(169)-M(230)+M(232)+M(243)-M(249))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,104)
  Gcoeff = (c(2)*(M(139)-M(145)-M(155)+M(157)+M(163)-M(169)-M(191)+M(215)-M(230)+M(232)+M(233)-M(234)-M(236)+M(238)+M(243) &
    -M(249))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,185)
  Gcoeff = (c(1)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(2)*(-M(145)+M(181)-M(191) &
    +M(205)-M(234)+M(235)-M(236)+M(237))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,24)
  Gcoeff = (c(1)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(2)*(-M(157)+M(161)-M(163) &
    +M(167)-M(232)+M(240)-M(243)+M(246))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,105)
  Gcoeff = (c(2)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,186)
  Gcoeff = (c(2)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,25)
  Gcoeff = (c(2)*(-M(139)+M(155)-M(161)-M(167)+M(169)+M(181)+M(205)-M(215)+M(230)-M(233)+M(235)+M(237)-M(238)-M(240)-M(246) &
    +M(249))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,106)
  Gcoeff = (c(3)*(-M(139)+M(155)-M(161)-M(167)+M(169)+M(181)+M(205)-M(215)+M(230)-M(233)+M(235)+M(237)-M(238)-M(240)-M(246) &
    +M(249))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,187)
  Gcoeff = (c(2)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,26)
  Gcoeff = (c(2)*(M(139)-M(145)-M(155)+M(157)+M(163)-M(169)-M(191)+M(215)-M(230)+M(232)+M(233)-M(234)-M(236)+M(238)+M(243) &
    -M(249))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,107)
  Gcoeff = (c(3)*(M(139)-M(145)-M(155)+M(157)+M(163)-M(169)-M(191)+M(215)-M(230)+M(232)+M(233)-M(234)-M(236)+M(238)+M(243) &
    -M(249))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,188)
  Gcoeff = (c(2)*(-M(145)+M(157)-M(161)+M(163)-M(167)+M(181)-M(191)+M(205)+M(232)-M(234)+M(235)-M(236)+M(237)-M(240)+M(243) &
    -M(246))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,27)
  Gcoeff = (c(2)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,108)
  Gcoeff = (c(3)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(67)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,189)
  Gcoeff = (c(1)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(2)*(M(131)-M(137)-M(143) &
    +M(145)+M(236)-M(242)-M(248)+M(250))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,28)
  Gcoeff = (c(1)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(2)*(M(163)-M(187)-M(211) &
    +M(217)+M(227)-M(229)-M(231)+M(232))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,109)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,190)
  Gcoeff = (c(1)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(2)*(M(163)-M(187)-M(211) &
    +M(217)+M(227)-M(229)-M(231)+M(232))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,29)
  Gcoeff = (c(1)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(2)*(M(131)-M(137)-M(143) &
    +M(145)+M(236)-M(242)-M(248)+M(250))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,110)
  Gcoeff = (c(2)*(M(131)-M(137)-M(143)+M(145)-M(163)+M(187)+M(211)-M(217)-M(227)+M(229)+M(231)-M(232)+M(236)-M(242)-M(248) &
    +M(250))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,191)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,30)
  Gcoeff = (c(2)*(M(131)-M(137)-M(143)+M(145)-M(163)+M(187)+M(211)-M(217)-M(227)+M(229)+M(231)-M(232)+M(236)-M(242)-M(248) &
    +M(250))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,111)
  Gcoeff = (c(3)*(M(131)-M(137)-M(143)+M(145)-M(163)+M(187)+M(211)-M(217)-M(227)+M(229)+M(231)-M(232)+M(236)-M(242)-M(248) &
    +M(250))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,192)
  Gcoeff = (c(1)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(2)*(-M(131)+M(133)+M(139) &
    -M(145)-M(236)+M(238)+M(244)-M(250))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,31)
  Gcoeff = (c(1)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(2)*(-M(163)+M(169)+M(193) &
    -M(217)-M(227)+M(228)+M(230)-M(232))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,112)
  Gcoeff = (c(2)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,193)
  Gcoeff = (c(1)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(2)*(-M(163)+M(169)+M(193) &
    -M(217)-M(227)+M(228)+M(230)-M(232))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,32)
  Gcoeff = (c(1)*(-M(57)+M(60)+M(72)-M(84)-M(101)+M(103)+M(109)-M(115)-M(125)+M(127)+M(129)-M(130))+c(2)*(-M(131)+M(133)+M(139) &
    -M(145)-M(236)+M(238)+M(244)-M(250))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,113)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(139)-M(145)+M(163)-M(169)-M(193)+M(217)+M(227)-M(228)-M(230)+M(232)-M(236)+M(238)+M(244) &
    -M(250))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,194)
  Gcoeff = (c(2)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,33)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(139)-M(145)+M(163)-M(169)-M(193)+M(217)+M(227)-M(228)-M(230)+M(232)-M(236)+M(238)+M(244) &
    -M(250))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,114)
  Gcoeff = (c(3)*(-M(131)+M(133)+M(139)-M(145)+M(163)-M(169)-M(193)+M(217)+M(227)-M(228)-M(230)+M(232)-M(236)+M(238)+M(244) &
    -M(250))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,195)
  Gcoeff = (c(1)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(2)*(-M(133)+M(137)-M(139) &
    +M(143)-M(238)+M(242)-M(244)+M(248))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,34)
  Gcoeff = (c(1)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(2)*(-M(169)+M(187)-M(193) &
    +M(211)-M(228)+M(229)-M(230)+M(231))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,115)
  Gcoeff = (c(2)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,196)
  Gcoeff = (c(1)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(2)*(-M(169)+M(187)-M(193) &
    +M(211)-M(228)+M(229)-M(230)+M(231))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,35)
  Gcoeff = (c(1)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(2)*(-M(133)+M(137)-M(139) &
    +M(143)-M(238)+M(242)-M(244)+M(248))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,116)
  Gcoeff = (c(2)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,197)
  Gcoeff = (c(2)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,36)
  Gcoeff = (c(2)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,117)
  Gcoeff = (c(3)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(30)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,198)
  Gcoeff = (c(2)*(M(135)-M(159)-M(180)+M(186)+M(197)-M(199)-M(204)+M(210)+M(215)-M(217)-M(219)+M(220)+M(221)-M(223)-M(227) &
    +M(233))) * den(87)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,145)
  Gcoeff = (c(2)*(-M(135)+M(149)+M(173)+M(180)-M(183)-M(189)+M(199)+M(204)-M(207)-M(213)-M(215)+M(216)+M(218)-M(220)+M(223) &
    -M(233))) * den(87)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,146)
  Gcoeff = (c(2)*(-M(149)+M(159)-M(173)+M(183)-M(186)+M(189)-M(197)+M(207)-M(210)+M(213)-M(216)+M(217)-M(218)+M(219)-M(221) &
    +M(227))) * den(87)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,147)
  Gcoeff = (c(2)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(87)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,148)
  Gcoeff = (c(2)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(87)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,149)
  Gcoeff = (c(2)*(M(149)-M(159)+M(173)-M(183)+M(186)-M(189)+M(197)-M(207)+M(210)-M(213)+M(216)-M(217)+M(218)-M(219)+M(221) &
    -M(227))) * den(87)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,150)
  Gcoeff = (c(3)*(-M(135)+M(159)+M(180)-M(186)-M(197)+M(199)+M(204)-M(210)-M(215)+M(217)+M(219)-M(220)-M(221)+M(223)+M(227) &
    -M(233))) * den(87)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,151)
  Gcoeff = (c(3)*(M(135)-M(149)-M(173)-M(180)+M(183)+M(189)-M(199)-M(204)+M(207)+M(213)+M(215)-M(216)-M(218)+M(220)-M(223) &
    +M(233))) * den(87)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,152)
  Gcoeff = (c(3)*(M(149)-M(159)+M(173)-M(183)+M(186)-M(189)+M(197)-M(207)+M(210)-M(213)+M(216)-M(217)+M(218)-M(219)+M(221) &
    -M(227))) * den(87)
  T2sum(1:5,14) = T2sum(1:5,14) + Gcoeff * G1tensor(:,153)
  Gcoeff = (c(1)*(M(5)-M(6)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(101)-M(103)-M(109)+M(115))+c(2)*(M(191)-M(193)-M(215)+M(217) &
    +M(227)-M(228)-M(233)+M(234))) * den(96)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,37)
  Gcoeff = (c(1)*(M(5)-M(6)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(101)-M(103)-M(109)+M(115))+c(2)*(M(131)-M(133)-M(155)+M(157) &
    +M(243)-M(244)-M(249)+M(250))) * den(96)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,118)
  Gcoeff = (c(2)*(M(131)-M(133)-M(155)+M(157)-M(191)+M(193)+M(215)-M(217)-M(227)+M(228)+M(233)-M(234)+M(243)-M(244)-M(249) &
    +M(250))) * den(96)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,199)
  Gcoeff = (c(1)*(M(5)-M(6)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(101)-M(103)-M(109)+M(115))+c(2)*(M(131)-M(133)-M(155)+M(157) &
    +M(243)-M(244)-M(249)+M(250))) * den(96)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,38)
  Gcoeff = (c(1)*(M(5)-M(6)-M(19)+M(20)+M(37)-M(38)-M(39)+M(40)+M(101)-M(103)-M(109)+M(115))+c(2)*(M(191)-M(193)-M(215)+M(217) &
    +M(227)-M(228)-M(233)+M(234))) * den(96)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,119)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(155)-M(157)+M(191)-M(193)-M(215)+M(217)+M(227)-M(228)-M(233)+M(234)-M(243)+M(244)+M(249) &
    -M(250))) * den(96)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,200)
  Gcoeff = (c(2)*(M(131)-M(133)-M(155)+M(157)-M(191)+M(193)+M(215)-M(217)-M(227)+M(228)+M(233)-M(234)+M(243)-M(244)-M(249) &
    +M(250))) * den(96)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,39)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(155)-M(157)+M(191)-M(193)-M(215)+M(217)+M(227)-M(228)-M(233)+M(234)-M(243)+M(244)+M(249) &
    -M(250))) * den(96)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,120)
  Gcoeff = (c(3)*(-M(131)+M(133)+M(155)-M(157)+M(191)-M(193)-M(215)+M(217)+M(227)-M(228)-M(233)+M(234)-M(243)+M(244)+M(249) &
    -M(250))) * den(96)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,201)
  Gcoeff = (c(1)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(2)*(M(203)-M(209)-M(215) &
    +M(217)+M(227)-M(233)-M(239)+M(241))) * den(197)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,40)
  Gcoeff = (c(1)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(2)*(M(131)-M(155)-M(179) &
    +M(185)+M(245)-M(247)-M(249)+M(250))) * den(197)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,121)
  Gcoeff = (c(2)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(197)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,202)
  Gcoeff = (c(1)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(2)*(M(131)-M(155)-M(179) &
    +M(185)+M(245)-M(247)-M(249)+M(250))) * den(197)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,41)
  Gcoeff = (c(1)*(M(41)-M(53)-M(65)+M(68)+M(101)+M(102)-M(103)-M(105)-M(109)-M(111)+M(115)+M(121))+c(2)*(M(203)-M(209)-M(215) &
    +M(217)+M(227)-M(233)-M(239)+M(241))) * den(197)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,122)
  Gcoeff = (c(2)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(197)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,203)
  Gcoeff = (c(2)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(197)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,42)
  Gcoeff = (c(2)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(197)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,123)
  Gcoeff = (c(3)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(197)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,204)
  Gcoeff = (c(1)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(2)*(M(179)-M(185)-M(191) &
    +M(193)+M(228)-M(234)-M(245)+M(247))) * den(192)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,43)
  Gcoeff = (c(1)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(2)*(M(133)-M(157)-M(203) &
    +M(209)+M(239)-M(241)-M(243)+M(244))) * den(192)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,124)
  Gcoeff = (c(2)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(192)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,205)
  Gcoeff = (c(1)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(2)*(M(133)-M(157)-M(203) &
    +M(209)+M(239)-M(241)-M(243)+M(244))) * den(192)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,44)
  Gcoeff = (c(1)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(2)*(M(179)-M(185)-M(191) &
    +M(193)+M(228)-M(234)-M(245)+M(247))) * den(192)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,125)
  Gcoeff = (c(2)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(192)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,206)
  Gcoeff = (c(2)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(192)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,45)
  Gcoeff = (c(2)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(192)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,126)
  Gcoeff = (c(3)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(192)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,207)
  Gcoeff = (c(2)*(M(132)-M(156)-M(180)+M(186)-M(203)+M(209)+M(215)-M(217)+M(221)-M(223)-M(225)+M(226)-M(227)+M(233)+M(239) &
    -M(241))) * den(98)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,136)
  Gcoeff = (c(2)*(M(131)-M(132)-M(155)+M(156)-M(179)+M(180)+M(185)-M(186)-M(221)+M(223)+M(225)-M(226)+M(245)-M(247)-M(249) &
    +M(250))) * den(98)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,137)
  Gcoeff = (c(2)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(98)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,138)
  Gcoeff = (c(2)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(98)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,139)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(98)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,140)
  Gcoeff = (c(2)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(98)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,141)
  Gcoeff = (c(3)*(-M(132)+M(156)+M(180)-M(186)+M(203)-M(209)-M(215)+M(217)-M(221)+M(223)+M(225)-M(226)+M(227)-M(233)-M(239) &
    +M(241))) * den(98)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,142)
  Gcoeff = (c(3)*(-M(131)+M(132)+M(155)-M(156)+M(179)-M(180)-M(185)+M(186)+M(221)-M(223)-M(225)+M(226)-M(245)+M(247)+M(249) &
    -M(250))) * den(98)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,143)
  Gcoeff = (c(3)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(98)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,144)
  Gcoeff = (c(2)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(408)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(2)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(408)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,46)
  Gcoeff = (c(3)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(408)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,91)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(155)-M(157)+M(191)-M(193)-M(215)+M(217)+M(227)-M(228)-M(233)+M(234)-M(243)+M(244)+M(249) &
    -M(250))) * den(408)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,2)
  Gcoeff = (c(2)*(M(131)-M(133)-M(155)+M(157)-M(191)+M(193)+M(215)-M(217)-M(227)+M(228)+M(233)-M(234)+M(243)-M(244)-M(249) &
    +M(250))) * den(408)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,47)
  Gcoeff = (c(3)*(M(131)-M(133)-M(155)+M(157)-M(191)+M(193)+M(215)-M(217)-M(227)+M(228)+M(233)-M(234)+M(243)-M(244)-M(249) &
    +M(250))) * den(408)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,92)
  Gcoeff = (c(2)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(408)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,3)
  Gcoeff = (c(2)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(408)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,48)
  Gcoeff = (c(3)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(408)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,93)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(135)-M(136)+M(155)-M(157)-M(159)+M(160)+M(195)-M(196)-M(219)+M(220)-M(243)+M(244)+M(249) &
    -M(250))) * den(47)
  T2sum(1:5,12) = T2sum(1:5,12) + Gcoeff * G1tensor(:,163)
  Gcoeff = (c(1)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(2)*(M(167)-M(169)-M(209)+M(211) &
    +M(229)-M(230)-M(239)+M(240))) * den(102)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,46)
  Gcoeff = (c(1)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(2)*(M(137)-M(139)-M(179)+M(181) &
    +M(237)-M(238)-M(247)+M(248))) * den(102)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,127)
  Gcoeff = (c(2)*(M(137)-M(139)-M(167)+M(169)-M(179)+M(181)+M(209)-M(211)-M(229)+M(230)+M(237)-M(238)+M(239)-M(240)-M(247) &
    +M(248))) * den(102)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,208)
  Gcoeff = (c(1)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(2)*(M(137)-M(139)-M(179)+M(181) &
    +M(237)-M(238)-M(247)+M(248))) * den(102)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,47)
  Gcoeff = (c(1)*(M(11)-M(12)-M(21)+M(22)+M(31)-M(32)-M(35)+M(36)-M(103)+M(107)-M(109)+M(113))+c(2)*(M(167)-M(169)-M(209)+M(211) &
    +M(229)-M(230)-M(239)+M(240))) * den(102)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,128)
  Gcoeff = (c(2)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248))) * den(102)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,209)
  Gcoeff = (c(2)*(M(137)-M(139)-M(167)+M(169)-M(179)+M(181)+M(209)-M(211)-M(229)+M(230)+M(237)-M(238)+M(239)-M(240)-M(247) &
    +M(248))) * den(102)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,48)
  Gcoeff = (c(2)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248))) * den(102)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,129)
  Gcoeff = (c(3)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248))) * den(102)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,210)
  Gcoeff = (c(1)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(2)*(-M(205)+M(209)-M(211) &
    +M(215)-M(229)+M(233)-M(235)+M(239))) * den(234)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,49)
  Gcoeff = (c(1)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(2)*(-M(137)+M(155)-M(161) &
    +M(179)-M(246)+M(247)-M(248)+M(249))) * den(234)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,130)
  Gcoeff = (c(2)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(234)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,211)
  Gcoeff = (c(1)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(2)*(-M(137)+M(155)-M(161) &
    +M(179)-M(246)+M(247)-M(248)+M(249))) * den(234)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,50)
  Gcoeff = (c(1)*(-M(44)+M(53)-M(56)+M(65)+M(103)+M(105)-M(107)-M(108)+M(109)+M(111)-M(113)-M(119))+c(2)*(-M(205)+M(209)-M(211) &
    +M(215)-M(229)+M(233)-M(235)+M(239))) * den(234)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,131)
  Gcoeff = (c(2)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(234)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,212)
  Gcoeff = (c(2)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(234)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,51)
  Gcoeff = (c(2)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(234)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,132)
  Gcoeff = (c(3)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(234)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,213)
  Gcoeff = (c(1)*(M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)-M(101)+M(107)+M(113)-M(115))+c(2)*(M(161)-M(163)-M(185)+M(187) &
    +M(231)-M(232)-M(245)+M(246))) * den(105)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,52)
  Gcoeff = (c(1)*(M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)-M(101)+M(107)+M(113)-M(115))+c(2)*(M(143)-M(145)-M(203)+M(205) &
    +M(235)-M(236)-M(241)+M(242))) * den(105)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,133)
  Gcoeff = (c(2)*(M(143)-M(145)-M(161)+M(163)+M(185)-M(187)-M(203)+M(205)-M(231)+M(232)+M(235)-M(236)-M(241)+M(242)+M(245) &
    -M(246))) * den(105)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,214)
  Gcoeff = (c(1)*(M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)-M(101)+M(107)+M(113)-M(115))+c(2)*(M(143)-M(145)-M(203)+M(205) &
    +M(235)-M(236)-M(241)+M(242))) * den(105)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,53)
  Gcoeff = (c(1)*(M(17)-M(18)-M(23)+M(24)+M(27)-M(28)-M(33)+M(34)-M(101)+M(107)+M(113)-M(115))+c(2)*(M(161)-M(163)-M(185)+M(187) &
    +M(231)-M(232)-M(245)+M(246))) * den(105)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,134)
  Gcoeff = (c(2)*(-M(143)+M(145)+M(161)-M(163)-M(185)+M(187)+M(203)-M(205)+M(231)-M(232)-M(235)+M(236)+M(241)-M(242)-M(245) &
    +M(246))) * den(105)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,215)
  Gcoeff = (c(2)*(M(143)-M(145)-M(161)+M(163)+M(185)-M(187)-M(203)+M(205)-M(231)+M(232)+M(235)-M(236)-M(241)+M(242)+M(245) &
    -M(246))) * den(105)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,54)
  Gcoeff = (c(2)*(-M(143)+M(145)+M(161)-M(163)-M(185)+M(187)+M(203)-M(205)+M(231)-M(232)-M(235)+M(236)+M(241)-M(242)-M(245) &
    +M(246))) * den(105)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,135)
  Gcoeff = (c(3)*(-M(143)+M(145)+M(161)-M(163)-M(185)+M(187)+M(203)-M(205)+M(231)-M(232)-M(235)+M(236)+M(241)-M(242)-M(245) &
    +M(246))) * den(105)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,216)
  Gcoeff = (c(1)*(M(41)-M(44)-M(56)+M(68)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121))+c(2)*(M(203)-M(205)-M(211) &
    +M(217)+M(227)-M(229)-M(235)+M(241))) * den(252)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,55)
  Gcoeff = (c(1)*(M(41)-M(44)-M(56)+M(68)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121))+c(2)*(M(131)-M(137)-M(161) &
    +M(185)+M(245)-M(246)-M(248)+M(250))) * den(252)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,136)
  Gcoeff = (c(2)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(252)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,217)
  Gcoeff = (c(1)*(M(41)-M(44)-M(56)+M(68)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121))+c(2)*(M(131)-M(137)-M(161) &
    +M(185)+M(245)-M(246)-M(248)+M(250))) * den(252)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,56)
  Gcoeff = (c(1)*(M(41)-M(44)-M(56)+M(68)+M(101)+M(102)-M(107)-M(108)-M(113)+M(115)-M(119)+M(121))+c(2)*(M(203)-M(205)-M(211) &
    +M(217)+M(227)-M(229)-M(235)+M(241))) * den(252)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,137)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(252)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,218)
  Gcoeff = (c(2)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(252)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,57)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(252)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,138)
  Gcoeff = (c(3)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(252)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,219)
  Gcoeff = (c(1)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(2)*(-M(181)+M(185)-M(187) &
    +M(191)-M(231)+M(234)-M(237)+M(245))) * den(216)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,58)
  Gcoeff = (c(1)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(2)*(-M(143)+M(157)-M(167) &
    +M(203)-M(240)+M(241)-M(242)+M(243))) * den(216)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,139)
  Gcoeff = (c(2)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(216)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,220)
  Gcoeff = (c(1)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(2)*(-M(143)+M(157)-M(167) &
    +M(203)-M(240)+M(241)-M(242)+M(243))) * den(216)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,59)
  Gcoeff = (c(1)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(2)*(-M(181)+M(185)-M(187) &
    +M(191)-M(231)+M(234)-M(237)+M(245))) * den(216)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,140)
  Gcoeff = (c(2)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(216)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,221)
  Gcoeff = (c(2)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(216)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,60)
  Gcoeff = (c(2)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(216)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,141)
  Gcoeff = (c(3)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(216)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,222)
  Gcoeff = (c(1)*(M(42)-M(47)-M(59)+M(80)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123))+c(2)*(M(179)-M(181)-M(187) &
    +M(193)+M(228)-M(231)-M(237)+M(247))) * den(221)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,61)
  Gcoeff = (c(1)*(M(42)-M(47)-M(59)+M(80)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123))+c(2)*(M(133)-M(143)-M(167) &
    +M(209)+M(239)-M(240)-M(242)+M(244))) * den(221)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,142)
  Gcoeff = (c(2)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(221)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,223)
  Gcoeff = (c(1)*(M(42)-M(47)-M(59)+M(80)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123))+c(2)*(M(133)-M(143)-M(167) &
    +M(209)+M(239)-M(240)-M(242)+M(244))) * den(221)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,62)
  Gcoeff = (c(1)*(M(42)-M(47)-M(59)+M(80)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123))+c(2)*(M(179)-M(181)-M(187) &
    +M(193)+M(228)-M(231)-M(237)+M(247))) * den(221)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,143)
  Gcoeff = (c(2)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(221)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,224)
  Gcoeff = (c(2)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(221)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,63)
  Gcoeff = (c(2)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(221)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,144)
  Gcoeff = (c(3)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(221)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,225)
  Gcoeff = (c(1)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(2)*(M(155)-M(161)-M(167) &
    +M(169)+M(230)-M(240)-M(246)+M(249))) * den(168)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,64)
  Gcoeff = (c(1)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(2)*(M(139)-M(181)-M(205) &
    +M(215)+M(233)-M(235)-M(237)+M(238))) * den(168)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,145)
  Gcoeff = (c(2)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(168)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,226)
  Gcoeff = (c(1)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(2)*(M(139)-M(181)-M(205) &
    +M(215)+M(233)-M(235)-M(237)+M(238))) * den(168)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,65)
  Gcoeff = (c(1)*(M(45)-M(66)-M(78)+M(83)+M(103)-M(107)+M(109)+M(110)-M(112)-M(113)-M(118)+M(124))+c(2)*(M(155)-M(161)-M(167) &
    +M(169)+M(230)-M(240)-M(246)+M(249))) * den(168)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,146)
  Gcoeff = (c(2)*(-M(139)+M(155)-M(161)-M(167)+M(169)+M(181)+M(205)-M(215)+M(230)-M(233)+M(235)+M(237)-M(238)-M(240)-M(246) &
    +M(249))) * den(168)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,227)
  Gcoeff = (c(2)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(168)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,66)
  Gcoeff = (c(2)*(-M(139)+M(155)-M(161)-M(167)+M(169)+M(181)+M(205)-M(215)+M(230)-M(233)+M(235)+M(237)-M(238)-M(240)-M(246) &
    +M(249))) * den(168)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,147)
  Gcoeff = (c(3)*(-M(139)+M(155)-M(161)-M(167)+M(169)+M(181)+M(205)-M(215)+M(230)-M(233)+M(235)+M(237)-M(238)-M(240)-M(246) &
    +M(249))) * den(168)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,228)
  Gcoeff = (c(2)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(436)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,4)
  Gcoeff = (c(2)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(436)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,49)
  Gcoeff = (c(3)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(436)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,94)
  Gcoeff = (c(2)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(436)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,5)
  Gcoeff = (c(2)*(-M(139)+M(155)-M(161)-M(167)+M(169)+M(181)+M(205)-M(215)+M(230)-M(233)+M(235)+M(237)-M(238)-M(240)-M(246) &
    +M(249))) * den(436)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,50)
  Gcoeff = (c(3)*(-M(139)+M(155)-M(161)-M(167)+M(169)+M(181)+M(205)-M(215)+M(230)-M(233)+M(235)+M(237)-M(238)-M(240)-M(246) &
    +M(249))) * den(436)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,95)
  Gcoeff = (c(2)*(M(137)-M(139)-M(167)+M(169)-M(179)+M(181)+M(209)-M(211)-M(229)+M(230)+M(237)-M(238)+M(239)-M(240)-M(247) &
    +M(248))) * den(436)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,6)
  Gcoeff = (c(2)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248))) * den(436)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,51)
  Gcoeff = (c(3)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248))) * den(436)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,96)
  Gcoeff = (c(1)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(2)*(-M(157)+M(161)-M(163) &
    +M(167)-M(232)+M(240)-M(243)+M(246))) * den(173)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,67)
  Gcoeff = (c(1)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(2)*(-M(145)+M(181)-M(191) &
    +M(205)-M(234)+M(235)-M(236)+M(237))) * den(173)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,148)
  Gcoeff = (c(2)*(-M(145)+M(157)-M(161)+M(163)-M(167)+M(181)-M(191)+M(205)+M(232)-M(234)+M(235)-M(236)+M(237)-M(240)+M(243) &
    -M(246))) * den(173)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,229)
  Gcoeff = (c(1)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(2)*(-M(145)+M(181)-M(191) &
    +M(205)-M(234)+M(235)-M(236)+M(237))) * den(173)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,68)
  Gcoeff = (c(1)*(-M(48)+M(66)-M(71)+M(78)-M(101)+M(107)+M(112)+M(113)-M(115)-M(116)+M(118)-M(122))+c(2)*(-M(157)+M(161)-M(163) &
    +M(167)-M(232)+M(240)-M(243)+M(246))) * den(173)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,149)
  Gcoeff = (c(2)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(173)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,230)
  Gcoeff = (c(2)*(-M(145)+M(157)-M(161)+M(163)-M(167)+M(181)-M(191)+M(205)+M(232)-M(234)+M(235)-M(236)+M(237)-M(240)+M(243) &
    -M(246))) * den(173)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,69)
  Gcoeff = (c(2)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(173)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,150)
  Gcoeff = (c(3)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(173)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,231)
  Gcoeff = (c(1)*(M(45)-M(48)-M(71)+M(83)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124))+c(2)*(M(155)-M(157)-M(163) &
    +M(169)+M(230)-M(232)-M(243)+M(249))) * den(178)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,70)
  Gcoeff = (c(1)*(M(45)-M(48)-M(71)+M(83)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124))+c(2)*(M(139)-M(145)-M(191) &
    +M(215)+M(233)-M(234)-M(236)+M(238))) * den(178)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,151)
  Gcoeff = (c(2)*(M(139)-M(145)-M(155)+M(157)+M(163)-M(169)-M(191)+M(215)-M(230)+M(232)+M(233)-M(234)-M(236)+M(238)+M(243) &
    -M(249))) * den(178)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,232)
  Gcoeff = (c(1)*(M(45)-M(48)-M(71)+M(83)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124))+c(2)*(M(139)-M(145)-M(191) &
    +M(215)+M(233)-M(234)-M(236)+M(238))) * den(178)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,71)
  Gcoeff = (c(1)*(M(45)-M(48)-M(71)+M(83)-M(101)+M(103)+M(109)+M(110)-M(115)-M(116)-M(122)+M(124))+c(2)*(M(155)-M(157)-M(163) &
    +M(169)+M(230)-M(232)-M(243)+M(249))) * den(178)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,152)
  Gcoeff = (c(2)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(178)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,233)
  Gcoeff = (c(2)*(M(139)-M(145)-M(155)+M(157)+M(163)-M(169)-M(191)+M(215)-M(230)+M(232)+M(233)-M(234)-M(236)+M(238)+M(243) &
    -M(249))) * den(178)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,72)
  Gcoeff = (c(2)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(178)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,153)
  Gcoeff = (c(3)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(178)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,234)
  Gcoeff = (c(2)*(-M(143)+M(145)+M(161)-M(163)-M(185)+M(187)+M(203)-M(205)+M(231)-M(232)-M(235)+M(236)+M(241)-M(242)-M(245) &
    +M(246))) * den(450)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,7)
  Gcoeff = (c(2)*(M(143)-M(145)-M(161)+M(163)+M(185)-M(187)-M(203)+M(205)-M(231)+M(232)+M(235)-M(236)-M(241)+M(242)+M(245) &
    -M(246))) * den(450)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,52)
  Gcoeff = (c(3)*(M(143)-M(145)-M(161)+M(163)+M(185)-M(187)-M(203)+M(205)-M(231)+M(232)+M(235)-M(236)-M(241)+M(242)+M(245) &
    -M(246))) * den(450)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,97)
  Gcoeff = (c(2)*(-M(145)+M(157)-M(161)+M(163)-M(167)+M(181)-M(191)+M(205)+M(232)-M(234)+M(235)-M(236)+M(237)-M(240)+M(243) &
    -M(246))) * den(450)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,8)
  Gcoeff = (c(2)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(450)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,53)
  Gcoeff = (c(3)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(450)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,98)
  Gcoeff = (c(2)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(450)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,9)
  Gcoeff = (c(2)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(450)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,54)
  Gcoeff = (c(3)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(450)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,99)
  Gcoeff = (c(2)*(M(131)-M(137)-M(143)+M(145)-M(163)+M(187)+M(211)-M(217)-M(227)+M(229)+M(231)-M(232)+M(236)-M(242)-M(248) &
    +M(250))) * den(457)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,10)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(457)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,55)
  Gcoeff = (c(3)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(457)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,100)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(139)-M(145)+M(163)-M(169)-M(193)+M(217)+M(227)-M(228)-M(230)+M(232)-M(236)+M(238)+M(244) &
    -M(250))) * den(457)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,11)
  Gcoeff = (c(2)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(457)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,56)
  Gcoeff = (c(3)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(457)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,101)
  Gcoeff = (c(2)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(457)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,12)
  Gcoeff = (c(2)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(457)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,57)
  Gcoeff = (c(3)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(457)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,102)
  Gcoeff = (c(1)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(2)*(M(131)-M(137)-M(143) &
    +M(145)+M(236)-M(242)-M(248)+M(250))) * den(144)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,73)
  Gcoeff = (c(1)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(2)*(M(163)-M(187)-M(211) &
    +M(217)+M(227)-M(229)-M(231)+M(232))) * den(144)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,154)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(144)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,235)
  Gcoeff = (c(1)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(2)*(M(163)-M(187)-M(211) &
    +M(217)+M(227)-M(229)-M(231)+M(232))) * den(144)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,74)
  Gcoeff = (c(1)*(M(57)-M(69)-M(81)+M(84)+M(101)-M(107)-M(113)+M(115)+M(125)-M(126)-M(128)+M(130))+c(2)*(M(131)-M(137)-M(143) &
    +M(145)+M(236)-M(242)-M(248)+M(250))) * den(144)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,155)
  Gcoeff = (c(2)*(M(131)-M(137)-M(143)+M(145)-M(163)+M(187)+M(211)-M(217)-M(227)+M(229)+M(231)-M(232)+M(236)-M(242)-M(248) &
    +M(250))) * den(144)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,236)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(144)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,75)
  Gcoeff = (c(2)*(M(131)-M(137)-M(143)+M(145)-M(163)+M(187)+M(211)-M(217)-M(227)+M(229)+M(231)-M(232)+M(236)-M(242)-M(248) &
    +M(250))) * den(144)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,156)
  Gcoeff = (c(3)*(M(131)-M(137)-M(143)+M(145)-M(163)+M(187)+M(211)-M(217)-M(227)+M(229)+M(231)-M(232)+M(236)-M(242)-M(248) &
    +M(250))) * den(144)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,237)
  Gcoeff = (c(2)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(511)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,13)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(511)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,58)
  Gcoeff = (c(3)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(511)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,103)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(511)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,14)
  Gcoeff = (c(2)*(M(131)-M(137)-M(143)+M(145)-M(163)+M(187)+M(211)-M(217)-M(227)+M(229)+M(231)-M(232)+M(236)-M(242)-M(248) &
    +M(250))) * den(511)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,59)
  Gcoeff = (c(3)*(M(131)-M(137)-M(143)+M(145)-M(163)+M(187)+M(211)-M(217)-M(227)+M(229)+M(231)-M(232)+M(236)-M(242)-M(248) &
    +M(250))) * den(511)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,104)
  Gcoeff = (c(2)*(-M(143)+M(145)+M(161)-M(163)-M(185)+M(187)+M(203)-M(205)+M(231)-M(232)-M(235)+M(236)+M(241)-M(242)-M(245) &
    +M(246))) * den(511)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,15)
  Gcoeff = (c(2)*(M(143)-M(145)-M(161)+M(163)+M(185)-M(187)-M(203)+M(205)-M(231)+M(232)+M(235)-M(236)-M(241)+M(242)+M(245) &
    -M(246))) * den(511)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,60)
  Gcoeff = (c(3)*(M(143)-M(145)-M(161)+M(163)+M(185)-M(187)-M(203)+M(205)-M(231)+M(232)+M(235)-M(236)-M(241)+M(242)+M(245) &
    -M(246))) * den(511)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,105)
  Gcoeff = (c(1)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(2)*(-M(133)+M(137)-M(139) &
    +M(143)-M(238)+M(242)-M(244)+M(248))) * den(149)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,76)
  Gcoeff = (c(1)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(2)*(-M(169)+M(187)-M(193) &
    +M(211)-M(228)+M(229)-M(230)+M(231))) * den(149)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,157)
  Gcoeff = (c(2)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(149)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,238)
  Gcoeff = (c(1)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(2)*(-M(169)+M(187)-M(193) &
    +M(211)-M(228)+M(229)-M(230)+M(231))) * den(149)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,77)
  Gcoeff = (c(1)*(-M(60)+M(69)-M(72)+M(81)-M(103)+M(107)-M(109)+M(113)+M(126)-M(127)+M(128)-M(129))+c(2)*(-M(133)+M(137)-M(139) &
    +M(143)-M(238)+M(242)-M(244)+M(248))) * den(149)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,158)
  Gcoeff = (c(2)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(149)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,239)
  Gcoeff = (c(2)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(149)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,78)
  Gcoeff = (c(2)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(149)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,159)
  Gcoeff = (c(3)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(149)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,240)
  Gcoeff = (c(1)*(M(57)-M(60)-M(72)+M(84)+M(101)-M(103)-M(109)+M(115)+M(125)-M(127)-M(129)+M(130))+c(2)*(M(131)-M(133)-M(139) &
    +M(145)+M(236)-M(238)-M(244)+M(250))) * den(154)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,79)
  Gcoeff = (c(1)*(M(57)-M(60)-M(72)+M(84)+M(101)-M(103)-M(109)+M(115)+M(125)-M(127)-M(129)+M(130))+c(2)*(M(163)-M(169)-M(193) &
    +M(217)+M(227)-M(228)-M(230)+M(232))) * den(154)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,160)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(139)-M(145)+M(163)-M(169)-M(193)+M(217)+M(227)-M(228)-M(230)+M(232)-M(236)+M(238)+M(244) &
    -M(250))) * den(154)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,241)
  Gcoeff = (c(1)*(M(57)-M(60)-M(72)+M(84)+M(101)-M(103)-M(109)+M(115)+M(125)-M(127)-M(129)+M(130))+c(2)*(M(163)-M(169)-M(193) &
    +M(217)+M(227)-M(228)-M(230)+M(232))) * den(154)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,80)
  Gcoeff = (c(1)*(M(57)-M(60)-M(72)+M(84)+M(101)-M(103)-M(109)+M(115)+M(125)-M(127)-M(129)+M(130))+c(2)*(M(131)-M(133)-M(139) &
    +M(145)+M(236)-M(238)-M(244)+M(250))) * den(154)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,161)
  Gcoeff = (c(2)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(154)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,242)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(139)-M(145)+M(163)-M(169)-M(193)+M(217)+M(227)-M(228)-M(230)+M(232)-M(236)+M(238)+M(244) &
    -M(250))) * den(154)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,81)
  Gcoeff = (c(2)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(154)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,162)
  Gcoeff = (c(3)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(154)
  T2sum(1:1,15) = T2sum(1:1,15) + Gcoeff * G0tensor(:,243)
  Gcoeff = (c(2)*(M(137)-M(139)-M(167)+M(169)-M(179)+M(181)+M(209)-M(211)-M(229)+M(230)+M(237)-M(238)+M(239)-M(240)-M(247) &
    +M(248))) * den(525)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,16)
  Gcoeff = (c(2)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248))) * den(525)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,61)
  Gcoeff = (c(3)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248))) * den(525)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,106)
  Gcoeff = (c(2)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(525)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,17)
  Gcoeff = (c(2)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(525)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,62)
  Gcoeff = (c(3)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(525)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,107)
  Gcoeff = (c(2)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(525)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,18)
  Gcoeff = (c(2)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(525)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,63)
  Gcoeff = (c(3)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(525)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,108)
  Gcoeff = (c(2)*(-M(139)+M(155)-M(161)-M(167)+M(169)+M(181)+M(205)-M(215)+M(230)-M(233)+M(235)+M(237)-M(238)-M(240)-M(246) &
    +M(249))) * den(532)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,19)
  Gcoeff = (c(2)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(532)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,64)
  Gcoeff = (c(3)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(532)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,109)
  Gcoeff = (c(2)*(M(139)-M(145)-M(155)+M(157)+M(163)-M(169)-M(191)+M(215)-M(230)+M(232)+M(233)-M(234)-M(236)+M(238)+M(243) &
    -M(249))) * den(532)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,20)
  Gcoeff = (c(2)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(532)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,65)
  Gcoeff = (c(3)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(532)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,110)
  Gcoeff = (c(2)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(532)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,21)
  Gcoeff = (c(2)*(-M(145)+M(157)-M(161)+M(163)-M(167)+M(181)-M(191)+M(205)+M(232)-M(234)+M(235)-M(236)+M(237)-M(240)+M(243) &
    -M(246))) * den(532)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,66)
  Gcoeff = (c(3)*(-M(145)+M(157)-M(161)+M(163)-M(167)+M(181)-M(191)+M(205)+M(232)-M(234)+M(235)-M(236)+M(237)-M(240)+M(243) &
    -M(246))) * den(532)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,111)
  Gcoeff = (c(2)*(M(131)-M(133)-M(155)+M(157)-M(191)+M(193)+M(215)-M(217)-M(227)+M(228)+M(233)-M(234)+M(243)-M(244)-M(249) &
    +M(250))) * den(555)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,22)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(155)-M(157)+M(191)-M(193)-M(215)+M(217)+M(227)-M(228)-M(233)+M(234)-M(243)+M(244)+M(249) &
    -M(250))) * den(555)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,67)
  Gcoeff = (c(3)*(-M(131)+M(133)+M(155)-M(157)+M(191)-M(193)-M(215)+M(217)+M(227)-M(228)-M(233)+M(234)-M(243)+M(244)+M(249) &
    -M(250))) * den(555)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,112)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(139)-M(145)+M(163)-M(169)-M(193)+M(217)+M(227)-M(228)-M(230)+M(232)-M(236)+M(238)+M(244) &
    -M(250))) * den(555)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,23)
  Gcoeff = (c(2)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(555)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,68)
  Gcoeff = (c(3)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(555)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,113)
  Gcoeff = (c(2)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(555)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,24)
  Gcoeff = (c(2)*(M(139)-M(145)-M(155)+M(157)+M(163)-M(169)-M(191)+M(215)-M(230)+M(232)+M(233)-M(234)-M(236)+M(238)+M(243) &
    -M(249))) * den(555)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,69)
  Gcoeff = (c(3)*(M(139)-M(145)-M(155)+M(157)+M(163)-M(169)-M(191)+M(215)-M(230)+M(232)+M(233)-M(234)-M(236)+M(238)+M(243) &
    -M(249))) * den(555)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,114)
  Gcoeff = (c(2)*(M(135)-M(136)-M(159)+M(160)-M(191)+M(193)+M(195)-M(196)+M(215)-M(217)-M(219)+M(220)-M(227)+M(228)+M(233) &
    -M(234))) * den(89)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,154)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(89)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,155)
  Gcoeff = (c(3)*(-M(135)+M(136)+M(159)-M(160)+M(191)-M(193)-M(195)+M(196)-M(215)+M(217)+M(219)-M(220)+M(227)-M(228)-M(233) &
    +M(234))) * den(89)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,156)
  Gcoeff = (c(2)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(89)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,157)
  Gcoeff = (c(2)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(89)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,158)
  Gcoeff = (c(3)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(89)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,159)
  Gcoeff = (c(2)*(-M(149)+M(150)+M(159)-M(160)-M(173)+M(174)+M(192)-M(193)+M(194)-M(195)-M(216)+M(217)-M(218)+M(219)+M(227) &
    -M(228))) * den(89)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,160)
  Gcoeff = (c(2)*(M(149)-M(150)-M(159)+M(160)+M(173)-M(174)-M(192)+M(193)-M(194)+M(195)+M(216)-M(217)+M(218)-M(219)-M(227) &
    +M(228))) * den(89)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,161)
  Gcoeff = (c(3)*(M(149)-M(150)-M(159)+M(160)+M(173)-M(174)-M(192)+M(193)-M(194)+M(195)+M(216)-M(217)+M(218)-M(219)-M(227) &
    +M(228))) * den(89)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,162)
  Gcoeff = (c(2)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(562)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,25)
  Gcoeff = (c(2)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(562)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,70)
  Gcoeff = (c(3)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(562)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,115)
  Gcoeff = (c(2)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(562)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,26)
  Gcoeff = (c(2)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(562)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,71)
  Gcoeff = (c(3)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(562)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,116)
  Gcoeff = (c(2)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(562)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,27)
  Gcoeff = (c(2)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(562)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,72)
  Gcoeff = (c(3)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(562)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,117)
  Gcoeff = (c(2)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(571)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,28)
  Gcoeff = (c(2)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(571)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,73)
  Gcoeff = (c(3)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(571)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,118)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(571)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,29)
  Gcoeff = (c(2)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(571)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,74)
  Gcoeff = (c(3)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(571)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,119)
  Gcoeff = (c(2)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(571)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,30)
  Gcoeff = (c(2)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(571)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,75)
  Gcoeff = (c(3)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(571)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,120)
  Gcoeff = (c(2)*(M(131)-M(133)-M(155)+M(157)-M(191)+M(193)+M(215)-M(217)-M(227)+M(228)+M(233)-M(234)+M(243)-M(244)-M(249) &
    +M(250))) * den(927)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,31)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(155)-M(157)+M(191)-M(193)-M(215)+M(217)+M(227)-M(228)-M(233)+M(234)-M(243)+M(244)+M(249) &
    -M(250))) * den(927)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,76)
  Gcoeff = (c(3)*(-M(131)+M(133)+M(155)-M(157)+M(191)-M(193)-M(215)+M(217)+M(227)-M(228)-M(233)+M(234)-M(243)+M(244)+M(249) &
    -M(250))) * den(927)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,121)
  Gcoeff = (c(2)*(M(131)-M(155)-M(179)+M(185)-M(203)+M(209)+M(215)-M(217)-M(227)+M(233)+M(239)-M(241)+M(245)-M(247)-M(249) &
    +M(250))) * den(1186)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,32)
  Gcoeff = (c(2)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(1186)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,77)
  Gcoeff = (c(3)*(-M(131)+M(155)+M(179)-M(185)+M(203)-M(209)-M(215)+M(217)+M(227)-M(233)-M(239)+M(241)-M(245)+M(247)+M(249) &
    -M(250))) * den(1186)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,122)
  Gcoeff = (c(2)*(-M(133)+M(157)+M(179)-M(185)-M(191)+M(193)+M(203)-M(209)+M(228)-M(234)-M(239)+M(241)+M(243)-M(244)-M(245) &
    +M(247))) * den(1188)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,33)
  Gcoeff = (c(2)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(1188)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,78)
  Gcoeff = (c(3)*(M(133)-M(157)-M(179)+M(185)+M(191)-M(193)-M(203)+M(209)-M(228)+M(234)+M(239)-M(241)-M(243)+M(244)+M(245) &
    -M(247))) * den(1188)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,123)
  Gcoeff = (c(2)*(M(137)-M(139)-M(167)+M(169)-M(179)+M(181)+M(209)-M(211)-M(229)+M(230)+M(237)-M(238)+M(239)-M(240)-M(247) &
    +M(248))) * den(1012)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,34)
  Gcoeff = (c(2)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248))) * den(1012)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,79)
  Gcoeff = (c(3)*(-M(137)+M(139)+M(167)-M(169)+M(179)-M(181)-M(209)+M(211)+M(229)-M(230)-M(237)+M(238)-M(239)+M(240)+M(247) &
    -M(248))) * den(1012)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,124)
  Gcoeff = (c(2)*(-M(137)+M(155)-M(161)+M(179)+M(205)-M(209)+M(211)-M(215)+M(229)-M(233)+M(235)-M(239)-M(246)+M(247)-M(248) &
    +M(249))) * den(1210)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,35)
  Gcoeff = (c(2)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(1210)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,80)
  Gcoeff = (c(3)*(M(137)-M(155)+M(161)-M(179)-M(205)+M(209)-M(211)+M(215)-M(229)+M(233)-M(235)+M(239)+M(246)-M(247)+M(248) &
    -M(249))) * den(1210)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,125)
  Gcoeff = (c(2)*(-M(139)+M(155)-M(161)-M(167)+M(169)+M(181)+M(205)-M(215)+M(230)-M(233)+M(235)+M(237)-M(238)-M(240)-M(246) &
    +M(249))) * den(1212)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,36)
  Gcoeff = (c(2)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(1212)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,81)
  Gcoeff = (c(3)*(M(139)-M(155)+M(161)+M(167)-M(169)-M(181)-M(205)+M(215)-M(230)+M(233)-M(235)-M(237)+M(238)+M(240)+M(246) &
    -M(249))) * den(1212)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,126)
  Gcoeff = (c(2)*(-M(143)+M(145)+M(161)-M(163)-M(185)+M(187)+M(203)-M(205)+M(231)-M(232)-M(235)+M(236)+M(241)-M(242)-M(245) &
    +M(246))) * den(1054)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,37)
  Gcoeff = (c(2)*(M(143)-M(145)-M(161)+M(163)+M(185)-M(187)-M(203)+M(205)-M(231)+M(232)+M(235)-M(236)-M(241)+M(242)+M(245) &
    -M(246))) * den(1054)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,82)
  Gcoeff = (c(3)*(M(143)-M(145)-M(161)+M(163)+M(185)-M(187)-M(203)+M(205)-M(231)+M(232)+M(235)-M(236)-M(241)+M(242)+M(245) &
    -M(246))) * den(1054)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,127)
  Gcoeff = (c(2)*(M(131)-M(137)-M(161)+M(185)-M(203)+M(205)+M(211)-M(217)-M(227)+M(229)+M(235)-M(241)+M(245)-M(246)-M(248) &
    +M(250))) * den(1222)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,38)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(1222)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,83)
  Gcoeff = (c(3)*(-M(131)+M(137)+M(161)-M(185)+M(203)-M(205)-M(211)+M(217)+M(227)-M(229)-M(235)+M(241)-M(245)+M(246)+M(248) &
    -M(250))) * den(1222)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,128)
  Gcoeff = (c(2)*(M(131)-M(137)-M(143)+M(145)-M(163)+M(187)+M(211)-M(217)-M(227)+M(229)+M(231)-M(232)+M(236)-M(242)-M(248) &
    +M(250))) * den(1224)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,39)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(1224)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,84)
  Gcoeff = (c(3)*(-M(131)+M(137)+M(143)-M(145)+M(163)-M(187)-M(211)+M(217)+M(227)-M(229)-M(231)+M(232)-M(236)+M(242)+M(248) &
    -M(250))) * den(1224)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,129)
  Gcoeff = (c(2)*(M(143)-M(157)+M(167)-M(181)+M(185)-M(187)+M(191)-M(203)-M(231)+M(234)-M(237)+M(240)-M(241)+M(242)-M(243) &
    +M(245))) * den(1246)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,40)
  Gcoeff = (c(2)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(1246)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,85)
  Gcoeff = (c(3)*(-M(143)+M(157)-M(167)+M(181)-M(185)+M(187)-M(191)+M(203)+M(231)-M(234)+M(237)-M(240)+M(241)-M(242)+M(243) &
    -M(245))) * den(1246)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,130)
  Gcoeff = (c(2)*(M(145)-M(157)+M(161)-M(163)+M(167)-M(181)+M(191)-M(205)-M(232)+M(234)-M(235)+M(236)-M(237)+M(240)-M(243) &
    +M(246))) * den(1248)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,41)
  Gcoeff = (c(2)*(-M(145)+M(157)-M(161)+M(163)-M(167)+M(181)-M(191)+M(205)+M(232)-M(234)+M(235)-M(236)+M(237)-M(240)+M(243) &
    -M(246))) * den(1248)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,86)
  Gcoeff = (c(3)*(-M(145)+M(157)-M(161)+M(163)-M(167)+M(181)-M(191)+M(205)+M(232)-M(234)+M(235)-M(236)+M(237)-M(240)+M(243) &
    -M(246))) * den(1248)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,131)
  Gcoeff = (c(2)*(-M(133)+M(143)+M(167)+M(179)-M(181)-M(187)+M(193)-M(209)+M(228)-M(231)-M(237)-M(239)+M(240)+M(242)-M(244) &
    +M(247))) * den(1258)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,42)
  Gcoeff = (c(2)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(1258)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,87)
  Gcoeff = (c(3)*(M(133)-M(143)-M(167)-M(179)+M(181)+M(187)-M(193)+M(209)-M(228)+M(231)+M(237)+M(239)-M(240)-M(242)+M(244) &
    -M(247))) * den(1258)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,132)
  Gcoeff = (c(2)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(1260)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,43)
  Gcoeff = (c(2)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(1260)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,88)
  Gcoeff = (c(3)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(1260)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,133)
  Gcoeff = (c(2)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(1270)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,44)
  Gcoeff = (c(2)*(M(139)-M(145)-M(155)+M(157)+M(163)-M(169)-M(191)+M(215)-M(230)+M(232)+M(233)-M(234)-M(236)+M(238)+M(243) &
    -M(249))) * den(1270)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,89)
  Gcoeff = (c(3)*(M(139)-M(145)-M(155)+M(157)+M(163)-M(169)-M(191)+M(215)-M(230)+M(232)+M(233)-M(234)-M(236)+M(238)+M(243) &
    -M(249))) * den(1270)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,134)
  Gcoeff = (c(2)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(1271)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,45)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(139)-M(145)+M(163)-M(169)-M(193)+M(217)+M(227)-M(228)-M(230)+M(232)-M(236)+M(238)+M(244) &
    -M(250))) * den(1271)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,90)
  Gcoeff = (c(3)*(-M(131)+M(133)+M(139)-M(145)+M(163)-M(169)-M(193)+M(217)+M(227)-M(228)-M(230)+M(232)-M(236)+M(238)+M(244) &
    -M(250))) * den(1271)
  T2sum(1:5,15) = T2sum(1:5,15) + Gcoeff * G1tensor(:,135)

end subroutine vamp_70

end module ol_vamp_70_ppjjjj_gggggg_1_/**/REALKIND
