
module ol_vamp_64_ppjjjj_gggggg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_64(M)
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
  complex(REALKIND), dimension(4,1,4,48) :: G0
  complex(REALKIND), dimension(4,5,4,93) :: G1
  complex(REALKIND), dimension(4,15,4,93) :: G2
  complex(REALKIND), dimension(1,189) :: G0tensor
  complex(REALKIND), dimension(5,99) :: G1tensor
  complex(REALKIND), dimension(35,57) :: G3tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,1425),Q(:,30),G1(:,:,:,1))
  call loop_CV_D(G1(:,:,:,1),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,1))
  call check_last_CV_D(l_switch,G2(:,:,:,1),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,1))
  call loop_DV_C(G0(:,:,:,1),Q(:,0),wf(:,1425),G1(:,:,:,2))
  call loop_DV_C(G1(:,:,:,2),Q(:,30),wf(:,-5),G2(:,:,:,2))
  call check_last_DV_C(l_switch,G2(:,:,:,2),Q(:,62),wf(:,0),G3tensor(:,2))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1425),Q(:,30),G1(:,:,:,3))
  call loop_UV_W(G1(:,:,:,3),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,3))
  call check_last_UV_W(l_switch,G2(:,:,:,3),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,3))
  call loop_QV_A(G0(:,:,:,1),wf(:,1429),G0(:,:,:,2))
  call loop_Q_A(G0(:,:,:,2),Q(:,27),ZERO,G1(:,:,:,4))
  call loop_QV_A(G1(:,:,:,4),wf(:,-5),G1(:,:,:,5))
  call loop_Q_A(G1(:,:,:,5),Q(:,59),ZERO,G2(:,:,:,4))
  call loop_QV_A(G2(:,:,:,4),wf(:,-2),G2(:,:,:,5))
  call check_last_Q_A(l_switch,G2(:,:,:,5),Q(:,63),ZERO,G3tensor(:,4))
  call loop_QV_A(G0(:,:,:,1),wf(:,1429),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,27),MT,G1(:,:,:,6))
  call loop_QV_A(G1(:,:,:,6),wf(:,-5),G1(:,:,:,7))
  call loop_Q_A(G1(:,:,:,7),Q(:,59),MT,G2(:,:,:,6))
  call loop_QV_A(G2(:,:,:,6),wf(:,-2),G2(:,:,:,7))
  call check_last_Q_A(l_switch,G2(:,:,:,7),Q(:,63),MT,G3tensor(:,5))
  call loop_AV_Q(G0(:,:,:,1),wf(:,1429),G0(:,:,:,4))
  call loop_A_Q(G0(:,:,:,4),Q(:,27),ZERO,G1(:,:,:,8))
  call loop_AV_Q(G1(:,:,:,8),wf(:,-5),G1(:,:,:,9))
  call loop_A_Q(G1(:,:,:,9),Q(:,59),ZERO,G2(:,:,:,8))
  call loop_AV_Q(G2(:,:,:,8),wf(:,-2),G2(:,:,:,9))
  call check_last_A_Q(l_switch,G2(:,:,:,9),Q(:,63),ZERO,G3tensor(:,6))
  call loop_AV_Q(G0(:,:,:,1),wf(:,1429),G0(:,:,:,5))
  call loop_A_Q(G0(:,:,:,5),Q(:,27),MT,G1(:,:,:,10))
  call loop_AV_Q(G1(:,:,:,10),wf(:,-5),G1(:,:,:,11))
  call loop_A_Q(G1(:,:,:,11),Q(:,59),MT,G2(:,:,:,10))
  call loop_AV_Q(G2(:,:,:,10),wf(:,-2),G2(:,:,:,11))
  call check_last_A_Q(l_switch,G2(:,:,:,11),Q(:,63),MT,G3tensor(:,7))
  call loop_QV_A(G0(:,:,:,1),wf(:,1429),G0(:,:,:,6))
  call loop_Q_A(G0(:,:,:,6),Q(:,27),MB,G1(:,:,:,12))
  call loop_QV_A(G1(:,:,:,12),wf(:,-5),G1(:,:,:,13))
  call loop_Q_A(G1(:,:,:,13),Q(:,59),MB,G2(:,:,:,12))
  call loop_QV_A(G2(:,:,:,12),wf(:,-2),G2(:,:,:,13))
  call check_last_Q_A(l_switch,G2(:,:,:,13),Q(:,63),MB,G3tensor(:,8))
  call loop_AV_Q(G0(:,:,:,1),wf(:,1429),G0(:,:,:,7))
  call loop_A_Q(G0(:,:,:,7),Q(:,27),MB,G1(:,:,:,14))
  call loop_AV_Q(G1(:,:,:,14),wf(:,-5),G1(:,:,:,15))
  call loop_A_Q(G1(:,:,:,15),Q(:,59),MB,G2(:,:,:,14))
  call loop_AV_Q(G2(:,:,:,14),wf(:,-2),G2(:,:,:,15))
  call check_last_A_Q(l_switch,G2(:,:,:,15),Q(:,63),MB,G3tensor(:,9))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,1429),Q(:,27),G1(:,:,:,16))
  call loop_CV_D(G1(:,:,:,16),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,16))
  call check_last_CV_D(l_switch,G2(:,:,:,16),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,10))
  call loop_DV_C(G0(:,:,:,1),Q(:,0),wf(:,1429),G1(:,:,:,17))
  call loop_DV_C(G1(:,:,:,17),Q(:,27),wf(:,-5),G2(:,:,:,17))
  call check_last_DV_C(l_switch,G2(:,:,:,17),Q(:,59),wf(:,-2),G3tensor(:,11))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1429),Q(:,27),G1(:,:,:,18))
  call loop_UV_W(G1(:,:,:,18),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,18))
  call check_last_UV_W(l_switch,G2(:,:,:,18),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,12))
  call loop_QV_A(G0(:,:,:,1),wf(:,1430),G0(:,:,:,8))
  call loop_Q_A(G0(:,:,:,8),Q(:,27),ZERO,G1(:,:,:,19))
  call loop_QV_A(G1(:,:,:,19),wf(:,-5),G1(:,:,:,20))
  call loop_Q_A(G1(:,:,:,20),Q(:,59),ZERO,G2(:,:,:,19))
  call loop_QV_A(G2(:,:,:,19),wf(:,-2),G2(:,:,:,20))
  call check_last_Q_A(l_switch,G2(:,:,:,20),Q(:,63),ZERO,G3tensor(:,13))
  call loop_QV_A(G0(:,:,:,1),wf(:,1430),G0(:,:,:,9))
  call loop_Q_A(G0(:,:,:,9),Q(:,27),MT,G1(:,:,:,21))
  call loop_QV_A(G1(:,:,:,21),wf(:,-5),G1(:,:,:,22))
  call loop_Q_A(G1(:,:,:,22),Q(:,59),MT,G2(:,:,:,21))
  call loop_QV_A(G2(:,:,:,21),wf(:,-2),G2(:,:,:,22))
  call check_last_Q_A(l_switch,G2(:,:,:,22),Q(:,63),MT,G3tensor(:,14))
  call loop_AV_Q(G0(:,:,:,1),wf(:,1430),G0(:,:,:,10))
  call loop_A_Q(G0(:,:,:,10),Q(:,27),ZERO,G1(:,:,:,23))
  call loop_AV_Q(G1(:,:,:,23),wf(:,-5),G1(:,:,:,24))
  call loop_A_Q(G1(:,:,:,24),Q(:,59),ZERO,G2(:,:,:,23))
  call loop_AV_Q(G2(:,:,:,23),wf(:,-2),G2(:,:,:,24))
  call check_last_A_Q(l_switch,G2(:,:,:,24),Q(:,63),ZERO,G3tensor(:,15))
  call loop_AV_Q(G0(:,:,:,1),wf(:,1430),G0(:,:,:,11))
  call loop_A_Q(G0(:,:,:,11),Q(:,27),MT,G1(:,:,:,25))
  call loop_AV_Q(G1(:,:,:,25),wf(:,-5),G1(:,:,:,26))
  call loop_A_Q(G1(:,:,:,26),Q(:,59),MT,G2(:,:,:,25))
  call loop_AV_Q(G2(:,:,:,25),wf(:,-2),G2(:,:,:,26))
  call check_last_A_Q(l_switch,G2(:,:,:,26),Q(:,63),MT,G3tensor(:,16))
  call loop_QV_A(G0(:,:,:,1),wf(:,1430),G0(:,:,:,12))
  call loop_Q_A(G0(:,:,:,12),Q(:,27),MB,G1(:,:,:,27))
  call loop_QV_A(G1(:,:,:,27),wf(:,-5),G1(:,:,:,28))
  call loop_Q_A(G1(:,:,:,28),Q(:,59),MB,G2(:,:,:,27))
  call loop_QV_A(G2(:,:,:,27),wf(:,-2),G2(:,:,:,28))
  call check_last_Q_A(l_switch,G2(:,:,:,28),Q(:,63),MB,G3tensor(:,17))
  call loop_AV_Q(G0(:,:,:,1),wf(:,1430),G0(:,:,:,13))
  call loop_A_Q(G0(:,:,:,13),Q(:,27),MB,G1(:,:,:,29))
  call loop_AV_Q(G1(:,:,:,29),wf(:,-5),G1(:,:,:,30))
  call loop_A_Q(G1(:,:,:,30),Q(:,59),MB,G2(:,:,:,29))
  call loop_AV_Q(G2(:,:,:,29),wf(:,-2),G2(:,:,:,30))
  call check_last_A_Q(l_switch,G2(:,:,:,30),Q(:,63),MB,G3tensor(:,18))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,1430),Q(:,27),G1(:,:,:,31))
  call loop_CV_D(G1(:,:,:,31),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,31))
  call check_last_CV_D(l_switch,G2(:,:,:,31),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,19))
  call loop_DV_C(G0(:,:,:,1),Q(:,0),wf(:,1430),G1(:,:,:,32))
  call loop_DV_C(G1(:,:,:,32),Q(:,27),wf(:,-5),G2(:,:,:,32))
  call check_last_DV_C(l_switch,G2(:,:,:,32),Q(:,59),wf(:,-2),G3tensor(:,20))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1430),Q(:,27),G1(:,:,:,33))
  call loop_UV_W(G1(:,:,:,33),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,33))
  call check_last_UV_W(l_switch,G2(:,:,:,33),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,21))
  call loop_QV_A(G0(:,:,:,1),wf(:,1431),G0(:,:,:,14))
  call loop_Q_A(G0(:,:,:,14),Q(:,29),ZERO,G1(:,:,:,34))
  call loop_QV_A(G1(:,:,:,34),wf(:,-5),G1(:,:,:,35))
  call loop_Q_A(G1(:,:,:,35),Q(:,61),ZERO,G2(:,:,:,34))
  call loop_QV_A(G2(:,:,:,34),wf(:,-1),G2(:,:,:,35))
  call check_last_Q_A(l_switch,G2(:,:,:,35),Q(:,63),ZERO,G3tensor(:,22))
  call loop_QV_A(G0(:,:,:,1),wf(:,1431),G0(:,:,:,15))
  call loop_Q_A(G0(:,:,:,15),Q(:,29),MT,G1(:,:,:,36))
  call loop_QV_A(G1(:,:,:,36),wf(:,-5),G1(:,:,:,37))
  call loop_Q_A(G1(:,:,:,37),Q(:,61),MT,G2(:,:,:,36))
  call loop_QV_A(G2(:,:,:,36),wf(:,-1),G2(:,:,:,37))
  call check_last_Q_A(l_switch,G2(:,:,:,37),Q(:,63),MT,G3tensor(:,23))
  call loop_AV_Q(G0(:,:,:,1),wf(:,1431),G0(:,:,:,16))
  call loop_A_Q(G0(:,:,:,16),Q(:,29),ZERO,G1(:,:,:,38))
  call loop_AV_Q(G1(:,:,:,38),wf(:,-5),G1(:,:,:,39))
  call loop_A_Q(G1(:,:,:,39),Q(:,61),ZERO,G2(:,:,:,38))
  call loop_AV_Q(G2(:,:,:,38),wf(:,-1),G2(:,:,:,39))
  call check_last_A_Q(l_switch,G2(:,:,:,39),Q(:,63),ZERO,G3tensor(:,24))
  call loop_AV_Q(G0(:,:,:,1),wf(:,1431),G0(:,:,:,17))
  call loop_A_Q(G0(:,:,:,17),Q(:,29),MT,G1(:,:,:,40))
  call loop_AV_Q(G1(:,:,:,40),wf(:,-5),G1(:,:,:,41))
  call loop_A_Q(G1(:,:,:,41),Q(:,61),MT,G2(:,:,:,40))
  call loop_AV_Q(G2(:,:,:,40),wf(:,-1),G2(:,:,:,41))
  call check_last_A_Q(l_switch,G2(:,:,:,41),Q(:,63),MT,G3tensor(:,25))
  call loop_QV_A(G0(:,:,:,1),wf(:,1431),G0(:,:,:,18))
  call loop_Q_A(G0(:,:,:,18),Q(:,29),MB,G1(:,:,:,42))
  call loop_QV_A(G1(:,:,:,42),wf(:,-5),G1(:,:,:,43))
  call loop_Q_A(G1(:,:,:,43),Q(:,61),MB,G2(:,:,:,42))
  call loop_QV_A(G2(:,:,:,42),wf(:,-1),G2(:,:,:,43))
  call check_last_Q_A(l_switch,G2(:,:,:,43),Q(:,63),MB,G3tensor(:,26))
  call loop_AV_Q(G0(:,:,:,1),wf(:,1431),G0(:,:,:,19))
  call loop_A_Q(G0(:,:,:,19),Q(:,29),MB,G1(:,:,:,44))
  call loop_AV_Q(G1(:,:,:,44),wf(:,-5),G1(:,:,:,45))
  call loop_A_Q(G1(:,:,:,45),Q(:,61),MB,G2(:,:,:,44))
  call loop_AV_Q(G2(:,:,:,44),wf(:,-1),G2(:,:,:,45))
  call check_last_A_Q(l_switch,G2(:,:,:,45),Q(:,63),MB,G3tensor(:,27))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,1431),Q(:,29),G1(:,:,:,46))
  call loop_CV_D(G1(:,:,:,46),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,46))
  call check_last_CV_D(l_switch,G2(:,:,:,46),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,28))
  call loop_DV_C(G0(:,:,:,1),Q(:,0),wf(:,1431),G1(:,:,:,47))
  call loop_DV_C(G1(:,:,:,47),Q(:,29),wf(:,-5),G2(:,:,:,47))
  call check_last_DV_C(l_switch,G2(:,:,:,47),Q(:,61),wf(:,-1),G3tensor(:,29))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1431),Q(:,29),G1(:,:,:,48))
  call loop_UV_W(G1(:,:,:,48),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,48))
  call check_last_UV_W(l_switch,G2(:,:,:,48),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,30))
  call loop_QV_A(G0(:,:,:,1),wf(:,1432),G0(:,:,:,20))
  call loop_Q_A(G0(:,:,:,20),Q(:,29),ZERO,G1(:,:,:,49))
  call loop_QV_A(G1(:,:,:,49),wf(:,-5),G1(:,:,:,50))
  call loop_Q_A(G1(:,:,:,50),Q(:,61),ZERO,G2(:,:,:,49))
  call loop_QV_A(G2(:,:,:,49),wf(:,-1),G2(:,:,:,50))
  call check_last_Q_A(l_switch,G2(:,:,:,50),Q(:,63),ZERO,G3tensor(:,31))
  call loop_QV_A(G0(:,:,:,1),wf(:,1432),G0(:,:,:,21))
  call loop_Q_A(G0(:,:,:,21),Q(:,29),MT,G1(:,:,:,51))
  call loop_QV_A(G1(:,:,:,51),wf(:,-5),G1(:,:,:,52))
  call loop_Q_A(G1(:,:,:,52),Q(:,61),MT,G2(:,:,:,51))
  call loop_QV_A(G2(:,:,:,51),wf(:,-1),G2(:,:,:,52))
  call check_last_Q_A(l_switch,G2(:,:,:,52),Q(:,63),MT,G3tensor(:,32))
  call loop_AV_Q(G0(:,:,:,1),wf(:,1432),G0(:,:,:,22))
  call loop_A_Q(G0(:,:,:,22),Q(:,29),ZERO,G1(:,:,:,53))
  call loop_AV_Q(G1(:,:,:,53),wf(:,-5),G1(:,:,:,54))
  call loop_A_Q(G1(:,:,:,54),Q(:,61),ZERO,G2(:,:,:,53))
  call loop_AV_Q(G2(:,:,:,53),wf(:,-1),G2(:,:,:,54))
  call check_last_A_Q(l_switch,G2(:,:,:,54),Q(:,63),ZERO,G3tensor(:,33))
  call loop_AV_Q(G0(:,:,:,1),wf(:,1432),G0(:,:,:,23))
  call loop_A_Q(G0(:,:,:,23),Q(:,29),MT,G1(:,:,:,55))
  call loop_AV_Q(G1(:,:,:,55),wf(:,-5),G1(:,:,:,56))
  call loop_A_Q(G1(:,:,:,56),Q(:,61),MT,G2(:,:,:,55))
  call loop_AV_Q(G2(:,:,:,55),wf(:,-1),G2(:,:,:,56))
  call check_last_A_Q(l_switch,G2(:,:,:,56),Q(:,63),MT,G3tensor(:,34))
  call loop_QV_A(G0(:,:,:,1),wf(:,1432),G0(:,:,:,24))
  call loop_Q_A(G0(:,:,:,24),Q(:,29),MB,G1(:,:,:,57))
  call loop_QV_A(G1(:,:,:,57),wf(:,-5),G1(:,:,:,58))
  call loop_Q_A(G1(:,:,:,58),Q(:,61),MB,G2(:,:,:,57))
  call loop_QV_A(G2(:,:,:,57),wf(:,-1),G2(:,:,:,58))
  call check_last_Q_A(l_switch,G2(:,:,:,58),Q(:,63),MB,G3tensor(:,35))
  call loop_AV_Q(G0(:,:,:,1),wf(:,1432),G0(:,:,:,25))
  call loop_A_Q(G0(:,:,:,25),Q(:,29),MB,G1(:,:,:,59))
  call loop_AV_Q(G1(:,:,:,59),wf(:,-5),G1(:,:,:,60))
  call loop_A_Q(G1(:,:,:,60),Q(:,61),MB,G2(:,:,:,59))
  call loop_AV_Q(G2(:,:,:,59),wf(:,-1),G2(:,:,:,60))
  call check_last_A_Q(l_switch,G2(:,:,:,60),Q(:,63),MB,G3tensor(:,36))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,1432),Q(:,29),G1(:,:,:,61))
  call loop_CV_D(G1(:,:,:,61),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,61))
  call check_last_CV_D(l_switch,G2(:,:,:,61),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,37))
  call loop_DV_C(G0(:,:,:,1),Q(:,0),wf(:,1432),G1(:,:,:,62))
  call loop_DV_C(G1(:,:,:,62),Q(:,29),wf(:,-5),G2(:,:,:,62))
  call check_last_DV_C(l_switch,G2(:,:,:,62),Q(:,61),wf(:,-1),G3tensor(:,38))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1432),Q(:,29),G1(:,:,:,63))
  call loop_UV_W(G1(:,:,:,63),Q(:,29),wf(:,-5),Q(:,32),G2(:,:,:,63))
  call check_last_UV_W(l_switch,G2(:,:,:,63),Q(:,61),wf(:,-1),Q(:,2),G3tensor(:,39))
  call loop_QV_A(G0(:,:,:,1),wf(:,1435),G0(:,:,:,26))
  call loop_Q_A(G0(:,:,:,26),Q(:,30),ZERO,G1(:,:,:,64))
  call loop_QV_A(G1(:,:,:,64),wf(:,-5),G1(:,:,:,65))
  call loop_Q_A(G1(:,:,:,65),Q(:,62),ZERO,G2(:,:,:,64))
  call loop_QV_A(G2(:,:,:,64),wf(:,0),G2(:,:,:,65))
  call check_last_Q_A(l_switch,G2(:,:,:,65),Q(:,63),ZERO,G3tensor(:,40))
  call loop_QV_A(G0(:,:,:,1),wf(:,1435),G0(:,:,:,27))
  call loop_Q_A(G0(:,:,:,27),Q(:,30),MT,G1(:,:,:,66))
  call loop_QV_A(G1(:,:,:,66),wf(:,-5),G1(:,:,:,67))
  call loop_Q_A(G1(:,:,:,67),Q(:,62),MT,G2(:,:,:,66))
  call loop_QV_A(G2(:,:,:,66),wf(:,0),G2(:,:,:,67))
  call check_last_Q_A(l_switch,G2(:,:,:,67),Q(:,63),MT,G3tensor(:,41))
  call loop_AV_Q(G0(:,:,:,1),wf(:,1435),G0(:,:,:,28))
  call loop_A_Q(G0(:,:,:,28),Q(:,30),ZERO,G1(:,:,:,68))
  call loop_AV_Q(G1(:,:,:,68),wf(:,-5),G1(:,:,:,69))
  call loop_A_Q(G1(:,:,:,69),Q(:,62),ZERO,G2(:,:,:,68))
  call loop_AV_Q(G2(:,:,:,68),wf(:,0),G2(:,:,:,69))
  call check_last_A_Q(l_switch,G2(:,:,:,69),Q(:,63),ZERO,G3tensor(:,42))
  call loop_AV_Q(G0(:,:,:,1),wf(:,1435),G0(:,:,:,29))
  call loop_A_Q(G0(:,:,:,29),Q(:,30),MT,G1(:,:,:,70))
  call loop_AV_Q(G1(:,:,:,70),wf(:,-5),G1(:,:,:,71))
  call loop_A_Q(G1(:,:,:,71),Q(:,62),MT,G2(:,:,:,70))
  call loop_AV_Q(G2(:,:,:,70),wf(:,0),G2(:,:,:,71))
  call check_last_A_Q(l_switch,G2(:,:,:,71),Q(:,63),MT,G3tensor(:,43))
  call loop_QV_A(G0(:,:,:,1),wf(:,1435),G0(:,:,:,30))
  call loop_Q_A(G0(:,:,:,30),Q(:,30),MB,G1(:,:,:,72))
  call loop_QV_A(G1(:,:,:,72),wf(:,-5),G1(:,:,:,73))
  call loop_Q_A(G1(:,:,:,73),Q(:,62),MB,G2(:,:,:,72))
  call loop_QV_A(G2(:,:,:,72),wf(:,0),G2(:,:,:,73))
  call check_last_Q_A(l_switch,G2(:,:,:,73),Q(:,63),MB,G3tensor(:,44))
  call loop_AV_Q(G0(:,:,:,1),wf(:,1435),G0(:,:,:,31))
  call loop_A_Q(G0(:,:,:,31),Q(:,30),MB,G1(:,:,:,74))
  call loop_AV_Q(G1(:,:,:,74),wf(:,-5),G1(:,:,:,75))
  call loop_A_Q(G1(:,:,:,75),Q(:,62),MB,G2(:,:,:,74))
  call loop_AV_Q(G2(:,:,:,74),wf(:,0),G2(:,:,:,75))
  call check_last_A_Q(l_switch,G2(:,:,:,75),Q(:,63),MB,G3tensor(:,45))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,1435),Q(:,30),G1(:,:,:,76))
  call loop_CV_D(G1(:,:,:,76),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,76))
  call check_last_CV_D(l_switch,G2(:,:,:,76),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,46))
  call loop_DV_C(G0(:,:,:,1),Q(:,0),wf(:,1435),G1(:,:,:,77))
  call loop_DV_C(G1(:,:,:,77),Q(:,30),wf(:,-5),G2(:,:,:,77))
  call check_last_DV_C(l_switch,G2(:,:,:,77),Q(:,62),wf(:,0),G3tensor(:,47))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1435),Q(:,30),G1(:,:,:,78))
  call loop_UV_W(G1(:,:,:,78),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,78))
  call check_last_UV_W(l_switch,G2(:,:,:,78),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,48))
  call loop_QV_A(G0(:,:,:,1),wf(:,1436),G0(:,:,:,32))
  call loop_Q_A(G0(:,:,:,32),Q(:,30),ZERO,G1(:,:,:,79))
  call loop_QV_A(G1(:,:,:,79),wf(:,-5),G1(:,:,:,80))
  call loop_Q_A(G1(:,:,:,80),Q(:,62),ZERO,G2(:,:,:,79))
  call loop_QV_A(G2(:,:,:,79),wf(:,0),G2(:,:,:,80))
  call check_last_Q_A(l_switch,G2(:,:,:,80),Q(:,63),ZERO,G3tensor(:,49))
  call loop_QV_A(G0(:,:,:,1),wf(:,1436),G0(:,:,:,33))
  call loop_Q_A(G0(:,:,:,33),Q(:,30),MT,G1(:,:,:,81))
  call loop_QV_A(G1(:,:,:,81),wf(:,-5),G1(:,:,:,82))
  call loop_Q_A(G1(:,:,:,82),Q(:,62),MT,G2(:,:,:,81))
  call loop_QV_A(G2(:,:,:,81),wf(:,0),G2(:,:,:,82))
  call check_last_Q_A(l_switch,G2(:,:,:,82),Q(:,63),MT,G3tensor(:,50))
  call loop_AV_Q(G0(:,:,:,1),wf(:,1436),G0(:,:,:,34))
  call loop_A_Q(G0(:,:,:,34),Q(:,30),ZERO,G1(:,:,:,83))
  call loop_AV_Q(G1(:,:,:,83),wf(:,-5),G1(:,:,:,84))
  call loop_A_Q(G1(:,:,:,84),Q(:,62),ZERO,G2(:,:,:,83))
  call loop_AV_Q(G2(:,:,:,83),wf(:,0),G2(:,:,:,84))
  call check_last_A_Q(l_switch,G2(:,:,:,84),Q(:,63),ZERO,G3tensor(:,51))
  call loop_AV_Q(G0(:,:,:,1),wf(:,1436),G0(:,:,:,35))
  call loop_A_Q(G0(:,:,:,35),Q(:,30),MT,G1(:,:,:,85))
  call loop_AV_Q(G1(:,:,:,85),wf(:,-5),G1(:,:,:,86))
  call loop_A_Q(G1(:,:,:,86),Q(:,62),MT,G2(:,:,:,85))
  call loop_AV_Q(G2(:,:,:,85),wf(:,0),G2(:,:,:,86))
  call check_last_A_Q(l_switch,G2(:,:,:,86),Q(:,63),MT,G3tensor(:,52))
  call loop_QV_A(G0(:,:,:,1),wf(:,1436),G0(:,:,:,36))
  call loop_Q_A(G0(:,:,:,36),Q(:,30),MB,G1(:,:,:,87))
  call loop_QV_A(G1(:,:,:,87),wf(:,-5),G1(:,:,:,88))
  call loop_Q_A(G1(:,:,:,88),Q(:,62),MB,G2(:,:,:,87))
  call loop_QV_A(G2(:,:,:,87),wf(:,0),G2(:,:,:,88))
  call check_last_Q_A(l_switch,G2(:,:,:,88),Q(:,63),MB,G3tensor(:,53))
  call loop_AV_Q(G0(:,:,:,1),wf(:,1436),G0(:,:,:,37))
  call loop_A_Q(G0(:,:,:,37),Q(:,30),MB,G1(:,:,:,89))
  call loop_AV_Q(G1(:,:,:,89),wf(:,-5),G1(:,:,:,90))
  call loop_A_Q(G1(:,:,:,90),Q(:,62),MB,G2(:,:,:,89))
  call loop_AV_Q(G2(:,:,:,89),wf(:,0),G2(:,:,:,90))
  call check_last_A_Q(l_switch,G2(:,:,:,90),Q(:,63),MB,G3tensor(:,54))
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,1436),Q(:,30),G1(:,:,:,91))
  call loop_CV_D(G1(:,:,:,91),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,91))
  call check_last_CV_D(l_switch,G2(:,:,:,91),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,55))
  call loop_DV_C(G0(:,:,:,1),Q(:,0),wf(:,1436),G1(:,:,:,92))
  call loop_DV_C(G1(:,:,:,92),Q(:,30),wf(:,-5),G2(:,:,:,92))
  call check_last_DV_C(l_switch,G2(:,:,:,92),Q(:,62),wf(:,0),G3tensor(:,56))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,1436),Q(:,30),G1(:,:,:,93))
  call loop_UV_W(G1(:,:,:,93),Q(:,30),wf(:,-5),Q(:,32),G2(:,:,:,93))
  call check_last_UV_W(l_switch,G2(:,:,:,93),Q(:,62),wf(:,0),Q(:,1),G3tensor(:,57))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,1),G0(:,:,:,38))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,38),wf(:,-4),wf(:,-3),G0tensor(:,1))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,38),wf(:,-3),wf(:,-4),G0tensor(:,2))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,38),wf(:,-4),wf(:,-3),G0tensor(:,3))
  call check_last_UV_W(l_switch,G0(:,:,:,38),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,1))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,3),G0(:,:,:,39))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,39),wf(:,-4),wf(:,-3),G0tensor(:,4))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,39),wf(:,-3),wf(:,-4),G0tensor(:,5))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,39),wf(:,-4),wf(:,-3),G0tensor(:,6))
  call check_last_UV_W(l_switch,G0(:,:,:,39),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,2))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,4),G0(:,:,:,40))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,40),wf(:,-4),wf(:,-3),G0tensor(:,7))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,40),wf(:,-3),wf(:,-4),G0tensor(:,8))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,40),wf(:,-4),wf(:,-3),G0tensor(:,9))
  call check_last_UV_W(l_switch,G0(:,:,:,40),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,3))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,1),wf(:,-5),G0(:,:,:,41))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,41),wf(:,-4),wf(:,-3),G0tensor(:,10))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,41),wf(:,-3),wf(:,-4),G0tensor(:,11))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,41),wf(:,-4),wf(:,-3),G0tensor(:,12))
  call check_last_UV_W(l_switch,G0(:,:,:,41),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,4))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,3),wf(:,-5),G0(:,:,:,42))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,42),wf(:,-4),wf(:,-3),G0tensor(:,13))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,42),wf(:,-3),wf(:,-4),G0tensor(:,14))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,42),wf(:,-4),wf(:,-3),G0tensor(:,15))
  call check_last_UV_W(l_switch,G0(:,:,:,42),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,5))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,4),wf(:,-5),G0(:,:,:,43))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,43),wf(:,-4),wf(:,-3),G0tensor(:,16))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,43),wf(:,-3),wf(:,-4),G0tensor(:,17))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,43),wf(:,-4),wf(:,-3),G0tensor(:,18))
  call check_last_UV_W(l_switch,G0(:,:,:,43),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,6))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,1),G0(:,:,:,44))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,44),wf(:,-4),wf(:,-3),G0tensor(:,19))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,44),wf(:,-3),wf(:,-4),G0tensor(:,20))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,44),wf(:,-4),wf(:,-3),G0tensor(:,21))
  call check_last_UV_W(l_switch,G0(:,:,:,44),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,7))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,3),G0(:,:,:,45))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,45),wf(:,-4),wf(:,-3),G0tensor(:,22))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,45),wf(:,-3),wf(:,-4),G0tensor(:,23))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,45),wf(:,-4),wf(:,-3),G0tensor(:,24))
  call check_last_UV_W(l_switch,G0(:,:,:,45),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,8))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,4),G0(:,:,:,46))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,46),wf(:,-4),wf(:,-3),G0tensor(:,25))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,46),wf(:,-3),wf(:,-4),G0tensor(:,26))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,46),wf(:,-4),wf(:,-3),G0tensor(:,27))
  call check_last_UV_W(l_switch,G0(:,:,:,46),Q(:,39),wf(:,75),Q(:,24),G1tensor(:,9))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,-3),G0(:,:,:,47))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-4),wf(:,1),G0tensor(:,28))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-4),wf(:,3),G0tensor(:,29))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-4),wf(:,4),G0tensor(:,30))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,1),wf(:,-4),G0tensor(:,31))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,3),wf(:,-4),G0tensor(:,32))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,4),wf(:,-4),G0tensor(:,33))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,-4),wf(:,1),G0tensor(:,34))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,-4),wf(:,3),G0tensor(:,35))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,-4),wf(:,4),G0tensor(:,36))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-2),wf(:,13),G0tensor(:,37))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-2),wf(:,15),G0tensor(:,38))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-2),wf(:,16),G0tensor(:,39))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,13),wf(:,-2),G0tensor(:,40))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,15),wf(:,-2),G0tensor(:,41))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,16),wf(:,-2),G0tensor(:,42))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,-2),wf(:,13),G0tensor(:,43))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,-2),wf(:,15),G0tensor(:,44))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,-2),wf(:,16),G0tensor(:,45))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-1),wf(:,31),G0tensor(:,46))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-1),wf(:,33),G0tensor(:,47))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-1),wf(:,34),G0tensor(:,48))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,31),wf(:,-1),G0tensor(:,49))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,33),wf(:,-1),G0tensor(:,50))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,34),wf(:,-1),G0tensor(:,51))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,-1),wf(:,31),G0tensor(:,52))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,-1),wf(:,33),G0tensor(:,53))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,-1),wf(:,34),G0tensor(:,54))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,0),wf(:,50),G0tensor(:,55))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,50),wf(:,0),G0tensor(:,56))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,0),wf(:,50),G0tensor(:,57))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,0),wf(:,53),G0tensor(:,58))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,53),wf(:,0),G0tensor(:,59))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,0),wf(:,53),G0tensor(:,60))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,0),wf(:,54),G0tensor(:,61))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,54),wf(:,0),G0tensor(:,62))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,0),wf(:,54),G0tensor(:,63))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,61),wf(:,66),G0tensor(:,64))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,66),wf(:,61),G0tensor(:,65))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,61),wf(:,66),G0tensor(:,66))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-4),wf(:,74),G0tensor(:,67))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,74),wf(:,-4),G0tensor(:,68))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,-4),wf(:,74),G0tensor(:,69))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-2),wf(:,88),G0tensor(:,70))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,88),wf(:,-2),G0tensor(:,71))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,-2),wf(:,88),G0tensor(:,72))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,80),Q(:,23),G1tensor(:,10))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,81),Q(:,23),G1tensor(:,11))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,82),Q(:,23),G1tensor(:,12))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,90),wf(:,95),G0tensor(:,73))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,95),wf(:,90),G0tensor(:,74))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,90),wf(:,95),G0tensor(:,75))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-4),wf(:,103),G0tensor(:,76))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,103),wf(:,-4),G0tensor(:,77))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,-4),wf(:,103),G0tensor(:,78))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,105),wf(:,109),G0tensor(:,79))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,109),wf(:,105),G0tensor(:,80))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,105),wf(:,109),G0tensor(:,81))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-4),wf(:,117),G0tensor(:,82))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,117),wf(:,-4),G0tensor(:,83))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,-4),wf(:,117),G0tensor(:,84))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-2),wf(:,135),G0tensor(:,85))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,135),wf(:,-2),G0tensor(:,86))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,-2),wf(:,135),G0tensor(:,87))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-2),wf(:,139),G0tensor(:,88))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,139),wf(:,-2),G0tensor(:,89))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,-2),wf(:,139),G0tensor(:,90))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-1),wf(:,152),G0tensor(:,91))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,152),wf(:,-1),G0tensor(:,92))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,-1),wf(:,152),G0tensor(:,93))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,145),Q(:,23),G1tensor(:,13))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,146),Q(:,23),G1tensor(:,14))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,147),Q(:,23),G1tensor(:,15))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-1),wf(:,171),G0tensor(:,94))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,171),wf(:,-1),G0tensor(:,95))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,-1),wf(:,171),G0tensor(:,96))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,-1),wf(:,175),G0tensor(:,97))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,175),wf(:,-1),G0tensor(:,98))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,-1),wf(:,175),G0tensor(:,99))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,183),Q(:,23),G1tensor(:,16))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,184),Q(:,23),G1tensor(:,17))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,185),Q(:,23),G1tensor(:,18))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1009),Q(:,23),G1tensor(:,19))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1010),Q(:,23),G1tensor(:,20))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1011),Q(:,23),G1tensor(:,21))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,0),wf(:,206),G0tensor(:,100))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,206),wf(:,0),G0tensor(:,101))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,0),wf(:,206),G0tensor(:,102))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,199),Q(:,23),G1tensor(:,22))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,200),Q(:,23),G1tensor(:,23))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,201),Q(:,23),G1tensor(:,24))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,0),wf(:,225),G0tensor(:,103))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,225),wf(:,0),G0tensor(:,104))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,0),wf(:,225),G0tensor(:,105))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,0),wf(:,229),G0tensor(:,106))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,47),wf(:,229),wf(:,0),G0tensor(:,107))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,47),wf(:,0),wf(:,229),G0tensor(:,108))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,237),Q(:,23),G1tensor(:,25))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,238),Q(:,23),G1tensor(:,26))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,239),Q(:,23),G1tensor(:,27))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1114),Q(:,23),G1tensor(:,28))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1115),Q(:,23),G1tensor(:,29))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1116),Q(:,23),G1tensor(:,30))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,255),Q(:,23),G1tensor(:,31))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,256),Q(:,23),G1tensor(:,32))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,257),Q(:,23),G1tensor(:,33))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1150),Q(:,23),G1tensor(:,34))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1151),Q(:,23),G1tensor(:,35))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1152),Q(:,23),G1tensor(:,36))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1165),Q(:,23),G1tensor(:,37))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1166),Q(:,23),G1tensor(:,38))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1167),Q(:,23),G1tensor(:,39))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,272),Q(:,23),G1tensor(:,40))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1322),Q(:,23),G1tensor(:,41))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1327),Q(:,23),G1tensor(:,42))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,275),Q(:,23),G1tensor(:,43))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1334),Q(:,23),G1tensor(:,44))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1339),Q(:,23),G1tensor(:,45))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,278),Q(:,23),G1tensor(:,46))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1346),Q(:,23),G1tensor(:,47))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1349),Q(:,23),G1tensor(:,48))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1393),Q(:,23),G1tensor(:,49))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1396),Q(:,23),G1tensor(:,50))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1405),Q(:,23),G1tensor(:,51))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1408),Q(:,23),G1tensor(:,52))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1417),Q(:,23),G1tensor(:,53))
  call check_last_UV_W(l_switch,G0(:,:,:,47),Q(:,40),wf(:,1418),Q(:,23),G1tensor(:,54))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,-5),G0(:,:,:,48))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-4),wf(:,1),G0tensor(:,109))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-4),wf(:,3),G0tensor(:,110))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-4),wf(:,4),G0tensor(:,111))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,1),wf(:,-4),G0tensor(:,112))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,3),wf(:,-4),G0tensor(:,113))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,4),wf(:,-4),G0tensor(:,114))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,-4),wf(:,1),G0tensor(:,115))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,-4),wf(:,3),G0tensor(:,116))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,-4),wf(:,4),G0tensor(:,117))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-2),wf(:,13),G0tensor(:,118))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-2),wf(:,15),G0tensor(:,119))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-2),wf(:,16),G0tensor(:,120))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,13),wf(:,-2),G0tensor(:,121))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,15),wf(:,-2),G0tensor(:,122))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,16),wf(:,-2),G0tensor(:,123))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,-2),wf(:,13),G0tensor(:,124))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,-2),wf(:,15),G0tensor(:,125))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,-2),wf(:,16),G0tensor(:,126))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-1),wf(:,31),G0tensor(:,127))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-1),wf(:,33),G0tensor(:,128))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-1),wf(:,34),G0tensor(:,129))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,31),wf(:,-1),G0tensor(:,130))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,33),wf(:,-1),G0tensor(:,131))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,34),wf(:,-1),G0tensor(:,132))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,-1),wf(:,31),G0tensor(:,133))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,-1),wf(:,33),G0tensor(:,134))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,-1),wf(:,34),G0tensor(:,135))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,0),wf(:,50),G0tensor(:,136))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,50),wf(:,0),G0tensor(:,137))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,0),wf(:,50),G0tensor(:,138))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,0),wf(:,53),G0tensor(:,139))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,53),wf(:,0),G0tensor(:,140))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,0),wf(:,53),G0tensor(:,141))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,0),wf(:,54),G0tensor(:,142))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,54),wf(:,0),G0tensor(:,143))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,0),wf(:,54),G0tensor(:,144))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,61),wf(:,66),G0tensor(:,145))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,66),wf(:,61),G0tensor(:,146))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,61),wf(:,66),G0tensor(:,147))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-4),wf(:,74),G0tensor(:,148))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,74),wf(:,-4),G0tensor(:,149))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,-4),wf(:,74),G0tensor(:,150))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-2),wf(:,88),G0tensor(:,151))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,88),wf(:,-2),G0tensor(:,152))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,-2),wf(:,88),G0tensor(:,153))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,80),Q(:,23),G1tensor(:,55))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,81),Q(:,23),G1tensor(:,56))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,82),Q(:,23),G1tensor(:,57))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,90),wf(:,95),G0tensor(:,154))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,95),wf(:,90),G0tensor(:,155))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,90),wf(:,95),G0tensor(:,156))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-4),wf(:,103),G0tensor(:,157))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,103),wf(:,-4),G0tensor(:,158))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,-4),wf(:,103),G0tensor(:,159))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,105),wf(:,109),G0tensor(:,160))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,109),wf(:,105),G0tensor(:,161))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,105),wf(:,109),G0tensor(:,162))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-4),wf(:,117),G0tensor(:,163))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,117),wf(:,-4),G0tensor(:,164))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,-4),wf(:,117),G0tensor(:,165))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-2),wf(:,135),G0tensor(:,166))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,135),wf(:,-2),G0tensor(:,167))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,-2),wf(:,135),G0tensor(:,168))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-2),wf(:,139),G0tensor(:,169))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,139),wf(:,-2),G0tensor(:,170))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,-2),wf(:,139),G0tensor(:,171))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-1),wf(:,152),G0tensor(:,172))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,152),wf(:,-1),G0tensor(:,173))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,-1),wf(:,152),G0tensor(:,174))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,145),Q(:,23),G1tensor(:,58))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,146),Q(:,23),G1tensor(:,59))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,147),Q(:,23),G1tensor(:,60))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-1),wf(:,171),G0tensor(:,175))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,171),wf(:,-1),G0tensor(:,176))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,-1),wf(:,171),G0tensor(:,177))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,-1),wf(:,175),G0tensor(:,178))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,175),wf(:,-1),G0tensor(:,179))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,-1),wf(:,175),G0tensor(:,180))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,183),Q(:,23),G1tensor(:,61))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,184),Q(:,23),G1tensor(:,62))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,185),Q(:,23),G1tensor(:,63))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1009),Q(:,23),G1tensor(:,64))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1010),Q(:,23),G1tensor(:,65))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1011),Q(:,23),G1tensor(:,66))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,0),wf(:,206),G0tensor(:,181))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,206),wf(:,0),G0tensor(:,182))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,0),wf(:,206),G0tensor(:,183))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,199),Q(:,23),G1tensor(:,67))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,200),Q(:,23),G1tensor(:,68))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,201),Q(:,23),G1tensor(:,69))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,0),wf(:,225),G0tensor(:,184))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,225),wf(:,0),G0tensor(:,185))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,0),wf(:,225),G0tensor(:,186))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,0),wf(:,229),G0tensor(:,187))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,48),wf(:,229),wf(:,0),G0tensor(:,188))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,48),wf(:,0),wf(:,229),G0tensor(:,189))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,237),Q(:,23),G1tensor(:,70))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,238),Q(:,23),G1tensor(:,71))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,239),Q(:,23),G1tensor(:,72))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1114),Q(:,23),G1tensor(:,73))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1115),Q(:,23),G1tensor(:,74))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1116),Q(:,23),G1tensor(:,75))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,255),Q(:,23),G1tensor(:,76))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,256),Q(:,23),G1tensor(:,77))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,257),Q(:,23),G1tensor(:,78))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1150),Q(:,23),G1tensor(:,79))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1151),Q(:,23),G1tensor(:,80))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1152),Q(:,23),G1tensor(:,81))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1165),Q(:,23),G1tensor(:,82))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1166),Q(:,23),G1tensor(:,83))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1167),Q(:,23),G1tensor(:,84))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,272),Q(:,23),G1tensor(:,85))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1322),Q(:,23),G1tensor(:,86))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1327),Q(:,23),G1tensor(:,87))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,275),Q(:,23),G1tensor(:,88))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1334),Q(:,23),G1tensor(:,89))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1339),Q(:,23),G1tensor(:,90))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,278),Q(:,23),G1tensor(:,91))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1346),Q(:,23),G1tensor(:,92))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1349),Q(:,23),G1tensor(:,93))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1393),Q(:,23),G1tensor(:,94))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1396),Q(:,23),G1tensor(:,95))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1405),Q(:,23),G1tensor(:,96))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1408),Q(:,23),G1tensor(:,97))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1417),Q(:,23),G1tensor(:,98))
  call check_last_UV_W(l_switch,G0(:,:,:,48),Q(:,40),wf(:,1418),Q(:,23),G1tensor(:,99))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(6)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(1260)
  T3sum(1:35,60) = T3sum(1:35,60) + Gcoeff * G3tensor(:,1)
  Gcoeff = (c(6)*(M(133)-M(137)+M(139)-M(143)-M(169)+M(187)-M(193)+M(211)-M(228)+M(229)-M(230)+M(231)+M(238)-M(242)+M(244) &
    -M(248))) * den(1260)
  T3sum(1:35,60) = T3sum(1:35,60) + Gcoeff * G3tensor(:,2)
  Gcoeff = (c(6)*(-M(133)+M(137)-M(139)+M(143)+M(169)-M(187)+M(193)-M(211)+M(228)-M(229)+M(230)-M(231)-M(238)+M(242)-M(244) &
    +M(248))) * den(1260)
  T3sum(1:35,60) = T3sum(1:35,60) + Gcoeff * G3tensor(:,3)
  Gcoeff = (c(9)*(M(135)-M(136)-M(173)+M(174)-M(191)+M(194)+M(215)-M(218))) * den(1264)
  T3sum(1:35,87) = T3sum(1:35,87) + Gcoeff * G3tensor(:,4)
  Gcoeff = (c(8)*(M(135)-M(136)-M(173)+M(174)-M(191)+M(194)+M(215)-M(218))) * den(1264)
  T3sum(1:35,95) = T3sum(1:35,95) + Gcoeff * G3tensor(:,5)
  Gcoeff = (c(9)*(M(149)-M(150)-M(192)+M(196)+M(216)-M(220)-M(233)+M(234))) * den(1264)
  T3sum(1:35,87) = T3sum(1:35,87) + Gcoeff * G3tensor(:,6)
  Gcoeff = (c(8)*(M(149)-M(150)-M(192)+M(196)+M(216)-M(220)-M(233)+M(234))) * den(1264)
  T3sum(1:35,95) = T3sum(1:35,95) + Gcoeff * G3tensor(:,7)
  Gcoeff = (c(9)*(M(135)-M(136)-M(173)+M(174)-M(191)+M(194)+M(215)-M(218))) * den(1264)
  T3sum(1:35,87) = T3sum(1:35,87) + Gcoeff * G3tensor(:,4)
  Gcoeff = (c(8)*(M(135)-M(136)-M(173)+M(174)-M(191)+M(194)+M(215)-M(218))) * den(1264)
  T3sum(1:35,96) = T3sum(1:35,96) + Gcoeff * G3tensor(:,8)
  Gcoeff = (c(9)*(M(149)-M(150)-M(192)+M(196)+M(216)-M(220)-M(233)+M(234))) * den(1264)
  T3sum(1:35,87) = T3sum(1:35,87) + Gcoeff * G3tensor(:,6)
  Gcoeff = (c(8)*(M(149)-M(150)-M(192)+M(196)+M(216)-M(220)-M(233)+M(234))) * den(1264)
  T3sum(1:35,96) = T3sum(1:35,96) + Gcoeff * G3tensor(:,9)
  Gcoeff = (c(6)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(1264)
  T3sum(1:35,87) = T3sum(1:35,87) + Gcoeff * G3tensor(:,10)
  Gcoeff = (c(6)*(M(135)-M(136)-M(149)+M(150)-M(173)+M(174)-M(191)+M(192)+M(194)-M(196)+M(215)-M(216)-M(218)+M(220)+M(233) &
    -M(234))) * den(1264)
  T3sum(1:35,87) = T3sum(1:35,87) + Gcoeff * G3tensor(:,11)
  Gcoeff = (c(6)*(-M(135)+M(136)+M(149)-M(150)+M(173)-M(174)+M(191)-M(192)-M(194)+M(196)-M(215)+M(216)+M(218)-M(220)-M(233) &
    +M(234))) * den(1264)
  T3sum(1:35,87) = T3sum(1:35,87) + Gcoeff * G3tensor(:,12)
  Gcoeff = (c(9)*(M(139)-M(145)-M(173)+M(174)+M(176)-M(178)-M(191)+M(215))) * den(1265)
  T3sum(1:35,87) = T3sum(1:35,87) + Gcoeff * G3tensor(:,13)
  Gcoeff = (c(8)*(M(139)-M(145)-M(173)+M(174)+M(176)-M(178)-M(191)+M(215))) * den(1265)
  T3sum(1:35,95) = T3sum(1:35,95) + Gcoeff * G3tensor(:,14)
  Gcoeff = (c(9)*(M(140)-M(146)-M(192)+M(216)-M(233)+M(234)+M(236)-M(238))) * den(1265)
  T3sum(1:35,87) = T3sum(1:35,87) + Gcoeff * G3tensor(:,15)
  Gcoeff = (c(8)*(M(140)-M(146)-M(192)+M(216)-M(233)+M(234)+M(236)-M(238))) * den(1265)
  T3sum(1:35,95) = T3sum(1:35,95) + Gcoeff * G3tensor(:,16)
  Gcoeff = (c(9)*(M(139)-M(145)-M(173)+M(174)+M(176)-M(178)-M(191)+M(215))) * den(1265)
  T3sum(1:35,87) = T3sum(1:35,87) + Gcoeff * G3tensor(:,13)
  Gcoeff = (c(8)*(M(139)-M(145)-M(173)+M(174)+M(176)-M(178)-M(191)+M(215))) * den(1265)
  T3sum(1:35,96) = T3sum(1:35,96) + Gcoeff * G3tensor(:,17)
  Gcoeff = (c(9)*(M(140)-M(146)-M(192)+M(216)-M(233)+M(234)+M(236)-M(238))) * den(1265)
  T3sum(1:35,87) = T3sum(1:35,87) + Gcoeff * G3tensor(:,15)
  Gcoeff = (c(8)*(M(140)-M(146)-M(192)+M(216)-M(233)+M(234)+M(236)-M(238))) * den(1265)
  T3sum(1:35,96) = T3sum(1:35,96) + Gcoeff * G3tensor(:,18)
  Gcoeff = (c(6)*(-M(139)+M(140)+M(145)-M(146)+M(173)-M(174)-M(176)+M(178)+M(191)-M(192)-M(215)+M(216)-M(233)+M(234)+M(236) &
    -M(238))) * den(1265)
  T3sum(1:35,87) = T3sum(1:35,87) + Gcoeff * G3tensor(:,19)
  Gcoeff = (c(6)*(M(139)-M(140)-M(145)+M(146)-M(173)+M(174)+M(176)-M(178)-M(191)+M(192)+M(215)-M(216)+M(233)-M(234)-M(236) &
    +M(238))) * den(1265)
  T3sum(1:35,87) = T3sum(1:35,87) + Gcoeff * G3tensor(:,20)
  Gcoeff = (c(6)*(-M(139)+M(140)+M(145)-M(146)+M(173)-M(174)-M(176)+M(178)+M(191)-M(192)-M(215)+M(216)-M(233)+M(234)+M(236) &
    -M(238))) * den(1265)
  T3sum(1:35,87) = T3sum(1:35,87) + Gcoeff * G3tensor(:,21)
  Gcoeff = (c(9)*(-M(149)+M(150)+M(159)-M(160)+M(192)-M(193)-M(216)+M(217))) * den(1266)
  T3sum(1:35,80) = T3sum(1:35,80) + Gcoeff * G3tensor(:,22)
  Gcoeff = (c(8)*(-M(149)+M(150)+M(159)-M(160)+M(192)-M(193)-M(216)+M(217))) * den(1266)
  T3sum(1:35,115) = T3sum(1:35,115) + Gcoeff * G3tensor(:,23)
  Gcoeff = (c(9)*(M(173)-M(174)-M(194)+M(195)+M(218)-M(219)-M(227)+M(228))) * den(1266)
  T3sum(1:35,80) = T3sum(1:35,80) + Gcoeff * G3tensor(:,24)
  Gcoeff = (c(8)*(M(173)-M(174)-M(194)+M(195)+M(218)-M(219)-M(227)+M(228))) * den(1266)
  T3sum(1:35,115) = T3sum(1:35,115) + Gcoeff * G3tensor(:,25)
  Gcoeff = (c(9)*(-M(149)+M(150)+M(159)-M(160)+M(192)-M(193)-M(216)+M(217))) * den(1266)
  T3sum(1:35,80) = T3sum(1:35,80) + Gcoeff * G3tensor(:,22)
  Gcoeff = (c(8)*(-M(149)+M(150)+M(159)-M(160)+M(192)-M(193)-M(216)+M(217))) * den(1266)
  T3sum(1:35,116) = T3sum(1:35,116) + Gcoeff * G3tensor(:,26)
  Gcoeff = (c(9)*(M(173)-M(174)-M(194)+M(195)+M(218)-M(219)-M(227)+M(228))) * den(1266)
  T3sum(1:35,80) = T3sum(1:35,80) + Gcoeff * G3tensor(:,24)
  Gcoeff = (c(8)*(M(173)-M(174)-M(194)+M(195)+M(218)-M(219)-M(227)+M(228))) * den(1266)
  T3sum(1:35,116) = T3sum(1:35,116) + Gcoeff * G3tensor(:,27)
  Gcoeff = (c(6)*(M(149)-M(150)-M(159)+M(160)+M(173)-M(174)-M(192)+M(193)-M(194)+M(195)+M(216)-M(217)+M(218)-M(219)-M(227) &
    +M(228))) * den(1266)
  T3sum(1:35,80) = T3sum(1:35,80) + Gcoeff * G3tensor(:,28)
  Gcoeff = (c(6)*(-M(149)+M(150)+M(159)-M(160)-M(173)+M(174)+M(192)-M(193)+M(194)-M(195)-M(216)+M(217)-M(218)+M(219)+M(227) &
    -M(228))) * den(1266)
  T3sum(1:35,80) = T3sum(1:35,80) + Gcoeff * G3tensor(:,29)
  Gcoeff = (c(6)*(M(149)-M(150)-M(159)+M(160)+M(173)-M(174)-M(192)+M(193)-M(194)+M(195)+M(216)-M(217)+M(218)-M(219)-M(227) &
    +M(228))) * den(1266)
  T3sum(1:35,80) = T3sum(1:35,80) + Gcoeff * G3tensor(:,30)
  Gcoeff = (c(9)*(-M(149)+M(150)+M(152)-M(154)+M(163)-M(169)-M(193)+M(217))) * den(1267)
  T3sum(1:35,80) = T3sum(1:35,80) + Gcoeff * G3tensor(:,31)
  Gcoeff = (c(8)*(-M(149)+M(150)+M(152)-M(154)+M(163)-M(169)-M(193)+M(217))) * den(1267)
  T3sum(1:35,115) = T3sum(1:35,115) + Gcoeff * G3tensor(:,32)
  Gcoeff = (c(9)*(M(164)-M(170)-M(194)+M(218)-M(227)+M(228)+M(230)-M(232))) * den(1267)
  T3sum(1:35,80) = T3sum(1:35,80) + Gcoeff * G3tensor(:,33)
  Gcoeff = (c(8)*(M(164)-M(170)-M(194)+M(218)-M(227)+M(228)+M(230)-M(232))) * den(1267)
  T3sum(1:35,115) = T3sum(1:35,115) + Gcoeff * G3tensor(:,34)
  Gcoeff = (c(9)*(-M(149)+M(150)+M(152)-M(154)+M(163)-M(169)-M(193)+M(217))) * den(1267)
  T3sum(1:35,80) = T3sum(1:35,80) + Gcoeff * G3tensor(:,31)
  Gcoeff = (c(8)*(-M(149)+M(150)+M(152)-M(154)+M(163)-M(169)-M(193)+M(217))) * den(1267)
  T3sum(1:35,116) = T3sum(1:35,116) + Gcoeff * G3tensor(:,35)
  Gcoeff = (c(9)*(M(164)-M(170)-M(194)+M(218)-M(227)+M(228)+M(230)-M(232))) * den(1267)
  T3sum(1:35,80) = T3sum(1:35,80) + Gcoeff * G3tensor(:,33)
  Gcoeff = (c(8)*(M(164)-M(170)-M(194)+M(218)-M(227)+M(228)+M(230)-M(232))) * den(1267)
  T3sum(1:35,116) = T3sum(1:35,116) + Gcoeff * G3tensor(:,36)
  Gcoeff = (c(6)*(M(149)-M(150)-M(152)+M(154)-M(163)+M(164)+M(169)-M(170)+M(193)-M(194)-M(217)+M(218)-M(227)+M(228)+M(230) &
    -M(232))) * den(1267)
  T3sum(1:35,80) = T3sum(1:35,80) + Gcoeff * G3tensor(:,37)
  Gcoeff = (c(6)*(-M(149)+M(150)+M(152)-M(154)+M(163)-M(164)-M(169)+M(170)-M(193)+M(194)+M(217)-M(218)+M(227)-M(228)-M(230) &
    +M(232))) * den(1267)
  T3sum(1:35,80) = T3sum(1:35,80) + Gcoeff * G3tensor(:,38)
  Gcoeff = (c(6)*(M(149)-M(150)-M(152)+M(154)-M(163)+M(164)+M(169)-M(170)+M(193)-M(194)-M(217)+M(218)-M(227)+M(228)+M(230) &
    -M(232))) * den(1267)
  T3sum(1:35,80) = T3sum(1:35,80) + Gcoeff * G3tensor(:,39)
  Gcoeff = (c(9)*(-M(230)+M(232)+M(233)-M(234)-M(236)+M(238)+M(243)-M(249))) * den(1270)
  T3sum(1:35,60) = T3sum(1:35,60) + Gcoeff * G3tensor(:,40)
  Gcoeff = (c(8)*(-M(230)+M(232)+M(233)-M(234)-M(236)+M(238)+M(243)-M(249))) * den(1270)
  T3sum(1:35,123) = T3sum(1:35,123) + Gcoeff * G3tensor(:,41)
  Gcoeff = (c(9)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215))) * den(1270)
  T3sum(1:35,60) = T3sum(1:35,60) + Gcoeff * G3tensor(:,42)
  Gcoeff = (c(8)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215))) * den(1270)
  T3sum(1:35,123) = T3sum(1:35,123) + Gcoeff * G3tensor(:,43)
  Gcoeff = (c(9)*(-M(230)+M(232)+M(233)-M(234)-M(236)+M(238)+M(243)-M(249))) * den(1270)
  T3sum(1:35,60) = T3sum(1:35,60) + Gcoeff * G3tensor(:,40)
  Gcoeff = (c(8)*(-M(230)+M(232)+M(233)-M(234)-M(236)+M(238)+M(243)-M(249))) * den(1270)
  T3sum(1:35,124) = T3sum(1:35,124) + Gcoeff * G3tensor(:,44)
  Gcoeff = (c(9)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215))) * den(1270)
  T3sum(1:35,60) = T3sum(1:35,60) + Gcoeff * G3tensor(:,42)
  Gcoeff = (c(8)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215))) * den(1270)
  T3sum(1:35,124) = T3sum(1:35,124) + Gcoeff * G3tensor(:,45)
  Gcoeff = (c(6)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(1270)
  T3sum(1:35,60) = T3sum(1:35,60) + Gcoeff * G3tensor(:,46)
  Gcoeff = (c(6)*(M(139)-M(145)-M(155)+M(157)+M(163)-M(169)-M(191)+M(215)-M(230)+M(232)+M(233)-M(234)-M(236)+M(238)+M(243) &
    -M(249))) * den(1270)
  T3sum(1:35,60) = T3sum(1:35,60) + Gcoeff * G3tensor(:,47)
  Gcoeff = (c(6)*(-M(139)+M(145)+M(155)-M(157)-M(163)+M(169)+M(191)-M(215)+M(230)-M(232)-M(233)+M(234)+M(236)-M(238)-M(243) &
    +M(249))) * den(1270)
  T3sum(1:35,60) = T3sum(1:35,60) + Gcoeff * G3tensor(:,48)
  Gcoeff = (c(9)*(M(227)-M(228)-M(230)+M(232)-M(236)+M(238)+M(244)-M(250))) * den(1271)
  T3sum(1:35,60) = T3sum(1:35,60) + Gcoeff * G3tensor(:,49)
  Gcoeff = (c(8)*(M(227)-M(228)-M(230)+M(232)-M(236)+M(238)+M(244)-M(250))) * den(1271)
  T3sum(1:35,123) = T3sum(1:35,123) + Gcoeff * G3tensor(:,50)
  Gcoeff = (c(9)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217))) * den(1271)
  T3sum(1:35,60) = T3sum(1:35,60) + Gcoeff * G3tensor(:,51)
  Gcoeff = (c(8)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217))) * den(1271)
  T3sum(1:35,123) = T3sum(1:35,123) + Gcoeff * G3tensor(:,52)
  Gcoeff = (c(9)*(M(227)-M(228)-M(230)+M(232)-M(236)+M(238)+M(244)-M(250))) * den(1271)
  T3sum(1:35,60) = T3sum(1:35,60) + Gcoeff * G3tensor(:,49)
  Gcoeff = (c(8)*(M(227)-M(228)-M(230)+M(232)-M(236)+M(238)+M(244)-M(250))) * den(1271)
  T3sum(1:35,124) = T3sum(1:35,124) + Gcoeff * G3tensor(:,53)
  Gcoeff = (c(9)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217))) * den(1271)
  T3sum(1:35,60) = T3sum(1:35,60) + Gcoeff * G3tensor(:,51)
  Gcoeff = (c(8)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217))) * den(1271)
  T3sum(1:35,124) = T3sum(1:35,124) + Gcoeff * G3tensor(:,54)
  Gcoeff = (c(6)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(1271)
  T3sum(1:35,60) = T3sum(1:35,60) + Gcoeff * G3tensor(:,55)
  Gcoeff = (c(6)*(-M(131)+M(133)+M(139)-M(145)+M(163)-M(169)-M(193)+M(217)+M(227)-M(228)-M(230)+M(232)-M(236)+M(238)+M(244) &
    -M(250))) * den(1271)
  T3sum(1:35,60) = T3sum(1:35,60) + Gcoeff * G3tensor(:,56)
  Gcoeff = (c(6)*(M(131)-M(133)-M(139)+M(145)-M(163)+M(169)+M(193)-M(217)-M(227)+M(228)+M(230)-M(232)+M(236)-M(238)-M(244) &
    +M(250))) * den(1271)
  T3sum(1:35,60) = T3sum(1:35,60) + Gcoeff * G3tensor(:,57)
  Gcoeff = (c(1)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(2)*(M(133)+M(135)-M(146)-M(152) &
    -M(170)-M(176)+M(220)+M(244))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,1)
  Gcoeff = (c(1)*(-M(41)-M(42)-M(43)+M(53)+M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92))+c(2)*(-M(133)-M(135)+M(157)+M(159) &
    +M(219)-M(220)+M(243)-M(244))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,4)
  Gcoeff = (c(1)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(2)*(M(146)+M(152)-M(157)-M(159) &
    +M(170)+M(176)-M(219)-M(243))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,7)
  Gcoeff = (c(1)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(2)*(M(131)+M(136)-M(140)-M(154) &
    -M(164)-M(178)+M(196)+M(250))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,2)
  Gcoeff = (c(1)*(-M(41)-M(42)-M(43)+M(53)+M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92))+c(2)*(-M(131)-M(136)+M(155)+M(160) &
    +M(195)-M(196)+M(249)-M(250))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,5)
  Gcoeff = (c(1)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(2)*(M(140)+M(154)-M(155)-M(160) &
    +M(164)+M(178)-M(195)-M(249))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,8)
  Gcoeff = (c(2)*(M(131)-M(133)-M(135)+M(136)-M(140)+M(146)+M(152)-M(154)-M(164)+M(170)+M(176)-M(178)+M(196)-M(220)-M(244) &
    +M(250))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,3)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(135)-M(136)+M(155)-M(157)-M(159)+M(160)+M(195)-M(196)-M(219)+M(220)-M(243)+M(244)+M(249) &
    -M(250))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,6)
  Gcoeff = (c(2)*(M(140)-M(146)-M(152)+M(154)-M(155)+M(157)+M(159)-M(160)+M(164)-M(170)-M(176)+M(178)-M(195)+M(219)+M(243) &
    -M(249))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,9)
  Gcoeff = (c(1)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(2)*(M(131)+M(136)-M(140)-M(154) &
    -M(164)-M(178)+M(196)+M(250))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,10)
  Gcoeff = (c(1)*(-M(41)-M(42)-M(43)+M(53)+M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92))+c(2)*(-M(131)-M(136)+M(155)+M(160) &
    +M(195)-M(196)+M(249)-M(250))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,13)
  Gcoeff = (c(1)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(2)*(M(140)+M(154)-M(155)-M(160) &
    +M(164)+M(178)-M(195)-M(249))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,16)
  Gcoeff = (c(1)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(2)*(M(133)+M(135)-M(146)-M(152) &
    -M(170)-M(176)+M(220)+M(244))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,11)
  Gcoeff = (c(1)*(-M(41)-M(42)-M(43)+M(53)+M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92))+c(2)*(-M(133)-M(135)+M(157)+M(159) &
    +M(219)-M(220)+M(243)-M(244))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,14)
  Gcoeff = (c(1)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(2)*(M(146)+M(152)-M(157)-M(159) &
    +M(170)+M(176)-M(219)-M(243))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,17)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(135)-M(136)+M(140)-M(146)-M(152)+M(154)+M(164)-M(170)-M(176)+M(178)-M(196)+M(220)+M(244) &
    -M(250))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,12)
  Gcoeff = (c(2)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,15)
  Gcoeff = (c(2)*(-M(140)+M(146)+M(152)-M(154)+M(155)-M(157)-M(159)+M(160)-M(164)+M(170)+M(176)-M(178)+M(195)-M(219)-M(243) &
    +M(249))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,18)
  Gcoeff = (c(2)*(M(131)-M(133)-M(135)+M(136)-M(140)+M(146)+M(152)-M(154)-M(164)+M(170)+M(176)-M(178)+M(196)-M(220)-M(244) &
    +M(250))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,19)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(135)-M(136)+M(155)-M(157)-M(159)+M(160)+M(195)-M(196)-M(219)+M(220)-M(243)+M(244)+M(249) &
    -M(250))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,22)
  Gcoeff = (c(2)*(M(140)-M(146)-M(152)+M(154)-M(155)+M(157)+M(159)-M(160)+M(164)-M(170)-M(176)+M(178)-M(195)+M(219)+M(243) &
    -M(249))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,25)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(135)-M(136)+M(140)-M(146)-M(152)+M(154)+M(164)-M(170)-M(176)+M(178)-M(196)+M(220)+M(244) &
    -M(250))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,20)
  Gcoeff = (c(2)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,23)
  Gcoeff = (c(2)*(-M(140)+M(146)+M(152)-M(154)+M(155)-M(157)-M(159)+M(160)-M(164)+M(170)+M(176)-M(178)+M(195)-M(219)-M(243) &
    +M(249))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,26)
  Gcoeff = (c(3)*(-M(131)+M(133)+M(135)-M(136)+M(140)-M(146)-M(152)+M(154)+M(164)-M(170)-M(176)+M(178)-M(196)+M(220)+M(244) &
    -M(250))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,21)
  Gcoeff = (c(3)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,24)
  Gcoeff = (c(3)*(-M(140)+M(146)+M(152)-M(154)+M(155)-M(157)-M(159)+M(160)-M(164)+M(170)+M(176)-M(178)+M(195)-M(219)-M(243) &
    +M(249))) * den(1)
  T2sum(1:1,1) = T2sum(1:1,1) + Gcoeff * G0tensor(:,27)
  Gcoeff = (c(1)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(2)*(M(133)+M(135)-M(146)-M(152) &
    -M(170)-M(176)+M(220)+M(244))) * den(1)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,28)
  Gcoeff = (c(1)*(-M(41)-M(42)-M(43)+M(53)+M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92))+c(2)*(-M(133)-M(135)+M(157)+M(159) &
    +M(219)-M(220)+M(243)-M(244))) * den(1)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,29)
  Gcoeff = (c(1)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(2)*(M(146)+M(152)-M(157)-M(159) &
    +M(170)+M(176)-M(219)-M(243))) * den(1)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,30)
  Gcoeff = (c(1)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(2)*(M(132)+M(134)-M(142)-M(148) &
    -M(166)-M(172)+M(202)+M(226))) * den(1)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,109)
  Gcoeff = (c(1)*(-M(41)-M(42)-M(43)+M(53)+M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92))+c(2)*(-M(132)-M(134)+M(156)+M(158) &
    +M(201)-M(202)+M(225)-M(226))) * den(1)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,110)
  Gcoeff = (c(1)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(2)*(M(142)+M(148)-M(156)-M(158) &
    +M(166)+M(172)-M(201)-M(225))) * den(1)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,111)
  Gcoeff = (c(1)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(2)*(M(132)+M(134)-M(142)-M(148) &
    -M(166)-M(172)+M(202)+M(226))) * den(1)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,31)
  Gcoeff = (c(1)*(-M(41)-M(42)-M(43)+M(53)+M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92))+c(2)*(-M(132)-M(134)+M(156)+M(158) &
    +M(201)-M(202)+M(225)-M(226))) * den(1)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,32)
  Gcoeff = (c(1)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(2)*(M(142)+M(148)-M(156)-M(158) &
    +M(166)+M(172)-M(201)-M(225))) * den(1)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,33)
  Gcoeff = (c(1)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(2)*(M(133)+M(135)-M(146)-M(152) &
    -M(170)-M(176)+M(220)+M(244))) * den(1)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,112)
  Gcoeff = (c(1)*(-M(41)-M(42)-M(43)+M(53)+M(54)+M(55)+M(65)-M(68)+M(77)-M(80)+M(89)-M(92))+c(2)*(-M(133)-M(135)+M(157)+M(159) &
    +M(219)-M(220)+M(243)-M(244))) * den(1)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,113)
  Gcoeff = (c(1)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(2)*(M(146)+M(152)-M(157)-M(159) &
    +M(170)+M(176)-M(219)-M(243))) * den(1)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,114)
  Gcoeff = (c(2)*(M(132)-M(133)+M(134)-M(135)-M(142)+M(146)-M(148)+M(152)-M(166)+M(170)-M(172)+M(176)+M(202)-M(220)+M(226) &
    -M(244))) * den(1)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,34)
  Gcoeff = (c(2)*(-M(132)+M(133)-M(134)+M(135)+M(156)-M(157)+M(158)-M(159)+M(201)-M(202)-M(219)+M(220)+M(225)-M(226)-M(243) &
    +M(244))) * den(1)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,35)
  Gcoeff = (c(2)*(M(142)-M(146)+M(148)-M(152)-M(156)+M(157)-M(158)+M(159)+M(166)-M(170)+M(172)-M(176)-M(201)+M(219)-M(225) &
    +M(243))) * den(1)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,36)
  Gcoeff = (c(2)*(-M(132)+M(133)-M(134)+M(135)+M(142)-M(146)+M(148)-M(152)+M(166)-M(170)+M(172)-M(176)-M(202)+M(220)-M(226) &
    +M(244))) * den(1)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,115)
  Gcoeff = (c(2)*(M(132)-M(133)+M(134)-M(135)-M(156)+M(157)-M(158)+M(159)-M(201)+M(202)+M(219)-M(220)-M(225)+M(226)+M(243) &
    -M(244))) * den(1)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,116)
  Gcoeff = (c(2)*(-M(142)+M(146)-M(148)+M(152)+M(156)-M(157)+M(158)-M(159)-M(166)+M(170)-M(172)+M(176)+M(201)-M(219)+M(225) &
    -M(243))) * den(1)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,117)
  Gcoeff = (c(1)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(2)*(-M(132)+M(143)+M(148)-M(151) &
    +M(166)-M(212)-M(226)+M(242))) * den(3)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,37)
  Gcoeff = (c(1)*(-M(47)-M(48)-M(49)+M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98))+c(2)*(-M(143)-M(148)+M(165)-M(166) &
    +M(203)+M(208)+M(241)-M(242))) * den(3)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,38)
  Gcoeff = (c(1)*(M(42)+M(45)+M(52)-M(54)-M(66)-M(77)-M(78)-M(79)+M(80)+M(83)+M(86)-M(91))+c(2)*(M(132)+M(151)-M(165)-M(203) &
    -M(208)+M(212)+M(226)-M(241))) * den(3)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,39)
  Gcoeff = (c(1)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(2)*(-M(135)-M(141)+M(144)+M(146) &
    +M(176)+M(200)-M(214)-M(220))) * den(3)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,40)
  Gcoeff = (c(1)*(-M(47)-M(48)-M(49)+M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98))+c(2)*(-M(144)-M(146)+M(175)-M(176) &
    +M(199)-M(200)+M(204)+M(206))) * den(3)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,41)
  Gcoeff = (c(1)*(M(42)+M(45)+M(52)-M(54)-M(66)-M(77)-M(78)-M(79)+M(80)+M(83)+M(86)-M(91))+c(2)*(M(135)+M(141)-M(175)-M(199) &
    -M(204)-M(206)+M(214)+M(220))) * den(3)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,42)
  Gcoeff = (c(2)*(M(132)-M(135)-M(141)-M(143)+M(144)+M(146)-M(148)+M(151)-M(166)+M(176)+M(200)+M(212)-M(214)-M(220)+M(226) &
    -M(242))) * den(3)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,43)
  Gcoeff = (c(2)*(M(143)-M(144)-M(146)+M(148)-M(165)+M(166)+M(175)-M(176)+M(199)-M(200)-M(203)+M(204)+M(206)-M(208)-M(241) &
    +M(242))) * den(3)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,44)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(141)-M(151)+M(165)-M(175)-M(199)+M(203)-M(204)-M(206)+M(208)-M(212)+M(214)+M(220)-M(226) &
    +M(241))) * den(3)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,45)
  Gcoeff = (c(1)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(2)*(-M(135)-M(141)+M(144)+M(146) &
    +M(176)+M(200)-M(214)-M(220))) * den(3)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,118)
  Gcoeff = (c(1)*(-M(47)-M(48)-M(49)+M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98))+c(2)*(-M(144)-M(146)+M(175)-M(176) &
    +M(199)-M(200)+M(204)+M(206))) * den(3)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,119)
  Gcoeff = (c(1)*(M(42)+M(45)+M(52)-M(54)-M(66)-M(77)-M(78)-M(79)+M(80)+M(83)+M(86)-M(91))+c(2)*(M(135)+M(141)-M(175)-M(199) &
    -M(204)-M(206)+M(214)+M(220))) * den(3)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,120)
  Gcoeff = (c(1)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(2)*(-M(132)+M(143)+M(148)-M(151) &
    +M(166)-M(212)-M(226)+M(242))) * den(3)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,121)
  Gcoeff = (c(1)*(-M(47)-M(48)-M(49)+M(54)-M(59)+M(66)-M(71)+M(77)+M(78)+M(79)+M(91)-M(98))+c(2)*(-M(143)-M(148)+M(165)-M(166) &
    +M(203)+M(208)+M(241)-M(242))) * den(3)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,122)
  Gcoeff = (c(1)*(M(42)+M(45)+M(52)-M(54)-M(66)-M(77)-M(78)-M(79)+M(80)+M(83)+M(86)-M(91))+c(2)*(M(132)+M(151)-M(165)-M(203) &
    -M(208)+M(212)+M(226)-M(241))) * den(3)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,123)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(141)+M(143)-M(144)-M(146)+M(148)-M(151)+M(166)-M(176)-M(200)-M(212)+M(214)+M(220)-M(226) &
    +M(242))) * den(3)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,124)
  Gcoeff = (c(2)*(-M(143)+M(144)+M(146)-M(148)+M(165)-M(166)-M(175)+M(176)-M(199)+M(200)+M(203)-M(204)-M(206)+M(208)+M(241) &
    -M(242))) * den(3)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,125)
  Gcoeff = (c(2)*(M(132)-M(135)-M(141)+M(151)-M(165)+M(175)+M(199)-M(203)+M(204)+M(206)-M(208)+M(212)-M(214)-M(220)+M(226) &
    -M(241))) * den(3)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,126)
  Gcoeff = (c(1)*(M(47)-M(54)-M(57)+M(59)+M(60)+M(61)-M(64)+M(72)-M(77)-M(84)-M(87)+M(99))+c(2)*(M(142)-M(156)+M(167)+M(172) &
    -M(175)-M(206)-M(225)+M(240))) * den(6)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,46)
  Gcoeff = (c(1)*(M(42)-M(47)-M(59)-M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99))+c(2)*(M(141)-M(142)-M(167)-M(172) &
    +M(209)+M(214)+M(239)-M(240))) * den(6)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,47)
  Gcoeff = (c(1)*(-M(42)+M(54)+M(57)+M(64)-M(69)+M(77)-M(80)-M(81)-M(82)+M(84)+M(87)-M(94))+c(2)*(-M(141)+M(156)+M(175)+M(206) &
    -M(209)-M(214)+M(225)-M(239))) * den(6)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,48)
  Gcoeff = (c(1)*(M(47)-M(54)-M(57)+M(59)+M(60)+M(61)-M(64)+M(72)-M(77)-M(84)-M(87)+M(99))+c(2)*(M(152)-M(159)-M(165)+M(168) &
    +M(170)+M(198)-M(208)-M(219))) * den(6)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,49)
  Gcoeff = (c(1)*(M(42)-M(47)-M(59)-M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99))+c(2)*(M(151)-M(152)-M(168)-M(170) &
    +M(197)-M(198)+M(210)+M(212))) * den(6)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,50)
  Gcoeff = (c(1)*(-M(42)+M(54)+M(57)+M(64)-M(69)+M(77)-M(80)-M(81)-M(82)+M(84)+M(87)-M(94))+c(2)*(-M(151)+M(159)+M(165)-M(197) &
    +M(208)-M(210)-M(212)+M(219))) * den(6)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,51)
  Gcoeff = (c(2)*(-M(142)+M(152)+M(156)-M(159)-M(165)-M(167)+M(168)+M(170)-M(172)+M(175)+M(198)+M(206)-M(208)-M(219)+M(225) &
    -M(240))) * den(6)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,52)
  Gcoeff = (c(2)*(-M(141)+M(142)+M(151)-M(152)+M(167)-M(168)-M(170)+M(172)+M(197)-M(198)-M(209)+M(210)+M(212)-M(214)-M(239) &
    +M(240))) * den(6)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,53)
  Gcoeff = (c(2)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(6)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,54)
  Gcoeff = (c(1)*(M(47)-M(54)-M(57)+M(59)+M(60)+M(61)-M(64)+M(72)-M(77)-M(84)-M(87)+M(99))+c(2)*(M(152)-M(159)-M(165)+M(168) &
    +M(170)+M(198)-M(208)-M(219))) * den(6)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,127)
  Gcoeff = (c(1)*(M(42)-M(47)-M(59)-M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99))+c(2)*(M(151)-M(152)-M(168)-M(170) &
    +M(197)-M(198)+M(210)+M(212))) * den(6)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,128)
  Gcoeff = (c(1)*(-M(42)+M(54)+M(57)+M(64)-M(69)+M(77)-M(80)-M(81)-M(82)+M(84)+M(87)-M(94))+c(2)*(-M(151)+M(159)+M(165)-M(197) &
    +M(208)-M(210)-M(212)+M(219))) * den(6)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,129)
  Gcoeff = (c(1)*(M(47)-M(54)-M(57)+M(59)+M(60)+M(61)-M(64)+M(72)-M(77)-M(84)-M(87)+M(99))+c(2)*(M(142)-M(156)+M(167)+M(172) &
    -M(175)-M(206)-M(225)+M(240))) * den(6)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,130)
  Gcoeff = (c(1)*(M(42)-M(47)-M(59)-M(60)-M(61)+M(69)-M(72)+M(80)+M(81)+M(82)+M(94)-M(99))+c(2)*(M(141)-M(142)-M(167)-M(172) &
    +M(209)+M(214)+M(239)-M(240))) * den(6)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,131)
  Gcoeff = (c(1)*(-M(42)+M(54)+M(57)+M(64)-M(69)+M(77)-M(80)-M(81)-M(82)+M(84)+M(87)-M(94))+c(2)*(-M(141)+M(156)+M(175)+M(206) &
    -M(209)-M(214)+M(225)-M(239))) * den(6)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,132)
  Gcoeff = (c(2)*(M(142)-M(152)-M(156)+M(159)+M(165)+M(167)-M(168)-M(170)+M(172)-M(175)-M(198)-M(206)+M(208)+M(219)-M(225) &
    +M(240))) * den(6)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,133)
  Gcoeff = (c(2)*(M(141)-M(142)-M(151)+M(152)-M(167)+M(168)+M(170)-M(172)-M(197)+M(198)+M(209)-M(210)-M(212)+M(214)+M(239) &
    -M(240))) * den(6)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,134)
  Gcoeff = (c(2)*(-M(141)+M(151)+M(156)-M(159)-M(165)+M(175)+M(197)+M(206)-M(208)-M(209)+M(210)+M(212)-M(214)-M(219)+M(225) &
    -M(239))) * den(6)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,135)
  Gcoeff = (c(1)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(2)*(M(134)-M(158)+M(197) &
    -M(199)-M(201)+M(202)-M(204)+M(210))) * den(84)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,55)
  Gcoeff = (c(1)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(2)*(M(133)-M(157)-M(203) &
    +M(209)+M(239)-M(241)-M(243)+M(244))) * den(84)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,56)
  Gcoeff = (c(2)*(M(133)-M(134)-M(157)+M(158)-M(197)+M(199)+M(201)-M(202)-M(203)+M(204)+M(209)-M(210)+M(239)-M(241)-M(243) &
    +M(244))) * den(84)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,57)
  Gcoeff = (c(1)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(2)*(-M(134)+M(144)+M(168) &
    -M(197)+M(198)+M(200)-M(202)-M(210))) * den(84)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,58)
  Gcoeff = (c(1)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(2)*(-M(133)+M(143)+M(167) &
    -M(209)-M(239)+M(240)+M(242)-M(244))) * den(84)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,59)
  Gcoeff = (c(2)*(-M(133)+M(134)+M(143)-M(144)+M(167)-M(168)+M(197)-M(198)-M(200)+M(202)-M(209)+M(210)-M(239)+M(240)+M(242) &
    -M(244))) * den(84)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,60)
  Gcoeff = (c(1)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(2)*(-M(144)+M(158)-M(168) &
    -M(198)+M(199)-M(200)+M(201)+M(204))) * den(84)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,61)
  Gcoeff = (c(1)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(2)*(-M(143)+M(157)-M(167) &
    +M(203)-M(240)+M(241)-M(242)+M(243))) * den(84)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,62)
  Gcoeff = (c(2)*(-M(143)+M(144)+M(157)-M(158)-M(167)+M(168)+M(198)-M(199)+M(200)-M(201)+M(203)-M(204)-M(240)+M(241)-M(242) &
    +M(243))) * den(84)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,63)
  Gcoeff = (c(1)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(2)*(M(133)-M(157)-M(203) &
    +M(209)+M(239)-M(241)-M(243)+M(244))) * den(84)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,136)
  Gcoeff = (c(1)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(2)*(M(134)-M(158)+M(197) &
    -M(199)-M(201)+M(202)-M(204)+M(210))) * den(84)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,137)
  Gcoeff = (c(2)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(84)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,138)
  Gcoeff = (c(1)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(2)*(-M(133)+M(143)+M(167) &
    -M(209)-M(239)+M(240)+M(242)-M(244))) * den(84)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,139)
  Gcoeff = (c(1)*(-M(42)+M(47)+M(59)-M(80)-M(103)-M(104)+M(107)-M(109)+M(113)+M(114)+M(120)-M(123))+c(2)*(-M(134)+M(144)+M(168) &
    -M(197)+M(198)+M(200)-M(202)-M(210))) * den(84)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,140)
  Gcoeff = (c(2)*(M(133)-M(134)-M(143)+M(144)-M(167)+M(168)-M(197)+M(198)+M(200)-M(202)+M(209)-M(210)+M(239)-M(240)-M(242) &
    +M(244))) * den(84)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,141)
  Gcoeff = (c(1)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(2)*(-M(143)+M(157)-M(167) &
    +M(203)-M(240)+M(241)-M(242)+M(243))) * den(84)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,142)
  Gcoeff = (c(1)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(2)*(-M(144)+M(158)-M(168) &
    -M(198)+M(199)-M(200)+M(201)+M(204))) * den(84)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,143)
  Gcoeff = (c(2)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(84)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,144)
  Gcoeff = (c(1)*(-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59)+M(80))+c(2)*(M(134)+M(141)-M(142)-M(144) &
    -M(172)-M(200)+M(202)+M(214))) * den(15)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,64)
  Gcoeff = (c(1)*(-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59)+M(80))+c(2)*(M(133)-M(143)+M(151)-M(152) &
    -M(170)+M(212)-M(242)+M(244))) * den(15)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,145)
  Gcoeff = (c(1)*(-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59)+M(80))+c(2)*(M(133)-M(143)+M(151)-M(152) &
    -M(170)+M(212)-M(242)+M(244))) * den(15)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,65)
  Gcoeff = (c(1)*(-M(25)+M(26)+M(29)-M(30)+M(31)-M(32)-M(35)+M(36)+M(42)-M(47)-M(59)+M(80))+c(2)*(M(134)+M(141)-M(142)-M(144) &
    -M(172)-M(200)+M(202)+M(214))) * den(15)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,146)
  Gcoeff = (c(2)*(M(133)-M(134)-M(141)+M(142)-M(143)+M(144)+M(151)-M(152)-M(170)+M(172)+M(200)-M(202)+M(212)-M(214)-M(242) &
    +M(244))) * den(15)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,66)
  Gcoeff = (c(2)*(-M(133)+M(134)+M(141)-M(142)+M(143)-M(144)-M(151)+M(152)+M(170)-M(172)-M(200)+M(202)-M(212)+M(214)+M(242) &
    -M(244))) * den(15)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,147)
  Gcoeff = (c(1)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(2)*(M(133)+M(135)-M(146)-M(152) &
    -M(170)-M(176)+M(220)+M(244))) * den(152)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,67)
  Gcoeff = (c(1)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(2)*(M(132)+M(134)-M(142)-M(148) &
    -M(166)-M(172)+M(202)+M(226))) * den(152)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,148)
  Gcoeff = (c(1)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(2)*(M(132)+M(134)-M(142)-M(148) &
    -M(166)-M(172)+M(202)+M(226))) * den(152)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,68)
  Gcoeff = (c(1)*(M(41)+M(42)+M(43)-M(44)-M(47)-M(50)-M(56)-M(59)-M(62)+M(68)+M(80)+M(92))+c(2)*(M(133)+M(135)-M(146)-M(152) &
    -M(170)-M(176)+M(220)+M(244))) * den(152)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,149)
  Gcoeff = (c(2)*(M(132)-M(133)+M(134)-M(135)-M(142)+M(146)-M(148)+M(152)-M(166)+M(170)-M(172)+M(176)+M(202)-M(220)+M(226) &
    -M(244))) * den(152)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,69)
  Gcoeff = (c(2)*(-M(132)+M(133)-M(134)+M(135)+M(142)-M(146)+M(148)-M(152)+M(166)-M(170)+M(172)-M(176)-M(202)+M(220)-M(226) &
    +M(244))) * den(152)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,150)
  Gcoeff = (c(1)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(2)*(-M(132)+M(143)+M(148)-M(151) &
    +M(166)-M(212)-M(226)+M(242))) * den(142)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,70)
  Gcoeff = (c(1)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(2)*(-M(135)-M(141)+M(144)+M(146) &
    +M(176)+M(200)-M(214)-M(220))) * den(142)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,71)
  Gcoeff = (c(2)*(M(132)-M(135)-M(141)-M(143)+M(144)+M(146)-M(148)+M(151)-M(166)+M(176)+M(200)+M(212)-M(214)-M(220)+M(226) &
    -M(242))) * den(142)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,72)
  Gcoeff = (c(1)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(2)*(-M(135)-M(141)+M(144)+M(146) &
    +M(176)+M(200)-M(214)-M(220))) * den(142)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,151)
  Gcoeff = (c(1)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(2)*(-M(132)+M(143)+M(148)-M(151) &
    +M(166)-M(212)-M(226)+M(242))) * den(142)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,152)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(141)+M(143)-M(144)-M(146)+M(148)-M(151)+M(166)-M(176)-M(200)-M(212)+M(214)+M(220)-M(226) &
    +M(242))) * den(142)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,153)
  Gcoeff = (c(2)*(M(132)-M(133)+M(134)-M(135)-M(142)+M(146)-M(148)+M(152)-M(166)+M(170)-M(172)+M(176)+M(202)-M(220)+M(226) &
    -M(244))) * den(338)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,10)
  Gcoeff = (c(2)*(M(133)-M(134)-M(141)+M(142)-M(143)+M(144)+M(151)-M(152)-M(170)+M(172)+M(200)-M(202)+M(212)-M(214)-M(242) &
    +M(244))) * den(338)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,11)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(141)+M(143)-M(144)-M(146)+M(148)-M(151)+M(166)-M(176)-M(200)-M(212)+M(214)+M(220)-M(226) &
    +M(242))) * den(338)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,12)
  Gcoeff = (c(2)*(-M(132)+M(133)-M(134)+M(135)+M(142)-M(146)+M(148)-M(152)+M(166)-M(170)+M(172)-M(176)-M(202)+M(220)-M(226) &
    +M(244))) * den(338)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,55)
  Gcoeff = (c(2)*(-M(133)+M(134)+M(141)-M(142)+M(143)-M(144)-M(151)+M(152)+M(170)-M(172)-M(200)+M(202)-M(212)+M(214)+M(242) &
    -M(244))) * den(338)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,56)
  Gcoeff = (c(2)*(M(132)-M(135)-M(141)-M(143)+M(144)+M(146)-M(148)+M(151)-M(166)+M(176)+M(200)+M(212)-M(214)-M(220)+M(226) &
    -M(242))) * den(338)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,57)
  Gcoeff = (c(1)*(-M(9)+M(10)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)-M(47)+M(54)-M(59)+M(77))+c(2)*(-M(148)+M(158)+M(165)-M(166) &
    -M(168)-M(198)+M(201)+M(208))) * den(36)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,73)
  Gcoeff = (c(1)*(-M(9)+M(10)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)-M(47)+M(54)-M(59)+M(77))+c(2)*(-M(146)+M(157)-M(167)+M(175) &
    -M(176)+M(206)-M(240)+M(243))) * den(36)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,154)
  Gcoeff = (c(1)*(-M(9)+M(10)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)-M(47)+M(54)-M(59)+M(77))+c(2)*(-M(146)+M(157)-M(167)+M(175) &
    -M(176)+M(206)-M(240)+M(243))) * den(36)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,74)
  Gcoeff = (c(1)*(-M(9)+M(10)+M(15)-M(16)+M(17)-M(18)-M(23)+M(24)-M(47)+M(54)-M(59)+M(77))+c(2)*(-M(148)+M(158)+M(165)-M(166) &
    -M(168)-M(198)+M(201)+M(208))) * den(36)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,155)
  Gcoeff = (c(2)*(-M(146)+M(148)+M(157)-M(158)-M(165)+M(166)-M(167)+M(168)+M(175)-M(176)+M(198)-M(201)+M(206)-M(208)-M(240) &
    +M(243))) * den(36)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,75)
  Gcoeff = (c(2)*(M(146)-M(148)-M(157)+M(158)+M(165)-M(166)+M(167)-M(168)-M(175)+M(176)-M(198)+M(201)-M(206)+M(208)+M(240) &
    -M(243))) * den(36)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,156)
  Gcoeff = (c(1)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(2)*(M(146)+M(152)-M(157)-M(159) &
    +M(170)+M(176)-M(219)-M(243))) * den(176)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,76)
  Gcoeff = (c(1)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(2)*(M(142)+M(148)-M(156)-M(158) &
    +M(166)+M(172)-M(201)-M(225))) * den(176)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,157)
  Gcoeff = (c(1)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(2)*(M(142)+M(148)-M(156)-M(158) &
    +M(166)+M(172)-M(201)-M(225))) * den(176)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,77)
  Gcoeff = (c(1)*(M(44)+M(47)+M(50)-M(53)-M(54)-M(55)+M(56)+M(59)+M(62)-M(65)-M(77)-M(89))+c(2)*(M(146)+M(152)-M(157)-M(159) &
    +M(170)+M(176)-M(219)-M(243))) * den(176)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,158)
  Gcoeff = (c(2)*(M(142)-M(146)+M(148)-M(152)-M(156)+M(157)-M(158)+M(159)+M(166)-M(170)+M(172)-M(176)-M(201)+M(219)-M(225) &
    +M(243))) * den(176)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,78)
  Gcoeff = (c(2)*(-M(142)+M(146)-M(148)+M(152)+M(156)-M(157)+M(158)-M(159)-M(166)+M(170)-M(172)+M(176)+M(201)-M(219)+M(225) &
    -M(243))) * den(176)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,159)
  Gcoeff = (c(1)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(2)*(-M(132)+M(156)+M(203)-M(209)+M(225) &
    -M(226)-M(239)+M(241))) * den(44)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,79)
  Gcoeff = (c(1)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(2)*(-M(135)+M(159)-M(197)+M(199)+M(204) &
    -M(210)+M(219)-M(220))) * den(44)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,160)
  Gcoeff = (c(1)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(2)*(-M(135)+M(159)-M(197)+M(199)+M(204) &
    -M(210)+M(219)-M(220))) * den(44)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,80)
  Gcoeff = (c(1)*(M(1)-M(2)-M(5)+M(6)-M(7)+M(8)+M(19)-M(20)-M(42)+M(54)+M(77)-M(80))+c(2)*(-M(132)+M(156)+M(203)-M(209)+M(225) &
    -M(226)-M(239)+M(241))) * den(44)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,161)
  Gcoeff = (c(2)*(M(132)-M(135)-M(156)+M(159)-M(197)+M(199)-M(203)+M(204)+M(209)-M(210)+M(219)-M(220)-M(225)+M(226)+M(239) &
    -M(241))) * den(44)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,81)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(156)-M(159)+M(197)-M(199)+M(203)-M(204)-M(209)+M(210)-M(219)+M(220)+M(225)-M(226)-M(239) &
    +M(241))) * den(44)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,162)
  Gcoeff = (c(1)*(M(41)+M(42)+M(43)-M(53)-M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92))+c(2)*(M(133)+M(135)-M(157)-M(159) &
    -M(219)+M(220)-M(243)+M(244))) * den(200)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,82)
  Gcoeff = (c(1)*(M(41)+M(42)+M(43)-M(53)-M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92))+c(2)*(M(132)+M(134)-M(156)-M(158) &
    -M(201)+M(202)-M(225)+M(226))) * den(200)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,163)
  Gcoeff = (c(1)*(M(41)+M(42)+M(43)-M(53)-M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92))+c(2)*(M(132)+M(134)-M(156)-M(158) &
    -M(201)+M(202)-M(225)+M(226))) * den(200)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,83)
  Gcoeff = (c(1)*(M(41)+M(42)+M(43)-M(53)-M(54)-M(55)-M(65)+M(68)-M(77)+M(80)-M(89)+M(92))+c(2)*(M(133)+M(135)-M(157)-M(159) &
    -M(219)+M(220)-M(243)+M(244))) * den(200)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,164)
  Gcoeff = (c(2)*(M(132)-M(133)+M(134)-M(135)-M(156)+M(157)-M(158)+M(159)-M(201)+M(202)+M(219)-M(220)-M(225)+M(226)+M(243) &
    -M(244))) * den(200)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,84)
  Gcoeff = (c(2)*(-M(132)+M(133)-M(134)+M(135)+M(156)-M(157)+M(158)-M(159)+M(201)-M(202)-M(219)+M(220)+M(225)-M(226)-M(243) &
    +M(244))) * den(200)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,165)
  Gcoeff = (c(1)*(M(42)+M(45)+M(52)-M(54)-M(66)-M(77)-M(78)-M(79)+M(80)+M(83)+M(86)-M(91))+c(2)*(M(132)+M(151)-M(165)-M(203) &
    -M(208)+M(212)+M(226)-M(241))) * den(250)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,85)
  Gcoeff = (c(1)*(M(42)+M(45)+M(52)-M(54)-M(66)-M(77)-M(78)-M(79)+M(80)+M(83)+M(86)-M(91))+c(2)*(M(135)+M(141)-M(175)-M(199) &
    -M(204)-M(206)+M(214)+M(220))) * den(250)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,86)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(141)-M(151)+M(165)-M(175)-M(199)+M(203)-M(204)-M(206)+M(208)-M(212)+M(214)+M(220)-M(226) &
    +M(241))) * den(250)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,87)
  Gcoeff = (c(1)*(M(42)+M(45)+M(52)-M(54)-M(66)-M(77)-M(78)-M(79)+M(80)+M(83)+M(86)-M(91))+c(2)*(M(135)+M(141)-M(175)-M(199) &
    -M(204)-M(206)+M(214)+M(220))) * den(250)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,166)
  Gcoeff = (c(1)*(M(42)+M(45)+M(52)-M(54)-M(66)-M(77)-M(78)-M(79)+M(80)+M(83)+M(86)-M(91))+c(2)*(M(132)+M(151)-M(165)-M(203) &
    -M(208)+M(212)+M(226)-M(241))) * den(250)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,167)
  Gcoeff = (c(2)*(M(132)-M(135)-M(141)+M(151)-M(165)+M(175)+M(199)-M(203)+M(204)+M(206)-M(208)+M(212)-M(214)-M(220)+M(226) &
    -M(241))) * den(250)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,168)
  Gcoeff = (c(1)*(M(47)+M(48)+M(49)-M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98))+c(2)*(M(143)+M(148)-M(165)+M(166) &
    -M(203)-M(208)-M(241)+M(242))) * den(255)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,88)
  Gcoeff = (c(1)*(M(47)+M(48)+M(49)-M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98))+c(2)*(M(144)+M(146)-M(175)+M(176) &
    -M(199)+M(200)-M(204)-M(206))) * den(255)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,89)
  Gcoeff = (c(2)*(-M(143)+M(144)+M(146)-M(148)+M(165)-M(166)-M(175)+M(176)-M(199)+M(200)+M(203)-M(204)-M(206)+M(208)+M(241) &
    -M(242))) * den(255)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,90)
  Gcoeff = (c(1)*(M(47)+M(48)+M(49)-M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98))+c(2)*(M(144)+M(146)-M(175)+M(176) &
    -M(199)+M(200)-M(204)-M(206))) * den(255)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,169)
  Gcoeff = (c(1)*(M(47)+M(48)+M(49)-M(54)+M(59)-M(66)+M(71)-M(77)-M(78)-M(79)-M(91)+M(98))+c(2)*(M(143)+M(148)-M(165)+M(166) &
    -M(203)-M(208)-M(241)+M(242))) * den(255)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,170)
  Gcoeff = (c(2)*(M(143)-M(144)-M(146)+M(148)-M(165)+M(166)+M(175)-M(176)+M(199)-M(200)-M(203)+M(204)+M(206)-M(208)-M(241) &
    +M(242))) * den(255)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,171)
  Gcoeff = (c(1)*(M(47)-M(54)-M(57)+M(59)+M(60)+M(61)-M(64)+M(72)-M(77)-M(84)-M(87)+M(99))+c(2)*(M(142)-M(156)+M(167)+M(172) &
    -M(175)-M(206)-M(225)+M(240))) * den(166)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,91)
  Gcoeff = (c(1)*(M(47)-M(54)-M(57)+M(59)+M(60)+M(61)-M(64)+M(72)-M(77)-M(84)-M(87)+M(99))+c(2)*(M(152)-M(159)-M(165)+M(168) &
    +M(170)+M(198)-M(208)-M(219))) * den(166)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,92)
  Gcoeff = (c(2)*(-M(142)+M(152)+M(156)-M(159)-M(165)-M(167)+M(168)+M(170)-M(172)+M(175)+M(198)+M(206)-M(208)-M(219)+M(225) &
    -M(240))) * den(166)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,93)
  Gcoeff = (c(1)*(M(47)-M(54)-M(57)+M(59)+M(60)+M(61)-M(64)+M(72)-M(77)-M(84)-M(87)+M(99))+c(2)*(M(152)-M(159)-M(165)+M(168) &
    +M(170)+M(198)-M(208)-M(219))) * den(166)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,172)
  Gcoeff = (c(1)*(M(47)-M(54)-M(57)+M(59)+M(60)+M(61)-M(64)+M(72)-M(77)-M(84)-M(87)+M(99))+c(2)*(M(142)-M(156)+M(167)+M(172) &
    -M(175)-M(206)-M(225)+M(240))) * den(166)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,173)
  Gcoeff = (c(2)*(M(142)-M(152)-M(156)+M(159)+M(165)+M(167)-M(168)-M(170)+M(172)-M(175)-M(198)-M(206)+M(208)+M(219)-M(225) &
    +M(240))) * den(166)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,174)
  Gcoeff = (c(2)*(M(142)-M(146)+M(148)-M(152)-M(156)+M(157)-M(158)+M(159)+M(166)-M(170)+M(172)-M(176)-M(201)+M(219)-M(225) &
    +M(243))) * den(481)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,13)
  Gcoeff = (c(2)*(-M(142)+M(152)+M(156)-M(159)-M(165)-M(167)+M(168)+M(170)-M(172)+M(175)+M(198)+M(206)-M(208)-M(219)+M(225) &
    -M(240))) * den(481)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,14)
  Gcoeff = (c(2)*(M(146)-M(148)-M(157)+M(158)+M(165)-M(166)+M(167)-M(168)-M(175)+M(176)-M(198)+M(201)-M(206)+M(208)+M(240) &
    -M(243))) * den(481)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,15)
  Gcoeff = (c(2)*(-M(142)+M(146)-M(148)+M(152)+M(156)-M(157)+M(158)-M(159)-M(166)+M(170)-M(172)+M(176)+M(201)-M(219)+M(225) &
    -M(243))) * den(481)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,58)
  Gcoeff = (c(2)*(M(142)-M(152)-M(156)+M(159)+M(165)+M(167)-M(168)-M(170)+M(172)-M(175)-M(198)-M(206)+M(208)+M(219)-M(225) &
    +M(240))) * den(481)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,59)
  Gcoeff = (c(2)*(-M(146)+M(148)+M(157)-M(158)-M(165)+M(166)-M(167)+M(168)+M(175)-M(176)+M(198)-M(201)+M(206)-M(208)-M(240) &
    +M(243))) * den(481)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,60)
  Gcoeff = (c(1)*(-M(42)+M(54)+M(57)+M(64)-M(69)+M(77)-M(80)-M(81)-M(82)+M(84)+M(87)-M(94))+c(2)*(-M(141)+M(156)+M(175)+M(206) &
    -M(209)-M(214)+M(225)-M(239))) * den(232)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,94)
  Gcoeff = (c(1)*(-M(42)+M(54)+M(57)+M(64)-M(69)+M(77)-M(80)-M(81)-M(82)+M(84)+M(87)-M(94))+c(2)*(-M(151)+M(159)+M(165)-M(197) &
    +M(208)-M(210)-M(212)+M(219))) * den(232)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,95)
  Gcoeff = (c(2)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(232)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,96)
  Gcoeff = (c(1)*(-M(42)+M(54)+M(57)+M(64)-M(69)+M(77)-M(80)-M(81)-M(82)+M(84)+M(87)-M(94))+c(2)*(-M(151)+M(159)+M(165)-M(197) &
    +M(208)-M(210)-M(212)+M(219))) * den(232)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,175)
  Gcoeff = (c(1)*(-M(42)+M(54)+M(57)+M(64)-M(69)+M(77)-M(80)-M(81)-M(82)+M(84)+M(87)-M(94))+c(2)*(-M(141)+M(156)+M(175)+M(206) &
    -M(209)-M(214)+M(225)-M(239))) * den(232)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,176)
  Gcoeff = (c(2)*(-M(141)+M(151)+M(156)-M(159)-M(165)+M(175)+M(197)+M(206)-M(208)-M(209)+M(210)+M(212)-M(214)-M(219)+M(225) &
    -M(239))) * den(232)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,177)
  Gcoeff = (c(1)*(-M(42)+M(47)+M(59)+M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99))+c(2)*(-M(141)+M(142)+M(167)+M(172) &
    -M(209)-M(214)-M(239)+M(240))) * den(239)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,97)
  Gcoeff = (c(1)*(-M(42)+M(47)+M(59)+M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99))+c(2)*(-M(151)+M(152)+M(168)+M(170) &
    -M(197)+M(198)-M(210)-M(212))) * den(239)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,98)
  Gcoeff = (c(2)*(M(141)-M(142)-M(151)+M(152)-M(167)+M(168)+M(170)-M(172)-M(197)+M(198)+M(209)-M(210)-M(212)+M(214)+M(239) &
    -M(240))) * den(239)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,99)
  Gcoeff = (c(1)*(-M(42)+M(47)+M(59)+M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99))+c(2)*(-M(151)+M(152)+M(168)+M(170) &
    -M(197)+M(198)-M(210)-M(212))) * den(239)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,178)
  Gcoeff = (c(1)*(-M(42)+M(47)+M(59)+M(60)+M(61)-M(69)+M(72)-M(80)-M(81)-M(82)-M(94)+M(99))+c(2)*(-M(141)+M(142)+M(167)+M(172) &
    -M(209)-M(214)-M(239)+M(240))) * den(239)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,179)
  Gcoeff = (c(2)*(-M(141)+M(142)+M(151)-M(152)+M(167)-M(168)-M(170)+M(172)+M(197)-M(198)-M(209)+M(210)+M(212)-M(214)-M(239) &
    +M(240))) * den(239)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,180)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(156)-M(159)+M(197)-M(199)+M(203)-M(204)-M(209)+M(210)-M(219)+M(220)+M(225)-M(226)-M(239) &
    +M(241))) * den(550)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,16)
  Gcoeff = (c(2)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(550)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,17)
  Gcoeff = (c(2)*(M(132)-M(135)-M(141)+M(151)-M(165)+M(175)+M(199)-M(203)+M(204)+M(206)-M(208)+M(212)-M(214)-M(220)+M(226) &
    -M(241))) * den(550)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,18)
  Gcoeff = (c(2)*(M(132)-M(135)-M(156)+M(159)-M(197)+M(199)-M(203)+M(204)+M(209)-M(210)+M(219)-M(220)-M(225)+M(226)+M(239) &
    -M(241))) * den(550)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,61)
  Gcoeff = (c(2)*(-M(141)+M(151)+M(156)-M(159)-M(165)+M(175)+M(197)+M(206)-M(208)-M(209)+M(210)+M(212)-M(214)-M(219)+M(225) &
    -M(239))) * den(550)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,62)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(141)-M(151)+M(165)-M(175)-M(199)+M(203)-M(204)-M(206)+M(208)-M(212)+M(214)+M(220)-M(226) &
    +M(241))) * den(550)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,63)
  Gcoeff = (c(2)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(565)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,19)
  Gcoeff = (c(2)*(M(133)-M(134)-M(143)+M(144)-M(167)+M(168)-M(197)+M(198)+M(200)-M(202)+M(209)-M(210)+M(239)-M(240)-M(242) &
    +M(244))) * den(565)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,20)
  Gcoeff = (c(2)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(565)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,21)
  Gcoeff = (c(2)*(M(133)-M(134)-M(157)+M(158)-M(197)+M(199)+M(201)-M(202)-M(203)+M(204)+M(209)-M(210)+M(239)-M(241)-M(243) &
    +M(244))) * den(565)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,64)
  Gcoeff = (c(2)*(-M(133)+M(134)+M(143)-M(144)+M(167)-M(168)+M(197)-M(198)-M(200)+M(202)-M(209)+M(210)-M(239)+M(240)+M(242) &
    -M(244))) * den(565)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,65)
  Gcoeff = (c(2)*(-M(143)+M(144)+M(157)-M(158)-M(167)+M(168)+M(198)-M(199)+M(200)-M(201)+M(203)-M(204)-M(240)+M(241)-M(242) &
    +M(243))) * den(565)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,66)
  Gcoeff = (c(1)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(2)*(M(134)-M(158)+M(197) &
    -M(199)-M(201)+M(202)-M(204)+M(210))) * den(192)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,100)
  Gcoeff = (c(1)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(2)*(M(133)-M(157)-M(203) &
    +M(209)+M(239)-M(241)-M(243)+M(244))) * den(192)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,101)
  Gcoeff = (c(2)*(M(133)-M(134)-M(157)+M(158)-M(197)+M(199)+M(201)-M(202)-M(203)+M(204)+M(209)-M(210)+M(239)-M(241)-M(243) &
    +M(244))) * den(192)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,102)
  Gcoeff = (c(1)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(2)*(M(133)-M(157)-M(203) &
    +M(209)+M(239)-M(241)-M(243)+M(244))) * den(192)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,181)
  Gcoeff = (c(1)*(M(42)-M(54)-M(77)+M(80)-M(101)+M(103)+M(104)-M(106)+M(109)-M(115)-M(117)+M(123))+c(2)*(M(134)-M(158)+M(197) &
    -M(199)-M(201)+M(202)-M(204)+M(210))) * den(192)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,182)
  Gcoeff = (c(2)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(192)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,183)
  Gcoeff = (c(2)*(M(132)-M(133)+M(134)-M(135)-M(156)+M(157)-M(158)+M(159)-M(201)+M(202)+M(219)-M(220)-M(225)+M(226)+M(243) &
    -M(244))) * den(607)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,22)
  Gcoeff = (c(2)*(M(133)-M(134)-M(157)+M(158)-M(197)+M(199)+M(201)-M(202)-M(203)+M(204)+M(209)-M(210)+M(239)-M(241)-M(243) &
    +M(244))) * den(607)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,23)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(156)-M(159)+M(197)-M(199)+M(203)-M(204)-M(209)+M(210)-M(219)+M(220)+M(225)-M(226)-M(239) &
    +M(241))) * den(607)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,24)
  Gcoeff = (c(2)*(-M(132)+M(133)-M(134)+M(135)+M(156)-M(157)+M(158)-M(159)+M(201)-M(202)-M(219)+M(220)+M(225)-M(226)-M(243) &
    +M(244))) * den(607)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,67)
  Gcoeff = (c(2)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(607)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,68)
  Gcoeff = (c(2)*(M(132)-M(135)-M(156)+M(159)-M(197)+M(199)-M(203)+M(204)+M(209)-M(210)+M(219)-M(220)-M(225)+M(226)+M(239) &
    -M(241))) * den(607)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,69)
  Gcoeff = (c(1)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(2)*(-M(144)+M(158)-M(168) &
    -M(198)+M(199)-M(200)+M(201)+M(204))) * den(216)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,103)
  Gcoeff = (c(1)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(2)*(-M(143)+M(157)-M(167) &
    +M(203)-M(240)+M(241)-M(242)+M(243))) * den(216)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,104)
  Gcoeff = (c(2)*(-M(143)+M(144)+M(157)-M(158)-M(167)+M(168)+M(198)-M(199)+M(200)-M(201)+M(203)-M(204)-M(240)+M(241)-M(242) &
    +M(243))) * den(216)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,105)
  Gcoeff = (c(1)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(2)*(-M(143)+M(157)-M(167) &
    +M(203)-M(240)+M(241)-M(242)+M(243))) * den(216)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,184)
  Gcoeff = (c(1)*(-M(47)+M(54)-M(59)+M(77)+M(101)+M(106)-M(107)-M(113)-M(114)+M(115)+M(117)-M(120))+c(2)*(-M(144)+M(158)-M(168) &
    -M(198)+M(199)-M(200)+M(201)+M(204))) * den(216)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,185)
  Gcoeff = (c(2)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(216)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,186)
  Gcoeff = (c(1)*(M(42)-M(47)-M(59)+M(80)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123))+c(2)*(M(134)-M(144)-M(168) &
    +M(197)-M(198)-M(200)+M(202)+M(210))) * den(221)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,106)
  Gcoeff = (c(1)*(M(42)-M(47)-M(59)+M(80)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123))+c(2)*(M(133)-M(143)-M(167) &
    +M(209)+M(239)-M(240)-M(242)+M(244))) * den(221)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,107)
  Gcoeff = (c(2)*(M(133)-M(134)-M(143)+M(144)-M(167)+M(168)-M(197)+M(198)+M(200)-M(202)+M(209)-M(210)+M(239)-M(240)-M(242) &
    +M(244))) * den(221)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,108)
  Gcoeff = (c(1)*(M(42)-M(47)-M(59)+M(80)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123))+c(2)*(M(133)-M(143)-M(167) &
    +M(209)+M(239)-M(240)-M(242)+M(244))) * den(221)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,187)
  Gcoeff = (c(1)*(M(42)-M(47)-M(59)+M(80)+M(103)+M(104)-M(107)+M(109)-M(113)-M(114)-M(120)+M(123))+c(2)*(M(134)-M(144)-M(168) &
    +M(197)-M(198)-M(200)+M(202)+M(210))) * den(221)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,188)
  Gcoeff = (c(2)*(-M(133)+M(134)+M(143)-M(144)+M(167)-M(168)+M(197)-M(198)-M(200)+M(202)-M(209)+M(210)-M(239)+M(240)+M(242) &
    -M(244))) * den(221)
  T2sum(1:1,2) = T2sum(1:1,2) + Gcoeff * G0tensor(:,189)
  Gcoeff = (c(2)*(M(146)-M(148)-M(157)+M(158)+M(165)-M(166)+M(167)-M(168)-M(175)+M(176)-M(198)+M(201)-M(206)+M(208)+M(240) &
    -M(243))) * den(676)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,25)
  Gcoeff = (c(2)*(-M(143)+M(144)+M(157)-M(158)-M(167)+M(168)+M(198)-M(199)+M(200)-M(201)+M(203)-M(204)-M(240)+M(241)-M(242) &
    +M(243))) * den(676)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,26)
  Gcoeff = (c(2)*(M(143)-M(144)-M(146)+M(148)-M(165)+M(166)+M(175)-M(176)+M(199)-M(200)-M(203)+M(204)+M(206)-M(208)-M(241) &
    +M(242))) * den(676)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,27)
  Gcoeff = (c(2)*(-M(146)+M(148)+M(157)-M(158)-M(165)+M(166)-M(167)+M(168)+M(175)-M(176)+M(198)-M(201)+M(206)-M(208)-M(240) &
    +M(243))) * den(676)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,70)
  Gcoeff = (c(2)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(676)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,71)
  Gcoeff = (c(2)*(-M(143)+M(144)+M(146)-M(148)+M(165)-M(166)-M(175)+M(176)-M(199)+M(200)+M(203)-M(204)-M(206)+M(208)+M(241) &
    -M(242))) * den(676)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,72)
  Gcoeff = (c(2)*(M(142)-M(152)-M(156)+M(159)+M(165)+M(167)-M(168)-M(170)+M(172)-M(175)-M(198)-M(206)+M(208)+M(219)-M(225) &
    +M(240))) * den(691)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,28)
  Gcoeff = (c(2)*(M(141)-M(142)-M(151)+M(152)-M(167)+M(168)+M(170)-M(172)-M(197)+M(198)+M(209)-M(210)-M(212)+M(214)+M(239) &
    -M(240))) * den(691)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,29)
  Gcoeff = (c(2)*(-M(141)+M(151)+M(156)-M(159)-M(165)+M(175)+M(197)+M(206)-M(208)-M(209)+M(210)+M(212)-M(214)-M(219)+M(225) &
    -M(239))) * den(691)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,30)
  Gcoeff = (c(2)*(-M(142)+M(152)+M(156)-M(159)-M(165)-M(167)+M(168)+M(170)-M(172)+M(175)+M(198)+M(206)-M(208)-M(219)+M(225) &
    -M(240))) * den(691)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,73)
  Gcoeff = (c(2)*(-M(141)+M(142)+M(151)-M(152)+M(167)-M(168)-M(170)+M(172)+M(197)-M(198)-M(209)+M(210)+M(212)-M(214)-M(239) &
    +M(240))) * den(691)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,74)
  Gcoeff = (c(2)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(691)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,75)
  Gcoeff = (c(2)*(-M(133)+M(134)+M(141)-M(142)+M(143)-M(144)-M(151)+M(152)+M(170)-M(172)-M(200)+M(202)-M(212)+M(214)+M(242) &
    -M(244))) * den(715)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,31)
  Gcoeff = (c(2)*(M(133)-M(134)-M(143)+M(144)-M(167)+M(168)-M(197)+M(198)+M(200)-M(202)+M(209)-M(210)+M(239)-M(240)-M(242) &
    +M(244))) * den(715)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,32)
  Gcoeff = (c(2)*(-M(141)+M(142)+M(151)-M(152)+M(167)-M(168)-M(170)+M(172)+M(197)-M(198)-M(209)+M(210)+M(212)-M(214)-M(239) &
    +M(240))) * den(715)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,33)
  Gcoeff = (c(2)*(M(133)-M(134)-M(141)+M(142)-M(143)+M(144)+M(151)-M(152)-M(170)+M(172)+M(200)-M(202)+M(212)-M(214)-M(242) &
    +M(244))) * den(715)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,76)
  Gcoeff = (c(2)*(-M(133)+M(134)+M(143)-M(144)+M(167)-M(168)+M(197)-M(198)-M(200)+M(202)-M(209)+M(210)-M(239)+M(240)+M(242) &
    -M(244))) * den(715)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,77)
  Gcoeff = (c(2)*(M(141)-M(142)-M(151)+M(152)-M(167)+M(168)+M(170)-M(172)-M(197)+M(198)+M(209)-M(210)-M(212)+M(214)+M(239) &
    -M(240))) * den(715)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,78)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(141)+M(143)-M(144)-M(146)+M(148)-M(151)+M(166)-M(176)-M(200)-M(212)+M(214)+M(220)-M(226) &
    +M(242))) * den(730)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,34)
  Gcoeff = (c(2)*(-M(143)+M(144)+M(146)-M(148)+M(165)-M(166)-M(175)+M(176)-M(199)+M(200)+M(203)-M(204)-M(206)+M(208)+M(241) &
    -M(242))) * den(730)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,35)
  Gcoeff = (c(2)*(M(132)-M(135)-M(141)+M(151)-M(165)+M(175)+M(199)-M(203)+M(204)+M(206)-M(208)+M(212)-M(214)-M(220)+M(226) &
    -M(241))) * den(730)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,36)
  Gcoeff = (c(2)*(M(132)-M(135)-M(141)-M(143)+M(144)+M(146)-M(148)+M(151)-M(166)+M(176)+M(200)+M(212)-M(214)-M(220)+M(226) &
    -M(242))) * den(730)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,79)
  Gcoeff = (c(2)*(M(143)-M(144)-M(146)+M(148)-M(165)+M(166)+M(175)-M(176)+M(199)-M(200)-M(203)+M(204)+M(206)-M(208)-M(241) &
    +M(242))) * den(730)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,80)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(141)-M(151)+M(165)-M(175)-M(199)+M(203)-M(204)-M(206)+M(208)-M(212)+M(214)+M(220)-M(226) &
    +M(241))) * den(730)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,81)
  Gcoeff = (c(2)*(M(131)-M(133)-M(135)+M(136)-M(140)+M(146)+M(152)-M(154)-M(164)+M(170)+M(176)-M(178)+M(196)-M(220)-M(244) &
    +M(250))) * den(138)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(135)-M(136)+M(155)-M(157)-M(159)+M(160)+M(195)-M(196)-M(219)+M(220)-M(243)+M(244)+M(249) &
    -M(250))) * den(138)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,2)
  Gcoeff = (c(2)*(M(140)-M(146)-M(152)+M(154)-M(155)+M(157)+M(159)-M(160)+M(164)-M(170)-M(176)+M(178)-M(195)+M(219)+M(243) &
    -M(249))) * den(138)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,3)
  Gcoeff = (c(2)*(-M(131)+M(133)+M(135)-M(136)+M(140)-M(146)-M(152)+M(154)+M(164)-M(170)-M(176)+M(178)-M(196)+M(220)+M(244) &
    -M(250))) * den(138)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,4)
  Gcoeff = (c(2)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(138)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,5)
  Gcoeff = (c(2)*(-M(140)+M(146)+M(152)-M(154)+M(155)-M(157)-M(159)+M(160)-M(164)+M(170)+M(176)-M(178)+M(195)-M(219)-M(243) &
    +M(249))) * den(138)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,6)
  Gcoeff = (c(3)*(-M(131)+M(133)+M(135)-M(136)+M(140)-M(146)-M(152)+M(154)+M(164)-M(170)-M(176)+M(178)-M(196)+M(220)+M(244) &
    -M(250))) * den(138)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,7)
  Gcoeff = (c(3)*(M(131)-M(133)-M(135)+M(136)-M(155)+M(157)+M(159)-M(160)-M(195)+M(196)+M(219)-M(220)+M(243)-M(244)-M(249) &
    +M(250))) * den(138)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,8)
  Gcoeff = (c(3)*(-M(140)+M(146)+M(152)-M(154)+M(155)-M(157)-M(159)+M(160)-M(164)+M(170)+M(176)-M(178)+M(195)-M(219)-M(243) &
    +M(249))) * den(138)
  T2sum(1:5,1) = T2sum(1:5,1) + Gcoeff * G1tensor(:,9)
  Gcoeff = (c(2)*(M(132)-M(133)+M(134)-M(135)-M(142)+M(146)-M(148)+M(152)-M(166)+M(170)-M(172)+M(176)+M(202)-M(220)+M(226) &
    -M(244))) * den(745)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,37)
  Gcoeff = (c(2)*(-M(132)+M(133)-M(134)+M(135)+M(156)-M(157)+M(158)-M(159)+M(201)-M(202)-M(219)+M(220)+M(225)-M(226)-M(243) &
    +M(244))) * den(745)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,38)
  Gcoeff = (c(2)*(M(142)-M(146)+M(148)-M(152)-M(156)+M(157)-M(158)+M(159)+M(166)-M(170)+M(172)-M(176)-M(201)+M(219)-M(225) &
    +M(243))) * den(745)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,39)
  Gcoeff = (c(2)*(-M(132)+M(133)-M(134)+M(135)+M(142)-M(146)+M(148)-M(152)+M(166)-M(170)+M(172)-M(176)-M(202)+M(220)-M(226) &
    +M(244))) * den(745)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,82)
  Gcoeff = (c(2)*(M(132)-M(133)+M(134)-M(135)-M(156)+M(157)-M(158)+M(159)-M(201)+M(202)+M(219)-M(220)-M(225)+M(226)+M(243) &
    -M(244))) * den(745)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,83)
  Gcoeff = (c(2)*(-M(142)+M(146)-M(148)+M(152)+M(156)-M(157)+M(158)-M(159)-M(166)+M(170)-M(172)+M(176)+M(201)-M(219)+M(225) &
    -M(243))) * den(745)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,84)
  Gcoeff = (c(2)*(-M(133)+M(134)+M(141)-M(142)+M(143)-M(144)-M(151)+M(152)+M(170)-M(172)-M(200)+M(202)-M(212)+M(214)+M(242) &
    -M(244))) * den(799)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,40)
  Gcoeff = (c(2)*(M(133)-M(134)-M(141)+M(142)-M(143)+M(144)+M(151)-M(152)-M(170)+M(172)+M(200)-M(202)+M(212)-M(214)-M(242) &
    +M(244))) * den(799)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,85)
  Gcoeff = (c(2)*(M(132)-M(133)+M(134)-M(135)-M(142)+M(146)-M(148)+M(152)-M(166)+M(170)-M(172)+M(176)+M(202)-M(220)+M(226) &
    -M(244))) * den(1157)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,41)
  Gcoeff = (c(2)*(-M(132)+M(133)-M(134)+M(135)+M(142)-M(146)+M(148)-M(152)+M(166)-M(170)+M(172)-M(176)-M(202)+M(220)-M(226) &
    +M(244))) * den(1157)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,86)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(141)+M(143)-M(144)-M(146)+M(148)-M(151)+M(166)-M(176)-M(200)-M(212)+M(214)+M(220)-M(226) &
    +M(242))) * den(1162)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,42)
  Gcoeff = (c(2)*(M(132)-M(135)-M(141)-M(143)+M(144)+M(146)-M(148)+M(151)-M(166)+M(176)+M(200)+M(212)-M(214)-M(220)+M(226) &
    -M(242))) * den(1162)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,87)
  Gcoeff = (c(2)*(M(146)-M(148)-M(157)+M(158)+M(165)-M(166)+M(167)-M(168)-M(175)+M(176)-M(198)+M(201)-M(206)+M(208)+M(240) &
    -M(243))) * den(860)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,43)
  Gcoeff = (c(2)*(-M(146)+M(148)+M(157)-M(158)-M(165)+M(166)-M(167)+M(168)+M(175)-M(176)+M(198)-M(201)+M(206)-M(208)-M(240) &
    +M(243))) * den(860)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,88)
  Gcoeff = (c(2)*(M(142)-M(146)+M(148)-M(152)-M(156)+M(157)-M(158)+M(159)+M(166)-M(170)+M(172)-M(176)-M(201)+M(219)-M(225) &
    +M(243))) * den(1169)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,44)
  Gcoeff = (c(2)*(-M(142)+M(146)-M(148)+M(152)+M(156)-M(157)+M(158)-M(159)-M(166)+M(170)-M(172)+M(176)+M(201)-M(219)+M(225) &
    -M(243))) * den(1169)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,89)
  Gcoeff = (c(2)*(M(142)-M(152)-M(156)+M(159)+M(165)+M(167)-M(168)-M(170)+M(172)-M(175)-M(198)-M(206)+M(208)+M(219)-M(225) &
    +M(240))) * den(1174)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,45)
  Gcoeff = (c(2)*(-M(142)+M(152)+M(156)-M(159)-M(165)-M(167)+M(168)+M(170)-M(172)+M(175)+M(198)+M(206)-M(208)-M(219)+M(225) &
    -M(240))) * den(1174)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,90)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(156)-M(159)+M(197)-M(199)+M(203)-M(204)-M(209)+M(210)-M(219)+M(220)+M(225)-M(226)-M(239) &
    +M(241))) * den(918)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,46)
  Gcoeff = (c(2)*(M(132)-M(135)-M(156)+M(159)-M(197)+M(199)-M(203)+M(204)+M(209)-M(210)+M(219)-M(220)-M(225)+M(226)+M(239) &
    -M(241))) * den(918)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,91)
  Gcoeff = (c(2)*(M(132)-M(133)+M(134)-M(135)-M(156)+M(157)-M(158)+M(159)-M(201)+M(202)+M(219)-M(220)-M(225)+M(226)+M(243) &
    -M(244))) * den(1181)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,47)
  Gcoeff = (c(2)*(-M(132)+M(133)-M(134)+M(135)+M(156)-M(157)+M(158)-M(159)+M(201)-M(202)-M(219)+M(220)+M(225)-M(226)-M(243) &
    +M(244))) * den(1181)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,92)
  Gcoeff = (c(2)*(-M(133)+M(134)+M(157)-M(158)+M(197)-M(199)-M(201)+M(202)+M(203)-M(204)-M(209)+M(210)-M(239)+M(241)+M(243) &
    -M(244))) * den(1184)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,48)
  Gcoeff = (c(2)*(M(133)-M(134)-M(157)+M(158)-M(197)+M(199)+M(201)-M(202)-M(203)+M(204)+M(209)-M(210)+M(239)-M(241)-M(243) &
    +M(244))) * den(1184)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,93)
  Gcoeff = (c(2)*(M(132)-M(135)-M(141)+M(151)-M(165)+M(175)+M(199)-M(203)+M(204)+M(206)-M(208)+M(212)-M(214)-M(220)+M(226) &
    -M(241))) * den(1228)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,49)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(141)-M(151)+M(165)-M(175)-M(199)+M(203)-M(204)-M(206)+M(208)-M(212)+M(214)+M(220)-M(226) &
    +M(241))) * den(1228)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,94)
  Gcoeff = (c(2)*(-M(141)+M(151)+M(156)-M(159)-M(165)+M(175)+M(197)+M(206)-M(208)-M(209)+M(210)+M(212)-M(214)-M(219)+M(225) &
    -M(239))) * den(1231)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,50)
  Gcoeff = (c(2)*(M(141)-M(151)-M(156)+M(159)+M(165)-M(175)-M(197)-M(206)+M(208)+M(209)-M(210)-M(212)+M(214)+M(219)-M(225) &
    +M(239))) * den(1231)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,95)
  Gcoeff = (c(2)*(M(143)-M(144)-M(146)+M(148)-M(165)+M(166)+M(175)-M(176)+M(199)-M(200)-M(203)+M(204)+M(206)-M(208)-M(241) &
    +M(242))) * den(1240)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,51)
  Gcoeff = (c(2)*(-M(143)+M(144)+M(146)-M(148)+M(165)-M(166)-M(175)+M(176)-M(199)+M(200)+M(203)-M(204)-M(206)+M(208)+M(241) &
    -M(242))) * den(1240)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,96)
  Gcoeff = (c(2)*(M(143)-M(144)-M(157)+M(158)+M(167)-M(168)-M(198)+M(199)-M(200)+M(201)-M(203)+M(204)+M(240)-M(241)+M(242) &
    -M(243))) * den(1243)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,52)
  Gcoeff = (c(2)*(-M(143)+M(144)+M(157)-M(158)-M(167)+M(168)+M(198)-M(199)+M(200)-M(201)+M(203)-M(204)-M(240)+M(241)-M(242) &
    +M(243))) * den(1243)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,97)
  Gcoeff = (c(2)*(-M(141)+M(142)+M(151)-M(152)+M(167)-M(168)-M(170)+M(172)+M(197)-M(198)-M(209)+M(210)+M(212)-M(214)-M(239) &
    +M(240))) * den(1252)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,53)
  Gcoeff = (c(2)*(M(141)-M(142)-M(151)+M(152)-M(167)+M(168)+M(170)-M(172)-M(197)+M(198)+M(209)-M(210)-M(212)+M(214)+M(239) &
    -M(240))) * den(1252)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,98)
  Gcoeff = (c(2)*(-M(133)+M(134)+M(143)-M(144)+M(167)-M(168)+M(197)-M(198)-M(200)+M(202)-M(209)+M(210)-M(239)+M(240)+M(242) &
    -M(244))) * den(1253)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,54)
  Gcoeff = (c(2)*(M(133)-M(134)-M(143)+M(144)-M(167)+M(168)-M(197)+M(198)+M(200)-M(202)+M(209)-M(210)+M(239)-M(240)-M(242) &
    +M(244))) * den(1253)
  T2sum(1:5,2) = T2sum(1:5,2) + Gcoeff * G1tensor(:,99)

end subroutine vamp_64

end module ol_vamp_64_ppjjjj_gggggg_1_/**/REALKIND
