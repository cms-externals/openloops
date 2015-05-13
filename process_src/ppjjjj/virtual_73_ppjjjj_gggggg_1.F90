
module ol_vamp_73_ppjjjj_gggggg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_73(M)
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
  complex(REALKIND), dimension(4,1,4,31) :: G0
  complex(REALKIND), dimension(1,18) :: G0tensor
  complex(REALKIND), dimension(5,30) :: G1tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,64),G0(:,:,:,2))
  call check_last_UV_W(l_switch,G0(:,:,:,2),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,1))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,64),wf(:,-2),G0(:,:,:,3))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,2))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,64),G0(:,:,:,4))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,3))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-2),wf(:,65),G0(:,:,:,5))
  call check_last_UV_W(l_switch,G0(:,:,:,5),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,4))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,65),wf(:,-2),G0(:,:,:,6))
  call check_last_UV_W(l_switch,G0(:,:,:,6),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,5))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-2),wf(:,65),G0(:,:,:,7))
  call check_last_UV_W(l_switch,G0(:,:,:,7),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,6))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,61),wf(:,84),G0(:,:,:,8))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,8),wf(:,-3),wf(:,-2),G0tensor(:,1))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,8),wf(:,-2),wf(:,-3),G0tensor(:,2))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,8),wf(:,-3),wf(:,-2),G0tensor(:,3))
  call check_last_UV_W(l_switch,G0(:,:,:,8),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,7))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,84),wf(:,61),G0(:,:,:,9))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,9),wf(:,-3),wf(:,-2),G0tensor(:,4))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,9),wf(:,-2),wf(:,-3),G0tensor(:,5))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,9),wf(:,-3),wf(:,-2),G0tensor(:,6))
  call check_last_UV_W(l_switch,G0(:,:,:,9),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,8))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,61),wf(:,84),G0(:,:,:,10))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,10),wf(:,-3),wf(:,-2),G0tensor(:,7))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,10),wf(:,-2),wf(:,-3),G0tensor(:,8))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,10),wf(:,-3),wf(:,-2),G0tensor(:,9))
  call check_last_UV_W(l_switch,G0(:,:,:,10),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,9))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,14),wf(:,61),G0(:,:,:,11))
  call check_last_UV_W(l_switch,G0(:,:,:,11),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,10))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,17),wf(:,61),G0(:,:,:,12))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,11))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,18),wf(:,61),G0(:,:,:,13))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,12))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,61),wf(:,14),G0(:,:,:,14))
  call check_last_UV_W(l_switch,G0(:,:,:,14),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,13))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,61),wf(:,17),G0(:,:,:,15))
  call check_last_UV_W(l_switch,G0(:,:,:,15),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,14))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,61),wf(:,18),G0(:,:,:,16))
  call check_last_UV_W(l_switch,G0(:,:,:,16),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,15))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,14),wf(:,61),G0(:,:,:,17))
  call check_last_UV_W(l_switch,G0(:,:,:,17),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,16))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,17),wf(:,61),G0(:,:,:,18))
  call check_last_UV_W(l_switch,G0(:,:,:,18),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,17))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,18),wf(:,61),G0(:,:,:,19))
  call check_last_UV_W(l_switch,G0(:,:,:,19),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,18))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,88),G0(:,:,:,20))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,20),wf(:,-3),wf(:,-2),G0tensor(:,10))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,20),wf(:,-2),wf(:,-3),G0tensor(:,11))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,20),wf(:,-3),wf(:,-2),G0tensor(:,12))
  call check_last_UV_W(l_switch,G0(:,:,:,20),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,19))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,88),wf(:,-5),G0(:,:,:,21))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,21),wf(:,-3),wf(:,-2),G0tensor(:,13))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,21),wf(:,-2),wf(:,-3),G0tensor(:,14))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,21),wf(:,-3),wf(:,-2),G0tensor(:,15))
  call check_last_UV_W(l_switch,G0(:,:,:,21),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,20))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,88),G0(:,:,:,22))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,22),wf(:,-3),wf(:,-2),G0tensor(:,16))
  call check_last_GGG_G_12(l_switch,G0(:,:,:,22),wf(:,-2),wf(:,-3),G0tensor(:,17))
  call check_last_GGG_G_23(l_switch,G0(:,:,:,22),wf(:,-3),wf(:,-2),G0tensor(:,18))
  call check_last_UV_W(l_switch,G0(:,:,:,22),Q(:,51),wf(:,62),Q(:,12),G1tensor(:,21))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,85),G0(:,:,:,23))
  call check_last_UV_W(l_switch,G0(:,:,:,23),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,22))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,86),G0(:,:,:,24))
  call check_last_UV_W(l_switch,G0(:,:,:,24),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,23))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-5),wf(:,87),G0(:,:,:,25))
  call check_last_UV_W(l_switch,G0(:,:,:,25),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,24))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,85),wf(:,-5),G0(:,:,:,26))
  call check_last_UV_W(l_switch,G0(:,:,:,26),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,25))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,86),wf(:,-5),G0(:,:,:,27))
  call check_last_UV_W(l_switch,G0(:,:,:,27),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,26))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,87),wf(:,-5),G0(:,:,:,28))
  call check_last_UV_W(l_switch,G0(:,:,:,28),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,27))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,85),G0(:,:,:,29))
  call check_last_UV_W(l_switch,G0(:,:,:,29),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,28))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,86),G0(:,:,:,30))
  call check_last_UV_W(l_switch,G0(:,:,:,30),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,29))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-5),wf(:,87),G0(:,:,:,31))
  call check_last_UV_W(l_switch,G0(:,:,:,31),Q(:,47),wf(:,-4),Q(:,16),G1tensor(:,30))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(2)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(305)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(2)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(305)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,2)
  Gcoeff = (c(3)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(305)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,3)
  Gcoeff = (c(2)*(M(131)-M(137)-M(143)+M(145)-M(149)+M(151)+M(153)-M(154)-M(164)+M(188)+M(212)-M(218)+M(236)-M(242)-M(248) &
    +M(250))) * den(305)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,4)
  Gcoeff = (c(2)*(-M(131)+M(137)+M(143)-M(145)+M(149)-M(151)-M(153)+M(154)+M(164)-M(188)-M(212)+M(218)-M(236)+M(242)+M(248) &
    -M(250))) * den(305)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,5)
  Gcoeff = (c(3)*(-M(131)+M(137)+M(143)-M(145)+M(149)-M(151)-M(153)+M(154)+M(164)-M(188)-M(212)+M(218)-M(236)+M(242)+M(248) &
    -M(250))) * den(305)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,6)
  Gcoeff = (c(1)*(M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(86)+M(98))+c(2)*(M(131)-M(132)+M(148)-M(154) &
    -M(164)+M(166)-M(226)+M(250))) * den(27)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,1)
  Gcoeff = (c(1)*(M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(86)+M(98))+c(2)*(M(137)-M(138)+M(147)-M(153) &
    -M(188)+M(190)-M(224)+M(248))) * den(27)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,2)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(27)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,3)
  Gcoeff = (c(1)*(M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(86)+M(98))+c(2)*(M(137)-M(138)+M(147)-M(153) &
    -M(188)+M(190)-M(224)+M(248))) * den(27)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,4)
  Gcoeff = (c(1)*(M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(86)+M(98))+c(2)*(M(131)-M(132)+M(148)-M(154) &
    -M(164)+M(166)-M(226)+M(250))) * den(27)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,5)
  Gcoeff = (c(2)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(27)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,6)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(27)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,7)
  Gcoeff = (c(2)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(27)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,8)
  Gcoeff = (c(3)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(27)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,9)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(29)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,10)
  Gcoeff = (c(2)*(M(132)-M(135)-M(141)-M(143)+M(144)+M(146)-M(148)+M(151)-M(166)+M(176)+M(200)+M(212)-M(214)-M(220)+M(226) &
    -M(242))) * den(29)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,11)
  Gcoeff = (c(2)*(M(135)-M(138)+M(141)-M(144)+M(145)-M(146)+M(147)-M(149)-M(176)+M(190)-M(200)+M(214)-M(218)+M(220)-M(224) &
    +M(236))) * den(29)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,12)
  Gcoeff = (c(2)*(M(132)-M(138)-M(143)+M(145)+M(147)-M(148)-M(149)+M(151)-M(166)+M(190)+M(212)-M(218)-M(224)+M(226)+M(236) &
    -M(242))) * den(29)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,13)
  Gcoeff = (c(2)*(-M(132)+M(135)+M(141)+M(143)-M(144)-M(146)+M(148)-M(151)+M(166)-M(176)-M(200)-M(212)+M(214)+M(220)-M(226) &
    +M(242))) * den(29)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,14)
  Gcoeff = (c(2)*(-M(135)+M(138)-M(141)+M(144)-M(145)+M(146)-M(147)+M(149)+M(176)-M(190)+M(200)-M(214)+M(218)-M(220)+M(224) &
    -M(236))) * den(29)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,15)
  Gcoeff = (c(3)*(M(132)-M(138)-M(143)+M(145)+M(147)-M(148)-M(149)+M(151)-M(166)+M(190)+M(212)-M(218)-M(224)+M(226)+M(236) &
    -M(242))) * den(29)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,16)
  Gcoeff = (c(3)*(-M(132)+M(135)+M(141)+M(143)-M(144)-M(146)+M(148)-M(151)+M(166)-M(176)-M(200)-M(212)+M(214)+M(220)-M(226) &
    +M(242))) * den(29)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,17)
  Gcoeff = (c(3)*(-M(135)+M(138)-M(141)+M(144)-M(145)+M(146)-M(147)+M(149)+M(176)-M(190)+M(200)-M(214)+M(218)-M(220)+M(224) &
    -M(236))) * den(29)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,18)
  Gcoeff = (c(1)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(2)*(-M(138)+M(145)+M(147)-M(149) &
    +M(190)-M(218)-M(224)+M(236))) * den(142)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,10)
  Gcoeff = (c(1)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(2)*(-M(132)+M(143)+M(148)-M(151) &
    +M(166)-M(212)-M(226)+M(242))) * den(142)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,11)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(142)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,12)
  Gcoeff = (c(1)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(2)*(-M(132)+M(143)+M(148)-M(151) &
    +M(166)-M(212)-M(226)+M(242))) * den(142)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,13)
  Gcoeff = (c(1)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(2)*(-M(138)+M(145)+M(147)-M(149) &
    +M(190)-M(218)-M(224)+M(236))) * den(142)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,14)
  Gcoeff = (c(2)*(M(132)-M(138)-M(143)+M(145)+M(147)-M(148)-M(149)+M(151)-M(166)+M(190)+M(212)-M(218)-M(224)+M(226)+M(236) &
    -M(242))) * den(142)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,15)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(142)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,16)
  Gcoeff = (c(2)*(M(132)-M(138)-M(143)+M(145)+M(147)-M(148)-M(149)+M(151)-M(166)+M(190)+M(212)-M(218)-M(224)+M(226)+M(236) &
    -M(242))) * den(142)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,17)
  Gcoeff = (c(3)*(M(132)-M(138)-M(143)+M(145)+M(147)-M(148)-M(149)+M(151)-M(166)+M(190)+M(212)-M(218)-M(224)+M(226)+M(236) &
    -M(242))) * den(142)
  T2sum(1:1,6) = T2sum(1:1,6) + Gcoeff * G0tensor(:,18)
  Gcoeff = (c(2)*(M(131)-M(132)-M(134)+M(136)-M(140)+M(142)+M(148)-M(154)-M(164)+M(166)+M(172)-M(178)+M(196)-M(202)-M(226) &
    +M(250))) * den(346)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,22)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(346)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,23)
  Gcoeff = (c(2)*(M(134)-M(136)-M(137)+M(138)+M(140)-M(142)-M(147)+M(153)-M(172)+M(178)+M(188)-M(190)-M(196)+M(202)+M(224) &
    -M(248))) * den(346)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,24)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(134)-M(136)+M(140)-M(142)-M(148)+M(154)+M(164)-M(166)-M(172)+M(178)-M(196)+M(202)+M(226) &
    -M(250))) * den(346)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,25)
  Gcoeff = (c(2)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(346)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,26)
  Gcoeff = (c(2)*(-M(134)+M(136)+M(137)-M(138)-M(140)+M(142)+M(147)-M(153)+M(172)-M(178)-M(188)+M(190)+M(196)-M(202)-M(224) &
    +M(248))) * den(346)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,27)
  Gcoeff = (c(3)*(-M(131)+M(132)+M(134)-M(136)+M(140)-M(142)-M(148)+M(154)+M(164)-M(166)-M(172)+M(178)-M(196)+M(202)+M(226) &
    -M(250))) * den(346)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,28)
  Gcoeff = (c(3)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(346)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,29)
  Gcoeff = (c(3)*(-M(134)+M(136)+M(137)-M(138)-M(140)+M(142)+M(147)-M(153)+M(172)-M(178)-M(188)+M(190)+M(196)-M(202)-M(224) &
    +M(248))) * den(346)
  T1sum(1:5,3) = T1sum(1:5,3) + Gcoeff * G1tensor(:,30)
  Gcoeff = (c(2)*(-M(131)+M(132)+M(137)-M(138)+M(147)-M(148)-M(153)+M(154)+M(164)-M(166)-M(188)+M(190)-M(224)+M(226)+M(248) &
    -M(250))) * den(1352)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,7)
  Gcoeff = (c(2)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(1352)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,8)
  Gcoeff = (c(3)*(M(131)-M(132)-M(137)+M(138)-M(147)+M(148)+M(153)-M(154)-M(164)+M(166)+M(188)-M(190)+M(224)-M(226)-M(248) &
    +M(250))) * den(1352)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,9)
  Gcoeff = (c(2)*(-M(132)+M(138)+M(143)-M(145)-M(147)+M(148)+M(149)-M(151)+M(166)-M(190)-M(212)+M(218)+M(224)-M(226)-M(236) &
    +M(242))) * den(143)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,19)
  Gcoeff = (c(2)*(M(132)-M(138)-M(143)+M(145)+M(147)-M(148)-M(149)+M(151)-M(166)+M(190)+M(212)-M(218)-M(224)+M(226)+M(236) &
    -M(242))) * den(143)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,20)
  Gcoeff = (c(3)*(M(132)-M(138)-M(143)+M(145)+M(147)-M(148)-M(149)+M(151)-M(166)+M(190)+M(212)-M(218)-M(224)+M(226)+M(236) &
    -M(242))) * den(143)
  T2sum(1:5,6) = T2sum(1:5,6) + Gcoeff * G1tensor(:,21)

end subroutine vamp_73

end module ol_vamp_73_ppjjjj_gggggg_1_/**/REALKIND
