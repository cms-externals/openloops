
module ol_vamp_8_ppjjjj_gggggg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_8(M)
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
  complex(REALKIND), dimension(4,1,4,27) :: G0
  complex(REALKIND), dimension(4,5,4,17) :: G1
  complex(REALKIND), dimension(4,15,4,8) :: G2
  complex(REALKIND), dimension(5,27) :: G1tensor
  complex(REALKIND), dimension(15,9) :: G2tensor
  complex(REALKIND), dimension(35,8) :: G3tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,61),wf(:,-4),G0(:,:,:,2))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-5),wf(:,-3),G0(:,:,:,3))
  call check_last_UV_W(l_switch,G0(:,:,:,3),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,1))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-3),wf(:,-5),G0(:,:,:,4))
  call check_last_UV_W(l_switch,G0(:,:,:,4),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,2))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-5),wf(:,-3),G0(:,:,:,5))
  call check_last_UV_W(l_switch,G0(:,:,:,5),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,3))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-5),wf(:,-2),G0(:,:,:,6))
  call check_last_UV_W(l_switch,G0(:,:,:,6),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,4))
  call loop_GGG_G_12(G0(:,:,:,2),wf(:,-2),wf(:,-5),G0(:,:,:,7))
  call check_last_UV_W(l_switch,G0(:,:,:,7),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,5))
  call loop_GGG_G_23(G0(:,:,:,2),wf(:,-5),wf(:,-2),G0(:,:,:,8))
  call check_last_UV_W(l_switch,G0(:,:,:,8),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,6))
  call loop_UV_W(G0(:,:,:,2),Q(:,19),wf(:,-5),Q(:,32),G1(:,:,:,1))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,1),wf(:,-3),wf(:,-2),G1tensor(:,7))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,1),wf(:,-2),wf(:,-3),G1tensor(:,8))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,1),wf(:,-3),wf(:,-2),G1tensor(:,9))
  call check_last_UV_W(l_switch,G1(:,:,:,1),Q(:,51),wf(:,62),Q(:,12),G2tensor(:,1))
  call loop_UV_W(G0(:,:,:,2),Q(:,19),wf(:,70),Q(:,36),G1(:,:,:,2))
  call check_last_UV_W(l_switch,G1(:,:,:,2),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,2))
  call loop_UV_W(G0(:,:,:,2),Q(:,19),wf(:,79),Q(:,40),G1(:,:,:,3))
  call check_last_UV_W(l_switch,G1(:,:,:,3),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,3))
  call loop_UV_W(G0(:,:,:,2),Q(:,19),wf(:,-3),Q(:,8),G1(:,:,:,4))
  call loop_UV_W(G1(:,:,:,4),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,1))
  call check_last_UV_W(l_switch,G2(:,:,:,1),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,1))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-4),wf(:,61),G0(:,:,:,9))
  call loop_GGG_G_12(G0(:,:,:,9),wf(:,-5),wf(:,-3),G0(:,:,:,10))
  call check_last_UV_W(l_switch,G0(:,:,:,10),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,10))
  call loop_GGG_G_12(G0(:,:,:,9),wf(:,-3),wf(:,-5),G0(:,:,:,11))
  call check_last_UV_W(l_switch,G0(:,:,:,11),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,11))
  call loop_GGG_G_23(G0(:,:,:,9),wf(:,-5),wf(:,-3),G0(:,:,:,12))
  call check_last_UV_W(l_switch,G0(:,:,:,12),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,12))
  call loop_GGG_G_12(G0(:,:,:,9),wf(:,-5),wf(:,-2),G0(:,:,:,13))
  call check_last_UV_W(l_switch,G0(:,:,:,13),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,13))
  call loop_GGG_G_12(G0(:,:,:,9),wf(:,-2),wf(:,-5),G0(:,:,:,14))
  call check_last_UV_W(l_switch,G0(:,:,:,14),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,14))
  call loop_GGG_G_23(G0(:,:,:,9),wf(:,-5),wf(:,-2),G0(:,:,:,15))
  call check_last_UV_W(l_switch,G0(:,:,:,15),Q(:,55),wf(:,-3),Q(:,8),G1tensor(:,15))
  call loop_UV_W(G0(:,:,:,9),Q(:,19),wf(:,-5),Q(:,32),G1(:,:,:,5))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,5),wf(:,-3),wf(:,-2),G1tensor(:,16))
  call check_last_GGG_G_12(l_switch,G1(:,:,:,5),wf(:,-2),wf(:,-3),G1tensor(:,17))
  call check_last_GGG_G_23(l_switch,G1(:,:,:,5),wf(:,-3),wf(:,-2),G1tensor(:,18))
  call check_last_UV_W(l_switch,G1(:,:,:,5),Q(:,51),wf(:,62),Q(:,12),G2tensor(:,4))
  call loop_UV_W(G0(:,:,:,9),Q(:,19),wf(:,70),Q(:,36),G1(:,:,:,6))
  call check_last_UV_W(l_switch,G1(:,:,:,6),Q(:,55),wf(:,-3),Q(:,8),G2tensor(:,5))
  call loop_UV_W(G0(:,:,:,9),Q(:,19),wf(:,79),Q(:,40),G1(:,:,:,7))
  call check_last_UV_W(l_switch,G1(:,:,:,7),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,6))
  call loop_UV_W(G0(:,:,:,9),Q(:,19),wf(:,-3),Q(:,8),G1(:,:,:,8))
  call loop_UV_W(G1(:,:,:,8),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,2))
  call check_last_UV_W(l_switch,G2(:,:,:,2),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,2))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,-3),wf(:,61),G0(:,:,:,16))
  call loop_GGG_G_12(G0(:,:,:,16),wf(:,-5),wf(:,-4),G0(:,:,:,17))
  call check_last_UV_W(l_switch,G0(:,:,:,17),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,19))
  call loop_GGG_G_12(G0(:,:,:,16),wf(:,-4),wf(:,-5),G0(:,:,:,18))
  call check_last_UV_W(l_switch,G0(:,:,:,18),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,20))
  call loop_GGG_G_23(G0(:,:,:,16),wf(:,-5),wf(:,-4),G0(:,:,:,19))
  call check_last_UV_W(l_switch,G0(:,:,:,19),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,21))
  call loop_UV_W(G0(:,:,:,16),Q(:,11),wf(:,84),Q(:,48),G1(:,:,:,9))
  call check_last_UV_W(l_switch,G1(:,:,:,9),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,7))
  call loop_UV_W(G0(:,:,:,16),Q(:,11),wf(:,-5),Q(:,32),G1(:,:,:,10))
  call loop_UV_W(G1(:,:,:,10),Q(:,43),wf(:,-4),Q(:,16),G2(:,:,:,3))
  call check_last_UV_W(l_switch,G2(:,:,:,3),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,3))
  call loop_UV_W(G0(:,:,:,16),Q(:,11),wf(:,-4),Q(:,16),G1(:,:,:,11))
  call loop_UV_W(G1(:,:,:,11),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,4))
  call check_last_UV_W(l_switch,G2(:,:,:,4),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,4))
  call loop_GGG_G_12(G0(:,:,:,1),wf(:,61),wf(:,-3),G0(:,:,:,20))
  call loop_GGG_G_12(G0(:,:,:,20),wf(:,-5),wf(:,-4),G0(:,:,:,21))
  call check_last_UV_W(l_switch,G0(:,:,:,21),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,22))
  call loop_GGG_G_12(G0(:,:,:,20),wf(:,-4),wf(:,-5),G0(:,:,:,22))
  call check_last_UV_W(l_switch,G0(:,:,:,22),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,23))
  call loop_GGG_G_23(G0(:,:,:,20),wf(:,-5),wf(:,-4),G0(:,:,:,23))
  call check_last_UV_W(l_switch,G0(:,:,:,23),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,24))
  call loop_UV_W(G0(:,:,:,20),Q(:,11),wf(:,84),Q(:,48),G1(:,:,:,12))
  call check_last_UV_W(l_switch,G1(:,:,:,12),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,8))
  call loop_UV_W(G0(:,:,:,20),Q(:,11),wf(:,-5),Q(:,32),G1(:,:,:,13))
  call loop_UV_W(G1(:,:,:,13),Q(:,43),wf(:,-4),Q(:,16),G2(:,:,:,5))
  call check_last_UV_W(l_switch,G2(:,:,:,5),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,5))
  call loop_UV_W(G0(:,:,:,20),Q(:,11),wf(:,-4),Q(:,16),G1(:,:,:,14))
  call loop_UV_W(G1(:,:,:,14),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,6))
  call check_last_UV_W(l_switch,G2(:,:,:,6),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,6))
  call loop_GGG_G_23(G0(:,:,:,1),wf(:,-3),wf(:,61),G0(:,:,:,24))
  call loop_GGG_G_12(G0(:,:,:,24),wf(:,-5),wf(:,-4),G0(:,:,:,25))
  call check_last_UV_W(l_switch,G0(:,:,:,25),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,25))
  call loop_GGG_G_12(G0(:,:,:,24),wf(:,-4),wf(:,-5),G0(:,:,:,26))
  call check_last_UV_W(l_switch,G0(:,:,:,26),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,26))
  call loop_GGG_G_23(G0(:,:,:,24),wf(:,-5),wf(:,-4),G0(:,:,:,27))
  call check_last_UV_W(l_switch,G0(:,:,:,27),Q(:,59),wf(:,-2),Q(:,4),G1tensor(:,27))
  call loop_UV_W(G0(:,:,:,24),Q(:,11),wf(:,84),Q(:,48),G1(:,:,:,15))
  call check_last_UV_W(l_switch,G1(:,:,:,15),Q(:,59),wf(:,-2),Q(:,4),G2tensor(:,9))
  call loop_UV_W(G0(:,:,:,24),Q(:,11),wf(:,-5),Q(:,32),G1(:,:,:,16))
  call loop_UV_W(G1(:,:,:,16),Q(:,43),wf(:,-4),Q(:,16),G2(:,:,:,7))
  call check_last_UV_W(l_switch,G2(:,:,:,7),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,7))
  call loop_UV_W(G0(:,:,:,24),Q(:,11),wf(:,-4),Q(:,16),G1(:,:,:,17))
  call loop_UV_W(G1(:,:,:,17),Q(:,27),wf(:,-5),Q(:,32),G2(:,:,:,8))
  call check_last_UV_W(l_switch,G2(:,:,:,8),Q(:,59),wf(:,-2),Q(:,4),G3tensor(:,8))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(4)*(-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(31)-M(32)+M(33)-M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)-M(41)-M(42) &
    -M(43)+M(44)-M(45)-M(46)+M(47)+M(48)+M(49)+M(50)+M(51)-M(52)+M(56)+M(59)+M(62)-M(68)+M(71)+M(74)-M(80)-M(83)-M(86)-M(92)-M(95) &
    +M(98))+c(6)*(-M(132)+M(148)+M(166)-M(226))) * den(11)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,1)
  Gcoeff = (c(4)*(-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42) &
    -M(43)+M(44)-M(45)+M(46)+M(47)+M(48)+M(49)+M(50)-M(51)-M(52)+M(56)+M(59)+M(62)-M(68)+M(71)-M(74)-M(80)-M(83)-M(86)-M(92)+M(95) &
    +M(98))+c(6)*(-M(135)+M(146)+M(176)-M(220))) * den(11)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,2)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(132)-M(135)+M(146)-M(148) &
    -M(166)+M(176)-M(220)+M(226))) * den(11)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,3)
  Gcoeff = (c(5)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(6)*(-M(132)+M(143)+M(148)-M(151) &
    +M(166)-M(212)-M(226)+M(242))) * den(11)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,10)
  Gcoeff = (c(5)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(6)*(-M(135)-M(141)+M(144)+M(146) &
    +M(176)+M(200)-M(214)-M(220))) * den(11)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,11)
  Gcoeff = (c(6)*(M(132)-M(135)-M(141)-M(143)+M(144)+M(146)-M(148)+M(151)-M(166)+M(176)+M(200)+M(212)-M(214)-M(220)+M(226) &
    -M(242))) * den(11)
  T3sum(1:5,17) = T3sum(1:5,17) + Gcoeff * G1tensor(:,12)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42) &
    -M(43)-M(44)-M(45)-M(46)+M(47)+M(48)-M(49)+M(50)+M(51)+M(52)-M(56)+M(59)+M(62)+M(68)+M(71)+M(74)-M(80)-M(83)+M(86)-M(92)-M(95) &
    -M(98))+c(6)*(-M(137)+M(153)+M(188)-M(248))) * den(11)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,19)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42) &
    -M(43)+M(44)+M(45)+M(46)+M(47)-M(48)-M(49)+M(50)-M(51)+M(52)+M(56)+M(59)+M(62)-M(68)-M(71)-M(74)-M(80)+M(83)+M(86)-M(92)+M(95) &
    -M(98))+c(6)*(-M(134)+M(142)+M(172)-M(202))) * den(11)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,22)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(-M(134)+M(137)+M(142)-M(153) &
    +M(172)-M(188)-M(202)+M(248))) * den(11)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,25)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42) &
    -M(43)-M(44)-M(45)-M(46)+M(47)+M(48)+M(49)+M(50)+M(51)-M(52)-M(56)+M(59)+M(62)+M(68)+M(71)+M(74)-M(80)-M(83)-M(86)-M(92)-M(95) &
    +M(98))+c(6)*(-M(138)+M(147)+M(190)-M(224))) * den(11)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,20)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)-M(41)-M(42) &
    -M(43)+M(44)+M(45)+M(46)+M(47)-M(48)+M(49)+M(50)-M(51)-M(52)+M(56)+M(59)+M(62)-M(68)-M(71)-M(74)-M(80)+M(83)-M(86)-M(92)+M(95) &
    +M(98))+c(6)*(-M(136)+M(140)+M(178)-M(196))) * den(11)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,23)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(-M(136)+M(138)+M(140)-M(147) &
    +M(178)-M(190)-M(196)+M(224))) * den(11)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,26)
  Gcoeff = (c(5)*(M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(86)+M(98))+c(6)*(M(137)-M(138)+M(147)-M(153) &
    -M(188)+M(190)-M(224)+M(248))) * den(11)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,21)
  Gcoeff = (c(5)*(M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(86)+M(98))+c(6)*(M(134)-M(136)+M(140)-M(142) &
    -M(172)+M(178)-M(196)+M(202))) * den(11)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,24)
  Gcoeff = (c(6)*(M(134)-M(136)-M(137)+M(138)+M(140)-M(142)-M(147)+M(153)-M(172)+M(178)+M(188)-M(190)-M(196)+M(202)+M(224) &
    -M(248))) * den(11)
  T3sum(1:5,18) = T3sum(1:5,18) + Gcoeff * G1tensor(:,27)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42) &
    -M(43)-M(44)-M(45)-M(46)+M(47)+M(48)+M(49)+M(50)+M(51)-M(52)-M(56)+M(59)+M(62)+M(68)+M(71)+M(74)-M(80)-M(83)-M(86)-M(92)-M(95) &
    +M(98))+c(6)*(-M(138)+M(147)+M(190)-M(224))) * den(11)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,4)
  Gcoeff = (c(4)*(M(25)-M(26)-M(27)+M(28)-M(29)+M(30)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42) &
    +M(43)-M(44)-M(45)-M(46)+M(47)+M(48)+M(49)-M(50)+M(51)-M(52)-M(56)+M(59)-M(62)+M(68)+M(71)+M(74)-M(80)-M(83)-M(86)+M(92)-M(95) &
    +M(98))+c(6)*(-M(141)+M(144)+M(200)-M(214))) * den(11)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,5)
  Gcoeff = (c(5)*(-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62)+M(92))+c(6)*(M(138)-M(141)+M(144)-M(147) &
    -M(190)+M(200)-M(214)+M(224))) * den(11)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,6)
  Gcoeff = (c(5)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(6)*(-M(138)+M(145)+M(147)-M(149) &
    +M(190)-M(218)-M(224)+M(236))) * den(11)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,13)
  Gcoeff = (c(5)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(6)*(-M(135)-M(141)+M(144)+M(146) &
    +M(176)+M(200)-M(214)-M(220))) * den(11)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,14)
  Gcoeff = (c(6)*(-M(135)+M(138)-M(141)+M(144)-M(145)+M(146)-M(147)+M(149)+M(176)-M(190)+M(200)-M(214)+M(218)-M(220)+M(224) &
    -M(236))) * den(11)
  T3sum(1:5,20) = T3sum(1:5,20) + Gcoeff * G1tensor(:,15)
  Gcoeff = (c(4)*(-M(25)+M(26)-M(27)+M(28)+M(29)-M(30)+M(31)-M(32)+M(33)-M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)-M(41)-M(42) &
    -M(43)+M(44)-M(45)-M(46)+M(47)+M(48)+M(49)+M(50)+M(51)-M(52)+M(56)+M(59)+M(62)-M(68)+M(71)+M(74)-M(80)-M(83)-M(86)-M(92)-M(95) &
    +M(98))+c(6)*(-M(132)+M(148)+M(166)-M(226))) * den(11)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,7)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42) &
    -M(43)-M(44)-M(45)-M(46)+M(47)+M(48)+M(49)+M(50)+M(51)-M(52)-M(56)+M(59)+M(62)+M(68)+M(71)+M(74)-M(80)-M(83)-M(86)-M(92)-M(95) &
    +M(98))+c(6)*(-M(138)+M(147)+M(190)-M(224))) * den(11)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,8)
  Gcoeff = (c(5)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56)+M(68))+c(6)*(M(132)-M(138)+M(147)-M(148) &
    -M(166)+M(190)-M(224)+M(226))) * den(11)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,9)
  Gcoeff = (c(5)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(6)*(-M(132)+M(143)+M(148)-M(151) &
    +M(166)-M(212)-M(226)+M(242))) * den(11)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,16)
  Gcoeff = (c(5)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(6)*(-M(138)+M(145)+M(147)-M(149) &
    +M(190)-M(218)-M(224)+M(236))) * den(11)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,17)
  Gcoeff = (c(6)*(M(132)-M(138)-M(143)+M(145)+M(147)-M(148)-M(149)+M(151)-M(166)+M(190)+M(212)-M(218)-M(224)+M(226)+M(236) &
    -M(242))) * den(11)
  T3sum(1:5,23) = T3sum(1:5,23) + Gcoeff * G1tensor(:,18)
  Gcoeff = (c(5)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(33)+M(34)+M(41)-M(44)-M(56)+M(68))+c(6)*(M(132)-M(138)+M(147)-M(148) &
    -M(166)+M(190)-M(224)+M(226))) * den(13)
  T3sum(1:15,23) = T3sum(1:15,23) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(6)*(M(132)-M(138)-M(143)+M(145)+M(147)-M(148)-M(149)+M(151)-M(166)+M(190)+M(212)-M(218)-M(224)+M(226)+M(236) &
    -M(242))) * den(13)
  T3sum(1:15,23) = T3sum(1:15,23) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(5)*(-M(27)+M(28)-M(31)+M(32)+M(33)-M(34)+M(35)-M(36)+M(43)-M(50)-M(62)+M(92))+c(6)*(M(138)-M(141)+M(144)-M(147) &
    -M(190)+M(200)-M(214)+M(224))) * den(17)
  T3sum(1:15,20) = T3sum(1:15,20) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(6)*(-M(135)+M(138)-M(141)+M(144)-M(145)+M(146)-M(147)+M(149)+M(176)-M(190)+M(200)-M(214)+M(218)-M(220)+M(224) &
    -M(236))) * den(17)
  T3sum(1:15,20) = T3sum(1:15,20) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(5)*(M(27)-M(28)-M(33)+M(34)-M(37)+M(38)+M(39)-M(40)+M(46)-M(51)-M(74)+M(95))+c(6)*(M(132)-M(135)+M(146)-M(148) &
    -M(166)+M(176)-M(220)+M(226))) * den(23)
  T3sum(1:15,17) = T3sum(1:15,17) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(6)*(M(132)-M(135)-M(141)-M(143)+M(144)+M(146)-M(148)+M(151)-M(166)+M(176)+M(200)+M(212)-M(214)-M(220)+M(226) &
    -M(242))) * den(23)
  T3sum(1:15,17) = T3sum(1:15,17) + Gcoeff * G2tensor(:,6)
  Gcoeff = (c(4)*(-M(25)+M(26)+M(27)-M(28)+M(29)-M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42) &
    -M(43)+M(44)-M(45)+M(46)+M(47)+M(48)+M(49)+M(50)-M(51)-M(52)+M(56)+M(59)+M(62)-M(68)+M(71)-M(74)-M(80)-M(83)-M(86)-M(92)+M(95) &
    +M(98))+c(6)*(-M(135)+M(146)+M(176)-M(220))) * den(11)
  T4sum(1:35,141) = T4sum(1:35,141) + Gcoeff * G3tensor(:,1)
  Gcoeff = (c(5)*(-M(42)-M(45)+M(47)+M(48)+M(49)-M(52)+M(59)+M(71)-M(80)-M(83)-M(86)+M(98))+c(6)*(-M(135)-M(141)+M(144)+M(146) &
    +M(176)+M(200)-M(214)-M(220))) * den(11)
  T4sum(1:35,141) = T4sum(1:35,141) + Gcoeff * G3tensor(:,2)
  Gcoeff = (c(5)*(M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(86)+M(98))+c(6)*(M(137)-M(138)+M(147)-M(153) &
    -M(188)+M(190)-M(224)+M(248))) * den(27)
  T3sum(1:15,18) = T3sum(1:15,18) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(5)*(M(31)-M(32)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(49)-M(52)-M(86)+M(98))+c(6)*(M(134)-M(136)+M(140)-M(142) &
    -M(172)+M(178)-M(196)+M(202))) * den(27)
  T3sum(1:15,18) = T3sum(1:15,18) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(6)*(M(134)-M(136)-M(137)+M(138)+M(140)-M(142)-M(147)+M(153)-M(172)+M(178)+M(188)-M(190)-M(196)+M(202)+M(224) &
    -M(248))) * den(27)
  T3sum(1:15,18) = T3sum(1:15,18) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)+M(41)-M(42) &
    -M(43)-M(44)-M(45)-M(46)+M(47)+M(48)-M(49)+M(50)+M(51)+M(52)-M(56)+M(59)+M(62)+M(68)+M(71)+M(74)-M(80)-M(83)+M(86)-M(92)-M(95) &
    -M(98))+c(6)*(-M(137)+M(153)+M(188)-M(248))) * den(11)
  T4sum(1:35,143) = T4sum(1:35,143) + Gcoeff * G3tensor(:,3)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)-M(31)+M(32)-M(33)+M(34)+M(35)-M(36)-M(37)+M(38)+M(39)-M(40)-M(41)-M(42) &
    -M(43)+M(44)+M(45)+M(46)+M(47)-M(48)-M(49)+M(50)-M(51)+M(52)+M(56)+M(59)+M(62)-M(68)-M(71)-M(74)-M(80)+M(83)+M(86)-M(92)+M(95) &
    -M(98))+c(6)*(-M(134)+M(142)+M(172)-M(202))) * den(11)
  T4sum(1:35,143) = T4sum(1:35,143) + Gcoeff * G3tensor(:,5)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(-M(134)+M(137)+M(142)-M(153) &
    +M(172)-M(188)-M(202)+M(248))) * den(11)
  T4sum(1:35,143) = T4sum(1:35,143) + Gcoeff * G3tensor(:,7)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)+M(41)-M(42) &
    -M(43)-M(44)-M(45)-M(46)+M(47)+M(48)+M(49)+M(50)+M(51)-M(52)-M(56)+M(59)+M(62)+M(68)+M(71)+M(74)-M(80)-M(83)-M(86)-M(92)-M(95) &
    +M(98))+c(6)*(-M(138)+M(147)+M(190)-M(224))) * den(11)
  T4sum(1:35,144) = T4sum(1:35,144) + Gcoeff * G3tensor(:,4)
  Gcoeff = (c(4)*(M(25)-M(26)+M(27)-M(28)-M(29)+M(30)+M(31)-M(32)-M(33)+M(34)-M(35)+M(36)+M(37)-M(38)-M(39)+M(40)-M(41)-M(42) &
    -M(43)+M(44)+M(45)+M(46)+M(47)-M(48)+M(49)+M(50)-M(51)-M(52)+M(56)+M(59)+M(62)-M(68)-M(71)-M(74)-M(80)+M(83)-M(86)-M(92)+M(95) &
    +M(98))+c(6)*(-M(136)+M(140)+M(178)-M(196))) * den(11)
  T4sum(1:35,144) = T4sum(1:35,144) + Gcoeff * G3tensor(:,6)
  Gcoeff = (c(5)*(-M(41)+M(44)+M(45)+M(46)-M(48)-M(51)+M(56)-M(68)-M(71)-M(74)+M(83)+M(95))+c(6)*(-M(136)+M(138)+M(140)-M(147) &
    +M(178)-M(190)-M(196)+M(224))) * den(11)
  T4sum(1:35,144) = T4sum(1:35,144) + Gcoeff * G3tensor(:,8)

end subroutine vamp_8

end module ol_vamp_8_ppjjjj_gggggg_1_/**/REALKIND
