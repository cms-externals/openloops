
module ol_colourmatrix_ppllajj_eexddxbbxag_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(152,4)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1(  1,:) = [  36,  12,  12,   0]
  K1(  2,:) = [  12,  36,   0,  12]
  K1(  3,:) = [  12,   0,  36,  12]
  K1(  4,:) = [   0,  12,  12,  36]
  K1(  5,:) = [   0,   0,   0,   0]
  K1(  6,:) = [   0,   0,   0,   0]
  K1(  7,:) = [   0,   0,   0,   0]
  K1(  8,:) = [   0,   0,   0,   0]
  K1(  9,:) = [   0,   0,   0,   0]
  K1( 10,:) = [   0,   0,   0,   0]
  K1( 11,:) = [   0,   0,   0,   0]
  K1( 12,:) = [   0,   0,   0,   0]
  K1( 13,:) = [   0,   0,   0,   0]
  K1( 14,:) = [   0,   0,   0,   0]
  K1( 15,:) = [   0,   0,   0,   0]
  K1( 16,:) = [   0,   0,   0,   0]
  K1( 17,:) = [   0,   0,   0,   0]
  K1( 18,:) = [   0,   0,   0,   0]
  K1( 19,:) = [   0,   0,   0,   0]
  K1( 20,:) = [   0,   0,   0,   0]
  K1( 21,:) = [   0,   0,   0,   0]
  K1( 22,:) = [   0,   0,   0,   0]
  K1( 23,:) = [   0,   0,   0,   0]
  K1( 24,:) = [   0,   0,   0,   0]
  K1( 25,:) = [  48,  16,  16,   0]
  K1( 26,:) = [  16,  48,   0,  16]
  K1( 27,:) = [  16,   0,  48,  16]
  K1( 28,:) = [   0,  16,  16,  48]
  K1( 29,:) = [   0,   0,   0,   0]
  K1( 30,:) = [   0,   0,   0,   0]
  K1( 31,:) = [   0,   0,   0,   0]
  K1( 32,:) = [   0,   0,   0,   0]
  K1( 33,:) = [   0,   0,   0,   0]
  K1( 34,:) = [   0,   0,   0,   0]
  K1( 35,:) = [   0,   0,   0,   0]
  K1( 36,:) = [   0,   0,   0,   0]
  K1( 37,:) = [   6,   2,   2,   0]
  K1( 38,:) = [   2,   0,  -6, -16]
  K1( 39,:) = [   2,  -6,   0, -16]
  K1( 40,:) = [   0, -16, -16, -48]
  K1( 41,:) = [  48,  16,  16,   0]
  K1( 42,:) = [  16,  48,   0,  16]
  K1( 43,:) = [  16,   0,  48,  16]
  K1( 44,:) = [   0,  16,  16,  48]
  K1( 45,:) = [   0,   0,   0,   0]
  K1( 46,:) = [   0,   0,   0,   0]
  K1( 47,:) = [   0,   0,   0,   0]
  K1( 48,:) = [   0,   0,   0,   0]
  K1( 49,:) = [   0,   0,   0,   0]
  K1( 50,:) = [   0,   0,   0,   0]
  K1( 51,:) = [   0,   0,   0,   0]
  K1( 52,:) = [   0,   0,   0,   0]
  K1( 53,:) = [   0,  16,  -2,   6]
  K1( 54,:) = [  16,   0,   6,  -2]
  K1( 55,:) = [  -2,   6,   0,  16]
  K1( 56,:) = [   6,  -2,  16,   0]
  K1( 57,:) = [   0,   2, -16,  -6]
  K1( 58,:) = [   2,   6,   0,   2]
  K1( 59,:) = [ -16,   0, -48, -16]
  K1( 60,:) = [  -6,   2, -16,   0]
  K1( 61,:) = [  48,  16,  16,   0]
  K1( 62,:) = [  16,  48,   0,  16]
  K1( 63,:) = [  16,   0,  48,  16]
  K1( 64,:) = [   0,  16,  16,  48]
  K1( 65,:) = [   0,   0,   0,   0]
  K1( 66,:) = [   0,   0,   0,   0]
  K1( 67,:) = [   0,   0,   0,   0]
  K1( 68,:) = [   0,   0,   0,   0]
  K1( 69,:) = [   0,   0,   0,   0]
  K1( 70,:) = [   0,   0,   0,   0]
  K1( 71,:) = [   0,   0,   0,   0]
  K1( 72,:) = [   0,   0,   0,   0]
  K1( 73,:) = [   0, -16,   2,  -6]
  K1( 74,:) = [ -16, -48,   0, -16]
  K1( 75,:) = [   2,   0,   6,   2]
  K1( 76,:) = [  -6, -16,   2,   0]
  K1( 77,:) = [   0,  -2,  16,   6]
  K1( 78,:) = [  -2,   0,   6,  16]
  K1( 79,:) = [  16,   6,   0,  -2]
  K1( 80,:) = [   6,  16,  -2,   0]
  K1( 81,:) = [ -48, -16, -16,   0]
  K1( 82,:) = [ -16,   0,  -6,   2]
  K1( 83,:) = [ -16,  -6,   0,   2]
  K1( 84,:) = [   0,   2,   2,   6]
  K1( 85,:) = [  48,  16,  16,   0]
  K1( 86,:) = [  16,  48,   0,  16]
  K1( 87,:) = [  16,   0,  48,  16]
  K1( 88,:) = [   0,  16,  16,  48]
  K1( 89,:) = [   0,   0,   0,   0]
  K1( 90,:) = [   0,   0,   0,   0]
  K1( 91,:) = [   0,   0,   0,   0]
  K1( 92,:) = [   0,   0,   0,   0]
  K1( 93,:) = [   0,   0,   0,   0]
  K1( 94,:) = [   0,   0,   0,   0]
  K1( 95,:) = [   0,   0,   0,   0]
  K1( 96,:) = [   0,   0,   0,   0]
  K1( 97,:) = [   0,   0,   0,   0]
  K1( 98,:) = [   0,   0,   0,   0]
  K1( 99,:) = [   0,   0,   0,   0]
  K1(100,:) = [   0,   0,   0,   0]
  K1(101,:) = [   0,   0,   0,   0]
  K1(102,:) = [   0,   0,   0,   0]
  K1(103,:) = [   0,   0,   0,   0]
  K1(104,:) = [   0,   0,   0,   0]
  K1(105,:) = [   0,   0,   0,   0]
  K1(106,:) = [   0,   0,   0,   0]
  K1(107,:) = [   0,   0,   0,   0]
  K1(108,:) = [   0,   0,   0,   0]
  K1(109,:) = [   0,   0,   0,   0]
  K1(110,:) = [   0,   0,   0,   0]
  K1(111,:) = [   0,   0,   0,   0]
  K1(112,:) = [   0,   0,   0,   0]
  K1(113,:) = [   0,   0,   0,   0]
  K1(114,:) = [   0,   0,   0,   0]
  K1(115,:) = [   0,   0,   0,   0]
  K1(116,:) = [   0,   0,   0,   0]
  K1(117,:) = [   0,   0,   0,   0]
  K1(118,:) = [   0,   0,   0,   0]
  K1(119,:) = [   0,   0,   0,   0]
  K1(120,:) = [   0,   0,   0,   0]
  K1(121,:) = [   0,   0,   0,   0]
  K1(122,:) = [   0,   0,   0,   0]
  K1(123,:) = [   0,   0,   0,   0]
  K1(124,:) = [   0,   0,   0,   0]
  K1(125,:) = [ -54, -18, -18,   0]
  K1(126,:) = [ -18,   0,   0,  18]
  K1(127,:) = [ -18,   0, -54, -18]
  K1(128,:) = [   0,  18, -18,   0]
  K1(129,:) = [ -54, -18, -18,   0]
  K1(130,:) = [ -18, -54,   0, -18]
  K1(131,:) = [ -18,   0,   0,  18]
  K1(132,:) = [   0, -18,  18,   0]
  K1(133,:) = [   0, -18,  18,   0]
  K1(134,:) = [ -18, -54,   0, -18]
  K1(135,:) = [  18,   0,   0, -18]
  K1(136,:) = [   0, -18, -18, -54]
  K1(137,:) = [   0,  18, -18,   0]
  K1(138,:) = [  18,   0,   0, -18]
  K1(139,:) = [ -18,   0, -54, -18]
  K1(140,:) = [   0, -18, -18, -54]
  K1(141,:) = [   0,   0,   0,   0]
  K1(142,:) = [   0,   0,   0,   0]
  K1(143,:) = [   0,   0,   0,   0]
  K1(144,:) = [   0,   0,   0,   0]
  K1(145,:) = [ 108,  36,  36,   0]
  K1(146,:) = [  36, 108,   0,  36]
  K1(147,:) = [  36,   0, 108,  36]
  K1(148,:) = [   0,  36,  36, 108]
  K1(149,:) = [   0,   0,   0,   0]
  K1(150,:) = [   0,   0,   0,   0]
  K1(151,:) = [   0,   0,   0,   0]
  K1(152,:) = [   0,   0,   0,   0]
  K1 = (1._/**/REALKIND / 3) * K1

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllajj_eexddxbbxag_1_/**/REALKIND



module ol_forced_parameters_ppllajj_eexddxbbxag_1_/**/REALKIND
  implicit none
  contains
  subroutine check_forced_parameters
    use ol_parameters_decl_/**/REALKIND
    use ol_loop_parameters_decl_/**/REALKIND
#ifndef PRECISION_dp
    use ol_loop_parameters_decl_/**/DREALKIND, only: LeadingColour, nc, nf
#endif
    implicit none
    logical, save :: checks_not_written = .true.

    if (checks_not_written) then
    ! e.g.
    ! if (ME /= 0) write(*,101) 'ME = 0'
  if (ME /= 0) write(*,101) 'ME = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppllajj_eexddxbbxag_1_/**/REALKIND

module ol_tree_ppllajj_eexddxbbxag_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(6)
  complex(REALKIND), save :: den(634)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 256 ! number of helicity configurations
  integer(intkind2), save :: nhel = 256 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(256) ! physical helicity states
  complex(DREALKIND) :: M1helarr(4,256) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = (CI*eQED**3*gQCD**3)/9._/**/REALKIND
    f(2) = (CI*eQED**3*gQCD**3)/3._/**/REALKIND
    f(3) = CI*eQED**3*gQCD**3
    f(4) = (eQED**3*gQCD**3)/9._/**/REALKIND
    f(5) = (eQED**3*gQCD**3)/3._/**/REALKIND
    f(6) = eQED**3*gQCD**3

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,3))
  den(2) = 1 / (Q(5,80) - MB2)
  den(3) = 1 / (Q(5,12))
  den(4) = 1 / (Q(5,160) - MB2)
  den(7) = 1 / (Q(5,83) - MB2)
  den(10) = 1 / (Q(5,3) - MZ2)
  den(16) = 1 / (Q(5,163) - MB2)
  den(22) = 1 / (Q(5,35) - MB2)
  den(24) = 1 / (Q(5,140))
  den(31) = 1 / (Q(5,208) - MB2)
  den(37) = 1 / (Q(5,92) - MB2)
  den(41) = 1 / (Q(5,44) - MB2)
  den(51) = 1 / (Q(5,144) - MB2)
  den(52) = 1 / (Q(5,96) - MB2)
  den(55) = 1 / (Q(5,147) - MB2)
  den(63) = 1 / (Q(5,99) - MB2)
  den(69) = 1 / (Q(5,19) - MB2)
  den(76) = 1 / (Q(5,108) - MB2)
  den(80) = 1 / (Q(5,224) - MB2)
  den(86) = 1 / (Q(5,28) - MB2)
  den(99) = 1 / (Q(5,156) - MB2)
  den(107) = 1 / (Q(5,172) - MB2)
  den(138) = 1 / (Q(5,68))
  den(139) = 1 / (Q(5,136))
  den(140) = 1 / (Q(5,48))
  den(143) = 1 / (Q(5,71))
  den(151) = 1 / (Q(5,139))
  den(157) = 1 / (Q(5,11))
  den(159) = 1 / (Q(5,196))
  den(166) = 1 / (Q(5,116))
  den(170) = 1 / (Q(5,176))
  den(176) = 1 / (Q(5,56))
  den(186) = 1 / (Q(5,132))
  den(187) = 1 / (Q(5,72))
  den(190) = 1 / (Q(5,135))
  den(198) = 1 / (Q(5,75))
  den(204) = 1 / (Q(5,7))
  den(206) = 1 / (Q(5,120))
  den(211) = 1 / (Q(5,200))
  den(221) = 1 / (Q(5,52))
  den(234) = 1 / (Q(5,180))
  den(242) = 1 / (Q(5,184))
  den(274) = 1 / (Q(5,204))
  den(283) = 1 / (Q(5,76))
  den(306) = 1 / (Q(5,51))
  den(364) = 1 / (Q(5,112))
  den(391) = 1 / (Q(5,240))
  den(397) = 1 / (Q(5,15))
  den(499) = 1 / (Q(5,65))
  den(500) = 1 / (Q(5,67))
  den(503) = 1 / (Q(5,67) - MZ2)
  den(528) = 1 / (Q(5,66))

  ! denominators

  den(5) = den(1)*den(2)
  den(6) = den(3)*den(4)
  den(8) = den(5)*den(7)
  den(9) = den(6)*den(8)
  den(11) = den(2)*den(10)
  den(12) = den(7)*den(11)
  den(13) = den(6)*den(12)
  den(14) = den(1)*den(4)
  den(15) = den(2)*den(3)
  den(17) = den(14)*den(16)
  den(18) = den(15)*den(17)
  den(19) = den(4)*den(10)
  den(20) = den(16)*den(19)
  den(21) = den(15)*den(20)
  den(23) = den(1)*den(22)
  den(25) = den(3)*den(24)
  den(26) = den(2)*den(23)
  den(27) = den(25)*den(26)
  den(28) = den(10)*den(22)
  den(29) = den(2)*den(28)
  den(30) = den(25)*den(29)
  den(32) = den(2)*den(31)
  den(33) = den(3)*den(23)
  den(34) = den(32)*den(33)
  den(35) = den(3)*den(28)
  den(36) = den(32)*den(35)
  den(38) = den(15)*den(37)
  den(39) = den(23)*den(38)
  den(40) = den(28)*den(38)
  den(42) = den(3)*den(41)
  den(43) = den(1)*den(42)
  den(44) = den(32)*den(43)
  den(45) = den(10)*den(42)
  den(46) = den(32)*den(45)
  den(47) = den(8)*den(42)
  den(48) = den(12)*den(42)
  den(49) = den(8)*den(25)
  den(50) = den(12)*den(25)
  den(53) = den(1)*den(51)
  den(54) = den(3)*den(52)
  den(56) = den(53)*den(55)
  den(57) = den(54)*den(56)
  den(58) = den(10)*den(51)
  den(59) = den(55)*den(58)
  den(60) = den(54)*den(59)
  den(61) = den(1)*den(52)
  den(62) = den(3)*den(51)
  den(64) = den(61)*den(63)
  den(65) = den(62)*den(64)
  den(66) = den(10)*den(52)
  den(67) = den(63)*den(66)
  den(68) = den(62)*den(67)
  den(70) = den(1)*den(69)
  den(71) = den(52)*den(70)
  den(72) = den(25)*den(71)
  den(73) = den(10)*den(69)
  den(74) = den(52)*den(73)
  den(75) = den(25)*den(74)
  den(77) = den(54)*den(76)
  den(78) = den(70)*den(77)
  den(79) = den(73)*den(77)
  den(81) = den(52)*den(80)
  den(82) = den(3)*den(70)
  den(83) = den(81)*den(82)
  den(84) = den(3)*den(73)
  den(85) = den(81)*den(84)
  den(87) = den(3)*den(86)
  den(88) = den(64)*den(87)
  den(89) = den(67)*den(87)
  den(90) = den(1)*den(87)
  den(91) = den(81)*den(90)
  den(92) = den(10)*den(87)
  den(93) = den(81)*den(92)
  den(94) = den(25)*den(64)
  den(95) = den(25)*den(67)
  den(96) = den(31)*den(51)
  den(97) = den(33)*den(96)
  den(98) = den(35)*den(96)
  den(100) = den(62)*den(99)
  den(101) = den(23)*den(100)
  den(102) = den(28)*den(100)
  den(103) = den(43)*den(96)
  den(104) = den(45)*den(96)
  den(105) = den(42)*den(56)
  den(106) = den(42)*den(59)
  den(108) = den(6)*den(107)
  den(109) = den(70)*den(108)
  den(110) = den(73)*den(108)
  den(111) = den(4)*den(80)
  den(112) = den(82)*den(111)
  den(113) = den(84)*den(111)
  den(114) = den(17)*den(87)
  den(115) = den(20)*den(87)
  den(116) = den(90)*den(111)
  den(117) = den(92)*den(111)
  den(118) = den(7)*den(70)
  den(119) = den(42)*den(118)
  den(120) = den(7)*den(73)
  den(121) = den(42)*den(120)
  den(122) = den(42)*den(76)
  den(123) = den(70)*den(122)
  den(124) = den(73)*den(122)
  den(125) = den(25)*den(107)
  den(126) = den(70)*den(125)
  den(127) = den(73)*den(125)
  den(128) = den(37)*den(87)
  den(129) = den(23)*den(128)
  den(130) = den(28)*den(128)
  den(131) = den(23)*den(63)
  den(132) = den(87)*den(131)
  den(133) = den(28)*den(63)
  den(134) = den(87)*den(133)
  den(135) = den(25)*den(99)
  den(136) = den(23)*den(135)
  den(137) = den(28)*den(135)
  den(141) = den(1)*den(138)
  den(142) = den(139)*den(140)
  den(144) = den(141)*den(143)
  den(145) = den(142)*den(144)
  den(146) = den(10)*den(138)
  den(147) = den(143)*den(146)
  den(148) = den(142)*den(147)
  den(149) = den(1)*den(139)
  den(150) = den(138)*den(140)
  den(152) = den(149)*den(151)
  den(153) = den(150)*den(152)
  den(154) = den(10)*den(139)
  den(155) = den(151)*den(154)
  den(156) = den(150)*den(155)
  den(158) = den(1)*den(157)
  den(160) = den(138)*den(159)
  den(161) = den(140)*den(158)
  den(162) = den(160)*den(161)
  den(163) = den(10)*den(157)
  den(164) = den(140)*den(163)
  den(165) = den(160)*den(164)
  den(167) = den(150)*den(166)
  den(168) = den(158)*den(167)
  den(169) = den(163)*den(167)
  den(171) = den(140)*den(170)
  den(172) = den(138)*den(158)
  den(173) = den(171)*den(172)
  den(174) = den(138)*den(163)
  den(175) = den(171)*den(174)
  den(177) = den(140)*den(176)
  den(178) = den(1)*den(177)
  den(179) = den(160)*den(178)
  den(180) = den(10)*den(177)
  den(181) = den(160)*den(180)
  den(182) = den(144)*den(177)
  den(183) = den(147)*den(177)
  den(184) = den(144)*den(171)
  den(185) = den(147)*den(171)
  den(188) = den(1)*den(186)
  den(189) = den(140)*den(187)
  den(191) = den(188)*den(190)
  den(192) = den(189)*den(191)
  den(193) = den(10)*den(186)
  den(194) = den(190)*den(193)
  den(195) = den(189)*den(194)
  den(196) = den(1)*den(187)
  den(197) = den(140)*den(186)
  den(199) = den(196)*den(198)
  den(200) = den(197)*den(199)
  den(201) = den(10)*den(187)
  den(202) = den(198)*den(201)
  den(203) = den(197)*den(202)
  den(205) = den(1)*den(204)
  den(207) = den(189)*den(206)
  den(208) = den(205)*den(207)
  den(209) = den(10)*den(204)
  den(210) = den(207)*den(209)
  den(212) = den(187)*den(211)
  den(213) = den(140)*den(205)
  den(214) = den(212)*den(213)
  den(215) = den(140)*den(209)
  den(216) = den(212)*den(215)
  den(217) = den(187)*den(205)
  den(218) = den(171)*den(217)
  den(219) = den(187)*den(209)
  den(220) = den(171)*den(219)
  den(222) = den(140)*den(221)
  den(223) = den(199)*den(222)
  den(224) = den(202)*den(222)
  den(225) = den(1)*den(222)
  den(226) = den(212)*den(225)
  den(227) = den(10)*den(222)
  den(228) = den(212)*den(227)
  den(229) = den(171)*den(199)
  den(230) = den(171)*den(202)
  den(231) = den(159)*den(186)
  den(232) = den(161)*den(231)
  den(233) = den(164)*den(231)
  den(235) = den(197)*den(234)
  den(236) = den(158)*den(235)
  den(237) = den(163)*den(235)
  den(238) = den(178)*den(231)
  den(239) = den(180)*den(231)
  den(240) = den(177)*den(191)
  den(241) = den(177)*den(194)
  den(243) = den(142)*den(242)
  den(244) = den(205)*den(243)
  den(245) = den(209)*den(243)
  den(246) = den(139)*den(211)
  den(247) = den(213)*den(246)
  den(248) = den(215)*den(246)
  den(249) = den(152)*den(222)
  den(250) = den(155)*den(222)
  den(251) = den(225)*den(246)
  den(252) = den(227)*den(246)
  den(253) = den(143)*den(205)
  den(254) = den(177)*den(253)
  den(255) = den(143)*den(209)
  den(256) = den(177)*den(255)
  den(257) = den(171)*den(242)
  den(258) = den(205)*den(257)
  den(259) = den(209)*den(257)
  den(260) = den(177)*den(206)
  den(261) = den(205)*den(260)
  den(262) = den(209)*den(260)
  den(263) = den(166)*den(222)
  den(264) = den(158)*den(263)
  den(265) = den(163)*den(263)
  den(266) = den(158)*den(198)
  den(267) = den(222)*den(266)
  den(268) = den(163)*den(198)
  den(269) = den(222)*den(268)
  den(270) = den(171)*den(234)
  den(271) = den(158)*den(270)
  den(272) = den(163)*den(270)
  den(273) = den(138)*den(139)
  den(275) = den(273)*den(274)
  den(276) = den(70)*den(275)
  den(277) = den(73)*den(275)
  den(278) = den(23)*den(275)
  den(279) = den(28)*den(275)
  den(280) = den(51)*den(170)
  den(281) = den(172)*den(280)
  den(282) = den(174)*den(280)
  den(284) = den(138)*den(283)
  den(285) = den(51)*den(284)
  den(286) = den(23)*den(285)
  den(287) = den(28)*den(285)
  den(288) = den(56)*den(284)
  den(289) = den(59)*den(284)
  den(290) = den(144)*den(280)
  den(291) = den(147)*den(280)
  den(292) = den(4)*den(170)
  den(293) = den(172)*den(292)
  den(294) = den(174)*den(292)
  den(295) = den(70)*den(284)
  den(296) = den(4)*den(295)
  den(297) = den(73)*den(284)
  den(298) = den(4)*den(297)
  den(299) = den(17)*den(284)
  den(300) = den(20)*den(284)
  den(301) = den(144)*den(292)
  den(302) = den(147)*den(292)
  den(303) = den(76)*den(284)
  den(304) = den(70)*den(303)
  den(305) = den(73)*den(303)
  den(307) = den(70)*den(306)
  den(308) = den(284)*den(307)
  den(309) = den(73)*den(306)
  den(310) = den(284)*den(309)
  den(311) = den(160)*den(274)
  den(312) = den(70)*den(311)
  den(313) = den(73)*den(311)
  den(314) = den(37)*den(284)
  den(315) = den(23)*den(314)
  den(316) = den(28)*den(314)
  den(317) = den(23)*den(306)
  den(318) = den(284)*den(317)
  den(319) = den(28)*den(306)
  den(320) = den(284)*den(319)
  den(321) = den(23)*den(311)
  den(322) = den(28)*den(311)
  den(323) = den(186)*den(187)
  den(324) = den(274)*den(323)
  den(325) = den(70)*den(324)
  den(326) = den(73)*den(324)
  den(327) = den(23)*den(324)
  den(328) = den(28)*den(324)
  den(329) = den(217)*den(280)
  den(330) = den(219)*den(280)
  den(331) = den(187)*den(283)
  den(332) = den(51)*den(331)
  den(333) = den(23)*den(332)
  den(334) = den(28)*den(332)
  den(335) = den(56)*den(331)
  den(336) = den(59)*den(331)
  den(337) = den(199)*den(280)
  den(338) = den(202)*den(280)
  den(339) = den(217)*den(292)
  den(340) = den(219)*den(292)
  den(341) = den(70)*den(331)
  den(342) = den(4)*den(341)
  den(343) = den(73)*den(331)
  den(344) = den(4)*den(343)
  den(345) = den(17)*den(331)
  den(346) = den(20)*den(331)
  den(347) = den(199)*den(292)
  den(348) = den(202)*den(292)
  den(349) = den(76)*den(331)
  den(350) = den(70)*den(349)
  den(351) = den(73)*den(349)
  den(352) = den(307)*den(331)
  den(353) = den(309)*den(331)
  den(354) = den(212)*den(274)
  den(355) = den(70)*den(354)
  den(356) = den(73)*den(354)
  den(357) = den(37)*den(331)
  den(358) = den(23)*den(357)
  den(359) = den(28)*den(357)
  den(360) = den(317)*den(331)
  den(361) = den(319)*den(331)
  den(362) = den(23)*den(354)
  den(363) = den(28)*den(354)
  den(365) = den(2)*den(364)
  den(366) = den(158)*den(186)
  den(367) = den(365)*den(366)
  den(368) = den(163)*den(186)
  den(369) = den(365)*den(368)
  den(370) = den(24)*den(186)
  den(371) = den(2)*den(370)
  den(372) = den(23)*den(371)
  den(373) = den(28)*den(371)
  den(374) = den(8)*den(370)
  den(375) = den(12)*den(370)
  den(376) = den(191)*den(365)
  den(377) = den(194)*den(365)
  den(378) = den(139)*den(205)
  den(379) = den(365)*den(378)
  den(380) = den(139)*den(209)
  den(381) = den(365)*den(380)
  den(382) = den(24)*den(139)
  den(383) = den(2)*den(382)
  den(384) = den(23)*den(383)
  den(385) = den(28)*den(383)
  den(386) = den(8)*den(382)
  den(387) = den(12)*den(382)
  den(388) = den(152)*den(365)
  den(389) = den(155)*den(365)
  den(390) = den(2)*den(4)
  den(392) = den(390)*den(391)
  den(393) = den(205)*den(392)
  den(394) = den(209)*den(392)
  den(395) = den(158)*den(392)
  den(396) = den(163)*den(392)
  den(398) = den(205)*den(397)
  den(399) = den(32)*den(398)
  den(400) = den(209)*den(397)
  den(401) = den(32)*den(400)
  den(402) = den(365)*den(398)
  den(403) = den(365)*den(400)
  den(404) = den(206)*den(365)
  den(405) = den(205)*den(404)
  den(406) = den(209)*den(404)
  den(407) = den(158)*den(397)
  den(408) = den(32)*den(407)
  den(409) = den(163)*den(397)
  den(410) = den(32)*den(409)
  den(411) = den(365)*den(407)
  den(412) = den(365)*den(409)
  den(413) = den(166)*den(365)
  den(414) = den(158)*den(413)
  den(415) = den(163)*den(413)
  den(416) = den(52)*den(364)
  den(417) = den(366)*den(416)
  den(418) = den(368)*den(416)
  den(419) = den(70)*den(370)
  den(420) = den(52)*den(419)
  den(421) = den(73)*den(370)
  den(422) = den(52)*den(421)
  den(423) = den(64)*den(370)
  den(424) = den(67)*den(370)
  den(425) = den(191)*den(416)
  den(426) = den(194)*den(416)
  den(427) = den(378)*den(416)
  den(428) = den(380)*den(416)
  den(429) = den(70)*den(382)
  den(430) = den(52)*den(429)
  den(431) = den(73)*den(382)
  den(432) = den(52)*den(431)
  den(433) = den(64)*den(382)
  den(434) = den(67)*den(382)
  den(435) = den(152)*den(416)
  den(436) = den(155)*den(416)
  den(437) = den(51)*den(52)
  den(438) = den(391)*den(437)
  den(439) = den(205)*den(438)
  den(440) = den(209)*den(438)
  den(441) = den(158)*den(438)
  den(442) = den(163)*den(438)
  den(443) = den(81)*den(398)
  den(444) = den(81)*den(400)
  den(445) = den(398)*den(416)
  den(446) = den(400)*den(416)
  den(447) = den(206)*den(416)
  den(448) = den(205)*den(447)
  den(449) = den(209)*den(447)
  den(450) = den(81)*den(407)
  den(451) = den(81)*den(409)
  den(452) = den(407)*den(416)
  den(453) = den(409)*den(416)
  den(454) = den(166)*den(416)
  den(455) = den(158)*den(454)
  den(456) = den(163)*den(454)
  den(457) = den(107)*den(370)
  den(458) = den(70)*den(457)
  den(459) = den(73)*den(457)
  den(460) = den(231)*den(274)
  den(461) = den(70)*den(460)
  den(462) = den(73)*den(460)
  den(463) = den(99)*den(370)
  den(464) = den(23)*den(463)
  den(465) = den(28)*den(463)
  den(466) = den(23)*den(460)
  den(467) = den(28)*den(460)
  den(468) = den(107)*den(382)
  den(469) = den(70)*den(468)
  den(470) = den(73)*den(468)
  den(471) = den(246)*den(274)
  den(472) = den(70)*den(471)
  den(473) = den(73)*den(471)
  den(474) = den(99)*den(382)
  den(475) = den(23)*den(474)
  den(476) = den(28)*den(474)
  den(477) = den(23)*den(471)
  den(478) = den(28)*den(471)
  den(479) = den(96)*den(398)
  den(480) = den(96)*den(400)
  den(481) = den(242)*den(280)
  den(482) = den(205)*den(481)
  den(483) = den(209)*den(481)
  den(484) = den(96)*den(407)
  den(485) = den(96)*den(409)
  den(486) = den(234)*den(280)
  den(487) = den(158)*den(486)
  den(488) = den(163)*den(486)
  den(489) = den(111)*den(398)
  den(490) = den(111)*den(400)
  den(491) = den(242)*den(292)
  den(492) = den(205)*den(491)
  den(493) = den(209)*den(491)
  den(494) = den(111)*den(407)
  den(495) = den(111)*den(409)
  den(496) = den(234)*den(292)
  den(497) = den(158)*den(496)
  den(498) = den(163)*den(496)
  den(501) = den(499)*den(500)
  den(502) = den(100)*den(501)
  den(504) = den(499)*den(503)
  den(505) = den(100)*den(504)
  den(506) = den(51)*den(501)
  den(507) = den(42)*den(506)
  den(508) = den(51)*den(504)
  den(509) = den(42)*den(508)
  den(510) = den(108)*den(501)
  den(511) = den(108)*den(504)
  den(512) = den(87)*den(501)
  den(513) = den(4)*den(512)
  den(514) = den(87)*den(504)
  den(515) = den(4)*den(514)
  den(516) = den(7)*den(501)
  den(517) = den(42)*den(516)
  den(518) = den(7)*den(504)
  den(519) = den(42)*den(518)
  den(520) = den(25)*den(516)
  den(521) = den(25)*den(518)
  den(522) = den(63)*den(501)
  den(523) = den(87)*den(522)
  den(524) = den(63)*den(504)
  den(525) = den(87)*den(524)
  den(526) = den(135)*den(501)
  den(527) = den(135)*den(504)
  den(529) = den(500)*den(528)
  den(530) = den(100)*den(529)
  den(531) = den(503)*den(528)
  den(532) = den(100)*den(531)
  den(533) = den(51)*den(529)
  den(534) = den(42)*den(533)
  den(535) = den(51)*den(531)
  den(536) = den(42)*den(535)
  den(537) = den(108)*den(529)
  den(538) = den(108)*den(531)
  den(539) = den(87)*den(529)
  den(540) = den(4)*den(539)
  den(541) = den(87)*den(531)
  den(542) = den(4)*den(541)
  den(543) = den(7)*den(529)
  den(544) = den(42)*den(543)
  den(545) = den(7)*den(531)
  den(546) = den(42)*den(545)
  den(547) = den(25)*den(543)
  den(548) = den(25)*den(545)
  den(549) = den(63)*den(529)
  den(550) = den(87)*den(549)
  den(551) = den(63)*den(531)
  den(552) = den(87)*den(551)
  den(553) = den(135)*den(529)
  den(554) = den(135)*den(531)
  den(555) = den(235)*den(501)
  den(556) = den(235)*den(504)
  den(557) = den(186)*den(501)
  den(558) = den(177)*den(557)
  den(559) = den(186)*den(504)
  den(560) = den(177)*den(559)
  den(561) = den(243)*den(501)
  den(562) = den(243)*den(504)
  den(563) = den(222)*den(501)
  den(564) = den(139)*den(563)
  den(565) = den(222)*den(504)
  den(566) = den(139)*den(565)
  den(567) = den(143)*den(501)
  den(568) = den(171)*den(567)
  den(569) = den(143)*den(504)
  den(570) = den(171)*den(569)
  den(571) = den(177)*den(567)
  den(572) = den(177)*den(569)
  den(573) = den(270)*den(501)
  den(574) = den(270)*den(504)
  den(575) = den(198)*den(501)
  den(576) = den(222)*den(575)
  den(577) = den(198)*den(504)
  den(578) = den(222)*den(577)
  den(579) = den(235)*den(529)
  den(580) = den(235)*den(531)
  den(581) = den(186)*den(529)
  den(582) = den(177)*den(581)
  den(583) = den(186)*den(531)
  den(584) = den(177)*den(583)
  den(585) = den(243)*den(529)
  den(586) = den(243)*den(531)
  den(587) = den(222)*den(529)
  den(588) = den(139)*den(587)
  den(589) = den(222)*den(531)
  den(590) = den(139)*den(589)
  den(591) = den(143)*den(529)
  den(592) = den(171)*den(591)
  den(593) = den(143)*den(531)
  den(594) = den(171)*den(593)
  den(595) = den(177)*den(591)
  den(596) = den(177)*den(593)
  den(597) = den(270)*den(529)
  den(598) = den(270)*den(531)
  den(599) = den(198)*den(529)
  den(600) = den(222)*den(599)
  den(601) = den(198)*den(531)
  den(602) = den(222)*den(601)
  den(603) = den(370)*den(516)
  den(604) = den(370)*den(518)
  den(605) = den(463)*den(501)
  den(606) = den(463)*den(504)
  den(607) = den(382)*den(516)
  den(608) = den(382)*den(518)
  den(609) = den(474)*den(501)
  den(610) = den(474)*den(504)
  den(611) = den(280)*den(567)
  den(612) = den(280)*den(569)
  den(613) = den(486)*den(501)
  den(614) = den(486)*den(504)
  den(615) = den(292)*den(567)
  den(616) = den(292)*den(569)
  den(617) = den(496)*den(501)
  den(618) = den(496)*den(504)
  den(619) = den(370)*den(543)
  den(620) = den(370)*den(545)
  den(621) = den(463)*den(529)
  den(622) = den(463)*den(531)
  den(623) = den(382)*den(543)
  den(624) = den(382)*den(545)
  den(625) = den(474)*den(529)
  den(626) = den(474)*den(531)
  den(627) = den(280)*den(591)
  den(628) = den(280)*den(593)
  den(629) = den(486)*den(529)
  den(630) = den(486)*den(531)
  den(631) = den(292)*den(591)
  den(632) = den(292)*den(593)
  den(633) = den(496)*den(529)
  den(634) = den(496)*den(531)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_ppllajj_eexddxbbxag_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_ppllajj_eexddxbbxag_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for e- e+ down anti-down bottom anti-bottom gamma glue -> 0
! I   = emitter, 0 means none (replace wave function I in the current crossing by the momentum MOM),
!       for I < 0 emitter for PowHeg's B^mu,nu
! MOM = external "polarisation vector" for gluon emitter
! nextcombs is the length of the array extcombs
! The elements of the array extcombs specify for which external particle combinations
!   the colour correlations will be calculated. For particles i,j: i*(i-1)/2+j
!   i=j=0 -> 0 means no colour insertion.
! M2munu = Spin correlated born squared amplitude in PowHeg format B^mu,nu for emitter -I
! **********************************************************************
  use ol_parameters_decl_/**/REALKIND !, only: ci, parameters_status, ZERO, scalefactor, >masses<
  use ol_parameters_init_/**/REALKIND, only: ensure_mp_init
  use ol_kinematics_/**/REALKIND, only: conv_mom_scatt2in, internal_momenta
  use ol_momenta_decl_/**/DREALKIND, only: momenta_nan_check
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_data_types_/**/REALKIND
  use ol_helicity_bookkeeping_/**/REALKIND, only: &
    & helbookkeeping_wf, helsync, flip_phase
  use ol_helicity_init, only: helbookkeeping_flip, helsync_flip
  use ol_h_propagators_/**/REALKIND
  use ol_h_wavefunctions_/**/REALKIND
  use ol_h_vertices_/**/REALKIND
  use ol_h_contractions_/**/REALKIND
  use ol_external_ppllajj_eexddxbbxag_1, only: external_perm_ppllajj_eexddxbbxag_1, &
    & external_perm_inv_ppllajj_eexddxbbxag_1, extcomb_perm_ppllajj_eexddxbbxag_1, &
    & average_factor_ppllajj_eexddxbbxag_1
  use ol_external_ppllajj_eexddxbbxag_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_ppllajj_eexddxbbxag_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_ppllajj_eexddxbbxag_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_ppllajj_eexddxbbxag_1
  use ol_kinematics_/**/REALKIND, only: LC2Std_Rep_cmplx
  implicit none

  real(DREALKIND), intent(in)  :: P_scatt(0:3,8)
  real(REALKIND),  intent(out) :: M2(0:38-1)
  real(REALKIND),  intent(out) :: M2munu(4,4)
  integer,         intent(in)  :: I
  real(REALKIND),  intent(in)  :: MOM(0:3)
  integer,         intent(in)  :: nextcombs
  integer,         intent(in)  :: extcombs(nextcombs)

  integer           :: ReplacePol, JBmunu, extcombs_permuted(nextcombs), shift, k, r, l, m, n
  real(REALKIND)    :: P(0:3,8)
  real(REALKIND)    :: extmasses2(8)
  real(REALKIND)    :: M2add(0:38-1), M2munuadd
  complex(REALKIND) :: MOM_LC(4), M1(4), M1helarray(4,256)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(2), ex6(2), ex7(2), ex8(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,25), wf8(8,56), wf16(16,180), wf32(32,40), wf64(64,16), wf256(256,376)

  type(polcont) :: A(256,376)
  complex(REALKIND) :: Aj(376)

  complex(REALKIND) :: omega(2) ! phases for helicity correlations

  call ensure_mp_init()

  if (hel_not_initialised) call hel_init()
  if (colmat_not_initialised) call colourmatrix_init()
  if (factors_status /= parameters_status) then
    ! Note: if factors_init would only be called when parameters changed which are relevant for the factors,
    ! a different 'status' would have to be used, because check_forced_parameters should be called after every parameter change.
    call check_forced_parameters()
    call factors_init()
  end if

  if (momenta_nan_check(P_scatt) /= 0) then
    M2 = 0
    return
  end if

  ! Convert 2 -> n-2 PS-point to n -> 0 (so that P(1) + ... + P(n) = 0)
  extmasses2 = [ rZERO2, rZERO2, rZERO2, rZERO2, rMB2, rMB2, rZERO2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_ppllajj_eexddxbbxag_1,8)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,8)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_ppllajj_eexddxbbxag_1(I)
  else if (I < 0) then
    JBmunu = external_perm_ppllajj_eexddxbbxag_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_ppllajj_eexddxbbxag_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_Q(P(:,1), rZERO, H1, ex1)
  call wf_A(P(:,2), rZERO, H2, ex2)
  call wf_Q(P(:,3), rZERO, H3, ex3)
  call wf_A(P(:,4), rZERO, H4, ex4)
  call wf_Q(P(:,5), rMB, H5, ex5)
  call wf_A(P(:,6), rMB, H6, ex6)
  call wf_V(P(:,7), rZERO, H7, ex7)
  call wf_V(P(:,8), rZERO, H8, ex8)


  if (ntry == 1) then
    shift = 1
    ! call helbookkeeping_flip(H1, 1, shift, eflip, exthel, firstpol)
    ! call helbookkeeping_wf(H1, ex1, shift) ...
    call helbookkeeping_flip(H1, 1, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H1, ex1, shift)
    call helbookkeeping_flip(H2, 2, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H2, ex2, shift)
    call helbookkeeping_flip(H3, 3, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H3, ex3, shift)
    call helbookkeeping_flip(H4, 4, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H4, ex4, shift)
    call helbookkeeping_flip(H5, 5, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H5, ex5, shift)
    call helbookkeeping_flip(H6, 6, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H6, ex6, shift)
    call helbookkeeping_flip(H7, 7, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H7, ex7, shift)
    call helbookkeeping_flip(H8, 8, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H8, ex8, shift)

  end if

  ! internal WFs
  ! e.g. call vert_VQ_A(ntry, ex3, ex1, wf1, n1, t1) ...
  call vert_QA_V(ntry, ex1, ex2, wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_QA_V(ntry, ex3, ex4, wf4(:,2), n3(:,2), t3x4(:,:,2))
  call vert_VQ_A(ntry, ex7, ex5, wf4(:,3), n3(:,3), t3x4(:,:,3))
  call vert_AV_Q(ntry, ex6, ex8, wf4(:,4), n3(:,4), t3x4(:,:,4))
  call prop_Q_A(ntry, wf4(:,3), Q(:,80), MB, 1_intkind1, wf4(:,5), n2(1))
  call prop_A_Q(ntry, wf4(:,4), Q(:,160), MB, 1_intkind1, wf4(:,6), n2(2))
  call vert_VQ_A(ntry, wf4(:,1), wf4(:,5), wf16(:,1), n3(:,5), t3x16(:,:,1))
  call vert_AV_Q(ntry, wf4(:,6), wf4(:,2), wf16(:,2), n3(:,6), t3x16(:,:,2))
  call prop_Q_A(ntry, wf16(:,1), Q(:,83), MB, 1_intkind1, wf16(:,3), n2(3))
  call vert_QA_Z(gZl,ntry, ex1, ex2, wf4(:,7), n3(:,7), t3x4(:,:,5))
  call prop_W_W(ntry, wf4(:,7), Q(:,3), MZ, 1_intkind1, wf4(:,8), n2(4))
  call vert_ZQ_A(gZd,ntry, wf4(:,8), wf4(:,5), wf16(:,4), n3(:,8), t3x16(:,:,3))
  call prop_Q_A(ntry, wf16(:,4), Q(:,83), MB, 1_intkind1, wf16(:,5), n2(5))
  call vert_AV_Q(ntry, wf4(:,6), wf4(:,1), wf16(:,6), n3(:,9), t3x16(:,:,4))
  call vert_VQ_A(ntry, wf4(:,2), wf4(:,5), wf16(:,7), n3(:,10), t3x16(:,:,5))
  call prop_A_Q(ntry, wf16(:,6), Q(:,163), MB, 1_intkind1, wf16(:,8), n2(6))
  call vert_AZ_Q(gZd,ntry, wf4(:,6), wf4(:,8), wf16(:,9), n3(:,11), t3x16(:,:,6))
  call prop_A_Q(ntry, wf16(:,9), Q(:,163), MB, 1_intkind1, wf16(:,10), n2(7))
  call vert_AV_Q(ntry, ex6, wf4(:,1), wf8(:,1), n3(:,12), t3x8(:,:,1))
  call vert_UV_W(ntry, wf4(:,2), Q(:,12), ex8, Q(:,128), wf8(:,2), n3(:,13), t3x8(:,:,2))
  call prop_A_Q(ntry, wf8(:,1), Q(:,35), MB, 1_intkind1, wf8(:,3), n2(8))
  call vert_QA_V(ntry, wf4(:,5), wf8(:,3), wf32(:,1), n3(:,14), t3x32(:,:,1))
  call vert_AZ_Q(gZd,ntry, ex6, wf4(:,8), wf8(:,4), n3(:,15), t3x8(:,:,3))
  call prop_A_Q(ntry, wf8(:,4), Q(:,35), MB, 1_intkind1, wf8(:,5), n2(9))
  call vert_QA_V(ntry, wf4(:,5), wf8(:,5), wf32(:,2), n3(:,16), t3x32(:,:,2))
  call vert_VQ_A(ntry, ex8, wf4(:,5), wf8(:,6), n3(:,17), t3x8(:,:,4))
  call prop_Q_A(ntry, wf8(:,6), Q(:,208), MB, 1_intkind1, wf8(:,7), n2(10))
  call vert_AV_Q(ntry, wf8(:,3), wf4(:,2), wf32(:,3), n3(:,18), t3x32(:,:,3))
  call vert_AV_Q(ntry, wf8(:,5), wf4(:,2), wf32(:,4), n3(:,19), t3x32(:,:,4))
  call prop_Q_A(ntry, wf16(:,7), Q(:,92), MB, 1_intkind1, wf16(:,11), n2(11))
  call vert_AV_Q(ntry, wf8(:,3), ex8, wf16(:,12), n3(:,20), t3x16(:,:,7))
  call vert_AV_Q(ntry, wf8(:,5), ex8, wf16(:,13), n3(:,21), t3x16(:,:,8))
  call vert_AV_Q(ntry, ex6, wf4(:,2), wf8(:,8), n3(:,22), t3x8(:,:,5))
  call prop_A_Q(ntry, wf8(:,8), Q(:,44), MB, 1_intkind1, wf8(:,9), n2(12))
  call vert_AV_Q(ntry, wf8(:,9), wf4(:,1), wf32(:,5), n3(:,23), t3x32(:,:,5))
  call vert_AZ_Q(gZd,ntry, wf8(:,9), wf4(:,8), wf32(:,6), n3(:,24), t3x32(:,:,6))
  call vert_AV_Q(ntry, wf8(:,9), ex8, wf16(:,14), n3(:,25), t3x16(:,:,9))
  call vert_AV_Q(ntry, ex6, wf8(:,2), wf16(:,15), n3(:,26), t3x16(:,:,10))
  call vert_VQ_A(ntry, ex8, ex5, wf4(:,9), n3(:,27), t3x4(:,:,6))
  call vert_AV_Q(ntry, ex6, ex7, wf4(:,10), n3(:,28), t3x4(:,:,7))
  call prop_Q_A(ntry, wf4(:,9), Q(:,144), MB, 1_intkind1, wf4(:,11), n2(13))
  call prop_A_Q(ntry, wf4(:,10), Q(:,96), MB, 1_intkind1, wf4(:,12), n2(14))
  call vert_VQ_A(ntry, wf4(:,1), wf4(:,11), wf16(:,16), n3(:,29), t3x16(:,:,11))
  call vert_AV_Q(ntry, wf4(:,12), wf4(:,2), wf16(:,17), n3(:,30), t3x16(:,:,12))
  call prop_Q_A(ntry, wf16(:,16), Q(:,147), MB, 1_intkind1, wf16(:,18), n2(15))
  call vert_ZQ_A(gZd,ntry, wf4(:,8), wf4(:,11), wf16(:,19), n3(:,31), t3x16(:,:,13))
  call prop_Q_A(ntry, wf16(:,19), Q(:,147), MB, 1_intkind1, wf16(:,20), n2(16))
  call vert_AV_Q(ntry, wf4(:,12), wf4(:,1), wf16(:,21), n3(:,32), t3x16(:,:,14))
  call vert_VQ_A(ntry, wf4(:,2), wf4(:,11), wf16(:,22), n3(:,33), t3x16(:,:,15))
  call prop_A_Q(ntry, wf16(:,21), Q(:,99), MB, 1_intkind1, wf16(:,23), n2(17))
  call vert_AZ_Q(gZd,ntry, wf4(:,12), wf4(:,8), wf16(:,24), n3(:,34), t3x16(:,:,16))
  call prop_A_Q(ntry, wf16(:,24), Q(:,99), MB, 1_intkind1, wf16(:,25), n2(18))
  call vert_VQ_A(ntry, wf4(:,1), ex5, wf8(:,10), n3(:,35), t3x8(:,:,6))
  call prop_Q_A(ntry, wf8(:,10), Q(:,19), MB, 1_intkind1, wf8(:,11), n2(19))
  call vert_QA_V(ntry, wf8(:,11), wf4(:,12), wf32(:,7), n3(:,36), t3x32(:,:,7))
  call vert_ZQ_A(gZd,ntry, wf4(:,8), ex5, wf8(:,12), n3(:,37), t3x8(:,:,7))
  call prop_Q_A(ntry, wf8(:,12), Q(:,19), MB, 1_intkind1, wf8(:,13), n2(20))
  call vert_QA_V(ntry, wf8(:,13), wf4(:,12), wf32(:,8), n3(:,38), t3x32(:,:,8))
  call prop_A_Q(ntry, wf16(:,17), Q(:,108), MB, 1_intkind1, wf16(:,26), n2(21))
  call vert_VQ_A(ntry, ex8, wf8(:,11), wf16(:,27), n3(:,39), t3x16(:,:,17))
  call vert_VQ_A(ntry, ex8, wf8(:,13), wf16(:,28), n3(:,40), t3x16(:,:,18))
  call vert_AV_Q(ntry, wf4(:,12), ex8, wf8(:,14), n3(:,41), t3x8(:,:,8))
  call prop_A_Q(ntry, wf8(:,14), Q(:,224), MB, 1_intkind1, wf8(:,15), n2(22))
  call vert_VQ_A(ntry, wf4(:,2), wf8(:,11), wf32(:,9), n3(:,42), t3x32(:,:,9))
  call vert_VQ_A(ntry, wf4(:,2), wf8(:,13), wf32(:,10), n3(:,43), t3x32(:,:,10))
  call vert_VQ_A(ntry, wf4(:,2), ex5, wf8(:,16), n3(:,44), t3x8(:,:,9))
  call prop_Q_A(ntry, wf8(:,16), Q(:,28), MB, 1_intkind1, wf8(:,17), n2(23))
  call vert_VQ_A(ntry, ex8, wf8(:,17), wf16(:,29), n3(:,45), t3x16(:,:,19))
  call vert_VQ_A(ntry, wf4(:,1), wf8(:,17), wf32(:,11), n3(:,46), t3x32(:,:,11))
  call vert_ZQ_A(gZd,ntry, wf4(:,8), wf8(:,17), wf32(:,12), n3(:,47), t3x32(:,:,12))
  call vert_VQ_A(ntry, wf8(:,2), ex5, wf16(:,30), n3(:,48), t3x16(:,:,20))
  call vert_VQ_A(ntry, ex7, wf4(:,11), wf8(:,18), n3(:,49), t3x8(:,:,10))
  call prop_Q_A(ntry, wf8(:,18), Q(:,208), MB, 1_intkind1, wf8(:,19), n2(24))
  call prop_Q_A(ntry, wf16(:,22), Q(:,156), MB, 1_intkind1, wf16(:,31), n2(25))
  call vert_AV_Q(ntry, wf8(:,3), ex7, wf16(:,32), n3(:,50), t3x16(:,:,21))
  call vert_AV_Q(ntry, wf8(:,5), ex7, wf16(:,33), n3(:,51), t3x16(:,:,22))
  call vert_AV_Q(ntry, wf8(:,9), ex7, wf16(:,34), n3(:,52), t3x16(:,:,23))
  call prop_A_Q(ntry, wf16(:,2), Q(:,172), MB, 1_intkind1, wf16(:,35), n2(26))
  call vert_VQ_A(ntry, ex7, wf8(:,11), wf16(:,36), n3(:,53), t3x16(:,:,24))
  call vert_VQ_A(ntry, ex7, wf8(:,13), wf16(:,37), n3(:,54), t3x16(:,:,25))
  call vert_AV_Q(ntry, wf4(:,6), ex7, wf8(:,20), n3(:,55), t3x8(:,:,11))
  call prop_A_Q(ntry, wf8(:,20), Q(:,224), MB, 1_intkind1, wf8(:,21), n2(27))
  call vert_VQ_A(ntry, ex7, wf8(:,17), wf16(:,38), n3(:,56), t3x16(:,:,26))
  call prop_Q_A(ntry, wf16(:,36), Q(:,83), MB, 1_intkind1, wf16(:,39), n2(28))
  call prop_Q_A(ntry, wf16(:,37), Q(:,83), MB, 1_intkind1, wf16(:,40), n2(29))
  call prop_A_Q(ntry, wf16(:,34), Q(:,108), MB, 1_intkind1, wf16(:,41), n2(30))
  call prop_A_Q(ntry, wf16(:,15), Q(:,172), MB, 1_intkind1, wf16(:,42), n2(31))
  call prop_Q_A(ntry, wf16(:,38), Q(:,92), MB, 1_intkind1, wf16(:,43), n2(32))
  call prop_A_Q(ntry, wf16(:,32), Q(:,99), MB, 1_intkind1, wf16(:,44), n2(33))
  call prop_A_Q(ntry, wf16(:,33), Q(:,99), MB, 1_intkind1, wf16(:,45), n2(34))
  call prop_Q_A(ntry, wf16(:,30), Q(:,156), MB, 1_intkind1, wf16(:,46), n2(35))
  call vert_VQ_A(ntry, ex7, ex3, wf4(:,13), n3(:,57), t3x4(:,:,8))
  call vert_AV_Q(ntry, ex4, ex8, wf4(:,14), n3(:,58), t3x4(:,:,9))
  call vert_QA_V(ntry, ex5, ex6, wf4(:,15), n3(:,59), t3x4(:,:,10))
  call prop_Q_A(ntry, wf4(:,13), Q(:,68), ZERO, 0_intkind1, wf4(:,16), n2(36))
  call prop_A_Q(ntry, wf4(:,14), Q(:,136), ZERO, 0_intkind1, wf4(:,17), n2(37))
  call vert_VQ_A(ntry, wf4(:,1), wf4(:,16), wf16(:,47), n3(:,60), t3x16(:,:,27))
  call vert_AV_Q(ntry, wf4(:,17), wf4(:,15), wf16(:,48), n3(:,61), t3x16(:,:,28))
  call prop_Q_A(ntry, wf16(:,47), Q(:,71), ZERO, 0_intkind1, wf16(:,49), n2(38))
  call vert_ZQ_A(gZd,ntry, wf4(:,8), wf4(:,16), wf16(:,50), n3(:,62), t3x16(:,:,29))
  call prop_Q_A(ntry, wf16(:,50), Q(:,71), ZERO, 0_intkind1, wf16(:,51), n2(39))
  call vert_AV_Q(ntry, wf4(:,17), wf4(:,1), wf16(:,52), n3(:,63), t3x16(:,:,30))
  call vert_VQ_A(ntry, wf4(:,15), wf4(:,16), wf16(:,53), n3(:,64), t3x16(:,:,31))
  call prop_A_Q(ntry, wf16(:,52), Q(:,139), ZERO, 0_intkind1, wf16(:,54), n2(40))
  call vert_AZ_Q(gZd,ntry, wf4(:,17), wf4(:,8), wf16(:,55), n3(:,65), t3x16(:,:,32))
  call prop_A_Q(ntry, wf16(:,55), Q(:,139), ZERO, 0_intkind1, wf16(:,56), n2(41))
  call vert_AV_Q(ntry, ex4, wf4(:,1), wf8(:,22), n3(:,66), t3x8(:,:,12))
  call vert_VQ_A(ntry, ex8, wf4(:,16), wf8(:,23), n3(:,67), t3x8(:,:,13))
  call prop_A_Q(ntry, wf8(:,22), Q(:,11), ZERO, 0_intkind1, wf8(:,24), n2(42))
  call prop_Q_A(ntry, wf8(:,23), Q(:,196), ZERO, 0_intkind1, wf8(:,25), n2(43))
  call vert_AV_Q(ntry, wf8(:,24), wf4(:,15), wf32(:,13), n3(:,68), t3x32(:,:,13))
  call vert_AZ_Q(gZd,ntry, ex4, wf4(:,8), wf8(:,26), n3(:,69), t3x8(:,:,14))
  call prop_A_Q(ntry, wf8(:,26), Q(:,11), ZERO, 0_intkind1, wf8(:,27), n2(44))
  call vert_AV_Q(ntry, wf8(:,27), wf4(:,15), wf32(:,14), n3(:,70), t3x32(:,:,14))
  call prop_Q_A(ntry, wf16(:,53), Q(:,116), ZERO, 0_intkind1, wf16(:,57), n2(45))
  call vert_AV_Q(ntry, wf8(:,24), ex8, wf16(:,58), n3(:,71), t3x16(:,:,33))
  call vert_AV_Q(ntry, wf8(:,27), ex8, wf16(:,59), n3(:,72), t3x16(:,:,34))
  call vert_UV_W(ntry, wf4(:,15), Q(:,48), ex8, Q(:,128), wf8(:,28), n3(:,73), t3x8(:,:,15))
  call vert_QA_V(ntry, wf4(:,16), wf8(:,24), wf32(:,15), n3(:,74), t3x32(:,:,15))
  call vert_QA_V(ntry, wf4(:,16), wf8(:,27), wf32(:,16), n3(:,75), t3x32(:,:,16))
  call vert_AV_Q(ntry, ex4, wf4(:,15), wf8(:,29), n3(:,76), t3x8(:,:,16))
  call prop_A_Q(ntry, wf8(:,29), Q(:,56), ZERO, 0_intkind1, wf8(:,30), n2(46))
  call vert_AV_Q(ntry, wf8(:,30), wf4(:,1), wf32(:,17), n3(:,77), t3x32(:,:,17))
  call vert_AZ_Q(gZd,ntry, wf8(:,30), wf4(:,8), wf32(:,18), n3(:,78), t3x32(:,:,18))
  call vert_AV_Q(ntry, wf8(:,30), ex8, wf16(:,60), n3(:,79), t3x16(:,:,35))
  call vert_AV_Q(ntry, ex4, wf8(:,28), wf16(:,61), n3(:,80), t3x16(:,:,36))
  call vert_VQ_A(ntry, ex8, ex3, wf4(:,18), n3(:,81), t3x4(:,:,11))
  call vert_AV_Q(ntry, ex4, ex7, wf4(:,19), n3(:,82), t3x4(:,:,12))
  call prop_Q_A(ntry, wf4(:,18), Q(:,132), ZERO, 0_intkind1, wf4(:,20), n2(47))
  call prop_A_Q(ntry, wf4(:,19), Q(:,72), ZERO, 0_intkind1, wf4(:,21), n2(48))
  call vert_VQ_A(ntry, wf4(:,1), wf4(:,20), wf16(:,62), n3(:,83), t3x16(:,:,37))
  call vert_AV_Q(ntry, wf4(:,21), wf4(:,15), wf16(:,63), n3(:,84), t3x16(:,:,38))
  call prop_Q_A(ntry, wf16(:,62), Q(:,135), ZERO, 0_intkind1, wf16(:,64), n2(49))
  call vert_ZQ_A(gZd,ntry, wf4(:,8), wf4(:,20), wf16(:,65), n3(:,85), t3x16(:,:,39))
  call prop_Q_A(ntry, wf16(:,65), Q(:,135), ZERO, 0_intkind1, wf16(:,66), n2(50))
  call vert_AV_Q(ntry, wf4(:,21), wf4(:,1), wf16(:,67), n3(:,86), t3x16(:,:,40))
  call vert_VQ_A(ntry, wf4(:,15), wf4(:,20), wf16(:,68), n3(:,87), t3x16(:,:,41))
  call prop_A_Q(ntry, wf16(:,67), Q(:,75), ZERO, 0_intkind1, wf16(:,69), n2(51))
  call vert_AZ_Q(gZd,ntry, wf4(:,21), wf4(:,8), wf16(:,70), n3(:,88), t3x16(:,:,42))
  call prop_A_Q(ntry, wf16(:,70), Q(:,75), ZERO, 0_intkind1, wf16(:,71), n2(52))
  call vert_VQ_A(ntry, wf4(:,1), ex3, wf8(:,31), n3(:,89), t3x8(:,:,17))
  call prop_Q_A(ntry, wf8(:,31), Q(:,7), ZERO, 0_intkind1, wf8(:,32), n2(53))
  call prop_A_Q(ntry, wf16(:,63), Q(:,120), ZERO, 0_intkind1, wf16(:,72), n2(54))
  call vert_VQ_A(ntry, ex8, wf8(:,32), wf16(:,73), n3(:,90), t3x16(:,:,43))
  call vert_ZQ_A(gZd,ntry, wf4(:,8), ex3, wf8(:,33), n3(:,91), t3x8(:,:,18))
  call prop_Q_A(ntry, wf8(:,33), Q(:,7), ZERO, 0_intkind1, wf8(:,34), n2(55))
  call vert_VQ_A(ntry, ex8, wf8(:,34), wf16(:,74), n3(:,92), t3x16(:,:,44))
  call vert_AV_Q(ntry, wf4(:,21), ex8, wf8(:,35), n3(:,93), t3x8(:,:,19))
  call prop_A_Q(ntry, wf8(:,35), Q(:,200), ZERO, 0_intkind1, wf8(:,36), n2(56))
  call vert_VQ_A(ntry, wf4(:,15), wf8(:,32), wf32(:,19), n3(:,94), t3x32(:,:,19))
  call vert_VQ_A(ntry, wf4(:,15), wf8(:,34), wf32(:,20), n3(:,95), t3x32(:,:,20))
  call vert_QA_V(ntry, wf8(:,32), wf4(:,21), wf32(:,21), n3(:,96), t3x32(:,:,21))
  call vert_QA_V(ntry, wf8(:,34), wf4(:,21), wf32(:,22), n3(:,97), t3x32(:,:,22))
  call vert_VQ_A(ntry, wf4(:,15), ex3, wf8(:,37), n3(:,98), t3x8(:,:,20))
  call prop_Q_A(ntry, wf8(:,37), Q(:,52), ZERO, 0_intkind1, wf8(:,38), n2(57))
  call vert_VQ_A(ntry, ex8, wf8(:,38), wf16(:,75), n3(:,99), t3x16(:,:,45))
  call vert_VQ_A(ntry, wf4(:,1), wf8(:,38), wf32(:,23), n3(:,100), t3x32(:,:,23))
  call vert_ZQ_A(gZd,ntry, wf4(:,8), wf8(:,38), wf32(:,24), n3(:,101), t3x32(:,:,24))
  call vert_VQ_A(ntry, wf8(:,28), ex3, wf16(:,76), n3(:,102), t3x16(:,:,46))
  call vert_VQ_A(ntry, ex7, wf4(:,20), wf8(:,39), n3(:,103), t3x8(:,:,21))
  call prop_Q_A(ntry, wf8(:,39), Q(:,196), ZERO, 0_intkind1, wf8(:,40), n2(58))
  call prop_Q_A(ntry, wf16(:,68), Q(:,180), ZERO, 0_intkind1, wf16(:,77), n2(59))
  call vert_AV_Q(ntry, wf8(:,24), ex7, wf16(:,78), n3(:,104), t3x16(:,:,47))
  call vert_AV_Q(ntry, wf8(:,27), ex7, wf16(:,79), n3(:,105), t3x16(:,:,48))
  call vert_AV_Q(ntry, wf8(:,30), ex7, wf16(:,80), n3(:,106), t3x16(:,:,49))
  call prop_A_Q(ntry, wf16(:,48), Q(:,184), ZERO, 0_intkind1, wf16(:,81), n2(60))
  call vert_VQ_A(ntry, ex7, wf8(:,32), wf16(:,82), n3(:,107), t3x16(:,:,50))
  call vert_VQ_A(ntry, ex7, wf8(:,34), wf16(:,83), n3(:,108), t3x16(:,:,51))
  call vert_AV_Q(ntry, wf4(:,17), ex7, wf8(:,41), n3(:,109), t3x8(:,:,22))
  call prop_A_Q(ntry, wf8(:,41), Q(:,200), ZERO, 0_intkind1, wf8(:,42), n2(61))
  call vert_VQ_A(ntry, ex7, wf8(:,38), wf16(:,84), n3(:,110), t3x16(:,:,52))
  call prop_Q_A(ntry, wf16(:,82), Q(:,71), ZERO, 0_intkind1, wf16(:,85), n2(62))
  call prop_Q_A(ntry, wf16(:,83), Q(:,71), ZERO, 0_intkind1, wf16(:,86), n2(63))
  call prop_A_Q(ntry, wf16(:,61), Q(:,184), ZERO, 0_intkind1, wf16(:,87), n2(64))
  call prop_A_Q(ntry, wf16(:,80), Q(:,120), ZERO, 0_intkind1, wf16(:,88), n2(65))
  call prop_Q_A(ntry, wf16(:,84), Q(:,116), ZERO, 0_intkind1, wf16(:,89), n2(66))
  call prop_A_Q(ntry, wf16(:,78), Q(:,75), ZERO, 0_intkind1, wf16(:,90), n2(67))
  call prop_A_Q(ntry, wf16(:,79), Q(:,75), ZERO, 0_intkind1, wf16(:,91), n2(68))
  call prop_Q_A(ntry, wf16(:,76), Q(:,180), ZERO, 0_intkind1, wf16(:,92), n2(69))
  call vert_QA_V(ntry, wf4(:,16), wf4(:,17), wf16(:,93), n3(:,111), t3x16(:,:,53))
  call vert_QA_V(ntry, wf8(:,11), ex6, wf16(:,94), n3(:,112), t3x16(:,:,54))
  call vert_QA_V(ntry, wf8(:,13), ex6, wf16(:,95), n3(:,113), t3x16(:,:,55))
  call vert_QA_V(ntry, ex5, wf8(:,3), wf16(:,96), n3(:,114), t3x16(:,:,56))
  call vert_QA_V(ntry, ex5, wf8(:,5), wf16(:,97), n3(:,115), t3x16(:,:,57))
  call vert_QA_V(ntry, wf4(:,11), ex6, wf8(:,43), n3(:,116), t3x8(:,:,23))
  call vert_QA_V(ntry, wf4(:,16), ex4, wf8(:,44), n3(:,117), t3x8(:,:,24))
  call vert_VQ_A(ntry, wf8(:,44), wf4(:,11), wf32(:,25), n3(:,118), t3x32(:,:,25))
  call vert_AV_Q(ntry, ex6, wf8(:,44), wf16(:,98), n3(:,119), t3x16(:,:,58))
  call vert_AV_Q(ntry, ex4, wf8(:,43), wf16(:,99), n3(:,120), t3x16(:,:,59))
  call vert_QA_V(ntry, ex5, wf4(:,6), wf8(:,45), n3(:,121), t3x8(:,:,25))
  call vert_VQ_A(ntry, wf8(:,44), wf8(:,11), wf64(:,1), n3(:,122), t3x64(:,:,1))
  call vert_VQ_A(ntry, wf8(:,44), wf8(:,13), wf64(:,2), n3(:,123), t3x64(:,:,2))
  call vert_VQ_A(ntry, wf8(:,44), ex5, wf16(:,100), n3(:,124), t3x16(:,:,60))
  call vert_AV_Q(ntry, ex4, wf8(:,45), wf16(:,101), n3(:,125), t3x16(:,:,61))
  call prop_A_Q(ntry, wf16(:,98), Q(:,108), MB, 1_intkind1, wf16(:,102), n2(70))
  call vert_UV_W(ntry, wf8(:,44), Q(:,76), ex8, Q(:,128), wf16(:,103), n3(:,126), t3x16(:,:,62))
  call vert_QA_V(ntry, wf8(:,25), ex4, wf16(:,104), n3(:,127), t3x16(:,:,63))
  call prop_Q_A(ntry, wf16(:,100), Q(:,92), MB, 1_intkind1, wf16(:,105), n2(71))
  call vert_QA_V(ntry, wf4(:,20), wf4(:,21), wf16(:,106), n3(:,128), t3x16(:,:,64))
  call vert_QA_V(ntry, ex3, wf4(:,21), wf8(:,46), n3(:,129), t3x8(:,:,26))
  call vert_VQ_A(ntry, wf8(:,46), wf4(:,11), wf32(:,26), n3(:,130), t3x32(:,:,26))
  call vert_AV_Q(ntry, ex6, wf8(:,46), wf16(:,107), n3(:,131), t3x16(:,:,65))
  call vert_VQ_A(ntry, wf8(:,43), ex3, wf16(:,108), n3(:,132), t3x16(:,:,66))
  call vert_VQ_A(ntry, wf8(:,46), wf8(:,11), wf64(:,3), n3(:,133), t3x64(:,:,3))
  call vert_VQ_A(ntry, wf8(:,46), wf8(:,13), wf64(:,4), n3(:,134), t3x64(:,:,4))
  call vert_VQ_A(ntry, wf8(:,46), ex5, wf16(:,109), n3(:,135), t3x16(:,:,67))
  call vert_VQ_A(ntry, wf8(:,45), ex3, wf16(:,110), n3(:,136), t3x16(:,:,68))
  call prop_A_Q(ntry, wf16(:,107), Q(:,108), MB, 1_intkind1, wf16(:,111), n2(72))
  call vert_UV_W(ntry, wf8(:,46), Q(:,76), ex8, Q(:,128), wf16(:,112), n3(:,137), t3x16(:,:,69))
  call vert_QA_V(ntry, ex3, wf8(:,36), wf16(:,113), n3(:,138), t3x16(:,:,70))
  call prop_Q_A(ntry, wf16(:,109), Q(:,92), MB, 1_intkind1, wf16(:,114), n2(73))
  call vert_QA_V(ntry, wf4(:,5), ex6, wf8(:,47), n3(:,139), t3x8(:,:,27))
  call vert_QA_V(ntry, wf4(:,20), wf8(:,24), wf32(:,27), n3(:,140), t3x32(:,:,27))
  call vert_QA_V(ntry, wf4(:,20), wf8(:,27), wf32(:,28), n3(:,141), t3x32(:,:,28))
  call vert_QA_V(ntry, wf4(:,20), ex4, wf8(:,48), n3(:,142), t3x8(:,:,28))
  call vert_VQ_A(ntry, wf8(:,48), wf4(:,5), wf32(:,29), n3(:,143), t3x32(:,:,29))
  call vert_AV_Q(ntry, ex6, wf8(:,48), wf16(:,115), n3(:,144), t3x16(:,:,71))
  call vert_AV_Q(ntry, ex4, wf8(:,47), wf16(:,116), n3(:,145), t3x16(:,:,72))
  call vert_QA_V(ntry, wf8(:,32), wf4(:,17), wf32(:,30), n3(:,146), t3x32(:,:,30))
  call vert_QA_V(ntry, wf8(:,34), wf4(:,17), wf32(:,31), n3(:,147), t3x32(:,:,31))
  call vert_QA_V(ntry, ex3, wf4(:,17), wf8(:,49), n3(:,148), t3x8(:,:,29))
  call vert_VQ_A(ntry, wf8(:,49), wf4(:,5), wf32(:,32), n3(:,149), t3x32(:,:,32))
  call vert_AV_Q(ntry, ex6, wf8(:,49), wf16(:,117), n3(:,150), t3x16(:,:,73))
  call vert_VQ_A(ntry, wf8(:,47), ex3, wf16(:,118), n3(:,151), t3x16(:,:,74))
  call vert_QA_V(ntry, wf4(:,5), wf4(:,6), wf16(:,119), n3(:,152), t3x16(:,:,75))
  call vert_QA_V(ntry, wf8(:,32), ex4, wf16(:,120), n3(:,153), t3x16(:,:,76))
  call vert_QA_V(ntry, wf8(:,34), ex4, wf16(:,121), n3(:,154), t3x16(:,:,77))
  call vert_QA_V(ntry, ex3, wf8(:,24), wf16(:,122), n3(:,155), t3x16(:,:,78))
  call vert_QA_V(ntry, ex3, wf8(:,27), wf16(:,123), n3(:,156), t3x16(:,:,79))
  call vert_QA_V(ntry, wf8(:,7), ex6, wf16(:,124), n3(:,157), t3x16(:,:,80))
  call vert_UV_W(ntry, wf8(:,47), Q(:,112), ex8, Q(:,128), wf16(:,125), n3(:,158), t3x16(:,:,81))
  call prop_A_Q(ntry, wf16(:,116), Q(:,120), ZERO, 0_intkind1, wf16(:,126), n2(74))
  call prop_Q_A(ntry, wf16(:,118), Q(:,116), ZERO, 0_intkind1, wf16(:,127), n2(75))
  call vert_QA_V(ntry, ex5, wf4(:,12), wf8(:,50), n3(:,159), t3x8(:,:,30))
  call vert_VQ_A(ntry, wf8(:,48), wf8(:,11), wf64(:,5), n3(:,160), t3x64(:,:,5))
  call vert_VQ_A(ntry, wf8(:,48), wf8(:,13), wf64(:,6), n3(:,161), t3x64(:,:,6))
  call vert_VQ_A(ntry, wf8(:,48), ex5, wf16(:,128), n3(:,162), t3x16(:,:,82))
  call vert_AV_Q(ntry, ex4, wf8(:,50), wf16(:,129), n3(:,163), t3x16(:,:,83))
  call vert_VQ_A(ntry, wf8(:,49), wf8(:,11), wf64(:,7), n3(:,164), t3x64(:,:,7))
  call vert_VQ_A(ntry, wf8(:,49), wf8(:,13), wf64(:,8), n3(:,165), t3x64(:,:,8))
  call vert_VQ_A(ntry, wf8(:,49), ex5, wf16(:,130), n3(:,166), t3x16(:,:,84))
  call vert_VQ_A(ntry, wf8(:,50), ex3, wf16(:,131), n3(:,167), t3x16(:,:,85))
  call vert_QA_V(ntry, wf4(:,11), wf4(:,12), wf16(:,132), n3(:,168), t3x16(:,:,86))
  call vert_QA_V(ntry, ex5, wf8(:,15), wf16(:,133), n3(:,169), t3x16(:,:,87))
  call vert_UV_W(ntry, wf8(:,50), Q(:,112), ex8, Q(:,128), wf16(:,134), n3(:,170), t3x16(:,:,88))
  call prop_A_Q(ntry, wf16(:,129), Q(:,120), ZERO, 0_intkind1, wf16(:,135), n2(76))
  call prop_Q_A(ntry, wf16(:,131), Q(:,116), ZERO, 0_intkind1, wf16(:,136), n2(77))
  call prop_A_Q(ntry, wf16(:,115), Q(:,172), MB, 1_intkind1, wf16(:,137), n2(78))
  call vert_QA_V(ntry, wf8(:,40), ex4, wf16(:,138), n3(:,171), t3x16(:,:,89))
  call prop_Q_A(ntry, wf16(:,128), Q(:,156), MB, 1_intkind1, wf16(:,139), n2(79))
  call prop_A_Q(ntry, wf16(:,117), Q(:,172), MB, 1_intkind1, wf16(:,140), n2(80))
  call vert_QA_V(ntry, ex3, wf8(:,42), wf16(:,141), n3(:,172), t3x16(:,:,90))
  call prop_Q_A(ntry, wf16(:,130), Q(:,156), MB, 1_intkind1, wf16(:,142), n2(81))
  call vert_QA_V(ntry, wf8(:,19), ex6, wf16(:,143), n3(:,173), t3x16(:,:,91))
  call prop_A_Q(ntry, wf16(:,99), Q(:,184), ZERO, 0_intkind1, wf16(:,144), n2(82))
  call prop_Q_A(ntry, wf16(:,108), Q(:,180), ZERO, 0_intkind1, wf16(:,145), n2(83))
  call vert_QA_V(ntry, ex5, wf8(:,21), wf16(:,146), n3(:,174), t3x16(:,:,92))
  call prop_A_Q(ntry, wf16(:,101), Q(:,184), ZERO, 0_intkind1, wf16(:,147), n2(84))
  call prop_Q_A(ntry, wf16(:,110), Q(:,180), ZERO, 0_intkind1, wf16(:,148), n2(85))
  call vert_VQ_A(ntry, ex7, ex1, wf4(:,22), n3(:,175), t3x4(:,:,13))
  call prop_Q_A(ntry, wf4(:,22), Q(:,65), ZERO, 0_intkind1, wf4(:,23), n2(86))
  call vert_QA_V(ntry, wf4(:,23), ex2, wf8(:,51), n3(:,176), t3x8(:,:,31))
  call vert_AV_Q(ntry, ex6, wf8(:,51), wf16(:,149), n3(:,177), t3x16(:,:,93))
  call vert_QA_Z(gZl,ntry, wf4(:,23), ex2, wf8(:,52), n3(:,178), t3x8(:,:,32))
  call prop_W_W(ntry, wf8(:,52), Q(:,67), MZ, 1_intkind1, wf8(:,53), n2(87))
  call vert_AZ_Q(gZd,ntry, ex6, wf8(:,53), wf16(:,150), n3(:,179), t3x16(:,:,94))
  call vert_VQ_A(ntry, wf8(:,51), wf4(:,11), wf32(:,33), n3(:,180), t3x32(:,:,33))
  call vert_ZQ_A(gZd,ntry, wf8(:,53), wf4(:,11), wf32(:,34), n3(:,181), t3x32(:,:,34))
  call vert_VQ_A(ntry, wf8(:,51), ex5, wf16(:,151), n3(:,182), t3x16(:,:,95))
  call vert_ZQ_A(gZd,ntry, wf8(:,53), ex5, wf16(:,152), n3(:,183), t3x16(:,:,96))
  call vert_VQ_A(ntry, wf8(:,51), wf8(:,17), wf64(:,9), n3(:,184), t3x64(:,:,9))
  call vert_ZQ_A(gZd,ntry, wf8(:,53), wf8(:,17), wf64(:,10), n3(:,185), t3x64(:,:,10))
  call prop_Q_A(ntry, wf16(:,151), Q(:,83), MB, 1_intkind1, wf16(:,153), n2(88))
  call prop_Q_A(ntry, wf16(:,152), Q(:,83), MB, 1_intkind1, wf16(:,154), n2(89))
  call prop_A_Q(ntry, wf16(:,149), Q(:,99), MB, 1_intkind1, wf16(:,155), n2(90))
  call prop_A_Q(ntry, wf16(:,150), Q(:,99), MB, 1_intkind1, wf16(:,156), n2(91))
  call vert_AV_Q(ntry, ex2, ex7, wf4(:,24), n3(:,186), t3x4(:,:,14))
  call prop_A_Q(ntry, wf4(:,24), Q(:,66), ZERO, 0_intkind1, wf4(:,25), n2(92))
  call vert_QA_V(ntry, ex1, wf4(:,25), wf8(:,54), n3(:,187), t3x8(:,:,33))
  call vert_AV_Q(ntry, ex6, wf8(:,54), wf16(:,157), n3(:,188), t3x16(:,:,97))
  call vert_QA_Z(gZl,ntry, ex1, wf4(:,25), wf8(:,55), n3(:,189), t3x8(:,:,34))
  call prop_W_W(ntry, wf8(:,55), Q(:,67), MZ, 1_intkind1, wf8(:,56), n2(93))
  call vert_AZ_Q(gZd,ntry, ex6, wf8(:,56), wf16(:,158), n3(:,190), t3x16(:,:,98))
  call vert_VQ_A(ntry, wf8(:,54), wf4(:,11), wf32(:,35), n3(:,191), t3x32(:,:,35))
  call vert_ZQ_A(gZd,ntry, wf8(:,56), wf4(:,11), wf32(:,36), n3(:,192), t3x32(:,:,36))
  call vert_VQ_A(ntry, wf8(:,54), ex5, wf16(:,159), n3(:,193), t3x16(:,:,99))
  call vert_ZQ_A(gZd,ntry, wf8(:,56), ex5, wf16(:,160), n3(:,194), t3x16(:,:,100))
  call vert_VQ_A(ntry, wf8(:,54), wf8(:,17), wf64(:,11), n3(:,195), t3x64(:,:,11))
  call vert_ZQ_A(gZd,ntry, wf8(:,56), wf8(:,17), wf64(:,12), n3(:,196), t3x64(:,:,12))
  call prop_Q_A(ntry, wf16(:,159), Q(:,83), MB, 1_intkind1, wf16(:,161), n2(94))
  call prop_Q_A(ntry, wf16(:,160), Q(:,83), MB, 1_intkind1, wf16(:,162), n2(95))
  call prop_A_Q(ntry, wf16(:,157), Q(:,99), MB, 1_intkind1, wf16(:,163), n2(96))
  call prop_A_Q(ntry, wf16(:,158), Q(:,99), MB, 1_intkind1, wf16(:,164), n2(97))
  call vert_AV_Q(ntry, ex4, wf8(:,51), wf16(:,165), n3(:,197), t3x16(:,:,101))
  call vert_AZ_Q(gZd,ntry, ex4, wf8(:,53), wf16(:,166), n3(:,198), t3x16(:,:,102))
  call vert_VQ_A(ntry, wf8(:,51), wf4(:,20), wf32(:,37), n3(:,199), t3x32(:,:,37))
  call vert_ZQ_A(gZd,ntry, wf8(:,53), wf4(:,20), wf32(:,38), n3(:,200), t3x32(:,:,38))
  call vert_VQ_A(ntry, wf8(:,51), ex3, wf16(:,167), n3(:,201), t3x16(:,:,103))
  call vert_ZQ_A(gZd,ntry, wf8(:,53), ex3, wf16(:,168), n3(:,202), t3x16(:,:,104))
  call vert_VQ_A(ntry, wf8(:,51), wf8(:,38), wf64(:,13), n3(:,203), t3x64(:,:,13))
  call vert_ZQ_A(gZd,ntry, wf8(:,53), wf8(:,38), wf64(:,14), n3(:,204), t3x64(:,:,14))
  call prop_Q_A(ntry, wf16(:,167), Q(:,71), ZERO, 0_intkind1, wf16(:,169), n2(98))
  call prop_Q_A(ntry, wf16(:,168), Q(:,71), ZERO, 0_intkind1, wf16(:,170), n2(99))
  call prop_A_Q(ntry, wf16(:,165), Q(:,75), ZERO, 0_intkind1, wf16(:,171), n2(100))
  call prop_A_Q(ntry, wf16(:,166), Q(:,75), ZERO, 0_intkind1, wf16(:,172), n2(101))
  call vert_AV_Q(ntry, ex4, wf8(:,54), wf16(:,173), n3(:,205), t3x16(:,:,105))
  call vert_AZ_Q(gZd,ntry, ex4, wf8(:,56), wf16(:,174), n3(:,206), t3x16(:,:,106))
  call vert_VQ_A(ntry, wf8(:,54), wf4(:,20), wf32(:,39), n3(:,207), t3x32(:,:,39))
  call vert_ZQ_A(gZd,ntry, wf8(:,56), wf4(:,20), wf32(:,40), n3(:,208), t3x32(:,:,40))
  call vert_VQ_A(ntry, wf8(:,54), ex3, wf16(:,175), n3(:,209), t3x16(:,:,107))
  call vert_ZQ_A(gZd,ntry, wf8(:,56), ex3, wf16(:,176), n3(:,210), t3x16(:,:,108))
  call vert_VQ_A(ntry, wf8(:,54), wf8(:,38), wf64(:,15), n3(:,211), t3x64(:,:,15))
  call vert_ZQ_A(gZd,ntry, wf8(:,56), wf8(:,38), wf64(:,16), n3(:,212), t3x64(:,:,16))
  call prop_Q_A(ntry, wf16(:,175), Q(:,71), ZERO, 0_intkind1, wf16(:,177), n2(102))
  call prop_Q_A(ntry, wf16(:,176), Q(:,71), ZERO, 0_intkind1, wf16(:,178), n2(103))
  call prop_A_Q(ntry, wf16(:,173), Q(:,75), ZERO, 0_intkind1, wf16(:,179), n2(104))
  call prop_A_Q(ntry, wf16(:,174), Q(:,75), ZERO, 0_intkind1, wf16(:,180), n2(105))


  ! colour-stripped amplitudes
  do nsync = ntry+ntry-1, ntry+1  !  nsync = 1,2  for 1st point and nsync = 3 later
    call diagrams()
    if (nsync == 1) then
      call helsync(nsync, A, nhel, Hel)
      call helsync_flip(nsync, nhel, Hel, eflip, exthel)
    end if
  end do

  do k = 1, nhel
    call colourvector(A, k, M1helarray(:,k))
  end do
  M1helarray(:,nhel+1:) = 0
  M1helarr = M1helarray ! fill cache

  M2 = 0
  if (ReplacePol == 0) then ! no helicity correlation

    do k = 1, nhel
      call colint(M1helarray(:,k), M2add, extcombs_permuted)
      M2 = M2 + M2add
    end do

  else ! helicity correlation

    call flip_phase(P(:,ReplacePol), firstpol(ReplacePol), MOM, omega)
    do k = 1, nhel
      M1 = M1helarray(:,k)
      r = eflip(k, ReplacePol) ! Flip helicity of external particle ReplacePol (gluon emitter).
      if (r <= nhel) then      ! Only add flipped helicity configuration if it doesn't vanish.
        M1 = M1 + omega(exthel(k,ReplacePol)) * M1helarray(:,r)
      end if
      call colint(M1, M2add, extcombs_permuted)
      M2 = M2 + M2add
    end do
    M2 = 0.25_/**/REALKIND * M2

  end if

  if ( JBmunu /= 0 ) then ! Bmunu helicity correlation
    M2munu = 0
  end if

  ! Colour and helicity average and symmetry factor of outgoing particles
  M2add = M2 / average_factor_ppllajj_eexddxbbxag_1

  do k = 0, 38-1
    M2(k) = M2add(extcomb_perm_ppllajj_eexddxbbxag_1(k))
  end do

  if (ntry == 1) ntry = 2

  if (scalefactor /= old_scalefactor) then
    scalebackfactor = scalefactor**(2*8-8)
    old_scalefactor = scalefactor
  end if
  M2 = scalebackfactor * M2

  contains

subroutine diagrams()
  implicit none
  ! e.g. call cont_VV(nsync, wf3, wf6, A(:,1), n64, t64, nhel, den(5)) ...

    call cont_QA(nsync, wf16(:,2), wf16(:,3), A(:,1), n3(:,213), t3x256(:,:,1), nhel, den(9))
    call cont_QA(nsync, wf16(:,2), wf16(:,5), A(:,2), n3(:,214), t3x256(:,:,2), nhel, den(13))
    call cont_QA(nsync, wf16(:,7), wf16(:,8), A(:,3), n3(:,215), t3x256(:,:,3), nhel, den(18))
    call cont_QA(nsync, wf16(:,7), wf16(:,10), A(:,4), n3(:,216), t3x256(:,:,4), nhel, den(21))
    call cont_VV(nsync, wf8(:,2), wf32(:,1), A(:,5), n3(:,217), t3x256(:,:,5), nhel, den(27))
    call cont_VV(nsync, wf8(:,2), wf32(:,2), A(:,6), n3(:,218), t3x256(:,:,6), nhel, den(30))
    call cont_QA(nsync, wf8(:,7), wf32(:,3), A(:,7), n3(:,219), t3x256(:,:,7), nhel, den(34))
    call cont_QA(nsync, wf8(:,7), wf32(:,4), A(:,8), n3(:,220), t3x256(:,:,8), nhel, den(36))
    call cont_QA(nsync, wf16(:,11), wf16(:,12), A(:,9), n3(:,221), t3x256(:,:,9), nhel, den(39))
    call cont_QA(nsync, wf16(:,11), wf16(:,13), A(:,10), n3(:,222), t3x256(:,:,10), nhel, den(40))
    call cont_QA(nsync, wf8(:,7), wf32(:,5), A(:,11), n3(:,223), t3x256(:,:,11), nhel, den(44))
    call cont_QA(nsync, wf8(:,7), wf32(:,6), A(:,12), n3(:,224), t3x256(:,:,12), nhel, den(46))
    call cont_QA(nsync, wf16(:,3), wf16(:,14), A(:,13), n3(:,225), t3x256(:,:,13), nhel, den(47))
    call cont_QA(nsync, wf16(:,5), wf16(:,14), A(:,14), n3(:,226), t3x256(:,:,14), nhel, den(48))
    call cont_QA(nsync, wf16(:,3), wf16(:,15), A(:,15), n3(:,227), t3x256(:,:,15), nhel, den(49))
    call cont_QA(nsync, wf16(:,5), wf16(:,15), A(:,16), n3(:,228), t3x256(:,:,16), nhel, den(50))
    call cont_QA(nsync, wf16(:,17), wf16(:,18), A(:,17), n3(:,229), t3x256(:,:,17), nhel, den(57))
    call cont_QA(nsync, wf16(:,17), wf16(:,20), A(:,18), n3(:,230), t3x256(:,:,18), nhel, den(60))
    call cont_QA(nsync, wf16(:,22), wf16(:,23), A(:,19), n3(:,231), t3x256(:,:,19), nhel, den(65))
    call cont_QA(nsync, wf16(:,22), wf16(:,25), A(:,20), n3(:,232), t3x256(:,:,20), nhel, den(68))
    call cont_VV(nsync, wf8(:,2), wf32(:,7), A(:,21), n3(:,233), t3x256(:,:,21), nhel, den(72))
    call cont_VV(nsync, wf8(:,2), wf32(:,8), A(:,22), n3(:,234), t3x256(:,:,22), nhel, den(75))
    call cont_QA(nsync, wf16(:,26), wf16(:,27), A(:,23), n3(:,235), t3x256(:,:,23), nhel, den(78))
    call cont_QA(nsync, wf16(:,26), wf16(:,28), A(:,24), n3(:,236), t3x256(:,:,24), nhel, den(79))
    call cont_QA(nsync, wf8(:,15), wf32(:,9), A(:,25), n3(:,237), t3x256(:,:,25), nhel, den(83))
    call cont_QA(nsync, wf8(:,15), wf32(:,10), A(:,26), n3(:,238), t3x256(:,:,26), nhel, den(85))
    call cont_QA(nsync, wf16(:,23), wf16(:,29), A(:,27), n3(:,239), t3x256(:,:,27), nhel, den(88))
    call cont_QA(nsync, wf16(:,25), wf16(:,29), A(:,28), n3(:,240), t3x256(:,:,28), nhel, den(89))
    call cont_QA(nsync, wf8(:,15), wf32(:,11), A(:,29), n3(:,241), t3x256(:,:,29), nhel, den(91))
    call cont_QA(nsync, wf8(:,15), wf32(:,12), A(:,30), n3(:,242), t3x256(:,:,30), nhel, den(93))
    call cont_QA(nsync, wf16(:,23), wf16(:,30), A(:,31), n3(:,243), t3x256(:,:,31), nhel, den(94))
    call cont_QA(nsync, wf16(:,25), wf16(:,30), A(:,32), n3(:,244), t3x256(:,:,32), nhel, den(95))
    call cont_QA(nsync, wf32(:,3), wf8(:,19), A(:,33), n3(:,245), t3x256(:,:,33), nhel, den(97))
    call cont_QA(nsync, wf32(:,4), wf8(:,19), A(:,34), n3(:,246), t3x256(:,:,34), nhel, den(98))
    call cont_QA(nsync, wf16(:,31), wf16(:,32), A(:,35), n3(:,247), t3x256(:,:,35), nhel, den(101))
    call cont_QA(nsync, wf16(:,31), wf16(:,33), A(:,36), n3(:,248), t3x256(:,:,36), nhel, den(102))
    call cont_QA(nsync, wf32(:,5), wf8(:,19), A(:,37), n3(:,249), t3x256(:,:,37), nhel, den(103))
    call cont_QA(nsync, wf32(:,6), wf8(:,19), A(:,38), n3(:,250), t3x256(:,:,38), nhel, den(104))
    call cont_QA(nsync, wf16(:,18), wf16(:,34), A(:,39), n3(:,251), t3x256(:,:,39), nhel, den(105))
    call cont_QA(nsync, wf16(:,20), wf16(:,34), A(:,40), n3(:,252), t3x256(:,:,40), nhel, den(106))
    call cont_QA(nsync, wf16(:,35), wf16(:,36), A(:,41), n3(:,253), t3x256(:,:,41), nhel, den(109))
    call cont_QA(nsync, wf16(:,35), wf16(:,37), A(:,42), n3(:,254), t3x256(:,:,42), nhel, den(110))
    call cont_QA(nsync, wf32(:,9), wf8(:,21), A(:,43), n3(:,255), t3x256(:,:,43), nhel, den(112))
    call cont_QA(nsync, wf32(:,10), wf8(:,21), A(:,44), n3(:,256), t3x256(:,:,44), nhel, den(113))
    call cont_QA(nsync, wf16(:,8), wf16(:,38), A(:,45), n3(:,257), t3x256(:,:,45), nhel, den(114))
    call cont_QA(nsync, wf16(:,10), wf16(:,38), A(:,46), n3(:,258), t3x256(:,:,46), nhel, den(115))
    call cont_QA(nsync, wf32(:,11), wf8(:,21), A(:,47), n3(:,259), t3x256(:,:,47), nhel, den(116))
    call cont_QA(nsync, wf32(:,12), wf8(:,21), A(:,48), n3(:,260), t3x256(:,:,48), nhel, den(117))
    call cont_QA(nsync, wf16(:,14), wf16(:,39), A(:,49), n3(:,261), t3x256(:,:,49), nhel, den(119))
    call cont_QA(nsync, wf16(:,14), wf16(:,40), A(:,50), n3(:,262), t3x256(:,:,50), nhel, den(121))
    call cont_QA(nsync, wf16(:,27), wf16(:,41), A(:,51), n3(:,263), t3x256(:,:,51), nhel, den(123))
    call cont_QA(nsync, wf16(:,28), wf16(:,41), A(:,52), n3(:,264), t3x256(:,:,52), nhel, den(124))
    call cont_QA(nsync, wf16(:,36), wf16(:,42), A(:,53), n3(:,265), t3x256(:,:,53), nhel, den(126))
    call cont_QA(nsync, wf16(:,37), wf16(:,42), A(:,54), n3(:,266), t3x256(:,:,54), nhel, den(127))
    call cont_QA(nsync, wf16(:,12), wf16(:,43), A(:,55), n3(:,267), t3x256(:,:,55), nhel, den(129))
    call cont_QA(nsync, wf16(:,13), wf16(:,43), A(:,56), n3(:,268), t3x256(:,:,56), nhel, den(130))
    call cont_QA(nsync, wf16(:,29), wf16(:,44), A(:,57), n3(:,269), t3x256(:,:,57), nhel, den(132))
    call cont_QA(nsync, wf16(:,29), wf16(:,45), A(:,58), n3(:,270), t3x256(:,:,58), nhel, den(134))
    call cont_QA(nsync, wf16(:,32), wf16(:,46), A(:,59), n3(:,271), t3x256(:,:,59), nhel, den(136))
    call cont_QA(nsync, wf16(:,33), wf16(:,46), A(:,60), n3(:,272), t3x256(:,:,60), nhel, den(137))
    call cont_QA(nsync, wf16(:,48), wf16(:,49), A(:,61), n3(:,273), t3x256(:,:,61), nhel, den(145))
    call cont_QA(nsync, wf16(:,48), wf16(:,51), A(:,62), n3(:,274), t3x256(:,:,62), nhel, den(148))
    call cont_QA(nsync, wf16(:,53), wf16(:,54), A(:,63), n3(:,275), t3x256(:,:,63), nhel, den(153))
    call cont_QA(nsync, wf16(:,53), wf16(:,56), A(:,64), n3(:,276), t3x256(:,:,64), nhel, den(156))
    call cont_QA(nsync, wf8(:,25), wf32(:,13), A(:,65), n3(:,277), t3x256(:,:,65), nhel, den(162))
    call cont_QA(nsync, wf8(:,25), wf32(:,14), A(:,66), n3(:,278), t3x256(:,:,66), nhel, den(165))
    call cont_QA(nsync, wf16(:,57), wf16(:,58), A(:,67), n3(:,279), t3x256(:,:,67), nhel, den(168))
    call cont_QA(nsync, wf16(:,57), wf16(:,59), A(:,68), n3(:,280), t3x256(:,:,68), nhel, den(169))
    call cont_VV(nsync, wf8(:,28), wf32(:,15), A(:,69), n3(:,281), t3x256(:,:,69), nhel, den(173))
    call cont_VV(nsync, wf8(:,28), wf32(:,16), A(:,70), n3(:,282), t3x256(:,:,70), nhel, den(175))
    call cont_QA(nsync, wf8(:,25), wf32(:,17), A(:,71), n3(:,283), t3x256(:,:,71), nhel, den(179))
    call cont_QA(nsync, wf8(:,25), wf32(:,18), A(:,72), n3(:,284), t3x256(:,:,72), nhel, den(181))
    call cont_QA(nsync, wf16(:,49), wf16(:,60), A(:,73), n3(:,285), t3x256(:,:,73), nhel, den(182))
    call cont_QA(nsync, wf16(:,51), wf16(:,60), A(:,74), n3(:,286), t3x256(:,:,74), nhel, den(183))
    call cont_QA(nsync, wf16(:,49), wf16(:,61), A(:,75), n3(:,287), t3x256(:,:,75), nhel, den(184))
    call cont_QA(nsync, wf16(:,51), wf16(:,61), A(:,76), n3(:,288), t3x256(:,:,76), nhel, den(185))
    call cont_QA(nsync, wf16(:,63), wf16(:,64), A(:,77), n3(:,289), t3x256(:,:,77), nhel, den(192))
    call cont_QA(nsync, wf16(:,63), wf16(:,66), A(:,78), n3(:,290), t3x256(:,:,78), nhel, den(195))
    call cont_QA(nsync, wf16(:,68), wf16(:,69), A(:,79), n3(:,291), t3x256(:,:,79), nhel, den(200))
    call cont_QA(nsync, wf16(:,68), wf16(:,71), A(:,80), n3(:,292), t3x256(:,:,80), nhel, den(203))
    call cont_QA(nsync, wf16(:,72), wf16(:,73), A(:,81), n3(:,293), t3x256(:,:,81), nhel, den(208))
    call cont_QA(nsync, wf16(:,72), wf16(:,74), A(:,82), n3(:,294), t3x256(:,:,82), nhel, den(210))
    call cont_QA(nsync, wf8(:,36), wf32(:,19), A(:,83), n3(:,295), t3x256(:,:,83), nhel, den(214))
    call cont_QA(nsync, wf8(:,36), wf32(:,20), A(:,84), n3(:,296), t3x256(:,:,84), nhel, den(216))
    call cont_VV(nsync, wf8(:,28), wf32(:,21), A(:,85), n3(:,297), t3x256(:,:,85), nhel, den(218))
    call cont_VV(nsync, wf8(:,28), wf32(:,22), A(:,86), n3(:,298), t3x256(:,:,86), nhel, den(220))
    call cont_QA(nsync, wf16(:,69), wf16(:,75), A(:,87), n3(:,299), t3x256(:,:,87), nhel, den(223))
    call cont_QA(nsync, wf16(:,71), wf16(:,75), A(:,88), n3(:,300), t3x256(:,:,88), nhel, den(224))
    call cont_QA(nsync, wf8(:,36), wf32(:,23), A(:,89), n3(:,301), t3x256(:,:,89), nhel, den(226))
    call cont_QA(nsync, wf8(:,36), wf32(:,24), A(:,90), n3(:,302), t3x256(:,:,90), nhel, den(228))
    call cont_QA(nsync, wf16(:,69), wf16(:,76), A(:,91), n3(:,303), t3x256(:,:,91), nhel, den(229))
    call cont_QA(nsync, wf16(:,71), wf16(:,76), A(:,92), n3(:,304), t3x256(:,:,92), nhel, den(230))
    call cont_QA(nsync, wf32(:,13), wf8(:,40), A(:,93), n3(:,305), t3x256(:,:,93), nhel, den(232))
    call cont_QA(nsync, wf32(:,14), wf8(:,40), A(:,94), n3(:,306), t3x256(:,:,94), nhel, den(233))
    call cont_QA(nsync, wf16(:,77), wf16(:,78), A(:,95), n3(:,307), t3x256(:,:,95), nhel, den(236))
    call cont_QA(nsync, wf16(:,77), wf16(:,79), A(:,96), n3(:,308), t3x256(:,:,96), nhel, den(237))
    call cont_QA(nsync, wf32(:,17), wf8(:,40), A(:,97), n3(:,309), t3x256(:,:,97), nhel, den(238))
    call cont_QA(nsync, wf32(:,18), wf8(:,40), A(:,98), n3(:,310), t3x256(:,:,98), nhel, den(239))
    call cont_QA(nsync, wf16(:,64), wf16(:,80), A(:,99), n3(:,311), t3x256(:,:,99), nhel, den(240))
    call cont_QA(nsync, wf16(:,66), wf16(:,80), A(:,100), n3(:,312), t3x256(:,:,100), nhel, den(241))
    call cont_QA(nsync, wf16(:,81), wf16(:,82), A(:,101), n3(:,313), t3x256(:,:,101), nhel, den(244))
    call cont_QA(nsync, wf16(:,81), wf16(:,83), A(:,102), n3(:,314), t3x256(:,:,102), nhel, den(245))
    call cont_QA(nsync, wf32(:,19), wf8(:,42), A(:,103), n3(:,315), t3x256(:,:,103), nhel, den(247))
    call cont_QA(nsync, wf32(:,20), wf8(:,42), A(:,104), n3(:,316), t3x256(:,:,104), nhel, den(248))
    call cont_QA(nsync, wf16(:,54), wf16(:,84), A(:,105), n3(:,317), t3x256(:,:,105), nhel, den(249))
    call cont_QA(nsync, wf16(:,56), wf16(:,84), A(:,106), n3(:,318), t3x256(:,:,106), nhel, den(250))
    call cont_QA(nsync, wf32(:,23), wf8(:,42), A(:,107), n3(:,319), t3x256(:,:,107), nhel, den(251))
    call cont_QA(nsync, wf32(:,24), wf8(:,42), A(:,108), n3(:,320), t3x256(:,:,108), nhel, den(252))
    call cont_QA(nsync, wf16(:,60), wf16(:,85), A(:,109), n3(:,321), t3x256(:,:,109), nhel, den(254))
    call cont_QA(nsync, wf16(:,60), wf16(:,86), A(:,110), n3(:,322), t3x256(:,:,110), nhel, den(256))
    call cont_QA(nsync, wf16(:,82), wf16(:,87), A(:,111), n3(:,323), t3x256(:,:,111), nhel, den(258))
    call cont_QA(nsync, wf16(:,83), wf16(:,87), A(:,112), n3(:,324), t3x256(:,:,112), nhel, den(259))
    call cont_QA(nsync, wf16(:,73), wf16(:,88), A(:,113), n3(:,325), t3x256(:,:,113), nhel, den(261))
    call cont_QA(nsync, wf16(:,74), wf16(:,88), A(:,114), n3(:,326), t3x256(:,:,114), nhel, den(262))
    call cont_QA(nsync, wf16(:,58), wf16(:,89), A(:,115), n3(:,327), t3x256(:,:,115), nhel, den(264))
    call cont_QA(nsync, wf16(:,59), wf16(:,89), A(:,116), n3(:,328), t3x256(:,:,116), nhel, den(265))
    call cont_QA(nsync, wf16(:,75), wf16(:,90), A(:,117), n3(:,329), t3x256(:,:,117), nhel, den(267))
    call cont_QA(nsync, wf16(:,75), wf16(:,91), A(:,118), n3(:,330), t3x256(:,:,118), nhel, den(269))
    call cont_QA(nsync, wf16(:,78), wf16(:,92), A(:,119), n3(:,331), t3x256(:,:,119), nhel, den(271))
    call cont_QA(nsync, wf16(:,79), wf16(:,92), A(:,120), n3(:,332), t3x256(:,:,120), nhel, den(272))
    call cont_VV(nsync, wf16(:,93), wf16(:,94), A(:,121), n3(:,333), t3x256(:,:,121), nhel, den(276))
    call cont_VV(nsync, wf16(:,93), wf16(:,95), A(:,122), n3(:,334), t3x256(:,:,122), nhel, den(277))
    call cont_VV(nsync, wf16(:,93), wf16(:,96), A(:,123), n3(:,335), t3x256(:,:,123), nhel, den(278))
    call cont_VV(nsync, wf16(:,93), wf16(:,97), A(:,124), n3(:,336), t3x256(:,:,124), nhel, den(279))
    call cont_VV(nsync, wf32(:,15), wf8(:,43), A(:,125), n3(:,337), t3x256(:,:,125), nhel, den(281))
    call cont_VV(nsync, wf32(:,16), wf8(:,43), A(:,126), n3(:,338), t3x256(:,:,126), nhel, den(282))
    call cont_QA(nsync, wf8(:,3), wf32(:,25), A(:,127), n3(:,339), t3x256(:,:,127), nhel, den(286))
    call cont_QA(nsync, wf8(:,5), wf32(:,25), A(:,128), n3(:,340), t3x256(:,:,128), nhel, den(287))
    call cont_QA(nsync, wf16(:,18), wf16(:,98), A(:,129), n3(:,341), t3x256(:,:,129), nhel, den(288))
    call cont_QA(nsync, wf16(:,20), wf16(:,98), A(:,130), n3(:,342), t3x256(:,:,130), nhel, den(289))
    call cont_QA(nsync, wf16(:,49), wf16(:,99), A(:,131), n3(:,343), t3x256(:,:,131), nhel, den(290))
    call cont_QA(nsync, wf16(:,51), wf16(:,99), A(:,132), n3(:,344), t3x256(:,:,132), nhel, den(291))
    call cont_VV(nsync, wf32(:,15), wf8(:,45), A(:,133), n3(:,345), t3x256(:,:,133), nhel, den(293))
    call cont_VV(nsync, wf32(:,16), wf8(:,45), A(:,134), n3(:,346), t3x256(:,:,134), nhel, den(294))
    call cont_QA(nsync, wf4(:,6), wf64(:,1), A(:,135), n3(:,347), t3x256(:,:,135), nhel, den(296))
    call cont_QA(nsync, wf4(:,6), wf64(:,2), A(:,136), n3(:,348), t3x256(:,:,136), nhel, den(298))
    call cont_QA(nsync, wf16(:,8), wf16(:,100), A(:,137), n3(:,349), t3x256(:,:,137), nhel, den(299))
    call cont_QA(nsync, wf16(:,10), wf16(:,100), A(:,138), n3(:,350), t3x256(:,:,138), nhel, den(300))
    call cont_QA(nsync, wf16(:,49), wf16(:,101), A(:,139), n3(:,351), t3x256(:,:,139), nhel, den(301))
    call cont_QA(nsync, wf16(:,51), wf16(:,101), A(:,140), n3(:,352), t3x256(:,:,140), nhel, den(302))
    call cont_QA(nsync, wf16(:,27), wf16(:,102), A(:,141), n3(:,353), t3x256(:,:,141), nhel, den(304))
    call cont_QA(nsync, wf16(:,28), wf16(:,102), A(:,142), n3(:,354), t3x256(:,:,142), nhel, den(305))
    call cont_VV(nsync, wf16(:,94), wf16(:,103), A(:,143), n3(:,355), t3x256(:,:,143), nhel, den(308))
    call cont_VV(nsync, wf16(:,95), wf16(:,103), A(:,144), n3(:,356), t3x256(:,:,144), nhel, den(310))
    call cont_VV(nsync, wf16(:,94), wf16(:,104), A(:,145), n3(:,357), t3x256(:,:,145), nhel, den(312))
    call cont_VV(nsync, wf16(:,95), wf16(:,104), A(:,146), n3(:,358), t3x256(:,:,146), nhel, den(313))
    call cont_QA(nsync, wf16(:,12), wf16(:,105), A(:,147), n3(:,359), t3x256(:,:,147), nhel, den(315))
    call cont_QA(nsync, wf16(:,13), wf16(:,105), A(:,148), n3(:,360), t3x256(:,:,148), nhel, den(316))
    call cont_VV(nsync, wf16(:,96), wf16(:,103), A(:,149), n3(:,361), t3x256(:,:,149), nhel, den(318))
    call cont_VV(nsync, wf16(:,97), wf16(:,103), A(:,150), n3(:,362), t3x256(:,:,150), nhel, den(320))
    call cont_VV(nsync, wf16(:,96), wf16(:,104), A(:,151), n3(:,363), t3x256(:,:,151), nhel, den(321))
    call cont_VV(nsync, wf16(:,97), wf16(:,104), A(:,152), n3(:,364), t3x256(:,:,152), nhel, den(322))
    call cont_VV(nsync, wf16(:,94), wf16(:,106), A(:,153), n3(:,365), t3x256(:,:,153), nhel, den(325))
    call cont_VV(nsync, wf16(:,95), wf16(:,106), A(:,154), n3(:,366), t3x256(:,:,154), nhel, den(326))
    call cont_VV(nsync, wf16(:,96), wf16(:,106), A(:,155), n3(:,367), t3x256(:,:,155), nhel, den(327))
    call cont_VV(nsync, wf16(:,97), wf16(:,106), A(:,156), n3(:,368), t3x256(:,:,156), nhel, den(328))
    call cont_VV(nsync, wf32(:,21), wf8(:,43), A(:,157), n3(:,369), t3x256(:,:,157), nhel, den(329))
    call cont_VV(nsync, wf32(:,22), wf8(:,43), A(:,158), n3(:,370), t3x256(:,:,158), nhel, den(330))
    call cont_QA(nsync, wf8(:,3), wf32(:,26), A(:,159), n3(:,371), t3x256(:,:,159), nhel, den(333))
    call cont_QA(nsync, wf8(:,5), wf32(:,26), A(:,160), n3(:,372), t3x256(:,:,160), nhel, den(334))
    call cont_QA(nsync, wf16(:,18), wf16(:,107), A(:,161), n3(:,373), t3x256(:,:,161), nhel, den(335))
    call cont_QA(nsync, wf16(:,20), wf16(:,107), A(:,162), n3(:,374), t3x256(:,:,162), nhel, den(336))
    call cont_QA(nsync, wf16(:,69), wf16(:,108), A(:,163), n3(:,375), t3x256(:,:,163), nhel, den(337))
    call cont_QA(nsync, wf16(:,71), wf16(:,108), A(:,164), n3(:,376), t3x256(:,:,164), nhel, den(338))
    call cont_VV(nsync, wf32(:,21), wf8(:,45), A(:,165), n3(:,377), t3x256(:,:,165), nhel, den(339))
    call cont_VV(nsync, wf32(:,22), wf8(:,45), A(:,166), n3(:,378), t3x256(:,:,166), nhel, den(340))
    call cont_QA(nsync, wf4(:,6), wf64(:,3), A(:,167), n3(:,379), t3x256(:,:,167), nhel, den(342))
    call cont_QA(nsync, wf4(:,6), wf64(:,4), A(:,168), n3(:,380), t3x256(:,:,168), nhel, den(344))
    call cont_QA(nsync, wf16(:,8), wf16(:,109), A(:,169), n3(:,381), t3x256(:,:,169), nhel, den(345))
    call cont_QA(nsync, wf16(:,10), wf16(:,109), A(:,170), n3(:,382), t3x256(:,:,170), nhel, den(346))
    call cont_QA(nsync, wf16(:,69), wf16(:,110), A(:,171), n3(:,383), t3x256(:,:,171), nhel, den(347))
    call cont_QA(nsync, wf16(:,71), wf16(:,110), A(:,172), n3(:,384), t3x256(:,:,172), nhel, den(348))
    call cont_QA(nsync, wf16(:,27), wf16(:,111), A(:,173), n3(:,385), t3x256(:,:,173), nhel, den(350))
    call cont_QA(nsync, wf16(:,28), wf16(:,111), A(:,174), n3(:,386), t3x256(:,:,174), nhel, den(351))
    call cont_VV(nsync, wf16(:,94), wf16(:,112), A(:,175), n3(:,387), t3x256(:,:,175), nhel, den(352))
    call cont_VV(nsync, wf16(:,95), wf16(:,112), A(:,176), n3(:,388), t3x256(:,:,176), nhel, den(353))
    call cont_VV(nsync, wf16(:,94), wf16(:,113), A(:,177), n3(:,389), t3x256(:,:,177), nhel, den(355))
    call cont_VV(nsync, wf16(:,95), wf16(:,113), A(:,178), n3(:,390), t3x256(:,:,178), nhel, den(356))
    call cont_QA(nsync, wf16(:,12), wf16(:,114), A(:,179), n3(:,391), t3x256(:,:,179), nhel, den(358))
    call cont_QA(nsync, wf16(:,13), wf16(:,114), A(:,180), n3(:,392), t3x256(:,:,180), nhel, den(359))
    call cont_VV(nsync, wf16(:,96), wf16(:,112), A(:,181), n3(:,393), t3x256(:,:,181), nhel, den(360))
    call cont_VV(nsync, wf16(:,97), wf16(:,112), A(:,182), n3(:,394), t3x256(:,:,182), nhel, den(361))
    call cont_VV(nsync, wf16(:,96), wf16(:,113), A(:,183), n3(:,395), t3x256(:,:,183), nhel, den(362))
    call cont_VV(nsync, wf16(:,97), wf16(:,113), A(:,184), n3(:,396), t3x256(:,:,184), nhel, den(363))
    call cont_VV(nsync, wf8(:,47), wf32(:,27), A(:,185), n3(:,397), t3x256(:,:,185), nhel, den(367))
    call cont_VV(nsync, wf8(:,47), wf32(:,28), A(:,186), n3(:,398), t3x256(:,:,186), nhel, den(369))
    call cont_QA(nsync, wf8(:,3), wf32(:,29), A(:,187), n3(:,399), t3x256(:,:,187), nhel, den(372))
    call cont_QA(nsync, wf8(:,5), wf32(:,29), A(:,188), n3(:,400), t3x256(:,:,188), nhel, den(373))
    call cont_QA(nsync, wf16(:,3), wf16(:,115), A(:,189), n3(:,401), t3x256(:,:,189), nhel, den(374))
    call cont_QA(nsync, wf16(:,5), wf16(:,115), A(:,190), n3(:,402), t3x256(:,:,190), nhel, den(375))
    call cont_QA(nsync, wf16(:,64), wf16(:,116), A(:,191), n3(:,403), t3x256(:,:,191), nhel, den(376))
    call cont_QA(nsync, wf16(:,66), wf16(:,116), A(:,192), n3(:,404), t3x256(:,:,192), nhel, den(377))
    call cont_VV(nsync, wf8(:,47), wf32(:,30), A(:,193), n3(:,405), t3x256(:,:,193), nhel, den(379))
    call cont_VV(nsync, wf8(:,47), wf32(:,31), A(:,194), n3(:,406), t3x256(:,:,194), nhel, den(381))
    call cont_QA(nsync, wf8(:,3), wf32(:,32), A(:,195), n3(:,407), t3x256(:,:,195), nhel, den(384))
    call cont_QA(nsync, wf8(:,5), wf32(:,32), A(:,196), n3(:,408), t3x256(:,:,196), nhel, den(385))
    call cont_QA(nsync, wf16(:,3), wf16(:,117), A(:,197), n3(:,409), t3x256(:,:,197), nhel, den(386))
    call cont_QA(nsync, wf16(:,5), wf16(:,117), A(:,198), n3(:,410), t3x256(:,:,198), nhel, den(387))
    call cont_QA(nsync, wf16(:,54), wf16(:,118), A(:,199), n3(:,411), t3x256(:,:,199), nhel, den(388))
    call cont_QA(nsync, wf16(:,56), wf16(:,118), A(:,200), n3(:,412), t3x256(:,:,200), nhel, den(389))
    call cont_VV(nsync, wf16(:,119), wf16(:,120), A(:,201), n3(:,413), t3x256(:,:,201), nhel, den(393))
    call cont_VV(nsync, wf16(:,119), wf16(:,121), A(:,202), n3(:,414), t3x256(:,:,202), nhel, den(394))
    call cont_VV(nsync, wf16(:,119), wf16(:,122), A(:,203), n3(:,415), t3x256(:,:,203), nhel, den(395))
    call cont_VV(nsync, wf16(:,119), wf16(:,123), A(:,204), n3(:,416), t3x256(:,:,204), nhel, den(396))
    call cont_VV(nsync, wf16(:,120), wf16(:,124), A(:,205), n3(:,417), t3x256(:,:,205), nhel, den(399))
    call cont_VV(nsync, wf16(:,121), wf16(:,124), A(:,206), n3(:,418), t3x256(:,:,206), nhel, den(401))
    call cont_VV(nsync, wf16(:,120), wf16(:,125), A(:,207), n3(:,419), t3x256(:,:,207), nhel, den(402))
    call cont_VV(nsync, wf16(:,121), wf16(:,125), A(:,208), n3(:,420), t3x256(:,:,208), nhel, den(403))
    call cont_QA(nsync, wf16(:,73), wf16(:,126), A(:,209), n3(:,421), t3x256(:,:,209), nhel, den(405))
    call cont_QA(nsync, wf16(:,74), wf16(:,126), A(:,210), n3(:,422), t3x256(:,:,210), nhel, den(406))
    call cont_VV(nsync, wf16(:,122), wf16(:,124), A(:,211), n3(:,423), t3x256(:,:,211), nhel, den(408))
    call cont_VV(nsync, wf16(:,123), wf16(:,124), A(:,212), n3(:,424), t3x256(:,:,212), nhel, den(410))
    call cont_VV(nsync, wf16(:,122), wf16(:,125), A(:,213), n3(:,425), t3x256(:,:,213), nhel, den(411))
    call cont_VV(nsync, wf16(:,123), wf16(:,125), A(:,214), n3(:,426), t3x256(:,:,214), nhel, den(412))
    call cont_QA(nsync, wf16(:,58), wf16(:,127), A(:,215), n3(:,427), t3x256(:,:,215), nhel, den(414))
    call cont_QA(nsync, wf16(:,59), wf16(:,127), A(:,216), n3(:,428), t3x256(:,:,216), nhel, den(415))
    call cont_VV(nsync, wf32(:,27), wf8(:,50), A(:,217), n3(:,429), t3x256(:,:,217), nhel, den(417))
    call cont_VV(nsync, wf32(:,28), wf8(:,50), A(:,218), n3(:,430), t3x256(:,:,218), nhel, den(418))
    call cont_QA(nsync, wf4(:,12), wf64(:,5), A(:,219), n3(:,431), t3x256(:,:,219), nhel, den(420))
    call cont_QA(nsync, wf4(:,12), wf64(:,6), A(:,220), n3(:,432), t3x256(:,:,220), nhel, den(422))
    call cont_QA(nsync, wf16(:,23), wf16(:,128), A(:,221), n3(:,433), t3x256(:,:,221), nhel, den(423))
    call cont_QA(nsync, wf16(:,25), wf16(:,128), A(:,222), n3(:,434), t3x256(:,:,222), nhel, den(424))
    call cont_QA(nsync, wf16(:,64), wf16(:,129), A(:,223), n3(:,435), t3x256(:,:,223), nhel, den(425))
    call cont_QA(nsync, wf16(:,66), wf16(:,129), A(:,224), n3(:,436), t3x256(:,:,224), nhel, den(426))
    call cont_VV(nsync, wf32(:,30), wf8(:,50), A(:,225), n3(:,437), t3x256(:,:,225), nhel, den(427))
    call cont_VV(nsync, wf32(:,31), wf8(:,50), A(:,226), n3(:,438), t3x256(:,:,226), nhel, den(428))
    call cont_QA(nsync, wf4(:,12), wf64(:,7), A(:,227), n3(:,439), t3x256(:,:,227), nhel, den(430))
    call cont_QA(nsync, wf4(:,12), wf64(:,8), A(:,228), n3(:,440), t3x256(:,:,228), nhel, den(432))
    call cont_QA(nsync, wf16(:,23), wf16(:,130), A(:,229), n3(:,441), t3x256(:,:,229), nhel, den(433))
    call cont_QA(nsync, wf16(:,25), wf16(:,130), A(:,230), n3(:,442), t3x256(:,:,230), nhel, den(434))
    call cont_QA(nsync, wf16(:,54), wf16(:,131), A(:,231), n3(:,443), t3x256(:,:,231), nhel, den(435))
    call cont_QA(nsync, wf16(:,56), wf16(:,131), A(:,232), n3(:,444), t3x256(:,:,232), nhel, den(436))
    call cont_VV(nsync, wf16(:,120), wf16(:,132), A(:,233), n3(:,445), t3x256(:,:,233), nhel, den(439))
    call cont_VV(nsync, wf16(:,121), wf16(:,132), A(:,234), n3(:,446), t3x256(:,:,234), nhel, den(440))
    call cont_VV(nsync, wf16(:,122), wf16(:,132), A(:,235), n3(:,447), t3x256(:,:,235), nhel, den(441))
    call cont_VV(nsync, wf16(:,123), wf16(:,132), A(:,236), n3(:,448), t3x256(:,:,236), nhel, den(442))
    call cont_VV(nsync, wf16(:,120), wf16(:,133), A(:,237), n3(:,449), t3x256(:,:,237), nhel, den(443))
    call cont_VV(nsync, wf16(:,121), wf16(:,133), A(:,238), n3(:,450), t3x256(:,:,238), nhel, den(444))
    call cont_VV(nsync, wf16(:,120), wf16(:,134), A(:,239), n3(:,451), t3x256(:,:,239), nhel, den(445))
    call cont_VV(nsync, wf16(:,121), wf16(:,134), A(:,240), n3(:,452), t3x256(:,:,240), nhel, den(446))
    call cont_QA(nsync, wf16(:,73), wf16(:,135), A(:,241), n3(:,453), t3x256(:,:,241), nhel, den(448))
    call cont_QA(nsync, wf16(:,74), wf16(:,135), A(:,242), n3(:,454), t3x256(:,:,242), nhel, den(449))
    call cont_VV(nsync, wf16(:,122), wf16(:,133), A(:,243), n3(:,455), t3x256(:,:,243), nhel, den(450))
    call cont_VV(nsync, wf16(:,123), wf16(:,133), A(:,244), n3(:,456), t3x256(:,:,244), nhel, den(451))
    call cont_VV(nsync, wf16(:,122), wf16(:,134), A(:,245), n3(:,457), t3x256(:,:,245), nhel, den(452))
    call cont_VV(nsync, wf16(:,123), wf16(:,134), A(:,246), n3(:,458), t3x256(:,:,246), nhel, den(453))
    call cont_QA(nsync, wf16(:,58), wf16(:,136), A(:,247), n3(:,459), t3x256(:,:,247), nhel, den(455))
    call cont_QA(nsync, wf16(:,59), wf16(:,136), A(:,248), n3(:,460), t3x256(:,:,248), nhel, den(456))
    call cont_QA(nsync, wf16(:,36), wf16(:,137), A(:,249), n3(:,461), t3x256(:,:,249), nhel, den(458))
    call cont_QA(nsync, wf16(:,37), wf16(:,137), A(:,250), n3(:,462), t3x256(:,:,250), nhel, den(459))
    call cont_VV(nsync, wf16(:,94), wf16(:,138), A(:,251), n3(:,463), t3x256(:,:,251), nhel, den(461))
    call cont_VV(nsync, wf16(:,95), wf16(:,138), A(:,252), n3(:,464), t3x256(:,:,252), nhel, den(462))
    call cont_QA(nsync, wf16(:,32), wf16(:,139), A(:,253), n3(:,465), t3x256(:,:,253), nhel, den(464))
    call cont_QA(nsync, wf16(:,33), wf16(:,139), A(:,254), n3(:,466), t3x256(:,:,254), nhel, den(465))
    call cont_VV(nsync, wf16(:,96), wf16(:,138), A(:,255), n3(:,467), t3x256(:,:,255), nhel, den(466))
    call cont_VV(nsync, wf16(:,97), wf16(:,138), A(:,256), n3(:,468), t3x256(:,:,256), nhel, den(467))
    call cont_QA(nsync, wf16(:,36), wf16(:,140), A(:,257), n3(:,469), t3x256(:,:,257), nhel, den(469))
    call cont_QA(nsync, wf16(:,37), wf16(:,140), A(:,258), n3(:,470), t3x256(:,:,258), nhel, den(470))
    call cont_VV(nsync, wf16(:,94), wf16(:,141), A(:,259), n3(:,471), t3x256(:,:,259), nhel, den(472))
    call cont_VV(nsync, wf16(:,95), wf16(:,141), A(:,260), n3(:,472), t3x256(:,:,260), nhel, den(473))
    call cont_QA(nsync, wf16(:,32), wf16(:,142), A(:,261), n3(:,473), t3x256(:,:,261), nhel, den(475))
    call cont_QA(nsync, wf16(:,33), wf16(:,142), A(:,262), n3(:,474), t3x256(:,:,262), nhel, den(476))
    call cont_VV(nsync, wf16(:,96), wf16(:,141), A(:,263), n3(:,475), t3x256(:,:,263), nhel, den(477))
    call cont_VV(nsync, wf16(:,97), wf16(:,141), A(:,264), n3(:,476), t3x256(:,:,264), nhel, den(478))
    call cont_VV(nsync, wf16(:,120), wf16(:,143), A(:,265), n3(:,477), t3x256(:,:,265), nhel, den(479))
    call cont_VV(nsync, wf16(:,121), wf16(:,143), A(:,266), n3(:,478), t3x256(:,:,266), nhel, den(480))
    call cont_QA(nsync, wf16(:,82), wf16(:,144), A(:,267), n3(:,479), t3x256(:,:,267), nhel, den(482))
    call cont_QA(nsync, wf16(:,83), wf16(:,144), A(:,268), n3(:,480), t3x256(:,:,268), nhel, den(483))
    call cont_VV(nsync, wf16(:,122), wf16(:,143), A(:,269), n3(:,481), t3x256(:,:,269), nhel, den(484))
    call cont_VV(nsync, wf16(:,123), wf16(:,143), A(:,270), n3(:,482), t3x256(:,:,270), nhel, den(485))
    call cont_QA(nsync, wf16(:,78), wf16(:,145), A(:,271), n3(:,483), t3x256(:,:,271), nhel, den(487))
    call cont_QA(nsync, wf16(:,79), wf16(:,145), A(:,272), n3(:,484), t3x256(:,:,272), nhel, den(488))
    call cont_VV(nsync, wf16(:,120), wf16(:,146), A(:,273), n3(:,485), t3x256(:,:,273), nhel, den(489))
    call cont_VV(nsync, wf16(:,121), wf16(:,146), A(:,274), n3(:,486), t3x256(:,:,274), nhel, den(490))
    call cont_QA(nsync, wf16(:,82), wf16(:,147), A(:,275), n3(:,487), t3x256(:,:,275), nhel, den(492))
    call cont_QA(nsync, wf16(:,83), wf16(:,147), A(:,276), n3(:,488), t3x256(:,:,276), nhel, den(493))
    call cont_VV(nsync, wf16(:,122), wf16(:,146), A(:,277), n3(:,489), t3x256(:,:,277), nhel, den(494))
    call cont_VV(nsync, wf16(:,123), wf16(:,146), A(:,278), n3(:,490), t3x256(:,:,278), nhel, den(495))
    call cont_QA(nsync, wf16(:,78), wf16(:,148), A(:,279), n3(:,491), t3x256(:,:,279), nhel, den(497))
    call cont_QA(nsync, wf16(:,79), wf16(:,148), A(:,280), n3(:,492), t3x256(:,:,280), nhel, den(498))
    call cont_QA(nsync, wf16(:,31), wf16(:,149), A(:,281), n3(:,493), t3x256(:,:,281), nhel, den(502))
    call cont_QA(nsync, wf16(:,31), wf16(:,150), A(:,282), n3(:,494), t3x256(:,:,282), nhel, den(505))
    call cont_QA(nsync, wf8(:,9), wf32(:,33), A(:,283), n3(:,495), t3x256(:,:,283), nhel, den(507))
    call cont_QA(nsync, wf8(:,9), wf32(:,34), A(:,284), n3(:,496), t3x256(:,:,284), nhel, den(509))
    call cont_QA(nsync, wf16(:,35), wf16(:,151), A(:,285), n3(:,497), t3x256(:,:,285), nhel, den(510))
    call cont_QA(nsync, wf16(:,35), wf16(:,152), A(:,286), n3(:,498), t3x256(:,:,286), nhel, den(511))
    call cont_QA(nsync, wf4(:,6), wf64(:,9), A(:,287), n3(:,499), t3x256(:,:,287), nhel, den(513))
    call cont_QA(nsync, wf4(:,6), wf64(:,10), A(:,288), n3(:,500), t3x256(:,:,288), nhel, den(515))
    call cont_QA(nsync, wf16(:,14), wf16(:,153), A(:,289), n3(:,501), t3x256(:,:,289), nhel, den(517))
    call cont_QA(nsync, wf16(:,14), wf16(:,154), A(:,290), n3(:,502), t3x256(:,:,290), nhel, den(519))
    call cont_QA(nsync, wf16(:,15), wf16(:,153), A(:,291), n3(:,503), t3x256(:,:,291), nhel, den(520))
    call cont_QA(nsync, wf16(:,15), wf16(:,154), A(:,292), n3(:,504), t3x256(:,:,292), nhel, den(521))
    call cont_QA(nsync, wf16(:,29), wf16(:,155), A(:,293), n3(:,505), t3x256(:,:,293), nhel, den(523))
    call cont_QA(nsync, wf16(:,29), wf16(:,156), A(:,294), n3(:,506), t3x256(:,:,294), nhel, den(525))
    call cont_QA(nsync, wf16(:,46), wf16(:,149), A(:,295), n3(:,507), t3x256(:,:,295), nhel, den(526))
    call cont_QA(nsync, wf16(:,46), wf16(:,150), A(:,296), n3(:,508), t3x256(:,:,296), nhel, den(527))
    call cont_QA(nsync, wf16(:,31), wf16(:,157), A(:,297), n3(:,509), t3x256(:,:,297), nhel, den(530))
    call cont_QA(nsync, wf16(:,31), wf16(:,158), A(:,298), n3(:,510), t3x256(:,:,298), nhel, den(532))
    call cont_QA(nsync, wf8(:,9), wf32(:,35), A(:,299), n3(:,511), t3x256(:,:,299), nhel, den(534))
    call cont_QA(nsync, wf8(:,9), wf32(:,36), A(:,300), n3(:,512), t3x256(:,:,300), nhel, den(536))
    call cont_QA(nsync, wf16(:,35), wf16(:,159), A(:,301), n3(:,513), t3x256(:,:,301), nhel, den(537))
    call cont_QA(nsync, wf16(:,35), wf16(:,160), A(:,302), n3(:,514), t3x256(:,:,302), nhel, den(538))
    call cont_QA(nsync, wf4(:,6), wf64(:,11), A(:,303), n3(:,515), t3x256(:,:,303), nhel, den(540))
    call cont_QA(nsync, wf4(:,6), wf64(:,12), A(:,304), n3(:,516), t3x256(:,:,304), nhel, den(542))
    call cont_QA(nsync, wf16(:,14), wf16(:,161), A(:,305), n3(:,517), t3x256(:,:,305), nhel, den(544))
    call cont_QA(nsync, wf16(:,14), wf16(:,162), A(:,306), n3(:,518), t3x256(:,:,306), nhel, den(546))
    call cont_QA(nsync, wf16(:,15), wf16(:,161), A(:,307), n3(:,519), t3x256(:,:,307), nhel, den(547))
    call cont_QA(nsync, wf16(:,15), wf16(:,162), A(:,308), n3(:,520), t3x256(:,:,308), nhel, den(548))
    call cont_QA(nsync, wf16(:,29), wf16(:,163), A(:,309), n3(:,521), t3x256(:,:,309), nhel, den(550))
    call cont_QA(nsync, wf16(:,29), wf16(:,164), A(:,310), n3(:,522), t3x256(:,:,310), nhel, den(552))
    call cont_QA(nsync, wf16(:,46), wf16(:,157), A(:,311), n3(:,523), t3x256(:,:,311), nhel, den(553))
    call cont_QA(nsync, wf16(:,46), wf16(:,158), A(:,312), n3(:,524), t3x256(:,:,312), nhel, den(554))
    call cont_QA(nsync, wf16(:,77), wf16(:,165), A(:,313), n3(:,525), t3x256(:,:,313), nhel, den(555))
    call cont_QA(nsync, wf16(:,77), wf16(:,166), A(:,314), n3(:,526), t3x256(:,:,314), nhel, den(556))
    call cont_QA(nsync, wf8(:,30), wf32(:,37), A(:,315), n3(:,527), t3x256(:,:,315), nhel, den(558))
    call cont_QA(nsync, wf8(:,30), wf32(:,38), A(:,316), n3(:,528), t3x256(:,:,316), nhel, den(560))
    call cont_QA(nsync, wf16(:,81), wf16(:,167), A(:,317), n3(:,529), t3x256(:,:,317), nhel, den(561))
    call cont_QA(nsync, wf16(:,81), wf16(:,168), A(:,318), n3(:,530), t3x256(:,:,318), nhel, den(562))
    call cont_QA(nsync, wf4(:,17), wf64(:,13), A(:,319), n3(:,531), t3x256(:,:,319), nhel, den(564))
    call cont_QA(nsync, wf4(:,17), wf64(:,14), A(:,320), n3(:,532), t3x256(:,:,320), nhel, den(566))
    call cont_QA(nsync, wf16(:,61), wf16(:,169), A(:,321), n3(:,533), t3x256(:,:,321), nhel, den(568))
    call cont_QA(nsync, wf16(:,61), wf16(:,170), A(:,322), n3(:,534), t3x256(:,:,322), nhel, den(570))
    call cont_QA(nsync, wf16(:,60), wf16(:,169), A(:,323), n3(:,535), t3x256(:,:,323), nhel, den(571))
    call cont_QA(nsync, wf16(:,60), wf16(:,170), A(:,324), n3(:,536), t3x256(:,:,324), nhel, den(572))
    call cont_QA(nsync, wf16(:,92), wf16(:,165), A(:,325), n3(:,537), t3x256(:,:,325), nhel, den(573))
    call cont_QA(nsync, wf16(:,92), wf16(:,166), A(:,326), n3(:,538), t3x256(:,:,326), nhel, den(574))
    call cont_QA(nsync, wf16(:,75), wf16(:,171), A(:,327), n3(:,539), t3x256(:,:,327), nhel, den(576))
    call cont_QA(nsync, wf16(:,75), wf16(:,172), A(:,328), n3(:,540), t3x256(:,:,328), nhel, den(578))
    call cont_QA(nsync, wf16(:,77), wf16(:,173), A(:,329), n3(:,541), t3x256(:,:,329), nhel, den(579))
    call cont_QA(nsync, wf16(:,77), wf16(:,174), A(:,330), n3(:,542), t3x256(:,:,330), nhel, den(580))
    call cont_QA(nsync, wf8(:,30), wf32(:,39), A(:,331), n3(:,543), t3x256(:,:,331), nhel, den(582))
    call cont_QA(nsync, wf8(:,30), wf32(:,40), A(:,332), n3(:,544), t3x256(:,:,332), nhel, den(584))
    call cont_QA(nsync, wf16(:,81), wf16(:,175), A(:,333), n3(:,545), t3x256(:,:,333), nhel, den(585))
    call cont_QA(nsync, wf16(:,81), wf16(:,176), A(:,334), n3(:,546), t3x256(:,:,334), nhel, den(586))
    call cont_QA(nsync, wf4(:,17), wf64(:,15), A(:,335), n3(:,547), t3x256(:,:,335), nhel, den(588))
    call cont_QA(nsync, wf4(:,17), wf64(:,16), A(:,336), n3(:,548), t3x256(:,:,336), nhel, den(590))
    call cont_QA(nsync, wf16(:,61), wf16(:,177), A(:,337), n3(:,549), t3x256(:,:,337), nhel, den(592))
    call cont_QA(nsync, wf16(:,61), wf16(:,178), A(:,338), n3(:,550), t3x256(:,:,338), nhel, den(594))
    call cont_QA(nsync, wf16(:,60), wf16(:,177), A(:,339), n3(:,551), t3x256(:,:,339), nhel, den(595))
    call cont_QA(nsync, wf16(:,60), wf16(:,178), A(:,340), n3(:,552), t3x256(:,:,340), nhel, den(596))
    call cont_QA(nsync, wf16(:,92), wf16(:,173), A(:,341), n3(:,553), t3x256(:,:,341), nhel, den(597))
    call cont_QA(nsync, wf16(:,92), wf16(:,174), A(:,342), n3(:,554), t3x256(:,:,342), nhel, den(598))
    call cont_QA(nsync, wf16(:,75), wf16(:,179), A(:,343), n3(:,555), t3x256(:,:,343), nhel, den(600))
    call cont_QA(nsync, wf16(:,75), wf16(:,180), A(:,344), n3(:,556), t3x256(:,:,344), nhel, den(602))
    call cont_QA(nsync, wf16(:,115), wf16(:,153), A(:,345), n3(:,557), t3x256(:,:,345), nhel, den(603))
    call cont_QA(nsync, wf16(:,115), wf16(:,154), A(:,346), n3(:,558), t3x256(:,:,346), nhel, den(604))
    call cont_QA(nsync, wf16(:,139), wf16(:,149), A(:,347), n3(:,559), t3x256(:,:,347), nhel, den(605))
    call cont_QA(nsync, wf16(:,139), wf16(:,150), A(:,348), n3(:,560), t3x256(:,:,348), nhel, den(606))
    call cont_QA(nsync, wf16(:,117), wf16(:,153), A(:,349), n3(:,561), t3x256(:,:,349), nhel, den(607))
    call cont_QA(nsync, wf16(:,117), wf16(:,154), A(:,350), n3(:,562), t3x256(:,:,350), nhel, den(608))
    call cont_QA(nsync, wf16(:,142), wf16(:,149), A(:,351), n3(:,563), t3x256(:,:,351), nhel, den(609))
    call cont_QA(nsync, wf16(:,142), wf16(:,150), A(:,352), n3(:,564), t3x256(:,:,352), nhel, den(610))
    call cont_QA(nsync, wf16(:,99), wf16(:,169), A(:,353), n3(:,565), t3x256(:,:,353), nhel, den(611))
    call cont_QA(nsync, wf16(:,99), wf16(:,170), A(:,354), n3(:,566), t3x256(:,:,354), nhel, den(612))
    call cont_QA(nsync, wf16(:,145), wf16(:,165), A(:,355), n3(:,567), t3x256(:,:,355), nhel, den(613))
    call cont_QA(nsync, wf16(:,145), wf16(:,166), A(:,356), n3(:,568), t3x256(:,:,356), nhel, den(614))
    call cont_QA(nsync, wf16(:,101), wf16(:,169), A(:,357), n3(:,569), t3x256(:,:,357), nhel, den(615))
    call cont_QA(nsync, wf16(:,101), wf16(:,170), A(:,358), n3(:,570), t3x256(:,:,358), nhel, den(616))
    call cont_QA(nsync, wf16(:,148), wf16(:,165), A(:,359), n3(:,571), t3x256(:,:,359), nhel, den(617))
    call cont_QA(nsync, wf16(:,148), wf16(:,166), A(:,360), n3(:,572), t3x256(:,:,360), nhel, den(618))
    call cont_QA(nsync, wf16(:,115), wf16(:,161), A(:,361), n3(:,573), t3x256(:,:,361), nhel, den(619))
    call cont_QA(nsync, wf16(:,115), wf16(:,162), A(:,362), n3(:,574), t3x256(:,:,362), nhel, den(620))
    call cont_QA(nsync, wf16(:,139), wf16(:,157), A(:,363), n3(:,575), t3x256(:,:,363), nhel, den(621))
    call cont_QA(nsync, wf16(:,139), wf16(:,158), A(:,364), n3(:,576), t3x256(:,:,364), nhel, den(622))
    call cont_QA(nsync, wf16(:,117), wf16(:,161), A(:,365), n3(:,577), t3x256(:,:,365), nhel, den(623))
    call cont_QA(nsync, wf16(:,117), wf16(:,162), A(:,366), n3(:,578), t3x256(:,:,366), nhel, den(624))
    call cont_QA(nsync, wf16(:,142), wf16(:,157), A(:,367), n3(:,579), t3x256(:,:,367), nhel, den(625))
    call cont_QA(nsync, wf16(:,142), wf16(:,158), A(:,368), n3(:,580), t3x256(:,:,368), nhel, den(626))
    call cont_QA(nsync, wf16(:,99), wf16(:,177), A(:,369), n3(:,581), t3x256(:,:,369), nhel, den(627))
    call cont_QA(nsync, wf16(:,99), wf16(:,178), A(:,370), n3(:,582), t3x256(:,:,370), nhel, den(628))
    call cont_QA(nsync, wf16(:,145), wf16(:,173), A(:,371), n3(:,583), t3x256(:,:,371), nhel, den(629))
    call cont_QA(nsync, wf16(:,145), wf16(:,174), A(:,372), n3(:,584), t3x256(:,:,372), nhel, den(630))
    call cont_QA(nsync, wf16(:,101), wf16(:,177), A(:,373), n3(:,585), t3x256(:,:,373), nhel, den(631))
    call cont_QA(nsync, wf16(:,101), wf16(:,178), A(:,374), n3(:,586), t3x256(:,:,374), nhel, den(632))
    call cont_QA(nsync, wf16(:,148), wf16(:,173), A(:,375), n3(:,587), t3x256(:,:,375), nhel, den(633))
    call cont_QA(nsync, wf16(:,148), wf16(:,174), A(:,376), n3(:,588), t3x256(:,:,376), nhel, den(634))

end subroutine diagrams


elemental function diagmap(j, n)
  implicit none
  integer, intent(in) :: j, n
  complex(REALKIND) :: diagmap
  diagmap = A(j,n)%j
end function diagmap

function diagsum(j, pos, neg)
  implicit none
  integer, intent(in) :: j, pos(:), neg(:)
  complex(REALKIND) :: diagsum
  diagsum = sum(diagmap(j, pos)) - sum(diagmap(j, neg))
end function diagsum

subroutine colourvector(A, j, M1)
  implicit none
  type(polcont) :: A(:,:)
  integer, intent(in) :: j
  complex(REALKIND), intent(out) :: M1(4) ! M1helarray(4,256)
  integer :: empty(0)

  M1(1) = ((A(j,61)%j+A(j,63)%j+A(j,65)%j+A(j,67)%j+A(j,71)%j+A(j,73)%j+A(j,77)%j+A(j,79)%j+A(j,81)%j+A(j,83)%j+A(j,87)%j &
       +A(j,89)%j+A(j,93)%j+A(j,95)%j+A(j,97)%j+A(j,99)%j+A(j,101)%j+A(j,103)%j+A(j,105)%j+A(j,107)%j+A(j,109)%j+A(j,113)%j &
       +A(j,115)%j+A(j,117)%j+A(j,121)%j+A(j,123)%j+A(j,145)%j+A(j,151)%j+A(j,153)%j+A(j,155)%j+A(j,177)%j+A(j,183)%j+A(j,185)%j &
       +A(j,187)%j+A(j,189)%j+A(j,191)%j+A(j,193)%j+A(j,195)%j+A(j,197)%j+A(j,199)%j+A(j,209)%j+A(j,215)%j+A(j,217)%j+A(j,219)%j &
       +A(j,221)%j+A(j,223)%j+A(j,225)%j+A(j,227)%j+A(j,229)%j+A(j,231)%j+A(j,241)%j+A(j,247)%j+A(j,249)%j+A(j,251)%j+A(j,253)%j &
       +A(j,255)%j+A(j,257)%j+A(j,259)%j+A(j,261)%j+A(j,263)%j)*f(1))/6._/**/REALKIND+((A(j,62)%j+A(j,64)%j+A(j,66)%j+A(j,68)%j &
       +A(j,72)%j+A(j,74)%j+A(j,78)%j+A(j,80)%j+A(j,82)%j+A(j,84)%j+A(j,88)%j+A(j,90)%j+A(j,94)%j+A(j,96)%j+A(j,98)%j+A(j,100)%j &
       +A(j,102)%j+A(j,104)%j+A(j,106)%j+A(j,108)%j+A(j,110)%j+A(j,114)%j+A(j,116)%j+A(j,118)%j+A(j,122)%j+A(j,124)%j+A(j,146)%j &
       +A(j,152)%j+A(j,154)%j+A(j,156)%j+A(j,178)%j+A(j,184)%j+A(j,186)%j+A(j,188)%j+A(j,190)%j+A(j,192)%j+A(j,194)%j+A(j,196)%j &
       +A(j,198)%j+A(j,200)%j+A(j,210)%j+A(j,216)%j+A(j,218)%j+A(j,220)%j+A(j,222)%j+A(j,224)%j+A(j,226)%j+A(j,228)%j+A(j,230)%j &
       +A(j,232)%j+A(j,242)%j+A(j,248)%j+A(j,250)%j+A(j,252)%j+A(j,254)%j+A(j,256)%j+A(j,258)%j+A(j,260)%j+A(j,262)%j+A(j,264)%j &
       +A(j,313)%j+A(j,315)%j+A(j,317)%j+A(j,319)%j+A(j,323)%j+A(j,327)%j+A(j,329)%j+A(j,331)%j+A(j,333)%j+A(j,335)%j+A(j,339)%j &
       +A(j,343)%j+A(j,345)%j+A(j,347)%j+A(j,349)%j+A(j,351)%j+A(j,361)%j+A(j,363)%j+A(j,365)%j+A(j,367)%j)*f(2))/6._/**/REALKIND &
       +((A(j,314)%j+A(j,316)%j+A(j,318)%j+A(j,320)%j+A(j,324)%j+A(j,328)%j+A(j,330)%j+A(j,332)%j+A(j,334)%j+A(j,336)%j+A(j,340)%j &
       +A(j,344)%j+A(j,346)%j+A(j,348)%j+A(j,350)%j+A(j,352)%j+A(j,362)%j+A(j,364)%j+A(j,366)%j+A(j,368)%j)*f(3))/6._/**/REALKIND
  M1(2) = ((-A(j,7)%j-A(j,11)%j-A(j,13)%j-A(j,17)%j-A(j,19)%j-A(j,23)%j-A(j,33)%j-A(j,35)%j-A(j,37)%j-A(j,39)%j-A(j,49)%j &
       -A(j,51)%j-A(j,61)%j-A(j,63)%j-A(j,67)%j-A(j,83)%j-A(j,87)%j-A(j,89)%j-A(j,101)%j-A(j,103)%j-A(j,105)%j-A(j,107)%j &
       -A(j,115)%j-A(j,117)%j-A(j,121)%j-A(j,123)%j-A(j,125)%j-A(j,127)%j-A(j,129)%j-A(j,131)%j-A(j,141)%j-A(j,157)%j-A(j,159)%j &
       -A(j,161)%j-A(j,163)%j-A(j,173)%j-A(j,177)%j-A(j,183)%j-A(j,193)%j-A(j,195)%j-A(j,197)%j-A(j,199)%j-A(j,205)%j-A(j,211)%j &
       -A(j,215)%j-A(j,225)%j-A(j,227)%j-A(j,229)%j-A(j,231)%j-A(j,233)%j-A(j,235)%j-A(j,247)%j-A(j,257)%j-A(j,259)%j-A(j,261)%j &
       -A(j,263)%j-A(j,265)%j-A(j,267)%j-A(j,269)%j-A(j,271)%j)*f(1))/2._/**/REALKIND+((-A(j,8)%j-A(j,12)%j-A(j,14)%j-A(j,18)%j &
       -A(j,20)%j-A(j,24)%j-A(j,34)%j-A(j,36)%j-A(j,38)%j-A(j,40)%j-A(j,50)%j-A(j,52)%j-A(j,62)%j-A(j,64)%j-A(j,68)%j-A(j,84)%j &
       -A(j,88)%j-A(j,90)%j-A(j,102)%j-A(j,104)%j-A(j,106)%j-A(j,108)%j-A(j,116)%j-A(j,118)%j-A(j,122)%j-A(j,124)%j-A(j,126)%j &
       -A(j,128)%j-A(j,130)%j-A(j,132)%j-A(j,142)%j-A(j,158)%j-A(j,160)%j-A(j,162)%j-A(j,164)%j-A(j,174)%j-A(j,178)%j-A(j,184)%j &
       -A(j,194)%j-A(j,196)%j-A(j,198)%j-A(j,200)%j-A(j,206)%j-A(j,212)%j-A(j,216)%j-A(j,226)%j-A(j,228)%j-A(j,230)%j-A(j,232)%j &
       -A(j,234)%j-A(j,236)%j-A(j,248)%j-A(j,258)%j-A(j,260)%j-A(j,262)%j-A(j,264)%j-A(j,266)%j-A(j,268)%j-A(j,270)%j-A(j,272)%j &
       -A(j,281)%j-A(j,283)%j-A(j,289)%j-A(j,297)%j-A(j,299)%j-A(j,305)%j-A(j,317)%j-A(j,319)%j-A(j,327)%j-A(j,333)%j-A(j,335)%j &
       -A(j,343)%j-A(j,349)%j-A(j,351)%j-A(j,353)%j-A(j,355)%j-A(j,365)%j-A(j,367)%j-A(j,369)%j-A(j,371)%j)*f(2))/2._/**/REALKIND &
       +((-A(j,282)%j-A(j,284)%j-A(j,290)%j-A(j,298)%j-A(j,300)%j-A(j,306)%j-A(j,318)%j-A(j,320)%j-A(j,328)%j-A(j,334)%j &
       -A(j,336)%j-A(j,344)%j-A(j,350)%j-A(j,352)%j-A(j,354)%j-A(j,356)%j-A(j,366)%j-A(j,368)%j-A(j,370)%j &
       -A(j,372)%j)*f(3))/2._/**/REALKIND+(CI*(-A(j,5)%j-A(j,15)%j-A(j,21)%j-A(j,31)%j-A(j,53)%j-A(j,59)%j+A(j,69)%j+A(j,75)%j &
       +A(j,85)%j+A(j,91)%j+A(j,111)%j+A(j,119)%j-A(j,143)%j-A(j,149)%j-A(j,175)%j-A(j,181)%j+A(j,207)%j+A(j,213)%j+A(j,239)%j &
       +A(j,245)%j)*f(4))/2._/**/REALKIND+(CI*(-A(j,6)%j-A(j,16)%j-A(j,22)%j-A(j,32)%j-A(j,54)%j-A(j,60)%j+A(j,70)%j+A(j,76)%j &
       +A(j,86)%j+A(j,92)%j+A(j,112)%j+A(j,120)%j-A(j,144)%j-A(j,150)%j-A(j,176)%j-A(j,182)%j+A(j,208)%j+A(j,214)%j+A(j,240)%j &
       +A(j,246)%j-A(j,291)%j-A(j,295)%j-A(j,307)%j-A(j,311)%j+A(j,321)%j+A(j,325)%j+A(j,337)%j+A(j,341)%j)*f(5))/2._/**/REALKIND &
       +(CI*(-A(j,292)%j-A(j,296)%j-A(j,308)%j-A(j,312)%j+A(j,322)%j+A(j,326)%j+A(j,338)%j+A(j,342)%j)*f(6))/2._/**/REALKIND
  M1(3) = ((-A(j,1)%j-A(j,3)%j-A(j,9)%j-A(j,25)%j-A(j,27)%j-A(j,29)%j-A(j,41)%j-A(j,43)%j-A(j,45)%j-A(j,47)%j-A(j,55)%j-A(j,57)%j &
       -A(j,65)%j-A(j,71)%j-A(j,73)%j-A(j,77)%j-A(j,79)%j-A(j,81)%j-A(j,93)%j-A(j,95)%j-A(j,97)%j-A(j,99)%j-A(j,109)%j-A(j,113)%j &
       -A(j,133)%j-A(j,135)%j-A(j,137)%j-A(j,139)%j-A(j,145)%j-A(j,147)%j-A(j,151)%j-A(j,153)%j-A(j,155)%j-A(j,165)%j-A(j,167)%j &
       -A(j,169)%j-A(j,171)%j-A(j,179)%j-A(j,185)%j-A(j,187)%j-A(j,189)%j-A(j,191)%j-A(j,201)%j-A(j,203)%j-A(j,209)%j-A(j,217)%j &
       -A(j,219)%j-A(j,221)%j-A(j,223)%j-A(j,237)%j-A(j,241)%j-A(j,243)%j-A(j,249)%j-A(j,251)%j-A(j,253)%j-A(j,255)%j-A(j,273)%j &
       -A(j,275)%j-A(j,277)%j-A(j,279)%j)*f(1))/2._/**/REALKIND+((-A(j,2)%j-A(j,4)%j-A(j,10)%j-A(j,26)%j-A(j,28)%j-A(j,30)%j &
       -A(j,42)%j-A(j,44)%j-A(j,46)%j-A(j,48)%j-A(j,56)%j-A(j,58)%j-A(j,66)%j-A(j,72)%j-A(j,74)%j-A(j,78)%j-A(j,80)%j-A(j,82)%j &
       -A(j,94)%j-A(j,96)%j-A(j,98)%j-A(j,100)%j-A(j,110)%j-A(j,114)%j-A(j,134)%j-A(j,136)%j-A(j,138)%j-A(j,140)%j-A(j,146)%j &
       -A(j,148)%j-A(j,152)%j-A(j,154)%j-A(j,156)%j-A(j,166)%j-A(j,168)%j-A(j,170)%j-A(j,172)%j-A(j,180)%j-A(j,186)%j-A(j,188)%j &
       -A(j,190)%j-A(j,192)%j-A(j,202)%j-A(j,204)%j-A(j,210)%j-A(j,218)%j-A(j,220)%j-A(j,222)%j-A(j,224)%j-A(j,238)%j-A(j,242)%j &
       -A(j,244)%j-A(j,250)%j-A(j,252)%j-A(j,254)%j-A(j,256)%j-A(j,274)%j-A(j,276)%j-A(j,278)%j-A(j,280)%j-A(j,285)%j-A(j,287)%j &
       -A(j,293)%j-A(j,301)%j-A(j,303)%j-A(j,309)%j-A(j,313)%j-A(j,315)%j-A(j,323)%j-A(j,329)%j-A(j,331)%j-A(j,339)%j-A(j,345)%j &
       -A(j,347)%j-A(j,357)%j-A(j,359)%j-A(j,361)%j-A(j,363)%j-A(j,373)%j-A(j,375)%j)*f(2))/2._/**/REALKIND+((-A(j,286)%j &
       -A(j,288)%j-A(j,294)%j-A(j,302)%j-A(j,304)%j-A(j,310)%j-A(j,314)%j-A(j,316)%j-A(j,324)%j-A(j,330)%j-A(j,332)%j-A(j,340)%j &
       -A(j,346)%j-A(j,348)%j-A(j,358)%j-A(j,360)%j-A(j,362)%j-A(j,364)%j-A(j,374)%j-A(j,376)%j)*f(3))/2._/**/REALKIND &
       +(CI*(A(j,5)%j+A(j,15)%j+A(j,21)%j+A(j,31)%j+A(j,53)%j+A(j,59)%j-A(j,69)%j-A(j,75)%j-A(j,85)%j-A(j,91)%j-A(j,111)%j &
       -A(j,119)%j+A(j,143)%j+A(j,149)%j+A(j,175)%j+A(j,181)%j-A(j,207)%j-A(j,213)%j-A(j,239)%j-A(j,245)%j)*f(4))/2._/**/REALKIND &
       +(CI*(A(j,6)%j+A(j,16)%j+A(j,22)%j+A(j,32)%j+A(j,54)%j+A(j,60)%j-A(j,70)%j-A(j,76)%j-A(j,86)%j-A(j,92)%j-A(j,112)%j &
       -A(j,120)%j+A(j,144)%j+A(j,150)%j+A(j,176)%j+A(j,182)%j-A(j,208)%j-A(j,214)%j-A(j,240)%j-A(j,246)%j+A(j,291)%j+A(j,295)%j &
       +A(j,307)%j+A(j,311)%j-A(j,321)%j-A(j,325)%j-A(j,337)%j-A(j,341)%j)*f(5))/2._/**/REALKIND+(CI*(A(j,292)%j+A(j,296)%j &
       +A(j,308)%j+A(j,312)%j-A(j,322)%j-A(j,326)%j-A(j,338)%j-A(j,342)%j)*f(6))/2._/**/REALKIND
  M1(4) = ((A(j,1)%j+A(j,3)%j+A(j,7)%j+A(j,9)%j+A(j,11)%j+A(j,13)%j+A(j,17)%j+A(j,19)%j+A(j,23)%j+A(j,25)%j+A(j,27)%j+A(j,29)%j &
       +A(j,33)%j+A(j,35)%j+A(j,37)%j+A(j,39)%j+A(j,41)%j+A(j,43)%j+A(j,45)%j+A(j,47)%j+A(j,49)%j+A(j,51)%j+A(j,55)%j+A(j,57)%j &
       +A(j,125)%j+A(j,127)%j+A(j,129)%j+A(j,131)%j+A(j,133)%j+A(j,135)%j+A(j,137)%j+A(j,139)%j+A(j,141)%j+A(j,147)%j+A(j,157)%j &
       +A(j,159)%j+A(j,161)%j+A(j,163)%j+A(j,165)%j+A(j,167)%j+A(j,169)%j+A(j,171)%j+A(j,173)%j+A(j,179)%j+A(j,201)%j+A(j,203)%j &
       +A(j,205)%j+A(j,211)%j+A(j,233)%j+A(j,235)%j+A(j,237)%j+A(j,243)%j+A(j,265)%j+A(j,267)%j+A(j,269)%j+A(j,271)%j+A(j,273)%j &
       +A(j,275)%j+A(j,277)%j+A(j,279)%j)*f(1))/6._/**/REALKIND+((A(j,2)%j+A(j,4)%j+A(j,8)%j+A(j,10)%j+A(j,12)%j+A(j,14)%j &
       +A(j,18)%j+A(j,20)%j+A(j,24)%j+A(j,26)%j+A(j,28)%j+A(j,30)%j+A(j,34)%j+A(j,36)%j+A(j,38)%j+A(j,40)%j+A(j,42)%j+A(j,44)%j &
       +A(j,46)%j+A(j,48)%j+A(j,50)%j+A(j,52)%j+A(j,56)%j+A(j,58)%j+A(j,126)%j+A(j,128)%j+A(j,130)%j+A(j,132)%j+A(j,134)%j &
       +A(j,136)%j+A(j,138)%j+A(j,140)%j+A(j,142)%j+A(j,148)%j+A(j,158)%j+A(j,160)%j+A(j,162)%j+A(j,164)%j+A(j,166)%j+A(j,168)%j &
       +A(j,170)%j+A(j,172)%j+A(j,174)%j+A(j,180)%j+A(j,202)%j+A(j,204)%j+A(j,206)%j+A(j,212)%j+A(j,234)%j+A(j,236)%j+A(j,238)%j &
       +A(j,244)%j+A(j,266)%j+A(j,268)%j+A(j,270)%j+A(j,272)%j+A(j,274)%j+A(j,276)%j+A(j,278)%j+A(j,280)%j+A(j,281)%j+A(j,283)%j &
       +A(j,285)%j+A(j,287)%j+A(j,289)%j+A(j,293)%j+A(j,297)%j+A(j,299)%j+A(j,301)%j+A(j,303)%j+A(j,305)%j+A(j,309)%j+A(j,353)%j &
       +A(j,355)%j+A(j,357)%j+A(j,359)%j+A(j,369)%j+A(j,371)%j+A(j,373)%j+A(j,375)%j)*f(2))/6._/**/REALKIND+((A(j,282)%j &
       +A(j,284)%j+A(j,286)%j+A(j,288)%j+A(j,290)%j+A(j,294)%j+A(j,298)%j+A(j,300)%j+A(j,302)%j+A(j,304)%j+A(j,306)%j+A(j,310)%j &
       +A(j,354)%j+A(j,356)%j+A(j,358)%j+A(j,360)%j+A(j,370)%j+A(j,372)%j+A(j,374)%j+A(j,376)%j)*f(3))/6._/**/REALKIND

end subroutine colourvector


! **********************************************************************
subroutine colint(M, M2colint, extcombs)
! M(i)   = <M|Ci> colour component of matrix element
! COLINT = <M|M>
!        = Sum_{i,j} <M|Ci> * <Ci|Cj> * <Cj|M>
!        = colour-summed squared matrix element
! K1(i,j) = <Ci|Cj>
! M2colint is an array which contains the colour interference for each colour matrix
! The elements of the array extcombs specifies for which external particle
! combinations the colour correlations will be calculated. For particles i,j: i*(i-1)/2+j
! i=j=0 -> 0 means no colour insertion.
! **********************************************************************
  use ol_colourmatrix_ppllajj_eexddxbbxag_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(4)
  real(REALKIND),    intent(out) :: M2colint(0:38-1)
  integer,           intent(in)  :: extcombs(:)
  integer :: extcomb, colmatpos, i, j, eco

  M2colint = 0

  do eco = 1, size(extcombs)
    extcomb = extcombs(eco)
    colmatpos = 4*extcomb
    do i = 1, 4
      do j = 1, 4
        M2colint(extcomb) = M2colint(extcomb) + real(conjg(M(i))*K1(i+colmatpos,j)*M(j))
      end do
    end do
  end do

end subroutine colint


! **********************************************************************
subroutine colintmunu(M1, M2, M2colint)
! M(i)   = <M|Ci> colour component of matrix element
! COLINT = <M|M>
!        = Sum_{i,j} <M|Ci> * <Ci|Cj> * <Cj|M>
!        = colour-summed squared matrix element
! K1(i,j) = <Ci|Cj>
! M2colint is an array which contains the colour interference for each colour matrix
! **********************************************************************
  use ol_colourmatrix_ppllajj_eexddxbbxag_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(4)
  complex(REALKIND), intent(in)  :: M2(4)
  real(REALKIND),    intent(out) :: M2colint
  integer :: i, j

  M2colint = 0

  do i = 1, 4
    do j = 1, 4
      M2colint = M2colint + real(conjg(M1(i))*K1(i,j)*M2(j))
    end do
  end do

end subroutine colintmunu

end subroutine amp2


#ifdef PRECISION_dp
subroutine colourvector(M1out, nhelout) &
  & bind(c,name="ol_tree_colvect_ppllajj_eexddxbbxag_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(4,256)
  integer, intent(out) :: nhelout
  M1out = M1helarr
  nhelout = nhel
end subroutine colourvector
#endif


! =================================================== !
! Only interfaces for easier usage of AMP2_<procname> !
! =================================================== !

#ifdef PRECISION_dp
subroutine amp2tree(P, M2) &
    & bind(c,name="ol_f_amp2tree_ppllajj_eexddxbbxag_1")
#else
subroutine amp2tree(P, M2)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix element without fuss.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,8)
  real(REALKIND),  intent(out) :: M2
  real(REALKIND) :: M2tmp(0:38-1)
  real(REALKIND) :: M2munu(4,4)
  call amp2(P, M2tmp, 0, [ 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND ], 1, [ 0 ], M2munu)
  M2 = M2tmp(0)
end subroutine amp2tree


#ifdef PRECISION_dp
subroutine amp2ccone(P, M2, I, J) &
    & bind(c,name="ol_f_amp2ccone_ppllajj_eexddxbbxag_1")
#else
subroutine amp2ccone(P, M2, I, J)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix for the colour correlation matrix for particles I and J.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,8)
  real(REALKIND),  intent(out) :: M2
  integer,         intent(in)  :: I, J
  integer        :: extcomb
  real(REALKIND) :: M2tmp(0:38-1)
  real(REALKIND) :: M2munu(4,4)
  if (J <= I) then
    extcomb = I*(I-1)/2 + J
  else
    extcomb = J*(J-1)/2 + I
  end if
  call amp2(P, M2tmp, 0, [ 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND ], 1, [ extcomb ], M2munu)
  M2 = M2tmp(extcomb)
end subroutine amp2ccone


#ifdef PRECISION_dp
subroutine amp2ccall(P, M2) &
    & bind(c,name="ol_f_amp2ccall_ppllajj_eexddxbbxag_1")
#else
subroutine amp2ccall(P, M2)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix for all colour correlation matrices.
  ! The correlation between particles i and j is at position i*(i-1)/2+j of the array M2.
  ! M2(0) is AMP2tree
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,8)
  real(REALKIND),  intent(out) :: M2(0:38-1)
  real(REALKIND) :: M2munu(4,4)
  integer :: k
  call amp2(P, M2, 0, [ 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND ], &
    38, [ (k, k = 0, 38-1) ], M2munu)
end subroutine amp2ccall


#ifdef PRECISION_dp
subroutine amp2hcone(P, M2, I, J, MOM) &
    & bind(c,name="ol_f_amp2hcone_ppllajj_eexddxbbxag_1")
#else
subroutine amp2hcone(P, M2, I, J, MOM)
#endif
  ! This is an interface for AMP2.
  ! Calculates the helicity correlation for emitter I with momentum MOM and spectator J
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,8)
  real(REALKIND),  intent(out) :: M2
  integer,         intent(in)  :: I, J
  real(REALKIND),  intent(in)  :: MOM(0:3)
  integer        :: extcomb
  real(REALKIND) :: M2tmp(0:38-1)
  real(REALKIND) :: M2munu(4,4)
  if (J <= I) then
    extcomb = I*(I-1)/2 + J
  else
    extcomb = J*(J-1)/2 + I
  end if
  call amp2(P, M2tmp, I, MOM, 1, [ extcomb ], M2munu)
  M2 = M2tmp(extcomb)
end subroutine amp2hcone


#ifdef PRECISION_dp
subroutine amp2hcall(P, M2, I, MOM) &
    & bind(c,name="ol_f_amp2hcall_ppllajj_eexddxbbxag_1")
#else
subroutine amp2hcall(P, M2, I, MOM)
#endif
  ! This is an interface for AMP2.
  ! Calculates all helicity correlations for emitter I with momentum MOM.
  ! The correlator for spectator j is at position j of the array M2.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,8)
  real(REALKIND),  intent(out) :: M2(8)
  integer,         intent(in)  :: I
  real(REALKIND),  intent(in)  :: MOM(0:3)
  real(REALKIND) :: M2tmp(0:38-1)
  real(REALKIND) :: M2munu(4,4)
  integer        :: J, extcombs(8)
  do J = 1, 8
    if (J <= I) then
      extcombs(J) = I*(I-1)/2 + J
    else
      extcombs(J) = J*(J-1)/2 + I
    end if
  end do
  call amp2(P, M2tmp, I, MOM, 8,extcombs, M2munu)
  do J = 1, 8
    M2(J) = M2tmp(extcombs(J))
  end do
end subroutine amp2hcall


#ifdef PRECISION_dp

subroutine amp2tree_c(p, m2) &
    & bind(c,name="ol_amp2tree_ppllajj_eexddxbbxag_1")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,8)
  real(c_double), intent(out) :: m2
  real(DREALKIND) :: f_p(0:3,8)
  real(DREALKIND) :: f_m2
  f_p = p
  call amp2tree(f_p, f_m2)
  m2 = f_m2
end subroutine amp2tree_c

subroutine amp2ccone_c(p, m2, i, j) &
    & bind(c,name="ol_amp2ccone_ppllajj_eexddxbbxag_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,8)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(DREALKIND) :: f_p(0:3,8)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  f_p = p
  f_i = i
  f_j = j
  call amp2ccone(f_p, f_m2, f_i, f_j)
  m2 = f_m2
end subroutine amp2ccone_c

subroutine amp2ccall_c(p, m2) &
    & bind(c,name="ol_amp2ccall_ppllajj_eexddxbbxag_1")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,8)
  real(c_double), intent(out) :: m2(0:38-1)
  real(DREALKIND) :: f_p(0:3,8)
  real(DREALKIND) :: f_m2(0:38-1)
  f_p = p
  call amp2ccall(f_p, f_m2)
  m2 = f_m2
end subroutine amp2ccall_c

subroutine amp2hcone_c(p, m2, i, j, mom) &
    & bind(c,name="ol_amp2hcone_ppllajj_eexddxbbxag_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,8)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,8)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  real(DREALKIND) :: f_mom(0:3)
  f_p = p
  f_i = i
  f_j = j
  f_mom = mom
  call amp2hcone(f_p, f_m2, f_i, f_j, f_mom)
  m2 = f_m2
end subroutine amp2hcone_c

subroutine amp2hcall_c(p, m2, i, mom) &
    & bind(c,name="ol_amp2hcall_ppllajj_eexddxbbxag_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,8)
  real(c_double), intent(out) :: m2(8)
  integer(c_int), intent(in) :: i
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,8)
  real(DREALKIND) :: f_m2(8)
  integer :: f_i
  real(DREALKIND) :: f_mom(0:3)
  f_p = p
  f_i = i
  f_mom = mom
  call amp2hcall(f_p, f_m2, f_i, f_mom)
  m2 = f_m2
end subroutine amp2hcall_c



! Only for compatibility with the old interface
subroutine amp2tree_legacy(p, m2) &
    & bind(c,name="amp2tree_ppllajj_eexddxbbxag_1_")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,8)
  real(c_double), intent(out) :: m2
  real(DREALKIND) :: f_p(0:3,8)
  real(DREALKIND) :: f_m2
  f_p = p
  call amp2tree(f_p, f_m2)
  m2 = f_m2
end subroutine amp2tree_legacy

subroutine amp2ccone_legacy(p, m2, i, j) &
    & bind(c,name="amp2ccone_ppllajj_eexddxbbxag_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,8)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(DREALKIND) :: f_p(0:3,8)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  f_p = p
  f_i = i
  f_j = j
  call amp2ccone(f_p, f_m2, f_i, f_j)
  m2 = f_m2
end subroutine amp2ccone_legacy

subroutine amp2ccall_legacy(p, m2) &
    & bind(c,name="amp2ccall_ppllajj_eexddxbbxag_1_")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,8)
  real(c_double), intent(out) :: m2(0:38-1)
  real(DREALKIND) :: f_p(0:3,8)
  real(DREALKIND) :: f_m2(0:38-1)
  f_p = p
  call amp2ccall(f_p, f_m2)
  m2 = f_m2
end subroutine amp2ccall_legacy

subroutine amp2hcone_legacy(p, m2, i, j, mom) &
    & bind(c,name="amp2hcone_ppllajj_eexddxbbxag_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,8)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,8)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  real(DREALKIND) :: f_mom(0:3)
  f_p = p
  f_i = i
  f_j = j
  f_mom = mom
  call amp2hcone(f_p, f_m2, f_i, f_j, f_mom)
  m2 = f_m2
end subroutine amp2hcone_legacy

subroutine amp2hcall_legacy(p, m2, i, mom) &
    & bind(c,name="amp2hcall_ppllajj_eexddxbbxag_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,8)
  real(c_double), intent(out) :: m2(8)
  integer(c_int), intent(in) :: i
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,8)
  real(DREALKIND) :: f_m2(8)
  integer :: f_i
  real(DREALKIND) :: f_mom(0:3)
  f_p = p
  f_i = i
  f_mom = mom
  call amp2hcall(f_p, f_m2, f_i, f_mom)
  m2 = f_m2
end subroutine amp2hcall_legacy

#endif

end module ol_tree_ppllajj_eexddxbbxag_1_/**/REALKIND
