
module ol_colourmatrix_pphzjj_bbbxbxhzg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(120,4)
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
  K1(  5,:) = [  48,  16,  16,   0]
  K1(  6,:) = [  16,  48,   0,  16]
  K1(  7,:) = [  16,   0,  48,  16]
  K1(  8,:) = [   0,  16,  16,  48]
  K1(  9,:) = [   0,  16,  -2,   6]
  K1( 10,:) = [  16,   0,   6,  -2]
  K1( 11,:) = [  -2,   6,   0,  16]
  K1( 12,:) = [   6,  -2,  16,   0]
  K1( 13,:) = [  48,  16,  16,   0]
  K1( 14,:) = [  16,  48,   0,  16]
  K1( 15,:) = [  16,   0,  48,  16]
  K1( 16,:) = [   0,  16,  16,  48]
  K1( 17,:) = [   6,   2,   2,   0]
  K1( 18,:) = [   2,   0,  -6, -16]
  K1( 19,:) = [   2,  -6,   0, -16]
  K1( 20,:) = [   0, -16, -16, -48]
  K1( 21,:) = [   0,   2, -16,  -6]
  K1( 22,:) = [   2,   6,   0,   2]
  K1( 23,:) = [ -16,   0, -48, -16]
  K1( 24,:) = [  -6,   2, -16,   0]
  K1( 25,:) = [  48,  16,  16,   0]
  K1( 26,:) = [  16,  48,   0,  16]
  K1( 27,:) = [  16,   0,  48,  16]
  K1( 28,:) = [   0,  16,  16,  48]
  K1( 29,:) = [   0, -16,   2,  -6]
  K1( 30,:) = [ -16, -48,   0, -16]
  K1( 31,:) = [   2,   0,   6,   2]
  K1( 32,:) = [  -6, -16,   2,   0]
  K1( 33,:) = [ -48, -16, -16,   0]
  K1( 34,:) = [ -16,   0,  -6,   2]
  K1( 35,:) = [ -16,  -6,   0,   2]
  K1( 36,:) = [   0,   2,   2,   6]
  K1( 37,:) = [   0,  -2,  16,   6]
  K1( 38,:) = [  -2,   0,   6,  16]
  K1( 39,:) = [  16,   6,   0,  -2]
  K1( 40,:) = [   6,  16,  -2,   0]
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
  K1( 53,:) = [   0,   0,   0,   0]
  K1( 54,:) = [   0,   0,   0,   0]
  K1( 55,:) = [   0,   0,   0,   0]
  K1( 56,:) = [   0,   0,   0,   0]
  K1( 57,:) = [   0,   0,   0,   0]
  K1( 58,:) = [   0,   0,   0,   0]
  K1( 59,:) = [   0,   0,   0,   0]
  K1( 60,:) = [   0,   0,   0,   0]
  K1( 61,:) = [   0,   0,   0,   0]
  K1( 62,:) = [   0,   0,   0,   0]
  K1( 63,:) = [   0,   0,   0,   0]
  K1( 64,:) = [   0,   0,   0,   0]
  K1( 65,:) = [   0,   0,   0,   0]
  K1( 66,:) = [   0,   0,   0,   0]
  K1( 67,:) = [   0,   0,   0,   0]
  K1( 68,:) = [   0,   0,   0,   0]
  K1( 69,:) = [   0,   0,   0,   0]
  K1( 70,:) = [   0,   0,   0,   0]
  K1( 71,:) = [   0,   0,   0,   0]
  K1( 72,:) = [   0,   0,   0,   0]
  K1( 73,:) = [   0,   0,   0,   0]
  K1( 74,:) = [   0,   0,   0,   0]
  K1( 75,:) = [   0,   0,   0,   0]
  K1( 76,:) = [   0,   0,   0,   0]
  K1( 77,:) = [   0,   0,   0,   0]
  K1( 78,:) = [   0,   0,   0,   0]
  K1( 79,:) = [   0,   0,   0,   0]
  K1( 80,:) = [   0,   0,   0,   0]
  K1( 81,:) = [   0,   0,   0,   0]
  K1( 82,:) = [   0,   0,   0,   0]
  K1( 83,:) = [   0,   0,   0,   0]
  K1( 84,:) = [   0,   0,   0,   0]
  K1( 85,:) = [   0,   0,   0,   0]
  K1( 86,:) = [   0,   0,   0,   0]
  K1( 87,:) = [   0,   0,   0,   0]
  K1( 88,:) = [   0,   0,   0,   0]
  K1( 89,:) = [ -54, -18, -18,   0]
  K1( 90,:) = [ -18,   0,   0,  18]
  K1( 91,:) = [ -18,   0, -54, -18]
  K1( 92,:) = [   0,  18, -18,   0]
  K1( 93,:) = [   0, -18,  18,   0]
  K1( 94,:) = [ -18, -54,   0, -18]
  K1( 95,:) = [  18,   0,   0, -18]
  K1( 96,:) = [   0, -18, -18, -54]
  K1( 97,:) = [ -54, -18, -18,   0]
  K1( 98,:) = [ -18, -54,   0, -18]
  K1( 99,:) = [ -18,   0,   0,  18]
  K1(100,:) = [   0, -18,  18,   0]
  K1(101,:) = [   0,  18, -18,   0]
  K1(102,:) = [  18,   0,   0, -18]
  K1(103,:) = [ -18,   0, -54, -18]
  K1(104,:) = [   0, -18, -18, -54]
  K1(105,:) = [   0,   0,   0,   0]
  K1(106,:) = [   0,   0,   0,   0]
  K1(107,:) = [   0,   0,   0,   0]
  K1(108,:) = [   0,   0,   0,   0]
  K1(109,:) = [   0,   0,   0,   0]
  K1(110,:) = [   0,   0,   0,   0]
  K1(111,:) = [   0,   0,   0,   0]
  K1(112,:) = [   0,   0,   0,   0]
  K1(113,:) = [ 108,  36,  36,   0]
  K1(114,:) = [  36, 108,   0,  36]
  K1(115,:) = [  36,   0, 108,  36]
  K1(116,:) = [   0,  36,  36, 108]
  K1(117,:) = [   0,   0,   0,   0]
  K1(118,:) = [   0,   0,   0,   0]
  K1(119,:) = [   0,   0,   0,   0]
  K1(120,:) = [   0,   0,   0,   0]
  K1 = (1._/**/REALKIND / 3) * K1

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphzjj_bbbxbxhzg_1_/**/REALKIND



module ol_forced_parameters_pphzjj_bbbxbxhzg_1_/**/REALKIND
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
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (YU /= 0) write(*,101) 'YU = 0'
  if (YD /= 0) write(*,101) 'YD = 0'
  if (YS /= 0) write(*,101) 'YS = 0'
  if (YC /= 0) write(*,101) 'YC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'
  if (wMZ /= 0) write(*,101) 'wMZ = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphzjj_bbbxbxhzg_1_/**/REALKIND

module ol_tree_pphzjj_bbbxbxhzg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(4)
  complex(REALKIND), save :: den(489)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 96 ! number of helicity configurations
  integer(intkind2), save :: nhel = 96 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(96) ! physical helicity states
  complex(DREALKIND) :: M1helarr(4,96) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = (CI*eQED**2*gQCD**3*lambdaHZZ*MW)/(cw**2*sw)
    f(2) = (eQED**2*gQCD**3*lambdaHZZ*MW)/(cw**2*sw)
    f(3) = (CI*eQED**2*gQCD**3*YB)/(2._/**/REALKIND*MW*sw)
    f(4) = (eQED**2*gQCD**3*YB)/(MW*sw*2._/**/REALKIND)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,5))
  den(2) = 1 / (Q(5,18) - MB2)
  den(3) = 1 / (Q(5,40) - MB2)
  den(5) = 1 / (Q(5,69))
  den(9) = 1 / (Q(5,82) - MB2)
  den(13) = 1 / (Q(5,104) - MB2)
  den(16) = 1 / (Q(5,72) - MB2)
  den(18) = 1 / (Q(5,50) - MB2)
  den(23) = 1 / (Q(5,13) - MB2)
  den(28) = 1 / (Q(5,34) - MB2)
  den(29) = 1 / (Q(5,24) - MB2)
  den(33) = 1 / (Q(5,98) - MB2)
  den(37) = 1 / (Q(5,88) - MB2)
  den(40) = 1 / (Q(5,66) - MB2)
  den(44) = 1 / (Q(5,56) - MB2)
  den(47) = 1 / (Q(5,7) - MB2)
  den(66) = 1 / (Q(5,48) - MZ2)
  den(83) = 1 / (Q(5,17) - MB2)
  den(84) = 1 / (Q(5,6))
  den(86) = 1 / (Q(5,81) - MB2)
  den(90) = 1 / (Q(5,70))
  den(96) = 1 / (Q(5,49) - MB2)
  den(100) = 1 / (Q(5,14) - MB2)
  den(105) = 1 / (Q(5,33) - MB2)
  den(107) = 1 / (Q(5,97) - MB2)
  den(114) = 1 / (Q(5,65) - MB2)
  den(149) = 1 / (Q(5,9))
  den(150) = 1 / (Q(5,36) - MB2)
  den(152) = 1 / (Q(5,73))
  den(158) = 1 / (Q(5,100) - MB2)
  den(161) = 1 / (Q(5,68) - MB2)
  den(170) = 1 / (Q(5,20) - MB2)
  den(176) = 1 / (Q(5,84) - MB2)
  den(181) = 1 / (Q(5,52) - MB2)
  den(184) = 1 / (Q(5,11) - MB2)
  den(215) = 1 / (Q(5,10))
  den(219) = 1 / (Q(5,74))
  den(267) = 1 / (Q(5,25))
  den(271) = 1 / (Q(5,42))
  den(275) = 1 / (Q(5,21))
  den(279) = 1 / (Q(5,38))
  den(319) = 1 / (Q(5,41))
  den(323) = 1 / (Q(5,26))
  den(327) = 1 / (Q(5,37))
  den(331) = 1 / (Q(5,22))

  ! denominators

  den(4) = den(2)*den(3)
  den(6) = den(1)*den(5)
  den(7) = den(4)*den(6)
  den(8) = den(1)*den(3)
  den(10) = den(2)*den(9)
  den(11) = den(8)*den(10)
  den(12) = den(1)*den(2)
  den(14) = den(3)*den(13)
  den(15) = den(12)*den(14)
  den(17) = den(1)*den(16)
  den(19) = den(2)*den(18)
  den(20) = den(17)*den(19)
  den(21) = den(13)*den(16)
  den(22) = den(12)*den(21)
  den(24) = den(1)*den(23)
  den(25) = den(19)*den(24)
  den(26) = den(10)*den(24)
  den(27) = den(6)*den(19)
  den(30) = den(28)*den(29)
  den(31) = den(6)*den(30)
  den(32) = den(1)*den(29)
  den(34) = den(28)*den(33)
  den(35) = den(32)*den(34)
  den(36) = den(1)*den(28)
  den(38) = den(29)*den(37)
  den(39) = den(36)*den(38)
  den(41) = den(33)*den(40)
  den(42) = den(32)*den(41)
  den(43) = den(1)*den(40)
  den(45) = den(29)*den(44)
  den(46) = den(43)*den(45)
  den(48) = den(1)*den(47)
  den(49) = den(38)*den(48)
  den(50) = den(45)*den(48)
  den(51) = den(6)*den(45)
  den(52) = den(18)*den(28)
  den(53) = den(17)*den(52)
  den(54) = den(16)*den(37)
  den(55) = den(36)*den(54)
  den(56) = den(24)*den(52)
  den(57) = den(24)*den(34)
  den(58) = den(6)*den(52)
  den(59) = den(9)*den(40)
  den(60) = den(8)*den(59)
  den(61) = den(3)*den(44)
  den(62) = den(43)*den(61)
  den(63) = den(14)*den(48)
  den(64) = den(48)*den(61)
  den(65) = den(6)*den(61)
  den(67) = den(40)*den(66)
  den(68) = den(24)*den(67)
  den(69) = den(44)*den(66)
  den(70) = den(43)*den(69)
  den(71) = den(16)*den(66)
  den(72) = den(48)*den(71)
  den(73) = den(18)*den(66)
  den(74) = den(17)*den(73)
  den(75) = den(48)*den(69)
  den(76) = den(24)*den(73)
  den(77) = den(6)*den(73)
  den(78) = den(6)*den(69)
  den(79) = den(24)*den(59)
  den(80) = den(24)*den(41)
  den(81) = den(21)*den(48)
  den(82) = den(48)*den(54)
  den(85) = den(3)*den(84)
  den(87) = den(83)*den(86)
  den(88) = den(85)*den(87)
  den(89) = den(3)*den(83)
  den(91) = den(84)*den(90)
  den(92) = den(89)*den(91)
  den(93) = den(83)*den(84)
  den(94) = den(14)*den(93)
  den(95) = den(16)*den(84)
  den(97) = den(83)*den(96)
  den(98) = den(95)*den(97)
  den(99) = den(21)*den(93)
  den(101) = den(84)*den(100)
  den(102) = den(97)*den(101)
  den(103) = den(91)*den(97)
  den(104) = den(87)*den(101)
  den(106) = den(29)*den(84)
  den(108) = den(105)*den(107)
  den(109) = den(106)*den(108)
  den(110) = den(29)*den(105)
  den(111) = den(91)*den(110)
  den(112) = den(84)*den(105)
  den(113) = den(38)*den(112)
  den(115) = den(107)*den(114)
  den(116) = den(106)*den(115)
  den(117) = den(84)*den(114)
  den(118) = den(45)*den(117)
  den(119) = den(47)*den(84)
  den(120) = den(38)*den(119)
  den(121) = den(45)*den(119)
  den(122) = den(45)*den(91)
  den(123) = den(96)*den(105)
  den(124) = den(95)*den(123)
  den(125) = den(54)*den(112)
  den(126) = den(101)*den(123)
  den(127) = den(91)*den(123)
  den(128) = den(101)*den(108)
  den(129) = den(86)*den(114)
  den(130) = den(85)*den(129)
  den(131) = den(61)*den(117)
  den(132) = den(14)*den(119)
  den(133) = den(61)*den(119)
  den(134) = den(61)*den(91)
  den(135) = den(66)*den(114)
  den(136) = den(101)*den(135)
  den(137) = den(69)*den(117)
  den(138) = den(71)*den(119)
  den(139) = den(66)*den(96)
  den(140) = den(95)*den(139)
  den(141) = den(69)*den(119)
  den(142) = den(91)*den(139)
  den(143) = den(69)*den(91)
  den(144) = den(101)*den(139)
  den(145) = den(101)*den(129)
  den(146) = den(101)*den(115)
  den(147) = den(21)*den(119)
  den(148) = den(54)*den(119)
  den(151) = den(2)*den(150)
  den(153) = den(149)*den(152)
  den(154) = den(151)*den(153)
  den(155) = den(149)*den(150)
  den(156) = den(10)*den(155)
  den(157) = den(2)*den(149)
  den(159) = den(150)*den(158)
  den(160) = den(157)*den(159)
  den(162) = den(149)*den(161)
  den(163) = den(19)*den(162)
  den(164) = den(158)*den(161)
  den(165) = den(157)*den(164)
  den(166) = den(23)*den(149)
  den(167) = den(19)*den(166)
  den(168) = den(10)*den(166)
  den(169) = den(19)*den(153)
  den(171) = den(28)*den(170)
  den(172) = den(153)*den(171)
  den(173) = den(149)*den(170)
  den(174) = den(34)*den(173)
  den(175) = den(28)*den(149)
  den(177) = den(170)*den(176)
  den(178) = den(175)*den(177)
  den(179) = den(41)*den(173)
  den(180) = den(40)*den(149)
  den(182) = den(170)*den(181)
  den(183) = den(180)*den(182)
  den(185) = den(149)*den(184)
  den(186) = den(177)*den(185)
  den(187) = den(182)*den(185)
  den(188) = den(153)*den(182)
  den(189) = den(52)*den(162)
  den(190) = den(161)*den(176)
  den(191) = den(175)*den(190)
  den(192) = den(52)*den(166)
  den(193) = den(34)*den(166)
  den(194) = den(52)*den(153)
  den(195) = den(59)*den(155)
  den(196) = den(150)*den(181)
  den(197) = den(180)*den(196)
  den(198) = den(159)*den(185)
  den(199) = den(185)*den(196)
  den(200) = den(153)*den(196)
  den(201) = den(67)*den(166)
  den(202) = den(66)*den(181)
  den(203) = den(180)*den(202)
  den(204) = den(66)*den(161)
  den(205) = den(185)*den(204)
  den(206) = den(73)*den(162)
  den(207) = den(185)*den(202)
  den(208) = den(73)*den(166)
  den(209) = den(73)*den(153)
  den(210) = den(153)*den(202)
  den(211) = den(59)*den(166)
  den(212) = den(41)*den(166)
  den(213) = den(164)*den(185)
  den(214) = den(185)*den(190)
  den(216) = den(150)*den(215)
  den(217) = den(87)*den(216)
  den(218) = den(83)*den(150)
  den(220) = den(215)*den(219)
  den(221) = den(218)*den(220)
  den(222) = den(83)*den(215)
  den(223) = den(159)*den(222)
  den(224) = den(161)*den(215)
  den(225) = den(97)*den(224)
  den(226) = den(164)*den(222)
  den(227) = den(100)*den(215)
  den(228) = den(97)*den(227)
  den(229) = den(97)*den(220)
  den(230) = den(87)*den(227)
  den(231) = den(170)*den(215)
  den(232) = den(108)*den(231)
  den(233) = den(105)*den(170)
  den(234) = den(220)*den(233)
  den(235) = den(105)*den(215)
  den(236) = den(177)*den(235)
  den(237) = den(115)*den(231)
  den(238) = den(114)*den(215)
  den(239) = den(182)*den(238)
  den(240) = den(184)*den(215)
  den(241) = den(177)*den(240)
  den(242) = den(182)*den(240)
  den(243) = den(182)*den(220)
  den(244) = den(123)*den(224)
  den(245) = den(190)*den(235)
  den(246) = den(123)*den(227)
  den(247) = den(123)*den(220)
  den(248) = den(108)*den(227)
  den(249) = den(129)*den(216)
  den(250) = den(196)*den(238)
  den(251) = den(159)*den(240)
  den(252) = den(196)*den(240)
  den(253) = den(196)*den(220)
  den(254) = den(135)*den(227)
  den(255) = den(202)*den(238)
  den(256) = den(204)*den(240)
  den(257) = den(139)*den(224)
  den(258) = den(202)*den(240)
  den(259) = den(139)*den(220)
  den(260) = den(202)*den(220)
  den(261) = den(139)*den(227)
  den(262) = den(129)*den(227)
  den(263) = den(115)*den(227)
  den(264) = den(164)*den(240)
  den(265) = den(190)*den(240)
  den(266) = den(28)*den(161)
  den(268) = den(83)*den(267)
  den(269) = den(266)*den(268)
  den(270) = den(83)*den(161)
  den(272) = den(28)*den(271)
  den(273) = den(270)*den(272)
  den(274) = den(16)*den(28)
  den(276) = den(83)*den(275)
  den(277) = den(274)*den(276)
  den(278) = den(16)*den(83)
  den(280) = den(28)*den(279)
  den(281) = den(278)*den(280)
  den(282) = den(272)*den(276)
  den(283) = den(34)*den(276)
  den(284) = den(268)*den(280)
  den(285) = den(34)*den(268)
  den(286) = den(87)*den(280)
  den(287) = den(87)*den(272)
  den(288) = den(40)*den(150)
  den(289) = den(268)*den(288)
  den(290) = den(40)*den(219)
  den(291) = den(218)*den(290)
  den(292) = den(150)*den(279)
  den(293) = den(278)*den(292)
  den(294) = den(16)*den(219)
  den(295) = den(218)*den(294)
  den(296) = den(268)*den(292)
  den(297) = den(159)*den(268)
  den(298) = den(87)*den(292)
  den(299) = den(3)*den(40)
  den(300) = den(276)*den(299)
  den(301) = den(40)*den(90)
  den(302) = den(89)*den(301)
  den(303) = den(90)*den(161)
  den(304) = den(89)*den(303)
  den(305) = den(3)*den(271)
  den(306) = den(270)*den(305)
  den(307) = den(14)*den(276)
  den(308) = den(276)*den(305)
  den(309) = den(87)*den(305)
  den(310) = den(41)*den(276)
  den(311) = den(41)*den(268)
  den(312) = den(97)*den(301)
  den(313) = den(97)*den(290)
  den(314) = den(164)*den(268)
  den(315) = den(97)*den(303)
  den(316) = den(21)*den(276)
  den(317) = den(97)*den(294)
  den(318) = den(2)*den(161)
  den(320) = den(105)*den(319)
  den(321) = den(318)*den(320)
  den(322) = den(105)*den(161)
  den(324) = den(2)*den(323)
  den(325) = den(322)*den(324)
  den(326) = den(2)*den(16)
  den(328) = den(105)*den(327)
  den(329) = den(326)*den(328)
  den(330) = den(16)*den(105)
  den(332) = den(2)*den(331)
  den(333) = den(330)*den(332)
  den(334) = den(324)*den(328)
  den(335) = den(10)*den(328)
  den(336) = den(320)*den(332)
  den(337) = den(10)*den(320)
  den(338) = den(108)*den(332)
  den(339) = den(108)*den(324)
  den(340) = den(114)*den(152)
  den(341) = den(151)*den(340)
  den(342) = den(114)*den(150)
  den(343) = den(324)*den(342)
  den(344) = den(150)*den(327)
  den(345) = den(326)*den(344)
  den(346) = den(16)*den(152)
  den(347) = den(151)*den(346)
  den(348) = den(10)*den(344)
  den(349) = den(324)*den(344)
  den(350) = den(159)*den(324)
  den(351) = den(5)*den(114)
  den(352) = den(4)*den(351)
  den(353) = den(3)*den(114)
  den(354) = den(332)*den(353)
  den(355) = den(5)*den(161)
  den(356) = den(4)*den(355)
  den(357) = den(3)*den(319)
  den(358) = den(318)*den(357)
  den(359) = den(10)*den(357)
  den(360) = den(14)*den(332)
  den(361) = den(332)*den(357)
  den(362) = den(19)*den(351)
  den(363) = den(19)*den(340)
  den(364) = den(115)*den(332)
  den(365) = den(115)*den(324)
  den(366) = den(19)*den(355)
  den(367) = den(164)*den(324)
  den(368) = den(19)*den(346)
  den(369) = den(21)*den(332)
  den(370) = den(40)*den(170)
  den(371) = den(320)*den(370)
  den(372) = den(233)*den(290)
  den(373) = den(170)*den(331)
  den(374) = den(330)*den(373)
  den(375) = den(233)*den(294)
  den(376) = den(320)*den(373)
  den(377) = den(177)*den(320)
  den(378) = den(108)*den(373)
  den(379) = den(171)*den(340)
  den(380) = den(114)*den(170)
  den(381) = den(272)*den(380)
  den(382) = den(170)*den(275)
  den(383) = den(274)*den(382)
  den(384) = den(171)*den(346)
  den(385) = den(34)*den(382)
  den(386) = den(272)*den(382)
  den(387) = den(177)*den(272)
  den(388) = den(353)*den(373)
  den(389) = den(305)*den(380)
  den(390) = den(299)*den(382)
  den(391) = den(357)*den(370)
  den(392) = den(14)*den(382)
  den(393) = den(177)*den(357)
  den(394) = den(14)*den(373)
  den(395) = den(177)*den(305)
  den(396) = den(305)*den(382)
  den(397) = den(357)*den(373)
  den(398) = den(182)*den(340)
  den(399) = den(115)*den(373)
  den(400) = den(41)*den(382)
  den(401) = den(182)*den(290)
  den(402) = den(21)*den(382)
  den(403) = den(182)*den(346)
  den(404) = den(21)*den(373)
  den(405) = den(182)*den(294)
  den(406) = den(29)*den(40)
  den(407) = den(328)*den(406)
  den(408) = den(110)*den(301)
  den(409) = den(110)*den(303)
  den(410) = den(29)*den(323)
  den(411) = den(322)*den(410)
  den(412) = den(38)*den(328)
  den(413) = den(328)*den(410)
  den(414) = den(108)*den(410)
  den(415) = den(30)*den(351)
  den(416) = den(29)*den(114)
  den(417) = den(280)*den(416)
  den(418) = den(30)*den(355)
  den(419) = den(29)*den(267)
  den(420) = den(266)*den(419)
  den(421) = den(34)*den(419)
  den(422) = den(38)*den(280)
  den(423) = den(280)*den(419)
  den(424) = den(292)*den(416)
  den(425) = den(342)*den(410)
  den(426) = den(344)*den(406)
  den(427) = den(288)*den(419)
  den(428) = den(38)*den(344)
  den(429) = den(159)*den(419)
  den(430) = den(38)*den(292)
  den(431) = den(159)*den(410)
  den(432) = den(344)*den(410)
  den(433) = den(292)*den(419)
  den(434) = den(45)*den(351)
  den(435) = den(115)*den(410)
  den(436) = den(41)*den(419)
  den(437) = den(45)*den(301)
  den(438) = den(45)*den(355)
  den(439) = den(164)*den(419)
  den(440) = den(45)*den(303)
  den(441) = den(164)*den(410)
  den(442) = den(59)*den(328)
  den(443) = den(59)*den(320)
  den(444) = den(123)*den(301)
  den(445) = den(123)*den(290)
  den(446) = den(190)*den(320)
  den(447) = den(123)*den(303)
  den(448) = den(54)*den(328)
  den(449) = den(123)*den(294)
  den(450) = den(52)*den(351)
  den(451) = den(52)*den(340)
  den(452) = den(129)*den(280)
  den(453) = den(129)*den(272)
  den(454) = den(52)*den(355)
  den(455) = den(190)*den(272)
  den(456) = den(52)*den(346)
  den(457) = den(54)*den(280)
  den(458) = den(196)*den(340)
  den(459) = den(129)*den(292)
  den(460) = den(59)*den(344)
  den(461) = den(196)*den(290)
  den(462) = den(54)*den(344)
  den(463) = den(196)*den(346)
  den(464) = den(54)*den(292)
  den(465) = den(196)*den(294)
  den(466) = den(61)*den(351)
  den(467) = den(129)*den(305)
  den(468) = den(59)*den(357)
  den(469) = den(61)*den(301)
  den(470) = den(61)*den(355)
  den(471) = den(190)*den(357)
  den(472) = den(61)*den(303)
  den(473) = den(190)*den(305)
  den(474) = den(69)*den(351)
  den(475) = den(73)*den(351)
  den(476) = den(202)*den(340)
  den(477) = den(73)*den(340)
  den(478) = den(69)*den(301)
  den(479) = den(202)*den(290)
  den(480) = den(139)*den(290)
  den(481) = den(139)*den(301)
  den(482) = den(69)*den(355)
  den(483) = den(69)*den(303)
  den(484) = den(73)*den(355)
  den(485) = den(139)*den(303)
  den(486) = den(202)*den(294)
  den(487) = den(202)*den(346)
  den(488) = den(73)*den(346)
  den(489) = den(139)*den(294)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_pphzjj_bbbxbxhzg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_pphzjj_bbbxbxhzg_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for bottom bottom anti-bottom anti-bottom higgs Z glue -> 0
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
  use ol_external_pphzjj_bbbxbxhzg_1, only: external_perm_pphzjj_bbbxbxhzg_1, &
    & external_perm_inv_pphzjj_bbbxbxhzg_1, extcomb_perm_pphzjj_bbbxbxhzg_1, &
    & average_factor_pphzjj_bbbxbxhzg_1
  use ol_external_pphzjj_bbbxbxhzg_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_pphzjj_bbbxbxhzg_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_pphzjj_bbbxbxhzg_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_pphzjj_bbbxbxhzg_1
  use ol_kinematics_/**/REALKIND, only: LC2Std_Rep_cmplx
  implicit none

  real(DREALKIND), intent(in)  :: P_scatt(0:3,7)
  real(REALKIND),  intent(out) :: M2(0:30-1)
  real(REALKIND),  intent(out) :: M2munu(4,4)
  integer,         intent(in)  :: I
  real(REALKIND),  intent(in)  :: MOM(0:3)
  integer,         intent(in)  :: nextcombs
  integer,         intent(in)  :: extcombs(nextcombs)

  integer           :: ReplacePol, JBmunu, extcombs_permuted(nextcombs), shift, k, r, l, m, n
  real(REALKIND)    :: P(0:3,7)
  real(REALKIND)    :: extmasses2(7)
  real(REALKIND)    :: M2add(0:30-1), M2munuadd
  complex(REALKIND) :: MOM_LC(4), M1(4), M1helarray(4,96)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(1), ex6(3), ex7(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf2(2,8), wf3(3,2), wf4(4,36), wf6(6,32), wf8(8,74), wf12(12,62), wf16(16,32), wf24(24,46), wf96(96,328)

  type(polcont) :: A(96,328)
  complex(REALKIND) :: Aj(328)

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
  extmasses2 = [ rMB2, rMB2, rMB2, rMB2, rMH2, rMZ2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_pphzjj_bbbxbxhzg_1,7)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,7)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_pphzjj_bbbxbxhzg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_pphzjj_bbbxbxhzg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_pphzjj_bbbxbxhzg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_Q(P(:,1), rMB, H1, ex1)
  call wf_Q(P(:,2), rMB, H2, ex2)
  call wf_A(P(:,3), rMB, H3, ex3)
  call wf_A(P(:,4), rMB, H4, ex4)
  call wf_S(P(:,5), rMH, H5, ex5)
  call wf_V(P(:,6), rMZ, H6, ex6)
  call wf_V(P(:,7), rZERO, H7, ex7)


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

  end if

  ! internal WFs
  ! e.g. call vert_VQ_A(ntry, ex3, ex1, wf1, n1, t1) ...
  call vert_QA_V(ntry, ex1, ex3, wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_QS_A(gH,ntry, ex2, ex5, wf2(:,1), n3(:,2), t3x2(:,:,1))
  call vert_AZ_Q(gZd,ntry, ex4, ex6, wf6(:,1), n3(:,3), t3x6(:,:,1))
  call prop_Q_A(ntry, wf2(:,1), Q(:,18), MB, 1_intkind1, wf2(:,2), n2(1))
  call prop_A_Q(ntry, wf6(:,1), Q(:,40), MB, 1_intkind1, wf6(:,2), n2(2))
  call vert_UV_W(ntry, wf4(:,1), Q(:,5), ex7, Q(:,64), wf8(:,1), n3(:,4), t3x8(:,:,1))
  call vert_QA_V(ntry, wf2(:,2), wf6(:,2), wf12(:,1), n3(:,5), t3x12(:,:,1))
  call vert_VQ_A(ntry, ex7, wf2(:,2), wf4(:,2), n3(:,6), t3x4(:,:,2))
  call vert_AV_Q(ntry, wf6(:,2), wf4(:,1), wf24(:,1), n3(:,7), t3x24(:,:,1))
  call prop_Q_A(ntry, wf4(:,2), Q(:,82), MB, 1_intkind1, wf4(:,3), n2(3))
  call vert_AV_Q(ntry, wf6(:,2), ex7, wf12(:,2), n3(:,8), t3x12(:,:,2))
  call vert_VQ_A(ntry, wf4(:,1), wf2(:,2), wf8(:,2), n3(:,9), t3x8(:,:,2))
  call prop_A_Q(ntry, wf12(:,2), Q(:,104), MB, 1_intkind1, wf12(:,3), n2(4))
  call vert_AV_Q(ntry, ex4, ex7, wf4(:,4), n3(:,10), t3x4(:,:,3))
  call prop_A_Q(ntry, wf4(:,4), Q(:,72), MB, 1_intkind1, wf4(:,5), n2(5))
  call vert_ZQ_A(gZd,ntry, ex6, wf2(:,2), wf6(:,3), n3(:,11), t3x6(:,:,2))
  call vert_AV_Q(ntry, wf4(:,5), wf4(:,1), wf16(:,1), n3(:,12), t3x16(:,:,1))
  call prop_Q_A(ntry, wf6(:,3), Q(:,50), MB, 1_intkind1, wf6(:,4), n2(6))
  call vert_AZ_Q(gZd,ntry, wf4(:,5), ex6, wf12(:,4), n3(:,13), t3x12(:,:,3))
  call prop_A_Q(ntry, wf12(:,4), Q(:,104), MB, 1_intkind1, wf12(:,5), n2(7))
  call vert_AV_Q(ntry, ex4, wf4(:,1), wf8(:,3), n3(:,14), t3x8(:,:,3))
  call prop_A_Q(ntry, wf8(:,3), Q(:,13), MB, 1_intkind1, wf8(:,4), n2(8))
  call vert_AV_Q(ntry, wf8(:,4), ex7, wf16(:,2), n3(:,15), t3x16(:,:,2))
  call vert_AZ_Q(gZd,ntry, wf8(:,4), ex6, wf24(:,2), n3(:,16), t3x24(:,:,2))
  call vert_QA_V(ntry, wf6(:,4), ex4, wf12(:,6), n3(:,17), t3x12(:,:,4))
  call vert_ZQ_A(gZd,ntry, ex6, ex2, wf6(:,5), n3(:,18), t3x6(:,:,3))
  call vert_SA_Q(gH,ntry, ex5, ex4, wf2(:,3), n3(:,19), t3x2(:,:,2))
  call prop_Q_A(ntry, wf6(:,5), Q(:,34), MB, 1_intkind1, wf6(:,6), n2(9))
  call prop_A_Q(ntry, wf2(:,3), Q(:,24), MB, 1_intkind1, wf2(:,4), n2(10))
  call vert_QA_V(ntry, wf6(:,6), wf2(:,4), wf12(:,7), n3(:,20), t3x12(:,:,5))
  call vert_VQ_A(ntry, ex7, wf6(:,6), wf12(:,8), n3(:,21), t3x12(:,:,6))
  call vert_AV_Q(ntry, wf2(:,4), wf4(:,1), wf8(:,5), n3(:,22), t3x8(:,:,4))
  call prop_Q_A(ntry, wf12(:,8), Q(:,98), MB, 1_intkind1, wf12(:,9), n2(11))
  call vert_AV_Q(ntry, wf2(:,4), ex7, wf4(:,6), n3(:,23), t3x4(:,:,4))
  call vert_VQ_A(ntry, wf4(:,1), wf6(:,6), wf24(:,3), n3(:,24), t3x24(:,:,3))
  call prop_A_Q(ntry, wf4(:,6), Q(:,88), MB, 1_intkind1, wf4(:,7), n2(12))
  call vert_VQ_A(ntry, ex7, ex2, wf4(:,8), n3(:,25), t3x4(:,:,5))
  call prop_Q_A(ntry, wf4(:,8), Q(:,66), MB, 1_intkind1, wf4(:,9), n2(13))
  call vert_ZQ_A(gZd,ntry, ex6, wf4(:,9), wf12(:,10), n3(:,26), t3x12(:,:,7))
  call prop_Q_A(ntry, wf12(:,10), Q(:,98), MB, 1_intkind1, wf12(:,11), n2(14))
  call vert_AZ_Q(gZd,ntry, wf2(:,4), ex6, wf6(:,7), n3(:,27), t3x6(:,:,4))
  call vert_VQ_A(ntry, wf4(:,1), wf4(:,9), wf16(:,3), n3(:,28), t3x16(:,:,3))
  call prop_A_Q(ntry, wf6(:,7), Q(:,56), MB, 1_intkind1, wf6(:,8), n2(15))
  call vert_VQ_A(ntry, wf4(:,1), ex2, wf8(:,6), n3(:,29), t3x8(:,:,5))
  call prop_Q_A(ntry, wf8(:,6), Q(:,7), MB, 1_intkind1, wf8(:,7), n2(16))
  call vert_ZQ_A(gZd,ntry, ex6, wf8(:,7), wf24(:,4), n3(:,30), t3x24(:,:,4))
  call vert_VQ_A(ntry, ex7, wf8(:,7), wf16(:,4), n3(:,31), t3x16(:,:,4))
  call vert_QA_V(ntry, ex2, wf6(:,8), wf12(:,12), n3(:,32), t3x12(:,:,8))
  call vert_QS_A(gH,ntry, wf6(:,6), ex5, wf6(:,9), n3(:,33), t3x6(:,:,5))
  call prop_Q_A(ntry, wf6(:,9), Q(:,50), MB, 1_intkind1, wf6(:,10), n2(17))
  call vert_SA_Q(gH,ntry, ex5, wf4(:,5), wf4(:,10), n3(:,34), t3x4(:,:,6))
  call prop_A_Q(ntry, wf4(:,10), Q(:,88), MB, 1_intkind1, wf4(:,11), n2(18))
  call vert_SA_Q(gH,ntry, ex5, wf8(:,4), wf8(:,8), n3(:,35), t3x8(:,:,6))
  call vert_QA_V(ntry, wf6(:,10), ex4, wf12(:,13), n3(:,36), t3x12(:,:,9))
  call vert_QS_A(gH,ntry, wf4(:,9), ex5, wf4(:,12), n3(:,37), t3x4(:,:,7))
  call prop_Q_A(ntry, wf4(:,12), Q(:,82), MB, 1_intkind1, wf4(:,13), n2(19))
  call vert_SA_Q(gH,ntry, ex5, wf6(:,2), wf6(:,11), n3(:,38), t3x6(:,:,6))
  call prop_A_Q(ntry, wf6(:,11), Q(:,56), MB, 1_intkind1, wf6(:,12), n2(20))
  call vert_QS_A(gH,ntry, wf8(:,7), ex5, wf8(:,9), n3(:,39), t3x8(:,:,7))
  call vert_QA_V(ntry, ex2, wf6(:,12), wf12(:,14), n3(:,40), t3x12(:,:,10))
  call vert_SV_V(ntry, ex5, ex6, wf3(:,1), n3(:,41), t3x3(:,:,1))
  call prop_W_W(ntry, wf3(:,1), Q(:,48), MZ, 1_intkind1, wf3(:,2), n2(21))
  call vert_ZQ_A(gZd,ntry, wf3(:,2), wf4(:,9), wf12(:,15), n3(:,42), t3x12(:,:,11))
  call vert_AZ_Q(gZd,ntry, ex4, wf3(:,2), wf6(:,13), n3(:,43), t3x6(:,:,7))
  call prop_A_Q(ntry, wf6(:,13), Q(:,56), MB, 1_intkind1, wf6(:,14), n2(22))
  call vert_AZ_Q(gZd,ntry, wf4(:,5), wf3(:,2), wf12(:,16), n3(:,44), t3x12(:,:,12))
  call vert_ZQ_A(gZd,ntry, wf3(:,2), ex2, wf6(:,15), n3(:,45), t3x6(:,:,8))
  call prop_Q_A(ntry, wf6(:,15), Q(:,50), MB, 1_intkind1, wf6(:,16), n2(23))
  call vert_VQ_A(ntry, ex7, wf6(:,16), wf12(:,17), n3(:,46), t3x12(:,:,13))
  call vert_QA_V(ntry, wf6(:,16), ex4, wf12(:,18), n3(:,47), t3x12(:,:,14))
  call vert_QA_V(ntry, ex2, wf6(:,14), wf12(:,19), n3(:,48), t3x12(:,:,15))
  call vert_QS_A(gH,ntry, ex1, ex5, wf2(:,5), n3(:,49), t3x2(:,:,3))
  call vert_QA_V(ntry, ex2, ex3, wf4(:,14), n3(:,50), t3x4(:,:,8))
  call prop_Q_A(ntry, wf2(:,5), Q(:,17), MB, 1_intkind1, wf2(:,6), n2(24))
  call vert_VQ_A(ntry, ex7, wf2(:,6), wf4(:,15), n3(:,51), t3x4(:,:,9))
  call vert_AV_Q(ntry, wf6(:,2), wf4(:,14), wf24(:,5), n3(:,52), t3x24(:,:,5))
  call prop_Q_A(ntry, wf4(:,15), Q(:,81), MB, 1_intkind1, wf4(:,16), n2(25))
  call vert_UV_W(ntry, wf4(:,14), Q(:,6), ex7, Q(:,64), wf8(:,10), n3(:,53), t3x8(:,:,8))
  call vert_QA_V(ntry, wf2(:,6), wf6(:,2), wf12(:,20), n3(:,54), t3x12(:,:,16))
  call vert_VQ_A(ntry, wf4(:,14), wf2(:,6), wf8(:,11), n3(:,55), t3x8(:,:,9))
  call vert_ZQ_A(gZd,ntry, ex6, wf2(:,6), wf6(:,17), n3(:,56), t3x6(:,:,9))
  call vert_AV_Q(ntry, wf4(:,5), wf4(:,14), wf16(:,5), n3(:,57), t3x16(:,:,5))
  call prop_Q_A(ntry, wf6(:,17), Q(:,49), MB, 1_intkind1, wf6(:,18), n2(26))
  call vert_AV_Q(ntry, ex4, wf4(:,14), wf8(:,12), n3(:,58), t3x8(:,:,10))
  call prop_A_Q(ntry, wf8(:,12), Q(:,14), MB, 1_intkind1, wf8(:,13), n2(27))
  call vert_AV_Q(ntry, wf8(:,13), ex7, wf16(:,6), n3(:,59), t3x16(:,:,6))
  call vert_QA_V(ntry, wf6(:,18), ex4, wf12(:,21), n3(:,60), t3x12(:,:,17))
  call vert_AZ_Q(gZd,ntry, wf8(:,13), ex6, wf24(:,6), n3(:,61), t3x24(:,:,6))
  call vert_ZQ_A(gZd,ntry, ex6, ex1, wf6(:,19), n3(:,62), t3x6(:,:,10))
  call prop_Q_A(ntry, wf6(:,19), Q(:,33), MB, 1_intkind1, wf6(:,20), n2(28))
  call vert_VQ_A(ntry, ex7, wf6(:,20), wf12(:,22), n3(:,63), t3x12(:,:,18))
  call vert_AV_Q(ntry, wf2(:,4), wf4(:,14), wf8(:,14), n3(:,64), t3x8(:,:,11))
  call prop_Q_A(ntry, wf12(:,22), Q(:,97), MB, 1_intkind1, wf12(:,23), n2(29))
  call vert_QA_V(ntry, wf6(:,20), wf2(:,4), wf12(:,24), n3(:,65), t3x12(:,:,19))
  call vert_VQ_A(ntry, wf4(:,14), wf6(:,20), wf24(:,7), n3(:,66), t3x24(:,:,7))
  call vert_VQ_A(ntry, ex7, ex1, wf4(:,17), n3(:,67), t3x4(:,:,10))
  call prop_Q_A(ntry, wf4(:,17), Q(:,65), MB, 1_intkind1, wf4(:,18), n2(30))
  call vert_ZQ_A(gZd,ntry, ex6, wf4(:,18), wf12(:,25), n3(:,68), t3x12(:,:,20))
  call prop_Q_A(ntry, wf12(:,25), Q(:,97), MB, 1_intkind1, wf12(:,26), n2(31))
  call vert_VQ_A(ntry, wf4(:,14), wf4(:,18), wf16(:,7), n3(:,69), t3x16(:,:,7))
  call vert_VQ_A(ntry, wf4(:,14), ex1, wf8(:,15), n3(:,70), t3x8(:,:,12))
  call prop_Q_A(ntry, wf8(:,15), Q(:,7), MB, 1_intkind1, wf8(:,16), n2(32))
  call vert_ZQ_A(gZd,ntry, ex6, wf8(:,16), wf24(:,8), n3(:,71), t3x24(:,:,8))
  call vert_VQ_A(ntry, ex7, wf8(:,16), wf16(:,8), n3(:,72), t3x16(:,:,8))
  call vert_QA_V(ntry, ex1, wf6(:,8), wf12(:,27), n3(:,73), t3x12(:,:,21))
  call vert_QS_A(gH,ntry, wf6(:,20), ex5, wf6(:,21), n3(:,74), t3x6(:,:,11))
  call prop_Q_A(ntry, wf6(:,21), Q(:,49), MB, 1_intkind1, wf6(:,22), n2(33))
  call vert_QA_V(ntry, wf6(:,22), ex4, wf12(:,28), n3(:,75), t3x12(:,:,22))
  call vert_SA_Q(gH,ntry, ex5, wf8(:,13), wf8(:,17), n3(:,76), t3x8(:,:,13))
  call vert_QS_A(gH,ntry, wf4(:,18), ex5, wf4(:,19), n3(:,77), t3x4(:,:,11))
  call prop_Q_A(ntry, wf4(:,19), Q(:,81), MB, 1_intkind1, wf4(:,20), n2(34))
  call vert_QS_A(gH,ntry, wf8(:,16), ex5, wf8(:,18), n3(:,78), t3x8(:,:,14))
  call vert_QA_V(ntry, ex1, wf6(:,12), wf12(:,29), n3(:,79), t3x12(:,:,23))
  call vert_ZQ_A(gZd,ntry, wf3(:,2), wf4(:,18), wf12(:,30), n3(:,80), t3x12(:,:,24))
  call vert_ZQ_A(gZd,ntry, wf3(:,2), ex1, wf6(:,23), n3(:,81), t3x6(:,:,12))
  call prop_Q_A(ntry, wf6(:,23), Q(:,49), MB, 1_intkind1, wf6(:,24), n2(35))
  call vert_QA_V(ntry, wf6(:,24), ex4, wf12(:,31), n3(:,82), t3x12(:,:,25))
  call vert_QA_V(ntry, ex1, wf6(:,14), wf12(:,32), n3(:,83), t3x12(:,:,26))
  call vert_VQ_A(ntry, ex7, wf6(:,24), wf12(:,33), n3(:,84), t3x12(:,:,27))
  call vert_QA_V(ntry, ex1, ex4, wf4(:,21), n3(:,85), t3x4(:,:,12))
  call vert_AZ_Q(gZd,ntry, ex3, ex6, wf6(:,25), n3(:,86), t3x6(:,:,13))
  call prop_A_Q(ntry, wf6(:,25), Q(:,36), MB, 1_intkind1, wf6(:,26), n2(36))
  call vert_UV_W(ntry, wf4(:,21), Q(:,9), ex7, Q(:,64), wf8(:,19), n3(:,87), t3x8(:,:,15))
  call vert_QA_V(ntry, wf2(:,2), wf6(:,26), wf12(:,34), n3(:,88), t3x12(:,:,28))
  call vert_AV_Q(ntry, wf6(:,26), wf4(:,21), wf24(:,9), n3(:,89), t3x24(:,:,9))
  call vert_AV_Q(ntry, wf6(:,26), ex7, wf12(:,35), n3(:,90), t3x12(:,:,29))
  call vert_VQ_A(ntry, wf4(:,21), wf2(:,2), wf8(:,20), n3(:,91), t3x8(:,:,16))
  call prop_A_Q(ntry, wf12(:,35), Q(:,100), MB, 1_intkind1, wf12(:,36), n2(37))
  call vert_AV_Q(ntry, ex3, ex7, wf4(:,22), n3(:,92), t3x4(:,:,13))
  call prop_A_Q(ntry, wf4(:,22), Q(:,68), MB, 1_intkind1, wf4(:,23), n2(38))
  call vert_AV_Q(ntry, wf4(:,23), wf4(:,21), wf16(:,9), n3(:,93), t3x16(:,:,9))
  call vert_AZ_Q(gZd,ntry, wf4(:,23), ex6, wf12(:,37), n3(:,94), t3x12(:,:,30))
  call prop_A_Q(ntry, wf12(:,37), Q(:,100), MB, 1_intkind1, wf12(:,38), n2(39))
  call vert_AV_Q(ntry, ex3, wf4(:,21), wf8(:,21), n3(:,95), t3x8(:,:,17))
  call prop_A_Q(ntry, wf8(:,21), Q(:,13), MB, 1_intkind1, wf8(:,22), n2(40))
  call vert_AV_Q(ntry, wf8(:,22), ex7, wf16(:,10), n3(:,96), t3x16(:,:,10))
  call vert_AZ_Q(gZd,ntry, wf8(:,22), ex6, wf24(:,10), n3(:,97), t3x24(:,:,10))
  call vert_QA_V(ntry, wf6(:,4), ex3, wf12(:,39), n3(:,98), t3x12(:,:,31))
  call vert_SA_Q(gH,ntry, ex5, ex3, wf2(:,7), n3(:,99), t3x2(:,:,4))
  call prop_A_Q(ntry, wf2(:,7), Q(:,20), MB, 1_intkind1, wf2(:,8), n2(41))
  call vert_QA_V(ntry, wf6(:,6), wf2(:,8), wf12(:,40), n3(:,100), t3x12(:,:,32))
  call vert_AV_Q(ntry, wf2(:,8), wf4(:,21), wf8(:,23), n3(:,101), t3x8(:,:,18))
  call vert_AV_Q(ntry, wf2(:,8), ex7, wf4(:,24), n3(:,102), t3x4(:,:,14))
  call vert_VQ_A(ntry, wf4(:,21), wf6(:,6), wf24(:,11), n3(:,103), t3x24(:,:,11))
  call prop_A_Q(ntry, wf4(:,24), Q(:,84), MB, 1_intkind1, wf4(:,25), n2(42))
  call vert_AZ_Q(gZd,ntry, wf2(:,8), ex6, wf6(:,27), n3(:,104), t3x6(:,:,14))
  call vert_VQ_A(ntry, wf4(:,21), wf4(:,9), wf16(:,11), n3(:,105), t3x16(:,:,11))
  call prop_A_Q(ntry, wf6(:,27), Q(:,52), MB, 1_intkind1, wf6(:,28), n2(43))
  call vert_VQ_A(ntry, wf4(:,21), ex2, wf8(:,24), n3(:,106), t3x8(:,:,19))
  call prop_Q_A(ntry, wf8(:,24), Q(:,11), MB, 1_intkind1, wf8(:,25), n2(44))
  call vert_ZQ_A(gZd,ntry, ex6, wf8(:,25), wf24(:,12), n3(:,107), t3x24(:,:,12))
  call vert_VQ_A(ntry, ex7, wf8(:,25), wf16(:,12), n3(:,108), t3x16(:,:,12))
  call vert_QA_V(ntry, ex2, wf6(:,28), wf12(:,41), n3(:,109), t3x12(:,:,33))
  call vert_SA_Q(gH,ntry, ex5, wf4(:,23), wf4(:,26), n3(:,110), t3x4(:,:,15))
  call prop_A_Q(ntry, wf4(:,26), Q(:,84), MB, 1_intkind1, wf4(:,27), n2(45))
  call vert_SA_Q(gH,ntry, ex5, wf8(:,22), wf8(:,26), n3(:,111), t3x8(:,:,20))
  call vert_QA_V(ntry, wf6(:,10), ex3, wf12(:,42), n3(:,112), t3x12(:,:,34))
  call vert_SA_Q(gH,ntry, ex5, wf6(:,26), wf6(:,29), n3(:,113), t3x6(:,:,15))
  call prop_A_Q(ntry, wf6(:,29), Q(:,52), MB, 1_intkind1, wf6(:,30), n2(46))
  call vert_QS_A(gH,ntry, wf8(:,25), ex5, wf8(:,27), n3(:,114), t3x8(:,:,21))
  call vert_QA_V(ntry, ex2, wf6(:,30), wf12(:,43), n3(:,115), t3x12(:,:,35))
  call vert_AZ_Q(gZd,ntry, ex3, wf3(:,2), wf6(:,31), n3(:,116), t3x6(:,:,16))
  call prop_A_Q(ntry, wf6(:,31), Q(:,52), MB, 1_intkind1, wf6(:,32), n2(47))
  call vert_AZ_Q(gZd,ntry, wf4(:,23), wf3(:,2), wf12(:,44), n3(:,117), t3x12(:,:,36))
  call vert_QA_V(ntry, wf6(:,16), ex3, wf12(:,45), n3(:,118), t3x12(:,:,37))
  call vert_QA_V(ntry, ex2, wf6(:,32), wf12(:,46), n3(:,119), t3x12(:,:,38))
  call vert_QA_V(ntry, ex2, ex4, wf4(:,28), n3(:,120), t3x4(:,:,16))
  call vert_AV_Q(ntry, wf6(:,26), wf4(:,28), wf24(:,13), n3(:,121), t3x24(:,:,13))
  call vert_UV_W(ntry, wf4(:,28), Q(:,10), ex7, Q(:,64), wf8(:,28), n3(:,122), t3x8(:,:,22))
  call vert_QA_V(ntry, wf2(:,6), wf6(:,26), wf12(:,47), n3(:,123), t3x12(:,:,39))
  call vert_VQ_A(ntry, wf4(:,28), wf2(:,6), wf8(:,29), n3(:,124), t3x8(:,:,23))
  call vert_AV_Q(ntry, wf4(:,23), wf4(:,28), wf16(:,13), n3(:,125), t3x16(:,:,13))
  call vert_AV_Q(ntry, ex3, wf4(:,28), wf8(:,30), n3(:,126), t3x8(:,:,24))
  call prop_A_Q(ntry, wf8(:,30), Q(:,14), MB, 1_intkind1, wf8(:,31), n2(48))
  call vert_AV_Q(ntry, wf8(:,31), ex7, wf16(:,14), n3(:,127), t3x16(:,:,14))
  call vert_QA_V(ntry, wf6(:,18), ex3, wf12(:,48), n3(:,128), t3x12(:,:,40))
  call vert_AZ_Q(gZd,ntry, wf8(:,31), ex6, wf24(:,14), n3(:,129), t3x24(:,:,14))
  call vert_AV_Q(ntry, wf2(:,8), wf4(:,28), wf8(:,32), n3(:,130), t3x8(:,:,25))
  call vert_QA_V(ntry, wf6(:,20), wf2(:,8), wf12(:,49), n3(:,131), t3x12(:,:,41))
  call vert_VQ_A(ntry, wf4(:,28), wf6(:,20), wf24(:,15), n3(:,132), t3x24(:,:,15))
  call vert_VQ_A(ntry, wf4(:,28), wf4(:,18), wf16(:,15), n3(:,133), t3x16(:,:,15))
  call vert_VQ_A(ntry, wf4(:,28), ex1, wf8(:,33), n3(:,134), t3x8(:,:,26))
  call prop_Q_A(ntry, wf8(:,33), Q(:,11), MB, 1_intkind1, wf8(:,34), n2(49))
  call vert_ZQ_A(gZd,ntry, ex6, wf8(:,34), wf24(:,16), n3(:,135), t3x24(:,:,16))
  call vert_VQ_A(ntry, ex7, wf8(:,34), wf16(:,16), n3(:,136), t3x16(:,:,16))
  call vert_QA_V(ntry, ex1, wf6(:,28), wf12(:,50), n3(:,137), t3x12(:,:,42))
  call vert_QA_V(ntry, wf6(:,22), ex3, wf12(:,51), n3(:,138), t3x12(:,:,43))
  call vert_SA_Q(gH,ntry, ex5, wf8(:,31), wf8(:,35), n3(:,139), t3x8(:,:,27))
  call vert_QS_A(gH,ntry, wf8(:,34), ex5, wf8(:,36), n3(:,140), t3x8(:,:,28))
  call vert_QA_V(ntry, ex1, wf6(:,30), wf12(:,52), n3(:,141), t3x12(:,:,44))
  call vert_QA_V(ntry, wf6(:,24), ex3, wf12(:,53), n3(:,142), t3x12(:,:,45))
  call vert_QA_V(ntry, ex1, wf6(:,32), wf12(:,54), n3(:,143), t3x12(:,:,46))
  call vert_QA_V(ntry, wf2(:,6), ex4, wf4(:,29), n3(:,144), t3x4(:,:,17))
  call vert_QA_V(ntry, wf6(:,6), wf4(:,23), wf24(:,17), n3(:,145), t3x24(:,:,17))
  call vert_QA_V(ntry, wf6(:,6), ex4, wf12(:,55), n3(:,146), t3x12(:,:,47))
  call vert_QA_V(ntry, wf2(:,6), wf4(:,23), wf8(:,37), n3(:,147), t3x8(:,:,29))
  call vert_QA_V(ntry, wf2(:,6), ex3, wf4(:,30), n3(:,148), t3x4(:,:,18))
  call vert_QA_V(ntry, wf6(:,6), wf4(:,5), wf24(:,18), n3(:,149), t3x24(:,:,18))
  call vert_QA_V(ntry, wf6(:,6), ex3, wf12(:,56), n3(:,150), t3x12(:,:,48))
  call vert_QA_V(ntry, wf2(:,6), wf4(:,5), wf8(:,38), n3(:,151), t3x8(:,:,30))
  call vert_UV_W(ntry, wf4(:,30), Q(:,21), ex7, Q(:,64), wf8(:,39), n3(:,152), t3x8(:,:,31))
  call vert_AV_Q(ntry, ex4, wf4(:,30), wf8(:,40), n3(:,153), t3x8(:,:,32))
  call vert_UV_W(ntry, wf12(:,56), Q(:,38), ex7, Q(:,64), wf24(:,19), n3(:,154), t3x24(:,:,19))
  call vert_AV_Q(ntry, ex3, wf4(:,29), wf8(:,41), n3(:,155), t3x8(:,:,33))
  call vert_AV_Q(ntry, ex4, wf12(:,56), wf24(:,20), n3(:,156), t3x24(:,:,20))
  call vert_AV_Q(ntry, ex3, wf12(:,55), wf24(:,21), n3(:,157), t3x24(:,:,21))
  call vert_QA_V(ntry, wf4(:,9), wf6(:,26), wf24(:,22), n3(:,158), t3x24(:,:,22))
  call vert_QA_V(ntry, wf4(:,9), ex4, wf8(:,42), n3(:,159), t3x8(:,:,34))
  call vert_QA_V(ntry, ex2, wf6(:,26), wf12(:,57), n3(:,160), t3x12(:,:,49))
  call vert_QA_V(ntry, ex2, wf4(:,5), wf8(:,43), n3(:,161), t3x8(:,:,35))
  call vert_UV_W(ntry, wf12(:,57), Q(:,38), ex7, Q(:,64), wf24(:,23), n3(:,162), t3x24(:,:,23))
  call vert_VQ_A(ntry, wf4(:,29), ex2, wf8(:,44), n3(:,163), t3x8(:,:,36))
  call vert_AV_Q(ntry, ex4, wf12(:,57), wf24(:,24), n3(:,164), t3x24(:,:,24))
  call vert_QA_V(ntry, wf4(:,9), wf6(:,2), wf24(:,25), n3(:,165), t3x24(:,:,25))
  call vert_QA_V(ntry, wf4(:,9), ex3, wf8(:,45), n3(:,166), t3x8(:,:,37))
  call vert_QA_V(ntry, ex2, wf4(:,23), wf8(:,46), n3(:,167), t3x8(:,:,38))
  call vert_QA_V(ntry, ex2, wf6(:,2), wf12(:,58), n3(:,168), t3x12(:,:,50))
  call vert_VQ_A(ntry, wf4(:,30), ex2, wf8(:,47), n3(:,169), t3x8(:,:,39))
  call vert_UV_W(ntry, wf12(:,58), Q(:,42), ex7, Q(:,64), wf24(:,26), n3(:,170), t3x24(:,:,26))
  call vert_AV_Q(ntry, ex3, wf12(:,58), wf24(:,27), n3(:,171), t3x24(:,:,27))
  call vert_AV_Q(ntry, ex4, wf8(:,45), wf16(:,17), n3(:,172), t3x16(:,:,17))
  call vert_AV_Q(ntry, ex3, wf8(:,42), wf16(:,18), n3(:,173), t3x16(:,:,18))
  call vert_AV_Q(ntry, ex4, wf8(:,46), wf16(:,19), n3(:,174), t3x16(:,:,19))
  call vert_AV_Q(ntry, ex3, wf8(:,43), wf16(:,20), n3(:,175), t3x16(:,:,20))
  call vert_QA_V(ntry, wf6(:,20), ex4, wf12(:,59), n3(:,176), t3x12(:,:,51))
  call vert_QA_V(ntry, wf2(:,2), wf4(:,23), wf8(:,48), n3(:,177), t3x8(:,:,40))
  call vert_QA_V(ntry, wf2(:,2), ex4, wf4(:,31), n3(:,178), t3x4(:,:,19))
  call vert_QA_V(ntry, wf6(:,20), wf4(:,23), wf24(:,28), n3(:,179), t3x24(:,:,28))
  call vert_QA_V(ntry, wf6(:,20), ex3, wf12(:,60), n3(:,180), t3x12(:,:,52))
  call vert_QA_V(ntry, wf2(:,2), wf4(:,5), wf8(:,49), n3(:,181), t3x8(:,:,41))
  call vert_QA_V(ntry, wf2(:,2), ex3, wf4(:,32), n3(:,182), t3x4(:,:,20))
  call vert_QA_V(ntry, wf6(:,20), wf4(:,5), wf24(:,29), n3(:,183), t3x24(:,:,29))
  call vert_UV_W(ntry, wf12(:,60), Q(:,37), ex7, Q(:,64), wf24(:,30), n3(:,184), t3x24(:,:,30))
  call vert_AV_Q(ntry, ex4, wf12(:,60), wf24(:,31), n3(:,185), t3x24(:,:,31))
  call vert_UV_W(ntry, wf4(:,32), Q(:,22), ex7, Q(:,64), wf8(:,50), n3(:,186), t3x8(:,:,42))
  call vert_AV_Q(ntry, ex3, wf12(:,59), wf24(:,32), n3(:,187), t3x24(:,:,32))
  call vert_AV_Q(ntry, ex4, wf4(:,32), wf8(:,51), n3(:,188), t3x8(:,:,43))
  call vert_AV_Q(ntry, ex3, wf4(:,31), wf8(:,52), n3(:,189), t3x8(:,:,44))
  call vert_QA_V(ntry, wf4(:,18), ex4, wf8(:,53), n3(:,190), t3x8(:,:,45))
  call vert_QA_V(ntry, wf4(:,18), wf6(:,26), wf24(:,33), n3(:,191), t3x24(:,:,33))
  call vert_QA_V(ntry, ex1, wf6(:,26), wf12(:,61), n3(:,192), t3x12(:,:,53))
  call vert_QA_V(ntry, ex1, wf4(:,5), wf8(:,54), n3(:,193), t3x8(:,:,46))
  call vert_AV_Q(ntry, ex4, wf12(:,61), wf24(:,34), n3(:,194), t3x24(:,:,34))
  call vert_UV_W(ntry, wf12(:,61), Q(:,37), ex7, Q(:,64), wf24(:,35), n3(:,195), t3x24(:,:,35))
  call vert_VQ_A(ntry, wf4(:,31), ex1, wf8(:,55), n3(:,196), t3x8(:,:,47))
  call vert_QA_V(ntry, wf4(:,18), ex3, wf8(:,56), n3(:,197), t3x8(:,:,48))
  call vert_QA_V(ntry, wf4(:,18), wf6(:,2), wf24(:,36), n3(:,198), t3x24(:,:,36))
  call vert_QA_V(ntry, ex1, wf4(:,23), wf8(:,57), n3(:,199), t3x8(:,:,49))
  call vert_QA_V(ntry, ex1, wf6(:,2), wf12(:,62), n3(:,200), t3x12(:,:,54))
  call vert_AV_Q(ntry, ex3, wf12(:,62), wf24(:,37), n3(:,201), t3x24(:,:,37))
  call vert_VQ_A(ntry, wf4(:,32), ex1, wf8(:,58), n3(:,202), t3x8(:,:,50))
  call vert_UV_W(ntry, wf12(:,62), Q(:,41), ex7, Q(:,64), wf24(:,38), n3(:,203), t3x24(:,:,38))
  call vert_AV_Q(ntry, ex4, wf8(:,56), wf16(:,21), n3(:,204), t3x16(:,:,21))
  call vert_AV_Q(ntry, ex3, wf8(:,53), wf16(:,22), n3(:,205), t3x16(:,:,22))
  call vert_AV_Q(ntry, ex4, wf8(:,57), wf16(:,23), n3(:,206), t3x16(:,:,23))
  call vert_AV_Q(ntry, ex3, wf8(:,54), wf16(:,24), n3(:,207), t3x16(:,:,24))
  call vert_QA_V(ntry, wf4(:,9), wf2(:,8), wf8(:,59), n3(:,208), t3x8(:,:,51))
  call vert_QA_V(ntry, ex2, wf2(:,8), wf4(:,33), n3(:,209), t3x4(:,:,21))
  call vert_UV_W(ntry, wf4(:,33), Q(:,22), ex7, Q(:,64), wf8(:,60), n3(:,210), t3x8(:,:,52))
  call vert_VQ_A(ntry, wf12(:,59), ex2, wf24(:,39), n3(:,211), t3x24(:,:,39))
  call vert_AV_Q(ntry, ex4, wf4(:,33), wf8(:,61), n3(:,212), t3x8(:,:,53))
  call vert_QA_V(ntry, wf4(:,18), wf2(:,8), wf8(:,62), n3(:,213), t3x8(:,:,54))
  call vert_QA_V(ntry, ex1, wf2(:,8), wf4(:,34), n3(:,214), t3x4(:,:,22))
  call vert_AV_Q(ntry, ex4, wf4(:,34), wf8(:,63), n3(:,215), t3x8(:,:,55))
  call vert_UV_W(ntry, wf4(:,34), Q(:,21), ex7, Q(:,64), wf8(:,64), n3(:,216), t3x8(:,:,56))
  call vert_VQ_A(ntry, wf12(:,55), ex1, wf24(:,40), n3(:,217), t3x24(:,:,40))
  call vert_VQ_A(ntry, wf4(:,34), ex2, wf8(:,65), n3(:,218), t3x8(:,:,57))
  call vert_VQ_A(ntry, wf12(:,62), ex2, wf24(:,41), n3(:,219), t3x24(:,:,41))
  call vert_VQ_A(ntry, wf4(:,33), ex1, wf8(:,66), n3(:,220), t3x8(:,:,58))
  call vert_VQ_A(ntry, wf12(:,58), ex1, wf24(:,42), n3(:,221), t3x24(:,:,42))
  call vert_VQ_A(ntry, wf8(:,53), ex2, wf16(:,25), n3(:,222), t3x16(:,:,25))
  call vert_VQ_A(ntry, wf8(:,42), ex1, wf16(:,26), n3(:,223), t3x16(:,:,26))
  call vert_VQ_A(ntry, wf8(:,54), ex2, wf16(:,27), n3(:,224), t3x16(:,:,27))
  call vert_VQ_A(ntry, wf8(:,43), ex1, wf16(:,28), n3(:,225), t3x16(:,:,28))
  call vert_QA_V(ntry, wf4(:,9), wf2(:,4), wf8(:,67), n3(:,226), t3x8(:,:,59))
  call vert_QA_V(ntry, ex2, wf2(:,4), wf4(:,35), n3(:,227), t3x4(:,:,23))
  call vert_VQ_A(ntry, wf12(:,60), ex2, wf24(:,43), n3(:,228), t3x24(:,:,43))
  call vert_UV_W(ntry, wf4(:,35), Q(:,26), ex7, Q(:,64), wf8(:,68), n3(:,229), t3x8(:,:,60))
  call vert_AV_Q(ntry, ex3, wf4(:,35), wf8(:,69), n3(:,230), t3x8(:,:,61))
  call vert_QA_V(ntry, wf4(:,18), wf2(:,4), wf8(:,70), n3(:,231), t3x8(:,:,62))
  call vert_QA_V(ntry, ex1, wf2(:,4), wf4(:,36), n3(:,232), t3x4(:,:,24))
  call vert_AV_Q(ntry, ex3, wf4(:,36), wf8(:,71), n3(:,233), t3x8(:,:,63))
  call vert_VQ_A(ntry, wf12(:,56), ex1, wf24(:,44), n3(:,234), t3x24(:,:,44))
  call vert_UV_W(ntry, wf4(:,36), Q(:,25), ex7, Q(:,64), wf8(:,72), n3(:,235), t3x8(:,:,64))
  call vert_VQ_A(ntry, wf12(:,61), ex2, wf24(:,45), n3(:,236), t3x24(:,:,45))
  call vert_VQ_A(ntry, wf4(:,36), ex2, wf8(:,73), n3(:,237), t3x8(:,:,65))
  call vert_VQ_A(ntry, wf12(:,57), ex1, wf24(:,46), n3(:,238), t3x24(:,:,46))
  call vert_VQ_A(ntry, wf4(:,35), ex1, wf8(:,74), n3(:,239), t3x8(:,:,66))
  call vert_VQ_A(ntry, wf8(:,56), ex2, wf16(:,29), n3(:,240), t3x16(:,:,29))
  call vert_VQ_A(ntry, wf8(:,45), ex1, wf16(:,30), n3(:,241), t3x16(:,:,30))
  call vert_VQ_A(ntry, wf8(:,57), ex2, wf16(:,31), n3(:,242), t3x16(:,:,31))
  call vert_VQ_A(ntry, wf8(:,46), ex1, wf16(:,32), n3(:,243), t3x16(:,:,32))


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
  M2add = M2 / average_factor_pphzjj_bbbxbxhzg_1

  do k = 0, 30-1
    M2(k) = M2add(extcomb_perm_pphzjj_bbbxbxhzg_1(k))
  end do

  if (ntry == 1) ntry = 2

  if (scalefactor /= old_scalefactor) then
    scalebackfactor = scalefactor**(2*7-8)
    old_scalefactor = scalefactor
  end if
  M2 = scalebackfactor * M2

  contains

subroutine diagrams()
  implicit none
  ! e.g. call cont_VV(nsync, wf3, wf6, A(:,1), n64, t64, nhel, den(5)) ...

    call cont_VV(nsync, wf8(:,1), wf12(:,1), A(:,1), n3(:,244), t3x96(:,:,1), nhel, den(7))
    call cont_QA(nsync, wf24(:,1), wf4(:,3), A(:,2), n3(:,245), t3x96(:,:,2), nhel, den(11))
    call cont_QA(nsync, wf8(:,2), wf12(:,3), A(:,3), n3(:,246), t3x96(:,:,3), nhel, den(15))
    call cont_QA(nsync, wf16(:,1), wf6(:,4), A(:,4), n3(:,247), t3x96(:,:,4), nhel, den(20))
    call cont_QA(nsync, wf8(:,2), wf12(:,5), A(:,5), n3(:,248), t3x96(:,:,5), nhel, den(22))
    call cont_QA(nsync, wf6(:,4), wf16(:,2), A(:,6), n3(:,249), t3x96(:,:,6), nhel, den(25))
    call cont_QA(nsync, wf4(:,3), wf24(:,2), A(:,7), n3(:,250), t3x96(:,:,7), nhel, den(26))
    call cont_VV(nsync, wf8(:,1), wf12(:,6), A(:,8), n3(:,251), t3x96(:,:,8), nhel, den(27))
    call cont_VV(nsync, wf8(:,1), wf12(:,7), A(:,9), n3(:,252), t3x96(:,:,9), nhel, den(31))
    call cont_QA(nsync, wf8(:,5), wf12(:,9), A(:,10), n3(:,253), t3x96(:,:,10), nhel, den(35))
    call cont_QA(nsync, wf24(:,3), wf4(:,7), A(:,11), n3(:,254), t3x96(:,:,11), nhel, den(39))
    call cont_QA(nsync, wf8(:,5), wf12(:,11), A(:,12), n3(:,255), t3x96(:,:,12), nhel, den(42))
    call cont_QA(nsync, wf16(:,3), wf6(:,8), A(:,13), n3(:,256), t3x96(:,:,13), nhel, den(46))
    call cont_QA(nsync, wf4(:,7), wf24(:,4), A(:,14), n3(:,257), t3x96(:,:,14), nhel, den(49))
    call cont_QA(nsync, wf6(:,8), wf16(:,4), A(:,15), n3(:,258), t3x96(:,:,15), nhel, den(50))
    call cont_VV(nsync, wf8(:,1), wf12(:,12), A(:,16), n3(:,259), t3x96(:,:,16), nhel, den(51))
    call cont_QA(nsync, wf16(:,1), wf6(:,10), A(:,17), n3(:,260), t3x96(:,:,17), nhel, den(53))
    call cont_QA(nsync, wf24(:,3), wf4(:,11), A(:,18), n3(:,261), t3x96(:,:,18), nhel, den(55))
    call cont_QA(nsync, wf16(:,2), wf6(:,10), A(:,19), n3(:,262), t3x96(:,:,19), nhel, den(56))
    call cont_QA(nsync, wf12(:,9), wf8(:,8), A(:,20), n3(:,263), t3x96(:,:,20), nhel, den(57))
    call cont_VV(nsync, wf8(:,1), wf12(:,13), A(:,21), n3(:,264), t3x96(:,:,21), nhel, den(58))
    call cont_QA(nsync, wf24(:,1), wf4(:,13), A(:,22), n3(:,265), t3x96(:,:,22), nhel, den(60))
    call cont_QA(nsync, wf16(:,3), wf6(:,12), A(:,23), n3(:,266), t3x96(:,:,23), nhel, den(62))
    call cont_QA(nsync, wf12(:,3), wf8(:,9), A(:,24), n3(:,267), t3x96(:,:,24), nhel, den(63))
    call cont_QA(nsync, wf16(:,4), wf6(:,12), A(:,25), n3(:,268), t3x96(:,:,25), nhel, den(64))
    call cont_VV(nsync, wf8(:,1), wf12(:,14), A(:,26), n3(:,269), t3x96(:,:,26), nhel, den(65))
    call cont_QA(nsync, wf8(:,4), wf12(:,15), A(:,27), n3(:,270), t3x96(:,:,27), nhel, den(68))
    call cont_QA(nsync, wf16(:,3), wf6(:,14), A(:,28), n3(:,271), t3x96(:,:,28), nhel, den(70))
    call cont_QA(nsync, wf8(:,7), wf12(:,16), A(:,29), n3(:,272), t3x96(:,:,29), nhel, den(72))
    call cont_QA(nsync, wf16(:,1), wf6(:,16), A(:,30), n3(:,273), t3x96(:,:,30), nhel, den(74))
    call cont_QA(nsync, wf16(:,4), wf6(:,14), A(:,31), n3(:,274), t3x96(:,:,31), nhel, den(75))
    call cont_QA(nsync, wf8(:,4), wf12(:,17), A(:,32), n3(:,275), t3x96(:,:,32), nhel, den(76))
    call cont_VV(nsync, wf8(:,1), wf12(:,18), A(:,33), n3(:,276), t3x96(:,:,33), nhel, den(77))
    call cont_VV(nsync, wf8(:,1), wf12(:,19), A(:,34), n3(:,277), t3x96(:,:,34), nhel, den(78))
    call cont_QA(nsync, wf24(:,2), wf4(:,13), A(:,35), n3(:,278), t3x96(:,:,35), nhel, den(79))
    call cont_QA(nsync, wf12(:,11), wf8(:,8), A(:,36), n3(:,279), t3x96(:,:,36), nhel, den(80))
    call cont_QA(nsync, wf12(:,5), wf8(:,9), A(:,37), n3(:,280), t3x96(:,:,37), nhel, den(81))
    call cont_QA(nsync, wf24(:,4), wf4(:,11), A(:,38), n3(:,281), t3x96(:,:,38), nhel, den(82))
    call cont_QA(nsync, wf24(:,5), wf4(:,16), A(:,39), n3(:,282), t3x96(:,:,39), nhel, den(88))
    call cont_VV(nsync, wf8(:,10), wf12(:,20), A(:,40), n3(:,283), t3x96(:,:,40), nhel, den(92))
    call cont_QA(nsync, wf12(:,3), wf8(:,11), A(:,41), n3(:,284), t3x96(:,:,41), nhel, den(94))
    call cont_QA(nsync, wf16(:,5), wf6(:,18), A(:,42), n3(:,285), t3x96(:,:,42), nhel, den(98))
    call cont_QA(nsync, wf12(:,5), wf8(:,11), A(:,43), n3(:,286), t3x96(:,:,43), nhel, den(99))
    call cont_QA(nsync, wf6(:,18), wf16(:,6), A(:,44), n3(:,287), t3x96(:,:,44), nhel, den(102))
    call cont_VV(nsync, wf8(:,10), wf12(:,21), A(:,45), n3(:,288), t3x96(:,:,45), nhel, den(103))
    call cont_QA(nsync, wf4(:,16), wf24(:,6), A(:,46), n3(:,289), t3x96(:,:,46), nhel, den(104))
    call cont_QA(nsync, wf8(:,14), wf12(:,23), A(:,47), n3(:,290), t3x96(:,:,47), nhel, den(109))
    call cont_VV(nsync, wf8(:,10), wf12(:,24), A(:,48), n3(:,291), t3x96(:,:,48), nhel, den(111))
    call cont_QA(nsync, wf4(:,7), wf24(:,7), A(:,49), n3(:,292), t3x96(:,:,49), nhel, den(113))
    call cont_QA(nsync, wf8(:,14), wf12(:,26), A(:,50), n3(:,293), t3x96(:,:,50), nhel, den(116))
    call cont_QA(nsync, wf6(:,8), wf16(:,7), A(:,51), n3(:,294), t3x96(:,:,51), nhel, den(118))
    call cont_QA(nsync, wf4(:,7), wf24(:,8), A(:,52), n3(:,295), t3x96(:,:,52), nhel, den(120))
    call cont_QA(nsync, wf6(:,8), wf16(:,8), A(:,53), n3(:,296), t3x96(:,:,53), nhel, den(121))
    call cont_VV(nsync, wf8(:,10), wf12(:,27), A(:,54), n3(:,297), t3x96(:,:,54), nhel, den(122))
    call cont_QA(nsync, wf16(:,5), wf6(:,22), A(:,55), n3(:,298), t3x96(:,:,55), nhel, den(124))
    call cont_QA(nsync, wf4(:,11), wf24(:,7), A(:,56), n3(:,299), t3x96(:,:,56), nhel, den(125))
    call cont_QA(nsync, wf16(:,6), wf6(:,22), A(:,57), n3(:,300), t3x96(:,:,57), nhel, den(126))
    call cont_VV(nsync, wf8(:,10), wf12(:,28), A(:,58), n3(:,301), t3x96(:,:,58), nhel, den(127))
    call cont_QA(nsync, wf12(:,23), wf8(:,17), A(:,59), n3(:,302), t3x96(:,:,59), nhel, den(128))
    call cont_QA(nsync, wf24(:,5), wf4(:,20), A(:,60), n3(:,303), t3x96(:,:,60), nhel, den(130))
    call cont_QA(nsync, wf6(:,12), wf16(:,7), A(:,61), n3(:,304), t3x96(:,:,61), nhel, den(131))
    call cont_QA(nsync, wf12(:,3), wf8(:,18), A(:,62), n3(:,305), t3x96(:,:,62), nhel, den(132))
    call cont_QA(nsync, wf6(:,12), wf16(:,8), A(:,63), n3(:,306), t3x96(:,:,63), nhel, den(133))
    call cont_VV(nsync, wf8(:,10), wf12(:,29), A(:,64), n3(:,307), t3x96(:,:,64), nhel, den(134))
    call cont_QA(nsync, wf8(:,13), wf12(:,30), A(:,65), n3(:,308), t3x96(:,:,65), nhel, den(136))
    call cont_QA(nsync, wf6(:,14), wf16(:,7), A(:,66), n3(:,309), t3x96(:,:,66), nhel, den(137))
    call cont_QA(nsync, wf12(:,16), wf8(:,16), A(:,67), n3(:,310), t3x96(:,:,67), nhel, den(138))
    call cont_QA(nsync, wf16(:,5), wf6(:,24), A(:,68), n3(:,311), t3x96(:,:,68), nhel, den(140))
    call cont_QA(nsync, wf6(:,14), wf16(:,8), A(:,69), n3(:,312), t3x96(:,:,69), nhel, den(141))
    call cont_VV(nsync, wf8(:,10), wf12(:,31), A(:,70), n3(:,313), t3x96(:,:,70), nhel, den(142))
    call cont_VV(nsync, wf8(:,10), wf12(:,32), A(:,71), n3(:,314), t3x96(:,:,71), nhel, den(143))
    call cont_QA(nsync, wf8(:,13), wf12(:,33), A(:,72), n3(:,315), t3x96(:,:,72), nhel, den(144))
    call cont_QA(nsync, wf24(:,6), wf4(:,20), A(:,73), n3(:,316), t3x96(:,:,73), nhel, den(145))
    call cont_QA(nsync, wf12(:,26), wf8(:,17), A(:,74), n3(:,317), t3x96(:,:,74), nhel, den(146))
    call cont_QA(nsync, wf12(:,5), wf8(:,18), A(:,75), n3(:,318), t3x96(:,:,75), nhel, den(147))
    call cont_QA(nsync, wf4(:,11), wf24(:,8), A(:,76), n3(:,319), t3x96(:,:,76), nhel, den(148))
    call cont_VV(nsync, wf8(:,19), wf12(:,34), A(:,77), n3(:,320), t3x96(:,:,77), nhel, den(154))
    call cont_QA(nsync, wf4(:,3), wf24(:,9), A(:,78), n3(:,321), t3x96(:,:,78), nhel, den(156))
    call cont_QA(nsync, wf8(:,20), wf12(:,36), A(:,79), n3(:,322), t3x96(:,:,79), nhel, den(160))
    call cont_QA(nsync, wf6(:,4), wf16(:,9), A(:,80), n3(:,323), t3x96(:,:,80), nhel, den(163))
    call cont_QA(nsync, wf8(:,20), wf12(:,38), A(:,81), n3(:,324), t3x96(:,:,81), nhel, den(165))
    call cont_QA(nsync, wf6(:,4), wf16(:,10), A(:,82), n3(:,325), t3x96(:,:,82), nhel, den(167))
    call cont_QA(nsync, wf4(:,3), wf24(:,10), A(:,83), n3(:,326), t3x96(:,:,83), nhel, den(168))
    call cont_VV(nsync, wf8(:,19), wf12(:,39), A(:,84), n3(:,327), t3x96(:,:,84), nhel, den(169))
    call cont_VV(nsync, wf8(:,19), wf12(:,40), A(:,85), n3(:,328), t3x96(:,:,85), nhel, den(172))
    call cont_QA(nsync, wf12(:,9), wf8(:,23), A(:,86), n3(:,329), t3x96(:,:,86), nhel, den(174))
    call cont_QA(nsync, wf24(:,11), wf4(:,25), A(:,87), n3(:,330), t3x96(:,:,87), nhel, den(178))
    call cont_QA(nsync, wf12(:,11), wf8(:,23), A(:,88), n3(:,331), t3x96(:,:,88), nhel, den(179))
    call cont_QA(nsync, wf16(:,11), wf6(:,28), A(:,89), n3(:,332), t3x96(:,:,89), nhel, den(183))
    call cont_QA(nsync, wf4(:,25), wf24(:,12), A(:,90), n3(:,333), t3x96(:,:,90), nhel, den(186))
    call cont_QA(nsync, wf6(:,28), wf16(:,12), A(:,91), n3(:,334), t3x96(:,:,91), nhel, den(187))
    call cont_VV(nsync, wf8(:,19), wf12(:,41), A(:,92), n3(:,335), t3x96(:,:,92), nhel, den(188))
    call cont_QA(nsync, wf6(:,10), wf16(:,9), A(:,93), n3(:,336), t3x96(:,:,93), nhel, den(189))
    call cont_QA(nsync, wf24(:,11), wf4(:,27), A(:,94), n3(:,337), t3x96(:,:,94), nhel, den(191))
    call cont_QA(nsync, wf6(:,10), wf16(:,10), A(:,95), n3(:,338), t3x96(:,:,95), nhel, den(192))
    call cont_QA(nsync, wf12(:,9), wf8(:,26), A(:,96), n3(:,339), t3x96(:,:,96), nhel, den(193))
    call cont_VV(nsync, wf8(:,19), wf12(:,42), A(:,97), n3(:,340), t3x96(:,:,97), nhel, den(194))
    call cont_QA(nsync, wf4(:,13), wf24(:,9), A(:,98), n3(:,341), t3x96(:,:,98), nhel, den(195))
    call cont_QA(nsync, wf16(:,11), wf6(:,30), A(:,99), n3(:,342), t3x96(:,:,99), nhel, den(197))
    call cont_QA(nsync, wf12(:,36), wf8(:,27), A(:,100), n3(:,343), t3x96(:,:,100), nhel, den(198))
    call cont_QA(nsync, wf16(:,12), wf6(:,30), A(:,101), n3(:,344), t3x96(:,:,101), nhel, den(199))
    call cont_VV(nsync, wf8(:,19), wf12(:,43), A(:,102), n3(:,345), t3x96(:,:,102), nhel, den(200))
    call cont_QA(nsync, wf12(:,15), wf8(:,22), A(:,103), n3(:,346), t3x96(:,:,103), nhel, den(201))
    call cont_QA(nsync, wf16(:,11), wf6(:,32), A(:,104), n3(:,347), t3x96(:,:,104), nhel, den(203))
    call cont_QA(nsync, wf8(:,25), wf12(:,44), A(:,105), n3(:,348), t3x96(:,:,105), nhel, den(205))
    call cont_QA(nsync, wf6(:,16), wf16(:,9), A(:,106), n3(:,349), t3x96(:,:,106), nhel, den(206))
    call cont_QA(nsync, wf16(:,12), wf6(:,32), A(:,107), n3(:,350), t3x96(:,:,107), nhel, den(207))
    call cont_QA(nsync, wf12(:,17), wf8(:,22), A(:,108), n3(:,351), t3x96(:,:,108), nhel, den(208))
    call cont_VV(nsync, wf8(:,19), wf12(:,45), A(:,109), n3(:,352), t3x96(:,:,109), nhel, den(209))
    call cont_VV(nsync, wf8(:,19), wf12(:,46), A(:,110), n3(:,353), t3x96(:,:,110), nhel, den(210))
    call cont_QA(nsync, wf4(:,13), wf24(:,10), A(:,111), n3(:,354), t3x96(:,:,111), nhel, den(211))
    call cont_QA(nsync, wf12(:,11), wf8(:,26), A(:,112), n3(:,355), t3x96(:,:,112), nhel, den(212))
    call cont_QA(nsync, wf12(:,38), wf8(:,27), A(:,113), n3(:,356), t3x96(:,:,113), nhel, den(213))
    call cont_QA(nsync, wf24(:,12), wf4(:,27), A(:,114), n3(:,357), t3x96(:,:,114), nhel, den(214))
    call cont_QA(nsync, wf4(:,16), wf24(:,13), A(:,115), n3(:,358), t3x96(:,:,115), nhel, den(217))
    call cont_VV(nsync, wf8(:,28), wf12(:,47), A(:,116), n3(:,359), t3x96(:,:,116), nhel, den(221))
    call cont_QA(nsync, wf12(:,36), wf8(:,29), A(:,117), n3(:,360), t3x96(:,:,117), nhel, den(223))
    call cont_QA(nsync, wf6(:,18), wf16(:,13), A(:,118), n3(:,361), t3x96(:,:,118), nhel, den(225))
    call cont_QA(nsync, wf12(:,38), wf8(:,29), A(:,119), n3(:,362), t3x96(:,:,119), nhel, den(226))
    call cont_QA(nsync, wf6(:,18), wf16(:,14), A(:,120), n3(:,363), t3x96(:,:,120), nhel, den(228))
    call cont_VV(nsync, wf8(:,28), wf12(:,48), A(:,121), n3(:,364), t3x96(:,:,121), nhel, den(229))
    call cont_QA(nsync, wf4(:,16), wf24(:,14), A(:,122), n3(:,365), t3x96(:,:,122), nhel, den(230))
    call cont_QA(nsync, wf12(:,23), wf8(:,32), A(:,123), n3(:,366), t3x96(:,:,123), nhel, den(232))
    call cont_VV(nsync, wf8(:,28), wf12(:,49), A(:,124), n3(:,367), t3x96(:,:,124), nhel, den(234))
    call cont_QA(nsync, wf4(:,25), wf24(:,15), A(:,125), n3(:,368), t3x96(:,:,125), nhel, den(236))
    call cont_QA(nsync, wf12(:,26), wf8(:,32), A(:,126), n3(:,369), t3x96(:,:,126), nhel, den(237))
    call cont_QA(nsync, wf6(:,28), wf16(:,15), A(:,127), n3(:,370), t3x96(:,:,127), nhel, den(239))
    call cont_QA(nsync, wf4(:,25), wf24(:,16), A(:,128), n3(:,371), t3x96(:,:,128), nhel, den(241))
    call cont_QA(nsync, wf6(:,28), wf16(:,16), A(:,129), n3(:,372), t3x96(:,:,129), nhel, den(242))
    call cont_VV(nsync, wf8(:,28), wf12(:,50), A(:,130), n3(:,373), t3x96(:,:,130), nhel, den(243))
    call cont_QA(nsync, wf6(:,22), wf16(:,13), A(:,131), n3(:,374), t3x96(:,:,131), nhel, den(244))
    call cont_QA(nsync, wf4(:,27), wf24(:,15), A(:,132), n3(:,375), t3x96(:,:,132), nhel, den(245))
    call cont_QA(nsync, wf6(:,22), wf16(:,14), A(:,133), n3(:,376), t3x96(:,:,133), nhel, den(246))
    call cont_VV(nsync, wf8(:,28), wf12(:,51), A(:,134), n3(:,377), t3x96(:,:,134), nhel, den(247))
    call cont_QA(nsync, wf12(:,23), wf8(:,35), A(:,135), n3(:,378), t3x96(:,:,135), nhel, den(248))
    call cont_QA(nsync, wf4(:,20), wf24(:,13), A(:,136), n3(:,379), t3x96(:,:,136), nhel, den(249))
    call cont_QA(nsync, wf6(:,30), wf16(:,15), A(:,137), n3(:,380), t3x96(:,:,137), nhel, den(250))
    call cont_QA(nsync, wf12(:,36), wf8(:,36), A(:,138), n3(:,381), t3x96(:,:,138), nhel, den(251))
    call cont_QA(nsync, wf6(:,30), wf16(:,16), A(:,139), n3(:,382), t3x96(:,:,139), nhel, den(252))
    call cont_VV(nsync, wf8(:,28), wf12(:,52), A(:,140), n3(:,383), t3x96(:,:,140), nhel, den(253))
    call cont_QA(nsync, wf12(:,30), wf8(:,31), A(:,141), n3(:,384), t3x96(:,:,141), nhel, den(254))
    call cont_QA(nsync, wf6(:,32), wf16(:,15), A(:,142), n3(:,385), t3x96(:,:,142), nhel, den(255))
    call cont_QA(nsync, wf12(:,44), wf8(:,34), A(:,143), n3(:,386), t3x96(:,:,143), nhel, den(256))
    call cont_QA(nsync, wf6(:,24), wf16(:,13), A(:,144), n3(:,387), t3x96(:,:,144), nhel, den(257))
    call cont_QA(nsync, wf6(:,32), wf16(:,16), A(:,145), n3(:,388), t3x96(:,:,145), nhel, den(258))
    call cont_VV(nsync, wf8(:,28), wf12(:,53), A(:,146), n3(:,389), t3x96(:,:,146), nhel, den(259))
    call cont_VV(nsync, wf8(:,28), wf12(:,54), A(:,147), n3(:,390), t3x96(:,:,147), nhel, den(260))
    call cont_QA(nsync, wf12(:,33), wf8(:,31), A(:,148), n3(:,391), t3x96(:,:,148), nhel, den(261))
    call cont_QA(nsync, wf4(:,20), wf24(:,14), A(:,149), n3(:,392), t3x96(:,:,149), nhel, den(262))
    call cont_QA(nsync, wf12(:,26), wf8(:,35), A(:,150), n3(:,393), t3x96(:,:,150), nhel, den(263))
    call cont_QA(nsync, wf12(:,38), wf8(:,36), A(:,151), n3(:,394), t3x96(:,:,151), nhel, den(264))
    call cont_QA(nsync, wf4(:,27), wf24(:,16), A(:,152), n3(:,395), t3x96(:,:,152), nhel, den(265))
    call cont_VV(nsync, wf4(:,29), wf24(:,17), A(:,153), n3(:,396), t3x96(:,:,153), nhel, den(269))
    call cont_VV(nsync, wf12(:,55), wf8(:,37), A(:,154), n3(:,397), t3x96(:,:,154), nhel, den(273))
    call cont_VV(nsync, wf4(:,30), wf24(:,18), A(:,155), n3(:,398), t3x96(:,:,155), nhel, den(277))
    call cont_VV(nsync, wf12(:,56), wf8(:,38), A(:,156), n3(:,399), t3x96(:,:,156), nhel, den(281))
    call cont_VV(nsync, wf12(:,55), wf8(:,39), A(:,157), n3(:,400), t3x96(:,:,157), nhel, den(282))
    call cont_QA(nsync, wf12(:,9), wf8(:,40), A(:,158), n3(:,401), t3x96(:,:,158), nhel, den(283))
    call cont_VV(nsync, wf4(:,29), wf24(:,19), A(:,159), n3(:,402), t3x96(:,:,159), nhel, den(284))
    call cont_QA(nsync, wf12(:,9), wf8(:,41), A(:,160), n3(:,403), t3x96(:,:,160), nhel, den(285))
    call cont_QA(nsync, wf4(:,16), wf24(:,20), A(:,161), n3(:,404), t3x96(:,:,161), nhel, den(286))
    call cont_QA(nsync, wf4(:,16), wf24(:,21), A(:,162), n3(:,405), t3x96(:,:,162), nhel, den(287))
    call cont_VV(nsync, wf4(:,29), wf24(:,22), A(:,163), n3(:,406), t3x96(:,:,163), nhel, den(289))
    call cont_VV(nsync, wf12(:,47), wf8(:,42), A(:,164), n3(:,407), t3x96(:,:,164), nhel, den(291))
    call cont_VV(nsync, wf8(:,38), wf12(:,57), A(:,165), n3(:,408), t3x96(:,:,165), nhel, den(293))
    call cont_VV(nsync, wf12(:,47), wf8(:,43), A(:,166), n3(:,409), t3x96(:,:,166), nhel, den(295))
    call cont_VV(nsync, wf4(:,29), wf24(:,23), A(:,167), n3(:,410), t3x96(:,:,167), nhel, den(296))
    call cont_QA(nsync, wf12(:,36), wf8(:,44), A(:,168), n3(:,411), t3x96(:,:,168), nhel, den(297))
    call cont_QA(nsync, wf4(:,16), wf24(:,24), A(:,169), n3(:,412), t3x96(:,:,169), nhel, den(298))
    call cont_VV(nsync, wf4(:,30), wf24(:,25), A(:,170), n3(:,413), t3x96(:,:,170), nhel, den(300))
    call cont_VV(nsync, wf12(:,20), wf8(:,45), A(:,171), n3(:,414), t3x96(:,:,171), nhel, den(302))
    call cont_VV(nsync, wf12(:,20), wf8(:,46), A(:,172), n3(:,415), t3x96(:,:,172), nhel, den(304))
    call cont_VV(nsync, wf8(:,37), wf12(:,58), A(:,173), n3(:,416), t3x96(:,:,173), nhel, den(306))
    call cont_QA(nsync, wf12(:,3), wf8(:,47), A(:,174), n3(:,417), t3x96(:,:,174), nhel, den(307))
    call cont_VV(nsync, wf4(:,30), wf24(:,26), A(:,175), n3(:,418), t3x96(:,:,175), nhel, den(308))
    call cont_QA(nsync, wf4(:,16), wf24(:,27), A(:,176), n3(:,419), t3x96(:,:,176), nhel, den(309))
    call cont_QA(nsync, wf12(:,11), wf8(:,40), A(:,177), n3(:,420), t3x96(:,:,177), nhel, den(310))
    call cont_QA(nsync, wf12(:,11), wf8(:,41), A(:,178), n3(:,421), t3x96(:,:,178), nhel, den(311))
    call cont_QA(nsync, wf6(:,18), wf16(:,17), A(:,179), n3(:,422), t3x96(:,:,179), nhel, den(312))
    call cont_QA(nsync, wf6(:,18), wf16(:,18), A(:,180), n3(:,423), t3x96(:,:,180), nhel, den(313))
    call cont_QA(nsync, wf12(:,38), wf8(:,44), A(:,181), n3(:,424), t3x96(:,:,181), nhel, den(314))
    call cont_QA(nsync, wf6(:,18), wf16(:,19), A(:,182), n3(:,425), t3x96(:,:,182), nhel, den(315))
    call cont_QA(nsync, wf12(:,5), wf8(:,47), A(:,183), n3(:,426), t3x96(:,:,183), nhel, den(316))
    call cont_QA(nsync, wf6(:,18), wf16(:,20), A(:,184), n3(:,427), t3x96(:,:,184), nhel, den(317))
    call cont_VV(nsync, wf12(:,59), wf8(:,48), A(:,185), n3(:,428), t3x96(:,:,185), nhel, den(321))
    call cont_VV(nsync, wf4(:,31), wf24(:,28), A(:,186), n3(:,429), t3x96(:,:,186), nhel, den(325))
    call cont_VV(nsync, wf12(:,60), wf8(:,49), A(:,187), n3(:,430), t3x96(:,:,187), nhel, den(329))
    call cont_VV(nsync, wf4(:,32), wf24(:,29), A(:,188), n3(:,431), t3x96(:,:,188), nhel, den(333))
    call cont_VV(nsync, wf4(:,31), wf24(:,30), A(:,189), n3(:,432), t3x96(:,:,189), nhel, den(334))
    call cont_QA(nsync, wf4(:,3), wf24(:,31), A(:,190), n3(:,433), t3x96(:,:,190), nhel, den(335))
    call cont_VV(nsync, wf12(:,59), wf8(:,50), A(:,191), n3(:,434), t3x96(:,:,191), nhel, den(336))
    call cont_QA(nsync, wf4(:,3), wf24(:,32), A(:,192), n3(:,435), t3x96(:,:,192), nhel, den(337))
    call cont_QA(nsync, wf12(:,23), wf8(:,51), A(:,193), n3(:,436), t3x96(:,:,193), nhel, den(338))
    call cont_QA(nsync, wf12(:,23), wf8(:,52), A(:,194), n3(:,437), t3x96(:,:,194), nhel, den(339))
    call cont_VV(nsync, wf12(:,34), wf8(:,53), A(:,195), n3(:,438), t3x96(:,:,195), nhel, den(341))
    call cont_VV(nsync, wf4(:,31), wf24(:,33), A(:,196), n3(:,439), t3x96(:,:,196), nhel, den(343))
    call cont_VV(nsync, wf8(:,49), wf12(:,61), A(:,197), n3(:,440), t3x96(:,:,197), nhel, den(345))
    call cont_VV(nsync, wf12(:,34), wf8(:,54), A(:,198), n3(:,441), t3x96(:,:,198), nhel, den(347))
    call cont_QA(nsync, wf4(:,3), wf24(:,34), A(:,199), n3(:,442), t3x96(:,:,199), nhel, den(348))
    call cont_VV(nsync, wf4(:,31), wf24(:,35), A(:,200), n3(:,443), t3x96(:,:,200), nhel, den(349))
    call cont_QA(nsync, wf12(:,36), wf8(:,55), A(:,201), n3(:,444), t3x96(:,:,201), nhel, den(350))
    call cont_VV(nsync, wf12(:,1), wf8(:,56), A(:,202), n3(:,445), t3x96(:,:,202), nhel, den(352))
    call cont_VV(nsync, wf4(:,32), wf24(:,36), A(:,203), n3(:,446), t3x96(:,:,203), nhel, den(354))
    call cont_VV(nsync, wf12(:,1), wf8(:,57), A(:,204), n3(:,447), t3x96(:,:,204), nhel, den(356))
    call cont_VV(nsync, wf8(:,48), wf12(:,62), A(:,205), n3(:,448), t3x96(:,:,205), nhel, den(358))
    call cont_QA(nsync, wf4(:,3), wf24(:,37), A(:,206), n3(:,449), t3x96(:,:,206), nhel, den(359))
    call cont_QA(nsync, wf12(:,3), wf8(:,58), A(:,207), n3(:,450), t3x96(:,:,207), nhel, den(360))
    call cont_VV(nsync, wf4(:,32), wf24(:,38), A(:,208), n3(:,451), t3x96(:,:,208), nhel, den(361))
    call cont_QA(nsync, wf6(:,4), wf16(:,21), A(:,209), n3(:,452), t3x96(:,:,209), nhel, den(362))
    call cont_QA(nsync, wf6(:,4), wf16(:,22), A(:,210), n3(:,453), t3x96(:,:,210), nhel, den(363))
    call cont_QA(nsync, wf12(:,26), wf8(:,51), A(:,211), n3(:,454), t3x96(:,:,211), nhel, den(364))
    call cont_QA(nsync, wf12(:,26), wf8(:,52), A(:,212), n3(:,455), t3x96(:,:,212), nhel, den(365))
    call cont_QA(nsync, wf6(:,4), wf16(:,23), A(:,213), n3(:,456), t3x96(:,:,213), nhel, den(366))
    call cont_QA(nsync, wf12(:,38), wf8(:,55), A(:,214), n3(:,457), t3x96(:,:,214), nhel, den(367))
    call cont_QA(nsync, wf6(:,4), wf16(:,24), A(:,215), n3(:,458), t3x96(:,:,215), nhel, den(368))
    call cont_QA(nsync, wf12(:,5), wf8(:,58), A(:,216), n3(:,459), t3x96(:,:,216), nhel, den(369))
    call cont_VV(nsync, wf12(:,59), wf8(:,59), A(:,217), n3(:,460), t3x96(:,:,217), nhel, den(371))
    call cont_VV(nsync, wf12(:,49), wf8(:,42), A(:,218), n3(:,461), t3x96(:,:,218), nhel, den(372))
    call cont_VV(nsync, wf24(:,29), wf4(:,33), A(:,219), n3(:,462), t3x96(:,:,219), nhel, den(374))
    call cont_VV(nsync, wf12(:,49), wf8(:,43), A(:,220), n3(:,463), t3x96(:,:,220), nhel, den(375))
    call cont_VV(nsync, wf12(:,59), wf8(:,60), A(:,221), n3(:,464), t3x96(:,:,221), nhel, den(376))
    call cont_QA(nsync, wf4(:,25), wf24(:,39), A(:,222), n3(:,465), t3x96(:,:,222), nhel, den(377))
    call cont_QA(nsync, wf12(:,23), wf8(:,61), A(:,223), n3(:,466), t3x96(:,:,223), nhel, den(378))
    call cont_VV(nsync, wf12(:,40), wf8(:,53), A(:,224), n3(:,467), t3x96(:,:,224), nhel, den(379))
    call cont_VV(nsync, wf12(:,55), wf8(:,62), A(:,225), n3(:,468), t3x96(:,:,225), nhel, den(381))
    call cont_VV(nsync, wf24(:,18), wf4(:,34), A(:,226), n3(:,469), t3x96(:,:,226), nhel, den(383))
    call cont_VV(nsync, wf12(:,40), wf8(:,54), A(:,227), n3(:,470), t3x96(:,:,227), nhel, den(384))
    call cont_QA(nsync, wf12(:,9), wf8(:,63), A(:,228), n3(:,471), t3x96(:,:,228), nhel, den(385))
    call cont_VV(nsync, wf12(:,55), wf8(:,64), A(:,229), n3(:,472), t3x96(:,:,229), nhel, den(386))
    call cont_QA(nsync, wf4(:,25), wf24(:,40), A(:,230), n3(:,473), t3x96(:,:,230), nhel, den(387))
    call cont_VV(nsync, wf24(:,36), wf4(:,33), A(:,231), n3(:,474), t3x96(:,:,231), nhel, den(388))
    call cont_VV(nsync, wf12(:,58), wf8(:,62), A(:,232), n3(:,475), t3x96(:,:,232), nhel, den(389))
    call cont_VV(nsync, wf24(:,25), wf4(:,34), A(:,233), n3(:,476), t3x96(:,:,233), nhel, den(390))
    call cont_VV(nsync, wf12(:,62), wf8(:,59), A(:,234), n3(:,477), t3x96(:,:,234), nhel, den(391))
    call cont_QA(nsync, wf12(:,3), wf8(:,65), A(:,235), n3(:,478), t3x96(:,:,235), nhel, den(392))
    call cont_QA(nsync, wf4(:,25), wf24(:,41), A(:,236), n3(:,479), t3x96(:,:,236), nhel, den(393))
    call cont_QA(nsync, wf12(:,3), wf8(:,66), A(:,237), n3(:,480), t3x96(:,:,237), nhel, den(394))
    call cont_QA(nsync, wf4(:,25), wf24(:,42), A(:,238), n3(:,481), t3x96(:,:,238), nhel, den(395))
    call cont_VV(nsync, wf12(:,58), wf8(:,64), A(:,239), n3(:,482), t3x96(:,:,239), nhel, den(396))
    call cont_VV(nsync, wf24(:,38), wf4(:,33), A(:,240), n3(:,483), t3x96(:,:,240), nhel, den(397))
    call cont_QA(nsync, wf6(:,28), wf16(:,25), A(:,241), n3(:,484), t3x96(:,:,241), nhel, den(398))
    call cont_QA(nsync, wf12(:,26), wf8(:,61), A(:,242), n3(:,485), t3x96(:,:,242), nhel, den(399))
    call cont_QA(nsync, wf12(:,11), wf8(:,63), A(:,243), n3(:,486), t3x96(:,:,243), nhel, den(400))
    call cont_QA(nsync, wf6(:,28), wf16(:,26), A(:,244), n3(:,487), t3x96(:,:,244), nhel, den(401))
    call cont_QA(nsync, wf12(:,5), wf8(:,65), A(:,245), n3(:,488), t3x96(:,:,245), nhel, den(402))
    call cont_QA(nsync, wf6(:,28), wf16(:,27), A(:,246), n3(:,489), t3x96(:,:,246), nhel, den(403))
    call cont_QA(nsync, wf12(:,5), wf8(:,66), A(:,247), n3(:,490), t3x96(:,:,247), nhel, den(404))
    call cont_QA(nsync, wf6(:,28), wf16(:,28), A(:,248), n3(:,491), t3x96(:,:,248), nhel, den(405))
    call cont_VV(nsync, wf12(:,60), wf8(:,67), A(:,249), n3(:,492), t3x96(:,:,249), nhel, den(407))
    call cont_VV(nsync, wf12(:,24), wf8(:,45), A(:,250), n3(:,493), t3x96(:,:,250), nhel, den(408))
    call cont_VV(nsync, wf12(:,24), wf8(:,46), A(:,251), n3(:,494), t3x96(:,:,251), nhel, den(409))
    call cont_VV(nsync, wf24(:,28), wf4(:,35), A(:,252), n3(:,495), t3x96(:,:,252), nhel, den(411))
    call cont_QA(nsync, wf4(:,7), wf24(:,43), A(:,253), n3(:,496), t3x96(:,:,253), nhel, den(412))
    call cont_VV(nsync, wf12(:,60), wf8(:,68), A(:,254), n3(:,497), t3x96(:,:,254), nhel, den(413))
    call cont_QA(nsync, wf12(:,23), wf8(:,69), A(:,255), n3(:,498), t3x96(:,:,255), nhel, den(414))
    call cont_VV(nsync, wf12(:,7), wf8(:,56), A(:,256), n3(:,499), t3x96(:,:,256), nhel, den(415))
    call cont_VV(nsync, wf12(:,56), wf8(:,70), A(:,257), n3(:,500), t3x96(:,:,257), nhel, den(417))
    call cont_VV(nsync, wf12(:,7), wf8(:,57), A(:,258), n3(:,501), t3x96(:,:,258), nhel, den(418))
    call cont_VV(nsync, wf24(:,17), wf4(:,36), A(:,259), n3(:,502), t3x96(:,:,259), nhel, den(420))
    call cont_QA(nsync, wf12(:,9), wf8(:,71), A(:,260), n3(:,503), t3x96(:,:,260), nhel, den(421))
    call cont_QA(nsync, wf4(:,7), wf24(:,44), A(:,261), n3(:,504), t3x96(:,:,261), nhel, den(422))
    call cont_VV(nsync, wf12(:,56), wf8(:,72), A(:,262), n3(:,505), t3x96(:,:,262), nhel, den(423))
    call cont_VV(nsync, wf12(:,57), wf8(:,70), A(:,263), n3(:,506), t3x96(:,:,263), nhel, den(424))
    call cont_VV(nsync, wf24(:,33), wf4(:,35), A(:,264), n3(:,507), t3x96(:,:,264), nhel, den(425))
    call cont_VV(nsync, wf12(:,61), wf8(:,67), A(:,265), n3(:,508), t3x96(:,:,265), nhel, den(426))
    call cont_VV(nsync, wf24(:,22), wf4(:,36), A(:,266), n3(:,509), t3x96(:,:,266), nhel, den(427))
    call cont_QA(nsync, wf4(:,7), wf24(:,45), A(:,267), n3(:,510), t3x96(:,:,267), nhel, den(428))
    call cont_QA(nsync, wf12(:,36), wf8(:,73), A(:,268), n3(:,511), t3x96(:,:,268), nhel, den(429))
    call cont_QA(nsync, wf4(:,7), wf24(:,46), A(:,269), n3(:,512), t3x96(:,:,269), nhel, den(430))
    call cont_QA(nsync, wf12(:,36), wf8(:,74), A(:,270), n3(:,513), t3x96(:,:,270), nhel, den(431))
    call cont_VV(nsync, wf24(:,35), wf4(:,35), A(:,271), n3(:,514), t3x96(:,:,271), nhel, den(432))
    call cont_VV(nsync, wf12(:,57), wf8(:,72), A(:,272), n3(:,515), t3x96(:,:,272), nhel, den(433))
    call cont_QA(nsync, wf6(:,8), wf16(:,29), A(:,273), n3(:,516), t3x96(:,:,273), nhel, den(434))
    call cont_QA(nsync, wf12(:,26), wf8(:,69), A(:,274), n3(:,517), t3x96(:,:,274), nhel, den(435))
    call cont_QA(nsync, wf12(:,11), wf8(:,71), A(:,275), n3(:,518), t3x96(:,:,275), nhel, den(436))
    call cont_QA(nsync, wf6(:,8), wf16(:,30), A(:,276), n3(:,519), t3x96(:,:,276), nhel, den(437))
    call cont_QA(nsync, wf6(:,8), wf16(:,31), A(:,277), n3(:,520), t3x96(:,:,277), nhel, den(438))
    call cont_QA(nsync, wf12(:,38), wf8(:,73), A(:,278), n3(:,521), t3x96(:,:,278), nhel, den(439))
    call cont_QA(nsync, wf6(:,8), wf16(:,32), A(:,279), n3(:,522), t3x96(:,:,279), nhel, den(440))
    call cont_QA(nsync, wf12(:,38), wf8(:,74), A(:,280), n3(:,523), t3x96(:,:,280), nhel, den(441))
    call cont_QA(nsync, wf4(:,13), wf24(:,31), A(:,281), n3(:,524), t3x96(:,:,281), nhel, den(442))
    call cont_QA(nsync, wf4(:,13), wf24(:,32), A(:,282), n3(:,525), t3x96(:,:,282), nhel, den(443))
    call cont_QA(nsync, wf6(:,22), wf16(:,17), A(:,283), n3(:,526), t3x96(:,:,283), nhel, den(444))
    call cont_QA(nsync, wf6(:,22), wf16(:,18), A(:,284), n3(:,527), t3x96(:,:,284), nhel, den(445))
    call cont_QA(nsync, wf4(:,27), wf24(:,39), A(:,285), n3(:,528), t3x96(:,:,285), nhel, den(446))
    call cont_QA(nsync, wf6(:,22), wf16(:,19), A(:,286), n3(:,529), t3x96(:,:,286), nhel, den(447))
    call cont_QA(nsync, wf4(:,11), wf24(:,43), A(:,287), n3(:,530), t3x96(:,:,287), nhel, den(448))
    call cont_QA(nsync, wf6(:,22), wf16(:,20), A(:,288), n3(:,531), t3x96(:,:,288), nhel, den(449))
    call cont_QA(nsync, wf6(:,10), wf16(:,21), A(:,289), n3(:,532), t3x96(:,:,289), nhel, den(450))
    call cont_QA(nsync, wf6(:,10), wf16(:,22), A(:,290), n3(:,533), t3x96(:,:,290), nhel, den(451))
    call cont_QA(nsync, wf4(:,20), wf24(:,20), A(:,291), n3(:,534), t3x96(:,:,291), nhel, den(452))
    call cont_QA(nsync, wf4(:,20), wf24(:,21), A(:,292), n3(:,535), t3x96(:,:,292), nhel, den(453))
    call cont_QA(nsync, wf6(:,10), wf16(:,23), A(:,293), n3(:,536), t3x96(:,:,293), nhel, den(454))
    call cont_QA(nsync, wf4(:,27), wf24(:,40), A(:,294), n3(:,537), t3x96(:,:,294), nhel, den(455))
    call cont_QA(nsync, wf6(:,10), wf16(:,24), A(:,295), n3(:,538), t3x96(:,:,295), nhel, den(456))
    call cont_QA(nsync, wf4(:,11), wf24(:,44), A(:,296), n3(:,539), t3x96(:,:,296), nhel, den(457))
    call cont_QA(nsync, wf6(:,30), wf16(:,25), A(:,297), n3(:,540), t3x96(:,:,297), nhel, den(458))
    call cont_QA(nsync, wf4(:,20), wf24(:,24), A(:,298), n3(:,541), t3x96(:,:,298), nhel, den(459))
    call cont_QA(nsync, wf4(:,13), wf24(:,34), A(:,299), n3(:,542), t3x96(:,:,299), nhel, den(460))
    call cont_QA(nsync, wf6(:,30), wf16(:,26), A(:,300), n3(:,543), t3x96(:,:,300), nhel, den(461))
    call cont_QA(nsync, wf4(:,11), wf24(:,45), A(:,301), n3(:,544), t3x96(:,:,301), nhel, den(462))
    call cont_QA(nsync, wf6(:,30), wf16(:,27), A(:,302), n3(:,545), t3x96(:,:,302), nhel, den(463))
    call cont_QA(nsync, wf4(:,11), wf24(:,46), A(:,303), n3(:,546), t3x96(:,:,303), nhel, den(464))
    call cont_QA(nsync, wf6(:,30), wf16(:,28), A(:,304), n3(:,547), t3x96(:,:,304), nhel, den(465))
    call cont_QA(nsync, wf6(:,12), wf16(:,29), A(:,305), n3(:,548), t3x96(:,:,305), nhel, den(466))
    call cont_QA(nsync, wf4(:,20), wf24(:,27), A(:,306), n3(:,549), t3x96(:,:,306), nhel, den(467))
    call cont_QA(nsync, wf4(:,13), wf24(:,37), A(:,307), n3(:,550), t3x96(:,:,307), nhel, den(468))
    call cont_QA(nsync, wf6(:,12), wf16(:,30), A(:,308), n3(:,551), t3x96(:,:,308), nhel, den(469))
    call cont_QA(nsync, wf6(:,12), wf16(:,31), A(:,309), n3(:,552), t3x96(:,:,309), nhel, den(470))
    call cont_QA(nsync, wf4(:,27), wf24(:,41), A(:,310), n3(:,553), t3x96(:,:,310), nhel, den(471))
    call cont_QA(nsync, wf6(:,12), wf16(:,32), A(:,311), n3(:,554), t3x96(:,:,311), nhel, den(472))
    call cont_QA(nsync, wf4(:,27), wf24(:,42), A(:,312), n3(:,555), t3x96(:,:,312), nhel, den(473))
    call cont_QA(nsync, wf6(:,14), wf16(:,29), A(:,313), n3(:,556), t3x96(:,:,313), nhel, den(474))
    call cont_VV(nsync, wf12(:,18), wf8(:,56), A(:,314), n3(:,557), t3x96(:,:,314), nhel, den(475))
    call cont_VV(nsync, wf12(:,46), wf8(:,53), A(:,315), n3(:,558), t3x96(:,:,315), nhel, den(476))
    call cont_VV(nsync, wf12(:,45), wf8(:,53), A(:,316), n3(:,559), t3x96(:,:,316), nhel, den(477))
    call cont_QA(nsync, wf6(:,14), wf16(:,30), A(:,317), n3(:,560), t3x96(:,:,317), nhel, den(478))
    call cont_VV(nsync, wf12(:,54), wf8(:,42), A(:,318), n3(:,561), t3x96(:,:,318), nhel, den(479))
    call cont_VV(nsync, wf12(:,53), wf8(:,42), A(:,319), n3(:,562), t3x96(:,:,319), nhel, den(480))
    call cont_VV(nsync, wf12(:,31), wf8(:,45), A(:,320), n3(:,563), t3x96(:,:,320), nhel, den(481))
    call cont_QA(nsync, wf6(:,14), wf16(:,31), A(:,321), n3(:,564), t3x96(:,:,321), nhel, den(482))
    call cont_QA(nsync, wf6(:,14), wf16(:,32), A(:,322), n3(:,565), t3x96(:,:,322), nhel, den(483))
    call cont_QA(nsync, wf6(:,16), wf16(:,23), A(:,323), n3(:,566), t3x96(:,:,323), nhel, den(484))
    call cont_VV(nsync, wf12(:,31), wf8(:,46), A(:,324), n3(:,567), t3x96(:,:,324), nhel, den(485))
    call cont_QA(nsync, wf6(:,32), wf16(:,28), A(:,325), n3(:,568), t3x96(:,:,325), nhel, den(486))
    call cont_QA(nsync, wf6(:,32), wf16(:,27), A(:,326), n3(:,569), t3x96(:,:,326), nhel, den(487))
    call cont_QA(nsync, wf6(:,16), wf16(:,24), A(:,327), n3(:,570), t3x96(:,:,327), nhel, den(488))
    call cont_VV(nsync, wf12(:,53), wf8(:,43), A(:,328), n3(:,571), t3x96(:,:,328), nhel, den(489))

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
  complex(REALKIND), intent(out) :: M1(4) ! M1helarray(4,96)
  integer :: empty(0)

  M1(1) = ((-A(j,141)%j-A(j,142)%j-A(j,143)%j-A(j,144)%j-A(j,145)%j-A(j,148)%j-A(j,313)%j-A(j,314)%j-A(j,321)%j &
       -A(j,323)%j)*f(1))/6._/**/REALKIND+((-A(j,65)%j-A(j,66)%j-A(j,72)%j-A(j,105)%j-A(j,106)%j-A(j,107)%j-A(j,315)%j-A(j,316)%j &
       -A(j,322)%j-A(j,324)%j)*f(1))/2._/**/REALKIND+(CI*(-A(j,70)%j-A(j,71)%j+A(j,109)%j+A(j,110)%j)*f(2))/2._/**/REALKIND+(( &
       -A(j,39)%j-A(j,44)%j-A(j,46)%j-A(j,47)%j-A(j,50)%j-A(j,51)%j-A(j,57)%j-A(j,59)%j-A(j,60)%j-A(j,61)%j-A(j,73)%j-A(j,74)%j &
       -A(j,79)%j-A(j,80)%j-A(j,81)%j-A(j,87)%j-A(j,90)%j-A(j,91)%j-A(j,93)%j-A(j,94)%j-A(j,100)%j-A(j,101)%j-A(j,113)%j &
       -A(j,114)%j-A(j,153)%j-A(j,161)%j-A(j,168)%j-A(j,169)%j-A(j,172)%j-A(j,181)%j-A(j,182)%j-A(j,185)%j-A(j,193)%j-A(j,195)%j &
       -A(j,203)%j-A(j,205)%j-A(j,210)%j-A(j,211)%j-A(j,222)%j-A(j,223)%j-A(j,224)%j-A(j,231)%j-A(j,236)%j-A(j,241)%j-A(j,242)%j &
       -A(j,251)%j-A(j,257)%j-A(j,259)%j-A(j,263)%j-A(j,268)%j-A(j,278)%j-A(j,279)%j-A(j,285)%j-A(j,286)%j-A(j,290)%j-A(j,291)%j &
       -A(j,297)%j-A(j,298)%j-A(j,310)%j-A(j,311)%j)*f(3))/2._/**/REALKIND+((-A(j,115)%j-A(j,117)%j-A(j,118)%j-A(j,119)%j &
       -A(j,120)%j-A(j,122)%j-A(j,123)%j-A(j,125)%j-A(j,126)%j-A(j,127)%j-A(j,128)%j-A(j,129)%j-A(j,131)%j-A(j,132)%j-A(j,133)%j &
       -A(j,135)%j-A(j,136)%j-A(j,137)%j-A(j,138)%j-A(j,139)%j-A(j,149)%j-A(j,150)%j-A(j,151)%j-A(j,152)%j-A(j,154)%j-A(j,162)%j &
       -A(j,173)%j-A(j,176)%j-A(j,186)%j-A(j,194)%j-A(j,196)%j-A(j,201)%j-A(j,202)%j-A(j,204)%j-A(j,209)%j-A(j,212)%j-A(j,213)%j &
       -A(j,214)%j-A(j,225)%j-A(j,230)%j-A(j,232)%j-A(j,238)%j-A(j,252)%j-A(j,255)%j-A(j,256)%j-A(j,258)%j-A(j,264)%j-A(j,270)%j &
       -A(j,273)%j-A(j,274)%j-A(j,277)%j-A(j,280)%j-A(j,289)%j-A(j,292)%j-A(j,293)%j-A(j,294)%j-A(j,305)%j-A(j,306)%j-A(j,309)%j &
       -A(j,312)%j)*f(3))/6._/**/REALKIND+(CI*(-A(j,40)%j-A(j,45)%j-A(j,48)%j-A(j,54)%j-A(j,58)%j-A(j,64)%j+A(j,77)%j+A(j,84)%j &
       +A(j,85)%j+A(j,92)%j+A(j,97)%j+A(j,102)%j-A(j,159)%j-A(j,167)%j-A(j,191)%j+A(j,208)%j-A(j,221)%j+A(j,240)%j+A(j,262)%j &
       +A(j,272)%j)*f(4))/2._/**/REALKIND
  M1(2) = ((A(j,27)%j+A(j,28)%j+A(j,32)%j+A(j,143)%j+A(j,144)%j+A(j,145)%j+A(j,318)%j+A(j,319)%j+A(j,321)%j &
       +A(j,323)%j)*f(1))/2._/**/REALKIND+((A(j,103)%j+A(j,104)%j+A(j,105)%j+A(j,106)%j+A(j,107)%j+A(j,108)%j+A(j,317)%j &
       +A(j,320)%j+A(j,322)%j+A(j,324)%j)*f(1))/6._/**/REALKIND+(CI*(A(j,33)%j+A(j,34)%j-A(j,146)%j &
       -A(j,147)%j)*f(2))/2._/**/REALKIND+((A(j,78)%j+A(j,79)%j+A(j,80)%j+A(j,81)%j+A(j,82)%j+A(j,83)%j+A(j,86)%j+A(j,87)%j &
       +A(j,88)%j+A(j,89)%j+A(j,90)%j+A(j,91)%j+A(j,93)%j+A(j,94)%j+A(j,95)%j+A(j,96)%j+A(j,98)%j+A(j,99)%j+A(j,100)%j+A(j,101)%j &
       +A(j,111)%j+A(j,112)%j+A(j,113)%j+A(j,114)%j+A(j,153)%j+A(j,160)%j+A(j,163)%j+A(j,168)%j+A(j,171)%j+A(j,172)%j+A(j,178)%j &
       +A(j,179)%j+A(j,181)%j+A(j,182)%j+A(j,185)%j+A(j,192)%j+A(j,205)%j+A(j,206)%j+A(j,217)%j+A(j,222)%j+A(j,234)%j+A(j,236)%j &
       +A(j,250)%j+A(j,251)%j+A(j,259)%j+A(j,260)%j+A(j,266)%j+A(j,268)%j+A(j,275)%j+A(j,276)%j+A(j,278)%j+A(j,279)%j+A(j,282)%j &
       +A(j,283)%j+A(j,285)%j+A(j,286)%j+A(j,307)%j+A(j,308)%j+A(j,310)%j+A(j,311)%j)*f(3))/6._/**/REALKIND+((A(j,2)%j+A(j,6)%j &
       +A(j,7)%j+A(j,10)%j+A(j,12)%j+A(j,13)%j+A(j,19)%j+A(j,20)%j+A(j,22)%j+A(j,23)%j+A(j,35)%j+A(j,36)%j+A(j,117)%j+A(j,118)%j &
       +A(j,119)%j+A(j,125)%j+A(j,128)%j+A(j,129)%j+A(j,131)%j+A(j,132)%j+A(j,138)%j+A(j,139)%j+A(j,151)%j+A(j,152)%j+A(j,154)%j &
       +A(j,158)%j+A(j,164)%j+A(j,170)%j+A(j,173)%j+A(j,177)%j+A(j,180)%j+A(j,186)%j+A(j,190)%j+A(j,199)%j+A(j,201)%j+A(j,204)%j &
       +A(j,213)%j+A(j,214)%j+A(j,218)%j+A(j,228)%j+A(j,230)%j+A(j,233)%j+A(j,238)%j+A(j,243)%j+A(j,244)%j+A(j,249)%j+A(j,252)%j &
       +A(j,258)%j+A(j,265)%j+A(j,270)%j+A(j,277)%j+A(j,280)%j+A(j,281)%j+A(j,284)%j+A(j,293)%j+A(j,294)%j+A(j,299)%j+A(j,300)%j &
       +A(j,309)%j+A(j,312)%j)*f(3))/2._/**/REALKIND+(CI*(A(j,1)%j+A(j,8)%j+A(j,9)%j+A(j,16)%j+A(j,21)%j+A(j,26)%j-A(j,116)%j &
       -A(j,121)%j-A(j,124)%j-A(j,130)%j-A(j,134)%j-A(j,140)%j+A(j,157)%j-A(j,175)%j+A(j,189)%j+A(j,200)%j+A(j,229)%j+A(j,239)%j &
       -A(j,254)%j+A(j,271)%j)*f(4))/2._/**/REALKIND
  M1(3) = ((A(j,65)%j+A(j,66)%j+A(j,67)%j+A(j,68)%j+A(j,69)%j+A(j,72)%j+A(j,315)%j+A(j,316)%j+A(j,326)%j &
       +A(j,327)%j)*f(1))/6._/**/REALKIND+((A(j,29)%j+A(j,30)%j+A(j,31)%j+A(j,141)%j+A(j,142)%j+A(j,148)%j+A(j,313)%j+A(j,314)%j &
       +A(j,325)%j+A(j,328)%j)*f(1))/2._/**/REALKIND+(CI*(-A(j,33)%j-A(j,34)%j+A(j,146)%j+A(j,147)%j)*f(2))/2._/**/REALKIND &
       +((A(j,39)%j+A(j,41)%j+A(j,42)%j+A(j,43)%j+A(j,44)%j+A(j,46)%j+A(j,47)%j+A(j,49)%j+A(j,50)%j+A(j,51)%j+A(j,52)%j+A(j,53)%j &
       +A(j,55)%j+A(j,56)%j+A(j,57)%j+A(j,59)%j+A(j,60)%j+A(j,61)%j+A(j,62)%j+A(j,63)%j+A(j,73)%j+A(j,74)%j+A(j,75)%j+A(j,76)%j &
       +A(j,156)%j+A(j,161)%j+A(j,165)%j+A(j,169)%j+A(j,188)%j+A(j,193)%j+A(j,195)%j+A(j,198)%j+A(j,203)%j+A(j,207)%j+A(j,210)%j &
       +A(j,211)%j+A(j,215)%j+A(j,216)%j+A(j,219)%j+A(j,223)%j+A(j,224)%j+A(j,227)%j+A(j,231)%j+A(j,237)%j+A(j,241)%j+A(j,242)%j &
       +A(j,246)%j+A(j,247)%j+A(j,257)%j+A(j,261)%j+A(j,263)%j+A(j,269)%j+A(j,290)%j+A(j,291)%j+A(j,295)%j+A(j,296)%j+A(j,297)%j &
       +A(j,298)%j+A(j,302)%j+A(j,303)%j)*f(3))/6._/**/REALKIND+((A(j,3)%j+A(j,4)%j+A(j,5)%j+A(j,11)%j+A(j,14)%j+A(j,15)%j &
       +A(j,17)%j+A(j,18)%j+A(j,24)%j+A(j,25)%j+A(j,37)%j+A(j,38)%j+A(j,115)%j+A(j,120)%j+A(j,122)%j+A(j,123)%j+A(j,126)%j &
       +A(j,127)%j+A(j,133)%j+A(j,135)%j+A(j,136)%j+A(j,137)%j+A(j,149)%j+A(j,150)%j+A(j,155)%j+A(j,162)%j+A(j,166)%j+A(j,174)%j &
       +A(j,176)%j+A(j,183)%j+A(j,184)%j+A(j,187)%j+A(j,194)%j+A(j,196)%j+A(j,197)%j+A(j,202)%j+A(j,209)%j+A(j,212)%j+A(j,220)%j &
       +A(j,225)%j+A(j,226)%j+A(j,232)%j+A(j,235)%j+A(j,245)%j+A(j,248)%j+A(j,253)%j+A(j,255)%j+A(j,256)%j+A(j,264)%j+A(j,267)%j &
       +A(j,273)%j+A(j,274)%j+A(j,287)%j+A(j,288)%j+A(j,289)%j+A(j,292)%j+A(j,301)%j+A(j,304)%j+A(j,305)%j &
       +A(j,306)%j)*f(3))/2._/**/REALKIND+(CI*(-A(j,1)%j-A(j,8)%j-A(j,9)%j-A(j,16)%j-A(j,21)%j-A(j,26)%j+A(j,116)%j+A(j,121)%j &
       +A(j,124)%j+A(j,130)%j+A(j,134)%j+A(j,140)%j-A(j,157)%j+A(j,175)%j-A(j,189)%j-A(j,200)%j-A(j,229)%j-A(j,239)%j+A(j,254)%j &
       -A(j,271)%j)*f(4))/2._/**/REALKIND
  M1(4) = ((-A(j,67)%j-A(j,68)%j-A(j,69)%j-A(j,103)%j-A(j,104)%j-A(j,108)%j-A(j,317)%j-A(j,320)%j-A(j,326)%j &
       -A(j,327)%j)*f(1))/2._/**/REALKIND+((-A(j,27)%j-A(j,28)%j-A(j,29)%j-A(j,30)%j-A(j,31)%j-A(j,32)%j-A(j,318)%j-A(j,319)%j &
       -A(j,325)%j-A(j,328)%j)*f(1))/6._/**/REALKIND+(CI*(A(j,70)%j+A(j,71)%j-A(j,109)%j-A(j,110)%j)*f(2))/2._/**/REALKIND+(( &
       -A(j,2)%j-A(j,3)%j-A(j,4)%j-A(j,5)%j-A(j,6)%j-A(j,7)%j-A(j,10)%j-A(j,11)%j-A(j,12)%j-A(j,13)%j-A(j,14)%j-A(j,15)%j &
       -A(j,17)%j-A(j,18)%j-A(j,19)%j-A(j,20)%j-A(j,22)%j-A(j,23)%j-A(j,24)%j-A(j,25)%j-A(j,35)%j-A(j,36)%j-A(j,37)%j-A(j,38)%j &
       -A(j,155)%j-A(j,158)%j-A(j,164)%j-A(j,166)%j-A(j,170)%j-A(j,174)%j-A(j,177)%j-A(j,180)%j-A(j,183)%j-A(j,184)%j-A(j,187)%j &
       -A(j,190)%j-A(j,197)%j-A(j,199)%j-A(j,218)%j-A(j,220)%j-A(j,226)%j-A(j,228)%j-A(j,233)%j-A(j,235)%j-A(j,243)%j-A(j,244)%j &
       -A(j,245)%j-A(j,248)%j-A(j,249)%j-A(j,253)%j-A(j,265)%j-A(j,267)%j-A(j,281)%j-A(j,284)%j-A(j,287)%j-A(j,288)%j-A(j,299)%j &
       -A(j,300)%j-A(j,301)%j-A(j,304)%j)*f(3))/6._/**/REALKIND+((-A(j,41)%j-A(j,42)%j-A(j,43)%j-A(j,49)%j-A(j,52)%j-A(j,53)%j &
       -A(j,55)%j-A(j,56)%j-A(j,62)%j-A(j,63)%j-A(j,75)%j-A(j,76)%j-A(j,78)%j-A(j,82)%j-A(j,83)%j-A(j,86)%j-A(j,88)%j-A(j,89)%j &
       -A(j,95)%j-A(j,96)%j-A(j,98)%j-A(j,99)%j-A(j,111)%j-A(j,112)%j-A(j,156)%j-A(j,160)%j-A(j,163)%j-A(j,165)%j-A(j,171)%j &
       -A(j,178)%j-A(j,179)%j-A(j,188)%j-A(j,192)%j-A(j,198)%j-A(j,206)%j-A(j,207)%j-A(j,215)%j-A(j,216)%j-A(j,217)%j-A(j,219)%j &
       -A(j,227)%j-A(j,234)%j-A(j,237)%j-A(j,246)%j-A(j,247)%j-A(j,250)%j-A(j,260)%j-A(j,261)%j-A(j,266)%j-A(j,269)%j-A(j,275)%j &
       -A(j,276)%j-A(j,282)%j-A(j,283)%j-A(j,295)%j-A(j,296)%j-A(j,302)%j-A(j,303)%j-A(j,307)%j-A(j,308)%j)*f(3))/2._/**/REALKIND &
       +(CI*(A(j,40)%j+A(j,45)%j+A(j,48)%j+A(j,54)%j+A(j,58)%j+A(j,64)%j-A(j,77)%j-A(j,84)%j-A(j,85)%j-A(j,92)%j-A(j,97)%j &
       -A(j,102)%j+A(j,159)%j+A(j,167)%j+A(j,191)%j-A(j,208)%j+A(j,221)%j-A(j,240)%j-A(j,262)%j-A(j,272)%j)*f(4))/2._/**/REALKIND

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
  use ol_colourmatrix_pphzjj_bbbxbxhzg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(4)
  real(REALKIND),    intent(out) :: M2colint(0:30-1)
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
  use ol_colourmatrix_pphzjj_bbbxbxhzg_1_/**/REALKIND, only: K1
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
  & bind(c,name="ol_tree_colvect_pphzjj_bbbxbxhzg_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(4,96)
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
    & bind(c,name="ol_f_amp2tree_pphzjj_bbbxbxhzg_1")
#else
subroutine amp2tree(P, M2)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix element without fuss.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,7)
  real(REALKIND),  intent(out) :: M2
  real(REALKIND) :: M2tmp(0:30-1)
  real(REALKIND) :: M2munu(4,4)
  call amp2(P, M2tmp, 0, [ 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND ], 1, [ 0 ], M2munu)
  M2 = M2tmp(0)
end subroutine amp2tree


#ifdef PRECISION_dp
subroutine amp2ccone(P, M2, I, J) &
    & bind(c,name="ol_f_amp2ccone_pphzjj_bbbxbxhzg_1")
#else
subroutine amp2ccone(P, M2, I, J)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix for the colour correlation matrix for particles I and J.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,7)
  real(REALKIND),  intent(out) :: M2
  integer,         intent(in)  :: I, J
  integer        :: extcomb
  real(REALKIND) :: M2tmp(0:30-1)
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
    & bind(c,name="ol_f_amp2ccall_pphzjj_bbbxbxhzg_1")
#else
subroutine amp2ccall(P, M2)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix for all colour correlation matrices.
  ! The correlation between particles i and j is at position i*(i-1)/2+j of the array M2.
  ! M2(0) is AMP2tree
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,7)
  real(REALKIND),  intent(out) :: M2(0:30-1)
  real(REALKIND) :: M2munu(4,4)
  integer :: k
  call amp2(P, M2, 0, [ 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND ], &
    30, [ (k, k = 0, 30-1) ], M2munu)
end subroutine amp2ccall


#ifdef PRECISION_dp
subroutine amp2hcone(P, M2, I, J, MOM) &
    & bind(c,name="ol_f_amp2hcone_pphzjj_bbbxbxhzg_1")
#else
subroutine amp2hcone(P, M2, I, J, MOM)
#endif
  ! This is an interface for AMP2.
  ! Calculates the helicity correlation for emitter I with momentum MOM and spectator J
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,7)
  real(REALKIND),  intent(out) :: M2
  integer,         intent(in)  :: I, J
  real(REALKIND),  intent(in)  :: MOM(0:3)
  integer        :: extcomb
  real(REALKIND) :: M2tmp(0:30-1)
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
    & bind(c,name="ol_f_amp2hcall_pphzjj_bbbxbxhzg_1")
#else
subroutine amp2hcall(P, M2, I, MOM)
#endif
  ! This is an interface for AMP2.
  ! Calculates all helicity correlations for emitter I with momentum MOM.
  ! The correlator for spectator j is at position j of the array M2.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,7)
  real(REALKIND),  intent(out) :: M2(7)
  integer,         intent(in)  :: I
  real(REALKIND),  intent(in)  :: MOM(0:3)
  real(REALKIND) :: M2tmp(0:30-1)
  real(REALKIND) :: M2munu(4,4)
  integer        :: J, extcombs(7)
  do J = 1, 7
    if (J <= I) then
      extcombs(J) = I*(I-1)/2 + J
    else
      extcombs(J) = J*(J-1)/2 + I
    end if
  end do
  call amp2(P, M2tmp, I, MOM, 7,extcombs, M2munu)
  do J = 1, 7
    M2(J) = M2tmp(extcombs(J))
  end do
end subroutine amp2hcall


#ifdef PRECISION_dp

subroutine amp2tree_c(p, m2) &
    & bind(c,name="ol_amp2tree_pphzjj_bbbxbxhzg_1")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,7)
  real(c_double), intent(out) :: m2
  real(DREALKIND) :: f_p(0:3,7)
  real(DREALKIND) :: f_m2
  f_p = p
  call amp2tree(f_p, f_m2)
  m2 = f_m2
end subroutine amp2tree_c

subroutine amp2ccone_c(p, m2, i, j) &
    & bind(c,name="ol_amp2ccone_pphzjj_bbbxbxhzg_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,7)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(DREALKIND) :: f_p(0:3,7)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  f_p = p
  f_i = i
  f_j = j
  call amp2ccone(f_p, f_m2, f_i, f_j)
  m2 = f_m2
end subroutine amp2ccone_c

subroutine amp2ccall_c(p, m2) &
    & bind(c,name="ol_amp2ccall_pphzjj_bbbxbxhzg_1")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,7)
  real(c_double), intent(out) :: m2(0:30-1)
  real(DREALKIND) :: f_p(0:3,7)
  real(DREALKIND) :: f_m2(0:30-1)
  f_p = p
  call amp2ccall(f_p, f_m2)
  m2 = f_m2
end subroutine amp2ccall_c

subroutine amp2hcone_c(p, m2, i, j, mom) &
    & bind(c,name="ol_amp2hcone_pphzjj_bbbxbxhzg_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,7)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,7)
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
    & bind(c,name="ol_amp2hcall_pphzjj_bbbxbxhzg_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,7)
  real(c_double), intent(out) :: m2(7)
  integer(c_int), intent(in) :: i
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,7)
  real(DREALKIND) :: f_m2(7)
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
    & bind(c,name="amp2tree_pphzjj_bbbxbxhzg_1_")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,7)
  real(c_double), intent(out) :: m2
  real(DREALKIND) :: f_p(0:3,7)
  real(DREALKIND) :: f_m2
  f_p = p
  call amp2tree(f_p, f_m2)
  m2 = f_m2
end subroutine amp2tree_legacy

subroutine amp2ccone_legacy(p, m2, i, j) &
    & bind(c,name="amp2ccone_pphzjj_bbbxbxhzg_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,7)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(DREALKIND) :: f_p(0:3,7)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  f_p = p
  f_i = i
  f_j = j
  call amp2ccone(f_p, f_m2, f_i, f_j)
  m2 = f_m2
end subroutine amp2ccone_legacy

subroutine amp2ccall_legacy(p, m2) &
    & bind(c,name="amp2ccall_pphzjj_bbbxbxhzg_1_")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,7)
  real(c_double), intent(out) :: m2(0:30-1)
  real(DREALKIND) :: f_p(0:3,7)
  real(DREALKIND) :: f_m2(0:30-1)
  f_p = p
  call amp2ccall(f_p, f_m2)
  m2 = f_m2
end subroutine amp2ccall_legacy

subroutine amp2hcone_legacy(p, m2, i, j, mom) &
    & bind(c,name="amp2hcone_pphzjj_bbbxbxhzg_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,7)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,7)
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
    & bind(c,name="amp2hcall_pphzjj_bbbxbxhzg_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,7)
  real(c_double), intent(out) :: m2(7)
  integer(c_int), intent(in) :: i
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,7)
  real(DREALKIND) :: f_m2(7)
  integer :: f_i
  real(DREALKIND) :: f_mom(0:3)
  f_p = p
  f_i = i
  f_mom = mom
  call amp2hcall(f_p, f_m2, f_i, f_mom)
  m2 = f_m2
end subroutine amp2hcall_legacy

#endif

end module ol_tree_pphzjj_bbbxbxhzg_1_/**/REALKIND
