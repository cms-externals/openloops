
module ol_colourmatrix_ppvvvj_bbbxbxzzz_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(60,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [   9,   3]
  K1( 2,:) = [   3,   9]
  K1( 3,:) = [  12,   4]
  K1( 4,:) = [   4,  12]
  K1( 5,:) = [   0,   4]
  K1( 6,:) = [   4,   0]
  K1( 7,:) = [  12,   4]
  K1( 8,:) = [   4,  12]
  K1( 9,:) = [   0,  -4]
  K1(10,:) = [  -4, -12]
  K1(11,:) = [ -12,  -4]
  K1(12,:) = [  -4,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [ -12,  -4]
  K1(16,:) = [  -4,   0]
  K1(17,:) = [   0,  -4]
  K1(18,:) = [  -4, -12]
  K1(19,:) = [   0,   4]
  K1(20,:) = [   4,   0]
  K1(21,:) = [  12,   4]
  K1(22,:) = [   4,  12]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [   0,   0]
  K1(28,:) = [   0,   0]
  K1(29,:) = [   0,   0]
  K1(30,:) = [   0,   0]
  K1(31,:) = [   0,   0]
  K1(32,:) = [   0,   0]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [   0,   0]
  K1(38,:) = [   0,   0]
  K1(39,:) = [   0,   0]
  K1(40,:) = [   0,   0]
  K1(41,:) = [   0,   0]
  K1(42,:) = [   0,   0]
  K1(43,:) = [   0,   0]
  K1(44,:) = [   0,   0]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]
  K1(47,:) = [   0,   0]
  K1(48,:) = [   0,   0]
  K1(49,:) = [   0,   0]
  K1(50,:) = [   0,   0]
  K1(51,:) = [   0,   0]
  K1(52,:) = [   0,   0]
  K1(53,:) = [   0,   0]
  K1(54,:) = [   0,   0]
  K1(55,:) = [   0,   0]
  K1(56,:) = [   0,   0]
  K1(57,:) = [   0,   0]
  K1(58,:) = [   0,   0]
  K1(59,:) = [   0,   0]
  K1(60,:) = [   0,   0]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppvvvj_bbbxbxzzz_1_/**/REALKIND



module ol_forced_parameters_ppvvvj_bbbxbxzzz_1_/**/REALKIND
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
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMZ /= 0) write(*,101) 'wMZ = 0'
  if (wMZ /= 0) write(*,101) 'wMZ = 0'
  if (wMZ /= 0) write(*,101) 'wMZ = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppvvvj_bbbxbxzzz_1_/**/REALKIND

module ol_tree_ppvvvj_bbbxbxzzz_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(3)
  complex(REALKIND), save :: den(563)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 432 ! number of helicity configurations
  integer(intkind2), save :: nhel = 432 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(432) ! physical helicity states
  complex(DREALKIND) :: M1helarr(2,432) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = CI*eQED**3*gQCD**2
    f(2) = (CI*eQED**3*gQCD**2*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(3) = (CI*eQED**3*gQCD**2*MW**2)/(cw**4*sw**2)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,18) - MB2)
  den(2) = 1 / (Q(5,5))
  den(3) = 1 / (Q(5,40) - MB2)
  den(5) = 1 / (Q(5,82) - MB2)
  den(9) = 1 / (Q(5,104) - MB2)
  den(12) = 1 / (Q(5,72) - MB2)
  den(14) = 1 / (Q(5,50) - MB2)
  den(19) = 1 / (Q(5,96) - MH2)
  den(21) = 1 / (Q(5,13) - MB2)
  den(28) = 1 / (Q(5,34) - MB2)
  den(29) = 1 / (Q(5,24) - MB2)
  den(31) = 1 / (Q(5,98) - MB2)
  den(35) = 1 / (Q(5,88) - MB2)
  den(38) = 1 / (Q(5,66) - MB2)
  den(42) = 1 / (Q(5,56) - MB2)
  den(46) = 1 / (Q(5,7) - MB2)
  den(57) = 1 / (Q(5,80) - MH2)
  den(74) = 1 / (Q(5,48) - MH2)
  den(83) = 1 / (Q(5,112) - MZ2)
  den(103) = 1 / (Q(5,17) - MB2)
  den(104) = 1 / (Q(5,6))
  den(106) = 1 / (Q(5,81) - MB2)
  den(112) = 1 / (Q(5,49) - MB2)
  den(117) = 1 / (Q(5,14) - MB2)
  den(123) = 1 / (Q(5,33) - MB2)
  den(125) = 1 / (Q(5,97) - MB2)
  den(130) = 1 / (Q(5,65) - MB2)
  den(179) = 1 / (Q(5,9))
  den(180) = 1 / (Q(5,36) - MB2)
  den(184) = 1 / (Q(5,100) - MB2)
  den(187) = 1 / (Q(5,68) - MB2)
  den(198) = 1 / (Q(5,20) - MB2)
  den(202) = 1 / (Q(5,84) - MB2)
  den(207) = 1 / (Q(5,52) - MB2)
  den(211) = 1 / (Q(5,11) - MB2)
  den(255) = 1 / (Q(5,10))
  den(313) = 1 / (Q(5,25))
  den(317) = 1 / (Q(5,42))
  den(321) = 1 / (Q(5,21))
  den(325) = 1 / (Q(5,38))
  den(335) = 1 / (Q(5,74))
  den(347) = 1 / (Q(5,70))
  den(369) = 1 / (Q(5,41))
  den(373) = 1 / (Q(5,26))
  den(377) = 1 / (Q(5,37))
  den(381) = 1 / (Q(5,22))
  den(389) = 1 / (Q(5,73))
  den(401) = 1 / (Q(5,69))

  ! denominators

  den(4) = den(2)*den(3)
  den(6) = den(1)*den(5)
  den(7) = den(4)*den(6)
  den(8) = den(1)*den(2)
  den(10) = den(3)*den(9)
  den(11) = den(8)*den(10)
  den(13) = den(2)*den(12)
  den(15) = den(1)*den(14)
  den(16) = den(13)*den(15)
  den(17) = den(9)*den(12)
  den(18) = den(8)*den(17)
  den(20) = den(1)*den(19)
  den(22) = den(2)*den(21)
  den(23) = den(20)*den(22)
  den(24) = den(9)*den(19)
  den(25) = den(8)*den(24)
  den(26) = den(15)*den(22)
  den(27) = den(6)*den(22)
  den(30) = den(2)*den(29)
  den(32) = den(28)*den(31)
  den(33) = den(30)*den(32)
  den(34) = den(2)*den(28)
  den(36) = den(29)*den(35)
  den(37) = den(34)*den(36)
  den(39) = den(31)*den(38)
  den(40) = den(30)*den(39)
  den(41) = den(2)*den(38)
  den(43) = den(29)*den(42)
  den(44) = den(41)*den(43)
  den(45) = den(19)*den(29)
  den(47) = den(2)*den(46)
  den(48) = den(45)*den(47)
  den(49) = den(19)*den(31)
  den(50) = den(30)*den(49)
  den(51) = den(36)*den(47)
  den(52) = den(43)*den(47)
  den(53) = den(14)*den(28)
  den(54) = den(13)*den(53)
  den(55) = den(12)*den(35)
  den(56) = den(34)*den(55)
  den(58) = den(28)*den(57)
  den(59) = den(22)*den(58)
  den(60) = den(35)*den(57)
  den(61) = den(34)*den(60)
  den(62) = den(22)*den(53)
  den(63) = den(22)*den(32)
  den(64) = den(5)*den(38)
  den(65) = den(4)*den(64)
  den(66) = den(3)*den(42)
  den(67) = den(41)*den(66)
  den(68) = den(3)*den(57)
  den(69) = den(47)*den(68)
  den(70) = den(5)*den(57)
  den(71) = den(4)*den(70)
  den(72) = den(10)*den(47)
  den(73) = den(47)*den(66)
  den(75) = den(38)*den(74)
  den(76) = den(22)*den(75)
  den(77) = den(42)*den(74)
  den(78) = den(41)*den(77)
  den(79) = den(12)*den(74)
  den(80) = den(47)*den(79)
  den(81) = den(14)*den(74)
  den(82) = den(13)*den(81)
  den(84) = den(74)*den(83)
  den(85) = den(47)*den(84)
  den(86) = den(47)*den(77)
  den(87) = den(22)*den(84)
  den(88) = den(22)*den(81)
  den(89) = den(22)*den(64)
  den(90) = den(22)*den(39)
  den(91) = den(17)*den(47)
  den(92) = den(47)*den(55)
  den(93) = den(57)*den(83)
  den(94) = den(47)*den(93)
  den(95) = den(47)*den(60)
  den(96) = den(22)*den(93)
  den(97) = den(22)*den(70)
  den(98) = den(19)*den(83)
  den(99) = den(47)*den(98)
  den(100) = den(24)*den(47)
  den(101) = den(22)*den(98)
  den(102) = den(22)*den(49)
  den(105) = den(3)*den(104)
  den(107) = den(103)*den(106)
  den(108) = den(105)*den(107)
  den(109) = den(103)*den(104)
  den(110) = den(10)*den(109)
  den(111) = den(12)*den(104)
  den(113) = den(103)*den(112)
  den(114) = den(111)*den(113)
  den(115) = den(17)*den(109)
  den(116) = den(19)*den(103)
  den(118) = den(104)*den(117)
  den(119) = den(116)*den(118)
  den(120) = den(24)*den(109)
  den(121) = den(113)*den(118)
  den(122) = den(107)*den(118)
  den(124) = den(29)*den(104)
  den(126) = den(123)*den(125)
  den(127) = den(124)*den(126)
  den(128) = den(104)*den(123)
  den(129) = den(36)*den(128)
  den(131) = den(125)*den(130)
  den(132) = den(124)*den(131)
  den(133) = den(104)*den(130)
  den(134) = den(43)*den(133)
  den(135) = den(46)*den(104)
  den(136) = den(45)*den(135)
  den(137) = den(19)*den(125)
  den(138) = den(124)*den(137)
  den(139) = den(36)*den(135)
  den(140) = den(43)*den(135)
  den(141) = den(112)*den(123)
  den(142) = den(111)*den(141)
  den(143) = den(55)*den(128)
  den(144) = den(57)*den(123)
  den(145) = den(118)*den(144)
  den(146) = den(60)*den(128)
  den(147) = den(118)*den(141)
  den(148) = den(118)*den(126)
  den(149) = den(106)*den(130)
  den(150) = den(105)*den(149)
  den(151) = den(66)*den(133)
  den(152) = den(68)*den(135)
  den(153) = den(57)*den(106)
  den(154) = den(105)*den(153)
  den(155) = den(10)*den(135)
  den(156) = den(66)*den(135)
  den(157) = den(74)*den(130)
  den(158) = den(118)*den(157)
  den(159) = den(77)*den(133)
  den(160) = den(79)*den(135)
  den(161) = den(74)*den(112)
  den(162) = den(111)*den(161)
  den(163) = den(84)*den(135)
  den(164) = den(77)*den(135)
  den(165) = den(84)*den(118)
  den(166) = den(118)*den(161)
  den(167) = den(118)*den(149)
  den(168) = den(118)*den(131)
  den(169) = den(17)*den(135)
  den(170) = den(55)*den(135)
  den(171) = den(93)*den(135)
  den(172) = den(60)*den(135)
  den(173) = den(93)*den(118)
  den(174) = den(118)*den(153)
  den(175) = den(98)*den(135)
  den(176) = den(24)*den(135)
  den(177) = den(98)*den(118)
  den(178) = den(118)*den(137)
  den(181) = den(179)*den(180)
  den(182) = den(6)*den(181)
  den(183) = den(1)*den(179)
  den(185) = den(180)*den(184)
  den(186) = den(183)*den(185)
  den(188) = den(179)*den(187)
  den(189) = den(15)*den(188)
  den(190) = den(184)*den(187)
  den(191) = den(183)*den(190)
  den(192) = den(21)*den(179)
  den(193) = den(20)*den(192)
  den(194) = den(19)*den(184)
  den(195) = den(183)*den(194)
  den(196) = den(15)*den(192)
  den(197) = den(6)*den(192)
  den(199) = den(179)*den(198)
  den(200) = den(32)*den(199)
  den(201) = den(28)*den(179)
  den(203) = den(198)*den(202)
  den(204) = den(201)*den(203)
  den(205) = den(39)*den(199)
  den(206) = den(38)*den(179)
  den(208) = den(198)*den(207)
  den(209) = den(206)*den(208)
  den(210) = den(19)*den(198)
  den(212) = den(179)*den(211)
  den(213) = den(210)*den(212)
  den(214) = den(49)*den(199)
  den(215) = den(203)*den(212)
  den(216) = den(208)*den(212)
  den(217) = den(53)*den(188)
  den(218) = den(187)*den(202)
  den(219) = den(201)*den(218)
  den(220) = den(58)*den(192)
  den(221) = den(57)*den(202)
  den(222) = den(201)*den(221)
  den(223) = den(53)*den(192)
  den(224) = den(32)*den(192)
  den(225) = den(64)*den(181)
  den(226) = den(180)*den(207)
  den(227) = den(206)*den(226)
  den(228) = den(57)*den(180)
  den(229) = den(212)*den(228)
  den(230) = den(70)*den(181)
  den(231) = den(185)*den(212)
  den(232) = den(212)*den(226)
  den(233) = den(75)*den(192)
  den(234) = den(74)*den(207)
  den(235) = den(206)*den(234)
  den(236) = den(74)*den(187)
  den(237) = den(212)*den(236)
  den(238) = den(81)*den(188)
  den(239) = den(84)*den(212)
  den(240) = den(212)*den(234)
  den(241) = den(84)*den(192)
  den(242) = den(81)*den(192)
  den(243) = den(64)*den(192)
  den(244) = den(39)*den(192)
  den(245) = den(190)*den(212)
  den(246) = den(212)*den(218)
  den(247) = den(93)*den(212)
  den(248) = den(212)*den(221)
  den(249) = den(93)*den(192)
  den(250) = den(70)*den(192)
  den(251) = den(98)*den(212)
  den(252) = den(194)*den(212)
  den(253) = den(98)*den(192)
  den(254) = den(49)*den(192)
  den(256) = den(180)*den(255)
  den(257) = den(107)*den(256)
  den(258) = den(103)*den(255)
  den(259) = den(185)*den(258)
  den(260) = den(187)*den(255)
  den(261) = den(113)*den(260)
  den(262) = den(190)*den(258)
  den(263) = den(117)*den(255)
  den(264) = den(116)*den(263)
  den(265) = den(194)*den(258)
  den(266) = den(113)*den(263)
  den(267) = den(107)*den(263)
  den(268) = den(198)*den(255)
  den(269) = den(126)*den(268)
  den(270) = den(123)*den(255)
  den(271) = den(203)*den(270)
  den(272) = den(131)*den(268)
  den(273) = den(130)*den(255)
  den(274) = den(208)*den(273)
  den(275) = den(211)*den(255)
  den(276) = den(210)*den(275)
  den(277) = den(137)*den(268)
  den(278) = den(203)*den(275)
  den(279) = den(208)*den(275)
  den(280) = den(141)*den(260)
  den(281) = den(218)*den(270)
  den(282) = den(144)*den(263)
  den(283) = den(221)*den(270)
  den(284) = den(141)*den(263)
  den(285) = den(126)*den(263)
  den(286) = den(149)*den(256)
  den(287) = den(226)*den(273)
  den(288) = den(228)*den(275)
  den(289) = den(153)*den(256)
  den(290) = den(185)*den(275)
  den(291) = den(226)*den(275)
  den(292) = den(157)*den(263)
  den(293) = den(234)*den(273)
  den(294) = den(236)*den(275)
  den(295) = den(161)*den(260)
  den(296) = den(84)*den(275)
  den(297) = den(234)*den(275)
  den(298) = den(84)*den(263)
  den(299) = den(161)*den(263)
  den(300) = den(149)*den(263)
  den(301) = den(131)*den(263)
  den(302) = den(190)*den(275)
  den(303) = den(218)*den(275)
  den(304) = den(93)*den(275)
  den(305) = den(221)*den(275)
  den(306) = den(93)*den(263)
  den(307) = den(153)*den(263)
  den(308) = den(98)*den(275)
  den(309) = den(194)*den(275)
  den(310) = den(98)*den(263)
  den(311) = den(137)*den(263)
  den(312) = den(28)*den(187)
  den(314) = den(103)*den(313)
  den(315) = den(312)*den(314)
  den(316) = den(103)*den(187)
  den(318) = den(28)*den(317)
  den(319) = den(316)*den(318)
  den(320) = den(12)*den(28)
  den(322) = den(103)*den(321)
  den(323) = den(320)*den(322)
  den(324) = den(12)*den(103)
  den(326) = den(28)*den(325)
  den(327) = den(324)*den(326)
  den(328) = den(32)*den(322)
  den(329) = den(32)*den(314)
  den(330) = den(107)*den(326)
  den(331) = den(107)*den(318)
  den(332) = den(38)*den(180)
  den(333) = den(314)*den(332)
  den(334) = den(103)*den(180)
  den(336) = den(38)*den(335)
  den(337) = den(334)*den(336)
  den(338) = den(180)*den(325)
  den(339) = den(324)*den(338)
  den(340) = den(12)*den(335)
  den(341) = den(334)*den(340)
  den(342) = den(185)*den(314)
  den(343) = den(107)*den(338)
  den(344) = den(3)*den(38)
  den(345) = den(322)*den(344)
  den(346) = den(3)*den(103)
  den(348) = den(38)*den(347)
  den(349) = den(346)*den(348)
  den(350) = den(187)*den(347)
  den(351) = den(346)*den(350)
  den(352) = den(3)*den(317)
  den(353) = den(316)*den(352)
  den(354) = den(10)*den(322)
  den(355) = den(107)*den(352)
  den(356) = den(39)*den(322)
  den(357) = den(39)*den(314)
  den(358) = den(113)*den(348)
  den(359) = den(113)*den(336)
  den(360) = den(190)*den(314)
  den(361) = den(113)*den(350)
  den(362) = den(17)*den(322)
  den(363) = den(113)*den(340)
  den(364) = den(24)*den(322)
  den(365) = den(49)*den(322)
  den(366) = den(194)*den(314)
  den(367) = den(49)*den(314)
  den(368) = den(1)*den(187)
  den(370) = den(123)*den(369)
  den(371) = den(368)*den(370)
  den(372) = den(123)*den(187)
  den(374) = den(1)*den(373)
  den(375) = den(372)*den(374)
  den(376) = den(1)*den(12)
  den(378) = den(123)*den(377)
  den(379) = den(376)*den(378)
  den(380) = den(12)*den(123)
  den(382) = den(1)*den(381)
  den(383) = den(380)*den(382)
  den(384) = den(6)*den(378)
  den(385) = den(6)*den(370)
  den(386) = den(126)*den(382)
  den(387) = den(126)*den(374)
  den(388) = den(1)*den(180)
  den(390) = den(130)*den(389)
  den(391) = den(388)*den(390)
  den(392) = den(130)*den(180)
  den(393) = den(374)*den(392)
  den(394) = den(180)*den(377)
  den(395) = den(376)*den(394)
  den(396) = den(12)*den(389)
  den(397) = den(388)*den(396)
  den(398) = den(6)*den(394)
  den(399) = den(185)*den(374)
  den(400) = den(1)*den(3)
  den(402) = den(130)*den(401)
  den(403) = den(400)*den(402)
  den(404) = den(3)*den(130)
  den(405) = den(382)*den(404)
  den(406) = den(187)*den(401)
  den(407) = den(400)*den(406)
  den(408) = den(3)*den(369)
  den(409) = den(368)*den(408)
  den(410) = den(6)*den(408)
  den(411) = den(10)*den(382)
  den(412) = den(15)*den(402)
  den(413) = den(15)*den(390)
  den(414) = den(131)*den(382)
  den(415) = den(131)*den(374)
  den(416) = den(15)*den(406)
  den(417) = den(190)*den(374)
  den(418) = den(15)*den(396)
  den(419) = den(17)*den(382)
  den(420) = den(24)*den(382)
  den(421) = den(194)*den(374)
  den(422) = den(137)*den(374)
  den(423) = den(137)*den(382)
  den(424) = den(38)*den(198)
  den(425) = den(370)*den(424)
  den(426) = den(123)*den(198)
  den(427) = den(336)*den(426)
  den(428) = den(198)*den(381)
  den(429) = den(380)*den(428)
  den(430) = den(340)*den(426)
  den(431) = den(203)*den(370)
  den(432) = den(126)*den(428)
  den(433) = den(28)*den(198)
  den(434) = den(390)*den(433)
  den(435) = den(130)*den(198)
  den(436) = den(318)*den(435)
  den(437) = den(198)*den(321)
  den(438) = den(320)*den(437)
  den(439) = den(396)*den(433)
  den(440) = den(32)*den(437)
  den(441) = den(203)*den(318)
  den(442) = den(404)*den(428)
  den(443) = den(352)*den(435)
  den(444) = den(344)*den(437)
  den(445) = den(408)*den(424)
  den(446) = den(10)*den(437)
  den(447) = den(203)*den(408)
  den(448) = den(10)*den(428)
  den(449) = den(203)*den(352)
  den(450) = den(208)*den(390)
  den(451) = den(131)*den(428)
  den(452) = den(39)*den(437)
  den(453) = den(208)*den(336)
  den(454) = den(17)*den(437)
  den(455) = den(208)*den(396)
  den(456) = den(17)*den(428)
  den(457) = den(208)*den(340)
  den(458) = den(24)*den(437)
  den(459) = den(24)*den(428)
  den(460) = den(49)*den(437)
  den(461) = den(137)*den(428)
  den(462) = den(29)*den(38)
  den(463) = den(378)*den(462)
  den(464) = den(29)*den(123)
  den(465) = den(348)*den(464)
  den(466) = den(350)*den(464)
  den(467) = den(29)*den(373)
  den(468) = den(372)*den(467)
  den(469) = den(36)*den(378)
  den(470) = den(126)*den(467)
  den(471) = den(28)*den(29)
  den(472) = den(402)*den(471)
  den(473) = den(29)*den(130)
  den(474) = den(326)*den(473)
  den(475) = den(406)*den(471)
  den(476) = den(29)*den(313)
  den(477) = den(312)*den(476)
  den(478) = den(32)*den(476)
  den(479) = den(36)*den(326)
  den(480) = den(338)*den(473)
  den(481) = den(392)*den(467)
  den(482) = den(394)*den(462)
  den(483) = den(332)*den(476)
  den(484) = den(36)*den(394)
  den(485) = den(185)*den(476)
  den(486) = den(36)*den(338)
  den(487) = den(185)*den(467)
  den(488) = den(43)*den(402)
  den(489) = den(131)*den(467)
  den(490) = den(39)*den(476)
  den(491) = den(43)*den(348)
  den(492) = den(43)*den(406)
  den(493) = den(190)*den(476)
  den(494) = den(43)*den(350)
  den(495) = den(190)*den(467)
  den(496) = den(194)*den(467)
  den(497) = den(194)*den(476)
  den(498) = den(49)*den(476)
  den(499) = den(137)*den(467)
  den(500) = den(64)*den(378)
  den(501) = den(64)*den(370)
  den(502) = den(141)*den(348)
  den(503) = den(141)*den(336)
  den(504) = den(218)*den(370)
  den(505) = den(141)*den(350)
  den(506) = den(55)*den(378)
  den(507) = den(141)*den(340)
  den(508) = den(60)*den(378)
  den(509) = den(70)*den(378)
  den(510) = den(221)*den(370)
  den(511) = den(70)*den(370)
  den(512) = den(53)*den(402)
  den(513) = den(53)*den(390)
  den(514) = den(149)*den(326)
  den(515) = den(149)*den(318)
  den(516) = den(53)*den(406)
  den(517) = den(218)*den(318)
  den(518) = den(53)*den(396)
  den(519) = den(55)*den(326)
  den(520) = den(60)*den(326)
  den(521) = den(221)*den(318)
  den(522) = den(153)*den(318)
  den(523) = den(153)*den(326)
  den(524) = den(226)*den(390)
  den(525) = den(149)*den(338)
  den(526) = den(64)*den(394)
  den(527) = den(226)*den(336)
  den(528) = den(55)*den(394)
  den(529) = den(226)*den(396)
  den(530) = den(55)*den(338)
  den(531) = den(226)*den(340)
  den(532) = den(60)*den(394)
  den(533) = den(60)*den(338)
  den(534) = den(70)*den(394)
  den(535) = den(153)*den(338)
  den(536) = den(66)*den(402)
  den(537) = den(149)*den(352)
  den(538) = den(64)*den(408)
  den(539) = den(66)*den(348)
  den(540) = den(66)*den(406)
  den(541) = den(218)*den(408)
  den(542) = den(66)*den(350)
  den(543) = den(218)*den(352)
  den(544) = den(221)*den(352)
  den(545) = den(221)*den(408)
  den(546) = den(70)*den(408)
  den(547) = den(153)*den(352)
  den(548) = den(77)*den(402)
  den(549) = den(81)*den(402)
  den(550) = den(234)*den(390)
  den(551) = den(81)*den(390)
  den(552) = den(77)*den(348)
  den(553) = den(234)*den(336)
  den(554) = den(161)*den(336)
  den(555) = den(161)*den(348)
  den(556) = den(77)*den(406)
  den(557) = den(77)*den(350)
  den(558) = den(81)*den(406)
  den(559) = den(161)*den(350)
  den(560) = den(234)*den(340)
  den(561) = den(234)*den(396)
  den(562) = den(81)*den(396)
  den(563) = den(161)*den(340)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_ppvvvj_bbbxbxzzz_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_ppvvvj_bbbxbxzzz_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for bottom bottom anti-bottom anti-bottom Z Z Z -> 0
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
  use ol_external_ppvvvj_bbbxbxzzz_1, only: external_perm_ppvvvj_bbbxbxzzz_1, &
    & external_perm_inv_ppvvvj_bbbxbxzzz_1, extcomb_perm_ppvvvj_bbbxbxzzz_1, &
    & average_factor_ppvvvj_bbbxbxzzz_1
  use ol_external_ppvvvj_bbbxbxzzz_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_ppvvvj_bbbxbxzzz_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_ppvvvj_bbbxbxzzz_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_ppvvvj_bbbxbxzzz_1
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
  complex(REALKIND) :: MOM_LC(4), M1(2), M1helarray(2,432)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(3), ex6(3), ex7(3)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,4), wf6(6,24), wf8(8,16), wf9(9,3), wf12(12,24), wf16(16,8), wf18(18,72), wf24(24,96), wf27(27,6), &
    wf36(36,42), wf54(54,18), wf432(432,384)

  type(polcont) :: A(432,384)
  complex(REALKIND) :: Aj(384)

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
  extmasses2 = [ rMB2, rMB2, rMB2, rMB2, rMZ2, rMZ2, rMZ2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_ppvvvj_bbbxbxzzz_1,7)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,7)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_ppvvvj_bbbxbxzzz_1(I)
  else if (I < 0) then
    JBmunu = external_perm_ppvvvj_bbbxbxzzz_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_ppvvvj_bbbxbxzzz_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_Q(P(:,1), rMB, H1, ex1)
  call wf_Q(P(:,2), rMB, H2, ex2)
  call wf_A(P(:,3), rMB, H3, ex3)
  call wf_A(P(:,4), rMB, H4, ex4)
  call wf_V(P(:,5), rMZ, H5, ex5)
  call wf_V(P(:,6), rMZ, H6, ex6)
  call wf_V(P(:,7), rMZ, H7, ex7)


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
  call vert_ZQ_A(gZd,ntry, ex5, ex2, wf6(:,1), n3(:,2), t3x6(:,:,1))
  call vert_AZ_Q(gZd,ntry, ex4, ex6, wf6(:,2), n3(:,3), t3x6(:,:,2))
  call prop_Q_A(ntry, wf6(:,1), Q(:,18), MB, 1_intkind1, wf6(:,3), n2(1))
  call prop_A_Q(ntry, wf6(:,2), Q(:,40), MB, 1_intkind1, wf6(:,4), n2(2))
  call vert_ZQ_A(gZd,ntry, ex7, wf6(:,3), wf18(:,1), n3(:,4), t3x18(:,:,1))
  call vert_AV_Q(ntry, wf6(:,4), wf4(:,1), wf24(:,1), n3(:,5), t3x24(:,:,1))
  call prop_Q_A(ntry, wf18(:,1), Q(:,82), MB, 1_intkind1, wf18(:,2), n2(3))
  call vert_AZ_Q(gZd,ntry, wf6(:,4), ex7, wf18(:,3), n3(:,6), t3x18(:,:,2))
  call vert_VQ_A(ntry, wf4(:,1), wf6(:,3), wf24(:,2), n3(:,7), t3x24(:,:,2))
  call prop_A_Q(ntry, wf18(:,3), Q(:,104), MB, 1_intkind1, wf18(:,4), n2(4))
  call vert_AZ_Q(gZd,ntry, ex4, ex7, wf6(:,5), n3(:,8), t3x6(:,:,3))
  call prop_A_Q(ntry, wf6(:,5), Q(:,72), MB, 1_intkind1, wf6(:,6), n2(5))
  call vert_ZQ_A(gZd,ntry, ex6, wf6(:,3), wf18(:,5), n3(:,9), t3x18(:,:,3))
  call vert_AV_Q(ntry, wf6(:,6), wf4(:,1), wf24(:,3), n3(:,10), t3x24(:,:,3))
  call prop_Q_A(ntry, wf18(:,5), Q(:,50), MB, 1_intkind1, wf18(:,6), n2(6))
  call vert_AZ_Q(gZd,ntry, wf6(:,6), ex6, wf18(:,7), n3(:,11), t3x18(:,:,4))
  call prop_A_Q(ntry, wf18(:,7), Q(:,104), MB, 1_intkind1, wf18(:,8), n2(7))
  call vert_VV_S(ntry, ex6, ex7, wf9(:,1), n3(:,12), t3x9(:,:,1))
  call vert_AV_Q(ntry, ex4, wf4(:,1), wf8(:,1), n3(:,13), t3x8(:,:,1))
  call vert_QS_A(gH,ntry, wf6(:,3), wf9(:,1), wf54(:,1), n3(:,14), t3x54(:,:,1))
  call prop_A_Q(ntry, wf8(:,1), Q(:,13), MB, 1_intkind1, wf8(:,2), n2(8))
  call vert_SA_Q(gH,ntry, wf9(:,1), ex4, wf18(:,9), n3(:,15), t3x18(:,:,5))
  call prop_A_Q(ntry, wf18(:,9), Q(:,104), MB, 1_intkind1, wf18(:,10), n2(9))
  call vert_AZ_Q(gZd,ntry, wf8(:,2), ex7, wf24(:,4), n3(:,16), t3x24(:,:,4))
  call vert_AZ_Q(gZd,ntry, wf8(:,2), ex6, wf24(:,5), n3(:,17), t3x24(:,:,5))
  call vert_ZQ_A(gZd,ntry, ex6, ex2, wf6(:,7), n3(:,18), t3x6(:,:,4))
  call vert_AZ_Q(gZd,ntry, ex4, ex5, wf6(:,8), n3(:,19), t3x6(:,:,5))
  call prop_Q_A(ntry, wf6(:,7), Q(:,34), MB, 1_intkind1, wf6(:,9), n2(10))
  call prop_A_Q(ntry, wf6(:,8), Q(:,24), MB, 1_intkind1, wf6(:,10), n2(11))
  call vert_ZQ_A(gZd,ntry, ex7, wf6(:,9), wf18(:,11), n3(:,20), t3x18(:,:,6))
  call vert_AV_Q(ntry, wf6(:,10), wf4(:,1), wf24(:,6), n3(:,21), t3x24(:,:,6))
  call prop_Q_A(ntry, wf18(:,11), Q(:,98), MB, 1_intkind1, wf18(:,12), n2(12))
  call vert_AZ_Q(gZd,ntry, wf6(:,10), ex7, wf18(:,13), n3(:,22), t3x18(:,:,7))
  call vert_VQ_A(ntry, wf4(:,1), wf6(:,9), wf24(:,7), n3(:,23), t3x24(:,:,7))
  call prop_A_Q(ntry, wf18(:,13), Q(:,88), MB, 1_intkind1, wf18(:,14), n2(13))
  call vert_ZQ_A(gZd,ntry, ex7, ex2, wf6(:,11), n3(:,24), t3x6(:,:,6))
  call prop_Q_A(ntry, wf6(:,11), Q(:,66), MB, 1_intkind1, wf6(:,12), n2(14))
  call vert_ZQ_A(gZd,ntry, ex6, wf6(:,12), wf18(:,15), n3(:,25), t3x18(:,:,8))
  call prop_Q_A(ntry, wf18(:,15), Q(:,98), MB, 1_intkind1, wf18(:,16), n2(15))
  call vert_AZ_Q(gZd,ntry, wf6(:,10), ex6, wf18(:,17), n3(:,26), t3x18(:,:,9))
  call vert_VQ_A(ntry, wf4(:,1), wf6(:,12), wf24(:,8), n3(:,27), t3x24(:,:,8))
  call prop_A_Q(ntry, wf18(:,17), Q(:,56), MB, 1_intkind1, wf18(:,18), n2(16))
  call vert_VQ_A(ntry, wf4(:,1), ex2, wf8(:,3), n3(:,28), t3x8(:,:,2))
  call vert_SA_Q(gH,ntry, wf9(:,1), wf6(:,10), wf54(:,2), n3(:,29), t3x54(:,:,2))
  call prop_Q_A(ntry, wf8(:,3), Q(:,7), MB, 1_intkind1, wf8(:,4), n2(17))
  call vert_QS_A(gH,ntry, ex2, wf9(:,1), wf18(:,19), n3(:,30), t3x18(:,:,10))
  call prop_Q_A(ntry, wf18(:,19), Q(:,98), MB, 1_intkind1, wf18(:,20), n2(18))
  call vert_ZQ_A(gZd,ntry, ex6, wf8(:,4), wf24(:,9), n3(:,31), t3x24(:,:,9))
  call vert_ZQ_A(gZd,ntry, ex7, wf8(:,4), wf24(:,10), n3(:,32), t3x24(:,:,10))
  call vert_ZQ_A(gZd,ntry, ex5, wf6(:,9), wf18(:,21), n3(:,33), t3x18(:,:,11))
  call prop_Q_A(ntry, wf18(:,21), Q(:,50), MB, 1_intkind1, wf18(:,22), n2(19))
  call vert_AZ_Q(gZd,ntry, wf6(:,6), ex5, wf18(:,23), n3(:,34), t3x18(:,:,12))
  call prop_A_Q(ntry, wf18(:,23), Q(:,88), MB, 1_intkind1, wf18(:,24), n2(20))
  call vert_VV_S(ntry, ex5, ex7, wf9(:,2), n3(:,35), t3x9(:,:,2))
  call vert_QS_A(gH,ntry, wf6(:,9), wf9(:,2), wf54(:,3), n3(:,36), t3x54(:,:,3))
  call vert_SA_Q(gH,ntry, wf9(:,2), ex4, wf18(:,25), n3(:,37), t3x18(:,:,13))
  call prop_A_Q(ntry, wf18(:,25), Q(:,88), MB, 1_intkind1, wf18(:,26), n2(21))
  call vert_AZ_Q(gZd,ntry, wf8(:,2), ex5, wf24(:,11), n3(:,38), t3x24(:,:,11))
  call vert_ZQ_A(gZd,ntry, ex5, wf6(:,12), wf18(:,27), n3(:,39), t3x18(:,:,14))
  call prop_Q_A(ntry, wf18(:,27), Q(:,82), MB, 1_intkind1, wf18(:,28), n2(22))
  call vert_AZ_Q(gZd,ntry, wf6(:,4), ex5, wf18(:,29), n3(:,40), t3x18(:,:,15))
  call prop_A_Q(ntry, wf18(:,29), Q(:,56), MB, 1_intkind1, wf18(:,30), n2(23))
  call vert_SA_Q(gH,ntry, wf9(:,2), wf6(:,4), wf54(:,4), n3(:,41), t3x54(:,:,4))
  call vert_QS_A(gH,ntry, ex2, wf9(:,2), wf18(:,31), n3(:,42), t3x18(:,:,16))
  call prop_Q_A(ntry, wf18(:,31), Q(:,82), MB, 1_intkind1, wf18(:,32), n2(24))
  call vert_ZQ_A(gZd,ntry, ex5, wf8(:,4), wf24(:,12), n3(:,43), t3x24(:,:,12))
  call vert_VV_S(ntry, ex5, ex6, wf9(:,3), n3(:,44), t3x9(:,:,3))
  call vert_QS_A(gH,ntry, wf6(:,12), wf9(:,3), wf54(:,5), n3(:,45), t3x54(:,:,5))
  call vert_SA_Q(gH,ntry, wf9(:,3), ex4, wf18(:,33), n3(:,46), t3x18(:,:,17))
  call prop_A_Q(ntry, wf18(:,33), Q(:,56), MB, 1_intkind1, wf18(:,34), n2(25))
  call vert_SA_Q(gH,ntry, wf9(:,3), wf6(:,6), wf54(:,6), n3(:,47), t3x54(:,:,6))
  call vert_QS_A(gH,ntry, ex2, wf9(:,3), wf18(:,35), n3(:,48), t3x18(:,:,18))
  call prop_Q_A(ntry, wf18(:,35), Q(:,50), MB, 1_intkind1, wf18(:,36), n2(26))
  call vert_SV_V(ntry, wf9(:,3), ex7, wf27(:,1), n3(:,49), t3x27(:,:,1))
  call prop_W_W(ntry, wf27(:,1), Q(:,112), MZ, 1_intkind1, wf27(:,2), n2(27))
  call vert_QA_Z(gZd,ntry, wf8(:,4), ex4, wf16(:,1), n3(:,50), t3x16(:,:,1))
  call vert_QA_Z(gZd,ntry, ex2, wf8(:,2), wf16(:,2), n3(:,51), t3x16(:,:,2))
  call vert_ZQ_A(gZd,ntry, ex7, wf18(:,36), wf54(:,7), n3(:,52), t3x54(:,:,7))
  call vert_SV_V(ntry, wf9(:,2), ex6, wf27(:,3), n3(:,53), t3x27(:,:,2))
  call prop_W_W(ntry, wf27(:,3), Q(:,112), MZ, 1_intkind1, wf27(:,4), n2(28))
  call vert_ZQ_A(gZd,ntry, ex6, wf18(:,32), wf54(:,8), n3(:,54), t3x54(:,:,8))
  call vert_SV_V(ntry, wf9(:,1), ex5, wf27(:,5), n3(:,55), t3x27(:,:,3))
  call prop_W_W(ntry, wf27(:,5), Q(:,112), MZ, 1_intkind1, wf27(:,6), n2(29))
  call vert_ZQ_A(gZd,ntry, ex5, wf18(:,20), wf54(:,9), n3(:,56), t3x54(:,:,9))
  call vert_ZQ_A(gZd,ntry, ex5, ex1, wf6(:,13), n3(:,57), t3x6(:,:,7))
  call vert_QA_V(ntry, ex2, ex3, wf4(:,2), n3(:,58), t3x4(:,:,2))
  call prop_Q_A(ntry, wf6(:,13), Q(:,17), MB, 1_intkind1, wf6(:,14), n2(30))
  call vert_ZQ_A(gZd,ntry, ex7, wf6(:,14), wf18(:,37), n3(:,59), t3x18(:,:,19))
  call vert_AV_Q(ntry, wf6(:,4), wf4(:,2), wf24(:,13), n3(:,60), t3x24(:,:,13))
  call prop_Q_A(ntry, wf18(:,37), Q(:,81), MB, 1_intkind1, wf18(:,38), n2(31))
  call vert_VQ_A(ntry, wf4(:,2), wf6(:,14), wf24(:,14), n3(:,61), t3x24(:,:,14))
  call vert_ZQ_A(gZd,ntry, ex6, wf6(:,14), wf18(:,39), n3(:,62), t3x18(:,:,20))
  call vert_AV_Q(ntry, wf6(:,6), wf4(:,2), wf24(:,15), n3(:,63), t3x24(:,:,15))
  call prop_Q_A(ntry, wf18(:,39), Q(:,49), MB, 1_intkind1, wf18(:,40), n2(32))
  call vert_AV_Q(ntry, ex4, wf4(:,2), wf8(:,5), n3(:,64), t3x8(:,:,3))
  call vert_QS_A(gH,ntry, wf6(:,14), wf9(:,1), wf54(:,10), n3(:,65), t3x54(:,:,10))
  call prop_A_Q(ntry, wf8(:,5), Q(:,14), MB, 1_intkind1, wf8(:,6), n2(33))
  call vert_AZ_Q(gZd,ntry, wf8(:,6), ex7, wf24(:,16), n3(:,66), t3x24(:,:,16))
  call vert_AZ_Q(gZd,ntry, wf8(:,6), ex6, wf24(:,17), n3(:,67), t3x24(:,:,17))
  call vert_ZQ_A(gZd,ntry, ex6, ex1, wf6(:,15), n3(:,68), t3x6(:,:,8))
  call prop_Q_A(ntry, wf6(:,15), Q(:,33), MB, 1_intkind1, wf6(:,16), n2(34))
  call vert_ZQ_A(gZd,ntry, ex7, wf6(:,16), wf18(:,41), n3(:,69), t3x18(:,:,21))
  call vert_AV_Q(ntry, wf6(:,10), wf4(:,2), wf24(:,18), n3(:,70), t3x24(:,:,18))
  call prop_Q_A(ntry, wf18(:,41), Q(:,97), MB, 1_intkind1, wf18(:,42), n2(35))
  call vert_VQ_A(ntry, wf4(:,2), wf6(:,16), wf24(:,19), n3(:,71), t3x24(:,:,19))
  call vert_ZQ_A(gZd,ntry, ex7, ex1, wf6(:,17), n3(:,72), t3x6(:,:,9))
  call prop_Q_A(ntry, wf6(:,17), Q(:,65), MB, 1_intkind1, wf6(:,18), n2(36))
  call vert_ZQ_A(gZd,ntry, ex6, wf6(:,18), wf18(:,43), n3(:,73), t3x18(:,:,22))
  call prop_Q_A(ntry, wf18(:,43), Q(:,97), MB, 1_intkind1, wf18(:,44), n2(37))
  call vert_VQ_A(ntry, wf4(:,2), wf6(:,18), wf24(:,20), n3(:,74), t3x24(:,:,20))
  call vert_VQ_A(ntry, wf4(:,2), ex1, wf8(:,7), n3(:,75), t3x8(:,:,4))
  call prop_Q_A(ntry, wf8(:,7), Q(:,7), MB, 1_intkind1, wf8(:,8), n2(38))
  call vert_QS_A(gH,ntry, ex1, wf9(:,1), wf18(:,45), n3(:,76), t3x18(:,:,23))
  call prop_Q_A(ntry, wf18(:,45), Q(:,97), MB, 1_intkind1, wf18(:,46), n2(39))
  call vert_ZQ_A(gZd,ntry, ex6, wf8(:,8), wf24(:,21), n3(:,77), t3x24(:,:,21))
  call vert_ZQ_A(gZd,ntry, ex7, wf8(:,8), wf24(:,22), n3(:,78), t3x24(:,:,22))
  call vert_ZQ_A(gZd,ntry, ex5, wf6(:,16), wf18(:,47), n3(:,79), t3x18(:,:,24))
  call prop_Q_A(ntry, wf18(:,47), Q(:,49), MB, 1_intkind1, wf18(:,48), n2(40))
  call vert_QS_A(gH,ntry, wf6(:,16), wf9(:,2), wf54(:,11), n3(:,80), t3x54(:,:,11))
  call vert_AZ_Q(gZd,ntry, wf8(:,6), ex5, wf24(:,23), n3(:,81), t3x24(:,:,23))
  call vert_ZQ_A(gZd,ntry, ex5, wf6(:,18), wf18(:,49), n3(:,82), t3x18(:,:,25))
  call prop_Q_A(ntry, wf18(:,49), Q(:,81), MB, 1_intkind1, wf18(:,50), n2(41))
  call vert_QS_A(gH,ntry, ex1, wf9(:,2), wf18(:,51), n3(:,83), t3x18(:,:,26))
  call prop_Q_A(ntry, wf18(:,51), Q(:,81), MB, 1_intkind1, wf18(:,52), n2(42))
  call vert_ZQ_A(gZd,ntry, ex5, wf8(:,8), wf24(:,24), n3(:,84), t3x24(:,:,24))
  call vert_QS_A(gH,ntry, wf6(:,18), wf9(:,3), wf54(:,12), n3(:,85), t3x54(:,:,12))
  call vert_QS_A(gH,ntry, ex1, wf9(:,3), wf18(:,53), n3(:,86), t3x18(:,:,27))
  call prop_Q_A(ntry, wf18(:,53), Q(:,49), MB, 1_intkind1, wf18(:,54), n2(43))
  call vert_QA_Z(gZd,ntry, wf8(:,8), ex4, wf16(:,3), n3(:,87), t3x16(:,:,3))
  call vert_QA_Z(gZd,ntry, ex1, wf8(:,6), wf16(:,4), n3(:,88), t3x16(:,:,4))
  call vert_ZQ_A(gZd,ntry, ex7, wf18(:,54), wf54(:,13), n3(:,89), t3x54(:,:,13))
  call vert_ZQ_A(gZd,ntry, ex6, wf18(:,52), wf54(:,14), n3(:,90), t3x54(:,:,14))
  call vert_ZQ_A(gZd,ntry, ex5, wf18(:,46), wf54(:,15), n3(:,91), t3x54(:,:,15))
  call vert_QA_V(ntry, ex1, ex4, wf4(:,3), n3(:,92), t3x4(:,:,3))
  call vert_AZ_Q(gZd,ntry, ex3, ex6, wf6(:,19), n3(:,93), t3x6(:,:,10))
  call prop_A_Q(ntry, wf6(:,19), Q(:,36), MB, 1_intkind1, wf6(:,20), n2(44))
  call vert_AV_Q(ntry, wf6(:,20), wf4(:,3), wf24(:,25), n3(:,94), t3x24(:,:,25))
  call vert_AZ_Q(gZd,ntry, wf6(:,20), ex7, wf18(:,55), n3(:,95), t3x18(:,:,28))
  call vert_VQ_A(ntry, wf4(:,3), wf6(:,3), wf24(:,26), n3(:,96), t3x24(:,:,26))
  call prop_A_Q(ntry, wf18(:,55), Q(:,100), MB, 1_intkind1, wf18(:,56), n2(45))
  call vert_AZ_Q(gZd,ntry, ex3, ex7, wf6(:,21), n3(:,97), t3x6(:,:,11))
  call prop_A_Q(ntry, wf6(:,21), Q(:,68), MB, 1_intkind1, wf6(:,22), n2(46))
  call vert_AV_Q(ntry, wf6(:,22), wf4(:,3), wf24(:,27), n3(:,98), t3x24(:,:,27))
  call vert_AZ_Q(gZd,ntry, wf6(:,22), ex6, wf18(:,57), n3(:,99), t3x18(:,:,29))
  call prop_A_Q(ntry, wf18(:,57), Q(:,100), MB, 1_intkind1, wf18(:,58), n2(47))
  call vert_AV_Q(ntry, ex3, wf4(:,3), wf8(:,9), n3(:,100), t3x8(:,:,5))
  call prop_A_Q(ntry, wf8(:,9), Q(:,13), MB, 1_intkind1, wf8(:,10), n2(48))
  call vert_SA_Q(gH,ntry, wf9(:,1), ex3, wf18(:,59), n3(:,101), t3x18(:,:,30))
  call prop_A_Q(ntry, wf18(:,59), Q(:,100), MB, 1_intkind1, wf18(:,60), n2(49))
  call vert_AZ_Q(gZd,ntry, wf8(:,10), ex7, wf24(:,28), n3(:,102), t3x24(:,:,28))
  call vert_AZ_Q(gZd,ntry, wf8(:,10), ex6, wf24(:,29), n3(:,103), t3x24(:,:,29))
  call vert_AZ_Q(gZd,ntry, ex3, ex5, wf6(:,23), n3(:,104), t3x6(:,:,12))
  call prop_A_Q(ntry, wf6(:,23), Q(:,20), MB, 1_intkind1, wf6(:,24), n2(50))
  call vert_AV_Q(ntry, wf6(:,24), wf4(:,3), wf24(:,30), n3(:,105), t3x24(:,:,30))
  call vert_AZ_Q(gZd,ntry, wf6(:,24), ex7, wf18(:,61), n3(:,106), t3x18(:,:,31))
  call vert_VQ_A(ntry, wf4(:,3), wf6(:,9), wf24(:,31), n3(:,107), t3x24(:,:,31))
  call prop_A_Q(ntry, wf18(:,61), Q(:,84), MB, 1_intkind1, wf18(:,62), n2(51))
  call vert_AZ_Q(gZd,ntry, wf6(:,24), ex6, wf18(:,63), n3(:,108), t3x18(:,:,32))
  call vert_VQ_A(ntry, wf4(:,3), wf6(:,12), wf24(:,32), n3(:,109), t3x24(:,:,32))
  call prop_A_Q(ntry, wf18(:,63), Q(:,52), MB, 1_intkind1, wf18(:,64), n2(52))
  call vert_VQ_A(ntry, wf4(:,3), ex2, wf8(:,11), n3(:,110), t3x8(:,:,6))
  call vert_SA_Q(gH,ntry, wf9(:,1), wf6(:,24), wf54(:,16), n3(:,111), t3x54(:,:,16))
  call prop_Q_A(ntry, wf8(:,11), Q(:,11), MB, 1_intkind1, wf8(:,12), n2(53))
  call vert_ZQ_A(gZd,ntry, ex6, wf8(:,12), wf24(:,33), n3(:,112), t3x24(:,:,33))
  call vert_ZQ_A(gZd,ntry, ex7, wf8(:,12), wf24(:,34), n3(:,113), t3x24(:,:,34))
  call vert_AZ_Q(gZd,ntry, wf6(:,22), ex5, wf18(:,65), n3(:,114), t3x18(:,:,33))
  call prop_A_Q(ntry, wf18(:,65), Q(:,84), MB, 1_intkind1, wf18(:,66), n2(54))
  call vert_SA_Q(gH,ntry, wf9(:,2), ex3, wf18(:,67), n3(:,115), t3x18(:,:,34))
  call prop_A_Q(ntry, wf18(:,67), Q(:,84), MB, 1_intkind1, wf18(:,68), n2(55))
  call vert_AZ_Q(gZd,ntry, wf8(:,10), ex5, wf24(:,35), n3(:,116), t3x24(:,:,35))
  call vert_AZ_Q(gZd,ntry, wf6(:,20), ex5, wf18(:,69), n3(:,117), t3x18(:,:,35))
  call prop_A_Q(ntry, wf18(:,69), Q(:,52), MB, 1_intkind1, wf18(:,70), n2(56))
  call vert_SA_Q(gH,ntry, wf9(:,2), wf6(:,20), wf54(:,17), n3(:,118), t3x54(:,:,17))
  call vert_ZQ_A(gZd,ntry, ex5, wf8(:,12), wf24(:,36), n3(:,119), t3x24(:,:,36))
  call vert_SA_Q(gH,ntry, wf9(:,3), ex3, wf18(:,71), n3(:,120), t3x18(:,:,36))
  call prop_A_Q(ntry, wf18(:,71), Q(:,52), MB, 1_intkind1, wf18(:,72), n2(57))
  call vert_SA_Q(gH,ntry, wf9(:,3), wf6(:,22), wf54(:,18), n3(:,121), t3x54(:,:,18))
  call vert_QA_Z(gZd,ntry, wf8(:,12), ex3, wf16(:,5), n3(:,122), t3x16(:,:,5))
  call vert_QA_Z(gZd,ntry, ex2, wf8(:,10), wf16(:,6), n3(:,123), t3x16(:,:,6))
  call vert_QA_V(ntry, ex2, ex4, wf4(:,4), n3(:,124), t3x4(:,:,4))
  call vert_AV_Q(ntry, wf6(:,20), wf4(:,4), wf24(:,37), n3(:,125), t3x24(:,:,37))
  call vert_VQ_A(ntry, wf4(:,4), wf6(:,14), wf24(:,38), n3(:,126), t3x24(:,:,38))
  call vert_AV_Q(ntry, wf6(:,22), wf4(:,4), wf24(:,39), n3(:,127), t3x24(:,:,39))
  call vert_AV_Q(ntry, ex3, wf4(:,4), wf8(:,13), n3(:,128), t3x8(:,:,7))
  call prop_A_Q(ntry, wf8(:,13), Q(:,14), MB, 1_intkind1, wf8(:,14), n2(58))
  call vert_AZ_Q(gZd,ntry, wf8(:,14), ex7, wf24(:,40), n3(:,129), t3x24(:,:,40))
  call vert_AZ_Q(gZd,ntry, wf8(:,14), ex6, wf24(:,41), n3(:,130), t3x24(:,:,41))
  call vert_AV_Q(ntry, wf6(:,24), wf4(:,4), wf24(:,42), n3(:,131), t3x24(:,:,42))
  call vert_VQ_A(ntry, wf4(:,4), wf6(:,16), wf24(:,43), n3(:,132), t3x24(:,:,43))
  call vert_VQ_A(ntry, wf4(:,4), wf6(:,18), wf24(:,44), n3(:,133), t3x24(:,:,44))
  call vert_VQ_A(ntry, wf4(:,4), ex1, wf8(:,15), n3(:,134), t3x8(:,:,8))
  call prop_Q_A(ntry, wf8(:,15), Q(:,11), MB, 1_intkind1, wf8(:,16), n2(59))
  call vert_ZQ_A(gZd,ntry, ex6, wf8(:,16), wf24(:,45), n3(:,135), t3x24(:,:,45))
  call vert_ZQ_A(gZd,ntry, ex7, wf8(:,16), wf24(:,46), n3(:,136), t3x24(:,:,46))
  call vert_AZ_Q(gZd,ntry, wf8(:,14), ex5, wf24(:,47), n3(:,137), t3x24(:,:,47))
  call vert_ZQ_A(gZd,ntry, ex5, wf8(:,16), wf24(:,48), n3(:,138), t3x24(:,:,48))
  call vert_QA_Z(gZd,ntry, wf8(:,16), ex3, wf16(:,7), n3(:,139), t3x16(:,:,7))
  call vert_QA_Z(gZd,ntry, ex1, wf8(:,14), wf16(:,8), n3(:,140), t3x16(:,:,8))
  call vert_QA_V(ntry, wf6(:,14), ex4, wf12(:,1), n3(:,141), t3x12(:,:,1))
  call vert_QA_V(ntry, wf6(:,9), wf6(:,22), wf36(:,1), n3(:,142), t3x36(:,:,1))
  call vert_QA_V(ntry, wf6(:,9), ex4, wf12(:,2), n3(:,143), t3x12(:,:,2))
  call vert_QA_V(ntry, wf6(:,14), wf6(:,22), wf36(:,2), n3(:,144), t3x36(:,:,2))
  call vert_QA_V(ntry, wf6(:,14), ex3, wf12(:,3), n3(:,145), t3x12(:,:,3))
  call vert_QA_V(ntry, wf6(:,9), wf6(:,6), wf36(:,3), n3(:,146), t3x36(:,:,3))
  call vert_QA_V(ntry, wf6(:,9), ex3, wf12(:,4), n3(:,147), t3x12(:,:,4))
  call vert_QA_V(ntry, wf6(:,14), wf6(:,6), wf36(:,4), n3(:,148), t3x36(:,:,4))
  call vert_AV_Q(ntry, ex4, wf12(:,3), wf24(:,49), n3(:,149), t3x24(:,:,49))
  call vert_AV_Q(ntry, ex3, wf12(:,1), wf24(:,50), n3(:,150), t3x24(:,:,50))
  call vert_AV_Q(ntry, ex4, wf12(:,4), wf24(:,51), n3(:,151), t3x24(:,:,51))
  call vert_AV_Q(ntry, ex3, wf12(:,2), wf24(:,52), n3(:,152), t3x24(:,:,52))
  call vert_QA_V(ntry, wf6(:,12), wf6(:,20), wf36(:,5), n3(:,153), t3x36(:,:,5))
  call vert_QA_V(ntry, wf6(:,12), ex4, wf12(:,5), n3(:,154), t3x12(:,:,5))
  call vert_QA_V(ntry, wf6(:,14), wf6(:,20), wf36(:,6), n3(:,155), t3x36(:,:,6))
  call vert_QA_V(ntry, ex2, wf6(:,20), wf12(:,6), n3(:,156), t3x12(:,:,6))
  call vert_QA_V(ntry, ex2, wf6(:,6), wf12(:,7), n3(:,157), t3x12(:,:,7))
  call vert_VQ_A(ntry, wf12(:,1), ex2, wf24(:,53), n3(:,158), t3x24(:,:,53))
  call vert_AV_Q(ntry, ex4, wf12(:,6), wf24(:,54), n3(:,159), t3x24(:,:,54))
  call vert_QA_V(ntry, wf6(:,12), wf6(:,4), wf36(:,7), n3(:,160), t3x36(:,:,7))
  call vert_QA_V(ntry, wf6(:,12), ex3, wf12(:,8), n3(:,161), t3x12(:,:,8))
  call vert_QA_V(ntry, wf6(:,14), wf6(:,4), wf36(:,8), n3(:,162), t3x36(:,:,8))
  call vert_QA_V(ntry, ex2, wf6(:,22), wf12(:,9), n3(:,163), t3x12(:,:,9))
  call vert_QA_V(ntry, ex2, wf6(:,4), wf12(:,10), n3(:,164), t3x12(:,:,10))
  call vert_VQ_A(ntry, wf12(:,3), ex2, wf24(:,55), n3(:,165), t3x24(:,:,55))
  call vert_AV_Q(ntry, ex3, wf12(:,10), wf24(:,56), n3(:,166), t3x24(:,:,56))
  call vert_AV_Q(ntry, ex4, wf12(:,8), wf24(:,57), n3(:,167), t3x24(:,:,57))
  call vert_AV_Q(ntry, ex3, wf12(:,5), wf24(:,58), n3(:,168), t3x24(:,:,58))
  call vert_AV_Q(ntry, ex4, wf12(:,9), wf24(:,59), n3(:,169), t3x24(:,:,59))
  call vert_AV_Q(ntry, ex3, wf12(:,7), wf24(:,60), n3(:,170), t3x24(:,:,60))
  call vert_QA_V(ntry, wf18(:,20), ex4, wf36(:,9), n3(:,171), t3x36(:,:,9))
  call vert_QA_V(ntry, ex2, wf18(:,60), wf36(:,10), n3(:,172), t3x36(:,:,10))
  call vert_QA_V(ntry, wf18(:,20), ex3, wf36(:,11), n3(:,173), t3x36(:,:,11))
  call vert_QA_V(ntry, wf6(:,16), ex4, wf12(:,11), n3(:,174), t3x12(:,:,11))
  call vert_QA_V(ntry, wf6(:,3), wf6(:,22), wf36(:,12), n3(:,175), t3x36(:,:,12))
  call vert_QA_V(ntry, wf6(:,3), ex4, wf12(:,12), n3(:,176), t3x12(:,:,12))
  call vert_QA_V(ntry, wf6(:,16), wf6(:,22), wf36(:,13), n3(:,177), t3x36(:,:,13))
  call vert_QA_V(ntry, wf6(:,16), ex3, wf12(:,13), n3(:,178), t3x12(:,:,13))
  call vert_QA_V(ntry, wf6(:,3), wf6(:,6), wf36(:,14), n3(:,179), t3x36(:,:,14))
  call vert_QA_V(ntry, wf6(:,3), ex3, wf12(:,14), n3(:,180), t3x12(:,:,14))
  call vert_QA_V(ntry, wf6(:,16), wf6(:,6), wf36(:,15), n3(:,181), t3x36(:,:,15))
  call vert_AV_Q(ntry, ex4, wf12(:,13), wf24(:,61), n3(:,182), t3x24(:,:,61))
  call vert_AV_Q(ntry, ex3, wf12(:,11), wf24(:,62), n3(:,183), t3x24(:,:,62))
  call vert_AV_Q(ntry, ex4, wf12(:,14), wf24(:,63), n3(:,184), t3x24(:,:,63))
  call vert_AV_Q(ntry, ex3, wf12(:,12), wf24(:,64), n3(:,185), t3x24(:,:,64))
  call vert_QA_V(ntry, wf6(:,18), ex4, wf12(:,15), n3(:,186), t3x12(:,:,15))
  call vert_QA_V(ntry, wf6(:,3), wf6(:,20), wf36(:,16), n3(:,187), t3x36(:,:,16))
  call vert_QA_V(ntry, wf6(:,18), wf6(:,20), wf36(:,17), n3(:,188), t3x36(:,:,17))
  call vert_QA_V(ntry, ex1, wf6(:,20), wf12(:,16), n3(:,189), t3x12(:,:,16))
  call vert_QA_V(ntry, ex1, wf6(:,6), wf12(:,17), n3(:,190), t3x12(:,:,17))
  call vert_AV_Q(ntry, ex4, wf12(:,16), wf24(:,65), n3(:,191), t3x24(:,:,65))
  call vert_VQ_A(ntry, wf12(:,12), ex1, wf24(:,66), n3(:,192), t3x24(:,:,66))
  call vert_QA_V(ntry, wf6(:,18), ex3, wf12(:,18), n3(:,193), t3x12(:,:,18))
  call vert_QA_V(ntry, wf6(:,3), wf6(:,4), wf36(:,18), n3(:,194), t3x36(:,:,18))
  call vert_QA_V(ntry, wf6(:,18), wf6(:,4), wf36(:,19), n3(:,195), t3x36(:,:,19))
  call vert_QA_V(ntry, ex1, wf6(:,22), wf12(:,19), n3(:,196), t3x12(:,:,19))
  call vert_QA_V(ntry, ex1, wf6(:,4), wf12(:,20), n3(:,197), t3x12(:,:,20))
  call vert_AV_Q(ntry, ex3, wf12(:,20), wf24(:,67), n3(:,198), t3x24(:,:,67))
  call vert_VQ_A(ntry, wf12(:,14), ex1, wf24(:,68), n3(:,199), t3x24(:,:,68))
  call vert_AV_Q(ntry, ex4, wf12(:,18), wf24(:,69), n3(:,200), t3x24(:,:,69))
  call vert_AV_Q(ntry, ex3, wf12(:,15), wf24(:,70), n3(:,201), t3x24(:,:,70))
  call vert_AV_Q(ntry, ex4, wf12(:,19), wf24(:,71), n3(:,202), t3x24(:,:,71))
  call vert_AV_Q(ntry, ex3, wf12(:,17), wf24(:,72), n3(:,203), t3x24(:,:,72))
  call vert_QA_V(ntry, ex1, wf18(:,60), wf36(:,20), n3(:,204), t3x36(:,:,20))
  call vert_QA_V(ntry, wf18(:,46), ex3, wf36(:,21), n3(:,205), t3x36(:,:,21))
  call vert_QA_V(ntry, wf18(:,46), ex4, wf36(:,22), n3(:,206), t3x36(:,:,22))
  call vert_QA_V(ntry, wf6(:,12), wf6(:,24), wf36(:,23), n3(:,207), t3x36(:,:,23))
  call vert_QA_V(ntry, wf6(:,16), wf6(:,24), wf36(:,24), n3(:,208), t3x36(:,:,24))
  call vert_QA_V(ntry, ex2, wf6(:,24), wf12(:,21), n3(:,209), t3x12(:,:,21))
  call vert_VQ_A(ntry, wf12(:,11), ex2, wf24(:,73), n3(:,210), t3x24(:,:,73))
  call vert_AV_Q(ntry, ex4, wf12(:,21), wf24(:,74), n3(:,211), t3x24(:,:,74))
  call vert_QA_V(ntry, wf6(:,9), wf6(:,24), wf36(:,25), n3(:,212), t3x36(:,:,25))
  call vert_QA_V(ntry, wf6(:,18), wf6(:,24), wf36(:,26), n3(:,213), t3x36(:,:,26))
  call vert_QA_V(ntry, ex1, wf6(:,24), wf12(:,22), n3(:,214), t3x12(:,:,22))
  call vert_AV_Q(ntry, ex4, wf12(:,22), wf24(:,75), n3(:,215), t3x24(:,:,75))
  call vert_VQ_A(ntry, wf12(:,2), ex1, wf24(:,76), n3(:,216), t3x24(:,:,76))
  call vert_VQ_A(ntry, wf12(:,22), ex2, wf24(:,77), n3(:,217), t3x24(:,:,77))
  call vert_VQ_A(ntry, wf12(:,20), ex2, wf24(:,78), n3(:,218), t3x24(:,:,78))
  call vert_VQ_A(ntry, wf12(:,21), ex1, wf24(:,79), n3(:,219), t3x24(:,:,79))
  call vert_VQ_A(ntry, wf12(:,10), ex1, wf24(:,80), n3(:,220), t3x24(:,:,80))
  call vert_VQ_A(ntry, wf12(:,15), ex2, wf24(:,81), n3(:,221), t3x24(:,:,81))
  call vert_VQ_A(ntry, wf12(:,5), ex1, wf24(:,82), n3(:,222), t3x24(:,:,82))
  call vert_VQ_A(ntry, wf12(:,17), ex2, wf24(:,83), n3(:,223), t3x24(:,:,83))
  call vert_VQ_A(ntry, wf12(:,7), ex1, wf24(:,84), n3(:,224), t3x24(:,:,84))
  call vert_QA_V(ntry, wf6(:,12), wf6(:,10), wf36(:,27), n3(:,225), t3x36(:,:,27))
  call vert_QA_V(ntry, wf6(:,16), wf6(:,10), wf36(:,28), n3(:,226), t3x36(:,:,28))
  call vert_QA_V(ntry, ex2, wf6(:,10), wf12(:,23), n3(:,227), t3x12(:,:,23))
  call vert_VQ_A(ntry, wf12(:,13), ex2, wf24(:,85), n3(:,228), t3x24(:,:,85))
  call vert_AV_Q(ntry, ex3, wf12(:,23), wf24(:,86), n3(:,229), t3x24(:,:,86))
  call vert_QA_V(ntry, wf6(:,9), wf6(:,10), wf36(:,29), n3(:,230), t3x36(:,:,29))
  call vert_QA_V(ntry, wf6(:,18), wf6(:,10), wf36(:,30), n3(:,231), t3x36(:,:,30))
  call vert_QA_V(ntry, ex1, wf6(:,10), wf12(:,24), n3(:,232), t3x12(:,:,24))
  call vert_AV_Q(ntry, ex3, wf12(:,24), wf24(:,87), n3(:,233), t3x24(:,:,87))
  call vert_VQ_A(ntry, wf12(:,4), ex1, wf24(:,88), n3(:,234), t3x24(:,:,88))
  call vert_VQ_A(ntry, wf12(:,16), ex2, wf24(:,89), n3(:,235), t3x24(:,:,89))
  call vert_VQ_A(ntry, wf12(:,24), ex2, wf24(:,90), n3(:,236), t3x24(:,:,90))
  call vert_VQ_A(ntry, wf12(:,6), ex1, wf24(:,91), n3(:,237), t3x24(:,:,91))
  call vert_VQ_A(ntry, wf12(:,23), ex1, wf24(:,92), n3(:,238), t3x24(:,:,92))
  call vert_VQ_A(ntry, wf12(:,18), ex2, wf24(:,93), n3(:,239), t3x24(:,:,93))
  call vert_VQ_A(ntry, wf12(:,8), ex1, wf24(:,94), n3(:,240), t3x24(:,:,94))
  call vert_VQ_A(ntry, wf12(:,19), ex2, wf24(:,95), n3(:,241), t3x24(:,:,95))
  call vert_VQ_A(ntry, wf12(:,9), ex1, wf24(:,96), n3(:,242), t3x24(:,:,96))
  call vert_QA_V(ntry, wf18(:,32), ex4, wf36(:,31), n3(:,243), t3x36(:,:,31))
  call vert_QA_V(ntry, ex2, wf18(:,68), wf36(:,32), n3(:,244), t3x36(:,:,32))
  call vert_QA_V(ntry, wf18(:,32), ex3, wf36(:,33), n3(:,245), t3x36(:,:,33))
  call vert_QA_V(ntry, ex1, wf18(:,68), wf36(:,34), n3(:,246), t3x36(:,:,34))
  call vert_QA_V(ntry, wf18(:,52), ex3, wf36(:,35), n3(:,247), t3x36(:,:,35))
  call vert_QA_V(ntry, wf18(:,52), ex4, wf36(:,36), n3(:,248), t3x36(:,:,36))
  call vert_QA_V(ntry, wf18(:,36), ex4, wf36(:,37), n3(:,249), t3x36(:,:,37))
  call vert_QA_V(ntry, ex2, wf18(:,72), wf36(:,38), n3(:,250), t3x36(:,:,38))
  call vert_QA_V(ntry, wf18(:,36), ex3, wf36(:,39), n3(:,251), t3x36(:,:,39))
  call vert_QA_V(ntry, ex1, wf18(:,72), wf36(:,40), n3(:,252), t3x36(:,:,40))
  call vert_QA_V(ntry, wf18(:,54), ex3, wf36(:,41), n3(:,253), t3x36(:,:,41))
  call vert_QA_V(ntry, wf18(:,54), ex4, wf36(:,42), n3(:,254), t3x36(:,:,42))


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
  M2add = M2 / average_factor_ppvvvj_bbbxbxzzz_1

  do k = 0, 30-1
    M2(k) = M2add(extcomb_perm_ppvvvj_bbbxbxzzz_1(k))
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

    call cont_QA(nsync, wf24(:,1), wf18(:,2), A(:,1), n3(:,255), t3x432(:,:,1), nhel, den(7))
    call cont_QA(nsync, wf24(:,2), wf18(:,4), A(:,2), n3(:,256), t3x432(:,:,2), nhel, den(11))
    call cont_QA(nsync, wf24(:,3), wf18(:,6), A(:,3), n3(:,257), t3x432(:,:,3), nhel, den(16))
    call cont_QA(nsync, wf24(:,2), wf18(:,8), A(:,4), n3(:,258), t3x432(:,:,4), nhel, den(18))
    call cont_QA(nsync, wf54(:,1), wf8(:,2), A(:,5), n3(:,259), t3x432(:,:,5), nhel, den(23))
    call cont_QA(nsync, wf24(:,2), wf18(:,10), A(:,6), n3(:,260), t3x432(:,:,6), nhel, den(25))
    call cont_QA(nsync, wf18(:,6), wf24(:,4), A(:,7), n3(:,261), t3x432(:,:,7), nhel, den(26))
    call cont_QA(nsync, wf18(:,2), wf24(:,5), A(:,8), n3(:,262), t3x432(:,:,8), nhel, den(27))
    call cont_QA(nsync, wf24(:,6), wf18(:,12), A(:,9), n3(:,263), t3x432(:,:,9), nhel, den(33))
    call cont_QA(nsync, wf24(:,7), wf18(:,14), A(:,10), n3(:,264), t3x432(:,:,10), nhel, den(37))
    call cont_QA(nsync, wf24(:,6), wf18(:,16), A(:,11), n3(:,265), t3x432(:,:,11), nhel, den(40))
    call cont_QA(nsync, wf24(:,8), wf18(:,18), A(:,12), n3(:,266), t3x432(:,:,12), nhel, den(44))
    call cont_QA(nsync, wf54(:,2), wf8(:,4), A(:,13), n3(:,267), t3x432(:,:,13), nhel, den(48))
    call cont_QA(nsync, wf24(:,6), wf18(:,20), A(:,14), n3(:,268), t3x432(:,:,14), nhel, den(50))
    call cont_QA(nsync, wf18(:,14), wf24(:,9), A(:,15), n3(:,269), t3x432(:,:,15), nhel, den(51))
    call cont_QA(nsync, wf18(:,18), wf24(:,10), A(:,16), n3(:,270), t3x432(:,:,16), nhel, den(52))
    call cont_QA(nsync, wf24(:,3), wf18(:,22), A(:,17), n3(:,271), t3x432(:,:,17), nhel, den(54))
    call cont_QA(nsync, wf24(:,7), wf18(:,24), A(:,18), n3(:,272), t3x432(:,:,18), nhel, den(56))
    call cont_QA(nsync, wf8(:,2), wf54(:,3), A(:,19), n3(:,273), t3x432(:,:,19), nhel, den(59))
    call cont_QA(nsync, wf24(:,7), wf18(:,26), A(:,20), n3(:,274), t3x432(:,:,20), nhel, den(61))
    call cont_QA(nsync, wf24(:,4), wf18(:,22), A(:,21), n3(:,275), t3x432(:,:,21), nhel, den(62))
    call cont_QA(nsync, wf18(:,12), wf24(:,11), A(:,22), n3(:,276), t3x432(:,:,22), nhel, den(63))
    call cont_QA(nsync, wf24(:,1), wf18(:,28), A(:,23), n3(:,277), t3x432(:,:,23), nhel, den(65))
    call cont_QA(nsync, wf24(:,8), wf18(:,30), A(:,24), n3(:,278), t3x432(:,:,24), nhel, den(67))
    call cont_QA(nsync, wf8(:,4), wf54(:,4), A(:,25), n3(:,279), t3x432(:,:,25), nhel, den(69))
    call cont_QA(nsync, wf24(:,1), wf18(:,32), A(:,26), n3(:,280), t3x432(:,:,26), nhel, den(71))
    call cont_QA(nsync, wf18(:,4), wf24(:,12), A(:,27), n3(:,281), t3x432(:,:,27), nhel, den(72))
    call cont_QA(nsync, wf24(:,10), wf18(:,30), A(:,28), n3(:,282), t3x432(:,:,28), nhel, den(73))
    call cont_QA(nsync, wf8(:,2), wf54(:,5), A(:,29), n3(:,283), t3x432(:,:,29), nhel, den(76))
    call cont_QA(nsync, wf24(:,8), wf18(:,34), A(:,30), n3(:,284), t3x432(:,:,30), nhel, den(78))
    call cont_QA(nsync, wf8(:,4), wf54(:,6), A(:,31), n3(:,285), t3x432(:,:,31), nhel, den(80))
    call cont_QA(nsync, wf24(:,3), wf18(:,36), A(:,32), n3(:,286), t3x432(:,:,32), nhel, den(82))
    call cont_VV(nsync, wf27(:,2), wf16(:,1), A(:,33), n3(:,287), t3x432(:,:,33), nhel, den(85))
    call cont_QA(nsync, wf24(:,10), wf18(:,34), A(:,34), n3(:,288), t3x432(:,:,34), nhel, den(86))
    call cont_VV(nsync, wf27(:,2), wf16(:,2), A(:,35), n3(:,289), t3x432(:,:,35), nhel, den(87))
    call cont_QA(nsync, wf8(:,2), wf54(:,7), A(:,36), n3(:,290), t3x432(:,:,36), nhel, den(88))
    call cont_QA(nsync, wf24(:,5), wf18(:,28), A(:,37), n3(:,291), t3x432(:,:,37), nhel, den(89))
    call cont_QA(nsync, wf18(:,16), wf24(:,11), A(:,38), n3(:,292), t3x432(:,:,38), nhel, den(90))
    call cont_QA(nsync, wf18(:,8), wf24(:,12), A(:,39), n3(:,293), t3x432(:,:,39), nhel, den(91))
    call cont_QA(nsync, wf24(:,9), wf18(:,24), A(:,40), n3(:,294), t3x432(:,:,40), nhel, den(92))
    call cont_VV(nsync, wf16(:,1), wf27(:,4), A(:,41), n3(:,295), t3x432(:,:,41), nhel, den(94))
    call cont_QA(nsync, wf24(:,9), wf18(:,26), A(:,42), n3(:,296), t3x432(:,:,42), nhel, den(95))
    call cont_VV(nsync, wf16(:,2), wf27(:,4), A(:,43), n3(:,297), t3x432(:,:,43), nhel, den(96))
    call cont_QA(nsync, wf8(:,2), wf54(:,8), A(:,44), n3(:,298), t3x432(:,:,44), nhel, den(97))
    call cont_VV(nsync, wf16(:,1), wf27(:,6), A(:,45), n3(:,299), t3x432(:,:,45), nhel, den(99))
    call cont_QA(nsync, wf18(:,10), wf24(:,12), A(:,46), n3(:,300), t3x432(:,:,46), nhel, den(100))
    call cont_VV(nsync, wf16(:,2), wf27(:,6), A(:,47), n3(:,301), t3x432(:,:,47), nhel, den(101))
    call cont_QA(nsync, wf8(:,2), wf54(:,9), A(:,48), n3(:,302), t3x432(:,:,48), nhel, den(102))
    call cont_QA(nsync, wf24(:,13), wf18(:,38), A(:,49), n3(:,303), t3x432(:,:,49), nhel, den(108))
    call cont_QA(nsync, wf18(:,4), wf24(:,14), A(:,50), n3(:,304), t3x432(:,:,50), nhel, den(110))
    call cont_QA(nsync, wf24(:,15), wf18(:,40), A(:,51), n3(:,305), t3x432(:,:,51), nhel, den(114))
    call cont_QA(nsync, wf18(:,8), wf24(:,14), A(:,52), n3(:,306), t3x432(:,:,52), nhel, den(115))
    call cont_QA(nsync, wf54(:,10), wf8(:,6), A(:,53), n3(:,307), t3x432(:,:,53), nhel, den(119))
    call cont_QA(nsync, wf18(:,10), wf24(:,14), A(:,54), n3(:,308), t3x432(:,:,54), nhel, den(120))
    call cont_QA(nsync, wf18(:,40), wf24(:,16), A(:,55), n3(:,309), t3x432(:,:,55), nhel, den(121))
    call cont_QA(nsync, wf18(:,38), wf24(:,17), A(:,56), n3(:,310), t3x432(:,:,56), nhel, den(122))
    call cont_QA(nsync, wf24(:,18), wf18(:,42), A(:,57), n3(:,311), t3x432(:,:,57), nhel, den(127))
    call cont_QA(nsync, wf18(:,14), wf24(:,19), A(:,58), n3(:,312), t3x432(:,:,58), nhel, den(129))
    call cont_QA(nsync, wf24(:,18), wf18(:,44), A(:,59), n3(:,313), t3x432(:,:,59), nhel, den(132))
    call cont_QA(nsync, wf18(:,18), wf24(:,20), A(:,60), n3(:,314), t3x432(:,:,60), nhel, den(134))
    call cont_QA(nsync, wf54(:,2), wf8(:,8), A(:,61), n3(:,315), t3x432(:,:,61), nhel, den(136))
    call cont_QA(nsync, wf24(:,18), wf18(:,46), A(:,62), n3(:,316), t3x432(:,:,62), nhel, den(138))
    call cont_QA(nsync, wf18(:,14), wf24(:,21), A(:,63), n3(:,317), t3x432(:,:,63), nhel, den(139))
    call cont_QA(nsync, wf18(:,18), wf24(:,22), A(:,64), n3(:,318), t3x432(:,:,64), nhel, den(140))
    call cont_QA(nsync, wf24(:,15), wf18(:,48), A(:,65), n3(:,319), t3x432(:,:,65), nhel, den(142))
    call cont_QA(nsync, wf18(:,24), wf24(:,19), A(:,66), n3(:,320), t3x432(:,:,66), nhel, den(143))
    call cont_QA(nsync, wf8(:,6), wf54(:,11), A(:,67), n3(:,321), t3x432(:,:,67), nhel, den(145))
    call cont_QA(nsync, wf18(:,26), wf24(:,19), A(:,68), n3(:,322), t3x432(:,:,68), nhel, den(146))
    call cont_QA(nsync, wf24(:,16), wf18(:,48), A(:,69), n3(:,323), t3x432(:,:,69), nhel, den(147))
    call cont_QA(nsync, wf18(:,42), wf24(:,23), A(:,70), n3(:,324), t3x432(:,:,70), nhel, den(148))
    call cont_QA(nsync, wf24(:,13), wf18(:,50), A(:,71), n3(:,325), t3x432(:,:,71), nhel, den(150))
    call cont_QA(nsync, wf18(:,30), wf24(:,20), A(:,72), n3(:,326), t3x432(:,:,72), nhel, den(151))
    call cont_QA(nsync, wf54(:,4), wf8(:,8), A(:,73), n3(:,327), t3x432(:,:,73), nhel, den(152))
    call cont_QA(nsync, wf24(:,13), wf18(:,52), A(:,74), n3(:,328), t3x432(:,:,74), nhel, den(154))
    call cont_QA(nsync, wf18(:,4), wf24(:,24), A(:,75), n3(:,329), t3x432(:,:,75), nhel, den(155))
    call cont_QA(nsync, wf18(:,30), wf24(:,22), A(:,76), n3(:,330), t3x432(:,:,76), nhel, den(156))
    call cont_QA(nsync, wf8(:,6), wf54(:,12), A(:,77), n3(:,331), t3x432(:,:,77), nhel, den(158))
    call cont_QA(nsync, wf18(:,34), wf24(:,20), A(:,78), n3(:,332), t3x432(:,:,78), nhel, den(159))
    call cont_QA(nsync, wf54(:,6), wf8(:,8), A(:,79), n3(:,333), t3x432(:,:,79), nhel, den(160))
    call cont_QA(nsync, wf24(:,15), wf18(:,54), A(:,80), n3(:,334), t3x432(:,:,80), nhel, den(162))
    call cont_VV(nsync, wf27(:,2), wf16(:,3), A(:,81), n3(:,335), t3x432(:,:,81), nhel, den(163))
    call cont_QA(nsync, wf18(:,34), wf24(:,22), A(:,82), n3(:,336), t3x432(:,:,82), nhel, den(164))
    call cont_VV(nsync, wf27(:,2), wf16(:,4), A(:,83), n3(:,337), t3x432(:,:,83), nhel, den(165))
    call cont_QA(nsync, wf8(:,6), wf54(:,13), A(:,84), n3(:,338), t3x432(:,:,84), nhel, den(166))
    call cont_QA(nsync, wf24(:,17), wf18(:,50), A(:,85), n3(:,339), t3x432(:,:,85), nhel, den(167))
    call cont_QA(nsync, wf18(:,44), wf24(:,23), A(:,86), n3(:,340), t3x432(:,:,86), nhel, den(168))
    call cont_QA(nsync, wf18(:,8), wf24(:,24), A(:,87), n3(:,341), t3x432(:,:,87), nhel, den(169))
    call cont_QA(nsync, wf18(:,24), wf24(:,21), A(:,88), n3(:,342), t3x432(:,:,88), nhel, den(170))
    call cont_VV(nsync, wf27(:,4), wf16(:,3), A(:,89), n3(:,343), t3x432(:,:,89), nhel, den(171))
    call cont_QA(nsync, wf18(:,26), wf24(:,21), A(:,90), n3(:,344), t3x432(:,:,90), nhel, den(172))
    call cont_VV(nsync, wf27(:,4), wf16(:,4), A(:,91), n3(:,345), t3x432(:,:,91), nhel, den(173))
    call cont_QA(nsync, wf8(:,6), wf54(:,14), A(:,92), n3(:,346), t3x432(:,:,92), nhel, den(174))
    call cont_VV(nsync, wf27(:,6), wf16(:,3), A(:,93), n3(:,347), t3x432(:,:,93), nhel, den(175))
    call cont_QA(nsync, wf18(:,10), wf24(:,24), A(:,94), n3(:,348), t3x432(:,:,94), nhel, den(176))
    call cont_VV(nsync, wf27(:,6), wf16(:,4), A(:,95), n3(:,349), t3x432(:,:,95), nhel, den(177))
    call cont_QA(nsync, wf8(:,6), wf54(:,15), A(:,96), n3(:,350), t3x432(:,:,96), nhel, den(178))
    call cont_QA(nsync, wf18(:,2), wf24(:,25), A(:,97), n3(:,351), t3x432(:,:,97), nhel, den(182))
    call cont_QA(nsync, wf24(:,26), wf18(:,56), A(:,98), n3(:,352), t3x432(:,:,98), nhel, den(186))
    call cont_QA(nsync, wf18(:,6), wf24(:,27), A(:,99), n3(:,353), t3x432(:,:,99), nhel, den(189))
    call cont_QA(nsync, wf24(:,26), wf18(:,58), A(:,100), n3(:,354), t3x432(:,:,100), nhel, den(191))
    call cont_QA(nsync, wf54(:,1), wf8(:,10), A(:,101), n3(:,355), t3x432(:,:,101), nhel, den(193))
    call cont_QA(nsync, wf24(:,26), wf18(:,60), A(:,102), n3(:,356), t3x432(:,:,102), nhel, den(195))
    call cont_QA(nsync, wf18(:,6), wf24(:,28), A(:,103), n3(:,357), t3x432(:,:,103), nhel, den(196))
    call cont_QA(nsync, wf18(:,2), wf24(:,29), A(:,104), n3(:,358), t3x432(:,:,104), nhel, den(197))
    call cont_QA(nsync, wf18(:,12), wf24(:,30), A(:,105), n3(:,359), t3x432(:,:,105), nhel, den(200))
    call cont_QA(nsync, wf24(:,31), wf18(:,62), A(:,106), n3(:,360), t3x432(:,:,106), nhel, den(204))
    call cont_QA(nsync, wf18(:,16), wf24(:,30), A(:,107), n3(:,361), t3x432(:,:,107), nhel, den(205))
    call cont_QA(nsync, wf24(:,32), wf18(:,64), A(:,108), n3(:,362), t3x432(:,:,108), nhel, den(209))
    call cont_QA(nsync, wf54(:,16), wf8(:,12), A(:,109), n3(:,363), t3x432(:,:,109), nhel, den(213))
    call cont_QA(nsync, wf18(:,20), wf24(:,30), A(:,110), n3(:,364), t3x432(:,:,110), nhel, den(214))
    call cont_QA(nsync, wf18(:,62), wf24(:,33), A(:,111), n3(:,365), t3x432(:,:,111), nhel, den(215))
    call cont_QA(nsync, wf18(:,64), wf24(:,34), A(:,112), n3(:,366), t3x432(:,:,112), nhel, den(216))
    call cont_QA(nsync, wf18(:,22), wf24(:,27), A(:,113), n3(:,367), t3x432(:,:,113), nhel, den(217))
    call cont_QA(nsync, wf24(:,31), wf18(:,66), A(:,114), n3(:,368), t3x432(:,:,114), nhel, den(219))
    call cont_QA(nsync, wf54(:,3), wf8(:,10), A(:,115), n3(:,369), t3x432(:,:,115), nhel, den(220))
    call cont_QA(nsync, wf24(:,31), wf18(:,68), A(:,116), n3(:,370), t3x432(:,:,116), nhel, den(222))
    call cont_QA(nsync, wf18(:,22), wf24(:,28), A(:,117), n3(:,371), t3x432(:,:,117), nhel, den(223))
    call cont_QA(nsync, wf18(:,12), wf24(:,35), A(:,118), n3(:,372), t3x432(:,:,118), nhel, den(224))
    call cont_QA(nsync, wf18(:,28), wf24(:,25), A(:,119), n3(:,373), t3x432(:,:,119), nhel, den(225))
    call cont_QA(nsync, wf24(:,32), wf18(:,70), A(:,120), n3(:,374), t3x432(:,:,120), nhel, den(227))
    call cont_QA(nsync, wf8(:,12), wf54(:,17), A(:,121), n3(:,375), t3x432(:,:,121), nhel, den(229))
    call cont_QA(nsync, wf18(:,32), wf24(:,25), A(:,122), n3(:,376), t3x432(:,:,122), nhel, den(230))
    call cont_QA(nsync, wf18(:,56), wf24(:,36), A(:,123), n3(:,377), t3x432(:,:,123), nhel, den(231))
    call cont_QA(nsync, wf24(:,34), wf18(:,70), A(:,124), n3(:,378), t3x432(:,:,124), nhel, den(232))
    call cont_QA(nsync, wf54(:,5), wf8(:,10), A(:,125), n3(:,379), t3x432(:,:,125), nhel, den(233))
    call cont_QA(nsync, wf24(:,32), wf18(:,72), A(:,126), n3(:,380), t3x432(:,:,126), nhel, den(235))
    call cont_QA(nsync, wf8(:,12), wf54(:,18), A(:,127), n3(:,381), t3x432(:,:,127), nhel, den(237))
    call cont_QA(nsync, wf18(:,36), wf24(:,27), A(:,128), n3(:,382), t3x432(:,:,128), nhel, den(238))
    call cont_VV(nsync, wf27(:,2), wf16(:,5), A(:,129), n3(:,383), t3x432(:,:,129), nhel, den(239))
    call cont_QA(nsync, wf24(:,34), wf18(:,72), A(:,130), n3(:,384), t3x432(:,:,130), nhel, den(240))
    call cont_VV(nsync, wf27(:,2), wf16(:,6), A(:,131), n3(:,385), t3x432(:,:,131), nhel, den(241))
    call cont_QA(nsync, wf54(:,7), wf8(:,10), A(:,132), n3(:,386), t3x432(:,:,132), nhel, den(242))
    call cont_QA(nsync, wf18(:,28), wf24(:,29), A(:,133), n3(:,387), t3x432(:,:,133), nhel, den(243))
    call cont_QA(nsync, wf18(:,16), wf24(:,35), A(:,134), n3(:,388), t3x432(:,:,134), nhel, den(244))
    call cont_QA(nsync, wf18(:,58), wf24(:,36), A(:,135), n3(:,389), t3x432(:,:,135), nhel, den(245))
    call cont_QA(nsync, wf24(:,33), wf18(:,66), A(:,136), n3(:,390), t3x432(:,:,136), nhel, den(246))
    call cont_VV(nsync, wf27(:,4), wf16(:,5), A(:,137), n3(:,391), t3x432(:,:,137), nhel, den(247))
    call cont_QA(nsync, wf24(:,33), wf18(:,68), A(:,138), n3(:,392), t3x432(:,:,138), nhel, den(248))
    call cont_VV(nsync, wf27(:,4), wf16(:,6), A(:,139), n3(:,393), t3x432(:,:,139), nhel, den(249))
    call cont_QA(nsync, wf54(:,8), wf8(:,10), A(:,140), n3(:,394), t3x432(:,:,140), nhel, den(250))
    call cont_VV(nsync, wf27(:,6), wf16(:,5), A(:,141), n3(:,395), t3x432(:,:,141), nhel, den(251))
    call cont_QA(nsync, wf18(:,60), wf24(:,36), A(:,142), n3(:,396), t3x432(:,:,142), nhel, den(252))
    call cont_VV(nsync, wf27(:,6), wf16(:,6), A(:,143), n3(:,397), t3x432(:,:,143), nhel, den(253))
    call cont_QA(nsync, wf54(:,9), wf8(:,10), A(:,144), n3(:,398), t3x432(:,:,144), nhel, den(254))
    call cont_QA(nsync, wf18(:,38), wf24(:,37), A(:,145), n3(:,399), t3x432(:,:,145), nhel, den(257))
    call cont_QA(nsync, wf18(:,56), wf24(:,38), A(:,146), n3(:,400), t3x432(:,:,146), nhel, den(259))
    call cont_QA(nsync, wf18(:,40), wf24(:,39), A(:,147), n3(:,401), t3x432(:,:,147), nhel, den(261))
    call cont_QA(nsync, wf18(:,58), wf24(:,38), A(:,148), n3(:,402), t3x432(:,:,148), nhel, den(262))
    call cont_QA(nsync, wf54(:,10), wf8(:,14), A(:,149), n3(:,403), t3x432(:,:,149), nhel, den(264))
    call cont_QA(nsync, wf18(:,60), wf24(:,38), A(:,150), n3(:,404), t3x432(:,:,150), nhel, den(265))
    call cont_QA(nsync, wf18(:,40), wf24(:,40), A(:,151), n3(:,405), t3x432(:,:,151), nhel, den(266))
    call cont_QA(nsync, wf18(:,38), wf24(:,41), A(:,152), n3(:,406), t3x432(:,:,152), nhel, den(267))
    call cont_QA(nsync, wf18(:,42), wf24(:,42), A(:,153), n3(:,407), t3x432(:,:,153), nhel, den(269))
    call cont_QA(nsync, wf18(:,62), wf24(:,43), A(:,154), n3(:,408), t3x432(:,:,154), nhel, den(271))
    call cont_QA(nsync, wf18(:,44), wf24(:,42), A(:,155), n3(:,409), t3x432(:,:,155), nhel, den(272))
    call cont_QA(nsync, wf18(:,64), wf24(:,44), A(:,156), n3(:,410), t3x432(:,:,156), nhel, den(274))
    call cont_QA(nsync, wf54(:,16), wf8(:,16), A(:,157), n3(:,411), t3x432(:,:,157), nhel, den(276))
    call cont_QA(nsync, wf18(:,46), wf24(:,42), A(:,158), n3(:,412), t3x432(:,:,158), nhel, den(277))
    call cont_QA(nsync, wf18(:,62), wf24(:,45), A(:,159), n3(:,413), t3x432(:,:,159), nhel, den(278))
    call cont_QA(nsync, wf18(:,64), wf24(:,46), A(:,160), n3(:,414), t3x432(:,:,160), nhel, den(279))
    call cont_QA(nsync, wf18(:,48), wf24(:,39), A(:,161), n3(:,415), t3x432(:,:,161), nhel, den(280))
    call cont_QA(nsync, wf18(:,66), wf24(:,43), A(:,162), n3(:,416), t3x432(:,:,162), nhel, den(281))
    call cont_QA(nsync, wf54(:,11), wf8(:,14), A(:,163), n3(:,417), t3x432(:,:,163), nhel, den(282))
    call cont_QA(nsync, wf18(:,68), wf24(:,43), A(:,164), n3(:,418), t3x432(:,:,164), nhel, den(283))
    call cont_QA(nsync, wf18(:,48), wf24(:,40), A(:,165), n3(:,419), t3x432(:,:,165), nhel, den(284))
    call cont_QA(nsync, wf18(:,42), wf24(:,47), A(:,166), n3(:,420), t3x432(:,:,166), nhel, den(285))
    call cont_QA(nsync, wf18(:,50), wf24(:,37), A(:,167), n3(:,421), t3x432(:,:,167), nhel, den(286))
    call cont_QA(nsync, wf18(:,70), wf24(:,44), A(:,168), n3(:,422), t3x432(:,:,168), nhel, den(287))
    call cont_QA(nsync, wf54(:,17), wf8(:,16), A(:,169), n3(:,423), t3x432(:,:,169), nhel, den(288))
    call cont_QA(nsync, wf18(:,52), wf24(:,37), A(:,170), n3(:,424), t3x432(:,:,170), nhel, den(289))
    call cont_QA(nsync, wf18(:,56), wf24(:,48), A(:,171), n3(:,425), t3x432(:,:,171), nhel, den(290))
    call cont_QA(nsync, wf18(:,70), wf24(:,46), A(:,172), n3(:,426), t3x432(:,:,172), nhel, den(291))
    call cont_QA(nsync, wf54(:,12), wf8(:,14), A(:,173), n3(:,427), t3x432(:,:,173), nhel, den(292))
    call cont_QA(nsync, wf18(:,72), wf24(:,44), A(:,174), n3(:,428), t3x432(:,:,174), nhel, den(293))
    call cont_QA(nsync, wf54(:,18), wf8(:,16), A(:,175), n3(:,429), t3x432(:,:,175), nhel, den(294))
    call cont_QA(nsync, wf18(:,54), wf24(:,39), A(:,176), n3(:,430), t3x432(:,:,176), nhel, den(295))
    call cont_VV(nsync, wf27(:,2), wf16(:,7), A(:,177), n3(:,431), t3x432(:,:,177), nhel, den(296))
    call cont_QA(nsync, wf18(:,72), wf24(:,46), A(:,178), n3(:,432), t3x432(:,:,178), nhel, den(297))
    call cont_VV(nsync, wf27(:,2), wf16(:,8), A(:,179), n3(:,433), t3x432(:,:,179), nhel, den(298))
    call cont_QA(nsync, wf54(:,13), wf8(:,14), A(:,180), n3(:,434), t3x432(:,:,180), nhel, den(299))
    call cont_QA(nsync, wf18(:,50), wf24(:,41), A(:,181), n3(:,435), t3x432(:,:,181), nhel, den(300))
    call cont_QA(nsync, wf18(:,44), wf24(:,47), A(:,182), n3(:,436), t3x432(:,:,182), nhel, den(301))
    call cont_QA(nsync, wf18(:,58), wf24(:,48), A(:,183), n3(:,437), t3x432(:,:,183), nhel, den(302))
    call cont_QA(nsync, wf18(:,66), wf24(:,45), A(:,184), n3(:,438), t3x432(:,:,184), nhel, den(303))
    call cont_VV(nsync, wf27(:,4), wf16(:,7), A(:,185), n3(:,439), t3x432(:,:,185), nhel, den(304))
    call cont_QA(nsync, wf18(:,68), wf24(:,45), A(:,186), n3(:,440), t3x432(:,:,186), nhel, den(305))
    call cont_VV(nsync, wf27(:,4), wf16(:,8), A(:,187), n3(:,441), t3x432(:,:,187), nhel, den(306))
    call cont_QA(nsync, wf54(:,14), wf8(:,14), A(:,188), n3(:,442), t3x432(:,:,188), nhel, den(307))
    call cont_VV(nsync, wf27(:,6), wf16(:,7), A(:,189), n3(:,443), t3x432(:,:,189), nhel, den(308))
    call cont_QA(nsync, wf18(:,60), wf24(:,48), A(:,190), n3(:,444), t3x432(:,:,190), nhel, den(309))
    call cont_VV(nsync, wf27(:,6), wf16(:,8), A(:,191), n3(:,445), t3x432(:,:,191), nhel, den(310))
    call cont_QA(nsync, wf54(:,15), wf8(:,14), A(:,192), n3(:,446), t3x432(:,:,192), nhel, den(311))
    call cont_VV(nsync, wf12(:,1), wf36(:,1), A(:,193), n3(:,447), t3x432(:,:,193), nhel, den(315))
    call cont_VV(nsync, wf12(:,2), wf36(:,2), A(:,194), n3(:,448), t3x432(:,:,194), nhel, den(319))
    call cont_VV(nsync, wf12(:,3), wf36(:,3), A(:,195), n3(:,449), t3x432(:,:,195), nhel, den(323))
    call cont_VV(nsync, wf12(:,4), wf36(:,4), A(:,196), n3(:,450), t3x432(:,:,196), nhel, den(327))
    call cont_QA(nsync, wf18(:,12), wf24(:,49), A(:,197), n3(:,451), t3x432(:,:,197), nhel, den(328))
    call cont_QA(nsync, wf18(:,12), wf24(:,50), A(:,198), n3(:,452), t3x432(:,:,198), nhel, den(329))
    call cont_QA(nsync, wf18(:,38), wf24(:,51), A(:,199), n3(:,453), t3x432(:,:,199), nhel, den(330))
    call cont_QA(nsync, wf18(:,38), wf24(:,52), A(:,200), n3(:,454), t3x432(:,:,200), nhel, den(331))
    call cont_VV(nsync, wf12(:,1), wf36(:,5), A(:,201), n3(:,455), t3x432(:,:,201), nhel, den(333))
    call cont_VV(nsync, wf12(:,5), wf36(:,6), A(:,202), n3(:,456), t3x432(:,:,202), nhel, den(337))
    call cont_VV(nsync, wf36(:,4), wf12(:,6), A(:,203), n3(:,457), t3x432(:,:,203), nhel, den(339))
    call cont_VV(nsync, wf36(:,6), wf12(:,7), A(:,204), n3(:,458), t3x432(:,:,204), nhel, den(341))
    call cont_QA(nsync, wf18(:,56), wf24(:,53), A(:,205), n3(:,459), t3x432(:,:,205), nhel, den(342))
    call cont_QA(nsync, wf18(:,38), wf24(:,54), A(:,206), n3(:,460), t3x432(:,:,206), nhel, den(343))
    call cont_VV(nsync, wf12(:,3), wf36(:,7), A(:,207), n3(:,461), t3x432(:,:,207), nhel, den(345))
    call cont_VV(nsync, wf12(:,8), wf36(:,8), A(:,208), n3(:,462), t3x432(:,:,208), nhel, den(349))
    call cont_VV(nsync, wf36(:,8), wf12(:,9), A(:,209), n3(:,463), t3x432(:,:,209), nhel, den(351))
    call cont_VV(nsync, wf36(:,2), wf12(:,10), A(:,210), n3(:,464), t3x432(:,:,210), nhel, den(353))
    call cont_QA(nsync, wf18(:,4), wf24(:,55), A(:,211), n3(:,465), t3x432(:,:,211), nhel, den(354))
    call cont_QA(nsync, wf18(:,38), wf24(:,56), A(:,212), n3(:,466), t3x432(:,:,212), nhel, den(355))
    call cont_QA(nsync, wf18(:,16), wf24(:,49), A(:,213), n3(:,467), t3x432(:,:,213), nhel, den(356))
    call cont_QA(nsync, wf18(:,16), wf24(:,50), A(:,214), n3(:,468), t3x432(:,:,214), nhel, den(357))
    call cont_QA(nsync, wf18(:,40), wf24(:,57), A(:,215), n3(:,469), t3x432(:,:,215), nhel, den(358))
    call cont_QA(nsync, wf18(:,40), wf24(:,58), A(:,216), n3(:,470), t3x432(:,:,216), nhel, den(359))
    call cont_QA(nsync, wf18(:,58), wf24(:,53), A(:,217), n3(:,471), t3x432(:,:,217), nhel, den(360))
    call cont_QA(nsync, wf18(:,40), wf24(:,59), A(:,218), n3(:,472), t3x432(:,:,218), nhel, den(361))
    call cont_QA(nsync, wf18(:,8), wf24(:,55), A(:,219), n3(:,473), t3x432(:,:,219), nhel, den(362))
    call cont_QA(nsync, wf18(:,40), wf24(:,60), A(:,220), n3(:,474), t3x432(:,:,220), nhel, den(363))
    call cont_QA(nsync, wf18(:,10), wf24(:,55), A(:,221), n3(:,475), t3x432(:,:,221), nhel, den(364))
    call cont_VV(nsync, wf12(:,3), wf36(:,9), A(:,222), n3(:,476), t3x432(:,:,222), nhel, den(365))
    call cont_VV(nsync, wf12(:,1), wf36(:,10), A(:,223), n3(:,477), t3x432(:,:,223), nhel, den(366))
    call cont_VV(nsync, wf12(:,1), wf36(:,11), A(:,224), n3(:,478), t3x432(:,:,224), nhel, den(367))
    call cont_VV(nsync, wf12(:,11), wf36(:,12), A(:,225), n3(:,479), t3x432(:,:,225), nhel, den(371))
    call cont_VV(nsync, wf12(:,12), wf36(:,13), A(:,226), n3(:,480), t3x432(:,:,226), nhel, den(375))
    call cont_VV(nsync, wf12(:,13), wf36(:,14), A(:,227), n3(:,481), t3x432(:,:,227), nhel, den(379))
    call cont_VV(nsync, wf12(:,14), wf36(:,15), A(:,228), n3(:,482), t3x432(:,:,228), nhel, den(383))
    call cont_QA(nsync, wf18(:,2), wf24(:,61), A(:,229), n3(:,483), t3x432(:,:,229), nhel, den(384))
    call cont_QA(nsync, wf18(:,2), wf24(:,62), A(:,230), n3(:,484), t3x432(:,:,230), nhel, den(385))
    call cont_QA(nsync, wf18(:,42), wf24(:,63), A(:,231), n3(:,485), t3x432(:,:,231), nhel, den(386))
    call cont_QA(nsync, wf18(:,42), wf24(:,64), A(:,232), n3(:,486), t3x432(:,:,232), nhel, den(387))
    call cont_VV(nsync, wf12(:,15), wf36(:,16), A(:,233), n3(:,487), t3x432(:,:,233), nhel, den(391))
    call cont_VV(nsync, wf12(:,12), wf36(:,17), A(:,234), n3(:,488), t3x432(:,:,234), nhel, den(393))
    call cont_VV(nsync, wf36(:,14), wf12(:,16), A(:,235), n3(:,489), t3x432(:,:,235), nhel, den(395))
    call cont_VV(nsync, wf36(:,16), wf12(:,17), A(:,236), n3(:,490), t3x432(:,:,236), nhel, den(397))
    call cont_QA(nsync, wf18(:,2), wf24(:,65), A(:,237), n3(:,491), t3x432(:,:,237), nhel, den(398))
    call cont_QA(nsync, wf18(:,56), wf24(:,66), A(:,238), n3(:,492), t3x432(:,:,238), nhel, den(399))
    call cont_VV(nsync, wf12(:,18), wf36(:,18), A(:,239), n3(:,493), t3x432(:,:,239), nhel, den(403))
    call cont_VV(nsync, wf12(:,14), wf36(:,19), A(:,240), n3(:,494), t3x432(:,:,240), nhel, den(405))
    call cont_VV(nsync, wf36(:,18), wf12(:,19), A(:,241), n3(:,495), t3x432(:,:,241), nhel, den(407))
    call cont_VV(nsync, wf36(:,12), wf12(:,20), A(:,242), n3(:,496), t3x432(:,:,242), nhel, den(409))
    call cont_QA(nsync, wf18(:,2), wf24(:,67), A(:,243), n3(:,497), t3x432(:,:,243), nhel, den(410))
    call cont_QA(nsync, wf18(:,4), wf24(:,68), A(:,244), n3(:,498), t3x432(:,:,244), nhel, den(411))
    call cont_QA(nsync, wf18(:,6), wf24(:,69), A(:,245), n3(:,499), t3x432(:,:,245), nhel, den(412))
    call cont_QA(nsync, wf18(:,6), wf24(:,70), A(:,246), n3(:,500), t3x432(:,:,246), nhel, den(413))
    call cont_QA(nsync, wf18(:,44), wf24(:,63), A(:,247), n3(:,501), t3x432(:,:,247), nhel, den(414))
    call cont_QA(nsync, wf18(:,44), wf24(:,64), A(:,248), n3(:,502), t3x432(:,:,248), nhel, den(415))
    call cont_QA(nsync, wf18(:,6), wf24(:,71), A(:,249), n3(:,503), t3x432(:,:,249), nhel, den(416))
    call cont_QA(nsync, wf18(:,58), wf24(:,66), A(:,250), n3(:,504), t3x432(:,:,250), nhel, den(417))
    call cont_QA(nsync, wf18(:,6), wf24(:,72), A(:,251), n3(:,505), t3x432(:,:,251), nhel, den(418))
    call cont_QA(nsync, wf18(:,8), wf24(:,68), A(:,252), n3(:,506), t3x432(:,:,252), nhel, den(419))
    call cont_QA(nsync, wf18(:,10), wf24(:,68), A(:,253), n3(:,507), t3x432(:,:,253), nhel, den(420))
    call cont_VV(nsync, wf12(:,12), wf36(:,20), A(:,254), n3(:,508), t3x432(:,:,254), nhel, den(421))
    call cont_VV(nsync, wf12(:,12), wf36(:,21), A(:,255), n3(:,509), t3x432(:,:,255), nhel, den(422))
    call cont_VV(nsync, wf12(:,14), wf36(:,22), A(:,256), n3(:,510), t3x432(:,:,256), nhel, den(423))
    call cont_VV(nsync, wf12(:,11), wf36(:,23), A(:,257), n3(:,511), t3x432(:,:,257), nhel, den(425))
    call cont_VV(nsync, wf12(:,5), wf36(:,24), A(:,258), n3(:,512), t3x432(:,:,258), nhel, den(427))
    call cont_VV(nsync, wf36(:,15), wf12(:,21), A(:,259), n3(:,513), t3x432(:,:,259), nhel, den(429))
    call cont_VV(nsync, wf12(:,7), wf36(:,24), A(:,260), n3(:,514), t3x432(:,:,260), nhel, den(430))
    call cont_QA(nsync, wf18(:,62), wf24(:,73), A(:,261), n3(:,515), t3x432(:,:,261), nhel, den(431))
    call cont_QA(nsync, wf18(:,42), wf24(:,74), A(:,262), n3(:,516), t3x432(:,:,262), nhel, den(432))
    call cont_VV(nsync, wf12(:,15), wf36(:,25), A(:,263), n3(:,517), t3x432(:,:,263), nhel, den(434))
    call cont_VV(nsync, wf12(:,2), wf36(:,26), A(:,264), n3(:,518), t3x432(:,:,264), nhel, den(436))
    call cont_VV(nsync, wf36(:,3), wf12(:,22), A(:,265), n3(:,519), t3x432(:,:,265), nhel, den(438))
    call cont_VV(nsync, wf12(:,17), wf36(:,25), A(:,266), n3(:,520), t3x432(:,:,266), nhel, den(439))
    call cont_QA(nsync, wf18(:,12), wf24(:,75), A(:,267), n3(:,521), t3x432(:,:,267), nhel, den(440))
    call cont_QA(nsync, wf18(:,62), wf24(:,76), A(:,268), n3(:,522), t3x432(:,:,268), nhel, den(441))
    call cont_VV(nsync, wf36(:,19), wf12(:,21), A(:,269), n3(:,523), t3x432(:,:,269), nhel, den(442))
    call cont_VV(nsync, wf12(:,10), wf36(:,26), A(:,270), n3(:,524), t3x432(:,:,270), nhel, den(443))
    call cont_VV(nsync, wf36(:,7), wf12(:,22), A(:,271), n3(:,525), t3x432(:,:,271), nhel, den(444))
    call cont_VV(nsync, wf12(:,20), wf36(:,23), A(:,272), n3(:,526), t3x432(:,:,272), nhel, den(445))
    call cont_QA(nsync, wf18(:,4), wf24(:,77), A(:,273), n3(:,527), t3x432(:,:,273), nhel, den(446))
    call cont_QA(nsync, wf18(:,62), wf24(:,78), A(:,274), n3(:,528), t3x432(:,:,274), nhel, den(447))
    call cont_QA(nsync, wf18(:,4), wf24(:,79), A(:,275), n3(:,529), t3x432(:,:,275), nhel, den(448))
    call cont_QA(nsync, wf18(:,62), wf24(:,80), A(:,276), n3(:,530), t3x432(:,:,276), nhel, den(449))
    call cont_QA(nsync, wf18(:,64), wf24(:,81), A(:,277), n3(:,531), t3x432(:,:,277), nhel, den(450))
    call cont_QA(nsync, wf18(:,44), wf24(:,74), A(:,278), n3(:,532), t3x432(:,:,278), nhel, den(451))
    call cont_QA(nsync, wf18(:,16), wf24(:,75), A(:,279), n3(:,533), t3x432(:,:,279), nhel, den(452))
    call cont_QA(nsync, wf18(:,64), wf24(:,82), A(:,280), n3(:,534), t3x432(:,:,280), nhel, den(453))
    call cont_QA(nsync, wf18(:,8), wf24(:,77), A(:,281), n3(:,535), t3x432(:,:,281), nhel, den(454))
    call cont_QA(nsync, wf18(:,64), wf24(:,83), A(:,282), n3(:,536), t3x432(:,:,282), nhel, den(455))
    call cont_QA(nsync, wf18(:,8), wf24(:,79), A(:,283), n3(:,537), t3x432(:,:,283), nhel, den(456))
    call cont_QA(nsync, wf18(:,64), wf24(:,84), A(:,284), n3(:,538), t3x432(:,:,284), nhel, den(457))
    call cont_QA(nsync, wf18(:,10), wf24(:,77), A(:,285), n3(:,539), t3x432(:,:,285), nhel, den(458))
    call cont_QA(nsync, wf18(:,10), wf24(:,79), A(:,286), n3(:,540), t3x432(:,:,286), nhel, den(459))
    call cont_QA(nsync, wf18(:,20), wf24(:,75), A(:,287), n3(:,541), t3x432(:,:,287), nhel, den(460))
    call cont_VV(nsync, wf36(:,22), wf12(:,21), A(:,288), n3(:,542), t3x432(:,:,288), nhel, den(461))
    call cont_VV(nsync, wf12(:,13), wf36(:,27), A(:,289), n3(:,543), t3x432(:,:,289), nhel, den(463))
    call cont_VV(nsync, wf12(:,8), wf36(:,28), A(:,290), n3(:,544), t3x432(:,:,290), nhel, den(465))
    call cont_VV(nsync, wf12(:,9), wf36(:,28), A(:,291), n3(:,545), t3x432(:,:,291), nhel, den(466))
    call cont_VV(nsync, wf36(:,13), wf12(:,23), A(:,292), n3(:,546), t3x432(:,:,292), nhel, den(468))
    call cont_QA(nsync, wf18(:,14), wf24(:,85), A(:,293), n3(:,547), t3x432(:,:,293), nhel, den(469))
    call cont_QA(nsync, wf18(:,42), wf24(:,86), A(:,294), n3(:,548), t3x432(:,:,294), nhel, den(470))
    call cont_VV(nsync, wf12(:,18), wf36(:,29), A(:,295), n3(:,549), t3x432(:,:,295), nhel, den(472))
    call cont_VV(nsync, wf12(:,4), wf36(:,30), A(:,296), n3(:,550), t3x432(:,:,296), nhel, den(474))
    call cont_VV(nsync, wf12(:,19), wf36(:,29), A(:,297), n3(:,551), t3x432(:,:,297), nhel, den(475))
    call cont_VV(nsync, wf36(:,1), wf12(:,24), A(:,298), n3(:,552), t3x432(:,:,298), nhel, den(477))
    call cont_QA(nsync, wf18(:,12), wf24(:,87), A(:,299), n3(:,553), t3x432(:,:,299), nhel, den(478))
    call cont_QA(nsync, wf18(:,14), wf24(:,88), A(:,300), n3(:,554), t3x432(:,:,300), nhel, den(479))
    call cont_VV(nsync, wf12(:,6), wf36(:,30), A(:,301), n3(:,555), t3x432(:,:,301), nhel, den(480))
    call cont_VV(nsync, wf36(:,17), wf12(:,23), A(:,302), n3(:,556), t3x432(:,:,302), nhel, den(481))
    call cont_VV(nsync, wf12(:,16), wf36(:,27), A(:,303), n3(:,557), t3x432(:,:,303), nhel, den(482))
    call cont_VV(nsync, wf36(:,5), wf12(:,24), A(:,304), n3(:,558), t3x432(:,:,304), nhel, den(483))
    call cont_QA(nsync, wf18(:,14), wf24(:,89), A(:,305), n3(:,559), t3x432(:,:,305), nhel, den(484))
    call cont_QA(nsync, wf18(:,56), wf24(:,90), A(:,306), n3(:,560), t3x432(:,:,306), nhel, den(485))
    call cont_QA(nsync, wf18(:,14), wf24(:,91), A(:,307), n3(:,561), t3x432(:,:,307), nhel, den(486))
    call cont_QA(nsync, wf18(:,56), wf24(:,92), A(:,308), n3(:,562), t3x432(:,:,308), nhel, den(487))
    call cont_QA(nsync, wf18(:,18), wf24(:,93), A(:,309), n3(:,563), t3x432(:,:,309), nhel, den(488))
    call cont_QA(nsync, wf18(:,44), wf24(:,86), A(:,310), n3(:,564), t3x432(:,:,310), nhel, den(489))
    call cont_QA(nsync, wf18(:,16), wf24(:,87), A(:,311), n3(:,565), t3x432(:,:,311), nhel, den(490))
    call cont_QA(nsync, wf18(:,18), wf24(:,94), A(:,312), n3(:,566), t3x432(:,:,312), nhel, den(491))
    call cont_QA(nsync, wf18(:,18), wf24(:,95), A(:,313), n3(:,567), t3x432(:,:,313), nhel, den(492))
    call cont_QA(nsync, wf18(:,58), wf24(:,90), A(:,314), n3(:,568), t3x432(:,:,314), nhel, den(493))
    call cont_QA(nsync, wf18(:,18), wf24(:,96), A(:,315), n3(:,569), t3x432(:,:,315), nhel, den(494))
    call cont_QA(nsync, wf18(:,58), wf24(:,92), A(:,316), n3(:,570), t3x432(:,:,316), nhel, den(495))
    call cont_QA(nsync, wf18(:,60), wf24(:,92), A(:,317), n3(:,571), t3x432(:,:,317), nhel, den(496))
    call cont_QA(nsync, wf18(:,60), wf24(:,90), A(:,318), n3(:,572), t3x432(:,:,318), nhel, den(497))
    call cont_QA(nsync, wf18(:,20), wf24(:,87), A(:,319), n3(:,573), t3x432(:,:,319), nhel, den(498))
    call cont_VV(nsync, wf36(:,21), wf12(:,23), A(:,320), n3(:,574), t3x432(:,:,320), nhel, den(499))
    call cont_QA(nsync, wf18(:,28), wf24(:,61), A(:,321), n3(:,575), t3x432(:,:,321), nhel, den(500))
    call cont_QA(nsync, wf18(:,28), wf24(:,62), A(:,322), n3(:,576), t3x432(:,:,322), nhel, den(501))
    call cont_QA(nsync, wf18(:,48), wf24(:,57), A(:,323), n3(:,577), t3x432(:,:,323), nhel, den(502))
    call cont_QA(nsync, wf18(:,48), wf24(:,58), A(:,324), n3(:,578), t3x432(:,:,324), nhel, den(503))
    call cont_QA(nsync, wf18(:,66), wf24(:,73), A(:,325), n3(:,579), t3x432(:,:,325), nhel, den(504))
    call cont_QA(nsync, wf18(:,48), wf24(:,59), A(:,326), n3(:,580), t3x432(:,:,326), nhel, den(505))
    call cont_QA(nsync, wf18(:,24), wf24(:,85), A(:,327), n3(:,581), t3x432(:,:,327), nhel, den(506))
    call cont_QA(nsync, wf18(:,48), wf24(:,60), A(:,328), n3(:,582), t3x432(:,:,328), nhel, den(507))
    call cont_QA(nsync, wf18(:,26), wf24(:,85), A(:,329), n3(:,583), t3x432(:,:,329), nhel, den(508))
    call cont_VV(nsync, wf12(:,13), wf36(:,31), A(:,330), n3(:,584), t3x432(:,:,330), nhel, den(509))
    call cont_VV(nsync, wf12(:,11), wf36(:,32), A(:,331), n3(:,585), t3x432(:,:,331), nhel, den(510))
    call cont_VV(nsync, wf12(:,11), wf36(:,33), A(:,332), n3(:,586), t3x432(:,:,332), nhel, den(511))
    call cont_QA(nsync, wf18(:,22), wf24(:,69), A(:,333), n3(:,587), t3x432(:,:,333), nhel, den(512))
    call cont_QA(nsync, wf18(:,22), wf24(:,70), A(:,334), n3(:,588), t3x432(:,:,334), nhel, den(513))
    call cont_QA(nsync, wf18(:,50), wf24(:,51), A(:,335), n3(:,589), t3x432(:,:,335), nhel, den(514))
    call cont_QA(nsync, wf18(:,50), wf24(:,52), A(:,336), n3(:,590), t3x432(:,:,336), nhel, den(515))
    call cont_QA(nsync, wf18(:,22), wf24(:,71), A(:,337), n3(:,591), t3x432(:,:,337), nhel, den(516))
    call cont_QA(nsync, wf18(:,66), wf24(:,76), A(:,338), n3(:,592), t3x432(:,:,338), nhel, den(517))
    call cont_QA(nsync, wf18(:,22), wf24(:,72), A(:,339), n3(:,593), t3x432(:,:,339), nhel, den(518))
    call cont_QA(nsync, wf18(:,24), wf24(:,88), A(:,340), n3(:,594), t3x432(:,:,340), nhel, den(519))
    call cont_QA(nsync, wf18(:,26), wf24(:,88), A(:,341), n3(:,595), t3x432(:,:,341), nhel, den(520))
    call cont_VV(nsync, wf12(:,2), wf36(:,34), A(:,342), n3(:,596), t3x432(:,:,342), nhel, den(521))
    call cont_VV(nsync, wf12(:,2), wf36(:,35), A(:,343), n3(:,597), t3x432(:,:,343), nhel, den(522))
    call cont_VV(nsync, wf12(:,4), wf36(:,36), A(:,344), n3(:,598), t3x432(:,:,344), nhel, den(523))
    call cont_QA(nsync, wf18(:,70), wf24(:,81), A(:,345), n3(:,599), t3x432(:,:,345), nhel, den(524))
    call cont_QA(nsync, wf18(:,50), wf24(:,54), A(:,346), n3(:,600), t3x432(:,:,346), nhel, den(525))
    call cont_QA(nsync, wf18(:,28), wf24(:,65), A(:,347), n3(:,601), t3x432(:,:,347), nhel, den(526))
    call cont_QA(nsync, wf18(:,70), wf24(:,82), A(:,348), n3(:,602), t3x432(:,:,348), nhel, den(527))
    call cont_QA(nsync, wf18(:,24), wf24(:,89), A(:,349), n3(:,603), t3x432(:,:,349), nhel, den(528))
    call cont_QA(nsync, wf18(:,70), wf24(:,83), A(:,350), n3(:,604), t3x432(:,:,350), nhel, den(529))
    call cont_QA(nsync, wf18(:,24), wf24(:,91), A(:,351), n3(:,605), t3x432(:,:,351), nhel, den(530))
    call cont_QA(nsync, wf18(:,70), wf24(:,84), A(:,352), n3(:,606), t3x432(:,:,352), nhel, den(531))
    call cont_QA(nsync, wf18(:,26), wf24(:,89), A(:,353), n3(:,607), t3x432(:,:,353), nhel, den(532))
    call cont_QA(nsync, wf18(:,26), wf24(:,91), A(:,354), n3(:,608), t3x432(:,:,354), nhel, den(533))
    call cont_QA(nsync, wf18(:,32), wf24(:,65), A(:,355), n3(:,609), t3x432(:,:,355), nhel, den(534))
    call cont_VV(nsync, wf12(:,6), wf36(:,36), A(:,356), n3(:,610), t3x432(:,:,356), nhel, den(535))
    call cont_QA(nsync, wf18(:,30), wf24(:,93), A(:,357), n3(:,611), t3x432(:,:,357), nhel, den(536))
    call cont_QA(nsync, wf18(:,50), wf24(:,56), A(:,358), n3(:,612), t3x432(:,:,358), nhel, den(537))
    call cont_QA(nsync, wf18(:,28), wf24(:,67), A(:,359), n3(:,613), t3x432(:,:,359), nhel, den(538))
    call cont_QA(nsync, wf18(:,30), wf24(:,94), A(:,360), n3(:,614), t3x432(:,:,360), nhel, den(539))
    call cont_QA(nsync, wf18(:,30), wf24(:,95), A(:,361), n3(:,615), t3x432(:,:,361), nhel, den(540))
    call cont_QA(nsync, wf18(:,66), wf24(:,78), A(:,362), n3(:,616), t3x432(:,:,362), nhel, den(541))
    call cont_QA(nsync, wf18(:,30), wf24(:,96), A(:,363), n3(:,617), t3x432(:,:,363), nhel, den(542))
    call cont_QA(nsync, wf18(:,66), wf24(:,80), A(:,364), n3(:,618), t3x432(:,:,364), nhel, den(543))
    call cont_QA(nsync, wf18(:,68), wf24(:,80), A(:,365), n3(:,619), t3x432(:,:,365), nhel, den(544))
    call cont_QA(nsync, wf18(:,68), wf24(:,78), A(:,366), n3(:,620), t3x432(:,:,366), nhel, den(545))
    call cont_QA(nsync, wf18(:,32), wf24(:,67), A(:,367), n3(:,621), t3x432(:,:,367), nhel, den(546))
    call cont_VV(nsync, wf12(:,10), wf36(:,35), A(:,368), n3(:,622), t3x432(:,:,368), nhel, den(547))
    call cont_QA(nsync, wf18(:,34), wf24(:,93), A(:,369), n3(:,623), t3x432(:,:,369), nhel, den(548))
    call cont_VV(nsync, wf12(:,18), wf36(:,37), A(:,370), n3(:,624), t3x432(:,:,370), nhel, den(549))
    call cont_VV(nsync, wf12(:,15), wf36(:,38), A(:,371), n3(:,625), t3x432(:,:,371), nhel, den(550))
    call cont_VV(nsync, wf12(:,15), wf36(:,39), A(:,372), n3(:,626), t3x432(:,:,372), nhel, den(551))
    call cont_QA(nsync, wf18(:,34), wf24(:,94), A(:,373), n3(:,627), t3x432(:,:,373), nhel, den(552))
    call cont_VV(nsync, wf12(:,5), wf36(:,40), A(:,374), n3(:,628), t3x432(:,:,374), nhel, den(553))
    call cont_VV(nsync, wf12(:,5), wf36(:,41), A(:,375), n3(:,629), t3x432(:,:,375), nhel, den(554))
    call cont_VV(nsync, wf12(:,8), wf36(:,42), A(:,376), n3(:,630), t3x432(:,:,376), nhel, den(555))
    call cont_QA(nsync, wf18(:,34), wf24(:,95), A(:,377), n3(:,631), t3x432(:,:,377), nhel, den(556))
    call cont_QA(nsync, wf18(:,34), wf24(:,96), A(:,378), n3(:,632), t3x432(:,:,378), nhel, den(557))
    call cont_QA(nsync, wf18(:,36), wf24(:,71), A(:,379), n3(:,633), t3x432(:,:,379), nhel, den(558))
    call cont_VV(nsync, wf12(:,9), wf36(:,42), A(:,380), n3(:,634), t3x432(:,:,380), nhel, den(559))
    call cont_QA(nsync, wf18(:,72), wf24(:,84), A(:,381), n3(:,635), t3x432(:,:,381), nhel, den(560))
    call cont_QA(nsync, wf18(:,72), wf24(:,83), A(:,382), n3(:,636), t3x432(:,:,382), nhel, den(561))
    call cont_QA(nsync, wf18(:,36), wf24(:,72), A(:,383), n3(:,637), t3x432(:,:,383), nhel, den(562))
    call cont_VV(nsync, wf12(:,7), wf36(:,41), A(:,384), n3(:,638), t3x432(:,:,384), nhel, den(563))

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
  complex(REALKIND), intent(out) :: M1(2) ! M1helarray(2,432)
  integer :: empty(0)

  M1(1) = ((A(j,49)%j+A(j,50)%j+A(j,51)%j+A(j,52)%j+A(j,55)%j+A(j,56)%j+A(j,57)%j+A(j,58)%j+A(j,59)%j+A(j,60)%j+A(j,63)%j &
       +A(j,64)%j+A(j,65)%j+A(j,66)%j+A(j,69)%j+A(j,70)%j+A(j,71)%j+A(j,72)%j+A(j,75)%j+A(j,76)%j+A(j,85)%j+A(j,86)%j+A(j,87)%j &
       +A(j,88)%j+A(j,97)%j+A(j,98)%j+A(j,99)%j+A(j,100)%j+A(j,103)%j+A(j,104)%j+A(j,105)%j+A(j,106)%j+A(j,107)%j+A(j,108)%j &
       +A(j,111)%j+A(j,112)%j+A(j,113)%j+A(j,114)%j+A(j,117)%j+A(j,118)%j+A(j,119)%j+A(j,120)%j+A(j,123)%j+A(j,124)%j+A(j,133)%j &
       +A(j,134)%j+A(j,135)%j+A(j,136)%j+A(j,193)%j+A(j,196)%j+A(j,198)%j+A(j,199)%j+A(j,201)%j+A(j,203)%j+A(j,205)%j+A(j,206)%j &
       +A(j,208)%j+A(j,209)%j+A(j,214)%j+A(j,215)%j+A(j,217)%j+A(j,218)%j+A(j,225)%j+A(j,228)%j+A(j,230)%j+A(j,231)%j+A(j,233)%j &
       +A(j,236)%j+A(j,240)%j+A(j,242)%j+A(j,243)%j+A(j,244)%j+A(j,246)%j+A(j,247)%j+A(j,251)%j+A(j,252)%j+A(j,257)%j+A(j,259)%j &
       +A(j,261)%j+A(j,262)%j+A(j,263)%j+A(j,266)%j+A(j,269)%j+A(j,272)%j+A(j,274)%j+A(j,275)%j+A(j,277)%j+A(j,278)%j+A(j,282)%j &
       +A(j,283)%j+A(j,290)%j+A(j,291)%j+A(j,296)%j+A(j,298)%j+A(j,299)%j+A(j,300)%j+A(j,301)%j+A(j,304)%j+A(j,306)%j+A(j,307)%j &
       +A(j,311)%j+A(j,312)%j+A(j,314)%j+A(j,315)%j+A(j,322)%j+A(j,323)%j+A(j,325)%j+A(j,326)%j+A(j,334)%j+A(j,335)%j+A(j,339)%j &
       +A(j,340)%j+A(j,345)%j+A(j,346)%j+A(j,350)%j+A(j,351)%j+A(j,359)%j+A(j,360)%j+A(j,362)%j+A(j,363)%j)*f(1))/6._/**/REALKIND &
       +((A(j,1)%j+A(j,2)%j+A(j,3)%j+A(j,4)%j+A(j,7)%j+A(j,8)%j+A(j,9)%j+A(j,10)%j+A(j,11)%j+A(j,12)%j+A(j,15)%j+A(j,16)%j &
       +A(j,17)%j+A(j,18)%j+A(j,21)%j+A(j,22)%j+A(j,23)%j+A(j,24)%j+A(j,27)%j+A(j,28)%j+A(j,37)%j+A(j,38)%j+A(j,39)%j+A(j,40)%j &
       +A(j,145)%j+A(j,146)%j+A(j,147)%j+A(j,148)%j+A(j,151)%j+A(j,152)%j+A(j,153)%j+A(j,154)%j+A(j,155)%j+A(j,156)%j+A(j,159)%j &
       +A(j,160)%j+A(j,161)%j+A(j,162)%j+A(j,165)%j+A(j,166)%j+A(j,167)%j+A(j,168)%j+A(j,171)%j+A(j,172)%j+A(j,181)%j+A(j,182)%j &
       +A(j,183)%j+A(j,184)%j+A(j,194)%j+A(j,195)%j+A(j,197)%j+A(j,200)%j+A(j,202)%j+A(j,204)%j+A(j,207)%j+A(j,210)%j+A(j,211)%j &
       +A(j,212)%j+A(j,213)%j+A(j,216)%j+A(j,219)%j+A(j,220)%j+A(j,226)%j+A(j,227)%j+A(j,229)%j+A(j,232)%j+A(j,234)%j+A(j,235)%j &
       +A(j,237)%j+A(j,238)%j+A(j,239)%j+A(j,241)%j+A(j,245)%j+A(j,248)%j+A(j,249)%j+A(j,250)%j+A(j,258)%j+A(j,260)%j+A(j,264)%j &
       +A(j,265)%j+A(j,267)%j+A(j,268)%j+A(j,270)%j+A(j,271)%j+A(j,273)%j+A(j,276)%j+A(j,279)%j+A(j,280)%j+A(j,281)%j+A(j,284)%j &
       +A(j,289)%j+A(j,292)%j+A(j,293)%j+A(j,294)%j+A(j,295)%j+A(j,297)%j+A(j,302)%j+A(j,303)%j+A(j,305)%j+A(j,308)%j+A(j,309)%j &
       +A(j,310)%j+A(j,313)%j+A(j,316)%j+A(j,321)%j+A(j,324)%j+A(j,327)%j+A(j,328)%j+A(j,333)%j+A(j,336)%j+A(j,337)%j+A(j,338)%j &
       +A(j,347)%j+A(j,348)%j+A(j,349)%j+A(j,352)%j+A(j,357)%j+A(j,358)%j+A(j,361)%j+A(j,364)%j)*f(1))/2._/**/REALKIND+(( &
       -A(j,53)%j-A(j,54)%j-A(j,61)%j-A(j,62)%j-A(j,67)%j-A(j,68)%j-A(j,73)%j-A(j,74)%j-A(j,77)%j-A(j,78)%j-A(j,79)%j-A(j,80)%j &
       -A(j,82)%j-A(j,84)%j-A(j,90)%j-A(j,92)%j-A(j,94)%j-A(j,96)%j-A(j,101)%j-A(j,102)%j-A(j,109)%j-A(j,110)%j-A(j,115)%j &
       -A(j,116)%j-A(j,121)%j-A(j,122)%j-A(j,125)%j-A(j,126)%j-A(j,127)%j-A(j,128)%j-A(j,130)%j-A(j,132)%j-A(j,138)%j-A(j,140)%j &
       -A(j,142)%j-A(j,144)%j-A(j,223)%j-A(j,224)%j-A(j,253)%j-A(j,256)%j-A(j,286)%j-A(j,288)%j-A(j,318)%j-A(j,319)%j-A(j,331)%j &
       -A(j,332)%j-A(j,341)%j-A(j,344)%j-A(j,354)%j-A(j,356)%j-A(j,366)%j-A(j,367)%j-A(j,371)%j-A(j,372)%j-A(j,373)%j-A(j,376)%j &
       -A(j,378)%j-A(j,380)%j-A(j,382)%j-A(j,383)%j)*f(2))/6._/**/REALKIND+((-A(j,5)%j-A(j,6)%j-A(j,13)%j-A(j,14)%j-A(j,19)%j &
       -A(j,20)%j-A(j,25)%j-A(j,26)%j-A(j,29)%j-A(j,30)%j-A(j,31)%j-A(j,32)%j-A(j,34)%j-A(j,36)%j-A(j,42)%j-A(j,44)%j-A(j,46)%j &
       -A(j,48)%j-A(j,149)%j-A(j,150)%j-A(j,157)%j-A(j,158)%j-A(j,163)%j-A(j,164)%j-A(j,169)%j-A(j,170)%j-A(j,173)%j-A(j,174)%j &
       -A(j,175)%j-A(j,176)%j-A(j,178)%j-A(j,180)%j-A(j,186)%j-A(j,188)%j-A(j,190)%j-A(j,192)%j-A(j,221)%j-A(j,222)%j-A(j,254)%j &
       -A(j,255)%j-A(j,285)%j-A(j,287)%j-A(j,317)%j-A(j,320)%j-A(j,329)%j-A(j,330)%j-A(j,342)%j-A(j,343)%j-A(j,353)%j-A(j,355)%j &
       -A(j,365)%j-A(j,368)%j-A(j,369)%j-A(j,370)%j-A(j,374)%j-A(j,375)%j-A(j,377)%j-A(j,379)%j-A(j,381)%j &
       -A(j,384)%j)*f(2))/2._/**/REALKIND+((-A(j,81)%j-A(j,83)%j-A(j,89)%j-A(j,91)%j-A(j,93)%j-A(j,95)%j-A(j,129)%j-A(j,131)%j &
       -A(j,137)%j-A(j,139)%j-A(j,141)%j-A(j,143)%j)*f(3))/6._/**/REALKIND+((-A(j,33)%j-A(j,35)%j-A(j,41)%j-A(j,43)%j-A(j,45)%j &
       -A(j,47)%j-A(j,177)%j-A(j,179)%j-A(j,185)%j-A(j,187)%j-A(j,189)%j-A(j,191)%j)*f(3))/2._/**/REALKIND
  M1(2) = ((-A(j,49)%j-A(j,50)%j-A(j,51)%j-A(j,52)%j-A(j,55)%j-A(j,56)%j-A(j,57)%j-A(j,58)%j-A(j,59)%j-A(j,60)%j-A(j,63)%j &
       -A(j,64)%j-A(j,65)%j-A(j,66)%j-A(j,69)%j-A(j,70)%j-A(j,71)%j-A(j,72)%j-A(j,75)%j-A(j,76)%j-A(j,85)%j-A(j,86)%j-A(j,87)%j &
       -A(j,88)%j-A(j,97)%j-A(j,98)%j-A(j,99)%j-A(j,100)%j-A(j,103)%j-A(j,104)%j-A(j,105)%j-A(j,106)%j-A(j,107)%j-A(j,108)%j &
       -A(j,111)%j-A(j,112)%j-A(j,113)%j-A(j,114)%j-A(j,117)%j-A(j,118)%j-A(j,119)%j-A(j,120)%j-A(j,123)%j-A(j,124)%j-A(j,133)%j &
       -A(j,134)%j-A(j,135)%j-A(j,136)%j-A(j,193)%j-A(j,196)%j-A(j,198)%j-A(j,199)%j-A(j,201)%j-A(j,203)%j-A(j,205)%j-A(j,206)%j &
       -A(j,208)%j-A(j,209)%j-A(j,214)%j-A(j,215)%j-A(j,217)%j-A(j,218)%j-A(j,225)%j-A(j,228)%j-A(j,230)%j-A(j,231)%j-A(j,233)%j &
       -A(j,236)%j-A(j,240)%j-A(j,242)%j-A(j,243)%j-A(j,244)%j-A(j,246)%j-A(j,247)%j-A(j,251)%j-A(j,252)%j-A(j,257)%j-A(j,259)%j &
       -A(j,261)%j-A(j,262)%j-A(j,263)%j-A(j,266)%j-A(j,269)%j-A(j,272)%j-A(j,274)%j-A(j,275)%j-A(j,277)%j-A(j,278)%j-A(j,282)%j &
       -A(j,283)%j-A(j,290)%j-A(j,291)%j-A(j,296)%j-A(j,298)%j-A(j,299)%j-A(j,300)%j-A(j,301)%j-A(j,304)%j-A(j,306)%j-A(j,307)%j &
       -A(j,311)%j-A(j,312)%j-A(j,314)%j-A(j,315)%j-A(j,322)%j-A(j,323)%j-A(j,325)%j-A(j,326)%j-A(j,334)%j-A(j,335)%j-A(j,339)%j &
       -A(j,340)%j-A(j,345)%j-A(j,346)%j-A(j,350)%j-A(j,351)%j-A(j,359)%j-A(j,360)%j-A(j,362)%j-A(j,363)%j)*f(1))/2._/**/REALKIND &
       +((-A(j,1)%j-A(j,2)%j-A(j,3)%j-A(j,4)%j-A(j,7)%j-A(j,8)%j-A(j,9)%j-A(j,10)%j-A(j,11)%j-A(j,12)%j-A(j,15)%j-A(j,16)%j &
       -A(j,17)%j-A(j,18)%j-A(j,21)%j-A(j,22)%j-A(j,23)%j-A(j,24)%j-A(j,27)%j-A(j,28)%j-A(j,37)%j-A(j,38)%j-A(j,39)%j-A(j,40)%j &
       -A(j,145)%j-A(j,146)%j-A(j,147)%j-A(j,148)%j-A(j,151)%j-A(j,152)%j-A(j,153)%j-A(j,154)%j-A(j,155)%j-A(j,156)%j-A(j,159)%j &
       -A(j,160)%j-A(j,161)%j-A(j,162)%j-A(j,165)%j-A(j,166)%j-A(j,167)%j-A(j,168)%j-A(j,171)%j-A(j,172)%j-A(j,181)%j-A(j,182)%j &
       -A(j,183)%j-A(j,184)%j-A(j,194)%j-A(j,195)%j-A(j,197)%j-A(j,200)%j-A(j,202)%j-A(j,204)%j-A(j,207)%j-A(j,210)%j-A(j,211)%j &
       -A(j,212)%j-A(j,213)%j-A(j,216)%j-A(j,219)%j-A(j,220)%j-A(j,226)%j-A(j,227)%j-A(j,229)%j-A(j,232)%j-A(j,234)%j-A(j,235)%j &
       -A(j,237)%j-A(j,238)%j-A(j,239)%j-A(j,241)%j-A(j,245)%j-A(j,248)%j-A(j,249)%j-A(j,250)%j-A(j,258)%j-A(j,260)%j-A(j,264)%j &
       -A(j,265)%j-A(j,267)%j-A(j,268)%j-A(j,270)%j-A(j,271)%j-A(j,273)%j-A(j,276)%j-A(j,279)%j-A(j,280)%j-A(j,281)%j-A(j,284)%j &
       -A(j,289)%j-A(j,292)%j-A(j,293)%j-A(j,294)%j-A(j,295)%j-A(j,297)%j-A(j,302)%j-A(j,303)%j-A(j,305)%j-A(j,308)%j-A(j,309)%j &
       -A(j,310)%j-A(j,313)%j-A(j,316)%j-A(j,321)%j-A(j,324)%j-A(j,327)%j-A(j,328)%j-A(j,333)%j-A(j,336)%j-A(j,337)%j-A(j,338)%j &
       -A(j,347)%j-A(j,348)%j-A(j,349)%j-A(j,352)%j-A(j,357)%j-A(j,358)%j-A(j,361)%j-A(j,364)%j)*f(1))/6._/**/REALKIND+((A(j,53)%j &
       +A(j,54)%j+A(j,61)%j+A(j,62)%j+A(j,67)%j+A(j,68)%j+A(j,73)%j+A(j,74)%j+A(j,77)%j+A(j,78)%j+A(j,79)%j+A(j,80)%j+A(j,82)%j &
       +A(j,84)%j+A(j,90)%j+A(j,92)%j+A(j,94)%j+A(j,96)%j+A(j,101)%j+A(j,102)%j+A(j,109)%j+A(j,110)%j+A(j,115)%j+A(j,116)%j &
       +A(j,121)%j+A(j,122)%j+A(j,125)%j+A(j,126)%j+A(j,127)%j+A(j,128)%j+A(j,130)%j+A(j,132)%j+A(j,138)%j+A(j,140)%j+A(j,142)%j &
       +A(j,144)%j+A(j,223)%j+A(j,224)%j+A(j,253)%j+A(j,256)%j+A(j,286)%j+A(j,288)%j+A(j,318)%j+A(j,319)%j+A(j,331)%j+A(j,332)%j &
       +A(j,341)%j+A(j,344)%j+A(j,354)%j+A(j,356)%j+A(j,366)%j+A(j,367)%j+A(j,371)%j+A(j,372)%j+A(j,373)%j+A(j,376)%j+A(j,378)%j &
       +A(j,380)%j+A(j,382)%j+A(j,383)%j)*f(2))/2._/**/REALKIND+((A(j,5)%j+A(j,6)%j+A(j,13)%j+A(j,14)%j+A(j,19)%j+A(j,20)%j &
       +A(j,25)%j+A(j,26)%j+A(j,29)%j+A(j,30)%j+A(j,31)%j+A(j,32)%j+A(j,34)%j+A(j,36)%j+A(j,42)%j+A(j,44)%j+A(j,46)%j+A(j,48)%j &
       +A(j,149)%j+A(j,150)%j+A(j,157)%j+A(j,158)%j+A(j,163)%j+A(j,164)%j+A(j,169)%j+A(j,170)%j+A(j,173)%j+A(j,174)%j+A(j,175)%j &
       +A(j,176)%j+A(j,178)%j+A(j,180)%j+A(j,186)%j+A(j,188)%j+A(j,190)%j+A(j,192)%j+A(j,221)%j+A(j,222)%j+A(j,254)%j+A(j,255)%j &
       +A(j,285)%j+A(j,287)%j+A(j,317)%j+A(j,320)%j+A(j,329)%j+A(j,330)%j+A(j,342)%j+A(j,343)%j+A(j,353)%j+A(j,355)%j+A(j,365)%j &
       +A(j,368)%j+A(j,369)%j+A(j,370)%j+A(j,374)%j+A(j,375)%j+A(j,377)%j+A(j,379)%j+A(j,381)%j+A(j,384)%j)*f(2))/6._/**/REALKIND &
       +((A(j,81)%j+A(j,83)%j+A(j,89)%j+A(j,91)%j+A(j,93)%j+A(j,95)%j+A(j,129)%j+A(j,131)%j+A(j,137)%j+A(j,139)%j+A(j,141)%j &
       +A(j,143)%j)*f(3))/2._/**/REALKIND+((A(j,33)%j+A(j,35)%j+A(j,41)%j+A(j,43)%j+A(j,45)%j+A(j,47)%j+A(j,177)%j+A(j,179)%j &
       +A(j,185)%j+A(j,187)%j+A(j,189)%j+A(j,191)%j)*f(3))/6._/**/REALKIND

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
  use ol_colourmatrix_ppvvvj_bbbxbxzzz_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(2)
  real(REALKIND),    intent(out) :: M2colint(0:30-1)
  integer,           intent(in)  :: extcombs(:)
  integer :: extcomb, colmatpos, i, j, eco

  M2colint = 0

  do eco = 1, size(extcombs)
    extcomb = extcombs(eco)
    colmatpos = 2*extcomb
    do i = 1, 2
      do j = 1, 2
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
  use ol_colourmatrix_ppvvvj_bbbxbxzzz_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(2)
  complex(REALKIND), intent(in)  :: M2(2)
  real(REALKIND),    intent(out) :: M2colint
  integer :: i, j

  M2colint = 0

  do i = 1, 2
    do j = 1, 2
      M2colint = M2colint + real(conjg(M1(i))*K1(i,j)*M2(j))
    end do
  end do

end subroutine colintmunu

end subroutine amp2


#ifdef PRECISION_dp
subroutine colourvector(M1out, nhelout) &
  & bind(c,name="ol_tree_colvect_ppvvvj_bbbxbxzzz_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(2,432)
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
    & bind(c,name="ol_f_amp2tree_ppvvvj_bbbxbxzzz_1")
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
    & bind(c,name="ol_f_amp2ccone_ppvvvj_bbbxbxzzz_1")
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
    & bind(c,name="ol_f_amp2ccall_ppvvvj_bbbxbxzzz_1")
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
    & bind(c,name="ol_f_amp2hcone_ppvvvj_bbbxbxzzz_1")
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
    & bind(c,name="ol_f_amp2hcall_ppvvvj_bbbxbxzzz_1")
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
    & bind(c,name="ol_amp2tree_ppvvvj_bbbxbxzzz_1")
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
    & bind(c,name="ol_amp2ccone_ppvvvj_bbbxbxzzz_1")
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
    & bind(c,name="ol_amp2ccall_ppvvvj_bbbxbxzzz_1")
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
    & bind(c,name="ol_amp2hcone_ppvvvj_bbbxbxzzz_1")
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
    & bind(c,name="ol_amp2hcall_ppvvvj_bbbxbxzzz_1")
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
    & bind(c,name="amp2tree_ppvvvj_bbbxbxzzz_1_")
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
    & bind(c,name="amp2ccone_ppvvvj_bbbxbxzzz_1_")
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
    & bind(c,name="amp2ccall_ppvvvj_bbbxbxzzz_1_")
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
    & bind(c,name="amp2hcone_ppvvvj_bbbxbxzzz_1_")
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
    & bind(c,name="amp2hcall_ppvvvj_bbbxbxzzz_1_")
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

end module ol_tree_ppvvvj_bbbxbxzzz_1_/**/REALKIND
