
module ol_colourmatrix_ppvvvj_uuuxuxazz_1_/**/REALKIND
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
end module ol_colourmatrix_ppvvvj_uuuxuxazz_1_/**/REALKIND



module ol_forced_parameters_ppvvvj_uuuxuxazz_1_/**/REALKIND
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
  if (wMZ /= 0) write(*,101) 'wMZ = 0'
  if (wMZ /= 0) write(*,101) 'wMZ = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppvvvj_uuuxuxazz_1_/**/REALKIND

module ol_tree_ppvvvj_uuuxuxazz_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(1)
  complex(REALKIND), save :: den(388)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 288 ! number of helicity configurations
  integer(intkind2), save :: nhel = 288 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(288) ! physical helicity states
  complex(DREALKIND) :: M1helarr(2,288) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = (2*CI*eQED**3*gQCD**2)/3._/**/REALKIND

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,18))
  den(2) = 1 / (Q(5,5))
  den(3) = 1 / (Q(5,40))
  den(5) = 1 / (Q(5,82))
  den(9) = 1 / (Q(5,104))
  den(12) = 1 / (Q(5,72))
  den(14) = 1 / (Q(5,50))
  den(19) = 1 / (Q(5,13))
  den(23) = 1 / (Q(5,34))
  den(24) = 1 / (Q(5,24))
  den(26) = 1 / (Q(5,98))
  den(30) = 1 / (Q(5,88))
  den(33) = 1 / (Q(5,66))
  den(37) = 1 / (Q(5,56))
  den(40) = 1 / (Q(5,7))
  den(60) = 1 / (Q(5,17))
  den(61) = 1 / (Q(5,6))
  den(63) = 1 / (Q(5,81))
  den(69) = 1 / (Q(5,49))
  den(73) = 1 / (Q(5,14))
  den(77) = 1 / (Q(5,33))
  den(79) = 1 / (Q(5,97))
  den(84) = 1 / (Q(5,65))
  den(106) = 1 / (Q(5,9))
  den(107) = 1 / (Q(5,36))
  den(111) = 1 / (Q(5,100))
  den(114) = 1 / (Q(5,68))
  den(122) = 1 / (Q(5,20))
  den(126) = 1 / (Q(5,84))
  den(131) = 1 / (Q(5,52))
  den(134) = 1 / (Q(5,11))
  den(152) = 1 / (Q(5,10))
  den(186) = 1 / (Q(5,25))
  den(190) = 1 / (Q(5,42))
  den(194) = 1 / (Q(5,21))
  den(198) = 1 / (Q(5,38))
  den(208) = 1 / (Q(5,74))
  den(220) = 1 / (Q(5,70))
  den(238) = 1 / (Q(5,41))
  den(242) = 1 / (Q(5,26))
  den(246) = 1 / (Q(5,37))
  den(250) = 1 / (Q(5,22))
  den(258) = 1 / (Q(5,73))
  den(270) = 1 / (Q(5,69))

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
  den(20) = den(2)*den(19)
  den(21) = den(15)*den(20)
  den(22) = den(6)*den(20)
  den(25) = den(2)*den(24)
  den(27) = den(23)*den(26)
  den(28) = den(25)*den(27)
  den(29) = den(2)*den(23)
  den(31) = den(24)*den(30)
  den(32) = den(29)*den(31)
  den(34) = den(26)*den(33)
  den(35) = den(25)*den(34)
  den(36) = den(2)*den(33)
  den(38) = den(24)*den(37)
  den(39) = den(36)*den(38)
  den(41) = den(2)*den(40)
  den(42) = den(31)*den(41)
  den(43) = den(38)*den(41)
  den(44) = den(14)*den(23)
  den(45) = den(13)*den(44)
  den(46) = den(12)*den(30)
  den(47) = den(29)*den(46)
  den(48) = den(20)*den(44)
  den(49) = den(20)*den(27)
  den(50) = den(5)*den(33)
  den(51) = den(4)*den(50)
  den(52) = den(3)*den(37)
  den(53) = den(36)*den(52)
  den(54) = den(10)*den(41)
  den(55) = den(41)*den(52)
  den(56) = den(20)*den(50)
  den(57) = den(20)*den(34)
  den(58) = den(17)*den(41)
  den(59) = den(41)*den(46)
  den(62) = den(3)*den(61)
  den(64) = den(60)*den(63)
  den(65) = den(62)*den(64)
  den(66) = den(60)*den(61)
  den(67) = den(10)*den(66)
  den(68) = den(12)*den(61)
  den(70) = den(60)*den(69)
  den(71) = den(68)*den(70)
  den(72) = den(17)*den(66)
  den(74) = den(61)*den(73)
  den(75) = den(70)*den(74)
  den(76) = den(64)*den(74)
  den(78) = den(24)*den(61)
  den(80) = den(77)*den(79)
  den(81) = den(78)*den(80)
  den(82) = den(61)*den(77)
  den(83) = den(31)*den(82)
  den(85) = den(79)*den(84)
  den(86) = den(78)*den(85)
  den(87) = den(61)*den(84)
  den(88) = den(38)*den(87)
  den(89) = den(40)*den(61)
  den(90) = den(31)*den(89)
  den(91) = den(38)*den(89)
  den(92) = den(69)*den(77)
  den(93) = den(68)*den(92)
  den(94) = den(46)*den(82)
  den(95) = den(74)*den(92)
  den(96) = den(74)*den(80)
  den(97) = den(63)*den(84)
  den(98) = den(62)*den(97)
  den(99) = den(52)*den(87)
  den(100) = den(10)*den(89)
  den(101) = den(52)*den(89)
  den(102) = den(74)*den(97)
  den(103) = den(74)*den(85)
  den(104) = den(17)*den(89)
  den(105) = den(46)*den(89)
  den(108) = den(106)*den(107)
  den(109) = den(6)*den(108)
  den(110) = den(1)*den(106)
  den(112) = den(107)*den(111)
  den(113) = den(110)*den(112)
  den(115) = den(106)*den(114)
  den(116) = den(15)*den(115)
  den(117) = den(111)*den(114)
  den(118) = den(110)*den(117)
  den(119) = den(19)*den(106)
  den(120) = den(15)*den(119)
  den(121) = den(6)*den(119)
  den(123) = den(106)*den(122)
  den(124) = den(27)*den(123)
  den(125) = den(23)*den(106)
  den(127) = den(122)*den(126)
  den(128) = den(125)*den(127)
  den(129) = den(34)*den(123)
  den(130) = den(33)*den(106)
  den(132) = den(122)*den(131)
  den(133) = den(130)*den(132)
  den(135) = den(106)*den(134)
  den(136) = den(127)*den(135)
  den(137) = den(132)*den(135)
  den(138) = den(44)*den(115)
  den(139) = den(114)*den(126)
  den(140) = den(125)*den(139)
  den(141) = den(44)*den(119)
  den(142) = den(27)*den(119)
  den(143) = den(50)*den(108)
  den(144) = den(107)*den(131)
  den(145) = den(130)*den(144)
  den(146) = den(112)*den(135)
  den(147) = den(135)*den(144)
  den(148) = den(50)*den(119)
  den(149) = den(34)*den(119)
  den(150) = den(117)*den(135)
  den(151) = den(135)*den(139)
  den(153) = den(107)*den(152)
  den(154) = den(64)*den(153)
  den(155) = den(60)*den(152)
  den(156) = den(112)*den(155)
  den(157) = den(114)*den(152)
  den(158) = den(70)*den(157)
  den(159) = den(117)*den(155)
  den(160) = den(73)*den(152)
  den(161) = den(70)*den(160)
  den(162) = den(64)*den(160)
  den(163) = den(122)*den(152)
  den(164) = den(80)*den(163)
  den(165) = den(77)*den(152)
  den(166) = den(127)*den(165)
  den(167) = den(85)*den(163)
  den(168) = den(84)*den(152)
  den(169) = den(132)*den(168)
  den(170) = den(134)*den(152)
  den(171) = den(127)*den(170)
  den(172) = den(132)*den(170)
  den(173) = den(92)*den(157)
  den(174) = den(139)*den(165)
  den(175) = den(92)*den(160)
  den(176) = den(80)*den(160)
  den(177) = den(97)*den(153)
  den(178) = den(144)*den(168)
  den(179) = den(112)*den(170)
  den(180) = den(144)*den(170)
  den(181) = den(97)*den(160)
  den(182) = den(85)*den(160)
  den(183) = den(117)*den(170)
  den(184) = den(139)*den(170)
  den(185) = den(23)*den(114)
  den(187) = den(60)*den(186)
  den(188) = den(185)*den(187)
  den(189) = den(60)*den(114)
  den(191) = den(23)*den(190)
  den(192) = den(189)*den(191)
  den(193) = den(12)*den(23)
  den(195) = den(60)*den(194)
  den(196) = den(193)*den(195)
  den(197) = den(12)*den(60)
  den(199) = den(23)*den(198)
  den(200) = den(197)*den(199)
  den(201) = den(27)*den(195)
  den(202) = den(27)*den(187)
  den(203) = den(64)*den(199)
  den(204) = den(64)*den(191)
  den(205) = den(33)*den(107)
  den(206) = den(187)*den(205)
  den(207) = den(60)*den(107)
  den(209) = den(33)*den(208)
  den(210) = den(207)*den(209)
  den(211) = den(107)*den(198)
  den(212) = den(197)*den(211)
  den(213) = den(12)*den(208)
  den(214) = den(207)*den(213)
  den(215) = den(112)*den(187)
  den(216) = den(64)*den(211)
  den(217) = den(3)*den(33)
  den(218) = den(195)*den(217)
  den(219) = den(3)*den(60)
  den(221) = den(33)*den(220)
  den(222) = den(219)*den(221)
  den(223) = den(114)*den(220)
  den(224) = den(219)*den(223)
  den(225) = den(3)*den(190)
  den(226) = den(189)*den(225)
  den(227) = den(10)*den(195)
  den(228) = den(64)*den(225)
  den(229) = den(34)*den(195)
  den(230) = den(34)*den(187)
  den(231) = den(70)*den(221)
  den(232) = den(70)*den(209)
  den(233) = den(117)*den(187)
  den(234) = den(70)*den(223)
  den(235) = den(17)*den(195)
  den(236) = den(70)*den(213)
  den(237) = den(1)*den(114)
  den(239) = den(77)*den(238)
  den(240) = den(237)*den(239)
  den(241) = den(77)*den(114)
  den(243) = den(1)*den(242)
  den(244) = den(241)*den(243)
  den(245) = den(1)*den(12)
  den(247) = den(77)*den(246)
  den(248) = den(245)*den(247)
  den(249) = den(12)*den(77)
  den(251) = den(1)*den(250)
  den(252) = den(249)*den(251)
  den(253) = den(6)*den(247)
  den(254) = den(6)*den(239)
  den(255) = den(80)*den(251)
  den(256) = den(80)*den(243)
  den(257) = den(1)*den(107)
  den(259) = den(84)*den(258)
  den(260) = den(257)*den(259)
  den(261) = den(84)*den(107)
  den(262) = den(243)*den(261)
  den(263) = den(107)*den(246)
  den(264) = den(245)*den(263)
  den(265) = den(12)*den(258)
  den(266) = den(257)*den(265)
  den(267) = den(6)*den(263)
  den(268) = den(112)*den(243)
  den(269) = den(1)*den(3)
  den(271) = den(84)*den(270)
  den(272) = den(269)*den(271)
  den(273) = den(3)*den(84)
  den(274) = den(251)*den(273)
  den(275) = den(114)*den(270)
  den(276) = den(269)*den(275)
  den(277) = den(3)*den(238)
  den(278) = den(237)*den(277)
  den(279) = den(6)*den(277)
  den(280) = den(10)*den(251)
  den(281) = den(15)*den(271)
  den(282) = den(15)*den(259)
  den(283) = den(85)*den(251)
  den(284) = den(85)*den(243)
  den(285) = den(15)*den(275)
  den(286) = den(117)*den(243)
  den(287) = den(15)*den(265)
  den(288) = den(17)*den(251)
  den(289) = den(33)*den(122)
  den(290) = den(239)*den(289)
  den(291) = den(77)*den(122)
  den(292) = den(209)*den(291)
  den(293) = den(122)*den(250)
  den(294) = den(249)*den(293)
  den(295) = den(213)*den(291)
  den(296) = den(127)*den(239)
  den(297) = den(80)*den(293)
  den(298) = den(23)*den(122)
  den(299) = den(259)*den(298)
  den(300) = den(84)*den(122)
  den(301) = den(191)*den(300)
  den(302) = den(122)*den(194)
  den(303) = den(193)*den(302)
  den(304) = den(265)*den(298)
  den(305) = den(27)*den(302)
  den(306) = den(127)*den(191)
  den(307) = den(273)*den(293)
  den(308) = den(225)*den(300)
  den(309) = den(217)*den(302)
  den(310) = den(277)*den(289)
  den(311) = den(10)*den(302)
  den(312) = den(127)*den(277)
  den(313) = den(10)*den(293)
  den(314) = den(127)*den(225)
  den(315) = den(132)*den(259)
  den(316) = den(85)*den(293)
  den(317) = den(34)*den(302)
  den(318) = den(132)*den(209)
  den(319) = den(17)*den(302)
  den(320) = den(132)*den(265)
  den(321) = den(17)*den(293)
  den(322) = den(132)*den(213)
  den(323) = den(24)*den(33)
  den(324) = den(247)*den(323)
  den(325) = den(24)*den(77)
  den(326) = den(221)*den(325)
  den(327) = den(223)*den(325)
  den(328) = den(24)*den(242)
  den(329) = den(241)*den(328)
  den(330) = den(31)*den(247)
  den(331) = den(80)*den(328)
  den(332) = den(23)*den(24)
  den(333) = den(271)*den(332)
  den(334) = den(24)*den(84)
  den(335) = den(199)*den(334)
  den(336) = den(275)*den(332)
  den(337) = den(24)*den(186)
  den(338) = den(185)*den(337)
  den(339) = den(27)*den(337)
  den(340) = den(31)*den(199)
  den(341) = den(211)*den(334)
  den(342) = den(261)*den(328)
  den(343) = den(263)*den(323)
  den(344) = den(205)*den(337)
  den(345) = den(31)*den(263)
  den(346) = den(112)*den(337)
  den(347) = den(31)*den(211)
  den(348) = den(112)*den(328)
  den(349) = den(38)*den(271)
  den(350) = den(85)*den(328)
  den(351) = den(34)*den(337)
  den(352) = den(38)*den(221)
  den(353) = den(38)*den(275)
  den(354) = den(117)*den(337)
  den(355) = den(38)*den(223)
  den(356) = den(117)*den(328)
  den(357) = den(50)*den(247)
  den(358) = den(50)*den(239)
  den(359) = den(92)*den(221)
  den(360) = den(92)*den(209)
  den(361) = den(139)*den(239)
  den(362) = den(92)*den(223)
  den(363) = den(46)*den(247)
  den(364) = den(92)*den(213)
  den(365) = den(44)*den(271)
  den(366) = den(44)*den(259)
  den(367) = den(97)*den(199)
  den(368) = den(97)*den(191)
  den(369) = den(44)*den(275)
  den(370) = den(139)*den(191)
  den(371) = den(44)*den(265)
  den(372) = den(46)*den(199)
  den(373) = den(144)*den(259)
  den(374) = den(97)*den(211)
  den(375) = den(50)*den(263)
  den(376) = den(144)*den(209)
  den(377) = den(46)*den(263)
  den(378) = den(144)*den(265)
  den(379) = den(46)*den(211)
  den(380) = den(144)*den(213)
  den(381) = den(52)*den(271)
  den(382) = den(97)*den(225)
  den(383) = den(50)*den(277)
  den(384) = den(52)*den(221)
  den(385) = den(52)*den(275)
  den(386) = den(139)*den(277)
  den(387) = den(52)*den(223)
  den(388) = den(139)*den(225)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_ppvvvj_uuuxuxazz_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_ppvvvj_uuuxuxazz_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for up up anti-up anti-up gamma Z Z -> 0
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
  use ol_external_ppvvvj_uuuxuxazz_1, only: external_perm_ppvvvj_uuuxuxazz_1, &
    & external_perm_inv_ppvvvj_uuuxuxazz_1, extcomb_perm_ppvvvj_uuuxuxazz_1, &
    & average_factor_ppvvvj_uuuxuxazz_1
  use ol_external_ppvvvj_uuuxuxazz_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_ppvvvj_uuuxuxazz_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_ppvvvj_uuuxuxazz_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_ppvvvj_uuuxuxazz_1
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
  complex(REALKIND) :: MOM_LC(4), M1(2), M1helarray(2,288)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(2), ex6(3), ex7(3)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,12), wf6(6,16), wf8(8,24), wf12(12,48), wf16(16,32), wf18(18,16), wf24(24,80), wf36(36,8), wf288(288,240)

  type(polcont) :: A(288,240)
  complex(REALKIND) :: Aj(240)

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
  extmasses2 = [ rZERO2, rZERO2, rZERO2, rZERO2, rZERO2, rMZ2, rMZ2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_ppvvvj_uuuxuxazz_1,7)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,7)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_ppvvvj_uuuxuxazz_1(I)
  else if (I < 0) then
    JBmunu = external_perm_ppvvvj_uuuxuxazz_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_ppvvvj_uuuxuxazz_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_Q(P(:,1), rZERO, H1, ex1)
  call wf_Q(P(:,2), rZERO, H2, ex2)
  call wf_A(P(:,3), rZERO, H3, ex3)
  call wf_A(P(:,4), rZERO, H4, ex4)
  call wf_V(P(:,5), rZERO, H5, ex5)
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
  call vert_VQ_A(ntry, ex5, ex2, wf4(:,2), n3(:,2), t3x4(:,:,2))
  call vert_AZ_Q(gZu,ntry, ex4, ex6, wf6(:,1), n3(:,3), t3x6(:,:,1))
  call prop_Q_A(ntry, wf4(:,2), Q(:,18), ZERO, 0_intkind1, wf4(:,3), n2(1))
  call prop_A_Q(ntry, wf6(:,1), Q(:,40), ZERO, 0_intkind1, wf6(:,2), n2(2))
  call vert_ZQ_A(gZu,ntry, ex7, wf4(:,3), wf12(:,1), n3(:,4), t3x12(:,:,1))
  call vert_AV_Q(ntry, wf6(:,2), wf4(:,1), wf24(:,1), n3(:,5), t3x24(:,:,1))
  call prop_Q_A(ntry, wf12(:,1), Q(:,82), ZERO, 0_intkind1, wf12(:,2), n2(3))
  call vert_AZ_Q(gZu,ntry, wf6(:,2), ex7, wf18(:,1), n3(:,6), t3x18(:,:,1))
  call vert_VQ_A(ntry, wf4(:,1), wf4(:,3), wf16(:,1), n3(:,7), t3x16(:,:,1))
  call prop_A_Q(ntry, wf18(:,1), Q(:,104), ZERO, 0_intkind1, wf18(:,2), n2(4))
  call vert_AZ_Q(gZu,ntry, ex4, ex7, wf6(:,3), n3(:,8), t3x6(:,:,2))
  call prop_A_Q(ntry, wf6(:,3), Q(:,72), ZERO, 0_intkind1, wf6(:,4), n2(5))
  call vert_ZQ_A(gZu,ntry, ex6, wf4(:,3), wf12(:,3), n3(:,9), t3x12(:,:,2))
  call vert_AV_Q(ntry, wf6(:,4), wf4(:,1), wf24(:,2), n3(:,10), t3x24(:,:,2))
  call prop_Q_A(ntry, wf12(:,3), Q(:,50), ZERO, 0_intkind1, wf12(:,4), n2(6))
  call vert_AZ_Q(gZu,ntry, wf6(:,4), ex6, wf18(:,3), n3(:,11), t3x18(:,:,2))
  call prop_A_Q(ntry, wf18(:,3), Q(:,104), ZERO, 0_intkind1, wf18(:,4), n2(7))
  call vert_AV_Q(ntry, ex4, wf4(:,1), wf8(:,1), n3(:,12), t3x8(:,:,1))
  call prop_A_Q(ntry, wf8(:,1), Q(:,13), ZERO, 0_intkind1, wf8(:,2), n2(8))
  call vert_AZ_Q(gZu,ntry, wf8(:,2), ex7, wf24(:,3), n3(:,13), t3x24(:,:,3))
  call vert_AZ_Q(gZu,ntry, wf8(:,2), ex6, wf24(:,4), n3(:,14), t3x24(:,:,4))
  call vert_ZQ_A(gZu,ntry, ex6, ex2, wf6(:,5), n3(:,15), t3x6(:,:,3))
  call vert_AV_Q(ntry, ex4, ex5, wf4(:,4), n3(:,16), t3x4(:,:,3))
  call prop_Q_A(ntry, wf6(:,5), Q(:,34), ZERO, 0_intkind1, wf6(:,6), n2(9))
  call prop_A_Q(ntry, wf4(:,4), Q(:,24), ZERO, 0_intkind1, wf4(:,5), n2(10))
  call vert_ZQ_A(gZu,ntry, ex7, wf6(:,6), wf18(:,5), n3(:,17), t3x18(:,:,3))
  call vert_AV_Q(ntry, wf4(:,5), wf4(:,1), wf16(:,2), n3(:,18), t3x16(:,:,2))
  call prop_Q_A(ntry, wf18(:,5), Q(:,98), ZERO, 0_intkind1, wf18(:,6), n2(11))
  call vert_AZ_Q(gZu,ntry, wf4(:,5), ex7, wf12(:,5), n3(:,19), t3x12(:,:,3))
  call vert_VQ_A(ntry, wf4(:,1), wf6(:,6), wf24(:,5), n3(:,20), t3x24(:,:,5))
  call prop_A_Q(ntry, wf12(:,5), Q(:,88), ZERO, 0_intkind1, wf12(:,6), n2(12))
  call vert_ZQ_A(gZu,ntry, ex7, ex2, wf6(:,7), n3(:,21), t3x6(:,:,4))
  call prop_Q_A(ntry, wf6(:,7), Q(:,66), ZERO, 0_intkind1, wf6(:,8), n2(13))
  call vert_ZQ_A(gZu,ntry, ex6, wf6(:,8), wf18(:,7), n3(:,22), t3x18(:,:,4))
  call prop_Q_A(ntry, wf18(:,7), Q(:,98), ZERO, 0_intkind1, wf18(:,8), n2(14))
  call vert_AZ_Q(gZu,ntry, wf4(:,5), ex6, wf12(:,7), n3(:,23), t3x12(:,:,4))
  call vert_VQ_A(ntry, wf4(:,1), wf6(:,8), wf24(:,6), n3(:,24), t3x24(:,:,6))
  call prop_A_Q(ntry, wf12(:,7), Q(:,56), ZERO, 0_intkind1, wf12(:,8), n2(15))
  call vert_VQ_A(ntry, wf4(:,1), ex2, wf8(:,3), n3(:,25), t3x8(:,:,2))
  call prop_Q_A(ntry, wf8(:,3), Q(:,7), ZERO, 0_intkind1, wf8(:,4), n2(16))
  call vert_ZQ_A(gZu,ntry, ex6, wf8(:,4), wf24(:,7), n3(:,26), t3x24(:,:,7))
  call vert_ZQ_A(gZu,ntry, ex7, wf8(:,4), wf24(:,8), n3(:,27), t3x24(:,:,8))
  call vert_VQ_A(ntry, ex5, wf6(:,6), wf12(:,9), n3(:,28), t3x12(:,:,5))
  call prop_Q_A(ntry, wf12(:,9), Q(:,50), ZERO, 0_intkind1, wf12(:,10), n2(17))
  call vert_AV_Q(ntry, wf6(:,4), ex5, wf12(:,11), n3(:,29), t3x12(:,:,6))
  call prop_A_Q(ntry, wf12(:,11), Q(:,88), ZERO, 0_intkind1, wf12(:,12), n2(18))
  call vert_AV_Q(ntry, wf8(:,2), ex5, wf16(:,3), n3(:,30), t3x16(:,:,3))
  call vert_VQ_A(ntry, ex5, wf6(:,8), wf12(:,13), n3(:,31), t3x12(:,:,7))
  call prop_Q_A(ntry, wf12(:,13), Q(:,82), ZERO, 0_intkind1, wf12(:,14), n2(19))
  call vert_AV_Q(ntry, wf6(:,2), ex5, wf12(:,15), n3(:,32), t3x12(:,:,8))
  call prop_A_Q(ntry, wf12(:,15), Q(:,56), ZERO, 0_intkind1, wf12(:,16), n2(20))
  call vert_VQ_A(ntry, ex5, wf8(:,4), wf16(:,4), n3(:,33), t3x16(:,:,4))
  call vert_VQ_A(ntry, ex5, ex1, wf4(:,6), n3(:,34), t3x4(:,:,4))
  call vert_QA_V(ntry, ex2, ex3, wf4(:,7), n3(:,35), t3x4(:,:,5))
  call prop_Q_A(ntry, wf4(:,6), Q(:,17), ZERO, 0_intkind1, wf4(:,8), n2(21))
  call vert_ZQ_A(gZu,ntry, ex7, wf4(:,8), wf12(:,17), n3(:,36), t3x12(:,:,9))
  call vert_AV_Q(ntry, wf6(:,2), wf4(:,7), wf24(:,9), n3(:,37), t3x24(:,:,9))
  call prop_Q_A(ntry, wf12(:,17), Q(:,81), ZERO, 0_intkind1, wf12(:,18), n2(22))
  call vert_VQ_A(ntry, wf4(:,7), wf4(:,8), wf16(:,5), n3(:,38), t3x16(:,:,5))
  call vert_ZQ_A(gZu,ntry, ex6, wf4(:,8), wf12(:,19), n3(:,39), t3x12(:,:,10))
  call vert_AV_Q(ntry, wf6(:,4), wf4(:,7), wf24(:,10), n3(:,40), t3x24(:,:,10))
  call prop_Q_A(ntry, wf12(:,19), Q(:,49), ZERO, 0_intkind1, wf12(:,20), n2(23))
  call vert_AV_Q(ntry, ex4, wf4(:,7), wf8(:,5), n3(:,41), t3x8(:,:,3))
  call prop_A_Q(ntry, wf8(:,5), Q(:,14), ZERO, 0_intkind1, wf8(:,6), n2(24))
  call vert_AZ_Q(gZu,ntry, wf8(:,6), ex7, wf24(:,11), n3(:,42), t3x24(:,:,11))
  call vert_AZ_Q(gZu,ntry, wf8(:,6), ex6, wf24(:,12), n3(:,43), t3x24(:,:,12))
  call vert_ZQ_A(gZu,ntry, ex6, ex1, wf6(:,9), n3(:,44), t3x6(:,:,5))
  call prop_Q_A(ntry, wf6(:,9), Q(:,33), ZERO, 0_intkind1, wf6(:,10), n2(25))
  call vert_ZQ_A(gZu,ntry, ex7, wf6(:,10), wf18(:,9), n3(:,45), t3x18(:,:,5))
  call vert_AV_Q(ntry, wf4(:,5), wf4(:,7), wf16(:,6), n3(:,46), t3x16(:,:,6))
  call prop_Q_A(ntry, wf18(:,9), Q(:,97), ZERO, 0_intkind1, wf18(:,10), n2(26))
  call vert_VQ_A(ntry, wf4(:,7), wf6(:,10), wf24(:,13), n3(:,47), t3x24(:,:,13))
  call vert_ZQ_A(gZu,ntry, ex7, ex1, wf6(:,11), n3(:,48), t3x6(:,:,6))
  call prop_Q_A(ntry, wf6(:,11), Q(:,65), ZERO, 0_intkind1, wf6(:,12), n2(27))
  call vert_ZQ_A(gZu,ntry, ex6, wf6(:,12), wf18(:,11), n3(:,49), t3x18(:,:,6))
  call prop_Q_A(ntry, wf18(:,11), Q(:,97), ZERO, 0_intkind1, wf18(:,12), n2(28))
  call vert_VQ_A(ntry, wf4(:,7), wf6(:,12), wf24(:,14), n3(:,50), t3x24(:,:,14))
  call vert_VQ_A(ntry, wf4(:,7), ex1, wf8(:,7), n3(:,51), t3x8(:,:,4))
  call prop_Q_A(ntry, wf8(:,7), Q(:,7), ZERO, 0_intkind1, wf8(:,8), n2(29))
  call vert_ZQ_A(gZu,ntry, ex6, wf8(:,8), wf24(:,15), n3(:,52), t3x24(:,:,15))
  call vert_ZQ_A(gZu,ntry, ex7, wf8(:,8), wf24(:,16), n3(:,53), t3x24(:,:,16))
  call vert_VQ_A(ntry, ex5, wf6(:,10), wf12(:,21), n3(:,54), t3x12(:,:,11))
  call prop_Q_A(ntry, wf12(:,21), Q(:,49), ZERO, 0_intkind1, wf12(:,22), n2(30))
  call vert_AV_Q(ntry, wf8(:,6), ex5, wf16(:,7), n3(:,55), t3x16(:,:,7))
  call vert_VQ_A(ntry, ex5, wf6(:,12), wf12(:,23), n3(:,56), t3x12(:,:,12))
  call prop_Q_A(ntry, wf12(:,23), Q(:,81), ZERO, 0_intkind1, wf12(:,24), n2(31))
  call vert_VQ_A(ntry, ex5, wf8(:,8), wf16(:,8), n3(:,57), t3x16(:,:,8))
  call vert_QA_V(ntry, ex1, ex4, wf4(:,9), n3(:,58), t3x4(:,:,6))
  call vert_AZ_Q(gZu,ntry, ex3, ex6, wf6(:,13), n3(:,59), t3x6(:,:,7))
  call prop_A_Q(ntry, wf6(:,13), Q(:,36), ZERO, 0_intkind1, wf6(:,14), n2(32))
  call vert_AV_Q(ntry, wf6(:,14), wf4(:,9), wf24(:,17), n3(:,60), t3x24(:,:,17))
  call vert_AZ_Q(gZu,ntry, wf6(:,14), ex7, wf18(:,13), n3(:,61), t3x18(:,:,7))
  call vert_VQ_A(ntry, wf4(:,9), wf4(:,3), wf16(:,9), n3(:,62), t3x16(:,:,9))
  call prop_A_Q(ntry, wf18(:,13), Q(:,100), ZERO, 0_intkind1, wf18(:,14), n2(33))
  call vert_AZ_Q(gZu,ntry, ex3, ex7, wf6(:,15), n3(:,63), t3x6(:,:,8))
  call prop_A_Q(ntry, wf6(:,15), Q(:,68), ZERO, 0_intkind1, wf6(:,16), n2(34))
  call vert_AV_Q(ntry, wf6(:,16), wf4(:,9), wf24(:,18), n3(:,64), t3x24(:,:,18))
  call vert_AZ_Q(gZu,ntry, wf6(:,16), ex6, wf18(:,15), n3(:,65), t3x18(:,:,8))
  call prop_A_Q(ntry, wf18(:,15), Q(:,100), ZERO, 0_intkind1, wf18(:,16), n2(35))
  call vert_AV_Q(ntry, ex3, wf4(:,9), wf8(:,9), n3(:,66), t3x8(:,:,5))
  call prop_A_Q(ntry, wf8(:,9), Q(:,13), ZERO, 0_intkind1, wf8(:,10), n2(36))
  call vert_AZ_Q(gZu,ntry, wf8(:,10), ex7, wf24(:,19), n3(:,67), t3x24(:,:,19))
  call vert_AZ_Q(gZu,ntry, wf8(:,10), ex6, wf24(:,20), n3(:,68), t3x24(:,:,20))
  call vert_AV_Q(ntry, ex3, ex5, wf4(:,10), n3(:,69), t3x4(:,:,7))
  call prop_A_Q(ntry, wf4(:,10), Q(:,20), ZERO, 0_intkind1, wf4(:,11), n2(37))
  call vert_AV_Q(ntry, wf4(:,11), wf4(:,9), wf16(:,10), n3(:,70), t3x16(:,:,10))
  call vert_AZ_Q(gZu,ntry, wf4(:,11), ex7, wf12(:,25), n3(:,71), t3x12(:,:,13))
  call vert_VQ_A(ntry, wf4(:,9), wf6(:,6), wf24(:,21), n3(:,72), t3x24(:,:,21))
  call prop_A_Q(ntry, wf12(:,25), Q(:,84), ZERO, 0_intkind1, wf12(:,26), n2(38))
  call vert_AZ_Q(gZu,ntry, wf4(:,11), ex6, wf12(:,27), n3(:,73), t3x12(:,:,14))
  call vert_VQ_A(ntry, wf4(:,9), wf6(:,8), wf24(:,22), n3(:,74), t3x24(:,:,22))
  call prop_A_Q(ntry, wf12(:,27), Q(:,52), ZERO, 0_intkind1, wf12(:,28), n2(39))
  call vert_VQ_A(ntry, wf4(:,9), ex2, wf8(:,11), n3(:,75), t3x8(:,:,6))
  call prop_Q_A(ntry, wf8(:,11), Q(:,11), ZERO, 0_intkind1, wf8(:,12), n2(40))
  call vert_ZQ_A(gZu,ntry, ex6, wf8(:,12), wf24(:,23), n3(:,76), t3x24(:,:,23))
  call vert_ZQ_A(gZu,ntry, ex7, wf8(:,12), wf24(:,24), n3(:,77), t3x24(:,:,24))
  call vert_AV_Q(ntry, wf6(:,16), ex5, wf12(:,29), n3(:,78), t3x12(:,:,15))
  call prop_A_Q(ntry, wf12(:,29), Q(:,84), ZERO, 0_intkind1, wf12(:,30), n2(41))
  call vert_AV_Q(ntry, wf8(:,10), ex5, wf16(:,11), n3(:,79), t3x16(:,:,11))
  call vert_AV_Q(ntry, wf6(:,14), ex5, wf12(:,31), n3(:,80), t3x12(:,:,16))
  call prop_A_Q(ntry, wf12(:,31), Q(:,52), ZERO, 0_intkind1, wf12(:,32), n2(42))
  call vert_VQ_A(ntry, ex5, wf8(:,12), wf16(:,12), n3(:,81), t3x16(:,:,12))
  call vert_QA_V(ntry, ex2, ex4, wf4(:,12), n3(:,82), t3x4(:,:,8))
  call vert_AV_Q(ntry, wf6(:,14), wf4(:,12), wf24(:,25), n3(:,83), t3x24(:,:,25))
  call vert_VQ_A(ntry, wf4(:,12), wf4(:,8), wf16(:,13), n3(:,84), t3x16(:,:,13))
  call vert_AV_Q(ntry, wf6(:,16), wf4(:,12), wf24(:,26), n3(:,85), t3x24(:,:,26))
  call vert_AV_Q(ntry, ex3, wf4(:,12), wf8(:,13), n3(:,86), t3x8(:,:,7))
  call prop_A_Q(ntry, wf8(:,13), Q(:,14), ZERO, 0_intkind1, wf8(:,14), n2(43))
  call vert_AZ_Q(gZu,ntry, wf8(:,14), ex7, wf24(:,27), n3(:,87), t3x24(:,:,27))
  call vert_AZ_Q(gZu,ntry, wf8(:,14), ex6, wf24(:,28), n3(:,88), t3x24(:,:,28))
  call vert_AV_Q(ntry, wf4(:,11), wf4(:,12), wf16(:,14), n3(:,89), t3x16(:,:,14))
  call vert_VQ_A(ntry, wf4(:,12), wf6(:,10), wf24(:,29), n3(:,90), t3x24(:,:,29))
  call vert_VQ_A(ntry, wf4(:,12), wf6(:,12), wf24(:,30), n3(:,91), t3x24(:,:,30))
  call vert_VQ_A(ntry, wf4(:,12), ex1, wf8(:,15), n3(:,92), t3x8(:,:,8))
  call prop_Q_A(ntry, wf8(:,15), Q(:,11), ZERO, 0_intkind1, wf8(:,16), n2(44))
  call vert_ZQ_A(gZu,ntry, ex6, wf8(:,16), wf24(:,31), n3(:,93), t3x24(:,:,31))
  call vert_ZQ_A(gZu,ntry, ex7, wf8(:,16), wf24(:,32), n3(:,94), t3x24(:,:,32))
  call vert_AV_Q(ntry, wf8(:,14), ex5, wf16(:,15), n3(:,95), t3x16(:,:,15))
  call vert_VQ_A(ntry, ex5, wf8(:,16), wf16(:,16), n3(:,96), t3x16(:,:,16))
  call vert_QA_V(ntry, wf4(:,8), ex4, wf8(:,17), n3(:,97), t3x8(:,:,9))
  call vert_QA_V(ntry, wf6(:,6), wf6(:,16), wf36(:,1), n3(:,98), t3x36(:,:,1))
  call vert_QA_V(ntry, wf6(:,6), ex4, wf12(:,33), n3(:,99), t3x12(:,:,17))
  call vert_QA_V(ntry, wf4(:,8), wf6(:,16), wf24(:,33), n3(:,100), t3x24(:,:,33))
  call vert_QA_V(ntry, wf4(:,8), ex3, wf8(:,18), n3(:,101), t3x8(:,:,10))
  call vert_QA_V(ntry, wf6(:,6), wf6(:,4), wf36(:,2), n3(:,102), t3x36(:,:,2))
  call vert_QA_V(ntry, wf6(:,6), ex3, wf12(:,34), n3(:,103), t3x12(:,:,18))
  call vert_QA_V(ntry, wf4(:,8), wf6(:,4), wf24(:,34), n3(:,104), t3x24(:,:,34))
  call vert_AV_Q(ntry, ex4, wf8(:,18), wf16(:,17), n3(:,105), t3x16(:,:,17))
  call vert_AV_Q(ntry, ex3, wf8(:,17), wf16(:,18), n3(:,106), t3x16(:,:,18))
  call vert_AV_Q(ntry, ex4, wf12(:,34), wf24(:,35), n3(:,107), t3x24(:,:,35))
  call vert_AV_Q(ntry, ex3, wf12(:,33), wf24(:,36), n3(:,108), t3x24(:,:,36))
  call vert_QA_V(ntry, wf6(:,8), wf6(:,14), wf36(:,3), n3(:,109), t3x36(:,:,3))
  call vert_QA_V(ntry, wf6(:,8), ex4, wf12(:,35), n3(:,110), t3x12(:,:,19))
  call vert_QA_V(ntry, wf4(:,8), wf6(:,14), wf24(:,37), n3(:,111), t3x24(:,:,37))
  call vert_QA_V(ntry, ex2, wf6(:,14), wf12(:,36), n3(:,112), t3x12(:,:,20))
  call vert_QA_V(ntry, ex2, wf6(:,4), wf12(:,37), n3(:,113), t3x12(:,:,21))
  call vert_VQ_A(ntry, wf8(:,17), ex2, wf16(:,19), n3(:,114), t3x16(:,:,19))
  call vert_AV_Q(ntry, ex4, wf12(:,36), wf24(:,38), n3(:,115), t3x24(:,:,38))
  call vert_QA_V(ntry, wf6(:,8), wf6(:,2), wf36(:,4), n3(:,116), t3x36(:,:,4))
  call vert_QA_V(ntry, wf6(:,8), ex3, wf12(:,38), n3(:,117), t3x12(:,:,22))
  call vert_QA_V(ntry, wf4(:,8), wf6(:,2), wf24(:,39), n3(:,118), t3x24(:,:,39))
  call vert_QA_V(ntry, ex2, wf6(:,16), wf12(:,39), n3(:,119), t3x12(:,:,23))
  call vert_QA_V(ntry, ex2, wf6(:,2), wf12(:,40), n3(:,120), t3x12(:,:,24))
  call vert_VQ_A(ntry, wf8(:,18), ex2, wf16(:,20), n3(:,121), t3x16(:,:,20))
  call vert_AV_Q(ntry, ex3, wf12(:,40), wf24(:,40), n3(:,122), t3x24(:,:,40))
  call vert_AV_Q(ntry, ex4, wf12(:,38), wf24(:,41), n3(:,123), t3x24(:,:,41))
  call vert_AV_Q(ntry, ex3, wf12(:,35), wf24(:,42), n3(:,124), t3x24(:,:,42))
  call vert_AV_Q(ntry, ex4, wf12(:,39), wf24(:,43), n3(:,125), t3x24(:,:,43))
  call vert_AV_Q(ntry, ex3, wf12(:,37), wf24(:,44), n3(:,126), t3x24(:,:,44))
  call vert_QA_V(ntry, wf6(:,10), ex4, wf12(:,41), n3(:,127), t3x12(:,:,25))
  call vert_QA_V(ntry, wf4(:,3), wf6(:,16), wf24(:,45), n3(:,128), t3x24(:,:,45))
  call vert_QA_V(ntry, wf4(:,3), ex4, wf8(:,19), n3(:,129), t3x8(:,:,11))
  call vert_QA_V(ntry, wf6(:,10), wf6(:,16), wf36(:,5), n3(:,130), t3x36(:,:,5))
  call vert_QA_V(ntry, wf6(:,10), ex3, wf12(:,42), n3(:,131), t3x12(:,:,26))
  call vert_QA_V(ntry, wf4(:,3), wf6(:,4), wf24(:,46), n3(:,132), t3x24(:,:,46))
  call vert_QA_V(ntry, wf4(:,3), ex3, wf8(:,20), n3(:,133), t3x8(:,:,12))
  call vert_QA_V(ntry, wf6(:,10), wf6(:,4), wf36(:,6), n3(:,134), t3x36(:,:,6))
  call vert_AV_Q(ntry, ex4, wf12(:,42), wf24(:,47), n3(:,135), t3x24(:,:,47))
  call vert_AV_Q(ntry, ex3, wf12(:,41), wf24(:,48), n3(:,136), t3x24(:,:,48))
  call vert_AV_Q(ntry, ex4, wf8(:,20), wf16(:,21), n3(:,137), t3x16(:,:,21))
  call vert_AV_Q(ntry, ex3, wf8(:,19), wf16(:,22), n3(:,138), t3x16(:,:,22))
  call vert_QA_V(ntry, wf6(:,12), ex4, wf12(:,43), n3(:,139), t3x12(:,:,27))
  call vert_QA_V(ntry, wf4(:,3), wf6(:,14), wf24(:,49), n3(:,140), t3x24(:,:,49))
  call vert_QA_V(ntry, wf6(:,12), wf6(:,14), wf36(:,7), n3(:,141), t3x36(:,:,7))
  call vert_QA_V(ntry, ex1, wf6(:,14), wf12(:,44), n3(:,142), t3x12(:,:,28))
  call vert_QA_V(ntry, ex1, wf6(:,4), wf12(:,45), n3(:,143), t3x12(:,:,29))
  call vert_AV_Q(ntry, ex4, wf12(:,44), wf24(:,50), n3(:,144), t3x24(:,:,50))
  call vert_VQ_A(ntry, wf8(:,19), ex1, wf16(:,23), n3(:,145), t3x16(:,:,23))
  call vert_QA_V(ntry, wf6(:,12), ex3, wf12(:,46), n3(:,146), t3x12(:,:,30))
  call vert_QA_V(ntry, wf4(:,3), wf6(:,2), wf24(:,51), n3(:,147), t3x24(:,:,51))
  call vert_QA_V(ntry, wf6(:,12), wf6(:,2), wf36(:,8), n3(:,148), t3x36(:,:,8))
  call vert_QA_V(ntry, ex1, wf6(:,16), wf12(:,47), n3(:,149), t3x12(:,:,31))
  call vert_QA_V(ntry, ex1, wf6(:,2), wf12(:,48), n3(:,150), t3x12(:,:,32))
  call vert_AV_Q(ntry, ex3, wf12(:,48), wf24(:,52), n3(:,151), t3x24(:,:,52))
  call vert_VQ_A(ntry, wf8(:,20), ex1, wf16(:,24), n3(:,152), t3x16(:,:,24))
  call vert_AV_Q(ntry, ex4, wf12(:,46), wf24(:,53), n3(:,153), t3x24(:,:,53))
  call vert_AV_Q(ntry, ex3, wf12(:,43), wf24(:,54), n3(:,154), t3x24(:,:,54))
  call vert_AV_Q(ntry, ex4, wf12(:,47), wf24(:,55), n3(:,155), t3x24(:,:,55))
  call vert_AV_Q(ntry, ex3, wf12(:,45), wf24(:,56), n3(:,156), t3x24(:,:,56))
  call vert_QA_V(ntry, wf6(:,8), wf4(:,11), wf24(:,57), n3(:,157), t3x24(:,:,57))
  call vert_QA_V(ntry, wf6(:,10), wf4(:,11), wf24(:,58), n3(:,158), t3x24(:,:,58))
  call vert_QA_V(ntry, ex2, wf4(:,11), wf8(:,21), n3(:,159), t3x8(:,:,13))
  call vert_VQ_A(ntry, wf12(:,41), ex2, wf24(:,59), n3(:,160), t3x24(:,:,59))
  call vert_AV_Q(ntry, ex4, wf8(:,21), wf16(:,25), n3(:,161), t3x16(:,:,25))
  call vert_QA_V(ntry, wf6(:,6), wf4(:,11), wf24(:,60), n3(:,162), t3x24(:,:,60))
  call vert_QA_V(ntry, wf6(:,12), wf4(:,11), wf24(:,61), n3(:,163), t3x24(:,:,61))
  call vert_QA_V(ntry, ex1, wf4(:,11), wf8(:,22), n3(:,164), t3x8(:,:,14))
  call vert_AV_Q(ntry, ex4, wf8(:,22), wf16(:,26), n3(:,165), t3x16(:,:,26))
  call vert_VQ_A(ntry, wf12(:,33), ex1, wf24(:,62), n3(:,166), t3x24(:,:,62))
  call vert_VQ_A(ntry, wf8(:,22), ex2, wf16(:,27), n3(:,167), t3x16(:,:,27))
  call vert_VQ_A(ntry, wf12(:,48), ex2, wf24(:,63), n3(:,168), t3x24(:,:,63))
  call vert_VQ_A(ntry, wf8(:,21), ex1, wf16(:,28), n3(:,169), t3x16(:,:,28))
  call vert_VQ_A(ntry, wf12(:,40), ex1, wf24(:,64), n3(:,170), t3x24(:,:,64))
  call vert_VQ_A(ntry, wf12(:,43), ex2, wf24(:,65), n3(:,171), t3x24(:,:,65))
  call vert_VQ_A(ntry, wf12(:,35), ex1, wf24(:,66), n3(:,172), t3x24(:,:,66))
  call vert_VQ_A(ntry, wf12(:,45), ex2, wf24(:,67), n3(:,173), t3x24(:,:,67))
  call vert_VQ_A(ntry, wf12(:,37), ex1, wf24(:,68), n3(:,174), t3x24(:,:,68))
  call vert_QA_V(ntry, wf6(:,8), wf4(:,5), wf24(:,69), n3(:,175), t3x24(:,:,69))
  call vert_QA_V(ntry, wf6(:,10), wf4(:,5), wf24(:,70), n3(:,176), t3x24(:,:,70))
  call vert_QA_V(ntry, ex2, wf4(:,5), wf8(:,23), n3(:,177), t3x8(:,:,15))
  call vert_VQ_A(ntry, wf12(:,42), ex2, wf24(:,71), n3(:,178), t3x24(:,:,71))
  call vert_AV_Q(ntry, ex3, wf8(:,23), wf16(:,29), n3(:,179), t3x16(:,:,29))
  call vert_QA_V(ntry, wf6(:,6), wf4(:,5), wf24(:,72), n3(:,180), t3x24(:,:,72))
  call vert_QA_V(ntry, wf6(:,12), wf4(:,5), wf24(:,73), n3(:,181), t3x24(:,:,73))
  call vert_QA_V(ntry, ex1, wf4(:,5), wf8(:,24), n3(:,182), t3x8(:,:,16))
  call vert_AV_Q(ntry, ex3, wf8(:,24), wf16(:,30), n3(:,183), t3x16(:,:,30))
  call vert_VQ_A(ntry, wf12(:,34), ex1, wf24(:,74), n3(:,184), t3x24(:,:,74))
  call vert_VQ_A(ntry, wf12(:,44), ex2, wf24(:,75), n3(:,185), t3x24(:,:,75))
  call vert_VQ_A(ntry, wf8(:,24), ex2, wf16(:,31), n3(:,186), t3x16(:,:,31))
  call vert_VQ_A(ntry, wf12(:,36), ex1, wf24(:,76), n3(:,187), t3x24(:,:,76))
  call vert_VQ_A(ntry, wf8(:,23), ex1, wf16(:,32), n3(:,188), t3x16(:,:,32))
  call vert_VQ_A(ntry, wf12(:,46), ex2, wf24(:,77), n3(:,189), t3x24(:,:,77))
  call vert_VQ_A(ntry, wf12(:,38), ex1, wf24(:,78), n3(:,190), t3x24(:,:,78))
  call vert_VQ_A(ntry, wf12(:,47), ex2, wf24(:,79), n3(:,191), t3x24(:,:,79))
  call vert_VQ_A(ntry, wf12(:,39), ex1, wf24(:,80), n3(:,192), t3x24(:,:,80))


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
  M2add = M2 / average_factor_ppvvvj_uuuxuxazz_1

  do k = 0, 30-1
    M2(k) = M2add(extcomb_perm_ppvvvj_uuuxuxazz_1(k))
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

    call cont_QA(nsync, wf24(:,1), wf12(:,2), A(:,1), n3(:,193), t3x288(:,:,1), nhel, den(7))
    call cont_QA(nsync, wf16(:,1), wf18(:,2), A(:,2), n3(:,194), t3x288(:,:,2), nhel, den(11))
    call cont_QA(nsync, wf24(:,2), wf12(:,4), A(:,3), n3(:,195), t3x288(:,:,3), nhel, den(16))
    call cont_QA(nsync, wf16(:,1), wf18(:,4), A(:,4), n3(:,196), t3x288(:,:,4), nhel, den(18))
    call cont_QA(nsync, wf12(:,4), wf24(:,3), A(:,5), n3(:,197), t3x288(:,:,5), nhel, den(21))
    call cont_QA(nsync, wf12(:,2), wf24(:,4), A(:,6), n3(:,198), t3x288(:,:,6), nhel, den(22))
    call cont_QA(nsync, wf16(:,2), wf18(:,6), A(:,7), n3(:,199), t3x288(:,:,7), nhel, den(28))
    call cont_QA(nsync, wf24(:,5), wf12(:,6), A(:,8), n3(:,200), t3x288(:,:,8), nhel, den(32))
    call cont_QA(nsync, wf16(:,2), wf18(:,8), A(:,9), n3(:,201), t3x288(:,:,9), nhel, den(35))
    call cont_QA(nsync, wf24(:,6), wf12(:,8), A(:,10), n3(:,202), t3x288(:,:,10), nhel, den(39))
    call cont_QA(nsync, wf12(:,6), wf24(:,7), A(:,11), n3(:,203), t3x288(:,:,11), nhel, den(42))
    call cont_QA(nsync, wf12(:,8), wf24(:,8), A(:,12), n3(:,204), t3x288(:,:,12), nhel, den(43))
    call cont_QA(nsync, wf24(:,2), wf12(:,10), A(:,13), n3(:,205), t3x288(:,:,13), nhel, den(45))
    call cont_QA(nsync, wf24(:,5), wf12(:,12), A(:,14), n3(:,206), t3x288(:,:,14), nhel, den(47))
    call cont_QA(nsync, wf24(:,3), wf12(:,10), A(:,15), n3(:,207), t3x288(:,:,15), nhel, den(48))
    call cont_QA(nsync, wf18(:,6), wf16(:,3), A(:,16), n3(:,208), t3x288(:,:,16), nhel, den(49))
    call cont_QA(nsync, wf24(:,1), wf12(:,14), A(:,17), n3(:,209), t3x288(:,:,17), nhel, den(51))
    call cont_QA(nsync, wf24(:,6), wf12(:,16), A(:,18), n3(:,210), t3x288(:,:,18), nhel, den(53))
    call cont_QA(nsync, wf18(:,2), wf16(:,4), A(:,19), n3(:,211), t3x288(:,:,19), nhel, den(54))
    call cont_QA(nsync, wf24(:,8), wf12(:,16), A(:,20), n3(:,212), t3x288(:,:,20), nhel, den(55))
    call cont_QA(nsync, wf24(:,4), wf12(:,14), A(:,21), n3(:,213), t3x288(:,:,21), nhel, den(56))
    call cont_QA(nsync, wf18(:,8), wf16(:,3), A(:,22), n3(:,214), t3x288(:,:,22), nhel, den(57))
    call cont_QA(nsync, wf18(:,4), wf16(:,4), A(:,23), n3(:,215), t3x288(:,:,23), nhel, den(58))
    call cont_QA(nsync, wf24(:,7), wf12(:,12), A(:,24), n3(:,216), t3x288(:,:,24), nhel, den(59))
    call cont_QA(nsync, wf24(:,9), wf12(:,18), A(:,25), n3(:,217), t3x288(:,:,25), nhel, den(65))
    call cont_QA(nsync, wf18(:,2), wf16(:,5), A(:,26), n3(:,218), t3x288(:,:,26), nhel, den(67))
    call cont_QA(nsync, wf24(:,10), wf12(:,20), A(:,27), n3(:,219), t3x288(:,:,27), nhel, den(71))
    call cont_QA(nsync, wf18(:,4), wf16(:,5), A(:,28), n3(:,220), t3x288(:,:,28), nhel, den(72))
    call cont_QA(nsync, wf12(:,20), wf24(:,11), A(:,29), n3(:,221), t3x288(:,:,29), nhel, den(75))
    call cont_QA(nsync, wf12(:,18), wf24(:,12), A(:,30), n3(:,222), t3x288(:,:,30), nhel, den(76))
    call cont_QA(nsync, wf16(:,6), wf18(:,10), A(:,31), n3(:,223), t3x288(:,:,31), nhel, den(81))
    call cont_QA(nsync, wf12(:,6), wf24(:,13), A(:,32), n3(:,224), t3x288(:,:,32), nhel, den(83))
    call cont_QA(nsync, wf16(:,6), wf18(:,12), A(:,33), n3(:,225), t3x288(:,:,33), nhel, den(86))
    call cont_QA(nsync, wf12(:,8), wf24(:,14), A(:,34), n3(:,226), t3x288(:,:,34), nhel, den(88))
    call cont_QA(nsync, wf12(:,6), wf24(:,15), A(:,35), n3(:,227), t3x288(:,:,35), nhel, den(90))
    call cont_QA(nsync, wf12(:,8), wf24(:,16), A(:,36), n3(:,228), t3x288(:,:,36), nhel, den(91))
    call cont_QA(nsync, wf24(:,10), wf12(:,22), A(:,37), n3(:,229), t3x288(:,:,37), nhel, den(93))
    call cont_QA(nsync, wf12(:,12), wf24(:,13), A(:,38), n3(:,230), t3x288(:,:,38), nhel, den(94))
    call cont_QA(nsync, wf24(:,11), wf12(:,22), A(:,39), n3(:,231), t3x288(:,:,39), nhel, den(95))
    call cont_QA(nsync, wf18(:,10), wf16(:,7), A(:,40), n3(:,232), t3x288(:,:,40), nhel, den(96))
    call cont_QA(nsync, wf24(:,9), wf12(:,24), A(:,41), n3(:,233), t3x288(:,:,41), nhel, den(98))
    call cont_QA(nsync, wf12(:,16), wf24(:,14), A(:,42), n3(:,234), t3x288(:,:,42), nhel, den(99))
    call cont_QA(nsync, wf18(:,2), wf16(:,8), A(:,43), n3(:,235), t3x288(:,:,43), nhel, den(100))
    call cont_QA(nsync, wf12(:,16), wf24(:,16), A(:,44), n3(:,236), t3x288(:,:,44), nhel, den(101))
    call cont_QA(nsync, wf24(:,12), wf12(:,24), A(:,45), n3(:,237), t3x288(:,:,45), nhel, den(102))
    call cont_QA(nsync, wf18(:,12), wf16(:,7), A(:,46), n3(:,238), t3x288(:,:,46), nhel, den(103))
    call cont_QA(nsync, wf18(:,4), wf16(:,8), A(:,47), n3(:,239), t3x288(:,:,47), nhel, den(104))
    call cont_QA(nsync, wf12(:,12), wf24(:,15), A(:,48), n3(:,240), t3x288(:,:,48), nhel, den(105))
    call cont_QA(nsync, wf12(:,2), wf24(:,17), A(:,49), n3(:,241), t3x288(:,:,49), nhel, den(109))
    call cont_QA(nsync, wf16(:,9), wf18(:,14), A(:,50), n3(:,242), t3x288(:,:,50), nhel, den(113))
    call cont_QA(nsync, wf12(:,4), wf24(:,18), A(:,51), n3(:,243), t3x288(:,:,51), nhel, den(116))
    call cont_QA(nsync, wf16(:,9), wf18(:,16), A(:,52), n3(:,244), t3x288(:,:,52), nhel, den(118))
    call cont_QA(nsync, wf12(:,4), wf24(:,19), A(:,53), n3(:,245), t3x288(:,:,53), nhel, den(120))
    call cont_QA(nsync, wf12(:,2), wf24(:,20), A(:,54), n3(:,246), t3x288(:,:,54), nhel, den(121))
    call cont_QA(nsync, wf18(:,6), wf16(:,10), A(:,55), n3(:,247), t3x288(:,:,55), nhel, den(124))
    call cont_QA(nsync, wf24(:,21), wf12(:,26), A(:,56), n3(:,248), t3x288(:,:,56), nhel, den(128))
    call cont_QA(nsync, wf18(:,8), wf16(:,10), A(:,57), n3(:,249), t3x288(:,:,57), nhel, den(129))
    call cont_QA(nsync, wf24(:,22), wf12(:,28), A(:,58), n3(:,250), t3x288(:,:,58), nhel, den(133))
    call cont_QA(nsync, wf12(:,26), wf24(:,23), A(:,59), n3(:,251), t3x288(:,:,59), nhel, den(136))
    call cont_QA(nsync, wf12(:,28), wf24(:,24), A(:,60), n3(:,252), t3x288(:,:,60), nhel, den(137))
    call cont_QA(nsync, wf12(:,10), wf24(:,18), A(:,61), n3(:,253), t3x288(:,:,61), nhel, den(138))
    call cont_QA(nsync, wf24(:,21), wf12(:,30), A(:,62), n3(:,254), t3x288(:,:,62), nhel, den(140))
    call cont_QA(nsync, wf12(:,10), wf24(:,19), A(:,63), n3(:,255), t3x288(:,:,63), nhel, den(141))
    call cont_QA(nsync, wf18(:,6), wf16(:,11), A(:,64), n3(:,256), t3x288(:,:,64), nhel, den(142))
    call cont_QA(nsync, wf12(:,14), wf24(:,17), A(:,65), n3(:,257), t3x288(:,:,65), nhel, den(143))
    call cont_QA(nsync, wf24(:,22), wf12(:,32), A(:,66), n3(:,258), t3x288(:,:,66), nhel, den(145))
    call cont_QA(nsync, wf18(:,14), wf16(:,12), A(:,67), n3(:,259), t3x288(:,:,67), nhel, den(146))
    call cont_QA(nsync, wf24(:,24), wf12(:,32), A(:,68), n3(:,260), t3x288(:,:,68), nhel, den(147))
    call cont_QA(nsync, wf12(:,14), wf24(:,20), A(:,69), n3(:,261), t3x288(:,:,69), nhel, den(148))
    call cont_QA(nsync, wf18(:,8), wf16(:,11), A(:,70), n3(:,262), t3x288(:,:,70), nhel, den(149))
    call cont_QA(nsync, wf18(:,16), wf16(:,12), A(:,71), n3(:,263), t3x288(:,:,71), nhel, den(150))
    call cont_QA(nsync, wf24(:,23), wf12(:,30), A(:,72), n3(:,264), t3x288(:,:,72), nhel, den(151))
    call cont_QA(nsync, wf12(:,18), wf24(:,25), A(:,73), n3(:,265), t3x288(:,:,73), nhel, den(154))
    call cont_QA(nsync, wf18(:,14), wf16(:,13), A(:,74), n3(:,266), t3x288(:,:,74), nhel, den(156))
    call cont_QA(nsync, wf12(:,20), wf24(:,26), A(:,75), n3(:,267), t3x288(:,:,75), nhel, den(158))
    call cont_QA(nsync, wf18(:,16), wf16(:,13), A(:,76), n3(:,268), t3x288(:,:,76), nhel, den(159))
    call cont_QA(nsync, wf12(:,20), wf24(:,27), A(:,77), n3(:,269), t3x288(:,:,77), nhel, den(161))
    call cont_QA(nsync, wf12(:,18), wf24(:,28), A(:,78), n3(:,270), t3x288(:,:,78), nhel, den(162))
    call cont_QA(nsync, wf18(:,10), wf16(:,14), A(:,79), n3(:,271), t3x288(:,:,79), nhel, den(164))
    call cont_QA(nsync, wf12(:,26), wf24(:,29), A(:,80), n3(:,272), t3x288(:,:,80), nhel, den(166))
    call cont_QA(nsync, wf18(:,12), wf16(:,14), A(:,81), n3(:,273), t3x288(:,:,81), nhel, den(167))
    call cont_QA(nsync, wf12(:,28), wf24(:,30), A(:,82), n3(:,274), t3x288(:,:,82), nhel, den(169))
    call cont_QA(nsync, wf12(:,26), wf24(:,31), A(:,83), n3(:,275), t3x288(:,:,83), nhel, den(171))
    call cont_QA(nsync, wf12(:,28), wf24(:,32), A(:,84), n3(:,276), t3x288(:,:,84), nhel, den(172))
    call cont_QA(nsync, wf12(:,22), wf24(:,26), A(:,85), n3(:,277), t3x288(:,:,85), nhel, den(173))
    call cont_QA(nsync, wf12(:,30), wf24(:,29), A(:,86), n3(:,278), t3x288(:,:,86), nhel, den(174))
    call cont_QA(nsync, wf12(:,22), wf24(:,27), A(:,87), n3(:,279), t3x288(:,:,87), nhel, den(175))
    call cont_QA(nsync, wf18(:,10), wf16(:,15), A(:,88), n3(:,280), t3x288(:,:,88), nhel, den(176))
    call cont_QA(nsync, wf12(:,24), wf24(:,25), A(:,89), n3(:,281), t3x288(:,:,89), nhel, den(177))
    call cont_QA(nsync, wf12(:,32), wf24(:,30), A(:,90), n3(:,282), t3x288(:,:,90), nhel, den(178))
    call cont_QA(nsync, wf18(:,14), wf16(:,16), A(:,91), n3(:,283), t3x288(:,:,91), nhel, den(179))
    call cont_QA(nsync, wf12(:,32), wf24(:,32), A(:,92), n3(:,284), t3x288(:,:,92), nhel, den(180))
    call cont_QA(nsync, wf12(:,24), wf24(:,28), A(:,93), n3(:,285), t3x288(:,:,93), nhel, den(181))
    call cont_QA(nsync, wf18(:,12), wf16(:,15), A(:,94), n3(:,286), t3x288(:,:,94), nhel, den(182))
    call cont_QA(nsync, wf18(:,16), wf16(:,16), A(:,95), n3(:,287), t3x288(:,:,95), nhel, den(183))
    call cont_QA(nsync, wf12(:,30), wf24(:,31), A(:,96), n3(:,288), t3x288(:,:,96), nhel, den(184))
    call cont_VV(nsync, wf8(:,17), wf36(:,1), A(:,97), n3(:,289), t3x288(:,:,97), nhel, den(188))
    call cont_VV(nsync, wf12(:,33), wf24(:,33), A(:,98), n3(:,290), t3x288(:,:,98), nhel, den(192))
    call cont_VV(nsync, wf8(:,18), wf36(:,2), A(:,99), n3(:,291), t3x288(:,:,99), nhel, den(196))
    call cont_VV(nsync, wf12(:,34), wf24(:,34), A(:,100), n3(:,292), t3x288(:,:,100), nhel, den(200))
    call cont_QA(nsync, wf18(:,6), wf16(:,17), A(:,101), n3(:,293), t3x288(:,:,101), nhel, den(201))
    call cont_QA(nsync, wf18(:,6), wf16(:,18), A(:,102), n3(:,294), t3x288(:,:,102), nhel, den(202))
    call cont_QA(nsync, wf12(:,18), wf24(:,35), A(:,103), n3(:,295), t3x288(:,:,103), nhel, den(203))
    call cont_QA(nsync, wf12(:,18), wf24(:,36), A(:,104), n3(:,296), t3x288(:,:,104), nhel, den(204))
    call cont_VV(nsync, wf8(:,17), wf36(:,3), A(:,105), n3(:,297), t3x288(:,:,105), nhel, den(206))
    call cont_VV(nsync, wf12(:,35), wf24(:,37), A(:,106), n3(:,298), t3x288(:,:,106), nhel, den(210))
    call cont_VV(nsync, wf24(:,34), wf12(:,36), A(:,107), n3(:,299), t3x288(:,:,107), nhel, den(212))
    call cont_VV(nsync, wf24(:,37), wf12(:,37), A(:,108), n3(:,300), t3x288(:,:,108), nhel, den(214))
    call cont_QA(nsync, wf18(:,14), wf16(:,19), A(:,109), n3(:,301), t3x288(:,:,109), nhel, den(215))
    call cont_QA(nsync, wf12(:,18), wf24(:,38), A(:,110), n3(:,302), t3x288(:,:,110), nhel, den(216))
    call cont_VV(nsync, wf8(:,18), wf36(:,4), A(:,111), n3(:,303), t3x288(:,:,111), nhel, den(218))
    call cont_VV(nsync, wf12(:,38), wf24(:,39), A(:,112), n3(:,304), t3x288(:,:,112), nhel, den(222))
    call cont_VV(nsync, wf24(:,39), wf12(:,39), A(:,113), n3(:,305), t3x288(:,:,113), nhel, den(224))
    call cont_VV(nsync, wf24(:,33), wf12(:,40), A(:,114), n3(:,306), t3x288(:,:,114), nhel, den(226))
    call cont_QA(nsync, wf18(:,2), wf16(:,20), A(:,115), n3(:,307), t3x288(:,:,115), nhel, den(227))
    call cont_QA(nsync, wf12(:,18), wf24(:,40), A(:,116), n3(:,308), t3x288(:,:,116), nhel, den(228))
    call cont_QA(nsync, wf18(:,8), wf16(:,17), A(:,117), n3(:,309), t3x288(:,:,117), nhel, den(229))
    call cont_QA(nsync, wf18(:,8), wf16(:,18), A(:,118), n3(:,310), t3x288(:,:,118), nhel, den(230))
    call cont_QA(nsync, wf12(:,20), wf24(:,41), A(:,119), n3(:,311), t3x288(:,:,119), nhel, den(231))
    call cont_QA(nsync, wf12(:,20), wf24(:,42), A(:,120), n3(:,312), t3x288(:,:,120), nhel, den(232))
    call cont_QA(nsync, wf18(:,16), wf16(:,19), A(:,121), n3(:,313), t3x288(:,:,121), nhel, den(233))
    call cont_QA(nsync, wf12(:,20), wf24(:,43), A(:,122), n3(:,314), t3x288(:,:,122), nhel, den(234))
    call cont_QA(nsync, wf18(:,4), wf16(:,20), A(:,123), n3(:,315), t3x288(:,:,123), nhel, den(235))
    call cont_QA(nsync, wf12(:,20), wf24(:,44), A(:,124), n3(:,316), t3x288(:,:,124), nhel, den(236))
    call cont_VV(nsync, wf12(:,41), wf24(:,45), A(:,125), n3(:,317), t3x288(:,:,125), nhel, den(240))
    call cont_VV(nsync, wf8(:,19), wf36(:,5), A(:,126), n3(:,318), t3x288(:,:,126), nhel, den(244))
    call cont_VV(nsync, wf12(:,42), wf24(:,46), A(:,127), n3(:,319), t3x288(:,:,127), nhel, den(248))
    call cont_VV(nsync, wf8(:,20), wf36(:,6), A(:,128), n3(:,320), t3x288(:,:,128), nhel, den(252))
    call cont_QA(nsync, wf12(:,2), wf24(:,47), A(:,129), n3(:,321), t3x288(:,:,129), nhel, den(253))
    call cont_QA(nsync, wf12(:,2), wf24(:,48), A(:,130), n3(:,322), t3x288(:,:,130), nhel, den(254))
    call cont_QA(nsync, wf18(:,10), wf16(:,21), A(:,131), n3(:,323), t3x288(:,:,131), nhel, den(255))
    call cont_QA(nsync, wf18(:,10), wf16(:,22), A(:,132), n3(:,324), t3x288(:,:,132), nhel, den(256))
    call cont_VV(nsync, wf12(:,43), wf24(:,49), A(:,133), n3(:,325), t3x288(:,:,133), nhel, den(260))
    call cont_VV(nsync, wf8(:,19), wf36(:,7), A(:,134), n3(:,326), t3x288(:,:,134), nhel, den(262))
    call cont_VV(nsync, wf24(:,46), wf12(:,44), A(:,135), n3(:,327), t3x288(:,:,135), nhel, den(264))
    call cont_VV(nsync, wf24(:,49), wf12(:,45), A(:,136), n3(:,328), t3x288(:,:,136), nhel, den(266))
    call cont_QA(nsync, wf12(:,2), wf24(:,50), A(:,137), n3(:,329), t3x288(:,:,137), nhel, den(267))
    call cont_QA(nsync, wf18(:,14), wf16(:,23), A(:,138), n3(:,330), t3x288(:,:,138), nhel, den(268))
    call cont_VV(nsync, wf12(:,46), wf24(:,51), A(:,139), n3(:,331), t3x288(:,:,139), nhel, den(272))
    call cont_VV(nsync, wf8(:,20), wf36(:,8), A(:,140), n3(:,332), t3x288(:,:,140), nhel, den(274))
    call cont_VV(nsync, wf24(:,51), wf12(:,47), A(:,141), n3(:,333), t3x288(:,:,141), nhel, den(276))
    call cont_VV(nsync, wf24(:,45), wf12(:,48), A(:,142), n3(:,334), t3x288(:,:,142), nhel, den(278))
    call cont_QA(nsync, wf12(:,2), wf24(:,52), A(:,143), n3(:,335), t3x288(:,:,143), nhel, den(279))
    call cont_QA(nsync, wf18(:,2), wf16(:,24), A(:,144), n3(:,336), t3x288(:,:,144), nhel, den(280))
    call cont_QA(nsync, wf12(:,4), wf24(:,53), A(:,145), n3(:,337), t3x288(:,:,145), nhel, den(281))
    call cont_QA(nsync, wf12(:,4), wf24(:,54), A(:,146), n3(:,338), t3x288(:,:,146), nhel, den(282))
    call cont_QA(nsync, wf18(:,12), wf16(:,21), A(:,147), n3(:,339), t3x288(:,:,147), nhel, den(283))
    call cont_QA(nsync, wf18(:,12), wf16(:,22), A(:,148), n3(:,340), t3x288(:,:,148), nhel, den(284))
    call cont_QA(nsync, wf12(:,4), wf24(:,55), A(:,149), n3(:,341), t3x288(:,:,149), nhel, den(285))
    call cont_QA(nsync, wf18(:,16), wf16(:,23), A(:,150), n3(:,342), t3x288(:,:,150), nhel, den(286))
    call cont_QA(nsync, wf12(:,4), wf24(:,56), A(:,151), n3(:,343), t3x288(:,:,151), nhel, den(287))
    call cont_QA(nsync, wf18(:,4), wf16(:,24), A(:,152), n3(:,344), t3x288(:,:,152), nhel, den(288))
    call cont_VV(nsync, wf12(:,41), wf24(:,57), A(:,153), n3(:,345), t3x288(:,:,153), nhel, den(290))
    call cont_VV(nsync, wf12(:,35), wf24(:,58), A(:,154), n3(:,346), t3x288(:,:,154), nhel, den(292))
    call cont_VV(nsync, wf36(:,6), wf8(:,21), A(:,155), n3(:,347), t3x288(:,:,155), nhel, den(294))
    call cont_VV(nsync, wf12(:,37), wf24(:,58), A(:,156), n3(:,348), t3x288(:,:,156), nhel, den(295))
    call cont_QA(nsync, wf12(:,26), wf24(:,59), A(:,157), n3(:,349), t3x288(:,:,157), nhel, den(296))
    call cont_QA(nsync, wf18(:,10), wf16(:,25), A(:,158), n3(:,350), t3x288(:,:,158), nhel, den(297))
    call cont_VV(nsync, wf12(:,43), wf24(:,60), A(:,159), n3(:,351), t3x288(:,:,159), nhel, den(299))
    call cont_VV(nsync, wf12(:,33), wf24(:,61), A(:,160), n3(:,352), t3x288(:,:,160), nhel, den(301))
    call cont_VV(nsync, wf36(:,2), wf8(:,22), A(:,161), n3(:,353), t3x288(:,:,161), nhel, den(303))
    call cont_VV(nsync, wf12(:,45), wf24(:,60), A(:,162), n3(:,354), t3x288(:,:,162), nhel, den(304))
    call cont_QA(nsync, wf18(:,6), wf16(:,26), A(:,163), n3(:,355), t3x288(:,:,163), nhel, den(305))
    call cont_QA(nsync, wf12(:,26), wf24(:,62), A(:,164), n3(:,356), t3x288(:,:,164), nhel, den(306))
    call cont_VV(nsync, wf36(:,8), wf8(:,21), A(:,165), n3(:,357), t3x288(:,:,165), nhel, den(307))
    call cont_VV(nsync, wf12(:,40), wf24(:,61), A(:,166), n3(:,358), t3x288(:,:,166), nhel, den(308))
    call cont_VV(nsync, wf36(:,4), wf8(:,22), A(:,167), n3(:,359), t3x288(:,:,167), nhel, den(309))
    call cont_VV(nsync, wf12(:,48), wf24(:,57), A(:,168), n3(:,360), t3x288(:,:,168), nhel, den(310))
    call cont_QA(nsync, wf18(:,2), wf16(:,27), A(:,169), n3(:,361), t3x288(:,:,169), nhel, den(311))
    call cont_QA(nsync, wf12(:,26), wf24(:,63), A(:,170), n3(:,362), t3x288(:,:,170), nhel, den(312))
    call cont_QA(nsync, wf18(:,2), wf16(:,28), A(:,171), n3(:,363), t3x288(:,:,171), nhel, den(313))
    call cont_QA(nsync, wf12(:,26), wf24(:,64), A(:,172), n3(:,364), t3x288(:,:,172), nhel, den(314))
    call cont_QA(nsync, wf12(:,28), wf24(:,65), A(:,173), n3(:,365), t3x288(:,:,173), nhel, den(315))
    call cont_QA(nsync, wf18(:,12), wf16(:,25), A(:,174), n3(:,366), t3x288(:,:,174), nhel, den(316))
    call cont_QA(nsync, wf18(:,8), wf16(:,26), A(:,175), n3(:,367), t3x288(:,:,175), nhel, den(317))
    call cont_QA(nsync, wf12(:,28), wf24(:,66), A(:,176), n3(:,368), t3x288(:,:,176), nhel, den(318))
    call cont_QA(nsync, wf18(:,4), wf16(:,27), A(:,177), n3(:,369), t3x288(:,:,177), nhel, den(319))
    call cont_QA(nsync, wf12(:,28), wf24(:,67), A(:,178), n3(:,370), t3x288(:,:,178), nhel, den(320))
    call cont_QA(nsync, wf18(:,4), wf16(:,28), A(:,179), n3(:,371), t3x288(:,:,179), nhel, den(321))
    call cont_QA(nsync, wf12(:,28), wf24(:,68), A(:,180), n3(:,372), t3x288(:,:,180), nhel, den(322))
    call cont_VV(nsync, wf12(:,42), wf24(:,69), A(:,181), n3(:,373), t3x288(:,:,181), nhel, den(324))
    call cont_VV(nsync, wf12(:,38), wf24(:,70), A(:,182), n3(:,374), t3x288(:,:,182), nhel, den(326))
    call cont_VV(nsync, wf12(:,39), wf24(:,70), A(:,183), n3(:,375), t3x288(:,:,183), nhel, den(327))
    call cont_VV(nsync, wf36(:,5), wf8(:,23), A(:,184), n3(:,376), t3x288(:,:,184), nhel, den(329))
    call cont_QA(nsync, wf12(:,6), wf24(:,71), A(:,185), n3(:,377), t3x288(:,:,185), nhel, den(330))
    call cont_QA(nsync, wf18(:,10), wf16(:,29), A(:,186), n3(:,378), t3x288(:,:,186), nhel, den(331))
    call cont_VV(nsync, wf12(:,46), wf24(:,72), A(:,187), n3(:,379), t3x288(:,:,187), nhel, den(333))
    call cont_VV(nsync, wf12(:,34), wf24(:,73), A(:,188), n3(:,380), t3x288(:,:,188), nhel, den(335))
    call cont_VV(nsync, wf12(:,47), wf24(:,72), A(:,189), n3(:,381), t3x288(:,:,189), nhel, den(336))
    call cont_VV(nsync, wf36(:,1), wf8(:,24), A(:,190), n3(:,382), t3x288(:,:,190), nhel, den(338))
    call cont_QA(nsync, wf18(:,6), wf16(:,30), A(:,191), n3(:,383), t3x288(:,:,191), nhel, den(339))
    call cont_QA(nsync, wf12(:,6), wf24(:,74), A(:,192), n3(:,384), t3x288(:,:,192), nhel, den(340))
    call cont_VV(nsync, wf12(:,36), wf24(:,73), A(:,193), n3(:,385), t3x288(:,:,193), nhel, den(341))
    call cont_VV(nsync, wf36(:,7), wf8(:,23), A(:,194), n3(:,386), t3x288(:,:,194), nhel, den(342))
    call cont_VV(nsync, wf12(:,44), wf24(:,69), A(:,195), n3(:,387), t3x288(:,:,195), nhel, den(343))
    call cont_VV(nsync, wf36(:,3), wf8(:,24), A(:,196), n3(:,388), t3x288(:,:,196), nhel, den(344))
    call cont_QA(nsync, wf12(:,6), wf24(:,75), A(:,197), n3(:,389), t3x288(:,:,197), nhel, den(345))
    call cont_QA(nsync, wf18(:,14), wf16(:,31), A(:,198), n3(:,390), t3x288(:,:,198), nhel, den(346))
    call cont_QA(nsync, wf12(:,6), wf24(:,76), A(:,199), n3(:,391), t3x288(:,:,199), nhel, den(347))
    call cont_QA(nsync, wf18(:,14), wf16(:,32), A(:,200), n3(:,392), t3x288(:,:,200), nhel, den(348))
    call cont_QA(nsync, wf12(:,8), wf24(:,77), A(:,201), n3(:,393), t3x288(:,:,201), nhel, den(349))
    call cont_QA(nsync, wf18(:,12), wf16(:,29), A(:,202), n3(:,394), t3x288(:,:,202), nhel, den(350))
    call cont_QA(nsync, wf18(:,8), wf16(:,30), A(:,203), n3(:,395), t3x288(:,:,203), nhel, den(351))
    call cont_QA(nsync, wf12(:,8), wf24(:,78), A(:,204), n3(:,396), t3x288(:,:,204), nhel, den(352))
    call cont_QA(nsync, wf12(:,8), wf24(:,79), A(:,205), n3(:,397), t3x288(:,:,205), nhel, den(353))
    call cont_QA(nsync, wf18(:,16), wf16(:,31), A(:,206), n3(:,398), t3x288(:,:,206), nhel, den(354))
    call cont_QA(nsync, wf12(:,8), wf24(:,80), A(:,207), n3(:,399), t3x288(:,:,207), nhel, den(355))
    call cont_QA(nsync, wf18(:,16), wf16(:,32), A(:,208), n3(:,400), t3x288(:,:,208), nhel, den(356))
    call cont_QA(nsync, wf12(:,14), wf24(:,47), A(:,209), n3(:,401), t3x288(:,:,209), nhel, den(357))
    call cont_QA(nsync, wf12(:,14), wf24(:,48), A(:,210), n3(:,402), t3x288(:,:,210), nhel, den(358))
    call cont_QA(nsync, wf12(:,22), wf24(:,41), A(:,211), n3(:,403), t3x288(:,:,211), nhel, den(359))
    call cont_QA(nsync, wf12(:,22), wf24(:,42), A(:,212), n3(:,404), t3x288(:,:,212), nhel, den(360))
    call cont_QA(nsync, wf12(:,30), wf24(:,59), A(:,213), n3(:,405), t3x288(:,:,213), nhel, den(361))
    call cont_QA(nsync, wf12(:,22), wf24(:,43), A(:,214), n3(:,406), t3x288(:,:,214), nhel, den(362))
    call cont_QA(nsync, wf12(:,12), wf24(:,71), A(:,215), n3(:,407), t3x288(:,:,215), nhel, den(363))
    call cont_QA(nsync, wf12(:,22), wf24(:,44), A(:,216), n3(:,408), t3x288(:,:,216), nhel, den(364))
    call cont_QA(nsync, wf12(:,10), wf24(:,53), A(:,217), n3(:,409), t3x288(:,:,217), nhel, den(365))
    call cont_QA(nsync, wf12(:,10), wf24(:,54), A(:,218), n3(:,410), t3x288(:,:,218), nhel, den(366))
    call cont_QA(nsync, wf12(:,24), wf24(:,35), A(:,219), n3(:,411), t3x288(:,:,219), nhel, den(367))
    call cont_QA(nsync, wf12(:,24), wf24(:,36), A(:,220), n3(:,412), t3x288(:,:,220), nhel, den(368))
    call cont_QA(nsync, wf12(:,10), wf24(:,55), A(:,221), n3(:,413), t3x288(:,:,221), nhel, den(369))
    call cont_QA(nsync, wf12(:,30), wf24(:,62), A(:,222), n3(:,414), t3x288(:,:,222), nhel, den(370))
    call cont_QA(nsync, wf12(:,10), wf24(:,56), A(:,223), n3(:,415), t3x288(:,:,223), nhel, den(371))
    call cont_QA(nsync, wf12(:,12), wf24(:,74), A(:,224), n3(:,416), t3x288(:,:,224), nhel, den(372))
    call cont_QA(nsync, wf12(:,32), wf24(:,65), A(:,225), n3(:,417), t3x288(:,:,225), nhel, den(373))
    call cont_QA(nsync, wf12(:,24), wf24(:,38), A(:,226), n3(:,418), t3x288(:,:,226), nhel, den(374))
    call cont_QA(nsync, wf12(:,14), wf24(:,50), A(:,227), n3(:,419), t3x288(:,:,227), nhel, den(375))
    call cont_QA(nsync, wf12(:,32), wf24(:,66), A(:,228), n3(:,420), t3x288(:,:,228), nhel, den(376))
    call cont_QA(nsync, wf12(:,12), wf24(:,75), A(:,229), n3(:,421), t3x288(:,:,229), nhel, den(377))
    call cont_QA(nsync, wf12(:,32), wf24(:,67), A(:,230), n3(:,422), t3x288(:,:,230), nhel, den(378))
    call cont_QA(nsync, wf12(:,12), wf24(:,76), A(:,231), n3(:,423), t3x288(:,:,231), nhel, den(379))
    call cont_QA(nsync, wf12(:,32), wf24(:,68), A(:,232), n3(:,424), t3x288(:,:,232), nhel, den(380))
    call cont_QA(nsync, wf12(:,16), wf24(:,77), A(:,233), n3(:,425), t3x288(:,:,233), nhel, den(381))
    call cont_QA(nsync, wf12(:,24), wf24(:,40), A(:,234), n3(:,426), t3x288(:,:,234), nhel, den(382))
    call cont_QA(nsync, wf12(:,14), wf24(:,52), A(:,235), n3(:,427), t3x288(:,:,235), nhel, den(383))
    call cont_QA(nsync, wf12(:,16), wf24(:,78), A(:,236), n3(:,428), t3x288(:,:,236), nhel, den(384))
    call cont_QA(nsync, wf12(:,16), wf24(:,79), A(:,237), n3(:,429), t3x288(:,:,237), nhel, den(385))
    call cont_QA(nsync, wf12(:,30), wf24(:,63), A(:,238), n3(:,430), t3x288(:,:,238), nhel, den(386))
    call cont_QA(nsync, wf12(:,16), wf24(:,80), A(:,239), n3(:,431), t3x288(:,:,239), nhel, den(387))
    call cont_QA(nsync, wf12(:,30), wf24(:,64), A(:,240), n3(:,432), t3x288(:,:,240), nhel, den(388))

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
  complex(REALKIND), intent(out) :: M1(2) ! M1helarray(2,288)
  integer :: empty(0)

  M1(1) = ((-A(j,25)%j-A(j,26)%j-A(j,27)%j-A(j,28)%j-A(j,29)%j-A(j,30)%j-A(j,31)%j-A(j,32)%j-A(j,33)%j-A(j,34)%j-A(j,35)%j &
       -A(j,36)%j-A(j,37)%j-A(j,38)%j-A(j,39)%j-A(j,40)%j-A(j,41)%j-A(j,42)%j-A(j,43)%j-A(j,44)%j-A(j,45)%j-A(j,46)%j-A(j,47)%j &
       -A(j,48)%j-A(j,49)%j-A(j,50)%j-A(j,51)%j-A(j,52)%j-A(j,53)%j-A(j,54)%j-A(j,55)%j-A(j,56)%j-A(j,57)%j-A(j,58)%j-A(j,59)%j &
       -A(j,60)%j-A(j,61)%j-A(j,62)%j-A(j,63)%j-A(j,64)%j-A(j,65)%j-A(j,66)%j-A(j,67)%j-A(j,68)%j-A(j,69)%j-A(j,70)%j-A(j,71)%j &
       -A(j,72)%j-A(j,97)%j-A(j,100)%j-A(j,102)%j-A(j,103)%j-A(j,105)%j-A(j,107)%j-A(j,109)%j-A(j,110)%j-A(j,112)%j-A(j,113)%j &
       -A(j,118)%j-A(j,119)%j-A(j,121)%j-A(j,122)%j-A(j,125)%j-A(j,128)%j-A(j,130)%j-A(j,131)%j-A(j,133)%j-A(j,136)%j-A(j,140)%j &
       -A(j,142)%j-A(j,143)%j-A(j,144)%j-A(j,146)%j-A(j,147)%j-A(j,151)%j-A(j,152)%j-A(j,153)%j-A(j,155)%j-A(j,157)%j-A(j,158)%j &
       -A(j,159)%j-A(j,162)%j-A(j,165)%j-A(j,168)%j-A(j,170)%j-A(j,171)%j-A(j,173)%j-A(j,174)%j-A(j,178)%j-A(j,179)%j-A(j,182)%j &
       -A(j,183)%j-A(j,188)%j-A(j,190)%j-A(j,191)%j-A(j,192)%j-A(j,193)%j-A(j,196)%j-A(j,198)%j-A(j,199)%j-A(j,203)%j-A(j,204)%j &
       -A(j,206)%j-A(j,207)%j-A(j,210)%j-A(j,211)%j-A(j,213)%j-A(j,214)%j-A(j,218)%j-A(j,219)%j-A(j,223)%j-A(j,224)%j-A(j,225)%j &
       -A(j,226)%j-A(j,230)%j-A(j,231)%j-A(j,235)%j-A(j,236)%j-A(j,238)%j-A(j,239)%j)*f(1))/6._/**/REALKIND+((-A(j,1)%j-A(j,2)%j &
       -A(j,3)%j-A(j,4)%j-A(j,5)%j-A(j,6)%j-A(j,7)%j-A(j,8)%j-A(j,9)%j-A(j,10)%j-A(j,11)%j-A(j,12)%j-A(j,13)%j-A(j,14)%j-A(j,15)%j &
       -A(j,16)%j-A(j,17)%j-A(j,18)%j-A(j,19)%j-A(j,20)%j-A(j,21)%j-A(j,22)%j-A(j,23)%j-A(j,24)%j-A(j,73)%j-A(j,74)%j-A(j,75)%j &
       -A(j,76)%j-A(j,77)%j-A(j,78)%j-A(j,79)%j-A(j,80)%j-A(j,81)%j-A(j,82)%j-A(j,83)%j-A(j,84)%j-A(j,85)%j-A(j,86)%j-A(j,87)%j &
       -A(j,88)%j-A(j,89)%j-A(j,90)%j-A(j,91)%j-A(j,92)%j-A(j,93)%j-A(j,94)%j-A(j,95)%j-A(j,96)%j-A(j,98)%j-A(j,99)%j-A(j,101)%j &
       -A(j,104)%j-A(j,106)%j-A(j,108)%j-A(j,111)%j-A(j,114)%j-A(j,115)%j-A(j,116)%j-A(j,117)%j-A(j,120)%j-A(j,123)%j-A(j,124)%j &
       -A(j,126)%j-A(j,127)%j-A(j,129)%j-A(j,132)%j-A(j,134)%j-A(j,135)%j-A(j,137)%j-A(j,138)%j-A(j,139)%j-A(j,141)%j-A(j,145)%j &
       -A(j,148)%j-A(j,149)%j-A(j,150)%j-A(j,154)%j-A(j,156)%j-A(j,160)%j-A(j,161)%j-A(j,163)%j-A(j,164)%j-A(j,166)%j-A(j,167)%j &
       -A(j,169)%j-A(j,172)%j-A(j,175)%j-A(j,176)%j-A(j,177)%j-A(j,180)%j-A(j,181)%j-A(j,184)%j-A(j,185)%j-A(j,186)%j-A(j,187)%j &
       -A(j,189)%j-A(j,194)%j-A(j,195)%j-A(j,197)%j-A(j,200)%j-A(j,201)%j-A(j,202)%j-A(j,205)%j-A(j,208)%j-A(j,209)%j-A(j,212)%j &
       -A(j,215)%j-A(j,216)%j-A(j,217)%j-A(j,220)%j-A(j,221)%j-A(j,222)%j-A(j,227)%j-A(j,228)%j-A(j,229)%j-A(j,232)%j-A(j,233)%j &
       -A(j,234)%j-A(j,237)%j-A(j,240)%j)*f(1))/2._/**/REALKIND
  M1(2) = ((A(j,25)%j+A(j,26)%j+A(j,27)%j+A(j,28)%j+A(j,29)%j+A(j,30)%j+A(j,31)%j+A(j,32)%j+A(j,33)%j+A(j,34)%j+A(j,35)%j &
       +A(j,36)%j+A(j,37)%j+A(j,38)%j+A(j,39)%j+A(j,40)%j+A(j,41)%j+A(j,42)%j+A(j,43)%j+A(j,44)%j+A(j,45)%j+A(j,46)%j+A(j,47)%j &
       +A(j,48)%j+A(j,49)%j+A(j,50)%j+A(j,51)%j+A(j,52)%j+A(j,53)%j+A(j,54)%j+A(j,55)%j+A(j,56)%j+A(j,57)%j+A(j,58)%j+A(j,59)%j &
       +A(j,60)%j+A(j,61)%j+A(j,62)%j+A(j,63)%j+A(j,64)%j+A(j,65)%j+A(j,66)%j+A(j,67)%j+A(j,68)%j+A(j,69)%j+A(j,70)%j+A(j,71)%j &
       +A(j,72)%j+A(j,97)%j+A(j,100)%j+A(j,102)%j+A(j,103)%j+A(j,105)%j+A(j,107)%j+A(j,109)%j+A(j,110)%j+A(j,112)%j+A(j,113)%j &
       +A(j,118)%j+A(j,119)%j+A(j,121)%j+A(j,122)%j+A(j,125)%j+A(j,128)%j+A(j,130)%j+A(j,131)%j+A(j,133)%j+A(j,136)%j+A(j,140)%j &
       +A(j,142)%j+A(j,143)%j+A(j,144)%j+A(j,146)%j+A(j,147)%j+A(j,151)%j+A(j,152)%j+A(j,153)%j+A(j,155)%j+A(j,157)%j+A(j,158)%j &
       +A(j,159)%j+A(j,162)%j+A(j,165)%j+A(j,168)%j+A(j,170)%j+A(j,171)%j+A(j,173)%j+A(j,174)%j+A(j,178)%j+A(j,179)%j+A(j,182)%j &
       +A(j,183)%j+A(j,188)%j+A(j,190)%j+A(j,191)%j+A(j,192)%j+A(j,193)%j+A(j,196)%j+A(j,198)%j+A(j,199)%j+A(j,203)%j+A(j,204)%j &
       +A(j,206)%j+A(j,207)%j+A(j,210)%j+A(j,211)%j+A(j,213)%j+A(j,214)%j+A(j,218)%j+A(j,219)%j+A(j,223)%j+A(j,224)%j+A(j,225)%j &
       +A(j,226)%j+A(j,230)%j+A(j,231)%j+A(j,235)%j+A(j,236)%j+A(j,238)%j+A(j,239)%j)*f(1))/2._/**/REALKIND+((A(j,1)%j+A(j,2)%j &
       +A(j,3)%j+A(j,4)%j+A(j,5)%j+A(j,6)%j+A(j,7)%j+A(j,8)%j+A(j,9)%j+A(j,10)%j+A(j,11)%j+A(j,12)%j+A(j,13)%j+A(j,14)%j+A(j,15)%j &
       +A(j,16)%j+A(j,17)%j+A(j,18)%j+A(j,19)%j+A(j,20)%j+A(j,21)%j+A(j,22)%j+A(j,23)%j+A(j,24)%j+A(j,73)%j+A(j,74)%j+A(j,75)%j &
       +A(j,76)%j+A(j,77)%j+A(j,78)%j+A(j,79)%j+A(j,80)%j+A(j,81)%j+A(j,82)%j+A(j,83)%j+A(j,84)%j+A(j,85)%j+A(j,86)%j+A(j,87)%j &
       +A(j,88)%j+A(j,89)%j+A(j,90)%j+A(j,91)%j+A(j,92)%j+A(j,93)%j+A(j,94)%j+A(j,95)%j+A(j,96)%j+A(j,98)%j+A(j,99)%j+A(j,101)%j &
       +A(j,104)%j+A(j,106)%j+A(j,108)%j+A(j,111)%j+A(j,114)%j+A(j,115)%j+A(j,116)%j+A(j,117)%j+A(j,120)%j+A(j,123)%j+A(j,124)%j &
       +A(j,126)%j+A(j,127)%j+A(j,129)%j+A(j,132)%j+A(j,134)%j+A(j,135)%j+A(j,137)%j+A(j,138)%j+A(j,139)%j+A(j,141)%j+A(j,145)%j &
       +A(j,148)%j+A(j,149)%j+A(j,150)%j+A(j,154)%j+A(j,156)%j+A(j,160)%j+A(j,161)%j+A(j,163)%j+A(j,164)%j+A(j,166)%j+A(j,167)%j &
       +A(j,169)%j+A(j,172)%j+A(j,175)%j+A(j,176)%j+A(j,177)%j+A(j,180)%j+A(j,181)%j+A(j,184)%j+A(j,185)%j+A(j,186)%j+A(j,187)%j &
       +A(j,189)%j+A(j,194)%j+A(j,195)%j+A(j,197)%j+A(j,200)%j+A(j,201)%j+A(j,202)%j+A(j,205)%j+A(j,208)%j+A(j,209)%j+A(j,212)%j &
       +A(j,215)%j+A(j,216)%j+A(j,217)%j+A(j,220)%j+A(j,221)%j+A(j,222)%j+A(j,227)%j+A(j,228)%j+A(j,229)%j+A(j,232)%j+A(j,233)%j &
       +A(j,234)%j+A(j,237)%j+A(j,240)%j)*f(1))/6._/**/REALKIND

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
  use ol_colourmatrix_ppvvvj_uuuxuxazz_1_/**/REALKIND, only: K1
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
  use ol_colourmatrix_ppvvvj_uuuxuxazz_1_/**/REALKIND, only: K1
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
  & bind(c,name="ol_tree_colvect_ppvvvj_uuuxuxazz_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(2,288)
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
    & bind(c,name="ol_f_amp2tree_ppvvvj_uuuxuxazz_1")
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
    & bind(c,name="ol_f_amp2ccone_ppvvvj_uuuxuxazz_1")
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
    & bind(c,name="ol_f_amp2ccall_ppvvvj_uuuxuxazz_1")
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
    & bind(c,name="ol_f_amp2hcone_ppvvvj_uuuxuxazz_1")
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
    & bind(c,name="ol_f_amp2hcall_ppvvvj_uuuxuxazz_1")
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
    & bind(c,name="ol_amp2tree_ppvvvj_uuuxuxazz_1")
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
    & bind(c,name="ol_amp2ccone_ppvvvj_uuuxuxazz_1")
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
    & bind(c,name="ol_amp2ccall_ppvvvj_uuuxuxazz_1")
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
    & bind(c,name="ol_amp2hcone_ppvvvj_uuuxuxazz_1")
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
    & bind(c,name="ol_amp2hcall_ppvvvj_uuuxuxazz_1")
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
    & bind(c,name="amp2tree_ppvvvj_uuuxuxazz_1_")
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
    & bind(c,name="amp2ccone_ppvvvj_uuuxuxazz_1_")
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
    & bind(c,name="amp2ccall_ppvvvj_uuuxuxazz_1_")
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
    & bind(c,name="amp2hcone_ppvvvj_uuuxuxazz_1_")
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
    & bind(c,name="amp2hcall_ppvvvj_uuuxuxazz_1_")
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

end module ol_tree_ppvvvj_uuuxuxazz_1_/**/REALKIND
