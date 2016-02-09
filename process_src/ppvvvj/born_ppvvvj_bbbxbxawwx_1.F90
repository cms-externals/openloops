
module ol_colourmatrix_ppvvvj_bbbxbxawwx_1_/**/REALKIND
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
end module ol_colourmatrix_ppvvvj_bbbxbxawwx_1_/**/REALKIND



module ol_forced_parameters_ppvvvj_bbbxbxawwx_1_/**/REALKIND
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
  if (wMW /= 0) write(*,101) 'wMW = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppvvvj_bbbxbxawwx_1_/**/REALKIND

module ol_tree_ppvvvj_bbbxbxawwx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(9)
  complex(REALKIND), save :: den(408)
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
    f(1) = (CI*eQED**3*gQCD**2)/9._/**/REALKIND
    f(2) = (CI*eQED**3*gQCD**2)/3._/**/REALKIND
    f(3) = (CI*eQED**3*gQCD**2)/(6._/**/REALKIND*sw**2)
    f(4) = (CI*eQED**3*gQCD**2)/(3._/**/REALKIND*sw**2)
    f(5) = (CI*eQED**3*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(6) = (CI*eQED**3*gQCD**2*MB)/(6._/**/REALKIND*sw**2)
    f(7) = (CI*eQED**3*gQCD**2*MB)/(2._/**/REALKIND*sw**2)
    f(8) = (CI*cw*eQED**3*gQCD**2)/(3._/**/REALKIND*sw)
    f(9) = (CI*cw*eQED**3*gQCD**2)/sw

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,5))
  den(2) = 1 / (Q(5,112))
  den(3) = 1 / (Q(5,7) - MB2)
  den(6) = 1 / (Q(5,112) - MZ2)
  den(8) = 1 / (Q(5,114) - MB2)
  den(13) = 1 / (Q(5,6))
  den(17) = 1 / (Q(5,113) - MB2)
  den(22) = 1 / (Q(5,9))
  den(23) = 1 / (Q(5,11) - MB2)
  den(29) = 1 / (Q(5,10))
  den(35) = 1 / (Q(5,18) - MB2)
  den(36) = 1 / (Q(5,40) - MT2)
  den(38) = 1 / (Q(5,82) - MT2)
  den(42) = 1 / (Q(5,104) - MB2)
  den(45) = 1 / (Q(5,96) - MH2)
  den(47) = 1 / (Q(5,13) - MB2)
  den(50) = 1 / (Q(5,96))
  den(53) = 1 / (Q(5,96) - MZ2)
  den(63) = 1 / (Q(5,66) - MT2)
  den(64) = 1 / (Q(5,24) - MB2)
  den(66) = 1 / (Q(5,98) - MB2)
  den(70) = 1 / (Q(5,56) - MT2)
  den(90) = 1 / (Q(5,80) - MW2)
  den(97) = 1 / (Q(5,48) - MW2)
  den(102) = 1 / (Q(5,112) - MH2)
  den(131) = 1 / (Q(5,17) - MB2)
  den(133) = 1 / (Q(5,81) - MT2)
  den(139) = 1 / (Q(5,14) - MB2)
  den(150) = 1 / (Q(5,65) - MT2)
  den(152) = 1 / (Q(5,97) - MB2)
  den(200) = 1 / (Q(5,36) - MT2)
  den(204) = 1 / (Q(5,100) - MB2)
  den(218) = 1 / (Q(5,20) - MB2)
  den(222) = 1 / (Q(5,52) - MT2)
  den(322) = 1 / (Q(5,25))
  den(327) = 1 / (Q(5,21))
  den(346) = 1 / (Q(5,26))
  den(351) = 1 / (Q(5,22))

  ! denominators

  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(7) = den(4)*den(6)
  den(9) = den(2)*den(8)
  den(10) = den(1)*den(9)
  den(11) = den(6)*den(8)
  den(12) = den(1)*den(11)
  den(14) = den(3)*den(13)
  den(15) = den(2)*den(14)
  den(16) = den(6)*den(14)
  den(18) = den(2)*den(17)
  den(19) = den(13)*den(18)
  den(20) = den(6)*den(17)
  den(21) = den(13)*den(20)
  den(24) = den(22)*den(23)
  den(25) = den(2)*den(24)
  den(26) = den(6)*den(24)
  den(27) = den(9)*den(22)
  den(28) = den(11)*den(22)
  den(30) = den(23)*den(29)
  den(31) = den(2)*den(30)
  den(32) = den(6)*den(30)
  den(33) = den(18)*den(29)
  den(34) = den(20)*den(29)
  den(37) = den(1)*den(36)
  den(39) = den(35)*den(38)
  den(40) = den(37)*den(39)
  den(41) = den(1)*den(35)
  den(43) = den(36)*den(42)
  den(44) = den(41)*den(43)
  den(46) = den(35)*den(45)
  den(48) = den(1)*den(47)
  den(49) = den(46)*den(48)
  den(51) = den(35)*den(50)
  den(52) = den(48)*den(51)
  den(54) = den(35)*den(53)
  den(55) = den(48)*den(54)
  den(56) = den(42)*den(45)
  den(57) = den(41)*den(56)
  den(58) = den(42)*den(50)
  den(59) = den(41)*den(58)
  den(60) = den(42)*den(53)
  den(61) = den(41)*den(60)
  den(62) = den(39)*den(48)
  den(65) = den(1)*den(64)
  den(67) = den(63)*den(66)
  den(68) = den(65)*den(67)
  den(69) = den(1)*den(63)
  den(71) = den(64)*den(70)
  den(72) = den(69)*den(71)
  den(73) = den(45)*den(64)
  den(74) = den(4)*den(73)
  den(75) = den(50)*den(64)
  den(76) = den(4)*den(75)
  den(77) = den(53)*den(64)
  den(78) = den(4)*den(77)
  den(79) = den(45)*den(66)
  den(80) = den(65)*den(79)
  den(81) = den(50)*den(66)
  den(82) = den(65)*den(81)
  den(83) = den(53)*den(66)
  den(84) = den(65)*den(83)
  den(85) = den(4)*den(71)
  den(86) = den(38)*den(63)
  den(87) = den(37)*den(86)
  den(88) = den(36)*den(70)
  den(89) = den(69)*den(88)
  den(91) = den(36)*den(90)
  den(92) = den(4)*den(91)
  den(93) = den(38)*den(90)
  den(94) = den(37)*den(93)
  den(95) = den(4)*den(43)
  den(96) = den(4)*den(88)
  den(98) = den(63)*den(97)
  den(99) = den(48)*den(98)
  den(100) = den(70)*den(97)
  den(101) = den(69)*den(100)
  den(103) = den(97)*den(102)
  den(104) = den(4)*den(103)
  den(105) = den(2)*den(97)
  den(106) = den(4)*den(105)
  den(107) = den(6)*den(97)
  den(108) = den(4)*den(107)
  den(109) = den(4)*den(100)
  den(110) = den(48)*den(103)
  den(111) = den(48)*den(105)
  den(112) = den(48)*den(107)
  den(113) = den(48)*den(86)
  den(114) = den(48)*den(67)
  den(115) = den(90)*den(102)
  den(116) = den(4)*den(115)
  den(117) = den(2)*den(90)
  den(118) = den(4)*den(117)
  den(119) = den(6)*den(90)
  den(120) = den(4)*den(119)
  den(121) = den(48)*den(115)
  den(122) = den(48)*den(117)
  den(123) = den(48)*den(119)
  den(124) = den(48)*den(93)
  den(125) = den(4)*den(56)
  den(126) = den(4)*den(58)
  den(127) = den(4)*den(60)
  den(128) = den(48)*den(79)
  den(129) = den(48)*den(81)
  den(130) = den(48)*den(83)
  den(132) = den(13)*den(36)
  den(134) = den(131)*den(133)
  den(135) = den(132)*den(134)
  den(136) = den(13)*den(131)
  den(137) = den(43)*den(136)
  den(138) = den(45)*den(131)
  den(140) = den(13)*den(139)
  den(141) = den(138)*den(140)
  den(142) = den(50)*den(131)
  den(143) = den(140)*den(142)
  den(144) = den(53)*den(131)
  den(145) = den(140)*den(144)
  den(146) = den(56)*den(136)
  den(147) = den(58)*den(136)
  den(148) = den(60)*den(136)
  den(149) = den(134)*den(140)
  den(151) = den(13)*den(64)
  den(153) = den(150)*den(152)
  den(154) = den(151)*den(153)
  den(155) = den(13)*den(150)
  den(156) = den(71)*den(155)
  den(157) = den(14)*den(73)
  den(158) = den(14)*den(75)
  den(159) = den(14)*den(77)
  den(160) = den(45)*den(152)
  den(161) = den(151)*den(160)
  den(162) = den(50)*den(152)
  den(163) = den(151)*den(162)
  den(164) = den(53)*den(152)
  den(165) = den(151)*den(164)
  den(166) = den(14)*den(71)
  den(167) = den(133)*den(150)
  den(168) = den(132)*den(167)
  den(169) = den(88)*den(155)
  den(170) = den(14)*den(91)
  den(171) = den(90)*den(133)
  den(172) = den(132)*den(171)
  den(173) = den(14)*den(43)
  den(174) = den(14)*den(88)
  den(175) = den(97)*den(150)
  den(176) = den(140)*den(175)
  den(177) = den(100)*den(155)
  den(178) = den(14)*den(103)
  den(179) = den(14)*den(105)
  den(180) = den(14)*den(107)
  den(181) = den(14)*den(100)
  den(182) = den(103)*den(140)
  den(183) = den(105)*den(140)
  den(184) = den(107)*den(140)
  den(185) = den(140)*den(167)
  den(186) = den(140)*den(153)
  den(187) = den(14)*den(115)
  den(188) = den(14)*den(117)
  den(189) = den(14)*den(119)
  den(190) = den(115)*den(140)
  den(191) = den(117)*den(140)
  den(192) = den(119)*den(140)
  den(193) = den(140)*den(171)
  den(194) = den(14)*den(56)
  den(195) = den(14)*den(58)
  den(196) = den(14)*den(60)
  den(197) = den(140)*den(160)
  den(198) = den(140)*den(162)
  den(199) = den(140)*den(164)
  den(201) = den(22)*den(200)
  den(202) = den(39)*den(201)
  den(203) = den(22)*den(35)
  den(205) = den(200)*den(204)
  den(206) = den(203)*den(205)
  den(207) = den(22)*den(47)
  den(208) = den(46)*den(207)
  den(209) = den(51)*den(207)
  den(210) = den(54)*den(207)
  den(211) = den(45)*den(204)
  den(212) = den(203)*den(211)
  den(213) = den(50)*den(204)
  den(214) = den(203)*den(213)
  den(215) = den(53)*den(204)
  den(216) = den(203)*den(215)
  den(217) = den(39)*den(207)
  den(219) = den(22)*den(218)
  den(220) = den(67)*den(219)
  den(221) = den(22)*den(63)
  den(223) = den(218)*den(222)
  den(224) = den(221)*den(223)
  den(225) = den(45)*den(218)
  den(226) = den(24)*den(225)
  den(227) = den(50)*den(218)
  den(228) = den(24)*den(227)
  den(229) = den(53)*den(218)
  den(230) = den(24)*den(229)
  den(231) = den(79)*den(219)
  den(232) = den(81)*den(219)
  den(233) = den(83)*den(219)
  den(234) = den(24)*den(223)
  den(235) = den(86)*den(201)
  den(236) = den(200)*den(222)
  den(237) = den(221)*den(236)
  den(238) = den(90)*den(200)
  den(239) = den(24)*den(238)
  den(240) = den(93)*den(201)
  den(241) = den(24)*den(205)
  den(242) = den(24)*den(236)
  den(243) = den(98)*den(207)
  den(244) = den(97)*den(222)
  den(245) = den(221)*den(244)
  den(246) = den(24)*den(103)
  den(247) = den(24)*den(105)
  den(248) = den(24)*den(107)
  den(249) = den(24)*den(244)
  den(250) = den(103)*den(207)
  den(251) = den(105)*den(207)
  den(252) = den(107)*den(207)
  den(253) = den(86)*den(207)
  den(254) = den(67)*den(207)
  den(255) = den(24)*den(115)
  den(256) = den(24)*den(117)
  den(257) = den(24)*den(119)
  den(258) = den(115)*den(207)
  den(259) = den(117)*den(207)
  den(260) = den(119)*den(207)
  den(261) = den(93)*den(207)
  den(262) = den(24)*den(211)
  den(263) = den(24)*den(213)
  den(264) = den(24)*den(215)
  den(265) = den(79)*den(207)
  den(266) = den(81)*den(207)
  den(267) = den(83)*den(207)
  den(268) = den(29)*den(200)
  den(269) = den(134)*den(268)
  den(270) = den(29)*den(131)
  den(271) = den(205)*den(270)
  den(272) = den(29)*den(139)
  den(273) = den(138)*den(272)
  den(274) = den(142)*den(272)
  den(275) = den(144)*den(272)
  den(276) = den(211)*den(270)
  den(277) = den(213)*den(270)
  den(278) = den(215)*den(270)
  den(279) = den(134)*den(272)
  den(280) = den(29)*den(218)
  den(281) = den(153)*den(280)
  den(282) = den(29)*den(150)
  den(283) = den(223)*den(282)
  den(284) = den(30)*den(225)
  den(285) = den(30)*den(227)
  den(286) = den(30)*den(229)
  den(287) = den(160)*den(280)
  den(288) = den(162)*den(280)
  den(289) = den(164)*den(280)
  den(290) = den(30)*den(223)
  den(291) = den(167)*den(268)
  den(292) = den(236)*den(282)
  den(293) = den(30)*den(238)
  den(294) = den(171)*den(268)
  den(295) = den(30)*den(205)
  den(296) = den(30)*den(236)
  den(297) = den(175)*den(272)
  den(298) = den(244)*den(282)
  den(299) = den(30)*den(103)
  den(300) = den(30)*den(105)
  den(301) = den(30)*den(107)
  den(302) = den(30)*den(244)
  den(303) = den(103)*den(272)
  den(304) = den(105)*den(272)
  den(305) = den(107)*den(272)
  den(306) = den(167)*den(272)
  den(307) = den(153)*den(272)
  den(308) = den(30)*den(115)
  den(309) = den(30)*den(117)
  den(310) = den(30)*den(119)
  den(311) = den(115)*den(272)
  den(312) = den(117)*den(272)
  den(313) = den(119)*den(272)
  den(314) = den(171)*den(272)
  den(315) = den(30)*den(211)
  den(316) = den(30)*den(213)
  den(317) = den(30)*den(215)
  den(318) = den(160)*den(272)
  den(319) = den(162)*den(272)
  den(320) = den(164)*den(272)
  den(321) = den(63)*den(200)
  den(323) = den(131)*den(322)
  den(324) = den(321)*den(323)
  den(325) = den(205)*den(323)
  den(326) = den(36)*den(63)
  den(328) = den(131)*den(327)
  den(329) = den(326)*den(328)
  den(330) = den(43)*den(328)
  den(331) = den(67)*den(328)
  den(332) = den(67)*den(323)
  den(333) = den(56)*den(328)
  den(334) = den(58)*den(328)
  den(335) = den(60)*den(328)
  den(336) = den(79)*den(328)
  den(337) = den(81)*den(328)
  den(338) = den(83)*den(328)
  den(339) = den(211)*den(323)
  den(340) = den(213)*den(323)
  den(341) = den(215)*den(323)
  den(342) = den(79)*den(323)
  den(343) = den(81)*den(323)
  den(344) = den(83)*den(323)
  den(345) = den(150)*den(200)
  den(347) = den(35)*den(346)
  den(348) = den(345)*den(347)
  den(349) = den(205)*den(347)
  den(350) = den(36)*den(150)
  den(352) = den(35)*den(351)
  den(353) = den(350)*den(352)
  den(354) = den(43)*den(352)
  den(355) = den(153)*den(352)
  den(356) = den(153)*den(347)
  den(357) = den(56)*den(352)
  den(358) = den(58)*den(352)
  den(359) = den(60)*den(352)
  den(360) = den(211)*den(347)
  den(361) = den(213)*den(347)
  den(362) = den(215)*den(347)
  den(363) = den(160)*den(347)
  den(364) = den(162)*den(347)
  den(365) = den(164)*den(347)
  den(366) = den(160)*den(352)
  den(367) = den(162)*den(352)
  den(368) = den(164)*den(352)
  den(369) = den(218)*den(351)
  den(370) = den(350)*den(369)
  den(371) = den(218)*den(327)
  den(372) = den(326)*den(371)
  den(373) = den(43)*den(371)
  den(374) = den(43)*den(369)
  den(375) = den(153)*den(369)
  den(376) = den(67)*den(371)
  den(377) = den(56)*den(371)
  den(378) = den(58)*den(371)
  den(379) = den(60)*den(371)
  den(380) = den(56)*den(369)
  den(381) = den(58)*den(369)
  den(382) = den(60)*den(369)
  den(383) = den(79)*den(371)
  den(384) = den(81)*den(371)
  den(385) = den(83)*den(371)
  den(386) = den(160)*den(369)
  den(387) = den(162)*den(369)
  den(388) = den(164)*den(369)
  den(389) = den(64)*den(346)
  den(390) = den(345)*den(389)
  den(391) = den(64)*den(322)
  den(392) = den(321)*den(391)
  den(393) = den(205)*den(391)
  den(394) = den(205)*den(389)
  den(395) = den(153)*den(389)
  den(396) = den(67)*den(391)
  den(397) = den(211)*den(389)
  den(398) = den(213)*den(389)
  den(399) = den(215)*den(389)
  den(400) = den(211)*den(391)
  den(401) = den(213)*den(391)
  den(402) = den(215)*den(391)
  den(403) = den(79)*den(391)
  den(404) = den(81)*den(391)
  den(405) = den(83)*den(391)
  den(406) = den(160)*den(389)
  den(407) = den(162)*den(389)
  den(408) = den(164)*den(389)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_ppvvvj_bbbxbxawwx_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_ppvvvj_bbbxbxawwx_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for bottom bottom anti-bottom anti-bottom gamma W- W+ -> 0
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
  use ol_external_ppvvvj_bbbxbxawwx_1, only: external_perm_ppvvvj_bbbxbxawwx_1, &
    & external_perm_inv_ppvvvj_bbbxbxawwx_1, extcomb_perm_ppvvvj_bbbxbxawwx_1, &
    & average_factor_ppvvvj_bbbxbxawwx_1
  use ol_external_ppvvvj_bbbxbxawwx_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_ppvvvj_bbbxbxawwx_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_ppvvvj_bbbxbxawwx_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_ppvvvj_bbbxbxawwx_1
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
  type(wfun) :: wf4(4,12), wf6(6,12), wf8(8,24), wf9(9,3), wf12(12,24), wf16(16,56), wf18(18,40), wf24(24,16), wf36(36,58), &
    wf288(288,280)

  type(polcont) :: A(288,280)
  complex(REALKIND) :: Aj(280)

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
  extmasses2 = [ rMB2, rMB2, rMB2, rMB2, rZERO2, rMW2, rMW2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_ppvvvj_bbbxbxawwx_1,7)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,7)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_ppvvvj_bbbxbxawwx_1(I)
  else if (I < 0) then
    JBmunu = external_perm_ppvvvj_bbbxbxawwx_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_ppvvvj_bbbxbxawwx_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_Q(P(:,1), rMB, H1, ex1)
  call wf_Q(P(:,2), rMB, H2, ex2)
  call wf_A(P(:,3), rMB, H3, ex3)
  call wf_A(P(:,4), rMB, H4, ex4)
  call wf_V(P(:,5), rZERO, H5, ex5)
  call wf_V(P(:,6), rMW, H6, ex6)
  call wf_V(P(:,7), rMW, H7, ex7)


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
  call vert_WWV_V(ntry, ex6, ex7, ex5, wf18(:,1), n4(:,1), t4x18(:,:,1))
  call vert_VQ_A(ntry, wf4(:,1), ex2, wf8(:,1), n3(:,2), t3x8(:,:,1))
  call vert_AV_Q(ntry, ex4, wf18(:,1), wf36(:,1), n3(:,3), t3x36(:,:,1))
  call prop_Q_A(ntry, wf8(:,1), Q(:,7), MB, 1_intkind1, wf8(:,2), n2(1))
  call prop_W_W(ntry, wf18(:,1), Q(:,112), MZ, 1_intkind1, wf18(:,2), n2(2))
  call vert_AZ_Q(gZd,ntry, ex4, wf18(:,2), wf36(:,2), n3(:,4), t3x36(:,:,2))
  call vert_VQ_A(ntry, wf18(:,1), ex2, wf36(:,3), n3(:,5), t3x36(:,:,3))
  call vert_AV_Q(ntry, ex4, wf4(:,1), wf8(:,3), n3(:,6), t3x8(:,:,2))
  call prop_Q_A(ntry, wf36(:,3), Q(:,114), MB, 1_intkind1, wf36(:,4), n2(3))
  call vert_ZQ_A(gZd,ntry, wf18(:,2), ex2, wf36(:,5), n3(:,7), t3x36(:,:,4))
  call prop_Q_A(ntry, wf36(:,5), Q(:,114), MB, 1_intkind1, wf36(:,6), n2(4))
  call vert_QA_V(ntry, ex2, ex3, wf4(:,2), n3(:,8), t3x4(:,:,2))
  call vert_VQ_A(ntry, wf4(:,2), ex1, wf8(:,4), n3(:,9), t3x8(:,:,3))
  call prop_Q_A(ntry, wf8(:,4), Q(:,7), MB, 1_intkind1, wf8(:,5), n2(5))
  call vert_VQ_A(ntry, wf18(:,1), ex1, wf36(:,7), n3(:,10), t3x36(:,:,5))
  call vert_AV_Q(ntry, ex4, wf4(:,2), wf8(:,6), n3(:,11), t3x8(:,:,4))
  call prop_Q_A(ntry, wf36(:,7), Q(:,113), MB, 1_intkind1, wf36(:,8), n2(6))
  call vert_ZQ_A(gZd,ntry, wf18(:,2), ex1, wf36(:,9), n3(:,12), t3x36(:,:,6))
  call prop_Q_A(ntry, wf36(:,9), Q(:,113), MB, 1_intkind1, wf36(:,10), n2(7))
  call vert_QA_V(ntry, ex1, ex4, wf4(:,3), n3(:,13), t3x4(:,:,3))
  call vert_VQ_A(ntry, wf4(:,3), ex2, wf8(:,7), n3(:,14), t3x8(:,:,5))
  call vert_AV_Q(ntry, ex3, wf18(:,1), wf36(:,11), n3(:,15), t3x36(:,:,7))
  call prop_Q_A(ntry, wf8(:,7), Q(:,11), MB, 1_intkind1, wf8(:,8), n2(8))
  call vert_AZ_Q(gZd,ntry, ex3, wf18(:,2), wf36(:,12), n3(:,16), t3x36(:,:,8))
  call vert_AV_Q(ntry, ex3, wf4(:,3), wf8(:,9), n3(:,17), t3x8(:,:,6))
  call vert_QA_V(ntry, ex2, ex4, wf4(:,4), n3(:,18), t3x4(:,:,4))
  call vert_VQ_A(ntry, wf4(:,4), ex1, wf8(:,10), n3(:,19), t3x8(:,:,7))
  call prop_Q_A(ntry, wf8(:,10), Q(:,11), MB, 1_intkind1, wf8(:,11), n2(9))
  call vert_AV_Q(ntry, ex3, wf4(:,4), wf8(:,12), n3(:,20), t3x8(:,:,8))
  call vert_VQ_A(ntry, ex5, ex2, wf4(:,5), n3(:,21), t3x4(:,:,5))
  call vert_AW_Q(ntry, ex4, ex6, wf6(:,1), n3(:,22), t3x6(:,:,1))
  call prop_Q_A(ntry, wf4(:,5), Q(:,18), MB, 1_intkind1, wf4(:,6), n2(10))
  call prop_A_Q(ntry, wf6(:,1), Q(:,40), MT, 1_intkind1, wf6(:,2), n2(11))
  call vert_WQ_A(ntry, ex7, wf4(:,6), wf12(:,1), n3(:,23), t3x12(:,:,1))
  call vert_AV_Q(ntry, wf6(:,2), wf4(:,1), wf24(:,1), n3(:,24), t3x24(:,:,1))
  call prop_Q_A(ntry, wf12(:,1), Q(:,82), MT, 1_intkind1, wf12(:,2), n2(12))
  call vert_AW_Q(ntry, wf6(:,2), ex7, wf18(:,3), n3(:,25), t3x18(:,:,1))
  call vert_VQ_A(ntry, wf4(:,1), wf4(:,6), wf16(:,1), n3(:,26), t3x16(:,:,1))
  call prop_A_Q(ntry, wf18(:,3), Q(:,104), MB, 1_intkind1, wf18(:,4), n2(13))
  call vert_VV_S(ntry, ex6, ex7, wf9(:,1), n3(:,27), t3x9(:,:,1))
  call vert_QS_A(gH,ntry, wf4(:,6), wf9(:,1), wf36(:,13), n3(:,28), t3x36(:,:,9))
  call prop_A_Q(ntry, wf8(:,3), Q(:,13), MB, 1_intkind1, wf8(:,13), n2(14))
  call vert_UV_W(ntry, ex7, Q(:,64), ex6, Q(:,32), wf9(:,2), n3(:,29), t3x9(:,:,2))
  call vert_VQ_A(ntry, wf9(:,2), wf4(:,6), wf36(:,14), n3(:,30), t3x36(:,:,10))
  call prop_W_W(ntry, wf9(:,2), Q(:,96), MZ, 1_intkind1, wf9(:,3), n2(15))
  call vert_ZQ_A(gZd,ntry, wf9(:,3), wf4(:,6), wf36(:,15), n3(:,31), t3x36(:,:,11))
  call vert_SA_Q(gH,ntry, wf9(:,1), ex4, wf18(:,5), n3(:,32), t3x18(:,:,2))
  call prop_A_Q(ntry, wf18(:,5), Q(:,104), MB, 1_intkind1, wf18(:,6), n2(16))
  call vert_AV_Q(ntry, ex4, wf9(:,2), wf18(:,7), n3(:,33), t3x18(:,:,3))
  call prop_A_Q(ntry, wf18(:,7), Q(:,104), MB, 1_intkind1, wf18(:,8), n2(17))
  call vert_AZ_Q(gZd,ntry, ex4, wf9(:,3), wf18(:,9), n3(:,34), t3x18(:,:,4))
  call prop_A_Q(ntry, wf18(:,9), Q(:,104), MB, 1_intkind1, wf18(:,10), n2(18))
  call vert_AW_Q(ntry, wf8(:,13), ex6, wf24(:,2), n3(:,35), t3x24(:,:,2))
  call vert_WQ_A(ntry, ex7, ex2, wf6(:,3), n3(:,36), t3x6(:,:,2))
  call vert_AV_Q(ntry, ex4, ex5, wf4(:,7), n3(:,37), t3x4(:,:,6))
  call prop_Q_A(ntry, wf6(:,3), Q(:,66), MT, 1_intkind1, wf6(:,4), n2(19))
  call prop_A_Q(ntry, wf4(:,7), Q(:,24), MB, 1_intkind1, wf4(:,8), n2(20))
  call vert_WQ_A(ntry, ex6, wf6(:,4), wf18(:,11), n3(:,38), t3x18(:,:,5))
  call vert_AV_Q(ntry, wf4(:,8), wf4(:,1), wf16(:,2), n3(:,39), t3x16(:,:,2))
  call prop_Q_A(ntry, wf18(:,11), Q(:,98), MB, 1_intkind1, wf18(:,12), n2(21))
  call vert_AW_Q(ntry, wf4(:,8), ex6, wf12(:,3), n3(:,40), t3x12(:,:,2))
  call vert_VQ_A(ntry, wf4(:,1), wf6(:,4), wf24(:,3), n3(:,41), t3x24(:,:,3))
  call prop_A_Q(ntry, wf12(:,3), Q(:,56), MT, 1_intkind1, wf12(:,4), n2(22))
  call vert_SA_Q(gH,ntry, wf9(:,1), wf4(:,8), wf36(:,16), n3(:,42), t3x36(:,:,12))
  call vert_AV_Q(ntry, wf4(:,8), wf9(:,2), wf36(:,17), n3(:,43), t3x36(:,:,13))
  call vert_AZ_Q(gZd,ntry, wf4(:,8), wf9(:,3), wf36(:,18), n3(:,44), t3x36(:,:,14))
  call vert_QS_A(gH,ntry, ex2, wf9(:,1), wf18(:,13), n3(:,45), t3x18(:,:,6))
  call prop_Q_A(ntry, wf18(:,13), Q(:,98), MB, 1_intkind1, wf18(:,14), n2(23))
  call vert_VQ_A(ntry, wf9(:,2), ex2, wf18(:,15), n3(:,46), t3x18(:,:,7))
  call prop_Q_A(ntry, wf18(:,15), Q(:,98), MB, 1_intkind1, wf18(:,16), n2(24))
  call vert_ZQ_A(gZd,ntry, wf9(:,3), ex2, wf18(:,17), n3(:,47), t3x18(:,:,8))
  call prop_Q_A(ntry, wf18(:,17), Q(:,98), MB, 1_intkind1, wf18(:,18), n2(25))
  call vert_WQ_A(ntry, ex7, wf8(:,2), wf24(:,4), n3(:,48), t3x24(:,:,4))
  call vert_VQ_A(ntry, ex5, wf6(:,4), wf12(:,5), n3(:,49), t3x12(:,:,3))
  call prop_Q_A(ntry, wf12(:,5), Q(:,82), MT, 1_intkind1, wf12(:,6), n2(26))
  call vert_AV_Q(ntry, wf6(:,2), ex5, wf12(:,7), n3(:,50), t3x12(:,:,4))
  call prop_A_Q(ntry, wf12(:,7), Q(:,56), MT, 1_intkind1, wf12(:,8), n2(27))
  call vert_UV_W(ntry, ex5, Q(:,16), ex7, Q(:,64), wf6(:,5), n3(:,51), t3x6(:,:,3))
  call prop_W_W(ntry, wf6(:,5), Q(:,80), MW, 1_intkind1, wf6(:,6), n2(28))
  call vert_AW_Q(ntry, wf6(:,2), wf6(:,6), wf36(:,19), n3(:,52), t3x36(:,:,15))
  call vert_WQ_A(ntry, wf6(:,6), ex2, wf12(:,9), n3(:,53), t3x12(:,:,5))
  call prop_Q_A(ntry, wf12(:,9), Q(:,82), MT, 1_intkind1, wf12(:,10), n2(29))
  call vert_VQ_A(ntry, ex5, wf8(:,2), wf16(:,3), n3(:,54), t3x16(:,:,3))
  call vert_UV_W(ntry, ex6, Q(:,32), ex5, Q(:,16), wf6(:,7), n3(:,55), t3x6(:,:,4))
  call prop_W_W(ntry, wf6(:,7), Q(:,48), MW, 1_intkind1, wf6(:,8), n2(30))
  call vert_WQ_A(ntry, wf6(:,8), wf6(:,4), wf36(:,20), n3(:,56), t3x36(:,:,16))
  call vert_AW_Q(ntry, ex4, wf6(:,8), wf12(:,11), n3(:,57), t3x12(:,:,6))
  call prop_A_Q(ntry, wf12(:,11), Q(:,56), MT, 1_intkind1, wf12(:,12), n2(31))
  call vert_VV_S(ntry, wf6(:,8), ex7, wf18(:,19), n3(:,58), t3x18(:,:,9))
  call vert_AQ_S(gH,ntry, ex4, wf8(:,2), wf16(:,4), n3(:,59), t3x16(:,:,4))
  call vert_UV_W(ntry, ex7, Q(:,64), wf6(:,8), Q(:,48), wf18(:,20), n3(:,60), t3x18(:,:,10))
  call vert_QA_V(ntry, wf8(:,2), ex4, wf16(:,5), n3(:,61), t3x16(:,:,5))
  call prop_W_W(ntry, wf18(:,20), Q(:,112), MZ, 1_intkind1, wf18(:,21), n2(32))
  call vert_QA_Z(gZd,ntry, wf8(:,2), ex4, wf16(:,6), n3(:,62), t3x16(:,:,6))
  call vert_AQ_S(gH,ntry, wf8(:,13), ex2, wf16(:,7), n3(:,63), t3x16(:,:,7))
  call vert_QA_V(ntry, ex2, wf8(:,13), wf16(:,8), n3(:,64), t3x16(:,:,8))
  call vert_QA_Z(gZd,ntry, ex2, wf8(:,13), wf16(:,9), n3(:,65), t3x16(:,:,9))
  call vert_AV_Q(ntry, wf8(:,13), ex5, wf16(:,10), n3(:,66), t3x16(:,:,10))
  call vert_VV_S(ntry, ex6, wf6(:,6), wf18(:,22), n3(:,67), t3x18(:,:,11))
  call vert_UV_W(ntry, wf6(:,6), Q(:,80), ex6, Q(:,32), wf18(:,23), n3(:,68), t3x18(:,:,12))
  call prop_W_W(ntry, wf18(:,23), Q(:,112), MZ, 1_intkind1, wf18(:,24), n2(33))
  call vert_WQ_A(ntry, ex6, wf12(:,10), wf36(:,21), n3(:,69), t3x36(:,:,17))
  call vert_VQ_A(ntry, ex5, wf18(:,14), wf36(:,22), n3(:,70), t3x36(:,:,18))
  call vert_VQ_A(ntry, ex5, wf18(:,16), wf36(:,23), n3(:,71), t3x36(:,:,19))
  call vert_VQ_A(ntry, ex5, wf18(:,18), wf36(:,24), n3(:,72), t3x36(:,:,20))
  call vert_VQ_A(ntry, ex5, ex1, wf4(:,9), n3(:,73), t3x4(:,:,7))
  call prop_Q_A(ntry, wf4(:,9), Q(:,17), MB, 1_intkind1, wf4(:,10), n2(34))
  call vert_WQ_A(ntry, ex7, wf4(:,10), wf12(:,13), n3(:,74), t3x12(:,:,7))
  call vert_AV_Q(ntry, wf6(:,2), wf4(:,2), wf24(:,5), n3(:,75), t3x24(:,:,5))
  call prop_Q_A(ntry, wf12(:,13), Q(:,81), MT, 1_intkind1, wf12(:,14), n2(35))
  call vert_VQ_A(ntry, wf4(:,2), wf4(:,10), wf16(:,11), n3(:,76), t3x16(:,:,11))
  call vert_QS_A(gH,ntry, wf4(:,10), wf9(:,1), wf36(:,25), n3(:,77), t3x36(:,:,21))
  call prop_A_Q(ntry, wf8(:,6), Q(:,14), MB, 1_intkind1, wf8(:,14), n2(36))
  call vert_VQ_A(ntry, wf9(:,2), wf4(:,10), wf36(:,26), n3(:,78), t3x36(:,:,22))
  call vert_ZQ_A(gZd,ntry, wf9(:,3), wf4(:,10), wf36(:,27), n3(:,79), t3x36(:,:,23))
  call vert_AW_Q(ntry, wf8(:,14), ex6, wf24(:,6), n3(:,80), t3x24(:,:,6))
  call vert_WQ_A(ntry, ex7, ex1, wf6(:,9), n3(:,81), t3x6(:,:,5))
  call prop_Q_A(ntry, wf6(:,9), Q(:,65), MT, 1_intkind1, wf6(:,10), n2(37))
  call vert_WQ_A(ntry, ex6, wf6(:,10), wf18(:,25), n3(:,82), t3x18(:,:,13))
  call vert_AV_Q(ntry, wf4(:,8), wf4(:,2), wf16(:,12), n3(:,83), t3x16(:,:,12))
  call prop_Q_A(ntry, wf18(:,25), Q(:,97), MB, 1_intkind1, wf18(:,26), n2(38))
  call vert_VQ_A(ntry, wf4(:,2), wf6(:,10), wf24(:,7), n3(:,84), t3x24(:,:,7))
  call vert_QS_A(gH,ntry, ex1, wf9(:,1), wf18(:,27), n3(:,85), t3x18(:,:,14))
  call prop_Q_A(ntry, wf18(:,27), Q(:,97), MB, 1_intkind1, wf18(:,28), n2(39))
  call vert_VQ_A(ntry, wf9(:,2), ex1, wf18(:,29), n3(:,86), t3x18(:,:,15))
  call prop_Q_A(ntry, wf18(:,29), Q(:,97), MB, 1_intkind1, wf18(:,30), n2(40))
  call vert_ZQ_A(gZd,ntry, wf9(:,3), ex1, wf18(:,31), n3(:,87), t3x18(:,:,16))
  call prop_Q_A(ntry, wf18(:,31), Q(:,97), MB, 1_intkind1, wf18(:,32), n2(41))
  call vert_WQ_A(ntry, ex7, wf8(:,5), wf24(:,8), n3(:,88), t3x24(:,:,8))
  call vert_VQ_A(ntry, ex5, wf6(:,10), wf12(:,15), n3(:,89), t3x12(:,:,8))
  call prop_Q_A(ntry, wf12(:,15), Q(:,81), MT, 1_intkind1, wf12(:,16), n2(42))
  call vert_WQ_A(ntry, wf6(:,6), ex1, wf12(:,17), n3(:,90), t3x12(:,:,9))
  call prop_Q_A(ntry, wf12(:,17), Q(:,81), MT, 1_intkind1, wf12(:,18), n2(43))
  call vert_VQ_A(ntry, ex5, wf8(:,5), wf16(:,13), n3(:,91), t3x16(:,:,13))
  call vert_WQ_A(ntry, wf6(:,8), wf6(:,10), wf36(:,28), n3(:,92), t3x36(:,:,24))
  call vert_AQ_S(gH,ntry, ex4, wf8(:,5), wf16(:,14), n3(:,93), t3x16(:,:,14))
  call vert_QA_V(ntry, wf8(:,5), ex4, wf16(:,15), n3(:,94), t3x16(:,:,15))
  call vert_QA_Z(gZd,ntry, wf8(:,5), ex4, wf16(:,16), n3(:,95), t3x16(:,:,16))
  call vert_AQ_S(gH,ntry, wf8(:,14), ex1, wf16(:,17), n3(:,96), t3x16(:,:,17))
  call vert_QA_V(ntry, ex1, wf8(:,14), wf16(:,18), n3(:,97), t3x16(:,:,18))
  call vert_QA_Z(gZd,ntry, ex1, wf8(:,14), wf16(:,19), n3(:,98), t3x16(:,:,19))
  call vert_AV_Q(ntry, wf8(:,14), ex5, wf16(:,20), n3(:,99), t3x16(:,:,20))
  call vert_WQ_A(ntry, ex6, wf12(:,18), wf36(:,29), n3(:,100), t3x36(:,:,25))
  call vert_VQ_A(ntry, ex5, wf18(:,28), wf36(:,30), n3(:,101), t3x36(:,:,26))
  call vert_VQ_A(ntry, ex5, wf18(:,30), wf36(:,31), n3(:,102), t3x36(:,:,27))
  call vert_VQ_A(ntry, ex5, wf18(:,32), wf36(:,32), n3(:,103), t3x36(:,:,28))
  call vert_AW_Q(ntry, ex3, ex6, wf6(:,11), n3(:,104), t3x6(:,:,6))
  call prop_A_Q(ntry, wf6(:,11), Q(:,36), MT, 1_intkind1, wf6(:,12), n2(44))
  call vert_AV_Q(ntry, wf6(:,12), wf4(:,3), wf24(:,9), n3(:,105), t3x24(:,:,9))
  call vert_AW_Q(ntry, wf6(:,12), ex7, wf18(:,33), n3(:,106), t3x18(:,:,17))
  call vert_VQ_A(ntry, wf4(:,3), wf4(:,6), wf16(:,21), n3(:,107), t3x16(:,:,21))
  call prop_A_Q(ntry, wf18(:,33), Q(:,100), MB, 1_intkind1, wf18(:,34), n2(45))
  call prop_A_Q(ntry, wf8(:,9), Q(:,13), MB, 1_intkind1, wf8(:,15), n2(46))
  call vert_SA_Q(gH,ntry, wf9(:,1), ex3, wf18(:,35), n3(:,108), t3x18(:,:,18))
  call prop_A_Q(ntry, wf18(:,35), Q(:,100), MB, 1_intkind1, wf18(:,36), n2(47))
  call vert_AV_Q(ntry, ex3, wf9(:,2), wf18(:,37), n3(:,109), t3x18(:,:,19))
  call prop_A_Q(ntry, wf18(:,37), Q(:,100), MB, 1_intkind1, wf18(:,38), n2(48))
  call vert_AZ_Q(gZd,ntry, ex3, wf9(:,3), wf18(:,39), n3(:,110), t3x18(:,:,20))
  call prop_A_Q(ntry, wf18(:,39), Q(:,100), MB, 1_intkind1, wf18(:,40), n2(49))
  call vert_AW_Q(ntry, wf8(:,15), ex6, wf24(:,10), n3(:,111), t3x24(:,:,10))
  call vert_AV_Q(ntry, ex3, ex5, wf4(:,11), n3(:,112), t3x4(:,:,8))
  call prop_A_Q(ntry, wf4(:,11), Q(:,20), MB, 1_intkind1, wf4(:,12), n2(50))
  call vert_AV_Q(ntry, wf4(:,12), wf4(:,3), wf16(:,22), n3(:,113), t3x16(:,:,22))
  call vert_AW_Q(ntry, wf4(:,12), ex6, wf12(:,19), n3(:,114), t3x12(:,:,10))
  call vert_VQ_A(ntry, wf4(:,3), wf6(:,4), wf24(:,11), n3(:,115), t3x24(:,:,11))
  call prop_A_Q(ntry, wf12(:,19), Q(:,52), MT, 1_intkind1, wf12(:,20), n2(51))
  call vert_SA_Q(gH,ntry, wf9(:,1), wf4(:,12), wf36(:,33), n3(:,116), t3x36(:,:,29))
  call vert_AV_Q(ntry, wf4(:,12), wf9(:,2), wf36(:,34), n3(:,117), t3x36(:,:,30))
  call vert_AZ_Q(gZd,ntry, wf4(:,12), wf9(:,3), wf36(:,35), n3(:,118), t3x36(:,:,31))
  call vert_WQ_A(ntry, ex7, wf8(:,8), wf24(:,12), n3(:,119), t3x24(:,:,12))
  call vert_AV_Q(ntry, wf6(:,12), ex5, wf12(:,21), n3(:,120), t3x12(:,:,11))
  call prop_A_Q(ntry, wf12(:,21), Q(:,52), MT, 1_intkind1, wf12(:,22), n2(52))
  call vert_AW_Q(ntry, wf6(:,12), wf6(:,6), wf36(:,36), n3(:,121), t3x36(:,:,32))
  call vert_VQ_A(ntry, ex5, wf8(:,8), wf16(:,23), n3(:,122), t3x16(:,:,23))
  call vert_AW_Q(ntry, ex3, wf6(:,8), wf12(:,23), n3(:,123), t3x12(:,:,12))
  call prop_A_Q(ntry, wf12(:,23), Q(:,52), MT, 1_intkind1, wf12(:,24), n2(53))
  call vert_AQ_S(gH,ntry, ex3, wf8(:,8), wf16(:,24), n3(:,124), t3x16(:,:,24))
  call vert_QA_V(ntry, wf8(:,8), ex3, wf16(:,25), n3(:,125), t3x16(:,:,25))
  call vert_QA_Z(gZd,ntry, wf8(:,8), ex3, wf16(:,26), n3(:,126), t3x16(:,:,26))
  call vert_AQ_S(gH,ntry, wf8(:,15), ex2, wf16(:,27), n3(:,127), t3x16(:,:,27))
  call vert_QA_V(ntry, ex2, wf8(:,15), wf16(:,28), n3(:,128), t3x16(:,:,28))
  call vert_QA_Z(gZd,ntry, ex2, wf8(:,15), wf16(:,29), n3(:,129), t3x16(:,:,29))
  call vert_AV_Q(ntry, wf8(:,15), ex5, wf16(:,30), n3(:,130), t3x16(:,:,30))
  call vert_AV_Q(ntry, wf6(:,12), wf4(:,4), wf24(:,13), n3(:,131), t3x24(:,:,13))
  call vert_VQ_A(ntry, wf4(:,4), wf4(:,10), wf16(:,31), n3(:,132), t3x16(:,:,31))
  call prop_A_Q(ntry, wf8(:,12), Q(:,14), MB, 1_intkind1, wf8(:,16), n2(54))
  call vert_AW_Q(ntry, wf8(:,16), ex6, wf24(:,14), n3(:,133), t3x24(:,:,14))
  call vert_AV_Q(ntry, wf4(:,12), wf4(:,4), wf16(:,32), n3(:,134), t3x16(:,:,32))
  call vert_VQ_A(ntry, wf4(:,4), wf6(:,10), wf24(:,15), n3(:,135), t3x24(:,:,15))
  call vert_WQ_A(ntry, ex7, wf8(:,11), wf24(:,16), n3(:,136), t3x24(:,:,16))
  call vert_VQ_A(ntry, ex5, wf8(:,11), wf16(:,33), n3(:,137), t3x16(:,:,33))
  call vert_AQ_S(gH,ntry, ex3, wf8(:,11), wf16(:,34), n3(:,138), t3x16(:,:,34))
  call vert_QA_V(ntry, wf8(:,11), ex3, wf16(:,35), n3(:,139), t3x16(:,:,35))
  call vert_QA_Z(gZd,ntry, wf8(:,11), ex3, wf16(:,36), n3(:,140), t3x16(:,:,36))
  call vert_AQ_S(gH,ntry, wf8(:,16), ex1, wf16(:,37), n3(:,141), t3x16(:,:,37))
  call vert_QA_V(ntry, ex1, wf8(:,16), wf16(:,38), n3(:,142), t3x16(:,:,38))
  call vert_QA_Z(gZd,ntry, ex1, wf8(:,16), wf16(:,39), n3(:,143), t3x16(:,:,39))
  call vert_AV_Q(ntry, wf8(:,16), ex5, wf16(:,40), n3(:,144), t3x16(:,:,40))
  call vert_QA_V(ntry, wf4(:,10), ex4, wf8(:,17), n3(:,145), t3x8(:,:,9))
  call vert_QA_V(ntry, wf6(:,4), wf6(:,12), wf36(:,37), n3(:,146), t3x36(:,:,33))
  call vert_VQ_A(ntry, wf8(:,17), ex2, wf16(:,41), n3(:,147), t3x16(:,:,41))
  call vert_QA_V(ntry, wf4(:,10), ex3, wf8(:,18), n3(:,148), t3x8(:,:,10))
  call vert_QA_V(ntry, wf6(:,4), wf6(:,2), wf36(:,38), n3(:,149), t3x36(:,:,34))
  call vert_VQ_A(ntry, wf8(:,18), ex2, wf16(:,42), n3(:,150), t3x16(:,:,42))
  call vert_AV_Q(ntry, ex4, wf8(:,18), wf16(:,43), n3(:,151), t3x16(:,:,43))
  call vert_AV_Q(ntry, ex3, wf8(:,17), wf16(:,44), n3(:,152), t3x16(:,:,44))
  call vert_QA_V(ntry, wf18(:,14), ex4, wf36(:,39), n3(:,153), t3x36(:,:,35))
  call vert_QA_V(ntry, wf18(:,16), ex4, wf36(:,40), n3(:,154), t3x36(:,:,36))
  call vert_QA_V(ntry, wf18(:,18), ex4, wf36(:,41), n3(:,155), t3x36(:,:,37))
  call vert_QA_V(ntry, ex2, wf18(:,36), wf36(:,42), n3(:,156), t3x36(:,:,38))
  call vert_QA_V(ntry, ex2, wf18(:,38), wf36(:,43), n3(:,157), t3x36(:,:,39))
  call vert_QA_V(ntry, ex2, wf18(:,40), wf36(:,44), n3(:,158), t3x36(:,:,40))
  call vert_QA_V(ntry, wf18(:,14), ex3, wf36(:,45), n3(:,159), t3x36(:,:,41))
  call vert_QA_V(ntry, wf18(:,16), ex3, wf36(:,46), n3(:,160), t3x36(:,:,42))
  call vert_QA_V(ntry, wf18(:,18), ex3, wf36(:,47), n3(:,161), t3x36(:,:,43))
  call vert_QA_V(ntry, wf4(:,6), ex4, wf8(:,19), n3(:,162), t3x8(:,:,11))
  call vert_QA_V(ntry, wf6(:,10), wf6(:,12), wf36(:,48), n3(:,163), t3x36(:,:,44))
  call vert_VQ_A(ntry, wf8(:,19), ex1, wf16(:,45), n3(:,164), t3x16(:,:,45))
  call vert_QA_V(ntry, wf4(:,6), ex3, wf8(:,20), n3(:,165), t3x8(:,:,12))
  call vert_QA_V(ntry, wf6(:,10), wf6(:,2), wf36(:,49), n3(:,166), t3x36(:,:,45))
  call vert_VQ_A(ntry, wf8(:,20), ex1, wf16(:,46), n3(:,167), t3x16(:,:,46))
  call vert_AV_Q(ntry, ex4, wf8(:,20), wf16(:,47), n3(:,168), t3x16(:,:,47))
  call vert_AV_Q(ntry, ex3, wf8(:,19), wf16(:,48), n3(:,169), t3x16(:,:,48))
  call vert_QA_V(ntry, ex1, wf18(:,36), wf36(:,50), n3(:,170), t3x36(:,:,46))
  call vert_QA_V(ntry, ex1, wf18(:,38), wf36(:,51), n3(:,171), t3x36(:,:,47))
  call vert_QA_V(ntry, ex1, wf18(:,40), wf36(:,52), n3(:,172), t3x36(:,:,48))
  call vert_QA_V(ntry, wf18(:,28), ex3, wf36(:,53), n3(:,173), t3x36(:,:,49))
  call vert_QA_V(ntry, wf18(:,30), ex3, wf36(:,54), n3(:,174), t3x36(:,:,50))
  call vert_QA_V(ntry, wf18(:,32), ex3, wf36(:,55), n3(:,175), t3x36(:,:,51))
  call vert_QA_V(ntry, wf18(:,28), ex4, wf36(:,56), n3(:,176), t3x36(:,:,52))
  call vert_QA_V(ntry, wf18(:,30), ex4, wf36(:,57), n3(:,177), t3x36(:,:,53))
  call vert_QA_V(ntry, wf18(:,32), ex4, wf36(:,58), n3(:,178), t3x36(:,:,54))
  call vert_QA_V(ntry, ex2, wf4(:,12), wf8(:,21), n3(:,179), t3x8(:,:,13))
  call vert_QA_V(ntry, ex1, wf4(:,12), wf8(:,22), n3(:,180), t3x8(:,:,14))
  call vert_VQ_A(ntry, wf8(:,22), ex2, wf16(:,49), n3(:,181), t3x16(:,:,49))
  call vert_VQ_A(ntry, wf8(:,21), ex1, wf16(:,50), n3(:,182), t3x16(:,:,50))
  call vert_AV_Q(ntry, ex4, wf8(:,21), wf16(:,51), n3(:,183), t3x16(:,:,51))
  call vert_AV_Q(ntry, ex4, wf8(:,22), wf16(:,52), n3(:,184), t3x16(:,:,52))
  call vert_QA_V(ntry, ex2, wf4(:,8), wf8(:,23), n3(:,185), t3x8(:,:,15))
  call vert_QA_V(ntry, ex1, wf4(:,8), wf8(:,24), n3(:,186), t3x8(:,:,16))
  call vert_VQ_A(ntry, wf8(:,24), ex2, wf16(:,53), n3(:,187), t3x16(:,:,53))
  call vert_VQ_A(ntry, wf8(:,23), ex1, wf16(:,54), n3(:,188), t3x16(:,:,54))
  call vert_AV_Q(ntry, ex3, wf8(:,23), wf16(:,55), n3(:,189), t3x16(:,:,55))
  call vert_AV_Q(ntry, ex3, wf8(:,24), wf16(:,56), n3(:,190), t3x16(:,:,56))


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
  M2add = M2 / average_factor_ppvvvj_bbbxbxawwx_1

  do k = 0, 30-1
    M2(k) = M2add(extcomb_perm_ppvvvj_bbbxbxawwx_1(k))
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

    call cont_QA(nsync, wf36(:,1), wf8(:,2), A(:,1), n3(:,191), t3x288(:,:,1), nhel, den(5))
    call cont_QA(nsync, wf8(:,2), wf36(:,2), A(:,2), n3(:,192), t3x288(:,:,2), nhel, den(7))
    call cont_QA(nsync, wf8(:,3), wf36(:,4), A(:,3), n3(:,193), t3x288(:,:,3), nhel, den(10))
    call cont_QA(nsync, wf8(:,3), wf36(:,6), A(:,4), n3(:,194), t3x288(:,:,4), nhel, den(12))
    call cont_QA(nsync, wf36(:,1), wf8(:,5), A(:,5), n3(:,195), t3x288(:,:,5), nhel, den(15))
    call cont_QA(nsync, wf36(:,2), wf8(:,5), A(:,6), n3(:,196), t3x288(:,:,6), nhel, den(16))
    call cont_QA(nsync, wf8(:,6), wf36(:,8), A(:,7), n3(:,197), t3x288(:,:,7), nhel, den(19))
    call cont_QA(nsync, wf8(:,6), wf36(:,10), A(:,8), n3(:,198), t3x288(:,:,8), nhel, den(21))
    call cont_QA(nsync, wf36(:,11), wf8(:,8), A(:,9), n3(:,199), t3x288(:,:,9), nhel, den(25))
    call cont_QA(nsync, wf8(:,8), wf36(:,12), A(:,10), n3(:,200), t3x288(:,:,10), nhel, den(26))
    call cont_QA(nsync, wf36(:,4), wf8(:,9), A(:,11), n3(:,201), t3x288(:,:,11), nhel, den(27))
    call cont_QA(nsync, wf36(:,6), wf8(:,9), A(:,12), n3(:,202), t3x288(:,:,12), nhel, den(28))
    call cont_QA(nsync, wf36(:,11), wf8(:,11), A(:,13), n3(:,203), t3x288(:,:,13), nhel, den(31))
    call cont_QA(nsync, wf36(:,12), wf8(:,11), A(:,14), n3(:,204), t3x288(:,:,14), nhel, den(32))
    call cont_QA(nsync, wf36(:,8), wf8(:,12), A(:,15), n3(:,205), t3x288(:,:,15), nhel, den(33))
    call cont_QA(nsync, wf36(:,10), wf8(:,12), A(:,16), n3(:,206), t3x288(:,:,16), nhel, den(34))
    call cont_QA(nsync, wf24(:,1), wf12(:,2), A(:,17), n3(:,207), t3x288(:,:,17), nhel, den(40))
    call cont_QA(nsync, wf16(:,1), wf18(:,4), A(:,18), n3(:,208), t3x288(:,:,18), nhel, den(44))
    call cont_QA(nsync, wf36(:,13), wf8(:,13), A(:,19), n3(:,209), t3x288(:,:,19), nhel, den(49))
    call cont_QA(nsync, wf8(:,13), wf36(:,14), A(:,20), n3(:,210), t3x288(:,:,20), nhel, den(52))
    call cont_QA(nsync, wf8(:,13), wf36(:,15), A(:,21), n3(:,211), t3x288(:,:,21), nhel, den(55))
    call cont_QA(nsync, wf16(:,1), wf18(:,6), A(:,22), n3(:,212), t3x288(:,:,22), nhel, den(57))
    call cont_QA(nsync, wf16(:,1), wf18(:,8), A(:,23), n3(:,213), t3x288(:,:,23), nhel, den(59))
    call cont_QA(nsync, wf16(:,1), wf18(:,10), A(:,24), n3(:,214), t3x288(:,:,24), nhel, den(61))
    call cont_QA(nsync, wf12(:,2), wf24(:,2), A(:,25), n3(:,215), t3x288(:,:,25), nhel, den(62))
    call cont_QA(nsync, wf16(:,2), wf18(:,12), A(:,26), n3(:,216), t3x288(:,:,26), nhel, den(68))
    call cont_QA(nsync, wf24(:,3), wf12(:,4), A(:,27), n3(:,217), t3x288(:,:,27), nhel, den(72))
    call cont_QA(nsync, wf8(:,2), wf36(:,16), A(:,28), n3(:,218), t3x288(:,:,28), nhel, den(74))
    call cont_QA(nsync, wf8(:,2), wf36(:,17), A(:,29), n3(:,219), t3x288(:,:,29), nhel, den(76))
    call cont_QA(nsync, wf8(:,2), wf36(:,18), A(:,30), n3(:,220), t3x288(:,:,30), nhel, den(78))
    call cont_QA(nsync, wf16(:,2), wf18(:,14), A(:,31), n3(:,221), t3x288(:,:,31), nhel, den(80))
    call cont_QA(nsync, wf16(:,2), wf18(:,16), A(:,32), n3(:,222), t3x288(:,:,32), nhel, den(82))
    call cont_QA(nsync, wf16(:,2), wf18(:,18), A(:,33), n3(:,223), t3x288(:,:,33), nhel, den(84))
    call cont_QA(nsync, wf12(:,4), wf24(:,4), A(:,34), n3(:,224), t3x288(:,:,34), nhel, den(85))
    call cont_QA(nsync, wf24(:,1), wf12(:,6), A(:,35), n3(:,225), t3x288(:,:,35), nhel, den(87))
    call cont_QA(nsync, wf24(:,3), wf12(:,8), A(:,36), n3(:,226), t3x288(:,:,36), nhel, den(89))
    call cont_QA(nsync, wf8(:,2), wf36(:,19), A(:,37), n3(:,227), t3x288(:,:,37), nhel, den(92))
    call cont_QA(nsync, wf24(:,1), wf12(:,10), A(:,38), n3(:,228), t3x288(:,:,38), nhel, den(94))
    call cont_QA(nsync, wf18(:,4), wf16(:,3), A(:,39), n3(:,229), t3x288(:,:,39), nhel, den(95))
    call cont_QA(nsync, wf24(:,4), wf12(:,8), A(:,40), n3(:,230), t3x288(:,:,40), nhel, den(96))
    call cont_QA(nsync, wf8(:,13), wf36(:,20), A(:,41), n3(:,231), t3x288(:,:,41), nhel, den(99))
    call cont_QA(nsync, wf24(:,3), wf12(:,12), A(:,42), n3(:,232), t3x288(:,:,42), nhel, den(101))
    call cont_SS(nsync, wf18(:,19), wf16(:,4), A(:,43), n3(:,233), t3x288(:,:,43), nhel, den(104))
    call cont_VV(nsync, wf18(:,20), wf16(:,5), A(:,44), n3(:,234), t3x288(:,:,44), nhel, den(106))
    call cont_VV(nsync, wf18(:,21), wf16(:,6), A(:,45), n3(:,235), t3x288(:,:,45), nhel, den(108))
    call cont_QA(nsync, wf24(:,4), wf12(:,12), A(:,46), n3(:,236), t3x288(:,:,46), nhel, den(109))
    call cont_SS(nsync, wf18(:,19), wf16(:,7), A(:,47), n3(:,237), t3x288(:,:,47), nhel, den(110))
    call cont_VV(nsync, wf18(:,20), wf16(:,8), A(:,48), n3(:,238), t3x288(:,:,48), nhel, den(111))
    call cont_VV(nsync, wf18(:,21), wf16(:,9), A(:,49), n3(:,239), t3x288(:,:,49), nhel, den(112))
    call cont_QA(nsync, wf24(:,2), wf12(:,6), A(:,50), n3(:,240), t3x288(:,:,50), nhel, den(113))
    call cont_QA(nsync, wf18(:,12), wf16(:,10), A(:,51), n3(:,241), t3x288(:,:,51), nhel, den(114))
    call cont_SS(nsync, wf16(:,4), wf18(:,22), A(:,52), n3(:,242), t3x288(:,:,52), nhel, den(116))
    call cont_VV(nsync, wf16(:,5), wf18(:,23), A(:,53), n3(:,243), t3x288(:,:,53), nhel, den(118))
    call cont_VV(nsync, wf16(:,6), wf18(:,24), A(:,54), n3(:,244), t3x288(:,:,54), nhel, den(120))
    call cont_SS(nsync, wf16(:,7), wf18(:,22), A(:,55), n3(:,245), t3x288(:,:,55), nhel, den(121))
    call cont_VV(nsync, wf16(:,8), wf18(:,23), A(:,56), n3(:,246), t3x288(:,:,56), nhel, den(122))
    call cont_VV(nsync, wf16(:,9), wf18(:,24), A(:,57), n3(:,247), t3x288(:,:,57), nhel, den(123))
    call cont_QA(nsync, wf8(:,13), wf36(:,21), A(:,58), n3(:,248), t3x288(:,:,58), nhel, den(124))
    call cont_QA(nsync, wf18(:,6), wf16(:,3), A(:,59), n3(:,249), t3x288(:,:,59), nhel, den(125))
    call cont_QA(nsync, wf18(:,8), wf16(:,3), A(:,60), n3(:,250), t3x288(:,:,60), nhel, den(126))
    call cont_QA(nsync, wf18(:,10), wf16(:,3), A(:,61), n3(:,251), t3x288(:,:,61), nhel, den(127))
    call cont_QA(nsync, wf8(:,13), wf36(:,22), A(:,62), n3(:,252), t3x288(:,:,62), nhel, den(128))
    call cont_QA(nsync, wf8(:,13), wf36(:,23), A(:,63), n3(:,253), t3x288(:,:,63), nhel, den(129))
    call cont_QA(nsync, wf8(:,13), wf36(:,24), A(:,64), n3(:,254), t3x288(:,:,64), nhel, den(130))
    call cont_QA(nsync, wf24(:,5), wf12(:,14), A(:,65), n3(:,255), t3x288(:,:,65), nhel, den(135))
    call cont_QA(nsync, wf18(:,4), wf16(:,11), A(:,66), n3(:,256), t3x288(:,:,66), nhel, den(137))
    call cont_QA(nsync, wf36(:,25), wf8(:,14), A(:,67), n3(:,257), t3x288(:,:,67), nhel, den(141))
    call cont_QA(nsync, wf8(:,14), wf36(:,26), A(:,68), n3(:,258), t3x288(:,:,68), nhel, den(143))
    call cont_QA(nsync, wf8(:,14), wf36(:,27), A(:,69), n3(:,259), t3x288(:,:,69), nhel, den(145))
    call cont_QA(nsync, wf18(:,6), wf16(:,11), A(:,70), n3(:,260), t3x288(:,:,70), nhel, den(146))
    call cont_QA(nsync, wf18(:,8), wf16(:,11), A(:,71), n3(:,261), t3x288(:,:,71), nhel, den(147))
    call cont_QA(nsync, wf18(:,10), wf16(:,11), A(:,72), n3(:,262), t3x288(:,:,72), nhel, den(148))
    call cont_QA(nsync, wf12(:,14), wf24(:,6), A(:,73), n3(:,263), t3x288(:,:,73), nhel, den(149))
    call cont_QA(nsync, wf16(:,12), wf18(:,26), A(:,74), n3(:,264), t3x288(:,:,74), nhel, den(154))
    call cont_QA(nsync, wf12(:,4), wf24(:,7), A(:,75), n3(:,265), t3x288(:,:,75), nhel, den(156))
    call cont_QA(nsync, wf8(:,5), wf36(:,16), A(:,76), n3(:,266), t3x288(:,:,76), nhel, den(157))
    call cont_QA(nsync, wf8(:,5), wf36(:,17), A(:,77), n3(:,267), t3x288(:,:,77), nhel, den(158))
    call cont_QA(nsync, wf8(:,5), wf36(:,18), A(:,78), n3(:,268), t3x288(:,:,78), nhel, den(159))
    call cont_QA(nsync, wf16(:,12), wf18(:,28), A(:,79), n3(:,269), t3x288(:,:,79), nhel, den(161))
    call cont_QA(nsync, wf16(:,12), wf18(:,30), A(:,80), n3(:,270), t3x288(:,:,80), nhel, den(163))
    call cont_QA(nsync, wf16(:,12), wf18(:,32), A(:,81), n3(:,271), t3x288(:,:,81), nhel, den(165))
    call cont_QA(nsync, wf12(:,4), wf24(:,8), A(:,82), n3(:,272), t3x288(:,:,82), nhel, den(166))
    call cont_QA(nsync, wf24(:,5), wf12(:,16), A(:,83), n3(:,273), t3x288(:,:,83), nhel, den(168))
    call cont_QA(nsync, wf12(:,8), wf24(:,7), A(:,84), n3(:,274), t3x288(:,:,84), nhel, den(169))
    call cont_QA(nsync, wf8(:,5), wf36(:,19), A(:,85), n3(:,275), t3x288(:,:,85), nhel, den(170))
    call cont_QA(nsync, wf24(:,5), wf12(:,18), A(:,86), n3(:,276), t3x288(:,:,86), nhel, den(172))
    call cont_QA(nsync, wf18(:,4), wf16(:,13), A(:,87), n3(:,277), t3x288(:,:,87), nhel, den(173))
    call cont_QA(nsync, wf12(:,8), wf24(:,8), A(:,88), n3(:,278), t3x288(:,:,88), nhel, den(174))
    call cont_QA(nsync, wf8(:,14), wf36(:,28), A(:,89), n3(:,279), t3x288(:,:,89), nhel, den(176))
    call cont_QA(nsync, wf12(:,12), wf24(:,7), A(:,90), n3(:,280), t3x288(:,:,90), nhel, den(177))
    call cont_SS(nsync, wf18(:,19), wf16(:,14), A(:,91), n3(:,281), t3x288(:,:,91), nhel, den(178))
    call cont_VV(nsync, wf18(:,20), wf16(:,15), A(:,92), n3(:,282), t3x288(:,:,92), nhel, den(179))
    call cont_VV(nsync, wf18(:,21), wf16(:,16), A(:,93), n3(:,283), t3x288(:,:,93), nhel, den(180))
    call cont_QA(nsync, wf12(:,12), wf24(:,8), A(:,94), n3(:,284), t3x288(:,:,94), nhel, den(181))
    call cont_SS(nsync, wf18(:,19), wf16(:,17), A(:,95), n3(:,285), t3x288(:,:,95), nhel, den(182))
    call cont_VV(nsync, wf18(:,20), wf16(:,18), A(:,96), n3(:,286), t3x288(:,:,96), nhel, den(183))
    call cont_VV(nsync, wf18(:,21), wf16(:,19), A(:,97), n3(:,287), t3x288(:,:,97), nhel, den(184))
    call cont_QA(nsync, wf24(:,6), wf12(:,16), A(:,98), n3(:,288), t3x288(:,:,98), nhel, den(185))
    call cont_QA(nsync, wf18(:,26), wf16(:,20), A(:,99), n3(:,289), t3x288(:,:,99), nhel, den(186))
    call cont_SS(nsync, wf18(:,22), wf16(:,14), A(:,100), n3(:,290), t3x288(:,:,100), nhel, den(187))
    call cont_VV(nsync, wf18(:,23), wf16(:,15), A(:,101), n3(:,291), t3x288(:,:,101), nhel, den(188))
    call cont_VV(nsync, wf18(:,24), wf16(:,16), A(:,102), n3(:,292), t3x288(:,:,102), nhel, den(189))
    call cont_SS(nsync, wf18(:,22), wf16(:,17), A(:,103), n3(:,293), t3x288(:,:,103), nhel, den(190))
    call cont_VV(nsync, wf18(:,23), wf16(:,18), A(:,104), n3(:,294), t3x288(:,:,104), nhel, den(191))
    call cont_VV(nsync, wf18(:,24), wf16(:,19), A(:,105), n3(:,295), t3x288(:,:,105), nhel, den(192))
    call cont_QA(nsync, wf8(:,14), wf36(:,29), A(:,106), n3(:,296), t3x288(:,:,106), nhel, den(193))
    call cont_QA(nsync, wf18(:,6), wf16(:,13), A(:,107), n3(:,297), t3x288(:,:,107), nhel, den(194))
    call cont_QA(nsync, wf18(:,8), wf16(:,13), A(:,108), n3(:,298), t3x288(:,:,108), nhel, den(195))
    call cont_QA(nsync, wf18(:,10), wf16(:,13), A(:,109), n3(:,299), t3x288(:,:,109), nhel, den(196))
    call cont_QA(nsync, wf8(:,14), wf36(:,30), A(:,110), n3(:,300), t3x288(:,:,110), nhel, den(197))
    call cont_QA(nsync, wf8(:,14), wf36(:,31), A(:,111), n3(:,301), t3x288(:,:,111), nhel, den(198))
    call cont_QA(nsync, wf8(:,14), wf36(:,32), A(:,112), n3(:,302), t3x288(:,:,112), nhel, den(199))
    call cont_QA(nsync, wf12(:,2), wf24(:,9), A(:,113), n3(:,303), t3x288(:,:,113), nhel, den(202))
    call cont_QA(nsync, wf16(:,21), wf18(:,34), A(:,114), n3(:,304), t3x288(:,:,114), nhel, den(206))
    call cont_QA(nsync, wf36(:,13), wf8(:,15), A(:,115), n3(:,305), t3x288(:,:,115), nhel, den(208))
    call cont_QA(nsync, wf36(:,14), wf8(:,15), A(:,116), n3(:,306), t3x288(:,:,116), nhel, den(209))
    call cont_QA(nsync, wf36(:,15), wf8(:,15), A(:,117), n3(:,307), t3x288(:,:,117), nhel, den(210))
    call cont_QA(nsync, wf16(:,21), wf18(:,36), A(:,118), n3(:,308), t3x288(:,:,118), nhel, den(212))
    call cont_QA(nsync, wf16(:,21), wf18(:,38), A(:,119), n3(:,309), t3x288(:,:,119), nhel, den(214))
    call cont_QA(nsync, wf16(:,21), wf18(:,40), A(:,120), n3(:,310), t3x288(:,:,120), nhel, den(216))
    call cont_QA(nsync, wf12(:,2), wf24(:,10), A(:,121), n3(:,311), t3x288(:,:,121), nhel, den(217))
    call cont_QA(nsync, wf18(:,12), wf16(:,22), A(:,122), n3(:,312), t3x288(:,:,122), nhel, den(220))
    call cont_QA(nsync, wf24(:,11), wf12(:,20), A(:,123), n3(:,313), t3x288(:,:,123), nhel, den(224))
    call cont_QA(nsync, wf8(:,8), wf36(:,33), A(:,124), n3(:,314), t3x288(:,:,124), nhel, den(226))
    call cont_QA(nsync, wf8(:,8), wf36(:,34), A(:,125), n3(:,315), t3x288(:,:,125), nhel, den(228))
    call cont_QA(nsync, wf8(:,8), wf36(:,35), A(:,126), n3(:,316), t3x288(:,:,126), nhel, den(230))
    call cont_QA(nsync, wf18(:,14), wf16(:,22), A(:,127), n3(:,317), t3x288(:,:,127), nhel, den(231))
    call cont_QA(nsync, wf18(:,16), wf16(:,22), A(:,128), n3(:,318), t3x288(:,:,128), nhel, den(232))
    call cont_QA(nsync, wf18(:,18), wf16(:,22), A(:,129), n3(:,319), t3x288(:,:,129), nhel, den(233))
    call cont_QA(nsync, wf12(:,20), wf24(:,12), A(:,130), n3(:,320), t3x288(:,:,130), nhel, den(234))
    call cont_QA(nsync, wf12(:,6), wf24(:,9), A(:,131), n3(:,321), t3x288(:,:,131), nhel, den(235))
    call cont_QA(nsync, wf24(:,11), wf12(:,22), A(:,132), n3(:,322), t3x288(:,:,132), nhel, den(237))
    call cont_QA(nsync, wf8(:,8), wf36(:,36), A(:,133), n3(:,323), t3x288(:,:,133), nhel, den(239))
    call cont_QA(nsync, wf12(:,10), wf24(:,9), A(:,134), n3(:,324), t3x288(:,:,134), nhel, den(240))
    call cont_QA(nsync, wf18(:,34), wf16(:,23), A(:,135), n3(:,325), t3x288(:,:,135), nhel, den(241))
    call cont_QA(nsync, wf24(:,12), wf12(:,22), A(:,136), n3(:,326), t3x288(:,:,136), nhel, den(242))
    call cont_QA(nsync, wf36(:,20), wf8(:,15), A(:,137), n3(:,327), t3x288(:,:,137), nhel, den(243))
    call cont_QA(nsync, wf24(:,11), wf12(:,24), A(:,138), n3(:,328), t3x288(:,:,138), nhel, den(245))
    call cont_SS(nsync, wf18(:,19), wf16(:,24), A(:,139), n3(:,329), t3x288(:,:,139), nhel, den(246))
    call cont_VV(nsync, wf18(:,20), wf16(:,25), A(:,140), n3(:,330), t3x288(:,:,140), nhel, den(247))
    call cont_VV(nsync, wf18(:,21), wf16(:,26), A(:,141), n3(:,331), t3x288(:,:,141), nhel, den(248))
    call cont_QA(nsync, wf24(:,12), wf12(:,24), A(:,142), n3(:,332), t3x288(:,:,142), nhel, den(249))
    call cont_SS(nsync, wf18(:,19), wf16(:,27), A(:,143), n3(:,333), t3x288(:,:,143), nhel, den(250))
    call cont_VV(nsync, wf18(:,20), wf16(:,28), A(:,144), n3(:,334), t3x288(:,:,144), nhel, den(251))
    call cont_VV(nsync, wf18(:,21), wf16(:,29), A(:,145), n3(:,335), t3x288(:,:,145), nhel, den(252))
    call cont_QA(nsync, wf12(:,6), wf24(:,10), A(:,146), n3(:,336), t3x288(:,:,146), nhel, den(253))
    call cont_QA(nsync, wf18(:,12), wf16(:,30), A(:,147), n3(:,337), t3x288(:,:,147), nhel, den(254))
    call cont_SS(nsync, wf18(:,22), wf16(:,24), A(:,148), n3(:,338), t3x288(:,:,148), nhel, den(255))
    call cont_VV(nsync, wf18(:,23), wf16(:,25), A(:,149), n3(:,339), t3x288(:,:,149), nhel, den(256))
    call cont_VV(nsync, wf18(:,24), wf16(:,26), A(:,150), n3(:,340), t3x288(:,:,150), nhel, den(257))
    call cont_SS(nsync, wf18(:,22), wf16(:,27), A(:,151), n3(:,341), t3x288(:,:,151), nhel, den(258))
    call cont_VV(nsync, wf18(:,23), wf16(:,28), A(:,152), n3(:,342), t3x288(:,:,152), nhel, den(259))
    call cont_VV(nsync, wf18(:,24), wf16(:,29), A(:,153), n3(:,343), t3x288(:,:,153), nhel, den(260))
    call cont_QA(nsync, wf36(:,21), wf8(:,15), A(:,154), n3(:,344), t3x288(:,:,154), nhel, den(261))
    call cont_QA(nsync, wf18(:,36), wf16(:,23), A(:,155), n3(:,345), t3x288(:,:,155), nhel, den(262))
    call cont_QA(nsync, wf18(:,38), wf16(:,23), A(:,156), n3(:,346), t3x288(:,:,156), nhel, den(263))
    call cont_QA(nsync, wf18(:,40), wf16(:,23), A(:,157), n3(:,347), t3x288(:,:,157), nhel, den(264))
    call cont_QA(nsync, wf36(:,22), wf8(:,15), A(:,158), n3(:,348), t3x288(:,:,158), nhel, den(265))
    call cont_QA(nsync, wf36(:,23), wf8(:,15), A(:,159), n3(:,349), t3x288(:,:,159), nhel, den(266))
    call cont_QA(nsync, wf36(:,24), wf8(:,15), A(:,160), n3(:,350), t3x288(:,:,160), nhel, den(267))
    call cont_QA(nsync, wf12(:,14), wf24(:,13), A(:,161), n3(:,351), t3x288(:,:,161), nhel, den(269))
    call cont_QA(nsync, wf18(:,34), wf16(:,31), A(:,162), n3(:,352), t3x288(:,:,162), nhel, den(271))
    call cont_QA(nsync, wf36(:,25), wf8(:,16), A(:,163), n3(:,353), t3x288(:,:,163), nhel, den(273))
    call cont_QA(nsync, wf36(:,26), wf8(:,16), A(:,164), n3(:,354), t3x288(:,:,164), nhel, den(274))
    call cont_QA(nsync, wf36(:,27), wf8(:,16), A(:,165), n3(:,355), t3x288(:,:,165), nhel, den(275))
    call cont_QA(nsync, wf18(:,36), wf16(:,31), A(:,166), n3(:,356), t3x288(:,:,166), nhel, den(276))
    call cont_QA(nsync, wf18(:,38), wf16(:,31), A(:,167), n3(:,357), t3x288(:,:,167), nhel, den(277))
    call cont_QA(nsync, wf18(:,40), wf16(:,31), A(:,168), n3(:,358), t3x288(:,:,168), nhel, den(278))
    call cont_QA(nsync, wf12(:,14), wf24(:,14), A(:,169), n3(:,359), t3x288(:,:,169), nhel, den(279))
    call cont_QA(nsync, wf18(:,26), wf16(:,32), A(:,170), n3(:,360), t3x288(:,:,170), nhel, den(281))
    call cont_QA(nsync, wf12(:,20), wf24(:,15), A(:,171), n3(:,361), t3x288(:,:,171), nhel, den(283))
    call cont_QA(nsync, wf8(:,11), wf36(:,33), A(:,172), n3(:,362), t3x288(:,:,172), nhel, den(284))
    call cont_QA(nsync, wf8(:,11), wf36(:,34), A(:,173), n3(:,363), t3x288(:,:,173), nhel, den(285))
    call cont_QA(nsync, wf8(:,11), wf36(:,35), A(:,174), n3(:,364), t3x288(:,:,174), nhel, den(286))
    call cont_QA(nsync, wf18(:,28), wf16(:,32), A(:,175), n3(:,365), t3x288(:,:,175), nhel, den(287))
    call cont_QA(nsync, wf18(:,30), wf16(:,32), A(:,176), n3(:,366), t3x288(:,:,176), nhel, den(288))
    call cont_QA(nsync, wf18(:,32), wf16(:,32), A(:,177), n3(:,367), t3x288(:,:,177), nhel, den(289))
    call cont_QA(nsync, wf12(:,20), wf24(:,16), A(:,178), n3(:,368), t3x288(:,:,178), nhel, den(290))
    call cont_QA(nsync, wf12(:,16), wf24(:,13), A(:,179), n3(:,369), t3x288(:,:,179), nhel, den(291))
    call cont_QA(nsync, wf12(:,22), wf24(:,15), A(:,180), n3(:,370), t3x288(:,:,180), nhel, den(292))
    call cont_QA(nsync, wf8(:,11), wf36(:,36), A(:,181), n3(:,371), t3x288(:,:,181), nhel, den(293))
    call cont_QA(nsync, wf12(:,18), wf24(:,13), A(:,182), n3(:,372), t3x288(:,:,182), nhel, den(294))
    call cont_QA(nsync, wf18(:,34), wf16(:,33), A(:,183), n3(:,373), t3x288(:,:,183), nhel, den(295))
    call cont_QA(nsync, wf12(:,22), wf24(:,16), A(:,184), n3(:,374), t3x288(:,:,184), nhel, den(296))
    call cont_QA(nsync, wf36(:,28), wf8(:,16), A(:,185), n3(:,375), t3x288(:,:,185), nhel, den(297))
    call cont_QA(nsync, wf12(:,24), wf24(:,15), A(:,186), n3(:,376), t3x288(:,:,186), nhel, den(298))
    call cont_SS(nsync, wf18(:,19), wf16(:,34), A(:,187), n3(:,377), t3x288(:,:,187), nhel, den(299))
    call cont_VV(nsync, wf18(:,20), wf16(:,35), A(:,188), n3(:,378), t3x288(:,:,188), nhel, den(300))
    call cont_VV(nsync, wf18(:,21), wf16(:,36), A(:,189), n3(:,379), t3x288(:,:,189), nhel, den(301))
    call cont_QA(nsync, wf12(:,24), wf24(:,16), A(:,190), n3(:,380), t3x288(:,:,190), nhel, den(302))
    call cont_SS(nsync, wf18(:,19), wf16(:,37), A(:,191), n3(:,381), t3x288(:,:,191), nhel, den(303))
    call cont_VV(nsync, wf18(:,20), wf16(:,38), A(:,192), n3(:,382), t3x288(:,:,192), nhel, den(304))
    call cont_VV(nsync, wf18(:,21), wf16(:,39), A(:,193), n3(:,383), t3x288(:,:,193), nhel, den(305))
    call cont_QA(nsync, wf12(:,16), wf24(:,14), A(:,194), n3(:,384), t3x288(:,:,194), nhel, den(306))
    call cont_QA(nsync, wf18(:,26), wf16(:,40), A(:,195), n3(:,385), t3x288(:,:,195), nhel, den(307))
    call cont_SS(nsync, wf18(:,22), wf16(:,34), A(:,196), n3(:,386), t3x288(:,:,196), nhel, den(308))
    call cont_VV(nsync, wf18(:,23), wf16(:,35), A(:,197), n3(:,387), t3x288(:,:,197), nhel, den(309))
    call cont_VV(nsync, wf18(:,24), wf16(:,36), A(:,198), n3(:,388), t3x288(:,:,198), nhel, den(310))
    call cont_SS(nsync, wf18(:,22), wf16(:,37), A(:,199), n3(:,389), t3x288(:,:,199), nhel, den(311))
    call cont_VV(nsync, wf18(:,23), wf16(:,38), A(:,200), n3(:,390), t3x288(:,:,200), nhel, den(312))
    call cont_VV(nsync, wf18(:,24), wf16(:,39), A(:,201), n3(:,391), t3x288(:,:,201), nhel, den(313))
    call cont_QA(nsync, wf36(:,29), wf8(:,16), A(:,202), n3(:,392), t3x288(:,:,202), nhel, den(314))
    call cont_QA(nsync, wf18(:,36), wf16(:,33), A(:,203), n3(:,393), t3x288(:,:,203), nhel, den(315))
    call cont_QA(nsync, wf18(:,38), wf16(:,33), A(:,204), n3(:,394), t3x288(:,:,204), nhel, den(316))
    call cont_QA(nsync, wf18(:,40), wf16(:,33), A(:,205), n3(:,395), t3x288(:,:,205), nhel, den(317))
    call cont_QA(nsync, wf36(:,30), wf8(:,16), A(:,206), n3(:,396), t3x288(:,:,206), nhel, den(318))
    call cont_QA(nsync, wf36(:,31), wf8(:,16), A(:,207), n3(:,397), t3x288(:,:,207), nhel, den(319))
    call cont_QA(nsync, wf36(:,32), wf8(:,16), A(:,208), n3(:,398), t3x288(:,:,208), nhel, den(320))
    call cont_VV(nsync, wf8(:,17), wf36(:,37), A(:,209), n3(:,399), t3x288(:,:,209), nhel, den(324))
    call cont_QA(nsync, wf18(:,34), wf16(:,41), A(:,210), n3(:,400), t3x288(:,:,210), nhel, den(325))
    call cont_VV(nsync, wf8(:,18), wf36(:,38), A(:,211), n3(:,401), t3x288(:,:,211), nhel, den(329))
    call cont_QA(nsync, wf18(:,4), wf16(:,42), A(:,212), n3(:,402), t3x288(:,:,212), nhel, den(330))
    call cont_QA(nsync, wf18(:,12), wf16(:,43), A(:,213), n3(:,403), t3x288(:,:,213), nhel, den(331))
    call cont_QA(nsync, wf18(:,12), wf16(:,44), A(:,214), n3(:,404), t3x288(:,:,214), nhel, den(332))
    call cont_QA(nsync, wf18(:,6), wf16(:,42), A(:,215), n3(:,405), t3x288(:,:,215), nhel, den(333))
    call cont_QA(nsync, wf18(:,8), wf16(:,42), A(:,216), n3(:,406), t3x288(:,:,216), nhel, den(334))
    call cont_QA(nsync, wf18(:,10), wf16(:,42), A(:,217), n3(:,407), t3x288(:,:,217), nhel, den(335))
    call cont_VV(nsync, wf8(:,18), wf36(:,39), A(:,218), n3(:,408), t3x288(:,:,218), nhel, den(336))
    call cont_VV(nsync, wf8(:,18), wf36(:,40), A(:,219), n3(:,409), t3x288(:,:,219), nhel, den(337))
    call cont_VV(nsync, wf8(:,18), wf36(:,41), A(:,220), n3(:,410), t3x288(:,:,220), nhel, den(338))
    call cont_VV(nsync, wf8(:,17), wf36(:,42), A(:,221), n3(:,411), t3x288(:,:,221), nhel, den(339))
    call cont_VV(nsync, wf8(:,17), wf36(:,43), A(:,222), n3(:,412), t3x288(:,:,222), nhel, den(340))
    call cont_VV(nsync, wf8(:,17), wf36(:,44), A(:,223), n3(:,413), t3x288(:,:,223), nhel, den(341))
    call cont_VV(nsync, wf8(:,17), wf36(:,45), A(:,224), n3(:,414), t3x288(:,:,224), nhel, den(342))
    call cont_VV(nsync, wf8(:,17), wf36(:,46), A(:,225), n3(:,415), t3x288(:,:,225), nhel, den(343))
    call cont_VV(nsync, wf8(:,17), wf36(:,47), A(:,226), n3(:,416), t3x288(:,:,226), nhel, den(344))
    call cont_VV(nsync, wf8(:,19), wf36(:,48), A(:,227), n3(:,417), t3x288(:,:,227), nhel, den(348))
    call cont_QA(nsync, wf18(:,34), wf16(:,45), A(:,228), n3(:,418), t3x288(:,:,228), nhel, den(349))
    call cont_VV(nsync, wf8(:,20), wf36(:,49), A(:,229), n3(:,419), t3x288(:,:,229), nhel, den(353))
    call cont_QA(nsync, wf18(:,4), wf16(:,46), A(:,230), n3(:,420), t3x288(:,:,230), nhel, den(354))
    call cont_QA(nsync, wf18(:,26), wf16(:,47), A(:,231), n3(:,421), t3x288(:,:,231), nhel, den(355))
    call cont_QA(nsync, wf18(:,26), wf16(:,48), A(:,232), n3(:,422), t3x288(:,:,232), nhel, den(356))
    call cont_QA(nsync, wf18(:,6), wf16(:,46), A(:,233), n3(:,423), t3x288(:,:,233), nhel, den(357))
    call cont_QA(nsync, wf18(:,8), wf16(:,46), A(:,234), n3(:,424), t3x288(:,:,234), nhel, den(358))
    call cont_QA(nsync, wf18(:,10), wf16(:,46), A(:,235), n3(:,425), t3x288(:,:,235), nhel, den(359))
    call cont_VV(nsync, wf8(:,19), wf36(:,50), A(:,236), n3(:,426), t3x288(:,:,236), nhel, den(360))
    call cont_VV(nsync, wf8(:,19), wf36(:,51), A(:,237), n3(:,427), t3x288(:,:,237), nhel, den(361))
    call cont_VV(nsync, wf8(:,19), wf36(:,52), A(:,238), n3(:,428), t3x288(:,:,238), nhel, den(362))
    call cont_VV(nsync, wf8(:,19), wf36(:,53), A(:,239), n3(:,429), t3x288(:,:,239), nhel, den(363))
    call cont_VV(nsync, wf8(:,19), wf36(:,54), A(:,240), n3(:,430), t3x288(:,:,240), nhel, den(364))
    call cont_VV(nsync, wf8(:,19), wf36(:,55), A(:,241), n3(:,431), t3x288(:,:,241), nhel, den(365))
    call cont_VV(nsync, wf8(:,20), wf36(:,56), A(:,242), n3(:,432), t3x288(:,:,242), nhel, den(366))
    call cont_VV(nsync, wf8(:,20), wf36(:,57), A(:,243), n3(:,433), t3x288(:,:,243), nhel, den(367))
    call cont_VV(nsync, wf8(:,20), wf36(:,58), A(:,244), n3(:,434), t3x288(:,:,244), nhel, den(368))
    call cont_VV(nsync, wf36(:,49), wf8(:,21), A(:,245), n3(:,435), t3x288(:,:,245), nhel, den(370))
    call cont_VV(nsync, wf36(:,38), wf8(:,22), A(:,246), n3(:,436), t3x288(:,:,246), nhel, den(372))
    call cont_QA(nsync, wf18(:,4), wf16(:,49), A(:,247), n3(:,437), t3x288(:,:,247), nhel, den(373))
    call cont_QA(nsync, wf18(:,4), wf16(:,50), A(:,248), n3(:,438), t3x288(:,:,248), nhel, den(374))
    call cont_QA(nsync, wf18(:,26), wf16(:,51), A(:,249), n3(:,439), t3x288(:,:,249), nhel, den(375))
    call cont_QA(nsync, wf18(:,12), wf16(:,52), A(:,250), n3(:,440), t3x288(:,:,250), nhel, den(376))
    call cont_QA(nsync, wf18(:,6), wf16(:,49), A(:,251), n3(:,441), t3x288(:,:,251), nhel, den(377))
    call cont_QA(nsync, wf18(:,8), wf16(:,49), A(:,252), n3(:,442), t3x288(:,:,252), nhel, den(378))
    call cont_QA(nsync, wf18(:,10), wf16(:,49), A(:,253), n3(:,443), t3x288(:,:,253), nhel, den(379))
    call cont_QA(nsync, wf18(:,6), wf16(:,50), A(:,254), n3(:,444), t3x288(:,:,254), nhel, den(380))
    call cont_QA(nsync, wf18(:,8), wf16(:,50), A(:,255), n3(:,445), t3x288(:,:,255), nhel, den(381))
    call cont_QA(nsync, wf18(:,10), wf16(:,50), A(:,256), n3(:,446), t3x288(:,:,256), nhel, den(382))
    call cont_QA(nsync, wf18(:,14), wf16(:,52), A(:,257), n3(:,447), t3x288(:,:,257), nhel, den(383))
    call cont_QA(nsync, wf18(:,16), wf16(:,52), A(:,258), n3(:,448), t3x288(:,:,258), nhel, den(384))
    call cont_QA(nsync, wf18(:,18), wf16(:,52), A(:,259), n3(:,449), t3x288(:,:,259), nhel, den(385))
    call cont_VV(nsync, wf36(:,56), wf8(:,21), A(:,260), n3(:,450), t3x288(:,:,260), nhel, den(386))
    call cont_VV(nsync, wf36(:,57), wf8(:,21), A(:,261), n3(:,451), t3x288(:,:,261), nhel, den(387))
    call cont_VV(nsync, wf36(:,58), wf8(:,21), A(:,262), n3(:,452), t3x288(:,:,262), nhel, den(388))
    call cont_VV(nsync, wf36(:,48), wf8(:,23), A(:,263), n3(:,453), t3x288(:,:,263), nhel, den(390))
    call cont_VV(nsync, wf36(:,37), wf8(:,24), A(:,264), n3(:,454), t3x288(:,:,264), nhel, den(392))
    call cont_QA(nsync, wf18(:,34), wf16(:,53), A(:,265), n3(:,455), t3x288(:,:,265), nhel, den(393))
    call cont_QA(nsync, wf18(:,34), wf16(:,54), A(:,266), n3(:,456), t3x288(:,:,266), nhel, den(394))
    call cont_QA(nsync, wf18(:,26), wf16(:,55), A(:,267), n3(:,457), t3x288(:,:,267), nhel, den(395))
    call cont_QA(nsync, wf18(:,12), wf16(:,56), A(:,268), n3(:,458), t3x288(:,:,268), nhel, den(396))
    call cont_QA(nsync, wf18(:,36), wf16(:,54), A(:,269), n3(:,459), t3x288(:,:,269), nhel, den(397))
    call cont_QA(nsync, wf18(:,38), wf16(:,54), A(:,270), n3(:,460), t3x288(:,:,270), nhel, den(398))
    call cont_QA(nsync, wf18(:,40), wf16(:,54), A(:,271), n3(:,461), t3x288(:,:,271), nhel, den(399))
    call cont_QA(nsync, wf18(:,36), wf16(:,53), A(:,272), n3(:,462), t3x288(:,:,272), nhel, den(400))
    call cont_QA(nsync, wf18(:,38), wf16(:,53), A(:,273), n3(:,463), t3x288(:,:,273), nhel, den(401))
    call cont_QA(nsync, wf18(:,40), wf16(:,53), A(:,274), n3(:,464), t3x288(:,:,274), nhel, den(402))
    call cont_QA(nsync, wf18(:,14), wf16(:,56), A(:,275), n3(:,465), t3x288(:,:,275), nhel, den(403))
    call cont_QA(nsync, wf18(:,16), wf16(:,56), A(:,276), n3(:,466), t3x288(:,:,276), nhel, den(404))
    call cont_QA(nsync, wf18(:,18), wf16(:,56), A(:,277), n3(:,467), t3x288(:,:,277), nhel, den(405))
    call cont_VV(nsync, wf36(:,53), wf8(:,23), A(:,278), n3(:,468), t3x288(:,:,278), nhel, den(406))
    call cont_VV(nsync, wf36(:,54), wf8(:,23), A(:,279), n3(:,469), t3x288(:,:,279), nhel, den(407))
    call cont_VV(nsync, wf36(:,55), wf8(:,23), A(:,280), n3(:,470), t3x288(:,:,280), nhel, den(408))

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

  M1(1) = ((-A(j,68)%j-A(j,71)%j-A(j,77)%j-A(j,80)%j-A(j,108)%j-A(j,111)%j-A(j,116)%j-A(j,119)%j-A(j,125)%j-A(j,128)%j-A(j,156)%j &
       -A(j,159)%j-A(j,222)%j-A(j,225)%j-A(j,234)%j-A(j,243)%j-A(j,255)%j-A(j,261)%j-A(j,273)%j-A(j,276)%j)*f(1))/6._/**/REALKIND &
       +((-A(j,20)%j-A(j,23)%j-A(j,29)%j-A(j,32)%j-A(j,60)%j-A(j,63)%j-A(j,164)%j-A(j,167)%j-A(j,173)%j-A(j,176)%j-A(j,204)%j &
       -A(j,207)%j-A(j,216)%j-A(j,219)%j-A(j,237)%j-A(j,240)%j-A(j,252)%j-A(j,258)%j-A(j,270)%j-A(j,279)%j)*f(1))/2._/**/REALKIND &
       +((-A(j,5)%j-A(j,7)%j-A(j,9)%j-A(j,11)%j+A(j,92)%j+A(j,96)%j+A(j,101)%j+A(j,104)%j+A(j,140)%j+A(j,144)%j+A(j,149)%j &
       +A(j,152)%j)*f(2))/6._/**/REALKIND+((-A(j,1)%j-A(j,3)%j-A(j,13)%j-A(j,15)%j+A(j,44)%j+A(j,48)%j+A(j,53)%j+A(j,56)%j &
       +A(j,188)%j+A(j,192)%j+A(j,197)%j+A(j,200)%j)*f(2))/2._/**/REALKIND+((A(j,17)%j+A(j,18)%j+A(j,25)%j+A(j,26)%j+A(j,27)%j &
       +A(j,34)%j+A(j,39)%j+A(j,51)%j+A(j,161)%j+A(j,162)%j+A(j,169)%j+A(j,170)%j+A(j,171)%j+A(j,178)%j+A(j,183)%j+A(j,195)%j &
       +A(j,211)%j+A(j,212)%j+A(j,213)%j+A(j,227)%j+A(j,228)%j+A(j,232)%j+A(j,246)%j+A(j,247)%j+A(j,250)%j+A(j,263)%j+A(j,266)%j &
       +A(j,267)%j)*f(3))/2._/**/REALKIND+((A(j,65)%j+A(j,66)%j+A(j,73)%j+A(j,74)%j+A(j,75)%j+A(j,82)%j+A(j,87)%j+A(j,99)%j &
       +A(j,113)%j+A(j,114)%j+A(j,121)%j+A(j,122)%j+A(j,123)%j+A(j,130)%j+A(j,135)%j+A(j,147)%j+A(j,209)%j+A(j,210)%j+A(j,214)%j &
       +A(j,229)%j+A(j,230)%j+A(j,231)%j+A(j,245)%j+A(j,248)%j+A(j,249)%j+A(j,264)%j+A(j,265)%j+A(j,268)%j)*f(3))/6._/**/REALKIND &
       +((-A(j,83)%j-A(j,84)%j-A(j,88)%j-A(j,98)%j-A(j,131)%j-A(j,132)%j-A(j,136)%j-A(j,146)%j)*f(4))/6._/**/REALKIND+((-A(j,35)%j &
       -A(j,36)%j-A(j,40)%j-A(j,50)%j-A(j,179)%j-A(j,180)%j-A(j,184)%j-A(j,194)%j)*f(4))/2._/**/REALKIND+((-A(j,85)%j-A(j,86)%j &
       -A(j,89)%j-A(j,90)%j-A(j,94)%j-A(j,106)%j-A(j,133)%j-A(j,134)%j-A(j,137)%j-A(j,138)%j-A(j,142)%j &
       -A(j,154)%j)*f(5))/6._/**/REALKIND+((-A(j,37)%j-A(j,38)%j-A(j,41)%j-A(j,42)%j-A(j,46)%j-A(j,58)%j-A(j,181)%j-A(j,182)%j &
       -A(j,185)%j-A(j,186)%j-A(j,190)%j-A(j,202)%j)*f(5))/2._/**/REALKIND+((-A(j,67)%j-A(j,70)%j-A(j,76)%j-A(j,79)%j-A(j,107)%j &
       -A(j,110)%j-A(j,115)%j-A(j,118)%j-A(j,124)%j-A(j,127)%j-A(j,155)%j-A(j,158)%j-A(j,221)%j-A(j,224)%j-A(j,233)%j-A(j,242)%j &
       -A(j,254)%j-A(j,260)%j-A(j,272)%j-A(j,275)%j)*f(6))/6._/**/REALKIND+((-A(j,19)%j-A(j,22)%j-A(j,28)%j-A(j,31)%j-A(j,59)%j &
       -A(j,62)%j-A(j,163)%j-A(j,166)%j-A(j,172)%j-A(j,175)%j-A(j,203)%j-A(j,206)%j-A(j,215)%j-A(j,218)%j-A(j,236)%j-A(j,239)%j &
       -A(j,251)%j-A(j,257)%j-A(j,269)%j-A(j,278)%j)*f(6))/2._/**/REALKIND+((A(j,91)%j+A(j,95)%j+A(j,100)%j+A(j,103)%j+A(j,139)%j &
       +A(j,143)%j+A(j,148)%j+A(j,151)%j)*f(7))/6._/**/REALKIND+((A(j,43)%j+A(j,47)%j+A(j,52)%j+A(j,55)%j+A(j,187)%j+A(j,191)%j &
       +A(j,196)%j+A(j,199)%j)*f(7))/2._/**/REALKIND+((A(j,69)%j+A(j,72)%j+A(j,78)%j+A(j,81)%j+A(j,109)%j+A(j,112)%j+A(j,117)%j &
       +A(j,120)%j+A(j,126)%j+A(j,129)%j+A(j,157)%j+A(j,160)%j+A(j,223)%j+A(j,226)%j+A(j,235)%j+A(j,244)%j+A(j,256)%j+A(j,262)%j &
       +A(j,274)%j+A(j,277)%j)*f(8))/6._/**/REALKIND+((A(j,21)%j+A(j,24)%j+A(j,30)%j+A(j,33)%j+A(j,61)%j+A(j,64)%j+A(j,165)%j &
       +A(j,168)%j+A(j,174)%j+A(j,177)%j+A(j,205)%j+A(j,208)%j+A(j,217)%j+A(j,220)%j+A(j,238)%j+A(j,241)%j+A(j,253)%j+A(j,259)%j &
       +A(j,271)%j+A(j,280)%j)*f(8))/2._/**/REALKIND+((A(j,6)%j+A(j,8)%j+A(j,10)%j+A(j,12)%j-A(j,93)%j-A(j,97)%j-A(j,102)%j &
       -A(j,105)%j-A(j,141)%j-A(j,145)%j-A(j,150)%j-A(j,153)%j)*f(9))/6._/**/REALKIND+((A(j,2)%j+A(j,4)%j+A(j,14)%j+A(j,16)%j &
       -A(j,45)%j-A(j,49)%j-A(j,54)%j-A(j,57)%j-A(j,189)%j-A(j,193)%j-A(j,198)%j-A(j,201)%j)*f(9))/2._/**/REALKIND
  M1(2) = ((A(j,68)%j+A(j,71)%j+A(j,77)%j+A(j,80)%j+A(j,108)%j+A(j,111)%j+A(j,116)%j+A(j,119)%j+A(j,125)%j+A(j,128)%j+A(j,156)%j &
       +A(j,159)%j+A(j,222)%j+A(j,225)%j+A(j,234)%j+A(j,243)%j+A(j,255)%j+A(j,261)%j+A(j,273)%j+A(j,276)%j)*f(1))/2._/**/REALKIND &
       +((A(j,20)%j+A(j,23)%j+A(j,29)%j+A(j,32)%j+A(j,60)%j+A(j,63)%j+A(j,164)%j+A(j,167)%j+A(j,173)%j+A(j,176)%j+A(j,204)%j &
       +A(j,207)%j+A(j,216)%j+A(j,219)%j+A(j,237)%j+A(j,240)%j+A(j,252)%j+A(j,258)%j+A(j,270)%j+A(j,279)%j)*f(1))/6._/**/REALKIND &
       +((A(j,5)%j+A(j,7)%j+A(j,9)%j+A(j,11)%j-A(j,92)%j-A(j,96)%j-A(j,101)%j-A(j,104)%j-A(j,140)%j-A(j,144)%j-A(j,149)%j &
       -A(j,152)%j)*f(2))/2._/**/REALKIND+((A(j,1)%j+A(j,3)%j+A(j,13)%j+A(j,15)%j-A(j,44)%j-A(j,48)%j-A(j,53)%j-A(j,56)%j &
       -A(j,188)%j-A(j,192)%j-A(j,197)%j-A(j,200)%j)*f(2))/6._/**/REALKIND+((-A(j,17)%j-A(j,18)%j-A(j,25)%j-A(j,26)%j-A(j,27)%j &
       -A(j,34)%j-A(j,39)%j-A(j,51)%j-A(j,161)%j-A(j,162)%j-A(j,169)%j-A(j,170)%j-A(j,171)%j-A(j,178)%j-A(j,183)%j-A(j,195)%j &
       -A(j,211)%j-A(j,212)%j-A(j,213)%j-A(j,227)%j-A(j,228)%j-A(j,232)%j-A(j,246)%j-A(j,247)%j-A(j,250)%j-A(j,263)%j-A(j,266)%j &
       -A(j,267)%j)*f(3))/6._/**/REALKIND+((-A(j,65)%j-A(j,66)%j-A(j,73)%j-A(j,74)%j-A(j,75)%j-A(j,82)%j-A(j,87)%j-A(j,99)%j &
       -A(j,113)%j-A(j,114)%j-A(j,121)%j-A(j,122)%j-A(j,123)%j-A(j,130)%j-A(j,135)%j-A(j,147)%j-A(j,209)%j-A(j,210)%j-A(j,214)%j &
       -A(j,229)%j-A(j,230)%j-A(j,231)%j-A(j,245)%j-A(j,248)%j-A(j,249)%j-A(j,264)%j-A(j,265)%j-A(j,268)%j)*f(3))/2._/**/REALKIND &
       +((A(j,83)%j+A(j,84)%j+A(j,88)%j+A(j,98)%j+A(j,131)%j+A(j,132)%j+A(j,136)%j+A(j,146)%j)*f(4))/2._/**/REALKIND+((A(j,35)%j &
       +A(j,36)%j+A(j,40)%j+A(j,50)%j+A(j,179)%j+A(j,180)%j+A(j,184)%j+A(j,194)%j)*f(4))/6._/**/REALKIND+((A(j,85)%j+A(j,86)%j &
       +A(j,89)%j+A(j,90)%j+A(j,94)%j+A(j,106)%j+A(j,133)%j+A(j,134)%j+A(j,137)%j+A(j,138)%j+A(j,142)%j &
       +A(j,154)%j)*f(5))/2._/**/REALKIND+((A(j,37)%j+A(j,38)%j+A(j,41)%j+A(j,42)%j+A(j,46)%j+A(j,58)%j+A(j,181)%j+A(j,182)%j &
       +A(j,185)%j+A(j,186)%j+A(j,190)%j+A(j,202)%j)*f(5))/6._/**/REALKIND+((A(j,67)%j+A(j,70)%j+A(j,76)%j+A(j,79)%j+A(j,107)%j &
       +A(j,110)%j+A(j,115)%j+A(j,118)%j+A(j,124)%j+A(j,127)%j+A(j,155)%j+A(j,158)%j+A(j,221)%j+A(j,224)%j+A(j,233)%j+A(j,242)%j &
       +A(j,254)%j+A(j,260)%j+A(j,272)%j+A(j,275)%j)*f(6))/2._/**/REALKIND+((A(j,19)%j+A(j,22)%j+A(j,28)%j+A(j,31)%j+A(j,59)%j &
       +A(j,62)%j+A(j,163)%j+A(j,166)%j+A(j,172)%j+A(j,175)%j+A(j,203)%j+A(j,206)%j+A(j,215)%j+A(j,218)%j+A(j,236)%j+A(j,239)%j &
       +A(j,251)%j+A(j,257)%j+A(j,269)%j+A(j,278)%j)*f(6))/6._/**/REALKIND+((-A(j,91)%j-A(j,95)%j-A(j,100)%j-A(j,103)%j-A(j,139)%j &
       -A(j,143)%j-A(j,148)%j-A(j,151)%j)*f(7))/2._/**/REALKIND+((-A(j,43)%j-A(j,47)%j-A(j,52)%j-A(j,55)%j-A(j,187)%j-A(j,191)%j &
       -A(j,196)%j-A(j,199)%j)*f(7))/6._/**/REALKIND+((-A(j,69)%j-A(j,72)%j-A(j,78)%j-A(j,81)%j-A(j,109)%j-A(j,112)%j-A(j,117)%j &
       -A(j,120)%j-A(j,126)%j-A(j,129)%j-A(j,157)%j-A(j,160)%j-A(j,223)%j-A(j,226)%j-A(j,235)%j-A(j,244)%j-A(j,256)%j-A(j,262)%j &
       -A(j,274)%j-A(j,277)%j)*f(8))/2._/**/REALKIND+((-A(j,21)%j-A(j,24)%j-A(j,30)%j-A(j,33)%j-A(j,61)%j-A(j,64)%j-A(j,165)%j &
       -A(j,168)%j-A(j,174)%j-A(j,177)%j-A(j,205)%j-A(j,208)%j-A(j,217)%j-A(j,220)%j-A(j,238)%j-A(j,241)%j-A(j,253)%j-A(j,259)%j &
       -A(j,271)%j-A(j,280)%j)*f(8))/6._/**/REALKIND+((-A(j,6)%j-A(j,8)%j-A(j,10)%j-A(j,12)%j+A(j,93)%j+A(j,97)%j+A(j,102)%j &
       +A(j,105)%j+A(j,141)%j+A(j,145)%j+A(j,150)%j+A(j,153)%j)*f(9))/2._/**/REALKIND+((-A(j,2)%j-A(j,4)%j-A(j,14)%j-A(j,16)%j &
       +A(j,45)%j+A(j,49)%j+A(j,54)%j+A(j,57)%j+A(j,189)%j+A(j,193)%j+A(j,198)%j+A(j,201)%j)*f(9))/6._/**/REALKIND

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
  use ol_colourmatrix_ppvvvj_bbbxbxawwx_1_/**/REALKIND, only: K1
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
  use ol_colourmatrix_ppvvvj_bbbxbxawwx_1_/**/REALKIND, only: K1
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
  & bind(c,name="ol_tree_colvect_ppvvvj_bbbxbxawwx_1")
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
    & bind(c,name="ol_f_amp2tree_ppvvvj_bbbxbxawwx_1")
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
    & bind(c,name="ol_f_amp2ccone_ppvvvj_bbbxbxawwx_1")
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
    & bind(c,name="ol_f_amp2ccall_ppvvvj_bbbxbxawwx_1")
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
    & bind(c,name="ol_f_amp2hcone_ppvvvj_bbbxbxawwx_1")
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
    & bind(c,name="ol_f_amp2hcall_ppvvvj_bbbxbxawwx_1")
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
    & bind(c,name="ol_amp2tree_ppvvvj_bbbxbxawwx_1")
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
    & bind(c,name="ol_amp2ccone_ppvvvj_bbbxbxawwx_1")
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
    & bind(c,name="ol_amp2ccall_ppvvvj_bbbxbxawwx_1")
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
    & bind(c,name="ol_amp2hcone_ppvvvj_bbbxbxawwx_1")
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
    & bind(c,name="ol_amp2hcall_ppvvvj_bbbxbxawwx_1")
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
    & bind(c,name="amp2tree_ppvvvj_bbbxbxawwx_1_")
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
    & bind(c,name="amp2ccone_ppvvvj_bbbxbxawwx_1_")
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
    & bind(c,name="amp2ccall_ppvvvj_bbbxbxawwx_1_")
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
    & bind(c,name="amp2hcone_ppvvvj_bbbxbxawwx_1_")
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
    & bind(c,name="amp2hcall_ppvvvj_bbbxbxawwx_1_")
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

end module ol_tree_ppvvvj_bbbxbxawwx_1_/**/REALKIND
