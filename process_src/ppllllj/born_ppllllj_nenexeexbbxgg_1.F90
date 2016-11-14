
module ol_colourmatrix_ppllllj_nenexeexbbxgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(76,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  48,  -6]
  K1( 2,:) = [  -6,  48]
  K1( 3,:) = [   0,   0]
  K1( 4,:) = [   0,   0]
  K1( 5,:) = [   0,   0]
  K1( 6,:) = [   0,   0]
  K1( 7,:) = [   0,   0]
  K1( 8,:) = [   0,   0]
  K1( 9,:) = [   0,   0]
  K1(10,:) = [   0,   0]
  K1(11,:) = [   0,   0]
  K1(12,:) = [   0,   0]
  K1(13,:) = [   0,   0]
  K1(14,:) = [   0,   0]
  K1(15,:) = [   0,   0]
  K1(16,:) = [   0,   0]
  K1(17,:) = [   0,   0]
  K1(18,:) = [   0,   0]
  K1(19,:) = [   0,   0]
  K1(20,:) = [   0,   0]
  K1(21,:) = [   0,   0]
  K1(22,:) = [   0,   0]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [   0,   0]
  K1(28,:) = [   0,   0]
  K1(29,:) = [   0,   0]
  K1(30,:) = [   0,   0]
  K1(31,:) = [  64,  -8]
  K1(32,:) = [  -8,  64]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [   0,   0]
  K1(38,:) = [   0,   0]
  K1(39,:) = [   0,   0]
  K1(40,:) = [   0,   0]
  K1(41,:) = [  -1, -10]
  K1(42,:) = [ -10,  -1]
  K1(43,:) = [  64,  -8]
  K1(44,:) = [  -8,  64]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]
  K1(47,:) = [   0,   0]
  K1(48,:) = [   0,   0]
  K1(49,:) = [   0,   0]
  K1(50,:) = [   0,   0]
  K1(51,:) = [   0,   0]
  K1(52,:) = [   0,   0]
  K1(53,:) = [   9,   9]
  K1(54,:) = [   9, -72]
  K1(55,:) = [ -72,   9]
  K1(56,:) = [   9,   9]
  K1(57,:) = [ 144, -18]
  K1(58,:) = [ -18, 144]
  K1(59,:) = [   0,   0]
  K1(60,:) = [   0,   0]
  K1(61,:) = [   0,   0]
  K1(62,:) = [   0,   0]
  K1(63,:) = [   0,   0]
  K1(64,:) = [   0,   0]
  K1(65,:) = [   0,   0]
  K1(66,:) = [   0,   0]
  K1(67,:) = [ -72,   9]
  K1(68,:) = [   9,   9]
  K1(69,:) = [   9,   9]
  K1(70,:) = [   9, -72]
  K1(71,:) = [ -81,   0]
  K1(72,:) = [   0, -81]
  K1(73,:) = [ 144, -18]
  K1(74,:) = [ -18, 144]
  K1(75,:) = [   0,   0]
  K1(76,:) = [   0,   0]
  K1 = (1._/**/REALKIND / 9) * K1

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllllj_nenexeexbbxgg_1_/**/REALKIND



module ol_forced_parameters_ppllllj_nenexeexbbxgg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllllj_nenexeexbbxgg_1_/**/REALKIND

module ol_tree_ppllllj_nenexeexbbxgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(16)
  complex(REALKIND), save :: den(347)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 256 ! number of helicity configurations
  integer(intkind2), save :: nhel = 256 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(256) ! physical helicity states
  complex(DREALKIND) :: M1helarr(2,256) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f( 1) = (CI*eQED**4*gQCD**2)/3._/**/REALKIND
    f( 2) = CI*eQED**4*gQCD**2
    f( 3) = (eQED**4*gQCD**2)/3._/**/REALKIND
    f( 4) = eQED**4*gQCD**2
    f( 5) = (CI*eQED**4*gQCD**2)/(4._/**/REALKIND*sw**4)
    f( 6) = (eQED**4*gQCD**2)/(sw**4*4._/**/REALKIND)
    f( 7) = (CI*eQED**4*gQCD**2*MB)/(4._/**/REALKIND*sw**4)
    f( 8) = (eQED**4*gQCD**2*MB)/(sw**4*4._/**/REALKIND)
    f( 9) = (CI*cw*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**3)
    f(10) = (cw*eQED**4*gQCD**2)/(sw**3*2._/**/REALKIND)
    f(11) = (CI*eQED**4*gQCD**2)/(6._/**/REALKIND*sw**2)
    f(12) = (CI*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(13) = (eQED**4*gQCD**2)/(sw**2*6._/**/REALKIND)
    f(14) = (eQED**4*gQCD**2)/(sw**2*2._/**/REALKIND)
    f(15) = (CI*eQED**4*gQCD**2*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(16) = (eQED**4*gQCD**2*MB)/(cw**2*sw**2*2._/**/REALKIND)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,3) - MZ2)
  den(2) = 1 / (Q(5,12) - MZ2)
  den(3) = 1 / (Q(5,80) - MB2)
  den(4) = 1 / (Q(5,160) - MB2)
  den(7) = 1 / (Q(5,15) - MH2)
  den(10) = 1 / (Q(5,12))
  den(13) = 1 / (Q(5,83) - MB2)
  den(20) = 1 / (Q(5,163) - MB2)
  den(25) = 1 / (Q(5,35) - MB2)
  den(27) = 1 / (Q(5,208) - MB2)
  den(33) = 1 / (Q(5,92) - MB2)
  den(38) = 1 / (Q(5,44) - MB2)
  den(48) = 1 / (Q(5,144) - MB2)
  den(49) = 1 / (Q(5,96) - MB2)
  den(54) = 1 / (Q(5,147) - MB2)
  den(61) = 1 / (Q(5,99) - MB2)
  den(66) = 1 / (Q(5,19) - MB2)
  den(68) = 1 / (Q(5,108) - MB2)
  den(73) = 1 / (Q(5,224) - MB2)
  den(79) = 1 / (Q(5,28) - MB2)
  den(92) = 1 / (Q(5,156) - MB2)
  den(102) = 1 / (Q(5,172) - MB2)
  den(115) = 1 / (Q(5,192))
  den(153) = 1 / (Q(5,7))
  den(155) = 1 / (Q(5,240))
  den(158) = 1 / (Q(5,240) - MZ2)
  den(161) = 1 / (Q(5,11))
  den(165) = 1 / (Q(5,15))
  den(168) = 1 / (Q(5,15) - MZ2)
  den(201) = 1 / (Q(5,9) - MW2)
  den(202) = 1 / (Q(5,6) - MW2)
  den(212) = 1 / (Q(5,89) - MT2)
  den(215) = 1 / (Q(5,38) - MT2)
  den(228) = 1 / (Q(5,153) - MT2)
  den(231) = 1 / (Q(5,25) - MT2)
  den(233) = 1 / (Q(5,102) - MT2)
  den(246) = 1 / (Q(5,166) - MT2)
  den(271) = 1 / (Q(5,14))
  den(301) = 1 / (Q(5,13))

  ! denominators

  den(5) = den(1)*den(2)
  den(6) = den(3)*den(4)
  den(8) = den(5)*den(7)
  den(9) = den(6)*den(8)
  den(11) = den(1)*den(3)
  den(12) = den(4)*den(10)
  den(14) = den(11)*den(13)
  den(15) = den(12)*den(14)
  den(16) = den(2)*den(4)
  den(17) = den(14)*den(16)
  den(18) = den(1)*den(4)
  den(19) = den(3)*den(10)
  den(21) = den(18)*den(20)
  den(22) = den(19)*den(21)
  den(23) = den(2)*den(3)
  den(24) = den(21)*den(23)
  den(26) = den(1)*den(25)
  den(28) = den(3)*den(27)
  den(29) = den(10)*den(26)
  den(30) = den(28)*den(29)
  den(31) = den(2)*den(26)
  den(32) = den(28)*den(31)
  den(34) = den(19)*den(33)
  den(35) = den(26)*den(34)
  den(36) = den(23)*den(33)
  den(37) = den(26)*den(36)
  den(39) = den(10)*den(38)
  den(40) = den(1)*den(39)
  den(41) = den(28)*den(40)
  den(42) = den(2)*den(38)
  den(43) = den(1)*den(42)
  den(44) = den(28)*den(43)
  den(45) = den(14)*den(39)
  den(46) = den(14)*den(42)
  den(47) = den(8)*den(28)
  den(50) = den(48)*den(49)
  den(51) = den(8)*den(50)
  den(52) = den(1)*den(48)
  den(53) = den(10)*den(49)
  den(55) = den(52)*den(54)
  den(56) = den(53)*den(55)
  den(57) = den(2)*den(49)
  den(58) = den(55)*den(57)
  den(59) = den(1)*den(49)
  den(60) = den(10)*den(48)
  den(62) = den(59)*den(61)
  den(63) = den(60)*den(62)
  den(64) = den(2)*den(48)
  den(65) = den(62)*den(64)
  den(67) = den(1)*den(66)
  den(69) = den(53)*den(68)
  den(70) = den(67)*den(69)
  den(71) = den(57)*den(68)
  den(72) = den(67)*den(71)
  den(74) = den(49)*den(73)
  den(75) = den(10)*den(67)
  den(76) = den(74)*den(75)
  den(77) = den(2)*den(67)
  den(78) = den(74)*den(77)
  den(80) = den(10)*den(79)
  den(81) = den(62)*den(80)
  den(82) = den(2)*den(79)
  den(83) = den(62)*den(82)
  den(84) = den(1)*den(80)
  den(85) = den(74)*den(84)
  den(86) = den(1)*den(82)
  den(87) = den(74)*den(86)
  den(88) = den(8)*den(74)
  den(89) = den(27)*den(48)
  den(90) = den(29)*den(89)
  den(91) = den(31)*den(89)
  den(93) = den(60)*den(92)
  den(94) = den(26)*den(93)
  den(95) = den(64)*den(92)
  den(96) = den(26)*den(95)
  den(97) = den(40)*den(89)
  den(98) = den(43)*den(89)
  den(99) = den(39)*den(55)
  den(100) = den(42)*den(55)
  den(101) = den(8)*den(89)
  den(103) = den(12)*den(102)
  den(104) = den(67)*den(103)
  den(105) = den(16)*den(102)
  den(106) = den(67)*den(105)
  den(107) = den(4)*den(73)
  den(108) = den(75)*den(107)
  den(109) = den(77)*den(107)
  den(110) = den(21)*den(80)
  den(111) = den(21)*den(82)
  den(112) = den(84)*den(107)
  den(113) = den(86)*den(107)
  den(114) = den(8)*den(107)
  den(116) = den(39)*den(67)
  den(117) = den(115)*den(116)
  den(118) = den(42)*den(67)
  den(119) = den(115)*den(118)
  den(120) = den(73)*den(115)
  den(121) = den(75)*den(120)
  den(122) = den(77)*den(120)
  den(123) = den(26)*den(80)
  den(124) = den(115)*den(123)
  den(125) = den(26)*den(82)
  den(126) = den(115)*den(125)
  den(127) = den(27)*den(115)
  den(128) = den(10)*den(127)
  den(129) = den(26)*den(128)
  den(130) = den(2)*den(127)
  den(131) = den(26)*den(130)
  den(132) = den(84)*den(120)
  den(133) = den(86)*den(120)
  den(134) = den(1)*den(127)
  den(135) = den(39)*den(134)
  den(136) = den(42)*den(134)
  den(137) = den(8)*den(127)
  den(138) = den(8)*den(120)
  den(139) = den(13)*den(67)
  den(140) = den(39)*den(139)
  den(141) = den(42)*den(139)
  den(142) = den(39)*den(68)
  den(143) = den(67)*den(142)
  den(144) = den(42)*den(68)
  den(145) = den(67)*den(144)
  den(146) = den(33)*den(80)
  den(147) = den(26)*den(146)
  den(148) = den(33)*den(82)
  den(149) = den(26)*den(148)
  den(150) = den(26)*den(61)
  den(151) = den(80)*den(150)
  den(152) = den(82)*den(150)
  den(154) = den(1)*den(153)
  den(156) = den(6)*den(155)
  den(157) = den(154)*den(156)
  den(159) = den(6)*den(158)
  den(160) = den(154)*den(159)
  den(162) = den(1)*den(161)
  den(163) = den(156)*den(162)
  den(164) = den(159)*den(162)
  den(166) = den(154)*den(165)
  den(167) = den(28)*den(166)
  den(169) = den(154)*den(168)
  den(170) = den(28)*den(169)
  den(171) = den(162)*den(165)
  den(172) = den(28)*den(171)
  den(173) = den(162)*den(168)
  den(174) = den(28)*den(173)
  den(175) = den(50)*den(155)
  den(176) = den(154)*den(175)
  den(177) = den(50)*den(158)
  den(178) = den(154)*den(177)
  den(179) = den(162)*den(175)
  den(180) = den(162)*den(177)
  den(181) = den(74)*den(166)
  den(182) = den(74)*den(169)
  den(183) = den(74)*den(171)
  den(184) = den(74)*den(173)
  den(185) = den(89)*den(166)
  den(186) = den(89)*den(169)
  den(187) = den(89)*den(171)
  den(188) = den(89)*den(173)
  den(189) = den(107)*den(166)
  den(190) = den(107)*den(169)
  den(191) = den(107)*den(171)
  den(192) = den(107)*den(173)
  den(193) = den(120)*den(166)
  den(194) = den(120)*den(169)
  den(195) = den(127)*den(166)
  den(196) = den(127)*den(169)
  den(197) = den(120)*den(171)
  den(198) = den(120)*den(173)
  den(199) = den(127)*den(171)
  den(200) = den(127)*den(173)
  den(203) = den(201)*den(202)
  den(204) = den(7)*den(203)
  den(205) = den(6)*den(204)
  den(206) = den(165)*den(203)
  den(207) = den(6)*den(206)
  den(208) = den(168)*den(203)
  den(209) = den(6)*den(208)
  den(210) = den(3)*den(201)
  den(211) = den(4)*den(202)
  den(213) = den(210)*den(212)
  den(214) = den(211)*den(213)
  den(216) = den(202)*den(215)
  den(217) = den(201)*den(216)
  den(218) = den(28)*den(217)
  den(219) = den(213)*den(216)
  den(220) = den(28)*den(204)
  den(221) = den(28)*den(206)
  den(222) = den(28)*den(208)
  den(223) = den(50)*den(204)
  den(224) = den(50)*den(206)
  den(225) = den(50)*den(208)
  den(226) = den(48)*den(201)
  den(227) = den(49)*den(202)
  den(229) = den(226)*den(228)
  den(230) = den(227)*den(229)
  den(232) = den(201)*den(231)
  den(234) = den(227)*den(233)
  den(235) = den(232)*den(234)
  den(236) = den(202)*den(232)
  den(237) = den(74)*den(236)
  den(238) = den(74)*den(204)
  den(239) = den(74)*den(206)
  den(240) = den(74)*den(208)
  den(241) = den(89)*den(217)
  den(242) = den(216)*den(229)
  den(243) = den(89)*den(204)
  den(244) = den(89)*den(206)
  den(245) = den(89)*den(208)
  den(247) = den(211)*den(246)
  den(248) = den(232)*den(247)
  den(249) = den(107)*den(236)
  den(250) = den(107)*den(204)
  den(251) = den(107)*den(206)
  den(252) = den(107)*den(208)
  den(253) = den(216)*den(232)
  den(254) = den(115)*den(253)
  den(255) = den(120)*den(236)
  den(256) = den(127)*den(201)
  den(257) = den(216)*den(256)
  den(258) = den(127)*den(204)
  den(259) = den(127)*den(206)
  den(260) = den(127)*den(208)
  den(261) = den(120)*den(204)
  den(262) = den(120)*den(206)
  den(263) = den(120)*den(208)
  den(264) = den(212)*den(232)
  den(265) = den(216)*den(264)
  den(266) = den(216)*den(233)
  den(267) = den(232)*den(266)
  den(268) = den(153)*den(202)
  den(269) = den(156)*den(268)
  den(270) = den(159)*den(268)
  den(272) = den(202)*den(271)
  den(273) = den(159)*den(272)
  den(274) = den(165)*den(268)
  den(275) = den(28)*den(274)
  den(276) = den(168)*den(268)
  den(277) = den(28)*den(276)
  den(278) = den(168)*den(272)
  den(279) = den(28)*den(278)
  den(280) = den(175)*den(268)
  den(281) = den(177)*den(268)
  den(282) = den(177)*den(272)
  den(283) = den(74)*den(274)
  den(284) = den(74)*den(276)
  den(285) = den(74)*den(278)
  den(286) = den(89)*den(274)
  den(287) = den(89)*den(276)
  den(288) = den(89)*den(278)
  den(289) = den(107)*den(274)
  den(290) = den(107)*den(276)
  den(291) = den(107)*den(278)
  den(292) = den(120)*den(274)
  den(293) = den(120)*den(276)
  den(294) = den(127)*den(274)
  den(295) = den(127)*den(276)
  den(296) = den(120)*den(278)
  den(297) = den(127)*den(278)
  den(298) = den(161)*den(201)
  den(299) = den(156)*den(298)
  den(300) = den(159)*den(298)
  den(302) = den(201)*den(301)
  den(303) = den(159)*den(302)
  den(304) = den(165)*den(298)
  den(305) = den(28)*den(304)
  den(306) = den(168)*den(298)
  den(307) = den(28)*den(306)
  den(308) = den(168)*den(302)
  den(309) = den(28)*den(308)
  den(310) = den(175)*den(298)
  den(311) = den(177)*den(298)
  den(312) = den(177)*den(302)
  den(313) = den(74)*den(304)
  den(314) = den(74)*den(306)
  den(315) = den(74)*den(308)
  den(316) = den(89)*den(304)
  den(317) = den(89)*den(306)
  den(318) = den(89)*den(308)
  den(319) = den(107)*den(304)
  den(320) = den(107)*den(306)
  den(321) = den(107)*den(308)
  den(322) = den(120)*den(304)
  den(323) = den(120)*den(306)
  den(324) = den(127)*den(304)
  den(325) = den(127)*den(306)
  den(326) = den(120)*den(308)
  den(327) = den(127)*den(308)
  den(328) = den(2)*den(301)
  den(329) = den(159)*den(328)
  den(330) = den(2)*den(271)
  den(331) = den(159)*den(330)
  den(332) = den(168)*den(328)
  den(333) = den(28)*den(332)
  den(334) = den(168)*den(330)
  den(335) = den(28)*den(334)
  den(336) = den(177)*den(328)
  den(337) = den(177)*den(330)
  den(338) = den(74)*den(332)
  den(339) = den(74)*den(334)
  den(340) = den(89)*den(332)
  den(341) = den(89)*den(334)
  den(342) = den(107)*den(332)
  den(343) = den(107)*den(334)
  den(344) = den(120)*den(332)
  den(345) = den(127)*den(332)
  den(346) = den(120)*den(334)
  den(347) = den(127)*den(334)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_ppllllj_nenexeexbbxgg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_ppllllj_nenexeexbbxgg_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for nu_e anti-nu_e e- e+ bottom anti-bottom glue glue -> 0
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
  use ol_external_ppllllj_nenexeexbbxgg_1, only: external_perm_ppllllj_nenexeexbbxgg_1, &
    & external_perm_inv_ppllllj_nenexeexbbxgg_1, extcomb_perm_ppllllj_nenexeexbbxgg_1, &
    & average_factor_ppllllj_nenexeexbbxgg_1
  use ol_external_ppllllj_nenexeexbbxgg_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_ppllllj_nenexeexbbxgg_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_ppllllj_nenexeexbbxgg_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_ppllllj_nenexeexbbxgg_1
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
  complex(REALKIND) :: MOM_LC(4), M1(2), M1helarray(2,256)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(2), ex6(2), ex7(2), ex8(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,18), wf8(8,44), wf16(16,106), wf32(32,14), wf64(64,5), wf256(256,203)

  type(polcont) :: A(256,203)
  complex(REALKIND) :: Aj(203)

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
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_ppllllj_nenexeexbbxgg_1,8)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,8)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_ppllllj_nenexeexbbxgg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_ppllllj_nenexeexbbxgg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_ppllllj_nenexeexbbxgg_1(extcombs(k))
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
  call vert_QA_Z(gZn,ntry, ex1, ex2, wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_QA_Z(gZl,ntry, ex3, ex4, wf4(:,2), n3(:,2), t3x4(:,:,2))
  call vert_VQ_A(ntry, ex7, ex5, wf4(:,3), n3(:,3), t3x4(:,:,3))
  call vert_AV_Q(ntry, ex6, ex8, wf4(:,4), n3(:,4), t3x4(:,:,4))
  call prop_W_W(ntry, wf4(:,1), Q(:,3), MZ, 1_intkind1, wf4(:,5), n2(1))
  call prop_W_W(ntry, wf4(:,2), Q(:,12), MZ, 1_intkind1, wf4(:,6), n2(2))
  call prop_Q_A(ntry, wf4(:,3), Q(:,80), MB, 1_intkind1, wf4(:,7), n2(3))
  call prop_A_Q(ntry, wf4(:,4), Q(:,160), MB, 1_intkind1, wf4(:,8), n2(4))
  call vert_VV_S(ntry, wf4(:,5), wf4(:,6), wf16(:,1), n3(:,5), t3x16(:,:,1))
  call vert_AQ_S(gH,ntry, wf4(:,8), wf4(:,7), wf16(:,2), n3(:,6), t3x16(:,:,2))
  call vert_QA_V(ntry, ex3, ex4, wf4(:,9), n3(:,7), t3x4(:,:,5))
  call vert_ZQ_A(gZd,ntry, wf4(:,5), wf4(:,7), wf16(:,3), n3(:,8), t3x16(:,:,3))
  call vert_AV_Q(ntry, wf4(:,8), wf4(:,9), wf16(:,4), n3(:,9), t3x16(:,:,4))
  call prop_Q_A(ntry, wf16(:,3), Q(:,83), MB, 1_intkind1, wf16(:,5), n2(5))
  call vert_AZ_Q(gZd,ntry, wf4(:,8), wf4(:,6), wf16(:,6), n3(:,10), t3x16(:,:,5))
  call vert_AZ_Q(gZd,ntry, wf4(:,8), wf4(:,5), wf16(:,7), n3(:,11), t3x16(:,:,6))
  call vert_VQ_A(ntry, wf4(:,9), wf4(:,7), wf16(:,8), n3(:,12), t3x16(:,:,7))
  call prop_A_Q(ntry, wf16(:,7), Q(:,163), MB, 1_intkind1, wf16(:,9), n2(6))
  call vert_ZQ_A(gZd,ntry, wf4(:,6), wf4(:,7), wf16(:,10), n3(:,13), t3x16(:,:,8))
  call vert_AZ_Q(gZd,ntry, ex6, wf4(:,5), wf8(:,1), n3(:,14), t3x8(:,:,1))
  call vert_VQ_A(ntry, ex8, wf4(:,7), wf8(:,2), n3(:,15), t3x8(:,:,2))
  call prop_A_Q(ntry, wf8(:,1), Q(:,35), MB, 1_intkind1, wf8(:,3), n2(7))
  call prop_Q_A(ntry, wf8(:,2), Q(:,208), MB, 1_intkind1, wf8(:,4), n2(8))
  call vert_AV_Q(ntry, wf8(:,3), wf4(:,9), wf32(:,1), n3(:,16), t3x32(:,:,1))
  call vert_AZ_Q(gZd,ntry, wf8(:,3), wf4(:,6), wf32(:,2), n3(:,17), t3x32(:,:,2))
  call prop_Q_A(ntry, wf16(:,8), Q(:,92), MB, 1_intkind1, wf16(:,11), n2(9))
  call vert_AV_Q(ntry, wf8(:,3), ex8, wf16(:,12), n3(:,18), t3x16(:,:,9))
  call prop_Q_A(ntry, wf16(:,10), Q(:,92), MB, 1_intkind1, wf16(:,13), n2(10))
  call vert_AV_Q(ntry, ex6, wf4(:,9), wf8(:,5), n3(:,19), t3x8(:,:,3))
  call prop_A_Q(ntry, wf8(:,5), Q(:,44), MB, 1_intkind1, wf8(:,6), n2(11))
  call vert_AZ_Q(gZd,ntry, wf8(:,6), wf4(:,5), wf32(:,3), n3(:,20), t3x32(:,:,3))
  call vert_AZ_Q(gZd,ntry, ex6, wf4(:,6), wf8(:,7), n3(:,21), t3x8(:,:,4))
  call prop_A_Q(ntry, wf8(:,7), Q(:,44), MB, 1_intkind1, wf8(:,8), n2(12))
  call vert_AZ_Q(gZd,ntry, wf8(:,8), wf4(:,5), wf32(:,4), n3(:,22), t3x32(:,:,4))
  call vert_AV_Q(ntry, wf8(:,6), ex8, wf16(:,14), n3(:,23), t3x16(:,:,10))
  call vert_AV_Q(ntry, wf8(:,8), ex8, wf16(:,15), n3(:,24), t3x16(:,:,11))
  call vert_AQ_S(gH,ntry, ex6, wf8(:,4), wf16(:,16), n3(:,25), t3x16(:,:,12))
  call vert_VQ_A(ntry, ex8, ex5, wf4(:,10), n3(:,26), t3x4(:,:,6))
  call vert_AV_Q(ntry, ex6, ex7, wf4(:,11), n3(:,27), t3x4(:,:,7))
  call prop_Q_A(ntry, wf4(:,10), Q(:,144), MB, 1_intkind1, wf4(:,12), n2(13))
  call prop_A_Q(ntry, wf4(:,11), Q(:,96), MB, 1_intkind1, wf4(:,13), n2(14))
  call vert_AQ_S(gH,ntry, wf4(:,13), wf4(:,12), wf16(:,17), n3(:,28), t3x16(:,:,13))
  call vert_ZQ_A(gZd,ntry, wf4(:,5), wf4(:,12), wf16(:,18), n3(:,29), t3x16(:,:,14))
  call vert_AV_Q(ntry, wf4(:,13), wf4(:,9), wf16(:,19), n3(:,30), t3x16(:,:,15))
  call prop_Q_A(ntry, wf16(:,18), Q(:,147), MB, 1_intkind1, wf16(:,20), n2(15))
  call vert_AZ_Q(gZd,ntry, wf4(:,13), wf4(:,6), wf16(:,21), n3(:,31), t3x16(:,:,16))
  call vert_AZ_Q(gZd,ntry, wf4(:,13), wf4(:,5), wf16(:,22), n3(:,32), t3x16(:,:,17))
  call vert_VQ_A(ntry, wf4(:,9), wf4(:,12), wf16(:,23), n3(:,33), t3x16(:,:,18))
  call prop_A_Q(ntry, wf16(:,22), Q(:,99), MB, 1_intkind1, wf16(:,24), n2(16))
  call vert_ZQ_A(gZd,ntry, wf4(:,6), wf4(:,12), wf16(:,25), n3(:,34), t3x16(:,:,19))
  call vert_ZQ_A(gZd,ntry, wf4(:,5), ex5, wf8(:,9), n3(:,35), t3x8(:,:,5))
  call prop_Q_A(ntry, wf8(:,9), Q(:,19), MB, 1_intkind1, wf8(:,10), n2(17))
  call prop_A_Q(ntry, wf16(:,19), Q(:,108), MB, 1_intkind1, wf16(:,26), n2(18))
  call vert_VQ_A(ntry, ex8, wf8(:,10), wf16(:,27), n3(:,36), t3x16(:,:,20))
  call prop_A_Q(ntry, wf16(:,21), Q(:,108), MB, 1_intkind1, wf16(:,28), n2(19))
  call vert_AV_Q(ntry, wf4(:,13), ex8, wf8(:,11), n3(:,37), t3x8(:,:,6))
  call prop_A_Q(ntry, wf8(:,11), Q(:,224), MB, 1_intkind1, wf8(:,12), n2(20))
  call vert_VQ_A(ntry, wf4(:,9), wf8(:,10), wf32(:,5), n3(:,38), t3x32(:,:,5))
  call vert_ZQ_A(gZd,ntry, wf4(:,6), wf8(:,10), wf32(:,6), n3(:,39), t3x32(:,:,6))
  call vert_VQ_A(ntry, wf4(:,9), ex5, wf8(:,13), n3(:,40), t3x8(:,:,7))
  call prop_Q_A(ntry, wf8(:,13), Q(:,28), MB, 1_intkind1, wf8(:,14), n2(21))
  call vert_VQ_A(ntry, ex8, wf8(:,14), wf16(:,29), n3(:,41), t3x16(:,:,21))
  call vert_ZQ_A(gZd,ntry, wf4(:,6), ex5, wf8(:,15), n3(:,42), t3x8(:,:,8))
  call prop_Q_A(ntry, wf8(:,15), Q(:,28), MB, 1_intkind1, wf8(:,16), n2(22))
  call vert_VQ_A(ntry, ex8, wf8(:,16), wf16(:,30), n3(:,43), t3x16(:,:,22))
  call vert_ZQ_A(gZd,ntry, wf4(:,5), wf8(:,14), wf32(:,7), n3(:,44), t3x32(:,:,7))
  call vert_ZQ_A(gZd,ntry, wf4(:,5), wf8(:,16), wf32(:,8), n3(:,45), t3x32(:,:,8))
  call vert_AQ_S(gH,ntry, wf8(:,12), ex5, wf16(:,31), n3(:,46), t3x16(:,:,23))
  call vert_VQ_A(ntry, ex7, wf4(:,12), wf8(:,17), n3(:,47), t3x8(:,:,9))
  call prop_Q_A(ntry, wf8(:,17), Q(:,208), MB, 1_intkind1, wf8(:,18), n2(23))
  call prop_Q_A(ntry, wf16(:,23), Q(:,156), MB, 1_intkind1, wf16(:,32), n2(24))
  call vert_AV_Q(ntry, wf8(:,3), ex7, wf16(:,33), n3(:,48), t3x16(:,:,24))
  call prop_Q_A(ntry, wf16(:,25), Q(:,156), MB, 1_intkind1, wf16(:,34), n2(25))
  call vert_AV_Q(ntry, wf8(:,6), ex7, wf16(:,35), n3(:,49), t3x16(:,:,25))
  call vert_AV_Q(ntry, wf8(:,8), ex7, wf16(:,36), n3(:,50), t3x16(:,:,26))
  call vert_AQ_S(gH,ntry, ex6, wf8(:,18), wf16(:,37), n3(:,51), t3x16(:,:,27))
  call prop_A_Q(ntry, wf16(:,4), Q(:,172), MB, 1_intkind1, wf16(:,38), n2(26))
  call vert_VQ_A(ntry, ex7, wf8(:,10), wf16(:,39), n3(:,52), t3x16(:,:,28))
  call prop_A_Q(ntry, wf16(:,6), Q(:,172), MB, 1_intkind1, wf16(:,40), n2(27))
  call vert_AV_Q(ntry, wf4(:,8), ex7, wf8(:,19), n3(:,53), t3x8(:,:,10))
  call prop_A_Q(ntry, wf8(:,19), Q(:,224), MB, 1_intkind1, wf8(:,20), n2(28))
  call vert_VQ_A(ntry, ex7, wf8(:,14), wf16(:,41), n3(:,54), t3x16(:,:,29))
  call vert_VQ_A(ntry, ex7, wf8(:,16), wf16(:,42), n3(:,55), t3x16(:,:,30))
  call vert_AQ_S(gH,ntry, wf8(:,20), ex5, wf16(:,43), n3(:,56), t3x16(:,:,31))
  call vert_UV_W(ntry, ex7, Q(:,64), ex8, Q(:,128), wf4(:,14), n3(:,57), t3x4(:,:,8))
  call vert_QA_V(ntry, wf8(:,10), wf8(:,6), wf64(:,1), n3(:,58), t3x64(:,:,1))
  call vert_QA_V(ntry, wf8(:,10), wf8(:,8), wf64(:,2), n3(:,59), t3x64(:,:,2))
  call vert_AV_Q(ntry, ex6, wf4(:,14), wf8(:,21), n3(:,60), t3x8(:,:,11))
  call prop_A_Q(ntry, wf8(:,21), Q(:,224), MB, 1_intkind1, wf8(:,22), n2(29))
  call vert_QA_V(ntry, wf8(:,14), wf8(:,3), wf64(:,3), n3(:,61), t3x64(:,:,3))
  call vert_QA_V(ntry, wf8(:,16), wf8(:,3), wf64(:,4), n3(:,62), t3x64(:,:,4))
  call vert_VQ_A(ntry, wf4(:,14), ex5, wf8(:,23), n3(:,63), t3x8(:,:,12))
  call prop_Q_A(ntry, wf8(:,23), Q(:,208), MB, 1_intkind1, wf8(:,24), n2(30))
  call vert_VQ_A(ntry, wf4(:,9), wf8(:,24), wf32(:,9), n3(:,64), t3x32(:,:,9))
  call vert_ZQ_A(gZd,ntry, wf4(:,6), wf8(:,24), wf32(:,10), n3(:,65), t3x32(:,:,10))
  call vert_ZQ_A(gZd,ntry, wf4(:,5), wf8(:,24), wf32(:,11), n3(:,66), t3x32(:,:,11))
  call vert_AQ_S(gH,ntry, ex6, wf8(:,24), wf16(:,44), n3(:,67), t3x16(:,:,32))
  call vert_AQ_S(gH,ntry, wf8(:,22), ex5, wf16(:,45), n3(:,68), t3x16(:,:,33))
  call prop_Q_A(ntry, wf16(:,39), Q(:,83), MB, 1_intkind1, wf16(:,46), n2(31))
  call prop_A_Q(ntry, wf16(:,35), Q(:,108), MB, 1_intkind1, wf16(:,47), n2(32))
  call prop_A_Q(ntry, wf16(:,36), Q(:,108), MB, 1_intkind1, wf16(:,48), n2(33))
  call prop_Q_A(ntry, wf16(:,41), Q(:,92), MB, 1_intkind1, wf16(:,49), n2(34))
  call prop_Q_A(ntry, wf16(:,42), Q(:,92), MB, 1_intkind1, wf16(:,50), n2(35))
  call prop_A_Q(ntry, wf16(:,33), Q(:,99), MB, 1_intkind1, wf16(:,51), n2(36))
  call vert_ZQ_A(gZl,ntry, wf4(:,5), ex3, wf8(:,25), n3(:,69), t3x8(:,:,13))
  call vert_QA_V(ntry, wf4(:,7), wf4(:,8), wf16(:,52), n3(:,70), t3x16(:,:,34))
  call prop_Q_A(ntry, wf8(:,25), Q(:,7), ZERO, 0_intkind1, wf8(:,26), n2(37))
  call vert_QA_V(ntry, wf8(:,26), ex4, wf16(:,53), n3(:,71), t3x16(:,:,35))
  call vert_QA_Z(gZd,ntry, wf4(:,7), wf4(:,8), wf16(:,54), n3(:,72), t3x16(:,:,36))
  call prop_W_W(ntry, wf16(:,54), Q(:,240), MZ, 1_intkind1, wf16(:,55), n2(38))
  call vert_QA_Z(gZl,ntry, wf8(:,26), ex4, wf16(:,56), n3(:,73), t3x16(:,:,37))
  call vert_AZ_Q(gZl,ntry, ex4, wf4(:,5), wf8(:,27), n3(:,74), t3x8(:,:,14))
  call prop_A_Q(ntry, wf8(:,27), Q(:,11), ZERO, 0_intkind1, wf8(:,28), n2(39))
  call vert_QA_V(ntry, ex3, wf8(:,28), wf16(:,57), n3(:,75), t3x16(:,:,38))
  call vert_QA_Z(gZl,ntry, ex3, wf8(:,28), wf16(:,58), n3(:,76), t3x16(:,:,39))
  call vert_QA_V(ntry, wf8(:,4), ex6, wf16(:,59), n3(:,77), t3x16(:,:,40))
  call vert_QA_Z(gZd,ntry, wf8(:,4), ex6, wf16(:,60), n3(:,78), t3x16(:,:,41))
  call prop_W_W(ntry, wf16(:,56), Q(:,15), MZ, 1_intkind1, wf16(:,61), n2(40))
  call prop_W_W(ntry, wf16(:,58), Q(:,15), MZ, 1_intkind1, wf16(:,62), n2(41))
  call vert_QA_V(ntry, wf4(:,12), wf4(:,13), wf16(:,63), n3(:,79), t3x16(:,:,42))
  call vert_QA_Z(gZd,ntry, wf4(:,12), wf4(:,13), wf16(:,64), n3(:,80), t3x16(:,:,43))
  call prop_W_W(ntry, wf16(:,64), Q(:,240), MZ, 1_intkind1, wf16(:,65), n2(42))
  call vert_QA_V(ntry, ex5, wf8(:,12), wf16(:,66), n3(:,81), t3x16(:,:,44))
  call vert_QA_Z(gZd,ntry, ex5, wf8(:,12), wf16(:,67), n3(:,82), t3x16(:,:,45))
  call vert_QA_V(ntry, wf8(:,18), ex6, wf16(:,68), n3(:,83), t3x16(:,:,46))
  call vert_QA_Z(gZd,ntry, wf8(:,18), ex6, wf16(:,69), n3(:,84), t3x16(:,:,47))
  call vert_QA_V(ntry, ex5, wf8(:,20), wf16(:,70), n3(:,85), t3x16(:,:,48))
  call vert_QA_Z(gZd,ntry, ex5, wf8(:,20), wf16(:,71), n3(:,86), t3x16(:,:,49))
  call vert_QA_V(ntry, ex5, wf8(:,22), wf16(:,72), n3(:,87), t3x16(:,:,50))
  call vert_QA_Z(gZd,ntry, ex5, wf8(:,22), wf16(:,73), n3(:,88), t3x16(:,:,51))
  call vert_QA_V(ntry, wf8(:,24), ex6, wf16(:,74), n3(:,89), t3x16(:,:,52))
  call vert_QA_Z(gZd,ntry, wf8(:,24), ex6, wf16(:,75), n3(:,90), t3x16(:,:,53))
  call vert_QA_W(ntry, ex1, ex4, wf4(:,15), n3(:,91), t3x4(:,:,9))
  call vert_QA_W(ntry, ex3, ex2, wf4(:,16), n3(:,92), t3x4(:,:,10))
  call prop_W_W(ntry, wf4(:,15), Q(:,9), MW, 1_intkind1, wf4(:,17), n2(43))
  call prop_W_W(ntry, wf4(:,16), Q(:,6), MW, 1_intkind1, wf4(:,18), n2(44))
  call vert_VV_S(ntry, wf4(:,18), wf4(:,17), wf16(:,76), n3(:,93), t3x16(:,:,54))
  call vert_UV_W(ntry, wf4(:,17), Q(:,9), wf4(:,18), Q(:,6), wf16(:,77), n3(:,94), t3x16(:,:,55))
  call prop_W_W(ntry, wf16(:,77), Q(:,15), MZ, 1_intkind1, wf16(:,78), n2(45))
  call vert_WQ_A(ntry, wf4(:,17), wf4(:,7), wf16(:,79), n3(:,95), t3x16(:,:,56))
  call vert_AW_Q(ntry, wf4(:,8), wf4(:,18), wf16(:,80), n3(:,96), t3x16(:,:,57))
  call prop_Q_A(ntry, wf16(:,79), Q(:,89), MT, 1_intkind1, wf16(:,81), n2(46))
  call vert_AW_Q(ntry, ex6, wf4(:,18), wf8(:,29), n3(:,97), t3x8(:,:,15))
  call prop_A_Q(ntry, wf8(:,29), Q(:,38), MT, 1_intkind1, wf8(:,30), n2(47))
  call vert_AW_Q(ntry, wf8(:,30), wf4(:,17), wf32(:,12), n3(:,98), t3x32(:,:,12))
  call vert_AV_Q(ntry, wf8(:,30), ex8, wf16(:,82), n3(:,99), t3x16(:,:,58))
  call vert_WQ_A(ntry, wf4(:,17), wf4(:,12), wf16(:,83), n3(:,100), t3x16(:,:,59))
  call vert_AW_Q(ntry, wf4(:,13), wf4(:,18), wf16(:,84), n3(:,101), t3x16(:,:,60))
  call prop_Q_A(ntry, wf16(:,83), Q(:,153), MT, 1_intkind1, wf16(:,85), n2(48))
  call vert_WQ_A(ntry, wf4(:,17), ex5, wf8(:,31), n3(:,102), t3x8(:,:,16))
  call prop_Q_A(ntry, wf8(:,31), Q(:,25), MT, 1_intkind1, wf8(:,32), n2(49))
  call prop_A_Q(ntry, wf16(:,84), Q(:,102), MT, 1_intkind1, wf16(:,86), n2(50))
  call vert_VQ_A(ntry, ex8, wf8(:,32), wf16(:,87), n3(:,103), t3x16(:,:,61))
  call vert_WQ_A(ntry, wf4(:,18), wf8(:,32), wf32(:,13), n3(:,104), t3x32(:,:,13))
  call vert_AV_Q(ntry, wf8(:,30), ex7, wf16(:,88), n3(:,105), t3x16(:,:,62))
  call prop_A_Q(ntry, wf16(:,80), Q(:,166), MT, 1_intkind1, wf16(:,89), n2(51))
  call vert_VQ_A(ntry, ex7, wf8(:,32), wf16(:,90), n3(:,106), t3x16(:,:,63))
  call vert_QA_V(ntry, wf8(:,32), wf8(:,30), wf64(:,5), n3(:,107), t3x64(:,:,5))
  call vert_WQ_A(ntry, wf4(:,17), wf8(:,24), wf32(:,14), n3(:,108), t3x32(:,:,14))
  call prop_Q_A(ntry, wf16(:,90), Q(:,89), MT, 1_intkind1, wf16(:,91), n2(52))
  call prop_A_Q(ntry, wf16(:,88), Q(:,102), MT, 1_intkind1, wf16(:,92), n2(53))
  call vert_WQ_A(ntry, wf4(:,18), ex1, wf8(:,33), n3(:,109), t3x8(:,:,17))
  call prop_Q_A(ntry, wf8(:,33), Q(:,7), ZERO, 0_intkind1, wf8(:,34), n2(54))
  call vert_QA_V(ntry, wf8(:,34), ex4, wf16(:,93), n3(:,110), t3x16(:,:,64))
  call vert_QA_Z(gZl,ntry, wf8(:,34), ex4, wf16(:,94), n3(:,111), t3x16(:,:,65))
  call vert_AW_Q(ntry, ex4, wf4(:,18), wf8(:,35), n3(:,112), t3x8(:,:,18))
  call prop_A_Q(ntry, wf8(:,35), Q(:,14), ZERO, 0_intkind1, wf8(:,36), n2(55))
  call vert_QA_Z(gZn,ntry, ex1, wf8(:,36), wf16(:,95), n3(:,113), t3x16(:,:,66))
  call prop_W_W(ntry, wf16(:,94), Q(:,15), MZ, 1_intkind1, wf16(:,96), n2(56))
  call prop_W_W(ntry, wf16(:,95), Q(:,15), MZ, 1_intkind1, wf16(:,97), n2(57))
  call vert_AW_Q(ntry, ex2, wf4(:,17), wf8(:,37), n3(:,114), t3x8(:,:,19))
  call prop_A_Q(ntry, wf8(:,37), Q(:,11), ZERO, 0_intkind1, wf8(:,38), n2(58))
  call vert_QA_V(ntry, ex3, wf8(:,38), wf16(:,98), n3(:,115), t3x16(:,:,67))
  call vert_QA_Z(gZl,ntry, ex3, wf8(:,38), wf16(:,99), n3(:,116), t3x16(:,:,68))
  call vert_WQ_A(ntry, wf4(:,17), ex3, wf8(:,39), n3(:,117), t3x8(:,:,20))
  call prop_Q_A(ntry, wf8(:,39), Q(:,13), ZERO, 0_intkind1, wf8(:,40), n2(59))
  call vert_QA_Z(gZn,ntry, wf8(:,40), ex2, wf16(:,100), n3(:,118), t3x16(:,:,69))
  call prop_W_W(ntry, wf16(:,99), Q(:,15), MZ, 1_intkind1, wf16(:,101), n2(60))
  call prop_W_W(ntry, wf16(:,100), Q(:,15), MZ, 1_intkind1, wf16(:,102), n2(61))
  call vert_ZQ_A(gZn,ntry, wf4(:,6), ex1, wf8(:,41), n3(:,119), t3x8(:,:,21))
  call prop_Q_A(ntry, wf8(:,41), Q(:,13), ZERO, 0_intkind1, wf8(:,42), n2(62))
  call vert_QA_Z(gZn,ntry, wf8(:,42), ex2, wf16(:,103), n3(:,120), t3x16(:,:,70))
  call vert_AZ_Q(gZn,ntry, ex2, wf4(:,6), wf8(:,43), n3(:,121), t3x8(:,:,22))
  call prop_A_Q(ntry, wf8(:,43), Q(:,14), ZERO, 0_intkind1, wf8(:,44), n2(63))
  call vert_QA_Z(gZn,ntry, ex1, wf8(:,44), wf16(:,104), n3(:,122), t3x16(:,:,71))
  call prop_W_W(ntry, wf16(:,103), Q(:,15), MZ, 1_intkind1, wf16(:,105), n2(64))
  call prop_W_W(ntry, wf16(:,104), Q(:,15), MZ, 1_intkind1, wf16(:,106), n2(65))


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
  M2add = M2 / average_factor_ppllllj_nenexeexbbxgg_1

  do k = 0, 38-1
    M2(k) = M2add(extcomb_perm_ppllllj_nenexeexbbxgg_1(k))
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

    call cont_SS(nsync, wf16(:,1), wf16(:,2), A(:,1), n3(:,123), t3x256(:,:,1), nhel, den(9))
    call cont_QA(nsync, wf16(:,4), wf16(:,5), A(:,2), n3(:,124), t3x256(:,:,2), nhel, den(15))
    call cont_QA(nsync, wf16(:,5), wf16(:,6), A(:,3), n3(:,125), t3x256(:,:,3), nhel, den(17))
    call cont_QA(nsync, wf16(:,8), wf16(:,9), A(:,4), n3(:,126), t3x256(:,:,4), nhel, den(22))
    call cont_QA(nsync, wf16(:,9), wf16(:,10), A(:,5), n3(:,127), t3x256(:,:,5), nhel, den(24))
    call cont_QA(nsync, wf8(:,4), wf32(:,1), A(:,6), n3(:,128), t3x256(:,:,6), nhel, den(30))
    call cont_QA(nsync, wf8(:,4), wf32(:,2), A(:,7), n3(:,129), t3x256(:,:,7), nhel, den(32))
    call cont_QA(nsync, wf16(:,11), wf16(:,12), A(:,8), n3(:,130), t3x256(:,:,8), nhel, den(35))
    call cont_QA(nsync, wf16(:,12), wf16(:,13), A(:,9), n3(:,131), t3x256(:,:,9), nhel, den(37))
    call cont_QA(nsync, wf8(:,4), wf32(:,3), A(:,10), n3(:,132), t3x256(:,:,10), nhel, den(41))
    call cont_QA(nsync, wf8(:,4), wf32(:,4), A(:,11), n3(:,133), t3x256(:,:,11), nhel, den(44))
    call cont_QA(nsync, wf16(:,5), wf16(:,14), A(:,12), n3(:,134), t3x256(:,:,12), nhel, den(45))
    call cont_QA(nsync, wf16(:,5), wf16(:,15), A(:,13), n3(:,135), t3x256(:,:,13), nhel, den(46))
    call cont_SS(nsync, wf16(:,1), wf16(:,16), A(:,14), n3(:,136), t3x256(:,:,14), nhel, den(47))
    call cont_SS(nsync, wf16(:,1), wf16(:,17), A(:,15), n3(:,137), t3x256(:,:,15), nhel, den(51))
    call cont_QA(nsync, wf16(:,19), wf16(:,20), A(:,16), n3(:,138), t3x256(:,:,16), nhel, den(56))
    call cont_QA(nsync, wf16(:,20), wf16(:,21), A(:,17), n3(:,139), t3x256(:,:,17), nhel, den(58))
    call cont_QA(nsync, wf16(:,23), wf16(:,24), A(:,18), n3(:,140), t3x256(:,:,18), nhel, den(63))
    call cont_QA(nsync, wf16(:,24), wf16(:,25), A(:,19), n3(:,141), t3x256(:,:,19), nhel, den(65))
    call cont_QA(nsync, wf16(:,26), wf16(:,27), A(:,20), n3(:,142), t3x256(:,:,20), nhel, den(70))
    call cont_QA(nsync, wf16(:,27), wf16(:,28), A(:,21), n3(:,143), t3x256(:,:,21), nhel, den(72))
    call cont_QA(nsync, wf8(:,12), wf32(:,5), A(:,22), n3(:,144), t3x256(:,:,22), nhel, den(76))
    call cont_QA(nsync, wf8(:,12), wf32(:,6), A(:,23), n3(:,145), t3x256(:,:,23), nhel, den(78))
    call cont_QA(nsync, wf16(:,24), wf16(:,29), A(:,24), n3(:,146), t3x256(:,:,24), nhel, den(81))
    call cont_QA(nsync, wf16(:,24), wf16(:,30), A(:,25), n3(:,147), t3x256(:,:,25), nhel, den(83))
    call cont_QA(nsync, wf8(:,12), wf32(:,7), A(:,26), n3(:,148), t3x256(:,:,26), nhel, den(85))
    call cont_QA(nsync, wf8(:,12), wf32(:,8), A(:,27), n3(:,149), t3x256(:,:,27), nhel, den(87))
    call cont_SS(nsync, wf16(:,1), wf16(:,31), A(:,28), n3(:,150), t3x256(:,:,28), nhel, den(88))
    call cont_QA(nsync, wf32(:,1), wf8(:,18), A(:,29), n3(:,151), t3x256(:,:,29), nhel, den(90))
    call cont_QA(nsync, wf32(:,2), wf8(:,18), A(:,30), n3(:,152), t3x256(:,:,30), nhel, den(91))
    call cont_QA(nsync, wf16(:,32), wf16(:,33), A(:,31), n3(:,153), t3x256(:,:,31), nhel, den(94))
    call cont_QA(nsync, wf16(:,33), wf16(:,34), A(:,32), n3(:,154), t3x256(:,:,32), nhel, den(96))
    call cont_QA(nsync, wf32(:,3), wf8(:,18), A(:,33), n3(:,155), t3x256(:,:,33), nhel, den(97))
    call cont_QA(nsync, wf32(:,4), wf8(:,18), A(:,34), n3(:,156), t3x256(:,:,34), nhel, den(98))
    call cont_QA(nsync, wf16(:,20), wf16(:,35), A(:,35), n3(:,157), t3x256(:,:,35), nhel, den(99))
    call cont_QA(nsync, wf16(:,20), wf16(:,36), A(:,36), n3(:,158), t3x256(:,:,36), nhel, den(100))
    call cont_SS(nsync, wf16(:,1), wf16(:,37), A(:,37), n3(:,159), t3x256(:,:,37), nhel, den(101))
    call cont_QA(nsync, wf16(:,38), wf16(:,39), A(:,38), n3(:,160), t3x256(:,:,38), nhel, den(104))
    call cont_QA(nsync, wf16(:,39), wf16(:,40), A(:,39), n3(:,161), t3x256(:,:,39), nhel, den(106))
    call cont_QA(nsync, wf32(:,5), wf8(:,20), A(:,40), n3(:,162), t3x256(:,:,40), nhel, den(108))
    call cont_QA(nsync, wf32(:,6), wf8(:,20), A(:,41), n3(:,163), t3x256(:,:,41), nhel, den(109))
    call cont_QA(nsync, wf16(:,9), wf16(:,41), A(:,42), n3(:,164), t3x256(:,:,42), nhel, den(110))
    call cont_QA(nsync, wf16(:,9), wf16(:,42), A(:,43), n3(:,165), t3x256(:,:,43), nhel, den(111))
    call cont_QA(nsync, wf32(:,7), wf8(:,20), A(:,44), n3(:,166), t3x256(:,:,44), nhel, den(112))
    call cont_QA(nsync, wf32(:,8), wf8(:,20), A(:,45), n3(:,167), t3x256(:,:,45), nhel, den(113))
    call cont_SS(nsync, wf16(:,1), wf16(:,43), A(:,46), n3(:,168), t3x256(:,:,46), nhel, den(114))
    call cont_VV(nsync, wf4(:,14), wf64(:,1), A(:,47), n3(:,169), t3x256(:,:,47), nhel, den(117))
    call cont_VV(nsync, wf4(:,14), wf64(:,2), A(:,48), n3(:,170), t3x256(:,:,48), nhel, den(119))
    call cont_QA(nsync, wf32(:,5), wf8(:,22), A(:,49), n3(:,171), t3x256(:,:,49), nhel, den(121))
    call cont_QA(nsync, wf32(:,6), wf8(:,22), A(:,50), n3(:,172), t3x256(:,:,50), nhel, den(122))
    call cont_VV(nsync, wf4(:,14), wf64(:,3), A(:,51), n3(:,173), t3x256(:,:,51), nhel, den(124))
    call cont_VV(nsync, wf4(:,14), wf64(:,4), A(:,52), n3(:,174), t3x256(:,:,52), nhel, den(126))
    call cont_QA(nsync, wf8(:,3), wf32(:,9), A(:,53), n3(:,175), t3x256(:,:,53), nhel, den(129))
    call cont_QA(nsync, wf8(:,3), wf32(:,10), A(:,54), n3(:,176), t3x256(:,:,54), nhel, den(131))
    call cont_QA(nsync, wf32(:,7), wf8(:,22), A(:,55), n3(:,177), t3x256(:,:,55), nhel, den(132))
    call cont_QA(nsync, wf32(:,8), wf8(:,22), A(:,56), n3(:,178), t3x256(:,:,56), nhel, den(133))
    call cont_QA(nsync, wf8(:,6), wf32(:,11), A(:,57), n3(:,179), t3x256(:,:,57), nhel, den(135))
    call cont_QA(nsync, wf8(:,8), wf32(:,11), A(:,58), n3(:,180), t3x256(:,:,58), nhel, den(136))
    call cont_SS(nsync, wf16(:,1), wf16(:,44), A(:,59), n3(:,181), t3x256(:,:,59), nhel, den(137))
    call cont_SS(nsync, wf16(:,1), wf16(:,45), A(:,60), n3(:,182), t3x256(:,:,60), nhel, den(138))
    call cont_QA(nsync, wf16(:,14), wf16(:,46), A(:,61), n3(:,183), t3x256(:,:,61), nhel, den(140))
    call cont_QA(nsync, wf16(:,15), wf16(:,46), A(:,62), n3(:,184), t3x256(:,:,62), nhel, den(141))
    call cont_QA(nsync, wf16(:,27), wf16(:,47), A(:,63), n3(:,185), t3x256(:,:,63), nhel, den(143))
    call cont_QA(nsync, wf16(:,27), wf16(:,48), A(:,64), n3(:,186), t3x256(:,:,64), nhel, den(145))
    call cont_QA(nsync, wf16(:,12), wf16(:,49), A(:,65), n3(:,187), t3x256(:,:,65), nhel, den(147))
    call cont_QA(nsync, wf16(:,12), wf16(:,50), A(:,66), n3(:,188), t3x256(:,:,66), nhel, den(149))
    call cont_QA(nsync, wf16(:,29), wf16(:,51), A(:,67), n3(:,189), t3x256(:,:,67), nhel, den(151))
    call cont_QA(nsync, wf16(:,30), wf16(:,51), A(:,68), n3(:,190), t3x256(:,:,68), nhel, den(152))
    call cont_VV(nsync, wf16(:,52), wf16(:,53), A(:,69), n3(:,191), t3x256(:,:,69), nhel, den(157))
    call cont_VV(nsync, wf16(:,55), wf16(:,56), A(:,70), n3(:,192), t3x256(:,:,70), nhel, den(160))
    call cont_VV(nsync, wf16(:,52), wf16(:,57), A(:,71), n3(:,193), t3x256(:,:,71), nhel, den(163))
    call cont_VV(nsync, wf16(:,55), wf16(:,58), A(:,72), n3(:,194), t3x256(:,:,72), nhel, den(164))
    call cont_VV(nsync, wf16(:,53), wf16(:,59), A(:,73), n3(:,195), t3x256(:,:,73), nhel, den(167))
    call cont_VV(nsync, wf16(:,60), wf16(:,61), A(:,74), n3(:,196), t3x256(:,:,74), nhel, den(170))
    call cont_VV(nsync, wf16(:,57), wf16(:,59), A(:,75), n3(:,197), t3x256(:,:,75), nhel, den(172))
    call cont_VV(nsync, wf16(:,60), wf16(:,62), A(:,76), n3(:,198), t3x256(:,:,76), nhel, den(174))
    call cont_VV(nsync, wf16(:,53), wf16(:,63), A(:,77), n3(:,199), t3x256(:,:,77), nhel, den(176))
    call cont_VV(nsync, wf16(:,56), wf16(:,65), A(:,78), n3(:,200), t3x256(:,:,78), nhel, den(178))
    call cont_VV(nsync, wf16(:,57), wf16(:,63), A(:,79), n3(:,201), t3x256(:,:,79), nhel, den(179))
    call cont_VV(nsync, wf16(:,58), wf16(:,65), A(:,80), n3(:,202), t3x256(:,:,80), nhel, den(180))
    call cont_VV(nsync, wf16(:,53), wf16(:,66), A(:,81), n3(:,203), t3x256(:,:,81), nhel, den(181))
    call cont_VV(nsync, wf16(:,61), wf16(:,67), A(:,82), n3(:,204), t3x256(:,:,82), nhel, den(182))
    call cont_VV(nsync, wf16(:,57), wf16(:,66), A(:,83), n3(:,205), t3x256(:,:,83), nhel, den(183))
    call cont_VV(nsync, wf16(:,62), wf16(:,67), A(:,84), n3(:,206), t3x256(:,:,84), nhel, den(184))
    call cont_VV(nsync, wf16(:,53), wf16(:,68), A(:,85), n3(:,207), t3x256(:,:,85), nhel, den(185))
    call cont_VV(nsync, wf16(:,61), wf16(:,69), A(:,86), n3(:,208), t3x256(:,:,86), nhel, den(186))
    call cont_VV(nsync, wf16(:,57), wf16(:,68), A(:,87), n3(:,209), t3x256(:,:,87), nhel, den(187))
    call cont_VV(nsync, wf16(:,62), wf16(:,69), A(:,88), n3(:,210), t3x256(:,:,88), nhel, den(188))
    call cont_VV(nsync, wf16(:,53), wf16(:,70), A(:,89), n3(:,211), t3x256(:,:,89), nhel, den(189))
    call cont_VV(nsync, wf16(:,61), wf16(:,71), A(:,90), n3(:,212), t3x256(:,:,90), nhel, den(190))
    call cont_VV(nsync, wf16(:,57), wf16(:,70), A(:,91), n3(:,213), t3x256(:,:,91), nhel, den(191))
    call cont_VV(nsync, wf16(:,62), wf16(:,71), A(:,92), n3(:,214), t3x256(:,:,92), nhel, den(192))
    call cont_VV(nsync, wf16(:,53), wf16(:,72), A(:,93), n3(:,215), t3x256(:,:,93), nhel, den(193))
    call cont_VV(nsync, wf16(:,61), wf16(:,73), A(:,94), n3(:,216), t3x256(:,:,94), nhel, den(194))
    call cont_VV(nsync, wf16(:,53), wf16(:,74), A(:,95), n3(:,217), t3x256(:,:,95), nhel, den(195))
    call cont_VV(nsync, wf16(:,61), wf16(:,75), A(:,96), n3(:,218), t3x256(:,:,96), nhel, den(196))
    call cont_VV(nsync, wf16(:,57), wf16(:,72), A(:,97), n3(:,219), t3x256(:,:,97), nhel, den(197))
    call cont_VV(nsync, wf16(:,62), wf16(:,73), A(:,98), n3(:,220), t3x256(:,:,98), nhel, den(198))
    call cont_VV(nsync, wf16(:,57), wf16(:,74), A(:,99), n3(:,221), t3x256(:,:,99), nhel, den(199))
    call cont_VV(nsync, wf16(:,62), wf16(:,75), A(:,100), n3(:,222), t3x256(:,:,100), nhel, den(200))
    call cont_SS(nsync, wf16(:,2), wf16(:,76), A(:,101), n3(:,223), t3x256(:,:,101), nhel, den(205))
    call cont_VV(nsync, wf16(:,52), wf16(:,77), A(:,102), n3(:,224), t3x256(:,:,102), nhel, den(207))
    call cont_VV(nsync, wf16(:,54), wf16(:,78), A(:,103), n3(:,225), t3x256(:,:,103), nhel, den(209))
    call cont_QA(nsync, wf16(:,80), wf16(:,81), A(:,104), n3(:,226), t3x256(:,:,104), nhel, den(214))
    call cont_QA(nsync, wf8(:,4), wf32(:,12), A(:,105), n3(:,227), t3x256(:,:,105), nhel, den(218))
    call cont_QA(nsync, wf16(:,81), wf16(:,82), A(:,106), n3(:,228), t3x256(:,:,106), nhel, den(219))
    call cont_SS(nsync, wf16(:,16), wf16(:,76), A(:,107), n3(:,229), t3x256(:,:,107), nhel, den(220))
    call cont_VV(nsync, wf16(:,59), wf16(:,77), A(:,108), n3(:,230), t3x256(:,:,108), nhel, den(221))
    call cont_VV(nsync, wf16(:,60), wf16(:,78), A(:,109), n3(:,231), t3x256(:,:,109), nhel, den(222))
    call cont_SS(nsync, wf16(:,17), wf16(:,76), A(:,110), n3(:,232), t3x256(:,:,110), nhel, den(223))
    call cont_VV(nsync, wf16(:,63), wf16(:,77), A(:,111), n3(:,233), t3x256(:,:,111), nhel, den(224))
    call cont_VV(nsync, wf16(:,64), wf16(:,78), A(:,112), n3(:,234), t3x256(:,:,112), nhel, den(225))
    call cont_QA(nsync, wf16(:,84), wf16(:,85), A(:,113), n3(:,235), t3x256(:,:,113), nhel, den(230))
    call cont_QA(nsync, wf16(:,86), wf16(:,87), A(:,114), n3(:,236), t3x256(:,:,114), nhel, den(235))
    call cont_QA(nsync, wf8(:,12), wf32(:,13), A(:,115), n3(:,237), t3x256(:,:,115), nhel, den(237))
    call cont_SS(nsync, wf16(:,31), wf16(:,76), A(:,116), n3(:,238), t3x256(:,:,116), nhel, den(238))
    call cont_VV(nsync, wf16(:,66), wf16(:,77), A(:,117), n3(:,239), t3x256(:,:,117), nhel, den(239))
    call cont_VV(nsync, wf16(:,67), wf16(:,78), A(:,118), n3(:,240), t3x256(:,:,118), nhel, den(240))
    call cont_QA(nsync, wf8(:,18), wf32(:,12), A(:,119), n3(:,241), t3x256(:,:,119), nhel, den(241))
    call cont_QA(nsync, wf16(:,85), wf16(:,88), A(:,120), n3(:,242), t3x256(:,:,120), nhel, den(242))
    call cont_SS(nsync, wf16(:,37), wf16(:,76), A(:,121), n3(:,243), t3x256(:,:,121), nhel, den(243))
    call cont_VV(nsync, wf16(:,68), wf16(:,77), A(:,122), n3(:,244), t3x256(:,:,122), nhel, den(244))
    call cont_VV(nsync, wf16(:,69), wf16(:,78), A(:,123), n3(:,245), t3x256(:,:,123), nhel, den(245))
    call cont_QA(nsync, wf16(:,89), wf16(:,90), A(:,124), n3(:,246), t3x256(:,:,124), nhel, den(248))
    call cont_QA(nsync, wf8(:,20), wf32(:,13), A(:,125), n3(:,247), t3x256(:,:,125), nhel, den(249))
    call cont_SS(nsync, wf16(:,43), wf16(:,76), A(:,126), n3(:,248), t3x256(:,:,126), nhel, den(250))
    call cont_VV(nsync, wf16(:,70), wf16(:,77), A(:,127), n3(:,249), t3x256(:,:,127), nhel, den(251))
    call cont_VV(nsync, wf16(:,71), wf16(:,78), A(:,128), n3(:,250), t3x256(:,:,128), nhel, den(252))
    call cont_VV(nsync, wf4(:,14), wf64(:,5), A(:,129), n3(:,251), t3x256(:,:,129), nhel, den(254))
    call cont_QA(nsync, wf8(:,22), wf32(:,13), A(:,130), n3(:,252), t3x256(:,:,130), nhel, den(255))
    call cont_QA(nsync, wf8(:,30), wf32(:,14), A(:,131), n3(:,253), t3x256(:,:,131), nhel, den(257))
    call cont_SS(nsync, wf16(:,44), wf16(:,76), A(:,132), n3(:,254), t3x256(:,:,132), nhel, den(258))
    call cont_VV(nsync, wf16(:,74), wf16(:,77), A(:,133), n3(:,255), t3x256(:,:,133), nhel, den(259))
    call cont_VV(nsync, wf16(:,75), wf16(:,78), A(:,134), n3(:,256), t3x256(:,:,134), nhel, den(260))
    call cont_SS(nsync, wf16(:,45), wf16(:,76), A(:,135), n3(:,257), t3x256(:,:,135), nhel, den(261))
    call cont_VV(nsync, wf16(:,72), wf16(:,77), A(:,136), n3(:,258), t3x256(:,:,136), nhel, den(262))
    call cont_VV(nsync, wf16(:,73), wf16(:,78), A(:,137), n3(:,259), t3x256(:,:,137), nhel, den(263))
    call cont_QA(nsync, wf16(:,82), wf16(:,91), A(:,138), n3(:,260), t3x256(:,:,138), nhel, den(265))
    call cont_QA(nsync, wf16(:,87), wf16(:,92), A(:,139), n3(:,261), t3x256(:,:,139), nhel, den(267))
    call cont_VV(nsync, wf16(:,52), wf16(:,93), A(:,140), n3(:,262), t3x256(:,:,140), nhel, den(269))
    call cont_VV(nsync, wf16(:,55), wf16(:,94), A(:,141), n3(:,263), t3x256(:,:,141), nhel, den(270))
    call cont_VV(nsync, wf16(:,55), wf16(:,95), A(:,142), n3(:,264), t3x256(:,:,142), nhel, den(273))
    call cont_VV(nsync, wf16(:,59), wf16(:,93), A(:,143), n3(:,265), t3x256(:,:,143), nhel, den(275))
    call cont_VV(nsync, wf16(:,60), wf16(:,96), A(:,144), n3(:,266), t3x256(:,:,144), nhel, den(277))
    call cont_VV(nsync, wf16(:,60), wf16(:,97), A(:,145), n3(:,267), t3x256(:,:,145), nhel, den(279))
    call cont_VV(nsync, wf16(:,63), wf16(:,93), A(:,146), n3(:,268), t3x256(:,:,146), nhel, den(280))
    call cont_VV(nsync, wf16(:,65), wf16(:,94), A(:,147), n3(:,269), t3x256(:,:,147), nhel, den(281))
    call cont_VV(nsync, wf16(:,65), wf16(:,95), A(:,148), n3(:,270), t3x256(:,:,148), nhel, den(282))
    call cont_VV(nsync, wf16(:,66), wf16(:,93), A(:,149), n3(:,271), t3x256(:,:,149), nhel, den(283))
    call cont_VV(nsync, wf16(:,67), wf16(:,96), A(:,150), n3(:,272), t3x256(:,:,150), nhel, den(284))
    call cont_VV(nsync, wf16(:,67), wf16(:,97), A(:,151), n3(:,273), t3x256(:,:,151), nhel, den(285))
    call cont_VV(nsync, wf16(:,68), wf16(:,93), A(:,152), n3(:,274), t3x256(:,:,152), nhel, den(286))
    call cont_VV(nsync, wf16(:,69), wf16(:,96), A(:,153), n3(:,275), t3x256(:,:,153), nhel, den(287))
    call cont_VV(nsync, wf16(:,69), wf16(:,97), A(:,154), n3(:,276), t3x256(:,:,154), nhel, den(288))
    call cont_VV(nsync, wf16(:,70), wf16(:,93), A(:,155), n3(:,277), t3x256(:,:,155), nhel, den(289))
    call cont_VV(nsync, wf16(:,71), wf16(:,96), A(:,156), n3(:,278), t3x256(:,:,156), nhel, den(290))
    call cont_VV(nsync, wf16(:,71), wf16(:,97), A(:,157), n3(:,279), t3x256(:,:,157), nhel, den(291))
    call cont_VV(nsync, wf16(:,72), wf16(:,93), A(:,158), n3(:,280), t3x256(:,:,158), nhel, den(292))
    call cont_VV(nsync, wf16(:,73), wf16(:,96), A(:,159), n3(:,281), t3x256(:,:,159), nhel, den(293))
    call cont_VV(nsync, wf16(:,74), wf16(:,93), A(:,160), n3(:,282), t3x256(:,:,160), nhel, den(294))
    call cont_VV(nsync, wf16(:,75), wf16(:,96), A(:,161), n3(:,283), t3x256(:,:,161), nhel, den(295))
    call cont_VV(nsync, wf16(:,73), wf16(:,97), A(:,162), n3(:,284), t3x256(:,:,162), nhel, den(296))
    call cont_VV(nsync, wf16(:,75), wf16(:,97), A(:,163), n3(:,285), t3x256(:,:,163), nhel, den(297))
    call cont_VV(nsync, wf16(:,52), wf16(:,98), A(:,164), n3(:,286), t3x256(:,:,164), nhel, den(299))
    call cont_VV(nsync, wf16(:,55), wf16(:,99), A(:,165), n3(:,287), t3x256(:,:,165), nhel, den(300))
    call cont_VV(nsync, wf16(:,55), wf16(:,100), A(:,166), n3(:,288), t3x256(:,:,166), nhel, den(303))
    call cont_VV(nsync, wf16(:,59), wf16(:,98), A(:,167), n3(:,289), t3x256(:,:,167), nhel, den(305))
    call cont_VV(nsync, wf16(:,60), wf16(:,101), A(:,168), n3(:,290), t3x256(:,:,168), nhel, den(307))
    call cont_VV(nsync, wf16(:,60), wf16(:,102), A(:,169), n3(:,291), t3x256(:,:,169), nhel, den(309))
    call cont_VV(nsync, wf16(:,63), wf16(:,98), A(:,170), n3(:,292), t3x256(:,:,170), nhel, den(310))
    call cont_VV(nsync, wf16(:,65), wf16(:,99), A(:,171), n3(:,293), t3x256(:,:,171), nhel, den(311))
    call cont_VV(nsync, wf16(:,65), wf16(:,100), A(:,172), n3(:,294), t3x256(:,:,172), nhel, den(312))
    call cont_VV(nsync, wf16(:,66), wf16(:,98), A(:,173), n3(:,295), t3x256(:,:,173), nhel, den(313))
    call cont_VV(nsync, wf16(:,67), wf16(:,101), A(:,174), n3(:,296), t3x256(:,:,174), nhel, den(314))
    call cont_VV(nsync, wf16(:,67), wf16(:,102), A(:,175), n3(:,297), t3x256(:,:,175), nhel, den(315))
    call cont_VV(nsync, wf16(:,68), wf16(:,98), A(:,176), n3(:,298), t3x256(:,:,176), nhel, den(316))
    call cont_VV(nsync, wf16(:,69), wf16(:,101), A(:,177), n3(:,299), t3x256(:,:,177), nhel, den(317))
    call cont_VV(nsync, wf16(:,69), wf16(:,102), A(:,178), n3(:,300), t3x256(:,:,178), nhel, den(318))
    call cont_VV(nsync, wf16(:,70), wf16(:,98), A(:,179), n3(:,301), t3x256(:,:,179), nhel, den(319))
    call cont_VV(nsync, wf16(:,71), wf16(:,101), A(:,180), n3(:,302), t3x256(:,:,180), nhel, den(320))
    call cont_VV(nsync, wf16(:,71), wf16(:,102), A(:,181), n3(:,303), t3x256(:,:,181), nhel, den(321))
    call cont_VV(nsync, wf16(:,72), wf16(:,98), A(:,182), n3(:,304), t3x256(:,:,182), nhel, den(322))
    call cont_VV(nsync, wf16(:,73), wf16(:,101), A(:,183), n3(:,305), t3x256(:,:,183), nhel, den(323))
    call cont_VV(nsync, wf16(:,74), wf16(:,98), A(:,184), n3(:,306), t3x256(:,:,184), nhel, den(324))
    call cont_VV(nsync, wf16(:,75), wf16(:,101), A(:,185), n3(:,307), t3x256(:,:,185), nhel, den(325))
    call cont_VV(nsync, wf16(:,73), wf16(:,102), A(:,186), n3(:,308), t3x256(:,:,186), nhel, den(326))
    call cont_VV(nsync, wf16(:,75), wf16(:,102), A(:,187), n3(:,309), t3x256(:,:,187), nhel, den(327))
    call cont_VV(nsync, wf16(:,55), wf16(:,103), A(:,188), n3(:,310), t3x256(:,:,188), nhel, den(329))
    call cont_VV(nsync, wf16(:,55), wf16(:,104), A(:,189), n3(:,311), t3x256(:,:,189), nhel, den(331))
    call cont_VV(nsync, wf16(:,60), wf16(:,105), A(:,190), n3(:,312), t3x256(:,:,190), nhel, den(333))
    call cont_VV(nsync, wf16(:,60), wf16(:,106), A(:,191), n3(:,313), t3x256(:,:,191), nhel, den(335))
    call cont_VV(nsync, wf16(:,65), wf16(:,103), A(:,192), n3(:,314), t3x256(:,:,192), nhel, den(336))
    call cont_VV(nsync, wf16(:,65), wf16(:,104), A(:,193), n3(:,315), t3x256(:,:,193), nhel, den(337))
    call cont_VV(nsync, wf16(:,67), wf16(:,105), A(:,194), n3(:,316), t3x256(:,:,194), nhel, den(338))
    call cont_VV(nsync, wf16(:,67), wf16(:,106), A(:,195), n3(:,317), t3x256(:,:,195), nhel, den(339))
    call cont_VV(nsync, wf16(:,69), wf16(:,105), A(:,196), n3(:,318), t3x256(:,:,196), nhel, den(340))
    call cont_VV(nsync, wf16(:,69), wf16(:,106), A(:,197), n3(:,319), t3x256(:,:,197), nhel, den(341))
    call cont_VV(nsync, wf16(:,71), wf16(:,105), A(:,198), n3(:,320), t3x256(:,:,198), nhel, den(342))
    call cont_VV(nsync, wf16(:,71), wf16(:,106), A(:,199), n3(:,321), t3x256(:,:,199), nhel, den(343))
    call cont_VV(nsync, wf16(:,73), wf16(:,105), A(:,200), n3(:,322), t3x256(:,:,200), nhel, den(344))
    call cont_VV(nsync, wf16(:,75), wf16(:,105), A(:,201), n3(:,323), t3x256(:,:,201), nhel, den(345))
    call cont_VV(nsync, wf16(:,73), wf16(:,106), A(:,202), n3(:,324), t3x256(:,:,202), nhel, den(346))
    call cont_VV(nsync, wf16(:,75), wf16(:,106), A(:,203), n3(:,325), t3x256(:,:,203), nhel, den(347))

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
  complex(REALKIND), intent(out) :: M1(2) ! M1helarray(2,256)
  integer :: empty(0)

  M1(1) = (A(j,16)%j+A(j,18)%j+A(j,20)%j+A(j,22)%j+A(j,24)%j+A(j,26)%j+A(j,29)%j+A(j,31)%j+A(j,33)%j+A(j,35)%j+A(j,63)%j+A(j,67)%j &
       +A(j,77)%j+A(j,79)%j+A(j,81)%j+A(j,83)%j+A(j,85)%j+A(j,87)%j)*f(1)+(A(j,17)%j+A(j,19)%j+A(j,21)%j+A(j,23)%j+A(j,25)%j &
       +A(j,27)%j+A(j,30)%j+A(j,32)%j+A(j,34)%j+A(j,36)%j+A(j,64)%j+A(j,68)%j+A(j,78)%j+A(j,80)%j+A(j,82)%j+A(j,84)%j+A(j,86)%j &
       +A(j,88)%j+A(j,192)%j+A(j,193)%j+A(j,194)%j+A(j,195)%j+A(j,196)%j+A(j,197)%j)*f(2)+CI*(A(j,47)%j+A(j,49)%j+A(j,51)%j &
       +A(j,53)%j+A(j,55)%j+A(j,57)%j+A(j,93)%j+A(j,95)%j+A(j,97)%j+A(j,99)%j)*f(3)+CI*(A(j,48)%j+A(j,50)%j+A(j,52)%j+A(j,54)%j &
       +A(j,56)%j+A(j,58)%j+A(j,94)%j+A(j,96)%j+A(j,98)%j+A(j,100)%j+A(j,200)%j+A(j,201)%j+A(j,202)%j+A(j,203)%j)*f(4)+( &
       -A(j,113)%j-A(j,114)%j-A(j,115)%j-A(j,119)%j-A(j,120)%j-A(j,139)%j)*f(5)+CI*(-A(j,129)%j-A(j,130)%j-A(j,131)%j)*f(6) &
       +(A(j,110)%j+A(j,116)%j+A(j,121)%j)*f(7)+CI*(A(j,132)%j+A(j,135)%j)*f(8)+(-A(j,112)%j-A(j,118)%j-A(j,123)%j)*f(9)+CI*( &
       -A(j,134)%j-A(j,137)%j)*f(10)+(A(j,111)%j+A(j,117)%j+A(j,122)%j-A(j,146)%j-A(j,149)%j-A(j,152)%j-A(j,170)%j-A(j,173)%j &
       -A(j,176)%j)*f(11)+(-A(j,147)%j-A(j,148)%j-A(j,150)%j-A(j,151)%j-A(j,153)%j-A(j,154)%j-A(j,171)%j-A(j,172)%j-A(j,174)%j &
       -A(j,175)%j-A(j,177)%j-A(j,178)%j)*f(12)+CI*(A(j,133)%j+A(j,136)%j-A(j,158)%j-A(j,160)%j-A(j,182)%j-A(j,184)%j)*f(13)+CI*( &
       -A(j,159)%j-A(j,161)%j-A(j,162)%j-A(j,163)%j-A(j,183)%j-A(j,185)%j-A(j,186)%j-A(j,187)%j)*f(14)+(-A(j,15)%j-A(j,28)%j &
       -A(j,37)%j)*f(15)+CI*(-A(j,59)%j-A(j,60)%j)*f(16)
  M1(2) = (A(j,2)%j+A(j,4)%j+A(j,6)%j+A(j,8)%j+A(j,10)%j+A(j,12)%j+A(j,38)%j+A(j,40)%j+A(j,42)%j+A(j,44)%j+A(j,61)%j+A(j,65)%j &
       +A(j,69)%j+A(j,71)%j+A(j,73)%j+A(j,75)%j+A(j,89)%j+A(j,91)%j)*f(1)+(A(j,3)%j+A(j,5)%j+A(j,7)%j+A(j,9)%j+A(j,11)%j+A(j,13)%j &
       +A(j,39)%j+A(j,41)%j+A(j,43)%j+A(j,45)%j+A(j,62)%j+A(j,66)%j+A(j,70)%j+A(j,72)%j+A(j,74)%j+A(j,76)%j+A(j,90)%j+A(j,92)%j &
       +A(j,188)%j+A(j,189)%j+A(j,190)%j+A(j,191)%j+A(j,198)%j+A(j,199)%j)*f(2)+CI*(-A(j,47)%j-A(j,49)%j-A(j,51)%j-A(j,53)%j &
       -A(j,55)%j-A(j,57)%j-A(j,93)%j-A(j,95)%j-A(j,97)%j-A(j,99)%j)*f(3)+CI*(-A(j,48)%j-A(j,50)%j-A(j,52)%j-A(j,54)%j-A(j,56)%j &
       -A(j,58)%j-A(j,94)%j-A(j,96)%j-A(j,98)%j-A(j,100)%j-A(j,200)%j-A(j,201)%j-A(j,202)%j-A(j,203)%j)*f(4)+(-A(j,104)%j &
       -A(j,105)%j-A(j,106)%j-A(j,124)%j-A(j,125)%j-A(j,138)%j)*f(5)+CI*(A(j,129)%j+A(j,130)%j+A(j,131)%j)*f(6)+(A(j,101)%j &
       +A(j,107)%j+A(j,126)%j)*f(7)+CI*(-A(j,132)%j-A(j,135)%j)*f(8)+(-A(j,103)%j-A(j,109)%j-A(j,128)%j)*f(9)+CI*(A(j,134)%j &
       +A(j,137)%j)*f(10)+(A(j,102)%j+A(j,108)%j+A(j,127)%j-A(j,140)%j-A(j,143)%j-A(j,155)%j-A(j,164)%j-A(j,167)%j &
       -A(j,179)%j)*f(11)+(-A(j,141)%j-A(j,142)%j-A(j,144)%j-A(j,145)%j-A(j,156)%j-A(j,157)%j-A(j,165)%j-A(j,166)%j-A(j,168)%j &
       -A(j,169)%j-A(j,180)%j-A(j,181)%j)*f(12)+CI*(-A(j,133)%j-A(j,136)%j+A(j,158)%j+A(j,160)%j+A(j,182)%j+A(j,184)%j)*f(13) &
       +CI*(A(j,159)%j+A(j,161)%j+A(j,162)%j+A(j,163)%j+A(j,183)%j+A(j,185)%j+A(j,186)%j+A(j,187)%j)*f(14)+(-A(j,1)%j-A(j,14)%j &
       -A(j,46)%j)*f(15)+CI*(A(j,59)%j+A(j,60)%j)*f(16)

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
  use ol_colourmatrix_ppllllj_nenexeexbbxgg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(2)
  real(REALKIND),    intent(out) :: M2colint(0:38-1)
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
  use ol_colourmatrix_ppllllj_nenexeexbbxgg_1_/**/REALKIND, only: K1
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
  & bind(c,name="ol_tree_colvect_ppllllj_nenexeexbbxgg_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(2,256)
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
    & bind(c,name="ol_f_amp2tree_ppllllj_nenexeexbbxgg_1")
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
    & bind(c,name="ol_f_amp2ccone_ppllllj_nenexeexbbxgg_1")
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
    & bind(c,name="ol_f_amp2ccall_ppllllj_nenexeexbbxgg_1")
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
    & bind(c,name="ol_f_amp2hcone_ppllllj_nenexeexbbxgg_1")
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
    & bind(c,name="ol_f_amp2hcall_ppllllj_nenexeexbbxgg_1")
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
    & bind(c,name="ol_amp2tree_ppllllj_nenexeexbbxgg_1")
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
    & bind(c,name="ol_amp2ccone_ppllllj_nenexeexbbxgg_1")
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
    & bind(c,name="ol_amp2ccall_ppllllj_nenexeexbbxgg_1")
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
    & bind(c,name="ol_amp2hcone_ppllllj_nenexeexbbxgg_1")
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
    & bind(c,name="ol_amp2hcall_ppllllj_nenexeexbbxgg_1")
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
    & bind(c,name="amp2tree_ppllllj_nenexeexbbxgg_1_")
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
    & bind(c,name="amp2ccone_ppllllj_nenexeexbbxgg_1_")
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
    & bind(c,name="amp2ccall_ppllllj_nenexeexbbxgg_1_")
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
    & bind(c,name="amp2hcone_ppllllj_nenexeexbbxgg_1_")
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
    & bind(c,name="amp2hcall_ppllllj_nenexeexbbxgg_1_")
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

end module ol_tree_ppllllj_nenexeexbbxgg_1_/**/REALKIND
