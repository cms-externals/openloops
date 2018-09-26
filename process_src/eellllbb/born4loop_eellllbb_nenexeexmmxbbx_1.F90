
module ol_tree_eellllbb_nenexeexmmxbbx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(48)
  complex(REALKIND), save :: den(927)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 256 ! number of helicity configurations
  integer(intkind2), save :: nhel = 256 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(256) ! physical helicity states
  complex(DREALKIND) :: M1helarr(1,256) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f( 1) = (CI*eQED**6)/9._/**/REALKIND
    f( 2) = (CI*eQED**6)/3._/**/REALKIND
    f( 3) = CI*eQED**6
    f( 4) = (CI*eQED**6)/(8._/**/REALKIND*sw**6)
    f( 5) = (CI*eQED**6*MB)/(8._/**/REALKIND*sw**6)
    f( 6) = (CI*eQED**6*MM)/(8._/**/REALKIND*sw**6)
    f( 7) = (CI*eQED**6*MB*MM)/(8._/**/REALKIND*sw**6)
    f( 8) = (CI*eQED**6*MB*MM)/(16._/**/REALKIND*MW**2*sw**6)
    f( 9) = (CI*eQED**6*MB**2*MM)/(16._/**/REALKIND*MW**2*sw**6)
    f(10) = (3*CI*eQED**6*MB*MH**2*MM)/(16._/**/REALKIND*MW**2*sw**6)
    f(11) = (CI*eQED**6*MB*MM**2)/(16._/**/REALKIND*MW**2*sw**6)
    f(12) = (CI*eQED**6*MM*MT)/(16._/**/REALKIND*MW**2*sw**6)
    f(13) = (CI*cw*eQED**6)/(4._/**/REALKIND*sw**5)
    f(14) = (CI*eQED**6*MB)/(4._/**/REALKIND*cw*sw**5)
    f(15) = (CI*cw*eQED**6*MB)/(4._/**/REALKIND*sw**5)
    f(16) = (CI*eQED**6*MM)/(4._/**/REALKIND*cw*sw**5)
    f(17) = (CI*cw*eQED**6*MM)/(4._/**/REALKIND*sw**5)
    f(18) = (CI*cw*eQED**6*MB*MM)/(8._/**/REALKIND*MW**2*sw**5)
    f(19) = (CI*eQED**6)/(12._/**/REALKIND*sw**4)
    f(20) = (CI*eQED**6)/(6._/**/REALKIND*sw**4)
    f(21) = (CI*eQED**6)/(4._/**/REALKIND*sw**4)
    f(22) = (CI*cw**2*eQED**6)/(2._/**/REALKIND*sw**4)
    f(23) = (CI*eQED**6*MB)/(12._/**/REALKIND*sw**4)
    f(24) = (CI*eQED**6*MB)/(4._/**/REALKIND*sw**4)
    f(25) = (CI*eQED**6*MB)/(4._/**/REALKIND*cw**2*sw**4)
    f(26) = (CI*eQED**6*MM)/(12._/**/REALKIND*sw**4)
    f(27) = (CI*eQED**6*MM)/(4._/**/REALKIND*sw**4)
    f(28) = (CI*eQED**6*MM)/(4._/**/REALKIND*cw**2*sw**4)
    f(29) = (CI*eQED**6*MB*MM)/(4._/**/REALKIND*cw**4*sw**4)
    f(30) = (CI*eQED**6*MB*MM)/(24._/**/REALKIND*MW**2*sw**4)
    f(31) = (CI*eQED**6*MB*MM)/(8._/**/REALKIND*MW**2*sw**4)
    f(32) = (CI*eQED**6*MB*MM)/(8._/**/REALKIND*cw**2*MW**2*sw**4)
    f(33) = (CI*eQED**6*MB**2*MM)/(8._/**/REALKIND*cw**2*MW**2*sw**4)
    f(34) = (3*CI*eQED**6*MB*MH**2*MM)/(8._/**/REALKIND*cw**2*MW**2*sw**4)
    f(35) = (CI*eQED**6*MB*MM**2)/(8._/**/REALKIND*cw**2*MW**2*sw**4)
    f(36) = (CI*eQED**6*MW**2)/(2._/**/REALKIND*cw**2*sw**4)
    f(37) = (CI*cw*eQED**6)/(6._/**/REALKIND*sw**3)
    f(38) = (CI*cw*eQED**6)/(2._/**/REALKIND*sw**3)
    f(39) = (CI*eQED**6)/(18._/**/REALKIND*sw**2)
    f(40) = (CI*eQED**6)/(6._/**/REALKIND*sw**2)
    f(41) = (CI*eQED**6)/(2._/**/REALKIND*sw**2)
    f(42) = (CI*eQED**6*MB)/(6._/**/REALKIND*cw**2*sw**2)
    f(43) = (CI*eQED**6*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(44) = (CI*eQED**6*MM)/(6._/**/REALKIND*cw**2*sw**2)
    f(45) = (CI*eQED**6*MM)/(2._/**/REALKIND*cw**2*sw**2)
    f(46) = (CI*eQED**6*MB*MM)/(12._/**/REALKIND*MW**2*sw**2)
    f(47) = (CI*eQED**6*MB*MM)/(4._/**/REALKIND*MW**2*sw**2)
    f(48) = (CI*eQED**6*MW**2)/(cw**4*sw**2)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,3) - MZ2)
  den(2) = 1 / (Q(5,12) - MZ2)
  den(3) = 1 / (Q(5,48) - MH2)
  den(4) = 1 / (Q(5,192) - MH2)
  den(7) = 1 / (Q(5,9) - MW2)
  den(8) = 1 / (Q(5,6) - MW2)
  den(11) = 1 / (Q(5,48))
  den(12) = 1 / (Q(5,192))
  den(15) = 1 / (Q(5,192) - MZ2)
  den(17) = 1 / (Q(5,48) - MZ2)
  den(23) = 1 / (Q(5,15) - MH2)
  den(30) = 1 / (Q(5,51) - MZ2)
  den(35) = 1 / (Q(5,51) - MH2)
  den(40) = 1 / (Q(5,195) - MZ2)
  den(45) = 1 / (Q(5,195) - MH2)
  den(48) = 1 / (Q(5,12))
  den(49) = 1 / (Q(5,67) - MB2)
  den(51) = 1 / (Q(5,140) - MB2)
  den(63) = 1 / (Q(5,176) - MB2)
  den(75) = 1 / (Q(5,60) - MZ2)
  den(78) = 1 / (Q(5,60) - MH2)
  den(81) = 1 / (Q(5,76) - MB2)
  den(83) = 1 / (Q(5,131) - MB2)
  den(98) = 1 / (Q(5,112) - MB2)
  den(147) = 1 / (Q(5,19) - MM2)
  den(149) = 1 / (Q(5,44) - MM2)
  den(160) = 1 / (Q(5,204) - MH2)
  den(163) = 1 / (Q(5,204) - MZ2)
  den(166) = 1 / (Q(5,224) - MM2)
  den(178) = 1 / (Q(5,28) - MM2)
  den(180) = 1 / (Q(5,35) - MM2)
  den(193) = 1 / (Q(5,208) - MM2)
  den(243) = 1 / (Q(5,51))
  den(275) = 1 / (Q(5,60))
  den(309) = 1 / (Q(5,7))
  den(311) = 1 / (Q(5,240) - MZ2)
  den(317) = 1 / (Q(5,56))
  den(326) = 1 / (Q(5,200))
  den(335) = 1 / (Q(5,11))
  den(339) = 1 / (Q(5,52))
  den(348) = 1 / (Q(5,196))
  den(377) = 1 / (Q(5,15))
  den(380) = 1 / (Q(5,15) - MZ2)
  den(455) = 1 / (Q(5,204))
  den(489) = 1 / (Q(5,57) - MW2)
  den(508) = 1 / (Q(5,201) - MW2)
  den(525) = 1 / (Q(5,73) - MT2)
  den(527) = 1 / (Q(5,134) - MT2)
  den(539) = 1 / (Q(5,54) - MW2)
  den(574) = 1 / (Q(5,25))
  den(576) = 1 / (Q(5,38))
  den(580) = 1 / (Q(5,198) - MW2)
  den(637) = 1 / (Q(5,49))
  den(642) = 1 / (Q(5,193))
  den(647) = 1 / (Q(5,14))
  den(739) = 1 / (Q(5,13))
  den(743) = 1 / (Q(5,50))
  den(747) = 1 / (Q(5,194))

  ! denominators

  den(5) = den(1)*den(2)*den(3)
  den(6) = den(4)*den(5)
  den(9) = den(3)*den(7)*den(8)
  den(10) = den(4)*den(9)
  den(13) = den(7)*den(8)*den(11)
  den(14) = den(12)*den(13)
  den(16) = den(13)*den(15)
  den(18) = den(7)*den(8)*den(17)
  den(19) = den(12)*den(18)
  den(20) = den(15)*den(18)
  den(21) = den(1)*den(2)
  den(22) = den(3)*den(4)
  den(24) = den(21)*den(23)
  den(25) = den(22)*den(24)
  den(26) = den(15)*den(17)
  den(27) = den(24)*den(26)
  den(28) = den(1)*den(3)
  den(29) = den(2)*den(4)
  den(31) = den(28)*den(30)
  den(32) = den(29)*den(31)
  den(33) = den(1)*den(17)
  den(34) = den(2)*den(15)
  den(36) = den(33)*den(35)
  den(37) = den(34)*den(36)
  den(38) = den(1)*den(4)
  den(39) = den(2)*den(3)
  den(41) = den(38)*den(40)
  den(42) = den(39)*den(41)
  den(43) = den(1)*den(15)
  den(44) = den(2)*den(17)
  den(46) = den(43)*den(45)
  den(47) = den(44)*den(46)
  den(50) = den(1)*den(49)
  den(52) = den(48)*den(51)
  den(53) = den(3)*den(50)
  den(54) = den(52)*den(53)
  den(55) = den(2)*den(51)
  den(56) = den(53)*den(55)
  den(57) = den(11)*den(50)
  den(58) = den(52)*den(57)
  den(59) = den(17)*den(50)
  den(60) = den(52)*den(59)
  den(61) = den(55)*den(57)
  den(62) = den(55)*den(59)
  den(64) = den(3)*den(63)
  den(65) = den(48)*den(50)
  den(66) = den(64)*den(65)
  den(67) = den(2)*den(50)
  den(68) = den(64)*den(67)
  den(69) = den(11)*den(63)
  den(70) = den(65)*den(69)
  den(71) = den(17)*den(63)
  den(72) = den(65)*den(71)
  den(73) = den(67)*den(69)
  den(74) = den(67)*den(71)
  den(76) = den(39)*den(75)
  den(77) = den(50)*den(76)
  den(79) = den(44)*den(78)
  den(80) = den(50)*den(79)
  den(82) = den(48)*den(81)
  den(84) = den(1)*den(83)
  den(85) = den(3)*den(82)
  den(86) = den(84)*den(85)
  den(87) = den(2)*den(81)
  den(88) = den(3)*den(87)
  den(89) = den(84)*den(88)
  den(90) = den(11)*den(82)
  den(91) = den(84)*den(90)
  den(92) = den(17)*den(82)
  den(93) = den(84)*den(92)
  den(94) = den(11)*den(87)
  den(95) = den(84)*den(94)
  den(96) = den(17)*den(87)
  den(97) = den(84)*den(96)
  den(99) = den(3)*den(98)
  den(100) = den(48)*den(99)
  den(101) = den(84)*den(100)
  den(102) = den(2)*den(99)
  den(103) = den(84)*den(102)
  den(104) = den(11)*den(98)
  den(105) = den(48)*den(104)
  den(106) = den(84)*den(105)
  den(107) = den(17)*den(98)
  den(108) = den(48)*den(107)
  den(109) = den(84)*den(108)
  den(110) = den(2)*den(104)
  den(111) = den(84)*den(110)
  den(112) = den(2)*den(107)
  den(113) = den(84)*den(112)
  den(114) = den(76)*den(84)
  den(115) = den(79)*den(84)
  den(116) = den(1)*den(82)
  den(117) = den(64)*den(116)
  den(118) = den(1)*den(87)
  den(119) = den(64)*den(118)
  den(120) = den(69)*den(116)
  den(121) = den(71)*den(116)
  den(122) = den(69)*den(118)
  den(123) = den(71)*den(118)
  den(124) = den(31)*den(82)
  den(125) = den(31)*den(87)
  den(126) = den(36)*den(82)
  den(127) = den(36)*den(87)
  den(128) = den(1)*den(99)
  den(129) = den(52)*den(128)
  den(130) = den(55)*den(128)
  den(131) = den(1)*den(104)
  den(132) = den(52)*den(131)
  den(133) = den(1)*den(107)
  den(134) = den(52)*den(133)
  den(135) = den(55)*den(131)
  den(136) = den(55)*den(133)
  den(137) = den(31)*den(52)
  den(138) = den(31)*den(55)
  den(139) = den(36)*den(52)
  den(140) = den(36)*den(55)
  den(141) = den(24)*den(99)
  den(142) = den(24)*den(104)
  den(143) = den(24)*den(107)
  den(144) = den(24)*den(64)
  den(145) = den(24)*den(69)
  den(146) = den(24)*den(71)
  den(148) = den(1)*den(147)
  den(150) = den(48)*den(149)
  den(151) = den(148)*den(150)
  den(152) = den(4)*den(151)
  den(153) = den(2)*den(149)
  den(154) = den(148)*den(153)
  den(155) = den(4)*den(154)
  den(156) = den(12)*den(151)
  den(157) = den(15)*den(151)
  den(158) = den(12)*den(154)
  den(159) = den(15)*den(154)
  den(161) = den(34)*den(160)
  den(162) = den(148)*den(161)
  den(164) = den(29)*den(163)
  den(165) = den(148)*den(164)
  den(167) = den(4)*den(166)
  den(168) = den(48)*den(148)
  den(169) = den(167)*den(168)
  den(170) = den(2)*den(148)
  den(171) = den(167)*den(170)
  den(172) = den(12)*den(166)
  den(173) = den(168)*den(172)
  den(174) = den(15)*den(166)
  den(175) = den(168)*den(174)
  den(176) = den(170)*den(172)
  den(177) = den(170)*den(174)
  den(179) = den(48)*den(178)
  den(181) = den(1)*den(180)
  den(182) = den(179)*den(181)
  den(183) = den(4)*den(182)
  den(184) = den(2)*den(178)
  den(185) = den(181)*den(184)
  den(186) = den(4)*den(185)
  den(187) = den(12)*den(182)
  den(188) = den(15)*den(182)
  den(189) = den(12)*den(185)
  den(190) = den(15)*den(185)
  den(191) = den(161)*den(181)
  den(192) = den(164)*den(181)
  den(194) = den(4)*den(193)
  den(195) = den(48)*den(194)
  den(196) = den(181)*den(195)
  den(197) = den(2)*den(194)
  den(198) = den(181)*den(197)
  den(199) = den(12)*den(193)
  den(200) = den(48)*den(199)
  den(201) = den(181)*den(200)
  den(202) = den(15)*den(193)
  den(203) = den(48)*den(202)
  den(204) = den(181)*den(203)
  den(205) = den(2)*den(199)
  den(206) = den(181)*den(205)
  den(207) = den(2)*den(202)
  den(208) = den(181)*den(207)
  den(209) = den(46)*den(179)
  den(210) = den(46)*den(184)
  den(211) = den(41)*den(179)
  den(212) = den(41)*den(184)
  den(213) = den(1)*den(179)
  den(214) = den(167)*den(213)
  den(215) = den(1)*den(184)
  den(216) = den(167)*den(215)
  den(217) = den(172)*den(213)
  den(218) = den(174)*den(213)
  den(219) = den(172)*den(215)
  den(220) = den(174)*den(215)
  den(221) = den(46)*den(150)
  den(222) = den(46)*den(153)
  den(223) = den(41)*den(150)
  den(224) = den(41)*den(153)
  den(225) = den(1)*den(194)
  den(226) = den(150)*den(225)
  den(227) = den(153)*den(225)
  den(228) = den(1)*den(199)
  den(229) = den(150)*den(228)
  den(230) = den(1)*den(202)
  den(231) = den(150)*den(230)
  den(232) = den(153)*den(228)
  den(233) = den(153)*den(230)
  den(234) = den(24)*den(194)
  den(235) = den(24)*den(199)
  den(236) = den(24)*den(202)
  den(237) = den(24)*den(167)
  den(238) = den(24)*den(172)
  den(239) = den(24)*den(174)
  den(240) = den(35)*den(148)
  den(241) = den(82)*den(240)
  den(242) = den(87)*den(240)
  den(244) = den(148)*den(243)
  den(245) = den(82)*den(244)
  den(246) = den(30)*den(148)
  den(247) = den(82)*den(246)
  den(248) = den(87)*den(244)
  den(249) = den(87)*den(246)
  den(250) = den(52)*den(240)
  den(251) = den(55)*den(240)
  den(252) = den(52)*den(244)
  den(253) = den(52)*den(246)
  den(254) = den(55)*den(244)
  den(255) = den(55)*den(246)
  den(256) = den(35)*den(181)
  den(257) = den(82)*den(256)
  den(258) = den(87)*den(256)
  den(259) = den(181)*den(243)
  den(260) = den(82)*den(259)
  den(261) = den(30)*den(181)
  den(262) = den(82)*den(261)
  den(263) = den(87)*den(259)
  den(264) = den(87)*den(261)
  den(265) = den(52)*den(256)
  den(266) = den(55)*den(256)
  den(267) = den(52)*den(259)
  den(268) = den(52)*den(261)
  den(269) = den(55)*den(259)
  den(270) = den(55)*den(261)
  den(271) = den(78)*den(179)
  den(272) = den(50)*den(271)
  den(273) = den(78)*den(184)
  den(274) = den(50)*den(273)
  den(276) = den(179)*den(275)
  den(277) = den(50)*den(276)
  den(278) = den(75)*den(179)
  den(279) = den(50)*den(278)
  den(280) = den(184)*den(275)
  den(281) = den(50)*den(280)
  den(282) = den(75)*den(184)
  den(283) = den(50)*den(282)
  den(284) = den(78)*den(150)
  den(285) = den(50)*den(284)
  den(286) = den(78)*den(153)
  den(287) = den(50)*den(286)
  den(288) = den(150)*den(275)
  den(289) = den(50)*den(288)
  den(290) = den(75)*den(150)
  den(291) = den(50)*den(290)
  den(292) = den(153)*den(275)
  den(293) = den(50)*den(292)
  den(294) = den(75)*den(153)
  den(295) = den(50)*den(294)
  den(296) = den(84)*den(271)
  den(297) = den(84)*den(273)
  den(298) = den(84)*den(276)
  den(299) = den(84)*den(278)
  den(300) = den(84)*den(280)
  den(301) = den(84)*den(282)
  den(302) = den(84)*den(284)
  den(303) = den(84)*den(286)
  den(304) = den(84)*den(288)
  den(305) = den(84)*den(290)
  den(306) = den(84)*den(292)
  den(307) = den(84)*den(294)
  den(308) = den(3)*den(15)
  den(310) = den(1)*den(309)
  den(312) = den(308)*den(311)
  den(313) = den(310)*den(312)
  den(314) = den(4)*den(17)
  den(315) = den(311)*den(314)
  den(316) = den(310)*den(315)
  den(318) = den(11)*den(317)
  den(319) = den(310)*den(318)
  den(320) = den(12)*den(319)
  den(321) = den(15)*den(319)
  den(322) = den(17)*den(317)
  den(323) = den(310)*den(322)
  den(324) = den(12)*den(323)
  den(325) = den(15)*den(323)
  den(327) = den(12)*den(326)
  den(328) = den(310)*den(327)
  den(329) = den(11)*den(328)
  den(330) = den(17)*den(328)
  den(331) = den(15)*den(326)
  den(332) = den(310)*den(331)
  den(333) = den(11)*den(332)
  den(334) = den(17)*den(332)
  den(336) = den(1)*den(335)
  den(337) = den(312)*den(336)
  den(338) = den(315)*den(336)
  den(340) = den(11)*den(339)
  den(341) = den(336)*den(340)
  den(342) = den(12)*den(341)
  den(343) = den(15)*den(341)
  den(344) = den(17)*den(339)
  den(345) = den(336)*den(344)
  den(346) = den(12)*den(345)
  den(347) = den(15)*den(345)
  den(349) = den(12)*den(348)
  den(350) = den(336)*den(349)
  den(351) = den(11)*den(350)
  den(352) = den(17)*den(350)
  den(353) = den(15)*den(348)
  den(354) = den(336)*den(353)
  den(355) = den(11)*den(354)
  den(356) = den(17)*den(354)
  den(357) = den(41)*den(340)
  den(358) = den(41)*den(344)
  den(359) = den(31)*den(349)
  den(360) = den(31)*den(353)
  den(361) = den(41)*den(318)
  den(362) = den(41)*den(322)
  den(363) = den(31)*den(327)
  den(364) = den(31)*den(331)
  den(365) = den(1)*den(340)
  den(366) = den(327)*den(365)
  den(367) = den(331)*den(365)
  den(368) = den(1)*den(344)
  den(369) = den(327)*den(368)
  den(370) = den(331)*den(368)
  den(371) = den(1)*den(349)
  den(372) = den(318)*den(371)
  den(373) = den(322)*den(371)
  den(374) = den(1)*den(353)
  den(375) = den(318)*den(374)
  den(376) = den(322)*den(374)
  den(378) = den(310)*den(377)
  den(379) = den(64)*den(378)
  den(381) = den(310)*den(380)
  den(382) = den(64)*den(381)
  den(383) = den(69)*den(378)
  den(384) = den(71)*den(378)
  den(385) = den(69)*den(381)
  den(386) = den(71)*den(381)
  den(387) = den(99)*den(378)
  den(388) = den(99)*den(381)
  den(389) = den(104)*den(378)
  den(390) = den(107)*den(378)
  den(391) = den(104)*den(381)
  den(392) = den(107)*den(381)
  den(393) = den(336)*den(377)
  den(394) = den(64)*den(393)
  den(395) = den(336)*den(380)
  den(396) = den(64)*den(395)
  den(397) = den(69)*den(393)
  den(398) = den(71)*den(393)
  den(399) = den(69)*den(395)
  den(400) = den(71)*den(395)
  den(401) = den(99)*den(393)
  den(402) = den(99)*den(395)
  den(403) = den(104)*den(393)
  den(404) = den(107)*den(393)
  den(405) = den(104)*den(395)
  den(406) = den(107)*den(395)
  den(407) = den(275)*den(340)
  den(408) = den(50)*den(407)
  den(409) = den(75)*den(340)
  den(410) = den(50)*den(409)
  den(411) = den(275)*den(344)
  den(412) = den(50)*den(411)
  den(413) = den(75)*den(344)
  den(414) = den(50)*den(413)
  den(415) = den(275)*den(318)
  den(416) = den(50)*den(415)
  den(417) = den(275)*den(322)
  den(418) = den(50)*den(417)
  den(419) = den(75)*den(318)
  den(420) = den(50)*den(419)
  den(421) = den(75)*den(322)
  den(422) = den(50)*den(421)
  den(423) = den(84)*den(407)
  den(424) = den(84)*den(409)
  den(425) = den(84)*den(411)
  den(426) = den(84)*den(413)
  den(427) = den(84)*den(415)
  den(428) = den(84)*den(417)
  den(429) = den(84)*den(419)
  den(430) = den(84)*den(421)
  den(431) = den(167)*den(378)
  den(432) = den(167)*den(381)
  den(433) = den(172)*den(378)
  den(434) = den(174)*den(378)
  den(435) = den(172)*den(381)
  den(436) = den(174)*den(381)
  den(437) = den(194)*den(378)
  den(438) = den(194)*den(381)
  den(439) = den(199)*den(378)
  den(440) = den(202)*den(378)
  den(441) = den(199)*den(381)
  den(442) = den(202)*den(381)
  den(443) = den(167)*den(393)
  den(444) = den(167)*den(395)
  den(445) = den(172)*den(393)
  den(446) = den(174)*den(393)
  den(447) = den(172)*den(395)
  den(448) = den(174)*den(395)
  den(449) = den(194)*den(393)
  den(450) = den(194)*den(395)
  den(451) = den(199)*den(393)
  den(452) = den(202)*den(393)
  den(453) = den(199)*den(395)
  den(454) = den(202)*den(395)
  den(456) = den(327)*den(455)
  den(457) = den(148)*den(456)
  den(458) = den(331)*den(455)
  den(459) = den(148)*den(458)
  den(460) = den(163)*den(327)
  den(461) = den(148)*den(460)
  den(462) = den(163)*den(331)
  den(463) = den(148)*den(462)
  den(464) = den(349)*den(455)
  den(465) = den(148)*den(464)
  den(466) = den(163)*den(349)
  den(467) = den(148)*den(466)
  den(468) = den(353)*den(455)
  den(469) = den(148)*den(468)
  den(470) = den(163)*den(353)
  den(471) = den(148)*den(470)
  den(472) = den(181)*den(456)
  den(473) = den(181)*den(458)
  den(474) = den(181)*den(460)
  den(475) = den(181)*den(462)
  den(476) = den(181)*den(464)
  den(477) = den(181)*den(466)
  den(478) = den(181)*den(468)
  den(479) = den(181)*den(470)
  den(480) = den(7)*den(8)
  den(481) = den(23)*den(480)
  den(482) = den(22)*den(481)
  den(483) = den(380)*den(480)
  den(484) = den(308)*den(483)
  den(485) = den(314)*den(483)
  den(486) = den(26)*den(481)
  den(487) = den(3)*den(7)
  den(488) = den(4)*den(8)
  den(490) = den(487)*den(489)
  den(491) = den(488)*den(490)
  den(492) = den(8)*den(12)
  den(493) = den(490)*den(492)
  den(494) = den(8)*den(15)
  den(495) = den(490)*den(494)
  den(496) = den(7)*den(11)
  den(497) = den(489)*den(496)
  den(498) = den(488)*den(497)
  den(499) = den(7)*den(17)
  den(500) = den(489)*den(499)
  den(501) = den(488)*den(500)
  den(502) = den(492)*den(497)
  den(503) = den(494)*den(497)
  den(504) = den(492)*den(500)
  den(505) = den(494)*den(500)
  den(506) = den(4)*den(7)
  den(507) = den(3)*den(8)
  den(509) = den(506)*den(508)
  den(510) = den(507)*den(509)
  den(511) = den(7)*den(12)
  den(512) = den(508)*den(511)
  den(513) = den(507)*den(512)
  den(514) = den(7)*den(15)
  den(515) = den(508)*den(514)
  den(516) = den(507)*den(515)
  den(517) = den(8)*den(11)
  den(518) = den(509)*den(517)
  den(519) = den(8)*den(17)
  den(520) = den(509)*den(519)
  den(521) = den(512)*den(517)
  den(522) = den(515)*den(517)
  den(523) = den(512)*den(519)
  den(524) = den(515)*den(519)
  den(526) = den(7)*den(525)
  den(528) = den(8)*den(527)
  den(529) = den(3)*den(526)
  den(530) = den(528)*den(529)
  den(531) = den(11)*den(526)
  den(532) = den(528)*den(531)
  den(533) = den(17)*den(526)
  den(534) = den(528)*den(533)
  den(535) = den(8)*den(526)
  den(536) = den(64)*den(535)
  den(537) = den(69)*den(535)
  den(538) = den(71)*den(535)
  den(540) = den(507)*den(539)
  den(541) = den(526)*den(540)
  den(542) = den(517)*den(539)
  den(543) = den(526)*den(542)
  den(544) = den(519)*den(539)
  den(545) = den(526)*den(544)
  den(546) = den(7)*den(99)
  den(547) = den(528)*den(546)
  den(548) = den(7)*den(104)
  den(549) = den(528)*den(548)
  den(550) = den(7)*den(107)
  den(551) = den(528)*den(550)
  den(552) = den(490)*den(528)
  den(553) = den(497)*den(528)
  den(554) = den(500)*den(528)
  den(555) = den(99)*den(481)
  den(556) = den(377)*den(480)
  den(557) = den(99)*den(556)
  den(558) = den(99)*den(483)
  den(559) = den(104)*den(481)
  den(560) = den(107)*den(481)
  den(561) = den(104)*den(556)
  den(562) = den(104)*den(483)
  den(563) = den(107)*den(556)
  den(564) = den(107)*den(483)
  den(565) = den(64)*den(481)
  den(566) = den(64)*den(556)
  den(567) = den(64)*den(483)
  den(568) = den(69)*den(481)
  den(569) = den(71)*den(481)
  den(570) = den(69)*den(556)
  den(571) = den(69)*den(483)
  den(572) = den(71)*den(556)
  den(573) = den(71)*den(483)
  den(575) = den(7)*den(574)
  den(577) = den(8)*den(576)
  den(578) = den(575)*den(577)
  den(579) = den(15)*den(578)
  den(581) = den(488)*den(580)
  den(582) = den(575)*den(581)
  den(583) = den(492)*den(580)
  den(584) = den(575)*den(583)
  den(585) = den(494)*den(580)
  den(586) = den(575)*den(585)
  den(587) = den(8)*den(575)
  den(588) = den(167)*den(587)
  den(589) = den(172)*den(587)
  den(590) = den(174)*den(587)
  den(591) = den(509)*den(577)
  den(592) = den(512)*den(577)
  den(593) = den(515)*den(577)
  den(594) = den(7)*den(194)
  den(595) = den(577)*den(594)
  den(596) = den(7)*den(199)
  den(597) = den(577)*den(596)
  den(598) = den(7)*den(202)
  den(599) = den(577)*den(598)
  den(600) = den(194)*den(481)
  den(601) = den(194)*den(556)
  den(602) = den(194)*den(483)
  den(603) = den(199)*den(481)
  den(604) = den(202)*den(481)
  den(605) = den(199)*den(556)
  den(606) = den(199)*den(483)
  den(607) = den(202)*den(556)
  den(608) = den(202)*den(483)
  den(609) = den(167)*den(481)
  den(610) = den(172)*den(481)
  den(611) = den(174)*den(481)
  den(612) = den(167)*den(556)
  den(613) = den(167)*den(483)
  den(614) = den(172)*den(556)
  den(615) = den(174)*den(556)
  den(616) = den(172)*den(483)
  den(617) = den(174)*den(483)
  den(618) = den(489)*den(575)
  den(619) = den(528)*den(618)
  den(620) = den(539)*den(577)
  den(621) = den(526)*den(620)
  den(622) = den(8)*den(309)
  den(623) = den(312)*den(622)
  den(624) = den(315)*den(622)
  den(625) = den(318)*den(622)
  den(626) = den(12)*den(625)
  den(627) = den(15)*den(625)
  den(628) = den(322)*den(622)
  den(629) = den(12)*den(628)
  den(630) = den(15)*den(628)
  den(631) = den(327)*den(622)
  den(632) = den(11)*den(631)
  den(633) = den(17)*den(631)
  den(634) = den(331)*den(622)
  den(635) = den(11)*den(634)
  den(636) = den(17)*den(634)
  den(638) = den(17)*den(637)
  den(639) = den(581)*den(638)
  den(640) = den(583)*den(638)
  den(641) = den(585)*den(638)
  den(643) = den(15)*den(642)
  den(644) = den(540)*den(643)
  den(645) = den(542)*den(643)
  den(646) = den(544)*den(643)
  den(648) = den(8)*den(647)
  den(649) = den(312)*den(648)
  den(650) = den(315)*den(648)
  den(651) = den(318)*den(581)
  den(652) = den(322)*den(581)
  den(653) = den(318)*den(583)
  den(654) = den(318)*den(585)
  den(655) = den(322)*den(583)
  den(656) = den(322)*den(585)
  den(657) = den(327)*den(540)
  den(658) = den(331)*den(540)
  den(659) = den(327)*den(542)
  den(660) = den(327)*den(544)
  den(661) = den(331)*den(542)
  den(662) = den(331)*den(544)
  den(663) = den(638)*den(648)
  den(664) = den(15)*den(663)
  den(665) = den(8)*den(638)
  den(666) = den(327)*den(665)
  den(667) = den(331)*den(665)
  den(668) = den(643)*den(648)
  den(669) = den(17)*den(668)
  den(670) = den(8)*den(643)
  den(671) = den(318)*den(670)
  den(672) = den(322)*den(670)
  den(673) = den(377)*den(622)
  den(674) = den(64)*den(673)
  den(675) = den(380)*den(622)
  den(676) = den(64)*den(675)
  den(677) = den(69)*den(673)
  den(678) = den(71)*den(673)
  den(679) = den(69)*den(675)
  den(680) = den(71)*den(675)
  den(681) = den(99)*den(673)
  den(682) = den(99)*den(675)
  den(683) = den(104)*den(673)
  den(684) = den(107)*den(673)
  den(685) = den(104)*den(675)
  den(686) = den(107)*den(675)
  den(687) = den(489)*den(638)
  den(688) = den(528)*den(687)
  den(689) = den(380)*den(648)
  den(690) = den(64)*den(689)
  den(691) = den(69)*den(689)
  den(692) = den(71)*den(689)
  den(693) = den(318)*den(489)
  den(694) = den(528)*den(693)
  den(695) = den(322)*den(489)
  den(696) = den(528)*den(695)
  den(697) = den(99)*den(689)
  den(698) = den(104)*den(689)
  den(699) = den(107)*den(689)
  den(700) = den(167)*den(673)
  den(701) = den(167)*den(675)
  den(702) = den(172)*den(673)
  den(703) = den(174)*den(673)
  den(704) = den(172)*den(675)
  den(705) = den(174)*den(675)
  den(706) = den(194)*den(673)
  den(707) = den(194)*den(675)
  den(708) = den(199)*den(673)
  den(709) = den(202)*den(673)
  den(710) = den(199)*den(675)
  den(711) = den(202)*den(675)
  den(712) = den(167)*den(689)
  den(713) = den(172)*den(689)
  den(714) = den(174)*den(689)
  den(715) = den(327)*den(508)
  den(716) = den(577)*den(715)
  den(717) = den(331)*den(508)
  den(718) = den(577)*den(717)
  den(719) = den(194)*den(689)
  den(720) = den(199)*den(689)
  den(721) = den(202)*den(689)
  den(722) = den(508)*den(643)
  den(723) = den(577)*den(722)
  den(724) = den(7)*den(335)
  den(725) = den(312)*den(724)
  den(726) = den(315)*den(724)
  den(727) = den(340)*den(724)
  den(728) = den(12)*den(727)
  den(729) = den(15)*den(727)
  den(730) = den(344)*den(724)
  den(731) = den(12)*den(730)
  den(732) = den(15)*den(730)
  den(733) = den(349)*den(724)
  den(734) = den(11)*den(733)
  den(735) = den(17)*den(733)
  den(736) = den(353)*den(724)
  den(737) = den(11)*den(736)
  den(738) = den(17)*den(736)
  den(740) = den(7)*den(739)
  den(741) = den(312)*den(740)
  den(742) = den(315)*den(740)
  den(744) = den(17)*den(743)
  den(745) = den(740)*den(744)
  den(746) = den(15)*den(745)
  den(748) = den(15)*den(747)
  den(749) = den(740)*den(748)
  den(750) = den(17)*den(749)
  den(751) = den(509)*den(744)
  den(752) = den(512)*den(744)
  den(753) = den(515)*den(744)
  den(754) = den(490)*den(748)
  den(755) = den(497)*den(748)
  den(756) = den(500)*den(748)
  den(757) = den(340)*den(509)
  den(758) = den(344)*den(509)
  den(759) = den(340)*den(512)
  den(760) = den(340)*den(515)
  den(761) = den(344)*den(512)
  den(762) = den(344)*den(515)
  den(763) = den(349)*den(490)
  den(764) = den(353)*den(490)
  den(765) = den(349)*den(497)
  den(766) = den(349)*den(500)
  den(767) = den(353)*den(497)
  den(768) = den(353)*den(500)
  den(769) = den(7)*den(744)
  den(770) = den(349)*den(769)
  den(771) = den(353)*den(769)
  den(772) = den(7)*den(748)
  den(773) = den(340)*den(772)
  den(774) = den(344)*den(772)
  den(775) = den(377)*den(724)
  den(776) = den(64)*den(775)
  den(777) = den(380)*den(724)
  den(778) = den(64)*den(777)
  den(779) = den(69)*den(775)
  den(780) = den(71)*den(775)
  den(781) = den(69)*den(777)
  den(782) = den(71)*den(777)
  den(783) = den(99)*den(775)
  den(784) = den(99)*den(777)
  den(785) = den(104)*den(775)
  den(786) = den(107)*den(775)
  den(787) = den(104)*den(777)
  den(788) = den(107)*den(777)
  den(789) = den(380)*den(740)
  den(790) = den(64)*den(789)
  den(791) = den(69)*den(789)
  den(792) = den(71)*den(789)
  den(793) = den(99)*den(789)
  den(794) = den(104)*den(789)
  den(795) = den(107)*den(789)
  den(796) = den(539)*den(744)
  den(797) = den(526)*den(796)
  den(798) = den(340)*den(539)
  den(799) = den(526)*den(798)
  den(800) = den(344)*den(539)
  den(801) = den(526)*den(800)
  den(802) = den(167)*den(775)
  den(803) = den(167)*den(777)
  den(804) = den(172)*den(775)
  den(805) = den(174)*den(775)
  den(806) = den(172)*den(777)
  den(807) = den(174)*den(777)
  den(808) = den(194)*den(775)
  den(809) = den(194)*den(777)
  den(810) = den(199)*den(775)
  den(811) = den(202)*den(775)
  den(812) = den(199)*den(777)
  den(813) = den(202)*den(777)
  den(814) = den(167)*den(789)
  den(815) = den(172)*den(789)
  den(816) = den(174)*den(789)
  den(817) = den(194)*den(789)
  den(818) = den(199)*den(789)
  den(819) = den(202)*den(789)
  den(820) = den(349)*den(580)
  den(821) = den(575)*den(820)
  den(822) = den(353)*den(580)
  den(823) = den(575)*den(822)
  den(824) = den(580)*den(748)
  den(825) = den(575)*den(824)
  den(826) = den(2)*den(739)
  den(827) = den(312)*den(826)
  den(828) = den(315)*den(826)
  den(829) = den(164)*den(638)
  den(830) = den(76)*den(643)
  den(831) = den(2)*den(647)
  den(832) = den(312)*den(831)
  den(833) = den(315)*den(831)
  den(834) = den(164)*den(744)
  den(835) = den(76)*den(748)
  den(836) = den(744)*den(826)
  den(837) = den(15)*den(836)
  den(838) = den(748)*den(826)
  den(839) = den(17)*den(838)
  den(840) = den(638)*den(831)
  den(841) = den(15)*den(840)
  den(842) = den(638)*den(748)
  den(843) = den(2)*den(842)
  den(844) = den(643)*den(831)
  den(845) = den(17)*den(844)
  den(846) = den(643)*den(744)
  den(847) = den(2)*den(846)
  den(848) = den(380)*den(826)
  den(849) = den(64)*den(848)
  den(850) = den(69)*den(848)
  den(851) = den(71)*den(848)
  den(852) = den(99)*den(848)
  den(853) = den(104)*den(848)
  den(854) = den(107)*den(848)
  den(855) = den(30)*den(638)
  den(856) = den(52)*den(855)
  den(857) = den(55)*den(855)
  den(858) = den(82)*den(855)
  den(859) = den(87)*den(855)
  den(860) = den(380)*den(831)
  den(861) = den(64)*den(860)
  den(862) = den(69)*den(860)
  den(863) = den(71)*den(860)
  den(864) = den(30)*den(744)
  den(865) = den(52)*den(864)
  den(866) = den(55)*den(864)
  den(867) = den(99)*den(860)
  den(868) = den(104)*den(860)
  den(869) = den(107)*den(860)
  den(870) = den(82)*den(864)
  den(871) = den(87)*den(864)
  den(872) = den(167)*den(848)
  den(873) = den(172)*den(848)
  den(874) = den(174)*den(848)
  den(875) = den(194)*den(848)
  den(876) = den(199)*den(848)
  den(877) = den(202)*den(848)
  den(878) = den(167)*den(860)
  den(879) = den(172)*den(860)
  den(880) = den(174)*den(860)
  den(881) = den(40)*den(748)
  den(882) = den(150)*den(881)
  den(883) = den(153)*den(881)
  den(884) = den(194)*den(860)
  den(885) = den(199)*den(860)
  den(886) = den(202)*den(860)
  den(887) = den(179)*den(881)
  den(888) = den(184)*den(881)
  den(889) = den(40)*den(643)
  den(890) = den(150)*den(889)
  den(891) = den(153)*den(889)
  den(892) = den(179)*den(889)
  den(893) = den(184)*den(889)
  den(894) = den(327)*den(864)
  den(895) = den(331)*den(864)
  den(896) = den(318)*den(881)
  den(897) = den(322)*den(881)
  den(898) = den(340)*den(715)
  den(899) = den(340)*den(717)
  den(900) = den(344)*den(715)
  den(901) = den(344)*den(717)
  den(902) = den(349)*den(693)
  den(903) = den(349)*den(695)
  den(904) = den(353)*den(693)
  den(905) = den(353)*den(695)
  den(906) = den(349)*den(864)
  den(907) = den(353)*den(864)
  den(908) = den(340)*den(881)
  den(909) = den(344)*den(881)
  den(910) = den(327)*den(855)
  den(911) = den(331)*den(855)
  den(912) = den(349)*den(855)
  den(913) = den(353)*den(855)
  den(914) = den(318)*den(889)
  den(915) = den(322)*den(889)
  den(916) = den(340)*den(889)
  den(917) = den(344)*den(889)
  den(918) = den(715)*den(744)
  den(919) = den(717)*den(744)
  den(920) = den(693)*den(748)
  den(921) = den(695)*den(748)
  den(922) = den(638)*den(820)
  den(923) = den(638)*den(822)
  den(924) = den(638)*den(824)
  den(925) = den(643)*den(798)
  den(926) = den(643)*den(800)
  den(927) = den(643)*den(796)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_eellllbb_nenexeexmmxbbx_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_eellllbb_nenexeexmmxbbx_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for nu_e anti-nu_e e- e+ mu- mu+ bottom anti-bottom -> 0
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
  use ol_wavefunctions_/**/REALKIND, only: wf_V_Std
  use ol_h_vertices_/**/REALKIND
  use ol_h_contractions_/**/REALKIND
  use ol_external_eellllbb_nenexeexmmxbbx_1, only: external_perm_eellllbb_nenexeexmmxbbx_1, &
    & external_perm_inv_eellllbb_nenexeexmmxbbx_1, extcomb_perm_eellllbb_nenexeexmmxbbx_1, &
    & average_factor_eellllbb_nenexeexmmxbbx_1
  use ol_external_eellllbb_nenexeexmmxbbx_1, only: H, hel_not_initialised, hel_init, POLSEL
  use ol_colourmatrix_eellllbb_nenexeexmmxbbx_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_eellllbb_nenexeexmmxbbx_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_eellllbb_nenexeexmmxbbx_1
  use ol_kinematics_/**/REALKIND, only: LC2Std_Rep_cmplx
  implicit none

  real(DREALKIND), intent(in)  :: P_scatt(0:3,8)
  real(REALKIND),  intent(out) :: M2(0:38-1)
  real(REALKIND),  intent(out) :: M2munu(4,4)
  integer,         intent(in)  :: I
  real(REALKIND),  intent(in)  :: MOM(0:3)
  integer,         intent(in)  :: nextcombs
  integer,         intent(in)  :: extcombs(nextcombs)

  integer           :: ReplacePol, JBmunu, extcombs_permuted(nextcombs), shift, k, r, m, n
  real(REALKIND)    :: P(0:3,8)
  real(REALKIND)    :: extmasses2(8)
  real(REALKIND)    :: M2add(0:38-1)
  complex(REALKIND) :: M2munuadd(2)
  complex(REALKIND) :: MOM_LC(4), M1(1), M1helarray(1,256)
  real(REALKIND)    :: P_scatt_intern(0:3,8)
  complex(REALKIND) :: epLC(1:4), epStd(1:4,2)
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(2), ex6(2), ex7(2), ex8(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,17), wf8(8,96), wf16(16,206), wf32(32,54), wf64(64,59), wf256(256,616)

  type(polcont) :: A(256,616)
  complex(REALKIND) :: Aj(616)

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
  extmasses2 = [ rZERO2, rZERO2, rZERO2, rZERO2, rMM2, rMM2, rMB2, rMB2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_eellllbb_nenexeexmmxbbx_1,8)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,8)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_eellllbb_nenexeexmmxbbx_1(I)
  else if (I < 0) then
    JBmunu = external_perm_eellllbb_nenexeexmmxbbx_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_eellllbb_nenexeexmmxbbx_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  42 continue
  ! external WFs
  call pol_wf_Q(P(:,1), rZERO, H1, ex1, POLSEL(1))
  call pol_wf_A(P(:,2), rZERO, H2, ex2, POLSEL(2))
  call pol_wf_Q(P(:,3), rZERO, H3, ex3, POLSEL(3))
  call pol_wf_A(P(:,4), rZERO, H4, ex4, POLSEL(4))
  call pol_wf_Q(P(:,5), rMM, H5, ex5, POLSEL(5))
  call pol_wf_A(P(:,6), rMM, H6, ex6, POLSEL(6))
  call pol_wf_Q(P(:,7), rMB, H7, ex7, POLSEL(7))
  call pol_wf_A(P(:,8), rMB, H8, ex8, POLSEL(8))


  if (ntry == 1) then
    shift = 1
    ! call helbookkeeping_flip(H1, 1, shift, eflip, exthel, firstpol)
    ! call helbookkeeping_wf(H1, ex1, shift) ...

    if (any(POLSEL /= 0)) then

      call pol_wf_Q(P(:,1), rZERO, H1, ex1, 0)
      call pol_wf_A(P(:,2), rZERO, H2, ex2, 0)
      call pol_wf_Q(P(:,3), rZERO, H3, ex3, 0)
      call pol_wf_A(P(:,4), rZERO, H4, ex4, 0)
      call pol_wf_Q(P(:,5), rMM, H5, ex5, 0)
      call pol_wf_A(P(:,6), rMM, H6, ex6, 0)
      call pol_wf_Q(P(:,7), rMB, H7, ex7, 0)
      call pol_wf_A(P(:,8), rMB, H8, ex8, 0)

    end if

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
  call vert_AQ_S(gH,ntry, ex6, ex5, wf4(:,3), n3(:,3), t3x4(:,:,3))
  call vert_AQ_S(gH,ntry, ex8, ex7, wf4(:,4), n3(:,4), t3x4(:,:,4))
  call prop_W_W(ntry, wf4(:,1), Q(:,3), MZ, 1_intkind1, wf4(:,5), n2(1))
  call prop_W_W(ntry, wf4(:,2), Q(:,12), MZ, 1_intkind1, wf4(:,6), n2(2))
  call vert_VVS_S(ntry, wf4(:,5), wf4(:,6), wf4(:,3), wf64(:,1), n4(:,1), t4x64(:,:,1))
  call vert_QA_W(ntry, ex1, ex4, wf4(:,7), n3(:,5), t3x4(:,:,5))
  call vert_QA_W(ntry, ex3, ex2, wf4(:,8), n3(:,6), t3x4(:,:,6))
  call prop_W_W(ntry, wf4(:,7), Q(:,9), MW, 1_intkind1, wf4(:,9), n2(3))
  call prop_W_W(ntry, wf4(:,8), Q(:,6), MW, 1_intkind1, wf4(:,10), n2(4))
  call vert_VVS_S(ntry, wf4(:,10), wf4(:,9), wf4(:,3), wf64(:,2), n4(:,2), t4x64(:,:,2))
  call vert_QA_V(ntry, ex5, ex6, wf4(:,11), n3(:,7), t3x4(:,:,7))
  call vert_QA_V(ntry, ex7, ex8, wf4(:,12), n3(:,8), t3x4(:,:,8))
  call vert_WWV_V(ntry, wf4(:,10), wf4(:,9), wf4(:,11), wf64(:,3), n4(:,3), t4x64(:,:,3))
  call vert_QA_Z(gZd,ntry, ex7, ex8, wf4(:,13), n3(:,9), t3x4(:,:,9))
  call prop_W_W(ntry, wf4(:,13), Q(:,192), MZ, 1_intkind1, wf4(:,14), n2(5))
  call vert_QA_Z(gZl,ntry, ex5, ex6, wf4(:,15), n3(:,10), t3x4(:,:,10))
  call prop_W_W(ntry, wf4(:,15), Q(:,48), MZ, 1_intkind1, wf4(:,16), n2(6))
  call vert_WWV_V(ntry, wf4(:,10), wf4(:,9), wf4(:,16), wf64(:,4), n4(:,4), t4x64(:,:,4))
  call vert_VV_S(ntry, wf4(:,5), wf4(:,6), wf16(:,1), n3(:,11), t3x16(:,:,1))
  call vert_SS_S(ntry, wf4(:,3), wf4(:,4), wf16(:,2), n3(:,12), t3x16(:,:,2))
  call vert_VV_S(ntry, wf4(:,16), wf4(:,14), wf16(:,3), n3(:,13), t3x16(:,:,3))
  call vert_SV_V(ntry, wf4(:,3), wf4(:,5), wf16(:,4), n3(:,14), t3x16(:,:,4))
  call vert_SV_V(ntry, wf4(:,4), wf4(:,6), wf16(:,5), n3(:,15), t3x16(:,:,5))
  call prop_W_W(ntry, wf16(:,4), Q(:,51), MZ, 1_intkind1, wf16(:,6), n2(7))
  call vert_VV_S(ntry, wf4(:,5), wf4(:,16), wf16(:,7), n3(:,16), t3x16(:,:,6))
  call vert_VV_S(ntry, wf4(:,6), wf4(:,14), wf16(:,8), n3(:,17), t3x16(:,:,7))
  call vert_SV_V(ntry, wf4(:,4), wf4(:,5), wf16(:,9), n3(:,18), t3x16(:,:,8))
  call vert_SV_V(ntry, wf4(:,3), wf4(:,6), wf16(:,10), n3(:,19), t3x16(:,:,9))
  call prop_W_W(ntry, wf16(:,9), Q(:,195), MZ, 1_intkind1, wf16(:,11), n2(8))
  call vert_VV_S(ntry, wf4(:,5), wf4(:,14), wf16(:,12), n3(:,20), t3x16(:,:,10))
  call vert_VV_S(ntry, wf4(:,6), wf4(:,16), wf16(:,13), n3(:,21), t3x16(:,:,11))
  call vert_QA_V(ntry, ex3, ex4, wf4(:,17), n3(:,22), t3x4(:,:,11))
  call vert_ZQ_A(gZd,ntry, wf4(:,5), ex7, wf8(:,1), n3(:,23), t3x8(:,:,1))
  call vert_AV_Q(ntry, ex8, wf4(:,17), wf8(:,2), n3(:,24), t3x8(:,:,2))
  call prop_Q_A(ntry, wf8(:,1), Q(:,67), MB, 1_intkind1, wf8(:,3), n2(9))
  call prop_A_Q(ntry, wf8(:,2), Q(:,140), MB, 1_intkind1, wf8(:,4), n2(10))
  call vert_QS_A(gH,ntry, wf8(:,3), wf4(:,3), wf32(:,1), n3(:,25), t3x32(:,:,1))
  call vert_AZ_Q(gZd,ntry, ex8, wf4(:,6), wf8(:,5), n3(:,26), t3x8(:,:,3))
  call prop_A_Q(ntry, wf8(:,5), Q(:,140), MB, 1_intkind1, wf8(:,6), n2(11))
  call vert_VQ_A(ntry, wf4(:,11), wf8(:,3), wf32(:,2), n3(:,27), t3x32(:,:,2))
  call vert_ZQ_A(gZd,ntry, wf4(:,16), wf8(:,3), wf32(:,3), n3(:,28), t3x32(:,:,3))
  call vert_SA_Q(gH,ntry, wf4(:,3), ex8, wf8(:,7), n3(:,29), t3x8(:,:,4))
  call prop_A_Q(ntry, wf8(:,7), Q(:,176), MB, 1_intkind1, wf8(:,8), n2(12))
  call vert_VQ_A(ntry, wf4(:,17), wf8(:,3), wf32(:,4), n3(:,30), t3x32(:,:,4))
  call vert_ZQ_A(gZd,ntry, wf4(:,6), wf8(:,3), wf32(:,5), n3(:,31), t3x32(:,:,5))
  call vert_AV_Q(ntry, ex8, wf4(:,11), wf8(:,9), n3(:,32), t3x8(:,:,5))
  call prop_A_Q(ntry, wf8(:,9), Q(:,176), MB, 1_intkind1, wf8(:,10), n2(13))
  call vert_AZ_Q(gZd,ntry, ex8, wf4(:,16), wf8(:,11), n3(:,33), t3x8(:,:,6))
  call prop_A_Q(ntry, wf8(:,11), Q(:,176), MB, 1_intkind1, wf8(:,12), n2(14))
  call prop_W_W(ntry, wf16(:,10), Q(:,60), MZ, 1_intkind1, wf16(:,14), n2(15))
  call vert_QA_Z(gZd,ntry, wf8(:,3), ex8, wf16(:,15), n3(:,34), t3x16(:,:,12))
  call vert_AQ_S(gH,ntry, ex8, wf8(:,3), wf16(:,16), n3(:,35), t3x16(:,:,13))
  call vert_VQ_A(ntry, wf4(:,17), ex7, wf8(:,13), n3(:,36), t3x8(:,:,7))
  call vert_AZ_Q(gZd,ntry, ex8, wf4(:,5), wf8(:,14), n3(:,37), t3x8(:,:,8))
  call prop_Q_A(ntry, wf8(:,13), Q(:,76), MB, 1_intkind1, wf8(:,15), n2(16))
  call prop_A_Q(ntry, wf8(:,14), Q(:,131), MB, 1_intkind1, wf8(:,16), n2(17))
  call vert_QS_A(gH,ntry, wf8(:,15), wf4(:,3), wf32(:,6), n3(:,38), t3x32(:,:,6))
  call vert_ZQ_A(gZd,ntry, wf4(:,6), ex7, wf8(:,17), n3(:,39), t3x8(:,:,9))
  call prop_Q_A(ntry, wf8(:,17), Q(:,76), MB, 1_intkind1, wf8(:,18), n2(18))
  call vert_QS_A(gH,ntry, wf8(:,18), wf4(:,3), wf32(:,7), n3(:,40), t3x32(:,:,7))
  call vert_VQ_A(ntry, wf4(:,11), wf8(:,15), wf32(:,8), n3(:,41), t3x32(:,:,8))
  call vert_ZQ_A(gZd,ntry, wf4(:,16), wf8(:,15), wf32(:,9), n3(:,42), t3x32(:,:,9))
  call vert_VQ_A(ntry, wf4(:,11), wf8(:,18), wf32(:,10), n3(:,43), t3x32(:,:,10))
  call vert_ZQ_A(gZd,ntry, wf4(:,16), wf8(:,18), wf32(:,11), n3(:,44), t3x32(:,:,11))
  call vert_QS_A(gH,ntry, ex7, wf4(:,3), wf8(:,19), n3(:,45), t3x8(:,:,10))
  call prop_Q_A(ntry, wf8(:,19), Q(:,112), MB, 1_intkind1, wf8(:,20), n2(19))
  call vert_VQ_A(ntry, wf4(:,17), wf8(:,20), wf32(:,12), n3(:,46), t3x32(:,:,12))
  call vert_ZQ_A(gZd,ntry, wf4(:,6), wf8(:,20), wf32(:,13), n3(:,47), t3x32(:,:,13))
  call vert_VQ_A(ntry, wf4(:,11), ex7, wf8(:,21), n3(:,48), t3x8(:,:,11))
  call prop_Q_A(ntry, wf8(:,21), Q(:,112), MB, 1_intkind1, wf8(:,22), n2(20))
  call vert_VQ_A(ntry, wf4(:,17), wf8(:,22), wf32(:,14), n3(:,49), t3x32(:,:,14))
  call vert_ZQ_A(gZd,ntry, wf4(:,16), ex7, wf8(:,23), n3(:,50), t3x8(:,:,12))
  call prop_Q_A(ntry, wf8(:,23), Q(:,112), MB, 1_intkind1, wf8(:,24), n2(21))
  call vert_VQ_A(ntry, wf4(:,17), wf8(:,24), wf32(:,15), n3(:,51), t3x32(:,:,15))
  call vert_ZQ_A(gZd,ntry, wf4(:,6), wf8(:,22), wf32(:,16), n3(:,52), t3x32(:,:,16))
  call vert_ZQ_A(gZd,ntry, wf4(:,6), wf8(:,24), wf32(:,17), n3(:,53), t3x32(:,:,17))
  call vert_QA_Z(gZd,ntry, ex7, wf8(:,16), wf16(:,17), n3(:,54), t3x16(:,:,14))
  call vert_AQ_S(gH,ntry, wf8(:,16), ex7, wf16(:,18), n3(:,55), t3x16(:,:,15))
  call vert_ZQ_A(gZd,ntry, wf4(:,5), wf8(:,15), wf32(:,18), n3(:,56), t3x32(:,:,18))
  call vert_ZQ_A(gZd,ntry, wf4(:,5), wf8(:,18), wf32(:,19), n3(:,57), t3x32(:,:,19))
  call vert_QA_Z(gZd,ntry, wf8(:,15), ex8, wf16(:,19), n3(:,58), t3x16(:,:,16))
  call vert_QA_Z(gZd,ntry, wf8(:,18), ex8, wf16(:,20), n3(:,59), t3x16(:,:,17))
  call vert_AQ_S(gH,ntry, ex8, wf8(:,15), wf16(:,21), n3(:,60), t3x16(:,:,18))
  call vert_AQ_S(gH,ntry, ex8, wf8(:,18), wf16(:,22), n3(:,61), t3x16(:,:,19))
  call vert_ZQ_A(gZd,ntry, wf4(:,5), wf8(:,20), wf32(:,20), n3(:,62), t3x32(:,:,20))
  call vert_ZQ_A(gZd,ntry, wf4(:,5), wf8(:,22), wf32(:,21), n3(:,63), t3x32(:,:,21))
  call vert_ZQ_A(gZd,ntry, wf4(:,5), wf8(:,24), wf32(:,22), n3(:,64), t3x32(:,:,22))
  call vert_QA_Z(gZd,ntry, ex7, wf8(:,4), wf16(:,23), n3(:,65), t3x16(:,:,20))
  call vert_QA_Z(gZd,ntry, ex7, wf8(:,6), wf16(:,24), n3(:,66), t3x16(:,:,21))
  call vert_AQ_S(gH,ntry, wf8(:,4), ex7, wf16(:,25), n3(:,67), t3x16(:,:,22))
  call vert_AQ_S(gH,ntry, wf8(:,6), ex7, wf16(:,26), n3(:,68), t3x16(:,:,23))
  call vert_AQ_S(gH,ntry, ex8, wf8(:,20), wf16(:,27), n3(:,69), t3x16(:,:,24))
  call vert_AQ_S(gH,ntry, ex8, wf8(:,22), wf16(:,28), n3(:,70), t3x16(:,:,25))
  call vert_AQ_S(gH,ntry, ex8, wf8(:,24), wf16(:,29), n3(:,71), t3x16(:,:,26))
  call vert_AQ_S(gH,ntry, wf8(:,8), ex7, wf16(:,30), n3(:,72), t3x16(:,:,27))
  call vert_AQ_S(gH,ntry, wf8(:,10), ex7, wf16(:,31), n3(:,73), t3x16(:,:,28))
  call vert_AQ_S(gH,ntry, wf8(:,12), ex7, wf16(:,32), n3(:,74), t3x16(:,:,29))
  call vert_ZQ_A(gZl,ntry, wf4(:,5), ex5, wf8(:,25), n3(:,75), t3x8(:,:,13))
  call vert_AV_Q(ntry, ex6, wf4(:,17), wf8(:,26), n3(:,76), t3x8(:,:,14))
  call prop_Q_A(ntry, wf8(:,25), Q(:,19), MM, 1_intkind1, wf8(:,27), n2(22))
  call prop_A_Q(ntry, wf8(:,26), Q(:,44), MM, 1_intkind1, wf8(:,28), n2(23))
  call vert_AQ_S(gH,ntry, wf8(:,28), wf8(:,27), wf64(:,5), n3(:,77), t3x64(:,:,1))
  call vert_AZ_Q(gZl,ntry, ex6, wf4(:,6), wf8(:,29), n3(:,78), t3x8(:,:,15))
  call prop_A_Q(ntry, wf8(:,29), Q(:,44), MM, 1_intkind1, wf8(:,30), n2(24))
  call vert_AQ_S(gH,ntry, wf8(:,30), wf8(:,27), wf64(:,6), n3(:,79), t3x64(:,:,2))
  call vert_QA_V(ntry, wf8(:,27), wf8(:,28), wf64(:,7), n3(:,80), t3x64(:,:,3))
  call vert_QA_Z(gZl,ntry, wf8(:,27), wf8(:,28), wf64(:,8), n3(:,81), t3x64(:,:,4))
  call vert_QA_V(ntry, wf8(:,27), wf8(:,30), wf64(:,9), n3(:,82), t3x64(:,:,5))
  call vert_QA_Z(gZl,ntry, wf8(:,27), wf8(:,30), wf64(:,10), n3(:,83), t3x64(:,:,6))
  call vert_AQ_S(gH,ntry, ex6, wf8(:,27), wf16(:,33), n3(:,84), t3x16(:,:,30))
  call prop_W_W(ntry, wf16(:,5), Q(:,204), MZ, 1_intkind1, wf16(:,34), n2(25))
  call vert_QA_Z(gZl,ntry, wf8(:,27), ex6, wf16(:,35), n3(:,85), t3x16(:,:,31))
  call vert_SA_Q(gH,ntry, wf4(:,4), ex6, wf8(:,31), n3(:,86), t3x8(:,:,16))
  call prop_A_Q(ntry, wf8(:,31), Q(:,224), MM, 1_intkind1, wf8(:,32), n2(26))
  call vert_VQ_A(ntry, wf4(:,17), wf8(:,27), wf32(:,23), n3(:,87), t3x32(:,:,23))
  call vert_ZQ_A(gZl,ntry, wf4(:,6), wf8(:,27), wf32(:,24), n3(:,88), t3x32(:,:,24))
  call vert_AV_Q(ntry, ex6, wf4(:,12), wf8(:,33), n3(:,89), t3x8(:,:,17))
  call prop_A_Q(ntry, wf8(:,33), Q(:,224), MM, 1_intkind1, wf8(:,34), n2(27))
  call vert_AZ_Q(gZl,ntry, ex6, wf4(:,14), wf8(:,35), n3(:,90), t3x8(:,:,18))
  call prop_A_Q(ntry, wf8(:,35), Q(:,224), MM, 1_intkind1, wf8(:,36), n2(28))
  call vert_VQ_A(ntry, wf4(:,17), ex5, wf8(:,37), n3(:,91), t3x8(:,:,19))
  call vert_AZ_Q(gZl,ntry, ex6, wf4(:,5), wf8(:,38), n3(:,92), t3x8(:,:,20))
  call prop_Q_A(ntry, wf8(:,37), Q(:,28), MM, 1_intkind1, wf8(:,39), n2(29))
  call prop_A_Q(ntry, wf8(:,38), Q(:,35), MM, 1_intkind1, wf8(:,40), n2(30))
  call vert_AQ_S(gH,ntry, wf8(:,40), wf8(:,39), wf64(:,11), n3(:,93), t3x64(:,:,7))
  call vert_ZQ_A(gZl,ntry, wf4(:,6), ex5, wf8(:,41), n3(:,94), t3x8(:,:,21))
  call prop_Q_A(ntry, wf8(:,41), Q(:,28), MM, 1_intkind1, wf8(:,42), n2(31))
  call vert_AQ_S(gH,ntry, wf8(:,40), wf8(:,42), wf64(:,12), n3(:,95), t3x64(:,:,8))
  call vert_QA_V(ntry, wf8(:,39), wf8(:,40), wf64(:,13), n3(:,96), t3x64(:,:,9))
  call vert_QA_Z(gZl,ntry, wf8(:,39), wf8(:,40), wf64(:,14), n3(:,97), t3x64(:,:,10))
  call vert_QA_V(ntry, wf8(:,42), wf8(:,40), wf64(:,15), n3(:,98), t3x64(:,:,11))
  call vert_QA_Z(gZl,ntry, wf8(:,42), wf8(:,40), wf64(:,16), n3(:,99), t3x64(:,:,12))
  call vert_AQ_S(gH,ntry, wf8(:,40), ex5, wf16(:,36), n3(:,100), t3x16(:,:,32))
  call vert_QA_Z(gZl,ntry, ex5, wf8(:,40), wf16(:,37), n3(:,101), t3x16(:,:,33))
  call vert_QS_A(gH,ntry, ex5, wf4(:,4), wf8(:,43), n3(:,102), t3x8(:,:,22))
  call prop_Q_A(ntry, wf8(:,43), Q(:,208), MM, 1_intkind1, wf8(:,44), n2(32))
  call vert_VQ_A(ntry, wf4(:,17), wf8(:,44), wf32(:,25), n3(:,103), t3x32(:,:,25))
  call vert_ZQ_A(gZl,ntry, wf4(:,6), wf8(:,44), wf32(:,26), n3(:,104), t3x32(:,:,26))
  call vert_VQ_A(ntry, wf4(:,12), ex5, wf8(:,45), n3(:,105), t3x8(:,:,23))
  call prop_Q_A(ntry, wf8(:,45), Q(:,208), MM, 1_intkind1, wf8(:,46), n2(33))
  call vert_VQ_A(ntry, wf4(:,17), wf8(:,46), wf32(:,27), n3(:,106), t3x32(:,:,27))
  call vert_ZQ_A(gZl,ntry, wf4(:,14), ex5, wf8(:,47), n3(:,107), t3x8(:,:,24))
  call prop_Q_A(ntry, wf8(:,47), Q(:,208), MM, 1_intkind1, wf8(:,48), n2(34))
  call vert_VQ_A(ntry, wf4(:,17), wf8(:,48), wf32(:,28), n3(:,108), t3x32(:,:,28))
  call vert_ZQ_A(gZl,ntry, wf4(:,6), wf8(:,46), wf32(:,29), n3(:,109), t3x32(:,:,29))
  call vert_ZQ_A(gZl,ntry, wf4(:,6), wf8(:,48), wf32(:,30), n3(:,110), t3x32(:,:,30))
  call vert_AQ_S(gH,ntry, ex6, wf8(:,39), wf16(:,38), n3(:,111), t3x16(:,:,34))
  call vert_AQ_S(gH,ntry, ex6, wf8(:,42), wf16(:,39), n3(:,112), t3x16(:,:,35))
  call vert_QA_Z(gZl,ntry, wf8(:,39), ex6, wf16(:,40), n3(:,113), t3x16(:,:,36))
  call vert_QA_Z(gZl,ntry, wf8(:,42), ex6, wf16(:,41), n3(:,114), t3x16(:,:,37))
  call vert_ZQ_A(gZl,ntry, wf4(:,5), wf8(:,39), wf32(:,31), n3(:,115), t3x32(:,:,31))
  call vert_ZQ_A(gZl,ntry, wf4(:,5), wf8(:,42), wf32(:,32), n3(:,116), t3x32(:,:,32))
  call vert_AQ_S(gH,ntry, wf8(:,28), ex5, wf16(:,42), n3(:,117), t3x16(:,:,38))
  call vert_AQ_S(gH,ntry, wf8(:,30), ex5, wf16(:,43), n3(:,118), t3x16(:,:,39))
  call vert_QA_Z(gZl,ntry, ex5, wf8(:,28), wf16(:,44), n3(:,119), t3x16(:,:,40))
  call vert_QA_Z(gZl,ntry, ex5, wf8(:,30), wf16(:,45), n3(:,120), t3x16(:,:,41))
  call vert_ZQ_A(gZl,ntry, wf4(:,5), wf8(:,44), wf32(:,33), n3(:,121), t3x32(:,:,33))
  call vert_ZQ_A(gZl,ntry, wf4(:,5), wf8(:,46), wf32(:,34), n3(:,122), t3x32(:,:,34))
  call vert_ZQ_A(gZl,ntry, wf4(:,5), wf8(:,48), wf32(:,35), n3(:,123), t3x32(:,:,35))
  call vert_AQ_S(gH,ntry, ex6, wf8(:,44), wf16(:,46), n3(:,124), t3x16(:,:,42))
  call vert_AQ_S(gH,ntry, ex6, wf8(:,46), wf16(:,47), n3(:,125), t3x16(:,:,43))
  call vert_AQ_S(gH,ntry, ex6, wf8(:,48), wf16(:,48), n3(:,126), t3x16(:,:,44))
  call vert_AQ_S(gH,ntry, wf8(:,32), ex5, wf16(:,49), n3(:,127), t3x16(:,:,45))
  call vert_AQ_S(gH,ntry, wf8(:,34), ex5, wf16(:,50), n3(:,128), t3x16(:,:,46))
  call vert_AQ_S(gH,ntry, wf8(:,36), ex5, wf16(:,51), n3(:,129), t3x16(:,:,47))
  call vert_QA_V(ntry, wf8(:,27), ex6, wf16(:,52), n3(:,130), t3x16(:,:,48))
  call vert_QA_V(ntry, wf8(:,15), ex8, wf16(:,53), n3(:,131), t3x16(:,:,49))
  call prop_W_W(ntry, wf16(:,35), Q(:,51), MZ, 1_intkind1, wf16(:,54), n2(35))
  call vert_QA_V(ntry, wf8(:,18), ex8, wf16(:,55), n3(:,132), t3x16(:,:,50))
  call vert_QA_V(ntry, ex7, wf8(:,4), wf16(:,56), n3(:,133), t3x16(:,:,51))
  call vert_QA_V(ntry, ex7, wf8(:,6), wf16(:,57), n3(:,134), t3x16(:,:,52))
  call vert_QA_V(ntry, ex5, wf8(:,40), wf16(:,58), n3(:,135), t3x16(:,:,53))
  call prop_W_W(ntry, wf16(:,37), Q(:,51), MZ, 1_intkind1, wf16(:,59), n2(36))
  call vert_QA_V(ntry, wf8(:,39), ex6, wf16(:,60), n3(:,136), t3x16(:,:,54))
  call vert_QA_V(ntry, wf8(:,3), ex8, wf16(:,61), n3(:,137), t3x16(:,:,55))
  call prop_W_W(ntry, wf16(:,40), Q(:,60), MZ, 1_intkind1, wf16(:,62), n2(37))
  call vert_QA_V(ntry, wf8(:,42), ex6, wf16(:,63), n3(:,138), t3x16(:,:,56))
  call prop_W_W(ntry, wf16(:,41), Q(:,60), MZ, 1_intkind1, wf16(:,64), n2(38))
  call vert_QA_V(ntry, ex5, wf8(:,28), wf16(:,65), n3(:,139), t3x16(:,:,57))
  call prop_W_W(ntry, wf16(:,44), Q(:,60), MZ, 1_intkind1, wf16(:,66), n2(39))
  call vert_QA_V(ntry, ex5, wf8(:,30), wf16(:,67), n3(:,140), t3x16(:,:,58))
  call prop_W_W(ntry, wf16(:,45), Q(:,60), MZ, 1_intkind1, wf16(:,68), n2(40))
  call vert_QA_V(ntry, ex7, wf8(:,16), wf16(:,69), n3(:,141), t3x16(:,:,59))
  call vert_ZQ_A(gZl,ntry, wf4(:,5), ex3, wf8(:,49), n3(:,142), t3x8(:,:,25))
  call vert_SV_V(ntry, wf4(:,3), wf4(:,14), wf16(:,70), n3(:,143), t3x16(:,:,60))
  call prop_Q_A(ntry, wf8(:,49), Q(:,7), ZERO, 0_intkind1, wf8(:,50), n2(41))
  call prop_W_W(ntry, wf16(:,70), Q(:,240), MZ, 1_intkind1, wf16(:,71), n2(42))
  call vert_QA_Z(gZl,ntry, wf8(:,50), ex4, wf16(:,72), n3(:,144), t3x16(:,:,61))
  call vert_SV_V(ntry, wf4(:,4), wf4(:,16), wf16(:,73), n3(:,145), t3x16(:,:,62))
  call prop_W_W(ntry, wf16(:,73), Q(:,240), MZ, 1_intkind1, wf16(:,74), n2(43))
  call vert_AV_Q(ntry, ex4, wf4(:,11), wf8(:,51), n3(:,146), t3x8(:,:,26))
  call prop_A_Q(ntry, wf8(:,51), Q(:,56), ZERO, 0_intkind1, wf8(:,52), n2(44))
  call vert_QA_V(ntry, wf8(:,50), wf8(:,52), wf64(:,17), n3(:,147), t3x64(:,:,13))
  call vert_QA_Z(gZl,ntry, wf8(:,50), wf8(:,52), wf64(:,18), n3(:,148), t3x64(:,:,14))
  call vert_AZ_Q(gZl,ntry, ex4, wf4(:,16), wf8(:,53), n3(:,149), t3x8(:,:,27))
  call prop_A_Q(ntry, wf8(:,53), Q(:,56), ZERO, 0_intkind1, wf8(:,54), n2(45))
  call vert_QA_V(ntry, wf8(:,50), wf8(:,54), wf64(:,19), n3(:,150), t3x64(:,:,15))
  call vert_QA_Z(gZl,ntry, wf8(:,50), wf8(:,54), wf64(:,20), n3(:,151), t3x64(:,:,16))
  call vert_AV_Q(ntry, ex4, wf4(:,12), wf8(:,55), n3(:,152), t3x8(:,:,28))
  call prop_A_Q(ntry, wf8(:,55), Q(:,200), ZERO, 0_intkind1, wf8(:,56), n2(46))
  call vert_QA_V(ntry, wf8(:,50), wf8(:,56), wf64(:,21), n3(:,153), t3x64(:,:,17))
  call vert_QA_Z(gZl,ntry, wf8(:,50), wf8(:,56), wf64(:,22), n3(:,154), t3x64(:,:,18))
  call vert_AZ_Q(gZl,ntry, ex4, wf4(:,14), wf8(:,57), n3(:,155), t3x8(:,:,29))
  call prop_A_Q(ntry, wf8(:,57), Q(:,200), ZERO, 0_intkind1, wf8(:,58), n2(47))
  call vert_QA_V(ntry, wf8(:,50), wf8(:,58), wf64(:,23), n3(:,156), t3x64(:,:,19))
  call vert_QA_Z(gZl,ntry, wf8(:,50), wf8(:,58), wf64(:,24), n3(:,157), t3x64(:,:,20))
  call vert_AZ_Q(gZl,ntry, ex4, wf4(:,5), wf8(:,59), n3(:,158), t3x8(:,:,30))
  call prop_A_Q(ntry, wf8(:,59), Q(:,11), ZERO, 0_intkind1, wf8(:,60), n2(48))
  call vert_QA_Z(gZl,ntry, ex3, wf8(:,60), wf16(:,75), n3(:,159), t3x16(:,:,63))
  call vert_VQ_A(ntry, wf4(:,11), ex3, wf8(:,61), n3(:,160), t3x8(:,:,31))
  call prop_Q_A(ntry, wf8(:,61), Q(:,52), ZERO, 0_intkind1, wf8(:,62), n2(49))
  call vert_QA_V(ntry, wf8(:,62), wf8(:,60), wf64(:,25), n3(:,161), t3x64(:,:,21))
  call vert_QA_Z(gZl,ntry, wf8(:,62), wf8(:,60), wf64(:,26), n3(:,162), t3x64(:,:,22))
  call vert_ZQ_A(gZl,ntry, wf4(:,16), ex3, wf8(:,63), n3(:,163), t3x8(:,:,32))
  call prop_Q_A(ntry, wf8(:,63), Q(:,52), ZERO, 0_intkind1, wf8(:,64), n2(50))
  call vert_QA_V(ntry, wf8(:,64), wf8(:,60), wf64(:,27), n3(:,164), t3x64(:,:,23))
  call vert_QA_Z(gZl,ntry, wf8(:,64), wf8(:,60), wf64(:,28), n3(:,165), t3x64(:,:,24))
  call vert_VQ_A(ntry, wf4(:,12), ex3, wf8(:,65), n3(:,166), t3x8(:,:,33))
  call prop_Q_A(ntry, wf8(:,65), Q(:,196), ZERO, 0_intkind1, wf8(:,66), n2(51))
  call vert_QA_V(ntry, wf8(:,66), wf8(:,60), wf64(:,29), n3(:,167), t3x64(:,:,25))
  call vert_QA_Z(gZl,ntry, wf8(:,66), wf8(:,60), wf64(:,30), n3(:,168), t3x64(:,:,26))
  call vert_ZQ_A(gZl,ntry, wf4(:,14), ex3, wf8(:,67), n3(:,169), t3x8(:,:,34))
  call prop_Q_A(ntry, wf8(:,67), Q(:,196), ZERO, 0_intkind1, wf8(:,68), n2(52))
  call vert_QA_V(ntry, wf8(:,68), wf8(:,60), wf64(:,31), n3(:,170), t3x64(:,:,27))
  call vert_QA_Z(gZl,ntry, wf8(:,68), wf8(:,60), wf64(:,32), n3(:,171), t3x64(:,:,28))
  call vert_QA_Z(gZl,ntry, wf8(:,62), ex4, wf16(:,76), n3(:,172), t3x16(:,:,64))
  call vert_QA_Z(gZl,ntry, wf8(:,64), ex4, wf16(:,77), n3(:,173), t3x16(:,:,65))
  call vert_QA_Z(gZl,ntry, wf8(:,66), ex4, wf16(:,78), n3(:,174), t3x16(:,:,66))
  call vert_QA_Z(gZl,ntry, wf8(:,68), ex4, wf16(:,79), n3(:,175), t3x16(:,:,67))
  call vert_QA_Z(gZl,ntry, ex3, wf8(:,52), wf16(:,80), n3(:,176), t3x16(:,:,68))
  call vert_QA_Z(gZl,ntry, ex3, wf8(:,54), wf16(:,81), n3(:,177), t3x16(:,:,69))
  call vert_QA_Z(gZl,ntry, ex3, wf8(:,56), wf16(:,82), n3(:,178), t3x16(:,:,70))
  call vert_QA_Z(gZl,ntry, ex3, wf8(:,58), wf16(:,83), n3(:,179), t3x16(:,:,71))
  call vert_ZQ_A(gZl,ntry, wf4(:,5), wf8(:,62), wf32(:,36), n3(:,180), t3x32(:,:,36))
  call vert_ZQ_A(gZl,ntry, wf4(:,5), wf8(:,64), wf32(:,37), n3(:,181), t3x32(:,:,37))
  call vert_ZQ_A(gZl,ntry, wf4(:,5), wf8(:,66), wf32(:,38), n3(:,182), t3x32(:,:,38))
  call vert_ZQ_A(gZl,ntry, wf4(:,5), wf8(:,68), wf32(:,39), n3(:,183), t3x32(:,:,39))
  call vert_QA_V(ntry, wf8(:,50), ex4, wf16(:,84), n3(:,184), t3x16(:,:,72))
  call vert_QA_V(ntry, ex7, wf8(:,8), wf16(:,85), n3(:,185), t3x16(:,:,73))
  call vert_QA_Z(gZd,ntry, ex7, wf8(:,8), wf16(:,86), n3(:,186), t3x16(:,:,74))
  call prop_W_W(ntry, wf16(:,72), Q(:,15), MZ, 1_intkind1, wf16(:,87), n2(53))
  call vert_QA_V(ntry, ex7, wf8(:,10), wf16(:,88), n3(:,187), t3x16(:,:,75))
  call vert_QA_V(ntry, ex7, wf8(:,12), wf16(:,89), n3(:,188), t3x16(:,:,76))
  call vert_QA_Z(gZd,ntry, ex7, wf8(:,10), wf16(:,90), n3(:,189), t3x16(:,:,77))
  call vert_QA_Z(gZd,ntry, ex7, wf8(:,12), wf16(:,91), n3(:,190), t3x16(:,:,78))
  call vert_QA_V(ntry, wf8(:,20), ex8, wf16(:,92), n3(:,191), t3x16(:,:,79))
  call vert_QA_Z(gZd,ntry, wf8(:,20), ex8, wf16(:,93), n3(:,192), t3x16(:,:,80))
  call vert_QA_V(ntry, wf8(:,22), ex8, wf16(:,94), n3(:,193), t3x16(:,:,81))
  call vert_QA_V(ntry, wf8(:,24), ex8, wf16(:,95), n3(:,194), t3x16(:,:,82))
  call vert_QA_Z(gZd,ntry, wf8(:,22), ex8, wf16(:,96), n3(:,195), t3x16(:,:,83))
  call vert_QA_Z(gZd,ntry, wf8(:,24), ex8, wf16(:,97), n3(:,196), t3x16(:,:,84))
  call vert_QA_V(ntry, ex3, wf8(:,60), wf16(:,98), n3(:,197), t3x16(:,:,85))
  call prop_W_W(ntry, wf16(:,75), Q(:,15), MZ, 1_intkind1, wf16(:,99), n2(54))
  call vert_QA_V(ntry, wf8(:,62), ex4, wf16(:,100), n3(:,198), t3x16(:,:,86))
  call prop_W_W(ntry, wf16(:,76), Q(:,60), MZ, 1_intkind1, wf16(:,101), n2(55))
  call vert_QA_V(ntry, wf8(:,64), ex4, wf16(:,102), n3(:,199), t3x16(:,:,87))
  call prop_W_W(ntry, wf16(:,77), Q(:,60), MZ, 1_intkind1, wf16(:,103), n2(56))
  call vert_QA_V(ntry, ex3, wf8(:,52), wf16(:,104), n3(:,200), t3x16(:,:,88))
  call vert_QA_V(ntry, ex3, wf8(:,54), wf16(:,105), n3(:,201), t3x16(:,:,89))
  call prop_W_W(ntry, wf16(:,80), Q(:,60), MZ, 1_intkind1, wf16(:,106), n2(57))
  call prop_W_W(ntry, wf16(:,81), Q(:,60), MZ, 1_intkind1, wf16(:,107), n2(58))
  call vert_QA_V(ntry, ex5, wf8(:,32), wf16(:,108), n3(:,202), t3x16(:,:,90))
  call vert_QA_Z(gZl,ntry, ex5, wf8(:,32), wf16(:,109), n3(:,203), t3x16(:,:,91))
  call vert_QA_V(ntry, ex5, wf8(:,34), wf16(:,110), n3(:,204), t3x16(:,:,92))
  call vert_QA_V(ntry, ex5, wf8(:,36), wf16(:,111), n3(:,205), t3x16(:,:,93))
  call vert_QA_Z(gZl,ntry, ex5, wf8(:,34), wf16(:,112), n3(:,206), t3x16(:,:,94))
  call vert_QA_Z(gZl,ntry, ex5, wf8(:,36), wf16(:,113), n3(:,207), t3x16(:,:,95))
  call vert_QA_V(ntry, wf8(:,44), ex6, wf16(:,114), n3(:,208), t3x16(:,:,96))
  call vert_QA_Z(gZl,ntry, wf8(:,44), ex6, wf16(:,115), n3(:,209), t3x16(:,:,97))
  call vert_QA_V(ntry, wf8(:,46), ex6, wf16(:,116), n3(:,210), t3x16(:,:,98))
  call vert_QA_V(ntry, wf8(:,48), ex6, wf16(:,117), n3(:,211), t3x16(:,:,99))
  call vert_QA_Z(gZl,ntry, wf8(:,46), ex6, wf16(:,118), n3(:,212), t3x16(:,:,100))
  call vert_QA_Z(gZl,ntry, wf8(:,48), ex6, wf16(:,119), n3(:,213), t3x16(:,:,101))
  call vert_QA_V(ntry, ex3, wf8(:,56), wf16(:,120), n3(:,214), t3x16(:,:,102))
  call vert_QA_V(ntry, ex3, wf8(:,58), wf16(:,121), n3(:,215), t3x16(:,:,103))
  call prop_W_W(ntry, wf16(:,82), Q(:,204), MZ, 1_intkind1, wf16(:,122), n2(59))
  call prop_W_W(ntry, wf16(:,83), Q(:,204), MZ, 1_intkind1, wf16(:,123), n2(60))
  call vert_QA_V(ntry, wf8(:,66), ex4, wf16(:,124), n3(:,216), t3x16(:,:,104))
  call prop_W_W(ntry, wf16(:,78), Q(:,204), MZ, 1_intkind1, wf16(:,125), n2(61))
  call vert_QA_V(ntry, wf8(:,68), ex4, wf16(:,126), n3(:,217), t3x16(:,:,105))
  call prop_W_W(ntry, wf16(:,79), Q(:,204), MZ, 1_intkind1, wf16(:,127), n2(62))
  call vert_VV_S(ntry, wf4(:,10), wf4(:,9), wf16(:,128), n3(:,218), t3x16(:,:,106))
  call vert_UV_W(ntry, wf4(:,9), Q(:,9), wf4(:,10), Q(:,6), wf16(:,129), n3(:,219), t3x16(:,:,107))
  call prop_W_W(ntry, wf16(:,129), Q(:,15), MZ, 1_intkind1, wf16(:,130), n2(63))
  call vert_SV_V(ntry, wf4(:,3), wf4(:,9), wf16(:,131), n3(:,220), t3x16(:,:,108))
  call vert_SV_V(ntry, wf4(:,4), wf4(:,10), wf16(:,132), n3(:,221), t3x16(:,:,109))
  call prop_W_W(ntry, wf16(:,131), Q(:,57), MW, 1_intkind1, wf16(:,133), n2(64))
  call vert_UV_W(ntry, wf4(:,10), Q(:,6), wf4(:,12), Q(:,192), wf16(:,134), n3(:,222), t3x16(:,:,110))
  call vert_UV_W(ntry, wf4(:,10), Q(:,6), wf4(:,14), Q(:,192), wf16(:,135), n3(:,223), t3x16(:,:,111))
  call vert_UV_W(ntry, wf4(:,11), Q(:,48), wf4(:,9), Q(:,9), wf16(:,136), n3(:,224), t3x16(:,:,112))
  call prop_W_W(ntry, wf16(:,136), Q(:,57), MW, 1_intkind1, wf16(:,137), n2(65))
  call vert_UV_W(ntry, wf4(:,16), Q(:,48), wf4(:,9), Q(:,9), wf16(:,138), n3(:,225), t3x16(:,:,113))
  call prop_W_W(ntry, wf16(:,138), Q(:,57), MW, 1_intkind1, wf16(:,139), n2(66))
  call vert_SV_V(ntry, wf4(:,4), wf4(:,9), wf16(:,140), n3(:,226), t3x16(:,:,114))
  call vert_SV_V(ntry, wf4(:,3), wf4(:,10), wf16(:,141), n3(:,227), t3x16(:,:,115))
  call prop_W_W(ntry, wf16(:,140), Q(:,201), MW, 1_intkind1, wf16(:,142), n2(67))
  call vert_UV_W(ntry, wf4(:,12), Q(:,192), wf4(:,9), Q(:,9), wf16(:,143), n3(:,228), t3x16(:,:,116))
  call prop_W_W(ntry, wf16(:,143), Q(:,201), MW, 1_intkind1, wf16(:,144), n2(68))
  call vert_UV_W(ntry, wf4(:,14), Q(:,192), wf4(:,9), Q(:,9), wf16(:,145), n3(:,229), t3x16(:,:,117))
  call prop_W_W(ntry, wf16(:,145), Q(:,201), MW, 1_intkind1, wf16(:,146), n2(69))
  call vert_UV_W(ntry, wf4(:,10), Q(:,6), wf4(:,11), Q(:,48), wf16(:,147), n3(:,230), t3x16(:,:,118))
  call vert_UV_W(ntry, wf4(:,10), Q(:,6), wf4(:,16), Q(:,48), wf16(:,148), n3(:,231), t3x16(:,:,119))
  call vert_WQ_A(ntry, wf4(:,9), ex7, wf8(:,69), n3(:,232), t3x8(:,:,35))
  call vert_AW_Q(ntry, ex8, wf4(:,10), wf8(:,70), n3(:,233), t3x8(:,:,36))
  call prop_Q_A(ntry, wf8(:,69), Q(:,73), MT, 1_intkind1, wf8(:,71), n2(70))
  call prop_A_Q(ntry, wf8(:,70), Q(:,134), MT, 1_intkind1, wf8(:,72), n2(71))
  call vert_QS_A(gH,ntry, wf8(:,71), wf4(:,3), wf32(:,40), n3(:,234), t3x32(:,:,40))
  call vert_VQ_A(ntry, wf4(:,11), wf8(:,71), wf32(:,41), n3(:,235), t3x32(:,:,41))
  call vert_ZQ_A(gZu,ntry, wf4(:,16), wf8(:,71), wf32(:,42), n3(:,236), t3x32(:,:,42))
  call vert_WQ_A(ntry, wf4(:,10), wf8(:,71), wf32(:,43), n3(:,237), t3x32(:,:,43))
  call prop_W_W(ntry, wf16(:,141), Q(:,54), MW, 1_intkind1, wf16(:,149), n2(72))
  call vert_QA_W(ntry, wf8(:,71), ex8, wf16(:,150), n3(:,238), t3x16(:,:,120))
  call prop_W_W(ntry, wf16(:,147), Q(:,54), MW, 1_intkind1, wf16(:,151), n2(73))
  call prop_W_W(ntry, wf16(:,148), Q(:,54), MW, 1_intkind1, wf16(:,152), n2(74))
  call vert_WQ_A(ntry, wf4(:,9), wf8(:,20), wf32(:,44), n3(:,239), t3x32(:,:,44))
  call vert_WQ_A(ntry, wf4(:,9), wf8(:,22), wf32(:,45), n3(:,240), t3x32(:,:,45))
  call vert_WQ_A(ntry, wf4(:,9), wf8(:,24), wf32(:,46), n3(:,241), t3x32(:,:,46))
  call vert_QA_W(ntry, ex7, wf8(:,72), wf16(:,153), n3(:,242), t3x16(:,:,121))
  call vert_WQ_A(ntry, wf4(:,9), ex5, wf8(:,73), n3(:,243), t3x8(:,:,37))
  call vert_AW_Q(ntry, ex6, wf4(:,10), wf8(:,74), n3(:,244), t3x8(:,:,38))
  call prop_Q_A(ntry, wf8(:,73), Q(:,25), ZERO, 0_intkind1, wf8(:,75), n2(75))
  call prop_A_Q(ntry, wf8(:,74), Q(:,38), ZERO, 0_intkind1, wf8(:,76), n2(76))
  call vert_QA_Z(gZn,ntry, wf8(:,75), wf8(:,76), wf64(:,33), n3(:,245), t3x64(:,:,29))
  call prop_W_W(ntry, wf16(:,132), Q(:,198), MW, 1_intkind1, wf16(:,154), n2(77))
  call vert_QA_W(ntry, wf8(:,75), ex6, wf16(:,155), n3(:,246), t3x16(:,:,122))
  call prop_W_W(ntry, wf16(:,134), Q(:,198), MW, 1_intkind1, wf16(:,156), n2(78))
  call prop_W_W(ntry, wf16(:,135), Q(:,198), MW, 1_intkind1, wf16(:,157), n2(79))
  call vert_WQ_A(ntry, wf4(:,10), wf8(:,75), wf32(:,47), n3(:,247), t3x32(:,:,47))
  call vert_QA_W(ntry, ex5, wf8(:,76), wf16(:,158), n3(:,248), t3x16(:,:,123))
  call vert_WQ_A(ntry, wf4(:,9), wf8(:,44), wf32(:,48), n3(:,249), t3x32(:,:,48))
  call vert_WQ_A(ntry, wf4(:,9), wf8(:,46), wf32(:,49), n3(:,250), t3x32(:,:,49))
  call vert_WQ_A(ntry, wf4(:,9), wf8(:,48), wf32(:,50), n3(:,251), t3x32(:,:,50))
  call prop_W_W(ntry, wf16(:,155), Q(:,57), MW, 1_intkind1, wf16(:,159), n2(80))
  call prop_W_W(ntry, wf16(:,158), Q(:,54), MW, 1_intkind1, wf16(:,160), n2(81))
  call vert_WQ_A(ntry, wf4(:,10), ex1, wf8(:,77), n3(:,252), t3x8(:,:,39))
  call prop_Q_A(ntry, wf8(:,77), Q(:,7), ZERO, 0_intkind1, wf8(:,78), n2(82))
  call vert_QA_Z(gZl,ntry, wf8(:,78), ex4, wf16(:,161), n3(:,253), t3x16(:,:,124))
  call vert_QA_V(ntry, wf8(:,78), wf8(:,52), wf64(:,34), n3(:,254), t3x64(:,:,30))
  call vert_QA_Z(gZl,ntry, wf8(:,78), wf8(:,52), wf64(:,35), n3(:,255), t3x64(:,:,31))
  call vert_QA_V(ntry, wf8(:,78), wf8(:,54), wf64(:,36), n3(:,256), t3x64(:,:,32))
  call vert_QA_Z(gZl,ntry, wf8(:,78), wf8(:,54), wf64(:,37), n3(:,257), t3x64(:,:,33))
  call vert_QA_V(ntry, wf8(:,78), wf8(:,56), wf64(:,38), n3(:,258), t3x64(:,:,34))
  call vert_QA_Z(gZl,ntry, wf8(:,78), wf8(:,56), wf64(:,39), n3(:,259), t3x64(:,:,35))
  call vert_QA_V(ntry, wf8(:,78), wf8(:,58), wf64(:,40), n3(:,260), t3x64(:,:,36))
  call vert_QA_Z(gZl,ntry, wf8(:,78), wf8(:,58), wf64(:,41), n3(:,261), t3x64(:,:,37))
  call vert_ZQ_A(gZn,ntry, wf4(:,16), ex1, wf8(:,79), n3(:,262), t3x8(:,:,40))
  call prop_Q_A(ntry, wf8(:,79), Q(:,49), ZERO, 0_intkind1, wf8(:,80), n2(83))
  call vert_QA_W(ntry, wf8(:,80), ex4, wf16(:,162), n3(:,263), t3x16(:,:,125))
  call vert_ZQ_A(gZn,ntry, wf4(:,14), ex1, wf8(:,81), n3(:,264), t3x8(:,:,41))
  call prop_Q_A(ntry, wf8(:,81), Q(:,193), ZERO, 0_intkind1, wf8(:,82), n2(84))
  call vert_QA_W(ntry, wf8(:,82), ex4, wf16(:,163), n3(:,265), t3x16(:,:,126))
  call vert_AW_Q(ntry, ex4, wf4(:,10), wf8(:,83), n3(:,266), t3x8(:,:,42))
  call prop_A_Q(ntry, wf8(:,83), Q(:,14), ZERO, 0_intkind1, wf8(:,84), n2(85))
  call vert_QA_Z(gZn,ntry, ex1, wf8(:,84), wf16(:,164), n3(:,267), t3x16(:,:,127))
  call vert_QA_W(ntry, ex1, wf8(:,52), wf16(:,165), n3(:,268), t3x16(:,:,128))
  call vert_QA_W(ntry, ex1, wf8(:,54), wf16(:,166), n3(:,269), t3x16(:,:,129))
  call vert_QA_W(ntry, ex1, wf8(:,56), wf16(:,167), n3(:,270), t3x16(:,:,130))
  call vert_QA_W(ntry, ex1, wf8(:,58), wf16(:,168), n3(:,271), t3x16(:,:,131))
  call vert_QA_Z(gZn,ntry, wf8(:,80), wf8(:,84), wf64(:,42), n3(:,272), t3x64(:,:,38))
  call vert_WQ_A(ntry, wf4(:,10), wf8(:,80), wf32(:,51), n3(:,273), t3x32(:,:,51))
  call vert_QA_Z(gZn,ntry, wf8(:,82), wf8(:,84), wf64(:,43), n3(:,274), t3x64(:,:,39))
  call vert_WQ_A(ntry, wf4(:,10), wf8(:,82), wf32(:,52), n3(:,275), t3x32(:,:,52))
  call vert_QA_V(ntry, wf8(:,78), ex4, wf16(:,169), n3(:,276), t3x16(:,:,132))
  call prop_W_W(ntry, wf16(:,161), Q(:,15), MZ, 1_intkind1, wf16(:,170), n2(86))
  call prop_W_W(ntry, wf16(:,162), Q(:,57), MW, 1_intkind1, wf16(:,171), n2(87))
  call prop_W_W(ntry, wf16(:,164), Q(:,15), MZ, 1_intkind1, wf16(:,172), n2(88))
  call prop_W_W(ntry, wf16(:,165), Q(:,57), MW, 1_intkind1, wf16(:,173), n2(89))
  call prop_W_W(ntry, wf16(:,166), Q(:,57), MW, 1_intkind1, wf16(:,174), n2(90))
  call prop_W_W(ntry, wf16(:,167), Q(:,201), MW, 1_intkind1, wf16(:,175), n2(91))
  call prop_W_W(ntry, wf16(:,168), Q(:,201), MW, 1_intkind1, wf16(:,176), n2(92))
  call prop_W_W(ntry, wf16(:,163), Q(:,201), MW, 1_intkind1, wf16(:,177), n2(93))
  call vert_AW_Q(ntry, ex2, wf4(:,9), wf8(:,85), n3(:,277), t3x8(:,:,43))
  call prop_A_Q(ntry, wf8(:,85), Q(:,11), ZERO, 0_intkind1, wf8(:,86), n2(94))
  call vert_QA_Z(gZl,ntry, ex3, wf8(:,86), wf16(:,178), n3(:,278), t3x16(:,:,133))
  call vert_QA_V(ntry, wf8(:,62), wf8(:,86), wf64(:,44), n3(:,279), t3x64(:,:,40))
  call vert_QA_Z(gZl,ntry, wf8(:,62), wf8(:,86), wf64(:,45), n3(:,280), t3x64(:,:,41))
  call vert_QA_V(ntry, wf8(:,64), wf8(:,86), wf64(:,46), n3(:,281), t3x64(:,:,42))
  call vert_QA_Z(gZl,ntry, wf8(:,64), wf8(:,86), wf64(:,47), n3(:,282), t3x64(:,:,43))
  call vert_QA_V(ntry, wf8(:,66), wf8(:,86), wf64(:,48), n3(:,283), t3x64(:,:,44))
  call vert_QA_Z(gZl,ntry, wf8(:,66), wf8(:,86), wf64(:,49), n3(:,284), t3x64(:,:,45))
  call vert_QA_V(ntry, wf8(:,68), wf8(:,86), wf64(:,50), n3(:,285), t3x64(:,:,46))
  call vert_QA_Z(gZl,ntry, wf8(:,68), wf8(:,86), wf64(:,51), n3(:,286), t3x64(:,:,47))
  call vert_WQ_A(ntry, wf4(:,9), ex3, wf8(:,87), n3(:,287), t3x8(:,:,44))
  call prop_Q_A(ntry, wf8(:,87), Q(:,13), ZERO, 0_intkind1, wf8(:,88), n2(95))
  call vert_QA_Z(gZn,ntry, wf8(:,88), ex2, wf16(:,179), n3(:,288), t3x16(:,:,134))
  call vert_AZ_Q(gZn,ntry, ex2, wf4(:,16), wf8(:,89), n3(:,289), t3x8(:,:,45))
  call prop_A_Q(ntry, wf8(:,89), Q(:,50), ZERO, 0_intkind1, wf8(:,90), n2(96))
  call vert_QA_Z(gZn,ntry, wf8(:,88), wf8(:,90), wf64(:,52), n3(:,290), t3x64(:,:,48))
  call vert_AZ_Q(gZn,ntry, ex2, wf4(:,14), wf8(:,91), n3(:,291), t3x8(:,:,46))
  call prop_A_Q(ntry, wf8(:,91), Q(:,194), ZERO, 0_intkind1, wf8(:,92), n2(97))
  call vert_QA_Z(gZn,ntry, wf8(:,88), wf8(:,92), wf64(:,53), n3(:,292), t3x64(:,:,49))
  call vert_QA_W(ntry, ex3, wf8(:,90), wf16(:,180), n3(:,293), t3x16(:,:,135))
  call vert_QA_W(ntry, ex3, wf8(:,92), wf16(:,181), n3(:,294), t3x16(:,:,136))
  call vert_QA_W(ntry, wf8(:,62), ex2, wf16(:,182), n3(:,295), t3x16(:,:,137))
  call vert_QA_W(ntry, wf8(:,64), ex2, wf16(:,183), n3(:,296), t3x16(:,:,138))
  call vert_QA_W(ntry, wf8(:,66), ex2, wf16(:,184), n3(:,297), t3x16(:,:,139))
  call vert_QA_W(ntry, wf8(:,68), ex2, wf16(:,185), n3(:,298), t3x16(:,:,140))
  call vert_AW_Q(ntry, wf8(:,90), wf4(:,9), wf32(:,53), n3(:,299), t3x32(:,:,53))
  call vert_AW_Q(ntry, wf8(:,92), wf4(:,9), wf32(:,54), n3(:,300), t3x32(:,:,54))
  call vert_QA_V(ntry, ex3, wf8(:,86), wf16(:,186), n3(:,301), t3x16(:,:,141))
  call prop_W_W(ntry, wf16(:,178), Q(:,15), MZ, 1_intkind1, wf16(:,187), n2(98))
  call prop_W_W(ntry, wf16(:,179), Q(:,15), MZ, 1_intkind1, wf16(:,188), n2(99))
  call prop_W_W(ntry, wf16(:,180), Q(:,54), MW, 1_intkind1, wf16(:,189), n2(100))
  call prop_W_W(ntry, wf16(:,182), Q(:,54), MW, 1_intkind1, wf16(:,190), n2(101))
  call prop_W_W(ntry, wf16(:,183), Q(:,54), MW, 1_intkind1, wf16(:,191), n2(102))
  call prop_W_W(ntry, wf16(:,184), Q(:,198), MW, 1_intkind1, wf16(:,192), n2(103))
  call prop_W_W(ntry, wf16(:,185), Q(:,198), MW, 1_intkind1, wf16(:,193), n2(104))
  call prop_W_W(ntry, wf16(:,181), Q(:,198), MW, 1_intkind1, wf16(:,194), n2(105))
  call vert_ZQ_A(gZn,ntry, wf4(:,6), ex1, wf8(:,93), n3(:,302), t3x8(:,:,47))
  call prop_Q_A(ntry, wf8(:,93), Q(:,13), ZERO, 0_intkind1, wf8(:,94), n2(106))
  call vert_QA_Z(gZn,ntry, wf8(:,94), ex2, wf16(:,195), n3(:,303), t3x16(:,:,142))
  call vert_QA_Z(gZn,ntry, wf8(:,80), ex2, wf16(:,196), n3(:,304), t3x16(:,:,143))
  call vert_QA_Z(gZn,ntry, wf8(:,82), ex2, wf16(:,197), n3(:,305), t3x16(:,:,144))
  call vert_AZ_Q(gZn,ntry, ex2, wf4(:,6), wf8(:,95), n3(:,306), t3x8(:,:,48))
  call prop_A_Q(ntry, wf8(:,95), Q(:,14), ZERO, 0_intkind1, wf8(:,96), n2(107))
  call vert_QA_Z(gZn,ntry, ex1, wf8(:,96), wf16(:,198), n3(:,307), t3x16(:,:,145))
  call vert_QA_Z(gZn,ntry, ex1, wf8(:,90), wf16(:,199), n3(:,308), t3x16(:,:,146))
  call vert_QA_Z(gZn,ntry, ex1, wf8(:,92), wf16(:,200), n3(:,309), t3x16(:,:,147))
  call vert_QA_Z(gZn,ntry, wf8(:,94), wf8(:,90), wf64(:,54), n3(:,310), t3x64(:,:,50))
  call vert_QA_Z(gZn,ntry, wf8(:,94), wf8(:,92), wf64(:,55), n3(:,311), t3x64(:,:,51))
  call vert_QA_Z(gZn,ntry, wf8(:,80), wf8(:,96), wf64(:,56), n3(:,312), t3x64(:,:,52))
  call vert_QA_Z(gZn,ntry, wf8(:,80), wf8(:,92), wf64(:,57), n3(:,313), t3x64(:,:,53))
  call vert_QA_Z(gZn,ntry, wf8(:,82), wf8(:,96), wf64(:,58), n3(:,314), t3x64(:,:,54))
  call vert_QA_Z(gZn,ntry, wf8(:,82), wf8(:,90), wf64(:,59), n3(:,315), t3x64(:,:,55))
  call prop_W_W(ntry, wf16(:,195), Q(:,15), MZ, 1_intkind1, wf16(:,201), n2(108))
  call prop_W_W(ntry, wf16(:,196), Q(:,51), MZ, 1_intkind1, wf16(:,202), n2(109))
  call prop_W_W(ntry, wf16(:,198), Q(:,15), MZ, 1_intkind1, wf16(:,203), n2(110))
  call prop_W_W(ntry, wf16(:,199), Q(:,51), MZ, 1_intkind1, wf16(:,204), n2(111))
  call prop_W_W(ntry, wf16(:,200), Q(:,195), MZ, 1_intkind1, wf16(:,205), n2(112))
  call prop_W_W(ntry, wf16(:,197), Q(:,195), MZ, 1_intkind1, wf16(:,206), n2(113))


  ! colour-stripped amplitudes
  do nsync = ntry+ntry-1, ntry+1  !  nsync = 1,2  for 1st point and nsync = 3 later
    call diagrams()
    if (nsync == 1) then
      call helsync(nsync, A, nhel, Hel)
      call helsync_flip(nsync, nhel, Hel, eflip, exthel)
      if (any(POLSEL /= 0)) then
        ntry = 2
        goto 42
      end if
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

  if ( JBmunu /= 0 ) then ! POWHEG's Bmunu helicity correlation
    P_scatt_intern = P_scatt
    M2munu = 0
    ! get wfs
    call wf_V_Std(P_scatt_intern(:,-I), rZERO, -1, epLC)
    call LC2Std_Rep_cmplx(epLC, epStd(:,1))
    call wf_V_Std(P_scatt_intern(:,-I), rZERO, 1, epLC)
    call LC2Std_Rep_cmplx(epLC, epStd(:,2))

    ! sum over helicities
    do k = 1, nhel
      !same helicity case
      call colintmunu(M1helarray(:,k), M1helarray(:,k), M2munuadd(1))
      !opposite helicity case
      r = eflip(k, JBmunu) ! Flip helicity of external particle JBmunu (gluon emitter).
      if (r <= nhel) then  ! Only add opposite helicity configuration if it doesn't vanish.
        call colintmunu(M1helarray(:,k), M1helarray(:,r), M2munuadd(2))
      else
        M2munuadd(2) = 0
      end if
      ! Fill B^(mu,nu) = sum_(k) sum_(l1,l2) M^*_(k) M_(k) (eps^(mu)_(l1))^* eps^(nu)_(l2)
      do m = 1,4
        do n = 1,4
          M2munu(m,n) = M2munu(m,n) + M2munuadd(1)*conjg(epStd(m,exthel(k,JBmunu)))*epStd(n,exthel(k,JBmunu))
          if (r <= nhel) then
            M2munu(m,n) = M2munu(m,n) + M2munuadd(2)*conjg(epStd(m,exthel(k,JBmunu)))*epStd(n,exthel(r,JBmunu))
          end if
        end do
      end do
    end do

    M2munu = M2munu / average_factor_eellllbb_nenexeexmmxbbx_1
  end if

  ! Colour and helicity average and symmetry factor of outgoing particles
  M2add = M2 / average_factor_eellllbb_nenexeexmmxbbx_1

  do k = 0, 38-1
    M2(k) = M2add(extcomb_perm_eellllbb_nenexeexmmxbbx_1(k))
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

    call cont_SS(nsync, wf4(:,4), wf64(:,1), A(:,1), n3(:,316), t3x256(:,:,1), nhel, den(6))
    call cont_SS(nsync, wf4(:,4), wf64(:,2), A(:,2), n3(:,317), t3x256(:,:,2), nhel, den(10))
    call cont_VV(nsync, wf4(:,12), wf64(:,3), A(:,3), n3(:,318), t3x256(:,:,3), nhel, den(14))
    call cont_VV(nsync, wf64(:,3), wf4(:,14), A(:,4), n3(:,319), t3x256(:,:,4), nhel, den(16))
    call cont_VV(nsync, wf4(:,12), wf64(:,4), A(:,5), n3(:,320), t3x256(:,:,5), nhel, den(19))
    call cont_VV(nsync, wf4(:,14), wf64(:,4), A(:,6), n3(:,321), t3x256(:,:,6), nhel, den(20))
    call cont_SS(nsync, wf16(:,1), wf16(:,2), A(:,7), n3(:,322), t3x256(:,:,7), nhel, den(25))
    call cont_SS(nsync, wf16(:,1), wf16(:,3), A(:,8), n3(:,323), t3x256(:,:,8), nhel, den(27))
    call cont_VV(nsync, wf16(:,5), wf16(:,6), A(:,9), n3(:,324), t3x256(:,:,9), nhel, den(32))
    call cont_SS(nsync, wf16(:,7), wf16(:,8), A(:,10), n3(:,325), t3x256(:,:,10), nhel, den(37))
    call cont_VV(nsync, wf16(:,10), wf16(:,11), A(:,11), n3(:,326), t3x256(:,:,11), nhel, den(42))
    call cont_SS(nsync, wf16(:,12), wf16(:,13), A(:,12), n3(:,327), t3x256(:,:,12), nhel, den(47))
    call cont_QA(nsync, wf8(:,4), wf32(:,1), A(:,13), n3(:,328), t3x256(:,:,13), nhel, den(54))
    call cont_QA(nsync, wf32(:,1), wf8(:,6), A(:,14), n3(:,329), t3x256(:,:,14), nhel, den(56))
    call cont_QA(nsync, wf8(:,4), wf32(:,2), A(:,15), n3(:,330), t3x256(:,:,15), nhel, den(58))
    call cont_QA(nsync, wf8(:,4), wf32(:,3), A(:,16), n3(:,331), t3x256(:,:,16), nhel, den(60))
    call cont_QA(nsync, wf8(:,6), wf32(:,2), A(:,17), n3(:,332), t3x256(:,:,17), nhel, den(61))
    call cont_QA(nsync, wf8(:,6), wf32(:,3), A(:,18), n3(:,333), t3x256(:,:,18), nhel, den(62))
    call cont_QA(nsync, wf8(:,8), wf32(:,4), A(:,19), n3(:,334), t3x256(:,:,19), nhel, den(66))
    call cont_QA(nsync, wf8(:,8), wf32(:,5), A(:,20), n3(:,335), t3x256(:,:,20), nhel, den(68))
    call cont_QA(nsync, wf32(:,4), wf8(:,10), A(:,21), n3(:,336), t3x256(:,:,21), nhel, den(70))
    call cont_QA(nsync, wf32(:,4), wf8(:,12), A(:,22), n3(:,337), t3x256(:,:,22), nhel, den(72))
    call cont_QA(nsync, wf32(:,5), wf8(:,10), A(:,23), n3(:,338), t3x256(:,:,23), nhel, den(73))
    call cont_QA(nsync, wf32(:,5), wf8(:,12), A(:,24), n3(:,339), t3x256(:,:,24), nhel, den(74))
    call cont_VV(nsync, wf16(:,14), wf16(:,15), A(:,25), n3(:,340), t3x256(:,:,25), nhel, den(77))
    call cont_SS(nsync, wf16(:,13), wf16(:,16), A(:,26), n3(:,341), t3x256(:,:,26), nhel, den(80))
    call cont_QA(nsync, wf8(:,16), wf32(:,6), A(:,27), n3(:,342), t3x256(:,:,27), nhel, den(86))
    call cont_QA(nsync, wf8(:,16), wf32(:,7), A(:,28), n3(:,343), t3x256(:,:,28), nhel, den(89))
    call cont_QA(nsync, wf8(:,16), wf32(:,8), A(:,29), n3(:,344), t3x256(:,:,29), nhel, den(91))
    call cont_QA(nsync, wf8(:,16), wf32(:,9), A(:,30), n3(:,345), t3x256(:,:,30), nhel, den(93))
    call cont_QA(nsync, wf8(:,16), wf32(:,10), A(:,31), n3(:,346), t3x256(:,:,31), nhel, den(95))
    call cont_QA(nsync, wf8(:,16), wf32(:,11), A(:,32), n3(:,347), t3x256(:,:,32), nhel, den(97))
    call cont_QA(nsync, wf8(:,16), wf32(:,12), A(:,33), n3(:,348), t3x256(:,:,33), nhel, den(101))
    call cont_QA(nsync, wf8(:,16), wf32(:,13), A(:,34), n3(:,349), t3x256(:,:,34), nhel, den(103))
    call cont_QA(nsync, wf8(:,16), wf32(:,14), A(:,35), n3(:,350), t3x256(:,:,35), nhel, den(106))
    call cont_QA(nsync, wf8(:,16), wf32(:,15), A(:,36), n3(:,351), t3x256(:,:,36), nhel, den(109))
    call cont_QA(nsync, wf8(:,16), wf32(:,16), A(:,37), n3(:,352), t3x256(:,:,37), nhel, den(111))
    call cont_QA(nsync, wf8(:,16), wf32(:,17), A(:,38), n3(:,353), t3x256(:,:,38), nhel, den(113))
    call cont_VV(nsync, wf16(:,14), wf16(:,17), A(:,39), n3(:,354), t3x256(:,:,39), nhel, den(114))
    call cont_SS(nsync, wf16(:,13), wf16(:,18), A(:,40), n3(:,355), t3x256(:,:,40), nhel, den(115))
    call cont_QA(nsync, wf8(:,8), wf32(:,18), A(:,41), n3(:,356), t3x256(:,:,41), nhel, den(117))
    call cont_QA(nsync, wf8(:,8), wf32(:,19), A(:,42), n3(:,357), t3x256(:,:,42), nhel, den(119))
    call cont_QA(nsync, wf8(:,10), wf32(:,18), A(:,43), n3(:,358), t3x256(:,:,43), nhel, den(120))
    call cont_QA(nsync, wf8(:,12), wf32(:,18), A(:,44), n3(:,359), t3x256(:,:,44), nhel, den(121))
    call cont_QA(nsync, wf8(:,10), wf32(:,19), A(:,45), n3(:,360), t3x256(:,:,45), nhel, den(122))
    call cont_QA(nsync, wf8(:,12), wf32(:,19), A(:,46), n3(:,361), t3x256(:,:,46), nhel, den(123))
    call cont_VV(nsync, wf16(:,6), wf16(:,19), A(:,47), n3(:,362), t3x256(:,:,47), nhel, den(124))
    call cont_VV(nsync, wf16(:,6), wf16(:,20), A(:,48), n3(:,363), t3x256(:,:,48), nhel, den(125))
    call cont_SS(nsync, wf16(:,7), wf16(:,21), A(:,49), n3(:,364), t3x256(:,:,49), nhel, den(126))
    call cont_SS(nsync, wf16(:,7), wf16(:,22), A(:,50), n3(:,365), t3x256(:,:,50), nhel, den(127))
    call cont_QA(nsync, wf8(:,4), wf32(:,20), A(:,51), n3(:,366), t3x256(:,:,51), nhel, den(129))
    call cont_QA(nsync, wf8(:,6), wf32(:,20), A(:,52), n3(:,367), t3x256(:,:,52), nhel, den(130))
    call cont_QA(nsync, wf8(:,4), wf32(:,21), A(:,53), n3(:,368), t3x256(:,:,53), nhel, den(132))
    call cont_QA(nsync, wf8(:,4), wf32(:,22), A(:,54), n3(:,369), t3x256(:,:,54), nhel, den(134))
    call cont_QA(nsync, wf8(:,6), wf32(:,21), A(:,55), n3(:,370), t3x256(:,:,55), nhel, den(135))
    call cont_QA(nsync, wf8(:,6), wf32(:,22), A(:,56), n3(:,371), t3x256(:,:,56), nhel, den(136))
    call cont_VV(nsync, wf16(:,6), wf16(:,23), A(:,57), n3(:,372), t3x256(:,:,57), nhel, den(137))
    call cont_VV(nsync, wf16(:,6), wf16(:,24), A(:,58), n3(:,373), t3x256(:,:,58), nhel, den(138))
    call cont_SS(nsync, wf16(:,7), wf16(:,25), A(:,59), n3(:,374), t3x256(:,:,59), nhel, den(139))
    call cont_SS(nsync, wf16(:,7), wf16(:,26), A(:,60), n3(:,375), t3x256(:,:,60), nhel, den(140))
    call cont_SS(nsync, wf16(:,1), wf16(:,27), A(:,61), n3(:,376), t3x256(:,:,61), nhel, den(141))
    call cont_SS(nsync, wf16(:,1), wf16(:,28), A(:,62), n3(:,377), t3x256(:,:,62), nhel, den(142))
    call cont_SS(nsync, wf16(:,1), wf16(:,29), A(:,63), n3(:,378), t3x256(:,:,63), nhel, den(143))
    call cont_SS(nsync, wf16(:,1), wf16(:,30), A(:,64), n3(:,379), t3x256(:,:,64), nhel, den(144))
    call cont_SS(nsync, wf16(:,1), wf16(:,31), A(:,65), n3(:,380), t3x256(:,:,65), nhel, den(145))
    call cont_SS(nsync, wf16(:,1), wf16(:,32), A(:,66), n3(:,381), t3x256(:,:,66), nhel, den(146))
    call cont_SS(nsync, wf4(:,4), wf64(:,5), A(:,67), n3(:,382), t3x256(:,:,67), nhel, den(152))
    call cont_SS(nsync, wf4(:,4), wf64(:,6), A(:,68), n3(:,383), t3x256(:,:,68), nhel, den(155))
    call cont_VV(nsync, wf4(:,12), wf64(:,7), A(:,69), n3(:,384), t3x256(:,:,69), nhel, den(156))
    call cont_VV(nsync, wf4(:,14), wf64(:,8), A(:,70), n3(:,385), t3x256(:,:,70), nhel, den(157))
    call cont_VV(nsync, wf4(:,12), wf64(:,9), A(:,71), n3(:,386), t3x256(:,:,71), nhel, den(158))
    call cont_VV(nsync, wf4(:,14), wf64(:,10), A(:,72), n3(:,387), t3x256(:,:,72), nhel, den(159))
    call cont_SS(nsync, wf16(:,8), wf16(:,33), A(:,73), n3(:,388), t3x256(:,:,73), nhel, den(162))
    call cont_VV(nsync, wf16(:,34), wf16(:,35), A(:,74), n3(:,389), t3x256(:,:,74), nhel, den(165))
    call cont_QA(nsync, wf8(:,32), wf32(:,23), A(:,75), n3(:,390), t3x256(:,:,75), nhel, den(169))
    call cont_QA(nsync, wf8(:,32), wf32(:,24), A(:,76), n3(:,391), t3x256(:,:,76), nhel, den(171))
    call cont_QA(nsync, wf32(:,23), wf8(:,34), A(:,77), n3(:,392), t3x256(:,:,77), nhel, den(173))
    call cont_QA(nsync, wf32(:,23), wf8(:,36), A(:,78), n3(:,393), t3x256(:,:,78), nhel, den(175))
    call cont_QA(nsync, wf32(:,24), wf8(:,34), A(:,79), n3(:,394), t3x256(:,:,79), nhel, den(176))
    call cont_QA(nsync, wf32(:,24), wf8(:,36), A(:,80), n3(:,395), t3x256(:,:,80), nhel, den(177))
    call cont_SS(nsync, wf4(:,4), wf64(:,11), A(:,81), n3(:,396), t3x256(:,:,81), nhel, den(183))
    call cont_SS(nsync, wf4(:,4), wf64(:,12), A(:,82), n3(:,397), t3x256(:,:,82), nhel, den(186))
    call cont_VV(nsync, wf4(:,12), wf64(:,13), A(:,83), n3(:,398), t3x256(:,:,83), nhel, den(187))
    call cont_VV(nsync, wf4(:,14), wf64(:,14), A(:,84), n3(:,399), t3x256(:,:,84), nhel, den(188))
    call cont_VV(nsync, wf4(:,12), wf64(:,15), A(:,85), n3(:,400), t3x256(:,:,85), nhel, den(189))
    call cont_VV(nsync, wf4(:,14), wf64(:,16), A(:,86), n3(:,401), t3x256(:,:,86), nhel, den(190))
    call cont_SS(nsync, wf16(:,8), wf16(:,36), A(:,87), n3(:,402), t3x256(:,:,87), nhel, den(191))
    call cont_VV(nsync, wf16(:,34), wf16(:,37), A(:,88), n3(:,403), t3x256(:,:,88), nhel, den(192))
    call cont_QA(nsync, wf8(:,40), wf32(:,25), A(:,89), n3(:,404), t3x256(:,:,89), nhel, den(196))
    call cont_QA(nsync, wf8(:,40), wf32(:,26), A(:,90), n3(:,405), t3x256(:,:,90), nhel, den(198))
    call cont_QA(nsync, wf8(:,40), wf32(:,27), A(:,91), n3(:,406), t3x256(:,:,91), nhel, den(201))
    call cont_QA(nsync, wf8(:,40), wf32(:,28), A(:,92), n3(:,407), t3x256(:,:,92), nhel, den(204))
    call cont_QA(nsync, wf8(:,40), wf32(:,29), A(:,93), n3(:,408), t3x256(:,:,93), nhel, den(206))
    call cont_QA(nsync, wf8(:,40), wf32(:,30), A(:,94), n3(:,409), t3x256(:,:,94), nhel, den(208))
    call cont_SS(nsync, wf16(:,12), wf16(:,38), A(:,95), n3(:,410), t3x256(:,:,95), nhel, den(209))
    call cont_SS(nsync, wf16(:,12), wf16(:,39), A(:,96), n3(:,411), t3x256(:,:,96), nhel, den(210))
    call cont_VV(nsync, wf16(:,11), wf16(:,40), A(:,97), n3(:,412), t3x256(:,:,97), nhel, den(211))
    call cont_VV(nsync, wf16(:,11), wf16(:,41), A(:,98), n3(:,413), t3x256(:,:,98), nhel, den(212))
    call cont_QA(nsync, wf8(:,32), wf32(:,31), A(:,99), n3(:,414), t3x256(:,:,99), nhel, den(214))
    call cont_QA(nsync, wf8(:,32), wf32(:,32), A(:,100), n3(:,415), t3x256(:,:,100), nhel, den(216))
    call cont_QA(nsync, wf8(:,34), wf32(:,31), A(:,101), n3(:,416), t3x256(:,:,101), nhel, den(217))
    call cont_QA(nsync, wf8(:,36), wf32(:,31), A(:,102), n3(:,417), t3x256(:,:,102), nhel, den(218))
    call cont_QA(nsync, wf8(:,34), wf32(:,32), A(:,103), n3(:,418), t3x256(:,:,103), nhel, den(219))
    call cont_QA(nsync, wf8(:,36), wf32(:,32), A(:,104), n3(:,419), t3x256(:,:,104), nhel, den(220))
    call cont_SS(nsync, wf16(:,12), wf16(:,42), A(:,105), n3(:,420), t3x256(:,:,105), nhel, den(221))
    call cont_SS(nsync, wf16(:,12), wf16(:,43), A(:,106), n3(:,421), t3x256(:,:,106), nhel, den(222))
    call cont_VV(nsync, wf16(:,11), wf16(:,44), A(:,107), n3(:,422), t3x256(:,:,107), nhel, den(223))
    call cont_VV(nsync, wf16(:,11), wf16(:,45), A(:,108), n3(:,423), t3x256(:,:,108), nhel, den(224))
    call cont_QA(nsync, wf8(:,28), wf32(:,33), A(:,109), n3(:,424), t3x256(:,:,109), nhel, den(226))
    call cont_QA(nsync, wf8(:,30), wf32(:,33), A(:,110), n3(:,425), t3x256(:,:,110), nhel, den(227))
    call cont_QA(nsync, wf8(:,28), wf32(:,34), A(:,111), n3(:,426), t3x256(:,:,111), nhel, den(229))
    call cont_QA(nsync, wf8(:,28), wf32(:,35), A(:,112), n3(:,427), t3x256(:,:,112), nhel, den(231))
    call cont_QA(nsync, wf8(:,30), wf32(:,34), A(:,113), n3(:,428), t3x256(:,:,113), nhel, den(232))
    call cont_QA(nsync, wf8(:,30), wf32(:,35), A(:,114), n3(:,429), t3x256(:,:,114), nhel, den(233))
    call cont_SS(nsync, wf16(:,1), wf16(:,46), A(:,115), n3(:,430), t3x256(:,:,115), nhel, den(234))
    call cont_SS(nsync, wf16(:,1), wf16(:,47), A(:,116), n3(:,431), t3x256(:,:,116), nhel, den(235))
    call cont_SS(nsync, wf16(:,1), wf16(:,48), A(:,117), n3(:,432), t3x256(:,:,117), nhel, den(236))
    call cont_SS(nsync, wf16(:,1), wf16(:,49), A(:,118), n3(:,433), t3x256(:,:,118), nhel, den(237))
    call cont_SS(nsync, wf16(:,1), wf16(:,50), A(:,119), n3(:,434), t3x256(:,:,119), nhel, den(238))
    call cont_SS(nsync, wf16(:,1), wf16(:,51), A(:,120), n3(:,435), t3x256(:,:,120), nhel, den(239))
    call cont_SS(nsync, wf16(:,21), wf16(:,33), A(:,121), n3(:,436), t3x256(:,:,121), nhel, den(241))
    call cont_SS(nsync, wf16(:,22), wf16(:,33), A(:,122), n3(:,437), t3x256(:,:,122), nhel, den(242))
    call cont_VV(nsync, wf16(:,52), wf16(:,53), A(:,123), n3(:,438), t3x256(:,:,123), nhel, den(245))
    call cont_VV(nsync, wf16(:,19), wf16(:,54), A(:,124), n3(:,439), t3x256(:,:,124), nhel, den(247))
    call cont_VV(nsync, wf16(:,52), wf16(:,55), A(:,125), n3(:,440), t3x256(:,:,125), nhel, den(248))
    call cont_VV(nsync, wf16(:,20), wf16(:,54), A(:,126), n3(:,441), t3x256(:,:,126), nhel, den(249))
    call cont_SS(nsync, wf16(:,25), wf16(:,33), A(:,127), n3(:,442), t3x256(:,:,127), nhel, den(250))
    call cont_SS(nsync, wf16(:,26), wf16(:,33), A(:,128), n3(:,443), t3x256(:,:,128), nhel, den(251))
    call cont_VV(nsync, wf16(:,52), wf16(:,56), A(:,129), n3(:,444), t3x256(:,:,129), nhel, den(252))
    call cont_VV(nsync, wf16(:,23), wf16(:,54), A(:,130), n3(:,445), t3x256(:,:,130), nhel, den(253))
    call cont_VV(nsync, wf16(:,52), wf16(:,57), A(:,131), n3(:,446), t3x256(:,:,131), nhel, den(254))
    call cont_VV(nsync, wf16(:,24), wf16(:,54), A(:,132), n3(:,447), t3x256(:,:,132), nhel, den(255))
    call cont_SS(nsync, wf16(:,21), wf16(:,36), A(:,133), n3(:,448), t3x256(:,:,133), nhel, den(257))
    call cont_SS(nsync, wf16(:,22), wf16(:,36), A(:,134), n3(:,449), t3x256(:,:,134), nhel, den(258))
    call cont_VV(nsync, wf16(:,53), wf16(:,58), A(:,135), n3(:,450), t3x256(:,:,135), nhel, den(260))
    call cont_VV(nsync, wf16(:,19), wf16(:,59), A(:,136), n3(:,451), t3x256(:,:,136), nhel, den(262))
    call cont_VV(nsync, wf16(:,55), wf16(:,58), A(:,137), n3(:,452), t3x256(:,:,137), nhel, den(263))
    call cont_VV(nsync, wf16(:,20), wf16(:,59), A(:,138), n3(:,453), t3x256(:,:,138), nhel, den(264))
    call cont_SS(nsync, wf16(:,25), wf16(:,36), A(:,139), n3(:,454), t3x256(:,:,139), nhel, den(265))
    call cont_SS(nsync, wf16(:,26), wf16(:,36), A(:,140), n3(:,455), t3x256(:,:,140), nhel, den(266))
    call cont_VV(nsync, wf16(:,56), wf16(:,58), A(:,141), n3(:,456), t3x256(:,:,141), nhel, den(267))
    call cont_VV(nsync, wf16(:,23), wf16(:,59), A(:,142), n3(:,457), t3x256(:,:,142), nhel, den(268))
    call cont_VV(nsync, wf16(:,57), wf16(:,58), A(:,143), n3(:,458), t3x256(:,:,143), nhel, den(269))
    call cont_VV(nsync, wf16(:,24), wf16(:,59), A(:,144), n3(:,459), t3x256(:,:,144), nhel, den(270))
    call cont_SS(nsync, wf16(:,16), wf16(:,38), A(:,145), n3(:,460), t3x256(:,:,145), nhel, den(272))
    call cont_SS(nsync, wf16(:,16), wf16(:,39), A(:,146), n3(:,461), t3x256(:,:,146), nhel, den(274))
    call cont_VV(nsync, wf16(:,60), wf16(:,61), A(:,147), n3(:,462), t3x256(:,:,147), nhel, den(277))
    call cont_VV(nsync, wf16(:,15), wf16(:,62), A(:,148), n3(:,463), t3x256(:,:,148), nhel, den(279))
    call cont_VV(nsync, wf16(:,61), wf16(:,63), A(:,149), n3(:,464), t3x256(:,:,149), nhel, den(281))
    call cont_VV(nsync, wf16(:,15), wf16(:,64), A(:,150), n3(:,465), t3x256(:,:,150), nhel, den(283))
    call cont_SS(nsync, wf16(:,16), wf16(:,42), A(:,151), n3(:,466), t3x256(:,:,151), nhel, den(285))
    call cont_SS(nsync, wf16(:,16), wf16(:,43), A(:,152), n3(:,467), t3x256(:,:,152), nhel, den(287))
    call cont_VV(nsync, wf16(:,61), wf16(:,65), A(:,153), n3(:,468), t3x256(:,:,153), nhel, den(289))
    call cont_VV(nsync, wf16(:,15), wf16(:,66), A(:,154), n3(:,469), t3x256(:,:,154), nhel, den(291))
    call cont_VV(nsync, wf16(:,61), wf16(:,67), A(:,155), n3(:,470), t3x256(:,:,155), nhel, den(293))
    call cont_VV(nsync, wf16(:,15), wf16(:,68), A(:,156), n3(:,471), t3x256(:,:,156), nhel, den(295))
    call cont_SS(nsync, wf16(:,18), wf16(:,38), A(:,157), n3(:,472), t3x256(:,:,157), nhel, den(296))
    call cont_SS(nsync, wf16(:,18), wf16(:,39), A(:,158), n3(:,473), t3x256(:,:,158), nhel, den(297))
    call cont_VV(nsync, wf16(:,60), wf16(:,69), A(:,159), n3(:,474), t3x256(:,:,159), nhel, den(298))
    call cont_VV(nsync, wf16(:,17), wf16(:,62), A(:,160), n3(:,475), t3x256(:,:,160), nhel, den(299))
    call cont_VV(nsync, wf16(:,63), wf16(:,69), A(:,161), n3(:,476), t3x256(:,:,161), nhel, den(300))
    call cont_VV(nsync, wf16(:,17), wf16(:,64), A(:,162), n3(:,477), t3x256(:,:,162), nhel, den(301))
    call cont_SS(nsync, wf16(:,18), wf16(:,42), A(:,163), n3(:,478), t3x256(:,:,163), nhel, den(302))
    call cont_SS(nsync, wf16(:,18), wf16(:,43), A(:,164), n3(:,479), t3x256(:,:,164), nhel, den(303))
    call cont_VV(nsync, wf16(:,65), wf16(:,69), A(:,165), n3(:,480), t3x256(:,:,165), nhel, den(304))
    call cont_VV(nsync, wf16(:,17), wf16(:,66), A(:,166), n3(:,481), t3x256(:,:,166), nhel, den(305))
    call cont_VV(nsync, wf16(:,67), wf16(:,69), A(:,167), n3(:,482), t3x256(:,:,167), nhel, den(306))
    call cont_VV(nsync, wf16(:,17), wf16(:,68), A(:,168), n3(:,483), t3x256(:,:,168), nhel, den(307))
    call cont_VV(nsync, wf16(:,71), wf16(:,72), A(:,169), n3(:,484), t3x256(:,:,169), nhel, den(313))
    call cont_VV(nsync, wf16(:,72), wf16(:,74), A(:,170), n3(:,485), t3x256(:,:,170), nhel, den(316))
    call cont_VV(nsync, wf4(:,12), wf64(:,17), A(:,171), n3(:,486), t3x256(:,:,171), nhel, den(320))
    call cont_VV(nsync, wf4(:,14), wf64(:,18), A(:,172), n3(:,487), t3x256(:,:,172), nhel, den(321))
    call cont_VV(nsync, wf4(:,12), wf64(:,19), A(:,173), n3(:,488), t3x256(:,:,173), nhel, den(324))
    call cont_VV(nsync, wf4(:,14), wf64(:,20), A(:,174), n3(:,489), t3x256(:,:,174), nhel, den(325))
    call cont_VV(nsync, wf4(:,11), wf64(:,21), A(:,175), n3(:,490), t3x256(:,:,175), nhel, den(329))
    call cont_VV(nsync, wf4(:,16), wf64(:,22), A(:,176), n3(:,491), t3x256(:,:,176), nhel, den(330))
    call cont_VV(nsync, wf4(:,11), wf64(:,23), A(:,177), n3(:,492), t3x256(:,:,177), nhel, den(333))
    call cont_VV(nsync, wf4(:,16), wf64(:,24), A(:,178), n3(:,493), t3x256(:,:,178), nhel, den(334))
    call cont_VV(nsync, wf16(:,71), wf16(:,75), A(:,179), n3(:,494), t3x256(:,:,179), nhel, den(337))
    call cont_VV(nsync, wf16(:,74), wf16(:,75), A(:,180), n3(:,495), t3x256(:,:,180), nhel, den(338))
    call cont_VV(nsync, wf4(:,12), wf64(:,25), A(:,181), n3(:,496), t3x256(:,:,181), nhel, den(342))
    call cont_VV(nsync, wf4(:,14), wf64(:,26), A(:,182), n3(:,497), t3x256(:,:,182), nhel, den(343))
    call cont_VV(nsync, wf4(:,12), wf64(:,27), A(:,183), n3(:,498), t3x256(:,:,183), nhel, den(346))
    call cont_VV(nsync, wf4(:,14), wf64(:,28), A(:,184), n3(:,499), t3x256(:,:,184), nhel, den(347))
    call cont_VV(nsync, wf4(:,11), wf64(:,29), A(:,185), n3(:,500), t3x256(:,:,185), nhel, den(351))
    call cont_VV(nsync, wf4(:,16), wf64(:,30), A(:,186), n3(:,501), t3x256(:,:,186), nhel, den(352))
    call cont_VV(nsync, wf4(:,11), wf64(:,31), A(:,187), n3(:,502), t3x256(:,:,187), nhel, den(355))
    call cont_VV(nsync, wf4(:,16), wf64(:,32), A(:,188), n3(:,503), t3x256(:,:,188), nhel, den(356))
    call cont_VV(nsync, wf16(:,11), wf16(:,76), A(:,189), n3(:,504), t3x256(:,:,189), nhel, den(357))
    call cont_VV(nsync, wf16(:,11), wf16(:,77), A(:,190), n3(:,505), t3x256(:,:,190), nhel, den(358))
    call cont_VV(nsync, wf16(:,6), wf16(:,78), A(:,191), n3(:,506), t3x256(:,:,191), nhel, den(359))
    call cont_VV(nsync, wf16(:,6), wf16(:,79), A(:,192), n3(:,507), t3x256(:,:,192), nhel, den(360))
    call cont_VV(nsync, wf16(:,11), wf16(:,80), A(:,193), n3(:,508), t3x256(:,:,193), nhel, den(361))
    call cont_VV(nsync, wf16(:,11), wf16(:,81), A(:,194), n3(:,509), t3x256(:,:,194), nhel, den(362))
    call cont_VV(nsync, wf16(:,6), wf16(:,82), A(:,195), n3(:,510), t3x256(:,:,195), nhel, den(363))
    call cont_VV(nsync, wf16(:,6), wf16(:,83), A(:,196), n3(:,511), t3x256(:,:,196), nhel, den(364))
    call cont_QA(nsync, wf8(:,56), wf32(:,36), A(:,197), n3(:,512), t3x256(:,:,197), nhel, den(366))
    call cont_QA(nsync, wf8(:,58), wf32(:,36), A(:,198), n3(:,513), t3x256(:,:,198), nhel, den(367))
    call cont_QA(nsync, wf8(:,56), wf32(:,37), A(:,199), n3(:,514), t3x256(:,:,199), nhel, den(369))
    call cont_QA(nsync, wf8(:,58), wf32(:,37), A(:,200), n3(:,515), t3x256(:,:,200), nhel, den(370))
    call cont_QA(nsync, wf8(:,52), wf32(:,38), A(:,201), n3(:,516), t3x256(:,:,201), nhel, den(372))
    call cont_QA(nsync, wf8(:,54), wf32(:,38), A(:,202), n3(:,517), t3x256(:,:,202), nhel, den(373))
    call cont_QA(nsync, wf8(:,52), wf32(:,39), A(:,203), n3(:,518), t3x256(:,:,203), nhel, den(375))
    call cont_QA(nsync, wf8(:,54), wf32(:,39), A(:,204), n3(:,519), t3x256(:,:,204), nhel, den(376))
    call cont_VV(nsync, wf16(:,84), wf16(:,85), A(:,205), n3(:,520), t3x256(:,:,205), nhel, den(379))
    call cont_VV(nsync, wf16(:,86), wf16(:,87), A(:,206), n3(:,521), t3x256(:,:,206), nhel, den(382))
    call cont_VV(nsync, wf16(:,84), wf16(:,88), A(:,207), n3(:,522), t3x256(:,:,207), nhel, den(383))
    call cont_VV(nsync, wf16(:,84), wf16(:,89), A(:,208), n3(:,523), t3x256(:,:,208), nhel, den(384))
    call cont_VV(nsync, wf16(:,87), wf16(:,90), A(:,209), n3(:,524), t3x256(:,:,209), nhel, den(385))
    call cont_VV(nsync, wf16(:,87), wf16(:,91), A(:,210), n3(:,525), t3x256(:,:,210), nhel, den(386))
    call cont_VV(nsync, wf16(:,84), wf16(:,92), A(:,211), n3(:,526), t3x256(:,:,211), nhel, den(387))
    call cont_VV(nsync, wf16(:,87), wf16(:,93), A(:,212), n3(:,527), t3x256(:,:,212), nhel, den(388))
    call cont_VV(nsync, wf16(:,84), wf16(:,94), A(:,213), n3(:,528), t3x256(:,:,213), nhel, den(389))
    call cont_VV(nsync, wf16(:,84), wf16(:,95), A(:,214), n3(:,529), t3x256(:,:,214), nhel, den(390))
    call cont_VV(nsync, wf16(:,87), wf16(:,96), A(:,215), n3(:,530), t3x256(:,:,215), nhel, den(391))
    call cont_VV(nsync, wf16(:,87), wf16(:,97), A(:,216), n3(:,531), t3x256(:,:,216), nhel, den(392))
    call cont_VV(nsync, wf16(:,85), wf16(:,98), A(:,217), n3(:,532), t3x256(:,:,217), nhel, den(394))
    call cont_VV(nsync, wf16(:,86), wf16(:,99), A(:,218), n3(:,533), t3x256(:,:,218), nhel, den(396))
    call cont_VV(nsync, wf16(:,88), wf16(:,98), A(:,219), n3(:,534), t3x256(:,:,219), nhel, den(397))
    call cont_VV(nsync, wf16(:,89), wf16(:,98), A(:,220), n3(:,535), t3x256(:,:,220), nhel, den(398))
    call cont_VV(nsync, wf16(:,90), wf16(:,99), A(:,221), n3(:,536), t3x256(:,:,221), nhel, den(399))
    call cont_VV(nsync, wf16(:,91), wf16(:,99), A(:,222), n3(:,537), t3x256(:,:,222), nhel, den(400))
    call cont_VV(nsync, wf16(:,92), wf16(:,98), A(:,223), n3(:,538), t3x256(:,:,223), nhel, den(401))
    call cont_VV(nsync, wf16(:,93), wf16(:,99), A(:,224), n3(:,539), t3x256(:,:,224), nhel, den(402))
    call cont_VV(nsync, wf16(:,94), wf16(:,98), A(:,225), n3(:,540), t3x256(:,:,225), nhel, den(403))
    call cont_VV(nsync, wf16(:,95), wf16(:,98), A(:,226), n3(:,541), t3x256(:,:,226), nhel, den(404))
    call cont_VV(nsync, wf16(:,96), wf16(:,99), A(:,227), n3(:,542), t3x256(:,:,227), nhel, den(405))
    call cont_VV(nsync, wf16(:,97), wf16(:,99), A(:,228), n3(:,543), t3x256(:,:,228), nhel, den(406))
    call cont_VV(nsync, wf16(:,61), wf16(:,100), A(:,229), n3(:,544), t3x256(:,:,229), nhel, den(408))
    call cont_VV(nsync, wf16(:,15), wf16(:,101), A(:,230), n3(:,545), t3x256(:,:,230), nhel, den(410))
    call cont_VV(nsync, wf16(:,61), wf16(:,102), A(:,231), n3(:,546), t3x256(:,:,231), nhel, den(412))
    call cont_VV(nsync, wf16(:,15), wf16(:,103), A(:,232), n3(:,547), t3x256(:,:,232), nhel, den(414))
    call cont_VV(nsync, wf16(:,61), wf16(:,104), A(:,233), n3(:,548), t3x256(:,:,233), nhel, den(416))
    call cont_VV(nsync, wf16(:,61), wf16(:,105), A(:,234), n3(:,549), t3x256(:,:,234), nhel, den(418))
    call cont_VV(nsync, wf16(:,15), wf16(:,106), A(:,235), n3(:,550), t3x256(:,:,235), nhel, den(420))
    call cont_VV(nsync, wf16(:,15), wf16(:,107), A(:,236), n3(:,551), t3x256(:,:,236), nhel, den(422))
    call cont_VV(nsync, wf16(:,69), wf16(:,100), A(:,237), n3(:,552), t3x256(:,:,237), nhel, den(423))
    call cont_VV(nsync, wf16(:,17), wf16(:,101), A(:,238), n3(:,553), t3x256(:,:,238), nhel, den(424))
    call cont_VV(nsync, wf16(:,69), wf16(:,102), A(:,239), n3(:,554), t3x256(:,:,239), nhel, den(425))
    call cont_VV(nsync, wf16(:,17), wf16(:,103), A(:,240), n3(:,555), t3x256(:,:,240), nhel, den(426))
    call cont_VV(nsync, wf16(:,69), wf16(:,104), A(:,241), n3(:,556), t3x256(:,:,241), nhel, den(427))
    call cont_VV(nsync, wf16(:,69), wf16(:,105), A(:,242), n3(:,557), t3x256(:,:,242), nhel, den(428))
    call cont_VV(nsync, wf16(:,17), wf16(:,106), A(:,243), n3(:,558), t3x256(:,:,243), nhel, den(429))
    call cont_VV(nsync, wf16(:,17), wf16(:,107), A(:,244), n3(:,559), t3x256(:,:,244), nhel, den(430))
    call cont_VV(nsync, wf16(:,84), wf16(:,108), A(:,245), n3(:,560), t3x256(:,:,245), nhel, den(431))
    call cont_VV(nsync, wf16(:,87), wf16(:,109), A(:,246), n3(:,561), t3x256(:,:,246), nhel, den(432))
    call cont_VV(nsync, wf16(:,84), wf16(:,110), A(:,247), n3(:,562), t3x256(:,:,247), nhel, den(433))
    call cont_VV(nsync, wf16(:,84), wf16(:,111), A(:,248), n3(:,563), t3x256(:,:,248), nhel, den(434))
    call cont_VV(nsync, wf16(:,87), wf16(:,112), A(:,249), n3(:,564), t3x256(:,:,249), nhel, den(435))
    call cont_VV(nsync, wf16(:,87), wf16(:,113), A(:,250), n3(:,565), t3x256(:,:,250), nhel, den(436))
    call cont_VV(nsync, wf16(:,84), wf16(:,114), A(:,251), n3(:,566), t3x256(:,:,251), nhel, den(437))
    call cont_VV(nsync, wf16(:,87), wf16(:,115), A(:,252), n3(:,567), t3x256(:,:,252), nhel, den(438))
    call cont_VV(nsync, wf16(:,84), wf16(:,116), A(:,253), n3(:,568), t3x256(:,:,253), nhel, den(439))
    call cont_VV(nsync, wf16(:,84), wf16(:,117), A(:,254), n3(:,569), t3x256(:,:,254), nhel, den(440))
    call cont_VV(nsync, wf16(:,87), wf16(:,118), A(:,255), n3(:,570), t3x256(:,:,255), nhel, den(441))
    call cont_VV(nsync, wf16(:,87), wf16(:,119), A(:,256), n3(:,571), t3x256(:,:,256), nhel, den(442))
    call cont_VV(nsync, wf16(:,98), wf16(:,108), A(:,257), n3(:,572), t3x256(:,:,257), nhel, den(443))
    call cont_VV(nsync, wf16(:,99), wf16(:,109), A(:,258), n3(:,573), t3x256(:,:,258), nhel, den(444))
    call cont_VV(nsync, wf16(:,98), wf16(:,110), A(:,259), n3(:,574), t3x256(:,:,259), nhel, den(445))
    call cont_VV(nsync, wf16(:,98), wf16(:,111), A(:,260), n3(:,575), t3x256(:,:,260), nhel, den(446))
    call cont_VV(nsync, wf16(:,99), wf16(:,112), A(:,261), n3(:,576), t3x256(:,:,261), nhel, den(447))
    call cont_VV(nsync, wf16(:,99), wf16(:,113), A(:,262), n3(:,577), t3x256(:,:,262), nhel, den(448))
    call cont_VV(nsync, wf16(:,98), wf16(:,114), A(:,263), n3(:,578), t3x256(:,:,263), nhel, den(449))
    call cont_VV(nsync, wf16(:,99), wf16(:,115), A(:,264), n3(:,579), t3x256(:,:,264), nhel, den(450))
    call cont_VV(nsync, wf16(:,98), wf16(:,116), A(:,265), n3(:,580), t3x256(:,:,265), nhel, den(451))
    call cont_VV(nsync, wf16(:,98), wf16(:,117), A(:,266), n3(:,581), t3x256(:,:,266), nhel, den(452))
    call cont_VV(nsync, wf16(:,99), wf16(:,118), A(:,267), n3(:,582), t3x256(:,:,267), nhel, den(453))
    call cont_VV(nsync, wf16(:,99), wf16(:,119), A(:,268), n3(:,583), t3x256(:,:,268), nhel, den(454))
    call cont_VV(nsync, wf16(:,52), wf16(:,120), A(:,269), n3(:,584), t3x256(:,:,269), nhel, den(457))
    call cont_VV(nsync, wf16(:,52), wf16(:,121), A(:,270), n3(:,585), t3x256(:,:,270), nhel, den(459))
    call cont_VV(nsync, wf16(:,35), wf16(:,122), A(:,271), n3(:,586), t3x256(:,:,271), nhel, den(461))
    call cont_VV(nsync, wf16(:,35), wf16(:,123), A(:,272), n3(:,587), t3x256(:,:,272), nhel, den(463))
    call cont_VV(nsync, wf16(:,52), wf16(:,124), A(:,273), n3(:,588), t3x256(:,:,273), nhel, den(465))
    call cont_VV(nsync, wf16(:,35), wf16(:,125), A(:,274), n3(:,589), t3x256(:,:,274), nhel, den(467))
    call cont_VV(nsync, wf16(:,52), wf16(:,126), A(:,275), n3(:,590), t3x256(:,:,275), nhel, den(469))
    call cont_VV(nsync, wf16(:,35), wf16(:,127), A(:,276), n3(:,591), t3x256(:,:,276), nhel, den(471))
    call cont_VV(nsync, wf16(:,58), wf16(:,120), A(:,277), n3(:,592), t3x256(:,:,277), nhel, den(472))
    call cont_VV(nsync, wf16(:,58), wf16(:,121), A(:,278), n3(:,593), t3x256(:,:,278), nhel, den(473))
    call cont_VV(nsync, wf16(:,37), wf16(:,122), A(:,279), n3(:,594), t3x256(:,:,279), nhel, den(474))
    call cont_VV(nsync, wf16(:,37), wf16(:,123), A(:,280), n3(:,595), t3x256(:,:,280), nhel, den(475))
    call cont_VV(nsync, wf16(:,58), wf16(:,124), A(:,281), n3(:,596), t3x256(:,:,281), nhel, den(476))
    call cont_VV(nsync, wf16(:,37), wf16(:,125), A(:,282), n3(:,597), t3x256(:,:,282), nhel, den(477))
    call cont_VV(nsync, wf16(:,58), wf16(:,126), A(:,283), n3(:,598), t3x256(:,:,283), nhel, den(478))
    call cont_VV(nsync, wf16(:,37), wf16(:,127), A(:,284), n3(:,599), t3x256(:,:,284), nhel, den(479))
    call cont_SS(nsync, wf16(:,2), wf16(:,128), A(:,285), n3(:,600), t3x256(:,:,285), nhel, den(482))
    call cont_VV(nsync, wf16(:,70), wf16(:,130), A(:,286), n3(:,601), t3x256(:,:,286), nhel, den(484))
    call cont_VV(nsync, wf16(:,73), wf16(:,130), A(:,287), n3(:,602), t3x256(:,:,287), nhel, den(485))
    call cont_SS(nsync, wf16(:,3), wf16(:,128), A(:,288), n3(:,603), t3x256(:,:,288), nhel, den(486))
    call cont_VV(nsync, wf16(:,132), wf16(:,133), A(:,289), n3(:,604), t3x256(:,:,289), nhel, den(491))
    call cont_VV(nsync, wf16(:,133), wf16(:,134), A(:,290), n3(:,605), t3x256(:,:,290), nhel, den(493))
    call cont_VV(nsync, wf16(:,133), wf16(:,135), A(:,291), n3(:,606), t3x256(:,:,291), nhel, den(495))
    call cont_VV(nsync, wf16(:,132), wf16(:,137), A(:,292), n3(:,607), t3x256(:,:,292), nhel, den(498))
    call cont_VV(nsync, wf16(:,132), wf16(:,139), A(:,293), n3(:,608), t3x256(:,:,293), nhel, den(501))
    call cont_VV(nsync, wf16(:,134), wf16(:,137), A(:,294), n3(:,609), t3x256(:,:,294), nhel, den(502))
    call cont_VV(nsync, wf16(:,135), wf16(:,137), A(:,295), n3(:,610), t3x256(:,:,295), nhel, den(503))
    call cont_VV(nsync, wf16(:,134), wf16(:,139), A(:,296), n3(:,611), t3x256(:,:,296), nhel, den(504))
    call cont_VV(nsync, wf16(:,135), wf16(:,139), A(:,297), n3(:,612), t3x256(:,:,297), nhel, den(505))
    call cont_VV(nsync, wf16(:,141), wf16(:,142), A(:,298), n3(:,613), t3x256(:,:,298), nhel, den(510))
    call cont_VV(nsync, wf16(:,141), wf16(:,144), A(:,299), n3(:,614), t3x256(:,:,299), nhel, den(513))
    call cont_VV(nsync, wf16(:,141), wf16(:,146), A(:,300), n3(:,615), t3x256(:,:,300), nhel, den(516))
    call cont_VV(nsync, wf16(:,142), wf16(:,147), A(:,301), n3(:,616), t3x256(:,:,301), nhel, den(518))
    call cont_VV(nsync, wf16(:,142), wf16(:,148), A(:,302), n3(:,617), t3x256(:,:,302), nhel, den(520))
    call cont_VV(nsync, wf16(:,144), wf16(:,147), A(:,303), n3(:,618), t3x256(:,:,303), nhel, den(521))
    call cont_VV(nsync, wf16(:,146), wf16(:,147), A(:,304), n3(:,619), t3x256(:,:,304), nhel, den(522))
    call cont_VV(nsync, wf16(:,144), wf16(:,148), A(:,305), n3(:,620), t3x256(:,:,305), nhel, den(523))
    call cont_VV(nsync, wf16(:,146), wf16(:,148), A(:,306), n3(:,621), t3x256(:,:,306), nhel, den(524))
    call cont_QA(nsync, wf8(:,72), wf32(:,40), A(:,307), n3(:,622), t3x256(:,:,307), nhel, den(530))
    call cont_QA(nsync, wf8(:,72), wf32(:,41), A(:,308), n3(:,623), t3x256(:,:,308), nhel, den(532))
    call cont_QA(nsync, wf8(:,72), wf32(:,42), A(:,309), n3(:,624), t3x256(:,:,309), nhel, den(534))
    call cont_QA(nsync, wf8(:,8), wf32(:,43), A(:,310), n3(:,625), t3x256(:,:,310), nhel, den(536))
    call cont_QA(nsync, wf8(:,10), wf32(:,43), A(:,311), n3(:,626), t3x256(:,:,311), nhel, den(537))
    call cont_QA(nsync, wf8(:,12), wf32(:,43), A(:,312), n3(:,627), t3x256(:,:,312), nhel, den(538))
    call cont_VV(nsync, wf16(:,149), wf16(:,150), A(:,313), n3(:,628), t3x256(:,:,313), nhel, den(541))
    call cont_VV(nsync, wf16(:,150), wf16(:,151), A(:,314), n3(:,629), t3x256(:,:,314), nhel, den(543))
    call cont_VV(nsync, wf16(:,150), wf16(:,152), A(:,315), n3(:,630), t3x256(:,:,315), nhel, den(545))
    call cont_QA(nsync, wf8(:,72), wf32(:,44), A(:,316), n3(:,631), t3x256(:,:,316), nhel, den(547))
    call cont_QA(nsync, wf8(:,72), wf32(:,45), A(:,317), n3(:,632), t3x256(:,:,317), nhel, den(549))
    call cont_QA(nsync, wf8(:,72), wf32(:,46), A(:,318), n3(:,633), t3x256(:,:,318), nhel, den(551))
    call cont_VV(nsync, wf16(:,133), wf16(:,153), A(:,319), n3(:,634), t3x256(:,:,319), nhel, den(552))
    call cont_VV(nsync, wf16(:,137), wf16(:,153), A(:,320), n3(:,635), t3x256(:,:,320), nhel, den(553))
    call cont_VV(nsync, wf16(:,139), wf16(:,153), A(:,321), n3(:,636), t3x256(:,:,321), nhel, den(554))
    call cont_SS(nsync, wf16(:,27), wf16(:,128), A(:,322), n3(:,637), t3x256(:,:,322), nhel, den(555))
    call cont_VV(nsync, wf16(:,92), wf16(:,129), A(:,323), n3(:,638), t3x256(:,:,323), nhel, den(557))
    call cont_VV(nsync, wf16(:,93), wf16(:,130), A(:,324), n3(:,639), t3x256(:,:,324), nhel, den(558))
    call cont_SS(nsync, wf16(:,28), wf16(:,128), A(:,325), n3(:,640), t3x256(:,:,325), nhel, den(559))
    call cont_SS(nsync, wf16(:,29), wf16(:,128), A(:,326), n3(:,641), t3x256(:,:,326), nhel, den(560))
    call cont_VV(nsync, wf16(:,94), wf16(:,129), A(:,327), n3(:,642), t3x256(:,:,327), nhel, den(561))
    call cont_VV(nsync, wf16(:,96), wf16(:,130), A(:,328), n3(:,643), t3x256(:,:,328), nhel, den(562))
    call cont_VV(nsync, wf16(:,95), wf16(:,129), A(:,329), n3(:,644), t3x256(:,:,329), nhel, den(563))
    call cont_VV(nsync, wf16(:,97), wf16(:,130), A(:,330), n3(:,645), t3x256(:,:,330), nhel, den(564))
    call cont_SS(nsync, wf16(:,30), wf16(:,128), A(:,331), n3(:,646), t3x256(:,:,331), nhel, den(565))
    call cont_VV(nsync, wf16(:,85), wf16(:,129), A(:,332), n3(:,647), t3x256(:,:,332), nhel, den(566))
    call cont_VV(nsync, wf16(:,86), wf16(:,130), A(:,333), n3(:,648), t3x256(:,:,333), nhel, den(567))
    call cont_SS(nsync, wf16(:,31), wf16(:,128), A(:,334), n3(:,649), t3x256(:,:,334), nhel, den(568))
    call cont_SS(nsync, wf16(:,32), wf16(:,128), A(:,335), n3(:,650), t3x256(:,:,335), nhel, den(569))
    call cont_VV(nsync, wf16(:,88), wf16(:,129), A(:,336), n3(:,651), t3x256(:,:,336), nhel, den(570))
    call cont_VV(nsync, wf16(:,90), wf16(:,130), A(:,337), n3(:,652), t3x256(:,:,337), nhel, den(571))
    call cont_VV(nsync, wf16(:,89), wf16(:,129), A(:,338), n3(:,653), t3x256(:,:,338), nhel, den(572))
    call cont_VV(nsync, wf16(:,91), wf16(:,130), A(:,339), n3(:,654), t3x256(:,:,339), nhel, den(573))
    call cont_VV(nsync, wf4(:,14), wf64(:,33), A(:,340), n3(:,655), t3x256(:,:,340), nhel, den(579))
    call cont_VV(nsync, wf16(:,154), wf16(:,155), A(:,341), n3(:,656), t3x256(:,:,341), nhel, den(582))
    call cont_VV(nsync, wf16(:,155), wf16(:,156), A(:,342), n3(:,657), t3x256(:,:,342), nhel, den(584))
    call cont_VV(nsync, wf16(:,155), wf16(:,157), A(:,343), n3(:,658), t3x256(:,:,343), nhel, den(586))
    call cont_QA(nsync, wf8(:,32), wf32(:,47), A(:,344), n3(:,659), t3x256(:,:,344), nhel, den(588))
    call cont_QA(nsync, wf8(:,34), wf32(:,47), A(:,345), n3(:,660), t3x256(:,:,345), nhel, den(589))
    call cont_QA(nsync, wf8(:,36), wf32(:,47), A(:,346), n3(:,661), t3x256(:,:,346), nhel, den(590))
    call cont_VV(nsync, wf16(:,142), wf16(:,158), A(:,347), n3(:,662), t3x256(:,:,347), nhel, den(591))
    call cont_VV(nsync, wf16(:,144), wf16(:,158), A(:,348), n3(:,663), t3x256(:,:,348), nhel, den(592))
    call cont_VV(nsync, wf16(:,146), wf16(:,158), A(:,349), n3(:,664), t3x256(:,:,349), nhel, den(593))
    call cont_QA(nsync, wf8(:,76), wf32(:,48), A(:,350), n3(:,665), t3x256(:,:,350), nhel, den(595))
    call cont_QA(nsync, wf8(:,76), wf32(:,49), A(:,351), n3(:,666), t3x256(:,:,351), nhel, den(597))
    call cont_QA(nsync, wf8(:,76), wf32(:,50), A(:,352), n3(:,667), t3x256(:,:,352), nhel, den(599))
    call cont_SS(nsync, wf16(:,46), wf16(:,128), A(:,353), n3(:,668), t3x256(:,:,353), nhel, den(600))
    call cont_VV(nsync, wf16(:,114), wf16(:,129), A(:,354), n3(:,669), t3x256(:,:,354), nhel, den(601))
    call cont_VV(nsync, wf16(:,115), wf16(:,130), A(:,355), n3(:,670), t3x256(:,:,355), nhel, den(602))
    call cont_SS(nsync, wf16(:,47), wf16(:,128), A(:,356), n3(:,671), t3x256(:,:,356), nhel, den(603))
    call cont_SS(nsync, wf16(:,48), wf16(:,128), A(:,357), n3(:,672), t3x256(:,:,357), nhel, den(604))
    call cont_VV(nsync, wf16(:,116), wf16(:,129), A(:,358), n3(:,673), t3x256(:,:,358), nhel, den(605))
    call cont_VV(nsync, wf16(:,118), wf16(:,130), A(:,359), n3(:,674), t3x256(:,:,359), nhel, den(606))
    call cont_VV(nsync, wf16(:,117), wf16(:,129), A(:,360), n3(:,675), t3x256(:,:,360), nhel, den(607))
    call cont_VV(nsync, wf16(:,119), wf16(:,130), A(:,361), n3(:,676), t3x256(:,:,361), nhel, den(608))
    call cont_SS(nsync, wf16(:,49), wf16(:,128), A(:,362), n3(:,677), t3x256(:,:,362), nhel, den(609))
    call cont_SS(nsync, wf16(:,50), wf16(:,128), A(:,363), n3(:,678), t3x256(:,:,363), nhel, den(610))
    call cont_SS(nsync, wf16(:,51), wf16(:,128), A(:,364), n3(:,679), t3x256(:,:,364), nhel, den(611))
    call cont_VV(nsync, wf16(:,108), wf16(:,129), A(:,365), n3(:,680), t3x256(:,:,365), nhel, den(612))
    call cont_VV(nsync, wf16(:,109), wf16(:,130), A(:,366), n3(:,681), t3x256(:,:,366), nhel, den(613))
    call cont_VV(nsync, wf16(:,110), wf16(:,129), A(:,367), n3(:,682), t3x256(:,:,367), nhel, den(614))
    call cont_VV(nsync, wf16(:,111), wf16(:,129), A(:,368), n3(:,683), t3x256(:,:,368), nhel, den(615))
    call cont_VV(nsync, wf16(:,112), wf16(:,130), A(:,369), n3(:,684), t3x256(:,:,369), nhel, den(616))
    call cont_VV(nsync, wf16(:,113), wf16(:,130), A(:,370), n3(:,685), t3x256(:,:,370), nhel, den(617))
    call cont_VV(nsync, wf16(:,153), wf16(:,159), A(:,371), n3(:,686), t3x256(:,:,371), nhel, den(619))
    call cont_VV(nsync, wf16(:,150), wf16(:,160), A(:,372), n3(:,687), t3x256(:,:,372), nhel, den(621))
    call cont_VV(nsync, wf16(:,71), wf16(:,161), A(:,373), n3(:,688), t3x256(:,:,373), nhel, den(623))
    call cont_VV(nsync, wf16(:,74), wf16(:,161), A(:,374), n3(:,689), t3x256(:,:,374), nhel, den(624))
    call cont_VV(nsync, wf4(:,12), wf64(:,34), A(:,375), n3(:,690), t3x256(:,:,375), nhel, den(626))
    call cont_VV(nsync, wf4(:,14), wf64(:,35), A(:,376), n3(:,691), t3x256(:,:,376), nhel, den(627))
    call cont_VV(nsync, wf4(:,12), wf64(:,36), A(:,377), n3(:,692), t3x256(:,:,377), nhel, den(629))
    call cont_VV(nsync, wf4(:,14), wf64(:,37), A(:,378), n3(:,693), t3x256(:,:,378), nhel, den(630))
    call cont_VV(nsync, wf4(:,11), wf64(:,38), A(:,379), n3(:,694), t3x256(:,:,379), nhel, den(632))
    call cont_VV(nsync, wf4(:,16), wf64(:,39), A(:,380), n3(:,695), t3x256(:,:,380), nhel, den(633))
    call cont_VV(nsync, wf4(:,11), wf64(:,40), A(:,381), n3(:,696), t3x256(:,:,381), nhel, den(635))
    call cont_VV(nsync, wf4(:,16), wf64(:,41), A(:,382), n3(:,697), t3x256(:,:,382), nhel, den(636))
    call cont_VV(nsync, wf16(:,154), wf16(:,162), A(:,383), n3(:,698), t3x256(:,:,383), nhel, den(639))
    call cont_VV(nsync, wf16(:,156), wf16(:,162), A(:,384), n3(:,699), t3x256(:,:,384), nhel, den(640))
    call cont_VV(nsync, wf16(:,157), wf16(:,162), A(:,385), n3(:,700), t3x256(:,:,385), nhel, den(641))
    call cont_VV(nsync, wf16(:,149), wf16(:,163), A(:,386), n3(:,701), t3x256(:,:,386), nhel, den(644))
    call cont_VV(nsync, wf16(:,151), wf16(:,163), A(:,387), n3(:,702), t3x256(:,:,387), nhel, den(645))
    call cont_VV(nsync, wf16(:,152), wf16(:,163), A(:,388), n3(:,703), t3x256(:,:,388), nhel, den(646))
    call cont_VV(nsync, wf16(:,71), wf16(:,164), A(:,389), n3(:,704), t3x256(:,:,389), nhel, den(649))
    call cont_VV(nsync, wf16(:,74), wf16(:,164), A(:,390), n3(:,705), t3x256(:,:,390), nhel, den(650))
    call cont_VV(nsync, wf16(:,154), wf16(:,165), A(:,391), n3(:,706), t3x256(:,:,391), nhel, den(651))
    call cont_VV(nsync, wf16(:,154), wf16(:,166), A(:,392), n3(:,707), t3x256(:,:,392), nhel, den(652))
    call cont_VV(nsync, wf16(:,156), wf16(:,165), A(:,393), n3(:,708), t3x256(:,:,393), nhel, den(653))
    call cont_VV(nsync, wf16(:,157), wf16(:,165), A(:,394), n3(:,709), t3x256(:,:,394), nhel, den(654))
    call cont_VV(nsync, wf16(:,156), wf16(:,166), A(:,395), n3(:,710), t3x256(:,:,395), nhel, den(655))
    call cont_VV(nsync, wf16(:,157), wf16(:,166), A(:,396), n3(:,711), t3x256(:,:,396), nhel, den(656))
    call cont_VV(nsync, wf16(:,149), wf16(:,167), A(:,397), n3(:,712), t3x256(:,:,397), nhel, den(657))
    call cont_VV(nsync, wf16(:,149), wf16(:,168), A(:,398), n3(:,713), t3x256(:,:,398), nhel, den(658))
    call cont_VV(nsync, wf16(:,151), wf16(:,167), A(:,399), n3(:,714), t3x256(:,:,399), nhel, den(659))
    call cont_VV(nsync, wf16(:,152), wf16(:,167), A(:,400), n3(:,715), t3x256(:,:,400), nhel, den(660))
    call cont_VV(nsync, wf16(:,151), wf16(:,168), A(:,401), n3(:,716), t3x256(:,:,401), nhel, den(661))
    call cont_VV(nsync, wf16(:,152), wf16(:,168), A(:,402), n3(:,717), t3x256(:,:,402), nhel, den(662))
    call cont_VV(nsync, wf4(:,14), wf64(:,42), A(:,403), n3(:,718), t3x256(:,:,403), nhel, den(664))
    call cont_QA(nsync, wf8(:,56), wf32(:,51), A(:,404), n3(:,719), t3x256(:,:,404), nhel, den(666))
    call cont_QA(nsync, wf8(:,58), wf32(:,51), A(:,405), n3(:,720), t3x256(:,:,405), nhel, den(667))
    call cont_VV(nsync, wf4(:,16), wf64(:,43), A(:,406), n3(:,721), t3x256(:,:,406), nhel, den(669))
    call cont_QA(nsync, wf8(:,52), wf32(:,52), A(:,407), n3(:,722), t3x256(:,:,407), nhel, den(671))
    call cont_QA(nsync, wf8(:,54), wf32(:,52), A(:,408), n3(:,723), t3x256(:,:,408), nhel, den(672))
    call cont_VV(nsync, wf16(:,85), wf16(:,169), A(:,409), n3(:,724), t3x256(:,:,409), nhel, den(674))
    call cont_VV(nsync, wf16(:,86), wf16(:,170), A(:,410), n3(:,725), t3x256(:,:,410), nhel, den(676))
    call cont_VV(nsync, wf16(:,88), wf16(:,169), A(:,411), n3(:,726), t3x256(:,:,411), nhel, den(677))
    call cont_VV(nsync, wf16(:,89), wf16(:,169), A(:,412), n3(:,727), t3x256(:,:,412), nhel, den(678))
    call cont_VV(nsync, wf16(:,90), wf16(:,170), A(:,413), n3(:,728), t3x256(:,:,413), nhel, den(679))
    call cont_VV(nsync, wf16(:,91), wf16(:,170), A(:,414), n3(:,729), t3x256(:,:,414), nhel, den(680))
    call cont_VV(nsync, wf16(:,92), wf16(:,169), A(:,415), n3(:,730), t3x256(:,:,415), nhel, den(681))
    call cont_VV(nsync, wf16(:,93), wf16(:,170), A(:,416), n3(:,731), t3x256(:,:,416), nhel, den(682))
    call cont_VV(nsync, wf16(:,94), wf16(:,169), A(:,417), n3(:,732), t3x256(:,:,417), nhel, den(683))
    call cont_VV(nsync, wf16(:,95), wf16(:,169), A(:,418), n3(:,733), t3x256(:,:,418), nhel, den(684))
    call cont_VV(nsync, wf16(:,96), wf16(:,170), A(:,419), n3(:,734), t3x256(:,:,419), nhel, den(685))
    call cont_VV(nsync, wf16(:,97), wf16(:,170), A(:,420), n3(:,735), t3x256(:,:,420), nhel, den(686))
    call cont_VV(nsync, wf16(:,153), wf16(:,171), A(:,421), n3(:,736), t3x256(:,:,421), nhel, den(688))
    call cont_VV(nsync, wf16(:,86), wf16(:,172), A(:,422), n3(:,737), t3x256(:,:,422), nhel, den(690))
    call cont_VV(nsync, wf16(:,90), wf16(:,172), A(:,423), n3(:,738), t3x256(:,:,423), nhel, den(691))
    call cont_VV(nsync, wf16(:,91), wf16(:,172), A(:,424), n3(:,739), t3x256(:,:,424), nhel, den(692))
    call cont_VV(nsync, wf16(:,153), wf16(:,173), A(:,425), n3(:,740), t3x256(:,:,425), nhel, den(694))
    call cont_VV(nsync, wf16(:,153), wf16(:,174), A(:,426), n3(:,741), t3x256(:,:,426), nhel, den(696))
    call cont_VV(nsync, wf16(:,93), wf16(:,172), A(:,427), n3(:,742), t3x256(:,:,427), nhel, den(697))
    call cont_VV(nsync, wf16(:,96), wf16(:,172), A(:,428), n3(:,743), t3x256(:,:,428), nhel, den(698))
    call cont_VV(nsync, wf16(:,97), wf16(:,172), A(:,429), n3(:,744), t3x256(:,:,429), nhel, den(699))
    call cont_VV(nsync, wf16(:,108), wf16(:,169), A(:,430), n3(:,745), t3x256(:,:,430), nhel, den(700))
    call cont_VV(nsync, wf16(:,109), wf16(:,170), A(:,431), n3(:,746), t3x256(:,:,431), nhel, den(701))
    call cont_VV(nsync, wf16(:,110), wf16(:,169), A(:,432), n3(:,747), t3x256(:,:,432), nhel, den(702))
    call cont_VV(nsync, wf16(:,111), wf16(:,169), A(:,433), n3(:,748), t3x256(:,:,433), nhel, den(703))
    call cont_VV(nsync, wf16(:,112), wf16(:,170), A(:,434), n3(:,749), t3x256(:,:,434), nhel, den(704))
    call cont_VV(nsync, wf16(:,113), wf16(:,170), A(:,435), n3(:,750), t3x256(:,:,435), nhel, den(705))
    call cont_VV(nsync, wf16(:,114), wf16(:,169), A(:,436), n3(:,751), t3x256(:,:,436), nhel, den(706))
    call cont_VV(nsync, wf16(:,115), wf16(:,170), A(:,437), n3(:,752), t3x256(:,:,437), nhel, den(707))
    call cont_VV(nsync, wf16(:,116), wf16(:,169), A(:,438), n3(:,753), t3x256(:,:,438), nhel, den(708))
    call cont_VV(nsync, wf16(:,117), wf16(:,169), A(:,439), n3(:,754), t3x256(:,:,439), nhel, den(709))
    call cont_VV(nsync, wf16(:,118), wf16(:,170), A(:,440), n3(:,755), t3x256(:,:,440), nhel, den(710))
    call cont_VV(nsync, wf16(:,119), wf16(:,170), A(:,441), n3(:,756), t3x256(:,:,441), nhel, den(711))
    call cont_VV(nsync, wf16(:,109), wf16(:,172), A(:,442), n3(:,757), t3x256(:,:,442), nhel, den(712))
    call cont_VV(nsync, wf16(:,112), wf16(:,172), A(:,443), n3(:,758), t3x256(:,:,443), nhel, den(713))
    call cont_VV(nsync, wf16(:,113), wf16(:,172), A(:,444), n3(:,759), t3x256(:,:,444), nhel, den(714))
    call cont_VV(nsync, wf16(:,158), wf16(:,175), A(:,445), n3(:,760), t3x256(:,:,445), nhel, den(716))
    call cont_VV(nsync, wf16(:,158), wf16(:,176), A(:,446), n3(:,761), t3x256(:,:,446), nhel, den(718))
    call cont_VV(nsync, wf16(:,115), wf16(:,172), A(:,447), n3(:,762), t3x256(:,:,447), nhel, den(719))
    call cont_VV(nsync, wf16(:,118), wf16(:,172), A(:,448), n3(:,763), t3x256(:,:,448), nhel, den(720))
    call cont_VV(nsync, wf16(:,119), wf16(:,172), A(:,449), n3(:,764), t3x256(:,:,449), nhel, den(721))
    call cont_VV(nsync, wf16(:,158), wf16(:,177), A(:,450), n3(:,765), t3x256(:,:,450), nhel, den(723))
    call cont_VV(nsync, wf16(:,71), wf16(:,178), A(:,451), n3(:,766), t3x256(:,:,451), nhel, den(725))
    call cont_VV(nsync, wf16(:,74), wf16(:,178), A(:,452), n3(:,767), t3x256(:,:,452), nhel, den(726))
    call cont_VV(nsync, wf4(:,12), wf64(:,44), A(:,453), n3(:,768), t3x256(:,:,453), nhel, den(728))
    call cont_VV(nsync, wf4(:,14), wf64(:,45), A(:,454), n3(:,769), t3x256(:,:,454), nhel, den(729))
    call cont_VV(nsync, wf4(:,12), wf64(:,46), A(:,455), n3(:,770), t3x256(:,:,455), nhel, den(731))
    call cont_VV(nsync, wf4(:,14), wf64(:,47), A(:,456), n3(:,771), t3x256(:,:,456), nhel, den(732))
    call cont_VV(nsync, wf4(:,11), wf64(:,48), A(:,457), n3(:,772), t3x256(:,:,457), nhel, den(734))
    call cont_VV(nsync, wf4(:,16), wf64(:,49), A(:,458), n3(:,773), t3x256(:,:,458), nhel, den(735))
    call cont_VV(nsync, wf4(:,11), wf64(:,50), A(:,459), n3(:,774), t3x256(:,:,459), nhel, den(737))
    call cont_VV(nsync, wf4(:,16), wf64(:,51), A(:,460), n3(:,775), t3x256(:,:,460), nhel, den(738))
    call cont_VV(nsync, wf16(:,71), wf16(:,179), A(:,461), n3(:,776), t3x256(:,:,461), nhel, den(741))
    call cont_VV(nsync, wf16(:,74), wf16(:,179), A(:,462), n3(:,777), t3x256(:,:,462), nhel, den(742))
    call cont_VV(nsync, wf4(:,14), wf64(:,52), A(:,463), n3(:,778), t3x256(:,:,463), nhel, den(746))
    call cont_VV(nsync, wf4(:,16), wf64(:,53), A(:,464), n3(:,779), t3x256(:,:,464), nhel, den(750))
    call cont_VV(nsync, wf16(:,142), wf16(:,180), A(:,465), n3(:,780), t3x256(:,:,465), nhel, den(751))
    call cont_VV(nsync, wf16(:,144), wf16(:,180), A(:,466), n3(:,781), t3x256(:,:,466), nhel, den(752))
    call cont_VV(nsync, wf16(:,146), wf16(:,180), A(:,467), n3(:,782), t3x256(:,:,467), nhel, den(753))
    call cont_VV(nsync, wf16(:,133), wf16(:,181), A(:,468), n3(:,783), t3x256(:,:,468), nhel, den(754))
    call cont_VV(nsync, wf16(:,137), wf16(:,181), A(:,469), n3(:,784), t3x256(:,:,469), nhel, den(755))
    call cont_VV(nsync, wf16(:,139), wf16(:,181), A(:,470), n3(:,785), t3x256(:,:,470), nhel, den(756))
    call cont_VV(nsync, wf16(:,142), wf16(:,182), A(:,471), n3(:,786), t3x256(:,:,471), nhel, den(757))
    call cont_VV(nsync, wf16(:,142), wf16(:,183), A(:,472), n3(:,787), t3x256(:,:,472), nhel, den(758))
    call cont_VV(nsync, wf16(:,144), wf16(:,182), A(:,473), n3(:,788), t3x256(:,:,473), nhel, den(759))
    call cont_VV(nsync, wf16(:,146), wf16(:,182), A(:,474), n3(:,789), t3x256(:,:,474), nhel, den(760))
    call cont_VV(nsync, wf16(:,144), wf16(:,183), A(:,475), n3(:,790), t3x256(:,:,475), nhel, den(761))
    call cont_VV(nsync, wf16(:,146), wf16(:,183), A(:,476), n3(:,791), t3x256(:,:,476), nhel, den(762))
    call cont_VV(nsync, wf16(:,133), wf16(:,184), A(:,477), n3(:,792), t3x256(:,:,477), nhel, den(763))
    call cont_VV(nsync, wf16(:,133), wf16(:,185), A(:,478), n3(:,793), t3x256(:,:,478), nhel, den(764))
    call cont_VV(nsync, wf16(:,137), wf16(:,184), A(:,479), n3(:,794), t3x256(:,:,479), nhel, den(765))
    call cont_VV(nsync, wf16(:,139), wf16(:,184), A(:,480), n3(:,795), t3x256(:,:,480), nhel, den(766))
    call cont_VV(nsync, wf16(:,137), wf16(:,185), A(:,481), n3(:,796), t3x256(:,:,481), nhel, den(767))
    call cont_VV(nsync, wf16(:,139), wf16(:,185), A(:,482), n3(:,797), t3x256(:,:,482), nhel, den(768))
    call cont_QA(nsync, wf8(:,66), wf32(:,53), A(:,483), n3(:,798), t3x256(:,:,483), nhel, den(770))
    call cont_QA(nsync, wf8(:,68), wf32(:,53), A(:,484), n3(:,799), t3x256(:,:,484), nhel, den(771))
    call cont_QA(nsync, wf8(:,62), wf32(:,54), A(:,485), n3(:,800), t3x256(:,:,485), nhel, den(773))
    call cont_QA(nsync, wf8(:,64), wf32(:,54), A(:,486), n3(:,801), t3x256(:,:,486), nhel, den(774))
    call cont_VV(nsync, wf16(:,85), wf16(:,186), A(:,487), n3(:,802), t3x256(:,:,487), nhel, den(776))
    call cont_VV(nsync, wf16(:,86), wf16(:,187), A(:,488), n3(:,803), t3x256(:,:,488), nhel, den(778))
    call cont_VV(nsync, wf16(:,88), wf16(:,186), A(:,489), n3(:,804), t3x256(:,:,489), nhel, den(779))
    call cont_VV(nsync, wf16(:,89), wf16(:,186), A(:,490), n3(:,805), t3x256(:,:,490), nhel, den(780))
    call cont_VV(nsync, wf16(:,90), wf16(:,187), A(:,491), n3(:,806), t3x256(:,:,491), nhel, den(781))
    call cont_VV(nsync, wf16(:,91), wf16(:,187), A(:,492), n3(:,807), t3x256(:,:,492), nhel, den(782))
    call cont_VV(nsync, wf16(:,92), wf16(:,186), A(:,493), n3(:,808), t3x256(:,:,493), nhel, den(783))
    call cont_VV(nsync, wf16(:,93), wf16(:,187), A(:,494), n3(:,809), t3x256(:,:,494), nhel, den(784))
    call cont_VV(nsync, wf16(:,94), wf16(:,186), A(:,495), n3(:,810), t3x256(:,:,495), nhel, den(785))
    call cont_VV(nsync, wf16(:,95), wf16(:,186), A(:,496), n3(:,811), t3x256(:,:,496), nhel, den(786))
    call cont_VV(nsync, wf16(:,96), wf16(:,187), A(:,497), n3(:,812), t3x256(:,:,497), nhel, den(787))
    call cont_VV(nsync, wf16(:,97), wf16(:,187), A(:,498), n3(:,813), t3x256(:,:,498), nhel, den(788))
    call cont_VV(nsync, wf16(:,86), wf16(:,188), A(:,499), n3(:,814), t3x256(:,:,499), nhel, den(790))
    call cont_VV(nsync, wf16(:,90), wf16(:,188), A(:,500), n3(:,815), t3x256(:,:,500), nhel, den(791))
    call cont_VV(nsync, wf16(:,91), wf16(:,188), A(:,501), n3(:,816), t3x256(:,:,501), nhel, den(792))
    call cont_VV(nsync, wf16(:,93), wf16(:,188), A(:,502), n3(:,817), t3x256(:,:,502), nhel, den(793))
    call cont_VV(nsync, wf16(:,96), wf16(:,188), A(:,503), n3(:,818), t3x256(:,:,503), nhel, den(794))
    call cont_VV(nsync, wf16(:,97), wf16(:,188), A(:,504), n3(:,819), t3x256(:,:,504), nhel, den(795))
    call cont_VV(nsync, wf16(:,150), wf16(:,189), A(:,505), n3(:,820), t3x256(:,:,505), nhel, den(797))
    call cont_VV(nsync, wf16(:,150), wf16(:,190), A(:,506), n3(:,821), t3x256(:,:,506), nhel, den(799))
    call cont_VV(nsync, wf16(:,150), wf16(:,191), A(:,507), n3(:,822), t3x256(:,:,507), nhel, den(801))
    call cont_VV(nsync, wf16(:,108), wf16(:,186), A(:,508), n3(:,823), t3x256(:,:,508), nhel, den(802))
    call cont_VV(nsync, wf16(:,109), wf16(:,187), A(:,509), n3(:,824), t3x256(:,:,509), nhel, den(803))
    call cont_VV(nsync, wf16(:,110), wf16(:,186), A(:,510), n3(:,825), t3x256(:,:,510), nhel, den(804))
    call cont_VV(nsync, wf16(:,111), wf16(:,186), A(:,511), n3(:,826), t3x256(:,:,511), nhel, den(805))
    call cont_VV(nsync, wf16(:,112), wf16(:,187), A(:,512), n3(:,827), t3x256(:,:,512), nhel, den(806))
    call cont_VV(nsync, wf16(:,113), wf16(:,187), A(:,513), n3(:,828), t3x256(:,:,513), nhel, den(807))
    call cont_VV(nsync, wf16(:,114), wf16(:,186), A(:,514), n3(:,829), t3x256(:,:,514), nhel, den(808))
    call cont_VV(nsync, wf16(:,115), wf16(:,187), A(:,515), n3(:,830), t3x256(:,:,515), nhel, den(809))
    call cont_VV(nsync, wf16(:,116), wf16(:,186), A(:,516), n3(:,831), t3x256(:,:,516), nhel, den(810))
    call cont_VV(nsync, wf16(:,117), wf16(:,186), A(:,517), n3(:,832), t3x256(:,:,517), nhel, den(811))
    call cont_VV(nsync, wf16(:,118), wf16(:,187), A(:,518), n3(:,833), t3x256(:,:,518), nhel, den(812))
    call cont_VV(nsync, wf16(:,119), wf16(:,187), A(:,519), n3(:,834), t3x256(:,:,519), nhel, den(813))
    call cont_VV(nsync, wf16(:,109), wf16(:,188), A(:,520), n3(:,835), t3x256(:,:,520), nhel, den(814))
    call cont_VV(nsync, wf16(:,112), wf16(:,188), A(:,521), n3(:,836), t3x256(:,:,521), nhel, den(815))
    call cont_VV(nsync, wf16(:,113), wf16(:,188), A(:,522), n3(:,837), t3x256(:,:,522), nhel, den(816))
    call cont_VV(nsync, wf16(:,115), wf16(:,188), A(:,523), n3(:,838), t3x256(:,:,523), nhel, den(817))
    call cont_VV(nsync, wf16(:,118), wf16(:,188), A(:,524), n3(:,839), t3x256(:,:,524), nhel, den(818))
    call cont_VV(nsync, wf16(:,119), wf16(:,188), A(:,525), n3(:,840), t3x256(:,:,525), nhel, den(819))
    call cont_VV(nsync, wf16(:,155), wf16(:,192), A(:,526), n3(:,841), t3x256(:,:,526), nhel, den(821))
    call cont_VV(nsync, wf16(:,155), wf16(:,193), A(:,527), n3(:,842), t3x256(:,:,527), nhel, den(823))
    call cont_VV(nsync, wf16(:,155), wf16(:,194), A(:,528), n3(:,843), t3x256(:,:,528), nhel, den(825))
    call cont_VV(nsync, wf16(:,71), wf16(:,195), A(:,529), n3(:,844), t3x256(:,:,529), nhel, den(827))
    call cont_VV(nsync, wf16(:,74), wf16(:,195), A(:,530), n3(:,845), t3x256(:,:,530), nhel, den(828))
    call cont_VV(nsync, wf16(:,34), wf16(:,196), A(:,531), n3(:,846), t3x256(:,:,531), nhel, den(829))
    call cont_VV(nsync, wf16(:,14), wf16(:,197), A(:,532), n3(:,847), t3x256(:,:,532), nhel, den(830))
    call cont_VV(nsync, wf16(:,71), wf16(:,198), A(:,533), n3(:,848), t3x256(:,:,533), nhel, den(832))
    call cont_VV(nsync, wf16(:,74), wf16(:,198), A(:,534), n3(:,849), t3x256(:,:,534), nhel, den(833))
    call cont_VV(nsync, wf16(:,34), wf16(:,199), A(:,535), n3(:,850), t3x256(:,:,535), nhel, den(834))
    call cont_VV(nsync, wf16(:,14), wf16(:,200), A(:,536), n3(:,851), t3x256(:,:,536), nhel, den(835))
    call cont_VV(nsync, wf4(:,14), wf64(:,54), A(:,537), n3(:,852), t3x256(:,:,537), nhel, den(837))
    call cont_VV(nsync, wf4(:,16), wf64(:,55), A(:,538), n3(:,853), t3x256(:,:,538), nhel, den(839))
    call cont_VV(nsync, wf4(:,14), wf64(:,56), A(:,539), n3(:,854), t3x256(:,:,539), nhel, den(841))
    call cont_VV(nsync, wf4(:,6), wf64(:,57), A(:,540), n3(:,855), t3x256(:,:,540), nhel, den(843))
    call cont_VV(nsync, wf4(:,16), wf64(:,58), A(:,541), n3(:,856), t3x256(:,:,541), nhel, den(845))
    call cont_VV(nsync, wf4(:,6), wf64(:,59), A(:,542), n3(:,857), t3x256(:,:,542), nhel, den(847))
    call cont_VV(nsync, wf16(:,86), wf16(:,201), A(:,543), n3(:,858), t3x256(:,:,543), nhel, den(849))
    call cont_VV(nsync, wf16(:,90), wf16(:,201), A(:,544), n3(:,859), t3x256(:,:,544), nhel, den(850))
    call cont_VV(nsync, wf16(:,91), wf16(:,201), A(:,545), n3(:,860), t3x256(:,:,545), nhel, den(851))
    call cont_VV(nsync, wf16(:,93), wf16(:,201), A(:,546), n3(:,861), t3x256(:,:,546), nhel, den(852))
    call cont_VV(nsync, wf16(:,96), wf16(:,201), A(:,547), n3(:,862), t3x256(:,:,547), nhel, den(853))
    call cont_VV(nsync, wf16(:,97), wf16(:,201), A(:,548), n3(:,863), t3x256(:,:,548), nhel, den(854))
    call cont_VV(nsync, wf16(:,23), wf16(:,202), A(:,549), n3(:,864), t3x256(:,:,549), nhel, den(856))
    call cont_VV(nsync, wf16(:,24), wf16(:,202), A(:,550), n3(:,865), t3x256(:,:,550), nhel, den(857))
    call cont_VV(nsync, wf16(:,19), wf16(:,202), A(:,551), n3(:,866), t3x256(:,:,551), nhel, den(858))
    call cont_VV(nsync, wf16(:,20), wf16(:,202), A(:,552), n3(:,867), t3x256(:,:,552), nhel, den(859))
    call cont_VV(nsync, wf16(:,86), wf16(:,203), A(:,553), n3(:,868), t3x256(:,:,553), nhel, den(861))
    call cont_VV(nsync, wf16(:,90), wf16(:,203), A(:,554), n3(:,869), t3x256(:,:,554), nhel, den(862))
    call cont_VV(nsync, wf16(:,91), wf16(:,203), A(:,555), n3(:,870), t3x256(:,:,555), nhel, den(863))
    call cont_VV(nsync, wf16(:,23), wf16(:,204), A(:,556), n3(:,871), t3x256(:,:,556), nhel, den(865))
    call cont_VV(nsync, wf16(:,24), wf16(:,204), A(:,557), n3(:,872), t3x256(:,:,557), nhel, den(866))
    call cont_VV(nsync, wf16(:,93), wf16(:,203), A(:,558), n3(:,873), t3x256(:,:,558), nhel, den(867))
    call cont_VV(nsync, wf16(:,96), wf16(:,203), A(:,559), n3(:,874), t3x256(:,:,559), nhel, den(868))
    call cont_VV(nsync, wf16(:,97), wf16(:,203), A(:,560), n3(:,875), t3x256(:,:,560), nhel, den(869))
    call cont_VV(nsync, wf16(:,19), wf16(:,204), A(:,561), n3(:,876), t3x256(:,:,561), nhel, den(870))
    call cont_VV(nsync, wf16(:,20), wf16(:,204), A(:,562), n3(:,877), t3x256(:,:,562), nhel, den(871))
    call cont_VV(nsync, wf16(:,109), wf16(:,201), A(:,563), n3(:,878), t3x256(:,:,563), nhel, den(872))
    call cont_VV(nsync, wf16(:,112), wf16(:,201), A(:,564), n3(:,879), t3x256(:,:,564), nhel, den(873))
    call cont_VV(nsync, wf16(:,113), wf16(:,201), A(:,565), n3(:,880), t3x256(:,:,565), nhel, den(874))
    call cont_VV(nsync, wf16(:,115), wf16(:,201), A(:,566), n3(:,881), t3x256(:,:,566), nhel, den(875))
    call cont_VV(nsync, wf16(:,118), wf16(:,201), A(:,567), n3(:,882), t3x256(:,:,567), nhel, den(876))
    call cont_VV(nsync, wf16(:,119), wf16(:,201), A(:,568), n3(:,883), t3x256(:,:,568), nhel, den(877))
    call cont_VV(nsync, wf16(:,109), wf16(:,203), A(:,569), n3(:,884), t3x256(:,:,569), nhel, den(878))
    call cont_VV(nsync, wf16(:,112), wf16(:,203), A(:,570), n3(:,885), t3x256(:,:,570), nhel, den(879))
    call cont_VV(nsync, wf16(:,113), wf16(:,203), A(:,571), n3(:,886), t3x256(:,:,571), nhel, den(880))
    call cont_VV(nsync, wf16(:,44), wf16(:,205), A(:,572), n3(:,887), t3x256(:,:,572), nhel, den(882))
    call cont_VV(nsync, wf16(:,45), wf16(:,205), A(:,573), n3(:,888), t3x256(:,:,573), nhel, den(883))
    call cont_VV(nsync, wf16(:,115), wf16(:,203), A(:,574), n3(:,889), t3x256(:,:,574), nhel, den(884))
    call cont_VV(nsync, wf16(:,118), wf16(:,203), A(:,575), n3(:,890), t3x256(:,:,575), nhel, den(885))
    call cont_VV(nsync, wf16(:,119), wf16(:,203), A(:,576), n3(:,891), t3x256(:,:,576), nhel, den(886))
    call cont_VV(nsync, wf16(:,40), wf16(:,205), A(:,577), n3(:,892), t3x256(:,:,577), nhel, den(887))
    call cont_VV(nsync, wf16(:,41), wf16(:,205), A(:,578), n3(:,893), t3x256(:,:,578), nhel, den(888))
    call cont_VV(nsync, wf16(:,44), wf16(:,206), A(:,579), n3(:,894), t3x256(:,:,579), nhel, den(890))
    call cont_VV(nsync, wf16(:,45), wf16(:,206), A(:,580), n3(:,895), t3x256(:,:,580), nhel, den(891))
    call cont_VV(nsync, wf16(:,40), wf16(:,206), A(:,581), n3(:,896), t3x256(:,:,581), nhel, den(892))
    call cont_VV(nsync, wf16(:,41), wf16(:,206), A(:,582), n3(:,897), t3x256(:,:,582), nhel, den(893))
    call cont_VV(nsync, wf16(:,82), wf16(:,204), A(:,583), n3(:,898), t3x256(:,:,583), nhel, den(894))
    call cont_VV(nsync, wf16(:,83), wf16(:,204), A(:,584), n3(:,899), t3x256(:,:,584), nhel, den(895))
    call cont_VV(nsync, wf16(:,80), wf16(:,205), A(:,585), n3(:,900), t3x256(:,:,585), nhel, den(896))
    call cont_VV(nsync, wf16(:,81), wf16(:,205), A(:,586), n3(:,901), t3x256(:,:,586), nhel, den(897))
    call cont_VV(nsync, wf16(:,175), wf16(:,182), A(:,587), n3(:,902), t3x256(:,:,587), nhel, den(898))
    call cont_VV(nsync, wf16(:,176), wf16(:,182), A(:,588), n3(:,903), t3x256(:,:,588), nhel, den(899))
    call cont_VV(nsync, wf16(:,175), wf16(:,183), A(:,589), n3(:,904), t3x256(:,:,589), nhel, den(900))
    call cont_VV(nsync, wf16(:,176), wf16(:,183), A(:,590), n3(:,905), t3x256(:,:,590), nhel, den(901))
    call cont_VV(nsync, wf16(:,173), wf16(:,184), A(:,591), n3(:,906), t3x256(:,:,591), nhel, den(902))
    call cont_VV(nsync, wf16(:,174), wf16(:,184), A(:,592), n3(:,907), t3x256(:,:,592), nhel, den(903))
    call cont_VV(nsync, wf16(:,173), wf16(:,185), A(:,593), n3(:,908), t3x256(:,:,593), nhel, den(904))
    call cont_VV(nsync, wf16(:,174), wf16(:,185), A(:,594), n3(:,909), t3x256(:,:,594), nhel, den(905))
    call cont_VV(nsync, wf16(:,78), wf16(:,204), A(:,595), n3(:,910), t3x256(:,:,595), nhel, den(906))
    call cont_VV(nsync, wf16(:,79), wf16(:,204), A(:,596), n3(:,911), t3x256(:,:,596), nhel, den(907))
    call cont_VV(nsync, wf16(:,76), wf16(:,205), A(:,597), n3(:,912), t3x256(:,:,597), nhel, den(908))
    call cont_VV(nsync, wf16(:,77), wf16(:,205), A(:,598), n3(:,913), t3x256(:,:,598), nhel, den(909))
    call cont_VV(nsync, wf16(:,82), wf16(:,202), A(:,599), n3(:,914), t3x256(:,:,599), nhel, den(910))
    call cont_VV(nsync, wf16(:,83), wf16(:,202), A(:,600), n3(:,915), t3x256(:,:,600), nhel, den(911))
    call cont_VV(nsync, wf16(:,78), wf16(:,202), A(:,601), n3(:,916), t3x256(:,:,601), nhel, den(912))
    call cont_VV(nsync, wf16(:,79), wf16(:,202), A(:,602), n3(:,917), t3x256(:,:,602), nhel, den(913))
    call cont_VV(nsync, wf16(:,80), wf16(:,206), A(:,603), n3(:,918), t3x256(:,:,603), nhel, den(914))
    call cont_VV(nsync, wf16(:,81), wf16(:,206), A(:,604), n3(:,919), t3x256(:,:,604), nhel, den(915))
    call cont_VV(nsync, wf16(:,76), wf16(:,206), A(:,605), n3(:,920), t3x256(:,:,605), nhel, den(916))
    call cont_VV(nsync, wf16(:,77), wf16(:,206), A(:,606), n3(:,921), t3x256(:,:,606), nhel, den(917))
    call cont_VV(nsync, wf16(:,175), wf16(:,180), A(:,607), n3(:,922), t3x256(:,:,607), nhel, den(918))
    call cont_VV(nsync, wf16(:,176), wf16(:,180), A(:,608), n3(:,923), t3x256(:,:,608), nhel, den(919))
    call cont_VV(nsync, wf16(:,173), wf16(:,181), A(:,609), n3(:,924), t3x256(:,:,609), nhel, den(920))
    call cont_VV(nsync, wf16(:,174), wf16(:,181), A(:,610), n3(:,925), t3x256(:,:,610), nhel, den(921))
    call cont_VV(nsync, wf16(:,162), wf16(:,192), A(:,611), n3(:,926), t3x256(:,:,611), nhel, den(922))
    call cont_VV(nsync, wf16(:,162), wf16(:,193), A(:,612), n3(:,927), t3x256(:,:,612), nhel, den(923))
    call cont_VV(nsync, wf16(:,162), wf16(:,194), A(:,613), n3(:,928), t3x256(:,:,613), nhel, den(924))
    call cont_VV(nsync, wf16(:,163), wf16(:,190), A(:,614), n3(:,929), t3x256(:,:,614), nhel, den(925))
    call cont_VV(nsync, wf16(:,163), wf16(:,191), A(:,615), n3(:,930), t3x256(:,:,615), nhel, den(926))
    call cont_VV(nsync, wf16(:,163), wf16(:,189), A(:,616), n3(:,931), t3x256(:,:,616), nhel, den(927))

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
  complex(REALKIND), intent(out) :: M1(1) ! M1helarray(1,256)
  integer :: empty(0)

  M1(1) = (A(j,15)%j+A(j,21)%j+A(j,29)%j+A(j,35)%j+A(j,43)%j+A(j,53)%j+A(j,123)%j+A(j,129)%j+A(j,135)%j+A(j,141)%j+A(j,207)%j &
       +A(j,213)%j+A(j,219)%j+A(j,225)%j)*f(1)+(A(j,16)%j+A(j,17)%j+A(j,22)%j+A(j,23)%j+A(j,30)%j+A(j,31)%j+A(j,36)%j+A(j,37)%j &
       +A(j,44)%j+A(j,45)%j+A(j,54)%j+A(j,55)%j+A(j,69)%j+A(j,71)%j+A(j,77)%j+A(j,79)%j+A(j,83)%j+A(j,85)%j+A(j,91)%j+A(j,93)%j &
       +A(j,101)%j+A(j,103)%j+A(j,111)%j+A(j,113)%j+A(j,124)%j+A(j,125)%j+A(j,130)%j+A(j,131)%j+A(j,136)%j+A(j,137)%j+A(j,142)%j &
       +A(j,143)%j+A(j,147)%j+A(j,149)%j+A(j,153)%j+A(j,155)%j+A(j,159)%j+A(j,161)%j+A(j,165)%j+A(j,167)%j+A(j,171)%j+A(j,173)%j &
       +A(j,175)%j+A(j,176)%j+A(j,181)%j+A(j,183)%j+A(j,185)%j+A(j,186)%j+A(j,197)%j+A(j,199)%j+A(j,201)%j+A(j,202)%j+A(j,208)%j &
       +A(j,209)%j+A(j,214)%j+A(j,215)%j+A(j,220)%j+A(j,221)%j+A(j,226)%j+A(j,227)%j+A(j,229)%j+A(j,231)%j+A(j,233)%j+A(j,234)%j &
       +A(j,237)%j+A(j,239)%j+A(j,241)%j+A(j,242)%j+A(j,247)%j+A(j,249)%j+A(j,253)%j+A(j,255)%j+A(j,259)%j+A(j,261)%j+A(j,265)%j &
       +A(j,267)%j+A(j,269)%j+A(j,271)%j+A(j,273)%j+A(j,274)%j+A(j,277)%j+A(j,279)%j+A(j,281)%j+A(j,282)%j+A(j,544)%j+A(j,547)%j &
       +A(j,549)%j+A(j,551)%j+A(j,554)%j+A(j,556)%j+A(j,559)%j+A(j,561)%j+A(j,564)%j+A(j,567)%j+A(j,570)%j+A(j,575)%j+A(j,583)%j &
       +A(j,595)%j+A(j,599)%j+A(j,601)%j)*f(2)+(A(j,18)%j+A(j,24)%j+A(j,32)%j+A(j,38)%j+A(j,46)%j+A(j,56)%j+A(j,70)%j+A(j,72)%j &
       +A(j,78)%j+A(j,80)%j+A(j,84)%j+A(j,86)%j+A(j,92)%j+A(j,94)%j+A(j,102)%j+A(j,104)%j+A(j,112)%j+A(j,114)%j+A(j,126)%j &
       +A(j,132)%j+A(j,138)%j+A(j,144)%j+A(j,148)%j+A(j,150)%j+A(j,154)%j+A(j,156)%j+A(j,160)%j+A(j,162)%j+A(j,166)%j+A(j,168)%j &
       +A(j,172)%j+A(j,174)%j+A(j,177)%j+A(j,178)%j+A(j,182)%j+A(j,184)%j+A(j,187)%j+A(j,188)%j+A(j,198)%j+A(j,200)%j+A(j,203)%j &
       +A(j,204)%j+A(j,210)%j+A(j,216)%j+A(j,222)%j+A(j,228)%j+A(j,230)%j+A(j,232)%j+A(j,235)%j+A(j,236)%j+A(j,238)%j+A(j,240)%j &
       +A(j,243)%j+A(j,244)%j+A(j,248)%j+A(j,250)%j+A(j,254)%j+A(j,256)%j+A(j,260)%j+A(j,262)%j+A(j,266)%j+A(j,268)%j+A(j,270)%j &
       +A(j,272)%j+A(j,275)%j+A(j,276)%j+A(j,278)%j+A(j,280)%j+A(j,283)%j+A(j,284)%j+A(j,537)%j+A(j,538)%j+A(j,539)%j+A(j,540)%j &
       +A(j,541)%j+A(j,542)%j+A(j,545)%j+A(j,548)%j+A(j,550)%j+A(j,552)%j+A(j,555)%j+A(j,557)%j+A(j,560)%j+A(j,562)%j+A(j,565)%j &
       +A(j,568)%j+A(j,571)%j+A(j,572)%j+A(j,573)%j+A(j,576)%j+A(j,577)%j+A(j,578)%j+A(j,579)%j+A(j,580)%j+A(j,581)%j+A(j,582)%j &
       +A(j,584)%j+A(j,585)%j+A(j,586)%j+A(j,596)%j+A(j,597)%j+A(j,598)%j+A(j,600)%j+A(j,602)%j+A(j,603)%j+A(j,604)%j+A(j,605)%j &
       +A(j,606)%j)*f(3)+(-A(j,371)%j-A(j,372)%j)*f(4)+(A(j,341)%j+A(j,347)%j)*f(5)+(A(j,313)%j+A(j,319)%j)*f(6)+(-A(j,289)%j &
       -A(j,298)%j)*f(7)+(-A(j,2)%j+A(j,310)%j+A(j,316)%j+A(j,344)%j+A(j,350)%j)*f(8)+(-A(j,322)%j-A(j,331)%j)*f(9) &
       -A(j,285)%j*f(10)+(-A(j,353)%j-A(j,362)%j)*f(11)+A(j,307)%j*f(12)+(-A(j,315)%j-A(j,321)%j-A(j,343)%j-A(j,349)%j)*f(13) &
       +A(j,287)%j*f(14)+(A(j,293)%j+A(j,302)%j)*f(15)+A(j,286)%j*f(16)+(A(j,291)%j+A(j,300)%j)*f(17)+(A(j,324)%j+A(j,333)%j &
       +A(j,355)%j+A(j,366)%j)*f(18)+(-A(j,311)%j-A(j,317)%j+A(j,342)%j-A(j,345)%j+A(j,348)%j-A(j,351)%j-A(j,445)%j &
       -A(j,526)%j)*f(19)+A(j,308)%j*f(20)+(-A(j,309)%j-A(j,312)%j+A(j,314)%j-A(j,318)%j+A(j,320)%j-A(j,340)%j-A(j,346)%j &
       -A(j,352)%j-A(j,421)%j-A(j,425)%j-A(j,426)%j-A(j,446)%j-A(j,450)%j-A(j,505)%j-A(j,506)%j-A(j,507)%j-A(j,527)%j &
       -A(j,528)%j)*f(21)+(A(j,6)%j-A(j,297)%j-A(j,306)%j)*f(22)+(A(j,325)%j+A(j,334)%j)*f(23)+(-A(j,292)%j-A(j,301)%j+A(j,326)%j &
       +A(j,335)%j+A(j,383)%j+A(j,391)%j+A(j,392)%j+A(j,465)%j+A(j,471)%j+A(j,472)%j)*f(24)+(A(j,374)%j+A(j,390)%j+A(j,452)%j &
       +A(j,462)%j)*f(25)+(-A(j,290)%j-A(j,299)%j+A(j,356)%j+A(j,363)%j+A(j,397)%j+A(j,477)%j)*f(26)+(A(j,357)%j+A(j,364)%j &
       +A(j,386)%j+A(j,398)%j+A(j,468)%j+A(j,478)%j)*f(27)+(A(j,373)%j+A(j,389)%j+A(j,451)%j+A(j,461)%j)*f(28)+(A(j,9)%j &
       +A(j,11)%j)*f(29)+(-A(j,323)%j-A(j,332)%j+A(j,409)%j+A(j,415)%j+A(j,487)%j+A(j,493)%j)*f(30)+(-A(j,354)%j-A(j,365)%j &
       +A(j,410)%j+A(j,416)%j+A(j,422)%j+A(j,427)%j+A(j,430)%j+A(j,431)%j+A(j,436)%j+A(j,437)%j+A(j,442)%j+A(j,447)%j+A(j,488)%j &
       +A(j,494)%j+A(j,499)%j+A(j,502)%j+A(j,508)%j+A(j,509)%j+A(j,514)%j+A(j,515)%j+A(j,520)%j+A(j,523)%j)*f(31)+A(j,1)%j*f(32) &
       +(A(j,61)%j+A(j,64)%j)*f(33)+A(j,7)%j*f(34)+(A(j,115)%j+A(j,118)%j)*f(35)+A(j,288)%j*f(36)+(-A(j,5)%j+A(j,296)%j+A(j,305)%j &
       -A(j,328)%j-A(j,337)%j-A(j,359)%j-A(j,369)%j-A(j,400)%j-A(j,480)%j)*f(37)+(-A(j,4)%j+A(j,295)%j+A(j,304)%j-A(j,330)%j &
       -A(j,339)%j-A(j,361)%j-A(j,370)%j-A(j,385)%j-A(j,388)%j-A(j,394)%j-A(j,396)%j-A(j,402)%j-A(j,467)%j-A(j,470)%j-A(j,474)%j &
       -A(j,476)%j-A(j,482)%j)*f(38)+(A(j,327)%j+A(j,336)%j-A(j,411)%j-A(j,417)%j-A(j,489)%j-A(j,495)%j)*f(39)+(A(j,3)%j &
       -A(j,294)%j-A(j,303)%j+A(j,329)%j+A(j,338)%j+A(j,358)%j+A(j,367)%j-A(j,375)%j-A(j,377)%j-A(j,379)%j-A(j,380)%j+A(j,384)%j &
       +A(j,393)%j+A(j,395)%j+A(j,399)%j-A(j,404)%j-A(j,412)%j-A(j,413)%j-A(j,418)%j-A(j,419)%j-A(j,423)%j-A(j,428)%j-A(j,432)%j &
       -A(j,434)%j-A(j,438)%j-A(j,440)%j-A(j,443)%j-A(j,448)%j-A(j,453)%j-A(j,455)%j-A(j,457)%j-A(j,458)%j+A(j,466)%j+A(j,473)%j &
       +A(j,475)%j+A(j,479)%j-A(j,483)%j-A(j,490)%j-A(j,491)%j-A(j,496)%j-A(j,497)%j-A(j,500)%j-A(j,503)%j-A(j,510)%j-A(j,512)%j &
       -A(j,516)%j-A(j,518)%j-A(j,521)%j-A(j,524)%j-A(j,587)%j-A(j,589)%j-A(j,591)%j-A(j,592)%j-A(j,607)%j-A(j,611)%j)*f(40) &
       +(A(j,360)%j+A(j,368)%j-A(j,376)%j-A(j,378)%j-A(j,381)%j-A(j,382)%j+A(j,387)%j+A(j,401)%j-A(j,403)%j-A(j,405)%j-A(j,406)%j &
       -A(j,407)%j-A(j,408)%j-A(j,414)%j-A(j,420)%j-A(j,424)%j-A(j,429)%j-A(j,433)%j-A(j,435)%j-A(j,439)%j-A(j,441)%j-A(j,444)%j &
       -A(j,449)%j-A(j,454)%j-A(j,456)%j-A(j,459)%j-A(j,460)%j-A(j,463)%j-A(j,464)%j+A(j,469)%j+A(j,481)%j-A(j,484)%j-A(j,485)%j &
       -A(j,486)%j-A(j,492)%j-A(j,498)%j-A(j,501)%j-A(j,504)%j-A(j,511)%j-A(j,513)%j-A(j,517)%j-A(j,519)%j-A(j,522)%j-A(j,525)%j &
       -A(j,588)%j-A(j,590)%j-A(j,593)%j-A(j,594)%j-A(j,608)%j-A(j,609)%j-A(j,610)%j-A(j,612)%j-A(j,613)%j-A(j,614)%j-A(j,615)%j &
       -A(j,616)%j)*f(41)+(-A(j,49)%j-A(j,59)%j-A(j,62)%j-A(j,65)%j)*f(42)+(-A(j,26)%j-A(j,40)%j-A(j,50)%j-A(j,60)%j-A(j,63)%j &
       -A(j,66)%j-A(j,74)%j-A(j,88)%j-A(j,97)%j-A(j,98)%j-A(j,107)%j-A(j,108)%j-A(j,170)%j-A(j,180)%j-A(j,189)%j-A(j,190)%j &
       -A(j,193)%j-A(j,194)%j-A(j,530)%j-A(j,531)%j-A(j,534)%j-A(j,535)%j)*f(43)+(-A(j,47)%j-A(j,57)%j-A(j,116)%j-A(j,119)%j &
       -A(j,191)%j-A(j,195)%j)*f(44)+(-A(j,25)%j-A(j,39)%j-A(j,48)%j-A(j,58)%j-A(j,73)%j-A(j,87)%j-A(j,95)%j-A(j,96)%j-A(j,105)%j &
       -A(j,106)%j-A(j,117)%j-A(j,120)%j-A(j,169)%j-A(j,179)%j-A(j,192)%j-A(j,196)%j-A(j,529)%j-A(j,532)%j-A(j,533)%j &
       -A(j,536)%j)*f(45)+(-A(j,13)%j-A(j,19)%j-A(j,27)%j-A(j,33)%j-A(j,41)%j-A(j,51)%j-A(j,121)%j-A(j,127)%j-A(j,133)%j &
       -A(j,139)%j-A(j,205)%j-A(j,211)%j-A(j,217)%j-A(j,223)%j)*f(46)+(-A(j,14)%j-A(j,20)%j-A(j,28)%j-A(j,34)%j-A(j,42)%j &
       -A(j,52)%j-A(j,67)%j-A(j,68)%j-A(j,75)%j-A(j,76)%j-A(j,81)%j-A(j,82)%j-A(j,89)%j-A(j,90)%j-A(j,99)%j-A(j,100)%j-A(j,109)%j &
       -A(j,110)%j-A(j,122)%j-A(j,128)%j-A(j,134)%j-A(j,140)%j-A(j,145)%j-A(j,146)%j-A(j,151)%j-A(j,152)%j-A(j,157)%j-A(j,158)%j &
       -A(j,163)%j-A(j,164)%j-A(j,206)%j-A(j,212)%j-A(j,218)%j-A(j,224)%j-A(j,245)%j-A(j,246)%j-A(j,251)%j-A(j,252)%j-A(j,257)%j &
       -A(j,258)%j-A(j,263)%j-A(j,264)%j-A(j,543)%j-A(j,546)%j-A(j,553)%j-A(j,558)%j-A(j,563)%j-A(j,566)%j-A(j,569)%j &
       -A(j,574)%j)*f(47)+(-A(j,8)%j-A(j,10)%j-A(j,12)%j)*f(48)

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
  use ol_colourmatrix_eellllbb_nenexeexmmxbbx_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(1)
  real(REALKIND),    intent(out) :: M2colint(0:38-1)
  integer,           intent(in)  :: extcombs(:)
  integer :: extcomb, colmatpos, i, j, eco

  M2colint = 0

  do eco = 1, size(extcombs)
    extcomb = extcombs(eco)
    colmatpos = 1*extcomb
    do i = 1, 1
      do j = 1, 1
        M2colint(extcomb) = M2colint(extcomb) + real(conjg(M(i))*K1(i+colmatpos,j)*M(j))
      end do
    end do
  end do

end subroutine colint


! **********************************************************************
subroutine colintmunu(M1, M2, M2colint)
! M1(i)    = <M1|Ci> colour component of matrix element
! M2(i)    = <M2|Ci> colour component of matrix element
! M2colint = <M1|M2>
!          = Sum_{i,j} <M1|Ci> * <Ci|Cj> * <Cj|M2>
!          = colour-summed squared matrix element
! K1(i,j) = <Ci|Cj>
! **********************************************************************
  use ol_colourmatrix_eellllbb_nenexeexmmxbbx_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(1)
  complex(REALKIND), intent(in)  :: M2(1)
  complex(REALKIND),    intent(out) :: M2colint
  integer :: i, j

  M2colint = 0

  do i = 1, 1
    do j = 1, 1
      M2colint = M2colint + M1(i)*K1(i,j)*conjg(M2(j))
    end do
  end do

end subroutine colintmunu

end subroutine amp2


#ifdef PRECISION_dp
subroutine colourvector(M1out, nhelout) &
  & bind(c,name="ol_tree_colvect_eellllbb_nenexeexmmxbbx_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(1,256)
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
    & bind(c,name="ol_f_amp2tree_eellllbb_nenexeexmmxbbx_1")
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
    & bind(c,name="ol_f_amp2ccone_eellllbb_nenexeexmmxbbx_1")
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
    & bind(c,name="ol_f_amp2ccall_eellllbb_nenexeexmmxbbx_1")
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
    & bind(c,name="ol_f_amp2hcone_eellllbb_nenexeexmmxbbx_1")
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
    & bind(c,name="ol_f_amp2hcall_eellllbb_nenexeexmmxbbx_1")
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
    & bind(c,name="ol_amp2tree_eellllbb_nenexeexmmxbbx_1")
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
    & bind(c,name="ol_amp2ccone_eellllbb_nenexeexmmxbbx_1")
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
    & bind(c,name="ol_amp2ccall_eellllbb_nenexeexmmxbbx_1")
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
    & bind(c,name="ol_amp2hcone_eellllbb_nenexeexmmxbbx_1")
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
    & bind(c,name="ol_amp2hcall_eellllbb_nenexeexmmxbbx_1")
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
    & bind(c,name="amp2tree_eellllbb_nenexeexmmxbbx_1_")
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
    & bind(c,name="amp2ccone_eellllbb_nenexeexmmxbbx_1_")
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
    & bind(c,name="amp2ccall_eellllbb_nenexeexmmxbbx_1_")
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
    & bind(c,name="amp2hcone_eellllbb_nenexeexmmxbbx_1_")
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
    & bind(c,name="amp2hcall_eellllbb_nenexeexmmxbbx_1_")
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

end module ol_tree_eellllbb_nenexeexmmxbbx_1_/**/REALKIND
