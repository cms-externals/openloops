
module ol_colourmatrix_ppllnnjj_vbs_nenmexmxuxcxdsg_2_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(94,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  12,   0]
  K1( 2,:) = [   0,  12]
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
  K1(31,:) = [  16,   0]
  K1(32,:) = [   0,  16]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [   0,   0]
  K1(38,:) = [   0,   0]
  K1(39,:) = [   0,   0]
  K1(40,:) = [   0,   0]
  K1(41,:) = [   0,   2]
  K1(42,:) = [   2,   0]
  K1(43,:) = [  16,   0]
  K1(44,:) = [   0,  16]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]
  K1(47,:) = [   0,   0]
  K1(48,:) = [   0,   0]
  K1(49,:) = [   0,   0]
  K1(50,:) = [   0,   0]
  K1(51,:) = [   0,   0]
  K1(52,:) = [   0,   0]
  K1(53,:) = [   2,   0]
  K1(54,:) = [   0, -16]
  K1(55,:) = [   0,  -2]
  K1(56,:) = [  -2,   0]
  K1(57,:) = [  16,   0]
  K1(58,:) = [   0,  16]
  K1(59,:) = [   0,   0]
  K1(60,:) = [   0,   0]
  K1(61,:) = [   0,   0]
  K1(62,:) = [   0,   0]
  K1(63,:) = [   0,   0]
  K1(64,:) = [   0,   0]
  K1(65,:) = [   0,   0]
  K1(66,:) = [   0,   0]
  K1(67,:) = [   0,  -2]
  K1(68,:) = [  -2,   0]
  K1(69,:) = [ -16,   0]
  K1(70,:) = [   0,   2]
  K1(71,:) = [   0,   2]
  K1(72,:) = [   2,   0]
  K1(73,:) = [  16,   0]
  K1(74,:) = [   0,  16]
  K1(75,:) = [   0,   0]
  K1(76,:) = [   0,   0]
  K1(77,:) = [   0,   0]
  K1(78,:) = [   0,   0]
  K1(79,:) = [   0,   0]
  K1(80,:) = [   0,   0]
  K1(81,:) = [   0,   0]
  K1(82,:) = [   0,   0]
  K1(83,:) = [ -18,   0]
  K1(84,:) = [   0,   0]
  K1(85,:) = [   0,   0]
  K1(86,:) = [   0, -18]
  K1(87,:) = [ -18,   0]
  K1(88,:) = [   0,   0]
  K1(89,:) = [   0,   0]
  K1(90,:) = [   0, -18]
  K1(91,:) = [  36,   0]
  K1(92,:) = [   0,  36]
  K1(93,:) = [   0,   0]
  K1(94,:) = [   0,   0]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllnnjj_vbs_nenmexmxuxcxdsg_2_/**/REALKIND



module ol_forced_parameters_ppllnnjj_vbs_nenmexmxuxcxdsg_2_/**/REALKIND
  implicit none
  contains
  subroutine check_forced_parameters
    use ol_parameters_decl_/**/REALKIND
    use ol_loop_parameters_decl_/**/REALKIND
#ifndef PRECISION_dp
    use ol_loop_parameters_decl_/**/DREALKIND, only: LeadingColour, nc, nf, CKMORDER
#endif
    implicit none
    logical, save :: checks_not_written = .true.

    if (checks_not_written) then
    ! e.g.
    ! if (ME /= 0) write(*,101) 'ME = 0'
  if (ME /= 0) write(*,101) 'ME = 0'
  if (MM /= 0) write(*,101) 'MM = 0'
  if (CKMORDER /= 0) write(*,101) 'CKMORDER = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppllnnjj_vbs_nenmexmxuxcxdsg_2_/**/REALKIND

module ol_tree_ppllnnjj_vbs_nenmexmxuxcxdsg_2_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(11)
  complex(REALKIND), save :: den(704)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 512 ! number of helicity configurations
  integer(intkind2), save :: nhel = 512 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(512) ! physical helicity states
  complex(DREALKIND) :: M1helarr(2,512) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f( 1) = (CI*eQED**6*gQCD)/(8._/**/REALKIND*sw**6)
    f( 2) = (CI*eQED**6*gQCD)/(4._/**/REALKIND*sw**6)
    f( 3) = (CI*cw**2*eQED**6*gQCD)/(4._/**/REALKIND*sw**6)
    f( 4) = (CI*eQED**6*gQCD*MW**2)/(4._/**/REALKIND*sw**6)
    f( 5) = (CI*cw*eQED**6*gQCD)/(4._/**/REALKIND*sw**5)
    f( 6) = (CI*eQED**6*gQCD)/(36._/**/REALKIND*sw**4)
    f( 7) = (CI*eQED**6*gQCD)/(18._/**/REALKIND*sw**4)
    f( 8) = (CI*eQED**6*gQCD)/(12._/**/REALKIND*sw**4)
    f( 9) = (CI*eQED**6*gQCD)/(9._/**/REALKIND*sw**4)
    f(10) = (CI*eQED**6*gQCD)/(6._/**/REALKIND*sw**4)
    f(11) = (CI*eQED**6*gQCD)/(4._/**/REALKIND*sw**4)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,288))
  den(2) = 1 / (Q(5,5) - MW2)
  den(3) = 1 / (Q(5,10) - MW2)
  den(4) = 1 / (Q(5,80) - MW2)
  den(6) = 1 / (Q(5,416) - MW2)
  den(9) = 1 / (Q(5,384))
  den(12) = 1 / (Q(5,272))
  den(13) = 1 / (Q(5,160) - MW2)
  den(15) = 1 / (Q(5,336) - MW2)
  den(18) = 1 / (Q(5,320))
  den(22) = 1 / (Q(5,133))
  den(24) = 1 / (Q(5,90))
  den(28) = 1 / (Q(5,90) - MZ2)
  den(32) = 1 / (Q(5,298))
  den(37) = 1 / (Q(5,138))
  den(39) = 1 / (Q(5,85))
  den(43) = 1 / (Q(5,85) - MZ2)
  den(47) = 1 / (Q(5,293))
  den(51) = 1 / (Q(5,85) - MH2)
  den(59) = 1 / (Q(5,90) - MH2)
  den(67) = 1 / (Q(5,37))
  den(73) = 1 / (Q(5,394))
  den(77) = 1 / (Q(5,42))
  den(83) = 1 / (Q(5,389))
  den(103) = 1 / (Q(5,218))
  den(112) = 1 / (Q(5,122))
  den(117) = 1 / (Q(5,213))
  den(122) = 1 / (Q(5,117))
  den(128) = 1 / (Q(5,69))
  den(130) = 1 / (Q(5,282))
  den(135) = 1 / (Q(5,170))
  den(139) = 1 / (Q(5,170) - MZ2)
  den(143) = 1 / (Q(5,74))
  den(145) = 1 / (Q(5,277))
  den(150) = 1 / (Q(5,165))
  den(154) = 1 / (Q(5,165) - MZ2)
  den(157) = 1 / (Q(5,165) - MH2)
  den(163) = 1 / (Q(5,170) - MH2)
  den(173) = 1 / (Q(5,21))
  den(179) = 1 / (Q(5,330))
  den(183) = 1 / (Q(5,26))
  den(189) = 1 / (Q(5,325))
  den(209) = 1 / (Q(5,234))
  den(218) = 1 / (Q(5,186))
  den(223) = 1 / (Q(5,229))
  den(228) = 1 / (Q(5,181))
  den(241) = 1 / (Q(5,346))
  den(244) = 1 / (Q(5,346) - MZ2)
  den(267) = 1 / (Q(5,341))
  den(270) = 1 / (Q(5,341) - MZ2)
  den(349) = 1 / (Q(5,426))
  den(352) = 1 / (Q(5,426) - MZ2)
  den(369) = 1 / (Q(5,421))
  den(372) = 1 / (Q(5,421) - MZ2)
  den(413) = 1 / (Q(5,82))
  den(419) = 1 / (Q(5,88))
  den(428) = 1 / (Q(5,418))
  den(433) = 1 / (Q(5,87))
  den(437) = 1 / (Q(5,424))
  den(464) = 1 / (Q(5,162))
  den(470) = 1 / (Q(5,168))
  den(479) = 1 / (Q(5,338))
  den(484) = 1 / (Q(5,167))
  den(488) = 1 / (Q(5,344))
  den(539) = 1 / (Q(5,81))
  den(549) = 1 / (Q(5,84))
  den(553) = 1 / (Q(5,417))
  den(559) = 1 / (Q(5,91))
  den(563) = 1 / (Q(5,420))
  den(590) = 1 / (Q(5,161))
  den(600) = 1 / (Q(5,164))
  den(604) = 1 / (Q(5,337))
  den(610) = 1 / (Q(5,171))
  den(614) = 1 / (Q(5,340))

  ! denominators

  den(5) = den(2)*den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(5)*den(7)
  den(10) = den(6)*den(9)
  den(11) = den(5)*den(10)
  den(14) = den(2)*den(3)*den(13)
  den(16) = den(12)*den(15)
  den(17) = den(14)*den(16)
  den(19) = den(15)*den(18)
  den(20) = den(14)*den(19)
  den(21) = den(3)*den(4)
  den(23) = den(2)*den(22)
  den(25) = den(21)*den(24)
  den(26) = den(1)*den(23)
  den(27) = den(25)*den(26)
  den(29) = den(21)*den(28)
  den(30) = den(26)*den(29)
  den(31) = den(1)*den(3)
  den(33) = den(31)*den(32)
  den(34) = den(4)*den(23)
  den(35) = den(33)*den(34)
  den(36) = den(2)*den(4)
  den(38) = den(3)*den(37)
  den(40) = den(36)*den(39)
  den(41) = den(1)*den(38)
  den(42) = den(40)*den(41)
  den(44) = den(36)*den(43)
  den(45) = den(41)*den(44)
  den(46) = den(1)*den(2)
  den(48) = den(46)*den(47)
  den(49) = den(4)*den(38)
  den(50) = den(48)*den(49)
  den(52) = den(36)*den(51)
  den(53) = den(3)*den(7)
  den(54) = den(52)*den(53)
  den(55) = den(40)*den(53)
  den(56) = den(44)*den(53)
  den(57) = den(33)*den(40)
  den(58) = den(33)*den(44)
  den(60) = den(21)*den(59)
  den(61) = den(2)*den(7)
  den(62) = den(60)*den(61)
  den(63) = den(25)*den(61)
  den(64) = den(29)*den(61)
  den(65) = den(25)*den(48)
  den(66) = den(29)*den(48)
  den(68) = den(2)*den(67)
  den(69) = den(9)*den(68)
  den(70) = den(25)*den(69)
  den(71) = den(29)*den(69)
  den(72) = den(3)*den(9)
  den(74) = den(72)*den(73)
  den(75) = den(4)*den(68)
  den(76) = den(74)*den(75)
  den(78) = den(3)*den(77)
  den(79) = den(9)*den(78)
  den(80) = den(40)*den(79)
  den(81) = den(44)*den(79)
  den(82) = den(2)*den(9)
  den(84) = den(82)*den(83)
  den(85) = den(4)*den(78)
  den(86) = den(84)*den(85)
  den(87) = den(3)*den(10)
  den(88) = den(52)*den(87)
  den(89) = den(40)*den(87)
  den(90) = den(44)*den(87)
  den(91) = den(40)*den(74)
  den(92) = den(44)*den(74)
  den(93) = den(2)*den(10)
  den(94) = den(60)*den(93)
  den(95) = den(25)*den(93)
  den(96) = den(29)*den(93)
  den(97) = den(25)*den(84)
  den(98) = den(29)*den(84)
  den(99) = den(47)*den(68)
  den(100) = den(49)*den(99)
  den(101) = den(38)*den(73)
  den(102) = den(75)*den(101)
  den(104) = den(25)*den(103)
  den(105) = den(68)*den(104)
  den(106) = den(29)*den(103)
  den(107) = den(68)*den(106)
  den(108) = den(32)*den(78)
  den(109) = den(34)*den(108)
  den(110) = den(23)*den(83)
  den(111) = den(85)*den(110)
  den(113) = den(25)*den(112)
  den(114) = den(23)*den(113)
  den(115) = den(29)*den(112)
  den(116) = den(23)*den(115)
  den(118) = den(40)*den(117)
  den(119) = den(78)*den(118)
  den(120) = den(44)*den(117)
  den(121) = den(78)*den(120)
  den(123) = den(40)*den(122)
  den(124) = den(38)*den(123)
  den(125) = den(44)*den(122)
  den(126) = den(38)*den(125)
  den(127) = den(3)*den(12)
  den(129) = den(2)*den(128)
  den(131) = den(127)*den(130)
  den(132) = den(13)*den(129)
  den(133) = den(131)*den(132)
  den(134) = den(3)*den(13)
  den(136) = den(134)*den(135)
  den(137) = den(12)*den(129)
  den(138) = den(136)*den(137)
  den(140) = den(134)*den(139)
  den(141) = den(137)*den(140)
  den(142) = den(2)*den(12)
  den(144) = den(3)*den(143)
  den(146) = den(142)*den(145)
  den(147) = den(13)*den(144)
  den(148) = den(146)*den(147)
  den(149) = den(2)*den(13)
  den(151) = den(149)*den(150)
  den(152) = den(12)*den(144)
  den(153) = den(151)*den(152)
  den(155) = den(149)*den(154)
  den(156) = den(152)*den(155)
  den(158) = den(149)*den(157)
  den(159) = den(3)*den(16)
  den(160) = den(158)*den(159)
  den(161) = den(151)*den(159)
  den(162) = den(155)*den(159)
  den(164) = den(134)*den(163)
  den(165) = den(2)*den(16)
  den(166) = den(164)*den(165)
  den(167) = den(136)*den(165)
  den(168) = den(140)*den(165)
  den(169) = den(136)*den(146)
  den(170) = den(140)*den(146)
  den(171) = den(131)*den(151)
  den(172) = den(131)*den(155)
  den(174) = den(2)*den(173)
  den(175) = den(18)*den(174)
  den(176) = den(136)*den(175)
  den(177) = den(140)*den(175)
  den(178) = den(3)*den(18)
  den(180) = den(178)*den(179)
  den(181) = den(13)*den(174)
  den(182) = den(180)*den(181)
  den(184) = den(3)*den(183)
  den(185) = den(18)*den(184)
  den(186) = den(151)*den(185)
  den(187) = den(155)*den(185)
  den(188) = den(2)*den(18)
  den(190) = den(188)*den(189)
  den(191) = den(13)*den(184)
  den(192) = den(190)*den(191)
  den(193) = den(2)*den(19)
  den(194) = den(164)*den(193)
  den(195) = den(136)*den(193)
  den(196) = den(140)*den(193)
  den(197) = den(3)*den(19)
  den(198) = den(158)*den(197)
  den(199) = den(151)*den(197)
  den(200) = den(155)*den(197)
  den(201) = den(151)*den(180)
  den(202) = den(155)*den(180)
  den(203) = den(136)*den(190)
  den(204) = den(140)*den(190)
  den(205) = den(145)*den(174)
  den(206) = den(147)*den(205)
  den(207) = den(144)*den(179)
  den(208) = den(181)*den(207)
  den(210) = den(136)*den(209)
  den(211) = den(174)*den(210)
  den(212) = den(140)*den(209)
  den(213) = den(174)*den(212)
  den(214) = den(130)*den(184)
  den(215) = den(132)*den(214)
  den(216) = den(129)*den(189)
  den(217) = den(191)*den(216)
  den(219) = den(136)*den(218)
  den(220) = den(129)*den(219)
  den(221) = den(140)*den(218)
  den(222) = den(129)*den(221)
  den(224) = den(151)*den(223)
  den(225) = den(184)*den(224)
  den(226) = den(155)*den(223)
  den(227) = den(184)*den(226)
  den(229) = den(151)*den(228)
  den(230) = den(144)*den(229)
  den(231) = den(155)*den(228)
  den(232) = den(144)*den(231)
  den(233) = den(68)*den(150)
  den(234) = den(152)*den(233)
  den(235) = den(68)*den(154)
  den(236) = den(152)*den(235)
  den(237) = den(16)*den(68)
  den(238) = den(38)*den(237)
  den(239) = den(159)*den(233)
  den(240) = den(159)*den(235)
  den(242) = den(131)*den(241)
  den(243) = den(68)*den(242)
  den(245) = den(131)*den(244)
  den(246) = den(68)*den(245)
  den(247) = den(78)*den(135)
  den(248) = den(137)*den(247)
  den(249) = den(78)*den(139)
  den(250) = den(137)*den(249)
  den(251) = den(38)*den(135)
  den(252) = den(137)*den(251)
  den(253) = den(38)*den(139)
  den(254) = den(137)*den(253)
  den(255) = den(16)*den(78)
  den(256) = den(23)*den(255)
  den(257) = den(23)*den(150)
  den(258) = den(152)*den(257)
  den(259) = den(23)*den(154)
  den(260) = den(152)*den(259)
  den(261) = den(159)*den(257)
  den(262) = den(159)*den(259)
  den(263) = den(131)*den(257)
  den(264) = den(131)*den(259)
  den(265) = den(165)*den(247)
  den(266) = den(165)*den(249)
  den(268) = den(146)*den(267)
  den(269) = den(78)*den(268)
  den(271) = den(146)*den(270)
  den(272) = den(78)*den(271)
  den(273) = den(165)*den(251)
  den(274) = den(165)*den(253)
  den(275) = den(146)*den(251)
  den(276) = den(146)*den(253)
  den(277) = den(144)*den(174)
  den(278) = den(7)*den(277)
  den(279) = den(39)*den(174)
  den(280) = den(41)*den(279)
  den(281) = den(43)*den(174)
  den(282) = den(41)*den(281)
  den(283) = den(53)*den(279)
  den(284) = den(53)*den(281)
  den(285) = den(33)*den(279)
  den(286) = den(33)*den(281)
  den(287) = den(129)*den(184)
  den(288) = den(7)*den(287)
  den(289) = den(39)*den(129)
  den(290) = den(41)*den(289)
  den(291) = den(43)*den(129)
  den(292) = den(41)*den(291)
  den(293) = den(33)*den(289)
  den(294) = den(33)*den(291)
  den(295) = den(53)*den(289)
  den(296) = den(53)*den(291)
  den(297) = den(24)*den(184)
  den(298) = den(26)*den(297)
  den(299) = den(28)*den(184)
  den(300) = den(26)*den(299)
  den(301) = den(24)*den(144)
  den(302) = den(26)*den(301)
  den(303) = den(28)*den(144)
  den(304) = den(26)*den(303)
  den(305) = den(61)*den(297)
  den(306) = den(61)*den(299)
  den(307) = den(48)*den(297)
  den(308) = den(48)*den(299)
  den(309) = den(48)*den(301)
  den(310) = den(48)*den(303)
  den(311) = den(61)*den(301)
  den(312) = den(61)*den(303)
  den(313) = den(175)*den(247)
  den(314) = den(175)*den(249)
  den(315) = den(175)*den(251)
  den(316) = den(175)*den(253)
  den(317) = den(185)*den(233)
  den(318) = den(185)*den(235)
  den(319) = den(19)*den(68)
  den(320) = den(38)*den(319)
  den(321) = den(197)*den(233)
  den(322) = den(197)*den(235)
  den(323) = den(180)*den(241)
  den(324) = den(68)*den(323)
  den(325) = den(180)*den(244)
  den(326) = den(68)*den(325)
  den(327) = den(185)*den(257)
  den(328) = den(185)*den(259)
  den(329) = den(19)*den(78)
  den(330) = den(23)*den(329)
  den(331) = den(23)*den(323)
  den(332) = den(23)*den(325)
  den(333) = den(197)*den(257)
  den(334) = den(197)*den(259)
  den(335) = den(193)*den(247)
  den(336) = den(193)*den(249)
  den(337) = den(190)*den(267)
  den(338) = den(78)*den(337)
  den(339) = den(190)*den(270)
  den(340) = den(78)*den(339)
  den(341) = den(38)*den(337)
  den(342) = den(38)*den(339)
  den(343) = den(193)*den(251)
  den(344) = den(193)*den(253)
  den(345) = den(79)*den(279)
  den(346) = den(79)*den(281)
  den(347) = den(10)*den(174)
  den(348) = den(144)*den(347)
  den(350) = den(74)*den(349)
  den(351) = den(174)*den(350)
  den(353) = den(74)*den(352)
  den(354) = den(174)*den(353)
  den(355) = den(87)*den(279)
  den(356) = den(87)*den(281)
  den(357) = den(69)*den(297)
  den(358) = den(69)*den(299)
  den(359) = den(69)*den(301)
  den(360) = den(69)*den(303)
  den(361) = den(10)*den(184)
  den(362) = den(129)*den(361)
  den(363) = den(79)*den(289)
  den(364) = den(79)*den(291)
  den(365) = den(74)*den(289)
  den(366) = den(74)*den(291)
  den(367) = den(87)*den(289)
  den(368) = den(87)*den(291)
  den(370) = den(84)*den(369)
  den(371) = den(184)*den(370)
  den(373) = den(84)*den(372)
  den(374) = den(184)*den(373)
  den(375) = den(93)*den(297)
  den(376) = den(93)*den(299)
  den(377) = den(84)*den(301)
  den(378) = den(84)*den(303)
  den(379) = den(93)*den(301)
  den(380) = den(93)*den(303)
  den(381) = den(108)*den(279)
  den(382) = den(108)*den(281)
  den(383) = den(205)*den(247)
  den(384) = den(205)*den(249)
  den(385) = den(101)*den(279)
  den(386) = den(101)*den(281)
  den(387) = den(205)*den(251)
  den(388) = den(205)*den(253)
  den(389) = den(99)*den(297)
  den(390) = den(99)*den(299)
  den(391) = den(214)*den(233)
  den(392) = den(214)*den(235)
  den(393) = den(99)*den(301)
  den(394) = den(99)*den(303)
  den(395) = den(207)*den(233)
  den(396) = den(207)*den(235)
  den(397) = den(108)*den(289)
  den(398) = den(108)*den(291)
  den(399) = den(216)*den(247)
  den(400) = den(216)*den(249)
  den(401) = den(101)*den(289)
  den(402) = den(101)*den(291)
  den(403) = den(216)*den(251)
  den(404) = den(216)*den(253)
  den(405) = den(110)*den(297)
  den(406) = den(110)*den(299)
  den(407) = den(214)*den(257)
  den(408) = den(214)*den(259)
  den(409) = den(110)*den(301)
  den(410) = den(110)*den(303)
  den(411) = den(207)*den(257)
  den(412) = den(207)*den(259)
  den(414) = den(4)*den(413)
  den(415) = den(24)*den(414)
  den(416) = den(26)*den(415)
  den(417) = den(28)*den(414)
  den(418) = den(26)*den(417)
  den(420) = den(4)*den(419)
  den(421) = den(28)*den(420)
  den(422) = den(26)*den(421)
  den(423) = den(48)*den(415)
  den(424) = den(48)*den(417)
  den(425) = den(61)*den(415)
  den(426) = den(61)*den(417)
  den(427) = den(48)*den(421)
  den(429) = den(7)*den(428)
  den(430) = den(40)*den(429)
  den(431) = den(44)*den(429)
  den(432) = den(61)*den(421)
  den(434) = den(44)*den(433)
  den(435) = den(7)*den(434)
  den(436) = den(2)*den(414)
  den(438) = den(7)*den(437)
  den(439) = den(436)*den(438)
  den(440) = den(2)*den(420)
  den(441) = den(429)*den(440)
  den(442) = den(69)*den(415)
  den(443) = den(69)*den(417)
  den(444) = den(69)*den(421)
  den(445) = den(84)*den(415)
  den(446) = den(84)*den(417)
  den(447) = den(93)*den(415)
  den(448) = den(93)*den(417)
  den(449) = den(84)*den(421)
  den(450) = den(10)*den(428)
  den(451) = den(40)*den(450)
  den(452) = den(44)*den(450)
  den(453) = den(93)*den(421)
  den(454) = den(10)*den(434)
  den(455) = den(10)*den(437)
  den(456) = den(436)*den(455)
  den(457) = den(440)*den(450)
  den(458) = den(99)*den(415)
  den(459) = den(99)*den(417)
  den(460) = den(99)*den(421)
  den(461) = den(110)*den(415)
  den(462) = den(110)*den(417)
  den(463) = den(110)*den(421)
  den(465) = den(13)*den(464)
  den(466) = den(135)*den(465)
  den(467) = den(137)*den(466)
  den(468) = den(139)*den(465)
  den(469) = den(137)*den(468)
  den(471) = den(13)*den(470)
  den(472) = den(139)*den(471)
  den(473) = den(137)*den(472)
  den(474) = den(146)*den(466)
  den(475) = den(146)*den(468)
  den(476) = den(165)*den(466)
  den(477) = den(165)*den(468)
  den(478) = den(146)*den(472)
  den(480) = den(16)*den(479)
  den(481) = den(151)*den(480)
  den(482) = den(155)*den(480)
  den(483) = den(165)*den(472)
  den(485) = den(155)*den(484)
  den(486) = den(16)*den(485)
  den(487) = den(2)*den(465)
  den(489) = den(16)*den(488)
  den(490) = den(487)*den(489)
  den(491) = den(2)*den(471)
  den(492) = den(480)*den(491)
  den(493) = den(175)*den(466)
  den(494) = den(175)*den(468)
  den(495) = den(175)*den(472)
  den(496) = den(190)*den(472)
  den(497) = den(19)*den(479)
  den(498) = den(151)*den(497)
  den(499) = den(155)*den(497)
  den(500) = den(190)*den(466)
  den(501) = den(190)*den(468)
  den(502) = den(193)*den(466)
  den(503) = den(193)*den(468)
  den(504) = den(19)*den(485)
  den(505) = den(193)*den(472)
  den(506) = den(491)*den(497)
  den(507) = den(19)*den(488)
  den(508) = den(487)*den(507)
  den(509) = den(205)*den(466)
  den(510) = den(205)*den(468)
  den(511) = den(205)*den(472)
  den(512) = den(216)*den(472)
  den(513) = den(216)*den(466)
  den(514) = den(216)*den(468)
  den(515) = den(233)*den(480)
  den(516) = den(235)*den(480)
  den(517) = den(235)*den(489)
  den(518) = den(259)*den(489)
  den(519) = den(257)*den(480)
  den(520) = den(259)*den(480)
  den(521) = den(281)*den(438)
  den(522) = den(279)*den(429)
  den(523) = den(281)*den(429)
  den(524) = den(291)*den(438)
  den(525) = den(289)*den(429)
  den(526) = den(291)*den(429)
  den(527) = den(233)*den(497)
  den(528) = den(235)*den(497)
  den(529) = den(235)*den(507)
  den(530) = den(257)*den(497)
  den(531) = den(259)*den(497)
  den(532) = den(259)*den(507)
  den(533) = den(279)*den(450)
  den(534) = den(281)*den(450)
  den(535) = den(281)*den(455)
  den(536) = den(291)*den(455)
  den(537) = den(289)*den(450)
  den(538) = den(291)*den(450)
  den(540) = den(4)*den(539)
  den(541) = den(39)*den(540)
  den(542) = den(41)*den(541)
  den(543) = den(43)*den(540)
  den(544) = den(41)*den(543)
  den(545) = den(33)*den(541)
  den(546) = den(33)*den(543)
  den(547) = den(53)*den(541)
  den(548) = den(53)*den(543)
  den(550) = den(4)*den(549)
  den(551) = den(43)*den(550)
  den(552) = den(33)*den(551)
  den(554) = den(7)*den(553)
  den(555) = den(25)*den(554)
  den(556) = den(29)*den(554)
  den(557) = den(41)*den(551)
  den(558) = den(53)*den(551)
  den(560) = den(29)*den(559)
  den(561) = den(7)*den(560)
  den(562) = den(3)*den(540)
  den(564) = den(7)*den(563)
  den(565) = den(562)*den(564)
  den(566) = den(3)*den(550)
  den(567) = den(554)*den(566)
  den(568) = den(79)*den(541)
  den(569) = den(79)*den(543)
  den(570) = den(74)*den(541)
  den(571) = den(74)*den(543)
  den(572) = den(87)*den(541)
  den(573) = den(87)*den(543)
  den(574) = den(74)*den(551)
  den(575) = den(10)*den(553)
  den(576) = den(25)*den(575)
  den(577) = den(29)*den(575)
  den(578) = den(79)*den(551)
  den(579) = den(87)*den(551)
  den(580) = den(10)*den(560)
  den(581) = den(10)*den(563)
  den(582) = den(562)*den(581)
  den(583) = den(566)*den(575)
  den(584) = den(108)*den(541)
  den(585) = den(108)*den(543)
  den(586) = den(101)*den(541)
  den(587) = den(101)*den(543)
  den(588) = den(101)*den(551)
  den(589) = den(108)*den(551)
  den(591) = den(13)*den(590)
  den(592) = den(150)*den(591)
  den(593) = den(152)*den(592)
  den(594) = den(154)*den(591)
  den(595) = den(152)*den(594)
  den(596) = den(131)*den(592)
  den(597) = den(131)*den(594)
  den(598) = den(159)*den(592)
  den(599) = den(159)*den(594)
  den(601) = den(13)*den(600)
  den(602) = den(154)*den(601)
  den(603) = den(131)*den(602)
  den(605) = den(16)*den(604)
  den(606) = den(136)*den(605)
  den(607) = den(140)*den(605)
  den(608) = den(152)*den(602)
  den(609) = den(159)*den(602)
  den(611) = den(140)*den(610)
  den(612) = den(16)*den(611)
  den(613) = den(3)*den(591)
  den(615) = den(16)*den(614)
  den(616) = den(613)*den(615)
  den(617) = den(3)*den(601)
  den(618) = den(605)*den(617)
  den(619) = den(180)*den(602)
  den(620) = den(19)*den(604)
  den(621) = den(136)*den(620)
  den(622) = den(140)*den(620)
  den(623) = den(185)*den(592)
  den(624) = den(185)*den(594)
  den(625) = den(180)*den(592)
  den(626) = den(180)*den(594)
  den(627) = den(197)*den(592)
  den(628) = den(197)*den(594)
  den(629) = den(185)*den(602)
  den(630) = den(19)*den(611)
  den(631) = den(197)*den(602)
  den(632) = den(617)*den(620)
  den(633) = den(19)*den(614)
  den(634) = den(613)*den(633)
  den(635) = den(207)*den(602)
  den(636) = den(214)*den(592)
  den(637) = den(214)*den(594)
  den(638) = den(207)*den(592)
  den(639) = den(207)*den(594)
  den(640) = den(214)*den(602)
  den(641) = den(253)*den(615)
  den(642) = den(247)*den(605)
  den(643) = den(249)*den(605)
  den(644) = den(251)*den(605)
  den(645) = den(253)*den(605)
  den(646) = den(249)*den(615)
  den(647) = den(303)*den(564)
  den(648) = den(299)*den(564)
  den(649) = den(297)*den(554)
  den(650) = den(299)*den(554)
  den(651) = den(301)*den(554)
  den(652) = den(303)*den(554)
  den(653) = den(247)*den(620)
  den(654) = den(249)*den(620)
  den(655) = den(251)*den(620)
  den(656) = den(253)*den(620)
  den(657) = den(253)*den(633)
  den(658) = den(249)*den(633)
  den(659) = den(303)*den(581)
  den(660) = den(297)*den(575)
  den(661) = den(299)*den(575)
  den(662) = den(301)*den(575)
  den(663) = den(303)*den(575)
  den(664) = den(299)*den(581)
  den(665) = den(438)*den(551)
  den(666) = den(421)*den(564)
  den(667) = den(417)*den(564)
  den(668) = den(429)*den(551)
  den(669) = den(438)*den(543)
  den(670) = den(429)*den(541)
  den(671) = den(429)*den(543)
  den(672) = den(421)*den(554)
  den(673) = den(415)*den(554)
  den(674) = den(417)*den(554)
  den(675) = den(455)*den(551)
  den(676) = den(421)*den(581)
  den(677) = den(417)*den(581)
  den(678) = den(450)*den(551)
  den(679) = den(455)*den(543)
  den(680) = den(450)*den(541)
  den(681) = den(450)*den(543)
  den(682) = den(421)*den(575)
  den(683) = den(415)*den(575)
  den(684) = den(417)*den(575)
  den(685) = den(489)*den(602)
  den(686) = den(472)*den(615)
  den(687) = den(468)*den(615)
  den(688) = den(480)*den(602)
  den(689) = den(489)*den(594)
  den(690) = den(480)*den(592)
  den(691) = den(480)*den(594)
  den(692) = den(472)*den(605)
  den(693) = den(466)*den(605)
  den(694) = den(468)*den(605)
  den(695) = den(472)*den(633)
  den(696) = den(507)*den(602)
  den(697) = den(497)*den(602)
  den(698) = den(468)*den(633)
  den(699) = den(472)*den(620)
  den(700) = den(466)*den(620)
  den(701) = den(468)*den(620)
  den(702) = den(507)*den(594)
  den(703) = den(497)*den(592)
  den(704) = den(497)*den(594)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_ppllnnjj_vbs_nenmexmxuxcxdsg_2")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_ppllnnjj_vbs_nenmexmxuxcxdsg_2_/**/DREALKIND, only: &
    & ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for nu_e nu_mu e+ mu+ anti-up anti-charm down strange glue -> 0
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
  use ol_external_ppllnnjj_vbs_nenmexmxuxcxdsg_2, only: &
    & external_perm_ppllnnjj_vbs_nenmexmxuxcxdsg_2, &
    & external_perm_inv_ppllnnjj_vbs_nenmexmxuxcxdsg_2, &
    & extcomb_perm_ppllnnjj_vbs_nenmexmxuxcxdsg_2, &
    & average_factor_ppllnnjj_vbs_nenmexmxuxcxdsg_2
  use ol_external_ppllnnjj_vbs_nenmexmxuxcxdsg_2, only: &
    & H, hel_not_initialised, hel_init, POLSEL
  use ol_colourmatrix_ppllnnjj_vbs_nenmexmxuxcxdsg_2_/**/REALKIND, only: &
    & colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_ppllnnjj_vbs_nenmexmxuxcxdsg_2_/**/REALKIND, only: &
    & check_forced_parameters
  use ol_heltables_ppllnnjj_vbs_nenmexmxuxcxdsg_2
  use ol_kinematics_/**/REALKIND, only: LC2Std_Rep_cmplx
  implicit none

  real(DREALKIND), intent(in)  :: P_scatt(0:3,9)
  real(REALKIND),  intent(out) :: M2(0:47-1)
  real(REALKIND),  intent(out) :: M2munu(4,4)
  integer,         intent(in)  :: I
  real(REALKIND),  intent(in)  :: MOM(0:3)
  integer,         intent(in)  :: nextcombs
  integer,         intent(in)  :: extcombs(nextcombs)

  integer           :: ReplacePol, JBmunu, extcombs_permuted(nextcombs), shift, k, r, m, n
  real(REALKIND)    :: P(0:3,9)
  real(REALKIND)    :: extmasses2(9)
  real(REALKIND)    :: M2add(0:47-1)
  complex(REALKIND) :: M2munuadd(2)
  complex(REALKIND) :: MOM_LC(4), M1(2), M1helarray(2,512)
  real(REALKIND)    :: P_scatt_intern(0:3,9)
  complex(REALKIND) :: epLC(1:4), epStd(1:4,2)
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(2), ex6(2), ex7(2), ex8(2), ex9(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,16), wf8(8,40), wf16(16,120), wf32(32,206), wf64(64,10), wf512(512,452)

  type(polcont) :: A(512,452)
  complex(REALKIND) :: Aj(452)

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
  extmasses2 = [ rZERO2, rZERO2, rZERO2, rZERO2, rZERO2, rZERO2, rZERO2, rZERO2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, &
    & external_perm_inv_ppllnnjj_vbs_nenmexmxuxcxdsg_2,9)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,9)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_ppllnnjj_vbs_nenmexmxuxcxdsg_2(I)
  else if (I < 0) then
    JBmunu = external_perm_ppllnnjj_vbs_nenmexmxuxcxdsg_2(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_ppllnnjj_vbs_nenmexmxuxcxdsg_2(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  42 continue
  ! external WFs
  call pol_wf_Q(P(:,1), rZERO, H1, ex1, POLSEL(1))
  call pol_wf_Q(P(:,2), rZERO, H2, ex2, POLSEL(2))
  call pol_wf_A(P(:,3), rZERO, H3, ex3, POLSEL(3))
  call pol_wf_A(P(:,4), rZERO, H4, ex4, POLSEL(4))
  call pol_wf_A(P(:,5), rZERO, H5, ex5, POLSEL(5))
  call pol_wf_A(P(:,6), rZERO, H6, ex6, POLSEL(6))
  call pol_wf_Q(P(:,7), rZERO, H7, ex7, POLSEL(7))
  call pol_wf_Q(P(:,8), rZERO, H8, ex8, POLSEL(8))
  call pol_wf_V(P(:,9), rZERO, H9, ex9, POLSEL(9))


  if (ntry == 1) then
    shift = 1
    ! call helbookkeeping_flip(H1, 1, shift, eflip, exthel, firstpol)
    ! call helbookkeeping_wf(H1, ex1, shift) ...

    if (any(POLSEL /= 0)) then

      call pol_wf_Q(P(:,1), rZERO, H1, ex1, 0)
      call pol_wf_Q(P(:,2), rZERO, H2, ex2, 0)
      call pol_wf_A(P(:,3), rZERO, H3, ex3, 0)
      call pol_wf_A(P(:,4), rZERO, H4, ex4, 0)
      call pol_wf_A(P(:,5), rZERO, H5, ex5, 0)
      call pol_wf_A(P(:,6), rZERO, H6, ex6, 0)
      call pol_wf_Q(P(:,7), rZERO, H7, ex7, 0)
      call pol_wf_Q(P(:,8), rZERO, H8, ex8, 0)
      call pol_wf_V(P(:,9), rZERO, H9, ex9, 0)

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
    call helbookkeeping_flip(H9, 9, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H9, ex9, shift)

  end if

  ! internal WFs
  ! e.g. call vert_VQ_A(ntry, ex3, ex1, wf1, n1, t1) ...
  call vert_QA_W(ntry, ex1, ex3, wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_QA_W(ntry, ex2, ex4, wf4(:,2), n3(:,2), t3x4(:,:,2))
  call vert_QA_W(ntry, ex7, ex5, wf4(:,3), n3(:,3), t3x4(:,:,3))
  call vert_AV_Q(ntry, ex6, ex9, wf4(:,4), n3(:,4), t3x4(:,:,4))
  call prop_A_Q(ntry, wf4(:,4), Q(:,288), ZERO, 0_intkind1, wf4(:,5), n2(1))
  call prop_W_W(ntry, wf4(:,1), Q(:,5), MW, 1_intkind1, wf4(:,6), n2(2))
  call prop_W_W(ntry, wf4(:,2), Q(:,10), MW, 1_intkind1, wf4(:,7), n2(3))
  call prop_W_W(ntry, wf4(:,3), Q(:,80), MW, 1_intkind1, wf4(:,8), n2(4))
  call vert_QA_W(ntry, ex8, wf4(:,5), wf8(:,1), n3(:,5), t3x8(:,:,1))
  call vert_WWV_V(ntry, wf4(:,6), wf4(:,7), wf4(:,8), wf64(:,1), n4(:,1), t4x64(:,:,1))
  call prop_W_W(ntry, wf8(:,1), Q(:,416), MW, 1_intkind1, wf8(:,2), n2(5))
  call vert_VQ_A(ntry, ex9, ex8, wf4(:,9), n3(:,6), t3x4(:,:,5))
  call prop_Q_A(ntry, wf4(:,9), Q(:,384), ZERO, 0_intkind1, wf4(:,10), n2(6))
  call vert_QA_W(ntry, wf4(:,10), ex6, wf8(:,3), n3(:,7), t3x8(:,:,2))
  call prop_W_W(ntry, wf8(:,3), Q(:,416), MW, 1_intkind1, wf8(:,4), n2(7))
  call vert_AV_Q(ntry, ex5, ex9, wf4(:,11), n3(:,8), t3x4(:,:,6))
  call vert_QA_W(ntry, ex8, ex6, wf4(:,12), n3(:,9), t3x4(:,:,7))
  call prop_A_Q(ntry, wf4(:,11), Q(:,272), ZERO, 0_intkind1, wf4(:,13), n2(8))
  call prop_W_W(ntry, wf4(:,12), Q(:,160), MW, 1_intkind1, wf4(:,14), n2(9))
  call vert_QA_W(ntry, ex7, wf4(:,13), wf8(:,5), n3(:,10), t3x8(:,:,3))
  call vert_WWV_V(ntry, wf4(:,6), wf4(:,7), wf4(:,14), wf64(:,2), n4(:,2), t4x64(:,:,2))
  call prop_W_W(ntry, wf8(:,5), Q(:,336), MW, 1_intkind1, wf8(:,6), n2(10))
  call vert_VQ_A(ntry, ex9, ex7, wf4(:,15), n3(:,11), t3x4(:,:,8))
  call prop_Q_A(ntry, wf4(:,15), Q(:,320), ZERO, 0_intkind1, wf4(:,16), n2(11))
  call vert_QA_W(ntry, wf4(:,16), ex5, wf8(:,7), n3(:,12), t3x8(:,:,4))
  call prop_W_W(ntry, wf8(:,7), Q(:,336), MW, 1_intkind1, wf8(:,8), n2(12))
  call vert_WQ_A(ntry, wf4(:,6), ex8, wf8(:,9), n3(:,13), t3x8(:,:,5))
  call vert_UV_W(ntry, wf4(:,7), Q(:,10), wf4(:,8), Q(:,80), wf16(:,1), n3(:,14), t3x16(:,:,1))
  call prop_Q_A(ntry, wf8(:,9), Q(:,133), ZERO, 0_intkind1, wf8(:,10), n2(13))
  call vert_QA_V(ntry, wf8(:,10), wf4(:,5), wf32(:,1), n3(:,15), t3x32(:,:,1))
  call prop_W_W(ntry, wf16(:,1), Q(:,90), MZ, 1_intkind1, wf16(:,2), n2(14))
  call vert_QA_Z(gZu,ntry, wf8(:,10), wf4(:,5), wf32(:,2), n3(:,16), t3x32(:,:,2))
  call vert_AW_Q(ntry, wf4(:,5), wf4(:,7), wf16(:,3), n3(:,17), t3x16(:,:,2))
  call prop_A_Q(ntry, wf16(:,3), Q(:,298), ZERO, 0_intkind1, wf16(:,4), n2(15))
  call vert_WQ_A(ntry, wf4(:,8), wf8(:,10), wf32(:,3), n3(:,18), t3x32(:,:,3))
  call vert_WQ_A(ntry, wf4(:,7), ex8, wf8(:,11), n3(:,19), t3x8(:,:,6))
  call vert_UV_W(ntry, wf4(:,6), Q(:,5), wf4(:,8), Q(:,80), wf16(:,5), n3(:,20), t3x16(:,:,3))
  call prop_Q_A(ntry, wf8(:,11), Q(:,138), ZERO, 0_intkind1, wf8(:,12), n2(16))
  call vert_QA_V(ntry, wf8(:,12), wf4(:,5), wf32(:,4), n3(:,21), t3x32(:,:,4))
  call prop_W_W(ntry, wf16(:,5), Q(:,85), MZ, 1_intkind1, wf16(:,6), n2(17))
  call vert_QA_Z(gZu,ntry, wf8(:,12), wf4(:,5), wf32(:,5), n3(:,22), t3x32(:,:,5))
  call vert_AW_Q(ntry, wf4(:,5), wf4(:,6), wf16(:,7), n3(:,23), t3x16(:,:,4))
  call prop_A_Q(ntry, wf16(:,7), Q(:,293), ZERO, 0_intkind1, wf16(:,8), n2(18))
  call vert_WQ_A(ntry, wf4(:,8), wf8(:,12), wf32(:,6), n3(:,24), t3x32(:,:,6))
  call vert_VV_S(ntry, wf4(:,6), wf4(:,8), wf16(:,9), n3(:,25), t3x16(:,:,5))
  call vert_VV_S(ntry, wf4(:,7), wf8(:,2), wf32(:,7), n3(:,26), t3x32(:,:,7))
  call vert_UV_W(ntry, wf4(:,7), Q(:,10), wf8(:,2), Q(:,416), wf32(:,8), n3(:,27), t3x32(:,:,8))
  call vert_VQ_A(ntry, wf16(:,5), ex8, wf32(:,9), n3(:,28), t3x32(:,:,9))
  call vert_ZQ_A(gZd,ntry, wf16(:,6), ex8, wf32(:,10), n3(:,29), t3x32(:,:,10))
  call vert_VV_S(ntry, wf4(:,7), wf4(:,8), wf16(:,10), n3(:,30), t3x16(:,:,6))
  call vert_VV_S(ntry, wf4(:,6), wf8(:,2), wf32(:,11), n3(:,31), t3x32(:,:,11))
  call vert_UV_W(ntry, wf4(:,6), Q(:,5), wf8(:,2), Q(:,416), wf32(:,12), n3(:,32), t3x32(:,:,12))
  call vert_QA_V(ntry, ex8, wf16(:,8), wf32(:,13), n3(:,33), t3x32(:,:,13))
  call vert_QA_Z(gZd,ntry, ex8, wf16(:,8), wf32(:,14), n3(:,34), t3x32(:,:,14))
  call vert_AW_Q(ntry, ex6, wf4(:,6), wf8(:,13), n3(:,35), t3x8(:,:,7))
  call prop_A_Q(ntry, wf8(:,13), Q(:,37), ZERO, 0_intkind1, wf8(:,14), n2(19))
  call vert_QA_V(ntry, wf4(:,10), wf8(:,14), wf32(:,15), n3(:,36), t3x32(:,:,15))
  call vert_QA_Z(gZd,ntry, wf4(:,10), wf8(:,14), wf32(:,16), n3(:,37), t3x32(:,:,16))
  call vert_WQ_A(ntry, wf4(:,7), wf4(:,10), wf16(:,11), n3(:,38), t3x16(:,:,7))
  call prop_Q_A(ntry, wf16(:,11), Q(:,394), ZERO, 0_intkind1, wf16(:,12), n2(20))
  call vert_AW_Q(ntry, wf8(:,14), wf4(:,8), wf32(:,17), n3(:,39), t3x32(:,:,17))
  call vert_AW_Q(ntry, ex6, wf4(:,7), wf8(:,15), n3(:,40), t3x8(:,:,8))
  call prop_A_Q(ntry, wf8(:,15), Q(:,42), ZERO, 0_intkind1, wf8(:,16), n2(21))
  call vert_QA_V(ntry, wf4(:,10), wf8(:,16), wf32(:,18), n3(:,41), t3x32(:,:,18))
  call vert_QA_Z(gZd,ntry, wf4(:,10), wf8(:,16), wf32(:,19), n3(:,42), t3x32(:,:,19))
  call vert_WQ_A(ntry, wf4(:,6), wf4(:,10), wf16(:,13), n3(:,43), t3x16(:,:,8))
  call prop_Q_A(ntry, wf16(:,13), Q(:,389), ZERO, 0_intkind1, wf16(:,14), n2(22))
  call vert_AW_Q(ntry, wf8(:,16), wf4(:,8), wf32(:,20), n3(:,44), t3x32(:,:,20))
  call vert_VV_S(ntry, wf4(:,7), wf8(:,4), wf32(:,21), n3(:,45), t3x32(:,:,21))
  call vert_UV_W(ntry, wf4(:,7), Q(:,10), wf8(:,4), Q(:,416), wf32(:,22), n3(:,46), t3x32(:,:,22))
  call vert_AV_Q(ntry, ex6, wf16(:,5), wf32(:,23), n3(:,47), t3x32(:,:,23))
  call vert_AZ_Q(gZu,ntry, ex6, wf16(:,6), wf32(:,24), n3(:,48), t3x32(:,:,24))
  call vert_VV_S(ntry, wf4(:,6), wf8(:,4), wf32(:,25), n3(:,49), t3x32(:,:,25))
  call vert_UV_W(ntry, wf4(:,6), Q(:,5), wf8(:,4), Q(:,416), wf32(:,26), n3(:,50), t3x32(:,:,26))
  call vert_QA_V(ntry, wf16(:,14), ex6, wf32(:,27), n3(:,51), t3x32(:,:,27))
  call vert_QA_Z(gZu,ntry, wf16(:,14), ex6, wf32(:,28), n3(:,52), t3x32(:,:,28))
  call vert_AV_Q(ntry, wf8(:,14), ex9, wf16(:,15), n3(:,53), t3x16(:,:,9))
  call prop_A_Q(ntry, wf16(:,15), Q(:,293), ZERO, 0_intkind1, wf16(:,16), n2(23))
  call vert_VQ_A(ntry, ex9, wf8(:,12), wf16(:,17), n3(:,54), t3x16(:,:,10))
  call prop_Q_A(ntry, wf16(:,17), Q(:,394), ZERO, 0_intkind1, wf16(:,18), n2(24))
  call vert_VQ_A(ntry, wf16(:,1), ex8, wf32(:,29), n3(:,55), t3x32(:,:,29))
  call prop_Q_A(ntry, wf32(:,29), Q(:,218), ZERO, 0_intkind1, wf32(:,30), n2(25))
  call vert_ZQ_A(gZd,ntry, wf16(:,2), ex8, wf32(:,31), n3(:,56), t3x32(:,:,30))
  call prop_Q_A(ntry, wf32(:,31), Q(:,218), ZERO, 0_intkind1, wf32(:,32), n2(26))
  call vert_AV_Q(ntry, wf8(:,16), ex9, wf16(:,19), n3(:,57), t3x16(:,:,11))
  call prop_A_Q(ntry, wf16(:,19), Q(:,298), ZERO, 0_intkind1, wf16(:,20), n2(27))
  call vert_VQ_A(ntry, ex9, wf8(:,10), wf16(:,21), n3(:,58), t3x16(:,:,12))
  call prop_Q_A(ntry, wf16(:,21), Q(:,389), ZERO, 0_intkind1, wf16(:,22), n2(28))
  call vert_AV_Q(ntry, ex6, wf16(:,1), wf32(:,33), n3(:,59), t3x32(:,:,31))
  call prop_A_Q(ntry, wf32(:,33), Q(:,122), ZERO, 0_intkind1, wf32(:,34), n2(29))
  call vert_AZ_Q(gZu,ntry, ex6, wf16(:,2), wf32(:,35), n3(:,60), t3x32(:,:,32))
  call prop_A_Q(ntry, wf32(:,35), Q(:,122), ZERO, 0_intkind1, wf32(:,36), n2(30))
  call prop_Q_A(ntry, wf32(:,9), Q(:,213), ZERO, 0_intkind1, wf32(:,37), n2(31))
  call prop_Q_A(ntry, wf32(:,10), Q(:,213), ZERO, 0_intkind1, wf32(:,38), n2(32))
  call prop_A_Q(ntry, wf32(:,23), Q(:,117), ZERO, 0_intkind1, wf32(:,39), n2(33))
  call prop_A_Q(ntry, wf32(:,24), Q(:,117), ZERO, 0_intkind1, wf32(:,40), n2(34))
  call vert_WQ_A(ntry, wf4(:,6), ex7, wf8(:,17), n3(:,61), t3x8(:,:,9))
  call vert_AW_Q(ntry, wf4(:,13), wf4(:,7), wf16(:,23), n3(:,62), t3x16(:,:,13))
  call prop_Q_A(ntry, wf8(:,17), Q(:,69), ZERO, 0_intkind1, wf8(:,18), n2(35))
  call prop_A_Q(ntry, wf16(:,23), Q(:,282), ZERO, 0_intkind1, wf16(:,24), n2(36))
  call vert_WQ_A(ntry, wf4(:,14), wf8(:,18), wf32(:,41), n3(:,63), t3x32(:,:,33))
  call vert_UV_W(ntry, wf4(:,7), Q(:,10), wf4(:,14), Q(:,160), wf16(:,25), n3(:,64), t3x16(:,:,14))
  call vert_QA_V(ntry, wf8(:,18), wf4(:,13), wf32(:,42), n3(:,65), t3x32(:,:,34))
  call prop_W_W(ntry, wf16(:,25), Q(:,170), MZ, 1_intkind1, wf16(:,26), n2(37))
  call vert_QA_Z(gZu,ntry, wf8(:,18), wf4(:,13), wf32(:,43), n3(:,66), t3x32(:,:,35))
  call vert_WQ_A(ntry, wf4(:,7), ex7, wf8(:,19), n3(:,67), t3x8(:,:,10))
  call vert_AW_Q(ntry, wf4(:,13), wf4(:,6), wf16(:,27), n3(:,68), t3x16(:,:,15))
  call prop_Q_A(ntry, wf8(:,19), Q(:,74), ZERO, 0_intkind1, wf8(:,20), n2(38))
  call prop_A_Q(ntry, wf16(:,27), Q(:,277), ZERO, 0_intkind1, wf16(:,28), n2(39))
  call vert_WQ_A(ntry, wf4(:,14), wf8(:,20), wf32(:,44), n3(:,69), t3x32(:,:,36))
  call vert_UV_W(ntry, wf4(:,6), Q(:,5), wf4(:,14), Q(:,160), wf16(:,29), n3(:,70), t3x16(:,:,16))
  call vert_QA_V(ntry, wf8(:,20), wf4(:,13), wf32(:,45), n3(:,71), t3x32(:,:,37))
  call prop_W_W(ntry, wf16(:,29), Q(:,165), MZ, 1_intkind1, wf16(:,30), n2(40))
  call vert_QA_Z(gZu,ntry, wf8(:,20), wf4(:,13), wf32(:,46), n3(:,72), t3x32(:,:,38))
  call vert_VV_S(ntry, wf4(:,6), wf4(:,14), wf16(:,31), n3(:,73), t3x16(:,:,17))
  call vert_VV_S(ntry, wf4(:,7), wf8(:,6), wf32(:,47), n3(:,74), t3x32(:,:,39))
  call vert_UV_W(ntry, wf4(:,7), Q(:,10), wf8(:,6), Q(:,336), wf32(:,48), n3(:,75), t3x32(:,:,40))
  call vert_VV_S(ntry, wf4(:,7), wf4(:,14), wf16(:,32), n3(:,76), t3x16(:,:,18))
  call vert_VV_S(ntry, wf4(:,6), wf8(:,6), wf32(:,49), n3(:,77), t3x32(:,:,41))
  call vert_UV_W(ntry, wf4(:,6), Q(:,5), wf8(:,6), Q(:,336), wf32(:,50), n3(:,78), t3x32(:,:,42))
  call vert_QA_V(ntry, ex7, wf16(:,28), wf32(:,51), n3(:,79), t3x32(:,:,43))
  call vert_QA_Z(gZd,ntry, ex7, wf16(:,28), wf32(:,52), n3(:,80), t3x32(:,:,44))
  call vert_VQ_A(ntry, wf16(:,29), ex7, wf32(:,53), n3(:,81), t3x32(:,:,45))
  call vert_ZQ_A(gZd,ntry, wf16(:,30), ex7, wf32(:,54), n3(:,82), t3x32(:,:,46))
  call vert_AW_Q(ntry, ex5, wf4(:,6), wf8(:,21), n3(:,83), t3x8(:,:,11))
  call prop_A_Q(ntry, wf8(:,21), Q(:,21), ZERO, 0_intkind1, wf8(:,22), n2(41))
  call vert_QA_V(ntry, wf4(:,16), wf8(:,22), wf32(:,55), n3(:,84), t3x32(:,:,47))
  call vert_QA_Z(gZd,ntry, wf4(:,16), wf8(:,22), wf32(:,56), n3(:,85), t3x32(:,:,48))
  call vert_WQ_A(ntry, wf4(:,7), wf4(:,16), wf16(:,33), n3(:,86), t3x16(:,:,19))
  call prop_Q_A(ntry, wf16(:,33), Q(:,330), ZERO, 0_intkind1, wf16(:,34), n2(42))
  call vert_AW_Q(ntry, wf8(:,22), wf4(:,14), wf32(:,57), n3(:,87), t3x32(:,:,49))
  call vert_AW_Q(ntry, ex5, wf4(:,7), wf8(:,23), n3(:,88), t3x8(:,:,12))
  call prop_A_Q(ntry, wf8(:,23), Q(:,26), ZERO, 0_intkind1, wf8(:,24), n2(43))
  call vert_QA_V(ntry, wf4(:,16), wf8(:,24), wf32(:,58), n3(:,89), t3x32(:,:,50))
  call vert_QA_Z(gZd,ntry, wf4(:,16), wf8(:,24), wf32(:,59), n3(:,90), t3x32(:,:,51))
  call vert_WQ_A(ntry, wf4(:,6), wf4(:,16), wf16(:,35), n3(:,91), t3x16(:,:,20))
  call prop_Q_A(ntry, wf16(:,35), Q(:,325), ZERO, 0_intkind1, wf16(:,36), n2(44))
  call vert_AW_Q(ntry, wf8(:,24), wf4(:,14), wf32(:,60), n3(:,92), t3x32(:,:,52))
  call vert_VV_S(ntry, wf4(:,6), wf8(:,8), wf32(:,61), n3(:,93), t3x32(:,:,53))
  call vert_UV_W(ntry, wf4(:,6), Q(:,5), wf8(:,8), Q(:,336), wf32(:,62), n3(:,94), t3x32(:,:,54))
  call vert_VV_S(ntry, wf4(:,7), wf8(:,8), wf32(:,63), n3(:,95), t3x32(:,:,55))
  call vert_UV_W(ntry, wf4(:,7), Q(:,10), wf8(:,8), Q(:,336), wf32(:,64), n3(:,96), t3x32(:,:,56))
  call vert_AV_Q(ntry, ex5, wf16(:,29), wf32(:,65), n3(:,97), t3x32(:,:,57))
  call vert_AZ_Q(gZu,ntry, ex5, wf16(:,30), wf32(:,66), n3(:,98), t3x32(:,:,58))
  call vert_QA_V(ntry, wf16(:,36), ex5, wf32(:,67), n3(:,99), t3x32(:,:,59))
  call vert_QA_Z(gZu,ntry, wf16(:,36), ex5, wf32(:,68), n3(:,100), t3x32(:,:,60))
  call vert_AV_Q(ntry, wf8(:,22), ex9, wf16(:,37), n3(:,101), t3x16(:,:,21))
  call prop_A_Q(ntry, wf16(:,37), Q(:,277), ZERO, 0_intkind1, wf16(:,38), n2(45))
  call vert_VQ_A(ntry, ex9, wf8(:,20), wf16(:,39), n3(:,102), t3x16(:,:,22))
  call prop_Q_A(ntry, wf16(:,39), Q(:,330), ZERO, 0_intkind1, wf16(:,40), n2(46))
  call vert_VQ_A(ntry, wf16(:,25), ex7, wf32(:,69), n3(:,103), t3x32(:,:,61))
  call prop_Q_A(ntry, wf32(:,69), Q(:,234), ZERO, 0_intkind1, wf32(:,70), n2(47))
  call vert_ZQ_A(gZd,ntry, wf16(:,26), ex7, wf32(:,71), n3(:,104), t3x32(:,:,62))
  call prop_Q_A(ntry, wf32(:,71), Q(:,234), ZERO, 0_intkind1, wf32(:,72), n2(48))
  call vert_AV_Q(ntry, wf8(:,24), ex9, wf16(:,41), n3(:,105), t3x16(:,:,23))
  call prop_A_Q(ntry, wf16(:,41), Q(:,282), ZERO, 0_intkind1, wf16(:,42), n2(49))
  call vert_VQ_A(ntry, ex9, wf8(:,18), wf16(:,43), n3(:,106), t3x16(:,:,24))
  call prop_Q_A(ntry, wf16(:,43), Q(:,325), ZERO, 0_intkind1, wf16(:,44), n2(50))
  call vert_AV_Q(ntry, ex5, wf16(:,25), wf32(:,73), n3(:,107), t3x32(:,:,63))
  call prop_A_Q(ntry, wf32(:,73), Q(:,186), ZERO, 0_intkind1, wf32(:,74), n2(51))
  call vert_AZ_Q(gZu,ntry, ex5, wf16(:,26), wf32(:,75), n3(:,108), t3x32(:,:,64))
  call prop_A_Q(ntry, wf32(:,75), Q(:,186), ZERO, 0_intkind1, wf32(:,76), n2(52))
  call prop_Q_A(ntry, wf32(:,53), Q(:,229), ZERO, 0_intkind1, wf32(:,77), n2(53))
  call prop_Q_A(ntry, wf32(:,54), Q(:,229), ZERO, 0_intkind1, wf32(:,78), n2(54))
  call prop_A_Q(ntry, wf32(:,65), Q(:,181), ZERO, 0_intkind1, wf32(:,79), n2(55))
  call prop_A_Q(ntry, wf32(:,66), Q(:,181), ZERO, 0_intkind1, wf32(:,80), n2(56))
  call vert_QA_V(ntry, ex8, wf8(:,14), wf16(:,45), n3(:,109), t3x16(:,:,25))
  call vert_QA_Z(gZd,ntry, ex8, wf8(:,14), wf16(:,46), n3(:,110), t3x16(:,:,26))
  call prop_W_W(ntry, wf16(:,46), Q(:,165), MZ, 1_intkind1, wf16(:,47), n2(57))
  call vert_AW_Q(ntry, wf8(:,14), wf8(:,6), wf64(:,3), n3(:,111), t3x64(:,:,1))
  call vert_QA_V(ntry, ex7, wf16(:,24), wf32(:,81), n3(:,112), t3x32(:,:,65))
  call vert_QA_Z(gZd,ntry, ex7, wf16(:,24), wf32(:,82), n3(:,113), t3x32(:,:,66))
  call prop_W_W(ntry, wf32(:,82), Q(:,346), MZ, 1_intkind1, wf32(:,83), n2(58))
  call vert_QA_V(ntry, ex8, wf8(:,16), wf16(:,48), n3(:,114), t3x16(:,:,27))
  call vert_QA_Z(gZd,ntry, ex8, wf8(:,16), wf16(:,49), n3(:,115), t3x16(:,:,28))
  call prop_W_W(ntry, wf16(:,49), Q(:,170), MZ, 1_intkind1, wf16(:,50), n2(59))
  call vert_QA_V(ntry, wf8(:,12), ex6, wf16(:,51), n3(:,116), t3x16(:,:,29))
  call vert_QA_Z(gZu,ntry, wf8(:,12), ex6, wf16(:,52), n3(:,117), t3x16(:,:,30))
  call prop_W_W(ntry, wf16(:,52), Q(:,170), MZ, 1_intkind1, wf16(:,53), n2(60))
  call vert_AW_Q(ntry, wf8(:,16), wf8(:,6), wf64(:,4), n3(:,118), t3x64(:,:,2))
  call vert_QA_V(ntry, wf8(:,10), ex6, wf16(:,54), n3(:,119), t3x16(:,:,31))
  call vert_QA_Z(gZu,ntry, wf8(:,10), ex6, wf16(:,55), n3(:,120), t3x16(:,:,32))
  call prop_W_W(ntry, wf16(:,55), Q(:,165), MZ, 1_intkind1, wf16(:,56), n2(61))
  call prop_W_W(ntry, wf32(:,52), Q(:,341), MZ, 1_intkind1, wf32(:,84), n2(62))
  call vert_QA_W(ntry, wf8(:,20), wf8(:,22), wf64(:,5), n3(:,121), t3x64(:,:,3))
  call vert_QA_V(ntry, ex7, wf8(:,22), wf16(:,57), n3(:,122), t3x16(:,:,33))
  call vert_QA_Z(gZd,ntry, ex7, wf8(:,22), wf16(:,58), n3(:,123), t3x16(:,:,34))
  call prop_W_W(ntry, wf16(:,58), Q(:,85), MZ, 1_intkind1, wf16(:,59), n2(63))
  call vert_QA_V(ntry, ex8, wf16(:,4), wf32(:,85), n3(:,124), t3x32(:,:,67))
  call vert_QA_Z(gZd,ntry, ex8, wf16(:,4), wf32(:,86), n3(:,125), t3x32(:,:,68))
  call vert_QA_W(ntry, wf8(:,18), wf8(:,24), wf64(:,6), n3(:,126), t3x64(:,:,4))
  call vert_QA_V(ntry, wf8(:,18), ex5, wf16(:,60), n3(:,127), t3x16(:,:,35))
  call vert_QA_Z(gZu,ntry, wf8(:,18), ex5, wf16(:,61), n3(:,128), t3x16(:,:,36))
  call prop_W_W(ntry, wf16(:,61), Q(:,85), MZ, 1_intkind1, wf16(:,62), n2(64))
  call vert_QA_V(ntry, ex7, wf8(:,24), wf16(:,63), n3(:,129), t3x16(:,:,37))
  call vert_QA_Z(gZd,ntry, ex7, wf8(:,24), wf16(:,64), n3(:,130), t3x16(:,:,38))
  call prop_W_W(ntry, wf16(:,64), Q(:,90), MZ, 1_intkind1, wf16(:,65), n2(65))
  call vert_QA_V(ntry, wf8(:,20), ex5, wf16(:,66), n3(:,131), t3x16(:,:,39))
  call vert_QA_Z(gZu,ntry, wf8(:,20), ex5, wf16(:,67), n3(:,132), t3x16(:,:,40))
  call prop_W_W(ntry, wf16(:,67), Q(:,90), MZ, 1_intkind1, wf16(:,68), n2(66))
  call vert_AW_Q(ntry, wf8(:,14), wf8(:,8), wf64(:,7), n3(:,133), t3x64(:,:,5))
  call vert_QA_V(ntry, wf16(:,34), ex5, wf32(:,87), n3(:,134), t3x32(:,:,69))
  call vert_QA_Z(gZu,ntry, wf16(:,34), ex5, wf32(:,88), n3(:,135), t3x32(:,:,70))
  call prop_W_W(ntry, wf32(:,88), Q(:,346), MZ, 1_intkind1, wf32(:,89), n2(67))
  call vert_AW_Q(ntry, wf8(:,16), wf8(:,8), wf64(:,8), n3(:,136), t3x64(:,:,6))
  call prop_W_W(ntry, wf32(:,68), Q(:,341), MZ, 1_intkind1, wf32(:,90), n2(68))
  call vert_AW_Q(ntry, wf8(:,22), wf8(:,4), wf64(:,9), n3(:,137), t3x64(:,:,7))
  call vert_QA_V(ntry, wf16(:,12), ex6, wf32(:,91), n3(:,138), t3x32(:,:,71))
  call vert_QA_Z(gZu,ntry, wf16(:,12), ex6, wf32(:,92), n3(:,139), t3x32(:,:,72))
  call prop_W_W(ntry, wf32(:,92), Q(:,426), MZ, 1_intkind1, wf32(:,93), n2(69))
  call vert_AW_Q(ntry, wf8(:,24), wf8(:,4), wf64(:,10), n3(:,140), t3x64(:,:,8))
  call prop_W_W(ntry, wf32(:,28), Q(:,421), MZ, 1_intkind1, wf32(:,94), n2(70))
  call vert_VQ_A(ntry, wf16(:,57), ex8, wf32(:,95), n3(:,141), t3x32(:,:,73))
  call vert_ZQ_A(gZd,ntry, wf16(:,59), ex8, wf32(:,96), n3(:,142), t3x32(:,:,74))
  call vert_VQ_A(ntry, wf16(:,48), ex7, wf32(:,97), n3(:,143), t3x32(:,:,75))
  call vert_ZQ_A(gZd,ntry, wf16(:,50), ex7, wf32(:,98), n3(:,144), t3x32(:,:,76))
  call vert_AV_Q(ntry, ex6, wf16(:,57), wf32(:,99), n3(:,145), t3x32(:,:,77))
  call vert_AZ_Q(gZu,ntry, ex6, wf16(:,59), wf32(:,100), n3(:,146), t3x32(:,:,78))
  call vert_VQ_A(ntry, wf16(:,51), ex7, wf32(:,101), n3(:,147), t3x32(:,:,79))
  call vert_ZQ_A(gZd,ntry, wf16(:,53), ex7, wf32(:,102), n3(:,148), t3x32(:,:,80))
  call vert_VQ_A(ntry, wf16(:,63), ex8, wf32(:,103), n3(:,149), t3x32(:,:,81))
  call vert_ZQ_A(gZd,ntry, wf16(:,65), ex8, wf32(:,104), n3(:,150), t3x32(:,:,82))
  call vert_VQ_A(ntry, wf16(:,45), ex7, wf32(:,105), n3(:,151), t3x32(:,:,83))
  call vert_ZQ_A(gZd,ntry, wf16(:,47), ex7, wf32(:,106), n3(:,152), t3x32(:,:,84))
  call vert_VQ_A(ntry, wf16(:,66), ex8, wf32(:,107), n3(:,153), t3x32(:,:,85))
  call vert_ZQ_A(gZd,ntry, wf16(:,68), ex8, wf32(:,108), n3(:,154), t3x32(:,:,86))
  call vert_AV_Q(ntry, ex5, wf16(:,45), wf32(:,109), n3(:,155), t3x32(:,:,87))
  call vert_AZ_Q(gZu,ntry, ex5, wf16(:,47), wf32(:,110), n3(:,156), t3x32(:,:,88))
  call vert_VQ_A(ntry, wf16(:,60), ex8, wf32(:,111), n3(:,157), t3x32(:,:,89))
  call vert_ZQ_A(gZd,ntry, wf16(:,62), ex8, wf32(:,112), n3(:,158), t3x32(:,:,90))
  call vert_AV_Q(ntry, ex5, wf16(:,48), wf32(:,113), n3(:,159), t3x32(:,:,91))
  call vert_AZ_Q(gZu,ntry, ex5, wf16(:,50), wf32(:,114), n3(:,160), t3x32(:,:,92))
  call vert_AV_Q(ntry, ex6, wf16(:,60), wf32(:,115), n3(:,161), t3x32(:,:,93))
  call vert_AZ_Q(gZu,ntry, ex6, wf16(:,62), wf32(:,116), n3(:,162), t3x32(:,:,94))
  call vert_AV_Q(ntry, ex5, wf16(:,51), wf32(:,117), n3(:,163), t3x32(:,:,95))
  call vert_AZ_Q(gZu,ntry, ex5, wf16(:,53), wf32(:,118), n3(:,164), t3x32(:,:,96))
  call vert_AV_Q(ntry, ex6, wf16(:,63), wf32(:,119), n3(:,165), t3x32(:,:,97))
  call vert_AZ_Q(gZu,ntry, ex6, wf16(:,65), wf32(:,120), n3(:,166), t3x32(:,:,98))
  call vert_VQ_A(ntry, wf16(:,54), ex7, wf32(:,121), n3(:,167), t3x32(:,:,99))
  call vert_ZQ_A(gZd,ntry, wf16(:,56), ex7, wf32(:,122), n3(:,168), t3x32(:,:,100))
  call vert_AV_Q(ntry, ex6, wf16(:,66), wf32(:,123), n3(:,169), t3x32(:,:,101))
  call vert_AZ_Q(gZu,ntry, ex6, wf16(:,68), wf32(:,124), n3(:,170), t3x32(:,:,102))
  call vert_AV_Q(ntry, ex5, wf16(:,54), wf32(:,125), n3(:,171), t3x32(:,:,103))
  call vert_AZ_Q(gZu,ntry, ex5, wf16(:,56), wf32(:,126), n3(:,172), t3x32(:,:,104))
  call vert_WQ_A(ntry, wf4(:,8), ex2, wf8(:,25), n3(:,173), t3x8(:,:,13))
  call prop_Q_A(ntry, wf8(:,25), Q(:,82), ZERO, 0_intkind1, wf8(:,26), n2(71))
  call vert_QA_V(ntry, wf8(:,26), ex4, wf16(:,69), n3(:,174), t3x16(:,:,41))
  call vert_QA_Z(gZl,ntry, wf8(:,26), ex4, wf16(:,70), n3(:,175), t3x16(:,:,42))
  call prop_W_W(ntry, wf16(:,70), Q(:,90), MZ, 1_intkind1, wf16(:,71), n2(72))
  call vert_AW_Q(ntry, ex4, wf4(:,8), wf8(:,27), n3(:,176), t3x8(:,:,14))
  call prop_A_Q(ntry, wf8(:,27), Q(:,88), ZERO, 0_intkind1, wf8(:,28), n2(73))
  call vert_QA_Z(gZn,ntry, ex2, wf8(:,28), wf16(:,72), n3(:,177), t3x16(:,:,43))
  call prop_W_W(ntry, wf16(:,72), Q(:,90), MZ, 1_intkind1, wf16(:,73), n2(74))
  call vert_WQ_A(ntry, wf8(:,2), ex2, wf16(:,74), n3(:,178), t3x16(:,:,44))
  call vert_AV_Q(ntry, ex4, wf16(:,5), wf32(:,127), n3(:,179), t3x32(:,:,105))
  call prop_Q_A(ntry, wf16(:,74), Q(:,418), ZERO, 0_intkind1, wf16(:,75), n2(75))
  call vert_AZ_Q(gZl,ntry, ex4, wf16(:,6), wf32(:,128), n3(:,180), t3x32(:,:,106))
  call vert_ZQ_A(gZn,ntry, wf16(:,6), ex2, wf32(:,129), n3(:,181), t3x32(:,:,107))
  call vert_AW_Q(ntry, ex4, wf8(:,2), wf16(:,76), n3(:,182), t3x16(:,:,45))
  call prop_Q_A(ntry, wf32(:,129), Q(:,87), ZERO, 0_intkind1, wf32(:,130), n2(76))
  call vert_WQ_A(ntry, wf4(:,6), wf8(:,26), wf32(:,131), n3(:,183), t3x32(:,:,108))
  call prop_A_Q(ntry, wf16(:,76), Q(:,424), ZERO, 0_intkind1, wf16(:,77), n2(77))
  call vert_AW_Q(ntry, wf8(:,28), wf4(:,6), wf32(:,132), n3(:,184), t3x32(:,:,109))
  call vert_WQ_A(ntry, wf8(:,4), ex2, wf16(:,78), n3(:,185), t3x16(:,:,46))
  call prop_Q_A(ntry, wf16(:,78), Q(:,418), ZERO, 0_intkind1, wf16(:,79), n2(78))
  call vert_AW_Q(ntry, ex4, wf8(:,4), wf16(:,80), n3(:,186), t3x16(:,:,47))
  call prop_A_Q(ntry, wf16(:,80), Q(:,424), ZERO, 0_intkind1, wf16(:,81), n2(79))
  call vert_VQ_A(ntry, wf16(:,69), ex8, wf32(:,133), n3(:,187), t3x32(:,:,110))
  call vert_ZQ_A(gZd,ntry, wf16(:,71), ex8, wf32(:,134), n3(:,188), t3x32(:,:,111))
  call vert_ZQ_A(gZd,ntry, wf16(:,73), ex8, wf32(:,135), n3(:,189), t3x32(:,:,112))
  call vert_AV_Q(ntry, ex6, wf16(:,69), wf32(:,136), n3(:,190), t3x32(:,:,113))
  call vert_AZ_Q(gZu,ntry, ex6, wf16(:,71), wf32(:,137), n3(:,191), t3x32(:,:,114))
  call vert_AZ_Q(gZu,ntry, ex6, wf16(:,73), wf32(:,138), n3(:,192), t3x32(:,:,115))
  call vert_WQ_A(ntry, wf4(:,14), ex2, wf8(:,29), n3(:,193), t3x8(:,:,15))
  call prop_Q_A(ntry, wf8(:,29), Q(:,162), ZERO, 0_intkind1, wf8(:,30), n2(80))
  call vert_QA_V(ntry, wf8(:,30), ex4, wf16(:,82), n3(:,194), t3x16(:,:,48))
  call vert_QA_Z(gZl,ntry, wf8(:,30), ex4, wf16(:,83), n3(:,195), t3x16(:,:,49))
  call prop_W_W(ntry, wf16(:,83), Q(:,170), MZ, 1_intkind1, wf16(:,84), n2(81))
  call vert_AW_Q(ntry, ex4, wf4(:,14), wf8(:,31), n3(:,196), t3x8(:,:,16))
  call prop_A_Q(ntry, wf8(:,31), Q(:,168), ZERO, 0_intkind1, wf8(:,32), n2(82))
  call vert_QA_Z(gZn,ntry, ex2, wf8(:,32), wf16(:,85), n3(:,197), t3x16(:,:,50))
  call prop_W_W(ntry, wf16(:,85), Q(:,170), MZ, 1_intkind1, wf16(:,86), n2(83))
  call vert_WQ_A(ntry, wf8(:,6), ex2, wf16(:,87), n3(:,198), t3x16(:,:,51))
  call vert_AV_Q(ntry, ex4, wf16(:,29), wf32(:,139), n3(:,199), t3x32(:,:,116))
  call prop_Q_A(ntry, wf16(:,87), Q(:,338), ZERO, 0_intkind1, wf16(:,88), n2(84))
  call vert_AZ_Q(gZl,ntry, ex4, wf16(:,30), wf32(:,140), n3(:,200), t3x32(:,:,117))
  call vert_ZQ_A(gZn,ntry, wf16(:,30), ex2, wf32(:,141), n3(:,201), t3x32(:,:,118))
  call vert_AW_Q(ntry, ex4, wf8(:,6), wf16(:,89), n3(:,202), t3x16(:,:,52))
  call prop_Q_A(ntry, wf32(:,141), Q(:,167), ZERO, 0_intkind1, wf32(:,142), n2(85))
  call vert_WQ_A(ntry, wf4(:,6), wf8(:,30), wf32(:,143), n3(:,203), t3x32(:,:,119))
  call prop_A_Q(ntry, wf16(:,89), Q(:,344), ZERO, 0_intkind1, wf16(:,90), n2(86))
  call vert_AW_Q(ntry, wf8(:,32), wf4(:,6), wf32(:,144), n3(:,204), t3x32(:,:,120))
  call vert_WQ_A(ntry, wf8(:,8), ex2, wf16(:,91), n3(:,205), t3x16(:,:,53))
  call prop_Q_A(ntry, wf16(:,91), Q(:,338), ZERO, 0_intkind1, wf16(:,92), n2(87))
  call vert_AW_Q(ntry, ex4, wf8(:,8), wf16(:,93), n3(:,206), t3x16(:,:,54))
  call prop_A_Q(ntry, wf16(:,93), Q(:,344), ZERO, 0_intkind1, wf16(:,94), n2(88))
  call vert_VQ_A(ntry, wf16(:,82), ex7, wf32(:,145), n3(:,207), t3x32(:,:,121))
  call vert_ZQ_A(gZd,ntry, wf16(:,84), ex7, wf32(:,146), n3(:,208), t3x32(:,:,122))
  call vert_ZQ_A(gZd,ntry, wf16(:,86), ex7, wf32(:,147), n3(:,209), t3x32(:,:,123))
  call vert_AZ_Q(gZu,ntry, ex5, wf16(:,86), wf32(:,148), n3(:,210), t3x32(:,:,124))
  call vert_AV_Q(ntry, ex5, wf16(:,82), wf32(:,149), n3(:,211), t3x32(:,:,125))
  call vert_AZ_Q(gZu,ntry, ex5, wf16(:,84), wf32(:,150), n3(:,212), t3x32(:,:,126))
  call vert_QA_V(ntry, wf16(:,88), ex4, wf32(:,151), n3(:,213), t3x32(:,:,127))
  call vert_QA_Z(gZl,ntry, wf16(:,88), ex4, wf32(:,152), n3(:,214), t3x32(:,:,128))
  call vert_QA_Z(gZn,ntry, ex2, wf16(:,90), wf32(:,153), n3(:,215), t3x32(:,:,129))
  call vert_QA_Z(gZn,ntry, ex2, wf16(:,77), wf32(:,154), n3(:,216), t3x32(:,:,130))
  call vert_QA_V(ntry, wf16(:,75), ex4, wf32(:,155), n3(:,217), t3x32(:,:,131))
  call vert_QA_Z(gZl,ntry, wf16(:,75), ex4, wf32(:,156), n3(:,218), t3x32(:,:,132))
  call vert_QA_V(ntry, wf16(:,92), ex4, wf32(:,157), n3(:,219), t3x32(:,:,133))
  call vert_QA_Z(gZl,ntry, wf16(:,92), ex4, wf32(:,158), n3(:,220), t3x32(:,:,134))
  call vert_QA_Z(gZn,ntry, ex2, wf16(:,94), wf32(:,159), n3(:,221), t3x32(:,:,135))
  call vert_QA_V(ntry, wf16(:,79), ex4, wf32(:,160), n3(:,222), t3x32(:,:,136))
  call vert_QA_Z(gZl,ntry, wf16(:,79), ex4, wf32(:,161), n3(:,223), t3x32(:,:,137))
  call vert_QA_Z(gZn,ntry, ex2, wf16(:,81), wf32(:,162), n3(:,224), t3x32(:,:,138))
  call vert_WQ_A(ntry, wf4(:,8), ex1, wf8(:,33), n3(:,225), t3x8(:,:,17))
  call prop_Q_A(ntry, wf8(:,33), Q(:,81), ZERO, 0_intkind1, wf8(:,34), n2(89))
  call vert_QA_V(ntry, wf8(:,34), ex3, wf16(:,95), n3(:,226), t3x16(:,:,55))
  call vert_QA_Z(gZl,ntry, wf8(:,34), ex3, wf16(:,96), n3(:,227), t3x16(:,:,56))
  call prop_W_W(ntry, wf16(:,96), Q(:,85), MZ, 1_intkind1, wf16(:,97), n2(90))
  call vert_AW_Q(ntry, ex3, wf4(:,8), wf8(:,35), n3(:,228), t3x8(:,:,18))
  call prop_A_Q(ntry, wf8(:,35), Q(:,84), ZERO, 0_intkind1, wf8(:,36), n2(91))
  call vert_QA_Z(gZn,ntry, ex1, wf8(:,36), wf16(:,98), n3(:,229), t3x16(:,:,57))
  call prop_W_W(ntry, wf16(:,98), Q(:,85), MZ, 1_intkind1, wf16(:,99), n2(92))
  call vert_WQ_A(ntry, wf8(:,2), ex1, wf16(:,100), n3(:,230), t3x16(:,:,58))
  call vert_AV_Q(ntry, ex3, wf16(:,1), wf32(:,163), n3(:,231), t3x32(:,:,139))
  call prop_Q_A(ntry, wf16(:,100), Q(:,417), ZERO, 0_intkind1, wf16(:,101), n2(93))
  call vert_AZ_Q(gZl,ntry, ex3, wf16(:,2), wf32(:,164), n3(:,232), t3x32(:,:,140))
  call vert_ZQ_A(gZn,ntry, wf16(:,2), ex1, wf32(:,165), n3(:,233), t3x32(:,:,141))
  call vert_AW_Q(ntry, ex3, wf8(:,2), wf16(:,102), n3(:,234), t3x16(:,:,59))
  call prop_Q_A(ntry, wf32(:,165), Q(:,91), ZERO, 0_intkind1, wf32(:,166), n2(94))
  call vert_WQ_A(ntry, wf4(:,7), wf8(:,34), wf32(:,167), n3(:,235), t3x32(:,:,142))
  call prop_A_Q(ntry, wf16(:,102), Q(:,420), ZERO, 0_intkind1, wf16(:,103), n2(95))
  call vert_AW_Q(ntry, wf8(:,36), wf4(:,7), wf32(:,168), n3(:,236), t3x32(:,:,143))
  call vert_WQ_A(ntry, wf8(:,4), ex1, wf16(:,104), n3(:,237), t3x16(:,:,60))
  call prop_Q_A(ntry, wf16(:,104), Q(:,417), ZERO, 0_intkind1, wf16(:,105), n2(96))
  call vert_AW_Q(ntry, ex3, wf8(:,4), wf16(:,106), n3(:,238), t3x16(:,:,61))
  call prop_A_Q(ntry, wf16(:,106), Q(:,420), ZERO, 0_intkind1, wf16(:,107), n2(97))
  call vert_VQ_A(ntry, wf16(:,95), ex8, wf32(:,169), n3(:,239), t3x32(:,:,144))
  call vert_ZQ_A(gZd,ntry, wf16(:,97), ex8, wf32(:,170), n3(:,240), t3x32(:,:,145))
  call vert_AV_Q(ntry, ex6, wf16(:,95), wf32(:,171), n3(:,241), t3x32(:,:,146))
  call vert_AZ_Q(gZu,ntry, ex6, wf16(:,97), wf32(:,172), n3(:,242), t3x32(:,:,147))
  call vert_AZ_Q(gZu,ntry, ex6, wf16(:,99), wf32(:,173), n3(:,243), t3x32(:,:,148))
  call vert_ZQ_A(gZd,ntry, wf16(:,99), ex8, wf32(:,174), n3(:,244), t3x32(:,:,149))
  call vert_WQ_A(ntry, wf4(:,14), ex1, wf8(:,37), n3(:,245), t3x8(:,:,19))
  call prop_Q_A(ntry, wf8(:,37), Q(:,161), ZERO, 0_intkind1, wf8(:,38), n2(98))
  call vert_QA_V(ntry, wf8(:,38), ex3, wf16(:,108), n3(:,246), t3x16(:,:,62))
  call vert_QA_Z(gZl,ntry, wf8(:,38), ex3, wf16(:,109), n3(:,247), t3x16(:,:,63))
  call prop_W_W(ntry, wf16(:,109), Q(:,165), MZ, 1_intkind1, wf16(:,110), n2(99))
  call vert_AW_Q(ntry, ex3, wf4(:,14), wf8(:,39), n3(:,248), t3x8(:,:,20))
  call prop_A_Q(ntry, wf8(:,39), Q(:,164), ZERO, 0_intkind1, wf8(:,40), n2(100))
  call vert_QA_Z(gZn,ntry, ex1, wf8(:,40), wf16(:,111), n3(:,249), t3x16(:,:,64))
  call prop_W_W(ntry, wf16(:,111), Q(:,165), MZ, 1_intkind1, wf16(:,112), n2(101))
  call vert_WQ_A(ntry, wf8(:,6), ex1, wf16(:,113), n3(:,250), t3x16(:,:,65))
  call vert_AV_Q(ntry, ex3, wf16(:,25), wf32(:,175), n3(:,251), t3x32(:,:,150))
  call prop_Q_A(ntry, wf16(:,113), Q(:,337), ZERO, 0_intkind1, wf16(:,114), n2(102))
  call vert_AZ_Q(gZl,ntry, ex3, wf16(:,26), wf32(:,176), n3(:,252), t3x32(:,:,151))
  call vert_ZQ_A(gZn,ntry, wf16(:,26), ex1, wf32(:,177), n3(:,253), t3x32(:,:,152))
  call vert_AW_Q(ntry, ex3, wf8(:,6), wf16(:,115), n3(:,254), t3x16(:,:,66))
  call prop_Q_A(ntry, wf32(:,177), Q(:,171), ZERO, 0_intkind1, wf32(:,178), n2(103))
  call vert_WQ_A(ntry, wf4(:,7), wf8(:,38), wf32(:,179), n3(:,255), t3x32(:,:,153))
  call prop_A_Q(ntry, wf16(:,115), Q(:,340), ZERO, 0_intkind1, wf16(:,116), n2(104))
  call vert_AW_Q(ntry, wf8(:,40), wf4(:,7), wf32(:,180), n3(:,256), t3x32(:,:,154))
  call vert_WQ_A(ntry, wf8(:,8), ex1, wf16(:,117), n3(:,257), t3x16(:,:,67))
  call prop_Q_A(ntry, wf16(:,117), Q(:,337), ZERO, 0_intkind1, wf16(:,118), n2(105))
  call vert_AW_Q(ntry, ex3, wf8(:,8), wf16(:,119), n3(:,258), t3x16(:,:,68))
  call prop_A_Q(ntry, wf16(:,119), Q(:,340), ZERO, 0_intkind1, wf16(:,120), n2(106))
  call vert_AZ_Q(gZu,ntry, ex5, wf16(:,112), wf32(:,181), n3(:,259), t3x32(:,:,155))
  call vert_VQ_A(ntry, wf16(:,108), ex7, wf32(:,182), n3(:,260), t3x32(:,:,156))
  call vert_ZQ_A(gZd,ntry, wf16(:,110), ex7, wf32(:,183), n3(:,261), t3x32(:,:,157))
  call vert_AV_Q(ntry, ex5, wf16(:,108), wf32(:,184), n3(:,262), t3x32(:,:,158))
  call vert_AZ_Q(gZu,ntry, ex5, wf16(:,110), wf32(:,185), n3(:,263), t3x32(:,:,159))
  call vert_ZQ_A(gZd,ntry, wf16(:,112), ex7, wf32(:,186), n3(:,264), t3x32(:,:,160))
  call vert_QA_Z(gZn,ntry, ex1, wf16(:,116), wf32(:,187), n3(:,265), t3x32(:,:,161))
  call vert_QA_V(ntry, wf16(:,114), ex3, wf32(:,188), n3(:,266), t3x32(:,:,162))
  call vert_QA_Z(gZl,ntry, wf16(:,114), ex3, wf32(:,189), n3(:,267), t3x32(:,:,163))
  call vert_QA_Z(gZn,ntry, ex1, wf16(:,103), wf32(:,190), n3(:,268), t3x32(:,:,164))
  call vert_QA_V(ntry, wf16(:,101), ex3, wf32(:,191), n3(:,269), t3x32(:,:,165))
  call vert_QA_Z(gZl,ntry, wf16(:,101), ex3, wf32(:,192), n3(:,270), t3x32(:,:,166))
  call vert_QA_V(ntry, wf16(:,118), ex3, wf32(:,193), n3(:,271), t3x32(:,:,167))
  call vert_QA_Z(gZl,ntry, wf16(:,118), ex3, wf32(:,194), n3(:,272), t3x32(:,:,168))
  call vert_QA_Z(gZn,ntry, ex1, wf16(:,120), wf32(:,195), n3(:,273), t3x32(:,:,169))
  call vert_QA_Z(gZn,ntry, ex1, wf16(:,107), wf32(:,196), n3(:,274), t3x32(:,:,170))
  call vert_QA_V(ntry, wf16(:,105), ex3, wf32(:,197), n3(:,275), t3x32(:,:,171))
  call vert_QA_Z(gZl,ntry, wf16(:,105), ex3, wf32(:,198), n3(:,276), t3x32(:,:,172))
  call vert_ZQ_A(gZn,ntry, wf16(:,99), ex2, wf32(:,199), n3(:,277), t3x32(:,:,173))
  call vert_ZQ_A(gZn,ntry, wf16(:,73), ex1, wf32(:,200), n3(:,278), t3x32(:,:,174))
  call vert_AZ_Q(gZl,ntry, ex4, wf16(:,99), wf32(:,201), n3(:,279), t3x32(:,:,175))
  call vert_ZQ_A(gZn,ntry, wf16(:,97), ex2, wf32(:,202), n3(:,280), t3x32(:,:,176))
  call vert_ZQ_A(gZn,ntry, wf16(:,112), ex2, wf32(:,203), n3(:,281), t3x32(:,:,177))
  call vert_ZQ_A(gZn,ntry, wf16(:,86), ex1, wf32(:,204), n3(:,282), t3x32(:,:,178))
  call vert_AZ_Q(gZl,ntry, ex4, wf16(:,112), wf32(:,205), n3(:,283), t3x32(:,:,179))
  call vert_ZQ_A(gZn,ntry, wf16(:,110), ex2, wf32(:,206), n3(:,284), t3x32(:,:,180))


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
      call colint2(M1helarray(:,k), M1, M2add, extcombs_permuted)
      M2 = M2 + M2add
    end do
    M2 = 0.5_/**/REALKIND * M2

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

    M2munu = M2munu / average_factor_ppllnnjj_vbs_nenmexmxuxcxdsg_2
  end if

  ! Colour and helicity average and symmetry factor of outgoing particles
  M2add = M2 / average_factor_ppllnnjj_vbs_nenmexmxuxcxdsg_2

  do k = 0, 47-1
    M2(k) = M2add(extcomb_perm_ppllnnjj_vbs_nenmexmxuxcxdsg_2(k))
  end do

  if (ntry == 1) ntry = 2

  if (scalefactor /= old_scalefactor) then
    scalebackfactor = scalefactor**(2*9-8)
    old_scalefactor = scalefactor
  end if
  M2 = scalebackfactor * M2

  contains

subroutine diagrams()
  implicit none
  ! e.g. call cont_VV(nsync, wf3, wf6, A(:,1), n64, t64, nhel, den(5)) ...

    call cont_VV(nsync, wf64(:,1), wf8(:,2), A(:,1), n3(:,285), t3x512(:,:,1), nhel, den(8))
    call cont_VV(nsync, wf64(:,1), wf8(:,4), A(:,2), n3(:,286), t3x512(:,:,2), nhel, den(11))
    call cont_VV(nsync, wf64(:,2), wf8(:,6), A(:,3), n3(:,287), t3x512(:,:,3), nhel, den(17))
    call cont_VV(nsync, wf64(:,2), wf8(:,8), A(:,4), n3(:,288), t3x512(:,:,4), nhel, den(20))
    call cont_VV(nsync, wf16(:,1), wf32(:,1), A(:,5), n3(:,289), t3x512(:,:,5), nhel, den(27))
    call cont_VV(nsync, wf16(:,2), wf32(:,2), A(:,6), n3(:,290), t3x512(:,:,6), nhel, den(30))
    call cont_QA(nsync, wf16(:,4), wf32(:,3), A(:,7), n3(:,291), t3x512(:,:,7), nhel, den(35))
    call cont_VV(nsync, wf16(:,5), wf32(:,4), A(:,8), n3(:,292), t3x512(:,:,8), nhel, den(42))
    call cont_VV(nsync, wf16(:,6), wf32(:,5), A(:,9), n3(:,293), t3x512(:,:,9), nhel, den(45))
    call cont_QA(nsync, wf16(:,8), wf32(:,6), A(:,10), n3(:,294), t3x512(:,:,10), nhel, den(50))
    call cont_SS(nsync, wf16(:,9), wf32(:,7), A(:,11), n3(:,295), t3x512(:,:,11), nhel, den(54))
    call cont_VV(nsync, wf16(:,5), wf32(:,8), A(:,12), n3(:,296), t3x512(:,:,12), nhel, den(55))
    call cont_VV(nsync, wf16(:,6), wf32(:,8), A(:,13), n3(:,297), t3x512(:,:,13), nhel, den(56))
    call cont_QA(nsync, wf16(:,4), wf32(:,9), A(:,14), n3(:,298), t3x512(:,:,14), nhel, den(57))
    call cont_QA(nsync, wf16(:,4), wf32(:,10), A(:,15), n3(:,299), t3x512(:,:,15), nhel, den(58))
    call cont_SS(nsync, wf16(:,10), wf32(:,11), A(:,16), n3(:,300), t3x512(:,:,16), nhel, den(62))
    call cont_VV(nsync, wf16(:,1), wf32(:,12), A(:,17), n3(:,301), t3x512(:,:,17), nhel, den(63))
    call cont_VV(nsync, wf16(:,2), wf32(:,12), A(:,18), n3(:,302), t3x512(:,:,18), nhel, den(64))
    call cont_VV(nsync, wf16(:,1), wf32(:,13), A(:,19), n3(:,303), t3x512(:,:,19), nhel, den(65))
    call cont_VV(nsync, wf16(:,2), wf32(:,14), A(:,20), n3(:,304), t3x512(:,:,20), nhel, den(66))
    call cont_VV(nsync, wf16(:,1), wf32(:,15), A(:,21), n3(:,305), t3x512(:,:,21), nhel, den(70))
    call cont_VV(nsync, wf16(:,2), wf32(:,16), A(:,22), n3(:,306), t3x512(:,:,22), nhel, den(71))
    call cont_QA(nsync, wf16(:,12), wf32(:,17), A(:,23), n3(:,307), t3x512(:,:,23), nhel, den(76))
    call cont_VV(nsync, wf16(:,5), wf32(:,18), A(:,24), n3(:,308), t3x512(:,:,24), nhel, den(80))
    call cont_VV(nsync, wf16(:,6), wf32(:,19), A(:,25), n3(:,309), t3x512(:,:,25), nhel, den(81))
    call cont_QA(nsync, wf16(:,14), wf32(:,20), A(:,26), n3(:,310), t3x512(:,:,26), nhel, den(86))
    call cont_SS(nsync, wf16(:,9), wf32(:,21), A(:,27), n3(:,311), t3x512(:,:,27), nhel, den(88))
    call cont_VV(nsync, wf16(:,5), wf32(:,22), A(:,28), n3(:,312), t3x512(:,:,28), nhel, den(89))
    call cont_VV(nsync, wf16(:,6), wf32(:,22), A(:,29), n3(:,313), t3x512(:,:,29), nhel, den(90))
    call cont_QA(nsync, wf16(:,12), wf32(:,23), A(:,30), n3(:,314), t3x512(:,:,30), nhel, den(91))
    call cont_QA(nsync, wf16(:,12), wf32(:,24), A(:,31), n3(:,315), t3x512(:,:,31), nhel, den(92))
    call cont_SS(nsync, wf16(:,10), wf32(:,25), A(:,32), n3(:,316), t3x512(:,:,32), nhel, den(94))
    call cont_VV(nsync, wf16(:,1), wf32(:,26), A(:,33), n3(:,317), t3x512(:,:,33), nhel, den(95))
    call cont_VV(nsync, wf16(:,2), wf32(:,26), A(:,34), n3(:,318), t3x512(:,:,34), nhel, den(96))
    call cont_VV(nsync, wf16(:,1), wf32(:,27), A(:,35), n3(:,319), t3x512(:,:,35), nhel, den(97))
    call cont_VV(nsync, wf16(:,2), wf32(:,28), A(:,36), n3(:,320), t3x512(:,:,36), nhel, den(98))
    call cont_QA(nsync, wf32(:,6), wf16(:,16), A(:,37), n3(:,321), t3x512(:,:,37), nhel, den(100))
    call cont_QA(nsync, wf32(:,17), wf16(:,18), A(:,38), n3(:,322), t3x512(:,:,38), nhel, den(102))
    call cont_QA(nsync, wf16(:,15), wf32(:,30), A(:,39), n3(:,323), t3x512(:,:,39), nhel, den(105))
    call cont_QA(nsync, wf16(:,15), wf32(:,32), A(:,40), n3(:,324), t3x512(:,:,40), nhel, den(107))
    call cont_QA(nsync, wf32(:,3), wf16(:,20), A(:,41), n3(:,325), t3x512(:,:,41), nhel, den(109))
    call cont_QA(nsync, wf32(:,20), wf16(:,22), A(:,42), n3(:,326), t3x512(:,:,42), nhel, den(111))
    call cont_QA(nsync, wf16(:,21), wf32(:,34), A(:,43), n3(:,327), t3x512(:,:,43), nhel, den(114))
    call cont_QA(nsync, wf16(:,21), wf32(:,36), A(:,44), n3(:,328), t3x512(:,:,44), nhel, den(116))
    call cont_QA(nsync, wf16(:,19), wf32(:,37), A(:,45), n3(:,329), t3x512(:,:,45), nhel, den(119))
    call cont_QA(nsync, wf16(:,19), wf32(:,38), A(:,46), n3(:,330), t3x512(:,:,46), nhel, den(121))
    call cont_QA(nsync, wf16(:,17), wf32(:,39), A(:,47), n3(:,331), t3x512(:,:,47), nhel, den(124))
    call cont_QA(nsync, wf16(:,17), wf32(:,40), A(:,48), n3(:,332), t3x512(:,:,48), nhel, den(126))
    call cont_QA(nsync, wf16(:,24), wf32(:,41), A(:,49), n3(:,333), t3x512(:,:,49), nhel, den(133))
    call cont_VV(nsync, wf16(:,25), wf32(:,42), A(:,50), n3(:,334), t3x512(:,:,50), nhel, den(138))
    call cont_VV(nsync, wf16(:,26), wf32(:,43), A(:,51), n3(:,335), t3x512(:,:,51), nhel, den(141))
    call cont_QA(nsync, wf16(:,28), wf32(:,44), A(:,52), n3(:,336), t3x512(:,:,52), nhel, den(148))
    call cont_VV(nsync, wf16(:,29), wf32(:,45), A(:,53), n3(:,337), t3x512(:,:,53), nhel, den(153))
    call cont_VV(nsync, wf16(:,30), wf32(:,46), A(:,54), n3(:,338), t3x512(:,:,54), nhel, den(156))
    call cont_SS(nsync, wf16(:,31), wf32(:,47), A(:,55), n3(:,339), t3x512(:,:,55), nhel, den(160))
    call cont_VV(nsync, wf16(:,29), wf32(:,48), A(:,56), n3(:,340), t3x512(:,:,56), nhel, den(161))
    call cont_VV(nsync, wf16(:,30), wf32(:,48), A(:,57), n3(:,341), t3x512(:,:,57), nhel, den(162))
    call cont_SS(nsync, wf16(:,32), wf32(:,49), A(:,58), n3(:,342), t3x512(:,:,58), nhel, den(166))
    call cont_VV(nsync, wf16(:,25), wf32(:,50), A(:,59), n3(:,343), t3x512(:,:,59), nhel, den(167))
    call cont_VV(nsync, wf16(:,26), wf32(:,50), A(:,60), n3(:,344), t3x512(:,:,60), nhel, den(168))
    call cont_VV(nsync, wf16(:,25), wf32(:,51), A(:,61), n3(:,345), t3x512(:,:,61), nhel, den(169))
    call cont_VV(nsync, wf16(:,26), wf32(:,52), A(:,62), n3(:,346), t3x512(:,:,62), nhel, den(170))
    call cont_QA(nsync, wf16(:,24), wf32(:,53), A(:,63), n3(:,347), t3x512(:,:,63), nhel, den(171))
    call cont_QA(nsync, wf16(:,24), wf32(:,54), A(:,64), n3(:,348), t3x512(:,:,64), nhel, den(172))
    call cont_VV(nsync, wf16(:,25), wf32(:,55), A(:,65), n3(:,349), t3x512(:,:,65), nhel, den(176))
    call cont_VV(nsync, wf16(:,26), wf32(:,56), A(:,66), n3(:,350), t3x512(:,:,66), nhel, den(177))
    call cont_QA(nsync, wf16(:,34), wf32(:,57), A(:,67), n3(:,351), t3x512(:,:,67), nhel, den(182))
    call cont_VV(nsync, wf16(:,29), wf32(:,58), A(:,68), n3(:,352), t3x512(:,:,68), nhel, den(186))
    call cont_VV(nsync, wf16(:,30), wf32(:,59), A(:,69), n3(:,353), t3x512(:,:,69), nhel, den(187))
    call cont_QA(nsync, wf16(:,36), wf32(:,60), A(:,70), n3(:,354), t3x512(:,:,70), nhel, den(192))
    call cont_SS(nsync, wf16(:,32), wf32(:,61), A(:,71), n3(:,355), t3x512(:,:,71), nhel, den(194))
    call cont_VV(nsync, wf16(:,25), wf32(:,62), A(:,72), n3(:,356), t3x512(:,:,72), nhel, den(195))
    call cont_VV(nsync, wf16(:,26), wf32(:,62), A(:,73), n3(:,357), t3x512(:,:,73), nhel, den(196))
    call cont_SS(nsync, wf16(:,31), wf32(:,63), A(:,74), n3(:,358), t3x512(:,:,74), nhel, den(198))
    call cont_VV(nsync, wf16(:,29), wf32(:,64), A(:,75), n3(:,359), t3x512(:,:,75), nhel, den(199))
    call cont_VV(nsync, wf16(:,30), wf32(:,64), A(:,76), n3(:,360), t3x512(:,:,76), nhel, den(200))
    call cont_QA(nsync, wf16(:,34), wf32(:,65), A(:,77), n3(:,361), t3x512(:,:,77), nhel, den(201))
    call cont_QA(nsync, wf16(:,34), wf32(:,66), A(:,78), n3(:,362), t3x512(:,:,78), nhel, den(202))
    call cont_VV(nsync, wf16(:,25), wf32(:,67), A(:,79), n3(:,363), t3x512(:,:,79), nhel, den(203))
    call cont_VV(nsync, wf16(:,26), wf32(:,68), A(:,80), n3(:,364), t3x512(:,:,80), nhel, den(204))
    call cont_QA(nsync, wf32(:,44), wf16(:,38), A(:,81), n3(:,365), t3x512(:,:,81), nhel, den(206))
    call cont_QA(nsync, wf32(:,57), wf16(:,40), A(:,82), n3(:,366), t3x512(:,:,82), nhel, den(208))
    call cont_QA(nsync, wf16(:,37), wf32(:,70), A(:,83), n3(:,367), t3x512(:,:,83), nhel, den(211))
    call cont_QA(nsync, wf16(:,37), wf32(:,72), A(:,84), n3(:,368), t3x512(:,:,84), nhel, den(213))
    call cont_QA(nsync, wf32(:,41), wf16(:,42), A(:,85), n3(:,369), t3x512(:,:,85), nhel, den(215))
    call cont_QA(nsync, wf32(:,60), wf16(:,44), A(:,86), n3(:,370), t3x512(:,:,86), nhel, den(217))
    call cont_QA(nsync, wf16(:,43), wf32(:,74), A(:,87), n3(:,371), t3x512(:,:,87), nhel, den(220))
    call cont_QA(nsync, wf16(:,43), wf32(:,76), A(:,88), n3(:,372), t3x512(:,:,88), nhel, den(222))
    call cont_QA(nsync, wf16(:,41), wf32(:,77), A(:,89), n3(:,373), t3x512(:,:,89), nhel, den(225))
    call cont_QA(nsync, wf16(:,41), wf32(:,78), A(:,90), n3(:,374), t3x512(:,:,90), nhel, den(227))
    call cont_QA(nsync, wf16(:,39), wf32(:,79), A(:,91), n3(:,375), t3x512(:,:,91), nhel, den(230))
    call cont_QA(nsync, wf16(:,39), wf32(:,80), A(:,92), n3(:,376), t3x512(:,:,92), nhel, den(232))
    call cont_VV(nsync, wf32(:,45), wf16(:,45), A(:,93), n3(:,377), t3x512(:,:,93), nhel, den(234))
    call cont_VV(nsync, wf32(:,46), wf16(:,47), A(:,94), n3(:,378), t3x512(:,:,94), nhel, den(236))
    call cont_QA(nsync, wf8(:,12), wf64(:,3), A(:,95), n3(:,379), t3x512(:,:,95), nhel, den(238))
    call cont_VV(nsync, wf32(:,48), wf16(:,45), A(:,96), n3(:,380), t3x512(:,:,96), nhel, den(239))
    call cont_VV(nsync, wf32(:,48), wf16(:,47), A(:,97), n3(:,381), t3x512(:,:,97), nhel, den(240))
    call cont_VV(nsync, wf16(:,45), wf32(:,81), A(:,98), n3(:,382), t3x512(:,:,98), nhel, den(243))
    call cont_VV(nsync, wf16(:,46), wf32(:,83), A(:,99), n3(:,383), t3x512(:,:,99), nhel, den(246))
    call cont_VV(nsync, wf32(:,42), wf16(:,48), A(:,100), n3(:,384), t3x512(:,:,100), nhel, den(248))
    call cont_VV(nsync, wf32(:,43), wf16(:,50), A(:,101), n3(:,385), t3x512(:,:,101), nhel, den(250))
    call cont_VV(nsync, wf32(:,42), wf16(:,51), A(:,102), n3(:,386), t3x512(:,:,102), nhel, den(252))
    call cont_VV(nsync, wf32(:,43), wf16(:,53), A(:,103), n3(:,387), t3x512(:,:,103), nhel, den(254))
    call cont_QA(nsync, wf8(:,10), wf64(:,4), A(:,104), n3(:,388), t3x512(:,:,104), nhel, den(256))
    call cont_VV(nsync, wf32(:,45), wf16(:,54), A(:,105), n3(:,389), t3x512(:,:,105), nhel, den(258))
    call cont_VV(nsync, wf32(:,46), wf16(:,56), A(:,106), n3(:,390), t3x512(:,:,106), nhel, den(260))
    call cont_VV(nsync, wf32(:,48), wf16(:,54), A(:,107), n3(:,391), t3x512(:,:,107), nhel, den(261))
    call cont_VV(nsync, wf32(:,48), wf16(:,56), A(:,108), n3(:,392), t3x512(:,:,108), nhel, den(262))
    call cont_VV(nsync, wf32(:,81), wf16(:,54), A(:,109), n3(:,393), t3x512(:,:,109), nhel, den(263))
    call cont_VV(nsync, wf32(:,82), wf16(:,56), A(:,110), n3(:,394), t3x512(:,:,110), nhel, den(264))
    call cont_VV(nsync, wf32(:,50), wf16(:,48), A(:,111), n3(:,395), t3x512(:,:,111), nhel, den(265))
    call cont_VV(nsync, wf32(:,50), wf16(:,50), A(:,112), n3(:,396), t3x512(:,:,112), nhel, den(266))
    call cont_VV(nsync, wf32(:,51), wf16(:,48), A(:,113), n3(:,397), t3x512(:,:,113), nhel, den(269))
    call cont_VV(nsync, wf16(:,49), wf32(:,84), A(:,114), n3(:,398), t3x512(:,:,114), nhel, den(272))
    call cont_VV(nsync, wf32(:,50), wf16(:,51), A(:,115), n3(:,399), t3x512(:,:,115), nhel, den(273))
    call cont_VV(nsync, wf32(:,50), wf16(:,53), A(:,116), n3(:,400), t3x512(:,:,116), nhel, den(274))
    call cont_VV(nsync, wf32(:,51), wf16(:,51), A(:,117), n3(:,401), t3x512(:,:,117), nhel, den(275))
    call cont_VV(nsync, wf32(:,52), wf16(:,53), A(:,118), n3(:,402), t3x512(:,:,118), nhel, den(276))
    call cont_VV(nsync, wf8(:,2), wf64(:,5), A(:,119), n3(:,403), t3x512(:,:,119), nhel, den(278))
    call cont_VV(nsync, wf32(:,4), wf16(:,57), A(:,120), n3(:,404), t3x512(:,:,120), nhel, den(280))
    call cont_VV(nsync, wf32(:,5), wf16(:,59), A(:,121), n3(:,405), t3x512(:,:,121), nhel, den(282))
    call cont_VV(nsync, wf32(:,8), wf16(:,57), A(:,122), n3(:,406), t3x512(:,:,122), nhel, den(283))
    call cont_VV(nsync, wf32(:,8), wf16(:,59), A(:,123), n3(:,407), t3x512(:,:,123), nhel, den(284))
    call cont_VV(nsync, wf16(:,57), wf32(:,85), A(:,124), n3(:,408), t3x512(:,:,124), nhel, den(285))
    call cont_VV(nsync, wf16(:,59), wf32(:,86), A(:,125), n3(:,409), t3x512(:,:,125), nhel, den(286))
    call cont_VV(nsync, wf8(:,2), wf64(:,6), A(:,126), n3(:,410), t3x512(:,:,126), nhel, den(288))
    call cont_VV(nsync, wf32(:,4), wf16(:,60), A(:,127), n3(:,411), t3x512(:,:,127), nhel, den(290))
    call cont_VV(nsync, wf32(:,5), wf16(:,62), A(:,128), n3(:,412), t3x512(:,:,128), nhel, den(292))
    call cont_VV(nsync, wf32(:,85), wf16(:,60), A(:,129), n3(:,413), t3x512(:,:,129), nhel, den(293))
    call cont_VV(nsync, wf32(:,86), wf16(:,62), A(:,130), n3(:,414), t3x512(:,:,130), nhel, den(294))
    call cont_VV(nsync, wf32(:,8), wf16(:,60), A(:,131), n3(:,415), t3x512(:,:,131), nhel, den(295))
    call cont_VV(nsync, wf32(:,8), wf16(:,62), A(:,132), n3(:,416), t3x512(:,:,132), nhel, den(296))
    call cont_VV(nsync, wf32(:,1), wf16(:,63), A(:,133), n3(:,417), t3x512(:,:,133), nhel, den(298))
    call cont_VV(nsync, wf32(:,2), wf16(:,65), A(:,134), n3(:,418), t3x512(:,:,134), nhel, den(300))
    call cont_VV(nsync, wf32(:,1), wf16(:,66), A(:,135), n3(:,419), t3x512(:,:,135), nhel, den(302))
    call cont_VV(nsync, wf32(:,2), wf16(:,68), A(:,136), n3(:,420), t3x512(:,:,136), nhel, den(304))
    call cont_VV(nsync, wf32(:,12), wf16(:,63), A(:,137), n3(:,421), t3x512(:,:,137), nhel, den(305))
    call cont_VV(nsync, wf32(:,12), wf16(:,65), A(:,138), n3(:,422), t3x512(:,:,138), nhel, den(306))
    call cont_VV(nsync, wf32(:,13), wf16(:,63), A(:,139), n3(:,423), t3x512(:,:,139), nhel, den(307))
    call cont_VV(nsync, wf32(:,14), wf16(:,65), A(:,140), n3(:,424), t3x512(:,:,140), nhel, den(308))
    call cont_VV(nsync, wf32(:,13), wf16(:,66), A(:,141), n3(:,425), t3x512(:,:,141), nhel, den(309))
    call cont_VV(nsync, wf32(:,14), wf16(:,68), A(:,142), n3(:,426), t3x512(:,:,142), nhel, den(310))
    call cont_VV(nsync, wf32(:,12), wf16(:,66), A(:,143), n3(:,427), t3x512(:,:,143), nhel, den(311))
    call cont_VV(nsync, wf32(:,12), wf16(:,68), A(:,144), n3(:,428), t3x512(:,:,144), nhel, den(312))
    call cont_VV(nsync, wf32(:,55), wf16(:,48), A(:,145), n3(:,429), t3x512(:,:,145), nhel, den(313))
    call cont_VV(nsync, wf32(:,56), wf16(:,50), A(:,146), n3(:,430), t3x512(:,:,146), nhel, den(314))
    call cont_VV(nsync, wf32(:,55), wf16(:,51), A(:,147), n3(:,431), t3x512(:,:,147), nhel, den(315))
    call cont_VV(nsync, wf32(:,56), wf16(:,53), A(:,148), n3(:,432), t3x512(:,:,148), nhel, den(316))
    call cont_VV(nsync, wf32(:,58), wf16(:,45), A(:,149), n3(:,433), t3x512(:,:,149), nhel, den(317))
    call cont_VV(nsync, wf32(:,59), wf16(:,47), A(:,150), n3(:,434), t3x512(:,:,150), nhel, den(318))
    call cont_QA(nsync, wf8(:,12), wf64(:,7), A(:,151), n3(:,435), t3x512(:,:,151), nhel, den(320))
    call cont_VV(nsync, wf32(:,64), wf16(:,45), A(:,152), n3(:,436), t3x512(:,:,152), nhel, den(321))
    call cont_VV(nsync, wf32(:,64), wf16(:,47), A(:,153), n3(:,437), t3x512(:,:,153), nhel, den(322))
    call cont_VV(nsync, wf16(:,45), wf32(:,87), A(:,154), n3(:,438), t3x512(:,:,154), nhel, den(324))
    call cont_VV(nsync, wf16(:,46), wf32(:,89), A(:,155), n3(:,439), t3x512(:,:,155), nhel, den(326))
    call cont_VV(nsync, wf32(:,58), wf16(:,54), A(:,156), n3(:,440), t3x512(:,:,156), nhel, den(327))
    call cont_VV(nsync, wf32(:,59), wf16(:,56), A(:,157), n3(:,441), t3x512(:,:,157), nhel, den(328))
    call cont_QA(nsync, wf8(:,10), wf64(:,8), A(:,158), n3(:,442), t3x512(:,:,158), nhel, den(330))
    call cont_VV(nsync, wf16(:,54), wf32(:,87), A(:,159), n3(:,443), t3x512(:,:,159), nhel, den(331))
    call cont_VV(nsync, wf16(:,55), wf32(:,89), A(:,160), n3(:,444), t3x512(:,:,160), nhel, den(332))
    call cont_VV(nsync, wf32(:,64), wf16(:,54), A(:,161), n3(:,445), t3x512(:,:,161), nhel, den(333))
    call cont_VV(nsync, wf32(:,64), wf16(:,56), A(:,162), n3(:,446), t3x512(:,:,162), nhel, den(334))
    call cont_VV(nsync, wf32(:,62), wf16(:,48), A(:,163), n3(:,447), t3x512(:,:,163), nhel, den(335))
    call cont_VV(nsync, wf32(:,62), wf16(:,50), A(:,164), n3(:,448), t3x512(:,:,164), nhel, den(336))
    call cont_VV(nsync, wf32(:,67), wf16(:,48), A(:,165), n3(:,449), t3x512(:,:,165), nhel, den(338))
    call cont_VV(nsync, wf16(:,49), wf32(:,90), A(:,166), n3(:,450), t3x512(:,:,166), nhel, den(340))
    call cont_VV(nsync, wf32(:,67), wf16(:,51), A(:,167), n3(:,451), t3x512(:,:,167), nhel, den(341))
    call cont_VV(nsync, wf16(:,52), wf32(:,90), A(:,168), n3(:,452), t3x512(:,:,168), nhel, den(342))
    call cont_VV(nsync, wf32(:,62), wf16(:,51), A(:,169), n3(:,453), t3x512(:,:,169), nhel, den(343))
    call cont_VV(nsync, wf32(:,62), wf16(:,53), A(:,170), n3(:,454), t3x512(:,:,170), nhel, den(344))
    call cont_VV(nsync, wf32(:,18), wf16(:,57), A(:,171), n3(:,455), t3x512(:,:,171), nhel, den(345))
    call cont_VV(nsync, wf32(:,19), wf16(:,59), A(:,172), n3(:,456), t3x512(:,:,172), nhel, den(346))
    call cont_QA(nsync, wf8(:,20), wf64(:,9), A(:,173), n3(:,457), t3x512(:,:,173), nhel, den(348))
    call cont_VV(nsync, wf16(:,57), wf32(:,91), A(:,174), n3(:,458), t3x512(:,:,174), nhel, den(351))
    call cont_VV(nsync, wf16(:,58), wf32(:,93), A(:,175), n3(:,459), t3x512(:,:,175), nhel, den(354))
    call cont_VV(nsync, wf32(:,22), wf16(:,57), A(:,176), n3(:,460), t3x512(:,:,176), nhel, den(355))
    call cont_VV(nsync, wf32(:,22), wf16(:,59), A(:,177), n3(:,461), t3x512(:,:,177), nhel, den(356))
    call cont_VV(nsync, wf32(:,15), wf16(:,63), A(:,178), n3(:,462), t3x512(:,:,178), nhel, den(357))
    call cont_VV(nsync, wf32(:,16), wf16(:,65), A(:,179), n3(:,463), t3x512(:,:,179), nhel, den(358))
    call cont_VV(nsync, wf32(:,15), wf16(:,66), A(:,180), n3(:,464), t3x512(:,:,180), nhel, den(359))
    call cont_VV(nsync, wf32(:,16), wf16(:,68), A(:,181), n3(:,465), t3x512(:,:,181), nhel, den(360))
    call cont_QA(nsync, wf8(:,18), wf64(:,10), A(:,182), n3(:,466), t3x512(:,:,182), nhel, den(362))
    call cont_VV(nsync, wf32(:,18), wf16(:,60), A(:,183), n3(:,467), t3x512(:,:,183), nhel, den(363))
    call cont_VV(nsync, wf32(:,19), wf16(:,62), A(:,184), n3(:,468), t3x512(:,:,184), nhel, den(364))
    call cont_VV(nsync, wf16(:,60), wf32(:,91), A(:,185), n3(:,469), t3x512(:,:,185), nhel, den(365))
    call cont_VV(nsync, wf16(:,62), wf32(:,92), A(:,186), n3(:,470), t3x512(:,:,186), nhel, den(366))
    call cont_VV(nsync, wf32(:,22), wf16(:,60), A(:,187), n3(:,471), t3x512(:,:,187), nhel, den(367))
    call cont_VV(nsync, wf32(:,22), wf16(:,62), A(:,188), n3(:,472), t3x512(:,:,188), nhel, den(368))
    call cont_VV(nsync, wf32(:,27), wf16(:,63), A(:,189), n3(:,473), t3x512(:,:,189), nhel, den(371))
    call cont_VV(nsync, wf16(:,64), wf32(:,94), A(:,190), n3(:,474), t3x512(:,:,190), nhel, den(374))
    call cont_VV(nsync, wf32(:,26), wf16(:,63), A(:,191), n3(:,475), t3x512(:,:,191), nhel, den(375))
    call cont_VV(nsync, wf32(:,26), wf16(:,65), A(:,192), n3(:,476), t3x512(:,:,192), nhel, den(376))
    call cont_VV(nsync, wf32(:,27), wf16(:,66), A(:,193), n3(:,477), t3x512(:,:,193), nhel, den(377))
    call cont_VV(nsync, wf32(:,28), wf16(:,68), A(:,194), n3(:,478), t3x512(:,:,194), nhel, den(378))
    call cont_VV(nsync, wf32(:,26), wf16(:,66), A(:,195), n3(:,479), t3x512(:,:,195), nhel, den(379))
    call cont_VV(nsync, wf32(:,26), wf16(:,68), A(:,196), n3(:,480), t3x512(:,:,196), nhel, den(380))
    call cont_QA(nsync, wf16(:,20), wf32(:,95), A(:,197), n3(:,481), t3x512(:,:,197), nhel, den(381))
    call cont_QA(nsync, wf16(:,20), wf32(:,96), A(:,198), n3(:,482), t3x512(:,:,198), nhel, den(382))
    call cont_QA(nsync, wf16(:,38), wf32(:,97), A(:,199), n3(:,483), t3x512(:,:,199), nhel, den(383))
    call cont_QA(nsync, wf16(:,38), wf32(:,98), A(:,200), n3(:,484), t3x512(:,:,200), nhel, den(384))
    call cont_QA(nsync, wf16(:,18), wf32(:,99), A(:,201), n3(:,485), t3x512(:,:,201), nhel, den(385))
    call cont_QA(nsync, wf16(:,18), wf32(:,100), A(:,202), n3(:,486), t3x512(:,:,202), nhel, den(386))
    call cont_QA(nsync, wf16(:,38), wf32(:,101), A(:,203), n3(:,487), t3x512(:,:,203), nhel, den(387))
    call cont_QA(nsync, wf16(:,38), wf32(:,102), A(:,204), n3(:,488), t3x512(:,:,204), nhel, den(388))
    call cont_QA(nsync, wf16(:,16), wf32(:,103), A(:,205), n3(:,489), t3x512(:,:,205), nhel, den(389))
    call cont_QA(nsync, wf16(:,16), wf32(:,104), A(:,206), n3(:,490), t3x512(:,:,206), nhel, den(390))
    call cont_QA(nsync, wf16(:,42), wf32(:,105), A(:,207), n3(:,491), t3x512(:,:,207), nhel, den(391))
    call cont_QA(nsync, wf16(:,42), wf32(:,106), A(:,208), n3(:,492), t3x512(:,:,208), nhel, den(392))
    call cont_QA(nsync, wf16(:,16), wf32(:,107), A(:,209), n3(:,493), t3x512(:,:,209), nhel, den(393))
    call cont_QA(nsync, wf16(:,16), wf32(:,108), A(:,210), n3(:,494), t3x512(:,:,210), nhel, den(394))
    call cont_QA(nsync, wf16(:,40), wf32(:,109), A(:,211), n3(:,495), t3x512(:,:,211), nhel, den(395))
    call cont_QA(nsync, wf16(:,40), wf32(:,110), A(:,212), n3(:,496), t3x512(:,:,212), nhel, den(396))
    call cont_QA(nsync, wf16(:,20), wf32(:,111), A(:,213), n3(:,497), t3x512(:,:,213), nhel, den(397))
    call cont_QA(nsync, wf16(:,20), wf32(:,112), A(:,214), n3(:,498), t3x512(:,:,214), nhel, den(398))
    call cont_QA(nsync, wf16(:,44), wf32(:,113), A(:,215), n3(:,499), t3x512(:,:,215), nhel, den(399))
    call cont_QA(nsync, wf16(:,44), wf32(:,114), A(:,216), n3(:,500), t3x512(:,:,216), nhel, den(400))
    call cont_QA(nsync, wf16(:,18), wf32(:,115), A(:,217), n3(:,501), t3x512(:,:,217), nhel, den(401))
    call cont_QA(nsync, wf16(:,18), wf32(:,116), A(:,218), n3(:,502), t3x512(:,:,218), nhel, den(402))
    call cont_QA(nsync, wf16(:,44), wf32(:,117), A(:,219), n3(:,503), t3x512(:,:,219), nhel, den(403))
    call cont_QA(nsync, wf16(:,44), wf32(:,118), A(:,220), n3(:,504), t3x512(:,:,220), nhel, den(404))
    call cont_QA(nsync, wf16(:,22), wf32(:,119), A(:,221), n3(:,505), t3x512(:,:,221), nhel, den(405))
    call cont_QA(nsync, wf16(:,22), wf32(:,120), A(:,222), n3(:,506), t3x512(:,:,222), nhel, den(406))
    call cont_QA(nsync, wf16(:,42), wf32(:,121), A(:,223), n3(:,507), t3x512(:,:,223), nhel, den(407))
    call cont_QA(nsync, wf16(:,42), wf32(:,122), A(:,224), n3(:,508), t3x512(:,:,224), nhel, den(408))
    call cont_QA(nsync, wf16(:,22), wf32(:,123), A(:,225), n3(:,509), t3x512(:,:,225), nhel, den(409))
    call cont_QA(nsync, wf16(:,22), wf32(:,124), A(:,226), n3(:,510), t3x512(:,:,226), nhel, den(410))
    call cont_QA(nsync, wf16(:,40), wf32(:,125), A(:,227), n3(:,511), t3x512(:,:,227), nhel, den(411))
    call cont_QA(nsync, wf16(:,40), wf32(:,126), A(:,228), n3(:,512), t3x512(:,:,228), nhel, den(412))
    call cont_VV(nsync, wf32(:,1), wf16(:,69), A(:,229), n3(:,513), t3x512(:,:,229), nhel, den(416))
    call cont_VV(nsync, wf32(:,2), wf16(:,71), A(:,230), n3(:,514), t3x512(:,:,230), nhel, den(418))
    call cont_VV(nsync, wf32(:,2), wf16(:,73), A(:,231), n3(:,515), t3x512(:,:,231), nhel, den(422))
    call cont_VV(nsync, wf32(:,13), wf16(:,69), A(:,232), n3(:,516), t3x512(:,:,232), nhel, den(423))
    call cont_VV(nsync, wf32(:,14), wf16(:,71), A(:,233), n3(:,517), t3x512(:,:,233), nhel, den(424))
    call cont_VV(nsync, wf32(:,12), wf16(:,69), A(:,234), n3(:,518), t3x512(:,:,234), nhel, den(425))
    call cont_VV(nsync, wf32(:,12), wf16(:,71), A(:,235), n3(:,519), t3x512(:,:,235), nhel, den(426))
    call cont_VV(nsync, wf32(:,14), wf16(:,73), A(:,236), n3(:,520), t3x512(:,:,236), nhel, den(427))
    call cont_QA(nsync, wf32(:,127), wf16(:,75), A(:,237), n3(:,521), t3x512(:,:,237), nhel, den(430))
    call cont_QA(nsync, wf16(:,75), wf32(:,128), A(:,238), n3(:,522), t3x512(:,:,238), nhel, den(431))
    call cont_VV(nsync, wf32(:,12), wf16(:,73), A(:,239), n3(:,523), t3x512(:,:,239), nhel, den(432))
    call cont_QA(nsync, wf16(:,76), wf32(:,130), A(:,240), n3(:,524), t3x512(:,:,240), nhel, den(435))
    call cont_QA(nsync, wf32(:,131), wf16(:,77), A(:,241), n3(:,525), t3x512(:,:,241), nhel, den(439))
    call cont_QA(nsync, wf16(:,75), wf32(:,132), A(:,242), n3(:,526), t3x512(:,:,242), nhel, den(441))
    call cont_VV(nsync, wf32(:,15), wf16(:,69), A(:,243), n3(:,527), t3x512(:,:,243), nhel, den(442))
    call cont_VV(nsync, wf32(:,16), wf16(:,71), A(:,244), n3(:,528), t3x512(:,:,244), nhel, den(443))
    call cont_VV(nsync, wf32(:,16), wf16(:,73), A(:,245), n3(:,529), t3x512(:,:,245), nhel, den(444))
    call cont_VV(nsync, wf32(:,27), wf16(:,69), A(:,246), n3(:,530), t3x512(:,:,246), nhel, den(445))
    call cont_VV(nsync, wf32(:,28), wf16(:,71), A(:,247), n3(:,531), t3x512(:,:,247), nhel, den(446))
    call cont_VV(nsync, wf32(:,26), wf16(:,69), A(:,248), n3(:,532), t3x512(:,:,248), nhel, den(447))
    call cont_VV(nsync, wf32(:,26), wf16(:,71), A(:,249), n3(:,533), t3x512(:,:,249), nhel, den(448))
    call cont_VV(nsync, wf32(:,28), wf16(:,73), A(:,250), n3(:,534), t3x512(:,:,250), nhel, den(449))
    call cont_QA(nsync, wf32(:,127), wf16(:,79), A(:,251), n3(:,535), t3x512(:,:,251), nhel, den(451))
    call cont_QA(nsync, wf32(:,128), wf16(:,79), A(:,252), n3(:,536), t3x512(:,:,252), nhel, den(452))
    call cont_VV(nsync, wf32(:,26), wf16(:,73), A(:,253), n3(:,537), t3x512(:,:,253), nhel, den(453))
    call cont_QA(nsync, wf32(:,130), wf16(:,80), A(:,254), n3(:,538), t3x512(:,:,254), nhel, den(454))
    call cont_QA(nsync, wf32(:,131), wf16(:,81), A(:,255), n3(:,539), t3x512(:,:,255), nhel, den(456))
    call cont_QA(nsync, wf32(:,132), wf16(:,79), A(:,256), n3(:,540), t3x512(:,:,256), nhel, den(457))
    call cont_QA(nsync, wf16(:,16), wf32(:,133), A(:,257), n3(:,541), t3x512(:,:,257), nhel, den(458))
    call cont_QA(nsync, wf16(:,16), wf32(:,134), A(:,258), n3(:,542), t3x512(:,:,258), nhel, den(459))
    call cont_QA(nsync, wf16(:,16), wf32(:,135), A(:,259), n3(:,543), t3x512(:,:,259), nhel, den(460))
    call cont_QA(nsync, wf16(:,22), wf32(:,136), A(:,260), n3(:,544), t3x512(:,:,260), nhel, den(461))
    call cont_QA(nsync, wf16(:,22), wf32(:,137), A(:,261), n3(:,545), t3x512(:,:,261), nhel, den(462))
    call cont_QA(nsync, wf16(:,22), wf32(:,138), A(:,262), n3(:,546), t3x512(:,:,262), nhel, den(463))
    call cont_VV(nsync, wf32(:,42), wf16(:,82), A(:,263), n3(:,547), t3x512(:,:,263), nhel, den(467))
    call cont_VV(nsync, wf32(:,43), wf16(:,84), A(:,264), n3(:,548), t3x512(:,:,264), nhel, den(469))
    call cont_VV(nsync, wf32(:,43), wf16(:,86), A(:,265), n3(:,549), t3x512(:,:,265), nhel, den(473))
    call cont_VV(nsync, wf32(:,51), wf16(:,82), A(:,266), n3(:,550), t3x512(:,:,266), nhel, den(474))
    call cont_VV(nsync, wf32(:,52), wf16(:,84), A(:,267), n3(:,551), t3x512(:,:,267), nhel, den(475))
    call cont_VV(nsync, wf32(:,50), wf16(:,82), A(:,268), n3(:,552), t3x512(:,:,268), nhel, den(476))
    call cont_VV(nsync, wf32(:,50), wf16(:,84), A(:,269), n3(:,553), t3x512(:,:,269), nhel, den(477))
    call cont_VV(nsync, wf32(:,52), wf16(:,86), A(:,270), n3(:,554), t3x512(:,:,270), nhel, den(478))
    call cont_QA(nsync, wf32(:,139), wf16(:,88), A(:,271), n3(:,555), t3x512(:,:,271), nhel, den(481))
    call cont_QA(nsync, wf16(:,88), wf32(:,140), A(:,272), n3(:,556), t3x512(:,:,272), nhel, den(482))
    call cont_VV(nsync, wf32(:,50), wf16(:,86), A(:,273), n3(:,557), t3x512(:,:,273), nhel, den(483))
    call cont_QA(nsync, wf16(:,89), wf32(:,142), A(:,274), n3(:,558), t3x512(:,:,274), nhel, den(486))
    call cont_QA(nsync, wf32(:,143), wf16(:,90), A(:,275), n3(:,559), t3x512(:,:,275), nhel, den(490))
    call cont_QA(nsync, wf16(:,88), wf32(:,144), A(:,276), n3(:,560), t3x512(:,:,276), nhel, den(492))
    call cont_VV(nsync, wf32(:,55), wf16(:,82), A(:,277), n3(:,561), t3x512(:,:,277), nhel, den(493))
    call cont_VV(nsync, wf32(:,56), wf16(:,84), A(:,278), n3(:,562), t3x512(:,:,278), nhel, den(494))
    call cont_VV(nsync, wf32(:,56), wf16(:,86), A(:,279), n3(:,563), t3x512(:,:,279), nhel, den(495))
    call cont_VV(nsync, wf32(:,68), wf16(:,86), A(:,280), n3(:,564), t3x512(:,:,280), nhel, den(496))
    call cont_QA(nsync, wf32(:,139), wf16(:,92), A(:,281), n3(:,565), t3x512(:,:,281), nhel, den(498))
    call cont_QA(nsync, wf32(:,140), wf16(:,92), A(:,282), n3(:,566), t3x512(:,:,282), nhel, den(499))
    call cont_VV(nsync, wf32(:,67), wf16(:,82), A(:,283), n3(:,567), t3x512(:,:,283), nhel, den(500))
    call cont_VV(nsync, wf32(:,68), wf16(:,84), A(:,284), n3(:,568), t3x512(:,:,284), nhel, den(501))
    call cont_VV(nsync, wf32(:,62), wf16(:,82), A(:,285), n3(:,569), t3x512(:,:,285), nhel, den(502))
    call cont_VV(nsync, wf32(:,62), wf16(:,84), A(:,286), n3(:,570), t3x512(:,:,286), nhel, den(503))
    call cont_QA(nsync, wf32(:,142), wf16(:,93), A(:,287), n3(:,571), t3x512(:,:,287), nhel, den(504))
    call cont_VV(nsync, wf32(:,62), wf16(:,86), A(:,288), n3(:,572), t3x512(:,:,288), nhel, den(505))
    call cont_QA(nsync, wf32(:,144), wf16(:,92), A(:,289), n3(:,573), t3x512(:,:,289), nhel, den(506))
    call cont_QA(nsync, wf32(:,143), wf16(:,94), A(:,290), n3(:,574), t3x512(:,:,290), nhel, den(508))
    call cont_QA(nsync, wf16(:,38), wf32(:,145), A(:,291), n3(:,575), t3x512(:,:,291), nhel, den(509))
    call cont_QA(nsync, wf16(:,38), wf32(:,146), A(:,292), n3(:,576), t3x512(:,:,292), nhel, den(510))
    call cont_QA(nsync, wf16(:,38), wf32(:,147), A(:,293), n3(:,577), t3x512(:,:,293), nhel, den(511))
    call cont_QA(nsync, wf16(:,44), wf32(:,148), A(:,294), n3(:,578), t3x512(:,:,294), nhel, den(512))
    call cont_QA(nsync, wf16(:,44), wf32(:,149), A(:,295), n3(:,579), t3x512(:,:,295), nhel, den(513))
    call cont_QA(nsync, wf16(:,44), wf32(:,150), A(:,296), n3(:,580), t3x512(:,:,296), nhel, den(514))
    call cont_VV(nsync, wf16(:,45), wf32(:,151), A(:,297), n3(:,581), t3x512(:,:,297), nhel, den(515))
    call cont_VV(nsync, wf16(:,47), wf32(:,152), A(:,298), n3(:,582), t3x512(:,:,298), nhel, den(516))
    call cont_VV(nsync, wf16(:,47), wf32(:,153), A(:,299), n3(:,583), t3x512(:,:,299), nhel, den(517))
    call cont_VV(nsync, wf16(:,56), wf32(:,153), A(:,300), n3(:,584), t3x512(:,:,300), nhel, den(518))
    call cont_VV(nsync, wf16(:,54), wf32(:,151), A(:,301), n3(:,585), t3x512(:,:,301), nhel, den(519))
    call cont_VV(nsync, wf16(:,56), wf32(:,152), A(:,302), n3(:,586), t3x512(:,:,302), nhel, den(520))
    call cont_VV(nsync, wf16(:,59), wf32(:,154), A(:,303), n3(:,587), t3x512(:,:,303), nhel, den(521))
    call cont_VV(nsync, wf16(:,57), wf32(:,155), A(:,304), n3(:,588), t3x512(:,:,304), nhel, den(522))
    call cont_VV(nsync, wf16(:,59), wf32(:,156), A(:,305), n3(:,589), t3x512(:,:,305), nhel, den(523))
    call cont_VV(nsync, wf16(:,62), wf32(:,154), A(:,306), n3(:,590), t3x512(:,:,306), nhel, den(524))
    call cont_VV(nsync, wf16(:,60), wf32(:,155), A(:,307), n3(:,591), t3x512(:,:,307), nhel, den(525))
    call cont_VV(nsync, wf16(:,62), wf32(:,156), A(:,308), n3(:,592), t3x512(:,:,308), nhel, den(526))
    call cont_VV(nsync, wf16(:,45), wf32(:,157), A(:,309), n3(:,593), t3x512(:,:,309), nhel, den(527))
    call cont_VV(nsync, wf16(:,47), wf32(:,158), A(:,310), n3(:,594), t3x512(:,:,310), nhel, den(528))
    call cont_VV(nsync, wf16(:,47), wf32(:,159), A(:,311), n3(:,595), t3x512(:,:,311), nhel, den(529))
    call cont_VV(nsync, wf16(:,54), wf32(:,157), A(:,312), n3(:,596), t3x512(:,:,312), nhel, den(530))
    call cont_VV(nsync, wf16(:,56), wf32(:,158), A(:,313), n3(:,597), t3x512(:,:,313), nhel, den(531))
    call cont_VV(nsync, wf16(:,56), wf32(:,159), A(:,314), n3(:,598), t3x512(:,:,314), nhel, den(532))
    call cont_VV(nsync, wf16(:,57), wf32(:,160), A(:,315), n3(:,599), t3x512(:,:,315), nhel, den(533))
    call cont_VV(nsync, wf16(:,59), wf32(:,161), A(:,316), n3(:,600), t3x512(:,:,316), nhel, den(534))
    call cont_VV(nsync, wf16(:,59), wf32(:,162), A(:,317), n3(:,601), t3x512(:,:,317), nhel, den(535))
    call cont_VV(nsync, wf16(:,62), wf32(:,162), A(:,318), n3(:,602), t3x512(:,:,318), nhel, den(536))
    call cont_VV(nsync, wf16(:,60), wf32(:,160), A(:,319), n3(:,603), t3x512(:,:,319), nhel, den(537))
    call cont_VV(nsync, wf16(:,62), wf32(:,161), A(:,320), n3(:,604), t3x512(:,:,320), nhel, den(538))
    call cont_VV(nsync, wf32(:,4), wf16(:,95), A(:,321), n3(:,605), t3x512(:,:,321), nhel, den(542))
    call cont_VV(nsync, wf32(:,5), wf16(:,97), A(:,322), n3(:,606), t3x512(:,:,322), nhel, den(544))
    call cont_VV(nsync, wf32(:,85), wf16(:,95), A(:,323), n3(:,607), t3x512(:,:,323), nhel, den(545))
    call cont_VV(nsync, wf32(:,86), wf16(:,97), A(:,324), n3(:,608), t3x512(:,:,324), nhel, den(546))
    call cont_VV(nsync, wf32(:,8), wf16(:,95), A(:,325), n3(:,609), t3x512(:,:,325), nhel, den(547))
    call cont_VV(nsync, wf32(:,8), wf16(:,97), A(:,326), n3(:,610), t3x512(:,:,326), nhel, den(548))
    call cont_VV(nsync, wf32(:,86), wf16(:,99), A(:,327), n3(:,611), t3x512(:,:,327), nhel, den(552))
    call cont_QA(nsync, wf32(:,163), wf16(:,101), A(:,328), n3(:,612), t3x512(:,:,328), nhel, den(555))
    call cont_QA(nsync, wf16(:,101), wf32(:,164), A(:,329), n3(:,613), t3x512(:,:,329), nhel, den(556))
    call cont_VV(nsync, wf32(:,5), wf16(:,99), A(:,330), n3(:,614), t3x512(:,:,330), nhel, den(557))
    call cont_VV(nsync, wf32(:,8), wf16(:,99), A(:,331), n3(:,615), t3x512(:,:,331), nhel, den(558))
    call cont_QA(nsync, wf16(:,102), wf32(:,166), A(:,332), n3(:,616), t3x512(:,:,332), nhel, den(561))
    call cont_QA(nsync, wf32(:,167), wf16(:,103), A(:,333), n3(:,617), t3x512(:,:,333), nhel, den(565))
    call cont_QA(nsync, wf16(:,101), wf32(:,168), A(:,334), n3(:,618), t3x512(:,:,334), nhel, den(567))
    call cont_VV(nsync, wf32(:,18), wf16(:,95), A(:,335), n3(:,619), t3x512(:,:,335), nhel, den(568))
    call cont_VV(nsync, wf32(:,19), wf16(:,97), A(:,336), n3(:,620), t3x512(:,:,336), nhel, den(569))
    call cont_VV(nsync, wf32(:,91), wf16(:,95), A(:,337), n3(:,621), t3x512(:,:,337), nhel, den(570))
    call cont_VV(nsync, wf32(:,92), wf16(:,97), A(:,338), n3(:,622), t3x512(:,:,338), nhel, den(571))
    call cont_VV(nsync, wf32(:,22), wf16(:,95), A(:,339), n3(:,623), t3x512(:,:,339), nhel, den(572))
    call cont_VV(nsync, wf32(:,22), wf16(:,97), A(:,340), n3(:,624), t3x512(:,:,340), nhel, den(573))
    call cont_VV(nsync, wf32(:,92), wf16(:,99), A(:,341), n3(:,625), t3x512(:,:,341), nhel, den(574))
    call cont_QA(nsync, wf32(:,163), wf16(:,105), A(:,342), n3(:,626), t3x512(:,:,342), nhel, den(576))
    call cont_QA(nsync, wf32(:,164), wf16(:,105), A(:,343), n3(:,627), t3x512(:,:,343), nhel, den(577))
    call cont_VV(nsync, wf32(:,19), wf16(:,99), A(:,344), n3(:,628), t3x512(:,:,344), nhel, den(578))
    call cont_VV(nsync, wf32(:,22), wf16(:,99), A(:,345), n3(:,629), t3x512(:,:,345), nhel, den(579))
    call cont_QA(nsync, wf32(:,166), wf16(:,106), A(:,346), n3(:,630), t3x512(:,:,346), nhel, den(580))
    call cont_QA(nsync, wf32(:,167), wf16(:,107), A(:,347), n3(:,631), t3x512(:,:,347), nhel, den(582))
    call cont_QA(nsync, wf32(:,168), wf16(:,105), A(:,348), n3(:,632), t3x512(:,:,348), nhel, den(583))
    call cont_QA(nsync, wf16(:,20), wf32(:,169), A(:,349), n3(:,633), t3x512(:,:,349), nhel, den(584))
    call cont_QA(nsync, wf16(:,20), wf32(:,170), A(:,350), n3(:,634), t3x512(:,:,350), nhel, den(585))
    call cont_QA(nsync, wf16(:,18), wf32(:,171), A(:,351), n3(:,635), t3x512(:,:,351), nhel, den(586))
    call cont_QA(nsync, wf16(:,18), wf32(:,172), A(:,352), n3(:,636), t3x512(:,:,352), nhel, den(587))
    call cont_QA(nsync, wf16(:,18), wf32(:,173), A(:,353), n3(:,637), t3x512(:,:,353), nhel, den(588))
    call cont_QA(nsync, wf16(:,20), wf32(:,174), A(:,354), n3(:,638), t3x512(:,:,354), nhel, den(589))
    call cont_VV(nsync, wf32(:,45), wf16(:,108), A(:,355), n3(:,639), t3x512(:,:,355), nhel, den(593))
    call cont_VV(nsync, wf32(:,46), wf16(:,110), A(:,356), n3(:,640), t3x512(:,:,356), nhel, den(595))
    call cont_VV(nsync, wf32(:,81), wf16(:,108), A(:,357), n3(:,641), t3x512(:,:,357), nhel, den(596))
    call cont_VV(nsync, wf32(:,82), wf16(:,110), A(:,358), n3(:,642), t3x512(:,:,358), nhel, den(597))
    call cont_VV(nsync, wf32(:,48), wf16(:,108), A(:,359), n3(:,643), t3x512(:,:,359), nhel, den(598))
    call cont_VV(nsync, wf32(:,48), wf16(:,110), A(:,360), n3(:,644), t3x512(:,:,360), nhel, den(599))
    call cont_VV(nsync, wf32(:,82), wf16(:,112), A(:,361), n3(:,645), t3x512(:,:,361), nhel, den(603))
    call cont_QA(nsync, wf32(:,175), wf16(:,114), A(:,362), n3(:,646), t3x512(:,:,362), nhel, den(606))
    call cont_QA(nsync, wf16(:,114), wf32(:,176), A(:,363), n3(:,647), t3x512(:,:,363), nhel, den(607))
    call cont_VV(nsync, wf32(:,46), wf16(:,112), A(:,364), n3(:,648), t3x512(:,:,364), nhel, den(608))
    call cont_VV(nsync, wf32(:,48), wf16(:,112), A(:,365), n3(:,649), t3x512(:,:,365), nhel, den(609))
    call cont_QA(nsync, wf16(:,115), wf32(:,178), A(:,366), n3(:,650), t3x512(:,:,366), nhel, den(612))
    call cont_QA(nsync, wf32(:,179), wf16(:,116), A(:,367), n3(:,651), t3x512(:,:,367), nhel, den(616))
    call cont_QA(nsync, wf16(:,114), wf32(:,180), A(:,368), n3(:,652), t3x512(:,:,368), nhel, den(618))
    call cont_VV(nsync, wf32(:,88), wf16(:,112), A(:,369), n3(:,653), t3x512(:,:,369), nhel, den(619))
    call cont_QA(nsync, wf32(:,175), wf16(:,118), A(:,370), n3(:,654), t3x512(:,:,370), nhel, den(621))
    call cont_QA(nsync, wf32(:,176), wf16(:,118), A(:,371), n3(:,655), t3x512(:,:,371), nhel, den(622))
    call cont_VV(nsync, wf32(:,58), wf16(:,108), A(:,372), n3(:,656), t3x512(:,:,372), nhel, den(623))
    call cont_VV(nsync, wf32(:,59), wf16(:,110), A(:,373), n3(:,657), t3x512(:,:,373), nhel, den(624))
    call cont_VV(nsync, wf32(:,87), wf16(:,108), A(:,374), n3(:,658), t3x512(:,:,374), nhel, den(625))
    call cont_VV(nsync, wf32(:,88), wf16(:,110), A(:,375), n3(:,659), t3x512(:,:,375), nhel, den(626))
    call cont_VV(nsync, wf32(:,64), wf16(:,108), A(:,376), n3(:,660), t3x512(:,:,376), nhel, den(627))
    call cont_VV(nsync, wf32(:,64), wf16(:,110), A(:,377), n3(:,661), t3x512(:,:,377), nhel, den(628))
    call cont_VV(nsync, wf32(:,59), wf16(:,112), A(:,378), n3(:,662), t3x512(:,:,378), nhel, den(629))
    call cont_QA(nsync, wf32(:,178), wf16(:,119), A(:,379), n3(:,663), t3x512(:,:,379), nhel, den(630))
    call cont_VV(nsync, wf32(:,64), wf16(:,112), A(:,380), n3(:,664), t3x512(:,:,380), nhel, den(631))
    call cont_QA(nsync, wf32(:,180), wf16(:,118), A(:,381), n3(:,665), t3x512(:,:,381), nhel, den(632))
    call cont_QA(nsync, wf32(:,179), wf16(:,120), A(:,382), n3(:,666), t3x512(:,:,382), nhel, den(634))
    call cont_QA(nsync, wf16(:,40), wf32(:,181), A(:,383), n3(:,667), t3x512(:,:,383), nhel, den(635))
    call cont_QA(nsync, wf16(:,42), wf32(:,182), A(:,384), n3(:,668), t3x512(:,:,384), nhel, den(636))
    call cont_QA(nsync, wf16(:,42), wf32(:,183), A(:,385), n3(:,669), t3x512(:,:,385), nhel, den(637))
    call cont_QA(nsync, wf16(:,40), wf32(:,184), A(:,386), n3(:,670), t3x512(:,:,386), nhel, den(638))
    call cont_QA(nsync, wf16(:,40), wf32(:,185), A(:,387), n3(:,671), t3x512(:,:,387), nhel, den(639))
    call cont_QA(nsync, wf16(:,42), wf32(:,186), A(:,388), n3(:,672), t3x512(:,:,388), nhel, den(640))
    call cont_VV(nsync, wf16(:,53), wf32(:,187), A(:,389), n3(:,673), t3x512(:,:,389), nhel, den(641))
    call cont_VV(nsync, wf16(:,48), wf32(:,188), A(:,390), n3(:,674), t3x512(:,:,390), nhel, den(642))
    call cont_VV(nsync, wf16(:,50), wf32(:,189), A(:,391), n3(:,675), t3x512(:,:,391), nhel, den(643))
    call cont_VV(nsync, wf16(:,51), wf32(:,188), A(:,392), n3(:,676), t3x512(:,:,392), nhel, den(644))
    call cont_VV(nsync, wf16(:,53), wf32(:,189), A(:,393), n3(:,677), t3x512(:,:,393), nhel, den(645))
    call cont_VV(nsync, wf16(:,50), wf32(:,187), A(:,394), n3(:,678), t3x512(:,:,394), nhel, den(646))
    call cont_VV(nsync, wf16(:,68), wf32(:,190), A(:,395), n3(:,679), t3x512(:,:,395), nhel, den(647))
    call cont_VV(nsync, wf16(:,65), wf32(:,190), A(:,396), n3(:,680), t3x512(:,:,396), nhel, den(648))
    call cont_VV(nsync, wf16(:,63), wf32(:,191), A(:,397), n3(:,681), t3x512(:,:,397), nhel, den(649))
    call cont_VV(nsync, wf16(:,65), wf32(:,192), A(:,398), n3(:,682), t3x512(:,:,398), nhel, den(650))
    call cont_VV(nsync, wf16(:,66), wf32(:,191), A(:,399), n3(:,683), t3x512(:,:,399), nhel, den(651))
    call cont_VV(nsync, wf16(:,68), wf32(:,192), A(:,400), n3(:,684), t3x512(:,:,400), nhel, den(652))
    call cont_VV(nsync, wf16(:,48), wf32(:,193), A(:,401), n3(:,685), t3x512(:,:,401), nhel, den(653))
    call cont_VV(nsync, wf16(:,50), wf32(:,194), A(:,402), n3(:,686), t3x512(:,:,402), nhel, den(654))
    call cont_VV(nsync, wf16(:,51), wf32(:,193), A(:,403), n3(:,687), t3x512(:,:,403), nhel, den(655))
    call cont_VV(nsync, wf16(:,53), wf32(:,194), A(:,404), n3(:,688), t3x512(:,:,404), nhel, den(656))
    call cont_VV(nsync, wf16(:,53), wf32(:,195), A(:,405), n3(:,689), t3x512(:,:,405), nhel, den(657))
    call cont_VV(nsync, wf16(:,50), wf32(:,195), A(:,406), n3(:,690), t3x512(:,:,406), nhel, den(658))
    call cont_VV(nsync, wf16(:,68), wf32(:,196), A(:,407), n3(:,691), t3x512(:,:,407), nhel, den(659))
    call cont_VV(nsync, wf16(:,63), wf32(:,197), A(:,408), n3(:,692), t3x512(:,:,408), nhel, den(660))
    call cont_VV(nsync, wf16(:,65), wf32(:,198), A(:,409), n3(:,693), t3x512(:,:,409), nhel, den(661))
    call cont_VV(nsync, wf16(:,66), wf32(:,197), A(:,410), n3(:,694), t3x512(:,:,410), nhel, den(662))
    call cont_VV(nsync, wf16(:,68), wf32(:,198), A(:,411), n3(:,695), t3x512(:,:,411), nhel, den(663))
    call cont_VV(nsync, wf16(:,65), wf32(:,196), A(:,412), n3(:,696), t3x512(:,:,412), nhel, den(664))
    call cont_QA(nsync, wf16(:,77), wf32(:,199), A(:,413), n3(:,697), t3x512(:,:,413), nhel, den(665))
    call cont_QA(nsync, wf16(:,103), wf32(:,200), A(:,414), n3(:,698), t3x512(:,:,414), nhel, den(666))
    call cont_VV(nsync, wf16(:,71), wf32(:,190), A(:,415), n3(:,699), t3x512(:,:,415), nhel, den(667))
    call cont_QA(nsync, wf16(:,75), wf32(:,201), A(:,416), n3(:,700), t3x512(:,:,416), nhel, den(668))
    call cont_QA(nsync, wf16(:,77), wf32(:,202), A(:,417), n3(:,701), t3x512(:,:,417), nhel, den(669))
    call cont_VV(nsync, wf32(:,155), wf16(:,95), A(:,418), n3(:,702), t3x512(:,:,418), nhel, den(670))
    call cont_VV(nsync, wf32(:,156), wf16(:,97), A(:,419), n3(:,703), t3x512(:,:,419), nhel, den(671))
    call cont_VV(nsync, wf16(:,73), wf32(:,192), A(:,420), n3(:,704), t3x512(:,:,420), nhel, den(672))
    call cont_VV(nsync, wf16(:,69), wf32(:,191), A(:,421), n3(:,705), t3x512(:,:,421), nhel, den(673))
    call cont_VV(nsync, wf16(:,71), wf32(:,192), A(:,422), n3(:,706), t3x512(:,:,422), nhel, den(674))
    call cont_QA(nsync, wf16(:,81), wf32(:,199), A(:,423), n3(:,707), t3x512(:,:,423), nhel, den(675))
    call cont_QA(nsync, wf16(:,107), wf32(:,200), A(:,424), n3(:,708), t3x512(:,:,424), nhel, den(676))
    call cont_VV(nsync, wf16(:,71), wf32(:,196), A(:,425), n3(:,709), t3x512(:,:,425), nhel, den(677))
    call cont_QA(nsync, wf16(:,79), wf32(:,201), A(:,426), n3(:,710), t3x512(:,:,426), nhel, den(678))
    call cont_QA(nsync, wf16(:,81), wf32(:,202), A(:,427), n3(:,711), t3x512(:,:,427), nhel, den(679))
    call cont_VV(nsync, wf32(:,160), wf16(:,95), A(:,428), n3(:,712), t3x512(:,:,428), nhel, den(680))
    call cont_VV(nsync, wf32(:,161), wf16(:,97), A(:,429), n3(:,713), t3x512(:,:,429), nhel, den(681))
    call cont_VV(nsync, wf16(:,73), wf32(:,198), A(:,430), n3(:,714), t3x512(:,:,430), nhel, den(682))
    call cont_VV(nsync, wf16(:,69), wf32(:,197), A(:,431), n3(:,715), t3x512(:,:,431), nhel, den(683))
    call cont_VV(nsync, wf16(:,71), wf32(:,198), A(:,432), n3(:,716), t3x512(:,:,432), nhel, den(684))
    call cont_QA(nsync, wf16(:,90), wf32(:,203), A(:,433), n3(:,717), t3x512(:,:,433), nhel, den(685))
    call cont_QA(nsync, wf16(:,116), wf32(:,204), A(:,434), n3(:,718), t3x512(:,:,434), nhel, den(686))
    call cont_VV(nsync, wf16(:,84), wf32(:,187), A(:,435), n3(:,719), t3x512(:,:,435), nhel, den(687))
    call cont_QA(nsync, wf16(:,88), wf32(:,205), A(:,436), n3(:,720), t3x512(:,:,436), nhel, den(688))
    call cont_QA(nsync, wf16(:,90), wf32(:,206), A(:,437), n3(:,721), t3x512(:,:,437), nhel, den(689))
    call cont_VV(nsync, wf32(:,151), wf16(:,108), A(:,438), n3(:,722), t3x512(:,:,438), nhel, den(690))
    call cont_VV(nsync, wf32(:,152), wf16(:,110), A(:,439), n3(:,723), t3x512(:,:,439), nhel, den(691))
    call cont_VV(nsync, wf16(:,86), wf32(:,189), A(:,440), n3(:,724), t3x512(:,:,440), nhel, den(692))
    call cont_VV(nsync, wf16(:,82), wf32(:,188), A(:,441), n3(:,725), t3x512(:,:,441), nhel, den(693))
    call cont_VV(nsync, wf16(:,84), wf32(:,189), A(:,442), n3(:,726), t3x512(:,:,442), nhel, den(694))
    call cont_QA(nsync, wf16(:,120), wf32(:,204), A(:,443), n3(:,727), t3x512(:,:,443), nhel, den(695))
    call cont_QA(nsync, wf16(:,94), wf32(:,203), A(:,444), n3(:,728), t3x512(:,:,444), nhel, den(696))
    call cont_QA(nsync, wf16(:,92), wf32(:,205), A(:,445), n3(:,729), t3x512(:,:,445), nhel, den(697))
    call cont_VV(nsync, wf16(:,84), wf32(:,195), A(:,446), n3(:,730), t3x512(:,:,446), nhel, den(698))
    call cont_VV(nsync, wf16(:,86), wf32(:,194), A(:,447), n3(:,731), t3x512(:,:,447), nhel, den(699))
    call cont_VV(nsync, wf16(:,82), wf32(:,193), A(:,448), n3(:,732), t3x512(:,:,448), nhel, den(700))
    call cont_VV(nsync, wf16(:,84), wf32(:,194), A(:,449), n3(:,733), t3x512(:,:,449), nhel, den(701))
    call cont_QA(nsync, wf16(:,94), wf32(:,206), A(:,450), n3(:,734), t3x512(:,:,450), nhel, den(702))
    call cont_VV(nsync, wf32(:,157), wf16(:,108), A(:,451), n3(:,735), t3x512(:,:,451), nhel, den(703))
    call cont_VV(nsync, wf32(:,158), wf16(:,110), A(:,452), n3(:,736), t3x512(:,:,452), nhel, den(704))

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
  complex(REALKIND), intent(out) :: M1(2) ! M1helarray(2,512)
  integer :: empty(0)

  M1(1) = (A(j,49)%j+A(j,52)%j+A(j,67)%j+A(j,70)%j+A(j,81)%j+A(j,82)%j+A(j,85)%j+A(j,86)%j+A(j,95)%j+A(j,104)%j+A(j,151)%j &
       +A(j,158)%j+A(j,275)%j+A(j,276)%j+A(j,289)%j+A(j,290)%j+A(j,367)%j+A(j,368)%j+A(j,381)%j+A(j,382)%j)*f(1)+(A(j,3)%j &
       +A(j,4)%j)*f(2)+(A(j,57)%j+A(j,60)%j+A(j,73)%j+A(j,76)%j)*f(3)+(-A(j,55)%j-A(j,58)%j-A(j,71)%j-A(j,74)%j)*f(4)+(A(j,51)%j &
       +A(j,54)%j+A(j,62)%j+A(j,64)%j+A(j,66)%j+A(j,69)%j+A(j,78)%j+A(j,80)%j+A(j,84)%j+A(j,88)%j+A(j,90)%j+A(j,92)%j+A(j,97)%j &
       +A(j,108)%j+A(j,112)%j+A(j,116)%j+A(j,153)%j+A(j,162)%j+A(j,164)%j+A(j,170)%j+A(j,269)%j+A(j,272)%j+A(j,273)%j+A(j,274)%j &
       +A(j,282)%j+A(j,286)%j+A(j,287)%j+A(j,288)%j+A(j,360)%j+A(j,363)%j+A(j,365)%j+A(j,366)%j+A(j,371)%j+A(j,377)%j+A(j,379)%j &
       +A(j,380)%j)*f(5)+(A(j,98)%j+A(j,113)%j+A(j,145)%j+A(j,149)%j+A(j,199)%j+A(j,207)%j)*f(6)+(-A(j,93)%j-A(j,100)%j-A(j,109)%j &
       -A(j,117)%j-A(j,147)%j-A(j,154)%j-A(j,156)%j-A(j,165)%j-A(j,203)%j-A(j,211)%j-A(j,215)%j-A(j,223)%j)*f(7)+(-A(j,61)%j &
       -A(j,63)%j-A(j,65)%j-A(j,68)%j-A(j,83)%j-A(j,89)%j-A(j,96)%j-A(j,111)%j-A(j,152)%j-A(j,163)%j+A(j,266)%j+A(j,277)%j &
       +A(j,291)%j+A(j,297)%j+A(j,309)%j+A(j,357)%j+A(j,372)%j+A(j,384)%j+A(j,390)%j+A(j,401)%j)*f(8)+(A(j,102)%j+A(j,105)%j &
       +A(j,159)%j+A(j,167)%j+A(j,219)%j+A(j,227)%j)*f(9)+(A(j,50)%j+A(j,53)%j+A(j,77)%j+A(j,79)%j+A(j,87)%j+A(j,91)%j+A(j,107)%j &
       +A(j,115)%j+A(j,161)%j+A(j,169)%j-A(j,263)%j-A(j,283)%j-A(j,295)%j-A(j,301)%j-A(j,312)%j-A(j,355)%j-A(j,374)%j-A(j,386)%j &
       -A(j,392)%j-A(j,403)%j)*f(10)+(A(j,56)%j+A(j,59)%j+A(j,72)%j+A(j,75)%j+A(j,94)%j+A(j,99)%j+A(j,101)%j+A(j,103)%j+A(j,106)%j &
       +A(j,110)%j+A(j,114)%j+A(j,118)%j+A(j,146)%j+A(j,148)%j+A(j,150)%j+A(j,155)%j+A(j,157)%j+A(j,160)%j+A(j,166)%j+A(j,168)%j &
       +A(j,200)%j+A(j,204)%j+A(j,208)%j+A(j,212)%j+A(j,216)%j+A(j,220)%j+A(j,224)%j+A(j,228)%j+A(j,264)%j+A(j,265)%j+A(j,267)%j &
       -A(j,268)%j+A(j,270)%j-A(j,271)%j+A(j,278)%j+A(j,279)%j+A(j,280)%j-A(j,281)%j+A(j,284)%j-A(j,285)%j+A(j,292)%j+A(j,293)%j &
       +A(j,294)%j+A(j,296)%j+A(j,298)%j+A(j,299)%j+A(j,300)%j+A(j,302)%j+A(j,310)%j+A(j,311)%j+A(j,313)%j+A(j,314)%j+A(j,356)%j &
       +A(j,358)%j-A(j,359)%j+A(j,361)%j-A(j,362)%j+A(j,364)%j+A(j,369)%j-A(j,370)%j+A(j,373)%j+A(j,375)%j-A(j,376)%j+A(j,378)%j &
       +A(j,383)%j+A(j,385)%j+A(j,387)%j+A(j,388)%j+A(j,389)%j+A(j,391)%j+A(j,393)%j+A(j,394)%j+A(j,402)%j+A(j,404)%j+A(j,405)%j &
       +A(j,406)%j+A(j,433)%j+A(j,434)%j+A(j,435)%j+A(j,436)%j+A(j,437)%j+A(j,438)%j+A(j,439)%j+A(j,440)%j+A(j,441)%j+A(j,442)%j &
       +A(j,443)%j+A(j,444)%j+A(j,445)%j+A(j,446)%j+A(j,447)%j+A(j,448)%j+A(j,449)%j+A(j,450)%j+A(j,451)%j+A(j,452)%j)*f(11)
  M1(2) = (A(j,7)%j+A(j,10)%j+A(j,23)%j+A(j,26)%j+A(j,37)%j+A(j,38)%j+A(j,41)%j+A(j,42)%j+A(j,119)%j+A(j,126)%j+A(j,173)%j &
       +A(j,182)%j+A(j,241)%j+A(j,242)%j+A(j,255)%j+A(j,256)%j+A(j,333)%j+A(j,334)%j+A(j,347)%j+A(j,348)%j)*f(1)+(A(j,1)%j &
       +A(j,2)%j)*f(2)+(A(j,13)%j+A(j,18)%j+A(j,29)%j+A(j,34)%j)*f(3)+(-A(j,11)%j-A(j,16)%j-A(j,27)%j-A(j,32)%j)*f(4)+(A(j,6)%j &
       +A(j,9)%j+A(j,15)%j+A(j,20)%j+A(j,22)%j+A(j,25)%j+A(j,31)%j+A(j,36)%j+A(j,40)%j+A(j,44)%j+A(j,46)%j+A(j,48)%j+A(j,123)%j &
       +A(j,132)%j+A(j,138)%j+A(j,144)%j+A(j,177)%j+A(j,188)%j+A(j,192)%j+A(j,196)%j+A(j,235)%j+A(j,238)%j+A(j,239)%j+A(j,240)%j &
       +A(j,249)%j+A(j,252)%j+A(j,253)%j+A(j,254)%j+A(j,326)%j+A(j,329)%j+A(j,331)%j+A(j,332)%j+A(j,340)%j+A(j,343)%j+A(j,345)%j &
       +A(j,346)%j)*f(5)+(A(j,124)%j+A(j,139)%j+A(j,171)%j+A(j,178)%j+A(j,197)%j+A(j,205)%j)*f(6)+(-A(j,120)%j-A(j,129)%j &
       -A(j,133)%j-A(j,141)%j-A(j,174)%j-A(j,180)%j-A(j,183)%j-A(j,189)%j-A(j,201)%j-A(j,209)%j-A(j,213)%j-A(j,221)%j)*f(7)+( &
       -A(j,14)%j-A(j,19)%j-A(j,21)%j-A(j,24)%j-A(j,39)%j-A(j,45)%j-A(j,122)%j-A(j,137)%j-A(j,176)%j-A(j,191)%j+A(j,232)%j &
       +A(j,243)%j+A(j,257)%j+A(j,304)%j+A(j,315)%j+A(j,323)%j+A(j,335)%j+A(j,349)%j+A(j,397)%j+A(j,408)%j)*f(8)+(A(j,127)%j &
       +A(j,135)%j+A(j,185)%j+A(j,193)%j+A(j,217)%j+A(j,225)%j)*f(9)+(A(j,5)%j+A(j,8)%j+A(j,30)%j+A(j,35)%j+A(j,43)%j+A(j,47)%j &
       +A(j,131)%j+A(j,143)%j+A(j,187)%j+A(j,195)%j-A(j,229)%j-A(j,246)%j-A(j,260)%j-A(j,307)%j-A(j,319)%j-A(j,321)%j-A(j,337)%j &
       -A(j,351)%j-A(j,399)%j-A(j,410)%j)*f(10)+(A(j,12)%j+A(j,17)%j+A(j,28)%j+A(j,33)%j+A(j,121)%j+A(j,125)%j+A(j,128)%j &
       +A(j,130)%j+A(j,134)%j+A(j,136)%j+A(j,140)%j+A(j,142)%j+A(j,172)%j+A(j,175)%j+A(j,179)%j+A(j,181)%j+A(j,184)%j+A(j,186)%j &
       +A(j,190)%j+A(j,194)%j+A(j,198)%j+A(j,202)%j+A(j,206)%j+A(j,210)%j+A(j,214)%j+A(j,218)%j+A(j,222)%j+A(j,226)%j+A(j,230)%j &
       +A(j,231)%j+A(j,233)%j-A(j,234)%j+A(j,236)%j-A(j,237)%j+A(j,244)%j+A(j,245)%j+A(j,247)%j-A(j,248)%j+A(j,250)%j-A(j,251)%j &
       +A(j,258)%j+A(j,259)%j+A(j,261)%j+A(j,262)%j+A(j,303)%j+A(j,305)%j+A(j,306)%j+A(j,308)%j+A(j,316)%j+A(j,317)%j+A(j,318)%j &
       +A(j,320)%j+A(j,322)%j+A(j,324)%j-A(j,325)%j+A(j,327)%j-A(j,328)%j+A(j,330)%j+A(j,336)%j+A(j,338)%j-A(j,339)%j+A(j,341)%j &
       -A(j,342)%j+A(j,344)%j+A(j,350)%j+A(j,352)%j+A(j,353)%j+A(j,354)%j+A(j,395)%j+A(j,396)%j+A(j,398)%j+A(j,400)%j+A(j,407)%j &
       +A(j,409)%j+A(j,411)%j+A(j,412)%j+A(j,413)%j+A(j,414)%j+A(j,415)%j+A(j,416)%j+A(j,417)%j+A(j,418)%j+A(j,419)%j+A(j,420)%j &
       +A(j,421)%j+A(j,422)%j+A(j,423)%j+A(j,424)%j+A(j,425)%j+A(j,426)%j+A(j,427)%j+A(j,428)%j+A(j,429)%j+A(j,430)%j+A(j,431)%j &
       +A(j,432)%j)*f(11)

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
  use ol_colourmatrix_ppllnnjj_vbs_nenmexmxuxcxdsg_2_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(2)
  real(REALKIND),    intent(out) :: M2colint(0:47-1)
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
subroutine colint2(M1, M2, M2colint, extcombs)
! M1(i)   = <M1|Ci> colour component of matrix element
! M2(i)   = <M2|Ci> colour component of matrix element
! COLINT = <M|M>
!        = Sum_{i,j} <M1|Ci> * <Ci|Cj> * <Cj|M2>
!        = colour-summed squared matrix element
! K1(i,j) = <Ci|Cj>
! M2colint is an array which contains the colour interference for each colour matrix
! The elements of the array extcombs specifies for which external particle
! combinations the colour correlations will be calculated. For particles i,j: i*(i-1)/2+j
! i=j=0 -> 0 means no colour insertion.
! **********************************************************************
  use ol_colourmatrix_ppllnnjj_vbs_nenmexmxuxcxdsg_2_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(2), M2(2)
  real(REALKIND),    intent(out) :: M2colint(0:47-1)
  integer,           intent(in)  :: extcombs(:)
  integer :: extcomb, colmatpos, i, j, eco

  M2colint = 0

  do eco = 1, size(extcombs)
    extcomb = extcombs(eco)
    colmatpos = 2*extcomb
    do i = 1, 2
      do j = 1, 2
        M2colint(extcomb) = M2colint(extcomb) + real(conjg(M1(i))*K1(i+colmatpos,j)*M2(j))
      end do
    end do
  end do

end subroutine colint2


! **********************************************************************
subroutine colintmunu(M1, M2, M2colint)
! M1(i)    = <M1|Ci> colour component of matrix element
! M2(i)    = <M2|Ci> colour component of matrix element
! M2colint = <M1|M2>
!          = Sum_{i,j} <M1|Ci> * <Ci|Cj> * <Cj|M2>
!          = colour-summed squared matrix element
! K1(i,j) = <Ci|Cj>
! **********************************************************************
  use ol_colourmatrix_ppllnnjj_vbs_nenmexmxuxcxdsg_2_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(2)
  complex(REALKIND), intent(in)  :: M2(2)
  complex(REALKIND),    intent(out) :: M2colint
  integer :: i, j

  M2colint = 0

  do i = 1, 2
    do j = 1, 2
      M2colint = M2colint + M1(i)*K1(i,j)*conjg(M2(j))
    end do
  end do

end subroutine colintmunu

end subroutine amp2


#ifdef PRECISION_dp
subroutine colourvector(M1out, nhelout) &
  & bind(c,name="ol_tree_colvect_ppllnnjj_vbs_nenmexmxuxcxdsg_2")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(2,512)
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
    & bind(c,name="ol_f_amp2tree_ppllnnjj_vbs_nenmexmxuxcxdsg_2")
#else
subroutine amp2tree(P, M2)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix element without fuss.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,9)
  real(REALKIND),  intent(out) :: M2
  real(REALKIND) :: M2tmp(0:47-1)
  real(REALKIND) :: M2munu(4,4)
  call amp2(P, M2tmp, 0, [ 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND ], 1, [ 0 ], M2munu)
  M2 = M2tmp(0)
end subroutine amp2tree


#ifdef PRECISION_dp
subroutine amp2ccone(P, M2, I, J) &
    & bind(c,name="ol_f_amp2ccone_ppllnnjj_vbs_nenmexmxuxcxdsg_2")
#else
subroutine amp2ccone(P, M2, I, J)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix for the colour correlation matrix for particles I and J.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,9)
  real(REALKIND),  intent(out) :: M2
  integer,         intent(in)  :: I, J
  integer        :: extcomb
  real(REALKIND) :: M2tmp(0:47-1)
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
    & bind(c,name="ol_f_amp2ccall_ppllnnjj_vbs_nenmexmxuxcxdsg_2")
#else
subroutine amp2ccall(P, M2)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix for all colour correlation matrices.
  ! The correlation between particles i and j is at position i*(i-1)/2+j of the array M2.
  ! M2(0) is AMP2tree
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,9)
  real(REALKIND),  intent(out) :: M2(0:47-1)
  real(REALKIND) :: M2munu(4,4)
  integer :: k
  call amp2(P, M2, 0, [ 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND ], &
    47, [ (k, k = 0, 47-1) ], M2munu)
end subroutine amp2ccall


#ifdef PRECISION_dp
subroutine amp2hcone(P, M2, I, J, MOM) &
    & bind(c,name="ol_f_amp2hcone_ppllnnjj_vbs_nenmexmxuxcxdsg_2")
#else
subroutine amp2hcone(P, M2, I, J, MOM)
#endif
  ! This is an interface for AMP2.
  ! Calculates the helicity correlation for emitter I with momentum MOM and spectator J
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,9)
  real(REALKIND),  intent(out) :: M2
  integer,         intent(in)  :: I, J
  real(REALKIND),  intent(in)  :: MOM(0:3)
  integer        :: extcomb
  real(REALKIND) :: M2tmp(0:47-1)
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
    & bind(c,name="ol_f_amp2hcall_ppllnnjj_vbs_nenmexmxuxcxdsg_2")
#else
subroutine amp2hcall(P, M2, I, MOM)
#endif
  ! This is an interface for AMP2.
  ! Calculates all helicity correlations for emitter I with momentum MOM.
  ! The correlator for spectator j is at position j of the array M2.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,9)
  real(REALKIND),  intent(out) :: M2(9)
  integer,         intent(in)  :: I
  real(REALKIND),  intent(in)  :: MOM(0:3)
  real(REALKIND) :: M2tmp(0:47-1)
  real(REALKIND) :: M2munu(4,4)
  integer        :: J, extcombs(9)
  do J = 1, 9
    if (J <= I) then
      extcombs(J) = I*(I-1)/2 + J
    else
      extcombs(J) = J*(J-1)/2 + I
    end if
  end do
  call amp2(P, M2tmp, I, MOM, 9,extcombs, M2munu)
  do J = 1, 9
    M2(J) = M2tmp(extcombs(J))
  end do
end subroutine amp2hcall


#ifdef PRECISION_dp

subroutine amp2tree_c(p, m2) &
    & bind(c,name="ol_amp2tree_ppllnnjj_vbs_nenmexmxuxcxdsg_2")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,9)
  real(c_double), intent(out) :: m2
  real(DREALKIND) :: f_p(0:3,9)
  real(DREALKIND) :: f_m2
  f_p = p
  call amp2tree(f_p, f_m2)
  m2 = f_m2
end subroutine amp2tree_c

subroutine amp2ccone_c(p, m2, i, j) &
    & bind(c,name="ol_amp2ccone_ppllnnjj_vbs_nenmexmxuxcxdsg_2")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,9)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(DREALKIND) :: f_p(0:3,9)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  f_p = p
  f_i = i
  f_j = j
  call amp2ccone(f_p, f_m2, f_i, f_j)
  m2 = f_m2
end subroutine amp2ccone_c

subroutine amp2ccall_c(p, m2) &
    & bind(c,name="ol_amp2ccall_ppllnnjj_vbs_nenmexmxuxcxdsg_2")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,9)
  real(c_double), intent(out) :: m2(0:47-1)
  real(DREALKIND) :: f_p(0:3,9)
  real(DREALKIND) :: f_m2(0:47-1)
  f_p = p
  call amp2ccall(f_p, f_m2)
  m2 = f_m2
end subroutine amp2ccall_c

subroutine amp2hcone_c(p, m2, i, j, mom) &
    & bind(c,name="ol_amp2hcone_ppllnnjj_vbs_nenmexmxuxcxdsg_2")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,9)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,9)
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
    & bind(c,name="ol_amp2hcall_ppllnnjj_vbs_nenmexmxuxcxdsg_2")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,9)
  real(c_double), intent(out) :: m2(9)
  integer(c_int), intent(in) :: i
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,9)
  real(DREALKIND) :: f_m2(9)
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
    & bind(c,name="amp2tree_ppllnnjj_vbs_nenmexmxuxcxdsg_2_")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,9)
  real(c_double), intent(out) :: m2
  real(DREALKIND) :: f_p(0:3,9)
  real(DREALKIND) :: f_m2
  f_p = p
  call amp2tree(f_p, f_m2)
  m2 = f_m2
end subroutine amp2tree_legacy

subroutine amp2ccone_legacy(p, m2, i, j) &
    & bind(c,name="amp2ccone_ppllnnjj_vbs_nenmexmxuxcxdsg_2_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,9)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(DREALKIND) :: f_p(0:3,9)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  f_p = p
  f_i = i
  f_j = j
  call amp2ccone(f_p, f_m2, f_i, f_j)
  m2 = f_m2
end subroutine amp2ccone_legacy

subroutine amp2ccall_legacy(p, m2) &
    & bind(c,name="amp2ccall_ppllnnjj_vbs_nenmexmxuxcxdsg_2_")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,9)
  real(c_double), intent(out) :: m2(0:47-1)
  real(DREALKIND) :: f_p(0:3,9)
  real(DREALKIND) :: f_m2(0:47-1)
  f_p = p
  call amp2ccall(f_p, f_m2)
  m2 = f_m2
end subroutine amp2ccall_legacy

subroutine amp2hcone_legacy(p, m2, i, j, mom) &
    & bind(c,name="amp2hcone_ppllnnjj_vbs_nenmexmxuxcxdsg_2_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,9)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,9)
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
    & bind(c,name="amp2hcall_ppllnnjj_vbs_nenmexmxuxcxdsg_2_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,9)
  real(c_double), intent(out) :: m2(9)
  integer(c_int), intent(in) :: i
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,9)
  real(DREALKIND) :: f_m2(9)
  integer :: f_i
  real(DREALKIND) :: f_mom(0:3)
  f_p = p
  f_i = i
  f_mom = mom
  call amp2hcall(f_p, f_m2, f_i, f_mom)
  m2 = f_m2
end subroutine amp2hcall_legacy

#endif

end module ol_tree_ppllnnjj_vbs_nenmexmxuxcxdsg_2_/**/REALKIND
