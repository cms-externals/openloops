
module ol_tree_pphhjj_bbbxbxhh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(22)
  complex(REALKIND), save :: den(345)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 16 ! number of helicity configurations
  integer(intkind2), save :: nhel = 16 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(16) ! physical helicity states
  complex(DREALKIND) :: M1helarr(2,16) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f( 1) = (CI*eQED**4)/(4._/**/REALKIND*cw**2*sw**2)
    f( 2) = (CI*eQED**4)/(2._/**/REALKIND*cw**2*sw**2)
    f( 3) = (3*CI*eQED**4*lambdaHHH*lambdaHZZ*MH**2)/(2._/**/REALKIND*cw**2*sw**2)
    f( 4) = (CI*eQED**4*lambdaHZZ**2*MW**2)/(cw**4*sw**2)
    f( 5) = (CI*eQED**4*lambdaHZZ*YB)/(4._/**/REALKIND*cw**3*sw**3)
    f( 6) = (CI*eQED**4*MH**2*YB)/(8._/**/REALKIND*cw*MW**2*sw**3)
    f( 7) = (3*CI*eQED**4*lambdaHHH*MH**2*YB)/(8._/**/REALKIND*cw*MW**2*sw**3)
    f( 8) = (CI*eQED**4*lambdaHZZ*YB)/(2._/**/REALKIND*cw**2*sw**2)
    f( 9) = (CI*eQED**4*lambdaHHH*MH**2*YB)/(12._/**/REALKIND*MW**2*sw**2)
    f(10) = (3*CI*eQED**4*lambdaHHH*MH**2*YB)/(4._/**/REALKIND*MW**2*sw**2)
    f(11) = (CI*eQED**4*MH**2*YB**2)/(16._/**/REALKIND*MW**4*sw**4)
    f(12) = (3*CI*eQED**4*MH**2*YB**2)/(16._/**/REALKIND*MW**4*sw**4)
    f(13) = (CI*eQED**4*MH**4*YB**2)/(16._/**/REALKIND*MW**4*sw**4)
    f(14) = (3*CI*eQED**4*lambdaHHH*MH**4*YB**2)/(16._/**/REALKIND*MW**4*sw**4)
    f(15) = (9*CI*eQED**4*lambdaHHH**2*MH**4*YB**2)/(16._/**/REALKIND*MW**4*sw**4)
    f(16) = (CI*eQED**4*YB**2)/(16._/**/REALKIND*cw**2*MW**2*sw**4)
    f(17) = (CI*eQED**4*YB**2)/(8._/**/REALKIND*cw*MW**2*sw**3)
    f(18) = (CI*eQED**4*YB**2)/(36._/**/REALKIND*MW**2*sw**2)
    f(19) = (CI*eQED**4*YB**2)/(4._/**/REALKIND*MW**2*sw**2)
    f(20) = (CI*eQED**4*MH**2*YB**3)/(16._/**/REALKIND*MW**4*sw**4)
    f(21) = (3*CI*eQED**4*lambdaHHH*MH**2*YB**3)/(16._/**/REALKIND*MW**4*sw**4)
    f(22) = (CI*eQED**4*YB**4)/(16._/**/REALKIND*MW**4*sw**4)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,5) - MH2)
  den(2) = 1 / (Q(5,10) - MH2)
  den(4) = 1 / (Q(5,5) - MZ2)
  den(5) = 1 / (Q(5,10) - MZ2)
  den(7) = 1 / (Q(5,9) - MH2)
  den(8) = 1 / (Q(5,6) - MH2)
  den(10) = 1 / (Q(5,9) - MZ2)
  den(11) = 1 / (Q(5,6) - MZ2)
  den(13) = 1 / (Q(5,48) - MH2)
  den(16) = 1 / (Q(5,21) - MH2)
  den(19) = 1 / (Q(5,21) - MZ2)
  den(22) = 1 / (Q(5,26) - MH2)
  den(25) = 1 / (Q(5,26) - MZ2)
  den(28) = 1 / (Q(5,18) - MB2)
  den(29) = 1 / (Q(5,40) - MB2)
  den(34) = 1 / (Q(5,5))
  den(37) = 1 / (Q(5,13) - MB2)
  den(48) = 1 / (Q(5,34) - MB2)
  den(49) = 1 / (Q(5,24) - MB2)
  den(56) = 1 / (Q(5,7) - MB2)
  den(70) = 1 / (Q(5,42) - MH2)
  den(73) = 1 / (Q(5,42) - MZ2)
  den(86) = 1 / (Q(5,50) - MB2)
  den(93) = 1 / (Q(5,25) - MH2)
  den(96) = 1 / (Q(5,25) - MZ2)
  den(99) = 1 / (Q(5,22) - MH2)
  den(102) = 1 / (Q(5,22) - MZ2)
  den(105) = 1 / (Q(5,17) - MB2)
  den(110) = 1 / (Q(5,6))
  den(117) = 1 / (Q(5,14) - MB2)
  den(124) = 1 / (Q(5,33) - MB2)
  den(141) = 1 / (Q(5,41) - MH2)
  den(144) = 1 / (Q(5,41) - MZ2)
  den(160) = 1 / (Q(5,49) - MB2)
  den(165) = 1 / (Q(5,36) - MB2)
  den(170) = 1 / (Q(5,9))
  den(183) = 1 / (Q(5,20) - MB2)
  den(190) = 1 / (Q(5,11) - MB2)
  den(204) = 1 / (Q(5,38) - MH2)
  den(207) = 1 / (Q(5,38) - MZ2)
  den(227) = 1 / (Q(5,10))
  den(256) = 1 / (Q(5,37) - MH2)
  den(259) = 1 / (Q(5,37) - MZ2)
  den(280) = 1 / (Q(5,21))
  den(285) = 1 / (Q(5,38))
  den(294) = 1 / (Q(5,42))
  den(299) = 1 / (Q(5,37))
  den(304) = 1 / (Q(5,22))
  den(313) = 1 / (Q(5,41))
  den(332) = 1 / (Q(5,26))
  den(337) = 1 / (Q(5,25))

  ! denominators

  den(3) = den(1)*den(2)
  den(6) = den(4)*den(5)
  den(9) = den(7)*den(8)
  den(12) = den(10)*den(11)
  den(14) = den(3)*den(13)
  den(15) = den(6)*den(13)
  den(17) = den(1)*den(16)
  den(18) = den(2)*den(17)
  den(20) = den(4)*den(19)
  den(21) = den(5)*den(20)
  den(23) = den(2)*den(22)
  den(24) = den(1)*den(23)
  den(26) = den(5)*den(25)
  den(27) = den(4)*den(26)
  den(30) = den(1)*den(28)
  den(31) = den(29)*den(30)
  den(32) = den(4)*den(28)
  den(33) = den(29)*den(32)
  den(35) = den(28)*den(34)
  den(36) = den(29)*den(35)
  den(38) = den(1)*den(37)
  den(39) = den(28)*den(38)
  den(40) = den(4)*den(37)
  den(41) = den(28)*den(40)
  den(42) = den(34)*den(37)
  den(43) = den(28)*den(42)
  den(44) = den(22)*den(28)
  den(45) = den(1)*den(44)
  den(46) = den(25)*den(28)
  den(47) = den(4)*den(46)
  den(50) = den(1)*den(48)
  den(51) = den(49)*den(50)
  den(52) = den(4)*den(48)
  den(53) = den(49)*den(52)
  den(54) = den(34)*den(48)
  den(55) = den(49)*den(54)
  den(57) = den(1)*den(56)
  den(58) = den(49)*den(57)
  den(59) = den(4)*den(56)
  den(60) = den(49)*den(59)
  den(61) = den(34)*den(56)
  den(62) = den(49)*den(61)
  den(63) = den(22)*den(49)
  den(64) = den(1)*den(63)
  den(65) = den(25)*den(49)
  den(66) = den(4)*den(65)
  den(67) = den(38)*den(48)
  den(68) = den(40)*den(48)
  den(69) = den(42)*den(48)
  den(71) = den(48)*den(70)
  den(72) = den(1)*den(71)
  den(74) = den(48)*den(73)
  den(75) = den(4)*den(74)
  den(76) = den(29)*den(57)
  den(77) = den(29)*den(59)
  den(78) = den(29)*den(61)
  den(79) = den(29)*den(70)
  den(80) = den(1)*den(79)
  den(81) = den(29)*den(73)
  den(82) = den(4)*den(81)
  den(83) = den(13)*den(57)
  den(84) = den(13)*den(59)
  den(85) = den(13)*den(61)
  den(87) = den(13)*den(86)
  den(88) = den(1)*den(87)
  den(89) = den(4)*den(87)
  den(90) = den(34)*den(87)
  den(91) = den(9)*den(13)
  den(92) = den(12)*den(13)
  den(94) = den(7)*den(93)
  den(95) = den(8)*den(94)
  den(97) = den(10)*den(96)
  den(98) = den(11)*den(97)
  den(100) = den(8)*den(99)
  den(101) = den(7)*den(100)
  den(103) = den(11)*den(102)
  den(104) = den(10)*den(103)
  den(106) = den(8)*den(105)
  den(107) = den(29)*den(106)
  den(108) = den(11)*den(105)
  den(109) = den(29)*den(108)
  den(111) = den(105)*den(110)
  den(112) = den(29)*den(111)
  den(113) = den(93)*den(105)
  den(114) = den(8)*den(113)
  den(115) = den(96)*den(105)
  den(116) = den(11)*den(115)
  den(118) = den(8)*den(117)
  den(119) = den(105)*den(118)
  den(120) = den(11)*den(117)
  den(121) = den(105)*den(120)
  den(122) = den(110)*den(117)
  den(123) = den(105)*den(122)
  den(125) = den(8)*den(124)
  den(126) = den(49)*den(125)
  den(127) = den(11)*den(124)
  den(128) = den(49)*den(127)
  den(129) = den(110)*den(124)
  den(130) = den(49)*den(129)
  den(131) = den(8)*den(56)
  den(132) = den(49)*den(131)
  den(133) = den(11)*den(56)
  den(134) = den(49)*den(133)
  den(135) = den(56)*den(110)
  den(136) = den(49)*den(135)
  den(137) = den(49)*den(93)
  den(138) = den(8)*den(137)
  den(139) = den(49)*den(96)
  den(140) = den(11)*den(139)
  den(142) = den(124)*den(141)
  den(143) = den(8)*den(142)
  den(145) = den(124)*den(144)
  den(146) = den(11)*den(145)
  den(147) = den(118)*den(124)
  den(148) = den(120)*den(124)
  den(149) = den(122)*den(124)
  den(150) = den(29)*den(131)
  den(151) = den(29)*den(133)
  den(152) = den(29)*den(135)
  den(153) = den(29)*den(141)
  den(154) = den(8)*den(153)
  den(155) = den(29)*den(144)
  den(156) = den(11)*den(155)
  den(157) = den(13)*den(131)
  den(158) = den(13)*den(133)
  den(159) = den(13)*den(135)
  den(161) = den(13)*den(160)
  den(162) = den(8)*den(161)
  den(163) = den(11)*den(161)
  den(164) = den(110)*den(161)
  den(166) = den(7)*den(28)
  den(167) = den(165)*den(166)
  den(168) = den(10)*den(28)
  den(169) = den(165)*den(168)
  den(171) = den(28)*den(170)
  den(172) = den(165)*den(171)
  den(173) = den(7)*den(37)
  den(174) = den(28)*den(173)
  den(175) = den(10)*den(37)
  den(176) = den(28)*den(175)
  den(177) = den(37)*den(170)
  den(178) = den(28)*den(177)
  den(179) = den(28)*den(99)
  den(180) = den(7)*den(179)
  den(181) = den(28)*den(102)
  den(182) = den(10)*den(181)
  den(184) = den(7)*den(48)
  den(185) = den(183)*den(184)
  den(186) = den(10)*den(48)
  den(187) = den(183)*den(186)
  den(188) = den(48)*den(170)
  den(189) = den(183)*den(188)
  den(191) = den(7)*den(190)
  den(192) = den(183)*den(191)
  den(193) = den(10)*den(190)
  den(194) = den(183)*den(193)
  den(195) = den(170)*den(190)
  den(196) = den(183)*den(195)
  den(197) = den(99)*den(183)
  den(198) = den(7)*den(197)
  den(199) = den(102)*den(183)
  den(200) = den(10)*den(199)
  den(201) = den(48)*den(173)
  den(202) = den(48)*den(175)
  den(203) = den(48)*den(177)
  den(205) = den(48)*den(204)
  den(206) = den(7)*den(205)
  den(208) = den(48)*den(207)
  den(209) = den(10)*den(208)
  den(210) = den(165)*den(191)
  den(211) = den(165)*den(193)
  den(212) = den(165)*den(195)
  den(213) = den(165)*den(204)
  den(214) = den(7)*den(213)
  den(215) = den(165)*den(207)
  den(216) = den(10)*den(215)
  den(217) = den(13)*den(191)
  den(218) = den(13)*den(193)
  den(219) = den(13)*den(195)
  den(220) = den(7)*den(87)
  den(221) = den(10)*den(87)
  den(222) = den(87)*den(170)
  den(223) = den(2)*den(105)
  den(224) = den(165)*den(223)
  den(225) = den(5)*den(105)
  den(226) = den(165)*den(225)
  den(228) = den(105)*den(227)
  den(229) = den(165)*den(228)
  den(230) = den(16)*den(105)
  den(231) = den(2)*den(230)
  den(232) = den(19)*den(105)
  den(233) = den(5)*den(232)
  den(234) = den(2)*den(117)
  den(235) = den(105)*den(234)
  den(236) = den(5)*den(117)
  den(237) = den(105)*den(236)
  den(238) = den(117)*den(227)
  den(239) = den(105)*den(238)
  den(240) = den(2)*den(124)
  den(241) = den(183)*den(240)
  den(242) = den(5)*den(124)
  den(243) = den(183)*den(242)
  den(244) = den(124)*den(227)
  den(245) = den(183)*den(244)
  den(246) = den(2)*den(190)
  den(247) = den(183)*den(246)
  den(248) = den(5)*den(190)
  den(249) = den(183)*den(248)
  den(250) = den(190)*den(227)
  den(251) = den(183)*den(250)
  den(252) = den(16)*den(183)
  den(253) = den(2)*den(252)
  den(254) = den(19)*den(183)
  den(255) = den(5)*den(254)
  den(257) = den(124)*den(256)
  den(258) = den(2)*den(257)
  den(260) = den(124)*den(259)
  den(261) = den(5)*den(260)
  den(262) = den(124)*den(234)
  den(263) = den(124)*den(236)
  den(264) = den(124)*den(238)
  den(265) = den(165)*den(246)
  den(266) = den(165)*den(248)
  den(267) = den(165)*den(250)
  den(268) = den(165)*den(256)
  den(269) = den(2)*den(268)
  den(270) = den(165)*den(259)
  den(271) = den(5)*den(270)
  den(272) = den(13)*den(246)
  den(273) = den(13)*den(248)
  den(274) = den(13)*den(250)
  den(275) = den(2)*den(161)
  den(276) = den(5)*den(161)
  den(277) = den(161)*den(227)
  den(278) = den(48)*den(230)
  den(279) = den(48)*den(232)
  den(281) = den(105)*den(280)
  den(282) = den(48)*den(281)
  den(283) = den(105)*den(205)
  den(284) = den(105)*den(208)
  den(286) = den(48)*den(285)
  den(287) = den(105)*den(286)
  den(288) = den(105)*den(213)
  den(289) = den(105)*den(215)
  den(290) = den(165)*den(285)
  den(291) = den(105)*den(290)
  den(292) = den(79)*den(105)
  den(293) = den(81)*den(105)
  den(295) = den(29)*den(294)
  den(296) = den(105)*den(295)
  den(297) = den(28)*den(257)
  den(298) = den(28)*den(260)
  den(300) = den(124)*den(299)
  den(301) = den(28)*den(300)
  den(302) = den(124)*den(179)
  den(303) = den(124)*den(181)
  den(305) = den(28)*den(304)
  den(306) = den(124)*den(305)
  den(307) = den(28)*den(268)
  den(308) = den(28)*den(270)
  den(309) = den(165)*den(299)
  den(310) = den(28)*den(309)
  den(311) = den(28)*den(153)
  den(312) = den(28)*den(155)
  den(314) = den(29)*den(313)
  den(315) = den(28)*den(314)
  den(316) = den(124)*den(197)
  den(317) = den(124)*den(199)
  den(318) = den(183)*den(304)
  den(319) = den(124)*den(318)
  den(320) = den(48)*den(252)
  den(321) = den(48)*den(254)
  den(322) = den(183)*den(280)
  den(323) = den(48)*den(322)
  den(324) = den(29)*den(252)
  den(325) = den(29)*den(254)
  den(326) = den(29)*den(322)
  den(327) = den(153)*den(183)
  den(328) = den(155)*den(183)
  den(329) = den(183)*den(314)
  den(330) = den(63)*den(124)
  den(331) = den(65)*den(124)
  den(333) = den(49)*den(332)
  den(334) = den(124)*den(333)
  den(335) = den(48)*den(137)
  den(336) = den(48)*den(139)
  den(338) = den(49)*den(337)
  den(339) = den(48)*den(338)
  den(340) = den(49)*den(268)
  den(341) = den(49)*den(270)
  den(342) = den(49)*den(309)
  den(343) = den(137)*den(165)
  den(344) = den(139)*den(165)
  den(345) = den(165)*den(338)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_pphhjj_bbbxbxhh_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_pphhjj_bbbxbxhh_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for bottom bottom anti-bottom anti-bottom higgs higgs -> 0
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
  use ol_external_pphhjj_bbbxbxhh_1, only: external_perm_pphhjj_bbbxbxhh_1, &
    & external_perm_inv_pphhjj_bbbxbxhh_1, extcomb_perm_pphhjj_bbbxbxhh_1, &
    & average_factor_pphhjj_bbbxbxhh_1
  use ol_external_pphhjj_bbbxbxhh_1, only: H, hel_not_initialised, hel_init, POLSEL
  use ol_colourmatrix_pphhjj_bbbxbxhh_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_pphhjj_bbbxbxhh_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_pphhjj_bbbxbxhh_1
  use ol_kinematics_/**/REALKIND, only: LC2Std_Rep_cmplx
  implicit none

  real(DREALKIND), intent(in)  :: P_scatt(0:3,6)
  real(REALKIND),  intent(out) :: M2(0:23-1)
  real(REALKIND),  intent(out) :: M2munu(4,4)
  integer,         intent(in)  :: I
  real(REALKIND),  intent(in)  :: MOM(0:3)
  integer,         intent(in)  :: nextcombs
  integer,         intent(in)  :: extcombs(nextcombs)

  integer           :: ReplacePol, JBmunu, extcombs_permuted(nextcombs), shift, k, r, m, n
  real(REALKIND)    :: P(0:3,6)
  real(REALKIND)    :: extmasses2(6)
  real(REALKIND)    :: M2add(0:23-1)
  complex(REALKIND) :: M2munuadd(2)
  complex(REALKIND) :: MOM_LC(4), M1(2), M1helarray(2,16)
  real(REALKIND)    :: P_scatt_intern(0:3,6)
  complex(REALKIND) :: epLC(1:4), epStd(1:4,2)
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(1), ex6(1)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf1(1,1), wf2(2,30), wf4(4,126), wf8(8,96), wf16(16,334)

  type(polcont) :: A(16,324)
  complex(REALKIND) :: Aj(324)

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
  extmasses2 = [ rMB2, rMB2, rMB2, rMB2, rMH2, rMH2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_pphhjj_bbbxbxhh_1,6)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,6)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_pphhjj_bbbxbxhh_1(I)
  else if (I < 0) then
    JBmunu = external_perm_pphhjj_bbbxbxhh_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_pphhjj_bbbxbxhh_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  42 continue
  ! external WFs
  call pol_wf_Q(P(:,1), rMB, H1, ex1, POLSEL(1))
  call pol_wf_Q(P(:,2), rMB, H2, ex2, POLSEL(2))
  call pol_wf_A(P(:,3), rMB, H3, ex3, POLSEL(3))
  call pol_wf_A(P(:,4), rMB, H4, ex4, POLSEL(4))
  call pol_wf_S(P(:,5), rMH, H5, ex5, POLSEL(5))
  call pol_wf_S(P(:,6), rMH, H6, ex6, POLSEL(6))


  if (ntry == 1) then
    shift = 1
    ! call helbookkeeping_flip(H1, 1, shift, eflip, exthel, firstpol)
    ! call helbookkeeping_wf(H1, ex1, shift) ...

    if (any(POLSEL /= 0)) then

      call pol_wf_Q(P(:,1), rMB, H1, ex1, 0)
      call pol_wf_Q(P(:,2), rMB, H2, ex2, 0)
      call pol_wf_A(P(:,3), rMB, H3, ex3, 0)
      call pol_wf_A(P(:,4), rMB, H4, ex4, 0)
      call pol_wf_S(P(:,5), rMH, H5, ex5, 0)
      call pol_wf_S(P(:,6), rMH, H6, ex6, 0)

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

  end if

  ! internal WFs
  ! e.g. call vert_VQ_A(ntry, ex3, ex1, wf1, n1, t1) ...
  call vert_AQ_S(gH,ntry, ex3, ex1, wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_AQ_S(gH,ntry, ex4, ex2, wf4(:,2), n3(:,2), t3x4(:,:,2))
  call vert_SSS_S(ntry, wf4(:,1), ex5, ex6, wf4(:,3), n4(:,1), t4x4(:,:,1))
  call vert_AQ_S(gX,ntry, ex3, ex1, wf4(:,4), n3(:,3), t3x4(:,:,3))
  call vert_AQ_S(gX,ntry, ex4, ex2, wf4(:,5), n3(:,4), t3x4(:,:,4))
  call vert_SSS_S(ntry, wf4(:,4), ex5, ex6, wf4(:,6), n4(:,2), t4x4(:,:,2))
  call vert_QA_Z(gZd,ntry, ex1, ex3, wf4(:,7), n3(:,5), t3x4(:,:,5))
  call vert_QA_Z(gZd,ntry, ex2, ex4, wf4(:,8), n3(:,6), t3x4(:,:,6))
  call vert_SSV_V(ntry, ex5, ex6, wf4(:,7), wf4(:,9), n4(:,3), t4x4(:,:,3))
  call vert_AQ_S(gH,ntry, ex4, ex1, wf4(:,10), n3(:,7), t3x4(:,:,7))
  call vert_AQ_S(gH,ntry, ex3, ex2, wf4(:,11), n3(:,8), t3x4(:,:,8))
  call vert_SSS_S(ntry, wf4(:,10), ex5, ex6, wf4(:,12), n4(:,4), t4x4(:,:,4))
  call vert_AQ_S(gX,ntry, ex4, ex1, wf4(:,13), n3(:,9), t3x4(:,:,9))
  call vert_AQ_S(gX,ntry, ex3, ex2, wf4(:,14), n3(:,10), t3x4(:,:,10))
  call vert_SSS_S(ntry, wf4(:,13), ex5, ex6, wf4(:,15), n4(:,5), t4x4(:,:,5))
  call vert_QA_Z(gZd,ntry, ex1, ex4, wf4(:,16), n3(:,11), t3x4(:,:,11))
  call vert_QA_Z(gZd,ntry, ex2, ex3, wf4(:,17), n3(:,12), t3x4(:,:,12))
  call vert_SSV_V(ntry, ex5, ex6, wf4(:,16), wf4(:,18), n4(:,6), t4x4(:,:,6))
  call vert_SS_S(ntry, ex5, ex6, wf1(:,1), n3(:,13), t3x1(:,:,1))
  call vert_SS_S(ntry, wf4(:,1), wf4(:,2), wf16(:,1), n3(:,14), t3x16(:,:,1))
  call vert_SS_S(ntry, wf4(:,4), wf4(:,5), wf16(:,2), n3(:,15), t3x16(:,:,2))
  call vert_VS_T(ntry, wf4(:,8), Q(:,10), wf4(:,4), Q(:,5), wf16(:,3), n3(:,16), t3x16(:,:,3))
  call vert_VS_T(ntry, wf4(:,7), Q(:,5), wf4(:,5), Q(:,10), wf16(:,4), n3(:,17), t3x16(:,:,4))
  call vert_VV_S(ntry, wf4(:,7), wf4(:,8), wf16(:,5), n3(:,18), t3x16(:,:,5))
  call vert_SS_S(ntry, wf4(:,1), ex5, wf4(:,19), n3(:,19), t3x4(:,:,13))
  call vert_SS_S(ntry, wf4(:,2), ex6, wf4(:,20), n3(:,20), t3x4(:,:,14))
  call vert_SS_S(ntry, wf4(:,4), ex5, wf4(:,21), n3(:,21), t3x4(:,:,15))
  call vert_SS_S(ntry, wf4(:,5), ex6, wf4(:,22), n3(:,22), t3x4(:,:,16))
  call vert_ST_V(ntry, wf4(:,4), Q(:,5), ex5, Q(:,16), wf4(:,23), n3(:,23), t3x4(:,:,17))
  call vert_ST_V(ntry, wf4(:,5), Q(:,10), ex6, Q(:,32), wf4(:,24), n3(:,24), t3x4(:,:,18))
  call vert_TV_S(ntry, ex6, Q(:,32), wf4(:,8), Q(:,10), wf4(:,25), n3(:,25), t3x4(:,:,19))
  call vert_SV_V(ntry, ex6, wf4(:,8), wf4(:,26), n3(:,26), t3x4(:,:,20))
  call vert_TV_S(ntry, ex5, Q(:,16), wf4(:,7), Q(:,5), wf4(:,27), n3(:,27), t3x4(:,:,21))
  call vert_SV_V(ntry, ex5, wf4(:,7), wf4(:,28), n3(:,28), t3x4(:,:,22))
  call vert_SS_S(ntry, wf4(:,2), ex5, wf4(:,29), n3(:,29), t3x4(:,:,23))
  call vert_SS_S(ntry, wf4(:,1), ex6, wf4(:,30), n3(:,30), t3x4(:,:,24))
  call vert_SS_S(ntry, wf4(:,5), ex5, wf4(:,31), n3(:,31), t3x4(:,:,25))
  call vert_SS_S(ntry, wf4(:,4), ex6, wf4(:,32), n3(:,32), t3x4(:,:,26))
  call vert_ST_V(ntry, wf4(:,5), Q(:,10), ex5, Q(:,16), wf4(:,33), n3(:,33), t3x4(:,:,27))
  call vert_ST_V(ntry, wf4(:,4), Q(:,5), ex6, Q(:,32), wf4(:,34), n3(:,34), t3x4(:,:,28))
  call vert_TV_S(ntry, ex5, Q(:,16), wf4(:,8), Q(:,10), wf4(:,35), n3(:,35), t3x4(:,:,29))
  call vert_SV_V(ntry, ex5, wf4(:,8), wf4(:,36), n3(:,36), t3x4(:,:,30))
  call vert_TV_S(ntry, ex6, Q(:,32), wf4(:,7), Q(:,5), wf4(:,37), n3(:,37), t3x4(:,:,31))
  call vert_SV_V(ntry, ex6, wf4(:,7), wf4(:,38), n3(:,38), t3x4(:,:,32))
  call vert_QS_A(gH,ntry, ex2, ex5, wf2(:,1), n3(:,39), t3x2(:,:,1))
  call vert_SA_Q(gH,ntry, ex6, ex4, wf2(:,2), n3(:,40), t3x2(:,:,2))
  call prop_Q_A(ntry, wf2(:,1), Q(:,18), MB, 1_intkind1, wf2(:,3), n2(1))
  call prop_A_Q(ntry, wf2(:,2), Q(:,40), MB, 1_intkind1, wf2(:,4), n2(2))
  call vert_QS_A(gH,ntry, wf2(:,3), wf4(:,1), wf8(:,1), n3(:,41), t3x8(:,:,1))
  call vert_QS_A(gX,ntry, wf2(:,3), wf4(:,4), wf8(:,2), n3(:,42), t3x8(:,:,2))
  call vert_QA_V(ntry, ex1, ex3, wf4(:,39), n3(:,43), t3x4(:,:,33))
  call vert_VQ_A(ntry, wf4(:,39), wf2(:,3), wf8(:,3), n3(:,44), t3x8(:,:,3))
  call vert_ZQ_A(gZd,ntry, wf4(:,7), wf2(:,3), wf8(:,4), n3(:,45), t3x8(:,:,4))
  call vert_SA_Q(gH,ntry, wf4(:,1), ex4, wf8(:,5), n3(:,46), t3x8(:,:,5))
  call vert_QS_A(gH,ntry, wf2(:,3), ex6, wf2(:,5), n3(:,47), t3x2(:,:,3))
  call prop_A_Q(ntry, wf8(:,5), Q(:,13), MB, 1_intkind1, wf8(:,6), n2(3))
  call vert_SA_Q(gX,ntry, wf4(:,4), ex4, wf8(:,7), n3(:,48), t3x8(:,:,6))
  call prop_A_Q(ntry, wf8(:,7), Q(:,13), MB, 1_intkind1, wf8(:,8), n2(4))
  call vert_AV_Q(ntry, ex4, wf4(:,39), wf8(:,9), n3(:,49), t3x8(:,:,7))
  call prop_A_Q(ntry, wf8(:,9), Q(:,13), MB, 1_intkind1, wf8(:,10), n2(5))
  call vert_AZ_Q(gZd,ntry, ex4, wf4(:,7), wf8(:,11), n3(:,50), t3x8(:,:,8))
  call prop_A_Q(ntry, wf8(:,11), Q(:,13), MB, 1_intkind1, wf8(:,12), n2(6))
  call vert_AQ_S(gH,ntry, ex4, wf2(:,3), wf4(:,40), n3(:,51), t3x4(:,:,34))
  call vert_AQ_S(gX,ntry, ex4, wf2(:,3), wf4(:,41), n3(:,52), t3x4(:,:,35))
  call vert_QA_Z(gZd,ntry, wf2(:,3), ex4, wf4(:,42), n3(:,53), t3x4(:,:,36))
  call vert_QS_A(gH,ntry, ex2, ex6, wf2(:,6), n3(:,54), t3x2(:,:,4))
  call vert_SA_Q(gH,ntry, ex5, ex4, wf2(:,7), n3(:,55), t3x2(:,:,5))
  call prop_Q_A(ntry, wf2(:,6), Q(:,34), MB, 1_intkind1, wf2(:,8), n2(7))
  call prop_A_Q(ntry, wf2(:,7), Q(:,24), MB, 1_intkind1, wf2(:,9), n2(8))
  call vert_QS_A(gH,ntry, wf2(:,8), wf4(:,1), wf8(:,13), n3(:,56), t3x8(:,:,9))
  call vert_QS_A(gX,ntry, wf2(:,8), wf4(:,4), wf8(:,14), n3(:,57), t3x8(:,:,10))
  call vert_VQ_A(ntry, wf4(:,39), wf2(:,8), wf8(:,15), n3(:,58), t3x8(:,:,11))
  call vert_ZQ_A(gZd,ntry, wf4(:,7), wf2(:,8), wf8(:,16), n3(:,59), t3x8(:,:,12))
  call vert_QS_A(gH,ntry, ex2, wf4(:,1), wf8(:,17), n3(:,60), t3x8(:,:,13))
  call vert_SA_Q(gH,ntry, ex6, wf2(:,9), wf2(:,10), n3(:,61), t3x2(:,:,6))
  call prop_Q_A(ntry, wf8(:,17), Q(:,7), MB, 1_intkind1, wf8(:,18), n2(9))
  call vert_QS_A(gX,ntry, ex2, wf4(:,4), wf8(:,19), n3(:,62), t3x8(:,:,14))
  call prop_Q_A(ntry, wf8(:,19), Q(:,7), MB, 1_intkind1, wf8(:,20), n2(10))
  call vert_VQ_A(ntry, wf4(:,39), ex2, wf8(:,21), n3(:,63), t3x8(:,:,15))
  call prop_Q_A(ntry, wf8(:,21), Q(:,7), MB, 1_intkind1, wf8(:,22), n2(11))
  call vert_ZQ_A(gZd,ntry, wf4(:,7), ex2, wf8(:,23), n3(:,64), t3x8(:,:,16))
  call prop_Q_A(ntry, wf8(:,23), Q(:,7), MB, 1_intkind1, wf8(:,24), n2(12))
  call vert_AQ_S(gH,ntry, wf2(:,9), ex2, wf4(:,43), n3(:,65), t3x4(:,:,37))
  call vert_AQ_S(gX,ntry, wf2(:,9), ex2, wf4(:,44), n3(:,66), t3x4(:,:,38))
  call vert_QA_Z(gZd,ntry, ex2, wf2(:,9), wf4(:,45), n3(:,67), t3x4(:,:,39))
  call vert_QS_A(gH,ntry, wf2(:,8), ex5, wf2(:,11), n3(:,68), t3x2(:,:,7))
  call vert_AQ_S(gH,ntry, ex4, wf2(:,8), wf4(:,46), n3(:,69), t3x4(:,:,40))
  call vert_AQ_S(gX,ntry, ex4, wf2(:,8), wf4(:,47), n3(:,70), t3x4(:,:,41))
  call vert_QA_Z(gZd,ntry, wf2(:,8), ex4, wf4(:,48), n3(:,71), t3x4(:,:,42))
  call vert_SA_Q(gH,ntry, ex5, wf2(:,4), wf2(:,12), n3(:,72), t3x2(:,:,8))
  call vert_AQ_S(gH,ntry, wf2(:,4), ex2, wf4(:,49), n3(:,73), t3x4(:,:,43))
  call vert_AQ_S(gX,ntry, wf2(:,4), ex2, wf4(:,50), n3(:,74), t3x4(:,:,44))
  call vert_QA_Z(gZd,ntry, ex2, wf2(:,4), wf4(:,51), n3(:,75), t3x4(:,:,45))
  call vert_SA_Q(gH,ntry, wf1(:,1), ex4, wf2(:,13), n3(:,76), t3x2(:,:,9))
  call vert_QS_A(gH,ntry, ex2, wf1(:,1), wf2(:,14), n3(:,77), t3x2(:,:,10))
  call prop_Q_A(ntry, wf2(:,14), Q(:,50), MB, 1_intkind1, wf2(:,15), n2(13))
  call vert_SS_S(ntry, wf4(:,11), wf4(:,10), wf16(:,6), n3(:,78), t3x16(:,:,6))
  call vert_SS_S(ntry, wf4(:,14), wf4(:,13), wf16(:,7), n3(:,79), t3x16(:,:,7))
  call vert_VS_T(ntry, wf4(:,17), Q(:,6), wf4(:,13), Q(:,9), wf16(:,8), n3(:,80), t3x16(:,:,8))
  call vert_VS_T(ntry, wf4(:,16), Q(:,9), wf4(:,14), Q(:,6), wf16(:,9), n3(:,81), t3x16(:,:,9))
  call vert_VV_S(ntry, wf4(:,17), wf4(:,16), wf16(:,10), n3(:,82), t3x16(:,:,10))
  call vert_SS_S(ntry, wf4(:,10), ex5, wf4(:,52), n3(:,83), t3x4(:,:,46))
  call vert_SS_S(ntry, wf4(:,11), ex6, wf4(:,53), n3(:,84), t3x4(:,:,47))
  call vert_SS_S(ntry, wf4(:,13), ex5, wf4(:,54), n3(:,85), t3x4(:,:,48))
  call vert_SS_S(ntry, wf4(:,14), ex6, wf4(:,55), n3(:,86), t3x4(:,:,49))
  call vert_ST_V(ntry, wf4(:,13), Q(:,9), ex5, Q(:,16), wf4(:,56), n3(:,87), t3x4(:,:,50))
  call vert_ST_V(ntry, wf4(:,14), Q(:,6), ex6, Q(:,32), wf4(:,57), n3(:,88), t3x4(:,:,51))
  call vert_TV_S(ntry, ex6, Q(:,32), wf4(:,17), Q(:,6), wf4(:,58), n3(:,89), t3x4(:,:,52))
  call vert_SV_V(ntry, ex6, wf4(:,17), wf4(:,59), n3(:,90), t3x4(:,:,53))
  call vert_TV_S(ntry, ex5, Q(:,16), wf4(:,16), Q(:,9), wf4(:,60), n3(:,91), t3x4(:,:,54))
  call vert_SV_V(ntry, ex5, wf4(:,16), wf4(:,61), n3(:,92), t3x4(:,:,55))
  call vert_SS_S(ntry, wf4(:,11), ex5, wf4(:,62), n3(:,93), t3x4(:,:,56))
  call vert_SS_S(ntry, wf4(:,10), ex6, wf4(:,63), n3(:,94), t3x4(:,:,57))
  call vert_SS_S(ntry, wf4(:,14), ex5, wf4(:,64), n3(:,95), t3x4(:,:,58))
  call vert_SS_S(ntry, wf4(:,13), ex6, wf4(:,65), n3(:,96), t3x4(:,:,59))
  call vert_ST_V(ntry, wf4(:,14), Q(:,6), ex5, Q(:,16), wf4(:,66), n3(:,97), t3x4(:,:,60))
  call vert_ST_V(ntry, wf4(:,13), Q(:,9), ex6, Q(:,32), wf4(:,67), n3(:,98), t3x4(:,:,61))
  call vert_TV_S(ntry, ex5, Q(:,16), wf4(:,17), Q(:,6), wf4(:,68), n3(:,99), t3x4(:,:,62))
  call vert_SV_V(ntry, ex5, wf4(:,17), wf4(:,69), n3(:,100), t3x4(:,:,63))
  call vert_TV_S(ntry, ex6, Q(:,32), wf4(:,16), Q(:,9), wf4(:,70), n3(:,101), t3x4(:,:,64))
  call vert_SV_V(ntry, ex6, wf4(:,16), wf4(:,71), n3(:,102), t3x4(:,:,65))
  call vert_QS_A(gH,ntry, ex1, ex5, wf2(:,16), n3(:,103), t3x2(:,:,11))
  call prop_Q_A(ntry, wf2(:,16), Q(:,17), MB, 1_intkind1, wf2(:,17), n2(14))
  call vert_QS_A(gH,ntry, wf2(:,17), wf4(:,11), wf8(:,25), n3(:,104), t3x8(:,:,17))
  call vert_QS_A(gX,ntry, wf2(:,17), wf4(:,14), wf8(:,26), n3(:,105), t3x8(:,:,18))
  call vert_QA_V(ntry, ex2, ex3, wf4(:,72), n3(:,106), t3x4(:,:,66))
  call vert_VQ_A(ntry, wf4(:,72), wf2(:,17), wf8(:,27), n3(:,107), t3x8(:,:,19))
  call vert_ZQ_A(gZd,ntry, wf4(:,17), wf2(:,17), wf8(:,28), n3(:,108), t3x8(:,:,20))
  call vert_AQ_S(gH,ntry, ex4, wf2(:,17), wf4(:,73), n3(:,109), t3x4(:,:,67))
  call vert_AQ_S(gX,ntry, ex4, wf2(:,17), wf4(:,74), n3(:,110), t3x4(:,:,68))
  call vert_QA_Z(gZd,ntry, wf2(:,17), ex4, wf4(:,75), n3(:,111), t3x4(:,:,69))
  call vert_SA_Q(gH,ntry, wf4(:,11), ex4, wf8(:,29), n3(:,112), t3x8(:,:,21))
  call vert_QS_A(gH,ntry, wf2(:,17), ex6, wf2(:,18), n3(:,113), t3x2(:,:,12))
  call prop_A_Q(ntry, wf8(:,29), Q(:,14), MB, 1_intkind1, wf8(:,30), n2(15))
  call vert_SA_Q(gX,ntry, wf4(:,14), ex4, wf8(:,31), n3(:,114), t3x8(:,:,22))
  call prop_A_Q(ntry, wf8(:,31), Q(:,14), MB, 1_intkind1, wf8(:,32), n2(16))
  call vert_AV_Q(ntry, ex4, wf4(:,72), wf8(:,33), n3(:,115), t3x8(:,:,23))
  call prop_A_Q(ntry, wf8(:,33), Q(:,14), MB, 1_intkind1, wf8(:,34), n2(17))
  call vert_AZ_Q(gZd,ntry, ex4, wf4(:,17), wf8(:,35), n3(:,116), t3x8(:,:,24))
  call prop_A_Q(ntry, wf8(:,35), Q(:,14), MB, 1_intkind1, wf8(:,36), n2(18))
  call vert_QS_A(gH,ntry, ex1, ex6, wf2(:,19), n3(:,117), t3x2(:,:,13))
  call prop_Q_A(ntry, wf2(:,19), Q(:,33), MB, 1_intkind1, wf2(:,20), n2(19))
  call vert_QS_A(gH,ntry, wf2(:,20), wf4(:,11), wf8(:,37), n3(:,118), t3x8(:,:,25))
  call vert_QS_A(gX,ntry, wf2(:,20), wf4(:,14), wf8(:,38), n3(:,119), t3x8(:,:,26))
  call vert_VQ_A(ntry, wf4(:,72), wf2(:,20), wf8(:,39), n3(:,120), t3x8(:,:,27))
  call vert_ZQ_A(gZd,ntry, wf4(:,17), wf2(:,20), wf8(:,40), n3(:,121), t3x8(:,:,28))
  call vert_QS_A(gH,ntry, ex1, wf4(:,11), wf8(:,41), n3(:,122), t3x8(:,:,29))
  call prop_Q_A(ntry, wf8(:,41), Q(:,7), MB, 1_intkind1, wf8(:,42), n2(20))
  call vert_QS_A(gX,ntry, ex1, wf4(:,14), wf8(:,43), n3(:,123), t3x8(:,:,30))
  call prop_Q_A(ntry, wf8(:,43), Q(:,7), MB, 1_intkind1, wf8(:,44), n2(21))
  call vert_VQ_A(ntry, wf4(:,72), ex1, wf8(:,45), n3(:,124), t3x8(:,:,31))
  call prop_Q_A(ntry, wf8(:,45), Q(:,7), MB, 1_intkind1, wf8(:,46), n2(22))
  call vert_ZQ_A(gZd,ntry, wf4(:,17), ex1, wf8(:,47), n3(:,125), t3x8(:,:,32))
  call prop_Q_A(ntry, wf8(:,47), Q(:,7), MB, 1_intkind1, wf8(:,48), n2(23))
  call vert_AQ_S(gH,ntry, wf2(:,9), ex1, wf4(:,76), n3(:,126), t3x4(:,:,70))
  call vert_AQ_S(gX,ntry, wf2(:,9), ex1, wf4(:,77), n3(:,127), t3x4(:,:,71))
  call vert_QA_Z(gZd,ntry, ex1, wf2(:,9), wf4(:,78), n3(:,128), t3x4(:,:,72))
  call vert_AQ_S(gH,ntry, ex4, wf2(:,20), wf4(:,79), n3(:,129), t3x4(:,:,73))
  call vert_AQ_S(gX,ntry, ex4, wf2(:,20), wf4(:,80), n3(:,130), t3x4(:,:,74))
  call vert_QA_Z(gZd,ntry, wf2(:,20), ex4, wf4(:,81), n3(:,131), t3x4(:,:,75))
  call vert_QS_A(gH,ntry, wf2(:,20), ex5, wf2(:,21), n3(:,132), t3x2(:,:,14))
  call vert_AQ_S(gH,ntry, wf2(:,4), ex1, wf4(:,82), n3(:,133), t3x4(:,:,76))
  call vert_AQ_S(gX,ntry, wf2(:,4), ex1, wf4(:,83), n3(:,134), t3x4(:,:,77))
  call vert_QA_Z(gZd,ntry, ex1, wf2(:,4), wf4(:,84), n3(:,135), t3x4(:,:,78))
  call vert_QS_A(gH,ntry, ex1, wf1(:,1), wf2(:,22), n3(:,136), t3x2(:,:,15))
  call prop_Q_A(ntry, wf2(:,22), Q(:,49), MB, 1_intkind1, wf2(:,23), n2(24))
  call vert_SA_Q(gH,ntry, ex6, ex3, wf2(:,24), n3(:,137), t3x2(:,:,16))
  call prop_A_Q(ntry, wf2(:,24), Q(:,36), MB, 1_intkind1, wf2(:,25), n2(25))
  call vert_QS_A(gH,ntry, wf2(:,3), wf4(:,10), wf8(:,49), n3(:,138), t3x8(:,:,33))
  call vert_QS_A(gX,ntry, wf2(:,3), wf4(:,13), wf8(:,50), n3(:,139), t3x8(:,:,34))
  call vert_QA_V(ntry, ex1, ex4, wf4(:,85), n3(:,140), t3x4(:,:,79))
  call vert_VQ_A(ntry, wf4(:,85), wf2(:,3), wf8(:,51), n3(:,141), t3x8(:,:,35))
  call vert_ZQ_A(gZd,ntry, wf4(:,16), wf2(:,3), wf8(:,52), n3(:,142), t3x8(:,:,36))
  call vert_SA_Q(gH,ntry, wf4(:,10), ex3, wf8(:,53), n3(:,143), t3x8(:,:,37))
  call prop_A_Q(ntry, wf8(:,53), Q(:,13), MB, 1_intkind1, wf8(:,54), n2(26))
  call vert_SA_Q(gX,ntry, wf4(:,13), ex3, wf8(:,55), n3(:,144), t3x8(:,:,38))
  call prop_A_Q(ntry, wf8(:,55), Q(:,13), MB, 1_intkind1, wf8(:,56), n2(27))
  call vert_AV_Q(ntry, ex3, wf4(:,85), wf8(:,57), n3(:,145), t3x8(:,:,39))
  call prop_A_Q(ntry, wf8(:,57), Q(:,13), MB, 1_intkind1, wf8(:,58), n2(28))
  call vert_AZ_Q(gZd,ntry, ex3, wf4(:,16), wf8(:,59), n3(:,146), t3x8(:,:,40))
  call prop_A_Q(ntry, wf8(:,59), Q(:,13), MB, 1_intkind1, wf8(:,60), n2(29))
  call vert_AQ_S(gH,ntry, ex3, wf2(:,3), wf4(:,86), n3(:,147), t3x4(:,:,80))
  call vert_AQ_S(gX,ntry, ex3, wf2(:,3), wf4(:,87), n3(:,148), t3x4(:,:,81))
  call vert_QA_Z(gZd,ntry, wf2(:,3), ex3, wf4(:,88), n3(:,149), t3x4(:,:,82))
  call vert_SA_Q(gH,ntry, ex5, ex3, wf2(:,26), n3(:,150), t3x2(:,:,17))
  call prop_A_Q(ntry, wf2(:,26), Q(:,20), MB, 1_intkind1, wf2(:,27), n2(30))
  call vert_QS_A(gH,ntry, wf2(:,8), wf4(:,10), wf8(:,61), n3(:,151), t3x8(:,:,41))
  call vert_QS_A(gX,ntry, wf2(:,8), wf4(:,13), wf8(:,62), n3(:,152), t3x8(:,:,42))
  call vert_VQ_A(ntry, wf4(:,85), wf2(:,8), wf8(:,63), n3(:,153), t3x8(:,:,43))
  call vert_ZQ_A(gZd,ntry, wf4(:,16), wf2(:,8), wf8(:,64), n3(:,154), t3x8(:,:,44))
  call vert_QS_A(gH,ntry, ex2, wf4(:,10), wf8(:,65), n3(:,155), t3x8(:,:,45))
  call vert_SA_Q(gH,ntry, ex6, wf2(:,27), wf2(:,28), n3(:,156), t3x2(:,:,18))
  call prop_Q_A(ntry, wf8(:,65), Q(:,11), MB, 1_intkind1, wf8(:,66), n2(31))
  call vert_QS_A(gX,ntry, ex2, wf4(:,13), wf8(:,67), n3(:,157), t3x8(:,:,46))
  call prop_Q_A(ntry, wf8(:,67), Q(:,11), MB, 1_intkind1, wf8(:,68), n2(32))
  call vert_VQ_A(ntry, wf4(:,85), ex2, wf8(:,69), n3(:,158), t3x8(:,:,47))
  call prop_Q_A(ntry, wf8(:,69), Q(:,11), MB, 1_intkind1, wf8(:,70), n2(33))
  call vert_ZQ_A(gZd,ntry, wf4(:,16), ex2, wf8(:,71), n3(:,159), t3x8(:,:,48))
  call prop_Q_A(ntry, wf8(:,71), Q(:,11), MB, 1_intkind1, wf8(:,72), n2(34))
  call vert_AQ_S(gH,ntry, wf2(:,27), ex2, wf4(:,89), n3(:,160), t3x4(:,:,83))
  call vert_AQ_S(gX,ntry, wf2(:,27), ex2, wf4(:,90), n3(:,161), t3x4(:,:,84))
  call vert_QA_Z(gZd,ntry, ex2, wf2(:,27), wf4(:,91), n3(:,162), t3x4(:,:,85))
  call vert_AQ_S(gH,ntry, ex3, wf2(:,8), wf4(:,92), n3(:,163), t3x4(:,:,86))
  call vert_AQ_S(gX,ntry, ex3, wf2(:,8), wf4(:,93), n3(:,164), t3x4(:,:,87))
  call vert_QA_Z(gZd,ntry, wf2(:,8), ex3, wf4(:,94), n3(:,165), t3x4(:,:,88))
  call vert_SA_Q(gH,ntry, ex5, wf2(:,25), wf2(:,29), n3(:,166), t3x2(:,:,19))
  call vert_AQ_S(gH,ntry, wf2(:,25), ex2, wf4(:,95), n3(:,167), t3x4(:,:,89))
  call vert_AQ_S(gX,ntry, wf2(:,25), ex2, wf4(:,96), n3(:,168), t3x4(:,:,90))
  call vert_QA_Z(gZd,ntry, ex2, wf2(:,25), wf4(:,97), n3(:,169), t3x4(:,:,91))
  call vert_SA_Q(gH,ntry, wf1(:,1), ex3, wf2(:,30), n3(:,170), t3x2(:,:,20))
  call vert_QS_A(gH,ntry, wf2(:,17), wf4(:,2), wf8(:,73), n3(:,171), t3x8(:,:,49))
  call vert_QS_A(gX,ntry, wf2(:,17), wf4(:,5), wf8(:,74), n3(:,172), t3x8(:,:,50))
  call vert_QA_V(ntry, ex2, ex4, wf4(:,98), n3(:,173), t3x4(:,:,92))
  call vert_VQ_A(ntry, wf4(:,98), wf2(:,17), wf8(:,75), n3(:,174), t3x8(:,:,51))
  call vert_ZQ_A(gZd,ntry, wf4(:,8), wf2(:,17), wf8(:,76), n3(:,175), t3x8(:,:,52))
  call vert_AQ_S(gH,ntry, ex3, wf2(:,17), wf4(:,99), n3(:,176), t3x4(:,:,93))
  call vert_AQ_S(gX,ntry, ex3, wf2(:,17), wf4(:,100), n3(:,177), t3x4(:,:,94))
  call vert_QA_Z(gZd,ntry, wf2(:,17), ex3, wf4(:,101), n3(:,178), t3x4(:,:,95))
  call vert_SA_Q(gH,ntry, wf4(:,2), ex3, wf8(:,77), n3(:,179), t3x8(:,:,53))
  call prop_A_Q(ntry, wf8(:,77), Q(:,14), MB, 1_intkind1, wf8(:,78), n2(35))
  call vert_SA_Q(gX,ntry, wf4(:,5), ex3, wf8(:,79), n3(:,180), t3x8(:,:,54))
  call prop_A_Q(ntry, wf8(:,79), Q(:,14), MB, 1_intkind1, wf8(:,80), n2(36))
  call vert_AV_Q(ntry, ex3, wf4(:,98), wf8(:,81), n3(:,181), t3x8(:,:,55))
  call prop_A_Q(ntry, wf8(:,81), Q(:,14), MB, 1_intkind1, wf8(:,82), n2(37))
  call vert_AZ_Q(gZd,ntry, ex3, wf4(:,8), wf8(:,83), n3(:,182), t3x8(:,:,56))
  call prop_A_Q(ntry, wf8(:,83), Q(:,14), MB, 1_intkind1, wf8(:,84), n2(38))
  call vert_QS_A(gH,ntry, wf2(:,20), wf4(:,2), wf8(:,85), n3(:,183), t3x8(:,:,57))
  call vert_QS_A(gX,ntry, wf2(:,20), wf4(:,5), wf8(:,86), n3(:,184), t3x8(:,:,58))
  call vert_VQ_A(ntry, wf4(:,98), wf2(:,20), wf8(:,87), n3(:,185), t3x8(:,:,59))
  call vert_ZQ_A(gZd,ntry, wf4(:,8), wf2(:,20), wf8(:,88), n3(:,186), t3x8(:,:,60))
  call vert_QS_A(gH,ntry, ex1, wf4(:,2), wf8(:,89), n3(:,187), t3x8(:,:,61))
  call prop_Q_A(ntry, wf8(:,89), Q(:,11), MB, 1_intkind1, wf8(:,90), n2(39))
  call vert_QS_A(gX,ntry, ex1, wf4(:,5), wf8(:,91), n3(:,188), t3x8(:,:,62))
  call prop_Q_A(ntry, wf8(:,91), Q(:,11), MB, 1_intkind1, wf8(:,92), n2(40))
  call vert_VQ_A(ntry, wf4(:,98), ex1, wf8(:,93), n3(:,189), t3x8(:,:,63))
  call prop_Q_A(ntry, wf8(:,93), Q(:,11), MB, 1_intkind1, wf8(:,94), n2(41))
  call vert_ZQ_A(gZd,ntry, wf4(:,8), ex1, wf8(:,95), n3(:,190), t3x8(:,:,64))
  call prop_Q_A(ntry, wf8(:,95), Q(:,11), MB, 1_intkind1, wf8(:,96), n2(42))
  call vert_AQ_S(gH,ntry, wf2(:,27), ex1, wf4(:,102), n3(:,191), t3x4(:,:,96))
  call vert_AQ_S(gX,ntry, wf2(:,27), ex1, wf4(:,103), n3(:,192), t3x4(:,:,97))
  call vert_QA_Z(gZd,ntry, ex1, wf2(:,27), wf4(:,104), n3(:,193), t3x4(:,:,98))
  call vert_AQ_S(gH,ntry, ex3, wf2(:,20), wf4(:,105), n3(:,194), t3x4(:,:,99))
  call vert_AQ_S(gX,ntry, ex3, wf2(:,20), wf4(:,106), n3(:,195), t3x4(:,:,100))
  call vert_QA_Z(gZd,ntry, wf2(:,20), ex3, wf4(:,107), n3(:,196), t3x4(:,:,101))
  call vert_AQ_S(gH,ntry, wf2(:,25), ex1, wf4(:,108), n3(:,197), t3x4(:,:,102))
  call vert_AQ_S(gX,ntry, wf2(:,25), ex1, wf4(:,109), n3(:,198), t3x4(:,:,103))
  call vert_QA_Z(gZd,ntry, ex1, wf2(:,25), wf4(:,110), n3(:,199), t3x4(:,:,104))
  call vert_QA_V(ntry, wf2(:,17), ex3, wf4(:,111), n3(:,200), t3x4(:,:,105))
  call vert_QA_V(ntry, wf2(:,8), ex4, wf4(:,112), n3(:,201), t3x4(:,:,106))
  call vert_QA_V(ntry, wf2(:,8), ex3, wf4(:,113), n3(:,202), t3x4(:,:,107))
  call vert_QA_V(ntry, wf2(:,17), ex4, wf4(:,114), n3(:,203), t3x4(:,:,108))
  call vert_QA_V(ntry, ex2, wf2(:,25), wf4(:,115), n3(:,204), t3x4(:,:,109))
  call vert_QA_V(ntry, ex2, wf2(:,4), wf4(:,116), n3(:,205), t3x4(:,:,110))
  call vert_QA_V(ntry, wf2(:,20), ex3, wf4(:,117), n3(:,206), t3x4(:,:,111))
  call vert_QA_V(ntry, wf2(:,3), ex4, wf4(:,118), n3(:,207), t3x4(:,:,112))
  call vert_QA_V(ntry, wf2(:,3), ex3, wf4(:,119), n3(:,208), t3x4(:,:,113))
  call vert_QA_V(ntry, wf2(:,20), ex4, wf4(:,120), n3(:,209), t3x4(:,:,114))
  call vert_QA_V(ntry, ex1, wf2(:,25), wf4(:,121), n3(:,210), t3x4(:,:,115))
  call vert_QA_V(ntry, ex1, wf2(:,4), wf4(:,122), n3(:,211), t3x4(:,:,116))
  call vert_QA_V(ntry, ex2, wf2(:,27), wf4(:,123), n3(:,212), t3x4(:,:,117))
  call vert_QA_V(ntry, ex1, wf2(:,27), wf4(:,124), n3(:,213), t3x4(:,:,118))
  call vert_QA_V(ntry, ex2, wf2(:,9), wf4(:,125), n3(:,214), t3x4(:,:,119))
  call vert_QA_V(ntry, ex1, wf2(:,9), wf4(:,126), n3(:,215), t3x4(:,:,120))


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

    M2munu = M2munu / average_factor_pphhjj_bbbxbxhh_1
  end if

  ! Colour and helicity average and symmetry factor of outgoing particles
  M2add = M2 / average_factor_pphhjj_bbbxbxhh_1

  do k = 0, 23-1
    M2(k) = M2add(extcomb_perm_pphhjj_bbbxbxhh_1(k))
  end do

  if (ntry == 1) ntry = 2

  if (scalefactor /= old_scalefactor) then
    scalebackfactor = scalefactor**(2*6-8)
    old_scalefactor = scalefactor
  end if
  M2 = scalebackfactor * M2

  contains

subroutine diagrams()
  implicit none
  ! e.g. call cont_VV(nsync, wf3, wf6, A(:,1), n64, t64, nhel, den(5)) ...

    call cont_SS(nsync, wf4(:,2), wf4(:,3), A(:,1), n3(:,216), t3x16(:,:,11), nhel, den(3))
    call cont_SS(nsync, wf4(:,5), wf4(:,6), A(:,2), n3(:,217), t3x16(:,:,12), nhel, den(6))
    call cont_VV(nsync, wf4(:,8), wf4(:,9), A(:,3), n3(:,218), t3x16(:,:,13), nhel, den(6))
    call cont_SS(nsync, wf4(:,11), wf4(:,12), A(:,4), n3(:,219), t3x16(:,:,14), nhel, den(9))
    call cont_SS(nsync, wf4(:,14), wf4(:,15), A(:,5), n3(:,220), t3x16(:,:,15), nhel, den(12))
    call cont_VV(nsync, wf4(:,17), wf4(:,18), A(:,6), n3(:,221), t3x16(:,:,16), nhel, den(12))
    call cont_SS(nsync, wf1(:,1), wf16(:,1), A(:,7), n3(:,222), t3x16(:,:,17), nhel, den(14))
    call cont_SS(nsync, wf1(:,1), wf16(:,2), A(:,8), n3(:,223), t3x16(:,:,18), nhel, den(15))
    call cont_SS(nsync, wf1(:,1), wf16(:,3), A(:,9), n3(:,224), t3x16(:,:,19), nhel, den(15))
    call cont_SS(nsync, wf1(:,1), wf16(:,4), A(:,10), n3(:,225), t3x16(:,:,20), nhel, den(15))
    call cont_SS(nsync, wf1(:,1), wf16(:,5), A(:,11), n3(:,226), t3x16(:,:,21), nhel, den(15))
    call cont_SS(nsync, wf4(:,19), wf4(:,20), A(:,12), n3(:,227), t3x16(:,:,22), nhel, den(18))
    call cont_SS(nsync, wf4(:,21), wf4(:,22), A(:,13), n3(:,228), t3x16(:,:,23), nhel, den(21))
    call cont_VV(nsync, wf4(:,23), wf4(:,24), A(:,14), n3(:,229), t3x16(:,:,24), nhel, den(21))
    call cont_SS(nsync, wf4(:,21), wf4(:,25), A(:,15), n3(:,230), t3x16(:,:,25), nhel, den(21))
    call cont_VV(nsync, wf4(:,23), wf4(:,26), A(:,16), n3(:,231), t3x16(:,:,26), nhel, den(21))
    call cont_SS(nsync, wf4(:,22), wf4(:,27), A(:,17), n3(:,232), t3x16(:,:,27), nhel, den(21))
    call cont_VV(nsync, wf4(:,24), wf4(:,28), A(:,18), n3(:,233), t3x16(:,:,28), nhel, den(21))
    call cont_SS(nsync, wf4(:,25), wf4(:,27), A(:,19), n3(:,234), t3x16(:,:,29), nhel, den(21))
    call cont_VV(nsync, wf4(:,26), wf4(:,28), A(:,20), n3(:,235), t3x16(:,:,30), nhel, den(21))
    call cont_SS(nsync, wf4(:,29), wf4(:,30), A(:,21), n3(:,236), t3x16(:,:,31), nhel, den(24))
    call cont_SS(nsync, wf4(:,31), wf4(:,32), A(:,22), n3(:,237), t3x16(:,:,32), nhel, den(27))
    call cont_VV(nsync, wf4(:,33), wf4(:,34), A(:,23), n3(:,238), t3x16(:,:,33), nhel, den(27))
    call cont_SS(nsync, wf4(:,32), wf4(:,35), A(:,24), n3(:,239), t3x16(:,:,34), nhel, den(27))
    call cont_VV(nsync, wf4(:,34), wf4(:,36), A(:,25), n3(:,240), t3x16(:,:,35), nhel, den(27))
    call cont_SS(nsync, wf4(:,31), wf4(:,37), A(:,26), n3(:,241), t3x16(:,:,36), nhel, den(27))
    call cont_VV(nsync, wf4(:,33), wf4(:,38), A(:,27), n3(:,242), t3x16(:,:,37), nhel, den(27))
    call cont_SS(nsync, wf4(:,35), wf4(:,37), A(:,28), n3(:,243), t3x16(:,:,38), nhel, den(27))
    call cont_VV(nsync, wf4(:,36), wf4(:,38), A(:,29), n3(:,244), t3x16(:,:,39), nhel, den(27))
    call cont_QA(nsync, wf2(:,4), wf8(:,1), A(:,30), n3(:,245), t3x16(:,:,40), nhel, den(31))
    call cont_QA(nsync, wf2(:,4), wf8(:,2), A(:,31), n3(:,246), t3x16(:,:,41), nhel, den(33))
    call cont_QA(nsync, wf2(:,4), wf8(:,3), A(:,32), n3(:,247), t3x16(:,:,42), nhel, den(36))
    call cont_QA(nsync, wf2(:,4), wf8(:,4), A(:,33), n3(:,248), t3x16(:,:,43), nhel, den(33))
    call cont_QA(nsync, wf2(:,5), wf8(:,6), A(:,34), n3(:,249), t3x16(:,:,44), nhel, den(39))
    call cont_QA(nsync, wf2(:,5), wf8(:,8), A(:,35), n3(:,250), t3x16(:,:,45), nhel, den(41))
    call cont_QA(nsync, wf2(:,5), wf8(:,10), A(:,36), n3(:,251), t3x16(:,:,46), nhel, den(43))
    call cont_QA(nsync, wf2(:,5), wf8(:,12), A(:,37), n3(:,252), t3x16(:,:,47), nhel, den(41))
    call cont_SS(nsync, wf4(:,30), wf4(:,40), A(:,38), n3(:,253), t3x16(:,:,48), nhel, den(45))
    call cont_SS(nsync, wf4(:,32), wf4(:,41), A(:,39), n3(:,254), t3x16(:,:,49), nhel, den(47))
    call cont_VV(nsync, wf4(:,34), wf4(:,42), A(:,40), n3(:,255), t3x16(:,:,50), nhel, den(47))
    call cont_SS(nsync, wf4(:,37), wf4(:,41), A(:,41), n3(:,256), t3x16(:,:,51), nhel, den(47))
    call cont_VV(nsync, wf4(:,38), wf4(:,42), A(:,42), n3(:,257), t3x16(:,:,52), nhel, den(47))
    call cont_QA(nsync, wf2(:,9), wf8(:,13), A(:,43), n3(:,258), t3x16(:,:,53), nhel, den(51))
    call cont_QA(nsync, wf2(:,9), wf8(:,14), A(:,44), n3(:,259), t3x16(:,:,54), nhel, den(53))
    call cont_QA(nsync, wf2(:,9), wf8(:,15), A(:,45), n3(:,260), t3x16(:,:,55), nhel, den(55))
    call cont_QA(nsync, wf2(:,9), wf8(:,16), A(:,46), n3(:,261), t3x16(:,:,56), nhel, den(53))
    call cont_QA(nsync, wf2(:,10), wf8(:,18), A(:,47), n3(:,262), t3x16(:,:,57), nhel, den(58))
    call cont_QA(nsync, wf2(:,10), wf8(:,20), A(:,48), n3(:,263), t3x16(:,:,58), nhel, den(60))
    call cont_QA(nsync, wf2(:,10), wf8(:,22), A(:,49), n3(:,264), t3x16(:,:,59), nhel, den(62))
    call cont_QA(nsync, wf2(:,10), wf8(:,24), A(:,50), n3(:,265), t3x16(:,:,60), nhel, den(60))
    call cont_SS(nsync, wf4(:,30), wf4(:,43), A(:,51), n3(:,266), t3x16(:,:,61), nhel, den(64))
    call cont_SS(nsync, wf4(:,32), wf4(:,44), A(:,52), n3(:,267), t3x16(:,:,62), nhel, den(66))
    call cont_VV(nsync, wf4(:,34), wf4(:,45), A(:,53), n3(:,268), t3x16(:,:,63), nhel, den(66))
    call cont_SS(nsync, wf4(:,37), wf4(:,44), A(:,54), n3(:,269), t3x16(:,:,64), nhel, den(66))
    call cont_VV(nsync, wf4(:,38), wf4(:,45), A(:,55), n3(:,270), t3x16(:,:,65), nhel, den(66))
    call cont_QA(nsync, wf8(:,6), wf2(:,11), A(:,56), n3(:,271), t3x16(:,:,66), nhel, den(67))
    call cont_QA(nsync, wf8(:,8), wf2(:,11), A(:,57), n3(:,272), t3x16(:,:,67), nhel, den(68))
    call cont_QA(nsync, wf8(:,10), wf2(:,11), A(:,58), n3(:,273), t3x16(:,:,68), nhel, den(69))
    call cont_QA(nsync, wf8(:,12), wf2(:,11), A(:,59), n3(:,274), t3x16(:,:,69), nhel, den(68))
    call cont_SS(nsync, wf4(:,19), wf4(:,46), A(:,60), n3(:,275), t3x16(:,:,70), nhel, den(72))
    call cont_SS(nsync, wf4(:,21), wf4(:,47), A(:,61), n3(:,276), t3x16(:,:,71), nhel, den(75))
    call cont_VV(nsync, wf4(:,23), wf4(:,48), A(:,62), n3(:,277), t3x16(:,:,72), nhel, den(75))
    call cont_SS(nsync, wf4(:,27), wf4(:,47), A(:,63), n3(:,278), t3x16(:,:,73), nhel, den(75))
    call cont_VV(nsync, wf4(:,28), wf4(:,48), A(:,64), n3(:,279), t3x16(:,:,74), nhel, den(75))
    call cont_QA(nsync, wf8(:,18), wf2(:,12), A(:,65), n3(:,280), t3x16(:,:,75), nhel, den(76))
    call cont_QA(nsync, wf8(:,20), wf2(:,12), A(:,66), n3(:,281), t3x16(:,:,76), nhel, den(77))
    call cont_QA(nsync, wf8(:,22), wf2(:,12), A(:,67), n3(:,282), t3x16(:,:,77), nhel, den(78))
    call cont_QA(nsync, wf8(:,24), wf2(:,12), A(:,68), n3(:,283), t3x16(:,:,78), nhel, den(77))
    call cont_SS(nsync, wf4(:,19), wf4(:,49), A(:,69), n3(:,284), t3x16(:,:,79), nhel, den(80))
    call cont_SS(nsync, wf4(:,21), wf4(:,50), A(:,70), n3(:,285), t3x16(:,:,80), nhel, den(82))
    call cont_VV(nsync, wf4(:,23), wf4(:,51), A(:,71), n3(:,286), t3x16(:,:,81), nhel, den(82))
    call cont_SS(nsync, wf4(:,27), wf4(:,50), A(:,72), n3(:,287), t3x16(:,:,82), nhel, den(82))
    call cont_VV(nsync, wf4(:,28), wf4(:,51), A(:,73), n3(:,288), t3x16(:,:,83), nhel, den(82))
    call cont_QA(nsync, wf8(:,18), wf2(:,13), A(:,74), n3(:,289), t3x16(:,:,84), nhel, den(83))
    call cont_QA(nsync, wf8(:,20), wf2(:,13), A(:,75), n3(:,290), t3x16(:,:,85), nhel, den(84))
    call cont_QA(nsync, wf8(:,22), wf2(:,13), A(:,76), n3(:,291), t3x16(:,:,86), nhel, den(85))
    call cont_QA(nsync, wf8(:,24), wf2(:,13), A(:,77), n3(:,292), t3x16(:,:,87), nhel, den(84))
    call cont_QA(nsync, wf8(:,5), wf2(:,15), A(:,78), n3(:,293), t3x16(:,:,88), nhel, den(88))
    call cont_QA(nsync, wf8(:,7), wf2(:,15), A(:,79), n3(:,294), t3x16(:,:,89), nhel, den(89))
    call cont_QA(nsync, wf8(:,9), wf2(:,15), A(:,80), n3(:,295), t3x16(:,:,90), nhel, den(90))
    call cont_QA(nsync, wf8(:,11), wf2(:,15), A(:,81), n3(:,296), t3x16(:,:,91), nhel, den(89))
    call cont_SS(nsync, wf1(:,1), wf16(:,6), A(:,82), n3(:,297), t3x16(:,:,92), nhel, den(91))
    call cont_SS(nsync, wf1(:,1), wf16(:,7), A(:,83), n3(:,298), t3x16(:,:,93), nhel, den(92))
    call cont_SS(nsync, wf1(:,1), wf16(:,8), A(:,84), n3(:,299), t3x16(:,:,94), nhel, den(92))
    call cont_SS(nsync, wf1(:,1), wf16(:,9), A(:,85), n3(:,300), t3x16(:,:,95), nhel, den(92))
    call cont_SS(nsync, wf1(:,1), wf16(:,10), A(:,86), n3(:,301), t3x16(:,:,96), nhel, den(92))
    call cont_SS(nsync, wf4(:,52), wf4(:,53), A(:,87), n3(:,302), t3x16(:,:,97), nhel, den(95))
    call cont_SS(nsync, wf4(:,54), wf4(:,55), A(:,88), n3(:,303), t3x16(:,:,98), nhel, den(98))
    call cont_VV(nsync, wf4(:,56), wf4(:,57), A(:,89), n3(:,304), t3x16(:,:,99), nhel, den(98))
    call cont_SS(nsync, wf4(:,54), wf4(:,58), A(:,90), n3(:,305), t3x16(:,:,100), nhel, den(98))
    call cont_VV(nsync, wf4(:,56), wf4(:,59), A(:,91), n3(:,306), t3x16(:,:,101), nhel, den(98))
    call cont_SS(nsync, wf4(:,55), wf4(:,60), A(:,92), n3(:,307), t3x16(:,:,102), nhel, den(98))
    call cont_VV(nsync, wf4(:,57), wf4(:,61), A(:,93), n3(:,308), t3x16(:,:,103), nhel, den(98))
    call cont_SS(nsync, wf4(:,58), wf4(:,60), A(:,94), n3(:,309), t3x16(:,:,104), nhel, den(98))
    call cont_VV(nsync, wf4(:,59), wf4(:,61), A(:,95), n3(:,310), t3x16(:,:,105), nhel, den(98))
    call cont_SS(nsync, wf4(:,62), wf4(:,63), A(:,96), n3(:,311), t3x16(:,:,106), nhel, den(101))
    call cont_SS(nsync, wf4(:,64), wf4(:,65), A(:,97), n3(:,312), t3x16(:,:,107), nhel, den(104))
    call cont_VV(nsync, wf4(:,66), wf4(:,67), A(:,98), n3(:,313), t3x16(:,:,108), nhel, den(104))
    call cont_SS(nsync, wf4(:,65), wf4(:,68), A(:,99), n3(:,314), t3x16(:,:,109), nhel, den(104))
    call cont_VV(nsync, wf4(:,67), wf4(:,69), A(:,100), n3(:,315), t3x16(:,:,110), nhel, den(104))
    call cont_SS(nsync, wf4(:,64), wf4(:,70), A(:,101), n3(:,316), t3x16(:,:,111), nhel, den(104))
    call cont_VV(nsync, wf4(:,66), wf4(:,71), A(:,102), n3(:,317), t3x16(:,:,112), nhel, den(104))
    call cont_SS(nsync, wf4(:,68), wf4(:,70), A(:,103), n3(:,318), t3x16(:,:,113), nhel, den(104))
    call cont_VV(nsync, wf4(:,69), wf4(:,71), A(:,104), n3(:,319), t3x16(:,:,114), nhel, den(104))
    call cont_QA(nsync, wf2(:,4), wf8(:,25), A(:,105), n3(:,320), t3x16(:,:,115), nhel, den(107))
    call cont_QA(nsync, wf2(:,4), wf8(:,26), A(:,106), n3(:,321), t3x16(:,:,116), nhel, den(109))
    call cont_QA(nsync, wf2(:,4), wf8(:,27), A(:,107), n3(:,322), t3x16(:,:,117), nhel, den(112))
    call cont_QA(nsync, wf2(:,4), wf8(:,28), A(:,108), n3(:,323), t3x16(:,:,118), nhel, den(109))
    call cont_SS(nsync, wf4(:,53), wf4(:,73), A(:,109), n3(:,324), t3x16(:,:,119), nhel, den(114))
    call cont_SS(nsync, wf4(:,55), wf4(:,74), A(:,110), n3(:,325), t3x16(:,:,120), nhel, den(116))
    call cont_VV(nsync, wf4(:,57), wf4(:,75), A(:,111), n3(:,326), t3x16(:,:,121), nhel, den(116))
    call cont_SS(nsync, wf4(:,58), wf4(:,74), A(:,112), n3(:,327), t3x16(:,:,122), nhel, den(116))
    call cont_VV(nsync, wf4(:,59), wf4(:,75), A(:,113), n3(:,328), t3x16(:,:,123), nhel, den(116))
    call cont_QA(nsync, wf2(:,18), wf8(:,30), A(:,114), n3(:,329), t3x16(:,:,124), nhel, den(119))
    call cont_QA(nsync, wf2(:,18), wf8(:,32), A(:,115), n3(:,330), t3x16(:,:,125), nhel, den(121))
    call cont_QA(nsync, wf2(:,18), wf8(:,34), A(:,116), n3(:,331), t3x16(:,:,126), nhel, den(123))
    call cont_QA(nsync, wf2(:,18), wf8(:,36), A(:,117), n3(:,332), t3x16(:,:,127), nhel, den(121))
    call cont_QA(nsync, wf2(:,9), wf8(:,37), A(:,118), n3(:,333), t3x16(:,:,128), nhel, den(126))
    call cont_QA(nsync, wf2(:,9), wf8(:,38), A(:,119), n3(:,334), t3x16(:,:,129), nhel, den(128))
    call cont_QA(nsync, wf2(:,9), wf8(:,39), A(:,120), n3(:,335), t3x16(:,:,130), nhel, den(130))
    call cont_QA(nsync, wf2(:,9), wf8(:,40), A(:,121), n3(:,336), t3x16(:,:,131), nhel, den(128))
    call cont_QA(nsync, wf2(:,10), wf8(:,42), A(:,122), n3(:,337), t3x16(:,:,132), nhel, den(132))
    call cont_QA(nsync, wf2(:,10), wf8(:,44), A(:,123), n3(:,338), t3x16(:,:,133), nhel, den(134))
    call cont_QA(nsync, wf2(:,10), wf8(:,46), A(:,124), n3(:,339), t3x16(:,:,134), nhel, den(136))
    call cont_QA(nsync, wf2(:,10), wf8(:,48), A(:,125), n3(:,340), t3x16(:,:,135), nhel, den(134))
    call cont_SS(nsync, wf4(:,53), wf4(:,76), A(:,126), n3(:,341), t3x16(:,:,136), nhel, den(138))
    call cont_SS(nsync, wf4(:,55), wf4(:,77), A(:,127), n3(:,342), t3x16(:,:,137), nhel, den(140))
    call cont_SS(nsync, wf4(:,58), wf4(:,77), A(:,128), n3(:,343), t3x16(:,:,138), nhel, den(140))
    call cont_VV(nsync, wf4(:,57), wf4(:,78), A(:,129), n3(:,344), t3x16(:,:,139), nhel, den(140))
    call cont_VV(nsync, wf4(:,59), wf4(:,78), A(:,130), n3(:,345), t3x16(:,:,140), nhel, den(140))
    call cont_SS(nsync, wf4(:,62), wf4(:,79), A(:,131), n3(:,346), t3x16(:,:,141), nhel, den(143))
    call cont_SS(nsync, wf4(:,64), wf4(:,80), A(:,132), n3(:,347), t3x16(:,:,142), nhel, den(146))
    call cont_VV(nsync, wf4(:,66), wf4(:,81), A(:,133), n3(:,348), t3x16(:,:,143), nhel, den(146))
    call cont_SS(nsync, wf4(:,68), wf4(:,80), A(:,134), n3(:,349), t3x16(:,:,144), nhel, den(146))
    call cont_VV(nsync, wf4(:,69), wf4(:,81), A(:,135), n3(:,350), t3x16(:,:,145), nhel, den(146))
    call cont_QA(nsync, wf8(:,30), wf2(:,21), A(:,136), n3(:,351), t3x16(:,:,146), nhel, den(147))
    call cont_QA(nsync, wf8(:,32), wf2(:,21), A(:,137), n3(:,352), t3x16(:,:,147), nhel, den(148))
    call cont_QA(nsync, wf8(:,34), wf2(:,21), A(:,138), n3(:,353), t3x16(:,:,148), nhel, den(149))
    call cont_QA(nsync, wf8(:,36), wf2(:,21), A(:,139), n3(:,354), t3x16(:,:,149), nhel, den(148))
    call cont_QA(nsync, wf2(:,12), wf8(:,42), A(:,140), n3(:,355), t3x16(:,:,150), nhel, den(150))
    call cont_QA(nsync, wf2(:,12), wf8(:,44), A(:,141), n3(:,356), t3x16(:,:,151), nhel, den(151))
    call cont_QA(nsync, wf2(:,12), wf8(:,46), A(:,142), n3(:,357), t3x16(:,:,152), nhel, den(152))
    call cont_QA(nsync, wf2(:,12), wf8(:,48), A(:,143), n3(:,358), t3x16(:,:,153), nhel, den(151))
    call cont_SS(nsync, wf4(:,62), wf4(:,82), A(:,144), n3(:,359), t3x16(:,:,154), nhel, den(154))
    call cont_SS(nsync, wf4(:,64), wf4(:,83), A(:,145), n3(:,360), t3x16(:,:,155), nhel, den(156))
    call cont_SS(nsync, wf4(:,68), wf4(:,83), A(:,146), n3(:,361), t3x16(:,:,156), nhel, den(156))
    call cont_VV(nsync, wf4(:,66), wf4(:,84), A(:,147), n3(:,362), t3x16(:,:,157), nhel, den(156))
    call cont_VV(nsync, wf4(:,69), wf4(:,84), A(:,148), n3(:,363), t3x16(:,:,158), nhel, den(156))
    call cont_QA(nsync, wf2(:,13), wf8(:,42), A(:,149), n3(:,364), t3x16(:,:,159), nhel, den(157))
    call cont_QA(nsync, wf2(:,13), wf8(:,44), A(:,150), n3(:,365), t3x16(:,:,160), nhel, den(158))
    call cont_QA(nsync, wf2(:,13), wf8(:,46), A(:,151), n3(:,366), t3x16(:,:,161), nhel, den(159))
    call cont_QA(nsync, wf2(:,13), wf8(:,48), A(:,152), n3(:,367), t3x16(:,:,162), nhel, den(158))
    call cont_QA(nsync, wf8(:,29), wf2(:,23), A(:,153), n3(:,368), t3x16(:,:,163), nhel, den(162))
    call cont_QA(nsync, wf8(:,31), wf2(:,23), A(:,154), n3(:,369), t3x16(:,:,164), nhel, den(163))
    call cont_QA(nsync, wf8(:,33), wf2(:,23), A(:,155), n3(:,370), t3x16(:,:,165), nhel, den(164))
    call cont_QA(nsync, wf8(:,35), wf2(:,23), A(:,156), n3(:,371), t3x16(:,:,166), nhel, den(163))
    call cont_QA(nsync, wf2(:,25), wf8(:,49), A(:,157), n3(:,372), t3x16(:,:,167), nhel, den(167))
    call cont_QA(nsync, wf2(:,25), wf8(:,50), A(:,158), n3(:,373), t3x16(:,:,168), nhel, den(169))
    call cont_QA(nsync, wf2(:,25), wf8(:,51), A(:,159), n3(:,374), t3x16(:,:,169), nhel, den(172))
    call cont_QA(nsync, wf2(:,25), wf8(:,52), A(:,160), n3(:,375), t3x16(:,:,170), nhel, den(169))
    call cont_QA(nsync, wf2(:,5), wf8(:,54), A(:,161), n3(:,376), t3x16(:,:,171), nhel, den(174))
    call cont_QA(nsync, wf2(:,5), wf8(:,56), A(:,162), n3(:,377), t3x16(:,:,172), nhel, den(176))
    call cont_QA(nsync, wf2(:,5), wf8(:,58), A(:,163), n3(:,378), t3x16(:,:,173), nhel, den(178))
    call cont_QA(nsync, wf2(:,5), wf8(:,60), A(:,164), n3(:,379), t3x16(:,:,174), nhel, den(176))
    call cont_SS(nsync, wf4(:,63), wf4(:,86), A(:,165), n3(:,380), t3x16(:,:,175), nhel, den(180))
    call cont_SS(nsync, wf4(:,65), wf4(:,87), A(:,166), n3(:,381), t3x16(:,:,176), nhel, den(182))
    call cont_VV(nsync, wf4(:,67), wf4(:,88), A(:,167), n3(:,382), t3x16(:,:,177), nhel, den(182))
    call cont_SS(nsync, wf4(:,70), wf4(:,87), A(:,168), n3(:,383), t3x16(:,:,178), nhel, den(182))
    call cont_VV(nsync, wf4(:,71), wf4(:,88), A(:,169), n3(:,384), t3x16(:,:,179), nhel, den(182))
    call cont_QA(nsync, wf2(:,27), wf8(:,61), A(:,170), n3(:,385), t3x16(:,:,180), nhel, den(185))
    call cont_QA(nsync, wf2(:,27), wf8(:,62), A(:,171), n3(:,386), t3x16(:,:,181), nhel, den(187))
    call cont_QA(nsync, wf2(:,27), wf8(:,63), A(:,172), n3(:,387), t3x16(:,:,182), nhel, den(189))
    call cont_QA(nsync, wf2(:,27), wf8(:,64), A(:,173), n3(:,388), t3x16(:,:,183), nhel, den(187))
    call cont_QA(nsync, wf2(:,28), wf8(:,66), A(:,174), n3(:,389), t3x16(:,:,184), nhel, den(192))
    call cont_QA(nsync, wf2(:,28), wf8(:,68), A(:,175), n3(:,390), t3x16(:,:,185), nhel, den(194))
    call cont_QA(nsync, wf2(:,28), wf8(:,70), A(:,176), n3(:,391), t3x16(:,:,186), nhel, den(196))
    call cont_QA(nsync, wf2(:,28), wf8(:,72), A(:,177), n3(:,392), t3x16(:,:,187), nhel, den(194))
    call cont_SS(nsync, wf4(:,63), wf4(:,89), A(:,178), n3(:,393), t3x16(:,:,188), nhel, den(198))
    call cont_SS(nsync, wf4(:,65), wf4(:,90), A(:,179), n3(:,394), t3x16(:,:,189), nhel, den(200))
    call cont_VV(nsync, wf4(:,67), wf4(:,91), A(:,180), n3(:,395), t3x16(:,:,190), nhel, den(200))
    call cont_SS(nsync, wf4(:,70), wf4(:,90), A(:,181), n3(:,396), t3x16(:,:,191), nhel, den(200))
    call cont_VV(nsync, wf4(:,71), wf4(:,91), A(:,182), n3(:,397), t3x16(:,:,192), nhel, den(200))
    call cont_QA(nsync, wf2(:,11), wf8(:,54), A(:,183), n3(:,398), t3x16(:,:,193), nhel, den(201))
    call cont_QA(nsync, wf2(:,11), wf8(:,56), A(:,184), n3(:,399), t3x16(:,:,194), nhel, den(202))
    call cont_QA(nsync, wf2(:,11), wf8(:,58), A(:,185), n3(:,400), t3x16(:,:,195), nhel, den(203))
    call cont_QA(nsync, wf2(:,11), wf8(:,60), A(:,186), n3(:,401), t3x16(:,:,196), nhel, den(202))
    call cont_SS(nsync, wf4(:,52), wf4(:,92), A(:,187), n3(:,402), t3x16(:,:,197), nhel, den(206))
    call cont_SS(nsync, wf4(:,54), wf4(:,93), A(:,188), n3(:,403), t3x16(:,:,198), nhel, den(209))
    call cont_VV(nsync, wf4(:,56), wf4(:,94), A(:,189), n3(:,404), t3x16(:,:,199), nhel, den(209))
    call cont_SS(nsync, wf4(:,60), wf4(:,93), A(:,190), n3(:,405), t3x16(:,:,200), nhel, den(209))
    call cont_VV(nsync, wf4(:,61), wf4(:,94), A(:,191), n3(:,406), t3x16(:,:,201), nhel, den(209))
    call cont_QA(nsync, wf8(:,66), wf2(:,29), A(:,192), n3(:,407), t3x16(:,:,202), nhel, den(210))
    call cont_QA(nsync, wf8(:,68), wf2(:,29), A(:,193), n3(:,408), t3x16(:,:,203), nhel, den(211))
    call cont_QA(nsync, wf8(:,70), wf2(:,29), A(:,194), n3(:,409), t3x16(:,:,204), nhel, den(212))
    call cont_QA(nsync, wf8(:,72), wf2(:,29), A(:,195), n3(:,410), t3x16(:,:,205), nhel, den(211))
    call cont_SS(nsync, wf4(:,52), wf4(:,95), A(:,196), n3(:,411), t3x16(:,:,206), nhel, den(214))
    call cont_SS(nsync, wf4(:,54), wf4(:,96), A(:,197), n3(:,412), t3x16(:,:,207), nhel, den(216))
    call cont_VV(nsync, wf4(:,56), wf4(:,97), A(:,198), n3(:,413), t3x16(:,:,208), nhel, den(216))
    call cont_SS(nsync, wf4(:,60), wf4(:,96), A(:,199), n3(:,414), t3x16(:,:,209), nhel, den(216))
    call cont_VV(nsync, wf4(:,61), wf4(:,97), A(:,200), n3(:,415), t3x16(:,:,210), nhel, den(216))
    call cont_QA(nsync, wf8(:,66), wf2(:,30), A(:,201), n3(:,416), t3x16(:,:,211), nhel, den(217))
    call cont_QA(nsync, wf8(:,68), wf2(:,30), A(:,202), n3(:,417), t3x16(:,:,212), nhel, den(218))
    call cont_QA(nsync, wf8(:,70), wf2(:,30), A(:,203), n3(:,418), t3x16(:,:,213), nhel, den(219))
    call cont_QA(nsync, wf8(:,72), wf2(:,30), A(:,204), n3(:,419), t3x16(:,:,214), nhel, den(218))
    call cont_QA(nsync, wf2(:,15), wf8(:,53), A(:,205), n3(:,420), t3x16(:,:,215), nhel, den(220))
    call cont_QA(nsync, wf2(:,15), wf8(:,55), A(:,206), n3(:,421), t3x16(:,:,216), nhel, den(221))
    call cont_QA(nsync, wf2(:,15), wf8(:,57), A(:,207), n3(:,422), t3x16(:,:,217), nhel, den(222))
    call cont_QA(nsync, wf2(:,15), wf8(:,59), A(:,208), n3(:,423), t3x16(:,:,218), nhel, den(221))
    call cont_QA(nsync, wf2(:,25), wf8(:,73), A(:,209), n3(:,424), t3x16(:,:,219), nhel, den(224))
    call cont_QA(nsync, wf2(:,25), wf8(:,74), A(:,210), n3(:,425), t3x16(:,:,220), nhel, den(226))
    call cont_QA(nsync, wf2(:,25), wf8(:,75), A(:,211), n3(:,426), t3x16(:,:,221), nhel, den(229))
    call cont_QA(nsync, wf2(:,25), wf8(:,76), A(:,212), n3(:,427), t3x16(:,:,222), nhel, den(226))
    call cont_SS(nsync, wf4(:,20), wf4(:,99), A(:,213), n3(:,428), t3x16(:,:,223), nhel, den(231))
    call cont_SS(nsync, wf4(:,22), wf4(:,100), A(:,214), n3(:,429), t3x16(:,:,224), nhel, den(233))
    call cont_VV(nsync, wf4(:,24), wf4(:,101), A(:,215), n3(:,430), t3x16(:,:,225), nhel, den(233))
    call cont_SS(nsync, wf4(:,25), wf4(:,100), A(:,216), n3(:,431), t3x16(:,:,226), nhel, den(233))
    call cont_VV(nsync, wf4(:,26), wf4(:,101), A(:,217), n3(:,432), t3x16(:,:,227), nhel, den(233))
    call cont_QA(nsync, wf2(:,18), wf8(:,78), A(:,218), n3(:,433), t3x16(:,:,228), nhel, den(235))
    call cont_QA(nsync, wf2(:,18), wf8(:,80), A(:,219), n3(:,434), t3x16(:,:,229), nhel, den(237))
    call cont_QA(nsync, wf2(:,18), wf8(:,82), A(:,220), n3(:,435), t3x16(:,:,230), nhel, den(239))
    call cont_QA(nsync, wf2(:,18), wf8(:,84), A(:,221), n3(:,436), t3x16(:,:,231), nhel, den(237))
    call cont_QA(nsync, wf2(:,27), wf8(:,85), A(:,222), n3(:,437), t3x16(:,:,232), nhel, den(241))
    call cont_QA(nsync, wf2(:,27), wf8(:,86), A(:,223), n3(:,438), t3x16(:,:,233), nhel, den(243))
    call cont_QA(nsync, wf2(:,27), wf8(:,87), A(:,224), n3(:,439), t3x16(:,:,234), nhel, den(245))
    call cont_QA(nsync, wf2(:,27), wf8(:,88), A(:,225), n3(:,440), t3x16(:,:,235), nhel, den(243))
    call cont_QA(nsync, wf2(:,28), wf8(:,90), A(:,226), n3(:,441), t3x16(:,:,236), nhel, den(247))
    call cont_QA(nsync, wf2(:,28), wf8(:,92), A(:,227), n3(:,442), t3x16(:,:,237), nhel, den(249))
    call cont_QA(nsync, wf2(:,28), wf8(:,94), A(:,228), n3(:,443), t3x16(:,:,238), nhel, den(251))
    call cont_QA(nsync, wf2(:,28), wf8(:,96), A(:,229), n3(:,444), t3x16(:,:,239), nhel, den(249))
    call cont_SS(nsync, wf4(:,20), wf4(:,102), A(:,230), n3(:,445), t3x16(:,:,240), nhel, den(253))
    call cont_SS(nsync, wf4(:,22), wf4(:,103), A(:,231), n3(:,446), t3x16(:,:,241), nhel, den(255))
    call cont_SS(nsync, wf4(:,25), wf4(:,103), A(:,232), n3(:,447), t3x16(:,:,242), nhel, den(255))
    call cont_VV(nsync, wf4(:,24), wf4(:,104), A(:,233), n3(:,448), t3x16(:,:,243), nhel, den(255))
    call cont_VV(nsync, wf4(:,26), wf4(:,104), A(:,234), n3(:,449), t3x16(:,:,244), nhel, den(255))
    call cont_SS(nsync, wf4(:,29), wf4(:,105), A(:,235), n3(:,450), t3x16(:,:,245), nhel, den(258))
    call cont_SS(nsync, wf4(:,31), wf4(:,106), A(:,236), n3(:,451), t3x16(:,:,246), nhel, den(261))
    call cont_VV(nsync, wf4(:,33), wf4(:,107), A(:,237), n3(:,452), t3x16(:,:,247), nhel, den(261))
    call cont_SS(nsync, wf4(:,35), wf4(:,106), A(:,238), n3(:,453), t3x16(:,:,248), nhel, den(261))
    call cont_VV(nsync, wf4(:,36), wf4(:,107), A(:,239), n3(:,454), t3x16(:,:,249), nhel, den(261))
    call cont_QA(nsync, wf2(:,21), wf8(:,78), A(:,240), n3(:,455), t3x16(:,:,250), nhel, den(262))
    call cont_QA(nsync, wf2(:,21), wf8(:,80), A(:,241), n3(:,456), t3x16(:,:,251), nhel, den(263))
    call cont_QA(nsync, wf2(:,21), wf8(:,82), A(:,242), n3(:,457), t3x16(:,:,252), nhel, den(264))
    call cont_QA(nsync, wf2(:,21), wf8(:,84), A(:,243), n3(:,458), t3x16(:,:,253), nhel, den(263))
    call cont_QA(nsync, wf2(:,29), wf8(:,90), A(:,244), n3(:,459), t3x16(:,:,254), nhel, den(265))
    call cont_QA(nsync, wf2(:,29), wf8(:,92), A(:,245), n3(:,460), t3x16(:,:,255), nhel, den(266))
    call cont_QA(nsync, wf2(:,29), wf8(:,94), A(:,246), n3(:,461), t3x16(:,:,256), nhel, den(267))
    call cont_QA(nsync, wf2(:,29), wf8(:,96), A(:,247), n3(:,462), t3x16(:,:,257), nhel, den(266))
    call cont_SS(nsync, wf4(:,29), wf4(:,108), A(:,248), n3(:,463), t3x16(:,:,258), nhel, den(269))
    call cont_SS(nsync, wf4(:,31), wf4(:,109), A(:,249), n3(:,464), t3x16(:,:,259), nhel, den(271))
    call cont_SS(nsync, wf4(:,35), wf4(:,109), A(:,250), n3(:,465), t3x16(:,:,260), nhel, den(271))
    call cont_VV(nsync, wf4(:,33), wf4(:,110), A(:,251), n3(:,466), t3x16(:,:,261), nhel, den(271))
    call cont_VV(nsync, wf4(:,36), wf4(:,110), A(:,252), n3(:,467), t3x16(:,:,262), nhel, den(271))
    call cont_QA(nsync, wf2(:,30), wf8(:,90), A(:,253), n3(:,468), t3x16(:,:,263), nhel, den(272))
    call cont_QA(nsync, wf2(:,30), wf8(:,92), A(:,254), n3(:,469), t3x16(:,:,264), nhel, den(273))
    call cont_QA(nsync, wf2(:,30), wf8(:,94), A(:,255), n3(:,470), t3x16(:,:,265), nhel, den(274))
    call cont_QA(nsync, wf2(:,30), wf8(:,96), A(:,256), n3(:,471), t3x16(:,:,266), nhel, den(273))
    call cont_QA(nsync, wf2(:,23), wf8(:,77), A(:,257), n3(:,472), t3x16(:,:,267), nhel, den(275))
    call cont_QA(nsync, wf2(:,23), wf8(:,79), A(:,258), n3(:,473), t3x16(:,:,268), nhel, den(276))
    call cont_QA(nsync, wf2(:,23), wf8(:,81), A(:,259), n3(:,474), t3x16(:,:,269), nhel, den(277))
    call cont_QA(nsync, wf2(:,23), wf8(:,83), A(:,260), n3(:,475), t3x16(:,:,270), nhel, den(276))
    call cont_SS(nsync, wf4(:,46), wf4(:,99), A(:,261), n3(:,476), t3x16(:,:,271), nhel, den(278))
    call cont_SS(nsync, wf4(:,47), wf4(:,100), A(:,262), n3(:,477), t3x16(:,:,272), nhel, den(279))
    call cont_VV(nsync, wf4(:,111), wf4(:,112), A(:,263), n3(:,478), t3x16(:,:,273), nhel, den(282))
    call cont_VV(nsync, wf4(:,48), wf4(:,101), A(:,264), n3(:,479), t3x16(:,:,274), nhel, den(279))
    call cont_SS(nsync, wf4(:,73), wf4(:,92), A(:,265), n3(:,480), t3x16(:,:,275), nhel, den(283))
    call cont_SS(nsync, wf4(:,74), wf4(:,93), A(:,266), n3(:,481), t3x16(:,:,276), nhel, den(284))
    call cont_VV(nsync, wf4(:,113), wf4(:,114), A(:,267), n3(:,482), t3x16(:,:,277), nhel, den(287))
    call cont_VV(nsync, wf4(:,75), wf4(:,94), A(:,268), n3(:,483), t3x16(:,:,278), nhel, den(284))
    call cont_SS(nsync, wf4(:,73), wf4(:,95), A(:,269), n3(:,484), t3x16(:,:,279), nhel, den(288))
    call cont_SS(nsync, wf4(:,74), wf4(:,96), A(:,270), n3(:,485), t3x16(:,:,280), nhel, den(289))
    call cont_VV(nsync, wf4(:,114), wf4(:,115), A(:,271), n3(:,486), t3x16(:,:,281), nhel, den(291))
    call cont_VV(nsync, wf4(:,75), wf4(:,97), A(:,272), n3(:,487), t3x16(:,:,282), nhel, den(289))
    call cont_SS(nsync, wf4(:,49), wf4(:,99), A(:,273), n3(:,488), t3x16(:,:,283), nhel, den(292))
    call cont_SS(nsync, wf4(:,50), wf4(:,100), A(:,274), n3(:,489), t3x16(:,:,284), nhel, den(293))
    call cont_VV(nsync, wf4(:,111), wf4(:,116), A(:,275), n3(:,490), t3x16(:,:,285), nhel, den(296))
    call cont_VV(nsync, wf4(:,51), wf4(:,101), A(:,276), n3(:,491), t3x16(:,:,286), nhel, den(293))
    call cont_SS(nsync, wf4(:,40), wf4(:,105), A(:,277), n3(:,492), t3x16(:,:,287), nhel, den(297))
    call cont_SS(nsync, wf4(:,41), wf4(:,106), A(:,278), n3(:,493), t3x16(:,:,288), nhel, den(298))
    call cont_VV(nsync, wf4(:,117), wf4(:,118), A(:,279), n3(:,494), t3x16(:,:,289), nhel, den(301))
    call cont_VV(nsync, wf4(:,42), wf4(:,107), A(:,280), n3(:,495), t3x16(:,:,290), nhel, den(298))
    call cont_SS(nsync, wf4(:,79), wf4(:,86), A(:,281), n3(:,496), t3x16(:,:,291), nhel, den(302))
    call cont_SS(nsync, wf4(:,80), wf4(:,87), A(:,282), n3(:,497), t3x16(:,:,292), nhel, den(303))
    call cont_VV(nsync, wf4(:,119), wf4(:,120), A(:,283), n3(:,498), t3x16(:,:,293), nhel, den(306))
    call cont_VV(nsync, wf4(:,81), wf4(:,88), A(:,284), n3(:,499), t3x16(:,:,294), nhel, den(303))
    call cont_SS(nsync, wf4(:,40), wf4(:,108), A(:,285), n3(:,500), t3x16(:,:,295), nhel, den(307))
    call cont_SS(nsync, wf4(:,41), wf4(:,109), A(:,286), n3(:,501), t3x16(:,:,296), nhel, den(308))
    call cont_VV(nsync, wf4(:,118), wf4(:,121), A(:,287), n3(:,502), t3x16(:,:,297), nhel, den(310))
    call cont_VV(nsync, wf4(:,42), wf4(:,110), A(:,288), n3(:,503), t3x16(:,:,298), nhel, den(308))
    call cont_SS(nsync, wf4(:,82), wf4(:,86), A(:,289), n3(:,504), t3x16(:,:,299), nhel, den(311))
    call cont_SS(nsync, wf4(:,83), wf4(:,87), A(:,290), n3(:,505), t3x16(:,:,300), nhel, den(312))
    call cont_VV(nsync, wf4(:,119), wf4(:,122), A(:,291), n3(:,506), t3x16(:,:,301), nhel, den(315))
    call cont_VV(nsync, wf4(:,84), wf4(:,88), A(:,292), n3(:,507), t3x16(:,:,302), nhel, den(312))
    call cont_SS(nsync, wf4(:,79), wf4(:,89), A(:,293), n3(:,508), t3x16(:,:,303), nhel, den(316))
    call cont_SS(nsync, wf4(:,80), wf4(:,90), A(:,294), n3(:,509), t3x16(:,:,304), nhel, den(317))
    call cont_VV(nsync, wf4(:,120), wf4(:,123), A(:,295), n3(:,510), t3x16(:,:,305), nhel, den(319))
    call cont_VV(nsync, wf4(:,81), wf4(:,91), A(:,296), n3(:,511), t3x16(:,:,306), nhel, den(317))
    call cont_SS(nsync, wf4(:,46), wf4(:,102), A(:,297), n3(:,512), t3x16(:,:,307), nhel, den(320))
    call cont_SS(nsync, wf4(:,47), wf4(:,103), A(:,298), n3(:,513), t3x16(:,:,308), nhel, den(321))
    call cont_VV(nsync, wf4(:,112), wf4(:,124), A(:,299), n3(:,514), t3x16(:,:,309), nhel, den(323))
    call cont_VV(nsync, wf4(:,48), wf4(:,104), A(:,300), n3(:,515), t3x16(:,:,310), nhel, den(321))
    call cont_SS(nsync, wf4(:,49), wf4(:,102), A(:,301), n3(:,516), t3x16(:,:,311), nhel, den(324))
    call cont_SS(nsync, wf4(:,50), wf4(:,103), A(:,302), n3(:,517), t3x16(:,:,312), nhel, den(325))
    call cont_VV(nsync, wf4(:,116), wf4(:,124), A(:,303), n3(:,518), t3x16(:,:,313), nhel, den(326))
    call cont_VV(nsync, wf4(:,51), wf4(:,104), A(:,304), n3(:,519), t3x16(:,:,314), nhel, den(325))
    call cont_SS(nsync, wf4(:,82), wf4(:,89), A(:,305), n3(:,520), t3x16(:,:,315), nhel, den(327))
    call cont_SS(nsync, wf4(:,83), wf4(:,90), A(:,306), n3(:,521), t3x16(:,:,316), nhel, den(328))
    call cont_VV(nsync, wf4(:,122), wf4(:,123), A(:,307), n3(:,522), t3x16(:,:,317), nhel, den(329))
    call cont_VV(nsync, wf4(:,84), wf4(:,91), A(:,308), n3(:,523), t3x16(:,:,318), nhel, den(328))
    call cont_SS(nsync, wf4(:,43), wf4(:,105), A(:,309), n3(:,524), t3x16(:,:,319), nhel, den(330))
    call cont_SS(nsync, wf4(:,44), wf4(:,106), A(:,310), n3(:,525), t3x16(:,:,320), nhel, den(331))
    call cont_VV(nsync, wf4(:,117), wf4(:,125), A(:,311), n3(:,526), t3x16(:,:,321), nhel, den(334))
    call cont_VV(nsync, wf4(:,45), wf4(:,107), A(:,312), n3(:,527), t3x16(:,:,322), nhel, den(331))
    call cont_SS(nsync, wf4(:,76), wf4(:,92), A(:,313), n3(:,528), t3x16(:,:,323), nhel, den(335))
    call cont_SS(nsync, wf4(:,77), wf4(:,93), A(:,314), n3(:,529), t3x16(:,:,324), nhel, den(336))
    call cont_VV(nsync, wf4(:,113), wf4(:,126), A(:,315), n3(:,530), t3x16(:,:,325), nhel, den(339))
    call cont_VV(nsync, wf4(:,78), wf4(:,94), A(:,316), n3(:,531), t3x16(:,:,326), nhel, den(336))
    call cont_SS(nsync, wf4(:,43), wf4(:,108), A(:,317), n3(:,532), t3x16(:,:,327), nhel, den(340))
    call cont_SS(nsync, wf4(:,44), wf4(:,109), A(:,318), n3(:,533), t3x16(:,:,328), nhel, den(341))
    call cont_VV(nsync, wf4(:,121), wf4(:,125), A(:,319), n3(:,534), t3x16(:,:,329), nhel, den(342))
    call cont_VV(nsync, wf4(:,45), wf4(:,110), A(:,320), n3(:,535), t3x16(:,:,330), nhel, den(341))
    call cont_SS(nsync, wf4(:,76), wf4(:,95), A(:,321), n3(:,536), t3x16(:,:,331), nhel, den(343))
    call cont_SS(nsync, wf4(:,77), wf4(:,96), A(:,322), n3(:,537), t3x16(:,:,332), nhel, den(344))
    call cont_VV(nsync, wf4(:,115), wf4(:,126), A(:,323), n3(:,538), t3x16(:,:,333), nhel, den(345))
    call cont_VV(nsync, wf4(:,78), wf4(:,97), A(:,324), n3(:,539), t3x16(:,:,334), nhel, den(344))

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
  complex(REALKIND), intent(out) :: M1(2) ! M1helarray(2,16)
  integer :: empty(0)

  M1(1) = (A(j,94)%j+A(j,103)%j)*f(1)+A(j,6)%j*f(2)+A(j,86)%j*f(3)+(A(j,95)%j+A(j,104)%j)*f(4)+(-A(j,91)%j-A(j,93)%j-A(j,100)%j &
       -A(j,102)%j)*f(5)+(-A(j,90)%j-A(j,92)%j-A(j,99)%j-A(j,101)%j)*f(6)+(-A(j,84)%j-A(j,85)%j)*f(7)+(A(j,113)%j+A(j,130)%j &
       +A(j,135)%j+A(j,148)%j+A(j,169)%j+A(j,182)%j+A(j,191)%j+A(j,200)%j)*f(8)+(A(j,151)%j+A(j,155)%j+A(j,203)%j+A(j,207)%j)*f(9) &
       +(A(j,152)%j+A(j,156)%j+A(j,204)%j+A(j,208)%j)*f(10)+A(j,5)%j*f(11)-A(j,4)%j*f(12)+(A(j,88)%j+A(j,97)%j)*f(13) &
       +A(j,83)%j*f(14)+(-A(j,82)%j-A(j,87)%j-A(j,96)%j)*f(15)+(A(j,89)%j+A(j,98)%j)*f(16)+(-A(j,111)%j-A(j,112)%j-A(j,128)%j &
       -A(j,129)%j-A(j,133)%j-A(j,134)%j-A(j,146)%j-A(j,147)%j-A(j,167)%j-A(j,168)%j-A(j,180)%j-A(j,181)%j-A(j,189)%j-A(j,190)%j &
       -A(j,198)%j-A(j,199)%j)*f(17)+(A(j,107)%j+A(j,116)%j+A(j,120)%j+A(j,124)%j+A(j,138)%j+A(j,142)%j+A(j,159)%j+A(j,163)%j &
       +A(j,172)%j+A(j,176)%j+A(j,185)%j+A(j,194)%j+A(j,267)%j+A(j,271)%j+A(j,283)%j+A(j,291)%j+A(j,295)%j+A(j,307)%j+A(j,315)%j &
       +A(j,323)%j)*f(18)+(A(j,108)%j+A(j,117)%j+A(j,121)%j+A(j,125)%j+A(j,139)%j+A(j,143)%j+A(j,160)%j+A(j,164)%j+A(j,173)%j &
       +A(j,177)%j+A(j,186)%j+A(j,195)%j+A(j,268)%j+A(j,272)%j+A(j,284)%j+A(j,292)%j+A(j,296)%j+A(j,308)%j+A(j,316)%j &
       +A(j,324)%j)*f(19)+(A(j,110)%j+A(j,127)%j+A(j,132)%j+A(j,145)%j+A(j,166)%j+A(j,179)%j+A(j,188)%j+A(j,197)%j)*f(20)+( &
       -A(j,109)%j-A(j,126)%j-A(j,131)%j-A(j,144)%j-A(j,149)%j+A(j,150)%j-A(j,153)%j+A(j,154)%j-A(j,165)%j-A(j,178)%j-A(j,187)%j &
       -A(j,196)%j-A(j,201)%j+A(j,202)%j-A(j,205)%j+A(j,206)%j)*f(21)+(-A(j,105)%j+A(j,106)%j-A(j,114)%j+A(j,115)%j-A(j,118)%j &
       +A(j,119)%j-A(j,122)%j+A(j,123)%j-A(j,136)%j+A(j,137)%j-A(j,140)%j+A(j,141)%j-A(j,157)%j+A(j,158)%j-A(j,161)%j+A(j,162)%j &
       -A(j,170)%j+A(j,171)%j-A(j,174)%j+A(j,175)%j-A(j,183)%j+A(j,184)%j-A(j,192)%j+A(j,193)%j-A(j,265)%j+A(j,266)%j-A(j,269)%j &
       +A(j,270)%j-A(j,281)%j+A(j,282)%j-A(j,289)%j+A(j,290)%j-A(j,293)%j+A(j,294)%j-A(j,305)%j+A(j,306)%j-A(j,313)%j+A(j,314)%j &
       -A(j,321)%j+A(j,322)%j)*f(22)
  M1(2) = (-A(j,19)%j-A(j,28)%j)*f(1)-A(j,3)%j*f(2)-A(j,11)%j*f(3)+(-A(j,20)%j-A(j,29)%j)*f(4)+(A(j,16)%j+A(j,18)%j+A(j,25)%j &
       +A(j,27)%j)*f(5)+(A(j,15)%j+A(j,17)%j+A(j,24)%j+A(j,26)%j)*f(6)+(A(j,9)%j+A(j,10)%j)*f(7)+(-A(j,42)%j-A(j,55)%j-A(j,64)%j &
       -A(j,73)%j-A(j,217)%j-A(j,234)%j-A(j,239)%j-A(j,252)%j)*f(8)+(-A(j,76)%j-A(j,80)%j-A(j,255)%j-A(j,259)%j)*f(9)+(-A(j,77)%j &
       -A(j,81)%j-A(j,256)%j-A(j,260)%j)*f(10)-A(j,2)%j*f(11)+A(j,1)%j*f(12)+(-A(j,13)%j-A(j,22)%j)*f(13)-A(j,8)%j*f(14)+(A(j,7)%j &
       +A(j,12)%j+A(j,21)%j)*f(15)+(-A(j,14)%j-A(j,23)%j)*f(16)+(A(j,40)%j+A(j,41)%j+A(j,53)%j+A(j,54)%j+A(j,62)%j+A(j,63)%j &
       +A(j,71)%j+A(j,72)%j+A(j,215)%j+A(j,216)%j+A(j,232)%j+A(j,233)%j+A(j,237)%j+A(j,238)%j+A(j,250)%j+A(j,251)%j)*f(17)+( &
       -A(j,32)%j-A(j,36)%j-A(j,45)%j-A(j,49)%j-A(j,58)%j-A(j,67)%j-A(j,211)%j-A(j,220)%j-A(j,224)%j-A(j,228)%j-A(j,242)%j &
       -A(j,246)%j-A(j,263)%j-A(j,275)%j-A(j,279)%j-A(j,287)%j-A(j,299)%j-A(j,303)%j-A(j,311)%j-A(j,319)%j)*f(18)+(-A(j,33)%j &
       -A(j,37)%j-A(j,46)%j-A(j,50)%j-A(j,59)%j-A(j,68)%j-A(j,212)%j-A(j,221)%j-A(j,225)%j-A(j,229)%j-A(j,243)%j-A(j,247)%j &
       -A(j,264)%j-A(j,276)%j-A(j,280)%j-A(j,288)%j-A(j,300)%j-A(j,304)%j-A(j,312)%j-A(j,320)%j)*f(19)+(-A(j,39)%j-A(j,52)%j &
       -A(j,61)%j-A(j,70)%j-A(j,214)%j-A(j,231)%j-A(j,236)%j-A(j,249)%j)*f(20)+(A(j,38)%j+A(j,51)%j+A(j,60)%j+A(j,69)%j+A(j,74)%j &
       -A(j,75)%j+A(j,78)%j-A(j,79)%j+A(j,213)%j+A(j,230)%j+A(j,235)%j+A(j,248)%j+A(j,253)%j-A(j,254)%j+A(j,257)%j &
       -A(j,258)%j)*f(21)+(A(j,30)%j-A(j,31)%j+A(j,34)%j-A(j,35)%j+A(j,43)%j-A(j,44)%j+A(j,47)%j-A(j,48)%j+A(j,56)%j-A(j,57)%j &
       +A(j,65)%j-A(j,66)%j+A(j,209)%j-A(j,210)%j+A(j,218)%j-A(j,219)%j+A(j,222)%j-A(j,223)%j+A(j,226)%j-A(j,227)%j+A(j,240)%j &
       -A(j,241)%j+A(j,244)%j-A(j,245)%j+A(j,261)%j-A(j,262)%j+A(j,273)%j-A(j,274)%j+A(j,277)%j-A(j,278)%j+A(j,285)%j-A(j,286)%j &
       +A(j,297)%j-A(j,298)%j+A(j,301)%j-A(j,302)%j+A(j,309)%j-A(j,310)%j+A(j,317)%j-A(j,318)%j)*f(22)

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
  use ol_colourmatrix_pphhjj_bbbxbxhh_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(2)
  real(REALKIND),    intent(out) :: M2colint(0:23-1)
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
  use ol_colourmatrix_pphhjj_bbbxbxhh_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(2), M2(2)
  real(REALKIND),    intent(out) :: M2colint(0:23-1)
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
  use ol_colourmatrix_pphhjj_bbbxbxhh_1_/**/REALKIND, only: K1
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
  & bind(c,name="ol_tree_colvect_pphhjj_bbbxbxhh_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(2,16)
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
    & bind(c,name="ol_f_amp2tree_pphhjj_bbbxbxhh_1")
#else
subroutine amp2tree(P, M2)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix element without fuss.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,6)
  real(REALKIND),  intent(out) :: M2
  real(REALKIND) :: M2tmp(0:23-1)
  real(REALKIND) :: M2munu(4,4)
  call amp2(P, M2tmp, 0, [ 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND ], 1, [ 0 ], M2munu)
  M2 = M2tmp(0)
end subroutine amp2tree


#ifdef PRECISION_dp
subroutine amp2ccone(P, M2, I, J) &
    & bind(c,name="ol_f_amp2ccone_pphhjj_bbbxbxhh_1")
#else
subroutine amp2ccone(P, M2, I, J)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix for the colour correlation matrix for particles I and J.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,6)
  real(REALKIND),  intent(out) :: M2
  integer,         intent(in)  :: I, J
  integer        :: extcomb
  real(REALKIND) :: M2tmp(0:23-1)
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
    & bind(c,name="ol_f_amp2ccall_pphhjj_bbbxbxhh_1")
#else
subroutine amp2ccall(P, M2)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix for all colour correlation matrices.
  ! The correlation between particles i and j is at position i*(i-1)/2+j of the array M2.
  ! M2(0) is AMP2tree
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,6)
  real(REALKIND),  intent(out) :: M2(0:23-1)
  real(REALKIND) :: M2munu(4,4)
  integer :: k
  call amp2(P, M2, 0, [ 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND ], &
    23, [ (k, k = 0, 23-1) ], M2munu)
end subroutine amp2ccall


#ifdef PRECISION_dp
subroutine amp2hcone(P, M2, I, J, MOM) &
    & bind(c,name="ol_f_amp2hcone_pphhjj_bbbxbxhh_1")
#else
subroutine amp2hcone(P, M2, I, J, MOM)
#endif
  ! This is an interface for AMP2.
  ! Calculates the helicity correlation for emitter I with momentum MOM and spectator J
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,6)
  real(REALKIND),  intent(out) :: M2
  integer,         intent(in)  :: I, J
  real(REALKIND),  intent(in)  :: MOM(0:3)
  integer        :: extcomb
  real(REALKIND) :: M2tmp(0:23-1)
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
    & bind(c,name="ol_f_amp2hcall_pphhjj_bbbxbxhh_1")
#else
subroutine amp2hcall(P, M2, I, MOM)
#endif
  ! This is an interface for AMP2.
  ! Calculates all helicity correlations for emitter I with momentum MOM.
  ! The correlator for spectator j is at position j of the array M2.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,6)
  real(REALKIND),  intent(out) :: M2(6)
  integer,         intent(in)  :: I
  real(REALKIND),  intent(in)  :: MOM(0:3)
  real(REALKIND) :: M2tmp(0:23-1)
  real(REALKIND) :: M2munu(4,4)
  integer        :: J, extcombs(6)
  do J = 1, 6
    if (J <= I) then
      extcombs(J) = I*(I-1)/2 + J
    else
      extcombs(J) = J*(J-1)/2 + I
    end if
  end do
  call amp2(P, M2tmp, I, MOM, 6,extcombs, M2munu)
  do J = 1, 6
    M2(J) = M2tmp(extcombs(J))
  end do
end subroutine amp2hcall


#ifdef PRECISION_dp

subroutine amp2tree_c(p, m2) &
    & bind(c,name="ol_amp2tree_pphhjj_bbbxbxhh_1")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2
  f_p = p
  call amp2tree(f_p, f_m2)
  m2 = f_m2
end subroutine amp2tree_c

subroutine amp2ccone_c(p, m2, i, j) &
    & bind(c,name="ol_amp2ccone_pphhjj_bbbxbxhh_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  f_p = p
  f_i = i
  f_j = j
  call amp2ccone(f_p, f_m2, f_i, f_j)
  m2 = f_m2
end subroutine amp2ccone_c

subroutine amp2ccall_c(p, m2) &
    & bind(c,name="ol_amp2ccall_pphhjj_bbbxbxhh_1")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2(0:23-1)
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2(0:23-1)
  f_p = p
  call amp2ccall(f_p, f_m2)
  m2 = f_m2
end subroutine amp2ccall_c

subroutine amp2hcone_c(p, m2, i, j, mom) &
    & bind(c,name="ol_amp2hcone_pphhjj_bbbxbxhh_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,6)
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
    & bind(c,name="ol_amp2hcall_pphhjj_bbbxbxhh_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2(6)
  integer(c_int), intent(in) :: i
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2(6)
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
    & bind(c,name="amp2tree_pphhjj_bbbxbxhh_1_")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2
  f_p = p
  call amp2tree(f_p, f_m2)
  m2 = f_m2
end subroutine amp2tree_legacy

subroutine amp2ccone_legacy(p, m2, i, j) &
    & bind(c,name="amp2ccone_pphhjj_bbbxbxhh_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  f_p = p
  f_i = i
  f_j = j
  call amp2ccone(f_p, f_m2, f_i, f_j)
  m2 = f_m2
end subroutine amp2ccone_legacy

subroutine amp2ccall_legacy(p, m2) &
    & bind(c,name="amp2ccall_pphhjj_bbbxbxhh_1_")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2(0:23-1)
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2(0:23-1)
  f_p = p
  call amp2ccall(f_p, f_m2)
  m2 = f_m2
end subroutine amp2ccall_legacy

subroutine amp2hcone_legacy(p, m2, i, j, mom) &
    & bind(c,name="amp2hcone_pphhjj_bbbxbxhh_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,6)
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
    & bind(c,name="amp2hcall_pphhjj_bbbxbxhh_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2(6)
  integer(c_int), intent(in) :: i
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2(6)
  integer :: f_i
  real(DREALKIND) :: f_mom(0:3)
  f_p = p
  f_i = i
  f_mom = mom
  call amp2hcall(f_p, f_m2, f_i, f_mom)
  m2 = f_m2
end subroutine amp2hcall_legacy

#endif

end module ol_tree_pphhjj_bbbxbxhh_1_/**/REALKIND
