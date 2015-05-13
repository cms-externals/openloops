
module ol_colourmatrix_ppwajj_uxdddxawxg_1_/**/REALKIND
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
  K1(  9,:) = [   6,   2,   2,   0]
  K1( 10,:) = [   2,   0,  -6, -16]
  K1( 11,:) = [   2,  -6,   0, -16]
  K1( 12,:) = [   0, -16, -16, -48]
  K1( 13,:) = [  48,  16,  16,   0]
  K1( 14,:) = [  16,  48,   0,  16]
  K1( 15,:) = [  16,   0,  48,  16]
  K1( 16,:) = [   0,  16,  16,  48]
  K1( 17,:) = [   0,   2, -16,  -6]
  K1( 18,:) = [   2,   6,   0,   2]
  K1( 19,:) = [ -16,   0, -48, -16]
  K1( 20,:) = [  -6,   2, -16,   0]
  K1( 21,:) = [   0,  16,  -2,   6]
  K1( 22,:) = [  16,   0,   6,  -2]
  K1( 23,:) = [  -2,   6,   0,  16]
  K1( 24,:) = [   6,  -2,  16,   0]
  K1( 25,:) = [  48,  16,  16,   0]
  K1( 26,:) = [  16,  48,   0,  16]
  K1( 27,:) = [  16,   0,  48,  16]
  K1( 28,:) = [   0,  16,  16,  48]
  K1( 29,:) = [   0,  -2,  16,   6]
  K1( 30,:) = [  -2,   0,   6,  16]
  K1( 31,:) = [  16,   6,   0,  -2]
  K1( 32,:) = [   6,  16,  -2,   0]
  K1( 33,:) = [   0, -16,   2,  -6]
  K1( 34,:) = [ -16, -48,   0, -16]
  K1( 35,:) = [   2,   0,   6,   2]
  K1( 36,:) = [  -6, -16,   2,   0]
  K1( 37,:) = [ -48, -16, -16,   0]
  K1( 38,:) = [ -16,   0,  -6,   2]
  K1( 39,:) = [ -16,  -6,   0,   2]
  K1( 40,:) = [   0,   2,   2,   6]
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
  K1( 90,:) = [ -18, -54,   0, -18]
  K1( 91,:) = [ -18,   0,   0,  18]
  K1( 92,:) = [   0, -18,  18,   0]
  K1( 93,:) = [ -54, -18, -18,   0]
  K1( 94,:) = [ -18,   0,   0,  18]
  K1( 95,:) = [ -18,   0, -54, -18]
  K1( 96,:) = [   0,  18, -18,   0]
  K1( 97,:) = [   0, -18,  18,   0]
  K1( 98,:) = [ -18, -54,   0, -18]
  K1( 99,:) = [  18,   0,   0, -18]
  K1(100,:) = [   0, -18, -18, -54]
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
end module ol_colourmatrix_ppwajj_uxdddxawxg_1_/**/REALKIND



module ol_forced_parameters_ppwajj_uxdddxawxg_1_/**/REALKIND
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
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppwajj_uxdddxawxg_1_/**/REALKIND

module ol_tree_ppwajj_uxdddxawxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(6)
  complex(REALKIND), save :: den(265)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 192 ! number of helicity configurations
  integer(intkind2), save :: nhel = 192 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(192) ! physical helicity states
  complex(DREALKIND) :: M1helarr(4,192) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = (CI*eQED**2*gQCD**3)/(3._/**/REALKIND*sqrt2*sw)
    f(2) = (2*CI*eQED**2*gQCD**3)/(3._/**/REALKIND*sqrt2*sw)
    f(3) = (CI*eQED**2*gQCD**3)/(sqrt2*sw)
    f(4) = (eQED**2*gQCD**3)/(sqrt2*sw*3._/**/REALKIND)
    f(5) = (2*eQED**2*gQCD**3)/(sqrt2*sw*3._/**/REALKIND)
    f(6) = (eQED**2*gQCD**3)/(sqrt2*sw)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,17))
  den(2) = 1 / (Q(5,10))
  den(3) = 1 / (Q(5,36))
  den(5) = 1 / (Q(5,81))
  den(9) = 1 / (Q(5,74))
  den(13) = 1 / (Q(5,100))
  den(16) = 1 / (Q(5,68))
  den(18) = 1 / (Q(5,49))
  den(23) = 1 / (Q(5,14))
  den(28) = 1 / (Q(5,33))
  den(29) = 1 / (Q(5,20))
  den(31) = 1 / (Q(5,97))
  den(37) = 1 / (Q(5,84))
  den(40) = 1 / (Q(5,65))
  den(44) = 1 / (Q(5,52))
  den(47) = 1 / (Q(5,11))
  den(66) = 1 / (Q(5,48) - MW2)
  den(83) = 1 / (Q(5,34))
  den(84) = 1 / (Q(5,12))
  den(88) = 1 / (Q(5,98))
  den(92) = 1 / (Q(5,76))
  den(95) = 1 / (Q(5,66))
  den(104) = 1 / (Q(5,18))
  den(108) = 1 / (Q(5,82))
  den(115) = 1 / (Q(5,50))
  den(118) = 1 / (Q(5,13))
  den(151) = 1 / (Q(5,72))
  den(163) = 1 / (Q(5,26))
  den(168) = 1 / (Q(5,37))
  den(188) = 1 / (Q(5,28))
  den(192) = 1 / (Q(5,35))
  den(210) = 1 / (Q(5,24))
  den(219) = 1 / (Q(5,88))

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
  den(17) = den(2)*den(16)
  den(19) = den(1)*den(18)
  den(20) = den(17)*den(19)
  den(21) = den(13)*den(16)
  den(22) = den(12)*den(21)
  den(24) = den(2)*den(23)
  den(25) = den(19)*den(24)
  den(26) = den(10)*den(19)
  den(27) = den(6)*den(24)
  den(30) = den(2)*den(29)
  den(32) = den(28)*den(31)
  den(33) = den(30)*den(32)
  den(34) = den(28)*den(29)
  den(35) = den(10)*den(34)
  den(36) = den(2)*den(28)
  den(38) = den(29)*den(37)
  den(39) = den(36)*den(38)
  den(41) = den(31)*den(40)
  den(42) = den(30)*den(41)
  den(43) = den(2)*den(40)
  den(45) = den(29)*den(44)
  den(46) = den(43)*den(45)
  den(48) = den(2)*den(47)
  den(49) = den(38)*den(48)
  den(50) = den(45)*den(48)
  den(51) = den(10)*den(45)
  den(52) = den(18)*den(28)
  den(53) = den(17)*den(52)
  den(54) = den(16)*den(37)
  den(55) = den(36)*den(54)
  den(56) = den(24)*den(52)
  den(57) = den(10)*den(52)
  den(58) = den(24)*den(32)
  den(59) = den(5)*den(40)
  den(60) = den(4)*den(59)
  den(61) = den(3)*den(44)
  den(62) = den(43)*den(61)
  den(63) = den(14)*den(48)
  den(64) = den(48)*den(61)
  den(65) = den(10)*den(61)
  den(67) = den(40)*den(66)
  den(68) = den(24)*den(67)
  den(69) = den(44)*den(66)
  den(70) = den(43)*den(69)
  den(71) = den(16)*den(66)
  den(72) = den(48)*den(71)
  den(73) = den(18)*den(66)
  den(74) = den(17)*den(73)
  den(75) = den(48)*den(69)
  den(76) = den(10)*den(73)
  den(77) = den(10)*den(69)
  den(78) = den(24)*den(73)
  den(79) = den(24)*den(59)
  den(80) = den(24)*den(41)
  den(81) = den(21)*den(48)
  den(82) = den(48)*den(54)
  den(85) = den(83)*den(84)
  den(86) = den(6)*den(85)
  den(87) = den(1)*den(84)
  den(89) = den(83)*den(88)
  den(90) = den(87)*den(89)
  den(91) = den(1)*den(83)
  den(93) = den(84)*den(92)
  den(94) = den(91)*den(93)
  den(96) = den(84)*den(95)
  den(97) = den(19)*den(96)
  den(98) = den(88)*den(95)
  den(99) = den(87)*den(98)
  den(100) = den(23)*den(84)
  den(101) = den(19)*den(100)
  den(102) = den(19)*den(93)
  den(103) = den(6)*den(100)
  den(105) = den(84)*den(104)
  den(106) = den(32)*den(105)
  den(107) = den(28)*den(84)
  den(109) = den(104)*den(108)
  den(110) = den(107)*den(109)
  den(111) = den(28)*den(104)
  den(112) = den(93)*den(111)
  den(113) = den(41)*den(105)
  den(114) = den(40)*den(84)
  den(116) = den(104)*den(115)
  den(117) = den(114)*den(116)
  den(119) = den(84)*den(118)
  den(120) = den(109)*den(119)
  den(121) = den(116)*den(119)
  den(122) = den(93)*den(116)
  den(123) = den(52)*den(96)
  den(124) = den(95)*den(108)
  den(125) = den(107)*den(124)
  den(126) = den(52)*den(100)
  den(127) = den(52)*den(93)
  den(128) = den(32)*den(100)
  den(129) = den(59)*den(85)
  den(130) = den(83)*den(115)
  den(131) = den(114)*den(130)
  den(132) = den(89)*den(119)
  den(133) = den(119)*den(130)
  den(134) = den(93)*den(130)
  den(135) = den(67)*den(100)
  den(136) = den(66)*den(115)
  den(137) = den(114)*den(136)
  den(138) = den(66)*den(95)
  den(139) = den(119)*den(138)
  den(140) = den(73)*den(96)
  den(141) = den(73)*den(93)
  den(142) = den(93)*den(136)
  den(143) = den(119)*den(136)
  den(144) = den(73)*den(100)
  den(145) = den(59)*den(100)
  den(146) = den(41)*den(100)
  den(147) = den(98)*den(119)
  den(148) = den(119)*den(124)
  den(149) = den(16)*den(92)
  den(150) = den(91)*den(149)
  den(152) = den(92)*den(151)
  den(153) = den(91)*den(152)
  den(154) = den(9)*den(95)
  den(155) = den(8)*den(154)
  den(156) = den(9)*den(151)
  den(157) = den(8)*den(156)
  den(158) = den(19)*den(154)
  den(159) = den(19)*den(149)
  den(160) = den(19)*den(156)
  den(161) = den(19)*den(152)
  den(162) = den(16)*den(28)
  den(164) = den(104)*den(163)
  den(165) = den(162)*den(164)
  den(166) = den(111)*den(149)
  den(167) = den(104)*den(151)
  den(169) = den(28)*den(168)
  den(170) = den(167)*den(169)
  den(171) = den(111)*den(152)
  den(172) = den(164)*den(169)
  den(173) = den(109)*den(169)
  den(174) = den(32)*den(164)
  den(175) = den(3)*den(40)
  den(176) = den(164)*den(175)
  den(177) = den(3)*den(168)
  den(178) = den(167)*den(177)
  den(179) = den(109)*den(177)
  den(180) = den(164)*den(177)
  den(181) = den(14)*den(164)
  den(182) = den(41)*den(164)
  den(183) = den(21)*den(164)
  den(184) = den(116)*den(149)
  den(185) = den(116)*den(152)
  den(186) = den(34)*den(154)
  den(187) = den(28)*den(95)
  den(189) = den(29)*den(188)
  den(190) = den(187)*den(189)
  den(191) = den(29)*den(151)
  den(193) = den(28)*den(192)
  den(194) = den(191)*den(193)
  den(195) = den(34)*den(156)
  den(196) = den(38)*den(193)
  den(197) = den(189)*den(193)
  den(198) = den(32)*den(189)
  den(199) = den(40)*den(83)
  den(200) = den(189)*den(199)
  den(201) = den(83)*den(192)
  den(202) = den(191)*den(201)
  den(203) = den(38)*den(201)
  den(204) = den(189)*den(201)
  den(205) = den(89)*den(189)
  den(206) = den(41)*den(189)
  den(207) = den(45)*den(154)
  den(208) = den(98)*den(189)
  den(209) = den(45)*den(156)
  den(211) = den(95)*den(210)
  den(212) = den(169)*den(211)
  den(213) = den(188)*den(210)
  den(214) = den(187)*den(213)
  den(215) = den(16)*den(210)
  den(216) = den(193)*den(215)
  den(217) = den(163)*den(210)
  den(218) = den(162)*den(217)
  den(220) = den(210)*den(219)
  den(221) = den(193)*den(220)
  den(222) = den(193)*den(213)
  den(223) = den(169)*den(220)
  den(224) = den(169)*den(217)
  den(225) = den(32)*den(217)
  den(226) = den(32)*den(213)
  den(227) = den(199)*den(213)
  den(228) = den(201)*den(215)
  den(229) = den(201)*den(220)
  den(230) = den(201)*den(213)
  den(231) = den(89)*den(213)
  den(232) = den(175)*den(217)
  den(233) = den(177)*den(211)
  den(234) = den(177)*den(220)
  den(235) = den(14)*den(217)
  den(236) = den(177)*den(217)
  den(237) = den(41)*den(217)
  den(238) = den(41)*den(213)
  den(239) = den(98)*den(213)
  den(240) = den(21)*den(217)
  den(241) = den(124)*den(169)
  den(242) = den(52)*den(154)
  den(243) = den(54)*den(193)
  den(244) = den(52)*den(149)
  den(245) = den(151)*den(219)
  den(246) = den(193)*den(245)
  den(247) = den(169)*den(245)
  den(248) = den(52)*den(156)
  den(249) = den(52)*den(152)
  den(250) = den(54)*den(201)
  den(251) = den(130)*den(149)
  den(252) = den(201)*den(245)
  den(253) = den(130)*den(152)
  den(254) = den(124)*den(177)
  den(255) = den(61)*den(154)
  den(256) = den(177)*den(245)
  den(257) = den(61)*den(156)
  den(258) = den(69)*den(154)
  den(259) = den(73)*den(154)
  den(260) = den(136)*den(149)
  den(261) = den(73)*den(149)
  den(262) = den(69)*den(156)
  den(263) = den(136)*den(152)
  den(264) = den(73)*den(152)
  den(265) = den(73)*den(156)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_ppwajj_uxdddxawxg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_ppwajj_uxdddxawxg_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for anti-up down down anti-down gamma W+ glue -> 0
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
  use ol_external_ppwajj_uxdddxawxg_1, only: external_perm_ppwajj_uxdddxawxg_1, &
    & external_perm_inv_ppwajj_uxdddxawxg_1, extcomb_perm_ppwajj_uxdddxawxg_1, &
    & average_factor_ppwajj_uxdddxawxg_1
  use ol_external_ppwajj_uxdddxawxg_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_ppwajj_uxdddxawxg_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_ppwajj_uxdddxawxg_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_ppwajj_uxdddxawxg_1
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
  complex(REALKIND) :: MOM_LC(4), M1(4), M1helarray(4,192)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(2), ex6(3), ex7(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,18), wf6(6,8), wf8(8,34), wf12(12,34), wf16(16,37), wf24(24,44), wf192(192,164)

  type(polcont) :: A(192,164)
  complex(REALKIND) :: Aj(164)

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
  extmasses2 = [ rZERO2, rZERO2, rZERO2, rZERO2, rZERO2, rMW2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_ppwajj_uxdddxawxg_1,7)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,7)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_ppwajj_uxdddxawxg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_ppwajj_uxdddxawxg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_ppwajj_uxdddxawxg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_A(P(:,1), rZERO, H1, ex1)
  call wf_Q(P(:,2), rZERO, H2, ex2)
  call wf_Q(P(:,3), rZERO, H3, ex3)
  call wf_A(P(:,4), rZERO, H4, ex4)
  call wf_V(P(:,5), rZERO, H5, ex5)
  call wf_V(P(:,6), rMW, H6, ex6)
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
  call vert_AV_Q(ntry, ex1, ex5, wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_QA_V(ntry, ex2, ex4, wf4(:,2), n3(:,2), t3x4(:,:,2))
  call vert_WQ_A(ntry, ex6, ex3, wf6(:,1), n3(:,3), t3x6(:,:,1))
  call prop_A_Q(ntry, wf4(:,1), Q(:,17), ZERO, 0_intkind1, wf4(:,3), n2(1))
  call prop_Q_A(ntry, wf6(:,1), Q(:,36), ZERO, 0_intkind1, wf6(:,2), n2(2))
  call vert_AV_Q(ntry, wf4(:,3), ex7, wf8(:,1), n3(:,4), t3x8(:,:,1))
  call vert_VQ_A(ntry, wf4(:,2), wf6(:,2), wf24(:,1), n3(:,5), t3x24(:,:,1))
  call prop_A_Q(ntry, wf8(:,1), Q(:,81), ZERO, 0_intkind1, wf8(:,2), n2(3))
  call vert_UV_W(ntry, wf4(:,2), Q(:,10), ex7, Q(:,64), wf8(:,3), n3(:,6), t3x8(:,:,2))
  call vert_QA_V(ntry, wf6(:,2), wf4(:,3), wf24(:,2), n3(:,7), t3x24(:,:,2))
  call vert_VQ_A(ntry, ex7, wf6(:,2), wf12(:,1), n3(:,8), t3x12(:,:,1))
  call vert_AV_Q(ntry, wf4(:,3), wf4(:,2), wf16(:,1), n3(:,9), t3x16(:,:,1))
  call prop_Q_A(ntry, wf12(:,1), Q(:,100), ZERO, 0_intkind1, wf12(:,2), n2(4))
  call vert_VQ_A(ntry, ex7, ex3, wf4(:,4), n3(:,10), t3x4(:,:,3))
  call prop_Q_A(ntry, wf4(:,4), Q(:,68), ZERO, 0_intkind1, wf4(:,5), n2(5))
  call vert_AW_Q(ntry, wf4(:,3), ex6, wf12(:,3), n3(:,11), t3x12(:,:,2))
  call vert_VQ_A(ntry, wf4(:,2), wf4(:,5), wf16(:,2), n3(:,12), t3x16(:,:,2))
  call prop_A_Q(ntry, wf12(:,3), Q(:,49), ZERO, 0_intkind1, wf12(:,4), n2(6))
  call vert_WQ_A(ntry, ex6, wf4(:,5), wf12(:,5), n3(:,13), t3x12(:,:,3))
  call prop_Q_A(ntry, wf12(:,5), Q(:,100), ZERO, 0_intkind1, wf12(:,6), n2(7))
  call vert_VQ_A(ntry, wf4(:,2), ex3, wf8(:,4), n3(:,14), t3x8(:,:,3))
  call prop_Q_A(ntry, wf8(:,4), Q(:,14), ZERO, 0_intkind1, wf8(:,5), n2(8))
  call vert_VQ_A(ntry, ex7, wf8(:,5), wf16(:,3), n3(:,15), t3x16(:,:,3))
  call vert_QA_V(ntry, ex3, wf12(:,4), wf24(:,3), n3(:,16), t3x24(:,:,3))
  call vert_WQ_A(ntry, ex6, wf8(:,5), wf24(:,4), n3(:,17), t3x24(:,:,4))
  call vert_AW_Q(ntry, ex1, ex6, wf6(:,3), n3(:,18), t3x6(:,:,2))
  call vert_VQ_A(ntry, ex5, ex3, wf4(:,6), n3(:,19), t3x4(:,:,4))
  call prop_A_Q(ntry, wf6(:,3), Q(:,33), ZERO, 0_intkind1, wf6(:,4), n2(9))
  call prop_Q_A(ntry, wf4(:,6), Q(:,20), ZERO, 0_intkind1, wf4(:,7), n2(10))
  call vert_AV_Q(ntry, wf6(:,4), ex7, wf12(:,7), n3(:,20), t3x12(:,:,4))
  call vert_VQ_A(ntry, wf4(:,2), wf4(:,7), wf16(:,4), n3(:,21), t3x16(:,:,4))
  call prop_A_Q(ntry, wf12(:,7), Q(:,97), ZERO, 0_intkind1, wf12(:,8), n2(11))
  call vert_QA_V(ntry, wf4(:,7), wf6(:,4), wf24(:,5), n3(:,22), t3x24(:,:,5))
  call vert_VQ_A(ntry, ex7, wf4(:,7), wf8(:,6), n3(:,23), t3x8(:,:,4))
  call vert_AV_Q(ntry, wf6(:,4), wf4(:,2), wf24(:,6), n3(:,24), t3x24(:,:,6))
  call prop_Q_A(ntry, wf8(:,6), Q(:,84), ZERO, 0_intkind1, wf8(:,7), n2(12))
  call vert_AV_Q(ntry, ex1, ex7, wf4(:,8), n3(:,25), t3x4(:,:,5))
  call prop_A_Q(ntry, wf4(:,8), Q(:,65), ZERO, 0_intkind1, wf4(:,9), n2(13))
  call vert_AW_Q(ntry, wf4(:,9), ex6, wf12(:,9), n3(:,26), t3x12(:,:,5))
  call prop_A_Q(ntry, wf12(:,9), Q(:,97), ZERO, 0_intkind1, wf12(:,10), n2(14))
  call vert_WQ_A(ntry, ex6, wf4(:,7), wf12(:,11), n3(:,27), t3x12(:,:,6))
  call vert_AV_Q(ntry, wf4(:,9), wf4(:,2), wf16(:,5), n3(:,28), t3x16(:,:,5))
  call prop_Q_A(ntry, wf12(:,11), Q(:,52), ZERO, 0_intkind1, wf12(:,12), n2(15))
  call vert_AV_Q(ntry, ex1, wf4(:,2), wf8(:,8), n3(:,29), t3x8(:,:,5))
  call prop_A_Q(ntry, wf8(:,8), Q(:,11), ZERO, 0_intkind1, wf8(:,9), n2(16))
  call vert_AW_Q(ntry, wf8(:,9), ex6, wf24(:,7), n3(:,30), t3x24(:,:,7))
  call vert_AV_Q(ntry, wf8(:,9), ex7, wf16(:,6), n3(:,31), t3x16(:,:,6))
  call vert_QA_V(ntry, wf12(:,12), ex1, wf24(:,8), n3(:,32), t3x24(:,:,8))
  call vert_AV_Q(ntry, wf6(:,4), ex5, wf12(:,13), n3(:,33), t3x12(:,:,7))
  call prop_A_Q(ntry, wf12(:,13), Q(:,49), ZERO, 0_intkind1, wf12(:,14), n2(17))
  call vert_VQ_A(ntry, ex5, wf4(:,5), wf8(:,10), n3(:,34), t3x8(:,:,6))
  call prop_Q_A(ntry, wf8(:,10), Q(:,84), ZERO, 0_intkind1, wf8(:,11), n2(18))
  call vert_QA_V(ntry, ex3, wf12(:,14), wf24(:,9), n3(:,35), t3x24(:,:,9))
  call vert_VQ_A(ntry, ex5, wf8(:,5), wf16(:,7), n3(:,36), t3x16(:,:,7))
  call vert_AV_Q(ntry, wf4(:,9), ex5, wf8(:,12), n3(:,37), t3x8(:,:,7))
  call prop_A_Q(ntry, wf8(:,12), Q(:,81), ZERO, 0_intkind1, wf8(:,13), n2(19))
  call vert_VQ_A(ntry, ex5, wf6(:,2), wf12(:,15), n3(:,38), t3x12(:,:,8))
  call prop_Q_A(ntry, wf12(:,15), Q(:,52), ZERO, 0_intkind1, wf12(:,16), n2(20))
  call vert_AV_Q(ntry, wf8(:,9), ex5, wf16(:,8), n3(:,39), t3x16(:,:,8))
  call vert_QA_V(ntry, wf12(:,16), ex1, wf24(:,10), n3(:,40), t3x24(:,:,10))
  call vert_UV_W(ntry, ex5, Q(:,16), ex6, Q(:,32), wf6(:,5), n3(:,41), t3x6(:,:,3))
  call prop_W_W(ntry, wf6(:,5), Q(:,48), MW, 1_intkind1, wf6(:,6), n2(21))
  call vert_AW_Q(ntry, wf4(:,9), wf6(:,6), wf24(:,11), n3(:,42), t3x24(:,:,11))
  call vert_WQ_A(ntry, wf6(:,6), ex3, wf12(:,17), n3(:,43), t3x12(:,:,9))
  call prop_Q_A(ntry, wf12(:,17), Q(:,52), ZERO, 0_intkind1, wf12(:,18), n2(22))
  call vert_WQ_A(ntry, wf6(:,6), wf4(:,5), wf24(:,12), n3(:,44), t3x24(:,:,12))
  call vert_AW_Q(ntry, ex1, wf6(:,6), wf12(:,19), n3(:,45), t3x12(:,:,10))
  call prop_A_Q(ntry, wf12(:,19), Q(:,49), ZERO, 0_intkind1, wf12(:,20), n2(23))
  call vert_QA_V(ntry, ex3, wf12(:,20), wf24(:,13), n3(:,46), t3x24(:,:,13))
  call vert_QA_V(ntry, wf12(:,18), ex1, wf24(:,14), n3(:,47), t3x24(:,:,14))
  call vert_AV_Q(ntry, wf12(:,20), ex7, wf24(:,15), n3(:,48), t3x24(:,:,15))
  call vert_WQ_A(ntry, ex6, ex2, wf6(:,7), n3(:,49), t3x6(:,:,4))
  call vert_QA_V(ntry, ex3, ex4, wf4(:,10), n3(:,50), t3x4(:,:,6))
  call prop_Q_A(ntry, wf6(:,7), Q(:,34), ZERO, 0_intkind1, wf6(:,8), n2(24))
  call vert_VQ_A(ntry, wf4(:,10), wf6(:,8), wf24(:,16), n3(:,51), t3x24(:,:,16))
  call vert_VQ_A(ntry, ex7, wf6(:,8), wf12(:,21), n3(:,52), t3x12(:,:,11))
  call vert_AV_Q(ntry, wf4(:,3), wf4(:,10), wf16(:,9), n3(:,53), t3x16(:,:,9))
  call prop_Q_A(ntry, wf12(:,21), Q(:,98), ZERO, 0_intkind1, wf12(:,22), n2(25))
  call vert_UV_W(ntry, wf4(:,10), Q(:,12), ex7, Q(:,64), wf8(:,14), n3(:,54), t3x8(:,:,8))
  call vert_QA_V(ntry, wf6(:,8), wf4(:,3), wf24(:,17), n3(:,55), t3x24(:,:,17))
  call vert_VQ_A(ntry, ex7, ex2, wf4(:,11), n3(:,56), t3x4(:,:,7))
  call prop_Q_A(ntry, wf4(:,11), Q(:,66), ZERO, 0_intkind1, wf4(:,12), n2(26))
  call vert_VQ_A(ntry, wf4(:,10), wf4(:,12), wf16(:,10), n3(:,57), t3x16(:,:,10))
  call vert_WQ_A(ntry, ex6, wf4(:,12), wf12(:,23), n3(:,58), t3x12(:,:,12))
  call prop_Q_A(ntry, wf12(:,23), Q(:,98), ZERO, 0_intkind1, wf12(:,24), n2(27))
  call vert_VQ_A(ntry, wf4(:,10), ex2, wf8(:,15), n3(:,59), t3x8(:,:,9))
  call prop_Q_A(ntry, wf8(:,15), Q(:,14), ZERO, 0_intkind1, wf8(:,16), n2(28))
  call vert_VQ_A(ntry, ex7, wf8(:,16), wf16(:,11), n3(:,60), t3x16(:,:,11))
  call vert_QA_V(ntry, ex2, wf12(:,4), wf24(:,18), n3(:,61), t3x24(:,:,18))
  call vert_WQ_A(ntry, ex6, wf8(:,16), wf24(:,19), n3(:,62), t3x24(:,:,19))
  call vert_VQ_A(ntry, ex5, ex2, wf4(:,13), n3(:,63), t3x4(:,:,8))
  call prop_Q_A(ntry, wf4(:,13), Q(:,18), ZERO, 0_intkind1, wf4(:,14), n2(29))
  call vert_VQ_A(ntry, wf4(:,10), wf4(:,14), wf16(:,12), n3(:,64), t3x16(:,:,12))
  call vert_VQ_A(ntry, ex7, wf4(:,14), wf8(:,17), n3(:,65), t3x8(:,:,10))
  call vert_AV_Q(ntry, wf6(:,4), wf4(:,10), wf24(:,20), n3(:,66), t3x24(:,:,20))
  call prop_Q_A(ntry, wf8(:,17), Q(:,82), ZERO, 0_intkind1, wf8(:,18), n2(30))
  call vert_QA_V(ntry, wf4(:,14), wf6(:,4), wf24(:,21), n3(:,67), t3x24(:,:,21))
  call vert_WQ_A(ntry, ex6, wf4(:,14), wf12(:,25), n3(:,68), t3x12(:,:,13))
  call vert_AV_Q(ntry, wf4(:,9), wf4(:,10), wf16(:,13), n3(:,69), t3x16(:,:,13))
  call prop_Q_A(ntry, wf12(:,25), Q(:,50), ZERO, 0_intkind1, wf12(:,26), n2(31))
  call vert_AV_Q(ntry, ex1, wf4(:,10), wf8(:,19), n3(:,70), t3x8(:,:,11))
  call prop_A_Q(ntry, wf8(:,19), Q(:,13), ZERO, 0_intkind1, wf8(:,20), n2(32))
  call vert_AW_Q(ntry, wf8(:,20), ex6, wf24(:,22), n3(:,71), t3x24(:,:,22))
  call vert_AV_Q(ntry, wf8(:,20), ex7, wf16(:,14), n3(:,72), t3x16(:,:,14))
  call vert_QA_V(ntry, wf12(:,26), ex1, wf24(:,23), n3(:,73), t3x24(:,:,23))
  call vert_VQ_A(ntry, ex5, wf4(:,12), wf8(:,21), n3(:,74), t3x8(:,:,12))
  call prop_Q_A(ntry, wf8(:,21), Q(:,82), ZERO, 0_intkind1, wf8(:,22), n2(33))
  call vert_QA_V(ntry, ex2, wf12(:,14), wf24(:,24), n3(:,75), t3x24(:,:,24))
  call vert_VQ_A(ntry, ex5, wf8(:,16), wf16(:,15), n3(:,76), t3x16(:,:,15))
  call vert_VQ_A(ntry, ex5, wf6(:,8), wf12(:,27), n3(:,77), t3x12(:,:,14))
  call prop_Q_A(ntry, wf12(:,27), Q(:,50), ZERO, 0_intkind1, wf12(:,28), n2(34))
  call vert_AV_Q(ntry, wf8(:,20), ex5, wf16(:,16), n3(:,78), t3x16(:,:,16))
  call vert_QA_V(ntry, wf12(:,28), ex1, wf24(:,25), n3(:,79), t3x24(:,:,25))
  call vert_WQ_A(ntry, wf6(:,6), ex2, wf12(:,29), n3(:,80), t3x12(:,:,15))
  call prop_Q_A(ntry, wf12(:,29), Q(:,50), ZERO, 0_intkind1, wf12(:,30), n2(35))
  call vert_WQ_A(ntry, wf6(:,6), wf4(:,12), wf24(:,26), n3(:,81), t3x24(:,:,26))
  call vert_QA_V(ntry, ex2, wf12(:,20), wf24(:,27), n3(:,82), t3x24(:,:,27))
  call vert_QA_V(ntry, wf12(:,30), ex1, wf24(:,28), n3(:,83), t3x24(:,:,28))
  call vert_QA_V(ntry, wf4(:,5), ex4, wf8(:,23), n3(:,84), t3x8(:,:,13))
  call vert_AV_Q(ntry, ex4, ex7, wf4(:,15), n3(:,85), t3x4(:,:,9))
  call prop_A_Q(ntry, wf4(:,15), Q(:,72), ZERO, 0_intkind1, wf4(:,16), n2(36))
  call vert_QA_V(ntry, ex3, wf4(:,16), wf8(:,24), n3(:,86), t3x8(:,:,14))
  call vert_QA_V(ntry, wf4(:,12), ex4, wf8(:,25), n3(:,87), t3x8(:,:,15))
  call vert_QA_V(ntry, ex2, wf4(:,16), wf8(:,26), n3(:,88), t3x8(:,:,16))
  call vert_VQ_A(ntry, wf8(:,25), ex3, wf16(:,17), n3(:,89), t3x16(:,:,17))
  call vert_VQ_A(ntry, wf8(:,23), ex2, wf16(:,18), n3(:,90), t3x16(:,:,18))
  call vert_VQ_A(ntry, wf8(:,26), ex3, wf16(:,19), n3(:,91), t3x16(:,:,19))
  call vert_VQ_A(ntry, wf8(:,24), ex2, wf16(:,20), n3(:,92), t3x16(:,:,20))
  call vert_QA_V(ntry, wf4(:,14), ex4, wf8(:,27), n3(:,93), t3x8(:,:,17))
  call vert_QA_V(ntry, wf4(:,5), wf6(:,4), wf24(:,29), n3(:,94), t3x24(:,:,29))
  call vert_QA_V(ntry, ex3, wf6(:,4), wf12(:,31), n3(:,95), t3x12(:,:,16))
  call vert_QA_V(ntry, wf4(:,14), wf4(:,16), wf16(:,21), n3(:,96), t3x16(:,:,21))
  call vert_UV_W(ntry, wf12(:,31), Q(:,37), ex7, Q(:,64), wf24(:,30), n3(:,97), t3x24(:,:,30))
  call vert_AV_Q(ntry, ex4, wf12(:,31), wf24(:,31), n3(:,98), t3x24(:,:,31))
  call vert_VQ_A(ntry, wf8(:,27), ex3, wf16(:,22), n3(:,99), t3x16(:,:,22))
  call vert_QA_V(ntry, wf6(:,2), wf4(:,9), wf24(:,32), n3(:,100), t3x24(:,:,32))
  call vert_QA_V(ntry, wf6(:,2), ex1, wf12(:,32), n3(:,101), t3x12(:,:,17))
  call vert_AV_Q(ntry, ex4, wf12(:,32), wf24(:,33), n3(:,102), t3x24(:,:,33))
  call vert_UV_W(ntry, wf12(:,32), Q(:,37), ex7, Q(:,64), wf24(:,34), n3(:,103), t3x24(:,:,34))
  call vert_AV_Q(ntry, ex1, wf8(:,27), wf16(:,23), n3(:,104), t3x16(:,:,23))
  call vert_AV_Q(ntry, ex1, wf8(:,23), wf16(:,24), n3(:,105), t3x16(:,:,24))
  call vert_AV_Q(ntry, ex1, wf8(:,24), wf16(:,25), n3(:,106), t3x16(:,:,25))
  call vert_QA_V(ntry, wf4(:,7), ex4, wf8(:,28), n3(:,107), t3x8(:,:,18))
  call vert_QA_V(ntry, wf4(:,12), wf6(:,4), wf24(:,35), n3(:,108), t3x24(:,:,35))
  call vert_QA_V(ntry, ex2, wf6(:,4), wf12(:,33), n3(:,109), t3x12(:,:,18))
  call vert_QA_V(ntry, wf4(:,7), wf4(:,16), wf16(:,26), n3(:,110), t3x16(:,:,26))
  call vert_AV_Q(ntry, ex4, wf12(:,33), wf24(:,36), n3(:,111), t3x24(:,:,36))
  call vert_UV_W(ntry, wf12(:,33), Q(:,35), ex7, Q(:,64), wf24(:,37), n3(:,112), t3x24(:,:,37))
  call vert_VQ_A(ntry, wf8(:,28), ex2, wf16(:,27), n3(:,113), t3x16(:,:,27))
  call vert_QA_V(ntry, wf6(:,8), wf4(:,9), wf24(:,38), n3(:,114), t3x24(:,:,38))
  call vert_QA_V(ntry, wf6(:,8), ex1, wf12(:,34), n3(:,115), t3x12(:,:,19))
  call vert_AV_Q(ntry, ex4, wf12(:,34), wf24(:,39), n3(:,116), t3x24(:,:,39))
  call vert_UV_W(ntry, wf12(:,34), Q(:,35), ex7, Q(:,64), wf24(:,40), n3(:,117), t3x24(:,:,40))
  call vert_AV_Q(ntry, ex1, wf8(:,28), wf16(:,28), n3(:,118), t3x16(:,:,28))
  call vert_AV_Q(ntry, ex1, wf8(:,25), wf16(:,29), n3(:,119), t3x16(:,:,29))
  call vert_AV_Q(ntry, ex1, wf8(:,26), wf16(:,30), n3(:,120), t3x16(:,:,30))
  call vert_AV_Q(ntry, ex4, ex5, wf4(:,17), n3(:,121), t3x4(:,:,10))
  call prop_A_Q(ntry, wf4(:,17), Q(:,24), ZERO, 0_intkind1, wf4(:,18), n2(37))
  call vert_QA_V(ntry, wf4(:,12), wf4(:,18), wf16(:,31), n3(:,122), t3x16(:,:,31))
  call vert_QA_V(ntry, ex3, wf4(:,18), wf8(:,29), n3(:,123), t3x8(:,:,19))
  call vert_QA_V(ntry, wf4(:,5), wf4(:,18), wf16(:,32), n3(:,124), t3x16(:,:,32))
  call vert_QA_V(ntry, ex2, wf4(:,18), wf8(:,30), n3(:,125), t3x8(:,:,20))
  call vert_AV_Q(ntry, wf4(:,18), ex7, wf8(:,31), n3(:,126), t3x8(:,:,21))
  call prop_A_Q(ntry, wf8(:,31), Q(:,88), ZERO, 0_intkind1, wf8(:,32), n2(38))
  call vert_VQ_A(ntry, wf12(:,33), ex3, wf24(:,41), n3(:,127), t3x24(:,:,41))
  call vert_VQ_A(ntry, wf12(:,31), ex2, wf24(:,42), n3(:,128), t3x24(:,:,42))
  call vert_UV_W(ntry, wf8(:,30), Q(:,26), ex7, Q(:,64), wf16(:,33), n3(:,129), t3x16(:,:,33))
  call vert_VQ_A(ntry, wf8(:,30), ex3, wf16(:,34), n3(:,130), t3x16(:,:,34))
  call vert_VQ_A(ntry, wf8(:,29), ex2, wf16(:,35), n3(:,131), t3x16(:,:,35))
  call vert_VQ_A(ntry, wf12(:,34), ex3, wf24(:,43), n3(:,132), t3x24(:,:,43))
  call vert_AV_Q(ntry, ex1, wf8(:,29), wf16(:,36), n3(:,133), t3x16(:,:,36))
  call vert_VQ_A(ntry, wf12(:,32), ex2, wf24(:,44), n3(:,134), t3x24(:,:,44))
  call vert_AV_Q(ntry, ex1, wf8(:,30), wf16(:,37), n3(:,135), t3x16(:,:,37))
  call vert_AV_Q(ntry, wf4(:,16), ex5, wf8(:,33), n3(:,136), t3x8(:,:,22))
  call prop_A_Q(ntry, wf8(:,33), Q(:,88), ZERO, 0_intkind1, wf8(:,34), n2(39))


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
  M2add = M2 / average_factor_ppwajj_uxdddxawxg_1

  do k = 0, 30-1
    M2(k) = M2add(extcomb_perm_ppwajj_uxdddxawxg_1(k))
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

    call cont_QA(nsync, wf24(:,1), wf8(:,2), A(:,1), n3(:,137), t3x192(:,:,1), nhel, den(7))
    call cont_VV(nsync, wf8(:,3), wf24(:,2), A(:,2), n3(:,138), t3x192(:,:,2), nhel, den(11))
    call cont_QA(nsync, wf16(:,1), wf12(:,2), A(:,3), n3(:,139), t3x192(:,:,3), nhel, den(15))
    call cont_QA(nsync, wf16(:,2), wf12(:,4), A(:,4), n3(:,140), t3x192(:,:,4), nhel, den(20))
    call cont_QA(nsync, wf16(:,1), wf12(:,6), A(:,5), n3(:,141), t3x192(:,:,5), nhel, den(22))
    call cont_QA(nsync, wf12(:,4), wf16(:,3), A(:,6), n3(:,142), t3x192(:,:,6), nhel, den(25))
    call cont_VV(nsync, wf8(:,3), wf24(:,3), A(:,7), n3(:,143), t3x192(:,:,7), nhel, den(26))
    call cont_QA(nsync, wf8(:,2), wf24(:,4), A(:,8), n3(:,144), t3x192(:,:,8), nhel, den(27))
    call cont_QA(nsync, wf16(:,4), wf12(:,8), A(:,9), n3(:,145), t3x192(:,:,9), nhel, den(33))
    call cont_VV(nsync, wf8(:,3), wf24(:,5), A(:,10), n3(:,146), t3x192(:,:,10), nhel, den(35))
    call cont_QA(nsync, wf24(:,6), wf8(:,7), A(:,11), n3(:,147), t3x192(:,:,11), nhel, den(39))
    call cont_QA(nsync, wf16(:,4), wf12(:,10), A(:,12), n3(:,148), t3x192(:,:,12), nhel, den(42))
    call cont_QA(nsync, wf16(:,5), wf12(:,12), A(:,13), n3(:,149), t3x192(:,:,13), nhel, den(46))
    call cont_QA(nsync, wf8(:,7), wf24(:,7), A(:,14), n3(:,150), t3x192(:,:,14), nhel, den(49))
    call cont_QA(nsync, wf12(:,12), wf16(:,6), A(:,15), n3(:,151), t3x192(:,:,15), nhel, den(50))
    call cont_VV(nsync, wf8(:,3), wf24(:,8), A(:,16), n3(:,152), t3x192(:,:,16), nhel, den(51))
    call cont_QA(nsync, wf16(:,2), wf12(:,14), A(:,17), n3(:,153), t3x192(:,:,17), nhel, den(53))
    call cont_QA(nsync, wf24(:,6), wf8(:,11), A(:,18), n3(:,154), t3x192(:,:,18), nhel, den(55))
    call cont_QA(nsync, wf16(:,3), wf12(:,14), A(:,19), n3(:,155), t3x192(:,:,19), nhel, den(56))
    call cont_VV(nsync, wf8(:,3), wf24(:,9), A(:,20), n3(:,156), t3x192(:,:,20), nhel, den(57))
    call cont_QA(nsync, wf12(:,8), wf16(:,7), A(:,21), n3(:,157), t3x192(:,:,21), nhel, den(58))
    call cont_QA(nsync, wf24(:,1), wf8(:,13), A(:,22), n3(:,158), t3x192(:,:,22), nhel, den(60))
    call cont_QA(nsync, wf16(:,5), wf12(:,16), A(:,23), n3(:,159), t3x192(:,:,23), nhel, den(62))
    call cont_QA(nsync, wf12(:,2), wf16(:,8), A(:,24), n3(:,160), t3x192(:,:,24), nhel, den(63))
    call cont_QA(nsync, wf16(:,6), wf12(:,16), A(:,25), n3(:,161), t3x192(:,:,25), nhel, den(64))
    call cont_VV(nsync, wf8(:,3), wf24(:,10), A(:,26), n3(:,162), t3x192(:,:,26), nhel, den(65))
    call cont_QA(nsync, wf8(:,5), wf24(:,11), A(:,27), n3(:,163), t3x192(:,:,27), nhel, den(68))
    call cont_QA(nsync, wf16(:,5), wf12(:,18), A(:,28), n3(:,164), t3x192(:,:,28), nhel, den(70))
    call cont_QA(nsync, wf8(:,9), wf24(:,12), A(:,29), n3(:,165), t3x192(:,:,29), nhel, den(72))
    call cont_QA(nsync, wf16(:,2), wf12(:,20), A(:,30), n3(:,166), t3x192(:,:,30), nhel, den(74))
    call cont_QA(nsync, wf16(:,6), wf12(:,18), A(:,31), n3(:,167), t3x192(:,:,31), nhel, den(75))
    call cont_VV(nsync, wf8(:,3), wf24(:,13), A(:,32), n3(:,168), t3x192(:,:,32), nhel, den(76))
    call cont_VV(nsync, wf8(:,3), wf24(:,14), A(:,33), n3(:,169), t3x192(:,:,33), nhel, den(77))
    call cont_QA(nsync, wf8(:,5), wf24(:,15), A(:,34), n3(:,170), t3x192(:,:,34), nhel, den(78))
    call cont_QA(nsync, wf24(:,4), wf8(:,13), A(:,35), n3(:,171), t3x192(:,:,35), nhel, den(79))
    call cont_QA(nsync, wf12(:,10), wf16(:,7), A(:,36), n3(:,172), t3x192(:,:,36), nhel, den(80))
    call cont_QA(nsync, wf12(:,6), wf16(:,8), A(:,37), n3(:,173), t3x192(:,:,37), nhel, den(81))
    call cont_QA(nsync, wf24(:,7), wf8(:,11), A(:,38), n3(:,174), t3x192(:,:,38), nhel, den(82))
    call cont_QA(nsync, wf8(:,2), wf24(:,16), A(:,39), n3(:,175), t3x192(:,:,39), nhel, den(86))
    call cont_QA(nsync, wf16(:,9), wf12(:,22), A(:,40), n3(:,176), t3x192(:,:,40), nhel, den(90))
    call cont_VV(nsync, wf8(:,14), wf24(:,17), A(:,41), n3(:,177), t3x192(:,:,41), nhel, den(94))
    call cont_QA(nsync, wf12(:,4), wf16(:,10), A(:,42), n3(:,178), t3x192(:,:,42), nhel, den(97))
    call cont_QA(nsync, wf16(:,9), wf12(:,24), A(:,43), n3(:,179), t3x192(:,:,43), nhel, den(99))
    call cont_QA(nsync, wf12(:,4), wf16(:,11), A(:,44), n3(:,180), t3x192(:,:,44), nhel, den(101))
    call cont_VV(nsync, wf8(:,14), wf24(:,18), A(:,45), n3(:,181), t3x192(:,:,45), nhel, den(102))
    call cont_QA(nsync, wf8(:,2), wf24(:,19), A(:,46), n3(:,182), t3x192(:,:,46), nhel, den(103))
    call cont_QA(nsync, wf12(:,8), wf16(:,12), A(:,47), n3(:,183), t3x192(:,:,47), nhel, den(106))
    call cont_QA(nsync, wf24(:,20), wf8(:,18), A(:,48), n3(:,184), t3x192(:,:,48), nhel, den(110))
    call cont_VV(nsync, wf8(:,14), wf24(:,21), A(:,49), n3(:,185), t3x192(:,:,49), nhel, den(112))
    call cont_QA(nsync, wf12(:,10), wf16(:,12), A(:,50), n3(:,186), t3x192(:,:,50), nhel, den(113))
    call cont_QA(nsync, wf16(:,13), wf12(:,26), A(:,51), n3(:,187), t3x192(:,:,51), nhel, den(117))
    call cont_QA(nsync, wf8(:,18), wf24(:,22), A(:,52), n3(:,188), t3x192(:,:,52), nhel, den(120))
    call cont_QA(nsync, wf12(:,26), wf16(:,14), A(:,53), n3(:,189), t3x192(:,:,53), nhel, den(121))
    call cont_VV(nsync, wf8(:,14), wf24(:,23), A(:,54), n3(:,190), t3x192(:,:,54), nhel, den(122))
    call cont_QA(nsync, wf12(:,14), wf16(:,10), A(:,55), n3(:,191), t3x192(:,:,55), nhel, den(123))
    call cont_QA(nsync, wf24(:,20), wf8(:,22), A(:,56), n3(:,192), t3x192(:,:,56), nhel, den(125))
    call cont_QA(nsync, wf12(:,14), wf16(:,11), A(:,57), n3(:,193), t3x192(:,:,57), nhel, den(126))
    call cont_VV(nsync, wf8(:,14), wf24(:,24), A(:,58), n3(:,194), t3x192(:,:,58), nhel, den(127))
    call cont_QA(nsync, wf12(:,8), wf16(:,15), A(:,59), n3(:,195), t3x192(:,:,59), nhel, den(128))
    call cont_QA(nsync, wf8(:,13), wf24(:,16), A(:,60), n3(:,196), t3x192(:,:,60), nhel, den(129))
    call cont_QA(nsync, wf16(:,13), wf12(:,28), A(:,61), n3(:,197), t3x192(:,:,61), nhel, den(131))
    call cont_QA(nsync, wf12(:,22), wf16(:,16), A(:,62), n3(:,198), t3x192(:,:,62), nhel, den(132))
    call cont_QA(nsync, wf16(:,14), wf12(:,28), A(:,63), n3(:,199), t3x192(:,:,63), nhel, den(133))
    call cont_VV(nsync, wf8(:,14), wf24(:,25), A(:,64), n3(:,200), t3x192(:,:,64), nhel, den(134))
    call cont_QA(nsync, wf24(:,11), wf8(:,16), A(:,65), n3(:,201), t3x192(:,:,65), nhel, den(135))
    call cont_QA(nsync, wf16(:,13), wf12(:,30), A(:,66), n3(:,202), t3x192(:,:,66), nhel, den(137))
    call cont_QA(nsync, wf8(:,20), wf24(:,26), A(:,67), n3(:,203), t3x192(:,:,67), nhel, den(139))
    call cont_QA(nsync, wf12(:,20), wf16(:,10), A(:,68), n3(:,204), t3x192(:,:,68), nhel, den(140))
    call cont_VV(nsync, wf8(:,14), wf24(:,27), A(:,69), n3(:,205), t3x192(:,:,69), nhel, den(141))
    call cont_VV(nsync, wf8(:,14), wf24(:,28), A(:,70), n3(:,206), t3x192(:,:,70), nhel, den(142))
    call cont_QA(nsync, wf16(:,14), wf12(:,30), A(:,71), n3(:,207), t3x192(:,:,71), nhel, den(143))
    call cont_QA(nsync, wf24(:,15), wf8(:,16), A(:,72), n3(:,208), t3x192(:,:,72), nhel, den(144))
    call cont_QA(nsync, wf8(:,13), wf24(:,19), A(:,73), n3(:,209), t3x192(:,:,73), nhel, den(145))
    call cont_QA(nsync, wf12(:,10), wf16(:,15), A(:,74), n3(:,210), t3x192(:,:,74), nhel, den(146))
    call cont_QA(nsync, wf12(:,24), wf16(:,16), A(:,75), n3(:,211), t3x192(:,:,75), nhel, den(147))
    call cont_QA(nsync, wf24(:,22), wf8(:,22), A(:,76), n3(:,212), t3x192(:,:,76), nhel, den(148))
    call cont_VV(nsync, wf24(:,17), wf8(:,23), A(:,77), n3(:,213), t3x192(:,:,77), nhel, den(150))
    call cont_VV(nsync, wf24(:,17), wf8(:,24), A(:,78), n3(:,214), t3x192(:,:,78), nhel, den(153))
    call cont_VV(nsync, wf24(:,2), wf8(:,25), A(:,79), n3(:,215), t3x192(:,:,79), nhel, den(155))
    call cont_VV(nsync, wf24(:,2), wf8(:,26), A(:,80), n3(:,216), t3x192(:,:,80), nhel, den(157))
    call cont_QA(nsync, wf12(:,4), wf16(:,17), A(:,81), n3(:,217), t3x192(:,:,81), nhel, den(158))
    call cont_QA(nsync, wf12(:,4), wf16(:,18), A(:,82), n3(:,218), t3x192(:,:,82), nhel, den(159))
    call cont_QA(nsync, wf12(:,4), wf16(:,19), A(:,83), n3(:,219), t3x192(:,:,83), nhel, den(160))
    call cont_QA(nsync, wf12(:,4), wf16(:,20), A(:,84), n3(:,220), t3x192(:,:,84), nhel, den(161))
    call cont_VV(nsync, wf8(:,27), wf24(:,29), A(:,85), n3(:,221), t3x192(:,:,85), nhel, den(165))
    call cont_VV(nsync, wf24(:,21), wf8(:,23), A(:,86), n3(:,222), t3x192(:,:,86), nhel, den(166))
    call cont_VV(nsync, wf12(:,31), wf16(:,21), A(:,87), n3(:,223), t3x192(:,:,87), nhel, den(170))
    call cont_VV(nsync, wf24(:,21), wf8(:,24), A(:,88), n3(:,224), t3x192(:,:,88), nhel, den(171))
    call cont_VV(nsync, wf8(:,27), wf24(:,30), A(:,89), n3(:,225), t3x192(:,:,89), nhel, den(172))
    call cont_QA(nsync, wf8(:,18), wf24(:,31), A(:,90), n3(:,226), t3x192(:,:,90), nhel, den(173))
    call cont_QA(nsync, wf12(:,8), wf16(:,22), A(:,91), n3(:,227), t3x192(:,:,91), nhel, den(174))
    call cont_VV(nsync, wf8(:,27), wf24(:,32), A(:,92), n3(:,228), t3x192(:,:,92), nhel, den(176))
    call cont_VV(nsync, wf16(:,21), wf12(:,32), A(:,93), n3(:,229), t3x192(:,:,93), nhel, den(178))
    call cont_QA(nsync, wf8(:,18), wf24(:,33), A(:,94), n3(:,230), t3x192(:,:,94), nhel, den(179))
    call cont_VV(nsync, wf8(:,27), wf24(:,34), A(:,95), n3(:,231), t3x192(:,:,95), nhel, den(180))
    call cont_QA(nsync, wf12(:,2), wf16(:,23), A(:,96), n3(:,232), t3x192(:,:,96), nhel, den(181))
    call cont_QA(nsync, wf12(:,10), wf16(:,22), A(:,97), n3(:,233), t3x192(:,:,97), nhel, den(182))
    call cont_QA(nsync, wf12(:,6), wf16(:,23), A(:,98), n3(:,234), t3x192(:,:,98), nhel, den(183))
    call cont_QA(nsync, wf12(:,26), wf16(:,24), A(:,99), n3(:,235), t3x192(:,:,99), nhel, den(184))
    call cont_QA(nsync, wf12(:,26), wf16(:,25), A(:,100), n3(:,236), t3x192(:,:,100), nhel, den(185))
    call cont_VV(nsync, wf24(:,5), wf8(:,25), A(:,101), n3(:,237), t3x192(:,:,101), nhel, den(186))
    call cont_VV(nsync, wf8(:,28), wf24(:,35), A(:,102), n3(:,238), t3x192(:,:,102), nhel, den(190))
    call cont_VV(nsync, wf12(:,33), wf16(:,26), A(:,103), n3(:,239), t3x192(:,:,103), nhel, den(194))
    call cont_VV(nsync, wf24(:,5), wf8(:,26), A(:,104), n3(:,240), t3x192(:,:,104), nhel, den(195))
    call cont_QA(nsync, wf8(:,7), wf24(:,36), A(:,105), n3(:,241), t3x192(:,:,105), nhel, den(196))
    call cont_VV(nsync, wf8(:,28), wf24(:,37), A(:,106), n3(:,242), t3x192(:,:,106), nhel, den(197))
    call cont_QA(nsync, wf12(:,8), wf16(:,27), A(:,107), n3(:,243), t3x192(:,:,107), nhel, den(198))
    call cont_VV(nsync, wf8(:,28), wf24(:,38), A(:,108), n3(:,244), t3x192(:,:,108), nhel, den(200))
    call cont_VV(nsync, wf16(:,26), wf12(:,34), A(:,109), n3(:,245), t3x192(:,:,109), nhel, den(202))
    call cont_QA(nsync, wf8(:,7), wf24(:,39), A(:,110), n3(:,246), t3x192(:,:,110), nhel, den(203))
    call cont_VV(nsync, wf8(:,28), wf24(:,40), A(:,111), n3(:,247), t3x192(:,:,111), nhel, den(204))
    call cont_QA(nsync, wf12(:,22), wf16(:,28), A(:,112), n3(:,248), t3x192(:,:,112), nhel, den(205))
    call cont_QA(nsync, wf12(:,10), wf16(:,27), A(:,113), n3(:,249), t3x192(:,:,113), nhel, den(206))
    call cont_QA(nsync, wf12(:,12), wf16(:,29), A(:,114), n3(:,250), t3x192(:,:,114), nhel, den(207))
    call cont_QA(nsync, wf12(:,24), wf16(:,28), A(:,115), n3(:,251), t3x192(:,:,115), nhel, den(208))
    call cont_QA(nsync, wf12(:,12), wf16(:,30), A(:,116), n3(:,252), t3x192(:,:,116), nhel, den(209))
    call cont_VV(nsync, wf12(:,31), wf16(:,31), A(:,117), n3(:,253), t3x192(:,:,117), nhel, den(212))
    call cont_VV(nsync, wf24(:,35), wf8(:,29), A(:,118), n3(:,254), t3x192(:,:,118), nhel, den(214))
    call cont_VV(nsync, wf12(:,33), wf16(:,32), A(:,119), n3(:,255), t3x192(:,:,119), nhel, den(216))
    call cont_VV(nsync, wf24(:,29), wf8(:,30), A(:,120), n3(:,256), t3x192(:,:,120), nhel, den(218))
    call cont_QA(nsync, wf8(:,32), wf24(:,41), A(:,121), n3(:,257), t3x192(:,:,121), nhel, den(221))
    call cont_VV(nsync, wf24(:,37), wf8(:,29), A(:,122), n3(:,258), t3x192(:,:,122), nhel, den(222))
    call cont_QA(nsync, wf8(:,32), wf24(:,42), A(:,123), n3(:,259), t3x192(:,:,123), nhel, den(223))
    call cont_VV(nsync, wf12(:,31), wf16(:,33), A(:,124), n3(:,260), t3x192(:,:,124), nhel, den(224))
    call cont_QA(nsync, wf12(:,8), wf16(:,34), A(:,125), n3(:,261), t3x192(:,:,125), nhel, den(225))
    call cont_QA(nsync, wf12(:,8), wf16(:,35), A(:,126), n3(:,262), t3x192(:,:,126), nhel, den(226))
    call cont_VV(nsync, wf24(:,38), wf8(:,29), A(:,127), n3(:,263), t3x192(:,:,127), nhel, den(227))
    call cont_VV(nsync, wf12(:,34), wf16(:,32), A(:,128), n3(:,264), t3x192(:,:,128), nhel, den(228))
    call cont_QA(nsync, wf8(:,32), wf24(:,43), A(:,129), n3(:,265), t3x192(:,:,129), nhel, den(229))
    call cont_VV(nsync, wf24(:,40), wf8(:,29), A(:,130), n3(:,266), t3x192(:,:,130), nhel, den(230))
    call cont_QA(nsync, wf12(:,22), wf16(:,36), A(:,131), n3(:,267), t3x192(:,:,131), nhel, den(231))
    call cont_VV(nsync, wf24(:,32), wf8(:,30), A(:,132), n3(:,268), t3x192(:,:,132), nhel, den(232))
    call cont_VV(nsync, wf12(:,32), wf16(:,31), A(:,133), n3(:,269), t3x192(:,:,133), nhel, den(233))
    call cont_QA(nsync, wf8(:,32), wf24(:,44), A(:,134), n3(:,270), t3x192(:,:,134), nhel, den(234))
    call cont_QA(nsync, wf12(:,2), wf16(:,37), A(:,135), n3(:,271), t3x192(:,:,135), nhel, den(235))
    call cont_VV(nsync, wf24(:,34), wf8(:,30), A(:,136), n3(:,272), t3x192(:,:,136), nhel, den(236))
    call cont_QA(nsync, wf12(:,10), wf16(:,34), A(:,137), n3(:,273), t3x192(:,:,137), nhel, den(237))
    call cont_QA(nsync, wf12(:,10), wf16(:,35), A(:,138), n3(:,274), t3x192(:,:,138), nhel, den(238))
    call cont_QA(nsync, wf12(:,24), wf16(:,36), A(:,139), n3(:,275), t3x192(:,:,139), nhel, den(239))
    call cont_QA(nsync, wf12(:,6), wf16(:,37), A(:,140), n3(:,276), t3x192(:,:,140), nhel, den(240))
    call cont_QA(nsync, wf8(:,22), wf24(:,31), A(:,141), n3(:,277), t3x192(:,:,141), nhel, den(241))
    call cont_QA(nsync, wf12(:,14), wf16(:,17), A(:,142), n3(:,278), t3x192(:,:,142), nhel, den(242))
    call cont_QA(nsync, wf8(:,11), wf24(:,36), A(:,143), n3(:,279), t3x192(:,:,143), nhel, den(243))
    call cont_QA(nsync, wf12(:,14), wf16(:,18), A(:,144), n3(:,280), t3x192(:,:,144), nhel, den(244))
    call cont_QA(nsync, wf24(:,41), wf8(:,34), A(:,145), n3(:,281), t3x192(:,:,145), nhel, den(246))
    call cont_QA(nsync, wf24(:,42), wf8(:,34), A(:,146), n3(:,282), t3x192(:,:,146), nhel, den(247))
    call cont_QA(nsync, wf12(:,14), wf16(:,19), A(:,147), n3(:,283), t3x192(:,:,147), nhel, den(248))
    call cont_QA(nsync, wf12(:,14), wf16(:,20), A(:,148), n3(:,284), t3x192(:,:,148), nhel, den(249))
    call cont_QA(nsync, wf8(:,11), wf24(:,39), A(:,149), n3(:,285), t3x192(:,:,149), nhel, den(250))
    call cont_QA(nsync, wf12(:,28), wf16(:,24), A(:,150), n3(:,286), t3x192(:,:,150), nhel, den(251))
    call cont_QA(nsync, wf24(:,43), wf8(:,34), A(:,151), n3(:,287), t3x192(:,:,151), nhel, den(252))
    call cont_QA(nsync, wf12(:,28), wf16(:,25), A(:,152), n3(:,288), t3x192(:,:,152), nhel, den(253))
    call cont_QA(nsync, wf8(:,22), wf24(:,33), A(:,153), n3(:,289), t3x192(:,:,153), nhel, den(254))
    call cont_QA(nsync, wf12(:,16), wf16(:,29), A(:,154), n3(:,290), t3x192(:,:,154), nhel, den(255))
    call cont_QA(nsync, wf24(:,44), wf8(:,34), A(:,155), n3(:,291), t3x192(:,:,155), nhel, den(256))
    call cont_QA(nsync, wf12(:,16), wf16(:,30), A(:,156), n3(:,292), t3x192(:,:,156), nhel, den(257))
    call cont_VV(nsync, wf24(:,14), wf8(:,25), A(:,157), n3(:,293), t3x192(:,:,157), nhel, den(258))
    call cont_VV(nsync, wf24(:,13), wf8(:,25), A(:,158), n3(:,294), t3x192(:,:,158), nhel, den(259))
    call cont_VV(nsync, wf24(:,28), wf8(:,23), A(:,159), n3(:,295), t3x192(:,:,159), nhel, den(260))
    call cont_VV(nsync, wf24(:,27), wf8(:,23), A(:,160), n3(:,296), t3x192(:,:,160), nhel, den(261))
    call cont_QA(nsync, wf12(:,18), wf16(:,30), A(:,161), n3(:,297), t3x192(:,:,161), nhel, den(262))
    call cont_VV(nsync, wf24(:,28), wf8(:,24), A(:,162), n3(:,298), t3x192(:,:,162), nhel, den(263))
    call cont_VV(nsync, wf24(:,27), wf8(:,24), A(:,163), n3(:,299), t3x192(:,:,163), nhel, den(264))
    call cont_VV(nsync, wf24(:,13), wf8(:,26), A(:,164), n3(:,300), t3x192(:,:,164), nhel, den(265))

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
  complex(REALKIND), intent(out) :: M1(4) ! M1helarray(4,192)
  integer :: empty(0)

  M1(1) = ((A(j,47)%j+A(j,48)%j+A(j,50)%j+A(j,51)%j+A(j,52)%j+A(j,53)%j+A(j,55)%j+A(j,56)%j+A(j,57)%j+A(j,59)%j+A(j,74)%j &
       +A(j,76)%j+A(j,102)%j+A(j,107)%j+A(j,108)%j+A(j,112)%j+A(j,113)%j+A(j,115)%j+A(j,118)%j+A(j,126)%j+A(j,127)%j+A(j,131)%j &
       +A(j,138)%j+A(j,139)%j)*f(1))/6._/**/REALKIND+((A(j,9)%j+A(j,12)%j+A(j,13)%j+A(j,19)%j+A(j,21)%j+A(j,36)%j+A(j,90)%j &
       +A(j,91)%j+A(j,92)%j+A(j,94)%j+A(j,97)%j+A(j,101)%j+A(j,114)%j+A(j,117)%j+A(j,125)%j+A(j,132)%j+A(j,133)%j+A(j,137)%j &
       +A(j,141)%j+A(j,142)%j+A(j,153)%j)*f(1))/2._/**/REALKIND+((-A(j,39)%j-A(j,40)%j-A(j,42)%j-A(j,43)%j-A(j,44)%j-A(j,46)%j &
       -A(j,60)%j-A(j,61)%j-A(j,62)%j-A(j,63)%j-A(j,73)%j-A(j,75)%j)*f(2))/6._/**/REALKIND+((-A(j,1)%j-A(j,6)%j-A(j,8)%j-A(j,22)%j &
       -A(j,23)%j-A(j,35)%j-A(j,79)%j-A(j,81)%j-A(j,154)%j)*f(2))/2._/**/REALKIND+((-A(j,65)%j-A(j,66)%j-A(j,67)%j-A(j,68)%j &
       -A(j,71)%j-A(j,72)%j)*f(3))/6._/**/REALKIND+((-A(j,27)%j-A(j,28)%j-A(j,34)%j-A(j,157)%j-A(j,158)%j)*f(3))/2._/**/REALKIND &
       +(CI*(-A(j,10)%j-A(j,16)%j-A(j,20)%j+A(j,89)%j+A(j,95)%j-A(j,124)%j+A(j,136)%j)*f(4))/2._/**/REALKIND+(CI*(A(j,2)%j &
       +A(j,7)%j+A(j,26)%j)*f(5))/2._/**/REALKIND+(CI*(A(j,32)%j+A(j,33)%j)*f(6))/2._/**/REALKIND
  M1(2) = ((-A(j,9)%j-A(j,11)%j-A(j,12)%j-A(j,13)%j-A(j,14)%j-A(j,15)%j-A(j,17)%j-A(j,18)%j-A(j,19)%j-A(j,21)%j-A(j,36)%j &
       -A(j,38)%j-A(j,85)%j-A(j,91)%j-A(j,92)%j-A(j,96)%j-A(j,97)%j-A(j,98)%j-A(j,120)%j-A(j,125)%j-A(j,132)%j-A(j,135)%j &
       -A(j,137)%j-A(j,140)%j)*f(1))/6._/**/REALKIND+((-A(j,47)%j-A(j,50)%j-A(j,51)%j-A(j,57)%j-A(j,59)%j-A(j,74)%j-A(j,86)%j &
       -A(j,99)%j-A(j,105)%j-A(j,107)%j-A(j,108)%j-A(j,110)%j-A(j,113)%j-A(j,119)%j-A(j,126)%j-A(j,127)%j-A(j,128)%j-A(j,138)%j &
       -A(j,143)%j-A(j,144)%j-A(j,149)%j)*f(1))/2._/**/REALKIND+((A(j,1)%j+A(j,3)%j+A(j,4)%j+A(j,5)%j+A(j,6)%j+A(j,8)%j+A(j,22)%j &
       +A(j,23)%j+A(j,24)%j+A(j,25)%j+A(j,35)%j+A(j,37)%j)*f(2))/6._/**/REALKIND+((A(j,39)%j+A(j,44)%j+A(j,46)%j+A(j,60)%j &
       +A(j,61)%j+A(j,73)%j+A(j,77)%j+A(j,82)%j+A(j,150)%j)*f(2))/2._/**/REALKIND+((A(j,27)%j+A(j,28)%j+A(j,29)%j+A(j,30)%j &
       +A(j,31)%j+A(j,34)%j)*f(3))/6._/**/REALKIND+((A(j,65)%j+A(j,66)%j+A(j,72)%j+A(j,159)%j+A(j,160)%j)*f(3))/2._/**/REALKIND &
       +(CI*(A(j,49)%j+A(j,54)%j+A(j,58)%j-A(j,106)%j-A(j,111)%j-A(j,122)%j-A(j,130)%j)*f(4))/2._/**/REALKIND+(CI*(-A(j,41)%j &
       -A(j,45)%j-A(j,64)%j)*f(5))/2._/**/REALKIND+(CI*(-A(j,69)%j-A(j,70)%j)*f(6))/2._/**/REALKIND
  M1(3) = ((-A(j,48)%j-A(j,52)%j-A(j,53)%j-A(j,55)%j-A(j,56)%j-A(j,76)%j-A(j,88)%j-A(j,100)%j-A(j,102)%j-A(j,103)%j-A(j,109)%j &
       -A(j,112)%j-A(j,115)%j-A(j,118)%j-A(j,121)%j-A(j,129)%j-A(j,131)%j-A(j,139)%j-A(j,145)%j-A(j,148)%j &
       -A(j,151)%j)*f(1))/2._/**/REALKIND+((-A(j,87)%j-A(j,90)%j-A(j,93)%j-A(j,94)%j-A(j,101)%j-A(j,104)%j-A(j,114)%j-A(j,116)%j &
       -A(j,117)%j-A(j,123)%j-A(j,133)%j-A(j,134)%j-A(j,141)%j-A(j,142)%j-A(j,146)%j-A(j,147)%j-A(j,153)%j &
       -A(j,155)%j)*f(1))/6._/**/REALKIND+((A(j,40)%j+A(j,42)%j+A(j,43)%j+A(j,62)%j+A(j,63)%j+A(j,75)%j+A(j,78)%j+A(j,84)%j &
       +A(j,152)%j)*f(2))/2._/**/REALKIND+((A(j,79)%j+A(j,80)%j+A(j,81)%j+A(j,83)%j+A(j,154)%j+A(j,156)%j)*f(2))/6._/**/REALKIND &
       +((A(j,67)%j+A(j,68)%j+A(j,71)%j+A(j,162)%j+A(j,163)%j)*f(3))/2._/**/REALKIND+((A(j,157)%j+A(j,158)%j+A(j,161)%j &
       +A(j,164)%j)*f(3))/6._/**/REALKIND+(CI*(-A(j,49)%j-A(j,54)%j-A(j,58)%j+A(j,106)%j+A(j,111)%j+A(j,122)%j &
       +A(j,130)%j)*f(4))/2._/**/REALKIND+(CI*(A(j,41)%j+A(j,45)%j+A(j,64)%j)*f(5))/2._/**/REALKIND+(CI*(A(j,69)%j &
       +A(j,70)%j)*f(6))/2._/**/REALKIND
  M1(4) = ((A(j,86)%j+A(j,88)%j+A(j,99)%j+A(j,100)%j+A(j,103)%j+A(j,105)%j+A(j,109)%j+A(j,110)%j+A(j,119)%j+A(j,121)%j+A(j,128)%j &
       +A(j,129)%j+A(j,143)%j+A(j,144)%j+A(j,145)%j+A(j,148)%j+A(j,149)%j+A(j,151)%j)*f(1))/6._/**/REALKIND+((A(j,11)%j+A(j,14)%j &
       +A(j,15)%j+A(j,17)%j+A(j,18)%j+A(j,38)%j+A(j,85)%j+A(j,87)%j+A(j,93)%j+A(j,96)%j+A(j,98)%j+A(j,104)%j+A(j,116)%j+A(j,120)%j &
       +A(j,123)%j+A(j,134)%j+A(j,135)%j+A(j,140)%j+A(j,146)%j+A(j,147)%j+A(j,155)%j)*f(1))/2._/**/REALKIND+((-A(j,77)%j-A(j,78)%j &
       -A(j,82)%j-A(j,84)%j-A(j,150)%j-A(j,152)%j)*f(2))/6._/**/REALKIND+((-A(j,3)%j-A(j,4)%j-A(j,5)%j-A(j,24)%j-A(j,25)%j &
       -A(j,37)%j-A(j,80)%j-A(j,83)%j-A(j,156)%j)*f(2))/2._/**/REALKIND+((-A(j,159)%j-A(j,160)%j-A(j,162)%j &
       -A(j,163)%j)*f(3))/6._/**/REALKIND+((-A(j,29)%j-A(j,30)%j-A(j,31)%j-A(j,161)%j-A(j,164)%j)*f(3))/2._/**/REALKIND &
       +(CI*(A(j,10)%j+A(j,16)%j+A(j,20)%j-A(j,89)%j-A(j,95)%j+A(j,124)%j-A(j,136)%j)*f(4))/2._/**/REALKIND+(CI*(-A(j,2)%j &
       -A(j,7)%j-A(j,26)%j)*f(5))/2._/**/REALKIND+(CI*(-A(j,32)%j-A(j,33)%j)*f(6))/2._/**/REALKIND

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
  use ol_colourmatrix_ppwajj_uxdddxawxg_1_/**/REALKIND, only: K1
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
  use ol_colourmatrix_ppwajj_uxdddxawxg_1_/**/REALKIND, only: K1
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
  & bind(c,name="ol_tree_colvect_ppwajj_uxdddxawxg_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(4,192)
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
    & bind(c,name="ol_f_amp2tree_ppwajj_uxdddxawxg_1")
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
    & bind(c,name="ol_f_amp2ccone_ppwajj_uxdddxawxg_1")
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
    & bind(c,name="ol_f_amp2ccall_ppwajj_uxdddxawxg_1")
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
    & bind(c,name="ol_f_amp2hcone_ppwajj_uxdddxawxg_1")
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
    & bind(c,name="ol_f_amp2hcall_ppwajj_uxdddxawxg_1")
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
    & bind(c,name="ol_amp2tree_ppwajj_uxdddxawxg_1")
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
    & bind(c,name="ol_amp2ccone_ppwajj_uxdddxawxg_1")
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
    & bind(c,name="ol_amp2ccall_ppwajj_uxdddxawxg_1")
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
    & bind(c,name="ol_amp2hcone_ppwajj_uxdddxawxg_1")
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
    & bind(c,name="ol_amp2hcall_ppwajj_uxdddxawxg_1")
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
    & bind(c,name="amp2tree_ppwajj_uxdddxawxg_1_")
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
    & bind(c,name="amp2ccone_ppwajj_uxdddxawxg_1_")
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
    & bind(c,name="amp2ccall_ppwajj_uxdddxawxg_1_")
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
    & bind(c,name="amp2hcone_ppwajj_uxdddxawxg_1_")
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
    & bind(c,name="amp2hcall_ppwajj_uxdddxawxg_1_")
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

end module ol_tree_ppwajj_uxdddxawxg_1_/**/REALKIND
