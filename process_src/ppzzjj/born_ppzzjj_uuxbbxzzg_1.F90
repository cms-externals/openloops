
module ol_colourmatrix_ppzzjj_uuxbbxzzg_1_/**/REALKIND
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
  K1( 17,:) = [   0,  16,  -2,   6]
  K1( 18,:) = [  16,   0,   6,  -2]
  K1( 19,:) = [  -2,   6,   0,  16]
  K1( 20,:) = [   6,  -2,  16,   0]
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
  K1( 33,:) = [   0,  -2,  16,   6]
  K1( 34,:) = [  -2,   0,   6,  16]
  K1( 35,:) = [  16,   6,   0,  -2]
  K1( 36,:) = [   6,  16,  -2,   0]
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
  K1( 90,:) = [ -18,   0,   0,  18]
  K1( 91,:) = [ -18,   0, -54, -18]
  K1( 92,:) = [   0,  18, -18,   0]
  K1( 93,:) = [ -54, -18, -18,   0]
  K1( 94,:) = [ -18, -54,   0, -18]
  K1( 95,:) = [ -18,   0,   0,  18]
  K1( 96,:) = [   0, -18,  18,   0]
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
end module ol_colourmatrix_ppzzjj_uuxbbxzzg_1_/**/REALKIND



module ol_forced_parameters_ppzzjj_uuxbbxzzg_1_/**/REALKIND
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
  if (wMZ /= 0) write(*,101) 'wMZ = 0'
  if (wMZ /= 0) write(*,101) 'wMZ = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppzzjj_uuxbbxzzg_1_/**/REALKIND

module ol_tree_ppzzjj_uuxbbxzzg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(4)
  complex(REALKIND), save :: den(259)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 288 ! number of helicity configurations
  integer(intkind2), save :: nhel = 288 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(288) ! physical helicity states
  complex(DREALKIND) :: M1helarr(4,288) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = CI*eQED**2*gQCD**3
    f(2) = eQED**2*gQCD**3
    f(3) = (CI*eQED**2*gQCD**3*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(4) = (eQED**2*gQCD**3*MB)/(cw**2*sw**2*2._/**/REALKIND)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,3))
  den(2) = 1 / (Q(5,20) - MB2)
  den(3) = 1 / (Q(5,40) - MB2)
  den(5) = 1 / (Q(5,67))
  den(9) = 1 / (Q(5,84) - MB2)
  den(13) = 1 / (Q(5,104) - MB2)
  den(16) = 1 / (Q(5,72) - MB2)
  den(18) = 1 / (Q(5,52) - MB2)
  den(23) = 1 / (Q(5,11) - MB2)
  den(28) = 1 / (Q(5,36) - MB2)
  den(29) = 1 / (Q(5,24) - MB2)
  den(33) = 1 / (Q(5,100) - MB2)
  den(37) = 1 / (Q(5,88) - MB2)
  den(40) = 1 / (Q(5,68) - MB2)
  den(44) = 1 / (Q(5,56) - MB2)
  den(47) = 1 / (Q(5,7) - MB2)
  den(66) = 1 / (Q(5,48) - MH2)
  den(83) = 1 / (Q(5,17))
  den(84) = 1 / (Q(5,34))
  den(85) = 1 / (Q(5,12))
  den(87) = 1 / (Q(5,81))
  den(91) = 1 / (Q(5,98))
  den(95) = 1 / (Q(5,76))
  den(98) = 1 / (Q(5,66))
  den(100) = 1 / (Q(5,49))
  den(105) = 1 / (Q(5,14))
  den(110) = 1 / (Q(5,33))
  den(111) = 1 / (Q(5,18))
  den(113) = 1 / (Q(5,97))
  den(117) = 1 / (Q(5,82))
  den(122) = 1 / (Q(5,65))
  den(126) = 1 / (Q(5,50))
  den(129) = 1 / (Q(5,13))
  den(157) = 1 / (Q(5,44))
  den(161) = 1 / (Q(5,19))
  den(197) = 1 / (Q(5,28))
  den(201) = 1 / (Q(5,35))

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
  den(86) = den(84)*den(85)
  den(88) = den(83)*den(87)
  den(89) = den(86)*den(88)
  den(90) = den(83)*den(85)
  den(92) = den(84)*den(91)
  den(93) = den(90)*den(92)
  den(94) = den(83)*den(84)
  den(96) = den(85)*den(95)
  den(97) = den(94)*den(96)
  den(99) = den(85)*den(98)
  den(101) = den(83)*den(100)
  den(102) = den(99)*den(101)
  den(103) = den(91)*den(98)
  den(104) = den(90)*den(103)
  den(106) = den(85)*den(105)
  den(107) = den(101)*den(106)
  den(108) = den(96)*den(101)
  den(109) = den(88)*den(106)
  den(112) = den(85)*den(111)
  den(114) = den(110)*den(113)
  den(115) = den(112)*den(114)
  den(116) = den(85)*den(110)
  den(118) = den(111)*den(117)
  den(119) = den(116)*den(118)
  den(120) = den(110)*den(111)
  den(121) = den(96)*den(120)
  den(123) = den(113)*den(122)
  den(124) = den(112)*den(123)
  den(125) = den(85)*den(122)
  den(127) = den(111)*den(126)
  den(128) = den(125)*den(127)
  den(130) = den(85)*den(129)
  den(131) = den(118)*den(130)
  den(132) = den(127)*den(130)
  den(133) = den(96)*den(127)
  den(134) = den(100)*den(110)
  den(135) = den(99)*den(134)
  den(136) = den(98)*den(117)
  den(137) = den(116)*den(136)
  den(138) = den(106)*den(134)
  den(139) = den(96)*den(134)
  den(140) = den(106)*den(114)
  den(141) = den(87)*den(122)
  den(142) = den(86)*den(141)
  den(143) = den(84)*den(126)
  den(144) = den(125)*den(143)
  den(145) = den(92)*den(130)
  den(146) = den(130)*den(143)
  den(147) = den(96)*den(143)
  den(148) = den(106)*den(141)
  den(149) = den(106)*den(123)
  den(150) = den(103)*den(130)
  den(151) = den(130)*den(136)
  den(152) = den(40)*den(95)
  den(153) = den(94)*den(152)
  den(154) = den(16)*den(95)
  den(155) = den(94)*den(154)
  den(156) = den(83)*den(98)
  den(158) = den(28)*den(157)
  den(159) = den(156)*den(158)
  den(160) = den(16)*den(28)
  den(162) = den(83)*den(161)
  den(163) = den(160)*den(162)
  den(164) = den(34)*den(162)
  den(165) = den(158)*den(162)
  den(166) = den(88)*den(158)
  den(167) = den(3)*den(157)
  den(168) = den(156)*den(167)
  den(169) = den(3)*den(40)
  den(170) = den(162)*den(169)
  den(171) = den(14)*den(162)
  den(172) = den(162)*den(167)
  den(173) = den(88)*den(167)
  den(174) = den(41)*den(162)
  den(175) = den(101)*den(152)
  den(176) = den(21)*den(162)
  den(177) = den(101)*den(154)
  den(178) = den(120)*den(152)
  den(179) = den(120)*den(154)
  den(180) = den(111)*den(122)
  den(181) = den(158)*den(180)
  den(182) = den(111)*den(161)
  den(183) = den(160)*den(182)
  den(184) = den(34)*den(182)
  den(185) = den(158)*den(182)
  den(186) = den(118)*den(158)
  den(187) = den(167)*den(180)
  den(188) = den(169)*den(182)
  den(189) = den(14)*den(182)
  den(190) = den(167)*den(182)
  den(191) = den(118)*den(167)
  den(192) = den(41)*den(182)
  den(193) = den(127)*den(152)
  den(194) = den(21)*den(182)
  den(195) = den(127)*den(154)
  den(196) = den(98)*den(110)
  den(198) = den(2)*den(197)
  den(199) = den(196)*den(198)
  den(200) = den(2)*den(16)
  den(202) = den(110)*den(201)
  den(203) = den(200)*den(202)
  den(204) = den(10)*den(202)
  den(205) = den(198)*den(202)
  den(206) = den(114)*den(198)
  den(207) = den(84)*den(122)
  den(208) = den(198)*den(207)
  den(209) = den(84)*den(201)
  den(210) = den(200)*den(209)
  den(211) = den(10)*den(209)
  den(212) = den(198)*den(209)
  den(213) = den(92)*den(198)
  den(214) = den(5)*den(122)
  den(215) = den(4)*den(214)
  den(216) = den(5)*den(98)
  den(217) = den(4)*den(216)
  den(218) = den(19)*den(214)
  den(219) = den(123)*den(198)
  den(220) = den(19)*den(216)
  den(221) = den(103)*den(198)
  den(222) = den(29)*den(197)
  den(223) = den(196)*den(222)
  den(224) = den(29)*den(40)
  den(225) = den(202)*den(224)
  den(226) = den(38)*den(202)
  den(227) = den(202)*den(222)
  den(228) = den(114)*den(222)
  den(229) = den(207)*den(222)
  den(230) = den(209)*den(224)
  den(231) = den(38)*den(209)
  den(232) = den(209)*den(222)
  den(233) = den(92)*den(222)
  den(234) = den(30)*den(214)
  den(235) = den(30)*den(216)
  den(236) = den(45)*den(214)
  den(237) = den(123)*den(222)
  den(238) = den(45)*den(216)
  den(239) = den(103)*den(222)
  den(240) = den(59)*den(202)
  den(241) = den(134)*den(152)
  den(242) = den(54)*den(202)
  den(243) = den(134)*den(154)
  den(244) = den(59)*den(209)
  den(245) = den(143)*den(152)
  den(246) = den(54)*den(209)
  den(247) = den(143)*den(154)
  den(248) = den(52)*den(214)
  den(249) = den(141)*den(158)
  den(250) = den(52)*den(216)
  den(251) = den(136)*den(158)
  den(252) = den(61)*den(214)
  den(253) = den(141)*den(167)
  den(254) = den(61)*den(216)
  den(255) = den(136)*den(167)
  den(256) = den(69)*den(214)
  den(257) = den(73)*den(214)
  den(258) = den(69)*den(216)
  den(259) = den(73)*den(216)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_ppzzjj_uuxbbxzzg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_ppzzjj_uuxbbxzzg_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for up anti-up bottom anti-bottom Z Z glue -> 0
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
  use ol_external_ppzzjj_uuxbbxzzg_1, only: external_perm_ppzzjj_uuxbbxzzg_1, &
    & external_perm_inv_ppzzjj_uuxbbxzzg_1, extcomb_perm_ppzzjj_uuxbbxzzg_1, &
    & average_factor_ppzzjj_uuxbbxzzg_1
  use ol_external_ppzzjj_uuxbbxzzg_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_ppzzjj_uuxbbxzzg_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_ppzzjj_uuxbbxzzg_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_ppzzjj_uuxbbxzzg_1
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
  complex(REALKIND) :: MOM_LC(4), M1(4), M1helarray(4,288)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(3), ex6(3), ex7(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,10), wf6(6,16), wf8(8,14), wf9(9,1), wf12(12,40), wf16(16,16), wf18(18,20), wf24(24,44), wf36(36,17), &
    wf288(288,152)

  type(polcont) :: A(288,152)
  complex(REALKIND) :: Aj(152)

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
  extmasses2 = [ rZERO2, rZERO2, rMB2, rMB2, rMZ2, rMZ2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_ppzzjj_uuxbbxzzg_1,7)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,7)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_ppzzjj_uuxbbxzzg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_ppzzjj_uuxbbxzzg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_ppzzjj_uuxbbxzzg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_Q(P(:,1), rZERO, H1, ex1)
  call wf_A(P(:,2), rZERO, H2, ex2)
  call wf_Q(P(:,3), rMB, H3, ex3)
  call wf_A(P(:,4), rMB, H4, ex4)
  call wf_V(P(:,5), rMZ, H5, ex5)
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
  call vert_QA_V(ntry, ex1, ex2, wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_ZQ_A(gZd,ntry, ex5, ex3, wf6(:,1), n3(:,2), t3x6(:,:,1))
  call vert_AZ_Q(gZd,ntry, ex4, ex6, wf6(:,2), n3(:,3), t3x6(:,:,2))
  call prop_Q_A(ntry, wf6(:,1), Q(:,20), MB, 1_intkind1, wf6(:,3), n2(1))
  call prop_A_Q(ntry, wf6(:,2), Q(:,40), MB, 1_intkind1, wf6(:,4), n2(2))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), ex7, Q(:,64), wf8(:,1), n3(:,4), t3x8(:,:,1))
  call vert_QA_V(ntry, wf6(:,3), wf6(:,4), wf36(:,1), n3(:,5), t3x36(:,:,1))
  call vert_VQ_A(ntry, ex7, wf6(:,3), wf12(:,1), n3(:,6), t3x12(:,:,1))
  call vert_AV_Q(ntry, wf6(:,4), wf4(:,1), wf24(:,1), n3(:,7), t3x24(:,:,1))
  call prop_Q_A(ntry, wf12(:,1), Q(:,84), MB, 1_intkind1, wf12(:,2), n2(3))
  call vert_AV_Q(ntry, wf6(:,4), ex7, wf12(:,3), n3(:,8), t3x12(:,:,2))
  call vert_VQ_A(ntry, wf4(:,1), wf6(:,3), wf24(:,2), n3(:,9), t3x24(:,:,2))
  call prop_A_Q(ntry, wf12(:,3), Q(:,104), MB, 1_intkind1, wf12(:,4), n2(4))
  call vert_AV_Q(ntry, ex4, ex7, wf4(:,2), n3(:,10), t3x4(:,:,2))
  call prop_A_Q(ntry, wf4(:,2), Q(:,72), MB, 1_intkind1, wf4(:,3), n2(5))
  call vert_ZQ_A(gZd,ntry, ex6, wf6(:,3), wf18(:,1), n3(:,11), t3x18(:,:,1))
  call vert_AV_Q(ntry, wf4(:,3), wf4(:,1), wf16(:,1), n3(:,12), t3x16(:,:,1))
  call prop_Q_A(ntry, wf18(:,1), Q(:,52), MB, 1_intkind1, wf18(:,2), n2(6))
  call vert_AZ_Q(gZd,ntry, wf4(:,3), ex6, wf12(:,5), n3(:,13), t3x12(:,:,3))
  call prop_A_Q(ntry, wf12(:,5), Q(:,104), MB, 1_intkind1, wf12(:,6), n2(7))
  call vert_AV_Q(ntry, ex4, wf4(:,1), wf8(:,2), n3(:,14), t3x8(:,:,2))
  call prop_A_Q(ntry, wf8(:,2), Q(:,11), MB, 1_intkind1, wf8(:,3), n2(8))
  call vert_AV_Q(ntry, wf8(:,3), ex7, wf16(:,2), n3(:,15), t3x16(:,:,2))
  call vert_AZ_Q(gZd,ntry, wf8(:,3), ex6, wf24(:,3), n3(:,16), t3x24(:,:,3))
  call vert_QA_V(ntry, wf18(:,2), ex4, wf36(:,2), n3(:,17), t3x36(:,:,2))
  call vert_ZQ_A(gZd,ntry, ex6, ex3, wf6(:,5), n3(:,18), t3x6(:,:,3))
  call vert_AZ_Q(gZd,ntry, ex4, ex5, wf6(:,6), n3(:,19), t3x6(:,:,4))
  call prop_Q_A(ntry, wf6(:,5), Q(:,36), MB, 1_intkind1, wf6(:,7), n2(9))
  call prop_A_Q(ntry, wf6(:,6), Q(:,24), MB, 1_intkind1, wf6(:,8), n2(10))
  call vert_QA_V(ntry, wf6(:,7), wf6(:,8), wf36(:,3), n3(:,20), t3x36(:,:,3))
  call vert_VQ_A(ntry, ex7, wf6(:,7), wf12(:,7), n3(:,21), t3x12(:,:,4))
  call vert_AV_Q(ntry, wf6(:,8), wf4(:,1), wf24(:,4), n3(:,22), t3x24(:,:,4))
  call prop_Q_A(ntry, wf12(:,7), Q(:,100), MB, 1_intkind1, wf12(:,8), n2(11))
  call vert_AV_Q(ntry, wf6(:,8), ex7, wf12(:,9), n3(:,23), t3x12(:,:,5))
  call vert_VQ_A(ntry, wf4(:,1), wf6(:,7), wf24(:,5), n3(:,24), t3x24(:,:,5))
  call prop_A_Q(ntry, wf12(:,9), Q(:,88), MB, 1_intkind1, wf12(:,10), n2(12))
  call vert_VQ_A(ntry, ex7, ex3, wf4(:,4), n3(:,25), t3x4(:,:,3))
  call prop_Q_A(ntry, wf4(:,4), Q(:,68), MB, 1_intkind1, wf4(:,5), n2(13))
  call vert_ZQ_A(gZd,ntry, ex6, wf4(:,5), wf12(:,11), n3(:,26), t3x12(:,:,6))
  call prop_Q_A(ntry, wf12(:,11), Q(:,100), MB, 1_intkind1, wf12(:,12), n2(14))
  call vert_AZ_Q(gZd,ntry, wf6(:,8), ex6, wf18(:,3), n3(:,27), t3x18(:,:,2))
  call vert_VQ_A(ntry, wf4(:,1), wf4(:,5), wf16(:,3), n3(:,28), t3x16(:,:,3))
  call prop_A_Q(ntry, wf18(:,3), Q(:,56), MB, 1_intkind1, wf18(:,4), n2(15))
  call vert_VQ_A(ntry, wf4(:,1), ex3, wf8(:,4), n3(:,29), t3x8(:,:,3))
  call prop_Q_A(ntry, wf8(:,4), Q(:,7), MB, 1_intkind1, wf8(:,5), n2(16))
  call vert_ZQ_A(gZd,ntry, ex6, wf8(:,5), wf24(:,6), n3(:,30), t3x24(:,:,6))
  call vert_VQ_A(ntry, ex7, wf8(:,5), wf16(:,4), n3(:,31), t3x16(:,:,4))
  call vert_QA_V(ntry, ex3, wf18(:,4), wf36(:,4), n3(:,32), t3x36(:,:,4))
  call vert_ZQ_A(gZd,ntry, ex5, wf6(:,7), wf18(:,5), n3(:,33), t3x18(:,:,3))
  call prop_Q_A(ntry, wf18(:,5), Q(:,52), MB, 1_intkind1, wf18(:,6), n2(17))
  call vert_AZ_Q(gZd,ntry, wf4(:,3), ex5, wf12(:,13), n3(:,34), t3x12(:,:,7))
  call prop_A_Q(ntry, wf12(:,13), Q(:,88), MB, 1_intkind1, wf12(:,14), n2(18))
  call vert_AZ_Q(gZd,ntry, wf8(:,3), ex5, wf24(:,7), n3(:,35), t3x24(:,:,7))
  call vert_QA_V(ntry, wf18(:,6), ex4, wf36(:,5), n3(:,36), t3x36(:,:,5))
  call vert_ZQ_A(gZd,ntry, ex5, wf4(:,5), wf12(:,15), n3(:,37), t3x12(:,:,8))
  call prop_Q_A(ntry, wf12(:,15), Q(:,84), MB, 1_intkind1, wf12(:,16), n2(19))
  call vert_AZ_Q(gZd,ntry, wf6(:,4), ex5, wf18(:,7), n3(:,38), t3x18(:,:,4))
  call prop_A_Q(ntry, wf18(:,7), Q(:,56), MB, 1_intkind1, wf18(:,8), n2(20))
  call vert_ZQ_A(gZd,ntry, ex5, wf8(:,5), wf24(:,8), n3(:,39), t3x24(:,:,8))
  call vert_QA_V(ntry, ex3, wf18(:,8), wf36(:,6), n3(:,40), t3x36(:,:,6))
  call vert_VV_S(ntry, ex5, ex6, wf9(:,1), n3(:,41), t3x9(:,:,1))
  call vert_QS_A(gH,ntry, wf4(:,5), wf9(:,1), wf36(:,7), n3(:,42), t3x36(:,:,7))
  call vert_SA_Q(gH,ntry, wf9(:,1), ex4, wf18(:,9), n3(:,43), t3x18(:,:,5))
  call prop_A_Q(ntry, wf18(:,9), Q(:,56), MB, 1_intkind1, wf18(:,10), n2(21))
  call vert_SA_Q(gH,ntry, wf9(:,1), wf4(:,3), wf36(:,8), n3(:,44), t3x36(:,:,8))
  call vert_QS_A(gH,ntry, ex3, wf9(:,1), wf18(:,11), n3(:,45), t3x18(:,:,6))
  call prop_Q_A(ntry, wf18(:,11), Q(:,52), MB, 1_intkind1, wf18(:,12), n2(22))
  call vert_VQ_A(ntry, ex7, wf18(:,12), wf36(:,9), n3(:,46), t3x36(:,:,9))
  call vert_QA_V(ntry, wf18(:,12), ex4, wf36(:,10), n3(:,47), t3x36(:,:,10))
  call vert_QA_V(ntry, ex3, wf18(:,10), wf36(:,11), n3(:,48), t3x36(:,:,11))
  call vert_ZQ_A(gZu,ntry, ex5, ex1, wf6(:,9), n3(:,49), t3x6(:,:,5))
  call vert_AZ_Q(gZu,ntry, ex2, ex6, wf6(:,10), n3(:,50), t3x6(:,:,6))
  call vert_QA_V(ntry, ex3, ex4, wf4(:,6), n3(:,51), t3x4(:,:,4))
  call prop_Q_A(ntry, wf6(:,9), Q(:,17), ZERO, 0_intkind1, wf6(:,11), n2(23))
  call prop_A_Q(ntry, wf6(:,10), Q(:,34), ZERO, 0_intkind1, wf6(:,12), n2(24))
  call vert_VQ_A(ntry, ex7, wf6(:,11), wf12(:,17), n3(:,52), t3x12(:,:,9))
  call vert_AV_Q(ntry, wf6(:,12), wf4(:,6), wf24(:,9), n3(:,53), t3x24(:,:,9))
  call prop_Q_A(ntry, wf12(:,17), Q(:,81), ZERO, 0_intkind1, wf12(:,18), n2(25))
  call vert_AV_Q(ntry, wf6(:,12), ex7, wf12(:,19), n3(:,54), t3x12(:,:,10))
  call vert_VQ_A(ntry, wf4(:,6), wf6(:,11), wf24(:,10), n3(:,55), t3x24(:,:,10))
  call prop_A_Q(ntry, wf12(:,19), Q(:,98), ZERO, 0_intkind1, wf12(:,20), n2(26))
  call vert_UV_W(ntry, wf4(:,6), Q(:,12), ex7, Q(:,64), wf8(:,6), n3(:,56), t3x8(:,:,4))
  call vert_QA_V(ntry, wf6(:,11), wf6(:,12), wf36(:,12), n3(:,57), t3x36(:,:,12))
  call vert_AV_Q(ntry, ex2, ex7, wf4(:,7), n3(:,58), t3x4(:,:,5))
  call prop_A_Q(ntry, wf4(:,7), Q(:,66), ZERO, 0_intkind1, wf4(:,8), n2(27))
  call vert_ZQ_A(gZu,ntry, ex6, wf6(:,11), wf18(:,13), n3(:,59), t3x18(:,:,7))
  call vert_AV_Q(ntry, wf4(:,8), wf4(:,6), wf16(:,5), n3(:,60), t3x16(:,:,5))
  call prop_Q_A(ntry, wf18(:,13), Q(:,49), ZERO, 0_intkind1, wf18(:,14), n2(28))
  call vert_AZ_Q(gZu,ntry, wf4(:,8), ex6, wf12(:,21), n3(:,61), t3x12(:,:,11))
  call prop_A_Q(ntry, wf12(:,21), Q(:,98), ZERO, 0_intkind1, wf12(:,22), n2(29))
  call vert_AV_Q(ntry, ex2, wf4(:,6), wf8(:,7), n3(:,62), t3x8(:,:,5))
  call prop_A_Q(ntry, wf8(:,7), Q(:,14), ZERO, 0_intkind1, wf8(:,8), n2(30))
  call vert_AV_Q(ntry, wf8(:,8), ex7, wf16(:,6), n3(:,63), t3x16(:,:,6))
  call vert_QA_V(ntry, wf18(:,14), ex2, wf36(:,13), n3(:,64), t3x36(:,:,13))
  call vert_AZ_Q(gZu,ntry, wf8(:,8), ex6, wf24(:,11), n3(:,65), t3x24(:,:,11))
  call vert_ZQ_A(gZu,ntry, ex6, ex1, wf6(:,13), n3(:,66), t3x6(:,:,7))
  call vert_AZ_Q(gZu,ntry, ex2, ex5, wf6(:,14), n3(:,67), t3x6(:,:,8))
  call prop_Q_A(ntry, wf6(:,13), Q(:,33), ZERO, 0_intkind1, wf6(:,15), n2(31))
  call prop_A_Q(ntry, wf6(:,14), Q(:,18), ZERO, 0_intkind1, wf6(:,16), n2(32))
  call vert_VQ_A(ntry, ex7, wf6(:,15), wf12(:,23), n3(:,68), t3x12(:,:,12))
  call vert_AV_Q(ntry, wf6(:,16), wf4(:,6), wf24(:,12), n3(:,69), t3x24(:,:,12))
  call prop_Q_A(ntry, wf12(:,23), Q(:,97), ZERO, 0_intkind1, wf12(:,24), n2(33))
  call vert_AV_Q(ntry, wf6(:,16), ex7, wf12(:,25), n3(:,70), t3x12(:,:,13))
  call vert_VQ_A(ntry, wf4(:,6), wf6(:,15), wf24(:,13), n3(:,71), t3x24(:,:,13))
  call prop_A_Q(ntry, wf12(:,25), Q(:,82), ZERO, 0_intkind1, wf12(:,26), n2(34))
  call vert_QA_V(ntry, wf6(:,15), wf6(:,16), wf36(:,14), n3(:,72), t3x36(:,:,14))
  call vert_VQ_A(ntry, ex7, ex1, wf4(:,9), n3(:,73), t3x4(:,:,6))
  call prop_Q_A(ntry, wf4(:,9), Q(:,65), ZERO, 0_intkind1, wf4(:,10), n2(35))
  call vert_ZQ_A(gZu,ntry, ex6, wf4(:,10), wf12(:,27), n3(:,74), t3x12(:,:,14))
  call prop_Q_A(ntry, wf12(:,27), Q(:,97), ZERO, 0_intkind1, wf12(:,28), n2(36))
  call vert_AZ_Q(gZu,ntry, wf6(:,16), ex6, wf18(:,15), n3(:,75), t3x18(:,:,8))
  call vert_VQ_A(ntry, wf4(:,6), wf4(:,10), wf16(:,7), n3(:,76), t3x16(:,:,7))
  call prop_A_Q(ntry, wf18(:,15), Q(:,50), ZERO, 0_intkind1, wf18(:,16), n2(37))
  call vert_VQ_A(ntry, wf4(:,6), ex1, wf8(:,9), n3(:,77), t3x8(:,:,6))
  call prop_Q_A(ntry, wf8(:,9), Q(:,13), ZERO, 0_intkind1, wf8(:,10), n2(38))
  call vert_ZQ_A(gZu,ntry, ex6, wf8(:,10), wf24(:,14), n3(:,78), t3x24(:,:,14))
  call vert_VQ_A(ntry, ex7, wf8(:,10), wf16(:,8), n3(:,79), t3x16(:,:,8))
  call vert_QA_V(ntry, ex1, wf18(:,16), wf36(:,15), n3(:,80), t3x36(:,:,15))
  call vert_ZQ_A(gZu,ntry, ex5, wf6(:,15), wf18(:,17), n3(:,81), t3x18(:,:,9))
  call prop_Q_A(ntry, wf18(:,17), Q(:,49), ZERO, 0_intkind1, wf18(:,18), n2(39))
  call vert_AZ_Q(gZu,ntry, wf4(:,8), ex5, wf12(:,29), n3(:,82), t3x12(:,:,15))
  call prop_A_Q(ntry, wf12(:,29), Q(:,82), ZERO, 0_intkind1, wf12(:,30), n2(40))
  call vert_QA_V(ntry, wf18(:,18), ex2, wf36(:,16), n3(:,83), t3x36(:,:,16))
  call vert_AZ_Q(gZu,ntry, wf8(:,8), ex5, wf24(:,15), n3(:,84), t3x24(:,:,15))
  call vert_ZQ_A(gZu,ntry, ex5, wf4(:,10), wf12(:,31), n3(:,85), t3x12(:,:,16))
  call prop_Q_A(ntry, wf12(:,31), Q(:,81), ZERO, 0_intkind1, wf12(:,32), n2(41))
  call vert_AZ_Q(gZu,ntry, wf6(:,12), ex5, wf18(:,19), n3(:,86), t3x18(:,:,10))
  call prop_A_Q(ntry, wf18(:,19), Q(:,50), ZERO, 0_intkind1, wf18(:,20), n2(42))
  call vert_ZQ_A(gZu,ntry, ex5, wf8(:,10), wf24(:,16), n3(:,87), t3x24(:,:,16))
  call vert_QA_V(ntry, ex1, wf18(:,20), wf36(:,17), n3(:,88), t3x36(:,:,17))
  call vert_QA_V(ntry, wf4(:,5), ex4, wf8(:,11), n3(:,89), t3x8(:,:,7))
  call vert_QA_V(ntry, ex3, wf4(:,3), wf8(:,12), n3(:,90), t3x8(:,:,8))
  call vert_QA_V(ntry, wf6(:,7), ex4, wf12(:,33), n3(:,91), t3x12(:,:,17))
  call vert_QA_V(ntry, wf6(:,11), wf4(:,8), wf24(:,17), n3(:,92), t3x24(:,:,17))
  call vert_QA_V(ntry, wf6(:,11), ex2, wf12(:,34), n3(:,93), t3x12(:,:,18))
  call vert_QA_V(ntry, wf6(:,7), wf4(:,3), wf24(:,18), n3(:,94), t3x24(:,:,18))
  call vert_AV_Q(ntry, ex4, wf12(:,34), wf24(:,19), n3(:,95), t3x24(:,:,19))
  call vert_UV_W(ntry, wf12(:,34), Q(:,19), ex7, Q(:,64), wf24(:,20), n3(:,96), t3x24(:,:,20))
  call vert_AV_Q(ntry, ex2, wf12(:,33), wf24(:,21), n3(:,97), t3x24(:,:,21))
  call vert_QA_V(ntry, ex3, wf6(:,4), wf12(:,35), n3(:,98), t3x12(:,:,19))
  call vert_QA_V(ntry, wf4(:,5), wf6(:,4), wf24(:,22), n3(:,99), t3x24(:,:,22))
  call vert_VQ_A(ntry, wf12(:,34), ex3, wf24(:,23), n3(:,100), t3x24(:,:,23))
  call vert_AV_Q(ntry, ex2, wf12(:,35), wf24(:,24), n3(:,101), t3x24(:,:,24))
  call vert_AV_Q(ntry, ex2, wf8(:,11), wf16(:,9), n3(:,102), t3x16(:,:,9))
  call vert_AV_Q(ntry, ex2, wf8(:,12), wf16(:,10), n3(:,103), t3x16(:,:,10))
  call vert_QA_V(ntry, wf4(:,10), wf6(:,16), wf24(:,25), n3(:,104), t3x24(:,:,25))
  call vert_QA_V(ntry, ex1, wf6(:,16), wf12(:,36), n3(:,105), t3x12(:,:,20))
  call vert_AV_Q(ntry, ex4, wf12(:,36), wf24(:,26), n3(:,106), t3x24(:,:,26))
  call vert_UV_W(ntry, wf12(:,36), Q(:,19), ex7, Q(:,64), wf24(:,27), n3(:,107), t3x24(:,:,27))
  call vert_VQ_A(ntry, wf12(:,33), ex1, wf24(:,28), n3(:,108), t3x24(:,:,28))
  call vert_VQ_A(ntry, wf12(:,36), ex3, wf24(:,29), n3(:,109), t3x24(:,:,29))
  call vert_VQ_A(ntry, wf12(:,35), ex1, wf24(:,30), n3(:,110), t3x24(:,:,30))
  call vert_VQ_A(ntry, wf8(:,11), ex1, wf16(:,11), n3(:,111), t3x16(:,:,11))
  call vert_VQ_A(ntry, wf8(:,12), ex1, wf16(:,12), n3(:,112), t3x16(:,:,12))
  call vert_QA_V(ntry, wf6(:,3), ex4, wf12(:,37), n3(:,113), t3x12(:,:,21))
  call vert_QA_V(ntry, wf6(:,15), wf4(:,8), wf24(:,31), n3(:,114), t3x24(:,:,31))
  call vert_QA_V(ntry, wf6(:,15), ex2, wf12(:,38), n3(:,115), t3x12(:,:,22))
  call vert_QA_V(ntry, wf6(:,3), wf4(:,3), wf24(:,32), n3(:,116), t3x24(:,:,32))
  call vert_AV_Q(ntry, ex4, wf12(:,38), wf24(:,33), n3(:,117), t3x24(:,:,33))
  call vert_UV_W(ntry, wf12(:,38), Q(:,35), ex7, Q(:,64), wf24(:,34), n3(:,118), t3x24(:,:,34))
  call vert_AV_Q(ntry, ex2, wf12(:,37), wf24(:,35), n3(:,119), t3x24(:,:,35))
  call vert_QA_V(ntry, wf4(:,10), wf6(:,12), wf24(:,36), n3(:,120), t3x24(:,:,36))
  call vert_QA_V(ntry, ex1, wf6(:,12), wf12(:,39), n3(:,121), t3x12(:,:,23))
  call vert_AV_Q(ntry, ex4, wf12(:,39), wf24(:,37), n3(:,122), t3x24(:,:,37))
  call vert_UV_W(ntry, wf12(:,39), Q(:,35), ex7, Q(:,64), wf24(:,38), n3(:,123), t3x24(:,:,38))
  call vert_VQ_A(ntry, wf12(:,37), ex1, wf24(:,39), n3(:,124), t3x24(:,:,39))
  call vert_QA_V(ntry, wf4(:,10), ex2, wf8(:,13), n3(:,125), t3x8(:,:,9))
  call vert_QA_V(ntry, ex1, wf4(:,8), wf8(:,14), n3(:,126), t3x8(:,:,10))
  call vert_AV_Q(ntry, ex4, wf8(:,13), wf16(:,13), n3(:,127), t3x16(:,:,13))
  call vert_AV_Q(ntry, ex4, wf8(:,14), wf16(:,14), n3(:,128), t3x16(:,:,14))
  call vert_QA_V(ntry, ex3, wf6(:,8), wf12(:,40), n3(:,129), t3x12(:,:,24))
  call vert_QA_V(ntry, wf4(:,5), wf6(:,8), wf24(:,40), n3(:,130), t3x24(:,:,40))
  call vert_VQ_A(ntry, wf12(:,38), ex3, wf24(:,41), n3(:,131), t3x24(:,:,41))
  call vert_AV_Q(ntry, ex2, wf12(:,40), wf24(:,42), n3(:,132), t3x24(:,:,42))
  call vert_VQ_A(ntry, wf12(:,39), ex3, wf24(:,43), n3(:,133), t3x24(:,:,43))
  call vert_VQ_A(ntry, wf12(:,40), ex1, wf24(:,44), n3(:,134), t3x24(:,:,44))
  call vert_VQ_A(ntry, wf8(:,13), ex3, wf16(:,15), n3(:,135), t3x16(:,:,15))
  call vert_VQ_A(ntry, wf8(:,14), ex3, wf16(:,16), n3(:,136), t3x16(:,:,16))


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
  M2add = M2 / average_factor_ppzzjj_uuxbbxzzg_1

  do k = 0, 30-1
    M2(k) = M2add(extcomb_perm_ppzzjj_uuxbbxzzg_1(k))
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

    call cont_VV(nsync, wf8(:,1), wf36(:,1), A(:,1), n3(:,137), t3x288(:,:,1), nhel, den(7))
    call cont_QA(nsync, wf24(:,1), wf12(:,2), A(:,2), n3(:,138), t3x288(:,:,2), nhel, den(11))
    call cont_QA(nsync, wf24(:,2), wf12(:,4), A(:,3), n3(:,139), t3x288(:,:,3), nhel, den(15))
    call cont_QA(nsync, wf16(:,1), wf18(:,2), A(:,4), n3(:,140), t3x288(:,:,4), nhel, den(20))
    call cont_QA(nsync, wf24(:,2), wf12(:,6), A(:,5), n3(:,141), t3x288(:,:,5), nhel, den(22))
    call cont_QA(nsync, wf18(:,2), wf16(:,2), A(:,6), n3(:,142), t3x288(:,:,6), nhel, den(25))
    call cont_QA(nsync, wf12(:,2), wf24(:,3), A(:,7), n3(:,143), t3x288(:,:,7), nhel, den(26))
    call cont_VV(nsync, wf8(:,1), wf36(:,2), A(:,8), n3(:,144), t3x288(:,:,8), nhel, den(27))
    call cont_VV(nsync, wf8(:,1), wf36(:,3), A(:,9), n3(:,145), t3x288(:,:,9), nhel, den(31))
    call cont_QA(nsync, wf24(:,4), wf12(:,8), A(:,10), n3(:,146), t3x288(:,:,10), nhel, den(35))
    call cont_QA(nsync, wf24(:,5), wf12(:,10), A(:,11), n3(:,147), t3x288(:,:,11), nhel, den(39))
    call cont_QA(nsync, wf24(:,4), wf12(:,12), A(:,12), n3(:,148), t3x288(:,:,12), nhel, den(42))
    call cont_QA(nsync, wf16(:,3), wf18(:,4), A(:,13), n3(:,149), t3x288(:,:,13), nhel, den(46))
    call cont_QA(nsync, wf12(:,10), wf24(:,6), A(:,14), n3(:,150), t3x288(:,:,14), nhel, den(49))
    call cont_QA(nsync, wf18(:,4), wf16(:,4), A(:,15), n3(:,151), t3x288(:,:,15), nhel, den(50))
    call cont_VV(nsync, wf8(:,1), wf36(:,4), A(:,16), n3(:,152), t3x288(:,:,16), nhel, den(51))
    call cont_QA(nsync, wf16(:,1), wf18(:,6), A(:,17), n3(:,153), t3x288(:,:,17), nhel, den(53))
    call cont_QA(nsync, wf24(:,5), wf12(:,14), A(:,18), n3(:,154), t3x288(:,:,18), nhel, den(55))
    call cont_QA(nsync, wf16(:,2), wf18(:,6), A(:,19), n3(:,155), t3x288(:,:,19), nhel, den(56))
    call cont_QA(nsync, wf12(:,8), wf24(:,7), A(:,20), n3(:,156), t3x288(:,:,20), nhel, den(57))
    call cont_VV(nsync, wf8(:,1), wf36(:,5), A(:,21), n3(:,157), t3x288(:,:,21), nhel, den(58))
    call cont_QA(nsync, wf24(:,1), wf12(:,16), A(:,22), n3(:,158), t3x288(:,:,22), nhel, den(60))
    call cont_QA(nsync, wf16(:,3), wf18(:,8), A(:,23), n3(:,159), t3x288(:,:,23), nhel, den(62))
    call cont_QA(nsync, wf12(:,4), wf24(:,8), A(:,24), n3(:,160), t3x288(:,:,24), nhel, den(63))
    call cont_QA(nsync, wf16(:,4), wf18(:,8), A(:,25), n3(:,161), t3x288(:,:,25), nhel, den(64))
    call cont_VV(nsync, wf8(:,1), wf36(:,6), A(:,26), n3(:,162), t3x288(:,:,26), nhel, den(65))
    call cont_QA(nsync, wf8(:,3), wf36(:,7), A(:,27), n3(:,163), t3x288(:,:,27), nhel, den(68))
    call cont_QA(nsync, wf16(:,3), wf18(:,10), A(:,28), n3(:,164), t3x288(:,:,28), nhel, den(70))
    call cont_QA(nsync, wf8(:,5), wf36(:,8), A(:,29), n3(:,165), t3x288(:,:,29), nhel, den(72))
    call cont_QA(nsync, wf16(:,1), wf18(:,12), A(:,30), n3(:,166), t3x288(:,:,30), nhel, den(74))
    call cont_QA(nsync, wf16(:,4), wf18(:,10), A(:,31), n3(:,167), t3x288(:,:,31), nhel, den(75))
    call cont_QA(nsync, wf8(:,3), wf36(:,9), A(:,32), n3(:,168), t3x288(:,:,32), nhel, den(76))
    call cont_VV(nsync, wf8(:,1), wf36(:,10), A(:,33), n3(:,169), t3x288(:,:,33), nhel, den(77))
    call cont_VV(nsync, wf8(:,1), wf36(:,11), A(:,34), n3(:,170), t3x288(:,:,34), nhel, den(78))
    call cont_QA(nsync, wf24(:,3), wf12(:,16), A(:,35), n3(:,171), t3x288(:,:,35), nhel, den(79))
    call cont_QA(nsync, wf12(:,12), wf24(:,7), A(:,36), n3(:,172), t3x288(:,:,36), nhel, den(80))
    call cont_QA(nsync, wf12(:,6), wf24(:,8), A(:,37), n3(:,173), t3x288(:,:,37), nhel, den(81))
    call cont_QA(nsync, wf24(:,6), wf12(:,14), A(:,38), n3(:,174), t3x288(:,:,38), nhel, den(82))
    call cont_QA(nsync, wf24(:,9), wf12(:,18), A(:,39), n3(:,175), t3x288(:,:,39), nhel, den(89))
    call cont_QA(nsync, wf24(:,10), wf12(:,20), A(:,40), n3(:,176), t3x288(:,:,40), nhel, den(93))
    call cont_VV(nsync, wf8(:,6), wf36(:,12), A(:,41), n3(:,177), t3x288(:,:,41), nhel, den(97))
    call cont_QA(nsync, wf16(:,5), wf18(:,14), A(:,42), n3(:,178), t3x288(:,:,42), nhel, den(102))
    call cont_QA(nsync, wf24(:,10), wf12(:,22), A(:,43), n3(:,179), t3x288(:,:,43), nhel, den(104))
    call cont_QA(nsync, wf18(:,14), wf16(:,6), A(:,44), n3(:,180), t3x288(:,:,44), nhel, den(107))
    call cont_VV(nsync, wf8(:,6), wf36(:,13), A(:,45), n3(:,181), t3x288(:,:,45), nhel, den(108))
    call cont_QA(nsync, wf12(:,18), wf24(:,11), A(:,46), n3(:,182), t3x288(:,:,46), nhel, den(109))
    call cont_QA(nsync, wf24(:,12), wf12(:,24), A(:,47), n3(:,183), t3x288(:,:,47), nhel, den(115))
    call cont_QA(nsync, wf24(:,13), wf12(:,26), A(:,48), n3(:,184), t3x288(:,:,48), nhel, den(119))
    call cont_VV(nsync, wf8(:,6), wf36(:,14), A(:,49), n3(:,185), t3x288(:,:,49), nhel, den(121))
    call cont_QA(nsync, wf24(:,12), wf12(:,28), A(:,50), n3(:,186), t3x288(:,:,50), nhel, den(124))
    call cont_QA(nsync, wf16(:,7), wf18(:,16), A(:,51), n3(:,187), t3x288(:,:,51), nhel, den(128))
    call cont_QA(nsync, wf12(:,26), wf24(:,14), A(:,52), n3(:,188), t3x288(:,:,52), nhel, den(131))
    call cont_QA(nsync, wf18(:,16), wf16(:,8), A(:,53), n3(:,189), t3x288(:,:,53), nhel, den(132))
    call cont_VV(nsync, wf8(:,6), wf36(:,15), A(:,54), n3(:,190), t3x288(:,:,54), nhel, den(133))
    call cont_QA(nsync, wf16(:,5), wf18(:,18), A(:,55), n3(:,191), t3x288(:,:,55), nhel, den(135))
    call cont_QA(nsync, wf24(:,13), wf12(:,30), A(:,56), n3(:,192), t3x288(:,:,56), nhel, den(137))
    call cont_QA(nsync, wf16(:,6), wf18(:,18), A(:,57), n3(:,193), t3x288(:,:,57), nhel, den(138))
    call cont_VV(nsync, wf8(:,6), wf36(:,16), A(:,58), n3(:,194), t3x288(:,:,58), nhel, den(139))
    call cont_QA(nsync, wf12(:,24), wf24(:,15), A(:,59), n3(:,195), t3x288(:,:,59), nhel, den(140))
    call cont_QA(nsync, wf24(:,9), wf12(:,32), A(:,60), n3(:,196), t3x288(:,:,60), nhel, den(142))
    call cont_QA(nsync, wf16(:,7), wf18(:,20), A(:,61), n3(:,197), t3x288(:,:,61), nhel, den(144))
    call cont_QA(nsync, wf12(:,20), wf24(:,16), A(:,62), n3(:,198), t3x288(:,:,62), nhel, den(145))
    call cont_QA(nsync, wf16(:,8), wf18(:,20), A(:,63), n3(:,199), t3x288(:,:,63), nhel, den(146))
    call cont_VV(nsync, wf8(:,6), wf36(:,17), A(:,64), n3(:,200), t3x288(:,:,64), nhel, den(147))
    call cont_QA(nsync, wf24(:,11), wf12(:,32), A(:,65), n3(:,201), t3x288(:,:,65), nhel, den(148))
    call cont_QA(nsync, wf12(:,28), wf24(:,15), A(:,66), n3(:,202), t3x288(:,:,66), nhel, den(149))
    call cont_QA(nsync, wf12(:,22), wf24(:,16), A(:,67), n3(:,203), t3x288(:,:,67), nhel, den(150))
    call cont_QA(nsync, wf24(:,14), wf12(:,30), A(:,68), n3(:,204), t3x288(:,:,68), nhel, den(151))
    call cont_VV(nsync, wf36(:,12), wf8(:,11), A(:,69), n3(:,205), t3x288(:,:,69), nhel, den(153))
    call cont_VV(nsync, wf36(:,12), wf8(:,12), A(:,70), n3(:,206), t3x288(:,:,70), nhel, den(155))
    call cont_VV(nsync, wf12(:,33), wf24(:,17), A(:,71), n3(:,207), t3x288(:,:,71), nhel, den(159))
    call cont_VV(nsync, wf12(:,34), wf24(:,18), A(:,72), n3(:,208), t3x288(:,:,72), nhel, den(163))
    call cont_QA(nsync, wf12(:,8), wf24(:,19), A(:,73), n3(:,209), t3x288(:,:,73), nhel, den(164))
    call cont_VV(nsync, wf12(:,33), wf24(:,20), A(:,74), n3(:,210), t3x288(:,:,74), nhel, den(165))
    call cont_QA(nsync, wf12(:,18), wf24(:,21), A(:,75), n3(:,211), t3x288(:,:,75), nhel, den(166))
    call cont_VV(nsync, wf24(:,17), wf12(:,35), A(:,76), n3(:,212), t3x288(:,:,76), nhel, den(168))
    call cont_VV(nsync, wf12(:,34), wf24(:,22), A(:,77), n3(:,213), t3x288(:,:,77), nhel, den(170))
    call cont_QA(nsync, wf12(:,4), wf24(:,23), A(:,78), n3(:,214), t3x288(:,:,78), nhel, den(171))
    call cont_VV(nsync, wf24(:,20), wf12(:,35), A(:,79), n3(:,215), t3x288(:,:,79), nhel, den(172))
    call cont_QA(nsync, wf12(:,18), wf24(:,24), A(:,80), n3(:,216), t3x288(:,:,80), nhel, den(173))
    call cont_QA(nsync, wf12(:,12), wf24(:,19), A(:,81), n3(:,217), t3x288(:,:,81), nhel, den(174))
    call cont_QA(nsync, wf18(:,14), wf16(:,9), A(:,82), n3(:,218), t3x288(:,:,82), nhel, den(175))
    call cont_QA(nsync, wf12(:,6), wf24(:,23), A(:,83), n3(:,219), t3x288(:,:,83), nhel, den(176))
    call cont_QA(nsync, wf18(:,14), wf16(:,10), A(:,84), n3(:,220), t3x288(:,:,84), nhel, den(177))
    call cont_VV(nsync, wf36(:,14), wf8(:,11), A(:,85), n3(:,221), t3x288(:,:,85), nhel, den(178))
    call cont_VV(nsync, wf36(:,14), wf8(:,12), A(:,86), n3(:,222), t3x288(:,:,86), nhel, den(179))
    call cont_VV(nsync, wf12(:,33), wf24(:,25), A(:,87), n3(:,223), t3x288(:,:,87), nhel, den(181))
    call cont_VV(nsync, wf24(:,18), wf12(:,36), A(:,88), n3(:,224), t3x288(:,:,88), nhel, den(183))
    call cont_QA(nsync, wf12(:,8), wf24(:,26), A(:,89), n3(:,225), t3x288(:,:,89), nhel, den(184))
    call cont_VV(nsync, wf12(:,33), wf24(:,27), A(:,90), n3(:,226), t3x288(:,:,90), nhel, den(185))
    call cont_QA(nsync, wf12(:,26), wf24(:,28), A(:,91), n3(:,227), t3x288(:,:,91), nhel, den(186))
    call cont_VV(nsync, wf12(:,35), wf24(:,25), A(:,92), n3(:,228), t3x288(:,:,92), nhel, den(187))
    call cont_VV(nsync, wf24(:,22), wf12(:,36), A(:,93), n3(:,229), t3x288(:,:,93), nhel, den(188))
    call cont_QA(nsync, wf12(:,4), wf24(:,29), A(:,94), n3(:,230), t3x288(:,:,94), nhel, den(189))
    call cont_VV(nsync, wf12(:,35), wf24(:,27), A(:,95), n3(:,231), t3x288(:,:,95), nhel, den(190))
    call cont_QA(nsync, wf12(:,26), wf24(:,30), A(:,96), n3(:,232), t3x288(:,:,96), nhel, den(191))
    call cont_QA(nsync, wf12(:,12), wf24(:,26), A(:,97), n3(:,233), t3x288(:,:,97), nhel, den(192))
    call cont_QA(nsync, wf18(:,16), wf16(:,11), A(:,98), n3(:,234), t3x288(:,:,98), nhel, den(193))
    call cont_QA(nsync, wf12(:,6), wf24(:,29), A(:,99), n3(:,235), t3x288(:,:,99), nhel, den(194))
    call cont_QA(nsync, wf18(:,16), wf16(:,12), A(:,100), n3(:,236), t3x288(:,:,100), nhel, den(195))
    call cont_VV(nsync, wf12(:,37), wf24(:,31), A(:,101), n3(:,237), t3x288(:,:,101), nhel, den(199))
    call cont_VV(nsync, wf12(:,38), wf24(:,32), A(:,102), n3(:,238), t3x288(:,:,102), nhel, den(203))
    call cont_QA(nsync, wf12(:,2), wf24(:,33), A(:,103), n3(:,239), t3x288(:,:,103), nhel, den(204))
    call cont_VV(nsync, wf12(:,37), wf24(:,34), A(:,104), n3(:,240), t3x288(:,:,104), nhel, den(205))
    call cont_QA(nsync, wf12(:,24), wf24(:,35), A(:,105), n3(:,241), t3x288(:,:,105), nhel, den(206))
    call cont_VV(nsync, wf12(:,37), wf24(:,36), A(:,106), n3(:,242), t3x288(:,:,106), nhel, den(208))
    call cont_VV(nsync, wf24(:,32), wf12(:,39), A(:,107), n3(:,243), t3x288(:,:,107), nhel, den(210))
    call cont_QA(nsync, wf12(:,2), wf24(:,37), A(:,108), n3(:,244), t3x288(:,:,108), nhel, den(211))
    call cont_VV(nsync, wf12(:,37), wf24(:,38), A(:,109), n3(:,245), t3x288(:,:,109), nhel, den(212))
    call cont_QA(nsync, wf12(:,20), wf24(:,39), A(:,110), n3(:,246), t3x288(:,:,110), nhel, den(213))
    call cont_VV(nsync, wf36(:,1), wf8(:,13), A(:,111), n3(:,247), t3x288(:,:,111), nhel, den(215))
    call cont_VV(nsync, wf36(:,1), wf8(:,14), A(:,112), n3(:,248), t3x288(:,:,112), nhel, den(217))
    call cont_QA(nsync, wf18(:,2), wf16(:,13), A(:,113), n3(:,249), t3x288(:,:,113), nhel, den(218))
    call cont_QA(nsync, wf12(:,28), wf24(:,35), A(:,114), n3(:,250), t3x288(:,:,114), nhel, den(219))
    call cont_QA(nsync, wf18(:,2), wf16(:,14), A(:,115), n3(:,251), t3x288(:,:,115), nhel, den(220))
    call cont_QA(nsync, wf12(:,22), wf24(:,39), A(:,116), n3(:,252), t3x288(:,:,116), nhel, den(221))
    call cont_VV(nsync, wf24(:,31), wf12(:,40), A(:,117), n3(:,253), t3x288(:,:,117), nhel, den(223))
    call cont_VV(nsync, wf12(:,38), wf24(:,40), A(:,118), n3(:,254), t3x288(:,:,118), nhel, den(225))
    call cont_QA(nsync, wf12(:,10), wf24(:,41), A(:,119), n3(:,255), t3x288(:,:,119), nhel, den(226))
    call cont_VV(nsync, wf24(:,34), wf12(:,40), A(:,120), n3(:,256), t3x288(:,:,120), nhel, den(227))
    call cont_QA(nsync, wf12(:,24), wf24(:,42), A(:,121), n3(:,257), t3x288(:,:,121), nhel, den(228))
    call cont_VV(nsync, wf24(:,36), wf12(:,40), A(:,122), n3(:,258), t3x288(:,:,122), nhel, den(229))
    call cont_VV(nsync, wf12(:,39), wf24(:,40), A(:,123), n3(:,259), t3x288(:,:,123), nhel, den(230))
    call cont_QA(nsync, wf12(:,10), wf24(:,43), A(:,124), n3(:,260), t3x288(:,:,124), nhel, den(231))
    call cont_VV(nsync, wf24(:,38), wf12(:,40), A(:,125), n3(:,261), t3x288(:,:,125), nhel, den(232))
    call cont_QA(nsync, wf12(:,20), wf24(:,44), A(:,126), n3(:,262), t3x288(:,:,126), nhel, den(233))
    call cont_VV(nsync, wf36(:,3), wf8(:,13), A(:,127), n3(:,263), t3x288(:,:,127), nhel, den(234))
    call cont_VV(nsync, wf36(:,3), wf8(:,14), A(:,128), n3(:,264), t3x288(:,:,128), nhel, den(235))
    call cont_QA(nsync, wf18(:,4), wf16(:,15), A(:,129), n3(:,265), t3x288(:,:,129), nhel, den(236))
    call cont_QA(nsync, wf12(:,28), wf24(:,42), A(:,130), n3(:,266), t3x288(:,:,130), nhel, den(237))
    call cont_QA(nsync, wf18(:,4), wf16(:,16), A(:,131), n3(:,267), t3x288(:,:,131), nhel, den(238))
    call cont_QA(nsync, wf12(:,22), wf24(:,44), A(:,132), n3(:,268), t3x288(:,:,132), nhel, den(239))
    call cont_QA(nsync, wf12(:,16), wf24(:,33), A(:,133), n3(:,269), t3x288(:,:,133), nhel, den(240))
    call cont_QA(nsync, wf18(:,18), wf16(:,9), A(:,134), n3(:,270), t3x288(:,:,134), nhel, den(241))
    call cont_QA(nsync, wf12(:,14), wf24(:,41), A(:,135), n3(:,271), t3x288(:,:,135), nhel, den(242))
    call cont_QA(nsync, wf18(:,18), wf16(:,10), A(:,136), n3(:,272), t3x288(:,:,136), nhel, den(243))
    call cont_QA(nsync, wf12(:,16), wf24(:,37), A(:,137), n3(:,273), t3x288(:,:,137), nhel, den(244))
    call cont_QA(nsync, wf18(:,20), wf16(:,11), A(:,138), n3(:,274), t3x288(:,:,138), nhel, den(245))
    call cont_QA(nsync, wf12(:,14), wf24(:,43), A(:,139), n3(:,275), t3x288(:,:,139), nhel, den(246))
    call cont_QA(nsync, wf18(:,20), wf16(:,12), A(:,140), n3(:,276), t3x288(:,:,140), nhel, den(247))
    call cont_QA(nsync, wf18(:,6), wf16(:,13), A(:,141), n3(:,277), t3x288(:,:,141), nhel, den(248))
    call cont_QA(nsync, wf12(:,32), wf24(:,21), A(:,142), n3(:,278), t3x288(:,:,142), nhel, den(249))
    call cont_QA(nsync, wf18(:,6), wf16(:,14), A(:,143), n3(:,279), t3x288(:,:,143), nhel, den(250))
    call cont_QA(nsync, wf12(:,30), wf24(:,28), A(:,144), n3(:,280), t3x288(:,:,144), nhel, den(251))
    call cont_QA(nsync, wf18(:,8), wf16(:,15), A(:,145), n3(:,281), t3x288(:,:,145), nhel, den(252))
    call cont_QA(nsync, wf12(:,32), wf24(:,24), A(:,146), n3(:,282), t3x288(:,:,146), nhel, den(253))
    call cont_QA(nsync, wf18(:,8), wf16(:,16), A(:,147), n3(:,283), t3x288(:,:,147), nhel, den(254))
    call cont_QA(nsync, wf12(:,30), wf24(:,30), A(:,148), n3(:,284), t3x288(:,:,148), nhel, den(255))
    call cont_QA(nsync, wf18(:,10), wf16(:,15), A(:,149), n3(:,285), t3x288(:,:,149), nhel, den(256))
    call cont_QA(nsync, wf18(:,12), wf16(:,13), A(:,150), n3(:,286), t3x288(:,:,150), nhel, den(257))
    call cont_QA(nsync, wf18(:,10), wf16(:,16), A(:,151), n3(:,287), t3x288(:,:,151), nhel, den(258))
    call cont_QA(nsync, wf18(:,12), wf16(:,14), A(:,152), n3(:,288), t3x288(:,:,152), nhel, den(259))

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
  complex(REALKIND), intent(out) :: M1(4) ! M1helarray(4,288)
  integer :: empty(0)

  M1(1) = ((-A(j,39)%j-A(j,40)%j-A(j,42)%j-A(j,43)%j-A(j,44)%j-A(j,46)%j-A(j,47)%j-A(j,48)%j-A(j,50)%j-A(j,51)%j-A(j,52)%j &
       -A(j,53)%j-A(j,55)%j-A(j,56)%j-A(j,57)%j-A(j,59)%j-A(j,60)%j-A(j,61)%j-A(j,62)%j-A(j,63)%j-A(j,65)%j-A(j,66)%j-A(j,67)%j &
       -A(j,68)%j-A(j,71)%j-A(j,75)%j-A(j,76)%j-A(j,80)%j-A(j,87)%j-A(j,91)%j-A(j,92)%j-A(j,96)%j-A(j,101)%j-A(j,105)%j-A(j,106)%j &
       -A(j,110)%j-A(j,111)%j-A(j,112)%j-A(j,113)%j-A(j,114)%j-A(j,115)%j-A(j,116)%j-A(j,117)%j-A(j,121)%j-A(j,122)%j-A(j,126)%j &
       -A(j,127)%j-A(j,128)%j-A(j,129)%j-A(j,130)%j-A(j,131)%j-A(j,132)%j-A(j,141)%j-A(j,142)%j-A(j,143)%j-A(j,144)%j-A(j,145)%j &
       -A(j,146)%j-A(j,147)%j-A(j,148)%j)*f(1))/6._/**/REALKIND+((A(j,149)%j+A(j,150)%j+A(j,151)%j &
       +A(j,152)%j)*f(3))/6._/**/REALKIND
  M1(2) = ((A(j,2)%j+A(j,6)%j+A(j,7)%j+A(j,10)%j+A(j,12)%j+A(j,13)%j+A(j,19)%j+A(j,20)%j+A(j,22)%j+A(j,23)%j+A(j,35)%j+A(j,36)%j &
       +A(j,40)%j+A(j,42)%j+A(j,43)%j+A(j,48)%j+A(j,52)%j+A(j,53)%j+A(j,55)%j+A(j,56)%j+A(j,62)%j+A(j,63)%j+A(j,67)%j+A(j,68)%j &
       +A(j,69)%j+A(j,71)%j+A(j,73)%j+A(j,76)%j+A(j,77)%j+A(j,81)%j+A(j,82)%j+A(j,85)%j+A(j,89)%j+A(j,91)%j+A(j,93)%j+A(j,96)%j &
       +A(j,97)%j+A(j,98)%j+A(j,101)%j+A(j,103)%j+A(j,108)%j+A(j,110)%j+A(j,112)%j+A(j,115)%j+A(j,116)%j+A(j,117)%j+A(j,118)%j &
       +A(j,123)%j+A(j,126)%j+A(j,128)%j+A(j,131)%j+A(j,132)%j+A(j,133)%j+A(j,134)%j+A(j,137)%j+A(j,138)%j+A(j,143)%j+A(j,144)%j &
       +A(j,147)%j+A(j,148)%j)*f(1))/2._/**/REALKIND+(CI*(A(j,1)%j+A(j,8)%j+A(j,9)%j+A(j,16)%j+A(j,21)%j+A(j,26)%j-A(j,41)%j &
       -A(j,45)%j-A(j,49)%j-A(j,54)%j-A(j,58)%j-A(j,64)%j+A(j,74)%j+A(j,79)%j+A(j,90)%j+A(j,95)%j+A(j,104)%j+A(j,109)%j+A(j,120)%j &
       +A(j,125)%j)*f(2))/2._/**/REALKIND+((-A(j,27)%j-A(j,28)%j-A(j,32)%j-A(j,151)%j-A(j,152)%j)*f(3))/2._/**/REALKIND+(CI*( &
       -A(j,33)%j-A(j,34)%j)*f(4))/2._/**/REALKIND
  M1(3) = ((A(j,3)%j+A(j,4)%j+A(j,5)%j+A(j,11)%j+A(j,14)%j+A(j,15)%j+A(j,17)%j+A(j,18)%j+A(j,24)%j+A(j,25)%j+A(j,37)%j+A(j,38)%j &
       +A(j,39)%j+A(j,44)%j+A(j,46)%j+A(j,47)%j+A(j,50)%j+A(j,51)%j+A(j,57)%j+A(j,59)%j+A(j,60)%j+A(j,61)%j+A(j,65)%j+A(j,66)%j &
       +A(j,70)%j+A(j,72)%j+A(j,75)%j+A(j,78)%j+A(j,80)%j+A(j,83)%j+A(j,84)%j+A(j,86)%j+A(j,87)%j+A(j,88)%j+A(j,92)%j+A(j,94)%j &
       +A(j,99)%j+A(j,100)%j+A(j,102)%j+A(j,105)%j+A(j,106)%j+A(j,107)%j+A(j,111)%j+A(j,113)%j+A(j,114)%j+A(j,119)%j+A(j,121)%j &
       +A(j,122)%j+A(j,124)%j+A(j,127)%j+A(j,129)%j+A(j,130)%j+A(j,135)%j+A(j,136)%j+A(j,139)%j+A(j,140)%j+A(j,141)%j+A(j,142)%j &
       +A(j,145)%j+A(j,146)%j)*f(1))/2._/**/REALKIND+(CI*(-A(j,1)%j-A(j,8)%j-A(j,9)%j-A(j,16)%j-A(j,21)%j-A(j,26)%j+A(j,41)%j &
       +A(j,45)%j+A(j,49)%j+A(j,54)%j+A(j,58)%j+A(j,64)%j-A(j,74)%j-A(j,79)%j-A(j,90)%j-A(j,95)%j-A(j,104)%j-A(j,109)%j-A(j,120)%j &
       -A(j,125)%j)*f(2))/2._/**/REALKIND+((-A(j,29)%j-A(j,30)%j-A(j,31)%j-A(j,149)%j-A(j,150)%j)*f(3))/2._/**/REALKIND &
       +(CI*(A(j,33)%j+A(j,34)%j)*f(4))/2._/**/REALKIND
  M1(4) = ((-A(j,2)%j-A(j,3)%j-A(j,4)%j-A(j,5)%j-A(j,6)%j-A(j,7)%j-A(j,10)%j-A(j,11)%j-A(j,12)%j-A(j,13)%j-A(j,14)%j-A(j,15)%j &
       -A(j,17)%j-A(j,18)%j-A(j,19)%j-A(j,20)%j-A(j,22)%j-A(j,23)%j-A(j,24)%j-A(j,25)%j-A(j,35)%j-A(j,36)%j-A(j,37)%j-A(j,38)%j &
       -A(j,69)%j-A(j,70)%j-A(j,72)%j-A(j,73)%j-A(j,77)%j-A(j,78)%j-A(j,81)%j-A(j,82)%j-A(j,83)%j-A(j,84)%j-A(j,85)%j-A(j,86)%j &
       -A(j,88)%j-A(j,89)%j-A(j,93)%j-A(j,94)%j-A(j,97)%j-A(j,98)%j-A(j,99)%j-A(j,100)%j-A(j,102)%j-A(j,103)%j-A(j,107)%j &
       -A(j,108)%j-A(j,118)%j-A(j,119)%j-A(j,123)%j-A(j,124)%j-A(j,133)%j-A(j,134)%j-A(j,135)%j-A(j,136)%j-A(j,137)%j-A(j,138)%j &
       -A(j,139)%j-A(j,140)%j)*f(1))/6._/**/REALKIND+((A(j,27)%j+A(j,28)%j+A(j,29)%j+A(j,30)%j+A(j,31)%j &
       +A(j,32)%j)*f(3))/6._/**/REALKIND

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
  use ol_colourmatrix_ppzzjj_uuxbbxzzg_1_/**/REALKIND, only: K1
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
  use ol_colourmatrix_ppzzjj_uuxbbxzzg_1_/**/REALKIND, only: K1
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
  & bind(c,name="ol_tree_colvect_ppzzjj_uuxbbxzzg_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(4,288)
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
    & bind(c,name="ol_f_amp2tree_ppzzjj_uuxbbxzzg_1")
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
    & bind(c,name="ol_f_amp2ccone_ppzzjj_uuxbbxzzg_1")
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
    & bind(c,name="ol_f_amp2ccall_ppzzjj_uuxbbxzzg_1")
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
    & bind(c,name="ol_f_amp2hcone_ppzzjj_uuxbbxzzg_1")
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
    & bind(c,name="ol_f_amp2hcall_ppzzjj_uuxbbxzzg_1")
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
    & bind(c,name="ol_amp2tree_ppzzjj_uuxbbxzzg_1")
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
    & bind(c,name="ol_amp2ccone_ppzzjj_uuxbbxzzg_1")
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
    & bind(c,name="ol_amp2ccall_ppzzjj_uuxbbxzzg_1")
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
    & bind(c,name="ol_amp2hcone_ppzzjj_uuxbbxzzg_1")
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
    & bind(c,name="ol_amp2hcall_ppzzjj_uuxbbxzzg_1")
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
    & bind(c,name="amp2tree_ppzzjj_uuxbbxzzg_1_")
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
    & bind(c,name="amp2ccone_ppzzjj_uuxbbxzzg_1_")
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
    & bind(c,name="amp2ccall_ppzzjj_uuxbbxzzg_1_")
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
    & bind(c,name="amp2hcone_ppzzjj_uuxbbxzzg_1_")
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
    & bind(c,name="amp2hcall_ppzzjj_uuxbbxzzg_1_")
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

end module ol_tree_ppzzjj_uuxbbxzzg_1_/**/REALKIND
