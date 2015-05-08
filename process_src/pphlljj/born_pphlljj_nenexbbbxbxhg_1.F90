
module ol_colourmatrix_pphlljj_nenexbbbxbxhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(152,4)
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
  K1(  5,:) = [   0,   0,   0,   0]
  K1(  6,:) = [   0,   0,   0,   0]
  K1(  7,:) = [   0,   0,   0,   0]
  K1(  8,:) = [   0,   0,   0,   0]
  K1(  9,:) = [   0,   0,   0,   0]
  K1( 10,:) = [   0,   0,   0,   0]
  K1( 11,:) = [   0,   0,   0,   0]
  K1( 12,:) = [   0,   0,   0,   0]
  K1( 13,:) = [   0,   0,   0,   0]
  K1( 14,:) = [   0,   0,   0,   0]
  K1( 15,:) = [   0,   0,   0,   0]
  K1( 16,:) = [   0,   0,   0,   0]
  K1( 17,:) = [   0,   0,   0,   0]
  K1( 18,:) = [   0,   0,   0,   0]
  K1( 19,:) = [   0,   0,   0,   0]
  K1( 20,:) = [   0,   0,   0,   0]
  K1( 21,:) = [   0,   0,   0,   0]
  K1( 22,:) = [   0,   0,   0,   0]
  K1( 23,:) = [   0,   0,   0,   0]
  K1( 24,:) = [   0,   0,   0,   0]
  K1( 25,:) = [  48,  16,  16,   0]
  K1( 26,:) = [  16,  48,   0,  16]
  K1( 27,:) = [  16,   0,  48,  16]
  K1( 28,:) = [   0,  16,  16,  48]
  K1( 29,:) = [   0,   0,   0,   0]
  K1( 30,:) = [   0,   0,   0,   0]
  K1( 31,:) = [   0,   0,   0,   0]
  K1( 32,:) = [   0,   0,   0,   0]
  K1( 33,:) = [   0,   0,   0,   0]
  K1( 34,:) = [   0,   0,   0,   0]
  K1( 35,:) = [   0,   0,   0,   0]
  K1( 36,:) = [   0,   0,   0,   0]
  K1( 37,:) = [   0,  16,  -2,   6]
  K1( 38,:) = [  16,   0,   6,  -2]
  K1( 39,:) = [  -2,   6,   0,  16]
  K1( 40,:) = [   6,  -2,  16,   0]
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
  K1( 53,:) = [   6,   2,   2,   0]
  K1( 54,:) = [   2,   0,  -6, -16]
  K1( 55,:) = [   2,  -6,   0, -16]
  K1( 56,:) = [   0, -16, -16, -48]
  K1( 57,:) = [   0,   2, -16,  -6]
  K1( 58,:) = [   2,   6,   0,   2]
  K1( 59,:) = [ -16,   0, -48, -16]
  K1( 60,:) = [  -6,   2, -16,   0]
  K1( 61,:) = [  48,  16,  16,   0]
  K1( 62,:) = [  16,  48,   0,  16]
  K1( 63,:) = [  16,   0,  48,  16]
  K1( 64,:) = [   0,  16,  16,  48]
  K1( 65,:) = [   0,   0,   0,   0]
  K1( 66,:) = [   0,   0,   0,   0]
  K1( 67,:) = [   0,   0,   0,   0]
  K1( 68,:) = [   0,   0,   0,   0]
  K1( 69,:) = [   0,   0,   0,   0]
  K1( 70,:) = [   0,   0,   0,   0]
  K1( 71,:) = [   0,   0,   0,   0]
  K1( 72,:) = [   0,   0,   0,   0]
  K1( 73,:) = [   0, -16,   2,  -6]
  K1( 74,:) = [ -16, -48,   0, -16]
  K1( 75,:) = [   2,   0,   6,   2]
  K1( 76,:) = [  -6, -16,   2,   0]
  K1( 77,:) = [ -48, -16, -16,   0]
  K1( 78,:) = [ -16,   0,  -6,   2]
  K1( 79,:) = [ -16,  -6,   0,   2]
  K1( 80,:) = [   0,   2,   2,   6]
  K1( 81,:) = [   0,  -2,  16,   6]
  K1( 82,:) = [  -2,   0,   6,  16]
  K1( 83,:) = [  16,   6,   0,  -2]
  K1( 84,:) = [   6,  16,  -2,   0]
  K1( 85,:) = [  48,  16,  16,   0]
  K1( 86,:) = [  16,  48,   0,  16]
  K1( 87,:) = [  16,   0,  48,  16]
  K1( 88,:) = [   0,  16,  16,  48]
  K1( 89,:) = [   0,   0,   0,   0]
  K1( 90,:) = [   0,   0,   0,   0]
  K1( 91,:) = [   0,   0,   0,   0]
  K1( 92,:) = [   0,   0,   0,   0]
  K1( 93,:) = [   0,   0,   0,   0]
  K1( 94,:) = [   0,   0,   0,   0]
  K1( 95,:) = [   0,   0,   0,   0]
  K1( 96,:) = [   0,   0,   0,   0]
  K1( 97,:) = [   0,   0,   0,   0]
  K1( 98,:) = [   0,   0,   0,   0]
  K1( 99,:) = [   0,   0,   0,   0]
  K1(100,:) = [   0,   0,   0,   0]
  K1(101,:) = [   0,   0,   0,   0]
  K1(102,:) = [   0,   0,   0,   0]
  K1(103,:) = [   0,   0,   0,   0]
  K1(104,:) = [   0,   0,   0,   0]
  K1(105,:) = [   0,   0,   0,   0]
  K1(106,:) = [   0,   0,   0,   0]
  K1(107,:) = [   0,   0,   0,   0]
  K1(108,:) = [   0,   0,   0,   0]
  K1(109,:) = [   0,   0,   0,   0]
  K1(110,:) = [   0,   0,   0,   0]
  K1(111,:) = [   0,   0,   0,   0]
  K1(112,:) = [   0,   0,   0,   0]
  K1(113,:) = [   0,   0,   0,   0]
  K1(114,:) = [   0,   0,   0,   0]
  K1(115,:) = [   0,   0,   0,   0]
  K1(116,:) = [   0,   0,   0,   0]
  K1(117,:) = [   0,   0,   0,   0]
  K1(118,:) = [   0,   0,   0,   0]
  K1(119,:) = [   0,   0,   0,   0]
  K1(120,:) = [   0,   0,   0,   0]
  K1(121,:) = [   0,   0,   0,   0]
  K1(122,:) = [   0,   0,   0,   0]
  K1(123,:) = [   0,   0,   0,   0]
  K1(124,:) = [   0,   0,   0,   0]
  K1(125,:) = [ -54, -18, -18,   0]
  K1(126,:) = [ -18,   0,   0,  18]
  K1(127,:) = [ -18,   0, -54, -18]
  K1(128,:) = [   0,  18, -18,   0]
  K1(129,:) = [   0, -18,  18,   0]
  K1(130,:) = [ -18, -54,   0, -18]
  K1(131,:) = [  18,   0,   0, -18]
  K1(132,:) = [   0, -18, -18, -54]
  K1(133,:) = [ -54, -18, -18,   0]
  K1(134,:) = [ -18, -54,   0, -18]
  K1(135,:) = [ -18,   0,   0,  18]
  K1(136,:) = [   0, -18,  18,   0]
  K1(137,:) = [   0,  18, -18,   0]
  K1(138,:) = [  18,   0,   0, -18]
  K1(139,:) = [ -18,   0, -54, -18]
  K1(140,:) = [   0, -18, -18, -54]
  K1(141,:) = [   0,   0,   0,   0]
  K1(142,:) = [   0,   0,   0,   0]
  K1(143,:) = [   0,   0,   0,   0]
  K1(144,:) = [   0,   0,   0,   0]
  K1(145,:) = [ 108,  36,  36,   0]
  K1(146,:) = [  36, 108,   0,  36]
  K1(147,:) = [  36,   0, 108,  36]
  K1(148,:) = [   0,  36,  36, 108]
  K1(149,:) = [   0,   0,   0,   0]
  K1(150,:) = [   0,   0,   0,   0]
  K1(151,:) = [   0,   0,   0,   0]
  K1(152,:) = [   0,   0,   0,   0]
  K1 = (1._/**/REALKIND / 3) * K1

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphlljj_nenexbbbxbxhg_1_/**/REALKIND



module ol_forced_parameters_pphlljj_nenexbbbxbxhg_1_/**/REALKIND
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
  if (YE /= 0) write(*,101) 'YE = 0'
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


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphlljj_nenexbbbxbxhg_1_/**/REALKIND

module ol_tree_pphlljj_nenexbbbxbxhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(4)
  complex(REALKIND), save :: den(627)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 128 ! number of helicity configurations
  integer(intkind2), save :: nhel = 128 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(128) ! physical helicity states
  complex(DREALKIND) :: M1helarr(4,128) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = (CI*eQED**3*gQCD**3*lambdaHZZ*MW)/(cw**2*sw)
    f(2) = (eQED**3*gQCD**3*lambdaHZZ*MW)/(cw**2*sw)
    f(3) = (CI*eQED**3*gQCD**3*YB)/(2._/**/REALKIND*MW*sw)
    f(4) = (eQED**3*gQCD**3*YB)/(MW*sw*2._/**/REALKIND)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,3) - MZ2)
  den(2) = 1 / (Q(5,72) - MB2)
  den(3) = 1 / (Q(5,20))
  den(4) = 1 / (Q(5,160) - MB2)
  den(7) = 1 / (Q(5,75) - MB2)
  den(12) = 1 / (Q(5,163) - MB2)
  den(15) = 1 / (Q(5,35) - MB2)
  den(17) = 1 / (Q(5,148))
  den(21) = 1 / (Q(5,200) - MB2)
  den(25) = 1 / (Q(5,92) - MB2)
  den(28) = 1 / (Q(5,52) - MB2)
  den(34) = 1 / (Q(5,136) - MB2)
  den(35) = 1 / (Q(5,96) - MB2)
  den(38) = 1 / (Q(5,139) - MB2)
  den(43) = 1 / (Q(5,99) - MB2)
  den(46) = 1 / (Q(5,11) - MB2)
  den(50) = 1 / (Q(5,116) - MB2)
  den(53) = 1 / (Q(5,224) - MB2)
  den(57) = 1 / (Q(5,28) - MB2)
  den(65) = 1 / (Q(5,156) - MB2)
  den(68) = 1 / (Q(5,67) - MZ2)
  den(75) = 1 / (Q(5,180) - MB2)
  den(103) = 1 / (Q(5,68) - MB2)
  den(104) = 1 / (Q(5,24))
  den(107) = 1 / (Q(5,71) - MB2)
  den(112) = 1 / (Q(5,196) - MB2)
  den(116) = 1 / (Q(5,152))
  den(122) = 1 / (Q(5,56) - MB2)
  den(128) = 1 / (Q(5,132) - MB2)
  den(131) = 1 / (Q(5,135) - MB2)
  den(136) = 1 / (Q(5,7) - MB2)
  den(138) = 1 / (Q(5,120) - MB2)
  den(159) = 1 / (Q(5,184) - MB2)
  den(184) = 1 / (Q(5,36))
  den(185) = 1 / (Q(5,144) - MB2)
  den(190) = 1 / (Q(5,147) - MB2)
  den(193) = 1 / (Q(5,19) - MB2)
  den(195) = 1 / (Q(5,164))
  den(201) = 1 / (Q(5,108) - MB2)
  den(209) = 1 / (Q(5,80) - MB2)
  den(214) = 1 / (Q(5,83) - MB2)
  den(221) = 1 / (Q(5,208) - MB2)
  den(225) = 1 / (Q(5,44) - MB2)
  den(232) = 1 / (Q(5,172) - MB2)
  den(265) = 1 / (Q(5,40))
  den(272) = 1 / (Q(5,168))
  den(328) = 1 / (Q(5,100))
  den(334) = 1 / (Q(5,84))
  den(348) = 1 / (Q(5,212))
  den(360) = 1 / (Q(5,228))
  den(369) = 1 / (Q(5,43))
  den(374) = 1 / (Q(5,27))
  den(394) = 1 / (Q(5,104))
  den(401) = 1 / (Q(5,88))
  den(410) = 1 / (Q(5,216))
  den(422) = 1 / (Q(5,232))
  den(433) = 1 / (Q(5,23))
  den(437) = 1 / (Q(5,39))

  ! denominators

  den(5) = den(1)*den(2)
  den(6) = den(3)*den(4)
  den(8) = den(5)*den(7)
  den(9) = den(6)*den(8)
  den(10) = den(1)*den(4)
  den(11) = den(2)*den(3)
  den(13) = den(10)*den(12)
  den(14) = den(11)*den(13)
  den(16) = den(1)*den(15)
  den(18) = den(3)*den(17)
  den(19) = den(2)*den(16)
  den(20) = den(18)*den(19)
  den(22) = den(2)*den(21)
  den(23) = den(3)*den(16)
  den(24) = den(22)*den(23)
  den(26) = den(11)*den(25)
  den(27) = den(16)*den(26)
  den(29) = den(3)*den(28)
  den(30) = den(1)*den(29)
  den(31) = den(22)*den(30)
  den(32) = den(8)*den(29)
  den(33) = den(8)*den(18)
  den(36) = den(1)*den(34)
  den(37) = den(3)*den(35)
  den(39) = den(36)*den(38)
  den(40) = den(37)*den(39)
  den(41) = den(1)*den(35)
  den(42) = den(3)*den(34)
  den(44) = den(41)*den(43)
  den(45) = den(42)*den(44)
  den(47) = den(1)*den(46)
  den(48) = den(35)*den(47)
  den(49) = den(18)*den(48)
  den(51) = den(37)*den(50)
  den(52) = den(47)*den(51)
  den(54) = den(35)*den(53)
  den(55) = den(3)*den(47)
  den(56) = den(54)*den(55)
  den(58) = den(3)*den(57)
  den(59) = den(44)*den(58)
  den(60) = den(1)*den(58)
  den(61) = den(54)*den(60)
  den(62) = den(18)*den(44)
  den(63) = den(21)*den(34)
  den(64) = den(23)*den(63)
  den(66) = den(42)*den(65)
  den(67) = den(16)*den(66)
  den(69) = den(1)*den(68)
  den(70) = den(29)*den(34)
  den(71) = den(69)*den(70)
  den(72) = den(66)*den(69)
  den(73) = den(30)*den(63)
  den(74) = den(29)*den(39)
  den(76) = den(6)*den(75)
  den(77) = den(47)*den(76)
  den(78) = den(4)*den(53)
  den(79) = den(55)*den(78)
  den(80) = den(4)*den(58)
  den(81) = den(69)*den(80)
  den(82) = den(69)*den(76)
  den(83) = den(13)*den(58)
  den(84) = den(60)*den(78)
  den(85) = den(7)*den(47)
  den(86) = den(29)*den(85)
  den(87) = den(29)*den(50)
  den(88) = den(47)*den(87)
  den(89) = den(18)*den(75)
  den(90) = den(47)*den(89)
  den(91) = den(25)*den(58)
  den(92) = den(16)*den(91)
  den(93) = den(16)*den(43)
  den(94) = den(58)*den(93)
  den(95) = den(18)*den(65)
  den(96) = den(16)*den(95)
  den(97) = den(43)*den(69)
  den(98) = den(58)*den(97)
  den(99) = den(7)*den(69)
  den(100) = den(29)*den(99)
  den(101) = den(18)*den(99)
  den(102) = den(69)*den(95)
  den(105) = den(1)*den(103)
  den(106) = den(4)*den(104)
  den(108) = den(105)*den(107)
  den(109) = den(106)*den(108)
  den(110) = den(103)*den(104)
  den(111) = den(13)*den(110)
  den(113) = den(103)*den(112)
  den(114) = den(16)*den(104)
  den(115) = den(113)*den(114)
  den(117) = den(104)*den(116)
  den(118) = den(16)*den(103)
  den(119) = den(117)*den(118)
  den(120) = den(25)*den(110)
  den(121) = den(16)*den(120)
  den(123) = den(104)*den(122)
  den(124) = den(1)*den(123)
  den(125) = den(113)*den(124)
  den(126) = den(108)*den(123)
  den(127) = den(108)*den(117)
  den(129) = den(1)*den(128)
  den(130) = den(35)*den(104)
  den(132) = den(129)*den(131)
  den(133) = den(130)*den(132)
  den(134) = den(104)*den(128)
  den(135) = den(44)*den(134)
  den(137) = den(1)*den(136)
  den(139) = den(130)*den(138)
  den(140) = den(137)*den(139)
  den(141) = den(35)*den(137)
  den(142) = den(117)*den(141)
  den(143) = den(104)*den(137)
  den(144) = den(54)*den(143)
  den(145) = den(57)*den(104)
  den(146) = den(44)*den(145)
  den(147) = den(1)*den(145)
  den(148) = den(54)*den(147)
  den(149) = den(44)*den(117)
  den(150) = den(112)*den(128)
  den(151) = den(114)*den(150)
  den(152) = den(65)*den(134)
  den(153) = den(16)*den(152)
  den(154) = den(123)*den(128)
  den(155) = den(69)*den(154)
  den(156) = den(69)*den(152)
  den(157) = den(124)*den(150)
  den(158) = den(123)*den(132)
  den(160) = den(106)*den(159)
  den(161) = den(137)*den(160)
  den(162) = den(78)*den(143)
  den(163) = den(4)*den(145)
  den(164) = den(69)*den(163)
  den(165) = den(69)*den(160)
  den(166) = den(13)*den(145)
  den(167) = den(78)*den(147)
  den(168) = den(107)*den(137)
  den(169) = den(123)*den(168)
  den(170) = den(117)*den(159)
  den(171) = den(137)*den(170)
  den(172) = den(123)*den(138)
  den(173) = den(137)*den(172)
  den(174) = den(25)*den(145)
  den(175) = den(16)*den(174)
  den(176) = den(93)*den(145)
  den(177) = den(65)*den(117)
  den(178) = den(16)*den(177)
  den(179) = den(97)*den(145)
  den(180) = den(69)*den(107)
  den(181) = den(117)*den(180)
  den(182) = den(69)*den(177)
  den(183) = den(123)*den(180)
  den(186) = den(184)*den(185)
  den(187) = den(8)*den(186)
  den(188) = den(1)*den(185)
  den(189) = den(2)*den(184)
  den(191) = den(188)*den(190)
  den(192) = den(189)*den(191)
  den(194) = den(1)*den(193)
  den(196) = den(184)*den(195)
  den(197) = den(2)*den(194)
  den(198) = den(196)*den(197)
  den(199) = den(184)*den(194)
  den(200) = den(22)*den(199)
  den(202) = den(189)*den(201)
  den(203) = den(194)*den(202)
  den(204) = den(28)*den(184)
  den(205) = den(1)*den(204)
  den(206) = den(22)*den(205)
  den(207) = den(8)*den(204)
  den(208) = den(8)*den(196)
  den(210) = den(184)*den(209)
  den(211) = den(39)*den(210)
  den(212) = den(1)*den(209)
  den(213) = den(34)*den(184)
  den(215) = den(212)*den(214)
  den(216) = den(213)*den(215)
  den(217) = den(47)*den(209)
  den(218) = den(196)*den(217)
  den(219) = den(50)*den(210)
  den(220) = den(47)*den(219)
  den(222) = den(209)*den(221)
  den(223) = den(47)*den(184)
  den(224) = den(222)*den(223)
  den(226) = den(184)*den(225)
  den(227) = den(215)*den(226)
  den(228) = den(1)*den(226)
  den(229) = den(222)*den(228)
  den(230) = den(196)*den(215)
  den(231) = den(63)*den(199)
  den(233) = den(213)*den(232)
  den(234) = den(194)*den(233)
  den(235) = den(34)*den(204)
  den(236) = den(69)*den(235)
  den(237) = den(69)*den(233)
  den(238) = den(63)*den(205)
  den(239) = den(39)*den(204)
  den(240) = den(75)*den(186)
  den(241) = den(47)*den(240)
  den(242) = den(185)*den(221)
  den(243) = den(223)*den(242)
  den(244) = den(185)*den(226)
  den(245) = den(69)*den(244)
  den(246) = den(69)*den(240)
  den(247) = den(191)*den(226)
  den(248) = den(228)*den(242)
  den(249) = den(85)*den(204)
  den(250) = den(50)*den(204)
  den(251) = den(47)*den(250)
  den(252) = den(75)*den(196)
  den(253) = den(47)*den(252)
  den(254) = den(201)*den(226)
  den(255) = den(194)*den(254)
  den(256) = den(194)*den(214)
  den(257) = den(226)*den(256)
  den(258) = den(196)*den(232)
  den(259) = den(194)*den(258)
  den(260) = den(69)*den(214)
  den(261) = den(226)*den(260)
  den(262) = den(99)*den(204)
  den(263) = den(99)*den(196)
  den(264) = den(69)*den(258)
  den(266) = den(185)*den(265)
  den(267) = den(108)*den(266)
  den(268) = den(103)*den(265)
  den(269) = den(191)*den(268)
  den(270) = den(194)*den(265)
  den(271) = den(113)*den(270)
  den(273) = den(265)*den(272)
  den(274) = den(103)*den(194)
  den(275) = den(273)*den(274)
  den(276) = den(201)*den(268)
  den(277) = den(194)*den(276)
  den(278) = den(122)*den(265)
  den(279) = den(1)*den(278)
  den(280) = den(113)*den(279)
  den(281) = den(108)*den(278)
  den(282) = den(108)*den(273)
  den(283) = den(209)*den(265)
  den(284) = den(132)*den(283)
  den(285) = den(128)*den(265)
  den(286) = den(215)*den(285)
  den(287) = den(138)*den(283)
  den(288) = den(137)*den(287)
  den(289) = den(137)*den(209)
  den(290) = den(273)*den(289)
  den(291) = den(137)*den(265)
  den(292) = den(222)*den(291)
  den(293) = den(225)*den(265)
  den(294) = den(215)*den(293)
  den(295) = den(1)*den(293)
  den(296) = den(222)*den(295)
  den(297) = den(215)*den(273)
  den(298) = den(150)*den(270)
  den(299) = den(232)*den(285)
  den(300) = den(194)*den(299)
  den(301) = den(128)*den(278)
  den(302) = den(69)*den(301)
  den(303) = den(69)*den(299)
  den(304) = den(150)*den(279)
  den(305) = den(132)*den(278)
  den(306) = den(159)*den(266)
  den(307) = den(137)*den(306)
  den(308) = den(242)*den(291)
  den(309) = den(185)*den(293)
  den(310) = den(69)*den(309)
  den(311) = den(69)*den(306)
  den(312) = den(191)*den(293)
  den(313) = den(242)*den(295)
  den(314) = den(168)*den(278)
  den(315) = den(159)*den(273)
  den(316) = den(137)*den(315)
  den(317) = den(138)*den(278)
  den(318) = den(137)*den(317)
  den(319) = den(201)*den(293)
  den(320) = den(194)*den(319)
  den(321) = den(256)*den(293)
  den(322) = den(232)*den(273)
  den(323) = den(194)*den(322)
  den(324) = den(260)*den(293)
  den(325) = den(180)*den(273)
  den(326) = den(69)*den(322)
  den(327) = den(180)*den(278)
  den(329) = den(103)*den(328)
  den(330) = den(34)*den(194)
  den(331) = den(329)*den(330)
  den(332) = den(34)*den(272)
  den(333) = den(274)*den(332)
  den(335) = den(103)*den(334)
  den(336) = den(34)*den(335)
  den(337) = den(16)*den(336)
  den(338) = den(34)*den(116)
  den(339) = den(103)*den(338)
  den(340) = den(16)*den(339)
  den(341) = den(39)*den(335)
  den(342) = den(39)*den(329)
  den(343) = den(108)*den(338)
  den(344) = den(108)*den(332)
  den(345) = den(47)*den(185)
  den(346) = den(329)*den(345)
  den(347) = den(103)*den(185)
  den(349) = den(347)*den(348)
  den(350) = den(47)*den(349)
  den(351) = den(116)*den(185)
  den(352) = den(103)*den(351)
  den(353) = den(16)*den(352)
  den(354) = den(16)*den(349)
  den(355) = den(191)*den(329)
  den(356) = den(108)*den(351)
  den(357) = den(47)*den(335)
  den(358) = den(4)*den(357)
  den(359) = den(4)*den(103)
  den(361) = den(359)*den(360)
  den(362) = den(47)*den(361)
  den(363) = den(194)*den(361)
  den(364) = den(4)*den(272)
  den(365) = den(103)*den(364)
  den(366) = den(194)*den(365)
  den(367) = den(13)*den(335)
  den(368) = den(108)*den(364)
  den(370) = den(47)*den(369)
  den(371) = den(335)*den(370)
  den(372) = den(50)*den(335)
  den(373) = den(47)*den(372)
  den(375) = den(47)*den(374)
  den(376) = den(329)*den(375)
  den(377) = den(50)*den(329)
  den(378) = den(47)*den(377)
  den(379) = den(113)*den(375)
  den(380) = den(113)*den(348)
  den(381) = den(47)*den(380)
  den(382) = den(194)*den(374)
  den(383) = den(329)*den(382)
  den(384) = den(201)*den(329)
  den(385) = den(194)*den(384)
  den(386) = den(113)*den(382)
  den(387) = den(25)*den(335)
  den(388) = den(16)*den(387)
  den(389) = den(16)*den(369)
  den(390) = den(335)*den(389)
  den(391) = den(113)*den(389)
  den(392) = den(128)*den(195)
  den(393) = den(197)*den(392)
  den(395) = den(2)*den(394)
  den(396) = den(128)*den(194)
  den(397) = den(395)*den(396)
  den(398) = den(17)*den(128)
  den(399) = den(2)*den(398)
  den(400) = den(16)*den(399)
  den(402) = den(2)*den(401)
  den(403) = den(128)*den(402)
  den(404) = den(16)*den(403)
  den(405) = den(8)*den(398)
  den(406) = den(8)*den(392)
  den(407) = den(132)*den(402)
  den(408) = den(132)*den(395)
  den(409) = den(2)*den(185)
  den(411) = den(409)*den(410)
  den(412) = den(137)*den(411)
  den(413) = den(137)*den(185)
  den(414) = den(395)*den(413)
  den(415) = den(17)*den(185)
  den(416) = den(2)*den(415)
  den(417) = den(16)*den(416)
  den(418) = den(16)*den(411)
  den(419) = den(8)*den(415)
  den(420) = den(191)*den(395)
  den(421) = den(2)*den(4)
  den(423) = den(421)*den(422)
  den(424) = den(137)*den(423)
  den(425) = den(137)*den(402)
  den(426) = den(4)*den(425)
  den(427) = den(194)*den(423)
  den(428) = den(4)*den(195)
  den(429) = den(2)*den(428)
  den(430) = den(194)*den(429)
  den(431) = den(8)*den(428)
  den(432) = den(13)*den(402)
  den(434) = den(137)*den(433)
  den(435) = den(395)*den(434)
  den(436) = den(22)*den(434)
  den(438) = den(137)*den(437)
  den(439) = den(402)*den(438)
  den(440) = den(22)*den(410)
  den(441) = den(137)*den(440)
  den(442) = den(138)*den(402)
  den(443) = den(137)*den(442)
  den(444) = den(138)*den(395)
  den(445) = den(137)*den(444)
  den(446) = den(194)*den(433)
  den(447) = den(22)*den(446)
  den(448) = den(395)*den(446)
  den(449) = den(201)*den(395)
  den(450) = den(194)*den(449)
  den(451) = den(16)*den(437)
  den(452) = den(22)*den(451)
  den(453) = den(25)*den(402)
  den(454) = den(16)*den(453)
  den(455) = den(402)*den(451)
  den(456) = den(217)*den(392)
  den(457) = den(128)*den(209)
  den(458) = den(348)*den(457)
  den(459) = den(47)*den(458)
  den(460) = den(209)*den(401)
  den(461) = den(128)*den(460)
  den(462) = den(16)*den(461)
  den(463) = den(16)*den(458)
  den(464) = den(215)*den(392)
  den(465) = den(132)*den(460)
  den(466) = den(34)*den(209)
  den(467) = den(410)*den(466)
  den(468) = den(137)*den(467)
  den(469) = den(289)*den(332)
  den(470) = den(209)*den(334)
  den(471) = den(34)*den(470)
  den(472) = den(16)*den(471)
  den(473) = den(16)*den(467)
  den(474) = den(39)*den(470)
  den(475) = den(215)*den(332)
  den(476) = den(137)*den(460)
  den(477) = den(4)*den(476)
  den(478) = den(137)*den(364)
  den(479) = den(209)*den(478)
  den(480) = den(47)*den(470)
  den(481) = den(4)*den(480)
  den(482) = den(47)*den(428)
  den(483) = den(209)*den(482)
  den(484) = den(13)*den(470)
  den(485) = den(215)*den(428)
  den(486) = den(13)*den(460)
  den(487) = den(215)*den(364)
  den(488) = den(438)*den(460)
  den(489) = den(222)*den(410)
  den(490) = den(137)*den(489)
  den(491) = den(138)*den(460)
  den(492) = den(137)*den(491)
  den(493) = den(50)*den(470)
  den(494) = den(47)*den(493)
  den(495) = den(370)*den(470)
  den(496) = den(222)*den(348)
  den(497) = den(47)*den(496)
  den(498) = den(25)*den(470)
  den(499) = den(16)*den(498)
  den(500) = den(222)*den(451)
  den(501) = den(25)*den(460)
  den(502) = den(16)*den(501)
  den(503) = den(16)*den(496)
  den(504) = den(389)*den(470)
  den(505) = den(451)*den(460)
  den(506) = den(47)*den(398)
  den(507) = den(35)*den(506)
  den(508) = den(35)*den(128)
  den(509) = den(360)*den(508)
  den(510) = den(47)*den(509)
  den(511) = den(194)*den(509)
  den(512) = den(35)*den(394)
  den(513) = den(128)*den(512)
  den(514) = den(194)*den(513)
  den(515) = den(44)*den(398)
  den(516) = den(132)*den(512)
  den(517) = den(34)*den(35)
  den(518) = den(422)*den(517)
  den(519) = den(137)*den(518)
  den(520) = den(137)*den(338)
  den(521) = den(35)*den(520)
  den(522) = den(194)*den(518)
  den(523) = den(35)*den(328)
  den(524) = den(34)*den(523)
  den(525) = den(194)*den(524)
  den(526) = den(39)*den(523)
  den(527) = den(44)*den(338)
  den(528) = den(137)*den(351)
  den(529) = den(35)*den(528)
  den(530) = den(137)*den(512)
  den(531) = den(185)*den(530)
  den(532) = den(47)*den(415)
  den(533) = den(35)*den(532)
  den(534) = den(47)*den(523)
  den(535) = den(185)*den(534)
  den(536) = den(44)*den(415)
  den(537) = den(191)*den(523)
  den(538) = den(44)*den(351)
  den(539) = den(191)*den(512)
  den(540) = den(54)*den(422)
  den(541) = den(137)*den(540)
  den(542) = den(434)*den(512)
  den(543) = den(138)*den(512)
  den(544) = den(137)*den(543)
  den(545) = den(50)*den(523)
  den(546) = den(47)*den(545)
  den(547) = den(54)*den(360)
  den(548) = den(47)*den(547)
  den(549) = den(375)*den(523)
  den(550) = den(54)*den(446)
  den(551) = den(201)*den(523)
  den(552) = den(194)*den(551)
  den(553) = den(194)*den(547)
  den(554) = den(201)*den(512)
  den(555) = den(194)*den(554)
  den(556) = den(446)*den(512)
  den(557) = den(382)*den(523)
  den(558) = den(75)*den(398)
  den(559) = den(47)*den(558)
  den(560) = den(75)*den(392)
  den(561) = den(47)*den(560)
  den(562) = den(150)*den(375)
  den(563) = den(150)*den(348)
  den(564) = den(47)*den(563)
  den(565) = den(232)*den(392)
  den(566) = den(194)*den(565)
  den(567) = den(150)*den(382)
  den(568) = den(65)*den(398)
  den(569) = den(16)*den(568)
  den(570) = den(150)*den(389)
  den(571) = den(69)*den(568)
  den(572) = den(99)*den(398)
  den(573) = den(69)*den(565)
  den(574) = den(99)*den(392)
  den(575) = den(63)*den(434)
  den(576) = den(63)*den(410)
  den(577) = den(137)*den(576)
  den(578) = den(159)*den(338)
  den(579) = den(137)*den(578)
  den(580) = den(159)*den(332)
  den(581) = den(137)*den(580)
  den(582) = den(63)*den(446)
  den(583) = den(232)*den(332)
  den(584) = den(194)*den(583)
  den(585) = den(63)*den(451)
  den(586) = den(65)*den(338)
  den(587) = den(16)*den(586)
  den(588) = den(69)*den(586)
  den(589) = den(69)*den(583)
  den(590) = den(180)*den(332)
  den(591) = den(180)*den(338)
  den(592) = den(242)*den(410)
  den(593) = den(137)*den(592)
  den(594) = den(159)*den(351)
  den(595) = den(137)*den(594)
  den(596) = den(75)*den(415)
  den(597) = den(47)*den(596)
  den(598) = den(242)*den(348)
  den(599) = den(47)*den(598)
  den(600) = den(65)*den(415)
  den(601) = den(16)*den(600)
  den(602) = den(242)*den(451)
  den(603) = den(65)*den(351)
  den(604) = den(16)*den(603)
  den(605) = den(16)*den(598)
  den(606) = den(69)*den(600)
  den(607) = den(69)*den(603)
  den(608) = den(99)*den(415)
  den(609) = den(180)*den(351)
  den(610) = den(78)*den(422)
  den(611) = den(137)*den(610)
  den(612) = den(159)*den(364)
  den(613) = den(137)*den(612)
  den(614) = den(75)*den(428)
  den(615) = den(47)*den(614)
  den(616) = den(78)*den(360)
  den(617) = den(47)*den(616)
  den(618) = den(78)*den(446)
  den(619) = den(232)*den(428)
  den(620) = den(194)*den(619)
  den(621) = den(194)*den(616)
  den(622) = den(232)*den(364)
  den(623) = den(194)*den(622)
  den(624) = den(69)*den(622)
  den(625) = den(69)*den(619)
  den(626) = den(99)*den(428)
  den(627) = den(180)*den(364)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_pphlljj_nenexbbbxbxhg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_pphlljj_nenexbbbxbxhg_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for nu_e anti-nu_e bottom bottom anti-bottom anti-bottom higgs glue -> 0
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
  use ol_external_pphlljj_nenexbbbxbxhg_1, only: external_perm_pphlljj_nenexbbbxbxhg_1, &
    & external_perm_inv_pphlljj_nenexbbbxbxhg_1, extcomb_perm_pphlljj_nenexbbbxbxhg_1, &
    & average_factor_pphlljj_nenexbbbxbxhg_1
  use ol_external_pphlljj_nenexbbbxbxhg_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_pphlljj_nenexbbbxbxhg_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_pphlljj_nenexbbbxbxhg_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_pphlljj_nenexbbbxbxhg_1
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
  complex(REALKIND) :: MOM_LC(4), M1(4), M1helarray(4,128)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(2), ex6(2), ex7(1), ex8(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf2(2,8), wf4(4,40), wf8(8,156), wf16(16,112), wf32(32,34), wf64(64,6), wf128(128,328)

  type(polcont) :: A(128,328)
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
  extmasses2 = [ rZERO2, rZERO2, rMB2, rMB2, rMB2, rMB2, rMH2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_pphlljj_nenexbbbxbxhg_1,8)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,8)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_pphlljj_nenexbbbxbxhg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_pphlljj_nenexbbbxbxhg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_pphlljj_nenexbbbxbxhg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_Q(P(:,1), rZERO, H1, ex1)
  call wf_A(P(:,2), rZERO, H2, ex2)
  call wf_Q(P(:,3), rMB, H3, ex3)
  call wf_Q(P(:,4), rMB, H4, ex4)
  call wf_A(P(:,5), rMB, H5, ex5)
  call wf_A(P(:,6), rMB, H6, ex6)
  call wf_S(P(:,7), rMH, H7, ex7)
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
  call vert_QA_V(ntry, ex3, ex5, wf4(:,2), n3(:,2), t3x4(:,:,2))
  call vert_QS_A(gH,ntry, ex4, ex7, wf2(:,1), n3(:,3), t3x2(:,:,1))
  call vert_AV_Q(ntry, ex6, ex8, wf4(:,3), n3(:,4), t3x4(:,:,3))
  call prop_W_W(ntry, wf4(:,1), Q(:,3), MZ, 1_intkind1, wf4(:,4), n2(1))
  call prop_Q_A(ntry, wf2(:,1), Q(:,72), MB, 1_intkind1, wf2(:,2), n2(2))
  call prop_A_Q(ntry, wf4(:,3), Q(:,160), MB, 1_intkind1, wf4(:,5), n2(3))
  call vert_ZQ_A(gZd,ntry, wf4(:,4), wf2(:,2), wf8(:,1), n3(:,5), t3x8(:,:,1))
  call vert_AV_Q(ntry, wf4(:,5), wf4(:,2), wf16(:,1), n3(:,6), t3x16(:,:,1))
  call prop_Q_A(ntry, wf8(:,1), Q(:,75), MB, 1_intkind1, wf8(:,2), n2(4))
  call vert_AZ_Q(gZd,ntry, wf4(:,5), wf4(:,4), wf16(:,2), n3(:,7), t3x16(:,:,2))
  call vert_VQ_A(ntry, wf4(:,2), wf2(:,2), wf8(:,3), n3(:,8), t3x8(:,:,2))
  call prop_A_Q(ntry, wf16(:,2), Q(:,163), MB, 1_intkind1, wf16(:,3), n2(5))
  call vert_AZ_Q(gZd,ntry, ex6, wf4(:,4), wf8(:,4), n3(:,9), t3x8(:,:,3))
  call vert_UV_W(ntry, wf4(:,2), Q(:,20), ex8, Q(:,128), wf8(:,5), n3(:,10), t3x8(:,:,4))
  call prop_A_Q(ntry, wf8(:,4), Q(:,35), MB, 1_intkind1, wf8(:,6), n2(6))
  call vert_QA_V(ntry, wf2(:,2), wf8(:,6), wf16(:,4), n3(:,11), t3x16(:,:,3))
  call vert_VQ_A(ntry, ex8, wf2(:,2), wf4(:,6), n3(:,12), t3x4(:,:,4))
  call prop_Q_A(ntry, wf4(:,6), Q(:,200), MB, 1_intkind1, wf4(:,7), n2(7))
  call vert_AV_Q(ntry, wf8(:,6), wf4(:,2), wf32(:,1), n3(:,13), t3x32(:,:,1))
  call prop_Q_A(ntry, wf8(:,3), Q(:,92), MB, 1_intkind1, wf8(:,7), n2(8))
  call vert_AV_Q(ntry, wf8(:,6), ex8, wf16(:,5), n3(:,14), t3x16(:,:,4))
  call vert_AV_Q(ntry, ex6, wf4(:,2), wf8(:,8), n3(:,15), t3x8(:,:,5))
  call prop_A_Q(ntry, wf8(:,8), Q(:,52), MB, 1_intkind1, wf8(:,9), n2(9))
  call vert_AZ_Q(gZd,ntry, wf8(:,9), wf4(:,4), wf32(:,2), n3(:,16), t3x32(:,:,2))
  call vert_AV_Q(ntry, wf8(:,9), ex8, wf16(:,6), n3(:,17), t3x16(:,:,5))
  call vert_AV_Q(ntry, ex6, wf8(:,5), wf16(:,7), n3(:,18), t3x16(:,:,6))
  call vert_VQ_A(ntry, ex8, ex4, wf4(:,8), n3(:,19), t3x4(:,:,5))
  call vert_SA_Q(gH,ntry, ex7, ex6, wf2(:,3), n3(:,20), t3x2(:,:,2))
  call prop_Q_A(ntry, wf4(:,8), Q(:,136), MB, 1_intkind1, wf4(:,9), n2(10))
  call prop_A_Q(ntry, wf2(:,3), Q(:,96), MB, 1_intkind1, wf2(:,4), n2(11))
  call vert_ZQ_A(gZd,ntry, wf4(:,4), wf4(:,9), wf16(:,8), n3(:,21), t3x16(:,:,7))
  call vert_AV_Q(ntry, wf2(:,4), wf4(:,2), wf8(:,10), n3(:,22), t3x8(:,:,6))
  call prop_Q_A(ntry, wf16(:,8), Q(:,139), MB, 1_intkind1, wf16(:,9), n2(12))
  call vert_AZ_Q(gZd,ntry, wf2(:,4), wf4(:,4), wf8(:,11), n3(:,23), t3x8(:,:,7))
  call vert_VQ_A(ntry, wf4(:,2), wf4(:,9), wf16(:,10), n3(:,24), t3x16(:,:,8))
  call prop_A_Q(ntry, wf8(:,11), Q(:,99), MB, 1_intkind1, wf8(:,12), n2(13))
  call vert_ZQ_A(gZd,ntry, wf4(:,4), ex4, wf8(:,13), n3(:,25), t3x8(:,:,8))
  call prop_Q_A(ntry, wf8(:,13), Q(:,11), MB, 1_intkind1, wf8(:,14), n2(14))
  call vert_QA_V(ntry, wf8(:,14), wf2(:,4), wf16(:,11), n3(:,26), t3x16(:,:,9))
  call prop_A_Q(ntry, wf8(:,10), Q(:,116), MB, 1_intkind1, wf8(:,15), n2(15))
  call vert_VQ_A(ntry, ex8, wf8(:,14), wf16(:,12), n3(:,27), t3x16(:,:,10))
  call vert_AV_Q(ntry, wf2(:,4), ex8, wf4(:,10), n3(:,28), t3x4(:,:,6))
  call prop_A_Q(ntry, wf4(:,10), Q(:,224), MB, 1_intkind1, wf4(:,11), n2(16))
  call vert_VQ_A(ntry, wf4(:,2), wf8(:,14), wf32(:,3), n3(:,29), t3x32(:,:,3))
  call vert_VQ_A(ntry, wf4(:,2), ex4, wf8(:,16), n3(:,30), t3x8(:,:,9))
  call prop_Q_A(ntry, wf8(:,16), Q(:,28), MB, 1_intkind1, wf8(:,17), n2(17))
  call vert_VQ_A(ntry, ex8, wf8(:,17), wf16(:,13), n3(:,31), t3x16(:,:,11))
  call vert_ZQ_A(gZd,ntry, wf4(:,4), wf8(:,17), wf32(:,4), n3(:,32), t3x32(:,:,4))
  call vert_VQ_A(ntry, wf8(:,5), ex4, wf16(:,14), n3(:,33), t3x16(:,:,12))
  call vert_QS_A(gH,ntry, wf4(:,9), ex7, wf4(:,12), n3(:,34), t3x4(:,:,7))
  call prop_Q_A(ntry, wf4(:,12), Q(:,200), MB, 1_intkind1, wf4(:,13), n2(18))
  call prop_Q_A(ntry, wf16(:,10), Q(:,156), MB, 1_intkind1, wf16(:,15), n2(19))
  call vert_SA_Q(gH,ntry, ex7, wf8(:,6), wf8(:,18), n3(:,35), t3x8(:,:,10))
  call vert_SV_V(ntry, ex7, wf4(:,4), wf4(:,14), n3(:,36), t3x4(:,:,8))
  call prop_W_W(ntry, wf4(:,14), Q(:,67), MZ, 1_intkind1, wf4(:,15), n2(20))
  call vert_QA_Z(gZd,ntry, wf4(:,9), wf8(:,9), wf32(:,5), n3(:,37), t3x32(:,:,5))
  call vert_AZ_Q(gZd,ntry, ex6, wf4(:,15), wf8(:,19), n3(:,38), t3x8(:,:,11))
  call vert_SA_Q(gH,ntry, ex7, wf8(:,9), wf8(:,20), n3(:,39), t3x8(:,:,12))
  call prop_A_Q(ntry, wf16(:,1), Q(:,180), MB, 1_intkind1, wf16(:,16), n2(21))
  call vert_QS_A(gH,ntry, wf8(:,14), ex7, wf8(:,21), n3(:,40), t3x8(:,:,13))
  call vert_SA_Q(gH,ntry, ex7, wf4(:,5), wf4(:,16), n3(:,41), t3x4(:,:,9))
  call prop_A_Q(ntry, wf4(:,16), Q(:,224), MB, 1_intkind1, wf4(:,17), n2(22))
  call vert_QA_Z(gZd,ntry, wf8(:,17), wf4(:,5), wf32(:,6), n3(:,42), t3x32(:,:,6))
  call vert_ZQ_A(gZd,ntry, wf4(:,15), ex4, wf8(:,22), n3(:,43), t3x8(:,:,14))
  call vert_QS_A(gH,ntry, wf8(:,17), ex7, wf8(:,23), n3(:,44), t3x8(:,:,15))
  call prop_Q_A(ntry, wf8(:,21), Q(:,75), MB, 1_intkind1, wf8(:,24), n2(23))
  call prop_A_Q(ntry, wf8(:,20), Q(:,116), MB, 1_intkind1, wf8(:,25), n2(24))
  call prop_A_Q(ntry, wf16(:,7), Q(:,180), MB, 1_intkind1, wf16(:,17), n2(25))
  call prop_Q_A(ntry, wf8(:,23), Q(:,92), MB, 1_intkind1, wf8(:,26), n2(26))
  call prop_A_Q(ntry, wf8(:,18), Q(:,99), MB, 1_intkind1, wf8(:,27), n2(27))
  call prop_Q_A(ntry, wf16(:,14), Q(:,156), MB, 1_intkind1, wf16(:,18), n2(28))
  call prop_A_Q(ntry, wf8(:,19), Q(:,99), MB, 1_intkind1, wf8(:,28), n2(29))
  call prop_Q_A(ntry, wf8(:,22), Q(:,75), MB, 1_intkind1, wf8(:,29), n2(30))
  call vert_QS_A(gH,ntry, ex3, ex7, wf2(:,5), n3(:,45), t3x2(:,:,3))
  call vert_QA_V(ntry, ex4, ex5, wf4(:,18), n3(:,46), t3x4(:,:,10))
  call prop_Q_A(ntry, wf2(:,5), Q(:,68), MB, 1_intkind1, wf2(:,6), n2(31))
  call vert_ZQ_A(gZd,ntry, wf4(:,4), wf2(:,6), wf8(:,30), n3(:,47), t3x8(:,:,16))
  call vert_AV_Q(ntry, wf4(:,5), wf4(:,18), wf16(:,19), n3(:,48), t3x16(:,:,13))
  call prop_Q_A(ntry, wf8(:,30), Q(:,71), MB, 1_intkind1, wf8(:,31), n2(32))
  call vert_VQ_A(ntry, wf4(:,18), wf2(:,6), wf8(:,32), n3(:,49), t3x8(:,:,17))
  call vert_VQ_A(ntry, ex8, wf2(:,6), wf4(:,19), n3(:,50), t3x4(:,:,11))
  call prop_Q_A(ntry, wf4(:,19), Q(:,196), MB, 1_intkind1, wf4(:,20), n2(33))
  call vert_AV_Q(ntry, wf8(:,6), wf4(:,18), wf32(:,7), n3(:,51), t3x32(:,:,7))
  call vert_UV_W(ntry, wf4(:,18), Q(:,24), ex8, Q(:,128), wf8(:,33), n3(:,52), t3x8(:,:,18))
  call vert_QA_V(ntry, wf2(:,6), wf8(:,6), wf16(:,20), n3(:,53), t3x16(:,:,14))
  call prop_Q_A(ntry, wf8(:,32), Q(:,92), MB, 1_intkind1, wf8(:,34), n2(34))
  call vert_AV_Q(ntry, ex6, wf4(:,18), wf8(:,35), n3(:,54), t3x8(:,:,19))
  call prop_A_Q(ntry, wf8(:,35), Q(:,56), MB, 1_intkind1, wf8(:,36), n2(35))
  call vert_AZ_Q(gZd,ntry, wf8(:,36), wf4(:,4), wf32(:,8), n3(:,55), t3x32(:,:,8))
  call vert_AV_Q(ntry, wf8(:,36), ex8, wf16(:,21), n3(:,56), t3x16(:,:,15))
  call vert_AV_Q(ntry, ex6, wf8(:,33), wf16(:,22), n3(:,57), t3x16(:,:,16))
  call vert_VQ_A(ntry, ex8, ex3, wf4(:,21), n3(:,58), t3x4(:,:,12))
  call prop_Q_A(ntry, wf4(:,21), Q(:,132), MB, 1_intkind1, wf4(:,22), n2(36))
  call vert_ZQ_A(gZd,ntry, wf4(:,4), wf4(:,22), wf16(:,23), n3(:,59), t3x16(:,:,17))
  call vert_AV_Q(ntry, wf2(:,4), wf4(:,18), wf8(:,37), n3(:,60), t3x8(:,:,20))
  call prop_Q_A(ntry, wf16(:,23), Q(:,135), MB, 1_intkind1, wf16(:,24), n2(37))
  call vert_VQ_A(ntry, wf4(:,18), wf4(:,22), wf16(:,25), n3(:,61), t3x16(:,:,18))
  call vert_ZQ_A(gZd,ntry, wf4(:,4), ex3, wf8(:,38), n3(:,62), t3x8(:,:,21))
  call prop_Q_A(ntry, wf8(:,38), Q(:,7), MB, 1_intkind1, wf8(:,39), n2(38))
  call prop_A_Q(ntry, wf8(:,37), Q(:,120), MB, 1_intkind1, wf8(:,40), n2(39))
  call vert_VQ_A(ntry, ex8, wf8(:,39), wf16(:,26), n3(:,63), t3x16(:,:,19))
  call vert_QA_V(ntry, wf8(:,39), wf2(:,4), wf16(:,27), n3(:,64), t3x16(:,:,20))
  call vert_VQ_A(ntry, wf4(:,18), wf8(:,39), wf32(:,9), n3(:,65), t3x32(:,:,9))
  call vert_VQ_A(ntry, wf4(:,18), ex3, wf8(:,41), n3(:,66), t3x8(:,:,22))
  call prop_Q_A(ntry, wf8(:,41), Q(:,28), MB, 1_intkind1, wf8(:,42), n2(40))
  call vert_VQ_A(ntry, ex8, wf8(:,42), wf16(:,28), n3(:,67), t3x16(:,:,21))
  call vert_ZQ_A(gZd,ntry, wf4(:,4), wf8(:,42), wf32(:,10), n3(:,68), t3x32(:,:,10))
  call vert_VQ_A(ntry, wf8(:,33), ex3, wf16(:,29), n3(:,69), t3x16(:,:,22))
  call vert_QS_A(gH,ntry, wf4(:,22), ex7, wf4(:,23), n3(:,70), t3x4(:,:,13))
  call prop_Q_A(ntry, wf4(:,23), Q(:,196), MB, 1_intkind1, wf4(:,24), n2(41))
  call prop_Q_A(ntry, wf16(:,25), Q(:,156), MB, 1_intkind1, wf16(:,30), n2(42))
  call vert_QA_Z(gZd,ntry, wf4(:,22), wf8(:,36), wf32(:,11), n3(:,71), t3x32(:,:,11))
  call vert_SA_Q(gH,ntry, ex7, wf8(:,36), wf8(:,43), n3(:,72), t3x8(:,:,23))
  call prop_A_Q(ntry, wf16(:,19), Q(:,184), MB, 1_intkind1, wf16(:,31), n2(43))
  call vert_QS_A(gH,ntry, wf8(:,39), ex7, wf8(:,44), n3(:,73), t3x8(:,:,24))
  call vert_QA_Z(gZd,ntry, wf8(:,42), wf4(:,5), wf32(:,12), n3(:,74), t3x32(:,:,12))
  call vert_ZQ_A(gZd,ntry, wf4(:,15), ex3, wf8(:,45), n3(:,75), t3x8(:,:,25))
  call vert_QS_A(gH,ntry, wf8(:,42), ex7, wf8(:,46), n3(:,76), t3x8(:,:,26))
  call prop_Q_A(ntry, wf8(:,44), Q(:,71), MB, 1_intkind1, wf8(:,47), n2(44))
  call prop_A_Q(ntry, wf16(:,22), Q(:,184), MB, 1_intkind1, wf16(:,32), n2(45))
  call prop_A_Q(ntry, wf8(:,43), Q(:,120), MB, 1_intkind1, wf8(:,48), n2(46))
  call prop_Q_A(ntry, wf8(:,46), Q(:,92), MB, 1_intkind1, wf8(:,49), n2(47))
  call prop_Q_A(ntry, wf16(:,29), Q(:,156), MB, 1_intkind1, wf16(:,33), n2(48))
  call prop_Q_A(ntry, wf8(:,45), Q(:,71), MB, 1_intkind1, wf8(:,50), n2(49))
  call vert_QA_V(ntry, ex3, ex6, wf4(:,25), n3(:,77), t3x4(:,:,14))
  call vert_AV_Q(ntry, ex5, ex8, wf4(:,26), n3(:,78), t3x4(:,:,15))
  call prop_A_Q(ntry, wf4(:,26), Q(:,144), MB, 1_intkind1, wf4(:,27), n2(50))
  call vert_AV_Q(ntry, wf4(:,27), wf4(:,25), wf16(:,34), n3(:,79), t3x16(:,:,23))
  call vert_AZ_Q(gZd,ntry, wf4(:,27), wf4(:,4), wf16(:,35), n3(:,80), t3x16(:,:,24))
  call vert_VQ_A(ntry, wf4(:,25), wf2(:,2), wf8(:,51), n3(:,81), t3x8(:,:,27))
  call prop_A_Q(ntry, wf16(:,35), Q(:,147), MB, 1_intkind1, wf16(:,36), n2(51))
  call vert_AZ_Q(gZd,ntry, ex5, wf4(:,4), wf8(:,52), n3(:,82), t3x8(:,:,28))
  call vert_UV_W(ntry, wf4(:,25), Q(:,36), ex8, Q(:,128), wf8(:,53), n3(:,83), t3x8(:,:,29))
  call prop_A_Q(ntry, wf8(:,52), Q(:,19), MB, 1_intkind1, wf8(:,54), n2(52))
  call vert_QA_V(ntry, wf2(:,2), wf8(:,54), wf16(:,37), n3(:,84), t3x16(:,:,25))
  call vert_AV_Q(ntry, wf8(:,54), wf4(:,25), wf32(:,13), n3(:,85), t3x32(:,:,13))
  call prop_Q_A(ntry, wf8(:,51), Q(:,108), MB, 1_intkind1, wf8(:,55), n2(53))
  call vert_AV_Q(ntry, wf8(:,54), ex8, wf16(:,38), n3(:,86), t3x16(:,:,26))
  call vert_AV_Q(ntry, ex5, wf4(:,25), wf8(:,56), n3(:,87), t3x8(:,:,30))
  call prop_A_Q(ntry, wf8(:,56), Q(:,52), MB, 1_intkind1, wf8(:,57), n2(54))
  call vert_AZ_Q(gZd,ntry, wf8(:,57), wf4(:,4), wf32(:,14), n3(:,88), t3x32(:,:,14))
  call vert_AV_Q(ntry, wf8(:,57), ex8, wf16(:,39), n3(:,89), t3x16(:,:,27))
  call vert_AV_Q(ntry, ex5, wf8(:,53), wf16(:,40), n3(:,90), t3x16(:,:,28))
  call vert_SA_Q(gH,ntry, ex7, ex5, wf2(:,7), n3(:,91), t3x2(:,:,4))
  call prop_A_Q(ntry, wf2(:,7), Q(:,80), MB, 1_intkind1, wf2(:,8), n2(55))
  call vert_AV_Q(ntry, wf2(:,8), wf4(:,25), wf8(:,58), n3(:,92), t3x8(:,:,31))
  call vert_AZ_Q(gZd,ntry, wf2(:,8), wf4(:,4), wf8(:,59), n3(:,93), t3x8(:,:,32))
  call vert_VQ_A(ntry, wf4(:,25), wf4(:,9), wf16(:,41), n3(:,94), t3x16(:,:,29))
  call prop_A_Q(ntry, wf8(:,59), Q(:,83), MB, 1_intkind1, wf8(:,60), n2(56))
  call vert_QA_V(ntry, wf8(:,14), wf2(:,8), wf16(:,42), n3(:,95), t3x16(:,:,30))
  call prop_A_Q(ntry, wf8(:,58), Q(:,116), MB, 1_intkind1, wf8(:,61), n2(57))
  call vert_AV_Q(ntry, wf2(:,8), ex8, wf4(:,28), n3(:,96), t3x4(:,:,16))
  call prop_A_Q(ntry, wf4(:,28), Q(:,208), MB, 1_intkind1, wf4(:,29), n2(58))
  call vert_VQ_A(ntry, wf4(:,25), wf8(:,14), wf32(:,15), n3(:,97), t3x32(:,:,15))
  call vert_VQ_A(ntry, wf4(:,25), ex4, wf8(:,62), n3(:,98), t3x8(:,:,33))
  call prop_Q_A(ntry, wf8(:,62), Q(:,44), MB, 1_intkind1, wf8(:,63), n2(59))
  call vert_VQ_A(ntry, ex8, wf8(:,63), wf16(:,43), n3(:,99), t3x16(:,:,31))
  call vert_ZQ_A(gZd,ntry, wf4(:,4), wf8(:,63), wf32(:,16), n3(:,100), t3x32(:,:,16))
  call vert_VQ_A(ntry, wf8(:,53), ex4, wf16(:,44), n3(:,101), t3x16(:,:,32))
  call prop_Q_A(ntry, wf16(:,41), Q(:,172), MB, 1_intkind1, wf16(:,45), n2(60))
  call vert_SA_Q(gH,ntry, ex7, wf8(:,54), wf8(:,64), n3(:,102), t3x8(:,:,34))
  call vert_QA_Z(gZd,ntry, wf4(:,9), wf8(:,57), wf32(:,17), n3(:,103), t3x32(:,:,17))
  call vert_AZ_Q(gZd,ntry, ex5, wf4(:,15), wf8(:,65), n3(:,104), t3x8(:,:,35))
  call vert_SA_Q(gH,ntry, ex7, wf8(:,57), wf8(:,66), n3(:,105), t3x8(:,:,36))
  call prop_A_Q(ntry, wf16(:,34), Q(:,180), MB, 1_intkind1, wf16(:,46), n2(61))
  call vert_SA_Q(gH,ntry, ex7, wf4(:,27), wf4(:,30), n3(:,106), t3x4(:,:,17))
  call prop_A_Q(ntry, wf4(:,30), Q(:,208), MB, 1_intkind1, wf4(:,31), n2(62))
  call vert_QA_Z(gZd,ntry, wf8(:,63), wf4(:,27), wf32(:,18), n3(:,107), t3x32(:,:,18))
  call vert_QS_A(gH,ntry, wf8(:,63), ex7, wf8(:,67), n3(:,108), t3x8(:,:,37))
  call prop_A_Q(ntry, wf8(:,66), Q(:,116), MB, 1_intkind1, wf8(:,68), n2(63))
  call prop_A_Q(ntry, wf16(:,40), Q(:,180), MB, 1_intkind1, wf16(:,47), n2(64))
  call prop_Q_A(ntry, wf8(:,67), Q(:,108), MB, 1_intkind1, wf8(:,69), n2(65))
  call prop_A_Q(ntry, wf8(:,64), Q(:,83), MB, 1_intkind1, wf8(:,70), n2(66))
  call prop_Q_A(ntry, wf16(:,44), Q(:,172), MB, 1_intkind1, wf16(:,48), n2(67))
  call prop_A_Q(ntry, wf8(:,65), Q(:,83), MB, 1_intkind1, wf8(:,71), n2(68))
  call vert_QA_V(ntry, ex4, ex6, wf4(:,32), n3(:,109), t3x4(:,:,18))
  call vert_AV_Q(ntry, wf4(:,27), wf4(:,32), wf16(:,49), n3(:,110), t3x16(:,:,33))
  call vert_VQ_A(ntry, wf4(:,32), wf2(:,6), wf8(:,72), n3(:,111), t3x8(:,:,38))
  call vert_AV_Q(ntry, wf8(:,54), wf4(:,32), wf32(:,19), n3(:,112), t3x32(:,:,19))
  call vert_UV_W(ntry, wf4(:,32), Q(:,40), ex8, Q(:,128), wf8(:,73), n3(:,113), t3x8(:,:,39))
  call vert_QA_V(ntry, wf2(:,6), wf8(:,54), wf16(:,50), n3(:,114), t3x16(:,:,34))
  call prop_Q_A(ntry, wf8(:,72), Q(:,108), MB, 1_intkind1, wf8(:,74), n2(69))
  call vert_AV_Q(ntry, ex5, wf4(:,32), wf8(:,75), n3(:,115), t3x8(:,:,40))
  call prop_A_Q(ntry, wf8(:,75), Q(:,56), MB, 1_intkind1, wf8(:,76), n2(70))
  call vert_AZ_Q(gZd,ntry, wf8(:,76), wf4(:,4), wf32(:,20), n3(:,116), t3x32(:,:,20))
  call vert_AV_Q(ntry, wf8(:,76), ex8, wf16(:,51), n3(:,117), t3x16(:,:,35))
  call vert_AV_Q(ntry, ex5, wf8(:,73), wf16(:,52), n3(:,118), t3x16(:,:,36))
  call vert_AV_Q(ntry, wf2(:,8), wf4(:,32), wf8(:,77), n3(:,119), t3x8(:,:,41))
  call vert_VQ_A(ntry, wf4(:,32), wf4(:,22), wf16(:,53), n3(:,120), t3x16(:,:,37))
  call prop_A_Q(ntry, wf8(:,77), Q(:,120), MB, 1_intkind1, wf8(:,78), n2(71))
  call vert_QA_V(ntry, wf8(:,39), wf2(:,8), wf16(:,54), n3(:,121), t3x16(:,:,38))
  call vert_VQ_A(ntry, wf4(:,32), wf8(:,39), wf32(:,21), n3(:,122), t3x32(:,:,21))
  call vert_VQ_A(ntry, wf4(:,32), ex3, wf8(:,79), n3(:,123), t3x8(:,:,42))
  call prop_Q_A(ntry, wf8(:,79), Q(:,44), MB, 1_intkind1, wf8(:,80), n2(72))
  call vert_VQ_A(ntry, ex8, wf8(:,80), wf16(:,55), n3(:,124), t3x16(:,:,39))
  call vert_ZQ_A(gZd,ntry, wf4(:,4), wf8(:,80), wf32(:,22), n3(:,125), t3x32(:,:,22))
  call vert_VQ_A(ntry, wf8(:,73), ex3, wf16(:,56), n3(:,126), t3x16(:,:,40))
  call prop_Q_A(ntry, wf16(:,53), Q(:,172), MB, 1_intkind1, wf16(:,57), n2(73))
  call vert_QA_Z(gZd,ntry, wf4(:,22), wf8(:,76), wf32(:,23), n3(:,127), t3x32(:,:,23))
  call vert_SA_Q(gH,ntry, ex7, wf8(:,76), wf8(:,81), n3(:,128), t3x8(:,:,43))
  call prop_A_Q(ntry, wf16(:,49), Q(:,184), MB, 1_intkind1, wf16(:,58), n2(74))
  call vert_QA_Z(gZd,ntry, wf8(:,80), wf4(:,27), wf32(:,24), n3(:,129), t3x32(:,:,24))
  call vert_QS_A(gH,ntry, wf8(:,80), ex7, wf8(:,82), n3(:,130), t3x8(:,:,44))
  call prop_A_Q(ntry, wf16(:,52), Q(:,184), MB, 1_intkind1, wf16(:,59), n2(75))
  call prop_A_Q(ntry, wf8(:,81), Q(:,120), MB, 1_intkind1, wf8(:,83), n2(76))
  call prop_Q_A(ntry, wf8(:,82), Q(:,108), MB, 1_intkind1, wf8(:,84), n2(77))
  call prop_Q_A(ntry, wf16(:,56), Q(:,172), MB, 1_intkind1, wf16(:,60), n2(78))
  call vert_QA_V(ntry, wf2(:,6), ex6, wf4(:,33), n3(:,131), t3x4(:,:,19))
  call vert_QA_V(ntry, wf4(:,9), wf8(:,54), wf32(:,25), n3(:,132), t3x32(:,:,25))
  call vert_QA_V(ntry, wf4(:,9), ex6, wf8(:,85), n3(:,133), t3x8(:,:,45))
  call vert_QA_V(ntry, wf2(:,6), ex5, wf4(:,34), n3(:,134), t3x4(:,:,20))
  call vert_VQ_A(ntry, wf4(:,34), wf4(:,9), wf16(:,61), n3(:,135), t3x16(:,:,41))
  call vert_QA_V(ntry, wf4(:,9), ex5, wf8(:,86), n3(:,136), t3x8(:,:,46))
  call vert_VQ_A(ntry, wf8(:,86), wf2(:,6), wf16(:,62), n3(:,137), t3x16(:,:,42))
  call vert_AV_Q(ntry, ex6, wf4(:,34), wf8(:,87), n3(:,138), t3x8(:,:,47))
  call vert_AV_Q(ntry, ex5, wf4(:,33), wf8(:,88), n3(:,139), t3x8(:,:,48))
  call vert_AV_Q(ntry, ex6, wf8(:,86), wf16(:,63), n3(:,140), t3x16(:,:,43))
  call vert_AV_Q(ntry, ex5, wf8(:,85), wf16(:,64), n3(:,141), t3x16(:,:,44))
  call vert_QA_V(ntry, wf8(:,14), wf4(:,27), wf32(:,26), n3(:,142), t3x32(:,:,26))
  call vert_QA_V(ntry, wf2(:,6), wf4(:,27), wf8(:,89), n3(:,143), t3x8(:,:,49))
  call vert_QA_V(ntry, wf8(:,14), ex6, wf16(:,65), n3(:,144), t3x16(:,:,45))
  call vert_QA_V(ntry, ex4, wf4(:,27), wf8(:,90), n3(:,145), t3x8(:,:,50))
  call vert_VQ_A(ntry, wf8(:,90), wf2(:,6), wf16(:,66), n3(:,146), t3x16(:,:,46))
  call vert_QA_V(ntry, ex4, wf8(:,6), wf16(:,67), n3(:,147), t3x16(:,:,47))
  call vert_VQ_A(ntry, wf4(:,33), ex4, wf8(:,91), n3(:,148), t3x8(:,:,51))
  call vert_AV_Q(ntry, ex6, wf8(:,90), wf16(:,68), n3(:,149), t3x16(:,:,48))
  call vert_VQ_A(ntry, wf4(:,34), wf8(:,14), wf32(:,27), n3(:,150), t3x32(:,:,27))
  call vert_QA_V(ntry, wf2(:,6), wf4(:,5), wf8(:,92), n3(:,151), t3x8(:,:,52))
  call vert_QA_V(ntry, wf8(:,14), ex5, wf16(:,69), n3(:,152), t3x16(:,:,49))
  call vert_QA_V(ntry, ex4, wf8(:,54), wf16(:,70), n3(:,153), t3x16(:,:,50))
  call vert_QA_V(ntry, ex4, wf4(:,5), wf8(:,93), n3(:,154), t3x8(:,:,53))
  call vert_VQ_A(ntry, wf8(:,93), wf2(:,6), wf16(:,71), n3(:,155), t3x16(:,:,51))
  call vert_VQ_A(ntry, wf4(:,34), ex4, wf8(:,94), n3(:,156), t3x8(:,:,54))
  call vert_AV_Q(ntry, ex5, wf8(:,93), wf16(:,72), n3(:,157), t3x16(:,:,52))
  call vert_UV_W(ntry, wf4(:,34), Q(:,84), ex8, Q(:,128), wf8(:,95), n3(:,158), t3x8(:,:,55))
  call prop_A_Q(ntry, wf8(:,87), Q(:,116), MB, 1_intkind1, wf8(:,96), n2(79))
  call vert_UV_W(ntry, wf4(:,33), Q(:,100), ex8, Q(:,128), wf8(:,97), n3(:,159), t3x8(:,:,56))
  call prop_A_Q(ntry, wf8(:,88), Q(:,116), MB, 1_intkind1, wf8(:,98), n2(80))
  call vert_QA_V(ntry, wf4(:,20), ex6, wf8(:,99), n3(:,160), t3x8(:,:,57))
  call vert_QA_V(ntry, wf4(:,20), ex5, wf8(:,100), n3(:,161), t3x8(:,:,58))
  call prop_Q_A(ntry, wf8(:,91), Q(:,108), MB, 1_intkind1, wf8(:,101), n2(81))
  call prop_Q_A(ntry, wf8(:,94), Q(:,92), MB, 1_intkind1, wf8(:,102), n2(82))
  call vert_QA_V(ntry, wf4(:,22), ex6, wf8(:,103), n3(:,162), t3x8(:,:,59))
  call vert_QA_V(ntry, wf2(:,2), ex6, wf4(:,35), n3(:,163), t3x4(:,:,21))
  call vert_QA_V(ntry, wf4(:,22), wf8(:,54), wf32(:,28), n3(:,164), t3x32(:,:,28))
  call vert_QA_V(ntry, wf4(:,22), ex5, wf8(:,104), n3(:,165), t3x8(:,:,60))
  call vert_VQ_A(ntry, wf8(:,104), wf2(:,2), wf16(:,73), n3(:,166), t3x16(:,:,53))
  call vert_QA_V(ntry, wf2(:,2), ex5, wf4(:,36), n3(:,167), t3x4(:,:,22))
  call vert_VQ_A(ntry, wf4(:,36), wf4(:,22), wf16(:,74), n3(:,168), t3x16(:,:,54))
  call vert_AV_Q(ntry, ex6, wf8(:,104), wf16(:,75), n3(:,169), t3x16(:,:,55))
  call vert_AV_Q(ntry, ex5, wf8(:,103), wf16(:,76), n3(:,170), t3x16(:,:,56))
  call vert_AV_Q(ntry, ex6, wf4(:,36), wf8(:,105), n3(:,171), t3x8(:,:,61))
  call vert_AV_Q(ntry, ex5, wf4(:,35), wf8(:,106), n3(:,172), t3x8(:,:,62))
  call vert_QA_V(ntry, wf2(:,2), wf4(:,27), wf8(:,107), n3(:,173), t3x8(:,:,63))
  call vert_QA_V(ntry, wf8(:,39), ex6, wf16(:,77), n3(:,174), t3x16(:,:,57))
  call vert_QA_V(ntry, wf8(:,39), wf4(:,27), wf32(:,29), n3(:,175), t3x32(:,:,29))
  call vert_QA_V(ntry, ex3, wf4(:,27), wf8(:,108), n3(:,176), t3x8(:,:,64))
  call vert_VQ_A(ntry, wf8(:,108), wf2(:,2), wf16(:,78), n3(:,177), t3x16(:,:,58))
  call vert_QA_V(ntry, ex3, wf8(:,6), wf16(:,79), n3(:,178), t3x16(:,:,59))
  call vert_AV_Q(ntry, ex6, wf8(:,108), wf16(:,80), n3(:,179), t3x16(:,:,60))
  call vert_VQ_A(ntry, wf4(:,35), ex3, wf8(:,109), n3(:,180), t3x8(:,:,65))
  call vert_QA_V(ntry, wf2(:,2), wf4(:,5), wf8(:,110), n3(:,181), t3x8(:,:,66))
  call vert_QA_V(ntry, wf8(:,39), ex5, wf16(:,81), n3(:,182), t3x16(:,:,61))
  call vert_VQ_A(ntry, wf4(:,36), wf8(:,39), wf32(:,30), n3(:,183), t3x32(:,:,30))
  call vert_QA_V(ntry, ex3, wf8(:,54), wf16(:,82), n3(:,184), t3x16(:,:,62))
  call vert_QA_V(ntry, ex3, wf4(:,5), wf8(:,111), n3(:,185), t3x8(:,:,67))
  call vert_VQ_A(ntry, wf8(:,111), wf2(:,2), wf16(:,83), n3(:,186), t3x16(:,:,63))
  call vert_AV_Q(ntry, ex5, wf8(:,111), wf16(:,84), n3(:,187), t3x16(:,:,64))
  call vert_VQ_A(ntry, wf4(:,36), ex3, wf8(:,112), n3(:,188), t3x8(:,:,68))
  call vert_UV_W(ntry, wf4(:,35), Q(:,104), ex8, Q(:,128), wf8(:,113), n3(:,189), t3x8(:,:,69))
  call vert_QA_V(ntry, wf4(:,7), ex6, wf8(:,114), n3(:,190), t3x8(:,:,70))
  call vert_UV_W(ntry, wf4(:,36), Q(:,88), ex8, Q(:,128), wf8(:,115), n3(:,191), t3x8(:,:,71))
  call vert_QA_V(ntry, wf4(:,7), ex5, wf8(:,116), n3(:,192), t3x8(:,:,72))
  call prop_A_Q(ntry, wf8(:,105), Q(:,120), MB, 1_intkind1, wf8(:,117), n2(83))
  call prop_A_Q(ntry, wf8(:,106), Q(:,120), MB, 1_intkind1, wf8(:,118), n2(84))
  call prop_Q_A(ntry, wf8(:,109), Q(:,108), MB, 1_intkind1, wf8(:,119), n2(85))
  call prop_Q_A(ntry, wf8(:,112), Q(:,92), MB, 1_intkind1, wf8(:,120), n2(86))
  call vert_QA_V(ntry, wf4(:,22), wf2(:,8), wf8(:,121), n3(:,193), t3x8(:,:,73))
  call vert_QA_V(ntry, ex4, wf2(:,8), wf4(:,37), n3(:,194), t3x4(:,:,23))
  call vert_VQ_A(ntry, wf4(:,37), wf4(:,22), wf16(:,85), n3(:,195), t3x16(:,:,65))
  call vert_VQ_A(ntry, wf8(:,103), ex4, wf16(:,86), n3(:,196), t3x16(:,:,66))
  call vert_AV_Q(ntry, ex6, wf4(:,37), wf8(:,122), n3(:,197), t3x8(:,:,74))
  call vert_QA_V(ntry, wf4(:,9), wf2(:,8), wf8(:,123), n3(:,198), t3x8(:,:,75))
  call vert_QA_V(ntry, ex3, wf2(:,8), wf4(:,38), n3(:,199), t3x4(:,:,24))
  call vert_VQ_A(ntry, wf4(:,38), wf4(:,9), wf16(:,87), n3(:,200), t3x16(:,:,67))
  call vert_AV_Q(ntry, ex6, wf4(:,38), wf8(:,124), n3(:,201), t3x8(:,:,76))
  call vert_VQ_A(ntry, wf8(:,85), ex3, wf16(:,88), n3(:,202), t3x16(:,:,68))
  call vert_VQ_A(ntry, wf4(:,37), wf8(:,39), wf32(:,31), n3(:,203), t3x32(:,:,31))
  call vert_VQ_A(ntry, wf8(:,93), wf8(:,39), wf64(:,1), n3(:,204), t3x64(:,:,1))
  call vert_VQ_A(ntry, wf4(:,38), wf8(:,14), wf32(:,32), n3(:,205), t3x32(:,:,32))
  call vert_VQ_A(ntry, wf8(:,111), wf8(:,14), wf64(:,2), n3(:,206), t3x64(:,:,2))
  call vert_VQ_A(ntry, wf4(:,38), ex4, wf8(:,125), n3(:,207), t3x8(:,:,77))
  call vert_VQ_A(ntry, wf8(:,111), ex4, wf16(:,89), n3(:,208), t3x16(:,:,69))
  call vert_VQ_A(ntry, wf4(:,37), ex3, wf8(:,126), n3(:,209), t3x8(:,:,78))
  call vert_VQ_A(ntry, wf8(:,93), ex3, wf16(:,90), n3(:,210), t3x16(:,:,70))
  call vert_UV_W(ntry, wf4(:,37), Q(:,88), ex8, Q(:,128), wf8(:,127), n3(:,211), t3x8(:,:,79))
  call vert_QA_V(ntry, ex4, wf4(:,29), wf8(:,128), n3(:,212), t3x8(:,:,80))
  call prop_A_Q(ntry, wf8(:,122), Q(:,120), MB, 1_intkind1, wf8(:,129), n2(87))
  call prop_A_Q(ntry, wf8(:,124), Q(:,116), MB, 1_intkind1, wf8(:,130), n2(88))
  call vert_UV_W(ntry, wf4(:,38), Q(:,84), ex8, Q(:,128), wf8(:,131), n3(:,213), t3x8(:,:,81))
  call vert_QA_V(ntry, ex3, wf4(:,29), wf8(:,132), n3(:,214), t3x8(:,:,82))
  call prop_Q_A(ntry, wf8(:,125), Q(:,92), MB, 1_intkind1, wf8(:,133), n2(89))
  call prop_Q_A(ntry, wf8(:,126), Q(:,92), MB, 1_intkind1, wf8(:,134), n2(90))
  call vert_VQ_A(ntry, wf8(:,104), wf8(:,14), wf64(:,3), n3(:,215), t3x64(:,:,3))
  call vert_QA_V(ntry, wf4(:,22), wf2(:,4), wf8(:,135), n3(:,216), t3x8(:,:,83))
  call vert_QA_V(ntry, ex4, wf2(:,4), wf4(:,39), n3(:,217), t3x4(:,:,25))
  call vert_VQ_A(ntry, wf4(:,39), wf4(:,22), wf16(:,91), n3(:,218), t3x16(:,:,71))
  call vert_VQ_A(ntry, wf8(:,104), ex4, wf16(:,92), n3(:,219), t3x16(:,:,72))
  call vert_AV_Q(ntry, ex5, wf4(:,39), wf8(:,136), n3(:,220), t3x8(:,:,84))
  call vert_QA_V(ntry, wf4(:,9), wf2(:,4), wf8(:,137), n3(:,221), t3x8(:,:,85))
  call vert_VQ_A(ntry, wf8(:,86), wf8(:,39), wf64(:,4), n3(:,222), t3x64(:,:,4))
  call vert_QA_V(ntry, ex3, wf2(:,4), wf4(:,40), n3(:,223), t3x4(:,:,26))
  call vert_VQ_A(ntry, wf4(:,40), wf4(:,9), wf16(:,93), n3(:,224), t3x16(:,:,73))
  call vert_AV_Q(ntry, ex5, wf4(:,40), wf8(:,138), n3(:,225), t3x8(:,:,86))
  call vert_VQ_A(ntry, wf8(:,86), ex3, wf16(:,94), n3(:,226), t3x16(:,:,74))
  call vert_VQ_A(ntry, wf8(:,90), wf8(:,39), wf64(:,5), n3(:,227), t3x64(:,:,5))
  call vert_VQ_A(ntry, wf4(:,39), wf8(:,39), wf32(:,33), n3(:,228), t3x32(:,:,33))
  call vert_VQ_A(ntry, wf8(:,108), wf8(:,14), wf64(:,6), n3(:,229), t3x64(:,:,6))
  call vert_VQ_A(ntry, wf4(:,40), wf8(:,14), wf32(:,34), n3(:,230), t3x32(:,:,34))
  call vert_VQ_A(ntry, wf8(:,108), ex4, wf16(:,95), n3(:,231), t3x16(:,:,75))
  call vert_VQ_A(ntry, wf4(:,40), ex4, wf8(:,139), n3(:,232), t3x8(:,:,87))
  call vert_VQ_A(ntry, wf8(:,90), ex3, wf16(:,96), n3(:,233), t3x16(:,:,76))
  call vert_VQ_A(ntry, wf4(:,39), ex3, wf8(:,140), n3(:,234), t3x8(:,:,88))
  call vert_QA_V(ntry, ex4, wf4(:,11), wf8(:,141), n3(:,235), t3x8(:,:,89))
  call vert_UV_W(ntry, wf4(:,39), Q(:,104), ex8, Q(:,128), wf8(:,142), n3(:,236), t3x8(:,:,90))
  call prop_A_Q(ntry, wf8(:,136), Q(:,120), MB, 1_intkind1, wf8(:,143), n2(91))
  call prop_A_Q(ntry, wf8(:,138), Q(:,116), MB, 1_intkind1, wf8(:,144), n2(92))
  call vert_QA_V(ntry, ex3, wf4(:,11), wf8(:,145), n3(:,237), t3x8(:,:,91))
  call vert_UV_W(ntry, wf4(:,40), Q(:,100), ex8, Q(:,128), wf8(:,146), n3(:,238), t3x8(:,:,92))
  call prop_Q_A(ntry, wf8(:,139), Q(:,108), MB, 1_intkind1, wf8(:,147), n2(93))
  call prop_Q_A(ntry, wf8(:,140), Q(:,108), MB, 1_intkind1, wf8(:,148), n2(94))
  call prop_A_Q(ntry, wf16(:,75), Q(:,180), MB, 1_intkind1, wf16(:,97), n2(95))
  call prop_A_Q(ntry, wf16(:,76), Q(:,180), MB, 1_intkind1, wf16(:,98), n2(96))
  call vert_QA_V(ntry, wf4(:,24), ex6, wf8(:,149), n3(:,239), t3x8(:,:,93))
  call vert_QA_V(ntry, wf4(:,24), ex5, wf8(:,150), n3(:,240), t3x8(:,:,94))
  call prop_Q_A(ntry, wf16(:,86), Q(:,172), MB, 1_intkind1, wf16(:,99), n2(97))
  call prop_Q_A(ntry, wf16(:,92), Q(:,156), MB, 1_intkind1, wf16(:,100), n2(98))
  call vert_QA_V(ntry, wf4(:,13), ex6, wf8(:,151), n3(:,241), t3x8(:,:,95))
  call vert_QA_V(ntry, wf4(:,13), ex5, wf8(:,152), n3(:,242), t3x8(:,:,96))
  call prop_A_Q(ntry, wf16(:,63), Q(:,184), MB, 1_intkind1, wf16(:,101), n2(99))
  call prop_A_Q(ntry, wf16(:,64), Q(:,184), MB, 1_intkind1, wf16(:,102), n2(100))
  call prop_Q_A(ntry, wf16(:,88), Q(:,172), MB, 1_intkind1, wf16(:,103), n2(101))
  call prop_Q_A(ntry, wf16(:,94), Q(:,156), MB, 1_intkind1, wf16(:,104), n2(102))
  call vert_QA_V(ntry, ex4, wf4(:,31), wf8(:,153), n3(:,243), t3x8(:,:,97))
  call prop_A_Q(ntry, wf16(:,68), Q(:,184), MB, 1_intkind1, wf16(:,105), n2(103))
  call prop_A_Q(ntry, wf16(:,80), Q(:,180), MB, 1_intkind1, wf16(:,106), n2(104))
  call vert_QA_V(ntry, ex3, wf4(:,31), wf8(:,154), n3(:,244), t3x8(:,:,98))
  call prop_Q_A(ntry, wf16(:,95), Q(:,156), MB, 1_intkind1, wf16(:,107), n2(105))
  call prop_Q_A(ntry, wf16(:,96), Q(:,156), MB, 1_intkind1, wf16(:,108), n2(106))
  call vert_QA_V(ntry, ex4, wf4(:,17), wf8(:,155), n3(:,245), t3x8(:,:,99))
  call prop_A_Q(ntry, wf16(:,72), Q(:,184), MB, 1_intkind1, wf16(:,109), n2(107))
  call prop_A_Q(ntry, wf16(:,84), Q(:,180), MB, 1_intkind1, wf16(:,110), n2(108))
  call vert_QA_V(ntry, ex3, wf4(:,17), wf8(:,156), n3(:,246), t3x8(:,:,100))
  call prop_Q_A(ntry, wf16(:,89), Q(:,172), MB, 1_intkind1, wf16(:,111), n2(109))
  call prop_Q_A(ntry, wf16(:,90), Q(:,172), MB, 1_intkind1, wf16(:,112), n2(110))


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
  M2add = M2 / average_factor_pphlljj_nenexbbbxbxhg_1

  do k = 0, 38-1
    M2(k) = M2add(extcomb_perm_pphlljj_nenexbbbxbxhg_1(k))
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

    call cont_QA(nsync, wf16(:,1), wf8(:,2), A(:,1), n3(:,247), t3x128(:,:,1), nhel, den(9))
    call cont_QA(nsync, wf8(:,3), wf16(:,3), A(:,2), n3(:,248), t3x128(:,:,2), nhel, den(14))
    call cont_VV(nsync, wf8(:,5), wf16(:,4), A(:,3), n3(:,249), t3x128(:,:,3), nhel, den(20))
    call cont_QA(nsync, wf4(:,7), wf32(:,1), A(:,4), n3(:,250), t3x128(:,:,4), nhel, den(24))
    call cont_QA(nsync, wf8(:,7), wf16(:,5), A(:,5), n3(:,251), t3x128(:,:,5), nhel, den(27))
    call cont_QA(nsync, wf4(:,7), wf32(:,2), A(:,6), n3(:,252), t3x128(:,:,6), nhel, den(31))
    call cont_QA(nsync, wf8(:,2), wf16(:,6), A(:,7), n3(:,253), t3x128(:,:,7), nhel, den(32))
    call cont_QA(nsync, wf8(:,2), wf16(:,7), A(:,8), n3(:,254), t3x128(:,:,8), nhel, den(33))
    call cont_QA(nsync, wf8(:,10), wf16(:,9), A(:,9), n3(:,255), t3x128(:,:,9), nhel, den(40))
    call cont_QA(nsync, wf16(:,10), wf8(:,12), A(:,10), n3(:,256), t3x128(:,:,10), nhel, den(45))
    call cont_VV(nsync, wf8(:,5), wf16(:,11), A(:,11), n3(:,257), t3x128(:,:,11), nhel, den(49))
    call cont_QA(nsync, wf8(:,15), wf16(:,12), A(:,12), n3(:,258), t3x128(:,:,12), nhel, den(52))
    call cont_QA(nsync, wf4(:,11), wf32(:,3), A(:,13), n3(:,259), t3x128(:,:,13), nhel, den(56))
    call cont_QA(nsync, wf8(:,12), wf16(:,13), A(:,14), n3(:,260), t3x128(:,:,14), nhel, den(59))
    call cont_QA(nsync, wf4(:,11), wf32(:,4), A(:,15), n3(:,261), t3x128(:,:,15), nhel, den(61))
    call cont_QA(nsync, wf8(:,12), wf16(:,14), A(:,16), n3(:,262), t3x128(:,:,16), nhel, den(62))
    call cont_QA(nsync, wf32(:,1), wf4(:,13), A(:,17), n3(:,263), t3x128(:,:,17), nhel, den(64))
    call cont_QA(nsync, wf16(:,15), wf8(:,18), A(:,18), n3(:,264), t3x128(:,:,18), nhel, den(67))
    call cont_VV(nsync, wf4(:,15), wf32(:,5), A(:,19), n3(:,265), t3x128(:,:,19), nhel, den(71))
    call cont_QA(nsync, wf16(:,15), wf8(:,19), A(:,20), n3(:,266), t3x128(:,:,20), nhel, den(72))
    call cont_QA(nsync, wf32(:,2), wf4(:,13), A(:,21), n3(:,267), t3x128(:,:,21), nhel, den(73))
    call cont_QA(nsync, wf16(:,9), wf8(:,20), A(:,22), n3(:,268), t3x128(:,:,22), nhel, den(74))
    call cont_QA(nsync, wf16(:,16), wf8(:,21), A(:,23), n3(:,269), t3x128(:,:,23), nhel, den(77))
    call cont_QA(nsync, wf32(:,3), wf4(:,17), A(:,24), n3(:,270), t3x128(:,:,24), nhel, den(79))
    call cont_VV(nsync, wf4(:,15), wf32(:,6), A(:,25), n3(:,271), t3x128(:,:,25), nhel, den(81))
    call cont_QA(nsync, wf16(:,16), wf8(:,22), A(:,26), n3(:,272), t3x128(:,:,26), nhel, den(82))
    call cont_QA(nsync, wf16(:,3), wf8(:,23), A(:,27), n3(:,273), t3x128(:,:,27), nhel, den(83))
    call cont_QA(nsync, wf32(:,4), wf4(:,17), A(:,28), n3(:,274), t3x128(:,:,28), nhel, den(84))
    call cont_QA(nsync, wf16(:,6), wf8(:,24), A(:,29), n3(:,275), t3x128(:,:,29), nhel, den(86))
    call cont_QA(nsync, wf16(:,12), wf8(:,25), A(:,30), n3(:,276), t3x128(:,:,30), nhel, den(88))
    call cont_QA(nsync, wf8(:,21), wf16(:,17), A(:,31), n3(:,277), t3x128(:,:,31), nhel, den(90))
    call cont_QA(nsync, wf16(:,5), wf8(:,26), A(:,32), n3(:,278), t3x128(:,:,32), nhel, den(92))
    call cont_QA(nsync, wf16(:,13), wf8(:,27), A(:,33), n3(:,279), t3x128(:,:,33), nhel, den(94))
    call cont_QA(nsync, wf8(:,18), wf16(:,18), A(:,34), n3(:,280), t3x128(:,:,34), nhel, den(96))
    call cont_QA(nsync, wf16(:,13), wf8(:,28), A(:,35), n3(:,281), t3x128(:,:,35), nhel, den(98))
    call cont_QA(nsync, wf16(:,6), wf8(:,29), A(:,36), n3(:,282), t3x128(:,:,36), nhel, den(100))
    call cont_QA(nsync, wf16(:,7), wf8(:,29), A(:,37), n3(:,283), t3x128(:,:,37), nhel, den(101))
    call cont_QA(nsync, wf8(:,19), wf16(:,18), A(:,38), n3(:,284), t3x128(:,:,38), nhel, den(102))
    call cont_QA(nsync, wf16(:,19), wf8(:,31), A(:,39), n3(:,285), t3x128(:,:,39), nhel, den(109))
    call cont_QA(nsync, wf16(:,3), wf8(:,32), A(:,40), n3(:,286), t3x128(:,:,40), nhel, den(111))
    call cont_QA(nsync, wf4(:,20), wf32(:,7), A(:,41), n3(:,287), t3x128(:,:,41), nhel, den(115))
    call cont_VV(nsync, wf8(:,33), wf16(:,20), A(:,42), n3(:,288), t3x128(:,:,42), nhel, den(119))
    call cont_QA(nsync, wf16(:,5), wf8(:,34), A(:,43), n3(:,289), t3x128(:,:,43), nhel, den(121))
    call cont_QA(nsync, wf4(:,20), wf32(:,8), A(:,44), n3(:,290), t3x128(:,:,44), nhel, den(125))
    call cont_QA(nsync, wf8(:,31), wf16(:,21), A(:,45), n3(:,291), t3x128(:,:,45), nhel, den(126))
    call cont_QA(nsync, wf8(:,31), wf16(:,22), A(:,46), n3(:,292), t3x128(:,:,46), nhel, den(127))
    call cont_QA(nsync, wf8(:,37), wf16(:,24), A(:,47), n3(:,293), t3x128(:,:,47), nhel, den(133))
    call cont_QA(nsync, wf8(:,12), wf16(:,25), A(:,48), n3(:,294), t3x128(:,:,48), nhel, den(135))
    call cont_QA(nsync, wf8(:,40), wf16(:,26), A(:,49), n3(:,295), t3x128(:,:,49), nhel, den(140))
    call cont_VV(nsync, wf8(:,33), wf16(:,27), A(:,50), n3(:,296), t3x128(:,:,50), nhel, den(142))
    call cont_QA(nsync, wf4(:,11), wf32(:,9), A(:,51), n3(:,297), t3x128(:,:,51), nhel, den(144))
    call cont_QA(nsync, wf8(:,12), wf16(:,28), A(:,52), n3(:,298), t3x128(:,:,52), nhel, den(146))
    call cont_QA(nsync, wf4(:,11), wf32(:,10), A(:,53), n3(:,299), t3x128(:,:,53), nhel, den(148))
    call cont_QA(nsync, wf8(:,12), wf16(:,29), A(:,54), n3(:,300), t3x128(:,:,54), nhel, den(149))
    call cont_QA(nsync, wf32(:,7), wf4(:,24), A(:,55), n3(:,301), t3x128(:,:,55), nhel, den(151))
    call cont_QA(nsync, wf8(:,18), wf16(:,30), A(:,56), n3(:,302), t3x128(:,:,56), nhel, den(153))
    call cont_VV(nsync, wf4(:,15), wf32(:,11), A(:,57), n3(:,303), t3x128(:,:,57), nhel, den(155))
    call cont_QA(nsync, wf8(:,19), wf16(:,30), A(:,58), n3(:,304), t3x128(:,:,58), nhel, den(156))
    call cont_QA(nsync, wf32(:,8), wf4(:,24), A(:,59), n3(:,305), t3x128(:,:,59), nhel, den(157))
    call cont_QA(nsync, wf16(:,24), wf8(:,43), A(:,60), n3(:,306), t3x128(:,:,60), nhel, den(158))
    call cont_QA(nsync, wf16(:,31), wf8(:,44), A(:,61), n3(:,307), t3x128(:,:,61), nhel, den(161))
    call cont_QA(nsync, wf4(:,17), wf32(:,9), A(:,62), n3(:,308), t3x128(:,:,62), nhel, den(162))
    call cont_VV(nsync, wf4(:,15), wf32(:,12), A(:,63), n3(:,309), t3x128(:,:,63), nhel, den(164))
    call cont_QA(nsync, wf16(:,31), wf8(:,45), A(:,64), n3(:,310), t3x128(:,:,64), nhel, den(165))
    call cont_QA(nsync, wf16(:,3), wf8(:,46), A(:,65), n3(:,311), t3x128(:,:,65), nhel, den(166))
    call cont_QA(nsync, wf4(:,17), wf32(:,10), A(:,66), n3(:,312), t3x128(:,:,66), nhel, den(167))
    call cont_QA(nsync, wf16(:,21), wf8(:,47), A(:,67), n3(:,313), t3x128(:,:,67), nhel, den(169))
    call cont_QA(nsync, wf8(:,44), wf16(:,32), A(:,68), n3(:,314), t3x128(:,:,68), nhel, den(171))
    call cont_QA(nsync, wf16(:,26), wf8(:,48), A(:,69), n3(:,315), t3x128(:,:,69), nhel, den(173))
    call cont_QA(nsync, wf16(:,5), wf8(:,49), A(:,70), n3(:,316), t3x128(:,:,70), nhel, den(175))
    call cont_QA(nsync, wf8(:,27), wf16(:,28), A(:,71), n3(:,317), t3x128(:,:,71), nhel, den(176))
    call cont_QA(nsync, wf8(:,18), wf16(:,33), A(:,72), n3(:,318), t3x128(:,:,72), nhel, den(178))
    call cont_QA(nsync, wf8(:,28), wf16(:,28), A(:,73), n3(:,319), t3x128(:,:,73), nhel, den(179))
    call cont_QA(nsync, wf16(:,22), wf8(:,50), A(:,74), n3(:,320), t3x128(:,:,74), nhel, den(181))
    call cont_QA(nsync, wf8(:,19), wf16(:,33), A(:,75), n3(:,321), t3x128(:,:,75), nhel, den(182))
    call cont_QA(nsync, wf16(:,21), wf8(:,50), A(:,76), n3(:,322), t3x128(:,:,76), nhel, den(183))
    call cont_QA(nsync, wf8(:,2), wf16(:,34), A(:,77), n3(:,323), t3x128(:,:,77), nhel, den(187))
    call cont_QA(nsync, wf8(:,51), wf16(:,36), A(:,78), n3(:,324), t3x128(:,:,78), nhel, den(192))
    call cont_VV(nsync, wf8(:,53), wf16(:,37), A(:,79), n3(:,325), t3x128(:,:,79), nhel, den(198))
    call cont_QA(nsync, wf4(:,7), wf32(:,13), A(:,80), n3(:,326), t3x128(:,:,80), nhel, den(200))
    call cont_QA(nsync, wf8(:,55), wf16(:,38), A(:,81), n3(:,327), t3x128(:,:,81), nhel, den(203))
    call cont_QA(nsync, wf4(:,7), wf32(:,14), A(:,82), n3(:,328), t3x128(:,:,82), nhel, den(206))
    call cont_QA(nsync, wf8(:,2), wf16(:,39), A(:,83), n3(:,329), t3x128(:,:,83), nhel, den(207))
    call cont_QA(nsync, wf8(:,2), wf16(:,40), A(:,84), n3(:,330), t3x128(:,:,84), nhel, den(208))
    call cont_QA(nsync, wf16(:,9), wf8(:,58), A(:,85), n3(:,331), t3x128(:,:,85), nhel, den(211))
    call cont_QA(nsync, wf16(:,41), wf8(:,60), A(:,86), n3(:,332), t3x128(:,:,86), nhel, den(216))
    call cont_VV(nsync, wf8(:,53), wf16(:,42), A(:,87), n3(:,333), t3x128(:,:,87), nhel, den(218))
    call cont_QA(nsync, wf16(:,12), wf8(:,61), A(:,88), n3(:,334), t3x128(:,:,88), nhel, den(220))
    call cont_QA(nsync, wf4(:,29), wf32(:,15), A(:,89), n3(:,335), t3x128(:,:,89), nhel, den(224))
    call cont_QA(nsync, wf8(:,60), wf16(:,43), A(:,90), n3(:,336), t3x128(:,:,90), nhel, den(227))
    call cont_QA(nsync, wf4(:,29), wf32(:,16), A(:,91), n3(:,337), t3x128(:,:,91), nhel, den(229))
    call cont_QA(nsync, wf8(:,60), wf16(:,44), A(:,92), n3(:,338), t3x128(:,:,92), nhel, den(230))
    call cont_QA(nsync, wf4(:,13), wf32(:,13), A(:,93), n3(:,339), t3x128(:,:,93), nhel, den(231))
    call cont_QA(nsync, wf16(:,45), wf8(:,64), A(:,94), n3(:,340), t3x128(:,:,94), nhel, den(234))
    call cont_VV(nsync, wf4(:,15), wf32(:,17), A(:,95), n3(:,341), t3x128(:,:,95), nhel, den(236))
    call cont_QA(nsync, wf16(:,45), wf8(:,65), A(:,96), n3(:,342), t3x128(:,:,96), nhel, den(237))
    call cont_QA(nsync, wf4(:,13), wf32(:,14), A(:,97), n3(:,343), t3x128(:,:,97), nhel, den(238))
    call cont_QA(nsync, wf16(:,9), wf8(:,66), A(:,98), n3(:,344), t3x128(:,:,98), nhel, den(239))
    call cont_QA(nsync, wf8(:,21), wf16(:,46), A(:,99), n3(:,345), t3x128(:,:,99), nhel, den(241))
    call cont_QA(nsync, wf32(:,15), wf4(:,31), A(:,100), n3(:,346), t3x128(:,:,100), nhel, den(243))
    call cont_VV(nsync, wf4(:,15), wf32(:,18), A(:,101), n3(:,347), t3x128(:,:,101), nhel, den(245))
    call cont_QA(nsync, wf8(:,22), wf16(:,46), A(:,102), n3(:,348), t3x128(:,:,102), nhel, den(246))
    call cont_QA(nsync, wf16(:,36), wf8(:,67), A(:,103), n3(:,349), t3x128(:,:,103), nhel, den(247))
    call cont_QA(nsync, wf32(:,16), wf4(:,31), A(:,104), n3(:,350), t3x128(:,:,104), nhel, den(248))
    call cont_QA(nsync, wf8(:,24), wf16(:,39), A(:,105), n3(:,351), t3x128(:,:,105), nhel, den(249))
    call cont_QA(nsync, wf16(:,12), wf8(:,68), A(:,106), n3(:,352), t3x128(:,:,106), nhel, den(251))
    call cont_QA(nsync, wf8(:,21), wf16(:,47), A(:,107), n3(:,353), t3x128(:,:,107), nhel, den(253))
    call cont_QA(nsync, wf16(:,38), wf8(:,69), A(:,108), n3(:,354), t3x128(:,:,108), nhel, den(255))
    call cont_QA(nsync, wf16(:,43), wf8(:,70), A(:,109), n3(:,355), t3x128(:,:,109), nhel, den(257))
    call cont_QA(nsync, wf8(:,64), wf16(:,48), A(:,110), n3(:,356), t3x128(:,:,110), nhel, den(259))
    call cont_QA(nsync, wf16(:,43), wf8(:,71), A(:,111), n3(:,357), t3x128(:,:,111), nhel, den(261))
    call cont_QA(nsync, wf8(:,29), wf16(:,39), A(:,112), n3(:,358), t3x128(:,:,112), nhel, den(262))
    call cont_QA(nsync, wf8(:,29), wf16(:,40), A(:,113), n3(:,359), t3x128(:,:,113), nhel, den(263))
    call cont_QA(nsync, wf8(:,65), wf16(:,48), A(:,114), n3(:,360), t3x128(:,:,114), nhel, den(264))
    call cont_QA(nsync, wf8(:,31), wf16(:,49), A(:,115), n3(:,361), t3x128(:,:,115), nhel, den(267))
    call cont_QA(nsync, wf16(:,36), wf8(:,72), A(:,116), n3(:,362), t3x128(:,:,116), nhel, den(269))
    call cont_QA(nsync, wf4(:,20), wf32(:,19), A(:,117), n3(:,363), t3x128(:,:,117), nhel, den(271))
    call cont_VV(nsync, wf8(:,73), wf16(:,50), A(:,118), n3(:,364), t3x128(:,:,118), nhel, den(275))
    call cont_QA(nsync, wf16(:,38), wf8(:,74), A(:,119), n3(:,365), t3x128(:,:,119), nhel, den(277))
    call cont_QA(nsync, wf4(:,20), wf32(:,20), A(:,120), n3(:,366), t3x128(:,:,120), nhel, den(280))
    call cont_QA(nsync, wf8(:,31), wf16(:,51), A(:,121), n3(:,367), t3x128(:,:,121), nhel, den(281))
    call cont_QA(nsync, wf8(:,31), wf16(:,52), A(:,122), n3(:,368), t3x128(:,:,122), nhel, den(282))
    call cont_QA(nsync, wf16(:,24), wf8(:,77), A(:,123), n3(:,369), t3x128(:,:,123), nhel, den(284))
    call cont_QA(nsync, wf8(:,60), wf16(:,53), A(:,124), n3(:,370), t3x128(:,:,124), nhel, den(286))
    call cont_QA(nsync, wf16(:,26), wf8(:,78), A(:,125), n3(:,371), t3x128(:,:,125), nhel, den(288))
    call cont_VV(nsync, wf8(:,73), wf16(:,54), A(:,126), n3(:,372), t3x128(:,:,126), nhel, den(290))
    call cont_QA(nsync, wf4(:,29), wf32(:,21), A(:,127), n3(:,373), t3x128(:,:,127), nhel, den(292))
    call cont_QA(nsync, wf8(:,60), wf16(:,55), A(:,128), n3(:,374), t3x128(:,:,128), nhel, den(294))
    call cont_QA(nsync, wf4(:,29), wf32(:,22), A(:,129), n3(:,375), t3x128(:,:,129), nhel, den(296))
    call cont_QA(nsync, wf8(:,60), wf16(:,56), A(:,130), n3(:,376), t3x128(:,:,130), nhel, den(297))
    call cont_QA(nsync, wf4(:,24), wf32(:,19), A(:,131), n3(:,377), t3x128(:,:,131), nhel, den(298))
    call cont_QA(nsync, wf8(:,64), wf16(:,57), A(:,132), n3(:,378), t3x128(:,:,132), nhel, den(300))
    call cont_VV(nsync, wf4(:,15), wf32(:,23), A(:,133), n3(:,379), t3x128(:,:,133), nhel, den(302))
    call cont_QA(nsync, wf8(:,65), wf16(:,57), A(:,134), n3(:,380), t3x128(:,:,134), nhel, den(303))
    call cont_QA(nsync, wf4(:,24), wf32(:,20), A(:,135), n3(:,381), t3x128(:,:,135), nhel, den(304))
    call cont_QA(nsync, wf16(:,24), wf8(:,81), A(:,136), n3(:,382), t3x128(:,:,136), nhel, den(305))
    call cont_QA(nsync, wf8(:,44), wf16(:,58), A(:,137), n3(:,383), t3x128(:,:,137), nhel, den(307))
    call cont_QA(nsync, wf4(:,31), wf32(:,21), A(:,138), n3(:,384), t3x128(:,:,138), nhel, den(308))
    call cont_VV(nsync, wf4(:,15), wf32(:,24), A(:,139), n3(:,385), t3x128(:,:,139), nhel, den(310))
    call cont_QA(nsync, wf8(:,45), wf16(:,58), A(:,140), n3(:,386), t3x128(:,:,140), nhel, den(311))
    call cont_QA(nsync, wf16(:,36), wf8(:,82), A(:,141), n3(:,387), t3x128(:,:,141), nhel, den(312))
    call cont_QA(nsync, wf4(:,31), wf32(:,22), A(:,142), n3(:,388), t3x128(:,:,142), nhel, den(313))
    call cont_QA(nsync, wf8(:,47), wf16(:,51), A(:,143), n3(:,389), t3x128(:,:,143), nhel, den(314))
    call cont_QA(nsync, wf8(:,44), wf16(:,59), A(:,144), n3(:,390), t3x128(:,:,144), nhel, den(316))
    call cont_QA(nsync, wf16(:,26), wf8(:,83), A(:,145), n3(:,391), t3x128(:,:,145), nhel, den(318))
    call cont_QA(nsync, wf16(:,38), wf8(:,84), A(:,146), n3(:,392), t3x128(:,:,146), nhel, den(320))
    call cont_QA(nsync, wf8(:,70), wf16(:,55), A(:,147), n3(:,393), t3x128(:,:,147), nhel, den(321))
    call cont_QA(nsync, wf8(:,64), wf16(:,60), A(:,148), n3(:,394), t3x128(:,:,148), nhel, den(323))
    call cont_QA(nsync, wf8(:,71), wf16(:,55), A(:,149), n3(:,395), t3x128(:,:,149), nhel, den(324))
    call cont_QA(nsync, wf8(:,50), wf16(:,52), A(:,150), n3(:,396), t3x128(:,:,150), nhel, den(325))
    call cont_QA(nsync, wf8(:,65), wf16(:,60), A(:,151), n3(:,397), t3x128(:,:,151), nhel, den(326))
    call cont_QA(nsync, wf8(:,50), wf16(:,51), A(:,152), n3(:,398), t3x128(:,:,152), nhel, den(327))
    call cont_VV(nsync, wf4(:,33), wf32(:,25), A(:,153), n3(:,399), t3x128(:,:,153), nhel, den(331))
    call cont_VV(nsync, wf16(:,50), wf8(:,85), A(:,154), n3(:,400), t3x128(:,:,154), nhel, den(333))
    call cont_QA(nsync, wf8(:,6), wf16(:,61), A(:,155), n3(:,401), t3x128(:,:,155), nhel, den(337))
    call cont_QA(nsync, wf8(:,6), wf16(:,62), A(:,156), n3(:,402), t3x128(:,:,156), nhel, den(340))
    call cont_QA(nsync, wf16(:,9), wf8(:,87), A(:,157), n3(:,403), t3x128(:,:,157), nhel, den(341))
    call cont_QA(nsync, wf16(:,9), wf8(:,88), A(:,158), n3(:,404), t3x128(:,:,158), nhel, den(342))
    call cont_QA(nsync, wf8(:,31), wf16(:,63), A(:,159), n3(:,405), t3x128(:,:,159), nhel, den(343))
    call cont_QA(nsync, wf8(:,31), wf16(:,64), A(:,160), n3(:,406), t3x128(:,:,160), nhel, den(344))
    call cont_VV(nsync, wf4(:,33), wf32(:,26), A(:,161), n3(:,407), t3x128(:,:,161), nhel, den(346))
    call cont_VV(nsync, wf8(:,89), wf16(:,65), A(:,162), n3(:,408), t3x128(:,:,162), nhel, den(350))
    call cont_QA(nsync, wf8(:,6), wf16(:,66), A(:,163), n3(:,409), t3x128(:,:,163), nhel, den(353))
    call cont_VV(nsync, wf8(:,89), wf16(:,67), A(:,164), n3(:,410), t3x128(:,:,164), nhel, den(354))
    call cont_QA(nsync, wf16(:,36), wf8(:,91), A(:,165), n3(:,411), t3x128(:,:,165), nhel, den(355))
    call cont_QA(nsync, wf8(:,31), wf16(:,68), A(:,166), n3(:,412), t3x128(:,:,166), nhel, den(356))
    call cont_QA(nsync, wf4(:,5), wf32(:,27), A(:,167), n3(:,413), t3x128(:,:,167), nhel, den(358))
    call cont_VV(nsync, wf8(:,92), wf16(:,69), A(:,168), n3(:,414), t3x128(:,:,168), nhel, den(362))
    call cont_VV(nsync, wf8(:,92), wf16(:,70), A(:,169), n3(:,415), t3x128(:,:,169), nhel, den(363))
    call cont_QA(nsync, wf8(:,54), wf16(:,71), A(:,170), n3(:,416), t3x128(:,:,170), nhel, den(366))
    call cont_QA(nsync, wf16(:,3), wf8(:,94), A(:,171), n3(:,417), t3x128(:,:,171), nhel, den(367))
    call cont_QA(nsync, wf8(:,31), wf16(:,72), A(:,172), n3(:,418), t3x128(:,:,172), nhel, den(368))
    call cont_VV(nsync, wf16(:,65), wf8(:,95), A(:,173), n3(:,419), t3x128(:,:,173), nhel, den(371))
    call cont_QA(nsync, wf16(:,12), wf8(:,96), A(:,174), n3(:,420), t3x128(:,:,174), nhel, den(373))
    call cont_VV(nsync, wf16(:,69), wf8(:,97), A(:,175), n3(:,421), t3x128(:,:,175), nhel, den(376))
    call cont_QA(nsync, wf16(:,12), wf8(:,98), A(:,176), n3(:,422), t3x128(:,:,176), nhel, den(378))
    call cont_VV(nsync, wf16(:,69), wf8(:,99), A(:,177), n3(:,423), t3x128(:,:,177), nhel, den(379))
    call cont_VV(nsync, wf16(:,65), wf8(:,100), A(:,178), n3(:,424), t3x128(:,:,178), nhel, den(381))
    call cont_VV(nsync, wf16(:,70), wf8(:,97), A(:,179), n3(:,425), t3x128(:,:,179), nhel, den(383))
    call cont_QA(nsync, wf16(:,38), wf8(:,101), A(:,180), n3(:,426), t3x128(:,:,180), nhel, den(385))
    call cont_VV(nsync, wf16(:,70), wf8(:,99), A(:,181), n3(:,427), t3x128(:,:,181), nhel, den(386))
    call cont_QA(nsync, wf16(:,5), wf8(:,102), A(:,182), n3(:,428), t3x128(:,:,182), nhel, den(388))
    call cont_VV(nsync, wf16(:,67), wf8(:,95), A(:,183), n3(:,429), t3x128(:,:,183), nhel, den(390))
    call cont_VV(nsync, wf16(:,67), wf8(:,100), A(:,184), n3(:,430), t3x128(:,:,184), nhel, den(391))
    call cont_VV(nsync, wf16(:,37), wf8(:,103), A(:,185), n3(:,431), t3x128(:,:,185), nhel, den(393))
    call cont_VV(nsync, wf4(:,35), wf32(:,28), A(:,186), n3(:,432), t3x128(:,:,186), nhel, den(397))
    call cont_QA(nsync, wf8(:,6), wf16(:,73), A(:,187), n3(:,433), t3x128(:,:,187), nhel, den(400))
    call cont_QA(nsync, wf8(:,6), wf16(:,74), A(:,188), n3(:,434), t3x128(:,:,188), nhel, den(404))
    call cont_QA(nsync, wf8(:,2), wf16(:,75), A(:,189), n3(:,435), t3x128(:,:,189), nhel, den(405))
    call cont_QA(nsync, wf8(:,2), wf16(:,76), A(:,190), n3(:,436), t3x128(:,:,190), nhel, den(406))
    call cont_QA(nsync, wf16(:,24), wf8(:,105), A(:,191), n3(:,437), t3x128(:,:,191), nhel, den(407))
    call cont_QA(nsync, wf16(:,24), wf8(:,106), A(:,192), n3(:,438), t3x128(:,:,192), nhel, den(408))
    call cont_VV(nsync, wf8(:,107), wf16(:,77), A(:,193), n3(:,439), t3x128(:,:,193), nhel, den(412))
    call cont_VV(nsync, wf4(:,35), wf32(:,29), A(:,194), n3(:,440), t3x128(:,:,194), nhel, den(414))
    call cont_QA(nsync, wf8(:,6), wf16(:,78), A(:,195), n3(:,441), t3x128(:,:,195), nhel, den(417))
    call cont_VV(nsync, wf8(:,107), wf16(:,79), A(:,196), n3(:,442), t3x128(:,:,196), nhel, den(418))
    call cont_QA(nsync, wf8(:,2), wf16(:,80), A(:,197), n3(:,443), t3x128(:,:,197), nhel, den(419))
    call cont_QA(nsync, wf16(:,36), wf8(:,109), A(:,198), n3(:,444), t3x128(:,:,198), nhel, den(420))
    call cont_VV(nsync, wf8(:,110), wf16(:,81), A(:,199), n3(:,445), t3x128(:,:,199), nhel, den(424))
    call cont_QA(nsync, wf4(:,5), wf32(:,30), A(:,200), n3(:,446), t3x128(:,:,200), nhel, den(426))
    call cont_VV(nsync, wf8(:,110), wf16(:,82), A(:,201), n3(:,447), t3x128(:,:,201), nhel, den(427))
    call cont_QA(nsync, wf8(:,54), wf16(:,83), A(:,202), n3(:,448), t3x128(:,:,202), nhel, den(430))
    call cont_QA(nsync, wf8(:,2), wf16(:,84), A(:,203), n3(:,449), t3x128(:,:,203), nhel, den(431))
    call cont_QA(nsync, wf16(:,3), wf8(:,112), A(:,204), n3(:,450), t3x128(:,:,204), nhel, den(432))
    call cont_VV(nsync, wf16(:,81), wf8(:,113), A(:,205), n3(:,451), t3x128(:,:,205), nhel, den(435))
    call cont_VV(nsync, wf16(:,81), wf8(:,114), A(:,206), n3(:,452), t3x128(:,:,206), nhel, den(436))
    call cont_VV(nsync, wf16(:,77), wf8(:,115), A(:,207), n3(:,453), t3x128(:,:,207), nhel, den(439))
    call cont_VV(nsync, wf16(:,77), wf8(:,116), A(:,208), n3(:,454), t3x128(:,:,208), nhel, den(441))
    call cont_QA(nsync, wf16(:,26), wf8(:,117), A(:,209), n3(:,455), t3x128(:,:,209), nhel, den(443))
    call cont_QA(nsync, wf16(:,26), wf8(:,118), A(:,210), n3(:,456), t3x128(:,:,210), nhel, den(445))
    call cont_VV(nsync, wf16(:,82), wf8(:,114), A(:,211), n3(:,457), t3x128(:,:,211), nhel, den(447))
    call cont_VV(nsync, wf16(:,82), wf8(:,113), A(:,212), n3(:,458), t3x128(:,:,212), nhel, den(448))
    call cont_QA(nsync, wf16(:,38), wf8(:,119), A(:,213), n3(:,459), t3x128(:,:,213), nhel, den(450))
    call cont_VV(nsync, wf16(:,79), wf8(:,116), A(:,214), n3(:,460), t3x128(:,:,214), nhel, den(452))
    call cont_QA(nsync, wf16(:,5), wf8(:,120), A(:,215), n3(:,461), t3x128(:,:,215), nhel, den(454))
    call cont_VV(nsync, wf16(:,79), wf8(:,115), A(:,216), n3(:,462), t3x128(:,:,216), nhel, den(455))
    call cont_VV(nsync, wf16(:,42), wf8(:,103), A(:,217), n3(:,463), t3x128(:,:,217), nhel, den(456))
    call cont_VV(nsync, wf16(:,65), wf8(:,121), A(:,218), n3(:,464), t3x128(:,:,218), nhel, den(459))
    call cont_QA(nsync, wf8(:,6), wf16(:,85), A(:,219), n3(:,465), t3x128(:,:,219), nhel, den(462))
    call cont_VV(nsync, wf16(:,67), wf8(:,121), A(:,220), n3(:,466), t3x128(:,:,220), nhel, den(463))
    call cont_QA(nsync, wf8(:,60), wf16(:,86), A(:,221), n3(:,467), t3x128(:,:,221), nhel, den(464))
    call cont_QA(nsync, wf16(:,24), wf8(:,122), A(:,222), n3(:,468), t3x128(:,:,222), nhel, den(465))
    call cont_VV(nsync, wf16(:,77), wf8(:,123), A(:,223), n3(:,469), t3x128(:,:,223), nhel, den(468))
    call cont_VV(nsync, wf16(:,54), wf8(:,85), A(:,224), n3(:,470), t3x128(:,:,224), nhel, den(469))
    call cont_QA(nsync, wf8(:,6), wf16(:,87), A(:,225), n3(:,471), t3x128(:,:,225), nhel, den(472))
    call cont_VV(nsync, wf16(:,79), wf8(:,123), A(:,226), n3(:,472), t3x128(:,:,226), nhel, den(473))
    call cont_QA(nsync, wf16(:,9), wf8(:,124), A(:,227), n3(:,473), t3x128(:,:,227), nhel, den(474))
    call cont_QA(nsync, wf8(:,60), wf16(:,88), A(:,228), n3(:,474), t3x128(:,:,228), nhel, den(475))
    call cont_QA(nsync, wf4(:,5), wf32(:,31), A(:,229), n3(:,475), t3x128(:,:,229), nhel, den(477))
    call cont_QA(nsync, wf2(:,8), wf64(:,1), A(:,230), n3(:,476), t3x128(:,:,230), nhel, den(479))
    call cont_QA(nsync, wf4(:,5), wf32(:,32), A(:,231), n3(:,477), t3x128(:,:,231), nhel, den(481))
    call cont_QA(nsync, wf2(:,8), wf64(:,2), A(:,232), n3(:,478), t3x128(:,:,232), nhel, den(483))
    call cont_QA(nsync, wf16(:,3), wf8(:,125), A(:,233), n3(:,479), t3x128(:,:,233), nhel, den(484))
    call cont_QA(nsync, wf8(:,60), wf16(:,89), A(:,234), n3(:,480), t3x128(:,:,234), nhel, den(485))
    call cont_QA(nsync, wf16(:,3), wf8(:,126), A(:,235), n3(:,481), t3x128(:,:,235), nhel, den(486))
    call cont_QA(nsync, wf8(:,60), wf16(:,90), A(:,236), n3(:,482), t3x128(:,:,236), nhel, den(487))
    call cont_VV(nsync, wf16(:,77), wf8(:,127), A(:,237), n3(:,483), t3x128(:,:,237), nhel, den(488))
    call cont_VV(nsync, wf16(:,77), wf8(:,128), A(:,238), n3(:,484), t3x128(:,:,238), nhel, den(490))
    call cont_QA(nsync, wf16(:,26), wf8(:,129), A(:,239), n3(:,485), t3x128(:,:,239), nhel, den(492))
    call cont_QA(nsync, wf16(:,12), wf8(:,130), A(:,240), n3(:,486), t3x128(:,:,240), nhel, den(494))
    call cont_VV(nsync, wf16(:,65), wf8(:,131), A(:,241), n3(:,487), t3x128(:,:,241), nhel, den(495))
    call cont_VV(nsync, wf16(:,65), wf8(:,132), A(:,242), n3(:,488), t3x128(:,:,242), nhel, den(497))
    call cont_QA(nsync, wf16(:,5), wf8(:,133), A(:,243), n3(:,489), t3x128(:,:,243), nhel, den(499))
    call cont_VV(nsync, wf16(:,79), wf8(:,128), A(:,244), n3(:,490), t3x128(:,:,244), nhel, den(500))
    call cont_QA(nsync, wf16(:,5), wf8(:,134), A(:,245), n3(:,491), t3x128(:,:,245), nhel, den(502))
    call cont_VV(nsync, wf16(:,67), wf8(:,132), A(:,246), n3(:,492), t3x128(:,:,246), nhel, den(503))
    call cont_VV(nsync, wf16(:,67), wf8(:,131), A(:,247), n3(:,493), t3x128(:,:,247), nhel, den(504))
    call cont_VV(nsync, wf16(:,79), wf8(:,127), A(:,248), n3(:,494), t3x128(:,:,248), nhel, den(505))
    call cont_QA(nsync, wf2(:,4), wf64(:,3), A(:,249), n3(:,495), t3x128(:,:,249), nhel, den(507))
    call cont_VV(nsync, wf16(:,69), wf8(:,135), A(:,250), n3(:,496), t3x128(:,:,250), nhel, den(510))
    call cont_VV(nsync, wf16(:,70), wf8(:,135), A(:,251), n3(:,497), t3x128(:,:,251), nhel, den(511))
    call cont_QA(nsync, wf8(:,54), wf16(:,91), A(:,252), n3(:,498), t3x128(:,:,252), nhel, den(514))
    call cont_QA(nsync, wf8(:,12), wf16(:,92), A(:,253), n3(:,499), t3x128(:,:,253), nhel, den(515))
    call cont_QA(nsync, wf16(:,24), wf8(:,136), A(:,254), n3(:,500), t3x128(:,:,254), nhel, den(516))
    call cont_VV(nsync, wf16(:,81), wf8(:,137), A(:,255), n3(:,501), t3x128(:,:,255), nhel, den(519))
    call cont_QA(nsync, wf2(:,4), wf64(:,4), A(:,256), n3(:,502), t3x128(:,:,256), nhel, den(521))
    call cont_VV(nsync, wf16(:,82), wf8(:,137), A(:,257), n3(:,503), t3x128(:,:,257), nhel, den(522))
    call cont_QA(nsync, wf8(:,54), wf16(:,93), A(:,258), n3(:,504), t3x128(:,:,258), nhel, den(525))
    call cont_QA(nsync, wf16(:,9), wf8(:,138), A(:,259), n3(:,505), t3x128(:,:,259), nhel, den(526))
    call cont_QA(nsync, wf8(:,12), wf16(:,94), A(:,260), n3(:,506), t3x128(:,:,260), nhel, den(527))
    call cont_QA(nsync, wf2(:,4), wf64(:,5), A(:,261), n3(:,507), t3x128(:,:,261), nhel, den(529))
    call cont_QA(nsync, wf4(:,27), wf32(:,33), A(:,262), n3(:,508), t3x128(:,:,262), nhel, den(531))
    call cont_QA(nsync, wf2(:,4), wf64(:,6), A(:,263), n3(:,509), t3x128(:,:,263), nhel, den(533))
    call cont_QA(nsync, wf4(:,27), wf32(:,34), A(:,264), n3(:,510), t3x128(:,:,264), nhel, den(535))
    call cont_QA(nsync, wf8(:,12), wf16(:,95), A(:,265), n3(:,511), t3x128(:,:,265), nhel, den(536))
    call cont_QA(nsync, wf16(:,36), wf8(:,139), A(:,266), n3(:,512), t3x128(:,:,266), nhel, den(537))
    call cont_QA(nsync, wf8(:,12), wf16(:,96), A(:,267), n3(:,513), t3x128(:,:,267), nhel, den(538))
    call cont_QA(nsync, wf16(:,36), wf8(:,140), A(:,268), n3(:,514), t3x128(:,:,268), nhel, den(539))
    call cont_VV(nsync, wf16(:,81), wf8(:,141), A(:,269), n3(:,515), t3x128(:,:,269), nhel, den(541))
    call cont_VV(nsync, wf16(:,81), wf8(:,142), A(:,270), n3(:,516), t3x128(:,:,270), nhel, den(542))
    call cont_QA(nsync, wf16(:,26), wf8(:,143), A(:,271), n3(:,517), t3x128(:,:,271), nhel, den(544))
    call cont_QA(nsync, wf16(:,12), wf8(:,144), A(:,272), n3(:,518), t3x128(:,:,272), nhel, den(546))
    call cont_VV(nsync, wf16(:,69), wf8(:,145), A(:,273), n3(:,519), t3x128(:,:,273), nhel, den(548))
    call cont_VV(nsync, wf16(:,69), wf8(:,146), A(:,274), n3(:,520), t3x128(:,:,274), nhel, den(549))
    call cont_VV(nsync, wf16(:,82), wf8(:,141), A(:,275), n3(:,521), t3x128(:,:,275), nhel, den(550))
    call cont_QA(nsync, wf16(:,38), wf8(:,147), A(:,276), n3(:,522), t3x128(:,:,276), nhel, den(552))
    call cont_VV(nsync, wf16(:,70), wf8(:,145), A(:,277), n3(:,523), t3x128(:,:,277), nhel, den(553))
    call cont_QA(nsync, wf16(:,38), wf8(:,148), A(:,278), n3(:,524), t3x128(:,:,278), nhel, den(555))
    call cont_VV(nsync, wf16(:,82), wf8(:,142), A(:,279), n3(:,525), t3x128(:,:,279), nhel, den(556))
    call cont_VV(nsync, wf16(:,70), wf8(:,146), A(:,280), n3(:,526), t3x128(:,:,280), nhel, den(557))
    call cont_QA(nsync, wf8(:,21), wf16(:,97), A(:,281), n3(:,527), t3x128(:,:,281), nhel, den(559))
    call cont_QA(nsync, wf8(:,21), wf16(:,98), A(:,282), n3(:,528), t3x128(:,:,282), nhel, den(561))
    call cont_VV(nsync, wf16(:,69), wf8(:,149), A(:,283), n3(:,529), t3x128(:,:,283), nhel, den(562))
    call cont_VV(nsync, wf16(:,65), wf8(:,150), A(:,284), n3(:,530), t3x128(:,:,284), nhel, den(564))
    call cont_QA(nsync, wf8(:,64), wf16(:,99), A(:,285), n3(:,531), t3x128(:,:,285), nhel, den(566))
    call cont_VV(nsync, wf16(:,70), wf8(:,149), A(:,286), n3(:,532), t3x128(:,:,286), nhel, den(567))
    call cont_QA(nsync, wf8(:,18), wf16(:,100), A(:,287), n3(:,533), t3x128(:,:,287), nhel, den(569))
    call cont_VV(nsync, wf16(:,67), wf8(:,150), A(:,288), n3(:,534), t3x128(:,:,288), nhel, den(570))
    call cont_QA(nsync, wf8(:,19), wf16(:,100), A(:,289), n3(:,535), t3x128(:,:,289), nhel, den(571))
    call cont_QA(nsync, wf8(:,29), wf16(:,75), A(:,290), n3(:,536), t3x128(:,:,290), nhel, den(572))
    call cont_QA(nsync, wf8(:,65), wf16(:,99), A(:,291), n3(:,537), t3x128(:,:,291), nhel, den(573))
    call cont_QA(nsync, wf8(:,29), wf16(:,76), A(:,292), n3(:,538), t3x128(:,:,292), nhel, den(574))
    call cont_VV(nsync, wf16(:,81), wf8(:,151), A(:,293), n3(:,539), t3x128(:,:,293), nhel, den(575))
    call cont_VV(nsync, wf16(:,77), wf8(:,152), A(:,294), n3(:,540), t3x128(:,:,294), nhel, den(577))
    call cont_QA(nsync, wf8(:,44), wf16(:,101), A(:,295), n3(:,541), t3x128(:,:,295), nhel, den(579))
    call cont_QA(nsync, wf8(:,44), wf16(:,102), A(:,296), n3(:,542), t3x128(:,:,296), nhel, den(581))
    call cont_VV(nsync, wf16(:,82), wf8(:,151), A(:,297), n3(:,543), t3x128(:,:,297), nhel, den(582))
    call cont_QA(nsync, wf8(:,64), wf16(:,103), A(:,298), n3(:,544), t3x128(:,:,298), nhel, den(584))
    call cont_VV(nsync, wf16(:,79), wf8(:,152), A(:,299), n3(:,545), t3x128(:,:,299), nhel, den(585))
    call cont_QA(nsync, wf8(:,18), wf16(:,104), A(:,300), n3(:,546), t3x128(:,:,300), nhel, den(587))
    call cont_QA(nsync, wf8(:,19), wf16(:,104), A(:,301), n3(:,547), t3x128(:,:,301), nhel, den(588))
    call cont_QA(nsync, wf8(:,65), wf16(:,103), A(:,302), n3(:,548), t3x128(:,:,302), nhel, den(589))
    call cont_QA(nsync, wf8(:,50), wf16(:,64), A(:,303), n3(:,549), t3x128(:,:,303), nhel, den(590))
    call cont_QA(nsync, wf8(:,50), wf16(:,63), A(:,304), n3(:,550), t3x128(:,:,304), nhel, den(591))
    call cont_VV(nsync, wf16(:,77), wf8(:,153), A(:,305), n3(:,551), t3x128(:,:,305), nhel, den(593))
    call cont_QA(nsync, wf8(:,44), wf16(:,105), A(:,306), n3(:,552), t3x128(:,:,306), nhel, den(595))
    call cont_QA(nsync, wf8(:,21), wf16(:,106), A(:,307), n3(:,553), t3x128(:,:,307), nhel, den(597))
    call cont_VV(nsync, wf16(:,65), wf8(:,154), A(:,308), n3(:,554), t3x128(:,:,308), nhel, den(599))
    call cont_QA(nsync, wf8(:,18), wf16(:,107), A(:,309), n3(:,555), t3x128(:,:,309), nhel, den(601))
    call cont_VV(nsync, wf16(:,79), wf8(:,153), A(:,310), n3(:,556), t3x128(:,:,310), nhel, den(602))
    call cont_QA(nsync, wf8(:,18), wf16(:,108), A(:,311), n3(:,557), t3x128(:,:,311), nhel, den(604))
    call cont_VV(nsync, wf16(:,67), wf8(:,154), A(:,312), n3(:,558), t3x128(:,:,312), nhel, den(605))
    call cont_QA(nsync, wf8(:,19), wf16(:,107), A(:,313), n3(:,559), t3x128(:,:,313), nhel, den(606))
    call cont_QA(nsync, wf8(:,19), wf16(:,108), A(:,314), n3(:,560), t3x128(:,:,314), nhel, den(607))
    call cont_QA(nsync, wf8(:,29), wf16(:,80), A(:,315), n3(:,561), t3x128(:,:,315), nhel, den(608))
    call cont_QA(nsync, wf8(:,50), wf16(:,68), A(:,316), n3(:,562), t3x128(:,:,316), nhel, den(609))
    call cont_VV(nsync, wf16(:,81), wf8(:,155), A(:,317), n3(:,563), t3x128(:,:,317), nhel, den(611))
    call cont_QA(nsync, wf8(:,44), wf16(:,109), A(:,318), n3(:,564), t3x128(:,:,318), nhel, den(613))
    call cont_QA(nsync, wf8(:,21), wf16(:,110), A(:,319), n3(:,565), t3x128(:,:,319), nhel, den(615))
    call cont_VV(nsync, wf16(:,69), wf8(:,156), A(:,320), n3(:,566), t3x128(:,:,320), nhel, den(617))
    call cont_VV(nsync, wf16(:,82), wf8(:,155), A(:,321), n3(:,567), t3x128(:,:,321), nhel, den(618))
    call cont_QA(nsync, wf8(:,64), wf16(:,111), A(:,322), n3(:,568), t3x128(:,:,322), nhel, den(620))
    call cont_VV(nsync, wf16(:,70), wf8(:,156), A(:,323), n3(:,569), t3x128(:,:,323), nhel, den(621))
    call cont_QA(nsync, wf8(:,64), wf16(:,112), A(:,324), n3(:,570), t3x128(:,:,324), nhel, den(623))
    call cont_QA(nsync, wf8(:,65), wf16(:,112), A(:,325), n3(:,571), t3x128(:,:,325), nhel, den(624))
    call cont_QA(nsync, wf8(:,65), wf16(:,111), A(:,326), n3(:,572), t3x128(:,:,326), nhel, den(625))
    call cont_QA(nsync, wf8(:,29), wf16(:,84), A(:,327), n3(:,573), t3x128(:,:,327), nhel, den(626))
    call cont_QA(nsync, wf8(:,50), wf16(:,72), A(:,328), n3(:,574), t3x128(:,:,328), nhel, den(627))

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
  complex(REALKIND), intent(out) :: M1(4) ! M1helarray(4,128)
  integer :: empty(0)

  M1(1) = ((A(j,133)%j+A(j,134)%j+A(j,139)%j+A(j,140)%j+A(j,149)%j+A(j,152)%j+A(j,289)%j+A(j,290)%j+A(j,313)%j &
       +A(j,315)%j)*f(1))/6._/**/REALKIND+((A(j,57)%j+A(j,58)%j+A(j,76)%j+A(j,101)%j+A(j,102)%j+A(j,111)%j+A(j,291)%j+A(j,292)%j &
       +A(j,314)%j+A(j,316)%j)*f(1))/2._/**/REALKIND+(CI*(A(j,74)%j+A(j,75)%j-A(j,113)%j-A(j,114)%j)*f(2))/2._/**/REALKIND &
       +((A(j,41)%j+A(j,44)%j+A(j,45)%j+A(j,47)%j+A(j,48)%j+A(j,49)%j+A(j,55)%j+A(j,56)%j+A(j,59)%j+A(j,60)%j+A(j,67)%j+A(j,69)%j &
       +A(j,77)%j+A(j,78)%j+A(j,81)%j+A(j,89)%j+A(j,90)%j+A(j,91)%j+A(j,99)%j+A(j,100)%j+A(j,103)%j+A(j,104)%j+A(j,108)%j &
       +A(j,109)%j+A(j,161)%j+A(j,163)%j+A(j,165)%j+A(j,166)%j+A(j,177)%j+A(j,180)%j+A(j,181)%j+A(j,185)%j+A(j,188)%j+A(j,190)%j &
       +A(j,191)%j+A(j,193)%j+A(j,196)%j+A(j,209)%j+A(j,217)%j+A(j,219)%j+A(j,221)%j+A(j,222)%j+A(j,238)%j+A(j,239)%j+A(j,244)%j &
       +A(j,250)%j+A(j,251)%j+A(j,261)%j+A(j,264)%j+A(j,266)%j+A(j,267)%j+A(j,276)%j+A(j,282)%j+A(j,283)%j+A(j,285)%j+A(j,286)%j &
       +A(j,305)%j+A(j,306)%j+A(j,310)%j+A(j,311)%j)*f(3))/2._/**/REALKIND+((A(j,115)%j+A(j,116)%j+A(j,117)%j+A(j,119)%j &
       +A(j,120)%j+A(j,121)%j+A(j,123)%j+A(j,124)%j+A(j,125)%j+A(j,127)%j+A(j,128)%j+A(j,129)%j+A(j,131)%j+A(j,132)%j+A(j,135)%j &
       +A(j,136)%j+A(j,137)%j+A(j,138)%j+A(j,141)%j+A(j,142)%j+A(j,143)%j+A(j,145)%j+A(j,146)%j+A(j,147)%j+A(j,162)%j+A(j,164)%j &
       +A(j,178)%j+A(j,184)%j+A(j,186)%j+A(j,187)%j+A(j,189)%j+A(j,192)%j+A(j,194)%j+A(j,195)%j+A(j,197)%j+A(j,198)%j+A(j,210)%j &
       +A(j,213)%j+A(j,218)%j+A(j,220)%j+A(j,242)%j+A(j,246)%j+A(j,249)%j+A(j,252)%j+A(j,253)%j+A(j,254)%j+A(j,262)%j+A(j,263)%j &
       +A(j,265)%j+A(j,268)%j+A(j,271)%j+A(j,278)%j+A(j,281)%j+A(j,284)%j+A(j,287)%j+A(j,288)%j+A(j,307)%j+A(j,308)%j+A(j,309)%j &
       +A(j,312)%j)*f(3))/6._/**/REALKIND+(CI*(A(j,42)%j+A(j,46)%j+A(j,50)%j+A(j,54)%j+A(j,68)%j+A(j,72)%j-A(j,79)%j-A(j,84)%j &
       -A(j,87)%j-A(j,92)%j-A(j,107)%j-A(j,110)%j-A(j,175)%j-A(j,179)%j+A(j,207)%j+A(j,216)%j+A(j,237)%j+A(j,248)%j-A(j,274)%j &
       -A(j,280)%j)*f(4))/2._/**/REALKIND
  M1(2) = ((-A(j,19)%j-A(j,20)%j-A(j,36)%j-A(j,139)%j-A(j,140)%j-A(j,149)%j-A(j,302)%j-A(j,303)%j-A(j,313)%j &
       -A(j,315)%j)*f(1))/2._/**/REALKIND+((-A(j,95)%j-A(j,96)%j-A(j,101)%j-A(j,102)%j-A(j,111)%j-A(j,112)%j-A(j,301)%j-A(j,304)%j &
       -A(j,314)%j-A(j,316)%j)*f(1))/6._/**/REALKIND+(CI*(-A(j,37)%j-A(j,38)%j+A(j,150)%j+A(j,151)%j)*f(2))/2._/**/REALKIND+(( &
       -A(j,77)%j-A(j,78)%j-A(j,80)%j-A(j,81)%j-A(j,82)%j-A(j,83)%j-A(j,85)%j-A(j,86)%j-A(j,88)%j-A(j,89)%j-A(j,90)%j-A(j,91)%j &
       -A(j,93)%j-A(j,94)%j-A(j,97)%j-A(j,98)%j-A(j,99)%j-A(j,100)%j-A(j,103)%j-A(j,104)%j-A(j,105)%j-A(j,106)%j-A(j,108)%j &
       -A(j,109)%j-A(j,153)%j-A(j,156)%j-A(j,158)%j-A(j,159)%j-A(j,161)%j-A(j,163)%j-A(j,165)%j-A(j,166)%j-A(j,176)%j-A(j,180)%j &
       -A(j,193)%j-A(j,196)%j-A(j,208)%j-A(j,214)%j-A(j,223)%j-A(j,226)%j-A(j,238)%j-A(j,244)%j-A(j,256)%j-A(j,258)%j-A(j,259)%j &
       -A(j,260)%j-A(j,261)%j-A(j,264)%j-A(j,266)%j-A(j,267)%j-A(j,272)%j-A(j,276)%j-A(j,294)%j-A(j,295)%j-A(j,299)%j-A(j,300)%j &
       -A(j,305)%j-A(j,306)%j-A(j,310)%j-A(j,311)%j)*f(3))/6._/**/REALKIND+((-A(j,4)%j-A(j,6)%j-A(j,7)%j-A(j,9)%j-A(j,10)%j &
       -A(j,12)%j-A(j,17)%j-A(j,18)%j-A(j,21)%j-A(j,22)%j-A(j,29)%j-A(j,30)%j-A(j,115)%j-A(j,116)%j-A(j,119)%j-A(j,127)%j &
       -A(j,128)%j-A(j,129)%j-A(j,137)%j-A(j,138)%j-A(j,141)%j-A(j,142)%j-A(j,146)%j-A(j,147)%j-A(j,154)%j-A(j,155)%j-A(j,157)%j &
       -A(j,160)%j-A(j,162)%j-A(j,164)%j-A(j,174)%j-A(j,194)%j-A(j,195)%j-A(j,197)%j-A(j,198)%j-A(j,206)%j-A(j,211)%j-A(j,213)%j &
       -A(j,224)%j-A(j,225)%j-A(j,227)%j-A(j,228)%j-A(j,240)%j-A(j,242)%j-A(j,246)%j-A(j,255)%j-A(j,257)%j-A(j,262)%j-A(j,263)%j &
       -A(j,265)%j-A(j,268)%j-A(j,278)%j-A(j,293)%j-A(j,296)%j-A(j,297)%j-A(j,298)%j-A(j,307)%j-A(j,308)%j-A(j,309)%j &
       -A(j,312)%j)*f(3))/2._/**/REALKIND+(CI*(-A(j,3)%j-A(j,8)%j-A(j,11)%j-A(j,16)%j-A(j,31)%j-A(j,34)%j+A(j,118)%j+A(j,122)%j &
       +A(j,126)%j+A(j,130)%j+A(j,144)%j+A(j,148)%j-A(j,173)%j-A(j,183)%j+A(j,205)%j+A(j,212)%j-A(j,241)%j-A(j,247)%j+A(j,270)%j &
       +A(j,279)%j)*f(4))/2._/**/REALKIND
  M1(3) = ((-A(j,57)%j-A(j,58)%j-A(j,63)%j-A(j,64)%j-A(j,73)%j-A(j,76)%j-A(j,291)%j-A(j,292)%j-A(j,326)%j &
       -A(j,327)%j)*f(1))/6._/**/REALKIND+((-A(j,25)%j-A(j,26)%j-A(j,35)%j-A(j,133)%j-A(j,134)%j-A(j,152)%j-A(j,289)%j-A(j,290)%j &
       -A(j,325)%j-A(j,328)%j)*f(1))/2._/**/REALKIND+(CI*(A(j,37)%j+A(j,38)%j-A(j,150)%j-A(j,151)%j)*f(2))/2._/**/REALKIND+(( &
       -A(j,39)%j-A(j,40)%j-A(j,41)%j-A(j,43)%j-A(j,44)%j-A(j,45)%j-A(j,47)%j-A(j,48)%j-A(j,49)%j-A(j,51)%j-A(j,52)%j-A(j,53)%j &
       -A(j,55)%j-A(j,56)%j-A(j,59)%j-A(j,60)%j-A(j,61)%j-A(j,62)%j-A(j,65)%j-A(j,66)%j-A(j,67)%j-A(j,69)%j-A(j,70)%j-A(j,71)%j &
       -A(j,168)%j-A(j,169)%j-A(j,177)%j-A(j,181)%j-A(j,185)%j-A(j,188)%j-A(j,190)%j-A(j,191)%j-A(j,200)%j-A(j,202)%j-A(j,203)%j &
       -A(j,204)%j-A(j,209)%j-A(j,215)%j-A(j,217)%j-A(j,219)%j-A(j,221)%j-A(j,222)%j-A(j,229)%j-A(j,232)%j-A(j,234)%j-A(j,235)%j &
       -A(j,239)%j-A(j,245)%j-A(j,250)%j-A(j,251)%j-A(j,273)%j-A(j,277)%j-A(j,282)%j-A(j,283)%j-A(j,285)%j-A(j,286)%j-A(j,319)%j &
       -A(j,320)%j-A(j,322)%j-A(j,323)%j)*f(3))/6._/**/REALKIND+((-A(j,1)%j-A(j,2)%j-A(j,5)%j-A(j,13)%j-A(j,14)%j-A(j,15)%j &
       -A(j,23)%j-A(j,24)%j-A(j,27)%j-A(j,28)%j-A(j,32)%j-A(j,33)%j-A(j,117)%j-A(j,120)%j-A(j,121)%j-A(j,123)%j-A(j,124)%j &
       -A(j,125)%j-A(j,131)%j-A(j,132)%j-A(j,135)%j-A(j,136)%j-A(j,143)%j-A(j,145)%j-A(j,167)%j-A(j,170)%j-A(j,171)%j-A(j,172)%j &
       -A(j,178)%j-A(j,182)%j-A(j,184)%j-A(j,186)%j-A(j,187)%j-A(j,189)%j-A(j,192)%j-A(j,199)%j-A(j,201)%j-A(j,210)%j-A(j,218)%j &
       -A(j,220)%j-A(j,230)%j-A(j,231)%j-A(j,233)%j-A(j,236)%j-A(j,243)%j-A(j,249)%j-A(j,252)%j-A(j,253)%j-A(j,254)%j-A(j,269)%j &
       -A(j,271)%j-A(j,275)%j-A(j,281)%j-A(j,284)%j-A(j,287)%j-A(j,288)%j-A(j,317)%j-A(j,318)%j-A(j,321)%j &
       -A(j,324)%j)*f(3))/2._/**/REALKIND+(CI*(A(j,3)%j+A(j,8)%j+A(j,11)%j+A(j,16)%j+A(j,31)%j+A(j,34)%j-A(j,118)%j-A(j,122)%j &
       -A(j,126)%j-A(j,130)%j-A(j,144)%j-A(j,148)%j+A(j,173)%j+A(j,183)%j-A(j,205)%j-A(j,212)%j+A(j,241)%j+A(j,247)%j-A(j,270)%j &
       -A(j,279)%j)*f(4))/2._/**/REALKIND
  M1(4) = ((A(j,63)%j+A(j,64)%j+A(j,73)%j+A(j,95)%j+A(j,96)%j+A(j,112)%j+A(j,301)%j+A(j,304)%j+A(j,326)%j &
       +A(j,327)%j)*f(1))/2._/**/REALKIND+((A(j,19)%j+A(j,20)%j+A(j,25)%j+A(j,26)%j+A(j,35)%j+A(j,36)%j+A(j,302)%j+A(j,303)%j &
       +A(j,325)%j+A(j,328)%j)*f(1))/6._/**/REALKIND+(CI*(-A(j,74)%j-A(j,75)%j+A(j,113)%j+A(j,114)%j)*f(2))/2._/**/REALKIND &
       +((A(j,39)%j+A(j,40)%j+A(j,43)%j+A(j,51)%j+A(j,52)%j+A(j,53)%j+A(j,61)%j+A(j,62)%j+A(j,65)%j+A(j,66)%j+A(j,70)%j+A(j,71)%j &
       +A(j,80)%j+A(j,82)%j+A(j,83)%j+A(j,85)%j+A(j,86)%j+A(j,88)%j+A(j,93)%j+A(j,94)%j+A(j,97)%j+A(j,98)%j+A(j,105)%j+A(j,106)%j &
       +A(j,153)%j+A(j,156)%j+A(j,158)%j+A(j,159)%j+A(j,168)%j+A(j,169)%j+A(j,176)%j+A(j,200)%j+A(j,202)%j+A(j,203)%j+A(j,204)%j &
       +A(j,208)%j+A(j,214)%j+A(j,215)%j+A(j,223)%j+A(j,226)%j+A(j,229)%j+A(j,232)%j+A(j,234)%j+A(j,235)%j+A(j,245)%j+A(j,256)%j &
       +A(j,258)%j+A(j,259)%j+A(j,260)%j+A(j,272)%j+A(j,273)%j+A(j,277)%j+A(j,294)%j+A(j,295)%j+A(j,299)%j+A(j,300)%j+A(j,319)%j &
       +A(j,320)%j+A(j,322)%j+A(j,323)%j)*f(3))/2._/**/REALKIND+((A(j,1)%j+A(j,2)%j+A(j,4)%j+A(j,5)%j+A(j,6)%j+A(j,7)%j+A(j,9)%j &
       +A(j,10)%j+A(j,12)%j+A(j,13)%j+A(j,14)%j+A(j,15)%j+A(j,17)%j+A(j,18)%j+A(j,21)%j+A(j,22)%j+A(j,23)%j+A(j,24)%j+A(j,27)%j &
       +A(j,28)%j+A(j,29)%j+A(j,30)%j+A(j,32)%j+A(j,33)%j+A(j,154)%j+A(j,155)%j+A(j,157)%j+A(j,160)%j+A(j,167)%j+A(j,170)%j &
       +A(j,171)%j+A(j,172)%j+A(j,174)%j+A(j,182)%j+A(j,199)%j+A(j,201)%j+A(j,206)%j+A(j,211)%j+A(j,224)%j+A(j,225)%j+A(j,227)%j &
       +A(j,228)%j+A(j,230)%j+A(j,231)%j+A(j,233)%j+A(j,236)%j+A(j,240)%j+A(j,243)%j+A(j,255)%j+A(j,257)%j+A(j,269)%j+A(j,275)%j &
       +A(j,293)%j+A(j,296)%j+A(j,297)%j+A(j,298)%j+A(j,317)%j+A(j,318)%j+A(j,321)%j+A(j,324)%j)*f(3))/6._/**/REALKIND+(CI*( &
       -A(j,42)%j-A(j,46)%j-A(j,50)%j-A(j,54)%j-A(j,68)%j-A(j,72)%j+A(j,79)%j+A(j,84)%j+A(j,87)%j+A(j,92)%j+A(j,107)%j+A(j,110)%j &
       +A(j,175)%j+A(j,179)%j-A(j,207)%j-A(j,216)%j-A(j,237)%j-A(j,248)%j+A(j,274)%j+A(j,280)%j)*f(4))/2._/**/REALKIND

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
  use ol_colourmatrix_pphlljj_nenexbbbxbxhg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(4)
  real(REALKIND),    intent(out) :: M2colint(0:38-1)
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
  use ol_colourmatrix_pphlljj_nenexbbbxbxhg_1_/**/REALKIND, only: K1
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
  & bind(c,name="ol_tree_colvect_pphlljj_nenexbbbxbxhg_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(4,128)
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
    & bind(c,name="ol_f_amp2tree_pphlljj_nenexbbbxbxhg_1")
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
    & bind(c,name="ol_f_amp2ccone_pphlljj_nenexbbbxbxhg_1")
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
    & bind(c,name="ol_f_amp2ccall_pphlljj_nenexbbbxbxhg_1")
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
    & bind(c,name="ol_f_amp2hcone_pphlljj_nenexbbbxbxhg_1")
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
    & bind(c,name="ol_f_amp2hcall_pphlljj_nenexbbbxbxhg_1")
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
    & bind(c,name="ol_amp2tree_pphlljj_nenexbbbxbxhg_1")
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
    & bind(c,name="ol_amp2ccone_pphlljj_nenexbbbxbxhg_1")
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
    & bind(c,name="ol_amp2ccall_pphlljj_nenexbbbxbxhg_1")
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
    & bind(c,name="ol_amp2hcone_pphlljj_nenexbbbxbxhg_1")
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
    & bind(c,name="ol_amp2hcall_pphlljj_nenexbbbxbxhg_1")
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
    & bind(c,name="amp2tree_pphlljj_nenexbbbxbxhg_1_")
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
    & bind(c,name="amp2ccone_pphlljj_nenexbbbxbxhg_1_")
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
    & bind(c,name="amp2ccall_pphlljj_nenexbbbxbxhg_1_")
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
    & bind(c,name="amp2hcone_pphlljj_nenexbbbxbxhg_1_")
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
    & bind(c,name="amp2hcall_pphlljj_nenexbbbxbxhg_1_")
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

end module ol_tree_pphlljj_nenexbbbxbxhg_1_/**/REALKIND
