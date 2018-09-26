
module ol_colourmatrix_ppllnnjj_vbs_nexnmxemuudxdxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(188,4)
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
  K1( 25,:) = [   0,   0,   0,   0]
  K1( 26,:) = [   0,   0,   0,   0]
  K1( 27,:) = [   0,   0,   0,   0]
  K1( 28,:) = [   0,   0,   0,   0]
  K1( 29,:) = [   0,   0,   0,   0]
  K1( 30,:) = [   0,   0,   0,   0]
  K1( 31,:) = [   0,   0,   0,   0]
  K1( 32,:) = [   0,   0,   0,   0]
  K1( 33,:) = [   0,   0,   0,   0]
  K1( 34,:) = [   0,   0,   0,   0]
  K1( 35,:) = [   0,   0,   0,   0]
  K1( 36,:) = [   0,   0,   0,   0]
  K1( 37,:) = [   0,   0,   0,   0]
  K1( 38,:) = [   0,   0,   0,   0]
  K1( 39,:) = [   0,   0,   0,   0]
  K1( 40,:) = [   0,   0,   0,   0]
  K1( 41,:) = [   0,   0,   0,   0]
  K1( 42,:) = [   0,   0,   0,   0]
  K1( 43,:) = [   0,   0,   0,   0]
  K1( 44,:) = [   0,   0,   0,   0]
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
  K1( 73,:) = [   0,   0,   0,   0]
  K1( 74,:) = [   0,   0,   0,   0]
  K1( 75,:) = [   0,   0,   0,   0]
  K1( 76,:) = [   0,   0,   0,   0]
  K1( 77,:) = [   0,   0,   0,   0]
  K1( 78,:) = [   0,   0,   0,   0]
  K1( 79,:) = [   0,   0,   0,   0]
  K1( 80,:) = [   0,   0,   0,   0]
  K1( 81,:) = [   0,  16,  -2,   6]
  K1( 82,:) = [  16,   0,   6,  -2]
  K1( 83,:) = [  -2,   6,   0,  16]
  K1( 84,:) = [   6,  -2,  16,   0]
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
  K1(105,:) = [   6,   2,   2,   0]
  K1(106,:) = [   2,   0,  -6, -16]
  K1(107,:) = [   2,  -6,   0, -16]
  K1(108,:) = [   0, -16, -16, -48]
  K1(109,:) = [   0,   2, -16,  -6]
  K1(110,:) = [   2,   6,   0,   2]
  K1(111,:) = [ -16,   0, -48, -16]
  K1(112,:) = [  -6,   2, -16,   0]
  K1(113,:) = [  48,  16,  16,   0]
  K1(114,:) = [  16,  48,   0,  16]
  K1(115,:) = [  16,   0,  48,  16]
  K1(116,:) = [   0,  16,  16,  48]
  K1(117,:) = [   0,   0,   0,   0]
  K1(118,:) = [   0,   0,   0,   0]
  K1(119,:) = [   0,   0,   0,   0]
  K1(120,:) = [   0,   0,   0,   0]
  K1(121,:) = [   0,   0,   0,   0]
  K1(122,:) = [   0,   0,   0,   0]
  K1(123,:) = [   0,   0,   0,   0]
  K1(124,:) = [   0,   0,   0,   0]
  K1(125,:) = [   0,   0,   0,   0]
  K1(126,:) = [   0,   0,   0,   0]
  K1(127,:) = [   0,   0,   0,   0]
  K1(128,:) = [   0,   0,   0,   0]
  K1(129,:) = [   0,   0,   0,   0]
  K1(130,:) = [   0,   0,   0,   0]
  K1(131,:) = [   0,   0,   0,   0]
  K1(132,:) = [   0,   0,   0,   0]
  K1(133,:) = [   0, -16,   2,  -6]
  K1(134,:) = [ -16, -48,   0, -16]
  K1(135,:) = [   2,   0,   6,   2]
  K1(136,:) = [  -6, -16,   2,   0]
  K1(137,:) = [ -48, -16, -16,   0]
  K1(138,:) = [ -16,   0,  -6,   2]
  K1(139,:) = [ -16,  -6,   0,   2]
  K1(140,:) = [   0,   2,   2,   6]
  K1(141,:) = [   0,  -2,  16,   6]
  K1(142,:) = [  -2,   0,   6,  16]
  K1(143,:) = [  16,   6,   0,  -2]
  K1(144,:) = [   6,  16,  -2,   0]
  K1(145,:) = [  48,  16,  16,   0]
  K1(146,:) = [  16,  48,   0,  16]
  K1(147,:) = [  16,   0,  48,  16]
  K1(148,:) = [   0,  16,  16,  48]
  K1(149,:) = [   0,   0,   0,   0]
  K1(150,:) = [   0,   0,   0,   0]
  K1(151,:) = [   0,   0,   0,   0]
  K1(152,:) = [   0,   0,   0,   0]
  K1(153,:) = [   0,   0,   0,   0]
  K1(154,:) = [   0,   0,   0,   0]
  K1(155,:) = [   0,   0,   0,   0]
  K1(156,:) = [   0,   0,   0,   0]
  K1(157,:) = [   0,   0,   0,   0]
  K1(158,:) = [   0,   0,   0,   0]
  K1(159,:) = [   0,   0,   0,   0]
  K1(160,:) = [   0,   0,   0,   0]
  K1(161,:) = [   0,   0,   0,   0]
  K1(162,:) = [   0,   0,   0,   0]
  K1(163,:) = [   0,   0,   0,   0]
  K1(164,:) = [   0,   0,   0,   0]
  K1(165,:) = [ -54, -18, -18,   0]
  K1(166,:) = [ -18,   0,   0,  18]
  K1(167,:) = [ -18,   0, -54, -18]
  K1(168,:) = [   0,  18, -18,   0]
  K1(169,:) = [   0, -18,  18,   0]
  K1(170,:) = [ -18, -54,   0, -18]
  K1(171,:) = [  18,   0,   0, -18]
  K1(172,:) = [   0, -18, -18, -54]
  K1(173,:) = [ -54, -18, -18,   0]
  K1(174,:) = [ -18, -54,   0, -18]
  K1(175,:) = [ -18,   0,   0,  18]
  K1(176,:) = [   0, -18,  18,   0]
  K1(177,:) = [   0,  18, -18,   0]
  K1(178,:) = [  18,   0,   0, -18]
  K1(179,:) = [ -18,   0, -54, -18]
  K1(180,:) = [   0, -18, -18, -54]
  K1(181,:) = [ 108,  36,  36,   0]
  K1(182,:) = [  36, 108,   0,  36]
  K1(183,:) = [  36,   0, 108,  36]
  K1(184,:) = [   0,  36,  36, 108]
  K1(185,:) = [   0,   0,   0,   0]
  K1(186,:) = [   0,   0,   0,   0]
  K1(187,:) = [   0,   0,   0,   0]
  K1(188,:) = [   0,   0,   0,   0]
  K1 = (1._/**/REALKIND / 3) * K1

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllnnjj_vbs_nexnmxemuudxdxg_1_/**/REALKIND



module ol_forced_parameters_ppllnnjj_vbs_nexnmxemuudxdxg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllnnjj_vbs_nexnmxemuudxdxg_1_/**/REALKIND

module ol_tree_ppllnnjj_vbs_nexnmxemuudxdxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(2)
  complex(REALKIND), save :: den(226)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 512 ! number of helicity configurations
  integer(intkind2), save :: nhel = 512 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(512) ! physical helicity states
  complex(DREALKIND) :: M1helarr(4,512) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = (CI*eQED**4*gQCD**3)/(4._/**/REALKIND*sw**4)
    f(2) = (eQED**4*gQCD**3)/(sw**4*4._/**/REALKIND)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,5) - MW2)
  den(2) = 1 / (Q(5,10) - MW2)
  den(3) = 1 / (Q(5,37))
  den(5) = 1 / (Q(5,272))
  den(6) = 1 / (Q(5,74))
  den(9) = 1 / (Q(5,165))
  den(12) = 1 / (Q(5,138))
  den(15) = 1 / (Q(5,101))
  den(19) = 1 / (Q(5,282))
  den(22) = 1 / (Q(5,346))
  den(25) = 1 / (Q(5,42))
  den(27) = 1 / (Q(5,69))
  den(30) = 1 / (Q(5,170))
  den(38) = 1 / (Q(5,133))
  den(41) = 1 / (Q(5,106))
  den(50) = 1 / (Q(5,277))
  den(53) = 1 / (Q(5,341))
  den(58) = 1 / (Q(5,21))
  den(60) = 1 / (Q(5,288))
  den(62) = 1 / (Q(5,149))
  den(66) = 1 / (Q(5,85))
  den(70) = 1 / (Q(5,298))
  den(73) = 1 / (Q(5,362))
  den(76) = 1 / (Q(5,26))
  den(79) = 1 / (Q(5,154))
  den(88) = 1 / (Q(5,90))
  den(97) = 1 / (Q(5,293))
  den(100) = 1 / (Q(5,357))
  den(105) = 1 / (Q(5,320))
  den(112) = 1 / (Q(5,330))
  den(128) = 1 / (Q(5,325))
  den(136) = 1 / (Q(5,384))
  den(143) = 1 / (Q(5,394))
  den(145) = 1 / (Q(5,426))
  den(153) = 1 / (Q(5,410))
  den(161) = 1 / (Q(5,389))
  den(163) = 1 / (Q(5,421))
  den(166) = 1 / (Q(5,405))

  ! denominators

  den(4) = den(1)*den(3)
  den(7) = den(2)*den(6)
  den(8) = den(5)*den(7)
  den(10) = den(4)*den(9)
  den(11) = den(8)*den(10)
  den(13) = den(2)*den(12)
  den(14) = den(5)*den(13)
  den(16) = den(4)*den(15)
  den(17) = den(14)*den(16)
  den(18) = den(2)*den(5)
  den(20) = den(18)*den(19)
  den(21) = den(16)*den(20)
  den(23) = den(20)*den(22)
  den(24) = den(4)*den(23)
  den(26) = den(2)*den(25)
  den(28) = den(1)*den(27)
  den(29) = den(5)*den(28)
  den(31) = den(26)*den(30)
  den(32) = den(29)*den(31)
  den(33) = den(15)*den(28)
  den(34) = den(14)*den(33)
  den(35) = den(13)*den(30)
  den(36) = den(29)*den(35)
  den(37) = den(20)*den(33)
  den(39) = den(1)*den(38)
  den(40) = den(5)*den(39)
  den(42) = den(26)*den(41)
  den(43) = den(40)*den(42)
  den(44) = den(7)*den(41)
  den(45) = den(40)*den(44)
  den(46) = den(9)*den(39)
  den(47) = den(8)*den(46)
  den(48) = den(20)*den(46)
  den(49) = den(1)*den(5)
  den(51) = den(49)*den(50)
  den(52) = den(42)*den(51)
  den(54) = den(51)*den(53)
  den(55) = den(26)*den(54)
  den(56) = den(44)*den(51)
  den(57) = den(35)*den(51)
  den(59) = den(1)*den(58)
  den(61) = den(7)*den(60)
  den(63) = den(59)*den(62)
  den(64) = den(61)*den(63)
  den(65) = den(13)*den(60)
  den(67) = den(59)*den(66)
  den(68) = den(65)*den(67)
  den(69) = den(2)*den(60)
  den(71) = den(69)*den(70)
  den(72) = den(67)*den(71)
  den(74) = den(71)*den(73)
  den(75) = den(59)*den(74)
  den(77) = den(2)*den(76)
  den(78) = den(28)*den(60)
  den(80) = den(77)*den(79)
  den(81) = den(78)*den(80)
  den(82) = den(28)*den(66)
  den(83) = den(65)*den(82)
  den(84) = den(13)*den(79)
  den(85) = den(78)*den(84)
  den(86) = den(71)*den(82)
  den(87) = den(39)*den(60)
  den(89) = den(77)*den(88)
  den(90) = den(87)*den(89)
  den(91) = den(7)*den(88)
  den(92) = den(87)*den(91)
  den(93) = den(39)*den(62)
  den(94) = den(61)*den(93)
  den(95) = den(71)*den(93)
  den(96) = den(1)*den(60)
  den(98) = den(96)*den(97)
  den(99) = den(89)*den(98)
  den(101) = den(98)*den(100)
  den(102) = den(77)*den(101)
  den(103) = den(91)*den(98)
  den(104) = den(84)*den(98)
  den(106) = den(26)*den(105)
  den(107) = den(63)*den(106)
  den(108) = den(59)*den(105)
  den(109) = den(31)*den(108)
  den(110) = den(35)*den(108)
  den(111) = den(2)*den(105)
  den(113) = den(111)*den(112)
  den(114) = den(73)*den(113)
  den(115) = den(59)*den(114)
  den(116) = den(4)*den(105)
  den(117) = den(80)*den(116)
  den(118) = den(77)*den(105)
  den(119) = den(10)*den(118)
  den(120) = den(84)*den(116)
  den(121) = den(22)*den(113)
  den(122) = den(4)*den(121)
  den(123) = den(46)*den(118)
  den(124) = den(93)*den(106)
  den(125) = den(93)*den(113)
  den(126) = den(39)*den(121)
  den(127) = den(1)*den(105)
  den(129) = den(127)*den(128)
  den(130) = den(100)*den(129)
  den(131) = den(77)*den(130)
  den(132) = den(53)*den(129)
  den(133) = den(26)*den(132)
  den(134) = den(84)*den(129)
  den(135) = den(13)*den(132)
  den(137) = den(26)*den(136)
  den(138) = den(67)*den(137)
  den(139) = den(59)*den(136)
  den(140) = den(42)*den(139)
  den(141) = den(44)*den(139)
  den(142) = den(2)*den(136)
  den(144) = den(142)*den(143)
  den(146) = den(144)*den(145)
  den(147) = den(59)*den(146)
  den(148) = den(4)*den(136)
  den(149) = den(89)*den(148)
  den(150) = den(77)*den(136)
  den(151) = den(16)*den(150)
  den(152) = den(91)*den(148)
  den(154) = den(144)*den(153)
  den(155) = den(4)*den(154)
  den(156) = den(33)*den(150)
  den(157) = den(82)*den(137)
  den(158) = den(82)*den(144)
  den(159) = den(28)*den(154)
  den(160) = den(1)*den(136)
  den(162) = den(160)*den(161)
  den(164) = den(162)*den(163)
  den(165) = den(77)*den(164)
  den(167) = den(162)*den(166)
  den(168) = den(26)*den(167)
  den(169) = den(91)*den(162)
  den(170) = den(7)*den(167)
  den(171) = den(31)*den(67)
  den(172) = den(26)*den(70)
  den(173) = den(67)*den(172)
  den(174) = den(42)*den(63)
  den(175) = den(63)*den(172)
  den(176) = den(50)*den(59)
  den(177) = den(42)*den(176)
  den(178) = den(31)*den(176)
  den(179) = den(44)*den(63)
  den(180) = den(7)*den(112)
  den(181) = den(63)*den(180)
  den(182) = den(44)*den(176)
  den(183) = den(13)*den(143)
  den(184) = den(67)*den(183)
  den(185) = den(35)*den(67)
  den(186) = den(35)*den(176)
  den(187) = den(10)*den(89)
  den(188) = den(4)*den(97)
  den(189) = den(89)*den(188)
  den(190) = den(16)*den(80)
  den(191) = den(80)*den(188)
  den(192) = den(19)*den(77)
  den(193) = den(16)*den(192)
  den(194) = den(10)*den(192)
  den(195) = den(91)*den(188)
  den(196) = den(10)*den(91)
  den(197) = den(10)*den(180)
  den(198) = den(84)*den(188)
  den(199) = den(16)*den(183)
  den(200) = den(16)*den(84)
  den(201) = den(33)*den(80)
  den(202) = den(28)*den(128)
  den(203) = den(80)*den(202)
  den(204) = den(33)*den(192)
  den(205) = den(82)*den(172)
  den(206) = den(31)*den(82)
  den(207) = den(31)*den(202)
  den(208) = den(82)*den(183)
  den(209) = den(84)*den(202)
  den(210) = den(33)*den(183)
  den(211) = den(35)*den(202)
  den(212) = den(35)*den(82)
  den(213) = den(33)*den(84)
  den(214) = den(39)*den(161)
  den(215) = den(89)*den(214)
  den(216) = den(46)*den(89)
  den(217) = den(46)*den(192)
  den(218) = den(93)*den(172)
  den(219) = den(42)*den(214)
  den(220) = den(42)*den(93)
  den(221) = den(91)*den(214)
  den(222) = den(93)*den(180)
  den(223) = den(44)*den(214)
  den(224) = den(46)*den(180)
  den(225) = den(46)*den(91)
  den(226) = den(44)*den(93)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_ppllnnjj_vbs_nexnmxemuudxdxg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_ppllnnjj_vbs_nexnmxemuudxdxg_1_/**/DREALKIND, only: &
    & ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for anti-nu_e anti-nu_mu e- mu- up up anti-down anti-down glue -> 0
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
  use ol_external_ppllnnjj_vbs_nexnmxemuudxdxg_1, only: &
    & external_perm_ppllnnjj_vbs_nexnmxemuudxdxg_1, &
    & external_perm_inv_ppllnnjj_vbs_nexnmxemuudxdxg_1, &
    & extcomb_perm_ppllnnjj_vbs_nexnmxemuudxdxg_1, &
    & average_factor_ppllnnjj_vbs_nexnmxemuudxdxg_1
  use ol_external_ppllnnjj_vbs_nexnmxemuudxdxg_1, only: &
    & H, hel_not_initialised, hel_init, POLSEL
  use ol_colourmatrix_ppllnnjj_vbs_nexnmxemuudxdxg_1_/**/REALKIND, only: &
    & colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_ppllnnjj_vbs_nexnmxemuudxdxg_1_/**/REALKIND, only: &
    & check_forced_parameters
  use ol_heltables_ppllnnjj_vbs_nexnmxemuudxdxg_1
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
  complex(REALKIND) :: MOM_LC(4), M1(4), M1helarray(4,512)
  real(REALKIND)    :: P_scatt_intern(0:3,9)
  complex(REALKIND) :: epLC(1:4), epStd(1:4,2)
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(2), ex6(2), ex7(2), ex8(2), ex9(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,12), wf8(8,16), wf16(16,48), wf32(32,76), wf512(512,112)

  type(polcont) :: A(512,112)
  complex(REALKIND) :: Aj(112)

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
    & external_perm_inv_ppllnnjj_vbs_nexnmxemuudxdxg_1,9)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,9)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_ppllnnjj_vbs_nexnmxemuudxdxg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_ppllnnjj_vbs_nexnmxemuudxdxg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_ppllnnjj_vbs_nexnmxemuudxdxg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  42 continue
  ! external WFs
  call pol_wf_A(P(:,1), rZERO, H1, ex1, POLSEL(1))
  call pol_wf_A(P(:,2), rZERO, H2, ex2, POLSEL(2))
  call pol_wf_Q(P(:,3), rZERO, H3, ex3, POLSEL(3))
  call pol_wf_Q(P(:,4), rZERO, H4, ex4, POLSEL(4))
  call pol_wf_Q(P(:,5), rZERO, H5, ex5, POLSEL(5))
  call pol_wf_Q(P(:,6), rZERO, H6, ex6, POLSEL(6))
  call pol_wf_A(P(:,7), rZERO, H7, ex7, POLSEL(7))
  call pol_wf_A(P(:,8), rZERO, H8, ex8, POLSEL(8))
  call pol_wf_V(P(:,9), rZERO, H9, ex9, POLSEL(9))


  if (ntry == 1) then
    shift = 1
    ! call helbookkeeping_flip(H1, 1, shift, eflip, exthel, firstpol)
    ! call helbookkeeping_wf(H1, ex1, shift) ...

    if (any(POLSEL /= 0)) then

      call pol_wf_A(P(:,1), rZERO, H1, ex1, 0)
      call pol_wf_A(P(:,2), rZERO, H2, ex2, 0)
      call pol_wf_Q(P(:,3), rZERO, H3, ex3, 0)
      call pol_wf_Q(P(:,4), rZERO, H4, ex4, 0)
      call pol_wf_Q(P(:,5), rZERO, H5, ex5, 0)
      call pol_wf_Q(P(:,6), rZERO, H6, ex6, 0)
      call pol_wf_A(P(:,7), rZERO, H7, ex7, 0)
      call pol_wf_A(P(:,8), rZERO, H8, ex8, 0)
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
  call vert_QA_W(ntry, ex3, ex1, wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_QA_W(ntry, ex4, ex2, wf4(:,2), n3(:,2), t3x4(:,:,2))
  call vert_VQ_A(ntry, ex9, ex5, wf4(:,3), n3(:,3), t3x4(:,:,3))
  call prop_W_W(ntry, wf4(:,1), Q(:,5), MW, 1_intkind1, wf4(:,4), n2(1))
  call prop_W_W(ntry, wf4(:,2), Q(:,10), MW, 1_intkind1, wf4(:,5), n2(2))
  call vert_WQ_A(ntry, wf4(:,4), ex6, wf8(:,1), n3(:,4), t3x8(:,:,1))
  call vert_AW_Q(ntry, ex7, wf4(:,5), wf8(:,2), n3(:,5), t3x8(:,:,2))
  call prop_Q_A(ntry, wf8(:,1), Q(:,37), ZERO, 0_intkind1, wf8(:,3), n2(3))
  call prop_Q_A(ntry, wf4(:,3), Q(:,272), ZERO, 0_intkind1, wf4(:,6), n2(4))
  call prop_A_Q(ntry, wf8(:,2), Q(:,74), ZERO, 0_intkind1, wf8(:,4), n2(5))
  call vert_QA_V(ntry, wf8(:,3), ex8, wf16(:,1), n3(:,6), t3x16(:,:,1))
  call vert_QA_V(ntry, wf4(:,6), wf8(:,4), wf32(:,1), n3(:,7), t3x32(:,:,1))
  call vert_AW_Q(ntry, ex8, wf4(:,5), wf8(:,5), n3(:,8), t3x8(:,:,3))
  call prop_A_Q(ntry, wf8(:,5), Q(:,138), ZERO, 0_intkind1, wf8(:,6), n2(6))
  call vert_QA_V(ntry, wf8(:,3), ex7, wf16(:,2), n3(:,9), t3x16(:,:,2))
  call vert_QA_V(ntry, wf4(:,6), wf8(:,6), wf32(:,2), n3(:,10), t3x32(:,:,2))
  call vert_WQ_A(ntry, wf4(:,5), wf4(:,6), wf16(:,3), n3(:,11), t3x16(:,:,3))
  call prop_Q_A(ntry, wf16(:,3), Q(:,282), ZERO, 0_intkind1, wf16(:,4), n2(7))
  call vert_QA_V(ntry, wf16(:,4), ex8, wf32(:,3), n3(:,12), t3x32(:,:,3))
  call vert_QA_V(ntry, wf16(:,4), ex7, wf32(:,4), n3(:,13), t3x32(:,:,4))
  call vert_WQ_A(ntry, wf4(:,5), ex6, wf8(:,7), n3(:,14), t3x8(:,:,4))
  call vert_AW_Q(ntry, ex7, wf4(:,4), wf8(:,8), n3(:,15), t3x8(:,:,5))
  call prop_Q_A(ntry, wf8(:,7), Q(:,42), ZERO, 0_intkind1, wf8(:,9), n2(8))
  call prop_A_Q(ntry, wf8(:,8), Q(:,69), ZERO, 0_intkind1, wf8(:,10), n2(9))
  call vert_QA_V(ntry, wf8(:,9), ex8, wf16(:,5), n3(:,16), t3x16(:,:,4))
  call vert_QA_V(ntry, wf4(:,6), wf8(:,10), wf32(:,5), n3(:,17), t3x32(:,:,5))
  call vert_QA_V(ntry, ex6, wf8(:,10), wf16(:,6), n3(:,18), t3x16(:,:,5))
  call vert_QA_V(ntry, ex6, wf8(:,6), wf16(:,7), n3(:,19), t3x16(:,:,6))
  call vert_AW_Q(ntry, ex8, wf4(:,4), wf8(:,11), n3(:,20), t3x8(:,:,6))
  call prop_A_Q(ntry, wf8(:,11), Q(:,133), ZERO, 0_intkind1, wf8(:,12), n2(10))
  call vert_QA_V(ntry, wf8(:,9), ex7, wf16(:,8), n3(:,21), t3x16(:,:,7))
  call vert_QA_V(ntry, wf4(:,6), wf8(:,12), wf32(:,6), n3(:,22), t3x32(:,:,6))
  call vert_QA_V(ntry, ex6, wf8(:,4), wf16(:,9), n3(:,23), t3x16(:,:,8))
  call vert_QA_V(ntry, ex6, wf8(:,12), wf16(:,10), n3(:,24), t3x16(:,:,9))
  call vert_WQ_A(ntry, wf4(:,4), wf4(:,6), wf16(:,11), n3(:,25), t3x16(:,:,10))
  call prop_Q_A(ntry, wf16(:,11), Q(:,277), ZERO, 0_intkind1, wf16(:,12), n2(11))
  call vert_QA_V(ntry, wf16(:,12), ex8, wf32(:,7), n3(:,26), t3x32(:,:,7))
  call vert_QA_V(ntry, wf16(:,12), ex7, wf32(:,8), n3(:,27), t3x32(:,:,8))
  call vert_VQ_A(ntry, ex9, ex6, wf4(:,7), n3(:,28), t3x4(:,:,4))
  call vert_WQ_A(ntry, wf4(:,4), ex5, wf8(:,13), n3(:,29), t3x8(:,:,7))
  call prop_Q_A(ntry, wf8(:,13), Q(:,21), ZERO, 0_intkind1, wf8(:,14), n2(12))
  call prop_Q_A(ntry, wf4(:,7), Q(:,288), ZERO, 0_intkind1, wf4(:,8), n2(13))
  call vert_QA_V(ntry, wf8(:,14), ex8, wf16(:,13), n3(:,30), t3x16(:,:,11))
  call vert_QA_V(ntry, wf4(:,8), wf8(:,4), wf32(:,9), n3(:,31), t3x32(:,:,9))
  call vert_QA_V(ntry, wf8(:,14), ex7, wf16(:,14), n3(:,32), t3x16(:,:,12))
  call vert_QA_V(ntry, wf4(:,8), wf8(:,6), wf32(:,10), n3(:,33), t3x32(:,:,10))
  call vert_WQ_A(ntry, wf4(:,5), wf4(:,8), wf16(:,15), n3(:,34), t3x16(:,:,13))
  call prop_Q_A(ntry, wf16(:,15), Q(:,298), ZERO, 0_intkind1, wf16(:,16), n2(14))
  call vert_QA_V(ntry, wf16(:,16), ex8, wf32(:,11), n3(:,35), t3x32(:,:,11))
  call vert_QA_V(ntry, wf16(:,16), ex7, wf32(:,12), n3(:,36), t3x32(:,:,12))
  call vert_WQ_A(ntry, wf4(:,5), ex5, wf8(:,15), n3(:,37), t3x8(:,:,8))
  call prop_Q_A(ntry, wf8(:,15), Q(:,26), ZERO, 0_intkind1, wf8(:,16), n2(15))
  call vert_QA_V(ntry, wf8(:,16), ex8, wf16(:,17), n3(:,38), t3x16(:,:,14))
  call vert_QA_V(ntry, wf4(:,8), wf8(:,10), wf32(:,13), n3(:,39), t3x32(:,:,13))
  call vert_QA_V(ntry, ex5, wf8(:,10), wf16(:,18), n3(:,40), t3x16(:,:,15))
  call vert_QA_V(ntry, ex5, wf8(:,6), wf16(:,19), n3(:,41), t3x16(:,:,16))
  call vert_QA_V(ntry, wf8(:,16), ex7, wf16(:,20), n3(:,42), t3x16(:,:,17))
  call vert_QA_V(ntry, wf4(:,8), wf8(:,12), wf32(:,14), n3(:,43), t3x32(:,:,14))
  call vert_QA_V(ntry, ex5, wf8(:,4), wf16(:,21), n3(:,44), t3x16(:,:,18))
  call vert_QA_V(ntry, ex5, wf8(:,12), wf16(:,22), n3(:,45), t3x16(:,:,19))
  call vert_WQ_A(ntry, wf4(:,4), wf4(:,8), wf16(:,23), n3(:,46), t3x16(:,:,20))
  call prop_Q_A(ntry, wf16(:,23), Q(:,293), ZERO, 0_intkind1, wf16(:,24), n2(16))
  call vert_QA_V(ntry, wf16(:,24), ex8, wf32(:,15), n3(:,47), t3x32(:,:,15))
  call vert_QA_V(ntry, wf16(:,24), ex7, wf32(:,16), n3(:,48), t3x32(:,:,16))
  call vert_AV_Q(ntry, ex7, ex9, wf4(:,9), n3(:,49), t3x4(:,:,5))
  call prop_A_Q(ntry, wf4(:,9), Q(:,320), ZERO, 0_intkind1, wf4(:,10), n2(17))
  call vert_QA_V(ntry, wf8(:,9), wf4(:,10), wf32(:,17), n3(:,50), t3x32(:,:,17))
  call vert_QA_V(ntry, wf8(:,14), wf4(:,10), wf32(:,18), n3(:,51), t3x32(:,:,18))
  call vert_AW_Q(ntry, wf4(:,10), wf4(:,5), wf16(:,25), n3(:,52), t3x16(:,:,21))
  call prop_A_Q(ntry, wf16(:,25), Q(:,330), ZERO, 0_intkind1, wf16(:,26), n2(18))
  call vert_QA_V(ntry, ex6, wf16(:,26), wf32(:,19), n3(:,53), t3x32(:,:,19))
  call vert_QA_V(ntry, wf8(:,3), wf4(:,10), wf32(:,20), n3(:,54), t3x32(:,:,20))
  call vert_QA_V(ntry, wf8(:,16), wf4(:,10), wf32(:,21), n3(:,55), t3x32(:,:,21))
  call vert_QA_V(ntry, ex5, wf16(:,26), wf32(:,22), n3(:,56), t3x32(:,:,22))
  call vert_AW_Q(ntry, wf4(:,10), wf4(:,4), wf16(:,27), n3(:,57), t3x16(:,:,22))
  call prop_A_Q(ntry, wf16(:,27), Q(:,325), ZERO, 0_intkind1, wf16(:,28), n2(19))
  call vert_QA_V(ntry, ex6, wf16(:,28), wf32(:,23), n3(:,58), t3x32(:,:,23))
  call vert_QA_V(ntry, ex5, wf16(:,28), wf32(:,24), n3(:,59), t3x32(:,:,24))
  call vert_AV_Q(ntry, ex8, ex9, wf4(:,11), n3(:,60), t3x4(:,:,6))
  call prop_A_Q(ntry, wf4(:,11), Q(:,384), ZERO, 0_intkind1, wf4(:,12), n2(20))
  call vert_QA_V(ntry, wf8(:,9), wf4(:,12), wf32(:,25), n3(:,61), t3x32(:,:,25))
  call vert_QA_V(ntry, wf8(:,14), wf4(:,12), wf32(:,26), n3(:,62), t3x32(:,:,26))
  call vert_AW_Q(ntry, wf4(:,12), wf4(:,5), wf16(:,29), n3(:,63), t3x16(:,:,23))
  call prop_A_Q(ntry, wf16(:,29), Q(:,394), ZERO, 0_intkind1, wf16(:,30), n2(21))
  call vert_QA_V(ntry, ex6, wf16(:,30), wf32(:,27), n3(:,64), t3x32(:,:,27))
  call vert_QA_V(ntry, wf8(:,3), wf4(:,12), wf32(:,28), n3(:,65), t3x32(:,:,28))
  call vert_QA_V(ntry, wf8(:,16), wf4(:,12), wf32(:,29), n3(:,66), t3x32(:,:,29))
  call vert_QA_V(ntry, ex5, wf16(:,30), wf32(:,30), n3(:,67), t3x32(:,:,30))
  call vert_AW_Q(ntry, wf4(:,12), wf4(:,4), wf16(:,31), n3(:,68), t3x16(:,:,24))
  call prop_A_Q(ntry, wf16(:,31), Q(:,389), ZERO, 0_intkind1, wf16(:,32), n2(22))
  call vert_QA_V(ntry, ex6, wf16(:,32), wf32(:,31), n3(:,69), t3x32(:,:,31))
  call vert_QA_V(ntry, ex5, wf16(:,32), wf32(:,32), n3(:,70), t3x32(:,:,32))
  call vert_UV_W(ntry, wf16(:,14), Q(:,85), ex9, Q(:,256), wf32(:,33), n3(:,71), t3x32(:,:,33))
  call vert_VQ_A(ntry, ex9, wf8(:,9), wf16(:,33), n3(:,72), t3x16(:,:,25))
  call prop_Q_A(ntry, wf16(:,33), Q(:,298), ZERO, 0_intkind1, wf16(:,34), n2(23))
  call vert_AV_Q(ntry, ex8, wf16(:,14), wf32(:,34), n3(:,73), t3x32(:,:,34))
  call vert_UV_W(ntry, wf16(:,8), Q(:,106), ex9, Q(:,256), wf32(:,35), n3(:,74), t3x32(:,:,35))
  call vert_AV_Q(ntry, ex7, wf16(:,13), wf32(:,36), n3(:,75), t3x32(:,:,36))
  call vert_VQ_A(ntry, ex9, wf8(:,14), wf16(:,35), n3(:,76), t3x16(:,:,26))
  call prop_Q_A(ntry, wf16(:,35), Q(:,277), ZERO, 0_intkind1, wf16(:,36), n2(24))
  call vert_AV_Q(ntry, ex8, wf16(:,8), wf32(:,37), n3(:,77), t3x32(:,:,37))
  call vert_AV_Q(ntry, ex7, wf16(:,5), wf32(:,38), n3(:,78), t3x32(:,:,38))
  call vert_UV_W(ntry, wf16(:,9), Q(:,106), ex9, Q(:,256), wf32(:,39), n3(:,79), t3x32(:,:,39))
  call vert_AV_Q(ntry, wf8(:,4), ex9, wf16(:,37), n3(:,80), t3x16(:,:,27))
  call prop_A_Q(ntry, wf16(:,37), Q(:,330), ZERO, 0_intkind1, wf16(:,38), n2(25))
  call vert_VQ_A(ntry, wf16(:,13), ex6, wf32(:,40), n3(:,81), t3x32(:,:,40))
  call vert_AV_Q(ntry, ex8, wf16(:,9), wf32(:,41), n3(:,82), t3x32(:,:,41))
  call vert_AV_Q(ntry, wf8(:,6), ex9, wf16(:,39), n3(:,83), t3x16(:,:,28))
  call prop_A_Q(ntry, wf16(:,39), Q(:,394), ZERO, 0_intkind1, wf16(:,40), n2(26))
  call vert_VQ_A(ntry, wf16(:,14), ex6, wf32(:,42), n3(:,84), t3x32(:,:,42))
  call vert_UV_W(ntry, wf16(:,7), Q(:,170), ex9, Q(:,256), wf32(:,43), n3(:,85), t3x32(:,:,43))
  call vert_AV_Q(ntry, ex7, wf16(:,7), wf32(:,44), n3(:,86), t3x32(:,:,44))
  call vert_UV_W(ntry, wf16(:,20), Q(:,90), ex9, Q(:,256), wf32(:,45), n3(:,87), t3x32(:,:,45))
  call vert_VQ_A(ntry, ex9, wf8(:,3), wf16(:,41), n3(:,88), t3x16(:,:,29))
  call prop_Q_A(ntry, wf16(:,41), Q(:,293), ZERO, 0_intkind1, wf16(:,42), n2(27))
  call vert_AV_Q(ntry, ex8, wf16(:,20), wf32(:,46), n3(:,89), t3x32(:,:,46))
  call vert_UV_W(ntry, wf16(:,2), Q(:,101), ex9, Q(:,256), wf32(:,47), n3(:,90), t3x32(:,:,47))
  call vert_AV_Q(ntry, ex7, wf16(:,17), wf32(:,48), n3(:,91), t3x32(:,:,48))
  call vert_VQ_A(ntry, ex9, wf8(:,16), wf16(:,43), n3(:,92), t3x16(:,:,30))
  call prop_Q_A(ntry, wf16(:,43), Q(:,282), ZERO, 0_intkind1, wf16(:,44), n2(28))
  call vert_AV_Q(ntry, ex8, wf16(:,2), wf32(:,49), n3(:,93), t3x32(:,:,49))
  call vert_AV_Q(ntry, ex7, wf16(:,1), wf32(:,50), n3(:,94), t3x32(:,:,50))
  call vert_AV_Q(ntry, ex8, wf16(:,21), wf32(:,51), n3(:,95), t3x32(:,:,51))
  call vert_UV_W(ntry, wf16(:,21), Q(:,90), ex9, Q(:,256), wf32(:,52), n3(:,96), t3x32(:,:,52))
  call vert_VQ_A(ntry, wf16(:,1), ex5, wf32(:,53), n3(:,97), t3x32(:,:,53))
  call vert_AV_Q(ntry, ex7, wf16(:,19), wf32(:,54), n3(:,98), t3x32(:,:,54))
  call vert_VQ_A(ntry, wf16(:,2), ex5, wf32(:,55), n3(:,99), t3x32(:,:,55))
  call vert_UV_W(ntry, wf16(:,19), Q(:,154), ex9, Q(:,256), wf32(:,56), n3(:,100), t3x32(:,:,56))
  call vert_UV_W(ntry, wf16(:,6), Q(:,101), ex9, Q(:,256), wf32(:,57), n3(:,101), t3x32(:,:,57))
  call vert_AV_Q(ntry, wf8(:,10), ex9, wf16(:,45), n3(:,102), t3x16(:,:,31))
  call prop_A_Q(ntry, wf16(:,45), Q(:,325), ZERO, 0_intkind1, wf16(:,46), n2(29))
  call vert_VQ_A(ntry, wf16(:,17), ex6, wf32(:,58), n3(:,103), t3x32(:,:,58))
  call vert_AV_Q(ntry, ex8, wf16(:,6), wf32(:,59), n3(:,104), t3x32(:,:,59))
  call vert_AV_Q(ntry, ex8, wf16(:,18), wf32(:,60), n3(:,105), t3x32(:,:,60))
  call vert_UV_W(ntry, wf16(:,18), Q(:,85), ex9, Q(:,256), wf32(:,61), n3(:,106), t3x32(:,:,61))
  call vert_VQ_A(ntry, wf16(:,5), ex5, wf32(:,62), n3(:,107), t3x32(:,:,62))
  call vert_VQ_A(ntry, wf16(:,18), ex6, wf32(:,63), n3(:,108), t3x32(:,:,63))
  call vert_VQ_A(ntry, wf16(:,19), ex6, wf32(:,64), n3(:,109), t3x32(:,:,64))
  call vert_VQ_A(ntry, wf16(:,6), ex5, wf32(:,65), n3(:,110), t3x32(:,:,65))
  call vert_VQ_A(ntry, wf16(:,7), ex5, wf32(:,66), n3(:,111), t3x32(:,:,66))
  call vert_AV_Q(ntry, wf8(:,12), ex9, wf16(:,47), n3(:,112), t3x16(:,:,32))
  call prop_A_Q(ntry, wf16(:,47), Q(:,389), ZERO, 0_intkind1, wf16(:,48), n2(30))
  call vert_VQ_A(ntry, wf16(:,20), ex6, wf32(:,67), n3(:,113), t3x32(:,:,67))
  call vert_UV_W(ntry, wf16(:,10), Q(:,165), ex9, Q(:,256), wf32(:,68), n3(:,114), t3x32(:,:,68))
  call vert_AV_Q(ntry, ex7, wf16(:,10), wf32(:,69), n3(:,115), t3x32(:,:,69))
  call vert_AV_Q(ntry, ex7, wf16(:,22), wf32(:,70), n3(:,116), t3x32(:,:,70))
  call vert_VQ_A(ntry, wf16(:,8), ex5, wf32(:,71), n3(:,117), t3x32(:,:,71))
  call vert_UV_W(ntry, wf16(:,22), Q(:,149), ex9, Q(:,256), wf32(:,72), n3(:,118), t3x32(:,:,72))
  call vert_VQ_A(ntry, wf16(:,21), ex6, wf32(:,73), n3(:,119), t3x32(:,:,73))
  call vert_VQ_A(ntry, wf16(:,22), ex6, wf32(:,74), n3(:,120), t3x32(:,:,74))
  call vert_VQ_A(ntry, wf16(:,9), ex5, wf32(:,75), n3(:,121), t3x32(:,:,75))
  call vert_VQ_A(ntry, wf16(:,10), ex5, wf32(:,76), n3(:,122), t3x32(:,:,76))


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

    M2munu = M2munu / average_factor_ppllnnjj_vbs_nexnmxemuudxdxg_1
  end if

  ! Colour and helicity average and symmetry factor of outgoing particles
  M2add = M2 / average_factor_ppllnnjj_vbs_nexnmxemuudxdxg_1

  do k = 0, 47-1
    M2(k) = M2add(extcomb_perm_ppllnnjj_vbs_nexnmxemuudxdxg_1(k))
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

    call cont_VV(nsync, wf16(:,1), wf32(:,1), A(:,1), n3(:,123), t3x512(:,:,1), nhel, den(11))
    call cont_VV(nsync, wf16(:,2), wf32(:,2), A(:,2), n3(:,124), t3x512(:,:,2), nhel, den(17))
    call cont_VV(nsync, wf16(:,2), wf32(:,3), A(:,3), n3(:,125), t3x512(:,:,3), nhel, den(21))
    call cont_VV(nsync, wf16(:,1), wf32(:,4), A(:,4), n3(:,126), t3x512(:,:,4), nhel, den(24))
    call cont_VV(nsync, wf16(:,5), wf32(:,5), A(:,5), n3(:,127), t3x512(:,:,5), nhel, den(32))
    call cont_VV(nsync, wf32(:,2), wf16(:,6), A(:,6), n3(:,128), t3x512(:,:,6), nhel, den(34))
    call cont_VV(nsync, wf32(:,5), wf16(:,7), A(:,7), n3(:,129), t3x512(:,:,7), nhel, den(36))
    call cont_VV(nsync, wf32(:,3), wf16(:,6), A(:,8), n3(:,130), t3x512(:,:,8), nhel, den(37))
    call cont_VV(nsync, wf16(:,8), wf32(:,6), A(:,9), n3(:,131), t3x512(:,:,9), nhel, den(43))
    call cont_VV(nsync, wf32(:,6), wf16(:,9), A(:,10), n3(:,132), t3x512(:,:,10), nhel, den(45))
    call cont_VV(nsync, wf32(:,1), wf16(:,10), A(:,11), n3(:,133), t3x512(:,:,11), nhel, den(47))
    call cont_VV(nsync, wf32(:,4), wf16(:,10), A(:,12), n3(:,134), t3x512(:,:,12), nhel, den(48))
    call cont_VV(nsync, wf16(:,8), wf32(:,7), A(:,13), n3(:,135), t3x512(:,:,13), nhel, den(52))
    call cont_VV(nsync, wf16(:,5), wf32(:,8), A(:,14), n3(:,136), t3x512(:,:,14), nhel, den(55))
    call cont_VV(nsync, wf16(:,9), wf32(:,7), A(:,15), n3(:,137), t3x512(:,:,15), nhel, den(56))
    call cont_VV(nsync, wf16(:,7), wf32(:,8), A(:,16), n3(:,138), t3x512(:,:,16), nhel, den(57))
    call cont_VV(nsync, wf16(:,13), wf32(:,9), A(:,17), n3(:,139), t3x512(:,:,17), nhel, den(64))
    call cont_VV(nsync, wf16(:,14), wf32(:,10), A(:,18), n3(:,140), t3x512(:,:,18), nhel, den(68))
    call cont_VV(nsync, wf16(:,14), wf32(:,11), A(:,19), n3(:,141), t3x512(:,:,19), nhel, den(72))
    call cont_VV(nsync, wf16(:,13), wf32(:,12), A(:,20), n3(:,142), t3x512(:,:,20), nhel, den(75))
    call cont_VV(nsync, wf16(:,17), wf32(:,13), A(:,21), n3(:,143), t3x512(:,:,21), nhel, den(81))
    call cont_VV(nsync, wf32(:,10), wf16(:,18), A(:,22), n3(:,144), t3x512(:,:,22), nhel, den(83))
    call cont_VV(nsync, wf32(:,13), wf16(:,19), A(:,23), n3(:,145), t3x512(:,:,23), nhel, den(85))
    call cont_VV(nsync, wf32(:,11), wf16(:,18), A(:,24), n3(:,146), t3x512(:,:,24), nhel, den(86))
    call cont_VV(nsync, wf16(:,20), wf32(:,14), A(:,25), n3(:,147), t3x512(:,:,25), nhel, den(90))
    call cont_VV(nsync, wf32(:,14), wf16(:,21), A(:,26), n3(:,148), t3x512(:,:,26), nhel, den(92))
    call cont_VV(nsync, wf32(:,9), wf16(:,22), A(:,27), n3(:,149), t3x512(:,:,27), nhel, den(94))
    call cont_VV(nsync, wf32(:,12), wf16(:,22), A(:,28), n3(:,150), t3x512(:,:,28), nhel, den(95))
    call cont_VV(nsync, wf16(:,20), wf32(:,15), A(:,29), n3(:,151), t3x512(:,:,29), nhel, den(99))
    call cont_VV(nsync, wf16(:,17), wf32(:,16), A(:,30), n3(:,152), t3x512(:,:,30), nhel, den(102))
    call cont_VV(nsync, wf16(:,21), wf32(:,15), A(:,31), n3(:,153), t3x512(:,:,31), nhel, den(103))
    call cont_VV(nsync, wf16(:,19), wf32(:,16), A(:,32), n3(:,154), t3x512(:,:,32), nhel, den(104))
    call cont_VV(nsync, wf16(:,13), wf32(:,17), A(:,33), n3(:,155), t3x512(:,:,33), nhel, den(107))
    call cont_VV(nsync, wf16(:,5), wf32(:,18), A(:,34), n3(:,156), t3x512(:,:,34), nhel, den(109))
    call cont_VV(nsync, wf16(:,7), wf32(:,18), A(:,35), n3(:,157), t3x512(:,:,35), nhel, den(110))
    call cont_VV(nsync, wf16(:,13), wf32(:,19), A(:,36), n3(:,158), t3x512(:,:,36), nhel, den(115))
    call cont_VV(nsync, wf16(:,17), wf32(:,20), A(:,37), n3(:,159), t3x512(:,:,37), nhel, den(117))
    call cont_VV(nsync, wf16(:,1), wf32(:,21), A(:,38), n3(:,160), t3x512(:,:,38), nhel, den(119))
    call cont_VV(nsync, wf16(:,19), wf32(:,20), A(:,39), n3(:,161), t3x512(:,:,39), nhel, den(120))
    call cont_VV(nsync, wf16(:,1), wf32(:,22), A(:,40), n3(:,162), t3x512(:,:,40), nhel, den(122))
    call cont_VV(nsync, wf16(:,10), wf32(:,21), A(:,41), n3(:,163), t3x512(:,:,41), nhel, den(123))
    call cont_VV(nsync, wf16(:,22), wf32(:,17), A(:,42), n3(:,164), t3x512(:,:,42), nhel, den(124))
    call cont_VV(nsync, wf16(:,22), wf32(:,19), A(:,43), n3(:,165), t3x512(:,:,43), nhel, den(125))
    call cont_VV(nsync, wf16(:,10), wf32(:,22), A(:,44), n3(:,166), t3x512(:,:,44), nhel, den(126))
    call cont_VV(nsync, wf16(:,17), wf32(:,23), A(:,45), n3(:,167), t3x512(:,:,45), nhel, den(131))
    call cont_VV(nsync, wf16(:,5), wf32(:,24), A(:,46), n3(:,168), t3x512(:,:,46), nhel, den(133))
    call cont_VV(nsync, wf16(:,19), wf32(:,23), A(:,47), n3(:,169), t3x512(:,:,47), nhel, den(134))
    call cont_VV(nsync, wf16(:,7), wf32(:,24), A(:,48), n3(:,170), t3x512(:,:,48), nhel, den(135))
    call cont_VV(nsync, wf16(:,14), wf32(:,25), A(:,49), n3(:,171), t3x512(:,:,49), nhel, den(138))
    call cont_VV(nsync, wf16(:,8), wf32(:,26), A(:,50), n3(:,172), t3x512(:,:,50), nhel, den(140))
    call cont_VV(nsync, wf16(:,9), wf32(:,26), A(:,51), n3(:,173), t3x512(:,:,51), nhel, den(141))
    call cont_VV(nsync, wf16(:,14), wf32(:,27), A(:,52), n3(:,174), t3x512(:,:,52), nhel, den(147))
    call cont_VV(nsync, wf16(:,20), wf32(:,28), A(:,53), n3(:,175), t3x512(:,:,53), nhel, den(149))
    call cont_VV(nsync, wf16(:,2), wf32(:,29), A(:,54), n3(:,176), t3x512(:,:,54), nhel, den(151))
    call cont_VV(nsync, wf16(:,21), wf32(:,28), A(:,55), n3(:,177), t3x512(:,:,55), nhel, den(152))
    call cont_VV(nsync, wf16(:,2), wf32(:,30), A(:,56), n3(:,178), t3x512(:,:,56), nhel, den(155))
    call cont_VV(nsync, wf16(:,6), wf32(:,29), A(:,57), n3(:,179), t3x512(:,:,57), nhel, den(156))
    call cont_VV(nsync, wf16(:,18), wf32(:,25), A(:,58), n3(:,180), t3x512(:,:,58), nhel, den(157))
    call cont_VV(nsync, wf16(:,18), wf32(:,27), A(:,59), n3(:,181), t3x512(:,:,59), nhel, den(158))
    call cont_VV(nsync, wf16(:,6), wf32(:,30), A(:,60), n3(:,182), t3x512(:,:,60), nhel, den(159))
    call cont_VV(nsync, wf16(:,20), wf32(:,31), A(:,61), n3(:,183), t3x512(:,:,61), nhel, den(165))
    call cont_VV(nsync, wf16(:,8), wf32(:,32), A(:,62), n3(:,184), t3x512(:,:,62), nhel, den(168))
    call cont_VV(nsync, wf16(:,21), wf32(:,31), A(:,63), n3(:,185), t3x512(:,:,63), nhel, den(169))
    call cont_VV(nsync, wf16(:,9), wf32(:,32), A(:,64), n3(:,186), t3x512(:,:,64), nhel, den(170))
    call cont_VV(nsync, wf16(:,5), wf32(:,33), A(:,65), n3(:,187), t3x512(:,:,65), nhel, den(171))
    call cont_QA(nsync, wf16(:,34), wf32(:,34), A(:,66), n3(:,188), t3x512(:,:,66), nhel, den(173))
    call cont_VV(nsync, wf16(:,13), wf32(:,35), A(:,67), n3(:,189), t3x512(:,:,67), nhel, den(174))
    call cont_QA(nsync, wf16(:,34), wf32(:,36), A(:,68), n3(:,190), t3x512(:,:,68), nhel, den(175))
    call cont_QA(nsync, wf16(:,36), wf32(:,37), A(:,69), n3(:,191), t3x512(:,:,69), nhel, den(177))
    call cont_QA(nsync, wf16(:,36), wf32(:,38), A(:,70), n3(:,192), t3x512(:,:,70), nhel, den(178))
    call cont_VV(nsync, wf16(:,13), wf32(:,39), A(:,71), n3(:,193), t3x512(:,:,71), nhel, den(179))
    call cont_QA(nsync, wf16(:,38), wf32(:,40), A(:,72), n3(:,194), t3x512(:,:,72), nhel, den(181))
    call cont_QA(nsync, wf16(:,36), wf32(:,41), A(:,73), n3(:,195), t3x512(:,:,73), nhel, den(182))
    call cont_QA(nsync, wf16(:,40), wf32(:,42), A(:,74), n3(:,196), t3x512(:,:,74), nhel, den(184))
    call cont_VV(nsync, wf16(:,14), wf32(:,43), A(:,75), n3(:,197), t3x512(:,:,75), nhel, den(185))
    call cont_QA(nsync, wf16(:,36), wf32(:,44), A(:,76), n3(:,198), t3x512(:,:,76), nhel, den(186))
    call cont_VV(nsync, wf16(:,1), wf32(:,45), A(:,77), n3(:,199), t3x512(:,:,77), nhel, den(187))
    call cont_QA(nsync, wf16(:,42), wf32(:,46), A(:,78), n3(:,200), t3x512(:,:,78), nhel, den(189))
    call cont_VV(nsync, wf16(:,17), wf32(:,47), A(:,79), n3(:,201), t3x512(:,:,79), nhel, den(190))
    call cont_QA(nsync, wf16(:,42), wf32(:,48), A(:,80), n3(:,202), t3x512(:,:,80), nhel, den(191))
    call cont_QA(nsync, wf16(:,44), wf32(:,49), A(:,81), n3(:,203), t3x512(:,:,81), nhel, den(193))
    call cont_QA(nsync, wf16(:,44), wf32(:,50), A(:,82), n3(:,204), t3x512(:,:,82), nhel, den(194))
    call cont_QA(nsync, wf16(:,42), wf32(:,51), A(:,83), n3(:,205), t3x512(:,:,83), nhel, den(195))
    call cont_VV(nsync, wf16(:,1), wf32(:,52), A(:,84), n3(:,206), t3x512(:,:,84), nhel, den(196))
    call cont_QA(nsync, wf16(:,38), wf32(:,53), A(:,85), n3(:,207), t3x512(:,:,85), nhel, den(197))
    call cont_QA(nsync, wf16(:,42), wf32(:,54), A(:,86), n3(:,208), t3x512(:,:,86), nhel, den(198))
    call cont_QA(nsync, wf16(:,40), wf32(:,55), A(:,87), n3(:,209), t3x512(:,:,87), nhel, den(199))
    call cont_VV(nsync, wf16(:,2), wf32(:,56), A(:,88), n3(:,210), t3x512(:,:,88), nhel, den(200))
    call cont_VV(nsync, wf16(:,17), wf32(:,57), A(:,89), n3(:,211), t3x512(:,:,89), nhel, den(201))
    call cont_QA(nsync, wf16(:,46), wf32(:,58), A(:,90), n3(:,212), t3x512(:,:,90), nhel, den(203))
    call cont_QA(nsync, wf16(:,44), wf32(:,59), A(:,91), n3(:,213), t3x512(:,:,91), nhel, den(204))
    call cont_QA(nsync, wf16(:,34), wf32(:,60), A(:,92), n3(:,214), t3x512(:,:,92), nhel, den(205))
    call cont_VV(nsync, wf16(:,5), wf32(:,61), A(:,93), n3(:,215), t3x512(:,:,93), nhel, den(206))
    call cont_QA(nsync, wf16(:,46), wf32(:,62), A(:,94), n3(:,216), t3x512(:,:,94), nhel, den(207))
    call cont_QA(nsync, wf16(:,40), wf32(:,63), A(:,95), n3(:,217), t3x512(:,:,95), nhel, den(208))
    call cont_QA(nsync, wf16(:,46), wf32(:,64), A(:,96), n3(:,218), t3x512(:,:,96), nhel, den(209))
    call cont_QA(nsync, wf16(:,40), wf32(:,65), A(:,97), n3(:,219), t3x512(:,:,97), nhel, den(210))
    call cont_QA(nsync, wf16(:,46), wf32(:,66), A(:,98), n3(:,220), t3x512(:,:,98), nhel, den(211))
    call cont_VV(nsync, wf16(:,7), wf32(:,61), A(:,99), n3(:,221), t3x512(:,:,99), nhel, den(212))
    call cont_VV(nsync, wf16(:,6), wf32(:,56), A(:,100), n3(:,222), t3x512(:,:,100), nhel, den(213))
    call cont_QA(nsync, wf16(:,48), wf32(:,67), A(:,101), n3(:,223), t3x512(:,:,101), nhel, den(215))
    call cont_VV(nsync, wf16(:,20), wf32(:,68), A(:,102), n3(:,224), t3x512(:,:,102), nhel, den(216))
    call cont_QA(nsync, wf16(:,44), wf32(:,69), A(:,103), n3(:,225), t3x512(:,:,103), nhel, den(217))
    call cont_QA(nsync, wf16(:,34), wf32(:,70), A(:,104), n3(:,226), t3x512(:,:,104), nhel, den(218))
    call cont_QA(nsync, wf16(:,48), wf32(:,71), A(:,105), n3(:,227), t3x512(:,:,105), nhel, den(219))
    call cont_VV(nsync, wf16(:,8), wf32(:,72), A(:,106), n3(:,228), t3x512(:,:,106), nhel, den(220))
    call cont_QA(nsync, wf16(:,48), wf32(:,73), A(:,107), n3(:,229), t3x512(:,:,107), nhel, den(221))
    call cont_QA(nsync, wf16(:,38), wf32(:,74), A(:,108), n3(:,230), t3x512(:,:,108), nhel, den(222))
    call cont_QA(nsync, wf16(:,48), wf32(:,75), A(:,109), n3(:,231), t3x512(:,:,109), nhel, den(223))
    call cont_QA(nsync, wf16(:,38), wf32(:,76), A(:,110), n3(:,232), t3x512(:,:,110), nhel, den(224))
    call cont_VV(nsync, wf16(:,10), wf32(:,52), A(:,111), n3(:,233), t3x512(:,:,111), nhel, den(225))
    call cont_VV(nsync, wf16(:,9), wf32(:,72), A(:,112), n3(:,234), t3x512(:,:,112), nhel, den(226))

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
  complex(REALKIND), intent(out) :: M1(4) ! M1helarray(4,512)
  integer :: empty(0)

  M1(1) = ((-A(j,2)%j-A(j,3)%j-A(j,6)%j-A(j,8)%j-A(j,9)%j-A(j,10)%j-A(j,13)%j-A(j,15)%j-A(j,33)%j-A(j,36)%j-A(j,37)%j-A(j,39)%j &
       -A(j,42)%j-A(j,43)%j-A(j,45)%j-A(j,47)%j-A(j,69)%j-A(j,72)%j-A(j,73)%j-A(j,81)%j-A(j,90)%j-A(j,91)%j-A(j,96)%j &
       -A(j,108)%j)*f(1))/2._/**/REALKIND+((-A(j,1)%j-A(j,4)%j-A(j,5)%j-A(j,7)%j-A(j,11)%j-A(j,12)%j-A(j,14)%j-A(j,16)%j-A(j,34)%j &
       -A(j,35)%j-A(j,38)%j-A(j,40)%j-A(j,41)%j-A(j,44)%j-A(j,46)%j-A(j,48)%j-A(j,70)%j-A(j,76)%j-A(j,82)%j-A(j,85)%j-A(j,94)%j &
       -A(j,98)%j-A(j,103)%j-A(j,110)%j)*f(1))/6._/**/REALKIND+(CI*(-A(j,67)%j-A(j,71)%j-A(j,79)%j+A(j,88)%j-A(j,89)%j+A(j,100)%j &
       +A(j,106)%j+A(j,112)%j)*f(2))/2._/**/REALKIND
  M1(2) = ((A(j,17)%j+A(j,20)%j+A(j,21)%j+A(j,23)%j+A(j,27)%j+A(j,28)%j+A(j,30)%j+A(j,32)%j+A(j,33)%j+A(j,36)%j+A(j,37)%j &
       +A(j,39)%j+A(j,42)%j+A(j,43)%j+A(j,45)%j+A(j,47)%j+A(j,68)%j+A(j,72)%j+A(j,80)%j+A(j,86)%j+A(j,90)%j+A(j,96)%j+A(j,104)%j &
       +A(j,108)%j)*f(1))/6._/**/REALKIND+((A(j,18)%j+A(j,19)%j+A(j,22)%j+A(j,24)%j+A(j,25)%j+A(j,26)%j+A(j,29)%j+A(j,31)%j &
       +A(j,34)%j+A(j,35)%j+A(j,38)%j+A(j,40)%j+A(j,41)%j+A(j,44)%j+A(j,46)%j+A(j,48)%j+A(j,66)%j+A(j,78)%j+A(j,83)%j+A(j,85)%j &
       +A(j,92)%j+A(j,94)%j+A(j,98)%j+A(j,110)%j)*f(1))/2._/**/REALKIND+(CI*(A(j,65)%j-A(j,75)%j+A(j,77)%j+A(j,84)%j+A(j,93)%j &
       +A(j,99)%j-A(j,102)%j+A(j,111)%j)*f(2))/2._/**/REALKIND
  M1(3) = ((A(j,1)%j+A(j,4)%j+A(j,5)%j+A(j,7)%j+A(j,11)%j+A(j,12)%j+A(j,14)%j+A(j,16)%j+A(j,49)%j+A(j,52)%j+A(j,53)%j+A(j,55)%j &
       +A(j,58)%j+A(j,59)%j+A(j,61)%j+A(j,63)%j+A(j,70)%j+A(j,74)%j+A(j,76)%j+A(j,82)%j+A(j,95)%j+A(j,101)%j+A(j,103)%j &
       +A(j,107)%j)*f(1))/2._/**/REALKIND+((A(j,2)%j+A(j,3)%j+A(j,6)%j+A(j,8)%j+A(j,9)%j+A(j,10)%j+A(j,13)%j+A(j,15)%j+A(j,50)%j &
       +A(j,51)%j+A(j,54)%j+A(j,56)%j+A(j,57)%j+A(j,60)%j+A(j,62)%j+A(j,64)%j+A(j,69)%j+A(j,73)%j+A(j,81)%j+A(j,87)%j+A(j,91)%j &
       +A(j,97)%j+A(j,105)%j+A(j,109)%j)*f(1))/6._/**/REALKIND+(CI*(-A(j,65)%j+A(j,75)%j-A(j,77)%j-A(j,84)%j-A(j,93)%j-A(j,99)%j &
       +A(j,102)%j-A(j,111)%j)*f(2))/2._/**/REALKIND
  M1(4) = ((-A(j,18)%j-A(j,19)%j-A(j,22)%j-A(j,24)%j-A(j,25)%j-A(j,26)%j-A(j,29)%j-A(j,31)%j-A(j,49)%j-A(j,52)%j-A(j,53)%j &
       -A(j,55)%j-A(j,58)%j-A(j,59)%j-A(j,61)%j-A(j,63)%j-A(j,66)%j-A(j,74)%j-A(j,78)%j-A(j,83)%j-A(j,92)%j-A(j,95)%j-A(j,101)%j &
       -A(j,107)%j)*f(1))/6._/**/REALKIND+((-A(j,17)%j-A(j,20)%j-A(j,21)%j-A(j,23)%j-A(j,27)%j-A(j,28)%j-A(j,30)%j-A(j,32)%j &
       -A(j,50)%j-A(j,51)%j-A(j,54)%j-A(j,56)%j-A(j,57)%j-A(j,60)%j-A(j,62)%j-A(j,64)%j-A(j,68)%j-A(j,80)%j-A(j,86)%j-A(j,87)%j &
       -A(j,97)%j-A(j,104)%j-A(j,105)%j-A(j,109)%j)*f(1))/2._/**/REALKIND+(CI*(A(j,67)%j+A(j,71)%j+A(j,79)%j-A(j,88)%j+A(j,89)%j &
       -A(j,100)%j-A(j,106)%j-A(j,112)%j)*f(2))/2._/**/REALKIND

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
  use ol_colourmatrix_ppllnnjj_vbs_nexnmxemuudxdxg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(4)
  real(REALKIND),    intent(out) :: M2colint(0:47-1)
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
  use ol_colourmatrix_ppllnnjj_vbs_nexnmxemuudxdxg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(4), M2(4)
  real(REALKIND),    intent(out) :: M2colint(0:47-1)
  integer,           intent(in)  :: extcombs(:)
  integer :: extcomb, colmatpos, i, j, eco

  M2colint = 0

  do eco = 1, size(extcombs)
    extcomb = extcombs(eco)
    colmatpos = 4*extcomb
    do i = 1, 4
      do j = 1, 4
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
  use ol_colourmatrix_ppllnnjj_vbs_nexnmxemuudxdxg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(4)
  complex(REALKIND), intent(in)  :: M2(4)
  complex(REALKIND),    intent(out) :: M2colint
  integer :: i, j

  M2colint = 0

  do i = 1, 4
    do j = 1, 4
      M2colint = M2colint + M1(i)*K1(i,j)*conjg(M2(j))
    end do
  end do

end subroutine colintmunu

end subroutine amp2


#ifdef PRECISION_dp
subroutine colourvector(M1out, nhelout) &
  & bind(c,name="ol_tree_colvect_ppllnnjj_vbs_nexnmxemuudxdxg_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(4,512)
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
    & bind(c,name="ol_f_amp2tree_ppllnnjj_vbs_nexnmxemuudxdxg_1")
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
    & bind(c,name="ol_f_amp2ccone_ppllnnjj_vbs_nexnmxemuudxdxg_1")
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
    & bind(c,name="ol_f_amp2ccall_ppllnnjj_vbs_nexnmxemuudxdxg_1")
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
    & bind(c,name="ol_f_amp2hcone_ppllnnjj_vbs_nexnmxemuudxdxg_1")
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
    & bind(c,name="ol_f_amp2hcall_ppllnnjj_vbs_nexnmxemuudxdxg_1")
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
    & bind(c,name="ol_amp2tree_ppllnnjj_vbs_nexnmxemuudxdxg_1")
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
    & bind(c,name="ol_amp2ccone_ppllnnjj_vbs_nexnmxemuudxdxg_1")
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
    & bind(c,name="ol_amp2ccall_ppllnnjj_vbs_nexnmxemuudxdxg_1")
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
    & bind(c,name="ol_amp2hcone_ppllnnjj_vbs_nexnmxemuudxdxg_1")
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
    & bind(c,name="ol_amp2hcall_ppllnnjj_vbs_nexnmxemuudxdxg_1")
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
    & bind(c,name="amp2tree_ppllnnjj_vbs_nexnmxemuudxdxg_1_")
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
    & bind(c,name="amp2ccone_ppllnnjj_vbs_nexnmxemuudxdxg_1_")
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
    & bind(c,name="amp2ccall_ppllnnjj_vbs_nexnmxemuudxdxg_1_")
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
    & bind(c,name="amp2hcone_ppllnnjj_vbs_nexnmxemuudxdxg_1_")
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
    & bind(c,name="amp2hcall_ppllnnjj_vbs_nexnmxemuudxdxg_1_")
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

end module ol_tree_ppllnnjj_vbs_nexnmxemuudxdxg_1_/**/REALKIND
