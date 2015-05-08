
module ol_colourmatrix_pphwjj_udxhwggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(180,6)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1(  1,:) = [  384,  -48,  -48,    6,    6,   60]
  K1(  2,:) = [  -48,  384,    6,   60,  -48,    6]
  K1(  3,:) = [  -48,    6,  384,  -48,   60,    6]
  K1(  4,:) = [    6,   60,  -48,  384,    6,  -48]
  K1(  5,:) = [    6,  -48,   60,    6,  384,  -48]
  K1(  6,:) = [   60,    6,    6,  -48,  -48,  384]
  K1(  7,:) = [  512,  -64,  -64,    8,    8,   80]
  K1(  8,:) = [  -64,  512,    8,   80,  -64,    8]
  K1(  9,:) = [  -64,    8,  512,  -64,   80,    8]
  K1( 10,:) = [    8,   80,  -64,  512,    8,  -64]
  K1( 11,:) = [    8,  -64,   80,    8,  512,  -64]
  K1( 12,:) = [   80,    8,    8,  -64,  -64,  512]
  K1( 13,:) = [    1,   10,   10,  -62,  -62,   28]
  K1( 14,:) = [   10,    1,  -62,   28,   10,  -62]
  K1( 15,:) = [   10,  -62,    1,   10,   28,  -62]
  K1( 16,:) = [  -62,   28,   10,    1,  -62,   10]
  K1( 17,:) = [  -62,   10,   28,  -62,    1,   10]
  K1( 18,:) = [   28,  -62,  -62,   10,   10,    1]
  K1( 19,:) = [  512,  -64,  -64,    8,    8,   80]
  K1( 20,:) = [  -64,  512,    8,   80,  -64,    8]
  K1( 21,:) = [  -64,    8,  512,  -64,   80,    8]
  K1( 22,:) = [    8,   80,  -64,  512,    8,  -64]
  K1( 23,:) = [    8,  -64,   80,    8,  512,  -64]
  K1( 24,:) = [   80,    8,    8,  -64,  -64,  512]
  K1( 25,:) = [    0,    0,    0,    0,    0,    0]
  K1( 26,:) = [    0,    0,    0,    0,    0,    0]
  K1( 27,:) = [    0,    0,    0,    0,    0,    0]
  K1( 28,:) = [    0,    0,    0,    0,    0,    0]
  K1( 29,:) = [    0,    0,    0,    0,    0,    0]
  K1( 30,:) = [    0,    0,    0,    0,    0,    0]
  K1( 31,:) = [    0,    0,    0,    0,    0,    0]
  K1( 32,:) = [    0,    0,    0,    0,    0,    0]
  K1( 33,:) = [    0,    0,    0,    0,    0,    0]
  K1( 34,:) = [    0,    0,    0,    0,    0,    0]
  K1( 35,:) = [    0,    0,    0,    0,    0,    0]
  K1( 36,:) = [    0,    0,    0,    0,    0,    0]
  K1( 37,:) = [    0,    0,    0,    0,    0,    0]
  K1( 38,:) = [    0,    0,    0,    0,    0,    0]
  K1( 39,:) = [    0,    0,    0,    0,    0,    0]
  K1( 40,:) = [    0,    0,    0,    0,    0,    0]
  K1( 41,:) = [    0,    0,    0,    0,    0,    0]
  K1( 42,:) = [    0,    0,    0,    0,    0,    0]
  K1( 43,:) = [    0,    0,    0,    0,    0,    0]
  K1( 44,:) = [    0,    0,    0,    0,    0,    0]
  K1( 45,:) = [    0,    0,    0,    0,    0,    0]
  K1( 46,:) = [    0,    0,    0,    0,    0,    0]
  K1( 47,:) = [    0,    0,    0,    0,    0,    0]
  K1( 48,:) = [    0,    0,    0,    0,    0,    0]
  K1( 49,:) = [    0,    0,    0,    0,    0,    0]
  K1( 50,:) = [    0,    0,    0,    0,    0,    0]
  K1( 51,:) = [    0,    0,    0,    0,    0,    0]
  K1( 52,:) = [    0,    0,    0,    0,    0,    0]
  K1( 53,:) = [    0,    0,    0,    0,    0,    0]
  K1( 54,:) = [    0,    0,    0,    0,    0,    0]
  K1( 55,:) = [    0,    0,    0,    0,    0,    0]
  K1( 56,:) = [    0,    0,    0,    0,    0,    0]
  K1( 57,:) = [    0,    0,    0,    0,    0,    0]
  K1( 58,:) = [    0,    0,    0,    0,    0,    0]
  K1( 59,:) = [    0,    0,    0,    0,    0,    0]
  K1( 60,:) = [    0,    0,    0,    0,    0,    0]
  K1( 61,:) = [    0,    0,    0,    0,    0,    0]
  K1( 62,:) = [    0,    0,    0,    0,    0,    0]
  K1( 63,:) = [    0,    0,    0,    0,    0,    0]
  K1( 64,:) = [    0,    0,    0,    0,    0,    0]
  K1( 65,:) = [    0,    0,    0,    0,    0,    0]
  K1( 66,:) = [    0,    0,    0,    0,    0,    0]
  K1( 67,:) = [   -9,  -90,   -9,   -9,   72,  -90]
  K1( 68,:) = [  -90,   -9,   72,  -90,   -9,   -9]
  K1( 69,:) = [   -9,   72,   72,   72,   72,   -9]
  K1( 70,:) = [   -9,  -90,   72, -576,   -9,   72]
  K1( 71,:) = [   72,   -9,   72,   -9,   72,   72]
  K1( 72,:) = [  -90,   -9,   -9,   72,   72, -576]
  K1( 73,:) = [ -576,   72,   72,   -9,   -9,  -90]
  K1( 74,:) = [   72, -576,   -9,  -90,   72,   -9]
  K1( 75,:) = [   72,   -9,   72,   -9,   72,   72]
  K1( 76,:) = [   -9,  -90,   -9,   -9,   72,  -90]
  K1( 77,:) = [   -9,   72,   72,   72,   72,   -9]
  K1( 78,:) = [  -90,   -9,   72,  -90,   -9,   -9]
  K1( 79,:) = [    0,    0,    0,    0,    0,    0]
  K1( 80,:) = [    0,    0,    0,    0,    0,    0]
  K1( 81,:) = [    0,    0,    0,    0,    0,    0]
  K1( 82,:) = [    0,    0,    0,    0,    0,    0]
  K1( 83,:) = [    0,    0,    0,    0,    0,    0]
  K1( 84,:) = [    0,    0,    0,    0,    0,    0]
  K1( 85,:) = [    0,    0,    0,    0,    0,    0]
  K1( 86,:) = [    0,    0,    0,    0,    0,    0]
  K1( 87,:) = [    0,    0,    0,    0,    0,    0]
  K1( 88,:) = [    0,    0,    0,    0,    0,    0]
  K1( 89,:) = [    0,    0,    0,    0,    0,    0]
  K1( 90,:) = [    0,    0,    0,    0,    0,    0]
  K1( 91,:) = [ 1152, -144, -144,   18,   18,  180]
  K1( 92,:) = [ -144, 1152,   18,  180, -144,   18]
  K1( 93,:) = [ -144,   18, 1152, -144,  180,   18]
  K1( 94,:) = [   18,  180, -144, 1152,   18, -144]
  K1( 95,:) = [   18, -144,  180,   18, 1152, -144]
  K1( 96,:) = [  180,   18,   18, -144, -144, 1152]
  K1( 97,:) = [   72,   72,   -9,   72,   -9,   72]
  K1( 98,:) = [   72, -576,   -9,  -90,   72,   -9]
  K1( 99,:) = [   -9,   -9,   -9,  -90,  -90,   72]
  K1(100,:) = [   72,  -90,  -90,   -9,   -9,   -9]
  K1(101,:) = [   -9,   72,  -90,   -9, -576,   72]
  K1(102,:) = [   72,   -9,   72,   -9,   72,   72]
  K1(103,:) = [   72,   -9,   72,   -9,   72,   72]
  K1(104,:) = [   -9,   -9,   -9,  -90,  -90,   72]
  K1(105,:) = [   72,   -9, -576,   72,  -90,   -9]
  K1(106,:) = [   -9,  -90,   72, -576,   -9,   72]
  K1(107,:) = [   72,  -90,  -90,   -9,   -9,   -9]
  K1(108,:) = [   72,   72,   -9,   72,   -9,   72]
  K1(109,:) = [    0,    0,    0,    0,    0,    0]
  K1(110,:) = [    0,    0,    0,    0,    0,    0]
  K1(111,:) = [    0,    0,    0,    0,    0,    0]
  K1(112,:) = [    0,    0,    0,    0,    0,    0]
  K1(113,:) = [    0,    0,    0,    0,    0,    0]
  K1(114,:) = [    0,    0,    0,    0,    0,    0]
  K1(115,:) = [    0,    0,    0,    0,    0,    0]
  K1(116,:) = [    0,    0,    0,    0,    0,    0]
  K1(117,:) = [    0,    0,    0,    0,    0,    0]
  K1(118,:) = [    0,    0,    0,    0,    0,    0]
  K1(119,:) = [    0,    0,    0,    0,    0,    0]
  K1(120,:) = [    0,    0,    0,    0,    0,    0]
  K1(121,:) = [ -648,   81,    0,    0,  -81, -162]
  K1(122,:) = [   81,   81,    0,  162,   81,    0]
  K1(123,:) = [    0,    0, -648,   81, -162,  -81]
  K1(124,:) = [    0,  162,   81,   81,    0,   81]
  K1(125,:) = [  -81,   81, -162,    0, -648,    0]
  K1(126,:) = [ -162,    0,  -81,   81,    0, -648]
  K1(127,:) = [ 1152, -144, -144,   18,   18,  180]
  K1(128,:) = [ -144, 1152,   18,  180, -144,   18]
  K1(129,:) = [ -144,   18, 1152, -144,  180,   18]
  K1(130,:) = [   18,  180, -144, 1152,   18, -144]
  K1(131,:) = [   18, -144,  180,   18, 1152, -144]
  K1(132,:) = [  180,   18,   18, -144, -144, 1152]
  K1(133,:) = [ -576,   72,   72,   -9,   -9,  -90]
  K1(134,:) = [   72,   72,   -9,   72,   -9,   72]
  K1(135,:) = [   72,   -9, -576,   72,  -90,   -9]
  K1(136,:) = [   -9,   72,   72,   72,   72,   -9]
  K1(137,:) = [   -9,   -9,  -90,   72,   -9,  -90]
  K1(138,:) = [  -90,   72,   -9,   -9,  -90,   -9]
  K1(139,:) = [   -9,   -9,  -90,   72,   -9,  -90]
  K1(140,:) = [   -9,   72,   72,   72,   72,   -9]
  K1(141,:) = [  -90,   72,   -9,   -9,  -90,   -9]
  K1(142,:) = [   72,   72,   -9,   72,   -9,   72]
  K1(143,:) = [   -9,   72,  -90,   -9, -576,   72]
  K1(144,:) = [  -90,   -9,   -9,   72,   72, -576]
  K1(145,:) = [    0,    0,    0,    0,    0,    0]
  K1(146,:) = [    0,    0,    0,    0,    0,    0]
  K1(147,:) = [    0,    0,    0,    0,    0,    0]
  K1(148,:) = [    0,    0,    0,    0,    0,    0]
  K1(149,:) = [    0,    0,    0,    0,    0,    0]
  K1(150,:) = [    0,    0,    0,    0,    0,    0]
  K1(151,:) = [    0,    0,    0,    0,    0,    0]
  K1(152,:) = [    0,    0,    0,    0,    0,    0]
  K1(153,:) = [    0,    0,    0,    0,    0,    0]
  K1(154,:) = [    0,    0,    0,    0,    0,    0]
  K1(155,:) = [    0,    0,    0,    0,    0,    0]
  K1(156,:) = [    0,    0,    0,    0,    0,    0]
  K1(157,:) = [   81,   81,   81,    0,    0,  162]
  K1(158,:) = [   81, -648,  -81, -162,    0,    0]
  K1(159,:) = [   81,  -81, -648,    0, -162,    0]
  K1(160,:) = [    0, -162,    0, -648,  -81,   81]
  K1(161,:) = [    0,    0, -162,  -81, -648,   81]
  K1(162,:) = [  162,    0,    0,   81,   81,   81]
  K1(163,:) = [ -648,    0,   81,  -81,    0, -162]
  K1(164,:) = [    0, -648,    0, -162,   81,  -81]
  K1(165,:) = [   81,    0,   81,   81,  162,    0]
  K1(166,:) = [  -81, -162,   81, -648,    0,    0]
  K1(167,:) = [    0,   81,  162,    0,   81,   81]
  K1(168,:) = [ -162,  -81,    0,    0,   81, -648]
  K1(169,:) = [ 1152, -144, -144,   18,   18,  180]
  K1(170,:) = [ -144, 1152,   18,  180, -144,   18]
  K1(171,:) = [ -144,   18, 1152, -144,  180,   18]
  K1(172,:) = [   18,  180, -144, 1152,   18, -144]
  K1(173,:) = [   18, -144,  180,   18, 1152, -144]
  K1(174,:) = [  180,   18,   18, -144, -144, 1152]
  K1(175,:) = [    0,    0,    0,    0,    0,    0]
  K1(176,:) = [    0,    0,    0,    0,    0,    0]
  K1(177,:) = [    0,    0,    0,    0,    0,    0]
  K1(178,:) = [    0,    0,    0,    0,    0,    0]
  K1(179,:) = [    0,    0,    0,    0,    0,    0]
  K1(180,:) = [    0,    0,    0,    0,    0,    0]
  K1 = (1._/**/REALKIND / 54) * K1

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphwjj_udxhwggg_1_/**/REALKIND



module ol_forced_parameters_pphwjj_udxhwggg_1_/**/REALKIND
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
  if (YU /= 0) write(*,101) 'YU = 0'
  if (YD /= 0) write(*,101) 'YD = 0'
  if (YS /= 0) write(*,101) 'YS = 0'
  if (YC /= 0) write(*,101) 'YC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphwjj_udxhwggg_1_/**/REALKIND

module ol_tree_pphwjj_udxhwggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(2)
  complex(REALKIND), save :: den(106)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 96 ! number of helicity configurations
  integer(intkind2), save :: nhel = 96 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(96) ! physical helicity states
  complex(DREALKIND) :: M1helarr(6,96) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = (CI*eQED**2*gQCD**3*lambdaHWW*MW)/(sqrt2*sw**2)
    f(2) = (eQED**2*gQCD**3*lambdaHWW*MW)/(sqrt2*sw**2)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,12) - MW2)
  den(2) = 1 / (Q(5,112))
  den(3) = 1 / (Q(5,13))
  den(6) = 1 / (Q(5,113))
  den(9) = 1 / (Q(5,17))
  den(10) = 1 / (Q(5,34))
  den(12) = 1 / (Q(5,81))
  den(16) = 1 / (Q(5,98))
  den(19) = 1 / (Q(5,66))
  den(21) = 1 / (Q(5,49))
  den(26) = 1 / (Q(5,96))
  den(28) = 1 / (Q(5,14))
  den(35) = 1 / (Q(5,33))
  den(36) = 1 / (Q(5,18))
  den(38) = 1 / (Q(5,97))
  den(42) = 1 / (Q(5,82))
  den(45) = 1 / (Q(5,65))
  den(49) = 1 / (Q(5,50))
  den(62) = 1 / (Q(5,80))
  den(79) = 1 / (Q(5,48))

  ! denominators

  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(1)*den(7)
  den(11) = den(1)*den(10)
  den(13) = den(9)*den(12)
  den(14) = den(11)*den(13)
  den(15) = den(1)*den(9)
  den(17) = den(10)*den(16)
  den(18) = den(15)*den(17)
  den(20) = den(1)*den(19)
  den(22) = den(9)*den(21)
  den(23) = den(20)*den(22)
  den(24) = den(16)*den(19)
  den(25) = den(15)*den(24)
  den(27) = den(9)*den(26)
  den(29) = den(1)*den(28)
  den(30) = den(27)*den(29)
  den(31) = den(16)*den(26)
  den(32) = den(15)*den(31)
  den(33) = den(22)*den(29)
  den(34) = den(13)*den(29)
  den(37) = den(1)*den(36)
  den(39) = den(35)*den(38)
  den(40) = den(37)*den(39)
  den(41) = den(1)*den(35)
  den(43) = den(36)*den(42)
  den(44) = den(41)*den(43)
  den(46) = den(38)*den(45)
  den(47) = den(37)*den(46)
  den(48) = den(1)*den(45)
  den(50) = den(36)*den(49)
  den(51) = den(48)*den(50)
  den(52) = den(26)*den(36)
  den(53) = den(4)*den(52)
  den(54) = den(26)*den(38)
  den(55) = den(37)*den(54)
  den(56) = den(4)*den(43)
  den(57) = den(4)*den(50)
  den(58) = den(21)*den(35)
  den(59) = den(20)*den(58)
  den(60) = den(19)*den(42)
  den(61) = den(41)*den(60)
  den(63) = den(35)*den(62)
  den(64) = den(29)*den(63)
  den(65) = den(42)*den(62)
  den(66) = den(41)*den(65)
  den(67) = den(29)*den(58)
  den(68) = den(29)*den(39)
  den(69) = den(12)*den(45)
  den(70) = den(11)*den(69)
  den(71) = den(10)*den(49)
  den(72) = den(48)*den(71)
  den(73) = den(10)*den(62)
  den(74) = den(4)*den(73)
  den(75) = den(12)*den(62)
  den(76) = den(11)*den(75)
  den(77) = den(4)*den(17)
  den(78) = den(4)*den(71)
  den(80) = den(45)*den(79)
  den(81) = den(29)*den(80)
  den(82) = den(49)*den(79)
  den(83) = den(48)*den(82)
  den(84) = den(19)*den(79)
  den(85) = den(4)*den(84)
  den(86) = den(21)*den(79)
  den(87) = den(20)*den(86)
  den(88) = den(2)*den(79)
  den(89) = den(4)*den(88)
  den(90) = den(29)*den(88)
  den(91) = den(4)*den(82)
  den(92) = den(29)*den(86)
  den(93) = den(29)*den(69)
  den(94) = den(29)*den(46)
  den(95) = den(4)*den(24)
  den(96) = den(4)*den(60)
  den(97) = den(2)*den(62)
  den(98) = den(4)*den(97)
  den(99) = den(29)*den(97)
  den(100) = den(4)*den(65)
  den(101) = den(29)*den(75)
  den(102) = den(2)*den(26)
  den(103) = den(4)*den(102)
  den(104) = den(29)*den(102)
  den(105) = den(4)*den(31)
  den(106) = den(29)*den(54)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_pphwjj_udxhwggg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_pphwjj_udxhwggg_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for up anti-down higgs W- glue glue glue -> 0
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
  use ol_external_pphwjj_udxhwggg_1, only: external_perm_pphwjj_udxhwggg_1, &
    & external_perm_inv_pphwjj_udxhwggg_1, extcomb_perm_pphwjj_udxhwggg_1, &
    & average_factor_pphwjj_udxhwggg_1
  use ol_external_pphwjj_udxhwggg_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_pphwjj_udxhwggg_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_pphwjj_udxhwggg_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_pphwjj_udxhwggg_1
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
  complex(REALKIND) :: MOM_LC(4), M1(6), M1helarray(6,96)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(1), ex4(3), ex5(2), ex6(2), ex7(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf3(3,2), wf4(4,15), wf6(6,4), wf8(8,42), wf12(12,14), wf16(16,18), wf96(96,54)

  type(polcont) :: A(96,54)
  complex(REALKIND) :: Aj(54)

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
  extmasses2 = [ rZERO2, rZERO2, rMH2, rMW2, rZERO2, rZERO2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_pphwjj_udxhwggg_1,7)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,7)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_pphwjj_udxhwggg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_pphwjj_udxhwggg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_pphwjj_udxhwggg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_Q(P(:,1), rZERO, H1, ex1)
  call wf_A(P(:,2), rZERO, H2, ex2)
  call wf_S(P(:,3), rMH, H3, ex3)
  call wf_V(P(:,4), rMW, H4, ex4)
  call wf_V(P(:,5), rZERO, H5, ex5)
  call wf_V(P(:,6), rZERO, H6, ex6)
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
  call vert_SV_V(ntry, ex3, ex4, wf3(:,1), n3(:,1), t3x3(:,:,1))
  call vert_GGG_G(ntry, ex5, ex6, ex7, wf8(:,1), n4(:,1), t4x8(:,:,1))
  call prop_W_W(ntry, wf3(:,1), Q(:,12), MW, 1_intkind1, wf3(:,2), n2(1))
  call vert_WQ_A(ntry, wf3(:,2), ex1, wf6(:,1), n3(:,2), t3x6(:,:,1))
  call vert_AV_Q(ntry, ex2, wf8(:,1), wf16(:,1), n3(:,3), t3x16(:,:,1))
  call prop_Q_A(ntry, wf6(:,1), Q(:,13), ZERO, 0_intkind1, wf6(:,2), n2(2))
  call vert_GGG_G(ntry, ex6, ex7, ex5, wf8(:,2), n4(:,2), t4x8(:,:,2))
  call vert_AV_Q(ntry, ex2, wf8(:,2), wf16(:,2), n3(:,4), t3x16(:,:,2))
  call vert_GGG_G(ntry, ex7, ex5, ex6, wf8(:,3), n4(:,3), t4x8(:,:,3))
  call vert_AV_Q(ntry, ex2, wf8(:,3), wf16(:,3), n3(:,5), t3x16(:,:,3))
  call vert_VQ_A(ntry, wf8(:,1), ex1, wf16(:,4), n3(:,6), t3x16(:,:,4))
  call vert_AW_Q(ntry, ex2, wf3(:,2), wf6(:,3), n3(:,7), t3x6(:,:,2))
  call prop_Q_A(ntry, wf16(:,4), Q(:,113), ZERO, 0_intkind1, wf16(:,5), n2(3))
  call vert_VQ_A(ntry, wf8(:,2), ex1, wf16(:,6), n3(:,8), t3x16(:,:,5))
  call prop_Q_A(ntry, wf16(:,6), Q(:,113), ZERO, 0_intkind1, wf16(:,7), n2(4))
  call vert_VQ_A(ntry, wf8(:,3), ex1, wf16(:,8), n3(:,9), t3x16(:,:,6))
  call prop_Q_A(ntry, wf16(:,8), Q(:,113), ZERO, 0_intkind1, wf16(:,9), n2(5))
  call vert_VQ_A(ntry, ex5, ex1, wf4(:,1), n3(:,10), t3x4(:,:,1))
  call vert_AV_Q(ntry, ex2, ex6, wf4(:,2), n3(:,11), t3x4(:,:,2))
  call prop_Q_A(ntry, wf4(:,1), Q(:,17), ZERO, 0_intkind1, wf4(:,3), n2(6))
  call prop_A_Q(ntry, wf4(:,2), Q(:,34), ZERO, 0_intkind1, wf4(:,4), n2(7))
  call vert_VQ_A(ntry, ex7, wf4(:,3), wf8(:,4), n3(:,12), t3x8(:,:,1))
  call vert_AW_Q(ntry, wf4(:,4), wf3(:,2), wf12(:,1), n3(:,13), t3x12(:,:,1))
  call prop_Q_A(ntry, wf8(:,4), Q(:,81), ZERO, 0_intkind1, wf8(:,5), n2(8))
  call vert_AV_Q(ntry, wf4(:,4), ex7, wf8(:,6), n3(:,14), t3x8(:,:,2))
  call vert_WQ_A(ntry, wf3(:,2), wf4(:,3), wf12(:,2), n3(:,15), t3x12(:,:,2))
  call prop_A_Q(ntry, wf8(:,6), Q(:,98), ZERO, 0_intkind1, wf8(:,7), n2(9))
  call vert_AV_Q(ntry, ex2, ex7, wf4(:,5), n3(:,16), t3x4(:,:,3))
  call prop_A_Q(ntry, wf4(:,5), Q(:,66), ZERO, 0_intkind1, wf4(:,6), n2(10))
  call vert_VQ_A(ntry, ex6, wf4(:,3), wf8(:,8), n3(:,17), t3x8(:,:,3))
  call vert_AW_Q(ntry, wf4(:,6), wf3(:,2), wf12(:,3), n3(:,18), t3x12(:,:,3))
  call prop_Q_A(ntry, wf8(:,8), Q(:,49), ZERO, 0_intkind1, wf8(:,9), n2(11))
  call vert_AV_Q(ntry, wf4(:,6), ex6, wf8(:,10), n3(:,19), t3x8(:,:,4))
  call prop_A_Q(ntry, wf8(:,10), Q(:,98), ZERO, 0_intkind1, wf8(:,11), n2(12))
  call vert_UV_W(ntry, ex6, Q(:,32), ex7, Q(:,64), wf4(:,7), n3(:,20), t3x4(:,:,4))
  call vert_VQ_A(ntry, wf4(:,7), wf4(:,3), wf16(:,10), n3(:,21), t3x16(:,:,7))
  call prop_A_Q(ntry, wf6(:,3), Q(:,14), ZERO, 0_intkind1, wf6(:,4), n2(13))
  call vert_AV_Q(ntry, ex2, wf4(:,7), wf8(:,12), n3(:,22), t3x8(:,:,5))
  call prop_A_Q(ntry, wf8(:,12), Q(:,98), ZERO, 0_intkind1, wf8(:,13), n2(14))
  call vert_AV_Q(ntry, wf6(:,4), ex7, wf12(:,4), n3(:,23), t3x12(:,:,4))
  call vert_AV_Q(ntry, wf6(:,4), ex6, wf12(:,5), n3(:,24), t3x12(:,:,5))
  call vert_VQ_A(ntry, ex6, ex1, wf4(:,8), n3(:,25), t3x4(:,:,5))
  call vert_AV_Q(ntry, ex2, ex5, wf4(:,9), n3(:,26), t3x4(:,:,6))
  call prop_Q_A(ntry, wf4(:,8), Q(:,33), ZERO, 0_intkind1, wf4(:,10), n2(15))
  call prop_A_Q(ntry, wf4(:,9), Q(:,18), ZERO, 0_intkind1, wf4(:,11), n2(16))
  call vert_VQ_A(ntry, ex7, wf4(:,10), wf8(:,14), n3(:,27), t3x8(:,:,6))
  call vert_AW_Q(ntry, wf4(:,11), wf3(:,2), wf12(:,6), n3(:,28), t3x12(:,:,6))
  call prop_Q_A(ntry, wf8(:,14), Q(:,97), ZERO, 0_intkind1, wf8(:,15), n2(17))
  call vert_AV_Q(ntry, wf4(:,11), ex7, wf8(:,16), n3(:,29), t3x8(:,:,7))
  call vert_WQ_A(ntry, wf3(:,2), wf4(:,10), wf12(:,7), n3(:,30), t3x12(:,:,7))
  call prop_A_Q(ntry, wf8(:,16), Q(:,82), ZERO, 0_intkind1, wf8(:,17), n2(18))
  call vert_VQ_A(ntry, ex7, ex1, wf4(:,12), n3(:,31), t3x4(:,:,7))
  call prop_Q_A(ntry, wf4(:,12), Q(:,65), ZERO, 0_intkind1, wf4(:,13), n2(19))
  call vert_VQ_A(ntry, ex6, wf4(:,13), wf8(:,18), n3(:,32), t3x8(:,:,8))
  call prop_Q_A(ntry, wf8(:,18), Q(:,97), ZERO, 0_intkind1, wf8(:,19), n2(20))
  call vert_AV_Q(ntry, wf4(:,11), ex6, wf8(:,20), n3(:,33), t3x8(:,:,9))
  call vert_WQ_A(ntry, wf3(:,2), wf4(:,13), wf12(:,8), n3(:,34), t3x12(:,:,8))
  call prop_A_Q(ntry, wf8(:,20), Q(:,50), ZERO, 0_intkind1, wf8(:,21), n2(21))
  call vert_AV_Q(ntry, wf4(:,11), wf4(:,7), wf16(:,11), n3(:,35), t3x16(:,:,8))
  call vert_VQ_A(ntry, wf4(:,7), ex1, wf8(:,22), n3(:,36), t3x8(:,:,10))
  call prop_Q_A(ntry, wf8(:,22), Q(:,97), ZERO, 0_intkind1, wf8(:,23), n2(22))
  call vert_VQ_A(ntry, ex6, wf6(:,2), wf12(:,9), n3(:,37), t3x12(:,:,9))
  call vert_VQ_A(ntry, ex7, wf6(:,2), wf12(:,10), n3(:,38), t3x12(:,:,10))
  call vert_VQ_A(ntry, ex5, wf4(:,10), wf8(:,24), n3(:,39), t3x8(:,:,11))
  call prop_Q_A(ntry, wf8(:,24), Q(:,49), ZERO, 0_intkind1, wf8(:,25), n2(23))
  call vert_AV_Q(ntry, wf4(:,6), ex5, wf8(:,26), n3(:,40), t3x8(:,:,12))
  call prop_A_Q(ntry, wf8(:,26), Q(:,82), ZERO, 0_intkind1, wf8(:,27), n2(24))
  call vert_UV_W(ntry, ex5, Q(:,16), ex7, Q(:,64), wf4(:,14), n3(:,41), t3x4(:,:,8))
  call vert_VQ_A(ntry, wf4(:,14), wf4(:,10), wf16(:,12), n3(:,42), t3x16(:,:,9))
  call vert_AV_Q(ntry, ex2, wf4(:,14), wf8(:,28), n3(:,43), t3x8(:,:,13))
  call prop_A_Q(ntry, wf8(:,28), Q(:,82), ZERO, 0_intkind1, wf8(:,29), n2(25))
  call vert_AV_Q(ntry, wf6(:,4), ex5, wf12(:,11), n3(:,44), t3x12(:,:,11))
  call vert_VQ_A(ntry, ex5, wf4(:,13), wf8(:,30), n3(:,45), t3x8(:,:,14))
  call prop_Q_A(ntry, wf8(:,30), Q(:,81), ZERO, 0_intkind1, wf8(:,31), n2(26))
  call vert_AV_Q(ntry, wf4(:,4), ex5, wf8(:,32), n3(:,46), t3x8(:,:,15))
  call prop_A_Q(ntry, wf8(:,32), Q(:,50), ZERO, 0_intkind1, wf8(:,33), n2(27))
  call vert_AV_Q(ntry, wf4(:,4), wf4(:,14), wf16(:,13), n3(:,47), t3x16(:,:,10))
  call vert_VQ_A(ntry, wf4(:,14), ex1, wf8(:,34), n3(:,48), t3x8(:,:,16))
  call prop_Q_A(ntry, wf8(:,34), Q(:,81), ZERO, 0_intkind1, wf8(:,35), n2(28))
  call vert_VQ_A(ntry, ex5, wf6(:,2), wf12(:,12), n3(:,49), t3x12(:,:,12))
  call vert_UV_W(ntry, ex5, Q(:,16), ex6, Q(:,32), wf4(:,15), n3(:,50), t3x4(:,:,9))
  call vert_VQ_A(ntry, wf4(:,15), wf4(:,13), wf16(:,14), n3(:,51), t3x16(:,:,11))
  call vert_AV_Q(ntry, ex2, wf4(:,15), wf8(:,36), n3(:,52), t3x8(:,:,17))
  call prop_A_Q(ntry, wf8(:,36), Q(:,50), ZERO, 0_intkind1, wf8(:,37), n2(29))
  call vert_AV_Q(ntry, wf4(:,6), wf4(:,15), wf16(:,15), n3(:,53), t3x16(:,:,12))
  call vert_VQ_A(ntry, wf4(:,15), ex1, wf8(:,38), n3(:,54), t3x8(:,:,18))
  call prop_Q_A(ntry, wf8(:,38), Q(:,49), ZERO, 0_intkind1, wf8(:,39), n2(30))
  call vert_UV_W(ntry, wf4(:,15), Q(:,48), ex7, Q(:,64), wf8(:,40), n3(:,55), t3x8(:,:,19))
  call vert_QA_V(ntry, wf6(:,2), ex2, wf12(:,13), n3(:,56), t3x12(:,:,13))
  call vert_QA_V(ntry, ex1, wf6(:,4), wf12(:,14), n3(:,57), t3x12(:,:,14))
  call vert_VQ_A(ntry, ex7, wf8(:,39), wf16(:,16), n3(:,58), t3x16(:,:,13))
  call vert_UV_W(ntry, ex6, Q(:,32), wf4(:,14), Q(:,80), wf8(:,41), n3(:,59), t3x8(:,:,20))
  call vert_VQ_A(ntry, ex6, wf8(:,35), wf16(:,17), n3(:,60), t3x16(:,:,14))
  call vert_UV_W(ntry, ex5, Q(:,16), wf4(:,7), Q(:,96), wf8(:,42), n3(:,61), t3x8(:,:,21))
  call vert_VQ_A(ntry, ex5, wf8(:,23), wf16(:,18), n3(:,62), t3x16(:,:,15))


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
  M2add = M2 / average_factor_pphwjj_udxhwggg_1

  do k = 0, 30-1
    M2(k) = M2add(extcomb_perm_pphwjj_udxhwggg_1(k))
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

    call cont_QA(nsync, wf16(:,1), wf6(:,2), A(:,1), n3(:,63), t3x96(:,:,1), nhel, den(5))
    call cont_QA(nsync, wf6(:,2), wf16(:,2), A(:,2), n3(:,64), t3x96(:,:,2), nhel, den(5))
    call cont_QA(nsync, wf6(:,2), wf16(:,3), A(:,3), n3(:,65), t3x96(:,:,3), nhel, den(5))
    call cont_QA(nsync, wf6(:,3), wf16(:,5), A(:,4), n3(:,66), t3x96(:,:,4), nhel, den(8))
    call cont_QA(nsync, wf6(:,3), wf16(:,7), A(:,5), n3(:,67), t3x96(:,:,5), nhel, den(8))
    call cont_QA(nsync, wf6(:,3), wf16(:,9), A(:,6), n3(:,68), t3x96(:,:,6), nhel, den(8))
    call cont_QA(nsync, wf12(:,1), wf8(:,5), A(:,7), n3(:,69), t3x96(:,:,7), nhel, den(14))
    call cont_QA(nsync, wf12(:,2), wf8(:,7), A(:,8), n3(:,70), t3x96(:,:,8), nhel, den(18))
    call cont_QA(nsync, wf12(:,3), wf8(:,9), A(:,9), n3(:,71), t3x96(:,:,9), nhel, den(23))
    call cont_QA(nsync, wf12(:,2), wf8(:,11), A(:,10), n3(:,72), t3x96(:,:,10), nhel, den(25))
    call cont_QA(nsync, wf16(:,10), wf6(:,4), A(:,11), n3(:,73), t3x96(:,:,11), nhel, den(30))
    call cont_QA(nsync, wf12(:,2), wf8(:,13), A(:,12), n3(:,74), t3x96(:,:,12), nhel, den(32))
    call cont_QA(nsync, wf8(:,9), wf12(:,4), A(:,13), n3(:,75), t3x96(:,:,13), nhel, den(33))
    call cont_QA(nsync, wf8(:,5), wf12(:,5), A(:,14), n3(:,76), t3x96(:,:,14), nhel, den(34))
    call cont_QA(nsync, wf12(:,6), wf8(:,15), A(:,15), n3(:,77), t3x96(:,:,15), nhel, den(40))
    call cont_QA(nsync, wf12(:,7), wf8(:,17), A(:,16), n3(:,78), t3x96(:,:,16), nhel, den(44))
    call cont_QA(nsync, wf12(:,6), wf8(:,19), A(:,17), n3(:,79), t3x96(:,:,17), nhel, den(47))
    call cont_QA(nsync, wf12(:,8), wf8(:,21), A(:,18), n3(:,80), t3x96(:,:,18), nhel, den(51))
    call cont_QA(nsync, wf6(:,2), wf16(:,11), A(:,19), n3(:,81), t3x96(:,:,19), nhel, den(53))
    call cont_QA(nsync, wf12(:,6), wf8(:,23), A(:,20), n3(:,82), t3x96(:,:,20), nhel, den(55))
    call cont_QA(nsync, wf8(:,17), wf12(:,9), A(:,21), n3(:,83), t3x96(:,:,21), nhel, den(56))
    call cont_QA(nsync, wf8(:,21), wf12(:,10), A(:,22), n3(:,84), t3x96(:,:,22), nhel, den(57))
    call cont_QA(nsync, wf12(:,3), wf8(:,25), A(:,23), n3(:,85), t3x96(:,:,23), nhel, den(59))
    call cont_QA(nsync, wf12(:,7), wf8(:,27), A(:,24), n3(:,86), t3x96(:,:,24), nhel, den(61))
    call cont_QA(nsync, wf6(:,4), wf16(:,12), A(:,25), n3(:,87), t3x96(:,:,25), nhel, den(64))
    call cont_QA(nsync, wf12(:,7), wf8(:,29), A(:,26), n3(:,88), t3x96(:,:,26), nhel, den(66))
    call cont_QA(nsync, wf12(:,4), wf8(:,25), A(:,27), n3(:,89), t3x96(:,:,27), nhel, den(67))
    call cont_QA(nsync, wf8(:,15), wf12(:,11), A(:,28), n3(:,90), t3x96(:,:,28), nhel, den(68))
    call cont_QA(nsync, wf12(:,1), wf8(:,31), A(:,29), n3(:,91), t3x96(:,:,29), nhel, den(70))
    call cont_QA(nsync, wf12(:,8), wf8(:,33), A(:,30), n3(:,92), t3x96(:,:,30), nhel, den(72))
    call cont_QA(nsync, wf6(:,2), wf16(:,13), A(:,31), n3(:,93), t3x96(:,:,31), nhel, den(74))
    call cont_QA(nsync, wf12(:,1), wf8(:,35), A(:,32), n3(:,94), t3x96(:,:,32), nhel, den(76))
    call cont_QA(nsync, wf8(:,7), wf12(:,12), A(:,33), n3(:,95), t3x96(:,:,33), nhel, den(77))
    call cont_QA(nsync, wf12(:,10), wf8(:,33), A(:,34), n3(:,96), t3x96(:,:,34), nhel, den(78))
    call cont_QA(nsync, wf6(:,4), wf16(:,14), A(:,35), n3(:,97), t3x96(:,:,35), nhel, den(81))
    call cont_QA(nsync, wf12(:,8), wf8(:,37), A(:,36), n3(:,98), t3x96(:,:,36), nhel, den(83))
    call cont_QA(nsync, wf6(:,2), wf16(:,15), A(:,37), n3(:,99), t3x96(:,:,37), nhel, den(85))
    call cont_QA(nsync, wf12(:,3), wf8(:,39), A(:,38), n3(:,100), t3x96(:,:,38), nhel, den(87))
    call cont_VV(nsync, wf8(:,40), wf12(:,13), A(:,39), n3(:,101), t3x96(:,:,39), nhel, den(89))
    call cont_VV(nsync, wf8(:,40), wf12(:,14), A(:,40), n3(:,102), t3x96(:,:,40), nhel, den(90))
    call cont_QA(nsync, wf12(:,10), wf8(:,37), A(:,41), n3(:,103), t3x96(:,:,41), nhel, den(91))
    call cont_QA(nsync, wf6(:,4), wf16(:,16), A(:,42), n3(:,104), t3x96(:,:,42), nhel, den(92))
    call cont_QA(nsync, wf12(:,5), wf8(:,31), A(:,43), n3(:,105), t3x96(:,:,43), nhel, den(93))
    call cont_QA(nsync, wf8(:,19), wf12(:,11), A(:,44), n3(:,106), t3x96(:,:,44), nhel, den(94))
    call cont_QA(nsync, wf8(:,11), wf12(:,12), A(:,45), n3(:,107), t3x96(:,:,45), nhel, den(95))
    call cont_QA(nsync, wf12(:,9), wf8(:,27), A(:,46), n3(:,108), t3x96(:,:,46), nhel, den(96))
    call cont_VV(nsync, wf12(:,13), wf8(:,41), A(:,47), n3(:,109), t3x96(:,:,47), nhel, den(98))
    call cont_VV(nsync, wf12(:,14), wf8(:,41), A(:,48), n3(:,110), t3x96(:,:,48), nhel, den(99))
    call cont_QA(nsync, wf12(:,9), wf8(:,29), A(:,49), n3(:,111), t3x96(:,:,49), nhel, den(100))
    call cont_QA(nsync, wf6(:,4), wf16(:,17), A(:,50), n3(:,112), t3x96(:,:,50), nhel, den(101))
    call cont_VV(nsync, wf12(:,13), wf8(:,42), A(:,51), n3(:,113), t3x96(:,:,51), nhel, den(103))
    call cont_VV(nsync, wf12(:,14), wf8(:,42), A(:,52), n3(:,114), t3x96(:,:,52), nhel, den(104))
    call cont_QA(nsync, wf8(:,13), wf12(:,12), A(:,53), n3(:,115), t3x96(:,:,53), nhel, den(105))
    call cont_QA(nsync, wf6(:,4), wf16(:,18), A(:,54), n3(:,116), t3x96(:,:,54), nhel, den(106))

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
  complex(REALKIND), intent(out) :: M1(6) ! M1helarray(6,96)
  integer :: empty(0)

  M1(1) = (-A(j,1)%j+A(j,2)%j-A(j,4)%j+A(j,5)%j-A(j,17)%j-A(j,18)%j-A(j,22)%j-A(j,39)%j-A(j,40)%j-A(j,44)%j-A(j,51)%j &
       -A(j,52)%j)*f(1)+CI*(-A(j,19)%j-A(j,20)%j-A(j,35)%j-A(j,36)%j-A(j,41)%j-A(j,54)%j)*f(2)
  M1(2) = (-A(j,2)%j+A(j,3)%j-A(j,5)%j+A(j,6)%j-A(j,15)%j-A(j,16)%j-A(j,21)%j-A(j,28)%j+A(j,47)%j+A(j,48)%j+A(j,51)%j &
       +A(j,52)%j)*f(1)+CI*(A(j,19)%j+A(j,20)%j-A(j,25)%j-A(j,26)%j-A(j,49)%j+A(j,54)%j)*f(2)
  M1(3) = (A(j,1)%j-A(j,3)%j+A(j,4)%j-A(j,6)%j-A(j,29)%j-A(j,30)%j-A(j,34)%j+A(j,39)%j+A(j,40)%j-A(j,43)%j-A(j,47)%j &
       -A(j,48)%j)*f(1)+CI*(-A(j,31)%j-A(j,32)%j+A(j,35)%j+A(j,36)%j+A(j,41)%j-A(j,50)%j)*f(2)
  M1(4) = (-A(j,2)%j+A(j,3)%j-A(j,5)%j+A(j,6)%j-A(j,7)%j-A(j,8)%j-A(j,14)%j-A(j,33)%j+A(j,47)%j+A(j,48)%j+A(j,51)%j &
       +A(j,52)%j)*f(1)+CI*(-A(j,11)%j-A(j,12)%j+A(j,31)%j+A(j,32)%j+A(j,50)%j-A(j,53)%j)*f(2)
  M1(5) = (A(j,1)%j-A(j,3)%j+A(j,4)%j-A(j,6)%j-A(j,23)%j-A(j,24)%j-A(j,27)%j+A(j,39)%j+A(j,40)%j-A(j,46)%j-A(j,47)%j &
       -A(j,48)%j)*f(1)+CI*(A(j,25)%j+A(j,26)%j-A(j,37)%j-A(j,38)%j-A(j,42)%j+A(j,49)%j)*f(2)
  M1(6) = (-A(j,1)%j+A(j,2)%j-A(j,4)%j+A(j,5)%j-A(j,9)%j-A(j,10)%j-A(j,13)%j-A(j,39)%j-A(j,40)%j-A(j,45)%j-A(j,51)%j &
       -A(j,52)%j)*f(1)+CI*(A(j,11)%j+A(j,12)%j+A(j,37)%j+A(j,38)%j+A(j,42)%j+A(j,53)%j)*f(2)

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
  use ol_colourmatrix_pphwjj_udxhwggg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(6)
  real(REALKIND),    intent(out) :: M2colint(0:30-1)
  integer,           intent(in)  :: extcombs(:)
  integer :: extcomb, colmatpos, i, j, eco

  M2colint = 0

  do eco = 1, size(extcombs)
    extcomb = extcombs(eco)
    colmatpos = 6*extcomb
    do i = 1, 6
      do j = 1, 6
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
  use ol_colourmatrix_pphwjj_udxhwggg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(6)
  complex(REALKIND), intent(in)  :: M2(6)
  real(REALKIND),    intent(out) :: M2colint
  integer :: i, j

  M2colint = 0

  do i = 1, 6
    do j = 1, 6
      M2colint = M2colint + real(conjg(M1(i))*K1(i,j)*M2(j))
    end do
  end do

end subroutine colintmunu

end subroutine amp2


#ifdef PRECISION_dp
subroutine colourvector(M1out, nhelout) &
  & bind(c,name="ol_tree_colvect_pphwjj_udxhwggg_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(6,96)
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
    & bind(c,name="ol_f_amp2tree_pphwjj_udxhwggg_1")
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
    & bind(c,name="ol_f_amp2ccone_pphwjj_udxhwggg_1")
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
    & bind(c,name="ol_f_amp2ccall_pphwjj_udxhwggg_1")
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
    & bind(c,name="ol_f_amp2hcone_pphwjj_udxhwggg_1")
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
    & bind(c,name="ol_f_amp2hcall_pphwjj_udxhwggg_1")
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
    & bind(c,name="ol_amp2tree_pphwjj_udxhwggg_1")
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
    & bind(c,name="ol_amp2ccone_pphwjj_udxhwggg_1")
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
    & bind(c,name="ol_amp2ccall_pphwjj_udxhwggg_1")
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
    & bind(c,name="ol_amp2hcone_pphwjj_udxhwggg_1")
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
    & bind(c,name="ol_amp2hcall_pphwjj_udxhwggg_1")
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
    & bind(c,name="amp2tree_pphwjj_udxhwggg_1_")
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
    & bind(c,name="amp2ccone_pphwjj_udxhwggg_1_")
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
    & bind(c,name="amp2ccall_pphwjj_udxhwggg_1_")
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
    & bind(c,name="amp2hcone_pphwjj_udxhwggg_1_")
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
    & bind(c,name="amp2hcall_pphwjj_udxhwggg_1_")
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

end module ol_tree_pphwjj_udxhwggg_1_/**/REALKIND
