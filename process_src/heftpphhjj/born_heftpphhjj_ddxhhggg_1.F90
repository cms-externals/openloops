
module ol_colourmatrix_heftpphhjj_ddxhhggg_1_/**/REALKIND
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
end module ol_colourmatrix_heftpphhjj_ddxhhggg_1_/**/REALKIND



module ol_forced_parameters_heftpphhjj_ddxhhggg_1_/**/REALKIND
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
  if (MB /= 0) write(*,101) 'MB = 0'
  if (YB /= 0) write(*,101) 'YB = 0'
  if (CKMORDER /= 0) write(*,101) 'CKMORDER = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 5) write(*,101) 'nf = 5'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_heftpphhjj_ddxhhggg_1_/**/REALKIND

module ol_tree_heftpphhjj_ddxhhggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(4)
  complex(REALKIND), save :: den(231)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 32 ! number of helicity configurations
  integer(intkind2), save :: nhel = 32 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(32) ! physical helicity states
  complex(DREALKIND) :: M1helarr(6,32) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = (CI*eQED**2*gQCD**5)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f(2) = (eQED**2*gQCD**5)/(MW**2*pi**2*sw**2*48._/**/REALKIND)
    f(3) = (CI*eQED**2*gQCD**5*lambdaHHH*MH**2)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(4) = (eQED**2*gQCD**5*lambdaHHH*MH**2)/(MW**2*pi**2*sw**2*16._/**/REALKIND)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,3))
  den(2) = 1 / (Q(5,12) - MH2)
  den(4) = 1 / (Q(5,28))
  den(6) = 1 / (Q(5,44))
  den(8) = 1 / (Q(5,76))
  den(10) = 1 / (Q(5,112))
  den(12) = 1 / (Q(5,48))
  den(14) = 1 / (Q(5,80))
  den(16) = 1 / (Q(5,108))
  den(18) = 1 / (Q(5,96))
  den(20) = 1 / (Q(5,92))
  den(22) = 1 / (Q(5,60))
  den(24) = 1 / (Q(5,17))
  den(26) = 1 / (Q(5,18))
  den(28) = 1 / (Q(5,33))
  den(30) = 1 / (Q(5,34))
  den(32) = 1 / (Q(5,65))
  den(34) = 1 / (Q(5,66))
  den(38) = 1 / (Q(5,19))
  den(44) = 1 / (Q(5,35))
  den(49) = 1 / (Q(5,67))
  den(92) = 1 / (Q(5,78))
  den(98) = 1 / (Q(5,46))
  den(105) = 1 / (Q(5,77))
  den(111) = 1 / (Q(5,45))
  den(122) = 1 / (Q(5,49))
  den(130) = 1 / (Q(5,81))
  den(137) = 1 / (Q(5,30))
  den(143) = 1 / (Q(5,29))
  den(150) = 1 / (Q(5,97))
  den(186) = 1 / (Q(5,50))
  den(189) = 1 / (Q(5,82))
  den(206) = 1 / (Q(5,98))

  ! denominators

  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(7) = den(1)*den(6)
  den(9) = den(1)*den(8)
  den(11) = den(1)*den(10)
  den(13) = den(1)*den(12)
  den(15) = den(1)*den(14)
  den(17) = den(1)*den(16)
  den(19) = den(1)*den(18)
  den(21) = den(1)*den(20)
  den(23) = den(1)*den(22)
  den(25) = den(16)*den(24)
  den(27) = den(16)*den(26)
  den(29) = den(20)*den(28)
  den(31) = den(20)*den(30)
  den(33) = den(22)*den(32)
  den(35) = den(22)*den(34)
  den(36) = den(3)*den(12)
  den(37) = den(3)*den(14)
  den(39) = den(1)*den(38)
  den(40) = den(2)*den(39)
  den(41) = den(2)*den(4)
  den(42) = den(1)*den(41)
  den(43) = den(3)*den(18)
  den(45) = den(1)*den(44)
  den(46) = den(2)*den(45)
  den(47) = den(2)*den(6)
  den(48) = den(1)*den(47)
  den(50) = den(1)*den(49)
  den(51) = den(2)*den(50)
  den(52) = den(2)*den(8)
  den(53) = den(1)*den(52)
  den(54) = den(3)*den(10)
  den(55) = den(12)*den(50)
  den(56) = den(10)*den(12)
  den(57) = den(1)*den(56)
  den(58) = den(8)*den(13)
  den(59) = den(14)*den(45)
  den(60) = den(10)*den(14)
  den(61) = den(1)*den(60)
  den(62) = den(18)*den(39)
  den(63) = den(10)*den(18)
  den(64) = den(1)*den(63)
  den(65) = den(8)*den(39)
  den(66) = den(8)*den(20)
  den(67) = den(1)*den(66)
  den(68) = den(6)*den(15)
  den(69) = den(6)*den(39)
  den(70) = den(6)*den(22)
  den(71) = den(1)*den(70)
  den(72) = den(4)*den(19)
  den(73) = den(4)*den(45)
  den(74) = den(4)*den(22)
  den(75) = den(1)*den(74)
  den(76) = den(24)*den(38)
  den(77) = den(2)*den(76)
  den(78) = den(26)*den(38)
  den(79) = den(2)*den(78)
  den(80) = den(28)*den(44)
  den(81) = den(2)*den(80)
  den(82) = den(30)*den(44)
  den(83) = den(2)*den(82)
  den(84) = den(32)*den(49)
  den(85) = den(2)*den(84)
  den(86) = den(34)*den(49)
  den(87) = den(2)*den(86)
  den(88) = den(24)*den(30)
  den(89) = den(8)*den(88)
  den(90) = den(18)*den(76)
  den(91) = den(8)*den(76)
  den(93) = den(8)*den(92)
  den(94) = den(24)*den(93)
  den(95) = den(24)*den(34)
  den(96) = den(6)*den(95)
  den(97) = den(6)*den(76)
  den(99) = den(6)*den(98)
  den(100) = den(24)*den(99)
  den(101) = den(26)*den(28)
  den(102) = den(8)*den(101)
  den(103) = den(18)*den(78)
  den(104) = den(8)*den(78)
  den(106) = den(8)*den(105)
  den(107) = den(26)*den(106)
  den(108) = den(26)*den(32)
  den(109) = den(6)*den(108)
  den(110) = den(6)*den(78)
  den(112) = den(6)*den(111)
  den(113) = den(26)*den(112)
  den(114) = den(14)*den(80)
  den(115) = den(8)*den(80)
  den(116) = den(28)*den(93)
  den(117) = den(14)*den(82)
  den(118) = den(8)*den(82)
  den(119) = den(30)*den(106)
  den(120) = den(12)*den(84)
  den(121) = den(12)*den(86)
  den(123) = den(12)*den(122)
  den(124) = den(8)*den(123)
  den(125) = den(12)*den(106)
  den(126) = den(6)*den(84)
  den(127) = den(32)*den(99)
  den(128) = den(6)*den(86)
  den(129) = den(34)*den(112)
  den(131) = den(14)*den(130)
  den(132) = den(6)*den(131)
  den(133) = den(14)*den(112)
  den(134) = den(28)*den(34)
  den(135) = den(4)*den(134)
  den(136) = den(4)*den(80)
  den(138) = den(4)*den(137)
  den(139) = den(28)*den(138)
  den(140) = den(30)*den(32)
  den(141) = den(4)*den(140)
  den(142) = den(4)*den(82)
  den(144) = den(4)*den(143)
  den(145) = den(30)*den(144)
  den(146) = den(4)*den(84)
  den(147) = den(32)*den(138)
  den(148) = den(4)*den(86)
  den(149) = den(34)*den(144)
  den(151) = den(18)*den(150)
  den(152) = den(4)*den(151)
  den(153) = den(18)*den(144)
  den(154) = den(2)*den(12)
  den(155) = den(50)*den(154)
  den(156) = den(13)*den(52)
  den(157) = den(3)*den(56)
  den(158) = den(2)*den(14)
  den(159) = den(45)*den(158)
  den(160) = den(15)*den(47)
  den(161) = den(3)*den(60)
  den(162) = den(2)*den(18)
  den(163) = den(39)*den(162)
  den(164) = den(19)*den(41)
  den(165) = den(3)*den(63)
  den(166) = den(39)*den(47)
  den(167) = den(39)*den(52)
  den(168) = den(41)*den(45)
  den(169) = den(45)*den(52)
  den(170) = den(41)*den(50)
  den(171) = den(47)*den(50)
  den(172) = den(52)*den(88)
  den(173) = den(47)*den(95)
  den(174) = den(76)*den(162)
  den(175) = den(52)*den(76)
  den(176) = den(47)*den(76)
  den(177) = den(24)*den(122)
  den(178) = den(52)*den(177)
  den(179) = den(24)*den(130)
  den(180) = den(47)*den(179)
  den(181) = den(52)*den(101)
  den(182) = den(47)*den(108)
  den(183) = den(78)*den(162)
  den(184) = den(52)*den(78)
  den(185) = den(47)*den(78)
  den(187) = den(26)*den(186)
  den(188) = den(52)*den(187)
  den(190) = den(26)*den(189)
  den(191) = den(47)*den(190)
  den(192) = den(41)*den(134)
  den(193) = den(80)*den(158)
  den(194) = den(52)*den(80)
  den(195) = den(41)*den(80)
  den(196) = den(28)*den(122)
  den(197) = den(52)*den(196)
  den(198) = den(28)*den(150)
  den(199) = den(41)*den(198)
  den(200) = den(41)*den(140)
  den(201) = den(82)*den(158)
  den(202) = den(52)*den(82)
  den(203) = den(41)*den(82)
  den(204) = den(30)*den(186)
  den(205) = den(52)*den(204)
  den(207) = den(30)*den(206)
  den(208) = den(41)*den(207)
  den(209) = den(84)*den(154)
  den(210) = den(86)*den(154)
  den(211) = den(52)*den(123)
  den(212) = den(12)*den(186)
  den(213) = den(52)*den(212)
  den(214) = den(47)*den(84)
  den(215) = den(41)*den(84)
  den(216) = den(32)*den(130)
  den(217) = den(47)*den(216)
  den(218) = den(32)*den(150)
  den(219) = den(41)*den(218)
  den(220) = den(47)*den(86)
  den(221) = den(41)*den(86)
  den(222) = den(34)*den(189)
  den(223) = den(47)*den(222)
  den(224) = den(34)*den(206)
  den(225) = den(41)*den(224)
  den(226) = den(47)*den(131)
  den(227) = den(14)*den(189)
  den(228) = den(47)*den(227)
  den(229) = den(18)*den(206)
  den(230) = den(41)*den(229)
  den(231) = den(41)*den(151)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_heftpphhjj_ddxhhggg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_heftpphhjj_ddxhhggg_1_/**/DREALKIND, only: &
    & ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for down anti-down higgs higgs glue glue glue -> 0
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
  use ol_external_heftpphhjj_ddxhhggg_1, only: &
    & external_perm_heftpphhjj_ddxhhggg_1, &
    & external_perm_inv_heftpphhjj_ddxhhggg_1, &
    & extcomb_perm_heftpphhjj_ddxhhggg_1, &
    & average_factor_heftpphhjj_ddxhhggg_1
  use ol_external_heftpphhjj_ddxhhggg_1, only: &
    & H, hel_not_initialised, hel_init, POLSEL
  use ol_colourmatrix_heftpphhjj_ddxhhggg_1_/**/REALKIND, only: &
    & colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_heftpphhjj_ddxhhggg_1_/**/REALKIND, only: &
    & check_forced_parameters
  use ol_heltables_heftpphhjj_ddxhhggg_1
  use ol_kinematics_/**/REALKIND, only: LC2Std_Rep_cmplx
  implicit none

  real(DREALKIND), intent(in)  :: P_scatt(0:3,7)
  real(REALKIND),  intent(out) :: M2(0:30-1)
  real(REALKIND),  intent(out) :: M2munu(4,4)
  integer,         intent(in)  :: I
  real(REALKIND),  intent(in)  :: MOM(0:3)
  integer,         intent(in)  :: nextcombs
  integer,         intent(in)  :: extcombs(nextcombs)

  integer           :: ReplacePol, JBmunu, extcombs_permuted(nextcombs), shift, k, r, m, n
  real(REALKIND)    :: P(0:3,7)
  real(REALKIND)    :: extmasses2(7)
  real(REALKIND)    :: M2add(0:30-1)
  complex(REALKIND) :: M2munuadd(2)
  complex(REALKIND) :: MOM_LC(4), M1(6), M1helarray(6,32)
  real(REALKIND)    :: P_scatt_intern(0:3,7)
  complex(REALKIND) :: epLC(1:4), epStd(1:4,2)
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(1), ex4(1), ex5(2), ex6(2), ex7(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf1(1,1), wf2(2,6), wf4(4,55), wf8(8,60), wf16(16,45), wf32(32,171)

  type(polcont) :: A(32,168)
  complex(REALKIND) :: Aj(168)

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
  extmasses2 = [ rZERO2, rZERO2, rMH2, rMH2, rZERO2, rZERO2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, &
    & external_perm_inv_heftpphhjj_ddxhhggg_1,7)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,7)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_heftpphhjj_ddxhhggg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_heftpphhjj_ddxhhggg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_heftpphhjj_ddxhhggg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  42 continue
  ! external WFs
  call pol_wf_Q(P(:,1), rZERO, H1, ex1, POLSEL(1))
  call pol_wf_A(P(:,2), rZERO, H2, ex2, POLSEL(2))
  call pol_wf_S(P(:,3), rMH, H3, ex3, POLSEL(3))
  call pol_wf_S(P(:,4), rMH, H4, ex4, POLSEL(4))
  call pol_wf_V(P(:,5), rZERO, H5, ex5, POLSEL(5))
  call pol_wf_V(P(:,6), rZERO, H6, ex6, POLSEL(6))
  call pol_wf_V(P(:,7), rZERO, H7, ex7, POLSEL(7))


  if (ntry == 1) then
    shift = 1
    ! call helbookkeeping_flip(H1, 1, shift, eflip, exthel, firstpol)
    ! call helbookkeeping_wf(H1, ex1, shift) ...

    if (any(POLSEL /= 0)) then

      call pol_wf_Q(P(:,1), rZERO, H1, ex1, 0)
      call pol_wf_A(P(:,2), rZERO, H2, ex2, 0)
      call pol_wf_S(P(:,3), rMH, H3, ex3, 0)
      call pol_wf_S(P(:,4), rMH, H4, ex4, 0)
      call pol_wf_V(P(:,5), rZERO, H5, ex5, 0)
      call pol_wf_V(P(:,6), rZERO, H6, ex6, 0)
      call pol_wf_V(P(:,7), rZERO, H7, ex7, 0)

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

  end if

  ! internal WFs
  ! e.g. call vert_VQ_A(ntry, ex3, ex1, wf1, n1, t1) ...
  call vert_QA_V(ntry, ex1, ex2, wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_HHGGG_G(ntry, ex3, ex4, ex5, ex6, ex7, wf8(:,1), n6(:,1), t6x8(:,:,1))
  call vert_HHGGG_G(ntry, ex3, ex4, ex6, ex7, ex5, wf8(:,2), n6(:,2), t6x8(:,:,2))
  call vert_HHGGG_G(ntry, ex3, ex4, ex7, ex5, ex6, wf8(:,3), n6(:,3), t6x8(:,:,3))
  call vert_SS_S(ntry, ex3, ex4, wf1(:,1), n3(:,2), t3x1(:,:,1))
  call vert_GGGG_H(ntry, wf4(:,1), ex5, ex6, ex7, wf32(:,1), n5(:,1), t5x32(:,:,1))
  call vert_GGGG_H(ntry, wf4(:,1), ex6, ex7, ex5, wf32(:,2), n5(:,2), t5x32(:,:,2))
  call vert_GGGG_H(ntry, wf4(:,1), ex7, ex5, ex6, wf32(:,3), n5(:,3), t5x32(:,:,3))
  call vert_HHG_G(ntry, ex3, ex4, ex5, Q(:,16), wf2(:,1), Q(:,28), n4(:,1), t4x2(:,:,1))
  call vert_GGG_G(ntry, wf4(:,1), ex6, ex7, wf16(:,1), n4(:,2), t4x16(:,:,1))
  call vert_GGG_G(ntry, ex6, ex7, wf4(:,1), wf16(:,2), n4(:,3), t4x16(:,:,2))
  call vert_GGG_G(ntry, ex7, wf4(:,1), ex6, wf16(:,3), n4(:,4), t4x16(:,:,3))
  call vert_HHG_G(ntry, ex3, ex4, ex6, Q(:,32), wf2(:,2), Q(:,44), n4(:,5), t4x2(:,:,2))
  call vert_GGG_G(ntry, wf4(:,1), ex5, ex7, wf16(:,4), n4(:,6), t4x16(:,:,4))
  call vert_GGG_G(ntry, ex5, ex7, wf4(:,1), wf16(:,5), n4(:,7), t4x16(:,:,5))
  call vert_GGG_G(ntry, ex7, wf4(:,1), ex5, wf16(:,6), n4(:,8), t4x16(:,:,6))
  call vert_HHG_G(ntry, ex3, ex4, ex7, Q(:,64), wf2(:,3), Q(:,76), n4(:,9), t4x2(:,:,3))
  call vert_GGG_G(ntry, wf4(:,1), ex5, ex6, wf16(:,7), n4(:,10), t4x16(:,:,7))
  call vert_GGG_G(ntry, ex5, ex6, wf4(:,1), wf16(:,8), n4(:,11), t4x16(:,:,8))
  call vert_GGG_G(ntry, ex6, wf4(:,1), ex5, wf16(:,9), n4(:,12), t4x16(:,:,9))
  call vert_GGG_G(ntry, ex5, ex6, ex7, wf8(:,4), n4(:,13), t4x8(:,:,1))
  call vert_HHG_G(ntry, ex3, ex4, wf4(:,1), Q(:,3), wf4(:,2), Q(:,15), n4(:,14), t4x4(:,:,1))
  call vert_GGG_G(ntry, ex6, ex7, ex5, wf8(:,5), n4(:,15), t4x8(:,:,2))
  call vert_GGG_G(ntry, ex7, ex5, ex6, wf8(:,6), n4(:,16), t4x8(:,:,3))
  call vert_UV_W(ntry, ex5, Q(:,16), ex6, Q(:,32), wf4(:,3), n3(:,3), t3x4(:,:,2))
  call vert_HHGG_G(ntry, ex3, ex4, wf4(:,1), Q(:,3), ex7, Q(:,64), wf8(:,7), Q(:,79), n5(:,4), t5x8(:,:,1))
  call vert_UV_W(ntry, ex5, Q(:,16), ex7, Q(:,64), wf4(:,4), n3(:,4), t3x4(:,:,3))
  call vert_HHGG_G(ntry, ex3, ex4, wf4(:,1), Q(:,3), ex6, Q(:,32), wf8(:,8), Q(:,47), n5(:,5), t5x8(:,:,2))
  call vert_HHGG_G(ntry, ex3, ex4, ex6, Q(:,32), ex7, Q(:,64), wf4(:,5), Q(:,108), n5(:,6), t5x4(:,:,1))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), ex5, Q(:,16), wf8(:,9), n3(:,5), t3x8(:,:,1))
  call vert_UV_W(ntry, ex6, Q(:,32), ex7, Q(:,64), wf4(:,6), n3(:,6), t3x4(:,:,4))
  call vert_HHGG_G(ntry, ex3, ex4, wf4(:,1), Q(:,3), ex5, Q(:,16), wf8(:,10), Q(:,31), n5(:,7), t5x8(:,:,3))
  call vert_HHGG_G(ntry, ex3, ex4, ex5, Q(:,16), ex7, Q(:,64), wf4(:,7), Q(:,92), n5(:,8), t5x4(:,:,2))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), ex6, Q(:,32), wf8(:,11), n3(:,7), t3x8(:,:,2))
  call vert_HHGG_G(ntry, ex3, ex4, ex5, Q(:,16), ex6, Q(:,32), wf4(:,8), Q(:,60), n5(:,9), t5x4(:,:,3))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), ex7, Q(:,64), wf8(:,12), n3(:,8), t3x8(:,:,3))
  call vert_VQ_A(ntry, ex5, ex1, wf4(:,9), n3(:,9), t3x4(:,:,5))
  call prop_Q_A(ntry, wf4(:,9), Q(:,17), ZERO, 0_intkind1, wf4(:,10), n2(1))
  call vert_QA_V(ntry, wf4(:,10), ex2, wf8(:,13), n3(:,10), t3x8(:,:,4))
  call vert_AV_Q(ntry, ex2, ex5, wf4(:,11), n3(:,11), t3x4(:,:,6))
  call prop_A_Q(ntry, wf4(:,11), Q(:,18), ZERO, 0_intkind1, wf4(:,12), n2(2))
  call vert_QA_V(ntry, ex1, wf4(:,12), wf8(:,14), n3(:,12), t3x8(:,:,5))
  call vert_VQ_A(ntry, ex6, ex1, wf4(:,13), n3(:,13), t3x4(:,:,7))
  call prop_Q_A(ntry, wf4(:,13), Q(:,33), ZERO, 0_intkind1, wf4(:,14), n2(3))
  call vert_QA_V(ntry, wf4(:,14), ex2, wf8(:,15), n3(:,14), t3x8(:,:,6))
  call vert_AV_Q(ntry, ex2, ex6, wf4(:,15), n3(:,15), t3x4(:,:,8))
  call prop_A_Q(ntry, wf4(:,15), Q(:,34), ZERO, 0_intkind1, wf4(:,16), n2(4))
  call vert_QA_V(ntry, ex1, wf4(:,16), wf8(:,16), n3(:,16), t3x8(:,:,7))
  call vert_VQ_A(ntry, ex7, ex1, wf4(:,17), n3(:,17), t3x4(:,:,9))
  call prop_Q_A(ntry, wf4(:,17), Q(:,65), ZERO, 0_intkind1, wf4(:,18), n2(5))
  call vert_QA_V(ntry, wf4(:,18), ex2, wf8(:,17), n3(:,18), t3x8(:,:,8))
  call vert_AV_Q(ntry, ex2, ex7, wf4(:,19), n3(:,19), t3x4(:,:,10))
  call prop_A_Q(ntry, wf4(:,19), Q(:,66), ZERO, 0_intkind1, wf4(:,20), n2(6))
  call vert_QA_V(ntry, ex1, wf4(:,20), wf8(:,18), n3(:,20), t3x8(:,:,9))
  call vert_HGG_G(ntry, wf1(:,1), wf4(:,1), Q(:,3), ex7, Q(:,64), wf8(:,19), Q(:,79), n4(:,17), t4x8(:,:,4))
  call vert_HGG_G(ntry, wf1(:,1), wf4(:,1), Q(:,3), ex6, Q(:,32), wf8(:,20), Q(:,47), n4(:,18), t4x8(:,:,5))
  call vert_HGG_G(ntry, wf1(:,1), ex6, Q(:,32), ex7, Q(:,64), wf4(:,21), Q(:,108), n4(:,19), t4x4(:,:,2))
  call vert_HG_G(ntry, wf1(:,1), ex5, Q(:,16), wf2(:,4), Q(:,28), n3(:,21), t3x2(:,:,1))
  call vert_HGG_G(ntry, wf1(:,1), wf4(:,1), Q(:,3), ex5, Q(:,16), wf8(:,21), Q(:,31), n4(:,20), t4x8(:,:,6))
  call vert_HGG_G(ntry, wf1(:,1), ex5, Q(:,16), ex7, Q(:,64), wf4(:,22), Q(:,92), n4(:,21), t4x4(:,:,3))
  call vert_HG_G(ntry, wf1(:,1), ex6, Q(:,32), wf2(:,5), Q(:,44), n3(:,22), t3x2(:,:,2))
  call vert_HGG_G(ntry, wf1(:,1), ex5, Q(:,16), ex6, Q(:,32), wf4(:,23), Q(:,60), n4(:,22), t4x4(:,:,4))
  call vert_HG_G(ntry, wf1(:,1), ex7, Q(:,64), wf2(:,6), Q(:,76), n3(:,23), t3x2(:,:,3))
  call vert_HG_G(ntry, wf1(:,1), wf4(:,1), Q(:,3), wf4(:,24), Q(:,15), n3(:,24), t3x4(:,:,11))
  call vert_HHG_G(ntry, ex3, ex4, wf4(:,3), Q(:,48), wf4(:,25), Q(:,60), n4(:,23), t4x4(:,:,5))
  call vert_UV_W(ntry, wf4(:,3), Q(:,48), ex7, Q(:,64), wf8(:,22), n3(:,25), t3x8(:,:,10))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), wf4(:,3), Q(:,48), wf16(:,10), n3(:,26), t3x16(:,:,1))
  call vert_HHG_G(ntry, ex3, ex4, wf4(:,4), Q(:,80), wf4(:,26), Q(:,92), n4(:,24), t4x4(:,:,6))
  call vert_UV_W(ntry, ex6, Q(:,32), wf4(:,4), Q(:,80), wf8(:,23), n3(:,27), t3x8(:,:,11))
  call vert_HHG_G(ntry, ex3, ex4, wf4(:,6), Q(:,96), wf4(:,27), Q(:,108), n4(:,25), t4x4(:,:,7))
  call vert_UV_W(ntry, ex5, Q(:,16), wf4(:,6), Q(:,96), wf8(:,24), n3(:,28), t3x8(:,:,12))
  call vert_UV_W(ntry, ex6, Q(:,32), wf2(:,3), Q(:,76), wf4(:,28), n3(:,29), t3x4(:,:,12))
  call vert_UV_W(ntry, ex5, Q(:,16), wf2(:,3), Q(:,76), wf4(:,29), n3(:,30), t3x4(:,:,13))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), wf4(:,4), Q(:,80), wf16(:,11), n3(:,31), t3x16(:,:,2))
  call vert_UV_W(ntry, wf2(:,2), Q(:,44), ex7, Q(:,64), wf4(:,30), n3(:,32), t3x4(:,:,14))
  call vert_UV_W(ntry, ex5, Q(:,16), wf2(:,2), Q(:,44), wf4(:,31), n3(:,33), t3x4(:,:,15))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), wf4(:,6), Q(:,96), wf16(:,12), n3(:,34), t3x16(:,:,3))
  call vert_UV_W(ntry, wf2(:,1), Q(:,28), ex7, Q(:,64), wf4(:,32), n3(:,35), t3x4(:,:,16))
  call vert_UV_W(ntry, wf2(:,1), Q(:,28), ex6, Q(:,32), wf4(:,33), n3(:,36), t3x4(:,:,17))
  call vert_QA_V(ntry, wf4(:,10), wf4(:,16), wf16(:,13), n3(:,37), t3x16(:,:,4))
  call vert_AV_Q(ntry, ex2, wf2(:,3), wf4(:,34), n3(:,38), t3x4(:,:,18))
  call vert_VQ_A(ntry, ex6, wf4(:,10), wf8(:,25), n3(:,39), t3x8(:,:,13))
  call prop_A_Q(ntry, wf4(:,34), Q(:,78), ZERO, 0_intkind1, wf4(:,35), n2(7))
  call vert_QA_V(ntry, wf4(:,10), wf4(:,20), wf16(:,14), n3(:,40), t3x16(:,:,5))
  call vert_AV_Q(ntry, ex2, wf2(:,2), wf4(:,36), n3(:,41), t3x4(:,:,19))
  call vert_VQ_A(ntry, ex7, wf4(:,10), wf8(:,26), n3(:,42), t3x8(:,:,14))
  call prop_A_Q(ntry, wf4(:,36), Q(:,46), ZERO, 0_intkind1, wf4(:,37), n2(8))
  call vert_QA_V(ntry, wf4(:,14), wf4(:,12), wf16(:,15), n3(:,43), t3x16(:,:,6))
  call vert_VQ_A(ntry, wf2(:,3), ex1, wf4(:,38), n3(:,44), t3x4(:,:,20))
  call vert_AV_Q(ntry, wf4(:,12), ex6, wf8(:,27), n3(:,45), t3x8(:,:,15))
  call prop_Q_A(ntry, wf4(:,38), Q(:,77), ZERO, 0_intkind1, wf4(:,39), n2(9))
  call vert_QA_V(ntry, wf4(:,18), wf4(:,12), wf16(:,16), n3(:,46), t3x16(:,:,7))
  call vert_VQ_A(ntry, wf2(:,2), ex1, wf4(:,40), n3(:,47), t3x4(:,:,21))
  call vert_AV_Q(ntry, wf4(:,12), ex7, wf8(:,28), n3(:,48), t3x8(:,:,16))
  call prop_Q_A(ntry, wf4(:,40), Q(:,45), ZERO, 0_intkind1, wf4(:,41), n2(10))
  call vert_VQ_A(ntry, ex5, wf4(:,14), wf8(:,29), n3(:,49), t3x8(:,:,17))
  call vert_AV_Q(ntry, wf4(:,16), ex5, wf8(:,30), n3(:,50), t3x8(:,:,18))
  call vert_VQ_A(ntry, wf4(:,3), ex1, wf8(:,31), n3(:,51), t3x8(:,:,19))
  call prop_Q_A(ntry, wf8(:,31), Q(:,49), ZERO, 0_intkind1, wf8(:,32), n2(11))
  call vert_AV_Q(ntry, ex2, wf4(:,3), wf8(:,33), n3(:,52), t3x8(:,:,20))
  call vert_VQ_A(ntry, ex5, wf4(:,18), wf8(:,34), n3(:,53), t3x8(:,:,21))
  call vert_AV_Q(ntry, wf4(:,20), ex5, wf8(:,35), n3(:,54), t3x8(:,:,22))
  call vert_VQ_A(ntry, wf4(:,4), ex1, wf8(:,36), n3(:,55), t3x8(:,:,23))
  call prop_Q_A(ntry, wf8(:,36), Q(:,81), ZERO, 0_intkind1, wf8(:,37), n2(12))
  call vert_AV_Q(ntry, ex2, wf4(:,4), wf8(:,38), n3(:,56), t3x8(:,:,24))
  call vert_QA_V(ntry, wf4(:,14), wf4(:,20), wf16(:,17), n3(:,57), t3x16(:,:,8))
  call vert_AV_Q(ntry, ex2, wf2(:,1), wf4(:,42), n3(:,58), t3x4(:,:,22))
  call vert_VQ_A(ntry, ex7, wf4(:,14), wf8(:,39), n3(:,59), t3x8(:,:,25))
  call prop_A_Q(ntry, wf4(:,42), Q(:,30), ZERO, 0_intkind1, wf4(:,43), n2(13))
  call vert_QA_V(ntry, wf4(:,18), wf4(:,16), wf16(:,18), n3(:,60), t3x16(:,:,9))
  call vert_VQ_A(ntry, wf2(:,1), ex1, wf4(:,44), n3(:,61), t3x4(:,:,23))
  call vert_AV_Q(ntry, wf4(:,16), ex7, wf8(:,40), n3(:,62), t3x8(:,:,26))
  call prop_Q_A(ntry, wf4(:,44), Q(:,29), ZERO, 0_intkind1, wf4(:,45), n2(14))
  call vert_VQ_A(ntry, ex6, wf4(:,18), wf8(:,41), n3(:,63), t3x8(:,:,27))
  call vert_AV_Q(ntry, wf4(:,20), ex6, wf8(:,42), n3(:,64), t3x8(:,:,28))
  call vert_VQ_A(ntry, wf4(:,6), ex1, wf8(:,43), n3(:,65), t3x8(:,:,29))
  call prop_Q_A(ntry, wf8(:,43), Q(:,97), ZERO, 0_intkind1, wf8(:,44), n2(15))
  call vert_AV_Q(ntry, ex2, wf4(:,6), wf8(:,45), n3(:,66), t3x8(:,:,30))
  call vert_HG_G(ntry, wf1(:,1), wf4(:,3), Q(:,48), wf4(:,46), Q(:,60), n3(:,67), t3x4(:,:,24))
  call vert_HG_G(ntry, wf1(:,1), wf4(:,4), Q(:,80), wf4(:,47), Q(:,92), n3(:,68), t3x4(:,:,25))
  call vert_HG_G(ntry, wf1(:,1), wf4(:,6), Q(:,96), wf4(:,48), Q(:,108), n3(:,69), t3x4(:,:,26))
  call vert_UV_W(ntry, wf8(:,9), Q(:,19), ex7, Q(:,64), wf16(:,19), n3(:,70), t3x16(:,:,10))
  call vert_UV_W(ntry, wf8(:,9), Q(:,19), ex6, Q(:,32), wf16(:,20), n3(:,71), t3x16(:,:,11))
  call vert_UV_W(ntry, wf2(:,4), Q(:,28), ex7, Q(:,64), wf4(:,49), n3(:,72), t3x4(:,:,27))
  call vert_UV_W(ntry, ex5, Q(:,16), wf8(:,11), Q(:,35), wf16(:,21), n3(:,73), t3x16(:,:,12))
  call vert_UV_W(ntry, wf2(:,4), Q(:,28), ex6, Q(:,32), wf4(:,50), n3(:,74), t3x4(:,:,28))
  call vert_UV_W(ntry, ex5, Q(:,16), wf2(:,5), Q(:,44), wf4(:,51), n3(:,75), t3x4(:,:,29))
  call vert_UV_W(ntry, wf8(:,13), Q(:,19), ex6, Q(:,32), wf16(:,22), n3(:,76), t3x16(:,:,13))
  call vert_UV_W(ntry, wf8(:,13), Q(:,19), ex7, Q(:,64), wf16(:,23), n3(:,77), t3x16(:,:,14))
  call prop_Q_A(ntry, wf8(:,25), Q(:,49), ZERO, 0_intkind1, wf8(:,46), n2(16))
  call vert_QA_V(ntry, wf8(:,46), ex2, wf16(:,24), n3(:,78), t3x16(:,:,15))
  call prop_Q_A(ntry, wf8(:,26), Q(:,81), ZERO, 0_intkind1, wf8(:,47), n2(17))
  call vert_AV_Q(ntry, ex2, wf2(:,5), wf4(:,52), n3(:,79), t3x4(:,:,30))
  call vert_UV_W(ntry, wf8(:,14), Q(:,19), ex6, Q(:,32), wf16(:,25), n3(:,80), t3x16(:,:,16))
  call vert_UV_W(ntry, wf8(:,14), Q(:,19), ex7, Q(:,64), wf16(:,26), n3(:,81), t3x16(:,:,17))
  call prop_A_Q(ntry, wf8(:,27), Q(:,50), ZERO, 0_intkind1, wf8(:,48), n2(18))
  call vert_QA_V(ntry, ex1, wf8(:,48), wf16(:,27), n3(:,82), t3x16(:,:,18))
  call prop_A_Q(ntry, wf8(:,28), Q(:,82), ZERO, 0_intkind1, wf8(:,49), n2(19))
  call vert_VQ_A(ntry, wf2(:,5), ex1, wf4(:,53), n3(:,83), t3x4(:,:,31))
  call vert_UV_W(ntry, ex5, Q(:,16), wf8(:,15), Q(:,35), wf16(:,28), n3(:,84), t3x16(:,:,19))
  call vert_UV_W(ntry, wf8(:,15), Q(:,35), ex7, Q(:,64), wf16(:,29), n3(:,85), t3x16(:,:,20))
  call prop_Q_A(ntry, wf8(:,29), Q(:,49), ZERO, 0_intkind1, wf8(:,50), n2(20))
  call vert_QA_V(ntry, wf8(:,50), ex2, wf16(:,30), n3(:,86), t3x16(:,:,21))
  call prop_Q_A(ntry, wf8(:,39), Q(:,97), ZERO, 0_intkind1, wf8(:,51), n2(21))
  call vert_AV_Q(ntry, ex2, wf2(:,4), wf4(:,54), n3(:,87), t3x4(:,:,32))
  call vert_UV_W(ntry, ex5, Q(:,16), wf8(:,16), Q(:,35), wf16(:,31), n3(:,88), t3x16(:,:,22))
  call vert_UV_W(ntry, wf8(:,16), Q(:,35), ex7, Q(:,64), wf16(:,32), n3(:,89), t3x16(:,:,23))
  call prop_A_Q(ntry, wf8(:,30), Q(:,50), ZERO, 0_intkind1, wf8(:,52), n2(22))
  call vert_QA_V(ntry, ex1, wf8(:,52), wf16(:,33), n3(:,90), t3x16(:,:,24))
  call prop_A_Q(ntry, wf8(:,40), Q(:,98), ZERO, 0_intkind1, wf8(:,53), n2(23))
  call vert_VQ_A(ntry, wf2(:,4), ex1, wf4(:,55), n3(:,91), t3x4(:,:,33))
  call vert_QA_V(ntry, wf8(:,32), ex2, wf16(:,34), n3(:,92), t3x16(:,:,25))
  call prop_A_Q(ntry, wf8(:,33), Q(:,50), ZERO, 0_intkind1, wf8(:,54), n2(24))
  call vert_QA_V(ntry, ex1, wf8(:,54), wf16(:,35), n3(:,93), t3x16(:,:,26))
  call vert_UV_W(ntry, ex5, Q(:,16), wf8(:,17), Q(:,67), wf16(:,36), n3(:,94), t3x16(:,:,27))
  call vert_UV_W(ntry, ex6, Q(:,32), wf8(:,17), Q(:,67), wf16(:,37), n3(:,95), t3x16(:,:,28))
  call prop_Q_A(ntry, wf8(:,34), Q(:,81), ZERO, 0_intkind1, wf8(:,55), n2(25))
  call vert_QA_V(ntry, wf8(:,55), ex2, wf16(:,38), n3(:,96), t3x16(:,:,29))
  call prop_Q_A(ntry, wf8(:,41), Q(:,97), ZERO, 0_intkind1, wf8(:,56), n2(26))
  call vert_UV_W(ntry, ex5, Q(:,16), wf8(:,18), Q(:,67), wf16(:,39), n3(:,97), t3x16(:,:,30))
  call vert_UV_W(ntry, ex6, Q(:,32), wf8(:,18), Q(:,67), wf16(:,40), n3(:,98), t3x16(:,:,31))
  call prop_A_Q(ntry, wf8(:,35), Q(:,82), ZERO, 0_intkind1, wf8(:,57), n2(27))
  call vert_QA_V(ntry, ex1, wf8(:,57), wf16(:,41), n3(:,99), t3x16(:,:,32))
  call prop_A_Q(ntry, wf8(:,42), Q(:,98), ZERO, 0_intkind1, wf8(:,58), n2(28))
  call vert_QA_V(ntry, wf8(:,37), ex2, wf16(:,42), n3(:,100), t3x16(:,:,33))
  call prop_A_Q(ntry, wf8(:,38), Q(:,82), ZERO, 0_intkind1, wf8(:,59), n2(29))
  call vert_QA_V(ntry, ex1, wf8(:,59), wf16(:,43), n3(:,101), t3x16(:,:,34))
  call prop_A_Q(ntry, wf8(:,45), Q(:,98), ZERO, 0_intkind1, wf8(:,60), n2(30))
  call vert_QA_V(ntry, ex1, wf8(:,60), wf16(:,44), n3(:,102), t3x16(:,:,35))
  call vert_QA_V(ntry, wf8(:,44), ex2, wf16(:,45), n3(:,103), t3x16(:,:,36))


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

    M2munu = M2munu / average_factor_heftpphhjj_ddxhhggg_1
  end if

  ! Colour and helicity average and symmetry factor of outgoing particles
  M2add = M2 / average_factor_heftpphhjj_ddxhhggg_1

  do k = 0, 30-1
    M2(k) = M2add(extcomb_perm_heftpphhjj_ddxhhggg_1(k))
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

    call cont_VV(nsync, wf4(:,1), wf8(:,1), A(:,1), n3(:,104), t3x32(:,:,1), nhel, den(1))
    call cont_VV(nsync, wf4(:,1), wf8(:,2), A(:,2), n3(:,105), t3x32(:,:,2), nhel, den(1))
    call cont_VV(nsync, wf4(:,1), wf8(:,3), A(:,3), n3(:,106), t3x32(:,:,3), nhel, den(1))
    call cont_SS(nsync, wf1(:,1), wf32(:,1), A(:,4), n3(:,107), t3x32(:,:,4), nhel, den(3))
    call cont_SS(nsync, wf1(:,1), wf32(:,2), A(:,5), n3(:,108), t3x32(:,:,5), nhel, den(3))
    call cont_SS(nsync, wf1(:,1), wf32(:,3), A(:,6), n3(:,109), t3x32(:,:,6), nhel, den(3))
    call cont_VV(nsync, wf2(:,1), wf16(:,1), A(:,7), n3(:,110), t3x32(:,:,7), nhel, den(5))
    call cont_VV(nsync, wf2(:,1), wf16(:,2), A(:,8), n3(:,111), t3x32(:,:,8), nhel, den(5))
    call cont_VV(nsync, wf2(:,1), wf16(:,3), A(:,9), n3(:,112), t3x32(:,:,9), nhel, den(5))
    call cont_VV(nsync, wf2(:,2), wf16(:,4), A(:,10), n3(:,113), t3x32(:,:,10), nhel, den(7))
    call cont_VV(nsync, wf2(:,2), wf16(:,5), A(:,11), n3(:,114), t3x32(:,:,11), nhel, den(7))
    call cont_VV(nsync, wf2(:,2), wf16(:,6), A(:,12), n3(:,115), t3x32(:,:,12), nhel, den(7))
    call cont_VV(nsync, wf2(:,3), wf16(:,7), A(:,13), n3(:,116), t3x32(:,:,13), nhel, den(9))
    call cont_VV(nsync, wf2(:,3), wf16(:,8), A(:,14), n3(:,117), t3x32(:,:,14), nhel, den(9))
    call cont_VV(nsync, wf2(:,3), wf16(:,9), A(:,15), n3(:,118), t3x32(:,:,15), nhel, den(9))
    call cont_VV(nsync, wf8(:,4), wf4(:,2), A(:,16), n3(:,119), t3x32(:,:,16), nhel, den(11))
    call cont_VV(nsync, wf4(:,2), wf8(:,5), A(:,17), n3(:,120), t3x32(:,:,17), nhel, den(11))
    call cont_VV(nsync, wf4(:,2), wf8(:,6), A(:,18), n3(:,121), t3x32(:,:,18), nhel, den(11))
    call cont_VV(nsync, wf4(:,3), wf8(:,7), A(:,19), n3(:,122), t3x32(:,:,19), nhel, den(13))
    call cont_VV(nsync, wf4(:,4), wf8(:,8), A(:,20), n3(:,123), t3x32(:,:,20), nhel, den(15))
    call cont_VV(nsync, wf4(:,5), wf8(:,9), A(:,21), n3(:,124), t3x32(:,:,21), nhel, den(17))
    call cont_VV(nsync, wf4(:,6), wf8(:,10), A(:,22), n3(:,125), t3x32(:,:,22), nhel, den(19))
    call cont_VV(nsync, wf4(:,7), wf8(:,11), A(:,23), n3(:,126), t3x32(:,:,23), nhel, den(21))
    call cont_VV(nsync, wf4(:,8), wf8(:,12), A(:,24), n3(:,127), t3x32(:,:,24), nhel, den(23))
    call cont_VV(nsync, wf4(:,5), wf8(:,13), A(:,25), n3(:,128), t3x32(:,:,25), nhel, den(25))
    call cont_VV(nsync, wf4(:,5), wf8(:,14), A(:,26), n3(:,129), t3x32(:,:,26), nhel, den(27))
    call cont_VV(nsync, wf4(:,7), wf8(:,15), A(:,27), n3(:,130), t3x32(:,:,27), nhel, den(29))
    call cont_VV(nsync, wf4(:,7), wf8(:,16), A(:,28), n3(:,131), t3x32(:,:,28), nhel, den(31))
    call cont_VV(nsync, wf4(:,8), wf8(:,17), A(:,29), n3(:,132), t3x32(:,:,29), nhel, den(33))
    call cont_VV(nsync, wf4(:,8), wf8(:,18), A(:,30), n3(:,133), t3x32(:,:,30), nhel, den(35))
    call cont_VV(nsync, wf4(:,3), wf8(:,19), A(:,31), n3(:,134), t3x32(:,:,31), nhel, den(36))
    call cont_VV(nsync, wf4(:,4), wf8(:,20), A(:,32), n3(:,135), t3x32(:,:,32), nhel, den(37))
    call cont_VV(nsync, wf8(:,9), wf4(:,21), A(:,33), n3(:,136), t3x32(:,:,33), nhel, den(40))
    call cont_VV(nsync, wf16(:,1), wf2(:,4), A(:,34), n3(:,137), t3x32(:,:,34), nhel, den(42))
    call cont_VV(nsync, wf16(:,2), wf2(:,4), A(:,35), n3(:,138), t3x32(:,:,35), nhel, den(42))
    call cont_VV(nsync, wf16(:,3), wf2(:,4), A(:,36), n3(:,139), t3x32(:,:,36), nhel, den(42))
    call cont_VV(nsync, wf4(:,6), wf8(:,21), A(:,37), n3(:,140), t3x32(:,:,37), nhel, den(43))
    call cont_VV(nsync, wf8(:,11), wf4(:,22), A(:,38), n3(:,141), t3x32(:,:,38), nhel, den(46))
    call cont_VV(nsync, wf16(:,4), wf2(:,5), A(:,39), n3(:,142), t3x32(:,:,39), nhel, den(48))
    call cont_VV(nsync, wf16(:,5), wf2(:,5), A(:,40), n3(:,143), t3x32(:,:,40), nhel, den(48))
    call cont_VV(nsync, wf16(:,6), wf2(:,5), A(:,41), n3(:,144), t3x32(:,:,41), nhel, den(48))
    call cont_VV(nsync, wf8(:,12), wf4(:,23), A(:,42), n3(:,145), t3x32(:,:,42), nhel, den(51))
    call cont_VV(nsync, wf16(:,7), wf2(:,6), A(:,43), n3(:,146), t3x32(:,:,43), nhel, den(53))
    call cont_VV(nsync, wf16(:,8), wf2(:,6), A(:,44), n3(:,147), t3x32(:,:,44), nhel, den(53))
    call cont_VV(nsync, wf16(:,9), wf2(:,6), A(:,45), n3(:,148), t3x32(:,:,45), nhel, den(53))
    call cont_VV(nsync, wf8(:,4), wf4(:,24), A(:,46), n3(:,149), t3x32(:,:,46), nhel, den(54))
    call cont_VV(nsync, wf8(:,5), wf4(:,24), A(:,47), n3(:,150), t3x32(:,:,47), nhel, den(54))
    call cont_VV(nsync, wf8(:,6), wf4(:,24), A(:,48), n3(:,151), t3x32(:,:,48), nhel, den(54))
    call cont_VV(nsync, wf8(:,12), wf4(:,25), A(:,49), n3(:,152), t3x32(:,:,49), nhel, den(55))
    call cont_VV(nsync, wf4(:,2), wf8(:,22), A(:,50), n3(:,153), t3x32(:,:,50), nhel, den(57))
    call cont_VV(nsync, wf2(:,3), wf16(:,10), A(:,51), n3(:,154), t3x32(:,:,51), nhel, den(58))
    call cont_VV(nsync, wf8(:,11), wf4(:,26), A(:,52), n3(:,155), t3x32(:,:,52), nhel, den(59))
    call cont_VV(nsync, wf4(:,2), wf8(:,23), A(:,53), n3(:,156), t3x32(:,:,53), nhel, den(61))
    call cont_VV(nsync, wf8(:,9), wf4(:,27), A(:,54), n3(:,157), t3x32(:,:,54), nhel, den(62))
    call cont_VV(nsync, wf4(:,2), wf8(:,24), A(:,55), n3(:,158), t3x32(:,:,55), nhel, den(64))
    call cont_VV(nsync, wf8(:,9), wf4(:,28), A(:,56), n3(:,159), t3x32(:,:,56), nhel, den(65))
    call cont_VV(nsync, wf8(:,11), wf4(:,29), A(:,57), n3(:,160), t3x32(:,:,57), nhel, den(67))
    call cont_VV(nsync, wf2(:,2), wf16(:,11), A(:,58), n3(:,161), t3x32(:,:,58), nhel, den(68))
    call cont_VV(nsync, wf8(:,9), wf4(:,30), A(:,59), n3(:,162), t3x32(:,:,59), nhel, den(69))
    call cont_VV(nsync, wf8(:,12), wf4(:,31), A(:,60), n3(:,163), t3x32(:,:,60), nhel, den(71))
    call cont_VV(nsync, wf2(:,1), wf16(:,12), A(:,61), n3(:,164), t3x32(:,:,61), nhel, den(72))
    call cont_VV(nsync, wf8(:,11), wf4(:,32), A(:,62), n3(:,165), t3x32(:,:,62), nhel, den(73))
    call cont_VV(nsync, wf8(:,12), wf4(:,33), A(:,63), n3(:,166), t3x32(:,:,63), nhel, den(75))
    call cont_VV(nsync, wf8(:,13), wf4(:,21), A(:,64), n3(:,167), t3x32(:,:,64), nhel, den(77))
    call cont_VV(nsync, wf8(:,14), wf4(:,21), A(:,65), n3(:,168), t3x32(:,:,65), nhel, den(79))
    call cont_VV(nsync, wf8(:,15), wf4(:,22), A(:,66), n3(:,169), t3x32(:,:,66), nhel, den(81))
    call cont_VV(nsync, wf8(:,16), wf4(:,22), A(:,67), n3(:,170), t3x32(:,:,67), nhel, den(83))
    call cont_VV(nsync, wf8(:,17), wf4(:,23), A(:,68), n3(:,171), t3x32(:,:,68), nhel, den(85))
    call cont_VV(nsync, wf8(:,18), wf4(:,23), A(:,69), n3(:,172), t3x32(:,:,69), nhel, den(87))
    call cont_VV(nsync, wf2(:,3), wf16(:,13), A(:,70), n3(:,173), t3x32(:,:,70), nhel, den(89))
    call cont_VV(nsync, wf8(:,13), wf4(:,27), A(:,71), n3(:,174), t3x32(:,:,71), nhel, den(90))
    call cont_VV(nsync, wf8(:,13), wf4(:,28), A(:,72), n3(:,175), t3x32(:,:,72), nhel, den(91))
    call cont_QA(nsync, wf8(:,25), wf4(:,35), A(:,73), n3(:,176), t3x32(:,:,73), nhel, den(94))
    call cont_VV(nsync, wf2(:,2), wf16(:,14), A(:,74), n3(:,177), t3x32(:,:,74), nhel, den(96))
    call cont_VV(nsync, wf8(:,13), wf4(:,30), A(:,75), n3(:,178), t3x32(:,:,75), nhel, den(97))
    call cont_QA(nsync, wf8(:,26), wf4(:,37), A(:,76), n3(:,179), t3x32(:,:,76), nhel, den(100))
    call cont_VV(nsync, wf2(:,3), wf16(:,15), A(:,77), n3(:,180), t3x32(:,:,77), nhel, den(102))
    call cont_VV(nsync, wf8(:,14), wf4(:,27), A(:,78), n3(:,181), t3x32(:,:,78), nhel, den(103))
    call cont_VV(nsync, wf8(:,14), wf4(:,28), A(:,79), n3(:,182), t3x32(:,:,79), nhel, den(104))
    call cont_QA(nsync, wf8(:,27), wf4(:,39), A(:,80), n3(:,183), t3x32(:,:,80), nhel, den(107))
    call cont_VV(nsync, wf2(:,2), wf16(:,16), A(:,81), n3(:,184), t3x32(:,:,81), nhel, den(109))
    call cont_VV(nsync, wf8(:,14), wf4(:,30), A(:,82), n3(:,185), t3x32(:,:,82), nhel, den(110))
    call cont_QA(nsync, wf8(:,28), wf4(:,41), A(:,83), n3(:,186), t3x32(:,:,83), nhel, den(113))
    call cont_VV(nsync, wf8(:,15), wf4(:,26), A(:,84), n3(:,187), t3x32(:,:,84), nhel, den(114))
    call cont_VV(nsync, wf8(:,15), wf4(:,29), A(:,85), n3(:,188), t3x32(:,:,85), nhel, den(115))
    call cont_QA(nsync, wf4(:,35), wf8(:,29), A(:,86), n3(:,189), t3x32(:,:,86), nhel, den(116))
    call cont_VV(nsync, wf8(:,16), wf4(:,26), A(:,87), n3(:,190), t3x32(:,:,87), nhel, den(117))
    call cont_VV(nsync, wf8(:,16), wf4(:,29), A(:,88), n3(:,191), t3x32(:,:,88), nhel, den(118))
    call cont_QA(nsync, wf4(:,39), wf8(:,30), A(:,89), n3(:,192), t3x32(:,:,89), nhel, den(119))
    call cont_VV(nsync, wf8(:,17), wf4(:,25), A(:,90), n3(:,193), t3x32(:,:,90), nhel, den(120))
    call cont_VV(nsync, wf8(:,18), wf4(:,25), A(:,91), n3(:,194), t3x32(:,:,91), nhel, den(121))
    call cont_QA(nsync, wf4(:,34), wf8(:,32), A(:,92), n3(:,195), t3x32(:,:,92), nhel, den(124))
    call cont_QA(nsync, wf4(:,39), wf8(:,33), A(:,93), n3(:,196), t3x32(:,:,93), nhel, den(125))
    call cont_VV(nsync, wf8(:,17), wf4(:,31), A(:,94), n3(:,197), t3x32(:,:,94), nhel, den(126))
    call cont_QA(nsync, wf4(:,37), wf8(:,34), A(:,95), n3(:,198), t3x32(:,:,95), nhel, den(127))
    call cont_VV(nsync, wf8(:,18), wf4(:,31), A(:,96), n3(:,199), t3x32(:,:,96), nhel, den(128))
    call cont_QA(nsync, wf4(:,41), wf8(:,35), A(:,97), n3(:,200), t3x32(:,:,97), nhel, den(129))
    call cont_QA(nsync, wf4(:,36), wf8(:,37), A(:,98), n3(:,201), t3x32(:,:,98), nhel, den(132))
    call cont_QA(nsync, wf4(:,41), wf8(:,38), A(:,99), n3(:,202), t3x32(:,:,99), nhel, den(133))
    call cont_VV(nsync, wf2(:,1), wf16(:,17), A(:,100), n3(:,203), t3x32(:,:,100), nhel, den(135))
    call cont_VV(nsync, wf8(:,15), wf4(:,32), A(:,101), n3(:,204), t3x32(:,:,101), nhel, den(136))
    call cont_QA(nsync, wf8(:,39), wf4(:,43), A(:,102), n3(:,205), t3x32(:,:,102), nhel, den(139))
    call cont_VV(nsync, wf2(:,1), wf16(:,18), A(:,103), n3(:,206), t3x32(:,:,103), nhel, den(141))
    call cont_VV(nsync, wf8(:,16), wf4(:,32), A(:,104), n3(:,207), t3x32(:,:,104), nhel, den(142))
    call cont_QA(nsync, wf8(:,40), wf4(:,45), A(:,105), n3(:,208), t3x32(:,:,105), nhel, den(145))
    call cont_VV(nsync, wf8(:,17), wf4(:,33), A(:,106), n3(:,209), t3x32(:,:,106), nhel, den(146))
    call cont_QA(nsync, wf4(:,43), wf8(:,41), A(:,107), n3(:,210), t3x32(:,:,107), nhel, den(147))
    call cont_VV(nsync, wf8(:,18), wf4(:,33), A(:,108), n3(:,211), t3x32(:,:,108), nhel, den(148))
    call cont_QA(nsync, wf4(:,45), wf8(:,42), A(:,109), n3(:,212), t3x32(:,:,109), nhel, den(149))
    call cont_QA(nsync, wf4(:,42), wf8(:,44), A(:,110), n3(:,213), t3x32(:,:,110), nhel, den(152))
    call cont_QA(nsync, wf4(:,45), wf8(:,45), A(:,111), n3(:,214), t3x32(:,:,111), nhel, den(153))
    call cont_VV(nsync, wf8(:,12), wf4(:,46), A(:,112), n3(:,215), t3x32(:,:,112), nhel, den(155))
    call cont_VV(nsync, wf2(:,6), wf16(:,10), A(:,113), n3(:,216), t3x32(:,:,113), nhel, den(156))
    call cont_VV(nsync, wf4(:,24), wf8(:,22), A(:,114), n3(:,217), t3x32(:,:,114), nhel, den(157))
    call cont_VV(nsync, wf8(:,11), wf4(:,47), A(:,115), n3(:,218), t3x32(:,:,115), nhel, den(159))
    call cont_VV(nsync, wf2(:,5), wf16(:,11), A(:,116), n3(:,219), t3x32(:,:,116), nhel, den(160))
    call cont_VV(nsync, wf4(:,24), wf8(:,23), A(:,117), n3(:,220), t3x32(:,:,117), nhel, den(161))
    call cont_VV(nsync, wf8(:,9), wf4(:,48), A(:,118), n3(:,221), t3x32(:,:,118), nhel, den(163))
    call cont_VV(nsync, wf2(:,4), wf16(:,12), A(:,119), n3(:,222), t3x32(:,:,119), nhel, den(164))
    call cont_VV(nsync, wf4(:,24), wf8(:,24), A(:,120), n3(:,223), t3x32(:,:,120), nhel, den(165))
    call cont_VV(nsync, wf2(:,5), wf16(:,19), A(:,121), n3(:,224), t3x32(:,:,121), nhel, den(166))
    call cont_VV(nsync, wf2(:,6), wf16(:,20), A(:,122), n3(:,225), t3x32(:,:,122), nhel, den(167))
    call cont_VV(nsync, wf8(:,11), wf4(:,49), A(:,123), n3(:,226), t3x32(:,:,123), nhel, den(168))
    call cont_VV(nsync, wf2(:,6), wf16(:,21), A(:,124), n3(:,227), t3x32(:,:,124), nhel, den(169))
    call cont_VV(nsync, wf8(:,12), wf4(:,50), A(:,125), n3(:,228), t3x32(:,:,125), nhel, den(170))
    call cont_VV(nsync, wf8(:,12), wf4(:,51), A(:,126), n3(:,229), t3x32(:,:,126), nhel, den(171))
    call cont_VV(nsync, wf2(:,6), wf16(:,13), A(:,127), n3(:,230), t3x32(:,:,127), nhel, den(172))
    call cont_VV(nsync, wf2(:,5), wf16(:,14), A(:,128), n3(:,231), t3x32(:,:,128), nhel, den(173))
    call cont_VV(nsync, wf8(:,13), wf4(:,48), A(:,129), n3(:,232), t3x32(:,:,129), nhel, den(174))
    call cont_VV(nsync, wf2(:,6), wf16(:,22), A(:,130), n3(:,233), t3x32(:,:,130), nhel, den(175))
    call cont_VV(nsync, wf2(:,5), wf16(:,23), A(:,131), n3(:,234), t3x32(:,:,131), nhel, den(176))
    call cont_VV(nsync, wf2(:,6), wf16(:,24), A(:,132), n3(:,235), t3x32(:,:,132), nhel, den(178))
    call cont_QA(nsync, wf8(:,47), wf4(:,52), A(:,133), n3(:,236), t3x32(:,:,133), nhel, den(180))
    call cont_VV(nsync, wf2(:,6), wf16(:,15), A(:,134), n3(:,237), t3x32(:,:,134), nhel, den(181))
    call cont_VV(nsync, wf2(:,5), wf16(:,16), A(:,135), n3(:,238), t3x32(:,:,135), nhel, den(182))
    call cont_VV(nsync, wf8(:,14), wf4(:,48), A(:,136), n3(:,239), t3x32(:,:,136), nhel, den(183))
    call cont_VV(nsync, wf2(:,6), wf16(:,25), A(:,137), n3(:,240), t3x32(:,:,137), nhel, den(184))
    call cont_VV(nsync, wf2(:,5), wf16(:,26), A(:,138), n3(:,241), t3x32(:,:,138), nhel, den(185))
    call cont_VV(nsync, wf2(:,6), wf16(:,27), A(:,139), n3(:,242), t3x32(:,:,139), nhel, den(188))
    call cont_QA(nsync, wf8(:,49), wf4(:,53), A(:,140), n3(:,243), t3x32(:,:,140), nhel, den(191))
    call cont_VV(nsync, wf2(:,4), wf16(:,17), A(:,141), n3(:,244), t3x32(:,:,141), nhel, den(192))
    call cont_VV(nsync, wf8(:,15), wf4(:,47), A(:,142), n3(:,245), t3x32(:,:,142), nhel, den(193))
    call cont_VV(nsync, wf2(:,6), wf16(:,28), A(:,143), n3(:,246), t3x32(:,:,143), nhel, den(194))
    call cont_VV(nsync, wf2(:,4), wf16(:,29), A(:,144), n3(:,247), t3x32(:,:,144), nhel, den(195))
    call cont_VV(nsync, wf2(:,6), wf16(:,30), A(:,145), n3(:,248), t3x32(:,:,145), nhel, den(197))
    call cont_QA(nsync, wf8(:,51), wf4(:,54), A(:,146), n3(:,249), t3x32(:,:,146), nhel, den(199))
    call cont_VV(nsync, wf2(:,4), wf16(:,18), A(:,147), n3(:,250), t3x32(:,:,147), nhel, den(200))
    call cont_VV(nsync, wf8(:,16), wf4(:,47), A(:,148), n3(:,251), t3x32(:,:,148), nhel, den(201))
    call cont_VV(nsync, wf2(:,6), wf16(:,31), A(:,149), n3(:,252), t3x32(:,:,149), nhel, den(202))
    call cont_VV(nsync, wf2(:,4), wf16(:,32), A(:,150), n3(:,253), t3x32(:,:,150), nhel, den(203))
    call cont_VV(nsync, wf2(:,6), wf16(:,33), A(:,151), n3(:,254), t3x32(:,:,151), nhel, den(205))
    call cont_QA(nsync, wf8(:,53), wf4(:,55), A(:,152), n3(:,255), t3x32(:,:,152), nhel, den(208))
    call cont_VV(nsync, wf8(:,17), wf4(:,46), A(:,153), n3(:,256), t3x32(:,:,153), nhel, den(209))
    call cont_VV(nsync, wf8(:,18), wf4(:,46), A(:,154), n3(:,257), t3x32(:,:,154), nhel, den(210))
    call cont_VV(nsync, wf2(:,6), wf16(:,34), A(:,155), n3(:,258), t3x32(:,:,155), nhel, den(211))
    call cont_VV(nsync, wf2(:,6), wf16(:,35), A(:,156), n3(:,259), t3x32(:,:,156), nhel, den(213))
    call cont_VV(nsync, wf2(:,5), wf16(:,36), A(:,157), n3(:,260), t3x32(:,:,157), nhel, den(214))
    call cont_VV(nsync, wf2(:,4), wf16(:,37), A(:,158), n3(:,261), t3x32(:,:,158), nhel, den(215))
    call cont_VV(nsync, wf2(:,5), wf16(:,38), A(:,159), n3(:,262), t3x32(:,:,159), nhel, den(217))
    call cont_QA(nsync, wf4(:,54), wf8(:,56), A(:,160), n3(:,263), t3x32(:,:,160), nhel, den(219))
    call cont_VV(nsync, wf2(:,5), wf16(:,39), A(:,161), n3(:,264), t3x32(:,:,161), nhel, den(220))
    call cont_VV(nsync, wf2(:,4), wf16(:,40), A(:,162), n3(:,265), t3x32(:,:,162), nhel, den(221))
    call cont_VV(nsync, wf2(:,5), wf16(:,41), A(:,163), n3(:,266), t3x32(:,:,163), nhel, den(223))
    call cont_QA(nsync, wf4(:,55), wf8(:,58), A(:,164), n3(:,267), t3x32(:,:,164), nhel, den(225))
    call cont_VV(nsync, wf2(:,5), wf16(:,42), A(:,165), n3(:,268), t3x32(:,:,165), nhel, den(226))
    call cont_VV(nsync, wf2(:,5), wf16(:,43), A(:,166), n3(:,269), t3x32(:,:,166), nhel, den(228))
    call cont_VV(nsync, wf2(:,4), wf16(:,44), A(:,167), n3(:,270), t3x32(:,:,167), nhel, den(230))
    call cont_VV(nsync, wf2(:,4), wf16(:,45), A(:,168), n3(:,271), t3x32(:,:,168), nhel, den(231))

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
  complex(REALKIND), intent(out) :: M1(6) ! M1helarray(6,32)
  integer :: empty(0)

  M1(1) = (A(j,1)%j-A(j,2)%j+A(j,8)%j-A(j,9)%j-A(j,10)%j+A(j,12)%j+A(j,13)%j-A(j,14)%j+A(j,16)%j-A(j,17)%j-A(j,19)%j+A(j,21)%j &
       +A(j,22)%j-A(j,24)%j-A(j,49)%j+A(j,50)%j+A(j,51)%j+A(j,54)%j+A(j,55)%j+A(j,56)%j+A(j,59)%j-A(j,60)%j-A(j,61)%j-A(j,63)%j &
       +A(j,80)%j+A(j,81)%j+A(j,107)%j)*f(1)+CI*(A(j,26)%j+A(j,29)%j+A(j,78)%j+A(j,79)%j+A(j,82)%j+A(j,90)%j+A(j,93)%j+A(j,94)%j &
       +A(j,106)%j+A(j,110)%j)*f(2)+(-A(j,4)%j+A(j,6)%j+A(j,31)%j-A(j,33)%j-A(j,35)%j+A(j,36)%j-A(j,37)%j+A(j,39)%j-A(j,41)%j &
       +A(j,42)%j-A(j,43)%j+A(j,44)%j-A(j,46)%j+A(j,47)%j+A(j,112)%j-A(j,113)%j-A(j,114)%j-A(j,118)%j+A(j,119)%j-A(j,120)%j &
       +A(j,121)%j-A(j,122)%j+A(j,125)%j+A(j,126)%j-A(j,135)%j-A(j,139)%j-A(j,160)%j)*f(3)+CI*(-A(j,65)%j-A(j,68)%j-A(j,136)%j &
       -A(j,137)%j+A(j,138)%j-A(j,153)%j-A(j,156)%j+A(j,157)%j-A(j,158)%j-A(j,168)%j)*f(4)
  M1(2) = (A(j,2)%j-A(j,3)%j+A(j,7)%j-A(j,8)%j+A(j,10)%j-A(j,11)%j-A(j,13)%j+A(j,15)%j+A(j,17)%j-A(j,18)%j-A(j,20)%j-A(j,21)%j &
       -A(j,22)%j-A(j,23)%j-A(j,52)%j-A(j,53)%j-A(j,54)%j-A(j,55)%j-A(j,56)%j-A(j,57)%j+A(j,58)%j-A(j,59)%j+A(j,61)%j-A(j,62)%j &
       +A(j,77)%j+A(j,83)%j+A(j,102)%j)*f(1)+CI*(-A(j,26)%j+A(j,27)%j-A(j,78)%j-A(j,79)%j-A(j,82)%j+A(j,84)%j+A(j,85)%j+A(j,99)%j &
       +A(j,101)%j-A(j,110)%j)*f(2)+(A(j,4)%j-A(j,5)%j+A(j,32)%j+A(j,33)%j-A(j,34)%j+A(j,35)%j+A(j,37)%j+A(j,38)%j-A(j,39)%j &
       +A(j,40)%j+A(j,43)%j-A(j,45)%j-A(j,47)%j+A(j,48)%j+A(j,115)%j-A(j,116)%j+A(j,117)%j+A(j,118)%j-A(j,119)%j+A(j,120)%j &
       -A(j,121)%j+A(j,122)%j+A(j,123)%j-A(j,124)%j-A(j,134)%j-A(j,140)%j-A(j,146)%j)*f(3)+CI*(A(j,65)%j-A(j,66)%j+A(j,136)%j &
       +A(j,137)%j-A(j,138)%j-A(j,142)%j+A(j,143)%j+A(j,144)%j-A(j,166)%j+A(j,168)%j)*f(4)
  M1(3) = (-A(j,1)%j+A(j,3)%j-A(j,7)%j+A(j,9)%j+A(j,11)%j-A(j,12)%j+A(j,14)%j-A(j,15)%j-A(j,16)%j+A(j,18)%j+A(j,19)%j+A(j,20)%j &
       +A(j,23)%j+A(j,24)%j+A(j,49)%j-A(j,50)%j-A(j,51)%j+A(j,52)%j+A(j,53)%j+A(j,57)%j-A(j,58)%j+A(j,60)%j+A(j,62)%j+A(j,63)%j &
       +A(j,89)%j+A(j,95)%j+A(j,103)%j)*f(1)+CI*(A(j,28)%j-A(j,29)%j+A(j,87)%j+A(j,88)%j-A(j,90)%j-A(j,93)%j-A(j,94)%j+A(j,98)%j &
       +A(j,104)%j-A(j,106)%j)*f(2)+(A(j,5)%j-A(j,6)%j-A(j,31)%j-A(j,32)%j+A(j,34)%j-A(j,36)%j-A(j,38)%j-A(j,40)%j+A(j,41)%j &
       -A(j,42)%j-A(j,44)%j+A(j,45)%j+A(j,46)%j-A(j,48)%j-A(j,112)%j+A(j,113)%j+A(j,114)%j-A(j,115)%j+A(j,116)%j-A(j,117)%j &
       -A(j,123)%j+A(j,124)%j-A(j,125)%j-A(j,126)%j-A(j,147)%j-A(j,151)%j-A(j,159)%j)*f(3)+CI*(-A(j,67)%j+A(j,68)%j-A(j,148)%j &
       +A(j,149)%j+A(j,150)%j+A(j,153)%j+A(j,156)%j-A(j,157)%j+A(j,158)%j-A(j,165)%j)*f(4)
  M1(4) = (A(j,2)%j-A(j,3)%j+A(j,7)%j-A(j,8)%j+A(j,10)%j-A(j,11)%j-A(j,13)%j+A(j,15)%j+A(j,17)%j-A(j,18)%j-A(j,20)%j-A(j,21)%j &
       -A(j,22)%j-A(j,23)%j-A(j,52)%j-A(j,53)%j-A(j,54)%j-A(j,55)%j-A(j,56)%j-A(j,57)%j+A(j,58)%j-A(j,59)%j+A(j,61)%j-A(j,62)%j &
       +A(j,70)%j+A(j,76)%j+A(j,105)%j)*f(1)+CI*(A(j,25)%j-A(j,28)%j+A(j,71)%j+A(j,72)%j+A(j,75)%j-A(j,87)%j-A(j,88)%j-A(j,98)%j &
       -A(j,104)%j+A(j,111)%j)*f(2)+(A(j,4)%j-A(j,5)%j+A(j,32)%j+A(j,33)%j-A(j,34)%j+A(j,35)%j+A(j,37)%j+A(j,38)%j-A(j,39)%j &
       +A(j,40)%j+A(j,43)%j-A(j,45)%j-A(j,47)%j+A(j,48)%j+A(j,115)%j-A(j,116)%j+A(j,117)%j+A(j,118)%j-A(j,119)%j+A(j,120)%j &
       -A(j,121)%j+A(j,122)%j+A(j,123)%j-A(j,124)%j-A(j,127)%j-A(j,133)%j-A(j,152)%j)*f(3)+CI*(-A(j,64)%j+A(j,67)%j-A(j,129)%j &
       -A(j,130)%j+A(j,131)%j+A(j,148)%j-A(j,149)%j-A(j,150)%j+A(j,165)%j-A(j,167)%j)*f(4)
  M1(5) = (-A(j,1)%j+A(j,3)%j-A(j,7)%j+A(j,9)%j+A(j,11)%j-A(j,12)%j+A(j,14)%j-A(j,15)%j-A(j,16)%j+A(j,18)%j+A(j,19)%j+A(j,20)%j &
       +A(j,23)%j+A(j,24)%j+A(j,49)%j-A(j,50)%j-A(j,51)%j+A(j,52)%j+A(j,53)%j+A(j,57)%j-A(j,58)%j+A(j,60)%j+A(j,62)%j+A(j,63)%j &
       +A(j,86)%j+A(j,97)%j+A(j,100)%j)*f(1)+CI*(-A(j,27)%j+A(j,30)%j-A(j,84)%j-A(j,85)%j+A(j,91)%j+A(j,92)%j+A(j,96)%j-A(j,99)%j &
       -A(j,101)%j+A(j,108)%j)*f(2)+(A(j,5)%j-A(j,6)%j-A(j,31)%j-A(j,32)%j+A(j,34)%j-A(j,36)%j-A(j,38)%j-A(j,40)%j+A(j,41)%j &
       -A(j,42)%j-A(j,44)%j+A(j,45)%j+A(j,46)%j-A(j,48)%j-A(j,112)%j+A(j,113)%j+A(j,114)%j-A(j,115)%j+A(j,116)%j-A(j,117)%j &
       -A(j,123)%j+A(j,124)%j-A(j,125)%j-A(j,126)%j-A(j,141)%j-A(j,145)%j-A(j,163)%j)*f(3)+CI*(A(j,66)%j-A(j,69)%j+A(j,142)%j &
       -A(j,143)%j-A(j,144)%j-A(j,154)%j-A(j,155)%j+A(j,161)%j-A(j,162)%j+A(j,166)%j)*f(4)
  M1(6) = (A(j,1)%j-A(j,2)%j+A(j,8)%j-A(j,9)%j-A(j,10)%j+A(j,12)%j+A(j,13)%j-A(j,14)%j+A(j,16)%j-A(j,17)%j-A(j,19)%j+A(j,21)%j &
       +A(j,22)%j-A(j,24)%j-A(j,49)%j+A(j,50)%j+A(j,51)%j+A(j,54)%j+A(j,55)%j+A(j,56)%j+A(j,59)%j-A(j,60)%j-A(j,61)%j-A(j,63)%j &
       +A(j,73)%j+A(j,74)%j+A(j,109)%j)*f(1)+CI*(-A(j,25)%j-A(j,30)%j-A(j,71)%j-A(j,72)%j-A(j,75)%j-A(j,91)%j-A(j,92)%j-A(j,96)%j &
       -A(j,108)%j-A(j,111)%j)*f(2)+(-A(j,4)%j+A(j,6)%j+A(j,31)%j-A(j,33)%j-A(j,35)%j+A(j,36)%j-A(j,37)%j+A(j,39)%j-A(j,41)%j &
       +A(j,42)%j-A(j,43)%j+A(j,44)%j-A(j,46)%j+A(j,47)%j+A(j,112)%j-A(j,113)%j-A(j,114)%j-A(j,118)%j+A(j,119)%j-A(j,120)%j &
       +A(j,121)%j-A(j,122)%j+A(j,125)%j+A(j,126)%j-A(j,128)%j-A(j,132)%j-A(j,164)%j)*f(3)+CI*(A(j,64)%j+A(j,69)%j+A(j,129)%j &
       +A(j,130)%j-A(j,131)%j+A(j,154)%j+A(j,155)%j-A(j,161)%j+A(j,162)%j+A(j,167)%j)*f(4)

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
  use ol_colourmatrix_heftpphhjj_ddxhhggg_1_/**/REALKIND, only: K1
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
  use ol_colourmatrix_heftpphhjj_ddxhhggg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(6), M2(6)
  real(REALKIND),    intent(out) :: M2colint(0:30-1)
  integer,           intent(in)  :: extcombs(:)
  integer :: extcomb, colmatpos, i, j, eco

  M2colint = 0

  do eco = 1, size(extcombs)
    extcomb = extcombs(eco)
    colmatpos = 6*extcomb
    do i = 1, 6
      do j = 1, 6
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
  use ol_colourmatrix_heftpphhjj_ddxhhggg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(6)
  complex(REALKIND), intent(in)  :: M2(6)
  complex(REALKIND),    intent(out) :: M2colint
  integer :: i, j

  M2colint = 0

  do i = 1, 6
    do j = 1, 6
      M2colint = M2colint + M1(i)*K1(i,j)*conjg(M2(j))
    end do
  end do

end subroutine colintmunu

end subroutine amp2


#ifdef PRECISION_dp
subroutine colourvector(M1out, nhelout) &
  & bind(c,name="ol_tree_colvect_heftpphhjj_ddxhhggg_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(6,32)
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
    & bind(c,name="ol_f_amp2tree_heftpphhjj_ddxhhggg_1")
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
    & bind(c,name="ol_f_amp2ccone_heftpphhjj_ddxhhggg_1")
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
    & bind(c,name="ol_f_amp2ccall_heftpphhjj_ddxhhggg_1")
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
    & bind(c,name="ol_f_amp2hcone_heftpphhjj_ddxhhggg_1")
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
    & bind(c,name="ol_f_amp2hcall_heftpphhjj_ddxhhggg_1")
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
    & bind(c,name="ol_amp2tree_heftpphhjj_ddxhhggg_1")
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
    & bind(c,name="ol_amp2ccone_heftpphhjj_ddxhhggg_1")
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
    & bind(c,name="ol_amp2ccall_heftpphhjj_ddxhhggg_1")
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
    & bind(c,name="ol_amp2hcone_heftpphhjj_ddxhhggg_1")
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
    & bind(c,name="ol_amp2hcall_heftpphhjj_ddxhhggg_1")
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
    & bind(c,name="amp2tree_heftpphhjj_ddxhhggg_1_")
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
    & bind(c,name="amp2ccone_heftpphhjj_ddxhhggg_1_")
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
    & bind(c,name="amp2ccall_heftpphhjj_ddxhhggg_1_")
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
    & bind(c,name="amp2hcone_heftpphhjj_ddxhhggg_1_")
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
    & bind(c,name="amp2hcall_heftpphhjj_ddxhhggg_1_")
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

end module ol_tree_heftpphhjj_ddxhhggg_1_/**/REALKIND
