
module ol_colourmatrix_pphbbj_cccxcxbbxh_1_/**/REALKIND
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

  K1(  1,:) = [  27,   9,   9,   3,   3,   9]
  K1(  2,:) = [   9,  27,   3,   9,   9,   3]
  K1(  3,:) = [   9,   3,  27,   9,   9,   3]
  K1(  4,:) = [   3,   9,   9,  27,   3,   9]
  K1(  5,:) = [   3,   9,   9,   3,  27,   9]
  K1(  6,:) = [   9,   3,   3,   9,   9,  27]
  K1(  7,:) = [  36,  12,  12,   4,   4,  12]
  K1(  8,:) = [  12,  36,   4,  12,  12,   4]
  K1(  9,:) = [  12,   4,  36,  12,  12,   4]
  K1( 10,:) = [   4,  12,  12,  36,   4,  12]
  K1( 11,:) = [   4,  12,  12,   4,  36,  12]
  K1( 12,:) = [  12,   4,   4,  12,  12,  36]
  K1( 13,:) = [   0,   0,  12,   4,   4,   0]
  K1( 14,:) = [   0,   0,   4,  12,   0,   4]
  K1( 15,:) = [  12,   4,   0,   0,   0,   4]
  K1( 16,:) = [   4,  12,   0,   0,   4,   0]
  K1( 17,:) = [   4,   0,   0,   4,   0,  12]
  K1( 18,:) = [   0,   4,   4,   0,  12,   0]
  K1( 19,:) = [  36,  12,  12,   4,   4,  12]
  K1( 20,:) = [  12,  36,   4,  12,  12,   4]
  K1( 21,:) = [  12,   4,  36,  12,  12,   4]
  K1( 22,:) = [   4,  12,  12,  36,   4,  12]
  K1( 23,:) = [   4,  12,  12,   4,  36,  12]
  K1( 24,:) = [  12,   4,   4,  12,  12,  36]
  K1( 25,:) = [   0,   0,   0,  -4,  -4, -12]
  K1( 26,:) = [   0,   0,  -4, -12,   0,  -4]
  K1( 27,:) = [   0,  -4,   0, -12,   0,  -4]
  K1( 28,:) = [  -4, -12, -12, -36,  -4, -12]
  K1( 29,:) = [  -4,   0,   0,  -4,   0, -12]
  K1( 30,:) = [ -12,  -4,  -4, -12, -12, -36]
  K1( 31,:) = [   0, -12,   0,  -4,  -4,   0]
  K1( 32,:) = [ -12, -36,  -4, -12, -12,  -4]
  K1( 33,:) = [   0,  -4,   0,   0, -12,  -4]
  K1( 34,:) = [  -4, -12,   0,   0,  -4,   0]
  K1( 35,:) = [  -4, -12, -12,  -4, -36, -12]
  K1( 36,:) = [   0,  -4,  -4,   0, -12,   0]
  K1( 37,:) = [  36,  12,  12,   4,   4,  12]
  K1( 38,:) = [  12,  36,   4,  12,  12,   4]
  K1( 39,:) = [  12,   4,  36,  12,  12,   4]
  K1( 40,:) = [   4,  12,  12,  36,   4,  12]
  K1( 41,:) = [   4,  12,  12,   4,  36,  12]
  K1( 42,:) = [  12,   4,   4,  12,  12,  36]
  K1( 43,:) = [   0,   0, -12,  -4,  -4,   0]
  K1( 44,:) = [   0,   0,  -4,   0, -12,  -4]
  K1( 45,:) = [ -12,  -4, -36, -12, -12,  -4]
  K1( 46,:) = [  -4,   0, -12,   0,  -4,   0]
  K1( 47,:) = [  -4, -12, -12,  -4, -36, -12]
  K1( 48,:) = [   0,  -4,  -4,   0, -12,   0]
  K1( 49,:) = [ -36, -12, -12,  -4,  -4, -12]
  K1( 50,:) = [ -12,   0,  -4,   0,   0,  -4]
  K1( 51,:) = [ -12,  -4,   0,   0,   0,  -4]
  K1( 52,:) = [  -4,   0,   0,   0,  -4, -12]
  K1( 53,:) = [  -4,   0,   0,  -4,   0, -12]
  K1( 54,:) = [ -12,  -4,  -4, -12, -12, -36]
  K1( 55,:) = [   0,  12,   0,   4,   4,   0]
  K1( 56,:) = [  12,   0,   4,   0,   0,   4]
  K1( 57,:) = [   0,   4,   0,  12,   0,   4]
  K1( 58,:) = [   4,   0,  12,   0,   4,   0]
  K1( 59,:) = [   4,   0,   0,   4,   0,  12]
  K1( 60,:) = [   0,   4,   4,   0,  12,   0]
  K1( 61,:) = [  36,  12,  12,   4,   4,  12]
  K1( 62,:) = [  12,  36,   4,  12,  12,   4]
  K1( 63,:) = [  12,   4,  36,  12,  12,   4]
  K1( 64,:) = [   4,  12,  12,  36,   4,  12]
  K1( 65,:) = [   4,  12,  12,   4,  36,  12]
  K1( 66,:) = [  12,   4,   4,  12,  12,  36]
  K1( 67,:) = [   0,   0,   0,   4,   4,  12]
  K1( 68,:) = [   0,   0,   4,   0,  12,   4]
  K1( 69,:) = [   0,   4,   0,  12,   0,   4]
  K1( 70,:) = [   4,   0,  12,   0,   4,   0]
  K1( 71,:) = [   4,  12,   0,   4,   0,   0]
  K1( 72,:) = [  12,   4,   4,   0,   0,   0]
  K1( 73,:) = [   0,  12,   0,   4,   4,   0]
  K1( 74,:) = [  12,   0,   4,   0,   0,   4]
  K1( 75,:) = [   0,   4,   0,   0,  12,   4]
  K1( 76,:) = [   4,   0,   0,   0,   4,  12]
  K1( 77,:) = [   4,   0,  12,   4,   0,   0]
  K1( 78,:) = [   0,   4,   4,  12,   0,   0]
  K1( 79,:) = [ -36, -12, -12,  -4,  -4, -12]
  K1( 80,:) = [ -12,   0,  -4,   0,   0,  -4]
  K1( 81,:) = [ -12,  -4, -36, -12, -12,  -4]
  K1( 82,:) = [  -4,   0, -12,   0,  -4,   0]
  K1( 83,:) = [  -4,   0, -12,  -4,   0,   0]
  K1( 84,:) = [ -12,  -4,  -4,   0,   0,   0]
  K1( 85,:) = [   0, -12,   0,  -4,  -4,   0]
  K1( 86,:) = [ -12, -36,  -4, -12, -12,  -4]
  K1( 87,:) = [   0,  -4,   0, -12,   0,  -4]
  K1( 88,:) = [  -4, -12, -12, -36,  -4, -12]
  K1( 89,:) = [  -4, -12,   0,  -4,   0,   0]
  K1( 90,:) = [   0,  -4,  -4, -12,   0,   0]
  K1( 91,:) = [  36,  12,  12,   4,   4,  12]
  K1( 92,:) = [  12,  36,   4,  12,  12,   4]
  K1( 93,:) = [  12,   4,  36,  12,  12,   4]
  K1( 94,:) = [   4,  12,  12,  36,   4,  12]
  K1( 95,:) = [   4,  12,  12,   4,  36,  12]
  K1( 96,:) = [  12,   4,   4,  12,  12,  36]
  K1( 97,:) = [ -36, -12, -12,  -4,  -4, -12]
  K1( 98,:) = [ -12, -36,  -4, -12, -12,  -4]
  K1( 99,:) = [ -12,  -4,   0,   0,   0,  -4]
  K1(100,:) = [  -4, -12,   0,   0,  -4,   0]
  K1(101,:) = [  -4, -12,   0,  -4,   0,   0]
  K1(102,:) = [ -12,  -4,  -4,   0,   0,   0]
  K1(103,:) = [   0,   0, -12,  -4,  -4,   0]
  K1(104,:) = [   0,   0,  -4, -12,   0,  -4]
  K1(105,:) = [ -12,  -4, -36, -12, -12,  -4]
  K1(106,:) = [  -4, -12, -12, -36,  -4, -12]
  K1(107,:) = [  -4,   0, -12,  -4,   0,   0]
  K1(108,:) = [   0,  -4,  -4, -12,   0,   0]
  K1(109,:) = [   0,   0,   0,   4,   4,  12]
  K1(110,:) = [   0,   0,   4,  12,   0,   4]
  K1(111,:) = [   0,   4,   0,   0,  12,   4]
  K1(112,:) = [   4,  12,   0,   0,   4,   0]
  K1(113,:) = [   4,   0,  12,   4,   0,   0]
  K1(114,:) = [  12,   4,   4,   0,   0,   0]
  K1(115,:) = [   0,   0,  12,   4,   4,   0]
  K1(116,:) = [   0,   0,   4,   0,  12,   4]
  K1(117,:) = [  12,   4,   0,   0,   0,   4]
  K1(118,:) = [   4,   0,   0,   0,   4,  12]
  K1(119,:) = [   4,  12,   0,   4,   0,   0]
  K1(120,:) = [   0,   4,   4,  12,   0,   0]
  K1(121,:) = [   0,   0,   0,  -4,  -4, -12]
  K1(122,:) = [   0,   0,  -4,   0, -12,  -4]
  K1(123,:) = [   0,  -4,   0,   0, -12,  -4]
  K1(124,:) = [  -4,   0,   0,   0,  -4, -12]
  K1(125,:) = [  -4, -12, -12,  -4, -36, -12]
  K1(126,:) = [ -12,  -4,  -4, -12, -12, -36]
  K1(127,:) = [  36,  12,  12,   4,   4,  12]
  K1(128,:) = [  12,  36,   4,  12,  12,   4]
  K1(129,:) = [  12,   4,  36,  12,  12,   4]
  K1(130,:) = [   4,  12,  12,  36,   4,  12]
  K1(131,:) = [   4,  12,  12,   4,  36,  12]
  K1(132,:) = [  12,   4,   4,  12,  12,  36]
  K1(133,:) = [   0,   0,   0,   0,   0,   0]
  K1(134,:) = [   0,   0,   0,   0,   0,   0]
  K1(135,:) = [   0,   0,   0,   0,   0,   0]
  K1(136,:) = [   0,   0,   0,   0,   0,   0]
  K1(137,:) = [   0,   0,   0,   0,   0,   0]
  K1(138,:) = [   0,   0,   0,   0,   0,   0]
  K1(139,:) = [   0,   0,   0,   0,   0,   0]
  K1(140,:) = [   0,   0,   0,   0,   0,   0]
  K1(141,:) = [   0,   0,   0,   0,   0,   0]
  K1(142,:) = [   0,   0,   0,   0,   0,   0]
  K1(143,:) = [   0,   0,   0,   0,   0,   0]
  K1(144,:) = [   0,   0,   0,   0,   0,   0]
  K1(145,:) = [   0,   0,   0,   0,   0,   0]
  K1(146,:) = [   0,   0,   0,   0,   0,   0]
  K1(147,:) = [   0,   0,   0,   0,   0,   0]
  K1(148,:) = [   0,   0,   0,   0,   0,   0]
  K1(149,:) = [   0,   0,   0,   0,   0,   0]
  K1(150,:) = [   0,   0,   0,   0,   0,   0]
  K1(151,:) = [   0,   0,   0,   0,   0,   0]
  K1(152,:) = [   0,   0,   0,   0,   0,   0]
  K1(153,:) = [   0,   0,   0,   0,   0,   0]
  K1(154,:) = [   0,   0,   0,   0,   0,   0]
  K1(155,:) = [   0,   0,   0,   0,   0,   0]
  K1(156,:) = [   0,   0,   0,   0,   0,   0]
  K1(157,:) = [   0,   0,   0,   0,   0,   0]
  K1(158,:) = [   0,   0,   0,   0,   0,   0]
  K1(159,:) = [   0,   0,   0,   0,   0,   0]
  K1(160,:) = [   0,   0,   0,   0,   0,   0]
  K1(161,:) = [   0,   0,   0,   0,   0,   0]
  K1(162,:) = [   0,   0,   0,   0,   0,   0]
  K1(163,:) = [   0,   0,   0,   0,   0,   0]
  K1(164,:) = [   0,   0,   0,   0,   0,   0]
  K1(165,:) = [   0,   0,   0,   0,   0,   0]
  K1(166,:) = [   0,   0,   0,   0,   0,   0]
  K1(167,:) = [   0,   0,   0,   0,   0,   0]
  K1(168,:) = [   0,   0,   0,   0,   0,   0]
  K1(169,:) = [   0,   0,   0,   0,   0,   0]
  K1(170,:) = [   0,   0,   0,   0,   0,   0]
  K1(171,:) = [   0,   0,   0,   0,   0,   0]
  K1(172,:) = [   0,   0,   0,   0,   0,   0]
  K1(173,:) = [   0,   0,   0,   0,   0,   0]
  K1(174,:) = [   0,   0,   0,   0,   0,   0]
  K1(175,:) = [   0,   0,   0,   0,   0,   0]
  K1(176,:) = [   0,   0,   0,   0,   0,   0]
  K1(177,:) = [   0,   0,   0,   0,   0,   0]
  K1(178,:) = [   0,   0,   0,   0,   0,   0]
  K1(179,:) = [   0,   0,   0,   0,   0,   0]
  K1(180,:) = [   0,   0,   0,   0,   0,   0]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphbbj_cccxcxbbxh_1_/**/REALKIND



module ol_forced_parameters_pphbbj_cccxcxbbxh_1_/**/REALKIND
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
  if (CKMORDER /= 0) write(*,101) 'CKMORDER = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (YU /= 0) write(*,101) 'YU = 0'
  if (YD /= 0) write(*,101) 'YD = 0'
  if (YS /= 0) write(*,101) 'YS = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMC /= 0) write(*,101) 'wMC = 0'
  if (wMC /= 0) write(*,101) 'wMC = 0'
  if (wMC /= 0) write(*,101) 'wMC = 0'
  if (wMC /= 0) write(*,101) 'wMC = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphbbj_cccxcxbbxh_1_/**/REALKIND

module ol_tree_pphbbj_cccxcxbbxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(4)
  complex(REALKIND), save :: den(184)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 64 ! number of helicity configurations
  integer(intkind2), save :: nhel = 64 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(64) ! physical helicity states
  complex(DREALKIND) :: M1helarr(6,64) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = (CI*eQED*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f(2) = (eQED*gQCD**4*YB)/(MW*sw*2._/**/REALKIND)
    f(3) = (CI*eQED*gQCD**4*YC)/(2._/**/REALKIND*MW*sw)
    f(4) = (eQED*gQCD**4*YC)/(MW*sw*2._/**/REALKIND)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,5))
  den(2) = 1 / (Q(5,10))
  den(3) = 1 / (Q(5,80) - MB2)
  den(5) = 1 / (Q(5,37) - MB2)
  den(9) = 1 / (Q(5,42) - MB2)
  den(13) = 1 / (Q(5,112))
  den(16) = 1 / (Q(5,96) - MB2)
  den(18) = 1 / (Q(5,21) - MB2)
  den(22) = 1 / (Q(5,26) - MB2)
  den(29) = 1 / (Q(5,66) - MC2)
  den(30) = 1 / (Q(5,48))
  den(32) = 1 / (Q(5,13) - MC2)
  den(36) = 1 / (Q(5,74))
  den(40) = 1 / (Q(5,56) - MC2)
  den(43) = 1 / (Q(5,72) - MC2)
  den(45) = 1 / (Q(5,7) - MC2)
  den(51) = 1 / (Q(5,50) - MC2)
  den(64) = 1 / (Q(5,9))
  den(65) = 1 / (Q(5,6))
  den(67) = 1 / (Q(5,41) - MB2)
  den(71) = 1 / (Q(5,38) - MB2)
  den(77) = 1 / (Q(5,25) - MB2)
  den(81) = 1 / (Q(5,22) - MB2)
  den(87) = 1 / (Q(5,65) - MC2)
  den(89) = 1 / (Q(5,73))
  den(93) = 1 / (Q(5,14) - MC2)
  den(103) = 1 / (Q(5,49) - MC2)
  den(119) = 1 / (Q(5,70))
  den(123) = 1 / (Q(5,52) - MC2)
  den(126) = 1 / (Q(5,68) - MC2)
  den(128) = 1 / (Q(5,11) - MC2)
  den(146) = 1 / (Q(5,69))

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
  den(21) = den(1)*den(16)
  den(23) = den(2)*den(22)
  den(24) = den(21)*den(23)
  den(25) = den(13)*den(16)
  den(26) = den(12)*den(25)
  den(27) = den(10)*den(19)
  den(28) = den(6)*den(23)
  den(31) = den(29)*den(30)
  den(33) = den(1)*den(32)
  den(34) = den(31)*den(33)
  den(35) = den(1)*den(30)
  den(37) = den(29)*den(36)
  den(38) = den(35)*den(37)
  den(39) = den(1)*den(29)
  den(41) = den(30)*den(40)
  den(42) = den(39)*den(41)
  den(44) = den(30)*den(43)
  den(46) = den(1)*den(45)
  den(47) = den(44)*den(46)
  den(48) = den(36)*den(43)
  den(49) = den(35)*den(48)
  den(50) = den(1)*den(43)
  den(52) = den(30)*den(51)
  den(53) = den(50)*den(52)
  den(54) = den(41)*den(46)
  den(55) = den(33)*den(52)
  den(56) = den(19)*den(37)
  den(57) = den(6)*den(37)
  den(58) = den(19)*den(48)
  den(59) = den(6)*den(48)
  den(60) = den(14)*den(46)
  den(61) = den(14)*den(33)
  den(62) = den(25)*den(46)
  den(63) = den(25)*den(33)
  den(66) = den(3)*den(65)
  den(68) = den(64)*den(67)
  den(69) = den(66)*den(68)
  den(70) = den(3)*den(64)
  den(72) = den(65)*den(71)
  den(73) = den(70)*den(72)
  den(74) = den(64)*den(65)
  den(75) = den(14)*den(74)
  den(76) = den(16)*den(65)
  den(78) = den(64)*den(77)
  den(79) = den(76)*den(78)
  den(80) = den(16)*den(64)
  den(82) = den(65)*den(81)
  den(83) = den(80)*den(82)
  den(84) = den(25)*den(74)
  den(85) = den(72)*den(78)
  den(86) = den(68)*den(82)
  den(88) = den(30)*den(65)
  den(90) = den(87)*den(89)
  den(91) = den(88)*den(90)
  den(92) = den(30)*den(87)
  den(94) = den(65)*den(93)
  den(95) = den(92)*den(94)
  den(96) = den(65)*den(87)
  den(97) = den(41)*den(96)
  den(98) = den(45)*den(65)
  den(99) = den(44)*den(98)
  den(100) = den(43)*den(89)
  den(101) = den(88)*den(100)
  den(102) = den(43)*den(65)
  den(104) = den(30)*den(103)
  den(105) = den(102)*den(104)
  den(106) = den(41)*den(98)
  den(107) = den(94)*den(104)
  den(108) = den(82)*den(90)
  den(109) = den(72)*den(90)
  den(110) = den(72)*den(100)
  den(111) = den(82)*den(100)
  den(112) = den(14)*den(98)
  den(113) = den(14)*den(94)
  den(114) = den(25)*den(98)
  den(115) = den(25)*den(94)
  den(116) = den(32)*den(64)
  den(117) = den(31)*den(116)
  den(118) = den(30)*den(64)
  den(120) = den(29)*den(119)
  den(121) = den(118)*den(120)
  den(122) = den(29)*den(64)
  den(124) = den(30)*den(123)
  den(125) = den(122)*den(124)
  den(127) = den(30)*den(126)
  den(129) = den(64)*den(128)
  den(130) = den(127)*den(129)
  den(131) = den(119)*den(126)
  den(132) = den(118)*den(131)
  den(133) = den(64)*den(126)
  den(134) = den(52)*den(133)
  den(135) = den(124)*den(129)
  den(136) = den(52)*den(116)
  den(137) = den(78)*den(120)
  den(138) = den(68)*den(120)
  den(139) = den(78)*den(131)
  den(140) = den(68)*den(131)
  den(141) = den(14)*den(129)
  den(142) = den(14)*den(116)
  den(143) = den(25)*den(129)
  den(144) = den(25)*den(116)
  den(145) = den(2)*den(30)
  den(147) = den(87)*den(146)
  den(148) = den(145)*den(147)
  den(149) = den(2)*den(93)
  den(150) = den(92)*den(149)
  den(151) = den(2)*den(87)
  den(152) = den(124)*den(151)
  den(153) = den(2)*den(128)
  den(154) = den(127)*den(153)
  den(155) = den(126)*den(146)
  den(156) = den(145)*den(155)
  den(157) = den(2)*den(126)
  den(158) = den(104)*den(157)
  den(159) = den(124)*den(153)
  den(160) = den(104)*den(149)
  den(161) = den(23)*den(147)
  den(162) = den(10)*den(147)
  den(163) = den(10)*den(155)
  den(164) = den(23)*den(155)
  den(165) = den(14)*den(153)
  den(166) = den(14)*den(149)
  den(167) = den(25)*den(153)
  den(168) = den(25)*den(149)
  den(169) = den(41)*den(147)
  den(170) = den(52)*den(147)
  den(171) = den(90)*den(124)
  den(172) = den(52)*den(90)
  den(173) = den(41)*den(120)
  den(174) = den(37)*den(124)
  den(175) = den(37)*den(104)
  den(176) = den(104)*den(120)
  den(177) = den(41)*den(155)
  den(178) = den(41)*den(131)
  den(179) = den(52)*den(155)
  den(180) = den(104)*den(131)
  den(181) = den(48)*den(124)
  den(182) = den(100)*den(124)
  den(183) = den(52)*den(100)
  den(184) = den(48)*den(104)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_pphbbj_cccxcxbbxh_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_pphbbj_cccxcxbbxh_1_/**/DREALKIND, only: &
    & ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for charm charm anti-charm anti-charm bottom anti-bottom higgs -> 0
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
  use ol_external_pphbbj_cccxcxbbxh_1, only: &
    & external_perm_pphbbj_cccxcxbbxh_1, &
    & external_perm_inv_pphbbj_cccxcxbbxh_1, &
    & extcomb_perm_pphbbj_cccxcxbbxh_1, &
    & average_factor_pphbbj_cccxcxbbxh_1
  use ol_external_pphbbj_cccxcxbbxh_1, only: &
    & H, hel_not_initialised, hel_init, POLSEL
  use ol_colourmatrix_pphbbj_cccxcxbbxh_1_/**/REALKIND, only: &
    & colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_pphbbj_cccxcxbbxh_1_/**/REALKIND, only: &
    & check_forced_parameters
  use ol_heltables_pphbbj_cccxcxbbxh_1
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
  complex(REALKIND) :: MOM_LC(4), M1(6), M1helarray(6,64)
  real(REALKIND)    :: P_scatt_intern(0:3,7)
  complex(REALKIND) :: epLC(1:4), epStd(1:4,2)
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(2), ex6(2), ex7(1)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf2(2,12), wf4(4,15), wf8(8,94), wf16(16,20), wf64(64,96)

  type(polcont) :: A(64,96)
  complex(REALKIND) :: Aj(96)

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
  extmasses2 = [ rMC2, rMC2, rMC2, rMC2, rMB2, rMB2, rMH2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, &
    & external_perm_inv_pphbbj_cccxcxbbxh_1,7)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,7)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_pphbbj_cccxcxbbxh_1(I)
  else if (I < 0) then
    JBmunu = external_perm_pphbbj_cccxcxbbxh_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_pphbbj_cccxcxbbxh_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  42 continue
  ! external WFs
  call pol_wf_Q(P(:,1), rMC, H1, ex1, POLSEL(1))
  call pol_wf_Q(P(:,2), rMC, H2, ex2, POLSEL(2))
  call pol_wf_A(P(:,3), rMC, H3, ex3, POLSEL(3))
  call pol_wf_A(P(:,4), rMC, H4, ex4, POLSEL(4))
  call pol_wf_Q(P(:,5), rMB, H5, ex5, POLSEL(5))
  call pol_wf_A(P(:,6), rMB, H6, ex6, POLSEL(6))
  call pol_wf_S(P(:,7), rMH, H7, ex7, POLSEL(7))


  if (ntry == 1) then
    shift = 1
    ! call helbookkeeping_flip(H1, 1, shift, eflip, exthel, firstpol)
    ! call helbookkeeping_wf(H1, ex1, shift) ...

    if (any(POLSEL /= 0)) then

      call pol_wf_Q(P(:,1), rMC, H1, ex1, 0)
      call pol_wf_Q(P(:,2), rMC, H2, ex2, 0)
      call pol_wf_A(P(:,3), rMC, H3, ex3, 0)
      call pol_wf_A(P(:,4), rMC, H4, ex4, 0)
      call pol_wf_Q(P(:,5), rMB, H5, ex5, 0)
      call pol_wf_A(P(:,6), rMB, H6, ex6, 0)
      call pol_wf_S(P(:,7), rMH, H7, ex7, 0)

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
  call vert_QA_V(ntry, ex1, ex3, wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_QA_V(ntry, ex2, ex4, wf4(:,2), n3(:,2), t3x4(:,:,2))
  call vert_QS_A(gH,ntry, ex5, ex7, wf2(:,1), n3(:,3), t3x2(:,:,1))
  call prop_Q_A(ntry, wf2(:,1), Q(:,80), MB, 1_intkind1, wf2(:,2), n2(1))
  call vert_AV_Q(ntry, ex6, wf4(:,1), wf8(:,1), n3(:,4), t3x8(:,:,1))
  call vert_VQ_A(ntry, wf4(:,2), wf2(:,2), wf8(:,2), n3(:,5), t3x8(:,:,2))
  call prop_A_Q(ntry, wf8(:,1), Q(:,37), MB, 1_intkind1, wf8(:,3), n2(2))
  call vert_AV_Q(ntry, ex6, wf4(:,2), wf8(:,4), n3(:,6), t3x8(:,:,3))
  call vert_VQ_A(ntry, wf4(:,1), wf2(:,2), wf8(:,5), n3(:,7), t3x8(:,:,4))
  call prop_A_Q(ntry, wf8(:,4), Q(:,42), MB, 1_intkind1, wf8(:,6), n2(3))
  call vert_QA_V(ntry, wf2(:,2), ex6, wf4(:,3), n3(:,8), t3x4(:,:,3))
  call vert_UV_W(ntry, wf4(:,1), Q(:,5), wf4(:,2), Q(:,10), wf16(:,1), n3(:,9), t3x16(:,:,1))
  call vert_SA_Q(gH,ntry, ex7, ex6, wf2(:,3), n3(:,10), t3x2(:,:,2))
  call prop_A_Q(ntry, wf2(:,3), Q(:,96), MB, 1_intkind1, wf2(:,4), n2(4))
  call vert_VQ_A(ntry, wf4(:,1), ex5, wf8(:,7), n3(:,11), t3x8(:,:,5))
  call vert_AV_Q(ntry, wf2(:,4), wf4(:,2), wf8(:,8), n3(:,12), t3x8(:,:,6))
  call prop_Q_A(ntry, wf8(:,7), Q(:,21), MB, 1_intkind1, wf8(:,9), n2(5))
  call vert_VQ_A(ntry, wf4(:,2), ex5, wf8(:,10), n3(:,13), t3x8(:,:,7))
  call vert_AV_Q(ntry, wf2(:,4), wf4(:,1), wf8(:,11), n3(:,14), t3x8(:,:,8))
  call prop_Q_A(ntry, wf8(:,10), Q(:,26), MB, 1_intkind1, wf8(:,12), n2(6))
  call vert_QA_V(ntry, ex5, wf2(:,4), wf4(:,4), n3(:,15), t3x4(:,:,4))
  call vert_QS_A(gH,ntry, wf8(:,9), ex7, wf8(:,13), n3(:,16), t3x8(:,:,9))
  call vert_QS_A(gH,ntry, wf8(:,12), ex7, wf8(:,14), n3(:,17), t3x8(:,:,10))
  call vert_QS_A(gH,ntry, ex2, ex7, wf2(:,5), n3(:,18), t3x2(:,:,3))
  call vert_QA_V(ntry, ex5, ex6, wf4(:,5), n3(:,19), t3x4(:,:,5))
  call prop_Q_A(ntry, wf2(:,5), Q(:,66), MC, 1_intkind1, wf2(:,6), n2(7))
  call vert_AV_Q(ntry, ex4, wf4(:,1), wf8(:,15), n3(:,20), t3x8(:,:,11))
  call vert_VQ_A(ntry, wf4(:,5), wf2(:,6), wf8(:,16), n3(:,21), t3x8(:,:,12))
  call prop_A_Q(ntry, wf8(:,15), Q(:,13), MC, 1_intkind1, wf8(:,17), n2(8))
  call vert_QA_V(ntry, wf2(:,6), ex4, wf4(:,6), n3(:,22), t3x4(:,:,6))
  call vert_UV_W(ntry, wf4(:,1), Q(:,5), wf4(:,5), Q(:,48), wf16(:,2), n3(:,23), t3x16(:,:,2))
  call vert_AV_Q(ntry, ex4, wf4(:,5), wf8(:,18), n3(:,24), t3x8(:,:,13))
  call vert_VQ_A(ntry, wf4(:,1), wf2(:,6), wf8(:,19), n3(:,25), t3x8(:,:,14))
  call prop_A_Q(ntry, wf8(:,18), Q(:,56), MC, 1_intkind1, wf8(:,20), n2(9))
  call vert_SA_Q(gH,ntry, ex7, ex4, wf2(:,7), n3(:,26), t3x2(:,:,4))
  call prop_A_Q(ntry, wf2(:,7), Q(:,72), MC, 1_intkind1, wf2(:,8), n2(10))
  call vert_VQ_A(ntry, wf4(:,1), ex2, wf8(:,21), n3(:,27), t3x8(:,:,15))
  call vert_AV_Q(ntry, wf2(:,8), wf4(:,5), wf8(:,22), n3(:,28), t3x8(:,:,16))
  call prop_Q_A(ntry, wf8(:,21), Q(:,7), MC, 1_intkind1, wf8(:,23), n2(11))
  call vert_QA_V(ntry, ex2, wf2(:,8), wf4(:,7), n3(:,29), t3x4(:,:,7))
  call vert_VQ_A(ntry, wf4(:,5), ex2, wf8(:,24), n3(:,30), t3x8(:,:,17))
  call vert_AV_Q(ntry, wf2(:,8), wf4(:,1), wf8(:,25), n3(:,31), t3x8(:,:,18))
  call prop_Q_A(ntry, wf8(:,24), Q(:,50), MC, 1_intkind1, wf8(:,26), n2(12))
  call vert_QS_A(gH,ntry, wf8(:,23), ex7, wf8(:,27), n3(:,32), t3x8(:,:,19))
  call vert_QS_A(gH,ntry, wf8(:,26), ex7, wf8(:,28), n3(:,33), t3x8(:,:,20))
  call vert_AV_Q(ntry, ex6, wf4(:,6), wf8(:,29), n3(:,34), t3x8(:,:,21))
  call vert_VQ_A(ntry, wf4(:,6), ex5, wf8(:,30), n3(:,35), t3x8(:,:,22))
  call vert_AV_Q(ntry, ex6, wf4(:,7), wf8(:,31), n3(:,36), t3x8(:,:,23))
  call vert_VQ_A(ntry, wf4(:,7), ex5, wf8(:,32), n3(:,37), t3x8(:,:,24))
  call vert_QA_V(ntry, wf8(:,23), ex4, wf16(:,3), n3(:,38), t3x16(:,:,3))
  call vert_QA_V(ntry, ex2, wf8(:,17), wf16(:,4), n3(:,39), t3x16(:,:,4))
  call vert_QA_V(ntry, ex1, ex4, wf4(:,8), n3(:,40), t3x4(:,:,8))
  call vert_QA_V(ntry, ex2, ex3, wf4(:,9), n3(:,41), t3x4(:,:,9))
  call vert_AV_Q(ntry, ex6, wf4(:,8), wf8(:,33), n3(:,42), t3x8(:,:,25))
  call vert_VQ_A(ntry, wf4(:,9), wf2(:,2), wf8(:,34), n3(:,43), t3x8(:,:,26))
  call prop_A_Q(ntry, wf8(:,33), Q(:,41), MB, 1_intkind1, wf8(:,35), n2(13))
  call vert_AV_Q(ntry, ex6, wf4(:,9), wf8(:,36), n3(:,44), t3x8(:,:,27))
  call vert_VQ_A(ntry, wf4(:,8), wf2(:,2), wf8(:,37), n3(:,45), t3x8(:,:,28))
  call prop_A_Q(ntry, wf8(:,36), Q(:,38), MB, 1_intkind1, wf8(:,38), n2(14))
  call vert_UV_W(ntry, wf4(:,9), Q(:,6), wf4(:,8), Q(:,9), wf16(:,5), n3(:,46), t3x16(:,:,5))
  call vert_VQ_A(ntry, wf4(:,8), ex5, wf8(:,39), n3(:,47), t3x8(:,:,29))
  call vert_AV_Q(ntry, wf2(:,4), wf4(:,9), wf8(:,40), n3(:,48), t3x8(:,:,30))
  call prop_Q_A(ntry, wf8(:,39), Q(:,25), MB, 1_intkind1, wf8(:,41), n2(15))
  call vert_VQ_A(ntry, wf4(:,9), ex5, wf8(:,42), n3(:,49), t3x8(:,:,31))
  call vert_AV_Q(ntry, wf2(:,4), wf4(:,8), wf8(:,43), n3(:,50), t3x8(:,:,32))
  call prop_Q_A(ntry, wf8(:,42), Q(:,22), MB, 1_intkind1, wf8(:,44), n2(16))
  call vert_QS_A(gH,ntry, wf8(:,41), ex7, wf8(:,45), n3(:,51), t3x8(:,:,33))
  call vert_QS_A(gH,ntry, wf8(:,44), ex7, wf8(:,46), n3(:,52), t3x8(:,:,34))
  call vert_QS_A(gH,ntry, ex1, ex7, wf2(:,9), n3(:,53), t3x2(:,:,5))
  call prop_Q_A(ntry, wf2(:,9), Q(:,65), MC, 1_intkind1, wf2(:,10), n2(17))
  call vert_QA_V(ntry, wf2(:,10), ex4, wf4(:,10), n3(:,54), t3x4(:,:,10))
  call vert_UV_W(ntry, wf4(:,9), Q(:,6), wf4(:,5), Q(:,48), wf16(:,6), n3(:,55), t3x16(:,:,6))
  call vert_AV_Q(ntry, ex4, wf4(:,9), wf8(:,47), n3(:,56), t3x8(:,:,35))
  call vert_VQ_A(ntry, wf4(:,5), wf2(:,10), wf8(:,48), n3(:,57), t3x8(:,:,36))
  call prop_A_Q(ntry, wf8(:,47), Q(:,14), MC, 1_intkind1, wf8(:,49), n2(18))
  call vert_VQ_A(ntry, wf4(:,9), wf2(:,10), wf8(:,50), n3(:,58), t3x8(:,:,37))
  call vert_VQ_A(ntry, wf4(:,9), ex1, wf8(:,51), n3(:,59), t3x8(:,:,38))
  call prop_Q_A(ntry, wf8(:,51), Q(:,7), MC, 1_intkind1, wf8(:,52), n2(19))
  call vert_QA_V(ntry, ex1, wf2(:,8), wf4(:,11), n3(:,60), t3x4(:,:,11))
  call vert_VQ_A(ntry, wf4(:,5), ex1, wf8(:,53), n3(:,61), t3x8(:,:,39))
  call vert_AV_Q(ntry, wf2(:,8), wf4(:,9), wf8(:,54), n3(:,62), t3x8(:,:,40))
  call prop_Q_A(ntry, wf8(:,53), Q(:,49), MC, 1_intkind1, wf8(:,55), n2(20))
  call vert_QS_A(gH,ntry, wf8(:,52), ex7, wf8(:,56), n3(:,63), t3x8(:,:,41))
  call vert_QS_A(gH,ntry, wf8(:,55), ex7, wf8(:,57), n3(:,64), t3x8(:,:,42))
  call vert_AV_Q(ntry, ex6, wf4(:,10), wf8(:,58), n3(:,65), t3x8(:,:,43))
  call vert_VQ_A(ntry, wf4(:,10), ex5, wf8(:,59), n3(:,66), t3x8(:,:,44))
  call vert_VQ_A(ntry, wf4(:,11), ex5, wf8(:,60), n3(:,67), t3x8(:,:,45))
  call vert_AV_Q(ntry, ex6, wf4(:,11), wf8(:,61), n3(:,68), t3x8(:,:,46))
  call vert_QA_V(ntry, wf8(:,52), ex4, wf16(:,7), n3(:,69), t3x16(:,:,7))
  call vert_QA_V(ntry, ex1, wf8(:,49), wf16(:,8), n3(:,70), t3x16(:,:,8))
  call vert_AV_Q(ntry, ex3, wf4(:,8), wf8(:,62), n3(:,71), t3x8(:,:,47))
  call prop_A_Q(ntry, wf8(:,62), Q(:,13), MC, 1_intkind1, wf8(:,63), n2(21))
  call vert_QA_V(ntry, wf2(:,6), ex3, wf4(:,12), n3(:,72), t3x4(:,:,12))
  call vert_UV_W(ntry, wf4(:,8), Q(:,9), wf4(:,5), Q(:,48), wf16(:,9), n3(:,73), t3x16(:,:,9))
  call vert_AV_Q(ntry, ex3, wf4(:,5), wf8(:,64), n3(:,74), t3x8(:,:,48))
  call vert_VQ_A(ntry, wf4(:,8), wf2(:,6), wf8(:,65), n3(:,75), t3x8(:,:,49))
  call prop_A_Q(ntry, wf8(:,64), Q(:,52), MC, 1_intkind1, wf8(:,66), n2(22))
  call vert_SA_Q(gH,ntry, ex7, ex3, wf2(:,11), n3(:,76), t3x2(:,:,6))
  call prop_A_Q(ntry, wf2(:,11), Q(:,68), MC, 1_intkind1, wf2(:,12), n2(23))
  call vert_VQ_A(ntry, wf4(:,8), ex2, wf8(:,67), n3(:,77), t3x8(:,:,50))
  call vert_AV_Q(ntry, wf2(:,12), wf4(:,5), wf8(:,68), n3(:,78), t3x8(:,:,51))
  call prop_Q_A(ntry, wf8(:,67), Q(:,11), MC, 1_intkind1, wf8(:,69), n2(24))
  call vert_QA_V(ntry, ex2, wf2(:,12), wf4(:,13), n3(:,79), t3x4(:,:,13))
  call vert_AV_Q(ntry, wf2(:,12), wf4(:,8), wf8(:,70), n3(:,80), t3x8(:,:,52))
  call vert_QS_A(gH,ntry, wf8(:,69), ex7, wf8(:,71), n3(:,81), t3x8(:,:,53))
  call vert_AV_Q(ntry, ex6, wf4(:,12), wf8(:,72), n3(:,82), t3x8(:,:,54))
  call vert_VQ_A(ntry, wf4(:,12), ex5, wf8(:,73), n3(:,83), t3x8(:,:,55))
  call vert_AV_Q(ntry, ex6, wf4(:,13), wf8(:,74), n3(:,84), t3x8(:,:,56))
  call vert_VQ_A(ntry, wf4(:,13), ex5, wf8(:,75), n3(:,85), t3x8(:,:,57))
  call vert_QA_V(ntry, wf8(:,69), ex3, wf16(:,10), n3(:,86), t3x16(:,:,10))
  call vert_QA_V(ntry, ex2, wf8(:,63), wf16(:,11), n3(:,87), t3x16(:,:,11))
  call vert_QA_V(ntry, wf2(:,10), ex3, wf4(:,14), n3(:,88), t3x4(:,:,14))
  call vert_UV_W(ntry, wf4(:,2), Q(:,10), wf4(:,5), Q(:,48), wf16(:,12), n3(:,89), t3x16(:,:,12))
  call vert_AV_Q(ntry, ex3, wf4(:,2), wf8(:,76), n3(:,90), t3x8(:,:,58))
  call prop_A_Q(ntry, wf8(:,76), Q(:,14), MC, 1_intkind1, wf8(:,77), n2(25))
  call vert_VQ_A(ntry, wf4(:,2), wf2(:,10), wf8(:,78), n3(:,91), t3x8(:,:,59))
  call vert_VQ_A(ntry, wf4(:,2), ex1, wf8(:,79), n3(:,92), t3x8(:,:,60))
  call prop_Q_A(ntry, wf8(:,79), Q(:,11), MC, 1_intkind1, wf8(:,80), n2(26))
  call vert_QA_V(ntry, ex1, wf2(:,12), wf4(:,15), n3(:,93), t3x4(:,:,15))
  call vert_AV_Q(ntry, wf2(:,12), wf4(:,2), wf8(:,81), n3(:,94), t3x8(:,:,61))
  call vert_QS_A(gH,ntry, wf8(:,80), ex7, wf8(:,82), n3(:,95), t3x8(:,:,62))
  call vert_AV_Q(ntry, ex6, wf4(:,14), wf8(:,83), n3(:,96), t3x8(:,:,63))
  call vert_VQ_A(ntry, wf4(:,14), ex5, wf8(:,84), n3(:,97), t3x8(:,:,64))
  call vert_VQ_A(ntry, wf4(:,15), ex5, wf8(:,85), n3(:,98), t3x8(:,:,65))
  call vert_AV_Q(ntry, ex6, wf4(:,15), wf8(:,86), n3(:,99), t3x8(:,:,66))
  call vert_QA_V(ntry, wf8(:,80), ex3, wf16(:,13), n3(:,100), t3x16(:,:,13))
  call vert_QA_V(ntry, ex1, wf8(:,77), wf16(:,14), n3(:,101), t3x16(:,:,14))
  call vert_VQ_A(ntry, wf4(:,14), ex2, wf8(:,87), n3(:,102), t3x8(:,:,67))
  call vert_QA_V(ntry, wf8(:,26), ex4, wf16(:,15), n3(:,103), t3x16(:,:,15))
  call vert_QA_V(ntry, ex2, wf8(:,66), wf16(:,16), n3(:,104), t3x16(:,:,16))
  call vert_QA_V(ntry, wf8(:,26), ex3, wf16(:,17), n3(:,105), t3x16(:,:,17))
  call vert_VQ_A(ntry, wf4(:,12), ex1, wf8(:,88), n3(:,106), t3x8(:,:,68))
  call vert_QA_V(ntry, ex1, wf8(:,66), wf16(:,18), n3(:,107), t3x16(:,:,18))
  call vert_QA_V(ntry, wf8(:,55), ex3, wf16(:,19), n3(:,108), t3x16(:,:,19))
  call vert_QA_V(ntry, wf8(:,55), ex4, wf16(:,20), n3(:,109), t3x16(:,:,20))
  call vert_VQ_A(ntry, wf4(:,15), ex2, wf8(:,89), n3(:,110), t3x8(:,:,69))
  call vert_VQ_A(ntry, wf4(:,13), ex1, wf8(:,90), n3(:,111), t3x8(:,:,70))
  call vert_AV_Q(ntry, ex4, wf4(:,15), wf8(:,91), n3(:,112), t3x8(:,:,71))
  call vert_VQ_A(ntry, wf4(:,7), ex1, wf8(:,92), n3(:,113), t3x8(:,:,72))
  call vert_VQ_A(ntry, wf4(:,11), ex2, wf8(:,93), n3(:,114), t3x8(:,:,73))
  call vert_AV_Q(ntry, ex3, wf4(:,11), wf8(:,94), n3(:,115), t3x8(:,:,74))


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

    M2munu = M2munu / average_factor_pphbbj_cccxcxbbxh_1
  end if

  ! Colour and helicity average and symmetry factor of outgoing particles
  M2add = M2 / average_factor_pphbbj_cccxcxbbxh_1

  do k = 0, 30-1
    M2(k) = M2add(extcomb_perm_pphbbj_cccxcxbbxh_1(k))
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

    call cont_QA(nsync, wf8(:,2), wf8(:,3), A(:,1), n3(:,116), t3x64(:,:,1), nhel, den(7))
    call cont_QA(nsync, wf8(:,5), wf8(:,6), A(:,2), n3(:,117), t3x64(:,:,2), nhel, den(11))
    call cont_VV(nsync, wf4(:,3), wf16(:,1), A(:,3), n3(:,118), t3x64(:,:,3), nhel, den(15))
    call cont_QA(nsync, wf8(:,8), wf8(:,9), A(:,4), n3(:,119), t3x64(:,:,4), nhel, den(20))
    call cont_QA(nsync, wf8(:,11), wf8(:,12), A(:,5), n3(:,120), t3x64(:,:,5), nhel, den(24))
    call cont_VV(nsync, wf16(:,1), wf4(:,4), A(:,6), n3(:,121), t3x64(:,:,6), nhel, den(26))
    call cont_QA(nsync, wf8(:,6), wf8(:,13), A(:,7), n3(:,122), t3x64(:,:,7), nhel, den(27))
    call cont_QA(nsync, wf8(:,3), wf8(:,14), A(:,8), n3(:,123), t3x64(:,:,8), nhel, den(28))
    call cont_QA(nsync, wf8(:,16), wf8(:,17), A(:,9), n3(:,124), t3x64(:,:,9), nhel, den(34))
    call cont_VV(nsync, wf4(:,6), wf16(:,2), A(:,10), n3(:,125), t3x64(:,:,10), nhel, den(38))
    call cont_QA(nsync, wf8(:,19), wf8(:,20), A(:,11), n3(:,126), t3x64(:,:,11), nhel, den(42))
    call cont_QA(nsync, wf8(:,22), wf8(:,23), A(:,12), n3(:,127), t3x64(:,:,12), nhel, den(47))
    call cont_VV(nsync, wf16(:,2), wf4(:,7), A(:,13), n3(:,128), t3x64(:,:,13), nhel, den(49))
    call cont_QA(nsync, wf8(:,25), wf8(:,26), A(:,14), n3(:,129), t3x64(:,:,14), nhel, den(53))
    call cont_QA(nsync, wf8(:,20), wf8(:,27), A(:,15), n3(:,130), t3x64(:,:,15), nhel, den(54))
    call cont_QA(nsync, wf8(:,17), wf8(:,28), A(:,16), n3(:,131), t3x64(:,:,16), nhel, den(55))
    call cont_QA(nsync, wf8(:,9), wf8(:,29), A(:,17), n3(:,132), t3x64(:,:,17), nhel, den(56))
    call cont_QA(nsync, wf8(:,3), wf8(:,30), A(:,18), n3(:,133), t3x64(:,:,18), nhel, den(57))
    call cont_QA(nsync, wf8(:,9), wf8(:,31), A(:,19), n3(:,134), t3x64(:,:,19), nhel, den(58))
    call cont_QA(nsync, wf8(:,3), wf8(:,32), A(:,20), n3(:,135), t3x64(:,:,20), nhel, den(59))
    call cont_VV(nsync, wf4(:,3), wf16(:,3), A(:,21), n3(:,136), t3x64(:,:,21), nhel, den(60))
    call cont_VV(nsync, wf4(:,3), wf16(:,4), A(:,22), n3(:,137), t3x64(:,:,22), nhel, den(61))
    call cont_VV(nsync, wf4(:,4), wf16(:,3), A(:,23), n3(:,138), t3x64(:,:,23), nhel, den(62))
    call cont_VV(nsync, wf4(:,4), wf16(:,4), A(:,24), n3(:,139), t3x64(:,:,24), nhel, den(63))
    call cont_QA(nsync, wf8(:,34), wf8(:,35), A(:,25), n3(:,140), t3x64(:,:,25), nhel, den(69))
    call cont_QA(nsync, wf8(:,37), wf8(:,38), A(:,26), n3(:,141), t3x64(:,:,26), nhel, den(73))
    call cont_VV(nsync, wf4(:,3), wf16(:,5), A(:,27), n3(:,142), t3x64(:,:,27), nhel, den(75))
    call cont_QA(nsync, wf8(:,40), wf8(:,41), A(:,28), n3(:,143), t3x64(:,:,28), nhel, den(79))
    call cont_QA(nsync, wf8(:,43), wf8(:,44), A(:,29), n3(:,144), t3x64(:,:,29), nhel, den(83))
    call cont_VV(nsync, wf4(:,4), wf16(:,5), A(:,30), n3(:,145), t3x64(:,:,30), nhel, den(84))
    call cont_QA(nsync, wf8(:,38), wf8(:,45), A(:,31), n3(:,146), t3x64(:,:,31), nhel, den(85))
    call cont_QA(nsync, wf8(:,35), wf8(:,46), A(:,32), n3(:,147), t3x64(:,:,32), nhel, den(86))
    call cont_VV(nsync, wf4(:,10), wf16(:,6), A(:,33), n3(:,148), t3x64(:,:,33), nhel, den(91))
    call cont_QA(nsync, wf8(:,48), wf8(:,49), A(:,34), n3(:,149), t3x64(:,:,34), nhel, den(95))
    call cont_QA(nsync, wf8(:,20), wf8(:,50), A(:,35), n3(:,150), t3x64(:,:,35), nhel, den(97))
    call cont_QA(nsync, wf8(:,22), wf8(:,52), A(:,36), n3(:,151), t3x64(:,:,36), nhel, den(99))
    call cont_VV(nsync, wf16(:,6), wf4(:,11), A(:,37), n3(:,152), t3x64(:,:,37), nhel, den(101))
    call cont_QA(nsync, wf8(:,54), wf8(:,55), A(:,38), n3(:,153), t3x64(:,:,38), nhel, den(105))
    call cont_QA(nsync, wf8(:,20), wf8(:,56), A(:,39), n3(:,154), t3x64(:,:,39), nhel, den(106))
    call cont_QA(nsync, wf8(:,49), wf8(:,57), A(:,40), n3(:,155), t3x64(:,:,40), nhel, den(107))
    call cont_QA(nsync, wf8(:,44), wf8(:,58), A(:,41), n3(:,156), t3x64(:,:,41), nhel, den(108))
    call cont_QA(nsync, wf8(:,38), wf8(:,59), A(:,42), n3(:,157), t3x64(:,:,42), nhel, den(109))
    call cont_QA(nsync, wf8(:,38), wf8(:,60), A(:,43), n3(:,158), t3x64(:,:,43), nhel, den(110))
    call cont_QA(nsync, wf8(:,44), wf8(:,61), A(:,44), n3(:,159), t3x64(:,:,44), nhel, den(111))
    call cont_VV(nsync, wf4(:,3), wf16(:,7), A(:,45), n3(:,160), t3x64(:,:,45), nhel, den(112))
    call cont_VV(nsync, wf4(:,3), wf16(:,8), A(:,46), n3(:,161), t3x64(:,:,46), nhel, den(113))
    call cont_VV(nsync, wf4(:,4), wf16(:,7), A(:,47), n3(:,162), t3x64(:,:,47), nhel, den(114))
    call cont_VV(nsync, wf4(:,4), wf16(:,8), A(:,48), n3(:,163), t3x64(:,:,48), nhel, den(115))
    call cont_QA(nsync, wf8(:,16), wf8(:,63), A(:,49), n3(:,164), t3x64(:,:,49), nhel, den(117))
    call cont_VV(nsync, wf4(:,12), wf16(:,9), A(:,50), n3(:,165), t3x64(:,:,50), nhel, den(121))
    call cont_QA(nsync, wf8(:,65), wf8(:,66), A(:,51), n3(:,166), t3x64(:,:,51), nhel, den(125))
    call cont_QA(nsync, wf8(:,68), wf8(:,69), A(:,52), n3(:,167), t3x64(:,:,52), nhel, den(130))
    call cont_VV(nsync, wf16(:,9), wf4(:,13), A(:,53), n3(:,168), t3x64(:,:,53), nhel, den(132))
    call cont_QA(nsync, wf8(:,26), wf8(:,70), A(:,54), n3(:,169), t3x64(:,:,54), nhel, den(134))
    call cont_QA(nsync, wf8(:,66), wf8(:,71), A(:,55), n3(:,170), t3x64(:,:,55), nhel, den(135))
    call cont_QA(nsync, wf8(:,28), wf8(:,63), A(:,56), n3(:,171), t3x64(:,:,56), nhel, den(136))
    call cont_QA(nsync, wf8(:,41), wf8(:,72), A(:,57), n3(:,172), t3x64(:,:,57), nhel, den(137))
    call cont_QA(nsync, wf8(:,35), wf8(:,73), A(:,58), n3(:,173), t3x64(:,:,58), nhel, den(138))
    call cont_QA(nsync, wf8(:,41), wf8(:,74), A(:,59), n3(:,174), t3x64(:,:,59), nhel, den(139))
    call cont_QA(nsync, wf8(:,35), wf8(:,75), A(:,60), n3(:,175), t3x64(:,:,60), nhel, den(140))
    call cont_VV(nsync, wf4(:,3), wf16(:,10), A(:,61), n3(:,176), t3x64(:,:,61), nhel, den(141))
    call cont_VV(nsync, wf4(:,3), wf16(:,11), A(:,62), n3(:,177), t3x64(:,:,62), nhel, den(142))
    call cont_VV(nsync, wf4(:,4), wf16(:,10), A(:,63), n3(:,178), t3x64(:,:,63), nhel, den(143))
    call cont_VV(nsync, wf4(:,4), wf16(:,11), A(:,64), n3(:,179), t3x64(:,:,64), nhel, den(144))
    call cont_VV(nsync, wf4(:,14), wf16(:,12), A(:,65), n3(:,180), t3x64(:,:,65), nhel, den(148))
    call cont_QA(nsync, wf8(:,48), wf8(:,77), A(:,66), n3(:,181), t3x64(:,:,66), nhel, den(150))
    call cont_QA(nsync, wf8(:,66), wf8(:,78), A(:,67), n3(:,182), t3x64(:,:,67), nhel, den(152))
    call cont_QA(nsync, wf8(:,68), wf8(:,80), A(:,68), n3(:,183), t3x64(:,:,68), nhel, den(154))
    call cont_VV(nsync, wf16(:,12), wf4(:,15), A(:,69), n3(:,184), t3x64(:,:,69), nhel, den(156))
    call cont_QA(nsync, wf8(:,55), wf8(:,81), A(:,70), n3(:,185), t3x64(:,:,70), nhel, den(158))
    call cont_QA(nsync, wf8(:,66), wf8(:,82), A(:,71), n3(:,186), t3x64(:,:,71), nhel, den(159))
    call cont_QA(nsync, wf8(:,57), wf8(:,77), A(:,72), n3(:,187), t3x64(:,:,72), nhel, den(160))
    call cont_QA(nsync, wf8(:,12), wf8(:,83), A(:,73), n3(:,188), t3x64(:,:,73), nhel, den(161))
    call cont_QA(nsync, wf8(:,6), wf8(:,84), A(:,74), n3(:,189), t3x64(:,:,74), nhel, den(162))
    call cont_QA(nsync, wf8(:,6), wf8(:,85), A(:,75), n3(:,190), t3x64(:,:,75), nhel, den(163))
    call cont_QA(nsync, wf8(:,12), wf8(:,86), A(:,76), n3(:,191), t3x64(:,:,76), nhel, den(164))
    call cont_VV(nsync, wf4(:,3), wf16(:,13), A(:,77), n3(:,192), t3x64(:,:,77), nhel, den(165))
    call cont_VV(nsync, wf4(:,3), wf16(:,14), A(:,78), n3(:,193), t3x64(:,:,78), nhel, den(166))
    call cont_VV(nsync, wf4(:,4), wf16(:,13), A(:,79), n3(:,194), t3x64(:,:,79), nhel, den(167))
    call cont_VV(nsync, wf4(:,4), wf16(:,14), A(:,80), n3(:,195), t3x64(:,:,80), nhel, den(168))
    call cont_QA(nsync, wf8(:,20), wf8(:,87), A(:,81), n3(:,196), t3x64(:,:,81), nhel, den(169))
    call cont_VV(nsync, wf4(:,14), wf16(:,15), A(:,82), n3(:,197), t3x64(:,:,82), nhel, den(170))
    call cont_VV(nsync, wf4(:,10), wf16(:,16), A(:,83), n3(:,198), t3x64(:,:,83), nhel, den(171))
    call cont_VV(nsync, wf4(:,10), wf16(:,17), A(:,84), n3(:,199), t3x64(:,:,84), nhel, den(172))
    call cont_QA(nsync, wf8(:,20), wf8(:,88), A(:,85), n3(:,200), t3x64(:,:,85), nhel, den(173))
    call cont_VV(nsync, wf4(:,6), wf16(:,18), A(:,86), n3(:,201), t3x64(:,:,86), nhel, den(174))
    call cont_VV(nsync, wf4(:,6), wf16(:,19), A(:,87), n3(:,202), t3x64(:,:,87), nhel, den(175))
    call cont_VV(nsync, wf4(:,12), wf16(:,20), A(:,88), n3(:,203), t3x64(:,:,88), nhel, den(176))
    call cont_QA(nsync, wf8(:,20), wf8(:,89), A(:,89), n3(:,204), t3x64(:,:,89), nhel, den(177))
    call cont_QA(nsync, wf8(:,20), wf8(:,90), A(:,90), n3(:,205), t3x64(:,:,90), nhel, den(178))
    call cont_QA(nsync, wf8(:,26), wf8(:,91), A(:,91), n3(:,206), t3x64(:,:,91), nhel, den(179))
    call cont_VV(nsync, wf4(:,13), wf16(:,20), A(:,92), n3(:,207), t3x64(:,:,92), nhel, den(180))
    call cont_QA(nsync, wf8(:,66), wf8(:,92), A(:,93), n3(:,208), t3x64(:,:,93), nhel, den(181))
    call cont_QA(nsync, wf8(:,66), wf8(:,93), A(:,94), n3(:,209), t3x64(:,:,94), nhel, den(182))
    call cont_QA(nsync, wf8(:,26), wf8(:,94), A(:,95), n3(:,210), t3x64(:,:,95), nhel, den(183))
    call cont_VV(nsync, wf4(:,7), wf16(:,19), A(:,96), n3(:,211), t3x64(:,:,96), nhel, den(184))

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
  complex(REALKIND), intent(out) :: M1(6) ! M1helarray(6,64)
  integer :: empty(0)

  M1(1) = ((A(j,25)%j+A(j,29)%j+A(j,32)%j+A(j,46)%j+A(j,48)%j+A(j,61)%j+A(j,63)%j)*f(1))/4._/**/REALKIND+((A(j,1)%j+A(j,2)%j &
       +A(j,4)%j+A(j,5)%j+A(j,7)%j+A(j,8)%j+A(j,77)%j+A(j,78)%j+A(j,79)%j+A(j,80)%j)*f(1))/12._/**/REALKIND+(CI*(-A(j,27)%j &
       -A(j,30)%j)*f(2))/4._/**/REALKIND+((A(j,34)%j+A(j,38)%j+A(j,40)%j+A(j,41)%j+A(j,44)%j+A(j,51)%j+A(j,52)%j+A(j,55)%j &
       +A(j,58)%j+A(j,60)%j+A(j,83)%j+A(j,88)%j+A(j,92)%j+A(j,94)%j)*f(3))/4._/**/REALKIND+((A(j,17)%j+A(j,18)%j+A(j,19)%j &
       +A(j,20)%j+A(j,66)%j+A(j,67)%j+A(j,68)%j+A(j,70)%j+A(j,71)%j+A(j,72)%j+A(j,73)%j+A(j,74)%j+A(j,75)%j+A(j,76)%j+A(j,86)%j &
       +A(j,87)%j+A(j,93)%j+A(j,96)%j)*f(3))/12._/**/REALKIND+(CI*(A(j,33)%j+A(j,37)%j-A(j,50)%j-A(j,53)%j)*f(4))/4._/**/REALKIND
  M1(2) = ((-A(j,25)%j-A(j,26)%j-A(j,28)%j-A(j,29)%j-A(j,31)%j-A(j,32)%j-A(j,45)%j-A(j,46)%j-A(j,47)%j &
       -A(j,48)%j)*f(1))/12._/**/REALKIND+((-A(j,1)%j-A(j,5)%j-A(j,8)%j-A(j,21)%j-A(j,23)%j-A(j,78)%j &
       -A(j,80)%j)*f(1))/4._/**/REALKIND+(CI*(-A(j,3)%j-A(j,6)%j)*f(2))/4._/**/REALKIND+((-A(j,34)%j-A(j,35)%j-A(j,36)%j-A(j,38)%j &
       -A(j,39)%j-A(j,40)%j-A(j,41)%j-A(j,42)%j-A(j,43)%j-A(j,44)%j-A(j,57)%j-A(j,58)%j-A(j,59)%j-A(j,60)%j-A(j,85)%j-A(j,88)%j &
       -A(j,90)%j-A(j,92)%j)*f(3))/12._/**/REALKIND+((-A(j,11)%j-A(j,12)%j-A(j,15)%j-A(j,18)%j-A(j,20)%j-A(j,66)%j-A(j,70)%j &
       -A(j,72)%j-A(j,73)%j-A(j,76)%j-A(j,81)%j-A(j,87)%j-A(j,89)%j-A(j,96)%j)*f(3))/4._/**/REALKIND+(CI*(A(j,10)%j+A(j,13)%j &
       -A(j,65)%j-A(j,69)%j)*f(4))/4._/**/REALKIND
  M1(3) = ((-A(j,25)%j-A(j,26)%j-A(j,28)%j-A(j,29)%j-A(j,31)%j-A(j,32)%j-A(j,61)%j-A(j,62)%j-A(j,63)%j &
       -A(j,64)%j)*f(1))/12._/**/REALKIND+((-A(j,2)%j-A(j,4)%j-A(j,7)%j-A(j,22)%j-A(j,24)%j-A(j,77)%j &
       -A(j,79)%j)*f(1))/4._/**/REALKIND+(CI*(A(j,3)%j+A(j,6)%j)*f(2))/4._/**/REALKIND+((-A(j,9)%j-A(j,14)%j-A(j,16)%j-A(j,17)%j &
       -A(j,19)%j-A(j,67)%j-A(j,68)%j-A(j,71)%j-A(j,74)%j-A(j,75)%j-A(j,82)%j-A(j,86)%j-A(j,91)%j-A(j,93)%j)*f(3))/4._/**/REALKIND &
       +((-A(j,41)%j-A(j,42)%j-A(j,43)%j-A(j,44)%j-A(j,49)%j-A(j,51)%j-A(j,52)%j-A(j,54)%j-A(j,55)%j-A(j,56)%j-A(j,57)%j-A(j,58)%j &
       -A(j,59)%j-A(j,60)%j-A(j,83)%j-A(j,84)%j-A(j,94)%j-A(j,95)%j)*f(3))/12._/**/REALKIND+(CI*(-A(j,10)%j-A(j,13)%j+A(j,65)%j &
       +A(j,69)%j)*f(4))/4._/**/REALKIND
  M1(4) = ((A(j,1)%j+A(j,2)%j+A(j,4)%j+A(j,5)%j+A(j,7)%j+A(j,8)%j+A(j,21)%j+A(j,22)%j+A(j,23)%j+A(j,24)%j)*f(1))/12._/**/REALKIND &
       +((A(j,26)%j+A(j,28)%j+A(j,31)%j+A(j,45)%j+A(j,47)%j+A(j,62)%j+A(j,64)%j)*f(1))/4._/**/REALKIND+(CI*(A(j,27)%j &
       +A(j,30)%j)*f(2))/4._/**/REALKIND+((A(j,9)%j+A(j,11)%j+A(j,12)%j+A(j,14)%j+A(j,15)%j+A(j,16)%j+A(j,17)%j+A(j,18)%j &
       +A(j,19)%j+A(j,20)%j+A(j,73)%j+A(j,74)%j+A(j,75)%j+A(j,76)%j+A(j,81)%j+A(j,82)%j+A(j,89)%j &
       +A(j,91)%j)*f(3))/12._/**/REALKIND+((A(j,35)%j+A(j,36)%j+A(j,39)%j+A(j,42)%j+A(j,43)%j+A(j,49)%j+A(j,54)%j+A(j,56)%j &
       +A(j,57)%j+A(j,59)%j+A(j,84)%j+A(j,85)%j+A(j,90)%j+A(j,95)%j)*f(3))/4._/**/REALKIND+(CI*(-A(j,33)%j-A(j,37)%j+A(j,50)%j &
       +A(j,53)%j)*f(4))/4._/**/REALKIND
  M1(5) = ((A(j,25)%j+A(j,26)%j+A(j,28)%j+A(j,29)%j+A(j,31)%j+A(j,32)%j+A(j,45)%j+A(j,46)%j+A(j,47)%j+A(j,48)%j+A(j,61)%j &
       +A(j,62)%j+A(j,63)%j+A(j,64)%j)*f(1))/36._/**/REALKIND+((A(j,21)%j+A(j,22)%j+A(j,23)%j+A(j,24)%j+A(j,77)%j+A(j,78)%j &
       +A(j,79)%j+A(j,80)%j)*f(1))/12._/**/REALKIND+((A(j,34)%j+A(j,35)%j+A(j,36)%j+A(j,38)%j+A(j,39)%j+A(j,40)%j+A(j,41)%j &
       +A(j,42)%j+A(j,43)%j+A(j,44)%j+A(j,49)%j+A(j,51)%j+A(j,52)%j+A(j,54)%j+A(j,55)%j+A(j,56)%j+A(j,57)%j+A(j,58)%j+A(j,59)%j &
       +A(j,60)%j+A(j,83)%j+A(j,84)%j+A(j,85)%j+A(j,88)%j+A(j,90)%j+A(j,92)%j+A(j,94)%j+A(j,95)%j)*f(3))/36._/**/REALKIND &
       +((A(j,9)%j+A(j,11)%j+A(j,12)%j+A(j,14)%j+A(j,15)%j+A(j,16)%j+A(j,66)%j+A(j,67)%j+A(j,68)%j+A(j,70)%j+A(j,71)%j+A(j,72)%j &
       +A(j,81)%j+A(j,82)%j+A(j,86)%j+A(j,87)%j+A(j,89)%j+A(j,91)%j+A(j,93)%j+A(j,96)%j)*f(3))/12._/**/REALKIND
  M1(6) = ((-A(j,45)%j-A(j,46)%j-A(j,47)%j-A(j,48)%j-A(j,61)%j-A(j,62)%j-A(j,63)%j-A(j,64)%j)*f(1))/12._/**/REALKIND+((-A(j,1)%j &
       -A(j,2)%j-A(j,4)%j-A(j,5)%j-A(j,7)%j-A(j,8)%j-A(j,21)%j-A(j,22)%j-A(j,23)%j-A(j,24)%j-A(j,77)%j-A(j,78)%j-A(j,79)%j &
       -A(j,80)%j)*f(1))/36._/**/REALKIND+((-A(j,34)%j-A(j,35)%j-A(j,36)%j-A(j,38)%j-A(j,39)%j-A(j,40)%j-A(j,49)%j-A(j,51)%j &
       -A(j,52)%j-A(j,54)%j-A(j,55)%j-A(j,56)%j-A(j,83)%j-A(j,84)%j-A(j,85)%j-A(j,88)%j-A(j,90)%j-A(j,92)%j-A(j,94)%j &
       -A(j,95)%j)*f(3))/12._/**/REALKIND+((-A(j,9)%j-A(j,11)%j-A(j,12)%j-A(j,14)%j-A(j,15)%j-A(j,16)%j-A(j,17)%j-A(j,18)%j &
       -A(j,19)%j-A(j,20)%j-A(j,66)%j-A(j,67)%j-A(j,68)%j-A(j,70)%j-A(j,71)%j-A(j,72)%j-A(j,73)%j-A(j,74)%j-A(j,75)%j-A(j,76)%j &
       -A(j,81)%j-A(j,82)%j-A(j,86)%j-A(j,87)%j-A(j,89)%j-A(j,91)%j-A(j,93)%j-A(j,96)%j)*f(3))/36._/**/REALKIND

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
  use ol_colourmatrix_pphbbj_cccxcxbbxh_1_/**/REALKIND, only: K1
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
  use ol_colourmatrix_pphbbj_cccxcxbbxh_1_/**/REALKIND, only: K1
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
  use ol_colourmatrix_pphbbj_cccxcxbbxh_1_/**/REALKIND, only: K1
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
  & bind(c,name="ol_tree_colvect_pphbbj_cccxcxbbxh_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(6,64)
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
    & bind(c,name="ol_f_amp2tree_pphbbj_cccxcxbbxh_1")
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
    & bind(c,name="ol_f_amp2ccone_pphbbj_cccxcxbbxh_1")
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
    & bind(c,name="ol_f_amp2ccall_pphbbj_cccxcxbbxh_1")
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
    & bind(c,name="ol_f_amp2hcone_pphbbj_cccxcxbbxh_1")
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
    & bind(c,name="ol_f_amp2hcall_pphbbj_cccxcxbbxh_1")
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
    & bind(c,name="ol_amp2tree_pphbbj_cccxcxbbxh_1")
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
    & bind(c,name="ol_amp2ccone_pphbbj_cccxcxbbxh_1")
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
    & bind(c,name="ol_amp2ccall_pphbbj_cccxcxbbxh_1")
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
    & bind(c,name="ol_amp2hcone_pphbbj_cccxcxbbxh_1")
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
    & bind(c,name="ol_amp2hcall_pphbbj_cccxcxbbxh_1")
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
    & bind(c,name="amp2tree_pphbbj_cccxcxbbxh_1_")
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
    & bind(c,name="amp2ccone_pphbbj_cccxcxbbxh_1_")
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
    & bind(c,name="amp2ccall_pphbbj_cccxcxbbxh_1_")
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
    & bind(c,name="amp2hcone_pphbbj_cccxcxbbxh_1_")
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
    & bind(c,name="amp2hcall_pphbbj_cccxcxbbxh_1_")
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

end module ol_tree_pphbbj_cccxcxbbxh_1_/**/REALKIND
