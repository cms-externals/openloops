
module ol_colourmatrix_ppajjj_bbbbxbxbxa_1_/**/REALKIND
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
  K1( 25,:) = [   0,   0,   0,   4,   4,  12]
  K1( 26,:) = [   0,   0,   4,   0,  12,   4]
  K1( 27,:) = [   0,   4,   0,  12,   0,   4]
  K1( 28,:) = [   4,   0,  12,   0,   4,   0]
  K1( 29,:) = [   4,  12,   0,   4,   0,   0]
  K1( 30,:) = [  12,   4,   4,   0,   0,   0]
  K1( 31,:) = [   0,  12,   0,   4,   4,   0]
  K1( 32,:) = [  12,   0,   4,   0,   0,   4]
  K1( 33,:) = [   0,   4,   0,   0,  12,   4]
  K1( 34,:) = [   4,   0,   0,   0,   4,  12]
  K1( 35,:) = [   4,   0,  12,   4,   0,   0]
  K1( 36,:) = [   0,   4,   4,  12,   0,   0]
  K1( 37,:) = [  36,  12,  12,   4,   4,  12]
  K1( 38,:) = [  12,  36,   4,  12,  12,   4]
  K1( 39,:) = [  12,   4,  36,  12,  12,   4]
  K1( 40,:) = [   4,  12,  12,  36,   4,  12]
  K1( 41,:) = [   4,  12,  12,   4,  36,  12]
  K1( 42,:) = [  12,   4,   4,  12,  12,  36]
  K1( 43,:) = [   0,   0,   0,  -4,  -4, -12]
  K1( 44,:) = [   0,   0,  -4, -12,   0,  -4]
  K1( 45,:) = [   0,  -4,   0, -12,   0,  -4]
  K1( 46,:) = [  -4, -12, -12, -36,  -4, -12]
  K1( 47,:) = [  -4,   0,   0,  -4,   0, -12]
  K1( 48,:) = [ -12,  -4,  -4, -12, -12, -36]
  K1( 49,:) = [   0, -12,   0,  -4,  -4,   0]
  K1( 50,:) = [ -12, -36,  -4, -12, -12,  -4]
  K1( 51,:) = [   0,  -4,   0,   0, -12,  -4]
  K1( 52,:) = [  -4, -12,   0,   0,  -4,   0]
  K1( 53,:) = [  -4, -12, -12,  -4, -36, -12]
  K1( 54,:) = [   0,  -4,  -4,   0, -12,   0]
  K1( 55,:) = [ -36, -12, -12,  -4,  -4, -12]
  K1( 56,:) = [ -12,   0,  -4,   0,   0,  -4]
  K1( 57,:) = [ -12,  -4, -36, -12, -12,  -4]
  K1( 58,:) = [  -4,   0, -12,   0,  -4,   0]
  K1( 59,:) = [  -4,   0, -12,  -4,   0,   0]
  K1( 60,:) = [ -12,  -4,  -4,   0,   0,   0]
  K1( 61,:) = [  36,  12,  12,   4,   4,  12]
  K1( 62,:) = [  12,  36,   4,  12,  12,   4]
  K1( 63,:) = [  12,   4,  36,  12,  12,   4]
  K1( 64,:) = [   4,  12,  12,  36,   4,  12]
  K1( 65,:) = [   4,  12,  12,   4,  36,  12]
  K1( 66,:) = [  12,   4,   4,  12,  12,  36]
  K1( 67,:) = [   0,   0, -12,  -4,  -4,   0]
  K1( 68,:) = [   0,   0,  -4,   0, -12,  -4]
  K1( 69,:) = [ -12,  -4, -36, -12, -12,  -4]
  K1( 70,:) = [  -4,   0, -12,   0,  -4,   0]
  K1( 71,:) = [  -4, -12, -12,  -4, -36, -12]
  K1( 72,:) = [   0,  -4,  -4,   0, -12,   0]
  K1( 73,:) = [ -36, -12, -12,  -4,  -4, -12]
  K1( 74,:) = [ -12,   0,  -4,   0,   0,  -4]
  K1( 75,:) = [ -12,  -4,   0,   0,   0,  -4]
  K1( 76,:) = [  -4,   0,   0,   0,  -4, -12]
  K1( 77,:) = [  -4,   0,   0,  -4,   0, -12]
  K1( 78,:) = [ -12,  -4,  -4, -12, -12, -36]
  K1( 79,:) = [   0, -12,   0,  -4,  -4,   0]
  K1( 80,:) = [ -12, -36,  -4, -12, -12,  -4]
  K1( 81,:) = [   0,  -4,   0, -12,   0,  -4]
  K1( 82,:) = [  -4, -12, -12, -36,  -4, -12]
  K1( 83,:) = [  -4, -12,   0,  -4,   0,   0]
  K1( 84,:) = [   0,  -4,  -4, -12,   0,   0]
  K1( 85,:) = [   0,  12,   0,   4,   4,   0]
  K1( 86,:) = [  12,   0,   4,   0,   0,   4]
  K1( 87,:) = [   0,   4,   0,  12,   0,   4]
  K1( 88,:) = [   4,   0,  12,   0,   4,   0]
  K1( 89,:) = [   4,   0,   0,   4,   0,  12]
  K1( 90,:) = [   0,   4,   4,   0,  12,   0]
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
  K1(109,:) = [   0,   0,   0,  -4,  -4, -12]
  K1(110,:) = [   0,   0,  -4,   0, -12,  -4]
  K1(111,:) = [   0,  -4,   0,   0, -12,  -4]
  K1(112,:) = [  -4,   0,   0,   0,  -4, -12]
  K1(113,:) = [  -4, -12, -12,  -4, -36, -12]
  K1(114,:) = [ -12,  -4,  -4, -12, -12, -36]
  K1(115,:) = [   0,   0,   0,   4,   4,  12]
  K1(116,:) = [   0,   0,   4,  12,   0,   4]
  K1(117,:) = [   0,   4,   0,   0,  12,   4]
  K1(118,:) = [   4,  12,   0,   0,   4,   0]
  K1(119,:) = [   4,   0,  12,   4,   0,   0]
  K1(120,:) = [  12,   4,   4,   0,   0,   0]
  K1(121,:) = [   0,   0,  12,   4,   4,   0]
  K1(122,:) = [   0,   0,   4,   0,  12,   4]
  K1(123,:) = [  12,   4,   0,   0,   0,   4]
  K1(124,:) = [   4,   0,   0,   0,   4,  12]
  K1(125,:) = [   4,  12,   0,   4,   0,   0]
  K1(126,:) = [   0,   4,   4,  12,   0,   0]
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
end module ol_colourmatrix_ppajjj_bbbbxbxbxa_1_/**/REALKIND



module ol_forced_parameters_ppajjj_bbbbxbxbxa_1_/**/REALKIND
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
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppajjj_bbbbxbxbxa_1_/**/REALKIND

module ol_tree_ppajjj_bbbbxbxbxa_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(2)
  complex(REALKIND), save :: den(438)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 128 ! number of helicity configurations
  integer(intkind2), save :: nhel = 128 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(128) ! physical helicity states
  complex(DREALKIND) :: M1helarr(6,128) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = (CI*eQED*gQCD**4)/3._/**/REALKIND
    f(2) = (eQED*gQCD**4)/3._/**/REALKIND

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,9))
  den(2) = 1 / (Q(5,18))
  den(3) = 1 / (Q(5,68) - MB2)
  den(5) = 1 / (Q(5,41) - MB2)
  den(9) = 1 / (Q(5,50) - MB2)
  den(13) = 1 / (Q(5,100))
  den(16) = 1 / (Q(5,96) - MB2)
  den(18) = 1 / (Q(5,13) - MB2)
  den(22) = 1 / (Q(5,22) - MB2)
  den(29) = 1 / (Q(5,66) - MB2)
  den(30) = 1 / (Q(5,20))
  den(34) = 1 / (Q(5,98))
  den(38) = 1 / (Q(5,52) - MB2)
  den(42) = 1 / (Q(5,11) - MB2)
  den(51) = 1 / (Q(5,34))
  den(53) = 1 / (Q(5,25) - MB2)
  den(59) = 1 / (Q(5,84))
  den(62) = 1 / (Q(5,80) - MB2)
  den(66) = 1 / (Q(5,38) - MB2)
  den(73) = 1 / (Q(5,36))
  den(77) = 1 / (Q(5,82))
  den(106) = 1 / (Q(5,17))
  den(107) = 1 / (Q(5,10))
  den(109) = 1 / (Q(5,49) - MB2)
  den(113) = 1 / (Q(5,42) - MB2)
  den(119) = 1 / (Q(5,21) - MB2)
  den(123) = 1 / (Q(5,14) - MB2)
  den(129) = 1 / (Q(5,65) - MB2)
  den(131) = 1 / (Q(5,97))
  den(146) = 1 / (Q(5,33))
  den(150) = 1 / (Q(5,26) - MB2)
  den(156) = 1 / (Q(5,37) - MB2)
  den(165) = 1 / (Q(5,81))
  den(194) = 1 / (Q(5,12))
  den(200) = 1 / (Q(5,44) - MB2)
  den(204) = 1 / (Q(5,19) - MB2)
  den(229) = 1 / (Q(5,28) - MB2)
  den(233) = 1 / (Q(5,35) - MB2)
  den(272) = 1 / (Q(5,76))
  den(275) = 1 / (Q(5,72) - MB2)
  den(286) = 1 / (Q(5,74))
  den(328) = 1 / (Q(5,73))

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
  den(32) = den(6)*den(31)
  den(33) = den(1)*den(30)
  den(35) = den(29)*den(34)
  den(36) = den(33)*den(35)
  den(37) = den(1)*den(29)
  den(39) = den(30)*den(38)
  den(40) = den(37)*den(39)
  den(41) = den(16)*den(30)
  den(43) = den(1)*den(42)
  den(44) = den(41)*den(43)
  den(45) = den(22)*den(30)
  den(46) = den(21)*den(45)
  den(47) = den(16)*den(34)
  den(48) = den(33)*den(47)
  den(49) = den(39)*den(43)
  den(50) = den(6)*den(45)
  den(52) = den(3)*den(51)
  den(54) = den(1)*den(53)
  den(55) = den(52)*den(54)
  den(56) = den(9)*den(51)
  den(57) = den(8)*den(56)
  den(58) = den(1)*den(51)
  den(60) = den(3)*den(59)
  den(61) = den(58)*den(60)
  den(63) = den(51)*den(62)
  den(64) = den(19)*den(63)
  den(65) = den(1)*den(62)
  den(67) = den(51)*den(66)
  den(68) = den(65)*den(67)
  den(69) = den(59)*den(62)
  den(70) = den(58)*den(69)
  den(71) = den(19)*den(56)
  den(72) = den(54)*den(67)
  den(74) = den(29)*den(73)
  den(75) = den(54)*den(74)
  den(76) = den(1)*den(73)
  den(78) = den(29)*den(77)
  den(79) = den(76)*den(78)
  den(80) = den(38)*den(73)
  den(81) = den(37)*den(80)
  den(82) = den(62)*den(73)
  den(83) = den(43)*den(82)
  den(84) = den(66)*den(73)
  den(85) = den(65)*den(84)
  den(86) = den(62)*den(77)
  den(87) = den(76)*den(86)
  den(88) = den(43)*den(80)
  den(89) = den(54)*den(84)
  den(90) = den(19)*den(78)
  den(91) = den(19)*den(35)
  den(92) = den(35)*den(54)
  den(93) = den(6)*den(78)
  den(94) = den(14)*den(43)
  den(95) = den(43)*den(60)
  den(96) = den(14)*den(54)
  den(97) = den(6)*den(60)
  den(98) = den(43)*den(69)
  den(99) = den(19)*den(86)
  den(100) = den(6)*den(86)
  den(101) = den(6)*den(69)
  den(102) = den(25)*den(43)
  den(103) = den(19)*den(47)
  den(104) = den(25)*den(54)
  den(105) = den(47)*den(54)
  den(108) = den(3)*den(107)
  den(110) = den(106)*den(109)
  den(111) = den(108)*den(110)
  den(112) = den(3)*den(106)
  den(114) = den(107)*den(113)
  den(115) = den(112)*den(114)
  den(116) = den(106)*den(107)
  den(117) = den(14)*den(116)
  den(118) = den(16)*den(107)
  den(120) = den(106)*den(119)
  den(121) = den(118)*den(120)
  den(122) = den(16)*den(106)
  den(124) = den(107)*den(123)
  den(125) = den(122)*den(124)
  den(126) = den(25)*den(116)
  den(127) = den(114)*den(120)
  den(128) = den(110)*den(124)
  den(130) = den(30)*den(107)
  den(132) = den(129)*den(131)
  den(133) = den(130)*den(132)
  den(134) = den(30)*den(129)
  den(135) = den(114)*den(134)
  den(136) = den(107)*den(129)
  den(137) = den(39)*den(136)
  den(138) = den(42)*den(107)
  den(139) = den(41)*den(138)
  den(140) = den(30)*den(119)
  den(141) = den(118)*den(140)
  den(142) = den(16)*den(131)
  den(143) = den(130)*den(142)
  den(144) = den(39)*den(138)
  den(145) = den(114)*den(140)
  den(147) = den(109)*den(146)
  den(148) = den(108)*den(147)
  den(149) = den(3)*den(146)
  den(151) = den(107)*den(150)
  den(152) = den(149)*den(151)
  den(153) = den(107)*den(146)
  den(154) = den(60)*den(153)
  den(155) = den(62)*den(107)
  den(157) = den(146)*den(156)
  den(158) = den(155)*den(157)
  den(159) = den(62)*den(146)
  den(160) = den(124)*den(159)
  den(161) = den(69)*den(153)
  den(162) = den(151)*den(157)
  den(163) = den(124)*den(147)
  den(164) = den(73)*den(107)
  den(166) = den(129)*den(165)
  den(167) = den(164)*den(166)
  den(168) = den(73)*den(129)
  den(169) = den(151)*den(168)
  den(170) = den(80)*den(136)
  den(171) = den(82)*den(138)
  den(172) = den(73)*den(156)
  den(173) = den(155)*den(172)
  den(174) = den(62)*den(165)
  den(175) = den(164)*den(174)
  den(176) = den(80)*den(138)
  den(177) = den(151)*den(172)
  den(178) = den(124)*den(166)
  den(179) = den(114)*den(166)
  den(180) = den(124)*den(132)
  den(181) = den(132)*den(151)
  den(182) = den(14)*den(138)
  den(183) = den(60)*den(138)
  den(184) = den(14)*den(151)
  den(185) = den(60)*den(114)
  den(186) = den(69)*den(138)
  den(187) = den(114)*den(174)
  den(188) = den(69)*den(114)
  den(189) = den(124)*den(174)
  den(190) = den(25)*den(138)
  den(191) = den(25)*den(151)
  den(192) = den(142)*den(151)
  den(193) = den(124)*den(142)
  den(195) = den(29)*den(194)
  den(196) = den(110)*den(195)
  den(197) = den(106)*den(194)
  den(198) = den(35)*den(197)
  den(199) = den(29)*den(106)
  den(201) = den(194)*den(200)
  den(202) = den(199)*den(201)
  den(203) = den(16)*den(194)
  den(205) = den(106)*den(204)
  den(206) = den(203)*den(205)
  den(207) = den(123)*den(194)
  den(208) = den(122)*den(207)
  den(209) = den(47)*den(197)
  den(210) = den(201)*den(205)
  den(211) = den(110)*den(207)
  den(212) = den(2)*den(194)
  den(213) = den(132)*den(212)
  den(214) = den(129)*den(194)
  den(215) = den(10)*den(214)
  den(216) = den(2)*den(129)
  den(217) = den(201)*den(216)
  den(218) = den(2)*den(204)
  den(219) = den(203)*den(218)
  den(220) = den(18)*den(194)
  den(221) = den(17)*den(220)
  den(222) = den(142)*den(212)
  den(223) = den(201)*den(218)
  den(224) = den(10)*den(220)
  den(225) = den(147)*den(195)
  den(226) = den(146)*den(194)
  den(227) = den(78)*den(226)
  den(228) = den(29)*den(146)
  den(230) = den(194)*den(229)
  den(231) = den(228)*den(230)
  den(232) = den(62)*den(194)
  den(234) = den(146)*den(233)
  den(235) = den(232)*den(234)
  den(236) = den(159)*den(207)
  den(237) = den(86)*den(226)
  den(238) = den(230)*den(234)
  den(239) = den(147)*den(207)
  den(240) = den(51)*den(194)
  den(241) = den(166)*den(240)
  den(242) = den(56)*den(214)
  den(243) = den(51)*den(129)
  den(244) = den(230)*den(243)
  den(245) = den(51)*den(233)
  den(246) = den(232)*den(245)
  den(247) = den(63)*den(220)
  den(248) = den(174)*den(240)
  den(249) = den(230)*den(245)
  den(250) = den(56)*den(220)
  den(251) = den(166)*den(207)
  den(252) = den(166)*den(201)
  den(253) = den(132)*den(207)
  den(254) = den(132)*den(230)
  den(255) = den(35)*den(220)
  den(256) = den(78)*den(220)
  den(257) = den(78)*den(201)
  den(258) = den(35)*den(230)
  den(259) = den(174)*den(201)
  den(260) = den(86)*den(201)
  den(261) = den(86)*den(220)
  den(262) = den(174)*den(207)
  den(263) = den(47)*den(230)
  den(264) = den(142)*den(230)
  den(265) = den(47)*den(220)
  den(266) = den(142)*den(207)
  den(267) = den(53)*den(106)
  den(268) = den(52)*den(267)
  den(269) = den(51)*den(113)
  den(270) = den(112)*den(269)
  den(271) = den(51)*den(106)
  den(273) = den(3)*den(272)
  den(274) = den(271)*den(273)
  den(276) = den(51)*den(275)
  den(277) = den(120)*den(276)
  den(278) = den(106)*den(275)
  den(279) = den(67)*den(278)
  den(280) = den(272)*den(275)
  den(281) = den(271)*den(280)
  den(282) = den(120)*den(269)
  den(283) = den(67)*den(267)
  den(284) = den(74)*den(267)
  den(285) = den(73)*den(106)
  den(287) = den(29)*den(286)
  den(288) = den(285)*den(287)
  den(289) = den(73)*den(200)
  den(290) = den(199)*den(289)
  den(291) = den(73)*den(275)
  den(292) = den(205)*den(291)
  den(293) = den(84)*den(278)
  den(294) = den(275)*den(286)
  den(295) = den(285)*den(294)
  den(296) = den(205)*den(289)
  den(297) = den(84)*den(267)
  den(298) = den(120)*den(287)
  den(299) = den(35)*den(120)
  den(300) = den(35)*den(267)
  den(301) = den(110)*den(287)
  den(302) = den(14)*den(205)
  den(303) = den(205)*den(273)
  den(304) = den(14)*den(267)
  den(305) = den(110)*den(273)
  den(306) = den(205)*den(280)
  den(307) = den(120)*den(294)
  den(308) = den(110)*den(294)
  den(309) = den(110)*den(280)
  den(310) = den(25)*den(205)
  den(311) = den(47)*den(120)
  den(312) = den(25)*den(267)
  den(313) = den(47)*den(267)
  den(314) = den(5)*den(146)
  den(315) = den(4)*den(314)
  den(316) = den(2)*den(150)
  den(317) = den(149)*den(316)
  den(318) = den(2)*den(146)
  den(319) = den(273)*den(318)
  den(320) = den(2)*den(275)
  den(321) = den(157)*den(320)
  den(322) = den(146)*den(275)
  den(323) = den(23)*den(322)
  den(324) = den(280)*den(318)
  den(325) = den(157)*den(316)
  den(326) = den(23)*den(314)
  den(327) = den(2)*den(73)
  den(329) = den(129)*den(328)
  den(330) = den(327)*den(329)
  den(331) = den(168)*den(316)
  den(332) = den(216)*den(289)
  den(333) = den(218)*den(291)
  den(334) = den(172)*den(320)
  den(335) = den(275)*den(328)
  den(336) = den(327)*den(335)
  den(337) = den(218)*den(289)
  den(338) = den(172)*den(316)
  den(339) = den(23)*den(329)
  den(340) = den(10)*den(329)
  den(341) = den(23)*den(132)
  den(342) = den(132)*den(316)
  den(343) = den(14)*den(218)
  den(344) = den(218)*den(273)
  den(345) = den(14)*den(316)
  den(346) = den(10)*den(273)
  den(347) = den(218)*den(280)
  den(348) = den(10)*den(335)
  den(349) = den(10)*den(280)
  den(350) = den(23)*den(335)
  den(351) = den(25)*den(218)
  den(352) = den(25)*den(316)
  den(353) = den(142)*den(316)
  den(354) = den(23)*den(142)
  den(355) = den(31)*den(314)
  den(356) = den(30)*den(146)
  den(357) = den(287)*den(356)
  den(358) = den(30)*den(229)
  den(359) = den(228)*den(358)
  den(360) = den(30)*den(275)
  den(361) = den(234)*den(360)
  den(362) = den(45)*den(322)
  den(363) = den(294)*den(356)
  den(364) = den(234)*den(358)
  den(365) = den(45)*den(314)
  den(366) = den(30)*den(51)
  den(367) = den(329)*den(366)
  den(368) = den(134)*den(269)
  den(369) = den(243)*den(358)
  den(370) = den(245)*den(360)
  den(371) = den(140)*den(276)
  den(372) = den(335)*den(366)
  den(373) = den(245)*den(358)
  den(374) = den(140)*den(269)
  den(375) = den(45)*den(329)
  den(376) = den(39)*den(329)
  den(377) = den(45)*den(132)
  den(378) = den(132)*den(358)
  den(379) = den(35)*den(140)
  den(380) = den(140)*den(287)
  den(381) = den(39)*den(287)
  den(382) = den(35)*den(358)
  den(383) = den(39)*den(335)
  den(384) = den(39)*den(294)
  den(385) = den(140)*den(294)
  den(386) = den(45)*den(335)
  den(387) = den(47)*den(358)
  den(388) = den(142)*den(358)
  den(389) = den(47)*den(140)
  den(390) = den(45)*den(142)
  den(391) = den(157)*den(287)
  den(392) = den(78)*den(157)
  den(393) = den(78)*den(314)
  den(394) = den(147)*den(287)
  den(395) = den(60)*den(234)
  den(396) = den(234)*den(273)
  den(397) = den(60)*den(314)
  den(398) = den(147)*den(273)
  den(399) = den(234)*den(280)
  den(400) = den(157)*den(294)
  den(401) = den(147)*den(294)
  den(402) = den(147)*den(280)
  den(403) = den(69)*den(234)
  den(404) = den(86)*den(157)
  den(405) = den(69)*den(314)
  den(406) = den(86)*den(314)
  den(407) = den(67)*den(329)
  den(408) = den(56)*den(329)
  den(409) = den(67)*den(166)
  den(410) = den(166)*den(269)
  den(411) = den(60)*den(245)
  den(412) = den(245)*den(273)
  den(413) = den(60)*den(269)
  den(414) = den(56)*den(273)
  den(415) = den(245)*den(280)
  den(416) = den(56)*den(335)
  den(417) = den(56)*den(280)
  den(418) = den(67)*den(335)
  den(419) = den(69)*den(245)
  den(420) = den(69)*den(269)
  den(421) = den(174)*den(269)
  den(422) = den(67)*den(174)
  den(423) = den(84)*den(329)
  den(424) = den(80)*den(329)
  den(425) = den(84)*den(166)
  den(426) = den(166)*den(289)
  den(427) = den(78)*den(172)
  den(428) = den(172)*den(287)
  den(429) = den(80)*den(287)
  den(430) = den(78)*den(289)
  den(431) = den(80)*den(335)
  den(432) = den(80)*den(294)
  den(433) = den(172)*den(294)
  den(434) = den(84)*den(335)
  den(435) = den(86)*den(289)
  den(436) = den(174)*den(289)
  den(437) = den(86)*den(172)
  den(438) = den(84)*den(174)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_ppajjj_bbbbxbxbxa_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_ppajjj_bbbbxbxbxa_1_/**/DREALKIND, only: &
    & ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for bottom bottom bottom anti-bottom anti-bottom anti-bottom gamma -> 0
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
  use ol_external_ppajjj_bbbbxbxbxa_1, only: &
    & external_perm_ppajjj_bbbbxbxbxa_1, &
    & external_perm_inv_ppajjj_bbbbxbxbxa_1, &
    & extcomb_perm_ppajjj_bbbbxbxbxa_1, &
    & average_factor_ppajjj_bbbbxbxbxa_1
  use ol_external_ppajjj_bbbbxbxbxa_1, only: &
    & H, hel_not_initialised, hel_init, POLSEL
  use ol_colourmatrix_ppajjj_bbbbxbxbxa_1_/**/REALKIND, only: &
    & colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_ppajjj_bbbbxbxbxa_1_/**/REALKIND, only: &
    & check_forced_parameters
  use ol_heltables_ppajjj_bbbbxbxbxa_1
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
  complex(REALKIND) :: MOM_LC(4), M1(6), M1helarray(6,128)
  real(REALKIND)    :: P_scatt_intern(0:3,7)
  complex(REALKIND) :: epLC(1:4), epStd(1:4,2)
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(2), ex6(2), ex7(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,21), wf8(8,90), wf16(16,168), wf128(128,288)

  type(polcont) :: A(128,288)
  complex(REALKIND) :: Aj(288)

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
  extmasses2 = [ rMB2, rMB2, rMB2, rMB2, rMB2, rMB2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, &
    & external_perm_inv_ppajjj_bbbbxbxbxa_1,7)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,7)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_ppajjj_bbbbxbxbxa_1(I)
  else if (I < 0) then
    JBmunu = external_perm_ppajjj_bbbbxbxbxa_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_ppajjj_bbbbxbxbxa_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  42 continue
  ! external WFs
  call pol_wf_Q(P(:,1), rMB, H1, ex1, POLSEL(1))
  call pol_wf_Q(P(:,2), rMB, H2, ex2, POLSEL(2))
  call pol_wf_Q(P(:,3), rMB, H3, ex3, POLSEL(3))
  call pol_wf_A(P(:,4), rMB, H4, ex4, POLSEL(4))
  call pol_wf_A(P(:,5), rMB, H5, ex5, POLSEL(5))
  call pol_wf_A(P(:,6), rMB, H6, ex6, POLSEL(6))
  call pol_wf_V(P(:,7), rZERO, H7, ex7, POLSEL(7))


  if (ntry == 1) then
    shift = 1
    ! call helbookkeeping_flip(H1, 1, shift, eflip, exthel, firstpol)
    ! call helbookkeeping_wf(H1, ex1, shift) ...

    if (any(POLSEL /= 0)) then

      call pol_wf_Q(P(:,1), rMB, H1, ex1, 0)
      call pol_wf_Q(P(:,2), rMB, H2, ex2, 0)
      call pol_wf_Q(P(:,3), rMB, H3, ex3, 0)
      call pol_wf_A(P(:,4), rMB, H4, ex4, 0)
      call pol_wf_A(P(:,5), rMB, H5, ex5, 0)
      call pol_wf_A(P(:,6), rMB, H6, ex6, 0)
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
  call vert_QA_V(ntry, ex1, ex4, wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_QA_V(ntry, ex2, ex5, wf4(:,2), n3(:,2), t3x4(:,:,2))
  call vert_VQ_A(ntry, ex7, ex3, wf4(:,3), n3(:,3), t3x4(:,:,3))
  call prop_Q_A(ntry, wf4(:,3), Q(:,68), MB, 1_intkind1, wf4(:,4), n2(1))
  call vert_AV_Q(ntry, ex6, wf4(:,1), wf8(:,1), n3(:,4), t3x8(:,:,1))
  call vert_VQ_A(ntry, wf4(:,2), wf4(:,4), wf16(:,1), n3(:,5), t3x16(:,:,1))
  call prop_A_Q(ntry, wf8(:,1), Q(:,41), MB, 1_intkind1, wf8(:,2), n2(2))
  call vert_AV_Q(ntry, ex6, wf4(:,2), wf8(:,3), n3(:,6), t3x8(:,:,2))
  call vert_VQ_A(ntry, wf4(:,1), wf4(:,4), wf16(:,2), n3(:,7), t3x16(:,:,2))
  call prop_A_Q(ntry, wf8(:,3), Q(:,50), MB, 1_intkind1, wf8(:,4), n2(3))
  call vert_QA_V(ntry, wf4(:,4), ex6, wf8(:,5), n3(:,8), t3x8(:,:,3))
  call vert_UV_W(ntry, wf4(:,1), Q(:,9), wf4(:,2), Q(:,18), wf16(:,3), n3(:,9), t3x16(:,:,3))
  call vert_AV_Q(ntry, ex6, ex7, wf4(:,5), n3(:,10), t3x4(:,:,4))
  call prop_A_Q(ntry, wf4(:,5), Q(:,96), MB, 1_intkind1, wf4(:,6), n2(4))
  call vert_VQ_A(ntry, wf4(:,1), ex3, wf8(:,6), n3(:,11), t3x8(:,:,4))
  call vert_AV_Q(ntry, wf4(:,6), wf4(:,2), wf16(:,4), n3(:,12), t3x16(:,:,4))
  call prop_Q_A(ntry, wf8(:,6), Q(:,13), MB, 1_intkind1, wf8(:,7), n2(5))
  call vert_VQ_A(ntry, wf4(:,2), ex3, wf8(:,8), n3(:,13), t3x8(:,:,5))
  call vert_AV_Q(ntry, wf4(:,6), wf4(:,1), wf16(:,5), n3(:,14), t3x16(:,:,5))
  call prop_Q_A(ntry, wf8(:,8), Q(:,22), MB, 1_intkind1, wf8(:,9), n2(6))
  call vert_QA_V(ntry, ex3, wf4(:,6), wf8(:,10), n3(:,15), t3x8(:,:,6))
  call vert_VQ_A(ntry, ex7, wf8(:,7), wf16(:,6), n3(:,16), t3x16(:,:,6))
  call vert_VQ_A(ntry, ex7, wf8(:,9), wf16(:,7), n3(:,17), t3x16(:,:,7))
  call vert_VQ_A(ntry, ex7, ex2, wf4(:,7), n3(:,18), t3x4(:,:,5))
  call vert_QA_V(ntry, ex3, ex5, wf4(:,8), n3(:,19), t3x4(:,:,6))
  call prop_Q_A(ntry, wf4(:,7), Q(:,66), MB, 1_intkind1, wf4(:,9), n2(7))
  call vert_VQ_A(ntry, wf4(:,8), wf4(:,9), wf16(:,8), n3(:,20), t3x16(:,:,8))
  call vert_QA_V(ntry, wf4(:,9), ex6, wf8(:,11), n3(:,21), t3x8(:,:,7))
  call vert_UV_W(ntry, wf4(:,1), Q(:,9), wf4(:,8), Q(:,20), wf16(:,9), n3(:,22), t3x16(:,:,9))
  call vert_AV_Q(ntry, ex6, wf4(:,8), wf8(:,12), n3(:,23), t3x8(:,:,8))
  call vert_VQ_A(ntry, wf4(:,1), wf4(:,9), wf16(:,10), n3(:,24), t3x16(:,:,10))
  call prop_A_Q(ntry, wf8(:,12), Q(:,52), MB, 1_intkind1, wf8(:,13), n2(8))
  call vert_VQ_A(ntry, wf4(:,1), ex2, wf8(:,14), n3(:,25), t3x8(:,:,9))
  call vert_AV_Q(ntry, wf4(:,6), wf4(:,8), wf16(:,11), n3(:,26), t3x16(:,:,11))
  call prop_Q_A(ntry, wf8(:,14), Q(:,11), MB, 1_intkind1, wf8(:,15), n2(9))
  call vert_VQ_A(ntry, wf4(:,8), ex2, wf8(:,16), n3(:,27), t3x8(:,:,10))
  call prop_Q_A(ntry, wf8(:,16), Q(:,22), MB, 1_intkind1, wf8(:,17), n2(10))
  call vert_QA_V(ntry, ex2, wf4(:,6), wf8(:,18), n3(:,28), t3x8(:,:,11))
  call vert_VQ_A(ntry, ex7, wf8(:,15), wf16(:,12), n3(:,29), t3x16(:,:,12))
  call vert_VQ_A(ntry, ex7, wf8(:,17), wf16(:,13), n3(:,30), t3x16(:,:,13))
  call vert_QA_V(ntry, ex2, ex6, wf4(:,10), n3(:,31), t3x4(:,:,7))
  call vert_AV_Q(ntry, ex5, wf4(:,1), wf8(:,19), n3(:,32), t3x8(:,:,12))
  call vert_VQ_A(ntry, wf4(:,10), wf4(:,4), wf16(:,14), n3(:,33), t3x16(:,:,14))
  call prop_A_Q(ntry, wf8(:,19), Q(:,25), MB, 1_intkind1, wf8(:,20), n2(11))
  call vert_AV_Q(ntry, ex5, wf4(:,10), wf8(:,21), n3(:,34), t3x8(:,:,13))
  call prop_A_Q(ntry, wf8(:,21), Q(:,50), MB, 1_intkind1, wf8(:,22), n2(12))
  call vert_QA_V(ntry, wf4(:,4), ex5, wf8(:,23), n3(:,35), t3x8(:,:,14))
  call vert_UV_W(ntry, wf4(:,1), Q(:,9), wf4(:,10), Q(:,34), wf16(:,15), n3(:,36), t3x16(:,:,15))
  call vert_AV_Q(ntry, ex5, ex7, wf4(:,11), n3(:,37), t3x4(:,:,8))
  call prop_A_Q(ntry, wf4(:,11), Q(:,80), MB, 1_intkind1, wf4(:,12), n2(13))
  call vert_AV_Q(ntry, wf4(:,12), wf4(:,10), wf16(:,16), n3(:,38), t3x16(:,:,16))
  call vert_VQ_A(ntry, wf4(:,10), ex3, wf8(:,24), n3(:,39), t3x8(:,:,15))
  call vert_AV_Q(ntry, wf4(:,12), wf4(:,1), wf16(:,17), n3(:,40), t3x16(:,:,17))
  call prop_Q_A(ntry, wf8(:,24), Q(:,38), MB, 1_intkind1, wf8(:,25), n2(14))
  call vert_QA_V(ntry, ex3, wf4(:,12), wf8(:,26), n3(:,41), t3x8(:,:,16))
  call vert_VQ_A(ntry, ex7, wf8(:,25), wf16(:,18), n3(:,42), t3x16(:,:,18))
  call vert_QA_V(ntry, ex3, ex6, wf4(:,13), n3(:,43), t3x4(:,:,9))
  call vert_VQ_A(ntry, wf4(:,13), wf4(:,9), wf16(:,19), n3(:,44), t3x16(:,:,19))
  call vert_QA_V(ntry, wf4(:,9), ex5, wf8(:,27), n3(:,45), t3x8(:,:,17))
  call vert_UV_W(ntry, wf4(:,1), Q(:,9), wf4(:,13), Q(:,36), wf16(:,20), n3(:,46), t3x16(:,:,20))
  call vert_AV_Q(ntry, ex5, wf4(:,13), wf8(:,28), n3(:,47), t3x8(:,:,18))
  call prop_A_Q(ntry, wf8(:,28), Q(:,52), MB, 1_intkind1, wf8(:,29), n2(15))
  call vert_AV_Q(ntry, wf4(:,12), wf4(:,13), wf16(:,21), n3(:,48), t3x16(:,:,21))
  call vert_VQ_A(ntry, wf4(:,13), ex2, wf8(:,30), n3(:,49), t3x8(:,:,19))
  call prop_Q_A(ntry, wf8(:,30), Q(:,38), MB, 1_intkind1, wf8(:,31), n2(16))
  call vert_QA_V(ntry, ex2, wf4(:,12), wf8(:,32), n3(:,50), t3x8(:,:,20))
  call vert_VQ_A(ntry, ex7, wf8(:,31), wf16(:,22), n3(:,51), t3x16(:,:,22))
  call vert_QA_V(ntry, wf8(:,7), ex6, wf16(:,23), n3(:,52), t3x16(:,:,23))
  call vert_QA_V(ntry, wf8(:,7), ex5, wf16(:,24), n3(:,53), t3x16(:,:,24))
  call vert_QA_V(ntry, ex3, wf8(:,20), wf16(:,25), n3(:,54), t3x16(:,:,25))
  call vert_VQ_A(ntry, wf8(:,27), ex3, wf16(:,26), n3(:,55), t3x16(:,:,26))
  call vert_QA_V(ntry, wf8(:,15), ex5, wf16(:,27), n3(:,56), t3x16(:,:,27))
  call vert_QA_V(ntry, wf8(:,15), ex6, wf16(:,28), n3(:,57), t3x16(:,:,28))
  call vert_QA_V(ntry, ex2, wf8(:,20), wf16(:,29), n3(:,58), t3x16(:,:,29))
  call vert_VQ_A(ntry, wf8(:,23), ex2, wf16(:,30), n3(:,59), t3x16(:,:,30))
  call vert_AV_Q(ntry, ex6, wf8(:,32), wf16(:,31), n3(:,60), t3x16(:,:,31))
  call vert_VQ_A(ntry, wf8(:,32), ex3, wf16(:,32), n3(:,61), t3x16(:,:,32))
  call vert_VQ_A(ntry, wf8(:,26), ex2, wf16(:,33), n3(:,62), t3x16(:,:,33))
  call vert_AV_Q(ntry, ex5, wf8(:,18), wf16(:,34), n3(:,63), t3x16(:,:,34))
  call vert_VQ_A(ntry, wf8(:,10), ex2, wf16(:,35), n3(:,64), t3x16(:,:,35))
  call vert_VQ_A(ntry, wf8(:,18), ex3, wf16(:,36), n3(:,65), t3x16(:,:,36))
  call vert_QA_V(ntry, ex1, ex5, wf4(:,14), n3(:,66), t3x4(:,:,10))
  call vert_QA_V(ntry, ex2, ex4, wf4(:,15), n3(:,67), t3x4(:,:,11))
  call vert_AV_Q(ntry, ex6, wf4(:,14), wf8(:,33), n3(:,68), t3x8(:,:,21))
  call vert_VQ_A(ntry, wf4(:,15), wf4(:,4), wf16(:,37), n3(:,69), t3x16(:,:,37))
  call prop_A_Q(ntry, wf8(:,33), Q(:,49), MB, 1_intkind1, wf8(:,34), n2(17))
  call vert_AV_Q(ntry, ex6, wf4(:,15), wf8(:,35), n3(:,70), t3x8(:,:,22))
  call vert_VQ_A(ntry, wf4(:,14), wf4(:,4), wf16(:,38), n3(:,71), t3x16(:,:,38))
  call prop_A_Q(ntry, wf8(:,35), Q(:,42), MB, 1_intkind1, wf8(:,36), n2(18))
  call vert_UV_W(ntry, wf4(:,15), Q(:,10), wf4(:,14), Q(:,17), wf16(:,39), n3(:,72), t3x16(:,:,39))
  call vert_VQ_A(ntry, wf4(:,14), ex3, wf8(:,37), n3(:,73), t3x8(:,:,23))
  call vert_AV_Q(ntry, wf4(:,6), wf4(:,15), wf16(:,40), n3(:,74), t3x16(:,:,40))
  call prop_Q_A(ntry, wf8(:,37), Q(:,21), MB, 1_intkind1, wf8(:,38), n2(19))
  call vert_VQ_A(ntry, wf4(:,15), ex3, wf8(:,39), n3(:,75), t3x8(:,:,24))
  call vert_AV_Q(ntry, wf4(:,6), wf4(:,14), wf16(:,41), n3(:,76), t3x16(:,:,41))
  call prop_Q_A(ntry, wf8(:,39), Q(:,14), MB, 1_intkind1, wf8(:,40), n2(20))
  call vert_VQ_A(ntry, ex7, wf8(:,38), wf16(:,42), n3(:,77), t3x16(:,:,42))
  call vert_VQ_A(ntry, ex7, wf8(:,40), wf16(:,43), n3(:,78), t3x16(:,:,43))
  call vert_VQ_A(ntry, ex7, ex1, wf4(:,16), n3(:,79), t3x4(:,:,12))
  call prop_Q_A(ntry, wf4(:,16), Q(:,65), MB, 1_intkind1, wf4(:,17), n2(21))
  call vert_QA_V(ntry, wf4(:,17), ex6, wf8(:,41), n3(:,80), t3x8(:,:,25))
  call vert_UV_W(ntry, wf4(:,15), Q(:,10), wf4(:,8), Q(:,20), wf16(:,44), n3(:,81), t3x16(:,:,44))
  call vert_VQ_A(ntry, wf4(:,8), wf4(:,17), wf16(:,45), n3(:,82), t3x16(:,:,45))
  call vert_VQ_A(ntry, wf4(:,15), wf4(:,17), wf16(:,46), n3(:,83), t3x16(:,:,46))
  call vert_VQ_A(ntry, wf4(:,15), ex1, wf8(:,42), n3(:,84), t3x8(:,:,26))
  call prop_Q_A(ntry, wf8(:,42), Q(:,11), MB, 1_intkind1, wf8(:,43), n2(22))
  call vert_VQ_A(ntry, wf4(:,8), ex1, wf8(:,44), n3(:,85), t3x8(:,:,27))
  call prop_Q_A(ntry, wf8(:,44), Q(:,21), MB, 1_intkind1, wf8(:,45), n2(23))
  call vert_QA_V(ntry, ex1, wf4(:,6), wf8(:,46), n3(:,86), t3x8(:,:,28))
  call vert_VQ_A(ntry, ex7, wf8(:,43), wf16(:,47), n3(:,87), t3x16(:,:,47))
  call vert_VQ_A(ntry, ex7, wf8(:,45), wf16(:,48), n3(:,88), t3x16(:,:,48))
  call vert_QA_V(ntry, ex1, ex6, wf4(:,18), n3(:,89), t3x4(:,:,13))
  call vert_AV_Q(ntry, ex5, wf4(:,18), wf8(:,47), n3(:,90), t3x8(:,:,29))
  call prop_A_Q(ntry, wf8(:,47), Q(:,49), MB, 1_intkind1, wf8(:,48), n2(24))
  call vert_AV_Q(ntry, ex5, wf4(:,15), wf8(:,49), n3(:,91), t3x8(:,:,30))
  call vert_VQ_A(ntry, wf4(:,18), wf4(:,4), wf16(:,49), n3(:,92), t3x16(:,:,49))
  call prop_A_Q(ntry, wf8(:,49), Q(:,26), MB, 1_intkind1, wf8(:,50), n2(25))
  call vert_UV_W(ntry, wf4(:,15), Q(:,10), wf4(:,18), Q(:,33), wf16(:,50), n3(:,93), t3x16(:,:,50))
  call vert_VQ_A(ntry, wf4(:,18), ex3, wf8(:,51), n3(:,94), t3x8(:,:,31))
  call vert_AV_Q(ntry, wf4(:,12), wf4(:,15), wf16(:,51), n3(:,95), t3x16(:,:,51))
  call prop_Q_A(ntry, wf8(:,51), Q(:,37), MB, 1_intkind1, wf8(:,52), n2(26))
  call vert_AV_Q(ntry, wf4(:,12), wf4(:,18), wf16(:,52), n3(:,96), t3x16(:,:,52))
  call vert_VQ_A(ntry, ex7, wf8(:,52), wf16(:,53), n3(:,97), t3x16(:,:,53))
  call vert_QA_V(ntry, wf4(:,17), ex5, wf8(:,53), n3(:,98), t3x8(:,:,32))
  call vert_UV_W(ntry, wf4(:,15), Q(:,10), wf4(:,13), Q(:,36), wf16(:,54), n3(:,99), t3x16(:,:,54))
  call vert_VQ_A(ntry, wf4(:,13), wf4(:,17), wf16(:,55), n3(:,100), t3x16(:,:,55))
  call vert_VQ_A(ntry, wf4(:,13), ex1, wf8(:,54), n3(:,101), t3x8(:,:,33))
  call prop_Q_A(ntry, wf8(:,54), Q(:,37), MB, 1_intkind1, wf8(:,55), n2(27))
  call vert_QA_V(ntry, ex1, wf4(:,12), wf8(:,56), n3(:,102), t3x8(:,:,34))
  call vert_VQ_A(ntry, ex7, wf8(:,55), wf16(:,56), n3(:,103), t3x16(:,:,56))
  call vert_QA_V(ntry, wf8(:,40), ex6, wf16(:,57), n3(:,104), t3x16(:,:,57))
  call vert_VQ_A(ntry, wf8(:,53), ex3, wf16(:,58), n3(:,105), t3x16(:,:,58))
  call vert_QA_V(ntry, wf8(:,40), ex5, wf16(:,59), n3(:,106), t3x16(:,:,59))
  call vert_QA_V(ntry, ex3, wf8(:,50), wf16(:,60), n3(:,107), t3x16(:,:,60))
  call vert_QA_V(ntry, wf8(:,43), ex5, wf16(:,61), n3(:,108), t3x16(:,:,61))
  call vert_QA_V(ntry, wf8(:,43), ex6, wf16(:,62), n3(:,109), t3x16(:,:,62))
  call vert_QA_V(ntry, ex1, wf8(:,50), wf16(:,63), n3(:,110), t3x16(:,:,63))
  call vert_VQ_A(ntry, wf8(:,23), ex1, wf16(:,64), n3(:,111), t3x16(:,:,64))
  call vert_VQ_A(ntry, wf8(:,56), ex3, wf16(:,65), n3(:,112), t3x16(:,:,65))
  call vert_VQ_A(ntry, wf8(:,26), ex1, wf16(:,66), n3(:,113), t3x16(:,:,66))
  call vert_AV_Q(ntry, ex6, wf8(:,56), wf16(:,67), n3(:,114), t3x16(:,:,67))
  call vert_VQ_A(ntry, wf8(:,10), ex1, wf16(:,68), n3(:,115), t3x16(:,:,68))
  call vert_VQ_A(ntry, wf8(:,46), ex3, wf16(:,69), n3(:,116), t3x16(:,:,69))
  call vert_AV_Q(ntry, ex5, wf8(:,46), wf16(:,70), n3(:,117), t3x16(:,:,70))
  call vert_QA_V(ntry, ex3, ex4, wf4(:,19), n3(:,118), t3x4(:,:,14))
  call vert_VQ_A(ntry, wf4(:,19), wf4(:,9), wf16(:,71), n3(:,119), t3x16(:,:,71))
  call vert_UV_W(ntry, wf4(:,19), Q(:,12), wf4(:,14), Q(:,17), wf16(:,72), n3(:,120), t3x16(:,:,72))
  call vert_AV_Q(ntry, ex6, wf4(:,19), wf8(:,57), n3(:,121), t3x8(:,:,35))
  call vert_VQ_A(ntry, wf4(:,14), wf4(:,9), wf16(:,73), n3(:,122), t3x16(:,:,73))
  call prop_A_Q(ntry, wf8(:,57), Q(:,44), MB, 1_intkind1, wf8(:,58), n2(28))
  call vert_VQ_A(ntry, wf4(:,14), ex2, wf8(:,59), n3(:,123), t3x8(:,:,36))
  call vert_AV_Q(ntry, wf4(:,6), wf4(:,19), wf16(:,74), n3(:,124), t3x16(:,:,74))
  call prop_Q_A(ntry, wf8(:,59), Q(:,19), MB, 1_intkind1, wf8(:,60), n2(29))
  call vert_VQ_A(ntry, wf4(:,19), ex2, wf8(:,61), n3(:,125), t3x8(:,:,37))
  call prop_Q_A(ntry, wf8(:,61), Q(:,14), MB, 1_intkind1, wf8(:,62), n2(30))
  call vert_VQ_A(ntry, ex7, wf8(:,60), wf16(:,75), n3(:,126), t3x16(:,:,75))
  call vert_VQ_A(ntry, ex7, wf8(:,62), wf16(:,76), n3(:,127), t3x16(:,:,76))
  call vert_UV_W(ntry, wf4(:,19), Q(:,12), wf4(:,2), Q(:,18), wf16(:,77), n3(:,128), t3x16(:,:,77))
  call vert_VQ_A(ntry, wf4(:,19), wf4(:,17), wf16(:,78), n3(:,129), t3x16(:,:,78))
  call vert_VQ_A(ntry, wf4(:,2), wf4(:,17), wf16(:,79), n3(:,130), t3x16(:,:,79))
  call vert_VQ_A(ntry, wf4(:,2), ex1, wf8(:,63), n3(:,131), t3x8(:,:,38))
  call prop_Q_A(ntry, wf8(:,63), Q(:,19), MB, 1_intkind1, wf8(:,64), n2(31))
  call vert_VQ_A(ntry, wf4(:,19), ex1, wf8(:,65), n3(:,132), t3x8(:,:,39))
  call prop_Q_A(ntry, wf8(:,65), Q(:,13), MB, 1_intkind1, wf8(:,66), n2(32))
  call vert_VQ_A(ntry, ex7, wf8(:,64), wf16(:,80), n3(:,133), t3x16(:,:,80))
  call vert_VQ_A(ntry, ex7, wf8(:,66), wf16(:,81), n3(:,134), t3x16(:,:,81))
  call vert_UV_W(ntry, wf4(:,19), Q(:,12), wf4(:,18), Q(:,33), wf16(:,82), n3(:,135), t3x16(:,:,82))
  call vert_AV_Q(ntry, ex5, wf4(:,19), wf8(:,67), n3(:,136), t3x8(:,:,40))
  call vert_VQ_A(ntry, wf4(:,18), wf4(:,9), wf16(:,83), n3(:,137), t3x16(:,:,83))
  call prop_A_Q(ntry, wf8(:,67), Q(:,28), MB, 1_intkind1, wf8(:,68), n2(33))
  call vert_VQ_A(ntry, wf4(:,18), ex2, wf8(:,69), n3(:,138), t3x8(:,:,41))
  call vert_AV_Q(ntry, wf4(:,12), wf4(:,19), wf16(:,84), n3(:,139), t3x16(:,:,84))
  call prop_Q_A(ntry, wf8(:,69), Q(:,35), MB, 1_intkind1, wf8(:,70), n2(34))
  call vert_VQ_A(ntry, ex7, wf8(:,70), wf16(:,85), n3(:,140), t3x16(:,:,85))
  call vert_UV_W(ntry, wf4(:,19), Q(:,12), wf4(:,10), Q(:,34), wf16(:,86), n3(:,141), t3x16(:,:,86))
  call vert_VQ_A(ntry, wf4(:,10), wf4(:,17), wf16(:,87), n3(:,142), t3x16(:,:,87))
  call vert_VQ_A(ntry, wf4(:,10), ex1, wf8(:,71), n3(:,143), t3x8(:,:,42))
  call prop_Q_A(ntry, wf8(:,71), Q(:,35), MB, 1_intkind1, wf8(:,72), n2(35))
  call vert_VQ_A(ntry, ex7, wf8(:,72), wf16(:,88), n3(:,144), t3x16(:,:,88))
  call vert_QA_V(ntry, wf8(:,62), ex6, wf16(:,89), n3(:,145), t3x16(:,:,89))
  call vert_VQ_A(ntry, wf8(:,53), ex2, wf16(:,90), n3(:,146), t3x16(:,:,90))
  call vert_QA_V(ntry, wf8(:,62), ex5, wf16(:,91), n3(:,147), t3x16(:,:,91))
  call vert_QA_V(ntry, ex2, wf8(:,68), wf16(:,92), n3(:,148), t3x16(:,:,92))
  call vert_QA_V(ntry, wf8(:,66), ex5, wf16(:,93), n3(:,149), t3x16(:,:,93))
  call vert_QA_V(ntry, wf8(:,66), ex6, wf16(:,94), n3(:,150), t3x16(:,:,94))
  call vert_VQ_A(ntry, wf8(:,27), ex1, wf16(:,95), n3(:,151), t3x16(:,:,95))
  call vert_QA_V(ntry, ex1, wf8(:,68), wf16(:,96), n3(:,152), t3x16(:,:,96))
  call vert_VQ_A(ntry, wf8(:,56), ex2, wf16(:,97), n3(:,153), t3x16(:,:,97))
  call vert_VQ_A(ntry, wf8(:,32), ex1, wf16(:,98), n3(:,154), t3x16(:,:,98))
  call vert_VQ_A(ntry, wf8(:,18), ex1, wf16(:,99), n3(:,155), t3x16(:,:,99))
  call vert_VQ_A(ntry, wf8(:,46), ex2, wf16(:,100), n3(:,156), t3x16(:,:,100))
  call vert_AV_Q(ntry, ex4, wf4(:,14), wf8(:,73), n3(:,157), t3x8(:,:,43))
  call prop_A_Q(ntry, wf8(:,73), Q(:,25), MB, 1_intkind1, wf8(:,74), n2(36))
  call vert_AV_Q(ntry, ex4, wf4(:,10), wf8(:,75), n3(:,158), t3x8(:,:,44))
  call prop_A_Q(ntry, wf8(:,75), Q(:,42), MB, 1_intkind1, wf8(:,76), n2(37))
  call vert_QA_V(ntry, wf4(:,4), ex4, wf8(:,77), n3(:,159), t3x8(:,:,45))
  call vert_UV_W(ntry, wf4(:,14), Q(:,17), wf4(:,10), Q(:,34), wf16(:,101), n3(:,160), t3x16(:,:,101))
  call vert_AV_Q(ntry, ex4, ex7, wf4(:,20), n3(:,161), t3x4(:,:,15))
  call prop_A_Q(ntry, wf4(:,20), Q(:,72), MB, 1_intkind1, wf4(:,21), n2(38))
  call vert_AV_Q(ntry, wf4(:,21), wf4(:,10), wf16(:,102), n3(:,162), t3x16(:,:,102))
  call vert_AV_Q(ntry, wf4(:,21), wf4(:,14), wf16(:,103), n3(:,163), t3x16(:,:,103))
  call vert_QA_V(ntry, ex3, wf4(:,21), wf8(:,78), n3(:,164), t3x8(:,:,46))
  call vert_QA_V(ntry, wf4(:,9), ex4, wf8(:,79), n3(:,165), t3x8(:,:,47))
  call vert_UV_W(ntry, wf4(:,14), Q(:,17), wf4(:,13), Q(:,36), wf16(:,104), n3(:,166), t3x16(:,:,104))
  call vert_AV_Q(ntry, ex4, wf4(:,13), wf8(:,80), n3(:,167), t3x8(:,:,48))
  call prop_A_Q(ntry, wf8(:,80), Q(:,44), MB, 1_intkind1, wf8(:,81), n2(39))
  call vert_AV_Q(ntry, wf4(:,21), wf4(:,13), wf16(:,105), n3(:,168), t3x16(:,:,105))
  call vert_QA_V(ntry, ex2, wf4(:,21), wf8(:,82), n3(:,169), t3x8(:,:,49))
  call vert_QA_V(ntry, wf8(:,38), ex6, wf16(:,106), n3(:,170), t3x16(:,:,106))
  call vert_QA_V(ntry, wf8(:,38), ex4, wf16(:,107), n3(:,171), t3x16(:,:,107))
  call vert_QA_V(ntry, ex3, wf8(:,74), wf16(:,108), n3(:,172), t3x16(:,:,108))
  call vert_VQ_A(ntry, wf8(:,79), ex3, wf16(:,109), n3(:,173), t3x16(:,:,109))
  call vert_QA_V(ntry, wf8(:,60), ex4, wf16(:,110), n3(:,174), t3x16(:,:,110))
  call vert_QA_V(ntry, wf8(:,60), ex6, wf16(:,111), n3(:,175), t3x16(:,:,111))
  call vert_QA_V(ntry, ex2, wf8(:,74), wf16(:,112), n3(:,176), t3x16(:,:,112))
  call vert_VQ_A(ntry, wf8(:,77), ex2, wf16(:,113), n3(:,177), t3x16(:,:,113))
  call vert_AV_Q(ntry, ex6, wf8(:,82), wf16(:,114), n3(:,178), t3x16(:,:,114))
  call vert_VQ_A(ntry, wf8(:,82), ex3, wf16(:,115), n3(:,179), t3x16(:,:,115))
  call vert_VQ_A(ntry, wf8(:,78), ex2, wf16(:,116), n3(:,180), t3x16(:,:,116))
  call vert_AV_Q(ntry, ex4, wf8(:,18), wf16(:,117), n3(:,181), t3x16(:,:,117))
  call vert_AV_Q(ntry, ex4, wf4(:,18), wf8(:,83), n3(:,182), t3x8(:,:,50))
  call prop_A_Q(ntry, wf8(:,83), Q(:,41), MB, 1_intkind1, wf8(:,84), n2(40))
  call vert_AV_Q(ntry, ex4, wf4(:,2), wf8(:,85), n3(:,183), t3x8(:,:,51))
  call prop_A_Q(ntry, wf8(:,85), Q(:,26), MB, 1_intkind1, wf8(:,86), n2(41))
  call vert_UV_W(ntry, wf4(:,2), Q(:,18), wf4(:,18), Q(:,33), wf16(:,118), n3(:,184), t3x16(:,:,118))
  call vert_AV_Q(ntry, wf4(:,21), wf4(:,2), wf16(:,119), n3(:,185), t3x16(:,:,119))
  call vert_AV_Q(ntry, wf4(:,21), wf4(:,18), wf16(:,120), n3(:,186), t3x16(:,:,120))
  call vert_QA_V(ntry, wf4(:,17), ex4, wf8(:,87), n3(:,187), t3x8(:,:,52))
  call vert_UV_W(ntry, wf4(:,2), Q(:,18), wf4(:,13), Q(:,36), wf16(:,121), n3(:,188), t3x16(:,:,121))
  call vert_QA_V(ntry, ex1, wf4(:,21), wf8(:,88), n3(:,189), t3x8(:,:,53))
  call vert_QA_V(ntry, wf8(:,9), ex6, wf16(:,122), n3(:,190), t3x16(:,:,122))
  call vert_VQ_A(ntry, wf8(:,87), ex3, wf16(:,123), n3(:,191), t3x16(:,:,123))
  call vert_QA_V(ntry, wf8(:,9), ex4, wf16(:,124), n3(:,192), t3x16(:,:,124))
  call vert_QA_V(ntry, ex3, wf8(:,86), wf16(:,125), n3(:,193), t3x16(:,:,125))
  call vert_QA_V(ntry, wf8(:,64), ex4, wf16(:,126), n3(:,194), t3x16(:,:,126))
  call vert_QA_V(ntry, wf8(:,64), ex6, wf16(:,127), n3(:,195), t3x16(:,:,127))
  call vert_QA_V(ntry, ex1, wf8(:,86), wf16(:,128), n3(:,196), t3x16(:,:,128))
  call vert_VQ_A(ntry, wf8(:,77), ex1, wf16(:,129), n3(:,197), t3x16(:,:,129))
  call vert_VQ_A(ntry, wf8(:,88), ex3, wf16(:,130), n3(:,198), t3x16(:,:,130))
  call vert_VQ_A(ntry, wf8(:,78), ex1, wf16(:,131), n3(:,199), t3x16(:,:,131))
  call vert_AV_Q(ntry, ex6, wf8(:,88), wf16(:,132), n3(:,200), t3x16(:,:,132))
  call vert_AV_Q(ntry, ex4, wf8(:,46), wf16(:,133), n3(:,201), t3x16(:,:,133))
  call vert_UV_W(ntry, wf4(:,8), Q(:,20), wf4(:,18), Q(:,33), wf16(:,134), n3(:,202), t3x16(:,:,134))
  call vert_AV_Q(ntry, ex4, wf4(:,8), wf8(:,89), n3(:,203), t3x8(:,:,54))
  call prop_A_Q(ntry, wf8(:,89), Q(:,28), MB, 1_intkind1, wf8(:,90), n2(42))
  call vert_AV_Q(ntry, wf4(:,21), wf4(:,8), wf16(:,135), n3(:,204), t3x16(:,:,135))
  call vert_UV_W(ntry, wf4(:,8), Q(:,20), wf4(:,10), Q(:,34), wf16(:,136), n3(:,205), t3x16(:,:,136))
  call vert_QA_V(ntry, wf8(:,17), ex6, wf16(:,137), n3(:,206), t3x16(:,:,137))
  call vert_VQ_A(ntry, wf8(:,87), ex2, wf16(:,138), n3(:,207), t3x16(:,:,138))
  call vert_QA_V(ntry, wf8(:,17), ex4, wf16(:,139), n3(:,208), t3x16(:,:,139))
  call vert_QA_V(ntry, ex2, wf8(:,90), wf16(:,140), n3(:,209), t3x16(:,:,140))
  call vert_QA_V(ntry, wf8(:,45), ex4, wf16(:,141), n3(:,210), t3x16(:,:,141))
  call vert_QA_V(ntry, wf8(:,45), ex6, wf16(:,142), n3(:,211), t3x16(:,:,142))
  call vert_VQ_A(ntry, wf8(:,79), ex1, wf16(:,143), n3(:,212), t3x16(:,:,143))
  call vert_QA_V(ntry, ex1, wf8(:,90), wf16(:,144), n3(:,213), t3x16(:,:,144))
  call vert_VQ_A(ntry, wf8(:,88), ex2, wf16(:,145), n3(:,214), t3x16(:,:,145))
  call vert_VQ_A(ntry, wf8(:,82), ex1, wf16(:,146), n3(:,215), t3x16(:,:,146))
  call vert_QA_V(ntry, wf8(:,52), ex5, wf16(:,147), n3(:,216), t3x16(:,:,147))
  call vert_QA_V(ntry, wf8(:,52), ex4, wf16(:,148), n3(:,217), t3x16(:,:,148))
  call vert_QA_V(ntry, ex3, wf8(:,84), wf16(:,149), n3(:,218), t3x16(:,:,149))
  call vert_QA_V(ntry, wf8(:,70), ex4, wf16(:,150), n3(:,219), t3x16(:,:,150))
  call vert_QA_V(ntry, wf8(:,70), ex5, wf16(:,151), n3(:,220), t3x16(:,:,151))
  call vert_QA_V(ntry, ex2, wf8(:,84), wf16(:,152), n3(:,221), t3x16(:,:,152))
  call vert_AV_Q(ntry, ex5, wf8(:,82), wf16(:,153), n3(:,222), t3x16(:,:,153))
  call vert_AV_Q(ntry, ex4, wf8(:,32), wf16(:,154), n3(:,223), t3x16(:,:,154))
  call vert_QA_V(ntry, wf8(:,25), ex5, wf16(:,155), n3(:,224), t3x16(:,:,155))
  call vert_QA_V(ntry, wf8(:,25), ex4, wf16(:,156), n3(:,225), t3x16(:,:,156))
  call vert_QA_V(ntry, ex3, wf8(:,76), wf16(:,157), n3(:,226), t3x16(:,:,157))
  call vert_QA_V(ntry, wf8(:,72), ex4, wf16(:,158), n3(:,227), t3x16(:,:,158))
  call vert_QA_V(ntry, wf8(:,72), ex5, wf16(:,159), n3(:,228), t3x16(:,:,159))
  call vert_QA_V(ntry, ex1, wf8(:,76), wf16(:,160), n3(:,229), t3x16(:,:,160))
  call vert_AV_Q(ntry, ex5, wf8(:,88), wf16(:,161), n3(:,230), t3x16(:,:,161))
  call vert_AV_Q(ntry, ex4, wf8(:,56), wf16(:,162), n3(:,231), t3x16(:,:,162))
  call vert_QA_V(ntry, wf8(:,31), ex5, wf16(:,163), n3(:,232), t3x16(:,:,163))
  call vert_QA_V(ntry, wf8(:,31), ex4, wf16(:,164), n3(:,233), t3x16(:,:,164))
  call vert_QA_V(ntry, ex2, wf8(:,81), wf16(:,165), n3(:,234), t3x16(:,:,165))
  call vert_QA_V(ntry, wf8(:,55), ex4, wf16(:,166), n3(:,235), t3x16(:,:,166))
  call vert_QA_V(ntry, wf8(:,55), ex5, wf16(:,167), n3(:,236), t3x16(:,:,167))
  call vert_QA_V(ntry, ex1, wf8(:,81), wf16(:,168), n3(:,237), t3x16(:,:,168))


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

    M2munu = M2munu / average_factor_ppajjj_bbbbxbxbxa_1
  end if

  ! Colour and helicity average and symmetry factor of outgoing particles
  M2add = M2 / average_factor_ppajjj_bbbbxbxbxa_1

  do k = 0, 30-1
    M2(k) = M2add(extcomb_perm_ppajjj_bbbbxbxbxa_1(k))
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

    call cont_QA(nsync, wf16(:,1), wf8(:,2), A(:,1), n3(:,238), t3x128(:,:,1), nhel, den(7))
    call cont_QA(nsync, wf16(:,2), wf8(:,4), A(:,2), n3(:,239), t3x128(:,:,2), nhel, den(11))
    call cont_VV(nsync, wf8(:,5), wf16(:,3), A(:,3), n3(:,240), t3x128(:,:,3), nhel, den(15))
    call cont_QA(nsync, wf16(:,4), wf8(:,7), A(:,4), n3(:,241), t3x128(:,:,4), nhel, den(20))
    call cont_QA(nsync, wf16(:,5), wf8(:,9), A(:,5), n3(:,242), t3x128(:,:,5), nhel, den(24))
    call cont_VV(nsync, wf16(:,3), wf8(:,10), A(:,6), n3(:,243), t3x128(:,:,6), nhel, den(26))
    call cont_QA(nsync, wf8(:,4), wf16(:,6), A(:,7), n3(:,244), t3x128(:,:,7), nhel, den(27))
    call cont_QA(nsync, wf8(:,2), wf16(:,7), A(:,8), n3(:,245), t3x128(:,:,8), nhel, den(28))
    call cont_QA(nsync, wf8(:,2), wf16(:,8), A(:,9), n3(:,246), t3x128(:,:,9), nhel, den(32))
    call cont_VV(nsync, wf8(:,11), wf16(:,9), A(:,10), n3(:,247), t3x128(:,:,10), nhel, den(36))
    call cont_QA(nsync, wf16(:,10), wf8(:,13), A(:,11), n3(:,248), t3x128(:,:,11), nhel, den(40))
    call cont_QA(nsync, wf16(:,11), wf8(:,15), A(:,12), n3(:,249), t3x128(:,:,12), nhel, den(44))
    call cont_QA(nsync, wf16(:,5), wf8(:,17), A(:,13), n3(:,250), t3x128(:,:,13), nhel, den(46))
    call cont_VV(nsync, wf16(:,9), wf8(:,18), A(:,14), n3(:,251), t3x128(:,:,14), nhel, den(48))
    call cont_QA(nsync, wf8(:,13), wf16(:,12), A(:,15), n3(:,252), t3x128(:,:,15), nhel, den(49))
    call cont_QA(nsync, wf8(:,2), wf16(:,13), A(:,16), n3(:,253), t3x128(:,:,16), nhel, den(50))
    call cont_QA(nsync, wf16(:,14), wf8(:,20), A(:,17), n3(:,254), t3x128(:,:,17), nhel, den(55))
    call cont_QA(nsync, wf16(:,2), wf8(:,22), A(:,18), n3(:,255), t3x128(:,:,18), nhel, den(57))
    call cont_VV(nsync, wf8(:,23), wf16(:,15), A(:,19), n3(:,256), t3x128(:,:,19), nhel, den(61))
    call cont_QA(nsync, wf8(:,7), wf16(:,16), A(:,20), n3(:,257), t3x128(:,:,20), nhel, den(64))
    call cont_QA(nsync, wf16(:,17), wf8(:,25), A(:,21), n3(:,258), t3x128(:,:,21), nhel, den(68))
    call cont_VV(nsync, wf16(:,15), wf8(:,26), A(:,22), n3(:,259), t3x128(:,:,22), nhel, den(70))
    call cont_QA(nsync, wf16(:,6), wf8(:,22), A(:,23), n3(:,260), t3x128(:,:,23), nhel, den(71))
    call cont_QA(nsync, wf8(:,20), wf16(:,18), A(:,24), n3(:,261), t3x128(:,:,24), nhel, den(72))
    call cont_QA(nsync, wf8(:,20), wf16(:,19), A(:,25), n3(:,262), t3x128(:,:,25), nhel, den(75))
    call cont_VV(nsync, wf8(:,27), wf16(:,20), A(:,26), n3(:,263), t3x128(:,:,26), nhel, den(79))
    call cont_QA(nsync, wf16(:,10), wf8(:,29), A(:,27), n3(:,264), t3x128(:,:,27), nhel, den(81))
    call cont_QA(nsync, wf8(:,15), wf16(:,21), A(:,28), n3(:,265), t3x128(:,:,28), nhel, den(83))
    call cont_QA(nsync, wf16(:,17), wf8(:,31), A(:,29), n3(:,266), t3x128(:,:,29), nhel, den(85))
    call cont_VV(nsync, wf16(:,20), wf8(:,32), A(:,30), n3(:,267), t3x128(:,:,30), nhel, den(87))
    call cont_QA(nsync, wf16(:,12), wf8(:,29), A(:,31), n3(:,268), t3x128(:,:,31), nhel, den(88))
    call cont_QA(nsync, wf8(:,20), wf16(:,22), A(:,32), n3(:,269), t3x128(:,:,32), nhel, den(89))
    call cont_VV(nsync, wf8(:,27), wf16(:,23), A(:,33), n3(:,270), t3x128(:,:,33), nhel, den(90))
    call cont_VV(nsync, wf8(:,11), wf16(:,24), A(:,34), n3(:,271), t3x128(:,:,34), nhel, den(91))
    call cont_VV(nsync, wf8(:,11), wf16(:,25), A(:,35), n3(:,272), t3x128(:,:,35), nhel, den(92))
    call cont_QA(nsync, wf8(:,2), wf16(:,26), A(:,36), n3(:,273), t3x128(:,:,36), nhel, den(93))
    call cont_VV(nsync, wf8(:,5), wf16(:,27), A(:,37), n3(:,274), t3x128(:,:,37), nhel, den(94))
    call cont_VV(nsync, wf8(:,23), wf16(:,28), A(:,38), n3(:,275), t3x128(:,:,38), nhel, den(95))
    call cont_VV(nsync, wf8(:,5), wf16(:,29), A(:,39), n3(:,276), t3x128(:,:,39), nhel, den(96))
    call cont_QA(nsync, wf8(:,2), wf16(:,30), A(:,40), n3(:,277), t3x128(:,:,40), nhel, den(97))
    call cont_VV(nsync, wf8(:,26), wf16(:,28), A(:,41), n3(:,278), t3x128(:,:,41), nhel, den(98))
    call cont_QA(nsync, wf8(:,7), wf16(:,31), A(:,42), n3(:,279), t3x128(:,:,42), nhel, den(99))
    call cont_QA(nsync, wf8(:,2), wf16(:,32), A(:,43), n3(:,280), t3x128(:,:,43), nhel, den(100))
    call cont_QA(nsync, wf8(:,2), wf16(:,33), A(:,44), n3(:,281), t3x128(:,:,44), nhel, den(101))
    call cont_VV(nsync, wf8(:,10), wf16(:,27), A(:,45), n3(:,282), t3x128(:,:,45), nhel, den(102))
    call cont_QA(nsync, wf8(:,7), wf16(:,34), A(:,46), n3(:,283), t3x128(:,:,46), nhel, den(103))
    call cont_QA(nsync, wf8(:,20), wf16(:,35), A(:,47), n3(:,284), t3x128(:,:,47), nhel, den(104))
    call cont_QA(nsync, wf8(:,20), wf16(:,36), A(:,48), n3(:,285), t3x128(:,:,48), nhel, den(105))
    call cont_QA(nsync, wf16(:,37), wf8(:,34), A(:,49), n3(:,286), t3x128(:,:,49), nhel, den(111))
    call cont_QA(nsync, wf16(:,38), wf8(:,36), A(:,50), n3(:,287), t3x128(:,:,50), nhel, den(115))
    call cont_VV(nsync, wf8(:,5), wf16(:,39), A(:,51), n3(:,288), t3x128(:,:,51), nhel, den(117))
    call cont_QA(nsync, wf16(:,40), wf8(:,38), A(:,52), n3(:,289), t3x128(:,:,52), nhel, den(121))
    call cont_QA(nsync, wf16(:,41), wf8(:,40), A(:,53), n3(:,290), t3x128(:,:,53), nhel, den(125))
    call cont_VV(nsync, wf8(:,10), wf16(:,39), A(:,54), n3(:,291), t3x128(:,:,54), nhel, den(126))
    call cont_QA(nsync, wf8(:,36), wf16(:,42), A(:,55), n3(:,292), t3x128(:,:,55), nhel, den(127))
    call cont_QA(nsync, wf8(:,34), wf16(:,43), A(:,56), n3(:,293), t3x128(:,:,56), nhel, den(128))
    call cont_VV(nsync, wf8(:,41), wf16(:,44), A(:,57), n3(:,294), t3x128(:,:,57), nhel, den(133))
    call cont_QA(nsync, wf8(:,36), wf16(:,45), A(:,58), n3(:,295), t3x128(:,:,58), nhel, den(135))
    call cont_QA(nsync, wf8(:,13), wf16(:,46), A(:,59), n3(:,296), t3x128(:,:,59), nhel, den(137))
    call cont_QA(nsync, wf16(:,11), wf8(:,43), A(:,60), n3(:,297), t3x128(:,:,60), nhel, den(139))
    call cont_QA(nsync, wf16(:,40), wf8(:,45), A(:,61), n3(:,298), t3x128(:,:,61), nhel, den(141))
    call cont_VV(nsync, wf16(:,44), wf8(:,46), A(:,62), n3(:,299), t3x128(:,:,62), nhel, den(143))
    call cont_QA(nsync, wf8(:,13), wf16(:,47), A(:,63), n3(:,300), t3x128(:,:,63), nhel, den(144))
    call cont_QA(nsync, wf8(:,36), wf16(:,48), A(:,64), n3(:,301), t3x128(:,:,64), nhel, den(145))
    call cont_QA(nsync, wf16(:,37), wf8(:,48), A(:,65), n3(:,302), t3x128(:,:,65), nhel, den(148))
    call cont_QA(nsync, wf16(:,49), wf8(:,50), A(:,66), n3(:,303), t3x128(:,:,66), nhel, den(152))
    call cont_VV(nsync, wf8(:,23), wf16(:,50), A(:,67), n3(:,304), t3x128(:,:,67), nhel, den(154))
    call cont_QA(nsync, wf16(:,51), wf8(:,52), A(:,68), n3(:,305), t3x128(:,:,68), nhel, den(158))
    call cont_QA(nsync, wf8(:,40), wf16(:,52), A(:,69), n3(:,306), t3x128(:,:,69), nhel, den(160))
    call cont_VV(nsync, wf8(:,26), wf16(:,50), A(:,70), n3(:,307), t3x128(:,:,70), nhel, den(161))
    call cont_QA(nsync, wf8(:,50), wf16(:,53), A(:,71), n3(:,308), t3x128(:,:,71), nhel, den(162))
    call cont_QA(nsync, wf16(:,43), wf8(:,48), A(:,72), n3(:,309), t3x128(:,:,72), nhel, den(163))
    call cont_VV(nsync, wf8(:,53), wf16(:,54), A(:,73), n3(:,310), t3x128(:,:,73), nhel, den(167))
    call cont_QA(nsync, wf8(:,50), wf16(:,55), A(:,74), n3(:,311), t3x128(:,:,74), nhel, den(169))
    call cont_QA(nsync, wf8(:,29), wf16(:,46), A(:,75), n3(:,312), t3x128(:,:,75), nhel, den(170))
    call cont_QA(nsync, wf16(:,21), wf8(:,43), A(:,76), n3(:,313), t3x128(:,:,76), nhel, den(171))
    call cont_QA(nsync, wf16(:,51), wf8(:,55), A(:,77), n3(:,314), t3x128(:,:,77), nhel, den(173))
    call cont_VV(nsync, wf16(:,54), wf8(:,56), A(:,78), n3(:,315), t3x128(:,:,78), nhel, den(175))
    call cont_QA(nsync, wf8(:,29), wf16(:,47), A(:,79), n3(:,316), t3x128(:,:,79), nhel, den(176))
    call cont_QA(nsync, wf8(:,50), wf16(:,56), A(:,80), n3(:,317), t3x128(:,:,80), nhel, den(177))
    call cont_VV(nsync, wf8(:,53), wf16(:,57), A(:,81), n3(:,318), t3x128(:,:,81), nhel, den(178))
    call cont_QA(nsync, wf8(:,36), wf16(:,58), A(:,82), n3(:,319), t3x128(:,:,82), nhel, den(179))
    call cont_VV(nsync, wf8(:,41), wf16(:,59), A(:,83), n3(:,320), t3x128(:,:,83), nhel, den(180))
    call cont_VV(nsync, wf8(:,41), wf16(:,60), A(:,84), n3(:,321), t3x128(:,:,84), nhel, den(181))
    call cont_VV(nsync, wf8(:,5), wf16(:,61), A(:,85), n3(:,322), t3x128(:,:,85), nhel, den(182))
    call cont_VV(nsync, wf8(:,23), wf16(:,62), A(:,86), n3(:,323), t3x128(:,:,86), nhel, den(183))
    call cont_VV(nsync, wf8(:,5), wf16(:,63), A(:,87), n3(:,324), t3x128(:,:,87), nhel, den(184))
    call cont_QA(nsync, wf8(:,36), wf16(:,64), A(:,88), n3(:,325), t3x128(:,:,88), nhel, den(185))
    call cont_VV(nsync, wf8(:,26), wf16(:,62), A(:,89), n3(:,326), t3x128(:,:,89), nhel, den(186))
    call cont_QA(nsync, wf8(:,36), wf16(:,65), A(:,90), n3(:,327), t3x128(:,:,90), nhel, den(187))
    call cont_QA(nsync, wf8(:,36), wf16(:,66), A(:,91), n3(:,328), t3x128(:,:,91), nhel, den(188))
    call cont_QA(nsync, wf8(:,40), wf16(:,67), A(:,92), n3(:,329), t3x128(:,:,92), nhel, den(189))
    call cont_VV(nsync, wf8(:,10), wf16(:,61), A(:,93), n3(:,330), t3x128(:,:,93), nhel, den(190))
    call cont_QA(nsync, wf8(:,50), wf16(:,68), A(:,94), n3(:,331), t3x128(:,:,94), nhel, den(191))
    call cont_QA(nsync, wf8(:,50), wf16(:,69), A(:,95), n3(:,332), t3x128(:,:,95), nhel, den(192))
    call cont_QA(nsync, wf8(:,40), wf16(:,70), A(:,96), n3(:,333), t3x128(:,:,96), nhel, den(193))
    call cont_QA(nsync, wf8(:,34), wf16(:,71), A(:,97), n3(:,334), t3x128(:,:,97), nhel, den(196))
    call cont_VV(nsync, wf8(:,11), wf16(:,72), A(:,98), n3(:,335), t3x128(:,:,98), nhel, den(198))
    call cont_QA(nsync, wf16(:,73), wf8(:,58), A(:,99), n3(:,336), t3x128(:,:,99), nhel, den(202))
    call cont_QA(nsync, wf16(:,74), wf8(:,60), A(:,100), n3(:,337), t3x128(:,:,100), nhel, den(206))
    call cont_QA(nsync, wf16(:,41), wf8(:,62), A(:,101), n3(:,338), t3x128(:,:,101), nhel, den(208))
    call cont_VV(nsync, wf8(:,18), wf16(:,72), A(:,102), n3(:,339), t3x128(:,:,102), nhel, den(209))
    call cont_QA(nsync, wf8(:,58), wf16(:,75), A(:,103), n3(:,340), t3x128(:,:,103), nhel, den(210))
    call cont_QA(nsync, wf8(:,34), wf16(:,76), A(:,104), n3(:,341), t3x128(:,:,104), nhel, den(211))
    call cont_VV(nsync, wf8(:,41), wf16(:,77), A(:,105), n3(:,342), t3x128(:,:,105), nhel, den(213))
    call cont_QA(nsync, wf8(:,4), wf16(:,78), A(:,106), n3(:,343), t3x128(:,:,106), nhel, den(215))
    call cont_QA(nsync, wf8(:,58), wf16(:,79), A(:,107), n3(:,344), t3x128(:,:,107), nhel, den(217))
    call cont_QA(nsync, wf16(:,74), wf8(:,64), A(:,108), n3(:,345), t3x128(:,:,108), nhel, den(219))
    call cont_QA(nsync, wf16(:,4), wf8(:,66), A(:,109), n3(:,346), t3x128(:,:,109), nhel, den(221))
    call cont_VV(nsync, wf8(:,46), wf16(:,77), A(:,110), n3(:,347), t3x128(:,:,110), nhel, den(222))
    call cont_QA(nsync, wf8(:,58), wf16(:,80), A(:,111), n3(:,348), t3x128(:,:,111), nhel, den(223))
    call cont_QA(nsync, wf8(:,4), wf16(:,81), A(:,112), n3(:,349), t3x128(:,:,112), nhel, den(224))
    call cont_QA(nsync, wf8(:,48), wf16(:,71), A(:,113), n3(:,350), t3x128(:,:,113), nhel, den(225))
    call cont_VV(nsync, wf8(:,27), wf16(:,82), A(:,114), n3(:,351), t3x128(:,:,114), nhel, den(227))
    call cont_QA(nsync, wf16(:,83), wf8(:,68), A(:,115), n3(:,352), t3x128(:,:,115), nhel, den(231))
    call cont_QA(nsync, wf16(:,84), wf8(:,70), A(:,116), n3(:,353), t3x128(:,:,116), nhel, den(235))
    call cont_QA(nsync, wf16(:,52), wf8(:,62), A(:,117), n3(:,354), t3x128(:,:,117), nhel, den(236))
    call cont_VV(nsync, wf8(:,32), wf16(:,82), A(:,118), n3(:,355), t3x128(:,:,118), nhel, den(237))
    call cont_QA(nsync, wf8(:,68), wf16(:,85), A(:,119), n3(:,356), t3x128(:,:,119), nhel, den(238))
    call cont_QA(nsync, wf8(:,48), wf16(:,76), A(:,120), n3(:,357), t3x128(:,:,120), nhel, den(239))
    call cont_VV(nsync, wf8(:,53), wf16(:,86), A(:,121), n3(:,358), t3x128(:,:,121), nhel, den(241))
    call cont_QA(nsync, wf8(:,22), wf16(:,78), A(:,122), n3(:,359), t3x128(:,:,122), nhel, den(242))
    call cont_QA(nsync, wf8(:,68), wf16(:,87), A(:,123), n3(:,360), t3x128(:,:,123), nhel, den(244))
    call cont_QA(nsync, wf16(:,84), wf8(:,72), A(:,124), n3(:,361), t3x128(:,:,124), nhel, den(246))
    call cont_QA(nsync, wf16(:,16), wf8(:,66), A(:,125), n3(:,362), t3x128(:,:,125), nhel, den(247))
    call cont_VV(nsync, wf8(:,56), wf16(:,86), A(:,126), n3(:,363), t3x128(:,:,126), nhel, den(248))
    call cont_QA(nsync, wf8(:,68), wf16(:,88), A(:,127), n3(:,364), t3x128(:,:,127), nhel, den(249))
    call cont_QA(nsync, wf8(:,22), wf16(:,81), A(:,128), n3(:,365), t3x128(:,:,128), nhel, den(250))
    call cont_VV(nsync, wf8(:,53), wf16(:,89), A(:,129), n3(:,366), t3x128(:,:,129), nhel, den(251))
    call cont_QA(nsync, wf8(:,58), wf16(:,90), A(:,130), n3(:,367), t3x128(:,:,130), nhel, den(252))
    call cont_VV(nsync, wf8(:,41), wf16(:,91), A(:,131), n3(:,368), t3x128(:,:,131), nhel, den(253))
    call cont_VV(nsync, wf8(:,41), wf16(:,92), A(:,132), n3(:,369), t3x128(:,:,132), nhel, den(254))
    call cont_VV(nsync, wf8(:,11), wf16(:,93), A(:,133), n3(:,370), t3x128(:,:,133), nhel, den(255))
    call cont_VV(nsync, wf8(:,27), wf16(:,94), A(:,134), n3(:,371), t3x128(:,:,134), nhel, den(256))
    call cont_QA(nsync, wf8(:,58), wf16(:,95), A(:,135), n3(:,372), t3x128(:,:,135), nhel, den(257))
    call cont_VV(nsync, wf8(:,11), wf16(:,96), A(:,136), n3(:,373), t3x128(:,:,136), nhel, den(258))
    call cont_QA(nsync, wf8(:,58), wf16(:,97), A(:,137), n3(:,374), t3x128(:,:,137), nhel, den(259))
    call cont_QA(nsync, wf8(:,58), wf16(:,98), A(:,138), n3(:,375), t3x128(:,:,138), nhel, den(260))
    call cont_VV(nsync, wf8(:,32), wf16(:,94), A(:,139), n3(:,376), t3x128(:,:,139), nhel, den(261))
    call cont_QA(nsync, wf16(:,67), wf8(:,62), A(:,140), n3(:,377), t3x128(:,:,140), nhel, den(262))
    call cont_QA(nsync, wf8(:,68), wf16(:,99), A(:,141), n3(:,378), t3x128(:,:,141), nhel, den(263))
    call cont_QA(nsync, wf8(:,68), wf16(:,100), A(:,142), n3(:,379), t3x128(:,:,142), nhel, den(264))
    call cont_VV(nsync, wf8(:,18), wf16(:,93), A(:,143), n3(:,380), t3x128(:,:,143), nhel, den(265))
    call cont_QA(nsync, wf16(:,70), wf8(:,62), A(:,144), n3(:,381), t3x128(:,:,144), nhel, den(266))
    call cont_QA(nsync, wf16(:,14), wf8(:,74), A(:,145), n3(:,382), t3x128(:,:,145), nhel, den(268))
    call cont_QA(nsync, wf16(:,38), wf8(:,76), A(:,146), n3(:,383), t3x128(:,:,146), nhel, den(270))
    call cont_VV(nsync, wf8(:,77), wf16(:,101), A(:,147), n3(:,384), t3x128(:,:,147), nhel, den(274))
    call cont_QA(nsync, wf8(:,38), wf16(:,102), A(:,148), n3(:,385), t3x128(:,:,148), nhel, den(277))
    call cont_QA(nsync, wf8(:,25), wf16(:,103), A(:,149), n3(:,386), t3x128(:,:,149), nhel, den(279))
    call cont_VV(nsync, wf16(:,101), wf8(:,78), A(:,150), n3(:,387), t3x128(:,:,150), nhel, den(281))
    call cont_QA(nsync, wf16(:,42), wf8(:,76), A(:,151), n3(:,388), t3x128(:,:,151), nhel, den(282))
    call cont_QA(nsync, wf16(:,18), wf8(:,74), A(:,152), n3(:,389), t3x128(:,:,152), nhel, den(283))
    call cont_QA(nsync, wf16(:,19), wf8(:,74), A(:,153), n3(:,390), t3x128(:,:,153), nhel, den(284))
    call cont_VV(nsync, wf8(:,79), wf16(:,104), A(:,154), n3(:,391), t3x128(:,:,154), nhel, den(288))
    call cont_QA(nsync, wf16(:,73), wf8(:,81), A(:,155), n3(:,392), t3x128(:,:,155), nhel, den(290))
    call cont_QA(nsync, wf8(:,60), wf16(:,105), A(:,156), n3(:,393), t3x128(:,:,156), nhel, den(292))
    call cont_QA(nsync, wf8(:,31), wf16(:,103), A(:,157), n3(:,394), t3x128(:,:,157), nhel, den(293))
    call cont_VV(nsync, wf16(:,104), wf8(:,82), A(:,158), n3(:,395), t3x128(:,:,158), nhel, den(295))
    call cont_QA(nsync, wf16(:,75), wf8(:,81), A(:,159), n3(:,396), t3x128(:,:,159), nhel, den(296))
    call cont_QA(nsync, wf16(:,22), wf8(:,74), A(:,160), n3(:,397), t3x128(:,:,160), nhel, den(297))
    call cont_VV(nsync, wf8(:,79), wf16(:,106), A(:,161), n3(:,398), t3x128(:,:,161), nhel, den(298))
    call cont_VV(nsync, wf8(:,11), wf16(:,107), A(:,162), n3(:,399), t3x128(:,:,162), nhel, den(299))
    call cont_VV(nsync, wf8(:,11), wf16(:,108), A(:,163), n3(:,400), t3x128(:,:,163), nhel, den(300))
    call cont_QA(nsync, wf8(:,34), wf16(:,109), A(:,164), n3(:,401), t3x128(:,:,164), nhel, den(301))
    call cont_VV(nsync, wf8(:,5), wf16(:,110), A(:,165), n3(:,402), t3x128(:,:,165), nhel, den(302))
    call cont_VV(nsync, wf8(:,77), wf16(:,111), A(:,166), n3(:,403), t3x128(:,:,166), nhel, den(303))
    call cont_VV(nsync, wf8(:,5), wf16(:,112), A(:,167), n3(:,404), t3x128(:,:,167), nhel, den(304))
    call cont_QA(nsync, wf8(:,34), wf16(:,113), A(:,168), n3(:,405), t3x128(:,:,168), nhel, den(305))
    call cont_VV(nsync, wf8(:,78), wf16(:,111), A(:,169), n3(:,406), t3x128(:,:,169), nhel, den(306))
    call cont_QA(nsync, wf8(:,38), wf16(:,114), A(:,170), n3(:,407), t3x128(:,:,170), nhel, den(307))
    call cont_QA(nsync, wf8(:,34), wf16(:,115), A(:,171), n3(:,408), t3x128(:,:,171), nhel, den(308))
    call cont_QA(nsync, wf8(:,34), wf16(:,116), A(:,172), n3(:,409), t3x128(:,:,172), nhel, den(309))
    call cont_VV(nsync, wf8(:,10), wf16(:,110), A(:,173), n3(:,410), t3x128(:,:,173), nhel, den(310))
    call cont_QA(nsync, wf8(:,38), wf16(:,117), A(:,174), n3(:,411), t3x128(:,:,174), nhel, den(311))
    call cont_QA(nsync, wf16(:,35), wf8(:,74), A(:,175), n3(:,412), t3x128(:,:,175), nhel, den(312))
    call cont_QA(nsync, wf16(:,36), wf8(:,74), A(:,176), n3(:,413), t3x128(:,:,176), nhel, den(313))
    call cont_QA(nsync, wf16(:,1), wf8(:,84), A(:,177), n3(:,414), t3x128(:,:,177), nhel, den(315))
    call cont_QA(nsync, wf16(:,49), wf8(:,86), A(:,178), n3(:,415), t3x128(:,:,178), nhel, den(317))
    call cont_VV(nsync, wf8(:,77), wf16(:,118), A(:,179), n3(:,416), t3x128(:,:,179), nhel, den(319))
    call cont_QA(nsync, wf8(:,52), wf16(:,119), A(:,180), n3(:,417), t3x128(:,:,180), nhel, den(321))
    call cont_QA(nsync, wf8(:,9), wf16(:,120), A(:,181), n3(:,418), t3x128(:,:,181), nhel, den(323))
    call cont_VV(nsync, wf8(:,78), wf16(:,118), A(:,182), n3(:,419), t3x128(:,:,182), nhel, den(324))
    call cont_QA(nsync, wf16(:,53), wf8(:,86), A(:,183), n3(:,420), t3x128(:,:,183), nhel, den(325))
    call cont_QA(nsync, wf16(:,7), wf8(:,84), A(:,184), n3(:,421), t3x128(:,:,184), nhel, den(326))
    call cont_VV(nsync, wf8(:,87), wf16(:,121), A(:,185), n3(:,422), t3x128(:,:,185), nhel, den(330))
    call cont_QA(nsync, wf16(:,55), wf8(:,86), A(:,186), n3(:,423), t3x128(:,:,186), nhel, den(331))
    call cont_QA(nsync, wf16(:,79), wf8(:,81), A(:,187), n3(:,424), t3x128(:,:,187), nhel, den(332))
    call cont_QA(nsync, wf8(:,64), wf16(:,105), A(:,188), n3(:,425), t3x128(:,:,188), nhel, den(333))
    call cont_QA(nsync, wf8(:,55), wf16(:,119), A(:,189), n3(:,426), t3x128(:,:,189), nhel, den(334))
    call cont_VV(nsync, wf16(:,121), wf8(:,88), A(:,190), n3(:,427), t3x128(:,:,190), nhel, den(336))
    call cont_QA(nsync, wf16(:,80), wf8(:,81), A(:,191), n3(:,428), t3x128(:,:,191), nhel, den(337))
    call cont_QA(nsync, wf16(:,56), wf8(:,86), A(:,192), n3(:,429), t3x128(:,:,192), nhel, den(338))
    call cont_VV(nsync, wf8(:,87), wf16(:,122), A(:,193), n3(:,430), t3x128(:,:,193), nhel, den(339))
    call cont_QA(nsync, wf8(:,4), wf16(:,123), A(:,194), n3(:,431), t3x128(:,:,194), nhel, den(340))
    call cont_VV(nsync, wf8(:,41), wf16(:,124), A(:,195), n3(:,432), t3x128(:,:,195), nhel, den(341))
    call cont_VV(nsync, wf8(:,41), wf16(:,125), A(:,196), n3(:,433), t3x128(:,:,196), nhel, den(342))
    call cont_VV(nsync, wf8(:,5), wf16(:,126), A(:,197), n3(:,434), t3x128(:,:,197), nhel, den(343))
    call cont_VV(nsync, wf8(:,77), wf16(:,127), A(:,198), n3(:,435), t3x128(:,:,198), nhel, den(344))
    call cont_VV(nsync, wf8(:,5), wf16(:,128), A(:,199), n3(:,436), t3x128(:,:,199), nhel, den(345))
    call cont_QA(nsync, wf8(:,4), wf16(:,129), A(:,200), n3(:,437), t3x128(:,:,200), nhel, den(346))
    call cont_VV(nsync, wf8(:,78), wf16(:,127), A(:,201), n3(:,438), t3x128(:,:,201), nhel, den(347))
    call cont_QA(nsync, wf8(:,4), wf16(:,130), A(:,202), n3(:,439), t3x128(:,:,202), nhel, den(348))
    call cont_QA(nsync, wf8(:,4), wf16(:,131), A(:,203), n3(:,440), t3x128(:,:,203), nhel, den(349))
    call cont_QA(nsync, wf8(:,9), wf16(:,132), A(:,204), n3(:,441), t3x128(:,:,204), nhel, den(350))
    call cont_VV(nsync, wf8(:,10), wf16(:,126), A(:,205), n3(:,442), t3x128(:,:,205), nhel, den(351))
    call cont_QA(nsync, wf16(:,68), wf8(:,86), A(:,206), n3(:,443), t3x128(:,:,206), nhel, den(352))
    call cont_QA(nsync, wf16(:,69), wf8(:,86), A(:,207), n3(:,444), t3x128(:,:,207), nhel, den(353))
    call cont_QA(nsync, wf8(:,9), wf16(:,133), A(:,208), n3(:,445), t3x128(:,:,208), nhel, den(354))
    call cont_QA(nsync, wf16(:,8), wf8(:,84), A(:,209), n3(:,446), t3x128(:,:,209), nhel, den(355))
    call cont_VV(nsync, wf8(:,79), wf16(:,134), A(:,210), n3(:,447), t3x128(:,:,210), nhel, den(357))
    call cont_QA(nsync, wf16(:,83), wf8(:,90), A(:,211), n3(:,448), t3x128(:,:,211), nhel, den(359))
    call cont_QA(nsync, wf8(:,70), wf16(:,135), A(:,212), n3(:,449), t3x128(:,:,212), nhel, den(361))
    call cont_QA(nsync, wf8(:,17), wf16(:,120), A(:,213), n3(:,450), t3x128(:,:,213), nhel, den(362))
    call cont_VV(nsync, wf8(:,82), wf16(:,134), A(:,214), n3(:,451), t3x128(:,:,214), nhel, den(363))
    call cont_QA(nsync, wf16(:,85), wf8(:,90), A(:,215), n3(:,452), t3x128(:,:,215), nhel, den(364))
    call cont_QA(nsync, wf16(:,13), wf8(:,84), A(:,216), n3(:,453), t3x128(:,:,216), nhel, den(365))
    call cont_VV(nsync, wf8(:,87), wf16(:,136), A(:,217), n3(:,454), t3x128(:,:,217), nhel, den(367))
    call cont_QA(nsync, wf16(:,45), wf8(:,76), A(:,218), n3(:,455), t3x128(:,:,218), nhel, den(368))
    call cont_QA(nsync, wf16(:,87), wf8(:,90), A(:,219), n3(:,456), t3x128(:,:,219), nhel, den(369))
    call cont_QA(nsync, wf8(:,72), wf16(:,135), A(:,220), n3(:,457), t3x128(:,:,220), nhel, den(370))
    call cont_QA(nsync, wf8(:,45), wf16(:,102), A(:,221), n3(:,458), t3x128(:,:,221), nhel, den(371))
    call cont_VV(nsync, wf8(:,88), wf16(:,136), A(:,222), n3(:,459), t3x128(:,:,222), nhel, den(372))
    call cont_QA(nsync, wf16(:,88), wf8(:,90), A(:,223), n3(:,460), t3x128(:,:,223), nhel, den(373))
    call cont_QA(nsync, wf16(:,48), wf8(:,76), A(:,224), n3(:,461), t3x128(:,:,224), nhel, den(374))
    call cont_VV(nsync, wf8(:,87), wf16(:,137), A(:,225), n3(:,462), t3x128(:,:,225), nhel, den(375))
    call cont_QA(nsync, wf8(:,13), wf16(:,138), A(:,226), n3(:,463), t3x128(:,:,226), nhel, den(376))
    call cont_VV(nsync, wf8(:,41), wf16(:,139), A(:,227), n3(:,464), t3x128(:,:,227), nhel, den(377))
    call cont_VV(nsync, wf8(:,41), wf16(:,140), A(:,228), n3(:,465), t3x128(:,:,228), nhel, den(378))
    call cont_VV(nsync, wf8(:,11), wf16(:,141), A(:,229), n3(:,466), t3x128(:,:,229), nhel, den(379))
    call cont_VV(nsync, wf8(:,79), wf16(:,142), A(:,230), n3(:,467), t3x128(:,:,230), nhel, den(380))
    call cont_QA(nsync, wf8(:,13), wf16(:,143), A(:,231), n3(:,468), t3x128(:,:,231), nhel, den(381))
    call cont_VV(nsync, wf8(:,11), wf16(:,144), A(:,232), n3(:,469), t3x128(:,:,232), nhel, den(382))
    call cont_QA(nsync, wf8(:,13), wf16(:,145), A(:,233), n3(:,470), t3x128(:,:,233), nhel, den(383))
    call cont_QA(nsync, wf8(:,13), wf16(:,146), A(:,234), n3(:,471), t3x128(:,:,234), nhel, den(384))
    call cont_VV(nsync, wf8(:,82), wf16(:,142), A(:,235), n3(:,472), t3x128(:,:,235), nhel, den(385))
    call cont_QA(nsync, wf8(:,17), wf16(:,132), A(:,236), n3(:,473), t3x128(:,:,236), nhel, den(386))
    call cont_QA(nsync, wf16(:,99), wf8(:,90), A(:,237), n3(:,474), t3x128(:,:,237), nhel, den(387))
    call cont_QA(nsync, wf16(:,100), wf8(:,90), A(:,238), n3(:,475), t3x128(:,:,238), nhel, den(388))
    call cont_VV(nsync, wf8(:,18), wf16(:,141), A(:,239), n3(:,476), t3x128(:,:,239), nhel, den(389))
    call cont_QA(nsync, wf8(:,17), wf16(:,133), A(:,240), n3(:,477), t3x128(:,:,240), nhel, den(390))
    call cont_VV(nsync, wf8(:,79), wf16(:,147), A(:,241), n3(:,478), t3x128(:,:,241), nhel, den(391))
    call cont_VV(nsync, wf8(:,27), wf16(:,148), A(:,242), n3(:,479), t3x128(:,:,242), nhel, den(392))
    call cont_VV(nsync, wf8(:,27), wf16(:,149), A(:,243), n3(:,480), t3x128(:,:,243), nhel, den(393))
    call cont_QA(nsync, wf8(:,48), wf16(:,109), A(:,244), n3(:,481), t3x128(:,:,244), nhel, den(394))
    call cont_VV(nsync, wf8(:,23), wf16(:,150), A(:,245), n3(:,482), t3x128(:,:,245), nhel, den(395))
    call cont_VV(nsync, wf8(:,77), wf16(:,151), A(:,246), n3(:,483), t3x128(:,:,246), nhel, den(396))
    call cont_VV(nsync, wf8(:,23), wf16(:,152), A(:,247), n3(:,484), t3x128(:,:,247), nhel, den(397))
    call cont_QA(nsync, wf8(:,48), wf16(:,113), A(:,248), n3(:,485), t3x128(:,:,248), nhel, den(398))
    call cont_VV(nsync, wf8(:,78), wf16(:,151), A(:,249), n3(:,486), t3x128(:,:,249), nhel, den(399))
    call cont_QA(nsync, wf8(:,52), wf16(:,153), A(:,250), n3(:,487), t3x128(:,:,250), nhel, den(400))
    call cont_QA(nsync, wf8(:,48), wf16(:,115), A(:,251), n3(:,488), t3x128(:,:,251), nhel, den(401))
    call cont_QA(nsync, wf8(:,48), wf16(:,116), A(:,252), n3(:,489), t3x128(:,:,252), nhel, den(402))
    call cont_VV(nsync, wf8(:,26), wf16(:,150), A(:,253), n3(:,490), t3x128(:,:,253), nhel, den(403))
    call cont_QA(nsync, wf8(:,52), wf16(:,154), A(:,254), n3(:,491), t3x128(:,:,254), nhel, den(404))
    call cont_QA(nsync, wf16(:,33), wf8(:,84), A(:,255), n3(:,492), t3x128(:,:,255), nhel, den(405))
    call cont_QA(nsync, wf16(:,32), wf8(:,84), A(:,256), n3(:,493), t3x128(:,:,256), nhel, den(406))
    call cont_VV(nsync, wf8(:,87), wf16(:,155), A(:,257), n3(:,494), t3x128(:,:,257), nhel, den(407))
    call cont_QA(nsync, wf8(:,22), wf16(:,123), A(:,258), n3(:,495), t3x128(:,:,258), nhel, den(408))
    call cont_VV(nsync, wf8(:,53), wf16(:,156), A(:,259), n3(:,496), t3x128(:,:,259), nhel, den(409))
    call cont_VV(nsync, wf8(:,53), wf16(:,157), A(:,260), n3(:,497), t3x128(:,:,260), nhel, den(410))
    call cont_VV(nsync, wf8(:,23), wf16(:,158), A(:,261), n3(:,498), t3x128(:,:,261), nhel, den(411))
    call cont_VV(nsync, wf8(:,77), wf16(:,159), A(:,262), n3(:,499), t3x128(:,:,262), nhel, den(412))
    call cont_VV(nsync, wf8(:,23), wf16(:,160), A(:,263), n3(:,500), t3x128(:,:,263), nhel, den(413))
    call cont_QA(nsync, wf8(:,22), wf16(:,129), A(:,264), n3(:,501), t3x128(:,:,264), nhel, den(414))
    call cont_VV(nsync, wf8(:,78), wf16(:,159), A(:,265), n3(:,502), t3x128(:,:,265), nhel, den(415))
    call cont_QA(nsync, wf8(:,22), wf16(:,130), A(:,266), n3(:,503), t3x128(:,:,266), nhel, den(416))
    call cont_QA(nsync, wf8(:,22), wf16(:,131), A(:,267), n3(:,504), t3x128(:,:,267), nhel, den(417))
    call cont_QA(nsync, wf8(:,25), wf16(:,161), A(:,268), n3(:,505), t3x128(:,:,268), nhel, den(418))
    call cont_VV(nsync, wf8(:,26), wf16(:,158), A(:,269), n3(:,506), t3x128(:,:,269), nhel, den(419))
    call cont_QA(nsync, wf16(:,66), wf8(:,76), A(:,270), n3(:,507), t3x128(:,:,270), nhel, den(420))
    call cont_QA(nsync, wf16(:,65), wf8(:,76), A(:,271), n3(:,508), t3x128(:,:,271), nhel, den(421))
    call cont_QA(nsync, wf8(:,25), wf16(:,162), A(:,272), n3(:,509), t3x128(:,:,272), nhel, den(422))
    call cont_VV(nsync, wf8(:,87), wf16(:,163), A(:,273), n3(:,510), t3x128(:,:,273), nhel, den(423))
    call cont_QA(nsync, wf8(:,29), wf16(:,138), A(:,274), n3(:,511), t3x128(:,:,274), nhel, den(424))
    call cont_VV(nsync, wf8(:,53), wf16(:,164), A(:,275), n3(:,512), t3x128(:,:,275), nhel, den(425))
    call cont_VV(nsync, wf8(:,53), wf16(:,165), A(:,276), n3(:,513), t3x128(:,:,276), nhel, den(426))
    call cont_VV(nsync, wf8(:,27), wf16(:,166), A(:,277), n3(:,514), t3x128(:,:,277), nhel, den(427))
    call cont_VV(nsync, wf8(:,79), wf16(:,167), A(:,278), n3(:,515), t3x128(:,:,278), nhel, den(428))
    call cont_QA(nsync, wf8(:,29), wf16(:,143), A(:,279), n3(:,516), t3x128(:,:,279), nhel, den(429))
    call cont_VV(nsync, wf8(:,27), wf16(:,168), A(:,280), n3(:,517), t3x128(:,:,280), nhel, den(430))
    call cont_QA(nsync, wf8(:,29), wf16(:,145), A(:,281), n3(:,518), t3x128(:,:,281), nhel, den(431))
    call cont_QA(nsync, wf8(:,29), wf16(:,146), A(:,282), n3(:,519), t3x128(:,:,282), nhel, den(432))
    call cont_VV(nsync, wf8(:,82), wf16(:,167), A(:,283), n3(:,520), t3x128(:,:,283), nhel, den(433))
    call cont_QA(nsync, wf8(:,31), wf16(:,161), A(:,284), n3(:,521), t3x128(:,:,284), nhel, den(434))
    call cont_QA(nsync, wf16(:,98), wf8(:,81), A(:,285), n3(:,522), t3x128(:,:,285), nhel, den(435))
    call cont_QA(nsync, wf16(:,97), wf8(:,81), A(:,286), n3(:,523), t3x128(:,:,286), nhel, den(436))
    call cont_VV(nsync, wf8(:,32), wf16(:,166), A(:,287), n3(:,524), t3x128(:,:,287), nhel, den(437))
    call cont_QA(nsync, wf8(:,31), wf16(:,162), A(:,288), n3(:,525), t3x128(:,:,288), nhel, den(438))

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
  complex(REALKIND), intent(out) :: M1(6) ! M1helarray(6,128)
  integer :: empty(0)

  M1(1) = ((-A(j,106)%j-A(j,107)%j-A(j,108)%j-A(j,109)%j-A(j,111)%j-A(j,112)%j-A(j,113)%j-A(j,115)%j-A(j,116)%j-A(j,117)%j &
       -A(j,119)%j-A(j,120)%j-A(j,131)%j-A(j,132)%j-A(j,134)%j-A(j,135)%j-A(j,138)%j-A(j,139)%j-A(j,142)%j-A(j,144)%j-A(j,177)%j &
       -A(j,178)%j-A(j,180)%j-A(j,181)%j-A(j,183)%j-A(j,184)%j-A(j,195)%j-A(j,196)%j-A(j,198)%j-A(j,200)%j-A(j,201)%j-A(j,203)%j &
       -A(j,207)%j-A(j,208)%j-A(j,242)%j-A(j,243)%j-A(j,246)%j-A(j,248)%j-A(j,249)%j-A(j,252)%j-A(j,254)%j &
       -A(j,256)%j)*f(1))/36._/**/REALKIND+((-A(j,9)%j-A(j,13)%j-A(j,16)%j-A(j,18)%j-A(j,20)%j-A(j,23)%j-A(j,34)%j-A(j,40)%j &
       -A(j,44)%j-A(j,46)%j-A(j,49)%j-A(j,53)%j-A(j,56)%j-A(j,74)%j-A(j,77)%j-A(j,80)%j-A(j,81)%j-A(j,87)%j-A(j,92)%j-A(j,94)%j &
       -A(j,155)%j-A(j,156)%j-A(j,159)%j-A(j,164)%j-A(j,165)%j-A(j,171)%j-A(j,173)%j-A(j,219)%j-A(j,220)%j-A(j,223)%j-A(j,225)%j &
       -A(j,232)%j-A(j,236)%j-A(j,237)%j-A(j,258)%j-A(j,261)%j-A(j,266)%j-A(j,269)%j-A(j,276)%j-A(j,278)%j-A(j,283)%j &
       -A(j,286)%j)*f(1))/4._/**/REALKIND+((-A(j,1)%j-A(j,2)%j-A(j,4)%j-A(j,5)%j-A(j,7)%j-A(j,8)%j-A(j,33)%j-A(j,36)%j-A(j,42)%j &
       -A(j,43)%j-A(j,65)%j-A(j,66)%j-A(j,68)%j-A(j,69)%j-A(j,71)%j-A(j,72)%j-A(j,83)%j-A(j,84)%j-A(j,95)%j-A(j,96)%j-A(j,97)%j &
       -A(j,99)%j-A(j,100)%j-A(j,101)%j-A(j,103)%j-A(j,104)%j-A(j,122)%j-A(j,123)%j-A(j,124)%j-A(j,125)%j-A(j,127)%j-A(j,128)%j &
       -A(j,129)%j-A(j,130)%j-A(j,133)%j-A(j,136)%j-A(j,137)%j-A(j,140)%j-A(j,141)%j-A(j,143)%j-A(j,166)%j-A(j,168)%j-A(j,169)%j &
       -A(j,172)%j-A(j,186)%j-A(j,187)%j-A(j,188)%j-A(j,189)%j-A(j,191)%j-A(j,192)%j-A(j,193)%j-A(j,194)%j-A(j,197)%j-A(j,199)%j &
       -A(j,202)%j-A(j,204)%j-A(j,205)%j-A(j,206)%j-A(j,209)%j-A(j,211)%j-A(j,212)%j-A(j,213)%j-A(j,215)%j-A(j,216)%j-A(j,227)%j &
       -A(j,228)%j-A(j,238)%j-A(j,240)%j-A(j,241)%j-A(j,244)%j-A(j,245)%j-A(j,247)%j-A(j,250)%j-A(j,251)%j-A(j,253)%j-A(j,255)%j &
       -A(j,262)%j-A(j,264)%j-A(j,265)%j-A(j,267)%j-A(j,277)%j-A(j,280)%j-A(j,285)%j-A(j,287)%j)*f(1))/12._/**/REALKIND+(CI*( &
       -A(j,10)%j-A(j,14)%j+A(j,19)%j+A(j,22)%j+A(j,51)%j+A(j,54)%j-A(j,73)%j-A(j,78)%j+A(j,154)%j+A(j,158)%j-A(j,217)%j &
       -A(j,222)%j)*f(2))/4._/**/REALKIND
  M1(2) = ((A(j,58)%j+A(j,59)%j+A(j,60)%j+A(j,61)%j+A(j,63)%j+A(j,64)%j+A(j,65)%j+A(j,66)%j+A(j,68)%j+A(j,69)%j+A(j,71)%j &
       +A(j,72)%j+A(j,83)%j+A(j,84)%j+A(j,86)%j+A(j,88)%j+A(j,89)%j+A(j,91)%j+A(j,95)%j+A(j,96)%j+A(j,209)%j+A(j,211)%j+A(j,212)%j &
       +A(j,213)%j+A(j,215)%j+A(j,216)%j+A(j,227)%j+A(j,228)%j+A(j,230)%j+A(j,231)%j+A(j,234)%j+A(j,235)%j+A(j,238)%j+A(j,240)%j &
       +A(j,241)%j+A(j,244)%j+A(j,245)%j+A(j,247)%j+A(j,250)%j+A(j,251)%j+A(j,253)%j+A(j,255)%j)*f(1))/36._/**/REALKIND+((A(j,9)%j &
       +A(j,11)%j+A(j,12)%j+A(j,13)%j+A(j,15)%j+A(j,16)%j+A(j,38)%j+A(j,40)%j+A(j,41)%j+A(j,44)%j+A(j,49)%j+A(j,50)%j+A(j,52)%j &
       +A(j,53)%j+A(j,55)%j+A(j,56)%j+A(j,74)%j+A(j,75)%j+A(j,76)%j+A(j,77)%j+A(j,79)%j+A(j,80)%j+A(j,81)%j+A(j,82)%j+A(j,85)%j &
       +A(j,87)%j+A(j,90)%j+A(j,92)%j+A(j,93)%j+A(j,94)%j+A(j,113)%j+A(j,115)%j+A(j,116)%j+A(j,117)%j+A(j,119)%j+A(j,120)%j &
       +A(j,131)%j+A(j,132)%j+A(j,142)%j+A(j,144)%j+A(j,161)%j+A(j,164)%j+A(j,170)%j+A(j,171)%j+A(j,177)%j+A(j,178)%j+A(j,180)%j &
       +A(j,181)%j+A(j,183)%j+A(j,184)%j+A(j,195)%j+A(j,196)%j+A(j,207)%j+A(j,208)%j+A(j,218)%j+A(j,219)%j+A(j,220)%j+A(j,221)%j &
       +A(j,223)%j+A(j,224)%j+A(j,225)%j+A(j,226)%j+A(j,229)%j+A(j,232)%j+A(j,233)%j+A(j,236)%j+A(j,237)%j+A(j,239)%j+A(j,242)%j &
       +A(j,243)%j+A(j,246)%j+A(j,248)%j+A(j,249)%j+A(j,252)%j+A(j,254)%j+A(j,256)%j+A(j,261)%j+A(j,263)%j+A(j,269)%j+A(j,270)%j &
       +A(j,278)%j+A(j,279)%j+A(j,282)%j+A(j,283)%j)*f(1))/12._/**/REALKIND+((A(j,1)%j+A(j,5)%j+A(j,8)%j+A(j,27)%j+A(j,28)%j &
       +A(j,31)%j+A(j,36)%j+A(j,37)%j+A(j,43)%j+A(j,45)%j+A(j,97)%j+A(j,101)%j+A(j,104)%j+A(j,123)%j+A(j,124)%j+A(j,127)%j &
       +A(j,129)%j+A(j,136)%j+A(j,140)%j+A(j,141)%j+A(j,146)%j+A(j,148)%j+A(j,151)%j+A(j,162)%j+A(j,168)%j+A(j,172)%j+A(j,174)%j &
       +A(j,186)%j+A(j,189)%j+A(j,192)%j+A(j,193)%j+A(j,199)%j+A(j,204)%j+A(j,206)%j+A(j,260)%j+A(j,262)%j+A(j,265)%j+A(j,271)%j &
       +A(j,274)%j+A(j,277)%j+A(j,281)%j+A(j,287)%j)*f(1))/4._/**/REALKIND+(CI*(A(j,3)%j+A(j,6)%j-A(j,26)%j-A(j,30)%j-A(j,98)%j &
       -A(j,102)%j+A(j,121)%j+A(j,126)%j-A(j,147)%j-A(j,150)%j+A(j,185)%j+A(j,190)%j)*f(2))/4._/**/REALKIND
  M1(3) = ((A(j,97)%j+A(j,99)%j+A(j,100)%j+A(j,101)%j+A(j,103)%j+A(j,104)%j+A(j,122)%j+A(j,123)%j+A(j,124)%j+A(j,125)%j+A(j,127)%j &
       +A(j,128)%j+A(j,129)%j+A(j,130)%j+A(j,133)%j+A(j,136)%j+A(j,137)%j+A(j,140)%j+A(j,141)%j+A(j,143)%j+A(j,145)%j+A(j,146)%j &
       +A(j,148)%j+A(j,149)%j+A(j,151)%j+A(j,152)%j+A(j,162)%j+A(j,163)%j+A(j,166)%j+A(j,168)%j+A(j,169)%j+A(j,172)%j+A(j,174)%j &
       +A(j,176)%j+A(j,259)%j+A(j,260)%j+A(j,262)%j+A(j,264)%j+A(j,265)%j+A(j,267)%j+A(j,271)%j+A(j,272)%j)*f(1))/36._/**/REALKIND &
       +((A(j,2)%j+A(j,4)%j+A(j,7)%j+A(j,25)%j+A(j,29)%j+A(j,32)%j+A(j,33)%j+A(j,39)%j+A(j,42)%j+A(j,47)%j+A(j,58)%j+A(j,61)%j &
       +A(j,64)%j+A(j,65)%j+A(j,69)%j+A(j,72)%j+A(j,83)%j+A(j,88)%j+A(j,91)%j+A(j,96)%j+A(j,187)%j+A(j,188)%j+A(j,191)%j &
       +A(j,194)%j+A(j,197)%j+A(j,202)%j+A(j,205)%j+A(j,211)%j+A(j,212)%j+A(j,215)%j+A(j,228)%j+A(j,230)%j+A(j,235)%j+A(j,238)%j &
       +A(j,244)%j+A(j,245)%j+A(j,251)%j+A(j,253)%j+A(j,273)%j+A(j,280)%j+A(j,284)%j+A(j,285)%j)*f(1))/4._/**/REALKIND+((A(j,17)%j &
       +A(j,18)%j+A(j,20)%j+A(j,21)%j+A(j,23)%j+A(j,24)%j+A(j,34)%j+A(j,35)%j+A(j,46)%j+A(j,48)%j+A(j,49)%j+A(j,50)%j+A(j,52)%j &
       +A(j,53)%j+A(j,55)%j+A(j,56)%j+A(j,81)%j+A(j,82)%j+A(j,90)%j+A(j,92)%j+A(j,106)%j+A(j,107)%j+A(j,108)%j+A(j,109)%j &
       +A(j,111)%j+A(j,112)%j+A(j,113)%j+A(j,115)%j+A(j,116)%j+A(j,117)%j+A(j,119)%j+A(j,120)%j+A(j,131)%j+A(j,132)%j+A(j,134)%j &
       +A(j,135)%j+A(j,138)%j+A(j,139)%j+A(j,142)%j+A(j,144)%j+A(j,153)%j+A(j,155)%j+A(j,156)%j+A(j,157)%j+A(j,159)%j+A(j,160)%j &
       +A(j,161)%j+A(j,164)%j+A(j,165)%j+A(j,167)%j+A(j,170)%j+A(j,171)%j+A(j,173)%j+A(j,175)%j+A(j,198)%j+A(j,200)%j+A(j,201)%j &
       +A(j,203)%j+A(j,218)%j+A(j,219)%j+A(j,220)%j+A(j,221)%j+A(j,223)%j+A(j,224)%j+A(j,229)%j+A(j,232)%j+A(j,237)%j+A(j,239)%j &
       +A(j,246)%j+A(j,248)%j+A(j,249)%j+A(j,252)%j+A(j,257)%j+A(j,258)%j+A(j,261)%j+A(j,263)%j+A(j,266)%j+A(j,268)%j+A(j,269)%j &
       +A(j,270)%j+A(j,275)%j+A(j,276)%j+A(j,286)%j+A(j,288)%j)*f(1))/12._/**/REALKIND+(CI*(-A(j,3)%j-A(j,6)%j+A(j,26)%j+A(j,30)%j &
       +A(j,57)%j+A(j,62)%j-A(j,67)%j-A(j,70)%j-A(j,185)%j-A(j,190)%j+A(j,210)%j+A(j,214)%j)*f(2))/4._/**/REALKIND
  M1(4) = ((-A(j,9)%j-A(j,11)%j-A(j,12)%j-A(j,13)%j-A(j,15)%j-A(j,16)%j-A(j,17)%j-A(j,18)%j-A(j,20)%j-A(j,21)%j-A(j,23)%j &
       -A(j,24)%j-A(j,34)%j-A(j,35)%j-A(j,38)%j-A(j,40)%j-A(j,41)%j-A(j,44)%j-A(j,46)%j-A(j,48)%j-A(j,218)%j-A(j,219)%j-A(j,220)%j &
       -A(j,221)%j-A(j,223)%j-A(j,224)%j-A(j,225)%j-A(j,226)%j-A(j,229)%j-A(j,232)%j-A(j,233)%j-A(j,236)%j-A(j,237)%j-A(j,239)%j &
       -A(j,257)%j-A(j,258)%j-A(j,261)%j-A(j,263)%j-A(j,266)%j-A(j,268)%j-A(j,269)%j-A(j,270)%j)*f(1))/36._/**/REALKIND+(( &
       -A(j,1)%j-A(j,2)%j-A(j,4)%j-A(j,5)%j-A(j,7)%j-A(j,8)%j-A(j,25)%j-A(j,27)%j-A(j,28)%j-A(j,29)%j-A(j,31)%j-A(j,32)%j &
       -A(j,33)%j-A(j,36)%j-A(j,37)%j-A(j,39)%j-A(j,42)%j-A(j,43)%j-A(j,45)%j-A(j,47)%j-A(j,58)%j-A(j,59)%j-A(j,60)%j-A(j,61)%j &
       -A(j,63)%j-A(j,64)%j-A(j,86)%j-A(j,88)%j-A(j,89)%j-A(j,91)%j-A(j,122)%j-A(j,123)%j-A(j,124)%j-A(j,125)%j-A(j,127)%j &
       -A(j,128)%j-A(j,133)%j-A(j,136)%j-A(j,141)%j-A(j,143)%j-A(j,145)%j-A(j,146)%j-A(j,148)%j-A(j,149)%j-A(j,151)%j-A(j,152)%j &
       -A(j,162)%j-A(j,163)%j-A(j,174)%j-A(j,176)%j-A(j,193)%j-A(j,194)%j-A(j,202)%j-A(j,204)%j-A(j,209)%j-A(j,211)%j-A(j,212)%j &
       -A(j,213)%j-A(j,215)%j-A(j,216)%j-A(j,227)%j-A(j,228)%j-A(j,230)%j-A(j,231)%j-A(j,234)%j-A(j,235)%j-A(j,238)%j-A(j,240)%j &
       -A(j,245)%j-A(j,247)%j-A(j,253)%j-A(j,255)%j-A(j,259)%j-A(j,260)%j-A(j,262)%j-A(j,264)%j-A(j,265)%j-A(j,267)%j-A(j,271)%j &
       -A(j,272)%j-A(j,273)%j-A(j,274)%j-A(j,281)%j-A(j,284)%j)*f(1))/12._/**/REALKIND+((-A(j,50)%j-A(j,52)%j-A(j,55)%j-A(j,75)%j &
       -A(j,76)%j-A(j,79)%j-A(j,82)%j-A(j,85)%j-A(j,90)%j-A(j,93)%j-A(j,106)%j-A(j,109)%j-A(j,112)%j-A(j,115)%j-A(j,116)%j &
       -A(j,119)%j-A(j,132)%j-A(j,134)%j-A(j,139)%j-A(j,142)%j-A(j,153)%j-A(j,157)%j-A(j,160)%j-A(j,161)%j-A(j,167)%j-A(j,170)%j &
       -A(j,175)%j-A(j,177)%j-A(j,181)%j-A(j,184)%j-A(j,195)%j-A(j,200)%j-A(j,203)%j-A(j,208)%j-A(j,243)%j-A(j,246)%j-A(j,249)%j &
       -A(j,256)%j-A(j,275)%j-A(j,279)%j-A(j,282)%j-A(j,288)%j)*f(1))/4._/**/REALKIND+(CI*(-A(j,51)%j-A(j,54)%j+A(j,73)%j &
       +A(j,78)%j+A(j,105)%j+A(j,110)%j-A(j,114)%j-A(j,118)%j-A(j,154)%j-A(j,158)%j+A(j,179)%j+A(j,182)%j)*f(2))/4._/**/REALKIND
  M1(5) = ((-A(j,11)%j-A(j,12)%j-A(j,15)%j-A(j,17)%j-A(j,21)%j-A(j,24)%j-A(j,35)%j-A(j,38)%j-A(j,41)%j-A(j,48)%j-A(j,107)%j &
       -A(j,108)%j-A(j,111)%j-A(j,113)%j-A(j,117)%j-A(j,120)%j-A(j,131)%j-A(j,135)%j-A(j,138)%j-A(j,144)%j-A(j,178)%j-A(j,180)%j &
       -A(j,183)%j-A(j,196)%j-A(j,198)%j-A(j,201)%j-A(j,207)%j-A(j,218)%j-A(j,221)%j-A(j,224)%j-A(j,226)%j-A(j,229)%j-A(j,233)%j &
       -A(j,239)%j-A(j,242)%j-A(j,248)%j-A(j,252)%j-A(j,254)%j-A(j,257)%j-A(j,263)%j-A(j,268)%j-A(j,270)%j)*f(1))/4._/**/REALKIND &
       +((-A(j,25)%j-A(j,27)%j-A(j,28)%j-A(j,29)%j-A(j,31)%j-A(j,32)%j-A(j,37)%j-A(j,39)%j-A(j,45)%j-A(j,47)%j-A(j,58)%j-A(j,59)%j &
       -A(j,60)%j-A(j,61)%j-A(j,63)%j-A(j,64)%j-A(j,65)%j-A(j,66)%j-A(j,68)%j-A(j,69)%j-A(j,71)%j-A(j,72)%j-A(j,83)%j-A(j,84)%j &
       -A(j,86)%j-A(j,88)%j-A(j,89)%j-A(j,91)%j-A(j,95)%j-A(j,96)%j-A(j,97)%j-A(j,99)%j-A(j,100)%j-A(j,101)%j-A(j,103)%j &
       -A(j,104)%j-A(j,129)%j-A(j,130)%j-A(j,137)%j-A(j,140)%j-A(j,145)%j-A(j,146)%j-A(j,148)%j-A(j,149)%j-A(j,151)%j-A(j,152)%j &
       -A(j,162)%j-A(j,163)%j-A(j,166)%j-A(j,168)%j-A(j,169)%j-A(j,172)%j-A(j,174)%j-A(j,176)%j-A(j,186)%j-A(j,187)%j-A(j,188)%j &
       -A(j,189)%j-A(j,191)%j-A(j,192)%j-A(j,197)%j-A(j,199)%j-A(j,205)%j-A(j,206)%j-A(j,230)%j-A(j,231)%j-A(j,234)%j-A(j,235)%j &
       -A(j,241)%j-A(j,244)%j-A(j,250)%j-A(j,251)%j-A(j,259)%j-A(j,260)%j-A(j,271)%j-A(j,272)%j-A(j,273)%j-A(j,274)%j-A(j,277)%j &
       -A(j,280)%j-A(j,281)%j-A(j,284)%j-A(j,285)%j-A(j,287)%j)*f(1))/12._/**/REALKIND+((-A(j,49)%j-A(j,50)%j-A(j,52)%j-A(j,53)%j &
       -A(j,55)%j-A(j,56)%j-A(j,74)%j-A(j,75)%j-A(j,76)%j-A(j,77)%j-A(j,79)%j-A(j,80)%j-A(j,81)%j-A(j,82)%j-A(j,85)%j-A(j,87)%j &
       -A(j,90)%j-A(j,92)%j-A(j,93)%j-A(j,94)%j-A(j,153)%j-A(j,155)%j-A(j,156)%j-A(j,157)%j-A(j,159)%j-A(j,160)%j-A(j,161)%j &
       -A(j,164)%j-A(j,165)%j-A(j,167)%j-A(j,170)%j-A(j,171)%j-A(j,173)%j-A(j,175)%j-A(j,275)%j-A(j,276)%j-A(j,278)%j-A(j,279)%j &
       -A(j,282)%j-A(j,283)%j-A(j,286)%j-A(j,288)%j)*f(1))/36._/**/REALKIND+(CI*(A(j,10)%j+A(j,14)%j-A(j,19)%j-A(j,22)%j &
       -A(j,105)%j-A(j,110)%j+A(j,114)%j+A(j,118)%j-A(j,179)%j-A(j,182)%j+A(j,217)%j+A(j,222)%j)*f(2))/4._/**/REALKIND
  M1(6) = ((A(j,59)%j+A(j,60)%j+A(j,63)%j+A(j,66)%j+A(j,68)%j+A(j,71)%j+A(j,84)%j+A(j,86)%j+A(j,89)%j+A(j,95)%j+A(j,99)%j &
       +A(j,100)%j+A(j,103)%j+A(j,122)%j+A(j,125)%j+A(j,128)%j+A(j,130)%j+A(j,133)%j+A(j,137)%j+A(j,143)%j+A(j,145)%j+A(j,149)%j &
       +A(j,152)%j+A(j,163)%j+A(j,166)%j+A(j,169)%j+A(j,176)%j+A(j,209)%j+A(j,213)%j+A(j,216)%j+A(j,227)%j+A(j,231)%j+A(j,234)%j &
       +A(j,240)%j+A(j,241)%j+A(j,247)%j+A(j,250)%j+A(j,255)%j+A(j,259)%j+A(j,264)%j+A(j,267)%j+A(j,272)%j)*f(1))/4._/**/REALKIND &
       +((A(j,1)%j+A(j,2)%j+A(j,4)%j+A(j,5)%j+A(j,7)%j+A(j,8)%j+A(j,25)%j+A(j,27)%j+A(j,28)%j+A(j,29)%j+A(j,31)%j+A(j,32)%j &
       +A(j,33)%j+A(j,36)%j+A(j,37)%j+A(j,39)%j+A(j,42)%j+A(j,43)%j+A(j,45)%j+A(j,47)%j+A(j,186)%j+A(j,187)%j+A(j,188)%j &
       +A(j,189)%j+A(j,191)%j+A(j,192)%j+A(j,193)%j+A(j,194)%j+A(j,197)%j+A(j,199)%j+A(j,202)%j+A(j,204)%j+A(j,205)%j+A(j,206)%j &
       +A(j,273)%j+A(j,274)%j+A(j,277)%j+A(j,280)%j+A(j,281)%j+A(j,284)%j+A(j,285)%j+A(j,287)%j)*f(1))/36._/**/REALKIND+((A(j,9)%j &
       +A(j,11)%j+A(j,12)%j+A(j,13)%j+A(j,15)%j+A(j,16)%j+A(j,17)%j+A(j,18)%j+A(j,20)%j+A(j,21)%j+A(j,23)%j+A(j,24)%j+A(j,34)%j &
       +A(j,35)%j+A(j,38)%j+A(j,40)%j+A(j,41)%j+A(j,44)%j+A(j,46)%j+A(j,48)%j+A(j,74)%j+A(j,75)%j+A(j,76)%j+A(j,77)%j+A(j,79)%j &
       +A(j,80)%j+A(j,85)%j+A(j,87)%j+A(j,93)%j+A(j,94)%j+A(j,106)%j+A(j,107)%j+A(j,108)%j+A(j,109)%j+A(j,111)%j+A(j,112)%j &
       +A(j,134)%j+A(j,135)%j+A(j,138)%j+A(j,139)%j+A(j,153)%j+A(j,155)%j+A(j,156)%j+A(j,157)%j+A(j,159)%j+A(j,160)%j+A(j,165)%j &
       +A(j,167)%j+A(j,173)%j+A(j,175)%j+A(j,177)%j+A(j,178)%j+A(j,180)%j+A(j,181)%j+A(j,183)%j+A(j,184)%j+A(j,195)%j+A(j,196)%j &
       +A(j,198)%j+A(j,200)%j+A(j,201)%j+A(j,203)%j+A(j,207)%j+A(j,208)%j+A(j,225)%j+A(j,226)%j+A(j,233)%j+A(j,236)%j+A(j,242)%j &
       +A(j,243)%j+A(j,254)%j+A(j,256)%j+A(j,257)%j+A(j,258)%j+A(j,266)%j+A(j,268)%j+A(j,275)%j+A(j,276)%j+A(j,278)%j+A(j,279)%j &
       +A(j,282)%j+A(j,283)%j+A(j,286)%j+A(j,288)%j)*f(1))/12._/**/REALKIND+(CI*(-A(j,57)%j-A(j,62)%j+A(j,67)%j+A(j,70)%j &
       +A(j,98)%j+A(j,102)%j-A(j,121)%j-A(j,126)%j+A(j,147)%j+A(j,150)%j-A(j,210)%j-A(j,214)%j)*f(2))/4._/**/REALKIND

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
  use ol_colourmatrix_ppajjj_bbbbxbxbxa_1_/**/REALKIND, only: K1
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
  use ol_colourmatrix_ppajjj_bbbbxbxbxa_1_/**/REALKIND, only: K1
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
  use ol_colourmatrix_ppajjj_bbbbxbxbxa_1_/**/REALKIND, only: K1
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
  & bind(c,name="ol_tree_colvect_ppajjj_bbbbxbxbxa_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(6,128)
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
    & bind(c,name="ol_f_amp2tree_ppajjj_bbbbxbxbxa_1")
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
    & bind(c,name="ol_f_amp2ccone_ppajjj_bbbbxbxbxa_1")
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
    & bind(c,name="ol_f_amp2ccall_ppajjj_bbbbxbxbxa_1")
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
    & bind(c,name="ol_f_amp2hcone_ppajjj_bbbbxbxbxa_1")
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
    & bind(c,name="ol_f_amp2hcall_ppajjj_bbbbxbxbxa_1")
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
    & bind(c,name="ol_amp2tree_ppajjj_bbbbxbxbxa_1")
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
    & bind(c,name="ol_amp2ccone_ppajjj_bbbbxbxbxa_1")
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
    & bind(c,name="ol_amp2ccall_ppajjj_bbbbxbxbxa_1")
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
    & bind(c,name="ol_amp2hcone_ppajjj_bbbbxbxbxa_1")
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
    & bind(c,name="ol_amp2hcall_ppajjj_bbbbxbxbxa_1")
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
    & bind(c,name="amp2tree_ppajjj_bbbbxbxbxa_1_")
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
    & bind(c,name="amp2ccone_ppajjj_bbbbxbxbxa_1_")
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
    & bind(c,name="amp2ccall_ppajjj_bbbbxbxbxa_1_")
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
    & bind(c,name="amp2hcone_ppajjj_bbbbxbxbxa_1_")
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
    & bind(c,name="amp2hcall_ppajjj_bbbbxbxbxa_1_")
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

end module ol_tree_ppajjj_bbbbxbxbxa_1_/**/REALKIND
