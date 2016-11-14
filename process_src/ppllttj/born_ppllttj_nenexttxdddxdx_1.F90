
module ol_colourmatrix_ppllttj_nenexttxdddxdx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(228,6)
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
  K1(  7,:) = [   0,   0,   0,   0,   0,   0]
  K1(  8,:) = [   0,   0,   0,   0,   0,   0]
  K1(  9,:) = [   0,   0,   0,   0,   0,   0]
  K1( 10,:) = [   0,   0,   0,   0,   0,   0]
  K1( 11,:) = [   0,   0,   0,   0,   0,   0]
  K1( 12,:) = [   0,   0,   0,   0,   0,   0]
  K1( 13,:) = [   0,   0,   0,   0,   0,   0]
  K1( 14,:) = [   0,   0,   0,   0,   0,   0]
  K1( 15,:) = [   0,   0,   0,   0,   0,   0]
  K1( 16,:) = [   0,   0,   0,   0,   0,   0]
  K1( 17,:) = [   0,   0,   0,   0,   0,   0]
  K1( 18,:) = [   0,   0,   0,   0,   0,   0]
  K1( 19,:) = [   0,   0,   0,   0,   0,   0]
  K1( 20,:) = [   0,   0,   0,   0,   0,   0]
  K1( 21,:) = [   0,   0,   0,   0,   0,   0]
  K1( 22,:) = [   0,   0,   0,   0,   0,   0]
  K1( 23,:) = [   0,   0,   0,   0,   0,   0]
  K1( 24,:) = [   0,   0,   0,   0,   0,   0]
  K1( 25,:) = [   0,   0,   0,   0,   0,   0]
  K1( 26,:) = [   0,   0,   0,   0,   0,   0]
  K1( 27,:) = [   0,   0,   0,   0,   0,   0]
  K1( 28,:) = [   0,   0,   0,   0,   0,   0]
  K1( 29,:) = [   0,   0,   0,   0,   0,   0]
  K1( 30,:) = [   0,   0,   0,   0,   0,   0]
  K1( 31,:) = [   0,   0,   0,   0,   0,   0]
  K1( 32,:) = [   0,   0,   0,   0,   0,   0]
  K1( 33,:) = [   0,   0,   0,   0,   0,   0]
  K1( 34,:) = [   0,   0,   0,   0,   0,   0]
  K1( 35,:) = [   0,   0,   0,   0,   0,   0]
  K1( 36,:) = [   0,   0,   0,   0,   0,   0]
  K1( 37,:) = [  36,  12,  12,   4,   4,  12]
  K1( 38,:) = [  12,  36,   4,  12,  12,   4]
  K1( 39,:) = [  12,   4,  36,  12,  12,   4]
  K1( 40,:) = [   4,  12,  12,  36,   4,  12]
  K1( 41,:) = [   4,  12,  12,   4,  36,  12]
  K1( 42,:) = [  12,   4,   4,  12,  12,  36]
  K1( 43,:) = [   0,   0,   0,   0,   0,   0]
  K1( 44,:) = [   0,   0,   0,   0,   0,   0]
  K1( 45,:) = [   0,   0,   0,   0,   0,   0]
  K1( 46,:) = [   0,   0,   0,   0,   0,   0]
  K1( 47,:) = [   0,   0,   0,   0,   0,   0]
  K1( 48,:) = [   0,   0,   0,   0,   0,   0]
  K1( 49,:) = [   0,   0,   0,   0,   0,   0]
  K1( 50,:) = [   0,   0,   0,   0,   0,   0]
  K1( 51,:) = [   0,   0,   0,   0,   0,   0]
  K1( 52,:) = [   0,   0,   0,   0,   0,   0]
  K1( 53,:) = [   0,   0,   0,   0,   0,   0]
  K1( 54,:) = [   0,   0,   0,   0,   0,   0]
  K1( 55,:) = [   0,   0,   0,  -4,  -4, -12]
  K1( 56,:) = [   0,   0,  -4, -12,   0,  -4]
  K1( 57,:) = [   0,  -4,   0, -12,   0,  -4]
  K1( 58,:) = [  -4, -12, -12, -36,  -4, -12]
  K1( 59,:) = [  -4,   0,   0,  -4,   0, -12]
  K1( 60,:) = [ -12,  -4,  -4, -12, -12, -36]
  K1( 61,:) = [  36,  12,  12,   4,   4,  12]
  K1( 62,:) = [  12,  36,   4,  12,  12,   4]
  K1( 63,:) = [  12,   4,  36,  12,  12,   4]
  K1( 64,:) = [   4,  12,  12,  36,   4,  12]
  K1( 65,:) = [   4,  12,  12,   4,  36,  12]
  K1( 66,:) = [  12,   4,   4,  12,  12,  36]
  K1( 67,:) = [   0,   0,   0,   0,   0,   0]
  K1( 68,:) = [   0,   0,   0,   0,   0,   0]
  K1( 69,:) = [   0,   0,   0,   0,   0,   0]
  K1( 70,:) = [   0,   0,   0,   0,   0,   0]
  K1( 71,:) = [   0,   0,   0,   0,   0,   0]
  K1( 72,:) = [   0,   0,   0,   0,   0,   0]
  K1( 73,:) = [   0,   0,   0,   0,   0,   0]
  K1( 74,:) = [   0,   0,   0,   0,   0,   0]
  K1( 75,:) = [   0,   0,   0,   0,   0,   0]
  K1( 76,:) = [   0,   0,   0,   0,   0,   0]
  K1( 77,:) = [   0,   0,   0,   0,   0,   0]
  K1( 78,:) = [   0,   0,   0,   0,   0,   0]
  K1( 79,:) = [   0,   0,  12,   4,   4,   0]
  K1( 80,:) = [   0,   0,   4,  12,   0,   4]
  K1( 81,:) = [  12,   4,   0,   0,   0,   4]
  K1( 82,:) = [   4,  12,   0,   0,   4,   0]
  K1( 83,:) = [   4,   0,   0,   4,   0,  12]
  K1( 84,:) = [   0,   4,   4,   0,  12,   0]
  K1( 85,:) = [   0, -12,   0,  -4,  -4,   0]
  K1( 86,:) = [ -12, -36,  -4, -12, -12,  -4]
  K1( 87,:) = [   0,  -4,   0,   0, -12,  -4]
  K1( 88,:) = [  -4, -12,   0,   0,  -4,   0]
  K1( 89,:) = [  -4, -12, -12,  -4, -36, -12]
  K1( 90,:) = [   0,  -4,  -4,   0, -12,   0]
  K1( 91,:) = [  36,  12,  12,   4,   4,  12]
  K1( 92,:) = [  12,  36,   4,  12,  12,   4]
  K1( 93,:) = [  12,   4,  36,  12,  12,   4]
  K1( 94,:) = [   4,  12,  12,  36,   4,  12]
  K1( 95,:) = [   4,  12,  12,   4,  36,  12]
  K1( 96,:) = [  12,   4,   4,  12,  12,  36]
  K1( 97,:) = [   0,   0,   0,   0,   0,   0]
  K1( 98,:) = [   0,   0,   0,   0,   0,   0]
  K1( 99,:) = [   0,   0,   0,   0,   0,   0]
  K1(100,:) = [   0,   0,   0,   0,   0,   0]
  K1(101,:) = [   0,   0,   0,   0,   0,   0]
  K1(102,:) = [   0,   0,   0,   0,   0,   0]
  K1(103,:) = [   0,   0,   0,   0,   0,   0]
  K1(104,:) = [   0,   0,   0,   0,   0,   0]
  K1(105,:) = [   0,   0,   0,   0,   0,   0]
  K1(106,:) = [   0,   0,   0,   0,   0,   0]
  K1(107,:) = [   0,   0,   0,   0,   0,   0]
  K1(108,:) = [   0,   0,   0,   0,   0,   0]
  K1(109,:) = [   0,   0,   0,   4,   4,  12]
  K1(110,:) = [   0,   0,   4,   0,  12,   4]
  K1(111,:) = [   0,   4,   0,  12,   0,   4]
  K1(112,:) = [   4,   0,  12,   0,   4,   0]
  K1(113,:) = [   4,  12,   0,   4,   0,   0]
  K1(114,:) = [  12,   4,   4,   0,   0,   0]
  K1(115,:) = [ -36, -12, -12,  -4,  -4, -12]
  K1(116,:) = [ -12,   0,  -4,   0,   0,  -4]
  K1(117,:) = [ -12,  -4, -36, -12, -12,  -4]
  K1(118,:) = [  -4,   0, -12,   0,  -4,   0]
  K1(119,:) = [  -4,   0, -12,  -4,   0,   0]
  K1(120,:) = [ -12,  -4,  -4,   0,   0,   0]
  K1(121,:) = [   0,  12,   0,   4,   4,   0]
  K1(122,:) = [  12,   0,   4,   0,   0,   4]
  K1(123,:) = [   0,   4,   0,   0,  12,   4]
  K1(124,:) = [   4,   0,   0,   0,   4,  12]
  K1(125,:) = [   4,   0,  12,   4,   0,   0]
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
  K1(145,:) = [   0,   0, -12,  -4,  -4,   0]
  K1(146,:) = [   0,   0,  -4,   0, -12,  -4]
  K1(147,:) = [ -12,  -4, -36, -12, -12,  -4]
  K1(148,:) = [  -4,   0, -12,   0,  -4,   0]
  K1(149,:) = [  -4, -12, -12,  -4, -36, -12]
  K1(150,:) = [   0,  -4,  -4,   0, -12,   0]
  K1(151,:) = [   0,  12,   0,   4,   4,   0]
  K1(152,:) = [  12,   0,   4,   0,   0,   4]
  K1(153,:) = [   0,   4,   0,  12,   0,   4]
  K1(154,:) = [   4,   0,  12,   0,   4,   0]
  K1(155,:) = [   4,   0,   0,   4,   0,  12]
  K1(156,:) = [   0,   4,   4,   0,  12,   0]
  K1(157,:) = [ -36, -12, -12,  -4,  -4, -12]
  K1(158,:) = [ -12,   0,  -4,   0,   0,  -4]
  K1(159,:) = [ -12,  -4,   0,   0,   0,  -4]
  K1(160,:) = [  -4,   0,   0,   0,  -4, -12]
  K1(161,:) = [  -4,   0,   0,  -4,   0, -12]
  K1(162,:) = [ -12,  -4,  -4, -12, -12, -36]
  K1(163,:) = [   0, -12,   0,  -4,  -4,   0]
  K1(164,:) = [ -12, -36,  -4, -12, -12,  -4]
  K1(165,:) = [   0,  -4,   0, -12,   0,  -4]
  K1(166,:) = [  -4, -12, -12, -36,  -4, -12]
  K1(167,:) = [  -4, -12,   0,  -4,   0,   0]
  K1(168,:) = [   0,  -4,  -4, -12,   0,   0]
  K1(169,:) = [  36,  12,  12,   4,   4,  12]
  K1(170,:) = [  12,  36,   4,  12,  12,   4]
  K1(171,:) = [  12,   4,  36,  12,  12,   4]
  K1(172,:) = [   4,  12,  12,  36,   4,  12]
  K1(173,:) = [   4,  12,  12,   4,  36,  12]
  K1(174,:) = [  12,   4,   4,  12,  12,  36]
  K1(175,:) = [   0,   0,   0,   0,   0,   0]
  K1(176,:) = [   0,   0,   0,   0,   0,   0]
  K1(177,:) = [   0,   0,   0,   0,   0,   0]
  K1(178,:) = [   0,   0,   0,   0,   0,   0]
  K1(179,:) = [   0,   0,   0,   0,   0,   0]
  K1(180,:) = [   0,   0,   0,   0,   0,   0]
  K1(181,:) = [   0,   0,   0,   0,   0,   0]
  K1(182,:) = [   0,   0,   0,   0,   0,   0]
  K1(183,:) = [   0,   0,   0,   0,   0,   0]
  K1(184,:) = [   0,   0,   0,   0,   0,   0]
  K1(185,:) = [   0,   0,   0,   0,   0,   0]
  K1(186,:) = [   0,   0,   0,   0,   0,   0]
  K1(187,:) = [ -36, -12, -12,  -4,  -4, -12]
  K1(188,:) = [ -12, -36,  -4, -12, -12,  -4]
  K1(189,:) = [ -12,  -4,   0,   0,   0,  -4]
  K1(190,:) = [  -4, -12,   0,   0,  -4,   0]
  K1(191,:) = [  -4, -12,   0,  -4,   0,   0]
  K1(192,:) = [ -12,  -4,  -4,   0,   0,   0]
  K1(193,:) = [   0,   0,   0,   4,   4,  12]
  K1(194,:) = [   0,   0,   4,  12,   0,   4]
  K1(195,:) = [   0,   4,   0,   0,  12,   4]
  K1(196,:) = [   4,  12,   0,   0,   4,   0]
  K1(197,:) = [   4,   0,  12,   4,   0,   0]
  K1(198,:) = [  12,   4,   4,   0,   0,   0]
  K1(199,:) = [   0,   0, -12,  -4,  -4,   0]
  K1(200,:) = [   0,   0,  -4, -12,   0,  -4]
  K1(201,:) = [ -12,  -4, -36, -12, -12,  -4]
  K1(202,:) = [  -4, -12, -12, -36,  -4, -12]
  K1(203,:) = [  -4,   0, -12,  -4,   0,   0]
  K1(204,:) = [   0,  -4,  -4, -12,   0,   0]
  K1(205,:) = [   0,   0,   0,  -4,  -4, -12]
  K1(206,:) = [   0,   0,  -4,   0, -12,  -4]
  K1(207,:) = [   0,  -4,   0,   0, -12,  -4]
  K1(208,:) = [  -4,   0,   0,   0,  -4, -12]
  K1(209,:) = [  -4, -12, -12,  -4, -36, -12]
  K1(210,:) = [ -12,  -4,  -4, -12, -12, -36]
  K1(211,:) = [   0,   0,  12,   4,   4,   0]
  K1(212,:) = [   0,   0,   4,   0,  12,   4]
  K1(213,:) = [  12,   4,   0,   0,   0,   4]
  K1(214,:) = [   4,   0,   0,   0,   4,  12]
  K1(215,:) = [   4,  12,   0,   4,   0,   0]
  K1(216,:) = [   0,   4,   4,  12,   0,   0]
  K1(217,:) = [  36,  12,  12,   4,   4,  12]
  K1(218,:) = [  12,  36,   4,  12,  12,   4]
  K1(219,:) = [  12,   4,  36,  12,  12,   4]
  K1(220,:) = [   4,  12,  12,  36,   4,  12]
  K1(221,:) = [   4,  12,  12,   4,  36,  12]
  K1(222,:) = [  12,   4,   4,  12,  12,  36]
  K1(223,:) = [   0,   0,   0,   0,   0,   0]
  K1(224,:) = [   0,   0,   0,   0,   0,   0]
  K1(225,:) = [   0,   0,   0,   0,   0,   0]
  K1(226,:) = [   0,   0,   0,   0,   0,   0]
  K1(227,:) = [   0,   0,   0,   0,   0,   0]
  K1(228,:) = [   0,   0,   0,   0,   0,   0]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllttj_nenexttxdddxdx_1_/**/REALKIND



module ol_forced_parameters_ppllttj_nenexttxdddxdx_1_/**/REALKIND
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
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMT /= 0) write(*,101) 'wMT = 0'
  if (wMT /= 0) write(*,101) 'wMT = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppllttj_nenexttxdddxdx_1_/**/REALKIND

module ol_tree_ppllttj_nenexttxdddxdx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(2)
  complex(REALKIND), save :: den(226)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 256 ! number of helicity configurations
  integer(intkind2), save :: nhel = 256 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(256) ! physical helicity states
  complex(DREALKIND) :: M1helarr(6,256) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = CI*eQED**2*gQCD**4
    f(2) = eQED**2*gQCD**4

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,3) - MZ2)
  den(2) = 1 / (Q(5,12))
  den(3) = 1 / (Q(5,80))
  den(4) = 1 / (Q(5,35))
  den(6) = 1 / (Q(5,140))
  den(10) = 1 / (Q(5,208))
  den(15) = 1 / (Q(5,92))
  den(18) = 1 / (Q(5,44))
  den(20) = 1 / (Q(5,131))
  den(24) = 1 / (Q(5,112))
  den(33) = 1 / (Q(5,19))
  den(35) = 1 / (Q(5,96))
  den(39) = 1 / (Q(5,108))
  den(42) = 1 / (Q(5,224))
  den(46) = 1 / (Q(5,28))
  den(58) = 1 / (Q(5,144))
  den(59) = 1 / (Q(5,76))
  den(66) = 1 / (Q(5,156))
  den(69) = 1 / (Q(5,67))
  den(73) = 1 / (Q(5,176))
  den(81) = 1 / (Q(5,160))
  den(85) = 1 / (Q(5,172))
  den(99) = 1 / (Q(5,83))
  den(110) = 1 / (Q(5,99))
  den(123) = 1 / (Q(5,163))
  den(126) = 1 / (Q(5,147))
  den(132) = 1 / (Q(5,7) - MT2)
  den(134) = 1 / (Q(5,240))
  den(137) = 1 / (Q(5,88) - MT2)
  den(141) = 1 / (Q(5,168) - MT2)
  den(145) = 1 / (Q(5,11) - MT2)
  den(148) = 1 / (Q(5,84) - MT2)
  den(152) = 1 / (Q(5,164) - MT2)
  den(160) = 1 / (Q(5,15))
  den(176) = 1 / (Q(5,152) - MT2)
  den(180) = 1 / (Q(5,104) - MT2)
  den(185) = 1 / (Q(5,148) - MT2)
  den(189) = 1 / (Q(5,100) - MT2)

  ! denominators

  den(5) = den(1)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(3)*den(5)
  den(9) = den(7)*den(8)
  den(11) = den(3)*den(10)
  den(12) = den(2)*den(5)
  den(13) = den(11)*den(12)
  den(14) = den(2)*den(3)
  den(16) = den(14)*den(15)
  den(17) = den(5)*den(16)
  den(19) = den(2)*den(18)
  den(21) = den(1)*den(20)
  den(22) = den(3)*den(19)
  den(23) = den(21)*den(22)
  den(25) = den(3)*den(24)
  den(26) = den(2)*den(25)
  den(27) = den(21)*den(26)
  den(28) = den(16)*den(21)
  den(29) = den(1)*den(19)
  den(30) = den(11)*den(29)
  den(31) = den(1)*den(25)
  den(32) = den(7)*den(31)
  den(34) = den(1)*den(33)
  den(36) = den(34)*den(35)
  den(37) = den(7)*den(36)
  den(38) = den(2)*den(35)
  den(40) = den(38)*den(39)
  den(41) = den(34)*den(40)
  den(43) = den(35)*den(42)
  den(44) = den(2)*den(34)
  den(45) = den(43)*den(44)
  den(47) = den(2)*den(46)
  den(48) = den(35)*den(47)
  den(49) = den(21)*den(48)
  den(50) = den(24)*den(35)
  den(51) = den(2)*den(50)
  den(52) = den(21)*den(51)
  den(53) = den(21)*den(40)
  den(54) = den(1)*den(47)
  den(55) = den(43)*den(54)
  den(56) = den(1)*den(50)
  den(57) = den(7)*den(56)
  den(60) = den(2)*den(59)
  den(61) = den(5)*den(58)
  den(62) = den(60)*den(61)
  den(63) = den(10)*den(58)
  den(64) = den(12)*den(63)
  den(65) = den(2)*den(58)
  den(67) = den(65)*den(66)
  den(68) = den(5)*den(67)
  den(70) = den(1)*den(69)
  den(71) = den(19)*den(58)
  den(72) = den(70)*den(71)
  den(74) = den(58)*den(73)
  den(75) = den(2)*den(74)
  den(76) = den(70)*den(75)
  den(77) = den(67)*den(70)
  den(78) = den(29)*den(63)
  den(79) = den(1)*den(74)
  den(80) = den(60)*den(79)
  den(82) = den(34)*den(81)
  den(83) = den(60)*den(82)
  den(84) = den(2)*den(81)
  den(86) = den(84)*den(85)
  den(87) = den(34)*den(86)
  den(88) = den(42)*den(81)
  den(89) = den(44)*den(88)
  den(90) = den(47)*den(81)
  den(91) = den(70)*den(90)
  den(92) = den(73)*den(81)
  den(93) = den(2)*den(92)
  den(94) = den(70)*den(93)
  den(95) = den(70)*den(86)
  den(96) = den(54)*den(88)
  den(97) = den(1)*den(92)
  den(98) = den(60)*den(97)
  den(100) = den(34)*den(99)
  den(101) = den(19)*den(100)
  den(102) = den(19)*den(39)
  den(103) = den(34)*den(102)
  den(104) = den(39)*den(60)
  den(105) = den(34)*den(104)
  den(106) = den(7)*den(85)
  den(107) = den(34)*den(106)
  den(108) = den(15)*den(47)
  den(109) = den(5)*den(108)
  den(111) = den(5)*den(110)
  den(112) = den(47)*den(111)
  den(113) = den(15)*den(60)
  den(114) = den(5)*den(113)
  den(115) = den(7)*den(66)
  den(116) = den(5)*den(115)
  den(117) = den(70)*den(110)
  den(118) = den(47)*den(117)
  den(119) = den(70)*den(99)
  den(120) = den(19)*den(119)
  den(121) = den(7)*den(119)
  den(122) = den(70)*den(115)
  den(124) = den(21)*den(123)
  den(125) = den(47)*den(124)
  den(127) = den(21)*den(126)
  den(128) = den(19)*den(127)
  den(129) = den(21)*den(113)
  den(130) = den(60)*den(127)
  den(131) = den(3)*den(81)
  den(133) = den(1)*den(132)
  den(135) = den(131)*den(134)
  den(136) = den(133)*den(135)
  den(138) = den(3)*den(137)
  den(139) = den(133)*den(138)
  den(140) = den(81)*den(139)
  den(142) = den(81)*den(141)
  den(143) = den(133)*den(142)
  den(144) = den(3)*den(143)
  den(146) = den(1)*den(145)
  den(147) = den(135)*den(146)
  den(149) = den(3)*den(148)
  den(150) = den(146)*den(149)
  den(151) = den(81)*den(150)
  den(153) = den(81)*den(152)
  den(154) = den(146)*den(153)
  den(155) = den(3)*den(154)
  den(156) = den(1)*den(149)
  den(157) = den(142)*den(156)
  den(158) = den(1)*den(153)
  den(159) = den(138)*den(158)
  den(161) = den(133)*den(160)
  den(162) = den(11)*den(161)
  den(163) = den(25)*den(161)
  den(164) = den(146)*den(160)
  den(165) = den(11)*den(164)
  den(166) = den(25)*den(164)
  den(167) = den(15)*den(149)
  den(168) = den(5)*den(167)
  den(169) = den(15)*den(138)
  den(170) = den(5)*den(169)
  den(171) = den(21)*den(167)
  den(172) = den(21)*den(169)
  den(173) = den(35)*den(58)
  den(174) = den(134)*den(173)
  den(175) = den(133)*den(174)
  den(177) = den(58)*den(176)
  den(178) = den(133)*den(177)
  den(179) = den(35)*den(178)
  den(181) = den(35)*den(180)
  den(182) = den(133)*den(181)
  den(183) = den(58)*den(182)
  den(184) = den(146)*den(174)
  den(186) = den(58)*den(185)
  den(187) = den(146)*den(186)
  den(188) = den(35)*den(187)
  den(190) = den(35)*den(189)
  den(191) = den(146)*den(190)
  den(192) = den(58)*den(191)
  den(193) = den(1)*den(186)
  den(194) = den(181)*den(193)
  den(195) = den(1)*den(190)
  den(196) = den(177)*den(195)
  den(197) = den(43)*den(161)
  den(198) = den(50)*den(161)
  den(199) = den(43)*den(164)
  den(200) = den(50)*den(164)
  den(201) = den(39)*den(190)
  den(202) = den(34)*den(201)
  den(203) = den(39)*den(181)
  den(204) = den(34)*den(203)
  den(205) = den(21)*den(203)
  den(206) = den(21)*den(201)
  den(207) = den(63)*den(161)
  den(208) = den(74)*den(161)
  den(209) = den(63)*den(164)
  den(210) = den(74)*den(164)
  den(211) = den(66)*den(186)
  den(212) = den(5)*den(211)
  den(213) = den(66)*den(177)
  den(214) = den(5)*den(213)
  den(215) = den(70)*den(211)
  den(216) = den(70)*den(213)
  den(217) = den(88)*den(161)
  den(218) = den(92)*den(161)
  den(219) = den(88)*den(164)
  den(220) = den(92)*den(164)
  den(221) = den(85)*den(153)
  den(222) = den(34)*den(221)
  den(223) = den(85)*den(142)
  den(224) = den(34)*den(223)
  den(225) = den(70)*den(223)
  den(226) = den(70)*den(221)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_ppllttj_nenexttxdddxdx_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_ppllttj_nenexttxdddxdx_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for nu_e anti-nu_e top anti-top down down anti-down anti-down -> 0
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
  use ol_external_ppllttj_nenexttxdddxdx_1, only: external_perm_ppllttj_nenexttxdddxdx_1, &
    & external_perm_inv_ppllttj_nenexttxdddxdx_1, extcomb_perm_ppllttj_nenexttxdddxdx_1, &
    & average_factor_ppllttj_nenexttxdddxdx_1
  use ol_external_ppllttj_nenexttxdddxdx_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_ppllttj_nenexttxdddxdx_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_ppllttj_nenexttxdddxdx_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_ppllttj_nenexttxdddxdx_1
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
  complex(REALKIND) :: MOM_LC(4), M1(6), M1helarray(6,256)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(2), ex6(2), ex7(2), ex8(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,7), wf8(8,52), wf16(16,40), wf32(32,24), wf64(64,8), wf256(256,96)

  type(polcont) :: A(256,96)
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
  extmasses2 = [ rZERO2, rZERO2, rMT2, rMT2, rZERO2, rZERO2, rZERO2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_ppllttj_nenexttxdddxdx_1,8)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,8)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_ppllttj_nenexttxdddxdx_1(I)
  else if (I < 0) then
    JBmunu = external_perm_ppllttj_nenexttxdddxdx_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_ppllttj_nenexttxdddxdx_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_Q(P(:,1), rZERO, H1, ex1)
  call wf_A(P(:,2), rZERO, H2, ex2)
  call wf_Q(P(:,3), rMT, H3, ex3)
  call wf_A(P(:,4), rMT, H4, ex4)
  call wf_Q(P(:,5), rZERO, H5, ex5)
  call wf_Q(P(:,6), rZERO, H6, ex6)
  call wf_A(P(:,7), rZERO, H7, ex7)
  call wf_A(P(:,8), rZERO, H8, ex8)


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
  call vert_QA_V(ntry, ex3, ex4, wf4(:,2), n3(:,2), t3x4(:,:,2))
  call vert_QA_V(ntry, ex5, ex7, wf4(:,3), n3(:,3), t3x4(:,:,3))
  call prop_W_W(ntry, wf4(:,1), Q(:,3), MZ, 1_intkind1, wf4(:,4), n2(1))
  call vert_ZQ_A(gZd,ntry, wf4(:,4), ex6, wf8(:,1), n3(:,4), t3x8(:,:,1))
  call vert_AV_Q(ntry, ex8, wf4(:,2), wf8(:,2), n3(:,5), t3x8(:,:,2))
  call prop_Q_A(ntry, wf8(:,1), Q(:,35), ZERO, 0_intkind1, wf8(:,3), n2(2))
  call prop_A_Q(ntry, wf8(:,2), Q(:,140), ZERO, 0_intkind1, wf8(:,4), n2(3))
  call vert_VQ_A(ntry, wf4(:,3), wf8(:,3), wf32(:,1), n3(:,6), t3x32(:,:,1))
  call vert_AV_Q(ntry, ex8, wf4(:,3), wf8(:,5), n3(:,7), t3x8(:,:,3))
  call prop_A_Q(ntry, wf8(:,5), Q(:,208), ZERO, 0_intkind1, wf8(:,6), n2(4))
  call vert_VQ_A(ntry, wf4(:,2), wf8(:,3), wf32(:,2), n3(:,8), t3x32(:,:,2))
  call vert_UV_W(ntry, wf4(:,2), Q(:,12), wf4(:,3), Q(:,80), wf16(:,1), n3(:,9), t3x16(:,:,1))
  call vert_QA_V(ntry, wf8(:,3), ex8, wf16(:,2), n3(:,10), t3x16(:,:,2))
  call vert_VQ_A(ntry, wf4(:,2), ex6, wf8(:,7), n3(:,11), t3x8(:,:,4))
  call vert_AZ_Q(gZd,ntry, ex8, wf4(:,4), wf8(:,8), n3(:,12), t3x8(:,:,5))
  call prop_Q_A(ntry, wf8(:,7), Q(:,44), ZERO, 0_intkind1, wf8(:,9), n2(5))
  call prop_A_Q(ntry, wf8(:,8), Q(:,131), ZERO, 0_intkind1, wf8(:,10), n2(6))
  call vert_VQ_A(ntry, wf4(:,3), wf8(:,9), wf32(:,3), n3(:,13), t3x32(:,:,3))
  call vert_VQ_A(ntry, wf4(:,3), ex6, wf8(:,11), n3(:,14), t3x8(:,:,6))
  call prop_Q_A(ntry, wf8(:,11), Q(:,112), ZERO, 0_intkind1, wf8(:,12), n2(7))
  call vert_VQ_A(ntry, wf4(:,2), wf8(:,12), wf32(:,4), n3(:,15), t3x32(:,:,4))
  call vert_QA_V(ntry, ex6, wf8(:,10), wf16(:,3), n3(:,16), t3x16(:,:,3))
  call vert_ZQ_A(gZd,ntry, wf4(:,4), wf8(:,9), wf32(:,5), n3(:,17), t3x32(:,:,5))
  call vert_ZQ_A(gZd,ntry, wf4(:,4), wf8(:,12), wf32(:,6), n3(:,18), t3x32(:,:,6))
  call vert_QA_V(ntry, ex6, ex7, wf4(:,5), n3(:,19), t3x4(:,:,4))
  call vert_ZQ_A(gZd,ntry, wf4(:,4), ex5, wf8(:,13), n3(:,20), t3x8(:,:,7))
  call prop_Q_A(ntry, wf8(:,13), Q(:,19), ZERO, 0_intkind1, wf8(:,14), n2(8))
  call vert_VQ_A(ntry, wf4(:,5), wf8(:,14), wf32(:,7), n3(:,21), t3x32(:,:,7))
  call vert_UV_W(ntry, wf4(:,2), Q(:,12), wf4(:,5), Q(:,96), wf16(:,4), n3(:,22), t3x16(:,:,4))
  call vert_QA_V(ntry, wf8(:,14), ex8, wf16(:,5), n3(:,23), t3x16(:,:,5))
  call vert_AV_Q(ntry, ex8, wf4(:,5), wf8(:,15), n3(:,24), t3x8(:,:,8))
  call prop_A_Q(ntry, wf8(:,15), Q(:,224), ZERO, 0_intkind1, wf8(:,16), n2(9))
  call vert_VQ_A(ntry, wf4(:,2), wf8(:,14), wf32(:,8), n3(:,25), t3x32(:,:,8))
  call vert_VQ_A(ntry, wf4(:,2), ex5, wf8(:,17), n3(:,26), t3x8(:,:,9))
  call prop_Q_A(ntry, wf8(:,17), Q(:,28), ZERO, 0_intkind1, wf8(:,18), n2(10))
  call vert_VQ_A(ntry, wf4(:,5), wf8(:,18), wf32(:,9), n3(:,27), t3x32(:,:,9))
  call vert_VQ_A(ntry, wf4(:,5), ex5, wf8(:,19), n3(:,28), t3x8(:,:,10))
  call prop_Q_A(ntry, wf8(:,19), Q(:,112), ZERO, 0_intkind1, wf8(:,20), n2(11))
  call vert_VQ_A(ntry, wf4(:,2), wf8(:,20), wf32(:,10), n3(:,29), t3x32(:,:,10))
  call vert_QA_V(ntry, ex5, wf8(:,10), wf16(:,6), n3(:,30), t3x16(:,:,6))
  call vert_ZQ_A(gZd,ntry, wf4(:,4), wf8(:,18), wf32(:,11), n3(:,31), t3x32(:,:,11))
  call vert_ZQ_A(gZd,ntry, wf4(:,4), wf8(:,20), wf32(:,12), n3(:,32), t3x32(:,:,12))
  call vert_QA_V(ntry, ex5, ex8, wf4(:,6), n3(:,33), t3x4(:,:,5))
  call vert_AV_Q(ntry, ex7, wf4(:,2), wf8(:,21), n3(:,34), t3x8(:,:,11))
  call prop_A_Q(ntry, wf8(:,21), Q(:,76), ZERO, 0_intkind1, wf8(:,22), n2(12))
  call vert_VQ_A(ntry, wf4(:,6), wf8(:,3), wf32(:,13), n3(:,35), t3x32(:,:,13))
  call vert_AV_Q(ntry, ex7, wf4(:,6), wf8(:,23), n3(:,36), t3x8(:,:,12))
  call prop_A_Q(ntry, wf8(:,23), Q(:,208), ZERO, 0_intkind1, wf8(:,24), n2(13))
  call vert_UV_W(ntry, wf4(:,2), Q(:,12), wf4(:,6), Q(:,144), wf16(:,7), n3(:,37), t3x16(:,:,7))
  call vert_QA_V(ntry, wf8(:,3), ex7, wf16(:,8), n3(:,38), t3x16(:,:,8))
  call vert_AZ_Q(gZd,ntry, ex7, wf4(:,4), wf8(:,25), n3(:,39), t3x8(:,:,13))
  call prop_A_Q(ntry, wf8(:,25), Q(:,67), ZERO, 0_intkind1, wf8(:,26), n2(14))
  call vert_VQ_A(ntry, wf4(:,6), wf8(:,9), wf32(:,14), n3(:,40), t3x32(:,:,14))
  call vert_VQ_A(ntry, wf4(:,6), ex6, wf8(:,27), n3(:,41), t3x8(:,:,14))
  call prop_Q_A(ntry, wf8(:,27), Q(:,176), ZERO, 0_intkind1, wf8(:,28), n2(15))
  call vert_VQ_A(ntry, wf4(:,2), wf8(:,28), wf32(:,15), n3(:,42), t3x32(:,:,15))
  call vert_QA_V(ntry, ex6, wf8(:,26), wf16(:,9), n3(:,43), t3x16(:,:,9))
  call vert_ZQ_A(gZd,ntry, wf4(:,4), wf8(:,28), wf32(:,16), n3(:,44), t3x32(:,:,16))
  call vert_QA_V(ntry, ex6, ex8, wf4(:,7), n3(:,45), t3x4(:,:,6))
  call vert_VQ_A(ntry, wf4(:,7), wf8(:,14), wf32(:,17), n3(:,46), t3x32(:,:,17))
  call vert_UV_W(ntry, wf4(:,2), Q(:,12), wf4(:,7), Q(:,160), wf16(:,10), n3(:,47), t3x16(:,:,10))
  call vert_QA_V(ntry, wf8(:,14), ex7, wf16(:,11), n3(:,48), t3x16(:,:,11))
  call vert_AV_Q(ntry, ex7, wf4(:,7), wf8(:,29), n3(:,49), t3x8(:,:,15))
  call prop_A_Q(ntry, wf8(:,29), Q(:,224), ZERO, 0_intkind1, wf8(:,30), n2(16))
  call vert_VQ_A(ntry, wf4(:,7), wf8(:,18), wf32(:,18), n3(:,50), t3x32(:,:,18))
  call vert_VQ_A(ntry, wf4(:,7), ex5, wf8(:,31), n3(:,51), t3x8(:,:,16))
  call prop_Q_A(ntry, wf8(:,31), Q(:,176), ZERO, 0_intkind1, wf8(:,32), n2(17))
  call vert_VQ_A(ntry, wf4(:,2), wf8(:,32), wf32(:,19), n3(:,52), t3x32(:,:,19))
  call vert_QA_V(ntry, ex5, wf8(:,26), wf16(:,12), n3(:,53), t3x16(:,:,12))
  call vert_ZQ_A(gZd,ntry, wf4(:,4), wf8(:,32), wf32(:,20), n3(:,54), t3x32(:,:,20))
  call vert_QA_V(ntry, wf8(:,9), ex8, wf16(:,13), n3(:,55), t3x16(:,:,13))
  call vert_QA_V(ntry, wf8(:,9), ex7, wf16(:,14), n3(:,56), t3x16(:,:,14))
  call vert_QA_V(ntry, ex6, wf8(:,22), wf16(:,15), n3(:,57), t3x16(:,:,15))
  call vert_QA_V(ntry, ex6, wf8(:,4), wf16(:,16), n3(:,58), t3x16(:,:,16))
  call vert_QA_V(ntry, wf8(:,18), ex7, wf16(:,17), n3(:,59), t3x16(:,:,17))
  call vert_QA_V(ntry, wf8(:,18), ex8, wf16(:,18), n3(:,60), t3x16(:,:,18))
  call vert_QA_V(ntry, ex5, wf8(:,22), wf16(:,19), n3(:,61), t3x16(:,:,19))
  call vert_QA_V(ntry, ex5, wf8(:,4), wf16(:,20), n3(:,62), t3x16(:,:,20))
  call vert_ZQ_A(gZu,ntry, wf4(:,4), ex3, wf8(:,33), n3(:,63), t3x8(:,:,17))
  call vert_UV_W(ntry, wf4(:,3), Q(:,80), wf4(:,7), Q(:,160), wf16(:,21), n3(:,64), t3x16(:,:,21))
  call prop_Q_A(ntry, wf8(:,33), Q(:,7), MT, 1_intkind1, wf8(:,34), n2(18))
  call vert_QA_V(ntry, wf8(:,34), ex4, wf16(:,22), n3(:,65), t3x16(:,:,22))
  call vert_AV_Q(ntry, ex4, wf4(:,3), wf8(:,35), n3(:,66), t3x8(:,:,18))
  call prop_A_Q(ntry, wf8(:,35), Q(:,88), MT, 1_intkind1, wf8(:,36), n2(19))
  call vert_QA_V(ntry, wf8(:,34), wf8(:,36), wf64(:,1), n3(:,67), t3x64(:,:,1))
  call vert_AV_Q(ntry, ex4, wf4(:,7), wf8(:,37), n3(:,68), t3x8(:,:,19))
  call prop_A_Q(ntry, wf8(:,37), Q(:,168), MT, 1_intkind1, wf8(:,38), n2(20))
  call vert_QA_V(ntry, wf8(:,34), wf8(:,38), wf64(:,2), n3(:,69), t3x64(:,:,2))
  call vert_AZ_Q(gZu,ntry, ex4, wf4(:,4), wf8(:,39), n3(:,70), t3x8(:,:,20))
  call prop_A_Q(ntry, wf8(:,39), Q(:,11), MT, 1_intkind1, wf8(:,40), n2(21))
  call vert_QA_V(ntry, ex3, wf8(:,40), wf16(:,23), n3(:,71), t3x16(:,:,23))
  call vert_VQ_A(ntry, wf4(:,3), ex3, wf8(:,41), n3(:,72), t3x8(:,:,21))
  call prop_Q_A(ntry, wf8(:,41), Q(:,84), MT, 1_intkind1, wf8(:,42), n2(22))
  call vert_QA_V(ntry, wf8(:,42), wf8(:,40), wf64(:,3), n3(:,73), t3x64(:,:,3))
  call vert_VQ_A(ntry, wf4(:,7), ex3, wf8(:,43), n3(:,74), t3x8(:,:,22))
  call prop_Q_A(ntry, wf8(:,43), Q(:,164), MT, 1_intkind1, wf8(:,44), n2(23))
  call vert_QA_V(ntry, wf8(:,44), wf8(:,40), wf64(:,4), n3(:,75), t3x64(:,:,4))
  call vert_ZQ_A(gZu,ntry, wf4(:,4), wf8(:,42), wf32(:,21), n3(:,76), t3x32(:,:,21))
  call vert_ZQ_A(gZu,ntry, wf4(:,4), wf8(:,44), wf32(:,22), n3(:,77), t3x32(:,:,22))
  call vert_QA_V(ntry, ex6, wf8(:,6), wf16(:,24), n3(:,78), t3x16(:,:,24))
  call vert_QA_V(ntry, wf8(:,12), ex8, wf16(:,25), n3(:,79), t3x16(:,:,25))
  call vert_QA_V(ntry, wf8(:,42), ex4, wf16(:,26), n3(:,80), t3x16(:,:,26))
  call vert_QA_V(ntry, ex3, wf8(:,36), wf16(:,27), n3(:,81), t3x16(:,:,27))
  call vert_UV_W(ntry, wf4(:,5), Q(:,96), wf4(:,6), Q(:,144), wf16(:,28), n3(:,82), t3x16(:,:,28))
  call vert_AV_Q(ntry, ex4, wf4(:,6), wf8(:,45), n3(:,83), t3x8(:,:,23))
  call prop_A_Q(ntry, wf8(:,45), Q(:,152), MT, 1_intkind1, wf8(:,46), n2(24))
  call vert_QA_V(ntry, wf8(:,34), wf8(:,46), wf64(:,5), n3(:,84), t3x64(:,:,5))
  call vert_AV_Q(ntry, ex4, wf4(:,5), wf8(:,47), n3(:,85), t3x8(:,:,24))
  call prop_A_Q(ntry, wf8(:,47), Q(:,104), MT, 1_intkind1, wf8(:,48), n2(25))
  call vert_QA_V(ntry, wf8(:,34), wf8(:,48), wf64(:,6), n3(:,86), t3x64(:,:,6))
  call vert_VQ_A(ntry, wf4(:,6), ex3, wf8(:,49), n3(:,87), t3x8(:,:,25))
  call prop_Q_A(ntry, wf8(:,49), Q(:,148), MT, 1_intkind1, wf8(:,50), n2(26))
  call vert_QA_V(ntry, wf8(:,50), wf8(:,40), wf64(:,7), n3(:,88), t3x64(:,:,7))
  call vert_VQ_A(ntry, wf4(:,5), ex3, wf8(:,51), n3(:,89), t3x8(:,:,26))
  call prop_Q_A(ntry, wf8(:,51), Q(:,100), MT, 1_intkind1, wf8(:,52), n2(27))
  call vert_QA_V(ntry, wf8(:,52), wf8(:,40), wf64(:,8), n3(:,90), t3x64(:,:,8))
  call vert_ZQ_A(gZu,ntry, wf4(:,4), wf8(:,50), wf32(:,23), n3(:,91), t3x32(:,:,23))
  call vert_ZQ_A(gZu,ntry, wf4(:,4), wf8(:,52), wf32(:,24), n3(:,92), t3x32(:,:,24))
  call vert_QA_V(ntry, ex5, wf8(:,16), wf16(:,29), n3(:,93), t3x16(:,:,29))
  call vert_QA_V(ntry, wf8(:,20), ex8, wf16(:,30), n3(:,94), t3x16(:,:,30))
  call vert_QA_V(ntry, wf8(:,52), ex4, wf16(:,31), n3(:,95), t3x16(:,:,31))
  call vert_QA_V(ntry, ex3, wf8(:,48), wf16(:,32), n3(:,96), t3x16(:,:,32))
  call vert_QA_V(ntry, ex6, wf8(:,24), wf16(:,33), n3(:,97), t3x16(:,:,33))
  call vert_QA_V(ntry, wf8(:,28), ex7, wf16(:,34), n3(:,98), t3x16(:,:,34))
  call vert_QA_V(ntry, wf8(:,50), ex4, wf16(:,35), n3(:,99), t3x16(:,:,35))
  call vert_QA_V(ntry, ex3, wf8(:,46), wf16(:,36), n3(:,100), t3x16(:,:,36))
  call vert_QA_V(ntry, ex5, wf8(:,30), wf16(:,37), n3(:,101), t3x16(:,:,37))
  call vert_QA_V(ntry, wf8(:,32), ex7, wf16(:,38), n3(:,102), t3x16(:,:,38))
  call vert_QA_V(ntry, wf8(:,44), ex4, wf16(:,39), n3(:,103), t3x16(:,:,39))
  call vert_QA_V(ntry, ex3, wf8(:,38), wf16(:,40), n3(:,104), t3x16(:,:,40))


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
  M2add = M2 / average_factor_ppllttj_nenexttxdddxdx_1

  do k = 0, 38-1
    M2(k) = M2add(extcomb_perm_ppllttj_nenexttxdddxdx_1(k))
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

    call cont_QA(nsync, wf8(:,4), wf32(:,1), A(:,1), n3(:,105), t3x256(:,:,1), nhel, den(9))
    call cont_QA(nsync, wf8(:,6), wf32(:,2), A(:,2), n3(:,106), t3x256(:,:,2), nhel, den(13))
    call cont_VV(nsync, wf16(:,1), wf16(:,2), A(:,3), n3(:,107), t3x256(:,:,3), nhel, den(17))
    call cont_QA(nsync, wf8(:,10), wf32(:,3), A(:,4), n3(:,108), t3x256(:,:,4), nhel, den(23))
    call cont_QA(nsync, wf8(:,10), wf32(:,4), A(:,5), n3(:,109), t3x256(:,:,5), nhel, den(27))
    call cont_VV(nsync, wf16(:,1), wf16(:,3), A(:,6), n3(:,110), t3x256(:,:,6), nhel, den(28))
    call cont_QA(nsync, wf8(:,6), wf32(:,5), A(:,7), n3(:,111), t3x256(:,:,7), nhel, den(30))
    call cont_QA(nsync, wf8(:,4), wf32(:,6), A(:,8), n3(:,112), t3x256(:,:,8), nhel, den(32))
    call cont_QA(nsync, wf8(:,4), wf32(:,7), A(:,9), n3(:,113), t3x256(:,:,9), nhel, den(37))
    call cont_VV(nsync, wf16(:,4), wf16(:,5), A(:,10), n3(:,114), t3x256(:,:,10), nhel, den(41))
    call cont_QA(nsync, wf8(:,16), wf32(:,8), A(:,11), n3(:,115), t3x256(:,:,11), nhel, den(45))
    call cont_QA(nsync, wf8(:,10), wf32(:,9), A(:,12), n3(:,116), t3x256(:,:,12), nhel, den(49))
    call cont_QA(nsync, wf8(:,10), wf32(:,10), A(:,13), n3(:,117), t3x256(:,:,13), nhel, den(52))
    call cont_VV(nsync, wf16(:,4), wf16(:,6), A(:,14), n3(:,118), t3x256(:,:,14), nhel, den(53))
    call cont_QA(nsync, wf8(:,16), wf32(:,11), A(:,15), n3(:,119), t3x256(:,:,15), nhel, den(55))
    call cont_QA(nsync, wf8(:,4), wf32(:,12), A(:,16), n3(:,120), t3x256(:,:,16), nhel, den(57))
    call cont_QA(nsync, wf8(:,22), wf32(:,13), A(:,17), n3(:,121), t3x256(:,:,17), nhel, den(62))
    call cont_QA(nsync, wf32(:,2), wf8(:,24), A(:,18), n3(:,122), t3x256(:,:,18), nhel, den(64))
    call cont_VV(nsync, wf16(:,7), wf16(:,8), A(:,19), n3(:,123), t3x256(:,:,19), nhel, den(68))
    call cont_QA(nsync, wf8(:,26), wf32(:,14), A(:,20), n3(:,124), t3x256(:,:,20), nhel, den(72))
    call cont_QA(nsync, wf8(:,26), wf32(:,15), A(:,21), n3(:,125), t3x256(:,:,21), nhel, den(76))
    call cont_VV(nsync, wf16(:,7), wf16(:,9), A(:,22), n3(:,126), t3x256(:,:,22), nhel, den(77))
    call cont_QA(nsync, wf32(:,5), wf8(:,24), A(:,23), n3(:,127), t3x256(:,:,23), nhel, den(78))
    call cont_QA(nsync, wf8(:,22), wf32(:,16), A(:,24), n3(:,128), t3x256(:,:,24), nhel, den(80))
    call cont_QA(nsync, wf8(:,22), wf32(:,17), A(:,25), n3(:,129), t3x256(:,:,25), nhel, den(83))
    call cont_VV(nsync, wf16(:,10), wf16(:,11), A(:,26), n3(:,130), t3x256(:,:,26), nhel, den(87))
    call cont_QA(nsync, wf32(:,8), wf8(:,30), A(:,27), n3(:,131), t3x256(:,:,27), nhel, den(89))
    call cont_QA(nsync, wf8(:,26), wf32(:,18), A(:,28), n3(:,132), t3x256(:,:,28), nhel, den(91))
    call cont_QA(nsync, wf8(:,26), wf32(:,19), A(:,29), n3(:,133), t3x256(:,:,29), nhel, den(94))
    call cont_VV(nsync, wf16(:,10), wf16(:,12), A(:,30), n3(:,134), t3x256(:,:,30), nhel, den(95))
    call cont_QA(nsync, wf32(:,11), wf8(:,30), A(:,31), n3(:,135), t3x256(:,:,31), nhel, den(96))
    call cont_QA(nsync, wf8(:,22), wf32(:,20), A(:,32), n3(:,136), t3x256(:,:,32), nhel, den(98))
    call cont_VV(nsync, wf16(:,11), wf16(:,13), A(:,33), n3(:,137), t3x256(:,:,33), nhel, den(101))
    call cont_VV(nsync, wf16(:,5), wf16(:,14), A(:,34), n3(:,138), t3x256(:,:,34), nhel, den(103))
    call cont_VV(nsync, wf16(:,5), wf16(:,15), A(:,35), n3(:,139), t3x256(:,:,35), nhel, den(105))
    call cont_VV(nsync, wf16(:,11), wf16(:,16), A(:,36), n3(:,140), t3x256(:,:,36), nhel, den(107))
    call cont_VV(nsync, wf16(:,2), wf16(:,17), A(:,37), n3(:,141), t3x256(:,:,37), nhel, den(109))
    call cont_VV(nsync, wf16(:,8), wf16(:,18), A(:,38), n3(:,142), t3x256(:,:,38), nhel, den(112))
    call cont_VV(nsync, wf16(:,2), wf16(:,19), A(:,39), n3(:,143), t3x256(:,:,39), nhel, den(114))
    call cont_VV(nsync, wf16(:,8), wf16(:,20), A(:,40), n3(:,144), t3x256(:,:,40), nhel, den(116))
    call cont_VV(nsync, wf16(:,9), wf16(:,18), A(:,41), n3(:,145), t3x256(:,:,41), nhel, den(118))
    call cont_VV(nsync, wf16(:,12), wf16(:,13), A(:,42), n3(:,146), t3x256(:,:,42), nhel, den(120))
    call cont_VV(nsync, wf16(:,12), wf16(:,16), A(:,43), n3(:,147), t3x256(:,:,43), nhel, den(121))
    call cont_VV(nsync, wf16(:,9), wf16(:,20), A(:,44), n3(:,148), t3x256(:,:,44), nhel, den(122))
    call cont_VV(nsync, wf16(:,3), wf16(:,17), A(:,45), n3(:,149), t3x256(:,:,45), nhel, den(125))
    call cont_VV(nsync, wf16(:,6), wf16(:,14), A(:,46), n3(:,150), t3x256(:,:,46), nhel, den(128))
    call cont_VV(nsync, wf16(:,3), wf16(:,19), A(:,47), n3(:,151), t3x256(:,:,47), nhel, den(129))
    call cont_VV(nsync, wf16(:,6), wf16(:,15), A(:,48), n3(:,152), t3x256(:,:,48), nhel, den(130))
    call cont_VV(nsync, wf16(:,21), wf16(:,22), A(:,49), n3(:,153), t3x256(:,:,49), nhel, den(136))
    call cont_VV(nsync, wf4(:,7), wf64(:,1), A(:,50), n3(:,154), t3x256(:,:,50), nhel, den(140))
    call cont_VV(nsync, wf4(:,3), wf64(:,2), A(:,51), n3(:,155), t3x256(:,:,51), nhel, den(144))
    call cont_VV(nsync, wf16(:,21), wf16(:,23), A(:,52), n3(:,156), t3x256(:,:,52), nhel, den(147))
    call cont_VV(nsync, wf4(:,7), wf64(:,3), A(:,53), n3(:,157), t3x256(:,:,53), nhel, den(151))
    call cont_VV(nsync, wf4(:,3), wf64(:,4), A(:,54), n3(:,158), t3x256(:,:,54), nhel, den(155))
    call cont_QA(nsync, wf8(:,38), wf32(:,21), A(:,55), n3(:,159), t3x256(:,:,55), nhel, den(157))
    call cont_QA(nsync, wf8(:,36), wf32(:,22), A(:,56), n3(:,160), t3x256(:,:,56), nhel, den(159))
    call cont_VV(nsync, wf16(:,22), wf16(:,24), A(:,57), n3(:,161), t3x256(:,:,57), nhel, den(162))
    call cont_VV(nsync, wf16(:,22), wf16(:,25), A(:,58), n3(:,162), t3x256(:,:,58), nhel, den(163))
    call cont_VV(nsync, wf16(:,23), wf16(:,24), A(:,59), n3(:,163), t3x256(:,:,59), nhel, den(165))
    call cont_VV(nsync, wf16(:,23), wf16(:,25), A(:,60), n3(:,164), t3x256(:,:,60), nhel, den(166))
    call cont_VV(nsync, wf16(:,2), wf16(:,26), A(:,61), n3(:,165), t3x256(:,:,61), nhel, den(168))
    call cont_VV(nsync, wf16(:,2), wf16(:,27), A(:,62), n3(:,166), t3x256(:,:,62), nhel, den(170))
    call cont_VV(nsync, wf16(:,3), wf16(:,26), A(:,63), n3(:,167), t3x256(:,:,63), nhel, den(171))
    call cont_VV(nsync, wf16(:,3), wf16(:,27), A(:,64), n3(:,168), t3x256(:,:,64), nhel, den(172))
    call cont_VV(nsync, wf16(:,22), wf16(:,28), A(:,65), n3(:,169), t3x256(:,:,65), nhel, den(175))
    call cont_VV(nsync, wf4(:,5), wf64(:,5), A(:,66), n3(:,170), t3x256(:,:,66), nhel, den(179))
    call cont_VV(nsync, wf4(:,6), wf64(:,6), A(:,67), n3(:,171), t3x256(:,:,67), nhel, den(183))
    call cont_VV(nsync, wf16(:,23), wf16(:,28), A(:,68), n3(:,172), t3x256(:,:,68), nhel, den(184))
    call cont_VV(nsync, wf4(:,5), wf64(:,7), A(:,69), n3(:,173), t3x256(:,:,69), nhel, den(188))
    call cont_VV(nsync, wf4(:,6), wf64(:,8), A(:,70), n3(:,174), t3x256(:,:,70), nhel, den(192))
    call cont_QA(nsync, wf8(:,48), wf32(:,23), A(:,71), n3(:,175), t3x256(:,:,71), nhel, den(194))
    call cont_QA(nsync, wf8(:,46), wf32(:,24), A(:,72), n3(:,176), t3x256(:,:,72), nhel, den(196))
    call cont_VV(nsync, wf16(:,22), wf16(:,29), A(:,73), n3(:,177), t3x256(:,:,73), nhel, den(197))
    call cont_VV(nsync, wf16(:,22), wf16(:,30), A(:,74), n3(:,178), t3x256(:,:,74), nhel, den(198))
    call cont_VV(nsync, wf16(:,23), wf16(:,29), A(:,75), n3(:,179), t3x256(:,:,75), nhel, den(199))
    call cont_VV(nsync, wf16(:,23), wf16(:,30), A(:,76), n3(:,180), t3x256(:,:,76), nhel, den(200))
    call cont_VV(nsync, wf16(:,5), wf16(:,31), A(:,77), n3(:,181), t3x256(:,:,77), nhel, den(202))
    call cont_VV(nsync, wf16(:,5), wf16(:,32), A(:,78), n3(:,182), t3x256(:,:,78), nhel, den(204))
    call cont_VV(nsync, wf16(:,6), wf16(:,32), A(:,79), n3(:,183), t3x256(:,:,79), nhel, den(205))
    call cont_VV(nsync, wf16(:,6), wf16(:,31), A(:,80), n3(:,184), t3x256(:,:,80), nhel, den(206))
    call cont_VV(nsync, wf16(:,22), wf16(:,33), A(:,81), n3(:,185), t3x256(:,:,81), nhel, den(207))
    call cont_VV(nsync, wf16(:,22), wf16(:,34), A(:,82), n3(:,186), t3x256(:,:,82), nhel, den(208))
    call cont_VV(nsync, wf16(:,23), wf16(:,33), A(:,83), n3(:,187), t3x256(:,:,83), nhel, den(209))
    call cont_VV(nsync, wf16(:,23), wf16(:,34), A(:,84), n3(:,188), t3x256(:,:,84), nhel, den(210))
    call cont_VV(nsync, wf16(:,8), wf16(:,35), A(:,85), n3(:,189), t3x256(:,:,85), nhel, den(212))
    call cont_VV(nsync, wf16(:,8), wf16(:,36), A(:,86), n3(:,190), t3x256(:,:,86), nhel, den(214))
    call cont_VV(nsync, wf16(:,9), wf16(:,35), A(:,87), n3(:,191), t3x256(:,:,87), nhel, den(215))
    call cont_VV(nsync, wf16(:,9), wf16(:,36), A(:,88), n3(:,192), t3x256(:,:,88), nhel, den(216))
    call cont_VV(nsync, wf16(:,22), wf16(:,37), A(:,89), n3(:,193), t3x256(:,:,89), nhel, den(217))
    call cont_VV(nsync, wf16(:,22), wf16(:,38), A(:,90), n3(:,194), t3x256(:,:,90), nhel, den(218))
    call cont_VV(nsync, wf16(:,23), wf16(:,37), A(:,91), n3(:,195), t3x256(:,:,91), nhel, den(219))
    call cont_VV(nsync, wf16(:,23), wf16(:,38), A(:,92), n3(:,196), t3x256(:,:,92), nhel, den(220))
    call cont_VV(nsync, wf16(:,11), wf16(:,39), A(:,93), n3(:,197), t3x256(:,:,93), nhel, den(222))
    call cont_VV(nsync, wf16(:,11), wf16(:,40), A(:,94), n3(:,198), t3x256(:,:,94), nhel, den(224))
    call cont_VV(nsync, wf16(:,12), wf16(:,40), A(:,95), n3(:,199), t3x256(:,:,95), nhel, den(225))
    call cont_VV(nsync, wf16(:,12), wf16(:,39), A(:,96), n3(:,200), t3x256(:,:,96), nhel, den(226))

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
  complex(REALKIND), intent(out) :: M1(6) ! M1helarray(6,256)
  integer :: empty(0)

  M1(1) = ((A(j,9)%j+A(j,13)%j+A(j,16)%j+A(j,18)%j+A(j,20)%j+A(j,23)%j+A(j,34)%j+A(j,40)%j+A(j,44)%j+A(j,46)%j+A(j,67)%j+A(j,69)%j &
       +A(j,71)%j+A(j,74)%j+A(j,76)%j+A(j,78)%j+A(j,79)%j+A(j,81)%j+A(j,83)%j+A(j,85)%j+A(j,87)%j)*f(1))/4._/**/REALKIND &
       +((A(j,1)%j+A(j,2)%j+A(j,4)%j+A(j,5)%j+A(j,7)%j+A(j,8)%j+A(j,33)%j+A(j,36)%j+A(j,42)%j+A(j,43)%j+A(j,50)%j+A(j,51)%j &
       +A(j,53)%j+A(j,54)%j+A(j,55)%j+A(j,56)%j+A(j,57)%j+A(j,58)%j+A(j,59)%j+A(j,60)%j+A(j,61)%j+A(j,62)%j+A(j,63)%j+A(j,64)%j &
       +A(j,93)%j+A(j,94)%j+A(j,95)%j+A(j,96)%j)*f(1))/12._/**/REALKIND+(CI*(A(j,10)%j+A(j,14)%j-A(j,19)%j-A(j,22)%j+A(j,65)%j &
       +A(j,68)%j)*f(2))/4._/**/REALKIND
  M1(2) = ((-A(j,9)%j-A(j,11)%j-A(j,12)%j-A(j,13)%j-A(j,15)%j-A(j,16)%j-A(j,38)%j-A(j,40)%j-A(j,41)%j-A(j,44)%j-A(j,66)%j &
       -A(j,67)%j-A(j,69)%j-A(j,70)%j-A(j,71)%j-A(j,72)%j-A(j,73)%j-A(j,74)%j-A(j,75)%j-A(j,76)%j-A(j,77)%j-A(j,78)%j-A(j,79)%j &
       -A(j,80)%j-A(j,85)%j-A(j,86)%j-A(j,87)%j-A(j,88)%j)*f(1))/12._/**/REALKIND+((-A(j,1)%j-A(j,5)%j-A(j,8)%j-A(j,27)%j &
       -A(j,28)%j-A(j,31)%j-A(j,36)%j-A(j,37)%j-A(j,43)%j-A(j,45)%j-A(j,50)%j-A(j,54)%j-A(j,56)%j-A(j,58)%j-A(j,60)%j-A(j,62)%j &
       -A(j,64)%j-A(j,89)%j-A(j,91)%j-A(j,93)%j-A(j,96)%j)*f(1))/4._/**/REALKIND+(CI*(-A(j,3)%j-A(j,6)%j+A(j,26)%j+A(j,30)%j &
       -A(j,49)%j-A(j,52)%j)*f(2))/4._/**/REALKIND
  M1(3) = ((-A(j,17)%j-A(j,18)%j-A(j,20)%j-A(j,21)%j-A(j,23)%j-A(j,24)%j-A(j,34)%j-A(j,35)%j-A(j,46)%j-A(j,48)%j-A(j,66)%j &
       -A(j,67)%j-A(j,69)%j-A(j,70)%j-A(j,71)%j-A(j,72)%j-A(j,77)%j-A(j,78)%j-A(j,79)%j-A(j,80)%j-A(j,81)%j-A(j,82)%j-A(j,83)%j &
       -A(j,84)%j-A(j,85)%j-A(j,86)%j-A(j,87)%j-A(j,88)%j)*f(1))/12._/**/REALKIND+((-A(j,2)%j-A(j,4)%j-A(j,7)%j-A(j,25)%j &
       -A(j,29)%j-A(j,32)%j-A(j,33)%j-A(j,39)%j-A(j,42)%j-A(j,47)%j-A(j,51)%j-A(j,53)%j-A(j,55)%j-A(j,57)%j-A(j,59)%j-A(j,61)%j &
       -A(j,63)%j-A(j,90)%j-A(j,92)%j-A(j,94)%j-A(j,95)%j)*f(1))/4._/**/REALKIND+(CI*(A(j,3)%j+A(j,6)%j-A(j,26)%j-A(j,30)%j &
       +A(j,49)%j+A(j,52)%j)*f(2))/4._/**/REALKIND
  M1(4) = ((A(j,9)%j+A(j,11)%j+A(j,12)%j+A(j,13)%j+A(j,15)%j+A(j,16)%j+A(j,17)%j+A(j,18)%j+A(j,20)%j+A(j,21)%j+A(j,23)%j+A(j,24)%j &
       +A(j,34)%j+A(j,35)%j+A(j,38)%j+A(j,40)%j+A(j,41)%j+A(j,44)%j+A(j,46)%j+A(j,48)%j+A(j,66)%j+A(j,67)%j+A(j,69)%j+A(j,70)%j &
       +A(j,71)%j+A(j,72)%j+A(j,73)%j+A(j,74)%j+A(j,75)%j+A(j,76)%j+A(j,77)%j+A(j,78)%j+A(j,79)%j+A(j,80)%j+A(j,81)%j+A(j,82)%j &
       +A(j,83)%j+A(j,84)%j+A(j,85)%j+A(j,86)%j+A(j,87)%j+A(j,88)%j)*f(1))/36._/**/REALKIND+((A(j,1)%j+A(j,2)%j+A(j,4)%j+A(j,5)%j &
       +A(j,7)%j+A(j,8)%j+A(j,25)%j+A(j,27)%j+A(j,28)%j+A(j,29)%j+A(j,31)%j+A(j,32)%j+A(j,33)%j+A(j,36)%j+A(j,37)%j+A(j,39)%j &
       +A(j,42)%j+A(j,43)%j+A(j,45)%j+A(j,47)%j+A(j,57)%j+A(j,58)%j+A(j,59)%j+A(j,60)%j+A(j,89)%j+A(j,90)%j+A(j,91)%j &
       +A(j,92)%j)*f(1))/12._/**/REALKIND
  M1(5) = ((A(j,11)%j+A(j,12)%j+A(j,15)%j+A(j,17)%j+A(j,21)%j+A(j,24)%j+A(j,35)%j+A(j,38)%j+A(j,41)%j+A(j,48)%j+A(j,66)%j &
       +A(j,70)%j+A(j,72)%j+A(j,73)%j+A(j,75)%j+A(j,77)%j+A(j,80)%j+A(j,82)%j+A(j,84)%j+A(j,86)%j+A(j,88)%j)*f(1))/4._/**/REALKIND &
       +((A(j,25)%j+A(j,27)%j+A(j,28)%j+A(j,29)%j+A(j,31)%j+A(j,32)%j+A(j,37)%j+A(j,39)%j+A(j,45)%j+A(j,47)%j+A(j,50)%j+A(j,51)%j &
       +A(j,53)%j+A(j,54)%j+A(j,55)%j+A(j,56)%j+A(j,61)%j+A(j,62)%j+A(j,63)%j+A(j,64)%j+A(j,89)%j+A(j,90)%j+A(j,91)%j+A(j,92)%j &
       +A(j,93)%j+A(j,94)%j+A(j,95)%j+A(j,96)%j)*f(1))/12._/**/REALKIND+(CI*(-A(j,10)%j-A(j,14)%j+A(j,19)%j+A(j,22)%j-A(j,65)%j &
       -A(j,68)%j)*f(2))/4._/**/REALKIND
  M1(6) = ((-A(j,9)%j-A(j,11)%j-A(j,12)%j-A(j,13)%j-A(j,15)%j-A(j,16)%j-A(j,17)%j-A(j,18)%j-A(j,20)%j-A(j,21)%j-A(j,23)%j &
       -A(j,24)%j-A(j,34)%j-A(j,35)%j-A(j,38)%j-A(j,40)%j-A(j,41)%j-A(j,44)%j-A(j,46)%j-A(j,48)%j-A(j,73)%j-A(j,74)%j-A(j,75)%j &
       -A(j,76)%j-A(j,81)%j-A(j,82)%j-A(j,83)%j-A(j,84)%j)*f(1))/12._/**/REALKIND+((-A(j,1)%j-A(j,2)%j-A(j,4)%j-A(j,5)%j-A(j,7)%j &
       -A(j,8)%j-A(j,25)%j-A(j,27)%j-A(j,28)%j-A(j,29)%j-A(j,31)%j-A(j,32)%j-A(j,33)%j-A(j,36)%j-A(j,37)%j-A(j,39)%j-A(j,42)%j &
       -A(j,43)%j-A(j,45)%j-A(j,47)%j-A(j,50)%j-A(j,51)%j-A(j,53)%j-A(j,54)%j-A(j,55)%j-A(j,56)%j-A(j,57)%j-A(j,58)%j-A(j,59)%j &
       -A(j,60)%j-A(j,61)%j-A(j,62)%j-A(j,63)%j-A(j,64)%j-A(j,89)%j-A(j,90)%j-A(j,91)%j-A(j,92)%j-A(j,93)%j-A(j,94)%j-A(j,95)%j &
       -A(j,96)%j)*f(1))/36._/**/REALKIND

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
  use ol_colourmatrix_ppllttj_nenexttxdddxdx_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(6)
  real(REALKIND),    intent(out) :: M2colint(0:38-1)
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
  use ol_colourmatrix_ppllttj_nenexttxdddxdx_1_/**/REALKIND, only: K1
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
  & bind(c,name="ol_tree_colvect_ppllttj_nenexttxdddxdx_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(6,256)
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
    & bind(c,name="ol_f_amp2tree_ppllttj_nenexttxdddxdx_1")
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
    & bind(c,name="ol_f_amp2ccone_ppllttj_nenexttxdddxdx_1")
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
    & bind(c,name="ol_f_amp2ccall_ppllttj_nenexttxdddxdx_1")
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
    & bind(c,name="ol_f_amp2hcone_ppllttj_nenexttxdddxdx_1")
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
    & bind(c,name="ol_f_amp2hcall_ppllttj_nenexttxdddxdx_1")
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
    & bind(c,name="ol_amp2tree_ppllttj_nenexttxdddxdx_1")
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
    & bind(c,name="ol_amp2ccone_ppllttj_nenexttxdddxdx_1")
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
    & bind(c,name="ol_amp2ccall_ppllttj_nenexttxdddxdx_1")
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
    & bind(c,name="ol_amp2hcone_ppllttj_nenexttxdddxdx_1")
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
    & bind(c,name="ol_amp2hcall_ppllttj_nenexttxdddxdx_1")
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
    & bind(c,name="amp2tree_ppllttj_nenexttxdddxdx_1_")
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
    & bind(c,name="amp2ccone_ppllttj_nenexttxdddxdx_1_")
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
    & bind(c,name="amp2ccall_ppllttj_nenexttxdddxdx_1_")
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
    & bind(c,name="amp2hcone_ppllttj_nenexttxdddxdx_1_")
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
    & bind(c,name="amp2hcall_ppllttj_nenexttxdddxdx_1_")
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

end module ol_tree_ppllttj_nenexttxdddxdx_1_/**/REALKIND
