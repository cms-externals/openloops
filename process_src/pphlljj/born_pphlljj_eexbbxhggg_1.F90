
module ol_colourmatrix_pphlljj_eexbbxhggg_1_/**/REALKIND
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

  K1(  1,:) = [  384,  -48,  -48,    6,    6,   60]
  K1(  2,:) = [  -48,  384,    6,   60,  -48,    6]
  K1(  3,:) = [  -48,    6,  384,  -48,   60,    6]
  K1(  4,:) = [    6,   60,  -48,  384,    6,  -48]
  K1(  5,:) = [    6,  -48,   60,    6,  384,  -48]
  K1(  6,:) = [   60,    6,    6,  -48,  -48,  384]
  K1(  7,:) = [    0,    0,    0,    0,    0,    0]
  K1(  8,:) = [    0,    0,    0,    0,    0,    0]
  K1(  9,:) = [    0,    0,    0,    0,    0,    0]
  K1( 10,:) = [    0,    0,    0,    0,    0,    0]
  K1( 11,:) = [    0,    0,    0,    0,    0,    0]
  K1( 12,:) = [    0,    0,    0,    0,    0,    0]
  K1( 13,:) = [    0,    0,    0,    0,    0,    0]
  K1( 14,:) = [    0,    0,    0,    0,    0,    0]
  K1( 15,:) = [    0,    0,    0,    0,    0,    0]
  K1( 16,:) = [    0,    0,    0,    0,    0,    0]
  K1( 17,:) = [    0,    0,    0,    0,    0,    0]
  K1( 18,:) = [    0,    0,    0,    0,    0,    0]
  K1( 19,:) = [    0,    0,    0,    0,    0,    0]
  K1( 20,:) = [    0,    0,    0,    0,    0,    0]
  K1( 21,:) = [    0,    0,    0,    0,    0,    0]
  K1( 22,:) = [    0,    0,    0,    0,    0,    0]
  K1( 23,:) = [    0,    0,    0,    0,    0,    0]
  K1( 24,:) = [    0,    0,    0,    0,    0,    0]
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
  K1( 37,:) = [  512,  -64,  -64,    8,    8,   80]
  K1( 38,:) = [  -64,  512,    8,   80,  -64,    8]
  K1( 39,:) = [  -64,    8,  512,  -64,   80,    8]
  K1( 40,:) = [    8,   80,  -64,  512,    8,  -64]
  K1( 41,:) = [    8,  -64,   80,    8,  512,  -64]
  K1( 42,:) = [   80,    8,    8,  -64,  -64,  512]
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
  K1( 55,:) = [    1,   10,   10,  -62,  -62,   28]
  K1( 56,:) = [   10,    1,  -62,   28,   10,  -62]
  K1( 57,:) = [   10,  -62,    1,   10,   28,  -62]
  K1( 58,:) = [  -62,   28,   10,    1,  -62,   10]
  K1( 59,:) = [  -62,   10,   28,  -62,    1,   10]
  K1( 60,:) = [   28,  -62,  -62,   10,   10,    1]
  K1( 61,:) = [  512,  -64,  -64,    8,    8,   80]
  K1( 62,:) = [  -64,  512,    8,   80,  -64,    8]
  K1( 63,:) = [  -64,    8,  512,  -64,   80,    8]
  K1( 64,:) = [    8,   80,  -64,  512,    8,  -64]
  K1( 65,:) = [    8,  -64,   80,    8,  512,  -64]
  K1( 66,:) = [   80,    8,    8,  -64,  -64,  512]
  K1( 67,:) = [    0,    0,    0,    0,    0,    0]
  K1( 68,:) = [    0,    0,    0,    0,    0,    0]
  K1( 69,:) = [    0,    0,    0,    0,    0,    0]
  K1( 70,:) = [    0,    0,    0,    0,    0,    0]
  K1( 71,:) = [    0,    0,    0,    0,    0,    0]
  K1( 72,:) = [    0,    0,    0,    0,    0,    0]
  K1( 73,:) = [    0,    0,    0,    0,    0,    0]
  K1( 74,:) = [    0,    0,    0,    0,    0,    0]
  K1( 75,:) = [    0,    0,    0,    0,    0,    0]
  K1( 76,:) = [    0,    0,    0,    0,    0,    0]
  K1( 77,:) = [    0,    0,    0,    0,    0,    0]
  K1( 78,:) = [    0,    0,    0,    0,    0,    0]
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
  K1( 91,:) = [    0,    0,    0,    0,    0,    0]
  K1( 92,:) = [    0,    0,    0,    0,    0,    0]
  K1( 93,:) = [    0,    0,    0,    0,    0,    0]
  K1( 94,:) = [    0,    0,    0,    0,    0,    0]
  K1( 95,:) = [    0,    0,    0,    0,    0,    0]
  K1( 96,:) = [    0,    0,    0,    0,    0,    0]
  K1( 97,:) = [    0,    0,    0,    0,    0,    0]
  K1( 98,:) = [    0,    0,    0,    0,    0,    0]
  K1( 99,:) = [    0,    0,    0,    0,    0,    0]
  K1(100,:) = [    0,    0,    0,    0,    0,    0]
  K1(101,:) = [    0,    0,    0,    0,    0,    0]
  K1(102,:) = [    0,    0,    0,    0,    0,    0]
  K1(103,:) = [    0,    0,    0,    0,    0,    0]
  K1(104,:) = [    0,    0,    0,    0,    0,    0]
  K1(105,:) = [    0,    0,    0,    0,    0,    0]
  K1(106,:) = [    0,    0,    0,    0,    0,    0]
  K1(107,:) = [    0,    0,    0,    0,    0,    0]
  K1(108,:) = [    0,    0,    0,    0,    0,    0]
  K1(109,:) = [   -9,  -90,   -9,   -9,   72,  -90]
  K1(110,:) = [  -90,   -9,   72,  -90,   -9,   -9]
  K1(111,:) = [   -9,   72,   72,   72,   72,   -9]
  K1(112,:) = [   -9,  -90,   72, -576,   -9,   72]
  K1(113,:) = [   72,   -9,   72,   -9,   72,   72]
  K1(114,:) = [  -90,   -9,   -9,   72,   72, -576]
  K1(115,:) = [ -576,   72,   72,   -9,   -9,  -90]
  K1(116,:) = [   72, -576,   -9,  -90,   72,   -9]
  K1(117,:) = [   72,   -9,   72,   -9,   72,   72]
  K1(118,:) = [   -9,  -90,   -9,   -9,   72,  -90]
  K1(119,:) = [   -9,   72,   72,   72,   72,   -9]
  K1(120,:) = [  -90,   -9,   72,  -90,   -9,   -9]
  K1(121,:) = [    0,    0,    0,    0,    0,    0]
  K1(122,:) = [    0,    0,    0,    0,    0,    0]
  K1(123,:) = [    0,    0,    0,    0,    0,    0]
  K1(124,:) = [    0,    0,    0,    0,    0,    0]
  K1(125,:) = [    0,    0,    0,    0,    0,    0]
  K1(126,:) = [    0,    0,    0,    0,    0,    0]
  K1(127,:) = [ 1152, -144, -144,   18,   18,  180]
  K1(128,:) = [ -144, 1152,   18,  180, -144,   18]
  K1(129,:) = [ -144,   18, 1152, -144,  180,   18]
  K1(130,:) = [   18,  180, -144, 1152,   18, -144]
  K1(131,:) = [   18, -144,  180,   18, 1152, -144]
  K1(132,:) = [  180,   18,   18, -144, -144, 1152]
  K1(133,:) = [    0,    0,    0,    0,    0,    0]
  K1(134,:) = [    0,    0,    0,    0,    0,    0]
  K1(135,:) = [    0,    0,    0,    0,    0,    0]
  K1(136,:) = [    0,    0,    0,    0,    0,    0]
  K1(137,:) = [    0,    0,    0,    0,    0,    0]
  K1(138,:) = [    0,    0,    0,    0,    0,    0]
  K1(139,:) = [    0,    0,    0,    0,    0,    0]
  K1(140,:) = [    0,    0,    0,    0,    0,    0]
  K1(141,:) = [    0,    0,    0,    0,    0,    0]
  K1(142,:) = [    0,    0,    0,    0,    0,    0]
  K1(143,:) = [    0,    0,    0,    0,    0,    0]
  K1(144,:) = [    0,    0,    0,    0,    0,    0]
  K1(145,:) = [   72,   72,   -9,   72,   -9,   72]
  K1(146,:) = [   72, -576,   -9,  -90,   72,   -9]
  K1(147,:) = [   -9,   -9,   -9,  -90,  -90,   72]
  K1(148,:) = [   72,  -90,  -90,   -9,   -9,   -9]
  K1(149,:) = [   -9,   72,  -90,   -9, -576,   72]
  K1(150,:) = [   72,   -9,   72,   -9,   72,   72]
  K1(151,:) = [   72,   -9,   72,   -9,   72,   72]
  K1(152,:) = [   -9,   -9,   -9,  -90,  -90,   72]
  K1(153,:) = [   72,   -9, -576,   72,  -90,   -9]
  K1(154,:) = [   -9,  -90,   72, -576,   -9,   72]
  K1(155,:) = [   72,  -90,  -90,   -9,   -9,   -9]
  K1(156,:) = [   72,   72,   -9,   72,   -9,   72]
  K1(157,:) = [    0,    0,    0,    0,    0,    0]
  K1(158,:) = [    0,    0,    0,    0,    0,    0]
  K1(159,:) = [    0,    0,    0,    0,    0,    0]
  K1(160,:) = [    0,    0,    0,    0,    0,    0]
  K1(161,:) = [    0,    0,    0,    0,    0,    0]
  K1(162,:) = [    0,    0,    0,    0,    0,    0]
  K1(163,:) = [ -648,   81,    0,    0,  -81, -162]
  K1(164,:) = [   81,   81,    0,  162,   81,    0]
  K1(165,:) = [    0,    0, -648,   81, -162,  -81]
  K1(166,:) = [    0,  162,   81,   81,    0,   81]
  K1(167,:) = [  -81,   81, -162,    0, -648,    0]
  K1(168,:) = [ -162,    0,  -81,   81,    0, -648]
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
  K1(181,:) = [    0,    0,    0,    0,    0,    0]
  K1(182,:) = [    0,    0,    0,    0,    0,    0]
  K1(183,:) = [    0,    0,    0,    0,    0,    0]
  K1(184,:) = [    0,    0,    0,    0,    0,    0]
  K1(185,:) = [    0,    0,    0,    0,    0,    0]
  K1(186,:) = [    0,    0,    0,    0,    0,    0]
  K1(187,:) = [ -576,   72,   72,   -9,   -9,  -90]
  K1(188,:) = [   72,   72,   -9,   72,   -9,   72]
  K1(189,:) = [   72,   -9, -576,   72,  -90,   -9]
  K1(190,:) = [   -9,   72,   72,   72,   72,   -9]
  K1(191,:) = [   -9,   -9,  -90,   72,   -9,  -90]
  K1(192,:) = [  -90,   72,   -9,   -9,  -90,   -9]
  K1(193,:) = [   -9,   -9,  -90,   72,   -9,  -90]
  K1(194,:) = [   -9,   72,   72,   72,   72,   -9]
  K1(195,:) = [  -90,   72,   -9,   -9,  -90,   -9]
  K1(196,:) = [   72,   72,   -9,   72,   -9,   72]
  K1(197,:) = [   -9,   72,  -90,   -9, -576,   72]
  K1(198,:) = [  -90,   -9,   -9,   72,   72, -576]
  K1(199,:) = [    0,    0,    0,    0,    0,    0]
  K1(200,:) = [    0,    0,    0,    0,    0,    0]
  K1(201,:) = [    0,    0,    0,    0,    0,    0]
  K1(202,:) = [    0,    0,    0,    0,    0,    0]
  K1(203,:) = [    0,    0,    0,    0,    0,    0]
  K1(204,:) = [    0,    0,    0,    0,    0,    0]
  K1(205,:) = [   81,   81,   81,    0,    0,  162]
  K1(206,:) = [   81, -648,  -81, -162,    0,    0]
  K1(207,:) = [   81,  -81, -648,    0, -162,    0]
  K1(208,:) = [    0, -162,    0, -648,  -81,   81]
  K1(209,:) = [    0,    0, -162,  -81, -648,   81]
  K1(210,:) = [  162,    0,    0,   81,   81,   81]
  K1(211,:) = [ -648,    0,   81,  -81,    0, -162]
  K1(212,:) = [    0, -648,    0, -162,   81,  -81]
  K1(213,:) = [   81,    0,   81,   81,  162,    0]
  K1(214,:) = [  -81, -162,   81, -648,    0,    0]
  K1(215,:) = [    0,   81,  162,    0,   81,   81]
  K1(216,:) = [ -162,  -81,    0,    0,   81, -648]
  K1(217,:) = [ 1152, -144, -144,   18,   18,  180]
  K1(218,:) = [ -144, 1152,   18,  180, -144,   18]
  K1(219,:) = [ -144,   18, 1152, -144,  180,   18]
  K1(220,:) = [   18,  180, -144, 1152,   18, -144]
  K1(221,:) = [   18, -144,  180,   18, 1152, -144]
  K1(222,:) = [  180,   18,   18, -144, -144, 1152]
  K1(223,:) = [    0,    0,    0,    0,    0,    0]
  K1(224,:) = [    0,    0,    0,    0,    0,    0]
  K1(225,:) = [    0,    0,    0,    0,    0,    0]
  K1(226,:) = [    0,    0,    0,    0,    0,    0]
  K1(227,:) = [    0,    0,    0,    0,    0,    0]
  K1(228,:) = [    0,    0,    0,    0,    0,    0]
  K1 = (1._/**/REALKIND / 54) * K1

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphlljj_eexbbxhggg_1_/**/REALKIND



module ol_forced_parameters_pphlljj_eexbbxhggg_1_/**/REALKIND
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
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphlljj_eexbbxhggg_1_/**/REALKIND

module ol_tree_pphlljj_eexbbxhggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(6)
  complex(REALKIND), save :: den(759)
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
    f(1) = (CI*eQED**3*gQCD**3*lambdaHZZ*MW)/(cw**2*sw)
    f(2) = (eQED**3*gQCD**3*lambdaHZZ*MW)/(cw**2*sw)
    f(3) = (CI*eQED**3*gQCD**3*YB)/(6._/**/REALKIND*MW*sw)
    f(4) = (CI*eQED**3*gQCD**3*YB)/(2._/**/REALKIND*MW*sw)
    f(5) = (eQED**3*gQCD**3*YB)/(MW*sw*6._/**/REALKIND)
    f(6) = (eQED**3*gQCD**3*YB)/(MW*sw*2._/**/REALKIND)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,3))
  den(2) = 1 / (Q(5,20) - MB2)
  den(3) = 1 / (Q(5,224))
  den(5) = 1 / (Q(5,11) - MB2)
  den(8) = 1 / (Q(5,3) - MZ2)
  den(12) = 1 / (Q(5,232) - MB2)
  den(17) = 1 / (Q(5,24) - MB2)
  den(19) = 1 / (Q(5,7) - MB2)
  den(25) = 1 / (Q(5,228) - MB2)
  den(34) = 1 / (Q(5,19) - MZ2)
  den(38) = 1 / (Q(5,40) - MB2)
  den(39) = 1 / (Q(5,192))
  den(41) = 1 / (Q(5,23) - MB2)
  den(48) = 1 / (Q(5,43) - MB2)
  den(54) = 1 / (Q(5,84) - MB2)
  den(56) = 1 / (Q(5,168) - MB2)
  den(64) = 1 / (Q(5,104) - MB2)
  den(66) = 1 / (Q(5,148) - MB2)
  den(78) = 1 / (Q(5,72) - MB2)
  den(79) = 1 / (Q(5,160))
  den(85) = 1 / (Q(5,75) - MB2)
  den(91) = 1 / (Q(5,52) - MB2)
  den(93) = 1 / (Q(5,200) - MB2)
  den(112) = 1 / (Q(5,136) - MB2)
  den(113) = 1 / (Q(5,96))
  den(119) = 1 / (Q(5,139) - MB2)
  den(129) = 1 / (Q(5,116) - MB2)
  den(167) = 1 / (Q(5,180) - MB2)
  den(187) = 1 / (Q(5,212) - MB2)
  den(222) = 1 / (Q(5,36) - MB2)
  den(225) = 1 / (Q(5,39) - MB2)
  den(232) = 1 / (Q(5,27) - MB2)
  den(237) = 1 / (Q(5,100) - MB2)
  den(239) = 1 / (Q(5,152) - MB2)
  den(247) = 1 / (Q(5,88) - MB2)
  den(249) = 1 / (Q(5,164) - MB2)
  den(261) = 1 / (Q(5,68) - MB2)
  den(264) = 1 / (Q(5,71) - MB2)
  den(280) = 1 / (Q(5,56) - MB2)
  den(282) = 1 / (Q(5,196) - MB2)
  den(294) = 1 / (Q(5,132) - MB2)
  den(297) = 1 / (Q(5,135) - MB2)
  den(306) = 1 / (Q(5,120) - MB2)
  den(343) = 1 / (Q(5,184) - MB2)
  den(362) = 1 / (Q(5,216) - MB2)

  ! denominators

  den(4) = den(2)*den(3)
  den(6) = den(1)*den(5)
  den(7) = den(4)*den(6)
  den(9) = den(5)*den(8)
  den(10) = den(4)*den(9)
  den(11) = den(1)*den(2)
  den(13) = den(3)*den(12)
  den(14) = den(11)*den(13)
  den(15) = den(2)*den(8)
  den(16) = den(13)*den(15)
  den(18) = den(3)*den(17)
  den(20) = den(1)*den(19)
  den(21) = den(18)*den(20)
  den(22) = den(8)*den(19)
  den(23) = den(18)*den(22)
  den(24) = den(1)*den(17)
  den(26) = den(3)*den(25)
  den(27) = den(24)*den(26)
  den(28) = den(8)*den(17)
  den(29) = den(26)*den(28)
  den(30) = den(13)*den(20)
  den(31) = den(13)*den(22)
  den(32) = den(6)*den(26)
  den(33) = den(9)*den(26)
  den(35) = den(8)*den(34)
  den(36) = den(13)*den(35)
  den(37) = den(26)*den(35)
  den(40) = den(38)*den(39)
  den(42) = den(11)*den(41)
  den(43) = den(40)*den(42)
  den(44) = den(15)*den(41)
  den(45) = den(40)*den(44)
  den(46) = den(1)*den(38)
  den(47) = den(2)*den(39)
  den(49) = den(46)*den(48)
  den(50) = den(47)*den(49)
  den(51) = den(8)*den(38)
  den(52) = den(48)*den(51)
  den(53) = den(47)*den(52)
  den(55) = den(2)*den(54)
  den(57) = den(38)*den(56)
  den(58) = den(1)*den(55)
  den(59) = den(57)*den(58)
  den(60) = den(8)*den(55)
  den(61) = den(57)*den(60)
  den(62) = den(49)*den(55)
  den(63) = den(52)*den(55)
  den(65) = den(38)*den(64)
  den(67) = den(2)*den(66)
  den(68) = den(1)*den(65)
  den(69) = den(67)*den(68)
  den(70) = den(8)*den(65)
  den(71) = den(67)*den(70)
  den(72) = den(49)*den(67)
  den(73) = den(52)*den(67)
  den(74) = den(42)*den(65)
  den(75) = den(44)*den(65)
  den(76) = den(42)*den(57)
  den(77) = den(44)*den(57)
  den(80) = den(78)*den(79)
  den(81) = den(42)*den(80)
  den(82) = den(44)*den(80)
  den(83) = den(1)*den(78)
  den(84) = den(2)*den(79)
  den(86) = den(83)*den(85)
  den(87) = den(84)*den(86)
  den(88) = den(8)*den(78)
  den(89) = den(85)*den(88)
  den(90) = den(84)*den(89)
  den(92) = den(2)*den(91)
  den(94) = den(78)*den(93)
  den(95) = den(1)*den(92)
  den(96) = den(94)*den(95)
  den(97) = den(8)*den(92)
  den(98) = den(94)*den(97)
  den(99) = den(86)*den(92)
  den(100) = den(89)*den(92)
  den(101) = den(64)*den(78)
  den(102) = den(1)*den(101)
  den(103) = den(67)*den(102)
  den(104) = den(8)*den(101)
  den(105) = den(67)*den(104)
  den(106) = den(67)*den(86)
  den(107) = den(67)*den(89)
  den(108) = den(42)*den(101)
  den(109) = den(44)*den(101)
  den(110) = den(42)*den(94)
  den(111) = den(44)*den(94)
  den(114) = den(112)*den(113)
  den(115) = den(42)*den(114)
  den(116) = den(44)*den(114)
  den(117) = den(1)*den(112)
  den(118) = den(2)*den(113)
  den(120) = den(117)*den(119)
  den(121) = den(118)*den(120)
  den(122) = den(8)*den(112)
  den(123) = den(119)*den(122)
  den(124) = den(118)*den(123)
  den(125) = den(6)*den(113)
  den(126) = den(67)*den(125)
  den(127) = den(9)*den(113)
  den(128) = den(67)*den(127)
  den(130) = den(118)*den(129)
  den(131) = den(6)*den(130)
  den(132) = den(9)*den(130)
  den(133) = den(3)*den(113)
  den(134) = den(2)*den(6)
  den(135) = den(133)*den(134)
  den(136) = den(2)*den(9)
  den(137) = den(133)*den(136)
  den(138) = den(64)*den(113)
  den(139) = den(1)*den(138)
  den(140) = den(67)*den(139)
  den(141) = den(8)*den(138)
  den(142) = den(67)*den(141)
  den(143) = den(42)*den(138)
  den(144) = den(44)*den(138)
  den(145) = den(42)*den(133)
  den(146) = den(44)*den(133)
  den(147) = den(93)*den(112)
  den(148) = den(95)*den(147)
  den(149) = den(97)*den(147)
  den(150) = den(92)*den(120)
  den(151) = den(92)*den(123)
  den(152) = den(56)*den(112)
  den(153) = den(1)*den(152)
  den(154) = den(55)*den(153)
  den(155) = den(8)*den(152)
  den(156) = den(55)*den(155)
  den(157) = den(55)*den(120)
  den(158) = den(55)*den(123)
  den(159) = den(42)*den(152)
  den(160) = den(44)*den(152)
  den(161) = den(42)*den(147)
  den(162) = den(44)*den(147)
  den(163) = den(6)*den(79)
  den(164) = den(55)*den(163)
  den(165) = den(9)*den(79)
  den(166) = den(55)*den(165)
  den(168) = den(84)*den(167)
  den(169) = den(6)*den(168)
  den(170) = den(9)*den(168)
  den(171) = den(3)*den(79)
  den(172) = den(134)*den(171)
  den(173) = den(136)*den(171)
  den(174) = den(56)*den(79)
  den(175) = den(1)*den(174)
  den(176) = den(55)*den(175)
  den(177) = den(8)*den(174)
  den(178) = den(55)*den(177)
  den(179) = den(42)*den(174)
  den(180) = den(44)*den(174)
  den(181) = den(42)*den(171)
  den(182) = den(44)*den(171)
  den(183) = den(6)*den(92)
  den(184) = den(39)*den(183)
  den(185) = den(9)*den(92)
  den(186) = den(39)*den(185)
  den(188) = den(47)*den(187)
  den(189) = den(6)*den(188)
  den(190) = den(9)*den(188)
  den(191) = den(3)*den(39)
  den(192) = den(134)*den(191)
  den(193) = den(136)*den(191)
  den(194) = den(39)*den(93)
  den(195) = den(1)*den(194)
  den(196) = den(92)*den(195)
  den(197) = den(8)*den(194)
  den(198) = den(92)*den(197)
  den(199) = den(42)*den(194)
  den(200) = den(44)*den(194)
  den(201) = den(42)*den(191)
  den(202) = den(44)*den(191)
  den(203) = den(6)*den(85)
  den(204) = den(92)*den(203)
  den(205) = den(9)*den(85)
  den(206) = den(92)*den(205)
  den(207) = den(92)*den(129)
  den(208) = den(6)*den(207)
  den(209) = den(9)*den(207)
  den(210) = den(6)*den(48)
  den(211) = den(55)*den(210)
  den(212) = den(9)*den(48)
  den(213) = den(55)*den(212)
  den(214) = den(55)*den(129)
  den(215) = den(6)*den(214)
  den(216) = den(9)*den(214)
  den(217) = den(67)*den(210)
  den(218) = den(67)*den(212)
  den(219) = den(67)*den(167)
  den(220) = den(6)*den(219)
  den(221) = den(9)*den(219)
  den(223) = den(1)*den(222)
  den(224) = den(17)*den(39)
  den(226) = den(223)*den(225)
  den(227) = den(224)*den(226)
  den(228) = den(8)*den(222)
  den(229) = den(225)*den(228)
  den(230) = den(224)*den(229)
  den(231) = den(39)*den(222)
  den(233) = den(24)*den(232)
  den(234) = den(231)*den(233)
  den(235) = den(28)*den(232)
  den(236) = den(231)*den(235)
  den(238) = den(222)*den(237)
  den(240) = den(17)*den(239)
  den(241) = den(1)*den(238)
  den(242) = den(240)*den(241)
  den(243) = den(8)*den(238)
  den(244) = den(240)*den(243)
  den(245) = den(233)*den(238)
  den(246) = den(235)*den(238)
  den(248) = den(17)*den(247)
  den(250) = den(222)*den(249)
  den(251) = den(1)*den(248)
  den(252) = den(250)*den(251)
  den(253) = den(8)*den(248)
  den(254) = den(250)*den(253)
  den(255) = den(233)*den(250)
  den(256) = den(235)*den(250)
  den(257) = den(226)*den(248)
  den(258) = den(229)*den(248)
  den(259) = den(226)*den(240)
  den(260) = den(229)*den(240)
  den(262) = den(1)*den(261)
  den(263) = den(17)*den(79)
  den(265) = den(262)*den(264)
  den(266) = den(263)*den(265)
  den(267) = den(8)*den(261)
  den(268) = den(264)*den(267)
  den(269) = den(263)*den(268)
  den(270) = den(79)*den(261)
  den(271) = den(233)*den(270)
  den(272) = den(235)*den(270)
  den(273) = den(237)*den(261)
  den(274) = den(1)*den(273)
  den(275) = den(240)*den(274)
  den(276) = den(8)*den(273)
  den(277) = den(240)*den(276)
  den(278) = den(233)*den(273)
  den(279) = den(235)*den(273)
  den(281) = den(17)*den(280)
  den(283) = den(261)*den(282)
  den(284) = den(1)*den(281)
  den(285) = den(283)*den(284)
  den(286) = den(8)*den(281)
  den(287) = den(283)*den(286)
  den(288) = den(233)*den(283)
  den(289) = den(235)*den(283)
  den(290) = den(265)*den(281)
  den(291) = den(268)*den(281)
  den(292) = den(240)*den(265)
  den(293) = den(240)*den(268)
  den(295) = den(1)*den(294)
  den(296) = den(17)*den(113)
  den(298) = den(295)*den(297)
  den(299) = den(296)*den(298)
  den(300) = den(8)*den(294)
  den(301) = den(297)*den(300)
  den(302) = den(296)*den(301)
  den(303) = den(113)*den(294)
  den(304) = den(233)*den(303)
  den(305) = den(235)*den(303)
  den(307) = den(296)*den(306)
  den(308) = den(20)*den(307)
  den(309) = den(22)*den(307)
  den(310) = den(20)*den(113)
  den(311) = den(240)*den(310)
  den(312) = den(22)*den(113)
  den(313) = den(240)*den(312)
  den(314) = den(17)*den(20)
  den(315) = den(133)*den(314)
  den(316) = den(17)*den(22)
  den(317) = den(133)*den(316)
  den(318) = den(113)*den(237)
  den(319) = den(233)*den(318)
  den(320) = den(235)*den(318)
  den(321) = den(1)*den(318)
  den(322) = den(240)*den(321)
  den(323) = den(8)*den(318)
  den(324) = den(240)*den(323)
  den(325) = den(133)*den(233)
  den(326) = den(133)*den(235)
  den(327) = den(249)*den(294)
  den(328) = den(1)*den(327)
  den(329) = den(248)*den(328)
  den(330) = den(8)*den(327)
  den(331) = den(248)*den(330)
  den(332) = den(233)*den(327)
  den(333) = den(235)*den(327)
  den(334) = den(282)*den(294)
  den(335) = den(284)*den(334)
  den(336) = den(286)*den(334)
  den(337) = den(233)*den(334)
  den(338) = den(235)*den(334)
  den(339) = den(281)*den(298)
  den(340) = den(281)*den(301)
  den(341) = den(248)*den(298)
  den(342) = den(248)*den(301)
  den(344) = den(263)*den(343)
  den(345) = den(20)*den(344)
  den(346) = den(22)*den(344)
  den(347) = den(20)*den(79)
  den(348) = den(248)*den(347)
  den(349) = den(22)*den(79)
  den(350) = den(248)*den(349)
  den(351) = den(171)*den(314)
  den(352) = den(171)*den(316)
  den(353) = den(79)*den(249)
  den(354) = den(233)*den(353)
  den(355) = den(235)*den(353)
  den(356) = den(1)*den(353)
  den(357) = den(248)*den(356)
  den(358) = den(8)*den(353)
  den(359) = den(248)*den(358)
  den(360) = den(171)*den(233)
  den(361) = den(171)*den(235)
  den(363) = den(224)*den(362)
  den(364) = den(20)*den(363)
  den(365) = den(22)*den(363)
  den(366) = den(20)*den(281)
  den(367) = den(39)*den(366)
  den(368) = den(22)*den(281)
  den(369) = den(39)*den(368)
  den(370) = den(191)*den(314)
  den(371) = den(191)*den(316)
  den(372) = den(39)*den(282)
  den(373) = den(233)*den(372)
  den(374) = den(235)*den(372)
  den(375) = den(191)*den(233)
  den(376) = den(191)*den(235)
  den(377) = den(1)*den(372)
  den(378) = den(281)*den(377)
  den(379) = den(8)*den(372)
  den(380) = den(281)*den(379)
  den(381) = den(20)*den(225)
  den(382) = den(248)*den(381)
  den(383) = den(22)*den(225)
  den(384) = den(248)*den(383)
  den(385) = den(240)*den(381)
  den(386) = den(240)*den(383)
  den(387) = den(20)*den(264)
  den(388) = den(281)*den(387)
  den(389) = den(22)*den(264)
  den(390) = den(281)*den(389)
  den(391) = den(240)*den(343)
  den(392) = den(20)*den(391)
  den(393) = den(22)*den(391)
  den(394) = den(281)*den(306)
  den(395) = den(20)*den(394)
  den(396) = den(22)*den(394)
  den(397) = den(248)*den(306)
  den(398) = den(20)*den(397)
  den(399) = den(22)*den(397)
  den(400) = den(35)*den(78)
  den(401) = den(250)*den(400)
  den(402) = den(35)*den(222)
  den(403) = den(94)*den(402)
  den(404) = den(91)*den(222)
  den(405) = den(1)*den(404)
  den(406) = den(94)*den(405)
  den(407) = den(8)*den(404)
  den(408) = den(94)*den(407)
  den(409) = den(86)*den(404)
  den(410) = den(89)*den(404)
  den(411) = den(78)*den(247)
  den(412) = den(1)*den(411)
  den(413) = den(250)*den(412)
  den(414) = den(8)*den(411)
  den(415) = den(250)*den(414)
  den(416) = den(86)*den(250)
  den(417) = den(89)*den(250)
  den(418) = den(226)*den(411)
  den(419) = den(229)*den(411)
  den(420) = den(94)*den(226)
  den(421) = den(94)*den(229)
  den(422) = den(35)*den(112)
  den(423) = den(238)*den(422)
  den(424) = den(147)*den(402)
  den(425) = den(147)*den(405)
  den(426) = den(147)*den(407)
  den(427) = den(120)*den(404)
  den(428) = den(123)*den(404)
  den(429) = den(112)*den(239)
  den(430) = den(1)*den(429)
  den(431) = den(238)*den(430)
  den(432) = den(8)*den(429)
  den(433) = den(238)*den(432)
  den(434) = den(120)*den(238)
  den(435) = den(123)*den(238)
  den(436) = den(226)*den(429)
  den(437) = den(229)*den(429)
  den(438) = den(147)*den(226)
  den(439) = den(147)*den(229)
  den(440) = den(6)*den(404)
  den(441) = den(39)*den(440)
  den(442) = den(9)*den(404)
  den(443) = den(39)*den(442)
  den(444) = den(25)*den(231)
  den(445) = den(6)*den(444)
  den(446) = den(9)*den(444)
  den(447) = den(35)*den(444)
  den(448) = den(194)*den(222)
  den(449) = den(35)*den(448)
  den(450) = den(195)*den(404)
  den(451) = den(197)*den(404)
  den(452) = den(194)*den(226)
  den(453) = den(194)*den(229)
  den(454) = den(203)*den(404)
  den(455) = den(205)*den(404)
  den(456) = den(129)*den(404)
  den(457) = den(6)*den(456)
  den(458) = den(9)*den(456)
  den(459) = den(6)*den(232)
  den(460) = den(238)*den(459)
  den(461) = den(9)*den(232)
  den(462) = den(238)*den(461)
  den(463) = den(129)*den(238)
  den(464) = den(6)*den(463)
  den(465) = den(9)*den(463)
  den(466) = den(250)*den(459)
  den(467) = den(250)*den(461)
  den(468) = den(167)*den(250)
  den(469) = den(6)*den(468)
  den(470) = den(9)*den(468)
  den(471) = den(35)*den(232)
  den(472) = den(238)*den(471)
  den(473) = den(250)*den(471)
  den(474) = den(35)*den(38)
  den(475) = den(283)*den(474)
  den(476) = den(35)*den(261)
  den(477) = den(57)*den(476)
  den(478) = den(54)*den(261)
  den(479) = den(1)*den(478)
  den(480) = den(57)*den(479)
  den(481) = den(8)*den(478)
  den(482) = den(57)*den(481)
  den(483) = den(49)*den(478)
  den(484) = den(52)*den(478)
  den(485) = den(38)*den(280)
  den(486) = den(1)*den(485)
  den(487) = den(283)*den(486)
  den(488) = den(8)*den(485)
  den(489) = den(283)*den(488)
  den(490) = den(49)*den(283)
  den(491) = den(52)*den(283)
  den(492) = den(265)*den(485)
  den(493) = den(268)*den(485)
  den(494) = den(57)*den(265)
  den(495) = den(57)*den(268)
  den(496) = den(334)*den(474)
  den(497) = den(35)*den(294)
  den(498) = den(65)*den(497)
  den(499) = den(66)*den(294)
  den(500) = den(1)*den(499)
  den(501) = den(65)*den(500)
  den(502) = den(8)*den(499)
  den(503) = den(65)*den(502)
  den(504) = den(49)*den(499)
  den(505) = den(52)*den(499)
  den(506) = den(334)*den(486)
  den(507) = den(334)*den(488)
  den(508) = den(49)*den(334)
  den(509) = den(52)*den(334)
  den(510) = den(298)*den(485)
  den(511) = den(301)*den(485)
  den(512) = den(65)*den(298)
  den(513) = den(65)*den(301)
  den(514) = den(12)*den(40)
  den(515) = den(20)*den(514)
  den(516) = den(22)*den(514)
  den(517) = den(20)*den(485)
  den(518) = den(39)*den(517)
  den(519) = den(22)*den(485)
  den(520) = den(39)*den(519)
  den(521) = den(35)*den(514)
  den(522) = den(38)*den(372)
  den(523) = den(35)*den(522)
  den(524) = den(49)*den(372)
  den(525) = den(52)*den(372)
  den(526) = den(377)*den(485)
  den(527) = den(379)*den(485)
  den(528) = den(20)*den(41)
  den(529) = den(65)*den(528)
  den(530) = den(22)*den(41)
  den(531) = den(65)*den(530)
  den(532) = den(57)*den(528)
  den(533) = den(57)*den(530)
  den(534) = den(387)*den(485)
  den(535) = den(389)*den(485)
  den(536) = den(57)*den(343)
  den(537) = den(20)*den(536)
  den(538) = den(22)*den(536)
  den(539) = den(306)*den(485)
  den(540) = den(20)*den(539)
  den(541) = den(22)*den(539)
  den(542) = den(65)*den(306)
  den(543) = den(20)*den(542)
  den(544) = den(22)*den(542)
  den(545) = den(35)*den(41)
  den(546) = den(57)*den(545)
  den(547) = den(65)*den(545)
  den(548) = den(273)*den(422)
  den(549) = den(152)*den(476)
  den(550) = den(152)*den(479)
  den(551) = den(152)*den(481)
  den(552) = den(120)*den(478)
  den(553) = den(123)*den(478)
  den(554) = den(273)*den(430)
  den(555) = den(273)*den(432)
  den(556) = den(120)*den(273)
  den(557) = den(123)*den(273)
  den(558) = den(265)*den(429)
  den(559) = den(268)*den(429)
  den(560) = den(152)*den(265)
  den(561) = den(152)*den(268)
  den(562) = den(6)*den(478)
  den(563) = den(79)*den(562)
  den(564) = den(9)*den(478)
  den(565) = den(79)*den(564)
  den(566) = den(25)*den(270)
  den(567) = den(6)*den(566)
  den(568) = den(9)*den(566)
  den(569) = den(35)*den(566)
  den(570) = den(174)*den(261)
  den(571) = den(35)*den(570)
  den(572) = den(175)*den(478)
  den(573) = den(177)*den(478)
  den(574) = den(174)*den(265)
  den(575) = den(174)*den(268)
  den(576) = den(210)*den(478)
  den(577) = den(212)*den(478)
  den(578) = den(129)*den(478)
  den(579) = den(6)*den(578)
  den(580) = den(9)*den(578)
  den(581) = den(273)*den(459)
  den(582) = den(273)*den(461)
  den(583) = den(129)*den(273)
  den(584) = den(6)*den(583)
  den(585) = den(9)*den(583)
  den(586) = den(283)*den(459)
  den(587) = den(283)*den(461)
  den(588) = den(187)*den(283)
  den(589) = den(6)*den(588)
  den(590) = den(9)*den(588)
  den(591) = den(273)*den(471)
  den(592) = den(283)*den(471)
  den(593) = den(327)*den(400)
  den(594) = den(101)*den(497)
  den(595) = den(101)*den(500)
  den(596) = den(101)*den(502)
  den(597) = den(86)*den(499)
  den(598) = den(89)*den(499)
  den(599) = den(327)*den(412)
  den(600) = den(327)*den(414)
  den(601) = den(86)*den(327)
  den(602) = den(89)*den(327)
  den(603) = den(298)*den(411)
  den(604) = den(301)*den(411)
  den(605) = den(101)*den(298)
  den(606) = den(101)*den(301)
  den(607) = den(12)*den(80)
  den(608) = den(20)*den(607)
  den(609) = den(22)*den(607)
  den(610) = den(20)*den(411)
  den(611) = den(79)*den(610)
  den(612) = den(22)*den(411)
  den(613) = den(79)*den(612)
  den(614) = den(35)*den(607)
  den(615) = den(78)*den(353)
  den(616) = den(35)*den(615)
  den(617) = den(86)*den(353)
  den(618) = den(89)*den(353)
  den(619) = den(356)*den(411)
  den(620) = den(358)*den(411)
  den(621) = den(101)*den(528)
  den(622) = den(101)*den(530)
  den(623) = den(94)*den(528)
  den(624) = den(94)*den(530)
  den(625) = den(381)*den(411)
  den(626) = den(383)*den(411)
  den(627) = den(94)*den(362)
  den(628) = den(20)*den(627)
  den(629) = den(22)*den(627)
  den(630) = den(306)*den(411)
  den(631) = den(20)*den(630)
  den(632) = den(22)*den(630)
  den(633) = den(101)*den(306)
  den(634) = den(20)*den(633)
  den(635) = den(22)*den(633)
  den(636) = den(94)*den(545)
  den(637) = den(101)*den(545)
  den(638) = den(6)*den(499)
  den(639) = den(113)*den(638)
  den(640) = den(9)*den(499)
  den(641) = den(113)*den(640)
  den(642) = den(25)*den(303)
  den(643) = den(6)*den(642)
  den(644) = den(9)*den(642)
  den(645) = den(35)*den(642)
  den(646) = den(138)*den(294)
  den(647) = den(35)*den(646)
  den(648) = den(139)*den(499)
  den(649) = den(141)*den(499)
  den(650) = den(138)*den(298)
  den(651) = den(138)*den(301)
  den(652) = den(12)*den(114)
  den(653) = den(20)*den(652)
  den(654) = den(22)*den(652)
  den(655) = den(20)*den(429)
  den(656) = den(113)*den(655)
  den(657) = den(22)*den(429)
  den(658) = den(113)*den(657)
  den(659) = den(35)*den(652)
  den(660) = den(112)*den(318)
  den(661) = den(35)*den(660)
  den(662) = den(120)*den(318)
  den(663) = den(123)*den(318)
  den(664) = den(321)*den(429)
  den(665) = den(323)*den(429)
  den(666) = den(12)*den(133)
  den(667) = den(20)*den(666)
  den(668) = den(22)*den(666)
  den(669) = den(138)*den(528)
  den(670) = den(138)*den(530)
  den(671) = den(138)*den(306)
  den(672) = den(20)*den(671)
  den(673) = den(22)*den(671)
  den(674) = den(129)*den(318)
  den(675) = den(6)*den(674)
  den(676) = den(9)*den(674)
  den(677) = den(25)*den(133)
  den(678) = den(6)*den(677)
  den(679) = den(9)*den(677)
  den(680) = den(318)*den(459)
  den(681) = den(318)*den(461)
  den(682) = den(133)*den(545)
  den(683) = den(35)*den(677)
  den(684) = den(138)*den(545)
  den(685) = den(318)*den(471)
  den(686) = den(210)*den(499)
  den(687) = den(212)*den(499)
  den(688) = den(167)*den(499)
  den(689) = den(6)*den(688)
  den(690) = den(9)*den(688)
  den(691) = den(327)*den(459)
  den(692) = den(327)*den(461)
  den(693) = den(167)*den(327)
  den(694) = den(6)*den(693)
  den(695) = den(9)*den(693)
  den(696) = den(334)*den(459)
  den(697) = den(334)*den(461)
  den(698) = den(187)*den(334)
  den(699) = den(6)*den(698)
  den(700) = den(9)*den(698)
  den(701) = den(327)*den(471)
  den(702) = den(334)*den(471)
  den(703) = den(152)*den(528)
  den(704) = den(152)*den(530)
  den(705) = den(147)*den(528)
  den(706) = den(147)*den(530)
  den(707) = den(381)*den(429)
  den(708) = den(383)*den(429)
  den(709) = den(147)*den(362)
  den(710) = den(20)*den(709)
  den(711) = den(22)*den(709)
  den(712) = den(343)*den(429)
  den(713) = den(20)*den(712)
  den(714) = den(22)*den(712)
  den(715) = den(152)*den(343)
  den(716) = den(20)*den(715)
  den(717) = den(22)*den(715)
  den(718) = den(147)*den(545)
  den(719) = den(152)*den(545)
  den(720) = den(12)*den(171)
  den(721) = den(20)*den(720)
  den(722) = den(22)*den(720)
  den(723) = den(174)*den(528)
  den(724) = den(174)*den(530)
  den(725) = den(174)*den(343)
  den(726) = den(20)*den(725)
  den(727) = den(22)*den(725)
  den(728) = den(167)*den(353)
  den(729) = den(6)*den(728)
  den(730) = den(9)*den(728)
  den(731) = den(25)*den(171)
  den(732) = den(6)*den(731)
  den(733) = den(9)*den(731)
  den(734) = den(353)*den(459)
  den(735) = den(353)*den(461)
  den(736) = den(171)*den(545)
  den(737) = den(35)*den(731)
  den(738) = den(174)*den(545)
  den(739) = den(353)*den(471)
  den(740) = den(12)*den(191)
  den(741) = den(20)*den(740)
  den(742) = den(22)*den(740)
  den(743) = den(194)*den(528)
  den(744) = den(194)*den(530)
  den(745) = den(194)*den(362)
  den(746) = den(20)*den(745)
  den(747) = den(22)*den(745)
  den(748) = den(25)*den(191)
  den(749) = den(6)*den(748)
  den(750) = den(9)*den(748)
  den(751) = den(187)*den(372)
  den(752) = den(6)*den(751)
  den(753) = den(9)*den(751)
  den(754) = den(372)*den(459)
  den(755) = den(372)*den(461)
  den(756) = den(191)*den(545)
  den(757) = den(35)*den(748)
  den(758) = den(194)*den(545)
  den(759) = den(372)*den(471)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_pphlljj_eexbbxhggg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_pphlljj_eexbbxhggg_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for e- e+ bottom anti-bottom higgs glue glue glue -> 0
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
  use ol_external_pphlljj_eexbbxhggg_1, only: external_perm_pphlljj_eexbbxhggg_1, &
    & external_perm_inv_pphlljj_eexbbxhggg_1, extcomb_perm_pphlljj_eexbbxhggg_1, &
    & average_factor_pphlljj_eexbbxhggg_1
  use ol_external_pphlljj_eexbbxhggg_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_pphlljj_eexbbxhggg_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_pphlljj_eexbbxhggg_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_pphlljj_eexbbxhggg_1
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
  complex(REALKIND) :: MOM_LC(4), M1(6), M1helarray(6,128)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(1), ex6(2), ex7(2), ex8(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf2(2,4), wf4(4,44), wf8(8,154), wf16(16,137), wf32(32,60), wf128(128,510)

  type(polcont) :: A(128,510)
  complex(REALKIND) :: Aj(510)

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
  extmasses2 = [ rZERO2, rZERO2, rMB2, rMB2, rMH2, rZERO2, rZERO2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_pphlljj_eexbbxhggg_1,8)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,8)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_pphlljj_eexbbxhggg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_pphlljj_eexbbxhggg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_pphlljj_eexbbxhggg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_Q(P(:,1), rZERO, H1, ex1)
  call wf_A(P(:,2), rZERO, H2, ex2)
  call wf_Q(P(:,3), rMB, H3, ex3)
  call wf_A(P(:,4), rMB, H4, ex4)
  call wf_S(P(:,5), rMH, H5, ex5)
  call wf_V(P(:,6), rZERO, H6, ex6)
  call wf_V(P(:,7), rZERO, H7, ex7)
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
  call vert_QA_V(ntry, ex1, ex2, wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_QS_A(gH,ntry, ex3, ex5, wf2(:,1), n3(:,2), t3x2(:,:,1))
  call vert_GGG_G(ntry, ex6, ex7, ex8, wf8(:,1), n4(:,1), t4x8(:,:,1))
  call prop_Q_A(ntry, wf2(:,1), Q(:,20), MB, 1_intkind1, wf2(:,2), n2(1))
  call vert_AV_Q(ntry, ex4, wf4(:,1), wf8(:,2), n3(:,3), t3x8(:,:,1))
  call vert_VQ_A(ntry, wf8(:,1), wf2(:,2), wf16(:,1), n3(:,4), t3x16(:,:,1))
  call prop_A_Q(ntry, wf8(:,2), Q(:,11), MB, 1_intkind1, wf8(:,3), n2(2))
  call vert_GGG_G(ntry, ex7, ex8, ex6, wf8(:,4), n4(:,2), t4x8(:,:,2))
  call vert_VQ_A(ntry, wf8(:,4), wf2(:,2), wf16(:,2), n3(:,5), t3x16(:,:,2))
  call vert_GGG_G(ntry, ex8, ex6, ex7, wf8(:,5), n4(:,3), t4x8(:,:,3))
  call vert_VQ_A(ntry, wf8(:,5), wf2(:,2), wf16(:,3), n3(:,6), t3x16(:,:,3))
  call vert_QA_Z(gZl,ntry, ex1, ex2, wf4(:,2), n3(:,7), t3x4(:,:,2))
  call prop_W_W(ntry, wf4(:,2), Q(:,3), MZ, 1_intkind1, wf4(:,3), n2(3))
  call vert_AZ_Q(gZd,ntry, ex4, wf4(:,3), wf8(:,6), n3(:,8), t3x8(:,:,2))
  call prop_A_Q(ntry, wf8(:,6), Q(:,11), MB, 1_intkind1, wf8(:,7), n2(4))
  call vert_AV_Q(ntry, ex4, wf8(:,1), wf16(:,4), n3(:,9), t3x16(:,:,4))
  call vert_VQ_A(ntry, wf4(:,1), wf2(:,2), wf8(:,8), n3(:,10), t3x8(:,:,3))
  call prop_A_Q(ntry, wf16(:,4), Q(:,232), MB, 1_intkind1, wf16(:,5), n2(5))
  call vert_AV_Q(ntry, ex4, wf8(:,4), wf16(:,6), n3(:,11), t3x16(:,:,5))
  call prop_A_Q(ntry, wf16(:,6), Q(:,232), MB, 1_intkind1, wf16(:,7), n2(6))
  call vert_AV_Q(ntry, ex4, wf8(:,5), wf16(:,8), n3(:,12), t3x16(:,:,6))
  call prop_A_Q(ntry, wf16(:,8), Q(:,232), MB, 1_intkind1, wf16(:,9), n2(7))
  call vert_ZQ_A(gZd,ntry, wf4(:,3), wf2(:,2), wf8(:,9), n3(:,13), t3x8(:,:,4))
  call vert_SA_Q(gH,ntry, ex5, ex4, wf2(:,3), n3(:,14), t3x2(:,:,2))
  call prop_A_Q(ntry, wf2(:,3), Q(:,24), MB, 1_intkind1, wf2(:,4), n2(8))
  call vert_VQ_A(ntry, wf4(:,1), ex3, wf8(:,10), n3(:,15), t3x8(:,:,5))
  call vert_AV_Q(ntry, wf2(:,4), wf8(:,1), wf16(:,10), n3(:,16), t3x16(:,:,7))
  call prop_Q_A(ntry, wf8(:,10), Q(:,7), MB, 1_intkind1, wf8(:,11), n2(9))
  call vert_AV_Q(ntry, wf2(:,4), wf8(:,4), wf16(:,11), n3(:,17), t3x16(:,:,8))
  call vert_AV_Q(ntry, wf2(:,4), wf8(:,5), wf16(:,12), n3(:,18), t3x16(:,:,9))
  call vert_ZQ_A(gZd,ntry, wf4(:,3), ex3, wf8(:,12), n3(:,19), t3x8(:,:,6))
  call prop_Q_A(ntry, wf8(:,12), Q(:,7), MB, 1_intkind1, wf8(:,13), n2(10))
  call vert_VQ_A(ntry, wf8(:,1), ex3, wf16(:,13), n3(:,20), t3x16(:,:,10))
  call vert_AV_Q(ntry, wf2(:,4), wf4(:,1), wf8(:,14), n3(:,21), t3x8(:,:,7))
  call prop_Q_A(ntry, wf16(:,13), Q(:,228), MB, 1_intkind1, wf16(:,14), n2(11))
  call vert_VQ_A(ntry, wf8(:,4), ex3, wf16(:,15), n3(:,22), t3x16(:,:,11))
  call prop_Q_A(ntry, wf16(:,15), Q(:,228), MB, 1_intkind1, wf16(:,16), n2(12))
  call vert_VQ_A(ntry, wf8(:,5), ex3, wf16(:,17), n3(:,23), t3x16(:,:,12))
  call prop_Q_A(ntry, wf16(:,17), Q(:,228), MB, 1_intkind1, wf16(:,18), n2(13))
  call vert_AZ_Q(gZd,ntry, wf2(:,4), wf4(:,3), wf8(:,15), n3(:,24), t3x8(:,:,8))
  call vert_QS_A(gH,ntry, wf8(:,11), ex5, wf8(:,16), n3(:,25), t3x8(:,:,9))
  call vert_QS_A(gH,ntry, wf8(:,13), ex5, wf8(:,17), n3(:,26), t3x8(:,:,10))
  call vert_QS_A(gH,ntry, wf16(:,14), ex5, wf16(:,19), n3(:,27), t3x16(:,:,13))
  call vert_QS_A(gH,ntry, wf16(:,16), ex5, wf16(:,20), n3(:,28), t3x16(:,:,14))
  call vert_QS_A(gH,ntry, wf16(:,18), ex5, wf16(:,21), n3(:,29), t3x16(:,:,15))
  call vert_SV_V(ntry, ex5, wf4(:,3), wf4(:,4), n3(:,30), t3x4(:,:,3))
  call prop_W_W(ntry, wf4(:,4), Q(:,19), MZ, 1_intkind1, wf4(:,5), n2(14))
  call vert_QA_Z(gZd,ntry, ex3, wf16(:,5), wf32(:,1), n3(:,31), t3x32(:,:,1))
  call vert_QA_Z(gZd,ntry, ex3, wf16(:,7), wf32(:,2), n3(:,32), t3x32(:,:,2))
  call vert_QA_Z(gZd,ntry, ex3, wf16(:,9), wf32(:,3), n3(:,33), t3x32(:,:,3))
  call vert_QA_Z(gZd,ntry, wf16(:,14), ex4, wf32(:,4), n3(:,34), t3x32(:,:,4))
  call vert_QA_Z(gZd,ntry, wf16(:,16), ex4, wf32(:,5), n3(:,35), t3x32(:,:,5))
  call vert_QA_Z(gZd,ntry, wf16(:,18), ex4, wf32(:,6), n3(:,36), t3x32(:,:,6))
  call vert_AV_Q(ntry, ex4, ex6, wf4(:,6), n3(:,37), t3x4(:,:,4))
  call vert_UV_W(ntry, ex7, Q(:,64), ex8, Q(:,128), wf4(:,7), n3(:,38), t3x4(:,:,5))
  call prop_A_Q(ntry, wf4(:,6), Q(:,40), MB, 1_intkind1, wf4(:,8), n2(15))
  call vert_AV_Q(ntry, wf4(:,8), wf4(:,7), wf16(:,22), n3(:,39), t3x16(:,:,16))
  call prop_Q_A(ntry, wf8(:,8), Q(:,23), MB, 1_intkind1, wf8(:,18), n2(16))
  call prop_Q_A(ntry, wf8(:,9), Q(:,23), MB, 1_intkind1, wf8(:,19), n2(17))
  call vert_AV_Q(ntry, wf4(:,8), wf4(:,1), wf16(:,23), n3(:,40), t3x16(:,:,17))
  call vert_VQ_A(ntry, wf4(:,7), wf2(:,2), wf8(:,20), n3(:,41), t3x8(:,:,11))
  call prop_A_Q(ntry, wf16(:,23), Q(:,43), MB, 1_intkind1, wf16(:,24), n2(18))
  call vert_AZ_Q(gZd,ntry, wf4(:,8), wf4(:,3), wf16(:,25), n3(:,42), t3x16(:,:,18))
  call prop_A_Q(ntry, wf16(:,25), Q(:,43), MB, 1_intkind1, wf16(:,26), n2(19))
  call vert_VQ_A(ntry, ex7, wf2(:,2), wf4(:,9), n3(:,43), t3x4(:,:,6))
  call vert_AV_Q(ntry, wf4(:,8), ex8, wf8(:,21), n3(:,44), t3x8(:,:,12))
  call prop_Q_A(ntry, wf4(:,9), Q(:,84), MB, 1_intkind1, wf4(:,10), n2(20))
  call prop_A_Q(ntry, wf8(:,21), Q(:,168), MB, 1_intkind1, wf8(:,22), n2(21))
  call vert_VQ_A(ntry, wf4(:,1), wf4(:,10), wf16(:,27), n3(:,45), t3x16(:,:,19))
  call vert_ZQ_A(gZd,ntry, wf4(:,3), wf4(:,10), wf16(:,28), n3(:,46), t3x16(:,:,20))
  call vert_VQ_A(ntry, ex8, wf4(:,10), wf8(:,23), n3(:,47), t3x8(:,:,13))
  call vert_AV_Q(ntry, wf4(:,8), ex7, wf8(:,24), n3(:,48), t3x8(:,:,14))
  call vert_VQ_A(ntry, ex8, wf2(:,2), wf4(:,11), n3(:,49), t3x4(:,:,7))
  call prop_A_Q(ntry, wf8(:,24), Q(:,104), MB, 1_intkind1, wf8(:,25), n2(22))
  call prop_Q_A(ntry, wf4(:,11), Q(:,148), MB, 1_intkind1, wf4(:,12), n2(23))
  call vert_AV_Q(ntry, wf8(:,25), wf4(:,1), wf32(:,7), n3(:,50), t3x32(:,:,7))
  call vert_AZ_Q(gZd,ntry, wf8(:,25), wf4(:,3), wf32(:,8), n3(:,51), t3x32(:,:,8))
  call vert_VQ_A(ntry, ex7, wf4(:,12), wf8(:,26), n3(:,52), t3x8(:,:,15))
  call vert_AV_Q(ntry, wf8(:,25), ex8, wf16(:,29), n3(:,53), t3x16(:,:,21))
  call vert_AV_Q(ntry, wf8(:,22), ex7, wf16(:,30), n3(:,54), t3x16(:,:,22))
  call vert_AV_Q(ntry, ex4, ex7, wf4(:,13), n3(:,55), t3x4(:,:,8))
  call vert_UV_W(ntry, ex6, Q(:,32), ex8, Q(:,128), wf4(:,14), n3(:,56), t3x4(:,:,9))
  call prop_A_Q(ntry, wf4(:,13), Q(:,72), MB, 1_intkind1, wf4(:,15), n2(24))
  call vert_AV_Q(ntry, wf4(:,15), wf4(:,14), wf16(:,31), n3(:,57), t3x16(:,:,23))
  call vert_AV_Q(ntry, wf4(:,15), wf4(:,1), wf16(:,32), n3(:,58), t3x16(:,:,24))
  call vert_VQ_A(ntry, wf4(:,14), wf2(:,2), wf8(:,27), n3(:,59), t3x8(:,:,16))
  call prop_A_Q(ntry, wf16(:,32), Q(:,75), MB, 1_intkind1, wf16(:,33), n2(25))
  call vert_AZ_Q(gZd,ntry, wf4(:,15), wf4(:,3), wf16(:,34), n3(:,60), t3x16(:,:,25))
  call prop_A_Q(ntry, wf16(:,34), Q(:,75), MB, 1_intkind1, wf16(:,35), n2(26))
  call vert_VQ_A(ntry, ex6, wf2(:,2), wf4(:,16), n3(:,61), t3x4(:,:,10))
  call vert_AV_Q(ntry, wf4(:,15), ex8, wf8(:,28), n3(:,62), t3x8(:,:,17))
  call prop_Q_A(ntry, wf4(:,16), Q(:,52), MB, 1_intkind1, wf4(:,17), n2(27))
  call prop_A_Q(ntry, wf8(:,28), Q(:,200), MB, 1_intkind1, wf8(:,29), n2(28))
  call vert_VQ_A(ntry, wf4(:,1), wf4(:,17), wf16(:,36), n3(:,63), t3x16(:,:,26))
  call vert_ZQ_A(gZd,ntry, wf4(:,3), wf4(:,17), wf16(:,37), n3(:,64), t3x16(:,:,27))
  call vert_VQ_A(ntry, ex8, wf4(:,17), wf8(:,30), n3(:,65), t3x8(:,:,18))
  call vert_AV_Q(ntry, wf4(:,15), ex6, wf8(:,31), n3(:,66), t3x8(:,:,19))
  call prop_A_Q(ntry, wf8(:,31), Q(:,104), MB, 1_intkind1, wf8(:,32), n2(29))
  call vert_AV_Q(ntry, wf8(:,32), wf4(:,1), wf32(:,9), n3(:,67), t3x32(:,:,9))
  call vert_AZ_Q(gZd,ntry, wf8(:,32), wf4(:,3), wf32(:,10), n3(:,68), t3x32(:,:,10))
  call vert_VQ_A(ntry, ex6, wf4(:,12), wf8(:,33), n3(:,69), t3x8(:,:,20))
  call vert_AV_Q(ntry, wf8(:,32), ex8, wf16(:,38), n3(:,70), t3x16(:,:,28))
  call vert_AV_Q(ntry, wf8(:,29), ex6, wf16(:,39), n3(:,71), t3x16(:,:,29))
  call vert_AV_Q(ntry, ex4, ex8, wf4(:,18), n3(:,72), t3x4(:,:,11))
  call vert_UV_W(ntry, ex6, Q(:,32), ex7, Q(:,64), wf4(:,19), n3(:,73), t3x4(:,:,12))
  call prop_A_Q(ntry, wf4(:,18), Q(:,136), MB, 1_intkind1, wf4(:,20), n2(30))
  call vert_AV_Q(ntry, wf4(:,20), wf4(:,19), wf16(:,40), n3(:,74), t3x16(:,:,30))
  call vert_AV_Q(ntry, wf4(:,20), wf4(:,1), wf16(:,41), n3(:,75), t3x16(:,:,31))
  call vert_VQ_A(ntry, wf4(:,19), wf2(:,2), wf8(:,34), n3(:,76), t3x8(:,:,21))
  call prop_A_Q(ntry, wf16(:,41), Q(:,139), MB, 1_intkind1, wf16(:,42), n2(31))
  call vert_AZ_Q(gZd,ntry, wf4(:,20), wf4(:,3), wf16(:,43), n3(:,77), t3x16(:,:,32))
  call prop_A_Q(ntry, wf16(:,43), Q(:,139), MB, 1_intkind1, wf16(:,44), n2(32))
  call vert_AV_Q(ntry, wf8(:,3), wf4(:,19), wf32(:,11), n3(:,78), t3x32(:,:,11))
  call vert_AV_Q(ntry, wf8(:,7), wf4(:,19), wf32(:,12), n3(:,79), t3x32(:,:,12))
  call prop_Q_A(ntry, wf8(:,34), Q(:,116), MB, 1_intkind1, wf8(:,35), n2(33))
  call vert_AV_Q(ntry, wf8(:,3), ex8, wf16(:,45), n3(:,80), t3x16(:,:,33))
  call vert_AV_Q(ntry, wf8(:,7), ex8, wf16(:,46), n3(:,81), t3x16(:,:,34))
  call vert_UV_W(ntry, wf4(:,19), Q(:,96), ex8, Q(:,128), wf8(:,36), n3(:,82), t3x8(:,:,22))
  call vert_QA_V(ntry, wf2(:,2), wf8(:,3), wf16(:,47), n3(:,83), t3x16(:,:,35))
  call vert_QA_V(ntry, wf2(:,2), wf8(:,7), wf16(:,48), n3(:,84), t3x16(:,:,36))
  call vert_AV_Q(ntry, ex4, wf4(:,19), wf8(:,37), n3(:,85), t3x8(:,:,23))
  call prop_A_Q(ntry, wf8(:,37), Q(:,104), MB, 1_intkind1, wf8(:,38), n2(34))
  call vert_AV_Q(ntry, wf8(:,38), wf4(:,1), wf32(:,13), n3(:,86), t3x32(:,:,13))
  call vert_AZ_Q(gZd,ntry, wf8(:,38), wf4(:,3), wf32(:,14), n3(:,87), t3x32(:,:,14))
  call vert_AV_Q(ntry, wf8(:,38), ex8, wf16(:,49), n3(:,88), t3x16(:,:,37))
  call vert_AV_Q(ntry, ex4, wf8(:,36), wf16(:,50), n3(:,89), t3x16(:,:,38))
  call vert_AV_Q(ntry, wf4(:,20), ex7, wf8(:,39), n3(:,90), t3x8(:,:,24))
  call prop_A_Q(ntry, wf8(:,39), Q(:,200), MB, 1_intkind1, wf8(:,40), n2(35))
  call vert_VQ_A(ntry, ex7, wf4(:,17), wf8(:,41), n3(:,91), t3x8(:,:,25))
  call vert_AV_Q(ntry, wf4(:,20), ex6, wf8(:,42), n3(:,92), t3x8(:,:,26))
  call prop_A_Q(ntry, wf8(:,42), Q(:,168), MB, 1_intkind1, wf8(:,43), n2(36))
  call vert_AV_Q(ntry, wf8(:,43), wf4(:,1), wf32(:,15), n3(:,93), t3x32(:,:,15))
  call vert_AZ_Q(gZd,ntry, wf8(:,43), wf4(:,3), wf32(:,16), n3(:,94), t3x32(:,:,16))
  call vert_VQ_A(ntry, ex6, wf4(:,10), wf8(:,44), n3(:,95), t3x8(:,:,27))
  call vert_AV_Q(ntry, wf8(:,43), ex7, wf16(:,51), n3(:,96), t3x16(:,:,39))
  call vert_AV_Q(ntry, wf8(:,40), ex6, wf16(:,52), n3(:,97), t3x16(:,:,40))
  call vert_AV_Q(ntry, wf8(:,3), wf4(:,14), wf32(:,17), n3(:,98), t3x32(:,:,17))
  call vert_AV_Q(ntry, wf8(:,7), wf4(:,14), wf32(:,18), n3(:,99), t3x32(:,:,18))
  call prop_Q_A(ntry, wf8(:,27), Q(:,180), MB, 1_intkind1, wf8(:,45), n2(37))
  call vert_AV_Q(ntry, wf8(:,3), ex7, wf16(:,53), n3(:,100), t3x16(:,:,41))
  call vert_AV_Q(ntry, wf8(:,7), ex7, wf16(:,54), n3(:,101), t3x16(:,:,42))
  call vert_UV_W(ntry, ex7, Q(:,64), wf4(:,14), Q(:,160), wf8(:,46), n3(:,102), t3x8(:,:,28))
  call vert_AV_Q(ntry, ex4, wf4(:,14), wf8(:,47), n3(:,103), t3x8(:,:,29))
  call prop_A_Q(ntry, wf8(:,47), Q(:,168), MB, 1_intkind1, wf8(:,48), n2(38))
  call vert_AV_Q(ntry, wf8(:,48), wf4(:,1), wf32(:,19), n3(:,104), t3x32(:,:,19))
  call vert_AZ_Q(gZd,ntry, wf8(:,48), wf4(:,3), wf32(:,20), n3(:,105), t3x32(:,:,20))
  call vert_AV_Q(ntry, wf8(:,48), ex7, wf16(:,55), n3(:,106), t3x16(:,:,43))
  call vert_AV_Q(ntry, ex4, wf8(:,46), wf16(:,56), n3(:,107), t3x16(:,:,44))
  call vert_QA_V(ntry, wf4(:,17), wf8(:,3), wf32(:,21), n3(:,108), t3x32(:,:,21))
  call vert_QA_V(ntry, wf4(:,17), wf8(:,7), wf32(:,22), n3(:,109), t3x32(:,:,22))
  call prop_Q_A(ntry, wf8(:,20), Q(:,212), MB, 1_intkind1, wf8(:,49), n2(39))
  call vert_AV_Q(ntry, wf8(:,3), ex6, wf16(:,57), n3(:,110), t3x16(:,:,45))
  call vert_AV_Q(ntry, wf8(:,7), ex6, wf16(:,58), n3(:,111), t3x16(:,:,46))
  call vert_UV_W(ntry, ex6, Q(:,32), wf4(:,7), Q(:,192), wf8(:,50), n3(:,112), t3x8(:,:,30))
  call vert_AV_Q(ntry, ex4, wf4(:,7), wf8(:,51), n3(:,113), t3x8(:,:,31))
  call prop_A_Q(ntry, wf8(:,51), Q(:,200), MB, 1_intkind1, wf8(:,52), n2(40))
  call vert_AV_Q(ntry, wf8(:,52), wf4(:,1), wf32(:,23), n3(:,114), t3x32(:,:,23))
  call vert_AZ_Q(gZd,ntry, wf8(:,52), wf4(:,3), wf32(:,24), n3(:,115), t3x32(:,:,24))
  call vert_AV_Q(ntry, wf8(:,52), ex6, wf16(:,59), n3(:,116), t3x16(:,:,47))
  call vert_AV_Q(ntry, ex4, wf8(:,50), wf16(:,60), n3(:,117), t3x16(:,:,48))
  call prop_A_Q(ntry, wf16(:,53), Q(:,75), MB, 1_intkind1, wf16(:,61), n2(41))
  call prop_A_Q(ntry, wf16(:,54), Q(:,75), MB, 1_intkind1, wf16(:,62), n2(42))
  call prop_Q_A(ntry, wf8(:,41), Q(:,116), MB, 1_intkind1, wf8(:,53), n2(43))
  call prop_A_Q(ntry, wf16(:,57), Q(:,43), MB, 1_intkind1, wf16(:,63), n2(44))
  call prop_A_Q(ntry, wf16(:,58), Q(:,43), MB, 1_intkind1, wf16(:,64), n2(45))
  call prop_Q_A(ntry, wf8(:,44), Q(:,116), MB, 1_intkind1, wf8(:,54), n2(46))
  call prop_Q_A(ntry, wf8(:,33), Q(:,180), MB, 1_intkind1, wf8(:,55), n2(47))
  call vert_VQ_A(ntry, ex6, ex3, wf4(:,21), n3(:,118), t3x4(:,:,13))
  call prop_Q_A(ntry, wf4(:,21), Q(:,36), MB, 1_intkind1, wf4(:,22), n2(48))
  call vert_VQ_A(ntry, wf4(:,1), wf4(:,22), wf16(:,65), n3(:,119), t3x16(:,:,49))
  call vert_AV_Q(ntry, wf2(:,4), wf4(:,7), wf8(:,56), n3(:,120), t3x8(:,:,32))
  call prop_Q_A(ntry, wf16(:,65), Q(:,39), MB, 1_intkind1, wf16(:,66), n2(49))
  call vert_ZQ_A(gZd,ntry, wf4(:,3), wf4(:,22), wf16(:,67), n3(:,121), t3x16(:,:,50))
  call prop_Q_A(ntry, wf16(:,67), Q(:,39), MB, 1_intkind1, wf16(:,68), n2(50))
  call vert_VQ_A(ntry, wf4(:,7), wf4(:,22), wf16(:,69), n3(:,122), t3x16(:,:,51))
  call prop_A_Q(ntry, wf8(:,14), Q(:,27), MB, 1_intkind1, wf8(:,57), n2(51))
  call prop_A_Q(ntry, wf8(:,15), Q(:,27), MB, 1_intkind1, wf8(:,58), n2(52))
  call vert_VQ_A(ntry, ex7, wf4(:,22), wf8(:,59), n3(:,123), t3x8(:,:,33))
  call vert_AV_Q(ntry, wf2(:,4), ex8, wf4(:,23), n3(:,124), t3x4(:,:,14))
  call prop_Q_A(ntry, wf8(:,59), Q(:,100), MB, 1_intkind1, wf8(:,60), n2(53))
  call prop_A_Q(ntry, wf4(:,23), Q(:,152), MB, 1_intkind1, wf4(:,24), n2(54))
  call vert_VQ_A(ntry, wf4(:,1), wf8(:,60), wf32(:,25), n3(:,125), t3x32(:,:,25))
  call vert_ZQ_A(gZd,ntry, wf4(:,3), wf8(:,60), wf32(:,26), n3(:,126), t3x32(:,:,26))
  call vert_VQ_A(ntry, ex8, wf8(:,60), wf16(:,70), n3(:,127), t3x16(:,:,52))
  call vert_AV_Q(ntry, wf2(:,4), ex7, wf4(:,25), n3(:,128), t3x4(:,:,15))
  call vert_VQ_A(ntry, ex8, wf4(:,22), wf8(:,61), n3(:,129), t3x8(:,:,34))
  call prop_A_Q(ntry, wf4(:,25), Q(:,88), MB, 1_intkind1, wf4(:,26), n2(55))
  call prop_Q_A(ntry, wf8(:,61), Q(:,164), MB, 1_intkind1, wf8(:,62), n2(56))
  call vert_AV_Q(ntry, wf4(:,26), wf4(:,1), wf16(:,71), n3(:,130), t3x16(:,:,53))
  call vert_AZ_Q(gZd,ntry, wf4(:,26), wf4(:,3), wf16(:,72), n3(:,131), t3x16(:,:,54))
  call vert_VQ_A(ntry, ex7, wf8(:,62), wf16(:,73), n3(:,132), t3x16(:,:,55))
  call vert_AV_Q(ntry, wf4(:,26), ex8, wf8(:,63), n3(:,133), t3x8(:,:,35))
  call vert_AV_Q(ntry, wf4(:,24), ex7, wf8(:,64), n3(:,134), t3x8(:,:,36))
  call vert_VQ_A(ntry, ex7, ex3, wf4(:,27), n3(:,135), t3x4(:,:,16))
  call prop_Q_A(ntry, wf4(:,27), Q(:,68), MB, 1_intkind1, wf4(:,28), n2(57))
  call vert_VQ_A(ntry, wf4(:,1), wf4(:,28), wf16(:,74), n3(:,136), t3x16(:,:,56))
  call vert_AV_Q(ntry, wf2(:,4), wf4(:,14), wf8(:,65), n3(:,137), t3x8(:,:,37))
  call prop_Q_A(ntry, wf16(:,74), Q(:,71), MB, 1_intkind1, wf16(:,75), n2(58))
  call vert_ZQ_A(gZd,ntry, wf4(:,3), wf4(:,28), wf16(:,76), n3(:,138), t3x16(:,:,57))
  call prop_Q_A(ntry, wf16(:,76), Q(:,71), MB, 1_intkind1, wf16(:,77), n2(59))
  call vert_VQ_A(ntry, wf4(:,14), wf4(:,28), wf16(:,78), n3(:,139), t3x16(:,:,58))
  call vert_VQ_A(ntry, ex6, wf4(:,28), wf8(:,66), n3(:,140), t3x8(:,:,38))
  call prop_Q_A(ntry, wf8(:,66), Q(:,100), MB, 1_intkind1, wf8(:,67), n2(60))
  call vert_VQ_A(ntry, wf4(:,1), wf8(:,67), wf32(:,27), n3(:,141), t3x32(:,:,27))
  call vert_ZQ_A(gZd,ntry, wf4(:,3), wf8(:,67), wf32(:,28), n3(:,142), t3x32(:,:,28))
  call vert_VQ_A(ntry, ex8, wf8(:,67), wf16(:,79), n3(:,143), t3x16(:,:,59))
  call vert_AV_Q(ntry, wf2(:,4), ex6, wf4(:,29), n3(:,144), t3x4(:,:,17))
  call vert_VQ_A(ntry, ex8, wf4(:,28), wf8(:,68), n3(:,145), t3x8(:,:,39))
  call prop_A_Q(ntry, wf4(:,29), Q(:,56), MB, 1_intkind1, wf4(:,30), n2(61))
  call prop_Q_A(ntry, wf8(:,68), Q(:,196), MB, 1_intkind1, wf8(:,69), n2(62))
  call vert_AV_Q(ntry, wf4(:,30), wf4(:,1), wf16(:,80), n3(:,146), t3x16(:,:,60))
  call vert_AZ_Q(gZd,ntry, wf4(:,30), wf4(:,3), wf16(:,81), n3(:,147), t3x16(:,:,61))
  call vert_VQ_A(ntry, ex6, wf8(:,69), wf16(:,82), n3(:,148), t3x16(:,:,62))
  call vert_AV_Q(ntry, wf4(:,30), ex8, wf8(:,70), n3(:,149), t3x8(:,:,40))
  call vert_AV_Q(ntry, wf4(:,24), ex6, wf8(:,71), n3(:,150), t3x8(:,:,41))
  call vert_VQ_A(ntry, ex8, ex3, wf4(:,31), n3(:,151), t3x4(:,:,18))
  call prop_Q_A(ntry, wf4(:,31), Q(:,132), MB, 1_intkind1, wf4(:,32), n2(63))
  call vert_VQ_A(ntry, wf4(:,1), wf4(:,32), wf16(:,83), n3(:,152), t3x16(:,:,63))
  call vert_AV_Q(ntry, wf2(:,4), wf4(:,19), wf8(:,72), n3(:,153), t3x8(:,:,42))
  call prop_Q_A(ntry, wf16(:,83), Q(:,135), MB, 1_intkind1, wf16(:,84), n2(64))
  call vert_ZQ_A(gZd,ntry, wf4(:,3), wf4(:,32), wf16(:,85), n3(:,154), t3x16(:,:,64))
  call prop_Q_A(ntry, wf16(:,85), Q(:,135), MB, 1_intkind1, wf16(:,86), n2(65))
  call vert_VQ_A(ntry, wf4(:,19), wf4(:,32), wf16(:,87), n3(:,155), t3x16(:,:,65))
  call prop_A_Q(ntry, wf8(:,72), Q(:,120), MB, 1_intkind1, wf8(:,73), n2(66))
  call vert_VQ_A(ntry, ex8, wf8(:,11), wf16(:,88), n3(:,156), t3x16(:,:,66))
  call vert_VQ_A(ntry, ex8, wf8(:,13), wf16(:,89), n3(:,157), t3x16(:,:,67))
  call vert_VQ_A(ntry, wf4(:,19), wf8(:,11), wf32(:,29), n3(:,158), t3x32(:,:,29))
  call vert_VQ_A(ntry, wf4(:,19), wf8(:,13), wf32(:,30), n3(:,159), t3x32(:,:,30))
  call vert_QA_V(ntry, wf8(:,11), wf2(:,4), wf16(:,90), n3(:,160), t3x16(:,:,68))
  call vert_QA_V(ntry, wf8(:,13), wf2(:,4), wf16(:,91), n3(:,161), t3x16(:,:,69))
  call vert_VQ_A(ntry, wf4(:,19), ex3, wf8(:,74), n3(:,162), t3x8(:,:,43))
  call prop_Q_A(ntry, wf8(:,74), Q(:,100), MB, 1_intkind1, wf8(:,75), n2(67))
  call vert_VQ_A(ntry, ex8, wf8(:,75), wf16(:,92), n3(:,163), t3x16(:,:,70))
  call vert_VQ_A(ntry, wf4(:,1), wf8(:,75), wf32(:,31), n3(:,164), t3x32(:,:,31))
  call vert_ZQ_A(gZd,ntry, wf4(:,3), wf8(:,75), wf32(:,32), n3(:,165), t3x32(:,:,32))
  call vert_VQ_A(ntry, wf8(:,36), ex3, wf16(:,93), n3(:,166), t3x16(:,:,71))
  call vert_VQ_A(ntry, ex6, wf4(:,32), wf8(:,76), n3(:,167), t3x8(:,:,44))
  call prop_Q_A(ntry, wf8(:,76), Q(:,164), MB, 1_intkind1, wf8(:,77), n2(68))
  call vert_VQ_A(ntry, wf4(:,1), wf8(:,77), wf32(:,33), n3(:,168), t3x32(:,:,33))
  call vert_ZQ_A(gZd,ntry, wf4(:,3), wf8(:,77), wf32(:,34), n3(:,169), t3x32(:,:,34))
  call vert_VQ_A(ntry, ex7, wf8(:,77), wf16(:,94), n3(:,170), t3x16(:,:,72))
  call vert_VQ_A(ntry, ex7, wf4(:,32), wf8(:,78), n3(:,171), t3x8(:,:,45))
  call prop_Q_A(ntry, wf8(:,78), Q(:,196), MB, 1_intkind1, wf8(:,79), n2(69))
  call vert_VQ_A(ntry, ex6, wf8(:,79), wf16(:,95), n3(:,172), t3x16(:,:,73))
  call vert_AV_Q(ntry, wf4(:,30), ex7, wf8(:,80), n3(:,173), t3x8(:,:,46))
  call vert_AV_Q(ntry, wf4(:,26), ex6, wf8(:,81), n3(:,174), t3x8(:,:,47))
  call prop_A_Q(ntry, wf8(:,65), Q(:,184), MB, 1_intkind1, wf8(:,82), n2(70))
  call vert_VQ_A(ntry, ex7, wf8(:,11), wf16(:,96), n3(:,175), t3x16(:,:,74))
  call vert_VQ_A(ntry, ex7, wf8(:,13), wf16(:,97), n3(:,176), t3x16(:,:,75))
  call vert_VQ_A(ntry, wf4(:,14), wf8(:,11), wf32(:,35), n3(:,177), t3x32(:,:,35))
  call vert_VQ_A(ntry, wf4(:,14), wf8(:,13), wf32(:,36), n3(:,178), t3x32(:,:,36))
  call vert_VQ_A(ntry, wf4(:,14), ex3, wf8(:,83), n3(:,179), t3x8(:,:,48))
  call prop_Q_A(ntry, wf8(:,83), Q(:,164), MB, 1_intkind1, wf8(:,84), n2(71))
  call vert_VQ_A(ntry, ex7, wf8(:,84), wf16(:,98), n3(:,180), t3x16(:,:,76))
  call vert_VQ_A(ntry, wf4(:,1), wf8(:,84), wf32(:,37), n3(:,181), t3x32(:,:,37))
  call vert_ZQ_A(gZd,ntry, wf4(:,3), wf8(:,84), wf32(:,38), n3(:,182), t3x32(:,:,38))
  call vert_VQ_A(ntry, wf8(:,46), ex3, wf16(:,99), n3(:,183), t3x16(:,:,77))
  call prop_A_Q(ntry, wf8(:,56), Q(:,216), MB, 1_intkind1, wf8(:,85), n2(72))
  call vert_VQ_A(ntry, ex6, wf8(:,11), wf16(:,100), n3(:,184), t3x16(:,:,78))
  call vert_VQ_A(ntry, ex6, wf8(:,13), wf16(:,101), n3(:,185), t3x16(:,:,79))
  call vert_QA_V(ntry, wf8(:,11), wf4(:,30), wf32(:,39), n3(:,186), t3x32(:,:,39))
  call vert_QA_V(ntry, wf8(:,13), wf4(:,30), wf32(:,40), n3(:,187), t3x32(:,:,40))
  call vert_VQ_A(ntry, wf4(:,7), ex3, wf8(:,86), n3(:,188), t3x8(:,:,49))
  call prop_Q_A(ntry, wf8(:,86), Q(:,196), MB, 1_intkind1, wf8(:,87), n2(73))
  call vert_VQ_A(ntry, ex6, wf8(:,87), wf16(:,102), n3(:,189), t3x16(:,:,80))
  call vert_VQ_A(ntry, wf8(:,50), ex3, wf16(:,103), n3(:,190), t3x16(:,:,81))
  call vert_VQ_A(ntry, wf4(:,1), wf8(:,87), wf32(:,41), n3(:,191), t3x32(:,:,41))
  call vert_ZQ_A(gZd,ntry, wf4(:,3), wf8(:,87), wf32(:,42), n3(:,192), t3x32(:,:,42))
  call prop_Q_A(ntry, wf16(:,100), Q(:,39), MB, 1_intkind1, wf16(:,104), n2(74))
  call prop_Q_A(ntry, wf16(:,101), Q(:,39), MB, 1_intkind1, wf16(:,105), n2(75))
  call prop_Q_A(ntry, wf16(:,96), Q(:,71), MB, 1_intkind1, wf16(:,106), n2(76))
  call prop_Q_A(ntry, wf16(:,97), Q(:,71), MB, 1_intkind1, wf16(:,107), n2(77))
  call prop_A_Q(ntry, wf8(:,71), Q(:,184), MB, 1_intkind1, wf8(:,88), n2(78))
  call prop_A_Q(ntry, wf8(:,80), Q(:,120), MB, 1_intkind1, wf8(:,89), n2(79))
  call prop_A_Q(ntry, wf8(:,81), Q(:,120), MB, 1_intkind1, wf8(:,90), n2(80))
  call vert_AZ_Q(gZd,ntry, wf4(:,15), wf4(:,5), wf16(:,108), n3(:,193), t3x16(:,:,82))
  call vert_ZQ_A(gZd,ntry, wf4(:,5), wf4(:,22), wf16(:,109), n3(:,194), t3x16(:,:,83))
  call vert_QS_A(gH,ntry, wf4(:,22), ex5, wf4(:,33), n3(:,195), t3x4(:,:,19))
  call prop_Q_A(ntry, wf4(:,33), Q(:,52), MB, 1_intkind1, wf4(:,34), n2(81))
  call vert_VQ_A(ntry, wf4(:,1), wf4(:,34), wf16(:,110), n3(:,196), t3x16(:,:,84))
  call vert_ZQ_A(gZd,ntry, wf4(:,3), wf4(:,34), wf16(:,111), n3(:,197), t3x16(:,:,85))
  call vert_VQ_A(ntry, ex8, wf4(:,34), wf8(:,91), n3(:,198), t3x8(:,:,50))
  call vert_SA_Q(gH,ntry, ex5, wf4(:,15), wf4(:,35), n3(:,199), t3x4(:,:,20))
  call prop_A_Q(ntry, wf4(:,35), Q(:,88), MB, 1_intkind1, wf4(:,36), n2(82))
  call vert_AV_Q(ntry, wf4(:,36), wf4(:,1), wf16(:,112), n3(:,200), t3x16(:,:,86))
  call vert_AZ_Q(gZd,ntry, wf4(:,36), wf4(:,3), wf16(:,113), n3(:,201), t3x16(:,:,87))
  call vert_QS_A(gH,ntry, wf8(:,62), ex5, wf8(:,92), n3(:,202), t3x8(:,:,51))
  call vert_AV_Q(ntry, wf4(:,36), ex8, wf8(:,93), n3(:,203), t3x8(:,:,52))
  call vert_SA_Q(gH,ntry, ex5, wf8(:,29), wf8(:,94), n3(:,204), t3x8(:,:,53))
  call vert_AZ_Q(gZd,ntry, wf4(:,20), wf4(:,5), wf16(:,114), n3(:,205), t3x16(:,:,88))
  call vert_VQ_A(ntry, ex7, wf4(:,34), wf8(:,95), n3(:,206), t3x8(:,:,54))
  call vert_SA_Q(gH,ntry, ex5, wf4(:,20), wf4(:,37), n3(:,207), t3x4(:,:,21))
  call prop_A_Q(ntry, wf4(:,37), Q(:,152), MB, 1_intkind1, wf4(:,38), n2(83))
  call vert_AV_Q(ntry, wf4(:,38), wf4(:,1), wf16(:,115), n3(:,208), t3x16(:,:,89))
  call vert_AZ_Q(gZd,ntry, wf4(:,38), wf4(:,3), wf16(:,116), n3(:,209), t3x16(:,:,90))
  call vert_QS_A(gH,ntry, wf8(:,60), ex5, wf8(:,96), n3(:,210), t3x8(:,:,55))
  call vert_AV_Q(ntry, wf4(:,38), ex7, wf8(:,97), n3(:,211), t3x8(:,:,56))
  call vert_SA_Q(gH,ntry, ex5, wf8(:,40), wf8(:,98), n3(:,212), t3x8(:,:,57))
  call vert_QA_V(ntry, wf4(:,34), wf8(:,3), wf32(:,43), n3(:,213), t3x32(:,:,43))
  call vert_QA_V(ntry, wf4(:,34), wf8(:,7), wf32(:,44), n3(:,214), t3x32(:,:,44))
  call prop_Q_A(ntry, wf16(:,69), Q(:,228), MB, 1_intkind1, wf16(:,117), n2(84))
  call vert_SA_Q(gH,ntry, ex5, wf8(:,3), wf8(:,99), n3(:,215), t3x8(:,:,58))
  call vert_SA_Q(gH,ntry, ex5, wf8(:,7), wf8(:,100), n3(:,216), t3x8(:,:,59))
  call vert_AZ_Q(gZd,ntry, ex4, wf4(:,5), wf8(:,101), n3(:,217), t3x8(:,:,60))
  call vert_QA_Z(gZd,ntry, wf4(:,22), wf8(:,52), wf32(:,45), n3(:,218), t3x32(:,:,45))
  call vert_SA_Q(gH,ntry, ex5, wf8(:,52), wf8(:,102), n3(:,219), t3x8(:,:,61))
  call prop_Q_A(ntry, wf8(:,95), Q(:,116), MB, 1_intkind1, wf8(:,103), n2(85))
  call prop_A_Q(ntry, wf8(:,99), Q(:,27), MB, 1_intkind1, wf8(:,104), n2(86))
  call prop_A_Q(ntry, wf8(:,100), Q(:,27), MB, 1_intkind1, wf8(:,105), n2(87))
  call prop_Q_A(ntry, wf8(:,96), Q(:,116), MB, 1_intkind1, wf8(:,106), n2(88))
  call prop_Q_A(ntry, wf8(:,92), Q(:,180), MB, 1_intkind1, wf8(:,107), n2(89))
  call prop_A_Q(ntry, wf8(:,101), Q(:,27), MB, 1_intkind1, wf8(:,108), n2(90))
  call vert_AZ_Q(gZd,ntry, wf4(:,8), wf4(:,5), wf16(:,118), n3(:,220), t3x16(:,:,91))
  call vert_ZQ_A(gZd,ntry, wf4(:,5), wf4(:,28), wf16(:,119), n3(:,221), t3x16(:,:,92))
  call vert_QS_A(gH,ntry, wf4(:,28), ex5, wf4(:,39), n3(:,222), t3x4(:,:,22))
  call prop_Q_A(ntry, wf4(:,39), Q(:,84), MB, 1_intkind1, wf4(:,40), n2(91))
  call vert_VQ_A(ntry, wf4(:,1), wf4(:,40), wf16(:,120), n3(:,223), t3x16(:,:,93))
  call vert_ZQ_A(gZd,ntry, wf4(:,3), wf4(:,40), wf16(:,121), n3(:,224), t3x16(:,:,94))
  call vert_VQ_A(ntry, ex8, wf4(:,40), wf8(:,109), n3(:,225), t3x8(:,:,62))
  call vert_SA_Q(gH,ntry, ex5, wf4(:,8), wf4(:,41), n3(:,226), t3x4(:,:,23))
  call prop_A_Q(ntry, wf4(:,41), Q(:,56), MB, 1_intkind1, wf4(:,42), n2(92))
  call vert_AV_Q(ntry, wf4(:,42), wf4(:,1), wf16(:,122), n3(:,227), t3x16(:,:,95))
  call vert_AZ_Q(gZd,ntry, wf4(:,42), wf4(:,3), wf16(:,123), n3(:,228), t3x16(:,:,96))
  call vert_QS_A(gH,ntry, wf8(:,69), ex5, wf8(:,110), n3(:,229), t3x8(:,:,63))
  call vert_AV_Q(ntry, wf4(:,42), ex8, wf8(:,111), n3(:,230), t3x8(:,:,64))
  call vert_SA_Q(gH,ntry, ex5, wf8(:,22), wf8(:,112), n3(:,231), t3x8(:,:,65))
  call vert_ZQ_A(gZd,ntry, wf4(:,5), wf4(:,32), wf16(:,124), n3(:,232), t3x16(:,:,97))
  call vert_QS_A(gH,ntry, wf4(:,32), ex5, wf4(:,43), n3(:,233), t3x4(:,:,24))
  call prop_Q_A(ntry, wf4(:,43), Q(:,148), MB, 1_intkind1, wf4(:,44), n2(93))
  call vert_VQ_A(ntry, wf4(:,1), wf4(:,44), wf16(:,125), n3(:,234), t3x16(:,:,98))
  call vert_ZQ_A(gZd,ntry, wf4(:,3), wf4(:,44), wf16(:,126), n3(:,235), t3x16(:,:,99))
  call vert_VQ_A(ntry, ex7, wf4(:,44), wf8(:,113), n3(:,236), t3x8(:,:,66))
  call vert_QS_A(gH,ntry, wf8(:,79), ex5, wf8(:,114), n3(:,237), t3x8(:,:,67))
  call vert_AV_Q(ntry, wf4(:,42), ex7, wf8(:,115), n3(:,238), t3x8(:,:,68))
  call vert_SA_Q(gH,ntry, ex5, wf8(:,25), wf8(:,116), n3(:,239), t3x8(:,:,69))
  call prop_A_Q(ntry, wf16(:,22), Q(:,232), MB, 1_intkind1, wf16(:,127), n2(94))
  call vert_QA_V(ntry, wf8(:,11), wf4(:,42), wf32(:,46), n3(:,240), t3x32(:,:,46))
  call vert_QA_V(ntry, wf8(:,13), wf4(:,42), wf32(:,47), n3(:,241), t3x32(:,:,47))
  call vert_ZQ_A(gZd,ntry, wf4(:,5), ex3, wf8(:,117), n3(:,242), t3x8(:,:,70))
  call vert_QA_Z(gZd,ntry, wf8(:,87), wf4(:,8), wf32(:,48), n3(:,243), t3x32(:,:,48))
  call vert_QS_A(gH,ntry, wf8(:,87), ex5, wf8(:,118), n3(:,244), t3x8(:,:,71))
  call prop_Q_A(ntry, wf8(:,16), Q(:,23), MB, 1_intkind1, wf8(:,119), n2(95))
  call prop_Q_A(ntry, wf8(:,17), Q(:,23), MB, 1_intkind1, wf8(:,120), n2(96))
  call prop_A_Q(ntry, wf8(:,112), Q(:,184), MB, 1_intkind1, wf8(:,121), n2(97))
  call prop_A_Q(ntry, wf8(:,115), Q(:,120), MB, 1_intkind1, wf8(:,122), n2(98))
  call prop_A_Q(ntry, wf8(:,116), Q(:,120), MB, 1_intkind1, wf8(:,123), n2(99))
  call prop_Q_A(ntry, wf8(:,117), Q(:,23), MB, 1_intkind1, wf8(:,124), n2(100))
  call vert_VQ_A(ntry, ex6, wf4(:,40), wf8(:,125), n3(:,245), t3x8(:,:,72))
  call vert_QS_A(gH,ntry, wf8(:,67), ex5, wf8(:,126), n3(:,246), t3x8(:,:,73))
  call vert_AV_Q(ntry, wf4(:,38), ex6, wf8(:,127), n3(:,247), t3x8(:,:,74))
  call vert_SA_Q(gH,ntry, ex5, wf8(:,43), wf8(:,128), n3(:,248), t3x8(:,:,75))
  call vert_QA_V(ntry, wf4(:,40), wf8(:,3), wf32(:,49), n3(:,249), t3x32(:,:,49))
  call vert_QA_V(ntry, wf4(:,40), wf8(:,7), wf32(:,50), n3(:,250), t3x32(:,:,50))
  call prop_Q_A(ntry, wf16(:,78), Q(:,228), MB, 1_intkind1, wf16(:,128), n2(101))
  call vert_QA_Z(gZd,ntry, wf4(:,28), wf8(:,48), wf32(:,51), n3(:,251), t3x32(:,:,51))
  call vert_SA_Q(gH,ntry, ex5, wf8(:,48), wf8(:,129), n3(:,252), t3x8(:,:,76))
  call prop_Q_A(ntry, wf8(:,125), Q(:,116), MB, 1_intkind1, wf8(:,130), n2(102))
  call prop_Q_A(ntry, wf8(:,126), Q(:,116), MB, 1_intkind1, wf8(:,131), n2(103))
  call prop_Q_A(ntry, wf8(:,110), Q(:,212), MB, 1_intkind1, wf8(:,132), n2(104))
  call vert_VQ_A(ntry, ex6, wf4(:,44), wf8(:,133), n3(:,253), t3x8(:,:,77))
  call vert_QS_A(gH,ntry, wf8(:,77), ex5, wf8(:,134), n3(:,254), t3x8(:,:,78))
  call vert_AV_Q(ntry, wf4(:,36), ex6, wf8(:,135), n3(:,255), t3x8(:,:,79))
  call vert_SA_Q(gH,ntry, ex5, wf8(:,32), wf8(:,136), n3(:,256), t3x8(:,:,80))
  call prop_A_Q(ntry, wf16(:,31), Q(:,232), MB, 1_intkind1, wf16(:,129), n2(105))
  call vert_QA_V(ntry, wf8(:,11), wf4(:,36), wf32(:,52), n3(:,257), t3x32(:,:,52))
  call vert_QA_V(ntry, wf8(:,13), wf4(:,36), wf32(:,53), n3(:,258), t3x32(:,:,53))
  call vert_QA_Z(gZd,ntry, wf8(:,84), wf4(:,15), wf32(:,54), n3(:,259), t3x32(:,:,54))
  call vert_QS_A(gH,ntry, wf8(:,84), ex5, wf8(:,137), n3(:,260), t3x8(:,:,81))
  call prop_A_Q(ntry, wf8(:,94), Q(:,216), MB, 1_intkind1, wf8(:,138), n2(106))
  call prop_A_Q(ntry, wf8(:,135), Q(:,120), MB, 1_intkind1, wf8(:,139), n2(107))
  call prop_A_Q(ntry, wf8(:,136), Q(:,120), MB, 1_intkind1, wf8(:,140), n2(108))
  call vert_QA_V(ntry, wf4(:,44), wf8(:,3), wf32(:,55), n3(:,261), t3x32(:,:,55))
  call vert_QA_V(ntry, wf4(:,44), wf8(:,7), wf32(:,56), n3(:,262), t3x32(:,:,56))
  call prop_Q_A(ntry, wf16(:,87), Q(:,228), MB, 1_intkind1, wf16(:,130), n2(109))
  call vert_QA_Z(gZd,ntry, wf4(:,32), wf8(:,38), wf32(:,57), n3(:,263), t3x32(:,:,57))
  call vert_SA_Q(gH,ntry, ex5, wf8(:,38), wf8(:,141), n3(:,264), t3x8(:,:,82))
  call prop_A_Q(ntry, wf16(:,40), Q(:,232), MB, 1_intkind1, wf16(:,131), n2(110))
  call vert_QA_V(ntry, wf8(:,11), wf4(:,38), wf32(:,58), n3(:,265), t3x32(:,:,58))
  call vert_QA_V(ntry, wf8(:,13), wf4(:,38), wf32(:,59), n3(:,266), t3x32(:,:,59))
  call vert_QA_Z(gZd,ntry, wf8(:,75), wf4(:,20), wf32(:,60), n3(:,267), t3x32(:,:,60))
  call vert_QS_A(gH,ntry, wf8(:,75), ex5, wf8(:,142), n3(:,268), t3x8(:,:,83))
  call prop_A_Q(ntry, wf16(:,50), Q(:,232), MB, 1_intkind1, wf16(:,132), n2(111))
  call prop_A_Q(ntry, wf8(:,141), Q(:,120), MB, 1_intkind1, wf8(:,143), n2(112))
  call prop_Q_A(ntry, wf8(:,142), Q(:,116), MB, 1_intkind1, wf8(:,144), n2(113))
  call prop_Q_A(ntry, wf16(:,93), Q(:,228), MB, 1_intkind1, wf16(:,133), n2(114))
  call prop_Q_A(ntry, wf8(:,133), Q(:,180), MB, 1_intkind1, wf8(:,145), n2(115))
  call prop_Q_A(ntry, wf8(:,134), Q(:,180), MB, 1_intkind1, wf8(:,146), n2(116))
  call prop_Q_A(ntry, wf8(:,114), Q(:,212), MB, 1_intkind1, wf8(:,147), n2(117))
  call prop_A_Q(ntry, wf8(:,98), Q(:,216), MB, 1_intkind1, wf8(:,148), n2(118))
  call prop_A_Q(ntry, wf8(:,127), Q(:,184), MB, 1_intkind1, wf8(:,149), n2(119))
  call prop_A_Q(ntry, wf8(:,128), Q(:,184), MB, 1_intkind1, wf8(:,150), n2(120))
  call prop_A_Q(ntry, wf16(:,56), Q(:,232), MB, 1_intkind1, wf16(:,134), n2(121))
  call prop_A_Q(ntry, wf8(:,129), Q(:,184), MB, 1_intkind1, wf8(:,151), n2(122))
  call prop_Q_A(ntry, wf8(:,137), Q(:,180), MB, 1_intkind1, wf8(:,152), n2(123))
  call prop_Q_A(ntry, wf16(:,99), Q(:,228), MB, 1_intkind1, wf16(:,135), n2(124))
  call prop_A_Q(ntry, wf16(:,60), Q(:,232), MB, 1_intkind1, wf16(:,136), n2(125))
  call prop_A_Q(ntry, wf8(:,102), Q(:,216), MB, 1_intkind1, wf8(:,153), n2(126))
  call prop_Q_A(ntry, wf16(:,103), Q(:,228), MB, 1_intkind1, wf16(:,137), n2(127))
  call prop_Q_A(ntry, wf8(:,118), Q(:,212), MB, 1_intkind1, wf8(:,154), n2(128))


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
  M2add = M2 / average_factor_pphlljj_eexbbxhggg_1

  do k = 0, 38-1
    M2(k) = M2add(extcomb_perm_pphlljj_eexbbxhggg_1(k))
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

    call cont_QA(nsync, wf16(:,1), wf8(:,3), A(:,1), n3(:,269), t3x128(:,:,1), nhel, den(7))
    call cont_QA(nsync, wf8(:,3), wf16(:,2), A(:,2), n3(:,270), t3x128(:,:,2), nhel, den(7))
    call cont_QA(nsync, wf8(:,3), wf16(:,3), A(:,3), n3(:,271), t3x128(:,:,3), nhel, den(7))
    call cont_QA(nsync, wf16(:,1), wf8(:,7), A(:,4), n3(:,272), t3x128(:,:,4), nhel, den(10))
    call cont_QA(nsync, wf16(:,2), wf8(:,7), A(:,5), n3(:,273), t3x128(:,:,5), nhel, den(10))
    call cont_QA(nsync, wf16(:,3), wf8(:,7), A(:,6), n3(:,274), t3x128(:,:,6), nhel, den(10))
    call cont_QA(nsync, wf8(:,8), wf16(:,5), A(:,7), n3(:,275), t3x128(:,:,7), nhel, den(14))
    call cont_QA(nsync, wf8(:,8), wf16(:,7), A(:,8), n3(:,276), t3x128(:,:,8), nhel, den(14))
    call cont_QA(nsync, wf8(:,8), wf16(:,9), A(:,9), n3(:,277), t3x128(:,:,9), nhel, den(14))
    call cont_QA(nsync, wf16(:,5), wf8(:,9), A(:,10), n3(:,278), t3x128(:,:,10), nhel, den(16))
    call cont_QA(nsync, wf16(:,7), wf8(:,9), A(:,11), n3(:,279), t3x128(:,:,11), nhel, den(16))
    call cont_QA(nsync, wf16(:,9), wf8(:,9), A(:,12), n3(:,280), t3x128(:,:,12), nhel, den(16))
    call cont_QA(nsync, wf16(:,10), wf8(:,11), A(:,13), n3(:,281), t3x128(:,:,13), nhel, den(21))
    call cont_QA(nsync, wf8(:,11), wf16(:,11), A(:,14), n3(:,282), t3x128(:,:,14), nhel, den(21))
    call cont_QA(nsync, wf8(:,11), wf16(:,12), A(:,15), n3(:,283), t3x128(:,:,15), nhel, den(21))
    call cont_QA(nsync, wf16(:,10), wf8(:,13), A(:,16), n3(:,284), t3x128(:,:,16), nhel, den(23))
    call cont_QA(nsync, wf16(:,11), wf8(:,13), A(:,17), n3(:,285), t3x128(:,:,17), nhel, den(23))
    call cont_QA(nsync, wf16(:,12), wf8(:,13), A(:,18), n3(:,286), t3x128(:,:,18), nhel, den(23))
    call cont_QA(nsync, wf8(:,14), wf16(:,14), A(:,19), n3(:,287), t3x128(:,:,19), nhel, den(27))
    call cont_QA(nsync, wf8(:,14), wf16(:,16), A(:,20), n3(:,288), t3x128(:,:,20), nhel, den(27))
    call cont_QA(nsync, wf8(:,14), wf16(:,18), A(:,21), n3(:,289), t3x128(:,:,21), nhel, den(27))
    call cont_QA(nsync, wf16(:,14), wf8(:,15), A(:,22), n3(:,290), t3x128(:,:,22), nhel, den(29))
    call cont_QA(nsync, wf16(:,16), wf8(:,15), A(:,23), n3(:,291), t3x128(:,:,23), nhel, den(29))
    call cont_QA(nsync, wf16(:,18), wf8(:,15), A(:,24), n3(:,292), t3x128(:,:,24), nhel, den(29))
    call cont_QA(nsync, wf16(:,5), wf8(:,16), A(:,25), n3(:,293), t3x128(:,:,25), nhel, den(30))
    call cont_QA(nsync, wf16(:,7), wf8(:,16), A(:,26), n3(:,294), t3x128(:,:,26), nhel, den(30))
    call cont_QA(nsync, wf16(:,9), wf8(:,16), A(:,27), n3(:,295), t3x128(:,:,27), nhel, den(30))
    call cont_QA(nsync, wf16(:,5), wf8(:,17), A(:,28), n3(:,296), t3x128(:,:,28), nhel, den(31))
    call cont_QA(nsync, wf16(:,7), wf8(:,17), A(:,29), n3(:,297), t3x128(:,:,29), nhel, den(31))
    call cont_QA(nsync, wf16(:,9), wf8(:,17), A(:,30), n3(:,298), t3x128(:,:,30), nhel, den(31))
    call cont_QA(nsync, wf8(:,3), wf16(:,19), A(:,31), n3(:,299), t3x128(:,:,31), nhel, den(32))
    call cont_QA(nsync, wf8(:,3), wf16(:,20), A(:,32), n3(:,300), t3x128(:,:,32), nhel, den(32))
    call cont_QA(nsync, wf8(:,3), wf16(:,21), A(:,33), n3(:,301), t3x128(:,:,33), nhel, den(32))
    call cont_QA(nsync, wf8(:,7), wf16(:,19), A(:,34), n3(:,302), t3x128(:,:,34), nhel, den(33))
    call cont_QA(nsync, wf8(:,7), wf16(:,20), A(:,35), n3(:,303), t3x128(:,:,35), nhel, den(33))
    call cont_QA(nsync, wf8(:,7), wf16(:,21), A(:,36), n3(:,304), t3x128(:,:,36), nhel, den(33))
    call cont_VV(nsync, wf4(:,5), wf32(:,1), A(:,37), n3(:,305), t3x128(:,:,37), nhel, den(36))
    call cont_VV(nsync, wf4(:,5), wf32(:,2), A(:,38), n3(:,306), t3x128(:,:,38), nhel, den(36))
    call cont_VV(nsync, wf4(:,5), wf32(:,3), A(:,39), n3(:,307), t3x128(:,:,39), nhel, den(36))
    call cont_VV(nsync, wf4(:,5), wf32(:,4), A(:,40), n3(:,308), t3x128(:,:,40), nhel, den(37))
    call cont_VV(nsync, wf4(:,5), wf32(:,5), A(:,41), n3(:,309), t3x128(:,:,41), nhel, den(37))
    call cont_VV(nsync, wf4(:,5), wf32(:,6), A(:,42), n3(:,310), t3x128(:,:,42), nhel, den(37))
    call cont_QA(nsync, wf16(:,22), wf8(:,18), A(:,43), n3(:,311), t3x128(:,:,43), nhel, den(43))
    call cont_QA(nsync, wf16(:,22), wf8(:,19), A(:,44), n3(:,312), t3x128(:,:,44), nhel, den(45))
    call cont_QA(nsync, wf8(:,20), wf16(:,24), A(:,45), n3(:,313), t3x128(:,:,45), nhel, den(50))
    call cont_QA(nsync, wf8(:,20), wf16(:,26), A(:,46), n3(:,314), t3x128(:,:,46), nhel, den(53))
    call cont_QA(nsync, wf8(:,22), wf16(:,27), A(:,47), n3(:,315), t3x128(:,:,47), nhel, den(59))
    call cont_QA(nsync, wf8(:,22), wf16(:,28), A(:,48), n3(:,316), t3x128(:,:,48), nhel, den(61))
    call cont_QA(nsync, wf16(:,24), wf8(:,23), A(:,49), n3(:,317), t3x128(:,:,49), nhel, den(62))
    call cont_QA(nsync, wf16(:,26), wf8(:,23), A(:,50), n3(:,318), t3x128(:,:,50), nhel, den(63))
    call cont_QA(nsync, wf4(:,12), wf32(:,7), A(:,51), n3(:,319), t3x128(:,:,51), nhel, den(69))
    call cont_QA(nsync, wf4(:,12), wf32(:,8), A(:,52), n3(:,320), t3x128(:,:,52), nhel, den(71))
    call cont_QA(nsync, wf16(:,24), wf8(:,26), A(:,53), n3(:,321), t3x128(:,:,53), nhel, den(72))
    call cont_QA(nsync, wf16(:,26), wf8(:,26), A(:,54), n3(:,322), t3x128(:,:,54), nhel, den(73))
    call cont_QA(nsync, wf8(:,18), wf16(:,29), A(:,55), n3(:,323), t3x128(:,:,55), nhel, den(74))
    call cont_QA(nsync, wf8(:,19), wf16(:,29), A(:,56), n3(:,324), t3x128(:,:,56), nhel, den(75))
    call cont_QA(nsync, wf8(:,18), wf16(:,30), A(:,57), n3(:,325), t3x128(:,:,57), nhel, den(76))
    call cont_QA(nsync, wf8(:,19), wf16(:,30), A(:,58), n3(:,326), t3x128(:,:,58), nhel, den(77))
    call cont_QA(nsync, wf8(:,18), wf16(:,31), A(:,59), n3(:,327), t3x128(:,:,59), nhel, den(81))
    call cont_QA(nsync, wf8(:,19), wf16(:,31), A(:,60), n3(:,328), t3x128(:,:,60), nhel, den(82))
    call cont_QA(nsync, wf8(:,27), wf16(:,33), A(:,61), n3(:,329), t3x128(:,:,61), nhel, den(87))
    call cont_QA(nsync, wf8(:,27), wf16(:,35), A(:,62), n3(:,330), t3x128(:,:,62), nhel, den(90))
    call cont_QA(nsync, wf8(:,29), wf16(:,36), A(:,63), n3(:,331), t3x128(:,:,63), nhel, den(96))
    call cont_QA(nsync, wf8(:,29), wf16(:,37), A(:,64), n3(:,332), t3x128(:,:,64), nhel, den(98))
    call cont_QA(nsync, wf16(:,33), wf8(:,30), A(:,65), n3(:,333), t3x128(:,:,65), nhel, den(99))
    call cont_QA(nsync, wf16(:,35), wf8(:,30), A(:,66), n3(:,334), t3x128(:,:,66), nhel, den(100))
    call cont_QA(nsync, wf4(:,12), wf32(:,9), A(:,67), n3(:,335), t3x128(:,:,67), nhel, den(103))
    call cont_QA(nsync, wf4(:,12), wf32(:,10), A(:,68), n3(:,336), t3x128(:,:,68), nhel, den(105))
    call cont_QA(nsync, wf16(:,33), wf8(:,33), A(:,69), n3(:,337), t3x128(:,:,69), nhel, den(106))
    call cont_QA(nsync, wf16(:,35), wf8(:,33), A(:,70), n3(:,338), t3x128(:,:,70), nhel, den(107))
    call cont_QA(nsync, wf8(:,18), wf16(:,38), A(:,71), n3(:,339), t3x128(:,:,71), nhel, den(108))
    call cont_QA(nsync, wf8(:,19), wf16(:,38), A(:,72), n3(:,340), t3x128(:,:,72), nhel, den(109))
    call cont_QA(nsync, wf8(:,18), wf16(:,39), A(:,73), n3(:,341), t3x128(:,:,73), nhel, den(110))
    call cont_QA(nsync, wf8(:,19), wf16(:,39), A(:,74), n3(:,342), t3x128(:,:,74), nhel, den(111))
    call cont_QA(nsync, wf8(:,18), wf16(:,40), A(:,75), n3(:,343), t3x128(:,:,75), nhel, den(115))
    call cont_QA(nsync, wf8(:,19), wf16(:,40), A(:,76), n3(:,344), t3x128(:,:,76), nhel, den(116))
    call cont_QA(nsync, wf8(:,34), wf16(:,42), A(:,77), n3(:,345), t3x128(:,:,77), nhel, den(121))
    call cont_QA(nsync, wf8(:,34), wf16(:,44), A(:,78), n3(:,346), t3x128(:,:,78), nhel, den(124))
    call cont_QA(nsync, wf4(:,12), wf32(:,11), A(:,79), n3(:,347), t3x128(:,:,79), nhel, den(126))
    call cont_QA(nsync, wf4(:,12), wf32(:,12), A(:,80), n3(:,348), t3x128(:,:,80), nhel, den(128))
    call cont_QA(nsync, wf8(:,35), wf16(:,45), A(:,81), n3(:,349), t3x128(:,:,81), nhel, den(131))
    call cont_QA(nsync, wf8(:,35), wf16(:,46), A(:,82), n3(:,350), t3x128(:,:,82), nhel, den(132))
    call cont_VV(nsync, wf8(:,36), wf16(:,47), A(:,83), n3(:,351), t3x128(:,:,83), nhel, den(135))
    call cont_VV(nsync, wf8(:,36), wf16(:,48), A(:,84), n3(:,352), t3x128(:,:,84), nhel, den(137))
    call cont_QA(nsync, wf4(:,12), wf32(:,13), A(:,85), n3(:,353), t3x128(:,:,85), nhel, den(140))
    call cont_QA(nsync, wf4(:,12), wf32(:,14), A(:,86), n3(:,354), t3x128(:,:,86), nhel, den(142))
    call cont_QA(nsync, wf8(:,18), wf16(:,49), A(:,87), n3(:,355), t3x128(:,:,87), nhel, den(143))
    call cont_QA(nsync, wf8(:,19), wf16(:,49), A(:,88), n3(:,356), t3x128(:,:,88), nhel, den(144))
    call cont_QA(nsync, wf8(:,18), wf16(:,50), A(:,89), n3(:,357), t3x128(:,:,89), nhel, den(145))
    call cont_QA(nsync, wf8(:,19), wf16(:,50), A(:,90), n3(:,358), t3x128(:,:,90), nhel, den(146))
    call cont_QA(nsync, wf16(:,36), wf8(:,40), A(:,91), n3(:,359), t3x128(:,:,91), nhel, den(148))
    call cont_QA(nsync, wf16(:,37), wf8(:,40), A(:,92), n3(:,360), t3x128(:,:,92), nhel, den(149))
    call cont_QA(nsync, wf16(:,42), wf8(:,41), A(:,93), n3(:,361), t3x128(:,:,93), nhel, den(150))
    call cont_QA(nsync, wf16(:,44), wf8(:,41), A(:,94), n3(:,362), t3x128(:,:,94), nhel, den(151))
    call cont_QA(nsync, wf4(:,10), wf32(:,15), A(:,95), n3(:,363), t3x128(:,:,95), nhel, den(154))
    call cont_QA(nsync, wf4(:,10), wf32(:,16), A(:,96), n3(:,364), t3x128(:,:,96), nhel, den(156))
    call cont_QA(nsync, wf16(:,42), wf8(:,44), A(:,97), n3(:,365), t3x128(:,:,97), nhel, den(157))
    call cont_QA(nsync, wf16(:,44), wf8(:,44), A(:,98), n3(:,366), t3x128(:,:,98), nhel, den(158))
    call cont_QA(nsync, wf8(:,18), wf16(:,51), A(:,99), n3(:,367), t3x128(:,:,99), nhel, den(159))
    call cont_QA(nsync, wf8(:,19), wf16(:,51), A(:,100), n3(:,368), t3x128(:,:,100), nhel, den(160))
    call cont_QA(nsync, wf8(:,18), wf16(:,52), A(:,101), n3(:,369), t3x128(:,:,101), nhel, den(161))
    call cont_QA(nsync, wf8(:,19), wf16(:,52), A(:,102), n3(:,370), t3x128(:,:,102), nhel, den(162))
    call cont_QA(nsync, wf4(:,10), wf32(:,17), A(:,103), n3(:,371), t3x128(:,:,103), nhel, den(164))
    call cont_QA(nsync, wf4(:,10), wf32(:,18), A(:,104), n3(:,372), t3x128(:,:,104), nhel, den(166))
    call cont_QA(nsync, wf8(:,45), wf16(:,53), A(:,105), n3(:,373), t3x128(:,:,105), nhel, den(169))
    call cont_QA(nsync, wf8(:,45), wf16(:,54), A(:,106), n3(:,374), t3x128(:,:,106), nhel, den(170))
    call cont_VV(nsync, wf16(:,47), wf8(:,46), A(:,107), n3(:,375), t3x128(:,:,107), nhel, den(172))
    call cont_VV(nsync, wf16(:,48), wf8(:,46), A(:,108), n3(:,376), t3x128(:,:,108), nhel, den(173))
    call cont_QA(nsync, wf4(:,10), wf32(:,19), A(:,109), n3(:,377), t3x128(:,:,109), nhel, den(176))
    call cont_QA(nsync, wf4(:,10), wf32(:,20), A(:,110), n3(:,378), t3x128(:,:,110), nhel, den(178))
    call cont_QA(nsync, wf8(:,18), wf16(:,55), A(:,111), n3(:,379), t3x128(:,:,111), nhel, den(179))
    call cont_QA(nsync, wf8(:,19), wf16(:,55), A(:,112), n3(:,380), t3x128(:,:,112), nhel, den(180))
    call cont_QA(nsync, wf8(:,18), wf16(:,56), A(:,113), n3(:,381), t3x128(:,:,113), nhel, den(181))
    call cont_QA(nsync, wf8(:,19), wf16(:,56), A(:,114), n3(:,382), t3x128(:,:,114), nhel, den(182))
    call cont_VV(nsync, wf4(:,7), wf32(:,21), A(:,115), n3(:,383), t3x128(:,:,115), nhel, den(184))
    call cont_VV(nsync, wf4(:,7), wf32(:,22), A(:,116), n3(:,384), t3x128(:,:,116), nhel, den(186))
    call cont_QA(nsync, wf8(:,49), wf16(:,57), A(:,117), n3(:,385), t3x128(:,:,117), nhel, den(189))
    call cont_QA(nsync, wf8(:,49), wf16(:,58), A(:,118), n3(:,386), t3x128(:,:,118), nhel, den(190))
    call cont_VV(nsync, wf16(:,47), wf8(:,50), A(:,119), n3(:,387), t3x128(:,:,119), nhel, den(192))
    call cont_VV(nsync, wf16(:,48), wf8(:,50), A(:,120), n3(:,388), t3x128(:,:,120), nhel, den(193))
    call cont_QA(nsync, wf4(:,17), wf32(:,23), A(:,121), n3(:,389), t3x128(:,:,121), nhel, den(196))
    call cont_QA(nsync, wf4(:,17), wf32(:,24), A(:,122), n3(:,390), t3x128(:,:,122), nhel, den(198))
    call cont_QA(nsync, wf8(:,18), wf16(:,59), A(:,123), n3(:,391), t3x128(:,:,123), nhel, den(199))
    call cont_QA(nsync, wf8(:,19), wf16(:,59), A(:,124), n3(:,392), t3x128(:,:,124), nhel, den(200))
    call cont_QA(nsync, wf8(:,18), wf16(:,60), A(:,125), n3(:,393), t3x128(:,:,125), nhel, den(201))
    call cont_QA(nsync, wf8(:,19), wf16(:,60), A(:,126), n3(:,394), t3x128(:,:,126), nhel, den(202))
    call cont_QA(nsync, wf8(:,30), wf16(:,61), A(:,127), n3(:,395), t3x128(:,:,127), nhel, den(204))
    call cont_QA(nsync, wf8(:,30), wf16(:,62), A(:,128), n3(:,396), t3x128(:,:,128), nhel, den(206))
    call cont_QA(nsync, wf16(:,45), wf8(:,53), A(:,129), n3(:,397), t3x128(:,:,129), nhel, den(208))
    call cont_QA(nsync, wf16(:,46), wf8(:,53), A(:,130), n3(:,398), t3x128(:,:,130), nhel, den(209))
    call cont_QA(nsync, wf8(:,23), wf16(:,63), A(:,131), n3(:,399), t3x128(:,:,131), nhel, den(211))
    call cont_QA(nsync, wf8(:,23), wf16(:,64), A(:,132), n3(:,400), t3x128(:,:,132), nhel, den(213))
    call cont_QA(nsync, wf16(:,45), wf8(:,54), A(:,133), n3(:,401), t3x128(:,:,133), nhel, den(215))
    call cont_QA(nsync, wf16(:,46), wf8(:,54), A(:,134), n3(:,402), t3x128(:,:,134), nhel, den(216))
    call cont_QA(nsync, wf8(:,26), wf16(:,63), A(:,135), n3(:,403), t3x128(:,:,135), nhel, den(217))
    call cont_QA(nsync, wf8(:,26), wf16(:,64), A(:,136), n3(:,404), t3x128(:,:,136), nhel, den(218))
    call cont_QA(nsync, wf16(:,53), wf8(:,55), A(:,137), n3(:,405), t3x128(:,:,137), nhel, den(220))
    call cont_QA(nsync, wf16(:,54), wf8(:,55), A(:,138), n3(:,406), t3x128(:,:,138), nhel, den(221))
    call cont_QA(nsync, wf8(:,56), wf16(:,66), A(:,139), n3(:,407), t3x128(:,:,139), nhel, den(227))
    call cont_QA(nsync, wf8(:,56), wf16(:,68), A(:,140), n3(:,408), t3x128(:,:,140), nhel, den(230))
    call cont_QA(nsync, wf16(:,69), wf8(:,57), A(:,141), n3(:,409), t3x128(:,:,141), nhel, den(234))
    call cont_QA(nsync, wf16(:,69), wf8(:,58), A(:,142), n3(:,410), t3x128(:,:,142), nhel, den(236))
    call cont_QA(nsync, wf4(:,24), wf32(:,25), A(:,143), n3(:,411), t3x128(:,:,143), nhel, den(242))
    call cont_QA(nsync, wf4(:,24), wf32(:,26), A(:,144), n3(:,412), t3x128(:,:,144), nhel, den(244))
    call cont_QA(nsync, wf8(:,57), wf16(:,70), A(:,145), n3(:,413), t3x128(:,:,145), nhel, den(245))
    call cont_QA(nsync, wf8(:,58), wf16(:,70), A(:,146), n3(:,414), t3x128(:,:,146), nhel, den(246))
    call cont_QA(nsync, wf8(:,62), wf16(:,71), A(:,147), n3(:,415), t3x128(:,:,147), nhel, den(252))
    call cont_QA(nsync, wf8(:,62), wf16(:,72), A(:,148), n3(:,416), t3x128(:,:,148), nhel, den(254))
    call cont_QA(nsync, wf8(:,57), wf16(:,73), A(:,149), n3(:,417), t3x128(:,:,149), nhel, den(255))
    call cont_QA(nsync, wf8(:,58), wf16(:,73), A(:,150), n3(:,418), t3x128(:,:,150), nhel, den(256))
    call cont_QA(nsync, wf16(:,66), wf8(:,63), A(:,151), n3(:,419), t3x128(:,:,151), nhel, den(257))
    call cont_QA(nsync, wf16(:,68), wf8(:,63), A(:,152), n3(:,420), t3x128(:,:,152), nhel, den(258))
    call cont_QA(nsync, wf16(:,66), wf8(:,64), A(:,153), n3(:,421), t3x128(:,:,153), nhel, den(259))
    call cont_QA(nsync, wf16(:,68), wf8(:,64), A(:,154), n3(:,422), t3x128(:,:,154), nhel, den(260))
    call cont_QA(nsync, wf8(:,65), wf16(:,75), A(:,155), n3(:,423), t3x128(:,:,155), nhel, den(266))
    call cont_QA(nsync, wf8(:,65), wf16(:,77), A(:,156), n3(:,424), t3x128(:,:,156), nhel, den(269))
    call cont_QA(nsync, wf8(:,57), wf16(:,78), A(:,157), n3(:,425), t3x128(:,:,157), nhel, den(271))
    call cont_QA(nsync, wf8(:,58), wf16(:,78), A(:,158), n3(:,426), t3x128(:,:,158), nhel, den(272))
    call cont_QA(nsync, wf4(:,24), wf32(:,27), A(:,159), n3(:,427), t3x128(:,:,159), nhel, den(275))
    call cont_QA(nsync, wf4(:,24), wf32(:,28), A(:,160), n3(:,428), t3x128(:,:,160), nhel, den(277))
    call cont_QA(nsync, wf8(:,57), wf16(:,79), A(:,161), n3(:,429), t3x128(:,:,161), nhel, den(278))
    call cont_QA(nsync, wf8(:,58), wf16(:,79), A(:,162), n3(:,430), t3x128(:,:,162), nhel, den(279))
    call cont_QA(nsync, wf8(:,69), wf16(:,80), A(:,163), n3(:,431), t3x128(:,:,163), nhel, den(285))
    call cont_QA(nsync, wf8(:,69), wf16(:,81), A(:,164), n3(:,432), t3x128(:,:,164), nhel, den(287))
    call cont_QA(nsync, wf8(:,57), wf16(:,82), A(:,165), n3(:,433), t3x128(:,:,165), nhel, den(288))
    call cont_QA(nsync, wf8(:,58), wf16(:,82), A(:,166), n3(:,434), t3x128(:,:,166), nhel, den(289))
    call cont_QA(nsync, wf16(:,75), wf8(:,70), A(:,167), n3(:,435), t3x128(:,:,167), nhel, den(290))
    call cont_QA(nsync, wf16(:,77), wf8(:,70), A(:,168), n3(:,436), t3x128(:,:,168), nhel, den(291))
    call cont_QA(nsync, wf16(:,75), wf8(:,71), A(:,169), n3(:,437), t3x128(:,:,169), nhel, den(292))
    call cont_QA(nsync, wf16(:,77), wf8(:,71), A(:,170), n3(:,438), t3x128(:,:,170), nhel, den(293))
    call cont_QA(nsync, wf8(:,72), wf16(:,84), A(:,171), n3(:,439), t3x128(:,:,171), nhel, den(299))
    call cont_QA(nsync, wf8(:,72), wf16(:,86), A(:,172), n3(:,440), t3x128(:,:,172), nhel, den(302))
    call cont_QA(nsync, wf8(:,57), wf16(:,87), A(:,173), n3(:,441), t3x128(:,:,173), nhel, den(304))
    call cont_QA(nsync, wf8(:,58), wf16(:,87), A(:,174), n3(:,442), t3x128(:,:,174), nhel, den(305))
    call cont_QA(nsync, wf8(:,73), wf16(:,88), A(:,175), n3(:,443), t3x128(:,:,175), nhel, den(308))
    call cont_QA(nsync, wf8(:,73), wf16(:,89), A(:,176), n3(:,444), t3x128(:,:,176), nhel, den(309))
    call cont_QA(nsync, wf4(:,24), wf32(:,29), A(:,177), n3(:,445), t3x128(:,:,177), nhel, den(311))
    call cont_QA(nsync, wf4(:,24), wf32(:,30), A(:,178), n3(:,446), t3x128(:,:,178), nhel, den(313))
    call cont_VV(nsync, wf8(:,36), wf16(:,90), A(:,179), n3(:,447), t3x128(:,:,179), nhel, den(315))
    call cont_VV(nsync, wf8(:,36), wf16(:,91), A(:,180), n3(:,448), t3x128(:,:,180), nhel, den(317))
    call cont_QA(nsync, wf8(:,57), wf16(:,92), A(:,181), n3(:,449), t3x128(:,:,181), nhel, den(319))
    call cont_QA(nsync, wf8(:,58), wf16(:,92), A(:,182), n3(:,450), t3x128(:,:,182), nhel, den(320))
    call cont_QA(nsync, wf4(:,24), wf32(:,31), A(:,183), n3(:,451), t3x128(:,:,183), nhel, den(322))
    call cont_QA(nsync, wf4(:,24), wf32(:,32), A(:,184), n3(:,452), t3x128(:,:,184), nhel, den(324))
    call cont_QA(nsync, wf8(:,57), wf16(:,93), A(:,185), n3(:,453), t3x128(:,:,185), nhel, den(325))
    call cont_QA(nsync, wf8(:,58), wf16(:,93), A(:,186), n3(:,454), t3x128(:,:,186), nhel, den(326))
    call cont_QA(nsync, wf4(:,26), wf32(:,33), A(:,187), n3(:,455), t3x128(:,:,187), nhel, den(329))
    call cont_QA(nsync, wf4(:,26), wf32(:,34), A(:,188), n3(:,456), t3x128(:,:,188), nhel, den(331))
    call cont_QA(nsync, wf8(:,57), wf16(:,94), A(:,189), n3(:,457), t3x128(:,:,189), nhel, den(332))
    call cont_QA(nsync, wf8(:,58), wf16(:,94), A(:,190), n3(:,458), t3x128(:,:,190), nhel, den(333))
    call cont_QA(nsync, wf16(:,80), wf8(:,79), A(:,191), n3(:,459), t3x128(:,:,191), nhel, den(335))
    call cont_QA(nsync, wf16(:,81), wf8(:,79), A(:,192), n3(:,460), t3x128(:,:,192), nhel, den(336))
    call cont_QA(nsync, wf8(:,57), wf16(:,95), A(:,193), n3(:,461), t3x128(:,:,193), nhel, den(337))
    call cont_QA(nsync, wf8(:,58), wf16(:,95), A(:,194), n3(:,462), t3x128(:,:,194), nhel, den(338))
    call cont_QA(nsync, wf16(:,84), wf8(:,80), A(:,195), n3(:,463), t3x128(:,:,195), nhel, den(339))
    call cont_QA(nsync, wf16(:,86), wf8(:,80), A(:,196), n3(:,464), t3x128(:,:,196), nhel, den(340))
    call cont_QA(nsync, wf16(:,84), wf8(:,81), A(:,197), n3(:,465), t3x128(:,:,197), nhel, den(341))
    call cont_QA(nsync, wf16(:,86), wf8(:,81), A(:,198), n3(:,466), t3x128(:,:,198), nhel, den(342))
    call cont_QA(nsync, wf8(:,82), wf16(:,96), A(:,199), n3(:,467), t3x128(:,:,199), nhel, den(345))
    call cont_QA(nsync, wf8(:,82), wf16(:,97), A(:,200), n3(:,468), t3x128(:,:,200), nhel, den(346))
    call cont_QA(nsync, wf4(:,26), wf32(:,35), A(:,201), n3(:,469), t3x128(:,:,201), nhel, den(348))
    call cont_QA(nsync, wf4(:,26), wf32(:,36), A(:,202), n3(:,470), t3x128(:,:,202), nhel, den(350))
    call cont_VV(nsync, wf8(:,46), wf16(:,90), A(:,203), n3(:,471), t3x128(:,:,203), nhel, den(351))
    call cont_VV(nsync, wf8(:,46), wf16(:,91), A(:,204), n3(:,472), t3x128(:,:,204), nhel, den(352))
    call cont_QA(nsync, wf8(:,57), wf16(:,98), A(:,205), n3(:,473), t3x128(:,:,205), nhel, den(354))
    call cont_QA(nsync, wf8(:,58), wf16(:,98), A(:,206), n3(:,474), t3x128(:,:,206), nhel, den(355))
    call cont_QA(nsync, wf4(:,26), wf32(:,37), A(:,207), n3(:,475), t3x128(:,:,207), nhel, den(357))
    call cont_QA(nsync, wf4(:,26), wf32(:,38), A(:,208), n3(:,476), t3x128(:,:,208), nhel, den(359))
    call cont_QA(nsync, wf8(:,57), wf16(:,99), A(:,209), n3(:,477), t3x128(:,:,209), nhel, den(360))
    call cont_QA(nsync, wf8(:,58), wf16(:,99), A(:,210), n3(:,478), t3x128(:,:,210), nhel, den(361))
    call cont_QA(nsync, wf8(:,85), wf16(:,100), A(:,211), n3(:,479), t3x128(:,:,211), nhel, den(364))
    call cont_QA(nsync, wf8(:,85), wf16(:,101), A(:,212), n3(:,480), t3x128(:,:,212), nhel, den(365))
    call cont_VV(nsync, wf4(:,7), wf32(:,39), A(:,213), n3(:,481), t3x128(:,:,213), nhel, den(367))
    call cont_VV(nsync, wf4(:,7), wf32(:,40), A(:,214), n3(:,482), t3x128(:,:,214), nhel, den(369))
    call cont_VV(nsync, wf8(:,50), wf16(:,90), A(:,215), n3(:,483), t3x128(:,:,215), nhel, den(370))
    call cont_VV(nsync, wf8(:,50), wf16(:,91), A(:,216), n3(:,484), t3x128(:,:,216), nhel, den(371))
    call cont_QA(nsync, wf8(:,57), wf16(:,102), A(:,217), n3(:,485), t3x128(:,:,217), nhel, den(373))
    call cont_QA(nsync, wf8(:,58), wf16(:,102), A(:,218), n3(:,486), t3x128(:,:,218), nhel, den(374))
    call cont_QA(nsync, wf8(:,57), wf16(:,103), A(:,219), n3(:,487), t3x128(:,:,219), nhel, den(375))
    call cont_QA(nsync, wf8(:,58), wf16(:,103), A(:,220), n3(:,488), t3x128(:,:,220), nhel, den(376))
    call cont_QA(nsync, wf4(:,30), wf32(:,41), A(:,221), n3(:,489), t3x128(:,:,221), nhel, den(378))
    call cont_QA(nsync, wf4(:,30), wf32(:,42), A(:,222), n3(:,490), t3x128(:,:,222), nhel, den(380))
    call cont_QA(nsync, wf8(:,63), wf16(:,104), A(:,223), n3(:,491), t3x128(:,:,223), nhel, den(382))
    call cont_QA(nsync, wf8(:,63), wf16(:,105), A(:,224), n3(:,492), t3x128(:,:,224), nhel, den(384))
    call cont_QA(nsync, wf8(:,64), wf16(:,104), A(:,225), n3(:,493), t3x128(:,:,225), nhel, den(385))
    call cont_QA(nsync, wf8(:,64), wf16(:,105), A(:,226), n3(:,494), t3x128(:,:,226), nhel, den(386))
    call cont_QA(nsync, wf8(:,70), wf16(:,106), A(:,227), n3(:,495), t3x128(:,:,227), nhel, den(388))
    call cont_QA(nsync, wf8(:,70), wf16(:,107), A(:,228), n3(:,496), t3x128(:,:,228), nhel, den(390))
    call cont_QA(nsync, wf16(:,96), wf8(:,88), A(:,229), n3(:,497), t3x128(:,:,229), nhel, den(392))
    call cont_QA(nsync, wf16(:,97), wf8(:,88), A(:,230), n3(:,498), t3x128(:,:,230), nhel, den(393))
    call cont_QA(nsync, wf16(:,88), wf8(:,89), A(:,231), n3(:,499), t3x128(:,:,231), nhel, den(395))
    call cont_QA(nsync, wf16(:,89), wf8(:,89), A(:,232), n3(:,500), t3x128(:,:,232), nhel, den(396))
    call cont_QA(nsync, wf16(:,88), wf8(:,90), A(:,233), n3(:,501), t3x128(:,:,233), nhel, den(398))
    call cont_QA(nsync, wf16(:,89), wf8(:,90), A(:,234), n3(:,502), t3x128(:,:,234), nhel, den(399))
    call cont_QA(nsync, wf8(:,62), wf16(:,108), A(:,235), n3(:,503), t3x128(:,:,235), nhel, den(401))
    call cont_QA(nsync, wf8(:,29), wf16(:,109), A(:,236), n3(:,504), t3x128(:,:,236), nhel, den(403))
    call cont_QA(nsync, wf8(:,29), wf16(:,110), A(:,237), n3(:,505), t3x128(:,:,237), nhel, den(406))
    call cont_QA(nsync, wf8(:,29), wf16(:,111), A(:,238), n3(:,506), t3x128(:,:,238), nhel, den(408))
    call cont_QA(nsync, wf16(:,33), wf8(:,91), A(:,239), n3(:,507), t3x128(:,:,239), nhel, den(409))
    call cont_QA(nsync, wf16(:,35), wf8(:,91), A(:,240), n3(:,508), t3x128(:,:,240), nhel, den(410))
    call cont_QA(nsync, wf8(:,62), wf16(:,112), A(:,241), n3(:,509), t3x128(:,:,241), nhel, den(413))
    call cont_QA(nsync, wf8(:,62), wf16(:,113), A(:,242), n3(:,510), t3x128(:,:,242), nhel, den(415))
    call cont_QA(nsync, wf16(:,33), wf8(:,92), A(:,243), n3(:,511), t3x128(:,:,243), nhel, den(416))
    call cont_QA(nsync, wf16(:,35), wf8(:,92), A(:,244), n3(:,512), t3x128(:,:,244), nhel, den(417))
    call cont_QA(nsync, wf16(:,66), wf8(:,93), A(:,245), n3(:,513), t3x128(:,:,245), nhel, den(418))
    call cont_QA(nsync, wf16(:,68), wf8(:,93), A(:,246), n3(:,514), t3x128(:,:,246), nhel, den(419))
    call cont_QA(nsync, wf16(:,66), wf8(:,94), A(:,247), n3(:,515), t3x128(:,:,247), nhel, den(420))
    call cont_QA(nsync, wf16(:,68), wf8(:,94), A(:,248), n3(:,516), t3x128(:,:,248), nhel, den(421))
    call cont_QA(nsync, wf8(:,60), wf16(:,114), A(:,249), n3(:,517), t3x128(:,:,249), nhel, den(423))
    call cont_QA(nsync, wf8(:,40), wf16(:,109), A(:,250), n3(:,518), t3x128(:,:,250), nhel, den(424))
    call cont_QA(nsync, wf8(:,40), wf16(:,110), A(:,251), n3(:,519), t3x128(:,:,251), nhel, den(425))
    call cont_QA(nsync, wf8(:,40), wf16(:,111), A(:,252), n3(:,520), t3x128(:,:,252), nhel, den(426))
    call cont_QA(nsync, wf16(:,42), wf8(:,95), A(:,253), n3(:,521), t3x128(:,:,253), nhel, den(427))
    call cont_QA(nsync, wf16(:,44), wf8(:,95), A(:,254), n3(:,522), t3x128(:,:,254), nhel, den(428))
    call cont_QA(nsync, wf8(:,60), wf16(:,115), A(:,255), n3(:,523), t3x128(:,:,255), nhel, den(431))
    call cont_QA(nsync, wf8(:,60), wf16(:,116), A(:,256), n3(:,524), t3x128(:,:,256), nhel, den(433))
    call cont_QA(nsync, wf16(:,42), wf8(:,96), A(:,257), n3(:,525), t3x128(:,:,257), nhel, den(434))
    call cont_QA(nsync, wf16(:,44), wf8(:,96), A(:,258), n3(:,526), t3x128(:,:,258), nhel, den(435))
    call cont_QA(nsync, wf16(:,66), wf8(:,97), A(:,259), n3(:,527), t3x128(:,:,259), nhel, den(436))
    call cont_QA(nsync, wf16(:,68), wf8(:,97), A(:,260), n3(:,528), t3x128(:,:,260), nhel, den(437))
    call cont_QA(nsync, wf16(:,66), wf8(:,98), A(:,261), n3(:,529), t3x128(:,:,261), nhel, den(438))
    call cont_QA(nsync, wf16(:,68), wf8(:,98), A(:,262), n3(:,530), t3x128(:,:,262), nhel, den(439))
    call cont_VV(nsync, wf4(:,7), wf32(:,43), A(:,263), n3(:,531), t3x128(:,:,263), nhel, den(441))
    call cont_VV(nsync, wf4(:,7), wf32(:,44), A(:,264), n3(:,532), t3x128(:,:,264), nhel, den(443))
    call cont_QA(nsync, wf16(:,117), wf8(:,99), A(:,265), n3(:,533), t3x128(:,:,265), nhel, den(445))
    call cont_QA(nsync, wf16(:,117), wf8(:,100), A(:,266), n3(:,534), t3x128(:,:,266), nhel, den(446))
    call cont_QA(nsync, wf16(:,117), wf8(:,101), A(:,267), n3(:,535), t3x128(:,:,267), nhel, den(447))
    call cont_VV(nsync, wf4(:,5), wf32(:,45), A(:,268), n3(:,536), t3x128(:,:,268), nhel, den(449))
    call cont_QA(nsync, wf32(:,23), wf4(:,34), A(:,269), n3(:,537), t3x128(:,:,269), nhel, den(450))
    call cont_QA(nsync, wf32(:,24), wf4(:,34), A(:,270), n3(:,538), t3x128(:,:,270), nhel, den(451))
    call cont_QA(nsync, wf16(:,66), wf8(:,102), A(:,271), n3(:,539), t3x128(:,:,271), nhel, den(452))
    call cont_QA(nsync, wf16(:,68), wf8(:,102), A(:,272), n3(:,540), t3x128(:,:,272), nhel, den(453))
    call cont_QA(nsync, wf16(:,61), wf8(:,91), A(:,273), n3(:,541), t3x128(:,:,273), nhel, den(454))
    call cont_QA(nsync, wf16(:,62), wf8(:,91), A(:,274), n3(:,542), t3x128(:,:,274), nhel, den(455))
    call cont_QA(nsync, wf16(:,45), wf8(:,103), A(:,275), n3(:,543), t3x128(:,:,275), nhel, den(457))
    call cont_QA(nsync, wf16(:,46), wf8(:,103), A(:,276), n3(:,544), t3x128(:,:,276), nhel, den(458))
    call cont_QA(nsync, wf16(:,70), wf8(:,104), A(:,277), n3(:,545), t3x128(:,:,277), nhel, den(460))
    call cont_QA(nsync, wf16(:,70), wf8(:,105), A(:,278), n3(:,546), t3x128(:,:,278), nhel, den(462))
    call cont_QA(nsync, wf16(:,45), wf8(:,106), A(:,279), n3(:,547), t3x128(:,:,279), nhel, den(464))
    call cont_QA(nsync, wf16(:,46), wf8(:,106), A(:,280), n3(:,548), t3x128(:,:,280), nhel, den(465))
    call cont_QA(nsync, wf16(:,73), wf8(:,104), A(:,281), n3(:,549), t3x128(:,:,281), nhel, den(466))
    call cont_QA(nsync, wf16(:,73), wf8(:,105), A(:,282), n3(:,550), t3x128(:,:,282), nhel, den(467))
    call cont_QA(nsync, wf16(:,53), wf8(:,107), A(:,283), n3(:,551), t3x128(:,:,283), nhel, den(469))
    call cont_QA(nsync, wf16(:,54), wf8(:,107), A(:,284), n3(:,552), t3x128(:,:,284), nhel, den(470))
    call cont_QA(nsync, wf16(:,70), wf8(:,108), A(:,285), n3(:,553), t3x128(:,:,285), nhel, den(472))
    call cont_QA(nsync, wf16(:,73), wf8(:,108), A(:,286), n3(:,554), t3x128(:,:,286), nhel, den(473))
    call cont_QA(nsync, wf8(:,69), wf16(:,118), A(:,287), n3(:,555), t3x128(:,:,287), nhel, den(475))
    call cont_QA(nsync, wf8(:,22), wf16(:,119), A(:,288), n3(:,556), t3x128(:,:,288), nhel, den(477))
    call cont_QA(nsync, wf8(:,22), wf16(:,120), A(:,289), n3(:,557), t3x128(:,:,289), nhel, den(480))
    call cont_QA(nsync, wf8(:,22), wf16(:,121), A(:,290), n3(:,558), t3x128(:,:,290), nhel, den(482))
    call cont_QA(nsync, wf16(:,24), wf8(:,109), A(:,291), n3(:,559), t3x128(:,:,291), nhel, den(483))
    call cont_QA(nsync, wf16(:,26), wf8(:,109), A(:,292), n3(:,560), t3x128(:,:,292), nhel, den(484))
    call cont_QA(nsync, wf8(:,69), wf16(:,122), A(:,293), n3(:,561), t3x128(:,:,293), nhel, den(487))
    call cont_QA(nsync, wf8(:,69), wf16(:,123), A(:,294), n3(:,562), t3x128(:,:,294), nhel, den(489))
    call cont_QA(nsync, wf16(:,24), wf8(:,110), A(:,295), n3(:,563), t3x128(:,:,295), nhel, den(490))
    call cont_QA(nsync, wf16(:,26), wf8(:,110), A(:,296), n3(:,564), t3x128(:,:,296), nhel, den(491))
    call cont_QA(nsync, wf16(:,75), wf8(:,111), A(:,297), n3(:,565), t3x128(:,:,297), nhel, den(492))
    call cont_QA(nsync, wf16(:,77), wf8(:,111), A(:,298), n3(:,566), t3x128(:,:,298), nhel, den(493))
    call cont_QA(nsync, wf16(:,75), wf8(:,112), A(:,299), n3(:,567), t3x128(:,:,299), nhel, den(494))
    call cont_QA(nsync, wf16(:,77), wf8(:,112), A(:,300), n3(:,568), t3x128(:,:,300), nhel, den(495))
    call cont_QA(nsync, wf8(:,79), wf16(:,118), A(:,301), n3(:,569), t3x128(:,:,301), nhel, den(496))
    call cont_QA(nsync, wf8(:,25), wf16(:,124), A(:,302), n3(:,570), t3x128(:,:,302), nhel, den(498))
    call cont_QA(nsync, wf8(:,25), wf16(:,125), A(:,303), n3(:,571), t3x128(:,:,303), nhel, den(501))
    call cont_QA(nsync, wf8(:,25), wf16(:,126), A(:,304), n3(:,572), t3x128(:,:,304), nhel, den(503))
    call cont_QA(nsync, wf16(:,24), wf8(:,113), A(:,305), n3(:,573), t3x128(:,:,305), nhel, den(504))
    call cont_QA(nsync, wf16(:,26), wf8(:,113), A(:,306), n3(:,574), t3x128(:,:,306), nhel, den(505))
    call cont_QA(nsync, wf8(:,79), wf16(:,122), A(:,307), n3(:,575), t3x128(:,:,307), nhel, den(506))
    call cont_QA(nsync, wf8(:,79), wf16(:,123), A(:,308), n3(:,576), t3x128(:,:,308), nhel, den(507))
    call cont_QA(nsync, wf16(:,24), wf8(:,114), A(:,309), n3(:,577), t3x128(:,:,309), nhel, den(508))
    call cont_QA(nsync, wf16(:,26), wf8(:,114), A(:,310), n3(:,578), t3x128(:,:,310), nhel, den(509))
    call cont_QA(nsync, wf16(:,84), wf8(:,115), A(:,311), n3(:,579), t3x128(:,:,311), nhel, den(510))
    call cont_QA(nsync, wf16(:,86), wf8(:,115), A(:,312), n3(:,580), t3x128(:,:,312), nhel, den(511))
    call cont_QA(nsync, wf16(:,84), wf8(:,116), A(:,313), n3(:,581), t3x128(:,:,313), nhel, den(512))
    call cont_QA(nsync, wf16(:,86), wf8(:,116), A(:,314), n3(:,582), t3x128(:,:,314), nhel, den(513))
    call cont_QA(nsync, wf8(:,16), wf16(:,127), A(:,315), n3(:,583), t3x128(:,:,315), nhel, den(515))
    call cont_QA(nsync, wf8(:,17), wf16(:,127), A(:,316), n3(:,584), t3x128(:,:,316), nhel, den(516))
    call cont_VV(nsync, wf4(:,7), wf32(:,46), A(:,317), n3(:,585), t3x128(:,:,317), nhel, den(518))
    call cont_VV(nsync, wf4(:,7), wf32(:,47), A(:,318), n3(:,586), t3x128(:,:,318), nhel, den(520))
    call cont_QA(nsync, wf16(:,127), wf8(:,117), A(:,319), n3(:,587), t3x128(:,:,319), nhel, den(521))
    call cont_VV(nsync, wf4(:,5), wf32(:,48), A(:,320), n3(:,588), t3x128(:,:,320), nhel, den(523))
    call cont_QA(nsync, wf16(:,24), wf8(:,118), A(:,321), n3(:,589), t3x128(:,:,321), nhel, den(524))
    call cont_QA(nsync, wf16(:,26), wf8(:,118), A(:,322), n3(:,590), t3x128(:,:,322), nhel, den(525))
    call cont_QA(nsync, wf32(:,41), wf4(:,42), A(:,323), n3(:,591), t3x128(:,:,323), nhel, den(526))
    call cont_QA(nsync, wf32(:,42), wf4(:,42), A(:,324), n3(:,592), t3x128(:,:,324), nhel, den(527))
    call cont_QA(nsync, wf16(:,29), wf8(:,119), A(:,325), n3(:,593), t3x128(:,:,325), nhel, den(529))
    call cont_QA(nsync, wf16(:,29), wf8(:,120), A(:,326), n3(:,594), t3x128(:,:,326), nhel, den(531))
    call cont_QA(nsync, wf16(:,30), wf8(:,119), A(:,327), n3(:,595), t3x128(:,:,327), nhel, den(532))
    call cont_QA(nsync, wf16(:,30), wf8(:,120), A(:,328), n3(:,596), t3x128(:,:,328), nhel, den(533))
    call cont_QA(nsync, wf16(:,106), wf8(:,111), A(:,329), n3(:,597), t3x128(:,:,329), nhel, den(534))
    call cont_QA(nsync, wf16(:,107), wf8(:,111), A(:,330), n3(:,598), t3x128(:,:,330), nhel, den(535))
    call cont_QA(nsync, wf16(:,96), wf8(:,121), A(:,331), n3(:,599), t3x128(:,:,331), nhel, den(537))
    call cont_QA(nsync, wf16(:,97), wf8(:,121), A(:,332), n3(:,600), t3x128(:,:,332), nhel, den(538))
    call cont_QA(nsync, wf16(:,88), wf8(:,122), A(:,333), n3(:,601), t3x128(:,:,333), nhel, den(540))
    call cont_QA(nsync, wf16(:,89), wf8(:,122), A(:,334), n3(:,602), t3x128(:,:,334), nhel, den(541))
    call cont_QA(nsync, wf16(:,88), wf8(:,123), A(:,335), n3(:,603), t3x128(:,:,335), nhel, den(543))
    call cont_QA(nsync, wf16(:,89), wf8(:,123), A(:,336), n3(:,604), t3x128(:,:,336), nhel, den(544))
    call cont_QA(nsync, wf16(:,30), wf8(:,124), A(:,337), n3(:,605), t3x128(:,:,337), nhel, den(546))
    call cont_QA(nsync, wf16(:,29), wf8(:,124), A(:,338), n3(:,606), t3x128(:,:,338), nhel, den(547))
    call cont_QA(nsync, wf8(:,67), wf16(:,114), A(:,339), n3(:,607), t3x128(:,:,339), nhel, den(548))
    call cont_QA(nsync, wf8(:,43), wf16(:,119), A(:,340), n3(:,608), t3x128(:,:,340), nhel, den(549))
    call cont_QA(nsync, wf8(:,43), wf16(:,120), A(:,341), n3(:,609), t3x128(:,:,341), nhel, den(550))
    call cont_QA(nsync, wf8(:,43), wf16(:,121), A(:,342), n3(:,610), t3x128(:,:,342), nhel, den(551))
    call cont_QA(nsync, wf16(:,42), wf8(:,125), A(:,343), n3(:,611), t3x128(:,:,343), nhel, den(552))
    call cont_QA(nsync, wf16(:,44), wf8(:,125), A(:,344), n3(:,612), t3x128(:,:,344), nhel, den(553))
    call cont_QA(nsync, wf8(:,67), wf16(:,115), A(:,345), n3(:,613), t3x128(:,:,345), nhel, den(554))
    call cont_QA(nsync, wf8(:,67), wf16(:,116), A(:,346), n3(:,614), t3x128(:,:,346), nhel, den(555))
    call cont_QA(nsync, wf16(:,42), wf8(:,126), A(:,347), n3(:,615), t3x128(:,:,347), nhel, den(556))
    call cont_QA(nsync, wf16(:,44), wf8(:,126), A(:,348), n3(:,616), t3x128(:,:,348), nhel, den(557))
    call cont_QA(nsync, wf16(:,75), wf8(:,127), A(:,349), n3(:,617), t3x128(:,:,349), nhel, den(558))
    call cont_QA(nsync, wf16(:,77), wf8(:,127), A(:,350), n3(:,618), t3x128(:,:,350), nhel, den(559))
    call cont_QA(nsync, wf16(:,75), wf8(:,128), A(:,351), n3(:,619), t3x128(:,:,351), nhel, den(560))
    call cont_QA(nsync, wf16(:,77), wf8(:,128), A(:,352), n3(:,620), t3x128(:,:,352), nhel, den(561))
    call cont_VV(nsync, wf4(:,14), wf32(:,49), A(:,353), n3(:,621), t3x128(:,:,353), nhel, den(563))
    call cont_VV(nsync, wf4(:,14), wf32(:,50), A(:,354), n3(:,622), t3x128(:,:,354), nhel, den(565))
    call cont_QA(nsync, wf8(:,99), wf16(:,128), A(:,355), n3(:,623), t3x128(:,:,355), nhel, den(567))
    call cont_QA(nsync, wf8(:,100), wf16(:,128), A(:,356), n3(:,624), t3x128(:,:,356), nhel, den(568))
    call cont_QA(nsync, wf8(:,101), wf16(:,128), A(:,357), n3(:,625), t3x128(:,:,357), nhel, den(569))
    call cont_VV(nsync, wf4(:,5), wf32(:,51), A(:,358), n3(:,626), t3x128(:,:,358), nhel, den(571))
    call cont_QA(nsync, wf32(:,19), wf4(:,40), A(:,359), n3(:,627), t3x128(:,:,359), nhel, den(572))
    call cont_QA(nsync, wf32(:,20), wf4(:,40), A(:,360), n3(:,628), t3x128(:,:,360), nhel, den(573))
    call cont_QA(nsync, wf16(:,75), wf8(:,129), A(:,361), n3(:,629), t3x128(:,:,361), nhel, den(574))
    call cont_QA(nsync, wf16(:,77), wf8(:,129), A(:,362), n3(:,630), t3x128(:,:,362), nhel, den(575))
    call cont_QA(nsync, wf16(:,63), wf8(:,109), A(:,363), n3(:,631), t3x128(:,:,363), nhel, den(576))
    call cont_QA(nsync, wf16(:,64), wf8(:,109), A(:,364), n3(:,632), t3x128(:,:,364), nhel, den(577))
    call cont_QA(nsync, wf16(:,45), wf8(:,130), A(:,365), n3(:,633), t3x128(:,:,365), nhel, den(579))
    call cont_QA(nsync, wf16(:,46), wf8(:,130), A(:,366), n3(:,634), t3x128(:,:,366), nhel, den(580))
    call cont_QA(nsync, wf16(:,79), wf8(:,104), A(:,367), n3(:,635), t3x128(:,:,367), nhel, den(581))
    call cont_QA(nsync, wf16(:,79), wf8(:,105), A(:,368), n3(:,636), t3x128(:,:,368), nhel, den(582))
    call cont_QA(nsync, wf16(:,45), wf8(:,131), A(:,369), n3(:,637), t3x128(:,:,369), nhel, den(584))
    call cont_QA(nsync, wf16(:,46), wf8(:,131), A(:,370), n3(:,638), t3x128(:,:,370), nhel, den(585))
    call cont_QA(nsync, wf16(:,82), wf8(:,104), A(:,371), n3(:,639), t3x128(:,:,371), nhel, den(586))
    call cont_QA(nsync, wf16(:,82), wf8(:,105), A(:,372), n3(:,640), t3x128(:,:,372), nhel, den(587))
    call cont_QA(nsync, wf16(:,57), wf8(:,132), A(:,373), n3(:,641), t3x128(:,:,373), nhel, den(589))
    call cont_QA(nsync, wf16(:,58), wf8(:,132), A(:,374), n3(:,642), t3x128(:,:,374), nhel, den(590))
    call cont_QA(nsync, wf16(:,79), wf8(:,108), A(:,375), n3(:,643), t3x128(:,:,375), nhel, den(591))
    call cont_QA(nsync, wf16(:,82), wf8(:,108), A(:,376), n3(:,644), t3x128(:,:,376), nhel, den(592))
    call cont_QA(nsync, wf8(:,77), wf16(:,108), A(:,377), n3(:,645), t3x128(:,:,377), nhel, den(593))
    call cont_QA(nsync, wf8(:,32), wf16(:,124), A(:,378), n3(:,646), t3x128(:,:,378), nhel, den(594))
    call cont_QA(nsync, wf8(:,32), wf16(:,125), A(:,379), n3(:,647), t3x128(:,:,379), nhel, den(595))
    call cont_QA(nsync, wf8(:,32), wf16(:,126), A(:,380), n3(:,648), t3x128(:,:,380), nhel, den(596))
    call cont_QA(nsync, wf16(:,33), wf8(:,133), A(:,381), n3(:,649), t3x128(:,:,381), nhel, den(597))
    call cont_QA(nsync, wf16(:,35), wf8(:,133), A(:,382), n3(:,650), t3x128(:,:,382), nhel, den(598))
    call cont_QA(nsync, wf8(:,77), wf16(:,112), A(:,383), n3(:,651), t3x128(:,:,383), nhel, den(599))
    call cont_QA(nsync, wf8(:,77), wf16(:,113), A(:,384), n3(:,652), t3x128(:,:,384), nhel, den(600))
    call cont_QA(nsync, wf16(:,33), wf8(:,134), A(:,385), n3(:,653), t3x128(:,:,385), nhel, den(601))
    call cont_QA(nsync, wf16(:,35), wf8(:,134), A(:,386), n3(:,654), t3x128(:,:,386), nhel, den(602))
    call cont_QA(nsync, wf16(:,84), wf8(:,135), A(:,387), n3(:,655), t3x128(:,:,387), nhel, den(603))
    call cont_QA(nsync, wf16(:,86), wf8(:,135), A(:,388), n3(:,656), t3x128(:,:,388), nhel, den(604))
    call cont_QA(nsync, wf16(:,84), wf8(:,136), A(:,389), n3(:,657), t3x128(:,:,389), nhel, den(605))
    call cont_QA(nsync, wf16(:,86), wf8(:,136), A(:,390), n3(:,658), t3x128(:,:,390), nhel, den(606))
    call cont_QA(nsync, wf8(:,16), wf16(:,129), A(:,391), n3(:,659), t3x128(:,:,391), nhel, den(608))
    call cont_QA(nsync, wf8(:,17), wf16(:,129), A(:,392), n3(:,660), t3x128(:,:,392), nhel, den(609))
    call cont_VV(nsync, wf4(:,14), wf32(:,52), A(:,393), n3(:,661), t3x128(:,:,393), nhel, den(611))
    call cont_VV(nsync, wf4(:,14), wf32(:,53), A(:,394), n3(:,662), t3x128(:,:,394), nhel, den(613))
    call cont_QA(nsync, wf8(:,117), wf16(:,129), A(:,395), n3(:,663), t3x128(:,:,395), nhel, den(614))
    call cont_VV(nsync, wf4(:,5), wf32(:,54), A(:,396), n3(:,664), t3x128(:,:,396), nhel, den(616))
    call cont_QA(nsync, wf16(:,33), wf8(:,137), A(:,397), n3(:,665), t3x128(:,:,397), nhel, den(617))
    call cont_QA(nsync, wf16(:,35), wf8(:,137), A(:,398), n3(:,666), t3x128(:,:,398), nhel, den(618))
    call cont_QA(nsync, wf32(:,37), wf4(:,36), A(:,399), n3(:,667), t3x128(:,:,399), nhel, den(619))
    call cont_QA(nsync, wf32(:,38), wf4(:,36), A(:,400), n3(:,668), t3x128(:,:,400), nhel, den(620))
    call cont_QA(nsync, wf16(:,38), wf8(:,119), A(:,401), n3(:,669), t3x128(:,:,401), nhel, den(621))
    call cont_QA(nsync, wf16(:,38), wf8(:,120), A(:,402), n3(:,670), t3x128(:,:,402), nhel, den(622))
    call cont_QA(nsync, wf16(:,39), wf8(:,119), A(:,403), n3(:,671), t3x128(:,:,403), nhel, den(623))
    call cont_QA(nsync, wf16(:,39), wf8(:,120), A(:,404), n3(:,672), t3x128(:,:,404), nhel, den(624))
    call cont_QA(nsync, wf16(:,104), wf8(:,93), A(:,405), n3(:,673), t3x128(:,:,405), nhel, den(625))
    call cont_QA(nsync, wf16(:,105), wf8(:,93), A(:,406), n3(:,674), t3x128(:,:,406), nhel, den(626))
    call cont_QA(nsync, wf16(:,100), wf8(:,138), A(:,407), n3(:,675), t3x128(:,:,407), nhel, den(628))
    call cont_QA(nsync, wf16(:,101), wf8(:,138), A(:,408), n3(:,676), t3x128(:,:,408), nhel, den(629))
    call cont_QA(nsync, wf16(:,88), wf8(:,139), A(:,409), n3(:,677), t3x128(:,:,409), nhel, den(631))
    call cont_QA(nsync, wf16(:,89), wf8(:,139), A(:,410), n3(:,678), t3x128(:,:,410), nhel, den(632))
    call cont_QA(nsync, wf16(:,88), wf8(:,140), A(:,411), n3(:,679), t3x128(:,:,411), nhel, den(634))
    call cont_QA(nsync, wf16(:,89), wf8(:,140), A(:,412), n3(:,680), t3x128(:,:,412), nhel, den(635))
    call cont_QA(nsync, wf16(:,39), wf8(:,124), A(:,413), n3(:,681), t3x128(:,:,413), nhel, den(636))
    call cont_QA(nsync, wf16(:,38), wf8(:,124), A(:,414), n3(:,682), t3x128(:,:,414), nhel, den(637))
    call cont_VV(nsync, wf4(:,19), wf32(:,55), A(:,415), n3(:,683), t3x128(:,:,415), nhel, den(639))
    call cont_VV(nsync, wf4(:,19), wf32(:,56), A(:,416), n3(:,684), t3x128(:,:,416), nhel, den(641))
    call cont_QA(nsync, wf8(:,99), wf16(:,130), A(:,417), n3(:,685), t3x128(:,:,417), nhel, den(643))
    call cont_QA(nsync, wf8(:,100), wf16(:,130), A(:,418), n3(:,686), t3x128(:,:,418), nhel, den(644))
    call cont_QA(nsync, wf8(:,101), wf16(:,130), A(:,419), n3(:,687), t3x128(:,:,419), nhel, den(645))
    call cont_VV(nsync, wf4(:,5), wf32(:,57), A(:,420), n3(:,688), t3x128(:,:,420), nhel, den(647))
    call cont_QA(nsync, wf32(:,13), wf4(:,44), A(:,421), n3(:,689), t3x128(:,:,421), nhel, den(648))
    call cont_QA(nsync, wf32(:,14), wf4(:,44), A(:,422), n3(:,690), t3x128(:,:,422), nhel, den(649))
    call cont_QA(nsync, wf16(:,84), wf8(:,141), A(:,423), n3(:,691), t3x128(:,:,423), nhel, den(650))
    call cont_QA(nsync, wf16(:,86), wf8(:,141), A(:,424), n3(:,692), t3x128(:,:,424), nhel, den(651))
    call cont_QA(nsync, wf8(:,16), wf16(:,131), A(:,425), n3(:,693), t3x128(:,:,425), nhel, den(653))
    call cont_QA(nsync, wf8(:,17), wf16(:,131), A(:,426), n3(:,694), t3x128(:,:,426), nhel, den(654))
    call cont_VV(nsync, wf4(:,19), wf32(:,58), A(:,427), n3(:,695), t3x128(:,:,427), nhel, den(656))
    call cont_VV(nsync, wf4(:,19), wf32(:,59), A(:,428), n3(:,696), t3x128(:,:,428), nhel, den(658))
    call cont_QA(nsync, wf8(:,117), wf16(:,131), A(:,429), n3(:,697), t3x128(:,:,429), nhel, den(659))
    call cont_VV(nsync, wf4(:,5), wf32(:,60), A(:,430), n3(:,698), t3x128(:,:,430), nhel, den(661))
    call cont_QA(nsync, wf16(:,42), wf8(:,142), A(:,431), n3(:,699), t3x128(:,:,431), nhel, den(662))
    call cont_QA(nsync, wf16(:,44), wf8(:,142), A(:,432), n3(:,700), t3x128(:,:,432), nhel, den(663))
    call cont_QA(nsync, wf32(:,31), wf4(:,38), A(:,433), n3(:,701), t3x128(:,:,433), nhel, den(664))
    call cont_QA(nsync, wf32(:,32), wf4(:,38), A(:,434), n3(:,702), t3x128(:,:,434), nhel, den(665))
    call cont_QA(nsync, wf8(:,16), wf16(:,132), A(:,435), n3(:,703), t3x128(:,:,435), nhel, den(667))
    call cont_QA(nsync, wf8(:,17), wf16(:,132), A(:,436), n3(:,704), t3x128(:,:,436), nhel, den(668))
    call cont_QA(nsync, wf16(:,49), wf8(:,119), A(:,437), n3(:,705), t3x128(:,:,437), nhel, den(669))
    call cont_QA(nsync, wf16(:,49), wf8(:,120), A(:,438), n3(:,706), t3x128(:,:,438), nhel, den(670))
    call cont_QA(nsync, wf16(:,88), wf8(:,143), A(:,439), n3(:,707), t3x128(:,:,439), nhel, den(672))
    call cont_QA(nsync, wf16(:,89), wf8(:,143), A(:,440), n3(:,708), t3x128(:,:,440), nhel, den(673))
    call cont_QA(nsync, wf16(:,45), wf8(:,144), A(:,441), n3(:,709), t3x128(:,:,441), nhel, den(675))
    call cont_QA(nsync, wf16(:,46), wf8(:,144), A(:,442), n3(:,710), t3x128(:,:,442), nhel, den(676))
    call cont_QA(nsync, wf8(:,99), wf16(:,133), A(:,443), n3(:,711), t3x128(:,:,443), nhel, den(678))
    call cont_QA(nsync, wf8(:,100), wf16(:,133), A(:,444), n3(:,712), t3x128(:,:,444), nhel, den(679))
    call cont_QA(nsync, wf16(:,92), wf8(:,104), A(:,445), n3(:,713), t3x128(:,:,445), nhel, den(680))
    call cont_QA(nsync, wf16(:,92), wf8(:,105), A(:,446), n3(:,714), t3x128(:,:,446), nhel, den(681))
    call cont_QA(nsync, wf16(:,50), wf8(:,124), A(:,447), n3(:,715), t3x128(:,:,447), nhel, den(682))
    call cont_QA(nsync, wf8(:,101), wf16(:,133), A(:,448), n3(:,716), t3x128(:,:,448), nhel, den(683))
    call cont_QA(nsync, wf16(:,49), wf8(:,124), A(:,449), n3(:,717), t3x128(:,:,449), nhel, den(684))
    call cont_QA(nsync, wf16(:,92), wf8(:,108), A(:,450), n3(:,718), t3x128(:,:,450), nhel, den(685))
    call cont_QA(nsync, wf16(:,63), wf8(:,113), A(:,451), n3(:,719), t3x128(:,:,451), nhel, den(686))
    call cont_QA(nsync, wf16(:,64), wf8(:,113), A(:,452), n3(:,720), t3x128(:,:,452), nhel, den(687))
    call cont_QA(nsync, wf16(:,53), wf8(:,145), A(:,453), n3(:,721), t3x128(:,:,453), nhel, den(689))
    call cont_QA(nsync, wf16(:,54), wf8(:,145), A(:,454), n3(:,722), t3x128(:,:,454), nhel, den(690))
    call cont_QA(nsync, wf16(:,94), wf8(:,104), A(:,455), n3(:,723), t3x128(:,:,455), nhel, den(691))
    call cont_QA(nsync, wf16(:,94), wf8(:,105), A(:,456), n3(:,724), t3x128(:,:,456), nhel, den(692))
    call cont_QA(nsync, wf16(:,53), wf8(:,146), A(:,457), n3(:,725), t3x128(:,:,457), nhel, den(694))
    call cont_QA(nsync, wf16(:,54), wf8(:,146), A(:,458), n3(:,726), t3x128(:,:,458), nhel, den(695))
    call cont_QA(nsync, wf16(:,95), wf8(:,104), A(:,459), n3(:,727), t3x128(:,:,459), nhel, den(696))
    call cont_QA(nsync, wf16(:,95), wf8(:,105), A(:,460), n3(:,728), t3x128(:,:,460), nhel, den(697))
    call cont_QA(nsync, wf16(:,57), wf8(:,147), A(:,461), n3(:,729), t3x128(:,:,461), nhel, den(699))
    call cont_QA(nsync, wf16(:,58), wf8(:,147), A(:,462), n3(:,730), t3x128(:,:,462), nhel, den(700))
    call cont_QA(nsync, wf16(:,94), wf8(:,108), A(:,463), n3(:,731), t3x128(:,:,463), nhel, den(701))
    call cont_QA(nsync, wf16(:,95), wf8(:,108), A(:,464), n3(:,732), t3x128(:,:,464), nhel, den(702))
    call cont_QA(nsync, wf16(:,51), wf8(:,119), A(:,465), n3(:,733), t3x128(:,:,465), nhel, den(703))
    call cont_QA(nsync, wf16(:,51), wf8(:,120), A(:,466), n3(:,734), t3x128(:,:,466), nhel, den(704))
    call cont_QA(nsync, wf16(:,52), wf8(:,119), A(:,467), n3(:,735), t3x128(:,:,467), nhel, den(705))
    call cont_QA(nsync, wf16(:,52), wf8(:,120), A(:,468), n3(:,736), t3x128(:,:,468), nhel, den(706))
    call cont_QA(nsync, wf16(:,104), wf8(:,97), A(:,469), n3(:,737), t3x128(:,:,469), nhel, den(707))
    call cont_QA(nsync, wf16(:,105), wf8(:,97), A(:,470), n3(:,738), t3x128(:,:,470), nhel, den(708))
    call cont_QA(nsync, wf16(:,100), wf8(:,148), A(:,471), n3(:,739), t3x128(:,:,471), nhel, den(710))
    call cont_QA(nsync, wf16(:,101), wf8(:,148), A(:,472), n3(:,740), t3x128(:,:,472), nhel, den(711))
    call cont_QA(nsync, wf16(:,96), wf8(:,149), A(:,473), n3(:,741), t3x128(:,:,473), nhel, den(713))
    call cont_QA(nsync, wf16(:,97), wf8(:,149), A(:,474), n3(:,742), t3x128(:,:,474), nhel, den(714))
    call cont_QA(nsync, wf16(:,96), wf8(:,150), A(:,475), n3(:,743), t3x128(:,:,475), nhel, den(716))
    call cont_QA(nsync, wf16(:,97), wf8(:,150), A(:,476), n3(:,744), t3x128(:,:,476), nhel, den(717))
    call cont_QA(nsync, wf16(:,52), wf8(:,124), A(:,477), n3(:,745), t3x128(:,:,477), nhel, den(718))
    call cont_QA(nsync, wf16(:,51), wf8(:,124), A(:,478), n3(:,746), t3x128(:,:,478), nhel, den(719))
    call cont_QA(nsync, wf8(:,16), wf16(:,134), A(:,479), n3(:,747), t3x128(:,:,479), nhel, den(721))
    call cont_QA(nsync, wf8(:,17), wf16(:,134), A(:,480), n3(:,748), t3x128(:,:,480), nhel, den(722))
    call cont_QA(nsync, wf16(:,55), wf8(:,119), A(:,481), n3(:,749), t3x128(:,:,481), nhel, den(723))
    call cont_QA(nsync, wf16(:,55), wf8(:,120), A(:,482), n3(:,750), t3x128(:,:,482), nhel, den(724))
    call cont_QA(nsync, wf16(:,96), wf8(:,151), A(:,483), n3(:,751), t3x128(:,:,483), nhel, den(726))
    call cont_QA(nsync, wf16(:,97), wf8(:,151), A(:,484), n3(:,752), t3x128(:,:,484), nhel, den(727))
    call cont_QA(nsync, wf16(:,53), wf8(:,152), A(:,485), n3(:,753), t3x128(:,:,485), nhel, den(729))
    call cont_QA(nsync, wf16(:,54), wf8(:,152), A(:,486), n3(:,754), t3x128(:,:,486), nhel, den(730))
    call cont_QA(nsync, wf8(:,99), wf16(:,135), A(:,487), n3(:,755), t3x128(:,:,487), nhel, den(732))
    call cont_QA(nsync, wf8(:,100), wf16(:,135), A(:,488), n3(:,756), t3x128(:,:,488), nhel, den(733))
    call cont_QA(nsync, wf16(:,98), wf8(:,104), A(:,489), n3(:,757), t3x128(:,:,489), nhel, den(734))
    call cont_QA(nsync, wf16(:,98), wf8(:,105), A(:,490), n3(:,758), t3x128(:,:,490), nhel, den(735))
    call cont_QA(nsync, wf16(:,56), wf8(:,124), A(:,491), n3(:,759), t3x128(:,:,491), nhel, den(736))
    call cont_QA(nsync, wf8(:,101), wf16(:,135), A(:,492), n3(:,760), t3x128(:,:,492), nhel, den(737))
    call cont_QA(nsync, wf16(:,55), wf8(:,124), A(:,493), n3(:,761), t3x128(:,:,493), nhel, den(738))
    call cont_QA(nsync, wf16(:,98), wf8(:,108), A(:,494), n3(:,762), t3x128(:,:,494), nhel, den(739))
    call cont_QA(nsync, wf8(:,16), wf16(:,136), A(:,495), n3(:,763), t3x128(:,:,495), nhel, den(741))
    call cont_QA(nsync, wf8(:,17), wf16(:,136), A(:,496), n3(:,764), t3x128(:,:,496), nhel, den(742))
    call cont_QA(nsync, wf16(:,59), wf8(:,119), A(:,497), n3(:,765), t3x128(:,:,497), nhel, den(743))
    call cont_QA(nsync, wf16(:,59), wf8(:,120), A(:,498), n3(:,766), t3x128(:,:,498), nhel, den(744))
    call cont_QA(nsync, wf16(:,100), wf8(:,153), A(:,499), n3(:,767), t3x128(:,:,499), nhel, den(746))
    call cont_QA(nsync, wf16(:,101), wf8(:,153), A(:,500), n3(:,768), t3x128(:,:,500), nhel, den(747))
    call cont_QA(nsync, wf8(:,99), wf16(:,137), A(:,501), n3(:,769), t3x128(:,:,501), nhel, den(749))
    call cont_QA(nsync, wf8(:,100), wf16(:,137), A(:,502), n3(:,770), t3x128(:,:,502), nhel, den(750))
    call cont_QA(nsync, wf16(:,57), wf8(:,154), A(:,503), n3(:,771), t3x128(:,:,503), nhel, den(752))
    call cont_QA(nsync, wf16(:,58), wf8(:,154), A(:,504), n3(:,772), t3x128(:,:,504), nhel, den(753))
    call cont_QA(nsync, wf16(:,102), wf8(:,104), A(:,505), n3(:,773), t3x128(:,:,505), nhel, den(754))
    call cont_QA(nsync, wf16(:,102), wf8(:,105), A(:,506), n3(:,774), t3x128(:,:,506), nhel, den(755))
    call cont_QA(nsync, wf16(:,60), wf8(:,124), A(:,507), n3(:,775), t3x128(:,:,507), nhel, den(756))
    call cont_QA(nsync, wf8(:,101), wf16(:,137), A(:,508), n3(:,776), t3x128(:,:,508), nhel, den(757))
    call cont_QA(nsync, wf16(:,59), wf8(:,124), A(:,509), n3(:,777), t3x128(:,:,509), nhel, den(758))
    call cont_QA(nsync, wf16(:,102), wf8(:,108), A(:,510), n3(:,778), t3x128(:,:,510), nhel, den(759))

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

  M1(1) = (A(j,37)%j-A(j,38)%j+A(j,40)%j-A(j,41)%j+A(j,301)%j+A(j,302)%j+A(j,338)%j+A(j,447)%j+A(j,448)%j+A(j,464)%j+A(j,507)%j &
       +A(j,508)%j)*f(1)+CI*(A(j,319)%j+A(j,320)%j+A(j,419)%j+A(j,420)%j+A(j,449)%j+A(j,510)%j)*f(2)+(A(j,1)%j-A(j,2)%j+A(j,7)%j &
       -A(j,8)%j+A(j,13)%j-A(j,14)%j+A(j,19)%j-A(j,20)%j+A(j,25)%j-A(j,26)%j+A(j,31)%j-A(j,32)%j+A(j,51)%j+A(j,53)%j+A(j,55)%j &
       +A(j,83)%j+A(j,89)%j+A(j,119)%j+A(j,125)%j+A(j,135)%j+A(j,179)%j+A(j,185)%j+A(j,191)%j+A(j,193)%j+A(j,195)%j+A(j,215)%j &
       +A(j,219)%j+A(j,231)%j+A(j,303)%j+A(j,305)%j+A(j,307)%j+A(j,309)%j+A(j,311)%j+A(j,313)%j+A(j,325)%j+A(j,333)%j+A(j,335)%j &
       +A(j,435)%j+A(j,443)%j+A(j,451)%j+A(j,459)%j+A(j,461)%j+A(j,495)%j+A(j,501)%j)*f(3)+(A(j,4)%j-A(j,5)%j+A(j,10)%j-A(j,11)%j &
       +A(j,16)%j-A(j,17)%j+A(j,22)%j-A(j,23)%j+A(j,28)%j-A(j,29)%j+A(j,34)%j-A(j,35)%j+A(j,52)%j+A(j,54)%j+A(j,56)%j+A(j,84)%j &
       +A(j,90)%j+A(j,120)%j+A(j,126)%j+A(j,136)%j+A(j,180)%j+A(j,186)%j+A(j,192)%j+A(j,194)%j+A(j,196)%j+A(j,216)%j+A(j,220)%j &
       +A(j,232)%j+A(j,304)%j+A(j,306)%j+A(j,308)%j+A(j,310)%j+A(j,312)%j+A(j,314)%j+A(j,326)%j+A(j,334)%j+A(j,336)%j+A(j,436)%j &
       +A(j,444)%j+A(j,452)%j+A(j,460)%j+A(j,462)%j+A(j,496)%j+A(j,502)%j)*f(4)+CI*(A(j,43)%j+A(j,45)%j+A(j,79)%j+A(j,85)%j &
       +A(j,87)%j+A(j,117)%j+A(j,171)%j+A(j,173)%j+A(j,175)%j+A(j,213)%j+A(j,217)%j+A(j,221)%j+A(j,315)%j+A(j,317)%j+A(j,321)%j &
       +A(j,323)%j+A(j,415)%j+A(j,417)%j+A(j,421)%j+A(j,423)%j+A(j,437)%j+A(j,439)%j+A(j,503)%j+A(j,505)%j)*f(5)+CI*(A(j,44)%j &
       +A(j,46)%j+A(j,80)%j+A(j,86)%j+A(j,88)%j+A(j,118)%j+A(j,172)%j+A(j,174)%j+A(j,176)%j+A(j,214)%j+A(j,218)%j+A(j,222)%j &
       +A(j,316)%j+A(j,318)%j+A(j,322)%j+A(j,324)%j+A(j,416)%j+A(j,418)%j+A(j,422)%j+A(j,424)%j+A(j,438)%j+A(j,440)%j+A(j,504)%j &
       +A(j,506)%j)*f(6)
  M1(2) = (A(j,38)%j-A(j,39)%j+A(j,41)%j-A(j,42)%j+A(j,287)%j+A(j,288)%j+A(j,337)%j+A(j,376)%j-A(j,491)%j-A(j,492)%j-A(j,507)%j &
       -A(j,508)%j)*f(1)+CI*(-A(j,319)%j-A(j,320)%j+A(j,357)%j+A(j,358)%j+A(j,493)%j-A(j,510)%j)*f(2)+(A(j,2)%j-A(j,3)%j+A(j,8)%j &
       -A(j,9)%j+A(j,14)%j-A(j,15)%j+A(j,20)%j-A(j,21)%j+A(j,26)%j-A(j,27)%j+A(j,32)%j-A(j,33)%j+A(j,47)%j+A(j,49)%j+A(j,57)%j &
       -A(j,107)%j-A(j,113)%j-A(j,119)%j-A(j,125)%j+A(j,131)%j+A(j,163)%j+A(j,165)%j+A(j,167)%j-A(j,203)%j-A(j,209)%j-A(j,215)%j &
       -A(j,219)%j+A(j,227)%j+A(j,289)%j+A(j,291)%j+A(j,293)%j+A(j,295)%j+A(j,297)%j+A(j,299)%j+A(j,327)%j+A(j,329)%j+A(j,331)%j &
       +A(j,363)%j+A(j,371)%j+A(j,373)%j-A(j,479)%j-A(j,487)%j-A(j,495)%j-A(j,501)%j)*f(3)+(A(j,5)%j-A(j,6)%j+A(j,11)%j-A(j,12)%j &
       +A(j,17)%j-A(j,18)%j+A(j,23)%j-A(j,24)%j+A(j,29)%j-A(j,30)%j+A(j,35)%j-A(j,36)%j+A(j,48)%j+A(j,50)%j+A(j,58)%j-A(j,108)%j &
       -A(j,114)%j-A(j,120)%j-A(j,126)%j+A(j,132)%j+A(j,164)%j+A(j,166)%j+A(j,168)%j-A(j,204)%j-A(j,210)%j-A(j,216)%j-A(j,220)%j &
       +A(j,228)%j+A(j,290)%j+A(j,292)%j+A(j,294)%j+A(j,296)%j+A(j,298)%j+A(j,300)%j+A(j,328)%j+A(j,330)%j+A(j,332)%j+A(j,364)%j &
       +A(j,372)%j+A(j,374)%j-A(j,480)%j-A(j,488)%j-A(j,496)%j-A(j,502)%j)*f(4)+CI*(-A(j,43)%j-A(j,45)%j+A(j,103)%j+A(j,109)%j &
       +A(j,111)%j-A(j,117)%j+A(j,155)%j+A(j,157)%j+A(j,199)%j-A(j,213)%j-A(j,217)%j-A(j,221)%j-A(j,315)%j-A(j,317)%j-A(j,321)%j &
       -A(j,323)%j+A(j,353)%j+A(j,355)%j+A(j,359)%j+A(j,361)%j+A(j,481)%j+A(j,483)%j-A(j,503)%j-A(j,505)%j)*f(5)+CI*(-A(j,44)%j &
       -A(j,46)%j+A(j,104)%j+A(j,110)%j+A(j,112)%j-A(j,118)%j+A(j,156)%j+A(j,158)%j+A(j,200)%j-A(j,214)%j-A(j,218)%j-A(j,222)%j &
       -A(j,316)%j-A(j,318)%j-A(j,322)%j-A(j,324)%j+A(j,354)%j+A(j,356)%j+A(j,360)%j+A(j,362)%j+A(j,482)%j+A(j,484)%j-A(j,504)%j &
       -A(j,506)%j)*f(6)
  M1(3) = (-A(j,37)%j+A(j,39)%j-A(j,40)%j+A(j,42)%j+A(j,377)%j+A(j,378)%j+A(j,414)%j-A(j,447)%j-A(j,448)%j+A(j,463)%j+A(j,491)%j &
       +A(j,492)%j)*f(1)+CI*(A(j,395)%j+A(j,396)%j-A(j,419)%j-A(j,420)%j-A(j,449)%j+A(j,494)%j)*f(2)+(-A(j,1)%j+A(j,3)%j-A(j,7)%j &
       +A(j,9)%j-A(j,13)%j+A(j,15)%j-A(j,19)%j+A(j,21)%j-A(j,25)%j+A(j,27)%j-A(j,31)%j+A(j,33)%j+A(j,67)%j+A(j,69)%j+A(j,71)%j &
       -A(j,83)%j-A(j,89)%j+A(j,107)%j+A(j,113)%j+A(j,137)%j-A(j,179)%j-A(j,185)%j+A(j,187)%j+A(j,189)%j+A(j,197)%j+A(j,203)%j &
       +A(j,209)%j+A(j,233)%j+A(j,379)%j+A(j,381)%j+A(j,383)%j+A(j,385)%j+A(j,387)%j+A(j,389)%j+A(j,401)%j+A(j,409)%j+A(j,411)%j &
       -A(j,435)%j-A(j,443)%j+A(j,453)%j+A(j,455)%j+A(j,457)%j+A(j,479)%j+A(j,487)%j)*f(3)+(-A(j,4)%j+A(j,6)%j-A(j,10)%j+A(j,12)%j &
       -A(j,16)%j+A(j,18)%j-A(j,22)%j+A(j,24)%j-A(j,28)%j+A(j,30)%j-A(j,34)%j+A(j,36)%j+A(j,68)%j+A(j,70)%j+A(j,72)%j-A(j,84)%j &
       -A(j,90)%j+A(j,108)%j+A(j,114)%j+A(j,138)%j-A(j,180)%j-A(j,186)%j+A(j,188)%j+A(j,190)%j+A(j,198)%j+A(j,204)%j+A(j,210)%j &
       +A(j,234)%j+A(j,380)%j+A(j,382)%j+A(j,384)%j+A(j,386)%j+A(j,388)%j+A(j,390)%j+A(j,402)%j+A(j,410)%j+A(j,412)%j-A(j,436)%j &
       -A(j,444)%j+A(j,454)%j+A(j,456)%j+A(j,458)%j+A(j,480)%j+A(j,488)%j)*f(4)+CI*(A(j,59)%j+A(j,61)%j-A(j,79)%j-A(j,85)%j &
       -A(j,87)%j+A(j,105)%j-A(j,171)%j-A(j,173)%j-A(j,175)%j+A(j,201)%j+A(j,205)%j+A(j,207)%j+A(j,391)%j+A(j,393)%j+A(j,397)%j &
       +A(j,399)%j-A(j,415)%j-A(j,417)%j-A(j,421)%j-A(j,423)%j-A(j,437)%j-A(j,439)%j+A(j,485)%j+A(j,489)%j)*f(5)+CI*(A(j,60)%j &
       +A(j,62)%j-A(j,80)%j-A(j,86)%j-A(j,88)%j+A(j,106)%j-A(j,172)%j-A(j,174)%j-A(j,176)%j+A(j,202)%j+A(j,206)%j+A(j,208)%j &
       +A(j,392)%j+A(j,394)%j+A(j,398)%j+A(j,400)%j-A(j,416)%j-A(j,418)%j-A(j,422)%j-A(j,424)%j-A(j,438)%j-A(j,440)%j+A(j,486)%j &
       +A(j,490)%j)*f(6)
  M1(4) = (A(j,38)%j-A(j,39)%j+A(j,41)%j-A(j,42)%j+A(j,235)%j+A(j,236)%j+A(j,286)%j+A(j,413)%j-A(j,491)%j-A(j,492)%j-A(j,507)%j &
       -A(j,508)%j)*f(1)+CI*(A(j,267)%j+A(j,268)%j-A(j,395)%j-A(j,396)%j-A(j,494)%j+A(j,509)%j)*f(2)+(A(j,2)%j-A(j,3)%j+A(j,8)%j &
       -A(j,9)%j+A(j,14)%j-A(j,15)%j+A(j,20)%j-A(j,21)%j+A(j,26)%j-A(j,27)%j+A(j,32)%j-A(j,33)%j+A(j,63)%j+A(j,65)%j+A(j,73)%j &
       -A(j,107)%j-A(j,113)%j-A(j,119)%j-A(j,125)%j+A(j,127)%j+A(j,147)%j+A(j,149)%j+A(j,151)%j-A(j,203)%j-A(j,209)%j-A(j,215)%j &
       -A(j,219)%j+A(j,223)%j+A(j,237)%j+A(j,239)%j+A(j,241)%j+A(j,243)%j+A(j,245)%j+A(j,247)%j+A(j,273)%j+A(j,281)%j+A(j,283)%j &
       +A(j,403)%j+A(j,405)%j+A(j,407)%j-A(j,479)%j-A(j,487)%j-A(j,495)%j-A(j,501)%j)*f(3)+(A(j,5)%j-A(j,6)%j+A(j,11)%j-A(j,12)%j &
       +A(j,17)%j-A(j,18)%j+A(j,23)%j-A(j,24)%j+A(j,29)%j-A(j,30)%j+A(j,35)%j-A(j,36)%j+A(j,64)%j+A(j,66)%j+A(j,74)%j-A(j,108)%j &
       -A(j,114)%j-A(j,120)%j-A(j,126)%j+A(j,128)%j+A(j,148)%j+A(j,150)%j+A(j,152)%j-A(j,204)%j-A(j,210)%j-A(j,216)%j-A(j,220)%j &
       +A(j,224)%j+A(j,238)%j+A(j,240)%j+A(j,242)%j+A(j,244)%j+A(j,246)%j+A(j,248)%j+A(j,274)%j+A(j,282)%j+A(j,284)%j+A(j,404)%j &
       +A(j,406)%j+A(j,408)%j-A(j,480)%j-A(j,488)%j-A(j,496)%j-A(j,502)%j)*f(4)+CI*(-A(j,59)%j-A(j,61)%j-A(j,105)%j+A(j,115)%j &
       +A(j,121)%j+A(j,123)%j+A(j,139)%j+A(j,141)%j-A(j,201)%j-A(j,205)%j-A(j,207)%j+A(j,211)%j+A(j,263)%j+A(j,265)%j+A(j,269)%j &
       +A(j,271)%j-A(j,391)%j-A(j,393)%j-A(j,397)%j-A(j,399)%j-A(j,485)%j-A(j,489)%j+A(j,497)%j+A(j,499)%j)*f(5)+CI*(-A(j,60)%j &
       -A(j,62)%j-A(j,106)%j+A(j,116)%j+A(j,122)%j+A(j,124)%j+A(j,140)%j+A(j,142)%j-A(j,202)%j-A(j,206)%j-A(j,208)%j+A(j,212)%j &
       +A(j,264)%j+A(j,266)%j+A(j,270)%j+A(j,272)%j-A(j,392)%j-A(j,394)%j-A(j,398)%j-A(j,400)%j-A(j,486)%j-A(j,490)%j+A(j,498)%j &
       +A(j,500)%j)*f(6)
  M1(5) = (-A(j,37)%j+A(j,39)%j-A(j,40)%j+A(j,42)%j+A(j,339)%j+A(j,340)%j+A(j,375)%j-A(j,447)%j-A(j,448)%j+A(j,478)%j+A(j,491)%j &
       +A(j,492)%j)*f(1)+CI*(-A(j,357)%j-A(j,358)%j+A(j,429)%j+A(j,430)%j+A(j,450)%j-A(j,493)%j)*f(2)+(-A(j,1)%j+A(j,3)%j-A(j,7)%j &
       +A(j,9)%j-A(j,13)%j+A(j,15)%j-A(j,19)%j+A(j,21)%j-A(j,25)%j+A(j,27)%j-A(j,31)%j+A(j,33)%j-A(j,83)%j-A(j,89)%j+A(j,95)%j &
       +A(j,97)%j+A(j,99)%j+A(j,107)%j+A(j,113)%j+A(j,133)%j+A(j,159)%j+A(j,161)%j+A(j,169)%j-A(j,179)%j-A(j,185)%j+A(j,203)%j &
       +A(j,209)%j+A(j,229)%j+A(j,341)%j+A(j,343)%j+A(j,345)%j+A(j,347)%j+A(j,349)%j+A(j,351)%j+A(j,365)%j+A(j,367)%j+A(j,369)%j &
       -A(j,435)%j-A(j,443)%j+A(j,465)%j+A(j,473)%j+A(j,475)%j+A(j,479)%j+A(j,487)%j)*f(3)+(-A(j,4)%j+A(j,6)%j-A(j,10)%j+A(j,12)%j &
       -A(j,16)%j+A(j,18)%j-A(j,22)%j+A(j,24)%j-A(j,28)%j+A(j,30)%j-A(j,34)%j+A(j,36)%j-A(j,84)%j-A(j,90)%j+A(j,96)%j+A(j,98)%j &
       +A(j,100)%j+A(j,108)%j+A(j,114)%j+A(j,134)%j+A(j,160)%j+A(j,162)%j+A(j,170)%j-A(j,180)%j-A(j,186)%j+A(j,204)%j+A(j,210)%j &
       +A(j,230)%j+A(j,342)%j+A(j,344)%j+A(j,346)%j+A(j,348)%j+A(j,350)%j+A(j,352)%j+A(j,366)%j+A(j,368)%j+A(j,370)%j-A(j,436)%j &
       -A(j,444)%j+A(j,466)%j+A(j,474)%j+A(j,476)%j+A(j,480)%j+A(j,488)%j)*f(4)+CI*(A(j,75)%j+A(j,77)%j+A(j,81)%j-A(j,103)%j &
       -A(j,109)%j-A(j,111)%j-A(j,155)%j-A(j,157)%j+A(j,177)%j+A(j,181)%j+A(j,183)%j-A(j,199)%j-A(j,353)%j-A(j,355)%j-A(j,359)%j &
       -A(j,361)%j+A(j,425)%j+A(j,427)%j+A(j,431)%j+A(j,433)%j+A(j,441)%j+A(j,445)%j-A(j,481)%j-A(j,483)%j)*f(5)+CI*(A(j,76)%j &
       +A(j,78)%j+A(j,82)%j-A(j,104)%j-A(j,110)%j-A(j,112)%j-A(j,156)%j-A(j,158)%j+A(j,178)%j+A(j,182)%j+A(j,184)%j-A(j,200)%j &
       -A(j,354)%j-A(j,356)%j-A(j,360)%j-A(j,362)%j+A(j,426)%j+A(j,428)%j+A(j,432)%j+A(j,434)%j+A(j,442)%j+A(j,446)%j-A(j,482)%j &
       -A(j,484)%j)*f(6)
  M1(6) = (A(j,37)%j-A(j,38)%j+A(j,40)%j-A(j,41)%j+A(j,249)%j+A(j,250)%j+A(j,285)%j+A(j,447)%j+A(j,448)%j+A(j,477)%j+A(j,507)%j &
       +A(j,508)%j)*f(1)+CI*(-A(j,267)%j-A(j,268)%j-A(j,429)%j-A(j,430)%j-A(j,450)%j-A(j,509)%j)*f(2)+(A(j,1)%j-A(j,2)%j+A(j,7)%j &
       -A(j,8)%j+A(j,13)%j-A(j,14)%j+A(j,19)%j-A(j,20)%j+A(j,25)%j-A(j,26)%j+A(j,31)%j-A(j,32)%j+A(j,83)%j+A(j,89)%j+A(j,91)%j &
       +A(j,93)%j+A(j,101)%j+A(j,119)%j+A(j,125)%j+A(j,129)%j+A(j,143)%j+A(j,145)%j+A(j,153)%j+A(j,179)%j+A(j,185)%j+A(j,215)%j &
       +A(j,219)%j+A(j,225)%j+A(j,251)%j+A(j,253)%j+A(j,255)%j+A(j,257)%j+A(j,259)%j+A(j,261)%j+A(j,275)%j+A(j,277)%j+A(j,279)%j &
       +A(j,435)%j+A(j,443)%j+A(j,467)%j+A(j,469)%j+A(j,471)%j+A(j,495)%j+A(j,501)%j)*f(3)+(A(j,4)%j-A(j,5)%j+A(j,10)%j-A(j,11)%j &
       +A(j,16)%j-A(j,17)%j+A(j,22)%j-A(j,23)%j+A(j,28)%j-A(j,29)%j+A(j,34)%j-A(j,35)%j+A(j,84)%j+A(j,90)%j+A(j,92)%j+A(j,94)%j &
       +A(j,102)%j+A(j,120)%j+A(j,126)%j+A(j,130)%j+A(j,144)%j+A(j,146)%j+A(j,154)%j+A(j,180)%j+A(j,186)%j+A(j,216)%j+A(j,220)%j &
       +A(j,226)%j+A(j,252)%j+A(j,254)%j+A(j,256)%j+A(j,258)%j+A(j,260)%j+A(j,262)%j+A(j,276)%j+A(j,278)%j+A(j,280)%j+A(j,436)%j &
       +A(j,444)%j+A(j,468)%j+A(j,470)%j+A(j,472)%j+A(j,496)%j+A(j,502)%j)*f(4)+CI*(-A(j,75)%j-A(j,77)%j-A(j,81)%j-A(j,115)%j &
       -A(j,121)%j-A(j,123)%j-A(j,139)%j-A(j,141)%j-A(j,177)%j-A(j,181)%j-A(j,183)%j-A(j,211)%j-A(j,263)%j-A(j,265)%j-A(j,269)%j &
       -A(j,271)%j-A(j,425)%j-A(j,427)%j-A(j,431)%j-A(j,433)%j-A(j,441)%j-A(j,445)%j-A(j,497)%j-A(j,499)%j)*f(5)+CI*(-A(j,76)%j &
       -A(j,78)%j-A(j,82)%j-A(j,116)%j-A(j,122)%j-A(j,124)%j-A(j,140)%j-A(j,142)%j-A(j,178)%j-A(j,182)%j-A(j,184)%j-A(j,212)%j &
       -A(j,264)%j-A(j,266)%j-A(j,270)%j-A(j,272)%j-A(j,426)%j-A(j,428)%j-A(j,432)%j-A(j,434)%j-A(j,442)%j-A(j,446)%j-A(j,498)%j &
       -A(j,500)%j)*f(6)

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
  use ol_colourmatrix_pphlljj_eexbbxhggg_1_/**/REALKIND, only: K1
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
  use ol_colourmatrix_pphlljj_eexbbxhggg_1_/**/REALKIND, only: K1
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
  & bind(c,name="ol_tree_colvect_pphlljj_eexbbxhggg_1")
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
    & bind(c,name="ol_f_amp2tree_pphlljj_eexbbxhggg_1")
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
    & bind(c,name="ol_f_amp2ccone_pphlljj_eexbbxhggg_1")
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
    & bind(c,name="ol_f_amp2ccall_pphlljj_eexbbxhggg_1")
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
    & bind(c,name="ol_f_amp2hcone_pphlljj_eexbbxhggg_1")
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
    & bind(c,name="ol_f_amp2hcall_pphlljj_eexbbxhggg_1")
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
    & bind(c,name="ol_amp2tree_pphlljj_eexbbxhggg_1")
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
    & bind(c,name="ol_amp2ccone_pphlljj_eexbbxhggg_1")
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
    & bind(c,name="ol_amp2ccall_pphlljj_eexbbxhggg_1")
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
    & bind(c,name="ol_amp2hcone_pphlljj_eexbbxhggg_1")
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
    & bind(c,name="ol_amp2hcall_pphlljj_eexbbxhggg_1")
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
    & bind(c,name="amp2tree_pphlljj_eexbbxhggg_1_")
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
    & bind(c,name="amp2ccone_pphlljj_eexbbxhggg_1_")
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
    & bind(c,name="amp2ccall_pphlljj_eexbbxhggg_1_")
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
    & bind(c,name="amp2hcone_pphlljj_eexbbxhggg_1_")
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
    & bind(c,name="amp2hcall_pphlljj_eexbbxhggg_1_")
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

end module ol_tree_pphlljj_eexbbxhggg_1_/**/REALKIND
