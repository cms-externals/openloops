
module ol_colourmatrix_ppvvvj_uuxbbxzzz_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(60,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [   9,   3]
  K1( 2,:) = [   3,   9]
  K1( 3,:) = [  12,   4]
  K1( 4,:) = [   4,  12]
  K1( 5,:) = [   0,  -4]
  K1( 6,:) = [  -4, -12]
  K1( 7,:) = [  12,   4]
  K1( 8,:) = [   4,  12]
  K1( 9,:) = [   0,   4]
  K1(10,:) = [   4,   0]
  K1(11,:) = [ -12,  -4]
  K1(12,:) = [  -4,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [ -12,  -4]
  K1(16,:) = [  -4,   0]
  K1(17,:) = [   0,   4]
  K1(18,:) = [   4,   0]
  K1(19,:) = [   0,  -4]
  K1(20,:) = [  -4, -12]
  K1(21,:) = [  12,   4]
  K1(22,:) = [   4,  12]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [   0,   0]
  K1(28,:) = [   0,   0]
  K1(29,:) = [   0,   0]
  K1(30,:) = [   0,   0]
  K1(31,:) = [   0,   0]
  K1(32,:) = [   0,   0]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [   0,   0]
  K1(38,:) = [   0,   0]
  K1(39,:) = [   0,   0]
  K1(40,:) = [   0,   0]
  K1(41,:) = [   0,   0]
  K1(42,:) = [   0,   0]
  K1(43,:) = [   0,   0]
  K1(44,:) = [   0,   0]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]
  K1(47,:) = [   0,   0]
  K1(48,:) = [   0,   0]
  K1(49,:) = [   0,   0]
  K1(50,:) = [   0,   0]
  K1(51,:) = [   0,   0]
  K1(52,:) = [   0,   0]
  K1(53,:) = [   0,   0]
  K1(54,:) = [   0,   0]
  K1(55,:) = [   0,   0]
  K1(56,:) = [   0,   0]
  K1(57,:) = [   0,   0]
  K1(58,:) = [   0,   0]
  K1(59,:) = [   0,   0]
  K1(60,:) = [   0,   0]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppvvvj_uuxbbxzzz_1_/**/REALKIND



module ol_forced_parameters_ppvvvj_uuxbbxzzz_1_/**/REALKIND
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
  if (wMZ /= 0) write(*,101) 'wMZ = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppvvvj_uuxbbxzzz_1_/**/REALKIND

module ol_tree_ppvvvj_uuxbbxzzz_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(3)
  complex(REALKIND), save :: den(281)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 432 ! number of helicity configurations
  integer(intkind2), save :: nhel = 432 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(432) ! physical helicity states
  complex(DREALKIND) :: M1helarr(2,432) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = CI*eQED**3*gQCD**2
    f(2) = (CI*eQED**3*gQCD**2*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(3) = (CI*eQED**3*gQCD**2*MW**2)/(cw**4*sw**2)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,20) - MB2)
  den(2) = 1 / (Q(5,3))
  den(3) = 1 / (Q(5,40) - MB2)
  den(5) = 1 / (Q(5,84) - MB2)
  den(9) = 1 / (Q(5,104) - MB2)
  den(12) = 1 / (Q(5,72) - MB2)
  den(14) = 1 / (Q(5,52) - MB2)
  den(19) = 1 / (Q(5,96) - MH2)
  den(21) = 1 / (Q(5,11) - MB2)
  den(28) = 1 / (Q(5,36) - MB2)
  den(29) = 1 / (Q(5,24) - MB2)
  den(31) = 1 / (Q(5,100) - MB2)
  den(35) = 1 / (Q(5,88) - MB2)
  den(38) = 1 / (Q(5,68) - MB2)
  den(42) = 1 / (Q(5,56) - MB2)
  den(46) = 1 / (Q(5,7) - MB2)
  den(57) = 1 / (Q(5,80) - MH2)
  den(74) = 1 / (Q(5,48) - MH2)
  den(83) = 1 / (Q(5,112) - MZ2)
  den(103) = 1 / (Q(5,17))
  den(104) = 1 / (Q(5,34))
  den(105) = 1 / (Q(5,12))
  den(107) = 1 / (Q(5,81))
  den(111) = 1 / (Q(5,98))
  den(114) = 1 / (Q(5,66))
  den(116) = 1 / (Q(5,49))
  den(121) = 1 / (Q(5,14))
  den(125) = 1 / (Q(5,33))
  den(126) = 1 / (Q(5,18))
  den(128) = 1 / (Q(5,97))
  den(132) = 1 / (Q(5,82))
  den(135) = 1 / (Q(5,65))
  den(139) = 1 / (Q(5,50))
  den(142) = 1 / (Q(5,13))
  den(169) = 1 / (Q(5,76))
  den(175) = 1 / (Q(5,44))
  den(179) = 1 / (Q(5,19))
  den(216) = 1 / (Q(5,28))
  den(220) = 1 / (Q(5,35))
  den(232) = 1 / (Q(5,67))

  ! denominators

  den(4) = den(2)*den(3)
  den(6) = den(1)*den(5)
  den(7) = den(4)*den(6)
  den(8) = den(1)*den(2)
  den(10) = den(3)*den(9)
  den(11) = den(8)*den(10)
  den(13) = den(2)*den(12)
  den(15) = den(1)*den(14)
  den(16) = den(13)*den(15)
  den(17) = den(9)*den(12)
  den(18) = den(8)*den(17)
  den(20) = den(1)*den(19)
  den(22) = den(2)*den(21)
  den(23) = den(20)*den(22)
  den(24) = den(9)*den(19)
  den(25) = den(8)*den(24)
  den(26) = den(15)*den(22)
  den(27) = den(6)*den(22)
  den(30) = den(2)*den(29)
  den(32) = den(28)*den(31)
  den(33) = den(30)*den(32)
  den(34) = den(2)*den(28)
  den(36) = den(29)*den(35)
  den(37) = den(34)*den(36)
  den(39) = den(31)*den(38)
  den(40) = den(30)*den(39)
  den(41) = den(2)*den(38)
  den(43) = den(29)*den(42)
  den(44) = den(41)*den(43)
  den(45) = den(19)*den(29)
  den(47) = den(2)*den(46)
  den(48) = den(45)*den(47)
  den(49) = den(19)*den(31)
  den(50) = den(30)*den(49)
  den(51) = den(36)*den(47)
  den(52) = den(43)*den(47)
  den(53) = den(14)*den(28)
  den(54) = den(13)*den(53)
  den(55) = den(12)*den(35)
  den(56) = den(34)*den(55)
  den(58) = den(28)*den(57)
  den(59) = den(22)*den(58)
  den(60) = den(35)*den(57)
  den(61) = den(34)*den(60)
  den(62) = den(22)*den(53)
  den(63) = den(22)*den(32)
  den(64) = den(5)*den(38)
  den(65) = den(4)*den(64)
  den(66) = den(3)*den(42)
  den(67) = den(41)*den(66)
  den(68) = den(3)*den(57)
  den(69) = den(47)*den(68)
  den(70) = den(5)*den(57)
  den(71) = den(4)*den(70)
  den(72) = den(10)*den(47)
  den(73) = den(47)*den(66)
  den(75) = den(38)*den(74)
  den(76) = den(22)*den(75)
  den(77) = den(42)*den(74)
  den(78) = den(41)*den(77)
  den(79) = den(12)*den(74)
  den(80) = den(47)*den(79)
  den(81) = den(14)*den(74)
  den(82) = den(13)*den(81)
  den(84) = den(74)*den(83)
  den(85) = den(47)*den(84)
  den(86) = den(47)*den(77)
  den(87) = den(22)*den(84)
  den(88) = den(22)*den(81)
  den(89) = den(22)*den(64)
  den(90) = den(22)*den(39)
  den(91) = den(17)*den(47)
  den(92) = den(47)*den(55)
  den(93) = den(57)*den(83)
  den(94) = den(47)*den(93)
  den(95) = den(47)*den(60)
  den(96) = den(22)*den(93)
  den(97) = den(22)*den(70)
  den(98) = den(19)*den(83)
  den(99) = den(47)*den(98)
  den(100) = den(24)*den(47)
  den(101) = den(22)*den(98)
  den(102) = den(22)*den(49)
  den(106) = den(104)*den(105)
  den(108) = den(103)*den(107)
  den(109) = den(106)*den(108)
  den(110) = den(103)*den(105)
  den(112) = den(104)*den(111)
  den(113) = den(110)*den(112)
  den(115) = den(105)*den(114)
  den(117) = den(103)*den(116)
  den(118) = den(115)*den(117)
  den(119) = den(111)*den(114)
  den(120) = den(110)*den(119)
  den(122) = den(105)*den(121)
  den(123) = den(117)*den(122)
  den(124) = den(108)*den(122)
  den(127) = den(105)*den(126)
  den(129) = den(125)*den(128)
  den(130) = den(127)*den(129)
  den(131) = den(105)*den(125)
  den(133) = den(126)*den(132)
  den(134) = den(131)*den(133)
  den(136) = den(128)*den(135)
  den(137) = den(127)*den(136)
  den(138) = den(105)*den(135)
  den(140) = den(126)*den(139)
  den(141) = den(138)*den(140)
  den(143) = den(105)*den(142)
  den(144) = den(133)*den(143)
  den(145) = den(140)*den(143)
  den(146) = den(116)*den(125)
  den(147) = den(115)*den(146)
  den(148) = den(114)*den(132)
  den(149) = den(131)*den(148)
  den(150) = den(122)*den(146)
  den(151) = den(122)*den(129)
  den(152) = den(107)*den(135)
  den(153) = den(106)*den(152)
  den(154) = den(104)*den(139)
  den(155) = den(138)*den(154)
  den(156) = den(112)*den(143)
  den(157) = den(143)*den(154)
  den(158) = den(84)*den(143)
  den(159) = den(84)*den(122)
  den(160) = den(122)*den(152)
  den(161) = den(122)*den(136)
  den(162) = den(119)*den(143)
  den(163) = den(143)*den(148)
  den(164) = den(93)*den(143)
  den(165) = den(93)*den(122)
  den(166) = den(98)*den(143)
  den(167) = den(98)*den(122)
  den(168) = den(103)*den(104)
  den(170) = den(38)*den(169)
  den(171) = den(168)*den(170)
  den(172) = den(12)*den(169)
  den(173) = den(168)*den(172)
  den(174) = den(103)*den(114)
  den(176) = den(28)*den(175)
  den(177) = den(174)*den(176)
  den(178) = den(12)*den(28)
  den(180) = den(103)*den(179)
  den(181) = den(178)*den(180)
  den(182) = den(32)*den(180)
  den(183) = den(108)*den(176)
  den(184) = den(3)*den(175)
  den(185) = den(174)*den(184)
  den(186) = den(3)*den(38)
  den(187) = den(180)*den(186)
  den(188) = den(10)*den(180)
  den(189) = den(108)*den(184)
  den(190) = den(39)*den(180)
  den(191) = den(117)*den(170)
  den(192) = den(17)*den(180)
  den(193) = den(117)*den(172)
  den(194) = den(24)*den(180)
  den(195) = den(49)*den(180)
  den(196) = den(125)*den(126)
  den(197) = den(170)*den(196)
  den(198) = den(172)*den(196)
  den(199) = den(126)*den(135)
  den(200) = den(176)*den(199)
  den(201) = den(126)*den(179)
  den(202) = den(178)*den(201)
  den(203) = den(32)*den(201)
  den(204) = den(133)*den(176)
  den(205) = den(184)*den(199)
  den(206) = den(186)*den(201)
  den(207) = den(10)*den(201)
  den(208) = den(133)*den(184)
  den(209) = den(39)*den(201)
  den(210) = den(140)*den(170)
  den(211) = den(17)*den(201)
  den(212) = den(140)*den(172)
  den(213) = den(24)*den(201)
  den(214) = den(49)*den(201)
  den(215) = den(114)*den(125)
  den(217) = den(1)*den(216)
  den(218) = den(215)*den(217)
  den(219) = den(1)*den(12)
  den(221) = den(125)*den(220)
  den(222) = den(219)*den(221)
  den(223) = den(6)*den(221)
  den(224) = den(129)*den(217)
  den(225) = den(104)*den(135)
  den(226) = den(217)*den(225)
  den(227) = den(104)*den(220)
  den(228) = den(219)*den(227)
  den(229) = den(6)*den(227)
  den(230) = den(112)*den(217)
  den(231) = den(1)*den(3)
  den(233) = den(135)*den(232)
  den(234) = den(231)*den(233)
  den(235) = den(114)*den(232)
  den(236) = den(231)*den(235)
  den(237) = den(15)*den(233)
  den(238) = den(136)*den(217)
  den(239) = den(15)*den(235)
  den(240) = den(119)*den(217)
  den(241) = den(29)*den(216)
  den(242) = den(215)*den(241)
  den(243) = den(29)*den(38)
  den(244) = den(221)*den(243)
  den(245) = den(36)*den(221)
  den(246) = den(129)*den(241)
  den(247) = den(225)*den(241)
  den(248) = den(227)*den(243)
  den(249) = den(36)*den(227)
  den(250) = den(112)*den(241)
  den(251) = den(28)*den(29)
  den(252) = den(233)*den(251)
  den(253) = den(235)*den(251)
  den(254) = den(43)*den(233)
  den(255) = den(136)*den(241)
  den(256) = den(43)*den(235)
  den(257) = den(119)*den(241)
  den(258) = den(64)*den(221)
  den(259) = den(146)*den(170)
  den(260) = den(55)*den(221)
  den(261) = den(146)*den(172)
  den(262) = den(60)*den(221)
  den(263) = den(70)*den(221)
  den(264) = den(64)*den(227)
  den(265) = den(154)*den(170)
  den(266) = den(55)*den(227)
  den(267) = den(154)*den(172)
  den(268) = den(60)*den(227)
  den(269) = den(70)*den(227)
  den(270) = den(53)*den(233)
  den(271) = den(152)*den(176)
  den(272) = den(53)*den(235)
  den(273) = den(148)*den(176)
  den(274) = den(66)*den(233)
  den(275) = den(152)*den(184)
  den(276) = den(66)*den(235)
  den(277) = den(148)*den(184)
  den(278) = den(77)*den(233)
  den(279) = den(81)*den(233)
  den(280) = den(77)*den(235)
  den(281) = den(81)*den(235)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_ppvvvj_uuxbbxzzz_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_ppvvvj_uuxbbxzzz_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for up anti-up bottom anti-bottom Z Z Z -> 0
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
  use ol_external_ppvvvj_uuxbbxzzz_1, only: external_perm_ppvvvj_uuxbbxzzz_1, &
    & external_perm_inv_ppvvvj_uuxbbxzzz_1, extcomb_perm_ppvvvj_uuxbbxzzz_1, &
    & average_factor_ppvvvj_uuxbbxzzz_1
  use ol_external_ppvvvj_uuxbbxzzz_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_ppvvvj_uuxbbxzzz_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_ppvvvj_uuxbbxzzz_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_ppvvvj_uuxbbxzzz_1
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
  complex(REALKIND) :: MOM_LC(4), M1(2), M1helarray(2,432)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(3), ex6(3), ex7(3)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,2), wf6(6,24), wf8(8,8), wf9(9,3), wf12(12,12), wf16(16,4), wf18(18,60), wf24(24,48), wf27(27,6), &
    wf36(36,12), wf54(54,9), wf432(432,162)

  type(polcont) :: A(432,162)
  complex(REALKIND) :: Aj(162)

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
  extmasses2 = [ rZERO2, rZERO2, rMB2, rMB2, rMZ2, rMZ2, rMZ2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_ppvvvj_uuxbbxzzz_1,7)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,7)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_ppvvvj_uuxbbxzzz_1(I)
  else if (I < 0) then
    JBmunu = external_perm_ppvvvj_uuxbbxzzz_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_ppvvvj_uuxbbxzzz_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_Q(P(:,1), rZERO, H1, ex1)
  call wf_A(P(:,2), rZERO, H2, ex2)
  call wf_Q(P(:,3), rMB, H3, ex3)
  call wf_A(P(:,4), rMB, H4, ex4)
  call wf_V(P(:,5), rMZ, H5, ex5)
  call wf_V(P(:,6), rMZ, H6, ex6)
  call wf_V(P(:,7), rMZ, H7, ex7)


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
  call vert_ZQ_A(gZd,ntry, ex7, wf6(:,3), wf18(:,1), n3(:,4), t3x18(:,:,1))
  call vert_AV_Q(ntry, wf6(:,4), wf4(:,1), wf24(:,1), n3(:,5), t3x24(:,:,1))
  call prop_Q_A(ntry, wf18(:,1), Q(:,84), MB, 1_intkind1, wf18(:,2), n2(3))
  call vert_AZ_Q(gZd,ntry, wf6(:,4), ex7, wf18(:,3), n3(:,6), t3x18(:,:,2))
  call vert_VQ_A(ntry, wf4(:,1), wf6(:,3), wf24(:,2), n3(:,7), t3x24(:,:,2))
  call prop_A_Q(ntry, wf18(:,3), Q(:,104), MB, 1_intkind1, wf18(:,4), n2(4))
  call vert_AZ_Q(gZd,ntry, ex4, ex7, wf6(:,5), n3(:,8), t3x6(:,:,3))
  call prop_A_Q(ntry, wf6(:,5), Q(:,72), MB, 1_intkind1, wf6(:,6), n2(5))
  call vert_ZQ_A(gZd,ntry, ex6, wf6(:,3), wf18(:,5), n3(:,9), t3x18(:,:,3))
  call vert_AV_Q(ntry, wf6(:,6), wf4(:,1), wf24(:,3), n3(:,10), t3x24(:,:,3))
  call prop_Q_A(ntry, wf18(:,5), Q(:,52), MB, 1_intkind1, wf18(:,6), n2(6))
  call vert_AZ_Q(gZd,ntry, wf6(:,6), ex6, wf18(:,7), n3(:,11), t3x18(:,:,4))
  call prop_A_Q(ntry, wf18(:,7), Q(:,104), MB, 1_intkind1, wf18(:,8), n2(7))
  call vert_VV_S(ntry, ex6, ex7, wf9(:,1), n3(:,12), t3x9(:,:,1))
  call vert_AV_Q(ntry, ex4, wf4(:,1), wf8(:,1), n3(:,13), t3x8(:,:,1))
  call vert_QS_A(gH,ntry, wf6(:,3), wf9(:,1), wf54(:,1), n3(:,14), t3x54(:,:,1))
  call prop_A_Q(ntry, wf8(:,1), Q(:,11), MB, 1_intkind1, wf8(:,2), n2(8))
  call vert_SA_Q(gH,ntry, wf9(:,1), ex4, wf18(:,9), n3(:,15), t3x18(:,:,5))
  call prop_A_Q(ntry, wf18(:,9), Q(:,104), MB, 1_intkind1, wf18(:,10), n2(9))
  call vert_AZ_Q(gZd,ntry, wf8(:,2), ex7, wf24(:,4), n3(:,16), t3x24(:,:,4))
  call vert_AZ_Q(gZd,ntry, wf8(:,2), ex6, wf24(:,5), n3(:,17), t3x24(:,:,5))
  call vert_ZQ_A(gZd,ntry, ex6, ex3, wf6(:,7), n3(:,18), t3x6(:,:,4))
  call vert_AZ_Q(gZd,ntry, ex4, ex5, wf6(:,8), n3(:,19), t3x6(:,:,5))
  call prop_Q_A(ntry, wf6(:,7), Q(:,36), MB, 1_intkind1, wf6(:,9), n2(10))
  call prop_A_Q(ntry, wf6(:,8), Q(:,24), MB, 1_intkind1, wf6(:,10), n2(11))
  call vert_ZQ_A(gZd,ntry, ex7, wf6(:,9), wf18(:,11), n3(:,20), t3x18(:,:,6))
  call vert_AV_Q(ntry, wf6(:,10), wf4(:,1), wf24(:,6), n3(:,21), t3x24(:,:,6))
  call prop_Q_A(ntry, wf18(:,11), Q(:,100), MB, 1_intkind1, wf18(:,12), n2(12))
  call vert_AZ_Q(gZd,ntry, wf6(:,10), ex7, wf18(:,13), n3(:,22), t3x18(:,:,7))
  call vert_VQ_A(ntry, wf4(:,1), wf6(:,9), wf24(:,7), n3(:,23), t3x24(:,:,7))
  call prop_A_Q(ntry, wf18(:,13), Q(:,88), MB, 1_intkind1, wf18(:,14), n2(13))
  call vert_ZQ_A(gZd,ntry, ex7, ex3, wf6(:,11), n3(:,24), t3x6(:,:,6))
  call prop_Q_A(ntry, wf6(:,11), Q(:,68), MB, 1_intkind1, wf6(:,12), n2(14))
  call vert_ZQ_A(gZd,ntry, ex6, wf6(:,12), wf18(:,15), n3(:,25), t3x18(:,:,8))
  call prop_Q_A(ntry, wf18(:,15), Q(:,100), MB, 1_intkind1, wf18(:,16), n2(15))
  call vert_AZ_Q(gZd,ntry, wf6(:,10), ex6, wf18(:,17), n3(:,26), t3x18(:,:,9))
  call vert_VQ_A(ntry, wf4(:,1), wf6(:,12), wf24(:,8), n3(:,27), t3x24(:,:,8))
  call prop_A_Q(ntry, wf18(:,17), Q(:,56), MB, 1_intkind1, wf18(:,18), n2(16))
  call vert_VQ_A(ntry, wf4(:,1), ex3, wf8(:,3), n3(:,28), t3x8(:,:,2))
  call vert_SA_Q(gH,ntry, wf9(:,1), wf6(:,10), wf54(:,2), n3(:,29), t3x54(:,:,2))
  call prop_Q_A(ntry, wf8(:,3), Q(:,7), MB, 1_intkind1, wf8(:,4), n2(17))
  call vert_QS_A(gH,ntry, ex3, wf9(:,1), wf18(:,19), n3(:,30), t3x18(:,:,10))
  call prop_Q_A(ntry, wf18(:,19), Q(:,100), MB, 1_intkind1, wf18(:,20), n2(18))
  call vert_ZQ_A(gZd,ntry, ex6, wf8(:,4), wf24(:,9), n3(:,31), t3x24(:,:,9))
  call vert_ZQ_A(gZd,ntry, ex7, wf8(:,4), wf24(:,10), n3(:,32), t3x24(:,:,10))
  call vert_ZQ_A(gZd,ntry, ex5, wf6(:,9), wf18(:,21), n3(:,33), t3x18(:,:,11))
  call prop_Q_A(ntry, wf18(:,21), Q(:,52), MB, 1_intkind1, wf18(:,22), n2(19))
  call vert_AZ_Q(gZd,ntry, wf6(:,6), ex5, wf18(:,23), n3(:,34), t3x18(:,:,12))
  call prop_A_Q(ntry, wf18(:,23), Q(:,88), MB, 1_intkind1, wf18(:,24), n2(20))
  call vert_VV_S(ntry, ex5, ex7, wf9(:,2), n3(:,35), t3x9(:,:,2))
  call vert_QS_A(gH,ntry, wf6(:,9), wf9(:,2), wf54(:,3), n3(:,36), t3x54(:,:,3))
  call vert_SA_Q(gH,ntry, wf9(:,2), ex4, wf18(:,25), n3(:,37), t3x18(:,:,13))
  call prop_A_Q(ntry, wf18(:,25), Q(:,88), MB, 1_intkind1, wf18(:,26), n2(21))
  call vert_AZ_Q(gZd,ntry, wf8(:,2), ex5, wf24(:,11), n3(:,38), t3x24(:,:,11))
  call vert_ZQ_A(gZd,ntry, ex5, wf6(:,12), wf18(:,27), n3(:,39), t3x18(:,:,14))
  call prop_Q_A(ntry, wf18(:,27), Q(:,84), MB, 1_intkind1, wf18(:,28), n2(22))
  call vert_AZ_Q(gZd,ntry, wf6(:,4), ex5, wf18(:,29), n3(:,40), t3x18(:,:,15))
  call prop_A_Q(ntry, wf18(:,29), Q(:,56), MB, 1_intkind1, wf18(:,30), n2(23))
  call vert_SA_Q(gH,ntry, wf9(:,2), wf6(:,4), wf54(:,4), n3(:,41), t3x54(:,:,4))
  call vert_QS_A(gH,ntry, ex3, wf9(:,2), wf18(:,31), n3(:,42), t3x18(:,:,16))
  call prop_Q_A(ntry, wf18(:,31), Q(:,84), MB, 1_intkind1, wf18(:,32), n2(24))
  call vert_ZQ_A(gZd,ntry, ex5, wf8(:,4), wf24(:,12), n3(:,43), t3x24(:,:,12))
  call vert_VV_S(ntry, ex5, ex6, wf9(:,3), n3(:,44), t3x9(:,:,3))
  call vert_QS_A(gH,ntry, wf6(:,12), wf9(:,3), wf54(:,5), n3(:,45), t3x54(:,:,5))
  call vert_SA_Q(gH,ntry, wf9(:,3), ex4, wf18(:,33), n3(:,46), t3x18(:,:,17))
  call prop_A_Q(ntry, wf18(:,33), Q(:,56), MB, 1_intkind1, wf18(:,34), n2(25))
  call vert_SA_Q(gH,ntry, wf9(:,3), wf6(:,6), wf54(:,6), n3(:,47), t3x54(:,:,6))
  call vert_QS_A(gH,ntry, ex3, wf9(:,3), wf18(:,35), n3(:,48), t3x18(:,:,18))
  call prop_Q_A(ntry, wf18(:,35), Q(:,52), MB, 1_intkind1, wf18(:,36), n2(26))
  call vert_SV_V(ntry, wf9(:,3), ex7, wf27(:,1), n3(:,49), t3x27(:,:,1))
  call prop_W_W(ntry, wf27(:,1), Q(:,112), MZ, 1_intkind1, wf27(:,2), n2(27))
  call vert_QA_Z(gZd,ntry, wf8(:,4), ex4, wf16(:,1), n3(:,50), t3x16(:,:,1))
  call vert_QA_Z(gZd,ntry, ex3, wf8(:,2), wf16(:,2), n3(:,51), t3x16(:,:,2))
  call vert_ZQ_A(gZd,ntry, ex7, wf18(:,36), wf54(:,7), n3(:,52), t3x54(:,:,7))
  call vert_SV_V(ntry, wf9(:,2), ex6, wf27(:,3), n3(:,53), t3x27(:,:,2))
  call prop_W_W(ntry, wf27(:,3), Q(:,112), MZ, 1_intkind1, wf27(:,4), n2(28))
  call vert_ZQ_A(gZd,ntry, ex6, wf18(:,32), wf54(:,8), n3(:,54), t3x54(:,:,8))
  call vert_SV_V(ntry, wf9(:,1), ex5, wf27(:,5), n3(:,55), t3x27(:,:,3))
  call prop_W_W(ntry, wf27(:,5), Q(:,112), MZ, 1_intkind1, wf27(:,6), n2(29))
  call vert_ZQ_A(gZd,ntry, ex5, wf18(:,20), wf54(:,9), n3(:,56), t3x54(:,:,9))
  call vert_ZQ_A(gZu,ntry, ex5, ex1, wf6(:,13), n3(:,57), t3x6(:,:,7))
  call vert_AZ_Q(gZu,ntry, ex2, ex6, wf6(:,14), n3(:,58), t3x6(:,:,8))
  call vert_QA_V(ntry, ex3, ex4, wf4(:,2), n3(:,59), t3x4(:,:,2))
  call prop_Q_A(ntry, wf6(:,13), Q(:,17), ZERO, 0_intkind1, wf6(:,15), n2(30))
  call prop_A_Q(ntry, wf6(:,14), Q(:,34), ZERO, 0_intkind1, wf6(:,16), n2(31))
  call vert_ZQ_A(gZu,ntry, ex7, wf6(:,15), wf18(:,37), n3(:,60), t3x18(:,:,19))
  call vert_AV_Q(ntry, wf6(:,16), wf4(:,2), wf24(:,13), n3(:,61), t3x24(:,:,13))
  call prop_Q_A(ntry, wf18(:,37), Q(:,81), ZERO, 0_intkind1, wf18(:,38), n2(32))
  call vert_AZ_Q(gZu,ntry, wf6(:,16), ex7, wf18(:,39), n3(:,62), t3x18(:,:,20))
  call vert_VQ_A(ntry, wf4(:,2), wf6(:,15), wf24(:,14), n3(:,63), t3x24(:,:,14))
  call prop_A_Q(ntry, wf18(:,39), Q(:,98), ZERO, 0_intkind1, wf18(:,40), n2(33))
  call vert_AZ_Q(gZu,ntry, ex2, ex7, wf6(:,17), n3(:,64), t3x6(:,:,9))
  call prop_A_Q(ntry, wf6(:,17), Q(:,66), ZERO, 0_intkind1, wf6(:,18), n2(34))
  call vert_ZQ_A(gZu,ntry, ex6, wf6(:,15), wf18(:,41), n3(:,65), t3x18(:,:,21))
  call vert_AV_Q(ntry, wf6(:,18), wf4(:,2), wf24(:,15), n3(:,66), t3x24(:,:,15))
  call prop_Q_A(ntry, wf18(:,41), Q(:,49), ZERO, 0_intkind1, wf18(:,42), n2(35))
  call vert_AZ_Q(gZu,ntry, wf6(:,18), ex6, wf18(:,43), n3(:,67), t3x18(:,:,22))
  call prop_A_Q(ntry, wf18(:,43), Q(:,98), ZERO, 0_intkind1, wf18(:,44), n2(36))
  call vert_AV_Q(ntry, ex2, wf4(:,2), wf8(:,5), n3(:,68), t3x8(:,:,3))
  call prop_A_Q(ntry, wf8(:,5), Q(:,14), ZERO, 0_intkind1, wf8(:,6), n2(37))
  call vert_AZ_Q(gZu,ntry, wf8(:,6), ex7, wf24(:,16), n3(:,69), t3x24(:,:,16))
  call vert_AZ_Q(gZu,ntry, wf8(:,6), ex6, wf24(:,17), n3(:,70), t3x24(:,:,17))
  call vert_ZQ_A(gZu,ntry, ex6, ex1, wf6(:,19), n3(:,71), t3x6(:,:,10))
  call vert_AZ_Q(gZu,ntry, ex2, ex5, wf6(:,20), n3(:,72), t3x6(:,:,11))
  call prop_Q_A(ntry, wf6(:,19), Q(:,33), ZERO, 0_intkind1, wf6(:,21), n2(38))
  call prop_A_Q(ntry, wf6(:,20), Q(:,18), ZERO, 0_intkind1, wf6(:,22), n2(39))
  call vert_ZQ_A(gZu,ntry, ex7, wf6(:,21), wf18(:,45), n3(:,73), t3x18(:,:,23))
  call vert_AV_Q(ntry, wf6(:,22), wf4(:,2), wf24(:,18), n3(:,74), t3x24(:,:,18))
  call prop_Q_A(ntry, wf18(:,45), Q(:,97), ZERO, 0_intkind1, wf18(:,46), n2(40))
  call vert_AZ_Q(gZu,ntry, wf6(:,22), ex7, wf18(:,47), n3(:,75), t3x18(:,:,24))
  call vert_VQ_A(ntry, wf4(:,2), wf6(:,21), wf24(:,19), n3(:,76), t3x24(:,:,19))
  call prop_A_Q(ntry, wf18(:,47), Q(:,82), ZERO, 0_intkind1, wf18(:,48), n2(41))
  call vert_ZQ_A(gZu,ntry, ex7, ex1, wf6(:,23), n3(:,77), t3x6(:,:,12))
  call prop_Q_A(ntry, wf6(:,23), Q(:,65), ZERO, 0_intkind1, wf6(:,24), n2(42))
  call vert_ZQ_A(gZu,ntry, ex6, wf6(:,24), wf18(:,49), n3(:,78), t3x18(:,:,25))
  call prop_Q_A(ntry, wf18(:,49), Q(:,97), ZERO, 0_intkind1, wf18(:,50), n2(43))
  call vert_AZ_Q(gZu,ntry, wf6(:,22), ex6, wf18(:,51), n3(:,79), t3x18(:,:,26))
  call vert_VQ_A(ntry, wf4(:,2), wf6(:,24), wf24(:,20), n3(:,80), t3x24(:,:,20))
  call prop_A_Q(ntry, wf18(:,51), Q(:,50), ZERO, 0_intkind1, wf18(:,52), n2(44))
  call vert_VQ_A(ntry, wf4(:,2), ex1, wf8(:,7), n3(:,81), t3x8(:,:,4))
  call prop_Q_A(ntry, wf8(:,7), Q(:,13), ZERO, 0_intkind1, wf8(:,8), n2(45))
  call vert_ZQ_A(gZu,ntry, ex6, wf8(:,8), wf24(:,21), n3(:,82), t3x24(:,:,21))
  call vert_ZQ_A(gZu,ntry, ex7, wf8(:,8), wf24(:,22), n3(:,83), t3x24(:,:,22))
  call vert_ZQ_A(gZu,ntry, ex5, wf6(:,21), wf18(:,53), n3(:,84), t3x18(:,:,27))
  call prop_Q_A(ntry, wf18(:,53), Q(:,49), ZERO, 0_intkind1, wf18(:,54), n2(46))
  call vert_AZ_Q(gZu,ntry, wf6(:,18), ex5, wf18(:,55), n3(:,85), t3x18(:,:,28))
  call prop_A_Q(ntry, wf18(:,55), Q(:,82), ZERO, 0_intkind1, wf18(:,56), n2(47))
  call vert_AZ_Q(gZu,ntry, wf8(:,6), ex5, wf24(:,23), n3(:,86), t3x24(:,:,23))
  call vert_ZQ_A(gZu,ntry, ex5, wf6(:,24), wf18(:,57), n3(:,87), t3x18(:,:,29))
  call prop_Q_A(ntry, wf18(:,57), Q(:,81), ZERO, 0_intkind1, wf18(:,58), n2(48))
  call vert_AZ_Q(gZu,ntry, wf6(:,16), ex5, wf18(:,59), n3(:,88), t3x18(:,:,30))
  call prop_A_Q(ntry, wf18(:,59), Q(:,50), ZERO, 0_intkind1, wf18(:,60), n2(49))
  call vert_ZQ_A(gZu,ntry, ex5, wf8(:,8), wf24(:,24), n3(:,89), t3x24(:,:,24))
  call vert_QA_Z(gZu,ntry, wf8(:,8), ex2, wf16(:,3), n3(:,90), t3x16(:,:,3))
  call vert_QA_Z(gZu,ntry, ex1, wf8(:,6), wf16(:,4), n3(:,91), t3x16(:,:,4))
  call vert_QA_V(ntry, wf6(:,12), ex4, wf12(:,1), n3(:,92), t3x12(:,:,1))
  call vert_QA_V(ntry, wf6(:,15), wf6(:,16), wf36(:,1), n3(:,93), t3x36(:,:,1))
  call vert_QA_V(ntry, ex3, wf6(:,6), wf12(:,2), n3(:,94), t3x12(:,:,2))
  call vert_QA_V(ntry, wf6(:,9), ex4, wf12(:,3), n3(:,95), t3x12(:,:,3))
  call vert_QA_V(ntry, wf6(:,15), wf6(:,18), wf36(:,2), n3(:,96), t3x36(:,:,2))
  call vert_QA_V(ntry, wf6(:,15), ex2, wf12(:,4), n3(:,97), t3x12(:,:,4))
  call vert_QA_V(ntry, wf6(:,9), wf6(:,6), wf36(:,3), n3(:,98), t3x36(:,:,3))
  call vert_AV_Q(ntry, ex4, wf12(:,4), wf24(:,25), n3(:,99), t3x24(:,:,25))
  call vert_AV_Q(ntry, ex2, wf12(:,3), wf24(:,26), n3(:,100), t3x24(:,:,26))
  call vert_QA_V(ntry, ex3, wf6(:,4), wf12(:,5), n3(:,101), t3x12(:,:,5))
  call vert_QA_V(ntry, wf6(:,12), wf6(:,4), wf36(:,4), n3(:,102), t3x36(:,:,4))
  call vert_VQ_A(ntry, wf12(:,4), ex3, wf24(:,27), n3(:,103), t3x24(:,:,27))
  call vert_AV_Q(ntry, ex2, wf12(:,5), wf24(:,28), n3(:,104), t3x24(:,:,28))
  call vert_AV_Q(ntry, ex2, wf12(:,1), wf24(:,29), n3(:,105), t3x24(:,:,29))
  call vert_AV_Q(ntry, ex2, wf12(:,2), wf24(:,30), n3(:,106), t3x24(:,:,30))
  call vert_QA_V(ntry, wf6(:,21), wf6(:,22), wf36(:,5), n3(:,107), t3x36(:,:,5))
  call vert_QA_V(ntry, wf6(:,24), wf6(:,22), wf36(:,6), n3(:,108), t3x36(:,:,6))
  call vert_QA_V(ntry, ex1, wf6(:,22), wf12(:,6), n3(:,109), t3x12(:,:,6))
  call vert_AV_Q(ntry, ex4, wf12(:,6), wf24(:,31), n3(:,110), t3x24(:,:,31))
  call vert_VQ_A(ntry, wf12(:,3), ex1, wf24(:,32), n3(:,111), t3x24(:,:,32))
  call vert_VQ_A(ntry, wf12(:,6), ex3, wf24(:,33), n3(:,112), t3x24(:,:,33))
  call vert_VQ_A(ntry, wf12(:,5), ex1, wf24(:,34), n3(:,113), t3x24(:,:,34))
  call vert_VQ_A(ntry, wf12(:,1), ex1, wf24(:,35), n3(:,114), t3x24(:,:,35))
  call vert_VQ_A(ntry, wf12(:,2), ex1, wf24(:,36), n3(:,115), t3x24(:,:,36))
  call vert_QA_V(ntry, wf6(:,3), ex4, wf12(:,7), n3(:,116), t3x12(:,:,7))
  call vert_QA_V(ntry, wf6(:,21), wf6(:,18), wf36(:,7), n3(:,117), t3x36(:,:,7))
  call vert_QA_V(ntry, wf6(:,21), ex2, wf12(:,8), n3(:,118), t3x12(:,:,8))
  call vert_QA_V(ntry, wf6(:,3), wf6(:,6), wf36(:,8), n3(:,119), t3x36(:,:,8))
  call vert_AV_Q(ntry, ex4, wf12(:,8), wf24(:,37), n3(:,120), t3x24(:,:,37))
  call vert_AV_Q(ntry, ex2, wf12(:,7), wf24(:,38), n3(:,121), t3x24(:,:,38))
  call vert_QA_V(ntry, wf6(:,24), wf6(:,16), wf36(:,9), n3(:,122), t3x36(:,:,9))
  call vert_QA_V(ntry, ex1, wf6(:,16), wf12(:,9), n3(:,123), t3x12(:,:,9))
  call vert_AV_Q(ntry, ex4, wf12(:,9), wf24(:,39), n3(:,124), t3x24(:,:,39))
  call vert_VQ_A(ntry, wf12(:,7), ex1, wf24(:,40), n3(:,125), t3x24(:,:,40))
  call vert_QA_V(ntry, wf6(:,24), ex2, wf12(:,10), n3(:,126), t3x12(:,:,10))
  call vert_QA_V(ntry, wf6(:,3), wf6(:,4), wf36(:,10), n3(:,127), t3x36(:,:,10))
  call vert_QA_V(ntry, ex1, wf6(:,18), wf12(:,11), n3(:,128), t3x12(:,:,11))
  call vert_AV_Q(ntry, ex4, wf12(:,10), wf24(:,41), n3(:,129), t3x24(:,:,41))
  call vert_AV_Q(ntry, ex4, wf12(:,11), wf24(:,42), n3(:,130), t3x24(:,:,42))
  call vert_QA_V(ntry, ex3, wf6(:,10), wf12(:,12), n3(:,131), t3x12(:,:,12))
  call vert_QA_V(ntry, wf6(:,12), wf6(:,10), wf36(:,11), n3(:,132), t3x36(:,:,11))
  call vert_VQ_A(ntry, wf12(:,8), ex3, wf24(:,43), n3(:,133), t3x24(:,:,43))
  call vert_AV_Q(ntry, ex2, wf12(:,12), wf24(:,44), n3(:,134), t3x24(:,:,44))
  call vert_VQ_A(ntry, wf12(:,9), ex3, wf24(:,45), n3(:,135), t3x24(:,:,45))
  call vert_VQ_A(ntry, wf12(:,12), ex1, wf24(:,46), n3(:,136), t3x24(:,:,46))
  call vert_QA_V(ntry, wf6(:,9), wf6(:,10), wf36(:,12), n3(:,137), t3x36(:,:,12))
  call vert_VQ_A(ntry, wf12(:,10), ex3, wf24(:,47), n3(:,138), t3x24(:,:,47))
  call vert_VQ_A(ntry, wf12(:,11), ex3, wf24(:,48), n3(:,139), t3x24(:,:,48))


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
  M2add = M2 / average_factor_ppvvvj_uuxbbxzzz_1

  do k = 0, 30-1
    M2(k) = M2add(extcomb_perm_ppvvvj_uuxbbxzzz_1(k))
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

    call cont_QA(nsync, wf24(:,1), wf18(:,2), A(:,1), n3(:,140), t3x432(:,:,1), nhel, den(7))
    call cont_QA(nsync, wf24(:,2), wf18(:,4), A(:,2), n3(:,141), t3x432(:,:,2), nhel, den(11))
    call cont_QA(nsync, wf24(:,3), wf18(:,6), A(:,3), n3(:,142), t3x432(:,:,3), nhel, den(16))
    call cont_QA(nsync, wf24(:,2), wf18(:,8), A(:,4), n3(:,143), t3x432(:,:,4), nhel, den(18))
    call cont_QA(nsync, wf54(:,1), wf8(:,2), A(:,5), n3(:,144), t3x432(:,:,5), nhel, den(23))
    call cont_QA(nsync, wf24(:,2), wf18(:,10), A(:,6), n3(:,145), t3x432(:,:,6), nhel, den(25))
    call cont_QA(nsync, wf18(:,6), wf24(:,4), A(:,7), n3(:,146), t3x432(:,:,7), nhel, den(26))
    call cont_QA(nsync, wf18(:,2), wf24(:,5), A(:,8), n3(:,147), t3x432(:,:,8), nhel, den(27))
    call cont_QA(nsync, wf24(:,6), wf18(:,12), A(:,9), n3(:,148), t3x432(:,:,9), nhel, den(33))
    call cont_QA(nsync, wf24(:,7), wf18(:,14), A(:,10), n3(:,149), t3x432(:,:,10), nhel, den(37))
    call cont_QA(nsync, wf24(:,6), wf18(:,16), A(:,11), n3(:,150), t3x432(:,:,11), nhel, den(40))
    call cont_QA(nsync, wf24(:,8), wf18(:,18), A(:,12), n3(:,151), t3x432(:,:,12), nhel, den(44))
    call cont_QA(nsync, wf54(:,2), wf8(:,4), A(:,13), n3(:,152), t3x432(:,:,13), nhel, den(48))
    call cont_QA(nsync, wf24(:,6), wf18(:,20), A(:,14), n3(:,153), t3x432(:,:,14), nhel, den(50))
    call cont_QA(nsync, wf18(:,14), wf24(:,9), A(:,15), n3(:,154), t3x432(:,:,15), nhel, den(51))
    call cont_QA(nsync, wf18(:,18), wf24(:,10), A(:,16), n3(:,155), t3x432(:,:,16), nhel, den(52))
    call cont_QA(nsync, wf24(:,3), wf18(:,22), A(:,17), n3(:,156), t3x432(:,:,17), nhel, den(54))
    call cont_QA(nsync, wf24(:,7), wf18(:,24), A(:,18), n3(:,157), t3x432(:,:,18), nhel, den(56))
    call cont_QA(nsync, wf8(:,2), wf54(:,3), A(:,19), n3(:,158), t3x432(:,:,19), nhel, den(59))
    call cont_QA(nsync, wf24(:,7), wf18(:,26), A(:,20), n3(:,159), t3x432(:,:,20), nhel, den(61))
    call cont_QA(nsync, wf24(:,4), wf18(:,22), A(:,21), n3(:,160), t3x432(:,:,21), nhel, den(62))
    call cont_QA(nsync, wf18(:,12), wf24(:,11), A(:,22), n3(:,161), t3x432(:,:,22), nhel, den(63))
    call cont_QA(nsync, wf24(:,1), wf18(:,28), A(:,23), n3(:,162), t3x432(:,:,23), nhel, den(65))
    call cont_QA(nsync, wf24(:,8), wf18(:,30), A(:,24), n3(:,163), t3x432(:,:,24), nhel, den(67))
    call cont_QA(nsync, wf8(:,4), wf54(:,4), A(:,25), n3(:,164), t3x432(:,:,25), nhel, den(69))
    call cont_QA(nsync, wf24(:,1), wf18(:,32), A(:,26), n3(:,165), t3x432(:,:,26), nhel, den(71))
    call cont_QA(nsync, wf18(:,4), wf24(:,12), A(:,27), n3(:,166), t3x432(:,:,27), nhel, den(72))
    call cont_QA(nsync, wf24(:,10), wf18(:,30), A(:,28), n3(:,167), t3x432(:,:,28), nhel, den(73))
    call cont_QA(nsync, wf8(:,2), wf54(:,5), A(:,29), n3(:,168), t3x432(:,:,29), nhel, den(76))
    call cont_QA(nsync, wf24(:,8), wf18(:,34), A(:,30), n3(:,169), t3x432(:,:,30), nhel, den(78))
    call cont_QA(nsync, wf8(:,4), wf54(:,6), A(:,31), n3(:,170), t3x432(:,:,31), nhel, den(80))
    call cont_QA(nsync, wf24(:,3), wf18(:,36), A(:,32), n3(:,171), t3x432(:,:,32), nhel, den(82))
    call cont_VV(nsync, wf27(:,2), wf16(:,1), A(:,33), n3(:,172), t3x432(:,:,33), nhel, den(85))
    call cont_QA(nsync, wf24(:,10), wf18(:,34), A(:,34), n3(:,173), t3x432(:,:,34), nhel, den(86))
    call cont_VV(nsync, wf27(:,2), wf16(:,2), A(:,35), n3(:,174), t3x432(:,:,35), nhel, den(87))
    call cont_QA(nsync, wf8(:,2), wf54(:,7), A(:,36), n3(:,175), t3x432(:,:,36), nhel, den(88))
    call cont_QA(nsync, wf24(:,5), wf18(:,28), A(:,37), n3(:,176), t3x432(:,:,37), nhel, den(89))
    call cont_QA(nsync, wf18(:,16), wf24(:,11), A(:,38), n3(:,177), t3x432(:,:,38), nhel, den(90))
    call cont_QA(nsync, wf18(:,8), wf24(:,12), A(:,39), n3(:,178), t3x432(:,:,39), nhel, den(91))
    call cont_QA(nsync, wf24(:,9), wf18(:,24), A(:,40), n3(:,179), t3x432(:,:,40), nhel, den(92))
    call cont_VV(nsync, wf16(:,1), wf27(:,4), A(:,41), n3(:,180), t3x432(:,:,41), nhel, den(94))
    call cont_QA(nsync, wf24(:,9), wf18(:,26), A(:,42), n3(:,181), t3x432(:,:,42), nhel, den(95))
    call cont_VV(nsync, wf16(:,2), wf27(:,4), A(:,43), n3(:,182), t3x432(:,:,43), nhel, den(96))
    call cont_QA(nsync, wf8(:,2), wf54(:,8), A(:,44), n3(:,183), t3x432(:,:,44), nhel, den(97))
    call cont_VV(nsync, wf16(:,1), wf27(:,6), A(:,45), n3(:,184), t3x432(:,:,45), nhel, den(99))
    call cont_QA(nsync, wf18(:,10), wf24(:,12), A(:,46), n3(:,185), t3x432(:,:,46), nhel, den(100))
    call cont_VV(nsync, wf16(:,2), wf27(:,6), A(:,47), n3(:,186), t3x432(:,:,47), nhel, den(101))
    call cont_QA(nsync, wf8(:,2), wf54(:,9), A(:,48), n3(:,187), t3x432(:,:,48), nhel, den(102))
    call cont_QA(nsync, wf24(:,13), wf18(:,38), A(:,49), n3(:,188), t3x432(:,:,49), nhel, den(109))
    call cont_QA(nsync, wf24(:,14), wf18(:,40), A(:,50), n3(:,189), t3x432(:,:,50), nhel, den(113))
    call cont_QA(nsync, wf24(:,15), wf18(:,42), A(:,51), n3(:,190), t3x432(:,:,51), nhel, den(118))
    call cont_QA(nsync, wf24(:,14), wf18(:,44), A(:,52), n3(:,191), t3x432(:,:,52), nhel, den(120))
    call cont_QA(nsync, wf18(:,42), wf24(:,16), A(:,53), n3(:,192), t3x432(:,:,53), nhel, den(123))
    call cont_QA(nsync, wf18(:,38), wf24(:,17), A(:,54), n3(:,193), t3x432(:,:,54), nhel, den(124))
    call cont_QA(nsync, wf24(:,18), wf18(:,46), A(:,55), n3(:,194), t3x432(:,:,55), nhel, den(130))
    call cont_QA(nsync, wf24(:,19), wf18(:,48), A(:,56), n3(:,195), t3x432(:,:,56), nhel, den(134))
    call cont_QA(nsync, wf24(:,18), wf18(:,50), A(:,57), n3(:,196), t3x432(:,:,57), nhel, den(137))
    call cont_QA(nsync, wf24(:,20), wf18(:,52), A(:,58), n3(:,197), t3x432(:,:,58), nhel, den(141))
    call cont_QA(nsync, wf18(:,48), wf24(:,21), A(:,59), n3(:,198), t3x432(:,:,59), nhel, den(144))
    call cont_QA(nsync, wf18(:,52), wf24(:,22), A(:,60), n3(:,199), t3x432(:,:,60), nhel, den(145))
    call cont_QA(nsync, wf24(:,15), wf18(:,54), A(:,61), n3(:,200), t3x432(:,:,61), nhel, den(147))
    call cont_QA(nsync, wf24(:,19), wf18(:,56), A(:,62), n3(:,201), t3x432(:,:,62), nhel, den(149))
    call cont_QA(nsync, wf24(:,16), wf18(:,54), A(:,63), n3(:,202), t3x432(:,:,63), nhel, den(150))
    call cont_QA(nsync, wf18(:,46), wf24(:,23), A(:,64), n3(:,203), t3x432(:,:,64), nhel, den(151))
    call cont_QA(nsync, wf24(:,13), wf18(:,58), A(:,65), n3(:,204), t3x432(:,:,65), nhel, den(153))
    call cont_QA(nsync, wf24(:,20), wf18(:,60), A(:,66), n3(:,205), t3x432(:,:,66), nhel, den(155))
    call cont_QA(nsync, wf18(:,40), wf24(:,24), A(:,67), n3(:,206), t3x432(:,:,67), nhel, den(156))
    call cont_QA(nsync, wf24(:,22), wf18(:,60), A(:,68), n3(:,207), t3x432(:,:,68), nhel, den(157))
    call cont_VV(nsync, wf27(:,2), wf16(:,3), A(:,69), n3(:,208), t3x432(:,:,69), nhel, den(158))
    call cont_VV(nsync, wf27(:,2), wf16(:,4), A(:,70), n3(:,209), t3x432(:,:,70), nhel, den(159))
    call cont_QA(nsync, wf24(:,17), wf18(:,58), A(:,71), n3(:,210), t3x432(:,:,71), nhel, den(160))
    call cont_QA(nsync, wf18(:,50), wf24(:,23), A(:,72), n3(:,211), t3x432(:,:,72), nhel, den(161))
    call cont_QA(nsync, wf18(:,44), wf24(:,24), A(:,73), n3(:,212), t3x432(:,:,73), nhel, den(162))
    call cont_QA(nsync, wf24(:,21), wf18(:,56), A(:,74), n3(:,213), t3x432(:,:,74), nhel, den(163))
    call cont_VV(nsync, wf27(:,4), wf16(:,3), A(:,75), n3(:,214), t3x432(:,:,75), nhel, den(164))
    call cont_VV(nsync, wf27(:,4), wf16(:,4), A(:,76), n3(:,215), t3x432(:,:,76), nhel, den(165))
    call cont_VV(nsync, wf27(:,6), wf16(:,3), A(:,77), n3(:,216), t3x432(:,:,77), nhel, den(166))
    call cont_VV(nsync, wf27(:,6), wf16(:,4), A(:,78), n3(:,217), t3x432(:,:,78), nhel, den(167))
    call cont_VV(nsync, wf12(:,1), wf36(:,1), A(:,79), n3(:,218), t3x432(:,:,79), nhel, den(171))
    call cont_VV(nsync, wf36(:,1), wf12(:,2), A(:,80), n3(:,219), t3x432(:,:,80), nhel, den(173))
    call cont_VV(nsync, wf12(:,3), wf36(:,2), A(:,81), n3(:,220), t3x432(:,:,81), nhel, den(177))
    call cont_VV(nsync, wf12(:,4), wf36(:,3), A(:,82), n3(:,221), t3x432(:,:,82), nhel, den(181))
    call cont_QA(nsync, wf18(:,12), wf24(:,25), A(:,83), n3(:,222), t3x432(:,:,83), nhel, den(182))
    call cont_QA(nsync, wf18(:,38), wf24(:,26), A(:,84), n3(:,223), t3x432(:,:,84), nhel, den(183))
    call cont_VV(nsync, wf36(:,2), wf12(:,5), A(:,85), n3(:,224), t3x432(:,:,85), nhel, den(185))
    call cont_VV(nsync, wf12(:,4), wf36(:,4), A(:,86), n3(:,225), t3x432(:,:,86), nhel, den(187))
    call cont_QA(nsync, wf18(:,4), wf24(:,27), A(:,87), n3(:,226), t3x432(:,:,87), nhel, den(188))
    call cont_QA(nsync, wf18(:,38), wf24(:,28), A(:,88), n3(:,227), t3x432(:,:,88), nhel, den(189))
    call cont_QA(nsync, wf18(:,16), wf24(:,25), A(:,89), n3(:,228), t3x432(:,:,89), nhel, den(190))
    call cont_QA(nsync, wf18(:,42), wf24(:,29), A(:,90), n3(:,229), t3x432(:,:,90), nhel, den(191))
    call cont_QA(nsync, wf18(:,8), wf24(:,27), A(:,91), n3(:,230), t3x432(:,:,91), nhel, den(192))
    call cont_QA(nsync, wf18(:,42), wf24(:,30), A(:,92), n3(:,231), t3x432(:,:,92), nhel, den(193))
    call cont_QA(nsync, wf18(:,10), wf24(:,27), A(:,93), n3(:,232), t3x432(:,:,93), nhel, den(194))
    call cont_QA(nsync, wf18(:,20), wf24(:,25), A(:,94), n3(:,233), t3x432(:,:,94), nhel, den(195))
    call cont_VV(nsync, wf12(:,1), wf36(:,5), A(:,95), n3(:,234), t3x432(:,:,95), nhel, den(197))
    call cont_VV(nsync, wf12(:,2), wf36(:,5), A(:,96), n3(:,235), t3x432(:,:,96), nhel, den(198))
    call cont_VV(nsync, wf12(:,3), wf36(:,6), A(:,97), n3(:,236), t3x432(:,:,97), nhel, den(200))
    call cont_VV(nsync, wf36(:,3), wf12(:,6), A(:,98), n3(:,237), t3x432(:,:,98), nhel, den(202))
    call cont_QA(nsync, wf18(:,12), wf24(:,31), A(:,99), n3(:,238), t3x432(:,:,99), nhel, den(203))
    call cont_QA(nsync, wf18(:,48), wf24(:,32), A(:,100), n3(:,239), t3x432(:,:,100), nhel, den(204))
    call cont_VV(nsync, wf12(:,5), wf36(:,6), A(:,101), n3(:,240), t3x432(:,:,101), nhel, den(205))
    call cont_VV(nsync, wf36(:,4), wf12(:,6), A(:,102), n3(:,241), t3x432(:,:,102), nhel, den(206))
    call cont_QA(nsync, wf18(:,4), wf24(:,33), A(:,103), n3(:,242), t3x432(:,:,103), nhel, den(207))
    call cont_QA(nsync, wf18(:,48), wf24(:,34), A(:,104), n3(:,243), t3x432(:,:,104), nhel, den(208))
    call cont_QA(nsync, wf18(:,16), wf24(:,31), A(:,105), n3(:,244), t3x432(:,:,105), nhel, den(209))
    call cont_QA(nsync, wf18(:,52), wf24(:,35), A(:,106), n3(:,245), t3x432(:,:,106), nhel, den(210))
    call cont_QA(nsync, wf18(:,8), wf24(:,33), A(:,107), n3(:,246), t3x432(:,:,107), nhel, den(211))
    call cont_QA(nsync, wf18(:,52), wf24(:,36), A(:,108), n3(:,247), t3x432(:,:,108), nhel, den(212))
    call cont_QA(nsync, wf18(:,10), wf24(:,33), A(:,109), n3(:,248), t3x432(:,:,109), nhel, den(213))
    call cont_QA(nsync, wf18(:,20), wf24(:,31), A(:,110), n3(:,249), t3x432(:,:,110), nhel, den(214))
    call cont_VV(nsync, wf12(:,7), wf36(:,7), A(:,111), n3(:,250), t3x432(:,:,111), nhel, den(218))
    call cont_VV(nsync, wf12(:,8), wf36(:,8), A(:,112), n3(:,251), t3x432(:,:,112), nhel, den(222))
    call cont_QA(nsync, wf18(:,2), wf24(:,37), A(:,113), n3(:,252), t3x432(:,:,113), nhel, den(223))
    call cont_QA(nsync, wf18(:,46), wf24(:,38), A(:,114), n3(:,253), t3x432(:,:,114), nhel, den(224))
    call cont_VV(nsync, wf12(:,7), wf36(:,9), A(:,115), n3(:,254), t3x432(:,:,115), nhel, den(226))
    call cont_VV(nsync, wf36(:,8), wf12(:,9), A(:,116), n3(:,255), t3x432(:,:,116), nhel, den(228))
    call cont_QA(nsync, wf18(:,2), wf24(:,39), A(:,117), n3(:,256), t3x432(:,:,117), nhel, den(229))
    call cont_QA(nsync, wf18(:,40), wf24(:,40), A(:,118), n3(:,257), t3x432(:,:,118), nhel, den(230))
    call cont_VV(nsync, wf12(:,10), wf36(:,10), A(:,119), n3(:,258), t3x432(:,:,119), nhel, den(234))
    call cont_VV(nsync, wf36(:,10), wf12(:,11), A(:,120), n3(:,259), t3x432(:,:,120), nhel, den(236))
    call cont_QA(nsync, wf18(:,6), wf24(:,41), A(:,121), n3(:,260), t3x432(:,:,121), nhel, den(237))
    call cont_QA(nsync, wf18(:,50), wf24(:,38), A(:,122), n3(:,261), t3x432(:,:,122), nhel, den(238))
    call cont_QA(nsync, wf18(:,6), wf24(:,42), A(:,123), n3(:,262), t3x432(:,:,123), nhel, den(239))
    call cont_QA(nsync, wf18(:,44), wf24(:,40), A(:,124), n3(:,263), t3x432(:,:,124), nhel, den(240))
    call cont_VV(nsync, wf36(:,7), wf12(:,12), A(:,125), n3(:,264), t3x432(:,:,125), nhel, den(242))
    call cont_VV(nsync, wf12(:,8), wf36(:,11), A(:,126), n3(:,265), t3x432(:,:,126), nhel, den(244))
    call cont_QA(nsync, wf18(:,14), wf24(:,43), A(:,127), n3(:,266), t3x432(:,:,127), nhel, den(245))
    call cont_QA(nsync, wf18(:,46), wf24(:,44), A(:,128), n3(:,267), t3x432(:,:,128), nhel, den(246))
    call cont_VV(nsync, wf36(:,9), wf12(:,12), A(:,129), n3(:,268), t3x432(:,:,129), nhel, den(247))
    call cont_VV(nsync, wf12(:,9), wf36(:,11), A(:,130), n3(:,269), t3x432(:,:,130), nhel, den(248))
    call cont_QA(nsync, wf18(:,14), wf24(:,45), A(:,131), n3(:,270), t3x432(:,:,131), nhel, den(249))
    call cont_QA(nsync, wf18(:,40), wf24(:,46), A(:,132), n3(:,271), t3x432(:,:,132), nhel, den(250))
    call cont_VV(nsync, wf12(:,10), wf36(:,12), A(:,133), n3(:,272), t3x432(:,:,133), nhel, den(252))
    call cont_VV(nsync, wf12(:,11), wf36(:,12), A(:,134), n3(:,273), t3x432(:,:,134), nhel, den(253))
    call cont_QA(nsync, wf18(:,18), wf24(:,47), A(:,135), n3(:,274), t3x432(:,:,135), nhel, den(254))
    call cont_QA(nsync, wf18(:,50), wf24(:,44), A(:,136), n3(:,275), t3x432(:,:,136), nhel, den(255))
    call cont_QA(nsync, wf18(:,18), wf24(:,48), A(:,137), n3(:,276), t3x432(:,:,137), nhel, den(256))
    call cont_QA(nsync, wf18(:,44), wf24(:,46), A(:,138), n3(:,277), t3x432(:,:,138), nhel, den(257))
    call cont_QA(nsync, wf18(:,28), wf24(:,37), A(:,139), n3(:,278), t3x432(:,:,139), nhel, den(258))
    call cont_QA(nsync, wf18(:,54), wf24(:,29), A(:,140), n3(:,279), t3x432(:,:,140), nhel, den(259))
    call cont_QA(nsync, wf18(:,24), wf24(:,43), A(:,141), n3(:,280), t3x432(:,:,141), nhel, den(260))
    call cont_QA(nsync, wf18(:,54), wf24(:,30), A(:,142), n3(:,281), t3x432(:,:,142), nhel, den(261))
    call cont_QA(nsync, wf18(:,26), wf24(:,43), A(:,143), n3(:,282), t3x432(:,:,143), nhel, den(262))
    call cont_QA(nsync, wf18(:,32), wf24(:,37), A(:,144), n3(:,283), t3x432(:,:,144), nhel, den(263))
    call cont_QA(nsync, wf18(:,28), wf24(:,39), A(:,145), n3(:,284), t3x432(:,:,145), nhel, den(264))
    call cont_QA(nsync, wf18(:,60), wf24(:,35), A(:,146), n3(:,285), t3x432(:,:,146), nhel, den(265))
    call cont_QA(nsync, wf18(:,24), wf24(:,45), A(:,147), n3(:,286), t3x432(:,:,147), nhel, den(266))
    call cont_QA(nsync, wf18(:,60), wf24(:,36), A(:,148), n3(:,287), t3x432(:,:,148), nhel, den(267))
    call cont_QA(nsync, wf18(:,26), wf24(:,45), A(:,149), n3(:,288), t3x432(:,:,149), nhel, den(268))
    call cont_QA(nsync, wf18(:,32), wf24(:,39), A(:,150), n3(:,289), t3x432(:,:,150), nhel, den(269))
    call cont_QA(nsync, wf18(:,22), wf24(:,41), A(:,151), n3(:,290), t3x432(:,:,151), nhel, den(270))
    call cont_QA(nsync, wf18(:,58), wf24(:,26), A(:,152), n3(:,291), t3x432(:,:,152), nhel, den(271))
    call cont_QA(nsync, wf18(:,22), wf24(:,42), A(:,153), n3(:,292), t3x432(:,:,153), nhel, den(272))
    call cont_QA(nsync, wf18(:,56), wf24(:,32), A(:,154), n3(:,293), t3x432(:,:,154), nhel, den(273))
    call cont_QA(nsync, wf18(:,30), wf24(:,47), A(:,155), n3(:,294), t3x432(:,:,155), nhel, den(274))
    call cont_QA(nsync, wf18(:,58), wf24(:,28), A(:,156), n3(:,295), t3x432(:,:,156), nhel, den(275))
    call cont_QA(nsync, wf18(:,30), wf24(:,48), A(:,157), n3(:,296), t3x432(:,:,157), nhel, den(276))
    call cont_QA(nsync, wf18(:,56), wf24(:,34), A(:,158), n3(:,297), t3x432(:,:,158), nhel, den(277))
    call cont_QA(nsync, wf18(:,34), wf24(:,47), A(:,159), n3(:,298), t3x432(:,:,159), nhel, den(278))
    call cont_QA(nsync, wf18(:,36), wf24(:,41), A(:,160), n3(:,299), t3x432(:,:,160), nhel, den(279))
    call cont_QA(nsync, wf18(:,34), wf24(:,48), A(:,161), n3(:,300), t3x432(:,:,161), nhel, den(280))
    call cont_QA(nsync, wf18(:,36), wf24(:,42), A(:,162), n3(:,301), t3x432(:,:,162), nhel, den(281))

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
  complex(REALKIND), intent(out) :: M1(2) ! M1helarray(2,432)
  integer :: empty(0)

  M1(1) = ((-A(j,1)%j-A(j,2)%j-A(j,3)%j-A(j,4)%j-A(j,7)%j-A(j,8)%j-A(j,9)%j-A(j,10)%j-A(j,11)%j-A(j,12)%j-A(j,15)%j-A(j,16)%j &
       -A(j,17)%j-A(j,18)%j-A(j,21)%j-A(j,22)%j-A(j,23)%j-A(j,24)%j-A(j,27)%j-A(j,28)%j-A(j,37)%j-A(j,38)%j-A(j,39)%j-A(j,40)%j &
       -A(j,49)%j-A(j,50)%j-A(j,51)%j-A(j,52)%j-A(j,53)%j-A(j,54)%j-A(j,55)%j-A(j,56)%j-A(j,57)%j-A(j,58)%j-A(j,59)%j-A(j,60)%j &
       -A(j,61)%j-A(j,62)%j-A(j,63)%j-A(j,64)%j-A(j,65)%j-A(j,66)%j-A(j,67)%j-A(j,68)%j-A(j,71)%j-A(j,72)%j-A(j,73)%j-A(j,74)%j &
       -A(j,79)%j-A(j,80)%j-A(j,81)%j-A(j,82)%j-A(j,83)%j-A(j,84)%j-A(j,85)%j-A(j,86)%j-A(j,87)%j-A(j,88)%j-A(j,89)%j-A(j,90)%j &
       -A(j,91)%j-A(j,92)%j-A(j,95)%j-A(j,96)%j-A(j,97)%j-A(j,98)%j-A(j,99)%j-A(j,100)%j-A(j,101)%j-A(j,102)%j-A(j,103)%j &
       -A(j,104)%j-A(j,105)%j-A(j,106)%j-A(j,107)%j-A(j,108)%j-A(j,111)%j-A(j,112)%j-A(j,113)%j-A(j,114)%j-A(j,115)%j-A(j,116)%j &
       -A(j,117)%j-A(j,118)%j-A(j,119)%j-A(j,120)%j-A(j,121)%j-A(j,122)%j-A(j,123)%j-A(j,124)%j-A(j,125)%j-A(j,126)%j-A(j,127)%j &
       -A(j,128)%j-A(j,129)%j-A(j,130)%j-A(j,131)%j-A(j,132)%j-A(j,133)%j-A(j,134)%j-A(j,135)%j-A(j,136)%j-A(j,137)%j-A(j,138)%j &
       -A(j,139)%j-A(j,140)%j-A(j,141)%j-A(j,142)%j-A(j,145)%j-A(j,146)%j-A(j,147)%j-A(j,148)%j-A(j,151)%j-A(j,152)%j-A(j,153)%j &
       -A(j,154)%j-A(j,155)%j-A(j,156)%j-A(j,157)%j-A(j,158)%j)*f(1))/2._/**/REALKIND+((A(j,5)%j+A(j,6)%j+A(j,13)%j+A(j,14)%j &
       +A(j,19)%j+A(j,20)%j+A(j,25)%j+A(j,26)%j+A(j,29)%j+A(j,30)%j+A(j,31)%j+A(j,32)%j+A(j,34)%j+A(j,36)%j+A(j,42)%j+A(j,44)%j &
       +A(j,46)%j+A(j,48)%j+A(j,93)%j+A(j,94)%j+A(j,109)%j+A(j,110)%j+A(j,143)%j+A(j,144)%j+A(j,149)%j+A(j,150)%j+A(j,159)%j &
       +A(j,160)%j+A(j,161)%j+A(j,162)%j)*f(2))/2._/**/REALKIND+((A(j,33)%j+A(j,35)%j+A(j,41)%j+A(j,43)%j+A(j,45)%j+A(j,47)%j &
       +A(j,69)%j+A(j,70)%j+A(j,75)%j+A(j,76)%j+A(j,77)%j+A(j,78)%j)*f(3))/2._/**/REALKIND
  M1(2) = ((A(j,1)%j+A(j,2)%j+A(j,3)%j+A(j,4)%j+A(j,7)%j+A(j,8)%j+A(j,9)%j+A(j,10)%j+A(j,11)%j+A(j,12)%j+A(j,15)%j+A(j,16)%j &
       +A(j,17)%j+A(j,18)%j+A(j,21)%j+A(j,22)%j+A(j,23)%j+A(j,24)%j+A(j,27)%j+A(j,28)%j+A(j,37)%j+A(j,38)%j+A(j,39)%j+A(j,40)%j &
       +A(j,49)%j+A(j,50)%j+A(j,51)%j+A(j,52)%j+A(j,53)%j+A(j,54)%j+A(j,55)%j+A(j,56)%j+A(j,57)%j+A(j,58)%j+A(j,59)%j+A(j,60)%j &
       +A(j,61)%j+A(j,62)%j+A(j,63)%j+A(j,64)%j+A(j,65)%j+A(j,66)%j+A(j,67)%j+A(j,68)%j+A(j,71)%j+A(j,72)%j+A(j,73)%j+A(j,74)%j &
       +A(j,79)%j+A(j,80)%j+A(j,81)%j+A(j,82)%j+A(j,83)%j+A(j,84)%j+A(j,85)%j+A(j,86)%j+A(j,87)%j+A(j,88)%j+A(j,89)%j+A(j,90)%j &
       +A(j,91)%j+A(j,92)%j+A(j,95)%j+A(j,96)%j+A(j,97)%j+A(j,98)%j+A(j,99)%j+A(j,100)%j+A(j,101)%j+A(j,102)%j+A(j,103)%j &
       +A(j,104)%j+A(j,105)%j+A(j,106)%j+A(j,107)%j+A(j,108)%j+A(j,111)%j+A(j,112)%j+A(j,113)%j+A(j,114)%j+A(j,115)%j+A(j,116)%j &
       +A(j,117)%j+A(j,118)%j+A(j,119)%j+A(j,120)%j+A(j,121)%j+A(j,122)%j+A(j,123)%j+A(j,124)%j+A(j,125)%j+A(j,126)%j+A(j,127)%j &
       +A(j,128)%j+A(j,129)%j+A(j,130)%j+A(j,131)%j+A(j,132)%j+A(j,133)%j+A(j,134)%j+A(j,135)%j+A(j,136)%j+A(j,137)%j+A(j,138)%j &
       +A(j,139)%j+A(j,140)%j+A(j,141)%j+A(j,142)%j+A(j,145)%j+A(j,146)%j+A(j,147)%j+A(j,148)%j+A(j,151)%j+A(j,152)%j+A(j,153)%j &
       +A(j,154)%j+A(j,155)%j+A(j,156)%j+A(j,157)%j+A(j,158)%j)*f(1))/6._/**/REALKIND+((-A(j,5)%j-A(j,6)%j-A(j,13)%j-A(j,14)%j &
       -A(j,19)%j-A(j,20)%j-A(j,25)%j-A(j,26)%j-A(j,29)%j-A(j,30)%j-A(j,31)%j-A(j,32)%j-A(j,34)%j-A(j,36)%j-A(j,42)%j-A(j,44)%j &
       -A(j,46)%j-A(j,48)%j-A(j,93)%j-A(j,94)%j-A(j,109)%j-A(j,110)%j-A(j,143)%j-A(j,144)%j-A(j,149)%j-A(j,150)%j-A(j,159)%j &
       -A(j,160)%j-A(j,161)%j-A(j,162)%j)*f(2))/6._/**/REALKIND+((-A(j,33)%j-A(j,35)%j-A(j,41)%j-A(j,43)%j-A(j,45)%j-A(j,47)%j &
       -A(j,69)%j-A(j,70)%j-A(j,75)%j-A(j,76)%j-A(j,77)%j-A(j,78)%j)*f(3))/6._/**/REALKIND

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
  use ol_colourmatrix_ppvvvj_uuxbbxzzz_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(2)
  real(REALKIND),    intent(out) :: M2colint(0:30-1)
  integer,           intent(in)  :: extcombs(:)
  integer :: extcomb, colmatpos, i, j, eco

  M2colint = 0

  do eco = 1, size(extcombs)
    extcomb = extcombs(eco)
    colmatpos = 2*extcomb
    do i = 1, 2
      do j = 1, 2
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
  use ol_colourmatrix_ppvvvj_uuxbbxzzz_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(2)
  complex(REALKIND), intent(in)  :: M2(2)
  real(REALKIND),    intent(out) :: M2colint
  integer :: i, j

  M2colint = 0

  do i = 1, 2
    do j = 1, 2
      M2colint = M2colint + real(conjg(M1(i))*K1(i,j)*M2(j))
    end do
  end do

end subroutine colintmunu

end subroutine amp2


#ifdef PRECISION_dp
subroutine colourvector(M1out, nhelout) &
  & bind(c,name="ol_tree_colvect_ppvvvj_uuxbbxzzz_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(2,432)
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
    & bind(c,name="ol_f_amp2tree_ppvvvj_uuxbbxzzz_1")
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
    & bind(c,name="ol_f_amp2ccone_ppvvvj_uuxbbxzzz_1")
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
    & bind(c,name="ol_f_amp2ccall_ppvvvj_uuxbbxzzz_1")
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
    & bind(c,name="ol_f_amp2hcone_ppvvvj_uuxbbxzzz_1")
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
    & bind(c,name="ol_f_amp2hcall_ppvvvj_uuxbbxzzz_1")
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
    & bind(c,name="ol_amp2tree_ppvvvj_uuxbbxzzz_1")
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
    & bind(c,name="ol_amp2ccone_ppvvvj_uuxbbxzzz_1")
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
    & bind(c,name="ol_amp2ccall_ppvvvj_uuxbbxzzz_1")
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
    & bind(c,name="ol_amp2hcone_ppvvvj_uuxbbxzzz_1")
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
    & bind(c,name="ol_amp2hcall_ppvvvj_uuxbbxzzz_1")
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
    & bind(c,name="amp2tree_ppvvvj_uuxbbxzzz_1_")
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
    & bind(c,name="amp2ccone_ppvvvj_uuxbbxzzz_1_")
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
    & bind(c,name="amp2ccall_ppvvvj_uuxbbxzzz_1_")
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
    & bind(c,name="amp2hcone_ppvvvj_uuxbbxzzz_1_")
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
    & bind(c,name="amp2hcall_ppvvvj_uuxbbxzzz_1_")
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

end module ol_tree_ppvvvj_uuxbbxzzz_1_/**/REALKIND
