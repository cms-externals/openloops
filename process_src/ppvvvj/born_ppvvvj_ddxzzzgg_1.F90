
module ol_colourmatrix_ppvvvj_ddxzzzgg_1_/**/REALKIND
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

  K1( 1,:) = [  48,  -6]
  K1( 2,:) = [  -6,  48]
  K1( 3,:) = [  64,  -8]
  K1( 4,:) = [  -8,  64]
  K1( 5,:) = [  -1, -10]
  K1( 6,:) = [ -10,  -1]
  K1( 7,:) = [  64,  -8]
  K1( 8,:) = [  -8,  64]
  K1( 9,:) = [   0,   0]
  K1(10,:) = [   0,   0]
  K1(11,:) = [   0,   0]
  K1(12,:) = [   0,   0]
  K1(13,:) = [   0,   0]
  K1(14,:) = [   0,   0]
  K1(15,:) = [   0,   0]
  K1(16,:) = [   0,   0]
  K1(17,:) = [   0,   0]
  K1(18,:) = [   0,   0]
  K1(19,:) = [   0,   0]
  K1(20,:) = [   0,   0]
  K1(21,:) = [   0,   0]
  K1(22,:) = [   0,   0]
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
  K1(33,:) = [   9,   9]
  K1(34,:) = [   9, -72]
  K1(35,:) = [ -72,   9]
  K1(36,:) = [   9,   9]
  K1(37,:) = [   0,   0]
  K1(38,:) = [   0,   0]
  K1(39,:) = [   0,   0]
  K1(40,:) = [   0,   0]
  K1(41,:) = [   0,   0]
  K1(42,:) = [   0,   0]
  K1(43,:) = [ 144, -18]
  K1(44,:) = [ -18, 144]
  K1(45,:) = [ -72,   9]
  K1(46,:) = [   9,   9]
  K1(47,:) = [   9,   9]
  K1(48,:) = [   9, -72]
  K1(49,:) = [   0,   0]
  K1(50,:) = [   0,   0]
  K1(51,:) = [   0,   0]
  K1(52,:) = [   0,   0]
  K1(53,:) = [   0,   0]
  K1(54,:) = [   0,   0]
  K1(55,:) = [ -81,   0]
  K1(56,:) = [   0, -81]
  K1(57,:) = [ 144, -18]
  K1(58,:) = [ -18, 144]
  K1(59,:) = [   0,   0]
  K1(60,:) = [   0,   0]
  K1 = (1._/**/REALKIND / 9) * K1

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppvvvj_ddxzzzgg_1_/**/REALKIND



module ol_forced_parameters_ppvvvj_ddxzzzgg_1_/**/REALKIND
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
  if (wMZ /= 0) write(*,101) 'wMZ = 0'
  if (wMZ /= 0) write(*,101) 'wMZ = 0'
  if (wMZ /= 0) write(*,101) 'wMZ = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppvvvj_ddxzzzgg_1_/**/REALKIND

module ol_tree_ppvvvj_ddxzzzgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(4)
  complex(REALKIND), save :: den(256)
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
    f(2) = eQED**3*gQCD**2
    f(3) = (CI*eQED**3*gQCD**2*MW**2)/(cw**4*sw**2)
    f(4) = (eQED**3*gQCD**2*MW**2)/(cw**4*sw**2)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,5))
  den(2) = 1 / (Q(5,10))
  den(3) = 1 / (Q(5,96))
  den(5) = 1 / (Q(5,21))
  den(9) = 1 / (Q(5,26))
  den(12) = 1 / (Q(5,42))
  den(15) = 1 / (Q(5,74))
  den(18) = 1 / (Q(5,37))
  den(22) = 1 / (Q(5,69))
  den(26) = 1 / (Q(5,18))
  den(28) = 1 / (Q(5,13))
  den(33) = 1 / (Q(5,50))
  den(36) = 1 / (Q(5,82))
  den(43) = 1 / (Q(5,34))
  den(46) = 1 / (Q(5,98))
  den(54) = 1 / (Q(5,66))
  den(67) = 1 / (Q(5,9))
  den(68) = 1 / (Q(5,6))
  den(70) = 1 / (Q(5,25))
  den(74) = 1 / (Q(5,22))
  den(77) = 1 / (Q(5,38))
  den(80) = 1 / (Q(5,70))
  den(83) = 1 / (Q(5,41))
  den(87) = 1 / (Q(5,73))
  den(91) = 1 / (Q(5,17))
  den(95) = 1 / (Q(5,14))
  den(100) = 1 / (Q(5,49))
  den(104) = 1 / (Q(5,81))
  den(108) = 1 / (Q(5,33))
  den(115) = 1 / (Q(5,97))
  den(119) = 1 / (Q(5,65))
  den(184) = 1 / (Q(5,12) - MH2)
  den(186) = 1 / (Q(5,28) - MZ2)
  den(225) = 1 / (Q(5,20) - MH2)
  den(235) = 1 / (Q(5,24) - MH2)

  ! denominators

  den(4) = den(2)*den(3)
  den(6) = den(1)*den(5)
  den(7) = den(4)*den(6)
  den(8) = den(1)*den(3)
  den(10) = den(2)*den(9)
  den(11) = den(8)*den(10)
  den(13) = den(2)*den(12)
  den(14) = den(6)*den(13)
  den(16) = den(2)*den(15)
  den(17) = den(6)*den(16)
  den(19) = den(1)*den(18)
  den(20) = den(10)*den(19)
  den(21) = den(16)*den(19)
  den(23) = den(1)*den(22)
  den(24) = den(10)*den(23)
  den(25) = den(13)*den(23)
  den(27) = den(3)*den(26)
  den(29) = den(1)*den(28)
  den(30) = den(27)*den(29)
  den(31) = den(9)*den(26)
  den(32) = den(8)*den(31)
  den(34) = den(26)*den(33)
  den(35) = den(29)*den(34)
  den(37) = den(26)*den(36)
  den(38) = den(29)*den(37)
  den(39) = den(19)*den(31)
  den(40) = den(19)*den(37)
  den(41) = den(23)*den(31)
  den(42) = den(23)*den(34)
  den(44) = den(33)*den(43)
  den(45) = den(29)*den(44)
  den(47) = den(43)*den(46)
  den(48) = den(29)*den(47)
  den(49) = den(12)*den(43)
  den(50) = den(6)*den(49)
  den(51) = den(6)*den(47)
  den(52) = den(23)*den(49)
  den(53) = den(23)*den(44)
  den(55) = den(36)*den(54)
  den(56) = den(29)*den(55)
  den(57) = den(46)*den(54)
  den(58) = den(29)*den(57)
  den(59) = den(15)*den(54)
  den(60) = den(6)*den(59)
  den(61) = den(6)*den(57)
  den(62) = den(19)*den(59)
  den(63) = den(19)*den(55)
  den(64) = den(3)*den(46)
  den(65) = den(29)*den(64)
  den(66) = den(6)*den(64)
  den(69) = den(3)*den(68)
  den(71) = den(67)*den(70)
  den(72) = den(69)*den(71)
  den(73) = den(3)*den(67)
  den(75) = den(68)*den(74)
  den(76) = den(73)*den(75)
  den(78) = den(68)*den(77)
  den(79) = den(71)*den(78)
  den(81) = den(68)*den(80)
  den(82) = den(71)*den(81)
  den(84) = den(67)*den(83)
  den(85) = den(75)*den(84)
  den(86) = den(81)*den(84)
  den(88) = den(67)*den(87)
  den(89) = den(75)*den(88)
  den(90) = den(78)*den(88)
  den(92) = den(70)*den(91)
  den(93) = den(69)*den(92)
  den(94) = den(3)*den(91)
  den(96) = den(68)*den(95)
  den(97) = den(94)*den(96)
  den(98) = den(78)*den(92)
  den(99) = den(81)*den(92)
  den(101) = den(91)*den(100)
  den(102) = den(96)*den(101)
  den(103) = den(81)*den(101)
  den(105) = den(91)*den(104)
  den(106) = den(96)*den(105)
  den(107) = den(78)*den(105)
  den(109) = den(83)*den(108)
  den(110) = den(75)*den(109)
  den(111) = den(81)*den(109)
  den(112) = den(100)*den(108)
  den(113) = den(96)*den(112)
  den(114) = den(81)*den(112)
  den(116) = den(108)*den(115)
  den(117) = den(96)*den(116)
  den(118) = den(75)*den(116)
  den(120) = den(87)*den(119)
  den(121) = den(75)*den(120)
  den(122) = den(78)*den(120)
  den(123) = den(104)*den(119)
  den(124) = den(96)*den(123)
  den(125) = den(78)*den(123)
  den(126) = den(115)*den(119)
  den(127) = den(96)*den(126)
  den(128) = den(75)*den(126)
  den(129) = den(3)*den(115)
  den(130) = den(75)*den(129)
  den(131) = den(96)*den(129)
  den(132) = den(28)*den(67)
  den(133) = den(27)*den(132)
  den(134) = den(26)*den(74)
  den(135) = den(73)*den(134)
  den(136) = den(34)*den(132)
  den(137) = den(37)*den(132)
  den(138) = den(84)*den(134)
  den(139) = den(37)*den(84)
  den(140) = den(88)*den(134)
  den(141) = den(34)*den(88)
  den(142) = den(44)*den(132)
  den(143) = den(47)*den(132)
  den(144) = den(43)*den(77)
  den(145) = den(71)*den(144)
  den(146) = den(47)*den(71)
  den(147) = den(88)*den(144)
  den(148) = den(44)*den(88)
  den(149) = den(55)*den(132)
  den(150) = den(57)*den(132)
  den(151) = den(54)*den(80)
  den(152) = den(71)*den(151)
  den(153) = den(57)*den(71)
  den(154) = den(84)*den(151)
  den(155) = den(55)*den(84)
  den(156) = den(64)*den(132)
  den(157) = den(64)*den(71)
  den(158) = den(5)*den(91)
  den(159) = den(4)*den(158)
  den(160) = den(2)*den(95)
  den(161) = den(94)*den(160)
  den(162) = den(13)*den(158)
  den(163) = den(16)*den(158)
  den(164) = den(101)*den(160)
  den(165) = den(16)*den(101)
  den(166) = den(105)*den(160)
  den(167) = den(13)*den(105)
  den(168) = den(18)*den(108)
  den(169) = den(10)*den(168)
  den(170) = den(16)*den(168)
  den(171) = den(112)*den(160)
  den(172) = den(16)*den(112)
  den(173) = den(116)*den(160)
  den(174) = den(10)*den(116)
  den(175) = den(22)*den(119)
  den(176) = den(10)*den(175)
  den(177) = den(13)*den(175)
  den(178) = den(123)*den(160)
  den(179) = den(13)*den(123)
  den(180) = den(126)*den(160)
  den(181) = den(10)*den(126)
  den(182) = den(10)*den(129)
  den(183) = den(129)*den(160)
  den(185) = den(54)*den(108)
  den(187) = den(184)*den(186)
  den(188) = den(185)*den(187)
  den(189) = den(116)*den(187)
  den(190) = den(43)*den(119)
  den(191) = den(187)*den(190)
  den(192) = den(47)*den(187)
  den(193) = den(126)*den(187)
  den(194) = den(57)*den(187)
  den(195) = den(64)*den(187)
  den(196) = den(129)*den(187)
  den(197) = den(49)*den(158)
  den(198) = den(47)*den(158)
  den(199) = den(92)*den(144)
  den(200) = den(47)*den(92)
  den(201) = den(105)*den(144)
  den(202) = den(49)*den(105)
  den(203) = den(59)*den(158)
  den(204) = den(57)*den(158)
  den(205) = den(92)*den(151)
  den(206) = den(57)*den(92)
  den(207) = den(101)*den(151)
  den(208) = den(59)*den(101)
  den(209) = den(64)*den(158)
  den(210) = den(64)*den(92)
  den(211) = den(31)*den(168)
  den(212) = den(37)*den(168)
  den(213) = den(109)*den(134)
  den(214) = den(37)*den(109)
  den(215) = den(116)*den(134)
  den(216) = den(31)*den(116)
  den(217) = den(31)*den(175)
  den(218) = den(34)*den(175)
  den(219) = den(120)*den(134)
  den(220) = den(34)*den(120)
  den(221) = den(126)*den(134)
  den(222) = den(31)*den(126)
  den(223) = den(31)*den(129)
  den(224) = den(129)*den(134)
  den(226) = den(186)*den(225)
  den(227) = den(185)*den(226)
  den(228) = den(116)*den(226)
  den(229) = den(190)*den(226)
  den(230) = den(47)*den(226)
  den(231) = den(126)*den(226)
  den(232) = den(57)*den(226)
  den(233) = den(64)*den(226)
  den(234) = den(129)*den(226)
  den(236) = den(186)*den(235)
  den(237) = den(185)*den(236)
  den(238) = den(116)*den(236)
  den(239) = den(190)*den(236)
  den(240) = den(47)*den(236)
  den(241) = den(126)*den(236)
  den(242) = den(57)*den(236)
  den(243) = den(64)*den(236)
  den(244) = den(129)*den(236)
  den(245) = den(59)*den(168)
  den(246) = den(55)*den(168)
  den(247) = den(109)*den(151)
  den(248) = den(55)*den(109)
  den(249) = den(112)*den(151)
  den(250) = den(59)*den(112)
  den(251) = den(49)*den(175)
  den(252) = den(44)*den(175)
  den(253) = den(120)*den(144)
  den(254) = den(44)*den(120)
  den(255) = den(123)*den(144)
  den(256) = den(49)*den(123)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_ppvvvj_ddxzzzgg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_ppvvvj_ddxzzzgg_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for down anti-down Z Z Z glue glue -> 0
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
  use ol_external_ppvvvj_ddxzzzgg_1, only: external_perm_ppvvvj_ddxzzzgg_1, &
    & external_perm_inv_ppvvvj_ddxzzzgg_1, extcomb_perm_ppvvvj_ddxzzzgg_1, &
    & average_factor_ppvvvj_ddxzzzgg_1
  use ol_external_ppvvvj_ddxzzzgg_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_ppvvvj_ddxzzzgg_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_ppvvvj_ddxzzzgg_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_ppvvvj_ddxzzzgg_1
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
  type(wfun) :: ex1(2), ex2(2), ex3(3), ex4(3), ex5(3), ex6(2), ex7(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,9), wf6(6,12), wf8(8,12), wf9(9,3), wf12(12,48), wf16(16,4), wf18(18,24), wf24(24,20), wf27(27,6), &
    wf36(36,60), wf54(54,18), wf432(432,168)

  type(polcont) :: A(432,168)
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
  extmasses2 = [ rZERO2, rZERO2, rMZ2, rMZ2, rMZ2, rZERO2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_ppvvvj_ddxzzzgg_1,7)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,7)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_ppvvvj_ddxzzzgg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_ppvvvj_ddxzzzgg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_ppvvvj_ddxzzzgg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_Q(P(:,1), rZERO, H1, ex1)
  call wf_A(P(:,2), rZERO, H2, ex2)
  call wf_V(P(:,3), rMZ, H3, ex3)
  call wf_V(P(:,4), rMZ, H4, ex4)
  call wf_V(P(:,5), rMZ, H5, ex5)
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
  call vert_ZQ_A(gZd,ntry, ex3, ex1, wf6(:,1), n3(:,1), t3x6(:,:,1))
  call vert_AZ_Q(gZd,ntry, ex2, ex4, wf6(:,2), n3(:,2), t3x6(:,:,2))
  call vert_UV_W(ntry, ex6, Q(:,32), ex7, Q(:,64), wf4(:,1), n3(:,3), t3x4(:,:,1))
  call prop_Q_A(ntry, wf6(:,1), Q(:,5), ZERO, 0_intkind1, wf6(:,3), n2(1))
  call prop_A_Q(ntry, wf6(:,2), Q(:,10), ZERO, 0_intkind1, wf6(:,4), n2(2))
  call vert_ZQ_A(gZd,ntry, ex5, wf6(:,3), wf18(:,1), n3(:,4), t3x18(:,:,1))
  call vert_AV_Q(ntry, wf6(:,4), wf4(:,1), wf24(:,1), n3(:,5), t3x24(:,:,1))
  call prop_Q_A(ntry, wf18(:,1), Q(:,21), ZERO, 0_intkind1, wf18(:,2), n2(3))
  call vert_AZ_Q(gZd,ntry, wf6(:,4), ex5, wf18(:,3), n3(:,6), t3x18(:,:,2))
  call vert_VQ_A(ntry, wf4(:,1), wf6(:,3), wf24(:,2), n3(:,7), t3x24(:,:,2))
  call prop_A_Q(ntry, wf18(:,3), Q(:,26), ZERO, 0_intkind1, wf18(:,4), n2(4))
  call vert_AV_Q(ntry, wf6(:,4), ex6, wf12(:,1), n3(:,8), t3x12(:,:,1))
  call prop_A_Q(ntry, wf12(:,1), Q(:,42), ZERO, 0_intkind1, wf12(:,2), n2(5))
  call vert_VQ_A(ntry, ex7, wf18(:,2), wf36(:,1), n3(:,9), t3x36(:,:,1))
  call vert_AV_Q(ntry, wf6(:,4), ex7, wf12(:,3), n3(:,10), t3x12(:,:,2))
  call prop_A_Q(ntry, wf12(:,3), Q(:,74), ZERO, 0_intkind1, wf12(:,4), n2(6))
  call vert_VQ_A(ntry, ex6, wf18(:,2), wf36(:,2), n3(:,11), t3x36(:,:,2))
  call vert_VQ_A(ntry, ex6, wf6(:,3), wf12(:,5), n3(:,12), t3x12(:,:,3))
  call prop_Q_A(ntry, wf12(:,5), Q(:,37), ZERO, 0_intkind1, wf12(:,6), n2(7))
  call vert_AV_Q(ntry, wf18(:,4), ex7, wf36(:,3), n3(:,13), t3x36(:,:,3))
  call vert_ZQ_A(gZd,ntry, ex5, wf12(:,6), wf36(:,4), n3(:,14), t3x36(:,:,4))
  call vert_VQ_A(ntry, ex7, wf6(:,3), wf12(:,7), n3(:,15), t3x12(:,:,4))
  call prop_Q_A(ntry, wf12(:,7), Q(:,69), ZERO, 0_intkind1, wf12(:,8), n2(8))
  call vert_AV_Q(ntry, wf18(:,4), ex6, wf36(:,5), n3(:,16), t3x36(:,:,5))
  call vert_AZ_Q(gZd,ntry, wf12(:,2), ex5, wf36(:,6), n3(:,17), t3x36(:,:,6))
  call vert_AZ_Q(gZd,ntry, ex2, ex5, wf6(:,5), n3(:,18), t3x6(:,:,3))
  call prop_A_Q(ntry, wf6(:,5), Q(:,18), ZERO, 0_intkind1, wf6(:,6), n2(9))
  call vert_ZQ_A(gZd,ntry, ex4, wf6(:,3), wf18(:,5), n3(:,19), t3x18(:,:,3))
  call vert_AV_Q(ntry, wf6(:,6), wf4(:,1), wf24(:,3), n3(:,20), t3x24(:,:,3))
  call prop_Q_A(ntry, wf18(:,5), Q(:,13), ZERO, 0_intkind1, wf18(:,6), n2(10))
  call vert_AZ_Q(gZd,ntry, wf6(:,6), ex4, wf18(:,7), n3(:,21), t3x18(:,:,4))
  call prop_A_Q(ntry, wf18(:,7), Q(:,26), ZERO, 0_intkind1, wf18(:,8), n2(11))
  call vert_AV_Q(ntry, wf6(:,6), ex6, wf12(:,9), n3(:,22), t3x12(:,:,5))
  call prop_A_Q(ntry, wf12(:,9), Q(:,50), ZERO, 0_intkind1, wf12(:,10), n2(12))
  call vert_VQ_A(ntry, ex7, wf18(:,6), wf36(:,7), n3(:,23), t3x36(:,:,7))
  call vert_AV_Q(ntry, wf6(:,6), ex7, wf12(:,11), n3(:,24), t3x12(:,:,6))
  call prop_A_Q(ntry, wf12(:,11), Q(:,82), ZERO, 0_intkind1, wf12(:,12), n2(13))
  call vert_VQ_A(ntry, ex6, wf18(:,6), wf36(:,8), n3(:,25), t3x36(:,:,8))
  call vert_AV_Q(ntry, wf18(:,8), ex7, wf36(:,9), n3(:,26), t3x36(:,:,9))
  call vert_ZQ_A(gZd,ntry, ex4, wf12(:,6), wf36(:,10), n3(:,27), t3x36(:,:,10))
  call vert_AV_Q(ntry, wf18(:,8), ex6, wf36(:,11), n3(:,28), t3x36(:,:,11))
  call vert_AZ_Q(gZd,ntry, wf12(:,10), ex4, wf36(:,12), n3(:,29), t3x36(:,:,12))
  call vert_AV_Q(ntry, ex2, ex6, wf4(:,2), n3(:,30), t3x4(:,:,2))
  call prop_A_Q(ntry, wf4(:,2), Q(:,34), ZERO, 0_intkind1, wf4(:,3), n2(14))
  call vert_AZ_Q(gZd,ntry, wf4(:,3), ex5, wf12(:,13), n3(:,31), t3x12(:,:,7))
  call prop_A_Q(ntry, wf12(:,13), Q(:,50), ZERO, 0_intkind1, wf12(:,14), n2(15))
  call vert_AV_Q(ntry, wf4(:,3), ex7, wf8(:,1), n3(:,32), t3x8(:,:,1))
  call prop_A_Q(ntry, wf8(:,1), Q(:,98), ZERO, 0_intkind1, wf8(:,2), n2(16))
  call vert_ZQ_A(gZd,ntry, ex5, wf18(:,6), wf54(:,1), n3(:,33), t3x54(:,:,1))
  call vert_AZ_Q(gZd,ntry, wf4(:,3), ex4, wf12(:,15), n3(:,34), t3x12(:,:,8))
  call prop_A_Q(ntry, wf12(:,15), Q(:,42), ZERO, 0_intkind1, wf12(:,16), n2(17))
  call vert_AV_Q(ntry, wf12(:,16), ex7, wf24(:,4), n3(:,35), t3x24(:,:,4))
  call vert_ZQ_A(gZd,ntry, ex4, wf18(:,2), wf54(:,2), n3(:,36), t3x54(:,:,2))
  call vert_AZ_Q(gZd,ntry, wf12(:,16), ex5, wf36(:,13), n3(:,37), t3x36(:,:,13))
  call vert_AZ_Q(gZd,ntry, wf12(:,14), ex4, wf36(:,14), n3(:,38), t3x36(:,:,14))
  call vert_AV_Q(ntry, ex2, ex7, wf4(:,4), n3(:,39), t3x4(:,:,3))
  call prop_A_Q(ntry, wf4(:,4), Q(:,66), ZERO, 0_intkind1, wf4(:,5), n2(18))
  call vert_AZ_Q(gZd,ntry, wf4(:,5), ex5, wf12(:,17), n3(:,40), t3x12(:,:,9))
  call prop_A_Q(ntry, wf12(:,17), Q(:,82), ZERO, 0_intkind1, wf12(:,18), n2(19))
  call vert_AV_Q(ntry, wf4(:,5), ex6, wf8(:,3), n3(:,41), t3x8(:,:,2))
  call prop_A_Q(ntry, wf8(:,3), Q(:,98), ZERO, 0_intkind1, wf8(:,4), n2(20))
  call vert_AZ_Q(gZd,ntry, wf4(:,5), ex4, wf12(:,19), n3(:,42), t3x12(:,:,10))
  call prop_A_Q(ntry, wf12(:,19), Q(:,74), ZERO, 0_intkind1, wf12(:,20), n2(21))
  call vert_AV_Q(ntry, wf12(:,20), ex6, wf24(:,5), n3(:,43), t3x24(:,:,5))
  call vert_AZ_Q(gZd,ntry, wf12(:,20), ex5, wf36(:,15), n3(:,44), t3x36(:,:,15))
  call vert_AZ_Q(gZd,ntry, wf12(:,18), ex4, wf36(:,16), n3(:,45), t3x36(:,:,16))
  call vert_AV_Q(ntry, ex2, wf4(:,1), wf8(:,5), n3(:,46), t3x8(:,:,3))
  call prop_A_Q(ntry, wf8(:,5), Q(:,98), ZERO, 0_intkind1, wf8(:,6), n2(22))
  call vert_AZ_Q(gZd,ntry, wf8(:,6), ex5, wf24(:,6), n3(:,47), t3x24(:,:,6))
  call vert_AZ_Q(gZd,ntry, wf8(:,6), ex4, wf24(:,7), n3(:,48), t3x24(:,:,7))
  call vert_ZQ_A(gZd,ntry, ex4, ex1, wf6(:,7), n3(:,49), t3x6(:,:,4))
  call vert_AZ_Q(gZd,ntry, ex2, ex3, wf6(:,8), n3(:,50), t3x6(:,:,5))
  call prop_Q_A(ntry, wf6(:,7), Q(:,9), ZERO, 0_intkind1, wf6(:,9), n2(23))
  call prop_A_Q(ntry, wf6(:,8), Q(:,6), ZERO, 0_intkind1, wf6(:,10), n2(24))
  call vert_ZQ_A(gZd,ntry, ex5, wf6(:,9), wf18(:,9), n3(:,51), t3x18(:,:,5))
  call vert_AV_Q(ntry, wf6(:,10), wf4(:,1), wf24(:,8), n3(:,52), t3x24(:,:,8))
  call prop_Q_A(ntry, wf18(:,9), Q(:,25), ZERO, 0_intkind1, wf18(:,10), n2(25))
  call vert_AZ_Q(gZd,ntry, wf6(:,10), ex5, wf18(:,11), n3(:,53), t3x18(:,:,6))
  call vert_VQ_A(ntry, wf4(:,1), wf6(:,9), wf24(:,9), n3(:,54), t3x24(:,:,9))
  call prop_A_Q(ntry, wf18(:,11), Q(:,22), ZERO, 0_intkind1, wf18(:,12), n2(26))
  call vert_AV_Q(ntry, wf6(:,10), ex6, wf12(:,21), n3(:,55), t3x12(:,:,11))
  call prop_A_Q(ntry, wf12(:,21), Q(:,38), ZERO, 0_intkind1, wf12(:,22), n2(27))
  call vert_VQ_A(ntry, ex7, wf18(:,10), wf36(:,17), n3(:,56), t3x36(:,:,17))
  call vert_AV_Q(ntry, wf6(:,10), ex7, wf12(:,23), n3(:,57), t3x12(:,:,12))
  call prop_A_Q(ntry, wf12(:,23), Q(:,70), ZERO, 0_intkind1, wf12(:,24), n2(28))
  call vert_VQ_A(ntry, ex6, wf18(:,10), wf36(:,18), n3(:,58), t3x36(:,:,18))
  call vert_VQ_A(ntry, ex6, wf6(:,9), wf12(:,25), n3(:,59), t3x12(:,:,13))
  call prop_Q_A(ntry, wf12(:,25), Q(:,41), ZERO, 0_intkind1, wf12(:,26), n2(29))
  call vert_AV_Q(ntry, wf18(:,12), ex7, wf36(:,19), n3(:,60), t3x36(:,:,19))
  call vert_ZQ_A(gZd,ntry, ex5, wf12(:,26), wf36(:,20), n3(:,61), t3x36(:,:,20))
  call vert_VQ_A(ntry, ex7, wf6(:,9), wf12(:,27), n3(:,62), t3x12(:,:,14))
  call prop_Q_A(ntry, wf12(:,27), Q(:,73), ZERO, 0_intkind1, wf12(:,28), n2(30))
  call vert_AV_Q(ntry, wf18(:,12), ex6, wf36(:,21), n3(:,63), t3x36(:,:,21))
  call vert_AZ_Q(gZd,ntry, wf12(:,22), ex5, wf36(:,22), n3(:,64), t3x36(:,:,22))
  call vert_ZQ_A(gZd,ntry, ex5, ex1, wf6(:,11), n3(:,65), t3x6(:,:,6))
  call prop_Q_A(ntry, wf6(:,11), Q(:,17), ZERO, 0_intkind1, wf6(:,12), n2(31))
  call vert_ZQ_A(gZd,ntry, ex4, wf6(:,12), wf18(:,13), n3(:,66), t3x18(:,:,7))
  call prop_Q_A(ntry, wf18(:,13), Q(:,25), ZERO, 0_intkind1, wf18(:,14), n2(32))
  call vert_AZ_Q(gZd,ntry, wf6(:,10), ex4, wf18(:,15), n3(:,67), t3x18(:,:,8))
  call vert_VQ_A(ntry, wf4(:,1), wf6(:,12), wf24(:,10), n3(:,68), t3x24(:,:,10))
  call prop_A_Q(ntry, wf18(:,15), Q(:,14), ZERO, 0_intkind1, wf18(:,16), n2(33))
  call vert_VQ_A(ntry, ex7, wf18(:,14), wf36(:,23), n3(:,69), t3x36(:,:,23))
  call vert_VQ_A(ntry, ex6, wf18(:,14), wf36(:,24), n3(:,70), t3x36(:,:,24))
  call vert_VQ_A(ntry, ex6, wf6(:,12), wf12(:,29), n3(:,71), t3x12(:,:,15))
  call prop_Q_A(ntry, wf12(:,29), Q(:,49), ZERO, 0_intkind1, wf12(:,30), n2(34))
  call vert_AV_Q(ntry, wf18(:,16), ex7, wf36(:,25), n3(:,72), t3x36(:,:,25))
  call vert_ZQ_A(gZd,ntry, ex4, wf12(:,30), wf36(:,26), n3(:,73), t3x36(:,:,26))
  call vert_VQ_A(ntry, ex7, wf6(:,12), wf12(:,31), n3(:,74), t3x12(:,:,16))
  call prop_Q_A(ntry, wf12(:,31), Q(:,81), ZERO, 0_intkind1, wf12(:,32), n2(35))
  call vert_AV_Q(ntry, wf18(:,16), ex6, wf36(:,27), n3(:,75), t3x36(:,:,27))
  call vert_AZ_Q(gZd,ntry, wf12(:,22), ex4, wf36(:,28), n3(:,76), t3x36(:,:,28))
  call vert_VQ_A(ntry, ex6, ex1, wf4(:,6), n3(:,77), t3x4(:,:,4))
  call prop_Q_A(ntry, wf4(:,6), Q(:,33), ZERO, 0_intkind1, wf4(:,7), n2(36))
  call vert_ZQ_A(gZd,ntry, ex4, wf4(:,7), wf12(:,33), n3(:,78), t3x12(:,:,17))
  call prop_Q_A(ntry, wf12(:,33), Q(:,41), ZERO, 0_intkind1, wf12(:,34), n2(37))
  call vert_VQ_A(ntry, ex7, wf12(:,34), wf24(:,11), n3(:,79), t3x24(:,:,11))
  call vert_ZQ_A(gZd,ntry, ex5, wf12(:,34), wf36(:,29), n3(:,80), t3x36(:,:,29))
  call vert_ZQ_A(gZd,ntry, ex5, wf4(:,7), wf12(:,35), n3(:,81), t3x12(:,:,18))
  call prop_Q_A(ntry, wf12(:,35), Q(:,49), ZERO, 0_intkind1, wf12(:,36), n2(38))
  call vert_ZQ_A(gZd,ntry, ex4, wf12(:,36), wf36(:,30), n3(:,82), t3x36(:,:,30))
  call vert_VQ_A(ntry, ex7, wf4(:,7), wf8(:,7), n3(:,83), t3x8(:,:,4))
  call prop_Q_A(ntry, wf8(:,7), Q(:,97), ZERO, 0_intkind1, wf8(:,8), n2(39))
  call vert_AZ_Q(gZd,ntry, wf18(:,16), ex5, wf54(:,3), n3(:,84), t3x54(:,:,3))
  call vert_AZ_Q(gZd,ntry, wf18(:,12), ex4, wf54(:,4), n3(:,85), t3x54(:,:,4))
  call vert_VQ_A(ntry, ex7, ex1, wf4(:,8), n3(:,86), t3x4(:,:,5))
  call prop_Q_A(ntry, wf4(:,8), Q(:,65), ZERO, 0_intkind1, wf4(:,9), n2(40))
  call vert_ZQ_A(gZd,ntry, ex4, wf4(:,9), wf12(:,37), n3(:,87), t3x12(:,:,19))
  call prop_Q_A(ntry, wf12(:,37), Q(:,73), ZERO, 0_intkind1, wf12(:,38), n2(41))
  call vert_VQ_A(ntry, ex6, wf12(:,38), wf24(:,12), n3(:,88), t3x24(:,:,12))
  call vert_ZQ_A(gZd,ntry, ex5, wf12(:,38), wf36(:,31), n3(:,89), t3x36(:,:,31))
  call vert_ZQ_A(gZd,ntry, ex5, wf4(:,9), wf12(:,39), n3(:,90), t3x12(:,:,20))
  call prop_Q_A(ntry, wf12(:,39), Q(:,81), ZERO, 0_intkind1, wf12(:,40), n2(42))
  call vert_ZQ_A(gZd,ntry, ex4, wf12(:,40), wf36(:,32), n3(:,91), t3x36(:,:,32))
  call vert_VQ_A(ntry, ex6, wf4(:,9), wf8(:,9), n3(:,92), t3x8(:,:,5))
  call prop_Q_A(ntry, wf8(:,9), Q(:,97), ZERO, 0_intkind1, wf8(:,10), n2(43))
  call vert_VQ_A(ntry, wf4(:,1), ex1, wf8(:,11), n3(:,93), t3x8(:,:,6))
  call prop_Q_A(ntry, wf8(:,11), Q(:,97), ZERO, 0_intkind1, wf8(:,12), n2(44))
  call vert_ZQ_A(gZd,ntry, ex4, wf8(:,12), wf24(:,13), n3(:,94), t3x24(:,:,13))
  call vert_ZQ_A(gZd,ntry, ex5, wf8(:,12), wf24(:,14), n3(:,95), t3x24(:,:,14))
  call vert_ZQ_A(gZd,ntry, ex3, wf6(:,9), wf18(:,17), n3(:,96), t3x18(:,:,9))
  call prop_Q_A(ntry, wf18(:,17), Q(:,13), ZERO, 0_intkind1, wf18(:,18), n2(45))
  call vert_AZ_Q(gZd,ntry, wf6(:,6), ex3, wf18(:,19), n3(:,97), t3x18(:,:,10))
  call prop_A_Q(ntry, wf18(:,19), Q(:,22), ZERO, 0_intkind1, wf18(:,20), n2(46))
  call vert_VQ_A(ntry, ex7, wf18(:,18), wf36(:,33), n3(:,98), t3x36(:,:,33))
  call vert_VQ_A(ntry, ex6, wf18(:,18), wf36(:,34), n3(:,99), t3x36(:,:,34))
  call vert_AV_Q(ntry, wf18(:,20), ex7, wf36(:,35), n3(:,100), t3x36(:,:,35))
  call vert_ZQ_A(gZd,ntry, ex3, wf12(:,26), wf36(:,36), n3(:,101), t3x36(:,:,36))
  call vert_AV_Q(ntry, wf18(:,20), ex6, wf36(:,37), n3(:,102), t3x36(:,:,37))
  call vert_AZ_Q(gZd,ntry, wf12(:,10), ex3, wf36(:,38), n3(:,103), t3x36(:,:,38))
  call vert_ZQ_A(gZd,ntry, ex5, wf18(:,18), wf54(:,5), n3(:,104), t3x54(:,:,5))
  call vert_AZ_Q(gZd,ntry, wf4(:,3), ex3, wf12(:,41), n3(:,105), t3x12(:,:,21))
  call prop_A_Q(ntry, wf12(:,41), Q(:,38), ZERO, 0_intkind1, wf12(:,42), n2(47))
  call vert_AV_Q(ntry, wf12(:,42), ex7, wf24(:,15), n3(:,106), t3x24(:,:,15))
  call vert_ZQ_A(gZd,ntry, ex3, wf18(:,10), wf54(:,6), n3(:,107), t3x54(:,:,6))
  call vert_AZ_Q(gZd,ntry, wf12(:,42), ex5, wf36(:,39), n3(:,108), t3x36(:,:,39))
  call vert_AZ_Q(gZd,ntry, wf12(:,14), ex3, wf36(:,40), n3(:,109), t3x36(:,:,40))
  call vert_AZ_Q(gZd,ntry, wf4(:,5), ex3, wf12(:,43), n3(:,110), t3x12(:,:,22))
  call prop_A_Q(ntry, wf12(:,43), Q(:,70), ZERO, 0_intkind1, wf12(:,44), n2(48))
  call vert_AV_Q(ntry, wf12(:,44), ex6, wf24(:,16), n3(:,111), t3x24(:,:,16))
  call vert_AZ_Q(gZd,ntry, wf12(:,44), ex5, wf36(:,41), n3(:,112), t3x36(:,:,41))
  call vert_AZ_Q(gZd,ntry, wf12(:,18), ex3, wf36(:,42), n3(:,113), t3x36(:,:,42))
  call vert_AZ_Q(gZd,ntry, wf8(:,6), ex3, wf24(:,17), n3(:,114), t3x24(:,:,17))
  call vert_ZQ_A(gZd,ntry, ex3, wf6(:,12), wf18(:,21), n3(:,115), t3x18(:,:,11))
  call prop_Q_A(ntry, wf18(:,21), Q(:,21), ZERO, 0_intkind1, wf18(:,22), n2(49))
  call vert_AZ_Q(gZd,ntry, wf6(:,4), ex3, wf18(:,23), n3(:,116), t3x18(:,:,12))
  call prop_A_Q(ntry, wf18(:,23), Q(:,14), ZERO, 0_intkind1, wf18(:,24), n2(50))
  call vert_VQ_A(ntry, ex7, wf18(:,22), wf36(:,43), n3(:,117), t3x36(:,:,43))
  call vert_VQ_A(ntry, ex6, wf18(:,22), wf36(:,44), n3(:,118), t3x36(:,:,44))
  call vert_AV_Q(ntry, wf18(:,24), ex7, wf36(:,45), n3(:,119), t3x36(:,:,45))
  call vert_ZQ_A(gZd,ntry, ex3, wf12(:,30), wf36(:,46), n3(:,120), t3x36(:,:,46))
  call vert_AV_Q(ntry, wf18(:,24), ex6, wf36(:,47), n3(:,121), t3x36(:,:,47))
  call vert_AZ_Q(gZd,ntry, wf12(:,2), ex3, wf36(:,48), n3(:,122), t3x36(:,:,48))
  call vert_ZQ_A(gZd,ntry, ex3, wf4(:,7), wf12(:,45), n3(:,123), t3x12(:,:,23))
  call prop_Q_A(ntry, wf12(:,45), Q(:,37), ZERO, 0_intkind1, wf12(:,46), n2(51))
  call vert_VQ_A(ntry, ex7, wf12(:,46), wf24(:,18), n3(:,124), t3x24(:,:,18))
  call vert_ZQ_A(gZd,ntry, ex5, wf12(:,46), wf36(:,49), n3(:,125), t3x36(:,:,49))
  call vert_ZQ_A(gZd,ntry, ex3, wf12(:,36), wf36(:,50), n3(:,126), t3x36(:,:,50))
  call vert_AZ_Q(gZd,ntry, wf18(:,24), ex5, wf54(:,7), n3(:,127), t3x54(:,:,7))
  call vert_AZ_Q(gZd,ntry, wf18(:,4), ex3, wf54(:,8), n3(:,128), t3x54(:,:,8))
  call vert_ZQ_A(gZd,ntry, ex3, wf4(:,9), wf12(:,47), n3(:,129), t3x12(:,:,24))
  call prop_Q_A(ntry, wf12(:,47), Q(:,69), ZERO, 0_intkind1, wf12(:,48), n2(52))
  call vert_VQ_A(ntry, ex6, wf12(:,48), wf24(:,19), n3(:,130), t3x24(:,:,19))
  call vert_ZQ_A(gZd,ntry, ex5, wf12(:,48), wf36(:,51), n3(:,131), t3x36(:,:,51))
  call vert_ZQ_A(gZd,ntry, ex3, wf12(:,40), wf36(:,52), n3(:,132), t3x36(:,:,52))
  call vert_ZQ_A(gZd,ntry, ex3, wf8(:,12), wf24(:,20), n3(:,133), t3x24(:,:,20))
  call vert_VV_S(ntry, ex3, ex4, wf9(:,1), n3(:,134), t3x9(:,:,1))
  call vert_SV_V(ntry, wf9(:,1), ex5, wf27(:,1), n3(:,135), t3x27(:,:,1))
  call vert_QA_Z(gZd,ntry, wf4(:,7), wf4(:,5), wf16(:,1), n3(:,136), t3x16(:,:,1))
  call prop_W_W(ntry, wf27(:,1), Q(:,28), MZ, 1_intkind1, wf27(:,2), n2(53))
  call vert_AZ_Q(gZd,ntry, ex2, wf27(:,2), wf54(:,9), n3(:,137), t3x54(:,:,9))
  call vert_QA_Z(gZd,ntry, wf4(:,9), wf4(:,3), wf16(:,2), n3(:,138), t3x16(:,:,2))
  call vert_ZQ_A(gZd,ntry, wf27(:,2), ex1, wf54(:,10), n3(:,139), t3x54(:,:,10))
  call vert_QA_Z(gZd,ntry, ex1, wf8(:,6), wf16(:,3), n3(:,140), t3x16(:,:,3))
  call vert_QA_Z(gZd,ntry, wf8(:,12), ex2, wf16(:,4), n3(:,141), t3x16(:,:,4))
  call vert_ZQ_A(gZd,ntry, ex4, wf18(:,22), wf54(:,11), n3(:,142), t3x54(:,:,11))
  call vert_ZQ_A(gZd,ntry, ex3, wf18(:,14), wf54(:,12), n3(:,143), t3x54(:,:,12))
  call vert_AZ_Q(gZd,ntry, wf12(:,42), ex4, wf36(:,53), n3(:,144), t3x36(:,:,53))
  call vert_AZ_Q(gZd,ntry, wf12(:,16), ex3, wf36(:,54), n3(:,145), t3x36(:,:,54))
  call vert_AZ_Q(gZd,ntry, wf12(:,44), ex4, wf36(:,55), n3(:,146), t3x36(:,:,55))
  call vert_AZ_Q(gZd,ntry, wf12(:,20), ex3, wf36(:,56), n3(:,147), t3x36(:,:,56))
  call vert_ZQ_A(gZd,ntry, ex4, wf12(:,46), wf36(:,57), n3(:,148), t3x36(:,:,57))
  call vert_ZQ_A(gZd,ntry, ex3, wf12(:,34), wf36(:,58), n3(:,149), t3x36(:,:,58))
  call vert_AZ_Q(gZd,ntry, wf18(:,20), ex4, wf54(:,13), n3(:,150), t3x54(:,:,13))
  call vert_AZ_Q(gZd,ntry, wf18(:,8), ex3, wf54(:,14), n3(:,151), t3x54(:,:,14))
  call vert_ZQ_A(gZd,ntry, ex4, wf12(:,48), wf36(:,59), n3(:,152), t3x36(:,:,59))
  call vert_ZQ_A(gZd,ntry, ex3, wf12(:,38), wf36(:,60), n3(:,153), t3x36(:,:,60))
  call vert_VV_S(ntry, ex3, ex5, wf9(:,2), n3(:,154), t3x9(:,:,2))
  call vert_SV_V(ntry, wf9(:,2), ex4, wf27(:,3), n3(:,155), t3x27(:,:,2))
  call prop_W_W(ntry, wf27(:,3), Q(:,28), MZ, 1_intkind1, wf27(:,4), n2(54))
  call vert_AZ_Q(gZd,ntry, ex2, wf27(:,4), wf54(:,15), n3(:,156), t3x54(:,:,15))
  call vert_ZQ_A(gZd,ntry, wf27(:,4), ex1, wf54(:,16), n3(:,157), t3x54(:,:,16))
  call vert_VV_S(ntry, ex4, ex5, wf9(:,3), n3(:,158), t3x9(:,:,3))
  call vert_SV_V(ntry, wf9(:,3), ex3, wf27(:,5), n3(:,159), t3x27(:,:,3))
  call prop_W_W(ntry, wf27(:,5), Q(:,28), MZ, 1_intkind1, wf27(:,6), n2(55))
  call vert_AZ_Q(gZd,ntry, ex2, wf27(:,6), wf54(:,17), n3(:,160), t3x54(:,:,17))
  call vert_ZQ_A(gZd,ntry, wf27(:,6), ex1, wf54(:,18), n3(:,161), t3x54(:,:,18))


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
  M2add = M2 / average_factor_ppvvvj_ddxzzzgg_1

  do k = 0, 30-1
    M2(k) = M2add(extcomb_perm_ppvvvj_ddxzzzgg_1(k))
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

    call cont_QA(nsync, wf24(:,1), wf18(:,2), A(:,1), n3(:,162), t3x432(:,:,1), nhel, den(7))
    call cont_QA(nsync, wf24(:,2), wf18(:,4), A(:,2), n3(:,163), t3x432(:,:,2), nhel, den(11))
    call cont_QA(nsync, wf12(:,2), wf36(:,1), A(:,3), n3(:,164), t3x432(:,:,3), nhel, den(14))
    call cont_QA(nsync, wf12(:,4), wf36(:,2), A(:,4), n3(:,165), t3x432(:,:,4), nhel, den(17))
    call cont_QA(nsync, wf12(:,6), wf36(:,3), A(:,5), n3(:,166), t3x432(:,:,5), nhel, den(20))
    call cont_QA(nsync, wf12(:,4), wf36(:,4), A(:,6), n3(:,167), t3x432(:,:,6), nhel, den(21))
    call cont_QA(nsync, wf12(:,8), wf36(:,5), A(:,7), n3(:,168), t3x432(:,:,7), nhel, den(24))
    call cont_QA(nsync, wf12(:,8), wf36(:,6), A(:,8), n3(:,169), t3x432(:,:,8), nhel, den(25))
    call cont_QA(nsync, wf24(:,3), wf18(:,6), A(:,9), n3(:,170), t3x432(:,:,9), nhel, den(30))
    call cont_QA(nsync, wf24(:,2), wf18(:,8), A(:,10), n3(:,171), t3x432(:,:,10), nhel, den(32))
    call cont_QA(nsync, wf12(:,10), wf36(:,7), A(:,11), n3(:,172), t3x432(:,:,11), nhel, den(35))
    call cont_QA(nsync, wf12(:,12), wf36(:,8), A(:,12), n3(:,173), t3x432(:,:,12), nhel, den(38))
    call cont_QA(nsync, wf12(:,6), wf36(:,9), A(:,13), n3(:,174), t3x432(:,:,13), nhel, den(39))
    call cont_QA(nsync, wf12(:,12), wf36(:,10), A(:,14), n3(:,175), t3x432(:,:,14), nhel, den(40))
    call cont_QA(nsync, wf12(:,8), wf36(:,11), A(:,15), n3(:,176), t3x432(:,:,15), nhel, den(41))
    call cont_QA(nsync, wf12(:,8), wf36(:,12), A(:,16), n3(:,177), t3x432(:,:,16), nhel, den(42))
    call cont_QA(nsync, wf36(:,7), wf12(:,14), A(:,17), n3(:,178), t3x432(:,:,17), nhel, den(45))
    call cont_QA(nsync, wf8(:,2), wf54(:,1), A(:,18), n3(:,179), t3x432(:,:,18), nhel, den(48))
    call cont_QA(nsync, wf18(:,2), wf24(:,4), A(:,19), n3(:,180), t3x432(:,:,19), nhel, den(50))
    call cont_QA(nsync, wf8(:,2), wf54(:,2), A(:,20), n3(:,181), t3x432(:,:,20), nhel, den(51))
    call cont_QA(nsync, wf12(:,8), wf36(:,13), A(:,21), n3(:,182), t3x432(:,:,21), nhel, den(52))
    call cont_QA(nsync, wf12(:,8), wf36(:,14), A(:,22), n3(:,183), t3x432(:,:,22), nhel, den(53))
    call cont_QA(nsync, wf36(:,8), wf12(:,18), A(:,23), n3(:,184), t3x432(:,:,23), nhel, den(56))
    call cont_QA(nsync, wf54(:,1), wf8(:,4), A(:,24), n3(:,185), t3x432(:,:,24), nhel, den(58))
    call cont_QA(nsync, wf18(:,2), wf24(:,5), A(:,25), n3(:,186), t3x432(:,:,25), nhel, den(60))
    call cont_QA(nsync, wf54(:,2), wf8(:,4), A(:,26), n3(:,187), t3x432(:,:,26), nhel, den(61))
    call cont_QA(nsync, wf12(:,6), wf36(:,15), A(:,27), n3(:,188), t3x432(:,:,27), nhel, den(62))
    call cont_QA(nsync, wf12(:,6), wf36(:,16), A(:,28), n3(:,189), t3x432(:,:,28), nhel, den(63))
    call cont_QA(nsync, wf18(:,6), wf24(:,6), A(:,29), n3(:,190), t3x432(:,:,29), nhel, den(65))
    call cont_QA(nsync, wf18(:,2), wf24(:,7), A(:,30), n3(:,191), t3x432(:,:,30), nhel, den(66))
    call cont_QA(nsync, wf24(:,8), wf18(:,10), A(:,31), n3(:,192), t3x432(:,:,31), nhel, den(72))
    call cont_QA(nsync, wf24(:,9), wf18(:,12), A(:,32), n3(:,193), t3x432(:,:,32), nhel, den(76))
    call cont_QA(nsync, wf12(:,22), wf36(:,17), A(:,33), n3(:,194), t3x432(:,:,33), nhel, den(79))
    call cont_QA(nsync, wf12(:,24), wf36(:,18), A(:,34), n3(:,195), t3x432(:,:,34), nhel, den(82))
    call cont_QA(nsync, wf12(:,26), wf36(:,19), A(:,35), n3(:,196), t3x432(:,:,35), nhel, den(85))
    call cont_QA(nsync, wf12(:,24), wf36(:,20), A(:,36), n3(:,197), t3x432(:,:,36), nhel, den(86))
    call cont_QA(nsync, wf12(:,28), wf36(:,21), A(:,37), n3(:,198), t3x432(:,:,37), nhel, den(89))
    call cont_QA(nsync, wf12(:,28), wf36(:,22), A(:,38), n3(:,199), t3x432(:,:,38), nhel, den(90))
    call cont_QA(nsync, wf24(:,8), wf18(:,14), A(:,39), n3(:,200), t3x432(:,:,39), nhel, den(93))
    call cont_QA(nsync, wf24(:,10), wf18(:,16), A(:,40), n3(:,201), t3x432(:,:,40), nhel, den(97))
    call cont_QA(nsync, wf12(:,22), wf36(:,23), A(:,41), n3(:,202), t3x432(:,:,41), nhel, den(98))
    call cont_QA(nsync, wf12(:,24), wf36(:,24), A(:,42), n3(:,203), t3x432(:,:,42), nhel, den(99))
    call cont_QA(nsync, wf12(:,30), wf36(:,25), A(:,43), n3(:,204), t3x432(:,:,43), nhel, den(102))
    call cont_QA(nsync, wf12(:,24), wf36(:,26), A(:,44), n3(:,205), t3x432(:,:,44), nhel, den(103))
    call cont_QA(nsync, wf12(:,32), wf36(:,27), A(:,45), n3(:,206), t3x432(:,:,45), nhel, den(106))
    call cont_QA(nsync, wf12(:,32), wf36(:,28), A(:,46), n3(:,207), t3x432(:,:,46), nhel, den(107))
    call cont_QA(nsync, wf18(:,12), wf24(:,11), A(:,47), n3(:,208), t3x432(:,:,47), nhel, den(110))
    call cont_QA(nsync, wf12(:,24), wf36(:,29), A(:,48), n3(:,209), t3x432(:,:,48), nhel, den(111))
    call cont_QA(nsync, wf36(:,25), wf12(:,36), A(:,49), n3(:,210), t3x432(:,:,49), nhel, den(113))
    call cont_QA(nsync, wf12(:,24), wf36(:,30), A(:,50), n3(:,211), t3x432(:,:,50), nhel, den(114))
    call cont_QA(nsync, wf8(:,8), wf54(:,3), A(:,51), n3(:,212), t3x432(:,:,51), nhel, den(117))
    call cont_QA(nsync, wf8(:,8), wf54(:,4), A(:,52), n3(:,213), t3x432(:,:,52), nhel, den(118))
    call cont_QA(nsync, wf18(:,12), wf24(:,12), A(:,53), n3(:,214), t3x432(:,:,53), nhel, den(121))
    call cont_QA(nsync, wf12(:,22), wf36(:,31), A(:,54), n3(:,215), t3x432(:,:,54), nhel, den(122))
    call cont_QA(nsync, wf36(:,27), wf12(:,40), A(:,55), n3(:,216), t3x432(:,:,55), nhel, den(124))
    call cont_QA(nsync, wf12(:,22), wf36(:,32), A(:,56), n3(:,217), t3x432(:,:,56), nhel, den(125))
    call cont_QA(nsync, wf54(:,3), wf8(:,10), A(:,57), n3(:,218), t3x432(:,:,57), nhel, den(127))
    call cont_QA(nsync, wf54(:,4), wf8(:,10), A(:,58), n3(:,219), t3x432(:,:,58), nhel, den(128))
    call cont_QA(nsync, wf18(:,12), wf24(:,13), A(:,59), n3(:,220), t3x432(:,:,59), nhel, den(130))
    call cont_QA(nsync, wf18(:,16), wf24(:,14), A(:,60), n3(:,221), t3x432(:,:,60), nhel, den(131))
    call cont_QA(nsync, wf24(:,3), wf18(:,18), A(:,61), n3(:,222), t3x432(:,:,61), nhel, den(133))
    call cont_QA(nsync, wf24(:,9), wf18(:,20), A(:,62), n3(:,223), t3x432(:,:,62), nhel, den(135))
    call cont_QA(nsync, wf12(:,10), wf36(:,33), A(:,63), n3(:,224), t3x432(:,:,63), nhel, den(136))
    call cont_QA(nsync, wf12(:,12), wf36(:,34), A(:,64), n3(:,225), t3x432(:,:,64), nhel, den(137))
    call cont_QA(nsync, wf12(:,26), wf36(:,35), A(:,65), n3(:,226), t3x432(:,:,65), nhel, den(138))
    call cont_QA(nsync, wf12(:,12), wf36(:,36), A(:,66), n3(:,227), t3x432(:,:,66), nhel, den(139))
    call cont_QA(nsync, wf12(:,28), wf36(:,37), A(:,67), n3(:,228), t3x432(:,:,67), nhel, den(140))
    call cont_QA(nsync, wf12(:,28), wf36(:,38), A(:,68), n3(:,229), t3x432(:,:,68), nhel, den(141))
    call cont_QA(nsync, wf12(:,14), wf36(:,33), A(:,69), n3(:,230), t3x432(:,:,69), nhel, den(142))
    call cont_QA(nsync, wf8(:,2), wf54(:,5), A(:,70), n3(:,231), t3x432(:,:,70), nhel, den(143))
    call cont_QA(nsync, wf18(:,10), wf24(:,15), A(:,71), n3(:,232), t3x432(:,:,71), nhel, den(145))
    call cont_QA(nsync, wf8(:,2), wf54(:,6), A(:,72), n3(:,233), t3x432(:,:,72), nhel, den(146))
    call cont_QA(nsync, wf12(:,28), wf36(:,39), A(:,73), n3(:,234), t3x432(:,:,73), nhel, den(147))
    call cont_QA(nsync, wf12(:,28), wf36(:,40), A(:,74), n3(:,235), t3x432(:,:,74), nhel, den(148))
    call cont_QA(nsync, wf12(:,18), wf36(:,34), A(:,75), n3(:,236), t3x432(:,:,75), nhel, den(149))
    call cont_QA(nsync, wf8(:,4), wf54(:,5), A(:,76), n3(:,237), t3x432(:,:,76), nhel, den(150))
    call cont_QA(nsync, wf18(:,10), wf24(:,16), A(:,77), n3(:,238), t3x432(:,:,77), nhel, den(152))
    call cont_QA(nsync, wf8(:,4), wf54(:,6), A(:,78), n3(:,239), t3x432(:,:,78), nhel, den(153))
    call cont_QA(nsync, wf12(:,26), wf36(:,41), A(:,79), n3(:,240), t3x432(:,:,79), nhel, den(154))
    call cont_QA(nsync, wf12(:,26), wf36(:,42), A(:,80), n3(:,241), t3x432(:,:,80), nhel, den(155))
    call cont_QA(nsync, wf24(:,6), wf18(:,18), A(:,81), n3(:,242), t3x432(:,:,81), nhel, den(156))
    call cont_QA(nsync, wf18(:,10), wf24(:,17), A(:,82), n3(:,243), t3x432(:,:,82), nhel, den(157))
    call cont_QA(nsync, wf24(:,1), wf18(:,22), A(:,83), n3(:,244), t3x432(:,:,83), nhel, den(159))
    call cont_QA(nsync, wf24(:,10), wf18(:,24), A(:,84), n3(:,245), t3x432(:,:,84), nhel, den(161))
    call cont_QA(nsync, wf12(:,2), wf36(:,43), A(:,85), n3(:,246), t3x432(:,:,85), nhel, den(162))
    call cont_QA(nsync, wf12(:,4), wf36(:,44), A(:,86), n3(:,247), t3x432(:,:,86), nhel, den(163))
    call cont_QA(nsync, wf12(:,30), wf36(:,45), A(:,87), n3(:,248), t3x432(:,:,87), nhel, den(164))
    call cont_QA(nsync, wf12(:,4), wf36(:,46), A(:,88), n3(:,249), t3x432(:,:,88), nhel, den(165))
    call cont_QA(nsync, wf12(:,32), wf36(:,47), A(:,89), n3(:,250), t3x432(:,:,89), nhel, den(166))
    call cont_QA(nsync, wf12(:,32), wf36(:,48), A(:,90), n3(:,251), t3x432(:,:,90), nhel, den(167))
    call cont_QA(nsync, wf18(:,4), wf24(:,18), A(:,91), n3(:,252), t3x432(:,:,91), nhel, den(169))
    call cont_QA(nsync, wf12(:,4), wf36(:,49), A(:,92), n3(:,253), t3x432(:,:,92), nhel, den(170))
    call cont_QA(nsync, wf12(:,36), wf36(:,45), A(:,93), n3(:,254), t3x432(:,:,93), nhel, den(171))
    call cont_QA(nsync, wf12(:,4), wf36(:,50), A(:,94), n3(:,255), t3x432(:,:,94), nhel, den(172))
    call cont_QA(nsync, wf8(:,8), wf54(:,7), A(:,95), n3(:,256), t3x432(:,:,95), nhel, den(173))
    call cont_QA(nsync, wf8(:,8), wf54(:,8), A(:,96), n3(:,257), t3x432(:,:,96), nhel, den(174))
    call cont_QA(nsync, wf18(:,4), wf24(:,19), A(:,97), n3(:,258), t3x432(:,:,97), nhel, den(176))
    call cont_QA(nsync, wf12(:,2), wf36(:,51), A(:,98), n3(:,259), t3x432(:,:,98), nhel, den(177))
    call cont_QA(nsync, wf12(:,40), wf36(:,47), A(:,99), n3(:,260), t3x432(:,:,99), nhel, den(178))
    call cont_QA(nsync, wf12(:,2), wf36(:,52), A(:,100), n3(:,261), t3x432(:,:,100), nhel, den(179))
    call cont_QA(nsync, wf8(:,10), wf54(:,7), A(:,101), n3(:,262), t3x432(:,:,101), nhel, den(180))
    call cont_QA(nsync, wf8(:,10), wf54(:,8), A(:,102), n3(:,263), t3x432(:,:,102), nhel, den(181))
    call cont_QA(nsync, wf18(:,4), wf24(:,20), A(:,103), n3(:,264), t3x432(:,:,103), nhel, den(182))
    call cont_QA(nsync, wf24(:,14), wf18(:,24), A(:,104), n3(:,265), t3x432(:,:,104), nhel, den(183))
    call cont_VV(nsync, wf16(:,1), wf27(:,2), A(:,105), n3(:,266), t3x432(:,:,105), nhel, den(188))
    call cont_QA(nsync, wf8(:,8), wf54(:,9), A(:,106), n3(:,267), t3x432(:,:,106), nhel, den(189))
    call cont_VV(nsync, wf27(:,2), wf16(:,2), A(:,107), n3(:,268), t3x432(:,:,107), nhel, den(191))
    call cont_QA(nsync, wf8(:,2), wf54(:,10), A(:,108), n3(:,269), t3x432(:,:,108), nhel, den(192))
    call cont_QA(nsync, wf8(:,10), wf54(:,9), A(:,109), n3(:,270), t3x432(:,:,109), nhel, den(193))
    call cont_QA(nsync, wf8(:,4), wf54(:,10), A(:,110), n3(:,271), t3x432(:,:,110), nhel, den(194))
    call cont_VV(nsync, wf27(:,2), wf16(:,3), A(:,111), n3(:,272), t3x432(:,:,111), nhel, den(195))
    call cont_VV(nsync, wf27(:,2), wf16(:,4), A(:,112), n3(:,273), t3x432(:,:,112), nhel, den(196))
    call cont_QA(nsync, wf12(:,16), wf36(:,43), A(:,113), n3(:,274), t3x432(:,:,113), nhel, den(197))
    call cont_QA(nsync, wf8(:,2), wf54(:,11), A(:,114), n3(:,275), t3x432(:,:,114), nhel, den(198))
    call cont_QA(nsync, wf18(:,14), wf24(:,15), A(:,115), n3(:,276), t3x432(:,:,115), nhel, den(199))
    call cont_QA(nsync, wf8(:,2), wf54(:,12), A(:,116), n3(:,277), t3x432(:,:,116), nhel, den(200))
    call cont_QA(nsync, wf12(:,32), wf36(:,53), A(:,117), n3(:,278), t3x432(:,:,117), nhel, den(201))
    call cont_QA(nsync, wf12(:,32), wf36(:,54), A(:,118), n3(:,279), t3x432(:,:,118), nhel, den(202))
    call cont_QA(nsync, wf12(:,20), wf36(:,44), A(:,119), n3(:,280), t3x432(:,:,119), nhel, den(203))
    call cont_QA(nsync, wf8(:,4), wf54(:,11), A(:,120), n3(:,281), t3x432(:,:,120), nhel, den(204))
    call cont_QA(nsync, wf18(:,14), wf24(:,16), A(:,121), n3(:,282), t3x432(:,:,121), nhel, den(205))
    call cont_QA(nsync, wf8(:,4), wf54(:,12), A(:,122), n3(:,283), t3x432(:,:,122), nhel, den(206))
    call cont_QA(nsync, wf12(:,30), wf36(:,55), A(:,123), n3(:,284), t3x432(:,:,123), nhel, den(207))
    call cont_QA(nsync, wf12(:,30), wf36(:,56), A(:,124), n3(:,285), t3x432(:,:,124), nhel, den(208))
    call cont_QA(nsync, wf24(:,7), wf18(:,22), A(:,125), n3(:,286), t3x432(:,:,125), nhel, den(209))
    call cont_QA(nsync, wf18(:,14), wf24(:,17), A(:,126), n3(:,287), t3x432(:,:,126), nhel, den(210))
    call cont_QA(nsync, wf18(:,8), wf24(:,18), A(:,127), n3(:,288), t3x432(:,:,127), nhel, den(211))
    call cont_QA(nsync, wf12(:,12), wf36(:,57), A(:,128), n3(:,289), t3x432(:,:,128), nhel, den(212))
    call cont_QA(nsync, wf12(:,34), wf36(:,35), A(:,129), n3(:,290), t3x432(:,:,129), nhel, den(213))
    call cont_QA(nsync, wf12(:,12), wf36(:,58), A(:,130), n3(:,291), t3x432(:,:,130), nhel, den(214))
    call cont_QA(nsync, wf8(:,8), wf54(:,13), A(:,131), n3(:,292), t3x432(:,:,131), nhel, den(215))
    call cont_QA(nsync, wf8(:,8), wf54(:,14), A(:,132), n3(:,293), t3x432(:,:,132), nhel, den(216))
    call cont_QA(nsync, wf18(:,8), wf24(:,19), A(:,133), n3(:,294), t3x432(:,:,133), nhel, den(217))
    call cont_QA(nsync, wf12(:,10), wf36(:,59), A(:,134), n3(:,295), t3x432(:,:,134), nhel, den(218))
    call cont_QA(nsync, wf12(:,38), wf36(:,37), A(:,135), n3(:,296), t3x432(:,:,135), nhel, den(219))
    call cont_QA(nsync, wf12(:,10), wf36(:,60), A(:,136), n3(:,297), t3x432(:,:,136), nhel, den(220))
    call cont_QA(nsync, wf8(:,10), wf54(:,13), A(:,137), n3(:,298), t3x432(:,:,137), nhel, den(221))
    call cont_QA(nsync, wf8(:,10), wf54(:,14), A(:,138), n3(:,299), t3x432(:,:,138), nhel, den(222))
    call cont_QA(nsync, wf18(:,8), wf24(:,20), A(:,139), n3(:,300), t3x432(:,:,139), nhel, den(223))
    call cont_QA(nsync, wf24(:,13), wf18(:,20), A(:,140), n3(:,301), t3x432(:,:,140), nhel, den(224))
    call cont_VV(nsync, wf16(:,1), wf27(:,4), A(:,141), n3(:,302), t3x432(:,:,141), nhel, den(227))
    call cont_QA(nsync, wf8(:,8), wf54(:,15), A(:,142), n3(:,303), t3x432(:,:,142), nhel, den(228))
    call cont_VV(nsync, wf16(:,2), wf27(:,4), A(:,143), n3(:,304), t3x432(:,:,143), nhel, den(229))
    call cont_QA(nsync, wf8(:,2), wf54(:,16), A(:,144), n3(:,305), t3x432(:,:,144), nhel, den(230))
    call cont_QA(nsync, wf8(:,10), wf54(:,15), A(:,145), n3(:,306), t3x432(:,:,145), nhel, den(231))
    call cont_QA(nsync, wf8(:,4), wf54(:,16), A(:,146), n3(:,307), t3x432(:,:,146), nhel, den(232))
    call cont_VV(nsync, wf16(:,3), wf27(:,4), A(:,147), n3(:,308), t3x432(:,:,147), nhel, den(233))
    call cont_VV(nsync, wf16(:,4), wf27(:,4), A(:,148), n3(:,309), t3x432(:,:,148), nhel, den(234))
    call cont_VV(nsync, wf16(:,1), wf27(:,6), A(:,149), n3(:,310), t3x432(:,:,149), nhel, den(237))
    call cont_QA(nsync, wf8(:,8), wf54(:,17), A(:,150), n3(:,311), t3x432(:,:,150), nhel, den(238))
    call cont_VV(nsync, wf16(:,2), wf27(:,6), A(:,151), n3(:,312), t3x432(:,:,151), nhel, den(239))
    call cont_QA(nsync, wf8(:,2), wf54(:,18), A(:,152), n3(:,313), t3x432(:,:,152), nhel, den(240))
    call cont_QA(nsync, wf8(:,10), wf54(:,17), A(:,153), n3(:,314), t3x432(:,:,153), nhel, den(241))
    call cont_QA(nsync, wf8(:,4), wf54(:,18), A(:,154), n3(:,315), t3x432(:,:,154), nhel, den(242))
    call cont_VV(nsync, wf16(:,3), wf27(:,6), A(:,155), n3(:,316), t3x432(:,:,155), nhel, den(243))
    call cont_VV(nsync, wf16(:,4), wf27(:,6), A(:,156), n3(:,317), t3x432(:,:,156), nhel, den(244))
    call cont_QA(nsync, wf12(:,20), wf36(:,49), A(:,157), n3(:,318), t3x432(:,:,157), nhel, den(245))
    call cont_QA(nsync, wf12(:,18), wf36(:,57), A(:,158), n3(:,319), t3x432(:,:,158), nhel, den(246))
    call cont_QA(nsync, wf12(:,34), wf36(:,41), A(:,159), n3(:,320), t3x432(:,:,159), nhel, den(247))
    call cont_QA(nsync, wf12(:,18), wf36(:,58), A(:,160), n3(:,321), t3x432(:,:,160), nhel, den(248))
    call cont_QA(nsync, wf12(:,36), wf36(:,55), A(:,161), n3(:,322), t3x432(:,:,161), nhel, den(249))
    call cont_QA(nsync, wf12(:,36), wf36(:,56), A(:,162), n3(:,323), t3x432(:,:,162), nhel, den(250))
    call cont_QA(nsync, wf12(:,16), wf36(:,51), A(:,163), n3(:,324), t3x432(:,:,163), nhel, den(251))
    call cont_QA(nsync, wf12(:,14), wf36(:,59), A(:,164), n3(:,325), t3x432(:,:,164), nhel, den(252))
    call cont_QA(nsync, wf12(:,38), wf36(:,39), A(:,165), n3(:,326), t3x432(:,:,165), nhel, den(253))
    call cont_QA(nsync, wf12(:,14), wf36(:,60), A(:,166), n3(:,327), t3x432(:,:,166), nhel, den(254))
    call cont_QA(nsync, wf12(:,40), wf36(:,53), A(:,167), n3(:,328), t3x432(:,:,167), nhel, den(255))
    call cont_QA(nsync, wf12(:,40), wf36(:,54), A(:,168), n3(:,329), t3x432(:,:,168), nhel, den(256))

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

  M1(1) = (-A(j,3)%j-A(j,7)%j-A(j,8)%j-A(j,11)%j-A(j,15)%j-A(j,16)%j-A(j,17)%j-A(j,18)%j-A(j,19)%j-A(j,20)%j-A(j,21)%j-A(j,22)%j &
       -A(j,33)%j-A(j,37)%j-A(j,38)%j-A(j,41)%j-A(j,45)%j-A(j,46)%j-A(j,53)%j-A(j,54)%j-A(j,55)%j-A(j,56)%j-A(j,57)%j-A(j,58)%j &
       -A(j,63)%j-A(j,67)%j-A(j,68)%j-A(j,69)%j-A(j,70)%j-A(j,71)%j-A(j,72)%j-A(j,73)%j-A(j,74)%j-A(j,85)%j-A(j,89)%j-A(j,90)%j &
       -A(j,97)%j-A(j,98)%j-A(j,99)%j-A(j,100)%j-A(j,101)%j-A(j,102)%j-A(j,113)%j-A(j,114)%j-A(j,115)%j-A(j,116)%j-A(j,117)%j &
       -A(j,118)%j-A(j,133)%j-A(j,134)%j-A(j,135)%j-A(j,136)%j-A(j,137)%j-A(j,138)%j-A(j,163)%j-A(j,164)%j-A(j,165)%j-A(j,166)%j &
       -A(j,167)%j-A(j,168)%j)*f(1)+CI*(-A(j,1)%j-A(j,2)%j-A(j,9)%j-A(j,10)%j-A(j,29)%j-A(j,30)%j-A(j,31)%j-A(j,32)%j-A(j,39)%j &
       -A(j,40)%j-A(j,59)%j-A(j,60)%j-A(j,61)%j-A(j,62)%j-A(j,81)%j-A(j,82)%j-A(j,83)%j-A(j,84)%j-A(j,103)%j-A(j,104)%j-A(j,125)%j &
       -A(j,126)%j-A(j,139)%j-A(j,140)%j)*f(2)+(A(j,107)%j+A(j,108)%j+A(j,109)%j+A(j,143)%j+A(j,144)%j+A(j,145)%j+A(j,151)%j &
       +A(j,152)%j+A(j,153)%j)*f(3)+CI*(A(j,111)%j+A(j,112)%j+A(j,147)%j+A(j,148)%j+A(j,155)%j+A(j,156)%j)*f(4)
  M1(2) = (-A(j,4)%j-A(j,5)%j-A(j,6)%j-A(j,12)%j-A(j,13)%j-A(j,14)%j-A(j,23)%j-A(j,24)%j-A(j,25)%j-A(j,26)%j-A(j,27)%j-A(j,28)%j &
       -A(j,34)%j-A(j,35)%j-A(j,36)%j-A(j,42)%j-A(j,43)%j-A(j,44)%j-A(j,47)%j-A(j,48)%j-A(j,49)%j-A(j,50)%j-A(j,51)%j-A(j,52)%j &
       -A(j,64)%j-A(j,65)%j-A(j,66)%j-A(j,75)%j-A(j,76)%j-A(j,77)%j-A(j,78)%j-A(j,79)%j-A(j,80)%j-A(j,86)%j-A(j,87)%j-A(j,88)%j &
       -A(j,91)%j-A(j,92)%j-A(j,93)%j-A(j,94)%j-A(j,95)%j-A(j,96)%j-A(j,119)%j-A(j,120)%j-A(j,121)%j-A(j,122)%j-A(j,123)%j &
       -A(j,124)%j-A(j,127)%j-A(j,128)%j-A(j,129)%j-A(j,130)%j-A(j,131)%j-A(j,132)%j-A(j,157)%j-A(j,158)%j-A(j,159)%j-A(j,160)%j &
       -A(j,161)%j-A(j,162)%j)*f(1)+CI*(A(j,1)%j+A(j,2)%j+A(j,9)%j+A(j,10)%j+A(j,29)%j+A(j,30)%j+A(j,31)%j+A(j,32)%j+A(j,39)%j &
       +A(j,40)%j+A(j,59)%j+A(j,60)%j+A(j,61)%j+A(j,62)%j+A(j,81)%j+A(j,82)%j+A(j,83)%j+A(j,84)%j+A(j,103)%j+A(j,104)%j+A(j,125)%j &
       +A(j,126)%j+A(j,139)%j+A(j,140)%j)*f(2)+(A(j,105)%j+A(j,106)%j+A(j,110)%j+A(j,141)%j+A(j,142)%j+A(j,146)%j+A(j,149)%j &
       +A(j,150)%j+A(j,154)%j)*f(3)+CI*(-A(j,111)%j-A(j,112)%j-A(j,147)%j-A(j,148)%j-A(j,155)%j-A(j,156)%j)*f(4)

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
  use ol_colourmatrix_ppvvvj_ddxzzzgg_1_/**/REALKIND, only: K1
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
  use ol_colourmatrix_ppvvvj_ddxzzzgg_1_/**/REALKIND, only: K1
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
  & bind(c,name="ol_tree_colvect_ppvvvj_ddxzzzgg_1")
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
    & bind(c,name="ol_f_amp2tree_ppvvvj_ddxzzzgg_1")
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
    & bind(c,name="ol_f_amp2ccone_ppvvvj_ddxzzzgg_1")
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
    & bind(c,name="ol_f_amp2ccall_ppvvvj_ddxzzzgg_1")
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
    & bind(c,name="ol_f_amp2hcone_ppvvvj_ddxzzzgg_1")
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
    & bind(c,name="ol_f_amp2hcall_ppvvvj_ddxzzzgg_1")
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
    & bind(c,name="ol_amp2tree_ppvvvj_ddxzzzgg_1")
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
    & bind(c,name="ol_amp2ccone_ppvvvj_ddxzzzgg_1")
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
    & bind(c,name="ol_amp2ccall_ppvvvj_ddxzzzgg_1")
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
    & bind(c,name="ol_amp2hcone_ppvvvj_ddxzzzgg_1")
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
    & bind(c,name="ol_amp2hcall_ppvvvj_ddxzzzgg_1")
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
    & bind(c,name="amp2tree_ppvvvj_ddxzzzgg_1_")
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
    & bind(c,name="amp2ccone_ppvvvj_ddxzzzgg_1_")
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
    & bind(c,name="amp2ccall_ppvvvj_ddxzzzgg_1_")
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
    & bind(c,name="amp2hcone_ppvvvj_ddxzzzgg_1_")
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
    & bind(c,name="amp2hcall_ppvvvj_ddxzzzgg_1_")
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

end module ol_tree_ppvvvj_ddxzzzgg_1_/**/REALKIND
