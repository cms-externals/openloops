
module ol_tree_ppjjjj_gggggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(1)
  complex(REALKIND), save :: den(290)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 64 ! number of helicity configurations
  integer(intkind2), save :: nhel = 64 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(64) ! physical helicity states
  complex(DREALKIND) :: M1helarr(120,64) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = CI*gQCD**4

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,7))
  den(2) = 1 / (Q(5,11))
  den(3) = 1 / (Q(5,19))
  den(4) = 1 / (Q(5,35))
  den(5) = 1 / (Q(5,13))
  den(6) = 1 / (Q(5,21))
  den(7) = 1 / (Q(5,37))
  den(8) = 1 / (Q(5,25))
  den(9) = 1 / (Q(5,41))
  den(10) = 1 / (Q(5,49))
  den(11) = 1 / (Q(5,3))
  den(12) = 1 / (Q(5,12))
  den(14) = 1 / (Q(5,20))
  den(16) = 1 / (Q(5,36))
  den(18) = 1 / (Q(5,56))
  den(20) = 1 / (Q(5,24))
  den(22) = 1 / (Q(5,40))
  den(24) = 1 / (Q(5,52))
  den(26) = 1 / (Q(5,48))
  den(28) = 1 / (Q(5,44))
  den(30) = 1 / (Q(5,28))
  den(32) = 1 / (Q(5,5))
  den(33) = 1 / (Q(5,10))
  den(35) = 1 / (Q(5,18))
  den(37) = 1 / (Q(5,34))
  den(40) = 1 / (Q(5,9))
  den(41) = 1 / (Q(5,6))
  den(43) = 1 / (Q(5,17))
  den(45) = 1 / (Q(5,33))
  den(62) = 1 / (Q(5,50))
  den(65) = 1 / (Q(5,42))
  den(67) = 1 / (Q(5,26))
  den(82) = 1 / (Q(5,38))
  den(84) = 1 / (Q(5,22))
  den(92) = 1 / (Q(5,14))

  ! denominators

  den(13) = den(11)*den(12)
  den(15) = den(11)*den(14)
  den(17) = den(11)*den(16)
  den(19) = den(11)*den(18)
  den(21) = den(11)*den(20)
  den(23) = den(11)*den(22)
  den(25) = den(11)*den(24)
  den(27) = den(11)*den(26)
  den(29) = den(11)*den(28)
  den(31) = den(11)*den(30)
  den(34) = den(32)*den(33)
  den(36) = den(32)*den(35)
  den(38) = den(32)*den(37)
  den(39) = den(18)*den(32)
  den(42) = den(40)*den(41)
  den(44) = den(41)*den(43)
  den(46) = den(41)*den(45)
  den(47) = den(18)*den(41)
  den(48) = den(35)*den(40)
  den(49) = den(37)*den(40)
  den(50) = den(24)*den(40)
  den(51) = den(33)*den(43)
  den(52) = den(33)*den(45)
  den(53) = den(24)*den(33)
  den(54) = den(37)*den(43)
  den(55) = den(28)*den(43)
  den(56) = den(35)*den(45)
  den(57) = den(28)*den(35)
  den(58) = den(30)*den(45)
  den(59) = den(30)*den(37)
  den(60) = den(20)*den(32)
  den(61) = den(22)*den(32)
  den(63) = den(32)*den(62)
  den(64) = den(26)*den(32)
  den(66) = den(32)*den(65)
  den(68) = den(32)*den(67)
  den(69) = den(14)*den(40)
  den(70) = den(16)*den(40)
  den(71) = den(40)*den(62)
  den(72) = den(12)*den(43)
  den(73) = den(12)*den(45)
  den(74) = den(12)*den(62)
  den(75) = den(16)*den(43)
  den(76) = den(43)*den(65)
  den(77) = den(14)*den(45)
  den(78) = den(14)*den(65)
  den(79) = den(45)*den(67)
  den(80) = den(16)*den(67)
  den(81) = den(26)*den(40)
  den(83) = den(40)*den(82)
  den(85) = den(40)*den(84)
  den(86) = den(22)*den(43)
  den(87) = den(43)*den(82)
  den(88) = den(20)*den(45)
  den(89) = den(20)*den(82)
  den(90) = den(45)*den(84)
  den(91) = den(22)*den(84)
  den(93) = den(43)*den(92)
  den(94) = den(45)*den(92)
  den(95) = den(26)*den(92)
  den(96) = den(20)*den(41)
  den(97) = den(22)*den(41)
  den(98) = den(10)*den(41)
  den(99) = den(26)*den(41)
  den(100) = den(9)*den(41)
  den(101) = den(8)*den(41)
  den(102) = den(14)*den(33)
  den(103) = den(16)*den(33)
  den(104) = den(10)*den(33)
  den(105) = den(12)*den(35)
  den(106) = den(12)*den(37)
  den(107) = den(10)*den(12)
  den(108) = den(16)*den(35)
  den(109) = den(9)*den(35)
  den(110) = den(14)*den(37)
  den(111) = den(9)*den(14)
  den(112) = den(8)*den(37)
  den(113) = den(8)*den(16)
  den(114) = den(26)*den(33)
  den(115) = den(7)*den(33)
  den(116) = den(6)*den(33)
  den(117) = den(22)*den(35)
  den(118) = den(7)*den(35)
  den(119) = den(20)*den(37)
  den(120) = den(7)*den(20)
  den(121) = den(6)*den(37)
  den(122) = den(6)*den(22)
  den(123) = den(5)*den(35)
  den(124) = den(5)*den(37)
  den(125) = den(5)*den(26)
  den(126) = den(12)*den(26)
  den(127) = den(4)*den(12)
  den(128) = den(3)*den(12)
  den(129) = den(14)*den(22)
  den(130) = den(4)*den(14)
  den(131) = den(16)*den(20)
  den(132) = den(4)*den(20)
  den(133) = den(3)*den(16)
  den(134) = den(3)*den(22)
  den(135) = den(2)*den(14)
  den(136) = den(2)*den(16)
  den(137) = den(2)*den(26)
  den(138) = den(1)*den(20)
  den(139) = den(1)*den(22)
  den(140) = den(1)*den(26)
  den(141) = den(13)*den(26)
  den(142) = den(3)*den(11)
  den(143) = den(12)*den(142)
  den(144) = den(12)*den(30)
  den(145) = den(11)*den(144)
  den(146) = den(15)*den(22)
  den(147) = den(2)*den(11)
  den(148) = den(14)*den(147)
  den(149) = den(14)*den(30)
  den(150) = den(11)*den(149)
  den(151) = den(17)*den(20)
  den(152) = den(1)*den(11)
  den(153) = den(20)*den(152)
  den(154) = den(20)*den(30)
  den(155) = den(11)*den(154)
  den(156) = den(16)*den(147)
  den(157) = den(16)*den(28)
  den(158) = den(11)*den(157)
  den(159) = den(22)*den(152)
  den(160) = den(22)*den(28)
  den(161) = den(11)*den(160)
  den(162) = den(26)*den(152)
  den(163) = den(24)*den(26)
  den(164) = den(11)*den(163)
  den(165) = den(26)*den(34)
  den(166) = den(6)*den(32)
  den(167) = den(33)*den(166)
  den(168) = den(33)*den(67)
  den(169) = den(32)*den(168)
  den(170) = den(22)*den(36)
  den(171) = den(5)*den(32)
  den(172) = den(35)*den(171)
  den(173) = den(35)*den(67)
  den(174) = den(32)*den(173)
  den(175) = den(20)*den(38)
  den(176) = den(1)*den(32)
  den(177) = den(20)*den(176)
  den(178) = den(20)*den(67)
  den(179) = den(32)*den(178)
  den(180) = den(37)*den(171)
  den(181) = den(37)*den(65)
  den(182) = den(32)*den(181)
  den(183) = den(22)*den(176)
  den(184) = den(22)*den(65)
  den(185) = den(32)*den(184)
  den(186) = den(26)*den(176)
  den(187) = den(26)*den(62)
  den(188) = den(32)*den(187)
  den(189) = den(26)*den(42)
  den(190) = den(8)*den(40)
  den(191) = den(41)*den(190)
  den(192) = den(41)*den(84)
  den(193) = den(40)*den(192)
  den(194) = den(22)*den(44)
  den(195) = den(8)*den(43)
  den(196) = den(41)*den(195)
  den(197) = den(41)*den(92)
  den(198) = den(43)*den(197)
  den(199) = den(20)*den(46)
  den(200) = den(1)*den(41)
  den(201) = den(20)*den(200)
  den(202) = den(8)*den(20)
  den(203) = den(41)*den(202)
  den(204) = den(9)*den(45)
  den(205) = den(41)*den(204)
  den(206) = den(45)*den(197)
  den(207) = den(22)*den(200)
  den(208) = den(9)*den(22)
  den(209) = den(41)*den(208)
  den(210) = den(26)*den(200)
  den(211) = den(10)*den(26)
  den(212) = den(41)*den(211)
  den(213) = den(16)*den(48)
  den(214) = den(5)*den(40)
  den(215) = den(35)*den(214)
  den(216) = den(35)*den(84)
  den(217) = den(40)*den(216)
  den(218) = den(14)*den(49)
  den(219) = den(2)*den(40)
  den(220) = den(14)*den(219)
  den(221) = den(14)*den(84)
  den(222) = den(40)*den(221)
  den(223) = den(37)*den(214)
  den(224) = den(37)*den(82)
  den(225) = den(40)*den(224)
  den(226) = den(16)*den(219)
  den(227) = den(16)*den(82)
  den(228) = den(40)*den(227)
  den(229) = den(26)*den(219)
  den(230) = den(40)*den(187)
  den(231) = den(16)*den(51)
  den(232) = den(6)*den(43)
  den(233) = den(33)*den(232)
  den(234) = den(33)*den(92)
  den(235) = den(43)*den(234)
  den(236) = den(14)*den(52)
  den(237) = den(2)*den(33)
  den(238) = den(14)*den(237)
  den(239) = den(6)*den(14)
  den(240) = den(33)*den(239)
  den(241) = den(7)*den(45)
  den(242) = den(33)*den(241)
  den(243) = den(45)*den(234)
  den(244) = den(16)*den(237)
  den(245) = den(7)*den(16)
  den(246) = den(33)*den(245)
  den(247) = den(26)*den(237)
  den(248) = den(33)*den(211)
  den(249) = den(12)*den(54)
  den(250) = den(3)*den(43)
  den(251) = den(12)*den(250)
  den(252) = den(12)*den(92)
  den(253) = den(43)*den(252)
  den(254) = den(12)*den(56)
  den(255) = den(3)*den(35)
  den(256) = den(12)*den(255)
  den(257) = den(5)*den(12)
  den(258) = den(35)*den(257)
  den(259) = den(4)*den(45)
  den(260) = den(12)*den(259)
  den(261) = den(45)*den(252)
  den(262) = den(4)*den(37)
  den(263) = den(12)*den(262)
  den(264) = den(37)*den(257)
  den(265) = den(26)*den(257)
  den(266) = den(12)*den(211)
  den(267) = den(37)*den(232)
  den(268) = den(43)*den(224)
  den(269) = den(16)*den(250)
  den(270) = den(43)*den(227)
  den(271) = den(22)*den(250)
  den(272) = den(43)*den(184)
  den(273) = den(35)*den(241)
  den(274) = den(45)*den(216)
  den(275) = den(16)*den(255)
  den(276) = den(35)*den(245)
  den(277) = den(22)*den(255)
  den(278) = den(35)*den(208)
  den(279) = den(14)*den(259)
  den(280) = den(45)*den(221)
  den(281) = den(14)*den(262)
  den(282) = den(37)*den(239)
  den(283) = den(22)*den(239)
  den(284) = den(14)*den(208)
  den(285) = den(20)*den(259)
  den(286) = den(45)*den(178)
  den(287) = den(20)*den(262)
  den(288) = den(37)*den(202)
  den(289) = den(20)*den(245)
  den(290) = den(16)*den(202)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_ppjjjj_gggggg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_ppjjjj_gggggg_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for glue glue glue glue glue glue -> 0
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
  use ol_external_ppjjjj_gggggg_1, only: external_perm_ppjjjj_gggggg_1, &
    & external_perm_inv_ppjjjj_gggggg_1, extcomb_perm_ppjjjj_gggggg_1, &
    & average_factor_ppjjjj_gggggg_1
  use ol_external_ppjjjj_gggggg_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_ppjjjj_gggggg_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_ppjjjj_gggggg_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_ppjjjj_gggggg_1
  use ol_kinematics_/**/REALKIND, only: LC2Std_Rep_cmplx
  implicit none

  real(DREALKIND), intent(in)  :: P_scatt(0:3,6)
  real(REALKIND),  intent(out) :: M2(0:23-1)
  real(REALKIND),  intent(out) :: M2munu(4,4)
  integer,         intent(in)  :: I
  real(REALKIND),  intent(in)  :: MOM(0:3)
  integer,         intent(in)  :: nextcombs
  integer,         intent(in)  :: extcombs(nextcombs)

  integer           :: ReplacePol, JBmunu, extcombs_permuted(nextcombs), shift, k, r, l, m, n
  real(REALKIND)    :: P(0:3,6)
  real(REALKIND)    :: extmasses2(6)
  real(REALKIND)    :: M2add(0:23-1), M2munuadd
  complex(REALKIND) :: MOM_LC(4), M1(120), M1helarray(120,64)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(2), ex6(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,15), wf8(8,120), wf16(16,150), wf64(64,510)

  type(polcont) :: A(64,510)
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
  extmasses2 = [ rZERO2, rZERO2, rZERO2, rZERO2, rZERO2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_ppjjjj_gggggg_1,6)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,6)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_ppjjjj_gggggg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_ppjjjj_gggggg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_ppjjjj_gggggg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_V(P(:,1), rZERO, H1, ex1)
  call wf_V(P(:,2), rZERO, H2, ex2)
  call wf_V(P(:,3), rZERO, H3, ex3)
  call wf_V(P(:,4), rZERO, H4, ex4)
  call wf_V(P(:,5), rZERO, H5, ex5)
  call wf_V(P(:,6), rZERO, H6, ex6)


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

  end if

  ! internal WFs
  ! e.g. call vert_VQ_A(ntry, ex3, ex1, wf1, n1, t1) ...
  call vert_GGG_G(ntry, ex1, ex2, ex3, wf8(:,1), n4(:,1), t4x8(:,:,1))
  call vert_GGG_G(ntry, ex4, ex5, ex6, wf8(:,2), n4(:,2), t4x8(:,:,2))
  call vert_GGG_G(ntry, ex2, ex3, ex1, wf8(:,3), n4(:,3), t4x8(:,:,3))
  call vert_GGG_G(ntry, ex3, ex1, ex2, wf8(:,4), n4(:,4), t4x8(:,:,4))
  call vert_GGG_G(ntry, ex5, ex6, ex4, wf8(:,5), n4(:,5), t4x8(:,:,5))
  call vert_GGG_G(ntry, ex6, ex4, ex5, wf8(:,6), n4(:,6), t4x8(:,:,6))
  call vert_GGG_G(ntry, ex1, ex2, ex4, wf8(:,7), n4(:,7), t4x8(:,:,7))
  call vert_GGG_G(ntry, ex3, ex5, ex6, wf8(:,8), n4(:,8), t4x8(:,:,8))
  call vert_GGG_G(ntry, ex2, ex4, ex1, wf8(:,9), n4(:,9), t4x8(:,:,9))
  call vert_GGG_G(ntry, ex4, ex1, ex2, wf8(:,10), n4(:,10), t4x8(:,:,10))
  call vert_GGG_G(ntry, ex5, ex6, ex3, wf8(:,11), n4(:,11), t4x8(:,:,11))
  call vert_GGG_G(ntry, ex6, ex3, ex5, wf8(:,12), n4(:,12), t4x8(:,:,12))
  call vert_GGG_G(ntry, ex1, ex2, ex5, wf8(:,13), n4(:,13), t4x8(:,:,13))
  call vert_GGG_G(ntry, ex3, ex4, ex6, wf8(:,14), n4(:,14), t4x8(:,:,14))
  call vert_GGG_G(ntry, ex2, ex5, ex1, wf8(:,15), n4(:,15), t4x8(:,:,15))
  call vert_GGG_G(ntry, ex5, ex1, ex2, wf8(:,16), n4(:,16), t4x8(:,:,16))
  call vert_GGG_G(ntry, ex4, ex6, ex3, wf8(:,17), n4(:,17), t4x8(:,:,17))
  call vert_GGG_G(ntry, ex6, ex3, ex4, wf8(:,18), n4(:,18), t4x8(:,:,18))
  call vert_GGG_G(ntry, ex1, ex2, ex6, wf8(:,19), n4(:,19), t4x8(:,:,19))
  call vert_GGG_G(ntry, ex3, ex4, ex5, wf8(:,20), n4(:,20), t4x8(:,:,20))
  call vert_GGG_G(ntry, ex2, ex6, ex1, wf8(:,21), n4(:,21), t4x8(:,:,21))
  call vert_GGG_G(ntry, ex6, ex1, ex2, wf8(:,22), n4(:,22), t4x8(:,:,22))
  call vert_GGG_G(ntry, ex4, ex5, ex3, wf8(:,23), n4(:,23), t4x8(:,:,23))
  call vert_GGG_G(ntry, ex5, ex3, ex4, wf8(:,24), n4(:,24), t4x8(:,:,24))
  call vert_GGG_G(ntry, ex1, ex3, ex4, wf8(:,25), n4(:,25), t4x8(:,:,25))
  call vert_GGG_G(ntry, ex2, ex5, ex6, wf8(:,26), n4(:,26), t4x8(:,:,26))
  call vert_GGG_G(ntry, ex3, ex4, ex1, wf8(:,27), n4(:,27), t4x8(:,:,27))
  call vert_GGG_G(ntry, ex4, ex1, ex3, wf8(:,28), n4(:,28), t4x8(:,:,28))
  call vert_GGG_G(ntry, ex5, ex6, ex2, wf8(:,29), n4(:,29), t4x8(:,:,29))
  call vert_GGG_G(ntry, ex6, ex2, ex5, wf8(:,30), n4(:,30), t4x8(:,:,30))
  call vert_GGG_G(ntry, ex1, ex3, ex5, wf8(:,31), n4(:,31), t4x8(:,:,31))
  call vert_GGG_G(ntry, ex2, ex4, ex6, wf8(:,32), n4(:,32), t4x8(:,:,32))
  call vert_GGG_G(ntry, ex3, ex5, ex1, wf8(:,33), n4(:,33), t4x8(:,:,33))
  call vert_GGG_G(ntry, ex5, ex1, ex3, wf8(:,34), n4(:,34), t4x8(:,:,34))
  call vert_GGG_G(ntry, ex4, ex6, ex2, wf8(:,35), n4(:,35), t4x8(:,:,35))
  call vert_GGG_G(ntry, ex6, ex2, ex4, wf8(:,36), n4(:,36), t4x8(:,:,36))
  call vert_GGG_G(ntry, ex1, ex3, ex6, wf8(:,37), n4(:,37), t4x8(:,:,37))
  call vert_GGG_G(ntry, ex2, ex4, ex5, wf8(:,38), n4(:,38), t4x8(:,:,38))
  call vert_GGG_G(ntry, ex3, ex6, ex1, wf8(:,39), n4(:,39), t4x8(:,:,39))
  call vert_GGG_G(ntry, ex6, ex1, ex3, wf8(:,40), n4(:,40), t4x8(:,:,40))
  call vert_GGG_G(ntry, ex4, ex5, ex2, wf8(:,41), n4(:,41), t4x8(:,:,41))
  call vert_GGG_G(ntry, ex5, ex2, ex4, wf8(:,42), n4(:,42), t4x8(:,:,42))
  call vert_GGG_G(ntry, ex1, ex4, ex5, wf8(:,43), n4(:,43), t4x8(:,:,43))
  call vert_GGG_G(ntry, ex2, ex3, ex6, wf8(:,44), n4(:,44), t4x8(:,:,44))
  call vert_GGG_G(ntry, ex4, ex5, ex1, wf8(:,45), n4(:,45), t4x8(:,:,45))
  call vert_GGG_G(ntry, ex5, ex1, ex4, wf8(:,46), n4(:,46), t4x8(:,:,46))
  call vert_GGG_G(ntry, ex3, ex6, ex2, wf8(:,47), n4(:,47), t4x8(:,:,47))
  call vert_GGG_G(ntry, ex6, ex2, ex3, wf8(:,48), n4(:,48), t4x8(:,:,48))
  call vert_GGG_G(ntry, ex1, ex4, ex6, wf8(:,49), n4(:,49), t4x8(:,:,49))
  call vert_GGG_G(ntry, ex2, ex3, ex5, wf8(:,50), n4(:,50), t4x8(:,:,50))
  call vert_GGG_G(ntry, ex4, ex6, ex1, wf8(:,51), n4(:,51), t4x8(:,:,51))
  call vert_GGG_G(ntry, ex6, ex1, ex4, wf8(:,52), n4(:,52), t4x8(:,:,52))
  call vert_GGG_G(ntry, ex3, ex5, ex2, wf8(:,53), n4(:,53), t4x8(:,:,53))
  call vert_GGG_G(ntry, ex5, ex2, ex3, wf8(:,54), n4(:,54), t4x8(:,:,54))
  call vert_GGG_G(ntry, ex1, ex5, ex6, wf8(:,55), n4(:,55), t4x8(:,:,55))
  call vert_GGG_G(ntry, ex2, ex3, ex4, wf8(:,56), n4(:,56), t4x8(:,:,56))
  call vert_GGG_G(ntry, ex5, ex6, ex1, wf8(:,57), n4(:,57), t4x8(:,:,57))
  call vert_GGG_G(ntry, ex6, ex1, ex5, wf8(:,58), n4(:,58), t4x8(:,:,58))
  call vert_GGG_G(ntry, ex3, ex4, ex2, wf8(:,59), n4(:,59), t4x8(:,:,59))
  call vert_GGG_G(ntry, ex4, ex2, ex3, wf8(:,60), n4(:,60), t4x8(:,:,60))
  call vert_UV_W(ntry, ex1, Q(:,1), ex2, Q(:,2), wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_UV_W(ntry, ex3, Q(:,4), ex4, Q(:,8), wf4(:,2), n3(:,2), t3x4(:,:,2))
  call vert_GGG_G(ntry, wf4(:,1), ex5, ex6, wf16(:,1), n4(:,61), t4x16(:,:,1))
  call vert_GGG_G(ntry, ex5, ex6, wf4(:,1), wf16(:,2), n4(:,62), t4x16(:,:,2))
  call vert_GGG_G(ntry, ex6, wf4(:,1), ex5, wf16(:,3), n4(:,63), t4x16(:,:,3))
  call vert_UV_W(ntry, ex3, Q(:,4), ex5, Q(:,16), wf4(:,3), n3(:,3), t3x4(:,:,3))
  call vert_GGG_G(ntry, wf4(:,1), ex4, ex6, wf16(:,4), n4(:,64), t4x16(:,:,4))
  call vert_GGG_G(ntry, ex4, ex6, wf4(:,1), wf16(:,5), n4(:,65), t4x16(:,:,5))
  call vert_GGG_G(ntry, ex6, wf4(:,1), ex4, wf16(:,6), n4(:,66), t4x16(:,:,6))
  call vert_UV_W(ntry, ex3, Q(:,4), ex6, Q(:,32), wf4(:,4), n3(:,4), t3x4(:,:,4))
  call vert_GGG_G(ntry, wf4(:,1), ex4, ex5, wf16(:,7), n4(:,67), t4x16(:,:,7))
  call vert_GGG_G(ntry, ex4, ex5, wf4(:,1), wf16(:,8), n4(:,68), t4x16(:,:,8))
  call vert_GGG_G(ntry, ex5, wf4(:,1), ex4, wf16(:,9), n4(:,69), t4x16(:,:,9))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), ex3, Q(:,4), wf8(:,61), n3(:,5), t3x8(:,:,1))
  call vert_UV_W(ntry, ex4, Q(:,8), ex5, Q(:,16), wf4(:,5), n3(:,6), t3x4(:,:,5))
  call vert_GGG_G(ntry, wf4(:,1), ex3, ex6, wf16(:,10), n4(:,70), t4x16(:,:,10))
  call vert_GGG_G(ntry, ex3, ex6, wf4(:,1), wf16(:,11), n4(:,71), t4x16(:,:,11))
  call vert_GGG_G(ntry, ex6, wf4(:,1), ex3, wf16(:,12), n4(:,72), t4x16(:,:,12))
  call vert_UV_W(ntry, ex4, Q(:,8), ex6, Q(:,32), wf4(:,6), n3(:,7), t3x4(:,:,6))
  call vert_GGG_G(ntry, wf4(:,1), ex3, ex5, wf16(:,13), n4(:,73), t4x16(:,:,13))
  call vert_GGG_G(ntry, ex3, ex5, wf4(:,1), wf16(:,14), n4(:,74), t4x16(:,:,14))
  call vert_GGG_G(ntry, ex5, wf4(:,1), ex3, wf16(:,15), n4(:,75), t4x16(:,:,15))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), ex4, Q(:,8), wf8(:,62), n3(:,8), t3x8(:,:,2))
  call vert_UV_W(ntry, ex5, Q(:,16), ex6, Q(:,32), wf4(:,7), n3(:,9), t3x4(:,:,7))
  call vert_GGG_G(ntry, wf4(:,1), ex3, ex4, wf16(:,16), n4(:,76), t4x16(:,:,16))
  call vert_GGG_G(ntry, ex3, ex4, wf4(:,1), wf16(:,17), n4(:,77), t4x16(:,:,17))
  call vert_GGG_G(ntry, ex4, wf4(:,1), ex3, wf16(:,18), n4(:,78), t4x16(:,:,18))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), ex5, Q(:,16), wf8(:,63), n3(:,10), t3x8(:,:,3))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), ex6, Q(:,32), wf8(:,64), n3(:,11), t3x8(:,:,4))
  call vert_UV_W(ntry, ex1, Q(:,1), ex3, Q(:,4), wf4(:,8), n3(:,12), t3x4(:,:,8))
  call vert_UV_W(ntry, ex2, Q(:,2), ex4, Q(:,8), wf4(:,9), n3(:,13), t3x4(:,:,9))
  call vert_GGG_G(ntry, wf4(:,8), ex5, ex6, wf16(:,19), n4(:,79), t4x16(:,:,19))
  call vert_GGG_G(ntry, ex5, ex6, wf4(:,8), wf16(:,20), n4(:,80), t4x16(:,:,20))
  call vert_GGG_G(ntry, ex6, wf4(:,8), ex5, wf16(:,21), n4(:,81), t4x16(:,:,21))
  call vert_UV_W(ntry, ex2, Q(:,2), ex5, Q(:,16), wf4(:,10), n3(:,14), t3x4(:,:,10))
  call vert_GGG_G(ntry, wf4(:,8), ex4, ex6, wf16(:,22), n4(:,82), t4x16(:,:,22))
  call vert_GGG_G(ntry, ex4, ex6, wf4(:,8), wf16(:,23), n4(:,83), t4x16(:,:,23))
  call vert_GGG_G(ntry, ex6, wf4(:,8), ex4, wf16(:,24), n4(:,84), t4x16(:,:,24))
  call vert_UV_W(ntry, ex2, Q(:,2), ex6, Q(:,32), wf4(:,11), n3(:,15), t3x4(:,:,11))
  call vert_GGG_G(ntry, wf4(:,8), ex4, ex5, wf16(:,25), n4(:,85), t4x16(:,:,25))
  call vert_GGG_G(ntry, ex4, ex5, wf4(:,8), wf16(:,26), n4(:,86), t4x16(:,:,26))
  call vert_GGG_G(ntry, ex5, wf4(:,8), ex4, wf16(:,27), n4(:,87), t4x16(:,:,27))
  call vert_UV_W(ntry, ex2, Q(:,2), wf4(:,8), Q(:,5), wf8(:,65), n3(:,16), t3x8(:,:,5))
  call vert_UV_W(ntry, ex1, Q(:,1), ex4, Q(:,8), wf4(:,12), n3(:,17), t3x4(:,:,12))
  call vert_UV_W(ntry, ex2, Q(:,2), ex3, Q(:,4), wf4(:,13), n3(:,18), t3x4(:,:,13))
  call vert_GGG_G(ntry, wf4(:,12), ex5, ex6, wf16(:,28), n4(:,88), t4x16(:,:,28))
  call vert_GGG_G(ntry, ex5, ex6, wf4(:,12), wf16(:,29), n4(:,89), t4x16(:,:,29))
  call vert_GGG_G(ntry, ex6, wf4(:,12), ex5, wf16(:,30), n4(:,90), t4x16(:,:,30))
  call vert_UV_W(ntry, ex1, Q(:,1), ex5, Q(:,16), wf4(:,14), n3(:,19), t3x4(:,:,14))
  call vert_GGG_G(ntry, ex4, wf4(:,14), ex6, wf16(:,31), n4(:,91), t4x16(:,:,31))
  call vert_GGG_G(ntry, wf4(:,14), ex6, ex4, wf16(:,32), n4(:,92), t4x16(:,:,32))
  call vert_GGG_G(ntry, ex6, ex4, wf4(:,14), wf16(:,33), n4(:,93), t4x16(:,:,33))
  call vert_UV_W(ntry, ex1, Q(:,1), ex6, Q(:,32), wf4(:,15), n3(:,20), t3x4(:,:,15))
  call vert_GGG_G(ntry, ex4, ex5, wf4(:,15), wf16(:,34), n4(:,94), t4x16(:,:,34))
  call vert_GGG_G(ntry, ex5, wf4(:,15), ex4, wf16(:,35), n4(:,95), t4x16(:,:,35))
  call vert_GGG_G(ntry, wf4(:,15), ex4, ex5, wf16(:,36), n4(:,96), t4x16(:,:,36))
  call vert_UV_W(ntry, ex1, Q(:,1), wf4(:,13), Q(:,6), wf8(:,66), n3(:,21), t3x8(:,:,6))
  call vert_GGG_G(ntry, ex3, wf4(:,12), ex6, wf16(:,37), n4(:,97), t4x16(:,:,37))
  call vert_GGG_G(ntry, wf4(:,12), ex6, ex3, wf16(:,38), n4(:,98), t4x16(:,:,38))
  call vert_GGG_G(ntry, ex6, ex3, wf4(:,12), wf16(:,39), n4(:,99), t4x16(:,:,39))
  call vert_GGG_G(ntry, ex3, wf4(:,12), ex5, wf16(:,40), n4(:,100), t4x16(:,:,40))
  call vert_GGG_G(ntry, wf4(:,12), ex5, ex3, wf16(:,41), n4(:,101), t4x16(:,:,41))
  call vert_GGG_G(ntry, ex5, ex3, wf4(:,12), wf16(:,42), n4(:,102), t4x16(:,:,42))
  call vert_UV_W(ntry, ex2, Q(:,2), wf4(:,12), Q(:,9), wf8(:,67), n3(:,22), t3x8(:,:,7))
  call vert_GGG_G(ntry, ex3, wf4(:,14), ex6, wf16(:,43), n4(:,103), t4x16(:,:,43))
  call vert_GGG_G(ntry, wf4(:,14), ex6, ex3, wf16(:,44), n4(:,104), t4x16(:,:,44))
  call vert_GGG_G(ntry, ex6, ex3, wf4(:,14), wf16(:,45), n4(:,105), t4x16(:,:,45))
  call vert_GGG_G(ntry, ex3, ex5, wf4(:,15), wf16(:,46), n4(:,106), t4x16(:,:,46))
  call vert_GGG_G(ntry, ex5, wf4(:,15), ex3, wf16(:,47), n4(:,107), t4x16(:,:,47))
  call vert_GGG_G(ntry, wf4(:,15), ex3, ex5, wf16(:,48), n4(:,108), t4x16(:,:,48))
  call vert_UV_W(ntry, ex1, Q(:,1), wf4(:,9), Q(:,10), wf8(:,68), n3(:,23), t3x8(:,:,8))
  call vert_GGG_G(ntry, ex3, ex4, wf4(:,14), wf16(:,49), n4(:,109), t4x16(:,:,49))
  call vert_GGG_G(ntry, ex4, wf4(:,14), ex3, wf16(:,50), n4(:,110), t4x16(:,:,50))
  call vert_GGG_G(ntry, wf4(:,14), ex3, ex4, wf16(:,51), n4(:,111), t4x16(:,:,51))
  call vert_UV_W(ntry, ex2, Q(:,2), wf4(:,14), Q(:,17), wf8(:,69), n3(:,24), t3x8(:,:,9))
  call vert_GGG_G(ntry, ex3, ex4, wf4(:,15), wf16(:,52), n4(:,112), t4x16(:,:,52))
  call vert_GGG_G(ntry, ex4, wf4(:,15), ex3, wf16(:,53), n4(:,113), t4x16(:,:,53))
  call vert_GGG_G(ntry, wf4(:,15), ex3, ex4, wf16(:,54), n4(:,114), t4x16(:,:,54))
  call vert_UV_W(ntry, ex1, Q(:,1), wf4(:,10), Q(:,18), wf8(:,70), n3(:,25), t3x8(:,:,10))
  call vert_UV_W(ntry, ex2, Q(:,2), wf4(:,15), Q(:,33), wf8(:,71), n3(:,26), t3x8(:,:,11))
  call vert_UV_W(ntry, ex1, Q(:,1), wf4(:,11), Q(:,34), wf8(:,72), n3(:,27), t3x8(:,:,12))
  call vert_GGG_G(ntry, ex2, wf4(:,8), ex6, wf16(:,55), n4(:,115), t4x16(:,:,55))
  call vert_GGG_G(ntry, wf4(:,8), ex6, ex2, wf16(:,56), n4(:,116), t4x16(:,:,56))
  call vert_GGG_G(ntry, ex6, ex2, wf4(:,8), wf16(:,57), n4(:,117), t4x16(:,:,57))
  call vert_GGG_G(ntry, ex2, wf4(:,8), ex5, wf16(:,58), n4(:,118), t4x16(:,:,58))
  call vert_GGG_G(ntry, wf4(:,8), ex5, ex2, wf16(:,59), n4(:,119), t4x16(:,:,59))
  call vert_GGG_G(ntry, ex5, ex2, wf4(:,8), wf16(:,60), n4(:,120), t4x16(:,:,60))
  call vert_UV_W(ntry, wf4(:,8), Q(:,5), ex4, Q(:,8), wf8(:,73), n3(:,28), t3x8(:,:,13))
  call vert_GGG_G(ntry, ex2, wf4(:,8), ex4, wf16(:,61), n4(:,121), t4x16(:,:,61))
  call vert_GGG_G(ntry, wf4(:,8), ex4, ex2, wf16(:,62), n4(:,122), t4x16(:,:,62))
  call vert_GGG_G(ntry, ex4, ex2, wf4(:,8), wf16(:,63), n4(:,123), t4x16(:,:,63))
  call vert_UV_W(ntry, wf4(:,8), Q(:,5), ex5, Q(:,16), wf8(:,74), n3(:,29), t3x8(:,:,14))
  call vert_UV_W(ntry, wf4(:,8), Q(:,5), ex6, Q(:,32), wf8(:,75), n3(:,30), t3x8(:,:,15))
  call vert_GGG_G(ntry, ex2, wf4(:,12), ex6, wf16(:,64), n4(:,124), t4x16(:,:,64))
  call vert_GGG_G(ntry, wf4(:,12), ex6, ex2, wf16(:,65), n4(:,125), t4x16(:,:,65))
  call vert_GGG_G(ntry, ex6, ex2, wf4(:,12), wf16(:,66), n4(:,126), t4x16(:,:,66))
  call vert_GGG_G(ntry, ex2, wf4(:,12), ex5, wf16(:,67), n4(:,127), t4x16(:,:,67))
  call vert_GGG_G(ntry, wf4(:,12), ex5, ex2, wf16(:,68), n4(:,128), t4x16(:,:,68))
  call vert_GGG_G(ntry, ex5, ex2, wf4(:,12), wf16(:,69), n4(:,129), t4x16(:,:,69))
  call vert_UV_W(ntry, ex3, Q(:,4), wf4(:,12), Q(:,9), wf8(:,76), n3(:,31), t3x8(:,:,16))
  call vert_GGG_G(ntry, ex2, wf4(:,14), ex6, wf16(:,70), n4(:,130), t4x16(:,:,70))
  call vert_GGG_G(ntry, wf4(:,14), ex6, ex2, wf16(:,71), n4(:,131), t4x16(:,:,71))
  call vert_GGG_G(ntry, ex6, ex2, wf4(:,14), wf16(:,72), n4(:,132), t4x16(:,:,72))
  call vert_GGG_G(ntry, ex2, ex5, wf4(:,15), wf16(:,73), n4(:,133), t4x16(:,:,73))
  call vert_GGG_G(ntry, ex5, wf4(:,15), ex2, wf16(:,74), n4(:,134), t4x16(:,:,74))
  call vert_GGG_G(ntry, wf4(:,15), ex2, ex5, wf16(:,75), n4(:,135), t4x16(:,:,75))
  call vert_UV_W(ntry, ex1, Q(:,1), wf4(:,2), Q(:,12), wf8(:,77), n3(:,32), t3x8(:,:,17))
  call vert_GGG_G(ntry, ex2, ex4, wf4(:,14), wf16(:,76), n4(:,136), t4x16(:,:,76))
  call vert_GGG_G(ntry, ex4, wf4(:,14), ex2, wf16(:,77), n4(:,137), t4x16(:,:,77))
  call vert_GGG_G(ntry, wf4(:,14), ex2, ex4, wf16(:,78), n4(:,138), t4x16(:,:,78))
  call vert_UV_W(ntry, ex3, Q(:,4), wf4(:,14), Q(:,17), wf8(:,78), n3(:,33), t3x8(:,:,18))
  call vert_GGG_G(ntry, ex2, ex4, wf4(:,15), wf16(:,79), n4(:,139), t4x16(:,:,79))
  call vert_GGG_G(ntry, ex4, wf4(:,15), ex2, wf16(:,80), n4(:,140), t4x16(:,:,80))
  call vert_GGG_G(ntry, wf4(:,15), ex2, ex4, wf16(:,81), n4(:,141), t4x16(:,:,81))
  call vert_UV_W(ntry, ex1, Q(:,1), wf4(:,3), Q(:,20), wf8(:,79), n3(:,34), t3x8(:,:,19))
  call vert_UV_W(ntry, ex3, Q(:,4), wf4(:,15), Q(:,33), wf8(:,80), n3(:,35), t3x8(:,:,20))
  call vert_UV_W(ntry, ex1, Q(:,1), wf4(:,4), Q(:,36), wf8(:,81), n3(:,36), t3x8(:,:,21))
  call vert_GGG_G(ntry, ex2, ex3, wf4(:,12), wf16(:,82), n4(:,142), t4x16(:,:,82))
  call vert_GGG_G(ntry, ex3, wf4(:,12), ex2, wf16(:,83), n4(:,143), t4x16(:,:,83))
  call vert_GGG_G(ntry, wf4(:,12), ex2, ex3, wf16(:,84), n4(:,144), t4x16(:,:,84))
  call vert_UV_W(ntry, wf4(:,12), Q(:,9), ex5, Q(:,16), wf8(:,82), n3(:,37), t3x8(:,:,22))
  call vert_UV_W(ntry, wf4(:,12), Q(:,9), ex6, Q(:,32), wf8(:,83), n3(:,38), t3x8(:,:,23))
  call vert_GGG_G(ntry, ex2, ex3, wf4(:,14), wf16(:,85), n4(:,145), t4x16(:,:,85))
  call vert_GGG_G(ntry, ex3, wf4(:,14), ex2, wf16(:,86), n4(:,146), t4x16(:,:,86))
  call vert_GGG_G(ntry, wf4(:,14), ex2, ex3, wf16(:,87), n4(:,147), t4x16(:,:,87))
  call vert_UV_W(ntry, ex4, Q(:,8), wf4(:,14), Q(:,17), wf8(:,84), n3(:,39), t3x8(:,:,24))
  call vert_GGG_G(ntry, ex2, ex3, wf4(:,15), wf16(:,88), n4(:,148), t4x16(:,:,88))
  call vert_GGG_G(ntry, ex3, wf4(:,15), ex2, wf16(:,89), n4(:,149), t4x16(:,:,89))
  call vert_GGG_G(ntry, wf4(:,15), ex2, ex3, wf16(:,90), n4(:,150), t4x16(:,:,90))
  call vert_UV_W(ntry, ex1, Q(:,1), wf4(:,5), Q(:,24), wf8(:,85), n3(:,40), t3x8(:,:,25))
  call vert_UV_W(ntry, ex4, Q(:,8), wf4(:,15), Q(:,33), wf8(:,86), n3(:,41), t3x8(:,:,26))
  call vert_UV_W(ntry, ex1, Q(:,1), wf4(:,6), Q(:,40), wf8(:,87), n3(:,42), t3x8(:,:,27))
  call vert_UV_W(ntry, wf4(:,14), Q(:,17), ex6, Q(:,32), wf8(:,88), n3(:,43), t3x8(:,:,28))
  call vert_UV_W(ntry, ex5, Q(:,16), wf4(:,15), Q(:,33), wf8(:,89), n3(:,44), t3x8(:,:,29))
  call vert_UV_W(ntry, ex1, Q(:,1), wf4(:,7), Q(:,48), wf8(:,90), n3(:,45), t3x8(:,:,30))
  call vert_GGG_G(ntry, ex1, wf4(:,13), ex6, wf16(:,91), n4(:,151), t4x16(:,:,91))
  call vert_GGG_G(ntry, wf4(:,13), ex6, ex1, wf16(:,92), n4(:,152), t4x16(:,:,92))
  call vert_GGG_G(ntry, ex6, ex1, wf4(:,13), wf16(:,93), n4(:,153), t4x16(:,:,93))
  call vert_GGG_G(ntry, ex1, wf4(:,13), ex5, wf16(:,94), n4(:,154), t4x16(:,:,94))
  call vert_GGG_G(ntry, wf4(:,13), ex5, ex1, wf16(:,95), n4(:,155), t4x16(:,:,95))
  call vert_GGG_G(ntry, ex5, ex1, wf4(:,13), wf16(:,96), n4(:,156), t4x16(:,:,96))
  call vert_UV_W(ntry, wf4(:,13), Q(:,6), ex4, Q(:,8), wf8(:,91), n3(:,46), t3x8(:,:,31))
  call vert_GGG_G(ntry, ex1, wf4(:,13), ex4, wf16(:,97), n4(:,157), t4x16(:,:,97))
  call vert_GGG_G(ntry, wf4(:,13), ex4, ex1, wf16(:,98), n4(:,158), t4x16(:,:,98))
  call vert_GGG_G(ntry, ex4, ex1, wf4(:,13), wf16(:,99), n4(:,159), t4x16(:,:,99))
  call vert_UV_W(ntry, wf4(:,13), Q(:,6), ex5, Q(:,16), wf8(:,92), n3(:,47), t3x8(:,:,32))
  call vert_UV_W(ntry, wf4(:,13), Q(:,6), ex6, Q(:,32), wf8(:,93), n3(:,48), t3x8(:,:,33))
  call vert_GGG_G(ntry, ex1, wf4(:,9), ex6, wf16(:,100), n4(:,160), t4x16(:,:,100))
  call vert_GGG_G(ntry, wf4(:,9), ex6, ex1, wf16(:,101), n4(:,161), t4x16(:,:,101))
  call vert_GGG_G(ntry, ex6, ex1, wf4(:,9), wf16(:,102), n4(:,162), t4x16(:,:,102))
  call vert_GGG_G(ntry, ex1, wf4(:,9), ex5, wf16(:,103), n4(:,163), t4x16(:,:,103))
  call vert_GGG_G(ntry, wf4(:,9), ex5, ex1, wf16(:,104), n4(:,164), t4x16(:,:,104))
  call vert_GGG_G(ntry, ex5, ex1, wf4(:,9), wf16(:,105), n4(:,165), t4x16(:,:,105))
  call vert_UV_W(ntry, ex3, Q(:,4), wf4(:,9), Q(:,10), wf8(:,94), n3(:,49), t3x8(:,:,34))
  call vert_GGG_G(ntry, ex1, wf4(:,10), ex6, wf16(:,106), n4(:,166), t4x16(:,:,106))
  call vert_GGG_G(ntry, wf4(:,10), ex6, ex1, wf16(:,107), n4(:,167), t4x16(:,:,107))
  call vert_GGG_G(ntry, ex6, ex1, wf4(:,10), wf16(:,108), n4(:,168), t4x16(:,:,108))
  call vert_GGG_G(ntry, ex1, ex5, wf4(:,11), wf16(:,109), n4(:,169), t4x16(:,:,109))
  call vert_GGG_G(ntry, ex5, wf4(:,11), ex1, wf16(:,110), n4(:,170), t4x16(:,:,110))
  call vert_GGG_G(ntry, wf4(:,11), ex1, ex5, wf16(:,111), n4(:,171), t4x16(:,:,111))
  call vert_UV_W(ntry, ex2, Q(:,2), wf4(:,2), Q(:,12), wf8(:,95), n3(:,50), t3x8(:,:,35))
  call vert_GGG_G(ntry, ex1, ex4, wf4(:,10), wf16(:,112), n4(:,172), t4x16(:,:,112))
  call vert_GGG_G(ntry, ex4, wf4(:,10), ex1, wf16(:,113), n4(:,173), t4x16(:,:,113))
  call vert_GGG_G(ntry, wf4(:,10), ex1, ex4, wf16(:,114), n4(:,174), t4x16(:,:,114))
  call vert_UV_W(ntry, ex3, Q(:,4), wf4(:,10), Q(:,18), wf8(:,96), n3(:,51), t3x8(:,:,36))
  call vert_GGG_G(ntry, ex1, ex4, wf4(:,11), wf16(:,115), n4(:,175), t4x16(:,:,115))
  call vert_GGG_G(ntry, ex4, wf4(:,11), ex1, wf16(:,116), n4(:,176), t4x16(:,:,116))
  call vert_GGG_G(ntry, wf4(:,11), ex1, ex4, wf16(:,117), n4(:,177), t4x16(:,:,117))
  call vert_UV_W(ntry, ex2, Q(:,2), wf4(:,3), Q(:,20), wf8(:,97), n3(:,52), t3x8(:,:,37))
  call vert_UV_W(ntry, ex3, Q(:,4), wf4(:,11), Q(:,34), wf8(:,98), n3(:,53), t3x8(:,:,38))
  call vert_UV_W(ntry, ex2, Q(:,2), wf4(:,4), Q(:,36), wf8(:,99), n3(:,54), t3x8(:,:,39))
  call vert_GGG_G(ntry, ex1, ex3, wf4(:,9), wf16(:,118), n4(:,178), t4x16(:,:,118))
  call vert_GGG_G(ntry, ex3, wf4(:,9), ex1, wf16(:,119), n4(:,179), t4x16(:,:,119))
  call vert_GGG_G(ntry, wf4(:,9), ex1, ex3, wf16(:,120), n4(:,180), t4x16(:,:,120))
  call vert_UV_W(ntry, wf4(:,9), Q(:,10), ex5, Q(:,16), wf8(:,100), n3(:,55), t3x8(:,:,40))
  call vert_UV_W(ntry, wf4(:,9), Q(:,10), ex6, Q(:,32), wf8(:,101), n3(:,56), t3x8(:,:,41))
  call vert_GGG_G(ntry, ex1, ex3, wf4(:,10), wf16(:,121), n4(:,181), t4x16(:,:,121))
  call vert_GGG_G(ntry, ex3, wf4(:,10), ex1, wf16(:,122), n4(:,182), t4x16(:,:,122))
  call vert_GGG_G(ntry, wf4(:,10), ex1, ex3, wf16(:,123), n4(:,183), t4x16(:,:,123))
  call vert_UV_W(ntry, ex4, Q(:,8), wf4(:,10), Q(:,18), wf8(:,102), n3(:,57), t3x8(:,:,42))
  call vert_GGG_G(ntry, ex1, ex3, wf4(:,11), wf16(:,124), n4(:,184), t4x16(:,:,124))
  call vert_GGG_G(ntry, ex3, wf4(:,11), ex1, wf16(:,125), n4(:,185), t4x16(:,:,125))
  call vert_GGG_G(ntry, wf4(:,11), ex1, ex3, wf16(:,126), n4(:,186), t4x16(:,:,126))
  call vert_UV_W(ntry, ex2, Q(:,2), wf4(:,5), Q(:,24), wf8(:,103), n3(:,58), t3x8(:,:,43))
  call vert_UV_W(ntry, ex4, Q(:,8), wf4(:,11), Q(:,34), wf8(:,104), n3(:,59), t3x8(:,:,44))
  call vert_UV_W(ntry, ex2, Q(:,2), wf4(:,6), Q(:,40), wf8(:,105), n3(:,60), t3x8(:,:,45))
  call vert_UV_W(ntry, wf4(:,10), Q(:,18), ex6, Q(:,32), wf8(:,106), n3(:,61), t3x8(:,:,46))
  call vert_UV_W(ntry, ex5, Q(:,16), wf4(:,11), Q(:,34), wf8(:,107), n3(:,62), t3x8(:,:,47))
  call vert_UV_W(ntry, ex2, Q(:,2), wf4(:,7), Q(:,48), wf8(:,108), n3(:,63), t3x8(:,:,48))
  call vert_GGG_G(ntry, ex1, ex2, wf4(:,2), wf16(:,127), n4(:,187), t4x16(:,:,127))
  call vert_GGG_G(ntry, ex2, wf4(:,2), ex1, wf16(:,128), n4(:,188), t4x16(:,:,128))
  call vert_GGG_G(ntry, wf4(:,2), ex1, ex2, wf16(:,129), n4(:,189), t4x16(:,:,129))
  call vert_UV_W(ntry, wf4(:,2), Q(:,12), ex5, Q(:,16), wf8(:,109), n3(:,64), t3x8(:,:,49))
  call vert_UV_W(ntry, wf4(:,2), Q(:,12), ex6, Q(:,32), wf8(:,110), n3(:,65), t3x8(:,:,50))
  call vert_GGG_G(ntry, ex1, ex2, wf4(:,3), wf16(:,130), n4(:,190), t4x16(:,:,130))
  call vert_GGG_G(ntry, ex2, wf4(:,3), ex1, wf16(:,131), n4(:,191), t4x16(:,:,131))
  call vert_GGG_G(ntry, wf4(:,3), ex1, ex2, wf16(:,132), n4(:,192), t4x16(:,:,132))
  call vert_UV_W(ntry, ex4, Q(:,8), wf4(:,3), Q(:,20), wf8(:,111), n3(:,66), t3x8(:,:,51))
  call vert_GGG_G(ntry, ex1, ex2, wf4(:,4), wf16(:,133), n4(:,193), t4x16(:,:,133))
  call vert_GGG_G(ntry, ex2, wf4(:,4), ex1, wf16(:,134), n4(:,194), t4x16(:,:,134))
  call vert_GGG_G(ntry, wf4(:,4), ex1, ex2, wf16(:,135), n4(:,195), t4x16(:,:,135))
  call vert_UV_W(ntry, ex3, Q(:,4), wf4(:,5), Q(:,24), wf8(:,112), n3(:,67), t3x8(:,:,52))
  call vert_UV_W(ntry, ex4, Q(:,8), wf4(:,4), Q(:,36), wf8(:,113), n3(:,68), t3x8(:,:,53))
  call vert_UV_W(ntry, ex3, Q(:,4), wf4(:,6), Q(:,40), wf8(:,114), n3(:,69), t3x8(:,:,54))
  call vert_UV_W(ntry, wf4(:,3), Q(:,20), ex6, Q(:,32), wf8(:,115), n3(:,70), t3x8(:,:,55))
  call vert_UV_W(ntry, ex5, Q(:,16), wf4(:,4), Q(:,36), wf8(:,116), n3(:,71), t3x8(:,:,56))
  call vert_UV_W(ntry, ex3, Q(:,4), wf4(:,7), Q(:,48), wf8(:,117), n3(:,72), t3x8(:,:,57))
  call vert_UV_W(ntry, wf4(:,5), Q(:,24), ex6, Q(:,32), wf8(:,118), n3(:,73), t3x8(:,:,58))
  call vert_UV_W(ntry, ex5, Q(:,16), wf4(:,6), Q(:,40), wf8(:,119), n3(:,74), t3x8(:,:,59))
  call vert_UV_W(ntry, ex4, Q(:,8), wf4(:,7), Q(:,48), wf8(:,120), n3(:,75), t3x8(:,:,60))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), wf4(:,2), Q(:,12), wf16(:,136), n3(:,76), t3x16(:,:,1))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), wf4(:,3), Q(:,20), wf16(:,137), n3(:,77), t3x16(:,:,2))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), wf4(:,4), Q(:,36), wf16(:,138), n3(:,78), t3x16(:,:,3))
  call vert_UV_W(ntry, wf4(:,8), Q(:,5), wf4(:,9), Q(:,10), wf16(:,139), n3(:,79), t3x16(:,:,4))
  call vert_UV_W(ntry, wf4(:,8), Q(:,5), wf4(:,10), Q(:,18), wf16(:,140), n3(:,80), t3x16(:,:,5))
  call vert_UV_W(ntry, wf4(:,8), Q(:,5), wf4(:,11), Q(:,34), wf16(:,141), n3(:,81), t3x16(:,:,6))
  call vert_UV_W(ntry, wf4(:,13), Q(:,6), wf4(:,12), Q(:,9), wf16(:,142), n3(:,82), t3x16(:,:,7))
  call vert_UV_W(ntry, wf4(:,13), Q(:,6), wf4(:,14), Q(:,17), wf16(:,143), n3(:,83), t3x16(:,:,8))
  call vert_UV_W(ntry, wf4(:,13), Q(:,6), wf4(:,15), Q(:,33), wf16(:,144), n3(:,84), t3x16(:,:,9))
  call vert_UV_W(ntry, wf4(:,12), Q(:,9), wf4(:,10), Q(:,18), wf16(:,145), n3(:,85), t3x16(:,:,10))
  call vert_UV_W(ntry, wf4(:,12), Q(:,9), wf4(:,11), Q(:,34), wf16(:,146), n3(:,86), t3x16(:,:,11))
  call vert_UV_W(ntry, wf4(:,9), Q(:,10), wf4(:,14), Q(:,17), wf16(:,147), n3(:,87), t3x16(:,:,12))
  call vert_UV_W(ntry, wf4(:,9), Q(:,10), wf4(:,15), Q(:,33), wf16(:,148), n3(:,88), t3x16(:,:,13))
  call vert_UV_W(ntry, wf4(:,14), Q(:,17), wf4(:,11), Q(:,34), wf16(:,149), n3(:,89), t3x16(:,:,14))
  call vert_UV_W(ntry, wf4(:,10), Q(:,18), wf4(:,15), Q(:,33), wf16(:,150), n3(:,90), t3x16(:,:,15))


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
  M2add = M2 / average_factor_ppjjjj_gggggg_1

  do k = 0, 23-1
    M2(k) = M2add(extcomb_perm_ppjjjj_gggggg_1(k))
  end do

  if (ntry == 1) ntry = 2

  if (scalefactor /= old_scalefactor) then
    scalebackfactor = scalefactor**(2*6-8)
    old_scalefactor = scalefactor
  end if
  M2 = scalebackfactor * M2

  contains

subroutine diagrams()
  implicit none
  ! e.g. call cont_VV(nsync, wf3, wf6, A(:,1), n64, t64, nhel, den(5)) ...

    call cont_VV(nsync, wf8(:,1), wf8(:,2), A(:,1), n3(:,91), t3x64(:,:,1), nhel, den(1))
    call cont_VV(nsync, wf8(:,2), wf8(:,3), A(:,2), n3(:,92), t3x64(:,:,2), nhel, den(1))
    call cont_VV(nsync, wf8(:,2), wf8(:,4), A(:,3), n3(:,93), t3x64(:,:,3), nhel, den(1))
    call cont_VV(nsync, wf8(:,1), wf8(:,5), A(:,4), n3(:,94), t3x64(:,:,4), nhel, den(1))
    call cont_VV(nsync, wf8(:,3), wf8(:,5), A(:,5), n3(:,95), t3x64(:,:,5), nhel, den(1))
    call cont_VV(nsync, wf8(:,4), wf8(:,5), A(:,6), n3(:,96), t3x64(:,:,6), nhel, den(1))
    call cont_VV(nsync, wf8(:,1), wf8(:,6), A(:,7), n3(:,97), t3x64(:,:,7), nhel, den(1))
    call cont_VV(nsync, wf8(:,3), wf8(:,6), A(:,8), n3(:,98), t3x64(:,:,8), nhel, den(1))
    call cont_VV(nsync, wf8(:,4), wf8(:,6), A(:,9), n3(:,99), t3x64(:,:,9), nhel, den(1))
    call cont_VV(nsync, wf8(:,7), wf8(:,8), A(:,10), n3(:,100), t3x64(:,:,10), nhel, den(2))
    call cont_VV(nsync, wf8(:,8), wf8(:,9), A(:,11), n3(:,101), t3x64(:,:,11), nhel, den(2))
    call cont_VV(nsync, wf8(:,8), wf8(:,10), A(:,12), n3(:,102), t3x64(:,:,12), nhel, den(2))
    call cont_VV(nsync, wf8(:,7), wf8(:,11), A(:,13), n3(:,103), t3x64(:,:,13), nhel, den(2))
    call cont_VV(nsync, wf8(:,9), wf8(:,11), A(:,14), n3(:,104), t3x64(:,:,14), nhel, den(2))
    call cont_VV(nsync, wf8(:,10), wf8(:,11), A(:,15), n3(:,105), t3x64(:,:,15), nhel, den(2))
    call cont_VV(nsync, wf8(:,7), wf8(:,12), A(:,16), n3(:,106), t3x64(:,:,16), nhel, den(2))
    call cont_VV(nsync, wf8(:,9), wf8(:,12), A(:,17), n3(:,107), t3x64(:,:,17), nhel, den(2))
    call cont_VV(nsync, wf8(:,10), wf8(:,12), A(:,18), n3(:,108), t3x64(:,:,18), nhel, den(2))
    call cont_VV(nsync, wf8(:,13), wf8(:,14), A(:,19), n3(:,109), t3x64(:,:,19), nhel, den(3))
    call cont_VV(nsync, wf8(:,14), wf8(:,15), A(:,20), n3(:,110), t3x64(:,:,20), nhel, den(3))
    call cont_VV(nsync, wf8(:,14), wf8(:,16), A(:,21), n3(:,111), t3x64(:,:,21), nhel, den(3))
    call cont_VV(nsync, wf8(:,13), wf8(:,17), A(:,22), n3(:,112), t3x64(:,:,22), nhel, den(3))
    call cont_VV(nsync, wf8(:,15), wf8(:,17), A(:,23), n3(:,113), t3x64(:,:,23), nhel, den(3))
    call cont_VV(nsync, wf8(:,16), wf8(:,17), A(:,24), n3(:,114), t3x64(:,:,24), nhel, den(3))
    call cont_VV(nsync, wf8(:,13), wf8(:,18), A(:,25), n3(:,115), t3x64(:,:,25), nhel, den(3))
    call cont_VV(nsync, wf8(:,15), wf8(:,18), A(:,26), n3(:,116), t3x64(:,:,26), nhel, den(3))
    call cont_VV(nsync, wf8(:,16), wf8(:,18), A(:,27), n3(:,117), t3x64(:,:,27), nhel, den(3))
    call cont_VV(nsync, wf8(:,19), wf8(:,20), A(:,28), n3(:,118), t3x64(:,:,28), nhel, den(4))
    call cont_VV(nsync, wf8(:,20), wf8(:,21), A(:,29), n3(:,119), t3x64(:,:,29), nhel, den(4))
    call cont_VV(nsync, wf8(:,20), wf8(:,22), A(:,30), n3(:,120), t3x64(:,:,30), nhel, den(4))
    call cont_VV(nsync, wf8(:,19), wf8(:,23), A(:,31), n3(:,121), t3x64(:,:,31), nhel, den(4))
    call cont_VV(nsync, wf8(:,21), wf8(:,23), A(:,32), n3(:,122), t3x64(:,:,32), nhel, den(4))
    call cont_VV(nsync, wf8(:,22), wf8(:,23), A(:,33), n3(:,123), t3x64(:,:,33), nhel, den(4))
    call cont_VV(nsync, wf8(:,19), wf8(:,24), A(:,34), n3(:,124), t3x64(:,:,34), nhel, den(4))
    call cont_VV(nsync, wf8(:,21), wf8(:,24), A(:,35), n3(:,125), t3x64(:,:,35), nhel, den(4))
    call cont_VV(nsync, wf8(:,22), wf8(:,24), A(:,36), n3(:,126), t3x64(:,:,36), nhel, den(4))
    call cont_VV(nsync, wf8(:,25), wf8(:,26), A(:,37), n3(:,127), t3x64(:,:,37), nhel, den(5))
    call cont_VV(nsync, wf8(:,26), wf8(:,27), A(:,38), n3(:,128), t3x64(:,:,38), nhel, den(5))
    call cont_VV(nsync, wf8(:,26), wf8(:,28), A(:,39), n3(:,129), t3x64(:,:,39), nhel, den(5))
    call cont_VV(nsync, wf8(:,25), wf8(:,29), A(:,40), n3(:,130), t3x64(:,:,40), nhel, den(5))
    call cont_VV(nsync, wf8(:,27), wf8(:,29), A(:,41), n3(:,131), t3x64(:,:,41), nhel, den(5))
    call cont_VV(nsync, wf8(:,28), wf8(:,29), A(:,42), n3(:,132), t3x64(:,:,42), nhel, den(5))
    call cont_VV(nsync, wf8(:,25), wf8(:,30), A(:,43), n3(:,133), t3x64(:,:,43), nhel, den(5))
    call cont_VV(nsync, wf8(:,27), wf8(:,30), A(:,44), n3(:,134), t3x64(:,:,44), nhel, den(5))
    call cont_VV(nsync, wf8(:,28), wf8(:,30), A(:,45), n3(:,135), t3x64(:,:,45), nhel, den(5))
    call cont_VV(nsync, wf8(:,31), wf8(:,32), A(:,46), n3(:,136), t3x64(:,:,46), nhel, den(6))
    call cont_VV(nsync, wf8(:,32), wf8(:,33), A(:,47), n3(:,137), t3x64(:,:,47), nhel, den(6))
    call cont_VV(nsync, wf8(:,32), wf8(:,34), A(:,48), n3(:,138), t3x64(:,:,48), nhel, den(6))
    call cont_VV(nsync, wf8(:,31), wf8(:,35), A(:,49), n3(:,139), t3x64(:,:,49), nhel, den(6))
    call cont_VV(nsync, wf8(:,33), wf8(:,35), A(:,50), n3(:,140), t3x64(:,:,50), nhel, den(6))
    call cont_VV(nsync, wf8(:,34), wf8(:,35), A(:,51), n3(:,141), t3x64(:,:,51), nhel, den(6))
    call cont_VV(nsync, wf8(:,31), wf8(:,36), A(:,52), n3(:,142), t3x64(:,:,52), nhel, den(6))
    call cont_VV(nsync, wf8(:,33), wf8(:,36), A(:,53), n3(:,143), t3x64(:,:,53), nhel, den(6))
    call cont_VV(nsync, wf8(:,34), wf8(:,36), A(:,54), n3(:,144), t3x64(:,:,54), nhel, den(6))
    call cont_VV(nsync, wf8(:,37), wf8(:,38), A(:,55), n3(:,145), t3x64(:,:,55), nhel, den(7))
    call cont_VV(nsync, wf8(:,38), wf8(:,39), A(:,56), n3(:,146), t3x64(:,:,56), nhel, den(7))
    call cont_VV(nsync, wf8(:,38), wf8(:,40), A(:,57), n3(:,147), t3x64(:,:,57), nhel, den(7))
    call cont_VV(nsync, wf8(:,37), wf8(:,41), A(:,58), n3(:,148), t3x64(:,:,58), nhel, den(7))
    call cont_VV(nsync, wf8(:,39), wf8(:,41), A(:,59), n3(:,149), t3x64(:,:,59), nhel, den(7))
    call cont_VV(nsync, wf8(:,40), wf8(:,41), A(:,60), n3(:,150), t3x64(:,:,60), nhel, den(7))
    call cont_VV(nsync, wf8(:,37), wf8(:,42), A(:,61), n3(:,151), t3x64(:,:,61), nhel, den(7))
    call cont_VV(nsync, wf8(:,39), wf8(:,42), A(:,62), n3(:,152), t3x64(:,:,62), nhel, den(7))
    call cont_VV(nsync, wf8(:,40), wf8(:,42), A(:,63), n3(:,153), t3x64(:,:,63), nhel, den(7))
    call cont_VV(nsync, wf8(:,43), wf8(:,44), A(:,64), n3(:,154), t3x64(:,:,64), nhel, den(8))
    call cont_VV(nsync, wf8(:,44), wf8(:,45), A(:,65), n3(:,155), t3x64(:,:,65), nhel, den(8))
    call cont_VV(nsync, wf8(:,44), wf8(:,46), A(:,66), n3(:,156), t3x64(:,:,66), nhel, den(8))
    call cont_VV(nsync, wf8(:,43), wf8(:,47), A(:,67), n3(:,157), t3x64(:,:,67), nhel, den(8))
    call cont_VV(nsync, wf8(:,45), wf8(:,47), A(:,68), n3(:,158), t3x64(:,:,68), nhel, den(8))
    call cont_VV(nsync, wf8(:,46), wf8(:,47), A(:,69), n3(:,159), t3x64(:,:,69), nhel, den(8))
    call cont_VV(nsync, wf8(:,43), wf8(:,48), A(:,70), n3(:,160), t3x64(:,:,70), nhel, den(8))
    call cont_VV(nsync, wf8(:,45), wf8(:,48), A(:,71), n3(:,161), t3x64(:,:,71), nhel, den(8))
    call cont_VV(nsync, wf8(:,46), wf8(:,48), A(:,72), n3(:,162), t3x64(:,:,72), nhel, den(8))
    call cont_VV(nsync, wf8(:,49), wf8(:,50), A(:,73), n3(:,163), t3x64(:,:,73), nhel, den(9))
    call cont_VV(nsync, wf8(:,50), wf8(:,51), A(:,74), n3(:,164), t3x64(:,:,74), nhel, den(9))
    call cont_VV(nsync, wf8(:,50), wf8(:,52), A(:,75), n3(:,165), t3x64(:,:,75), nhel, den(9))
    call cont_VV(nsync, wf8(:,49), wf8(:,53), A(:,76), n3(:,166), t3x64(:,:,76), nhel, den(9))
    call cont_VV(nsync, wf8(:,51), wf8(:,53), A(:,77), n3(:,167), t3x64(:,:,77), nhel, den(9))
    call cont_VV(nsync, wf8(:,52), wf8(:,53), A(:,78), n3(:,168), t3x64(:,:,78), nhel, den(9))
    call cont_VV(nsync, wf8(:,49), wf8(:,54), A(:,79), n3(:,169), t3x64(:,:,79), nhel, den(9))
    call cont_VV(nsync, wf8(:,51), wf8(:,54), A(:,80), n3(:,170), t3x64(:,:,80), nhel, den(9))
    call cont_VV(nsync, wf8(:,52), wf8(:,54), A(:,81), n3(:,171), t3x64(:,:,81), nhel, den(9))
    call cont_VV(nsync, wf8(:,55), wf8(:,56), A(:,82), n3(:,172), t3x64(:,:,82), nhel, den(10))
    call cont_VV(nsync, wf8(:,56), wf8(:,57), A(:,83), n3(:,173), t3x64(:,:,83), nhel, den(10))
    call cont_VV(nsync, wf8(:,56), wf8(:,58), A(:,84), n3(:,174), t3x64(:,:,84), nhel, den(10))
    call cont_VV(nsync, wf8(:,55), wf8(:,59), A(:,85), n3(:,175), t3x64(:,:,85), nhel, den(10))
    call cont_VV(nsync, wf8(:,57), wf8(:,59), A(:,86), n3(:,176), t3x64(:,:,86), nhel, den(10))
    call cont_VV(nsync, wf8(:,58), wf8(:,59), A(:,87), n3(:,177), t3x64(:,:,87), nhel, den(10))
    call cont_VV(nsync, wf8(:,55), wf8(:,60), A(:,88), n3(:,178), t3x64(:,:,88), nhel, den(10))
    call cont_VV(nsync, wf8(:,57), wf8(:,60), A(:,89), n3(:,179), t3x64(:,:,89), nhel, den(10))
    call cont_VV(nsync, wf8(:,58), wf8(:,60), A(:,90), n3(:,180), t3x64(:,:,90), nhel, den(10))
    call cont_VV(nsync, wf4(:,2), wf16(:,1), A(:,91), n3(:,181), t3x64(:,:,91), nhel, den(13))
    call cont_VV(nsync, wf4(:,2), wf16(:,2), A(:,92), n3(:,182), t3x64(:,:,92), nhel, den(13))
    call cont_VV(nsync, wf4(:,2), wf16(:,3), A(:,93), n3(:,183), t3x64(:,:,93), nhel, den(13))
    call cont_VV(nsync, wf4(:,3), wf16(:,4), A(:,94), n3(:,184), t3x64(:,:,94), nhel, den(15))
    call cont_VV(nsync, wf4(:,3), wf16(:,5), A(:,95), n3(:,185), t3x64(:,:,95), nhel, den(15))
    call cont_VV(nsync, wf4(:,3), wf16(:,6), A(:,96), n3(:,186), t3x64(:,:,96), nhel, den(15))
    call cont_VV(nsync, wf4(:,4), wf16(:,7), A(:,97), n3(:,187), t3x64(:,:,97), nhel, den(17))
    call cont_VV(nsync, wf4(:,4), wf16(:,8), A(:,98), n3(:,188), t3x64(:,:,98), nhel, den(17))
    call cont_VV(nsync, wf4(:,4), wf16(:,9), A(:,99), n3(:,189), t3x64(:,:,99), nhel, den(17))
    call cont_VV(nsync, wf8(:,2), wf8(:,61), A(:,100), n3(:,190), t3x64(:,:,100), nhel, den(19))
    call cont_VV(nsync, wf8(:,5), wf8(:,61), A(:,101), n3(:,191), t3x64(:,:,101), nhel, den(19))
    call cont_VV(nsync, wf8(:,6), wf8(:,61), A(:,102), n3(:,192), t3x64(:,:,102), nhel, den(19))
    call cont_VV(nsync, wf4(:,5), wf16(:,10), A(:,103), n3(:,193), t3x64(:,:,103), nhel, den(21))
    call cont_VV(nsync, wf4(:,5), wf16(:,11), A(:,104), n3(:,194), t3x64(:,:,104), nhel, den(21))
    call cont_VV(nsync, wf4(:,5), wf16(:,12), A(:,105), n3(:,195), t3x64(:,:,105), nhel, den(21))
    call cont_VV(nsync, wf4(:,6), wf16(:,13), A(:,106), n3(:,196), t3x64(:,:,106), nhel, den(23))
    call cont_VV(nsync, wf4(:,6), wf16(:,14), A(:,107), n3(:,197), t3x64(:,:,107), nhel, den(23))
    call cont_VV(nsync, wf4(:,6), wf16(:,15), A(:,108), n3(:,198), t3x64(:,:,108), nhel, den(23))
    call cont_VV(nsync, wf8(:,8), wf8(:,62), A(:,109), n3(:,199), t3x64(:,:,109), nhel, den(25))
    call cont_VV(nsync, wf8(:,11), wf8(:,62), A(:,110), n3(:,200), t3x64(:,:,110), nhel, den(25))
    call cont_VV(nsync, wf8(:,12), wf8(:,62), A(:,111), n3(:,201), t3x64(:,:,111), nhel, den(25))
    call cont_VV(nsync, wf4(:,7), wf16(:,16), A(:,112), n3(:,202), t3x64(:,:,112), nhel, den(27))
    call cont_VV(nsync, wf4(:,7), wf16(:,17), A(:,113), n3(:,203), t3x64(:,:,113), nhel, den(27))
    call cont_VV(nsync, wf4(:,7), wf16(:,18), A(:,114), n3(:,204), t3x64(:,:,114), nhel, den(27))
    call cont_VV(nsync, wf8(:,14), wf8(:,63), A(:,115), n3(:,205), t3x64(:,:,115), nhel, den(29))
    call cont_VV(nsync, wf8(:,17), wf8(:,63), A(:,116), n3(:,206), t3x64(:,:,116), nhel, den(29))
    call cont_VV(nsync, wf8(:,18), wf8(:,63), A(:,117), n3(:,207), t3x64(:,:,117), nhel, den(29))
    call cont_VV(nsync, wf8(:,20), wf8(:,64), A(:,118), n3(:,208), t3x64(:,:,118), nhel, den(31))
    call cont_VV(nsync, wf8(:,23), wf8(:,64), A(:,119), n3(:,209), t3x64(:,:,119), nhel, den(31))
    call cont_VV(nsync, wf8(:,24), wf8(:,64), A(:,120), n3(:,210), t3x64(:,:,120), nhel, den(31))
    call cont_VV(nsync, wf4(:,9), wf16(:,19), A(:,121), n3(:,211), t3x64(:,:,121), nhel, den(34))
    call cont_VV(nsync, wf4(:,9), wf16(:,20), A(:,122), n3(:,212), t3x64(:,:,122), nhel, den(34))
    call cont_VV(nsync, wf4(:,9), wf16(:,21), A(:,123), n3(:,213), t3x64(:,:,123), nhel, den(34))
    call cont_VV(nsync, wf4(:,10), wf16(:,22), A(:,124), n3(:,214), t3x64(:,:,124), nhel, den(36))
    call cont_VV(nsync, wf4(:,10), wf16(:,23), A(:,125), n3(:,215), t3x64(:,:,125), nhel, den(36))
    call cont_VV(nsync, wf4(:,10), wf16(:,24), A(:,126), n3(:,216), t3x64(:,:,126), nhel, den(36))
    call cont_VV(nsync, wf4(:,11), wf16(:,25), A(:,127), n3(:,217), t3x64(:,:,127), nhel, den(38))
    call cont_VV(nsync, wf4(:,11), wf16(:,26), A(:,128), n3(:,218), t3x64(:,:,128), nhel, den(38))
    call cont_VV(nsync, wf4(:,11), wf16(:,27), A(:,129), n3(:,219), t3x64(:,:,129), nhel, den(38))
    call cont_VV(nsync, wf8(:,2), wf8(:,65), A(:,130), n3(:,220), t3x64(:,:,130), nhel, den(39))
    call cont_VV(nsync, wf8(:,5), wf8(:,65), A(:,131), n3(:,221), t3x64(:,:,131), nhel, den(39))
    call cont_VV(nsync, wf8(:,6), wf8(:,65), A(:,132), n3(:,222), t3x64(:,:,132), nhel, den(39))
    call cont_VV(nsync, wf4(:,13), wf16(:,28), A(:,133), n3(:,223), t3x64(:,:,133), nhel, den(42))
    call cont_VV(nsync, wf4(:,13), wf16(:,29), A(:,134), n3(:,224), t3x64(:,:,134), nhel, den(42))
    call cont_VV(nsync, wf4(:,13), wf16(:,30), A(:,135), n3(:,225), t3x64(:,:,135), nhel, den(42))
    call cont_VV(nsync, wf4(:,13), wf16(:,31), A(:,136), n3(:,226), t3x64(:,:,136), nhel, den(44))
    call cont_VV(nsync, wf4(:,13), wf16(:,32), A(:,137), n3(:,227), t3x64(:,:,137), nhel, den(44))
    call cont_VV(nsync, wf4(:,13), wf16(:,33), A(:,138), n3(:,228), t3x64(:,:,138), nhel, den(44))
    call cont_VV(nsync, wf4(:,13), wf16(:,34), A(:,139), n3(:,229), t3x64(:,:,139), nhel, den(46))
    call cont_VV(nsync, wf4(:,13), wf16(:,35), A(:,140), n3(:,230), t3x64(:,:,140), nhel, den(46))
    call cont_VV(nsync, wf4(:,13), wf16(:,36), A(:,141), n3(:,231), t3x64(:,:,141), nhel, den(46))
    call cont_VV(nsync, wf8(:,2), wf8(:,66), A(:,142), n3(:,232), t3x64(:,:,142), nhel, den(47))
    call cont_VV(nsync, wf8(:,5), wf8(:,66), A(:,143), n3(:,233), t3x64(:,:,143), nhel, den(47))
    call cont_VV(nsync, wf8(:,6), wf8(:,66), A(:,144), n3(:,234), t3x64(:,:,144), nhel, den(47))
    call cont_VV(nsync, wf4(:,10), wf16(:,37), A(:,145), n3(:,235), t3x64(:,:,145), nhel, den(48))
    call cont_VV(nsync, wf4(:,10), wf16(:,38), A(:,146), n3(:,236), t3x64(:,:,146), nhel, den(48))
    call cont_VV(nsync, wf4(:,10), wf16(:,39), A(:,147), n3(:,237), t3x64(:,:,147), nhel, den(48))
    call cont_VV(nsync, wf4(:,11), wf16(:,40), A(:,148), n3(:,238), t3x64(:,:,148), nhel, den(49))
    call cont_VV(nsync, wf4(:,11), wf16(:,41), A(:,149), n3(:,239), t3x64(:,:,149), nhel, den(49))
    call cont_VV(nsync, wf4(:,11), wf16(:,42), A(:,150), n3(:,240), t3x64(:,:,150), nhel, den(49))
    call cont_VV(nsync, wf8(:,8), wf8(:,67), A(:,151), n3(:,241), t3x64(:,:,151), nhel, den(50))
    call cont_VV(nsync, wf8(:,11), wf8(:,67), A(:,152), n3(:,242), t3x64(:,:,152), nhel, den(50))
    call cont_VV(nsync, wf8(:,12), wf8(:,67), A(:,153), n3(:,243), t3x64(:,:,153), nhel, den(50))
    call cont_VV(nsync, wf4(:,9), wf16(:,43), A(:,154), n3(:,244), t3x64(:,:,154), nhel, den(51))
    call cont_VV(nsync, wf4(:,9), wf16(:,44), A(:,155), n3(:,245), t3x64(:,:,155), nhel, den(51))
    call cont_VV(nsync, wf4(:,9), wf16(:,45), A(:,156), n3(:,246), t3x64(:,:,156), nhel, den(51))
    call cont_VV(nsync, wf4(:,9), wf16(:,46), A(:,157), n3(:,247), t3x64(:,:,157), nhel, den(52))
    call cont_VV(nsync, wf4(:,9), wf16(:,47), A(:,158), n3(:,248), t3x64(:,:,158), nhel, den(52))
    call cont_VV(nsync, wf4(:,9), wf16(:,48), A(:,159), n3(:,249), t3x64(:,:,159), nhel, den(52))
    call cont_VV(nsync, wf8(:,8), wf8(:,68), A(:,160), n3(:,250), t3x64(:,:,160), nhel, den(53))
    call cont_VV(nsync, wf8(:,11), wf8(:,68), A(:,161), n3(:,251), t3x64(:,:,161), nhel, den(53))
    call cont_VV(nsync, wf8(:,12), wf8(:,68), A(:,162), n3(:,252), t3x64(:,:,162), nhel, den(53))
    call cont_VV(nsync, wf4(:,11), wf16(:,49), A(:,163), n3(:,253), t3x64(:,:,163), nhel, den(54))
    call cont_VV(nsync, wf4(:,11), wf16(:,50), A(:,164), n3(:,254), t3x64(:,:,164), nhel, den(54))
    call cont_VV(nsync, wf4(:,11), wf16(:,51), A(:,165), n3(:,255), t3x64(:,:,165), nhel, den(54))
    call cont_VV(nsync, wf8(:,14), wf8(:,69), A(:,166), n3(:,256), t3x64(:,:,166), nhel, den(55))
    call cont_VV(nsync, wf8(:,17), wf8(:,69), A(:,167), n3(:,257), t3x64(:,:,167), nhel, den(55))
    call cont_VV(nsync, wf8(:,18), wf8(:,69), A(:,168), n3(:,258), t3x64(:,:,168), nhel, den(55))
    call cont_VV(nsync, wf4(:,10), wf16(:,52), A(:,169), n3(:,259), t3x64(:,:,169), nhel, den(56))
    call cont_VV(nsync, wf4(:,10), wf16(:,53), A(:,170), n3(:,260), t3x64(:,:,170), nhel, den(56))
    call cont_VV(nsync, wf4(:,10), wf16(:,54), A(:,171), n3(:,261), t3x64(:,:,171), nhel, den(56))
    call cont_VV(nsync, wf8(:,14), wf8(:,70), A(:,172), n3(:,262), t3x64(:,:,172), nhel, den(57))
    call cont_VV(nsync, wf8(:,17), wf8(:,70), A(:,173), n3(:,263), t3x64(:,:,173), nhel, den(57))
    call cont_VV(nsync, wf8(:,18), wf8(:,70), A(:,174), n3(:,264), t3x64(:,:,174), nhel, den(57))
    call cont_VV(nsync, wf8(:,20), wf8(:,71), A(:,175), n3(:,265), t3x64(:,:,175), nhel, den(58))
    call cont_VV(nsync, wf8(:,23), wf8(:,71), A(:,176), n3(:,266), t3x64(:,:,176), nhel, den(58))
    call cont_VV(nsync, wf8(:,24), wf8(:,71), A(:,177), n3(:,267), t3x64(:,:,177), nhel, den(58))
    call cont_VV(nsync, wf8(:,20), wf8(:,72), A(:,178), n3(:,268), t3x64(:,:,178), nhel, den(59))
    call cont_VV(nsync, wf8(:,23), wf8(:,72), A(:,179), n3(:,269), t3x64(:,:,179), nhel, den(59))
    call cont_VV(nsync, wf8(:,24), wf8(:,72), A(:,180), n3(:,270), t3x64(:,:,180), nhel, den(59))
    call cont_VV(nsync, wf4(:,5), wf16(:,55), A(:,181), n3(:,271), t3x64(:,:,181), nhel, den(60))
    call cont_VV(nsync, wf4(:,5), wf16(:,56), A(:,182), n3(:,272), t3x64(:,:,182), nhel, den(60))
    call cont_VV(nsync, wf4(:,5), wf16(:,57), A(:,183), n3(:,273), t3x64(:,:,183), nhel, den(60))
    call cont_VV(nsync, wf4(:,6), wf16(:,58), A(:,184), n3(:,274), t3x64(:,:,184), nhel, den(61))
    call cont_VV(nsync, wf4(:,6), wf16(:,59), A(:,185), n3(:,275), t3x64(:,:,185), nhel, den(61))
    call cont_VV(nsync, wf4(:,6), wf16(:,60), A(:,186), n3(:,276), t3x64(:,:,186), nhel, den(61))
    call cont_VV(nsync, wf8(:,26), wf8(:,73), A(:,187), n3(:,277), t3x64(:,:,187), nhel, den(63))
    call cont_VV(nsync, wf8(:,29), wf8(:,73), A(:,188), n3(:,278), t3x64(:,:,188), nhel, den(63))
    call cont_VV(nsync, wf8(:,30), wf8(:,73), A(:,189), n3(:,279), t3x64(:,:,189), nhel, den(63))
    call cont_VV(nsync, wf4(:,7), wf16(:,61), A(:,190), n3(:,280), t3x64(:,:,190), nhel, den(64))
    call cont_VV(nsync, wf4(:,7), wf16(:,62), A(:,191), n3(:,281), t3x64(:,:,191), nhel, den(64))
    call cont_VV(nsync, wf4(:,7), wf16(:,63), A(:,192), n3(:,282), t3x64(:,:,192), nhel, den(64))
    call cont_VV(nsync, wf8(:,32), wf8(:,74), A(:,193), n3(:,283), t3x64(:,:,193), nhel, den(66))
    call cont_VV(nsync, wf8(:,35), wf8(:,74), A(:,194), n3(:,284), t3x64(:,:,194), nhel, den(66))
    call cont_VV(nsync, wf8(:,36), wf8(:,74), A(:,195), n3(:,285), t3x64(:,:,195), nhel, den(66))
    call cont_VV(nsync, wf8(:,38), wf8(:,75), A(:,196), n3(:,286), t3x64(:,:,196), nhel, den(68))
    call cont_VV(nsync, wf8(:,41), wf8(:,75), A(:,197), n3(:,287), t3x64(:,:,197), nhel, den(68))
    call cont_VV(nsync, wf8(:,42), wf8(:,75), A(:,198), n3(:,288), t3x64(:,:,198), nhel, den(68))
    call cont_VV(nsync, wf4(:,3), wf16(:,64), A(:,199), n3(:,289), t3x64(:,:,199), nhel, den(69))
    call cont_VV(nsync, wf4(:,3), wf16(:,65), A(:,200), n3(:,290), t3x64(:,:,200), nhel, den(69))
    call cont_VV(nsync, wf4(:,3), wf16(:,66), A(:,201), n3(:,291), t3x64(:,:,201), nhel, den(69))
    call cont_VV(nsync, wf4(:,4), wf16(:,67), A(:,202), n3(:,292), t3x64(:,:,202), nhel, den(70))
    call cont_VV(nsync, wf4(:,4), wf16(:,68), A(:,203), n3(:,293), t3x64(:,:,203), nhel, den(70))
    call cont_VV(nsync, wf4(:,4), wf16(:,69), A(:,204), n3(:,294), t3x64(:,:,204), nhel, den(70))
    call cont_VV(nsync, wf8(:,26), wf8(:,76), A(:,205), n3(:,295), t3x64(:,:,205), nhel, den(71))
    call cont_VV(nsync, wf8(:,29), wf8(:,76), A(:,206), n3(:,296), t3x64(:,:,206), nhel, den(71))
    call cont_VV(nsync, wf8(:,30), wf8(:,76), A(:,207), n3(:,297), t3x64(:,:,207), nhel, den(71))
    call cont_VV(nsync, wf4(:,2), wf16(:,70), A(:,208), n3(:,298), t3x64(:,:,208), nhel, den(72))
    call cont_VV(nsync, wf4(:,2), wf16(:,71), A(:,209), n3(:,299), t3x64(:,:,209), nhel, den(72))
    call cont_VV(nsync, wf4(:,2), wf16(:,72), A(:,210), n3(:,300), t3x64(:,:,210), nhel, den(72))
    call cont_VV(nsync, wf4(:,2), wf16(:,73), A(:,211), n3(:,301), t3x64(:,:,211), nhel, den(73))
    call cont_VV(nsync, wf4(:,2), wf16(:,74), A(:,212), n3(:,302), t3x64(:,:,212), nhel, den(73))
    call cont_VV(nsync, wf4(:,2), wf16(:,75), A(:,213), n3(:,303), t3x64(:,:,213), nhel, den(73))
    call cont_VV(nsync, wf8(:,26), wf8(:,77), A(:,214), n3(:,304), t3x64(:,:,214), nhel, den(74))
    call cont_VV(nsync, wf8(:,29), wf8(:,77), A(:,215), n3(:,305), t3x64(:,:,215), nhel, den(74))
    call cont_VV(nsync, wf8(:,30), wf8(:,77), A(:,216), n3(:,306), t3x64(:,:,216), nhel, den(74))
    call cont_VV(nsync, wf4(:,4), wf16(:,76), A(:,217), n3(:,307), t3x64(:,:,217), nhel, den(75))
    call cont_VV(nsync, wf4(:,4), wf16(:,77), A(:,218), n3(:,308), t3x64(:,:,218), nhel, den(75))
    call cont_VV(nsync, wf4(:,4), wf16(:,78), A(:,219), n3(:,309), t3x64(:,:,219), nhel, den(75))
    call cont_VV(nsync, wf8(:,32), wf8(:,78), A(:,220), n3(:,310), t3x64(:,:,220), nhel, den(76))
    call cont_VV(nsync, wf8(:,35), wf8(:,78), A(:,221), n3(:,311), t3x64(:,:,221), nhel, den(76))
    call cont_VV(nsync, wf8(:,36), wf8(:,78), A(:,222), n3(:,312), t3x64(:,:,222), nhel, den(76))
    call cont_VV(nsync, wf4(:,3), wf16(:,79), A(:,223), n3(:,313), t3x64(:,:,223), nhel, den(77))
    call cont_VV(nsync, wf4(:,3), wf16(:,80), A(:,224), n3(:,314), t3x64(:,:,224), nhel, den(77))
    call cont_VV(nsync, wf4(:,3), wf16(:,81), A(:,225), n3(:,315), t3x64(:,:,225), nhel, den(77))
    call cont_VV(nsync, wf8(:,32), wf8(:,79), A(:,226), n3(:,316), t3x64(:,:,226), nhel, den(78))
    call cont_VV(nsync, wf8(:,35), wf8(:,79), A(:,227), n3(:,317), t3x64(:,:,227), nhel, den(78))
    call cont_VV(nsync, wf8(:,36), wf8(:,79), A(:,228), n3(:,318), t3x64(:,:,228), nhel, den(78))
    call cont_VV(nsync, wf8(:,38), wf8(:,80), A(:,229), n3(:,319), t3x64(:,:,229), nhel, den(79))
    call cont_VV(nsync, wf8(:,41), wf8(:,80), A(:,230), n3(:,320), t3x64(:,:,230), nhel, den(79))
    call cont_VV(nsync, wf8(:,42), wf8(:,80), A(:,231), n3(:,321), t3x64(:,:,231), nhel, den(79))
    call cont_VV(nsync, wf8(:,38), wf8(:,81), A(:,232), n3(:,322), t3x64(:,:,232), nhel, den(80))
    call cont_VV(nsync, wf8(:,41), wf8(:,81), A(:,233), n3(:,323), t3x64(:,:,233), nhel, den(80))
    call cont_VV(nsync, wf8(:,42), wf8(:,81), A(:,234), n3(:,324), t3x64(:,:,234), nhel, den(80))
    call cont_VV(nsync, wf4(:,7), wf16(:,82), A(:,235), n3(:,325), t3x64(:,:,235), nhel, den(81))
    call cont_VV(nsync, wf4(:,7), wf16(:,83), A(:,236), n3(:,326), t3x64(:,:,236), nhel, den(81))
    call cont_VV(nsync, wf4(:,7), wf16(:,84), A(:,237), n3(:,327), t3x64(:,:,237), nhel, den(81))
    call cont_VV(nsync, wf8(:,44), wf8(:,82), A(:,238), n3(:,328), t3x64(:,:,238), nhel, den(83))
    call cont_VV(nsync, wf8(:,47), wf8(:,82), A(:,239), n3(:,329), t3x64(:,:,239), nhel, den(83))
    call cont_VV(nsync, wf8(:,48), wf8(:,82), A(:,240), n3(:,330), t3x64(:,:,240), nhel, den(83))
    call cont_VV(nsync, wf8(:,50), wf8(:,83), A(:,241), n3(:,331), t3x64(:,:,241), nhel, den(85))
    call cont_VV(nsync, wf8(:,53), wf8(:,83), A(:,242), n3(:,332), t3x64(:,:,242), nhel, den(85))
    call cont_VV(nsync, wf8(:,54), wf8(:,83), A(:,243), n3(:,333), t3x64(:,:,243), nhel, den(85))
    call cont_VV(nsync, wf4(:,6), wf16(:,85), A(:,244), n3(:,334), t3x64(:,:,244), nhel, den(86))
    call cont_VV(nsync, wf4(:,6), wf16(:,86), A(:,245), n3(:,335), t3x64(:,:,245), nhel, den(86))
    call cont_VV(nsync, wf4(:,6), wf16(:,87), A(:,246), n3(:,336), t3x64(:,:,246), nhel, den(86))
    call cont_VV(nsync, wf8(:,44), wf8(:,84), A(:,247), n3(:,337), t3x64(:,:,247), nhel, den(87))
    call cont_VV(nsync, wf8(:,47), wf8(:,84), A(:,248), n3(:,338), t3x64(:,:,248), nhel, den(87))
    call cont_VV(nsync, wf8(:,48), wf8(:,84), A(:,249), n3(:,339), t3x64(:,:,249), nhel, den(87))
    call cont_VV(nsync, wf4(:,5), wf16(:,88), A(:,250), n3(:,340), t3x64(:,:,250), nhel, den(88))
    call cont_VV(nsync, wf4(:,5), wf16(:,89), A(:,251), n3(:,341), t3x64(:,:,251), nhel, den(88))
    call cont_VV(nsync, wf4(:,5), wf16(:,90), A(:,252), n3(:,342), t3x64(:,:,252), nhel, den(88))
    call cont_VV(nsync, wf8(:,44), wf8(:,85), A(:,253), n3(:,343), t3x64(:,:,253), nhel, den(89))
    call cont_VV(nsync, wf8(:,47), wf8(:,85), A(:,254), n3(:,344), t3x64(:,:,254), nhel, den(89))
    call cont_VV(nsync, wf8(:,48), wf8(:,85), A(:,255), n3(:,345), t3x64(:,:,255), nhel, den(89))
    call cont_VV(nsync, wf8(:,50), wf8(:,86), A(:,256), n3(:,346), t3x64(:,:,256), nhel, den(90))
    call cont_VV(nsync, wf8(:,53), wf8(:,86), A(:,257), n3(:,347), t3x64(:,:,257), nhel, den(90))
    call cont_VV(nsync, wf8(:,54), wf8(:,86), A(:,258), n3(:,348), t3x64(:,:,258), nhel, den(90))
    call cont_VV(nsync, wf8(:,50), wf8(:,87), A(:,259), n3(:,349), t3x64(:,:,259), nhel, den(91))
    call cont_VV(nsync, wf8(:,53), wf8(:,87), A(:,260), n3(:,350), t3x64(:,:,260), nhel, den(91))
    call cont_VV(nsync, wf8(:,54), wf8(:,87), A(:,261), n3(:,351), t3x64(:,:,261), nhel, den(91))
    call cont_VV(nsync, wf8(:,56), wf8(:,88), A(:,262), n3(:,352), t3x64(:,:,262), nhel, den(93))
    call cont_VV(nsync, wf8(:,59), wf8(:,88), A(:,263), n3(:,353), t3x64(:,:,263), nhel, den(93))
    call cont_VV(nsync, wf8(:,60), wf8(:,88), A(:,264), n3(:,354), t3x64(:,:,264), nhel, den(93))
    call cont_VV(nsync, wf8(:,56), wf8(:,89), A(:,265), n3(:,355), t3x64(:,:,265), nhel, den(94))
    call cont_VV(nsync, wf8(:,59), wf8(:,89), A(:,266), n3(:,356), t3x64(:,:,266), nhel, den(94))
    call cont_VV(nsync, wf8(:,60), wf8(:,89), A(:,267), n3(:,357), t3x64(:,:,267), nhel, den(94))
    call cont_VV(nsync, wf8(:,56), wf8(:,90), A(:,268), n3(:,358), t3x64(:,:,268), nhel, den(95))
    call cont_VV(nsync, wf8(:,59), wf8(:,90), A(:,269), n3(:,359), t3x64(:,:,269), nhel, den(95))
    call cont_VV(nsync, wf8(:,60), wf8(:,90), A(:,270), n3(:,360), t3x64(:,:,270), nhel, den(95))
    call cont_VV(nsync, wf4(:,5), wf16(:,91), A(:,271), n3(:,361), t3x64(:,:,271), nhel, den(96))
    call cont_VV(nsync, wf4(:,5), wf16(:,92), A(:,272), n3(:,362), t3x64(:,:,272), nhel, den(96))
    call cont_VV(nsync, wf4(:,5), wf16(:,93), A(:,273), n3(:,363), t3x64(:,:,273), nhel, den(96))
    call cont_VV(nsync, wf4(:,6), wf16(:,94), A(:,274), n3(:,364), t3x64(:,:,274), nhel, den(97))
    call cont_VV(nsync, wf4(:,6), wf16(:,95), A(:,275), n3(:,365), t3x64(:,:,275), nhel, den(97))
    call cont_VV(nsync, wf4(:,6), wf16(:,96), A(:,276), n3(:,366), t3x64(:,:,276), nhel, den(97))
    call cont_VV(nsync, wf8(:,55), wf8(:,91), A(:,277), n3(:,367), t3x64(:,:,277), nhel, den(98))
    call cont_VV(nsync, wf8(:,57), wf8(:,91), A(:,278), n3(:,368), t3x64(:,:,278), nhel, den(98))
    call cont_VV(nsync, wf8(:,58), wf8(:,91), A(:,279), n3(:,369), t3x64(:,:,279), nhel, den(98))
    call cont_VV(nsync, wf4(:,7), wf16(:,97), A(:,280), n3(:,370), t3x64(:,:,280), nhel, den(99))
    call cont_VV(nsync, wf4(:,7), wf16(:,98), A(:,281), n3(:,371), t3x64(:,:,281), nhel, den(99))
    call cont_VV(nsync, wf4(:,7), wf16(:,99), A(:,282), n3(:,372), t3x64(:,:,282), nhel, den(99))
    call cont_VV(nsync, wf8(:,49), wf8(:,92), A(:,283), n3(:,373), t3x64(:,:,283), nhel, den(100))
    call cont_VV(nsync, wf8(:,51), wf8(:,92), A(:,284), n3(:,374), t3x64(:,:,284), nhel, den(100))
    call cont_VV(nsync, wf8(:,52), wf8(:,92), A(:,285), n3(:,375), t3x64(:,:,285), nhel, den(100))
    call cont_VV(nsync, wf8(:,43), wf8(:,93), A(:,286), n3(:,376), t3x64(:,:,286), nhel, den(101))
    call cont_VV(nsync, wf8(:,45), wf8(:,93), A(:,287), n3(:,377), t3x64(:,:,287), nhel, den(101))
    call cont_VV(nsync, wf8(:,46), wf8(:,93), A(:,288), n3(:,378), t3x64(:,:,288), nhel, den(101))
    call cont_VV(nsync, wf4(:,3), wf16(:,100), A(:,289), n3(:,379), t3x64(:,:,289), nhel, den(102))
    call cont_VV(nsync, wf4(:,3), wf16(:,101), A(:,290), n3(:,380), t3x64(:,:,290), nhel, den(102))
    call cont_VV(nsync, wf4(:,3), wf16(:,102), A(:,291), n3(:,381), t3x64(:,:,291), nhel, den(102))
    call cont_VV(nsync, wf4(:,4), wf16(:,103), A(:,292), n3(:,382), t3x64(:,:,292), nhel, den(103))
    call cont_VV(nsync, wf4(:,4), wf16(:,104), A(:,293), n3(:,383), t3x64(:,:,293), nhel, den(103))
    call cont_VV(nsync, wf4(:,4), wf16(:,105), A(:,294), n3(:,384), t3x64(:,:,294), nhel, den(103))
    call cont_VV(nsync, wf8(:,55), wf8(:,94), A(:,295), n3(:,385), t3x64(:,:,295), nhel, den(104))
    call cont_VV(nsync, wf8(:,57), wf8(:,94), A(:,296), n3(:,386), t3x64(:,:,296), nhel, den(104))
    call cont_VV(nsync, wf8(:,58), wf8(:,94), A(:,297), n3(:,387), t3x64(:,:,297), nhel, den(104))
    call cont_VV(nsync, wf4(:,2), wf16(:,106), A(:,298), n3(:,388), t3x64(:,:,298), nhel, den(105))
    call cont_VV(nsync, wf4(:,2), wf16(:,107), A(:,299), n3(:,389), t3x64(:,:,299), nhel, den(105))
    call cont_VV(nsync, wf4(:,2), wf16(:,108), A(:,300), n3(:,390), t3x64(:,:,300), nhel, den(105))
    call cont_VV(nsync, wf4(:,2), wf16(:,109), A(:,301), n3(:,391), t3x64(:,:,301), nhel, den(106))
    call cont_VV(nsync, wf4(:,2), wf16(:,110), A(:,302), n3(:,392), t3x64(:,:,302), nhel, den(106))
    call cont_VV(nsync, wf4(:,2), wf16(:,111), A(:,303), n3(:,393), t3x64(:,:,303), nhel, den(106))
    call cont_VV(nsync, wf8(:,55), wf8(:,95), A(:,304), n3(:,394), t3x64(:,:,304), nhel, den(107))
    call cont_VV(nsync, wf8(:,57), wf8(:,95), A(:,305), n3(:,395), t3x64(:,:,305), nhel, den(107))
    call cont_VV(nsync, wf8(:,58), wf8(:,95), A(:,306), n3(:,396), t3x64(:,:,306), nhel, den(107))
    call cont_VV(nsync, wf4(:,4), wf16(:,112), A(:,307), n3(:,397), t3x64(:,:,307), nhel, den(108))
    call cont_VV(nsync, wf4(:,4), wf16(:,113), A(:,308), n3(:,398), t3x64(:,:,308), nhel, den(108))
    call cont_VV(nsync, wf4(:,4), wf16(:,114), A(:,309), n3(:,399), t3x64(:,:,309), nhel, den(108))
    call cont_VV(nsync, wf8(:,49), wf8(:,96), A(:,310), n3(:,400), t3x64(:,:,310), nhel, den(109))
    call cont_VV(nsync, wf8(:,51), wf8(:,96), A(:,311), n3(:,401), t3x64(:,:,311), nhel, den(109))
    call cont_VV(nsync, wf8(:,52), wf8(:,96), A(:,312), n3(:,402), t3x64(:,:,312), nhel, den(109))
    call cont_VV(nsync, wf4(:,3), wf16(:,115), A(:,313), n3(:,403), t3x64(:,:,313), nhel, den(110))
    call cont_VV(nsync, wf4(:,3), wf16(:,116), A(:,314), n3(:,404), t3x64(:,:,314), nhel, den(110))
    call cont_VV(nsync, wf4(:,3), wf16(:,117), A(:,315), n3(:,405), t3x64(:,:,315), nhel, den(110))
    call cont_VV(nsync, wf8(:,49), wf8(:,97), A(:,316), n3(:,406), t3x64(:,:,316), nhel, den(111))
    call cont_VV(nsync, wf8(:,51), wf8(:,97), A(:,317), n3(:,407), t3x64(:,:,317), nhel, den(111))
    call cont_VV(nsync, wf8(:,52), wf8(:,97), A(:,318), n3(:,408), t3x64(:,:,318), nhel, den(111))
    call cont_VV(nsync, wf8(:,43), wf8(:,98), A(:,319), n3(:,409), t3x64(:,:,319), nhel, den(112))
    call cont_VV(nsync, wf8(:,45), wf8(:,98), A(:,320), n3(:,410), t3x64(:,:,320), nhel, den(112))
    call cont_VV(nsync, wf8(:,46), wf8(:,98), A(:,321), n3(:,411), t3x64(:,:,321), nhel, den(112))
    call cont_VV(nsync, wf8(:,43), wf8(:,99), A(:,322), n3(:,412), t3x64(:,:,322), nhel, den(113))
    call cont_VV(nsync, wf8(:,45), wf8(:,99), A(:,323), n3(:,413), t3x64(:,:,323), nhel, den(113))
    call cont_VV(nsync, wf8(:,46), wf8(:,99), A(:,324), n3(:,414), t3x64(:,:,324), nhel, den(113))
    call cont_VV(nsync, wf4(:,7), wf16(:,118), A(:,325), n3(:,415), t3x64(:,:,325), nhel, den(114))
    call cont_VV(nsync, wf4(:,7), wf16(:,119), A(:,326), n3(:,416), t3x64(:,:,326), nhel, den(114))
    call cont_VV(nsync, wf4(:,7), wf16(:,120), A(:,327), n3(:,417), t3x64(:,:,327), nhel, den(114))
    call cont_VV(nsync, wf8(:,37), wf8(:,100), A(:,328), n3(:,418), t3x64(:,:,328), nhel, den(115))
    call cont_VV(nsync, wf8(:,39), wf8(:,100), A(:,329), n3(:,419), t3x64(:,:,329), nhel, den(115))
    call cont_VV(nsync, wf8(:,40), wf8(:,100), A(:,330), n3(:,420), t3x64(:,:,330), nhel, den(115))
    call cont_VV(nsync, wf8(:,31), wf8(:,101), A(:,331), n3(:,421), t3x64(:,:,331), nhel, den(116))
    call cont_VV(nsync, wf8(:,33), wf8(:,101), A(:,332), n3(:,422), t3x64(:,:,332), nhel, den(116))
    call cont_VV(nsync, wf8(:,34), wf8(:,101), A(:,333), n3(:,423), t3x64(:,:,333), nhel, den(116))
    call cont_VV(nsync, wf4(:,6), wf16(:,121), A(:,334), n3(:,424), t3x64(:,:,334), nhel, den(117))
    call cont_VV(nsync, wf4(:,6), wf16(:,122), A(:,335), n3(:,425), t3x64(:,:,335), nhel, den(117))
    call cont_VV(nsync, wf4(:,6), wf16(:,123), A(:,336), n3(:,426), t3x64(:,:,336), nhel, den(117))
    call cont_VV(nsync, wf8(:,37), wf8(:,102), A(:,337), n3(:,427), t3x64(:,:,337), nhel, den(118))
    call cont_VV(nsync, wf8(:,39), wf8(:,102), A(:,338), n3(:,428), t3x64(:,:,338), nhel, den(118))
    call cont_VV(nsync, wf8(:,40), wf8(:,102), A(:,339), n3(:,429), t3x64(:,:,339), nhel, den(118))
    call cont_VV(nsync, wf4(:,5), wf16(:,124), A(:,340), n3(:,430), t3x64(:,:,340), nhel, den(119))
    call cont_VV(nsync, wf4(:,5), wf16(:,125), A(:,341), n3(:,431), t3x64(:,:,341), nhel, den(119))
    call cont_VV(nsync, wf4(:,5), wf16(:,126), A(:,342), n3(:,432), t3x64(:,:,342), nhel, den(119))
    call cont_VV(nsync, wf8(:,37), wf8(:,103), A(:,343), n3(:,433), t3x64(:,:,343), nhel, den(120))
    call cont_VV(nsync, wf8(:,39), wf8(:,103), A(:,344), n3(:,434), t3x64(:,:,344), nhel, den(120))
    call cont_VV(nsync, wf8(:,40), wf8(:,103), A(:,345), n3(:,435), t3x64(:,:,345), nhel, den(120))
    call cont_VV(nsync, wf8(:,31), wf8(:,104), A(:,346), n3(:,436), t3x64(:,:,346), nhel, den(121))
    call cont_VV(nsync, wf8(:,33), wf8(:,104), A(:,347), n3(:,437), t3x64(:,:,347), nhel, den(121))
    call cont_VV(nsync, wf8(:,34), wf8(:,104), A(:,348), n3(:,438), t3x64(:,:,348), nhel, den(121))
    call cont_VV(nsync, wf8(:,31), wf8(:,105), A(:,349), n3(:,439), t3x64(:,:,349), nhel, den(122))
    call cont_VV(nsync, wf8(:,33), wf8(:,105), A(:,350), n3(:,440), t3x64(:,:,350), nhel, den(122))
    call cont_VV(nsync, wf8(:,34), wf8(:,105), A(:,351), n3(:,441), t3x64(:,:,351), nhel, den(122))
    call cont_VV(nsync, wf8(:,25), wf8(:,106), A(:,352), n3(:,442), t3x64(:,:,352), nhel, den(123))
    call cont_VV(nsync, wf8(:,27), wf8(:,106), A(:,353), n3(:,443), t3x64(:,:,353), nhel, den(123))
    call cont_VV(nsync, wf8(:,28), wf8(:,106), A(:,354), n3(:,444), t3x64(:,:,354), nhel, den(123))
    call cont_VV(nsync, wf8(:,25), wf8(:,107), A(:,355), n3(:,445), t3x64(:,:,355), nhel, den(124))
    call cont_VV(nsync, wf8(:,27), wf8(:,107), A(:,356), n3(:,446), t3x64(:,:,356), nhel, den(124))
    call cont_VV(nsync, wf8(:,28), wf8(:,107), A(:,357), n3(:,447), t3x64(:,:,357), nhel, den(124))
    call cont_VV(nsync, wf8(:,25), wf8(:,108), A(:,358), n3(:,448), t3x64(:,:,358), nhel, den(125))
    call cont_VV(nsync, wf8(:,27), wf8(:,108), A(:,359), n3(:,449), t3x64(:,:,359), nhel, den(125))
    call cont_VV(nsync, wf8(:,28), wf8(:,108), A(:,360), n3(:,450), t3x64(:,:,360), nhel, den(125))
    call cont_VV(nsync, wf4(:,7), wf16(:,127), A(:,361), n3(:,451), t3x64(:,:,361), nhel, den(126))
    call cont_VV(nsync, wf4(:,7), wf16(:,128), A(:,362), n3(:,452), t3x64(:,:,362), nhel, den(126))
    call cont_VV(nsync, wf4(:,7), wf16(:,129), A(:,363), n3(:,453), t3x64(:,:,363), nhel, den(126))
    call cont_VV(nsync, wf8(:,19), wf8(:,109), A(:,364), n3(:,454), t3x64(:,:,364), nhel, den(127))
    call cont_VV(nsync, wf8(:,21), wf8(:,109), A(:,365), n3(:,455), t3x64(:,:,365), nhel, den(127))
    call cont_VV(nsync, wf8(:,22), wf8(:,109), A(:,366), n3(:,456), t3x64(:,:,366), nhel, den(127))
    call cont_VV(nsync, wf8(:,13), wf8(:,110), A(:,367), n3(:,457), t3x64(:,:,367), nhel, den(128))
    call cont_VV(nsync, wf8(:,15), wf8(:,110), A(:,368), n3(:,458), t3x64(:,:,368), nhel, den(128))
    call cont_VV(nsync, wf8(:,16), wf8(:,110), A(:,369), n3(:,459), t3x64(:,:,369), nhel, den(128))
    call cont_VV(nsync, wf4(:,6), wf16(:,130), A(:,370), n3(:,460), t3x64(:,:,370), nhel, den(129))
    call cont_VV(nsync, wf4(:,6), wf16(:,131), A(:,371), n3(:,461), t3x64(:,:,371), nhel, den(129))
    call cont_VV(nsync, wf4(:,6), wf16(:,132), A(:,372), n3(:,462), t3x64(:,:,372), nhel, den(129))
    call cont_VV(nsync, wf8(:,19), wf8(:,111), A(:,373), n3(:,463), t3x64(:,:,373), nhel, den(130))
    call cont_VV(nsync, wf8(:,21), wf8(:,111), A(:,374), n3(:,464), t3x64(:,:,374), nhel, den(130))
    call cont_VV(nsync, wf8(:,22), wf8(:,111), A(:,375), n3(:,465), t3x64(:,:,375), nhel, den(130))
    call cont_VV(nsync, wf4(:,5), wf16(:,133), A(:,376), n3(:,466), t3x64(:,:,376), nhel, den(131))
    call cont_VV(nsync, wf4(:,5), wf16(:,134), A(:,377), n3(:,467), t3x64(:,:,377), nhel, den(131))
    call cont_VV(nsync, wf4(:,5), wf16(:,135), A(:,378), n3(:,468), t3x64(:,:,378), nhel, den(131))
    call cont_VV(nsync, wf8(:,19), wf8(:,112), A(:,379), n3(:,469), t3x64(:,:,379), nhel, den(132))
    call cont_VV(nsync, wf8(:,21), wf8(:,112), A(:,380), n3(:,470), t3x64(:,:,380), nhel, den(132))
    call cont_VV(nsync, wf8(:,22), wf8(:,112), A(:,381), n3(:,471), t3x64(:,:,381), nhel, den(132))
    call cont_VV(nsync, wf8(:,13), wf8(:,113), A(:,382), n3(:,472), t3x64(:,:,382), nhel, den(133))
    call cont_VV(nsync, wf8(:,15), wf8(:,113), A(:,383), n3(:,473), t3x64(:,:,383), nhel, den(133))
    call cont_VV(nsync, wf8(:,16), wf8(:,113), A(:,384), n3(:,474), t3x64(:,:,384), nhel, den(133))
    call cont_VV(nsync, wf8(:,13), wf8(:,114), A(:,385), n3(:,475), t3x64(:,:,385), nhel, den(134))
    call cont_VV(nsync, wf8(:,15), wf8(:,114), A(:,386), n3(:,476), t3x64(:,:,386), nhel, den(134))
    call cont_VV(nsync, wf8(:,16), wf8(:,114), A(:,387), n3(:,477), t3x64(:,:,387), nhel, den(134))
    call cont_VV(nsync, wf8(:,7), wf8(:,115), A(:,388), n3(:,478), t3x64(:,:,388), nhel, den(135))
    call cont_VV(nsync, wf8(:,9), wf8(:,115), A(:,389), n3(:,479), t3x64(:,:,389), nhel, den(135))
    call cont_VV(nsync, wf8(:,10), wf8(:,115), A(:,390), n3(:,480), t3x64(:,:,390), nhel, den(135))
    call cont_VV(nsync, wf8(:,7), wf8(:,116), A(:,391), n3(:,481), t3x64(:,:,391), nhel, den(136))
    call cont_VV(nsync, wf8(:,9), wf8(:,116), A(:,392), n3(:,482), t3x64(:,:,392), nhel, den(136))
    call cont_VV(nsync, wf8(:,10), wf8(:,116), A(:,393), n3(:,483), t3x64(:,:,393), nhel, den(136))
    call cont_VV(nsync, wf8(:,7), wf8(:,117), A(:,394), n3(:,484), t3x64(:,:,394), nhel, den(137))
    call cont_VV(nsync, wf8(:,9), wf8(:,117), A(:,395), n3(:,485), t3x64(:,:,395), nhel, den(137))
    call cont_VV(nsync, wf8(:,10), wf8(:,117), A(:,396), n3(:,486), t3x64(:,:,396), nhel, den(137))
    call cont_VV(nsync, wf8(:,1), wf8(:,118), A(:,397), n3(:,487), t3x64(:,:,397), nhel, den(138))
    call cont_VV(nsync, wf8(:,3), wf8(:,118), A(:,398), n3(:,488), t3x64(:,:,398), nhel, den(138))
    call cont_VV(nsync, wf8(:,4), wf8(:,118), A(:,399), n3(:,489), t3x64(:,:,399), nhel, den(138))
    call cont_VV(nsync, wf8(:,1), wf8(:,119), A(:,400), n3(:,490), t3x64(:,:,400), nhel, den(139))
    call cont_VV(nsync, wf8(:,3), wf8(:,119), A(:,401), n3(:,491), t3x64(:,:,401), nhel, den(139))
    call cont_VV(nsync, wf8(:,4), wf8(:,119), A(:,402), n3(:,492), t3x64(:,:,402), nhel, den(139))
    call cont_VV(nsync, wf8(:,1), wf8(:,120), A(:,403), n3(:,493), t3x64(:,:,403), nhel, den(140))
    call cont_VV(nsync, wf8(:,3), wf8(:,120), A(:,404), n3(:,494), t3x64(:,:,404), nhel, den(140))
    call cont_VV(nsync, wf8(:,4), wf8(:,120), A(:,405), n3(:,495), t3x64(:,:,405), nhel, den(140))
    call cont_VV(nsync, wf4(:,7), wf16(:,136), A(:,406), n3(:,496), t3x64(:,:,406), nhel, den(141))
    call cont_VV(nsync, wf8(:,63), wf8(:,110), A(:,407), n3(:,497), t3x64(:,:,407), nhel, den(143))
    call cont_VV(nsync, wf8(:,64), wf8(:,109), A(:,408), n3(:,498), t3x64(:,:,408), nhel, den(145))
    call cont_VV(nsync, wf4(:,6), wf16(:,137), A(:,409), n3(:,499), t3x64(:,:,409), nhel, den(146))
    call cont_VV(nsync, wf8(:,62), wf8(:,115), A(:,410), n3(:,500), t3x64(:,:,410), nhel, den(148))
    call cont_VV(nsync, wf8(:,64), wf8(:,111), A(:,411), n3(:,501), t3x64(:,:,411), nhel, den(150))
    call cont_VV(nsync, wf4(:,5), wf16(:,138), A(:,412), n3(:,502), t3x64(:,:,412), nhel, den(151))
    call cont_VV(nsync, wf8(:,61), wf8(:,118), A(:,413), n3(:,503), t3x64(:,:,413), nhel, den(153))
    call cont_VV(nsync, wf8(:,64), wf8(:,112), A(:,414), n3(:,504), t3x64(:,:,414), nhel, den(155))
    call cont_VV(nsync, wf8(:,62), wf8(:,116), A(:,415), n3(:,505), t3x64(:,:,415), nhel, den(156))
    call cont_VV(nsync, wf8(:,63), wf8(:,113), A(:,416), n3(:,506), t3x64(:,:,416), nhel, den(158))
    call cont_VV(nsync, wf8(:,61), wf8(:,119), A(:,417), n3(:,507), t3x64(:,:,417), nhel, den(159))
    call cont_VV(nsync, wf8(:,63), wf8(:,114), A(:,418), n3(:,508), t3x64(:,:,418), nhel, den(161))
    call cont_VV(nsync, wf8(:,61), wf8(:,120), A(:,419), n3(:,509), t3x64(:,:,419), nhel, den(162))
    call cont_VV(nsync, wf8(:,62), wf8(:,117), A(:,420), n3(:,510), t3x64(:,:,420), nhel, den(164))
    call cont_VV(nsync, wf4(:,7), wf16(:,139), A(:,421), n3(:,511), t3x64(:,:,421), nhel, den(165))
    call cont_VV(nsync, wf8(:,74), wf8(:,101), A(:,422), n3(:,512), t3x64(:,:,422), nhel, den(167))
    call cont_VV(nsync, wf8(:,75), wf8(:,100), A(:,423), n3(:,513), t3x64(:,:,423), nhel, den(169))
    call cont_VV(nsync, wf4(:,6), wf16(:,140), A(:,424), n3(:,514), t3x64(:,:,424), nhel, den(170))
    call cont_VV(nsync, wf8(:,73), wf8(:,106), A(:,425), n3(:,515), t3x64(:,:,425), nhel, den(172))
    call cont_VV(nsync, wf8(:,75), wf8(:,102), A(:,426), n3(:,516), t3x64(:,:,426), nhel, den(174))
    call cont_VV(nsync, wf4(:,5), wf16(:,141), A(:,427), n3(:,517), t3x64(:,:,427), nhel, den(175))
    call cont_VV(nsync, wf8(:,65), wf8(:,118), A(:,428), n3(:,518), t3x64(:,:,428), nhel, den(177))
    call cont_VV(nsync, wf8(:,75), wf8(:,103), A(:,429), n3(:,519), t3x64(:,:,429), nhel, den(179))
    call cont_VV(nsync, wf8(:,73), wf8(:,107), A(:,430), n3(:,520), t3x64(:,:,430), nhel, den(180))
    call cont_VV(nsync, wf8(:,74), wf8(:,104), A(:,431), n3(:,521), t3x64(:,:,431), nhel, den(182))
    call cont_VV(nsync, wf8(:,65), wf8(:,119), A(:,432), n3(:,522), t3x64(:,:,432), nhel, den(183))
    call cont_VV(nsync, wf8(:,74), wf8(:,105), A(:,433), n3(:,523), t3x64(:,:,433), nhel, den(185))
    call cont_VV(nsync, wf8(:,65), wf8(:,120), A(:,434), n3(:,524), t3x64(:,:,434), nhel, den(186))
    call cont_VV(nsync, wf8(:,73), wf8(:,108), A(:,435), n3(:,525), t3x64(:,:,435), nhel, den(188))
    call cont_VV(nsync, wf4(:,7), wf16(:,142), A(:,436), n3(:,526), t3x64(:,:,436), nhel, den(189))
    call cont_VV(nsync, wf8(:,82), wf8(:,93), A(:,437), n3(:,527), t3x64(:,:,437), nhel, den(191))
    call cont_VV(nsync, wf8(:,83), wf8(:,92), A(:,438), n3(:,528), t3x64(:,:,438), nhel, den(193))
    call cont_VV(nsync, wf4(:,6), wf16(:,143), A(:,439), n3(:,529), t3x64(:,:,439), nhel, den(194))
    call cont_VV(nsync, wf8(:,84), wf8(:,93), A(:,440), n3(:,530), t3x64(:,:,440), nhel, den(196))
    call cont_VV(nsync, wf8(:,88), wf8(:,91), A(:,441), n3(:,531), t3x64(:,:,441), nhel, den(198))
    call cont_VV(nsync, wf4(:,5), wf16(:,144), A(:,442), n3(:,532), t3x64(:,:,442), nhel, den(199))
    call cont_VV(nsync, wf8(:,66), wf8(:,118), A(:,443), n3(:,533), t3x64(:,:,443), nhel, den(201))
    call cont_VV(nsync, wf8(:,85), wf8(:,93), A(:,444), n3(:,534), t3x64(:,:,444), nhel, den(203))
    call cont_VV(nsync, wf8(:,86), wf8(:,92), A(:,445), n3(:,535), t3x64(:,:,445), nhel, den(205))
    call cont_VV(nsync, wf8(:,89), wf8(:,91), A(:,446), n3(:,536), t3x64(:,:,446), nhel, den(206))
    call cont_VV(nsync, wf8(:,66), wf8(:,119), A(:,447), n3(:,537), t3x64(:,:,447), nhel, den(207))
    call cont_VV(nsync, wf8(:,87), wf8(:,92), A(:,448), n3(:,538), t3x64(:,:,448), nhel, den(209))
    call cont_VV(nsync, wf8(:,66), wf8(:,120), A(:,449), n3(:,539), t3x64(:,:,449), nhel, den(210))
    call cont_VV(nsync, wf8(:,90), wf8(:,91), A(:,450), n3(:,540), t3x64(:,:,450), nhel, den(212))
    call cont_VV(nsync, wf4(:,4), wf16(:,145), A(:,451), n3(:,541), t3x64(:,:,451), nhel, den(213))
    call cont_VV(nsync, wf8(:,76), wf8(:,106), A(:,452), n3(:,542), t3x64(:,:,452), nhel, den(215))
    call cont_VV(nsync, wf8(:,83), wf8(:,96), A(:,453), n3(:,543), t3x64(:,:,453), nhel, den(217))
    call cont_VV(nsync, wf4(:,3), wf16(:,146), A(:,454), n3(:,544), t3x64(:,:,454), nhel, den(218))
    call cont_VV(nsync, wf8(:,67), wf8(:,115), A(:,455), n3(:,545), t3x64(:,:,455), nhel, den(220))
    call cont_VV(nsync, wf8(:,83), wf8(:,97), A(:,456), n3(:,546), t3x64(:,:,456), nhel, den(222))
    call cont_VV(nsync, wf8(:,76), wf8(:,107), A(:,457), n3(:,547), t3x64(:,:,457), nhel, den(223))
    call cont_VV(nsync, wf8(:,82), wf8(:,98), A(:,458), n3(:,548), t3x64(:,:,458), nhel, den(225))
    call cont_VV(nsync, wf8(:,67), wf8(:,116), A(:,459), n3(:,549), t3x64(:,:,459), nhel, den(226))
    call cont_VV(nsync, wf8(:,82), wf8(:,99), A(:,460), n3(:,550), t3x64(:,:,460), nhel, den(228))
    call cont_VV(nsync, wf8(:,67), wf8(:,117), A(:,461), n3(:,551), t3x64(:,:,461), nhel, den(229))
    call cont_VV(nsync, wf8(:,76), wf8(:,108), A(:,462), n3(:,552), t3x64(:,:,462), nhel, den(230))
    call cont_VV(nsync, wf4(:,4), wf16(:,147), A(:,463), n3(:,553), t3x64(:,:,463), nhel, den(231))
    call cont_VV(nsync, wf8(:,78), wf8(:,101), A(:,464), n3(:,554), t3x64(:,:,464), nhel, den(233))
    call cont_VV(nsync, wf8(:,88), wf8(:,94), A(:,465), n3(:,555), t3x64(:,:,465), nhel, den(235))
    call cont_VV(nsync, wf4(:,3), wf16(:,148), A(:,466), n3(:,556), t3x64(:,:,466), nhel, den(236))
    call cont_VV(nsync, wf8(:,68), wf8(:,115), A(:,467), n3(:,557), t3x64(:,:,467), nhel, den(238))
    call cont_VV(nsync, wf8(:,79), wf8(:,101), A(:,468), n3(:,558), t3x64(:,:,468), nhel, den(240))
    call cont_VV(nsync, wf8(:,80), wf8(:,100), A(:,469), n3(:,559), t3x64(:,:,469), nhel, den(242))
    call cont_VV(nsync, wf8(:,89), wf8(:,94), A(:,470), n3(:,560), t3x64(:,:,470), nhel, den(243))
    call cont_VV(nsync, wf8(:,68), wf8(:,116), A(:,471), n3(:,561), t3x64(:,:,471), nhel, den(244))
    call cont_VV(nsync, wf8(:,81), wf8(:,100), A(:,472), n3(:,562), t3x64(:,:,472), nhel, den(246))
    call cont_VV(nsync, wf8(:,68), wf8(:,117), A(:,473), n3(:,563), t3x64(:,:,473), nhel, den(247))
    call cont_VV(nsync, wf8(:,90), wf8(:,94), A(:,474), n3(:,564), t3x64(:,:,474), nhel, den(248))
    call cont_VV(nsync, wf4(:,2), wf16(:,149), A(:,475), n3(:,565), t3x64(:,:,475), nhel, den(249))
    call cont_VV(nsync, wf8(:,69), wf8(:,110), A(:,476), n3(:,566), t3x64(:,:,476), nhel, den(251))
    call cont_VV(nsync, wf8(:,88), wf8(:,95), A(:,477), n3(:,567), t3x64(:,:,477), nhel, den(253))
    call cont_VV(nsync, wf4(:,2), wf16(:,150), A(:,478), n3(:,568), t3x64(:,:,478), nhel, den(254))
    call cont_VV(nsync, wf8(:,70), wf8(:,110), A(:,479), n3(:,569), t3x64(:,:,479), nhel, den(256))
    call cont_VV(nsync, wf8(:,77), wf8(:,106), A(:,480), n3(:,570), t3x64(:,:,480), nhel, den(258))
    call cont_VV(nsync, wf8(:,71), wf8(:,109), A(:,481), n3(:,571), t3x64(:,:,481), nhel, den(260))
    call cont_VV(nsync, wf8(:,89), wf8(:,95), A(:,482), n3(:,572), t3x64(:,:,482), nhel, den(261))
    call cont_VV(nsync, wf8(:,72), wf8(:,109), A(:,483), n3(:,573), t3x64(:,:,483), nhel, den(263))
    call cont_VV(nsync, wf8(:,77), wf8(:,107), A(:,484), n3(:,574), t3x64(:,:,484), nhel, den(264))
    call cont_VV(nsync, wf8(:,77), wf8(:,108), A(:,485), n3(:,575), t3x64(:,:,485), nhel, den(265))
    call cont_VV(nsync, wf8(:,90), wf8(:,95), A(:,486), n3(:,576), t3x64(:,:,486), nhel, den(266))
    call cont_VV(nsync, wf8(:,78), wf8(:,104), A(:,487), n3(:,577), t3x64(:,:,487), nhel, den(267))
    call cont_VV(nsync, wf8(:,84), wf8(:,98), A(:,488), n3(:,578), t3x64(:,:,488), nhel, den(268))
    call cont_VV(nsync, wf8(:,69), wf8(:,113), A(:,489), n3(:,579), t3x64(:,:,489), nhel, den(269))
    call cont_VV(nsync, wf8(:,84), wf8(:,99), A(:,490), n3(:,580), t3x64(:,:,490), nhel, den(270))
    call cont_VV(nsync, wf8(:,69), wf8(:,114), A(:,491), n3(:,581), t3x64(:,:,491), nhel, den(271))
    call cont_VV(nsync, wf8(:,78), wf8(:,105), A(:,492), n3(:,582), t3x64(:,:,492), nhel, den(272))
    call cont_VV(nsync, wf8(:,80), wf8(:,102), A(:,493), n3(:,583), t3x64(:,:,493), nhel, den(273))
    call cont_VV(nsync, wf8(:,86), wf8(:,96), A(:,494), n3(:,584), t3x64(:,:,494), nhel, den(274))
    call cont_VV(nsync, wf8(:,70), wf8(:,113), A(:,495), n3(:,585), t3x64(:,:,495), nhel, den(275))
    call cont_VV(nsync, wf8(:,81), wf8(:,102), A(:,496), n3(:,586), t3x64(:,:,496), nhel, den(276))
    call cont_VV(nsync, wf8(:,70), wf8(:,114), A(:,497), n3(:,587), t3x64(:,:,497), nhel, den(277))
    call cont_VV(nsync, wf8(:,87), wf8(:,96), A(:,498), n3(:,588), t3x64(:,:,498), nhel, den(278))
    call cont_VV(nsync, wf8(:,71), wf8(:,111), A(:,499), n3(:,589), t3x64(:,:,499), nhel, den(279))
    call cont_VV(nsync, wf8(:,86), wf8(:,97), A(:,500), n3(:,590), t3x64(:,:,500), nhel, den(280))
    call cont_VV(nsync, wf8(:,72), wf8(:,111), A(:,501), n3(:,591), t3x64(:,:,501), nhel, den(281))
    call cont_VV(nsync, wf8(:,79), wf8(:,104), A(:,502), n3(:,592), t3x64(:,:,502), nhel, den(282))
    call cont_VV(nsync, wf8(:,79), wf8(:,105), A(:,503), n3(:,593), t3x64(:,:,503), nhel, den(283))
    call cont_VV(nsync, wf8(:,87), wf8(:,97), A(:,504), n3(:,594), t3x64(:,:,504), nhel, den(284))
    call cont_VV(nsync, wf8(:,71), wf8(:,112), A(:,505), n3(:,595), t3x64(:,:,505), nhel, den(285))
    call cont_VV(nsync, wf8(:,80), wf8(:,103), A(:,506), n3(:,596), t3x64(:,:,506), nhel, den(286))
    call cont_VV(nsync, wf8(:,72), wf8(:,112), A(:,507), n3(:,597), t3x64(:,:,507), nhel, den(287))
    call cont_VV(nsync, wf8(:,85), wf8(:,98), A(:,508), n3(:,598), t3x64(:,:,508), nhel, den(288))
    call cont_VV(nsync, wf8(:,81), wf8(:,103), A(:,509), n3(:,599), t3x64(:,:,509), nhel, den(289))
    call cont_VV(nsync, wf8(:,85), wf8(:,99), A(:,510), n3(:,600), t3x64(:,:,510), nhel, den(290))

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
  complex(REALKIND), intent(out) :: M1(120) ! M1helarray(120,64)
  integer :: empty(0)

  M1(  1) = 2*(A(j,1)%j-A(j,2)%j-A(j,4)%j+A(j,5)%j-A(j,28)%j+A(j,30)%j+A(j,31)%j-A(j,33)%j+A(j,83)%j-A(j,84)%j-A(j,86)%j+A(j,87)%j &
         +A(j,92)%j-A(j,93)%j+A(j,100)%j-A(j,101)%j-A(j,103)%j+A(j,105)%j+A(j,112)%j-A(j,113)%j-A(j,118)%j+A(j,119)%j-A(j,139)%j &
         +A(j,140)%j+A(j,142)%j-A(j,143)%j+A(j,175)%j-A(j,176)%j-A(j,212)%j+A(j,213)%j+A(j,250)%j-A(j,252)%j-A(j,265)%j+A(j,266)%j &
         -A(j,268)%j+A(j,269)%j-A(j,271)%j+A(j,273)%j+A(j,278)%j-A(j,279)%j+A(j,280)%j-A(j,281)%j+A(j,305)%j-A(j,306)%j+A(j,361)%j &
         -A(j,362)%j-A(j,364)%j+A(j,366)%j-A(j,379)%j+A(j,381)%j+A(j,397)%j-A(j,398)%j+A(j,403)%j-A(j,404)%j+A(j,406)%j-A(j,408)%j &
         +A(j,413)%j-A(j,414)%j+A(j,419)%j+A(j,442)%j+A(j,443)%j-A(j,446)%j+A(j,449)%j-A(j,450)%j+A(j,481)%j-A(j,482)%j-A(j,486)%j &
         +A(j,505)%j)*f(1)
  M1(  2) = 2*(A(j,4)%j-A(j,5)%j-A(j,7)%j+A(j,8)%j-A(j,19)%j+A(j,21)%j+A(j,22)%j-A(j,24)%j+A(j,82)%j-A(j,83)%j-A(j,85)%j+A(j,86)%j &
         +A(j,91)%j-A(j,92)%j+A(j,101)%j-A(j,102)%j-A(j,106)%j+A(j,108)%j-A(j,112)%j+A(j,113)%j-A(j,115)%j+A(j,116)%j-A(j,137)%j &
         +A(j,138)%j+A(j,143)%j-A(j,144)%j+A(j,166)%j-A(j,167)%j-A(j,208)%j+A(j,209)%j+A(j,244)%j-A(j,246)%j+A(j,262)%j-A(j,263)%j &
         +A(j,268)%j-A(j,269)%j-A(j,274)%j+A(j,276)%j+A(j,277)%j-A(j,278)%j-A(j,280)%j+A(j,281)%j+A(j,304)%j-A(j,305)%j-A(j,361)%j &
         +A(j,362)%j-A(j,367)%j+A(j,369)%j-A(j,385)%j+A(j,387)%j-A(j,400)%j+A(j,401)%j-A(j,403)%j+A(j,404)%j-A(j,406)%j-A(j,407)%j &
         -A(j,417)%j-A(j,418)%j-A(j,419)%j+A(j,439)%j+A(j,441)%j-A(j,447)%j-A(j,449)%j+A(j,450)%j+A(j,476)%j+A(j,477)%j+A(j,486)%j &
         +A(j,491)%j)*f(1)
  M1(  3) = 2*(-A(j,1)%j+A(j,2)%j+A(j,7)%j-A(j,8)%j-A(j,31)%j+A(j,33)%j+A(j,34)%j-A(j,36)%j+A(j,74)%j-A(j,75)%j-A(j,77)%j &
         +A(j,78)%j+A(j,95)%j-A(j,96)%j-A(j,100)%j+A(j,102)%j+A(j,103)%j-A(j,105)%j+A(j,106)%j-A(j,107)%j-A(j,119)%j+A(j,120)%j &
         +A(j,139)%j-A(j,141)%j-A(j,142)%j+A(j,144)%j+A(j,176)%j-A(j,177)%j-A(j,224)%j+A(j,225)%j-A(j,250)%j+A(j,252)%j-A(j,256)%j &
         +A(j,257)%j-A(j,259)%j+A(j,260)%j+A(j,271)%j-A(j,273)%j+A(j,274)%j-A(j,275)%j+A(j,284)%j-A(j,285)%j+A(j,317)%j-A(j,318)%j &
         +A(j,370)%j-A(j,371)%j+A(j,373)%j-A(j,375)%j+A(j,379)%j-A(j,381)%j-A(j,397)%j+A(j,398)%j+A(j,400)%j-A(j,401)%j+A(j,409)%j &
         +A(j,411)%j-A(j,413)%j+A(j,414)%j+A(j,417)%j-A(j,442)%j-A(j,443)%j-A(j,445)%j+A(j,447)%j-A(j,448)%j-A(j,499)%j-A(j,500)%j &
         -A(j,504)%j-A(j,505)%j)*f(1)
  M1(  4) = 2*(A(j,4)%j-A(j,5)%j-A(j,7)%j+A(j,8)%j-A(j,10)%j+A(j,12)%j+A(j,13)%j-A(j,15)%j+A(j,73)%j-A(j,74)%j-A(j,76)%j+A(j,77)%j &
         +A(j,94)%j-A(j,95)%j+A(j,101)%j-A(j,102)%j-A(j,106)%j+A(j,107)%j-A(j,109)%j+A(j,110)%j-A(j,112)%j+A(j,114)%j-A(j,134)%j &
         +A(j,135)%j+A(j,143)%j-A(j,144)%j+A(j,151)%j-A(j,152)%j-A(j,199)%j+A(j,200)%j+A(j,235)%j-A(j,237)%j+A(j,241)%j-A(j,242)%j &
         +A(j,259)%j-A(j,260)%j-A(j,274)%j+A(j,275)%j-A(j,280)%j+A(j,282)%j+A(j,283)%j-A(j,284)%j+A(j,316)%j-A(j,317)%j-A(j,370)%j &
         +A(j,371)%j-A(j,388)%j+A(j,390)%j-A(j,394)%j+A(j,396)%j-A(j,400)%j+A(j,401)%j-A(j,403)%j+A(j,404)%j-A(j,409)%j-A(j,410)%j &
         -A(j,417)%j-A(j,419)%j-A(j,420)%j+A(j,436)%j+A(j,438)%j-A(j,447)%j+A(j,448)%j-A(j,449)%j+A(j,455)%j+A(j,456)%j+A(j,461)%j &
         +A(j,504)%j)*f(1)
  M1(  5) = 2*(-A(j,1)%j+A(j,2)%j+A(j,7)%j-A(j,8)%j-A(j,22)%j+A(j,24)%j+A(j,25)%j-A(j,27)%j+A(j,65)%j-A(j,66)%j-A(j,68)%j &
         +A(j,69)%j+A(j,98)%j-A(j,99)%j-A(j,100)%j+A(j,102)%j+A(j,103)%j-A(j,104)%j+A(j,106)%j-A(j,108)%j-A(j,116)%j+A(j,117)%j &
         +A(j,136)%j-A(j,138)%j-A(j,142)%j+A(j,144)%j+A(j,167)%j-A(j,168)%j-A(j,218)%j+A(j,219)%j-A(j,244)%j+A(j,246)%j-A(j,247)%j &
         +A(j,248)%j-A(j,253)%j+A(j,254)%j+A(j,271)%j-A(j,272)%j+A(j,274)%j-A(j,276)%j+A(j,287)%j-A(j,288)%j+A(j,323)%j-A(j,324)%j &
         +A(j,376)%j-A(j,377)%j+A(j,382)%j-A(j,384)%j+A(j,385)%j-A(j,387)%j-A(j,397)%j+A(j,398)%j+A(j,400)%j-A(j,401)%j+A(j,412)%j &
         -A(j,413)%j+A(j,416)%j+A(j,417)%j+A(j,418)%j-A(j,439)%j-A(j,440)%j-A(j,443)%j-A(j,444)%j+A(j,447)%j-A(j,489)%j-A(j,490)%j &
         -A(j,491)%j-A(j,510)%j)*f(1)
  M1(  6) = 2*(A(j,1)%j-A(j,2)%j-A(j,4)%j+A(j,5)%j-A(j,13)%j+A(j,15)%j+A(j,16)%j-A(j,18)%j+A(j,64)%j-A(j,65)%j-A(j,67)%j+A(j,68)%j &
         +A(j,97)%j-A(j,98)%j+A(j,100)%j-A(j,101)%j-A(j,103)%j+A(j,104)%j-A(j,110)%j+A(j,111)%j+A(j,112)%j-A(j,114)%j-A(j,133)%j &
         +A(j,134)%j+A(j,142)%j-A(j,143)%j+A(j,152)%j-A(j,153)%j-A(j,202)%j+A(j,203)%j-A(j,235)%j+A(j,237)%j+A(j,238)%j-A(j,239)%j &
         +A(j,253)%j-A(j,254)%j-A(j,271)%j+A(j,272)%j+A(j,280)%j-A(j,282)%j+A(j,286)%j-A(j,287)%j+A(j,322)%j-A(j,323)%j-A(j,376)%j &
         +A(j,377)%j+A(j,391)%j-A(j,393)%j+A(j,394)%j-A(j,396)%j+A(j,397)%j-A(j,398)%j+A(j,403)%j-A(j,404)%j-A(j,412)%j+A(j,413)%j &
         +A(j,415)%j+A(j,419)%j+A(j,420)%j-A(j,436)%j+A(j,437)%j+A(j,443)%j+A(j,444)%j+A(j,449)%j-A(j,459)%j+A(j,460)%j-A(j,461)%j &
         +A(j,510)%j)*f(1)
  M1(  7) = 2*(A(j,10)%j-A(j,11)%j-A(j,13)%j+A(j,14)%j+A(j,28)%j-A(j,30)%j-A(j,34)%j+A(j,36)%j+A(j,86)%j-A(j,87)%j-A(j,89)%j &
         +A(j,90)%j-A(j,92)%j+A(j,93)%j-A(j,94)%j+A(j,96)%j+A(j,109)%j-A(j,110)%j+A(j,113)%j-A(j,114)%j+A(j,118)%j-A(j,120)%j &
         -A(j,157)%j+A(j,158)%j+A(j,160)%j-A(j,161)%j-A(j,175)%j+A(j,177)%j+A(j,212)%j-A(j,213)%j+A(j,223)%j-A(j,225)%j-A(j,266)%j &
         +A(j,267)%j-A(j,269)%j+A(j,270)%j-A(j,289)%j+A(j,291)%j-A(j,296)%j+A(j,297)%j-A(j,305)%j+A(j,306)%j+A(j,326)%j-A(j,327)%j &
         -A(j,361)%j+A(j,362)%j+A(j,364)%j-A(j,366)%j-A(j,373)%j+A(j,375)%j+A(j,388)%j-A(j,389)%j+A(j,394)%j-A(j,395)%j-A(j,406)%j &
         +A(j,408)%j+A(j,410)%j-A(j,411)%j+A(j,420)%j+A(j,466)%j+A(j,467)%j+A(j,470)%j+A(j,473)%j+A(j,474)%j-A(j,481)%j+A(j,482)%j &
         +A(j,486)%j+A(j,499)%j)*f(1)
  M1(  8) = 2*(A(j,13)%j-A(j,14)%j-A(j,16)%j+A(j,17)%j+A(j,19)%j-A(j,21)%j-A(j,25)%j+A(j,27)%j+A(j,85)%j-A(j,86)%j-A(j,88)%j &
         +A(j,89)%j-A(j,91)%j+A(j,92)%j-A(j,97)%j+A(j,99)%j+A(j,110)%j-A(j,111)%j-A(j,113)%j+A(j,114)%j+A(j,115)%j-A(j,117)%j &
         -A(j,155)%j+A(j,156)%j+A(j,161)%j-A(j,162)%j-A(j,166)%j+A(j,168)%j+A(j,208)%j-A(j,209)%j+A(j,217)%j-A(j,219)%j+A(j,263)%j &
         -A(j,264)%j+A(j,269)%j-A(j,270)%j-A(j,292)%j+A(j,294)%j-A(j,295)%j+A(j,296)%j-A(j,304)%j+A(j,305)%j-A(j,326)%j+A(j,327)%j &
         +A(j,361)%j-A(j,362)%j+A(j,367)%j-A(j,369)%j-A(j,382)%j+A(j,384)%j-A(j,391)%j+A(j,392)%j-A(j,394)%j+A(j,395)%j+A(j,406)%j &
         +A(j,407)%j-A(j,415)%j-A(j,416)%j-A(j,420)%j+A(j,463)%j-A(j,465)%j-A(j,471)%j-A(j,473)%j-A(j,474)%j-A(j,476)%j-A(j,477)%j &
         -A(j,486)%j+A(j,489)%j)*f(1)
  M1(  9) = 2*(-A(j,10)%j+A(j,11)%j+A(j,16)%j-A(j,17)%j-A(j,31)%j+A(j,33)%j+A(j,34)%j-A(j,36)%j+A(j,56)%j-A(j,57)%j-A(j,59)%j &
         +A(j,60)%j+A(j,94)%j-A(j,96)%j+A(j,97)%j-A(j,98)%j+A(j,104)%j-A(j,105)%j-A(j,109)%j+A(j,111)%j-A(j,119)%j+A(j,120)%j &
         +A(j,157)%j-A(j,159)%j-A(j,160)%j+A(j,162)%j+A(j,176)%j-A(j,177)%j-A(j,223)%j+A(j,225)%j-A(j,229)%j+A(j,230)%j-A(j,232)%j &
         +A(j,233)%j-A(j,251)%j+A(j,252)%j+A(j,289)%j-A(j,291)%j+A(j,292)%j-A(j,293)%j+A(j,329)%j-A(j,330)%j+A(j,344)%j-A(j,345)%j &
         +A(j,373)%j-A(j,375)%j-A(j,376)%j+A(j,378)%j+A(j,379)%j-A(j,381)%j-A(j,388)%j+A(j,389)%j+A(j,391)%j-A(j,392)%j-A(j,410)%j &
         +A(j,411)%j-A(j,412)%j+A(j,414)%j+A(j,415)%j-A(j,466)%j-A(j,467)%j-A(j,469)%j+A(j,471)%j-A(j,472)%j-A(j,499)%j-A(j,505)%j &
         -A(j,506)%j-A(j,509)%j)*f(1)
  M1( 10) = 2*(-A(j,1)%j+A(j,3)%j+A(j,4)%j-A(j,6)%j+A(j,13)%j-A(j,14)%j-A(j,16)%j+A(j,17)%j+A(j,55)%j-A(j,56)%j-A(j,58)%j &
         +A(j,59)%j-A(j,97)%j+A(j,98)%j-A(j,100)%j+A(j,101)%j+A(j,103)%j-A(j,104)%j+A(j,110)%j-A(j,111)%j-A(j,112)%j+A(j,114)%j &
         -A(j,122)%j+A(j,123)%j+A(j,130)%j-A(j,131)%j+A(j,161)%j-A(j,162)%j-A(j,181)%j+A(j,182)%j+A(j,190)%j-A(j,192)%j+A(j,196)%j &
         -A(j,197)%j+A(j,232)%j-A(j,233)%j-A(j,292)%j+A(j,293)%j-A(j,325)%j+A(j,327)%j+A(j,328)%j-A(j,329)%j+A(j,343)%j-A(j,344)%j &
         +A(j,376)%j-A(j,378)%j-A(j,391)%j+A(j,392)%j-A(j,394)%j+A(j,395)%j-A(j,397)%j+A(j,399)%j-A(j,403)%j+A(j,405)%j+A(j,412)%j &
         -A(j,413)%j-A(j,415)%j-A(j,419)%j-A(j,420)%j-A(j,421)%j+A(j,423)%j+A(j,428)%j+A(j,429)%j+A(j,434)%j-A(j,471)%j+A(j,472)%j &
         -A(j,473)%j+A(j,509)%j)*f(1)
  M1( 11) = 2*(-A(j,10)%j+A(j,11)%j+A(j,16)%j-A(j,17)%j-A(j,22)%j+A(j,24)%j+A(j,25)%j-A(j,27)%j+A(j,47)%j-A(j,48)%j-A(j,50)%j &
         +A(j,51)%j+A(j,94)%j-A(j,95)%j+A(j,97)%j-A(j,99)%j+A(j,107)%j-A(j,108)%j-A(j,109)%j+A(j,111)%j-A(j,116)%j+A(j,117)%j &
         +A(j,154)%j-A(j,156)%j-A(j,160)%j+A(j,162)%j+A(j,167)%j-A(j,168)%j-A(j,217)%j+A(j,219)%j-A(j,220)%j+A(j,221)%j-A(j,226)%j &
         +A(j,227)%j-A(j,245)%j+A(j,246)%j+A(j,289)%j-A(j,290)%j+A(j,292)%j-A(j,294)%j+A(j,332)%j-A(j,333)%j+A(j,350)%j-A(j,351)%j &
         -A(j,370)%j+A(j,372)%j+A(j,382)%j-A(j,384)%j+A(j,385)%j-A(j,387)%j-A(j,388)%j+A(j,389)%j+A(j,391)%j-A(j,392)%j-A(j,409)%j &
         -A(j,410)%j+A(j,415)%j+A(j,416)%j+A(j,418)%j-A(j,463)%j-A(j,464)%j-A(j,467)%j-A(j,468)%j+A(j,471)%j-A(j,489)%j-A(j,491)%j &
         -A(j,492)%j-A(j,503)%j)*f(1)
  M1( 12) = 2*(-A(j,4)%j+A(j,6)%j+A(j,7)%j-A(j,9)%j+A(j,10)%j-A(j,11)%j-A(j,13)%j+A(j,14)%j+A(j,46)%j-A(j,47)%j-A(j,49)%j &
         +A(j,50)%j-A(j,94)%j+A(j,95)%j-A(j,101)%j+A(j,102)%j+A(j,106)%j-A(j,107)%j+A(j,109)%j-A(j,110)%j+A(j,112)%j-A(j,114)%j &
         -A(j,121)%j+A(j,122)%j+A(j,131)%j-A(j,132)%j+A(j,160)%j-A(j,161)%j-A(j,184)%j+A(j,185)%j-A(j,190)%j+A(j,192)%j+A(j,193)%j &
         -A(j,194)%j+A(j,226)%j-A(j,227)%j-A(j,289)%j+A(j,290)%j+A(j,325)%j-A(j,327)%j+A(j,331)%j-A(j,332)%j+A(j,349)%j-A(j,350)%j &
         +A(j,370)%j-A(j,372)%j+A(j,388)%j-A(j,389)%j+A(j,394)%j-A(j,395)%j+A(j,400)%j-A(j,402)%j+A(j,403)%j-A(j,405)%j+A(j,409)%j &
         +A(j,410)%j+A(j,417)%j+A(j,419)%j+A(j,420)%j+A(j,421)%j+A(j,422)%j-A(j,432)%j+A(j,433)%j-A(j,434)%j+A(j,467)%j+A(j,468)%j &
         +A(j,473)%j+A(j,503)%j)*f(1)
  M1( 13) = 2*(A(j,19)%j-A(j,20)%j-A(j,22)%j+A(j,23)%j+A(j,28)%j-A(j,30)%j-A(j,34)%j+A(j,36)%j+A(j,77)%j-A(j,78)%j-A(j,80)%j &
         +A(j,81)%j-A(j,91)%j+A(j,93)%j-A(j,95)%j+A(j,96)%j+A(j,107)%j-A(j,108)%j+A(j,115)%j-A(j,116)%j+A(j,118)%j-A(j,120)%j &
         -A(j,169)%j+A(j,170)%j+A(j,172)%j-A(j,173)%j-A(j,175)%j+A(j,177)%j+A(j,211)%j-A(j,213)%j+A(j,224)%j-A(j,225)%j-A(j,257)%j &
         +A(j,258)%j-A(j,260)%j+A(j,261)%j-A(j,298)%j+A(j,300)%j-A(j,311)%j+A(j,312)%j-A(j,317)%j+A(j,318)%j+A(j,335)%j-A(j,336)%j &
         +A(j,364)%j-A(j,366)%j+A(j,367)%j-A(j,368)%j-A(j,370)%j+A(j,371)%j-A(j,373)%j+A(j,375)%j+A(j,385)%j-A(j,386)%j+A(j,407)%j &
         +A(j,408)%j-A(j,409)%j-A(j,411)%j+A(j,418)%j+A(j,478)%j+A(j,479)%j-A(j,481)%j+A(j,494)%j+A(j,497)%j+A(j,498)%j+A(j,499)%j &
         +A(j,500)%j+A(j,504)%j)*f(1)
  M1( 14) = 2*(A(j,10)%j-A(j,12)%j-A(j,16)%j+A(j,18)%j+A(j,22)%j-A(j,23)%j-A(j,25)%j+A(j,26)%j+A(j,76)%j-A(j,77)%j-A(j,79)%j &
         +A(j,80)%j-A(j,94)%j+A(j,95)%j-A(j,97)%j+A(j,99)%j-A(j,107)%j+A(j,108)%j+A(j,109)%j-A(j,111)%j+A(j,116)%j-A(j,117)%j &
         -A(j,146)%j+A(j,147)%j-A(j,151)%j+A(j,153)%j+A(j,173)%j-A(j,174)%j+A(j,199)%j-A(j,200)%j+A(j,202)%j-A(j,204)%j+A(j,242)%j &
         -A(j,243)%j+A(j,260)%j-A(j,261)%j-A(j,307)%j+A(j,309)%j-A(j,310)%j+A(j,311)%j-A(j,316)%j+A(j,317)%j-A(j,335)%j+A(j,336)%j &
         +A(j,370)%j-A(j,371)%j-A(j,382)%j+A(j,383)%j-A(j,385)%j+A(j,386)%j+A(j,388)%j-A(j,390)%j-A(j,391)%j+A(j,393)%j+A(j,409)%j &
         +A(j,410)%j-A(j,415)%j-A(j,416)%j-A(j,418)%j-A(j,451)%j-A(j,453)%j-A(j,455)%j-A(j,456)%j+A(j,459)%j-A(j,495)%j-A(j,497)%j &
         -A(j,498)%j-A(j,504)%j)*f(1)
  M1( 15) = 2*(-A(j,19)%j+A(j,20)%j+A(j,25)%j-A(j,26)%j-A(j,28)%j+A(j,30)%j+A(j,31)%j-A(j,33)%j+A(j,59)%j-A(j,60)%j-A(j,62)%j &
         +A(j,63)%j+A(j,91)%j-A(j,93)%j+A(j,98)%j-A(j,99)%j-A(j,104)%j+A(j,105)%j-A(j,115)%j+A(j,117)%j-A(j,118)%j+A(j,119)%j &
         +A(j,169)%j-A(j,171)%j-A(j,172)%j+A(j,174)%j+A(j,175)%j-A(j,176)%j-A(j,211)%j+A(j,213)%j-A(j,230)%j+A(j,231)%j-A(j,233)%j &
         +A(j,234)%j+A(j,251)%j-A(j,252)%j+A(j,298)%j-A(j,300)%j+A(j,308)%j-A(j,309)%j-A(j,338)%j+A(j,339)%j-A(j,344)%j+A(j,345)%j &
         -A(j,364)%j+A(j,366)%j-A(j,367)%j+A(j,368)%j+A(j,376)%j-A(j,378)%j-A(j,379)%j+A(j,381)%j+A(j,382)%j-A(j,383)%j-A(j,407)%j &
         -A(j,408)%j+A(j,412)%j-A(j,414)%j+A(j,416)%j-A(j,478)%j-A(j,479)%j+A(j,481)%j+A(j,493)%j+A(j,495)%j+A(j,496)%j+A(j,505)%j &
         +A(j,506)%j+A(j,509)%j)*f(1)
  M1( 16) = 2*(A(j,1)%j-A(j,3)%j-A(j,7)%j+A(j,9)%j+A(j,22)%j-A(j,23)%j-A(j,25)%j+A(j,26)%j+A(j,58)%j-A(j,59)%j-A(j,61)%j+A(j,62)%j &
         -A(j,98)%j+A(j,99)%j+A(j,100)%j-A(j,102)%j-A(j,103)%j+A(j,104)%j-A(j,106)%j+A(j,108)%j+A(j,116)%j-A(j,117)%j-A(j,125)%j &
         +A(j,126)%j-A(j,130)%j+A(j,132)%j+A(j,173)%j-A(j,174)%j+A(j,181)%j-A(j,182)%j+A(j,184)%j-A(j,186)%j+A(j,197)%j-A(j,198)%j &
         +A(j,233)%j-A(j,234)%j-A(j,308)%j+A(j,309)%j-A(j,334)%j+A(j,336)%j-A(j,337)%j+A(j,338)%j-A(j,343)%j+A(j,344)%j-A(j,376)%j &
         +A(j,378)%j-A(j,382)%j+A(j,383)%j-A(j,385)%j+A(j,386)%j+A(j,397)%j-A(j,399)%j-A(j,400)%j+A(j,402)%j-A(j,412)%j+A(j,413)%j &
         -A(j,416)%j-A(j,417)%j-A(j,418)%j-A(j,424)%j-A(j,426)%j-A(j,428)%j-A(j,429)%j+A(j,432)%j-A(j,495)%j-A(j,496)%j-A(j,497)%j &
         -A(j,509)%j)*f(1)
  M1( 17) = 2*(-A(j,13)%j+A(j,15)%j+A(j,16)%j-A(j,18)%j-A(j,19)%j+A(j,20)%j+A(j,25)%j-A(j,26)%j+A(j,38)%j-A(j,39)%j-A(j,41)%j &
         +A(j,42)%j+A(j,91)%j-A(j,92)%j+A(j,97)%j-A(j,99)%j-A(j,110)%j+A(j,111)%j+A(j,113)%j-A(j,114)%j-A(j,115)%j+A(j,117)%j &
         +A(j,145)%j-A(j,147)%j+A(j,152)%j-A(j,153)%j-A(j,172)%j+A(j,174)%j-A(j,202)%j+A(j,204)%j-A(j,205)%j+A(j,206)%j-A(j,214)%j &
         +A(j,215)%j-A(j,236)%j+A(j,237)%j+A(j,298)%j-A(j,299)%j+A(j,307)%j-A(j,309)%j+A(j,353)%j-A(j,354)%j+A(j,359)%j-A(j,360)%j &
         -A(j,361)%j+A(j,363)%j-A(j,367)%j+A(j,368)%j+A(j,382)%j-A(j,383)%j+A(j,391)%j-A(j,393)%j+A(j,394)%j-A(j,396)%j-A(j,406)%j &
         -A(j,407)%j+A(j,415)%j+A(j,416)%j+A(j,420)%j+A(j,451)%j-A(j,452)%j-A(j,459)%j-A(j,461)%j-A(j,462)%j-A(j,479)%j-A(j,480)%j &
         -A(j,485)%j+A(j,495)%j)*f(1)
  M1( 18) = 2*(-A(j,4)%j+A(j,6)%j+A(j,7)%j-A(j,9)%j+A(j,19)%j-A(j,20)%j-A(j,22)%j+A(j,23)%j+A(j,37)%j-A(j,38)%j-A(j,40)%j &
         +A(j,41)%j-A(j,91)%j+A(j,92)%j-A(j,101)%j+A(j,102)%j+A(j,106)%j-A(j,108)%j+A(j,112)%j-A(j,113)%j+A(j,115)%j-A(j,116)%j &
         -A(j,124)%j+A(j,125)%j+A(j,131)%j-A(j,132)%j+A(j,172)%j-A(j,173)%j-A(j,184)%j+A(j,186)%j+A(j,187)%j-A(j,188)%j-A(j,190)%j &
         +A(j,191)%j+A(j,214)%j-A(j,215)%j-A(j,298)%j+A(j,299)%j+A(j,334)%j-A(j,336)%j+A(j,352)%j-A(j,353)%j+A(j,358)%j-A(j,359)%j &
         +A(j,361)%j-A(j,363)%j+A(j,367)%j-A(j,368)%j+A(j,385)%j-A(j,386)%j+A(j,400)%j-A(j,402)%j+A(j,403)%j-A(j,405)%j+A(j,406)%j &
         +A(j,407)%j+A(j,417)%j+A(j,418)%j+A(j,419)%j+A(j,424)%j+A(j,425)%j-A(j,432)%j-A(j,434)%j+A(j,435)%j+A(j,479)%j+A(j,480)%j &
         +A(j,485)%j+A(j,497)%j)*f(1)
  M1( 19) = 2*(A(j,19)%j-A(j,21)%j-A(j,25)%j+A(j,27)%j+A(j,28)%j-A(j,29)%j-A(j,31)%j+A(j,32)%j+A(j,68)%j-A(j,69)%j-A(j,71)%j &
         +A(j,72)%j-A(j,91)%j+A(j,93)%j-A(j,98)%j+A(j,99)%j+A(j,104)%j-A(j,105)%j+A(j,115)%j-A(j,117)%j+A(j,118)%j-A(j,119)%j &
         -A(j,163)%j+A(j,164)%j-A(j,166)%j+A(j,168)%j+A(j,178)%j-A(j,179)%j+A(j,208)%j-A(j,210)%j+A(j,218)%j-A(j,219)%j-A(j,248)%j &
         +A(j,249)%j-A(j,254)%j+A(j,255)%j-A(j,301)%j+A(j,303)%j-A(j,320)%j+A(j,321)%j-A(j,323)%j+A(j,324)%j+A(j,341)%j-A(j,342)%j &
         +A(j,364)%j-A(j,365)%j+A(j,367)%j-A(j,369)%j-A(j,376)%j+A(j,377)%j+A(j,379)%j-A(j,380)%j-A(j,382)%j+A(j,384)%j+A(j,407)%j &
         +A(j,408)%j-A(j,412)%j+A(j,414)%j-A(j,416)%j-A(j,475)%j-A(j,476)%j+A(j,483)%j+A(j,488)%j+A(j,489)%j+A(j,490)%j+A(j,507)%j &
         +A(j,508)%j+A(j,510)%j)*f(1)
  M1( 20) = 2*(A(j,10)%j-A(j,12)%j-A(j,16)%j+A(j,18)%j+A(j,31)%j-A(j,32)%j-A(j,34)%j+A(j,35)%j+A(j,67)%j-A(j,68)%j-A(j,70)%j &
         +A(j,71)%j-A(j,94)%j+A(j,96)%j-A(j,97)%j+A(j,98)%j-A(j,104)%j+A(j,105)%j+A(j,109)%j-A(j,111)%j+A(j,119)%j-A(j,120)%j &
         -A(j,149)%j+A(j,150)%j-A(j,151)%j+A(j,153)%j+A(j,179)%j-A(j,180)%j+A(j,199)%j-A(j,201)%j+A(j,202)%j-A(j,203)%j+A(j,239)%j &
         -A(j,240)%j+A(j,254)%j-A(j,255)%j-A(j,313)%j+A(j,315)%j-A(j,319)%j+A(j,320)%j-A(j,322)%j+A(j,323)%j-A(j,341)%j+A(j,342)%j &
         -A(j,373)%j+A(j,374)%j+A(j,376)%j-A(j,377)%j-A(j,379)%j+A(j,380)%j+A(j,388)%j-A(j,390)%j-A(j,391)%j+A(j,393)%j+A(j,410)%j &
         -A(j,411)%j+A(j,412)%j-A(j,414)%j-A(j,415)%j-A(j,454)%j-A(j,455)%j-A(j,458)%j+A(j,459)%j-A(j,460)%j-A(j,501)%j-A(j,507)%j &
         -A(j,508)%j-A(j,510)%j)*f(1)
  M1( 21) = 2*(-A(j,19)%j+A(j,21)%j+A(j,22)%j-A(j,24)%j-A(j,28)%j+A(j,29)%j+A(j,34)%j-A(j,35)%j+A(j,50)%j-A(j,51)%j-A(j,53)%j &
         +A(j,54)%j+A(j,91)%j-A(j,93)%j+A(j,95)%j-A(j,96)%j-A(j,107)%j+A(j,108)%j-A(j,115)%j+A(j,116)%j-A(j,118)%j+A(j,120)%j &
         +A(j,163)%j-A(j,165)%j+A(j,166)%j-A(j,167)%j-A(j,178)%j+A(j,180)%j-A(j,208)%j+A(j,210)%j-A(j,221)%j+A(j,222)%j-A(j,227)%j &
         +A(j,228)%j+A(j,245)%j-A(j,246)%j+A(j,301)%j-A(j,303)%j+A(j,314)%j-A(j,315)%j-A(j,347)%j+A(j,348)%j-A(j,350)%j+A(j,351)%j &
         -A(j,364)%j+A(j,365)%j-A(j,367)%j+A(j,369)%j+A(j,370)%j-A(j,372)%j+A(j,373)%j-A(j,374)%j-A(j,385)%j+A(j,387)%j-A(j,407)%j &
         -A(j,408)%j+A(j,409)%j+A(j,411)%j-A(j,418)%j+A(j,475)%j+A(j,476)%j-A(j,483)%j+A(j,487)%j+A(j,491)%j+A(j,492)%j+A(j,501)%j &
         +A(j,502)%j+A(j,503)%j)*f(1)
  M1( 22) = 2*(A(j,1)%j-A(j,3)%j-A(j,7)%j+A(j,9)%j+A(j,31)%j-A(j,32)%j-A(j,34)%j+A(j,35)%j+A(j,49)%j-A(j,50)%j-A(j,52)%j+A(j,53)%j &
         -A(j,95)%j+A(j,96)%j+A(j,100)%j-A(j,102)%j-A(j,103)%j+A(j,105)%j-A(j,106)%j+A(j,107)%j+A(j,119)%j-A(j,120)%j-A(j,128)%j &
         +A(j,129)%j-A(j,130)%j+A(j,132)%j+A(j,179)%j-A(j,180)%j+A(j,181)%j-A(j,183)%j+A(j,184)%j-A(j,185)%j+A(j,194)%j-A(j,195)%j &
         +A(j,227)%j-A(j,228)%j-A(j,314)%j+A(j,315)%j-A(j,340)%j+A(j,342)%j-A(j,346)%j+A(j,347)%j-A(j,349)%j+A(j,350)%j-A(j,370)%j &
         +A(j,372)%j-A(j,373)%j+A(j,374)%j-A(j,379)%j+A(j,380)%j+A(j,397)%j-A(j,399)%j-A(j,400)%j+A(j,402)%j-A(j,409)%j-A(j,411)%j &
         +A(j,413)%j-A(j,414)%j-A(j,417)%j-A(j,427)%j-A(j,428)%j-A(j,431)%j+A(j,432)%j-A(j,433)%j-A(j,501)%j-A(j,502)%j-A(j,503)%j &
         -A(j,507)%j)*f(1)
  M1( 23) = 2*(-A(j,10)%j+A(j,12)%j+A(j,13)%j-A(j,15)%j-A(j,28)%j+A(j,29)%j+A(j,34)%j-A(j,35)%j+A(j,41)%j-A(j,42)%j-A(j,44)%j &
         +A(j,45)%j+A(j,92)%j-A(j,93)%j+A(j,94)%j-A(j,96)%j-A(j,109)%j+A(j,110)%j-A(j,113)%j+A(j,114)%j-A(j,118)%j+A(j,120)%j &
         +A(j,148)%j-A(j,150)%j+A(j,151)%j-A(j,152)%j-A(j,178)%j+A(j,180)%j-A(j,199)%j+A(j,201)%j-A(j,206)%j+A(j,207)%j-A(j,215)%j &
         +A(j,216)%j+A(j,236)%j-A(j,237)%j+A(j,302)%j-A(j,303)%j+A(j,313)%j-A(j,315)%j-A(j,356)%j+A(j,357)%j-A(j,359)%j+A(j,360)%j &
         +A(j,361)%j-A(j,363)%j-A(j,364)%j+A(j,365)%j+A(j,373)%j-A(j,374)%j-A(j,388)%j+A(j,390)%j-A(j,394)%j+A(j,396)%j+A(j,406)%j &
         -A(j,408)%j-A(j,410)%j+A(j,411)%j-A(j,420)%j+A(j,454)%j+A(j,455)%j+A(j,457)%j+A(j,461)%j+A(j,462)%j-A(j,483)%j+A(j,484)%j &
         +A(j,485)%j+A(j,501)%j)*f(1)
  M1( 24) = 2*(-A(j,1)%j+A(j,3)%j+A(j,4)%j-A(j,6)%j+A(j,28)%j-A(j,29)%j-A(j,31)%j+A(j,32)%j+A(j,40)%j-A(j,41)%j-A(j,43)%j &
         +A(j,44)%j-A(j,92)%j+A(j,93)%j-A(j,100)%j+A(j,101)%j+A(j,103)%j-A(j,105)%j-A(j,112)%j+A(j,113)%j+A(j,118)%j-A(j,119)%j &
         -A(j,127)%j+A(j,128)%j+A(j,130)%j-A(j,131)%j+A(j,178)%j-A(j,179)%j-A(j,181)%j+A(j,183)%j+A(j,188)%j-A(j,189)%j+A(j,190)%j &
         -A(j,191)%j+A(j,215)%j-A(j,216)%j-A(j,302)%j+A(j,303)%j+A(j,340)%j-A(j,342)%j-A(j,355)%j+A(j,356)%j-A(j,358)%j+A(j,359)%j &
         -A(j,361)%j+A(j,363)%j+A(j,364)%j-A(j,365)%j+A(j,379)%j-A(j,380)%j-A(j,397)%j+A(j,399)%j-A(j,403)%j+A(j,405)%j-A(j,406)%j &
         +A(j,408)%j-A(j,413)%j+A(j,414)%j-A(j,419)%j+A(j,427)%j+A(j,428)%j-A(j,430)%j+A(j,434)%j-A(j,435)%j+A(j,483)%j-A(j,484)%j &
         -A(j,485)%j+A(j,507)%j)*f(1)
  M1( 25) = 2*(A(j,2)%j-A(j,3)%j-A(j,5)%j+A(j,6)%j-A(j,55)%j+A(j,57)%j+A(j,58)%j-A(j,60)%j-A(j,83)%j+A(j,84)%j+A(j,89)%j-A(j,90)%j &
         +A(j,122)%j-A(j,123)%j-A(j,130)%j+A(j,131)%j+A(j,139)%j-A(j,140)%j-A(j,142)%j+A(j,143)%j-A(j,158)%j+A(j,159)%j+A(j,181)%j &
         -A(j,182)%j-A(j,190)%j+A(j,192)%j-A(j,196)%j+A(j,197)%j+A(j,229)%j-A(j,230)%j-A(j,250)%j+A(j,251)%j+A(j,265)%j-A(j,267)%j &
         +A(j,268)%j-A(j,270)%j+A(j,271)%j-A(j,273)%j-A(j,278)%j+A(j,279)%j-A(j,280)%j+A(j,281)%j+A(j,296)%j-A(j,297)%j+A(j,325)%j &
         -A(j,326)%j-A(j,328)%j+A(j,330)%j-A(j,343)%j+A(j,345)%j+A(j,398)%j-A(j,399)%j+A(j,404)%j-A(j,405)%j+A(j,421)%j-A(j,423)%j &
         -A(j,428)%j-A(j,429)%j-A(j,434)%j-A(j,442)%j-A(j,443)%j+A(j,446)%j-A(j,449)%j+A(j,450)%j+A(j,469)%j-A(j,470)%j-A(j,474)%j &
         +A(j,506)%j)*f(1)
  M1( 26) = 2*(A(j,5)%j-A(j,6)%j-A(j,8)%j+A(j,9)%j-A(j,46)%j+A(j,48)%j+A(j,49)%j-A(j,51)%j-A(j,82)%j+A(j,83)%j+A(j,88)%j-A(j,89)%j &
         +A(j,121)%j-A(j,122)%j-A(j,131)%j+A(j,132)%j+A(j,137)%j-A(j,138)%j-A(j,143)%j+A(j,144)%j-A(j,154)%j+A(j,155)%j+A(j,184)%j &
         -A(j,185)%j+A(j,190)%j-A(j,192)%j-A(j,193)%j+A(j,194)%j+A(j,220)%j-A(j,221)%j-A(j,244)%j+A(j,245)%j-A(j,262)%j+A(j,264)%j &
         -A(j,268)%j+A(j,270)%j+A(j,274)%j-A(j,276)%j-A(j,277)%j+A(j,278)%j+A(j,280)%j-A(j,281)%j+A(j,295)%j-A(j,296)%j-A(j,325)%j &
         +A(j,326)%j-A(j,331)%j+A(j,333)%j-A(j,349)%j+A(j,351)%j-A(j,401)%j+A(j,402)%j-A(j,404)%j+A(j,405)%j-A(j,421)%j-A(j,422)%j &
         +A(j,432)%j-A(j,433)%j+A(j,434)%j-A(j,439)%j-A(j,441)%j+A(j,447)%j+A(j,449)%j-A(j,450)%j+A(j,464)%j+A(j,465)%j+A(j,474)%j &
         +A(j,492)%j)*f(1)
  M1( 27) = 2*(-A(j,2)%j+A(j,3)%j+A(j,8)%j-A(j,9)%j-A(j,58)%j+A(j,60)%j+A(j,61)%j-A(j,63)%j-A(j,74)%j+A(j,75)%j+A(j,80)%j &
         -A(j,81)%j+A(j,125)%j-A(j,126)%j+A(j,130)%j-A(j,132)%j-A(j,139)%j+A(j,141)%j+A(j,142)%j-A(j,144)%j-A(j,170)%j+A(j,171)%j &
         -A(j,181)%j+A(j,182)%j-A(j,184)%j+A(j,186)%j-A(j,197)%j+A(j,198)%j+A(j,230)%j-A(j,231)%j+A(j,250)%j-A(j,251)%j+A(j,256)%j &
         -A(j,258)%j+A(j,259)%j-A(j,261)%j-A(j,271)%j+A(j,273)%j-A(j,274)%j+A(j,275)%j-A(j,284)%j+A(j,285)%j+A(j,311)%j-A(j,312)%j &
         +A(j,334)%j-A(j,335)%j+A(j,337)%j-A(j,339)%j+A(j,343)%j-A(j,345)%j-A(j,398)%j+A(j,399)%j+A(j,401)%j-A(j,402)%j+A(j,424)%j &
         +A(j,426)%j+A(j,428)%j+A(j,429)%j-A(j,432)%j+A(j,442)%j+A(j,443)%j+A(j,445)%j-A(j,447)%j+A(j,448)%j-A(j,493)%j-A(j,494)%j &
         -A(j,498)%j-A(j,506)%j)*f(1)
  M1( 28) = 2*(A(j,5)%j-A(j,6)%j-A(j,8)%j+A(j,9)%j-A(j,37)%j+A(j,39)%j+A(j,40)%j-A(j,42)%j-A(j,73)%j+A(j,74)%j+A(j,79)%j-A(j,80)%j &
         +A(j,124)%j-A(j,125)%j-A(j,131)%j+A(j,132)%j+A(j,134)%j-A(j,135)%j-A(j,143)%j+A(j,144)%j-A(j,145)%j+A(j,146)%j+A(j,184)%j &
         -A(j,186)%j-A(j,187)%j+A(j,188)%j+A(j,190)%j-A(j,191)%j+A(j,205)%j-A(j,206)%j-A(j,235)%j+A(j,236)%j-A(j,241)%j+A(j,243)%j &
         -A(j,259)%j+A(j,261)%j+A(j,274)%j-A(j,275)%j+A(j,280)%j-A(j,282)%j-A(j,283)%j+A(j,284)%j+A(j,310)%j-A(j,311)%j-A(j,334)%j &
         +A(j,335)%j-A(j,352)%j+A(j,354)%j-A(j,358)%j+A(j,360)%j-A(j,401)%j+A(j,402)%j-A(j,404)%j+A(j,405)%j-A(j,424)%j-A(j,425)%j &
         +A(j,432)%j+A(j,434)%j-A(j,435)%j-A(j,436)%j-A(j,438)%j+A(j,447)%j-A(j,448)%j+A(j,449)%j+A(j,452)%j+A(j,453)%j+A(j,462)%j &
         +A(j,498)%j)*f(1)
  M1( 29) = 2*(-A(j,2)%j+A(j,3)%j+A(j,8)%j-A(j,9)%j-A(j,49)%j+A(j,51)%j+A(j,52)%j-A(j,54)%j-A(j,65)%j+A(j,66)%j+A(j,71)%j &
         -A(j,72)%j+A(j,128)%j-A(j,129)%j+A(j,130)%j-A(j,132)%j-A(j,136)%j+A(j,138)%j+A(j,142)%j-A(j,144)%j-A(j,164)%j+A(j,165)%j &
         -A(j,181)%j+A(j,183)%j-A(j,184)%j+A(j,185)%j-A(j,194)%j+A(j,195)%j+A(j,221)%j-A(j,222)%j+A(j,244)%j-A(j,245)%j+A(j,247)%j &
         -A(j,249)%j+A(j,253)%j-A(j,255)%j-A(j,271)%j+A(j,272)%j-A(j,274)%j+A(j,276)%j-A(j,287)%j+A(j,288)%j+A(j,320)%j-A(j,321)%j &
         +A(j,340)%j-A(j,341)%j+A(j,346)%j-A(j,348)%j+A(j,349)%j-A(j,351)%j-A(j,398)%j+A(j,399)%j+A(j,401)%j-A(j,402)%j+A(j,427)%j &
         +A(j,428)%j+A(j,431)%j-A(j,432)%j+A(j,433)%j+A(j,439)%j+A(j,440)%j+A(j,443)%j+A(j,444)%j-A(j,447)%j-A(j,487)%j-A(j,488)%j &
         -A(j,492)%j-A(j,508)%j)*f(1)
  M1( 30) = 2*(A(j,2)%j-A(j,3)%j-A(j,5)%j+A(j,6)%j-A(j,40)%j+A(j,42)%j+A(j,43)%j-A(j,45)%j-A(j,64)%j+A(j,65)%j+A(j,70)%j-A(j,71)%j &
         +A(j,127)%j-A(j,128)%j-A(j,130)%j+A(j,131)%j+A(j,133)%j-A(j,134)%j-A(j,142)%j+A(j,143)%j-A(j,148)%j+A(j,149)%j+A(j,181)%j &
         -A(j,183)%j-A(j,188)%j+A(j,189)%j-A(j,190)%j+A(j,191)%j+A(j,206)%j-A(j,207)%j+A(j,235)%j-A(j,236)%j-A(j,238)%j+A(j,240)%j &
         -A(j,253)%j+A(j,255)%j+A(j,271)%j-A(j,272)%j-A(j,280)%j+A(j,282)%j-A(j,286)%j+A(j,287)%j+A(j,319)%j-A(j,320)%j-A(j,340)%j &
         +A(j,341)%j+A(j,355)%j-A(j,357)%j+A(j,358)%j-A(j,360)%j+A(j,398)%j-A(j,399)%j+A(j,404)%j-A(j,405)%j-A(j,427)%j-A(j,428)%j &
         +A(j,430)%j-A(j,434)%j+A(j,435)%j+A(j,436)%j-A(j,437)%j-A(j,443)%j-A(j,444)%j-A(j,449)%j-A(j,457)%j+A(j,458)%j-A(j,462)%j &
         +A(j,508)%j)*f(1)
  M1( 31) = 2*(A(j,37)%j-A(j,38)%j-A(j,40)%j+A(j,41)%j+A(j,55)%j-A(j,57)%j-A(j,61)%j+A(j,63)%j+A(j,86)%j-A(j,87)%j-A(j,89)%j &
         +A(j,90)%j-A(j,122)%j+A(j,123)%j-A(j,124)%j+A(j,126)%j+A(j,158)%j-A(j,159)%j+A(j,169)%j-A(j,171)%j+A(j,187)%j-A(j,188)%j &
         +A(j,191)%j-A(j,192)%j+A(j,196)%j-A(j,198)%j-A(j,211)%j+A(j,212)%j+A(j,214)%j-A(j,215)%j-A(j,229)%j+A(j,231)%j-A(j,266)%j &
         +A(j,267)%j-A(j,269)%j+A(j,270)%j-A(j,296)%j+A(j,297)%j+A(j,299)%j-A(j,300)%j-A(j,305)%j+A(j,306)%j-A(j,325)%j+A(j,326)%j &
         +A(j,328)%j-A(j,330)%j-A(j,337)%j+A(j,339)%j+A(j,352)%j-A(j,353)%j+A(j,358)%j-A(j,359)%j+A(j,362)%j-A(j,363)%j-A(j,421)%j &
         +A(j,423)%j+A(j,425)%j-A(j,426)%j+A(j,435)%j-A(j,469)%j+A(j,470)%j+A(j,474)%j-A(j,478)%j+A(j,480)%j+A(j,482)%j+A(j,485)%j &
         +A(j,486)%j+A(j,493)%j)*f(1)
  M1( 32) = 2*(A(j,40)%j-A(j,41)%j-A(j,43)%j+A(j,44)%j+A(j,46)%j-A(j,48)%j-A(j,52)%j+A(j,54)%j+A(j,85)%j-A(j,86)%j-A(j,88)%j &
         +A(j,89)%j-A(j,121)%j+A(j,122)%j-A(j,127)%j+A(j,129)%j+A(j,154)%j-A(j,155)%j+A(j,163)%j-A(j,165)%j+A(j,188)%j-A(j,189)%j &
         -A(j,191)%j+A(j,192)%j+A(j,193)%j-A(j,195)%j-A(j,209)%j+A(j,210)%j+A(j,215)%j-A(j,216)%j-A(j,220)%j+A(j,222)%j+A(j,263)%j &
         -A(j,264)%j+A(j,269)%j-A(j,270)%j-A(j,295)%j+A(j,296)%j+A(j,301)%j-A(j,302)%j-A(j,304)%j+A(j,305)%j+A(j,325)%j-A(j,326)%j &
         +A(j,331)%j-A(j,333)%j-A(j,346)%j+A(j,348)%j-A(j,355)%j+A(j,356)%j-A(j,358)%j+A(j,359)%j-A(j,362)%j+A(j,363)%j+A(j,421)%j &
         +A(j,422)%j-A(j,430)%j-A(j,431)%j-A(j,435)%j-A(j,464)%j-A(j,465)%j-A(j,474)%j+A(j,475)%j-A(j,477)%j-A(j,484)%j-A(j,485)%j &
         -A(j,486)%j+A(j,487)%j)*f(1)
  M1( 33) = 2*(A(j,29)%j-A(j,30)%j-A(j,32)%j+A(j,33)%j-A(j,37)%j+A(j,38)%j+A(j,43)%j-A(j,44)%j-A(j,58)%j+A(j,60)%j+A(j,61)%j &
         -A(j,63)%j+A(j,124)%j-A(j,126)%j+A(j,127)%j-A(j,128)%j-A(j,169)%j+A(j,171)%j-A(j,175)%j+A(j,176)%j-A(j,178)%j+A(j,179)%j &
         +A(j,182)%j-A(j,183)%j-A(j,187)%j+A(j,189)%j-A(j,197)%j+A(j,198)%j+A(j,211)%j-A(j,213)%j-A(j,214)%j+A(j,216)%j+A(j,230)%j &
         -A(j,231)%j-A(j,251)%j+A(j,252)%j-A(j,299)%j+A(j,300)%j+A(j,302)%j-A(j,303)%j+A(j,337)%j-A(j,339)%j-A(j,340)%j+A(j,342)%j &
         +A(j,343)%j-A(j,345)%j-A(j,352)%j+A(j,353)%j+A(j,355)%j-A(j,356)%j+A(j,365)%j-A(j,366)%j+A(j,380)%j-A(j,381)%j-A(j,425)%j &
         +A(j,426)%j-A(j,427)%j+A(j,429)%j+A(j,430)%j+A(j,478)%j-A(j,480)%j-A(j,481)%j-A(j,483)%j+A(j,484)%j-A(j,493)%j-A(j,505)%j &
         -A(j,506)%j-A(j,507)%j)*f(1)
  M1( 34) = 2*(-A(j,1)%j+A(j,3)%j+A(j,4)%j-A(j,6)%j+A(j,28)%j-A(j,29)%j-A(j,31)%j+A(j,32)%j+A(j,40)%j-A(j,41)%j-A(j,43)%j &
         +A(j,44)%j-A(j,92)%j+A(j,93)%j-A(j,100)%j+A(j,101)%j+A(j,103)%j-A(j,105)%j-A(j,112)%j+A(j,113)%j+A(j,118)%j-A(j,119)%j &
         -A(j,127)%j+A(j,128)%j+A(j,130)%j-A(j,131)%j+A(j,178)%j-A(j,179)%j-A(j,181)%j+A(j,183)%j+A(j,188)%j-A(j,189)%j+A(j,190)%j &
         -A(j,191)%j+A(j,215)%j-A(j,216)%j-A(j,302)%j+A(j,303)%j+A(j,340)%j-A(j,342)%j-A(j,355)%j+A(j,356)%j-A(j,358)%j+A(j,359)%j &
         -A(j,361)%j+A(j,363)%j+A(j,364)%j-A(j,365)%j+A(j,379)%j-A(j,380)%j-A(j,397)%j+A(j,399)%j-A(j,403)%j+A(j,405)%j-A(j,406)%j &
         +A(j,408)%j-A(j,413)%j+A(j,414)%j-A(j,419)%j+A(j,427)%j+A(j,428)%j-A(j,430)%j+A(j,434)%j-A(j,435)%j+A(j,483)%j-A(j,484)%j &
         -A(j,485)%j+A(j,507)%j)*f(1)
  M1( 35) = 2*(A(j,20)%j-A(j,21)%j-A(j,23)%j+A(j,24)%j-A(j,37)%j+A(j,38)%j+A(j,43)%j-A(j,44)%j-A(j,49)%j+A(j,51)%j+A(j,52)%j &
         -A(j,54)%j+A(j,124)%j-A(j,125)%j+A(j,127)%j-A(j,129)%j-A(j,163)%j+A(j,165)%j-A(j,166)%j+A(j,167)%j-A(j,172)%j+A(j,173)%j &
         +A(j,185)%j-A(j,186)%j-A(j,187)%j+A(j,189)%j-A(j,194)%j+A(j,195)%j+A(j,208)%j-A(j,210)%j-A(j,214)%j+A(j,216)%j+A(j,221)%j &
         -A(j,222)%j-A(j,245)%j+A(j,246)%j+A(j,298)%j-A(j,299)%j-A(j,301)%j+A(j,302)%j-A(j,334)%j+A(j,336)%j+A(j,346)%j-A(j,348)%j &
         +A(j,349)%j-A(j,351)%j-A(j,352)%j+A(j,353)%j+A(j,355)%j-A(j,356)%j+A(j,368)%j-A(j,369)%j+A(j,386)%j-A(j,387)%j-A(j,424)%j &
         -A(j,425)%j+A(j,430)%j+A(j,431)%j+A(j,433)%j-A(j,475)%j-A(j,476)%j-A(j,479)%j-A(j,480)%j+A(j,484)%j-A(j,487)%j-A(j,491)%j &
         -A(j,492)%j-A(j,497)%j)*f(1)
  M1( 36) = 2*(-A(j,4)%j+A(j,6)%j+A(j,7)%j-A(j,9)%j+A(j,19)%j-A(j,20)%j-A(j,22)%j+A(j,23)%j+A(j,37)%j-A(j,38)%j-A(j,40)%j &
         +A(j,41)%j-A(j,91)%j+A(j,92)%j-A(j,101)%j+A(j,102)%j+A(j,106)%j-A(j,108)%j+A(j,112)%j-A(j,113)%j+A(j,115)%j-A(j,116)%j &
         -A(j,124)%j+A(j,125)%j+A(j,131)%j-A(j,132)%j+A(j,172)%j-A(j,173)%j-A(j,184)%j+A(j,186)%j+A(j,187)%j-A(j,188)%j-A(j,190)%j &
         +A(j,191)%j+A(j,214)%j-A(j,215)%j-A(j,298)%j+A(j,299)%j+A(j,334)%j-A(j,336)%j+A(j,352)%j-A(j,353)%j+A(j,358)%j-A(j,359)%j &
         +A(j,361)%j-A(j,363)%j+A(j,367)%j-A(j,368)%j+A(j,385)%j-A(j,386)%j+A(j,400)%j-A(j,402)%j+A(j,403)%j-A(j,405)%j+A(j,406)%j &
         +A(j,407)%j+A(j,417)%j+A(j,418)%j+A(j,419)%j+A(j,424)%j+A(j,425)%j-A(j,432)%j-A(j,434)%j+A(j,435)%j+A(j,479)%j+A(j,480)%j &
         +A(j,485)%j+A(j,497)%j)*f(1)
  M1( 37) = 2*(A(j,46)%j-A(j,47)%j-A(j,49)%j+A(j,50)%j+A(j,55)%j-A(j,57)%j-A(j,61)%j+A(j,63)%j+A(j,77)%j-A(j,78)%j-A(j,80)%j &
         +A(j,81)%j-A(j,121)%j+A(j,123)%j-A(j,125)%j+A(j,126)%j+A(j,157)%j-A(j,159)%j+A(j,170)%j-A(j,171)%j+A(j,185)%j-A(j,186)%j &
         +A(j,193)%j-A(j,194)%j+A(j,196)%j-A(j,198)%j-A(j,223)%j+A(j,224)%j+A(j,226)%j-A(j,227)%j-A(j,229)%j+A(j,231)%j-A(j,257)%j &
         +A(j,258)%j-A(j,260)%j+A(j,261)%j+A(j,290)%j-A(j,291)%j-A(j,311)%j+A(j,312)%j-A(j,317)%j+A(j,318)%j+A(j,328)%j-A(j,330)%j &
         +A(j,331)%j-A(j,332)%j-A(j,334)%j+A(j,335)%j-A(j,337)%j+A(j,339)%j+A(j,349)%j-A(j,350)%j+A(j,371)%j-A(j,372)%j+A(j,422)%j &
         +A(j,423)%j-A(j,424)%j-A(j,426)%j+A(j,433)%j-A(j,466)%j+A(j,468)%j-A(j,469)%j+A(j,493)%j+A(j,494)%j+A(j,498)%j+A(j,500)%j &
         +A(j,503)%j+A(j,504)%j)*f(1)
  M1( 38) = 2*(A(j,37)%j-A(j,39)%j-A(j,43)%j+A(j,45)%j+A(j,49)%j-A(j,50)%j-A(j,52)%j+A(j,53)%j+A(j,76)%j-A(j,77)%j-A(j,79)%j &
         +A(j,80)%j-A(j,124)%j+A(j,125)%j-A(j,127)%j+A(j,129)%j+A(j,145)%j-A(j,146)%j+A(j,148)%j-A(j,150)%j-A(j,185)%j+A(j,186)%j &
         +A(j,187)%j-A(j,189)%j+A(j,194)%j-A(j,195)%j-A(j,200)%j+A(j,201)%j-A(j,205)%j+A(j,207)%j+A(j,227)%j-A(j,228)%j+A(j,242)%j &
         -A(j,243)%j+A(j,260)%j-A(j,261)%j-A(j,310)%j+A(j,311)%j+A(j,313)%j-A(j,314)%j-A(j,316)%j+A(j,317)%j+A(j,334)%j-A(j,335)%j &
         -A(j,346)%j+A(j,347)%j-A(j,349)%j+A(j,350)%j+A(j,352)%j-A(j,354)%j-A(j,355)%j+A(j,357)%j-A(j,371)%j+A(j,372)%j+A(j,424)%j &
         +A(j,425)%j-A(j,430)%j-A(j,431)%j-A(j,433)%j-A(j,452)%j-A(j,453)%j+A(j,454)%j-A(j,456)%j+A(j,457)%j-A(j,498)%j-A(j,502)%j &
         -A(j,503)%j-A(j,504)%j)*f(1)
  M1( 39) = 2*(A(j,32)%j-A(j,33)%j-A(j,35)%j+A(j,36)%j-A(j,46)%j+A(j,47)%j+A(j,52)%j-A(j,53)%j-A(j,55)%j+A(j,57)%j+A(j,58)%j &
         -A(j,60)%j+A(j,121)%j-A(j,123)%j+A(j,128)%j-A(j,129)%j-A(j,157)%j+A(j,159)%j-A(j,176)%j+A(j,177)%j-A(j,179)%j+A(j,180)%j &
         -A(j,182)%j+A(j,183)%j-A(j,193)%j+A(j,195)%j-A(j,196)%j+A(j,197)%j+A(j,223)%j-A(j,225)%j-A(j,226)%j+A(j,228)%j+A(j,229)%j &
         -A(j,230)%j+A(j,251)%j-A(j,252)%j-A(j,290)%j+A(j,291)%j+A(j,314)%j-A(j,315)%j-A(j,328)%j+A(j,330)%j-A(j,331)%j+A(j,332)%j &
         +A(j,340)%j-A(j,342)%j-A(j,343)%j+A(j,345)%j+A(j,346)%j-A(j,347)%j-A(j,374)%j+A(j,375)%j-A(j,380)%j+A(j,381)%j-A(j,422)%j &
         -A(j,423)%j+A(j,427)%j-A(j,429)%j+A(j,431)%j+A(j,466)%j-A(j,468)%j+A(j,469)%j+A(j,499)%j+A(j,501)%j+A(j,502)%j+A(j,505)%j &
         +A(j,506)%j+A(j,507)%j)*f(1)
  M1( 40) = 2*(A(j,1)%j-A(j,3)%j-A(j,7)%j+A(j,9)%j+A(j,31)%j-A(j,32)%j-A(j,34)%j+A(j,35)%j+A(j,49)%j-A(j,50)%j-A(j,52)%j+A(j,53)%j &
         -A(j,95)%j+A(j,96)%j+A(j,100)%j-A(j,102)%j-A(j,103)%j+A(j,105)%j-A(j,106)%j+A(j,107)%j+A(j,119)%j-A(j,120)%j-A(j,128)%j &
         +A(j,129)%j-A(j,130)%j+A(j,132)%j+A(j,179)%j-A(j,180)%j+A(j,181)%j-A(j,183)%j+A(j,184)%j-A(j,185)%j+A(j,194)%j-A(j,195)%j &
         +A(j,227)%j-A(j,228)%j-A(j,314)%j+A(j,315)%j-A(j,340)%j+A(j,342)%j-A(j,346)%j+A(j,347)%j-A(j,349)%j+A(j,350)%j-A(j,370)%j &
         +A(j,372)%j-A(j,373)%j+A(j,374)%j-A(j,379)%j+A(j,380)%j+A(j,397)%j-A(j,399)%j-A(j,400)%j+A(j,402)%j-A(j,409)%j-A(j,411)%j &
         +A(j,413)%j-A(j,414)%j-A(j,417)%j-A(j,427)%j-A(j,428)%j-A(j,431)%j+A(j,432)%j-A(j,433)%j-A(j,501)%j-A(j,502)%j-A(j,503)%j &
         -A(j,507)%j)*f(1)
  M1( 41) = 2*(A(j,11)%j-A(j,12)%j-A(j,14)%j+A(j,15)%j-A(j,40)%j+A(j,42)%j+A(j,43)%j-A(j,45)%j-A(j,46)%j+A(j,47)%j+A(j,52)%j &
         -A(j,53)%j+A(j,121)%j-A(j,122)%j+A(j,127)%j-A(j,129)%j-A(j,148)%j+A(j,150)%j-A(j,151)%j+A(j,152)%j-A(j,160)%j+A(j,161)%j &
         -A(j,188)%j+A(j,189)%j+A(j,191)%j-A(j,192)%j-A(j,193)%j+A(j,195)%j+A(j,199)%j-A(j,201)%j+A(j,206)%j-A(j,207)%j-A(j,226)%j &
         +A(j,228)%j-A(j,236)%j+A(j,237)%j+A(j,289)%j-A(j,290)%j-A(j,313)%j+A(j,314)%j-A(j,325)%j+A(j,327)%j-A(j,331)%j+A(j,332)%j &
         +A(j,346)%j-A(j,347)%j+A(j,355)%j-A(j,357)%j+A(j,358)%j-A(j,360)%j+A(j,389)%j-A(j,390)%j+A(j,395)%j-A(j,396)%j-A(j,421)%j &
         -A(j,422)%j+A(j,430)%j+A(j,431)%j+A(j,435)%j-A(j,454)%j-A(j,455)%j-A(j,457)%j-A(j,461)%j-A(j,462)%j-A(j,467)%j-A(j,468)%j &
         -A(j,473)%j+A(j,502)%j)*f(1)
  M1( 42) = 2*(-A(j,4)%j+A(j,6)%j+A(j,7)%j-A(j,9)%j+A(j,10)%j-A(j,11)%j-A(j,13)%j+A(j,14)%j+A(j,46)%j-A(j,47)%j-A(j,49)%j &
         +A(j,50)%j-A(j,94)%j+A(j,95)%j-A(j,101)%j+A(j,102)%j+A(j,106)%j-A(j,107)%j+A(j,109)%j-A(j,110)%j+A(j,112)%j-A(j,114)%j &
         -A(j,121)%j+A(j,122)%j+A(j,131)%j-A(j,132)%j+A(j,160)%j-A(j,161)%j-A(j,184)%j+A(j,185)%j-A(j,190)%j+A(j,192)%j+A(j,193)%j &
         -A(j,194)%j+A(j,226)%j-A(j,227)%j-A(j,289)%j+A(j,290)%j+A(j,325)%j-A(j,327)%j+A(j,331)%j-A(j,332)%j+A(j,349)%j-A(j,350)%j &
         +A(j,370)%j-A(j,372)%j+A(j,388)%j-A(j,389)%j+A(j,394)%j-A(j,395)%j+A(j,400)%j-A(j,402)%j+A(j,403)%j-A(j,405)%j+A(j,409)%j &
         +A(j,410)%j+A(j,417)%j+A(j,419)%j+A(j,420)%j+A(j,421)%j+A(j,422)%j-A(j,432)%j+A(j,433)%j-A(j,434)%j+A(j,467)%j+A(j,468)%j &
         +A(j,473)%j+A(j,503)%j)*f(1)
  M1( 43) = 2*(A(j,46)%j-A(j,48)%j-A(j,52)%j+A(j,54)%j+A(j,55)%j-A(j,56)%j-A(j,58)%j+A(j,59)%j+A(j,68)%j-A(j,69)%j-A(j,71)%j &
         +A(j,72)%j-A(j,121)%j+A(j,123)%j-A(j,128)%j+A(j,129)%j+A(j,154)%j-A(j,156)%j+A(j,164)%j-A(j,165)%j+A(j,182)%j-A(j,183)%j &
         +A(j,193)%j-A(j,195)%j+A(j,196)%j-A(j,197)%j-A(j,217)%j+A(j,218)%j-A(j,220)%j+A(j,222)%j+A(j,232)%j-A(j,233)%j-A(j,248)%j &
         +A(j,249)%j-A(j,254)%j+A(j,255)%j+A(j,293)%j-A(j,294)%j-A(j,320)%j+A(j,321)%j-A(j,323)%j+A(j,324)%j+A(j,328)%j-A(j,329)%j &
         +A(j,331)%j-A(j,333)%j-A(j,340)%j+A(j,341)%j+A(j,343)%j-A(j,344)%j-A(j,346)%j+A(j,348)%j+A(j,377)%j-A(j,378)%j+A(j,422)%j &
         +A(j,423)%j-A(j,427)%j+A(j,429)%j-A(j,431)%j-A(j,463)%j-A(j,464)%j+A(j,472)%j+A(j,487)%j+A(j,488)%j+A(j,490)%j+A(j,508)%j &
         +A(j,509)%j+A(j,510)%j)*f(1)
  M1( 44) = 2*(A(j,37)%j-A(j,39)%j-A(j,43)%j+A(j,45)%j+A(j,58)%j-A(j,59)%j-A(j,61)%j+A(j,62)%j+A(j,67)%j-A(j,68)%j-A(j,70)%j &
         +A(j,71)%j-A(j,124)%j+A(j,126)%j-A(j,127)%j+A(j,128)%j+A(j,145)%j-A(j,147)%j+A(j,148)%j-A(j,149)%j-A(j,182)%j+A(j,183)%j &
         +A(j,187)%j-A(j,189)%j+A(j,197)%j-A(j,198)%j-A(j,203)%j+A(j,204)%j-A(j,205)%j+A(j,207)%j+A(j,233)%j-A(j,234)%j+A(j,239)%j &
         -A(j,240)%j+A(j,254)%j-A(j,255)%j+A(j,307)%j-A(j,308)%j-A(j,319)%j+A(j,320)%j-A(j,322)%j+A(j,323)%j-A(j,337)%j+A(j,338)%j &
         +A(j,340)%j-A(j,341)%j-A(j,343)%j+A(j,344)%j+A(j,352)%j-A(j,354)%j-A(j,355)%j+A(j,357)%j-A(j,377)%j+A(j,378)%j+A(j,425)%j &
         -A(j,426)%j+A(j,427)%j-A(j,429)%j-A(j,430)%j+A(j,451)%j-A(j,452)%j+A(j,457)%j-A(j,458)%j-A(j,460)%j-A(j,496)%j-A(j,508)%j &
         -A(j,509)%j-A(j,510)%j)*f(1)
  M1( 45) = 2*(A(j,23)%j-A(j,24)%j-A(j,26)%j+A(j,27)%j-A(j,46)%j+A(j,48)%j+A(j,49)%j-A(j,51)%j-A(j,55)%j+A(j,56)%j+A(j,61)%j &
         -A(j,62)%j+A(j,121)%j-A(j,123)%j+A(j,125)%j-A(j,126)%j-A(j,154)%j+A(j,156)%j-A(j,167)%j+A(j,168)%j-A(j,173)%j+A(j,174)%j &
         -A(j,185)%j+A(j,186)%j-A(j,193)%j+A(j,194)%j-A(j,196)%j+A(j,198)%j+A(j,217)%j-A(j,219)%j+A(j,220)%j-A(j,221)%j-A(j,232)%j &
         +A(j,234)%j+A(j,245)%j-A(j,246)%j-A(j,293)%j+A(j,294)%j+A(j,308)%j-A(j,309)%j-A(j,328)%j+A(j,329)%j-A(j,331)%j+A(j,333)%j &
         +A(j,334)%j-A(j,336)%j+A(j,337)%j-A(j,338)%j-A(j,349)%j+A(j,351)%j-A(j,383)%j+A(j,384)%j-A(j,386)%j+A(j,387)%j-A(j,422)%j &
         -A(j,423)%j+A(j,424)%j+A(j,426)%j-A(j,433)%j+A(j,463)%j+A(j,464)%j-A(j,472)%j+A(j,489)%j+A(j,491)%j+A(j,492)%j+A(j,495)%j &
         +A(j,496)%j+A(j,497)%j)*f(1)
  M1( 46) = 2*(A(j,1)%j-A(j,3)%j-A(j,7)%j+A(j,9)%j+A(j,22)%j-A(j,23)%j-A(j,25)%j+A(j,26)%j+A(j,58)%j-A(j,59)%j-A(j,61)%j+A(j,62)%j &
         -A(j,98)%j+A(j,99)%j+A(j,100)%j-A(j,102)%j-A(j,103)%j+A(j,104)%j-A(j,106)%j+A(j,108)%j+A(j,116)%j-A(j,117)%j-A(j,125)%j &
         +A(j,126)%j-A(j,130)%j+A(j,132)%j+A(j,173)%j-A(j,174)%j+A(j,181)%j-A(j,182)%j+A(j,184)%j-A(j,186)%j+A(j,197)%j-A(j,198)%j &
         +A(j,233)%j-A(j,234)%j-A(j,308)%j+A(j,309)%j-A(j,334)%j+A(j,336)%j-A(j,337)%j+A(j,338)%j-A(j,343)%j+A(j,344)%j-A(j,376)%j &
         +A(j,378)%j-A(j,382)%j+A(j,383)%j-A(j,385)%j+A(j,386)%j+A(j,397)%j-A(j,399)%j-A(j,400)%j+A(j,402)%j-A(j,412)%j+A(j,413)%j &
         -A(j,416)%j-A(j,417)%j-A(j,418)%j-A(j,424)%j-A(j,426)%j-A(j,428)%j-A(j,429)%j+A(j,432)%j-A(j,495)%j-A(j,496)%j-A(j,497)%j &
         -A(j,509)%j)*f(1)
  M1( 47) = 2*(A(j,14)%j-A(j,15)%j-A(j,17)%j+A(j,18)%j-A(j,37)%j+A(j,39)%j+A(j,40)%j-A(j,42)%j-A(j,55)%j+A(j,56)%j+A(j,61)%j &
         -A(j,62)%j+A(j,122)%j-A(j,123)%j+A(j,124)%j-A(j,126)%j-A(j,145)%j+A(j,147)%j-A(j,152)%j+A(j,153)%j-A(j,161)%j+A(j,162)%j &
         -A(j,187)%j+A(j,188)%j-A(j,191)%j+A(j,192)%j-A(j,196)%j+A(j,198)%j+A(j,202)%j-A(j,204)%j+A(j,205)%j-A(j,206)%j-A(j,232)%j &
         +A(j,234)%j+A(j,236)%j-A(j,237)%j+A(j,292)%j-A(j,293)%j-A(j,307)%j+A(j,308)%j+A(j,325)%j-A(j,327)%j-A(j,328)%j+A(j,329)%j &
         +A(j,337)%j-A(j,338)%j-A(j,352)%j+A(j,354)%j-A(j,358)%j+A(j,360)%j-A(j,392)%j+A(j,393)%j-A(j,395)%j+A(j,396)%j+A(j,421)%j &
         -A(j,423)%j-A(j,425)%j+A(j,426)%j-A(j,435)%j-A(j,451)%j+A(j,452)%j+A(j,459)%j+A(j,461)%j+A(j,462)%j+A(j,471)%j-A(j,472)%j &
         +A(j,473)%j+A(j,496)%j)*f(1)
  M1( 48) = 2*(-A(j,1)%j+A(j,3)%j+A(j,4)%j-A(j,6)%j+A(j,13)%j-A(j,14)%j-A(j,16)%j+A(j,17)%j+A(j,55)%j-A(j,56)%j-A(j,58)%j &
         +A(j,59)%j-A(j,97)%j+A(j,98)%j-A(j,100)%j+A(j,101)%j+A(j,103)%j-A(j,104)%j+A(j,110)%j-A(j,111)%j-A(j,112)%j+A(j,114)%j &
         -A(j,122)%j+A(j,123)%j+A(j,130)%j-A(j,131)%j+A(j,161)%j-A(j,162)%j-A(j,181)%j+A(j,182)%j+A(j,190)%j-A(j,192)%j+A(j,196)%j &
         -A(j,197)%j+A(j,232)%j-A(j,233)%j-A(j,292)%j+A(j,293)%j-A(j,325)%j+A(j,327)%j+A(j,328)%j-A(j,329)%j+A(j,343)%j-A(j,344)%j &
         +A(j,376)%j-A(j,378)%j-A(j,391)%j+A(j,392)%j-A(j,394)%j+A(j,395)%j-A(j,397)%j+A(j,399)%j-A(j,403)%j+A(j,405)%j+A(j,412)%j &
         -A(j,413)%j-A(j,415)%j-A(j,419)%j-A(j,420)%j-A(j,421)%j+A(j,423)%j+A(j,428)%j+A(j,429)%j+A(j,434)%j-A(j,471)%j+A(j,472)%j &
         -A(j,473)%j+A(j,509)%j)*f(1)
  M1( 49) = 2*(A(j,11)%j-A(j,12)%j-A(j,14)%j+A(j,15)%j-A(j,73)%j+A(j,75)%j+A(j,76)%j-A(j,78)%j-A(j,83)%j+A(j,84)%j+A(j,89)%j &
         -A(j,90)%j+A(j,134)%j-A(j,135)%j-A(j,140)%j+A(j,141)%j-A(j,151)%j+A(j,152)%j+A(j,157)%j-A(j,158)%j-A(j,160)%j+A(j,161)%j &
         +A(j,199)%j-A(j,200)%j-A(j,223)%j+A(j,224)%j-A(j,235)%j+A(j,237)%j-A(j,241)%j+A(j,242)%j+A(j,256)%j-A(j,257)%j+A(j,265)%j &
         -A(j,267)%j+A(j,268)%j-A(j,270)%j-A(j,278)%j+A(j,279)%j+A(j,281)%j-A(j,282)%j-A(j,283)%j+A(j,285)%j+A(j,289)%j-A(j,291)%j &
         +A(j,296)%j-A(j,297)%j-A(j,316)%j+A(j,318)%j-A(j,326)%j+A(j,327)%j+A(j,389)%j-A(j,390)%j+A(j,395)%j-A(j,396)%j-A(j,436)%j &
         -A(j,438)%j+A(j,445)%j+A(j,446)%j+A(j,450)%j-A(j,455)%j-A(j,456)%j-A(j,461)%j-A(j,466)%j-A(j,467)%j-A(j,470)%j-A(j,473)%j &
         -A(j,474)%j+A(j,500)%j)*f(1)
  M1( 50) = 2*(A(j,14)%j-A(j,15)%j-A(j,17)%j+A(j,18)%j-A(j,64)%j+A(j,66)%j+A(j,67)%j-A(j,69)%j-A(j,82)%j+A(j,83)%j+A(j,88)%j &
         -A(j,89)%j+A(j,133)%j-A(j,134)%j-A(j,136)%j+A(j,137)%j-A(j,152)%j+A(j,153)%j+A(j,155)%j-A(j,156)%j-A(j,161)%j+A(j,162)%j &
         +A(j,202)%j-A(j,203)%j-A(j,217)%j+A(j,218)%j+A(j,235)%j-A(j,237)%j-A(j,238)%j+A(j,239)%j+A(j,247)%j-A(j,248)%j-A(j,262)%j &
         +A(j,264)%j-A(j,268)%j+A(j,270)%j-A(j,277)%j+A(j,278)%j-A(j,281)%j+A(j,282)%j-A(j,286)%j+A(j,288)%j+A(j,292)%j-A(j,294)%j &
         +A(j,295)%j-A(j,296)%j-A(j,322)%j+A(j,324)%j+A(j,326)%j-A(j,327)%j-A(j,392)%j+A(j,393)%j-A(j,395)%j+A(j,396)%j+A(j,436)%j &
         -A(j,437)%j+A(j,440)%j-A(j,441)%j-A(j,450)%j+A(j,459)%j-A(j,460)%j+A(j,461)%j-A(j,463)%j+A(j,465)%j+A(j,471)%j+A(j,473)%j &
         +A(j,474)%j+A(j,490)%j)*f(1)
  M1( 51) = 2*(-A(j,11)%j+A(j,12)%j+A(j,17)%j-A(j,18)%j-A(j,56)%j+A(j,57)%j+A(j,62)%j-A(j,63)%j-A(j,76)%j+A(j,78)%j+A(j,79)%j &
         -A(j,81)%j+A(j,146)%j-A(j,147)%j+A(j,151)%j-A(j,153)%j-A(j,157)%j+A(j,159)%j+A(j,160)%j-A(j,162)%j-A(j,170)%j+A(j,171)%j &
         -A(j,199)%j+A(j,200)%j-A(j,202)%j+A(j,204)%j+A(j,223)%j-A(j,224)%j+A(j,229)%j-A(j,231)%j+A(j,232)%j-A(j,234)%j-A(j,242)%j &
         +A(j,243)%j+A(j,257)%j-A(j,258)%j-A(j,289)%j+A(j,291)%j-A(j,292)%j+A(j,293)%j+A(j,307)%j-A(j,308)%j+A(j,310)%j-A(j,312)%j &
         +A(j,316)%j-A(j,318)%j-A(j,329)%j+A(j,330)%j+A(j,338)%j-A(j,339)%j-A(j,389)%j+A(j,390)%j+A(j,392)%j-A(j,393)%j+A(j,451)%j &
         +A(j,453)%j+A(j,455)%j+A(j,456)%j-A(j,459)%j+A(j,466)%j+A(j,467)%j+A(j,469)%j-A(j,471)%j+A(j,472)%j-A(j,493)%j-A(j,494)%j &
         -A(j,496)%j-A(j,500)%j)*f(1)
  M1( 52) = 2*(A(j,14)%j-A(j,15)%j-A(j,17)%j+A(j,18)%j-A(j,37)%j+A(j,39)%j+A(j,40)%j-A(j,42)%j-A(j,55)%j+A(j,56)%j+A(j,61)%j &
         -A(j,62)%j+A(j,122)%j-A(j,123)%j+A(j,124)%j-A(j,126)%j-A(j,145)%j+A(j,147)%j-A(j,152)%j+A(j,153)%j-A(j,161)%j+A(j,162)%j &
         -A(j,187)%j+A(j,188)%j-A(j,191)%j+A(j,192)%j-A(j,196)%j+A(j,198)%j+A(j,202)%j-A(j,204)%j+A(j,205)%j-A(j,206)%j-A(j,232)%j &
         +A(j,234)%j+A(j,236)%j-A(j,237)%j+A(j,292)%j-A(j,293)%j-A(j,307)%j+A(j,308)%j+A(j,325)%j-A(j,327)%j-A(j,328)%j+A(j,329)%j &
         +A(j,337)%j-A(j,338)%j-A(j,352)%j+A(j,354)%j-A(j,358)%j+A(j,360)%j-A(j,392)%j+A(j,393)%j-A(j,395)%j+A(j,396)%j+A(j,421)%j &
         -A(j,423)%j-A(j,425)%j+A(j,426)%j-A(j,435)%j-A(j,451)%j+A(j,452)%j+A(j,459)%j+A(j,461)%j+A(j,462)%j+A(j,471)%j-A(j,472)%j &
         +A(j,473)%j+A(j,496)%j)*f(1)
  M1( 53) = 2*(-A(j,11)%j+A(j,12)%j+A(j,17)%j-A(j,18)%j-A(j,47)%j+A(j,48)%j+A(j,53)%j-A(j,54)%j-A(j,67)%j+A(j,69)%j+A(j,70)%j &
         -A(j,72)%j+A(j,149)%j-A(j,150)%j+A(j,151)%j-A(j,153)%j-A(j,154)%j+A(j,156)%j+A(j,160)%j-A(j,162)%j-A(j,164)%j+A(j,165)%j &
         -A(j,199)%j+A(j,201)%j-A(j,202)%j+A(j,203)%j+A(j,217)%j-A(j,218)%j+A(j,220)%j-A(j,222)%j+A(j,226)%j-A(j,228)%j-A(j,239)%j &
         +A(j,240)%j+A(j,248)%j-A(j,249)%j-A(j,289)%j+A(j,290)%j-A(j,292)%j+A(j,294)%j+A(j,313)%j-A(j,314)%j+A(j,319)%j-A(j,321)%j &
         +A(j,322)%j-A(j,324)%j-A(j,332)%j+A(j,333)%j+A(j,347)%j-A(j,348)%j-A(j,389)%j+A(j,390)%j+A(j,392)%j-A(j,393)%j+A(j,454)%j &
         +A(j,455)%j+A(j,458)%j-A(j,459)%j+A(j,460)%j+A(j,463)%j+A(j,464)%j+A(j,467)%j+A(j,468)%j-A(j,471)%j-A(j,487)%j-A(j,488)%j &
         -A(j,490)%j-A(j,502)%j)*f(1)
  M1( 54) = 2*(A(j,11)%j-A(j,12)%j-A(j,14)%j+A(j,15)%j-A(j,40)%j+A(j,42)%j+A(j,43)%j-A(j,45)%j-A(j,46)%j+A(j,47)%j+A(j,52)%j &
         -A(j,53)%j+A(j,121)%j-A(j,122)%j+A(j,127)%j-A(j,129)%j-A(j,148)%j+A(j,150)%j-A(j,151)%j+A(j,152)%j-A(j,160)%j+A(j,161)%j &
         -A(j,188)%j+A(j,189)%j+A(j,191)%j-A(j,192)%j-A(j,193)%j+A(j,195)%j+A(j,199)%j-A(j,201)%j+A(j,206)%j-A(j,207)%j-A(j,226)%j &
         +A(j,228)%j-A(j,236)%j+A(j,237)%j+A(j,289)%j-A(j,290)%j-A(j,313)%j+A(j,314)%j-A(j,325)%j+A(j,327)%j-A(j,331)%j+A(j,332)%j &
         +A(j,346)%j-A(j,347)%j+A(j,355)%j-A(j,357)%j+A(j,358)%j-A(j,360)%j+A(j,389)%j-A(j,390)%j+A(j,395)%j-A(j,396)%j-A(j,421)%j &
         -A(j,422)%j+A(j,430)%j+A(j,431)%j+A(j,435)%j-A(j,454)%j-A(j,455)%j-A(j,457)%j-A(j,461)%j-A(j,462)%j-A(j,467)%j-A(j,468)%j &
         -A(j,473)%j+A(j,502)%j)*f(1)
  M1( 55) = 2*(A(j,38)%j-A(j,39)%j-A(j,41)%j+A(j,42)%j+A(j,73)%j-A(j,75)%j-A(j,79)%j+A(j,81)%j+A(j,83)%j-A(j,84)%j-A(j,86)%j &
         +A(j,87)%j-A(j,134)%j+A(j,135)%j+A(j,140)%j-A(j,141)%j+A(j,145)%j-A(j,146)%j-A(j,169)%j+A(j,170)%j-A(j,205)%j+A(j,206)%j &
         +A(j,211)%j-A(j,212)%j-A(j,214)%j+A(j,215)%j+A(j,235)%j-A(j,236)%j+A(j,241)%j-A(j,243)%j-A(j,256)%j+A(j,258)%j-A(j,265)%j &
         +A(j,266)%j-A(j,268)%j+A(j,269)%j+A(j,278)%j-A(j,279)%j-A(j,281)%j+A(j,282)%j+A(j,283)%j-A(j,285)%j-A(j,299)%j+A(j,300)%j &
         +A(j,305)%j-A(j,306)%j-A(j,310)%j+A(j,312)%j+A(j,353)%j-A(j,354)%j+A(j,359)%j-A(j,360)%j-A(j,362)%j+A(j,363)%j+A(j,436)%j &
         +A(j,438)%j-A(j,445)%j-A(j,446)%j-A(j,450)%j-A(j,452)%j-A(j,453)%j-A(j,462)%j+A(j,478)%j-A(j,480)%j-A(j,482)%j-A(j,485)%j &
         -A(j,486)%j+A(j,494)%j)*f(1)
  M1( 56) = 2*(A(j,41)%j-A(j,42)%j-A(j,44)%j+A(j,45)%j+A(j,64)%j-A(j,66)%j-A(j,70)%j+A(j,72)%j+A(j,82)%j-A(j,83)%j-A(j,85)%j &
         +A(j,86)%j-A(j,133)%j+A(j,134)%j+A(j,136)%j-A(j,137)%j+A(j,148)%j-A(j,149)%j-A(j,163)%j+A(j,164)%j-A(j,206)%j+A(j,207)%j &
         +A(j,209)%j-A(j,210)%j-A(j,215)%j+A(j,216)%j-A(j,235)%j+A(j,236)%j+A(j,238)%j-A(j,240)%j-A(j,247)%j+A(j,249)%j+A(j,262)%j &
         -A(j,263)%j+A(j,268)%j-A(j,269)%j+A(j,277)%j-A(j,278)%j+A(j,281)%j-A(j,282)%j+A(j,286)%j-A(j,288)%j-A(j,301)%j+A(j,302)%j &
         +A(j,304)%j-A(j,305)%j-A(j,319)%j+A(j,321)%j-A(j,356)%j+A(j,357)%j-A(j,359)%j+A(j,360)%j+A(j,362)%j-A(j,363)%j-A(j,436)%j &
         +A(j,437)%j-A(j,440)%j+A(j,441)%j+A(j,450)%j+A(j,457)%j-A(j,458)%j+A(j,462)%j-A(j,475)%j+A(j,477)%j+A(j,484)%j+A(j,485)%j &
         +A(j,486)%j+A(j,488)%j)*f(1)
  M1( 57) = 2*(-A(j,29)%j+A(j,30)%j+A(j,35)%j-A(j,36)%j-A(j,38)%j+A(j,39)%j+A(j,44)%j-A(j,45)%j-A(j,76)%j+A(j,78)%j+A(j,79)%j &
         -A(j,81)%j-A(j,145)%j+A(j,146)%j-A(j,148)%j+A(j,150)%j+A(j,169)%j-A(j,170)%j+A(j,175)%j-A(j,177)%j+A(j,178)%j-A(j,180)%j &
         +A(j,200)%j-A(j,201)%j+A(j,205)%j-A(j,207)%j-A(j,211)%j+A(j,213)%j+A(j,214)%j-A(j,216)%j-A(j,224)%j+A(j,225)%j-A(j,242)%j &
         +A(j,243)%j+A(j,257)%j-A(j,258)%j+A(j,299)%j-A(j,300)%j-A(j,302)%j+A(j,303)%j+A(j,310)%j-A(j,312)%j-A(j,313)%j+A(j,315)%j &
         +A(j,316)%j-A(j,318)%j-A(j,353)%j+A(j,354)%j+A(j,356)%j-A(j,357)%j-A(j,365)%j+A(j,366)%j+A(j,374)%j-A(j,375)%j+A(j,452)%j &
         +A(j,453)%j-A(j,454)%j+A(j,456)%j-A(j,457)%j-A(j,478)%j+A(j,480)%j+A(j,481)%j+A(j,483)%j-A(j,484)%j-A(j,494)%j-A(j,499)%j &
         -A(j,500)%j-A(j,501)%j)*f(1)
  M1( 58) = 2*(-A(j,10)%j+A(j,12)%j+A(j,13)%j-A(j,15)%j-A(j,28)%j+A(j,29)%j+A(j,34)%j-A(j,35)%j+A(j,41)%j-A(j,42)%j-A(j,44)%j &
         +A(j,45)%j+A(j,92)%j-A(j,93)%j+A(j,94)%j-A(j,96)%j-A(j,109)%j+A(j,110)%j-A(j,113)%j+A(j,114)%j-A(j,118)%j+A(j,120)%j &
         +A(j,148)%j-A(j,150)%j+A(j,151)%j-A(j,152)%j-A(j,178)%j+A(j,180)%j-A(j,199)%j+A(j,201)%j-A(j,206)%j+A(j,207)%j-A(j,215)%j &
         +A(j,216)%j+A(j,236)%j-A(j,237)%j+A(j,302)%j-A(j,303)%j+A(j,313)%j-A(j,315)%j-A(j,356)%j+A(j,357)%j-A(j,359)%j+A(j,360)%j &
         +A(j,361)%j-A(j,363)%j-A(j,364)%j+A(j,365)%j+A(j,373)%j-A(j,374)%j-A(j,388)%j+A(j,390)%j-A(j,394)%j+A(j,396)%j+A(j,406)%j &
         -A(j,408)%j-A(j,410)%j+A(j,411)%j-A(j,420)%j+A(j,454)%j+A(j,455)%j+A(j,457)%j+A(j,461)%j+A(j,462)%j-A(j,483)%j+A(j,484)%j &
         +A(j,485)%j+A(j,501)%j)*f(1)
  M1( 59) = 2*(-A(j,20)%j+A(j,21)%j+A(j,26)%j-A(j,27)%j-A(j,38)%j+A(j,39)%j+A(j,44)%j-A(j,45)%j-A(j,67)%j+A(j,69)%j+A(j,70)%j &
         -A(j,72)%j-A(j,145)%j+A(j,147)%j-A(j,148)%j+A(j,149)%j+A(j,163)%j-A(j,164)%j+A(j,166)%j-A(j,168)%j+A(j,172)%j-A(j,174)%j &
         +A(j,203)%j-A(j,204)%j+A(j,205)%j-A(j,207)%j-A(j,208)%j+A(j,210)%j+A(j,214)%j-A(j,216)%j-A(j,218)%j+A(j,219)%j-A(j,239)%j &
         +A(j,240)%j+A(j,248)%j-A(j,249)%j-A(j,298)%j+A(j,299)%j+A(j,301)%j-A(j,302)%j-A(j,307)%j+A(j,309)%j+A(j,319)%j-A(j,321)%j &
         +A(j,322)%j-A(j,324)%j-A(j,353)%j+A(j,354)%j+A(j,356)%j-A(j,357)%j-A(j,368)%j+A(j,369)%j+A(j,383)%j-A(j,384)%j-A(j,451)%j &
         +A(j,452)%j-A(j,457)%j+A(j,458)%j+A(j,460)%j+A(j,475)%j+A(j,476)%j+A(j,479)%j+A(j,480)%j-A(j,484)%j-A(j,488)%j-A(j,489)%j &
         -A(j,490)%j-A(j,495)%j)*f(1)
  M1( 60) = 2*(-A(j,13)%j+A(j,15)%j+A(j,16)%j-A(j,18)%j-A(j,19)%j+A(j,20)%j+A(j,25)%j-A(j,26)%j+A(j,38)%j-A(j,39)%j-A(j,41)%j &
         +A(j,42)%j+A(j,91)%j-A(j,92)%j+A(j,97)%j-A(j,99)%j-A(j,110)%j+A(j,111)%j+A(j,113)%j-A(j,114)%j-A(j,115)%j+A(j,117)%j &
         +A(j,145)%j-A(j,147)%j+A(j,152)%j-A(j,153)%j-A(j,172)%j+A(j,174)%j-A(j,202)%j+A(j,204)%j-A(j,205)%j+A(j,206)%j-A(j,214)%j &
         +A(j,215)%j-A(j,236)%j+A(j,237)%j+A(j,298)%j-A(j,299)%j+A(j,307)%j-A(j,309)%j+A(j,353)%j-A(j,354)%j+A(j,359)%j-A(j,360)%j &
         -A(j,361)%j+A(j,363)%j-A(j,367)%j+A(j,368)%j+A(j,382)%j-A(j,383)%j+A(j,391)%j-A(j,393)%j+A(j,394)%j-A(j,396)%j-A(j,406)%j &
         -A(j,407)%j+A(j,415)%j+A(j,416)%j+A(j,420)%j+A(j,451)%j-A(j,452)%j-A(j,459)%j-A(j,461)%j-A(j,462)%j-A(j,479)%j-A(j,480)%j &
         -A(j,485)%j+A(j,495)%j)*f(1)
  M1( 61) = 2*(A(j,59)%j-A(j,60)%j-A(j,62)%j+A(j,63)%j+A(j,64)%j-A(j,65)%j-A(j,67)%j+A(j,68)%j+A(j,73)%j-A(j,75)%j-A(j,79)%j &
         +A(j,81)%j-A(j,133)%j+A(j,135)%j+A(j,139)%j-A(j,141)%j-A(j,146)%j+A(j,147)%j+A(j,170)%j-A(j,171)%j+A(j,203)%j-A(j,204)%j &
         -A(j,230)%j+A(j,231)%j-A(j,233)%j+A(j,234)%j+A(j,238)%j-A(j,239)%j+A(j,241)%j-A(j,243)%j-A(j,250)%j+A(j,251)%j+A(j,253)%j &
         -A(j,254)%j-A(j,256)%j+A(j,258)%j+A(j,272)%j-A(j,273)%j+A(j,283)%j-A(j,285)%j+A(j,286)%j-A(j,287)%j-A(j,307)%j+A(j,308)%j &
         -A(j,310)%j+A(j,312)%j+A(j,322)%j-A(j,323)%j-A(j,338)%j+A(j,339)%j-A(j,344)%j+A(j,345)%j+A(j,377)%j-A(j,378)%j+A(j,437)%j &
         +A(j,438)%j-A(j,442)%j+A(j,444)%j-A(j,445)%j-A(j,451)%j-A(j,453)%j+A(j,460)%j+A(j,493)%j+A(j,494)%j+A(j,496)%j+A(j,506)%j &
         +A(j,509)%j+A(j,510)%j)*f(1)
  M1( 62) = 2*(A(j,37)%j-A(j,39)%j-A(j,43)%j+A(j,45)%j+A(j,58)%j-A(j,59)%j-A(j,61)%j+A(j,62)%j+A(j,67)%j-A(j,68)%j-A(j,70)%j &
         +A(j,71)%j-A(j,124)%j+A(j,126)%j-A(j,127)%j+A(j,128)%j+A(j,145)%j-A(j,147)%j+A(j,148)%j-A(j,149)%j-A(j,182)%j+A(j,183)%j &
         +A(j,187)%j-A(j,189)%j+A(j,197)%j-A(j,198)%j-A(j,203)%j+A(j,204)%j-A(j,205)%j+A(j,207)%j+A(j,233)%j-A(j,234)%j+A(j,239)%j &
         -A(j,240)%j+A(j,254)%j-A(j,255)%j+A(j,307)%j-A(j,308)%j-A(j,319)%j+A(j,320)%j-A(j,322)%j+A(j,323)%j-A(j,337)%j+A(j,338)%j &
         +A(j,340)%j-A(j,341)%j-A(j,343)%j+A(j,344)%j+A(j,352)%j-A(j,354)%j-A(j,355)%j+A(j,357)%j-A(j,377)%j+A(j,378)%j+A(j,425)%j &
         -A(j,426)%j+A(j,427)%j-A(j,429)%j-A(j,430)%j+A(j,451)%j-A(j,452)%j+A(j,457)%j-A(j,458)%j-A(j,460)%j-A(j,496)%j-A(j,508)%j &
         -A(j,509)%j-A(j,510)%j)*f(1)
  M1( 63) = 2*(A(j,32)%j-A(j,33)%j-A(j,35)%j+A(j,36)%j-A(j,64)%j+A(j,65)%j+A(j,70)%j-A(j,71)%j-A(j,73)%j+A(j,75)%j+A(j,76)%j &
         -A(j,78)%j+A(j,133)%j-A(j,135)%j-A(j,139)%j+A(j,141)%j+A(j,149)%j-A(j,150)%j-A(j,176)%j+A(j,177)%j-A(j,179)%j+A(j,180)%j &
         -A(j,200)%j+A(j,201)%j+A(j,224)%j-A(j,225)%j-A(j,238)%j+A(j,240)%j-A(j,241)%j+A(j,242)%j+A(j,250)%j-A(j,252)%j-A(j,253)%j &
         +A(j,255)%j+A(j,256)%j-A(j,257)%j-A(j,272)%j+A(j,273)%j-A(j,283)%j+A(j,285)%j-A(j,286)%j+A(j,287)%j+A(j,313)%j-A(j,315)%j &
         -A(j,316)%j+A(j,318)%j+A(j,319)%j-A(j,320)%j+A(j,341)%j-A(j,342)%j-A(j,374)%j+A(j,375)%j-A(j,380)%j+A(j,381)%j-A(j,437)%j &
         -A(j,438)%j+A(j,442)%j-A(j,444)%j+A(j,445)%j+A(j,454)%j-A(j,456)%j+A(j,458)%j+A(j,499)%j+A(j,500)%j+A(j,501)%j+A(j,505)%j &
         +A(j,507)%j+A(j,508)%j)*f(1)
  M1( 64) = 2*(A(j,10)%j-A(j,12)%j-A(j,16)%j+A(j,18)%j+A(j,31)%j-A(j,32)%j-A(j,34)%j+A(j,35)%j+A(j,67)%j-A(j,68)%j-A(j,70)%j &
         +A(j,71)%j-A(j,94)%j+A(j,96)%j-A(j,97)%j+A(j,98)%j-A(j,104)%j+A(j,105)%j+A(j,109)%j-A(j,111)%j+A(j,119)%j-A(j,120)%j &
         -A(j,149)%j+A(j,150)%j-A(j,151)%j+A(j,153)%j+A(j,179)%j-A(j,180)%j+A(j,199)%j-A(j,201)%j+A(j,202)%j-A(j,203)%j+A(j,239)%j &
         -A(j,240)%j+A(j,254)%j-A(j,255)%j-A(j,313)%j+A(j,315)%j-A(j,319)%j+A(j,320)%j-A(j,322)%j+A(j,323)%j-A(j,341)%j+A(j,342)%j &
         -A(j,373)%j+A(j,374)%j+A(j,376)%j-A(j,377)%j-A(j,379)%j+A(j,380)%j+A(j,388)%j-A(j,390)%j-A(j,391)%j+A(j,393)%j+A(j,410)%j &
         -A(j,411)%j+A(j,412)%j-A(j,414)%j-A(j,415)%j-A(j,454)%j-A(j,455)%j-A(j,458)%j+A(j,459)%j-A(j,460)%j-A(j,501)%j-A(j,507)%j &
         -A(j,508)%j-A(j,510)%j)*f(1)
  M1( 65) = 2*(A(j,2)%j-A(j,3)%j-A(j,5)%j+A(j,6)%j-A(j,40)%j+A(j,42)%j+A(j,43)%j-A(j,45)%j-A(j,64)%j+A(j,65)%j+A(j,70)%j-A(j,71)%j &
         +A(j,127)%j-A(j,128)%j-A(j,130)%j+A(j,131)%j+A(j,133)%j-A(j,134)%j-A(j,142)%j+A(j,143)%j-A(j,148)%j+A(j,149)%j+A(j,181)%j &
         -A(j,183)%j-A(j,188)%j+A(j,189)%j-A(j,190)%j+A(j,191)%j+A(j,206)%j-A(j,207)%j+A(j,235)%j-A(j,236)%j-A(j,238)%j+A(j,240)%j &
         -A(j,253)%j+A(j,255)%j+A(j,271)%j-A(j,272)%j-A(j,280)%j+A(j,282)%j-A(j,286)%j+A(j,287)%j+A(j,319)%j-A(j,320)%j-A(j,340)%j &
         +A(j,341)%j+A(j,355)%j-A(j,357)%j+A(j,358)%j-A(j,360)%j+A(j,398)%j-A(j,399)%j+A(j,404)%j-A(j,405)%j-A(j,427)%j-A(j,428)%j &
         +A(j,430)%j-A(j,434)%j+A(j,435)%j+A(j,436)%j-A(j,437)%j-A(j,443)%j-A(j,444)%j-A(j,449)%j-A(j,457)%j+A(j,458)%j-A(j,462)%j &
         +A(j,508)%j)*f(1)
  M1( 66) = 2*(A(j,1)%j-A(j,2)%j-A(j,4)%j+A(j,5)%j-A(j,13)%j+A(j,15)%j+A(j,16)%j-A(j,18)%j+A(j,64)%j-A(j,65)%j-A(j,67)%j+A(j,68)%j &
         +A(j,97)%j-A(j,98)%j+A(j,100)%j-A(j,101)%j-A(j,103)%j+A(j,104)%j-A(j,110)%j+A(j,111)%j+A(j,112)%j-A(j,114)%j-A(j,133)%j &
         +A(j,134)%j+A(j,142)%j-A(j,143)%j+A(j,152)%j-A(j,153)%j-A(j,202)%j+A(j,203)%j-A(j,235)%j+A(j,237)%j+A(j,238)%j-A(j,239)%j &
         +A(j,253)%j-A(j,254)%j-A(j,271)%j+A(j,272)%j+A(j,280)%j-A(j,282)%j+A(j,286)%j-A(j,287)%j+A(j,322)%j-A(j,323)%j-A(j,376)%j &
         +A(j,377)%j+A(j,391)%j-A(j,393)%j+A(j,394)%j-A(j,396)%j+A(j,397)%j-A(j,398)%j+A(j,403)%j-A(j,404)%j-A(j,412)%j+A(j,413)%j &
         +A(j,415)%j+A(j,419)%j+A(j,420)%j-A(j,436)%j+A(j,437)%j+A(j,443)%j+A(j,444)%j+A(j,449)%j-A(j,459)%j+A(j,460)%j-A(j,461)%j &
         +A(j,510)%j)*f(1)
  M1( 67) = 2*(A(j,50)%j-A(j,51)%j-A(j,53)%j+A(j,54)%j+A(j,64)%j-A(j,66)%j-A(j,70)%j+A(j,72)%j+A(j,73)%j-A(j,74)%j-A(j,76)%j &
         +A(j,77)%j-A(j,133)%j+A(j,135)%j+A(j,136)%j-A(j,138)%j-A(j,149)%j+A(j,150)%j+A(j,164)%j-A(j,165)%j+A(j,200)%j-A(j,201)%j &
         -A(j,221)%j+A(j,222)%j-A(j,227)%j+A(j,228)%j+A(j,238)%j-A(j,240)%j+A(j,241)%j-A(j,242)%j-A(j,244)%j+A(j,245)%j-A(j,247)%j &
         +A(j,249)%j+A(j,259)%j-A(j,260)%j+A(j,275)%j-A(j,276)%j+A(j,283)%j-A(j,284)%j+A(j,286)%j-A(j,288)%j-A(j,313)%j+A(j,314)%j &
         +A(j,316)%j-A(j,317)%j-A(j,319)%j+A(j,321)%j-A(j,347)%j+A(j,348)%j-A(j,350)%j+A(j,351)%j+A(j,371)%j-A(j,372)%j+A(j,437)%j &
         +A(j,438)%j-A(j,439)%j-A(j,440)%j+A(j,448)%j-A(j,454)%j+A(j,456)%j-A(j,458)%j+A(j,487)%j+A(j,488)%j+A(j,492)%j+A(j,502)%j &
         +A(j,503)%j+A(j,504)%j)*f(1)
  M1( 68) = 2*(A(j,37)%j-A(j,39)%j-A(j,43)%j+A(j,45)%j+A(j,49)%j-A(j,50)%j-A(j,52)%j+A(j,53)%j+A(j,76)%j-A(j,77)%j-A(j,79)%j &
         +A(j,80)%j-A(j,124)%j+A(j,125)%j-A(j,127)%j+A(j,129)%j+A(j,145)%j-A(j,146)%j+A(j,148)%j-A(j,150)%j-A(j,185)%j+A(j,186)%j &
         +A(j,187)%j-A(j,189)%j+A(j,194)%j-A(j,195)%j-A(j,200)%j+A(j,201)%j-A(j,205)%j+A(j,207)%j+A(j,227)%j-A(j,228)%j+A(j,242)%j &
         -A(j,243)%j+A(j,260)%j-A(j,261)%j-A(j,310)%j+A(j,311)%j+A(j,313)%j-A(j,314)%j-A(j,316)%j+A(j,317)%j+A(j,334)%j-A(j,335)%j &
         -A(j,346)%j+A(j,347)%j-A(j,349)%j+A(j,350)%j+A(j,352)%j-A(j,354)%j-A(j,355)%j+A(j,357)%j-A(j,371)%j+A(j,372)%j+A(j,424)%j &
         +A(j,425)%j-A(j,430)%j-A(j,431)%j-A(j,433)%j-A(j,452)%j-A(j,453)%j+A(j,454)%j-A(j,456)%j+A(j,457)%j-A(j,498)%j-A(j,502)%j &
         -A(j,503)%j-A(j,504)%j)*f(1)
  M1( 69) = 2*(A(j,23)%j-A(j,24)%j-A(j,26)%j+A(j,27)%j-A(j,64)%j+A(j,66)%j+A(j,67)%j-A(j,69)%j-A(j,73)%j+A(j,74)%j+A(j,79)%j &
         -A(j,80)%j+A(j,133)%j-A(j,135)%j-A(j,136)%j+A(j,138)%j+A(j,146)%j-A(j,147)%j-A(j,167)%j+A(j,168)%j-A(j,173)%j+A(j,174)%j &
         -A(j,203)%j+A(j,204)%j+A(j,218)%j-A(j,219)%j-A(j,238)%j+A(j,239)%j-A(j,241)%j+A(j,243)%j+A(j,244)%j-A(j,246)%j+A(j,247)%j &
         -A(j,248)%j-A(j,259)%j+A(j,261)%j-A(j,275)%j+A(j,276)%j-A(j,283)%j+A(j,284)%j-A(j,286)%j+A(j,288)%j+A(j,307)%j-A(j,309)%j &
         +A(j,310)%j-A(j,311)%j-A(j,322)%j+A(j,324)%j+A(j,335)%j-A(j,336)%j-A(j,383)%j+A(j,384)%j-A(j,386)%j+A(j,387)%j-A(j,437)%j &
         -A(j,438)%j+A(j,439)%j+A(j,440)%j-A(j,448)%j+A(j,451)%j+A(j,453)%j-A(j,460)%j+A(j,489)%j+A(j,490)%j+A(j,491)%j+A(j,495)%j &
         +A(j,497)%j+A(j,498)%j)*f(1)
  M1( 70) = 2*(A(j,10)%j-A(j,12)%j-A(j,16)%j+A(j,18)%j+A(j,22)%j-A(j,23)%j-A(j,25)%j+A(j,26)%j+A(j,76)%j-A(j,77)%j-A(j,79)%j &
         +A(j,80)%j-A(j,94)%j+A(j,95)%j-A(j,97)%j+A(j,99)%j-A(j,107)%j+A(j,108)%j+A(j,109)%j-A(j,111)%j+A(j,116)%j-A(j,117)%j &
         -A(j,146)%j+A(j,147)%j-A(j,151)%j+A(j,153)%j+A(j,173)%j-A(j,174)%j+A(j,199)%j-A(j,200)%j+A(j,202)%j-A(j,204)%j+A(j,242)%j &
         -A(j,243)%j+A(j,260)%j-A(j,261)%j-A(j,307)%j+A(j,309)%j-A(j,310)%j+A(j,311)%j-A(j,316)%j+A(j,317)%j-A(j,335)%j+A(j,336)%j &
         +A(j,370)%j-A(j,371)%j-A(j,382)%j+A(j,383)%j-A(j,385)%j+A(j,386)%j+A(j,388)%j-A(j,390)%j-A(j,391)%j+A(j,393)%j+A(j,409)%j &
         +A(j,410)%j-A(j,415)%j-A(j,416)%j-A(j,418)%j-A(j,451)%j-A(j,453)%j-A(j,455)%j-A(j,456)%j+A(j,459)%j-A(j,495)%j-A(j,497)%j &
         -A(j,498)%j-A(j,504)%j)*f(1)
  M1( 71) = 2*(A(j,5)%j-A(j,6)%j-A(j,8)%j+A(j,9)%j-A(j,37)%j+A(j,39)%j+A(j,40)%j-A(j,42)%j-A(j,73)%j+A(j,74)%j+A(j,79)%j-A(j,80)%j &
         +A(j,124)%j-A(j,125)%j-A(j,131)%j+A(j,132)%j+A(j,134)%j-A(j,135)%j-A(j,143)%j+A(j,144)%j-A(j,145)%j+A(j,146)%j+A(j,184)%j &
         -A(j,186)%j-A(j,187)%j+A(j,188)%j+A(j,190)%j-A(j,191)%j+A(j,205)%j-A(j,206)%j-A(j,235)%j+A(j,236)%j-A(j,241)%j+A(j,243)%j &
         -A(j,259)%j+A(j,261)%j+A(j,274)%j-A(j,275)%j+A(j,280)%j-A(j,282)%j-A(j,283)%j+A(j,284)%j+A(j,310)%j-A(j,311)%j-A(j,334)%j &
         +A(j,335)%j-A(j,352)%j+A(j,354)%j-A(j,358)%j+A(j,360)%j-A(j,401)%j+A(j,402)%j-A(j,404)%j+A(j,405)%j-A(j,424)%j-A(j,425)%j &
         +A(j,432)%j+A(j,434)%j-A(j,435)%j-A(j,436)%j-A(j,438)%j+A(j,447)%j-A(j,448)%j+A(j,449)%j+A(j,452)%j+A(j,453)%j+A(j,462)%j &
         +A(j,498)%j)*f(1)
  M1( 72) = 2*(A(j,4)%j-A(j,5)%j-A(j,7)%j+A(j,8)%j-A(j,10)%j+A(j,12)%j+A(j,13)%j-A(j,15)%j+A(j,73)%j-A(j,74)%j-A(j,76)%j+A(j,77)%j &
         +A(j,94)%j-A(j,95)%j+A(j,101)%j-A(j,102)%j-A(j,106)%j+A(j,107)%j-A(j,109)%j+A(j,110)%j-A(j,112)%j+A(j,114)%j-A(j,134)%j &
         +A(j,135)%j+A(j,143)%j-A(j,144)%j+A(j,151)%j-A(j,152)%j-A(j,199)%j+A(j,200)%j+A(j,235)%j-A(j,237)%j+A(j,241)%j-A(j,242)%j &
         +A(j,259)%j-A(j,260)%j-A(j,274)%j+A(j,275)%j-A(j,280)%j+A(j,282)%j+A(j,283)%j-A(j,284)%j+A(j,316)%j-A(j,317)%j-A(j,370)%j &
         +A(j,371)%j-A(j,388)%j+A(j,390)%j-A(j,394)%j+A(j,396)%j-A(j,400)%j+A(j,401)%j-A(j,403)%j+A(j,404)%j-A(j,409)%j-A(j,410)%j &
         -A(j,417)%j-A(j,419)%j-A(j,420)%j+A(j,436)%j+A(j,438)%j-A(j,447)%j+A(j,448)%j-A(j,449)%j+A(j,455)%j+A(j,456)%j+A(j,461)%j &
         +A(j,504)%j)*f(1)
  M1( 73) = 2*(A(j,20)%j-A(j,21)%j-A(j,23)%j+A(j,24)%j-A(j,74)%j+A(j,75)%j+A(j,80)%j-A(j,81)%j-A(j,82)%j+A(j,84)%j+A(j,85)%j &
         -A(j,87)%j+A(j,137)%j-A(j,138)%j-A(j,140)%j+A(j,141)%j-A(j,166)%j+A(j,167)%j+A(j,169)%j-A(j,170)%j-A(j,172)%j+A(j,173)%j &
         +A(j,208)%j-A(j,209)%j-A(j,211)%j+A(j,212)%j-A(j,244)%j+A(j,246)%j+A(j,256)%j-A(j,258)%j+A(j,259)%j-A(j,261)%j-A(j,262)%j &
         +A(j,263)%j+A(j,265)%j-A(j,266)%j+A(j,275)%j-A(j,276)%j-A(j,277)%j+A(j,279)%j-A(j,284)%j+A(j,285)%j+A(j,298)%j-A(j,300)%j &
         -A(j,304)%j+A(j,306)%j+A(j,311)%j-A(j,312)%j-A(j,335)%j+A(j,336)%j+A(j,368)%j-A(j,369)%j+A(j,386)%j-A(j,387)%j-A(j,439)%j &
         -A(j,441)%j+A(j,445)%j+A(j,446)%j+A(j,448)%j-A(j,476)%j-A(j,477)%j-A(j,478)%j-A(j,479)%j+A(j,482)%j-A(j,491)%j-A(j,494)%j &
         -A(j,497)%j-A(j,498)%j)*f(1)
  M1( 74) = 2*(A(j,23)%j-A(j,24)%j-A(j,26)%j+A(j,27)%j-A(j,64)%j+A(j,66)%j+A(j,67)%j-A(j,69)%j-A(j,73)%j+A(j,74)%j+A(j,79)%j &
         -A(j,80)%j+A(j,133)%j-A(j,135)%j-A(j,136)%j+A(j,138)%j+A(j,146)%j-A(j,147)%j-A(j,167)%j+A(j,168)%j-A(j,173)%j+A(j,174)%j &
         -A(j,203)%j+A(j,204)%j+A(j,218)%j-A(j,219)%j-A(j,238)%j+A(j,239)%j-A(j,241)%j+A(j,243)%j+A(j,244)%j-A(j,246)%j+A(j,247)%j &
         -A(j,248)%j-A(j,259)%j+A(j,261)%j-A(j,275)%j+A(j,276)%j-A(j,283)%j+A(j,284)%j-A(j,286)%j+A(j,288)%j+A(j,307)%j-A(j,309)%j &
         +A(j,310)%j-A(j,311)%j-A(j,322)%j+A(j,324)%j+A(j,335)%j-A(j,336)%j-A(j,383)%j+A(j,384)%j-A(j,386)%j+A(j,387)%j-A(j,437)%j &
         -A(j,438)%j+A(j,439)%j+A(j,440)%j-A(j,448)%j+A(j,451)%j+A(j,453)%j-A(j,460)%j+A(j,489)%j+A(j,490)%j+A(j,491)%j+A(j,495)%j &
         +A(j,497)%j+A(j,498)%j)*f(1)
  M1( 75) = 2*(-A(j,20)%j+A(j,21)%j+A(j,26)%j-A(j,27)%j-A(j,56)%j+A(j,57)%j+A(j,62)%j-A(j,63)%j-A(j,85)%j+A(j,87)%j+A(j,88)%j &
         -A(j,90)%j+A(j,155)%j-A(j,156)%j-A(j,158)%j+A(j,159)%j+A(j,166)%j-A(j,168)%j-A(j,169)%j+A(j,171)%j+A(j,172)%j-A(j,174)%j &
         -A(j,208)%j+A(j,209)%j+A(j,211)%j-A(j,212)%j-A(j,217)%j+A(j,219)%j+A(j,229)%j-A(j,231)%j+A(j,232)%j-A(j,234)%j-A(j,263)%j &
         +A(j,264)%j+A(j,266)%j-A(j,267)%j+A(j,293)%j-A(j,294)%j+A(j,295)%j-A(j,297)%j-A(j,298)%j+A(j,300)%j+A(j,304)%j-A(j,306)%j &
         -A(j,308)%j+A(j,309)%j-A(j,329)%j+A(j,330)%j+A(j,338)%j-A(j,339)%j-A(j,368)%j+A(j,369)%j+A(j,383)%j-A(j,384)%j-A(j,463)%j &
         +A(j,465)%j+A(j,469)%j-A(j,470)%j+A(j,472)%j+A(j,476)%j+A(j,477)%j+A(j,478)%j+A(j,479)%j-A(j,482)%j-A(j,489)%j-A(j,493)%j &
         -A(j,495)%j-A(j,496)%j)*f(1)
  M1( 76) = 2*(A(j,23)%j-A(j,24)%j-A(j,26)%j+A(j,27)%j-A(j,46)%j+A(j,48)%j+A(j,49)%j-A(j,51)%j-A(j,55)%j+A(j,56)%j+A(j,61)%j &
         -A(j,62)%j+A(j,121)%j-A(j,123)%j+A(j,125)%j-A(j,126)%j-A(j,154)%j+A(j,156)%j-A(j,167)%j+A(j,168)%j-A(j,173)%j+A(j,174)%j &
         -A(j,185)%j+A(j,186)%j-A(j,193)%j+A(j,194)%j-A(j,196)%j+A(j,198)%j+A(j,217)%j-A(j,219)%j+A(j,220)%j-A(j,221)%j-A(j,232)%j &
         +A(j,234)%j+A(j,245)%j-A(j,246)%j-A(j,293)%j+A(j,294)%j+A(j,308)%j-A(j,309)%j-A(j,328)%j+A(j,329)%j-A(j,331)%j+A(j,333)%j &
         +A(j,334)%j-A(j,336)%j+A(j,337)%j-A(j,338)%j-A(j,349)%j+A(j,351)%j-A(j,383)%j+A(j,384)%j-A(j,386)%j+A(j,387)%j-A(j,422)%j &
         -A(j,423)%j+A(j,424)%j+A(j,426)%j-A(j,433)%j+A(j,463)%j+A(j,464)%j-A(j,472)%j+A(j,489)%j+A(j,491)%j+A(j,492)%j+A(j,495)%j &
         +A(j,496)%j+A(j,497)%j)*f(1)
  M1( 77) = 2*(-A(j,20)%j+A(j,21)%j+A(j,26)%j-A(j,27)%j-A(j,38)%j+A(j,39)%j+A(j,44)%j-A(j,45)%j-A(j,67)%j+A(j,69)%j+A(j,70)%j &
         -A(j,72)%j-A(j,145)%j+A(j,147)%j-A(j,148)%j+A(j,149)%j+A(j,163)%j-A(j,164)%j+A(j,166)%j-A(j,168)%j+A(j,172)%j-A(j,174)%j &
         +A(j,203)%j-A(j,204)%j+A(j,205)%j-A(j,207)%j-A(j,208)%j+A(j,210)%j+A(j,214)%j-A(j,216)%j-A(j,218)%j+A(j,219)%j-A(j,239)%j &
         +A(j,240)%j+A(j,248)%j-A(j,249)%j-A(j,298)%j+A(j,299)%j+A(j,301)%j-A(j,302)%j-A(j,307)%j+A(j,309)%j+A(j,319)%j-A(j,321)%j &
         +A(j,322)%j-A(j,324)%j-A(j,353)%j+A(j,354)%j+A(j,356)%j-A(j,357)%j-A(j,368)%j+A(j,369)%j+A(j,383)%j-A(j,384)%j-A(j,451)%j &
         +A(j,452)%j-A(j,457)%j+A(j,458)%j+A(j,460)%j+A(j,475)%j+A(j,476)%j+A(j,479)%j+A(j,480)%j-A(j,484)%j-A(j,488)%j-A(j,489)%j &
         -A(j,490)%j-A(j,495)%j)*f(1)
  M1( 78) = 2*(A(j,20)%j-A(j,21)%j-A(j,23)%j+A(j,24)%j-A(j,37)%j+A(j,38)%j+A(j,43)%j-A(j,44)%j-A(j,49)%j+A(j,51)%j+A(j,52)%j &
         -A(j,54)%j+A(j,124)%j-A(j,125)%j+A(j,127)%j-A(j,129)%j-A(j,163)%j+A(j,165)%j-A(j,166)%j+A(j,167)%j-A(j,172)%j+A(j,173)%j &
         +A(j,185)%j-A(j,186)%j-A(j,187)%j+A(j,189)%j-A(j,194)%j+A(j,195)%j+A(j,208)%j-A(j,210)%j-A(j,214)%j+A(j,216)%j+A(j,221)%j &
         -A(j,222)%j-A(j,245)%j+A(j,246)%j+A(j,298)%j-A(j,299)%j-A(j,301)%j+A(j,302)%j-A(j,334)%j+A(j,336)%j+A(j,346)%j-A(j,348)%j &
         +A(j,349)%j-A(j,351)%j-A(j,352)%j+A(j,353)%j+A(j,355)%j-A(j,356)%j+A(j,368)%j-A(j,369)%j+A(j,386)%j-A(j,387)%j-A(j,424)%j &
         -A(j,425)%j+A(j,430)%j+A(j,431)%j+A(j,433)%j-A(j,475)%j-A(j,476)%j-A(j,479)%j-A(j,480)%j+A(j,484)%j-A(j,487)%j-A(j,491)%j &
         -A(j,492)%j-A(j,497)%j)*f(1)
  M1( 79) = 2*(A(j,47)%j-A(j,48)%j-A(j,50)%j+A(j,51)%j+A(j,74)%j-A(j,75)%j-A(j,77)%j+A(j,78)%j+A(j,82)%j-A(j,84)%j-A(j,88)%j &
         +A(j,90)%j-A(j,137)%j+A(j,138)%j+A(j,140)%j-A(j,141)%j+A(j,154)%j-A(j,155)%j-A(j,157)%j+A(j,158)%j-A(j,220)%j+A(j,221)%j &
         +A(j,223)%j-A(j,224)%j-A(j,226)%j+A(j,227)%j+A(j,244)%j-A(j,245)%j-A(j,256)%j+A(j,257)%j-A(j,259)%j+A(j,260)%j+A(j,262)%j &
         -A(j,264)%j-A(j,265)%j+A(j,267)%j-A(j,275)%j+A(j,276)%j+A(j,277)%j-A(j,279)%j+A(j,284)%j-A(j,285)%j-A(j,290)%j+A(j,291)%j &
         -A(j,295)%j+A(j,297)%j+A(j,317)%j-A(j,318)%j+A(j,332)%j-A(j,333)%j+A(j,350)%j-A(j,351)%j-A(j,371)%j+A(j,372)%j+A(j,439)%j &
         +A(j,441)%j-A(j,445)%j-A(j,446)%j-A(j,448)%j-A(j,464)%j-A(j,465)%j+A(j,466)%j-A(j,468)%j+A(j,470)%j-A(j,492)%j-A(j,500)%j &
         -A(j,503)%j-A(j,504)%j)*f(1)
  M1( 80) = 2*(A(j,50)%j-A(j,51)%j-A(j,53)%j+A(j,54)%j+A(j,64)%j-A(j,66)%j-A(j,70)%j+A(j,72)%j+A(j,73)%j-A(j,74)%j-A(j,76)%j &
         +A(j,77)%j-A(j,133)%j+A(j,135)%j+A(j,136)%j-A(j,138)%j-A(j,149)%j+A(j,150)%j+A(j,164)%j-A(j,165)%j+A(j,200)%j-A(j,201)%j &
         -A(j,221)%j+A(j,222)%j-A(j,227)%j+A(j,228)%j+A(j,238)%j-A(j,240)%j+A(j,241)%j-A(j,242)%j-A(j,244)%j+A(j,245)%j-A(j,247)%j &
         +A(j,249)%j+A(j,259)%j-A(j,260)%j+A(j,275)%j-A(j,276)%j+A(j,283)%j-A(j,284)%j+A(j,286)%j-A(j,288)%j-A(j,313)%j+A(j,314)%j &
         +A(j,316)%j-A(j,317)%j-A(j,319)%j+A(j,321)%j-A(j,347)%j+A(j,348)%j-A(j,350)%j+A(j,351)%j+A(j,371)%j-A(j,372)%j+A(j,437)%j &
         +A(j,438)%j-A(j,439)%j-A(j,440)%j+A(j,448)%j-A(j,454)%j+A(j,456)%j-A(j,458)%j+A(j,487)%j+A(j,488)%j+A(j,492)%j+A(j,502)%j &
         +A(j,503)%j+A(j,504)%j)*f(1)
  M1( 81) = 2*(-A(j,29)%j+A(j,30)%j+A(j,35)%j-A(j,36)%j-A(j,47)%j+A(j,48)%j+A(j,53)%j-A(j,54)%j-A(j,85)%j+A(j,87)%j+A(j,88)%j &
         -A(j,90)%j-A(j,154)%j+A(j,155)%j+A(j,157)%j-A(j,158)%j-A(j,163)%j+A(j,165)%j+A(j,175)%j-A(j,177)%j+A(j,178)%j-A(j,180)%j &
         +A(j,209)%j-A(j,210)%j-A(j,212)%j+A(j,213)%j+A(j,220)%j-A(j,222)%j-A(j,223)%j+A(j,225)%j+A(j,226)%j-A(j,228)%j-A(j,263)%j &
         +A(j,264)%j+A(j,266)%j-A(j,267)%j+A(j,290)%j-A(j,291)%j+A(j,295)%j-A(j,297)%j-A(j,301)%j+A(j,303)%j+A(j,304)%j-A(j,306)%j &
         -A(j,314)%j+A(j,315)%j-A(j,332)%j+A(j,333)%j+A(j,347)%j-A(j,348)%j-A(j,365)%j+A(j,366)%j+A(j,374)%j-A(j,375)%j+A(j,464)%j &
         +A(j,465)%j-A(j,466)%j+A(j,468)%j-A(j,470)%j-A(j,475)%j+A(j,477)%j+A(j,481)%j-A(j,482)%j+A(j,483)%j-A(j,487)%j-A(j,499)%j &
         -A(j,501)%j-A(j,502)%j)*f(1)
  M1( 82) = 2*(-A(j,19)%j+A(j,21)%j+A(j,22)%j-A(j,24)%j-A(j,28)%j+A(j,29)%j+A(j,34)%j-A(j,35)%j+A(j,50)%j-A(j,51)%j-A(j,53)%j &
         +A(j,54)%j+A(j,91)%j-A(j,93)%j+A(j,95)%j-A(j,96)%j-A(j,107)%j+A(j,108)%j-A(j,115)%j+A(j,116)%j-A(j,118)%j+A(j,120)%j &
         +A(j,163)%j-A(j,165)%j+A(j,166)%j-A(j,167)%j-A(j,178)%j+A(j,180)%j-A(j,208)%j+A(j,210)%j-A(j,221)%j+A(j,222)%j-A(j,227)%j &
         +A(j,228)%j+A(j,245)%j-A(j,246)%j+A(j,301)%j-A(j,303)%j+A(j,314)%j-A(j,315)%j-A(j,347)%j+A(j,348)%j-A(j,350)%j+A(j,351)%j &
         -A(j,364)%j+A(j,365)%j-A(j,367)%j+A(j,369)%j+A(j,370)%j-A(j,372)%j+A(j,373)%j-A(j,374)%j-A(j,385)%j+A(j,387)%j-A(j,407)%j &
         -A(j,408)%j+A(j,409)%j+A(j,411)%j-A(j,418)%j+A(j,475)%j+A(j,476)%j-A(j,483)%j+A(j,487)%j+A(j,491)%j+A(j,492)%j+A(j,501)%j &
         +A(j,502)%j+A(j,503)%j)*f(1)
  M1( 83) = 2*(-A(j,11)%j+A(j,12)%j+A(j,17)%j-A(j,18)%j-A(j,47)%j+A(j,48)%j+A(j,53)%j-A(j,54)%j-A(j,67)%j+A(j,69)%j+A(j,70)%j &
         -A(j,72)%j+A(j,149)%j-A(j,150)%j+A(j,151)%j-A(j,153)%j-A(j,154)%j+A(j,156)%j+A(j,160)%j-A(j,162)%j-A(j,164)%j+A(j,165)%j &
         -A(j,199)%j+A(j,201)%j-A(j,202)%j+A(j,203)%j+A(j,217)%j-A(j,218)%j+A(j,220)%j-A(j,222)%j+A(j,226)%j-A(j,228)%j-A(j,239)%j &
         +A(j,240)%j+A(j,248)%j-A(j,249)%j-A(j,289)%j+A(j,290)%j-A(j,292)%j+A(j,294)%j+A(j,313)%j-A(j,314)%j+A(j,319)%j-A(j,321)%j &
         +A(j,322)%j-A(j,324)%j-A(j,332)%j+A(j,333)%j+A(j,347)%j-A(j,348)%j-A(j,389)%j+A(j,390)%j+A(j,392)%j-A(j,393)%j+A(j,454)%j &
         +A(j,455)%j+A(j,458)%j-A(j,459)%j+A(j,460)%j+A(j,463)%j+A(j,464)%j+A(j,467)%j+A(j,468)%j-A(j,471)%j-A(j,487)%j-A(j,488)%j &
         -A(j,490)%j-A(j,502)%j)*f(1)
  M1( 84) = 2*(-A(j,10)%j+A(j,11)%j+A(j,16)%j-A(j,17)%j-A(j,22)%j+A(j,24)%j+A(j,25)%j-A(j,27)%j+A(j,47)%j-A(j,48)%j-A(j,50)%j &
         +A(j,51)%j+A(j,94)%j-A(j,95)%j+A(j,97)%j-A(j,99)%j+A(j,107)%j-A(j,108)%j-A(j,109)%j+A(j,111)%j-A(j,116)%j+A(j,117)%j &
         +A(j,154)%j-A(j,156)%j-A(j,160)%j+A(j,162)%j+A(j,167)%j-A(j,168)%j-A(j,217)%j+A(j,219)%j-A(j,220)%j+A(j,221)%j-A(j,226)%j &
         +A(j,227)%j-A(j,245)%j+A(j,246)%j+A(j,289)%j-A(j,290)%j+A(j,292)%j-A(j,294)%j+A(j,332)%j-A(j,333)%j+A(j,350)%j-A(j,351)%j &
         -A(j,370)%j+A(j,372)%j+A(j,382)%j-A(j,384)%j+A(j,385)%j-A(j,387)%j-A(j,388)%j+A(j,389)%j+A(j,391)%j-A(j,392)%j-A(j,409)%j &
         -A(j,410)%j+A(j,415)%j+A(j,416)%j+A(j,418)%j-A(j,463)%j-A(j,464)%j-A(j,467)%j-A(j,468)%j+A(j,471)%j-A(j,489)%j-A(j,491)%j &
         -A(j,492)%j-A(j,503)%j)*f(1)
  M1( 85) = 2*(A(j,56)%j-A(j,57)%j-A(j,59)%j+A(j,60)%j+A(j,65)%j-A(j,66)%j-A(j,68)%j+A(j,69)%j+A(j,82)%j-A(j,84)%j-A(j,88)%j &
         +A(j,90)%j+A(j,136)%j-A(j,137)%j-A(j,139)%j+A(j,140)%j-A(j,155)%j+A(j,156)%j+A(j,158)%j-A(j,159)%j+A(j,217)%j-A(j,218)%j &
         -A(j,229)%j+A(j,230)%j-A(j,232)%j+A(j,233)%j-A(j,247)%j+A(j,248)%j+A(j,250)%j-A(j,251)%j-A(j,253)%j+A(j,254)%j+A(j,262)%j &
         -A(j,264)%j-A(j,265)%j+A(j,267)%j-A(j,272)%j+A(j,273)%j+A(j,277)%j-A(j,279)%j+A(j,287)%j-A(j,288)%j-A(j,293)%j+A(j,294)%j &
         -A(j,295)%j+A(j,297)%j+A(j,323)%j-A(j,324)%j+A(j,329)%j-A(j,330)%j+A(j,344)%j-A(j,345)%j-A(j,377)%j+A(j,378)%j-A(j,440)%j &
         +A(j,441)%j+A(j,442)%j-A(j,444)%j-A(j,446)%j+A(j,463)%j-A(j,465)%j-A(j,469)%j+A(j,470)%j-A(j,472)%j-A(j,490)%j-A(j,506)%j &
         -A(j,509)%j-A(j,510)%j)*f(1)
  M1( 86) = 2*(A(j,46)%j-A(j,48)%j-A(j,52)%j+A(j,54)%j+A(j,55)%j-A(j,56)%j-A(j,58)%j+A(j,59)%j+A(j,68)%j-A(j,69)%j-A(j,71)%j &
         +A(j,72)%j-A(j,121)%j+A(j,123)%j-A(j,128)%j+A(j,129)%j+A(j,154)%j-A(j,156)%j+A(j,164)%j-A(j,165)%j+A(j,182)%j-A(j,183)%j &
         +A(j,193)%j-A(j,195)%j+A(j,196)%j-A(j,197)%j-A(j,217)%j+A(j,218)%j-A(j,220)%j+A(j,222)%j+A(j,232)%j-A(j,233)%j-A(j,248)%j &
         +A(j,249)%j-A(j,254)%j+A(j,255)%j+A(j,293)%j-A(j,294)%j-A(j,320)%j+A(j,321)%j-A(j,323)%j+A(j,324)%j+A(j,328)%j-A(j,329)%j &
         +A(j,331)%j-A(j,333)%j-A(j,340)%j+A(j,341)%j+A(j,343)%j-A(j,344)%j-A(j,346)%j+A(j,348)%j+A(j,377)%j-A(j,378)%j+A(j,422)%j &
         +A(j,423)%j-A(j,427)%j+A(j,429)%j-A(j,431)%j-A(j,463)%j-A(j,464)%j+A(j,472)%j+A(j,487)%j+A(j,488)%j+A(j,490)%j+A(j,508)%j &
         +A(j,509)%j+A(j,510)%j)*f(1)
  M1( 87) = 2*(A(j,29)%j-A(j,30)%j-A(j,32)%j+A(j,33)%j-A(j,65)%j+A(j,66)%j+A(j,71)%j-A(j,72)%j-A(j,82)%j+A(j,84)%j+A(j,85)%j &
         -A(j,87)%j-A(j,136)%j+A(j,137)%j+A(j,139)%j-A(j,140)%j+A(j,163)%j-A(j,164)%j-A(j,175)%j+A(j,176)%j-A(j,178)%j+A(j,179)%j &
         -A(j,209)%j+A(j,210)%j+A(j,212)%j-A(j,213)%j+A(j,247)%j-A(j,249)%j-A(j,250)%j+A(j,252)%j+A(j,253)%j-A(j,255)%j-A(j,262)%j &
         +A(j,263)%j+A(j,265)%j-A(j,266)%j+A(j,272)%j-A(j,273)%j-A(j,277)%j+A(j,279)%j-A(j,287)%j+A(j,288)%j+A(j,301)%j-A(j,303)%j &
         -A(j,304)%j+A(j,306)%j+A(j,320)%j-A(j,321)%j-A(j,341)%j+A(j,342)%j+A(j,365)%j-A(j,366)%j+A(j,380)%j-A(j,381)%j+A(j,440)%j &
         -A(j,441)%j-A(j,442)%j+A(j,444)%j+A(j,446)%j+A(j,475)%j-A(j,477)%j-A(j,481)%j+A(j,482)%j-A(j,483)%j-A(j,488)%j-A(j,505)%j &
         -A(j,507)%j-A(j,508)%j)*f(1)
  M1( 88) = 2*(A(j,19)%j-A(j,21)%j-A(j,25)%j+A(j,27)%j+A(j,28)%j-A(j,29)%j-A(j,31)%j+A(j,32)%j+A(j,68)%j-A(j,69)%j-A(j,71)%j &
         +A(j,72)%j-A(j,91)%j+A(j,93)%j-A(j,98)%j+A(j,99)%j+A(j,104)%j-A(j,105)%j+A(j,115)%j-A(j,117)%j+A(j,118)%j-A(j,119)%j &
         -A(j,163)%j+A(j,164)%j-A(j,166)%j+A(j,168)%j+A(j,178)%j-A(j,179)%j+A(j,208)%j-A(j,210)%j+A(j,218)%j-A(j,219)%j-A(j,248)%j &
         +A(j,249)%j-A(j,254)%j+A(j,255)%j-A(j,301)%j+A(j,303)%j-A(j,320)%j+A(j,321)%j-A(j,323)%j+A(j,324)%j+A(j,341)%j-A(j,342)%j &
         +A(j,364)%j-A(j,365)%j+A(j,367)%j-A(j,369)%j-A(j,376)%j+A(j,377)%j+A(j,379)%j-A(j,380)%j-A(j,382)%j+A(j,384)%j+A(j,407)%j &
         +A(j,408)%j-A(j,412)%j+A(j,414)%j-A(j,416)%j-A(j,475)%j-A(j,476)%j+A(j,483)%j+A(j,488)%j+A(j,489)%j+A(j,490)%j+A(j,507)%j &
         +A(j,508)%j+A(j,510)%j)*f(1)
  M1( 89) = 2*(-A(j,2)%j+A(j,3)%j+A(j,8)%j-A(j,9)%j-A(j,49)%j+A(j,51)%j+A(j,52)%j-A(j,54)%j-A(j,65)%j+A(j,66)%j+A(j,71)%j &
         -A(j,72)%j+A(j,128)%j-A(j,129)%j+A(j,130)%j-A(j,132)%j-A(j,136)%j+A(j,138)%j+A(j,142)%j-A(j,144)%j-A(j,164)%j+A(j,165)%j &
         -A(j,181)%j+A(j,183)%j-A(j,184)%j+A(j,185)%j-A(j,194)%j+A(j,195)%j+A(j,221)%j-A(j,222)%j+A(j,244)%j-A(j,245)%j+A(j,247)%j &
         -A(j,249)%j+A(j,253)%j-A(j,255)%j-A(j,271)%j+A(j,272)%j-A(j,274)%j+A(j,276)%j-A(j,287)%j+A(j,288)%j+A(j,320)%j-A(j,321)%j &
         +A(j,340)%j-A(j,341)%j+A(j,346)%j-A(j,348)%j+A(j,349)%j-A(j,351)%j-A(j,398)%j+A(j,399)%j+A(j,401)%j-A(j,402)%j+A(j,427)%j &
         +A(j,428)%j+A(j,431)%j-A(j,432)%j+A(j,433)%j+A(j,439)%j+A(j,440)%j+A(j,443)%j+A(j,444)%j-A(j,447)%j-A(j,487)%j-A(j,488)%j &
         -A(j,492)%j-A(j,508)%j)*f(1)
  M1( 90) = 2*(-A(j,1)%j+A(j,2)%j+A(j,7)%j-A(j,8)%j-A(j,22)%j+A(j,24)%j+A(j,25)%j-A(j,27)%j+A(j,65)%j-A(j,66)%j-A(j,68)%j &
         +A(j,69)%j+A(j,98)%j-A(j,99)%j-A(j,100)%j+A(j,102)%j+A(j,103)%j-A(j,104)%j+A(j,106)%j-A(j,108)%j-A(j,116)%j+A(j,117)%j &
         +A(j,136)%j-A(j,138)%j-A(j,142)%j+A(j,144)%j+A(j,167)%j-A(j,168)%j-A(j,218)%j+A(j,219)%j-A(j,244)%j+A(j,246)%j-A(j,247)%j &
         +A(j,248)%j-A(j,253)%j+A(j,254)%j+A(j,271)%j-A(j,272)%j+A(j,274)%j-A(j,276)%j+A(j,287)%j-A(j,288)%j+A(j,323)%j-A(j,324)%j &
         +A(j,376)%j-A(j,377)%j+A(j,382)%j-A(j,384)%j+A(j,385)%j-A(j,387)%j-A(j,397)%j+A(j,398)%j+A(j,400)%j-A(j,401)%j+A(j,412)%j &
         -A(j,413)%j+A(j,416)%j+A(j,417)%j+A(j,418)%j-A(j,439)%j-A(j,440)%j-A(j,443)%j-A(j,444)%j+A(j,447)%j-A(j,489)%j-A(j,490)%j &
         -A(j,491)%j-A(j,510)%j)*f(1)
  M1( 91) = 2*(A(j,41)%j-A(j,42)%j-A(j,44)%j+A(j,45)%j+A(j,64)%j-A(j,66)%j-A(j,70)%j+A(j,72)%j+A(j,82)%j-A(j,83)%j-A(j,85)%j &
         +A(j,86)%j-A(j,133)%j+A(j,134)%j+A(j,136)%j-A(j,137)%j+A(j,148)%j-A(j,149)%j-A(j,163)%j+A(j,164)%j-A(j,206)%j+A(j,207)%j &
         +A(j,209)%j-A(j,210)%j-A(j,215)%j+A(j,216)%j-A(j,235)%j+A(j,236)%j+A(j,238)%j-A(j,240)%j-A(j,247)%j+A(j,249)%j+A(j,262)%j &
         -A(j,263)%j+A(j,268)%j-A(j,269)%j+A(j,277)%j-A(j,278)%j+A(j,281)%j-A(j,282)%j+A(j,286)%j-A(j,288)%j-A(j,301)%j+A(j,302)%j &
         +A(j,304)%j-A(j,305)%j-A(j,319)%j+A(j,321)%j-A(j,356)%j+A(j,357)%j-A(j,359)%j+A(j,360)%j+A(j,362)%j-A(j,363)%j-A(j,436)%j &
         +A(j,437)%j-A(j,440)%j+A(j,441)%j+A(j,450)%j+A(j,457)%j-A(j,458)%j+A(j,462)%j-A(j,475)%j+A(j,477)%j+A(j,484)%j+A(j,485)%j &
         +A(j,486)%j+A(j,488)%j)*f(1)
  M1( 92) = 2*(A(j,40)%j-A(j,41)%j-A(j,43)%j+A(j,44)%j+A(j,46)%j-A(j,48)%j-A(j,52)%j+A(j,54)%j+A(j,85)%j-A(j,86)%j-A(j,88)%j &
         +A(j,89)%j-A(j,121)%j+A(j,122)%j-A(j,127)%j+A(j,129)%j+A(j,154)%j-A(j,155)%j+A(j,163)%j-A(j,165)%j+A(j,188)%j-A(j,189)%j &
         -A(j,191)%j+A(j,192)%j+A(j,193)%j-A(j,195)%j-A(j,209)%j+A(j,210)%j+A(j,215)%j-A(j,216)%j-A(j,220)%j+A(j,222)%j+A(j,263)%j &
         -A(j,264)%j+A(j,269)%j-A(j,270)%j-A(j,295)%j+A(j,296)%j+A(j,301)%j-A(j,302)%j-A(j,304)%j+A(j,305)%j+A(j,325)%j-A(j,326)%j &
         +A(j,331)%j-A(j,333)%j-A(j,346)%j+A(j,348)%j-A(j,355)%j+A(j,356)%j-A(j,358)%j+A(j,359)%j-A(j,362)%j+A(j,363)%j+A(j,421)%j &
         +A(j,422)%j-A(j,430)%j-A(j,431)%j-A(j,435)%j-A(j,464)%j-A(j,465)%j-A(j,474)%j+A(j,475)%j-A(j,477)%j-A(j,484)%j-A(j,485)%j &
         -A(j,486)%j+A(j,487)%j)*f(1)
  M1( 93) = 2*(A(j,14)%j-A(j,15)%j-A(j,17)%j+A(j,18)%j-A(j,64)%j+A(j,66)%j+A(j,67)%j-A(j,69)%j-A(j,82)%j+A(j,83)%j+A(j,88)%j &
         -A(j,89)%j+A(j,133)%j-A(j,134)%j-A(j,136)%j+A(j,137)%j-A(j,152)%j+A(j,153)%j+A(j,155)%j-A(j,156)%j-A(j,161)%j+A(j,162)%j &
         +A(j,202)%j-A(j,203)%j-A(j,217)%j+A(j,218)%j+A(j,235)%j-A(j,237)%j-A(j,238)%j+A(j,239)%j+A(j,247)%j-A(j,248)%j-A(j,262)%j &
         +A(j,264)%j-A(j,268)%j+A(j,270)%j-A(j,277)%j+A(j,278)%j-A(j,281)%j+A(j,282)%j-A(j,286)%j+A(j,288)%j+A(j,292)%j-A(j,294)%j &
         +A(j,295)%j-A(j,296)%j-A(j,322)%j+A(j,324)%j+A(j,326)%j-A(j,327)%j-A(j,392)%j+A(j,393)%j-A(j,395)%j+A(j,396)%j+A(j,436)%j &
         -A(j,437)%j+A(j,440)%j-A(j,441)%j-A(j,450)%j+A(j,459)%j-A(j,460)%j+A(j,461)%j-A(j,463)%j+A(j,465)%j+A(j,471)%j+A(j,473)%j &
         +A(j,474)%j+A(j,490)%j)*f(1)
  M1( 94) = 2*(A(j,13)%j-A(j,14)%j-A(j,16)%j+A(j,17)%j+A(j,19)%j-A(j,21)%j-A(j,25)%j+A(j,27)%j+A(j,85)%j-A(j,86)%j-A(j,88)%j &
         +A(j,89)%j-A(j,91)%j+A(j,92)%j-A(j,97)%j+A(j,99)%j+A(j,110)%j-A(j,111)%j-A(j,113)%j+A(j,114)%j+A(j,115)%j-A(j,117)%j &
         -A(j,155)%j+A(j,156)%j+A(j,161)%j-A(j,162)%j-A(j,166)%j+A(j,168)%j+A(j,208)%j-A(j,209)%j+A(j,217)%j-A(j,219)%j+A(j,263)%j &
         -A(j,264)%j+A(j,269)%j-A(j,270)%j-A(j,292)%j+A(j,294)%j-A(j,295)%j+A(j,296)%j-A(j,304)%j+A(j,305)%j-A(j,326)%j+A(j,327)%j &
         +A(j,361)%j-A(j,362)%j+A(j,367)%j-A(j,369)%j-A(j,382)%j+A(j,384)%j-A(j,391)%j+A(j,392)%j-A(j,394)%j+A(j,395)%j+A(j,406)%j &
         +A(j,407)%j-A(j,415)%j-A(j,416)%j-A(j,420)%j+A(j,463)%j-A(j,465)%j-A(j,471)%j-A(j,473)%j-A(j,474)%j-A(j,476)%j-A(j,477)%j &
         -A(j,486)%j+A(j,489)%j)*f(1)
  M1( 95) = 2*(A(j,5)%j-A(j,6)%j-A(j,8)%j+A(j,9)%j-A(j,46)%j+A(j,48)%j+A(j,49)%j-A(j,51)%j-A(j,82)%j+A(j,83)%j+A(j,88)%j-A(j,89)%j &
         +A(j,121)%j-A(j,122)%j-A(j,131)%j+A(j,132)%j+A(j,137)%j-A(j,138)%j-A(j,143)%j+A(j,144)%j-A(j,154)%j+A(j,155)%j+A(j,184)%j &
         -A(j,185)%j+A(j,190)%j-A(j,192)%j-A(j,193)%j+A(j,194)%j+A(j,220)%j-A(j,221)%j-A(j,244)%j+A(j,245)%j-A(j,262)%j+A(j,264)%j &
         -A(j,268)%j+A(j,270)%j+A(j,274)%j-A(j,276)%j-A(j,277)%j+A(j,278)%j+A(j,280)%j-A(j,281)%j+A(j,295)%j-A(j,296)%j-A(j,325)%j &
         +A(j,326)%j-A(j,331)%j+A(j,333)%j-A(j,349)%j+A(j,351)%j-A(j,401)%j+A(j,402)%j-A(j,404)%j+A(j,405)%j-A(j,421)%j-A(j,422)%j &
         +A(j,432)%j-A(j,433)%j+A(j,434)%j-A(j,439)%j-A(j,441)%j+A(j,447)%j+A(j,449)%j-A(j,450)%j+A(j,464)%j+A(j,465)%j+A(j,474)%j &
         +A(j,492)%j)*f(1)
  M1( 96) = 2*(A(j,4)%j-A(j,5)%j-A(j,7)%j+A(j,8)%j-A(j,19)%j+A(j,21)%j+A(j,22)%j-A(j,24)%j+A(j,82)%j-A(j,83)%j-A(j,85)%j+A(j,86)%j &
         +A(j,91)%j-A(j,92)%j+A(j,101)%j-A(j,102)%j-A(j,106)%j+A(j,108)%j-A(j,112)%j+A(j,113)%j-A(j,115)%j+A(j,116)%j-A(j,137)%j &
         +A(j,138)%j+A(j,143)%j-A(j,144)%j+A(j,166)%j-A(j,167)%j-A(j,208)%j+A(j,209)%j+A(j,244)%j-A(j,246)%j+A(j,262)%j-A(j,263)%j &
         +A(j,268)%j-A(j,269)%j-A(j,274)%j+A(j,276)%j+A(j,277)%j-A(j,278)%j-A(j,280)%j+A(j,281)%j+A(j,304)%j-A(j,305)%j-A(j,361)%j &
         +A(j,362)%j-A(j,367)%j+A(j,369)%j-A(j,385)%j+A(j,387)%j-A(j,400)%j+A(j,401)%j-A(j,403)%j+A(j,404)%j-A(j,406)%j-A(j,407)%j &
         -A(j,417)%j-A(j,418)%j-A(j,419)%j+A(j,439)%j+A(j,441)%j-A(j,447)%j-A(j,449)%j+A(j,450)%j+A(j,476)%j+A(j,477)%j+A(j,486)%j &
         +A(j,491)%j)*f(1)
  M1( 97) = 2*(A(j,29)%j-A(j,30)%j-A(j,32)%j+A(j,33)%j-A(j,65)%j+A(j,66)%j+A(j,71)%j-A(j,72)%j-A(j,82)%j+A(j,84)%j+A(j,85)%j &
         -A(j,87)%j-A(j,136)%j+A(j,137)%j+A(j,139)%j-A(j,140)%j+A(j,163)%j-A(j,164)%j-A(j,175)%j+A(j,176)%j-A(j,178)%j+A(j,179)%j &
         -A(j,209)%j+A(j,210)%j+A(j,212)%j-A(j,213)%j+A(j,247)%j-A(j,249)%j-A(j,250)%j+A(j,252)%j+A(j,253)%j-A(j,255)%j-A(j,262)%j &
         +A(j,263)%j+A(j,265)%j-A(j,266)%j+A(j,272)%j-A(j,273)%j-A(j,277)%j+A(j,279)%j-A(j,287)%j+A(j,288)%j+A(j,301)%j-A(j,303)%j &
         -A(j,304)%j+A(j,306)%j+A(j,320)%j-A(j,321)%j-A(j,341)%j+A(j,342)%j+A(j,365)%j-A(j,366)%j+A(j,380)%j-A(j,381)%j+A(j,440)%j &
         -A(j,441)%j-A(j,442)%j+A(j,444)%j+A(j,446)%j+A(j,475)%j-A(j,477)%j-A(j,481)%j+A(j,482)%j-A(j,483)%j-A(j,488)%j-A(j,505)%j &
         -A(j,507)%j-A(j,508)%j)*f(1)
  M1( 98) = 2*(A(j,32)%j-A(j,33)%j-A(j,35)%j+A(j,36)%j-A(j,64)%j+A(j,65)%j+A(j,70)%j-A(j,71)%j-A(j,73)%j+A(j,75)%j+A(j,76)%j &
         -A(j,78)%j+A(j,133)%j-A(j,135)%j-A(j,139)%j+A(j,141)%j+A(j,149)%j-A(j,150)%j-A(j,176)%j+A(j,177)%j-A(j,179)%j+A(j,180)%j &
         -A(j,200)%j+A(j,201)%j+A(j,224)%j-A(j,225)%j-A(j,238)%j+A(j,240)%j-A(j,241)%j+A(j,242)%j+A(j,250)%j-A(j,252)%j-A(j,253)%j &
         +A(j,255)%j+A(j,256)%j-A(j,257)%j-A(j,272)%j+A(j,273)%j-A(j,283)%j+A(j,285)%j-A(j,286)%j+A(j,287)%j+A(j,313)%j-A(j,315)%j &
         -A(j,316)%j+A(j,318)%j+A(j,319)%j-A(j,320)%j+A(j,341)%j-A(j,342)%j-A(j,374)%j+A(j,375)%j-A(j,380)%j+A(j,381)%j-A(j,437)%j &
         -A(j,438)%j+A(j,442)%j-A(j,444)%j+A(j,445)%j+A(j,454)%j-A(j,456)%j+A(j,458)%j+A(j,499)%j+A(j,500)%j+A(j,501)%j+A(j,505)%j &
         +A(j,507)%j+A(j,508)%j)*f(1)
  M1( 99) = 2*(-A(j,29)%j+A(j,30)%j+A(j,35)%j-A(j,36)%j-A(j,47)%j+A(j,48)%j+A(j,53)%j-A(j,54)%j-A(j,85)%j+A(j,87)%j+A(j,88)%j &
         -A(j,90)%j-A(j,154)%j+A(j,155)%j+A(j,157)%j-A(j,158)%j-A(j,163)%j+A(j,165)%j+A(j,175)%j-A(j,177)%j+A(j,178)%j-A(j,180)%j &
         +A(j,209)%j-A(j,210)%j-A(j,212)%j+A(j,213)%j+A(j,220)%j-A(j,222)%j-A(j,223)%j+A(j,225)%j+A(j,226)%j-A(j,228)%j-A(j,263)%j &
         +A(j,264)%j+A(j,266)%j-A(j,267)%j+A(j,290)%j-A(j,291)%j+A(j,295)%j-A(j,297)%j-A(j,301)%j+A(j,303)%j+A(j,304)%j-A(j,306)%j &
         -A(j,314)%j+A(j,315)%j-A(j,332)%j+A(j,333)%j+A(j,347)%j-A(j,348)%j-A(j,365)%j+A(j,366)%j+A(j,374)%j-A(j,375)%j+A(j,464)%j &
         +A(j,465)%j-A(j,466)%j+A(j,468)%j-A(j,470)%j-A(j,475)%j+A(j,477)%j+A(j,481)%j-A(j,482)%j+A(j,483)%j-A(j,487)%j-A(j,499)%j &
         -A(j,501)%j-A(j,502)%j)*f(1)
  M1(100) = 2*(A(j,32)%j-A(j,33)%j-A(j,35)%j+A(j,36)%j-A(j,46)%j+A(j,47)%j+A(j,52)%j-A(j,53)%j-A(j,55)%j+A(j,57)%j+A(j,58)%j &
         -A(j,60)%j+A(j,121)%j-A(j,123)%j+A(j,128)%j-A(j,129)%j-A(j,157)%j+A(j,159)%j-A(j,176)%j+A(j,177)%j-A(j,179)%j+A(j,180)%j &
         -A(j,182)%j+A(j,183)%j-A(j,193)%j+A(j,195)%j-A(j,196)%j+A(j,197)%j+A(j,223)%j-A(j,225)%j-A(j,226)%j+A(j,228)%j+A(j,229)%j &
         -A(j,230)%j+A(j,251)%j-A(j,252)%j-A(j,290)%j+A(j,291)%j+A(j,314)%j-A(j,315)%j-A(j,328)%j+A(j,330)%j-A(j,331)%j+A(j,332)%j &
         +A(j,340)%j-A(j,342)%j-A(j,343)%j+A(j,345)%j+A(j,346)%j-A(j,347)%j-A(j,374)%j+A(j,375)%j-A(j,380)%j+A(j,381)%j-A(j,422)%j &
         -A(j,423)%j+A(j,427)%j-A(j,429)%j+A(j,431)%j+A(j,466)%j-A(j,468)%j+A(j,469)%j+A(j,499)%j+A(j,501)%j+A(j,502)%j+A(j,505)%j &
         +A(j,506)%j+A(j,507)%j)*f(1)
  M1(101) = 2*(-A(j,29)%j+A(j,30)%j+A(j,35)%j-A(j,36)%j-A(j,38)%j+A(j,39)%j+A(j,44)%j-A(j,45)%j-A(j,76)%j+A(j,78)%j+A(j,79)%j &
         -A(j,81)%j-A(j,145)%j+A(j,146)%j-A(j,148)%j+A(j,150)%j+A(j,169)%j-A(j,170)%j+A(j,175)%j-A(j,177)%j+A(j,178)%j-A(j,180)%j &
         +A(j,200)%j-A(j,201)%j+A(j,205)%j-A(j,207)%j-A(j,211)%j+A(j,213)%j+A(j,214)%j-A(j,216)%j-A(j,224)%j+A(j,225)%j-A(j,242)%j &
         +A(j,243)%j+A(j,257)%j-A(j,258)%j+A(j,299)%j-A(j,300)%j-A(j,302)%j+A(j,303)%j+A(j,310)%j-A(j,312)%j-A(j,313)%j+A(j,315)%j &
         +A(j,316)%j-A(j,318)%j-A(j,353)%j+A(j,354)%j+A(j,356)%j-A(j,357)%j-A(j,365)%j+A(j,366)%j+A(j,374)%j-A(j,375)%j+A(j,452)%j &
         +A(j,453)%j-A(j,454)%j+A(j,456)%j-A(j,457)%j-A(j,478)%j+A(j,480)%j+A(j,481)%j+A(j,483)%j-A(j,484)%j-A(j,494)%j-A(j,499)%j &
         -A(j,500)%j-A(j,501)%j)*f(1)
  M1(102) = 2*(A(j,29)%j-A(j,30)%j-A(j,32)%j+A(j,33)%j-A(j,37)%j+A(j,38)%j+A(j,43)%j-A(j,44)%j-A(j,58)%j+A(j,60)%j+A(j,61)%j &
         -A(j,63)%j+A(j,124)%j-A(j,126)%j+A(j,127)%j-A(j,128)%j-A(j,169)%j+A(j,171)%j-A(j,175)%j+A(j,176)%j-A(j,178)%j+A(j,179)%j &
         +A(j,182)%j-A(j,183)%j-A(j,187)%j+A(j,189)%j-A(j,197)%j+A(j,198)%j+A(j,211)%j-A(j,213)%j-A(j,214)%j+A(j,216)%j+A(j,230)%j &
         -A(j,231)%j-A(j,251)%j+A(j,252)%j-A(j,299)%j+A(j,300)%j+A(j,302)%j-A(j,303)%j+A(j,337)%j-A(j,339)%j-A(j,340)%j+A(j,342)%j &
         +A(j,343)%j-A(j,345)%j-A(j,352)%j+A(j,353)%j+A(j,355)%j-A(j,356)%j+A(j,365)%j-A(j,366)%j+A(j,380)%j-A(j,381)%j-A(j,425)%j &
         +A(j,426)%j-A(j,427)%j+A(j,429)%j+A(j,430)%j+A(j,478)%j-A(j,480)%j-A(j,481)%j-A(j,483)%j+A(j,484)%j-A(j,493)%j-A(j,505)%j &
         -A(j,506)%j-A(j,507)%j)*f(1)
  M1(103) = 2*(A(j,56)%j-A(j,57)%j-A(j,59)%j+A(j,60)%j+A(j,65)%j-A(j,66)%j-A(j,68)%j+A(j,69)%j+A(j,82)%j-A(j,84)%j-A(j,88)%j &
         +A(j,90)%j+A(j,136)%j-A(j,137)%j-A(j,139)%j+A(j,140)%j-A(j,155)%j+A(j,156)%j+A(j,158)%j-A(j,159)%j+A(j,217)%j-A(j,218)%j &
         -A(j,229)%j+A(j,230)%j-A(j,232)%j+A(j,233)%j-A(j,247)%j+A(j,248)%j+A(j,250)%j-A(j,251)%j-A(j,253)%j+A(j,254)%j+A(j,262)%j &
         -A(j,264)%j-A(j,265)%j+A(j,267)%j-A(j,272)%j+A(j,273)%j+A(j,277)%j-A(j,279)%j+A(j,287)%j-A(j,288)%j-A(j,293)%j+A(j,294)%j &
         -A(j,295)%j+A(j,297)%j+A(j,323)%j-A(j,324)%j+A(j,329)%j-A(j,330)%j+A(j,344)%j-A(j,345)%j-A(j,377)%j+A(j,378)%j-A(j,440)%j &
         +A(j,441)%j+A(j,442)%j-A(j,444)%j-A(j,446)%j+A(j,463)%j-A(j,465)%j-A(j,469)%j+A(j,470)%j-A(j,472)%j-A(j,490)%j-A(j,506)%j &
         -A(j,509)%j-A(j,510)%j)*f(1)
  M1(104) = 2*(A(j,59)%j-A(j,60)%j-A(j,62)%j+A(j,63)%j+A(j,64)%j-A(j,65)%j-A(j,67)%j+A(j,68)%j+A(j,73)%j-A(j,75)%j-A(j,79)%j &
         +A(j,81)%j-A(j,133)%j+A(j,135)%j+A(j,139)%j-A(j,141)%j-A(j,146)%j+A(j,147)%j+A(j,170)%j-A(j,171)%j+A(j,203)%j-A(j,204)%j &
         -A(j,230)%j+A(j,231)%j-A(j,233)%j+A(j,234)%j+A(j,238)%j-A(j,239)%j+A(j,241)%j-A(j,243)%j-A(j,250)%j+A(j,251)%j+A(j,253)%j &
         -A(j,254)%j-A(j,256)%j+A(j,258)%j+A(j,272)%j-A(j,273)%j+A(j,283)%j-A(j,285)%j+A(j,286)%j-A(j,287)%j-A(j,307)%j+A(j,308)%j &
         -A(j,310)%j+A(j,312)%j+A(j,322)%j-A(j,323)%j-A(j,338)%j+A(j,339)%j-A(j,344)%j+A(j,345)%j+A(j,377)%j-A(j,378)%j+A(j,437)%j &
         +A(j,438)%j-A(j,442)%j+A(j,444)%j-A(j,445)%j-A(j,451)%j-A(j,453)%j+A(j,460)%j+A(j,493)%j+A(j,494)%j+A(j,496)%j+A(j,506)%j &
         +A(j,509)%j+A(j,510)%j)*f(1)
  M1(105) = 2*(-A(j,20)%j+A(j,21)%j+A(j,26)%j-A(j,27)%j-A(j,56)%j+A(j,57)%j+A(j,62)%j-A(j,63)%j-A(j,85)%j+A(j,87)%j+A(j,88)%j &
         -A(j,90)%j+A(j,155)%j-A(j,156)%j-A(j,158)%j+A(j,159)%j+A(j,166)%j-A(j,168)%j-A(j,169)%j+A(j,171)%j+A(j,172)%j-A(j,174)%j &
         -A(j,208)%j+A(j,209)%j+A(j,211)%j-A(j,212)%j-A(j,217)%j+A(j,219)%j+A(j,229)%j-A(j,231)%j+A(j,232)%j-A(j,234)%j-A(j,263)%j &
         +A(j,264)%j+A(j,266)%j-A(j,267)%j+A(j,293)%j-A(j,294)%j+A(j,295)%j-A(j,297)%j-A(j,298)%j+A(j,300)%j+A(j,304)%j-A(j,306)%j &
         -A(j,308)%j+A(j,309)%j-A(j,329)%j+A(j,330)%j+A(j,338)%j-A(j,339)%j-A(j,368)%j+A(j,369)%j+A(j,383)%j-A(j,384)%j-A(j,463)%j &
         +A(j,465)%j+A(j,469)%j-A(j,470)%j+A(j,472)%j+A(j,476)%j+A(j,477)%j+A(j,478)%j+A(j,479)%j-A(j,482)%j-A(j,489)%j-A(j,493)%j &
         -A(j,495)%j-A(j,496)%j)*f(1)
  M1(106) = 2*(-A(j,19)%j+A(j,20)%j+A(j,25)%j-A(j,26)%j-A(j,28)%j+A(j,30)%j+A(j,31)%j-A(j,33)%j+A(j,59)%j-A(j,60)%j-A(j,62)%j &
         +A(j,63)%j+A(j,91)%j-A(j,93)%j+A(j,98)%j-A(j,99)%j-A(j,104)%j+A(j,105)%j-A(j,115)%j+A(j,117)%j-A(j,118)%j+A(j,119)%j &
         +A(j,169)%j-A(j,171)%j-A(j,172)%j+A(j,174)%j+A(j,175)%j-A(j,176)%j-A(j,211)%j+A(j,213)%j-A(j,230)%j+A(j,231)%j-A(j,233)%j &
         +A(j,234)%j+A(j,251)%j-A(j,252)%j+A(j,298)%j-A(j,300)%j+A(j,308)%j-A(j,309)%j-A(j,338)%j+A(j,339)%j-A(j,344)%j+A(j,345)%j &
         -A(j,364)%j+A(j,366)%j-A(j,367)%j+A(j,368)%j+A(j,376)%j-A(j,378)%j-A(j,379)%j+A(j,381)%j+A(j,382)%j-A(j,383)%j-A(j,407)%j &
         -A(j,408)%j+A(j,412)%j-A(j,414)%j+A(j,416)%j-A(j,478)%j-A(j,479)%j+A(j,481)%j+A(j,493)%j+A(j,495)%j+A(j,496)%j+A(j,505)%j &
         +A(j,506)%j+A(j,509)%j)*f(1)
  M1(107) = 2*(-A(j,11)%j+A(j,12)%j+A(j,17)%j-A(j,18)%j-A(j,56)%j+A(j,57)%j+A(j,62)%j-A(j,63)%j-A(j,76)%j+A(j,78)%j+A(j,79)%j &
         -A(j,81)%j+A(j,146)%j-A(j,147)%j+A(j,151)%j-A(j,153)%j-A(j,157)%j+A(j,159)%j+A(j,160)%j-A(j,162)%j-A(j,170)%j+A(j,171)%j &
         -A(j,199)%j+A(j,200)%j-A(j,202)%j+A(j,204)%j+A(j,223)%j-A(j,224)%j+A(j,229)%j-A(j,231)%j+A(j,232)%j-A(j,234)%j-A(j,242)%j &
         +A(j,243)%j+A(j,257)%j-A(j,258)%j-A(j,289)%j+A(j,291)%j-A(j,292)%j+A(j,293)%j+A(j,307)%j-A(j,308)%j+A(j,310)%j-A(j,312)%j &
         +A(j,316)%j-A(j,318)%j-A(j,329)%j+A(j,330)%j+A(j,338)%j-A(j,339)%j-A(j,389)%j+A(j,390)%j+A(j,392)%j-A(j,393)%j+A(j,451)%j &
         +A(j,453)%j+A(j,455)%j+A(j,456)%j-A(j,459)%j+A(j,466)%j+A(j,467)%j+A(j,469)%j-A(j,471)%j+A(j,472)%j-A(j,493)%j-A(j,494)%j &
         -A(j,496)%j-A(j,500)%j)*f(1)
  M1(108) = 2*(-A(j,10)%j+A(j,11)%j+A(j,16)%j-A(j,17)%j-A(j,31)%j+A(j,33)%j+A(j,34)%j-A(j,36)%j+A(j,56)%j-A(j,57)%j-A(j,59)%j &
         +A(j,60)%j+A(j,94)%j-A(j,96)%j+A(j,97)%j-A(j,98)%j+A(j,104)%j-A(j,105)%j-A(j,109)%j+A(j,111)%j-A(j,119)%j+A(j,120)%j &
         +A(j,157)%j-A(j,159)%j-A(j,160)%j+A(j,162)%j+A(j,176)%j-A(j,177)%j-A(j,223)%j+A(j,225)%j-A(j,229)%j+A(j,230)%j-A(j,232)%j &
         +A(j,233)%j-A(j,251)%j+A(j,252)%j+A(j,289)%j-A(j,291)%j+A(j,292)%j-A(j,293)%j+A(j,329)%j-A(j,330)%j+A(j,344)%j-A(j,345)%j &
         +A(j,373)%j-A(j,375)%j-A(j,376)%j+A(j,378)%j+A(j,379)%j-A(j,381)%j-A(j,388)%j+A(j,389)%j+A(j,391)%j-A(j,392)%j-A(j,410)%j &
         +A(j,411)%j-A(j,412)%j+A(j,414)%j+A(j,415)%j-A(j,466)%j-A(j,467)%j-A(j,469)%j+A(j,471)%j-A(j,472)%j-A(j,499)%j-A(j,505)%j &
         -A(j,506)%j-A(j,509)%j)*f(1)
  M1(109) = 2*(A(j,47)%j-A(j,48)%j-A(j,50)%j+A(j,51)%j+A(j,74)%j-A(j,75)%j-A(j,77)%j+A(j,78)%j+A(j,82)%j-A(j,84)%j-A(j,88)%j &
         +A(j,90)%j-A(j,137)%j+A(j,138)%j+A(j,140)%j-A(j,141)%j+A(j,154)%j-A(j,155)%j-A(j,157)%j+A(j,158)%j-A(j,220)%j+A(j,221)%j &
         +A(j,223)%j-A(j,224)%j-A(j,226)%j+A(j,227)%j+A(j,244)%j-A(j,245)%j-A(j,256)%j+A(j,257)%j-A(j,259)%j+A(j,260)%j+A(j,262)%j &
         -A(j,264)%j-A(j,265)%j+A(j,267)%j-A(j,275)%j+A(j,276)%j+A(j,277)%j-A(j,279)%j+A(j,284)%j-A(j,285)%j-A(j,290)%j+A(j,291)%j &
         -A(j,295)%j+A(j,297)%j+A(j,317)%j-A(j,318)%j+A(j,332)%j-A(j,333)%j+A(j,350)%j-A(j,351)%j-A(j,371)%j+A(j,372)%j+A(j,439)%j &
         +A(j,441)%j-A(j,445)%j-A(j,446)%j-A(j,448)%j-A(j,464)%j-A(j,465)%j+A(j,466)%j-A(j,468)%j+A(j,470)%j-A(j,492)%j-A(j,500)%j &
         -A(j,503)%j-A(j,504)%j)*f(1)
  M1(110) = 2*(A(j,46)%j-A(j,47)%j-A(j,49)%j+A(j,50)%j+A(j,55)%j-A(j,57)%j-A(j,61)%j+A(j,63)%j+A(j,77)%j-A(j,78)%j-A(j,80)%j &
         +A(j,81)%j-A(j,121)%j+A(j,123)%j-A(j,125)%j+A(j,126)%j+A(j,157)%j-A(j,159)%j+A(j,170)%j-A(j,171)%j+A(j,185)%j-A(j,186)%j &
         +A(j,193)%j-A(j,194)%j+A(j,196)%j-A(j,198)%j-A(j,223)%j+A(j,224)%j+A(j,226)%j-A(j,227)%j-A(j,229)%j+A(j,231)%j-A(j,257)%j &
         +A(j,258)%j-A(j,260)%j+A(j,261)%j+A(j,290)%j-A(j,291)%j-A(j,311)%j+A(j,312)%j-A(j,317)%j+A(j,318)%j+A(j,328)%j-A(j,330)%j &
         +A(j,331)%j-A(j,332)%j-A(j,334)%j+A(j,335)%j-A(j,337)%j+A(j,339)%j+A(j,349)%j-A(j,350)%j+A(j,371)%j-A(j,372)%j+A(j,422)%j &
         +A(j,423)%j-A(j,424)%j-A(j,426)%j+A(j,433)%j-A(j,466)%j+A(j,468)%j-A(j,469)%j+A(j,493)%j+A(j,494)%j+A(j,498)%j+A(j,500)%j &
         +A(j,503)%j+A(j,504)%j)*f(1)
  M1(111) = 2*(A(j,20)%j-A(j,21)%j-A(j,23)%j+A(j,24)%j-A(j,74)%j+A(j,75)%j+A(j,80)%j-A(j,81)%j-A(j,82)%j+A(j,84)%j+A(j,85)%j &
         -A(j,87)%j+A(j,137)%j-A(j,138)%j-A(j,140)%j+A(j,141)%j-A(j,166)%j+A(j,167)%j+A(j,169)%j-A(j,170)%j-A(j,172)%j+A(j,173)%j &
         +A(j,208)%j-A(j,209)%j-A(j,211)%j+A(j,212)%j-A(j,244)%j+A(j,246)%j+A(j,256)%j-A(j,258)%j+A(j,259)%j-A(j,261)%j-A(j,262)%j &
         +A(j,263)%j+A(j,265)%j-A(j,266)%j+A(j,275)%j-A(j,276)%j-A(j,277)%j+A(j,279)%j-A(j,284)%j+A(j,285)%j+A(j,298)%j-A(j,300)%j &
         -A(j,304)%j+A(j,306)%j+A(j,311)%j-A(j,312)%j-A(j,335)%j+A(j,336)%j+A(j,368)%j-A(j,369)%j+A(j,386)%j-A(j,387)%j-A(j,439)%j &
         -A(j,441)%j+A(j,445)%j+A(j,446)%j+A(j,448)%j-A(j,476)%j-A(j,477)%j-A(j,478)%j-A(j,479)%j+A(j,482)%j-A(j,491)%j-A(j,494)%j &
         -A(j,497)%j-A(j,498)%j)*f(1)
  M1(112) = 2*(A(j,19)%j-A(j,20)%j-A(j,22)%j+A(j,23)%j+A(j,28)%j-A(j,30)%j-A(j,34)%j+A(j,36)%j+A(j,77)%j-A(j,78)%j-A(j,80)%j &
         +A(j,81)%j-A(j,91)%j+A(j,93)%j-A(j,95)%j+A(j,96)%j+A(j,107)%j-A(j,108)%j+A(j,115)%j-A(j,116)%j+A(j,118)%j-A(j,120)%j &
         -A(j,169)%j+A(j,170)%j+A(j,172)%j-A(j,173)%j-A(j,175)%j+A(j,177)%j+A(j,211)%j-A(j,213)%j+A(j,224)%j-A(j,225)%j-A(j,257)%j &
         +A(j,258)%j-A(j,260)%j+A(j,261)%j-A(j,298)%j+A(j,300)%j-A(j,311)%j+A(j,312)%j-A(j,317)%j+A(j,318)%j+A(j,335)%j-A(j,336)%j &
         +A(j,364)%j-A(j,366)%j+A(j,367)%j-A(j,368)%j-A(j,370)%j+A(j,371)%j-A(j,373)%j+A(j,375)%j+A(j,385)%j-A(j,386)%j+A(j,407)%j &
         +A(j,408)%j-A(j,409)%j-A(j,411)%j+A(j,418)%j+A(j,478)%j+A(j,479)%j-A(j,481)%j+A(j,494)%j+A(j,497)%j+A(j,498)%j+A(j,499)%j &
         +A(j,500)%j+A(j,504)%j)*f(1)
  M1(113) = 2*(-A(j,2)%j+A(j,3)%j+A(j,8)%j-A(j,9)%j-A(j,58)%j+A(j,60)%j+A(j,61)%j-A(j,63)%j-A(j,74)%j+A(j,75)%j+A(j,80)%j &
         -A(j,81)%j+A(j,125)%j-A(j,126)%j+A(j,130)%j-A(j,132)%j-A(j,139)%j+A(j,141)%j+A(j,142)%j-A(j,144)%j-A(j,170)%j+A(j,171)%j &
         -A(j,181)%j+A(j,182)%j-A(j,184)%j+A(j,186)%j-A(j,197)%j+A(j,198)%j+A(j,230)%j-A(j,231)%j+A(j,250)%j-A(j,251)%j+A(j,256)%j &
         -A(j,258)%j+A(j,259)%j-A(j,261)%j-A(j,271)%j+A(j,273)%j-A(j,274)%j+A(j,275)%j-A(j,284)%j+A(j,285)%j+A(j,311)%j-A(j,312)%j &
         +A(j,334)%j-A(j,335)%j+A(j,337)%j-A(j,339)%j+A(j,343)%j-A(j,345)%j-A(j,398)%j+A(j,399)%j+A(j,401)%j-A(j,402)%j+A(j,424)%j &
         +A(j,426)%j+A(j,428)%j+A(j,429)%j-A(j,432)%j+A(j,442)%j+A(j,443)%j+A(j,445)%j-A(j,447)%j+A(j,448)%j-A(j,493)%j-A(j,494)%j &
         -A(j,498)%j-A(j,506)%j)*f(1)
  M1(114) = 2*(-A(j,1)%j+A(j,2)%j+A(j,7)%j-A(j,8)%j-A(j,31)%j+A(j,33)%j+A(j,34)%j-A(j,36)%j+A(j,74)%j-A(j,75)%j-A(j,77)%j &
         +A(j,78)%j+A(j,95)%j-A(j,96)%j-A(j,100)%j+A(j,102)%j+A(j,103)%j-A(j,105)%j+A(j,106)%j-A(j,107)%j-A(j,119)%j+A(j,120)%j &
         +A(j,139)%j-A(j,141)%j-A(j,142)%j+A(j,144)%j+A(j,176)%j-A(j,177)%j-A(j,224)%j+A(j,225)%j-A(j,250)%j+A(j,252)%j-A(j,256)%j &
         +A(j,257)%j-A(j,259)%j+A(j,260)%j+A(j,271)%j-A(j,273)%j+A(j,274)%j-A(j,275)%j+A(j,284)%j-A(j,285)%j+A(j,317)%j-A(j,318)%j &
         +A(j,370)%j-A(j,371)%j+A(j,373)%j-A(j,375)%j+A(j,379)%j-A(j,381)%j-A(j,397)%j+A(j,398)%j+A(j,400)%j-A(j,401)%j+A(j,409)%j &
         +A(j,411)%j-A(j,413)%j+A(j,414)%j+A(j,417)%j-A(j,442)%j-A(j,443)%j-A(j,445)%j+A(j,447)%j-A(j,448)%j-A(j,499)%j-A(j,500)%j &
         -A(j,504)%j-A(j,505)%j)*f(1)
  M1(115) = 2*(A(j,38)%j-A(j,39)%j-A(j,41)%j+A(j,42)%j+A(j,73)%j-A(j,75)%j-A(j,79)%j+A(j,81)%j+A(j,83)%j-A(j,84)%j-A(j,86)%j &
         +A(j,87)%j-A(j,134)%j+A(j,135)%j+A(j,140)%j-A(j,141)%j+A(j,145)%j-A(j,146)%j-A(j,169)%j+A(j,170)%j-A(j,205)%j+A(j,206)%j &
         +A(j,211)%j-A(j,212)%j-A(j,214)%j+A(j,215)%j+A(j,235)%j-A(j,236)%j+A(j,241)%j-A(j,243)%j-A(j,256)%j+A(j,258)%j-A(j,265)%j &
         +A(j,266)%j-A(j,268)%j+A(j,269)%j+A(j,278)%j-A(j,279)%j-A(j,281)%j+A(j,282)%j+A(j,283)%j-A(j,285)%j-A(j,299)%j+A(j,300)%j &
         +A(j,305)%j-A(j,306)%j-A(j,310)%j+A(j,312)%j+A(j,353)%j-A(j,354)%j+A(j,359)%j-A(j,360)%j-A(j,362)%j+A(j,363)%j+A(j,436)%j &
         +A(j,438)%j-A(j,445)%j-A(j,446)%j-A(j,450)%j-A(j,452)%j-A(j,453)%j-A(j,462)%j+A(j,478)%j-A(j,480)%j-A(j,482)%j-A(j,485)%j &
         -A(j,486)%j+A(j,494)%j)*f(1)
  M1(116) = 2*(A(j,37)%j-A(j,38)%j-A(j,40)%j+A(j,41)%j+A(j,55)%j-A(j,57)%j-A(j,61)%j+A(j,63)%j+A(j,86)%j-A(j,87)%j-A(j,89)%j &
         +A(j,90)%j-A(j,122)%j+A(j,123)%j-A(j,124)%j+A(j,126)%j+A(j,158)%j-A(j,159)%j+A(j,169)%j-A(j,171)%j+A(j,187)%j-A(j,188)%j &
         +A(j,191)%j-A(j,192)%j+A(j,196)%j-A(j,198)%j-A(j,211)%j+A(j,212)%j+A(j,214)%j-A(j,215)%j-A(j,229)%j+A(j,231)%j-A(j,266)%j &
         +A(j,267)%j-A(j,269)%j+A(j,270)%j-A(j,296)%j+A(j,297)%j+A(j,299)%j-A(j,300)%j-A(j,305)%j+A(j,306)%j-A(j,325)%j+A(j,326)%j &
         +A(j,328)%j-A(j,330)%j-A(j,337)%j+A(j,339)%j+A(j,352)%j-A(j,353)%j+A(j,358)%j-A(j,359)%j+A(j,362)%j-A(j,363)%j-A(j,421)%j &
         +A(j,423)%j+A(j,425)%j-A(j,426)%j+A(j,435)%j-A(j,469)%j+A(j,470)%j+A(j,474)%j-A(j,478)%j+A(j,480)%j+A(j,482)%j+A(j,485)%j &
         +A(j,486)%j+A(j,493)%j)*f(1)
  M1(117) = 2*(A(j,11)%j-A(j,12)%j-A(j,14)%j+A(j,15)%j-A(j,73)%j+A(j,75)%j+A(j,76)%j-A(j,78)%j-A(j,83)%j+A(j,84)%j+A(j,89)%j &
         -A(j,90)%j+A(j,134)%j-A(j,135)%j-A(j,140)%j+A(j,141)%j-A(j,151)%j+A(j,152)%j+A(j,157)%j-A(j,158)%j-A(j,160)%j+A(j,161)%j &
         +A(j,199)%j-A(j,200)%j-A(j,223)%j+A(j,224)%j-A(j,235)%j+A(j,237)%j-A(j,241)%j+A(j,242)%j+A(j,256)%j-A(j,257)%j+A(j,265)%j &
         -A(j,267)%j+A(j,268)%j-A(j,270)%j-A(j,278)%j+A(j,279)%j+A(j,281)%j-A(j,282)%j-A(j,283)%j+A(j,285)%j+A(j,289)%j-A(j,291)%j &
         +A(j,296)%j-A(j,297)%j-A(j,316)%j+A(j,318)%j-A(j,326)%j+A(j,327)%j+A(j,389)%j-A(j,390)%j+A(j,395)%j-A(j,396)%j-A(j,436)%j &
         -A(j,438)%j+A(j,445)%j+A(j,446)%j+A(j,450)%j-A(j,455)%j-A(j,456)%j-A(j,461)%j-A(j,466)%j-A(j,467)%j-A(j,470)%j-A(j,473)%j &
         -A(j,474)%j+A(j,500)%j)*f(1)
  M1(118) = 2*(A(j,10)%j-A(j,11)%j-A(j,13)%j+A(j,14)%j+A(j,28)%j-A(j,30)%j-A(j,34)%j+A(j,36)%j+A(j,86)%j-A(j,87)%j-A(j,89)%j &
         +A(j,90)%j-A(j,92)%j+A(j,93)%j-A(j,94)%j+A(j,96)%j+A(j,109)%j-A(j,110)%j+A(j,113)%j-A(j,114)%j+A(j,118)%j-A(j,120)%j &
         -A(j,157)%j+A(j,158)%j+A(j,160)%j-A(j,161)%j-A(j,175)%j+A(j,177)%j+A(j,212)%j-A(j,213)%j+A(j,223)%j-A(j,225)%j-A(j,266)%j &
         +A(j,267)%j-A(j,269)%j+A(j,270)%j-A(j,289)%j+A(j,291)%j-A(j,296)%j+A(j,297)%j-A(j,305)%j+A(j,306)%j+A(j,326)%j-A(j,327)%j &
         -A(j,361)%j+A(j,362)%j+A(j,364)%j-A(j,366)%j-A(j,373)%j+A(j,375)%j+A(j,388)%j-A(j,389)%j+A(j,394)%j-A(j,395)%j-A(j,406)%j &
         +A(j,408)%j+A(j,410)%j-A(j,411)%j+A(j,420)%j+A(j,466)%j+A(j,467)%j+A(j,470)%j+A(j,473)%j+A(j,474)%j-A(j,481)%j+A(j,482)%j &
         +A(j,486)%j+A(j,499)%j)*f(1)
  M1(119) = 2*(A(j,2)%j-A(j,3)%j-A(j,5)%j+A(j,6)%j-A(j,55)%j+A(j,57)%j+A(j,58)%j-A(j,60)%j-A(j,83)%j+A(j,84)%j+A(j,89)%j-A(j,90)%j &
         +A(j,122)%j-A(j,123)%j-A(j,130)%j+A(j,131)%j+A(j,139)%j-A(j,140)%j-A(j,142)%j+A(j,143)%j-A(j,158)%j+A(j,159)%j+A(j,181)%j &
         -A(j,182)%j-A(j,190)%j+A(j,192)%j-A(j,196)%j+A(j,197)%j+A(j,229)%j-A(j,230)%j-A(j,250)%j+A(j,251)%j+A(j,265)%j-A(j,267)%j &
         +A(j,268)%j-A(j,270)%j+A(j,271)%j-A(j,273)%j-A(j,278)%j+A(j,279)%j-A(j,280)%j+A(j,281)%j+A(j,296)%j-A(j,297)%j+A(j,325)%j &
         -A(j,326)%j-A(j,328)%j+A(j,330)%j-A(j,343)%j+A(j,345)%j+A(j,398)%j-A(j,399)%j+A(j,404)%j-A(j,405)%j+A(j,421)%j-A(j,423)%j &
         -A(j,428)%j-A(j,429)%j-A(j,434)%j-A(j,442)%j-A(j,443)%j+A(j,446)%j-A(j,449)%j+A(j,450)%j+A(j,469)%j-A(j,470)%j-A(j,474)%j &
         +A(j,506)%j)*f(1)
  M1(120) = 2*(A(j,1)%j-A(j,2)%j-A(j,4)%j+A(j,5)%j-A(j,28)%j+A(j,30)%j+A(j,31)%j-A(j,33)%j+A(j,83)%j-A(j,84)%j-A(j,86)%j+A(j,87)%j &
         +A(j,92)%j-A(j,93)%j+A(j,100)%j-A(j,101)%j-A(j,103)%j+A(j,105)%j+A(j,112)%j-A(j,113)%j-A(j,118)%j+A(j,119)%j-A(j,139)%j &
         +A(j,140)%j+A(j,142)%j-A(j,143)%j+A(j,175)%j-A(j,176)%j-A(j,212)%j+A(j,213)%j+A(j,250)%j-A(j,252)%j-A(j,265)%j+A(j,266)%j &
         -A(j,268)%j+A(j,269)%j-A(j,271)%j+A(j,273)%j+A(j,278)%j-A(j,279)%j+A(j,280)%j-A(j,281)%j+A(j,305)%j-A(j,306)%j+A(j,361)%j &
         -A(j,362)%j-A(j,364)%j+A(j,366)%j-A(j,379)%j+A(j,381)%j+A(j,397)%j-A(j,398)%j+A(j,403)%j-A(j,404)%j+A(j,406)%j-A(j,408)%j &
         +A(j,413)%j-A(j,414)%j+A(j,419)%j+A(j,442)%j+A(j,443)%j-A(j,446)%j+A(j,449)%j-A(j,450)%j+A(j,481)%j-A(j,482)%j-A(j,486)%j &
         +A(j,505)%j)*f(1)

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
  use ol_colourmatrix_ppjjjj_gggggg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(120)
  real(REALKIND),    intent(out) :: M2colint(0:23-1)
  integer,           intent(in)  :: extcombs(:)
  integer :: extcomb, colmatpos, i, j, eco

  M2colint = 0

  do eco = 1, size(extcombs)
    extcomb = extcombs(eco)
    colmatpos = 120*extcomb
    do i = 1, 120
      do j = 1, 120
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
  use ol_colourmatrix_ppjjjj_gggggg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(120)
  complex(REALKIND), intent(in)  :: M2(120)
  real(REALKIND),    intent(out) :: M2colint
  integer :: i, j

  M2colint = 0

  do i = 1, 120
    do j = 1, 120
      M2colint = M2colint + real(conjg(M1(i))*K1(i,j)*M2(j))
    end do
  end do

end subroutine colintmunu

end subroutine amp2


#ifdef PRECISION_dp
subroutine colourvector(M1out, nhelout) &
  & bind(c,name="ol_tree_colvect_ppjjjj_gggggg_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(120,64)
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
    & bind(c,name="ol_f_amp2tree_ppjjjj_gggggg_1")
#else
subroutine amp2tree(P, M2)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix element without fuss.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,6)
  real(REALKIND),  intent(out) :: M2
  real(REALKIND) :: M2tmp(0:23-1)
  real(REALKIND) :: M2munu(4,4)
  call amp2(P, M2tmp, 0, [ 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND ], 1, [ 0 ], M2munu)
  M2 = M2tmp(0)
end subroutine amp2tree


#ifdef PRECISION_dp
subroutine amp2ccone(P, M2, I, J) &
    & bind(c,name="ol_f_amp2ccone_ppjjjj_gggggg_1")
#else
subroutine amp2ccone(P, M2, I, J)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix for the colour correlation matrix for particles I and J.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,6)
  real(REALKIND),  intent(out) :: M2
  integer,         intent(in)  :: I, J
  integer        :: extcomb
  real(REALKIND) :: M2tmp(0:23-1)
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
    & bind(c,name="ol_f_amp2ccall_ppjjjj_gggggg_1")
#else
subroutine amp2ccall(P, M2)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix for all colour correlation matrices.
  ! The correlation between particles i and j is at position i*(i-1)/2+j of the array M2.
  ! M2(0) is AMP2tree
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,6)
  real(REALKIND),  intent(out) :: M2(0:23-1)
  real(REALKIND) :: M2munu(4,4)
  integer :: k
  call amp2(P, M2, 0, [ 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND ], &
    23, [ (k, k = 0, 23-1) ], M2munu)
end subroutine amp2ccall


#ifdef PRECISION_dp
subroutine amp2hcone(P, M2, I, J, MOM) &
    & bind(c,name="ol_f_amp2hcone_ppjjjj_gggggg_1")
#else
subroutine amp2hcone(P, M2, I, J, MOM)
#endif
  ! This is an interface for AMP2.
  ! Calculates the helicity correlation for emitter I with momentum MOM and spectator J
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,6)
  real(REALKIND),  intent(out) :: M2
  integer,         intent(in)  :: I, J
  real(REALKIND),  intent(in)  :: MOM(0:3)
  integer        :: extcomb
  real(REALKIND) :: M2tmp(0:23-1)
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
    & bind(c,name="ol_f_amp2hcall_ppjjjj_gggggg_1")
#else
subroutine amp2hcall(P, M2, I, MOM)
#endif
  ! This is an interface for AMP2.
  ! Calculates all helicity correlations for emitter I with momentum MOM.
  ! The correlator for spectator j is at position j of the array M2.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,6)
  real(REALKIND),  intent(out) :: M2(6)
  integer,         intent(in)  :: I
  real(REALKIND),  intent(in)  :: MOM(0:3)
  real(REALKIND) :: M2tmp(0:23-1)
  real(REALKIND) :: M2munu(4,4)
  integer        :: J, extcombs(6)
  do J = 1, 6
    if (J <= I) then
      extcombs(J) = I*(I-1)/2 + J
    else
      extcombs(J) = J*(J-1)/2 + I
    end if
  end do
  call amp2(P, M2tmp, I, MOM, 6,extcombs, M2munu)
  do J = 1, 6
    M2(J) = M2tmp(extcombs(J))
  end do
end subroutine amp2hcall


#ifdef PRECISION_dp

subroutine amp2tree_c(p, m2) &
    & bind(c,name="ol_amp2tree_ppjjjj_gggggg_1")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2
  f_p = p
  call amp2tree(f_p, f_m2)
  m2 = f_m2
end subroutine amp2tree_c

subroutine amp2ccone_c(p, m2, i, j) &
    & bind(c,name="ol_amp2ccone_ppjjjj_gggggg_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  f_p = p
  f_i = i
  f_j = j
  call amp2ccone(f_p, f_m2, f_i, f_j)
  m2 = f_m2
end subroutine amp2ccone_c

subroutine amp2ccall_c(p, m2) &
    & bind(c,name="ol_amp2ccall_ppjjjj_gggggg_1")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2(0:23-1)
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2(0:23-1)
  f_p = p
  call amp2ccall(f_p, f_m2)
  m2 = f_m2
end subroutine amp2ccall_c

subroutine amp2hcone_c(p, m2, i, j, mom) &
    & bind(c,name="ol_amp2hcone_ppjjjj_gggggg_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,6)
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
    & bind(c,name="ol_amp2hcall_ppjjjj_gggggg_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2(6)
  integer(c_int), intent(in) :: i
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2(6)
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
    & bind(c,name="amp2tree_ppjjjj_gggggg_1_")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2
  f_p = p
  call amp2tree(f_p, f_m2)
  m2 = f_m2
end subroutine amp2tree_legacy

subroutine amp2ccone_legacy(p, m2, i, j) &
    & bind(c,name="amp2ccone_ppjjjj_gggggg_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  f_p = p
  f_i = i
  f_j = j
  call amp2ccone(f_p, f_m2, f_i, f_j)
  m2 = f_m2
end subroutine amp2ccone_legacy

subroutine amp2ccall_legacy(p, m2) &
    & bind(c,name="amp2ccall_ppjjjj_gggggg_1_")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2(0:23-1)
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2(0:23-1)
  f_p = p
  call amp2ccall(f_p, f_m2)
  m2 = f_m2
end subroutine amp2ccall_legacy

subroutine amp2hcone_legacy(p, m2, i, j, mom) &
    & bind(c,name="amp2hcone_ppjjjj_gggggg_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,6)
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
    & bind(c,name="amp2hcall_ppjjjj_gggggg_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,6)
  real(c_double), intent(out) :: m2(6)
  integer(c_int), intent(in) :: i
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,6)
  real(DREALKIND) :: f_m2(6)
  integer :: f_i
  real(DREALKIND) :: f_mom(0:3)
  f_p = p
  f_i = i
  f_mom = mom
  call amp2hcall(f_p, f_m2, f_i, f_mom)
  m2 = f_m2
end subroutine amp2hcall_legacy

#endif

end module ol_tree_ppjjjj_gggggg_1_/**/REALKIND
