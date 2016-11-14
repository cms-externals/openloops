
module ol_tree_heftpphjjj_hggggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(1)
  complex(REALKIND), save :: den(290)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 32 ! number of helicity configurations
  integer(intkind2), save :: nhel = 32 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(32) ! physical helicity states
  complex(DREALKIND) :: M1helarr(24,32) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = (eQED*gQCD**5)/(MW*pi**2*sw*24._/**/REALKIND)

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
  den(11) = 1 / (Q(5,6))
  den(12) = 1 / (Q(5,10))
  den(13) = 1 / (Q(5,18))
  den(14) = 1 / (Q(5,34))
  den(15) = 1 / (Q(5,12))
  den(16) = 1 / (Q(5,20))
  den(17) = 1 / (Q(5,36))
  den(18) = 1 / (Q(5,24))
  den(19) = 1 / (Q(5,40))
  den(20) = 1 / (Q(5,48))
  den(21) = 1 / (Q(5,3))
  den(25) = 1 / (Q(5,56))
  den(29) = 1 / (Q(5,52))
  den(32) = 1 / (Q(5,44))
  den(34) = 1 / (Q(5,28))
  den(36) = 1 / (Q(5,5))
  den(41) = 1 / (Q(5,9))
  den(43) = 1 / (Q(5,17))
  den(45) = 1 / (Q(5,33))
  den(62) = 1 / (Q(5,50))
  den(65) = 1 / (Q(5,42))
  den(67) = 1 / (Q(5,26))
  den(82) = 1 / (Q(5,38))
  den(84) = 1 / (Q(5,22))
  den(92) = 1 / (Q(5,14))

  ! denominators

  den(22) = den(15)*den(21)
  den(23) = den(16)*den(21)
  den(24) = den(17)*den(21)
  den(26) = den(21)*den(25)
  den(27) = den(18)*den(21)
  den(28) = den(19)*den(21)
  den(30) = den(21)*den(29)
  den(31) = den(20)*den(21)
  den(33) = den(21)*den(32)
  den(35) = den(21)*den(34)
  den(37) = den(12)*den(36)
  den(38) = den(13)*den(36)
  den(39) = den(14)*den(36)
  den(40) = den(25)*den(36)
  den(42) = den(11)*den(41)
  den(44) = den(11)*den(43)
  den(46) = den(11)*den(45)
  den(47) = den(11)*den(25)
  den(48) = den(13)*den(41)
  den(49) = den(14)*den(41)
  den(50) = den(29)*den(41)
  den(51) = den(12)*den(43)
  den(52) = den(12)*den(45)
  den(53) = den(12)*den(29)
  den(54) = den(14)*den(43)
  den(55) = den(32)*den(43)
  den(56) = den(13)*den(45)
  den(57) = den(13)*den(32)
  den(58) = den(34)*den(45)
  den(59) = den(14)*den(34)
  den(60) = den(18)*den(36)
  den(61) = den(19)*den(36)
  den(63) = den(36)*den(62)
  den(64) = den(20)*den(36)
  den(66) = den(36)*den(65)
  den(68) = den(36)*den(67)
  den(69) = den(16)*den(41)
  den(70) = den(17)*den(41)
  den(71) = den(41)*den(62)
  den(72) = den(15)*den(43)
  den(73) = den(15)*den(45)
  den(74) = den(15)*den(62)
  den(75) = den(17)*den(43)
  den(76) = den(43)*den(65)
  den(77) = den(16)*den(45)
  den(78) = den(16)*den(65)
  den(79) = den(45)*den(67)
  den(80) = den(17)*den(67)
  den(81) = den(20)*den(41)
  den(83) = den(41)*den(82)
  den(85) = den(41)*den(84)
  den(86) = den(19)*den(43)
  den(87) = den(43)*den(82)
  den(88) = den(18)*den(45)
  den(89) = den(18)*den(82)
  den(90) = den(45)*den(84)
  den(91) = den(19)*den(84)
  den(93) = den(43)*den(92)
  den(94) = den(45)*den(92)
  den(95) = den(20)*den(92)
  den(96) = den(11)*den(18)
  den(97) = den(11)*den(19)
  den(98) = den(10)*den(11)
  den(99) = den(11)*den(20)
  den(100) = den(9)*den(11)
  den(101) = den(8)*den(11)
  den(102) = den(12)*den(16)
  den(103) = den(12)*den(17)
  den(104) = den(10)*den(12)
  den(105) = den(13)*den(15)
  den(106) = den(14)*den(15)
  den(107) = den(10)*den(15)
  den(108) = den(13)*den(17)
  den(109) = den(9)*den(13)
  den(110) = den(14)*den(16)
  den(111) = den(9)*den(16)
  den(112) = den(8)*den(14)
  den(113) = den(8)*den(17)
  den(114) = den(12)*den(20)
  den(115) = den(7)*den(12)
  den(116) = den(6)*den(12)
  den(117) = den(13)*den(19)
  den(118) = den(7)*den(13)
  den(119) = den(14)*den(18)
  den(120) = den(7)*den(18)
  den(121) = den(6)*den(14)
  den(122) = den(6)*den(19)
  den(123) = den(5)*den(13)
  den(124) = den(5)*den(14)
  den(125) = den(5)*den(20)
  den(126) = den(15)*den(20)
  den(127) = den(4)*den(15)
  den(128) = den(3)*den(15)
  den(129) = den(16)*den(19)
  den(130) = den(4)*den(16)
  den(131) = den(17)*den(18)
  den(132) = den(4)*den(18)
  den(133) = den(3)*den(17)
  den(134) = den(3)*den(19)
  den(135) = den(2)*den(16)
  den(136) = den(2)*den(17)
  den(137) = den(2)*den(20)
  den(138) = den(1)*den(18)
  den(139) = den(1)*den(19)
  den(140) = den(1)*den(20)
  den(141) = den(20)*den(22)
  den(142) = den(3)*den(21)
  den(143) = den(15)*den(142)
  den(144) = den(15)*den(34)
  den(145) = den(21)*den(144)
  den(146) = den(19)*den(23)
  den(147) = den(2)*den(21)
  den(148) = den(16)*den(147)
  den(149) = den(16)*den(34)
  den(150) = den(21)*den(149)
  den(151) = den(18)*den(24)
  den(152) = den(1)*den(21)
  den(153) = den(18)*den(152)
  den(154) = den(18)*den(34)
  den(155) = den(21)*den(154)
  den(156) = den(17)*den(147)
  den(157) = den(17)*den(32)
  den(158) = den(21)*den(157)
  den(159) = den(19)*den(152)
  den(160) = den(19)*den(32)
  den(161) = den(21)*den(160)
  den(162) = den(20)*den(152)
  den(163) = den(20)*den(29)
  den(164) = den(21)*den(163)
  den(165) = den(20)*den(37)
  den(166) = den(6)*den(36)
  den(167) = den(12)*den(166)
  den(168) = den(12)*den(67)
  den(169) = den(36)*den(168)
  den(170) = den(19)*den(38)
  den(171) = den(5)*den(36)
  den(172) = den(13)*den(171)
  den(173) = den(13)*den(67)
  den(174) = den(36)*den(173)
  den(175) = den(18)*den(39)
  den(176) = den(1)*den(36)
  den(177) = den(18)*den(176)
  den(178) = den(18)*den(67)
  den(179) = den(36)*den(178)
  den(180) = den(14)*den(171)
  den(181) = den(14)*den(65)
  den(182) = den(36)*den(181)
  den(183) = den(19)*den(176)
  den(184) = den(19)*den(65)
  den(185) = den(36)*den(184)
  den(186) = den(20)*den(176)
  den(187) = den(20)*den(62)
  den(188) = den(36)*den(187)
  den(189) = den(20)*den(42)
  den(190) = den(8)*den(41)
  den(191) = den(11)*den(190)
  den(192) = den(11)*den(84)
  den(193) = den(41)*den(192)
  den(194) = den(19)*den(44)
  den(195) = den(8)*den(43)
  den(196) = den(11)*den(195)
  den(197) = den(11)*den(92)
  den(198) = den(43)*den(197)
  den(199) = den(18)*den(46)
  den(200) = den(1)*den(11)
  den(201) = den(18)*den(200)
  den(202) = den(8)*den(18)
  den(203) = den(11)*den(202)
  den(204) = den(9)*den(45)
  den(205) = den(11)*den(204)
  den(206) = den(45)*den(197)
  den(207) = den(19)*den(200)
  den(208) = den(9)*den(19)
  den(209) = den(11)*den(208)
  den(210) = den(20)*den(200)
  den(211) = den(10)*den(20)
  den(212) = den(11)*den(211)
  den(213) = den(17)*den(48)
  den(214) = den(5)*den(41)
  den(215) = den(13)*den(214)
  den(216) = den(13)*den(84)
  den(217) = den(41)*den(216)
  den(218) = den(16)*den(49)
  den(219) = den(2)*den(41)
  den(220) = den(16)*den(219)
  den(221) = den(16)*den(84)
  den(222) = den(41)*den(221)
  den(223) = den(14)*den(214)
  den(224) = den(14)*den(82)
  den(225) = den(41)*den(224)
  den(226) = den(17)*den(219)
  den(227) = den(17)*den(82)
  den(228) = den(41)*den(227)
  den(229) = den(20)*den(219)
  den(230) = den(41)*den(187)
  den(231) = den(17)*den(51)
  den(232) = den(6)*den(43)
  den(233) = den(12)*den(232)
  den(234) = den(12)*den(92)
  den(235) = den(43)*den(234)
  den(236) = den(16)*den(52)
  den(237) = den(2)*den(12)
  den(238) = den(16)*den(237)
  den(239) = den(6)*den(16)
  den(240) = den(12)*den(239)
  den(241) = den(7)*den(45)
  den(242) = den(12)*den(241)
  den(243) = den(45)*den(234)
  den(244) = den(17)*den(237)
  den(245) = den(7)*den(17)
  den(246) = den(12)*den(245)
  den(247) = den(20)*den(237)
  den(248) = den(12)*den(211)
  den(249) = den(15)*den(54)
  den(250) = den(3)*den(43)
  den(251) = den(15)*den(250)
  den(252) = den(15)*den(92)
  den(253) = den(43)*den(252)
  den(254) = den(15)*den(56)
  den(255) = den(3)*den(13)
  den(256) = den(15)*den(255)
  den(257) = den(5)*den(15)
  den(258) = den(13)*den(257)
  den(259) = den(4)*den(45)
  den(260) = den(15)*den(259)
  den(261) = den(45)*den(252)
  den(262) = den(4)*den(14)
  den(263) = den(15)*den(262)
  den(264) = den(14)*den(257)
  den(265) = den(20)*den(257)
  den(266) = den(15)*den(211)
  den(267) = den(14)*den(232)
  den(268) = den(43)*den(224)
  den(269) = den(17)*den(250)
  den(270) = den(43)*den(227)
  den(271) = den(19)*den(250)
  den(272) = den(43)*den(184)
  den(273) = den(13)*den(241)
  den(274) = den(45)*den(216)
  den(275) = den(17)*den(255)
  den(276) = den(13)*den(245)
  den(277) = den(19)*den(255)
  den(278) = den(13)*den(208)
  den(279) = den(16)*den(259)
  den(280) = den(45)*den(221)
  den(281) = den(16)*den(262)
  den(282) = den(14)*den(239)
  den(283) = den(19)*den(239)
  den(284) = den(16)*den(208)
  den(285) = den(18)*den(259)
  den(286) = den(45)*den(178)
  den(287) = den(18)*den(262)
  den(288) = den(14)*den(202)
  den(289) = den(18)*den(245)
  den(290) = den(17)*den(202)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_heftpphjjj_hggggg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_heftpphjjj_hggggg_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for higgs glue glue glue glue glue -> 0
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
  use ol_external_heftpphjjj_hggggg_1, only: external_perm_heftpphjjj_hggggg_1, &
    & external_perm_inv_heftpphjjj_hggggg_1, extcomb_perm_heftpphjjj_hggggg_1, &
    & average_factor_heftpphjjj_hggggg_1
  use ol_external_heftpphjjj_hggggg_1, only: H, hel_not_initialised, hel_init, POLSEL
  use ol_colourmatrix_heftpphjjj_hggggg_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_heftpphjjj_hggggg_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_heftpphjjj_hggggg_1
  use ol_kinematics_/**/REALKIND, only: LC2Std_Rep_cmplx
  implicit none

  real(DREALKIND), intent(in)  :: P_scatt(0:3,6)
  real(REALKIND),  intent(out) :: M2(0:23-1)
  real(REALKIND),  intent(out) :: M2munu(4,4)
  integer,         intent(in)  :: I
  real(REALKIND),  intent(in)  :: MOM(0:3)
  integer,         intent(in)  :: nextcombs
  integer,         intent(in)  :: extcombs(nextcombs)

  integer           :: ReplacePol, JBmunu, extcombs_permuted(nextcombs), shift, k, r, m, n
  real(REALKIND)    :: P(0:3,6)
  real(REALKIND)    :: extmasses2(6)
  real(REALKIND)    :: M2add(0:23-1)
  complex(REALKIND) :: M2munuadd(2)
  complex(REALKIND) :: MOM_LC(4), M1(24), M1helarray(24,32)
  real(REALKIND)    :: P_scatt_intern(0:3,6)
  complex(REALKIND) :: epLC(1:4), epStd(1:4,2)
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(1), ex2(2), ex3(2), ex4(2), ex5(2), ex6(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf2(2,5), wf4(4,50), wf8(8,210), wf32(32,390)

  type(polcont) :: A(32,390)
  complex(REALKIND) :: Aj(390)

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
  extmasses2 = [ rMH2, rZERO2, rZERO2, rZERO2, rZERO2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_heftpphjjj_hggggg_1,6)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,6)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_heftpphjjj_hggggg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_heftpphjjj_hggggg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_heftpphjjj_hggggg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  42 continue
  ! external WFs
  call pol_wf_S(P(:,1), rMH, H1, ex1, POLSEL(1))
  call pol_wf_V(P(:,2), rZERO, H2, ex2, POLSEL(2))
  call pol_wf_V(P(:,3), rZERO, H3, ex3, POLSEL(3))
  call pol_wf_V(P(:,4), rZERO, H4, ex4, POLSEL(4))
  call pol_wf_V(P(:,5), rZERO, H5, ex5, POLSEL(5))
  call pol_wf_V(P(:,6), rZERO, H6, ex6, POLSEL(6))


  if (ntry == 1) then
    shift = 1
    ! call helbookkeeping_flip(H1, 1, shift, eflip, exthel, firstpol)
    ! call helbookkeeping_wf(H1, ex1, shift) ...

    if (any(POLSEL /= 0)) then

      call pol_wf_S(P(:,1), rMH, H1, ex1, 0)
      call pol_wf_V(P(:,2), rZERO, H2, ex2, 0)
      call pol_wf_V(P(:,3), rZERO, H3, ex3, 0)
      call pol_wf_V(P(:,4), rZERO, H4, ex4, 0)
      call pol_wf_V(P(:,5), rZERO, H5, ex5, 0)
      call pol_wf_V(P(:,6), rZERO, H6, ex6, 0)

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

  end if

  ! internal WFs
  ! e.g. call vert_VQ_A(ntry, ex3, ex1, wf1, n1, t1) ...
  call vert_HGG_G(ntry, ex1, ex2, Q(:,2), ex3, Q(:,4), wf4(:,1), Q(:,7), n4(:,1), t4x4(:,:,1))
  call vert_GGG_G(ntry, ex4, ex5, ex6, wf8(:,1), n4(:,2), t4x8(:,:,1))
  call vert_GGG_G(ntry, ex5, ex6, ex4, wf8(:,2), n4(:,3), t4x8(:,:,2))
  call vert_GGG_G(ntry, ex6, ex4, ex5, wf8(:,3), n4(:,4), t4x8(:,:,3))
  call vert_HGG_G(ntry, ex1, ex2, Q(:,2), ex4, Q(:,8), wf4(:,2), Q(:,11), n4(:,5), t4x4(:,:,2))
  call vert_GGG_G(ntry, ex3, ex5, ex6, wf8(:,4), n4(:,6), t4x8(:,:,4))
  call vert_GGG_G(ntry, ex5, ex6, ex3, wf8(:,5), n4(:,7), t4x8(:,:,5))
  call vert_GGG_G(ntry, ex6, ex3, ex5, wf8(:,6), n4(:,8), t4x8(:,:,6))
  call vert_HGG_G(ntry, ex1, ex2, Q(:,2), ex5, Q(:,16), wf4(:,3), Q(:,19), n4(:,9), t4x4(:,:,3))
  call vert_GGG_G(ntry, ex3, ex4, ex6, wf8(:,7), n4(:,10), t4x8(:,:,7))
  call vert_GGG_G(ntry, ex4, ex6, ex3, wf8(:,8), n4(:,11), t4x8(:,:,8))
  call vert_GGG_G(ntry, ex6, ex3, ex4, wf8(:,9), n4(:,12), t4x8(:,:,9))
  call vert_HGG_G(ntry, ex1, ex2, Q(:,2), ex6, Q(:,32), wf4(:,4), Q(:,35), n4(:,13), t4x4(:,:,4))
  call vert_GGG_G(ntry, ex3, ex4, ex5, wf8(:,10), n4(:,14), t4x8(:,:,10))
  call vert_GGG_G(ntry, ex4, ex5, ex3, wf8(:,11), n4(:,15), t4x8(:,:,11))
  call vert_GGG_G(ntry, ex5, ex3, ex4, wf8(:,12), n4(:,16), t4x8(:,:,12))
  call vert_HGG_G(ntry, ex1, ex3, Q(:,4), ex4, Q(:,8), wf4(:,5), Q(:,13), n4(:,17), t4x4(:,:,5))
  call vert_GGG_G(ntry, ex2, ex5, ex6, wf8(:,13), n4(:,18), t4x8(:,:,13))
  call vert_GGG_G(ntry, ex5, ex6, ex2, wf8(:,14), n4(:,19), t4x8(:,:,14))
  call vert_GGG_G(ntry, ex6, ex2, ex5, wf8(:,15), n4(:,20), t4x8(:,:,15))
  call vert_HGG_G(ntry, ex1, ex3, Q(:,4), ex5, Q(:,16), wf4(:,6), Q(:,21), n4(:,21), t4x4(:,:,6))
  call vert_GGG_G(ntry, ex2, ex4, ex6, wf8(:,16), n4(:,22), t4x8(:,:,16))
  call vert_GGG_G(ntry, ex4, ex6, ex2, wf8(:,17), n4(:,23), t4x8(:,:,17))
  call vert_GGG_G(ntry, ex6, ex2, ex4, wf8(:,18), n4(:,24), t4x8(:,:,18))
  call vert_HGG_G(ntry, ex1, ex3, Q(:,4), ex6, Q(:,32), wf4(:,7), Q(:,37), n4(:,25), t4x4(:,:,7))
  call vert_GGG_G(ntry, ex2, ex4, ex5, wf8(:,19), n4(:,26), t4x8(:,:,19))
  call vert_GGG_G(ntry, ex4, ex5, ex2, wf8(:,20), n4(:,27), t4x8(:,:,20))
  call vert_GGG_G(ntry, ex5, ex2, ex4, wf8(:,21), n4(:,28), t4x8(:,:,21))
  call vert_HGG_G(ntry, ex1, ex4, Q(:,8), ex5, Q(:,16), wf4(:,8), Q(:,25), n4(:,29), t4x4(:,:,8))
  call vert_GGG_G(ntry, ex2, ex3, ex6, wf8(:,22), n4(:,30), t4x8(:,:,22))
  call vert_GGG_G(ntry, ex3, ex6, ex2, wf8(:,23), n4(:,31), t4x8(:,:,23))
  call vert_GGG_G(ntry, ex6, ex2, ex3, wf8(:,24), n4(:,32), t4x8(:,:,24))
  call vert_HGG_G(ntry, ex1, ex4, Q(:,8), ex6, Q(:,32), wf4(:,9), Q(:,41), n4(:,33), t4x4(:,:,9))
  call vert_GGG_G(ntry, ex2, ex3, ex5, wf8(:,25), n4(:,34), t4x8(:,:,25))
  call vert_GGG_G(ntry, ex3, ex5, ex2, wf8(:,26), n4(:,35), t4x8(:,:,26))
  call vert_GGG_G(ntry, ex5, ex2, ex3, wf8(:,27), n4(:,36), t4x8(:,:,27))
  call vert_HGG_G(ntry, ex1, ex5, Q(:,16), ex6, Q(:,32), wf4(:,10), Q(:,49), n4(:,37), t4x4(:,:,10))
  call vert_GGG_G(ntry, ex2, ex3, ex4, wf8(:,28), n4(:,38), t4x8(:,:,28))
  call vert_GGG_G(ntry, ex3, ex4, ex2, wf8(:,29), n4(:,39), t4x8(:,:,29))
  call vert_GGG_G(ntry, ex4, ex2, ex3, wf8(:,30), n4(:,40), t4x8(:,:,30))
  call vert_UV_W(ntry, ex2, Q(:,2), ex3, Q(:,4), wf4(:,11), n3(:,1), t3x4(:,:,1))
  call vert_HGGG_G(ntry, ex1, ex4, ex5, ex6, wf8(:,31), n5(:,1), t5x8(:,:,1))
  call vert_HGGG_G(ntry, ex1, ex5, ex6, ex4, wf8(:,32), n5(:,2), t5x8(:,:,2))
  call vert_HGGG_G(ntry, ex1, ex6, ex4, ex5, wf8(:,33), n5(:,3), t5x8(:,:,3))
  call vert_UV_W(ntry, ex2, Q(:,2), ex4, Q(:,8), wf4(:,12), n3(:,2), t3x4(:,:,2))
  call vert_HGGG_G(ntry, ex1, ex3, ex5, ex6, wf8(:,34), n5(:,4), t5x8(:,:,4))
  call vert_HGGG_G(ntry, ex1, ex5, ex6, ex3, wf8(:,35), n5(:,5), t5x8(:,:,5))
  call vert_HGGG_G(ntry, ex1, ex6, ex3, ex5, wf8(:,36), n5(:,6), t5x8(:,:,6))
  call vert_UV_W(ntry, ex2, Q(:,2), ex5, Q(:,16), wf4(:,13), n3(:,3), t3x4(:,:,3))
  call vert_HGGG_G(ntry, ex1, ex3, ex4, ex6, wf8(:,37), n5(:,7), t5x8(:,:,7))
  call vert_HGGG_G(ntry, ex1, ex4, ex6, ex3, wf8(:,38), n5(:,8), t5x8(:,:,8))
  call vert_HGGG_G(ntry, ex1, ex6, ex3, ex4, wf8(:,39), n5(:,9), t5x8(:,:,9))
  call vert_UV_W(ntry, ex2, Q(:,2), ex6, Q(:,32), wf4(:,14), n3(:,4), t3x4(:,:,4))
  call vert_HGGG_G(ntry, ex1, ex3, ex4, ex5, wf8(:,40), n5(:,10), t5x8(:,:,10))
  call vert_HGGG_G(ntry, ex1, ex4, ex5, ex3, wf8(:,41), n5(:,11), t5x8(:,:,11))
  call vert_HGGG_G(ntry, ex1, ex5, ex3, ex4, wf8(:,42), n5(:,12), t5x8(:,:,12))
  call vert_UV_W(ntry, ex3, Q(:,4), ex4, Q(:,8), wf4(:,15), n3(:,5), t3x4(:,:,5))
  call vert_HGGG_G(ntry, ex1, ex2, ex5, ex6, wf8(:,43), n5(:,13), t5x8(:,:,13))
  call vert_HGGG_G(ntry, ex1, ex5, ex6, ex2, wf8(:,44), n5(:,14), t5x8(:,:,14))
  call vert_HGGG_G(ntry, ex1, ex6, ex2, ex5, wf8(:,45), n5(:,15), t5x8(:,:,15))
  call vert_UV_W(ntry, ex3, Q(:,4), ex5, Q(:,16), wf4(:,16), n3(:,6), t3x4(:,:,6))
  call vert_HGGG_G(ntry, ex1, ex2, ex4, ex6, wf8(:,46), n5(:,16), t5x8(:,:,16))
  call vert_HGGG_G(ntry, ex1, ex4, ex6, ex2, wf8(:,47), n5(:,17), t5x8(:,:,17))
  call vert_HGGG_G(ntry, ex1, ex6, ex2, ex4, wf8(:,48), n5(:,18), t5x8(:,:,18))
  call vert_UV_W(ntry, ex3, Q(:,4), ex6, Q(:,32), wf4(:,17), n3(:,7), t3x4(:,:,7))
  call vert_HGGG_G(ntry, ex1, ex2, ex4, ex5, wf8(:,49), n5(:,19), t5x8(:,:,19))
  call vert_HGGG_G(ntry, ex1, ex4, ex5, ex2, wf8(:,50), n5(:,20), t5x8(:,:,20))
  call vert_HGGG_G(ntry, ex1, ex5, ex2, ex4, wf8(:,51), n5(:,21), t5x8(:,:,21))
  call vert_UV_W(ntry, ex4, Q(:,8), ex5, Q(:,16), wf4(:,18), n3(:,8), t3x4(:,:,8))
  call vert_HGGG_G(ntry, ex1, ex2, ex3, ex6, wf8(:,52), n5(:,22), t5x8(:,:,22))
  call vert_HGGG_G(ntry, ex1, ex3, ex6, ex2, wf8(:,53), n5(:,23), t5x8(:,:,23))
  call vert_HGGG_G(ntry, ex1, ex6, ex2, ex3, wf8(:,54), n5(:,24), t5x8(:,:,24))
  call vert_UV_W(ntry, ex4, Q(:,8), ex6, Q(:,32), wf4(:,19), n3(:,9), t3x4(:,:,9))
  call vert_HGGG_G(ntry, ex1, ex2, ex3, ex5, wf8(:,55), n5(:,25), t5x8(:,:,25))
  call vert_HGGG_G(ntry, ex1, ex3, ex5, ex2, wf8(:,56), n5(:,26), t5x8(:,:,26))
  call vert_HGGG_G(ntry, ex1, ex5, ex2, ex3, wf8(:,57), n5(:,27), t5x8(:,:,27))
  call vert_UV_W(ntry, ex5, Q(:,16), ex6, Q(:,32), wf4(:,20), n3(:,10), t3x4(:,:,10))
  call vert_HGGG_G(ntry, ex1, ex2, ex3, ex4, wf8(:,58), n5(:,28), t5x8(:,:,28))
  call vert_HGGG_G(ntry, ex1, ex3, ex4, ex2, wf8(:,59), n5(:,29), t5x8(:,:,29))
  call vert_HGGG_G(ntry, ex1, ex4, ex2, ex3, wf8(:,60), n5(:,30), t5x8(:,:,30))
  call vert_HG_G(ntry, ex1, ex2, Q(:,2), wf2(:,1), Q(:,3), n3(:,11), t3x2(:,:,1))
  call vert_GGG_G(ntry, wf2(:,1), ex5, ex6, wf8(:,61), n4(:,41), t4x8(:,:,31))
  call vert_GGG_G(ntry, ex5, ex6, wf2(:,1), wf8(:,62), n4(:,42), t4x8(:,:,32))
  call vert_GGG_G(ntry, ex6, wf2(:,1), ex5, wf8(:,63), n4(:,43), t4x8(:,:,33))
  call vert_GGG_G(ntry, wf2(:,1), ex4, ex6, wf8(:,64), n4(:,44), t4x8(:,:,34))
  call vert_GGG_G(ntry, ex4, ex6, wf2(:,1), wf8(:,65), n4(:,45), t4x8(:,:,35))
  call vert_GGG_G(ntry, ex6, wf2(:,1), ex4, wf8(:,66), n4(:,46), t4x8(:,:,36))
  call vert_GGG_G(ntry, wf2(:,1), ex4, ex5, wf8(:,67), n4(:,47), t4x8(:,:,37))
  call vert_GGG_G(ntry, ex4, ex5, wf2(:,1), wf8(:,68), n4(:,48), t4x8(:,:,38))
  call vert_GGG_G(ntry, ex5, wf2(:,1), ex4, wf8(:,69), n4(:,49), t4x8(:,:,39))
  call vert_UV_W(ntry, wf2(:,1), Q(:,3), ex3, Q(:,4), wf4(:,21), n3(:,12), t3x4(:,:,11))
  call vert_GGG_G(ntry, wf2(:,1), ex3, ex6, wf8(:,70), n4(:,50), t4x8(:,:,40))
  call vert_GGG_G(ntry, ex3, ex6, wf2(:,1), wf8(:,71), n4(:,51), t4x8(:,:,41))
  call vert_GGG_G(ntry, ex6, wf2(:,1), ex3, wf8(:,72), n4(:,52), t4x8(:,:,42))
  call vert_GGG_G(ntry, wf2(:,1), ex3, ex5, wf8(:,73), n4(:,53), t4x8(:,:,43))
  call vert_GGG_G(ntry, ex3, ex5, wf2(:,1), wf8(:,74), n4(:,54), t4x8(:,:,44))
  call vert_GGG_G(ntry, ex5, wf2(:,1), ex3, wf8(:,75), n4(:,55), t4x8(:,:,45))
  call vert_UV_W(ntry, wf2(:,1), Q(:,3), ex4, Q(:,8), wf4(:,22), n3(:,13), t3x4(:,:,12))
  call vert_GGG_G(ntry, wf2(:,1), ex3, ex4, wf8(:,76), n4(:,56), t4x8(:,:,46))
  call vert_GGG_G(ntry, ex3, ex4, wf2(:,1), wf8(:,77), n4(:,57), t4x8(:,:,47))
  call vert_GGG_G(ntry, ex4, wf2(:,1), ex3, wf8(:,78), n4(:,58), t4x8(:,:,48))
  call vert_UV_W(ntry, wf2(:,1), Q(:,3), ex5, Q(:,16), wf4(:,23), n3(:,14), t3x4(:,:,13))
  call vert_UV_W(ntry, wf2(:,1), Q(:,3), ex6, Q(:,32), wf4(:,24), n3(:,15), t3x4(:,:,14))
  call vert_HG_G(ntry, ex1, ex3, Q(:,4), wf2(:,2), Q(:,5), n3(:,16), t3x2(:,:,2))
  call vert_GGG_G(ntry, wf2(:,2), ex5, ex6, wf8(:,79), n4(:,59), t4x8(:,:,49))
  call vert_GGG_G(ntry, ex5, ex6, wf2(:,2), wf8(:,80), n4(:,60), t4x8(:,:,50))
  call vert_GGG_G(ntry, ex6, wf2(:,2), ex5, wf8(:,81), n4(:,61), t4x8(:,:,51))
  call vert_GGG_G(ntry, wf2(:,2), ex4, ex6, wf8(:,82), n4(:,62), t4x8(:,:,52))
  call vert_GGG_G(ntry, ex4, ex6, wf2(:,2), wf8(:,83), n4(:,63), t4x8(:,:,53))
  call vert_GGG_G(ntry, ex6, wf2(:,2), ex4, wf8(:,84), n4(:,64), t4x8(:,:,54))
  call vert_GGG_G(ntry, wf2(:,2), ex4, ex5, wf8(:,85), n4(:,65), t4x8(:,:,55))
  call vert_GGG_G(ntry, ex4, ex5, wf2(:,2), wf8(:,86), n4(:,66), t4x8(:,:,56))
  call vert_GGG_G(ntry, ex5, wf2(:,2), ex4, wf8(:,87), n4(:,67), t4x8(:,:,57))
  call vert_UV_W(ntry, ex2, Q(:,2), wf2(:,2), Q(:,5), wf4(:,25), n3(:,17), t3x4(:,:,15))
  call vert_HG_G(ntry, ex1, ex4, Q(:,8), wf2(:,3), Q(:,9), n3(:,18), t3x2(:,:,3))
  call vert_GGG_G(ntry, wf2(:,3), ex5, ex6, wf8(:,88), n4(:,68), t4x8(:,:,58))
  call vert_GGG_G(ntry, ex5, ex6, wf2(:,3), wf8(:,89), n4(:,69), t4x8(:,:,59))
  call vert_GGG_G(ntry, ex6, wf2(:,3), ex5, wf8(:,90), n4(:,70), t4x8(:,:,60))
  call vert_HG_G(ntry, ex1, ex5, Q(:,16), wf2(:,4), Q(:,17), n3(:,19), t3x2(:,:,4))
  call vert_GGG_G(ntry, ex4, wf2(:,4), ex6, wf8(:,91), n4(:,71), t4x8(:,:,61))
  call vert_GGG_G(ntry, wf2(:,4), ex6, ex4, wf8(:,92), n4(:,72), t4x8(:,:,62))
  call vert_GGG_G(ntry, ex6, ex4, wf2(:,4), wf8(:,93), n4(:,73), t4x8(:,:,63))
  call vert_HG_G(ntry, ex1, ex6, Q(:,32), wf2(:,5), Q(:,33), n3(:,20), t3x2(:,:,5))
  call vert_GGG_G(ntry, ex4, ex5, wf2(:,5), wf8(:,94), n4(:,74), t4x8(:,:,64))
  call vert_GGG_G(ntry, ex5, wf2(:,5), ex4, wf8(:,95), n4(:,75), t4x8(:,:,65))
  call vert_GGG_G(ntry, wf2(:,5), ex4, ex5, wf8(:,96), n4(:,76), t4x8(:,:,66))
  call vert_HG_G(ntry, ex1, wf4(:,11), Q(:,6), wf4(:,26), Q(:,7), n3(:,21), t3x4(:,:,16))
  call vert_GGG_G(ntry, ex3, wf2(:,3), ex6, wf8(:,97), n4(:,77), t4x8(:,:,67))
  call vert_GGG_G(ntry, wf2(:,3), ex6, ex3, wf8(:,98), n4(:,78), t4x8(:,:,68))
  call vert_GGG_G(ntry, ex6, ex3, wf2(:,3), wf8(:,99), n4(:,79), t4x8(:,:,69))
  call vert_GGG_G(ntry, ex3, wf2(:,3), ex5, wf8(:,100), n4(:,80), t4x8(:,:,70))
  call vert_GGG_G(ntry, wf2(:,3), ex5, ex3, wf8(:,101), n4(:,81), t4x8(:,:,71))
  call vert_GGG_G(ntry, ex5, ex3, wf2(:,3), wf8(:,102), n4(:,82), t4x8(:,:,72))
  call vert_UV_W(ntry, ex2, Q(:,2), wf2(:,3), Q(:,9), wf4(:,27), n3(:,22), t3x4(:,:,17))
  call vert_GGG_G(ntry, ex3, wf2(:,4), ex6, wf8(:,103), n4(:,83), t4x8(:,:,73))
  call vert_GGG_G(ntry, wf2(:,4), ex6, ex3, wf8(:,104), n4(:,84), t4x8(:,:,74))
  call vert_GGG_G(ntry, ex6, ex3, wf2(:,4), wf8(:,105), n4(:,85), t4x8(:,:,75))
  call vert_GGG_G(ntry, ex3, ex5, wf2(:,5), wf8(:,106), n4(:,86), t4x8(:,:,76))
  call vert_GGG_G(ntry, ex5, wf2(:,5), ex3, wf8(:,107), n4(:,87), t4x8(:,:,77))
  call vert_GGG_G(ntry, wf2(:,5), ex3, ex5, wf8(:,108), n4(:,88), t4x8(:,:,78))
  call vert_HG_G(ntry, ex1, wf4(:,12), Q(:,10), wf4(:,28), Q(:,11), n3(:,23), t3x4(:,:,18))
  call vert_GGG_G(ntry, ex3, ex4, wf2(:,4), wf8(:,109), n4(:,89), t4x8(:,:,79))
  call vert_GGG_G(ntry, ex4, wf2(:,4), ex3, wf8(:,110), n4(:,90), t4x8(:,:,80))
  call vert_GGG_G(ntry, wf2(:,4), ex3, ex4, wf8(:,111), n4(:,91), t4x8(:,:,81))
  call vert_UV_W(ntry, ex2, Q(:,2), wf2(:,4), Q(:,17), wf4(:,29), n3(:,24), t3x4(:,:,19))
  call vert_GGG_G(ntry, ex3, ex4, wf2(:,5), wf8(:,112), n4(:,92), t4x8(:,:,82))
  call vert_GGG_G(ntry, ex4, wf2(:,5), ex3, wf8(:,113), n4(:,93), t4x8(:,:,83))
  call vert_GGG_G(ntry, wf2(:,5), ex3, ex4, wf8(:,114), n4(:,94), t4x8(:,:,84))
  call vert_HG_G(ntry, ex1, wf4(:,13), Q(:,18), wf4(:,30), Q(:,19), n3(:,25), t3x4(:,:,20))
  call vert_UV_W(ntry, ex2, Q(:,2), wf2(:,5), Q(:,33), wf4(:,31), n3(:,26), t3x4(:,:,21))
  call vert_HG_G(ntry, ex1, wf4(:,14), Q(:,34), wf4(:,32), Q(:,35), n3(:,27), t3x4(:,:,22))
  call vert_GGG_G(ntry, ex2, wf2(:,2), ex6, wf8(:,115), n4(:,95), t4x8(:,:,85))
  call vert_GGG_G(ntry, wf2(:,2), ex6, ex2, wf8(:,116), n4(:,96), t4x8(:,:,86))
  call vert_GGG_G(ntry, ex6, ex2, wf2(:,2), wf8(:,117), n4(:,97), t4x8(:,:,87))
  call vert_GGG_G(ntry, ex2, wf2(:,2), ex5, wf8(:,118), n4(:,98), t4x8(:,:,88))
  call vert_GGG_G(ntry, wf2(:,2), ex5, ex2, wf8(:,119), n4(:,99), t4x8(:,:,89))
  call vert_GGG_G(ntry, ex5, ex2, wf2(:,2), wf8(:,120), n4(:,100), t4x8(:,:,90))
  call vert_UV_W(ntry, wf2(:,2), Q(:,5), ex4, Q(:,8), wf4(:,33), n3(:,28), t3x4(:,:,23))
  call vert_GGG_G(ntry, ex2, wf2(:,2), ex4, wf8(:,121), n4(:,101), t4x8(:,:,91))
  call vert_GGG_G(ntry, wf2(:,2), ex4, ex2, wf8(:,122), n4(:,102), t4x8(:,:,92))
  call vert_GGG_G(ntry, ex4, ex2, wf2(:,2), wf8(:,123), n4(:,103), t4x8(:,:,93))
  call vert_UV_W(ntry, wf2(:,2), Q(:,5), ex5, Q(:,16), wf4(:,34), n3(:,29), t3x4(:,:,24))
  call vert_UV_W(ntry, wf2(:,2), Q(:,5), ex6, Q(:,32), wf4(:,35), n3(:,30), t3x4(:,:,25))
  call vert_GGG_G(ntry, ex2, wf2(:,3), ex6, wf8(:,124), n4(:,104), t4x8(:,:,94))
  call vert_GGG_G(ntry, wf2(:,3), ex6, ex2, wf8(:,125), n4(:,105), t4x8(:,:,95))
  call vert_GGG_G(ntry, ex6, ex2, wf2(:,3), wf8(:,126), n4(:,106), t4x8(:,:,96))
  call vert_GGG_G(ntry, ex2, wf2(:,3), ex5, wf8(:,127), n4(:,107), t4x8(:,:,97))
  call vert_GGG_G(ntry, wf2(:,3), ex5, ex2, wf8(:,128), n4(:,108), t4x8(:,:,98))
  call vert_GGG_G(ntry, ex5, ex2, wf2(:,3), wf8(:,129), n4(:,109), t4x8(:,:,99))
  call vert_UV_W(ntry, ex3, Q(:,4), wf2(:,3), Q(:,9), wf4(:,36), n3(:,31), t3x4(:,:,26))
  call vert_GGG_G(ntry, ex2, wf2(:,4), ex6, wf8(:,130), n4(:,110), t4x8(:,:,100))
  call vert_GGG_G(ntry, wf2(:,4), ex6, ex2, wf8(:,131), n4(:,111), t4x8(:,:,101))
  call vert_GGG_G(ntry, ex6, ex2, wf2(:,4), wf8(:,132), n4(:,112), t4x8(:,:,102))
  call vert_GGG_G(ntry, ex2, ex5, wf2(:,5), wf8(:,133), n4(:,113), t4x8(:,:,103))
  call vert_GGG_G(ntry, ex5, wf2(:,5), ex2, wf8(:,134), n4(:,114), t4x8(:,:,104))
  call vert_GGG_G(ntry, wf2(:,5), ex2, ex5, wf8(:,135), n4(:,115), t4x8(:,:,105))
  call vert_HG_G(ntry, ex1, wf4(:,15), Q(:,12), wf4(:,37), Q(:,13), n3(:,32), t3x4(:,:,27))
  call vert_GGG_G(ntry, ex2, ex4, wf2(:,4), wf8(:,136), n4(:,116), t4x8(:,:,106))
  call vert_GGG_G(ntry, ex4, wf2(:,4), ex2, wf8(:,137), n4(:,117), t4x8(:,:,107))
  call vert_GGG_G(ntry, wf2(:,4), ex2, ex4, wf8(:,138), n4(:,118), t4x8(:,:,108))
  call vert_UV_W(ntry, ex3, Q(:,4), wf2(:,4), Q(:,17), wf4(:,38), n3(:,33), t3x4(:,:,28))
  call vert_GGG_G(ntry, ex2, ex4, wf2(:,5), wf8(:,139), n4(:,119), t4x8(:,:,109))
  call vert_GGG_G(ntry, ex4, wf2(:,5), ex2, wf8(:,140), n4(:,120), t4x8(:,:,110))
  call vert_GGG_G(ntry, wf2(:,5), ex2, ex4, wf8(:,141), n4(:,121), t4x8(:,:,111))
  call vert_HG_G(ntry, ex1, wf4(:,16), Q(:,20), wf4(:,39), Q(:,21), n3(:,34), t3x4(:,:,29))
  call vert_UV_W(ntry, ex3, Q(:,4), wf2(:,5), Q(:,33), wf4(:,40), n3(:,35), t3x4(:,:,30))
  call vert_HG_G(ntry, ex1, wf4(:,17), Q(:,36), wf4(:,41), Q(:,37), n3(:,36), t3x4(:,:,31))
  call vert_GGG_G(ntry, ex2, ex3, wf2(:,3), wf8(:,142), n4(:,122), t4x8(:,:,112))
  call vert_GGG_G(ntry, ex3, wf2(:,3), ex2, wf8(:,143), n4(:,123), t4x8(:,:,113))
  call vert_GGG_G(ntry, wf2(:,3), ex2, ex3, wf8(:,144), n4(:,124), t4x8(:,:,114))
  call vert_UV_W(ntry, wf2(:,3), Q(:,9), ex5, Q(:,16), wf4(:,42), n3(:,37), t3x4(:,:,32))
  call vert_UV_W(ntry, wf2(:,3), Q(:,9), ex6, Q(:,32), wf4(:,43), n3(:,38), t3x4(:,:,33))
  call vert_GGG_G(ntry, ex2, ex3, wf2(:,4), wf8(:,145), n4(:,125), t4x8(:,:,115))
  call vert_GGG_G(ntry, ex3, wf2(:,4), ex2, wf8(:,146), n4(:,126), t4x8(:,:,116))
  call vert_GGG_G(ntry, wf2(:,4), ex2, ex3, wf8(:,147), n4(:,127), t4x8(:,:,117))
  call vert_UV_W(ntry, ex4, Q(:,8), wf2(:,4), Q(:,17), wf4(:,44), n3(:,39), t3x4(:,:,34))
  call vert_GGG_G(ntry, ex2, ex3, wf2(:,5), wf8(:,148), n4(:,128), t4x8(:,:,118))
  call vert_GGG_G(ntry, ex3, wf2(:,5), ex2, wf8(:,149), n4(:,129), t4x8(:,:,119))
  call vert_GGG_G(ntry, wf2(:,5), ex2, ex3, wf8(:,150), n4(:,130), t4x8(:,:,120))
  call vert_HG_G(ntry, ex1, wf4(:,18), Q(:,24), wf4(:,45), Q(:,25), n3(:,40), t3x4(:,:,35))
  call vert_UV_W(ntry, ex4, Q(:,8), wf2(:,5), Q(:,33), wf4(:,46), n3(:,41), t3x4(:,:,36))
  call vert_HG_G(ntry, ex1, wf4(:,19), Q(:,40), wf4(:,47), Q(:,41), n3(:,42), t3x4(:,:,37))
  call vert_UV_W(ntry, wf2(:,4), Q(:,17), ex6, Q(:,32), wf4(:,48), n3(:,43), t3x4(:,:,38))
  call vert_UV_W(ntry, ex5, Q(:,16), wf2(:,5), Q(:,33), wf4(:,49), n3(:,44), t3x4(:,:,39))
  call vert_HG_G(ntry, ex1, wf4(:,20), Q(:,48), wf4(:,50), Q(:,49), n3(:,45), t3x4(:,:,40))
  call vert_HGG_G(ntry, ex1, wf4(:,11), Q(:,6), ex6, Q(:,32), wf8(:,151), Q(:,39), n4(:,131), t4x8(:,:,121))
  call vert_HGG_G(ntry, ex1, wf4(:,11), Q(:,6), ex5, Q(:,16), wf8(:,152), Q(:,23), n4(:,132), t4x8(:,:,122))
  call vert_UV_W(ntry, wf4(:,11), Q(:,6), ex4, Q(:,8), wf8(:,153), n3(:,46), t3x8(:,:,1))
  call vert_HGG_G(ntry, ex1, wf4(:,11), Q(:,6), ex4, Q(:,8), wf8(:,154), Q(:,15), n4(:,133), t4x8(:,:,123))
  call vert_UV_W(ntry, wf4(:,11), Q(:,6), ex5, Q(:,16), wf8(:,155), n3(:,47), t3x8(:,:,2))
  call vert_UV_W(ntry, wf4(:,11), Q(:,6), ex6, Q(:,32), wf8(:,156), n3(:,48), t3x8(:,:,3))
  call vert_HGG_G(ntry, ex1, wf4(:,12), Q(:,10), ex6, Q(:,32), wf8(:,157), Q(:,43), n4(:,134), t4x8(:,:,124))
  call vert_HGG_G(ntry, ex1, wf4(:,12), Q(:,10), ex5, Q(:,16), wf8(:,158), Q(:,27), n4(:,135), t4x8(:,:,125))
  call vert_UV_W(ntry, ex3, Q(:,4), wf4(:,12), Q(:,10), wf8(:,159), n3(:,49), t3x8(:,:,4))
  call vert_HGG_G(ntry, ex1, wf4(:,13), Q(:,18), ex6, Q(:,32), wf8(:,160), Q(:,51), n4(:,136), t4x8(:,:,126))
  call vert_HGG_G(ntry, ex1, ex5, Q(:,16), wf4(:,14), Q(:,34), wf8(:,161), Q(:,51), n4(:,137), t4x8(:,:,127))
  call vert_UV_W(ntry, ex2, Q(:,2), wf4(:,15), Q(:,12), wf8(:,162), n3(:,50), t3x8(:,:,5))
  call vert_HGG_G(ntry, ex1, ex4, Q(:,8), wf4(:,13), Q(:,18), wf8(:,163), Q(:,27), n4(:,138), t4x8(:,:,128))
  call vert_UV_W(ntry, ex3, Q(:,4), wf4(:,13), Q(:,18), wf8(:,164), n3(:,51), t3x8(:,:,6))
  call vert_HGG_G(ntry, ex1, ex4, Q(:,8), wf4(:,14), Q(:,34), wf8(:,165), Q(:,43), n4(:,139), t4x8(:,:,129))
  call vert_UV_W(ntry, ex2, Q(:,2), wf4(:,16), Q(:,20), wf8(:,166), n3(:,52), t3x8(:,:,7))
  call vert_UV_W(ntry, ex3, Q(:,4), wf4(:,14), Q(:,34), wf8(:,167), n3(:,53), t3x8(:,:,8))
  call vert_UV_W(ntry, ex2, Q(:,2), wf4(:,17), Q(:,36), wf8(:,168), n3(:,54), t3x8(:,:,9))
  call vert_HGG_G(ntry, ex1, ex3, Q(:,4), wf4(:,12), Q(:,10), wf8(:,169), Q(:,15), n4(:,140), t4x8(:,:,130))
  call vert_UV_W(ntry, wf4(:,12), Q(:,10), ex5, Q(:,16), wf8(:,170), n3(:,55), t3x8(:,:,10))
  call vert_UV_W(ntry, wf4(:,12), Q(:,10), ex6, Q(:,32), wf8(:,171), n3(:,56), t3x8(:,:,11))
  call vert_HGG_G(ntry, ex1, ex3, Q(:,4), wf4(:,13), Q(:,18), wf8(:,172), Q(:,23), n4(:,141), t4x8(:,:,131))
  call vert_UV_W(ntry, ex4, Q(:,8), wf4(:,13), Q(:,18), wf8(:,173), n3(:,57), t3x8(:,:,12))
  call vert_HGG_G(ntry, ex1, ex3, Q(:,4), wf4(:,14), Q(:,34), wf8(:,174), Q(:,39), n4(:,142), t4x8(:,:,132))
  call vert_UV_W(ntry, ex2, Q(:,2), wf4(:,18), Q(:,24), wf8(:,175), n3(:,58), t3x8(:,:,13))
  call vert_UV_W(ntry, ex4, Q(:,8), wf4(:,14), Q(:,34), wf8(:,176), n3(:,59), t3x8(:,:,14))
  call vert_UV_W(ntry, ex2, Q(:,2), wf4(:,19), Q(:,40), wf8(:,177), n3(:,60), t3x8(:,:,15))
  call vert_UV_W(ntry, wf4(:,13), Q(:,18), ex6, Q(:,32), wf8(:,178), n3(:,61), t3x8(:,:,16))
  call vert_UV_W(ntry, ex5, Q(:,16), wf4(:,14), Q(:,34), wf8(:,179), n3(:,62), t3x8(:,:,17))
  call vert_UV_W(ntry, ex2, Q(:,2), wf4(:,20), Q(:,48), wf8(:,180), n3(:,63), t3x8(:,:,18))
  call vert_HGG_G(ntry, ex1, ex2, Q(:,2), wf4(:,15), Q(:,12), wf8(:,181), Q(:,15), n4(:,143), t4x8(:,:,133))
  call vert_UV_W(ntry, wf4(:,15), Q(:,12), ex5, Q(:,16), wf8(:,182), n3(:,64), t3x8(:,:,19))
  call vert_UV_W(ntry, wf4(:,15), Q(:,12), ex6, Q(:,32), wf8(:,183), n3(:,65), t3x8(:,:,20))
  call vert_HGG_G(ntry, ex1, ex2, Q(:,2), wf4(:,16), Q(:,20), wf8(:,184), Q(:,23), n4(:,144), t4x8(:,:,134))
  call vert_UV_W(ntry, ex4, Q(:,8), wf4(:,16), Q(:,20), wf8(:,185), n3(:,66), t3x8(:,:,21))
  call vert_HGG_G(ntry, ex1, ex2, Q(:,2), wf4(:,17), Q(:,36), wf8(:,186), Q(:,39), n4(:,145), t4x8(:,:,135))
  call vert_UV_W(ntry, ex3, Q(:,4), wf4(:,18), Q(:,24), wf8(:,187), n3(:,67), t3x8(:,:,22))
  call vert_UV_W(ntry, ex4, Q(:,8), wf4(:,17), Q(:,36), wf8(:,188), n3(:,68), t3x8(:,:,23))
  call vert_UV_W(ntry, ex3, Q(:,4), wf4(:,19), Q(:,40), wf8(:,189), n3(:,69), t3x8(:,:,24))
  call vert_UV_W(ntry, wf4(:,16), Q(:,20), ex6, Q(:,32), wf8(:,190), n3(:,70), t3x8(:,:,25))
  call vert_UV_W(ntry, ex5, Q(:,16), wf4(:,17), Q(:,36), wf8(:,191), n3(:,71), t3x8(:,:,26))
  call vert_UV_W(ntry, ex3, Q(:,4), wf4(:,20), Q(:,48), wf8(:,192), n3(:,72), t3x8(:,:,27))
  call vert_UV_W(ntry, wf4(:,18), Q(:,24), ex6, Q(:,32), wf8(:,193), n3(:,73), t3x8(:,:,28))
  call vert_UV_W(ntry, ex5, Q(:,16), wf4(:,19), Q(:,40), wf8(:,194), n3(:,74), t3x8(:,:,29))
  call vert_UV_W(ntry, ex4, Q(:,8), wf4(:,20), Q(:,48), wf8(:,195), n3(:,75), t3x8(:,:,30))
  call vert_UV_W(ntry, wf2(:,1), Q(:,3), wf4(:,15), Q(:,12), wf8(:,196), n3(:,76), t3x8(:,:,31))
  call vert_UV_W(ntry, wf2(:,1), Q(:,3), wf4(:,16), Q(:,20), wf8(:,197), n3(:,77), t3x8(:,:,32))
  call vert_UV_W(ntry, wf2(:,1), Q(:,3), wf4(:,17), Q(:,36), wf8(:,198), n3(:,78), t3x8(:,:,33))
  call vert_UV_W(ntry, wf2(:,2), Q(:,5), wf4(:,12), Q(:,10), wf8(:,199), n3(:,79), t3x8(:,:,34))
  call vert_UV_W(ntry, wf2(:,2), Q(:,5), wf4(:,13), Q(:,18), wf8(:,200), n3(:,80), t3x8(:,:,35))
  call vert_UV_W(ntry, wf2(:,2), Q(:,5), wf4(:,14), Q(:,34), wf8(:,201), n3(:,81), t3x8(:,:,36))
  call vert_UV_W(ntry, wf4(:,11), Q(:,6), wf2(:,3), Q(:,9), wf8(:,202), n3(:,82), t3x8(:,:,37))
  call vert_UV_W(ntry, wf4(:,11), Q(:,6), wf2(:,4), Q(:,17), wf8(:,203), n3(:,83), t3x8(:,:,38))
  call vert_UV_W(ntry, wf4(:,11), Q(:,6), wf2(:,5), Q(:,33), wf8(:,204), n3(:,84), t3x8(:,:,39))
  call vert_UV_W(ntry, wf2(:,3), Q(:,9), wf4(:,13), Q(:,18), wf8(:,205), n3(:,85), t3x8(:,:,40))
  call vert_UV_W(ntry, wf2(:,3), Q(:,9), wf4(:,14), Q(:,34), wf8(:,206), n3(:,86), t3x8(:,:,41))
  call vert_UV_W(ntry, wf4(:,12), Q(:,10), wf2(:,4), Q(:,17), wf8(:,207), n3(:,87), t3x8(:,:,42))
  call vert_UV_W(ntry, wf4(:,12), Q(:,10), wf2(:,5), Q(:,33), wf8(:,208), n3(:,88), t3x8(:,:,43))
  call vert_UV_W(ntry, wf2(:,4), Q(:,17), wf4(:,14), Q(:,34), wf8(:,209), n3(:,89), t3x8(:,:,44))
  call vert_UV_W(ntry, wf4(:,13), Q(:,18), wf2(:,5), Q(:,33), wf8(:,210), n3(:,90), t3x8(:,:,45))


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

    M2munu = M2munu / average_factor_heftpphjjj_hggggg_1
  end if

  ! Colour and helicity average and symmetry factor of outgoing particles
  M2add = M2 / average_factor_heftpphjjj_hggggg_1

  do k = 0, 23-1
    M2(k) = M2add(extcomb_perm_heftpphjjj_hggggg_1(k))
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

    call cont_VV(nsync, wf4(:,1), wf8(:,1), A(:,1), n3(:,91), t3x32(:,:,1), nhel, den(1))
    call cont_VV(nsync, wf4(:,1), wf8(:,2), A(:,2), n3(:,92), t3x32(:,:,2), nhel, den(1))
    call cont_VV(nsync, wf4(:,1), wf8(:,3), A(:,3), n3(:,93), t3x32(:,:,3), nhel, den(1))
    call cont_VV(nsync, wf4(:,2), wf8(:,4), A(:,4), n3(:,94), t3x32(:,:,4), nhel, den(2))
    call cont_VV(nsync, wf4(:,2), wf8(:,5), A(:,5), n3(:,95), t3x32(:,:,5), nhel, den(2))
    call cont_VV(nsync, wf4(:,2), wf8(:,6), A(:,6), n3(:,96), t3x32(:,:,6), nhel, den(2))
    call cont_VV(nsync, wf4(:,3), wf8(:,7), A(:,7), n3(:,97), t3x32(:,:,7), nhel, den(3))
    call cont_VV(nsync, wf4(:,3), wf8(:,8), A(:,8), n3(:,98), t3x32(:,:,8), nhel, den(3))
    call cont_VV(nsync, wf4(:,3), wf8(:,9), A(:,9), n3(:,99), t3x32(:,:,9), nhel, den(3))
    call cont_VV(nsync, wf4(:,4), wf8(:,10), A(:,10), n3(:,100), t3x32(:,:,10), nhel, den(4))
    call cont_VV(nsync, wf4(:,4), wf8(:,11), A(:,11), n3(:,101), t3x32(:,:,11), nhel, den(4))
    call cont_VV(nsync, wf4(:,4), wf8(:,12), A(:,12), n3(:,102), t3x32(:,:,12), nhel, den(4))
    call cont_VV(nsync, wf4(:,5), wf8(:,13), A(:,13), n3(:,103), t3x32(:,:,13), nhel, den(5))
    call cont_VV(nsync, wf4(:,5), wf8(:,14), A(:,14), n3(:,104), t3x32(:,:,14), nhel, den(5))
    call cont_VV(nsync, wf4(:,5), wf8(:,15), A(:,15), n3(:,105), t3x32(:,:,15), nhel, den(5))
    call cont_VV(nsync, wf4(:,6), wf8(:,16), A(:,16), n3(:,106), t3x32(:,:,16), nhel, den(6))
    call cont_VV(nsync, wf4(:,6), wf8(:,17), A(:,17), n3(:,107), t3x32(:,:,17), nhel, den(6))
    call cont_VV(nsync, wf4(:,6), wf8(:,18), A(:,18), n3(:,108), t3x32(:,:,18), nhel, den(6))
    call cont_VV(nsync, wf4(:,7), wf8(:,19), A(:,19), n3(:,109), t3x32(:,:,19), nhel, den(7))
    call cont_VV(nsync, wf4(:,7), wf8(:,20), A(:,20), n3(:,110), t3x32(:,:,20), nhel, den(7))
    call cont_VV(nsync, wf4(:,7), wf8(:,21), A(:,21), n3(:,111), t3x32(:,:,21), nhel, den(7))
    call cont_VV(nsync, wf4(:,8), wf8(:,22), A(:,22), n3(:,112), t3x32(:,:,22), nhel, den(8))
    call cont_VV(nsync, wf4(:,8), wf8(:,23), A(:,23), n3(:,113), t3x32(:,:,23), nhel, den(8))
    call cont_VV(nsync, wf4(:,8), wf8(:,24), A(:,24), n3(:,114), t3x32(:,:,24), nhel, den(8))
    call cont_VV(nsync, wf4(:,9), wf8(:,25), A(:,25), n3(:,115), t3x32(:,:,25), nhel, den(9))
    call cont_VV(nsync, wf4(:,9), wf8(:,26), A(:,26), n3(:,116), t3x32(:,:,26), nhel, den(9))
    call cont_VV(nsync, wf4(:,9), wf8(:,27), A(:,27), n3(:,117), t3x32(:,:,27), nhel, den(9))
    call cont_VV(nsync, wf4(:,10), wf8(:,28), A(:,28), n3(:,118), t3x32(:,:,28), nhel, den(10))
    call cont_VV(nsync, wf4(:,10), wf8(:,29), A(:,29), n3(:,119), t3x32(:,:,29), nhel, den(10))
    call cont_VV(nsync, wf4(:,10), wf8(:,30), A(:,30), n3(:,120), t3x32(:,:,30), nhel, den(10))
    call cont_VV(nsync, wf4(:,11), wf8(:,31), A(:,31), n3(:,121), t3x32(:,:,31), nhel, den(11))
    call cont_VV(nsync, wf4(:,11), wf8(:,32), A(:,32), n3(:,122), t3x32(:,:,32), nhel, den(11))
    call cont_VV(nsync, wf4(:,11), wf8(:,33), A(:,33), n3(:,123), t3x32(:,:,33), nhel, den(11))
    call cont_VV(nsync, wf4(:,12), wf8(:,34), A(:,34), n3(:,124), t3x32(:,:,34), nhel, den(12))
    call cont_VV(nsync, wf4(:,12), wf8(:,35), A(:,35), n3(:,125), t3x32(:,:,35), nhel, den(12))
    call cont_VV(nsync, wf4(:,12), wf8(:,36), A(:,36), n3(:,126), t3x32(:,:,36), nhel, den(12))
    call cont_VV(nsync, wf4(:,13), wf8(:,37), A(:,37), n3(:,127), t3x32(:,:,37), nhel, den(13))
    call cont_VV(nsync, wf4(:,13), wf8(:,38), A(:,38), n3(:,128), t3x32(:,:,38), nhel, den(13))
    call cont_VV(nsync, wf4(:,13), wf8(:,39), A(:,39), n3(:,129), t3x32(:,:,39), nhel, den(13))
    call cont_VV(nsync, wf4(:,14), wf8(:,40), A(:,40), n3(:,130), t3x32(:,:,40), nhel, den(14))
    call cont_VV(nsync, wf4(:,14), wf8(:,41), A(:,41), n3(:,131), t3x32(:,:,41), nhel, den(14))
    call cont_VV(nsync, wf4(:,14), wf8(:,42), A(:,42), n3(:,132), t3x32(:,:,42), nhel, den(14))
    call cont_VV(nsync, wf4(:,15), wf8(:,43), A(:,43), n3(:,133), t3x32(:,:,43), nhel, den(15))
    call cont_VV(nsync, wf4(:,15), wf8(:,44), A(:,44), n3(:,134), t3x32(:,:,44), nhel, den(15))
    call cont_VV(nsync, wf4(:,15), wf8(:,45), A(:,45), n3(:,135), t3x32(:,:,45), nhel, den(15))
    call cont_VV(nsync, wf4(:,16), wf8(:,46), A(:,46), n3(:,136), t3x32(:,:,46), nhel, den(16))
    call cont_VV(nsync, wf4(:,16), wf8(:,47), A(:,47), n3(:,137), t3x32(:,:,47), nhel, den(16))
    call cont_VV(nsync, wf4(:,16), wf8(:,48), A(:,48), n3(:,138), t3x32(:,:,48), nhel, den(16))
    call cont_VV(nsync, wf4(:,17), wf8(:,49), A(:,49), n3(:,139), t3x32(:,:,49), nhel, den(17))
    call cont_VV(nsync, wf4(:,17), wf8(:,50), A(:,50), n3(:,140), t3x32(:,:,50), nhel, den(17))
    call cont_VV(nsync, wf4(:,17), wf8(:,51), A(:,51), n3(:,141), t3x32(:,:,51), nhel, den(17))
    call cont_VV(nsync, wf4(:,18), wf8(:,52), A(:,52), n3(:,142), t3x32(:,:,52), nhel, den(18))
    call cont_VV(nsync, wf4(:,18), wf8(:,53), A(:,53), n3(:,143), t3x32(:,:,53), nhel, den(18))
    call cont_VV(nsync, wf4(:,18), wf8(:,54), A(:,54), n3(:,144), t3x32(:,:,54), nhel, den(18))
    call cont_VV(nsync, wf4(:,19), wf8(:,55), A(:,55), n3(:,145), t3x32(:,:,55), nhel, den(19))
    call cont_VV(nsync, wf4(:,19), wf8(:,56), A(:,56), n3(:,146), t3x32(:,:,56), nhel, den(19))
    call cont_VV(nsync, wf4(:,19), wf8(:,57), A(:,57), n3(:,147), t3x32(:,:,57), nhel, den(19))
    call cont_VV(nsync, wf4(:,20), wf8(:,58), A(:,58), n3(:,148), t3x32(:,:,58), nhel, den(20))
    call cont_VV(nsync, wf4(:,20), wf8(:,59), A(:,59), n3(:,149), t3x32(:,:,59), nhel, den(20))
    call cont_VV(nsync, wf4(:,20), wf8(:,60), A(:,60), n3(:,150), t3x32(:,:,60), nhel, den(20))
    call cont_VV(nsync, wf4(:,15), wf8(:,61), A(:,61), n3(:,151), t3x32(:,:,61), nhel, den(22))
    call cont_VV(nsync, wf4(:,15), wf8(:,62), A(:,62), n3(:,152), t3x32(:,:,62), nhel, den(22))
    call cont_VV(nsync, wf4(:,15), wf8(:,63), A(:,63), n3(:,153), t3x32(:,:,63), nhel, den(22))
    call cont_VV(nsync, wf4(:,16), wf8(:,64), A(:,64), n3(:,154), t3x32(:,:,64), nhel, den(23))
    call cont_VV(nsync, wf4(:,16), wf8(:,65), A(:,65), n3(:,155), t3x32(:,:,65), nhel, den(23))
    call cont_VV(nsync, wf4(:,16), wf8(:,66), A(:,66), n3(:,156), t3x32(:,:,66), nhel, den(23))
    call cont_VV(nsync, wf4(:,17), wf8(:,67), A(:,67), n3(:,157), t3x32(:,:,67), nhel, den(24))
    call cont_VV(nsync, wf4(:,17), wf8(:,68), A(:,68), n3(:,158), t3x32(:,:,68), nhel, den(24))
    call cont_VV(nsync, wf4(:,17), wf8(:,69), A(:,69), n3(:,159), t3x32(:,:,69), nhel, den(24))
    call cont_VV(nsync, wf8(:,1), wf4(:,21), A(:,70), n3(:,160), t3x32(:,:,70), nhel, den(26))
    call cont_VV(nsync, wf8(:,2), wf4(:,21), A(:,71), n3(:,161), t3x32(:,:,71), nhel, den(26))
    call cont_VV(nsync, wf8(:,3), wf4(:,21), A(:,72), n3(:,162), t3x32(:,:,72), nhel, den(26))
    call cont_VV(nsync, wf4(:,18), wf8(:,70), A(:,73), n3(:,163), t3x32(:,:,73), nhel, den(27))
    call cont_VV(nsync, wf4(:,18), wf8(:,71), A(:,74), n3(:,164), t3x32(:,:,74), nhel, den(27))
    call cont_VV(nsync, wf4(:,18), wf8(:,72), A(:,75), n3(:,165), t3x32(:,:,75), nhel, den(27))
    call cont_VV(nsync, wf4(:,19), wf8(:,73), A(:,76), n3(:,166), t3x32(:,:,76), nhel, den(28))
    call cont_VV(nsync, wf4(:,19), wf8(:,74), A(:,77), n3(:,167), t3x32(:,:,77), nhel, den(28))
    call cont_VV(nsync, wf4(:,19), wf8(:,75), A(:,78), n3(:,168), t3x32(:,:,78), nhel, den(28))
    call cont_VV(nsync, wf8(:,4), wf4(:,22), A(:,79), n3(:,169), t3x32(:,:,79), nhel, den(30))
    call cont_VV(nsync, wf8(:,5), wf4(:,22), A(:,80), n3(:,170), t3x32(:,:,80), nhel, den(30))
    call cont_VV(nsync, wf8(:,6), wf4(:,22), A(:,81), n3(:,171), t3x32(:,:,81), nhel, den(30))
    call cont_VV(nsync, wf4(:,20), wf8(:,76), A(:,82), n3(:,172), t3x32(:,:,82), nhel, den(31))
    call cont_VV(nsync, wf4(:,20), wf8(:,77), A(:,83), n3(:,173), t3x32(:,:,83), nhel, den(31))
    call cont_VV(nsync, wf4(:,20), wf8(:,78), A(:,84), n3(:,174), t3x32(:,:,84), nhel, den(31))
    call cont_VV(nsync, wf8(:,7), wf4(:,23), A(:,85), n3(:,175), t3x32(:,:,85), nhel, den(33))
    call cont_VV(nsync, wf8(:,8), wf4(:,23), A(:,86), n3(:,176), t3x32(:,:,86), nhel, den(33))
    call cont_VV(nsync, wf8(:,9), wf4(:,23), A(:,87), n3(:,177), t3x32(:,:,87), nhel, den(33))
    call cont_VV(nsync, wf8(:,10), wf4(:,24), A(:,88), n3(:,178), t3x32(:,:,88), nhel, den(35))
    call cont_VV(nsync, wf8(:,11), wf4(:,24), A(:,89), n3(:,179), t3x32(:,:,89), nhel, den(35))
    call cont_VV(nsync, wf8(:,12), wf4(:,24), A(:,90), n3(:,180), t3x32(:,:,90), nhel, den(35))
    call cont_VV(nsync, wf4(:,12), wf8(:,79), A(:,91), n3(:,181), t3x32(:,:,91), nhel, den(37))
    call cont_VV(nsync, wf4(:,12), wf8(:,80), A(:,92), n3(:,182), t3x32(:,:,92), nhel, den(37))
    call cont_VV(nsync, wf4(:,12), wf8(:,81), A(:,93), n3(:,183), t3x32(:,:,93), nhel, den(37))
    call cont_VV(nsync, wf4(:,13), wf8(:,82), A(:,94), n3(:,184), t3x32(:,:,94), nhel, den(38))
    call cont_VV(nsync, wf4(:,13), wf8(:,83), A(:,95), n3(:,185), t3x32(:,:,95), nhel, den(38))
    call cont_VV(nsync, wf4(:,13), wf8(:,84), A(:,96), n3(:,186), t3x32(:,:,96), nhel, den(38))
    call cont_VV(nsync, wf4(:,14), wf8(:,85), A(:,97), n3(:,187), t3x32(:,:,97), nhel, den(39))
    call cont_VV(nsync, wf4(:,14), wf8(:,86), A(:,98), n3(:,188), t3x32(:,:,98), nhel, den(39))
    call cont_VV(nsync, wf4(:,14), wf8(:,87), A(:,99), n3(:,189), t3x32(:,:,99), nhel, den(39))
    call cont_VV(nsync, wf8(:,1), wf4(:,25), A(:,100), n3(:,190), t3x32(:,:,100), nhel, den(40))
    call cont_VV(nsync, wf8(:,2), wf4(:,25), A(:,101), n3(:,191), t3x32(:,:,101), nhel, den(40))
    call cont_VV(nsync, wf8(:,3), wf4(:,25), A(:,102), n3(:,192), t3x32(:,:,102), nhel, den(40))
    call cont_VV(nsync, wf4(:,11), wf8(:,88), A(:,103), n3(:,193), t3x32(:,:,103), nhel, den(42))
    call cont_VV(nsync, wf4(:,11), wf8(:,89), A(:,104), n3(:,194), t3x32(:,:,104), nhel, den(42))
    call cont_VV(nsync, wf4(:,11), wf8(:,90), A(:,105), n3(:,195), t3x32(:,:,105), nhel, den(42))
    call cont_VV(nsync, wf4(:,11), wf8(:,91), A(:,106), n3(:,196), t3x32(:,:,106), nhel, den(44))
    call cont_VV(nsync, wf4(:,11), wf8(:,92), A(:,107), n3(:,197), t3x32(:,:,107), nhel, den(44))
    call cont_VV(nsync, wf4(:,11), wf8(:,93), A(:,108), n3(:,198), t3x32(:,:,108), nhel, den(44))
    call cont_VV(nsync, wf4(:,11), wf8(:,94), A(:,109), n3(:,199), t3x32(:,:,109), nhel, den(46))
    call cont_VV(nsync, wf4(:,11), wf8(:,95), A(:,110), n3(:,200), t3x32(:,:,110), nhel, den(46))
    call cont_VV(nsync, wf4(:,11), wf8(:,96), A(:,111), n3(:,201), t3x32(:,:,111), nhel, den(46))
    call cont_VV(nsync, wf8(:,1), wf4(:,26), A(:,112), n3(:,202), t3x32(:,:,112), nhel, den(47))
    call cont_VV(nsync, wf8(:,2), wf4(:,26), A(:,113), n3(:,203), t3x32(:,:,113), nhel, den(47))
    call cont_VV(nsync, wf8(:,3), wf4(:,26), A(:,114), n3(:,204), t3x32(:,:,114), nhel, den(47))
    call cont_VV(nsync, wf4(:,13), wf8(:,97), A(:,115), n3(:,205), t3x32(:,:,115), nhel, den(48))
    call cont_VV(nsync, wf4(:,13), wf8(:,98), A(:,116), n3(:,206), t3x32(:,:,116), nhel, den(48))
    call cont_VV(nsync, wf4(:,13), wf8(:,99), A(:,117), n3(:,207), t3x32(:,:,117), nhel, den(48))
    call cont_VV(nsync, wf4(:,14), wf8(:,100), A(:,118), n3(:,208), t3x32(:,:,118), nhel, den(49))
    call cont_VV(nsync, wf4(:,14), wf8(:,101), A(:,119), n3(:,209), t3x32(:,:,119), nhel, den(49))
    call cont_VV(nsync, wf4(:,14), wf8(:,102), A(:,120), n3(:,210), t3x32(:,:,120), nhel, den(49))
    call cont_VV(nsync, wf8(:,4), wf4(:,27), A(:,121), n3(:,211), t3x32(:,:,121), nhel, den(50))
    call cont_VV(nsync, wf8(:,5), wf4(:,27), A(:,122), n3(:,212), t3x32(:,:,122), nhel, den(50))
    call cont_VV(nsync, wf8(:,6), wf4(:,27), A(:,123), n3(:,213), t3x32(:,:,123), nhel, den(50))
    call cont_VV(nsync, wf4(:,12), wf8(:,103), A(:,124), n3(:,214), t3x32(:,:,124), nhel, den(51))
    call cont_VV(nsync, wf4(:,12), wf8(:,104), A(:,125), n3(:,215), t3x32(:,:,125), nhel, den(51))
    call cont_VV(nsync, wf4(:,12), wf8(:,105), A(:,126), n3(:,216), t3x32(:,:,126), nhel, den(51))
    call cont_VV(nsync, wf4(:,12), wf8(:,106), A(:,127), n3(:,217), t3x32(:,:,127), nhel, den(52))
    call cont_VV(nsync, wf4(:,12), wf8(:,107), A(:,128), n3(:,218), t3x32(:,:,128), nhel, den(52))
    call cont_VV(nsync, wf4(:,12), wf8(:,108), A(:,129), n3(:,219), t3x32(:,:,129), nhel, den(52))
    call cont_VV(nsync, wf8(:,4), wf4(:,28), A(:,130), n3(:,220), t3x32(:,:,130), nhel, den(53))
    call cont_VV(nsync, wf8(:,5), wf4(:,28), A(:,131), n3(:,221), t3x32(:,:,131), nhel, den(53))
    call cont_VV(nsync, wf8(:,6), wf4(:,28), A(:,132), n3(:,222), t3x32(:,:,132), nhel, den(53))
    call cont_VV(nsync, wf4(:,14), wf8(:,109), A(:,133), n3(:,223), t3x32(:,:,133), nhel, den(54))
    call cont_VV(nsync, wf4(:,14), wf8(:,110), A(:,134), n3(:,224), t3x32(:,:,134), nhel, den(54))
    call cont_VV(nsync, wf4(:,14), wf8(:,111), A(:,135), n3(:,225), t3x32(:,:,135), nhel, den(54))
    call cont_VV(nsync, wf8(:,7), wf4(:,29), A(:,136), n3(:,226), t3x32(:,:,136), nhel, den(55))
    call cont_VV(nsync, wf8(:,8), wf4(:,29), A(:,137), n3(:,227), t3x32(:,:,137), nhel, den(55))
    call cont_VV(nsync, wf8(:,9), wf4(:,29), A(:,138), n3(:,228), t3x32(:,:,138), nhel, den(55))
    call cont_VV(nsync, wf4(:,13), wf8(:,112), A(:,139), n3(:,229), t3x32(:,:,139), nhel, den(56))
    call cont_VV(nsync, wf4(:,13), wf8(:,113), A(:,140), n3(:,230), t3x32(:,:,140), nhel, den(56))
    call cont_VV(nsync, wf4(:,13), wf8(:,114), A(:,141), n3(:,231), t3x32(:,:,141), nhel, den(56))
    call cont_VV(nsync, wf8(:,7), wf4(:,30), A(:,142), n3(:,232), t3x32(:,:,142), nhel, den(57))
    call cont_VV(nsync, wf8(:,8), wf4(:,30), A(:,143), n3(:,233), t3x32(:,:,143), nhel, den(57))
    call cont_VV(nsync, wf8(:,9), wf4(:,30), A(:,144), n3(:,234), t3x32(:,:,144), nhel, den(57))
    call cont_VV(nsync, wf8(:,10), wf4(:,31), A(:,145), n3(:,235), t3x32(:,:,145), nhel, den(58))
    call cont_VV(nsync, wf8(:,11), wf4(:,31), A(:,146), n3(:,236), t3x32(:,:,146), nhel, den(58))
    call cont_VV(nsync, wf8(:,12), wf4(:,31), A(:,147), n3(:,237), t3x32(:,:,147), nhel, den(58))
    call cont_VV(nsync, wf8(:,10), wf4(:,32), A(:,148), n3(:,238), t3x32(:,:,148), nhel, den(59))
    call cont_VV(nsync, wf8(:,11), wf4(:,32), A(:,149), n3(:,239), t3x32(:,:,149), nhel, den(59))
    call cont_VV(nsync, wf8(:,12), wf4(:,32), A(:,150), n3(:,240), t3x32(:,:,150), nhel, den(59))
    call cont_VV(nsync, wf4(:,18), wf8(:,115), A(:,151), n3(:,241), t3x32(:,:,151), nhel, den(60))
    call cont_VV(nsync, wf4(:,18), wf8(:,116), A(:,152), n3(:,242), t3x32(:,:,152), nhel, den(60))
    call cont_VV(nsync, wf4(:,18), wf8(:,117), A(:,153), n3(:,243), t3x32(:,:,153), nhel, den(60))
    call cont_VV(nsync, wf4(:,19), wf8(:,118), A(:,154), n3(:,244), t3x32(:,:,154), nhel, den(61))
    call cont_VV(nsync, wf4(:,19), wf8(:,119), A(:,155), n3(:,245), t3x32(:,:,155), nhel, den(61))
    call cont_VV(nsync, wf4(:,19), wf8(:,120), A(:,156), n3(:,246), t3x32(:,:,156), nhel, den(61))
    call cont_VV(nsync, wf8(:,13), wf4(:,33), A(:,157), n3(:,247), t3x32(:,:,157), nhel, den(63))
    call cont_VV(nsync, wf8(:,14), wf4(:,33), A(:,158), n3(:,248), t3x32(:,:,158), nhel, den(63))
    call cont_VV(nsync, wf8(:,15), wf4(:,33), A(:,159), n3(:,249), t3x32(:,:,159), nhel, den(63))
    call cont_VV(nsync, wf4(:,20), wf8(:,121), A(:,160), n3(:,250), t3x32(:,:,160), nhel, den(64))
    call cont_VV(nsync, wf4(:,20), wf8(:,122), A(:,161), n3(:,251), t3x32(:,:,161), nhel, den(64))
    call cont_VV(nsync, wf4(:,20), wf8(:,123), A(:,162), n3(:,252), t3x32(:,:,162), nhel, den(64))
    call cont_VV(nsync, wf8(:,16), wf4(:,34), A(:,163), n3(:,253), t3x32(:,:,163), nhel, den(66))
    call cont_VV(nsync, wf8(:,17), wf4(:,34), A(:,164), n3(:,254), t3x32(:,:,164), nhel, den(66))
    call cont_VV(nsync, wf8(:,18), wf4(:,34), A(:,165), n3(:,255), t3x32(:,:,165), nhel, den(66))
    call cont_VV(nsync, wf8(:,19), wf4(:,35), A(:,166), n3(:,256), t3x32(:,:,166), nhel, den(68))
    call cont_VV(nsync, wf8(:,20), wf4(:,35), A(:,167), n3(:,257), t3x32(:,:,167), nhel, den(68))
    call cont_VV(nsync, wf8(:,21), wf4(:,35), A(:,168), n3(:,258), t3x32(:,:,168), nhel, den(68))
    call cont_VV(nsync, wf4(:,16), wf8(:,124), A(:,169), n3(:,259), t3x32(:,:,169), nhel, den(69))
    call cont_VV(nsync, wf4(:,16), wf8(:,125), A(:,170), n3(:,260), t3x32(:,:,170), nhel, den(69))
    call cont_VV(nsync, wf4(:,16), wf8(:,126), A(:,171), n3(:,261), t3x32(:,:,171), nhel, den(69))
    call cont_VV(nsync, wf4(:,17), wf8(:,127), A(:,172), n3(:,262), t3x32(:,:,172), nhel, den(70))
    call cont_VV(nsync, wf4(:,17), wf8(:,128), A(:,173), n3(:,263), t3x32(:,:,173), nhel, den(70))
    call cont_VV(nsync, wf4(:,17), wf8(:,129), A(:,174), n3(:,264), t3x32(:,:,174), nhel, den(70))
    call cont_VV(nsync, wf8(:,13), wf4(:,36), A(:,175), n3(:,265), t3x32(:,:,175), nhel, den(71))
    call cont_VV(nsync, wf8(:,14), wf4(:,36), A(:,176), n3(:,266), t3x32(:,:,176), nhel, den(71))
    call cont_VV(nsync, wf8(:,15), wf4(:,36), A(:,177), n3(:,267), t3x32(:,:,177), nhel, den(71))
    call cont_VV(nsync, wf4(:,15), wf8(:,130), A(:,178), n3(:,268), t3x32(:,:,178), nhel, den(72))
    call cont_VV(nsync, wf4(:,15), wf8(:,131), A(:,179), n3(:,269), t3x32(:,:,179), nhel, den(72))
    call cont_VV(nsync, wf4(:,15), wf8(:,132), A(:,180), n3(:,270), t3x32(:,:,180), nhel, den(72))
    call cont_VV(nsync, wf4(:,15), wf8(:,133), A(:,181), n3(:,271), t3x32(:,:,181), nhel, den(73))
    call cont_VV(nsync, wf4(:,15), wf8(:,134), A(:,182), n3(:,272), t3x32(:,:,182), nhel, den(73))
    call cont_VV(nsync, wf4(:,15), wf8(:,135), A(:,183), n3(:,273), t3x32(:,:,183), nhel, den(73))
    call cont_VV(nsync, wf8(:,13), wf4(:,37), A(:,184), n3(:,274), t3x32(:,:,184), nhel, den(74))
    call cont_VV(nsync, wf8(:,14), wf4(:,37), A(:,185), n3(:,275), t3x32(:,:,185), nhel, den(74))
    call cont_VV(nsync, wf8(:,15), wf4(:,37), A(:,186), n3(:,276), t3x32(:,:,186), nhel, den(74))
    call cont_VV(nsync, wf4(:,17), wf8(:,136), A(:,187), n3(:,277), t3x32(:,:,187), nhel, den(75))
    call cont_VV(nsync, wf4(:,17), wf8(:,137), A(:,188), n3(:,278), t3x32(:,:,188), nhel, den(75))
    call cont_VV(nsync, wf4(:,17), wf8(:,138), A(:,189), n3(:,279), t3x32(:,:,189), nhel, den(75))
    call cont_VV(nsync, wf8(:,16), wf4(:,38), A(:,190), n3(:,280), t3x32(:,:,190), nhel, den(76))
    call cont_VV(nsync, wf8(:,17), wf4(:,38), A(:,191), n3(:,281), t3x32(:,:,191), nhel, den(76))
    call cont_VV(nsync, wf8(:,18), wf4(:,38), A(:,192), n3(:,282), t3x32(:,:,192), nhel, den(76))
    call cont_VV(nsync, wf4(:,16), wf8(:,139), A(:,193), n3(:,283), t3x32(:,:,193), nhel, den(77))
    call cont_VV(nsync, wf4(:,16), wf8(:,140), A(:,194), n3(:,284), t3x32(:,:,194), nhel, den(77))
    call cont_VV(nsync, wf4(:,16), wf8(:,141), A(:,195), n3(:,285), t3x32(:,:,195), nhel, den(77))
    call cont_VV(nsync, wf8(:,16), wf4(:,39), A(:,196), n3(:,286), t3x32(:,:,196), nhel, den(78))
    call cont_VV(nsync, wf8(:,17), wf4(:,39), A(:,197), n3(:,287), t3x32(:,:,197), nhel, den(78))
    call cont_VV(nsync, wf8(:,18), wf4(:,39), A(:,198), n3(:,288), t3x32(:,:,198), nhel, den(78))
    call cont_VV(nsync, wf8(:,19), wf4(:,40), A(:,199), n3(:,289), t3x32(:,:,199), nhel, den(79))
    call cont_VV(nsync, wf8(:,20), wf4(:,40), A(:,200), n3(:,290), t3x32(:,:,200), nhel, den(79))
    call cont_VV(nsync, wf8(:,21), wf4(:,40), A(:,201), n3(:,291), t3x32(:,:,201), nhel, den(79))
    call cont_VV(nsync, wf8(:,19), wf4(:,41), A(:,202), n3(:,292), t3x32(:,:,202), nhel, den(80))
    call cont_VV(nsync, wf8(:,20), wf4(:,41), A(:,203), n3(:,293), t3x32(:,:,203), nhel, den(80))
    call cont_VV(nsync, wf8(:,21), wf4(:,41), A(:,204), n3(:,294), t3x32(:,:,204), nhel, den(80))
    call cont_VV(nsync, wf4(:,20), wf8(:,142), A(:,205), n3(:,295), t3x32(:,:,205), nhel, den(81))
    call cont_VV(nsync, wf4(:,20), wf8(:,143), A(:,206), n3(:,296), t3x32(:,:,206), nhel, den(81))
    call cont_VV(nsync, wf4(:,20), wf8(:,144), A(:,207), n3(:,297), t3x32(:,:,207), nhel, den(81))
    call cont_VV(nsync, wf8(:,22), wf4(:,42), A(:,208), n3(:,298), t3x32(:,:,208), nhel, den(83))
    call cont_VV(nsync, wf8(:,23), wf4(:,42), A(:,209), n3(:,299), t3x32(:,:,209), nhel, den(83))
    call cont_VV(nsync, wf8(:,24), wf4(:,42), A(:,210), n3(:,300), t3x32(:,:,210), nhel, den(83))
    call cont_VV(nsync, wf8(:,25), wf4(:,43), A(:,211), n3(:,301), t3x32(:,:,211), nhel, den(85))
    call cont_VV(nsync, wf8(:,26), wf4(:,43), A(:,212), n3(:,302), t3x32(:,:,212), nhel, den(85))
    call cont_VV(nsync, wf8(:,27), wf4(:,43), A(:,213), n3(:,303), t3x32(:,:,213), nhel, den(85))
    call cont_VV(nsync, wf4(:,19), wf8(:,145), A(:,214), n3(:,304), t3x32(:,:,214), nhel, den(86))
    call cont_VV(nsync, wf4(:,19), wf8(:,146), A(:,215), n3(:,305), t3x32(:,:,215), nhel, den(86))
    call cont_VV(nsync, wf4(:,19), wf8(:,147), A(:,216), n3(:,306), t3x32(:,:,216), nhel, den(86))
    call cont_VV(nsync, wf8(:,22), wf4(:,44), A(:,217), n3(:,307), t3x32(:,:,217), nhel, den(87))
    call cont_VV(nsync, wf8(:,23), wf4(:,44), A(:,218), n3(:,308), t3x32(:,:,218), nhel, den(87))
    call cont_VV(nsync, wf8(:,24), wf4(:,44), A(:,219), n3(:,309), t3x32(:,:,219), nhel, den(87))
    call cont_VV(nsync, wf4(:,18), wf8(:,148), A(:,220), n3(:,310), t3x32(:,:,220), nhel, den(88))
    call cont_VV(nsync, wf4(:,18), wf8(:,149), A(:,221), n3(:,311), t3x32(:,:,221), nhel, den(88))
    call cont_VV(nsync, wf4(:,18), wf8(:,150), A(:,222), n3(:,312), t3x32(:,:,222), nhel, den(88))
    call cont_VV(nsync, wf8(:,22), wf4(:,45), A(:,223), n3(:,313), t3x32(:,:,223), nhel, den(89))
    call cont_VV(nsync, wf8(:,23), wf4(:,45), A(:,224), n3(:,314), t3x32(:,:,224), nhel, den(89))
    call cont_VV(nsync, wf8(:,24), wf4(:,45), A(:,225), n3(:,315), t3x32(:,:,225), nhel, den(89))
    call cont_VV(nsync, wf8(:,25), wf4(:,46), A(:,226), n3(:,316), t3x32(:,:,226), nhel, den(90))
    call cont_VV(nsync, wf8(:,26), wf4(:,46), A(:,227), n3(:,317), t3x32(:,:,227), nhel, den(90))
    call cont_VV(nsync, wf8(:,27), wf4(:,46), A(:,228), n3(:,318), t3x32(:,:,228), nhel, den(90))
    call cont_VV(nsync, wf8(:,25), wf4(:,47), A(:,229), n3(:,319), t3x32(:,:,229), nhel, den(91))
    call cont_VV(nsync, wf8(:,26), wf4(:,47), A(:,230), n3(:,320), t3x32(:,:,230), nhel, den(91))
    call cont_VV(nsync, wf8(:,27), wf4(:,47), A(:,231), n3(:,321), t3x32(:,:,231), nhel, den(91))
    call cont_VV(nsync, wf8(:,28), wf4(:,48), A(:,232), n3(:,322), t3x32(:,:,232), nhel, den(93))
    call cont_VV(nsync, wf8(:,29), wf4(:,48), A(:,233), n3(:,323), t3x32(:,:,233), nhel, den(93))
    call cont_VV(nsync, wf8(:,30), wf4(:,48), A(:,234), n3(:,324), t3x32(:,:,234), nhel, den(93))
    call cont_VV(nsync, wf8(:,28), wf4(:,49), A(:,235), n3(:,325), t3x32(:,:,235), nhel, den(94))
    call cont_VV(nsync, wf8(:,29), wf4(:,49), A(:,236), n3(:,326), t3x32(:,:,236), nhel, den(94))
    call cont_VV(nsync, wf8(:,30), wf4(:,49), A(:,237), n3(:,327), t3x32(:,:,237), nhel, den(94))
    call cont_VV(nsync, wf8(:,28), wf4(:,50), A(:,238), n3(:,328), t3x32(:,:,238), nhel, den(95))
    call cont_VV(nsync, wf8(:,29), wf4(:,50), A(:,239), n3(:,329), t3x32(:,:,239), nhel, den(95))
    call cont_VV(nsync, wf8(:,30), wf4(:,50), A(:,240), n3(:,330), t3x32(:,:,240), nhel, den(95))
    call cont_VV(nsync, wf4(:,18), wf8(:,151), A(:,241), n3(:,331), t3x32(:,:,241), nhel, den(96))
    call cont_VV(nsync, wf4(:,19), wf8(:,152), A(:,242), n3(:,332), t3x32(:,:,242), nhel, den(97))
    call cont_VV(nsync, wf4(:,10), wf8(:,153), A(:,243), n3(:,333), t3x32(:,:,243), nhel, den(98))
    call cont_VV(nsync, wf4(:,20), wf8(:,154), A(:,244), n3(:,334), t3x32(:,:,244), nhel, den(99))
    call cont_VV(nsync, wf4(:,9), wf8(:,155), A(:,245), n3(:,335), t3x32(:,:,245), nhel, den(100))
    call cont_VV(nsync, wf4(:,8), wf8(:,156), A(:,246), n3(:,336), t3x32(:,:,246), nhel, den(101))
    call cont_VV(nsync, wf4(:,16), wf8(:,157), A(:,247), n3(:,337), t3x32(:,:,247), nhel, den(102))
    call cont_VV(nsync, wf4(:,17), wf8(:,158), A(:,248), n3(:,338), t3x32(:,:,248), nhel, den(103))
    call cont_VV(nsync, wf4(:,10), wf8(:,159), A(:,249), n3(:,339), t3x32(:,:,249), nhel, den(104))
    call cont_VV(nsync, wf4(:,15), wf8(:,160), A(:,250), n3(:,340), t3x32(:,:,250), nhel, den(105))
    call cont_VV(nsync, wf4(:,15), wf8(:,161), A(:,251), n3(:,341), t3x32(:,:,251), nhel, den(106))
    call cont_VV(nsync, wf4(:,10), wf8(:,162), A(:,252), n3(:,342), t3x32(:,:,252), nhel, den(107))
    call cont_VV(nsync, wf4(:,17), wf8(:,163), A(:,253), n3(:,343), t3x32(:,:,253), nhel, den(108))
    call cont_VV(nsync, wf4(:,9), wf8(:,164), A(:,254), n3(:,344), t3x32(:,:,254), nhel, den(109))
    call cont_VV(nsync, wf4(:,16), wf8(:,165), A(:,255), n3(:,345), t3x32(:,:,255), nhel, den(110))
    call cont_VV(nsync, wf4(:,9), wf8(:,166), A(:,256), n3(:,346), t3x32(:,:,256), nhel, den(111))
    call cont_VV(nsync, wf4(:,8), wf8(:,167), A(:,257), n3(:,347), t3x32(:,:,257), nhel, den(112))
    call cont_VV(nsync, wf4(:,8), wf8(:,168), A(:,258), n3(:,348), t3x32(:,:,258), nhel, den(113))
    call cont_VV(nsync, wf4(:,20), wf8(:,169), A(:,259), n3(:,349), t3x32(:,:,259), nhel, den(114))
    call cont_VV(nsync, wf4(:,7), wf8(:,170), A(:,260), n3(:,350), t3x32(:,:,260), nhel, den(115))
    call cont_VV(nsync, wf4(:,6), wf8(:,171), A(:,261), n3(:,351), t3x32(:,:,261), nhel, den(116))
    call cont_VV(nsync, wf4(:,19), wf8(:,172), A(:,262), n3(:,352), t3x32(:,:,262), nhel, den(117))
    call cont_VV(nsync, wf4(:,7), wf8(:,173), A(:,263), n3(:,353), t3x32(:,:,263), nhel, den(118))
    call cont_VV(nsync, wf4(:,18), wf8(:,174), A(:,264), n3(:,354), t3x32(:,:,264), nhel, den(119))
    call cont_VV(nsync, wf4(:,7), wf8(:,175), A(:,265), n3(:,355), t3x32(:,:,265), nhel, den(120))
    call cont_VV(nsync, wf4(:,6), wf8(:,176), A(:,266), n3(:,356), t3x32(:,:,266), nhel, den(121))
    call cont_VV(nsync, wf4(:,6), wf8(:,177), A(:,267), n3(:,357), t3x32(:,:,267), nhel, den(122))
    call cont_VV(nsync, wf4(:,5), wf8(:,178), A(:,268), n3(:,358), t3x32(:,:,268), nhel, den(123))
    call cont_VV(nsync, wf4(:,5), wf8(:,179), A(:,269), n3(:,359), t3x32(:,:,269), nhel, den(124))
    call cont_VV(nsync, wf4(:,5), wf8(:,180), A(:,270), n3(:,360), t3x32(:,:,270), nhel, den(125))
    call cont_VV(nsync, wf4(:,20), wf8(:,181), A(:,271), n3(:,361), t3x32(:,:,271), nhel, den(126))
    call cont_VV(nsync, wf4(:,4), wf8(:,182), A(:,272), n3(:,362), t3x32(:,:,272), nhel, den(127))
    call cont_VV(nsync, wf4(:,3), wf8(:,183), A(:,273), n3(:,363), t3x32(:,:,273), nhel, den(128))
    call cont_VV(nsync, wf4(:,19), wf8(:,184), A(:,274), n3(:,364), t3x32(:,:,274), nhel, den(129))
    call cont_VV(nsync, wf4(:,4), wf8(:,185), A(:,275), n3(:,365), t3x32(:,:,275), nhel, den(130))
    call cont_VV(nsync, wf4(:,18), wf8(:,186), A(:,276), n3(:,366), t3x32(:,:,276), nhel, den(131))
    call cont_VV(nsync, wf4(:,4), wf8(:,187), A(:,277), n3(:,367), t3x32(:,:,277), nhel, den(132))
    call cont_VV(nsync, wf4(:,3), wf8(:,188), A(:,278), n3(:,368), t3x32(:,:,278), nhel, den(133))
    call cont_VV(nsync, wf4(:,3), wf8(:,189), A(:,279), n3(:,369), t3x32(:,:,279), nhel, den(134))
    call cont_VV(nsync, wf4(:,2), wf8(:,190), A(:,280), n3(:,370), t3x32(:,:,280), nhel, den(135))
    call cont_VV(nsync, wf4(:,2), wf8(:,191), A(:,281), n3(:,371), t3x32(:,:,281), nhel, den(136))
    call cont_VV(nsync, wf4(:,2), wf8(:,192), A(:,282), n3(:,372), t3x32(:,:,282), nhel, den(137))
    call cont_VV(nsync, wf4(:,1), wf8(:,193), A(:,283), n3(:,373), t3x32(:,:,283), nhel, den(138))
    call cont_VV(nsync, wf4(:,1), wf8(:,194), A(:,284), n3(:,374), t3x32(:,:,284), nhel, den(139))
    call cont_VV(nsync, wf4(:,1), wf8(:,195), A(:,285), n3(:,375), t3x32(:,:,285), nhel, den(140))
    call cont_VV(nsync, wf4(:,20), wf8(:,196), A(:,286), n3(:,376), t3x32(:,:,286), nhel, den(141))
    call cont_VV(nsync, wf4(:,23), wf8(:,183), A(:,287), n3(:,377), t3x32(:,:,287), nhel, den(143))
    call cont_VV(nsync, wf4(:,24), wf8(:,182), A(:,288), n3(:,378), t3x32(:,:,288), nhel, den(145))
    call cont_VV(nsync, wf4(:,19), wf8(:,197), A(:,289), n3(:,379), t3x32(:,:,289), nhel, den(146))
    call cont_VV(nsync, wf4(:,22), wf8(:,190), A(:,290), n3(:,380), t3x32(:,:,290), nhel, den(148))
    call cont_VV(nsync, wf4(:,24), wf8(:,185), A(:,291), n3(:,381), t3x32(:,:,291), nhel, den(150))
    call cont_VV(nsync, wf4(:,18), wf8(:,198), A(:,292), n3(:,382), t3x32(:,:,292), nhel, den(151))
    call cont_VV(nsync, wf4(:,21), wf8(:,193), A(:,293), n3(:,383), t3x32(:,:,293), nhel, den(153))
    call cont_VV(nsync, wf4(:,24), wf8(:,187), A(:,294), n3(:,384), t3x32(:,:,294), nhel, den(155))
    call cont_VV(nsync, wf4(:,22), wf8(:,191), A(:,295), n3(:,385), t3x32(:,:,295), nhel, den(156))
    call cont_VV(nsync, wf4(:,23), wf8(:,188), A(:,296), n3(:,386), t3x32(:,:,296), nhel, den(158))
    call cont_VV(nsync, wf4(:,21), wf8(:,194), A(:,297), n3(:,387), t3x32(:,:,297), nhel, den(159))
    call cont_VV(nsync, wf4(:,23), wf8(:,189), A(:,298), n3(:,388), t3x32(:,:,298), nhel, den(161))
    call cont_VV(nsync, wf4(:,21), wf8(:,195), A(:,299), n3(:,389), t3x32(:,:,299), nhel, den(162))
    call cont_VV(nsync, wf4(:,22), wf8(:,192), A(:,300), n3(:,390), t3x32(:,:,300), nhel, den(164))
    call cont_VV(nsync, wf4(:,20), wf8(:,199), A(:,301), n3(:,391), t3x32(:,:,301), nhel, den(165))
    call cont_VV(nsync, wf4(:,34), wf8(:,171), A(:,302), n3(:,392), t3x32(:,:,302), nhel, den(167))
    call cont_VV(nsync, wf4(:,35), wf8(:,170), A(:,303), n3(:,393), t3x32(:,:,303), nhel, den(169))
    call cont_VV(nsync, wf4(:,19), wf8(:,200), A(:,304), n3(:,394), t3x32(:,:,304), nhel, den(170))
    call cont_VV(nsync, wf4(:,33), wf8(:,178), A(:,305), n3(:,395), t3x32(:,:,305), nhel, den(172))
    call cont_VV(nsync, wf4(:,35), wf8(:,173), A(:,306), n3(:,396), t3x32(:,:,306), nhel, den(174))
    call cont_VV(nsync, wf4(:,18), wf8(:,201), A(:,307), n3(:,397), t3x32(:,:,307), nhel, den(175))
    call cont_VV(nsync, wf4(:,25), wf8(:,193), A(:,308), n3(:,398), t3x32(:,:,308), nhel, den(177))
    call cont_VV(nsync, wf4(:,35), wf8(:,175), A(:,309), n3(:,399), t3x32(:,:,309), nhel, den(179))
    call cont_VV(nsync, wf4(:,33), wf8(:,179), A(:,310), n3(:,400), t3x32(:,:,310), nhel, den(180))
    call cont_VV(nsync, wf4(:,34), wf8(:,176), A(:,311), n3(:,401), t3x32(:,:,311), nhel, den(182))
    call cont_VV(nsync, wf4(:,25), wf8(:,194), A(:,312), n3(:,402), t3x32(:,:,312), nhel, den(183))
    call cont_VV(nsync, wf4(:,34), wf8(:,177), A(:,313), n3(:,403), t3x32(:,:,313), nhel, den(185))
    call cont_VV(nsync, wf4(:,25), wf8(:,195), A(:,314), n3(:,404), t3x32(:,:,314), nhel, den(186))
    call cont_VV(nsync, wf4(:,33), wf8(:,180), A(:,315), n3(:,405), t3x32(:,:,315), nhel, den(188))
    call cont_VV(nsync, wf4(:,20), wf8(:,202), A(:,316), n3(:,406), t3x32(:,:,316), nhel, den(189))
    call cont_VV(nsync, wf4(:,42), wf8(:,156), A(:,317), n3(:,407), t3x32(:,:,317), nhel, den(191))
    call cont_VV(nsync, wf4(:,43), wf8(:,155), A(:,318), n3(:,408), t3x32(:,:,318), nhel, den(193))
    call cont_VV(nsync, wf4(:,19), wf8(:,203), A(:,319), n3(:,409), t3x32(:,:,319), nhel, den(194))
    call cont_VV(nsync, wf4(:,44), wf8(:,156), A(:,320), n3(:,410), t3x32(:,:,320), nhel, den(196))
    call cont_VV(nsync, wf4(:,48), wf8(:,153), A(:,321), n3(:,411), t3x32(:,:,321), nhel, den(198))
    call cont_VV(nsync, wf4(:,18), wf8(:,204), A(:,322), n3(:,412), t3x32(:,:,322), nhel, den(199))
    call cont_VV(nsync, wf4(:,26), wf8(:,193), A(:,323), n3(:,413), t3x32(:,:,323), nhel, den(201))
    call cont_VV(nsync, wf4(:,45), wf8(:,156), A(:,324), n3(:,414), t3x32(:,:,324), nhel, den(203))
    call cont_VV(nsync, wf4(:,46), wf8(:,155), A(:,325), n3(:,415), t3x32(:,:,325), nhel, den(205))
    call cont_VV(nsync, wf4(:,49), wf8(:,153), A(:,326), n3(:,416), t3x32(:,:,326), nhel, den(206))
    call cont_VV(nsync, wf4(:,26), wf8(:,194), A(:,327), n3(:,417), t3x32(:,:,327), nhel, den(207))
    call cont_VV(nsync, wf4(:,47), wf8(:,155), A(:,328), n3(:,418), t3x32(:,:,328), nhel, den(209))
    call cont_VV(nsync, wf4(:,26), wf8(:,195), A(:,329), n3(:,419), t3x32(:,:,329), nhel, den(210))
    call cont_VV(nsync, wf4(:,50), wf8(:,153), A(:,330), n3(:,420), t3x32(:,:,330), nhel, den(212))
    call cont_VV(nsync, wf4(:,17), wf8(:,205), A(:,331), n3(:,421), t3x32(:,:,331), nhel, den(213))
    call cont_VV(nsync, wf4(:,36), wf8(:,178), A(:,332), n3(:,422), t3x32(:,:,332), nhel, den(215))
    call cont_VV(nsync, wf4(:,43), wf8(:,164), A(:,333), n3(:,423), t3x32(:,:,333), nhel, den(217))
    call cont_VV(nsync, wf4(:,16), wf8(:,206), A(:,334), n3(:,424), t3x32(:,:,334), nhel, den(218))
    call cont_VV(nsync, wf4(:,27), wf8(:,190), A(:,335), n3(:,425), t3x32(:,:,335), nhel, den(220))
    call cont_VV(nsync, wf4(:,43), wf8(:,166), A(:,336), n3(:,426), t3x32(:,:,336), nhel, den(222))
    call cont_VV(nsync, wf4(:,36), wf8(:,179), A(:,337), n3(:,427), t3x32(:,:,337), nhel, den(223))
    call cont_VV(nsync, wf4(:,42), wf8(:,167), A(:,338), n3(:,428), t3x32(:,:,338), nhel, den(225))
    call cont_VV(nsync, wf4(:,27), wf8(:,191), A(:,339), n3(:,429), t3x32(:,:,339), nhel, den(226))
    call cont_VV(nsync, wf4(:,42), wf8(:,168), A(:,340), n3(:,430), t3x32(:,:,340), nhel, den(228))
    call cont_VV(nsync, wf4(:,27), wf8(:,192), A(:,341), n3(:,431), t3x32(:,:,341), nhel, den(229))
    call cont_VV(nsync, wf4(:,36), wf8(:,180), A(:,342), n3(:,432), t3x32(:,:,342), nhel, den(230))
    call cont_VV(nsync, wf4(:,17), wf8(:,207), A(:,343), n3(:,433), t3x32(:,:,343), nhel, den(231))
    call cont_VV(nsync, wf4(:,38), wf8(:,171), A(:,344), n3(:,434), t3x32(:,:,344), nhel, den(233))
    call cont_VV(nsync, wf4(:,48), wf8(:,159), A(:,345), n3(:,435), t3x32(:,:,345), nhel, den(235))
    call cont_VV(nsync, wf4(:,16), wf8(:,208), A(:,346), n3(:,436), t3x32(:,:,346), nhel, den(236))
    call cont_VV(nsync, wf4(:,28), wf8(:,190), A(:,347), n3(:,437), t3x32(:,:,347), nhel, den(238))
    call cont_VV(nsync, wf4(:,39), wf8(:,171), A(:,348), n3(:,438), t3x32(:,:,348), nhel, den(240))
    call cont_VV(nsync, wf4(:,40), wf8(:,170), A(:,349), n3(:,439), t3x32(:,:,349), nhel, den(242))
    call cont_VV(nsync, wf4(:,49), wf8(:,159), A(:,350), n3(:,440), t3x32(:,:,350), nhel, den(243))
    call cont_VV(nsync, wf4(:,28), wf8(:,191), A(:,351), n3(:,441), t3x32(:,:,351), nhel, den(244))
    call cont_VV(nsync, wf4(:,41), wf8(:,170), A(:,352), n3(:,442), t3x32(:,:,352), nhel, den(246))
    call cont_VV(nsync, wf4(:,28), wf8(:,192), A(:,353), n3(:,443), t3x32(:,:,353), nhel, den(247))
    call cont_VV(nsync, wf4(:,50), wf8(:,159), A(:,354), n3(:,444), t3x32(:,:,354), nhel, den(248))
    call cont_VV(nsync, wf4(:,15), wf8(:,209), A(:,355), n3(:,445), t3x32(:,:,355), nhel, den(249))
    call cont_VV(nsync, wf4(:,29), wf8(:,183), A(:,356), n3(:,446), t3x32(:,:,356), nhel, den(251))
    call cont_VV(nsync, wf4(:,48), wf8(:,162), A(:,357), n3(:,447), t3x32(:,:,357), nhel, den(253))
    call cont_VV(nsync, wf4(:,15), wf8(:,210), A(:,358), n3(:,448), t3x32(:,:,358), nhel, den(254))
    call cont_VV(nsync, wf4(:,30), wf8(:,183), A(:,359), n3(:,449), t3x32(:,:,359), nhel, den(256))
    call cont_VV(nsync, wf4(:,37), wf8(:,178), A(:,360), n3(:,450), t3x32(:,:,360), nhel, den(258))
    call cont_VV(nsync, wf4(:,31), wf8(:,182), A(:,361), n3(:,451), t3x32(:,:,361), nhel, den(260))
    call cont_VV(nsync, wf4(:,49), wf8(:,162), A(:,362), n3(:,452), t3x32(:,:,362), nhel, den(261))
    call cont_VV(nsync, wf4(:,32), wf8(:,182), A(:,363), n3(:,453), t3x32(:,:,363), nhel, den(263))
    call cont_VV(nsync, wf4(:,37), wf8(:,179), A(:,364), n3(:,454), t3x32(:,:,364), nhel, den(264))
    call cont_VV(nsync, wf4(:,37), wf8(:,180), A(:,365), n3(:,455), t3x32(:,:,365), nhel, den(265))
    call cont_VV(nsync, wf4(:,50), wf8(:,162), A(:,366), n3(:,456), t3x32(:,:,366), nhel, den(266))
    call cont_VV(nsync, wf4(:,38), wf8(:,176), A(:,367), n3(:,457), t3x32(:,:,367), nhel, den(267))
    call cont_VV(nsync, wf4(:,44), wf8(:,167), A(:,368), n3(:,458), t3x32(:,:,368), nhel, den(268))
    call cont_VV(nsync, wf4(:,29), wf8(:,188), A(:,369), n3(:,459), t3x32(:,:,369), nhel, den(269))
    call cont_VV(nsync, wf4(:,44), wf8(:,168), A(:,370), n3(:,460), t3x32(:,:,370), nhel, den(270))
    call cont_VV(nsync, wf4(:,29), wf8(:,189), A(:,371), n3(:,461), t3x32(:,:,371), nhel, den(271))
    call cont_VV(nsync, wf4(:,38), wf8(:,177), A(:,372), n3(:,462), t3x32(:,:,372), nhel, den(272))
    call cont_VV(nsync, wf4(:,40), wf8(:,173), A(:,373), n3(:,463), t3x32(:,:,373), nhel, den(273))
    call cont_VV(nsync, wf4(:,46), wf8(:,164), A(:,374), n3(:,464), t3x32(:,:,374), nhel, den(274))
    call cont_VV(nsync, wf4(:,30), wf8(:,188), A(:,375), n3(:,465), t3x32(:,:,375), nhel, den(275))
    call cont_VV(nsync, wf4(:,41), wf8(:,173), A(:,376), n3(:,466), t3x32(:,:,376), nhel, den(276))
    call cont_VV(nsync, wf4(:,30), wf8(:,189), A(:,377), n3(:,467), t3x32(:,:,377), nhel, den(277))
    call cont_VV(nsync, wf4(:,47), wf8(:,164), A(:,378), n3(:,468), t3x32(:,:,378), nhel, den(278))
    call cont_VV(nsync, wf4(:,31), wf8(:,185), A(:,379), n3(:,469), t3x32(:,:,379), nhel, den(279))
    call cont_VV(nsync, wf4(:,46), wf8(:,166), A(:,380), n3(:,470), t3x32(:,:,380), nhel, den(280))
    call cont_VV(nsync, wf4(:,32), wf8(:,185), A(:,381), n3(:,471), t3x32(:,:,381), nhel, den(281))
    call cont_VV(nsync, wf4(:,39), wf8(:,176), A(:,382), n3(:,472), t3x32(:,:,382), nhel, den(282))
    call cont_VV(nsync, wf4(:,39), wf8(:,177), A(:,383), n3(:,473), t3x32(:,:,383), nhel, den(283))
    call cont_VV(nsync, wf4(:,47), wf8(:,166), A(:,384), n3(:,474), t3x32(:,:,384), nhel, den(284))
    call cont_VV(nsync, wf4(:,31), wf8(:,187), A(:,385), n3(:,475), t3x32(:,:,385), nhel, den(285))
    call cont_VV(nsync, wf4(:,40), wf8(:,175), A(:,386), n3(:,476), t3x32(:,:,386), nhel, den(286))
    call cont_VV(nsync, wf4(:,32), wf8(:,187), A(:,387), n3(:,477), t3x32(:,:,387), nhel, den(287))
    call cont_VV(nsync, wf4(:,45), wf8(:,167), A(:,388), n3(:,478), t3x32(:,:,388), nhel, den(288))
    call cont_VV(nsync, wf4(:,41), wf8(:,175), A(:,389), n3(:,479), t3x32(:,:,389), nhel, den(289))
    call cont_VV(nsync, wf4(:,45), wf8(:,168), A(:,390), n3(:,480), t3x32(:,:,390), nhel, den(290))

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
  complex(REALKIND), intent(out) :: M1(24) ! M1helarray(24,32)
  integer :: empty(0)

  M1( 1) = 2*CI*(-A(j,1)%j+A(j,2)%j+A(j,10)%j-A(j,11)%j-A(j,14)%j+A(j,15)%j+A(j,22)%j-A(j,24)%j-A(j,28)%j+A(j,29)%j-A(j,31)%j &
        +A(j,32)%j+A(j,40)%j-A(j,41)%j-A(j,44)%j+A(j,45)%j+A(j,52)%j-A(j,54)%j-A(j,58)%j+A(j,59)%j-A(j,62)%j+A(j,63)%j-A(j,70)%j &
        +A(j,71)%j+A(j,73)%j-A(j,75)%j-A(j,82)%j+A(j,83)%j+A(j,88)%j-A(j,89)%j+A(j,97)%j-A(j,98)%j-A(j,100)%j+A(j,101)%j &
        -A(j,103)%j+A(j,104)%j-A(j,106)%j+A(j,107)%j-A(j,109)%j+A(j,110)%j-A(j,112)%j+A(j,113)%j+A(j,118)%j-A(j,119)%j+A(j,133)%j &
        -A(j,134)%j+A(j,145)%j-A(j,146)%j+A(j,148)%j-A(j,149)%j+A(j,151)%j-A(j,153)%j-A(j,158)%j+A(j,159)%j-A(j,160)%j+A(j,161)%j &
        -A(j,176)%j+A(j,177)%j-A(j,179)%j+A(j,180)%j-A(j,182)%j+A(j,183)%j-A(j,185)%j+A(j,186)%j-A(j,205)%j+A(j,206)%j+A(j,208)%j &
        -A(j,210)%j+A(j,217)%j-A(j,219)%j+A(j,220)%j-A(j,222)%j+A(j,223)%j-A(j,225)%j-A(j,232)%j+A(j,233)%j-A(j,235)%j+A(j,236)%j &
        -A(j,238)%j+A(j,239)%j+A(j,241)%j-A(j,243)%j-A(j,244)%j+A(j,246)%j+A(j,251)%j-A(j,252)%j-A(j,257)%j-A(j,264)%j+A(j,269)%j &
        +A(j,270)%j-A(j,271)%j+A(j,272)%j+A(j,277)%j-A(j,283)%j-A(j,285)%j-A(j,286)%j+A(j,288)%j-A(j,293)%j+A(j,294)%j-A(j,299)%j &
        -A(j,307)%j-A(j,308)%j+A(j,310)%j-A(j,314)%j+A(j,315)%j-A(j,316)%j+A(j,317)%j+A(j,320)%j-A(j,321)%j+A(j,322)%j-A(j,323)%j &
        +A(j,324)%j-A(j,326)%j-A(j,329)%j-A(j,330)%j+A(j,337)%j-A(j,338)%j+A(j,342)%j+A(j,355)%j-A(j,357)%j+A(j,361)%j-A(j,362)%j &
        +A(j,363)%j+A(j,364)%j+A(j,365)%j-A(j,366)%j-A(j,368)%j+A(j,385)%j+A(j,387)%j-A(j,388)%j)*f(1)
  M1( 2) = 2*CI*(-A(j,2)%j+A(j,3)%j+A(j,7)%j-A(j,8)%j-A(j,13)%j+A(j,14)%j+A(j,25)%j-A(j,27)%j+A(j,28)%j-A(j,29)%j-A(j,32)%j &
        +A(j,33)%j+A(j,37)%j-A(j,38)%j-A(j,43)%j+A(j,44)%j+A(j,55)%j-A(j,57)%j+A(j,58)%j-A(j,59)%j-A(j,61)%j+A(j,62)%j-A(j,71)%j &
        +A(j,72)%j+A(j,76)%j-A(j,78)%j+A(j,82)%j-A(j,83)%j+A(j,85)%j-A(j,86)%j+A(j,94)%j-A(j,95)%j-A(j,101)%j+A(j,102)%j &
        -A(j,104)%j+A(j,105)%j-A(j,107)%j+A(j,108)%j-A(j,110)%j+A(j,111)%j-A(j,113)%j+A(j,114)%j+A(j,115)%j-A(j,116)%j+A(j,136)%j &
        -A(j,137)%j+A(j,139)%j-A(j,140)%j+A(j,142)%j-A(j,143)%j+A(j,154)%j-A(j,156)%j-A(j,157)%j+A(j,158)%j+A(j,160)%j-A(j,161)%j &
        -A(j,175)%j+A(j,176)%j-A(j,178)%j+A(j,179)%j-A(j,181)%j+A(j,182)%j-A(j,184)%j+A(j,185)%j+A(j,205)%j-A(j,206)%j+A(j,211)%j &
        -A(j,213)%j+A(j,214)%j-A(j,216)%j+A(j,226)%j-A(j,228)%j+A(j,229)%j-A(j,231)%j+A(j,232)%j-A(j,233)%j+A(j,235)%j-A(j,236)%j &
        +A(j,238)%j-A(j,239)%j+A(j,242)%j+A(j,243)%j+A(j,244)%j+A(j,245)%j-A(j,250)%j+A(j,252)%j-A(j,254)%j-A(j,262)%j-A(j,268)%j &
        -A(j,270)%j+A(j,271)%j+A(j,273)%j+A(j,279)%j+A(j,284)%j+A(j,285)%j+A(j,286)%j+A(j,287)%j+A(j,297)%j+A(j,298)%j+A(j,299)%j &
        -A(j,304)%j-A(j,305)%j+A(j,312)%j+A(j,314)%j-A(j,315)%j+A(j,316)%j+A(j,318)%j+A(j,319)%j+A(j,321)%j+A(j,325)%j+A(j,326)%j &
        +A(j,327)%j+A(j,328)%j+A(j,329)%j+A(j,330)%j-A(j,332)%j-A(j,333)%j-A(j,342)%j+A(j,356)%j+A(j,357)%j-A(j,358)%j+A(j,359)%j &
        -A(j,360)%j+A(j,362)%j-A(j,365)%j+A(j,366)%j+A(j,371)%j-A(j,374)%j+A(j,377)%j-A(j,378)%j)*f(1)
  M1( 3) = 2*CI*(A(j,1)%j-A(j,3)%j+A(j,11)%j-A(j,12)%j-A(j,17)%j+A(j,18)%j-A(j,22)%j+A(j,24)%j-A(j,25)%j+A(j,26)%j+A(j,31)%j &
        -A(j,33)%j+A(j,41)%j-A(j,42)%j-A(j,47)%j+A(j,48)%j-A(j,52)%j+A(j,54)%j-A(j,55)%j+A(j,56)%j-A(j,65)%j+A(j,66)%j+A(j,70)%j &
        -A(j,72)%j-A(j,73)%j+A(j,75)%j-A(j,76)%j+A(j,77)%j+A(j,89)%j-A(j,90)%j+A(j,98)%j-A(j,99)%j+A(j,100)%j-A(j,102)%j &
        +A(j,103)%j-A(j,105)%j+A(j,106)%j-A(j,108)%j+A(j,109)%j-A(j,111)%j+A(j,112)%j-A(j,114)%j+A(j,119)%j-A(j,120)%j+A(j,134)%j &
        -A(j,135)%j+A(j,146)%j-A(j,147)%j+A(j,149)%j-A(j,150)%j-A(j,151)%j+A(j,153)%j-A(j,154)%j+A(j,155)%j-A(j,164)%j+A(j,165)%j &
        -A(j,170)%j+A(j,171)%j-A(j,191)%j+A(j,192)%j-A(j,194)%j+A(j,195)%j-A(j,197)%j+A(j,198)%j-A(j,208)%j+A(j,210)%j-A(j,211)%j &
        +A(j,212)%j-A(j,214)%j+A(j,215)%j-A(j,217)%j+A(j,219)%j-A(j,220)%j+A(j,222)%j-A(j,223)%j+A(j,225)%j-A(j,226)%j+A(j,227)%j &
        -A(j,229)%j+A(j,230)%j-A(j,241)%j-A(j,242)%j-A(j,245)%j-A(j,246)%j+A(j,255)%j-A(j,256)%j+A(j,257)%j+A(j,264)%j+A(j,266)%j &
        +A(j,267)%j-A(j,274)%j-A(j,275)%j-A(j,277)%j+A(j,283)%j-A(j,284)%j-A(j,289)%j-A(j,291)%j+A(j,293)%j-A(j,294)%j-A(j,297)%j &
        +A(j,307)%j+A(j,308)%j+A(j,311)%j-A(j,312)%j+A(j,313)%j-A(j,317)%j-A(j,318)%j-A(j,319)%j-A(j,320)%j-A(j,322)%j+A(j,323)%j &
        -A(j,324)%j-A(j,325)%j-A(j,327)%j-A(j,328)%j+A(j,334)%j-A(j,336)%j+A(j,338)%j+A(j,367)%j+A(j,368)%j+A(j,372)%j-A(j,379)%j &
        -A(j,380)%j-A(j,381)%j+A(j,382)%j+A(j,383)%j-A(j,384)%j-A(j,385)%j-A(j,387)%j+A(j,388)%j)*f(1)
  M1( 4) = 2*CI*(-A(j,2)%j+A(j,3)%j+A(j,4)%j-A(j,5)%j-A(j,16)%j+A(j,17)%j+A(j,25)%j-A(j,26)%j+A(j,28)%j-A(j,30)%j-A(j,32)%j &
        +A(j,33)%j+A(j,34)%j-A(j,35)%j-A(j,46)%j+A(j,47)%j+A(j,55)%j-A(j,56)%j+A(j,58)%j-A(j,60)%j-A(j,64)%j+A(j,65)%j-A(j,71)%j &
        +A(j,72)%j+A(j,76)%j-A(j,77)%j+A(j,79)%j-A(j,80)%j+A(j,82)%j-A(j,84)%j+A(j,91)%j-A(j,92)%j-A(j,101)%j+A(j,102)%j &
        -A(j,104)%j+A(j,105)%j-A(j,107)%j+A(j,108)%j-A(j,110)%j+A(j,111)%j-A(j,113)%j+A(j,114)%j+A(j,121)%j-A(j,122)%j+A(j,124)%j &
        -A(j,125)%j+A(j,127)%j-A(j,128)%j+A(j,130)%j-A(j,131)%j+A(j,154)%j-A(j,155)%j+A(j,160)%j-A(j,162)%j-A(j,163)%j+A(j,164)%j &
        -A(j,169)%j+A(j,170)%j-A(j,190)%j+A(j,191)%j-A(j,193)%j+A(j,194)%j-A(j,196)%j+A(j,197)%j+A(j,205)%j-A(j,207)%j+A(j,211)%j &
        -A(j,212)%j+A(j,214)%j-A(j,215)%j+A(j,226)%j-A(j,227)%j+A(j,229)%j-A(j,230)%j+A(j,232)%j-A(j,234)%j+A(j,235)%j-A(j,237)%j &
        +A(j,238)%j-A(j,240)%j+A(j,242)%j+A(j,243)%j+A(j,244)%j+A(j,245)%j-A(j,247)%j-A(j,249)%j+A(j,256)%j-A(j,259)%j-A(j,261)%j &
        -A(j,267)%j+A(j,274)%j+A(j,280)%j+A(j,282)%j+A(j,284)%j+A(j,285)%j+A(j,289)%j+A(j,290)%j+A(j,297)%j+A(j,299)%j+A(j,300)%j &
        -A(j,301)%j-A(j,302)%j+A(j,312)%j-A(j,313)%j+A(j,314)%j+A(j,316)%j+A(j,318)%j+A(j,319)%j+A(j,321)%j+A(j,325)%j+A(j,326)%j &
        +A(j,327)%j+A(j,328)%j+A(j,329)%j+A(j,330)%j+A(j,335)%j+A(j,336)%j+A(j,341)%j-A(j,344)%j-A(j,345)%j-A(j,346)%j+A(j,347)%j &
        -A(j,348)%j-A(j,350)%j+A(j,353)%j-A(j,354)%j-A(j,372)%j+A(j,380)%j-A(j,383)%j+A(j,384)%j)*f(1)
  M1( 5) = 2*CI*(A(j,1)%j-A(j,3)%j+A(j,8)%j-A(j,9)%j-A(j,20)%j+A(j,21)%j-A(j,22)%j+A(j,23)%j-A(j,25)%j+A(j,27)%j+A(j,31)%j &
        -A(j,33)%j+A(j,38)%j-A(j,39)%j-A(j,50)%j+A(j,51)%j-A(j,52)%j+A(j,53)%j-A(j,55)%j+A(j,57)%j-A(j,68)%j+A(j,69)%j+A(j,70)%j &
        -A(j,72)%j-A(j,73)%j+A(j,74)%j-A(j,76)%j+A(j,78)%j+A(j,86)%j-A(j,87)%j+A(j,95)%j-A(j,96)%j+A(j,100)%j-A(j,102)%j &
        +A(j,103)%j-A(j,105)%j+A(j,106)%j-A(j,108)%j+A(j,109)%j-A(j,111)%j+A(j,112)%j-A(j,114)%j+A(j,116)%j-A(j,117)%j+A(j,137)%j &
        -A(j,138)%j+A(j,140)%j-A(j,141)%j+A(j,143)%j-A(j,144)%j-A(j,151)%j+A(j,152)%j-A(j,154)%j+A(j,156)%j-A(j,167)%j+A(j,168)%j &
        -A(j,173)%j+A(j,174)%j-A(j,188)%j+A(j,189)%j-A(j,200)%j+A(j,201)%j-A(j,203)%j+A(j,204)%j-A(j,208)%j+A(j,209)%j-A(j,211)%j &
        +A(j,213)%j-A(j,214)%j+A(j,216)%j-A(j,217)%j+A(j,218)%j-A(j,220)%j+A(j,221)%j-A(j,223)%j+A(j,224)%j-A(j,226)%j+A(j,228)%j &
        -A(j,229)%j+A(j,231)%j-A(j,241)%j-A(j,242)%j-A(j,245)%j-A(j,246)%j+A(j,253)%j+A(j,254)%j-A(j,258)%j+A(j,262)%j+A(j,263)%j &
        +A(j,265)%j-A(j,276)%j-A(j,278)%j-A(j,279)%j+A(j,283)%j-A(j,284)%j-A(j,292)%j+A(j,293)%j-A(j,296)%j-A(j,297)%j-A(j,298)%j &
        +A(j,304)%j+A(j,306)%j+A(j,308)%j+A(j,309)%j-A(j,312)%j-A(j,317)%j-A(j,318)%j-A(j,319)%j-A(j,320)%j-A(j,322)%j+A(j,323)%j &
        -A(j,324)%j-A(j,325)%j-A(j,327)%j-A(j,328)%j+A(j,331)%j+A(j,333)%j-A(j,340)%j-A(j,369)%j-A(j,370)%j-A(j,371)%j+A(j,373)%j &
        +A(j,374)%j-A(j,375)%j+A(j,376)%j-A(j,377)%j+A(j,378)%j+A(j,386)%j+A(j,389)%j-A(j,390)%j)*f(1)
  M1( 6) = 2*CI*(-A(j,1)%j+A(j,2)%j+A(j,5)%j-A(j,6)%j-A(j,19)%j+A(j,20)%j+A(j,22)%j-A(j,23)%j-A(j,28)%j+A(j,30)%j-A(j,31)%j &
        +A(j,32)%j+A(j,35)%j-A(j,36)%j-A(j,49)%j+A(j,50)%j+A(j,52)%j-A(j,53)%j-A(j,58)%j+A(j,60)%j-A(j,67)%j+A(j,68)%j-A(j,70)%j &
        +A(j,71)%j+A(j,73)%j-A(j,74)%j+A(j,80)%j-A(j,81)%j-A(j,82)%j+A(j,84)%j+A(j,92)%j-A(j,93)%j-A(j,100)%j+A(j,101)%j &
        -A(j,103)%j+A(j,104)%j-A(j,106)%j+A(j,107)%j-A(j,109)%j+A(j,110)%j-A(j,112)%j+A(j,113)%j+A(j,122)%j-A(j,123)%j+A(j,125)%j &
        -A(j,126)%j+A(j,128)%j-A(j,129)%j+A(j,131)%j-A(j,132)%j+A(j,151)%j-A(j,152)%j-A(j,160)%j+A(j,162)%j-A(j,166)%j+A(j,167)%j &
        -A(j,172)%j+A(j,173)%j-A(j,187)%j+A(j,188)%j-A(j,199)%j+A(j,200)%j-A(j,202)%j+A(j,203)%j-A(j,205)%j+A(j,207)%j+A(j,208)%j &
        -A(j,209)%j+A(j,217)%j-A(j,218)%j+A(j,220)%j-A(j,221)%j+A(j,223)%j-A(j,224)%j-A(j,232)%j+A(j,234)%j-A(j,235)%j+A(j,237)%j &
        -A(j,238)%j+A(j,240)%j+A(j,241)%j-A(j,243)%j-A(j,244)%j+A(j,246)%j-A(j,248)%j+A(j,249)%j+A(j,258)%j+A(j,259)%j-A(j,260)%j &
        -A(j,265)%j+A(j,276)%j-A(j,281)%j-A(j,282)%j-A(j,283)%j-A(j,285)%j+A(j,292)%j-A(j,293)%j-A(j,295)%j-A(j,299)%j-A(j,300)%j &
        +A(j,301)%j-A(j,303)%j-A(j,308)%j-A(j,309)%j-A(j,314)%j-A(j,316)%j+A(j,317)%j+A(j,320)%j-A(j,321)%j+A(j,322)%j-A(j,323)%j &
        +A(j,324)%j-A(j,326)%j-A(j,329)%j-A(j,330)%j-A(j,339)%j+A(j,340)%j-A(j,341)%j-A(j,343)%j+A(j,345)%j-A(j,349)%j+A(j,350)%j &
        -A(j,351)%j-A(j,352)%j-A(j,353)%j+A(j,354)%j+A(j,370)%j-A(j,386)%j-A(j,389)%j+A(j,390)%j)*f(1)
  M1( 7) = 2*CI*(-A(j,4)%j+A(j,5)%j-A(j,10)%j+A(j,12)%j+A(j,14)%j-A(j,15)%j+A(j,16)%j-A(j,18)%j-A(j,29)%j+A(j,30)%j-A(j,34)%j &
        +A(j,35)%j-A(j,40)%j+A(j,42)%j+A(j,44)%j-A(j,45)%j+A(j,46)%j-A(j,48)%j-A(j,59)%j+A(j,60)%j+A(j,62)%j-A(j,63)%j+A(j,64)%j &
        -A(j,66)%j-A(j,79)%j+A(j,80)%j-A(j,83)%j+A(j,84)%j-A(j,88)%j+A(j,90)%j-A(j,91)%j+A(j,92)%j-A(j,97)%j+A(j,99)%j-A(j,118)%j &
        +A(j,120)%j-A(j,121)%j+A(j,122)%j-A(j,124)%j+A(j,125)%j-A(j,127)%j+A(j,128)%j-A(j,130)%j+A(j,131)%j-A(j,133)%j+A(j,135)%j &
        -A(j,145)%j+A(j,147)%j-A(j,148)%j+A(j,150)%j+A(j,158)%j-A(j,159)%j-A(j,161)%j+A(j,162)%j+A(j,163)%j-A(j,165)%j+A(j,169)%j &
        -A(j,171)%j+A(j,176)%j-A(j,177)%j+A(j,179)%j-A(j,180)%j+A(j,182)%j-A(j,183)%j+A(j,185)%j-A(j,186)%j+A(j,190)%j-A(j,192)%j &
        +A(j,193)%j-A(j,195)%j+A(j,196)%j-A(j,198)%j-A(j,206)%j+A(j,207)%j-A(j,233)%j+A(j,234)%j-A(j,236)%j+A(j,237)%j-A(j,239)%j &
        +A(j,240)%j+A(j,247)%j+A(j,249)%j-A(j,251)%j+A(j,252)%j-A(j,255)%j+A(j,259)%j+A(j,261)%j-A(j,266)%j-A(j,269)%j-A(j,270)%j &
        +A(j,271)%j-A(j,272)%j+A(j,275)%j-A(j,280)%j-A(j,282)%j+A(j,286)%j-A(j,288)%j-A(j,290)%j+A(j,291)%j-A(j,300)%j+A(j,301)%j &
        +A(j,302)%j-A(j,310)%j-A(j,311)%j-A(j,315)%j-A(j,334)%j-A(j,335)%j-A(j,337)%j-A(j,341)%j-A(j,342)%j+A(j,344)%j+A(j,345)%j &
        +A(j,346)%j-A(j,347)%j+A(j,348)%j+A(j,350)%j-A(j,353)%j+A(j,354)%j-A(j,355)%j+A(j,357)%j-A(j,361)%j+A(j,362)%j-A(j,363)%j &
        -A(j,364)%j-A(j,365)%j+A(j,366)%j-A(j,367)%j+A(j,379)%j+A(j,381)%j-A(j,382)%j)*f(1)
  M1( 8) = 2*CI*(-A(j,5)%j+A(j,6)%j-A(j,7)%j+A(j,9)%j+A(j,13)%j-A(j,14)%j+A(j,19)%j-A(j,21)%j+A(j,29)%j-A(j,30)%j-A(j,35)%j &
        +A(j,36)%j-A(j,37)%j+A(j,39)%j+A(j,43)%j-A(j,44)%j+A(j,49)%j-A(j,51)%j+A(j,59)%j-A(j,60)%j+A(j,61)%j-A(j,62)%j+A(j,67)%j &
        -A(j,69)%j-A(j,80)%j+A(j,81)%j+A(j,83)%j-A(j,84)%j-A(j,85)%j+A(j,87)%j-A(j,92)%j+A(j,93)%j-A(j,94)%j+A(j,96)%j-A(j,115)%j &
        +A(j,117)%j-A(j,122)%j+A(j,123)%j-A(j,125)%j+A(j,126)%j-A(j,128)%j+A(j,129)%j-A(j,131)%j+A(j,132)%j-A(j,136)%j+A(j,138)%j &
        -A(j,139)%j+A(j,141)%j-A(j,142)%j+A(j,144)%j+A(j,157)%j-A(j,158)%j+A(j,161)%j-A(j,162)%j+A(j,166)%j-A(j,168)%j+A(j,172)%j &
        -A(j,174)%j+A(j,175)%j-A(j,176)%j+A(j,178)%j-A(j,179)%j+A(j,181)%j-A(j,182)%j+A(j,184)%j-A(j,185)%j+A(j,187)%j-A(j,189)%j &
        +A(j,199)%j-A(j,201)%j+A(j,202)%j-A(j,204)%j+A(j,206)%j-A(j,207)%j+A(j,233)%j-A(j,234)%j+A(j,236)%j-A(j,237)%j+A(j,239)%j &
        -A(j,240)%j+A(j,248)%j-A(j,249)%j+A(j,250)%j-A(j,252)%j-A(j,253)%j-A(j,259)%j+A(j,260)%j-A(j,263)%j+A(j,268)%j+A(j,270)%j &
        -A(j,271)%j-A(j,273)%j+A(j,278)%j+A(j,281)%j+A(j,282)%j-A(j,286)%j-A(j,287)%j+A(j,295)%j+A(j,296)%j+A(j,300)%j-A(j,301)%j &
        +A(j,303)%j+A(j,305)%j-A(j,306)%j+A(j,315)%j-A(j,331)%j+A(j,332)%j+A(j,339)%j+A(j,341)%j+A(j,342)%j+A(j,343)%j-A(j,345)%j &
        +A(j,349)%j-A(j,350)%j+A(j,351)%j+A(j,352)%j+A(j,353)%j-A(j,354)%j-A(j,356)%j-A(j,357)%j+A(j,358)%j-A(j,359)%j+A(j,360)%j &
        -A(j,362)%j+A(j,365)%j-A(j,366)%j+A(j,369)%j-A(j,373)%j+A(j,375)%j-A(j,376)%j)*f(1)
  M1( 9) = 2*CI*(A(j,4)%j-A(j,6)%j+A(j,11)%j-A(j,12)%j-A(j,16)%j+A(j,18)%j-A(j,19)%j+A(j,20)%j-A(j,23)%j+A(j,24)%j+A(j,34)%j &
        -A(j,36)%j+A(j,41)%j-A(j,42)%j-A(j,46)%j+A(j,48)%j-A(j,49)%j+A(j,50)%j-A(j,53)%j+A(j,54)%j-A(j,64)%j+A(j,66)%j-A(j,67)%j &
        +A(j,68)%j-A(j,74)%j+A(j,75)%j+A(j,79)%j-A(j,81)%j+A(j,89)%j-A(j,90)%j+A(j,91)%j-A(j,93)%j+A(j,98)%j-A(j,99)%j+A(j,119)%j &
        -A(j,120)%j+A(j,121)%j-A(j,123)%j+A(j,124)%j-A(j,126)%j+A(j,127)%j-A(j,129)%j+A(j,130)%j-A(j,132)%j+A(j,134)%j-A(j,135)%j &
        +A(j,146)%j-A(j,147)%j+A(j,149)%j-A(j,150)%j-A(j,152)%j+A(j,153)%j-A(j,163)%j+A(j,165)%j-A(j,166)%j+A(j,167)%j-A(j,169)%j &
        +A(j,171)%j-A(j,172)%j+A(j,173)%j-A(j,187)%j+A(j,188)%j-A(j,190)%j+A(j,192)%j-A(j,193)%j+A(j,195)%j-A(j,196)%j+A(j,198)%j &
        -A(j,199)%j+A(j,200)%j-A(j,202)%j+A(j,203)%j-A(j,209)%j+A(j,210)%j-A(j,218)%j+A(j,219)%j-A(j,221)%j+A(j,222)%j-A(j,224)%j &
        +A(j,225)%j-A(j,247)%j-A(j,248)%j+A(j,255)%j+A(j,257)%j+A(j,258)%j-A(j,260)%j-A(j,261)%j+A(j,264)%j-A(j,265)%j+A(j,266)%j &
        -A(j,275)%j+A(j,276)%j-A(j,277)%j+A(j,280)%j-A(j,281)%j+A(j,290)%j-A(j,291)%j+A(j,292)%j-A(j,294)%j-A(j,295)%j-A(j,302)%j &
        -A(j,303)%j+A(j,307)%j-A(j,309)%j+A(j,311)%j+A(j,334)%j+A(j,335)%j+A(j,338)%j-A(j,339)%j+A(j,340)%j-A(j,343)%j-A(j,344)%j &
        -A(j,346)%j+A(j,347)%j-A(j,348)%j-A(j,349)%j-A(j,351)%j-A(j,352)%j+A(j,367)%j+A(j,368)%j+A(j,370)%j-A(j,379)%j-A(j,381)%j &
        +A(j,382)%j-A(j,385)%j-A(j,386)%j-A(j,387)%j+A(j,388)%j-A(j,389)%j+A(j,390)%j)*f(1)
  M1(10) = 2*CI*(A(j,1)%j-A(j,2)%j-A(j,5)%j+A(j,6)%j+A(j,19)%j-A(j,20)%j-A(j,22)%j+A(j,23)%j+A(j,28)%j-A(j,30)%j+A(j,31)%j &
        -A(j,32)%j-A(j,35)%j+A(j,36)%j+A(j,49)%j-A(j,50)%j-A(j,52)%j+A(j,53)%j+A(j,58)%j-A(j,60)%j+A(j,67)%j-A(j,68)%j+A(j,70)%j &
        -A(j,71)%j-A(j,73)%j+A(j,74)%j-A(j,80)%j+A(j,81)%j+A(j,82)%j-A(j,84)%j-A(j,92)%j+A(j,93)%j+A(j,100)%j-A(j,101)%j &
        +A(j,103)%j-A(j,104)%j+A(j,106)%j-A(j,107)%j+A(j,109)%j-A(j,110)%j+A(j,112)%j-A(j,113)%j-A(j,122)%j+A(j,123)%j-A(j,125)%j &
        +A(j,126)%j-A(j,128)%j+A(j,129)%j-A(j,131)%j+A(j,132)%j-A(j,151)%j+A(j,152)%j+A(j,160)%j-A(j,162)%j+A(j,166)%j-A(j,167)%j &
        +A(j,172)%j-A(j,173)%j+A(j,187)%j-A(j,188)%j+A(j,199)%j-A(j,200)%j+A(j,202)%j-A(j,203)%j+A(j,205)%j-A(j,207)%j-A(j,208)%j &
        +A(j,209)%j-A(j,217)%j+A(j,218)%j-A(j,220)%j+A(j,221)%j-A(j,223)%j+A(j,224)%j+A(j,232)%j-A(j,234)%j+A(j,235)%j-A(j,237)%j &
        +A(j,238)%j-A(j,240)%j-A(j,241)%j+A(j,243)%j+A(j,244)%j-A(j,246)%j+A(j,248)%j-A(j,249)%j-A(j,258)%j-A(j,259)%j+A(j,260)%j &
        +A(j,265)%j-A(j,276)%j+A(j,281)%j+A(j,282)%j+A(j,283)%j+A(j,285)%j-A(j,292)%j+A(j,293)%j+A(j,295)%j+A(j,299)%j+A(j,300)%j &
        -A(j,301)%j+A(j,303)%j+A(j,308)%j+A(j,309)%j+A(j,314)%j+A(j,316)%j-A(j,317)%j-A(j,320)%j+A(j,321)%j-A(j,322)%j+A(j,323)%j &
        -A(j,324)%j+A(j,326)%j+A(j,329)%j+A(j,330)%j+A(j,339)%j-A(j,340)%j+A(j,341)%j+A(j,343)%j-A(j,345)%j+A(j,349)%j-A(j,350)%j &
        +A(j,351)%j+A(j,352)%j+A(j,353)%j-A(j,354)%j-A(j,370)%j+A(j,386)%j+A(j,389)%j-A(j,390)%j)*f(1)
  M1(11) = 2*CI*(A(j,4)%j-A(j,6)%j+A(j,8)%j-A(j,9)%j-A(j,16)%j+A(j,17)%j-A(j,19)%j+A(j,21)%j-A(j,26)%j+A(j,27)%j+A(j,34)%j &
        -A(j,36)%j+A(j,38)%j-A(j,39)%j-A(j,46)%j+A(j,47)%j-A(j,49)%j+A(j,51)%j-A(j,56)%j+A(j,57)%j-A(j,64)%j+A(j,65)%j-A(j,67)%j &
        +A(j,69)%j-A(j,77)%j+A(j,78)%j+A(j,79)%j-A(j,81)%j+A(j,86)%j-A(j,87)%j+A(j,91)%j-A(j,93)%j+A(j,95)%j-A(j,96)%j+A(j,116)%j &
        -A(j,117)%j+A(j,121)%j-A(j,123)%j+A(j,124)%j-A(j,126)%j+A(j,127)%j-A(j,129)%j+A(j,130)%j-A(j,132)%j+A(j,137)%j-A(j,138)%j &
        +A(j,140)%j-A(j,141)%j+A(j,143)%j-A(j,144)%j-A(j,155)%j+A(j,156)%j-A(j,163)%j+A(j,164)%j-A(j,166)%j+A(j,168)%j-A(j,169)%j &
        +A(j,170)%j-A(j,172)%j+A(j,174)%j-A(j,187)%j+A(j,189)%j-A(j,190)%j+A(j,191)%j-A(j,193)%j+A(j,194)%j-A(j,196)%j+A(j,197)%j &
        -A(j,199)%j+A(j,201)%j-A(j,202)%j+A(j,204)%j-A(j,212)%j+A(j,213)%j-A(j,215)%j+A(j,216)%j-A(j,227)%j+A(j,228)%j-A(j,230)%j &
        +A(j,231)%j-A(j,247)%j-A(j,248)%j+A(j,253)%j+A(j,254)%j+A(j,256)%j-A(j,260)%j-A(j,261)%j+A(j,262)%j+A(j,263)%j-A(j,267)%j &
        +A(j,274)%j-A(j,278)%j-A(j,279)%j+A(j,280)%j-A(j,281)%j+A(j,289)%j+A(j,290)%j-A(j,295)%j-A(j,296)%j-A(j,298)%j-A(j,302)%j &
        -A(j,303)%j+A(j,304)%j+A(j,306)%j-A(j,313)%j+A(j,331)%j+A(j,333)%j+A(j,335)%j+A(j,336)%j-A(j,339)%j-A(j,343)%j-A(j,344)%j &
        -A(j,346)%j+A(j,347)%j-A(j,348)%j-A(j,349)%j-A(j,351)%j-A(j,352)%j-A(j,369)%j-A(j,371)%j-A(j,372)%j+A(j,373)%j+A(j,374)%j &
        -A(j,375)%j+A(j,376)%j-A(j,377)%j+A(j,378)%j+A(j,380)%j-A(j,383)%j+A(j,384)%j)*f(1)
  M1(12) = 2*CI*(A(j,2)%j-A(j,3)%j-A(j,4)%j+A(j,5)%j+A(j,16)%j-A(j,17)%j-A(j,25)%j+A(j,26)%j-A(j,28)%j+A(j,30)%j+A(j,32)%j &
        -A(j,33)%j-A(j,34)%j+A(j,35)%j+A(j,46)%j-A(j,47)%j-A(j,55)%j+A(j,56)%j-A(j,58)%j+A(j,60)%j+A(j,64)%j-A(j,65)%j+A(j,71)%j &
        -A(j,72)%j-A(j,76)%j+A(j,77)%j-A(j,79)%j+A(j,80)%j-A(j,82)%j+A(j,84)%j-A(j,91)%j+A(j,92)%j+A(j,101)%j-A(j,102)%j &
        +A(j,104)%j-A(j,105)%j+A(j,107)%j-A(j,108)%j+A(j,110)%j-A(j,111)%j+A(j,113)%j-A(j,114)%j-A(j,121)%j+A(j,122)%j-A(j,124)%j &
        +A(j,125)%j-A(j,127)%j+A(j,128)%j-A(j,130)%j+A(j,131)%j-A(j,154)%j+A(j,155)%j-A(j,160)%j+A(j,162)%j+A(j,163)%j-A(j,164)%j &
        +A(j,169)%j-A(j,170)%j+A(j,190)%j-A(j,191)%j+A(j,193)%j-A(j,194)%j+A(j,196)%j-A(j,197)%j-A(j,205)%j+A(j,207)%j-A(j,211)%j &
        +A(j,212)%j-A(j,214)%j+A(j,215)%j-A(j,226)%j+A(j,227)%j-A(j,229)%j+A(j,230)%j-A(j,232)%j+A(j,234)%j-A(j,235)%j+A(j,237)%j &
        -A(j,238)%j+A(j,240)%j-A(j,242)%j-A(j,243)%j-A(j,244)%j-A(j,245)%j+A(j,247)%j+A(j,249)%j-A(j,256)%j+A(j,259)%j+A(j,261)%j &
        +A(j,267)%j-A(j,274)%j-A(j,280)%j-A(j,282)%j-A(j,284)%j-A(j,285)%j-A(j,289)%j-A(j,290)%j-A(j,297)%j-A(j,299)%j-A(j,300)%j &
        +A(j,301)%j+A(j,302)%j-A(j,312)%j+A(j,313)%j-A(j,314)%j-A(j,316)%j-A(j,318)%j-A(j,319)%j-A(j,321)%j-A(j,325)%j-A(j,326)%j &
        -A(j,327)%j-A(j,328)%j-A(j,329)%j-A(j,330)%j-A(j,335)%j-A(j,336)%j-A(j,341)%j+A(j,344)%j+A(j,345)%j+A(j,346)%j-A(j,347)%j &
        +A(j,348)%j+A(j,350)%j-A(j,353)%j+A(j,354)%j+A(j,372)%j-A(j,380)%j+A(j,383)%j-A(j,384)%j)*f(1)
  M1(13) = 2*CI*(-A(j,7)%j+A(j,8)%j-A(j,10)%j+A(j,12)%j+A(j,13)%j-A(j,15)%j+A(j,17)%j-A(j,18)%j-A(j,26)%j+A(j,27)%j-A(j,37)%j &
        +A(j,38)%j-A(j,40)%j+A(j,42)%j+A(j,43)%j-A(j,45)%j+A(j,47)%j-A(j,48)%j-A(j,56)%j+A(j,57)%j+A(j,61)%j-A(j,63)%j+A(j,65)%j &
        -A(j,66)%j-A(j,77)%j+A(j,78)%j-A(j,85)%j+A(j,86)%j-A(j,88)%j+A(j,90)%j-A(j,94)%j+A(j,95)%j-A(j,97)%j+A(j,99)%j-A(j,115)%j &
        +A(j,116)%j-A(j,118)%j+A(j,120)%j-A(j,133)%j+A(j,135)%j-A(j,136)%j+A(j,137)%j-A(j,139)%j+A(j,140)%j-A(j,142)%j+A(j,143)%j &
        -A(j,145)%j+A(j,147)%j-A(j,148)%j+A(j,150)%j-A(j,155)%j+A(j,156)%j+A(j,157)%j-A(j,159)%j+A(j,164)%j-A(j,165)%j+A(j,170)%j &
        -A(j,171)%j+A(j,175)%j-A(j,177)%j+A(j,178)%j-A(j,180)%j+A(j,181)%j-A(j,183)%j+A(j,184)%j-A(j,186)%j+A(j,191)%j-A(j,192)%j &
        +A(j,194)%j-A(j,195)%j+A(j,197)%j-A(j,198)%j-A(j,212)%j+A(j,213)%j-A(j,215)%j+A(j,216)%j-A(j,227)%j+A(j,228)%j-A(j,230)%j &
        +A(j,231)%j+A(j,250)%j-A(j,251)%j+A(j,254)%j-A(j,255)%j+A(j,256)%j+A(j,262)%j-A(j,266)%j-A(j,267)%j+A(j,268)%j-A(j,269)%j &
        -A(j,272)%j-A(j,273)%j+A(j,274)%j+A(j,275)%j-A(j,279)%j-A(j,287)%j-A(j,288)%j+A(j,289)%j+A(j,291)%j-A(j,298)%j+A(j,304)%j &
        +A(j,305)%j-A(j,310)%j-A(j,311)%j-A(j,313)%j+A(j,332)%j+A(j,333)%j-A(j,334)%j+A(j,336)%j-A(j,337)%j-A(j,355)%j-A(j,356)%j &
        +A(j,358)%j-A(j,359)%j+A(j,360)%j-A(j,361)%j-A(j,363)%j-A(j,364)%j-A(j,367)%j-A(j,371)%j-A(j,372)%j+A(j,374)%j-A(j,377)%j &
        +A(j,378)%j+A(j,379)%j+A(j,380)%j+A(j,381)%j-A(j,382)%j-A(j,383)%j+A(j,384)%j)*f(1)
  M1(14) = 2*CI*(-A(j,4)%j+A(j,6)%j-A(j,8)%j+A(j,9)%j+A(j,16)%j-A(j,17)%j+A(j,19)%j-A(j,21)%j+A(j,26)%j-A(j,27)%j-A(j,34)%j &
        +A(j,36)%j-A(j,38)%j+A(j,39)%j+A(j,46)%j-A(j,47)%j+A(j,49)%j-A(j,51)%j+A(j,56)%j-A(j,57)%j+A(j,64)%j-A(j,65)%j+A(j,67)%j &
        -A(j,69)%j+A(j,77)%j-A(j,78)%j-A(j,79)%j+A(j,81)%j-A(j,86)%j+A(j,87)%j-A(j,91)%j+A(j,93)%j-A(j,95)%j+A(j,96)%j-A(j,116)%j &
        +A(j,117)%j-A(j,121)%j+A(j,123)%j-A(j,124)%j+A(j,126)%j-A(j,127)%j+A(j,129)%j-A(j,130)%j+A(j,132)%j-A(j,137)%j+A(j,138)%j &
        -A(j,140)%j+A(j,141)%j-A(j,143)%j+A(j,144)%j+A(j,155)%j-A(j,156)%j+A(j,163)%j-A(j,164)%j+A(j,166)%j-A(j,168)%j+A(j,169)%j &
        -A(j,170)%j+A(j,172)%j-A(j,174)%j+A(j,187)%j-A(j,189)%j+A(j,190)%j-A(j,191)%j+A(j,193)%j-A(j,194)%j+A(j,196)%j-A(j,197)%j &
        +A(j,199)%j-A(j,201)%j+A(j,202)%j-A(j,204)%j+A(j,212)%j-A(j,213)%j+A(j,215)%j-A(j,216)%j+A(j,227)%j-A(j,228)%j+A(j,230)%j &
        -A(j,231)%j+A(j,247)%j+A(j,248)%j-A(j,253)%j-A(j,254)%j-A(j,256)%j+A(j,260)%j+A(j,261)%j-A(j,262)%j-A(j,263)%j+A(j,267)%j &
        -A(j,274)%j+A(j,278)%j+A(j,279)%j-A(j,280)%j+A(j,281)%j-A(j,289)%j-A(j,290)%j+A(j,295)%j+A(j,296)%j+A(j,298)%j+A(j,302)%j &
        +A(j,303)%j-A(j,304)%j-A(j,306)%j+A(j,313)%j-A(j,331)%j-A(j,333)%j-A(j,335)%j-A(j,336)%j+A(j,339)%j+A(j,343)%j+A(j,344)%j &
        +A(j,346)%j-A(j,347)%j+A(j,348)%j+A(j,349)%j+A(j,351)%j+A(j,352)%j+A(j,369)%j+A(j,371)%j+A(j,372)%j-A(j,373)%j-A(j,374)%j &
        +A(j,375)%j-A(j,376)%j+A(j,377)%j-A(j,378)%j-A(j,380)%j+A(j,383)%j-A(j,384)%j)*f(1)
  M1(15) = 2*CI*(A(j,7)%j-A(j,9)%j+A(j,10)%j-A(j,11)%j-A(j,13)%j+A(j,15)%j-A(j,20)%j+A(j,21)%j+A(j,23)%j-A(j,24)%j+A(j,37)%j &
        -A(j,39)%j+A(j,40)%j-A(j,41)%j-A(j,43)%j+A(j,45)%j-A(j,50)%j+A(j,51)%j+A(j,53)%j-A(j,54)%j-A(j,61)%j+A(j,63)%j-A(j,68)%j &
        +A(j,69)%j+A(j,74)%j-A(j,75)%j+A(j,85)%j-A(j,87)%j+A(j,88)%j-A(j,89)%j+A(j,94)%j-A(j,96)%j+A(j,97)%j-A(j,98)%j+A(j,115)%j &
        -A(j,117)%j+A(j,118)%j-A(j,119)%j+A(j,133)%j-A(j,134)%j+A(j,136)%j-A(j,138)%j+A(j,139)%j-A(j,141)%j+A(j,142)%j-A(j,144)%j &
        +A(j,145)%j-A(j,146)%j+A(j,148)%j-A(j,149)%j+A(j,152)%j-A(j,153)%j-A(j,157)%j+A(j,159)%j-A(j,167)%j+A(j,168)%j-A(j,173)%j &
        +A(j,174)%j-A(j,175)%j+A(j,177)%j-A(j,178)%j+A(j,180)%j-A(j,181)%j+A(j,183)%j-A(j,184)%j+A(j,186)%j-A(j,188)%j+A(j,189)%j &
        -A(j,200)%j+A(j,201)%j-A(j,203)%j+A(j,204)%j+A(j,209)%j-A(j,210)%j+A(j,218)%j-A(j,219)%j+A(j,221)%j-A(j,222)%j+A(j,224)%j &
        -A(j,225)%j-A(j,250)%j+A(j,251)%j+A(j,253)%j-A(j,257)%j-A(j,258)%j+A(j,263)%j-A(j,264)%j+A(j,265)%j-A(j,268)%j+A(j,269)%j &
        +A(j,272)%j+A(j,273)%j-A(j,276)%j+A(j,277)%j-A(j,278)%j+A(j,287)%j+A(j,288)%j-A(j,292)%j+A(j,294)%j-A(j,296)%j-A(j,305)%j &
        +A(j,306)%j-A(j,307)%j+A(j,309)%j+A(j,310)%j+A(j,331)%j-A(j,332)%j+A(j,337)%j-A(j,338)%j-A(j,340)%j+A(j,355)%j+A(j,356)%j &
        -A(j,358)%j+A(j,359)%j-A(j,360)%j+A(j,361)%j+A(j,363)%j+A(j,364)%j-A(j,368)%j-A(j,369)%j-A(j,370)%j+A(j,373)%j-A(j,375)%j &
        +A(j,376)%j+A(j,385)%j+A(j,386)%j+A(j,387)%j-A(j,388)%j+A(j,389)%j-A(j,390)%j)*f(1)
  M1(16) = 2*CI*(-A(j,1)%j+A(j,3)%j-A(j,8)%j+A(j,9)%j+A(j,20)%j-A(j,21)%j+A(j,22)%j-A(j,23)%j+A(j,25)%j-A(j,27)%j-A(j,31)%j &
        +A(j,33)%j-A(j,38)%j+A(j,39)%j+A(j,50)%j-A(j,51)%j+A(j,52)%j-A(j,53)%j+A(j,55)%j-A(j,57)%j+A(j,68)%j-A(j,69)%j-A(j,70)%j &
        +A(j,72)%j+A(j,73)%j-A(j,74)%j+A(j,76)%j-A(j,78)%j-A(j,86)%j+A(j,87)%j-A(j,95)%j+A(j,96)%j-A(j,100)%j+A(j,102)%j &
        -A(j,103)%j+A(j,105)%j-A(j,106)%j+A(j,108)%j-A(j,109)%j+A(j,111)%j-A(j,112)%j+A(j,114)%j-A(j,116)%j+A(j,117)%j-A(j,137)%j &
        +A(j,138)%j-A(j,140)%j+A(j,141)%j-A(j,143)%j+A(j,144)%j+A(j,151)%j-A(j,152)%j+A(j,154)%j-A(j,156)%j+A(j,167)%j-A(j,168)%j &
        +A(j,173)%j-A(j,174)%j+A(j,188)%j-A(j,189)%j+A(j,200)%j-A(j,201)%j+A(j,203)%j-A(j,204)%j+A(j,208)%j-A(j,209)%j+A(j,211)%j &
        -A(j,213)%j+A(j,214)%j-A(j,216)%j+A(j,217)%j-A(j,218)%j+A(j,220)%j-A(j,221)%j+A(j,223)%j-A(j,224)%j+A(j,226)%j-A(j,228)%j &
        +A(j,229)%j-A(j,231)%j+A(j,241)%j+A(j,242)%j+A(j,245)%j+A(j,246)%j-A(j,253)%j-A(j,254)%j+A(j,258)%j-A(j,262)%j-A(j,263)%j &
        -A(j,265)%j+A(j,276)%j+A(j,278)%j+A(j,279)%j-A(j,283)%j+A(j,284)%j+A(j,292)%j-A(j,293)%j+A(j,296)%j+A(j,297)%j+A(j,298)%j &
        -A(j,304)%j-A(j,306)%j-A(j,308)%j-A(j,309)%j+A(j,312)%j+A(j,317)%j+A(j,318)%j+A(j,319)%j+A(j,320)%j+A(j,322)%j-A(j,323)%j &
        +A(j,324)%j+A(j,325)%j+A(j,327)%j+A(j,328)%j-A(j,331)%j-A(j,333)%j+A(j,340)%j+A(j,369)%j+A(j,370)%j+A(j,371)%j-A(j,373)%j &
        -A(j,374)%j+A(j,375)%j-A(j,376)%j+A(j,377)%j-A(j,378)%j-A(j,386)%j-A(j,389)%j+A(j,390)%j)*f(1)
  M1(17) = 2*CI*(A(j,5)%j-A(j,6)%j+A(j,7)%j-A(j,9)%j-A(j,13)%j+A(j,14)%j-A(j,19)%j+A(j,21)%j-A(j,29)%j+A(j,30)%j+A(j,35)%j &
        -A(j,36)%j+A(j,37)%j-A(j,39)%j-A(j,43)%j+A(j,44)%j-A(j,49)%j+A(j,51)%j-A(j,59)%j+A(j,60)%j-A(j,61)%j+A(j,62)%j-A(j,67)%j &
        +A(j,69)%j+A(j,80)%j-A(j,81)%j-A(j,83)%j+A(j,84)%j+A(j,85)%j-A(j,87)%j+A(j,92)%j-A(j,93)%j+A(j,94)%j-A(j,96)%j+A(j,115)%j &
        -A(j,117)%j+A(j,122)%j-A(j,123)%j+A(j,125)%j-A(j,126)%j+A(j,128)%j-A(j,129)%j+A(j,131)%j-A(j,132)%j+A(j,136)%j-A(j,138)%j &
        +A(j,139)%j-A(j,141)%j+A(j,142)%j-A(j,144)%j-A(j,157)%j+A(j,158)%j-A(j,161)%j+A(j,162)%j-A(j,166)%j+A(j,168)%j-A(j,172)%j &
        +A(j,174)%j-A(j,175)%j+A(j,176)%j-A(j,178)%j+A(j,179)%j-A(j,181)%j+A(j,182)%j-A(j,184)%j+A(j,185)%j-A(j,187)%j+A(j,189)%j &
        -A(j,199)%j+A(j,201)%j-A(j,202)%j+A(j,204)%j-A(j,206)%j+A(j,207)%j-A(j,233)%j+A(j,234)%j-A(j,236)%j+A(j,237)%j-A(j,239)%j &
        +A(j,240)%j-A(j,248)%j+A(j,249)%j-A(j,250)%j+A(j,252)%j+A(j,253)%j+A(j,259)%j-A(j,260)%j+A(j,263)%j-A(j,268)%j-A(j,270)%j &
        +A(j,271)%j+A(j,273)%j-A(j,278)%j-A(j,281)%j-A(j,282)%j+A(j,286)%j+A(j,287)%j-A(j,295)%j-A(j,296)%j-A(j,300)%j+A(j,301)%j &
        -A(j,303)%j-A(j,305)%j+A(j,306)%j-A(j,315)%j+A(j,331)%j-A(j,332)%j-A(j,339)%j-A(j,341)%j-A(j,342)%j-A(j,343)%j+A(j,345)%j &
        -A(j,349)%j+A(j,350)%j-A(j,351)%j-A(j,352)%j-A(j,353)%j+A(j,354)%j+A(j,356)%j+A(j,357)%j-A(j,358)%j+A(j,359)%j-A(j,360)%j &
        +A(j,362)%j-A(j,365)%j+A(j,366)%j-A(j,369)%j+A(j,373)%j-A(j,375)%j+A(j,376)%j)*f(1)
  M1(18) = 2*CI*(A(j,2)%j-A(j,3)%j-A(j,7)%j+A(j,8)%j+A(j,13)%j-A(j,14)%j-A(j,25)%j+A(j,27)%j-A(j,28)%j+A(j,29)%j+A(j,32)%j &
        -A(j,33)%j-A(j,37)%j+A(j,38)%j+A(j,43)%j-A(j,44)%j-A(j,55)%j+A(j,57)%j-A(j,58)%j+A(j,59)%j+A(j,61)%j-A(j,62)%j+A(j,71)%j &
        -A(j,72)%j-A(j,76)%j+A(j,78)%j-A(j,82)%j+A(j,83)%j-A(j,85)%j+A(j,86)%j-A(j,94)%j+A(j,95)%j+A(j,101)%j-A(j,102)%j &
        +A(j,104)%j-A(j,105)%j+A(j,107)%j-A(j,108)%j+A(j,110)%j-A(j,111)%j+A(j,113)%j-A(j,114)%j-A(j,115)%j+A(j,116)%j-A(j,136)%j &
        +A(j,137)%j-A(j,139)%j+A(j,140)%j-A(j,142)%j+A(j,143)%j-A(j,154)%j+A(j,156)%j+A(j,157)%j-A(j,158)%j-A(j,160)%j+A(j,161)%j &
        +A(j,175)%j-A(j,176)%j+A(j,178)%j-A(j,179)%j+A(j,181)%j-A(j,182)%j+A(j,184)%j-A(j,185)%j-A(j,205)%j+A(j,206)%j-A(j,211)%j &
        +A(j,213)%j-A(j,214)%j+A(j,216)%j-A(j,226)%j+A(j,228)%j-A(j,229)%j+A(j,231)%j-A(j,232)%j+A(j,233)%j-A(j,235)%j+A(j,236)%j &
        -A(j,238)%j+A(j,239)%j-A(j,242)%j-A(j,243)%j-A(j,244)%j-A(j,245)%j+A(j,250)%j-A(j,252)%j+A(j,254)%j+A(j,262)%j+A(j,268)%j &
        +A(j,270)%j-A(j,271)%j-A(j,273)%j-A(j,279)%j-A(j,284)%j-A(j,285)%j-A(j,286)%j-A(j,287)%j-A(j,297)%j-A(j,298)%j-A(j,299)%j &
        +A(j,304)%j+A(j,305)%j-A(j,312)%j-A(j,314)%j+A(j,315)%j-A(j,316)%j-A(j,318)%j-A(j,319)%j-A(j,321)%j-A(j,325)%j-A(j,326)%j &
        -A(j,327)%j-A(j,328)%j-A(j,329)%j-A(j,330)%j+A(j,332)%j+A(j,333)%j+A(j,342)%j-A(j,356)%j-A(j,357)%j+A(j,358)%j-A(j,359)%j &
        +A(j,360)%j-A(j,362)%j+A(j,365)%j-A(j,366)%j-A(j,371)%j+A(j,374)%j-A(j,377)%j+A(j,378)%j)*f(1)
  M1(19) = 2*CI*(-A(j,7)%j+A(j,9)%j-A(j,10)%j+A(j,11)%j+A(j,13)%j-A(j,15)%j+A(j,20)%j-A(j,21)%j-A(j,23)%j+A(j,24)%j-A(j,37)%j &
        +A(j,39)%j-A(j,40)%j+A(j,41)%j+A(j,43)%j-A(j,45)%j+A(j,50)%j-A(j,51)%j-A(j,53)%j+A(j,54)%j+A(j,61)%j-A(j,63)%j+A(j,68)%j &
        -A(j,69)%j-A(j,74)%j+A(j,75)%j-A(j,85)%j+A(j,87)%j-A(j,88)%j+A(j,89)%j-A(j,94)%j+A(j,96)%j-A(j,97)%j+A(j,98)%j-A(j,115)%j &
        +A(j,117)%j-A(j,118)%j+A(j,119)%j-A(j,133)%j+A(j,134)%j-A(j,136)%j+A(j,138)%j-A(j,139)%j+A(j,141)%j-A(j,142)%j+A(j,144)%j &
        -A(j,145)%j+A(j,146)%j-A(j,148)%j+A(j,149)%j-A(j,152)%j+A(j,153)%j+A(j,157)%j-A(j,159)%j+A(j,167)%j-A(j,168)%j+A(j,173)%j &
        -A(j,174)%j+A(j,175)%j-A(j,177)%j+A(j,178)%j-A(j,180)%j+A(j,181)%j-A(j,183)%j+A(j,184)%j-A(j,186)%j+A(j,188)%j-A(j,189)%j &
        +A(j,200)%j-A(j,201)%j+A(j,203)%j-A(j,204)%j-A(j,209)%j+A(j,210)%j-A(j,218)%j+A(j,219)%j-A(j,221)%j+A(j,222)%j-A(j,224)%j &
        +A(j,225)%j+A(j,250)%j-A(j,251)%j-A(j,253)%j+A(j,257)%j+A(j,258)%j-A(j,263)%j+A(j,264)%j-A(j,265)%j+A(j,268)%j-A(j,269)%j &
        -A(j,272)%j-A(j,273)%j+A(j,276)%j-A(j,277)%j+A(j,278)%j-A(j,287)%j-A(j,288)%j+A(j,292)%j-A(j,294)%j+A(j,296)%j+A(j,305)%j &
        -A(j,306)%j+A(j,307)%j-A(j,309)%j-A(j,310)%j-A(j,331)%j+A(j,332)%j-A(j,337)%j+A(j,338)%j+A(j,340)%j-A(j,355)%j-A(j,356)%j &
        +A(j,358)%j-A(j,359)%j+A(j,360)%j-A(j,361)%j-A(j,363)%j-A(j,364)%j+A(j,368)%j+A(j,369)%j+A(j,370)%j-A(j,373)%j+A(j,375)%j &
        -A(j,376)%j-A(j,385)%j-A(j,386)%j-A(j,387)%j+A(j,388)%j-A(j,389)%j+A(j,390)%j)*f(1)
  M1(20) = 2*CI*(-A(j,4)%j+A(j,6)%j-A(j,11)%j+A(j,12)%j+A(j,16)%j-A(j,18)%j+A(j,19)%j-A(j,20)%j+A(j,23)%j-A(j,24)%j-A(j,34)%j &
        +A(j,36)%j-A(j,41)%j+A(j,42)%j+A(j,46)%j-A(j,48)%j+A(j,49)%j-A(j,50)%j+A(j,53)%j-A(j,54)%j+A(j,64)%j-A(j,66)%j+A(j,67)%j &
        -A(j,68)%j+A(j,74)%j-A(j,75)%j-A(j,79)%j+A(j,81)%j-A(j,89)%j+A(j,90)%j-A(j,91)%j+A(j,93)%j-A(j,98)%j+A(j,99)%j-A(j,119)%j &
        +A(j,120)%j-A(j,121)%j+A(j,123)%j-A(j,124)%j+A(j,126)%j-A(j,127)%j+A(j,129)%j-A(j,130)%j+A(j,132)%j-A(j,134)%j+A(j,135)%j &
        -A(j,146)%j+A(j,147)%j-A(j,149)%j+A(j,150)%j+A(j,152)%j-A(j,153)%j+A(j,163)%j-A(j,165)%j+A(j,166)%j-A(j,167)%j+A(j,169)%j &
        -A(j,171)%j+A(j,172)%j-A(j,173)%j+A(j,187)%j-A(j,188)%j+A(j,190)%j-A(j,192)%j+A(j,193)%j-A(j,195)%j+A(j,196)%j-A(j,198)%j &
        +A(j,199)%j-A(j,200)%j+A(j,202)%j-A(j,203)%j+A(j,209)%j-A(j,210)%j+A(j,218)%j-A(j,219)%j+A(j,221)%j-A(j,222)%j+A(j,224)%j &
        -A(j,225)%j+A(j,247)%j+A(j,248)%j-A(j,255)%j-A(j,257)%j-A(j,258)%j+A(j,260)%j+A(j,261)%j-A(j,264)%j+A(j,265)%j-A(j,266)%j &
        +A(j,275)%j-A(j,276)%j+A(j,277)%j-A(j,280)%j+A(j,281)%j-A(j,290)%j+A(j,291)%j-A(j,292)%j+A(j,294)%j+A(j,295)%j+A(j,302)%j &
        +A(j,303)%j-A(j,307)%j+A(j,309)%j-A(j,311)%j-A(j,334)%j-A(j,335)%j-A(j,338)%j+A(j,339)%j-A(j,340)%j+A(j,343)%j+A(j,344)%j &
        +A(j,346)%j-A(j,347)%j+A(j,348)%j+A(j,349)%j+A(j,351)%j+A(j,352)%j-A(j,367)%j-A(j,368)%j-A(j,370)%j+A(j,379)%j+A(j,381)%j &
        -A(j,382)%j+A(j,385)%j+A(j,386)%j+A(j,387)%j-A(j,388)%j+A(j,389)%j-A(j,390)%j)*f(1)
  M1(21) = 2*CI*(A(j,7)%j-A(j,8)%j+A(j,10)%j-A(j,12)%j-A(j,13)%j+A(j,15)%j-A(j,17)%j+A(j,18)%j+A(j,26)%j-A(j,27)%j+A(j,37)%j &
        -A(j,38)%j+A(j,40)%j-A(j,42)%j-A(j,43)%j+A(j,45)%j-A(j,47)%j+A(j,48)%j+A(j,56)%j-A(j,57)%j-A(j,61)%j+A(j,63)%j-A(j,65)%j &
        +A(j,66)%j+A(j,77)%j-A(j,78)%j+A(j,85)%j-A(j,86)%j+A(j,88)%j-A(j,90)%j+A(j,94)%j-A(j,95)%j+A(j,97)%j-A(j,99)%j+A(j,115)%j &
        -A(j,116)%j+A(j,118)%j-A(j,120)%j+A(j,133)%j-A(j,135)%j+A(j,136)%j-A(j,137)%j+A(j,139)%j-A(j,140)%j+A(j,142)%j-A(j,143)%j &
        +A(j,145)%j-A(j,147)%j+A(j,148)%j-A(j,150)%j+A(j,155)%j-A(j,156)%j-A(j,157)%j+A(j,159)%j-A(j,164)%j+A(j,165)%j-A(j,170)%j &
        +A(j,171)%j-A(j,175)%j+A(j,177)%j-A(j,178)%j+A(j,180)%j-A(j,181)%j+A(j,183)%j-A(j,184)%j+A(j,186)%j-A(j,191)%j+A(j,192)%j &
        -A(j,194)%j+A(j,195)%j-A(j,197)%j+A(j,198)%j+A(j,212)%j-A(j,213)%j+A(j,215)%j-A(j,216)%j+A(j,227)%j-A(j,228)%j+A(j,230)%j &
        -A(j,231)%j-A(j,250)%j+A(j,251)%j-A(j,254)%j+A(j,255)%j-A(j,256)%j-A(j,262)%j+A(j,266)%j+A(j,267)%j-A(j,268)%j+A(j,269)%j &
        +A(j,272)%j+A(j,273)%j-A(j,274)%j-A(j,275)%j+A(j,279)%j+A(j,287)%j+A(j,288)%j-A(j,289)%j-A(j,291)%j+A(j,298)%j-A(j,304)%j &
        -A(j,305)%j+A(j,310)%j+A(j,311)%j+A(j,313)%j-A(j,332)%j-A(j,333)%j+A(j,334)%j-A(j,336)%j+A(j,337)%j+A(j,355)%j+A(j,356)%j &
        -A(j,358)%j+A(j,359)%j-A(j,360)%j+A(j,361)%j+A(j,363)%j+A(j,364)%j+A(j,367)%j+A(j,371)%j+A(j,372)%j-A(j,374)%j+A(j,377)%j &
        -A(j,378)%j-A(j,379)%j-A(j,380)%j-A(j,381)%j+A(j,382)%j+A(j,383)%j-A(j,384)%j)*f(1)
  M1(22) = 2*CI*(-A(j,1)%j+A(j,3)%j-A(j,11)%j+A(j,12)%j+A(j,17)%j-A(j,18)%j+A(j,22)%j-A(j,24)%j+A(j,25)%j-A(j,26)%j-A(j,31)%j &
        +A(j,33)%j-A(j,41)%j+A(j,42)%j+A(j,47)%j-A(j,48)%j+A(j,52)%j-A(j,54)%j+A(j,55)%j-A(j,56)%j+A(j,65)%j-A(j,66)%j-A(j,70)%j &
        +A(j,72)%j+A(j,73)%j-A(j,75)%j+A(j,76)%j-A(j,77)%j-A(j,89)%j+A(j,90)%j-A(j,98)%j+A(j,99)%j-A(j,100)%j+A(j,102)%j &
        -A(j,103)%j+A(j,105)%j-A(j,106)%j+A(j,108)%j-A(j,109)%j+A(j,111)%j-A(j,112)%j+A(j,114)%j-A(j,119)%j+A(j,120)%j-A(j,134)%j &
        +A(j,135)%j-A(j,146)%j+A(j,147)%j-A(j,149)%j+A(j,150)%j+A(j,151)%j-A(j,153)%j+A(j,154)%j-A(j,155)%j+A(j,164)%j-A(j,165)%j &
        +A(j,170)%j-A(j,171)%j+A(j,191)%j-A(j,192)%j+A(j,194)%j-A(j,195)%j+A(j,197)%j-A(j,198)%j+A(j,208)%j-A(j,210)%j+A(j,211)%j &
        -A(j,212)%j+A(j,214)%j-A(j,215)%j+A(j,217)%j-A(j,219)%j+A(j,220)%j-A(j,222)%j+A(j,223)%j-A(j,225)%j+A(j,226)%j-A(j,227)%j &
        +A(j,229)%j-A(j,230)%j+A(j,241)%j+A(j,242)%j+A(j,245)%j+A(j,246)%j-A(j,255)%j+A(j,256)%j-A(j,257)%j-A(j,264)%j-A(j,266)%j &
        -A(j,267)%j+A(j,274)%j+A(j,275)%j+A(j,277)%j-A(j,283)%j+A(j,284)%j+A(j,289)%j+A(j,291)%j-A(j,293)%j+A(j,294)%j+A(j,297)%j &
        -A(j,307)%j-A(j,308)%j-A(j,311)%j+A(j,312)%j-A(j,313)%j+A(j,317)%j+A(j,318)%j+A(j,319)%j+A(j,320)%j+A(j,322)%j-A(j,323)%j &
        +A(j,324)%j+A(j,325)%j+A(j,327)%j+A(j,328)%j-A(j,334)%j+A(j,336)%j-A(j,338)%j-A(j,367)%j-A(j,368)%j-A(j,372)%j+A(j,379)%j &
        +A(j,380)%j+A(j,381)%j-A(j,382)%j-A(j,383)%j+A(j,384)%j+A(j,385)%j+A(j,387)%j-A(j,388)%j)*f(1)
  M1(23) = 2*CI*(A(j,4)%j-A(j,5)%j+A(j,10)%j-A(j,12)%j-A(j,14)%j+A(j,15)%j-A(j,16)%j+A(j,18)%j+A(j,29)%j-A(j,30)%j+A(j,34)%j &
        -A(j,35)%j+A(j,40)%j-A(j,42)%j-A(j,44)%j+A(j,45)%j-A(j,46)%j+A(j,48)%j+A(j,59)%j-A(j,60)%j-A(j,62)%j+A(j,63)%j-A(j,64)%j &
        +A(j,66)%j+A(j,79)%j-A(j,80)%j+A(j,83)%j-A(j,84)%j+A(j,88)%j-A(j,90)%j+A(j,91)%j-A(j,92)%j+A(j,97)%j-A(j,99)%j+A(j,118)%j &
        -A(j,120)%j+A(j,121)%j-A(j,122)%j+A(j,124)%j-A(j,125)%j+A(j,127)%j-A(j,128)%j+A(j,130)%j-A(j,131)%j+A(j,133)%j-A(j,135)%j &
        +A(j,145)%j-A(j,147)%j+A(j,148)%j-A(j,150)%j-A(j,158)%j+A(j,159)%j+A(j,161)%j-A(j,162)%j-A(j,163)%j+A(j,165)%j-A(j,169)%j &
        +A(j,171)%j-A(j,176)%j+A(j,177)%j-A(j,179)%j+A(j,180)%j-A(j,182)%j+A(j,183)%j-A(j,185)%j+A(j,186)%j-A(j,190)%j+A(j,192)%j &
        -A(j,193)%j+A(j,195)%j-A(j,196)%j+A(j,198)%j+A(j,206)%j-A(j,207)%j+A(j,233)%j-A(j,234)%j+A(j,236)%j-A(j,237)%j+A(j,239)%j &
        -A(j,240)%j-A(j,247)%j-A(j,249)%j+A(j,251)%j-A(j,252)%j+A(j,255)%j-A(j,259)%j-A(j,261)%j+A(j,266)%j+A(j,269)%j+A(j,270)%j &
        -A(j,271)%j+A(j,272)%j-A(j,275)%j+A(j,280)%j+A(j,282)%j-A(j,286)%j+A(j,288)%j+A(j,290)%j-A(j,291)%j+A(j,300)%j-A(j,301)%j &
        -A(j,302)%j+A(j,310)%j+A(j,311)%j+A(j,315)%j+A(j,334)%j+A(j,335)%j+A(j,337)%j+A(j,341)%j+A(j,342)%j-A(j,344)%j-A(j,345)%j &
        -A(j,346)%j+A(j,347)%j-A(j,348)%j-A(j,350)%j+A(j,353)%j-A(j,354)%j+A(j,355)%j-A(j,357)%j+A(j,361)%j-A(j,362)%j+A(j,363)%j &
        +A(j,364)%j+A(j,365)%j-A(j,366)%j+A(j,367)%j-A(j,379)%j-A(j,381)%j+A(j,382)%j)*f(1)
  M1(24) = 2*CI*(A(j,1)%j-A(j,2)%j-A(j,10)%j+A(j,11)%j+A(j,14)%j-A(j,15)%j-A(j,22)%j+A(j,24)%j+A(j,28)%j-A(j,29)%j+A(j,31)%j &
        -A(j,32)%j-A(j,40)%j+A(j,41)%j+A(j,44)%j-A(j,45)%j-A(j,52)%j+A(j,54)%j+A(j,58)%j-A(j,59)%j+A(j,62)%j-A(j,63)%j+A(j,70)%j &
        -A(j,71)%j-A(j,73)%j+A(j,75)%j+A(j,82)%j-A(j,83)%j-A(j,88)%j+A(j,89)%j-A(j,97)%j+A(j,98)%j+A(j,100)%j-A(j,101)%j &
        +A(j,103)%j-A(j,104)%j+A(j,106)%j-A(j,107)%j+A(j,109)%j-A(j,110)%j+A(j,112)%j-A(j,113)%j-A(j,118)%j+A(j,119)%j-A(j,133)%j &
        +A(j,134)%j-A(j,145)%j+A(j,146)%j-A(j,148)%j+A(j,149)%j-A(j,151)%j+A(j,153)%j+A(j,158)%j-A(j,159)%j+A(j,160)%j-A(j,161)%j &
        +A(j,176)%j-A(j,177)%j+A(j,179)%j-A(j,180)%j+A(j,182)%j-A(j,183)%j+A(j,185)%j-A(j,186)%j+A(j,205)%j-A(j,206)%j-A(j,208)%j &
        +A(j,210)%j-A(j,217)%j+A(j,219)%j-A(j,220)%j+A(j,222)%j-A(j,223)%j+A(j,225)%j+A(j,232)%j-A(j,233)%j+A(j,235)%j-A(j,236)%j &
        +A(j,238)%j-A(j,239)%j-A(j,241)%j+A(j,243)%j+A(j,244)%j-A(j,246)%j-A(j,251)%j+A(j,252)%j+A(j,257)%j+A(j,264)%j-A(j,269)%j &
        -A(j,270)%j+A(j,271)%j-A(j,272)%j-A(j,277)%j+A(j,283)%j+A(j,285)%j+A(j,286)%j-A(j,288)%j+A(j,293)%j-A(j,294)%j+A(j,299)%j &
        +A(j,307)%j+A(j,308)%j-A(j,310)%j+A(j,314)%j-A(j,315)%j+A(j,316)%j-A(j,317)%j-A(j,320)%j+A(j,321)%j-A(j,322)%j+A(j,323)%j &
        -A(j,324)%j+A(j,326)%j+A(j,329)%j+A(j,330)%j-A(j,337)%j+A(j,338)%j-A(j,342)%j-A(j,355)%j+A(j,357)%j-A(j,361)%j+A(j,362)%j &
        -A(j,363)%j-A(j,364)%j-A(j,365)%j+A(j,366)%j+A(j,368)%j-A(j,385)%j-A(j,387)%j+A(j,388)%j)*f(1)

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
  use ol_colourmatrix_heftpphjjj_hggggg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(24)
  real(REALKIND),    intent(out) :: M2colint(0:23-1)
  integer,           intent(in)  :: extcombs(:)
  integer :: extcomb, colmatpos, i, j, eco

  M2colint = 0

  do eco = 1, size(extcombs)
    extcomb = extcombs(eco)
    colmatpos = 24*extcomb
    do i = 1, 24
      do j = 1, 24
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
  use ol_colourmatrix_heftpphjjj_hggggg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(24), M2(24)
  real(REALKIND),    intent(out) :: M2colint(0:23-1)
  integer,           intent(in)  :: extcombs(:)
  integer :: extcomb, colmatpos, i, j, eco

  M2colint = 0

  do eco = 1, size(extcombs)
    extcomb = extcombs(eco)
    colmatpos = 24*extcomb
    do i = 1, 24
      do j = 1, 24
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
  use ol_colourmatrix_heftpphjjj_hggggg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(24)
  complex(REALKIND), intent(in)  :: M2(24)
  complex(REALKIND),    intent(out) :: M2colint
  integer :: i, j

  M2colint = 0

  do i = 1, 24
    do j = 1, 24
      M2colint = M2colint + M1(i)*K1(i,j)*conjg(M2(j))
    end do
  end do

end subroutine colintmunu

end subroutine amp2


#ifdef PRECISION_dp
subroutine colourvector(M1out, nhelout) &
  & bind(c,name="ol_tree_colvect_heftpphjjj_hggggg_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(24,32)
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
    & bind(c,name="ol_f_amp2tree_heftpphjjj_hggggg_1")
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
    & bind(c,name="ol_f_amp2ccone_heftpphjjj_hggggg_1")
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
    & bind(c,name="ol_f_amp2ccall_heftpphjjj_hggggg_1")
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
    & bind(c,name="ol_f_amp2hcone_heftpphjjj_hggggg_1")
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
    & bind(c,name="ol_f_amp2hcall_heftpphjjj_hggggg_1")
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
    & bind(c,name="ol_amp2tree_heftpphjjj_hggggg_1")
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
    & bind(c,name="ol_amp2ccone_heftpphjjj_hggggg_1")
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
    & bind(c,name="ol_amp2ccall_heftpphjjj_hggggg_1")
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
    & bind(c,name="ol_amp2hcone_heftpphjjj_hggggg_1")
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
    & bind(c,name="ol_amp2hcall_heftpphjjj_hggggg_1")
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
    & bind(c,name="amp2tree_heftpphjjj_hggggg_1_")
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
    & bind(c,name="amp2ccone_heftpphjjj_hggggg_1_")
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
    & bind(c,name="amp2ccall_heftpphjjj_hggggg_1_")
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
    & bind(c,name="amp2hcone_heftpphjjj_hggggg_1_")
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
    & bind(c,name="amp2hcall_heftpphjjj_hggggg_1_")
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

end module ol_tree_heftpphjjj_hggggg_1_/**/REALKIND
